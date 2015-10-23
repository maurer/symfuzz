(* SymFuzz *)

(** analyzer main

    @author Sang Kil Cha <sangkil.cha\@gmail.com>
    @since  2014-03-19

 *)

(*
Copyright (c) 2014, Sang Kil Cha
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the <organization> nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL SANG KIL CHA BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
*)

open Options
open Misc
open Dependency
open Log_interface
open Libinput_type
open Libinput_value

(** main analysis state *)
type analysis_state =
  {
    id: int;
    input_queue: input_vector Queue.t;
    target: string;   (* target binary name *)
    logdir: string;   (* log directory *)
    sockname: string;
  }

(** prepare execution environment for the input vector *)
let prepare_env vector target =
  FileMap.iter (fun name file ->
    (* replace file contents *)
    let content = File.get_conc_value file in
    let f = open_out name in
    output_string f content;
    close_out f
  ) vector.files;
  vector

let analysis options (ic,oc) st =
  (* step 1: run ibdi analysis *)
  let vector = Queue.pop st.input_queue in
  let vector = Symfuzz_protocol.push_vector oc vector in
  let vector = prepare_env vector st.target in
  let filenames =
    FileMap.fold (fun path _file names -> path::names) vector.files []
  in
  logf "launching analysis";
  let df = options.debug_flag in
  let bn = options.binname in
  Pin.execute vector st.logdir st.id st.sockname filenames df bn;
  (* step 2: obtain result from the analysis *)
  let _res = Symfuzz_protocol.pull_result ic oc in
  (* process the result *)
  st, vector

(*******************************************************************)
(* main symfuzz loop                                               *)
(*******************************************************************)
let rec main_loop options ((ic,oc) as chan) st =
  let _st, _vector = analysis options chan st in
  (* TODO: run another analysis based on the result *)
  ()

let force_mkdir path perm =
  try Unix.mkdir path perm with _ -> ()

let force_symlink f t =
  let () = try Unix.unlink t with _ -> () in
  try Unix.symlink f t with _ -> ()

let init_logdir prefix =
  let lognum = 0 in
  let rec initialize lognum =
    let dirname = Printf.sprintf "%s-%d" prefix lognum in
    if Sys.file_exists dirname then initialize (lognum+1)
    else
      let () = force_mkdir dirname 0o777 in
      let () = force_symlink dirname (prefix^"-last") in
      dirname
  in
  initialize lognum

let init_log options logdir =
  set_debug_flag (options.debug_flag);
  init_log (Filename.concat logdir "main.log")

let child_proc sock main_to_child_in main_to_child_out =
  let () = Unix.close main_to_child_out in
  let intra_in, intra_out = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let sock = Localsock.create_server sock in
  let t1 = Thread.create Comm.server_loop (main_to_child_in, intra_in) in
  let t2 = Thread.create Comm.client_accept_loop (sock, intra_out) in
  Thread.join t1;
  Thread.join t2;
  Unix.close intra_in;
  Unix.close intra_out

let parent_proc options iv pid main_to_child_in main_to_child_out st =
  let () = Unix.close main_to_child_in in
  try begin
    let chan = Comm.fd_to_chans main_to_child_out in
    let () = Symfuzz_protocol.ping_check (fst chan) (snd chan) in
    let () = Queue.push iv st.input_queue in
    main_loop options chan st
  end with e ->
    Printf.eprintf "exception@parent: %s\n" (Printexc.to_string e);
    exit 1

let check_binary path =
  if Sys.file_exists path then path
  else failwith (Printf.sprintf "invalid binary path: %s" path)

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)

let _ =
  let options = optparse () in
  let iv, binary =
    try get_input_vector ()
    with Not_found -> error_exit "no input vector provided"
  in
  let bin = check_binary binary in
  let sock = options.socket_name in
  let logdir = init_logdir "symfuzzlog" in
  let () = init_log options logdir in
  let st =
    {
      id=0;
      input_queue=Queue.create ();
      target=bin;
      logdir=logdir;
      sockname=sock;
    }
  in
  let main_to_child_in, main_to_child_out =
    Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  let () =
    match Unix.fork () with
    | 0 -> child_proc sock main_to_child_in main_to_child_out
    | pid -> parent_proc options iv pid main_to_child_in main_to_child_out st
  in
  exit 0

