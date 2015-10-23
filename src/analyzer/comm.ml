(* SymFuzz *)

(** communicator

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

open Symfuzz_protocol
open Misc

type db_type =
  {
    vector  : Libinput_type.input_vector list;
    results : int list
  }

let empty_db = {vector=[]; results=[]}

let fd_to_chans fd =
  Unix.in_channel_of_descr fd, Unix.out_channel_of_descr fd

let pop_vector db =
  let vector, rest =
    match db.vector with
    | vector::rest -> vector, rest
    | [] -> failwith "input vector does not exist"
  in
  vector, {db with vector=rest}

let push_result db numcor =
  {db with results=(numcor::db.results)}

let pop_result db =
  let result, rest =
    match db.results with
    | r::rest -> r, rest
    | [] -> failwith "result does not exist"
  in
  result, {db with results=rest}

let process_inter_sock db ic oc =
  match recv ic with
  | Register s -> {db with vector=(s::(db.vector))}
  | ReqResult ->
      let res, db' = pop_result db in
      let () = send oc (Result res) in
      db'
  | _ -> failwith "inter_sock unhandled message"

let process_intra_sock db ic oc =
  match recv ic with
  | IVReq ->
      let vec, db' = pop_vector db in
      let () = send oc (IVResponse vec) in
      db'
  | OptionReq ->
      let opt = !Options.options in
      let () = send oc (OptionResponse opt) in
      db
  | Result numcor ->
      let db' = push_result db numcor in
      let () = send oc Response in
      db'
  | _ -> failwith "intra_sock unhandled message"

(* internal service loop that manipulates global data *)
let server_loop (inter_sock, intra_sock) =
  let inter_ic, inter_oc = fd_to_chans inter_sock in
  let intra_ic, intra_oc = fd_to_chans intra_sock in
  let rec main_loop db cnt =
    let fds, _, _ = Unix.select [inter_sock; intra_sock] [] [] neg_infinity in
    let rec process_fds db = function
      | fd::tl when fd = inter_sock ->
          let db = process_inter_sock db inter_ic inter_oc in
          process_fds db tl
      | fd::tl when fd = intra_sock ->
          let db = process_intra_sock db intra_ic intra_oc in
          process_fds db tl
      | _::tl -> failwith "unknown file descriptor"
      | [] -> db
    in
    let db = process_fds db fds in
    main_loop db (cnt+1)
  in
  (* protocol starts after getting a ping message *)
  let () = wait_for_ping inter_ic inter_oc in
  try
    main_loop empty_db 0
  with
    | End_of_file -> exit 1
    | e ->
      begin
        Printf.eprintf "exception@server: %s\n" (Printexc.to_string e);
        Printf.eprintf "%s\n" (Printexc.get_backtrace ());
        flush stderr; exit 1
      end

let relay_packets packet inter_oc intra_ic intra_oc =
  send intra_oc packet;
  match recv intra_ic with
  | NoResponse -> ()
  | msg -> send inter_oc msg

(* communication link between pin instrumentor *)
let rec client_accept_loop (inter_sock, intra_sock) =
  let client_sock, _client_addr = Localsock.accept_client inter_sock in
  let inter_ic, inter_oc = fd_to_chans client_sock in
  let intra_ic, intra_oc = fd_to_chans intra_sock in
  let rec main_loop inter_ic =
    let () =
      match recv inter_ic with
      | p -> relay_packets p inter_oc intra_ic intra_oc
    in
    main_loop inter_ic
  in
  let () = wait_for_ping inter_ic inter_oc in
  try
    main_loop inter_ic
  with
    | Protocol.ProtocolError -> Printf.eprintf "protocol error\n"; exit 1
    | _ -> client_accept_loop (inter_sock, intra_sock)

