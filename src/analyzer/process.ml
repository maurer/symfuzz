(* SymFuzz *)

(** process handling

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

open Misc

(* simple fork and exec *)
let fork_and_exec cmds =
  let pid = Unix.fork () in
  let path =
    try List.hd cmds with Not_found -> failwith "fork_and_exec: wrong cmds"
  in
  match pid with
    | 0 -> (* child *)
        Unix.execvp path (Array.of_list cmds)
    | -1 -> failwith "failed to fork"
    | _ -> (* parent *)
        ignore( Unix.wait () )

(* proc map parsing *)
let read_proc_map pid =
  let procfile = Printf.sprintf "/proc/%d/maps" pid in
  readlines procfile

(* get loaded shared object *)
let get_loaded_so ?filterout:(filterout=fun _ -> false) pid =
  let proclist = read_proc_map pid in
  List.fold_left (fun acc line ->
    (* Printf.printf "%s\n" line; *)
    try
      let saddr, perm, inode, name = Scanf.sscanf line "%nx-%lx %4s %lx %5s %ld %s"
        (fun saddr _end perm _ _dev inode name ->
          saddr, perm, inode, name
        )
      in
      if perm.[0] = 'r' && perm.[2] = 'x' && inode > 0l then
        if filterout name then acc
        else
          if StringMap.mem name acc then acc
          else StringMap.add name saddr acc
      else
        acc
    with Scanf.Scan_failure _ ->
      acc
  ) StringMap.empty proclist

