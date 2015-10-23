(* SymFuzz *)

(** local socket

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

let clear_socket name =
  try Unix.unlink name
  with Unix.Unix_error _ -> ()

let create_server socket_name =
  let server = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  (* Printf.printf "server create\n"; flush stdout; *)
  let () = clear_socket socket_name in
  let () = Unix.bind server (Unix.ADDR_UNIX socket_name) in
  let () = Unix.listen server 10 in
  server

let accept_client socket =
  Unix.accept socket

let create_client socket_name trial =
  let client = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  (* Printf.printf "client create\n"; flush stdout; *)
  let rec loop cnt =
    if cnt > 0 then
      try
        Unix.connect client (Unix.ADDR_UNIX socket_name)
      with _ ->
        let () = Unix.sleep 1 in loop (cnt-1)
    else
      failwith "failed to connect to local socket"
  in
  let () = loop trial in
  client

