(* SymFuzz *)

(** command line options

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

open Libinput

let anon x = raise(Arg.Bad("Bad argument: '"^x^"'"))

let usage = "Usage: "^Sys.argv.(0)^" <options> -recipe [conf]\n"

type t =
  {
    recipe_file   : string;
    recipe_string : string list;
    target        : string option;
    socket_name   : string;
    binname       : string option;
    debug_flag    : bool;
  }

let options = ref
  {
    recipe_file="";
    recipe_string=[];
    target=None;
    socket_name="/tmp/symfuzz";
    binname=None;
    debug_flag=false;
  }

let specs =
  [
    ("-json",
     Arg.String (fun s ->
       options := {!options with recipe_file=s}),
     " specify json file as a seed");
    ("-debug",
     Arg.Unit (fun () ->
       options := {!options with debug_flag=true;}
      ),
     " enable debugging mode (iterate upto count)");
  ]

let read_json json =
  if json = "" then failwith "Must specify a configuration file." else ();
  let ic = open_in json in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  of_json s

let get_input_vector () =
  let input_vector = read_json !options.recipe_file in
  let bin_target =
    try get_binary_target input_vector
    with Not_found ->
      (Printf.eprintf "need to specify binary target\n"; exit 1)
  in
  let bin_target =
    match !options.target with
    | Some target ->
        assert (bin_target = target);
        target
    | None ->
        bin_target
  in
  input_vector, bin_target

let replace_options opts = options := opts

let option_check opts =
  if opts.debug_flag then Printexc.record_backtrace true
  else ()

let optparse () =
  let () = Arg.parse (Arg.align specs) anon usage in
  let () = option_check !options in
  !options

