(* SymFuzz *)

(** misc functions for BAP

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

open Big_int_Z
open Big_int_convenience

type seek_label_t =
  | KeepDoing
  | StopIt

let bigguy =
  if Nativeint.size = 32 then biconst64 0x100000000L
  else (* 64-bit *) big_int_of_string "18446744073709552000"

let maxint = big_int_of_nativeint Nativeint.max_int

let safe_bigint_to_native bigint =
  assert ( bigint < bigguy );
  if bigint >% maxint then
    nativeint_of_big_int (minus_big_int (bigguy -% bigint))
  else
    nativeint_of_big_int bigint

let safe_bigint_of_native n =
  if n < 0n then (big_int_of_nativeint n) +% bigguy
  else big_int_of_nativeint n

let undef_regexp = Str.regexp "[uU]ndefined"

let is_undefined_msg msg =
  try ignore (Str.search_forward undef_regexp msg 0); true
  with Not_found -> false

let is_the_label target_lbl = function
  | Ast.Label (lbl, _) -> (Ast.exp_of_lab lbl) = target_lbl
  | _ -> false

let seek_lbl nextlbl stmt =
  match nextlbl with
  | None -> StopIt
  | Some lbl ->
      if is_the_label lbl stmt then StopIt
      else KeepDoing

let is_call attrb = List.mem (Type.StrAttr "call") attrb
let is_ret attrb = List.mem (Type.StrAttr "ret") attrb

