(* SymFuzz *)

(** memory handling

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

open Pinapi
open Dependency
open Big_int_Z
open Big_int_convenience
open Log_analysis

exception Invalid_memory of address_t

let get_memory_byte tid addr =
  let memval = get_mem_byte tid addr in
  if memval.valid = '\x01' then memval.value
  else raise (Invalid_memory addr)

let get_memory tid addr len =
  let v = get_mem tid addr len in
  if String.length v = 0 then raise (Invalid_memory addr)
  else v

let get_string tid addr =
  get_mem_string tid addr

let bytes_to_native str =
  let len = String.length str in
  assert (len = Misc.addr_size);
  let rec loop i acc =
    if i < 0 then acc
    else (
      let v = Nativeint.of_int (int_of_char str.[i]) in
      let v = Nativeint.shift_left v (i*8) in
      loop (i-1) (Nativeint.logor acc v))
  in
  loop (len - 1) 0n

let bytes_to_big_int str =
  let len = String.length str in
  let rec loop i acc =
    if i < 0 then acc
    else (
      let v = big_int_of_int (int_of_char str.[i]) in
      let v = shift_left_big_int v (i*8) in
      loop (i-1) (or_big_int acc v))
  in
  loop (len - 1) bi0

let get_nativeint_from_addr tid addr =
  let s = get_memory tid addr Misc.addr_size in
  bytes_to_native s

