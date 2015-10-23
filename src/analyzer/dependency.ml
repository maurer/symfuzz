(* SymFuzz *)

(** input-byte dependency

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

type address_t = nativeint

type postidom_t =
  | UnknownPdom
  | Pdom of nativeint
  | Return

module DepSet = IntSet
type inputdep = DepSet.t

let dep_cache : (int, inputdep) Hashtbl.t = Hashtbl.create 7919

let dep_id cor = DepSet.fold (fun i acc -> acc + i) cor 0

let dep_memoize cor =
  let id = dep_id cor in
  try
    List.find (fun e -> DepSet.equal e cor) (Hashtbl.find_all dep_cache id)
  with Not_found ->
    Hashtbl.add dep_cache id cor; cor

let dep_to_string v =
  let fill_buf buf p off =
    let pos = string_of_int p in
    let len = String.length pos in
    String.blit pos 0 buf off len;
    Bytes.set buf (off+len) ';';
    off + len + 1, false
  in
  let bufsize = 1024 in
  let buf = Bytes.create bufsize in
  let len, cut =
    DepSet.fold (fun p (off, cut) ->
      if off >= (bufsize - 32) then off, true else fill_buf buf p off
    ) v (0, false)
  in
  let str = String.sub buf 0 len in
  if cut then str ^ "..." else str

let dep_is_bottom = DepSet.is_empty
let dep_union a b = dep_memoize (DepSet.union a b)
let dep_empty = dep_memoize (DepSet.empty)
let dep_equal = DepSet.equal
let dep_cardinal = DepSet.cardinal
let dep_iter = DepSet.iter
let dep_fold = DepSet.fold
let dep_singleton e = dep_memoize (DepSet.singleton e)

module rec AbstrVal : sig

  type t = inputdep list (* inputdep per each byte *)

  val bottom : t

  val value_to_string : t -> string

  val value_merge : t -> inputdep

  val equal : t -> t -> bool

  val is_bottom : t -> bool

  val is_tainted : t -> bool

end =
struct

  type t = inputdep list

  let bottom = []

  let value_to_string v =
    let to_string hd v =
      Printf.sprintf "%s, ...(%d)"
        (if dep_is_bottom hd then "Bot" else dep_to_string hd)
        (List.length v)
    in
    match v with
    | [] -> "Bot"
    | hd::_ -> to_string hd v

  let value_merge v =
    List.fold_left (fun acc v -> dep_union acc v) dep_empty v

  let equal a b =
    match a, b with
    | [], [] -> true
    | [], _ -> false
    | _, [] -> false
    | a, b -> List.for_all2 dep_equal a b

  let is_bottom = function
    | [] -> true
    | a -> List.for_all dep_is_bottom a

  let is_tainted c = not (is_bottom c)

end

include AbstrVal

module MemMap = Map.Make(Nativeint)

