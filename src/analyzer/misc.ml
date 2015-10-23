(* SymFuzz *)

(** misc

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

exception Overflow

module AddrSet = Set.Make(Nativeint)
module AddrMap = Map.Make(Nativeint)
module StringSet = Set.Make(String)
module StringMap = Map.Make(String)
module IntMap = Map.Make(struct type t = int let compare = compare end)
module IntSet = Set.Make(struct type t = int let compare = compare end)

let addr_size = Sys.word_size / 8
let addr_sizen = Nativeint.of_int addr_size

let bytes_to_list bytes =
  let rec loop i acc =
    if i < 0 then acc else loop (i-1) (bytes.[i] :: acc)
  in
  loop (String.length bytes - 1) []

let bytes_to_array bytes =
  let lst = bytes_to_list bytes in
  Array.of_list lst

let get_clean_addr addr =
  let addr = Int64.of_nativeint addr in
  if Sys.word_size = 32 then Int64.logand 0xffffffffL addr
  else addr

let readlines file =
  let chan = open_in file in
  let lines = ref [] in
  try
    while true do
      lines := input_line chan :: !lines
    done; []
  with End_of_file ->
    close_in chan;
    List.rev !lines

let chomp str =
  if str = "" then ""
  else
    let search_pos init p next =
      let rec search i =
        if p i then raise(Failure "empty")
        else
          match str.[i] with
            | ' ' | '\n' | '\r' | '\t' -> search (next i)
            | _ -> i
      in
      search init
    in
    let len = String.length str in
    try
      let left = search_pos 0 (fun i -> i >= len) (succ)
      and right = search_pos (len - 1) (fun i -> i < 0) (pred)
      in
      String.sub str left (right - left + 1)
    with
      Failure "empty" -> ""

let ( +< ) = Nativeint.add
let ( -< ) = Nativeint.sub
let ( *< ) = Nativeint.mul
let ( /< ) = Nativeint.div
let ( <<< ) = Nativeint.shift_left
let ( >>< ) = Nativeint.shift_right

type op_t =
  | Plus
  | Minus
  | Mul
  | Div

type sign_t =
  | Pos
  | Neg
  | Zero

let sign_to_string = function
  | Pos -> "positive"
  | Neg -> "negative"
  | Zero -> "zero"

let abs = Nativeint.abs

let plus_expectation s1 s2 =
  match s1, s2 with
    | Pos, Pos -> Pos
    | Neg, Neg -> raise Overflow
    | Pos, Neg -> Neg
    | Neg, Pos -> Neg
    | s, Zero -> s
    | Zero, s -> s

let minus_expectation = plus_expectation

let mul_expectation s1 s2 =
  match s1, s2 with
    | Pos, Pos -> Pos
    | Neg, Neg -> Pos
    | Pos, Neg -> Neg
    | Neg, Pos -> Neg
    | _, Zero -> Zero
    | Zero, _ -> Zero

let div_expectation = mul_expectation

let sign_of x = if x < 0n then Neg else if x = 0n then Zero else Pos

let get_expected_sign op x y =
  match op with
    | Plus -> plus_expectation (sign_of x) (sign_of y)
    | Minus -> minus_expectation (sign_of x) (sign_of y)
    | Mul -> mul_expectation (sign_of x) (sign_of y)
    | Div -> div_expectation (sign_of x) (sign_of y)

let assert_sign v op expectation =
  match expectation with
    | Pos -> if sign_of v <> Pos then raise Overflow else ()
    | Neg -> if sign_of v <> Neg then raise Overflow else ()
    | Zero -> if sign_of v <> Zero then raise Overflow else ()

let sign_check fn op x y =
  let expected_sign = get_expected_sign op x y in
  let v = fn x y in
  assert_sign v op expected_sign;
  v

let ( +! ) x y = sign_check Nativeint.add Plus x y
let ( -! ) x y = sign_check Nativeint.sub Minus x y
let ( *! ) x y = sign_check Nativeint.mul Mul x y
let ( /! ) x y = sign_check Nativeint.div Div x y

let error_exit msg =
  Printf.eprintf "%s\n" msg;
  exit 1

let repeat x num =
  let rec loop acc num =
    if num <= 0 then acc
    else loop (x::acc) (num-1)
  in
  loop [] num

