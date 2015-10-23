(* SymFuzz *)

(** extended array to handle large number of array entries in 32-bit platform

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

module EArray = struct
  type 'a t = 'a array array

  let maxlen = 1048576

  let getpos pos =
    let idx = pos / maxlen in
    let rem = pos mod maxlen in
    idx, rem

  let get arr pos : 'a =
    let idx, rem = getpos pos in
    Array.get (Array.get arr idx) rem

  let set arr pos v =
    let idx, rem = getpos pos in
    Array.set (Array.get arr idx) rem v

  let fold_left fn acc arr =
    Array.fold_left (fun acc arr ->
      Array.fold_left fn acc arr
    ) acc arr

  let iter fn arr =
    Array.iter (fun arr ->
      Array.iter fn arr
    ) arr

  let make size v =
    let lastidx, lastsize = getpos size in
    Array.init (lastidx+1) (fun idx ->
      if idx = lastidx then Array.make lastsize v
      else Array.make maxlen v
    )

end

