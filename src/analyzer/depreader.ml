(* SymFuzz *)

(** Dependency Reader

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

open Dependency
open Misc
open EArray

type pred = inputdep EArray.t

let get_dist file =
  (* distribution of b, derived from a large-scale fuzzing experiment *)
  let dist = readlines file |> List.map int_of_string in
  let maxb = List.fold_left (fun max b -> if max < b then b else max) 0 dist in
  let avg = (List.fold_left (fun acc b -> float b +. acc) 0.0 dist)
         /. (float (List.length dist))
  in
  let howmany i dist =
    List.fold_left (fun cnt b -> if b=i then cnt+1 else cnt) 0 dist
  in
  let rec probloop acc i =
    if i > maxb then acc
    else
      let p = float (howmany i dist) /. float (List.length dist) in
      if p > 0.0 then probloop (p::acc) (i+1) else probloop acc (i+1)
  in
  let distprob = probloop [] 1 in
  distprob, maxb, avg

let get_b_from_dist distprob maxb =
  let r = Random.float 1.0 in
  let rec get_b (acc, b) = function
    | [] -> b
    | _::[] -> b
    | p::tl -> let p = p +. acc in if p >= r then b else get_b (p, b+1) tl
  in
  get_b (0.0, 1) distprob

let read_pred fname =
  let ch = open_in fname in
  let pred = Marshal.from_channel ch in
  close_in ch;
  pred

let print_d pred =
  let idx = ref 0 in
  EArray.iter (fun dep ->
    idx := !idx + 1;
    Printf.printf "%4d -> %d\n" !idx ((dep_cardinal dep))
  ) pred

let sqr x = x *. x

let stddev l =
  let n, sx, sx2 =
    List.fold_left
      (fun (n, sx, sx2) x -> succ n, sx +. x, sx2 +. sqr x)
      (0, 0., 0.) l
  in
  sqrt ((sx2 -. sqr sx /. float n) /. float n)

(* how close to the average? *)
let within_range avg sdv lst total =
  let total = Int64.to_int total in
  let cnt =
    List.fold_left (fun (cnt) x ->
      if x >= (avg -. sdv) && x <= (avg +. sdv) then cnt + 1
      else cnt
    ) (0) lst
  in
  float cnt /. float total *. 100.0

let print_stat n (pred:pred) bar_d avgb =
  let total, lst, nz_cnt =
    EArray.fold_left (fun (total, lst, nz_cnt) dep ->
      let n = (dep_cardinal dep) |> Int64.of_int in
      Int64.add n total,
      (Int64.to_float n)::lst,
      if n > 0L then nz_cnt + 1 else nz_cnt
    ) (0L, [], 0) pred
  in
  let nb = Int64.mul n 8L in
  let avgf = (Int64.to_float total) /. (Int64.to_float n) in
  let nzavg = (Int64.to_float total) /. (float nz_cnt) in
  let sdv = stddev lst in
  let rate = within_range avgf sdv lst n in
  Printf.printf "total_d: %Ld, N: %Ld(%Ld), avg: %.1f(%.1f), nzavg: %.1f(%.1f), std.dev: %.2f(%.1f), rate: %.2f\n"
    total (n) (nb) (avgf) (avgf *. 8.0) nzavg (nzavg *. 8.0)
    (sdv) (sdv *. 8.0) rate;
  let n = Int64.to_float n in
  let r =
    if bar_d < avgb then avgb /. n else ((1.0) *. (n +. 1.0) /. n /. bar_d)
  in
  print_endline "bar_d,r";
  Printf.printf "%.f,%.3f\n" bar_d r

let set = Hashtbl.create 7901
(** floyd's algorithm for random k subset *)
let floyds_sampling n k =
  Hashtbl.clear set;
  for j = n - k + 1 to n do
    let t = (Random.int j) + 1 in
    if Hashtbl.mem set t then (Hashtbl.add set j true)
    else (Hashtbl.add set t true)
  done;
  set

let _ =
  let () = Printexc.record_backtrace true in
  let fname = try Sys.argv.(1) with _ -> failwith "filename not given" in
  let distfile = try Sys.argv.(2) with _ -> failwith "distfile not given" in
  let prog = try Sys.argv.(3) with _ -> failwith "prog name not given" in
  let pred : pred = read_pred fname in
  Printf.printf "Read gamma for %s ..." prog; flush stdout;
  let distprob, maxb, avgb = get_dist distfile in
  print_endline "Got probability distribution of b..."; flush stdout;
  (* print_d pred; *)
  let n = (Array.fold_left (fun n a -> (Array.length a) + n) 0 pred)
          |> Int64.of_int
  in
  (* compute average d from the distribution *)
  let rec average_loop () =
    let n = (Int64.to_int n) in
    let avg, cnt = estimation_loop 0.0 1.0 n in
    Printf.printf "%f, %d\n" avg cnt; avg
  and estimation_loop sum cnt n =
    let b = get_b_from_dist distprob maxb in
    let bs = floyds_sampling (n*8) b in
    let dep = Hashtbl.fold (fun k v acc -> dep_union (EArray.get pred ((k-1)/8)) acc
                           ) bs dep_empty
    in
    let c = dep_cardinal dep in
    if c = 0 then
      estimation_loop sum cnt n
    else begin
      let d = float_of_int c in
      let b = float_of_int b in
      let nextsum = sum +. d in
      let nextcnt = cnt +. b in
      let next = nextsum /. nextcnt in
      let prev = sum /. cnt in
      (* Printf.printf "%f, %f, b: %f, next: %f, prev: %f\n" nextcnt d b next prev;
      flush stdout; *)
      if abs_float (next -. prev) < 0.0000001 then next, int_of_float nextcnt
      else estimation_loop nextsum nextcnt n
    end
  in
  let avg = average_loop () in
  Printf.printf "Got the average d ... %f\n" avg; flush stdout;
  print_stat n pred avg avgb

