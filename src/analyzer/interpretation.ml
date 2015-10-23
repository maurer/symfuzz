(* SymFuzz *)

(** main interpretation

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

open Ast
open Type
open Big_int_Z
open Big_int_convenience
open Dependency
open Log_analysis
open State
open Misc
open Bapmisc
open LibBil

let jitting (addr, bytes, size) =
  let total_size = String.length bytes in
  let () = assert (Int32.of_int total_size = size) in
  of_bytesequence bil_handle bytes addr, big_int_of_int total_size

let jit =
  let cache = BatCache.lru_cache ~gen:jitting ~cap:6217 in
  fun addr bytes size ->
    let addr = biconst64 (get_clean_addr addr) in
    cache (addr, bytes, size)

let print_stmt stmt =
  logf "%s" (Pp.ast_stmt_to_string stmt)

let get_concrete_tuple = function
  | Int (v,t) -> v,t
  | e ->
      failwith (Printf.sprintf "invalid concrete value: %s"
                               (Pp.ast_exp_to_string e))

let load_mem st tid index typ =
  let open Memory in
  let index =
    match index with | Int (n,_) -> n | _ -> failwith "symbolic index"
  in
  let index = Bapmisc.safe_bigint_to_native index in
  let conc =
    try
      get_conc_mu st index typ
    with Not_found ->
      let i =
        match typ with
        | Type.Reg b when b = 8 ->
            big_int_of_int (int_of_char (get_memory_byte tid index))
        | Type.Reg b ->
            bytes_to_big_int (get_memory tid index (b/8))
        | _ -> failwith "do not support other index type"
      in
      Ast.Int (i, typ)
  in
  let abstr = get_abstr_mu st index typ in
  conc, abstr

let store_mem st index conc abstr typ =
  let index, _ = get_concrete_tuple index in
  let index = Bapmisc.safe_bigint_to_native index in
  let size = (Arithmetic.bits_of_width typ) / 8 in
  let st = set_abstr_mu st index abstr size in
  match conc with
  | Ast.Int (n,t) as e ->
      set_conc_mu st index e
  | _ -> st

let binop_abstract nbyte op e1 e2 a1 a2 =
  (* abstract binop semantic *)
  let binop a b =
    match a, b with
    | [], a -> a
    | b, [] -> b
    | a, b -> List.map2 dep_union a b
  in
  match op with
  | XOR when e1 = e2 -> repeat dep_empty nbyte
  | _ -> binop a1 a2

let take_n n lst =
  let rec loop acc n = function
    | [] ->
        if n <= 0 then List.rev acc else failwith "cannot take N"
    | hd::tl ->
        if n <= 0 then List.rev acc else loop (hd::acc) (n-1) tl
  in
  loop [] n lst

let cast_abstract abstr t =
  let nbytes = get_nbytes_from_typ t in
  let existing = List.length abstr in
  if nbytes > existing then
    let merged = value_merge abstr in
    let more = repeat merged (nbytes-existing) in
    abstr @ more
  else
    take_n nbytes abstr

let extract_abstr h l abstr =
  let n = (h -% l) +% bi1 in
  let nt = Type.Reg (int_of_big_int n) in
  cast_abstract abstr nt

let concat_abstract a1 a2 = a1 @ a2

let symbolic_memory st idx value =
  let idx = value_merge idx in
  (* overtainting  policy *)
  let value = List.map (fun dep -> dep_union idx dep) value in
  update_gamma st idx idx, value

let is_true_condition = function
  | Ast.Int (i,t) when i = bi0 && t = Type.reg_1 -> false
  | Ast.Int (i,t) when t = Type.reg_1 -> true
  | e ->
      Printf.eprintf "WHY: %s\n" (Pp.ast_exp_to_string e);
      failwith "wrong conditional exp"

let merge_push idstack addr dep =
  if Stack.is_empty idstack then
    Stack.push (addr, dep) idstack
  else begin
    let top_addr, top_dep = Stack.top idstack in
    if top_addr = 0n || (top_addr = addr) then
      let _ = Stack.pop idstack in
      Stack.push (addr, dep_union dep top_dep) idstack
    else if addr = 0n then
      let _ = Stack.pop idstack in
      Stack.push (top_addr, dep_union dep top_dep) idstack
    else
      Stack.push (addr, dep) idstack
  end

let merge_dependence idstack dep =
  if Stack.is_empty idstack then
    dep
  else begin
    let _old_addr, old_dep = Stack.top idstack in
    let new_dep = dep_union old_dep dep in
    new_dep
  end

let update_pdom_stack postidoms abstr idstack st =
  match postidoms with
  | Pdom pd ->
      if Stack.is_empty idstack then
        let () = Stack.push (pd, abstr) idstack in st
      else
        let () = merge_push idstack pd abstr in st
  | UnknownPdom ->
      let () = merge_push idstack 0n abstr in st
  | Return ->
      let () = merge_push idstack 0n abstr in st

let conditional_update tid st abstr target1 target2 =
  if is_tainted abstr then begin
    let abstr = value_merge abstr in
    let idstack = get_idstack st in
    let abstr' = merge_dependence idstack abstr in
    let postidoms = Rcfg.get_postidoms tid (get_pc st) target1 target2 in
    let st = stash_gamma st abstr abstr' in
    update_pdom_stack postidoms abstr' idstack st
  end else
    st

let eval_stmts stmts tid st =
  (* eval statement *)
  let rec eval_stmt st = function
    | Move (v,e,_) ->
        let conc, abstr, st = eval_expr st e in
        let st = set_delta st v conc in
        let st = update_vdelta st v abstr in
        st, None, is_tainted abstr
    | Jmp (e,attrb) ->
        let target, _abstr, st = eval_expr st e in
        st, None, false
    | CJmp (c,e1,e2,_) ->
        let cond, abstr, st = eval_expr st c in
        let target1, _, st = eval_expr st e1 in
        let target2, _, st = eval_expr st e2 in
        let st = conditional_update tid st abstr target1 target2 in
        let nextlbl = if is_true_condition cond then e1 else e2 in
        st, Some nextlbl, false
    | Label (lbl,_) as stmt ->
        let addr = label_address lbl in
        (match addr with
          | Some addr ->
              let st = set_last_lbl st stmt in
              set_pc st addr
          | None -> st
        ), None, false
    | Special (s,_,_) ->
        st, None, false
    | Assert (_,_) ->
        st, None, false
    | s ->
        failwith ("unsupported statements: "^(Pp.ast_stmt_to_string s))
  (* eval expression : returns (concrete value, abstract value, state) *)
  and eval_expr st = function
    | Load (mem,idx,_endian,typ) ->
        let index, abstr_idx, st = eval_expr st idx in
        let loaded, abstr = load_mem st tid index typ in
        let st, abstr =
          if is_tainted abstr_idx then symbolic_memory st abstr_idx abstr
          else st, abstr
        in
        loaded, abstr, st
    | Store (mem,idx,v,_endian,typ) ->
        let index, abstr_idx, st = eval_expr st idx in
        let conc, abstr, st = eval_expr st v in
        let st, abstr =
          if is_tainted abstr_idx then symbolic_memory st abstr_idx abstr
          else st, abstr
        in
        let st = store_mem st index conc abstr typ in
        Ast.exp_false, AbstrVal.bottom, st
    | BinOp (op,e1,e2) ->
        let conc1, abstr1, st = eval_expr st e1 in
        let conc2, abstr2, st = eval_expr st e2 in
        let conc1 = get_concrete_tuple conc1 in
        let conc2 = get_concrete_tuple conc2 in
        let conc, typ = Arithmetic.binop op conc1 conc2 in
        let conc = Int (conc, typ) in
        let nbyte = get_nbytes_from_typ typ in
        let abstr = binop_abstract nbyte op e1 e2 abstr1 abstr2 in
        conc, abstr, st
    | UnOp (typ,e) ->
        let conc, abstr, st = eval_expr st e in
        let conc = get_concrete_tuple conc in
        let conc, typ' = Arithmetic.unop typ conc in
        Int (conc, typ'), abstr, st
    | Var v ->
        let conc, abstr = State.get_var_value st v in
        conc, abstr, st
    | Lab _ as l ->
        l, AbstrVal.bottom, st
    | Int (v,typ) as e ->
        let nbyte = get_nbytes_from_typ typ in
        e, repeat dep_empty nbyte, st
    | Cast (cast,typ,e) ->
        let conc,abstr,st = eval_expr st e in
        let conc = get_concrete_tuple conc in
        let conc, typ' = Arithmetic.cast cast conc typ in
        let abstr = cast_abstract abstr typ in
        Int (conc,typ'), abstr, st
    | Let (v,e1,e2) -> (* let v = e1 in e2 *)
        let conc1,abstr1,st = eval_expr st e1 in
        let st = set_delta st v conc1 in
        let st = update_vdelta st v abstr1 in
        eval_expr st e2
    | Ite (cond,e1,e2) ->
        let cond,_abstr,st = eval_expr st cond in
        if is_true_condition cond then
          eval_expr st e1
        else
          eval_expr st e2
    | Extract (h,l,e) ->
        let conc,abstr,st = eval_expr st e in
        let conc = get_concrete_tuple conc in
        let conc,typ = Arithmetic.extract h l conc in
        let abstr = extract_abstr h l abstr in
        Int (conc,typ), abstr, st
    | Concat (e1,e2) ->
        let conc1,abstr1,st = eval_expr st e1 in
        let conc2,abstr2,st = eval_expr st e2 in
        let conc1 = get_concrete_tuple conc1 in
        let conc2 = get_concrete_tuple conc2 in
        let conc,typ = Arithmetic.concat conc1 conc2 in
        let conc = Int (conc, typ) in
        let abstr = concat_abstract abstr1 abstr2 in
        conc, abstr, st
    | Unknown (_,_) ->
        Ast.exp_false, AbstrVal.bottom, st
  in

  let rec eval_loop st nextlbl tainted = function
    | stmt::tl ->
        if seek_lbl nextlbl stmt = KeepDoing then
          eval_loop st nextlbl tainted tl
        else
          let st, nextlbl, newtaint = eval_stmt st stmt in
          eval_loop st nextlbl (tainted || newtaint) tl
    | [] -> st, tainted
  in
  eval_loop st None false stmts

let continue = 0
let stop_and_debug = 1

let flushall () =
  Printexc.print_backtrace stderr;
  flush stderr;
  flush (get_chan ())

let flatten_block block blksize =
  let blksize = int_of_big_int blksize in
  let rec loop acc total = function
    | (stmt_lst, size)::rest ->
        let size = int_of_big_int size in
        if total >= blksize then acc
        else loop (List.rev_append stmt_lst acc) (total+size) rest
    | [] -> acc
  in
  loop [] 0 block |> List.rev

let execute block blksize raw_context block_addr tid =
  let inscount = List.length block in
  let stmts = flatten_block block blksize in
  (* List.iter print_stmt stmts *)
  let ctxt = Context.get_context raw_context in
  let st = State.update_context ctxt in
  let st = State.update_bblcount st inscount (List.length stmts) in
  try
    let st = cleanup_context st in
    let st, tainted = eval_stmts stmts tid st in
    let st = State.update_taintbbl tainted st in
    let () = set_state st in
    continue
  with
    | Memory.Invalid_memory addr ->
        Printf.eprintf "FATAL ERROR: Invalid Memory Access @ %nx\n" addr;
        logf "@ blk addr: %nx\n" (block_addr);
        flushall ();
        stop_and_debug
    | e ->
        Printf.eprintf "FATAL ERROR: %s\n" (Printexc.to_string e);
        logf "@ blk addr: %nx\n" (block_addr);
        flushall ();
        stop_and_debug

