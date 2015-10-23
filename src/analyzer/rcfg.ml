(* SymFuzz *)

(** CFG recovery

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

open Bapmisc
open Misc
open Dependency
open Pinapi
open Log_analysis
open Big_int_convenience
open Ast
open LibBil

type recovery_st =
  {
    tid      : int;
    cfg      : Cfg.AST.G.t;
    entry    : Cfg.AST.G.vertex;
    exit     : Cfg.AST.G.vertex;
    ret      : Cfg.AST.G.vertex;
  }

let get_stmt (buf, addr) =
  stmt_of_seq State.bil_handle buf (safe_bigint_of_native addr)

let get_bap_stmt_from_buf =
  let stmt_cache = BatCache.lru_cache ~gen:get_stmt ~cap:61 in
  fun buf (naddr: nativeint) ->
    try let bap = stmt_cache (buf, naddr) in Some bap
    with _ -> None

let maxins_len = 32

let contains_jump prog =
  let jmp = function
    | Jmp (_,_) | CJmp (_,_,_,_) | Halt (_,_) -> true
    | _ -> false
  in
  List.exists jmp prog

let rec block_loop acc sizeacc buf addr naddr =
  match get_bap_stmt_from_buf buf naddr with
  | Some (il, size) ->
      let bufsize = String.length buf in
      if contains_jump il then ((il, size)::acc), (sizeacc +% size)
      else if bufsize < maxins_len then acc, sizeacc
      else
        let sizei = Big_int_Z.int_of_big_int size in
        let buf' = String.sub buf sizei (bufsize - sizei) in
        block_loop
          ((il, size)::acc)
          (sizeacc +% size)
          (buf')
          (addr +% size)
          (naddr +! (Nativeint.of_int sizei))
  | None ->
      acc, sizeacc

let get_block (tid, maxbuf, addr) =
  let naddr = safe_bigint_to_native addr in
  let buf = get_mem tid naddr maxbuf in
  let ils, size = block_loop [] bi0 buf addr naddr in
  List.rev ils, size

let get_bap_block =
  let maxbuf = 512 in (* XXX: this is large enough in most cases *)
  let cache = BatCache.lru_cache ~gen:get_block ~cap:1117 in
  fun tid addr -> cache (tid, maxbuf, addr)

let get_next_addrs addr size target1 target2 =
  let next = addr +% size in
  match target1, target2 with
  | Int (t1, _), Int (t2, _) ->
      if t1 = next then [next; t2]
      else if t2 = next then [next; t1]
      else [t1; t2]
  | Int (t1, _), _ ->
      if t1 = next then [next] else [next; t1]
  | _, Int (t2, _) ->
      if t2 = next then [next] else [next; t2]
  | _, _ ->
      [next]

let get_short_addr = function
  | Int (t, _) -> Some t, false
  | Lab str -> None, (String.sub str 0 6) = "nocjmp"
  | e -> None, false

let add_edge cfg v_from v_to =
  (* reverse cfg *)
  Cfg.AST.add_edge cfg v_to v_from

let add_exit_node blk st =
  match blk with
  | Some blk ->
      let cfg = add_edge st.cfg blk st.exit in {st with cfg=cfg}
  | None ->
      st

let get_current_blk st stmt parent visited blkaddr = function
  | Some _ as blk -> st, blk, visited
  | None ->
      let cfg, vertex = Cfg.AST.create_vertex st.cfg [stmt] in
      let visited = BIM.add blkaddr vertex visited in
      let cfg = add_edge cfg parent vertex in
      {st with cfg=cfg}, Some vertex, visited

let push_stack addr currblk addr_stack =
  Stack.push (addr, BatOption.get_exn currblk Not_found) addr_stack

let cjmp st target is_fallthrough addr_stack currblk fallthrough =
  match target with
  | None ->
      if is_fallthrough then
        let () = push_stack fallthrough currblk addr_stack in st
      else
        add_exit_node currblk st
  | Some target ->
      let () = push_stack target currblk addr_stack in st

let jmp st e currblk addr_stack =
  let addr, _nocjmp = get_short_addr e in
  let st =
    match addr with
    | None ->
        add_exit_node currblk st
    | Some addr ->
        let () = push_stack addr currblk addr_stack in
        st
  in
  st, None, true

let call st currblk fallthrough addr_stack =
  let () = push_stack fallthrough currblk addr_stack in
  st, None, true

let ret blk st =
  match blk with
  | Some blk ->
      let cfg = add_edge st.cfg blk st.ret in
      {st with cfg=cfg}, None, true
  | None ->
      st, None, true

let build_from_stmt st currblk exited addr_stack addr fallthrough = function
  | Move (_,_,_) -> st, currblk, exited
  | Jmp (e,attrb) ->
      if is_call attrb then call st currblk fallthrough addr_stack
      else if is_ret attrb then ret currblk st
      else jmp st e currblk addr_stack
  | CJmp (_,e1,e2,_) ->
      let t1, f1 = get_short_addr e1 in
      let t2, f2 = get_short_addr e2 in
      let st = cjmp st t1 f1 addr_stack currblk fallthrough in
      let st = cjmp st t2 f2 addr_stack currblk fallthrough in
      st, currblk, true
  | Label (_,_) -> st, currblk, exited
  | Special (_,_,_) -> st, currblk, exited
  | Assert (_,_) -> st, currblk, exited
  | Halt (_,_) -> add_exit_node currblk st, currblk, true
  | s ->
      failwith ("unsupported statements encountered while building cfg"
              ^ (Pp.ast_stmt_to_string s))

let rec build_cfg st addr_stack addr parent visited =
  let prog, size = get_bap_block st.tid addr in
  let rec inner_loop st currblk exited addr fallthrough visited = function
    | stmt::tl ->
        let st, currblk, visited =
          get_current_blk st stmt parent visited addr currblk
        in
        let st, currblk, exited =
          build_from_stmt st currblk exited addr_stack addr fallthrough stmt
        in
        if exited then st, currblk, exited, visited
        else inner_loop st currblk exited addr fallthrough visited tl
    | [] ->
        st, currblk, exited, visited
  in
  let rec build_loop st currblk exited nextaddr visited = function
    | (stmts, inst_size)::tl ->
        let fallthrough = nextaddr +% inst_size in
        let st, currblk, exited, visited =
          inner_loop st currblk exited nextaddr fallthrough visited stmts
        in
        if exited then st, exited, visited
        else build_loop st currblk exited fallthrough visited tl
    | [] -> st, exited, visited
  in
  match prog with
    | [] -> st, visited
    | prog -> begin
        let st, is_fin, visited =
          build_loop st None false addr visited prog
        in
        if is_fin then
          st, visited
        else
          let nextaddr = addr +% size in
          build_cfg st addr_stack nextaddr parent visited
      end

let rec resolve_loop st addr_stack visited =
  if Stack.is_empty addr_stack then
    st
  else
    let addr, parent = Stack.pop addr_stack in
    if BIM.mem addr visited then begin
      let vertex = BIM.find addr visited in
      let cfg = add_edge st.cfg parent vertex in
      resolve_loop {st with cfg=cfg} addr_stack visited
    end else begin
      let st, visited =
        build_cfg st addr_stack addr parent visited
      in
      resolve_loop st addr_stack visited
    end

module Dom = Dominator.Make(Cfg.AST.G)

let compute_idom = Dom.compute_idom

let get_bap_stmt tid naddr =
  let maxbuf = 16 in
  let buf = get_mem tid naddr maxbuf in
  match get_bap_stmt_from_buf buf naddr with
  | Some (il, size) -> il, size
  | None -> failwith "failed to obtain bap stmt"

let find_addr cfg pidom =
  let rec find_addr addr = function
    | Label (lbl,_)::_ -> State.label_address lbl
    | _::tl -> find_addr addr tl
    | [] -> addr
  in
  match find_addr None (Cfg.AST.get_stmts cfg pidom) with
  | None -> UnknownPdom
  | Some addr -> Pdom addr

let get_postidoms (tid, (naddr: nativeint), target1, target2) =
  let _il, inssize = get_bap_stmt tid naddr in
  let baddr = biconst64 (get_clean_addr naddr) in
  (* list of potential jump addrs *)
  let addrs = get_next_addrs baddr inssize target1 target2 in
  let cfg = Cfg.AST.empty () in
  let cfg, entry = Cfg_ast.create_entry cfg in
  let cfg, exit = Cfg_ast.find_exit cfg in (* will create an exit node *)
  let cfg, ret =
    Cfg.AST.create_vertex cfg [Label (Type.Addr bi0, [Type.Asm "ret"])]
  in
  let cfg = add_edge cfg ret exit in
  let st =
    {
      tid=tid;
      cfg=cfg;
      entry=entry;
      exit=exit;
      ret=ret;
    }
  in
  let addrstack = Stack.create () in
  let () = List.iter (fun addr -> Stack.push (addr, entry) addrstack) addrs in
  let st = resolve_loop st addrstack BIM.empty in
  (* cfg_debugging st; *)
  let ret =
    try begin
      let pidom = (compute_idom st.cfg) st.exit st.entry in
      if pidom = st.exit then UnknownPdom
      else if pidom = st.ret then Return
      else find_addr st.cfg pidom
    end with Not_found ->
      UnknownPdom
  in
  ret

let cfg_cache = BatCache.lru_cache ~gen:get_postidoms ~cap:997

(* TODO : handle tid for threaded apps *)
let get_postidoms tid addr target1 target2 =
  cfg_cache (tid, addr, target1, target2)

