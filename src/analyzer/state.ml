(* SymFuzz *)

(** analyzer state

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
open Log_analysis
open Big_int_convenience
open Misc
open EArray

type memory_conc_map_t = Ast.exp MemMap.t
type memory_abstr_map_t = (nativeint, inputdep) Hashtbl.t
type variable_conc_map_t = Ast.exp Var.VarMap.t
type variable_abstr_map_t = inputdep Var.VarMap.t

type t =
  {
    (** Memory to concrete *)
    mu_c       : memory_conc_map_t;
    (** Memory to abstract value *)
    mu_a       : memory_abstr_map_t;

    (** Variable to concrete *)
    delta_c    : variable_conc_map_t;
    (** Variable to abstract value *)
    delta_a    : variable_abstr_map_t;

    pc         : address_t;

    (** Inputdep predicate *)
    gamma      : inputdep EArray.t;

    (** Buffer for the updates *)
    gamma_buf  : (inputdep * inputdep) list;

    (** Virtual stack contains post-dominant address in this frame
        (pop the addr when we hit it) *)
    idstack    : (nativeint * inputdep) Stack.t;

    (** The size of the input that we are interested in *)
    input_size : int;

    (** Remember last label for debugging purpose *)
    last_lbl   : Ast.stmt;

    (** Input vector *)
    vector     : Libinput_type.input_vector;

    options    : Options.t option;
    debug_flag : bool;
    logdir     : string;

    (******** Several statistics ********)

    inscount           : int64;
    ilcount            : int64;
    bblcount           : int64;
    tainted_bblcount   : int64;
  }

let default_st =
  {
    mu_c=MemMap.empty;
    mu_a=Hashtbl.create 65536;
    delta_c=Var.VarMap.empty;
    delta_a=Var.VarMap.empty;
    pc=0n;
    gamma=[||];
    gamma_buf=[];
    idstack=Stack.create ();
    input_size=0;
    last_lbl=Ast.Comment ("initial", []);
    vector=Libinput_type.empty_vector;
    options=None;
    debug_flag=false;
    logdir="";

    inscount=0L;
    ilcount=0L;
    bblcount=0L;
    tainted_bblcount=0L;
  }

let state = ref default_st

let get_state () = !state
let set_state st = state := st

let bil_handle = (* TODO: decide the architecture based on the binary *)
  let native_arch = if Sys.word_size = 32 then Arch.X86_32 else Arch.X86_64 in
  LibBil.bil_open ~arch:native_arch None

let rec dependency_to_string = function
  | hd::tl -> Printf.sprintf "%s;%s" (Var.name hd) (dependency_to_string tl)
  | [] -> ""

let new_vector st vector =
  {st with vector=vector}

let new_option st opt =
  {st with options=Some opt}

let new_delta st variable value =
  let value = value_merge value in
  if dep_is_bottom value then
    {st with delta_a=Var.VarMap.remove variable st.delta_a}
  else
    {st with delta_c=Var.VarMap.remove variable st.delta_c;
             delta_a=Var.VarMap.add variable value st.delta_a}

let is_ymms reg =
  let max = 15 in (* XXX *)
  let rec loop idx =
    if idx > max then false
    else if Disasm_i386.R64.ymms.(idx) == reg then true
    else loop (idx+1)
  in
  loop 0

(* XXX: this is a hack due to a bug in BAP for handling YMM *)
let strip_high_ymm v =
  let rec loop cnt = function
    | hd::tl -> if cnt <= 1 then tl else loop (cnt-1) tl
    | [] -> failwith "cannot strip ymm"
  in
  loop 16 v

let update_vdelta st var v =
  let v = if is_ymms var then strip_high_ymm v else v in
  let v = value_merge v in
  {st with delta_a=Var.VarMap.add var v st.delta_a}

let update_context ctxt =
  let build_exp reg =
    Ast.Int ((Big_int_Z.big_int_of_nativeint reg), Type.Reg (Nativeint.size))
  in
  let build_exp_bool reg =
    if reg then Ast.Int (Big_int_convenience.bi1, Type.reg_1)
    else Ast.Int (Big_int_convenience.bi0, Type.reg_1)
  in
  let build_exp_xmm {Context.low=low; Context.high=high} =
    let low = Big_int_convenience.biconst64 low in
    let high = Big_int_convenience.biconst64 high in
    let high = Big_int_Z.shift_left_big_int high 64 in
    let xmm = Big_int_Z.or_big_int high low in
    Ast.Int (xmm, Type.reg_128)
  in
  let eax = build_exp ctxt.Context.eax in
  let ebx = build_exp ctxt.Context.ebx in
  let ecx = build_exp ctxt.Context.ecx in
  let edx = build_exp ctxt.Context.edx in
  let edi = build_exp ctxt.Context.edi in
  let esi = build_exp ctxt.Context.esi in
  let esp = build_exp ctxt.Context.esp in
  let ebp = build_exp ctxt.Context.ebp in
  let eip = build_exp ctxt.Context.eip in
  let eflags = build_exp ctxt.Context.eflags in
  let csseg = build_exp ctxt.Context.cs in
  let dsseg = build_exp ctxt.Context.ds in
  let esseg = build_exp ctxt.Context.es in
  let fsseg = build_exp ctxt.Context.fs in
  let gsseg = build_exp ctxt.Context.gs in
  let ssseg = build_exp ctxt.Context.ss in
  let fsbase = build_exp ctxt.Context.fs_base in
  let gsbase = build_exp ctxt.Context.gs_base in
  let cflag = build_exp_bool ctxt.Context.cflag in
  let pflag = build_exp_bool ctxt.Context.pflag in
  let aflag = build_exp_bool ctxt.Context.aflag in
  let zflag = build_exp_bool ctxt.Context.zflag in
  let sflag = build_exp_bool ctxt.Context.sflag in
  let dflag = build_exp_bool ctxt.Context.dflag in
  let oflag = build_exp_bool ctxt.Context.oflag in
  (* let acflag = build_exp_bool ctxt.Context.acflag in
  let idflag = build_exp_bool ctxt.Context.idflag in *)
  let xmm0 = build_exp_xmm ctxt.Context.xmm0 in
  let xmm1 = build_exp_xmm ctxt.Context.xmm1 in
  let xmm2 = build_exp_xmm ctxt.Context.xmm2 in
  let xmm3 = build_exp_xmm ctxt.Context.xmm3 in
  let xmm4 = build_exp_xmm ctxt.Context.xmm4 in
  let xmm5 = build_exp_xmm ctxt.Context.xmm5 in
  let xmm6 = build_exp_xmm ctxt.Context.xmm6 in
  let xmm7 = build_exp_xmm ctxt.Context.xmm7 in
  (* *)
  let open Disasm_i386 in
  let open Var in
  state := {!state with delta_c = VarMap.empty};
  state := {!state with delta_c = VarMap.add R32.eax eax !state.delta_c};
  state := {!state with delta_c = VarMap.add R32.ebx ebx !state.delta_c};
  state := {!state with delta_c = VarMap.add R32.ecx ecx !state.delta_c};
  state := {!state with delta_c = VarMap.add R32.edx edx !state.delta_c};
  state := {!state with delta_c = VarMap.add R32.edi edi !state.delta_c};
  state := {!state with delta_c = VarMap.add R32.esi esi !state.delta_c};
  state := {!state with delta_c = VarMap.add R32.esp esp !state.delta_c};
  state := {!state with delta_c = VarMap.add R32.ebp ebp !state.delta_c};
  state := {!state with delta_c = VarMap.add R32.eip eip !state.delta_c};
  state := {!state with delta_c = VarMap.add R32.eflags eflags !state.delta_c};
  state := {!state with delta_c = VarMap.add cs csseg !state.delta_c};
  state := {!state with delta_c = VarMap.add ds dsseg !state.delta_c};
  state := {!state with delta_c = VarMap.add es esseg !state.delta_c};
  state := {!state with delta_c = VarMap.add fs fsseg !state.delta_c};
  state := {!state with delta_c = VarMap.add gs gsseg !state.delta_c};
  state := {!state with delta_c = VarMap.add ss ssseg !state.delta_c};
  state := {!state with delta_c = VarMap.add R32.fs_base fsbase !state.delta_c};
  state := {!state with delta_c = VarMap.add R32.gs_base gsbase !state.delta_c};
  state := {!state with delta_c = VarMap.add cf cflag !state.delta_c};
  state := {!state with delta_c = VarMap.add pf pflag !state.delta_c};
  state := {!state with delta_c = VarMap.add af aflag !state.delta_c};
  state := {!state with delta_c = VarMap.add zf zflag !state.delta_c};
  state := {!state with delta_c = VarMap.add sf sflag !state.delta_c};
  state := {!state with delta_c = VarMap.add df dflag !state.delta_c};
  state := {!state with delta_c = VarMap.add oF oflag !state.delta_c};
  (*state := {!state with delta_c = VarMap.add acflag acflag !state.delta_c};
  state := {!state with delta_c = VarMap.add idflag idflag !state.delta_c};*)
  state := {!state with delta_c = VarMap.add R64.ymms.(0) xmm0 !state.delta_c};
  state := {!state with delta_c = VarMap.add R64.ymms.(1) xmm1 !state.delta_c};
  state := {!state with delta_c = VarMap.add R64.ymms.(2) xmm2 !state.delta_c};
  state := {!state with delta_c = VarMap.add R64.ymms.(3) xmm3 !state.delta_c};
  state := {!state with delta_c = VarMap.add R64.ymms.(4) xmm4 !state.delta_c};
  state := {!state with delta_c = VarMap.add R64.ymms.(5) xmm5 !state.delta_c};
  state := {!state with delta_c = VarMap.add R64.ymms.(6) xmm6 !state.delta_c};
  state := {!state with delta_c = VarMap.add R64.ymms.(7) xmm7 !state.delta_c};
  !state

let update_bblcount st numins numils =
  {st with bblcount=Int64.succ st.bblcount;
           inscount=Int64.add st.inscount (Int64.of_int numins);
           ilcount=Int64.add st.ilcount (Int64.of_int numils)}

let update_taintbbl tainted st =
  if tainted then {st with tainted_bblcount=Int64.succ st.tainted_bblcount}
  else st

let cleanup_context st =
  let variable_filter var _value =
    let name = Var.name var in
    if name.[0] = 'T' then false else true
  in
  (* clean up temporary context variables *)
  {st with delta_a=Var.VarMap.filter variable_filter st.delta_a;
           mu_c=MemMap.empty}

let dump_concrete_vars st chan =
  Printf.fprintf chan "---concrete---\n";
  Var.VarMap.iter (fun v e ->
    Printf.fprintf chan "(%s) -> %s\n" (Var.name v) (Pp.ast_exp_to_string e)
  ) st.delta_c

let dump_abstract_vars st chan =
  Printf.fprintf chan "---abstract---\n";
  Var.VarMap.iter (fun v e ->
    Printf.fprintf chan "(%s) -> %s\n" (Var.name v) (dep_to_string e)
  ) st.delta_a;
  Printf.fprintf chan "---tcartsba---\n"

let dump_vars st chan =
  dump_concrete_vars st chan;
  dump_abstract_vars st chan

let dump_concrete_mem st chan =
  Printf.fprintf chan "---concrete---\n";
  MemMap.iter (fun v e ->
    Printf.fprintf chan "(%nx) -> %s\n" v (Pp.ast_exp_to_string e)
  ) st.mu_c

let dump_abstract_mem st chan =
  Printf.fprintf chan "---abstract---\n";
  Hashtbl.iter (fun v e ->
    Printf.fprintf chan "(%nx) -> %s\n" v (dep_to_string e)
  ) st.mu_a;
  Printf.fprintf chan "---tcartsba---\n"

let dump_mem st chan =
  dump_concrete_mem st chan;
  dump_abstract_mem st chan

let merge_gamma st gamma myvalue newvalue =
  dep_fold (fun pos acc ->
    let set = EArray.get gamma pos in
    EArray.set gamma pos (dep_union set newvalue);
    acc
  ) myvalue gamma, st

let update_gamma_with_merge st myvalue newvalue =
  let gamma, st = merge_gamma st st.gamma myvalue newvalue in
  {st with gamma=gamma}

let update_gamma st myvalue newvalue =
  if dep_is_bottom newvalue then st
  else update_gamma_with_merge st myvalue newvalue

let stash_gamma st myvalue newvalue =
  {st with gamma_buf=((myvalue,newvalue)::st.gamma_buf)}

let clear_stash st =
  {st with gamma_buf=[]}

let update_gamma_from_stashed st =
  let st =
    List.fold_left (fun st (myvalue, newvalue) ->
      update_gamma st myvalue newvalue
    ) st st.gamma_buf
  in
  clear_stash st

let dump_gamma st chan =
  logf "----final----";
  let cnt, total =
    EArray.fold_left (fun (cnt, total) dep ->
      Printf.fprintf chan "[%04Ld] %d {%s}\n" cnt
        (dep_cardinal dep) (dep_to_string dep);
      let n = dep_cardinal dep |> Int64.of_int in
      (if n > 0L then Int64.add cnt 1L else cnt), Int64.add n total
    ) (0L,0L) st.gamma
  in
  let dumpfile = Filename.concat st.logdir "gamma.dmp" in
  let mch = open_out dumpfile in
  let () = Marshal.to_channel mch st.gamma [] in
  let () = close_out mch in
  try logf "avg: %Ld (%Ld / %Ld)" (Int64.div total cnt) total cnt
  with Division_by_zero -> logf "avg: 0 (0 / 0)"

let dump_stats st chan total_bblcnt =
  Printf.fprintf chan "----stats----\n";
  Printf.fprintf chan "instruction count     : %Ld\n" (st.inscount);
  Printf.fprintf chan "BAP IL      count     : %Ld\n" (st.ilcount);
  Printf.fprintf chan "basic block count     : %ld\n" (total_bblcnt);
  Printf.fprintf chan "tainted block count   : %Ld\n" (st.bblcount);
  flush chan

let dump_mem_info st chan =
  Printf.fprintf chan "mu_a: %d\n" (Hashtbl.length st.mu_a);
  flush chan

let input_to_abstract value source =
  let to_value idx = function
    | Libinput_value.Concrete _ -> dep_empty, false
    | Libinput_value.Symbolic _ -> dep_singleton idx, true
  in
  let value, symbolic =
    Libinput_value.value_fold (fun (acc, symb) idx v ->
      let v, has_symbolic = to_value idx v in
      v::acc, if has_symbolic then true else symb
    ) ([], false) value
  in
  if symbolic then List.rev value else []

let new_mu st addr value totalsize =
  if dep_is_bottom value then (Hashtbl.remove st.mu_a addr; st)
  else (Hashtbl.add st.mu_a addr value; st)

let introduce st pos addr len totalsize value source =
  let rec loop st addr max values skip =
    if addr < max then
      match values with
      | value::tl ->
          if skip > 0n then
            loop st addr max tl (Nativeint.pred skip)
          else
            let st = new_mu st addr value totalsize in
            loop st (addr +< 1n) max tl 0n
      | [] ->
          failwith "bad input value"
    else st
  in
  logf "introducing %s @ %nx (%d) [%nd] %d" source addr len pos totalsize;
  let len = Nativeint.of_int len in
  let values = input_to_abstract value source in
  if (List.length values) = 0 then
    st
  else
    let newgamma = EArray.make totalsize dep_empty in
    let st = {st with input_size=totalsize; gamma=newgamma} in
    loop st addr (addr +< len) values pos

let rec introduce_arguments tid argvp argi st =
  let argp = Memory.get_nativeint_from_addr tid argvp in
  if argp = 0n then
    st
  else
    let st = new_arg argp argi tid st in
    introduce_arguments tid (argvp +< Misc.addr_sizen) (argi+1) st
and new_arg argp argi tid st =
  let arg = Memory.get_string tid argp in
  let source = "argv"^(string_of_int argi) in
  let l = String.length arg in
  introduce st 0n argp l l (Libinput.find_arg_val argi st.vector) source

let introduce_files st addr len totalsize byte_pos path =
  let source = "file["^(path)^"]" in
  let v = Libinput.find_file_val path st.vector in
  introduce st byte_pos addr len totalsize v source

let introduce_environment st envc envp =
  let open Memory in
  let rec loop st cnt envpp =
    if cnt >= envc then st
    else
      (* TODO implement environment handling *)
      (* let envp = get_nativeint_from_addr envpp in
      let env = get_string envp in
      let () = debugf "env(%nx) -> %s" envp env in *)
      loop st (Int32.add cnt 1l) (envpp +< Misc.addr_sizen)
  in
  loop st 0l envp

let set_pc st addr =
  {st with pc=addr}

let get_pc = function
  | {pc=pc;} -> pc

let set_conc_mu st addr v =
  let rec for_each_byte st pos = function
    | Ast.Int (n,Type.Reg b) ->
        let mask = (biconst 0xff) <<% pos in
        if pos < b then
          let byte = ((n &% mask) >>% pos) in
          let byte = Ast.Int (byte, Type.reg_8) in
          let addr = addr +< (Nativeint.of_int (pos/8)) in
          for_each_byte {st with mu_c=MemMap.add addr byte st.mu_c} (pos+8) v
        else st
    | _ -> failwith "wrong address format"
  in
  for_each_byte st 0 v

let get_conc_mu st addr typ =
  let rec concat_bytes acc bitsize =
    if bitsize < 0 then acc
    else
      let bytesize = Nativeint.of_int (bitsize / 8) in
      let byte = MemMap.find (addr +< bytesize) st.mu_c in
      let byte =
        match byte with
        | Ast.Int (v,t) when t = Type.reg_8 -> v
        | _ -> failwith "wrong memory value"
      in
      concat_bytes (acc |% (byte <<% bitsize)) (bitsize-8)
  in
  match typ with
  | Type.Reg b ->
      let s = concat_bytes bi0 (b - 8) in
      Ast.Int (s,typ)
  | _ -> failwith "wrong address format"

let shrink_abstr (v: inputdep list) size =
  let u = value_merge v in
  repeat u size

let set_abstr_mu st addr v size =
  let lastaddr = addr +< (Nativeint.of_int size) in
  let len = List.length v in
  let v = (* XXX: this is a hack for handling byte-level unknown exp *)
    if len <> 0 && len > size then shrink_abstr v size
    else if len <> 0 && len < size then failwith "bad eval"
    else v
  in
  let rec setloop addr = function
    | [] ->
        if addr >= lastaddr then st
        else (Hashtbl.remove st.mu_a addr; setloop (addr +< 1n) [])
    | v::tl ->
        (Hashtbl.replace st.mu_a addr v; setloop (addr +< 1n) tl)
  in
  setloop addr v

let get_abstr_mu st addr typ =
  let rec concat_bytes acc bitsize : (inputdep list) =
    if bitsize < 0 then acc
    else begin
      let bytesize = Nativeint.of_int (bitsize / 8) in
      let target_addr = addr +< bytesize in
      if Hashtbl.mem st.mu_a target_addr then begin
        let abstr_byte = Hashtbl.find st.mu_a target_addr in
        if dep_is_bottom abstr_byte then
          concat_bytes (dep_empty::acc) (bitsize-8)
        else
          concat_bytes (abstr_byte::acc) (bitsize-8)
      end else
        concat_bytes (dep_empty::acc) (bitsize-8)
    end
  in
  match typ with
  | Type.Reg b -> concat_bytes [] (b - 8)
  | _ -> failwith "wrong address format"

let set_delta st var v =
  {st with delta_c=Var.VarMap.add var v st.delta_c}

let get_delta st var =
  Var.VarMap.find var st.delta_c

let get_nbytes_from_typ t =
  let n = Arithmetic.bits_of_width t in
  if n < 8 then 1
  else n / 8

let set_vdelta st var v =
  let v = value_merge v in
  {st with delta_a=Var.VarMap.add var v st.delta_a}

let get_vdelta st var =
  let v =
    if Var.VarMap.mem var st.delta_a then Var.VarMap.find var st.delta_a
    else dep_empty
  in
  let nbyte = get_nbytes_from_typ (Var.typ var) in
  if nbyte = 32 then
    (* XXX: this is a hack due to a bug in BAP for handling YMM *)
    List.rev_append (repeat dep_empty 16) (repeat v 16)
  else
    repeat v nbyte

let get_var_value st var =
  let delta_c = st.delta_c in
  let concrete =
    try
      Var.VarMap.find var delta_c
    with Not_found ->
      (* XXX hack *)
      if Var.name var = "R_FPU_CONTROL" then Ast.Int (bi0, Type.reg_16)
      else failwith ("variable "^(Var.name var)^" not found")
  in
  concrete, get_vdelta st var

let get_gamma st = st.gamma

let set_last_lbl st lbl = {st with last_lbl=lbl}

let get_last_lbl {last_lbl=last_lbl;} = Pp.ast_stmt_to_string last_lbl

let is_debugging st = st.debug_flag

let set_debugging flag st = {st with debug_flag=flag}

let set_logdir logdir st = {st with logdir=logdir}

let get_input_size {input_size=s} = s

let get_idstack {idstack=idstack} = idstack

let label_address = function
  | Type.Addr n ->
      Some (Bapmisc.safe_bigint_to_native n)
  | Type.Name s when String.sub s 0 3 = "pc_" ->
      Some (Nativeint.of_string (String.sub s 3 (String.length s - 3)))
  | Type.Name _ ->
      None

let esp = Disasm_i386.R32.esp

