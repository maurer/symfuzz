(* SymFuzz *)

(** analyzer main

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

open Symfuzz_protocol
open Interpretation
open Context
open Misc
open Log_analysis

(* local socket *)
let local_ic = ref stdin
let local_oc = ref stdout

(* new logfile using analysis id *)
let get_logfile prefix lognum logdir =
  let logfile = Printf.sprintf "%s-%ld.log" prefix lognum in
  Filename.concat logdir logfile

let get_input_vector ic oc st =
  let () = send oc IVReq in
  match recv ic with
  | IVResponse vector -> State.new_vector st vector
  | _ -> raise Protocol.ProtocolError

let get_option logpath ic oc st =
  let () = send oc OptionReq in
  match recv ic with
  | OptionResponse opt ->
      init_log logpath;
      set_flag_from_option opt;
      Options.option_check opt;
      State.new_option st opt
  | _ -> raise Protocol.ProtocolError

let send_result _st =
  let oc = !local_oc in
  let ic = !local_ic in
  send oc (Result 0); (* XXX we currently ignore the result *)
  match recv ic with
  | Response -> ()
  | _ -> raise Protocol.ProtocolError

(* called once at start *)
let proc_start logdir id sockname debug_flag tid argvp envc envp =
  let () = Printexc.record_backtrace true in (* XXX *)
  let logpath = get_logfile "analysis" id logdir in
  let sock = Localsock.create_client sockname 2 in
  let ic, oc = client_prepare sock in
  let st =
       State.get_state ()
    |> State.set_logdir logdir
    |> State.set_debugging debug_flag
    |> get_input_vector ic oc
    |> get_option logpath ic oc
    |> State.introduce_arguments tid argvp 0
  in
  (* let st = State.introduce_environment st envc envp in *)
  let () = State.set_state st in
  local_ic := ic;
  local_oc := oc

(* callend once before finish *)
let proc_end bblcnt =
  let st = State.get_state () in
  let chan = get_chan () in
  State.dump_gamma st chan;
  State.dump_stats st chan bblcnt;
  send_result st;
  fin_log ()

(* basic block instrumentation *)
let bbl_instrument addr size bytes context tid =
  let block, blksize = (* list of (stmt list) *)
    try jit addr bytes size
    with Failure s -> failwith ("IL translation failure: "^s)
  in
  Interpretation.execute block blksize context addr tid

(* right after read, we introduce taint *)
let symbolic_read bufaddr byte_pos len totalsize path =
  if len > 0 then begin
    logtimef "read into buffer @ %nx (%d) [%nd / %d]"
      bufaddr len byte_pos totalsize;
    let st = State.get_state () in
    let st = State.introduce_files st bufaddr len totalsize byte_pos path in
    State.set_state st
  end else ()

let eflag_taint st =
     Dependency.is_tainted (State.get_vdelta st Disasm_i386.cf)
  || Dependency.is_tainted (State.get_vdelta st Disasm_i386.pf)
  || Dependency.is_tainted (State.get_vdelta st Disasm_i386.af)
  || Dependency.is_tainted (State.get_vdelta st Disasm_i386.zf)
  || Dependency.is_tainted (State.get_vdelta st Disasm_i386.sf)
  || Dependency.is_tainted (State.get_vdelta st Disasm_i386.df)
  || Dependency.is_tainted (State.get_vdelta st Disasm_i386.oF)

let check_reg_taint_32 st' =
  let open Disasm_i386 in function
  | 0 -> Dependency.is_tainted (State.get_vdelta st' R32.eax)
  | 1 -> Dependency.is_tainted (State.get_vdelta st' R32.ebx)
  | 2 -> Dependency.is_tainted (State.get_vdelta st' R32.ecx)
  | 3 -> Dependency.is_tainted (State.get_vdelta st' R32.edx)
  | 4 -> Dependency.is_tainted (State.get_vdelta st' R32.edi)
  | 5 -> Dependency.is_tainted (State.get_vdelta st' R32.esi)
  | 6 -> Dependency.is_tainted (State.get_vdelta st' R32.ebp)
  | 7 -> Dependency.is_tainted (State.get_vdelta st' R32.esp)
  | 8 -> Dependency.is_tainted (State.get_vdelta st' R32.eip)
  | 9 -> eflag_taint st'
  | 10 -> Dependency.is_tainted (State.get_vdelta st' R64.ymms.(0))
  | 11 -> Dependency.is_tainted (State.get_vdelta st' R64.ymms.(1))
  | 12 -> Dependency.is_tainted (State.get_vdelta st' R64.ymms.(2))
  | 13 -> Dependency.is_tainted (State.get_vdelta st' R64.ymms.(3))
  | 14 -> Dependency.is_tainted (State.get_vdelta st' R64.ymms.(4))
  | 15 -> Dependency.is_tainted (State.get_vdelta st' R64.ymms.(5))
  | 16 -> Dependency.is_tainted (State.get_vdelta st' R64.ymms.(6))
  | 17 -> Dependency.is_tainted (State.get_vdelta st' R64.ymms.(7))
  | _ -> false

let check_reg_taint_64 st' =
  let open Disasm_i386 in function (* FIXME : add more regs *)
  | 0 -> Dependency.is_tainted (State.get_vdelta st' R64.rax)
  | 1 -> Dependency.is_tainted (State.get_vdelta st' R64.rbx)
  | 2 -> Dependency.is_tainted (State.get_vdelta st' R64.rcx)
  | 3 -> Dependency.is_tainted (State.get_vdelta st' R64.rdx)
  | 4 -> Dependency.is_tainted (State.get_vdelta st' R64.rdi)
  | 5 -> Dependency.is_tainted (State.get_vdelta st' R64.rsi)
  | 6 -> Dependency.is_tainted (State.get_vdelta st' R64.rbp)
  | 7 -> Dependency.is_tainted (State.get_vdelta st' R64.rsp)
  | 8 -> Dependency.is_tainted (State.get_vdelta st' R64.rip)
  | 9 -> eflag_taint st'
  | 10 -> Dependency.is_tainted (State.get_vdelta st' R64.ymms.(0))
  | 11 -> Dependency.is_tainted (State.get_vdelta st' R64.ymms.(1))
  | 12 -> Dependency.is_tainted (State.get_vdelta st' R64.ymms.(2))
  | 13 -> Dependency.is_tainted (State.get_vdelta st' R64.ymms.(3))
  | 14 -> Dependency.is_tainted (State.get_vdelta st' R64.ymms.(4))
  | 15 -> Dependency.is_tainted (State.get_vdelta st' R64.ymms.(5))
  | 16 -> Dependency.is_tainted (State.get_vdelta st' R64.ymms.(6))
  | 17 -> Dependency.is_tainted (State.get_vdelta st' R64.ymms.(7))
  | _ -> false

let check =
  if Sys.word_size = 32 then check_reg_taint_32
  else check_reg_taint_64

let check_reg_taint used_regs idx st =
  if Int64.logand Int64.one used_regs <= Int64.zero then false
  else check st idx

let check_regs_taint used_regs =
  let st = State.get_state () in
  let rec check_loop idx used_regs =
    if idx > 17 then false
    else if check_reg_taint used_regs idx st then true
    else check_loop (idx+1) (Int64.shift_right used_regs 1)
  in
  check_loop 0 used_regs

let check_mems_taint addrs =
  let st = State.get_state () in
  List.exists (fun ((addr: nativeint), (size: int)) ->
    Dependency.is_tainted (State.get_abstr_mu st addr (Type.Reg size))
  ) addrs

let check_postdom () =
  let st = State.get_state () in
  let idstack = State.get_idstack st in
  if Stack.is_empty idstack then 0n
  else let nextaddr, _ = Stack.top idstack in nextaddr

let push_to_stack retaddr stack =
  let _, dep = Stack.top stack in
  Stack.push (retaddr, dep) stack

let handle_call _tid retaddr =
  let st = State.get_state () in
  let stack = State.get_idstack st in
  if Stack.is_empty stack then Stack.push (retaddr, Dependency.dep_empty) stack
  else push_to_stack retaddr stack

let is_updatable abstr st =
  let inputsize = State.get_input_size st in
  let depsize = Dependency.dep_cardinal abstr in
  (* ignore all-byte correlation, e.g., strlen before parsing *)
  let threshold = 1.0 in
  (float_of_int depsize) /. (float_of_int inputsize) < threshold

let handle_ret _tid retaddr =
  let st = State.get_state () in
  let idstack = State.get_idstack st in
  let rec pop_loop st =
    let addr, abstr = Stack.pop idstack in
    let st =
      if is_updatable abstr st then
        State.update_gamma_from_stashed st
      else
        State.clear_stash st
    in
    if (addr = retaddr) then st
    else if Stack.is_empty idstack then st
    else pop_loop st
  in
  let st = if Stack.is_empty idstack then st else pop_loop st in
  State.set_state st

(* unlike other instructions, we always instrument call and ret instructions
   even though the corresponding bbl is not tainted. *)
let handle_callret retaddr tid ty =
  if ty = 0 (* call *) then handle_call tid retaddr
  else handle_ret tid retaddr

(* callback registration *)
let () = Callback.register "proc_start" proc_start
let () = Callback.register "proc_end" proc_end
let () = Callback.register "bbl_instrument" bbl_instrument
let () = Callback.register "symbolic_read" symbolic_read
let () = Callback.register "check_regs_taint" check_regs_taint
let () = Callback.register "check_mems_taint" check_mems_taint
let () = Callback.register "check_postdom" check_postdom
let () = Callback.register "handle_callret" handle_callret

