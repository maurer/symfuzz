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
open Misc

type t

val get_state : unit -> t
val set_state : t -> unit
val bil_handle : LibBil.t

val new_vector: t -> Libinput_type.input_vector -> t
val new_option : t -> Options.t -> t

val update_vdelta : t -> Var.t -> AbstrVal.t -> t
val update_context : Context.t -> t
val update_bblcount : t -> int -> int -> t
val update_taintbbl : bool -> t -> t
val cleanup_context : t -> t

val introduce_arguments : int -> address_t -> int -> t -> t
val introduce_files : t -> address_t -> int -> int -> nativeint -> string -> t
val introduce_environment : t -> int32 -> address_t -> t

val get_var_value : t -> Var.t -> Ast.exp * AbstrVal.t

val set_pc : t -> address_t -> t
val get_pc : t -> address_t
val set_conc_mu : t -> address_t -> Ast.exp -> t
val get_conc_mu : t -> address_t -> Type.typ -> Ast.exp
val set_abstr_mu : t -> address_t -> AbstrVal.t -> int -> t
val get_abstr_mu : t -> address_t -> Type.typ -> AbstrVal.t
val set_delta : t -> Var.t -> Ast.exp -> t
val get_delta : t -> Var.t -> Ast.exp
val set_vdelta : t -> Var.t -> AbstrVal.t -> t
val get_vdelta : t -> Var.t -> AbstrVal.t
val update_gamma : t -> inputdep -> inputdep -> t
val stash_gamma : t -> inputdep -> inputdep -> t
val update_gamma_from_stashed : t -> t
val clear_stash : t -> t
val set_last_lbl : t -> Ast.stmt -> t
val get_last_lbl : t -> string
val is_debugging : t -> bool
val set_debugging : bool -> t -> t
val set_logdir : string -> t -> t
val get_input_size : t -> int
val get_idstack : t -> (nativeint * inputdep) Stack.t

val dump_vars : t -> out_channel -> unit
val dump_abstract_vars : t -> out_channel -> unit
val dump_mem  : t -> out_channel -> unit
val dump_abstract_mem : t -> out_channel -> unit
val dump_gamma : t -> out_channel -> unit
val dump_stats : t -> out_channel -> int32 -> unit
val dump_mem_info : t -> out_channel -> unit

val get_nbytes_from_typ : Type.typ -> int

val label_address : Type.label -> nativeint option

val esp : Var.t

