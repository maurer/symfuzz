(* SymFuzz *)

(** program context

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

type raw_context_t

type xmm_t =
  {
    low        : int64;
    high       : int64;
  }

type t =
  {
    eax        : address_t;
    ebx        : address_t;
    ecx        : address_t;
    edx        : address_t;
    edi        : address_t;
    esi        : address_t;
    ebp        : address_t;
    esp        : address_t;
    eip        : address_t;
    eflags     : address_t;
    ldtr       : address_t;
    gdtr       : address_t;
    cs         : address_t;
    ds         : address_t;
    es         : address_t;
    fs         : address_t;
    gs         : address_t;
    ss         : address_t;
    fs_base    : address_t;
    gs_base    : address_t;
    cflag      : bool;
    pflag      : bool;
    aflag      : bool;
    zflag      : bool;
    sflag      : bool;
    dflag      : bool;
    oflag      : bool;
    acflag     : bool;
    idflag     : bool;
    xmm0       : xmm_t;
    xmm1       : xmm_t;
    xmm2       : xmm_t;
    xmm3       : xmm_t;
    xmm4       : xmm_t;
    xmm5       : xmm_t;
    xmm6       : xmm_t;
    xmm7       : xmm_t;
  }

val get_context : raw_context_t -> t

