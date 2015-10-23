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
open Log_analysis

type raw_context_t =
  address_t * address_t * address_t * address_t * (* eax, ebx, ecx, edx *)
  address_t * address_t * address_t * address_t * (* edi, esi, esp, ebp *)
  address_t * address_t * address_t * address_t * (* eip, eflags, ldtr, gdtr *)
  address_t * address_t * address_t * address_t * (* cs, ds, es, fs *)
  address_t * address_t * address_t * address_t * (* gs, ss, fs_base, gs_base *)
  bool * bool * bool * bool * (* cf, pf, af, zf *)
  bool * bool * bool * bool * (* sf, df, of, acf *)
  bool * (* idf *)
  int64 * int64 * (* xmm0 *)
  int64 * int64 * (* xmm1 *)
  int64 * int64 * (* xmm2 *)
  int64 * int64 * (* xmm3 *)
  int64 * int64 * (* xmm4 *)
  int64 * int64 * (* xmm5 *)
  int64 * int64 * (* xmm6 *)
  int64 * int64   (* xmm7 *)

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

let get_context = function
  | eax,ebx,ecx,edx,edi,esi,ebp,esp,eip,eflags,ldtr,gdtr,
    cs,ds,es,fs,gs,ss,fs_base,gs_base,
    cflag,pflag,aflag,zflag,sflag,dflag,oflag,acflag,idflag,
    xmm0_l,xmm0_h,xmm1_l,xmm1_h,xmm2_l,xmm2_h,xmm3_l,xmm3_h,
    xmm4_l,xmm4_h,xmm5_l,xmm5_h,xmm6_l,xmm6_h,xmm7_l,xmm7_h ->
      (* let () =
        logf "---CTXT---\nEAX:%nx; EBX:%nx; ECX:%nx; EDX:%nx\nEDI:%nx; ESI:%nx; EBP:%nx; ESP:%nx\nXMM0:%Lx;%Lx\n"
        eax ebx ecx edx edi esi ebp esp xmm0_l xmm0_h
      in *)
      {
        eax=eax; ebx=ebx; ecx=ecx; edx=edx;
        edi=edi; esi=esi; ebp=ebp; esp=esp;
        eip=eip; eflags=eflags; ldtr=ldtr; gdtr=gdtr;
        cs=cs; ds=ds; es=es; fs=fs; gs=gs; ss=ss;
        fs_base=fs_base; gs_base=gs_base;
        cflag=cflag; pflag=pflag; aflag=aflag; zflag=zflag;
        sflag=sflag; dflag=dflag; oflag=oflag; acflag=acflag;
        idflag=idflag;
        xmm0={low=xmm0_l; high=xmm0_h}; xmm1={low=xmm1_l; high=xmm1_h};
        xmm2={low=xmm2_l; high=xmm2_h}; xmm3={low=xmm3_l; high=xmm3_h};
        xmm4={low=xmm4_l; high=xmm4_h}; xmm5={low=xmm5_l; high=xmm5_h};
        xmm6={low=xmm6_l; high=xmm6_h}; xmm7={low=xmm7_l; high=xmm7_h};
      }

