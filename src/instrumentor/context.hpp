/// SymFuzz
/// @brief: program context for the instrumentor
/// @file: context.hpp
/// @author: Sang Kil Cha <sangkil.cha@gmail.com>
/// @date: 2014/03/19

/*
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
 */

#pragma once

#include "pin.H"

typedef struct xmm_t {

    uint64_t low;
    uint64_t high;

} xmm_t;

typedef struct reg_context {

    long eax;
    long ebx;
    long ecx;
    long edx;
    long edi;
    long esi;
    long ebp;
    long esp;
    long eip;

    long eflags;

    long ldtr;
    long gdtr;
    long cs;
    long ds;
    long es;
    long fs;
    long gs;
    long ss;
    long fs_base;
    long gs_base;

    /* eflags */
    long cf;
    long pf;
    long af;
    long zf;
    long sf;
    long df;
    long of;
    long acf;
    long idf;

    xmm_t xmm0;
    xmm_t xmm1;
    xmm_t xmm2;
    xmm_t xmm3;
    xmm_t xmm4;
    xmm_t xmm5;
    xmm_t xmm6;
    xmm_t xmm7;

    /* TODO: FP control register? */

} reg_context;

void get_reg_context( reg_context* dst_ctxt, const CONTEXT *ctxt, const ADDRINT eip );

typedef UINT64 regset_t;

#define regset_empty    (0LL)

#define regset_eax      (1LL << 0)
#define regset_ebx      (1LL << 1)
#define regset_ecx      (1LL << 2)
#define regset_edx      (1LL << 3)
#define regset_edi      (1LL << 4)
#define regset_esi      (1LL << 5)
#define regset_ebp      (1LL << 6)
#define regset_esp      (1LL << 7)
#define regset_eip      (1LL << 8)
#define regset_eflags   (1LL << 9)
#define regset_xmm0     (1LL << 10)
#define regset_xmm1     (1LL << 11)
#define regset_xmm2     (1LL << 12)
#define regset_xmm3     (1LL << 13)
#define regset_xmm4     (1LL << 14)
#define regset_xmm5     (1LL << 15)
#define regset_xmm6     (1LL << 16)
#define regset_xmm7     (1LL << 17)
#define regset_xmm8     (1LL << 18)
#define regset_xmm9     (1LL << 19)
#define regset_xmm10    (1LL << 20)
#define regset_xmm11    (1LL << 21)
#define regset_xmm12    (1LL << 22)
#define regset_xmm13    (1LL << 23)
#define regset_xmm14    (1LL << 24)
#define regset_xmm15    (1LL << 25)
#define regset_x87      (1LL << 26)
#define regset_st0      (1LL << 27)
#define regset_st1      (1LL << 28)
#define regset_st2      (1LL << 29)
#define regset_st3      (1LL << 30)
#define regset_st4      (1LL << 31)
#define regset_st5      (1LL << 32)
#define regset_st6      (1LL << 33)
#define regset_st7      (1LL << 34)

regset_t get_regset( const REG r );

bool any_reg_tainted( regset_t used_regs );

