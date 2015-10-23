/// SymFuzz
/// @brief: program context for the instrumentor
/// @file: context.cpp
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

#include "context.hpp"
#include "logger.hpp"
#include "analysis_api.hpp"

#include <stdint.h>

void store_xmm_value( UINT64* low, UINT64* high, REG reg, const CONTEXT *ctxt )
{
    UINT64 val[4];
    PIN_GetContextRegval( ctxt, reg, reinterpret_cast<UINT8*>(&val) );
    *low = val[0];
    *high = val[1];
}

void get_reg_context( reg_context* dst_ctxt,
                      const CONTEXT *ctxt,
                      const ADDRINT eip )
{
    long eflags;

    dst_ctxt->eax = PIN_GetContextReg( ctxt, REG_EAX );
    dst_ctxt->ebx = PIN_GetContextReg( ctxt, REG_EBX );
    dst_ctxt->ecx = PIN_GetContextReg( ctxt, REG_ECX );
    dst_ctxt->edx = PIN_GetContextReg( ctxt, REG_EDX );
    dst_ctxt->edi = PIN_GetContextReg( ctxt, REG_EDI );
    dst_ctxt->esi = PIN_GetContextReg( ctxt, REG_ESI );
    dst_ctxt->ebp = PIN_GetContextReg( ctxt, REG_EBP );
    dst_ctxt->esp = PIN_GetContextReg( ctxt, REG_ESP );
    dst_ctxt->eip = eip;

    eflags = dst_ctxt->eflags = PIN_GetContextReg( ctxt, REG_EFLAGS );

    dst_ctxt->ldtr = PIN_GetContextReg( ctxt, REG_LDTR );
    dst_ctxt->gdtr = 0;
    dst_ctxt->cs = PIN_GetContextReg( ctxt, REG_SEG_CS );
    dst_ctxt->ds = PIN_GetContextReg( ctxt, REG_SEG_DS );
    dst_ctxt->es = PIN_GetContextReg( ctxt, REG_SEG_ES );
    dst_ctxt->fs = PIN_GetContextReg( ctxt, REG_SEG_FS );
    dst_ctxt->gs = PIN_GetContextReg( ctxt, REG_SEG_GS );
    dst_ctxt->ss = PIN_GetContextReg( ctxt, REG_SEG_SS );
    dst_ctxt->fs_base = PIN_GetContextReg( ctxt, REG_SEG_FS_BASE );
    dst_ctxt->gs_base = PIN_GetContextReg( ctxt, REG_SEG_GS_BASE );

    dst_ctxt->cf = eflags & 0x1;
    dst_ctxt->pf = eflags & 0x4;
    dst_ctxt->af = eflags & 0x10;
    dst_ctxt->zf = eflags & 0x40;
    dst_ctxt->sf = eflags & 0x80;
    dst_ctxt->df = eflags & 0x400;
    dst_ctxt->of = eflags & 0x800;
    dst_ctxt->acf = eflags & 0x40000;
    dst_ctxt->idf = eflags & 0x200000;

    store_xmm_value( &dst_ctxt->xmm0.low,
                     &dst_ctxt->xmm0.high, REG_XMM0, ctxt );
    store_xmm_value( &dst_ctxt->xmm1.low,
                     &dst_ctxt->xmm1.high, REG_XMM1, ctxt );
    store_xmm_value( &dst_ctxt->xmm2.low,
                     &dst_ctxt->xmm2.high, REG_XMM2, ctxt );
    store_xmm_value( &dst_ctxt->xmm3.low,
                     &dst_ctxt->xmm3.high, REG_XMM3, ctxt );
    store_xmm_value( &dst_ctxt->xmm4.low,
                     &dst_ctxt->xmm4.high, REG_XMM4, ctxt );
    store_xmm_value( &dst_ctxt->xmm5.low,
                     &dst_ctxt->xmm5.high, REG_XMM5, ctxt );
    store_xmm_value( &dst_ctxt->xmm6.low,
                     &dst_ctxt->xmm6.high, REG_XMM6, ctxt );
    store_xmm_value( &dst_ctxt->xmm7.low,
                     &dst_ctxt->xmm7.high, REG_XMM7, ctxt );

    // out << std::hex
    //     << "BBL: " << eip << "\n---CTXT---\n"
    //     << "EAX:" << dst_ctxt.eax << "; "
    //     << "EBX:" << dst_ctxt.ebx << "; "
    //     << "ECX:" << dst_ctxt.ecx << "; "
    //     << "EDX:" << dst_ctxt.edx << ";\n"
    //     << "EDI:" << dst_ctxt.edi << "; "
    //     << "ESI:" << dst_ctxt.esi << "; "
    //     << "EBP:" << dst_ctxt.ebp << "; "
    //     << "ESP:" << dst_ctxt.esp << ";\n"
    //     << "XMM0:" << dst_ctxt.xmm0.low << ";" << dst_ctxt.xmm0.high << endl << endl;

    return;
}

regset_t get_regset( const REG r )
{
    if ( r == REG_INVALID() ) return regset_empty;

    switch ( r )
    {
#if __x86_64__
    case REG_RAX:
#endif
    case REG_EAX:
    case REG_AX:
    case REG_AL:
    case REG_AH:
        return regset_eax;
#if __x86_64__
    case REG_RBX:
#endif
    case REG_EBX:
    case REG_BX:
    case REG_BL:
    case REG_BH:
        return regset_ebx;
#if __x86_64__
    case REG_RCX:
#endif
    case REG_ECX:
    case REG_CX:
    case REG_CL:
    case REG_CH:
        return regset_ecx;
#if __x86_64__
    case REG_RDX:
#endif
    case REG_EDX:
    case REG_DX:
    case REG_DL:
    case REG_DH:
        return regset_edx;
#if __x86_64__
    case REG_RSI:
#endif
    case REG_ESI:
    case REG_SI:
        return regset_esi;
#if __x86_64__
    case REG_RDI:
#endif
    case REG_EDI:
    case REG_DI:
        return regset_edi;
    case REG_ESP:
    case REG_SP:
        return regset_esp;
    case REG_EBP:
    case REG_BP:
        return regset_ebp;
    case REG_EIP:
        return regset_eip;
    case REG_MM0:
    case REG_XMM0:
        return regset_xmm0;
    case REG_MM1:
    case REG_XMM1:
        return regset_xmm1;
    case REG_MM2:
    case REG_XMM2:
        return regset_xmm2;
    case REG_MM3:
    case REG_XMM3:
        return regset_xmm3;
    case REG_MM4:
    case REG_XMM4:
        return regset_xmm4;
    case REG_MM5:
    case REG_XMM5:
        return regset_xmm5;
    case REG_MM6:
    case REG_XMM6:
        return regset_xmm6;
    case REG_MM7:
    case REG_XMM7:
        return regset_xmm7;
    case REG_ST0:
        return regset_st0;
    case REG_ST1:
        return regset_st1;
    case REG_ST2:
        return regset_st2;
    case REG_ST3:
        return regset_st3;
    case REG_ST4:
        return regset_st4;
    case REG_ST5:
        return regset_st5;
    case REG_ST6:
        return regset_st6;
    case REG_ST7:
        return regset_st7;
    case REG_EFLAGS:
        return regset_eflags;
    case REG_SEG_CS:
    case REG_SEG_DS:
    case REG_SEG_ES:
    case REG_SEG_FS:
    case REG_SEG_GS:
    case REG_SEG_SS:
        return regset_empty;
    case REG_X87:
        // FIXME
        return regset_empty;
    default:
        out << "unknown register: " << REG_StringShort( r ).c_str() << endl;
        return regset_empty;
    }
}

bool any_reg_tainted( regset_t used_regs )
{
    return check_regs_taint( (UINT64) used_regs );;
}

