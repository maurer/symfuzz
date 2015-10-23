/// SymFuzz
/// @brief: tls
/// @file: tls.hpp
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
#include "memorylog.hpp"
#include "context.hpp"

typedef struct tls_info {
    int              syscall_num;
    long             arg0;
    long             arg1;
    long             arg2;
    long             arg3;
    long             arg4;
    memorylog_entry* wmemlogs;
    UINT32           wmemlog_cnt;
    memorylog_entry* rmemlogs;
    UINT32           rmemlog_cnt;
    reg_context*     bbl_ctxt;
    ADDRINT          next_pdom_to_hit;
    UINT32           pdom_hit;
} tls_info;

extern TLS_KEY g_tls_key;

