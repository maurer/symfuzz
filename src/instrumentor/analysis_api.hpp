/// SymFuzz
/// @brief: calling OCaml functions
/// @file: analysis_api.hpp
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

#include <stdint.h>
#include "bbl_cache.hpp"
#include "context.hpp"
#include "tls.hpp"

#ifdef __cplusplus
extern "C" {
#endif

void init_ocaml( char** argv );

///
void proc_start( const char* logdir,
                 const uint32_t analysis_id,
                 const char* sockname,
                 bool debug_flag,
                 const THREADID tid,
                 char** argvp,
                 int envc,
                 char** envp );

void proc_end( unsigned int bblcnt );

int  bbl_instrument( unsigned long addr,
                     const bbl_content* content,
                     const reg_context* context,
                     const THREADID tid );

void symbolic_read( ADDRINT addr,
                    ADDRINT pos,
                    ADDRINT ret,
                    ADDRINT totalsize,
                    const char* fname );

void handle_ret( const ADDRINT retaddr, const THREADID tid );
void handle_call( const ADDRINT retaddr, const THREADID tid );

bool check_regs_taint( UINT64 regs );
bool check_mems_taint( memorylog_entry* memlog, unsigned int cnt );

ADDRINT check_postdom();

#ifdef __cplusplus
} /* end of extern */
#endif

