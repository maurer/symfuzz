/// SymFuzz
/// @brief: log memory accesses
/// @file: memorylog.cpp
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

#include "memorylog.hpp"
#include "tls.hpp"
#include <cassert>
#include <cstdio>

void push_log( memorylog_entry* memlogs,
               UINT32* pLogcnt,
               ADDRINT addr,
               UINT32 size,
               bool storePrev )
{
    memlogs[*pLogcnt].addr = addr;

    if ( storePrev )
        assert( PIN_SafeCopy( &memlogs[*pLogcnt].val[0],
                              (const void*) addr,
                              size ) == size );
    else {}

    memlogs[*pLogcnt].size = (UINT16) size;

    *pLogcnt += 1;
    assert( *pLogcnt < MAX_ENTRY );
}

void init_memlogs( memorylog_entry** ptr )
{
    *ptr = new memorylog_entry [MAX_ENTRY];
    assert( *ptr != NULL );
}

void* get_log( THREADID tid, ADDRINT addr )
{
    tls_info* tdata =
        static_cast<tls_info*>( PIN_GetThreadData( g_tls_key, tid ) );

    for ( UINT32 i = 0; i < tdata->wmemlog_cnt; i ++ ) {
        if ( tdata->wmemlogs[i].addr == addr )
            return tdata->wmemlogs[i].val;
        else if ( tdata->wmemlogs[i].addr < addr
               && addr < (tdata->wmemlogs[i].addr + tdata->wmemlogs[i].size) )
            return tdata->wmemlogs[i].val;
    }

    return NULL;
}

