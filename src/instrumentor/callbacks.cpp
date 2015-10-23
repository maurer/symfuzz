/// SymFuzz
/// @brief: exposed callbacks for the analyzer
/// @file: callbacks.cpp
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

#include "callbacks.hpp"
#include "logger.hpp"
#include "misc.hpp"
#include "memorylog.hpp"

#include "pin.H"

#ifdef __cplusplus
extern "C" {
#endif
#include "pinapi.h"
#ifdef __cplusplus
} /* end of extern */
#endif

#include <cstdlib>

// prototypes
byte_val cb_get_mem_byte( unsigned int tid, unsigned long addr );
char* cb_get_mem( unsigned int tid, unsigned long addr, unsigned int size );
char* cb_get_mem_string( unsigned int tid, unsigned long addr );

void init_callbacks()
{
    get_mem_byte = cb_get_mem_byte;
    get_mem = cb_get_mem;
    get_mem_string = cb_get_mem_string;
}

//
// callback implementations
//

byte_val cb_get_mem_byte( unsigned int tid, unsigned long addr )
{
    byte_val v = {0, 0};
    VOID* ptr;
    size_t ret;

    ptr = get_log( tid, addr );
    if ( ptr ) {
        ret = PIN_SafeCopy( &v.value, (VOID*) ptr, 1 );
    } else {
        ret = PIN_SafeCopy( &v.value, (VOID*) addr, 1 );
    }
    if ( ret == 1 ) v.valid = 1;
    else v.valid = 0;

    return v;
}

char* cb_get_mem( unsigned int tid, unsigned long addr, unsigned int size )
{
    char* buf = (char*) malloc( size );
    if ( !buf ) error_exit( "not enough memory from cb_get_mem" );

    VOID* ptr = get_log( tid, addr );
    if ( !ptr ) ptr = (VOID*) addr;

    if ( PIN_SafeCopy( buf, ptr, size ) == size ) return buf;
    else return NULL;
}

bool zero_exists( const char* buf, unsigned long len, unsigned long* pos )
{
    register const char* s;
    register unsigned i = 0;

    for ( s = buf; *s; ++s, ++i ) {
        if ( i >= len ) break;
    }

    *pos = i;
    return (len != i);
}

char* cb_get_mem_string( unsigned int tid, unsigned long addr )
{
    const unsigned long interval = 4; // fetch interval
    unsigned long size = interval;
    unsigned long ptr = 0;
    unsigned long pos = 0;
    char* buf = (char*) malloc( size );
    if ( !buf ) error_exit( "not enough memory from cb_get_mem" );

    do {
        VOID* srcaddr = get_log( tid, addr + ptr );
        if ( !srcaddr ) srcaddr = (VOID*) (addr + ptr);

        if ( PIN_SafeCopy( buf + ptr, srcaddr, interval ) == interval ) {
            if ( zero_exists( buf + ptr, interval, &pos ) ) {
                *(buf + ptr + pos) = 0;
                break;
            } else {
                ptr += interval;
                size += interval;
                buf = (char*) realloc( buf, size );
            }
        } else {
            error_exit( "invalid memory access from get_mem_string" );
        }
    } while ( true );

    return buf;
}

