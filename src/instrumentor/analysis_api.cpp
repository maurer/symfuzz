/// SymFuzz
/// @brief: calling OCaml functions
/// @file: analysis_api.cpp
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

#include "analysis_api.hpp"
#include "misc.hpp"

#include <stdio.h>
#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif
#include <caml/callback.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#ifdef __cplusplus
} /* end of extern */
#endif

void init_ocaml( char** argv )
{
    caml_startup( argv );
}

void proc_start( const char* logdir,
                 const uint32_t analysis_id,
                 const char* sockname,
                 bool debug_flag,
                 const THREADID tid,
                 char** argvp,
                 int envc,
                 char** envp )
{
    CAMLparam0();
    CAMLlocalN( caml_args, 8 );
    static value *proc_start_closure = NULL;

    if ( !proc_start_closure ) {
        proc_start_closure = caml_named_value( "proc_start" );
    }

    caml_args[0] = caml_copy_string( logdir );
    caml_args[1] = caml_copy_int32( analysis_id );
    caml_args[2] = caml_copy_string( sockname );
    caml_args[3] = Val_bool( debug_flag );
    caml_args[4] = Val_int( tid );
    caml_args[5] = caml_copy_nativeint( (long) argvp );
    caml_args[6] = caml_copy_int32( envc );
    caml_args[7] = caml_copy_nativeint( (long) envp );

    caml_callbackN( *proc_start_closure, 8, caml_args );

    CAMLreturn0;
}

void proc_end( unsigned int bbl_cnt )
{
    CAMLparam0();

    value *proc_end_closure = caml_named_value( "proc_end" );
    caml_callback( *proc_end_closure, caml_copy_int32( bbl_cnt ) );

    CAMLreturn0;
}

int bbl_instrument( unsigned long addr,
                    const bbl_content* content,
                    const reg_context* context,
                    const THREADID tid )
{
    CAMLparam0();
    CAMLlocal1( ret );
    CAMLlocalN( caml_args, 5 );
    unsigned i, j;
    uint32_t size = (uint32_t) content->size;
    static value *bbl_instrument_closure = NULL;

    if ( !bbl_instrument_closure ) {
        bbl_instrument_closure = caml_named_value( "bbl_instrument" );
    }

    caml_args[0] = caml_copy_nativeint( addr );
    caml_args[1] = caml_copy_int32( size );

    caml_args[2] = caml_alloc_string( size );
    memcpy( (unsigned char*)String_val(caml_args[2]), content->content, size );

    caml_args[3] = caml_alloc_tuple( 45 );
    for ( i = 0; i < 20; ++i ) {
        Store_field( caml_args[3], i, caml_copy_nativeint( ((long*) &context->eax)[i] ) );
    }
    for ( i = 20; i < 29; ++i ) {
        Store_field( caml_args[3], i, Val_bool( ((long*) &context->eax)[i] ) );
    }
    for ( i = 29, j = 0; i < 45; ++i ) {
        Store_field( caml_args[3], i, caml_copy_int64( ((uint64_t*) &context->xmm0)[j++] ) );
        Store_field( caml_args[3], i, caml_copy_int64( ((uint64_t*) &context->xmm0)[j++] ) );
    }

    caml_args[4] = Val_int( tid );

    ret = caml_callbackN( *bbl_instrument_closure, 5, caml_args );

    CAMLreturnT( int, Int_val(ret) );
}

void symbolic_read( ADDRINT addr,
                    ADDRINT pos,
                    ADDRINT ret,
                    ADDRINT totalsize,
                    const char* fname )
{
    CAMLparam0();
    CAMLlocalN( caml_args, 5 );
    static value *proc_symbolic_read = NULL;

    if ( !proc_symbolic_read ) {
        proc_symbolic_read = caml_named_value( "symbolic_read" );
    }

    caml_args[0] = caml_copy_nativeint( (long) addr );
    caml_args[1] = caml_copy_nativeint( (long) pos );
    caml_args[2] = Val_int( (int) ret );
    caml_args[3] = Val_int( (int) totalsize );
    caml_args[4] = caml_copy_string( fname );

    caml_callbackN( *proc_symbolic_read, 5, caml_args );

    CAMLreturn0;
}

void handle_callret( const ADDRINT addr, const THREADID tid, UINT32 type )
{
    CAMLparam0();
    CAMLlocal3( caml_addr, caml_tid, caml_type );
    static value *proc_handle_callret = NULL;

    if ( !proc_handle_callret ) {
        proc_handle_callret = caml_named_value( "handle_callret" );
    }

    caml_addr = caml_copy_nativeint( (long) addr );
    caml_tid = Val_int( (int) tid );
    caml_type = Val_int( (int) type );

    caml_callback3( *proc_handle_callret, caml_addr, caml_tid, caml_type );

    CAMLreturn0;
}

void handle_call( const ADDRINT addr, const THREADID tid )
{
    handle_callret( addr, tid, 0 );
}

void handle_ret( const ADDRINT addr, const THREADID tid )
{
    handle_callret( addr, tid, 1 );
}

bool check_regs_taint( UINT64 regs )
{
    CAMLparam0();
    CAMLlocal2( used_regs, ret );
    static value *proc_check_regs_taint = NULL;

    if ( !proc_check_regs_taint ) {
        proc_check_regs_taint = caml_named_value( "check_regs_taint" );
    }

    used_regs = caml_copy_int64( regs );

    ret = caml_callback( *proc_check_regs_taint, used_regs );

    CAMLreturnT( bool, Bool_val( ret ) );
}

bool check_mems_taint( memorylog_entry* memlog, unsigned int cnt )
{
    CAMLparam0();
    CAMLlocal4( addrs, ret, v, tupl );
    static value *proc_check_mems_taint = NULL;

    if ( !proc_check_mems_taint ) {
        proc_check_mems_taint = caml_named_value( "check_mems_taint" );
    }

    addrs = Val_emptylist;
    for ( unsigned int i = 0; i < cnt; i ++  ) {
        tupl = caml_alloc_tuple( 2 );
        Store_field( tupl, 0, caml_copy_nativeint( memlog[i].addr ) );
        Store_field( tupl, 1, Val_int( memlog[i].size * 8 ) );
        v = caml_alloc_small( 2, 0 );
        Field( v, 0 ) = tupl;
        Field( v, 1 ) = addrs;
        addrs = v;
    }

    ret = caml_callback( *proc_check_mems_taint, addrs );

    CAMLreturnT( bool, Bool_val( ret ) );
}

ADDRINT check_postdom()
{
    CAMLparam0();
    CAMLlocal1( ret );
    static value *proc_check_postdom = NULL;

    if ( !proc_check_postdom ) {
        proc_check_postdom = caml_named_value( "check_postdom" );
    }

    ret = caml_callback( *proc_check_postdom, Val_unit );

    CAMLreturnT( ADDRINT, Nativeint_val( ret ) );
}

