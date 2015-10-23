/// SymFuzz
/// @brief: instrumentor main
/// @file: main.cpp
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

#include "pin.H"
#include <cstring>
#include <set>
#include <map>

#include "logger.hpp"
#include "misc.hpp"
#include "callbacks.hpp"
#include "analysis_api.hpp"
#include "bbl_cache.hpp"
#include "context.hpp"
#include "memorylog.hpp"
#include "tls.hpp"

extern "C" {
#include "xed-interface.h"
}

#include <syscall.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>

class fd_type
{
public:
    string         filename;
    unsigned long  position;
    unsigned long  totalsize;

    fd_type() {}
    fd_type( const char* path, unsigned long pos ):
        filename(path), position(pos)
    {
        struct stat st;
        int fd = open( path, O_RDONLY);
        if ( fd < 0 ) error_exit( "failed to construct fd_type" );
        fstat( fd, &st );
        close( fd );
        totalsize = (unsigned long) st.st_size;
    }
    fd_type( const fd_type& rhs ):
        filename(rhs.filename),
        position(rhs.position),
        totalsize(rhs.totalsize)
    {}
    ~fd_type() {}
};

int *g_argcp = NULL;
char **g_argvp = NULL;
char **g_envp = NULL;
const char* g_progname = NULL;
set<string> g_filenames;
map<int, fd_type> g_symbolic_fds;

// static PIN_LOCK g_lock;
static UINT32 g_bblcnt = 0;
#define BLK_PRINT_INTERVAL   10000
#define BLK_NEWLINE_INTERVAL 600000

/* ===================================================================== */
// Command line switches
/* ===================================================================== */
KNOB<string> KnobOutputFile( KNOB_MODE_WRITEONCE,  "pintool",
    "o", "instrument.log", "specify file name for symfuzz output");

KNOB<string> KnobOutputDirectory( KNOB_MODE_WRITEONCE, "pintool",
    "d", "", "specify log directory name for symfuzz output" );

KNOB<UINT32> KnobAnalysisId( KNOB_MODE_WRITEONCE, "pintool",
    "id", "", "specify analysis id" );

KNOB<string> KnobSockName( KNOB_MODE_WRITEONCE, "pintool",
    "sock", "", "specify local socket name" );

KNOB<BOOL> KnobDebug( KNOB_MODE_WRITEONCE, "pintool",
    "debug", "0", "specify local socket name" );

KNOB<string> KnobFileNames( KNOB_MODE_APPEND, "pintool",
    "filename", "", "specify target file names" );

KNOB<string> KnobBinName( KNOB_MODE_WRITEONCE, "pintool",
    "binname", "", "specify real binary name (if a symlink and a real file name are different)" );

INT32 Usage()
{
    cerr << "This tool prints out the number of dynamically executed " << endl
         << "instructions, basic blocks and threads in the application." << endl
         << endl;

    cerr << KNOB_BASE::StringKnobSummary() << endl;

    return -1;
}

// #define DEBUG
#undef DEBUG

#ifdef DEBUG
static bool stop_flag = false;

void stop_and_debug()
{
    stop_flag = true;
}
#else
void stop_and_debug()
{
    error_exit( "analysis failed" );
}
#endif

/* ===================================================================== */
// Analysis Routines
/* ===================================================================== */

VOID PIN_FAST_ANALYSIS_CALL BblHead( THREADID tid,
                                     ADDRINT addr,
                                     const CONTEXT* ctxt )
{
    tls_info* tdata =
        static_cast<tls_info*>( PIN_GetThreadData( g_tls_key, tid ) );

    tdata->wmemlog_cnt = 0;
    tdata->rmemlog_cnt = 0;
    get_reg_context( tdata->bbl_ctxt, ctxt, addr );
}

static inline bool any_rmem_tainted( tls_info* tdata )
{
    return check_mems_taint( tdata->rmemlogs, tdata->rmemlog_cnt );
}

static inline bool any_wmem_tainted( tls_info* tdata )
{
    return check_mems_taint( tdata->wmemlogs, tdata->wmemlog_cnt );
}

static inline bool is_tainted( regset_t used_regs, tls_info* tdata )
{
    return any_reg_tainted( used_regs )
        || any_rmem_tainted( tdata )
        || any_wmem_tainted ( tdata );
}

VOID PIN_FAST_ANALYSIS_CALL BblTail( THREADID tid,
                                     ADDRINT bbladdr,
                                     UINT32 size,
                                     UINT32 reg_upper,
                                     UINT32 reg_lower,
                                     ADDRINT esp,
                                     ADDRINT nextaddr,
                                     BOOL is_ret,
                                     BOOL is_call )
{
    const bbl_content* content;
    ADDRINT retaddr;
    tls_info* tdata =
        static_cast<tls_info*>( PIN_GetThreadData( g_tls_key, tid ) );
    regset_t used_regs = (((regset_t) reg_upper) << 32) | ((regset_t) reg_lower);

#ifdef DEBUG
    if ( stop_flag ) error_exit( "debug now" );
#endif
    g_bblcnt += 1;

    if ( g_bblcnt % BLK_PRINT_INTERVAL == 0 ) {
        out << "#"; out.flush();
    }
    if ( g_bblcnt % BLK_NEWLINE_INTERVAL == 0 ) {
        out << " " << std::dec << g_bblcnt << " blocks\n"; out.flush();
    }

    // this is safe,
    // because PIN's basic block does not have any control changes other than ret
    if ( is_ret ) {
        PIN_SafeCopy( &retaddr, (VOID*) esp, sizeof( ADDRINT ) );
        handle_ret( retaddr, tid );
    } else if ( is_call ) {
        retaddr = nextaddr;
        handle_call( retaddr ,tid );
    } else if ( tdata->pdom_hit ) {
        handle_ret( tdata->next_pdom_to_hit, tid );
        tdata->pdom_hit = 0;
        tdata->next_pdom_to_hit = 0;
    } else {}

    // if it is a clean block ... no need to interpret
    if ( !is_tainted( used_regs, tdata ) ) {
        return;
    }

    content = bbl_content_get( (unsigned char*) bbladdr, size );
    int ret = bbl_instrument( bbladdr,
                              content,
                              tdata->bbl_ctxt,
                              tid );
    if ( ret != 0 ) stop_and_debug();
    ADDRINT postdom = check_postdom();
    tdata->next_pdom_to_hit = postdom;
}

void PIN_FAST_ANALYSIS_CALL CheckPDom( THREADID tid, ADDRINT eip )
{
    tls_info* tdata =
        static_cast<tls_info*>( PIN_GetThreadData( g_tls_key, tid ) );

    if ( tdata->next_pdom_to_hit == eip ) tdata->pdom_hit = 1;
}

void PIN_FAST_ANALYSIS_CALL InsInstrW( THREADID tid, ADDRINT addr,
                                       UINT32 size, ADDRINT eip )
{
    tls_info* tdata =
        static_cast<tls_info*>( PIN_GetThreadData( g_tls_key, tid ) );

    push_log( tdata->wmemlogs, &tdata->wmemlog_cnt, addr, size, true );

    if ( tdata->next_pdom_to_hit == eip ) tdata->pdom_hit = 1;
}

void PIN_FAST_ANALYSIS_CALL InsInstrR( THREADID tid, ADDRINT addr,
                                       UINT32 size, ADDRINT eip )
{
    tls_info* tdata =
        static_cast<tls_info*>( PIN_GetThreadData( g_tls_key, tid ) );

    push_log( tdata->rmemlogs, &tdata->rmemlog_cnt, addr, size, false );

    if ( tdata->next_pdom_to_hit == eip ) tdata->pdom_hit = 1;
}

static inline bool want_to_start( ADDRINT addr, bool* instrumentation_start )
{
    IMG img = IMG_FindByAddress( addr );

    if ( !IMG_Valid(img) ) // kernel area
        return true;

    const char* tmpStr = IMG_Name(img).c_str();
    const char* ptr = strrchr( tmpStr, '/' );

    if ( strncmp( (ptr+1), g_progname, ::strlen(tmpStr) - 1 ) == 0 ) {
        *instrumentation_start = true;
        CODECACHE_FlushCache();
        return true;
    } else {
        return false;
    }
}

#ifdef DEBUG
VOID contextDbg( const CONTEXT *ctxt )
{
    out << std::hex
        << "BBL: " << PIN_GetContextReg( ctxt, REG_EIP ) << "\n---CTXT---\n"
        << "EAX:" << PIN_GetContextReg( ctxt, REG_EAX ) << "; "
        << "EBX:" << PIN_GetContextReg( ctxt, REG_EBX ) << "; "
        << "ECX:" << PIN_GetContextReg( ctxt, REG_ECX ) << "; "
        << "EDX:" << PIN_GetContextReg( ctxt, REG_EDX ) << ";\n"
        << "EDI:" << PIN_GetContextReg( ctxt, REG_EDI ) << "; "
        << "ESI:" << PIN_GetContextReg( ctxt, REG_ESI ) << "; "
        << "EBP:" << PIN_GetContextReg( ctxt, REG_EBP ) << "; "
        << "ESP:" << PIN_GetContextReg( ctxt, REG_ESP ) << ";" << endl;
    out.flush();
}

VOID DbgInstruction( INS ins, VOID *v )
{
    // Insert a call to docount before every instruction,
    // no arguments are passed
    INS_InsertCall( ins,
                    IPOINT_BEFORE,
                    (AFUNPTR)contextDbg,
                    IARG_CONST_CONTEXT,
                    IARG_END );
}

#endif
VOID cbLoadImage( IMG img, VOID *v )
{
  string imageName = IMG_Name(img);

  out << "Loaded image: " << imageName << " at "
      << std::hex << IMG_LowAddress(img) << endl;
}

inline regset_t get_used_regs( BBL bbl )
{
    regset_t used_regs = regset_empty;

    for( INS ins = BBL_InsHead( bbl ); INS_Valid( ins ); ins = INS_Next( ins ) )
    {
        xed_decoded_inst_t *xedd = INS_XedDec( ins );
        const xed_simple_flag_t *rfi = xed_decoded_inst_get_rflags_info( xedd );
        if( rfi != NULL ) {
            used_regs |= regset_eflags;
        }

        for ( unsigned int i = 0; i < INS_MaxNumWRegs( ins ); i ++ ) {
            used_regs |= get_regset( INS_RegW( ins, i ) );
        }

        if ( INS_IsSyscall( ins ) )
            used_regs |= regset_eax;

        for ( unsigned int i = 0; i < INS_MaxNumRRegs( ins ); i ++ ) {
            used_regs |= get_regset( INS_RegR( ins, i ) );
        }

        for ( unsigned int i = 0; i < INS_MemoryOperandCount( ins ); i ++ ) {
            used_regs |= get_regset( INS_OperandMemoryBaseReg( ins, i ) );
            used_regs |= get_regset( INS_OperandMemoryIndexReg( ins, i ) );
        }
    }

    return used_regs;
}

VOID per_instr_calls( BBL bbl )
{
    bool b_inserted = false;
    for( INS ins = BBL_InsHead( bbl ); INS_Valid( ins ); ins = INS_Next( ins ) )
    {
        const UINT32 memOperands = INS_MemoryOperandCount( ins );
        b_inserted = false;

        for ( UINT32 memOp = 0; memOp < memOperands; memOp ++ ) {
            if ( INS_MemoryOperandIsWritten( ins, memOp ) ) {
                b_inserted = true;
                INS_InsertPredicatedCall( ins, IPOINT_BEFORE,
                                          (AFUNPTR) InsInstrW,
                                          IARG_FAST_ANALYSIS_CALL,
                                          IARG_THREAD_ID,
                                          IARG_MEMORYOP_EA, memOp,
                                          IARG_MEMORYWRITE_SIZE,
                                          IARG_INST_PTR,
                                          IARG_END );
            }
            if ( INS_MemoryOperandIsRead( ins, memOp ) ) {
                b_inserted = true;
                INS_InsertPredicatedCall( ins, IPOINT_BEFORE,
                                          (AFUNPTR) InsInstrR,
                                          IARG_FAST_ANALYSIS_CALL,
                                          IARG_THREAD_ID,
                                          IARG_MEMORYOP_EA, memOp,
                                          IARG_MEMORYREAD_SIZE,
                                          IARG_INST_PTR,
                                          IARG_END );
            }
        }

        if ( ! b_inserted ) {
            INS_InsertCall( ins, IPOINT_BEFORE, (AFUNPTR) CheckPDom,
                            IARG_FAST_ANALYSIS_CALL,
                            IARG_THREAD_ID,
                            IARG_INST_PTR,
                            IARG_END );
        }
    }
}


VOID PerTrace( TRACE trace, VOID *v )
{
    static bool instrumentation_start = false;

    for ( BBL bbl = TRACE_BblHead(trace); BBL_Valid(bbl); bbl = BBL_Next(bbl) )
    {

        if ( instrumentation_start
          || want_to_start( BBL_Address(bbl), &instrumentation_start ) )
        {
            INS insHead = BBL_InsHead( bbl );
            INS insTail = BBL_InsTail( bbl );

            regset_t used_regs = get_used_regs( bbl );

            INS_InsertCall( insHead,
                            IPOINT_BEFORE,
                            (AFUNPTR) BblHead,
                            IARG_FAST_ANALYSIS_CALL,
                            IARG_CALL_ORDER, CALL_ORDER_FIRST,
                            IARG_THREAD_ID,
                            IARG_INST_PTR,
                            IARG_CONST_CONTEXT,
                            IARG_END );

            per_instr_calls( bbl );

            INS_InsertCall( insTail,
                            IPOINT_BEFORE,
                            (AFUNPTR) BblTail,
                            IARG_FAST_ANALYSIS_CALL,
                            IARG_THREAD_ID,
                            IARG_ADDRINT, INS_Address( insHead ),
                            IARG_UINT32, (UINT32) BBL_Size( bbl ),
                            IARG_UINT32, (UINT32) (used_regs >> 32),
                            IARG_UINT32, (UINT32) (used_regs & 0xffffffff),
                            IARG_REG_VALUE, REG_ESP,
                            IARG_ADDRINT, (INS_Address( insTail ) + INS_Size( insTail )),
                            IARG_BOOL, (BOOL) (INS_Category( insTail ) == XED_CATEGORY_RET),
                            IARG_BOOL, (BOOL) (INS_Category( insTail ) == XED_CATEGORY_CALL),
                            IARG_END );
        }
    }
}

VOID ThreadStart( THREADID tid,
                  CONTEXT *ctxt,
                  INT32 flags,
                  VOID *v )
{
    static unsigned threadCnt = 0;
    const BOOL debug_flag = KnobDebug.Value();
    ADDRINT esp;
    tls_info* tls = new tls_info;
    init_memlogs( &tls->wmemlogs );
    init_memlogs( &tls->rmemlogs );
    tls->wmemlog_cnt = 0;
    tls->rmemlog_cnt = 0;
    tls->bbl_ctxt = new reg_context;
    tls->next_pdom_to_hit = 0;
    tls->pdom_hit = 0;

    PIN_SetThreadData( g_tls_key, tls, tid );

    if ( threadCnt++ > 0 ) {
        return;
    }

    esp = (ADDRINT) PIN_GetContextReg( ctxt, REG_ESP );

    g_argcp = (int*) esp;
    g_argvp = (char**) ( esp + sizeof(ADDRINT) );
    // #argv + 2 (= argc itself + NULL)
    g_envp = (char**) (esp + (*((int*)esp)+2) * sizeof(ADDRINT));

    if ( KnobBinName.Value() != "" ) {
        g_progname = ::strdup( KnobBinName.Value().c_str() );
    } else {
        g_progname = ::strdup( g_argvp[0] );
    }
    not_null( g_progname );
    g_progname = ::basename( g_progname );

    char **env = g_envp;
    while ( *env != NULL ) env++;
    int envc = env - g_envp;

    proc_start( KnobOutputDirectory.Value().c_str(),
                KnobAnalysisId.Value(),
                KnobSockName.Value().c_str(),
                debug_flag,
                tid,
                g_argvp,
                envc,
                g_envp );

    out << timestamp()
        << " [AFUZZ] started\n" << std::hex
        << "    argc     = " << *g_argcp << endl
        << "    argv     = " << g_argvp << endl
        << "    argv[0]  = " << g_argvp[0] << endl
        << "    envp     = " << g_envp << endl;
}

VOID Fini( INT32 code, VOID *v )
{
    proc_end( g_bblcnt );

    out << timestamp() << " finished" << endl;
    close_log();
}

tls_info* get_tls_sys_num( THREADID tid )
{
    tls_info* tdata =
        static_cast<tls_info*>( PIN_GetThreadData( g_tls_key, tid ) );

    return tdata;
}

VOID SyscallEntry( THREADID tid, CONTEXT* ctxt, SYSCALL_STANDARD std, VOID* v )
{
    tls_info *p_tls = get_tls_sys_num( tid );
    int syscall_num = PIN_GetSyscallNumber( ctxt, std );
    p_tls->syscall_num = syscall_num;

    switch ( syscall_num ) {
    case SYS_open:
        p_tls->arg0 = PIN_GetSyscallArgument( ctxt, std, 0 );
        p_tls->arg1 = PIN_GetSyscallArgument( ctxt, std, 1 );
        p_tls->arg2 = PIN_GetSyscallArgument( ctxt, std, 2 );
        break;
    case SYS_close:
        p_tls->arg0 = PIN_GetSyscallArgument( ctxt, std, 0 );
        break;
    case SYS_read:
        p_tls->arg0 = PIN_GetSyscallArgument( ctxt, std, 0 );
        p_tls->arg1 = PIN_GetSyscallArgument( ctxt, std, 1 );
        p_tls->arg2 = PIN_GetSyscallArgument( ctxt, std, 2 );
        break;
    case SYS_dup3:
        p_tls->arg2 = PIN_GetSyscallArgument( ctxt, std, 2 );
    case SYS_dup2:
        p_tls->arg1 = PIN_GetSyscallArgument( ctxt, std, 1 );
    case SYS_dup:
        p_tls->arg0 = PIN_GetSyscallArgument( ctxt, std, 0 );
        break;
    case SYS_lseek:
        p_tls->arg0 = PIN_GetSyscallArgument( ctxt, std, 0 );
        p_tls->arg1 = PIN_GetSyscallArgument( ctxt, std, 1 );
        p_tls->arg2 = PIN_GetSyscallArgument( ctxt, std, 2 );
        break;
#ifdef SYS__llseek
    case SYS__llseek:
        p_tls->arg0 = PIN_GetSyscallArgument( ctxt, std, 0 );
        p_tls->arg1 = PIN_GetSyscallArgument( ctxt, std, 1 );
        p_tls->arg2 = PIN_GetSyscallArgument( ctxt, std, 2 );
        p_tls->arg3 = PIN_GetSyscallArgument( ctxt, std, 3 );
        p_tls->arg4 = PIN_GetSyscallArgument( ctxt, std, 4 );
        break;
#endif
    default:
        break;
    }
}

VOID syscall_post_open( const char* pathname, int fd )
{
    set<string>::const_iterator it = g_filenames.find( pathname );
    if ( it != g_filenames.end() ) {
        g_symbolic_fds.insert( make_pair( fd, fd_type( pathname, 0 ) ) );
    }
}

VOID syscall_post_close( int fd, int ret )
{
    if ( ret == 0 ) {
        map<int, fd_type>::iterator it = g_symbolic_fds.find( fd );
        if ( it != g_symbolic_fds.end() ) {
            g_symbolic_fds.erase( it );
        }
    }
}

VOID syscall_post_dup( int oldfd, int newfd )
{
    if ( newfd >= 0 ) {
        map<int, fd_type>::const_iterator it = g_symbolic_fds.find( oldfd );
        if ( it != g_symbolic_fds.end() ) {
            g_symbolic_fds.insert( make_pair( newfd, fd_type( it->second ) ) );
        }
    }
}

VOID syscall_post_read( ADDRINT fd, ADDRINT buf, ADDRINT ret )
{
    map<int, fd_type>::iterator it = g_symbolic_fds.find( (int) fd );
    if ( it != g_symbolic_fds.end() ) {
        symbolic_read( buf, it->second.position,
                       ret,
                       it->second.totalsize,
                       it->second.filename.c_str() );
        if ( ret > 0 )
            it->second.position += ret;
    }
}

VOID symbolic_lseek( map<int, fd_type>::iterator& it, unsigned long offset )
{
    it->second.position = offset;
}

VOID syscall_post_lseek( ADDRINT fd, ADDRINT ret )
{
    if ( ret < 0 ) return;

    map<int, fd_type>::iterator it = g_symbolic_fds.find( (int) fd );
    if ( it != g_symbolic_fds.end() ) {
        symbolic_lseek( it, (unsigned long) ret );
    }
}

VOID syscall_post__llseek( ADDRINT fd, ADDRINT res, ADDRINT ret )
{
    if ( ret < 0 ) return;

    loff_t offset;
    map<int, fd_type>::iterator it = g_symbolic_fds.find( (int) fd );

    if ( it != g_symbolic_fds.end() ) {
        PIN_SafeCopy( &offset, (VOID*) res, sizeof(loff_t) );
        symbolic_lseek( it, (unsigned long) offset );
    }
}

VOID SyscallExit( THREADID tid, CONTEXT* ctxt, SYSCALL_STANDARD std, VOID* v )
{
    tls_info *p_tls = get_tls_sys_num( tid );

    switch ( p_tls->syscall_num ) {
    case SYS_open:
        syscall_post_open( (const char*) p_tls->arg0,
                           PIN_GetSyscallReturn( ctxt, std ) );
        break;
    case SYS_close:
        syscall_post_close( p_tls->arg0,
                            PIN_GetSyscallReturn( ctxt, std ) );
    case SYS_read:
        syscall_post_read( p_tls->arg0,
                           p_tls->arg1,
                           PIN_GetSyscallReturn( ctxt, std ) );
        break;
    case SYS_dup:
    case SYS_dup2:
    case SYS_dup3:
        syscall_post_dup( p_tls->arg0, PIN_GetSyscallReturn( ctxt, std ) );
        break;
    case SYS_lseek:
        syscall_post_lseek( p_tls->arg0, PIN_GetSyscallReturn( ctxt, std ) );
        break;
#ifdef SYS__llseek
    case SYS__llseek:
        syscall_post__llseek( p_tls->arg0, p_tls->arg3,
                              PIN_GetSyscallReturn( ctxt, std ) );
        break;
#endif
    default:
        break;
    }
}

void init_pin()
{
    CODECACHE_ChangeMaxInsPerTrace( 16384 );

    // Open logfile
    string fileName = KnobOutputFile.Value();
    const char* filePath = fileName.c_str();

    if ( file_exists( filePath ) ) {
        open_log_append( filePath );
    } else {
        open_log_new( filePath );
    }

    for ( unsigned i = 0 ; i < KnobFileNames.NumberOfValues() ; i++ ) {
        g_filenames.insert( KnobFileNames.Value( i ) );
    }

    init_callbacks();
}

int main( int argc, char *argv[] )
{
    if( PIN_Init(argc,argv) )
    {
        return Usage();
    }

    init_pin(); // initialize global variables
    init_ocaml( argv );

    // PIN_InitLock( &g_lock );
    g_tls_key = PIN_CreateThreadDataKey( 0 );

    // Register function to be called to instrument traces
    TRACE_AddInstrumentFunction( PerTrace, 0 );

#ifdef DEBUG
    // for debugging only
    INS_AddInstrumentFunction( DbgInstruction, 0 );
#endif
    IMG_AddInstrumentFunction( cbLoadImage, 0 );

    // Register function to be called for every thread before it starts running
    PIN_AddThreadStartFunction( ThreadStart, 0 );

    // Register function to be called when the application exits
    PIN_AddFiniFunction( Fini, 0 );

    // Register syscall instrumentation
    PIN_AddSyscallEntryFunction( SyscallEntry, 0 );
    PIN_AddSyscallExitFunction( SyscallExit, 0 );

    PIN_StartProgram();

    return 0;
}

