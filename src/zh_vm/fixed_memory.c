/*
 * The Fixed Memory API
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * Copyright 1999-2001 Viktor Szakats (zh_xquery())
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
 *
 * As a special exception, the Ziher Project gives permission for
 * additional uses of the text contained in its release of Ziher.
 *
 * The exception is that, if you link the Ziher libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Ziher library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Ziher
 * Project under the name Ziher.  If you copy code from other
 * Ziher Project or Free Software Foundation releases into a copy of
 * Ziher, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Ziher, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/* NOTE: This definitions must be ahead of any and all #include statements */

#if ! defined( ZH_FM_STATISTICS ) && \
    ! defined( ZH_FM_STATISTICS_OFF ) && \
    ! defined( ZH_FM_STATISTICS_DYN_OFF )
#  define ZH_FM_STATISTICS_OFF
#endif

/* For Linux and mremap() function */
#ifndef _GNU_SOURCE
#  define _GNU_SOURCE
#endif


#define INCL_BASE

/* malloc.h has been obsoleted by stdlib.h, which is included via
   zh_vm_pub.h, which is include via zh_api.h
   #include <malloc.h>
 */

#define ZH_STACK_PRELOAD

#include "zh_vm_opt.h"
#include "zh_api.h"
#include "zh_item_api.h"
#include "zh_fs_api.h"
#include "zh_stack.h"
#include "zh_error_api.h"
#include "memory.zhh"
#include "zh_date.h"
#include "zh_set.h"
#include "zh_vm.h"

#if defined( ZH_OS_WIN )
#  include <windows.h>
#endif

#  include "zh_thread.h"
#  include "zh_atomic.h"

#if defined( ZH_FM_STD_ALLOC )
#  undef ZH_FM_DL_ALLOC
#  undef ZH_FM_DLMT_ALLOC
#  undef ZH_FM_WIN_ALLOC
#elif defined( ZH_FM_WIN_ALLOC )
#  undef ZH_FM_DL_ALLOC
#elif ! defined( ZH_FM_DL_ALLOC ) && ! defined( ZH_FM_WIN_ALLOC )
#  if defined( _MSC_VER ) || defined( ZH_FM_DLMT_ALLOC )
#     define ZH_FM_DL_ALLOC
#  else
      /* #define ZH_FM_DL_ALLOC */
#  endif
#endif

#if defined( ZH_FM_STATISTICS_OFF )
#  undef ZH_FM_STATISTICS
#endif


/* #define ZH_FM_WIN_ALLOC */
/* #define ZH_FM_STATISTICS */
/* #define ZH_PARANOID_MEM_CHECK */

#if defined( ZH_FM_DL_ALLOC )
#  if ! defined( ZH_FM_DLMT_ALLOC ) && ! defined( ZH_FM_DLMT_ALLOC_OFF ) 
#     if ( defined( _MSC_VER ) && ( _MSC_VER <= 1500 ) )
#        define ZH_FM_DLMT_ALLOC
#     endif
#  endif
/* #  define NO_MALLINFO 1 */
/* #  define INSECURE */
/* #  define USE_DL_PREFIX */
#  undef FORCEINLINE
#  if ! defined( FORCEINLINE )
#     define FORCEINLINE      ZH_FORCEINLINE
#  endif
#  define REALLOC_ZERO_BYTES_FREES
#     if defined( ZH_SPINLOCK_R )
#        define USE_LOCKS     2
#     else
#        define USE_LOCKS     1
#     endif
#     if defined( ZH_FM_DLMT_ALLOC )
#        define ONLY_MSPACES  1
#        define FOOTERS       1
#     endif
#  if defined( _MSC_VER )
#     if ! defined( USE_DL_PREFIX ) && ! defined( ZH_FM_DLMT_ALLOC )
#        define USE_DL_PREFIX
#     endif
#     pragma warning( push )
#     pragma warning( disable : 4702 )
#     if defined( ZH_OS_WIN_64 )
#        pragma warning( disable : 4267 )
#     endif
#  elif defined( __MINGW32__ )
#     if ! defined( USE_DL_PREFIX ) && ! defined( ZH_FM_DLMT_ALLOC )
#        define USE_DL_PREFIX
#     endif
#  endif
#  if defined( __cplusplus ) && ! defined( USE_DL_PREFIX )
#     define USE_DL_PREFIX
#  endif
#  if defined( ZH_OS_WIN )
#     if ! defined( ENOMEM )
#        define ENOMEM  12
#     endif
#     if ! defined( EINVAL )
#        define EINVAL  22
#     endif
#  endif
#  include "win_dl_malloc.h"
#  if defined( _MSC_VER )
#     pragma warning( pop )
#  endif
#  if defined( ZH_FM_DLMT_ALLOC )
#     define malloc( n )         mspace_malloc( zh_mspace(), ( n ) )
#     define realloc( p, n )     mspace_realloc( NULL, ( p ), ( n ) )
#     define free( p )           mspace_free( NULL, ( p ) )
#  elif defined( USE_DL_PREFIX )
#     define malloc( n )         dlmalloc( ( n ) )
#     define realloc( p, n )     dlrealloc( ( p ), ( n ) )
#     define free( p )           dlfree( ( p ) )
#  endif
#else
#  undef ZH_FM_DLMT_ALLOC
#  if defined( ZH_FM_WIN_ALLOC ) && defined( ZH_OS_WIN )
#     if defined( ZH_FM_LOCALALLOC )
#        define malloc( n )      ( void * ) LocalAlloc( LMEM_FIXED, ( n ) )
#        define realloc( p, n )  ( void * ) LocalReAlloc( ( HLOCAL ) ( p ), ( n ), LMEM_MOVEABLE )
#        define free( p )        LocalFree( ( HLOCAL ) ( p ) )
#     else
         static HANDLE s_hProcessHeap = NULL;
#        define ZH_FM_NEED_INIT
#        define ZH_FM_HEAP_INIT
#        define malloc( n )      ( void * ) HeapAlloc( s_hProcessHeap, 0, ( n ) )
#        define realloc( p, n )  ( void * ) HeapReAlloc( s_hProcessHeap, 0, ( void * ) ( p ), ( n ) )
#        define free( p )        HeapFree( s_hProcessHeap, 0, ( void * ) ( p ) )
#     endif
#  endif
#endif

#if ( defined( ZH_FM_STATISTICS ) || defined( ZH_FM_DLMT_ALLOC ) || \
      ! defined( ZH_ATOM_INC ) || ! defined( ZH_ATOM_DEC ) )

   static ZH_CRITICAL_NEW( s_fmMtx );
#  define ZH_FM_LOCK()           do { zh_threadEnterCriticalSection( &s_fmMtx )
#  define ZH_FM_UNLOCK()         zh_threadLeaveCriticalSection( &s_fmMtx ); } while( 0 )

#else

#  define ZH_FM_LOCK()
#  define ZH_FM_UNLOCK()

#endif

#if defined( ZH_FM_STATISTICS )
#  if ! defined( ZH_FM_NEED_INIT )
#     define ZH_FM_NEED_INIT
#  endif
#else
#  undef ZH_PARANOID_MEM_CHECK
#endif

#ifdef ZH_FM_NEED_INIT
static ZH_BOOL s_fInitedFM = ZH_FALSE;
#endif

#ifndef ZH_MEMFILER
#  define ZH_MEMFILER         0xff
#endif

#ifdef ZH_FM_STATISTICS

#define ZH_MEMINFO_SIGNATURE  0xfeedbeef

typedef struct _ZH_MEMINFO
{
   ZH_U32    u32Signature;
   int       iProcLine;
   ZH_USHORT uiReserved;
   ZH_SIZE   nSize;
   char      szProcName[ ZH_SYMBOL_NAME_LEN + 1 ];
   struct _ZH_MEMINFO * pPrevBlock;
   struct _ZH_MEMINFO * pNextBlock;
} ZH_MEMINFO, * PZH_MEMINFO;

#ifdef ZH_ALLOC_ALIGNMENT
#  define _ZH_MEMINFO_SIZE    ( ( ( sizeof( ZH_MEMINFO ) + ZH_ALLOC_ALIGNMENT - 1 ) - \
                                  ( sizeof( ZH_MEMINFO ) + ZH_ALLOC_ALIGNMENT - 1 ) % ZH_ALLOC_ALIGNMENT ) + \
                                ZH_COUNTER_OFFSET )
#else
#  define _ZH_MEMINFO_SIZE    ( sizeof( ZH_MEMINFO ) + ZH_COUNTER_OFFSET )
#endif

#define ZH_MEMINFO_SIZE       ( s_fStatistic ? sizeof( ZH_MEMINFO ) + ZH_COUNTER_OFFSET : ZH_COUNTER_OFFSET )
#define ZH_MEMSIG_SIZE        sizeof( ZH_U32 )

#define ZH_FM_GETSIG( p, n )  ZH_GET_UINT32( ( ZH_BYTE * ) ( p ) + ( n ) )
#define ZH_FM_SETSIG( p, n )  ZH_PUT_UINT32( ( ZH_BYTE * ) ( p ) + ( n ), ZH_MEMINFO_SIGNATURE )
#define ZH_FM_CLRSIG( p, n )  ZH_PUT_UINT32( ( ZH_BYTE * ) ( p ) + ( n ), 0 )

#define ZH_ALLOC_SIZE( n )    ( ( n ) + ( s_fStatistic ? _ZH_MEMINFO_SIZE + ZH_MEMSIG_SIZE : ZH_COUNTER_OFFSET ) )
#define ZH_FM_PTR( p )        ( ( PZH_MEMINFO ) ( ( ZH_BYTE * ) ( p ) - ZH_MEMINFO_SIZE ) )

#define ZH_FM_BLOCKSIZE( p )  ( s_fStatistic ? ZH_FM_PTR( pMem )->nSize : 0 )

/* NOTE: we cannot use here ZH_TRACE because it will overwrite the
 * function name/line number of code which called zh_xalloc()/zh_xgrab()
 */
#define ZH_TRACE_FM  ZH_TRACE_STEALTH

static ZH_BOOL s_fStatistic = ZH_FALSE;

static ZH_I_SIZE s_nMemoryBlocks      = 0; /* memory blocks used */
static ZH_I_SIZE s_nMemoryMaxBlocks   = 0; /* maximum number of used memory blocks */
static ZH_I_SIZE s_nMemoryConsumed    = 0; /* memory size consumed */
static ZH_I_SIZE s_nMemoryMaxConsumed = 0; /* memory max size consumed */
static ZH_I_SIZE s_nMemoryLimConsumed = 0; /* limit the size of memory consumed */

static PZH_MEMINFO s_pFirstBlock = NULL;
static PZH_MEMINFO s_pLastBlock  = NULL;

static char s_szFileName[ ZH_PATH_MAX ] = { '\0' };
static char s_szInfo[ 256 ] = { '\0' };

#else /* ! ZH_FM_STATISTICS */

typedef void * PZH_MEMINFO;
#define ZH_MEMINFO_SIZE  ZH_COUNTER_OFFSET
#define ZH_ALLOC_SIZE( n )  ( ( n ) + ZH_MEMINFO_SIZE )
#define ZH_FM_PTR( p )      ZH_COUNTER_PTR( p )
#define ZH_TRACE_FM      ZH_TRACE

#endif /* ZH_FM_STATISTICS */

#define ZH_MEM_PTR( p )     ( ( void * ) ( ( ZH_BYTE * ) ( p ) + ZH_MEMINFO_SIZE ) )



#if ! defined( ZH_ATOM_INC ) || ! defined( ZH_ATOM_DEC )

   /* ZH_ATOM_INC and ZH_ATOM_DEC have to be synced together */
#  undef ZH_ATOM_DEC
#  undef ZH_ATOM_INC
#  undef ZH_ATOM_GET
#  undef ZH_ATOM_SET
   static ZH_FORCEINLINE void zh_counterIncrement( volatile ZH_COUNTER * p )
   {
      ZH_FM_LOCK();
      ++( *p );
      ZH_FM_UNLOCK();
   }
#  define ZH_ATOM_INC( p )    zh_counterIncrement( p )
   static ZH_FORCEINLINE int zh_counterDecrement( volatile ZH_COUNTER * p )
   {
      int iResult;

      ZH_FM_LOCK();
      iResult = --( *p ) != 0;
      ZH_FM_UNLOCK();
      return iResult;
   }
#  define ZH_ATOM_DEC( p )    zh_counterDecrement( p )
#endif

#ifndef ZH_ATOM_GET
#  define ZH_ATOM_GET( p )     ( *( p ) )
#endif
#ifndef ZH_ATOM_SET
#  define ZH_ATOM_SET( p, n )  ( ( *( p ) ) = ( n ) )
#endif


#if defined( ZH_FM_DLMT_ALLOC )

#  if ! defined( ZH_MSPACE_COUNT )
#     define ZH_MSPACE_COUNT  16
#  endif

typedef struct
{
   int    count;
   mspace ms;
} ZH_MSPACE, * PZH_MSPACE;

static mspace    s_gm = NULL;
static ZH_MSPACE s_mspool[ ZH_MSPACE_COUNT ];


static mspace zh_mspace( void )
{
   PZH_MSPACE pm = ( PZH_MSPACE ) zh_stackAllocator();

   if( pm )
      return pm->ms;

   if( ! s_gm )
      s_gm = create_mspace( 0, 1 );

   return s_gm;
}

static PZH_MSPACE zh_mspace_alloc( void )
{
   if( s_mspool[ 0 ].ms == NULL && s_gm )
   {
      s_mspool[ 0 ].count = 1;
      s_mspool[ 0 ].ms = s_gm;
      return &s_mspool[ 0 ];
   }
   else
   {
      int i, imin = 0;
      for( i = 1; i < ZH_MSPACE_COUNT; ++i )
      {
         if( s_mspool[ i ].count < s_mspool[ imin ].count )
            imin = i;
      }
      if( s_mspool[ imin ].ms == NULL )
         s_mspool[ imin ].ms = create_mspace( 0, 1 );
      s_mspool[ imin ].count++;
      return &s_mspool[ imin ];
   }
}

static void * zh_mspace_update( void * pAlloc, int iCount )
{
   PZH_MSPACE pm = ( PZH_MSPACE ) pAlloc;

   if( pm && pm->count > iCount )
   {
      pAlloc = ( void * ) zh_mspace_alloc();
      pm->count--;
   }

   return pAlloc;
}

static void zh_mspace_cleanup( void )
{
   int i;

   s_gm = NULL;
   for( i = 0; i < ZH_MSPACE_COUNT; ++i )
   {
      if( s_mspool[ i ].ms )
      {
         destroy_mspace( s_mspool[ i ].ms );
         s_mspool[ i ].ms = NULL;
         s_mspool[ i ].count = 0;
      }
   }
}

#elif defined( ZH_FM_DL_ALLOC ) && defined( USE_DL_PREFIX )

static void dlmalloc_destroy( void )
{
   if( ok_magic( gm ) )
   {
      msegmentptr sp = &gm->seg;
      while( sp != 0 )
      {
         char * base = sp->base;
         size_t size = sp->size;
         flag_t flag = sp->sflags;
         sp = sp->next;
         if( ( flag & USE_MMAP_BIT ) && ! ( flag & EXTERN_BIT ) )
            CALL_MUNMAP( base, size );
      }
   }
}


#endif

void zh_xinit_thread( void )
{
#if defined( ZH_FM_DLMT_ALLOC )
   

   if( zh_stack.allocator == NULL )
   {
      ZH_FM_LOCK();
      zh_stack.allocator = ( void * ) zh_mspace_alloc();
      ZH_FM_UNLOCK();
   }
#endif
}

void zh_xexit_thread( void )
{
#if defined( ZH_FM_DLMT_ALLOC )
   
   PZH_MSPACE pm = ( PZH_MSPACE ) zh_stack.allocator;

   if( pm )
   {
      zh_stack.allocator = NULL;
      ZH_FM_LOCK();
      if( --pm->count == 0 )
         mspace_trim( pm->ms, 0 );
      ZH_FM_UNLOCK();
   }
#endif
}

void zh_xclean( void )
{
#if defined( ZH_FM_DLMT_ALLOC )
   ZH_FM_LOCK();
   {
      int i, imax, icount;

      if( s_gm )
         mspace_trim( s_gm, 0 );

      for( i = imax = icount = 0; i < ZH_MSPACE_COUNT; ++i )
      {
         if( s_mspool[ i ].ms )
         {
            icount += s_mspool[ i ].count;
            if( imax < s_mspool[ i ].count )
               imax = s_mspool[ i ].count;
            mspace_trim( s_mspool[ i ].ms, 0 );
         }
      }
      icount = ( icount + ZH_MSPACE_COUNT - 1 ) / ZH_MSPACE_COUNT;
      if( imax > icount )
      {
         /* balance mspaces between running threads */
         zh_vmUpdateAllocator( zh_mspace_update, icount );
      }
   }
   ZH_FM_UNLOCK();
#elif defined( ZH_FM_DL_ALLOC )
   dlmalloc_trim( 0 );
#endif
}

void zh_xsetfilename( const char * szValue )
{
#ifdef ZH_FM_STATISTICS
   if( szValue )
      zh_strncpy( s_szFileName, szValue, sizeof( s_szFileName ) - 1 );
   else
      s_szFileName[ 0 ] = '\0';
#else
   ZH_SYMBOL_UNUSED( szValue );
#endif
}

void zh_xsetinfo( const char * szValue )
{
#ifdef ZH_FM_STATISTICS
   zh_strncpy( s_szInfo, szValue, sizeof( s_szInfo ) - 1 );
#else
   ZH_SYMBOL_UNUSED( szValue );
#endif
}

void * zh_xalloc( ZH_SIZE nSize )         /* allocates fixed memory, returns NULL on failure */
{
   PZH_MEMINFO pMem;

   ZH_TRACE_FM( ZH_TR_DEBUG, ( "zh_xalloc(%" ZH_PFS "u)", nSize ) );

   if( nSize == 0 )
      zh_errInternal( ZH_EI_XALLOCNULLSIZE, NULL, NULL, NULL );

#ifdef ZH_FM_NEED_INIT
   if( ! s_fInitedFM )
      zh_xinit();
#endif

   pMem = ( PZH_MEMINFO ) malloc( ZH_ALLOC_SIZE( nSize ) );

   if( ! pMem )
      return pMem;

#ifdef ZH_FM_STATISTICS

   if( s_fStatistic )
   {
      PZH_TRACEINFO pTrace = zh_traceinfo();

      if( zh_tr_level() >= ZH_TR_DEBUG || pTrace->level == ZH_TR_FM )
      {
         /* NOTE: ZH line number/procname is not very useful during hunting
          * for memory leaks - this is why we are using the previously stored
          * function/line info - this is a location of code that called
          * zh_xalloc()/zh_xgrab()
          */
         pMem->iProcLine = pTrace->line;  /* C line number */
         if( pTrace->file )
            zh_strncpy( pMem->szProcName, pTrace->file, sizeof( pMem->szProcName ) - 1 );
         else
            pMem->szProcName[ 0 ] = '\0';
         pTrace->level = -1;
      }
      else
      {
         ZH_USHORT uiProcLine = 0;
         zh_stackBaseProcInfo( pMem->szProcName, &uiProcLine );
         pMem->iProcLine = uiProcLine;
      }

      ZH_FM_LOCK();

      if( ! s_pFirstBlock )
      {
         pMem->pPrevBlock = NULL;
         s_pFirstBlock = pMem;
      }
      else
      {
         pMem->pPrevBlock = s_pLastBlock;
         s_pLastBlock->pNextBlock = pMem;
      }
      s_pLastBlock = pMem;
      pMem->pNextBlock = NULL;

      pMem->u32Signature = ZH_MEMINFO_SIGNATURE;
      ZH_FM_SETSIG( ZH_MEM_PTR( pMem ), nSize );
      pMem->nSize = nSize;  /* size of the memory block */

      s_nMemoryConsumed += nSize + sizeof( ZH_COUNTER );
      if( s_nMemoryMaxConsumed < s_nMemoryConsumed )
         s_nMemoryMaxConsumed = s_nMemoryConsumed;
      s_nMemoryBlocks++;
      if( s_nMemoryMaxBlocks < s_nMemoryBlocks )
         s_nMemoryMaxBlocks = s_nMemoryBlocks;

      ZH_FM_UNLOCK();

      if( s_nMemoryLimConsumed > 0 && s_nMemoryConsumed > s_nMemoryLimConsumed )
      {
         free( pMem );
         return NULL;
      }

#ifdef ZH_PARANOID_MEM_CHECK
      memset( ZH_MEM_PTR( pMem ), ZH_MEMFILER, nSize );
#endif

   }

#endif /* ZH_FM_STATISTICS */

   ZH_ATOM_SET( ZH_COUNTER_PTR( ZH_MEM_PTR( pMem ) ), 1 );

   return ZH_MEM_PTR( pMem );
}

void * zh_xgrab( ZH_SIZE nSize )         /* allocates fixed memory, exits on failure */
{
   PZH_MEMINFO pMem;

   //ZH_TRACE_FM( ZH_TR_DEBUG, ( "zh_xgrab(%" ZH_PFS "u)", nSize ) );

   if( nSize == 0 )
      zh_errInternal( ZH_EI_XGRABNULLSIZE, NULL, NULL, NULL );

#ifdef ZH_FM_NEED_INIT
   if( ! s_fInitedFM )
      zh_xinit();
#endif

   pMem = ( PZH_MEMINFO ) malloc( ZH_ALLOC_SIZE( nSize ) );

   if( ! pMem )
      zh_errInternal( ZH_EI_XGRABALLOC, NULL, NULL, NULL );

#ifdef ZH_FM_STATISTICS

   if( s_fStatistic )
   {
      PZH_TRACEINFO pTrace = zh_traceinfo();

      if( zh_tr_level() >= ZH_TR_DEBUG || pTrace->level == ZH_TR_FM )
      {
         /* NOTE: ZH line number/procname is not very useful during hunting
          * for memory leaks - this is why we are using the previously stored
          * function/line info - this is a location of code that called
          * zh_xalloc()/zh_xgrab()
          */
         pMem->iProcLine = pTrace->line;  /* C line number */
         if( pTrace->file )
            zh_strncpy( pMem->szProcName, pTrace->file, sizeof( pMem->szProcName ) - 1 );
         else
            pMem->szProcName[ 0 ] = '\0';
         pTrace->level = -1;
      }
      else
      {
         ZH_USHORT uiProcLine = 0;
         zh_stackBaseProcInfo( pMem->szProcName, &uiProcLine );
         pMem->iProcLine = uiProcLine;
      }

      ZH_FM_LOCK();

      if( ! s_pFirstBlock )
      {
         pMem->pPrevBlock = NULL;
         s_pFirstBlock = pMem;
      }
      else
      {
         pMem->pPrevBlock = s_pLastBlock;
         s_pLastBlock->pNextBlock = pMem;
      }
      s_pLastBlock = pMem;
      pMem->pNextBlock = NULL;

      pMem->u32Signature = ZH_MEMINFO_SIGNATURE;
      ZH_FM_SETSIG( ZH_MEM_PTR( pMem ), nSize );
      pMem->nSize = nSize;  /* size of the memory block */

      s_nMemoryConsumed += nSize + sizeof( ZH_COUNTER );
      if( s_nMemoryMaxConsumed < s_nMemoryConsumed )
         s_nMemoryMaxConsumed = s_nMemoryConsumed;
      s_nMemoryBlocks++;
      if( s_nMemoryMaxBlocks < s_nMemoryBlocks )
         s_nMemoryMaxBlocks = s_nMemoryBlocks;

      ZH_FM_UNLOCK();

      if( s_nMemoryLimConsumed > 0 && s_nMemoryConsumed > s_nMemoryLimConsumed )
      {
         s_nMemoryLimConsumed = 0;
         zh_errInternal( ZH_EI_XGRABALLOC, NULL, NULL, NULL );
      }

#ifdef ZH_PARANOID_MEM_CHECK
      memset( ZH_MEM_PTR( pMem ), ZH_MEMFILER, nSize );
#endif
   }

#endif /* ZH_FM_STATISTICS */

   ZH_ATOM_SET( ZH_COUNTER_PTR( ZH_MEM_PTR( pMem ) ), 1 );

   return ZH_MEM_PTR( pMem );
}

void * zh_xrealloc( void * pMem, ZH_SIZE nSize )       /* reallocates memory */
{
   //ZH_TRACE_FM( ZH_TR_DEBUG, ( "zh_xrealloc(%p, %" ZH_PFS "u)", pMem, nSize ) );


#ifdef ZH_FM_STATISTICS
   if( pMem == NULL )
   {
      if( nSize == 0 )
         zh_errInternal( ZH_EI_XREALLOCNULLSIZE, NULL, NULL, NULL );
      return zh_xgrab( nSize );
   }
   else if( nSize == 0 )
   {
      zh_xfree( pMem );
      return NULL;
   }
   else if( s_fStatistic )
   {
      PZH_MEMINFO pMemBlock;
      ZH_SIZE nMemSize;

      pMemBlock = ZH_FM_PTR( pMem );

      if( pMemBlock->u32Signature != ZH_MEMINFO_SIGNATURE )
         zh_errInternal( ZH_EI_XREALLOCINV, NULL, NULL, NULL );

      nMemSize = pMemBlock->nSize;

      if( ZH_FM_GETSIG( pMem, nMemSize ) != ZH_MEMINFO_SIGNATURE )
         zh_errInternal( ZH_EI_XMEMOVERFLOW, NULL, NULL, NULL );

      pMemBlock->u32Signature = 0;
      ZH_FM_CLRSIG( ZH_MEM_PTR( pMemBlock ), nMemSize );

#if defined( ZH_PARANOID_MEM_CHECK ) || defined( ZH_FM_FORCE_REALLOC )
      pMem = malloc( ZH_ALLOC_SIZE( nSize ) );
#  endif

      ZH_FM_LOCK();

#if ! ( defined( ZH_PARANOID_MEM_CHECK ) || defined( ZH_FM_FORCE_REALLOC ) )
      pMem = realloc( pMemBlock, ZH_ALLOC_SIZE( nSize ) );
#endif

      if( pMem )
      {
#if defined( ZH_PARANOID_MEM_CHECK ) || defined( ZH_FM_FORCE_REALLOC )
         memcpy( pMem, pMemBlock, nSize < nMemSize ?
                 ZH_ALLOC_SIZE( nSize ) : ZH_ALLOC_SIZE( nMemSize ) );
#endif

         s_nMemoryConsumed += ( nSize - nMemSize );
         if( s_nMemoryMaxConsumed < s_nMemoryConsumed )
            s_nMemoryMaxConsumed = s_nMemoryConsumed;

         ( ( PZH_MEMINFO ) pMem )->nSize = nSize;  /* size of the memory block */
         ( ( PZH_MEMINFO ) pMem )->u32Signature = ZH_MEMINFO_SIGNATURE;
         ZH_FM_SETSIG( ZH_MEM_PTR( pMem ), nSize );

         if( ( ( PZH_MEMINFO ) pMem )->pPrevBlock )
            ( ( PZH_MEMINFO ) pMem )->pPrevBlock->pNextBlock = ( PZH_MEMINFO ) pMem;
         if( ( ( PZH_MEMINFO ) pMem )->pNextBlock )
            ( ( PZH_MEMINFO ) pMem )->pNextBlock->pPrevBlock = ( PZH_MEMINFO ) pMem;

         if( s_pFirstBlock == pMemBlock )
            s_pFirstBlock = ( PZH_MEMINFO ) pMem;
         if( s_pLastBlock == pMemBlock )
            s_pLastBlock = ( PZH_MEMINFO ) pMem;
      }

      ZH_FM_UNLOCK();

      if( s_nMemoryLimConsumed > 0 && s_nMemoryConsumed > s_nMemoryLimConsumed )
      {
         s_nMemoryLimConsumed = 0;
         zh_errInternal( ZH_EI_XREALLOC, NULL, NULL, NULL );
      }

#if defined( ZH_PARANOID_MEM_CHECK ) || defined( ZH_FM_FORCE_REALLOC )
#  ifdef ZH_PARANOID_MEM_CHECK
      memset( pMemBlock, ZH_MEMFILER, ZH_ALLOC_SIZE( nMemSize ) );
      if( nSize > nMemSize && pMem )
         memset( ( ZH_BYTE * ) ZH_MEM_PTR( pMem ) + nMemSize, ZH_MEMFILER, nSize - nMemSize );
#  endif
      free( pMemBlock );
#endif
   }
   else
      pMem = realloc( ZH_FM_PTR( pMem ), ZH_ALLOC_SIZE( nSize ) );

   if( ! pMem )
      zh_errInternal( ZH_EI_XREALLOC, NULL, NULL, NULL );

#else

   if( pMem == NULL )
   {
      if( nSize == 0 )
         zh_errInternal( ZH_EI_XREALLOCNULLSIZE, NULL, NULL, NULL );
      pMem = malloc( ZH_ALLOC_SIZE( nSize ) );
      if( pMem )
         ZH_ATOM_SET( ZH_COUNTER_PTR( ZH_MEM_PTR( pMem ) ), 1 );
   }
   else if( nSize == 0 )
   {
      free( ZH_FM_PTR( pMem ) );
      return NULL;
   }
   else
   {
#ifdef ZH_FM_FORCE_REALLOC
      PZH_MEMINFO pMemBlock = ZH_FM_PTR( pMem );

      pMem = realloc( pMemBlock, ZH_ALLOC_SIZE( nSize ) );
      if( pMem == pMemBlock )
      {
         pMem = malloc( ZH_ALLOC_SIZE( nSize ) );
         memcpy( pMem, pMemBlock, ZH_ALLOC_SIZE( nSize ) );
         memset( pMemBlock, ZH_MEMFILER, ZH_ALLOC_SIZE( nSize ) );
         free( pMemBlock );
      }
#else
      pMem = realloc( ZH_FM_PTR( pMem ), ZH_ALLOC_SIZE( nSize ) );
#endif
   }

   if( ! pMem )
      zh_errInternal( ZH_EI_XREALLOC, NULL, NULL, NULL );

#endif

   return ZH_MEM_PTR( pMem );
}

void zh_xfree( void * pMem )            /* frees fixed memory */
{
   ZH_TRACE_FM( ZH_TR_DEBUG, ( "zh_xfree(%p)", pMem ) );

   if( pMem )
   {
#ifdef ZH_FM_STATISTICS

      PZH_MEMINFO pMemBlock = ZH_FM_PTR( pMem );

      if( s_fStatistic )
      {
         if( pMemBlock->u32Signature != ZH_MEMINFO_SIGNATURE )
            zh_errInternal( ZH_EI_XFREEINV, NULL, NULL, NULL );

         if( ZH_FM_GETSIG( pMem, pMemBlock->nSize ) != ZH_MEMINFO_SIGNATURE )
            zh_errInternal( ZH_EI_XMEMOVERFLOW, NULL, NULL, NULL );

         ZH_FM_LOCK();

         s_nMemoryConsumed -= pMemBlock->nSize + sizeof( ZH_COUNTER );
         s_nMemoryBlocks--;

         if( pMemBlock->pPrevBlock )
            pMemBlock->pPrevBlock->pNextBlock = pMemBlock->pNextBlock;
         else
            s_pFirstBlock = pMemBlock->pNextBlock;

         if( pMemBlock->pNextBlock )
            pMemBlock->pNextBlock->pPrevBlock = pMemBlock->pPrevBlock;
         else
            s_pLastBlock = pMemBlock->pPrevBlock;

         ZH_FM_UNLOCK();

         pMemBlock->u32Signature = 0;
         ZH_FM_CLRSIG( pMem, pMemBlock->nSize );
#ifdef ZH_PARANOID_MEM_CHECK
         memset( pMemBlock, ZH_MEMFILER, ZH_ALLOC_SIZE( pMemBlock->nSize ) );
#endif
      }

      free( pMemBlock );

#else

      free( ZH_FM_PTR( pMem ) );

#endif
   }
   else
      zh_errInternal( ZH_EI_XFREENULL, NULL, NULL, NULL );
}

/* increment reference counter */
#undef zh_xRefInc
void zh_xRefInc( void * pMem )
{
   ZH_ATOM_INC( ZH_COUNTER_PTR( pMem ) );
}

/* decrement reference counter, return ZH_TRUE when 0 reached */
#undef zh_xRefDec
ZH_BOOL zh_xRefDec( void * pMem )
{
   return ZH_ATOM_DEC( ZH_COUNTER_PTR( pMem ) ) == 0;
}

/* decrement reference counter and free the block when 0 reached */
#undef zh_xRefFree
void zh_xRefFree( void * pMem )
{
#ifdef ZH_FM_STATISTICS

   if( s_fStatistic && ZH_FM_PTR( pMem )->u32Signature != ZH_MEMINFO_SIGNATURE )
      zh_errInternal( ZH_EI_XFREEINV, NULL, NULL, NULL );

   if( ZH_ATOM_DEC( ZH_COUNTER_PTR( pMem ) ) == 0 )
      zh_xfree( pMem );

#else

   if( ZH_ATOM_DEC( ZH_COUNTER_PTR( pMem ) ) == 0 )
      free( ZH_FM_PTR( pMem ) );

#endif
}

/* return number of references */
#undef zh_xRefCount
ZH_COUNTER zh_xRefCount( void * pMem )
{
   return ZH_ATOM_GET( ZH_COUNTER_PTR( pMem ) );
}

/* reallocates memory, create copy if reference counter greater then 1 */
#undef zh_xRefResize
void * zh_xRefResize( void * pMem, ZH_SIZE nSave, ZH_SIZE nSize, ZH_SIZE * pnAllocated )
{

#ifdef ZH_FM_STATISTICS
   if( ZH_ATOM_GET( ZH_COUNTER_PTR( pMem ) ) > 1 )
   {
      void * pMemNew = memcpy( zh_xgrab( nSize ), pMem, ZH_MIN( nSave, nSize ) );

      if( ZH_ATOM_DEC( ZH_COUNTER_PTR( pMem ) ) == 0 )
         zh_xfree( pMem );

      *pnAllocated = nSize;
      return pMemNew;
   }
   else if( nSize <= *pnAllocated )
      return pMem;

   *pnAllocated = nSize;
   return zh_xrealloc( pMem, nSize );

#else

   if( ZH_ATOM_GET( ZH_COUNTER_PTR( pMem ) ) > 1 )
   {
      void * pMemNew = malloc( ZH_ALLOC_SIZE( nSize ) );

      if( pMemNew )
      {
         ZH_ATOM_SET( ZH_COUNTER_PTR( ZH_MEM_PTR( pMemNew ) ), 1 );
         memcpy( ZH_MEM_PTR( pMemNew ), pMem, ZH_MIN( nSave, nSize ) );
         if( ZH_ATOM_DEC( ZH_COUNTER_PTR( pMem ) ) == 0 )
            free( ZH_FM_PTR( pMem ) );
         *pnAllocated = nSize;
         return ZH_MEM_PTR( pMemNew );
      }
   }
   else if( nSize <= *pnAllocated )
      return pMem;
   else
   {
      *pnAllocated = nSize;
      pMem = realloc( ZH_FM_PTR( pMem ), ZH_ALLOC_SIZE( nSize ) );
      if( pMem )
         return ZH_MEM_PTR( pMem );
   }

   zh_errInternal( ZH_EI_XREALLOC, NULL, NULL, NULL );
   return NULL;
#endif
}

/* NOTE: Debug function, it will always return 0 when ZH_FM_STATISTICS is
         not defined, don't use it for final code [vszakats] */

ZH_SIZE zh_xsize( void * pMem ) /* returns the size of an allocated memory block */
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xsize(%p)", pMem ) );

#ifdef ZH_FM_STATISTICS
   return ZH_FM_BLOCKSIZE( pMem );
#else
   ZH_SYMBOL_UNUSED( pMem );

   return 0;
#endif
}

/* NOTE: Debug function, it will always return NULL when ZH_FM_STATISTICS is
         not defined, don't use it for final code */

const char * zh_xinfo( void * pMem, int * piLine )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xinfo(%p,%p)", pMem, ( void * ) piLine ) );

#ifdef ZH_FM_STATISTICS
   {
      PZH_MEMINFO pMemBlock = ZH_FM_PTR( pMem );

      if( piLine )
         *piLine = pMemBlock->iProcLine;

      return pMemBlock->szProcName;
   }
#else

   ZH_SYMBOL_UNUSED( pMem );

   if( piLine )
      *piLine = 0;

   return NULL;
#endif
}

void zh_xinit( void ) /* Initialize fixed memory subsystem */
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xinit()" ) );

#ifdef ZH_FM_NEED_INIT
   if( ! s_fInitedFM )
   {
      s_fInitedFM = ZH_TRUE;

#  if defined( ZH_FM_HEAP_INIT )
      s_hProcessHeap = GetProcessHeap();
#  endif

#  ifdef ZH_FM_STATISTICS
      {
         char buffer[ 5 ];

         if( zh_getenv_buffer( "ZH_FM_STAT", buffer, sizeof( buffer ) ) )
         {
            if( zh_stricmp( "yes", buffer ) == 0 )
               s_fStatistic = ZH_TRUE;
            else if( zh_stricmp( "no", buffer ) == 0 )
               s_fStatistic = ZH_FALSE;
         }
#     ifndef ZH_FM_STATISTICS_DYN_OFF
         else
            s_fStatistic = ZH_TRUE;  /* enabled by default */
#     endif /* ZH_FM_STATISTICS_DYN_OFF */
      }
#  endif /* ZH_FM_STATISTICS */
   }
#endif /* ZH_FM_NEED_INIT */
}

/* Returns pointer to string containing printable version
   of pMem memory block */

#ifdef ZH_FM_STATISTICS
static char * zh_mem2str( char * membuffer, void * pMem, ZH_SIZE nSize )
{
   ZH_BYTE * cMem = ( ZH_BYTE * ) pMem;
   ZH_SIZE nIndex, nPrintable;

   nPrintable = 0;
   for( nIndex = 0; nIndex < nSize; nIndex++ )
   {
      if( ( cMem[ nIndex ] & 0x60 ) != 0 )
         nPrintable++;
   }

   if( nPrintable * 100 / nSize > 70 ) /* more then 70% printable chars */
   {
      /* format as string of original chars */
      for( nIndex = 0; nIndex < nSize; nIndex++ )
      {
         if( cMem[ nIndex ] >= ' ' )
            membuffer[ nIndex ] = cMem[ nIndex ];
         else
            membuffer[ nIndex ] = '.';
      }
      membuffer[ nIndex ] = '\0';
   }
   else
   {
      /* format as hex */
      for( nIndex = 0; nIndex < nSize; nIndex++ )
      {
         ZH_BYTE hinibble = cMem[ nIndex ] >> 4;
         ZH_BYTE lownibble = cMem[ nIndex ] & 0x0F;
         membuffer[ nIndex * 2 ]     = hinibble <= 9 ?
                               ( '0' + hinibble ) : ( 'A' + hinibble - 10 );
         membuffer[ nIndex * 2 + 1 ] = lownibble <= 9 ?
                               ( '0' + lownibble ) : ( 'A' + lownibble - 10 );
      }
      membuffer[ nIndex * 2 ] = '\0';
   }

   return membuffer;
}

#define ZH_MAX_MEM2STR_BLOCK  256
void zh_xexit( void ) /* Deinitialize fixed memory subsystem */
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xexit()" ) );

   if( s_nMemoryBlocks || zh_cmdargCheck( "INFO" ) )
   {
      char membuffer[ ZH_MAX_MEM2STR_BLOCK * 2 + 1 ]; /* multiplied by 2 to allow hex format */
      PZH_MEMINFO pMemBlock;
      ZH_USHORT ui;
      char buffer[ 100 ];
      FILE * hLog = NULL;

      if( s_nMemoryBlocks )
         hLog = zh_fopen( s_szFileName[ 0 ] ? s_szFileName : "zh_out.log", "a+" );

      zh_conOutErr( zh_conNewLine(), 0 );
      zh_conOutErr( "----------------------------------------", 0 );
      zh_conOutErr( zh_conNewLine(), 0 );
      zh_snprintf( buffer, sizeof( buffer ), ZH_I_( "Total memory allocated: %" ZH_PFS "i bytes (%" ZH_PFS "i block(s))" ), s_nMemoryMaxConsumed, s_nMemoryMaxBlocks );
      zh_conOutErr( buffer, 0 );

      if( s_nMemoryBlocks )
      {
         if( hLog )
         {
            char szTime[ 9 ];
            int iYear, iMonth, iDay;

            zh_dateToday( &iYear, &iMonth, &iDay );
            zh_dateTimeStr( szTime );

            fprintf( hLog, ZH_I_( "Application Memory Allocation Report - %s\n" ), zh_cmdargARGVN( 0 ) );
            fprintf( hLog, ZH_I_( "Terminated at: %04d-%02d-%02d %s\n" ), iYear, iMonth, iDay, szTime );
            if( s_szInfo[ 0 ] )
               fprintf( hLog, ZH_I_( "Info: %s\n" ), s_szInfo );
            fprintf( hLog, "%s\n", buffer );
         }

         zh_conOutErr( zh_conNewLine(), 0 );
         zh_snprintf( buffer, sizeof( buffer ), ZH_I_( "Warning, memory allocated but not released: %" ZH_PFS "i bytes (%" ZH_PFS "i block(s))" ), s_nMemoryConsumed, s_nMemoryBlocks );
         zh_conOutErr( buffer, 0 );

         if( hLog )
            fprintf( hLog, "%s\n", buffer );
      }
      else
      {
         zh_conOutErr( zh_conNewLine(), 0 );
         zh_conOutErr( ZH_I_( "Memory allocated but not released: none" ), 0 );
      }

      zh_conOutErr( zh_conNewLine(), 0 );

      for( ui = 1, pMemBlock = s_pFirstBlock; pMemBlock; pMemBlock = pMemBlock->pNextBlock, ++ui )
      {
         ZH_TRACE( ZH_TR_ERROR, ( "Block %i (size %" ZH_PFS "u) %s(%i), \"%s\"", ui,
            pMemBlock->nSize, pMemBlock->szProcName, pMemBlock->iProcLine,
            zh_mem2str( membuffer, ( char * ) ZH_MEM_PTR( pMemBlock ),
                        ZH_MIN( pMemBlock->nSize, ZH_MAX_MEM2STR_BLOCK ) ) ) );

         if( hLog )
         {
            fprintf( hLog, ZH_I_( "Block %i %p (size %" ZH_PFS "u) %s(%i), \"%s\"\n" ), ui,
                     ( char * ) ZH_MEM_PTR( pMemBlock ),
                     pMemBlock->nSize, pMemBlock->szProcName, pMemBlock->iProcLine,
                     zh_mem2str( membuffer, ( char * ) ZH_MEM_PTR( pMemBlock ),
                                 ZH_MIN( pMemBlock->nSize, ZH_MAX_MEM2STR_BLOCK ) ) );
         }
      }

      if( hLog )
      {
         fprintf( hLog, "------------------------------------------------------------------------\n" );
         fclose( hLog );
      }
   }

#if defined( ZH_FM_DL_ALLOC )
#  if defined( ZH_FM_DLMT_ALLOC )
      zh_mspace_cleanup();
#  elif defined( USE_DL_PREFIX )
      dlmalloc_destroy();
#  else
      malloc_trim( 0 );
#  endif
#endif
}

#else

void zh_xexit( void ) /* Deinitialize fixed memory subsystem */
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xexit()" ) );

#if defined( ZH_FM_DL_ALLOC )
#  if defined( ZH_FM_DLMT_ALLOC )
      zh_mspace_cleanup();
#  elif defined( USE_DL_PREFIX )
      dlmalloc_destroy();
#  else
      malloc_trim( 0 );
#  endif
#endif
}

#endif

ZH_SIZE zh_xquery( int iMode )
{
   ZH_SIZE nResult;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xquery(%d)", iMode ) );

   /* TODO: Return the correct values instead of 9999 [vszakats] */

   switch( iMode )
   {
      case ZH_MEM_CHAR:       /* (Free Variable Space [KB]) */
#if defined( ZH_OS_WIN ) && defined( ZH_OS_WIN_XP )
         {
            MEMORYSTATUSEX memorystatus;
            memset( &memorystatus, 0, sizeof( memorystatus ) );
            memorystatus.dwLength = sizeof( memorystatus );
            GlobalMemoryStatusEx( &memorystatus );
            nResult = ( ZH_SIZE ) ( memorystatus.ullAvailPhys / 1024 );
         }
#elif defined( ZH_OS_WIN )
         {
            MEMORYSTATUS memorystatus;
            GlobalMemoryStatus( &memorystatus );
            nResult = memorystatus.dwAvailPhys / 1024;
         }
#else
         nResult = 9999;
#endif
         break;

      case ZH_MEM_BLOCK:      /* (Largest String [KB]) */
#if defined( ZH_OS_WIN ) && defined( ZH_OS_WIN_XP )
         {
            MEMORYSTATUSEX memorystatus;
            memset( &memorystatus, 0, sizeof( memorystatus ) );
            memorystatus.dwLength = sizeof( memorystatus );
            GlobalMemoryStatusEx( &memorystatus );
            nResult = ( ZH_SIZE ) ( memorystatus.ullAvailPhys / 1024 );
         }
#elif defined( ZH_OS_WIN )
         {
            MEMORYSTATUS memorystatus;
            GlobalMemoryStatus( &memorystatus );
            nResult = ZH_MIN( memorystatus.dwAvailPhys, ULONG_MAX ) / 1024;
         }
#else
         nResult = 9999;
#endif
         break;

      case ZH_MEM_RUN:        /* (RUN Memory [KB]) */
#if defined( ZH_OS_WIN ) && defined( ZH_OS_WIN_XP )
         {
            MEMORYSTATUSEX memorystatus;
            memset( &memorystatus, 0, sizeof( memorystatus ) );
            memorystatus.dwLength = sizeof( memorystatus );
            GlobalMemoryStatusEx( &memorystatus );
            nResult = ( ZH_SIZE ) ( memorystatus.ullAvailPhys / 1024 );
         }
#elif defined( ZH_OS_WIN )
         {
            MEMORYSTATUS memorystatus;
            GlobalMemoryStatus( &memorystatus );
            nResult = memorystatus.dwAvailPhys / 1024;
         }
#else
         nResult = 9999;
#endif
         break;

      case ZH_MEM_VM:         /* UNDOCUMENTED! (Virtual Memory [KB]) */
#if defined( ZH_OS_WIN ) && defined( ZH_OS_WIN_XP )
         {
            MEMORYSTATUSEX memorystatus;
            memset( &memorystatus, 0, sizeof( memorystatus ) );
            memorystatus.dwLength = sizeof( memorystatus );
            GlobalMemoryStatusEx( &memorystatus );
            nResult = ( ZH_SIZE ) ( memorystatus.ullAvailVirtual / 1024 );
         }
#elif defined( ZH_OS_WIN )
         {
            MEMORYSTATUS memorystatus;
            GlobalMemoryStatus( &memorystatus );
            nResult = memorystatus.dwAvailVirtual / 1024;
         }
#else
         nResult = 9999;
#endif
         break;

      case ZH_MEM_EMS:        /* UNDOCUMENTED! (Free Expanded Memory [KB]) (?) */
#if defined( ZH_OS_WIN )
         nResult = 0;
#else
         nResult = 9999;
#endif
         break;

      case ZH_MEM_FM:         /* UNDOCUMENTED! (Fixed Memory/Heap [KB]) (?) */
#if defined( ZH_OS_WIN ) && defined( ZH_OS_WIN_XP )
         {
            MEMORYSTATUSEX memorystatus;
            memset( &memorystatus, 0, sizeof( memorystatus ) );
            memorystatus.dwLength = sizeof( memorystatus );
            GlobalMemoryStatusEx( &memorystatus );
            nResult = ( ZH_SIZE ) ( memorystatus.ullTotalPhys / 1024 );
         }
#elif defined( ZH_OS_WIN )
         {
            MEMORYSTATUS memorystatus;
            GlobalMemoryStatus( &memorystatus );
            nResult = memorystatus.dwTotalPhys / 1024;
         }
#else
         nResult = 9999;
#endif
         break;

      case ZH_MEM_FMSEGS:     /* (Segments in Fixed Memory/Heap) (?) */
#if defined( ZH_OS_WIN )
         nResult = 1;
#else
         nResult = 9999;
#endif
         break;

      case ZH_MEM_SWAP:       /* (Free Swap Memory [KB]) */
#if defined( ZH_OS_WIN ) && defined( ZH_OS_WIN_XP )
         {
            MEMORYSTATUSEX memorystatus;
            memset( &memorystatus, 0, sizeof( memorystatus ) );
            memorystatus.dwLength = sizeof( memorystatus );
            GlobalMemoryStatusEx( &memorystatus );
            nResult = ( ZH_SIZE ) ( memorystatus.ullAvailPageFile / 1024 );
         }
#elif defined( ZH_OS_WIN )
         {
            MEMORYSTATUS memorystatus;
            GlobalMemoryStatus( &memorystatus );
            nResult = memorystatus.dwAvailPageFile / 1024;
         }
         nResult = 9999;
         break;
#endif

      case ZH_MEM_CONV:       /* (Free Conventional [KB]) */
#if defined( ZH_OS_WIN )
         nResult = 0;
#else
         nResult = 9999;
#endif
         break;

      case ZH_MEM_EMSUSED:    /* (Used Expanded Memory [KB]) (?) */
         nResult = 0;
         break;

      case ZH_MEM_USED:       /* (Memory used [bytes]) */
#ifdef ZH_FM_STATISTICS
         nResult = s_nMemoryConsumed;
#elif defined( ZH_FM_DLMT_ALLOC )
         nResult = mspace_footprint( zh_mspace() );
#elif defined( ZH_FM_DL_ALLOC )
         nResult = dlmalloc_footprint();
#else
         nResult = 0;
#endif
         break;

      case ZH_MEM_BLOCKS:     /* (Memory blocks used) */
#ifdef ZH_FM_STATISTICS
         nResult = s_nMemoryBlocks;
#else
         nResult = 0;
#endif
         break;

      case ZH_MEM_USEDMAX:    /* (Maximum memory used [bytes]) */
#ifdef ZH_FM_STATISTICS
         nResult = s_nMemoryMaxConsumed;
#elif defined( ZH_FM_DLMT_ALLOC )
         nResult = mspace_max_footprint( zh_mspace() );
#elif defined( ZH_FM_DL_ALLOC )
         nResult = dlmalloc_max_footprint();
#else
         nResult = 0;
#endif
         break;

      case ZH_MEM_STACKITEMS: /* (Total items allocated for the stack) */
         nResult = zh_stackTotalItems();
         break;

      case ZH_MEM_STACK:      /* (Total memory size used by the stack [bytes]) */
         nResult = zh_stackTotalItems() * sizeof( ZH_ITEM );
         break;

      case ZH_MEM_STACK_TOP:  /* (Total items currently on the stack) */
      {
         
         nResult = zh_stackTopOffset();
         break;
      }
      case ZH_MEM_STATISTICS: /* (Is FM statistic enabled?) */
#ifdef ZH_FM_STATISTICS
         nResult = s_fStatistic;
#else
         nResult = 0;
#endif
         break;

      case ZH_MEM_CANLIMIT:   /* (Is used memory limit supported?) */
         if( zh_vmInternalsEnabled() )
         {
#if defined( ZH_FM_DLMT_ALLOC )
            nResult = 1;
#elif defined( ZH_FM_DL_ALLOC )
            nResult = 1;
#elif defined( ZH_FM_STATISTICS )
            nResult = s_fStatistic;
#else
            nResult = 0;
#endif
         }
         else
            nResult = 0;
         break;

      default:
         nResult = 0;
   }

   return nResult;
}

ZH_BOOL zh_xtraced( void )
{
#if ZH_TR_LEVEL >= ZH_TR_DEBUG
   return ZH_TRUE;
#else
   return ZH_FALSE;
#endif
}

ZH_FUNC( __FM_ALLOCLIMIT )
{
   ;
   zh_xclean();

   if( zh_vmInternalsEnabled() )
   {
#if defined( ZH_FM_DLMT_ALLOC )
      zh_retns( mspace_footprint_limit( zh_mspace() ) );
      if( ZH_IS_PARAM_NUM( 1 ) )
      {
         ZH_I_SIZE nLimit = zh_parns( 1 );

         if( nLimit <= 0 )
            nLimit = -1;
         mspace_set_footprint_limit( zh_mspace(), nLimit );
      }
#elif defined( ZH_FM_DL_ALLOC )
      zh_retns( dlmalloc_footprint_limit() );
      if( ZH_IS_PARAM_NUM( 1 ) )
      {
         ZH_I_SIZE nLimit = zh_parns( 1 );

         if( nLimit <= 0 )
            nLimit = -1;
         dlmalloc_set_footprint_limit( ( size_t ) nLimit );
      }
#elif defined( ZH_FM_STATISTICS )
      zh_retns( s_nMemoryLimConsumed ? s_nMemoryLimConsumed : -1 );
      if( ZH_IS_PARAM_NUM( 1 ) )
      {
         ZH_I_SIZE nLimit = zh_parns( 1 );

         s_nMemoryLimConsumed = ZH_MAX( nLimit, 0 );
      }
#else
      zh_retni( 0 );
#endif
   }
   else
      zh_retni( 0 );
}
