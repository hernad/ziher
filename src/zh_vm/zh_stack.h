/*
 * The eval stack
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

/* FIXME: There are several things in this file which are not part of the
          standard Ziher API, in other words these things are not
          guaranteed to remain unchanged. To avoid confusion these should be
          moved to somewhere else (like zhrtl.h). [vszakats] */

#ifndef ZH_STACK_H_
#define ZH_STACK_H_

#include "zh_vm_pub.h"

ZH_EXTERN_BEGIN

#if defined( _ZH_API_INTERNAL_ )
#  include "zh_thread.h"
#endif

/* thread specific data */
typedef void (*PZH_TSD_FUNC)(void *);
typedef struct
{
   int          iHandle;
   int          iSize;
   PZH_TSD_FUNC pInitFunc;
   PZH_TSD_FUNC pCleanFunc;
}
ZH_TSD, * PZH_TSD;
#define ZH_TSD_NEW(name,size,init,clean)  \
        ZH_TSD name = { 0, size, init, clean }
#define ZH_TSD_INIT(name,size,init,clean) do { \
            (name)->iHandle = 0; \
            (name)->iSize = (size); \
            (name)->pInitFunc = (init); \
            (name)->pCleanFunc = (clean); \
         } while( 0 )

typedef struct
{
   ZH_ERRCODE uiFError;
   ZH_ERRCODE uiErrorLast;
   ZH_ERRCODE uiOsErrorLast;
   ZH_ERRCODE uiSocketError;
   int        iSocketOsError;
}
ZH_IOERRORS, * PZH_IOERRORS;

typedef struct
{
   const char *   szDefaultRDD;     /* default RDD */
   ZH_BOOL        fNetError;        /* current NetErr() flag */

   void **        waList;           /* Allocated WorkAreas */
   ZH_USHORT      uiWaMax;          /* Number of allocated WA */
   ZH_USHORT      uiWaSpace;        /* Number of allocated WA */

   ZH_USHORT *    waNums;           /* Allocated WorkAreas */
   ZH_USHORT      uiWaNumMax;       /* Number of allocated WA */

   ZH_USHORT      uiCurrArea;       /* Current WokrArea number */
   void *         pCurrArea;        /* Current WorkArea pointer */
}
ZH_STACKRDD, * PZH_STACKRDD;

#ifdef _ZH_API_INTERNAL_

#include "zh_set.h"

typedef struct
{
   PZH_TSD  pTSD;
   void *   value;
}
ZH_TSD_HOLDER, * PZH_TSD_HOLDER;

typedef struct
{
   PZH_DYNSYMBOL    pDynSym;
   PZH_ITEM    pPrevMemvar;
}
ZH_PRIVATE_ITEM, * PZH_PRIVATE_ITEM;

typedef struct
{
   PZH_PRIVATE_ITEM stack;
   ZH_SIZE          size;
   ZH_SIZE          count;
   ZH_SIZE          base;
}
ZH_PRIVATE_STACK, * PZH_PRIVATE_STACK;

typedef struct
{
   void *     pMemvar;        /* memvar pointer ( publics & privates ) */
   ZH_USHORT  uiArea;         /* Workarea number */
}
ZH_DYN_HANDLES, * PZH_DYN_HANDLES;

/* stack managed by the virtual machine */
typedef struct
{
   PZH_ITEM * pPos;           /* pointer to the latest used item */
   PZH_ITEM * pEnd;           /* pointer to the end of stack items */
   PZH_ITEM * pItems;         /* pointer to the stack items */
   PZH_ITEM * pBase;          /* stack frame position for the current function call */
   ZH_ITEM    Return;         /* latest returned value */
   ZH_I_SIZE    nItems;         /* total items that may be held on the stack */
   ZH_I_SIZE    nWithObject;    /* stack offset to base current WITH OBJECT item */
   ZH_I_SIZE    nRecoverBase;   /* current SEQUENCE envelope offset or 0 if no SEQUENCE is active */
   ZH_USHORT  uiActionRequest;/* request for some action - stop processing of opcodes */
   ZH_USHORT  uiQuitState;    /* ZHVM is quitting */
   ZH_STACK_STATE state;      /* first (default) stack state frame */
   ZH_STACKRDD rdd;           /* RDD related data */
   char       szDate[ 9 ];    /* last returned date from zh_pards() YYYYMMDD format */
   void *     pCDP;           /* current codepage module */
   void *     pLang;          /* current language module */
   void *     pI18N;          /* current I18N module */
   void *     hGT;            /* current GT module */
   int        iTSD;           /* number of allocated TSD holders */
   PZH_TSD_HOLDER pTSD;       /* thread specific data holder */
   void *     pStatics;       /* statics base for the current function call */
   ZH_PRIVATE_STACK privates; /* private variables stack */
   ZH_SET_STRUCT set;
   int        iKeyPoll;       /* counter for GT/keyboard polling */
   ZH_BOOL    fDebugRequest;  /* request debugger activation */
   void *     pDebugInfo;     /* internal debugger structure */
   int        iUnlocked;      /* counter for nested zh_vmUnlock() calls */
   PZH_DYN_HANDLES pDynH;     /* dynamic symbol handles */
   int        iDynH;          /* number of dynamic symbol handles */
   void *     pStackLst;      /* this stack entry in stack linked list */
   ZH_IOERRORS IOErrors;      /* MT safe buffer for IO errors */
   ZH_TRACEINFO traceInfo;    /* MT safe buffer for ZH_TRACE data */
   char *     pDirBuffer;     /* MT safe buffer for zh_fsCurDir() results */
   void *     allocator;      /* memory manager global struct pointer */
} ZH_STACK, * PZH_STACK;

#if defined( _ZH_STACK_MACROS_ )

#     if defined( ZH_USE_TLS )
#        if ! defined( _ZH_STACK_LOCAL_MACROS_ )
               extern ZH_TLS_ATTR PZH_STACK zh_stack_ptr;
#        endif
#     else
         #error "TLS support undefined for this compiler" 
#     endif
#     if defined( ZH_STACK_PRELOAD ) && ! defined( ZH_USE_TLS )
         #error "TLS support undefined for this compiler" 
/*
#        if defined( zh_stack_ptr_get )
#           define ZH_STACK_TLS_RELOAD    _zh_stack_ptr_ = ( PZH_STACK ) zh_stack_ptr_get();
#           undef zh_stack_ptr
#        else
#           define ZH_STACK_TLS_RELOAD    _zh_stack_ptr_ = zh_stack_ptr;
#        endif
#        define ZH_STACK_TLS_PRELOAD   PZH_STACK ZH_STACK_TLS_RELOAD
#        define zh_stack            ( * _zh_stack_ptr_ )
#        define zh_stack_ref()      ( _zh_stack_ptr_ )
*/
#     else
#        define zh_stack            ( * zh_stack_ptr )
#        define zh_stack_ref()      ( zh_stack_ptr )
#     endif

#endif
#if ! defined( ZH_STACK_TLS_PRELOAD )
#  if defined( ZH_STACK_PRELOAD )
#     define ZH_STACK_TLS_PRELOAD
#     define ZH_STACK_TLS_RELOAD
#     undef  ZH_STACK_PRELOAD
#  elif defined( _ZH_STACK_MACROS_ )
#     define ZH_STACK_TLS_PRELOAD
#  endif
#endif

#endif /* _ZH_API_INTERNAL_ */

extern ZH_EXPORT void *      zh_stackId( void );
extern ZH_EXPORT PZH_ITEM    zh_stackItemFromTop( int nFromTop );
extern ZH_EXPORT PZH_ITEM    zh_stackItemFromBase( int nFromBase );
extern ZH_EXPORT PZH_ITEM    zh_stackBaseItem( void );
extern ZH_EXPORT PZH_ITEM    zh_stackSelfItem( void );   /* returns Self object at C function level */
extern ZH_EXPORT PZH_ITEM    zh_stackReturnItem( void ); /* returns RETURN Item from stack */

extern ZH_EXPORT PZH_ITEM    zh_stackAllocItem( void );  /* allocates new item on the top of stack, returns pointer to it */
extern ZH_EXPORT void        zh_stackPop( void );        /* pops an item from the stack */
extern ZH_EXPORT void        zh_stackPush( void );       /* pushes an item on to the stack */

extern           void        zh_stackPushReturn( void );
extern           void        zh_stackPopReturn( void );

extern ZH_EXPORT ZH_I_SIZE     zh_stackTopOffset( void );
extern ZH_EXPORT ZH_I_SIZE     zh_stackBaseOffset( void );
extern ZH_EXPORT ZH_I_SIZE     zh_stackTotalItems( void );
extern ZH_EXPORT PZH_ITEM    zh_stackItem( ZH_I_SIZE nItemPos );

/* stack management functions */
extern ZH_EXPORT int         zh_stackCallDepth( void );
extern ZH_EXPORT void        zh_stackBaseProcInfo( char * szProcName, ZH_USHORT * puiProcLine ); /* get current .zh function name and line number */

extern ZH_EXPORT ZH_I_SIZE     zh_stackBaseProcOffset( int iLevel );
extern ZH_EXPORT ZH_I_SIZE     zh_stackBaseSymbolOffset( PZH_SYMBOL pSymbol );
extern           void        zh_stackDispCall( void );

/* thread specific data */
extern ZH_EXPORT void * zh_stackGetTSD( PZH_TSD pTSD );
extern ZH_EXPORT void * zh_stackTestTSD( PZH_TSD pTSD );
extern ZH_EXPORT void   zh_stackReleaseTSD( PZH_TSD pTSD );

extern char *       zh_stackDateBuffer( void );
extern char *       zh_stackDirBuffer( void );
extern PZH_IOERRORS zh_stackIOErrors( void );
extern void *       zh_stackGetGT( void );
extern void         zh_stackSetGT( void * );
extern PZH_STACKRDD zh_stackRDD( void );

extern ZH_EXPORT void ** zh_stackDebugInfo( void );

#ifdef _ZH_API_INTERNAL_
extern void        zh_stackFree( void );       /* releases all memory used by the stack */
extern void        zh_stackInit( void );       /* initializes the stack */
extern void        zh_stackIncrease( void );   /* increase the stack size */
extern void        zh_stackDec( void );
extern void        zh_stackDecrease( ZH_SIZE nItems );
extern void        zh_stackRemove( ZH_I_SIZE nUntilPos );
extern PZH_ITEM    zh_stackNewFrame( PZH_STACK_STATE pFrame, ZH_USHORT uiParams );
extern void        zh_stackOldFrame( PZH_STACK_STATE pFrame );
extern void        zh_stackClearMemvarsBase( void );

extern PZH_ITEM    zh_stackLocalVariable( int iLocal );
extern PZH_ITEM    zh_stackLocalVariableAt( int * piFromBase );
extern PZH_ITEM ** zh_stackItemBasePtr( void );

extern ZH_EXPORT ZH_I_SIZE     zh_stackGetRecoverBase( void );
extern           void        zh_stackSetRecoverBase( ZH_I_SIZE nBase );
extern           ZH_USHORT   zh_stackGetActionRequest( void );
extern           void        zh_stackSetActionRequest( ZH_USHORT uiAction );

extern void        zh_stackSetStaticsBase( void * pBase );
extern void *      zh_stackGetStaticsBase( void );

extern           PZH_ITEM    zh_stackWithObjectItem( void );
extern ZH_EXPORT ZH_I_SIZE     zh_stackWithObjectOffset( void );
extern           void        zh_stackWithObjectSetOffset( ZH_I_SIZE nOffset );

extern int *       zh_stackKeyPolls( void );
extern ZH_BOOL *   zh_stackDebugRequest( void );

extern void        zh_stackDestroyTSD( void );

extern PZH_PRIVATE_STACK zh_stackGetPrivateStack( void );
extern void *      zh_stackGetCodepage( void );
extern void        zh_stackSetCDP( void * );
extern void *      zh_stackGetLang( void );
extern void        zh_stackSetLang( void * );
extern void *      zh_stackGetI18N( void );
extern void        zh_stackSetI18N( void * );

extern void        zh_stackIsStackRef( void *, PZH_TSD_FUNC );
extern void        zh_stackUpdateAllocator( void *, PZH_ALLOCUPDT_FUNC, int );

   extern void *           zh_stackList( void );
   extern void             zh_stackListSet( void * pStackLst );
   extern void             zh_stackIdSetActionRequest( void * pStackID, ZH_USHORT uiAction );
   extern PZH_DYN_HANDLES  zh_stackGetDynHandle( PZH_DYNSYMBOL pDynSym );
   extern int              zh_stackDynHandlesCount( void );
   extern void             zh_stackClearMemvars( int );
   extern ZH_BOOL          zh_stackQuitState( void );
   extern void             zh_stackSetQuitState( ZH_USHORT uiState );
   extern int              zh_stackUnlock( void );
   extern int              zh_stackLock( void );
   extern int              zh_stackLockCount( void );
   extern void *           zh_stackAllocator( void );

#endif /* _ZH_API_INTERNAL_ */

#if defined( _ZH_API_INTERNAL_ ) || defined( _ZH_SET_INTERNAL_ )
   extern PZH_SET_STRUCT zh_stackSetStruct( void );
#endif


#if defined( _ZH_STACK_MACROS_ )

#define zh_stackItemFromTop( n )    ( * ( zh_stack.pPos + ( int ) ( n ) ) )
#define zh_stackItemFromBase( n )   ( * ( zh_stack.pBase + ( int ) ( n ) + 1 ) )
#define zh_stackTopOffset( )        ( zh_stack.pPos - zh_stack.pItems )
#define zh_stackBaseOffset( )       ( zh_stack.pBase - zh_stack.pItems + 1 )
/* #define zh_stackTotalItems( )       ( zh_stack.nItems ) */
#define zh_stackBaseItem( )         ( * zh_stack.pBase )
#define zh_stackSelfItem( )         ( * ( zh_stack.pBase + 1 ) )
#define zh_stackItem( iItemPos )    ( * ( zh_stack.pItems + ( ZH_I_SIZE ) ( iItemPos ) ) )
#define zh_stackReturnItem( )       ( &zh_stack.Return )
#define zh_stackDateBuffer( )       ( zh_stack.szDate )
#define zh_stackItemBasePtr( )      ( &zh_stack.pItems )
#define zh_stackGetStaticsBase( )   ( zh_stack.pStatics )
#define zh_stackSetStaticsBase( p ) do { zh_stack.pStatics = ( p ); } while( 0 )
#define zh_stackGetRecoverBase( )   ( zh_stack.nRecoverBase )
#define zh_stackSetRecoverBase( n ) do { zh_stack.nRecoverBase = ( n ); } while( 0 )
#define zh_stackGetActionRequest( ) ( zh_stack.uiActionRequest )
#define zh_stackSetActionRequest( n )     do { zh_stack.uiActionRequest = ( n ); } while( 0 )
#define zh_stackWithObjectItem( )   ( zh_stack.nWithObject ? * ( zh_stack.pItems + zh_stack.nWithObject ) : NULL )
#define zh_stackWithObjectOffset( ) ( zh_stack.nWithObject )
#define zh_stackWithObjectSetOffset( n )  do { zh_stack.nWithObject = ( n ); } while( 0 )
#define zh_stackGetCodepage( )           ( zh_stack.pCDP )
#define zh_stackSetCDP( p )         do { zh_stack.pCDP = ( p ); } while( 0 )
#define zh_stackGetLang( )          ( zh_stack.pLang )
#define zh_stackSetLang( p )        do { zh_stack.pLang = ( p ); } while( 0 )
#define zh_stackGetI18N( )          ( zh_stack.pI18N )
#define zh_stackSetI18N( p )        do { zh_stack.pI18N = ( p ); } while( 0 )

#define zh_stackId( )               ( ( void * ) zh_stack_ref() )
#  define zh_stackList()            ( zh_stack.pStackLst )
#  define zh_stackListSet( p )      do { zh_stack.pStackLst = ( p ); } while( 0 )
#  define zh_stackDynHandlesCount() ( zh_stack.iDynH )
#  define zh_stackQuitState( )      ( zh_stack.uiQuitState != 0 )
#  define zh_stackSetQuitState( n ) do { zh_stack.uiQuitState = ( n ); } while( 0 )
#  define zh_stackUnlock()          ( ++zh_stack.iUnlocked )
#  define zh_stackLock()            ( --zh_stack.iUnlocked )
#  define zh_stackLockCount()       ( zh_stack.iUnlocked )

#define zh_stackAllocItem( )        ( ( ++zh_stack.pPos == zh_stack.pEnd ? \
                                        zh_stackIncrease() : ( void ) 0 ), \
                                      * ( zh_stack.pPos - 1 ) )

#ifdef ZH_STACK_SAFEMACROS

#define zh_stackDecrease( n )       do { \
                                       if( ( zh_stack.pPos -= (n) ) <= zh_stack.pBase ) \
                                          zh_errInternal( ZH_EI_STACKUFLOW, NULL, NULL, NULL ); \
                                    } while( 0 )

#define zh_stackDec( )              do { \
                                       if( --zh_stack.pPos <= zh_stack.pBase ) \
                                          zh_errInternal( ZH_EI_STACKUFLOW, NULL, NULL, NULL ); \
                                    } while( 0 )

#define zh_stackPop( )              do { \
                                       if( --zh_stack.pPos <= zh_stack.pBase ) \
                                          zh_errInternal( ZH_EI_STACKUFLOW, NULL, NULL, NULL ); \
                                       if( ZH_IS_COMPLEX( * zh_stack.pPos ) ) \
                                          zh_itemClear( * zh_stack.pPos ); \
                                    } while( 0 )

#define zh_stackPopReturn( )        do { \
                                       if( ZH_IS_COMPLEX( &zh_stack.Return ) ) \
                                          zh_itemClear( &zh_stack.Return ); \
                                       if( --zh_stack.pPos <= zh_stack.pBase ) \
                                          zh_errInternal( ZH_EI_STACKUFLOW, NULL, NULL, NULL ); \
                                       zh_itemRawMove( &zh_stack.Return, * zh_stack.pPos ); \
                                    } while( 0 )

#else

#define zh_stackDecrease( n )       do { zh_stack.pPos -= (n); } while( 0 )
#define zh_stackDec( )              do { --zh_stack.pPos; } while( 0 )
#define zh_stackPop( )              do { --zh_stack.pPos; \
                                       if( ZH_IS_COMPLEX( * zh_stack.pPos ) ) \
                                          zh_itemClear( * zh_stack.pPos ); \
                                    } while( 0 )
#define zh_stackPopReturn( )        do { \
                                       if( ZH_IS_COMPLEX( &zh_stack.Return ) ) \
                                          zh_itemClear( &zh_stack.Return ); \
                                       --zh_stack.pPos; \
                                       zh_itemRawMove( &zh_stack.Return, * zh_stack.pPos ); \
                                    } while( 0 )

#endif /* ZH_STACK_SAFEMACROS */

#define zh_stackPush( )             do { \
                                       if( ++zh_stack.pPos == zh_stack.pEnd ) \
                                          zh_stackIncrease(); \
                                    } while( 0 )

#define zh_stackPushReturn( )       do { \
                                       zh_itemRawMove( * zh_stack.pPos, &zh_stack.Return ); \
                                       if( ++zh_stack.pPos == zh_stack.pEnd ) \
                                          zh_stackIncrease(); \
                                    } while( 0 )

#define zh_stackLocalVariable( i )  ( zh_stack.pBase[ ( i ) + 1 + \
                                       ( ( ( *zh_stack.pBase )->item.asSymbol.paramcnt > \
                                           ( *zh_stack.pBase )->item.asSymbol.paramdeclcnt && \
                                           ( i ) > ( * zh_stack.pBase )->item.asSymbol.paramdeclcnt ) ? \
                                            ( * zh_stack.pBase )->item.asSymbol.paramcnt - \
                                            ( * zh_stack.pBase )->item.asSymbol.paramdeclcnt : 0 ) ] )

#define zh_stackLocalVariableAt( p )( ( ( ( *zh_stack.pBase )->item.asSymbol.paramcnt > \
                                          ( *zh_stack.pBase )->item.asSymbol.paramdeclcnt ) && \
                                        ( * (p) ) > ( * zh_stack.pBase )->item.asSymbol.paramdeclcnt ) ? \
                                      ( * ( zh_stack.pBase + ( int ) ( * (p) += \
                                          ( * zh_stack.pBase )->item.asSymbol.paramcnt - \
                                          ( * zh_stack.pBase )->item.asSymbol.paramdeclcnt ) + 1 ) ) : \
                                      ( * ( zh_stack.pBase + ( int ) ( * (p) ) + 1 ) ) )

#define zh_stackGetPrivateStack( )  ( &zh_stack.privates )
#define zh_stackSetStruct( )        ( &zh_stack.set )
#define zh_stackKeyPolls( )         ( &zh_stack.iKeyPoll )
#define zh_stackDebugRequest( )     ( &zh_stack.fDebugRequest )
#define zh_stackDebugInfo( )        ( &zh_stack.pDebugInfo )

#endif


ZH_EXTERN_END

#endif /* ZH_STACK_H_ */
