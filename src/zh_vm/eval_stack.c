/*
 * The eval stack management functions
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

#define ZH_STACK_PRELOAD

#include "zh_vm_opt.h"
#include "zh_api.h"
#include "zh_class_api.h"
#include "zh_stack.h"
#include "zh_item_api.h"
#include "zh_error_api.h"
#include "zh_rdd_api.h"
#include "zh_date.h"


#if ! defined( STACK_INITZH_ITEMS )
   #define STACK_INITZH_ITEMS    200
#endif
#if ! defined( STACK_EXPANDZH_ITEMS )
   #define STACK_EXPANDZH_ITEMS  20
#endif


#  include "zh_thread.h"

   static ZH_CRITICAL_NEW( TSD_counter );
   static int s_iTSDCounter = 0;

#  ifdef ZH_USE_TLS
      // https://en.wikipedia.org/wiki/Thread-local_storage
      /* compiler has native support for TLS */
#     if ! defined( _ZH_STACK_MACROS_ )
            static ZH_TLS_ATTR PZH_STACK zh_stack_ptr;
#     elif ! defined( _ZH_STACK_LOCAL_MACROS_ )
            ZH_TLS_ATTR PZH_STACK zh_stack_ptr = NULL;
#     endif

#     define zh_stack_alloc()    do { zh_stack_ptr = ( PZH_STACK ) \
                                      zh_xgrab( sizeof( ZH_STACK ) ); } while( 0 )
#     define zh_stack_dealloc()  do { zh_xfree( zh_stack_ptr ); \
                                      zh_stack_ptr = NULL; } while( 0 )
#     define zh_stack_ready()    (zh_stack_ptr != NULL)

#  else
      #error "TLS support undefined for this compiler"
#  endif /* ZH_USE_TLS */

#  if ! defined( ZH_STACK_PRELOAD )
#     undef zh_stack
#     define zh_stack   ( * zh_stack_ptr )
#  endif



static char s_szDirBuffer[ ZH_PATH_MAX ];
static ZH_IOERRORS s_IOErrors;
static ZH_TRACEINFO s_traceInfo;

static ZH_SYMBOL s_initSymbol = { "zh_stackInit", { ZH_FS_STATIC }, { NULL }, NULL };


static void zh_stack_init( PZH_STACK pStack )
{
   ZH_ISIZ n;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stack_init(%p)", ( void * ) pStack ) );

   memset( pStack, 0, sizeof( ZH_STACK ) );

   pStack->pItems = ( PZH_ITEM * ) zh_xgrab( sizeof( PZH_ITEM ) * STACK_INITZH_ITEMS );
   pStack->pBase  = pStack->pItems;
   pStack->pPos   = pStack->pItems;       /* points to the first stack item */
   pStack->nItems = STACK_INITZH_ITEMS;
   pStack->pEnd   = pStack->pItems + pStack->nItems;

   for( n = 0; n < pStack->nItems; ++n )
   {
      pStack->pItems[ n ] = ( PZH_ITEM ) zh_xgrab( sizeof( ZH_ITEM ) );
      pStack->pItems[ n ]->type = ZH_IT_NIL;
   }

   pStack->pPos++;
   zh_itemPutSymbol( *pStack->pItems, &s_initSymbol );
   ( *pStack->pItems )->item.asSymbol.stackstate = &pStack->state;

   pStack->rdd.uiCurrArea = 1;
   pStack->iKeyPoll = 1;
}

static void zh_stack_destroy_TSD( PZH_STACK pStack )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stack_destroy_TSD(%p)", ( void * ) pStack ) );

   while( pStack->iTSD )
   {
      if( pStack->pTSD[ pStack->iTSD ].pTSD )
      {
         if( pStack->pTSD[ pStack->iTSD ].pTSD->pCleanFunc )
            pStack->pTSD[ pStack->iTSD ].pTSD->pCleanFunc(
               pStack->pTSD[ pStack->iTSD ].value );
         zh_xfree( pStack->pTSD[ pStack->iTSD ].value );
      }
      if( --pStack->iTSD == 0 )
      {
         zh_xfree( pStack->pTSD );
         pStack->pTSD = NULL;
      }
   }
}

static void zh_stack_free( PZH_STACK pStack )
{
   ZH_ISIZ n;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stack_free(%p)", ( void * ) pStack ) );

   zh_stack_destroy_TSD( pStack );

   if( pStack->privates.stack )
   {
      zh_xfree( pStack->privates.stack );
      pStack->privates.stack = NULL;
      pStack->privates.size = pStack->privates.count =
      pStack->privates.base = 0;
   }
   n = pStack->nItems - 1;
   while( n >= 0 )
      zh_xfree( pStack->pItems[ n-- ] );
   zh_xfree( pStack->pItems );
   pStack->pItems = pStack->pPos = pStack->pBase = NULL;
   pStack->nItems = 0;
   if( pStack->pDirBuffer )
   {
      zh_xfree( pStack->pDirBuffer );
      pStack->pDirBuffer = NULL;
   }
   if( pStack->iDynH )
   {
      zh_xfree( pStack->pDynH );
      pStack->pDynH = NULL;
      pStack->iDynH = 0;
   }
}

void zh_stackDestroyTSD( void )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stackDestroyTSD()" ) );

   zh_stack_destroy_TSD( &zh_stack );
}

void * zh_stackGetTSD( PZH_TSD pTSD )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stackGetTSD(%p)", ( void * ) pTSD ) );

   if( pTSD->iHandle == 0 || pTSD->iHandle > zh_stack.iTSD ||
       zh_stack.pTSD[ pTSD->iHandle ].pTSD == NULL )
   {
      if( pTSD->iHandle == 0 )
      {
         zh_threadEnterCriticalSection( &TSD_counter );
         /* repeated test protected by mutex to avoid race condition */
         if( pTSD->iHandle == 0 )
            pTSD->iHandle = ++s_iTSDCounter;
         zh_threadLeaveCriticalSection( &TSD_counter );
      }

      if( pTSD->iHandle > zh_stack.iTSD )
      {
         zh_stack.pTSD = ( PZH_TSD_HOLDER )
                         zh_xrealloc( zh_stack.pTSD, ( pTSD->iHandle + 1 ) *
                                                     sizeof( ZH_TSD_HOLDER ) );
         memset( &zh_stack.pTSD[ zh_stack.iTSD + 1 ], 0,
                 ( pTSD->iHandle - zh_stack.iTSD ) * sizeof( ZH_TSD_HOLDER ) );
         zh_stack.iTSD = pTSD->iHandle;
      }
      zh_stack.pTSD[ pTSD->iHandle ].pTSD  = pTSD;
      zh_stack.pTSD[ pTSD->iHandle ].value = zh_xgrabz( pTSD->iSize );
      if( pTSD->pInitFunc )
         pTSD->pInitFunc( zh_stack.pTSD[ pTSD->iHandle ].value );
   }
   return zh_stack.pTSD[ pTSD->iHandle ].value;
}

void * zh_stackTestTSD( PZH_TSD pTSD )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stackTestTSD(%p)", ( void * ) pTSD ) );

   return ( pTSD->iHandle && pTSD->iHandle <= zh_stack.iTSD ) ?
                          zh_stack.pTSD[ pTSD->iHandle ].value : NULL;
}

void zh_stackReleaseTSD( PZH_TSD pTSD )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stackReleaseTSD(%p)", ( void * ) pTSD ) );

   if( pTSD->iHandle && pTSD->iHandle <= zh_stack.iTSD &&
       zh_stack.pTSD[ pTSD->iHandle ].value )
   {
      if( pTSD->pCleanFunc )
         pTSD->pCleanFunc( zh_stack.pTSD[ pTSD->iHandle ].value );
      zh_xfree( zh_stack.pTSD[ pTSD->iHandle ].value );
      zh_stack.pTSD[ pTSD->iHandle ].value = NULL;
      zh_stack.pTSD[ pTSD->iHandle ].pTSD  = NULL;
      pTSD->iHandle = 0;
      /* TODO: add recovery system to not lose TSD handles and
       *       make this functionality more general and public
       *       for 3rd party developers
       */
   }
}

void zh_stackInit( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stackInit()" ) );

   zh_stack_alloc();
   {
      ZH_STACK_TLS_PRELOAD
      zh_stack_init( &zh_stack );
      zh_xinit_thread();
   }
}

void zh_stackFree( void )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stackFree()" ) );

   zh_stack_free( &zh_stack );
   zh_xexit_thread();
   zh_stack_dealloc();
}

#undef zh_stackList
void * zh_stackList( void )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stackList()" ) );

   return zh_stack.pStackLst;
}

#undef zh_stackListSet
void zh_stackListSet( void * pStackLst )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stackListSet(%p)", pStackLst ) );

   zh_stack.pStackLst = pStackLst;
}

#undef zh_stackIdSetActionRequest
void zh_stackIdSetActionRequest( void * pStackId, ZH_USHORT uiAction )
{
   ( ( PZH_STACK ) pStackId )->uiActionRequest = uiAction;
}

#undef zh_stackDynHandlesCount
int zh_stackDynHandlesCount( void )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stackDynHandlesCount()" ) );

   return zh_stack.iDynH;
}

PZH_DYN_HANDLES zh_stackGetDynHandle( PZH_DYNSYMBOL pDynSym )
{
   ZH_STACK_TLS_PRELOAD
   int iDynSym;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stackGetDynHandle()" ) );

   iDynSym = pDynSym->uiSymNum;
   if( iDynSym > zh_stack.iDynH )
   {
      zh_stack.pDynH = ( PZH_DYN_HANDLES ) zh_xrealloc( zh_stack.pDynH,
                                          iDynSym * sizeof( ZH_DYN_HANDLES ) );
      memset( &zh_stack.pDynH[ zh_stack.iDynH ], 0,
              ( iDynSym - zh_stack.iDynH ) * sizeof( ZH_DYN_HANDLES ) );
      zh_stack.iDynH = iDynSym;
   }

   return &zh_stack.pDynH[ iDynSym - 1 ];
}

void zh_stackClearMemvars( int iExcept )
{
   ZH_STACK_TLS_PRELOAD
   int iDynSym;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stackClearMemvars(%d)", iExcept ) );

   iDynSym = zh_stack.iDynH;
   while( --iDynSym >= 0 )
   {
      if( zh_stack.pDynH[ iDynSym ].pMemvar && iDynSym != iExcept )
      {
         PZH_ITEM pMemvar = ( PZH_ITEM ) zh_stack.pDynH[ iDynSym ].pMemvar;
         zh_stack.pDynH[ iDynSym ].pMemvar = NULL;
         zh_memvarValueDecRef( pMemvar );
      }
   }
}

#undef zh_stackQuitState
ZH_BOOL zh_stackQuitState( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stack.uiQuitState != 0;
}

#undef zh_stackSetQuitState
void zh_stackSetQuitState( ZH_USHORT uiState )
{
   ZH_STACK_TLS_PRELOAD
   zh_stack.uiQuitState = uiState;
}

#undef zh_stackUnlock
int zh_stackUnlock( void )
{
   ZH_STACK_TLS_PRELOAD
   return ++zh_stack.iUnlocked;
}

#undef zh_stackLock
int zh_stackLock( void )
{
   ZH_STACK_TLS_PRELOAD
   return --zh_stack.iUnlocked;
}

#undef zh_stackLockCount
int zh_stackLockCount( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stack.iUnlocked;
}

#undef zh_stackKeyPolls
int * zh_stackKeyPolls( void )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stackKeyPolls()" ) );

   return &zh_stack.iKeyPoll;
}

#undef zh_stackDebugRequest
ZH_BOOL * zh_stackDebugRequest( void )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stackDebugRequest()" ) );

   return &zh_stack.fDebugRequest;
}

#undef zh_stackDebugInfo
void ** zh_stackDebugInfo( void )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stackDebugInfo()" ) );

   return &zh_stack.pDebugInfo;
}

#undef zh_stackGetPrivateStack
PZH_PRIVATE_STACK zh_stackGetPrivateStack( void )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stackGetPrivateStack()" ) );

   return &zh_stack.privates;
}

#undef zh_stackSetStruct
PZH_SET_STRUCT zh_stackSetStruct( void )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stackSetStruct()" ) );

   return &zh_stack.set;
}

#undef zh_stackId
void * zh_stackId( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stackId()" ) );

   if( zh_stack_ready() )
   {
      ZH_STACK_TLS_PRELOAD
      return ( void * ) &zh_stack;
   }
   else
      return NULL;
}

#undef zh_stackPop
void zh_stackPop( void )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stackPop()" ) );

   if( --zh_stack.pPos <= zh_stack.pBase )
      zh_errInternal( ZH_EI_STACKUFLOW, NULL, NULL, NULL );

   if( ZH_IS_COMPLEX( *zh_stack.pPos ) )
      zh_itemClear( *zh_stack.pPos );
}

#undef zh_stackPopReturn
void zh_stackPopReturn( void )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stackPopReturn()" ) );

   if( ZH_IS_COMPLEX( &zh_stack.Return ) )
      zh_itemClear( &zh_stack.Return );

   if( --zh_stack.pPos <= zh_stack.pBase )
      zh_errInternal( ZH_EI_STACKUFLOW, NULL, NULL, NULL );

   zh_itemRawMove( &zh_stack.Return, *zh_stack.pPos );
}

#undef zh_stackDec
void zh_stackDec( void )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stackDec()" ) );

   if( --zh_stack.pPos <= zh_stack.pBase )
      zh_errInternal( ZH_EI_STACKUFLOW, NULL, NULL, NULL );
}

#undef zh_stackDecrease
void zh_stackDecrease( ZH_SIZE nItems )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stackDecrease()" ) );

   if( ( zh_stack.pPos -= nItems ) <= zh_stack.pBase )
      zh_errInternal( ZH_EI_STACKUFLOW, NULL, NULL, NULL );
}

#undef zh_stackPush
void zh_stackPush( void )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stackPush()" ) );

   /* enough room for another item ? */
   if( ++zh_stack.pPos == zh_stack.pEnd )
      zh_stackIncrease();
}

#undef zh_stackAllocItem
PZH_ITEM zh_stackAllocItem( void )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stackAllocItem()" ) );

   if( ++zh_stack.pPos == zh_stack.pEnd )
      zh_stackIncrease();

   ( *( zh_stack.pPos - 1 ) )->type = ZH_IT_NIL;

   return *( zh_stack.pPos - 1 );
}

#undef zh_stackPushReturn
void zh_stackPushReturn( void )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stackPushReturn()" ) );

   zh_itemRawMove( *zh_stack.pPos, &zh_stack.Return );

   /* enough room for another item ? */
   if( ++zh_stack.pPos == zh_stack.pEnd )
      zh_stackIncrease();
}

void zh_stackIncrease( void )
{
   ZH_STACK_TLS_PRELOAD
   ZH_ISIZ nBaseIndex;   /* index of stack base */
   ZH_ISIZ nCurrIndex;   /* index of current top item */
   ZH_ISIZ nEndIndex;    /* index of current top item */

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stackIncrease()" ) );

   nBaseIndex = zh_stack.pBase - zh_stack.pItems;
   nCurrIndex = zh_stack.pPos - zh_stack.pItems;
   nEndIndex  = zh_stack.pEnd - zh_stack.pItems;

   /* no, make more headroom: */
   zh_stack.pItems = ( PZH_ITEM * ) zh_xrealloc( ( void * ) zh_stack.pItems,
            sizeof( PZH_ITEM ) * ( zh_stack.nItems + STACK_EXPANDZH_ITEMS ) );

   /* fix possibly modified by realloc pointers: */
   zh_stack.pPos   = zh_stack.pItems + nCurrIndex;
   zh_stack.pBase  = zh_stack.pItems + nBaseIndex;
   zh_stack.nItems += STACK_EXPANDZH_ITEMS;
   zh_stack.pEnd   = zh_stack.pItems + zh_stack.nItems;

   do
   {
      zh_stack.pItems[ nEndIndex ] = ( PZH_ITEM ) zh_xgrab( sizeof( ZH_ITEM ) );
      zh_stack.pItems[ nEndIndex ]->type = ZH_IT_NIL;
   }
   while( ++nEndIndex < zh_stack.nItems );
}

void zh_stackRemove( ZH_ISIZ nUntilPos )
{
   ZH_STACK_TLS_PRELOAD
   PZH_ITEM * pEnd = zh_stack.pItems + nUntilPos;

   while( zh_stack.pPos > pEnd )
   {
      --zh_stack.pPos;
      if( ZH_IS_COMPLEX( *zh_stack.pPos ) )
         zh_itemClear( *zh_stack.pPos );
   }
}

#if defined( ZH_VM_DEBUG )

static void zh_stackDispLocal( void )
{
   char buffer[ 1024 ];
   ZH_STACK_TLS_PRELOAD
   PZH_ITEM * pBase;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stackDispLocal()" ) );

   zh_conOutErr( zh_conNewLine(), 0 );
   zh_snprintf( buffer, sizeof( buffer ), ZH_I_( "Virtual Machine Stack Dump at %s(%i):" ),
                ( *zh_stack.pBase )->item.asSymbol.value->szName,
                ( *zh_stack.pBase )->item.asSymbol.stackstate->uiLineNo );
   zh_conOutErr( buffer, 0 );
   zh_conOutErr( zh_conNewLine(), 0 );
   zh_conOutErr( "--------------------------", 0 );

   for( pBase = zh_stack.pBase; pBase <= zh_stack.pPos; pBase++ )
   {
      zh_conOutErr( zh_conNewLine(), 0 );

      switch( zh_itemType( *pBase ) )
      {
         case ZH_IT_NIL:
            zh_snprintf( buffer, sizeof( buffer ), ZH_I_( "NIL " ) );
            break;

         case ZH_IT_ARRAY:
            if( zh_arrayIsObject( *pBase ) )
               zh_snprintf( buffer, sizeof( buffer ), ZH_I_( "OBJECT = %s " ), zh_objGetClsName( *pBase ) );
            else
               zh_snprintf( buffer, sizeof( buffer ), ZH_I_( "ARRAY " ) );
            break;

         case ZH_IT_BLOCK:
            zh_snprintf( buffer, sizeof( buffer ), ZH_I_( "BLOCK " ) );
            break;

         case ZH_IT_DATE:
         {
            char szDate[ 9 ];
            zh_snprintf( buffer, sizeof( buffer ), ZH_I_( "DATE = \"%s\" " ), zh_itemGetDS( *pBase, szDate ) );
         }
         break;

         case ZH_IT_TIMESTAMP:
         {
            char szDateTime[ 24 ];
            zh_snprintf( buffer, sizeof( buffer ), ZH_I_( "TIMESTAMP = \"%s\" " ),
                         zh_timeStampStr( szDateTime, ( *pBase )->item.asDateTime.julian,
                                          ( *pBase )->item.asDateTime.time ) );
         }
         break;

         case ZH_IT_DOUBLE:
            zh_snprintf( buffer, sizeof( buffer ), ZH_I_( "DOUBLE = %f " ), zh_itemGetND( *pBase ) );
            break;

         case ZH_IT_LOGICAL:
            zh_snprintf( buffer, sizeof( buffer ), ZH_I_( "LOGICAL = %s " ), zh_itemGetL( *pBase ) ? ".T." : ".F." );
            break;

         case ZH_IT_LONG:
         {
            char szBuf[ 24 ];
            zh_snprintf( buffer, sizeof( buffer ), ZH_I_( "LONG = %s " ), zh_numToStr( szBuf, sizeof( szBuf ), zh_itemGetNInt( *pBase ) ) );
            break;
         }

         case ZH_IT_INTEGER:
            zh_snprintf( buffer, sizeof( buffer ), ZH_I_( "INTEGER = %i " ), zh_itemGetNI( *pBase ) );
            break;

         case ZH_IT_STRING:
            zh_snprintf( buffer, sizeof( buffer ), ZH_I_( "STRING = \"%s\" " ), zh_itemGetCPtr( *pBase ) );
            break;

         case ZH_IT_SYMBOL:
            zh_snprintf( buffer, sizeof( buffer ), ZH_I_( "SYMBOL = %s " ), ( *pBase )->item.asSymbol.value->szName );
            break;

         case ZH_IT_POINTER:
            zh_snprintf( buffer, sizeof( buffer ), ZH_I_( "POINTER = %p " ), ( *pBase )->item.asPointer.value );
            break;

         default:
            zh_snprintf( buffer, sizeof( buffer ), ZH_I_( "UNKNOWN = TYPE %i " ), zh_itemType( *pBase ) );
            break;
      }

      zh_conOutErr( buffer, 0 );
   }
}

#endif

PZH_ITEM zh_stackNewFrame( PZH_STACK_STATE pFrame, ZH_USHORT uiParams )
{
   ZH_STACK_TLS_PRELOAD
   PZH_ITEM * pBase, pItem;

   pBase = zh_stack.pPos - uiParams - 2;
   pItem = *pBase;   /* procedure symbol */

   if( ! ZH_IS_SYMBOL( pItem ) )
   {
#if defined( ZH_VM_DEBUG )
      zh_stackDispLocal();
#endif
      zh_errInternal( ZH_EI_VMNOTSYMBOL, NULL, "zh_vmDo()", NULL );
   }

   pFrame->nBaseItem = zh_stack.pBase - zh_stack.pItems;
   pFrame->pStatics = zh_stack.pStatics;
   /* as some type of protection we can set zh_stack.pStatics to NULL here */
   pFrame->nPrivateBase = zh_memvarGetPrivatesBase();
   pFrame->uiClass = pFrame->uiMethod = pFrame->uiLineNo = 0;
   pFrame->fDebugging = ZH_FALSE;

   pItem->item.asSymbol.stackstate = pFrame;
   pItem->item.asSymbol.paramcnt = uiParams;
   /* set default value of 'paramdeclcnt' - it will be updated
    * in zh_vm[V]Frame only
    */
   pItem->item.asSymbol.paramdeclcnt = uiParams;
   zh_stack.pBase = pBase;

   return pItem;
}

void zh_stackOldFrame( PZH_STACK_STATE pFrame )
{
   ZH_STACK_TLS_PRELOAD
   if( zh_stack.pPos <= zh_stack.pBase )
      zh_errInternal( ZH_EI_STACKUFLOW, NULL, NULL, NULL );

   do
   {
      --zh_stack.pPos;
      if( ZH_IS_COMPLEX( *zh_stack.pPos ) )
         zh_itemClear( *zh_stack.pPos );
   }
   while( zh_stack.pPos > zh_stack.pBase );

   zh_stack.pBase = zh_stack.pItems + pFrame->nBaseItem;
   zh_stack.pStatics = pFrame->pStatics;
   zh_memvarSetPrivatesBase( pFrame->nPrivateBase );
}

#undef zh_stackItem
PZH_ITEM zh_stackItem( ZH_ISIZ nItemPos )
{
   ZH_STACK_TLS_PRELOAD
   if( nItemPos < 0 )
      zh_errInternal( ZH_EI_STACKUFLOW, NULL, NULL, NULL );

   return *( zh_stack.pItems + nItemPos );
}

#undef zh_stackItemFromTop
PZH_ITEM zh_stackItemFromTop( int iFromTop )
{
   ZH_STACK_TLS_PRELOAD
   if( iFromTop >= 0 )
      zh_errInternal( ZH_EI_STACKUFLOW, NULL, NULL, NULL );

   return *( zh_stack.pPos + iFromTop );
}

#undef zh_stackItemFromBase
PZH_ITEM zh_stackItemFromBase( int iFromBase )
{
   ZH_STACK_TLS_PRELOAD
   if( iFromBase < 0 )
      zh_errInternal( ZH_EI_STACKUFLOW, NULL, NULL, NULL );

   return *( zh_stack.pBase + iFromBase + 1 );
}

#undef zh_stackLocalVariable
PZH_ITEM zh_stackLocalVariable( int iLocal )
{
   ZH_STACK_TLS_PRELOAD
   PZH_ITEM pBase = *zh_stack.pBase;

/*
   if( iLocal <= 0 )
      zh_errInternal( ZH_EI_STACKUFLOW, NULL, NULL, NULL );
 */
   if( pBase->item.asSymbol.paramcnt > pBase->item.asSymbol.paramdeclcnt )
   {
      /* function with variable number of parameters:
       * FUNCTION foo( a,b,c,...)
       * LOCAL x,y,z
       * number of passed parameters is bigger then number of declared
       * parameters - skip additional parameters only for local variables
       */
      if( iLocal > pBase->item.asSymbol.paramdeclcnt )
         iLocal += pBase->item.asSymbol.paramcnt - pBase->item.asSymbol.paramdeclcnt;
   }
   return *( zh_stack.pBase + iLocal + 1 );
}

#undef zh_stackLocalVariableAt
PZH_ITEM zh_stackLocalVariableAt( int * piFromBase )
{
   ZH_STACK_TLS_PRELOAD
   PZH_ITEM pBase = *zh_stack.pBase;

/*
   if( *piFromBase <= 0 )
      zh_errInternal( ZH_EI_STACKUFLOW, NULL, NULL, NULL );
 */
   if( pBase->item.asSymbol.paramcnt > pBase->item.asSymbol.paramdeclcnt )
   {
      /* function with variable number of parameters:
       * FUNCTION foo( a,b,c,...)
       * LOCAL x,y,z
       * number of passed parameters is bigger then number of declared
       * parameters - skip additional parameters only for local variables
       */
      if( *piFromBase > pBase->item.asSymbol.paramdeclcnt )
         *piFromBase += pBase->item.asSymbol.paramcnt - pBase->item.asSymbol.paramdeclcnt;
   }
   return *( zh_stack.pBase + *piFromBase + 1 );
}

#undef zh_stackBaseItem
PZH_ITEM zh_stackBaseItem( void )
{
   ZH_STACK_TLS_PRELOAD
   return *zh_stack.pBase;
}

/* Returns SELF object, an evaluated codeblock or NIL for normal func/proc
 */
#undef zh_stackSelfItem
PZH_ITEM zh_stackSelfItem( void )
{
   ZH_STACK_TLS_PRELOAD
   return *( zh_stack.pBase + 1 );
}

#undef zh_stackReturnItem
PZH_ITEM zh_stackReturnItem( void )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stackReturnItem()" ) );

   return &zh_stack.Return;
}

#undef zh_stackTopOffset
ZH_ISIZ zh_stackTopOffset( void )
{
   ZH_STACK_TLS_PRELOAD

   return zh_stack.pPos - zh_stack.pItems;
}

#undef zh_stackBaseOffset
ZH_ISIZ zh_stackBaseOffset( void )
{
   ZH_STACK_TLS_PRELOAD

   return zh_stack.pBase - zh_stack.pItems + 1;
}

#undef zh_stackTotalItems
ZH_ISIZ zh_stackTotalItems( void )
{
   if( zh_stack_ready() )
   {
      ZH_STACK_TLS_PRELOAD

      return zh_stack.nItems;
   }
   return 0;
}

void * zh_stackAllocator( void )
{
   if( zh_stack_ready() )
   {
      ZH_STACK_TLS_PRELOAD

      return zh_stack.allocator;
   }
   return NULL;
}

#undef zh_stackDateBuffer
char * zh_stackDateBuffer( void )
{
   ZH_STACK_TLS_PRELOAD

   return zh_stack.szDate;
}

char * zh_stackDirBuffer( void )
{
   if( zh_stack_ready() )
   {
      ZH_STACK_TLS_PRELOAD
      if( ! zh_stack.pDirBuffer )
         zh_stack.pDirBuffer = ( char * ) zh_xgrab( ZH_PATH_MAX );
      return zh_stack.pDirBuffer;
   }
   return s_szDirBuffer;
}

PZH_IOERRORS zh_stackIOErrors( void )
{
   if( zh_stack_ready() )
   {
      ZH_STACK_TLS_PRELOAD
      return &zh_stack.IOErrors;
   }
   return &s_IOErrors;
}

void * zh_stackGetGT( void )
{
   if( zh_stack_ready() )
   {
      ZH_STACK_TLS_PRELOAD
      return zh_stack.hGT;
   }
   else
      return NULL;
}

void zh_stackSetGT( void * hGT )
{
   ZH_STACK_TLS_PRELOAD
   zh_stack.hGT = hGT;
}


PZH_STACKRDD zh_stackRDD( void )
{
   ZH_STACK_TLS_PRELOAD
   return &zh_stack.rdd;
}


#undef zh_stackGetStaticsBase
void * zh_stackGetStaticsBase( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stack.pStatics;
}

#undef zh_stackSetStaticsBase
void zh_stackSetStaticsBase( void * pBase )
{
   ZH_STACK_TLS_PRELOAD
   zh_stack.pStatics = pBase;
}

#undef zh_stackGetRecoverBase
ZH_ISIZ zh_stackGetRecoverBase( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stack.nRecoverBase;
}

#undef zh_stackSetRecoverBase
void zh_stackSetRecoverBase( ZH_ISIZ nBase )
{
   ZH_STACK_TLS_PRELOAD
   zh_stack.nRecoverBase = nBase;
}

#undef zh_stackGetActionRequest
ZH_USHORT zh_stackGetActionRequest( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stack.uiActionRequest;
}

#undef zh_stackSetActionRequest
void zh_stackSetActionRequest( ZH_USHORT uiAction )
{
   ZH_STACK_TLS_PRELOAD
   zh_stack.uiActionRequest = uiAction;
}

#undef zh_stackWithObjectItem
PZH_ITEM zh_stackWithObjectItem( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stack.nWithObject ?
                        * ( zh_stack.pItems + zh_stack.nWithObject ) : NULL;
}

#undef zh_stackWithObjectOffset
ZH_ISIZ zh_stackWithObjectOffset( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stack.nWithObject;
}

#undef zh_stackWithObjectSetOffset
void zh_stackWithObjectSetOffset( ZH_ISIZ nOffset )
{
   ZH_STACK_TLS_PRELOAD
   zh_stack.nWithObject = nOffset;
}

#undef zh_stackGetCodepage
void * zh_stackGetCodepage( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stack.pCDP;
}

#undef zh_stackSetCDP
void zh_stackSetCDP( void * pCDP )
{
   ZH_STACK_TLS_PRELOAD
   zh_stack.pCDP = pCDP;
}

#undef zh_stackGetLang
void * zh_stackGetLang( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stack.pLang;
}

#undef zh_stackSetLang
void zh_stackSetLang( void * pLang )
{
   ZH_STACK_TLS_PRELOAD
   zh_stack.pLang = pLang;
}

#undef zh_stackGetI18N
void * zh_stackGetI18N( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stack.pI18N;
}

#undef zh_stackSetI18N
void zh_stackSetI18N( void * pI18N )
{
   ZH_STACK_TLS_PRELOAD
   zh_stack.pI18N = pI18N;
}

#undef zh_stackItemBasePtr
PZH_ITEM ** zh_stackItemBasePtr( void )
{
   ZH_STACK_TLS_PRELOAD
   return &zh_stack.pItems;
}

void zh_stackClearMemvarsBase( void )
{
   ZH_STACK_TLS_PRELOAD
   PZH_ITEM pBase;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stackClearMemvarsBase()" ) );

   pBase = *zh_stack.pBase;

   while( pBase->item.asSymbol.stackstate->nPrivateBase != 0 )
   {
      pBase->item.asSymbol.stackstate->nPrivateBase = 0;
      pBase = *( zh_stack.pItems + pBase->item.asSymbol.stackstate->nBaseItem );
   }
}

int zh_stackCallDepth( void )
{
   ZH_STACK_TLS_PRELOAD
   ZH_ISIZ nOffset = zh_stack.pBase - zh_stack.pItems;
   int iLevel = 0;

   while( nOffset > 0 )
   {
      nOffset = ( *( zh_stack.pItems + nOffset ) )->item.asSymbol.stackstate->nBaseItem;
      ++iLevel;
   }

   return iLevel;
}

ZH_ISIZ zh_stackBaseProcOffset( int iLevel )
{
   ZH_STACK_TLS_PRELOAD
   ZH_ISIZ nOffset = zh_stack.pBase - zh_stack.pItems;

   while( iLevel-- > 0 && nOffset > 0 )
      nOffset = ( *( zh_stack.pItems + nOffset ) )->item.asSymbol.stackstate->nBaseItem;

   if( iLevel < 0 && ( nOffset > 0 || ZH_IS_SYMBOL( *zh_stack.pItems ) ) )
      return nOffset;
   else
      return -1;
}

ZH_ISIZ zh_stackBaseSymbolOffset( PZH_SYMBOL pSymbol )
{
   ZH_STACK_TLS_PRELOAD
   ZH_ISIZ nOffset = zh_stack.pBase - zh_stack.pItems;

   while( nOffset > 0 )
   {
      PZH_ITEM pItem = zh_stack.pItems[ nOffset ];
      if( pItem->item.asSymbol.value == pSymbol ||
          ( pSymbol->pDynSym != NULL &&
            pItem->item.asSymbol.value->pDynSym == pSymbol->pDynSym ) )
         return nOffset;
      nOffset = pItem->item.asSymbol.stackstate->nBaseItem;
   }
   return -1;
}

void zh_stackBaseProcInfo( char * szProcName, ZH_USHORT * puiProcLine )
{
   /*
    * This function is called by FM module and has to be ready for execution
    * before zh_stack initialization, [druzus]
    * szProcName should be at least ZH_SYMBOL_NAME_LEN + 1 bytes buffer
    */

   if( ! zh_stack_ready() )
   {
      szProcName[ 0 ] = '\0';
      * puiProcLine = 0;
   }
   else
   {
      ZH_STACK_TLS_PRELOAD
      if( zh_stack.pPos > zh_stack.pBase )
      {
         zh_strncpy( szProcName, ( *zh_stack.pBase )->item.asSymbol.value->szName,
                     ZH_SYMBOL_NAME_LEN );
         *puiProcLine = ( *zh_stack.pBase )->item.asSymbol.stackstate->uiLineNo;
      }
      else
      {
         szProcName[ 0 ] = '\0';
         *puiProcLine = 0;
      }
   }
}

void zh_stackDispCall( void )
{
   char buffer[ ZH_SYMBOL_NAME_LEN + ZH_SYMBOL_NAME_LEN + 5 + 10 ]; /* additional 10 bytes for line info (%hu) overhead */
   char file[ ZH_PATH_MAX ];
   ZH_USHORT uiLine;
   int iLevel;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stackDispCall()" ) );

   iLevel = 0;

   while( zh_procinfo( iLevel++, buffer, &uiLine, file ) )
   {
      int l = ( int ) strlen( buffer );
      zh_snprintf( buffer + l, sizeof( buffer ) - l, "(%hu)%s%s", uiLine, *file ? ZH_I_( " in " ) : "", file );

      zh_conOutErr( "Called from ", 0 );
      zh_conOutErr( buffer, 0 );
      zh_conOutErr( zh_conNewLine(), 0 );
   }
}

/* ------------------------------------------------------------------------ */
/* The garbage collector interface */
/* ------------------------------------------------------------------------ */

/* not used: helper function to scan all visible memvar variables

static ZH_DYNS_FUNC( zh_stackMemvarScan )
{
   PZH_ITEM pMemvar;

   ZH_SYMBOL_UNUSED( Cargo );

   pMemvar = zh_dynsymGetMemvar( pDynSymbol );
   if( pMemvar && ZH_IS_GCITEM( pMemvar ) )
      zh_gcItemRef( pMemvar );

   return ZH_TRUE;
}
*/

/* Mark all memvars (PRIVATEs and PUBLICs) */
static void zh_stackIsMemvarRef( PZH_STACK pStack )
{
   /* 1. Mark all hidden memvars (PRIVATEs and PUBLICs) */
   PZH_PRIVATE_STACK pPrivateStack = &pStack->privates;
   ZH_SIZE nCount = pPrivateStack->count;

   while( nCount )
   {
      PZH_ITEM pMemvar = pPrivateStack->stack[ --nCount ].pPrevMemvar;
      if( pMemvar && ZH_IS_GCITEM( pMemvar ) )
         zh_gcItemRef( pMemvar );
   }
   /* 2. Mark all visible memvars (PRIVATEs and PUBLICs) */
   {
      int iDynSym = pStack->iDynH;

      while( --iDynSym >= 0 )
      {
         PZH_ITEM pMemvar = ( PZH_ITEM ) pStack->pDynH[ iDynSym ].pMemvar;
         if( pMemvar && ZH_IS_GCITEM( pMemvar ) )
            zh_gcItemRef( pMemvar );
      }
   }
}

/* Mark all thread static variables */
static void zh_stackIsTsdRef( PZH_STACK pStack, PZH_TSD_FUNC pCleanFunc )
{
   int iTSD = pStack->iTSD;

   while( iTSD )
   {
      if( pStack->pTSD[ iTSD ].pTSD &&
          pStack->pTSD[ iTSD ].pTSD->pCleanFunc == pCleanFunc )
      {
         PZH_ITEM pItem = ( PZH_ITEM ) pStack->pTSD[ iTSD ].value;
         if( ZH_IS_GCITEM( pItem ) )
            zh_gcItemRef( pItem );
      }
      --iTSD;
   }
}

/* Mark all locals as used so they will not be released by the
 * garbage collector
 */
void zh_stackIsStackRef( void * pStackId, PZH_TSD_FUNC pCleanFunc )
{
   PZH_STACK pStack;
   ZH_ISIZ nCount;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stackIsStackRef()" ) );

   pStack = ( PZH_STACK ) pStackId;
   nCount = pStack->pPos - pStack->pItems;
   while( nCount > 0 )
   {
      PZH_ITEM pItem = pStack->pItems[ --nCount ];

      if( ZH_IS_GCITEM( pItem ) )
         zh_gcItemRef( pItem );
   }

   zh_gcItemRef( &pStack->Return );

   zh_stackIsMemvarRef( pStack );

   if( pCleanFunc )
      zh_stackIsTsdRef( pStack, pCleanFunc );

   zh_gtIsGtRef( pStack->hGT );
}

void zh_stackUpdateAllocator( void * pStackId, PZH_ALLOCUPDT_FUNC pFunc, int iCount )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stackUpdateAllocator(%p, %p, %d)", pStackId, ( void * ) pFunc, iCount ) );

   {
      PZH_STACK pStack = ( PZH_STACK ) pStackId;

      if( pStack->allocator )
         pStack->allocator = pFunc( pStack->allocator, iCount );
   }
}

PZH_TRACEINFO zh_traceinfo( void )
{
   if( zh_stack_ready() )
   {
      ZH_STACK_TLS_PRELOAD
      return &zh_stack.traceInfo;
   }
   return &s_traceInfo;
}

void zh_traceset( int level, const char * file, int line, const char * proc )
{
   PZH_TRACEINFO pTrace = zh_traceinfo();

   pTrace->level = level;
   pTrace->file  = file;
   pTrace->line  = line;
   pTrace->proc  = proc;
}
