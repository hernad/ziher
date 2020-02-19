/*
 * Debugging functions for LOCAL, STATIC variables and the stack
 *
 * Copyright 1999 Eddie Runia <eddie@runia.com>
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

#include "zh_vm_int.h"
#include "zh_api.h"
#include "zh_debug_api.h"
#include "zh_item_api.h"
#include "zh_api_error.h"
#include "zh_stack.h"

/* Existing debug functions
 * from debug.c:
 *    __dbgVMStkGCount()
 *    __dbgVMStkGList()
 *    __dbgVMStkLCount()
 *    __dbgVMStkLList()
 *    __dbgVMLocalList()
 *    __dbgVMParLList()
 * for locals:
 *    __dbgVMVarLGet()                   debugger.zh
 *    __dbgVMVarLSet()                   debugger.zh
 *    zh_dbg_vmVarLGet( int, int )       dbgentry.c
 *
 * from classes.c:
 *    zh_dbg_objSendMessage( ... )       dbgentry.c
 *
 * from hvm.c
 * general:
 *    __dbgInvokeDebug()                 debugger.zh
 *    __dbgProcLevel()                   debugger.zh
 *    zh_dbg_InvokeDebug( ZH_BOOL )      dbgentry.c
 *    zh_dbg_ProcLevel()                 dbgentry.c
 *    zh_dbg_SetEntry( * ENTRY_FUNC )    dbgentry.c
 * for statics:
 *    __dbgVMVarSList()
 *    __dbgVMVarSLen()
 *    __dbgVMVarSGet()                   debugger.zh
 *    __dbgVMVarSSet()                   debugger.zh
 *    zh_dbg_vmVarSGet( PZH_ITEM, int )  dbgentry.c
 * for globals (unused):
 *    __dbgVMVarGList()
 *    __dbgVMVarGGet()                   debugger.zh
 *    __dbgVMVarGSet()                   debugger.zh
 *    zh_dbg_vmVarGCount()               dbgentry.c
 *    zh_dbg_vmVarGGet( int, int )       dbgentry.c
 *
 *
 * Information from ZHVM send to debugger by __dbgEntry()
 *    ZH_DBG_MODULENAME, cName
 *    ZH_DBG_STATICNAME, nBase,  nIndex, cName
 *    ZH_DBG_LOCALNAME,  nIndex, cName
 *    ZH_DBG_SHOWLINE,   nLine
 *    ZH_DBG_ENDPROC
 *    ZH_DBG_GETENTRY
 *    ZH_DBG_VMQUIT
 */

/* Add <pItem> to array <pReturn> at pos <nPos>
 */
static void AddToArray( PZH_ITEM pItem, PZH_ITEM pReturn, ZH_SIZE nPos )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "AddToArray(%p, %p, %" ZH_PFS "u)", ( void * ) pItem, ( void * ) pReturn, nPos ) );

   if( ZH_IS_SYMBOL( pItem ) )  /* Symbol is pushed as text */
   {
      PZH_ITEM pArrayItem = zh_arrayGetItemPtr( pReturn, nPos );

      if( pArrayItem )
      {
         ZH_SIZE nLen = strlen( pItem->item.asSymbol.value->szName ) + 2;
         char * szBuff = ( char * ) zh_xgrab( nLen + 1 );

         zh_snprintf( szBuff, nLen + 1, "[%s]", pItem->item.asSymbol.value->szName );
         zh_itemPutCLPtr( pArrayItem, szBuff, nLen );
      }
   }
   else  /* Normal types */
      zh_itemArrayPut( pReturn, nPos, pItem );
}

/* __dbgVMStkGCount() --> <nVars>
 * Returns the length of the global stack
 */
ZH_FUNC( __DBGVMSTKGCOUNT )
{
   if( zh_vmInternalsEnabled() )
      zh_retns( zh_stackTopOffset() );
   else
      zh_retns( 0 );
}

/* __dbgVMStkGList() --> <aStack>
 * Returns the global stack
 */
ZH_FUNC( __DBGVMSTKGLIST )
{
   if( zh_vmInternalsEnabled() )
   {
      PZH_ITEM pReturn;
      ZH_ISIZ nLen = zh_stackTopOffset();
      ZH_ISIZ nPos;

      pReturn = zh_itemArrayNew( nLen );  /* Create a transfer array */

      for( nPos = 0; nPos < nLen; ++nPos )
         AddToArray( zh_stackItem( nPos ), pReturn, nPos + 1 );

      zh_itemReturnRelease( pReturn );
   }
   else
      zh_reta( 0 );
}

/* zh_stackLen( <nProcLevel> ) --> <nVars>
 * Returns params plus locals amount of the nProcLevel function
 */
static ZH_ISIZ zh_stackLen( int iLevel )
{
   ZH_ISIZ nBaseOffset, nPrevOffset, nLen;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stackLen()" ) );

   nBaseOffset = zh_stackBaseOffset();
   while( --iLevel > 0 && nBaseOffset > 1 )
      nBaseOffset = zh_stackItem( nBaseOffset - 1 )->item.asSymbol.stackstate->nBaseItem + 1;

   if( nBaseOffset > 1 )
   {
      nPrevOffset = zh_stackItem( nBaseOffset - 1 )->item.asSymbol.stackstate->nBaseItem;
      nLen = nBaseOffset - nPrevOffset - 3;
   }
   else
      nLen = 0;

   return nLen;
}

/* __dbgVMStkLCount( <nProcLevel> ) --> <nVars>
 * Returns params plus locals amount of the nProcLevel function
 */
ZH_FUNC( __DBGVMSTKLCOUNT )
{
   if( zh_vmInternalsEnabled() )
      zh_retns( zh_stackLen( zh_parni( 1 ) + 1 ) );
   else
      zh_retns( 0 );
}

/* __dbgVMStkLList() --> <aStack>
 * Returns the stack of the calling function
 * "[<symbol>]"  Means symbol.
 *
 * [1]        Symbol of current function
 * [2]        Self | NIL
 * [3 .. x]   Parameters
 * [x+1 .. y] Locals
 * [y+1 ..]   Pushed data
 */
ZH_FUNC( __DBGVMSTKLLIST )
{
   if( zh_vmInternalsEnabled() )
   {
      PZH_ITEM pReturn;
      ZH_ISIZ nLen, n;
      ZH_ISIZ nBaseOffset, nPrevOffset;

      nBaseOffset = zh_stackBaseOffset();
      nPrevOffset = zh_stackItem( nBaseOffset - 1 )->item.asSymbol.stackstate->nBaseItem;

      nLen = nBaseOffset - nPrevOffset - 3;
      pReturn = zh_itemArrayNew( nLen );  /* Create a transfer array */
      for( n = 0; n < nLen; ++n )
         AddToArray( zh_stackItem( nPrevOffset + n ), pReturn, n + 1 );

      zh_itemReturnRelease( pReturn );
   }
   else
      zh_reta( 0 );
}

ZH_FUNC( __DBGVMLOCALLIST )
{
   if( zh_vmInternalsEnabled() )
   {
      PZH_ITEM pArray;
      ZH_ISIZ nBaseOffset, nPrevOffset, nLen, n;
      int iLevel = zh_parni( 1 ) + 1;

      nBaseOffset = zh_stackBaseOffset();
      while( --iLevel > 0 && nBaseOffset > 1 )
         nBaseOffset = zh_stackItem( nBaseOffset - 1 )->item.asSymbol.stackstate->nBaseItem + 1;

      if( nBaseOffset > 1 )
      {
         PZH_ITEM pSymItm;

         nPrevOffset = zh_stackItem( nBaseOffset - 1 )->item.asSymbol.stackstate->nBaseItem;
         pSymItm = zh_stackItem( nPrevOffset );
         nPrevOffset += ZH_MAX( pSymItm->item.asSymbol.paramdeclcnt,
                                pSymItm->item.asSymbol.paramcnt ) + 1;
         nLen = nBaseOffset - nPrevOffset - 2;
      }
      else
         nLen = nPrevOffset = 0;

      pArray = zh_itemArrayNew( nLen );
      for( n = 1; n <= nLen; ++n )
         zh_itemCopyFromRef( zh_arrayGetItemPtr( pArray, n ),
                             zh_stackItem( nPrevOffset + n ) );

      zh_itemReturnRelease( pArray );
   }
   else
      zh_reta( 0 );
}

ZH_FUNC( __DBGVMPARLLIST )
{
   if( zh_vmInternalsEnabled() )
      zh_itemReturnRelease( zh_arrayFromParams( zh_parni( 1 ) + 1 ) );
   else
      zh_reta( 0 );
}

PZH_ITEM zh_dbg_vmVarLGet( int iLevel, int iLocal )
{
   PZH_ITEM pLocal = NULL;
   ZH_ISIZ nBaseOffset;

   nBaseOffset = zh_stackBaseOffset();
   while( iLevel-- > 0 && nBaseOffset > 1 )
      nBaseOffset = zh_stackItem( nBaseOffset - 1 )->item.asSymbol.stackstate->nBaseItem + 1;

   if( iLevel < 0 )
   {
      if( iLocal > SHRT_MAX )
      {
         iLocal -= USHRT_MAX;
         iLocal--;
      }

      if( iLocal >= 0 )
      {
         PZH_ITEM pBase = zh_stackItem( nBaseOffset - 1 );

         if( pBase->item.asSymbol.paramcnt > pBase->item.asSymbol.paramdeclcnt &&
             iLocal > pBase->item.asSymbol.paramdeclcnt )
            iLocal += pBase->item.asSymbol.paramcnt - pBase->item.asSymbol.paramdeclcnt;

         pLocal = zh_stackItem( nBaseOffset + iLocal );
      }
      else
         pLocal = zh_codeblockGetRef( zh_stackItem( nBaseOffset )->item.asBlock.value, iLocal );

      if( ZH_IS_BYREF( pLocal ) )
         pLocal = zh_itemUnRef( pLocal );
   }

   return pLocal;
}

ZH_FUNC( __DBGVMVARLGET )
{
   if( zh_vmInternalsEnabled() )
   {
      int iLevel = zh_parni( 1 ) + 1;
      int iLocal = zh_parni( 2 );
      PZH_ITEM pLocal = zh_dbg_vmVarLGet( iLevel, iLocal );

      if( pLocal )
         zh_itemReturn( pLocal );
      else
         zh_errRT_BASE( EG_ARG, 6005, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
   }
}

ZH_FUNC( __DBGVMVARLSET )
{
   if( zh_vmInternalsEnabled() )
   {
      int iLevel = zh_parni( 1 ) + 1;
      int iLocal = zh_parni( 2 );
      ZH_ISIZ nBaseOffset;

      nBaseOffset = zh_stackBaseOffset();
      while( iLevel-- > 0 && nBaseOffset > 1 )
         nBaseOffset = zh_stackItem( nBaseOffset - 1 )->item.asSymbol.stackstate->nBaseItem + 1;

      if( iLevel < 0 )
      {
         PZH_ITEM pLocal;

         if( iLocal > SHRT_MAX )
         {
            iLocal -= USHRT_MAX;
            iLocal--;
         }

         if( iLocal >= 0 )
         {
            PZH_ITEM pBase = zh_stackItem( nBaseOffset - 1 );

            if( pBase->item.asSymbol.paramcnt > pBase->item.asSymbol.paramdeclcnt &&
                iLocal > pBase->item.asSymbol.paramdeclcnt )
               iLocal += pBase->item.asSymbol.paramcnt - pBase->item.asSymbol.paramdeclcnt;

            pLocal = zh_stackItem( nBaseOffset + iLocal );
         }
         else
            pLocal = zh_codeblockGetRef( zh_stackItem( nBaseOffset )->item.asBlock.value, iLocal );

         zh_itemCopyToRef( pLocal, zh_stackItemFromBase( 3 ) );
      }
   }
}
