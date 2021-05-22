/*
 * Codeblock runtime support
 *
 * Copyright 1999 Ryszard Glab <rglab@imid.med.pl>
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

#include "zh_vm_opt.h"
#include "zh_api.h"
#include "zh_item_api.h"
#include "zh_class_api.h"
#include "zh_vm.h"
#include "zh_stack.h"
#include "zh_pcode.h"

/* Dummy returning NIL for buggy code which may store references
   to freed by GC codeblock in .zh destructors and then (after
   catching RT EG_DESTRUCTOR error) try to execute them */
static const ZH_BYTE s_pCode[ 2 ] = { ZH_P_PUSHNIL, ZH_P_ENDBLOCK };

/* Release all allocated memory when called from the garbage collector */
static ZH_GARBAGE_FUNC( zh_codeblockGarbageDelete )
{
   PZH_CODEBLOCK pCBlock = ( PZH_CODEBLOCK ) Cargo;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_codeblockGarbageDelete(%p)", Cargo ) );

   /* free space allocated for pcodes - if it was a macro-compiled codeblock */
   if( pCBlock->pCode && pCBlock->dynBuffer )
   {
      pCBlock->dynBuffer = ZH_FALSE;
      zh_xfree( ZH_UNCONST( pCBlock->pCode ) );
   }
   pCBlock->pCode = s_pCode;

   /* free space allocated for local variables */
   if( pCBlock->pLocals )
   {
      if( zh_xRefDec( pCBlock->pLocals ) )
      {
         while( pCBlock->uiLocals )
            zh_memvarValueDecRef( pCBlock->pLocals[ pCBlock->uiLocals-- ].item.asMemvar.value );
         zh_xfree( pCBlock->pLocals );
      }
      pCBlock->pLocals = NULL;
      pCBlock->uiLocals = 0;
   }
}

static ZH_GARBAGE_FUNC( zh_codeblockGarbageMark )
{
   PZH_CODEBLOCK pCBlock = ( PZH_CODEBLOCK ) Cargo;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_codeblockGarbageMark(%p)", Cargo ) );

   if( pCBlock->uiLocals )
   {
      PZH_ITEM pLocals = pCBlock->pLocals;
      ZH_USHORT uiLocals = pCBlock->uiLocals;

      do
      {
         zh_gcItemRef( &pLocals[ uiLocals ] );
      }
      while( --uiLocals );
   }
}

static const ZH_GC_FUNCS s_gcCodeblockFuncs =
{
   zh_codeblockGarbageDelete,
   zh_codeblockGarbageMark,
};

/* Creates the codeblock structure
 *
 * pBuffer -> the buffer with pcodes (without ZH_P_PUSH_BLOCK)
 * wLocals -> number of local variables referenced in a codeblock
 * pLocalPosTable -> a table with positions on eval stack for referenced variables
 * pSymbols    -> a pointer to the module symbol table
 *
 * Note: pLocalPosTable cannot be used if uiLocals is ZERO
 */
PZH_CODEBLOCK zh_codeblockNew( const ZH_BYTE * pBuffer,
                               ZH_USHORT uiLocals,
                               const ZH_BYTE * pLocalPosTable,
                               PZH_SYMBOL pSymbols,
                               ZH_SIZE nLen )
{
   ZH_STACK_TLS_PRELOAD
   PZH_CODEBLOCK pCBlock;
   PZH_ITEM pLocals, pBase;
   const ZH_BYTE * pCode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_codeblockNew(%p, %hu, %p, %p, %" ZH_PFS "u)", ( const void * ) pBuffer, uiLocals, ( const void * ) pLocalPosTable, ( void * ) pSymbols, nLen ) );

   /* Allocate memory for code block body and detach items zh_gcAllocRaw()
    * to be safe for automatic GC activation in zh_xgrab() without
    * calling zh_gcLock()/zh_gcUnlock(). [druzus]
    */

   if( nLen )
   {
      /* The codeblock pcode is stored in dynamically allocated memory that
       * can be deallocated after creation of a codeblock. We have to duplicate
       * the passed buffer
       */
      pCode = ( const ZH_BYTE * ) memcpy( zh_xgrab( nLen ), pBuffer, nLen );
   }
   else
   {
      /* The codeblock pcode is stored in static segment.
       * The only allowed operation on a codeblock is evaluating it then
       * there is no need to duplicate its pcode - just store the pointer to it
       */
      pCode = pBuffer;
   }

   if( uiLocals )
   {
      /* NOTE: if a codeblock will be created by macro compiler then
       * uiLocal have to be ZERO
       * uiLocal will be also ZERO if it is a nested codeblock
       */
      ZH_USHORT ui = 1;
      PZH_ITEM pLocal;

      /* Create a table that will store the values of local variables
       * accessed in a codeblock
       * The element 0 is unused
       * NOTE: This table can be shared by codeblocks created during
       * evaluation of this codeblock
       */
      pLocals = ( PZH_ITEM ) zh_xgrab( ( uiLocals + 1 ) * sizeof( ZH_ITEM ) );
      pLocals[ 0 ].type = ZH_IT_NIL;

      do
      {
         /* Swap the current value of local variable with the reference to this
          * value.
          */
         int iLocal = ZH_PCODE_MKUSHORT( pLocalPosTable );
         pLocal = zh_stackLocalVariable( iLocal );
         pLocalPosTable += 2;

         pLocal = zh_memvarDetachLocal( pLocal );
         zh_itemRawCpy( pLocals + ui, pLocal );
         /* Increment the reference counter so this value will not be
          * released if other codeblock will be deleted
          */
         zh_memvarValueIncRef( pLocal->item.asMemvar.value );
      }
      while( ++ui <= uiLocals );
   }
   else
   {
      /* Check if this codeblock is created during evaluation of another
       * codeblock - all inner codeblocks use the local variables table
       * created during creation of the outermost codeblock
       */
      PZH_ITEM pLocal;

      pLocal = zh_stackSelfItem();
      if( ZH_IS_BLOCK( pLocal ) )
      {
         PZH_CODEBLOCK pOwner = pLocal->item.asBlock.value;

         uiLocals = pOwner->uiLocals;
         pLocals  = pOwner->pLocals;
         if( pLocals )
            zh_xRefInc( pLocals );
      }
      else
         pLocals = NULL;
   }

   pBase = zh_stackBaseItem();
   pCBlock = ( PZH_CODEBLOCK ) zh_gcAllocRaw( sizeof( ZH_CODEBLOCK ), &s_gcCodeblockFuncs );

   pCBlock->pCode     = pCode;
   pCBlock->dynBuffer = nLen != 0;
   pCBlock->pDefSymb  = pBase->item.asSymbol.stackstate->uiClass ?
                        zh_clsMethodSym( pBase ) : pBase->item.asSymbol.value;
   pCBlock->pSymbols  = pSymbols;
   pCBlock->pStatics  = zh_stackGetStaticsBase();
   pCBlock->uiLocals  = uiLocals;
   pCBlock->pLocals   = pLocals;

   ZH_TRACE( ZH_TR_INFO, ( "codeblock created %p", ( void * ) pCBlock ) );

   return pCBlock;
}

PZH_CODEBLOCK zh_codeblockMacroNew( const ZH_BYTE * pBuffer, ZH_SIZE nLen )
{
   ZH_STACK_TLS_PRELOAD
   PZH_CODEBLOCK pCBlock;
   PZH_ITEM pBase;
   ZH_BYTE * pCode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_codeblockMacroNew(%p, %" ZH_PFS "u)", ( const void * ) pBuffer, nLen ) );

   /* The codeblock pcode is stored in dynamically allocated memory that
    * can be deallocated after creation of a codeblock. We have to duplicate
    * the passed buffer
    */
   /* allocate memory for code block body and detach items zh_gcAllocRaw()
    * to be safe for automatic GC activation in zh_xgrab() without
    * calling zh_gcLock()/zh_gcUnlock(). [druzus]
    */
   pCode = ( ZH_BYTE * ) memcpy( zh_xgrab( nLen ), pBuffer, nLen );

   pCBlock = ( PZH_CODEBLOCK ) zh_gcAllocRaw( sizeof( ZH_CODEBLOCK ), &s_gcCodeblockFuncs );
   pBase = zh_stackBaseItem();
   /* Store the number of referenced local variables */
   pCBlock->pCode     = pCode;
   pCBlock->dynBuffer = ZH_TRUE;
   pCBlock->pDefSymb  = pBase->item.asSymbol.stackstate->uiClass ?
                        zh_clsMethodSym( pBase ) : pBase->item.asSymbol.value;
   pCBlock->pSymbols  = NULL;  /* macro-compiled codeblock cannot access a local symbol table */
   pCBlock->pStatics  = zh_stackGetStaticsBase();
   pCBlock->uiLocals  = 0;
   pCBlock->pLocals   = NULL;

   ZH_TRACE( ZH_TR_INFO, ( "codeblock created %p", ( void * ) pCBlock ) );

   return pCBlock;
}

/* Get local variable referenced in a codeblock */
PZH_ITEM zh_codeblockGetVar( PZH_ITEM pItem, int iItemPos )
{
   PZH_CODEBLOCK pCBlock = pItem->item.asBlock.value;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_codeblockGetVar(%p, %d)", ( void * ) pItem, iItemPos ) );

   /* local variables accessed in a codeblock are always stored as reference */
   return zh_itemUnRef( pCBlock->pLocals - iItemPos );
}

/* Get local variable passed by reference */
PZH_ITEM zh_codeblockGetRef( PZH_CODEBLOCK pCBlock, int iItemPos )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_codeblockGetRef(%p, %d)", ( void * ) pCBlock, iItemPos ) );

   return pCBlock->pLocals - iItemPos;
}

/* retrieves the codeblock unique ID */
void * zh_codeblockId( PZH_ITEM pItem )
{
   if( ZH_IS_BLOCK( pItem ) )
      return ( void * ) pItem->item.asBlock.value;
   else
      return NULL;
}

/* retrieves numer of references to the codeblock */
ZH_COUNTER zh_codeblockRefs( PZH_ITEM pItem )
{
   if( ZH_IS_BLOCK( pItem ) )
      return zh_gcRefCount( pItem->item.asBlock.value );
   else
      return 0;
}
