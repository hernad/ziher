/*
 * The Eval API
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * Copyright 1999-2001 Viktor Szakats (zh_itemDo()/zh_itemDoC() (based on ZH_DO() by Ryszard Glab))
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
#include "zh_stack.h"
#include "zh_item_api.h"
#include "zh_error_api.h"
#include "zh_vm.h"

ZH_BOOL zh_evalNew( PZH_EVALINFO pEvalInfo, PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_evalNew(%p, %p)", ( void * ) pEvalInfo, ( void * ) pItem ) );

   if( pEvalInfo )
   {
      memset( pEvalInfo, 0, sizeof( ZH_EVALINFO ) );
      pEvalInfo->pItems[ 0 ] = pItem;
      pEvalInfo->paramCount = 0;

      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}

/* NOTE: CA-Cl*pper is buggy and will not check if more parameters are
         added than the maximum (9). [vszakats] */

/* NOTE: CA-Cl*pper NG suggests that the Items passed as parameters should/may
         be released by the programmer explicitly. But in fact zh_evalRelease()
         will automatically release them all. The sample programs in the
         NG are doing it that way. Releasing the parameters explicitly in
         Ziher will cause an internal error, while it will be silently
         ignored (?) in CA-Cl*pper. This is due to the different internal
         handling of the Items, but IIRC it causes leak in CA-Cl*pper. All in
         all, don't release the eval parameter Items explicitly to make both
         Ziher and CA-Cl*pper happy. [vszakats] */

ZH_BOOL zh_evalPutParam( PZH_EVALINFO pEvalInfo, PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_evalPutParam(%p, %p)", ( void * ) pEvalInfo, ( void * ) pItem ) );

   if( pEvalInfo && pItem && pEvalInfo->paramCount < ZH_EVAL_PARAM_MAX_ )
   {
      pEvalInfo->pItems[ ++pEvalInfo->paramCount ] = pItem;

      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}

PZH_ITEM zh_evalLaunch( PZH_EVALINFO pEvalInfo )
{
   PZH_ITEM pResult = NULL;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_evalLaunch(%p)", ( void * ) pEvalInfo ) );

   if( pEvalInfo )
   {
      PZH_ITEM pItem = pEvalInfo->pItems[ 0 ];
      PZH_SYMBOL pSymbol = NULL;

      if( ZH_IS_STRING( pItem ) )
      {
         PZH_DYNSYMBOL pDynSym = zh_dynsymFindName( pItem->item.asString.value );

         if( pDynSym )
         {
            pSymbol = pDynSym->pSymbol;
            pItem = NULL;
         }
      }
      else if( ZH_IS_SYMBOL( pItem ) )
      {
         pSymbol = pItem->item.asSymbol.value;
         pItem = NULL;
      }
      else if( ZH_IS_BLOCK( pItem ) )
      {
         pSymbol = &zh_symEval;
      }

      if( pSymbol )
      {
         ZH_USHORT uiParam = 0;

         zh_vmPushSymbol( pSymbol );
         if( pItem )
            zh_vmPush( pItem );
         else
            zh_vmPushNil();
         while( uiParam < pEvalInfo->paramCount )
            zh_vmPush( pEvalInfo->pItems[ ++uiParam ] );
         if( pItem )
            zh_vmSend( uiParam );
         else
            zh_vmProc( uiParam );
         pResult = zh_itemNew( zh_stackReturnItem() );
      }
   }

   return pResult;
}

/* NOTE: CA-Cl*pper NG states that zh_evalLaunch() must be called at least
         once and only once before calling zh_evalRelease(). Ziher doesn't
         have these requirements. [vszakats] */

ZH_BOOL zh_evalRelease( PZH_EVALINFO pEvalInfo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_evalRelease(%p)", ( void * ) pEvalInfo ) );

   if( pEvalInfo )
   {
      ZH_USHORT uiParam;

      for( uiParam = 0; uiParam <= pEvalInfo->paramCount; uiParam++ )
      {
         zh_itemRelease( pEvalInfo->pItems[ uiParam ] );
         pEvalInfo->pItems[ uiParam ] = NULL;
      }

      pEvalInfo->paramCount = 0;

      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}

/* NOTE: Same purpose as zh_evalLaunch(), but simpler, faster and more flexible.
         It can be used to call symbols, functions names, or blocks, the items
         don't need to be duplicated when passed as argument, one line is
         enough to initiate a call, the number of parameters is not limited.
         [vszakats]

   NOTE: When calling zh_itemDo() with no arguments for the Ziher item being
         evaluated, you must use '(PZH_ITEM *) 0' as the third parameter.
 */

PZH_ITEM zh_itemDo( PZH_ITEM pItem, ZH_ULONG ulPCount, ... )
{
   PZH_ITEM pResult = NULL;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemDo(%p, %lu, ...)", ( void * ) pItem, ulPCount ) );

   if( pItem )
   {
      PZH_SYMBOL pSymbol = NULL;

      if( ZH_IS_STRING( pItem ) )
      {
         PZH_DYNSYMBOL pDynSym = zh_dynsymFindName( pItem->item.asString.value );

         if( pDynSym )
         {
            pSymbol = pDynSym->pSymbol;
            pItem = NULL;
         }
      }
      else if( ZH_IS_SYMBOL( pItem ) )
      {
         pSymbol = pItem->item.asSymbol.value;
         pItem = NULL;
      }
      else if( ZH_IS_BLOCK( pItem ) )
      {
         pSymbol = &zh_symEval;
      }

      if( pSymbol )
      {
         if( zh_vmRequestReenter() )
         {
            zh_vmPushSymbol( pSymbol );
            if( pItem )
               zh_vmPush( pItem );
            else
               zh_vmPushNil();

            if( ulPCount )
            {
               ZH_ULONG ulParam;
               va_list va;
               va_start( va, ulPCount );
               for( ulParam = 1; ulParam <= ulPCount; ulParam++ )
                  zh_vmPush( va_arg( va, PZH_ITEM ) );
               va_end( va );
            }
            if( pItem )
               zh_vmSend( ( ZH_USHORT ) ulPCount );
            else
               zh_vmProc( ( ZH_USHORT ) ulPCount );

            pResult = zh_itemNew( zh_stackReturnItem() );
            zh_vmRequestRestore();
         }
      }
   }

   return pResult;
}

/* NOTE: Same as zh_itemDo(), but even simpler, since the function name can be
         directly passed as a zero terminated string. [vszakats]

   NOTE: When calling zh_itemDoC() with no arguments for the Ziher function
         being called, you must use '(PZH_ITEM *) 0' as the third parameter.
 */

PZH_ITEM zh_itemDoC( const char * szFunc, ZH_ULONG ulPCount, ... )
{
   PZH_ITEM pResult = NULL;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemDoC(%s, %lu, ...)", szFunc, ulPCount ) );

   if( szFunc )
   {
      PZH_DYNSYMBOL pDynSym = zh_dynsymFindName( szFunc );

      if( pDynSym )
      {
         if( zh_vmRequestReenter() )
         {
            zh_vmPushSymbol( pDynSym->pSymbol );
            zh_vmPushNil();
            if( ulPCount )
            {
               ZH_ULONG ulParam;
               va_list va;
               va_start( va, ulPCount );
               for( ulParam = 1; ulParam <= ulPCount; ulParam++ )
                  zh_vmPush( va_arg( va, PZH_ITEM ) );
               va_end( va );
            }
            zh_vmProc( ( ZH_USHORT ) ulPCount );
            pResult = zh_itemNew( zh_stackReturnItem() );
            zh_vmRequestRestore();
         }
      }
   }

   return pResult;
}

/*
 * Notice that these two functions place the result at zh_stackReturnItem(),
 * that you may access its value using a zh_par...( -1 ).
 */


void zh_evalBlock0( PZH_ITEM pCodeBlock )
{
   zh_vmPushEvalSym();
   zh_vmPush( pCodeBlock );
   zh_vmSend( 0 );
}

void zh_evalBlock1( PZH_ITEM pCodeBlock, PZH_ITEM pParam )
{
   zh_vmPushEvalSym();
   zh_vmPush( pCodeBlock );
   zh_vmPush( pParam );
   zh_vmSend( 1 );
}

/* same functionality but with a NULL terminated list of parameters */
void zh_evalBlock( PZH_ITEM pCodeBlock, ... )
{
   va_list args;
   ZH_USHORT uiParams = 0;
   PZH_ITEM pParam;

   zh_vmPushEvalSym();
   zh_vmPush( pCodeBlock );

   va_start( args, pCodeBlock );
   while( ( pParam = va_arg( args, PZH_ITEM ) ) != NULL )
   {
      zh_vmPush( pParam );
      uiParams++;
   }
   va_end( args );

   zh_vmSend( uiParams );
}

ZH_FUNC( ZH_FORNEXT ) /* nStart, nEnd | bEnd, bCode, nStep */
{
   PZH_ITEM pCodeBlock = zh_param( 3, ZH_IT_BLOCK );

   if( pCodeBlock )
   {
      ZH_MAXINT nStart = zh_parnint( 1 ), nEnd;
      ZH_MAXINT nStep = ( zh_pcount() > 3 ) ? zh_parnint( 4 ) : 1;

      PZH_ITEM pEndBlock = zh_param( 2, ZH_IT_BLOCK );

      if( pEndBlock )
      {
         zh_evalBlock0( pEndBlock );
         nEnd = zh_parnint( -1 );

         while( nStart <= nEnd )
         {
            zh_vmPushEvalSym();
            zh_vmPush( pCodeBlock );
            zh_vmPushNumInt( nStart );
            zh_vmSend( 1 );

            nStart += nStep;

            zh_evalBlock0( pEndBlock );
            nEnd = zh_parnint( -1 );
         }
      }
      else
      {
         nEnd = zh_parnint( 2 );
         while( nStart <= nEnd )
         {
            zh_vmPushEvalSym();
            zh_vmPush( pCodeBlock );
            zh_vmPushNumInt( nStart );
            zh_vmSend( 1 );

            nStart += nStep;
         }
      }
   }
}

/*
 * based on xZiher's zh_ExecFromArray() by Giancarlo Niccolai
 * This version supports the same syntax though it's independent
 * implementation [druzus]
 *
 * The following syntax is supported:
 *    zh_ExecFromArray( <cFuncName> [, <aParams> ] )
 *    zh_ExecFromArray( @<funcName>() [, <aParams> ] )
 *    zh_ExecFromArray( <bCodeBlock> [, <aParams> ] )
 *    zh_ExecFromArray( <oObject>, <cMethodName> [, <aParams> ] )
 *    zh_ExecFromArray( <oObject>, @<msgName>() [, <aParams> ] )
 * or:
 *    zh_ExecFromArray( <aExecArray> )
 * where <aExecArray> is in one of the following format:
 *    { <cFuncName> [, <params,...>] }
 *    { @<funcName>() [, <params,...>] }
 *    { <bCodeBlock> [, <params,...>] }
 *    { <oObject>, <cMethodName> [, <params,...>] }
 *    { <oObject>, @<msgName>() [, <params,...>] }
 */
ZH_FUNC( ZH_EXECFROMARRAY )
{
   PZH_SYMBOL pExecSym = NULL;
   PZH_ITEM pFunc = NULL;
   PZH_ITEM pSelf = NULL;
   PZH_ITEM pArray = NULL;
   PZH_ITEM pItem;
   ZH_ULONG ulParamOffset = 0;
   int iPCount = zh_pcount();

   /* decode parameters */
   if( iPCount )
   {
      PZH_ITEM pParam = zh_param( 1, ZH_IT_ANY );

      if( iPCount == 1 )
      {
         if( ZH_IS_ARRAY( pParam ) && ! ZH_IS_OBJECT( pParam ) )
         {
            pArray = pParam;
            pItem = zh_arrayGetItemPtr( pArray, 1 );
            if( ZH_IS_OBJECT( pItem ) )
            {
               pSelf = pItem;
               pFunc = zh_arrayGetItemPtr( pArray, 2 );
               ulParamOffset = 2;
            }
            else
            {
               pFunc = pItem;
               ulParamOffset = 1;
            }
         }
         else
            pFunc = pParam;
      }
      else if( ZH_IS_OBJECT( pParam ) && iPCount <= 3 )
      {
         pSelf = pParam;
         pFunc = zh_param( 2, ZH_IT_ANY );
         pArray = zh_param( 3, ZH_IT_ANY );
      }
      else if( iPCount == 2 )
      {
         pFunc = pParam;
         pArray = zh_param( 2, ZH_IT_ANY );
      }
   }

   if( pFunc && ( ! pArray || ZH_IS_ARRAY( pArray ) ) )
   {
      if( ZH_IS_SYMBOL( pFunc ) )
         pExecSym = zh_itemGetSymbol( pFunc );
      else if( ZH_IS_STRING( pFunc ) )
         pExecSym = zh_dynsymGet( zh_itemGetCPtr( pFunc ) )->pSymbol;
      else if( ZH_IS_BLOCK( pFunc ) && ! pSelf )
      {
         pSelf = pFunc;
         pExecSym = &zh_symEval;
      }
   }

   if( pExecSym )
   {
      pFunc = zh_stackBaseItem();
      pItem = zh_stackItem( pFunc->item.asSymbol.stackstate->nBaseItem );
      pFunc->item.asSymbol.stackstate->uiClass =
      pItem->item.asSymbol.stackstate->uiClass;
      pFunc->item.asSymbol.stackstate->uiMethod =
      pItem->item.asSymbol.stackstate->uiMethod;

      iPCount = 0;
      zh_vmPushSymbol( pExecSym );
      if( pSelf )
         zh_vmPush( pSelf );
      else
         zh_vmPushNil();

      if( pArray )
      {
         pItem = zh_arrayGetItemPtr( pArray, ++ulParamOffset );
         while( pItem && iPCount < 255 )
         {
            zh_vmPush( pItem );
            ++iPCount;
            pItem = zh_arrayGetItemPtr( pArray, ++ulParamOffset );
         }
      }

      if( pSelf )
         zh_vmSend( ( ZH_USHORT ) iPCount );
      else
         zh_vmProc( ( ZH_USHORT ) iPCount );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1099, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_BOOL zh_execFromArray( PZH_ITEM pParam )
{
   PZH_ITEM pArray = NULL;
   PZH_ITEM pSelf = NULL;
   ZH_ULONG ulParamOffset = 0;

   if( pParam && ZH_IS_ARRAY( pParam ) && ! ZH_IS_OBJECT( pParam ) )
   {
      pArray = pParam;
      pParam = zh_arrayGetItemPtr( pArray, 1 );
      if( ZH_IS_OBJECT( pParam ) )
      {
         pSelf = pParam;
         pParam = zh_arrayGetItemPtr( pArray, 2 );
         ulParamOffset = 2;
      }
      else
         ulParamOffset = 1;
   }

   if( pParam )
   {
      PZH_SYMBOL pExecSym = NULL;

      if( ZH_IS_SYMBOL( pParam ) )
         pExecSym = zh_itemGetSymbol( pParam );
      else if( ZH_IS_STRING( pParam ) )
         pExecSym = zh_dynsymGet( zh_itemGetCPtr( pParam ) )->pSymbol;
      else if( ZH_IS_BLOCK( pParam ) && ! pSelf )
      {
         pSelf = pParam;
         pExecSym = &zh_symEval;
      }

      if( pExecSym )
      {
         int iPCount = 0;

         zh_vmPushSymbol( pExecSym );
         if( pSelf )
            zh_vmPush( pSelf );
         else
            zh_vmPushNil();

         if( pArray )
         {
            pParam = zh_arrayGetItemPtr( pArray, ++ulParamOffset );
            while( pParam && iPCount < 255 )
            {
               zh_vmPush( pParam );
               ++iPCount;
               pParam = zh_arrayGetItemPtr( pArray, ++ulParamOffset );
            }
         }

         if( pSelf )
            zh_vmSend( ( ZH_USHORT ) iPCount );
         else
            zh_vmProc( ( ZH_USHORT ) iPCount );

         return ZH_TRUE;
      }
   }

   zh_errRT_BASE_SubstR( EG_ARG, 1099, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );

   return ZH_FALSE;
}

/* zh_ExecMsg( <sFuncSym>, <object>, [<params,...>] ) --> <xResult>
 * Execute <sFuncSym> with <object> set as QSELF() value
 */
ZH_FUNC( ZH_EXECMSG )
{
   int iParams = zh_pcount();

   if( iParams >= 2 && ZH_ISSYMBOL( 1 ) )
   {
      PZH_ITEM pBase = zh_stackBaseItem();
      pBase->item.asSymbol.paramcnt = pBase->item.asSymbol.paramdeclcnt = 0;
      zh_vmProc( ( ZH_USHORT ) ( iParams - 2 ) );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1099, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}
