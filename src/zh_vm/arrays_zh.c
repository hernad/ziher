/*
 * The Array API (Ziher level)
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

#include "zh_api.h"
#include "zh_stack.h"
#include "zh_item_api.h"
#include "zh_error_api.h"
#include "zh_lang_api.h"

/* This function creates an array item using 'iDimension' as an index
 * to retrieve the number of elements from the parameter list.
 */
static void zh_arrayNewRagged( PZH_ITEM pArray, int iDimension )
{
   ZH_SIZE nElements;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayNewRagged(%p, %d)", pArray, iDimension ) );

   nElements = zh_parns( iDimension );

   /* create an array */
   zh_arrayNew( pArray, nElements );

   if( ++iDimension <= zh_pcount() )
   {
      /* call self recursively to create next dimensions
       */
      while( nElements )
         zh_arrayNewRagged( zh_arrayGetItemPtr( pArray, nElements-- ), iDimension );
   }
}

ZH_FUNC( ARRAY )
{
   int iPCount = zh_pcount();

   if( iPCount > 0 )
   {
      ZH_BOOL bError = ZH_FALSE;
      int iParam;

      for( iParam = 1; iParam <= iPCount; iParam++ )
      {
         if( ! ZH_IS_PARAM_NUM( iParam ) )
         {
            bError = ZH_TRUE;
            break;
         }

         if( zh_parns( iParam ) < 0 ) /* || zh_parns( iParam ) <= 4096 */
         {
            zh_errRT_BASE( EG_BOUND, 1131, NULL, zh_langDGetErrorDesc( EG_ARRDIMENSION ), ZH_ERR_ARGS_BASEPARAMS );
            bError = ZH_TRUE;
            break;
         }
      }

      if( ! bError )
         zh_arrayNewRagged( zh_stackReturnItem(), 1 );
   }
}

ZH_FUNC( AADD )
{
   PZH_ITEM pArray = zh_param( 1, ZH_IT_ARRAY );

   if( pArray )
   {
      PZH_ITEM pValue = zh_param( 2, ZH_IT_ANY );

      if( pValue && zh_arrayAdd( pArray, pValue ) )
         zh_itemReturn( pValue );
      else
         zh_errRT_BASE( EG_BOUND, 1187, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1123, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}


ZH_FUNC( ASIZE )
{
   PZH_ITEM pArray = zh_param( 1, ZH_IT_ARRAY );

   if( pArray && ZH_IS_PARAM_NUM( 2 ) )
   {
      ZH_I_SIZE nSize = zh_parns( 2 );

      zh_arraySize( pArray, ZH_MAX( nSize, 0 ) );

      zh_itemReturn( pArray ); /* ASize() returns the array itself */
   }
   else

      zh_errRT_BASE( EG_ARG, 2023, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );

}

ZH_FUNC( ATAIL )
{
   PZH_ITEM pArray = zh_param( 1, ZH_IT_ARRAY );

   if( pArray )
      zh_arrayLast( pArray, zh_stackReturnItem() );
}

ZH_FUNC( AINS )
{
   PZH_ITEM pArray = zh_param( 1, ZH_IT_ARRAY );

   if( pArray )
   {
      ZH_I_SIZE nPos = zh_parns( 2 );

      if( nPos == 0 )
         nPos = 1;

      zh_arrayIns( pArray, nPos );

      zh_itemReturn( pArray ); /* AIns() returns the array itself */
   }
}

ZH_FUNC( ADEL )
{
   PZH_ITEM pArray = zh_param( 1, ZH_IT_ARRAY );

   if( pArray )
   {
      ZH_I_SIZE nPos = zh_parns( 2 );

      if( nPos == 0 )
         nPos = 1;

      zh_arrayDel( pArray, nPos );

      zh_itemReturn( pArray ); /* ADel() returns the array itself */
   }
}

ZH_FUNC( AFILL )
{
   PZH_ITEM pArray = zh_param( 1, ZH_IT_ARRAY );

   if( pArray )
   {
      PZH_ITEM pValue = zh_param( 2, ZH_IT_ANY );

      zh_itemReturn( pArray ); /* AFill() returns the array itself */

      if( pValue )
      {
         ZH_SIZE nStart, nCount;
         ZH_I_SIZE lStart = zh_parns( 3 ), lCount = zh_parns( 4 );

         /* Explicit lCount of 0 - Nothing to do! */
         if( ZH_IS_PARAM_NUM( 4 ) && lCount == 0 )
            return;
         else if( lStart < 0 )
            return;
         else if( lStart == 0 )
            lStart = 1;
         if( lCount < 0 )
         {
            if( lStart == 1 )
               nCount = 0;
            else
               return;
         }
         nStart = ( ZH_SIZE ) lStart;
         nCount = ( ZH_SIZE ) lCount;
         zh_arrayFill( pArray,
                       pValue,
                       ZH_IS_PARAM_NUM( 3 ) ? &nStart : NULL,
                       ZH_IS_PARAM_NUM( 4 ) ? &nCount : NULL );
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 6004, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ASCAN )
{
   PZH_ITEM pArray = zh_param( 1, ZH_IT_ARRAY );
   PZH_ITEM pValue = zh_param( 2, ZH_IT_ANY );

   if( pArray && pValue )
   {
      ZH_SIZE nStart = zh_parns( 3 );
      ZH_SIZE nCount = zh_parns( 4 );

      zh_retns( zh_arrayScanCase( pArray, pValue,
                                  ZH_IS_PARAM_NUM( 3 ) ? &nStart : NULL,
                                  ZH_IS_PARAM_NUM( 4 ) ? &nCount : NULL,
                                  ZH_FALSE,
                                  ZH_TRUE ) );
   }
   else
      zh_retni( 0 );
}

/* Same as AScan() but has an additional parameter to force exact comparison. */
ZH_FUNC( ZH_ASCAN )
{
   PZH_ITEM pArray = zh_param( 1, ZH_IT_ARRAY );
   PZH_ITEM pValue = zh_param( 2, ZH_IT_ANY );

   if( pArray && pValue )
   {
      ZH_SIZE nStart = zh_parns( 3 );
      ZH_SIZE nCount = zh_parns( 4 );

      zh_retns( zh_arrayScanCase( pArray, pValue,
                                  ZH_IS_PARAM_NUM( 3 ) ? &nStart : NULL,
                                  ZH_IS_PARAM_NUM( 4 ) ? &nCount : NULL,
                                  zh_parl( 5 ),
                                  ZH_TRUE ) );
   }
   else
      zh_retni( 0 );
}

/* Same as zh_AScan() but with case-insensitive string comparison. */
ZH_FUNC( ZH_ASCANI )
{
   PZH_ITEM pArray = zh_param( 1, ZH_IT_ARRAY );
   PZH_ITEM pValue = zh_param( 2, ZH_IT_ANY );

   if( pArray && pValue )
   {
      ZH_SIZE nStart = zh_parns( 3 );
      ZH_SIZE nCount = zh_parns( 4 );

      zh_retns( zh_arrayScanCase( pArray, pValue,
                                  ZH_IS_PARAM_NUM( 3 ) ? &nStart : NULL,
                                  ZH_IS_PARAM_NUM( 4 ) ? &nCount : NULL,
                                  zh_parl( 5 ),
                                  ZH_FALSE ) );
   }
   else
      zh_retni( 0 );
}

ZH_FUNC( ZH_RASCAN )
{
   PZH_ITEM pArray = zh_param( 1, ZH_IT_ARRAY );
   PZH_ITEM pValue = zh_param( 2, ZH_IT_ANY );

   if( pArray && pValue )
   {
      ZH_SIZE nStart = zh_parns( 3 );
      ZH_SIZE nCount = zh_parns( 4 );

      zh_retns( zh_arrayRevScan( pArray, pValue,
                                 ZH_IS_PARAM_NUM( 3 ) ? &nStart : NULL,
                                 ZH_IS_PARAM_NUM( 4 ) ? &nCount : NULL,
                                 zh_parl( 5 ) ) );
   }
   else
      zh_retni( 0 );
}

ZH_FUNC( ZH_AINS )
{
   PZH_ITEM pArray = zh_param( 1, ZH_IT_ARRAY );

   if( pArray )
   {
      ZH_I_SIZE nPos = zh_parns( 2 );

      if( nPos == 0 )
         nPos = 1;

      if( zh_parl( 4 ) )
      {
         ZH_SIZE nLen = zh_arrayLen( pArray ) + 1;
         if( nPos >= 1 && ( ZH_SIZE ) nPos <= nLen )
            zh_arraySize( pArray, nLen );
      }

      if( zh_arrayIns( pArray, nPos ) )
      {
         if( ! ZH_ISNIL( 3 ) )
            zh_arraySet( pArray, nPos, zh_param( 3, ZH_IT_ANY ) );
      }

      zh_itemReturn( pArray ); /* AIns() returns the array itself */
   }
}

ZH_FUNC( ZH_ADEL )
{
   PZH_ITEM pArray = zh_param( 1, ZH_IT_ARRAY );

   if( pArray )
   {
      ZH_I_SIZE nPos = zh_parns( 2 );

      if( nPos == 0 )
         nPos = 1;

      if( zh_arrayDel( pArray, nPos ) )
      {
         if( zh_parl( 3 ) )
            zh_arraySize( pArray, zh_arrayLen( pArray ) - 1 );
      }

      zh_itemReturn( pArray ); /* ADel() returns the array itself */
   }
}

/* TODO: In Xbase++ fifth parameter determines whether array elements
         are passed by reference to the code block. [vszakats] */

ZH_FUNC( AEVAL )
{
   PZH_ITEM pArray = zh_param( 1, ZH_IT_ARRAY );
   PZH_ITEM pBlock = zh_param( 2, ZH_IT_BLOCK );

   if( pArray && pBlock )
   {
      ZH_SIZE nStart = zh_parns( 3 );
      ZH_SIZE nCount = zh_parns( 4 );

      zh_arrayEval( pArray,
                    pBlock,
                    ZH_IS_PARAM_NUM( 3 ) ? &nStart : NULL,
                    ZH_IS_PARAM_NUM( 4 ) ? &nCount : NULL );

      zh_itemReturn( pArray ); /* AEval() returns the array itself */
   }
   else
      zh_errRT_BASE( EG_ARG, 2017, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ACOPY )
{
   PZH_ITEM pSrcArray = zh_param( 1, ZH_IT_ARRAY );
   PZH_ITEM pDstArray = zh_param( 2, ZH_IT_ARRAY );

   if( pSrcArray && pDstArray )
   {
      if( ! zh_arrayIsObject( pSrcArray ) && ! zh_arrayIsObject( pDstArray ) )
      {
         ZH_SIZE nStart = zh_parns( 3 );
         ZH_SIZE nCount = zh_parns( 4 );
         ZH_SIZE nTarget = zh_parns( 5 );

         zh_arrayCopy( pSrcArray,
                       pDstArray,
                       ZH_IS_PARAM_NUM( 3 ) ? &nStart : NULL,
                       ZH_IS_PARAM_NUM( 4 ) ? &nCount : NULL,
                       ZH_IS_PARAM_NUM( 5 ) ? &nTarget : NULL );
      }

      zh_itemReturn( pDstArray ); /* ACopy() returns the target array */
   }
}

ZH_FUNC( ACLONE )
{
   PZH_ITEM pSrcArray = zh_param( 1, ZH_IT_ARRAY );

   if( pSrcArray && ! zh_arrayIsObject( pSrcArray ) )
      zh_arrayCloneTo( zh_stackReturnItem(), pSrcArray ); /* AClone() returns the new array */
}

ZH_FUNC( ZH_APARAMS )
{
   zh_itemReturnRelease( zh_arrayFromParams( zh_parni( 1 ) + 1 ) );
}
