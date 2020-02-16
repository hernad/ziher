/*
 * ASort() function
 *
 * Copyright 1999-2001 Viktor Szakats
 * Copyright 1999-2001 Jose Lalin <dezac@corevia.com>
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

/* FIXME: The sorting engine requires signed indexes to work, this means
          that arrays larger than 2^31 elements cannot be sorted. [vszakats] */

/* NOTE: Based on PD code found in
         SORTING AND SEARCHING ALGORITHMS: A COOKBOOK, BY THOMAS NIEMANN
         https://www.cs.auckland.ac.nz/~jmor159/PLDS210/niemann/s_man.htm */

#include "zh_vm_int.h"
#include "zh_api.h"
#include "zh_item_api.h"
#include "zh_vm.h"

static ZH_BOOL zh_itemIsLess( PZH_BASEARRAY pBaseArray, PZH_ITEM pBlock,
                              ZH_SIZE nItem1, ZH_SIZE nItem2 )
{
   PZH_ITEM pItem1 = pBaseArray->pItems + nItem1,
            pItem2 = pBaseArray->pItems + nItem2;

   if( pBlock )
   {
      PZH_ITEM pRet;

      /* protection against array resizing by user codeblock */
      if( pBaseArray->nLen <= nItem1 || pBaseArray->nLen <= nItem2 )
         return ZH_FALSE;

      zh_vmPushEvalSym();
      zh_vmPush( pBlock );
      zh_vmPush( pItem1 );
      zh_vmPush( pItem2 );
      zh_vmSend( 2 );

      pRet = zh_param( -1, ZH_IT_ANY );

      /* CA-Cl*pper always takes return value as logical item
       * accepting 0, 1 as numeric representation of ZH_FALSE/ZH_TRUE
       */
      return ( ZH_IS_LOGICAL( pRet ) || ZH_IS_NUMERIC( pRet ) ) ?
             zh_itemGetL( pRet ) : ZH_TRUE;
   }

   /* Do native compare when no codeblock is supplied */

   if( ZH_IS_STRING( pItem1 ) && ZH_IS_STRING( pItem2 ) )
      return zh_itemStrCmp( pItem1, pItem2, ZH_FALSE ) < 0;
   else if( ZH_IS_NUMINT( pItem1 ) && ZH_IS_NUMINT( pItem2 ) )
      /* intentionally separate comparison for integer numbers
         to avoid precision lose in 64-bit integer to double conversion */
      return zh_itemGetNInt( pItem1 ) < zh_itemGetNInt( pItem2 );
   else if( ZH_IS_NUMERIC( pItem1 ) && ZH_IS_NUMERIC( pItem2 ) )
      return zh_itemGetND( pItem1 ) < zh_itemGetND( pItem2 );
   else if( ZH_IS_TIMESTAMP( pItem1 ) && ZH_IS_TIMESTAMP( pItem2 ) )
   {
      long lDate1, lTime1, lDate2, lTime2;

      zh_itemGetTDT( pItem1, &lDate1, &lTime1 );
      zh_itemGetTDT( pItem2, &lDate2, &lTime2 );
      return lDate1 == lDate2 ? lTime1 < lTime2 : lDate1 < lDate2;
   }
   else if( ZH_IS_DATETIME( pItem1 ) && ZH_IS_DATETIME( pItem2 ) )
      /* it's not exact comparison, compare only Julian date */
      return zh_itemGetDL( pItem1 ) < zh_itemGetDL( pItem2 );
   else if( ZH_IS_LOGICAL( pItem1 ) && ZH_IS_LOGICAL( pItem2 ) )
      return zh_itemGetL( pItem1 ) < zh_itemGetL( pItem2 );
   else
   {
      /* NOTE: For non-matching types CA-Cl*pper sorts always like this:
               Array/Object Block String Logical Date Numeric NIL [jlalin] */

      int iWeight1;
      int iWeight2;

      if( ZH_IS_ARRAY( pItem1 ) ) iWeight1 = 1;
      else if( ZH_IS_BLOCK( pItem1 ) ) iWeight1 = 2;
      else if( ZH_IS_STRING( pItem1 ) ) iWeight1 = 3;
      else if( ZH_IS_LOGICAL( pItem1 ) ) iWeight1 = 4;
      else if( ZH_IS_DATETIME( pItem1 ) ) iWeight1 = 5;
      else if( ZH_IS_NUMERIC( pItem1 ) ) iWeight1 = 6;
      else iWeight1 = 7;

      if( ZH_IS_ARRAY( pItem2 ) ) iWeight2 = 1;
      else if( ZH_IS_BLOCK( pItem2 ) ) iWeight2 = 2;
      else if( ZH_IS_STRING( pItem2 ) ) iWeight2 = 3;
      else if( ZH_IS_LOGICAL( pItem2 ) ) iWeight2 = 4;
      else if( ZH_IS_DATETIME( pItem2 ) ) iWeight2 = 5;
      else if( ZH_IS_NUMERIC( pItem2 ) ) iWeight2 = 6;
      else iWeight2 = 7;

      return iWeight1 < iWeight2;
   }
}


static ZH_BOOL zh_arraySortDO( PZH_BASEARRAY pBaseArray, PZH_ITEM pBlock,
                               ZH_SIZE * pSrc, ZH_SIZE * pBuf, ZH_SIZE nCount )
{
   if( nCount > 1 )
   {
      ZH_SIZE nCnt1, nCnt2, * pPtr1, * pPtr2, * pDst;
      ZH_BOOL fBuf1, fBuf2;

      nCnt1 = nCount >> 1;
      nCnt2 = nCount - nCnt1;
      pPtr1 = &pSrc[ 0 ];
      pPtr2 = &pSrc[ nCnt1 ];

      fBuf1 = zh_arraySortDO( pBaseArray, pBlock, pPtr1, &pBuf[ 0 ], nCnt1 );
      fBuf2 = zh_arraySortDO( pBaseArray, pBlock, pPtr2, &pBuf[ nCnt1 ], nCnt2 );
      if( fBuf1 )
         pDst = pBuf;
      else
      {
         pDst = pSrc;
         pPtr1 = &pBuf[ 0 ];
      }
      if( ! fBuf2 )
         pPtr2 = &pBuf[ nCnt1 ];

      while( nCnt1 > 0 && nCnt2 > 0 )
      {
         if( zh_itemIsLess( pBaseArray, pBlock, *pPtr2, *pPtr1 ) )
         {
            *pDst++ = *pPtr2++;
            nCnt2--;
         }
         else
         {
            *pDst++ = *pPtr1++;
            nCnt1--;
         }
      }
      if( nCnt1 > 0 )
      {
         do
            *pDst++ = *pPtr1++;
         while( --nCnt1 );
      }
      else if( nCnt2 > 0 && fBuf1 == fBuf2 )
      {
         do
            *pDst++ = *pPtr2++;
         while( --nCnt2 );
      }
      return ! fBuf1;
   }
   return ZH_TRUE;
}

static void zh_arraySortStart( PZH_BASEARRAY pBaseArray, PZH_ITEM pBlock,
                               ZH_SIZE nStart, ZH_SIZE nCount )
{
   ZH_SIZE * pBuffer, * pDest, * pPos, nPos, nTo;

   pBuffer = ( ZH_SIZE * ) zh_xgrab( sizeof( ZH_SIZE ) * 2 * nCount );
   for( nPos = 0; nPos < nCount; ++nPos )
      pBuffer[ nPos ] = nStart + nPos;

   if( zh_arraySortDO( pBaseArray, pBlock, pBuffer, &pBuffer[ nCount ], nCount ) )
      pPos = ( pDest = pBuffer ) + nCount;
   else
      pDest = ( pPos = pBuffer ) + nCount;

   /* protection against array resizing by user codeblock */
   if( nStart + nCount > pBaseArray->nLen )
   {
      if( pBaseArray->nLen > nStart )
      {
         for( nPos = nTo = 0; nPos < nCount; ++nPos )
         {
            if( pDest[ nPos ] < pBaseArray->nLen )
               pDest[ nTo++ ] = pDest[ nPos ];
         }
         nCount = nTo;
      }
      else
         nCount = 0;
   }

   for( nPos = 0; nPos < nCount; ++nPos )
      pPos[ pDest[ nPos ] - nStart ] = nPos;

   for( nPos = 0; nPos < nCount; ++nPos )
   {
      if( nPos + nStart != pDest[ nPos ] )
      {
         zh_itemRawSwap( pBaseArray->pItems + nPos + nStart,
                         pBaseArray->pItems + pDest[ nPos ] );
         pDest[ pPos[ nPos ] ] = pDest[ nPos ];
         pPos[ pDest[ nPos ] - nStart ] = pPos[ nPos ];
      }
   }

   zh_xfree( pBuffer );
}


ZH_BOOL zh_arraySort( PZH_ITEM pArray, ZH_SIZE * pnStart, ZH_SIZE * pnCount, PZH_ITEM pBlock )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arraySort(%p, %p, %p, %p)", ( void * ) pArray, ( void * ) pnStart, ( void * ) pnCount, ( void * ) pBlock ) );

   if( ZH_IS_ARRAY( pArray ) )
   {
      PZH_BASEARRAY pBaseArray = pArray->item.asArray.value;
      ZH_SIZE nLen = pBaseArray->nLen;
      ZH_SIZE nStart;

      if( pnStart && *pnStart >= 1 )
         nStart = *pnStart;
      else
         nStart = 1;

      if( nStart <= nLen )
      {
         ZH_SIZE nCount;

         if( pnCount && *pnCount >= 1 && ( *pnCount <= nLen - nStart ) )
            nCount = *pnCount;
         else
            nCount = nLen - nStart + 1;

         if( nStart + nCount > nLen )             /* check range */
            nCount = nLen - nStart + 1;

         /* Optimize when only one or no element is to be sorted */
         if( nCount > 1 )
            zh_arraySortStart( pBaseArray, pBlock, nStart - 1, nCount );
      }

      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}

ZH_FUNC( ASORT )
{
   PZH_ITEM pArray = zh_param( 1, ZH_IT_ARRAY );

   if( pArray && ! zh_arrayIsObject( pArray ) )
   {
      ZH_SIZE nStart = zh_parns( 2 );
      ZH_SIZE nCount = zh_parns( 3 );

      zh_arraySort( pArray,
                    ZH_IS_PARAM_NUM( 2 ) ? &nStart : NULL,
                    ZH_IS_PARAM_NUM( 3 ) ? &nCount : NULL,
                    zh_param( 4, ZH_IT_EVALITEM ) );

      zh_itemReturn( pArray ); /* ASort() returns the array itself */
   }
}
