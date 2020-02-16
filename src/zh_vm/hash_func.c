/*
 * The Hash tables API (PRG level)
 *
 * Copyright 2007 Przemyslaw Czerpak
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
#include "zh_item_api.h"
#include "zh_api_error.h"
#include "zh_lang_api.h"
#include "zh_vm.h"
#include "zh_stack.h"

ZH_FUNC( ZH_HASH )
{
   int iPCount = zh_pcount();

   if( iPCount & 1 )
      zh_errRT_BASE( EG_BOUND, 1131, NULL, zh_langDGetErrorDesc( EG_ARRDIMENSION ), ZH_ERR_ARGS_BASEPARAMS );
   else
   {
      PZH_ITEM pHash = zh_hashNew( NULL );
      int iParam;
      for( iParam = 1; iParam <= iPCount; iParam += 2 )
      {
         PZH_ITEM pKey = zh_param( iParam, ZH_IT_HASHKEY );
         PZH_ITEM pValue = zh_param( iParam + 1, ZH_IT_ANY );
         if( pKey )
            zh_hashAdd( pHash, pKey, pValue );
         else
         {
            zh_errRT_BASE( EG_BOUND, 1133, NULL, zh_langDGetErrorDesc( EG_ARRASSIGN ), 3, pHash, zh_param( iParam, ZH_IT_ANY ), pValue );
            break;
         }
      }
      zh_itemReturnRelease( pHash );
   }
}

ZH_FUNC( ZH_HHASKEY )
{
   PZH_ITEM pHash = zh_param( 1, ZH_IT_HASH );
   PZH_ITEM pKey = zh_param( 2, ZH_IT_HASHKEY );

   if( pHash && pKey )
   {
      ZH_SIZE nPos;
      zh_retl( zh_hashScanSoft( pHash, pKey, &nPos ) );
      zh_storns( nPos, 3 );
   }
   else
      zh_errRT_BASE( EG_ARG, 1123, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_HPOS )
{
   PZH_ITEM pHash = zh_param( 1, ZH_IT_HASH );
   PZH_ITEM pKey = zh_param( 2, ZH_IT_HASHKEY );

   if( pHash && pKey )
   {
      ZH_SIZE nPos;
      zh_hashScan( pHash, pKey, &nPos );
      zh_retns( nPos );
   }
   else
      zh_errRT_BASE( EG_ARG, 1123, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_HGET )
{
   PZH_ITEM pHash = zh_param( 1, ZH_IT_HASH );
   PZH_ITEM pKey = zh_param( 2, ZH_IT_HASHKEY );

   if( pHash && pKey )
   {
      PZH_ITEM pDest = zh_hashGetItemPtr( pHash, pKey, ZH_HASH_AUTOADD_ACCESS );
      if( pDest )
         zh_itemReturn( pDest );
      else
         zh_errRT_BASE( EG_BOUND, 1132, NULL, zh_langDGetErrorDesc( EG_ARRACCESS ), 2, pHash, pKey );
   }
   else
      zh_errRT_BASE( EG_ARG, 1123, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_HGETDEF )
{
   PZH_ITEM pHash = zh_param( 1, ZH_IT_HASH );
   PZH_ITEM pKey = zh_param( 2, ZH_IT_HASHKEY );

   if( pHash && pKey )
   {
      PZH_ITEM pDest = zh_hashGetItemPtr( pHash, pKey, ZH_HASH_AUTOADD_ACCESS );
      if( pDest )
         zh_itemReturn( pDest );
      else
      {
         PZH_ITEM pDefault = zh_param( 3, ZH_IT_ANY );
         if( pDefault )
            zh_itemReturn( pDefault );
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 1123, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_HGETREF )
{
   PZH_ITEM pHash = zh_param( 1, ZH_IT_HASH );
   PZH_ITEM pKey = zh_param( 2, ZH_IT_HASHKEY );

   if( pHash && pKey )
   {
      PZH_ITEM pDest = zh_hashGetItemPtr( pHash, pKey, ZH_HASH_AUTOADD_ACCESS );
      zh_itemParamStore( 3, pDest );
      zh_retl( pDest != NULL );
   }
   else
      zh_retl( ZH_FALSE );
}

ZH_FUNC( ZH_HSET )
{
   PZH_ITEM pHash = zh_param( 1, ZH_IT_HASH );
   PZH_ITEM pKey = zh_param( 2, ZH_IT_HASHKEY );
   PZH_ITEM pValue = zh_param( 3, ZH_IT_ANY );

   if( pHash && pKey && pValue )
   {
      zh_hashAdd( pHash, pKey, pValue );
      zh_itemReturn( pHash );
   }
   else
      zh_errRT_BASE( EG_ARG, 1123, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_HDEL )
{
   PZH_ITEM pHash = zh_param( 1, ZH_IT_HASH );
   PZH_ITEM pKey = zh_param( 2, ZH_IT_HASHKEY );

   if( pHash && pKey )
   {
      zh_hashDel( pHash, pKey );
      zh_itemReturn( pHash );
   }
   else
      zh_errRT_BASE( EG_ARG, 1123, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_HKEYAT )
{
   PZH_ITEM pHash = zh_param( 1, ZH_IT_HASH );
   PZH_ITEM pPos = zh_param( 2, ZH_IT_NUMERIC );

   if( pHash && pPos )
   {
      PZH_ITEM pKey = zh_hashGetKeyAt( pHash, zh_itemGetNS( pPos ) );
      if( pKey )
         zh_itemReturn( pKey );
      else
         zh_errRT_BASE( EG_BOUND, 1187, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
   }
   else
      zh_errRT_BASE( EG_ARG, 1123, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_HVALUEAT )
{
   PZH_ITEM pHash = zh_param( 1, ZH_IT_HASH );
   PZH_ITEM pPos = zh_param( 2, ZH_IT_NUMERIC );
   PZH_ITEM pValue = zh_param( 3, ZH_IT_ANY );

   if( pHash && pPos )
   {
      PZH_ITEM pItem = zh_hashGetValueAt( pHash, zh_itemGetNS( pPos ) );
      if( pItem )
      {
         if( pValue )
            zh_itemCopy( pItem, pValue );
         else
            pValue = pItem;
         zh_itemReturn( pValue );
      }
      else
         zh_errRT_BASE( EG_BOUND, 1187, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
   }
   else
      zh_errRT_BASE( EG_ARG, 1123, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_HPAIRAT )
{
   PZH_ITEM pHash = zh_param( 1, ZH_IT_HASH );
   PZH_ITEM pPos = zh_param( 2, ZH_IT_NUMERIC );

   if( pHash && pPos )
   {
      PZH_ITEM pKey = zh_hashGetKeyAt( pHash, zh_itemGetNS( pPos ) );
      PZH_ITEM pValue = zh_hashGetValueAt( pHash, zh_itemGetNS( pPos ) );
      if( pKey && pValue )
      {
         PZH_ITEM pDstKey = zh_param( 3, ZH_IT_BYREF );
         PZH_ITEM pDstVal = zh_param( 4, ZH_IT_BYREF );
         if( pDstKey && pDstVal )
         {
            zh_itemCopy( pDstKey, pKey );
            zh_itemCopy( pDstVal, pValue );
         }
         else
         {
            PZH_ITEM pResult = zh_itemArrayNew( 2 );
            zh_arraySet( pResult, 1, pKey );
            zh_arraySet( pResult, 2, pValue );
            zh_itemReturnRelease( pResult );
         }
      }
      else
         zh_errRT_BASE( EG_BOUND, 1187, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
   }
   else
      zh_errRT_BASE( EG_ARG, 1123, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_HDELAT )
{
   PZH_ITEM pHash = zh_param( 1, ZH_IT_HASH );
   PZH_ITEM pPos = zh_param( 2, ZH_IT_NUMERIC );

   if( pHash && pPos )
   {
      if( zh_hashDelAt( pHash, zh_itemGetNS( pPos ) ) )
         zh_itemReturn( pHash );
      else
         zh_errRT_BASE( EG_BOUND, 1133, NULL, zh_langDGetErrorDesc( EG_ARRASSIGN ), 2, pHash, pPos );
   }
   else
      zh_errRT_BASE( EG_ARG, 1123, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}


ZH_FUNC( ZH_HKEYS )
{
   PZH_ITEM pHash = zh_param( 1, ZH_IT_HASH );

   if( pHash )
      zh_itemReturnRelease( zh_hashGetKeys( pHash ) );
   else
      zh_errRT_BASE( EG_ARG, 1123, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_HVALUES )
{
   PZH_ITEM pHash = zh_param( 1, ZH_IT_HASH );

   if( pHash )
      zh_itemReturnRelease( zh_hashGetValues( pHash ) );
   else
      zh_errRT_BASE( EG_ARG, 1123, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_HCLEAR )
{
   PZH_ITEM pHash = zh_param( 1, ZH_IT_HASH );

   if( pHash )
   {
      zh_hashClear( pHash );
      zh_itemReturn( pHash );
   }
   else
      zh_errRT_BASE( EG_ARG, 1123, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_HFILL )
{
   PZH_ITEM pHash = zh_param( 1, ZH_IT_HASH );
   PZH_ITEM pValue = zh_param( 2, ZH_IT_ANY );

   if( pHash && pValue )
   {
      PZH_ITEM pDest;
      ZH_SIZE nPos = 0;

      while( ( pDest = zh_hashGetValueAt( pHash, ++nPos ) ) != NULL )
         zh_itemCopy( pDest, pValue );

      zh_itemReturn( pHash );
   }
   else
      zh_errRT_BASE( EG_ARG, 1123, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_HCLONE )
{
   PZH_ITEM pHash = zh_param( 1, ZH_IT_HASH );

   if( pHash )
      zh_hashCloneTo( zh_stackReturnItem(), pHash );
   else
      zh_errRT_BASE( EG_ARG, 1123, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_HCOPY )
{
   PZH_ITEM pSource = zh_param( 1, ZH_IT_HASH );
   PZH_ITEM pDest = zh_param( 2, ZH_IT_HASH );

   if( pSource && pDest )
   {
      ZH_SIZE nLen = zh_hashLen( pSource ), nStart, nCount;

      nStart = zh_parns( 3 );
      if( ! nStart )
         ++nStart;
      nCount = ZH_IS_PARAM_NUM( 4 ) ? ( ZH_SIZE ) zh_parns( 4 ) : nLen - nStart + 1;

      while( nCount-- )
      {
         PZH_ITEM pKey = zh_hashGetKeyAt( pSource, nStart );
         PZH_ITEM pValue = zh_hashGetValueAt( pSource, nStart );
         if( pKey && pValue )
            zh_hashAdd( pDest, pKey, pValue );
         else
            break;
         ++nStart;
      }

      zh_itemReturn( pDest );
   }
   else
      zh_errRT_BASE( EG_ARG, 1123, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_HMERGE )
{
   PZH_ITEM pDest = zh_param( 1, ZH_IT_HASH );
   PZH_ITEM pSource = zh_param( 2, ZH_IT_HASH );

   if( pDest && pSource )
   {
      PZH_ITEM pAction = zh_param( 3, ZH_IT_EVALITEM | ZH_IT_NUMERIC );

      if( pAction && ZH_IS_EVALITEM( pAction ) )
      {
         ZH_SIZE nLen = zh_hashLen( pSource ), nPos = 0;
         while( ++nPos <= nLen )
         {
            PZH_ITEM pKey = zh_hashGetKeyAt( pSource, nPos );
            PZH_ITEM pValue = zh_hashGetValueAt( pSource, nPos );
            if( pKey && pValue )
            {
               zh_vmPushEvalSym();
               zh_vmPush( pAction );
               zh_vmPush( pKey );
               zh_vmPush( pValue );
               zh_vmPushSize( nPos );
               zh_vmSend( 3 );
               {
                  PZH_ITEM pReturn = zh_stackReturnItem();
                  if( ZH_IS_LOGICAL( pReturn ) && zh_itemGetL( pReturn ) )
                     zh_hashAdd( pDest, pKey, pValue );
               }
            }
            else
               break;
         }
      }
      else
         zh_hashJoin( pDest, pSource, pAction ? zh_itemGetNI( pAction ) : ZH_HASH_UNION );

      zh_itemReturn( pDest );
   }
   else
      zh_errRT_BASE( EG_ARG, 1123, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_HEVAL )
{
   PZH_ITEM pHash = zh_param( 1, ZH_IT_HASH );
   PZH_ITEM pBlock = zh_param( 2, ZH_IT_EVALITEM );

   if( pHash && pBlock )
   {
      ZH_SIZE nLen = zh_hashLen( pHash ), nStart, nCount;

      nStart = zh_parns( 3 );
      if( ! nStart )
         ++nStart;
      nCount = ZH_IS_PARAM_NUM( 4 ) ? ( ZH_SIZE ) zh_parns( 4 ) : nLen - nStart + 1;

      while( nCount-- )
      {
         PZH_ITEM pKey = zh_hashGetKeyAt( pHash, nStart );
         PZH_ITEM pValue = zh_hashGetValueAt( pHash, nStart );
         if( pKey && pValue )
         {
            zh_vmPushEvalSym();
            zh_vmPush( pBlock );
            zh_vmPush( pKey );
            zh_vmPush( pValue );
            zh_vmPushSize( nStart );
            zh_vmSend( 3 );
         }
         else
            break;
         ++nStart;
      }

      zh_itemReturn( pHash );
   }
   else
      zh_errRT_BASE( EG_ARG, 1123, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_HSCAN )
{
   PZH_ITEM pHash = zh_param( 1, ZH_IT_HASH );
   PZH_ITEM pValue = zh_param( 2, ZH_IT_ANY );

   if( pHash && pValue )
   {
      ZH_BOOL fExact = zh_parl( 5 ), fFound = ZH_FALSE;
      ZH_SIZE nLen = zh_hashLen( pHash ), nStart, nCount;

      nStart = zh_parns( 3 );
      if( ! nStart )
         ++nStart;
      nCount = ZH_IS_PARAM_NUM( 4 ) ? ( ZH_SIZE ) zh_parns( 4 ) : nLen - nStart + 1;

      if( ZH_IS_EVALITEM( pValue ) )
      {
         while( nCount-- )
         {
            PZH_ITEM pKey = zh_hashGetKeyAt( pHash, nStart );
            PZH_ITEM pVal = zh_hashGetValueAt( pHash, nStart );
            if( pKey && pValue )
            {
               zh_vmPushEvalSym();
               zh_vmPush( pValue );
               zh_vmPush( pKey );
               zh_vmPush( pVal );
               zh_vmPushSize( nStart );
               zh_vmSend( 3 );
               {
                  PZH_ITEM pReturn = zh_stackReturnItem();
                  if( ZH_IS_LOGICAL( pReturn ) && zh_itemGetL( pReturn ) )
                  {
                     fFound = ZH_TRUE;
                     break;
                  }
               }
            }
            else
               break;
            ++nStart;
         }
      }
      else if( ZH_IS_STRING( pValue ) )
      {
         while( nCount-- )
         {
            PZH_ITEM pItem = zh_hashGetValueAt( pHash, nStart );
            if( pItem )
            {
               if( ZH_IS_STRING( pItem ) && zh_itemStrCmp( pItem, pValue, fExact ) == 0 )
               {
                  fFound = ZH_TRUE;
                  break;
               }
            }
            else
               break;
            ++nStart;
         }
      }
      else if( ZH_IS_NUMINT( pValue ) )
      {
         ZH_MAXINT nValue = zh_itemGetNInt( pValue );
         while( nCount-- )
         {
            PZH_ITEM pItem = zh_hashGetValueAt( pHash, nStart );
            if( pItem )
            {
               if( ZH_IS_NUMERIC( pItem ) && zh_itemGetNInt( pItem ) == nValue &&
                   zh_itemGetND( pItem ) == ( double ) nValue )
               {
                  fFound = ZH_TRUE;
                  break;
               }
            }
            else
               break;
            ++nStart;
         }
      }
      else if( ZH_IS_NUMERIC( pValue ) )
      {
         double dValue = zh_itemGetND( pValue );
         while( nCount-- )
         {
            PZH_ITEM pItem = zh_hashGetValueAt( pHash, nStart );
            if( pItem )
            {
               if( ZH_IS_NUMERIC( pItem ) && zh_itemGetND( pItem ) == dValue )
               {
                  fFound = ZH_TRUE;
                  break;
               }
            }
            else
               break;
            ++nStart;
         }
      }
      else if( ZH_IS_DATETIME( pValue ) )
      {
         while( nCount-- )
         {
            PZH_ITEM pItem = zh_hashGetValueAt( pHash, nStart );
            if( pItem )
            {
               if( ZH_IS_DATETIME( pItem ) &&
                   pItem->item.asDateTime.julian == pValue->item.asDateTime.julian &&
                   ( ! fExact || pItem->item.asDateTime.time == pValue->item.asDateTime.time ) )
               {
                  fFound = ZH_TRUE;
                  break;
               }
            }
            else
               break;
            ++nStart;
         }
      }
      else if( ZH_IS_LOGICAL( pValue ) )
      {
         ZH_BOOL fValue = zh_itemGetDL( pValue );
         while( nCount-- )
         {
            PZH_ITEM pItem = zh_hashGetValueAt( pHash, nStart );
            if( pItem )
            {
               if( ZH_IS_LOGICAL( pItem ) && zh_itemGetL( pItem ) == fValue )
               {
                  fFound = ZH_TRUE;
                  break;
               }
            }
            else
               break;
            ++nStart;
         }
      }
      else if( ZH_IS_NIL( pValue ) )
      {
         while( nCount-- )
         {
            PZH_ITEM pItem = zh_hashGetValueAt( pHash, nStart );
            if( pItem )
            {
               if( ZH_IS_NIL( pItem ) )
               {
                  fFound = ZH_TRUE;
                  break;
               }
            }
            else
               break;
            ++nStart;
         }
      }
      else if( ZH_IS_POINTER( pValue ) )
      {
         while( nCount-- )
         {
            PZH_ITEM pItem = zh_hashGetValueAt( pHash, nStart );
            if( pItem )
            {
               if( ZH_IS_POINTER( pItem ) &&
                   pItem->item.asPointer.value == pValue->item.asPointer.value )
               {
                  fFound = ZH_TRUE;
                  break;
               }
            }
            else
               break;
            ++nStart;
         }
      }
      else if( fExact && ZH_IS_ARRAY( pValue ) )
      {
         while( nCount-- )
         {
            PZH_ITEM pItem = zh_hashGetValueAt( pHash, nStart );
            if( pItem )
            {
               if( ZH_IS_ARRAY( pItem ) &&
                   pItem->item.asArray.value == pValue->item.asArray.value )
               {
                  fFound = ZH_TRUE;
                  break;
               }
            }
            else
               break;
            ++nStart;
         }
      }
      else if( fExact && ZH_IS_HASH( pValue ) )
      {
         while( nCount-- )
         {
            PZH_ITEM pItem = zh_hashGetValueAt( pHash, nStart );
            if( pItem )
            {
               if( ZH_IS_HASH( pItem ) &&
                   pItem->item.asHash.value == pValue->item.asHash.value )
               {
                  fFound = ZH_TRUE;
                  break;
               }
            }
            else
               break;
            ++nStart;
         }
      }

      zh_retns( fFound ? nStart : 0 );
   }
   else
      zh_errRT_BASE( EG_ARG, 1123, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_HSORT )
{
   PZH_ITEM pHash = zh_param( 1, ZH_IT_HASH );

   if( pHash )
   {
      zh_hashSort( pHash );
      zh_itemReturn( pHash );
   }
   else
      zh_errRT_BASE( EG_ARG, 2017, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_HCASEMATCH )
{
   PZH_ITEM pHash = zh_param( 1, ZH_IT_HASH );

   if( pHash )
   {
      PZH_ITEM pValue = zh_param( 2, ZH_IT_LOGICAL );
      int iFlags = zh_hashGetFlags( pHash );

      zh_retl( ( iFlags & ZH_HASH_IGNORECASE ) == 0 );

      if( pValue )
      {
         if( zh_itemGetL( pValue ) )
         {
            if( ( iFlags & ZH_HASH_IGNORECASE ) != 0 )
            {
               zh_hashClearFlags( pHash, ZH_HASH_IGNORECASE );
               zh_hashSetFlags( pHash, ZH_HASH_RESORT );
            }
         }
         else if( ( iFlags & ZH_HASH_IGNORECASE ) == 0 )
         {
            zh_hashClearFlags( pHash, ZH_HASH_BINARY );
            zh_hashSetFlags( pHash, ZH_HASH_IGNORECASE | ZH_HASH_RESORT );
         }
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2017, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_HBINARY )
{
   PZH_ITEM pHash = zh_param( 1, ZH_IT_HASH );

   if( pHash )
   {
      PZH_ITEM pValue = zh_param( 2, ZH_IT_LOGICAL );
      int iFlags = zh_hashGetFlags( pHash );

      zh_retl( ( iFlags & ZH_HASH_BINARY ) != 0 );

      if( pValue )
      {
         if( zh_itemGetL( pValue ) )
         {
            if( ( iFlags & ZH_HASH_BINARY ) == 0 )
            {
               zh_hashClearFlags( pHash, ZH_HASH_IGNORECASE );
               zh_hashSetFlags( pHash, ZH_HASH_BINARY | ZH_HASH_RESORT );
            }
         }
         else if( ( iFlags & ZH_HASH_BINARY ) != 0 )
         {
            zh_hashClearFlags( pHash, ZH_HASH_BINARY );
            zh_hashSetFlags( pHash, ZH_HASH_RESORT );
         }
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2017, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_HAUTOADD )
{
   PZH_ITEM pHash = zh_param( 1, ZH_IT_HASH );

   if( pHash )
   {
      PZH_ITEM pValue = zh_param( 2, ZH_IT_LOGICAL | ZH_IT_NUMERIC );
      int iOldFlags = zh_hashGetFlags( pHash ) & ZH_HASH_AUTOADD_MASK;

      zh_retni( iOldFlags );

      if( zh_pcount() >= 3 )
         zh_hashSetDefault( pHash, zh_param( 3, ZH_IT_ANY ) );

      if( pValue )
      {
         if( ZH_IS_LOGICAL( pValue ) )
         {
            if( zh_itemGetL( pValue ) )
               zh_hashSetFlags( pHash, zh_hashGetDefault( pHash ) ?
                           ZH_HASH_AUTOADD_ALWAYS : ZH_HASH_AUTOADD_ASSIGN );
            else if( iOldFlags )
               zh_hashClearFlags( pHash, iOldFlags );
         }
         else
         {
            int iNewFlags = zh_itemGetNI( pValue );
            if( ( iNewFlags | iOldFlags ) != iNewFlags )
               zh_hashClearFlags( pHash, iOldFlags );
            if( iNewFlags )
               zh_hashSetFlags( pHash, iNewFlags );
         }
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2017, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_HKEEPORDER )
{
   PZH_ITEM pHash = zh_param( 1, ZH_IT_HASH );

   if( pHash )
   {
      PZH_ITEM pValue = zh_param( 2, ZH_IT_LOGICAL );
      int iFlags = zh_hashGetFlags( pHash );

      zh_retl( ( iFlags & ZH_HASH_KEEPORDER ) != 0 );

      if( pValue )
      {
         if( zh_itemGetL( pValue ) )
         {
            if( ( iFlags & ZH_HASH_KEEPORDER ) == 0 )
               zh_hashSetFlags( pHash, ZH_HASH_KEEPORDER );
         }
         else
         {
            if( ( iFlags & ZH_HASH_KEEPORDER ) != 0 )
               zh_hashClearFlags( pHash, ZH_HASH_KEEPORDER );
         }
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2017, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_HALLOCATE )
{
   PZH_ITEM pHash = zh_param( 1, ZH_IT_HASH );
   PZH_ITEM pValue = zh_param( 2, ZH_IT_NUMERIC );

   if( pHash && pValue )
   {
      ZH_ISIZ nMem = zh_itemGetNS( pValue );
      if( nMem >= 0 )
         zh_hashPreallocate( pHash, nMem );
   }
   else
      zh_errRT_BASE( EG_ARG, 2017, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_HDEFAULT )
{
   PZH_ITEM pHash = zh_param( 1, ZH_IT_HASH );

   if( pHash )
   {
      zh_itemReturn( zh_hashGetDefault( pHash ) );
      if( zh_pcount() > 1 )
         zh_hashSetDefault( pHash, zh_param( 2, ZH_IT_ANY ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2017, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

