/*
 * The Hash tables API (C level)
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

#ifndef _ZH_HASH_INTERNAL_
#define _ZH_HASH_INTERNAL_
#endif

#include "zh_vm_opt.h"
#include "zh_api.h"
#include "zh_item_api.h"
#include "zh_error_api.h"
#include "zh_lang_api.h"
#include "zh_vm.h"
#include "zh_xvm.h"
#include "zh_stack.h"

#define ZH_HASH_ITEM_ALLOC    16

/* internal structures for hashes */
typedef struct _ZH_HASHPAIR
{
   ZH_ITEM key;
   ZH_ITEM value;
} ZH_HASHPAIR, * PZH_HASHPAIR;

typedef struct _ZH_BASEHASH
{
   PZH_HASHPAIR pPairs;       /* pointer to the array of key/value pairs */
   PZH_ITEM     pDefault;     /* default autoadd value */
   ZH_SIZE *    pnPos;        /* the sort order for ZH_HASH_KEEPORDER */
   ZH_SIZE      nSize;        /* size of allocated pair array */
   ZH_SIZE      nLen;         /* number of used items in pair array */
   int          iFlags;       /* hash item flags */
} ZH_BASEHASH, * PZH_BASEHASH;


/* This releases hash when called from the garbage collector */
static ZH_GARBAGE_FUNC( zh_hashGarbageRelease )
{
   PZH_BASEHASH pBaseHash = ( PZH_BASEHASH ) Cargo;

   ZH_TRACE( ZH_TR_INFO, ( "zh_hashGarbageRelease(%p)", ( void * ) pBaseHash ) );

   if( pBaseHash->nSize > 0 )
   {
      while( pBaseHash->nLen )
      {
         PZH_ITEM pKey, pVal;

         pBaseHash->nLen--;
         pKey = &pBaseHash->pPairs[ pBaseHash->nLen ].key;
         pVal = &pBaseHash->pPairs[ pBaseHash->nLen ].value;

         /* small hack for buggy destructors in hash items */
         pBaseHash->iFlags |= ZH_HASH_RESORT;

         if( ZH_IS_GCITEM( pKey ) && ZH_IS_GCITEM( pVal ) )
         {
            

            zh_itemRawMove( zh_stackAllocItem(), pVal );
            zh_itemClear( pKey );
            zh_stackPop();
         }
         else
         {
            if( ZH_IS_COMPLEX( pKey ) )
               zh_itemClear( pKey );
            if( ZH_IS_COMPLEX( pVal ) )
               zh_itemClear( pVal );
         }
      }

      if( pBaseHash->pnPos )
      {
         zh_xfree( pBaseHash->pnPos );
         pBaseHash->pnPos = NULL;
      }

      if( pBaseHash->pPairs )
      {
         zh_xfree( pBaseHash->pPairs );
         pBaseHash->pPairs = NULL;
      }
   }

   if( pBaseHash->pDefault )
   {
      PZH_ITEM pDefault = pBaseHash->pDefault;
      pBaseHash->pDefault = NULL;
      zh_itemRelease( pDefault );
   }
}

static ZH_GARBAGE_FUNC( zh_hashGarbageMark )
{
   PZH_BASEHASH pBaseHash = ( PZH_BASEHASH ) Cargo;

   ZH_TRACE( ZH_TR_INFO, ( "zh_hashMarkGarbage(%p)", ( void * ) pBaseHash ) );

   if( pBaseHash->nLen > 0 )
   {
      PZH_HASHPAIR pPairs = pBaseHash->pPairs;
      ZH_SIZE nLen = pBaseHash->nLen;

      while( nLen-- )
      {
         if( ZH_IS_GCITEM( &pPairs[ nLen ].key ) )
            zh_gcItemRef( &pPairs[ nLen ].key );
         if( ZH_IS_GCITEM( &pPairs[ nLen ].value ) )
            zh_gcItemRef( &pPairs[ nLen ].value );
      }
   }
   if( pBaseHash->pDefault )
      zh_gcMark( pBaseHash->pDefault );
}

static const ZH_GC_FUNCS s_gcHashFuncs =
{
   zh_hashGarbageRelease,
   zh_hashGarbageMark
};

static int zh_hashItemCmp( PZH_ITEM pKey1, PZH_ITEM pKey2, int iFlags )
{
   if( ZH_IS_STRING( pKey1 ) )
   {
      if( ZH_IS_STRING( pKey2 ) )
      {
         if( iFlags & ZH_HASH_BINARY )
            return pKey1->item.asString.length < pKey2->item.asString.length ? -1 :
                 ( pKey1->item.asString.length > pKey2->item.asString.length ? 1 :
                   memcmp( pKey1->item.asString.value,
                           pKey2->item.asString.value,
                           pKey1->item.asString.length ) );
         else if( iFlags & ZH_HASH_IGNORECASE )
            return zh_itemStrICmp( pKey1, pKey2, ZH_TRUE );
         else
            return zh_itemStrCmp( pKey1, pKey2, ZH_TRUE );
      }
      else
         return 1;
   }
   else if( ZH_IS_DATETIME( pKey1 ) )
   {
      if( ZH_IS_DATETIME( pKey2 ) )
         return pKey1->item.asDateTime.julian < pKey2->item.asDateTime.julian ? -1 :
              ( pKey1->item.asDateTime.julian > pKey2->item.asDateTime.julian ? 1 :
              ( pKey1->item.asDateTime.time < pKey2->item.asDateTime.time ? -1 :
              ( pKey1->item.asDateTime.time > pKey2->item.asDateTime.time ? 1 : 0 ) ) );
      else if( ZH_IS_STRING( pKey2 ) )
         return -1;
      else
         return 1;
   }
   else if( ZH_IS_POINTER( pKey1 ) )
   {
      if( ZH_IS_POINTER( pKey2 ) )
         return pKey1->item.asPointer.value < pKey2->item.asPointer.value ? -1 :
              ( pKey1->item.asPointer.value > pKey2->item.asPointer.value ? 1 : 0 );
      else if( ZH_IS_STRING( pKey2 ) || ZH_IS_DATETIME( pKey2 ) )
         return -1;
      else
         return 1;
   }
   else if( ZH_IS_NUMINT( pKey1 ) && ZH_IS_NUMINT( pKey2 ) )
   {
      ZH_MAXINT n1 = ZH_ITEM_GET_NUMINTRAW( pKey1 ),
                n2 = ZH_ITEM_GET_NUMINTRAW( pKey2 );
      return n1 < n2 ? -1 : ( n1 > n2 ? 1 : 0 );
   }
   else if( ZH_IS_NUMERIC( pKey2 ) )
   {
      double d1 = zh_itemGetND( pKey1 ), d2 = zh_itemGetND( pKey2 );
      return d1 < d2 ? -1 : ( d1 > d2 ? 1 : 0 );
   }
   return -1;
}

static void zh_hashResort( PZH_BASEHASH pBaseHash )
{
   ZH_SIZE nPos;
   PZH_HASHPAIR pPairs = ( PZH_HASHPAIR )
                           zh_xgrab( pBaseHash->nLen * sizeof( ZH_HASHPAIR ) );
   for( nPos = 0; nPos < pBaseHash->nLen; ++nPos )
   {
      memcpy( pPairs + nPos, pBaseHash->pPairs + pBaseHash->pnPos[ nPos ], sizeof( ZH_HASHPAIR ) );
      pBaseHash->pnPos[ nPos ] = nPos;
   }

   zh_xfree( pBaseHash->pPairs );
   pBaseHash->pPairs = pPairs;
   pBaseHash->nSize = pBaseHash->nLen;
   pBaseHash->pnPos = ( ZH_SIZE * )
         zh_xrealloc( pBaseHash->pnPos, pBaseHash->nSize * sizeof( ZH_SIZE ) );
}

static void zh_hashSortDo( PZH_BASEHASH pBaseHash )
{
   ZH_SIZE nFrom;
   int iFlags = pBaseHash->iFlags;

   if( pBaseHash->pnPos )
   {
      ZH_SIZE * pnPos = pBaseHash->pnPos;

      pnPos[ 0 ] = 0;
      for( nFrom = 1; nFrom < pBaseHash->nLen; ++nFrom )
      {
         PZH_ITEM pKey = &pBaseHash->pPairs[ nFrom ].key;
         ZH_SIZE nLeft = 0, nRight = nFrom;

         while( nLeft < nRight )
         {
            ZH_SIZE nMiddle = ( nLeft + nRight ) >> 1;
            int i = zh_hashItemCmp( &pBaseHash->pPairs[ pnPos[ nMiddle ] ].key,
                                    pKey, iFlags );
            if( i > 0 )
               nRight = nMiddle;
            else
               nLeft = nMiddle + 1;
         }
         if( nLeft < nFrom )
         {
            nRight = nFrom;
            do
               pnPos[ nRight ] = pnPos[ nRight - 1 ];
            while( --nRight > nLeft );
         }
         pnPos[ nLeft ] = nFrom;
      }
   }
   else
   {
      /* The hash array is probably quite well sorted so this trivial
       * algorithm is the most efficient one [druzus]
       */

      for( nFrom = 1; nFrom < pBaseHash->nLen; ++nFrom )
      {
         ZH_SIZE nPos = nFrom;
         while( nPos > 0 && zh_hashItemCmp( &pBaseHash->pPairs[ nPos - 1 ].key,
                                            &pBaseHash->pPairs[ nPos ].key,
                                            iFlags ) > 0 )
         {
            ZH_HASHPAIR pair;
            memcpy( &pair, pBaseHash->pPairs + nPos - 1, sizeof( ZH_HASHPAIR ) );
            memcpy( pBaseHash->pPairs + nPos - 1, pBaseHash->pPairs + nPos, sizeof( ZH_HASHPAIR ) );
            memcpy( pBaseHash->pPairs + nPos, &pair, sizeof( ZH_HASHPAIR ) );
            --nPos;
         }
      }
   }

   pBaseHash->iFlags &= ~ZH_HASH_RESORT;
}

static ZH_BOOL zh_hashFind( PZH_BASEHASH pBaseHash, PZH_ITEM pKey, ZH_SIZE * pnPos )
{
   ZH_SIZE nLeft, nRight;
   int iFlags = pBaseHash->iFlags;

   if( iFlags & ZH_HASH_RESORT )
      zh_hashSortDo( pBaseHash );

   nLeft = 0;
   nRight = pBaseHash->nLen;

   while( nLeft < nRight )
   {
      ZH_SIZE nMiddle = ( nLeft + nRight ) >> 1;
      int i = zh_hashItemCmp( &pBaseHash->pPairs[ pBaseHash->pnPos ?
                                  pBaseHash->pnPos[ nMiddle ] : nMiddle ].key,
                              pKey, iFlags );
      if( i == 0 )
      {
         *pnPos = pBaseHash->pnPos ? pBaseHash->pnPos[ nMiddle ] : nMiddle;
         return ZH_TRUE;
      }
      else if( i < 0 )
         nLeft = nMiddle + 1;
      else
         nRight = nMiddle;
   }

   *pnPos = nLeft;
   return ZH_FALSE;
}

static void zh_hashResize( PZH_BASEHASH pBaseHash, ZH_SIZE nNewSize )
{
   if( pBaseHash->nSize < nNewSize )
   {
      if( pBaseHash->nSize )
      {
         pBaseHash->pPairs = ( PZH_HASHPAIR ) zh_xrealloc( pBaseHash->pPairs,
                                          nNewSize * sizeof( ZH_HASHPAIR ) );
         if( pBaseHash->pnPos )
            pBaseHash->pnPos = ( ZH_SIZE * ) zh_xrealloc( pBaseHash->pnPos,
                                             nNewSize * sizeof( ZH_SIZE ) );
      }
      else
      {
         pBaseHash->pPairs = ( PZH_HASHPAIR ) zh_xgrab( nNewSize * sizeof( ZH_HASHPAIR ) );
         if( pBaseHash->iFlags & ZH_HASH_KEEPORDER )
            pBaseHash->pnPos = ( ZH_SIZE * ) zh_xgrab( nNewSize * sizeof( ZH_SIZE ) );
      }

      do
      {
         pBaseHash->pPairs[ pBaseHash->nSize ].key.type = ZH_IT_NIL;
         pBaseHash->pPairs[ pBaseHash->nSize ].value.type = ZH_IT_NIL;
      }
      while( ++pBaseHash->nSize < nNewSize );
   }
   else if( pBaseHash->nSize > nNewSize && pBaseHash->nLen <= nNewSize )
   {
      pBaseHash->nSize = nNewSize;
      if( nNewSize )
      {
         pBaseHash->pPairs = ( PZH_HASHPAIR ) zh_xrealloc( pBaseHash->pPairs,
                                          nNewSize * sizeof( ZH_HASHPAIR ) );
         if( pBaseHash->pnPos )
            pBaseHash->pnPos = ( ZH_SIZE * ) zh_xrealloc( pBaseHash->pnPos,
                                             nNewSize * sizeof( ZH_SIZE ) );
      }
      else
      {
         zh_xfree( pBaseHash->pPairs );
         pBaseHash->pPairs = NULL;
         if( pBaseHash->pnPos )
         {
            zh_xfree( pBaseHash->pnPos );
            pBaseHash->pnPos = NULL;
         }
      }
   }
}

static PZH_ITEM zh_hashValuePtr( PZH_BASEHASH pBaseHash, PZH_ITEM pKey, ZH_BOOL fAdd )
{
   ZH_SIZE nPos;

   if( ! zh_hashFind( pBaseHash, pKey, &nPos ) )
   {
      if( ! fAdd )
         return NULL;

      if( pBaseHash->nSize == pBaseHash->nLen )
         zh_hashResize( pBaseHash, pBaseHash->nSize + ZH_HASH_ITEM_ALLOC );

      if( pBaseHash->pnPos )
      {
         memmove( pBaseHash->pnPos + nPos + 1, pBaseHash->pnPos + nPos,
                  ( pBaseHash->nLen - nPos ) * sizeof( ZH_SIZE ) );
         nPos = ( pBaseHash->pnPos[ nPos ] = pBaseHash->nLen );
      }
      else if( nPos < pBaseHash->nLen )
      {
         memmove( pBaseHash->pPairs + nPos + 1, pBaseHash->pPairs + nPos,
                  ( pBaseHash->nLen - nPos ) * sizeof( ZH_HASHPAIR ) );
         pBaseHash->pPairs[ nPos ].key.type = ZH_IT_NIL;
         pBaseHash->pPairs[ nPos ].value.type = ZH_IT_NIL;
      }

      pBaseHash->nLen++;
      zh_itemCopy( &pBaseHash->pPairs[ nPos ].key, pKey );
      if( pBaseHash->pDefault )
         zh_itemCloneTo( &pBaseHash->pPairs[ nPos ].value, pBaseHash->pDefault );
   }

   return &pBaseHash->pPairs[ nPos ].value;
}

static ZH_BOOL zh_hashNewValue( PZH_BASEHASH pBaseHash, PZH_ITEM pKey, PZH_ITEM pValue )
{
   ZH_SIZE nPos;

   if( ! zh_hashFind( pBaseHash, pKey, &nPos ) )
   {
      if( pBaseHash->nSize == pBaseHash->nLen )
         zh_hashResize( pBaseHash, pBaseHash->nSize + ZH_HASH_ITEM_ALLOC );

      if( pBaseHash->pnPos )
      {
         memmove( pBaseHash->pnPos + nPos + 1, pBaseHash->pnPos + nPos,
                  ( pBaseHash->nLen - nPos ) * sizeof( ZH_SIZE ) );
         nPos = ( pBaseHash->pnPos[ nPos ] = pBaseHash->nLen );
      }
      else if( nPos < pBaseHash->nLen )
      {
         memmove( pBaseHash->pPairs + nPos + 1, pBaseHash->pPairs + nPos,
                  ( pBaseHash->nLen - nPos ) * sizeof( ZH_HASHPAIR ) );
         pBaseHash->pPairs[ nPos ].key.type = ZH_IT_NIL;
         pBaseHash->pPairs[ nPos ].value.type = ZH_IT_NIL;
      }

      pBaseHash->nLen++;
      zh_itemCopy( &pBaseHash->pPairs[ nPos ].key, pKey );
      zh_itemCopyFromRef( &pBaseHash->pPairs[ nPos ].value, pValue );

      return ZH_TRUE;
   }

   return ZH_FALSE;
}

static void zh_hashNewPair( PZH_BASEHASH pBaseHash, PZH_ITEM * pKeyPtr, PZH_ITEM * pValPtr )
{
   if( pBaseHash->nSize == pBaseHash->nLen )
      zh_hashResize( pBaseHash, pBaseHash->nSize + ZH_HASH_ITEM_ALLOC );

   if( pBaseHash->pnPos )
      pBaseHash->pnPos[ pBaseHash->nLen ] = pBaseHash->nLen;

   *pKeyPtr = &pBaseHash->pPairs[ pBaseHash->nLen ].key;
   *pValPtr = &pBaseHash->pPairs[ pBaseHash->nLen ].value;

   pBaseHash->nLen++;
}

static void zh_hashDelPair( PZH_BASEHASH pBaseHash, ZH_SIZE nPos )
{
   if( --pBaseHash->nLen == 0 )
   {
      PZH_HASHPAIR pPairs = pBaseHash->pPairs;
      pBaseHash->pPairs = NULL;
      pBaseHash->nSize = 0;
      if( pBaseHash->pnPos )
      {
         zh_xfree( pBaseHash->pnPos );
         pBaseHash->pnPos = NULL;
      }
      if( ZH_IS_COMPLEX( &pPairs->key ) )
         zh_itemClear( &pPairs->key );
      if( ZH_IS_COMPLEX( &pPairs->value ) )
         zh_itemClear( &pPairs->value );
      zh_xfree( pPairs );
   }
   else
   {
      if( pBaseHash->pnPos && ( pBaseHash->iFlags & ZH_HASH_RESORT ) == 0 )
      {
#ifdef ZH_FAST_HASH_DEL
         ZH_SIZE * pnPos, * pnDel, * pnLast;

         pnPos = pBaseHash->pnPos + pBaseHash->nLen;
         pnDel = pnLast = NULL;
         for( ;; )
         {
            if( *pnPos == nPos )
            {
               pnDel = pnPos;
               if( pnLast != NULL )
                  break;
            }
            if( *pnPos == pBaseHash->nLen )
            {
               pnLast = pnPos;
               if( pnDel != NULL )
                  break;
            }
            if( pnPos-- == pBaseHash->pnPos )
               zh_errInternal( ZH_EI_ERRUNRECOV, "ZH_HDEL(): corrupted hash index", NULL, NULL );
         }
         *pnLast = *pnDel;
         if( pnDel < pBaseHash->pnPos + pBaseHash->nLen )
            memmove( pnDel, pnDel + 1,
                     ( pBaseHash->pnPos + pBaseHash->nLen - pnDel ) * sizeof( ZH_SIZE ) );
         if( nPos != pBaseHash->nLen )
         {
            ZH_HASHPAIR pair;
            memcpy( &pair, pBaseHash->pPairs + nPos, sizeof( ZH_HASHPAIR ) );
            memcpy( pBaseHash->pPairs + nPos, pBaseHash->pPairs + pBaseHash->nLen,
                    sizeof( ZH_HASHPAIR ) );
            nPos = pBaseHash->nLen;
            memcpy( pBaseHash->pPairs + nPos, &pair, sizeof( ZH_HASHPAIR ) );
         }
#else
         ZH_SIZE n = 0;
         while( n < pBaseHash->nLen )
         {
            if( pBaseHash->pnPos[ n ] > nPos )
               pBaseHash->pnPos[ n++ ]--;
            else if( pBaseHash->pnPos[ n ] == nPos )
               memmove( &pBaseHash->pnPos[ n ], &pBaseHash->pnPos[ n + 1 ],
                        ( pBaseHash->nLen - n ) * sizeof( ZH_SIZE ) );
            else
               ++n;
         }
#endif
      }

      if( nPos != pBaseHash->nLen )
      {
         ZH_HASHPAIR pair;
         memcpy( &pair, pBaseHash->pPairs + nPos, sizeof( ZH_HASHPAIR ) );
         memmove( pBaseHash->pPairs + nPos, pBaseHash->pPairs + nPos + 1,
                  ( pBaseHash->nLen - nPos ) * sizeof( ZH_HASHPAIR ) );
         nPos = pBaseHash->nLen;
         memcpy( pBaseHash->pPairs + nPos, &pair, sizeof( ZH_HASHPAIR ) );
      }

      zh_itemSetNil( &pBaseHash->pPairs[ nPos ].key );
      zh_itemSetNil( &pBaseHash->pPairs[ nPos ].value );
      if( pBaseHash->nSize - pBaseHash->nLen > ( ZH_HASH_ITEM_ALLOC << 1 ) )
      {
         pBaseHash->nSize -= ZH_HASH_ITEM_ALLOC;
         pBaseHash->pPairs = ( PZH_HASHPAIR ) zh_xrealloc( pBaseHash->pPairs,
                              pBaseHash->nSize * sizeof( ZH_HASHPAIR ) );
         if( pBaseHash->pnPos )
            pBaseHash->pnPos = ( ZH_SIZE * ) zh_xrealloc( pBaseHash->pnPos,
                                 pBaseHash->nSize * sizeof( ZH_SIZE ) );
      }
   }
}

PZH_ITEM zh_hashNew( PZH_ITEM pItem )
{
   PZH_BASEHASH pBaseHash;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_hashNew(%p)", ( void * ) pItem ) );

   if( pItem == NULL )
      pItem = zh_itemNew( NULL );
   else if( ZH_IS_COMPLEX( pItem ) )
      zh_itemClear( pItem );

   pBaseHash = ( PZH_BASEHASH ) zh_gcAllocRaw( sizeof( ZH_BASEHASH ), &s_gcHashFuncs );
   pBaseHash->pPairs   = NULL;
   pBaseHash->pnPos    = NULL;
   pBaseHash->nSize    = 0;
   pBaseHash->nLen     = 0;
   pBaseHash->iFlags   = ZH_HASH_FLAG_DEFAULT;
   pBaseHash->pDefault = NULL;

   pItem->type = ZH_IT_HASH;
   pItem->item.asHash.value = pBaseHash;

   return pItem;
}

ZH_SIZE zh_hashLen( PZH_ITEM pHash )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_hashLen(%p)", ( void * ) pHash ) );

   if( ZH_IS_HASH( pHash ) )
      return pHash->item.asHash.value->nLen;
   else
      return 0;
}

void zh_hashPreallocate( PZH_ITEM pHash, ZH_SIZE nNewSize )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_hashPreallocate(%p,%" ZH_PFS "u)", ( void * ) pHash, nNewSize ) );

   if( ZH_IS_HASH( pHash ) )
      zh_hashResize( pHash->item.asHash.value, nNewSize );
}

ZH_BOOL zh_hashAllocNewPair( PZH_ITEM pHash, PZH_ITEM * pKeyPtr, PZH_ITEM * pValPtr )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_hashAllocNewPair(%p,%p,%p)", ( void * ) pHash, ( void * ) pKeyPtr, ( void * ) pValPtr ) );

   if( ZH_IS_HASH( pHash ) )
   {
      zh_hashNewPair( pHash->item.asHash.value, pKeyPtr, pValPtr );
      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}

void zh_hashSort( PZH_ITEM pHash )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_hashSort(%p)", ( void * ) pHash ) );

   if( ZH_IS_HASH( pHash ) )
   {
      PZH_BASEHASH pBaseHash = pHash->item.asHash.value;

      if( pBaseHash->iFlags & ZH_HASH_RESORT )
         zh_hashSortDo( pBaseHash );

      if( pBaseHash->pnPos )
         zh_hashResort( pBaseHash );
   }
}

PZH_ITEM zh_hashGetItemPtr( PZH_ITEM pHash, PZH_ITEM pKey, int iFlags )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_hashGetItemPtr(%p,%p,%d)", ( void * ) pHash, ( void * ) pKey, iFlags ) );

   if( ZH_IS_HASH( pHash ) && ZH_IS_HASHKEY( pKey ) )
   {
      PZH_ITEM pDest = zh_hashValuePtr( pHash->item.asHash.value, pKey,
         iFlags && ( pHash->item.asHash.value->iFlags & iFlags ) == iFlags );
      if( pDest )
         return ZH_IS_BYREF( pDest ) ? zh_itemUnRef( pDest ) : pDest;
   }

   return NULL;
}

PZH_ITEM zh_hashGetCItemPtr( PZH_ITEM pHash, const char * pszKey )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_hashGetCItemPtr(%p,%s)", ( void * ) pHash, pszKey ) );

   if( ZH_IS_HASH( pHash ) )
   {
      
      /* we will not make any copy of pKey (autoadd is disabled) so it's
       * safe to use zh_itemPutCConst()
       */
      PZH_ITEM pKey = zh_itemPutCConst( zh_stackAllocItem(), pszKey );
      PZH_ITEM pDest = zh_hashValuePtr( pHash->item.asHash.value, pKey, ZH_FALSE );
      zh_stackPop();
      if( pDest )
         return ZH_IS_BYREF( pDest ) ? zh_itemUnRef( pDest ) : pDest;
   }

   return NULL;
}

ZH_SIZE zh_hashGetCItemPos( PZH_ITEM pHash, const char * pszKey )
{
   ZH_SIZE nPos = 0;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_hashGetCItemPos(%p,%s)", ( void * ) pHash, pszKey ) );

   if( ZH_IS_HASH( pHash ) )
   {
      
      /* we will not make any copy of pKey (autoadd is disabled) so it's
       * safe to use zh_itemPutCConst()
       */
      PZH_ITEM pKey = zh_itemPutCConst( zh_stackAllocItem(), pszKey );

      if( zh_hashFind( pHash->item.asHash.value, pKey, &nPos ) )
         nPos++;
      else
         nPos = 0;
      zh_stackPop();
   }

   return nPos;
}

PZH_ITEM zh_hashGetItemRefPtr( PZH_ITEM pHash, PZH_ITEM pKey )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_hashGetItemRefPtr(%p,%p)", ( void * ) pHash, ( void * ) pKey ) );

   if( ZH_IS_HASH( pHash ) && ZH_IS_HASHKEY( pKey ) )
   {
      PZH_ITEM pDest = zh_hashValuePtr( pHash->item.asHash.value, pKey,
            ( pHash->item.asHash.value->iFlags & ZH_HASH_AUTOADD_REFERENCE ) ==
            ZH_HASH_AUTOADD_REFERENCE );
      if( pDest )
      {
         if( ! ZH_IS_BYREF( pDest ) )
            pDest = zh_memvarDetachLocal( pDest );
         return pDest;
      }
   }

   return NULL;
}

ZH_BOOL zh_hashScan( PZH_ITEM pHash, PZH_ITEM pKey, ZH_SIZE * pnPos )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_hashScan(%p,%p,%p)", ( void * ) pHash, ( void * ) pKey, ( void * ) pnPos ) );

   if( ZH_IS_HASH( pHash ) )
   {
      ZH_SIZE nPos;
      if( ZH_IS_HASHKEY( pKey ) )
      {
         if( zh_hashFind( pHash->item.asHash.value, pKey, &nPos ) )
         {
            if( pnPos )
               *pnPos = nPos + 1;
            return ZH_TRUE;
         }
      }
      else if( ZH_IS_HASH( pKey ) && pKey->item.asHash.value->nLen == 1 )
      {
         if( zh_hashFind( pHash->item.asHash.value, &pKey->item.asHash.value->pPairs[ 0 ].key, &nPos ) )
         {
            PZH_ITEM pVal1 = &pHash->item.asHash.value->pPairs[ nPos ].value;
            PZH_ITEM pVal2 = &pKey->item.asHash.value->pPairs[ 0 ].value;

            if( zh_itemEqual( pVal1, pVal2 ) )
            {
               if( pnPos )
                  *pnPos = nPos + 1;
               return ZH_TRUE;
            }
         }
      }
   }
   if( pnPos )
      *pnPos = 0;
   return ZH_FALSE;
}

ZH_BOOL zh_hashScanSoft( PZH_ITEM pHash, PZH_ITEM pKey, ZH_SIZE * pnPos )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_hashScanSoft(%p,%p,%p)", ( void * ) pHash, ( void * ) pKey, ( void * ) pnPos ) );

   if( ZH_IS_HASH( pHash ) && ZH_IS_HASHKEY( pKey ) )
   {
      ZH_SIZE nPos;
      if( zh_hashFind( pHash->item.asHash.value, pKey, &nPos ) )
      {
         if( pnPos )
            *pnPos = nPos + 1;
         return ZH_TRUE;
      }
      else
      {
         if( pnPos )
         {
            if( nPos != 0 && pHash->item.asHash.value->pnPos )
               nPos = pHash->item.asHash.value->pnPos[ nPos - 1 ] + 1;
            *pnPos = nPos;
         }
         return ZH_FALSE;
      }
   }
   if( pnPos )
      *pnPos = 0;
   return ZH_FALSE;
}

ZH_BOOL zh_hashClear( PZH_ITEM pHash )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_hashClear(%p)", ( void * ) pHash ) );

   if( ZH_IS_HASH( pHash ) )
   {
      if( pHash->item.asHash.value->nSize )
      {
         while( pHash->item.asHash.value->nLen )
         {
            pHash->item.asHash.value->nLen--;
            if( ZH_IS_COMPLEX( &pHash->item.asHash.value->pPairs[ pHash->item.asHash.value->nLen ].key ) )
               zh_itemClear( &pHash->item.asHash.value->pPairs[ pHash->item.asHash.value->nLen ].key );
            if( ZH_IS_COMPLEX( &pHash->item.asHash.value->pPairs[ pHash->item.asHash.value->nLen ].value ) )
               zh_itemClear( &pHash->item.asHash.value->pPairs[ pHash->item.asHash.value->nLen ].value );
         }
         /*
          * This condition is a protection against recursive call
          * from .zh object destructor [druzus]
          */
         if( pHash->item.asHash.value->nSize )
         {
            zh_xfree( pHash->item.asHash.value->pPairs );
            pHash->item.asHash.value->pPairs = NULL;
            pHash->item.asHash.value->nSize = 0;
            if( pHash->item.asHash.value->pnPos )
            {
               zh_xfree( pHash->item.asHash.value->pnPos );
               pHash->item.asHash.value->pnPos = NULL;
            }
         }
      }
      return ZH_TRUE;
   }

   return ZH_FALSE;
}

ZH_BOOL zh_hashDel( PZH_ITEM pHash, PZH_ITEM pKey )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_hashDel(%p,%p)", ( void * ) pHash, ( void * ) pKey ) );

   if( ZH_IS_HASH( pHash ) && ZH_IS_HASHKEY( pKey ) )
   {
      PZH_BASEHASH pBaseHash = pHash->item.asHash.value;
      ZH_SIZE nPos;

      if( zh_hashFind( pBaseHash, pKey, &nPos ) )
      {
         zh_hashDelPair( pBaseHash, nPos );
         return ZH_TRUE;
      }
   }

   return ZH_FALSE;
}

ZH_BOOL zh_hashRemove( PZH_ITEM pHash, PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_hashRemove(%p,%p)", ( void * ) pHash, ( void * ) pItem ) );

   if( ZH_IS_HASH( pHash ) )
   {
      if( ZH_IS_HASHKEY( pItem ) )
      {
         zh_hashDel( pHash, pItem );
         return ZH_TRUE;
      }
      else if( ZH_IS_ARRAY( pItem ) )
      {
         ZH_SIZE n = 0;
         PZH_ITEM pKey;
         while( ( pKey = zh_arrayGetItemPtr( pItem, ++n ) ) != NULL )
            zh_hashDel( pHash, pKey );
         return ZH_TRUE;
      }
      else if( ZH_IS_HASH( pItem ) )
      {
         if( pHash->item.asHash.value == pItem->item.asHash.value )
            zh_hashClear( pHash );
         else
         {
            ZH_SIZE nLen = 0;
            while( nLen < pItem->item.asHash.value->nLen )
               zh_hashDel( pHash, &pItem->item.asHash.value->pPairs[ nLen++ ].key );
         }
         return ZH_TRUE;
      }
   }
   return ZH_FALSE;
}

ZH_BOOL zh_hashAdd( PZH_ITEM pHash, PZH_ITEM pKey, PZH_ITEM pValue )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_hashAdd(%p,%p,%p)", ( void * ) pHash, ( void * ) pKey, ( void * ) pValue ) );

   if( ZH_IS_HASH( pHash ) && ZH_IS_HASHKEY( pKey ) )
   {
      PZH_ITEM pDest = zh_hashValuePtr( pHash->item.asHash.value, pKey, ZH_TRUE );
      if( pDest )
      {
         if( ZH_IS_BYREF( pDest ) )
            pDest = zh_itemUnRef( pDest );
         if( pValue )
            zh_itemCopyFromRef( pDest, pValue );
         else
            zh_itemSetNil( pDest );
         return ZH_TRUE;
      }
   }

   return ZH_FALSE;
}

ZH_BOOL zh_hashAddNew( PZH_ITEM pHash, PZH_ITEM pKey, PZH_ITEM pValue )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_hashAddNew(%p,%p,%p)", ( void * ) pHash, ( void * ) pKey, ( void * ) pValue ) );

   if( ZH_IS_HASH( pHash ) && ZH_IS_HASHKEY( pKey ) )
      return zh_hashNewValue( pHash->item.asHash.value, pKey, pValue );
   else
      return ZH_FALSE;
}

PZH_ITEM zh_hashGetKeyAt( PZH_ITEM pHash, ZH_SIZE nPos )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_hashGetKeyAt(%p,%" ZH_PFS "u)", ( void * ) pHash, nPos ) );

   if( ZH_IS_HASH( pHash ) && nPos > 0 && nPos <= pHash->item.asHash.value->nLen )
      return &pHash->item.asHash.value->pPairs[ nPos - 1 ].key;
   else
      return NULL;
}

PZH_ITEM zh_hashGetValueAt( PZH_ITEM pHash, ZH_SIZE nPos )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_hashGetValueAt(%p,%" ZH_PFS "u)", ( void * ) pHash, nPos ) );

   if( ZH_IS_HASH( pHash ) && nPos > 0 && nPos <= pHash->item.asHash.value->nLen )
   {
      PZH_ITEM pValue = &pHash->item.asHash.value->pPairs[ nPos - 1 ].value;
      return ZH_IS_BYREF( pValue ) ? zh_itemUnRef( pValue ) : pValue;
   }
   else
      return NULL;
}

ZH_BOOL zh_hashDelAt( PZH_ITEM pHash, ZH_SIZE nPos )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_hashDelAt(%p,%" ZH_PFS "u)", ( void * ) pHash, nPos ) );

   if( ZH_IS_HASH( pHash ) && nPos > 0 && nPos <= pHash->item.asHash.value->nLen )
   {
      zh_hashDelPair( pHash->item.asHash.value, nPos - 1 );
      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}

/* retrieves the hash unique ID */
void * zh_hashId( PZH_ITEM pHash )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_hashId(%p)", ( void * ) pHash ) );

   if( ZH_IS_HASH( pHash ) )
      return ( void * ) pHash->item.asHash.value;
   else
      return NULL;
}

/* retrieves numer of references to the hash */
ZH_COUNTER zh_hashRefs( PZH_ITEM pHash )
{
   if( ZH_IS_HASH( pHash ) )
      return zh_gcRefCount( pHash->item.asHash.value );
   else
      return 0;
}

void zh_hashCloneBody( PZH_ITEM pDest, PZH_ITEM pHash, PZH_NESTED_CLONED pClonedList )
{
   ZH_SIZE nPos;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_hashCloneBody(%p,%p,%p)", ( void * ) pDest, ( void * ) pHash, ( void * ) pClonedList ) );

   zh_hashNew( pDest );
   pDest->item.asHash.value->iFlags = pHash->item.asHash.value->iFlags;
   zh_hashResize( pDest->item.asHash.value, pHash->item.asHash.value->nLen );
   if( pHash->item.asHash.value->pDefault )
   {
      pDest->item.asHash.value->pDefault =
                              zh_itemNew( pHash->item.asHash.value->pDefault );
      zh_gcUnlock( pDest->item.asHash.value->pDefault );
   }
   if( pHash->item.asHash.value->pnPos )
      memcpy( pDest->item.asHash.value->pnPos,
              pHash->item.asHash.value->pnPos,
              pHash->item.asHash.value->nLen * sizeof( ZH_SIZE ) );
   for( nPos = 0; nPos < pHash->item.asHash.value->nLen; ++nPos )
   {
      PZH_ITEM pValue = &pHash->item.asHash.value->pPairs[ nPos ].value;
      if( ZH_IS_BYREF( pValue ) )
         pValue = zh_itemUnRef( pValue );
      zh_itemCopy( &pDest->item.asHash.value->pPairs[ nPos ].key,
                   &pHash->item.asHash.value->pPairs[ nPos ].key );
      pDest->item.asHash.value->nLen++;
      zh_nestedCloneDo( &pDest->item.asHash.value->pPairs[ nPos ].value, pValue, pClonedList );
   }
}

PZH_ITEM zh_hashCloneTo( PZH_ITEM pDest, PZH_ITEM pHash )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_hashCloneTo(%p,%p)", ( void * ) pDest, ( void * ) pHash ) );

   if( ZH_IS_HASH( pHash ) )
   {
      ZH_NESTED_CLONED clonedList;

      zh_nestedCloneInit( &clonedList, ( void * ) pHash->item.asHash.value, pDest );
      zh_hashCloneBody( pDest, pHash, &clonedList );
      zh_nestedCloneFree( &clonedList );
   }

   return pDest;
}

PZH_ITEM zh_hashClone( PZH_ITEM pHash )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_hashClone(%p)", ( void * ) pHash ) );

   return zh_hashCloneTo( zh_itemNew( NULL ), pHash );
}

void zh_hashJoin( PZH_ITEM pDest, PZH_ITEM pSource, int iType )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_hashJoin(%p,%p,%d)", ( void * ) pDest, ( void * ) pSource, iType ) );

   if( ZH_IS_HASH( pDest ) && ZH_IS_HASH( pSource ) )
   {
      PZH_BASEHASH pBaseHash;
      ZH_SIZE nPos;

      switch( iType )
      {
         case ZH_HASH_UNION:        /* OR */
            pBaseHash = pSource->item.asHash.value;
            if( pBaseHash != pDest->item.asHash.value )
            {
               for( nPos = 0; nPos < pBaseHash->nLen; ++nPos )
               {
                  PZH_ITEM pVal = &pBaseHash->pPairs[ nPos ].value;
                  if( ZH_IS_BYREF( pVal ) )
                     pVal = zh_itemUnRef( pVal );
                  zh_hashAdd( pDest, &pBaseHash->pPairs[ nPos ].key, pVal );
               }
            }
            break;

         case ZH_HASH_INTERSECT:    /* AND */
            pBaseHash = pDest->item.asHash.value;
            if( pBaseHash != pSource->item.asHash.value )
            {
               for( nPos = 0; nPos < pBaseHash->nLen; )
               {
                  ZH_SIZE nSrcPos;
                  if( zh_hashFind( pSource->item.asHash.value,
                                   &pBaseHash->pPairs[ nPos ].key, &nSrcPos ) )
                  {
                     PZH_ITEM pDestVal = &pBaseHash->pPairs[ nPos ].value;
                     if( ZH_IS_BYREF( pDestVal ) )
                        pDestVal = zh_itemUnRef( pDestVal );
                     zh_itemCopyFromRef( pDestVal,
                                         &pSource->item.asHash.value->pPairs[ nSrcPos ].value );
                     ++nPos;
                  }
                  else
                     zh_hashDelPair( pBaseHash, nPos );
               }
            }
            break;

         case ZH_HASH_DIFFERENCE:   /* XOR */
            pBaseHash = pSource->item.asHash.value;
            if( pBaseHash == pDest->item.asHash.value )
               zh_hashClear( pDest );
            else
            {
               for( nPos = 0; nPos < pBaseHash->nLen; ++nPos )
               {
                  if( ! zh_hashDel( pDest, &pBaseHash->pPairs[ nPos ].key ) )
                  {
                     PZH_ITEM pVal = &pBaseHash->pPairs[ nPos ].value;
                     if( ZH_IS_BYREF( pVal ) )
                        pVal = zh_itemUnRef( pVal );
                     zh_hashAdd( pDest, &pBaseHash->pPairs[ nPos ].key, pVal );
                  }
               }
            }
            break;

         case ZH_HASH_REMOVE:       /* NOT -> h1 AND ( h1 XOR h2 ) */
            pBaseHash = pSource->item.asHash.value;
            if( pBaseHash == pDest->item.asHash.value )
               zh_hashClear( pDest );
            else
            {
               for( nPos = 0; nPos < pBaseHash->nLen; ++nPos )
                  zh_hashDel( pDest, &pBaseHash->pPairs[ nPos ].key );
            }
            break;
      }
   }
}

PZH_ITEM zh_hashGetKeys( PZH_ITEM pHash )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_hashGetKeys(%p)", ( void * ) pHash ) );

   if( ZH_IS_HASH( pHash ) )
   {
      PZH_ITEM pKeys = zh_itemArrayNew( zh_hashLen( pHash ) ), pKey;
      ZH_SIZE nPos = 0;

      while( ( pKey = zh_hashGetKeyAt( pHash, ++nPos ) ) != NULL )
      {
         PZH_ITEM pDest = zh_arrayGetItemPtr( pKeys, nPos );
         if( ! pDest )
            break;
         zh_itemCopy( pDest, pKey );
      }
      return pKeys;
   }

   return NULL;
}

PZH_ITEM zh_hashGetValues( PZH_ITEM pHash )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_hashGetValues(%p)", ( void * ) pHash ) );

   if( ZH_IS_HASH( pHash ) )
   {
      PZH_ITEM pValues = zh_itemArrayNew( zh_hashLen( pHash ) ), pVal;
      ZH_SIZE nPos = 0;

      while( ( pVal = zh_hashGetValueAt( pHash, ++nPos ) ) != NULL )
      {
         PZH_ITEM pDest = zh_arrayGetItemPtr( pValues, nPos );
         if( ! pDest )
            break;
         zh_itemCopy( pDest, pVal );
      }
      return pValues;
   }

   return NULL;
}

void zh_hashSetDefault( PZH_ITEM pHash, PZH_ITEM pValue )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_hashSetDefault(%p,%p)", ( void * ) pHash, ( void * ) pValue ) );

   if( ZH_IS_HASH( pHash ) )
   {
      if( pHash->item.asHash.value->pDefault )
      {
         zh_itemRelease( pHash->item.asHash.value->pDefault );
         pHash->item.asHash.value->pDefault = NULL;
      }
      if( pValue && ! ZH_IS_NIL( pValue ) &&
          ( ! ZH_IS_HASH( pValue ) || pHash->item.asHash.value !=
                                     pValue->item.asHash.value ) )
      {
         pHash->item.asHash.value->pDefault = zh_itemClone( pValue );
         zh_gcUnlock( pHash->item.asHash.value->pDefault );
      }
   }
}

PZH_ITEM zh_hashGetDefault( PZH_ITEM pHash )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_hashGetDefault(%p)", ( void * ) pHash ) );

   if( ZH_IS_HASH( pHash ) )
      return pHash->item.asHash.value->pDefault;
   else
      return NULL;
}

void zh_hashSetFlags( PZH_ITEM pHash, int iFlags )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_hashSetFlags(%p,%d)", ( void * ) pHash, iFlags ) );

   if( ZH_IS_HASH( pHash ) )
   {
      pHash->item.asHash.value->iFlags |= iFlags;
      if( pHash->item.asHash.value->pnPos == NULL &&
          pHash->item.asHash.value->nSize &&
          ( pHash->item.asHash.value->iFlags & ZH_HASH_KEEPORDER ) != 0 )
      {
         ZH_SIZE n = pHash->item.asHash.value->nSize;

         pHash->item.asHash.value->pnPos = ( ZH_SIZE * )
                                             zh_xgrab( n * sizeof( ZH_SIZE ) );
         do
         {
            --n;
            pHash->item.asHash.value->pnPos[ n ] = n;
         }
         while( n );
      }
   }
}

void zh_hashClearFlags( PZH_ITEM pHash, int iFlags )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_hashClearFlags(%p,%d)", ( void * ) pHash, iFlags ) );

   if( ZH_IS_HASH( pHash ) )
   {
      pHash->item.asHash.value->iFlags &= ~iFlags;
      if( pHash->item.asHash.value->pnPos != NULL &&
          ( pHash->item.asHash.value->iFlags & ZH_HASH_KEEPORDER ) == 0 )
      {
         zh_hashResort( pHash->item.asHash.value );
         zh_xfree( pHash->item.asHash.value->pnPos );
         pHash->item.asHash.value->pnPos = NULL;
      }
   }
}

int zh_hashGetFlags( PZH_ITEM pHash )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_hashGetFlags(%p)", ( void * ) pHash ) );

   if( ZH_IS_HASH( pHash ) )
      return pHash->item.asHash.value->iFlags;
   else
      return 0;
}
