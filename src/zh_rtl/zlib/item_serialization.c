/*
 * Item serialization code
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

#include "zh_api.h"
#include "zh_item_api.h"
#include "zh_class_api.h"
#include "zh_codepage_api.h"
#include "zh_error_api.h"
#include "zh_zlib.h"
#include "zh_vm.h"
#include "zh_stack.h"
#include "serialization.zhh"


/*
ZH_UCHAR [ 1 ] - item type
   0. NIL               0
   1. TRUE              0
   2. FALSE             0
   3. ZERO              0
   4. INT8              1
   5. INT16             2
   6. INT24             3
   7. INT32             4
   8. INT64             8
   9. DOUBLE IEEE754 LE 8
  10. DATE              3
  11. STRING8           1+n
  12. STRING16          2+n
  13. STRING32          4+n
  14. ARRAY8            1+n
  15. ARRAY16           2+n
  16. ARRAY32           4+n
  17. ARRAYREF8         1+n
  18. ARRAYREF16        2+n
  19. ARRAYREF32        4+n
  20. HASH8             1+n
  21. HASH16            2+n
  22. HASH32            4+n
  23. HASHREF8          1+n
  24. HASHREF16         2+n
  25. HASHREF32         4+n
  26. SYMBOL            1+n
  27. CYCLIC REFERENCE  4
  28. OBJECT MARKER     n+1+m+1
  29. STRNUL            0
  30. STRPAD8           1+1+n
  31. STRPAD16          2+2+n
  32. STRPAD32          4+4+n
  33. INT8NUM           1+1
  34. INT16NUM          2+2
  35. INT24NUM          3+1
  36. INT32NUM          4+1
  37. INT64NUM          8+1
  38. DBLNUM            8+1+1
  39. TIMESTAMP         8
  40. HASHFLAGS         2
  41. HASHDEFAULT VALUE 0
  42. ZCOMPRESS         4+4+n

xZiher types ZH_SERIAL_XZH_*:
  67. 'C' <BE64:n><str> 8+n
  76. 'L' 'T'|'F'       1
  78. 'N' 'I'<BE64>     1+8
  78. 'N' 'L'<BE64>     1+8
  78. 'N' 'X'<BE64>     1+8
  78. 'N' 'D'<IEEE754LE>1+8
  68. 'D' <BE64>        8
  84. 'T' <IEEE754LE>   8
  90. 'Z'               0
complex ones:
  65. 'A' <BE64>        8+n   <val>,...
  66. 'B'               1+n   <val>       (ZH_SaveBlock())
  72. 'H' <BE64>        8+n   <key,val>,...
  79. 'O' <BE64>        8+n   <clsname>,<msg,val>,...   (__ClsGetPropertiesAndValues())
  81. 'Q' <BE64:n>      8+n   <clsname>,ZHPersistent:SaveToText(raw)
  82. 'R' 'A' <BE64>    1+8   (index to array of arrays)
  82. 'R' 'O' <BE64>    1+8   (index to array of objects)
  82. 'R' 'H' <BE64>    1+8   (index to array of hashes)
  82. 'R' 'B' <BE64>    1+8   (index to array of codeblock)
*/

#define ZH_SERIAL_NIL         0
#define ZH_SERIAL_TRUE        1
#define ZH_SERIAL_FALSE       2
#define ZH_SERIAL_ZERO        3
#define ZH_SERIAL_INT8        4
#define ZH_SERIAL_INT16       5
#define ZH_SERIAL_INT24       6
#define ZH_SERIAL_INT32       7
#define ZH_SERIAL_INT64       8
#define ZH_SERIAL_DOUBLE      9
#define ZH_SERIAL_DATE       10
#define ZH_SERIAL_STRING8    11
#define ZH_SERIAL_STRING16   12
#define ZH_SERIAL_STRING32   13
#define ZH_SERIAL_ARRAY8     14
#define ZH_SERIAL_ARRAY16    15
#define ZH_SERIAL_ARRAY32    16
#define ZH_SERIAL_ARRAYREF8  17
#define ZH_SERIAL_ARRAYREF16 18
#define ZH_SERIAL_ARRAYREF32 19
#define ZH_SERIAL_HASH8      20
#define ZH_SERIAL_HASH16     21
#define ZH_SERIAL_HASH32     22
#define ZH_SERIAL_HASHREF8   23
#define ZH_SERIAL_HASHREF16  24
#define ZH_SERIAL_HASHREF32  25
#define ZH_SERIAL_SYMBOL     26
#define ZH_SERIAL_REF        27
#define ZH_SERIAL_OBJ        28
#define ZH_SERIAL_STRNUL     29
#define ZH_SERIAL_STRPAD8    30
#define ZH_SERIAL_STRPAD16   31
#define ZH_SERIAL_STRPAD32   32
#define ZH_SERIAL_INT8NUM    33
#define ZH_SERIAL_INT16NUM   34
#define ZH_SERIAL_INT24NUM   35
#define ZH_SERIAL_INT32NUM   36
#define ZH_SERIAL_INT64NUM   37
#define ZH_SERIAL_DBLNUM     38
#define ZH_SERIAL_TIMESTAMP  39
#define ZH_SERIAL_HASHFLAGS  40
#define ZH_SERIAL_HASHDEFVAL 41
#define ZH_SERIAL_ZCOMPRESS  42
/* xZiher types */
#define ZH_SERIAL_XZH_A      65
#define ZH_SERIAL_XZH_B      66
#define ZH_SERIAL_XZH_C      67
#define ZH_SERIAL_XZH_D      68
#define ZH_SERIAL_XZH_H      72
#define ZH_SERIAL_XZH_L      76
#define ZH_SERIAL_XZH_O      79
#define ZH_SERIAL_XZH_Q      81
#define ZH_SERIAL_XZH_R      82
#define ZH_SERIAL_XZH_N      78
#define ZH_SERIAL_XZH_T      84
#define ZH_SERIAL_XZH_Z      90


#define ZH_SERIAL_DUMMYOFFSET ( ( ZH_SIZE ) -1 )

#define ZH_SERIAL_REFLSTINIT  16

typedef struct
{
   void *   value;
   ZH_SIZE  nOffset;
   int      iRefs;
   int      iType;
} ZH_REF_ITEM, * PZH_REF_ITEM;

typedef struct
{
   ZH_SIZE        nSize;
   ZH_SIZE        nCount;
   PZH_REF_ITEM   pRefs;
} ZH_REF_LIST, * PZH_REF_LIST;

static ZH_SIZE zh_deserializeItem( PZH_ITEM pItem,
                                   PZH_CODEPAGE cdpIn, PZH_CODEPAGE cdpOut,
                                   const ZH_UCHAR * pBuffer, ZH_SIZE nOffset,
                                   PZH_REF_LIST pRefList );


static void zh_itemSerialRefListInit( PZH_REF_LIST pRefList )
{
   memset( pRefList, 0, sizeof( ZH_REF_LIST ) );
}

static void zh_itemSerialRefListFree( PZH_REF_LIST pRefList )
{
   if( pRefList->nSize )
      zh_xfree( pRefList->pRefs );
}

static PZH_REF_ITEM zh_itemSerialValueFind( PZH_REF_LIST pRefList, void * value,
                                            ZH_SIZE * pnPos )
{
   ZH_SIZE nFirst, nLast, nMiddle;

   nFirst = 0;
   nLast = pRefList->nCount;
   nMiddle = ( nFirst + nLast ) >> 1;

   while( nFirst < nLast )
   {
      if( ( ZH_PTRUINT ) pRefList->pRefs[ nMiddle ].value < ( ZH_PTRUINT ) value )
         nFirst = nMiddle + 1;
      else if( ( ZH_PTRUINT ) pRefList->pRefs[ nMiddle ].value > ( ZH_PTRUINT ) value )
         nLast = nMiddle;
      else
      {
         * pnPos = nMiddle;
         return &pRefList->pRefs[ nMiddle ];
      }
      nMiddle = ( nFirst + nLast ) >> 1;
   }

   * pnPos = nMiddle;

   return NULL;
}

static PZH_REF_ITEM zh_itemSerialOffsetFind( PZH_REF_LIST pRefList, ZH_SIZE nOffset,
                                             int iType, ZH_SIZE * pnPos )
{
   ZH_SIZE nFirst, nLast, nMiddle;

   nFirst = 0;
   nLast = pRefList->nCount;
   nMiddle = ( nFirst + nLast ) >> 1;

   while( nFirst < nLast )
   {
      if( pRefList->pRefs[ nMiddle ].nOffset < nOffset )
         nFirst = nMiddle + 1;
      else if( pRefList->pRefs[ nMiddle ].nOffset > nOffset )
         nLast = nMiddle;
      else if( pRefList->pRefs[ nMiddle ].iType < iType )
         nFirst = nMiddle + 1;
      else if( pRefList->pRefs[ nMiddle ].iType > iType )
         nLast = nMiddle;
      else
      {
         * pnPos = nMiddle;
         return &pRefList->pRefs[ nMiddle ];
      }
      nMiddle = ( nFirst + nLast ) >> 1;
   }

   * pnPos = nMiddle;

   return NULL;
}

static PZH_REF_ITEM zh_itemSerialRefNew( PZH_REF_LIST pRefList, ZH_SIZE nPos )
{
   PZH_REF_ITEM pRef;
   ZH_SIZE nMove;

   if( pRefList->nCount >= pRefList->nSize )
   {
      if( pRefList->nSize == 0 )
         pRefList->nSize = ZH_SERIAL_REFLSTINIT;
      else
         pRefList->nSize += pRefList->nSize >> 1;
      pRefList->pRefs = ( PZH_REF_ITEM )
                        zh_xrealloc( pRefList->pRefs,
                                     pRefList->nSize * sizeof( ZH_REF_ITEM ) );
   }

   nMove = pRefList->nCount - nPos;
   pRef = &pRefList->pRefs[ pRefList->nCount++ ];
   while( nMove-- > 0 )
   {
      *pRef = *( pRef - 1 );
      pRef--;
   }

   return pRef;
}

/* used by zh_itemSerialSize() for ZH_IT_ARRAY and ZH_IT_HASH */
static ZH_BOOL zh_itemSerialValueRef( PZH_REF_LIST pRefList, void * value,
                                      ZH_SIZE nOffset )
{
   PZH_REF_ITEM pRef;
   ZH_SIZE nPos;

   if( ( pRef = zh_itemSerialValueFind( pRefList, value, &nPos ) ) != NULL )
   {
      pRef->iRefs = 1;
      return ZH_TRUE;
   }

   pRef = zh_itemSerialRefNew( pRefList, nPos );

   pRef->value = value;
   pRef->nOffset = nOffset;
   pRef->iRefs = 0;
   pRef->iType = 0;

   return ZH_FALSE;
}

/* used between zh_itemSerialSize() and zh_serializeItem() */
static void zh_itemSerialUnusedFree( PZH_REF_LIST pRefList )
{
   if( pRefList->nSize )
   {
      ZH_SIZE nPos, nCount;

      for( nPos = nCount = 0; nPos < pRefList->nCount; ++nPos )
      {
         if( pRefList->pRefs[ nPos ].iRefs != 0 )
         {
            if( nCount != nPos )
               memcpy( &pRefList->pRefs[ nCount ], &pRefList->pRefs[ nPos ],
                       sizeof( ZH_REF_ITEM ) );
            ++nCount;
         }
      }
      pRefList->nSize = pRefList->nCount = nCount;
      pRefList->pRefs = ( PZH_REF_ITEM )
                        zh_xrealloc( pRefList->pRefs,
                                     nCount * sizeof( ZH_REF_ITEM ) );
   }
}

/* used by zh_serializeItem() for ZH_IT_ARRAY and ZH_IT_HASH */
static ZH_BOOL zh_itemSerialValueOffset( PZH_REF_LIST pRefList, void * value,
                                         ZH_SIZE nOffset, ZH_SIZE * pnRef )
{
   PZH_REF_ITEM pRef;
   ZH_SIZE nPos;

   if( ( pRef = zh_itemSerialValueFind( pRefList, value, &nPos ) ) != NULL )
   {
      *pnRef = pRef->nOffset;
      return pRef->nOffset < nOffset;
   }

   *pnRef = ZH_SERIAL_DUMMYOFFSET;
   return ZH_FALSE;
}

/* used by zh_deserializeTest()
   for ZH_SERIAL_ARRAYREF*, ZH_SERIAL_HASHREF*, ZH_SERIAL_REF */
static ZH_BOOL zh_itemSerialOffsetRef( PZH_REF_LIST pRefList, ZH_SIZE nOffset )
{
   PZH_REF_ITEM pRef;
   ZH_SIZE nPos;

   if( zh_itemSerialOffsetFind( pRefList, nOffset, 0, &nPos ) != NULL )
      return ZH_TRUE;

   pRef = zh_itemSerialRefNew( pRefList, nPos );

   pRef->value = NULL;
   pRef->nOffset = nOffset;
   pRef->iRefs = 0;
   pRef->iType = 0;

   return ZH_FALSE;
}

/* used by zh_deserializeTest() for ZH_SERIAL_XZH_R */
static void zh_itemSerialTypedRef( PZH_REF_LIST pRefList, int iType,
                                   ZH_SIZE nIndex )
{
   ZH_SIZE nPos;

   if( zh_itemSerialOffsetFind( pRefList, nIndex, iType, &nPos ) == NULL )
   {
      PZH_REF_ITEM pRef = zh_itemSerialRefNew( pRefList, nPos );

      pRef->value = NULL;
      pRef->nOffset = nIndex;
      pRef->iRefs = 0;
      pRef->iType = iType;
   }
}

/* used by zh_deserializeItem()
   for ZH_SERIAL_ARRAYREF* and ZH_SERIAL_HASHREF* */
static void zh_itemSerialOffsetSet( PZH_REF_LIST pRefList, PZH_ITEM pItem,
                                    ZH_SIZE nOffset )
{
   PZH_REF_ITEM pRef;
   ZH_SIZE nPos;

   if( ( pRef = zh_itemSerialOffsetFind( pRefList, nOffset, 0, &nPos ) ) != NULL )
      pRef->value = ( void * ) pItem;
}

/* used by zh_deserializeItem() for ZH_SERIAL_REF */
static void zh_itemSerialOffsetGet( PZH_REF_LIST pRefList, PZH_ITEM pItem,
                                    ZH_SIZE nOffset )
{
   PZH_REF_ITEM pRef;
   ZH_SIZE nPos;

   if( ( pRef = zh_itemSerialOffsetFind( pRefList, nOffset, 0, &nPos ) ) != NULL )
      zh_itemCopy( pItem, ( PZH_ITEM ) pRef->value );
}

/* used by zh_deserializeItem() for
   ZH_SERIAL_XZH_A, ZH_SERIAL_XZH_H, ZH_SERIAL_XZH_Q, ZH_SERIAL_XZH_O */
static void zh_itemSerialTypedSet( PZH_REF_LIST pRefList, PZH_ITEM pItem, int iType )
{
   ZH_SIZE nPos = pRefList->nCount;

   while( nPos-- )
   {
      PZH_REF_ITEM pRef = &pRefList->pRefs[ nPos ];

      if( pRef->iType == iType && pRef->value == NULL )
      {
         if( ( ZH_SIZE ) ++pRef->iRefs == pRef->nOffset )
            pRef->value = ( void * ) pItem;
      }
   }
}

/* used by zh_deserializeItem() for ZH_SERIAL_XZH_R */
static void zh_itemSerialTypedGet( PZH_REF_LIST pRefList, PZH_ITEM pItem,
                                   int iType, ZH_SIZE nIndex )
{
   PZH_REF_ITEM pRef;
   ZH_SIZE nPos;

   if( ( pRef = zh_itemSerialOffsetFind( pRefList, nIndex, iType, &nPos ) ) != NULL )
   {
      if( pRef->value )
         zh_itemCopy( pItem, ( PZH_ITEM ) pRef->value );
   }
}

static ZH_SIZE zh_itemSerialSize( PZH_ITEM pItem, int iFlags,
                                  PZH_CODEPAGE cdpIn, PZH_CODEPAGE cdpOut,
                                  PZH_REF_LIST pRefList, ZH_SIZE nOffset )
{
   ZH_SIZE nSize, nLen, u;
   ZH_MAXINT lVal;
   ZH_USHORT uiClass;
   const char * szVal;

   if( ZH_IS_BYREF( pItem ) )
      pItem = zh_itemUnRef( pItem );

   switch( zh_itemType( pItem ) )
   {
      case ZH_IT_NIL:
      case ZH_IT_LOGICAL:
         nSize = 1;
         break;

      case ZH_IT_DATE:
         nSize = 4;
         break;

      case ZH_IT_TIMESTAMP:
         nSize = 9;
         break;

      case ZH_IT_INTEGER:
      case ZH_IT_LONG:
         lVal = zh_itemGetNInt( pItem );
         if( lVal == 0 )
            nSize = ( iFlags & ZH_SERIALIZE_NUMSIZE ) ? 2 : 1;
         else if( ZH_LIM_INT8( lVal ) )
            nSize = 2;
         else if( ZH_LIM_INT16( lVal ) )
            nSize = 3;
         else if( ZH_LIM_INT24( lVal ) )
            nSize = 4;
         else if( ZH_LIM_INT32( lVal ) )
            nSize = 5;
         else
            nSize = 9;
         if( iFlags & ZH_SERIALIZE_NUMSIZE )
            nSize++;
         break;

      case ZH_IT_DOUBLE:
         if( iFlags & ZH_SERIALIZE_NUMSIZE )
            nSize = 11;
         else
            nSize = ( zh_itemGetND( pItem ) == 0.0 ) ? 1 : 9;
         break;

      case ZH_IT_SYMBOL:
         nSize = 2 + strlen( zh_itemGetSymbol( pItem )->szName );
         break;

      case ZH_IT_STRING:
      case ZH_IT_MEMO:
         szVal = zh_itemGetCPtr( pItem );
         nLen = zh_itemGetCLen( pItem );
         if( nLen == 0 )
            nSize = 1;
         else
         {
            u = nLen;
            while( u && szVal[ u - 1 ] == ' ' )
               --u;
            u = nLen - u;
            nLen = zh_cdpnDupLen( szVal, nLen, cdpIn, cdpOut );
            if( nLen <= 255 )
               nSize = u > 1 ? nLen - u + 3 : nLen + 2;
            else if( nLen <= UINT16_MAX )
               nSize = u > 2 ? nLen - u + 5 : nLen + 3;
            else
               nSize = u > 4 ? nLen - u + 9 : nLen + 5;
         }
         break;

      case ZH_IT_ARRAY:
         nSize = 0;
         uiClass = zh_objGetClass( pItem );
         if( uiClass )
         {
            const char * szClass = zh_clsName( uiClass ),
                       * szFunc = zh_clsFuncName( uiClass );
            if( szClass && szFunc )
               nSize += strlen( szClass ) + strlen( szFunc ) + 3;
         }
         if( ( iFlags & ZH_SERIALIZE_IGNOREREF ) == 0 && zh_arrayRefs( pItem ) > 1 &&
             zh_itemSerialValueRef( pRefList, zh_arrayId( pItem ), nOffset + nSize ) )
         {
            nSize = 5;
         }
         else
         {
            nLen = zh_arrayLen( pItem );
            if( nLen <= 255 )
               nSize += 2;
            else if( nLen <= UINT16_MAX )
               nSize += 3;
            else
               nSize += 5;
            for( u = 1; u <= nLen; u++ )
               nSize += zh_itemSerialSize( zh_arrayGetItemPtr( pItem, u ), iFlags,
                                           cdpIn, cdpOut, pRefList, nOffset + nSize );
         }
         break;

      case ZH_IT_HASH:
         if( ( iFlags & ZH_SERIALIZE_IGNOREREF ) == 0 && zh_hashRefs( pItem ) > 1 &&
             zh_itemSerialValueRef( pRefList, zh_hashId( pItem ), nOffset ) )
         {
            nSize = 5;
         }
         else
         {
            PZH_ITEM pDefVal;

            if( ( zh_hashGetFlags( pItem ) & ~ZH_HASH_RESORT ) != ZH_HASH_FLAG_DEFAULT )
               nSize = 3;
            else
               nSize = 0;
            pDefVal = zh_hashGetDefault( pItem );
            if( pDefVal )
            {
               nSize++;
               nSize += zh_itemSerialSize( pDefVal, iFlags,
                                           cdpIn, cdpOut, pRefList, nOffset + nSize );
            }
            nLen = zh_hashLen( pItem );
            if( nLen <= 255 )
               nSize += 2;
            else if( nLen <= UINT16_MAX )
               nSize += 3;
            else
               nSize += 5;
            for( u = 1; u <= nLen; u++ )
            {
               nSize += zh_itemSerialSize( zh_hashGetKeyAt( pItem, u ), iFlags,
                                           cdpIn, cdpOut, pRefList, nOffset + nSize );
               nSize += zh_itemSerialSize( zh_hashGetValueAt( pItem, u ), iFlags,
                                           cdpIn, cdpOut, pRefList, nOffset + nSize );
            }
         }
         break;

      default:
         /* map to NIL */
         nSize = 1;
   }

   return nSize;
}

static ZH_SIZE zh_serializeItem( PZH_ITEM pItem, ZH_BOOL iFlags,
                                 PZH_CODEPAGE cdpIn, PZH_CODEPAGE cdpOut,
                                 ZH_UCHAR * pBuffer, ZH_SIZE nOffset,
                                 PZH_REF_LIST pRefList )
{
   ZH_MAXINT lVal;
   double d;
   int iWidth, iDecimal;
   long l, l2;
   const char * szVal;
   ZH_SIZE nRef, nLen, n;

   if( ZH_IS_BYREF( pItem ) )
      pItem = zh_itemUnRef( pItem );

   switch( zh_itemType( pItem ) )
   {
      case ZH_IT_NIL:
         pBuffer[ nOffset++ ] = ZH_SERIAL_NIL;
         break;

      case ZH_IT_LOGICAL:
         pBuffer[ nOffset++ ] = zh_itemGetL( pItem ) ? ZH_SERIAL_TRUE : ZH_SERIAL_FALSE;
         break;

      case ZH_IT_DATE:
         pBuffer[ nOffset++ ] = ZH_SERIAL_DATE;
         l = zh_itemGetDL( pItem );
         ZH_PUT_LE_UINT24( &pBuffer[ nOffset ], l );
         nOffset += 3;
         break;

      case ZH_IT_TIMESTAMP:
         pBuffer[ nOffset++ ] = ZH_SERIAL_TIMESTAMP;
         zh_itemGetTDT( pItem, &l, &l2 );
         ZH_PUT_LE_UINT32( &pBuffer[ nOffset ], l );
         nOffset += 4;
         ZH_PUT_LE_UINT32( &pBuffer[ nOffset ], l2 );
         nOffset += 4;
         break;

      case ZH_IT_INTEGER:
      case ZH_IT_LONG:
         lVal = zh_itemGetNInt( pItem );
         if( iFlags & ZH_SERIALIZE_NUMSIZE )
         {
            zh_itemGetNLen( pItem, &iWidth, NULL );
            if( ZH_LIM_INT8( lVal ) )
            {
               pBuffer[ nOffset++ ] = ZH_SERIAL_INT8NUM;
               pBuffer[ nOffset++ ] = ( ZH_UCHAR ) lVal;
            }
            else if( ZH_LIM_INT16( lVal ) )
            {
               pBuffer[ nOffset++ ] = ZH_SERIAL_INT16NUM;
               ZH_PUT_LE_UINT16( &pBuffer[ nOffset ], lVal );
               nOffset += 2;
            }
            else if( ZH_LIM_INT24( lVal ) )
            {
               pBuffer[ nOffset++ ] = ZH_SERIAL_INT24NUM;
               ZH_PUT_LE_UINT24( &pBuffer[ nOffset ], lVal );
               nOffset += 3;
            }
            else if( ZH_LIM_INT32( lVal ) )
            {
               pBuffer[ nOffset++ ] = ZH_SERIAL_INT32NUM;
               ZH_PUT_LE_UINT32( &pBuffer[ nOffset ], lVal );
               nOffset += 4;
            }
            else
            {
               pBuffer[ nOffset++ ] = ZH_SERIAL_INT64NUM;
               ZH_PUT_LE_UINT64( &pBuffer[ nOffset ], lVal );
               nOffset += 8;
            }
            pBuffer[ nOffset++ ] = ( ZH_UCHAR ) iWidth;
         }
         else if( lVal == 0 )
         {
            pBuffer[ nOffset++ ] = ZH_SERIAL_ZERO;
         }
         else if( ZH_LIM_INT8( lVal ) )
         {
            pBuffer[ nOffset++ ] = ZH_SERIAL_INT8;
            pBuffer[ nOffset++ ] = ( ZH_UCHAR ) lVal;
         }
         else if( ZH_LIM_INT16( lVal ) )
         {
            pBuffer[ nOffset++ ] = ZH_SERIAL_INT16;
            ZH_PUT_LE_UINT16( &pBuffer[ nOffset ], lVal );
            nOffset += 2;
         }
         else if( ZH_LIM_INT24( lVal ) )
         {
            pBuffer[ nOffset++ ] = ZH_SERIAL_INT24;
            ZH_PUT_LE_UINT24( &pBuffer[ nOffset ], lVal );
            nOffset += 3;
         }
         else if( ZH_LIM_INT32( lVal ) )
         {
            pBuffer[ nOffset++ ] = ZH_SERIAL_INT32;
            ZH_PUT_LE_UINT32( &pBuffer[ nOffset ], lVal );
            nOffset += 4;
         }
         else
         {
            pBuffer[ nOffset++ ] = ZH_SERIAL_INT64;
            ZH_PUT_LE_UINT64( &pBuffer[ nOffset ], lVal );
            nOffset += 8;
         }
         break;

      case ZH_IT_DOUBLE:
         d = zh_itemGetND( pItem );
         if( iFlags & ZH_SERIALIZE_NUMSIZE )
         {
            zh_itemGetNLen( pItem, &iWidth, &iDecimal );
            pBuffer[ nOffset++ ] = ZH_SERIAL_DBLNUM;
            ZH_PUT_LE_DOUBLE( &pBuffer[ nOffset ], d );
            nOffset += 8;
            pBuffer[ nOffset++ ] = ( ZH_UCHAR ) iWidth;
            pBuffer[ nOffset++ ] = ( ZH_UCHAR ) iDecimal;
         }
         else if( d == 0.0 )
         {
            pBuffer[ nOffset++ ] = ZH_SERIAL_ZERO;
         }
         else
         {
            pBuffer[ nOffset++ ] = ZH_SERIAL_DOUBLE;
            ZH_PUT_LE_DOUBLE( &pBuffer[ nOffset ], d );
            nOffset += 8;
         }
         break;

      case ZH_IT_SYMBOL:
         szVal = zh_itemGetSymbol( pItem )->szName;
         nLen = strlen( szVal );
         if( nLen > 0xFF )
            nLen = 0xFF;
         pBuffer[ nOffset++ ] = ZH_SERIAL_SYMBOL;
         pBuffer[ nOffset++ ] = ( ZH_UCHAR ) nLen;
         memcpy( &pBuffer[ nOffset ], szVal, nLen );
         nOffset += nLen;
         break;

      case ZH_IT_STRING:
      case ZH_IT_MEMO:
         szVal = zh_itemGetCPtr( pItem );
         nLen = zh_itemGetCLen( pItem );
         if( nLen == 0 )
         {
            pBuffer[ nOffset++ ] = ZH_SERIAL_STRNUL;
         }
         else
         {
            ZH_SIZE nSize = n = nLen;
            while( n && szVal[ n - 1 ] == ' ' )
               --n;
            n = nLen - n;
            nLen = zh_cdpnDupLen( szVal, nLen, cdpIn, cdpOut );
            if( nLen <= 255 )
            {
               if( n > 1 )
               {
                  nLen -= n;
                  nSize -= n;
                  pBuffer[ nOffset++ ] = ZH_SERIAL_STRPAD8;
                  pBuffer[ nOffset++ ] = ( ZH_UCHAR ) nLen;
                  pBuffer[ nOffset++ ] = ( ZH_UCHAR ) n;
               }
               else
               {
                  pBuffer[ nOffset++ ] = ZH_SERIAL_STRING8;
                  pBuffer[ nOffset++ ] = ( ZH_UCHAR ) nLen;
               }
            }
            else if( nLen <= UINT16_MAX )
            {
               if( n > 2 )
               {
                  nLen -= n;
                  nSize -= n;
                  pBuffer[ nOffset++ ] = ZH_SERIAL_STRPAD16;
                  ZH_PUT_LE_UINT16( &pBuffer[ nOffset ], nLen );
                  nOffset += 2;
                  ZH_PUT_LE_UINT16( &pBuffer[ nOffset ], n );
                  nOffset += 2;
               }
               else
               {
                  pBuffer[ nOffset++ ] = ZH_SERIAL_STRING16;
                  ZH_PUT_LE_UINT16( &pBuffer[ nOffset ], nLen );
                  nOffset += 2;
               }
            }
            else
            {
               if( n > 4 )
               {
                  nLen -= n;
                  nSize -= n;
                  pBuffer[ nOffset++ ] = ZH_SERIAL_STRPAD32;
                  ZH_PUT_LE_UINT32( &pBuffer[ nOffset ], nLen );
                  nOffset += 4;
                  ZH_PUT_LE_UINT32( &pBuffer[ nOffset ], n );
                  nOffset += 4;
               }
               else
               {
                  pBuffer[ nOffset++ ] = ZH_SERIAL_STRING32;
                  ZH_PUT_LE_UINT32( &pBuffer[ nOffset ], nLen );
                  nOffset += 4;
               }
            }
            n = nLen;
            zh_cdpnDup2( szVal, nSize, ( char * ) &pBuffer[ nOffset ], &n,
                         cdpIn, cdpOut );
            nOffset += nLen;
         }
         break;

      case ZH_IT_ARRAY:
         nRef = ZH_SERIAL_DUMMYOFFSET;
         if( zh_arrayRefs( pItem ) > 1 &&
             zh_itemSerialValueOffset( pRefList, zh_arrayId( pItem ), nOffset, &nRef ) )
         {
            pBuffer[ nOffset++ ] = ZH_SERIAL_REF;
            ZH_PUT_LE_UINT32( &pBuffer[ nOffset ], nRef );
            nOffset += 4;
         }
         else
         {
            ZH_USHORT uiClass = zh_objGetClass( pItem );
            if( uiClass )
            {
               const char * szClass = zh_clsName( uiClass ),
                          * szFunc = zh_clsFuncName( uiClass );
               if( szClass && szFunc )
               {
                  pBuffer[ nOffset++ ] = ZH_SERIAL_OBJ;
                  nLen = strlen( szClass ) + 1;
                  memcpy( &pBuffer[ nOffset ], szClass, nLen );
                  nOffset += nLen;
                  nLen = strlen( szFunc ) + 1;
                  memcpy( &pBuffer[ nOffset ], szFunc, nLen );
                  nOffset += nLen;
               }
            }
            nLen = zh_arrayLen( pItem );
            if( nLen <= 255 )
            {
               pBuffer[ nOffset++ ] = nRef == ZH_SERIAL_DUMMYOFFSET ?
                                      ZH_SERIAL_ARRAY8 : ZH_SERIAL_ARRAYREF8;
               pBuffer[ nOffset++ ] = ( ZH_UCHAR ) nLen;
            }
            else if( nLen <= UINT16_MAX )
            {
               pBuffer[ nOffset++ ] = nRef == ZH_SERIAL_DUMMYOFFSET ?
                                      ZH_SERIAL_ARRAY16 : ZH_SERIAL_ARRAYREF16;
               ZH_PUT_LE_UINT16( &pBuffer[ nOffset ], nLen );
               nOffset += 2;
            }
            else
            {
               pBuffer[ nOffset++ ] = nRef == ZH_SERIAL_DUMMYOFFSET ?
                                      ZH_SERIAL_ARRAY32 : ZH_SERIAL_ARRAYREF32;
               ZH_PUT_LE_UINT32( &pBuffer[ nOffset ], nLen );
               nOffset += 4;
            }
            for( n = 1; n <= nLen; n++ )
               nOffset = zh_serializeItem( zh_arrayGetItemPtr( pItem, n ), iFlags,
                                           cdpIn, cdpOut, pBuffer, nOffset, pRefList );
         }
         break;

      case ZH_IT_HASH:
         nRef = ZH_SERIAL_DUMMYOFFSET;
         if( zh_hashRefs( pItem ) > 1 &&
             zh_itemSerialValueOffset( pRefList, zh_hashId( pItem ), nOffset, &nRef ) )
         {
            pBuffer[ nOffset++ ] = ZH_SERIAL_REF;
            ZH_PUT_LE_UINT32( &pBuffer[ nOffset ], nRef );
            nOffset += 4;
         }
         else
         {
            int iHashFlags = zh_hashGetFlags( pItem );
            PZH_ITEM pDefVal = zh_hashGetDefault( pItem );

            if( ( iHashFlags & ~ZH_HASH_RESORT ) != ZH_HASH_FLAG_DEFAULT )
            {
               pBuffer[ nOffset++ ] = ZH_SERIAL_HASHFLAGS;
               ZH_PUT_LE_UINT16( &pBuffer[ nOffset ], iHashFlags );
               nOffset += 2;
            }
            if( pDefVal )
            {
               pBuffer[ nOffset++ ] = ZH_SERIAL_HASHDEFVAL;
               nOffset = zh_serializeItem( pDefVal, iFlags,
                                           cdpIn, cdpOut, pBuffer, nOffset, pRefList );
            }
            nLen = zh_hashLen( pItem );
            if( nLen <= 255 )
            {
               pBuffer[ nOffset++ ] = nRef == ZH_SERIAL_DUMMYOFFSET ?
                                      ZH_SERIAL_HASH8 : ZH_SERIAL_HASHREF8;
               pBuffer[ nOffset++ ] = ( ZH_UCHAR ) nLen;
            }
            else if( nLen <= UINT16_MAX )
            {
               pBuffer[ nOffset++ ] = nRef == ZH_SERIAL_DUMMYOFFSET ?
                                      ZH_SERIAL_HASH16 : ZH_SERIAL_HASHREF16;
               ZH_PUT_LE_UINT16( &pBuffer[ nOffset ], nLen );
               nOffset += 2;
            }
            else
            {
               pBuffer[ nOffset++ ] = nRef == ZH_SERIAL_DUMMYOFFSET ?
                                      ZH_SERIAL_HASH32 : ZH_SERIAL_HASHREF32;
               ZH_PUT_LE_UINT32( &pBuffer[ nOffset ], nLen );
               nOffset += 4;
            }
            for( n = 1; n <= nLen; n++ )
            {
               nOffset = zh_serializeItem( zh_hashGetKeyAt( pItem, n ), iFlags,
                                           cdpIn, cdpOut, pBuffer, nOffset, pRefList );
               nOffset = zh_serializeItem( zh_hashGetValueAt( pItem, n ), iFlags,
                                           cdpIn, cdpOut, pBuffer, nOffset, pRefList );
            }
         }
         break;

      default:
         /* map to NIL */
         pBuffer[ nOffset++ ] = ZH_SERIAL_NIL;
         break;
   }

   return nOffset;
}

static ZH_BOOL zh_deserializeTest( const ZH_UCHAR ** pBufferPtr, ZH_SIZE * pnSize,
                                   ZH_SIZE nOffset, PZH_REF_LIST pRefList )
{
   const ZH_UCHAR * pBuffer = *pBufferPtr;
   ZH_SIZE nSize = *pnSize, nLen = 0;

   if( nSize == 0 )
      return ZH_FALSE;

   switch( *pBuffer++ )
   {
      case ZH_SERIAL_NIL:
      case ZH_SERIAL_TRUE:
      case ZH_SERIAL_FALSE:
      case ZH_SERIAL_ZERO:
      case ZH_SERIAL_STRNUL:
         nSize = 1;
         break;
      case ZH_SERIAL_INT8:
         nSize = 2;
         break;
      case ZH_SERIAL_INT8NUM:
      case ZH_SERIAL_INT16:
         nSize = 3;
         break;
      case ZH_SERIAL_INT16NUM:
      case ZH_SERIAL_INT24:
      case ZH_SERIAL_DATE:
         nSize = 4;
         break;
      case ZH_SERIAL_INT24NUM:
      case ZH_SERIAL_INT32:
         nSize = 5;
         break;
      case ZH_SERIAL_INT32NUM:
         nSize = 6;
         break;
      case ZH_SERIAL_INT64:
      case ZH_SERIAL_DOUBLE:
      case ZH_SERIAL_TIMESTAMP:
         nSize = 9;
         break;
      case ZH_SERIAL_INT64NUM:
         nSize = 10;
         break;
      case ZH_SERIAL_DBLNUM:
         nSize = 11;
         break;
      case ZH_SERIAL_SYMBOL:
      case ZH_SERIAL_STRING8:
         nSize = 2 + ( nSize >= 2 ? *pBuffer : nSize );
         break;
      case ZH_SERIAL_STRING16:
         nSize = 3 + ( nSize >= 3 ? ZH_GET_LE_UINT16( pBuffer ) : nSize );
         break;
      case ZH_SERIAL_STRING32:
         nSize = 5 + ( nSize >= 5 ? ZH_GET_LE_UINT32( pBuffer ) : nSize );
         break;
      case ZH_SERIAL_STRPAD8:
         nSize = 3 + ( nSize >= 3 ? *pBuffer : nSize );
         break;
      case ZH_SERIAL_STRPAD16:
         nSize = 5 + ( nSize >= 5 ? ZH_GET_LE_UINT16( pBuffer ) : nSize );
         break;
      case ZH_SERIAL_STRPAD32:
         nSize = 9 + ( nSize >= 9 ? ZH_GET_LE_UINT32( pBuffer ) : nSize );
         break;
      case ZH_SERIAL_ARRAYREF8:
         if( zh_itemSerialOffsetRef( pRefList, nOffset ) )
            return ZH_FALSE;
         /* fallthrough */
      case ZH_SERIAL_ARRAY8:
         if( nSize >= 2 )
         {
            nSize = 2;
            nLen = *pBuffer;
         }
         else
            nSize++;
         break;
      case ZH_SERIAL_ARRAYREF16:
         if( zh_itemSerialOffsetRef( pRefList, nOffset ) )
            return ZH_FALSE;
         /* fallthrough */
      case ZH_SERIAL_ARRAY16:
         if( nSize >= 3 )
         {
            nSize = 3;
            nLen = ZH_GET_LE_UINT16( pBuffer );
         }
         else
            nSize++;
         break;
      case ZH_SERIAL_ARRAYREF32:
         if( zh_itemSerialOffsetRef( pRefList, nOffset ) )
            return ZH_FALSE;
         /* fallthrough */
      case ZH_SERIAL_ARRAY32:
         if( nSize >= 5 )
         {
            nSize = 5;
            nLen = ZH_GET_LE_UINT32( pBuffer );
         }
         else
            nSize++;
         break;
      case ZH_SERIAL_HASHREF8:
         if( zh_itemSerialOffsetRef( pRefList, nOffset ) )
            return ZH_FALSE;
         /* fallthrough */
      case ZH_SERIAL_HASH8:
         if( nSize >= 2 )
         {
            nSize = 2;
            nLen = *pBuffer << 1;
         }
         else
            nSize++;
         break;
      case ZH_SERIAL_HASHREF16:
         if( zh_itemSerialOffsetRef( pRefList, nOffset ) )
            return ZH_FALSE;
         /* fallthrough */
      case ZH_SERIAL_HASH16:
         if( nSize >= 3 )
         {
            nSize = 3;
            nLen = ZH_GET_LE_UINT16( pBuffer ) << 1;
         }
         else
            nSize++;
         break;
      case ZH_SERIAL_HASHREF32:
         if( zh_itemSerialOffsetRef( pRefList, nOffset ) )
            return ZH_FALSE;
         /* fallthrough */
      case ZH_SERIAL_HASH32:
         if( nSize >= 5 )
         {
            nSize = 5;
            nLen = ZH_GET_LE_UINT32( pBuffer ) << 1;
         }
         else
            nSize++;
         break;
      case ZH_SERIAL_REF:
         if( ! zh_itemSerialOffsetRef( pRefList, ZH_GET_LE_UINT32( pBuffer ) ) )
            return ZH_FALSE;
         nSize = 5;
         break;
      case ZH_SERIAL_OBJ:
         nLen = zh_strnlen( ( const char * ) pBuffer, nSize - 1 ) + 1;
         if( nLen >= nSize )
            nSize++;
         else
         {
            nLen += zh_strnlen( ( const char * ) pBuffer + nLen, nSize - nLen - 1 ) + 2;
            if( nLen >= nSize )
               nSize++;
            else
               nSize = nLen;
         }
         nLen = 1;
         break;
      case ZH_SERIAL_HASHFLAGS:
         nSize = 3;
         nLen = 1;
         break;
      case ZH_SERIAL_HASHDEFVAL:
         nSize = 1;
         nLen = 2;
         break;
      case ZH_SERIAL_ZCOMPRESS:
         nSize = 9 + ( nSize >= 9 ? ZH_GET_LE_UINT32( pBuffer ) : nSize );
         break;

      /* xZiher types */
      case ZH_SERIAL_XZH_C:
         nSize = 9 + ( nSize >= 9 ? ( ZH_SIZE ) ZH_GET_BE_UINT64( pBuffer ) : nSize );
         break;
      case ZH_SERIAL_XZH_L:
         nSize = 2;
         break;
      case ZH_SERIAL_XZH_N:
         if( nSize >= 2 && *pBuffer == 'X' )
            /* this is workaround for bug in xZiher serialization code */
            nSize = 20;
         else
            nSize = 10;
         break;
      case ZH_SERIAL_XZH_D:
      case ZH_SERIAL_XZH_T:
         nSize = 9;
         break;
      case ZH_SERIAL_XZH_Z:
         nSize = 1;
         break;
      case ZH_SERIAL_XZH_A:
         if( nSize >= 9 )
         {
            nSize = 9;
            nLen = ( ZH_SIZE ) ZH_GET_BE_UINT64( pBuffer );
         }
         else
            nSize++;
         break;
      case ZH_SERIAL_XZH_B:
         nSize = 1;
         nLen = 1;
         break;
      case ZH_SERIAL_XZH_H:
         if( nSize >= 9 )
         {
            nSize = 9;
            nLen = ( ZH_SIZE ) ZH_GET_BE_UINT64( pBuffer ) << 1;
         }
         else
            nSize++;
         break;
      case ZH_SERIAL_XZH_O:
         if( nSize >= 9 )
         {
            nSize = 9;
            nLen = ( ( ZH_SIZE ) ZH_GET_BE_UINT64( pBuffer ) << 1 ) + 1;
         }
         else
            nSize++;
         break;
      case ZH_SERIAL_XZH_Q:
         if( nSize >= 18 && pBuffer[ 8 ] == ZH_SERIAL_XZH_C )
         {
            ZH_SIZE nData = ( ZH_SIZE ) ZH_GET_BE_UINT64( pBuffer );
            if( nData >= 9 && nData - 9 >=
                ( ZH_SIZE ) ZH_GET_BE_UINT64( &pBuffer[ 9 ] ) )
               nSize = 9 + nData;
            else
               nSize++;
         }
         else
            nSize++;
         nSize = 9 + ( nSize >= 9 ? ( ZH_SIZE ) ZH_GET_BE_UINT64( pBuffer ) : nSize );
         break;
      case ZH_SERIAL_XZH_R:
         if( nSize++ >= 10 )
         {
            switch( pBuffer[ 0 ] )
            {
               case ZH_SERIAL_XZH_A:
               case ZH_SERIAL_XZH_H:
               case ZH_SERIAL_XZH_O:
                  zh_itemSerialTypedRef( pRefList, pBuffer[ 0 ],
                              ( ZH_SIZE ) ZH_GET_BE_UINT64( &pBuffer[ 1 ] ) );
                  /* fallthrough */
               case ZH_SERIAL_XZH_B:
                  /* we do not support xZiher codeblock deserialization: ZH_RestoreBlock( pItem ) */
                  nSize = 10;
                  break;
            }
         }
         break;

      default:
         nSize = 1;
         break;
   }

   if( nSize > *pnSize )
      return ZH_FALSE;

   *pnSize -= nSize;
   *pBufferPtr += nSize;

   while( nLen )
   {
      nOffset += nSize;
      nSize = *pnSize;
      if( ! zh_deserializeTest( pBufferPtr, pnSize, nOffset, pRefList ) )
         return ZH_FALSE;
      nSize -= *pnSize;
      --nLen;
   }

   return ZH_TRUE;
}

static ZH_SIZE zh_deserializeHash( PZH_ITEM pItem,
                                   PZH_CODEPAGE cdpIn, PZH_CODEPAGE cdpOut,
                                   const ZH_UCHAR * pBuffer, ZH_SIZE nOffset,
                                   ZH_SIZE nLen, PZH_REF_LIST pRefList )
{
   zh_hashNew( pItem );

   if( nLen )
   {
#if 0
      PZH_ITEM pKey = zh_itemNew( NULL );
      PZH_ITEM pVal = zh_itemNew( NULL );

      zh_hashPreallocate( pItem, nLen );
      while( nLen-- )
      {
         nOffset = zh_deserializeItem( pKey, cdpIn, cdpOut, pBuffer, nOffset, pRefList );
         nOffset = zh_deserializeItem( pVal, cdpIn, cdpOut, pBuffer, nOffset, pRefList );
         zh_hashAdd( pItem, pKey, pVal );
      }
      zh_itemRelease( pKey );
      zh_itemRelease( pVal );
#else
      PZH_ITEM pKey, pVal;

      zh_hashSetFlags( pItem, ZH_HASH_BINARY | ZH_HASH_RESORT );
      zh_hashPreallocate( pItem, nLen );
      while( nLen-- )
      {
         if( zh_hashAllocNewPair( pItem, &pKey, &pVal ) )
         {
            nOffset = zh_deserializeItem( pKey, cdpIn, cdpOut, pBuffer, nOffset, pRefList );
            nOffset = zh_deserializeItem( pVal, cdpIn, cdpOut, pBuffer, nOffset, pRefList );
         }
      }
#endif
   }

   return nOffset;
}

static ZH_SIZE zh_deserializeArray( PZH_ITEM pItem,
                                    PZH_CODEPAGE cdpIn, PZH_CODEPAGE cdpOut,
                                    const ZH_UCHAR * pBuffer, ZH_SIZE nOffset,
                                    ZH_SIZE nLen, PZH_REF_LIST pRefList )
{
   ZH_SIZE u;

   zh_arrayNew( pItem, nLen );

   for( u = 1; u <= nLen; u++ )
      nOffset = zh_deserializeItem( zh_arrayGetItemPtr( pItem, u ),
                                    cdpIn, cdpOut, pBuffer, nOffset, pRefList );

   return nOffset;
}

static ZH_SIZE zh_deserializeItem( PZH_ITEM pItem,
                                   PZH_CODEPAGE cdpIn, PZH_CODEPAGE cdpOut,
                                   const ZH_UCHAR * pBuffer, ZH_SIZE nOffset,
                                   PZH_REF_LIST pRefList )
{
   ZH_SIZE nLen, nPad, nSize;
   char * szVal;

   switch( pBuffer[ nOffset++ ] )
   {
      case ZH_SERIAL_NIL:
         zh_itemClear( pItem );
         break;

      case ZH_SERIAL_TRUE:
         zh_itemPutL( pItem, ZH_TRUE );
         break;

      case ZH_SERIAL_FALSE:
         zh_itemPutL( pItem, ZH_FALSE );
         break;

      case ZH_SERIAL_ZERO:
         zh_itemPutNI( pItem, 0 );
         break;

      case ZH_SERIAL_INT8:
         zh_itemPutNI( pItem, ( signed char ) pBuffer[ nOffset++ ] );
         break;

      case ZH_SERIAL_INT16:
         zh_itemPutNI( pItem, ZH_GET_LE_INT16( &pBuffer[ nOffset ] ) );
         nOffset += 2;
         break;

      case ZH_SERIAL_INT24:
         zh_itemPutNInt( pItem, ZH_GET_LE_INT24( &pBuffer[ nOffset ] ) );
         nOffset += 3;
         break;

      case ZH_SERIAL_INT32:
         zh_itemPutNInt( pItem, ZH_GET_LE_INT32( &pBuffer[ nOffset ] ) );
         nOffset += 4;
         break;

      case ZH_SERIAL_INT64:
         zh_itemPutNInt( pItem, ZH_GET_LE_INT64( &pBuffer[ nOffset ] ) );
         nOffset += 8;
         break;

      case ZH_SERIAL_INT8NUM:
         zh_itemPutNILen( pItem, ( signed char ) pBuffer[ nOffset ],
                          pBuffer[ nOffset + 1 ] );
         nOffset += 2;
         break;

      case ZH_SERIAL_INT16NUM:
         zh_itemPutNILen( pItem, ZH_GET_LE_INT16( &pBuffer[ nOffset ] ),
                          pBuffer[ nOffset + 2 ] );
         nOffset += 3;
         break;

      case ZH_SERIAL_INT24NUM:
         zh_itemPutNIntLen( pItem, ZH_GET_LE_INT24( &pBuffer[ nOffset ] ),
                            pBuffer[ nOffset + 3 ] );
         nOffset += 4;
         break;

      case ZH_SERIAL_INT32NUM:
         zh_itemPutNIntLen( pItem, ZH_GET_LE_INT32( &pBuffer[ nOffset ] ),
                            pBuffer[ nOffset + 4 ] );
         nOffset += 5;
         break;

      case ZH_SERIAL_INT64NUM:
         zh_itemPutNIntLen( pItem, ZH_GET_LE_INT64( &pBuffer[ nOffset ] ),
                            pBuffer[ nOffset + 8 ] );
         nOffset += 9;
         break;

      case ZH_SERIAL_DOUBLE:
         zh_itemPutND( pItem, ZH_GET_LE_DOUBLE( &pBuffer[ nOffset ] ) );
         nOffset += 8;
         break;

      case ZH_SERIAL_DBLNUM:
         zh_itemPutNDLen( pItem, ZH_GET_LE_DOUBLE( &pBuffer[ nOffset ] ),
                          pBuffer[ nOffset + 8 ], pBuffer[ nOffset + 9 ] );
         nOffset += 10;
         break;

      case ZH_SERIAL_DATE:
         zh_itemPutDL( pItem, ZH_GET_LE_UINT24( &pBuffer[ nOffset ] ) );
         nOffset += 3;
         break;

      case ZH_SERIAL_TIMESTAMP:
         zh_itemPutTDT( pItem, ZH_GET_LE_UINT32( &pBuffer[ nOffset ] ),
                               ZH_GET_LE_UINT32( &pBuffer[ nOffset + 4 ] ) );
         nOffset += 8;
         break;

      case ZH_SERIAL_SYMBOL:
         nLen = pBuffer[ nOffset++ ];
         szVal = zh_strndup( ( const char * ) &pBuffer[ nOffset ], nLen );
         zh_itemPutSymbol( pItem, zh_dynsymGetSymbol( szVal ) );
         zh_xfree( szVal );
         nOffset += nLen;
         break;

      case ZH_SERIAL_STRNUL:
         zh_itemPutCL( pItem, NULL, 0 );
         break;
      case ZH_SERIAL_STRING8:
         nSize = nLen = pBuffer[ nOffset++ ];
         szVal = zh_cdpnDup( ( const char * ) &pBuffer[ nOffset ], &nLen,
                             cdpIn, cdpOut );
         zh_itemPutCLPtr( pItem, szVal, nLen );
         nOffset += nSize;
         break;
      case ZH_SERIAL_STRING16:
         nSize = nLen = ZH_GET_LE_UINT16( &pBuffer[ nOffset ] );
         nOffset += 2;
         szVal = zh_cdpnDup( ( const char * ) &pBuffer[ nOffset ], &nLen,
                             cdpIn, cdpOut );
         zh_itemPutCLPtr( pItem, szVal, nLen );
         nOffset += nSize;
         break;
      case ZH_SERIAL_STRING32:
         nSize = nLen = ZH_GET_LE_UINT32( &pBuffer[ nOffset ] );
         nOffset += 4;
         szVal = zh_cdpnDup( ( const char * ) &pBuffer[ nOffset ], &nLen,
                             cdpIn, cdpOut );
         zh_itemPutCLPtr( pItem, szVal, nLen );
         nOffset += nSize;
         break;
      case ZH_SERIAL_STRPAD8:
         nSize = pBuffer[ nOffset++ ];
         nPad = pBuffer[ nOffset++ ];
         nLen = zh_cdpnDupLen( ( const char * ) &pBuffer[ nOffset ], nSize,
                                cdpIn, cdpOut );
         szVal = ( char * ) zh_xgrab( nLen + nPad + 1 );
         zh_cdpnDup2( ( const char * ) &pBuffer[ nOffset ], nSize,
                      szVal, &nLen, cdpIn, cdpOut );
         memset( szVal + nLen, ' ', nPad );
         zh_itemPutCLPtr( pItem, szVal, nLen + nPad );
         nOffset += nSize;
         break;
      case ZH_SERIAL_STRPAD16:
         nSize = ZH_GET_LE_UINT16( &pBuffer[ nOffset ] );
         nOffset += 2;
         nPad = ZH_GET_LE_UINT16( &pBuffer[ nOffset ] );
         nOffset += 2;
         nLen = zh_cdpnDupLen( ( const char * ) &pBuffer[ nOffset ], nSize,
                                cdpIn, cdpOut );
         szVal = ( char * ) zh_xgrab( nLen + nPad + 1 );
         zh_cdpnDup2( ( const char * ) &pBuffer[ nOffset ], nSize,
                      szVal, &nLen, cdpIn, cdpOut );
         memset( szVal + nLen, ' ', nPad );
         zh_itemPutCLPtr( pItem, szVal, nLen + nPad );
         nOffset += nSize;
         break;
      case ZH_SERIAL_STRPAD32:
         nSize = ZH_GET_LE_UINT32( &pBuffer[ nOffset ] );
         nOffset += 4;
         nPad = ZH_GET_LE_UINT32( &pBuffer[ nOffset ] );
         nOffset += 4;
         nLen = zh_cdpnDupLen( ( const char * ) &pBuffer[ nOffset ], nSize,
                                cdpIn, cdpOut );
         szVal = ( char * ) zh_xgrab( nLen + nPad + 1 );
         zh_cdpnDup2( ( const char * ) &pBuffer[ nOffset ], nSize,
                      szVal, &nLen, cdpIn, cdpOut );
         zh_xmemset( szVal + nLen, ' ', nPad );
         zh_itemPutCLPtr( pItem, szVal, nLen + nPad );
         nOffset += nSize;
         break;

      case ZH_SERIAL_ARRAYREF8:
         zh_itemSerialOffsetSet( pRefList, pItem, nOffset - 1 );
         /* fallthrough */
      case ZH_SERIAL_ARRAY8:
         nLen = pBuffer[ nOffset++ ];
         nOffset = zh_deserializeArray( pItem, cdpIn, cdpOut, pBuffer, nOffset, nLen, pRefList );
         break;
      case ZH_SERIAL_ARRAYREF16:
         zh_itemSerialOffsetSet( pRefList, pItem, nOffset - 1 );
         /* fallthrough */
      case ZH_SERIAL_ARRAY16:
         nLen = ZH_GET_LE_UINT16( &pBuffer[ nOffset ] );
         nOffset = zh_deserializeArray( pItem, cdpIn, cdpOut, pBuffer, nOffset + 2, nLen, pRefList );
         break;
      case ZH_SERIAL_ARRAYREF32:
         zh_itemSerialOffsetSet( pRefList, pItem, nOffset - 1 );
         /* fallthrough */
      case ZH_SERIAL_ARRAY32:
         nLen = ZH_GET_LE_UINT32( &pBuffer[ nOffset ] );
         nOffset = zh_deserializeArray( pItem, cdpIn, cdpOut, pBuffer, nOffset + 4, nLen, pRefList );
         break;

      case ZH_SERIAL_HASHREF8:
         zh_itemSerialOffsetSet( pRefList, pItem, nOffset - 1 );
         /* fallthrough */
      case ZH_SERIAL_HASH8:
         nLen = pBuffer[ nOffset++ ];
         nOffset = zh_deserializeHash( pItem, cdpIn, cdpOut, pBuffer, nOffset, nLen, pRefList );
         break;
      case ZH_SERIAL_HASHREF16:
         zh_itemSerialOffsetSet( pRefList, pItem, nOffset - 1 );
         /* fallthrough */
      case ZH_SERIAL_HASH16:
         nLen = ZH_GET_LE_UINT16( &pBuffer[ nOffset ] );
         nOffset = zh_deserializeHash( pItem, cdpIn, cdpOut, pBuffer, nOffset + 2, nLen, pRefList );
         break;
      case ZH_SERIAL_HASHREF32:
         zh_itemSerialOffsetSet( pRefList, pItem, nOffset - 1 );
         /* fallthrough */
      case ZH_SERIAL_HASH32:
         nLen = ZH_GET_LE_UINT32( &pBuffer[ nOffset ] );
         nOffset = zh_deserializeHash( pItem, cdpIn, cdpOut, pBuffer, nOffset + 4, nLen, pRefList );
         break;

      case ZH_SERIAL_REF:
         zh_itemSerialOffsetGet( pRefList, pItem,
                                 ZH_GET_LE_UINT32( &pBuffer[ nOffset ] ) );
         nOffset += 4;
         break;

      case ZH_SERIAL_OBJ:
      {
         const char * szClass, * szFunc;
         szClass = ( const char * ) &pBuffer[ nOffset ];
         nLen = strlen( szClass );
         szFunc = szClass + nLen + 1;
         nOffset = zh_deserializeItem( pItem, cdpIn, cdpOut, pBuffer,
                              nOffset + nLen + strlen( szFunc ) + 2, pRefList );
         zh_objSetClass( pItem, szClass, szFunc );
         break;
      }

      case ZH_SERIAL_HASHFLAGS:
      {
         int iHashFlags = ZH_GET_LE_UINT16( &pBuffer[ nOffset ] );
         nOffset = zh_deserializeItem( pItem, cdpIn, cdpOut, pBuffer,
                                       nOffset + 2, pRefList );
         zh_hashClearFlags( pItem, ZH_HASH_FLAG_MASK );
         if( ( iHashFlags & ( ZH_HASH_KEEPORDER | ZH_HASH_BINARY ) ) != ZH_HASH_BINARY )
            iHashFlags |= ZH_HASH_RESORT;
         zh_hashSetFlags( pItem, iHashFlags );
         break;
      }

      case ZH_SERIAL_HASHDEFVAL:
      {
         PZH_ITEM pDefVal = zh_itemNew( NULL );
         nOffset = zh_deserializeItem( pDefVal, cdpIn, cdpOut, pBuffer,
                                       nOffset, pRefList );
         nOffset = zh_deserializeItem( pItem, cdpIn, cdpOut, pBuffer,
                                       nOffset, pRefList );
         zh_hashSetDefault( pItem, pDefVal );
         zh_itemRelease( pDefVal );
         break;
      }

      case ZH_SERIAL_ZCOMPRESS:
         nSize = ZH_GET_LE_UINT32( &pBuffer[ nOffset ] );
         nOffset += 4;
         nLen = ZH_GET_LE_UINT32( &pBuffer[ nOffset ] );
         nOffset += 4;
         szVal = ( char * ) zh_xgrab( nLen + 1 );
         switch( zh_zlibUncompress( szVal, &nLen,
                                    ( const char * ) &pBuffer[ nOffset ], nSize ) )
         {
            case ZH_ZLIB_RES_OK:
            {
               ZH_REF_LIST refListZ;

               zh_itemSerialRefListInit( &refListZ );
               pBuffer = ( const ZH_UCHAR * ) szVal;
               if( zh_deserializeTest( &pBuffer, &nLen, 0, &refListZ ) )
                  zh_deserializeItem( pItem, cdpIn, cdpOut, ( const ZH_UCHAR * ) szVal, 0, &refListZ );
               else
                  zh_itemClear( pItem );
               zh_itemSerialRefListFree( &refListZ );
               break;
            }
            case ZH_ZLIB_RES_UNSUPPORTED:
               if( zh_vmRequestQuery() == 0 )
               {
                  zh_itemPutCLPtr( pItem, szVal, nLen );
                  zh_errRT_BASE_Ext1( EG_ARG, 3016, NULL, ZH_ERR_FUNCNAME, 0, EF_CANDEFAULT, 1, pItem );
                  szVal = NULL;
               }
               /* fallthrough */

            default:
               zh_itemClear( pItem );
         }

         if( szVal )
            zh_xfree( szVal );
         nOffset += nSize;
         break;

      /* xZiher types */
      case ZH_SERIAL_XZH_C:
         nSize = nLen = ( ZH_SIZE ) ZH_GET_BE_UINT64( &pBuffer[ nOffset ] );
         nOffset += 8;
         szVal = zh_cdpnDup( ( const char * ) &pBuffer[ nOffset ], &nLen,
                             cdpIn, cdpOut );
         zh_itemPutCLPtr( pItem, szVal, nLen );
         nOffset += nSize;
         break;
      case ZH_SERIAL_XZH_L:
         zh_itemPutL( pItem, pBuffer[ nOffset++ ] == 'T' );
         break;
      case ZH_SERIAL_XZH_N:
         switch( pBuffer[ nOffset++ ] )
         {
            case 'I':
               zh_itemPutNI( pItem, ( int ) ZH_GET_BE_UINT64( &pBuffer[ nOffset ] ) );
               break;
            case 'L':
               zh_itemPutNL( pItem, ( long ) ZH_GET_BE_UINT64( &pBuffer[ nOffset ] ) );
               break;
            case 'X':
               zh_itemPutNInt( pItem, ( ZH_MAXINT ) ZH_GET_BE_UINT64( &pBuffer[ nOffset ] ) );
               /* this is workaround for bug in xZiher serialization code */
               nOffset += 10;
               break;
            case 'D':
               zh_itemPutND( pItem, ZH_GET_LE_DOUBLE( &pBuffer[ nOffset ] ) );
               break;
            default:
               zh_itemClear( pItem );
               break;
         }
         nOffset += 8;
         break;
      case ZH_SERIAL_XZH_D:
         zh_itemPutDL( pItem, ( long ) ZH_GET_BE_UINT64( &pBuffer[ nOffset ] ) );
         nOffset += 8;
         break;
      case ZH_SERIAL_XZH_T:
         zh_itemPutTD( pItem, ZH_GET_LE_DOUBLE( &pBuffer[ nOffset ] ) );
         nOffset += 8;
         break;
      case ZH_SERIAL_XZH_Z:
         zh_itemClear( pItem );
         break;
      case ZH_SERIAL_XZH_A:
         nLen = ( ZH_SIZE ) ZH_GET_BE_UINT64( &pBuffer[ nOffset ] );
         zh_itemSerialTypedSet( pRefList, pItem, ZH_SERIAL_XZH_A );
         nOffset = zh_deserializeArray( pItem, cdpIn, cdpOut, pBuffer, nOffset + 8, nLen, pRefList );
         break;
      case ZH_SERIAL_XZH_B:
         nOffset = zh_deserializeItem( pItem, cdpIn, cdpOut, pBuffer,
                                       nOffset, pRefList );
         /* we do not support xZiher codeblock deserialization: ZH_RestoreBlock( pItem ) */
         #if 0
         zh_itemSerialTypedSet( pRefList, pItem, ZH_SERIAL_XZH_B );
         #endif
         zh_itemClear( pItem );
         break;
      case ZH_SERIAL_XZH_H:
         nLen = ( ZH_SIZE ) ZH_GET_BE_UINT64( &pBuffer[ nOffset ] );
         zh_itemSerialTypedSet( pRefList, pItem, ZH_SERIAL_XZH_H );
         nOffset = zh_deserializeHash( pItem, cdpIn, cdpOut, pBuffer, nOffset + 8, nLen, pRefList );
         zh_hashSetFlags( pItem, ZH_HASH_KEEPORDER | ZH_HASH_RESORT );
         break;
      case ZH_SERIAL_XZH_O:
      {
         ZH_USHORT uiClass;

         nLen = ( ZH_SIZE ) ZH_GET_BE_UINT64( &pBuffer[ nOffset ] );
         /* deserialize :className */
         nOffset = zh_deserializeItem( pItem, cdpIn, cdpOut, pBuffer,
                                       nOffset + 8, pRefList );
         /* find class handle */
         uiClass = zh_clsFindClass( zh_itemGetCPtr( pItem ), NULL );
         if( uiClass && zh_vmRequestReenter() )
         {
            PZH_ITEM pMsg = zh_stackAllocItem(),
                     pVal = zh_stackAllocItem();

            zh_clsAssociate( uiClass );
            zh_itemMove( pItem, zh_stackReturnItem() );
            zh_itemSerialTypedSet( pRefList, pItem, ZH_SERIAL_XZH_O );

            while( nLen-- )
            {
               nOffset = zh_deserializeItem( pMsg, cdpIn, cdpOut, pBuffer,
                                             nOffset, pRefList );
               nOffset = zh_deserializeItem( pVal, cdpIn, cdpOut, pBuffer,
                                             nOffset, pRefList );
               if( zh_vmRequestQuery() == 0 )
               {
                  char szMsg[ ZH_SYMBOL_NAME_LEN ];
                  zh_snprintf( szMsg, sizeof( szMsg ), "_%s", zh_itemGetCPtr( pMsg ) );
                  zh_objSendMsg( pItem, szMsg, 1, pVal );
               }
            }
            zh_stackPop();
            zh_stackPop();
            zh_vmRequestRestore();
         }
         else
         {
            zh_itemSerialTypedSet( pRefList, pItem, ZH_SERIAL_XZH_O );
            while( nLen-- )
            {
               nOffset = zh_deserializeItem( pItem, cdpIn, cdpOut, pBuffer,
                                             nOffset, pRefList );
               nOffset = zh_deserializeItem( pItem, cdpIn, cdpOut, pBuffer,
                                             nOffset, pRefList );
            }
            zh_itemClear( pItem );
         }
         break;
      }
      case ZH_SERIAL_XZH_Q:
      {
         ZH_USHORT uiClass;

         nPad = ( ZH_SIZE ) ZH_GET_BE_UINT64( &pBuffer[ nOffset ] ) + nOffset + 8;
         /* deserialize :className */
         nOffset = zh_deserializeItem( pItem, cdpIn, cdpOut, pBuffer,
                                       nOffset + 8, pRefList );
         nLen = nPad - nOffset;
         /* get serialized HBPERSISTENT text */
         szVal = zh_cdpnDup( ( const char * ) &pBuffer[ nOffset ], &nLen,
                             cdpIn, cdpOut );
         nOffset = nPad;
         /* find class handle */
         uiClass = zh_clsFindClass( zh_itemGetCPtr( pItem ), NULL );
         zh_itemPutCLPtr( pItem, szVal, nLen );
         if( uiClass && zh_vmRequestReenter() )
         {
            zh_clsAssociate( uiClass );
            zh_vmPushDynSym( zh_dynsymGetCase( "LOADFROMTEXT" ) );
            zh_vmPush( zh_stackReturnItem() );
            zh_vmPush( pItem );
            zh_vmPushLogical( ZH_TRUE );
            zh_itemMove( pItem, zh_stackReturnItem() );
            zh_vmSend( 2 );
            zh_vmRequestRestore();
         }
         else
            zh_itemClear( pItem );
         zh_itemSerialTypedSet( pRefList, pItem, ZH_SERIAL_XZH_O );
         break;
      }
      case ZH_SERIAL_XZH_R:
         zh_itemSerialTypedGet( pRefList, pItem, pBuffer[ nOffset ],
                     ( ZH_SIZE ) ZH_GET_BE_UINT64( &pBuffer[ nOffset + 1 ] ) );
         nOffset += 9;
         break;

      default:
         zh_itemClear( pItem );
         break;
   }

   return nOffset;
}

/*
 * public API functions
 */
char * zh_itemSerializeCP( PZH_ITEM pItem, int iFlags,
                           PZH_CODEPAGE cdpIn, PZH_CODEPAGE cdpOut,
                           ZH_SIZE * pnSize )
{
   ZH_REF_LIST refList;
   ZH_UCHAR * pBuffer;
   ZH_SIZE nSize;

   zh_itemSerialRefListInit( &refList );
   nSize = zh_itemSerialSize( pItem, iFlags, cdpIn, cdpOut, &refList, 0 );
   pBuffer = ( ZH_UCHAR * ) zh_xgrab( nSize + 1 );
   zh_itemSerialUnusedFree( &refList );
   zh_serializeItem( pItem, iFlags, cdpIn, cdpOut, pBuffer, 0, &refList );
   zh_itemSerialRefListFree( &refList );

   if( ( iFlags & ZH_SERIALIZE_COMPRESS ) != 0 && nSize > 20 )
   {
      ZH_SIZE nDest = zh_zlibCompressBound( nSize );
      if( nDest > 0 )
      {
         char * pDest = ( char * ) zh_xgrab( nDest );
         if( zh_zlibCompress( pDest, &nDest, ( const char * ) pBuffer, nSize,
                           ZH_ZLIB_COMPRESSION_DEFAULT ) == ZH_ZLIB_RES_OK )
         {
            if( nDest + 9 < nSize )
            {
               pBuffer[ 0 ] = ZH_SERIAL_ZCOMPRESS;
               ZH_PUT_LE_UINT32( &pBuffer[ 1 ], nDest );
               ZH_PUT_LE_UINT32( &pBuffer[ 5 ], nSize );
               memcpy( &pBuffer[ 9 ], pDest, nDest );
               nSize = nDest + 9;
               pBuffer = ( ZH_UCHAR * ) zh_xrealloc( pBuffer, nSize + 1 );
            }
         }
         zh_xfree( pDest );
      }
   }

   pBuffer[ nSize ] = '\0';
   if( pnSize )
      *pnSize = nSize;

   return ( char * ) pBuffer;
}

char * zh_itemSerialize( PZH_ITEM pItem, int iFlags, ZH_SIZE *pnSize )
{
   return zh_itemSerializeCP( pItem, iFlags, NULL, NULL, pnSize );
}

PZH_ITEM zh_itemDeserializeCP( const char ** pBufferPtr, ZH_SIZE * pnSize,
                               PZH_CODEPAGE cdpIn, PZH_CODEPAGE cdpOut )
{
   const ZH_UCHAR * pBuffer = ( const ZH_UCHAR * ) *pBufferPtr;
   PZH_ITEM pItem = NULL;
   ZH_REF_LIST refList;

   zh_itemSerialRefListInit( &refList );
   if( ! pnSize || zh_deserializeTest( ( const ZH_UCHAR ** ) pBufferPtr, pnSize, 0, &refList ) )
   {
      pItem = zh_itemNew( NULL );
      zh_deserializeItem( pItem, cdpIn, cdpOut, pBuffer, 0, &refList );
   }
   zh_itemSerialRefListFree( &refList );

   return pItem;
}

PZH_ITEM zh_itemDeserialize( const char ** pBufferPtr, ZH_SIZE * pnSize )
{
   return zh_itemDeserializeCP( pBufferPtr, pnSize, NULL, NULL );
}

ZH_FUNC( ZH_SERIALIZE )
{
   PZH_ITEM pItem = zh_param( 1, ZH_IT_ANY );

   if( pItem )
   {
      PZH_CODEPAGE cdpIn, cdpOut;
      const char * pszCdpIn, * pszCdpOut;
      char * pBuffer;
      ZH_SIZE nSize;
      int iFlags;

      pszCdpIn = zh_parc( 3 );
      pszCdpOut = zh_parc( 4 );

      cdpIn = pszCdpIn ? zh_cdpFindExt( pszCdpIn ) : zh_vmCDP();
      cdpOut = pszCdpOut ? zh_cdpFindExt( pszCdpOut ) : zh_vmCDP();

      if( ZH_IS_PARAM_NUM( 2 ) )
         iFlags = zh_parni( 2 );
      else
         iFlags = zh_parl( 2 ) ? ZH_SERIALIZE_NUMSIZE : 0;

      pBuffer = zh_itemSerializeCP( pItem, iFlags, cdpIn, cdpOut, &nSize );
      zh_retclen_buffer( pBuffer, nSize );
   }
}

ZH_FUNC( ZH_DESERIALIZE )
{
   PZH_ITEM pParam = zh_param( 1, ZH_IT_BYREF );
   ZH_SIZE nSize = zh_parclen( 1 );

   if( nSize )
   {
      PZH_ITEM pItem;
      PZH_CODEPAGE cdpIn, cdpOut;
      const char * pBuffer = zh_parc( 1 );
      const char * pszCdpIn = zh_parc( 2 ),
                 * pszCdpOut = zh_parc( 3 );

      cdpIn = pszCdpIn ? zh_cdpFindExt( pszCdpIn ) : zh_vmCDP();
      cdpOut = pszCdpOut ? zh_cdpFindExt( pszCdpOut ) : zh_vmCDP();

      pItem = zh_itemDeserializeCP( &pBuffer, &nSize, cdpIn, cdpOut );
      if( pItem )
      {
         zh_itemReturn( pItem );
         if( pParam )
         {
            zh_itemPutCL( pItem, pBuffer, nSize );
            zh_itemMove( pParam, pItem );
         }
         zh_itemRelease( pItem );
      }
      else if( pParam )
         zh_itemClear( pParam );
   }
   else if( pParam )
      zh_itemClear( pParam );
}
