/*
 * DBFNSX RDD
 *
 * Copyright 2008 Przemyslaw Czerpak
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


#define ZH_NSX_SIX_STRICT
#define ZH_NSX_CLEAR_UNUSED
/* #define ZH_NSX_EXTERNAL_PAGEBUFFER */

/*
#define ZH_NSX_DEBUG
#define ZH_NSX_DEBUG_EXT
*/

#include "zh_api.h"
#include "zh_apifs.h"
#include "hbrddnsx.h"
#include "zh_item_api.h"
#include "zh_api_error.h"
#include "zh_init.h"
#include "zh_lang_api.h"
#include "zh_vm.h"
#include "zh_set.h"
#include "zh_stack.h"
#include "zh_math.h"
#include "rdd_sys.zhh"
#include "hbregex.h"

static RDDFUNCS  nsxSuper;
static ZH_USHORT s_uiRddId;

/* temporary casts to suppress 32/64-bit Windows warnings */
#define ZH_USHORTCAST  ZH_USHORT
#define ZH_INTCAST     int

#define zh_nsxKeyFree( K )                zh_xfree( K )
#define zh_nsxFileOffset( I, B )          ( ( ZH_FOFFSET ) ( B ) << ( ( I )->LargeFile ? NSX_PAGELEN_BITS : 0 ) )
#define zh_nsxGetRecSize( r )             ( ( r ) < 0x10000 ? 2 : ( ( r ) < 0x1000000 ? 3 : 4 ) )
#define zh_nsxPageBuffer( p )             ( ( p )->data.buffer )
#define zh_nsxIsLeaf( p )                 ( ( ( p )->data.buffer[ 0 ] & NSX_LEAFPAGE ) != 0 )
#define zh_nsxIsRoot( p )                 ( ( ( p )->data.buffer[ 0 ] & NSX_ROOTPAGE ) != 0 )
#define zh_nsxPageType( p )               ( zh_nsxPageBuffer( p )[ 0 ] )
#define zh_nsxSetPageType( p, t )         do { zh_nsxPageBuffer( p )[ 0 ] = ( t ); } while( 0 )
#define zh_nsxGetKeyRecSize( p )          ( zh_nsxPageBuffer( p )[ 1 ] )
#define zh_nsxGetKeyRecSizePtr( p )       ( ( p )[ 1 ] )
#define zh_nsxSetKeyRecSize( p, n )       do { zh_nsxPageBuffer( p )[ 1 ] = ( n ); } while( 0 )
#define zh_nsxGetBranchKeyPtr( p, l, n )  ( zh_nsxPageBuffer( p ) + ( n ) * ( ( l ) + 8 ) + 8 )
#define zh_nsxBranchKeyVal( p )           ( ( p ) + 8 )
#define zh_nsxBranchKeySize( p, l )       ( ( l ) + 8 )
/*
 * The helper functions (endian dependent) - on big endian machines
 * or RISC with strict alignment it's much better to use functions
 * then macros to inform compiler that can count complex parameters
 * only once.
 * On other machines it should not cause noticeable differences because
 * most of modern C compilers auto inline small functions
 */
#if defined( ZH_LITTLE_ENDIAN ) && ! defined( ZH_STRICT_ALIGNMENT ) && 1

#define zh_nsxGetKeyCount( p )            ZH_GET_LE_UINT16( zh_nsxPageBuffer( p ) + 2 )
#define zh_nsxSetKeyCount( p, n )         ZH_PUT_LE_UINT16( zh_nsxPageBuffer( p ) + 2, ( n ) )

#define zh_nsxLeafGetFreeOffset( p )      ZH_GET_LE_UINT16( zh_nsxPageBuffer( p ) + 4 )
#define zh_nsxLeafSetFreeOffset( p, n )   ZH_PUT_LE_UINT16( zh_nsxPageBuffer( p ) + 4, ( n ) )

#define zh_nsxGetLowerPage( p )           ZH_GET_LE_UINT32( zh_nsxPageBuffer( p ) + 4 )
#define zh_nsxSetLowerPage( p, n )        ZH_PUT_LE_UINT32( zh_nsxPageBuffer( p ) + 4, ( n ) )

#define zh_nsxGetKeyPage( p, l, n )       ZH_GET_LE_UINT32( zh_nsxGetBranchKeyPtr( p, l, n ) )
#define zh_nsxSetKeyPage( p, l, n, u )    ZH_PUT_LE_UINT32( zh_nsxGetBranchKeyPtr( p, l, n ), ( u ) )

#define zh_nsxGetKeyRec( p, l, n )        ZH_GET_LE_UINT32( zh_nsxGetBranchKeyPtr( p, l, n ) + 4 )
#define zh_nsxSetKeyRec( p, l, n, r )     ZH_PUT_LE_UINT32( zh_nsxGetBranchKeyPtr( p, l, n ) + 4, ( r ) )

#define zh_nsxGetKeyVal( p, l, n )        ( zh_nsxGetBranchKeyPtr( p, l, n ) + 8 )

#define zh_nsxBranchKeyPage( p )          ZH_GET_LE_UINT32( p )
#define zh_nsxBranchKeyRec( p )           ZH_GET_LE_UINT32( ( p ) + 4 )
#define zh_nsxBranchKeySetPage( p, u )    do { ZH_PUT_LE_UINT32( ( p ), ( u ) ); } while( 0 )
#define zh_nsxBranchKeySetRec( p, u )     do { ZH_PUT_LE_UINT32( ( p ) + 4, ( u ) ); } while( 0 )

#else

static ZH_USHORT zh_nsxGetKeyCount( LPPAGEINFO pPage )
{
   ZH_UCHAR * ptr = zh_nsxPageBuffer( pPage ) + 2;

   return ZH_GET_LE_UINT16( ptr );
}

static void zh_nsxSetKeyCount( LPPAGEINFO pPage, ZH_USHORT uiKeys )
{
   ZH_UCHAR * ptr = zh_nsxPageBuffer( pPage ) + 2;

   ZH_PUT_LE_UINT16( ptr, uiKeys );
}

static ZH_ULONG zh_nsxGetLowerPage( LPPAGEINFO pPage )
{
   ZH_UCHAR * ptr = zh_nsxPageBuffer( pPage ) + 4;

   return ZH_GET_LE_UINT32( ptr );
}

static void zh_nsxSetLowerPage( LPPAGEINFO pPage, ZH_ULONG ulPage )
{
   ZH_UCHAR * ptr = zh_nsxPageBuffer( pPage ) + 4;

   ZH_PUT_LE_UINT32( ptr, ulPage );
}

static ZH_ULONG zh_nsxGetKeyPage( LPPAGEINFO pPage, ZH_USHORT uiLen, ZH_USHORT uiKey )
{
   ZH_UCHAR * ptr = zh_nsxGetBranchKeyPtr( pPage, uiLen, uiKey );

   return ZH_GET_LE_UINT32( ptr );
}

static void zh_nsxSetKeyPage( LPPAGEINFO pPage, ZH_USHORT uiLen, ZH_USHORT uiKey, ZH_ULONG ulPage )
{
   ZH_UCHAR * ptr = zh_nsxGetBranchKeyPtr( pPage, uiLen, uiKey );

   ZH_PUT_LE_UINT32( ptr, ulPage );
}

static ZH_ULONG zh_nsxGetKeyRec( LPPAGEINFO pPage, ZH_USHORT uiLen, ZH_USHORT uiKey )
{
   ZH_UCHAR * ptr = zh_nsxGetBranchKeyPtr( pPage, uiLen, uiKey ) + 4;

   return ZH_GET_LE_UINT32( ptr );
}

static void zh_nsxSetKeyRec( LPPAGEINFO pPage, ZH_USHORT uiLen, ZH_USHORT uiKey, ZH_ULONG ulRec )
{
   ZH_UCHAR * ptr = zh_nsxGetBranchKeyPtr( pPage, uiLen, uiKey ) + 4;

   ZH_PUT_LE_UINT32( ptr, ulRec );
}

static ZH_UCHAR * zh_nsxGetKeyVal( LPPAGEINFO pPage, ZH_USHORT uiLen, ZH_USHORT uiKey )
{
   return zh_nsxGetBranchKeyPtr( pPage, uiLen, uiKey ) + 8;
}

static void zh_nsxBranchKeySetPage( ZH_UCHAR * ptr, ZH_ULONG ulPage )
{
   ZH_PUT_LE_UINT32( ptr, ulPage );
}

static void zh_nsxBranchKeySetRec( ZH_UCHAR * ptr, ZH_ULONG ulRec )
{
   ptr += 4;
   ZH_PUT_LE_UINT32( ptr, ulRec );
}

static ZH_ULONG zh_nsxBranchKeyPage( ZH_UCHAR * ptr )
{
   return ZH_GET_LE_UINT32( ptr );
}

static ZH_ULONG zh_nsxBranchKeyRec( ZH_UCHAR * ptr )
{
   ptr += 4;
   return ZH_GET_LE_UINT32( ptr );
}

static ZH_USHORT zh_nsxLeafGetFreeOffset( LPPAGEINFO pPage )
{
   ZH_UCHAR * ptr = zh_nsxPageBuffer( pPage ) + 4;

   return ZH_GET_LE_UINT16( ptr );
}

static void zh_nsxLeafSetFreeOffset( LPPAGEINFO pPage, ZH_USHORT uiOffset )
{
   ZH_UCHAR * ptr = zh_nsxPageBuffer( pPage ) + 4;

   ZH_PUT_LE_UINT16( ptr, uiOffset );
}

#endif

/* #define ZH_NSX_NO_CORRUPT_PROTECT */

static ZH_USHORT zh_nsxLeafGetKey( LPTAGINFO pTag, LPPAGEINFO pPage, ZH_USHORT uiOffset,
                                   ZH_UCHAR * bPrevValue, ZH_ULONG * pulRecNo )
{
   ZH_UCHAR * ptr = ( ZH_UCHAR * ) zh_nsxPageBuffer( pPage );
   ZH_UCHAR ucRecLen = zh_nsxGetKeyRecSizePtr( ptr ), ucSize;

#ifndef ZH_NSX_NO_CORRUPT_PROTECT
   /* protection against corrupted NSX files */
   if( ucRecLen + uiOffset >= pPage->uiOffset )
      return 0;
#endif /* ZH_NSX_NO_CORRUPT_PROTECT */

   switch( ucRecLen )
   {
      case 1:
         *pulRecNo = ptr[ uiOffset++ ];
         break;
      case 2:
         *pulRecNo = ZH_GET_LE_UINT16( &ptr[ uiOffset ] );
         uiOffset += 2;
         break;
      case 3:
         *pulRecNo = ZH_GET_LE_UINT24( &ptr[ uiOffset ] );
         uiOffset += 3;
         break;
      case 4:
         *pulRecNo = ZH_GET_LE_UINT32( &ptr[ uiOffset ] );
         uiOffset += 4;
         break;
      default:
         /* protection against corrupted NSX files */
         return 0;
   }
   ucSize = ptr[ uiOffset++ ];
   if( ucSize != ucRecLen + 1 ) /* key value is not fully duplicated */
   {
      ZH_UCHAR len = ( ZH_UCHAR ) pTag->KeyLength;
      ZH_UCHAR ucDupCount;

      /* ucSize = 0 is a special case when RecLen is 4 and KeySize is 250
       * in such case ucSize - ( ucRecLen + 2 ) gives 250 = NSX_MAXKEYLEN
       */
      ucSize -= ucRecLen + 2;

#ifndef ZH_NSX_NO_CORRUPT_PROTECT
      /* protection against corrupted NSX files */
      if( ucSize > NSX_MAXKEYLEN || uiOffset + ucSize >= pPage->uiOffset )
         return 0;
#endif /* ZH_NSX_NO_CORRUPT_PROTECT */

      ucDupCount = ptr[ uiOffset++ ];
      if( ucSize + ucDupCount == len )
      {
         /* key value is stored as raw data and can be copied as is */
         memcpy( &bPrevValue[ ucDupCount ], &ptr[ uiOffset ], ucSize );
         uiOffset += ucSize;
      }
#ifndef ZH_NSX_NO_CORRUPT_PROTECT
      /* protection against corrupted NSX files */
      else if( ucSize + ucDupCount > len )
         return 0;
#endif /* ZH_NSX_NO_CORRUPT_PROTECT */

      else
      {
         while( ucSize-- )
         {
            ZH_UCHAR uc = ptr[ uiOffset++ ];

            if( uc == NSX_RLE_CHAR )
            {
               ZH_UCHAR ucRepl;

#ifndef ZH_NSX_NO_CORRUPT_PROTECT
               /* protection against corrupted NSX files */
               if( ! ucSize-- )
                  return 0;
#else
               --ucSize;
#endif /* ZH_NSX_NO_CORRUPT_PROTECT */

               if( ( ucRepl = ptr[ uiOffset++ ] ) != 1 )
               {
#ifndef ZH_NSX_NO_CORRUPT_PROTECT
                  /* protection against corrupted NSX files */
                  if( ! ucSize-- || ucRepl + ucDupCount > len )
                     return 0;
#else
                  --ucSize;
#endif /* ZH_NSX_NO_CORRUPT_PROTECT */

                  uc = ptr[ uiOffset++ ];
                  while( ucRepl-- )
                     bPrevValue[ ucDupCount++ ] = uc;
                  continue;
               }
            }

#ifndef ZH_NSX_NO_CORRUPT_PROTECT
            /* protection against corrupted NSX files */
            if( ucDupCount >= len )
               return 0;
#endif /* ZH_NSX_NO_CORRUPT_PROTECT */

            bPrevValue[ ucDupCount++ ] = uc;
         }

         while( ucDupCount < len )
            bPrevValue[ ucDupCount++ ] = pTag->TrailChar;
      }
   }
   return uiOffset;
}

static ZH_USHORT zh_nsxLeafPutKey( LPTAGINFO pTag, LPPAGEINFO pPage, ZH_USHORT uiOffset,
                                   ZH_UCHAR * bPrevValue, ZH_UCHAR * pKeyValue, ZH_ULONG ulRecNo )
{
   ZH_UCHAR * ptr = ( ZH_UCHAR * ) zh_nsxPageBuffer( pPage ) + uiOffset,
            * pDst, * pSrc, * pEnd;
   ZH_UCHAR ucSize = zh_nsxGetKeyRecSize( pPage ), ucDupCount = 0,
            ucLen = ( ZH_UCHAR ) pTag->KeyLength;
   int iMax;

   if( uiOffset + ucSize >= NSX_PAGELEN )
      return 0;

   switch( ucSize )
   {
      case 1:
         *ptr++ = ( ZH_UCHAR ) ulRecNo;
         break;
      case 2:
         ZH_PUT_LE_UINT16( ptr, ulRecNo );
         ptr += 2;
         break;
      case 3:
         ZH_PUT_LE_UINT24( ptr, ulRecNo );
         ptr += 3;
         break;
      default:
         ZH_PUT_LE_UINT32( ptr, ulRecNo );
         ptr += 4;
         break;
   }
   ++ucSize;   /* Size */

   if( bPrevValue )
   {
      while( bPrevValue[ ucDupCount ] == pKeyValue[ ucDupCount ] )
      {
         ++ucDupCount;
         if( --ucLen == 0 )
         {
            *ptr = ucSize;
            return uiOffset + ucSize;
         }
      }
   }
   ++ucSize;   /* DupCount */

   if( uiOffset + ucSize > NSX_PAGELEN )
      return 0;

   ptr[ 1 ] = ucDupCount;

   pSrc = &pKeyValue[ ucDupCount ];
   pEnd = pSrc + ucLen;

   while( pEnd[ -1 ] == pTag->TrailChar )
   {
      if( --pEnd == pSrc )
      {
         *ptr = ucSize;
         return uiOffset + ucSize;
      }
   }

   pDst = ptr + 2;
   iMax = NSX_PAGELEN - uiOffset - ucSize + 1;
   if( iMax > ( int ) ucLen )
      iMax = ucLen;
   if( iMax > 0 )
   {
      while( pSrc < pEnd )
      {
         ZH_UCHAR uc = *pSrc++;
         if( uc == NSX_RLE_CHAR )
         {
            if( pSrc < pEnd && *pSrc == NSX_RLE_CHAR )
            {
               ZH_UCHAR ucRepl = 2;
               if( ( iMax -= 3 ) <= 0 )
                  break;
               ++pSrc;
               while( pSrc < pEnd && *pSrc == NSX_RLE_CHAR )
               {
                  ++pSrc;
                  ++ucRepl;
               }
               *pDst++ = NSX_RLE_CHAR;
               *pDst++ = ucRepl;
               *pDst++ = NSX_RLE_CHAR;
            }
            else
            {
               if( ( iMax -= 2 ) <= 0 )
                  break;
               *pDst++ = NSX_RLE_CHAR;
               *pDst++ = 1;
            }
         }
         else if( pEnd - pSrc > 2 &&
                  *pSrc == uc && pSrc[ 1 ] == uc && pSrc[ 2 ] == uc )
         {
            ZH_UCHAR ucRepl = 4;
            if( ( iMax -= 3 ) <= 0 )
               break;
            pSrc += 3;
            while( pSrc < pEnd && *pSrc == uc )
            {
               ++pSrc;
               ++ucRepl;
            }
            *pDst++ = NSX_RLE_CHAR;
            *pDst++ = ucRepl;
            *pDst++ = uc;
         }
         else if( --iMax == 0 )
            break;
         else
            *pDst++ = uc;
      }
      if( iMax > 0 )
      {
         ucSize += ( ZH_UCHAR ) ( pDst - ( ptr + 2 ) );
         *ptr = ucSize;
         return uiOffset + ucSize;
      }
   }

   uiOffset += ucSize + ucLen;
   if( uiOffset > NSX_PAGELEN )
      return 0;
   memcpy( ptr + 2, &pKeyValue[ ucDupCount ], ucLen );
   *ptr = ucSize + ucLen;

   return uiOffset;
}

/*
 * generate Run-Time error
 */
static ZH_ERRCODE zh_nsxErrorRT( NSXAREAP pArea,
                                 ZH_ERRCODE errGenCode, ZH_ERRCODE errSubCode,
                                 const char * szFileName, ZH_ERRCODE errOsCode,
                                 ZH_USHORT uiFlags, PZH_ITEM * pErrorPtr )
{
   ZH_ERRCODE iRet = ZH_FAILURE;

   if( zh_vmRequestQuery() == 0 )
   {
      PZH_ITEM pError;
      if( pErrorPtr )
      {
         if( ! *pErrorPtr )
            *pErrorPtr = zh_errNew();
         pError = *pErrorPtr;
      }
      else
         pError = zh_errNew();
      zh_errPutGenCode( pError, errGenCode );
      zh_errPutSubCode( pError, errSubCode );
      zh_errPutOsCode( pError, errOsCode );
      zh_errPutDescription( pError, zh_langDGetErrorDesc( errGenCode ) );
      if( szFileName )
         zh_errPutFileName( pError, szFileName );
      if( uiFlags )
         zh_errPutFlags( pError, uiFlags );
      iRet = SELF_ERROR( &pArea->dbfarea.area, pError );
      if( ! pErrorPtr )
         zh_errRelease( pError );
   }
   return iRet;
}

static void zh_nsxCorruptError( LPNSXINDEX pIndex )
{
   zh_nsxErrorRT( pIndex->pArea, EG_CORRUPTION, EDBF_CORRUPT,
                  pIndex->IndexName, 0, 0, NULL );
}

/*
 * create new index key
 */
static LPKEYINFO zh_nsxKeyNew( int keylen )
{
   LPKEYINFO pKey;

   pKey = ( LPKEYINFO ) zh_xgrab( sizeof( KEYINFO ) + keylen );
   pKey->val[ keylen ] = '\0';
   pKey->page = pKey->rec = 0;

   return pKey;
}

/*
 * copy index key, if dst is null create new dst key else destroy dst
 */
static LPKEYINFO zh_nsxKeyCopy( LPKEYINFO pKeyDest, LPKEYINFO pKey, int keylen )
{
   if( ! pKeyDest )
      pKeyDest = ( LPKEYINFO ) zh_xgrab( sizeof( KEYINFO ) + keylen );

   memcpy( pKeyDest, pKey, sizeof( KEYINFO ) + keylen );

   return pKeyDest;
}

/*
 * get NSX key type for given item
 */
static ZH_BYTE zh_nsxItemType( PZH_ITEM pItem )
{
   switch( zh_itemType( pItem ) )
   {
      case ZH_IT_STRING:
      case ZH_IT_MEMO:
         return 'C';

      case ZH_IT_INTEGER:
      case ZH_IT_LONG:
      case ZH_IT_DOUBLE:
         return 'N';

      case ZH_IT_DATE:
         return 'D';

      case ZH_IT_TIMESTAMP:
         return 'T';

      case ZH_IT_LOGICAL:
         return 'L';

      default:
         return 'U';
   }
}

/*
 * convert NSX (Clipper) type of key expression to internal one
 */
static ZH_UCHAR zh_nsxKeyType( ZH_USHORT uiType, ZH_BYTE * pbTrail )
{
   switch( uiType )
   {
      case NSX_TYPE_LNUM:
      case NSX_TYPE_DNUM:
         *pbTrail = '\0';
         return 'N';

      case NSX_TYPE_LDATE:
         *pbTrail = '\0';
         return 'D';

      case NSX_TYPE_TIMESTAMP:
         *pbTrail = '\0';
         return 'T';

      case NSX_TYPE_LOG:
         *pbTrail = '\0';
         return 'L';

      case NSX_TYPE_CHAR:
         *pbTrail = ' ';
         return 'C';

      default:
         *pbTrail = '\0';
         return 'U';
   }
}

/*
 * convert internal type of key expression to comparable type
 */
static ZH_UCHAR zh_nsxKeyTypeCmp( ZH_UCHAR ucType )
{
   return ucType == 'T' ? 'D' : ucType;
}

/*
 * convert internal type of key expression to NSX (Clipper) one
 */
static ZH_USHORT zh_nsxKeyTypeRaw( ZH_UCHAR ucType )
{
   switch( ucType )
   {
      case 'N':
         return NSX_TYPE_DNUM;

      case 'D':
         return NSX_TYPE_LDATE;

      case 'T':
         return NSX_TYPE_TIMESTAMP;

      case 'L':
         return NSX_TYPE_LOG;

      case 'C':
         return NSX_TYPE_CHAR;

      default:
         return '\0';
   }
}

/*
 * store Item in index key
 * TODO: uiType check and generate RT error if necessary
 *       probably not here or we will have to add parameter
 *       for scope key evaluation
 */
static LPKEYINFO zh_nsxKeyPutItem( LPKEYINFO pKey, PZH_ITEM pItem, ZH_ULONG ulRecNo,
                                   LPTAGINFO pTag, ZH_BOOL fTrans, ZH_USHORT *puiLen )
{
   double d;
   ZH_SIZE len;

   if( ! pKey )
      pKey = zh_nsxKeyNew( pTag->KeyLength );

   if( puiLen )
   {
      *puiLen = pTag->KeyLength;
      pKey->mode = NSX_CMP_PREFIX;
   }
   else
      pKey->mode = NSX_CMP_EXACT;

   switch( zh_nsxItemType( pItem ) )
   {
      case 'C':
         if( fTrans )
         {
            len = pTag->KeyLength;
            zh_cdpnDup2( zh_itemGetCPtr( pItem ), zh_itemGetCLen( pItem ),
                         ( char * ) pKey->val, &len,
                         zh_vmCDP(), pTag->pIndex->pArea->dbfarea.area.cdPage );
         }
         else
         {
            len = zh_itemGetCLen( pItem );
            if( len > ( ZH_SIZE ) pTag->KeyLength )
               len = pTag->KeyLength;
            memcpy( pKey->val, zh_itemGetCPtr( pItem ), len );
         }
         if( len < ( ZH_SIZE ) pTag->KeyLength )
         {
            memset( pKey->val + len, pTag->TrailChar, pTag->KeyLength - len );
            if( puiLen )
               *puiLen = ( ZH_USHORT ) len;
         }
         break;
      case 'N':
         d = zh_itemGetND( pItem );
         ZH_DBL2ORD( &d, pKey->val );
         break;
      case 'D':
         d = ( double ) zh_itemGetDL( pItem );
         ZH_DBL2ORD( &d, pKey->val );
         if( puiLen && pTag->KeyType == 'T' )
            pKey->mode = NSX_CMP_DATE;
         break;
      case 'T':
         if( pTag->KeyType == 'D' )
            d = ( double ) zh_itemGetDL( pItem );
         else
            d = zh_itemGetTD( pItem );
         ZH_DBL2ORD( &d, pKey->val );
         break;
      case 'L':
         pKey->val[ 0 ] = zh_itemGetL( pItem ) ? 'T' : 'F';
         break;
      default:
         memset( pKey->val, '\0', pTag->KeyLength );
   }
   pKey->rec = ulRecNo;
   pKey->page = 0;

   return pKey;
}

/*
 * get Item from index key
 */
static PZH_ITEM zh_nsxKeyGetItem( PZH_ITEM pItem, LPKEYINFO pKey,
                                  LPTAGINFO pTag, ZH_BOOL fTrans )
{
   double d;

   if( pKey )
   {
      switch( pTag->KeyType )
      {
         case 'C':
            if( fTrans )
            {
               ZH_SIZE nLen = pTag->KeyLength;
               char * pszVal = zh_cdpnDup( ( const char * ) pKey->val, &nLen,
                                           pTag->pIndex->pArea->dbfarea.area.cdPage, zh_vmCDP() );
               pItem = zh_itemPutCLPtr( pItem, pszVal, nLen );
            }
            else
            {
               pItem = zh_itemPutCL( pItem, ( char * ) pKey->val, pTag->KeyLength );
            }
            break;
         case 'N':
            ZH_ORD2DBL( pKey->val, &d );
            pItem = zh_itemPutND( pItem, d );
            break;
         case 'D':
            ZH_ORD2DBL( pKey->val, &d );
            pItem = zh_itemPutDL( pItem, ( long ) d );
            break;
         case 'T':
            ZH_ORD2DBL( pKey->val, &d );
            pItem = zh_itemPutTD( pItem, d );
            break;
         case 'L':
            pItem = zh_itemPutL( pItem, pKey->val[ 0 ] == 'T' );
            break;
         default:
            if( pItem )
               zh_itemClear( pItem );
            else
               pItem = zh_itemNew( NULL );
      }
   }
   else if( pItem )
      zh_itemClear( pItem );
   else
      pItem = zh_itemNew( NULL );

   return pItem;
}

/*
 * evaluate key expression and create new Key from the result
 */
static LPKEYINFO zh_nsxEvalKey( LPKEYINFO pKey, LPTAGINFO pTag )
{
   NSXAREAP pArea = pTag->pIndex->pArea;
   PZH_ITEM pItem;
   PZH_CODEPAGE cdpTmp = zh_cdpSelect( pArea->dbfarea.area.cdPage );

   if( pTag->nField )
   {
      pItem = zh_itemNew( NULL );
      SELF_GETVALUE( &pArea->dbfarea.area, pTag->nField, pItem );
      pKey = zh_nsxKeyPutItem( pKey, pItem, pArea->dbfarea.ulRecNo, pTag, ZH_FALSE, NULL );
      zh_itemRelease( pItem );
   }
   else
   {
      int iCurrArea = zh_rddGetCurrentWorkAreaNumber();

      if( iCurrArea != pArea->dbfarea.area.uiArea )
         zh_rddSelectWorkAreaNumber( pArea->dbfarea.area.uiArea );
      else
         iCurrArea = 0;

      pItem = zh_vmEvalBlockOrMacro( pTag->pKeyItem );
      pKey = zh_nsxKeyPutItem( pKey, pItem, pArea->dbfarea.ulRecNo, pTag, ZH_FALSE, NULL );

      if( iCurrArea )
         zh_rddSelectWorkAreaNumber( iCurrArea );
   }

   zh_cdpSelect( cdpTmp );

   return pKey;
}

/*
 * evaluate conditional expression and return the logical result
 */
static ZH_BOOL zh_nsxEvalCond( NSXAREAP pArea, PZH_ITEM pCondItem, ZH_BOOL fSetWA )
{
   int iCurrArea = 0;
   ZH_BOOL fRet;

   if( fSetWA )
   {
      iCurrArea = zh_rddGetCurrentWorkAreaNumber();
      if( iCurrArea != pArea->dbfarea.area.uiArea )
         zh_rddSelectWorkAreaNumber( pArea->dbfarea.area.uiArea );
      else
         iCurrArea = 0;
   }

   fRet = zh_itemGetL( zh_vmEvalBlockOrMacro( pCondItem ) );

   if( iCurrArea )
      zh_rddSelectWorkAreaNumber( iCurrArea );

   return fRet;
}

/*
 * evaluate seek/skip block: {| key, rec | ... }
 */
static ZH_BOOL zh_nsxEvalSeekCond( LPTAGINFO pTag, PZH_ITEM pCondItem )
{
   ZH_BOOL fRet;
   PZH_ITEM pKeyVal, pKeyRec;

   pKeyVal = zh_nsxKeyGetItem( NULL, pTag->CurKeyInfo, pTag, ZH_TRUE );
   pKeyRec = zh_itemPutNInt( NULL, pTag->CurKeyInfo->rec );

   fRet = zh_itemGetL( zh_vmEvalBlockV( pCondItem, 2, pKeyVal, pKeyRec ) );

   zh_itemRelease( pKeyVal );
   zh_itemRelease( pKeyRec );

   return fRet;
}

/*
 * compare two values using Tag conditions (len & type)
 */
static int zh_nsxValCompare( LPTAGINFO pTag, const ZH_UCHAR * val1, int len1,
                             const ZH_UCHAR * val2, int len2, int iMode )
{
   int iLimit, iResult = 0;

   iLimit = ( len1 > len2 ) ? len2 : len1;

   if( pTag->KeyType == 'C' )
   {
      if( iLimit > 0 )
      {
         if( ZH_CODEPAGE_ISBINSORT( pTag->pIndex->pArea->dbfarea.area.cdPage ) )
            iResult = memcmp( val1, val2, iLimit );
         else
            return -zh_cdpcmp( ( const char * ) val2, ( ZH_SIZE ) len2,
                               ( const char * ) val1, ( ZH_SIZE ) len1,
                               pTag->pIndex->pArea->dbfarea.area.cdPage, 0 );
      }

      if( iResult == 0 )
      {
         if( len1 > len2 )
            iResult = 1;
         else if( len1 < len2 && iMode == NSX_CMP_EXACT )
            iResult = -1;
      }
      else if( iResult > 0 )
         iResult = 1;
      else
         iResult = -1;
   }
   else if( iMode == NSX_CMP_DATE && iLimit == 8 )
   {
      double d1, d2;
      long l;

      ZH_ORD2DBL( val1, &d1 );
      ZH_ORD2DBL( val2, &d2 );
      l = ( long ) d1 - ( long ) d2;
      if( l < 0 )
         iResult = -1;
      else if( l > 0 )
         iResult = 1;
   }
   else
   {
      if( iLimit == 0 || ( iResult = memcmp( val1, val2, iLimit ) ) == 0 )
      {
         if( len1 > len2 )
            iResult = 1;
         else if( len1 < len2 )
            iResult = -1;
      }
      else if( iResult > 0 )
         iResult = 1;
      else
         iResult = -1;
   }
   return iResult;
}

/*
 * check if a given key is in top scope
 */
static ZH_BOOL zh_nsxInTopScope( LPTAGINFO pTag, ZH_UCHAR * key )
{
   PZH_NSXSCOPE pScope = pTag->fUsrDescend ? &pTag->bottom : &pTag->top;

   if( pScope->scopeKeyLen )
   {
      int i = zh_nsxValCompare( pTag, pScope->scopeKey->val, pScope->scopeKeyLen,
                                 key, pTag->KeyLength, pScope->scopeKey->mode );
      return pTag->fUsrDescend ? i >= 0 : i <= 0;
   }
   else
      return ZH_TRUE;
}

/*
 * check if a given key is in bottom scope
 */
static ZH_BOOL zh_nsxInBottomScope( LPTAGINFO pTag, ZH_UCHAR * key )
{
   PZH_NSXSCOPE pScope = pTag->fUsrDescend ? &pTag->top : &pTag->bottom;

   if( pScope->scopeKeyLen )
   {
      int i = zh_nsxValCompare( pTag, pScope->scopeKey->val, pScope->scopeKeyLen,
                                key, pTag->KeyLength, pScope->scopeKey->mode );
      return pTag->fUsrDescend ? i <= 0 : i >= 0;
   }
   else
      return ZH_TRUE;
}

/*
 * check if a given key is in current scope
 */
static ZH_BOOL zh_nsxKeyInScope( LPTAGINFO pTag, LPKEYINFO pKey )
{
   return zh_nsxInTopScope( pTag, pKey->val ) &&
          zh_nsxInBottomScope( pTag, pKey->val );
}

/*
 * clear top or bottom scope
 */
static void zh_nsxTagClearScope( LPTAGINFO pTag, ZH_USHORT nScope )
{
   NSXAREAP pArea = pTag->pIndex->pArea;
   PZH_NSXSCOPE pScope;

   /* resolve any pending scope relations first */
   if( pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped )
      SELF_FORCEREL( &pArea->dbfarea.area );

   if( pTag->fUsrDescend )
      nScope = ( nScope == 0 ) ? 1 : 0;

   pScope = ( nScope == 0 ) ? &pTag->top : &pTag->bottom;

   if( pScope->scopeKey )
   {
      zh_nsxKeyFree( pScope->scopeKey );
      pScope->scopeKey = NULL;
   }
   if( pScope->scopeItem )
   {
      zh_itemRelease( pScope->scopeItem );
      pScope->scopeItem = NULL;
   }
   pScope->scopeKeyLen = 0;

   pTag->keyCount = 0;
}

/*
 * set top or bottom scope
 */
static void zh_nsxTagSetScope( LPTAGINFO pTag, ZH_USHORT nScope, PZH_ITEM pItem )
{
   NSXAREAP pArea = pTag->pIndex->pArea;
   PZH_ITEM pScopeVal;

   /* resolve any pending scope relations first */
   if( pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped )
      SELF_FORCEREL( &pArea->dbfarea.area );

   pScopeVal = ( zh_itemType( pItem ) & ZH_IT_BLOCK ) ?
                           zh_vmEvalBlock( pItem ) : pItem;

   if( zh_nsxKeyTypeCmp( pTag->KeyType ) == zh_nsxKeyTypeCmp( zh_nsxItemType( pScopeVal ) ) )
   {
      PZH_NSXSCOPE pScope;
      ZH_BOOL fTop = ( nScope == 0 );

      if( pTag->fUsrDescend )
         fTop = ! fTop;

      pScope = fTop ? &pTag->top : &pTag->bottom;

      pScope->scopeKey = zh_nsxKeyPutItem( pScope->scopeKey, pScopeVal,
               fTop ? NSX_IGNORE_REC_NUM : NSX_MAX_REC_NUM,
               pTag, ZH_TRUE, &pScope->scopeKeyLen );

      if( pScope->scopeItem == NULL )
         pScope->scopeItem = zh_itemNew( NULL );
      zh_itemCopy( pScope->scopeItem, pItem );

      pTag->keyCount = 0;
   }
   else
   {
      zh_nsxTagClearScope( pTag, nScope );
   }
}

/*
 * get top or bottom scope item
 */
static void zh_nsxTagGetScope( LPTAGINFO pTag, ZH_USHORT nScope, PZH_ITEM pItem )
{
   NSXAREAP pArea = pTag->pIndex->pArea;
   PZH_NSXSCOPE pScope;

   /* resolve any pending scope relations first */
   if( pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped )
      SELF_FORCEREL( &pArea->dbfarea.area );

   if( pTag->fUsrDescend )
      nScope = ( nScope == 0 ) ? 1 : 0;

   pScope = ( nScope == 0 ) ? &pTag->top : &pTag->bottom;

   if( pScope->scopeItem )
      zh_itemCopy( pItem, pScope->scopeItem );
   else
      zh_itemClear( pItem );
}

/*
 * refresh top and bottom scope value if set as codeblock
 */
static void zh_nsxTagRefreshScope( LPTAGINFO pTag )
{
   PZH_ITEM pItem;

   /* resolve any pending scope relations first */
   if( pTag->pIndex->pArea->dbfarea.lpdbPendingRel &&
       pTag->pIndex->pArea->dbfarea.lpdbPendingRel->isScoped )
      SELF_FORCEREL( &pTag->pIndex->pArea->dbfarea.area );

   if( zh_itemType( pTag->top.scopeItem ) & ZH_IT_BLOCK )
   {
      pItem = zh_vmEvalBlock( pTag->top.scopeItem );
      pTag->top.scopeKey = zh_nsxKeyPutItem( pTag->top.scopeKey, pItem,
               pTag->top.scopeKey->rec, pTag, ZH_TRUE, &pTag->top.scopeKeyLen );
   }
   if( zh_itemType( pTag->bottom.scopeItem ) & ZH_IT_BLOCK )
   {
      pItem = zh_vmEvalBlock( pTag->bottom.scopeItem );
      pTag->bottom.scopeKey = zh_nsxKeyPutItem( pTag->bottom.scopeKey, pItem,
         pTag->bottom.scopeKey->rec, pTag, ZH_TRUE, &pTag->bottom.scopeKeyLen );
   }
}

/*
 * an interface for fast check record number in record filter
 */
static ZH_BOOL zh_nsxCheckRecordScope( NSXAREAP pArea, ZH_ULONG ulRec )
{
   ZH_LONG lRecNo = ( ZH_LONG ) ulRec;

   if( SELF_COUNTSCOPE( &pArea->dbfarea.area, NULL, &lRecNo ) == ZH_SUCCESS && lRecNo == 0 )
   {
      return ZH_FALSE;
   }
   return ZH_TRUE;
}

#ifdef ZH_NSX_DEBUG
static void zh_nsxTagCheckBuffers( LPTAGINFO pTag )
{
   LPPAGEINFO pPage;
   ZH_ULONG i;

   if( ( pTag->HdrChanged || pTag->pIndex->Changed ) && ! pTag->pIndex->lockWrite )
      zh_errInternal( 9999, "zh_nsxTagCheckBuffers: tag modified in unlocked index", NULL, NULL );

   for( i = 0; i < pTag->pIndex->ulPages; i++ )
   {
      pPage = pTag->pIndex->pages[ i ];
      if( pPage->Changed && ! pTag->pIndex->lockWrite )
         zh_errInternal( 9999, "zh_nsxTagCheckBuffers: page modified in unlocked index", NULL, NULL );
      if( pPage->iUsed )
         zh_errInternal( 9999, "zh_nsxTagCheckBuffers: page still allocated", NULL, NULL );
   }
}

static void zh_nsxPageCheckKeys( LPPAGEINFO pPage, LPTAGINFO pTag, int iPos, int iType )
{
   ZH_UCHAR pKeyVal[ NSX_MAXKEYLEN ], pKeyPrev[ NSX_MAXKEYLEN ];
   ZH_ULONG ulRecNo = 0, ulPrevRec;
   ZH_USHORT uiOffset = NSX_LEAFKEYOFFSET, u;
   int i;

   if( zh_nsxIsLeaf( pPage ) && pPage->uiKeys )
   {
      /* We do not need real previous key value and we can use any */
      memset( pKeyVal, pTag->TrailChar, pTag->KeyLength );
      uiOffset = zh_nsxLeafGetKey( pTag, pPage, uiOffset, pKeyVal, &ulRecNo );
      if( uiOffset == 0 )
      {
         zh_nsxCorruptError( pTag->pIndex );
         return;
      }
   }

   for( u = 1; u < pPage->uiKeys; u++ )
   {
      if( zh_nsxIsLeaf( pPage ) )
      {
         memcpy( pKeyPrev, pKeyVal, pTag->KeyLength );
         ulPrevRec = ulRecNo;
         uiOffset = zh_nsxLeafGetKey( pTag, pPage, uiOffset, pKeyVal, &ulRecNo );
         if( uiOffset == 0 )
         {
            zh_nsxCorruptError( pTag->pIndex );
            return;
         }
         i = zh_nsxValCompare( pTag, pKeyPrev, pTag->KeyLength,
                               pKeyVal, pTag->KeyLength, NSX_CMP_EXACT );
         if( i == 0 )
            i = ulRecNo > ulPrevRec ? -1 : 1;

         if( i > 0 )
         {
            printf( "\r\nuiKeys=%d(%d/%d), (%d)[%.*s]<%ld>>(%d)[%.*s]<%ld>", pPage->uiKeys, iPos, iType,
                    u - 1, pTag->KeyLength, pKeyPrev, ulPrevRec, u, pTag->KeyLength, pKeyVal, ulRecNo );
            fflush( stdout );
            zh_errInternal( 9999, "zh_nsxPageCheckKeys: keys sorted wrong.", NULL, NULL );
         }
      }
      else
      {
         i = zh_nsxValCompare( pTag,
                               zh_nsxGetKeyVal( pPage, pTag->KeyLength, u - 1 ),
                               pTag->KeyLength,
                               zh_nsxGetKeyVal( pPage, pTag->KeyLength, u ),
                               pTag->KeyLength, NSX_CMP_EXACT );
         if( i == 0 )
            i = zh_nsxGetKeyRec( pPage, pTag->KeyLength, u ) >
                zh_nsxGetKeyRec( pPage, pTag->KeyLength, u - 1 ) ? -1 : 1;
         if( i > 0 )
         {
            printf( "\r\nuiKeys=%d(%d/%d), (%d)[%.*s]>(%d)[%.*s]", pPage->uiKeys, iPos, iType,
                    u - 1, pTag->KeyLength, zh_nsxGetKeyVal( pPage, pTag->KeyLength, u - 1 ),
                    u, pTag->KeyLength, zh_nsxGetKeyVal( pPage, pTag->KeyLength, u ) );
            fflush( stdout );
            zh_errInternal( 9999, "zh_nsxPageCheckKeys: keys sorted wrong.", NULL, NULL );
         }
      }
   }
   if( zh_nsxIsLeaf( pPage ) && pPage->uiOffset != uiOffset )
   {
      printf( "\r\npPage->uiOffset=%d, uiOffset=%d\r\n", pPage->uiOffset, uiOffset );
      fflush( stdout );
      zh_errInternal( 9999, "zh_nsxPageCheckKeys: wrong free offset in leaf page.", NULL, NULL );
   }
}
#endif

/*
 * read a given block from index file
 */
static ZH_BOOL zh_nsxBlockRead( LPNSXINDEX pIndex, ZH_ULONG ulBlock, void * buffer, int iSize )
{
   if( ! pIndex->lockRead && ! pIndex->lockWrite )
      zh_errInternal( 9103, "zh_nsxBlockRead on not locked index file.", NULL, NULL );

   if( zh_fileReadAt( pIndex->pFile, buffer, iSize,
                      zh_nsxFileOffset( pIndex, ulBlock ) ) != ( ZH_SIZE ) iSize )
   {
      zh_nsxErrorRT( pIndex->pArea, EG_READ, EDBF_READ,
                     pIndex->IndexName, zh_fsError(), 0, NULL );
      return ZH_FALSE;
   }
   return ZH_TRUE;
}

/*
 * write a given block into index file
 */
static ZH_BOOL zh_nsxBlockWrite( LPNSXINDEX pIndex, ZH_ULONG ulBlock, const void * buffer, int iSize )
{
   if( ! pIndex->lockWrite )
      zh_errInternal( 9102, "zh_nsxBlockWrite on not locked index file.", NULL, NULL );

   if( zh_fileWriteAt( pIndex->pFile, buffer, iSize,
                       zh_nsxFileOffset( pIndex, ulBlock ) ) != ( ZH_SIZE ) iSize )
   {
      zh_nsxErrorRT( pIndex->pArea, EG_WRITE, EDBF_WRITE,
                     pIndex->IndexName, zh_fsError(), 0, NULL );
      return ZH_FALSE;
   }
   return ZH_TRUE;
}

/*
 * write a given tag page to file
 */
static ZH_BOOL zh_nsxPageSave( LPNSXINDEX pIndex, LPPAGEINFO pPage )
{
   zh_nsxSetKeyCount( pPage, pPage->uiKeys );
   if( zh_nsxIsLeaf( pPage ) )
   {
      zh_nsxLeafSetFreeOffset( pPage, pPage->uiOffset );
   }
   if( ! zh_nsxBlockWrite( pIndex, pPage->Page,
                           zh_nsxPageBuffer( pPage ), NSX_PAGELEN ) )
      return ZH_FALSE;
   pPage->Changed = ZH_FALSE;
   pIndex->fFlush = ZH_TRUE;
   /* In shared mode we have to update counter in version field of
      NSXHEADER to signal for other stations that their index buffers
      have to be discarded */
   if( pIndex->fShared )
      pIndex->Changed = ZH_TRUE;
   return ZH_TRUE;
}

/*
 * discard all index buffers due to concurrent access
 */
static void zh_nsxDiscardBuffers( LPNSXINDEX pIndex )
{
   int i;

   pIndex->ulPages = pIndex->ulPageLast = 0;
   pIndex->pChanged = pIndex->pFirst = pIndex->pLast = NULL;
   for( i = 0; i < pIndex->iTags; i++ )
   {
      pIndex->lpTags[ i ]->RootBlock = 0;
      pIndex->lpTags[ i ]->stackLevel = 0;
      pIndex->lpTags[ i ]->CurKeyOffset = 0;
   }
   zh_fileFlush( pIndex->pFile, ZH_FALSE );
}

/*
 * update tag flags
 */
static void zh_nsxTagUpdateFlags( LPTAGINFO pTag )
{
   pTag->Custom    = ( pTag->TagFlags & NSX_TAG_NOUPDATE ) != 0;
   pTag->ChgOnly   = ( pTag->TagFlags & NSX_TAG_CHGONLY  ) != 0;
   pTag->Partial   = ( pTag->TagFlags & NSX_TAG_PARTIAL  ) != 0;
   pTag->Template  = ( pTag->TagFlags & NSX_TAG_TEMPLATE ) != 0;
   pTag->MultiKey  = ( pTag->TagFlags & NSX_TAG_MULTIKEY ) != 0;
}

/*
 * check tag header in compound index
 */
static ZH_BOOL zh_nsxTagHeaderCheck( LPTAGINFO pTag )
{
   if( ! pTag->RootBlock )
   {
      if( pTag->HeadBlock )
      {
         NSXTAGHEADERUPDT header;

         if( zh_nsxBlockRead( pTag->pIndex, pTag->HeadBlock, &header, sizeof( header ) ) )
         {
            if( header.Signature[ 0 ] == NSX_SIGNATURE )
            {
               pTag->TagFlags = header.TagFlags[ 0 ];
               pTag->RootBlock = ZH_GET_LE_UINT32( header.RootPage );
               zh_nsxTagUpdateFlags( pTag );
            }
         }
      }
   }
   return pTag->RootBlock != 0;
}

static ZH_ULONG zh_nsxTagRootBlock( LPTAGINFO pTag )
{
   if( ! pTag->RootBlock )
   {
      if( ! zh_nsxTagHeaderCheck( pTag ) )
      {
         zh_nsxCorruptError( pTag->pIndex );
         return 0;
      }
   }

   return pTag->RootBlock;
}

/*
 * free buffers for pages in the tag
 */
static void zh_nsxFreePageBuffer( LPNSXINDEX pIndex )
{
   ZH_ULONG ul, ulMax = pIndex->ulPagesDepth;

   if( ulMax )
   {
      LPPAGEINFO * pPagePtr = pIndex->pages;

      for( ul = 0; ul < ulMax; ul++, pPagePtr++ )
      {
         if( *pPagePtr )
         {
#ifdef ZH_NSX_EXTERNAL_PAGEBUFFER
            if( zh_nsxPageBuffer( *pPagePtr ) )
               zh_xfree( zh_nsxPageBuffer( *pPagePtr ) );
#endif
            zh_xfree( *pPagePtr );
         }
      }
      zh_xfree( pIndex->pages );
      pIndex->pages = NULL;
      pIndex->ulPages = pIndex->ulPageLast = pIndex->ulPagesDepth = 0;
      pIndex->pFirst = pIndex->pLast = pIndex->pChanged = NULL;
   }
}

/*
 * trunc index file, left only space for header
 */
static void zh_nsxIndexTrunc( LPNSXINDEX pIndex )
{
   if( ! pIndex->lockWrite )
      zh_errInternal( 9102, "zh_nsxIndexTrunc on not locked index file.", NULL, NULL );

   zh_nsxFreePageBuffer( pIndex );
   pIndex->Update = pIndex->Changed = pIndex->fFlush = ZH_TRUE;
   pIndex->Version = 0;
   pIndex->FileSize = 0;
   pIndex->NextAvail = NSX_DUMMYNODE;
   zh_fileTruncAt( pIndex->pFile, NSX_PAGELEN );
}

/*
 * try to find given tag page in the buffer
 */
static LPPAGEINFO zh_nsxPageFind( LPTAGINFO pTag, ZH_ULONG ulPage )
{
   LPPAGEINFO * pPagePtr = pTag->pIndex->pages;
   ZH_ULONG u;

   for( u = pTag->pIndex->ulPages; u; u--, pPagePtr++ )
   {
      if( *pPagePtr && ( *pPagePtr )->Page == ulPage )
         return *pPagePtr;
   }
   return NULL;
}

/*
 * try to find free space in buffer
 */
static LPPAGEINFO zh_nsxPageGetBuffer( LPTAGINFO pTag, ZH_ULONG ulPage )
{
   LPNSXINDEX pIndex = pTag->pIndex;
   LPPAGEINFO * pPagePtr;

   if( pIndex->ulPages < pIndex->ulPagesDepth )
   {
      pPagePtr = &pIndex->pages[ pIndex->ulPages++ ];
   }
   else if( pIndex->pFirst )
   {
      LPPAGEINFO pPage = pIndex->pFirst;

      if( pPage->iUsed )
         zh_errInternal( 9999, "zh_nsxPageGetBuffer: page used.", NULL, NULL );
      if( pPage->Changed )
         zh_errInternal( 9999, "zh_nsxPageGetBuffer: page changed.", NULL, NULL );

      pIndex->pFirst = pPage->pNext;
      if( pIndex->pFirst )
         pIndex->pFirst->pPrev = NULL;
      else
         pIndex->pLast = NULL;
      pPage->pPrev = NULL;
      pPage->Page = ulPage;
      pPage->iUsed = 1;

      return pPage;
   }
   else if( pIndex->ulPagesDepth == 0 )
   {
      pIndex->ulPages = 1;
      pIndex->ulPageLast = 0;
      pIndex->ulPagesDepth = NSX_PAGE_BUFFER;
      pIndex->pages = ( LPPAGEINFO * ) zh_xgrabz( sizeof( LPPAGEINFO ) * NSX_PAGE_BUFFER );
      pPagePtr = &pIndex->pages[ 0 ];
   }
   else
   {
      ZH_ULONG ul = pIndex->ulPageLast;
      for( ;; )
      {
         if( ++ul >= pIndex->ulPagesDepth )
            ul = 0;
         pPagePtr = &pIndex->pages[ ul ];
         if( ! ( *pPagePtr )->iUsed && ! ( *pPagePtr )->Changed )
         {
            pIndex->ulPageLast = ul;
            break;
         }
         if( ul == pIndex->ulPageLast )
         {
            ul = pIndex->ulPagesDepth;
            pIndex->ulPagesDepth += NSX_PAGE_BUFFER >> 1;
            pIndex->pages = ( LPPAGEINFO * ) zh_xrealloc( pIndex->pages,
                                 sizeof( LPPAGEINFO ) * pIndex->ulPagesDepth );
            memset( pIndex->pages + ul, 0,
                         ( NSX_PAGE_BUFFER >> 1 ) * sizeof( LPPAGEINFO ) );
            pIndex->ulPages++;
            pPagePtr = &pIndex->pages[ ul ];
            pIndex->ulPageLast = 0;
            break;
         }
      }
   }

   if( ! *pPagePtr )
      *pPagePtr = ( LPPAGEINFO ) zh_xgrabz( sizeof( ZH_PAGEINFO ) );
#ifdef ZH_NSX_EXTERNAL_PAGEBUFFER
   if( ! zh_nsxPageBuffer( *pPagePtr ) )
      zh_nsxPageBuffer( *pPagePtr ) = ( ZH_UCHAR * ) zh_xgrabz( NSX_PAGELEN );
#endif
   ( *pPagePtr )->pPrev = NULL;
   ( *pPagePtr )->Page  = ulPage;
   ( *pPagePtr )->iUsed = 1;
   return *pPagePtr;
}

/*
 * mark used page as free
 */
static void zh_nsxPageRelease( LPTAGINFO pTag, LPPAGEINFO pPage )
{
   LPNSXINDEX pIndex = pTag->pIndex;

   if( --pPage->iUsed == 0 )
   {
      if( pPage->Changed )
      {
         if( ! pPage->pPrev )
         {
            pPage->pPrev = pPage;
            pPage->pNext = pIndex->pChanged;
            pIndex->pChanged = pPage;
         }
      }
      else if( pIndex->pLast )
      {
         pIndex->pLast->pNext = pPage;
         pPage->pPrev = pIndex->pLast;
         pPage->pNext = NULL;
         pIndex->pLast = pPage;
      }
      else
      {
         pPage->pNext = pPage->pPrev = NULL;
         pIndex->pFirst = pIndex->pLast = pPage;
      }
   }
   else if( pPage->iUsed < 0 )
      zh_errInternal( 9999, "zh_nsxPageRelease: unused page freed.", NULL, NULL );

#ifdef ZH_NSX_DEBUG_EXT
   if( zh_nsxPageType( pPage ) != 'f' )
      zh_nsxPageCheckKeys( pPage, pTag, 0, 11 );
#endif
}

/*
 * load page from index file or the buffer
 */
static LPPAGEINFO zh_nsxPageLoad( LPTAGINFO pTag, ZH_ULONG ulPage )
{
   LPPAGEINFO pPage;

   if( ! ulPage )
   {
      zh_nsxCorruptError( pTag->pIndex );
      return NULL;
   }

   pPage = zh_nsxPageFind( pTag, ulPage );
   if( pPage )
   {
      if( ! pPage->Changed && ! pPage->iUsed )
      {
         if( pPage->pNext )
            pPage->pNext->pPrev = pPage->pPrev;
         else
            pTag->pIndex->pLast = pPage->pPrev;
         if( pPage->pPrev )
         {
            pPage->pPrev->pNext = pPage->pNext;
            pPage->pPrev = NULL;
         }
         else
            pTag->pIndex->pFirst = pPage->pNext;
      }
      pPage->iUsed++;
   }
   else
   {
      pPage = zh_nsxPageGetBuffer( pTag, ulPage );
      pPage->Changed = ZH_FALSE;
      if( ! zh_nsxBlockRead( pTag->pIndex, ulPage,
                             zh_nsxPageBuffer( pPage ), NSX_PAGELEN ) )
      {
         zh_nsxPageRelease( pTag, pPage );
         return NULL;
      }
      if( zh_nsxPageType( pPage ) != 'f' )
      {
         pPage->uiKeys = zh_nsxGetKeyCount( pPage );
         pPage->uiOffset = zh_nsxIsLeaf( pPage ) ?
                           zh_nsxLeafGetFreeOffset( pPage ) : 0;
         if( zh_nsxPageType( pPage ) > ( NSX_ROOTPAGE | NSX_LEAFPAGE ) ||
             pPage->uiOffset > NSX_PAGELEN ||
             ( ! zh_nsxIsLeaf( pPage ) && pPage->uiKeys > pTag->MaxKeys ) )
         {
            zh_nsxPageRelease( pTag, pPage );
            zh_nsxCorruptError( pTag->pIndex );
            return NULL;
         }
#ifdef ZH_NSX_DEBUG_EXT
         zh_nsxPageCheckKeys( pPage, pTag, 0, 21 );
#endif
      }
   }
   return pPage;
}

/*
 * initialize empty page structure
 */
static void zh_nsxPageInit( LPPAGEINFO pPage )
{
   memset( pPage->data.buffer, 0, NSX_PAGELEN );
   pPage->uiKeys = pPage->uiOffset = 0;
}

/*
 * free the index page for future reuse
 */
static void zh_nsxPageFree( LPTAGINFO pTag, LPPAGEINFO pPage )
{
   zh_nsxSetPageType( pPage, 'f' );
#ifdef ZH_NSX_EXTERNAL_PAGEBUFFER
   ZH_PUT_LE_UINT32( pPage->data.rootHeader->FreePage, pTag->pIndex->NextAvail );
#else
   ZH_PUT_LE_UINT32( pPage->data.rootHeader.FreePage, pTag->pIndex->NextAvail );
#endif
   pTag->pIndex->NextAvail = pPage->Page;
   pTag->pIndex->Changed = pPage->Changed = ZH_TRUE;
}

/*
 * add given page to list of free pages
 */
static void zh_nsxPageAddFree( LPTAGINFO pTag, ZH_ULONG ulPage )
{
   LPPAGEINFO pPage = zh_nsxPageGetBuffer( pTag, ulPage );

   pPage->Changed = ZH_TRUE;
   zh_nsxPageInit( pPage );
   zh_nsxPageFree( pTag, pPage );
   zh_nsxPageSave( pTag->pIndex, pPage );
   zh_nsxPageRelease( pTag, pPage );
}

/*
 * allocate new page address
 */
static ZH_ULONG zh_nsxPageAlloc( LPNSXINDEX pIndex )
{
   ZH_ULONG ulPage;

   if( ! pIndex->FileSize )
   {
      ZH_FOFFSET fOffset;
      fOffset = zh_fileSize( pIndex->pFile );
      pIndex->FileSize = ( ZH_ULONG )
                     ( fOffset >> ( pIndex->LargeFile ? NSX_PAGELEN_BITS : 0 ) );
   }
   ulPage = pIndex->FileSize;
   pIndex->FileSize += pIndex->LargeFile ? 1 : NSX_PAGELEN;
   return ulPage;
}

/*
 * allocate new page in index file - reuse freed one or increase file
 */
static LPPAGEINFO zh_nsxPageNew( LPTAGINFO pTag, ZH_BOOL fNull )
{
   LPPAGEINFO pPage;

   if( pTag->pIndex->NextAvail && pTag->pIndex->NextAvail != NSX_DUMMYNODE )
   {
      pPage = zh_nsxPageLoad( pTag, pTag->pIndex->NextAvail );
      if( ! pPage )
         return NULL;
      else
      {
#ifdef ZH_NSX_EXTERNAL_PAGEBUFFER
         pTag->pIndex->NextAvail = ZH_GET_LE_UINT32( pPage->data.rootHeader->FreePage );
#else
         pTag->pIndex->NextAvail = ZH_GET_LE_UINT32( pPage->data.rootHeader.FreePage );
#endif
      }
   }
   else
   {
      pPage = zh_nsxPageGetBuffer( pTag, fNull ? 0 : zh_nsxPageAlloc( pTag->pIndex ) );
   }
   zh_nsxPageInit( pPage );
   pTag->pIndex->Changed = pPage->Changed = ZH_TRUE;

   return pPage;
}

/*
 * get free page in index file
 */
static ZH_ULONG zh_nsxPageGetFree( LPTAGINFO pTag )
{
   LPPAGEINFO pPage = zh_nsxPageNew( pTag, ZH_FALSE );
   ZH_ULONG ulPage = 0;

   if( pPage )
   {
      ulPage = pPage->Page;
      pPage->Changed = ZH_FALSE;
      zh_nsxPageRelease( pTag, pPage );
   }
   return ulPage;
}

/*
 * SIX3 compatible template index expression detection
 */
static ZH_BOOL zh_nsxIsTemplateFunc( const char * szKeyExpr )
{
   return zh_strnicmp( szKeyExpr, "sxChar(", 7 ) == 0 ||
          zh_strnicmp( szKeyExpr, "sxDate(", 7 ) == 0 ||
          zh_strnicmp( szKeyExpr, "sxNum(", 6 ) == 0 ||
          zh_strnicmp( szKeyExpr, "sxLog(", 6 ) == 0;
}

/*
 * create the new tag structure
 */
static LPTAGINFO zh_nsxTagNew( LPNSXINDEX pIndex, const char * szTagName,
                               const char * szKeyExpr, PZH_ITEM pKeyExpr,
                               ZH_UCHAR ucKeyType, ZH_USHORT uiKeyLen, ZH_BYTE bTrail,
                               const char * szForExpr, PZH_ITEM pForExpr,
                               ZH_BOOL fAscendKey, ZH_BOOL fUnique, ZH_BOOL fCustom )
{
   LPTAGINFO pTag;

   pTag = ( LPTAGINFO ) zh_xgrabz( sizeof( TAGINFO ) );
   pTag->TagName = zh_strndup( szTagName, NSX_TAGNAME );
   pTag->pIndex = pIndex;
   if( szKeyExpr )
      pTag->KeyExpr = zh_strndup( szKeyExpr, NSX_MAXEXPLEN );

   if( pForExpr && szForExpr )
      pTag->ForExpr = zh_strndup( szForExpr, NSX_MAXEXPLEN );

   pTag->nField = zh_rddFieldExpIndex( &pIndex->pArea->dbfarea.area, pTag->KeyExpr );
   pTag->pKeyItem = pKeyExpr;
   pTag->pForItem = pForExpr;
   pTag->AscendKey = fAscendKey;
   pTag->fUsrDescend = ! pTag->AscendKey;
   pTag->UniqueKey = fUnique;
   pTag->Custom = fCustom;
   pTag->MultiKey = fCustom && DBFAREA_DATA( &pIndex->pArea->dbfarea )->fMultiKey;
   pTag->KeyType = ucKeyType;
   pTag->KeyLength = uiKeyLen;
   pTag->TrailChar = bTrail;

   pTag->MaxKeys = ( NSX_PAGELEN - 8 ) / ( uiKeyLen + 8 );
   pTag->CurKeyInfo = zh_nsxKeyNew( pTag->KeyLength );

   return pTag;
}

/*
 * free from memory tag structure
 */
static void zh_nsxTagFree( LPTAGINFO pTag )
{
   if( pTag == pTag->pIndex->pArea->lpCurTag )
      pTag->pIndex->pArea->lpCurTag = NULL;
   zh_xfree( pTag->TagName );
   if( pTag->KeyExpr )
      zh_xfree( pTag->KeyExpr );
   if( pTag->ForExpr )
      zh_xfree( pTag->ForExpr );
   if( pTag->pKeyItem )
      zh_vmDestroyBlockOrMacro( pTag->pKeyItem );
   if( pTag->pForItem )
      zh_vmDestroyBlockOrMacro( pTag->pForItem );
   if( pTag->HotKeyInfo )
      zh_nsxKeyFree( pTag->HotKeyInfo );
   zh_nsxKeyFree( pTag->CurKeyInfo );
   zh_nsxTagClearScope( pTag, 0 );
   zh_nsxTagClearScope( pTag, 1 );
   if( pTag->stack )
   {
      while( pTag->stackSize-- )
      {
         if( pTag->stack[ pTag->stackSize ].value )
            zh_xfree( pTag->stack[ pTag->stackSize ].value );
      }
      zh_xfree( pTag->stack );
   }
   zh_xfree( pTag );
}

/*
 * delete tag from compound index
 */
static void zh_nsxTagDelete( LPTAGINFO pTag )
{
   LPNSXINDEX pIndex = pTag->pIndex;
   int i;

   for( i = 0; i < pIndex->iTags; i++ )
   {
      if( pTag == pIndex->lpTags[ i ] )
      {
         while( ++i < pIndex->iTags )
            pIndex->lpTags[ i - 1 ] = pIndex->lpTags[ i ];
         if( --pIndex->iTags )
            pIndex->lpTags = ( LPTAGINFO * ) zh_xrealloc( pIndex->lpTags,
                                       sizeof( LPTAGINFO ) * pIndex->iTags );
         else
            zh_xfree( pIndex->lpTags );
         break;
      }
   }
   zh_nsxTagFree( pTag );
   pIndex->pArea->fSetTagNumbers = ZH_TRUE;
}

/*
 * add tag to compound index
 */
static ZH_ERRCODE zh_nsxTagAdd( LPNSXINDEX pIndex, LPTAGINFO pTag )
{
   if( pIndex->iTags >= NSX_MAXTAGS )
      return ZH_FAILURE;

   if( pIndex->iTags )
      pIndex->lpTags = ( LPTAGINFO * ) zh_xrealloc( pIndex->lpTags,
                                 sizeof( LPTAGINFO ) * ( pIndex->iTags + 1 ) );
   else
      pIndex->lpTags = ( LPTAGINFO * ) zh_xgrab( sizeof( LPTAGINFO ) );

   pIndex->lpTags[ pIndex->iTags++ ] = pTag;
   pIndex->pArea->fSetTagNumbers = ZH_TRUE;
   return ZH_SUCCESS;
}

/*
 * create new tag and load it from index file
 */
static LPTAGINFO zh_nsxTagLoad( LPNSXINDEX pIndex, ZH_ULONG ulBlock,
                                const char * szTagName, LPNSXTAGHEADER lpNSX )
{
   LPTAGINFO pTag;
   PZH_ITEM pKeyExp, pForExp = NULL;
   ZH_USHORT uiUnique, uiDescend, uiKeySize;
   ZH_UCHAR ucType, ucTrail;

   uiUnique = ZH_GET_LE_UINT16( lpNSX->Unique );
   uiDescend = ZH_GET_LE_UINT16( lpNSX->Descend );
   uiKeySize = ZH_GET_LE_UINT16( lpNSX->KeySize );
   ucType = zh_nsxKeyType( ZH_GET_LE_UINT16( lpNSX->KeyType ), &ucTrail );

   if( lpNSX->Signature[ 0 ] != NSX_SIGNATURE ||
       uiUnique > 1 || uiDescend > 1 || ucType == 'U' ||
       uiKeySize == 0 || uiKeySize > NSX_MAXKEYLEN || lpNSX->KeyExpr[ 0 ] < 0x20 )
      return NULL;

   if( SELF_COMPILE( &pIndex->pArea->dbfarea.area, ( const char * ) lpNSX->KeyExpr ) == ZH_FAILURE )
      return NULL;
   pKeyExp = pIndex->pArea->dbfarea.area.valResult;
   pIndex->pArea->dbfarea.area.valResult = NULL;

   if( lpNSX->ForExpr[ 0 ] >= 0x20 )
   {
      if( SELF_COMPILE( &pIndex->pArea->dbfarea.area, ( const char * ) lpNSX->ForExpr ) == ZH_FAILURE )
      {
         zh_vmDestroyBlockOrMacro( pKeyExp );
         return NULL;
      }
      pForExp = pIndex->pArea->dbfarea.area.valResult;
      pIndex->pArea->dbfarea.area.valResult = NULL;
   }
   pTag = zh_nsxTagNew( pIndex, szTagName,
                        ( const char * ) lpNSX->KeyExpr, pKeyExp,
                        ucType, uiKeySize, ucTrail,
                        ( const char * ) lpNSX->ForExpr, pForExp,
                        uiDescend == 0, uiUnique != 0,
                        ( lpNSX->TagFlags[ 0 ] & NSX_TAG_NOUPDATE ) != 0 );

   pTag->TagFlags = lpNSX->TagFlags[ 0 ];
   zh_nsxTagUpdateFlags( pTag );
   pTag->HeadBlock = ulBlock;
   pTag->RootBlock = ZH_GET_LE_UINT32( lpNSX->RootPage );

   return pTag;
}

/*
 * add tag into NSX header
 */
static void zh_nsxIndexTagAdd( LPNSXINDEX pIndex, LPTAGINFO pTag )
{
   int iTags = ZH_GET_LE_UINT16( pIndex->HeaderBuff.TagCount ), i;
   LPNSXTAGITEM pTagItem = pIndex->HeaderBuff.TagList;

   for( i = 0; i < iTags; pTagItem++, i++ )
   {
      if( ! zh_strnicmp( ( const char * ) pTagItem->TagName, pTag->TagName, NSX_TAGNAME ) )
         break;
   }
   if( i == iTags )
   {
      ++iTags;
      ZH_PUT_LE_UINT16( pIndex->HeaderBuff.TagCount, iTags );
      zh_strncpy( ( char * ) pTagItem->TagName, pTag->TagName, NSX_TAGNAME );
   }
   ZH_PUT_LE_UINT32( pTagItem->TagOffset, pTag->HeadBlock );
   pIndex->Update = ZH_TRUE;
}

/*
 * delete tag from NSX header
 */
static void zh_nsxIndexTagDel( LPNSXINDEX pIndex, const char * szTagName )
{
   int iTags = ZH_GET_LE_UINT16( pIndex->HeaderBuff.TagCount ), i;
   LPNSXTAGITEM pTagItem = pIndex->HeaderBuff.TagList;

   for( i = 0; i < iTags; pTagItem++, i++ )
   {
      if( ! zh_strnicmp( ( const char * ) pTagItem->TagName, szTagName, NSX_TAGNAME ) )
      {
         memmove( pTagItem, pTagItem + 1, ( iTags - i ) * sizeof( NSXTAGITEM ) );
         memset( pTagItem + iTags - 1, 0, sizeof( NSXTAGITEM ) );
         --iTags;
         ZH_PUT_LE_UINT16( pIndex->HeaderBuff.TagCount, iTags );
         pIndex->Update = ZH_TRUE;
         break;
      }
   }
}

/*
 * find tag header block in NSX header
 */
static ZH_ULONG zh_nsxIndexTagFind( LPNSXROOTHEADER lpNSX, const char * szTagName )
{
   int iTags = ZH_GET_LE_UINT16( lpNSX->TagCount ), i;
   LPNSXTAGITEM pTagItem = lpNSX->TagList;

   for( i = 0; i < iTags; pTagItem++, i++ )
   {
      if( ! zh_strnicmp( ( const char * ) pTagItem->TagName, szTagName, NSX_TAGNAME ) )
         return ZH_GET_LE_UINT32( pTagItem->TagOffset );
   }
   return NSX_DUMMYNODE;
}

/*
 * Write tag header
 */
static ZH_ERRCODE zh_nsxTagHeaderSave( LPTAGINFO pTag )
{
   LPNSXINDEX pIndex = pTag->pIndex;
   NSXTAGHEADER Header;
   int iSize = NSX_TAGHEAD_HEADSIZE;

   if( ! pTag->HeadBlock )
   {
      pTag->HeadBlock = zh_nsxPageGetFree( pTag );
      if( ! pTag->HeadBlock )
         return ZH_FAILURE;
      zh_nsxIndexTagAdd( pIndex, pTag );
   }

   Header.Signature[ 0 ] = NSX_SIGNATURE;
   Header.TagFlags[ 0 ]  = ( pTag->Partial  ? NSX_TAG_PARTIAL  : 0 ) |
                           ( pTag->Template ? NSX_TAG_TEMPLATE : 0 ) |
                           ( pTag->ChgOnly  ? NSX_TAG_CHGONLY  : 0 ) |
                           ( pTag->Custom   ? NSX_TAG_NOUPDATE : 0 ) |
                           ( pTag->MultiKey ? NSX_TAG_MULTIKEY : 0 );
   ZH_PUT_LE_UINT32( Header.RootPage, pTag->RootBlock );

   if( pIndex->Update )
   {
      ZH_USHORT type = zh_nsxKeyTypeRaw( pTag->KeyType );
      int iLen;

      memset( ( ZH_BYTE * ) &Header + NSX_TAGHEAD_HEADSIZE, 0,
              sizeof( Header ) - NSX_TAGHEAD_HEADSIZE );

      ZH_PUT_LE_UINT16( Header.KeyType, type );
      ZH_PUT_LE_UINT16( Header.KeySize,  pTag->KeyLength );
      Header.Unique[ 0 ]  = pTag->UniqueKey ? 1 : 0;
      Header.Descend[ 0 ] = pTag->AscendKey ? 0 : 1;

      iLen = ( int ) strlen( pTag->KeyExpr );
      if( iLen > NSX_MAXEXPLEN )
         iLen = NSX_MAXEXPLEN;
      memcpy( Header.KeyExpr, pTag->KeyExpr, iLen );
      if( pTag->ForExpr )
      {
         iLen = ( int ) strlen( pTag->ForExpr );
         if( iLen > NSX_MAXEXPLEN )
            iLen = NSX_MAXEXPLEN;
         memcpy( Header.ForExpr, pTag->ForExpr, iLen );
      }
      iSize = sizeof( Header );
   }

   if( ! zh_nsxBlockWrite( pIndex, pTag->HeadBlock, &Header, iSize ) )
      return ZH_FAILURE;

   pTag->HdrChanged = ZH_FALSE;
   pIndex->Changed = pIndex->fFlush = ZH_TRUE;

   return ZH_SUCCESS;
}

/*
 * create new index structure
 */
static LPNSXINDEX zh_nsxIndexNew( NSXAREAP pArea )
{
   LPNSXINDEX pIndex;

   pIndex = ( LPNSXINDEX ) zh_xgrabz( sizeof( NSXINDEX ) );

   pIndex->pFile = NULL;
   pIndex->pArea = pArea;
   return pIndex;
}

/*
 * close the index file and free from memory index and tag structures
 */
static void zh_nsxIndexFree( LPNSXINDEX pIndex )
{
   zh_nsxFreePageBuffer( pIndex );
   if( pIndex->iTags )
   {
      int i;
      for( i = 0; i < pIndex->iTags; i++ )
         zh_nsxTagFree( pIndex->lpTags[ i ] );
      zh_xfree( pIndex->lpTags );
   }
   if( pIndex->pFile )
   {
      zh_fileClose( pIndex->pFile );
      if( pIndex->fDelete )
      {
         zh_fileDelete( pIndex->RealName ? pIndex->RealName : pIndex->IndexName );
      }
   }
   if( pIndex->IndexName )
      zh_xfree( pIndex->IndexName );
   if( pIndex->RealName )
      zh_xfree( pIndex->RealName );
   pIndex->pArea->fSetTagNumbers = ZH_TRUE;
   zh_xfree( pIndex );
}

/*
 * Write tag header
 */
static ZH_ERRCODE zh_nsxIndexHeaderSave( LPNSXINDEX pIndex )
{
   int iSize = pIndex->Update ? NSX_PAGELEN : NSX_ROOTHEAD_HEADSIZE;

   pIndex->Version++;
   pIndex->Version &= 0xFFFF;
   pIndex->HeaderBuff.Signature[ 0 ]  = pIndex->LargeFile ?
                                        NSX_SIGNATURE_LARGE : NSX_SIGNATURE;
   pIndex->HeaderBuff.IndexFlags[ 0 ] = 0;
   ZH_PUT_LE_UINT16( pIndex->HeaderBuff.TagCount, pIndex->iTags );
   ZH_PUT_LE_UINT16( pIndex->HeaderBuff.Version,  pIndex->Version );
   ZH_PUT_LE_UINT32( pIndex->HeaderBuff.FreePage, pIndex->NextAvail );
   ZH_PUT_LE_UINT32( pIndex->HeaderBuff.FileSize, pIndex->FileSize );

   if( ! zh_nsxBlockWrite( pIndex, 0, &pIndex->HeaderBuff, iSize ) )
      return ZH_FAILURE;

   pIndex->Changed = pIndex->Update = ZH_FALSE;

   return ZH_SUCCESS;
}

/*
 * load new tags from index file
 */
static ZH_ERRCODE zh_nsxIndexLoad( LPNSXINDEX pIndex )
{
   ZH_BYTE signature;

   if( ! pIndex->fValidHeader )
   {
      if( ! zh_nsxBlockRead( pIndex, 0, &pIndex->HeaderBuff, NSX_PAGELEN ) )
         return ZH_FAILURE;
      pIndex->fValidHeader = ZH_TRUE;
   }

   signature = pIndex->HeaderBuff.Signature[ 0 ];
   if( ( signature != NSX_SIGNATURE && signature != NSX_SIGNATURE_LARGE ) ||
       pIndex->HeaderBuff.IndexFlags[ 0 ] != 0 )
   {
      zh_nsxCorruptError( pIndex );
      return ZH_FAILURE;
   }
   else
   {
      NSXTAGHEADER tagbuffer;
      int iTags = ZH_GET_LE_UINT16( pIndex->HeaderBuff.TagCount );
      LPNSXTAGITEM pTagItem = pIndex->HeaderBuff.TagList;

      if( iTags > NSX_MAXTAGS )
         return ZH_FAILURE;

      pIndex->LargeFile = signature == NSX_SIGNATURE_LARGE;
      pIndex->Version   = ZH_GET_LE_UINT16( pIndex->HeaderBuff.Version );
      pIndex->NextAvail = ZH_GET_LE_UINT32( pIndex->HeaderBuff.FreePage );
      pIndex->FileSize  = ZH_GET_LE_UINT32( pIndex->HeaderBuff.FileSize );

      for( pIndex->iTags = 0; pIndex->iTags < iTags; pTagItem++ )
      {
         ZH_ULONG ulBlock = ZH_GET_LE_UINT32( pTagItem->TagOffset );
         LPTAGINFO pTag;
         if( ulBlock == 0 || pTagItem->TagName[ 0 ] <= 0x20 )
            return ZH_FAILURE;
         if( ! zh_nsxBlockRead( pIndex, ulBlock,
                                &tagbuffer, sizeof( NSXTAGHEADER ) ) )
            return ZH_FAILURE;
         pTag = zh_nsxTagLoad( pIndex, ulBlock, ( const char * ) pTagItem->TagName, &tagbuffer );
         if( ! pTag )
            return ZH_FAILURE;
         zh_nsxTagAdd( pIndex, pTag );
      }
   }

   return ZH_SUCCESS;
}

/*
 * read index header and check for concurrent access
 */
static ZH_ERRCODE zh_nsxIndexHeaderRead( LPNSXINDEX pIndex )
{
   if( ! zh_nsxBlockRead( pIndex, 0, &pIndex->HeaderBuff, NSX_PAGELEN ) )
      return ZH_FAILURE;

   if( ( pIndex->FileSize ? pIndex->HeaderBuff.Signature[ 0 ] !=
               ( pIndex->LargeFile ? NSX_SIGNATURE_LARGE : NSX_SIGNATURE ) :
         ( pIndex->HeaderBuff.Signature[ 0 ] != NSX_SIGNATURE &&
           pIndex->HeaderBuff.Signature[ 0 ] != NSX_SIGNATURE_LARGE ) ) ||
       pIndex->HeaderBuff.IndexFlags[ 0 ] != 0 )
   {
      zh_nsxCorruptError( pIndex );
      return ZH_FAILURE;
   }
   else
   {
      ZH_ULONG ulVersion, ulNext, ulFileSize;

      ulVersion = ZH_GET_LE_UINT16( pIndex->HeaderBuff.Version );
      ulNext = ZH_GET_LE_UINT32( pIndex->HeaderBuff.FreePage );
      ulFileSize = ZH_GET_LE_UINT32( pIndex->HeaderBuff.FileSize );

      if( pIndex->Version != ulVersion || pIndex->NextAvail != ulNext ||
          pIndex->FileSize != ulFileSize )
      {
         int i;
         zh_nsxDiscardBuffers( pIndex );
         pIndex->Version = ulVersion;
         pIndex->NextAvail = ulNext;
         pIndex->FileSize = ulFileSize;
         for( i = 0; i < pIndex->iTags; i++ )
         {
            pIndex->lpTags[ i ]->HeadBlock =
               zh_nsxIndexTagFind( &pIndex->HeaderBuff, pIndex->lpTags[ i ]->TagName );
            if( ! pIndex->lpTags[ i ]->HeadBlock )
               pIndex->lpTags[ i ]->RootBlock = 0;
         }
      }
   }

   return ZH_SUCCESS;
}

/*
 * write modified pages to index file
 */
static void zh_nsxIndexFlush( LPNSXINDEX pIndex )
{
   int i;

   while( pIndex->pChanged )
   {
      LPPAGEINFO pPage = pIndex->pChanged;
      pIndex->pChanged = pPage->pNext;
      if( pPage->Changed )
      {
         zh_nsxPageSave( pIndex, pPage );
         ++pPage->iUsed;
         zh_nsxPageRelease( pIndex->lpTags[ 0 ], pPage );
      }
      else
         zh_errInternal( 9999, "zh_nsxIndexFlush: unchaged page in the list.", NULL, NULL );
   }

   for( i = 0; i < pIndex->iTags; i++ )
      if( pIndex->lpTags[ i ]->HdrChanged )
         zh_nsxTagHeaderSave( pIndex->lpTags[ i ] );
   if( pIndex->Changed )
      zh_nsxIndexHeaderSave( pIndex );
}

/*
 * lock index for reading (shared lock)
 */
static ZH_BOOL zh_nsxIndexLockRead( LPNSXINDEX pIndex )
{
   ZH_BOOL fOK;

   if( pIndex->lockRead > 0 || pIndex->lockWrite > 0 || ! pIndex->fShared ||
       ZH_DIRTYREAD( &pIndex->pArea->dbfarea ) )
   {
      fOK = ZH_TRUE;
      pIndex->lockRead++;
   }
   else
   {
      fOK = zh_dbfLockIdxFile( &pIndex->pArea->dbfarea, pIndex->pFile,
                               FL_LOCK | FLX_SHARED | FLX_WAIT, ZH_FALSE,
                               &pIndex->lockData );
      /* if fOK then check VERSION field in NSXHEADER and
       * if it has been changed then discard all page buffers
       */
      if( fOK )
      {
         pIndex->lockRead++;
         if( zh_nsxIndexHeaderRead( pIndex ) != ZH_SUCCESS )
         {
            pIndex->lockRead--;
            zh_dbfLockIdxFile( &pIndex->pArea->dbfarea, pIndex->pFile,
                               FL_UNLOCK, ZH_FALSE, &pIndex->lockData );
            return ZH_FALSE;
         }
      }
   }
   if( ! fOK )
      zh_nsxErrorRT( pIndex->pArea, EG_LOCK, EDBF_LOCK, pIndex->IndexName, zh_fsError(), 0, NULL );

   return fOK;
}

/*
 * lock index for writing (exclusive lock)
 */
static ZH_BOOL zh_nsxIndexLockWrite( LPNSXINDEX pIndex, ZH_BOOL fCheck )
{
   ZH_BOOL fOK;

   if( pIndex->fReadonly )
      zh_errInternal( 9101, "zh_nsxIndexLockWrite: readonly index.", NULL, NULL );

   if( pIndex->lockRead )
      zh_errInternal( 9105, "zh_nsxIndexLockWrite: writeLock after readLock.", NULL, NULL );

   if( pIndex->lockWrite > 0 || ! pIndex->fShared )
   {
      fOK = ZH_TRUE;
      pIndex->lockWrite++;
   }
   else
   {
      fOK = zh_dbfLockIdxFile( &pIndex->pArea->dbfarea, pIndex->pFile,
                               FL_LOCK | FLX_EXCLUSIVE | FLX_WAIT, ZH_FALSE,
                               &pIndex->lockData );
      /* if fOK then check VERSION field in NSXHEADER and
       * if it has been changed then discard all page buffers
       */
      if( fOK )
      {
         pIndex->lockWrite++;
         if( fCheck && zh_nsxIndexHeaderRead( pIndex ) != ZH_SUCCESS )
         {
            pIndex->lockWrite--;
            zh_dbfLockIdxFile( &pIndex->pArea->dbfarea, pIndex->pFile,
                               FL_UNLOCK, ZH_FALSE, &pIndex->lockData );
            return ZH_FALSE;
         }
      }
   }
   if( ! fOK )
      zh_nsxErrorRT( pIndex->pArea, EG_LOCK, EDBF_LOCK, pIndex->IndexName, zh_fsError(), 0, NULL );

   return fOK;
}

/*
 * remove index read lock (shared lock)
 */
static ZH_BOOL zh_nsxIndexUnLockRead( LPNSXINDEX pIndex )
{
   ZH_BOOL fOK;

#ifdef ZH_NSX_DEBUG
   int i;
   for( i = 0; i < pIndex->iTags; i++ )
      zh_nsxTagCheckBuffers( pIndex->lpTags[ i ] );
#endif

   pIndex->lockRead--;
   if( pIndex->lockRead < 0 )
      zh_errInternal( 9106, "zh_nsxIndexUnLockRead: bad count of locks.", NULL, NULL );

   if( pIndex->lockRead || pIndex->lockWrite || ! pIndex->fShared ||
       ZH_DIRTYREAD( &pIndex->pArea->dbfarea ) )
   {
      fOK = ZH_TRUE;
   }
   else
   {
      pIndex->fValidHeader = ZH_FALSE;
      fOK = zh_dbfLockIdxFile( &pIndex->pArea->dbfarea, pIndex->pFile,
                               FL_UNLOCK, ZH_FALSE, &pIndex->lockData );
   }
   if( ! fOK )
      zh_errInternal( 9108, "zh_nsxIndexUnLockRead: unlock error.", NULL, NULL );

   return fOK;
}

/*
 * remove index write lock (exclusive lock)
 */
static ZH_BOOL zh_nsxIndexUnLockWrite( LPNSXINDEX pIndex )
{
   ZH_BOOL fOK;

#ifdef ZH_NSX_DEBUG
   int i;
   for( i = 0; i < pIndex->iTags; i++ )
      zh_nsxTagCheckBuffers( pIndex->lpTags[ i ] );
#endif

   if( pIndex->lockWrite <= 0 )
      zh_errInternal( 9106, "zh_nsxIndexUnLockWrite: bad count of locks.", NULL, NULL );
   if( pIndex->lockRead )
      zh_errInternal( 9105, "zh_nsxIndexUnLockWrite: writeUnLock before readUnLock.", NULL, NULL );

   zh_nsxIndexFlush( pIndex );
   pIndex->lockWrite--;

   if( pIndex->lockWrite || ! pIndex->fShared )
   {
      fOK = ZH_TRUE;
   }
   else
   {
      zh_fileFlush( pIndex->pFile, ZH_TRUE );
      pIndex->fValidHeader = ZH_FALSE;
      fOK = zh_dbfLockIdxFile( &pIndex->pArea->dbfarea, pIndex->pFile,
                               FL_UNLOCK, ZH_FALSE, &pIndex->lockData );
   }
   if( ! fOK )
      zh_errInternal( 9108, "zh_nsxIndexUnLockWrite: unlock error.", NULL, NULL );

   return fOK;
}

/*
 * lock tag for reading (shared lock)
 */
static ZH_BOOL zh_nsxTagLockRead( LPTAGINFO pTag )
{
   ZH_BOOL fOK = ZH_FALSE;

   if( zh_nsxIndexLockRead( pTag->pIndex ) )
   {
      fOK = zh_nsxTagHeaderCheck( pTag );
      if( ! fOK )
      {
         zh_nsxIndexUnLockRead( pTag->pIndex );
         zh_nsxCorruptError( pTag->pIndex );
      }
   }
   return fOK;
}

/*
 * lock tag for writing (exclusive lock)
 */
static ZH_BOOL zh_nsxTagLockWrite( LPTAGINFO pTag )
{
   ZH_BOOL fOK = ZH_FALSE;

   if( zh_nsxIndexLockWrite( pTag->pIndex, ZH_TRUE ) )
   {
      fOK = zh_nsxTagHeaderCheck( pTag );
      if( ! fOK )
      {
         zh_nsxIndexUnLockWrite( pTag->pIndex );
         zh_nsxCorruptError( pTag->pIndex );
      }
   }
   return fOK;
}

/*
 * remove tag read lock (shared lock)
 */
static ZH_BOOL zh_nsxTagUnLockRead( LPTAGINFO pTag )
{
   return zh_nsxIndexUnLockRead( pTag->pIndex );
}

/*
 * remove tag write lock (exclusive lock)
 */
static ZH_BOOL zh_nsxTagUnLockWrite( LPTAGINFO pTag )
{
   return zh_nsxIndexUnLockWrite( pTag->pIndex );
}

/*
 * retrieve previous key value from upper branch page
 */
static ZH_BOOL zh_nsxTagGetPrevKey( LPTAGINFO pTag, ZH_UCHAR * pKeyVal, int iLevel )
{
   while( --iLevel >= 0 )
   {
      if( pTag->stack[ iLevel ].ikey != 0 )
      {
         memcpy( pKeyVal, pTag->stack[ iLevel ].value, pTag->KeyLength );
         return ZH_TRUE;
      }
   }
   #if 0
   memset( pKeyVal, pTag->TrailChar, pTag->KeyLength );
   #endif
   return ZH_FALSE;
}

/*
 * decode key from leaf page into given buffer
 */
static ZH_BOOL zh_nsxPageGetLeafKey( LPTAGINFO pTag, LPPAGEINFO pPage, ZH_USHORT uiKey,
                                     ZH_UCHAR * pKeyVal, ZH_ULONG * pulRecNo )
{
   ZH_USHORT uiOffset = NSX_LEAFKEYOFFSET;

   zh_nsxTagGetPrevKey( pTag, pKeyVal, pTag->stackLevel - 1 );
   do
   {
      uiOffset = zh_nsxLeafGetKey( pTag, pPage, uiOffset,
                                   pKeyVal, pulRecNo );
      if( uiOffset == 0 )
      {
         zh_nsxCorruptError( pTag->pIndex );
         *pulRecNo = 0;
         return ZH_FALSE;
      }
   }
   while( uiKey-- );

   return ZH_TRUE;
}

/*
 * retrieve key from page
 */
static ZH_BOOL zh_nsxTagGetCurKey( LPTAGINFO pTag, LPPAGEINFO pPage, ZH_USHORT uiKey )
{
   if( zh_nsxIsLeaf( pPage ) )
   {
      if( uiKey >= pPage->uiKeys )
      {
         pTag->CurKeyInfo->rec = pTag->CurKeyInfo->page = 0;
      }
      else
      {
         if( pTag->CurKeyInfo->rec == 0 ||
             pTag->CurKeyInfo->page != pPage->Page ||
             uiKey < pTag->CurKeyNo || pTag->CurKeyOffset == 0 )
         {
            pTag->CurKeyOffset = NSX_LEAFKEYOFFSET;
            pTag->CurKeyNo = ( ZH_USHORT ) -1;
            zh_nsxTagGetPrevKey( pTag, pTag->CurKeyInfo->val, pTag->stackLevel - 1 );
         }
         pTag->CurKeyInfo->page = pPage->Page;

         while( pTag->CurKeyNo != uiKey )
         {
            pTag->CurKeyOffset = zh_nsxLeafGetKey( pTag, pPage,
                                                   pTag->CurKeyOffset,
                                                   pTag->CurKeyInfo->val,
                                                   &pTag->CurKeyInfo->rec );
            if( pTag->CurKeyOffset == 0 )
            {
               zh_nsxCorruptError( pTag->pIndex );
               pTag->CurKeyInfo->rec = 0;
               return ZH_FALSE;
            }
            pTag->CurKeyNo++;
         }
      }
   }
   else if( uiKey && uiKey <= pPage->uiKeys )
   {
      --uiKey;
      memcpy( pTag->CurKeyInfo->val,
              zh_nsxGetKeyVal( pPage, pTag->KeyLength, uiKey ), pTag->KeyLength );
      pTag->CurKeyInfo->rec = zh_nsxGetKeyRec( pPage, pTag->KeyLength, uiKey );
      pTag->CurKeyInfo->page = pPage->Page;
   }
   else
      pTag->CurKeyInfo->rec = pTag->CurKeyInfo->page = 0;

   return ZH_TRUE;
}

/*
 * set next page and key in page path
 */
static void zh_nsxTagSetPageStack( LPTAGINFO pTag, LPPAGEINFO pPage, ZH_USHORT uiKey )
{

   if( pTag->stackLevel == pTag->stackSize )
   {
      if( pTag->stackSize == 0 )
      {
         pTag->stackSize = NSX_STACKSIZE;
         pTag->stack = ( LPTREESTACK ) zh_xgrabz( sizeof( TREE_STACK ) * NSX_STACKSIZE );
      }
      else
      {
         pTag->stack = ( LPTREESTACK ) zh_xrealloc( pTag->stack,
                  sizeof( TREE_STACK ) * ( pTag->stackSize + NSX_STACKSIZE ) );
         memset( pTag->stack + sizeof( TREE_STACK ) * pTag->stackSize, 0,
                 sizeof( TREE_STACK ) * NSX_STACKSIZE );
         pTag->stackSize += NSX_STACKSIZE;
      }
   }

   if( ! zh_nsxIsLeaf( pPage ) && uiKey )
   {
      if( ! pTag->stack[ pTag->stackLevel ].value )
         pTag->stack[ pTag->stackLevel ].value = ( ZH_UCHAR * ) zh_xgrab( pTag->KeyLength );
      memcpy( pTag->stack[ pTag->stackLevel ].value,
              zh_nsxGetKeyVal( pPage, pTag->KeyLength, uiKey - 1 ),
              pTag->KeyLength );
   }
   pTag->stack[ pTag->stackLevel ].page = pPage->Page;
   pTag->stack[ pTag->stackLevel++ ].ikey = uiKey;
}

/*
 * go down from the given index page to the first key
 */
static LPPAGEINFO zh_nsxPageTopMove( LPTAGINFO pTag, ZH_ULONG ulPage )
{
   LPPAGEINFO pPage;

   for( ;; )
   {
      pPage = zh_nsxPageLoad( pTag, ulPage );
      if( ! pPage )
         return NULL;
      zh_nsxTagSetPageStack( pTag, pPage, 0 );
      if( zh_nsxIsLeaf( pPage ) )
      {
         if( pPage->uiKeys == 0 && pTag->stackLevel > 1 )
         {
            zh_nsxPageRelease( pTag, pPage );
            zh_nsxCorruptError( pTag->pIndex );
            return NULL;
         }
         break;
      }
      ulPage = zh_nsxGetLowerPage( pPage );
      zh_nsxPageRelease( pTag, pPage );
   }

   return pPage;
}

/*
 * go down from the given index page to the last key
 */
static LPPAGEINFO zh_nsxPageBottomMove( LPTAGINFO pTag, ZH_ULONG ulPage )
{
   LPPAGEINFO pPage;

   for( ;; )
   {
      pPage = zh_nsxPageLoad( pTag, ulPage );
      if( ! pPage )
         return NULL;
      if( zh_nsxIsLeaf( pPage ) )
      {
         zh_nsxTagSetPageStack( pTag, pPage, ( pPage->uiKeys > 0 ? pPage->uiKeys - 1 : 0 ) );
         if( pPage->uiKeys == 0 && pTag->stackLevel > 1 && ! pTag->pIndex->pArea->pSort )
         {
            zh_nsxPageRelease( pTag, pPage );
            zh_nsxCorruptError( pTag->pIndex );
            return NULL;
         }
         break;
      }
      zh_nsxTagSetPageStack( pTag, pPage, pPage->uiKeys );
      ulPage = pPage->uiKeys ? zh_nsxGetKeyPage( pPage, pTag->KeyLength, pPage->uiKeys - 1 ) :
                               zh_nsxGetLowerPage( pPage );
      zh_nsxPageRelease( pTag, pPage );
   }

   return pPage;
}

/*
 * set page path to the first key in tag
 */
static ZH_BOOL zh_nsxTagTopKey( LPTAGINFO pTag )
{
   LPPAGEINFO pPage;
   ZH_BOOL fFound;

   pTag->stackLevel = 0;
   pPage = zh_nsxPageTopMove( pTag, zh_nsxTagRootBlock( pTag ) );
   if( ! pPage )
      return ZH_FALSE;
   fFound = zh_nsxTagGetCurKey( pTag, pPage, 0 ) && pPage->uiKeys != 0;
   zh_nsxPageRelease( pTag, pPage );
   return fFound;
}

/*
 * set page path to the last key in tag
 */
static ZH_BOOL zh_nsxTagBottomKey( LPTAGINFO pTag )
{
   LPPAGEINFO pPage;
   ZH_BOOL fFound;

   pTag->stackLevel = 0;
   pPage = zh_nsxPageBottomMove( pTag, zh_nsxTagRootBlock( pTag ) );
   if( ! pPage )
      return ZH_FALSE;
   fFound = zh_nsxTagGetCurKey( pTag, pPage,
                                pTag->stack[ pTag->stackLevel - 1 ].ikey ) &&
            pPage->uiKeys != 0;
   zh_nsxPageRelease( pTag, pPage );
   return fFound;
}

/*
 * update page path to the next key in tag
 */
static ZH_BOOL zh_nsxTagNextKey( LPTAGINFO pTag )
{
   int iLevel = pTag->stackLevel - 1;

   if( iLevel >= 0 )
   {
      LPPAGEINFO pPage;
      ZH_ULONG ulPage;
      ZH_BOOL fFound;

      pPage = zh_nsxPageLoad( pTag, pTag->stack[ iLevel ].page );
      if( ! pPage )
         return ZH_FALSE;
      if( ! zh_nsxIsLeaf( pPage ) )
      {
         ulPage = pTag->stack[ iLevel ].ikey == 0 ? 0 :
                  zh_nsxGetKeyPage( pPage, pTag->KeyLength, pTag->stack[ iLevel ].ikey - 1 );
         zh_nsxPageRelease( pTag, pPage );
         pPage = zh_nsxPageTopMove( pTag, ulPage );
         if( ! pPage )
            return ZH_FALSE;
      }
      else if( pTag->stack[ iLevel ].ikey + 1 < pPage->uiKeys )
         pTag->stack[ iLevel ].ikey++;
      else
      {
         for( ;; )
         {
            zh_nsxPageRelease( pTag, pPage );
            if( --iLevel < 0 )
               return ZH_FALSE;
            pPage = zh_nsxPageLoad( pTag, pTag->stack[ iLevel ].page );
            if( ! pPage )
               return ZH_FALSE;
            if( pPage->uiKeys && pTag->stack[ iLevel ].ikey < ( ZH_SHORT ) pPage->uiKeys )
            {
               if( ! pTag->stack[ iLevel ].value )
                  pTag->stack[ iLevel ].value = ( ZH_UCHAR * ) zh_xgrab( pTag->KeyLength );
               memcpy( pTag->stack[ iLevel ].value,
                       zh_nsxGetKeyVal( pPage, pTag->KeyLength,
                                        pTag->stack[ iLevel ].ikey ),
                       pTag->KeyLength );
               pTag->stack[ iLevel ].ikey++;
               break;
            }
         }
         pTag->stackLevel = ( ZH_USHORTCAST ) iLevel + 1;
      }
      fFound = zh_nsxTagGetCurKey( pTag, pPage,
                                   pTag->stack[ pTag->stackLevel - 1 ].ikey );
      zh_nsxPageRelease( pTag, pPage );
      return fFound;
   }
   return ZH_FALSE;
}

/*
 * update page path to the previous key in tag
 */
static ZH_BOOL zh_nsxTagPrevKey( LPTAGINFO pTag )
{
   int iLevel = pTag->stackLevel - 1;

   if( iLevel >= 0 )
   {
      LPPAGEINFO pPage;
      ZH_ULONG ulPage;
      ZH_BOOL fFound;

      pPage = zh_nsxPageLoad( pTag, pTag->stack[ iLevel ].page );
      if( ! pPage )
         return ZH_FALSE;

      if( ! zh_nsxIsLeaf( pPage ) )
      {
         ulPage = pTag->stack[ iLevel ].ikey == 0 ? 0 :
                  pTag->stack[ iLevel ].ikey == 1 ? zh_nsxGetLowerPage( pPage ) :
                  zh_nsxGetKeyPage( pPage, pTag->KeyLength, pTag->stack[ iLevel ].ikey - 2 );
         zh_nsxPageRelease( pTag, pPage );
         if( --pTag->stack[ iLevel ].ikey )
         {
            if( ! pTag->stack[ iLevel ].value )
               pTag->stack[ iLevel ].value = ( ZH_UCHAR * ) zh_xgrab( pTag->KeyLength );
            memcpy( pTag->stack[ iLevel ].value,
                    zh_nsxGetKeyVal( pPage, pTag->KeyLength,
                                     pTag->stack[ iLevel ].ikey - 1 ),
                    pTag->KeyLength );
         }
         pPage = zh_nsxPageBottomMove( pTag, ulPage );
         if( ! pPage )
            return ZH_FALSE;
      }
      else if( pTag->stack[ iLevel ].ikey )
         pTag->stack[ iLevel ].ikey--;
      else
      {
         for( ;; )
         {
            zh_nsxPageRelease( pTag, pPage );
            if( --iLevel < 0 )
               return ZH_FALSE;
            pPage = zh_nsxPageLoad( pTag, pTag->stack[ iLevel ].page );
            if( ! pPage )
               return ZH_FALSE;
            if( pTag->stack[ iLevel ].ikey )
               break;
         }
         pTag->stackLevel = ( ZH_USHORTCAST ) iLevel + 1;
      }

      fFound = zh_nsxTagGetCurKey( pTag, pPage,
                                   pTag->stack[ pTag->stackLevel - 1 ].ikey );
      zh_nsxPageRelease( pTag, pPage );
      return fFound;
   }
   return ZH_FALSE;
}

/*
 * find a key value in page
 */
static int zh_nsxPageKeyFind( LPTAGINFO pTag, LPPAGEINFO pPage,
                              ZH_UCHAR * key, ZH_SHORT keylen, int mode,
                              ZH_BOOL fLast, ZH_ULONG ulRecNo, ZH_BOOL * fStop )
{
   int iBegin, iEnd, iLast, k, i;
   ZH_ULONG ulRec;

   *fStop = ZH_FALSE;

   if( pPage->uiKeys == 0 )
      return 0;
   else if( zh_nsxIsLeaf( pPage ) )
   {
      ZH_USHORT uiOffset = NSX_LEAFKEYOFFSET, u;
      ZH_UCHAR pKeyVal[ NSX_MAXKEYLEN ];

      zh_nsxTagGetPrevKey( pTag, pKeyVal, pTag->stackLevel );
      for( u = 0; u < pPage->uiKeys; ++u )
      {
         uiOffset = zh_nsxLeafGetKey( pTag, pPage, uiOffset,
                                      pKeyVal, &ulRec );
         if( uiOffset == 0 )
         {
            zh_nsxCorruptError( pTag->pIndex );
            break;
         }
         k = zh_nsxValCompare( pTag, key, keylen,
                               pKeyVal, pTag->KeyLength, mode );
         if( k == 0 )
         {
            if( ulRecNo != 0 )
            {
               if( ulRecNo < ulRec )
                  k = -1;
               else if( ulRecNo > ulRec )
                  k = 1;
               else
               {
                  *fStop = ZH_TRUE;
                  return u;
               }
            }
         }

         if( k < 0 )
            break;
         else if( k == 0 && ! fLast )
         {
            *fStop = ZH_TRUE;
            break;
         }
      }
      return u;
   }
   else
   {
      iBegin = 0;
      iLast = pPage->uiKeys;
      iEnd = iLast - 1;

      while( iBegin <= iEnd )
      {
         i = ( iBegin + iEnd ) >> 1;
         k = zh_nsxValCompare( pTag, key, keylen,
                               zh_nsxGetKeyVal( pPage, pTag->KeyLength, i ),
                               pTag->KeyLength, mode );
         if( k == 0 )
         {
            if( ulRecNo != 0 )
            {
               ulRec = zh_nsxGetKeyRec( pPage, pTag->KeyLength, i );
               if( ulRecNo < ulRec )
                  k = -1;
               else if( ulRecNo > ulRec )
                  k = 1;
               else
               {
                  *fStop = ZH_TRUE;
                  return i + 1;
               }
            }
         }

         if( fLast ? k >= 0 : k > 0 )
            iBegin = i + 1;
         else
         {
            if( k == 0 )
               *fStop = ZH_TRUE;
            iLast = i;
            iEnd = i - 1;
         }
      }
      return iLast;
   }
}

/*
 * set page path to given key in tag
 */
static ZH_BOOL zh_nsxTagKeyFind( LPTAGINFO pTag, LPKEYINFO pKey, ZH_USHORT uiLen )
{
   LPPAGEINFO pPage;
   ZH_ULONG ulPage, ulRecNo = 0;
   int iKey;
   ZH_BOOL fStop = ZH_FALSE, fLast = ZH_FALSE, fOut;

   if( pKey->rec == NSX_MAX_REC_NUM )         /* for seek last */
      fLast = ZH_TRUE;
   else if( pKey->rec != NSX_IGNORE_REC_NUM ) /* for key del, add and current key */
      ulRecNo = pKey->rec;
   /* else -> normal seek */

   pTag->stackLevel = 0;

   ulPage = zh_nsxTagRootBlock( pTag );
   if( ! ulPage )
   {
      pTag->TagBOF = pTag->TagEOF = ZH_TRUE;
      pTag->CurKeyInfo->rec = 0;
      return ZH_FALSE;
   }

   for( ;; )
   {
      pPage = zh_nsxPageLoad( pTag, ulPage );
      if( ! pPage )
         return ZH_FALSE;

      iKey = zh_nsxPageKeyFind( pTag, pPage, pKey->val, uiLen, pKey->mode,
                                fLast, ulRecNo, &fStop );
      zh_nsxTagSetPageStack( pTag, pPage, ( ZH_USHORTCAST ) iKey );
      if( ( fStop && ulRecNo ) || zh_nsxIsLeaf( pPage ) )
         break;

      ulPage = iKey == 0 ? zh_nsxGetLowerPage( pPage ) :
                           zh_nsxGetKeyPage( pPage, pTag->KeyLength, iKey - 1 );
      zh_nsxPageRelease( pTag, pPage );
   }

   fOut = ! zh_nsxTagGetCurKey( pTag, pPage, ( ZH_USHORTCAST ) iKey );
   zh_nsxPageRelease( pTag, pPage );
   if( fOut )
      return ZH_FALSE;

   if( ulRecNo == 0 )
   {
      if( fLast )
      {
         if( ! zh_nsxTagPrevKey( pTag ) )
         {
            fOut = ZH_TRUE;
            fStop = ZH_FALSE;
         }
         else
         {
            fStop = zh_nsxValCompare( pTag, pKey->val, uiLen, pTag->CurKeyInfo->val,
                                      pTag->KeyLength, pKey->mode ) == 0;
         }
      }
      else if( ! fStop && pTag->CurKeyInfo->rec == 0 )
      {
         if( ! zh_nsxTagNextKey( pTag ) )  /* Tag EOF */
         {
            fOut = ZH_TRUE;
            fStop = ZH_FALSE;
         }
         else
         {
            fStop = zh_nsxValCompare( pTag, pKey->val, uiLen,
                                      pTag->CurKeyInfo->val, pTag->KeyLength,
                                      pKey->mode ) == 0;
         }
      }
   }

   pTag->TagBOF = pTag->TagEOF = fOut || pTag->CurKeyInfo->rec == 0;

   return fStop;
}


/* ************************************************************************* */
/* tag update functions */
/* ************************************************************************* */

/*
 * add key to branch page
 */
static void zh_nsxPageKeyAdd( LPTAGINFO pTag, LPPAGEINFO pPage, ZH_USHORT uiPos,
                              LPKEYINFO pKey )
{
   ZH_UCHAR * ptr = zh_nsxGetBranchKeyPtr( pPage, pTag->KeyLength, uiPos );

   if( uiPos < pPage->uiKeys )
   {
      memmove( zh_nsxGetBranchKeyPtr( pPage, pTag->KeyLength, uiPos + 1 ), ptr,
               zh_nsxBranchKeySize( pPage, pTag->KeyLength ) *
               ( pPage->uiKeys - uiPos ) );
   }

   zh_nsxBranchKeySetPage( ptr, pKey->page );
   zh_nsxBranchKeySetRec( ptr, pKey->rec );
   memcpy( zh_nsxBranchKeyVal( ptr ), pKey->val, pTag->KeyLength );
   pPage->Changed = ZH_TRUE;
   pPage->uiKeys++;
}

/*
 * split single branch page into two and return key to the new one
 */
static LPKEYINFO zh_nsxPageSplit( LPTAGINFO pTag, LPPAGEINFO pPage,
                                  LPKEYINFO pKey, ZH_USHORT uiPos )
{
   LPPAGEINFO pNewPage;
   ZH_USHORT n, uiKeys, uiHalf, uiLen;

   pNewPage = zh_nsxPageNew( pTag, ZH_FALSE );
   if( ! pNewPage )
   {
      zh_nsxKeyFree( pKey );
      return NULL;
   }
   zh_nsxSetKeyRecSize( pNewPage, 4 );

   uiLen = zh_nsxBranchKeySize( pPage, pTag->KeyLength );
   uiKeys = pPage->uiKeys;
   uiHalf = ( uiKeys + 1 ) >> 1;

   if( uiHalf < uiPos )
      uiHalf++;

   if( uiHalf < uiPos )
   {
      n = uiPos - uiHalf;
      if( n )
         memcpy( zh_nsxGetBranchKeyPtr( pNewPage, pTag->KeyLength, 0 ),
                 zh_nsxGetBranchKeyPtr( pPage, pTag->KeyLength, uiHalf ),
                 uiLen * n );
      pNewPage->uiKeys = n;
      zh_nsxPageKeyAdd( pTag, pNewPage, n, pKey );
      n = uiKeys - uiPos;
      if( n )
      {
         memcpy( zh_nsxGetBranchKeyPtr( pNewPage, pTag->KeyLength, pNewPage->uiKeys ),
                 zh_nsxGetBranchKeyPtr( pPage, pTag->KeyLength, uiPos ),
                 uiLen * n );
         pNewPage->uiKeys += n;
      }
      pPage->uiKeys = uiHalf;
   }
   else
   {
      memcpy( zh_nsxGetBranchKeyPtr( pNewPage, pTag->KeyLength, 0 ),
              zh_nsxGetBranchKeyPtr( pPage, pTag->KeyLength, uiHalf ),
              uiLen * ( uiKeys - uiHalf ) );
      pNewPage->uiKeys = uiKeys - uiHalf;
      pPage->uiKeys = uiHalf;
      if( uiPos != uiHalf )
         zh_nsxPageKeyAdd( pTag, pPage, uiPos, pKey );
   }

   if( uiPos != uiHalf )
   {
      ZH_UCHAR * ptr = zh_nsxGetBranchKeyPtr( pPage, pTag->KeyLength, pPage->uiKeys - 1 );
      pKey->page = zh_nsxBranchKeyPage( ptr );
      pKey->rec = zh_nsxBranchKeyRec( ptr );
      memcpy( pKey->val, zh_nsxBranchKeyVal( ptr ), pTag->KeyLength );
      pPage->uiKeys--;
   }
   zh_nsxSetLowerPage( pNewPage, pKey->page );
   pKey->page = pNewPage->Page;

   pPage->Changed = pNewPage->Changed = ZH_TRUE;
#ifdef ZH_NSX_DEBUG
   zh_nsxPageCheckKeys( pNewPage, pTag, uiPos, 1 );
   zh_nsxPageCheckKeys( pPage, pTag, uiPos - pNewPage->uiKeys, 2 );
#endif
   zh_nsxPageRelease( pTag, pNewPage );

   return pKey;
}


/*
 * insert key into current stack page
 */
static ZH_BOOL zh_nsxTagInsertKey( LPTAGINFO pTag, LPPAGEINFO pPage,
                                   LPKEYINFO pKey, ZH_UCHAR * pKeyPrev )
{
   ZH_USHORT uiOffset = NSX_LEAFKEYOFFSET, uiKeyOffset = 0,
             uiKey = pTag->stack[ pTag->stackLevel - 1 ].ikey,
             uiKeys = pPage->uiKeys, uiHalfOffset = 0, uiHalfKeys = 0, u;
   int iLen = pTag->KeyLength;
   ZH_ULONG ulRecNo;
   ZH_UCHAR pKeyVal[ NSX_MAXKEYLEN ], * pKeyBuff, * ptr, ucRecSize;
   LPKEYINFO pNewKey = NULL;

#ifdef ZH_NSX_DEBUG_EXT
   zh_nsxPageCheckKeys( pPage, pTag, uiKey, 41 );
#endif

   ptr = pKeyBuff = ( ZH_UCHAR * ) zh_xgrab( ( uiKeys + 1 ) * ( iLen + 4 ) );
   if( pKeyPrev )
      memcpy( pKeyVal, pKeyPrev, iLen );
   else
      zh_nsxTagGetPrevKey( pTag, pKeyVal, pTag->stackLevel - 1 );

   for( u = 0; u <= uiKeys; ++u )
   {
      if( u == uiKey )
      {
         uiKeyOffset = uiOffset;
         ZH_PUT_LE_UINT32( ptr, pKey->rec );
         ptr += 4;
         memcpy( ptr, pKey->val, iLen );
         ptr += iLen;
      }
      else
      {
         if( uiHalfOffset == 0 && u < uiKey && uiOffset >= NSX_LEAFSPLITOFFSET )
         {
            uiHalfOffset = uiOffset;
            uiHalfKeys = u;
         }
         uiOffset = zh_nsxLeafGetKey( pTag, pPage, uiOffset, pKeyVal, &ulRecNo );
         if( uiOffset == 0 )
         {
            zh_xfree( pKeyBuff );
            pTag->stackLevel = 0;
            zh_nsxPageRelease( pTag, pPage );
            zh_nsxCorruptError( pTag->pIndex );
            return ZH_FALSE;
         }
         ZH_PUT_LE_UINT32( ptr, ulRecNo );
         ptr += 4;
         memcpy( ptr, pKeyVal, iLen );
         ptr += iLen;
      }
   }
   pPage->uiKeys++;
   ucRecSize = zh_nsxGetKeyRecSize( pPage );
   if( zh_nsxGetRecSize( pKey->rec ) > ucRecSize )
   {
      ucRecSize = zh_nsxGetRecSize( pKey->rec );
      zh_nsxSetKeyRecSize( pPage, ucRecSize );
      uiKeyOffset = NSX_LEAFKEYOFFSET;
      uiKey = uiHalfOffset = uiHalfKeys = 0;
   }
   pPage->uiOffset = uiKeyOffset;
   ptr = pKeyBuff + uiKey * ( iLen + 4 );
   if( uiKey == 0 )
   {
      if( zh_nsxTagGetPrevKey( pTag, pKeyVal, pTag->stackLevel - 1 ) )
         pKeyPrev = pKeyVal;
      else
         pKeyPrev = NULL;
   }
   else
      pKeyPrev = ptr - iLen;

   pPage->Changed = ZH_TRUE;
   for(; uiKey <= uiKeys; ++uiKey )
   {
      if( uiHalfOffset == 0 && pPage->uiOffset >= NSX_LEAFSPLITOFFSET )
      {
         uiHalfOffset = pPage->uiOffset;
         uiHalfKeys = uiKey;
      }
      uiOffset = zh_nsxLeafPutKey( pTag, pPage, pPage->uiOffset, pKeyPrev,
                                   ptr + 4, ZH_GET_LE_UINT32( ptr ) );
      if( uiOffset == 0 )
      {
         if( pNewKey )
            zh_errInternal( 9999, "zh_nsxTagInsertKey: multiple leaf page split", NULL, NULL );
         else if( uiHalfOffset == 0 )
            zh_errInternal( 9999, "zh_nsxTagInsertKey: split offset not set", NULL, NULL );

         pPage->uiOffset = uiHalfOffset;
         uiKey = pPage->uiKeys = uiHalfKeys;
         ptr = pKeyBuff + uiKey * ( iLen + 4 );
         zh_nsxPageType( pPage ) &= ~NSX_ROOTPAGE;
#ifdef ZH_NSX_DEBUG
         zh_nsxPageCheckKeys( pPage, pTag, uiKey, 41 );
#endif
         zh_nsxPageRelease( pTag, pPage );
         pPage = zh_nsxPageNew( pTag, ZH_FALSE );
         if( ! pPage )
         {
            zh_xfree( pKeyBuff );
            pTag->CurKeyOffset = 0;
            pTag->stackLevel = 0;
            return ZH_FALSE;
         }
         pNewKey = zh_nsxKeyNew( iLen );
         pNewKey->page = pPage->Page;
         pNewKey->rec = ZH_GET_LE_UINT32( ptr );
         memcpy( pNewKey->val, ptr + 4, iLen );
         zh_nsxSetPageType( pPage, NSX_LEAFPAGE );
         zh_nsxSetKeyRecSize( pPage, ucRecSize );
         pPage->uiKeys = uiKeys - uiKey;
         pPage->Changed = ZH_TRUE;
         uiOffset = NSX_LEAFKEYOFFSET;
      }
      pPage->uiOffset = uiOffset;
      pKeyPrev = ptr + 4;
      ptr += iLen + 4;
   }
#ifdef ZH_NSX_DEBUG
   zh_nsxPageCheckKeys( pPage, pTag, uiKey, 42 );
#endif
   zh_xfree( pKeyBuff );

   if( pNewKey )
   {
      int iLevel = pTag->stackLevel - 1;

      while( --iLevel >= 0 && pNewKey )
      {
         int iKey;

         zh_nsxPageRelease( pTag, pPage );
         pPage = zh_nsxPageLoad( pTag, pTag->stack[ iLevel ].page );
         if( ! pPage )
         {
            zh_nsxKeyFree( pNewKey );
            pTag->CurKeyOffset = 0;
            pTag->stackLevel = 0;
            return ZH_FALSE;
         }
         iKey = pTag->stack[ iLevel ].ikey;
         if( pPage->uiKeys < pTag->MaxKeys )
         {
            zh_nsxPageKeyAdd( pTag, pPage, ( ZH_USHORTCAST ) iKey, pNewKey );
            zh_nsxKeyFree( pNewKey );
            pNewKey = NULL;
         }
         else
         {
            pNewKey = zh_nsxPageSplit( pTag, pPage, pNewKey, ( ZH_USHORTCAST ) iKey );
         }
      }

      if( pNewKey )
      {
         if( zh_nsxIsRoot( pPage ) )
         {
            zh_nsxPageType( pPage ) &= ~NSX_ROOTPAGE;
            pPage->Changed = ZH_TRUE;
         }
         zh_nsxPageRelease( pTag, pPage );
         pPage = zh_nsxPageNew( pTag, ZH_FALSE );
         if( ! pPage )
         {
            zh_nsxKeyFree( pNewKey );
            pTag->CurKeyOffset = 0;
            pTag->stackLevel = 0;
            return ZH_FALSE;
         }
         zh_nsxSetPageType( pPage, NSX_ROOTPAGE );
         zh_nsxSetKeyRecSize( pPage, 4 );
         zh_nsxSetLowerPage( pPage, pTag->RootBlock );
         zh_nsxPageKeyAdd( pTag, pPage, 0, pNewKey );
         pTag->RootBlock = pPage->Page;
         pTag->HdrChanged = ZH_TRUE;
         pTag->CurKeyOffset = 0;
         pTag->stackLevel = 0;
         zh_nsxKeyFree( pNewKey );
      }
   }
   zh_nsxPageRelease( pTag, pPage );
   pTag->CurKeyOffset = 0;
   pTag->stackLevel = 0;
   return ZH_TRUE;
}

/*
 * add key to the index at the current page path
 */
static ZH_BOOL zh_nsxTagKeyAdd( LPTAGINFO pTag, LPKEYINFO pKey )
{
   LPPAGEINFO pPage;
   ZH_BOOL fFound, fBottom = ZH_FALSE;

   if( pTag->UniqueKey )
   {
      ZH_ULONG ulRecNo = pKey->rec;

      pKey->rec = NSX_IGNORE_REC_NUM;
      fFound = zh_nsxTagKeyFind( pTag, pKey, pTag->KeyLength );
      pKey->rec = ulRecNo;
      if( fFound )
         return ZH_FALSE;
      fBottom = ZH_TRUE;
   }
   else
   {
      pKey->page = NSX_MAX_REC_NUM;
      fFound = zh_nsxTagKeyFind( pTag, pKey, pTag->KeyLength );
      pKey->page = 0;
      if( fFound )
      {
         if( pTag->MultiKey )
            fBottom = ZH_TRUE;
         else
            return ZH_FALSE;
      }
   }

   if( pTag->stackLevel == 0 )
      return ZH_FALSE;
   pPage = zh_nsxPageLoad( pTag, pTag->stack[ pTag->stackLevel - 1 ].page );
   if( ! pPage )
      return ZH_FALSE;
   if( fBottom && ! zh_nsxIsLeaf( pPage ) )
   {
      ZH_ULONG ulPage;

      ulPage = pTag->stack[ pTag->stackLevel - 1 ].ikey == 0 ?
               zh_nsxGetLowerPage( pPage ) :
               zh_nsxGetKeyPage( pPage, pTag->KeyLength, pTag->stack[ pTag->stackLevel - 1 ].ikey - 1 );
      zh_nsxPageRelease( pTag, pPage );
      pPage = zh_nsxPageBottomMove( pTag, ulPage );
      if( ! pPage )
         return ZH_FALSE;
      if( pTag->stack[ pTag->stackLevel - 1 ].ikey < ( ZH_SHORT ) pPage->uiKeys )
         pTag->stack[ pTag->stackLevel - 1 ].ikey++;
   }

   return zh_nsxTagInsertKey( pTag, pPage, pKey, NULL );
}

/*
 * del key from the leaf page
 */
static void zh_nsxPageLeafKeyDel( LPTAGINFO pTag, LPPAGEINFO pPage, ZH_USHORT uiKey )
{
   ZH_UCHAR pKeyVal[ NSX_MAXKEYLEN ], pKeyVal2[ NSX_MAXKEYLEN ];
   ZH_ULONG ulRecNo;
   ZH_USHORT uiOffset = NSX_LEAFKEYOFFSET, u;
   ZH_BOOL fPrev;

   fPrev = zh_nsxTagGetPrevKey( pTag, pKeyVal, pTag->stackLevel - 1 );
   for( u = 0; u < uiKey; ++u )
   {
      uiOffset = zh_nsxLeafGetKey( pTag, pPage, uiOffset, pKeyVal, &ulRecNo );
      if( uiOffset == 0 )
      {
         zh_nsxCorruptError( pTag->pIndex );
         return;
      }
   }
   if( --pPage->uiKeys > uiKey )
   {
      ZH_UCHAR * pPrevVal = NULL;

      /* save previous key value */
      if( fPrev || uiKey )
      {
         memcpy( pKeyVal2, pKeyVal, pTag->KeyLength );
         pPrevVal = pKeyVal2;
      }

      /* get deleted key value */
      u = zh_nsxLeafGetKey( pTag, pPage, uiOffset, pKeyVal, &ulRecNo );
      if( u != 0 )
         /* get next key value */
         u = zh_nsxLeafGetKey( pTag, pPage, u, pKeyVal, &ulRecNo );
      if( u == 0 )
      {
         zh_nsxCorruptError( pTag->pIndex );
         return;
      }
      /* store next key encoded with previous key value */
      uiOffset = zh_nsxLeafPutKey( pTag, pPage, uiOffset,
                                   pPrevVal, pKeyVal, ulRecNo );
      /* move other keys */
      memmove( zh_nsxPageBuffer( pPage ) + uiOffset,
               zh_nsxPageBuffer( pPage ) + u, pPage->uiOffset - u );
      uiOffset += pPage->uiOffset - u;
   }
   pPage->uiOffset = uiOffset;
   pPage->Changed = ZH_TRUE;
}

/*
 * del key at the current page path from the index
 */
static ZH_BOOL zh_nsxTagKeyDel( LPTAGINFO pTag, LPKEYINFO pKey )
{
   int iLevel, iKey;
   LPPAGEINFO pPage;
   ZH_ULONG ulPage, ulRecNo;
   ZH_BOOL fResult = ZH_TRUE;

   pKey->page = 0;
   if( pTag->stackLevel == 0 || pTag->CurKeyInfo->rec != pKey->rec ||
       memcmp( pTag->CurKeyInfo->val, pKey->val, pTag->KeyLength ) != 0 )
   {
      if( ! zh_nsxTagKeyFind( pTag, pKey, pTag->KeyLength ) )
         return ZH_FALSE;
   }

   iLevel = pTag->stackLevel - 1;
   iKey = pTag->stack[ iLevel ].ikey;
   pPage = zh_nsxPageLoad( pTag, pTag->stack[ iLevel ].page );
   if( ! pPage )
      return ZH_FALSE;

   if( ! zh_nsxIsLeaf( pPage ) )
   {
      int iBaseLevel, iBaseKey;
      LPPAGEINFO pBasePage;

      pBasePage = pPage;
      iBaseKey = iKey - 1; /* iBaseKey >= 0 */
      iBaseLevel = iLevel;
      ulPage = zh_nsxGetKeyPage( pBasePage, pTag->KeyLength, iBaseKey );
      pPage = zh_nsxPageTopMove( pTag, ulPage );
      if( ! pPage )
      {
         zh_nsxPageRelease( pTag, pBasePage );
         return ZH_FALSE;
      }
      iLevel = pTag->stackLevel - 1;
      iKey = pTag->stack[ iLevel ].ikey;  /* iKey = 0 */
      if( ! zh_nsxPageGetLeafKey( pTag, pPage, ( ZH_USHORTCAST ) iKey,
                                  zh_nsxGetKeyVal( pBasePage, pTag->KeyLength, iBaseKey ),
                                  &ulRecNo ) )
      {
         zh_nsxPageRelease( pTag, pBasePage );
         zh_nsxPageRelease( pTag, pPage );
         pTag->stackLevel = 0;
         return ZH_FALSE;
      }
      zh_nsxSetKeyRec( pBasePage, pTag->KeyLength, iBaseKey, ulRecNo );
      if( ! pTag->stack[ iBaseLevel ].value )
         pTag->stack[ iBaseLevel ].value = ( ZH_UCHAR * ) zh_xgrab( pTag->KeyLength );
      memcpy( pTag->stack[ iBaseLevel ].value,
              zh_nsxGetKeyVal( pBasePage, pTag->KeyLength, iBaseKey ),
              pTag->KeyLength );
      pBasePage->Changed = ZH_TRUE;
#ifdef ZH_NSX_DEBUG
      zh_nsxPageCheckKeys( pBasePage, pTag, iBaseKey, 61 );
#endif
      zh_nsxPageRelease( pTag, pBasePage );
   }

   if( pPage->uiKeys > 1 )
   {
      zh_nsxPageLeafKeyDel( pTag, pPage, ( ZH_USHORTCAST ) iKey );
#ifdef ZH_NSX_DEBUG
      zh_nsxPageCheckKeys( pPage, pTag, iKey, 62 );
#endif
      zh_nsxPageRelease( pTag, pPage );
   }
   else
   {
      while( --iLevel >= 0 )
      {
         zh_nsxPageFree( pTag, pPage );
         zh_nsxPageRelease( pTag, pPage );
         pPage = zh_nsxPageLoad( pTag, pTag->stack[ iLevel ].page );
         if( pPage->uiKeys )
            break;
      }
      if( iLevel < 0 )
      {
         zh_nsxSetPageType( pPage, NSX_ROOTPAGE | NSX_LEAFPAGE );
         zh_nsxSetKeyRecSize( pPage, zh_nsxGetRecSize( 0 ) );
         pPage->uiOffset = NSX_LEAFKEYOFFSET;
         pPage->uiKeys = 0;
         pPage->Changed = ZH_TRUE;
         zh_nsxPageRelease( pTag, pPage );
      }
      else
      {
         LPKEYINFO pKeyNew = zh_nsxKeyNew( pTag->KeyLength );
         ZH_UCHAR * pKeyPtr, * pKeyPrev = NULL;

         iKey = pTag->stack[ iLevel ].ikey;
         if( iKey == 0 )
         {
            ulPage = zh_nsxGetKeyPage( pPage, pTag->KeyLength, 0 );
            zh_nsxSetLowerPage( pPage, ulPage );
            pKeyPrev = pKeyNew->val;
         }
         else
         {
            ulPage = --iKey == 0 ? zh_nsxGetLowerPage( pPage ) :
                     zh_nsxGetKeyPage( pPage, pTag->KeyLength, iKey - 1 );
         }
         pKeyPtr = zh_nsxGetBranchKeyPtr( pPage, pTag->KeyLength, iKey );
         pKeyNew->rec = zh_nsxBranchKeyRec( pKeyPtr );
         memcpy( pKeyNew->val, zh_nsxBranchKeyVal( pKeyPtr ), pTag->KeyLength );
         if( --pPage->uiKeys > iKey )
         {
            memmove( pKeyPtr,
                     zh_nsxGetBranchKeyPtr( pPage, pTag->KeyLength, iKey + 1 ),
                     zh_nsxBranchKeySize( pPage, pTag->KeyLength ) *
                     ( pPage->uiKeys - iKey ) );
         }
         pPage->Changed = ZH_TRUE;
         pTag->stackLevel = ( ZH_USHORTCAST ) iLevel;
         zh_nsxTagSetPageStack( pTag, pPage, ( ZH_USHORTCAST ) iKey );
         zh_nsxPageRelease( pTag, pPage );
         if( pKeyPrev )
         {
            pPage = zh_nsxPageTopMove( pTag, ulPage );
         }
         else
         {
            pPage = zh_nsxPageBottomMove( pTag, ulPage );
            if( pPage )
               pTag->stack[ pTag->stackLevel - 1 ].ikey++;
         }
         fResult = pPage && zh_nsxTagInsertKey( pTag, pPage, pKeyNew, pKeyPrev );
         zh_nsxKeyFree( pKeyNew );
      }
   }
   pTag->CurKeyOffset = 0;
   pTag->stackLevel = 0;
   return fResult;
}

/* ************************************************************************* */
/* end of tag update functions */
/* ************************************************************************* */


/*
 * Skip in tag respecting record filter only
 */
static void zh_nsxTagSkipFilter( LPTAGINFO pTag, ZH_BOOL fForward )
{
   ZH_BOOL fBack, fEof = fForward ? pTag->TagEOF : pTag->TagBOF;

   fBack = pTag->fUsrDescend ? fForward : ! fForward;

   while( ! fEof && ! zh_nsxCheckRecordScope( pTag->pIndex->pArea,
                                              pTag->CurKeyInfo->rec ) )
   {
      if( fBack )
         fEof = ! zh_nsxTagPrevKey( pTag );
      else
         fEof = ! zh_nsxTagNextKey( pTag );

      if( ! fEof && ! zh_nsxKeyInScope( pTag, pTag->CurKeyInfo ) )
      {
         fEof = ZH_TRUE;
      }
   }
   if( fEof )
   {
      if( fForward )
         pTag->TagEOF = ZH_TRUE;
      else
         pTag->TagBOF = ZH_TRUE;
   }
}

/*
 * go to the first visible record in Tag
 */
static void zh_nsxTagGoTop( LPTAGINFO pTag )
{
   PZH_NSXSCOPE pScope = pTag->fUsrDescend ? &pTag->bottom : &pTag->top;

   if( pScope->scopeKeyLen )
      zh_nsxTagKeyFind( pTag, pScope->scopeKey, pScope->scopeKeyLen );
   else if( pTag->fUsrDescend )
      zh_nsxTagBottomKey( pTag );
   else
      zh_nsxTagTopKey( pTag );

   pTag->TagEOF = pTag->CurKeyInfo->rec == 0 ||
                  ! zh_nsxKeyInScope( pTag, pTag->CurKeyInfo );

   if( ! pTag->TagEOF && pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter )
      zh_nsxTagSkipFilter( pTag, ZH_TRUE );

   pTag->TagBOF = pTag->TagEOF;
}

/*
 * go to the last visible record in Tag
 */
static void zh_nsxTagGoBottom( LPTAGINFO pTag )
{
   PZH_NSXSCOPE pScope = pTag->fUsrDescend ? &pTag->top : &pTag->bottom;

   if( pScope->scopeKeyLen )
      zh_nsxTagKeyFind( pTag, pScope->scopeKey, pScope->scopeKeyLen );
   else if( pTag->fUsrDescend )
      zh_nsxTagTopKey( pTag );
   else
      zh_nsxTagBottomKey( pTag );

   pTag->TagBOF = pTag->CurKeyInfo->rec == 0 ||
                  ! zh_nsxKeyInScope( pTag, pTag->CurKeyInfo );

   if( ! pTag->TagBOF && pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter )
      zh_nsxTagSkipFilter( pTag, ZH_FALSE );

   pTag->TagEOF = pTag->TagBOF;
}

/*
 * skip to Next Key in the Tag
 */
static void zh_nsxTagSkipNext( LPTAGINFO pTag )
{
   pTag->TagBOF = ZH_FALSE;

   if( pTag->stackLevel == 0 )
      pTag->TagEOF = ZH_TRUE;
   else if( ! zh_nsxInTopScope( pTag, pTag->CurKeyInfo->val ) )
      zh_nsxTagGoTop( pTag );
   else if( pTag->fUsrDescend )
      pTag->TagEOF = ! zh_nsxTagPrevKey( pTag );
   else
      pTag->TagEOF = ! zh_nsxTagNextKey( pTag );

   if( ! pTag->TagEOF && ! zh_nsxKeyInScope( pTag, pTag->CurKeyInfo ) )
      pTag->TagEOF = ZH_TRUE;

   if( ! pTag->TagEOF && pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter )
      zh_nsxTagSkipFilter( pTag, ZH_TRUE );
}

/*
 * skip to Previous Key in the Tag
 */
static void zh_nsxTagSkipPrev( LPTAGINFO pTag )
{
   pTag->TagEOF = ZH_FALSE;

   if( pTag->stackLevel == 0 )
      zh_nsxTagGoBottom( pTag );
   else if( pTag->fUsrDescend )
      pTag->TagBOF = ! zh_nsxTagNextKey( pTag );
   else
      pTag->TagBOF = ! zh_nsxTagPrevKey( pTag );

   if( ! pTag->TagBOF && ! zh_nsxKeyInScope( pTag, pTag->CurKeyInfo ) )
      pTag->TagBOF = ZH_TRUE;

   if( ! pTag->TagBOF && pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter )
      zh_nsxTagSkipFilter( pTag, ZH_FALSE );
}

/*
 * count keys in the given page and all subpages
 */
static ZH_ULONG zh_nsxPageCountKeys( LPTAGINFO pTag, ZH_ULONG ulPage )
{
   LPPAGEINFO pPage = zh_nsxPageLoad( pTag, ulPage );
   ZH_ULONG ulKeys;

   if( ! pPage )
      return 0;

   ulKeys = pPage->uiKeys;
   if( ! zh_nsxIsLeaf( pPage ) )
   {
      ZH_USHORT u;
      ulKeys += zh_nsxPageCountKeys( pTag, zh_nsxGetLowerPage( pPage ) );
      for( u = 0; u < pPage->uiKeys; u++ )
      {
         ulKeys += zh_nsxPageCountKeys( pTag,
                              zh_nsxGetKeyPage( pPage, pTag->KeyLength, u ) );
      }
   }
   zh_nsxPageRelease( pTag, pPage );

   return ulKeys;
}

/*
 * count relative position of current location in page stack
 */
static double zh_nsxTagCountRelKeyPos( LPTAGINFO pTag )
{
   int iLevel = pTag->stackLevel;
   double dPos = 1.0;

   while( --iLevel >= 0 )
   {
      LPPAGEINFO pPage = zh_nsxPageLoad( pTag, pTag->stack[ iLevel ].page );
      int iKeys, iKey;
      if( ! pPage )
         break;
      iKey = pTag->stack[ iLevel ].ikey;
      iKeys = pPage->uiKeys;
      if( ! zh_nsxIsLeaf( pPage ) )
      {
         if( iKey && iLevel == pTag->stackLevel - 1 )
            --iKey;
         ++iKeys;
      }
      else
         dPos = 0.5;
      if( iKeys )
         dPos = ( dPos + iKey ) / iKeys;
      zh_nsxPageRelease( pTag, pPage );
   }
   if( pTag->fUsrDescend )
      dPos = 1.0 - dPos;
   return dPos;
}

static void zh_nsxTagGoToRelKeyPos( LPTAGINFO pTag, double dPos )
{
   LPPAGEINFO pPage;
   ZH_ULONG ulPage;
   int iKey, iKeys;

   if( pTag->fUsrDescend )
      dPos = 1.0 - dPos;

   pTag->stackLevel = 0;

   ulPage = zh_nsxTagRootBlock( pTag );
   if( ! ulPage )
      return;

   for( ;; )
   {
      pPage = zh_nsxPageLoad( pTag, ulPage );
      if( ! pPage )
      {
         pTag->stackLevel = 0;
         return;
      }
      iKeys = pPage->uiKeys;
      if( iKeys == 0 )
         iKey = 0;
      else
      {
         if( ! zh_nsxIsLeaf( pPage ) )
            ++iKeys;
         iKey = ( int ) ( dPos * iKeys );
         if( iKey >= iKeys )
            iKey = iKeys - 1;
         dPos = dPos * iKeys - iKey;
         if( dPos <= 0.0 )
            dPos = 0.0;
         else if( dPos >= 1.0 )
            dPos = 1.0;
      }
      zh_nsxTagSetPageStack( pTag, pPage, ( ZH_USHORTCAST ) iKey );
      if( zh_nsxIsLeaf( pPage ) )
         break;
      ulPage = iKey == 0 ? zh_nsxGetLowerPage( pPage ) :
                           zh_nsxGetKeyPage( pPage, pTag->KeyLength, iKey - 1 );
      zh_nsxPageRelease( pTag, pPage );
   }

   zh_nsxTagGetCurKey( pTag, pPage, ( ZH_USHORTCAST ) iKey );
   zh_nsxPageRelease( pTag, pPage );

   /* reposition for branch keys */
   if( dPos > 0.75 )
      zh_nsxTagNextKey( pTag );
   else if( dPos < 0.25 )
      zh_nsxTagPrevKey( pTag );
}

/*
 * refresh CurKey value and set proper path from RootPage to LeafPage
 */
static ZH_BOOL zh_nsxCurKeyRefresh( LPTAGINFO pTag )
{
   NSXAREAP pArea = pTag->pIndex->pArea;

   if( pArea->dbfarea.lpdbPendingRel )
      SELF_FORCEREL( &pArea->dbfarea.area );

   if( ! pArea->dbfarea.fPositioned )
   {
      pTag->stackLevel = 0;
      pTag->TagBOF = pTag->TagEOF = ZH_TRUE;
      pTag->CurKeyInfo->rec = 0;
      return ZH_FALSE;
   }
   else if( pTag->stackLevel == 0 || pTag->CurKeyInfo->rec != pArea->dbfarea.ulRecNo )
   {
      ZH_BOOL fValidBuf = pArea->dbfarea.fValidBuffer;
      ZH_BYTE buf[ NSX_MAXKEYLEN ];
      ZH_BOOL fBuf = ZH_FALSE;
      LPKEYINFO pKey = NULL;
      /* Try to find previous if it's key for the same record */
      if( pTag->CurKeyInfo->rec == pArea->dbfarea.ulRecNo )
      {
         fBuf = ZH_TRUE;
         memcpy( buf, pTag->CurKeyInfo->val, pTag->KeyLength );
         pKey = zh_nsxKeyCopy( pKey, pTag->CurKeyInfo, pTag->KeyLength );
         zh_nsxTagKeyFind( pTag, pKey, pTag->KeyLength );
      }
      if( pTag->CurKeyInfo->rec != pArea->dbfarea.ulRecNo )
      {
         /* not found, create new key from DBF and if differs seek again */
         pKey = zh_nsxEvalKey( pKey, pTag );
         if( ! fBuf || memcmp( buf, pKey->val, pTag->KeyLength ) != 0 )
            zh_nsxTagKeyFind( pTag, pKey, pTag->KeyLength );
         /* not found, if key was generated from DBF buffer then force to
          * update it, create the new key and if differs seek again */
         if( pTag->CurKeyInfo->rec != pArea->dbfarea.ulRecNo && fValidBuf )
         {
            SELF_GOTO( &pArea->dbfarea.area, pArea->dbfarea.ulRecNo );
            memcpy( buf, pKey->val, pTag->KeyLength );
            pKey = zh_nsxEvalKey( pKey, pTag );
            if( memcmp( buf, pKey->val, pTag->KeyLength ) != 0 )
               zh_nsxTagKeyFind( pTag, pKey, pTag->KeyLength );
         }
         if( pTag->CurKeyInfo->rec != pArea->dbfarea.ulRecNo && pTag->Template )
         {
            zh_nsxTagGoTop( pTag );
            while( ! pTag->TagEOF )
            {
               if( pTag->CurKeyInfo->rec == pArea->dbfarea.ulRecNo )
                  break;
               zh_nsxTagSkipNext( pTag );
            }
         }
      }
      zh_nsxKeyFree( pKey );
      return pTag->CurKeyInfo->rec != 0 && pTag->CurKeyInfo->rec == pArea->dbfarea.ulRecNo;
   }
   pTag->TagBOF = pTag->TagEOF = ZH_FALSE;
   return ZH_TRUE;
}

/*
 * free pages allocated by tag
 */
static ZH_BOOL zh_nsxTagPagesFree( LPTAGINFO pTag, ZH_ULONG ulPage )
{
   LPPAGEINFO pPage = zh_nsxPageLoad( pTag, ulPage );
   ZH_BOOL fOK = pPage != NULL;

   if( fOK )
   {
      if( ! zh_nsxIsLeaf( pPage ) )
      {
         ZH_USHORT u;
         fOK = zh_nsxTagPagesFree( pTag, zh_nsxGetLowerPage( pPage ) );
         for( u = 0; fOK && u < pPage->uiKeys; u++ )
         {
            fOK = zh_nsxTagPagesFree( pTag,
                              zh_nsxGetKeyPage( pPage, pTag->KeyLength, u ) );
         }
      }
      if( fOK )
      {
         zh_nsxPageFree( pTag, pPage );
         if( ! pPage->pPrev )
            /* page is in not in hot pages list, write it now */
            fOK = zh_nsxPageSave( pTag->pIndex, pPage );
      }
      zh_nsxPageRelease( pTag, pPage );
   }

   return fOK;
}

/*
 * free space allocated by tag
 */
static ZH_ERRCODE zh_nsxTagSpaceFree( LPTAGINFO pTag )
{
   if( zh_nsxTagHeaderCheck( pTag ) )
   {
      if( pTag->RootBlock )
      {
         if( ! zh_nsxTagPagesFree( pTag, pTag->RootBlock ) )
            return ZH_FAILURE;
      }
      zh_nsxPageAddFree( pTag, pTag->HeadBlock );
      zh_nsxIndexTagDel( pTag->pIndex, pTag->TagName );
      pTag->pIndex->Changed = ZH_TRUE;
   }
   zh_nsxTagDelete( pTag );
   return ZH_SUCCESS;
}

/*
 * create index file name
 */
static void zh_nsxCreateFName( NSXAREAP pArea, const char * szBagName, ZH_BOOL * fProd,
                               char * szFileName, char * szTagName )
{
   PZH_FNAME pFileName;
   PZH_ITEM pExt = NULL;
   ZH_BOOL fName = szBagName && *szBagName;

   pFileName = zh_fsFNameSplit( fName ? szBagName : pArea->dbfarea.szDataFileName );

   if( szTagName )
   {
      if( pFileName->szName )
         zh_strncpyUpperTrim( szTagName, pFileName->szName, NSX_TAGNAME );
      else
         szTagName[ 0 ] = '\0';
   }

   if( ! fName || ( ! pFileName->szExtension && zh_setGetDefExtension() ) )
   {
      DBORDERINFO pExtInfo;
      memset( &pExtInfo, 0, sizeof( pExtInfo ) );
      pExt = pExtInfo.itmResult = zh_itemPutC( NULL, NULL );
      if( SELF_ORDINFO( &pArea->dbfarea.area, DBOI_BAGEXT, &pExtInfo ) == ZH_SUCCESS &&
          zh_itemGetCLen( pExt ) > 0 )
      {
         pFileName->szExtension = zh_itemGetCPtr( pExt );
      }
   }
   zh_fsFNameMerge( szFileName, pFileName );

   if( fProd )
   {
      if( ! pFileName->szName )
         *fProd = ZH_FALSE;
      else if( ! fName )
         *fProd = ZH_TRUE;
      else
      {
         PZH_FNAME pTableFileName = zh_fsFNameSplit( pArea->dbfarea.szDataFileName );

         *fProd = pTableFileName->szName &&
                  zh_stricmp( pTableFileName->szName, pFileName->szName ) == 0;
         if( *fProd && pFileName->szExtension && ! pExt )
         {
            DBORDERINFO pExtInfo;
            memset( &pExtInfo, 0, sizeof( pExtInfo ) );
            pExt = pExtInfo.itmResult = zh_itemPutC( NULL, NULL );
            if( SELF_ORDINFO( &pArea->dbfarea.area, DBOI_BAGEXT, &pExtInfo ) == ZH_SUCCESS )
            {
               *fProd = zh_stricmp( pFileName->szExtension,
                                    zh_itemGetCPtr( pExt ) ) == 0;
            }
         }
         zh_xfree( pTableFileName );
      }
   }
   zh_xfree( pFileName );
   if( pExt )
      zh_itemRelease( pExt );
}

/*
 * find order bag by its name
 */
static LPNSXINDEX zh_nsxFindBag( NSXAREAP pArea, const char * szBagName )
{
   LPNSXINDEX pIndex;
   PZH_FNAME pSeek;

   pSeek = zh_fsFNameSplit( szBagName );
   if( ! pSeek->szName )
      pSeek->szName = "";

   pIndex = pArea->lpIndexes;
   while( pIndex )
   {
      ZH_BOOL fFound;
      PZH_FNAME pName = zh_fsFNameSplit( pIndex->IndexName );
      if( ! pName->szName )
         pName->szName = "";
      fFound = ! zh_stricmp( pName->szName, pSeek->szName ) &&
               ( ! pSeek->szPath || ( pName->szPath &&
                                      ! zh_stricmp( pName->szPath, pSeek->szPath ) ) ) &&
               ( ! pSeek->szExtension || ( pName->szExtension &&
                                           ! zh_stricmp( pName->szExtension, pSeek->szExtension ) ) );
      zh_xfree( pName );
      if( fFound )
         break;
      pIndex = pIndex->pNext;
   }
   zh_xfree( pSeek );
   return pIndex;
}

/*
 * Find tag by name in index bag
 */
static int zh_nsxFindTagByName( LPNSXINDEX pIndex, const char * szTag )
{
   int i;

   for( i = 0; i < pIndex->iTags; i++ )
   {
      if( ! zh_strnicmp( pIndex->lpTags[ i ]->TagName, szTag,
                         NSX_TAGNAME ) )
         return i + 1;
   }
   return 0;
}

/*
 * Find the tag by its name or number
 */
static LPTAGINFO zh_nsxFindTag( NSXAREAP pArea, PZH_ITEM pTagItem,
                                PZH_ITEM pBagItem )
{
   LPNSXINDEX pIndex;
   ZH_BOOL fBag;

   if( ! pTagItem ||
       ( zh_itemType( pTagItem ) & ( ZH_IT_STRING | ZH_IT_NUMERIC ) ) == 0 )
      return pArea->lpCurTag;

   fBag = ZH_IS_STRING( pTagItem ) && zh_itemGetCLen( pBagItem ) > 0;
   if( fBag )
   {
      pIndex = zh_nsxFindBag( pArea, zh_itemGetCPtr( pBagItem ) );
   }
   else
   {
      int iBag = zh_itemGetNI( pBagItem );

      pIndex = pArea->lpIndexes;
      if( iBag > 0 )
      {
         fBag = ZH_TRUE;
         while( pIndex )
         {
            if( --iBag == 0 )
               break;
            pIndex = pIndex->pNext;
         }
      }
      else if( iBag < 0 )
      {
         pIndex = NULL;
      }
   }
   if( pIndex )
   {
      if( zh_itemType( pTagItem ) & ZH_IT_STRING )
      {
         const char * szTag = zh_itemGetCPtr( pTagItem );
         int iTag;

         if( fBag )
            iTag = zh_nsxFindTagByName( pIndex, szTag );
         else
         {
            do
            {
               iTag = zh_nsxFindTagByName( pIndex, szTag );
               if( iTag )
                  break;
               pIndex = pIndex->pNext;
            }
            while( pIndex );
         }
         if( iTag )
            return pIndex->lpTags[ iTag - 1 ];
      }
      else
      {
         int i = zh_itemGetNI( pTagItem ) - 1;

         if( i >= 0 )
         {
            if( fBag )
            {
               if( i < pIndex->iTags )
                  return pIndex->lpTags[ i ];
            }
            else
            {
               do
               {
                  if( i < pIndex->iTags )
                     return pIndex->lpTags[ i ];
                  i -= pIndex->iTags;
                  pIndex = pIndex->pNext;
               }
               while( pIndex );
            }
         }
      }
   }

   return NULL;
}

/*
 * find the given tag number
 */
static int zh_nsxFindTagNum( NSXAREAP pArea, LPTAGINFO pTag )
{
   if( pArea->fSetTagNumbers )
   {
      LPNSXINDEX pIndex = pArea->lpIndexes;
      ZH_USHORT uiNum = 0, i;

      pTag->uiNumber = 0;
      while( pIndex )
      {
         for( i = 0; i < pIndex->iTags; i++ )
         {
            pIndex->lpTags[ i ]->uiNumber = ++uiNum;
         }
         pIndex = pIndex->pNext;
      }
      pArea->fSetTagNumbers = ZH_FALSE;
   }
   return pTag->uiNumber;
}

/*
 * count number of tags
 */
static int zh_nsxTagCount( NSXAREAP pArea )
{
   LPNSXINDEX pIndex = pArea->lpIndexes;
   int i = 0;

   while( pIndex )
   {
      i += pIndex->iTags;
      pIndex = pIndex->pNext;
   }

   return i;
}

/*
 * count number of keys in given tag
 */
static ZH_ULONG zh_nsxOrdKeyCount( LPTAGINFO pTag )
{
   ZH_ULONG ulKeyCount = 0;

   if( ! pTag->pIndex->fShared && pTag->keyCount &&
       ! pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter )
      return pTag->keyCount;

   if( zh_nsxTagLockRead( pTag ) )
   {
      zh_nsxTagRefreshScope( pTag );

      if( pTag->top.scopeKeyLen || pTag->bottom.scopeKeyLen ||
          pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter )
      {
         zh_nsxTagGoTop( pTag );
         while( ! pTag->TagEOF )
         {
            ulKeyCount++;
            zh_nsxTagSkipNext( pTag );
         }
      }
      else
      {
         ulKeyCount = zh_nsxPageCountKeys( pTag, pTag->RootBlock );
      }
      if( ! pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter )
         pTag->keyCount = ulKeyCount;
      zh_nsxTagUnLockRead( pTag );
   }

   return ulKeyCount;
}

/*
 * get the logical key position in the given tag
 */
static ZH_ULONG zh_nsxOrdKeyNo( LPTAGINFO pTag )
{
   ZH_ULONG ulKeyNo = 0;

   if( zh_nsxTagLockRead( pTag ) )
   {
      zh_nsxTagRefreshScope( pTag );
      if( zh_nsxCurKeyRefresh( pTag ) )
      {
         if( pTag->top.scopeKeyLen || pTag->bottom.scopeKeyLen ||
             pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter )
         {
            if( zh_nsxKeyInScope( pTag, pTag->CurKeyInfo ) )
            {
               do
               {
                  ulKeyNo++;
                  zh_nsxTagSkipPrev( pTag );
               }
               while( ! pTag->TagBOF );
            }
         }
         else
         {
            int iLevel = pTag->stackLevel;
            ZH_BOOL fBack = pTag->fUsrDescend, fFirst = ZH_TRUE;

            while( --iLevel >= 0 )
            {
               LPPAGEINFO pPage = zh_nsxPageLoad( pTag, pTag->stack[ iLevel ].page );
               int iKey;
               if( ! pPage )
                  break;
               iKey = pTag->stack[ iLevel ].ikey;
               if( fBack )
               {
                  if( zh_nsxIsLeaf( pPage ) )
                     ulKeyNo += pPage->uiKeys - iKey;
                  else
                  {
                     if( fFirst && iKey )
                        --iKey;
                     ulKeyNo += pPage->uiKeys - iKey;
                     while( ++iKey <= pPage->uiKeys )
                     {
                        ulKeyNo += zh_nsxPageCountKeys( pTag,
                           zh_nsxGetKeyPage( pPage, pTag->KeyLength, iKey - 1 ) );
                     }
                  }
                  fFirst = ZH_FALSE;
               }
               else
               {
                  if( zh_nsxIsLeaf( pPage ) )
                     ulKeyNo += iKey + 1;
                  else
                  {
                     ulKeyNo += iKey;
                     while( --iKey >= 0 )
                     {
                        ulKeyNo += zh_nsxPageCountKeys( pTag,
                           iKey == 0 ? zh_nsxGetLowerPage( pPage ) :
                           zh_nsxGetKeyPage( pPage, pTag->KeyLength, iKey - 1 ) );
                     }
                  }
               }
               zh_nsxPageRelease( pTag, pPage );
            }
         }
      }
      zh_nsxTagUnLockRead( pTag );
   }
   return ulKeyNo;
}

/*
 * set logical key position in given tag
 */
static ZH_BOOL zh_nsxOrdKeyGoto( LPTAGINFO pTag, ZH_ULONG ulKeyNo )
{
   NSXAREAP pArea = pTag->pIndex->pArea;

   if( ! ulKeyNo || ! zh_nsxTagLockRead( pTag ) )
      return ZH_FALSE;

   zh_nsxTagRefreshScope( pTag );
   zh_nsxTagGoTop( pTag );
   if( pTag->TagEOF || pTag->top.scopeKeyLen || pTag->bottom.scopeKeyLen ||
       pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter )
   {
      while( ! pTag->TagEOF && --ulKeyNo )
         zh_nsxTagSkipNext( pTag );
   }
   else
   {
      int iLevel = pTag->stackLevel - 1;
      ZH_BOOL fBack = pTag->fUsrDescend, fFirst = ZH_TRUE;
      LPPAGEINFO pPage;
      ZH_ULONG ulPage;

      --ulKeyNo;
      while( ulKeyNo != 0 && iLevel >= 0 )
      {
         int iKey = pTag->stack[ iLevel ].ikey;
         pPage = zh_nsxPageLoad( pTag, pTag->stack[ iLevel ].page );
         if( ! pPage )
            break;
         if( fBack )
         {
            if( zh_nsxIsLeaf( pPage ) )
            {
               if( ( ZH_ULONG ) iKey < ulKeyNo )
               {
                  --iLevel;
                  ulKeyNo -= iKey;
               }
               else
               {
                  pTag->stack[ iLevel ].ikey -= ( ZH_SHORT ) ulKeyNo;
                  ulKeyNo = 0;
               }
            }
            else
            {
               if( --iKey < 0 )
                  --iLevel;
               else if( fFirst || --ulKeyNo )
               {
                  pTag->stackLevel = ( ZH_USHORTCAST ) iLevel;
                  zh_nsxTagSetPageStack( pTag, pPage, ( ZH_USHORTCAST ) iKey );
                  ulPage = iKey == 0 ? zh_nsxGetLowerPage( pPage ) :
                           zh_nsxGetKeyPage( pPage, pTag->KeyLength, iKey - 1 );
                  zh_nsxPageRelease( pTag, pPage );
                  pPage = zh_nsxPageBottomMove( pTag, ulPage );
                  if( ! pPage )
                     break;
                  iLevel = pTag->stackLevel - 1;
                  --ulKeyNo;
               }
            }
            fFirst = ZH_FALSE;
         }
         else
         {
            if( ! zh_nsxIsLeaf( pPage ) )
            {
               if( iKey >= pPage->uiKeys )
                  --iLevel;
               else
               {
                  pTag->stackLevel = ( ZH_USHORTCAST ) iLevel;
                  zh_nsxTagSetPageStack( pTag, pPage, ( ZH_USHORTCAST ) ( iKey + 1 ) );
                  if( --ulKeyNo )
                  {
                     ulPage = zh_nsxGetKeyPage( pPage, pTag->KeyLength, iKey );
                     zh_nsxPageRelease( pTag, pPage );
                     pPage = zh_nsxPageTopMove( pTag, ulPage );
                     if( ! pPage )
                        break;
                     iLevel = pTag->stackLevel - 1;
                     --ulKeyNo;
                  }
               }
            }
            else
            {
               iKey = pPage->uiKeys - iKey - 1;
               if( ( ZH_ULONG ) iKey < ulKeyNo )
               {
                  --iLevel;
                  ulKeyNo -= iKey;
               }
               else
               {
                  pTag->stack[ iLevel ].ikey += ( ZH_SHORT ) ulKeyNo;
                  ulKeyNo = 0;
               }
            }
         }
         pTag->stackLevel = ( ZH_USHORTCAST ) ( iLevel + 1 );
         if( ulKeyNo == 0 )
         {
            if( ! zh_nsxTagGetCurKey( pTag, pPage, pTag->stack[ iLevel ].ikey ) )
               pTag->TagEOF = ZH_TRUE;
         }
         zh_nsxPageRelease( pTag, pPage );
      }
   }

   if( ulKeyNo != 0 || pTag->TagEOF )
   {
      pTag->stackLevel = 0;
      SELF_GOTO( &pArea->dbfarea.area, 0 );
   }
   else
   {
      LPTAGINFO pSavedTag = pArea->lpCurTag;
      pArea->lpCurTag = pTag;
      if( SELF_GOTO( &pArea->dbfarea.area, pTag->CurKeyInfo->rec ) == ZH_SUCCESS )
         SELF_SKIPFILTER( &pArea->dbfarea.area, 1 );
      pArea->lpCurTag = pSavedTag;
   }
   zh_nsxTagUnLockRead( pTag );
   return ZH_TRUE;
}

/*
 * get the relative key position (from 0.0 to 1.0) in the given tag
 */
static double zh_nsxOrdGetRelKeyPos( LPTAGINFO pTag )
{
   double dPos = 0.0, dStart = 0.0, dStop = 1.0, dFact = 0.0000000000001;
   ZH_BOOL fOK = ZH_TRUE, fFilter = pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter;

   if( ! zh_nsxTagLockRead( pTag ) )
      return ZH_FALSE;

   zh_nsxTagRefreshScope( pTag );

   pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter = ZH_FALSE;
   if( pTag->fUsrDescend ? pTag->bottom.scopeKeyLen : pTag->top.scopeKeyLen )
   {
      zh_nsxTagGoTop( pTag );
      if( pTag->TagEOF )
         fOK = ZH_FALSE;
      else
         dStart = zh_nsxTagCountRelKeyPos( pTag );
   }
   if( fOK && ( pTag->fUsrDescend ? pTag->top.scopeKeyLen : pTag->bottom.scopeKeyLen ) )
   {
      zh_nsxTagGoBottom( pTag );
      if( pTag->TagBOF )
         fOK = ZH_FALSE;
      else
         dStop = zh_nsxTagCountRelKeyPos( pTag );
   }
   pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter = fFilter;

   if( fOK )
   {
      if( zh_nsxCurKeyRefresh( pTag ) &&
          zh_nsxKeyInScope( pTag, pTag->CurKeyInfo ) )
      {
         if( dStart >= dStop - dFact )
            dPos = 0.5;
         else
         {
            dPos = zh_nsxTagCountRelKeyPos( pTag );
            dPos = ( dPos - dStart ) / ( dStop - dStart );
            /* fix possible differences in FL representation */
            if( dPos <= 0.0 )
               dPos = 0.0;
            else if( dPos >= 1.0 )
               dPos = 1.0;
         }
      }
   }
   zh_nsxTagUnLockRead( pTag );

   return dPos;
}

/*
 * set the relative key position (from 0.0 to 1.0) in the given tag
 */
static void zh_nsxOrdSetRelKeyPos( LPTAGINFO pTag, double dPos )
{
   if( zh_nsxTagLockRead( pTag ) )
   {
      NSXAREAP pArea = pTag->pIndex->pArea;
      double dStart = 0.0, dStop = 1.0, dFact = 0.0000000000001;
      ZH_BOOL fOK = ZH_TRUE, fFilter = pArea->dbfarea.area.dbfi.fFilter;
      ZH_BOOL fForward = ZH_TRUE, fTop = ZH_FALSE;

      zh_nsxTagRefreshScope( pTag );

      if( dPos >= 1.0 )
         fForward = ZH_FALSE;
      else if( dPos <= 0.0 )
         fTop = ZH_TRUE;
      else
      {
         pArea->dbfarea.area.dbfi.fFilter = ZH_FALSE;
         if( pTag->fUsrDescend ? pTag->bottom.scopeKeyLen : pTag->top.scopeKeyLen )
         {
            zh_nsxTagGoTop( pTag );
            if( pTag->TagEOF )
               fOK = ZH_FALSE;
            else
               dStart = zh_nsxTagCountRelKeyPos( pTag );
         }
         if( fOK && ( pTag->fUsrDescend ? pTag->top.scopeKeyLen : pTag->bottom.scopeKeyLen ) )
         {
            zh_nsxTagGoBottom( pTag );
            if( pTag->TagBOF )
               fOK = ZH_FALSE;
            else
               dStop = zh_nsxTagCountRelKeyPos( pTag );
         }
         pArea->dbfarea.area.dbfi.fFilter = fFilter;

         if( fOK )
         {
            if( dStart >= dStop - dFact )
            {
               fTop = ZH_TRUE;
            }
            else
            {
               dPos = dPos * ( dStop - dStart ) + dStart;
               zh_nsxTagGoToRelKeyPos( pTag, dPos );
               if( pTag->CurKeyInfo->rec == 0 )
                  fForward = ZH_FALSE;
               else if( ! zh_nsxInTopScope( pTag, pTag->CurKeyInfo->val ) )
                  fTop = ZH_TRUE;
               else if( ! zh_nsxInBottomScope( pTag, pTag->CurKeyInfo->val ) )
                  fForward = ZH_FALSE;
            }
         }
      }
      if( ! fOK )
      {
         SELF_GOTO( &pArea->dbfarea.area, 0 );
      }
      else
      {
         LPTAGINFO pSavedTag = pArea->lpCurTag;
         pArea->lpCurTag = pTag;

         pArea->dbfarea.area.fTop = pArea->dbfarea.area.fBottom = ZH_FALSE;

         if( fForward )
         {
            if( fTop )
               zh_nsxTagGoTop( pTag );
            if( pTag->CurKeyInfo->rec != 0 )
            {
               if( SELF_GOTO( &pArea->dbfarea.area, pTag->CurKeyInfo->rec ) == ZH_SUCCESS )
               {
                  SELF_SKIPFILTER( &pArea->dbfarea.area, 1 );
                  if( pArea->dbfarea.area.fEof && ! fTop )
                     fForward = ZH_FALSE;
               }
            }
            else if( fTop )
               SELF_GOTO( &pArea->dbfarea.area, 0 );
            else
               fForward = ZH_FALSE;
         }
         if( ! fForward )
         {
            zh_nsxTagGoBottom( pTag );
            if( SELF_GOTO( &pArea->dbfarea.area, pTag->CurKeyInfo->rec ) == ZH_SUCCESS &&
                pTag->CurKeyInfo->rec != 0 )
            {
               pArea->dbfarea.area.fBottom = ZH_TRUE;
               SELF_SKIPFILTER( &pArea->dbfarea.area, -1 );
            }
         }
         pArea->lpCurTag = pSavedTag;
      }
      zh_nsxTagUnLockRead( pTag );
   }
}

/*
 * skip to next/previous unique key
 */
static ZH_BOOL zh_nsxOrdSkipUnique( LPTAGINFO pTag, ZH_LONG lToSkip )
{
   NSXAREAP pArea = pTag->pIndex->pArea;

   if( pArea->dbfarea.lpdbPendingRel )
      SELF_FORCEREL( &pArea->dbfarea.area );

   pArea->dbfarea.area.fTop = pArea->dbfarea.area.fBottom = ZH_FALSE;

   if( zh_nsxTagLockRead( pTag ) )
   {
      ZH_BOOL fOut = ZH_FALSE, fEof = ZH_FALSE, fForward = ( lToSkip >= 0 );

      LPTAGINFO pSavedTag = pArea->lpCurTag;
      pArea->lpCurTag = pTag;

      zh_nsxTagRefreshScope( pTag );
      if( zh_nsxCurKeyRefresh( pTag ) )
      {
         ZH_UCHAR keyVal[ NSX_MAXKEYLEN ];
         memcpy( keyVal, pTag->CurKeyInfo->val, pTag->KeyLength );

         do
         {
            if( fForward )
               zh_nsxTagSkipNext( pTag );
            else
               zh_nsxTagSkipPrev( pTag );
            fOut = pTag->TagEOF || pTag->TagBOF;
         }
         while( ! fOut && zh_nsxValCompare( pTag,
                                            pTag->CurKeyInfo->val, pTag->KeyLength,
                                            keyVal, pTag->KeyLength,
                                            NSX_CMP_EXACT ) == 0 );
      }
      else if( ! fForward && ! pArea->dbfarea.fPositioned )
      {
         zh_nsxTagGoBottom( pTag );
         fEof = pTag->TagEOF;
      }
      else
      {
         fOut = ZH_TRUE;
      }
      if( fOut )
      {
         if( fForward )
            fEof = ZH_TRUE;
         else
         {
            zh_nsxTagGoTop( pTag );
            fEof = pTag->TagEOF;
         }
      }
      zh_nsxTagUnLockRead( pTag );

      if( SELF_GOTO( &pArea->dbfarea.area, fEof ? 0 : pTag->CurKeyInfo->rec ) == ZH_SUCCESS &&
          ! fEof )
      {
         SELF_SKIPFILTER( &pArea->dbfarea.area, ( fForward || fOut ) ? 1 : -1 );
         if( ! fForward && fOut )
            pArea->dbfarea.area.fBof = ZH_TRUE;
      }

      /* Update Bof and Eof flags */
      if( fForward )
         pArea->dbfarea.area.fBof = ZH_FALSE;
      else
         pArea->dbfarea.area.fEof = ZH_FALSE;

      pArea->lpCurTag = pSavedTag;
      return ZH_TRUE;
   }
   return ZH_FALSE;
}

/*
 * skip while code block doesn't return ZH_TRUE
 */
static ZH_BOOL zh_nsxOrdSkipEval( LPTAGINFO pTag, ZH_BOOL fForward, PZH_ITEM pEval )
{
   NSXAREAP pArea = pTag->pIndex->pArea;
   ZH_BOOL fFound = ZH_FALSE;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_nsxOrdSkipEval(%p, %d, %p)", ( void * ) pTag, fForward, ( void * ) pEval ) );

   if( ( zh_itemType( pEval ) & ZH_IT_BLOCK ) == 0 )
   {
      if( SELF_SKIP( &pArea->dbfarea.area, fForward ? 1 : -1 ) != ZH_SUCCESS )
         return ZH_FALSE;
      return fForward ? ! pArea->dbfarea.area.fEof : ! pArea->dbfarea.area.fBof;
   }

   if( pArea->dbfarea.lpdbPendingRel )
      SELF_FORCEREL( &pArea->dbfarea.area );

   pArea->dbfarea.area.fTop = pArea->dbfarea.area.fBottom = ZH_FALSE;

   if( zh_nsxTagLockRead( pTag ) )
   {
      LPTAGINFO pSavedTag = pArea->lpCurTag;
      pArea->lpCurTag = pTag;

      zh_nsxTagRefreshScope( pTag );
      if( zh_nsxCurKeyRefresh( pTag ) )
      {
         if( fForward )
            zh_nsxTagSkipNext( pTag );
         else
            zh_nsxTagSkipPrev( pTag );

         while( fForward ? ! pTag->TagEOF : ! pTag->TagBOF )
         {
            if( SELF_GOTO( &pArea->dbfarea.area, pTag->CurKeyInfo->rec ) != ZH_SUCCESS )
               break;
            if( zh_nsxEvalSeekCond( pTag, pEval ) )
            {
               ZH_ULONG ulRecNo = pArea->dbfarea.ulRecNo;
               if( SELF_SKIPFILTER( &pArea->dbfarea.area, fForward ? 1 : -1 ) != ZH_SUCCESS ||
                   pArea->dbfarea.ulRecNo == ulRecNo || zh_nsxEvalSeekCond( pTag, pEval ) )
               {
                  fFound = ZH_TRUE;
                  break;
               }
            }
            if( fForward )
               zh_nsxTagSkipNext( pTag );
            else
               zh_nsxTagSkipPrev( pTag );
         }
         if( ! fFound )
         {
            if( fForward )
               SELF_GOTO( &pArea->dbfarea.area, 0 );
            else
            {
               SELF_GOTOP( &pArea->dbfarea.area );
               pArea->dbfarea.area.fBof = ZH_TRUE;
            }
         }
      }
      pArea->lpCurTag = pSavedTag;
      zh_nsxTagUnLockRead( pTag );
   }

   /* Update Bof and Eof flags */
   if( fForward )
      pArea->dbfarea.area.fBof = ZH_FALSE;
   else
      pArea->dbfarea.area.fEof = ZH_FALSE;

   return fFound;
}

/*
 * skip while code block doesn't return ZH_TRUE
 */
static ZH_BOOL zh_nsxOrdSkipWild( LPTAGINFO pTag, ZH_BOOL fForward, PZH_ITEM pWildItm )
{
   NSXAREAP pArea = pTag->pIndex->pArea;
   const char *szPattern;
   char *szFree = NULL;
   ZH_BOOL fFound = ZH_FALSE;
   int iFixed = 0;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_nsxOrdSkipWild(%p, %d, %p)", ( void * ) pTag, fForward, ( void * ) pWildItm ) );

   szPattern = zh_itemGetCPtr( pWildItm );

   if( pTag->KeyType != 'C' || ! szPattern || ! *szPattern )
   {
      if( SELF_SKIP( &pArea->dbfarea.area, fForward ? 1 : -1 ) != ZH_SUCCESS )
         return ZH_FALSE;
      return fForward ? ! pArea->dbfarea.area.fEof : ! pArea->dbfarea.area.fBof;
   }

   if( pArea->dbfarea.area.cdPage != zh_vmCDP() )
   {
      szPattern = szFree = zh_cdpDup( szPattern, zh_vmCDP(), pArea->dbfarea.area.cdPage );
   }
   while( iFixed < pTag->KeyLength && szPattern[ iFixed ] &&
          szPattern[ iFixed ] != '*' && szPattern[ iFixed ] != '?' )
   {
      ++iFixed;
   }

   if( pArea->dbfarea.lpdbPendingRel )
      SELF_FORCEREL( &pArea->dbfarea.area );

   pArea->dbfarea.area.fTop = pArea->dbfarea.area.fBottom = ZH_FALSE;

   if( zh_nsxTagLockRead( pTag ) )
   {
      LPTAGINFO pSavedTag = pArea->lpCurTag;
      pArea->lpCurTag = pTag;

      zh_nsxTagRefreshScope( pTag );
      if( zh_nsxCurKeyRefresh( pTag ) )
      {
         int iStop = fForward ? -1 : 1;
         if( pTag->fUsrDescend )
            iStop = -iStop;
         if( iFixed && zh_nsxValCompare( pTag,
                                         ( const ZH_UCHAR * ) szPattern, iFixed,
                                         pTag->CurKeyInfo->val, iFixed,
                                         NSX_CMP_PREFIX ) == -iStop )
         {
            LPKEYINFO pKey;
            pKey = zh_nsxKeyNew( pTag->KeyLength );
            memcpy( pKey->val, szPattern, iFixed );
            pKey->val[ iFixed ] = '\0';
            pKey->rec = pArea->lpCurTag->fUsrDescend ? NSX_MAX_REC_NUM :
                                                       NSX_IGNORE_REC_NUM;
            pKey->mode = NSX_CMP_PREFIX;
            if( ! zh_nsxTagKeyFind( pTag, pKey, ( ZH_USHORTCAST ) iFixed ) )
            {
               if( fForward )
                  pTag->TagEOF = ZH_TRUE;
               else
                  pTag->TagBOF = ZH_TRUE;
            }
            zh_nsxKeyFree( pKey );
         }
         else if( fForward )
            zh_nsxTagSkipNext( pTag );
         else
            zh_nsxTagSkipPrev( pTag );

         while( fForward ? ! pTag->TagEOF : ! pTag->TagBOF )
         {
            if( zh_strMatchWild( ( const char * ) pTag->CurKeyInfo->val, szPattern ) )
            {
               ZH_ULONG ulRecNo = pTag->CurKeyInfo->rec;
               if( SELF_GOTO( &pArea->dbfarea.area, ulRecNo ) != ZH_SUCCESS )
                  break;
               if( SELF_SKIPFILTER( &pArea->dbfarea.area, fForward ? 1 : -1 ) != ZH_SUCCESS ||
                   pArea->dbfarea.ulRecNo == ulRecNo ||
                   zh_strMatchWild( ( const char * ) pTag->CurKeyInfo->val, szPattern ) )
               {
                  fFound = ZH_TRUE;
                  break;
               }
            }
            if( iFixed && zh_nsxValCompare( pTag, ( const ZH_UCHAR * ) szPattern, iFixed,
                             pTag->CurKeyInfo->val, iFixed, NSX_CMP_PREFIX ) == iStop )
            {
               break;
            }
            if( fForward )
               zh_nsxTagSkipNext( pTag );
            else
               zh_nsxTagSkipPrev( pTag );
         }
         if( ! fFound )
         {
            if( fForward )
               SELF_GOTO( &pArea->dbfarea.area, 0 );
            else
            {
               SELF_GOTOP( &pArea->dbfarea.area );
               pArea->dbfarea.area.fBof = ZH_TRUE;
            }
         }
      }
      pArea->lpCurTag = pSavedTag;
      zh_nsxTagUnLockRead( pTag );
   }

   /* Update Bof and Eof flags */
   if( fForward )
      pArea->dbfarea.area.fBof = ZH_FALSE;
   else
      pArea->dbfarea.area.fEof = ZH_FALSE;

   if( szFree )
      zh_xfree( szFree );

   return fFound;
}

static ZH_BOOL zh_nsxRegexMatch( LPTAGINFO pTag, PZH_REGEX pRegEx, const char * szKey )
{
   ZH_SIZE nLen = pTag->KeyLength;
   char szBuff[ NSX_MAXKEYLEN + 1 ];

   if( pTag->pIndex->pArea->dbfarea.area.cdPage != zh_vmCDP() )
   {
      nLen = sizeof( szBuff ) - 1;
      zh_cdpnDup2( szKey, pTag->KeyLength, szBuff, &nLen,
                   pTag->pIndex->pArea->dbfarea.area.cdPage, zh_vmCDP() );
      szBuff[ nLen ] = '\0';
      szKey = szBuff;
   }
   return zh_regexMatch( pRegEx, szKey, nLen, ZH_FALSE );
}

/*
 * skip while regular expression on index key val doesn't return ZH_TRUE
 */
static ZH_BOOL zh_nsxOrdSkipRegEx( LPTAGINFO pTag, ZH_BOOL fForward, PZH_ITEM pRegExItm )
{
   NSXAREAP pArea = pTag->pIndex->pArea;
   ZH_BOOL fFound = ZH_FALSE;
   PZH_REGEX pRegEx;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_nsxOrdSkipRegEx(%p, %d, %p)", ( void * ) pTag, fForward, ( void * ) pRegExItm ) );

   if( pTag->KeyType != 'C' || ( pRegEx = zh_regexGet( pRegExItm, 0 ) ) == NULL )
   {
      if( SELF_SKIP( &pArea->dbfarea.area, fForward ? 1 : -1 ) != ZH_SUCCESS )
         return ZH_FALSE;
      return fForward ? ! pArea->dbfarea.area.fEof : ! pArea->dbfarea.area.fBof;
   }

   if( pArea->dbfarea.lpdbPendingRel )
      SELF_FORCEREL( &pArea->dbfarea.area );

   pArea->dbfarea.area.fTop = pArea->dbfarea.area.fBottom = ZH_FALSE;

   if( zh_nsxTagLockRead( pTag ) )
   {
      LPTAGINFO pSavedTag = pArea->lpCurTag;
      pArea->lpCurTag = pTag;

      zh_nsxTagRefreshScope( pTag );
      if( zh_nsxCurKeyRefresh( pTag ) )
      {
         if( fForward )
            zh_nsxTagSkipNext( pTag );
         else
            zh_nsxTagSkipPrev( pTag );

         while( fForward ? ! pTag->TagEOF : ! pTag->TagBOF )
         {
            if( SELF_GOTO( &pArea->dbfarea.area, pTag->CurKeyInfo->rec ) != ZH_SUCCESS )
               break;

            if( zh_nsxRegexMatch( pTag, pRegEx, ( const char * ) pTag->CurKeyInfo->val ) )
            {
               ZH_ULONG ulRecNo = pArea->dbfarea.ulRecNo;
               if( SELF_SKIPFILTER( &pArea->dbfarea.area, fForward ? 1 : -1 ) != ZH_SUCCESS ||
                   pArea->dbfarea.ulRecNo == ulRecNo ||
                   zh_nsxRegexMatch( pTag, pRegEx, ( const char * ) pTag->CurKeyInfo->val ) )
               {
                  fFound = ZH_TRUE;
                  break;
               }
            }
            if( fForward )
               zh_nsxTagSkipNext( pTag );
            else
               zh_nsxTagSkipPrev( pTag );
         }
         if( ! fFound )
         {
            if( fForward )
               SELF_GOTO( &pArea->dbfarea.area, 0 );
            else
            {
               SELF_GOTOP( &pArea->dbfarea.area );
               pArea->dbfarea.area.fBof = ZH_TRUE;
            }
         }
      }
      pArea->lpCurTag = pSavedTag;
      zh_nsxTagUnLockRead( pTag );
   }

   /* Update Bof and Eof flags */
   if( fForward )
      pArea->dbfarea.area.fBof = ZH_FALSE;
   else
      pArea->dbfarea.area.fEof = ZH_FALSE;

   zh_regexFree( pRegEx );

   return fFound;
}

/*
 * add key to custom tag (ordKeyAdd())
 * user key value is not implemented
 */
static ZH_BOOL zh_nsxOrdKeyAdd( LPTAGINFO pTag, PZH_ITEM pItem )
{
   NSXAREAP pArea = pTag->pIndex->pArea;
   ZH_BOOL fResult = ZH_FALSE;
   LPKEYINFO pKey;

   if( pArea->dbfarea.lpdbPendingRel )
      SELF_FORCEREL( &pArea->dbfarea.area );

   if( ! pArea->dbfarea.fPositioned )
      return ZH_FALSE;

   if( pTag->pForItem && ! zh_nsxEvalCond( pArea, pTag->pForItem, ZH_TRUE ) )
      return ZH_FALSE;

   if( pTag->Template && pItem && zh_itemType( pItem ) != ZH_IT_NIL )
   {
      pKey = zh_nsxKeyPutItem( NULL, pItem, pArea->dbfarea.ulRecNo, pTag, ZH_TRUE, NULL );
   }
   else
   {
      pKey = zh_nsxEvalKey( NULL, pTag );
   }

   if( zh_nsxTagLockWrite( pTag ) )
   {
      if( zh_nsxTagKeyAdd( pTag, pKey ) )
      {
         fResult = ZH_TRUE;
         if( ! pTag->pIndex->fShared && pTag->keyCount &&
             zh_nsxKeyInScope( pTag, pKey ) )
            pTag->keyCount++;
      }
      zh_nsxTagUnLockWrite( pTag );
   }
   zh_nsxKeyFree( pKey );
   return fResult;
}

/*
 * del key from custom tag (ordKeyDel())
 * user key value is not implemented
 */
static ZH_BOOL zh_nsxOrdKeyDel( LPTAGINFO pTag, PZH_ITEM pItem )
{
   NSXAREAP pArea = pTag->pIndex->pArea;
   ZH_BOOL fResult = ZH_FALSE;
   LPKEYINFO pKey = NULL;

   if( pArea->dbfarea.lpdbPendingRel )
      SELF_FORCEREL( &pArea->dbfarea.area );

   if( ! pArea->dbfarea.fPositioned )
      return ZH_FALSE;

   if( pTag->pForItem && ! zh_nsxEvalCond( pArea, pTag->pForItem, ZH_TRUE ) )
      return ZH_FALSE;

   if( pTag->Template && pItem && zh_itemType( pItem ) != ZH_IT_NIL )
   {
      pKey = zh_nsxKeyPutItem( NULL, pItem, pArea->dbfarea.ulRecNo, pTag, ZH_TRUE, NULL );
   }

   if( zh_nsxTagLockWrite( pTag ) )
   {
      if( pKey == NULL )
      {
         if( zh_nsxCurKeyRefresh( pTag ) )
            pKey = zh_nsxKeyCopy( NULL, pTag->CurKeyInfo, pTag->KeyLength );
         else
            pKey = zh_nsxEvalKey( NULL, pTag );
      }
      if( zh_nsxTagKeyDel( pTag, pKey ) )
      {
         fResult = ZH_TRUE;
         if( ! pTag->pIndex->fShared && pTag->keyCount &&
             zh_nsxKeyInScope( pTag, pKey ) )
            pTag->keyCount--;
      }
      zh_nsxTagUnLockWrite( pTag );
   }
   zh_nsxKeyFree( pKey );
   return fResult;
}

/*
 * DBOI_FINDREC find a specific record in the tag - it's useful for
 * custom indexes when the same record can be stored more then once
 * or when the used index key is unknown
 */
static ZH_BOOL zh_nsxOrdFindRec( LPTAGINFO pTag, ZH_ULONG ulRecNo, ZH_BOOL fCont )
{
   NSXAREAP pArea = pTag->pIndex->pArea;
   ZH_BOOL fFound = ZH_FALSE;

   if( ulRecNo )
   {
      if( pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped )
         SELF_FORCEREL( &pArea->dbfarea.area );

      if( zh_nsxTagLockRead( pTag ) )
      {
         zh_nsxTagRefreshScope( pTag );
         if( fCont )
         {
            if( ! zh_nsxCurKeyRefresh( pTag ) )
               ulRecNo = 0;
            else
               zh_nsxTagSkipNext( pTag );
         }
         else
         {
            zh_nsxTagGoTop( pTag );
         }
         if( ulRecNo )
         {
            while( ! pTag->TagEOF )
            {
               if( pTag->CurKeyInfo->rec == ulRecNo )
               {
                  fFound = ZH_TRUE;
                  break;
               }
               zh_nsxTagSkipNext( pTag );
            }
         }
         zh_nsxTagUnLockRead( pTag );
      }
   }
   SELF_GOTO( &pArea->dbfarea.area, fFound ? ulRecNo : 0 );
   return fFound;
}

/*
 * evaluate given C function in given scope
 */
static ZH_ULONG zh_nsxOrdScopeEval( LPTAGINFO pTag,
                                    ZH_EVALSCOPE_FUNC pFunc, void * pParam,
                                    PZH_ITEM pItemLo, PZH_ITEM pItemHi )
{
   ZH_ULONG ulCount = 0, ulLen = ( ZH_ULONG ) pTag->KeyLength;
   PZH_ITEM pItemTop = zh_itemNew( NULL ), pItemBottom = zh_itemNew( NULL );
   ZH_BOOL fDescend = pTag->fUsrDescend;

   if( fDescend )
   {
      PZH_ITEM pTemp = pItemLo;
      pItemLo = pItemHi;
      pItemHi = pTemp;
      pTag->fUsrDescend = ZH_FALSE;
   }
   zh_nsxTagGetScope( pTag, 0, pItemTop );
   zh_nsxTagGetScope( pTag, 1, pItemBottom );
   zh_nsxTagSetScope( pTag, 0, pItemLo );
   zh_nsxTagSetScope( pTag, 1, pItemHi );

   if( zh_nsxTagLockRead( pTag ) )
   {
      zh_nsxTagGoTop( pTag );
      while( ! pTag->TagEOF )
      {
         pFunc( pTag->CurKeyInfo->rec, ( ZH_BYTE * ) pTag->CurKeyInfo->val, ulLen, pParam );
         ulCount++;
         zh_nsxTagSkipNext( pTag );
      }
      zh_nsxTagUnLockRead( pTag );
   }

   zh_nsxTagSetScope( pTag, 0, pItemTop );
   zh_nsxTagSetScope( pTag, 1, pItemBottom );
   zh_itemRelease( pItemTop );
   zh_itemRelease( pItemBottom );

   pTag->fUsrDescend = fDescend;

   return ulCount;
}

/* ************************************************************************* */
/* create index: zh_nsxTagCreate() */
/* ************************************************************************* */

static int zh_nsxQuickSortCompare( LPNSXSORTINFO pSort, ZH_UCHAR * pKey1, ZH_UCHAR * pKey2 )
{
   int iLen = pSort->keyLen, i;

   i = zh_nsxValCompare( pSort->pTag, pKey1, iLen, pKey2, iLen, NSX_CMP_EXACT );
   if( i == 0 )
      i = ( ZH_GET_LE_UINT32( pKey1 + iLen ) < ZH_GET_LE_UINT32( pKey2 + iLen ) ) ? -1 : 1;

   return i;
}

static ZH_BOOL zh_nsxQSort( LPNSXSORTINFO pSort, ZH_UCHAR * pSrc, ZH_UCHAR * pBuf, ZH_LONG lKeys )
{
   if( lKeys > 1 )
   {
      int iLen = pSort->keyLen + 4;
      ZH_LONG l1, l2;
      ZH_UCHAR * pPtr1, * pPtr2, * pDst;
      ZH_BOOL f1, f2;

      l1 = lKeys >> 1;
      l2 = lKeys - l1;
      pPtr1 = &pSrc[ 0 ];
      pPtr2 = &pSrc[ l1 * iLen ];

      f1 = zh_nsxQSort( pSort, pPtr1, &pBuf[ 0 ], l1 );
      f2 = zh_nsxQSort( pSort, pPtr2, &pBuf[ l1 * iLen ], l2 );
      if( f1 )
      {
         pDst = pBuf;
      }
      else
      {
         pDst = pSrc;
         pPtr1 = &pBuf[ 0 ];
      }
      if( ! f2 )
      {
         pPtr2 = &pBuf[ l1 * iLen ];
      }
      while( l1 > 0 && l2 > 0 )
      {
         if( zh_nsxQuickSortCompare( pSort, pPtr1, pPtr2 ) <= 0 )
         {
            memcpy( pDst, pPtr1, iLen );
            pPtr1 += iLen;
            l1--;
         }
         else
         {
            memcpy( pDst, pPtr2, iLen );
            pPtr2 += iLen;
            l2--;
         }
         pDst += iLen;
      }
      if( l1 > 0 )
         memcpy( pDst, pPtr1, iLen * l1 );
      else if( l2 > 0 && f1 == f2 )
         memcpy( pDst, pPtr2, iLen * l2 );
      return ! f1;
   }
   return ZH_TRUE;
}

static void zh_nsxSortSortPage( LPNSXSORTINFO pSort )
{
   ZH_SIZE nSize = ( ZH_SIZE ) pSort->ulKeys * ( pSort->keyLen + 4 );

   if( ! zh_nsxQSort( pSort, pSort->pKeyPool, &pSort->pKeyPool[ nSize ], pSort->ulKeys ) )
      pSort->pStartKey = &pSort->pKeyPool[ nSize ];
   else
      pSort->pStartKey = pSort->pKeyPool;
}

static void zh_nsxSortBufferFlush( LPNSXSORTINFO pSort )
{
   if( pSort->ulPagesIO )
   {
      LPNSXINDEX pIndex = pSort->pTag->pIndex;
      ZH_SIZE nSize = ( ZH_SIZE ) pSort->ulPagesIO * NSX_PAGELEN;
      if( zh_fileWriteAt( pIndex->pFile, pSort->pBuffIO, nSize,
                    zh_nsxFileOffset( pIndex, pSort->ulFirstIO ) ) != nSize )
      {
         zh_nsxErrorRT( pIndex->pArea, EG_WRITE, EDBF_WRITE,
                        pIndex->IndexName, zh_fsError(), 0, NULL );
      }
      pSort->ulPagesIO = 0;
      pIndex->fFlush = ZH_TRUE;
      if( pIndex->fShared )
         pIndex->Changed = ZH_TRUE;
   }
}

static void zh_nsxSortStorePage( LPNSXSORTINFO pSort, LPPAGEINFO pPage )
{
   LPNSXINDEX pIndex = pSort->pTag->pIndex;

   if( ! pPage->Page )
   {
      pPage->Page = zh_nsxPageAlloc( pIndex );
      if( pSort->ulSizeIO )
      {
         if( pSort->ulPagesIO == pSort->ulSizeIO )
            zh_nsxSortBufferFlush( pSort );
         if( ! pSort->ulPagesIO ||
             zh_nsxFileOffset( pIndex, pSort->ulLastIO ) + NSX_PAGELEN ==
             zh_nsxFileOffset( pIndex, pPage->Page ) )
         {
            zh_nsxSetKeyCount( pPage, pPage->uiKeys );
            if( zh_nsxIsLeaf( pPage ) )
               zh_nsxLeafSetFreeOffset( pPage, pPage->uiOffset );
            memcpy( pSort->pBuffIO + pSort->ulPagesIO * NSX_PAGELEN,
                    zh_nsxPageBuffer( pPage ), NSX_PAGELEN );
            pSort->ulLastIO = pPage->Page;
            if( ! pSort->ulPagesIO++ )
               pSort->ulFirstIO = pPage->Page;
            pPage->Changed = ZH_FALSE;
            return;
         }
      }
   }
   if( ! pPage->pPrev )
      /* page is in not in hot pages list, write it now */
      zh_nsxPageSave( pIndex, pPage );
}

static ZH_BOOL zh_nsxSortAddNodeKey( LPNSXSORTINFO pSort, ZH_UCHAR * pKeyVal, ZH_ULONG ulRec )
{
   LPPAGEINFO pPage;
   ZH_ULONG ulPage = 0;
   int iLevel = 0;

   for( ;; )
   {
      pPage = pSort->NodeList[ iLevel ];
      if( pPage == NULL )
      {
         pPage = pSort->NodeList[ iLevel ] = zh_nsxPageNew( pSort->pTag, ZH_TRUE );
         if( ! pPage )
            return ZH_FALSE;
         if( iLevel == 0 )
         {
            /* executed once for first key only */
            zh_nsxSetPageType( pPage, NSX_LEAFPAGE );
            zh_nsxSetKeyRecSize( pPage, pSort->recSize );
            pPage->uiOffset = zh_nsxLeafPutKey( pSort->pTag, pPage,
                                                NSX_LEAFKEYOFFSET, NULL,
                                                pKeyVal, ulRec );
         }
         else
         {
            zh_nsxSetKeyRecSize( pPage, 4 );
            zh_nsxSetLowerPage( pPage, ulPage );
         }
         break;
      }
      else if( iLevel == 0 )
      {
         ZH_USHORT uiOffset = zh_nsxLeafPutKey( pSort->pTag, pPage,
                                                pPage->uiOffset, pSort->pLastKey,
                                                pKeyVal, ulRec );
         if( uiOffset != 0 )
         {
            pPage->uiOffset = uiOffset;
            break;
         }
#if defined( ZH_NSX_CLEAR_UNUSED )
         else
            memset( zh_nsxPageBuffer( pPage ) + pPage->uiOffset, 0,
                    NSX_PAGELEN - pPage->uiOffset );
#endif
      }
      else if( pPage->uiKeys == 0 )
      {
         zh_nsxSetLowerPage( pPage, ulPage );
         break;
      }
      else
      {
         zh_nsxSetKeyPage( pPage, pSort->keyLen, pPage->uiKeys - 1, ulPage );
         if( pPage->uiKeys < pSort->pTag->MaxKeys )
            break;
      }

      zh_nsxSortStorePage( pSort, pPage );
      ulPage = pPage->Page;
      zh_nsxPageRelease( pSort->pTag, pPage );
      pPage = pSort->NodeList[ iLevel ] = zh_nsxPageNew( pSort->pTag, ZH_TRUE );
      if( ! pPage )
         return ZH_FALSE;
      if( iLevel == 0 )
      {
         pSort->ulLastLeaf = ulPage;
         zh_nsxSetPageType( pPage, NSX_LEAFPAGE );
         zh_nsxSetKeyRecSize( pPage, pSort->recSize );
         pPage->uiOffset = NSX_LEAFKEYOFFSET;
      }
      else
         zh_nsxSetKeyRecSize( pPage, 4 );
      iLevel++;
   }

   if( iLevel > 0 )
   {
      ZH_UCHAR * pKeyPtr = zh_nsxGetBranchKeyPtr( pPage, pSort->keyLen, pPage->uiKeys );
      zh_nsxBranchKeySetRec( pKeyPtr, ulRec );
      memcpy( zh_nsxBranchKeyVal( pKeyPtr ), pKeyVal, pSort->keyLen );
   }
   pPage->uiKeys++;

   return ZH_TRUE;
}

static void zh_nsxSortWritePage( LPNSXSORTINFO pSort )
{
   ZH_SIZE nSize = ( ZH_SIZE ) pSort->ulKeys * ( pSort->keyLen + 4 );

   zh_nsxSortSortPage( pSort );

   if( pSort->pTempFile == NULL )
   {
      char szName[ ZH_PATH_MAX ];
      pSort->pTempFile = zh_fileCreateTemp( NULL, NULL, FC_NORMAL, szName );
      if( pSort->pTempFile == NULL )
         zh_nsxErrorRT( pSort->pTag->pIndex->pArea, EG_CREATE, EDBF_CREATE_TEMP,
                        szName, zh_fsError(), 0, NULL );
      else
         pSort->szTempFileName = zh_strdup( szName );
   }

   pSort->pSwapPage[ pSort->ulCurPage ].ulKeys = pSort->ulKeys;
   if( pSort->pTempFile != NULL )
   {
      pSort->pSwapPage[ pSort->ulCurPage ].nOffset = zh_fileSize( pSort->pTempFile );
      if( zh_fileWriteAt( pSort->pTempFile, pSort->pStartKey,
                          nSize, pSort->pSwapPage[ pSort->ulCurPage ].nOffset ) != nSize )
         zh_nsxErrorRT( pSort->pTag->pIndex->pArea, EG_WRITE, EDBF_WRITE_TEMP,
                        pSort->szTempFileName, zh_fsError(), 0, NULL );
   }
   else
      pSort->pSwapPage[ pSort->ulCurPage ].nOffset = 0;
   pSort->ulKeys = 0;
   pSort->ulCurPage++;
}

static void zh_nsxSortGetPageKey( LPNSXSORTINFO pSort, ZH_ULONG ulPage,
                                  ZH_UCHAR ** pKeyVal, ZH_ULONG * pulRec )
{
   int iLen = pSort->keyLen;

   if( pSort->pSwapPage[ ulPage ].ulKeyBuf == 0 )
   {
      ZH_ULONG ulKeys = ZH_MIN( pSort->ulPgKeys, pSort->pSwapPage[ ulPage ].ulKeys );
      ZH_SIZE nSize = ( ZH_SIZE ) ulKeys * ( iLen + 4 );

      if( pSort->pTempFile != NULL &&
          zh_fileReadAt( pSort->pTempFile, pSort->pSwapPage[ ulPage ].pKeyPool,
                         nSize, pSort->pSwapPage[ ulPage ].nOffset ) != nSize )
      {
         zh_nsxErrorRT( pSort->pTag->pIndex->pArea, EG_READ, EDBF_READ_TEMP,
                        pSort->szTempFileName, zh_fsError(), 0, NULL );
      }
      pSort->pSwapPage[ ulPage ].nOffset += nSize;
      pSort->pSwapPage[ ulPage ].ulKeyBuf = ulKeys;
      pSort->pSwapPage[ ulPage ].ulCurKey = 0;
   }
   *pKeyVal = &pSort->pSwapPage[ ulPage ].pKeyPool[ pSort->pSwapPage[ ulPage ].ulCurKey * ( iLen + 4 ) ];
   *pulRec = ZH_GET_LE_UINT32( *pKeyVal + iLen );
}

static void zh_nsxSortOrderPages( LPNSXSORTINFO pSort )
{
   pSort->ulFirst = 0;
   pSort->pSortedPages = ( ZH_ULONG * ) zh_xgrab( pSort->ulPages * sizeof( ZH_ULONG ) );
   pSort->pSortedPages[ 0 ] = 0;

   if( pSort->ulTotKeys > 0 )
   {
      int iLen = pSort->keyLen;
      ZH_UCHAR *pKey = NULL;

      ZH_ULONG n;

      for( n = 0; n < pSort->ulPages; n++ )
      {
         ZH_ULONG ulRec;
         ZH_LONG l, r;

         zh_nsxSortGetPageKey( pSort, n, &pKey, &ulRec );
         l = 0;
         r = n - 1;
         while( l <= r )
         {
            int i;
            ZH_ULONG ulPage;
            ZH_LONG m;
            ZH_UCHAR * pTmp;

            m = ( l + r ) >> 1;
            ulPage = pSort->pSortedPages[ m ];
            pTmp = &pSort->pSwapPage[ ulPage ].pKeyPool[ pSort->pSwapPage[ ulPage ].ulCurKey * ( iLen + 4 ) ];
            i = zh_nsxValCompare( pSort->pTag, pKey, iLen, pTmp, iLen, NSX_CMP_EXACT );
            if( i == 0 )
               i = ( ulRec < ZH_GET_LE_UINT32( &pTmp[ iLen ] ) ) ? -1 : 1;
            if( i >= 0 )
               l = m + 1;
            else
               r = m - 1;
         }
         for( r = n; r > l; r-- )
            pSort->pSortedPages[ r ] = pSort->pSortedPages[ r - 1 ];
         pSort->pSortedPages[ l ] = n;
      }
   }
}

static ZH_BOOL zh_nsxSortKeyGet( LPNSXSORTINFO pSort, ZH_UCHAR ** pKeyVal, ZH_ULONG * pulRec )
{
   ZH_ULONG ulPage = pSort->pSortedPages[ pSort->ulFirst ];

   /* check if first page has some keys yet */
   if( pSort->pSwapPage[ ulPage ].ulKeys > 0 )
   {
      int iLen = pSort->keyLen;
      ZH_UCHAR * pKey;
      ZH_ULONG ulRec;
      ZH_LONG l, r;

      /*
       * last key was taken from this page - we have to resort it.
       * This is done intentionally here to be sure that the key
       * value return by this function will not be overwritten by
       * next keys in page read from temporary file in function
       * zh_nsxSortGetPageKey() - please do not move this part down
       * even it seems to be correct
       */
      zh_nsxSortGetPageKey( pSort, ulPage, &pKey, &ulRec );

      l = pSort->ulFirst + 1;
      r = pSort->ulPages - 1;
      while( l <= r )
      {
         int i;
         ZH_ULONG ulPg;
         ZH_LONG m;
         ZH_UCHAR * pTmp;

         m = ( l + r ) >> 1;
         ulPg = pSort->pSortedPages[ m ];
         pTmp = &pSort->pSwapPage[ ulPg ].pKeyPool[ pSort->pSwapPage[ ulPg ].ulCurKey * ( iLen + 4 ) ];
         i = zh_nsxValCompare( pSort->pTag, pKey, iLen, pTmp, iLen, NSX_CMP_EXACT );
         if( i == 0 )
            i = ( ulRec < ZH_GET_LE_UINT32( &pTmp[ iLen ] ) ) ? -1 : 1;
         if( i > 0 )
            l = m + 1;
         else
            r = m - 1;
      }
      if( l > ( ZH_LONG ) pSort->ulFirst + 1 )
      {
         ulPage = pSort->pSortedPages[ pSort->ulFirst ];
         for( r = pSort->ulFirst + 1; r < l; r++ )
            pSort->pSortedPages[ r - 1 ] = pSort->pSortedPages[ r ];
         pSort->pSortedPages[ l - 1 ] = ulPage;
      }
   }
   else
   {
      pSort->ulFirst++;
   }
   if( pSort->ulFirst < pSort->ulPages )
   {
      ulPage = pSort->pSortedPages[ pSort->ulFirst ];
      zh_nsxSortGetPageKey( pSort, ulPage, pKeyVal, pulRec );
      pSort->pSwapPage[ ulPage ].ulCurKey++;
      pSort->pSwapPage[ ulPage ].ulKeys--;
      pSort->pSwapPage[ ulPage ].ulKeyBuf--;
      return ZH_TRUE;
   }

   *pKeyVal = NULL;
   *pulRec = 0;

   return ZH_FALSE;
}

static void zh_nsxSortKeyAdd( LPNSXSORTINFO pSort, ZH_ULONG ulRec, const char * pKeyVal, int iKeyLen )
{
   int iLen = pSort->keyLen;
   ZH_UCHAR * pDst;

   if( pSort->ulKeys >= pSort->ulPgKeys )
   {
      zh_nsxSortWritePage( pSort );
   }
   pDst = &pSort->pKeyPool[ pSort->ulKeys * ( iLen + 4 ) ];

   if( iLen > iKeyLen )
   {
      memcpy( pDst, pKeyVal, iKeyLen );
      memset( &pDst[ iKeyLen ], pSort->trailChar, iLen - iKeyLen );
   }
   else
   {
      memcpy( pDst, pKeyVal, iLen );
   }
   ZH_PUT_LE_UINT32( &pDst[ iLen ], ulRec );
   pSort->ulKeys++;
   pSort->ulTotKeys++;
}

static LPNSXSORTINFO zh_nsxSortNew( LPTAGINFO pTag, ZH_ULONG ulRecCount )
{
   LPNSXSORTINFO pSort;
   ZH_UCHAR * pBuf;
   int iLen = pTag->KeyLength;
   ZH_ULONG ulSize, ulMax, ulMin;

   if( ulRecCount == 0 )
      ulRecCount = 1;

   pSort = ( LPNSXSORTINFO ) zh_xgrabz( sizeof( NSXSORTINFO ) );

   ulMin = ( ZH_ULONG ) ceil( sqrt( ( double ) ulRecCount ) );
   ulMax = ( ( ZH_ULONG ) ceil( sqrt( ( double ) ulRecCount / ( iLen + 4 ) ) ) ) << 7;
   /*
    * this effectively increase allocated memory buffer for very large files
    * moving the maximum to: 267'443'712 for 4'294'967'295 records and 250
    * index key length.
    * if you want to force smaller buffer I wrote below then add here:
    * ulMax = ulMin;
    */
   ulSize = ( 1L << 20 ) / ( iLen + 4 );
   while( ulMax < ulSize )
      ulMax <<= 1;
   if( ulMax > ulRecCount )
      ulMax = ulRecCount;

   do
   {
      ulSize = ulMax * ( iLen + 4 );
      pBuf = ( ZH_UCHAR * ) zh_xalloc( ulSize << 2 );
      if( pBuf )
      {
         zh_xfree( pBuf );
         pBuf = ( ZH_UCHAR * ) zh_xalloc( ulSize << 1 );
      }
      else
      {
         ulMax >>= 1;
      }
   }
   while( ! pBuf && ulMax >= ulMin );

   if( ! pBuf )
   {
      /* call zh_xgrab() to force out of memory error,
       * though in multi process environment this call may return
       * with success when other process free some memory
       * (also the size of buf is reduced to absolute minimum).
       * Sorry but I'm to lazy to implement indexing with smaller
       * memory though it's possible - just simply I can even create
       * index on-line by key adding like in normal update process.
       * The memory necessary to index file is now ~
       *    ~ (keySize+4+sizeof(NSXSWAPPAGE)) * sqrt(ulRecCount) * 2
       * so the maximum is for DBF with 2^32 records and keySize 256 ~
       * ~ 2^17 * 284 ~=~ 37 MB
       * this is not a problem for current computers and I do not see
       * any way to use DBFs with four billions records and indexes with
       * such long (256 bytes) keys on the old ones - they will be simply
       * to slow. IMHO it's also better to signal out of memory here and
       * force some system upgrades then run process which will have to
       * take many hours, Druzus.
       */
      ulMax = ulMin;
      pBuf = ( ZH_UCHAR * ) zh_xgrab( ( ulMax << 1 ) * ( iLen + 4 ) );
   }

   pSort->pTag = pTag;
   pSort->pTempFile = NULL;
   pSort->keyLen = iLen;
   pSort->trailChar = pTag->TrailChar;
   pSort->recSize = zh_nsxGetRecSize( ulRecCount );
   pSort->fUnique = pTag->UniqueKey;
   pSort->ulMaxKey = ulMax << 1;
   pSort->ulPgKeys = ulMax;
   pSort->ulMaxRec = ulRecCount;
   pSort->pKeyPool = pBuf;
   pSort->ulPages = ( ulRecCount + pSort->ulPgKeys - 1 ) / pSort->ulPgKeys;
   /* check for overflow on 32-bit machines when number of records is nearly 2^32 */
   if( ! pSort->ulPages )
      pSort->ulPages = ulRecCount / pSort->ulPgKeys + 1;
   pSort->pSwapPage = ( LPNSXSWAPPAGE ) zh_xgrabz( sizeof( NSXSWAPPAGE ) * pSort->ulPages );
   return pSort;
}

static void zh_nsxSortFree( LPNSXSORTINFO pSort, ZH_BOOL fFull )
{
   if( pSort->pTempFile != NULL )
   {
      zh_fileClose( pSort->pTempFile );
      pSort->pTempFile = NULL;
   }
   if( pSort->szTempFileName )
   {
      zh_fileDelete( pSort->szTempFileName );
      zh_xfree( pSort->szTempFileName );
      pSort->szTempFileName = NULL;
   }
   if( pSort->pKeyPool )
   {
      zh_xfree( pSort->pKeyPool );
      pSort->pKeyPool = NULL;
   }
   if( pSort->pSwapPage )
   {
      zh_xfree( pSort->pSwapPage );
      pSort->pSwapPage = NULL;
   }
   if( pSort->pBuffIO )
   {
      zh_xfree( pSort->pBuffIO );
      pSort->pBuffIO = NULL;
   }
   if( pSort->pSortedPages )
   {
      zh_xfree( pSort->pSortedPages );
      pSort->pSortedPages = NULL;
   }
   if( fFull )
   {
      zh_xfree( pSort );
   }
}

static void zh_nsxSortOut( LPNSXSORTINFO pSort )
{
   ZH_BOOL fUnique = pSort->fUnique, fNext;
   LPTAGINFO pTag = pSort->pTag;
   ZH_ULONG ulLastPage = 0, ulPage, ulRec, ulKey;
   ZH_UCHAR * pKeyVal = NULL;
   int iLen = pSort->keyLen, iLevel;

   pSort->ulPages = pSort->ulCurPage + 1;
   pSort->ulPgKeys = pSort->ulMaxKey / pSort->ulPages;
   if( pSort->ulPages > 1 )
   {
      ZH_UCHAR * pBuf = pSort->pKeyPool;
      zh_nsxSortWritePage( pSort );
      for( ulPage = 0; ulPage < pSort->ulPages; ulPage++ )
      {
         pSort->pSwapPage[ ulPage ].ulKeyBuf = 0;
         pSort->pSwapPage[ ulPage ].ulCurKey = 0;
         pSort->pSwapPage[ ulPage ].pKeyPool = pBuf;
         pBuf += pSort->ulPgKeys * ( pSort->keyLen + 4 );
      }
   }
   else
   {
      zh_nsxSortSortPage( pSort );
      pSort->pSwapPage[ 0 ].ulKeys = pSort->ulKeys;
      pSort->pSwapPage[ 0 ].ulKeyBuf = pSort->ulKeys;
      pSort->pSwapPage[ 0 ].ulCurKey = 0;
      pSort->pSwapPage[ 0 ].pKeyPool = pSort->pStartKey;
   }

#if defined( ZH_NSX_SIX_STRICT )
   if( ! pTag->HeadBlock )
   {
      pTag->HeadBlock = zh_nsxPageAlloc( pTag->pIndex );
      zh_nsxIndexTagAdd( pTag->pIndex, pTag );
   }
#endif

   zh_nsxSortOrderPages( pSort );

   if( zh_vmRequestQuery() != 0 )
      return;

   for( ulKey = 0; ulKey < pSort->ulTotKeys; ulKey++ )
   {
      if( ! zh_nsxSortKeyGet( pSort, &pKeyVal, &ulRec ) )
      {
         if( zh_vmRequestQuery() != 0 )
            return;
         zh_errInternal( 9999, "zh_nsxSortOut: memory structure corrupted.", NULL, NULL );
      }
      if( fUnique )
      {
         if( ulKey != 0 && zh_nsxValCompare( pTag, pSort->pLastKey, iLen,
                                             pKeyVal, iLen, NSX_CMP_EXACT ) == 0 )
            continue;
      }
#ifdef ZH_NSX_DEBUG_EXT
      if( ulKey != 0 )
      {
         int i = zh_nsxValCompare( pTag, pSort->pLastKey, iLen, pKeyVal,
                                   iLen, NSX_CMP_EXACT );
         if( i == 0 )
            i = ( pSort->ulLastRec < ulRec ) ? -1 : 1;
         if( i > 0 )
         {
            printf( "\r\nulKey=%ld, pKeyVal=[%s][%ld], pKeyLast=[%s][%ld]\r\n",
                    ulKey, pKeyVal, ulRec, pSort->pLastKey, pSort->ulLastRec ); fflush( stdout );
            if( zh_vmRequestQuery() != 0 )
               return;
            zh_errInternal( 9999, "zh_nsxSortOut: sorting fails.", NULL, NULL );
         }
      }
#endif
      if( ! zh_nsxSortAddNodeKey( pSort, pKeyVal, ulRec ) )
         return;
      if( ulKey < pSort->ulTotKeys - 1 )
      {
         pSort->ulLastRec = ulRec;
         memcpy( pSort->pLastKey, pKeyVal, iLen );
      }
   }

#ifdef ZH_NSX_DEBUG
   {
      ZH_UCHAR * pTemp;
      if( zh_nsxSortKeyGet( pSort, &pTemp, &ulRec ) )
      {
         if( zh_vmRequestQuery() != 0 )
            return;
         zh_errInternal( 9999, "zh_nsxSortOut: memory structure corrupted(2).", NULL, NULL );
      }
   }
#endif

   if( pSort->NodeList[ 0 ] == NULL )
   {
      pSort->NodeList[ 0 ] = zh_nsxPageNew( pTag, ZH_TRUE );
      if( pSort->NodeList[ 0 ] == NULL )
         return;
      zh_nsxSetPageType( pSort->NodeList[ 0 ], NSX_LEAFPAGE );
      zh_nsxSetKeyRecSize( pSort->NodeList[ 0 ], pSort->recSize );
      pSort->NodeList[ 0 ]->uiOffset = NSX_LEAFKEYOFFSET;
   }

   iLevel = 0;
   ulPage = 0;
   fNext = ZH_TRUE;
   do
   {
      if( iLevel + 1 == NSX_STACKSIZE || pSort->NodeList[ iLevel + 1 ] == NULL )
      {
         zh_nsxPageType( pSort->NodeList[ iLevel ] ) |= NSX_ROOTPAGE;
         zh_nsxSortStorePage( pSort, pSort->NodeList[ iLevel ] );
         if( ulPage && ! ulLastPage && pSort->NodeList[ iLevel ]->uiKeys != 0 )
            ulLastPage = pSort->NodeList[ iLevel ]->Page;
         pTag->RootBlock = pSort->NodeList[ iLevel ]->Page;
         fNext = ZH_FALSE;
      }
      else
      {
         zh_nsxSortStorePage( pSort, pSort->NodeList[ iLevel ] );
         if( iLevel == 0 )
         {
            if( pSort->NodeList[ iLevel ]->uiKeys == 0 )
               /* last leaf page is empty */
               ulPage = pSort->NodeList[ 0 ]->Page;
         }
         else if( ulPage && ! ulLastPage && pSort->NodeList[ iLevel ]->uiKeys != 0 )
            ulLastPage = pSort->NodeList[ iLevel ]->Page;

         if( pSort->NodeList[ iLevel + 1 ]->uiKeys == 0 )
            zh_nsxSetLowerPage( pSort->NodeList[ iLevel + 1 ],
                                pSort->NodeList[ iLevel ]->Page );
         else
            zh_nsxSetKeyPage( pSort->NodeList[ iLevel + 1 ], pSort->keyLen,
                              pSort->NodeList[ iLevel + 1 ]->uiKeys - 1,
                              pSort->NodeList[ iLevel ]->Page );
      }
      zh_nsxPageRelease( pTag, pSort->NodeList[ iLevel ] );
      iLevel++;
   }
   while( fNext );

   /* small hack to detect if we still have the value of the key before
    * last in pSort->pLastKey in unique indexes. Such information is
    * necessary to properly update the last leaf key so it will not
    * be empty. SIX3 NSX RDD does not do that so it can create corrupted
    * indexes in such case.
    */
   fUnique = fUnique && ulLastPage &&
             zh_nsxValCompare( pTag, pSort->pLastKey, iLen,
                               pKeyVal, iLen, NSX_CMP_EXACT ) == 0;

   zh_nsxSortBufferFlush( pSort );
   zh_nsxSortFree( pSort, ZH_FALSE );

   if( ulLastPage && zh_vmRequestQuery() == 0 )
   {
      /* last leaf page is empty, we have to move last key
       * to this page and move the last key from the last
       * nonempty leaf page to previous last key location
       * in branch node
       * SIX3 NSX RDD does not balance the right most branch
       * pages and only move the last to keys
       */
      LPPAGEINFO pPage, pLastLeaf, pLastPage;

      if( fUnique )
      {
         /* SIXNSX does not do that - see note before above - it's a bug */
         zh_nsxTagBottomKey( pTag );
         if( ! zh_nsxTagPrevKey( pTag ) || ! zh_nsxTagPrevKey( pTag ) )
            return;
         pSort->ulLastRec = pTag->CurKeyInfo->rec;
         memcpy( pSort->pLastKey, pTag->CurKeyInfo->val, iLen );
         pTag->CurKeyOffset = 0;
         pTag->stackLevel = 0;
      }

      pPage = zh_nsxPageLoad( pTag, ulPage );
      if( pPage )
      {
         pLastLeaf = zh_nsxPageLoad( pTag, pSort->ulLastLeaf );
         if( pLastLeaf )
         {
            pLastPage = zh_nsxPageLoad( pTag, ulLastPage );
            if( pLastPage )
            {
               ZH_UCHAR * pBuffer;
               ZH_USHORT uiKey, uiRecSize;

               /* remove the key before last from the last nonempty leaf page */
               pBuffer = ( ZH_UCHAR * ) zh_nsxPageBuffer( pLastLeaf );
               uiRecSize = zh_nsxGetKeyRecSize( pLastLeaf );
               pLastLeaf->uiOffset = NSX_LEAFKEYOFFSET;
               pLastLeaf->uiKeys--;
               for( uiKey = 0; uiKey < pLastLeaf->uiKeys; uiKey++ )
                  pLastLeaf->uiOffset += pBuffer[ pLastLeaf->uiOffset + uiRecSize ];
#if defined( ZH_NSX_CLEAR_UNUSED ) && ! defined( ZH_NSX_SIX_STRICT )
               memset( zh_nsxPageBuffer( pLastLeaf ) + pLastLeaf->uiOffset, 0,
                       NSX_PAGELEN - pLastLeaf->uiOffset );
#endif
               /* copy last key to last leaf page */
               pKeyVal = ( ZH_UCHAR * ) zh_nsxGetKeyVal( pLastPage, pTag->KeyLength,
                                                      pLastPage->uiKeys - 1 );
               ulRec = zh_nsxGetKeyRec( pLastPage, pTag->KeyLength,
                                        pLastPage->uiKeys - 1 );
               pPage->uiOffset = zh_nsxLeafPutKey( pTag, pPage, NSX_LEAFKEYOFFSET,
                                                   pSort->pLastKey, pKeyVal, ulRec );
               pPage->uiKeys++;
               /* copy the key before last to the last branch page */
               zh_nsxSetKeyRec( pLastPage, pTag->KeyLength,
                                pLastPage->uiKeys - 1, pSort->ulLastRec );
               memcpy( pKeyVal, pSort->pLastKey, pTag->KeyLength );

               /* mark pages as modified */
               pPage->Changed = pLastPage->Changed = pLastLeaf->Changed = ZH_TRUE;

               zh_nsxPageRelease( pTag, pLastPage );
            }
            zh_nsxPageRelease( pTag, pLastLeaf );
         }
         zh_nsxPageRelease( pTag, pPage );
      }
   }
}

/*
 * create tag in index file
 */
static ZH_ERRCODE zh_nsxTagCreate( LPTAGINFO pTag, ZH_BOOL fReindex )
{
   LPNSXAREA pArea = pTag->pIndex->pArea;
   PZH_ITEM pWhileItem = NULL, pEvalItem = NULL;
   ZH_ULONG ulRecCount, ulRecNo = pArea->dbfarea.ulRecNo;
   LPNSXSORTINFO pSort;
   ZH_LONG lStep = 0;
   ZH_ERRCODE errCode = ZH_SUCCESS;

   if( pArea->dbfarea.area.lpdbOrdCondInfo )
   {
      pWhileItem = pArea->dbfarea.area.lpdbOrdCondInfo->itmCobWhile;
      lStep = pArea->dbfarea.area.lpdbOrdCondInfo->lStep;
      pEvalItem = pArea->dbfarea.area.lpdbOrdCondInfo->itmCobEval;
   }

   if( pTag->Custom )
   {
      ulRecCount = 0;
   }
   else
   {
      errCode = SELF_RECCOUNT( &pArea->dbfarea.area, &ulRecCount );
      if( errCode != ZH_SUCCESS )
         return errCode;
   }
   pArea->pSort = pSort = zh_nsxSortNew( pTag, ulRecCount );
   pSort->fReindex = fReindex;

   if( ulRecCount == 0 )
   {
      LPPAGEINFO pPage;

#if defined( ZH_NSX_SIX_STRICT )
      if( ! pTag->HeadBlock )
      {
         pTag->HeadBlock = zh_nsxPageAlloc( pTag->pIndex );
         zh_nsxIndexTagAdd( pTag->pIndex, pTag );
      }
#endif

      pPage = zh_nsxPageNew( pTag, ZH_FALSE );
      if( pPage )
      {
         pTag->RootBlock = pPage->Page;
         zh_nsxSetPageType( pPage, NSX_ROOTPAGE | NSX_LEAFPAGE );
         zh_nsxSetKeyRecSize( pPage, zh_nsxGetRecSize( 0 ) );
         pPage->uiOffset = NSX_LEAFKEYOFFSET;
         zh_nsxPageRelease( pTag, pPage );
      }
      else
      {
         errCode = ZH_FAILURE;
      }
   }
   else
   {
      LPTAGINFO pSaveTag = pArea->lpCurTag;
      ZH_ULONG ulStartRec = 0, ulNextCount = 0;
      ZH_BOOL fDirectRead, fUseFilter = ZH_FALSE;
      ZH_BYTE * pSaveRecBuff = pArea->dbfarea.pRecord;
      char szBuffer[ NSX_MAXKEYLEN ];
      int iRecBuff = 0, iRecBufSize, iRec;
      double d;
      PZH_CODEPAGE cdpTmp = zh_cdpSelect( pArea->dbfarea.area.cdPage );
      PZH_ITEM pItem = NULL;
      PZH_ITEM pForItem;

      pForItem = pTag->pForItem;
      if( pTag->nField )
         pItem = zh_itemNew( NULL );

      if( ! pArea->dbfarea.area.lpdbOrdCondInfo || pArea->dbfarea.area.lpdbOrdCondInfo->fAll )
      {
         pArea->lpCurTag = NULL;
      }
      else
      {
         if( pArea->dbfarea.area.lpdbOrdCondInfo->itmRecID )
            ulStartRec = zh_itemGetNL( pArea->dbfarea.area.lpdbOrdCondInfo->itmRecID );
         if( ulStartRec )
         {
            ulNextCount = 1;
         }
         else if( pArea->dbfarea.area.lpdbOrdCondInfo->fRest || pArea->dbfarea.area.lpdbOrdCondInfo->lNextCount > 0 )
         {
            if( pArea->dbfarea.area.lpdbOrdCondInfo->itmStartRecID )
               ulStartRec = zh_itemGetNL( pArea->dbfarea.area.lpdbOrdCondInfo->itmStartRecID );
            if( ! ulStartRec )
               ulStartRec = ulRecNo;
            if( pArea->dbfarea.area.lpdbOrdCondInfo->lNextCount > 0 )
               ulNextCount = pArea->dbfarea.area.lpdbOrdCondInfo->lNextCount;
         }
         else if( pArea->dbfarea.area.lpdbOrdCondInfo->fUseFilter )
         {
            fUseFilter = ZH_TRUE;
         }
         else if( ! pArea->dbfarea.area.lpdbOrdCondInfo->fUseCurrent )
         {
            pArea->lpCurTag = NULL;
         }
         else if( pArea->lpCurTag )
         {
            if( zh_nsxTagLockRead( pArea->lpCurTag ) )
            {
               zh_nsxTagRefreshScope( pArea->lpCurTag );
               zh_nsxTagGoTop( pArea->lpCurTag );
               if( ! pArea->lpCurTag->TagEOF )
                  ulStartRec = pArea->lpCurTag->CurKeyInfo->rec;
               zh_nsxTagUnLockRead( pArea->lpCurTag );
            }
         }
      }

      pSort->ulSizeIO = ( 1 << 16 ) / NSX_PAGELEN;
      pSort->pBuffIO = ( ZH_UCHAR * ) zh_xgrab( pSort->ulSizeIO * NSX_PAGELEN );
      iRecBufSize = ( pSort->ulSizeIO * NSX_PAGELEN ) / pArea->dbfarea.uiRecordLen;
      fDirectRead = ! zh_setGetStrictRead() && iRecBufSize > 1 && /* ! pArea->dbfarea.area.lpdbRelations && */
                    ( ! pArea->dbfarea.area.lpdbOrdCondInfo || pArea->dbfarea.area.lpdbOrdCondInfo->fAll ||
                      ( pArea->lpCurTag == NULL && ! fUseFilter ) );

      if( ulStartRec == 0 && pArea->lpCurTag == NULL )
         ulStartRec = 1;

      if( ulStartRec == 0 )
      {
         errCode = SELF_GOTOP( &pArea->dbfarea.area );
      }
      else
      {
         errCode = SELF_GOTO( &pArea->dbfarea.area, ulStartRec );
         if( fUseFilter && errCode == ZH_SUCCESS )
            errCode = SELF_SKIPFILTER( &pArea->dbfarea.area, 1 );
      }

      ulRecNo = pArea->dbfarea.ulRecNo;

      while( errCode == ZH_SUCCESS && ! pArea->dbfarea.area.fEof )
      {
         if( zh_vmRequestQuery() != 0 )
         {
            errCode = ZH_FAILURE;
            break;
         }

         if( fDirectRead )
         {
            if( ulRecNo > ulRecCount )
               break;
            if( iRecBuff == 0 || iRecBuff >= iRecBufSize )
            {
               ZH_SIZE nSize;

               if( ulRecCount - ulRecNo >= ( ZH_ULONG ) iRecBufSize )
                  iRec = iRecBufSize;
               else
                  iRec = ulRecCount - ulRecNo + 1;
               if( ulNextCount > 0 && ulNextCount < ( ZH_ULONG ) iRec )
                  iRec = ( int ) ulNextCount;
               nSize = ( ZH_SIZE ) iRec * pArea->dbfarea.uiRecordLen;
               if( zh_fileReadAt( pArea->dbfarea.pDataFile, pSort->pBuffIO, nSize,
                                  ( ZH_FOFFSET ) pArea->dbfarea.uiHeaderLen +
                                  ( ZH_FOFFSET ) ( ulRecNo - 1 ) *
                                  ( ZH_FOFFSET ) pArea->dbfarea.uiRecordLen ) != nSize )
               {
                  zh_nsxErrorRT( pTag->pIndex->pArea, EG_READ, EDBF_READ,
                                 pTag->pIndex->IndexName, zh_fsError(), 0, NULL );
                  break;
               }
               iRecBuff = 0;
            }
            pArea->dbfarea.pRecord = pSort->pBuffIO + iRecBuff * pArea->dbfarea.uiRecordLen;
            pArea->dbfarea.ulRecNo = ulRecNo;
            if( SELF_GETREC( &pArea->dbfarea.area, NULL ) == ZH_FAILURE )
               break;
            pArea->dbfarea.fValidBuffer = pArea->dbfarea.fPositioned = ZH_TRUE;
            pArea->dbfarea.fDeleted = pArea->dbfarea.pRecord[ 0 ] == '*';
            /* Force relational movement in child WorkAreas */
            if( pArea->dbfarea.area.lpdbRelations )
            {
               errCode = SELF_SYNCCHILDREN( &pArea->dbfarea.area );
               if( errCode != ZH_SUCCESS )
                  break;
            }
            iRecBuff++;
         }

         if( pWhileItem && ! zh_nsxEvalCond( NULL, pWhileItem, ZH_FALSE ) )
            break;

         if( ulRecNo <= ulRecCount &&
             ( pForItem == NULL || zh_nsxEvalCond( pArea, pForItem, ZH_FALSE ) ) )
         {
            if( pTag->nField )
               errCode = SELF_GETVALUE( &pArea->dbfarea.area, pTag->nField, pItem );
            else
               pItem = zh_vmEvalBlockOrMacro( pTag->pKeyItem );

            switch( zh_itemType( pItem ) )
            {
               case ZH_IT_STRING:
               case ZH_IT_MEMO:
                  zh_nsxSortKeyAdd( pSort, pArea->dbfarea.ulRecNo,
                                    zh_itemGetCPtr( pItem ),
                                    ( ZH_INTCAST ) zh_itemGetCLen( pItem ) );
                  break;

               case ZH_IT_INTEGER:
               case ZH_IT_LONG:
               case ZH_IT_DOUBLE:
                  d = zh_itemGetND( pItem );
                  ZH_DBL2ORD( &d, szBuffer );
                  zh_nsxSortKeyAdd( pSort, pArea->dbfarea.ulRecNo, szBuffer, 8 );
                  break;

               case ZH_IT_DATE:
               case ZH_IT_TIMESTAMP:
                  if( pTag->KeyType == 'T' )
                     d = zh_itemGetTD( pItem );
                  else
                     d = ( double ) zh_itemGetDL( pItem );
                  ZH_DBL2ORD( &d, szBuffer );
                  zh_nsxSortKeyAdd( pSort, pArea->dbfarea.ulRecNo, szBuffer, 8 );
                  break;

               case ZH_IT_LOGICAL:
                  szBuffer[ 0 ] = zh_itemGetL( pItem ) ? 'T' : 'F';
                  zh_nsxSortKeyAdd( pSort, pArea->dbfarea.ulRecNo, szBuffer, 1 );
                  break;

               default:
                  zh_nsxErrorRT( pArea, EG_DATATYPE, EDBF_INVALIDKEY,
                                 pTag->pIndex->IndexName, 0, 0, NULL );
                  errCode = ZH_FAILURE;
                  pTag->Partial = ZH_TRUE;
                  pEvalItem = NULL;
                  ulNextCount = 1;
                  break;
            }
         }

         if( ulNextCount > 0 )
         {
            if( --ulNextCount == 0 )
               break;
         }

         if( pEvalItem )
         {
            if( lStep >= pArea->dbfarea.area.lpdbOrdCondInfo->lStep )
            {
               lStep = 0;
               if( ! zh_nsxEvalCond( pArea, pEvalItem, ZH_FALSE ) )
               {
                  pTag->Partial = ZH_TRUE;
                  break;
               }
            }
            ++lStep;
         }

         if( fDirectRead )
            ulRecNo++;
         else if( errCode == ZH_SUCCESS )
         {
            errCode = SELF_SKIPRAW( &pArea->dbfarea.area, 1 );
            if( fUseFilter && errCode == ZH_SUCCESS )
               errCode = SELF_SKIPFILTER( &pArea->dbfarea.area, 1 );
            ulRecNo = pArea->dbfarea.ulRecNo;
         }
      }

      if( fDirectRead )
      {
         pArea->dbfarea.pRecord = pSaveRecBuff;
         pArea->dbfarea.fValidBuffer = ZH_FALSE;
         if( errCode == ZH_SUCCESS )
            errCode = SELF_GOTO( &pArea->dbfarea.area, ulRecNo );
      }

      if( errCode == ZH_SUCCESS )
         zh_nsxSortOut( pSort );

      if( pTag->nField )
         zh_itemRelease( pItem );

      pArea->lpCurTag = pSaveTag;
      zh_cdpSelect( cdpTmp );
   }

   zh_nsxSortFree( pSort, ZH_TRUE );
   pArea->pSort = NULL;

   return errCode;
}

/*
 * recreate tags in index file
 */
static ZH_ERRCODE zh_nsxReIndex( LPNSXINDEX pIndex )
{
   ZH_ERRCODE errCode = ZH_FAILURE;

   if( zh_nsxIndexLockWrite( pIndex, ZH_FALSE ) )
   {
      int i;

      errCode = ZH_SUCCESS;
      zh_nsxIndexTrunc( pIndex );

      for( i = 0; i < pIndex->iTags; i++ )
      {
         LPTAGINFO pTag = pIndex->lpTags[ i ];
         pTag->HeadBlock = pTag->RootBlock = pTag->keyCount = 0;
         pTag->HdrChanged = ZH_TRUE;
         errCode = zh_nsxTagCreate( pTag, ZH_TRUE );
         if( errCode != ZH_SUCCESS )
            break;
      }
      zh_nsxIndexUnLockWrite( pIndex );
   }
   return errCode;
}


/* ************************************************************************* */

/* Implementation of RDD methods */

static ZH_ERRCODE zh_nsxGoBottom( NSXAREAP pArea )
{
   ZH_ERRCODE retval;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_nsxGoBottom(%p)", ( void * ) pArea ) );

   if( SELF_GOCOLD( &pArea->dbfarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   if( ! pArea->lpCurTag )
      return SUPER_GOBOTTOM( &pArea->dbfarea.area );

   if( pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped )
      SELF_FORCEREL( &pArea->dbfarea.area );

   if( ! zh_nsxTagLockRead( pArea->lpCurTag ) )
      return ZH_FAILURE;
   zh_nsxTagRefreshScope( pArea->lpCurTag );

   zh_nsxTagGoBottom( pArea->lpCurTag );

   pArea->dbfarea.area.fTop = ZH_FALSE;
   pArea->dbfarea.area.fBottom = ZH_TRUE;

   if( pArea->lpCurTag->TagEOF )
      retval = SELF_GOTO( &pArea->dbfarea.area, 0 );
   else
   {
      retval = SELF_GOTO( &pArea->dbfarea.area, pArea->lpCurTag->CurKeyInfo->rec );
      if( retval != ZH_FAILURE && pArea->dbfarea.fPositioned )
         retval = SELF_SKIPFILTER( &pArea->dbfarea.area, -1 );
   }
   zh_nsxTagUnLockRead( pArea->lpCurTag );

   return retval;
}

static ZH_ERRCODE zh_nsxTop( NSXAREAP pArea )
{
   ZH_ERRCODE retval;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_nsxTop(%p)", ( void * ) pArea ) );

   if( SELF_GOCOLD( &pArea->dbfarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   if( ! pArea->lpCurTag )
      return SUPER_GOTOP( &pArea->dbfarea.area );

   if( pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped )
      SELF_FORCEREL( &pArea->dbfarea.area );

   if( ! zh_nsxTagLockRead( pArea->lpCurTag ) )
      return ZH_FAILURE;
   zh_nsxTagRefreshScope( pArea->lpCurTag );

   zh_nsxTagGoTop( pArea->lpCurTag );

   pArea->dbfarea.area.fTop = ZH_TRUE;
   pArea->dbfarea.area.fBottom = ZH_FALSE;

   if( pArea->lpCurTag->TagEOF )
      retval = SELF_GOTO( &pArea->dbfarea.area, 0 );
   else
   {
      retval = SELF_GOTO( &pArea->dbfarea.area, pArea->lpCurTag->CurKeyInfo->rec );
      if( retval != ZH_FAILURE && pArea->dbfarea.fPositioned )
         retval = SELF_SKIPFILTER( &pArea->dbfarea.area, 1 );
   }
   zh_nsxTagUnLockRead( pArea->lpCurTag );

   return retval;
}

static ZH_ERRCODE zh_nsxSeek( NSXAREAP pArea, ZH_BOOL fSoftSeek, PZH_ITEM pItem, ZH_BOOL fFindLast )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_nsxSeek(%p, %d, %p, %d)", ( void * ) pArea, fSoftSeek, ( void * ) pItem, fFindLast ) );

   if( SELF_GOCOLD( &pArea->dbfarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   if( ! pArea->lpCurTag )
   {
      zh_nsxErrorRT( pArea, EG_NOORDER, EDBF_NOTINDEXED, NULL, 0, EF_CANDEFAULT, NULL );
      return ZH_FAILURE;
   }
   else
   {
      LPKEYINFO pKey;
      ZH_ERRCODE retval = ZH_SUCCESS;
      ZH_BOOL  fEOF = ZH_FALSE, fLast;
      ZH_USHORT uiLen;
      ZH_ULONG ulRec;

      if( pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped )
         SELF_FORCEREL( &pArea->dbfarea.area );

      pArea->dbfarea.area.fTop = pArea->dbfarea.area.fBottom = ZH_FALSE;
      pArea->dbfarea.area.fEof = ZH_FALSE;

      fLast = pArea->lpCurTag->fUsrDescend ? ! fFindLast : fFindLast;

      pKey = zh_nsxKeyPutItem( NULL, pItem,
                               fLast ? NSX_MAX_REC_NUM : NSX_IGNORE_REC_NUM,
                               pArea->lpCurTag, ZH_TRUE, &uiLen );

      if( ! zh_nsxTagLockRead( pArea->lpCurTag ) )
      {
         zh_nsxKeyFree( pKey );
         return ZH_FAILURE;
      }
      zh_nsxTagRefreshScope( pArea->lpCurTag );

      if( zh_nsxTagKeyFind( pArea->lpCurTag, pKey, uiLen ) )
         ulRec = pArea->lpCurTag->CurKeyInfo->rec;
      else
         ulRec = 0;

      if( ( ulRec == 0 && ! fSoftSeek ) || pArea->lpCurTag->TagEOF )
         fEOF = ZH_TRUE;
      else
      {
         if( ! zh_nsxInBottomScope( pArea->lpCurTag, pArea->lpCurTag->CurKeyInfo->val ) )
            fEOF = ZH_TRUE;
         else if( ! zh_nsxInTopScope( pArea->lpCurTag, pArea->lpCurTag->CurKeyInfo->val ) )
         {
            zh_nsxTagGoTop( pArea->lpCurTag );
            if( pArea->lpCurTag->CurKeyInfo->rec == 0 ||
                pArea->lpCurTag->TagEOF )
               fEOF = ZH_TRUE;
         }
      }
      zh_nsxTagUnLockRead( pArea->lpCurTag );
      if( ! fEOF )
      {
         retval = SELF_GOTO( &pArea->dbfarea.area, pArea->lpCurTag->CurKeyInfo->rec );
         if( retval != ZH_FAILURE && pArea->dbfarea.fPositioned )
         {
            retval = SELF_SKIPFILTER( &pArea->dbfarea.area, fFindLast ? -1 : 1 );
            if( retval != ZH_FAILURE && ulRec && pArea->dbfarea.fPositioned )
            {
               pArea->dbfarea.area.fFound = ( ulRec == pArea->dbfarea.ulRecNo ||
                     zh_nsxValCompare( pArea->lpCurTag, pKey->val, uiLen,
                                       pArea->lpCurTag->CurKeyInfo->val,
                                       pArea->lpCurTag->KeyLength,
                                       NSX_CMP_PREFIX ) == 0 );
               if( ! pArea->dbfarea.area.fFound && ! fSoftSeek )
                  fEOF = ZH_TRUE;
            }
         }
      }
      if( retval != ZH_FAILURE && ( fEOF ||
          ! zh_nsxKeyInScope( pArea->lpCurTag, pArea->lpCurTag->CurKeyInfo ) ) )
      {
         retval = SELF_GOTO( &pArea->dbfarea.area, 0 );
      }
      if( pArea->dbfarea.fPositioned || pArea->dbfarea.ulRecNo != 1 )
         pArea->dbfarea.area.fBof = ZH_FALSE;
      zh_nsxKeyFree( pKey );
      return retval;
   }
}

static ZH_ERRCODE zh_nsxSkipRaw( NSXAREAP pArea, ZH_LONG lToSkip )
{
   ZH_ERRCODE retval;
   ZH_BOOL fOut = ZH_FALSE, fForward;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_nsxSkipRaw(%p, %ld)", ( void * ) pArea, lToSkip ) );

   if( SELF_GOCOLD( &pArea->dbfarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   if( ! pArea->lpCurTag || lToSkip == 0 )
      return SUPER_SKIPRAW( &pArea->dbfarea.area, lToSkip );

   if( ! zh_nsxTagLockRead( pArea->lpCurTag ) )
      return ZH_FAILURE;
   zh_nsxTagRefreshScope( pArea->lpCurTag );

   fForward = ( lToSkip > 0 );

   if( ! zh_nsxCurKeyRefresh( pArea->lpCurTag ) )
   {
      if( fForward || pArea->dbfarea.fPositioned )
         fOut = ZH_TRUE;
      else
      {
         zh_nsxTagGoBottom( pArea->lpCurTag );
         fOut = pArea->lpCurTag->TagEOF;
         lToSkip++;
      }
   }

   if( fForward )
   {
      while( ! fOut && ! pArea->lpCurTag->TagEOF && lToSkip-- > 0 )
      {
         zh_nsxTagSkipNext( pArea->lpCurTag );
      }
      retval = SELF_GOTO( &pArea->dbfarea.area,
                                    ( pArea->lpCurTag->TagEOF || fOut ) ? 0 :
                                    pArea->lpCurTag->CurKeyInfo->rec );
   }
   else /* if( lToSkip < 0 ) */
   {
      while( ! fOut && ! pArea->lpCurTag->TagBOF && lToSkip++ < 0 )
      {
         zh_nsxTagSkipPrev( pArea->lpCurTag );
      }
      if( fOut || pArea->lpCurTag->TagBOF )
      {
         zh_nsxTagGoTop( pArea->lpCurTag );
         fOut = ZH_TRUE;
      }
      retval = SELF_GOTO( &pArea->dbfarea.area, pArea->lpCurTag->TagEOF ? 0 :
                                          pArea->lpCurTag->CurKeyInfo->rec );
      pArea->dbfarea.area.fBof = fOut;
   }

   zh_nsxTagUnLockRead( pArea->lpCurTag );
   /* Update Bof and Eof flags */
#if 0
   if( fForward )
      pArea->dbfarea.area.fBof = ZH_FALSE;
   else
      pArea->dbfarea.area.fEof = ZH_FALSE;
#endif
   return retval;
}

/*
 * Flush _system_ buffers to disk
 */
static ZH_ERRCODE zh_nsxFlush( NSXAREAP pArea )
{
   ZH_ERRCODE errCode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_nsxFlush(%p)", ( void * ) pArea ) );

   errCode = SELF_GOCOLD( &pArea->dbfarea.area );
   if( errCode == ZH_SUCCESS )
   {
      errCode = SUPER_FLUSH( &pArea->dbfarea.area );

      if( zh_setGetHardCommit() )
      {
         LPNSXINDEX pIndex = pArea->lpIndexes;
         while( pIndex )
         {
            if( pIndex->fFlush && ! pIndex->fDelete )
            {
               zh_fileCommit( pIndex->pFile );
               pIndex->fFlush = ZH_FALSE;
            }
            pIndex = pIndex->pNext;
         }
      }
   }

   return errCode;
}

/*
 * Perform a write of WorkArea memory to the data store.
 */
static ZH_ERRCODE zh_nsxGoCold( NSXAREAP pArea )
{
   ZH_BOOL fRecordChanged = pArea->dbfarea.fRecordChanged;
   ZH_BOOL fAppend = pArea->dbfarea.fAppend;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_nsxGoCold(%p)", ( void * ) pArea ) );

   if( SUPER_GOCOLD( &pArea->dbfarea.area ) == ZH_SUCCESS )
   {
      if( fRecordChanged || pArea->fIdxAppend )
      {
         if( fAppend && pArea->dbfarea.fShared )
         {
            if( pArea->fIdxAppend )
               zh_errInternal( 9999, "zh_nsxGoCold: multiple appending without GOCOLD.", NULL, NULL );
            pArea->fIdxAppend = ZH_TRUE;
         }
         else
         {
            LPNSXINDEX pIndex = pArea->lpIndexes;
            LPTAGINFO pTag;
            LPKEYINFO pKey;
            ZH_BOOL fAdd, fDel, fLck = ZH_FALSE;
            int i;

            /* The pending relation may move the record pointer so we should
               disable them for KEY/FOR evaluation */
            LPDBRELINFO lpdbPendingRel = pArea->dbfarea.lpdbPendingRel;
            pArea->dbfarea.lpdbPendingRel = NULL;

            if( pArea->dbfarea.fShared )
            {
               fAppend = pArea->fIdxAppend;
               pArea->fIdxAppend = ZH_FALSE;
            }

            while( pIndex )
            {
               for( i = 0; i < pIndex->iTags; i++ )
               {
                  pTag = pIndex->lpTags[ i ];
                  if( pIndex->fReadonly || pTag->Custom || ! pTag->HeadBlock ||
                      ( fAppend && pTag->ChgOnly ) )
                     continue;

                  pKey = zh_nsxEvalKey( NULL, pTag );

                  fAdd = ( pTag->pForItem == NULL ||
                           zh_nsxEvalCond( pArea, pTag->pForItem, ZH_TRUE ) );
                  if( fAppend )
                  {
                     fDel = ZH_FALSE;
                  }
                  else
                  {
                     if( zh_nsxValCompare( pTag, pKey->val, pTag->KeyLength,
                                           pTag->HotKeyInfo->val, pTag->KeyLength,
                                           NSX_CMP_EXACT ) == 0 )
                     {
                        if( pTag->HotFor ? fAdd : ! fAdd )
                           fAdd = fDel = ZH_FALSE;
                        else
                           fDel = ! fAdd;
                     }
                     else
                     {
                        fDel = pTag->HotFor || pTag->Partial;
                     }
                  }
                  if( fDel || fAdd )
                  {
                     if( ! fLck )
                     {
                        if( ! zh_nsxIndexLockWrite( pIndex, ZH_TRUE ) )
                        {
                           zh_nsxKeyFree( pKey );
                           break;
                        }
                        fLck = ZH_TRUE;
                     }
                     if( ! pTag->HeadBlock || ! zh_nsxTagHeaderCheck( pTag ) )
                        fAdd = fDel = ZH_FALSE;
                     if( fDel )
                     {
                        if( zh_nsxTagKeyDel( pTag, pTag->HotKeyInfo ) )
                        {
                           if( ! pIndex->fShared && pTag->keyCount &&
                               zh_nsxKeyInScope( pTag, pTag->HotKeyInfo ) )
                              pTag->keyCount--;
                        }
                        else
                        {
                           if( pTag->ChgOnly )
                              fAdd = ZH_FALSE;
                           else if( ! pTag->Partial && ! pTag->UniqueKey )
                              zh_nsxCorruptError( pTag->pIndex );
                        }
                     }
                     if( fAdd )
                     {
                        if( zh_nsxTagKeyAdd( pTag, pKey ) )
                        {
                           if( ! pIndex->fShared && pTag->keyCount &&
                               zh_nsxKeyInScope( pTag, pKey ) )
                              pTag->keyCount++;
                        }
                     }
                  }
                  zh_nsxKeyFree( pKey );
               }
               if( fLck )
               {
                  zh_nsxIndexUnLockWrite( pIndex );
                  fLck = ZH_FALSE;
               }
               pIndex = pIndex->pNext;
            }

            /* Restore disabled pending relation */
            pArea->dbfarea.lpdbPendingRel = lpdbPendingRel;
         }
      }
      return ZH_SUCCESS;
   }
   return ZH_FAILURE;
}

/*
 * Mark the WorkArea data buffer as hot.
 */
static ZH_ERRCODE zh_nsxGoHot( NSXAREAP pArea )
{
   ZH_ERRCODE errCode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_nsxGoHot(%p)", ( void * ) pArea ) );

   errCode = SUPER_GOHOT( &pArea->dbfarea.area );
   if( errCode == ZH_SUCCESS )
   {
      if( ! pArea->fIdxAppend )
      {
         LPNSXINDEX pIndex = pArea->lpIndexes;
         LPTAGINFO pTag;
         int i;

         while( pIndex )
         {
            if( ! pIndex->fReadonly )
            {
               for( i = 0; i < pIndex->iTags; i++ )
               {
                  pTag = pIndex->lpTags[ i ];
                  if( ! pTag->Custom )
                  {
                     pTag->HotKeyInfo = zh_nsxEvalKey( pTag->HotKeyInfo, pTag );
                     pTag->HotFor = ( pTag->pForItem == NULL ||
                                 zh_nsxEvalCond( pArea, pTag->pForItem, ZH_TRUE ) );
                  }
               }
            }
            pIndex = pIndex->pNext;
         }
      }
      return ZH_SUCCESS;
   }
   return errCode;
}

/*
 * Close the table in the WorkArea.
 */
static ZH_ERRCODE zh_nsxClose( NSXAREAP pArea )
{
   ZH_ERRCODE errCode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_nsxClose(%p)", ( void * ) pArea ) );

   if( SELF_GOCOLD( &pArea->dbfarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   errCode = SUPER_CLOSE( &pArea->dbfarea.area );

   if( errCode == ZH_SUCCESS )
   {
      if( pArea->pSort )
      {
         zh_nsxSortFree( pArea->pSort, ZH_TRUE );
         pArea->pSort = NULL;
      }

      SELF_ORDLSTCLEAR( &pArea->dbfarea.area );

      /* close also production indexes if any */
      while( pArea->lpIndexes )
      {
         LPNSXINDEX pIndex = pArea->lpIndexes;
         pArea->lpIndexes = pIndex->pNext;
         zh_nsxIndexFree( pIndex );
      }
   }

   return errCode;
}

/*
 * Retrieve the size of the WorkArea structure.
 */
static ZH_ERRCODE zh_nsxStructSize( NSXAREAP pArea, ZH_USHORT * uiSize )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_nsxStructSize(%p, %p)", ( void * ) pArea, ( void * ) uiSize ) );
   ZH_SYMBOL_UNUSED( pArea );

   *uiSize = sizeof( NSXAREA );
   return ZH_SUCCESS;
}

/*
 * Open a data store in the WorkArea.
 */
static ZH_ERRCODE zh_nsxOpen( NSXAREAP pArea, LPDBOPENINFO pOpenInfo )
{
   ZH_ERRCODE errCode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_nsxOpen(%p, %p)", ( void * ) pArea, ( void * ) pOpenInfo ) );

   if( ! pArea->dbfarea.bLockType )
   {
      PZH_ITEM pItem = zh_itemNew( NULL );
      errCode = SELF_INFO( &pArea->dbfarea.area, DBI_LOCKSCHEME, pItem );
      if( errCode != ZH_SUCCESS )
      {
         zh_itemRelease( pItem );
         return errCode;
      }
      pArea->dbfarea.bLockType = ( ZH_BYTE ) zh_itemGetNI( pItem );
      zh_itemRelease( pItem );
      if( pArea->dbfarea.bLockType == 0 )
         pArea->dbfarea.bLockType = DB_DBFLOCK_CLIPPER;
   }

   errCode = SUPER_OPEN( &pArea->dbfarea.area, pOpenInfo );

   if( errCode == ZH_SUCCESS && ( DBFAREA_DATA( &pArea->dbfarea )->fStrictStruct ?
                               pArea->dbfarea.fHasTags : zh_setGetAutOpen() ) )
   {
      char szFileName[ ZH_PATH_MAX ];

      zh_nsxCreateFName( pArea, NULL, NULL, szFileName, NULL );
      /* CL5.2 DBFCDX and Six3 CDX/NSX RDDs looking for
         production indexes respect SET PATH but Ziher in
         core DBF* index RDDs is CL5.3/COMIX compatible and
         looks for production indexes only in the directory
         where DBF file is located and only SIXCDX Ziher
         RDD is CL5.2/Six3 compatible [druzus] */
      if( zh_fileExists( szFileName, NULL ) ||
          DBFAREA_DATA( &pArea->dbfarea )->fStrictStruct )
      {
         DBORDERINFO pOrderInfo;

         pOrderInfo.itmResult = zh_itemPutNI( NULL, 0 );
         pOrderInfo.atomBagName = zh_itemPutC( NULL, szFileName );
         pOrderInfo.itmNewVal = NULL;
         pOrderInfo.itmOrder  = NULL;
         errCode = SELF_ORDLSTADD( &pArea->dbfarea.area, &pOrderInfo );
         if( errCode == ZH_SUCCESS )
         {
            pOrderInfo.itmOrder  = zh_itemPutNI( NULL, zh_setGetAutOrder() );
            errCode = SELF_ORDLSTFOCUS( &pArea->dbfarea.area, &pOrderInfo );
            zh_itemRelease( pOrderInfo.itmOrder );
            if( errCode == ZH_SUCCESS )
               errCode = SELF_GOTOP( &pArea->dbfarea.area );
         }
         zh_itemRelease( pOrderInfo.atomBagName );
         zh_itemRelease( pOrderInfo.itmResult );
      }
   }

   return errCode;
}

static ZH_ERRCODE zh_nsxPack( NSXAREAP pArea )
{
   ZH_ERRCODE errCode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_nsxPack(%p)", ( void * ) pArea ) );

   errCode = SUPER_PACK( &pArea->dbfarea.area );
   if( errCode == ZH_SUCCESS )
      return SELF_ORDLSTREBUILD( &pArea->dbfarea.area );

   return errCode;
}

static ZH_ERRCODE zh_nsxZap( NSXAREAP pArea )
{
   ZH_ERRCODE errCode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_nsxZap(%p)", ( void * ) pArea ) );

   errCode = SUPER_ZAP( &pArea->dbfarea.area );
   if( errCode == ZH_SUCCESS )
      return SELF_ORDLSTREBUILD( &pArea->dbfarea.area );

   return errCode;
}

static ZH_ERRCODE zh_nsxOrderCreate( NSXAREAP pArea, LPDBORDERCREATEINFO pOrderInfo )
{
   PZH_ITEM pResult, pKeyExp, pForExp = NULL;
   int iLen, iTag;
   char szFileName[ ZH_PATH_MAX ], szTagName[ NSX_TAGNAME + 1 ];
   const char * szKey, * szFor = NULL;
   LPNSXINDEX pIndex, * pIndexPtr;
   LPTAGINFO pTag = NULL;
   ZH_ERRCODE errCode;
   ZH_ULONG ulRecNo;
   ZH_BOOL fNewFile, fProd, fLocked = ZH_FALSE, fAscend = ZH_TRUE, fCustom = ZH_FALSE,
        fTemporary = ZH_FALSE, fExclusive = ZH_FALSE;
   ZH_BYTE bType, bTrail;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_nsxOrderCreate(%p, %p)", ( void * ) pArea, ( void * ) pOrderInfo ) );

   errCode = SELF_GOCOLD( &pArea->dbfarea.area );
   if( errCode != ZH_SUCCESS )
      return errCode;

   if( pArea->dbfarea.lpdbPendingRel )
   {
      errCode = SELF_FORCEREL( &pArea->dbfarea.area );
      if( errCode != ZH_SUCCESS )
         return errCode;
   }

   szKey = zh_itemGetCPtr( pOrderInfo->abExpr );
   /* If we have a codeblock for the expression, use it */
   if( pOrderInfo->itmCobExpr )
      pKeyExp = zh_itemNew( pOrderInfo->itmCobExpr );
   else /* Otherwise, try compiling the key expression string */
   {
      errCode = SELF_COMPILE( &pArea->dbfarea.area, szKey );
      if( errCode != ZH_SUCCESS )
         return errCode;
      pKeyExp = pArea->dbfarea.area.valResult;
      pArea->dbfarea.area.valResult = NULL;
   }

   /* Get a blank record before testing expression */
   ulRecNo = pArea->dbfarea.ulRecNo;
   errCode = SELF_GOTO( &pArea->dbfarea.area, 0 );
   if( errCode != ZH_SUCCESS )
      return errCode;

   errCode = SELF_EVALBLOCK( &pArea->dbfarea.area, pKeyExp );
   if( errCode != ZH_SUCCESS )
   {
      zh_vmDestroyBlockOrMacro( pKeyExp );
      SELF_GOTO( &pArea->dbfarea.area, ulRecNo );
      return errCode;
   }
   pResult = pArea->dbfarea.area.valResult;
   pArea->dbfarea.area.valResult = NULL;

   bTrail = '\0';
   bType = zh_nsxItemType( pResult );
   switch( bType )
   {
      case 'N':
      case 'D':
      case 'T':
         iLen = 8;
         break;
      case 'L':
         iLen = 1;
         break;
      case 'C':
         iLen = ( ZH_INTCAST ) zh_itemGetCLen( pResult );
         if( iLen > NSX_MAXKEYLEN )
            iLen = NSX_MAXKEYLEN;
         bTrail = ' ';
         break;
      default:
         bType = 'U';
         iLen = 0;
   }
   zh_itemRelease( pResult );

   /* Make sure KEY has proper type and iLen is not 0 */
   if( iLen == 0 )
   {
      zh_vmDestroyBlockOrMacro( pKeyExp );
      SELF_GOTO( &pArea->dbfarea.area, ulRecNo );
      zh_nsxErrorRT( pArea, bType == 'U' ? EG_DATATYPE : EG_DATAWIDTH,
                     EDBF_INVALIDKEY, NULL, 0, 0, NULL );
      return ZH_FAILURE;
   }

   if( pArea->dbfarea.area.lpdbOrdCondInfo )
   {
      fAscend = ! pArea->dbfarea.area.lpdbOrdCondInfo->fDescending;
      fCustom = pArea->dbfarea.area.lpdbOrdCondInfo->fCustom;
      fTemporary = pArea->dbfarea.area.lpdbOrdCondInfo->fTemporary;
      fExclusive = pArea->dbfarea.area.lpdbOrdCondInfo->fExclusive;
      /* Check conditional expression */
      szFor = pArea->dbfarea.area.lpdbOrdCondInfo->abFor;
      if( pArea->dbfarea.area.lpdbOrdCondInfo->itmCobFor )
         /* If we have a codeblock for the conditional expression, use it */
         pForExp = zh_itemNew( pArea->dbfarea.area.lpdbOrdCondInfo->itmCobFor );
      else if( szFor )
      {
         /* Otherwise, try compiling the conditional expression string */
         errCode = SELF_COMPILE( &pArea->dbfarea.area, szFor );
         if( errCode != ZH_SUCCESS )
         {
            zh_vmDestroyBlockOrMacro( pKeyExp );
            SELF_GOTO( &pArea->dbfarea.area, ulRecNo );
            return errCode;
         }
         pForExp = pArea->dbfarea.area.valResult;
         pArea->dbfarea.area.valResult = NULL;
      }
   }

   if( pArea->dbfarea.fTemporary )
      fTemporary = ZH_TRUE;

   /* Test conditional expression */
   if( pForExp )
   {
      ZH_BOOL fOK;

      errCode = SELF_EVALBLOCK( &pArea->dbfarea.area, pForExp );
      if( errCode != ZH_SUCCESS )
      {
         zh_vmDestroyBlockOrMacro( pKeyExp );
         zh_vmDestroyBlockOrMacro( pForExp );
         SELF_GOTO( &pArea->dbfarea.area, ulRecNo );
         return errCode;
      }
      fOK = zh_itemType( pArea->dbfarea.area.valResult ) & ZH_IT_LOGICAL;
      zh_itemRelease( pArea->dbfarea.area.valResult );
      pArea->dbfarea.area.valResult = NULL;
      if( ! fOK )
      {
         zh_vmDestroyBlockOrMacro( pKeyExp );
         zh_vmDestroyBlockOrMacro( pForExp );
         SELF_GOTO( &pArea->dbfarea.area, ulRecNo );
         zh_nsxErrorRT( pArea, EG_DATATYPE, EDBF_INVALIDFOR, NULL, 0, 0, NULL );
         return ZH_FAILURE;
      }
   }

   SELF_GOTO( &pArea->dbfarea.area, ulRecNo );

   /*
    * abBagName -> cBag, atomBagName -> cTag
    * The following scheme implemented:
    * 1. abBagName == NULL   -> add the Tag to the structural index
    * 2. atomBagName == NULL -> overwrite any index file of abBagName
    * 3. add the Tag to index file
    */
   fNewFile = ! pOrderInfo->atomBagName || ! pOrderInfo->atomBagName[ 0 ];
   zh_nsxCreateFName( pArea, pOrderInfo->abBagName,
                      &fProd, szFileName, szTagName );
   if( ! fNewFile )
      zh_strncpyUpperTrim( szTagName, pOrderInfo->atomBagName, NSX_TAGNAME );

   pIndex = zh_nsxFindBag( pArea, szFileName );
   if( pIndex )
   {
      if( pIndex->fReadonly )
      {
         zh_vmDestroyBlockOrMacro( pKeyExp );
         if( pForExp != NULL )
            zh_vmDestroyBlockOrMacro( pForExp );
         zh_nsxErrorRT( pArea, EG_READONLY, EDBF_READONLY, pIndex->IndexName, 0, 0, NULL );
         return ZH_FAILURE;
      }
#if 0 /* enable this code if you want to forbid tag deleting in shared mode */
      else if( pIndex->fShared )
      {
         zh_vmDestroyBlockOrMacro( pKeyExp );
         if( pForExp != NULL )
            zh_vmDestroyBlockOrMacro( pForExp );
         zh_nsxErrorRT( pArea, EG_SHARED, EDBF_SHARED, pIndex->IndexName, 0, 0, NULL );
         return ZH_FAILURE;
      }
#endif
   }
   else
   {
      PZH_FILE pFile;
      ZH_BOOL bRetry, fShared = pArea->dbfarea.fShared && ! fTemporary && ! fExclusive;
      PZH_ITEM pError = NULL;
      char szSpFile[ ZH_PATH_MAX ];

      do
      {
         if( fTemporary )
         {
            pFile = zh_fileCreateTemp( NULL, NULL, FC_NORMAL, szSpFile );
            fNewFile = ZH_TRUE;
         }
         else
         {
            pFile = zh_fileExtOpen( szFileName, NULL, FO_READWRITE |
                                    ( fShared ? FO_DENYNONE : FO_EXCLUSIVE ) |
                                    ( fNewFile ? FXO_TRUNCATE : FXO_APPEND ) |
                                    FXO_DEFAULTS | FXO_SHARELOCK | FXO_COPYNAME |
                                    FXO_NOSEEKPOS,
                                    NULL, pError );
         }
         if( ! pFile )
            bRetry = zh_nsxErrorRT( pArea, EG_CREATE, EDBF_CREATE, szFileName,
                                    zh_fsError(), EF_CANRETRY | EF_CANDEFAULT,
                                    &pError ) == E_RETRY;
         else
         {
            bRetry = ZH_FALSE;
            if( ! fNewFile )
               fNewFile = ( zh_fileSize( pFile ) == 0 );
         }
      }
      while( bRetry );

      if( pError )
         zh_errRelease( pError );

      if( ! pFile )
      {
         zh_vmDestroyBlockOrMacro( pKeyExp );
         if( pForExp != NULL )
            zh_vmDestroyBlockOrMacro( pForExp );
         return ZH_FAILURE;
      }

      pIndex = zh_nsxIndexNew( pArea );
      pIndex->IndexName = zh_strdup( szFileName );
      pIndex->fReadonly = ZH_FALSE;
      pIndex->fShared = fShared;
      pIndex->pFile = pFile;
      pIndex->fDelete = fTemporary;
      if( fTemporary )
         pIndex->RealName = zh_strdup( szSpFile );
      else
         pIndex->Production = fProd;

      if( ! fNewFile )
      {
         if( ! zh_nsxIndexLockWrite( pIndex, ZH_TRUE ) )
            errCode = ZH_FAILURE;
         else
         {
            errCode = zh_nsxIndexLoad( pIndex );
            if( errCode != ZH_SUCCESS )
               zh_nsxIndexUnLockWrite( pIndex );
            else
               fLocked = ZH_TRUE;
         }
         if( errCode != ZH_SUCCESS )
         {
            zh_nsxIndexFree( pIndex );
            zh_vmDestroyBlockOrMacro( pKeyExp );
            if( pForExp != NULL )
               zh_vmDestroyBlockOrMacro( pForExp );
            zh_nsxErrorRT( pArea, EG_CORRUPTION, EDBF_CORRUPT, szFileName, 0, 0, NULL );
            return errCode;
         }
      }
      else
      {
         pIndex->LargeFile = ( pIndex->pArea->dbfarea.bLockType == DB_DBFLOCK_HB64 );
      }

      pIndexPtr = &pArea->lpIndexes;
      while( *pIndexPtr )
         pIndexPtr = &( *pIndexPtr )->pNext;
      *pIndexPtr = pIndex;
   }

   if( ! fLocked && ! zh_nsxIndexLockWrite( pIndex, ! fNewFile ) )
   {
      errCode = ZH_FAILURE;
   }

   iTag = zh_nsxFindTagByName( pIndex, szTagName );
   if( errCode == ZH_SUCCESS && iTag == 0 && pIndex->iTags == NSX_MAXTAGS )
   {
      zh_nsxIndexUnLockWrite( pIndex );
      zh_vmDestroyBlockOrMacro( pKeyExp );
      if( pForExp != NULL )
         zh_vmDestroyBlockOrMacro( pForExp );
      zh_nsxErrorRT( pArea, EG_LIMIT, EDBF_LIMITEXCEEDED, pIndex->IndexName, 0, 0, NULL );
      errCode = ZH_FAILURE;
   }

   if( errCode == ZH_SUCCESS )
   {
      pTag = zh_nsxTagNew( pIndex, szTagName,
                           szKey, pKeyExp, bType, ( ZH_USHORT ) iLen, bTrail,
                           szFor, pForExp,
                           fAscend, pOrderInfo->fUnique, fCustom );
      pTag->Template = zh_nsxIsTemplateFunc( pTag->KeyExpr );
      pTag->Partial = ( pArea->dbfarea.area.lpdbOrdCondInfo && ! pArea->dbfarea.area.lpdbOrdCondInfo->fAll );

      if( fNewFile )
      {
         while( pIndex->iTags )
            zh_nsxTagDelete( pIndex->lpTags[ 0 ] );
         zh_nsxIndexTrunc( pIndex );
         iTag = 0;
      }

      if( iTag )
      {
         pTag->HeadBlock = pIndex->lpTags[ iTag - 1 ]->HeadBlock;
         if( zh_nsxTagHeaderCheck( pIndex->lpTags[ iTag - 1 ] ) &&
             ! zh_nsxTagPagesFree( pIndex->lpTags[ iTag - 1 ],
                                   pIndex->lpTags[ iTag - 1 ]->RootBlock ) )
         {
            errCode = ZH_FAILURE;
         }
         else
         {
            pTag->uiNumber = pIndex->lpTags[ iTag - 1 ]->uiNumber;
            zh_nsxTagFree( pIndex->lpTags[ iTag - 1 ] );
            pIndex->lpTags[ iTag - 1 ] = pTag;
         }
      }
      else
      {
         zh_nsxTagAdd( pIndex, pTag );
         zh_nsxIndexTagAdd( pIndex, pTag );
      }

      if( errCode == ZH_SUCCESS )
      {
         pIndex->Update = pIndex->Changed = pTag->HdrChanged = ZH_TRUE;
         errCode = zh_nsxTagCreate( pTag, ZH_FALSE );
      }
      zh_nsxIndexUnLockWrite( pIndex );
   }

   pIndexPtr = &pArea->lpIndexes;
   while( *pIndexPtr && *pIndexPtr != pIndex )
      pIndexPtr = &( *pIndexPtr )->pNext;

   /* It should not happen, reentrance? */
   if( ! *pIndexPtr )
      return ZH_FAILURE;

   if( errCode != ZH_SUCCESS )
   {
      /* TODO: free only new indexes */
      *pIndexPtr = pIndex->pNext;
      zh_nsxIndexFree( pIndex );
      return errCode;
   }

   if( ! pArea->dbfarea.area.lpdbOrdCondInfo || ! pArea->dbfarea.area.lpdbOrdCondInfo->fAdditive )
   {
      *pIndexPtr = pIndex->pNext;
      pIndex->pNext = NULL;
      SELF_ORDLSTCLEAR( &pArea->dbfarea.area );
      pIndexPtr = &pArea->lpIndexes;
      while( *pIndexPtr )
         pIndexPtr = &( *pIndexPtr )->pNext;
      *pIndexPtr = pIndex;
      pArea->fSetTagNumbers = ZH_TRUE;
   }
   if( pIndex->Production && ! pArea->dbfarea.fHasTags &&
       ( DBFAREA_DATA( &pArea->dbfarea )->fStrictStruct || zh_setGetAutOpen() ) )
   {
      pArea->dbfarea.fHasTags = ZH_TRUE;
      if( ! pArea->dbfarea.fReadonly && ( pArea->dbfarea.dbfHeader.bHasTags & 0x01 ) == 0 )
         SELF_WRITEDBHEADER( &pArea->dbfarea.area );
   }
   pArea->lpCurTag = pTag;
   SELF_ORDSETCOND( &pArea->dbfarea.area, NULL );
   return SELF_GOTOP( &pArea->dbfarea.area );
}

static ZH_ERRCODE zh_nsxOrderDestroy( NSXAREAP pArea, LPDBORDERINFO pOrderInfo )
{
   ZH_ERRCODE errCode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_nsxOrderDestroy(%p, %p)", ( void * ) pArea, ( void * ) pOrderInfo ) );

   errCode = SELF_GOCOLD( &pArea->dbfarea.area );
   if( errCode != ZH_SUCCESS )
      return errCode;

   if( pArea->dbfarea.lpdbPendingRel )
   {
      errCode = SELF_FORCEREL( &pArea->dbfarea.area );
      if( errCode != ZH_SUCCESS )
         return errCode;
   }

   if( pOrderInfo->itmOrder )
   {
      LPTAGINFO pTag = zh_nsxFindTag( pArea, pOrderInfo->itmOrder, pOrderInfo->atomBagName );

      if( pTag )
      {
         LPNSXINDEX pIndex = pTag->pIndex;

         if( pIndex->iTags == 1 )
         {
            ZH_BOOL fProd = pIndex->Production;
            LPNSXINDEX * pIndexPtr = &pArea->lpIndexes;
            while( *pIndexPtr != pIndex )
               pIndexPtr = &( *pIndexPtr )->pNext;
            *pIndexPtr      = pIndex->pNext;
            pIndex->fDelete = ZH_TRUE;
            zh_nsxIndexFree( pIndex );
            if( fProd && pArea->dbfarea.fHasTags &&
                ( DBFAREA_DATA( &pArea->dbfarea )->fStrictStruct || zh_setGetAutOpen() ) )
            {
               pArea->dbfarea.fHasTags = ZH_FALSE;
               if( ! pArea->dbfarea.fReadonly && ( pArea->dbfarea.dbfHeader.bHasTags & 0x01 ) != 0 )
                  SELF_WRITEDBHEADER( &pArea->dbfarea.area );
            }
         }
         else if( pIndex->fReadonly )
         {
            zh_nsxErrorRT( pArea, EG_READONLY, EDBF_READONLY, pIndex->IndexName, 0, 0, NULL );
            return ZH_FAILURE;
         }
#if 0    /* enable this code if you want to forbid tag deleting in shared mode */
         else if( pIndex->fShared )
         {
            zh_nsxErrorRT( pArea, EG_SHARED, EDBF_SHARED, pIndex->IndexName, 0, 0, NULL );
            return ZH_FAILURE;
         }
#endif
         else if( ! zh_nsxIndexLockWrite( pIndex, ZH_TRUE ) )
         {
            return ZH_FAILURE;
         }
         else
         {
            errCode = zh_nsxTagSpaceFree( pTag );
            zh_nsxIndexUnLockWrite( pIndex );
         }
      }
   }

   return errCode;
}

static ZH_ERRCODE zh_nsxOrderInfo( NSXAREAP pArea, ZH_USHORT uiIndex, LPDBORDERINFO pInfo )
{
   LPTAGINFO pTag;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_nsxOrderInfo(%p, %hu, %p)", ( void * ) pArea, uiIndex, ( void * ) pInfo ) );

   switch( uiIndex )
   {
      case DBOI_STRICTREAD:
         pInfo->itmResult = zh_itemPutNil( pInfo->itmResult );
         return SELF_RDDINFO( SELF_RDDNODE( &pArea->dbfarea.area ), RDDI_STRICTREAD, 0, pInfo->itmResult );
      case DBOI_OPTIMIZE:
         pInfo->itmResult = zh_itemPutNil( pInfo->itmResult );
         return SELF_RDDINFO( SELF_RDDNODE( &pArea->dbfarea.area ), RDDI_OPTIMIZE, 0, pInfo->itmResult );
      case DBOI_AUTOOPEN:
         pInfo->itmResult = zh_itemPutNil( pInfo->itmResult );
         return SELF_RDDINFO( SELF_RDDNODE( &pArea->dbfarea.area ), RDDI_AUTOOPEN, 0, pInfo->itmResult );
      case DBOI_AUTOORDER:
         pInfo->itmResult = zh_itemPutNil( pInfo->itmResult );
         return SELF_RDDINFO( SELF_RDDNODE( &pArea->dbfarea.area ), RDDI_AUTOORDER, 0, pInfo->itmResult );
      case DBOI_AUTOSHARE:
         pInfo->itmResult = zh_itemPutNil( pInfo->itmResult );
         return SELF_RDDINFO( SELF_RDDNODE( &pArea->dbfarea.area ), RDDI_AUTOSHARE, 0, pInfo->itmResult );
      case DBOI_BAGEXT:
         pInfo->itmResult = zh_itemPutNil( pInfo->itmResult );
         return SELF_RDDINFO( SELF_RDDNODE( &pArea->dbfarea.area ), RDDI_ORDBAGEXT, 0, pInfo->itmResult );
      case DBOI_EVALSTEP:
         pInfo->itmResult = zh_itemPutNL( pInfo->itmResult,
                  pArea->dbfarea.area.lpdbOrdCondInfo ? pArea->dbfarea.area.lpdbOrdCondInfo->lStep : 0 );
         return ZH_SUCCESS;
      case DBOI_KEYSINCLUDED:
         pInfo->itmResult = zh_itemPutNL( pInfo->itmResult,
                       pArea->pSort ? pArea->pSort->ulTotKeys : 0 );
         return ZH_SUCCESS;
      case DBOI_I_TAGNAME:
         pInfo->itmResult = zh_itemPutC( pInfo->itmResult,
                      pArea->pSort ? pArea->pSort->pTag->TagName : NULL );
         return ZH_SUCCESS;
      case DBOI_I_BAGNAME:
         pInfo->itmResult = zh_itemPutC( pInfo->itmResult, pArea->pSort ?
                      pArea->pSort->pTag->pIndex->IndexName : NULL );
         return ZH_SUCCESS;
      case DBOI_ISREINDEX:
         pInfo->itmResult = zh_itemPutL( pInfo->itmResult,
                      pArea->pSort ? pArea->pSort->fReindex : ZH_FALSE );
         return ZH_SUCCESS;
      case DBOI_LOCKOFFSET:
      case DBOI_HPLOCKING:
      {
         ZH_DBFLOCKDATA lockData;

         zh_dbfLockIdxGetData( pArea->dbfarea.bLockType, &lockData );
         if( uiIndex == DBOI_LOCKOFFSET )
            pInfo->itmResult = zh_itemPutNInt( pInfo->itmResult, lockData.offset );
         else
            pInfo->itmResult = zh_itemPutL( pInfo->itmResult, lockData.size > 0 );
         return ZH_SUCCESS;
      }
      case DBOI_ORDERCOUNT:
      {
         int i;

         if( zh_itemGetCLen( pInfo->atomBagName ) > 0 )
         {
            LPNSXINDEX pIndex = zh_nsxFindBag( pArea,
                                       zh_itemGetCPtr( pInfo->atomBagName ) );
            i = pIndex ? pIndex->iTags : 0;
         }
         else
            i = zh_nsxTagCount( pArea );

         pInfo->itmResult = zh_itemPutNI( pInfo->itmResult, i );
         return ZH_SUCCESS;
      }
      case DBOI_BAGCOUNT:
      {
         int i = 0;
         LPNSXINDEX pIndex = pArea->lpIndexes;
         while( pIndex )
         {
            ++i;
            pIndex = pIndex->pNext;
         }
         pInfo->itmResult = zh_itemPutNI( pInfo->itmResult, i );
         return ZH_SUCCESS;
      }
      case DBOI_BAGNUMBER:
      {
         LPNSXINDEX pIndex = pArea->lpIndexes, pIndexSeek = NULL;
         int i = 0;

         if( zh_itemGetCLen( pInfo->atomBagName ) > 0 )
            pIndexSeek = zh_nsxFindBag( pArea,
                                        zh_itemGetCPtr( pInfo->atomBagName ) );
         else if( pArea->lpCurTag )
            pIndexSeek = pArea->lpCurTag->pIndex;

         if( pIndexSeek )
         {
            do
            {
               ++i;
               if( pIndex == pIndexSeek )
                  break;
               pIndex = pIndex->pNext;
            }
            while( pIndex );
         }
         pInfo->itmResult = zh_itemPutNI( pInfo->itmResult, pIndex ? i : 0 );
         return ZH_SUCCESS;
      }
      case DBOI_BAGORDER:
      {
         LPNSXINDEX pIndex = pArea->lpIndexes, pIndexSeek = NULL;
         int i = 0;

         if( zh_itemGetCLen( pInfo->atomBagName ) > 0 )
            pIndexSeek = zh_nsxFindBag( pArea,
                                        zh_itemGetCPtr( pInfo->atomBagName ) );
         else if( pArea->lpCurTag )
            pIndexSeek = pArea->lpCurTag->pIndex;

         if( pIndexSeek )
         {
            ++i;
            do
            {
               if( pIndex == pIndexSeek )
                  break;
               i += pIndex->iTags;
               pIndex = pIndex->pNext;
            }
            while( pIndex );
         }
         pInfo->itmResult = zh_itemPutNI( pInfo->itmResult, pIndex ? i : 0 );
         return ZH_SUCCESS;
      }
      case DBOI_RESETPOS:
         return ZH_SUCCESS;
   }

   if( SELF_GOCOLD( &pArea->dbfarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   pTag = zh_nsxFindTag( pArea, pInfo->itmOrder, pInfo->atomBagName );

   if( pTag )
   {
      switch( uiIndex )
      {
         case DBOI_CONDITION:
            pInfo->itmResult = zh_itemPutC( pInfo->itmResult, pTag->ForExpr );
            if( zh_itemType( pInfo->itmNewVal ) & ZH_IT_STRING )
            {
               const char * szForExpr = zh_itemGetCPtr( pInfo->itmNewVal );
               if( pTag->ForExpr ?
                   strncmp( pTag->ForExpr, szForExpr, NSX_MAXEXPLEN ) != 0 :
                   *szForExpr )
               {
                  PZH_ITEM pForItem = NULL;
                  ZH_BOOL fOK = *szForExpr == 0;
                  if( ! fOK )
                  {
                     if( SELF_COMPILE( &pArea->dbfarea.area, szForExpr ) == ZH_SUCCESS )
                     {
                        pForItem = pArea->dbfarea.area.valResult;
                        pArea->dbfarea.area.valResult = NULL;
                        if( SELF_EVALBLOCK( &pArea->dbfarea.area, pForItem ) == ZH_SUCCESS )
                        {
                           fOK = zh_itemType( pArea->dbfarea.area.valResult ) & ZH_IT_LOGICAL;
                           zh_itemRelease( pArea->dbfarea.area.valResult );
                           pArea->dbfarea.area.valResult = NULL;
                        }
                     }
                  }
                  if( fOK && zh_nsxTagLockWrite( pTag ) )
                  {
                     if( pTag->ForExpr )
                        zh_xfree( pTag->ForExpr );
                     if( pTag->pForItem )
                        zh_vmDestroyBlockOrMacro( pTag->pForItem );
                     if( pForItem )
                     {
                        pTag->ForExpr = zh_strndup( szForExpr, NSX_MAXEXPLEN );
                        pTag->pForItem = pForItem;
                        pForItem = NULL;
                     }
                     else
                     {
                        pTag->ForExpr = NULL;
                        pTag->pForItem = NULL;
                     }
                     pTag->Partial = ZH_TRUE;
                     pTag->HdrChanged = ZH_TRUE;
                     pTag->pIndex->Update = ZH_TRUE;
                     zh_nsxTagUnLockWrite( pTag );
                  }
                  if( pForItem )
                     zh_vmDestroyBlockOrMacro( pForItem );
               }
            }
            break;
         case DBOI_EXPRESSION:
            pInfo->itmResult = zh_itemPutC( pInfo->itmResult, pTag->KeyExpr );
            break;
         case DBOI_BAGNAME:
         {
            PZH_FNAME pFileName = zh_fsFNameSplit( pTag->pIndex->IndexName );
            pInfo->itmResult = zh_itemPutC( pInfo->itmResult, pFileName->szName );
            zh_xfree( pFileName );
            break;
         }
         case DBOI_NAME:
            pInfo->itmResult = zh_itemPutC( pInfo->itmResult, pTag->TagName );
            break;
         case DBOI_NUMBER:
            pInfo->itmResult = zh_itemPutNI( pInfo->itmResult, zh_nsxFindTagNum( pArea, pTag ) );
            break;
         case DBOI_FILEHANDLE:
            pInfo->itmResult = zh_itemPutNInt( pInfo->itmResult, ( ZH_NHANDLE )
                                               zh_fileHandle( pTag->pIndex->pFile ) );
            break;
         case DBOI_FULLPATH:
            pInfo->itmResult = zh_itemPutC( pInfo->itmResult, pTag->pIndex->IndexName );
            break;
         case DBOI_KEYCOUNT:
         case DBOI_KEYCOUNTRAW:
            pInfo->itmResult = zh_itemPutNL( pInfo->itmResult, zh_nsxOrdKeyCount( pTag ) );
            break;
         case DBOI_POSITION:
         case DBOI_KEYNORAW:
         /* case DBOI_RECNO: */
            if( zh_itemType( pInfo->itmNewVal ) & ZH_IT_NUMERIC )
               pInfo->itmResult = zh_itemPutL( pInfo->itmResult,
                  zh_nsxOrdKeyGoto( pTag, zh_itemGetNL( pInfo->itmNewVal ) ) );
            else
               pInfo->itmResult = zh_itemPutNL( pInfo->itmResult, zh_nsxOrdKeyNo( pTag ) );
            break;
         case DBOI_RELKEYPOS:
            if( zh_itemType( pInfo->itmNewVal ) & ZH_IT_NUMERIC )
               zh_nsxOrdSetRelKeyPos( pTag, zh_itemGetND( pInfo->itmNewVal ) );
            else
               pInfo->itmResult = zh_itemPutND( pInfo->itmResult, zh_nsxOrdGetRelKeyPos( pTag ) );
            break;
         case DBOI_ISCOND:
            pInfo->itmResult = zh_itemPutL( pInfo->itmResult, pTag->ForExpr != NULL );
            break;
         case DBOI_ISDESC:
            pInfo->itmResult = zh_itemPutL( pInfo->itmResult, pTag->fUsrDescend );
            if( zh_itemType( pInfo->itmNewVal ) & ZH_IT_LOGICAL )
               pTag->fUsrDescend = zh_itemGetL( pInfo->itmNewVal );
            break;
         case DBOI_UNIQUE:
            pInfo->itmResult = zh_itemPutL( pInfo->itmResult, pTag->UniqueKey );
            break;
         case DBOI_CUSTOM:
            if( ! pTag->Template &&
                ( zh_itemType( pInfo->itmNewVal ) & ZH_IT_LOGICAL ) )
            {
               ZH_BOOL fNewVal = zh_itemGetL( pInfo->itmNewVal );
               if( pTag->Custom ? ! fNewVal : fNewVal )
               {
                  if( zh_nsxTagLockWrite( pTag ) )
                  {
                     if( ! pTag->Template && ( pTag->Custom ? ! fNewVal : fNewVal ) )
                     {
                        pTag->Custom = fNewVal;
                        pTag->Partial = ZH_TRUE;
                        pTag->ChgOnly = ZH_FALSE;
                        pTag->HdrChanged = ZH_TRUE;
                     }
                     zh_nsxTagUnLockWrite( pTag );
                  }
               }
            }
            pInfo->itmResult = zh_itemPutL( pInfo->itmResult, pTag->Custom );
            break;
         case DBOI_CHGONLY:
            if( ! pTag->Custom &&
                ( zh_itemType( pInfo->itmNewVal ) & ZH_IT_LOGICAL ) )
            {
               ZH_BOOL fNewVal = zh_itemGetL( pInfo->itmNewVal );
               if( pTag->ChgOnly ? ! fNewVal : fNewVal )
               {
                  if( zh_nsxTagLockWrite( pTag ) )
                  {
                     if( ! pTag->Custom && ( pTag->ChgOnly ? ! fNewVal : fNewVal ) )
                     {
                        pTag->ChgOnly = fNewVal;
                        pTag->Partial = ZH_TRUE;
                        pTag->HdrChanged = ZH_TRUE;
                     }
                     zh_nsxTagUnLockWrite( pTag );
                  }
               }
            }
            pInfo->itmResult = zh_itemPutL( pInfo->itmResult, pTag->ChgOnly );
            break;
         case DBOI_TEMPLATE:
            if( pTag->Custom && ! pTag->Template &&
                zh_itemGetL( pInfo->itmNewVal ) )
            {
               if( zh_nsxTagLockWrite( pTag ) )
               {
                  if( pTag->Custom && ! pTag->Template )
                  {
                     pTag->Template = ZH_TRUE;
                     pTag->HdrChanged = ZH_TRUE;
                  }
                  zh_nsxTagUnLockWrite( pTag );
               }
            }
            pInfo->itmResult = zh_itemPutL( pInfo->itmResult, pTag->Template );
            break;
         case DBOI_MULTIKEY:
            if( pTag->Custom && ! pTag->MultiKey &&
                zh_itemGetL( pInfo->itmNewVal ) )
            {
               if( zh_nsxTagLockWrite( pTag ) )
               {
                  if( pTag->Custom && ! pTag->MultiKey )
                  {
                     pTag->MultiKey = ZH_TRUE;
                     pTag->HdrChanged = ZH_TRUE;
                  }
                  zh_nsxTagUnLockWrite( pTag );
               }
            }
            pInfo->itmResult = zh_itemPutL( pInfo->itmResult, pTag->MultiKey );
            break;
         case DBOI_PARTIAL:
            pInfo->itmResult = zh_itemPutL( pInfo->itmResult, pTag->Partial );
            break;
         case DBOI_SCOPETOP:
            if( pInfo->itmResult )
               zh_nsxTagGetScope( pTag, 0, pInfo->itmResult );
            if( pInfo->itmNewVal )
               zh_nsxTagSetScope( pTag, 0, pInfo->itmNewVal );
            break;
         case DBOI_SCOPEBOTTOM:
            if( pInfo->itmResult )
               zh_nsxTagGetScope( pTag, 1, pInfo->itmResult );
            if( pInfo->itmNewVal )
               zh_nsxTagSetScope( pTag, 1, pInfo->itmNewVal );
            break;
         case DBOI_SCOPESET:
            if( pInfo->itmNewVal )
            {
               zh_nsxTagSetScope( pTag, 0, pInfo->itmNewVal );
               zh_nsxTagSetScope( pTag, 1, pInfo->itmNewVal );
            }
            if( pInfo->itmResult )
               zh_itemClear( pInfo->itmResult );
            break;
         case DBOI_SCOPETOPCLEAR:
            if( pInfo->itmResult )
               zh_nsxTagGetScope( pTag, 0, pInfo->itmResult );
            zh_nsxTagClearScope( pTag, 0 );
            break;
         case DBOI_SCOPEBOTTOMCLEAR:
            if( pInfo->itmResult )
               zh_nsxTagGetScope( pTag, 1, pInfo->itmResult );
            zh_nsxTagClearScope( pTag, 1 );
            break;
         case DBOI_SCOPECLEAR:
            zh_nsxTagClearScope( pTag, 0 );
            zh_nsxTagClearScope( pTag, 1 );
            if( pInfo->itmResult )
               zh_itemClear( pInfo->itmResult );
            break;
         case DBOI_KEYADD:
            if( pTag->pIndex->fReadonly )
            {
               zh_nsxErrorRT( pArea, EG_READONLY, EDBF_READONLY,
                              pTag->pIndex->IndexName, 0, 0, NULL );
               return ZH_FAILURE;
            }
            if( pTag->Custom )
            {
               pInfo->itmResult = zh_itemPutL( pInfo->itmResult,
                            zh_nsxOrdKeyAdd( pTag, pInfo->itmNewVal ) );
            }
            else
            {
               zh_nsxErrorRT( pArea, 0, EDBF_NOTCUSTOM, NULL, 0, 0, NULL );
               return ZH_FAILURE;
            }
            break;
         case DBOI_KEYDELETE:
            if( pTag->pIndex->fReadonly )
            {
               zh_nsxErrorRT( pArea, EG_READONLY, EDBF_READONLY,
                              pTag->pIndex->IndexName, 0, 0, NULL );
               return ZH_FAILURE;
            }
            if( pTag->Custom )
            {
               pInfo->itmResult = zh_itemPutL( pInfo->itmResult,
                            zh_nsxOrdKeyDel( pTag, pInfo->itmNewVal ) );
            }
            else
            {
               zh_nsxErrorRT( pArea, 0, EDBF_NOTCUSTOM, NULL, 0, 0, NULL );
               return ZH_FAILURE;
            }
            break;
         case DBOI_KEYTYPE:
            {
               char szType[ 2 ];
               szType[ 0 ] = ( char ) pTag->KeyType;
               szType[ 1 ] = 0;
               pInfo->itmResult = zh_itemPutC( pInfo->itmResult, szType );
            }
            break;
         case DBOI_KEYSIZE:
            pInfo->itmResult = zh_itemPutNI( pInfo->itmResult, pTag->KeyLength );
            break;
         case DBOI_KEYDEC:
         /* there is no fixed number of decimal places for numeric keys
            in NSX format */
            pInfo->itmResult = zh_itemPutNI( pInfo->itmResult, 0 );
            break;
         case DBOI_KEYVAL:
            if( zh_nsxTagLockRead( pTag ) )
            {
               if( zh_nsxCurKeyRefresh( pTag ) )
                  pInfo->itmResult = zh_nsxKeyGetItem( pInfo->itmResult,
                                       pTag->CurKeyInfo, pTag, ZH_TRUE );
               else if( pInfo->itmResult )
                  zh_itemClear( pInfo->itmResult );
               zh_nsxTagUnLockRead( pTag );
            }
            break;
         case DBOI_SKIPUNIQUE:
            pInfo->itmResult = zh_itemPutL( pInfo->itmResult,
               zh_nsxOrdSkipUnique( pTag,
                        pInfo->itmNewVal && ZH_IS_NUMERIC( pInfo->itmNewVal ) ?
                        zh_itemGetNL( pInfo->itmNewVal ) : 1 ) );
            break;
         case DBOI_SKIPEVAL:
         case DBOI_SKIPEVALBACK:
            pInfo->itmResult = zh_itemPutL( pInfo->itmResult,
               zh_nsxOrdSkipEval( pTag, uiIndex == DBOI_SKIPEVAL, pInfo->itmNewVal ) );
            break;
         case DBOI_SKIPWILD:
         case DBOI_SKIPWILDBACK:
            pInfo->itmResult = zh_itemPutL( pInfo->itmResult,
               zh_nsxOrdSkipWild( pTag, uiIndex == DBOI_SKIPWILD, pInfo->itmNewVal ) );
            break;
         case DBOI_SKIPREGEX:
         case DBOI_SKIPREGEXBACK:
            pInfo->itmResult = zh_itemPutL( pInfo->itmResult,
               zh_nsxOrdSkipRegEx( pTag, uiIndex == DBOI_SKIPREGEX, pInfo->itmNewVal ) );
            break;
         case DBOI_FINDREC:
         case DBOI_FINDRECCONT:
            pInfo->itmResult = zh_itemPutL( pInfo->itmResult,
               zh_nsxOrdFindRec( pTag, zh_itemGetNL( pInfo->itmNewVal ),
                                 uiIndex == DBOI_FINDRECCONT ) );
            break;
         case DBOI_SCOPEEVAL:
            if( ( zh_itemType( pInfo->itmNewVal ) & ZH_IT_ARRAY ) &&
                zh_arrayLen( pInfo->itmNewVal ) == DBRMI_SIZE &&
                zh_arrayGetPtr( pInfo->itmNewVal, DBRMI_FUNCTION ) != NULL )
            {
               pInfo->itmResult = zh_itemPutNL( pInfo->itmResult,
                  zh_nsxOrdScopeEval( pTag, ( ZH_EVALSCOPE_FUNC )
                     zh_arrayGetPtr( pInfo->itmNewVal, DBRMI_FUNCTION ),
                     zh_arrayGetPtr( pInfo->itmNewVal, DBRMI_PARAM ),
                     zh_arrayGetItemPtr( pInfo->itmNewVal, DBRMI_LOVAL ),
                     zh_arrayGetItemPtr( pInfo->itmNewVal, DBRMI_HIVAL ) ) );
            }
            else
               pInfo->itmResult = zh_itemPutNI( pInfo->itmResult, 0 );
            break;
         case DBOI_UPDATECOUNTER:
            /* refresh update counter */
            if( zh_nsxIndexLockRead( pTag->pIndex ) )
               zh_nsxIndexUnLockRead( pTag->pIndex );
            pInfo->itmResult = zh_itemPutNInt( pInfo->itmResult, pTag->pIndex->Version );
            break;
         case DBOI_READLOCK:
            if( zh_itemType( pInfo->itmNewVal ) & ZH_IT_LOGICAL )
               pInfo->itmResult = zh_itemPutL( pInfo->itmResult,
                            zh_itemGetL( pInfo->itmNewVal ) ?
                                 zh_nsxIndexLockRead( pTag->pIndex ) :
                                 zh_nsxIndexUnLockRead( pTag->pIndex ) );
            else
               pInfo->itmResult = zh_itemPutL( pInfo->itmResult, pTag->pIndex->lockRead > 0 );
            break;
         case DBOI_WRITELOCK:
            if( zh_itemType( pInfo->itmNewVal ) & ZH_IT_LOGICAL )
               pInfo->itmResult = zh_itemPutL( pInfo->itmResult,
                            zh_itemGetL( pInfo->itmNewVal ) ?
                                 zh_nsxIndexLockWrite( pTag->pIndex, ZH_TRUE ) :
                                 zh_nsxIndexUnLockWrite( pTag->pIndex ) );
            else
               pInfo->itmResult = zh_itemPutL( pInfo->itmResult, pTag->pIndex->lockWrite > 0 );
            break;
         case DBOI_ISSORTRECNO:
         case DBOI_ISMULTITAG:
            pInfo->itmResult = zh_itemPutL( pInfo->itmResult, ZH_TRUE );
            break;
         case DBOI_LARGEFILE:
            pInfo->itmResult = zh_itemPutL( pInfo->itmResult, pTag->pIndex->LargeFile );
            break;
         case DBOI_SHARED:
            pInfo->itmResult = zh_itemPutL( pInfo->itmResult, pTag->pIndex->fShared );
            if( zh_itemType( pInfo->itmNewVal ) & ZH_IT_LOGICAL )
               pTag->pIndex->fShared = zh_itemGetL( pInfo->itmNewVal );
            break;
         case DBOI_ISREADONLY:
            pInfo->itmResult = zh_itemPutL( pInfo->itmResult, pTag->pIndex->fReadonly );
            break;
         case DBOI_INDEXTYPE:
            pInfo->itmResult = zh_itemPutNI( pInfo->itmResult,
               pTag->pIndex->iTags > 1 ? DBOI_TYPE_COMPOUND : DBOI_TYPE_COMPACT );
            break;
         case DBOI_INDEXPAGESIZE:
            pInfo->itmResult = zh_itemPutNI( pInfo->itmResult, NSX_PAGELEN );
            break;
      }
   }
   else if( pInfo->itmResult )
   {
      switch( uiIndex )
      {
         case DBOI_KEYCOUNT:
         case DBOI_KEYCOUNTRAW:
         {
            ZH_ULONG ulRecCount = 0;
            SELF_RECCOUNT( &pArea->dbfarea.area, &ulRecCount );
            zh_itemPutNInt( pInfo->itmResult, ulRecCount );
            break;
         }
         case DBOI_POSITION:
         case DBOI_KEYNORAW:
         /* case DBOI_RECNO: */
            if( pInfo->itmNewVal && zh_itemType( pInfo->itmNewVal ) & ZH_IT_NUMERIC )
               zh_itemPutL( pInfo->itmResult, SELF_GOTO( &pArea->dbfarea.area,
                              zh_itemGetNL( pInfo->itmNewVal ) ) == ZH_SUCCESS );
            else
               SELF_RECID( &pArea->dbfarea.area, pInfo->itmResult );
            break;
         case DBOI_RELKEYPOS:
            if( zh_itemType( pInfo->itmNewVal ) & ZH_IT_NUMERIC )
            {
               double dPos = zh_itemGetND( pInfo->itmNewVal );
               LPTAGINFO pSavedTag = pArea->lpCurTag;
               pArea->lpCurTag = NULL;
               if( dPos >= 1.0 )
               {
                  SELF_GOBOTTOM( &pArea->dbfarea.area );
               }
               else if( dPos <= 0.0 )
               {
                  SELF_GOTOP( &pArea->dbfarea.area );
               }
               else
               {
                  ZH_ULONG ulRecCount, ulRecNo;
                  SELF_RECCOUNT( &pArea->dbfarea.area, &ulRecCount );
                  ulRecNo = ( ZH_ULONG ) dPos * ulRecCount + 1;
                  if( ulRecNo >= ulRecCount )
                     ulRecNo = ulRecCount;
                  if( SELF_GOTO( &pArea->dbfarea.area, ulRecNo ) == ZH_SUCCESS &&
                      SELF_SKIPFILTER( &pArea->dbfarea.area, 1 ) == ZH_SUCCESS &&
                      pArea->dbfarea.area.fEof )
                     SELF_GOTOP( &pArea->dbfarea.area );
               }
               pArea->lpCurTag = pSavedTag;
            }
            else
            {
               ZH_ULONG ulRecNo = 0, ulRecCount = 0;
               double dPos = 0.0;
               /* resolve any pending relations */
               if( SELF_RECNO( &pArea->dbfarea.area, &ulRecNo ) == ZH_SUCCESS )
               {
                  if( ! pArea->dbfarea.fPositioned )
                  {
                     if( ulRecNo > 1 )
                        dPos = 1.0;
                  }
                  else
                  {
                     SELF_RECCOUNT( &pArea->dbfarea.area, &ulRecCount );
                     if( ulRecCount != 0 )
                        dPos = ( 0.5 + ulRecNo ) / ulRecCount;
                  }
               }
               zh_itemPutND( pInfo->itmResult, dPos );
            }
            break;
         case DBOI_SKIPUNIQUE:
            zh_itemPutL( pInfo->itmResult, SELF_SKIP( &pArea->dbfarea.area,
                        pInfo->itmNewVal && ZH_IS_NUMERIC( pInfo->itmNewVal ) ?
                        zh_itemGetNL( pInfo->itmNewVal ) : 1 ) == ZH_SUCCESS );
            break;
         case DBOI_SKIPEVAL:
         case DBOI_SKIPEVALBACK:
         case DBOI_SKIPWILD:
         case DBOI_SKIPWILDBACK:
         case DBOI_SKIPREGEX:
         case DBOI_SKIPREGEXBACK:
         case DBOI_FINDREC:
         case DBOI_FINDRECCONT:
            SELF_GOTO( &pArea->dbfarea.area, 0 );
            zh_itemPutL( pInfo->itmResult, ZH_FALSE );
            break;
         case DBOI_ISCOND:
         case DBOI_ISDESC:
         case DBOI_UNIQUE:
         case DBOI_CUSTOM:
         case DBOI_KEYADD:
         case DBOI_KEYDELETE:

         case DBOI_ISSORTRECNO:
         case DBOI_ISMULTITAG:
         case DBOI_LARGEFILE:
         case DBOI_TEMPLATE:
         case DBOI_MULTIKEY:
         case DBOI_PARTIAL:
         case DBOI_CHGONLY:
         case DBOI_SHARED:
         case DBOI_ISREADONLY:
         case DBOI_WRITELOCK:
         case DBOI_READLOCK:
            zh_itemPutL( pInfo->itmResult, ZH_FALSE );
            break;
         case DBOI_KEYVAL:
         case DBOI_SCOPETOP:
         case DBOI_SCOPEBOTTOM:
         case DBOI_SCOPESET:
         case DBOI_SCOPETOPCLEAR:
         case DBOI_SCOPEBOTTOMCLEAR:
         case DBOI_SCOPECLEAR:
            zh_itemClear( pInfo->itmResult );
            break;
         case DBOI_KEYSIZE:
         case DBOI_KEYDEC:
         case DBOI_NUMBER:
         case DBOI_ORDERCOUNT:
         case DBOI_SCOPEEVAL:
         case DBOI_UPDATECOUNTER:
         case DBOI_INDEXPAGESIZE:
            zh_itemPutNI( pInfo->itmResult, 0 );
            break;
         case DBOI_FILEHANDLE:
            zh_itemPutNInt( pInfo->itmResult, ( ZH_NHANDLE ) FS_ERROR );
            break;
         case DBOI_INDEXTYPE:
            zh_itemPutNI( pInfo->itmResult, DBOI_TYPE_UNDEF );
            break;
         case DBOI_BAGNAME:
         case DBOI_CONDITION:
         case DBOI_EXPRESSION:
         case DBOI_FULLPATH:
         case DBOI_NAME:
         case DBOI_KEYTYPE:
            zh_itemPutC( pInfo->itmResult, NULL );
            break;
         default:
            zh_itemClear( pInfo->itmResult );
      }
   }
   return ZH_SUCCESS;
}

static ZH_ERRCODE zh_nsxCountScope( NSXAREAP pArea, void * pPtr, ZH_LONG * plRecNo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_nsxCountScope(%p, %p, %p)", ( void * ) pArea, pPtr, ( void * ) plRecNo ) );

   if( pPtr == NULL )
      return ZH_SUCCESS;

   return SUPER_COUNTSCOPE( &pArea->dbfarea.area, pPtr, plRecNo );
}

static ZH_ERRCODE zh_nsxOrderListAdd( NSXAREAP pArea, LPDBORDERINFO pOrderInfo )
{
   PZH_FILE pFile;
   char szFileName[ ZH_PATH_MAX ];
   LPNSXINDEX pIndex;
   ZH_ERRCODE errCode;
   ZH_BOOL fProd;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_nsxOrderListAdd(%p, %p)", ( void * ) pArea, ( void * ) pOrderInfo ) );

   errCode = SELF_GOCOLD( &pArea->dbfarea.area );
   if( errCode != ZH_SUCCESS )
      return errCode;

   if( zh_itemGetCLen( pOrderInfo->atomBagName ) == 0 )
      return ZH_FAILURE;

   zh_nsxCreateFName( pArea, zh_itemGetCPtr( pOrderInfo->atomBagName ),
                      &fProd, szFileName, NULL );

   pIndex = zh_nsxFindBag( pArea, szFileName );

   if( ! pIndex )
   {
      PZH_ITEM pError = NULL;
      LPNSXINDEX * pIndexPtr;
      ZH_BOOL fRetry, fReadonly, fShared;

      fReadonly = pArea->dbfarea.fReadonly;
      fShared = pArea->dbfarea.fShared;
      do
      {
         fRetry = ZH_FALSE;
         pFile = zh_fileExtOpen( szFileName, NULL,
                                 ( fReadonly ? FO_READ : FO_READWRITE ) |
                                 ( fShared ? FO_DENYNONE : FO_EXCLUSIVE ) |
                                 FXO_DEFAULTS | FXO_SHARELOCK | FXO_COPYNAME |
                                 FXO_NOSEEKPOS,
                                 NULL, pError );
         if( ! pFile )
         {
            fRetry = zh_nsxErrorRT( pArea, EG_OPEN, EDBF_OPEN_INDEX, szFileName,
                                    zh_fsError(), EF_CANRETRY | EF_CANDEFAULT,
                                    &pError ) == E_RETRY;
         }
      }
      while( fRetry );

      if( pError )
         zh_errRelease( pError );

      if( ! pFile )
         return ZH_FAILURE;

      pIndex = zh_nsxIndexNew( pArea );
      pIndex->IndexName = zh_strdup( szFileName );
      pIndex->fReadonly = fReadonly;
      pIndex->fShared = fShared;
      pIndex->pFile = pFile;
      pIndex->Production = fProd;

      if( zh_nsxIndexLockRead( pIndex ) )
      {
         errCode = zh_nsxIndexLoad( pIndex );
         zh_nsxIndexUnLockRead( pIndex );
      }
      else
         errCode = ZH_FAILURE;

      if( errCode != ZH_SUCCESS )
      {
         zh_nsxIndexFree( pIndex );
         zh_nsxErrorRT( pArea, EG_CORRUPTION, EDBF_CORRUPT, szFileName, 0, 0, NULL );
         return errCode;
      }

      pIndexPtr = &pArea->lpIndexes;
      while( *pIndexPtr )
         pIndexPtr = &( *pIndexPtr )->pNext;
      *pIndexPtr = pIndex;
   }

   if( ! pArea->lpCurTag && pIndex->iTags )
   {
      pArea->lpCurTag = pIndex->lpTags[ 0 ];
      errCode = SELF_GOTOP( &pArea->dbfarea.area );
   }
   return errCode;
}

static ZH_ERRCODE zh_nsxOrderListClear( NSXAREAP pArea )
{
   LPNSXINDEX * pIndexPtr, pIndex;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_nsxOrderListClear(%p)", ( void * ) pArea ) );

   if( SELF_GOCOLD( &pArea->dbfarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   pArea->lpCurTag = NULL;
   pIndexPtr = &pArea->lpIndexes;
   while( *pIndexPtr )
   {
      pIndex = *pIndexPtr;
      if( pIndex->Production && ( DBFAREA_DATA( &pArea->dbfarea )->fStrictStruct ?
                                  pArea->dbfarea.fHasTags : zh_setGetAutOpen() ) )
      {
         pIndexPtr = &pIndex->pNext;
      }
      else
      {
         *pIndexPtr = pIndex->pNext;
         zh_nsxIndexFree( pIndex );
      }
   }
   return ZH_SUCCESS;
}

static ZH_ERRCODE zh_nsxOrderListDelete( NSXAREAP pArea, LPDBORDERINFO pOrderInfo )
{
   char szTagName[ NSX_TAGNAME + 1 ];
   char szFileName[ ZH_PATH_MAX ];
   LPNSXINDEX pIndex;
   ZH_BOOL fProd;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_nsxOrderListDelete(%p, %p)", ( void * ) pArea, ( void * ) pOrderInfo ) );

   if( SELF_GOCOLD( &pArea->dbfarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   zh_nsxCreateFName( pArea, zh_itemGetCPtr( pOrderInfo->atomBagName ), &fProd,
                      szFileName, szTagName );
   pIndex = zh_nsxFindBag( pArea, szFileName );

   if( pIndex && ! ( pIndex->Production &&
                     ( DBFAREA_DATA( &pArea->dbfarea )->fStrictStruct ?
                       pArea->dbfarea.fHasTags : zh_setGetAutOpen() ) ) )
   {
      LPNSXINDEX * pIndexPtr = &pArea->lpIndexes;
      while( *pIndexPtr )
      {
         if( pIndex == *pIndexPtr )
         {
            *pIndexPtr = pIndex->pNext;
            zh_nsxIndexFree( pIndex );
            break;
         }
         pIndexPtr = &( *pIndexPtr )->pNext;
      }
   }
   return ZH_SUCCESS;
}

static ZH_ERRCODE zh_nsxOrderListFocus( NSXAREAP pArea, LPDBORDERINFO pOrderInfo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_nsxOrderListFocus(%p, %p)", ( void * ) pArea, ( void * ) pOrderInfo ) );

   pOrderInfo->itmResult = zh_itemPutC( pOrderInfo->itmResult,
                             pArea->lpCurTag ? pArea->lpCurTag->TagName : NULL );

   if( pOrderInfo->itmOrder )
   {
      LPTAGINFO pTag = zh_nsxFindTag( pArea, pOrderInfo->itmOrder,
                                      pOrderInfo->atomBagName );
      pArea->lpCurTag = pTag;
   }

   return ZH_SUCCESS;
}

static ZH_ERRCODE zh_nsxOrderListRebuild( NSXAREAP pArea )
{
   LPTAGINFO pCurrTag;
   LPNSXINDEX pIndex;
   ZH_ERRCODE errCode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_nsxOrderListRebuild(%p)", ( void * ) pArea ) );

   errCode = SELF_GOCOLD( &pArea->dbfarea.area );
   if( errCode != ZH_SUCCESS )
      return errCode;

   if( pArea->dbfarea.fShared )
   {
      zh_nsxErrorRT( pArea, EG_SHARED, EDBF_SHARED, pArea->dbfarea.szDataFileName, 0, 0, NULL );
      return ZH_FAILURE;
   }
   if( pArea->dbfarea.fReadonly )
   {
      zh_nsxErrorRT( pArea, EG_READONLY, EDBF_READONLY, pArea->dbfarea.szDataFileName, 0, 0, NULL );
      return ZH_FAILURE;
   }

   if( pArea->dbfarea.lpdbPendingRel )
   {
      errCode = SELF_FORCEREL( &pArea->dbfarea.area );
      if( errCode != ZH_SUCCESS )
         return errCode;
   }
   pCurrTag = pArea->lpCurTag;
   pArea->lpCurTag = NULL;
   pIndex = pArea->lpIndexes;
   while( pIndex && errCode == ZH_SUCCESS )
   {
      errCode = zh_nsxReIndex( pIndex );
      pIndex = pIndex->pNext;
   }
   if( errCode == ZH_SUCCESS )
   {
      pArea->lpCurTag = pCurrTag;
      errCode = SELF_GOTOP( &pArea->dbfarea.area );
   }
   return errCode;
}

static ZH_ERRCODE zh_nsxRddInfo( LPRDDNODE pRDD, ZH_USHORT uiIndex, ZH_ULONG ulConnect, PZH_ITEM pItem )
{
   LPDBFDATA pData;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_nsxRddInfo(%p, %hu, %lu, %p)", ( void * ) pRDD, uiIndex, ulConnect, ( void * ) pItem ) );

   pData = DBFNODE_DATA( pRDD );

   if( pData->bMemoType == 0 )
   {
      pData->bMemoType = DB_MEMO_SMT;
   }

   switch( uiIndex )
   {
      case RDDI_ORDBAGEXT:
      case RDDI_ORDEREXT:
      case RDDI_ORDSTRUCTEXT:
      {
         const char * szNew = zh_itemGetCPtr( pItem );
         char * szNewVal;

         szNewVal = szNew[ 0 ] == '.' && szNew[ 1 ] ? zh_strdup( szNew ) : NULL;
         zh_itemPutC( pItem, pData->szIndexExt[ 0 ] ? pData->szIndexExt : NSX_INDEXEXT );
         if( szNewVal )
         {
            zh_strncpy( pData->szIndexExt, szNewVal, ZH_MAX_FILE_EXT );
            zh_xfree( szNewVal );
         }
         break;
      }

      case RDDI_MULTITAG:
      case RDDI_SORTRECNO:
      case RDDI_STRUCTORD:
         zh_itemPutL( pItem, ZH_TRUE );
         break;

      case RDDI_STRICTSTRUCT:
      {
         ZH_BOOL fStrictStruct = pData->fStrictStruct;
         if( zh_itemType( pItem ) & ZH_IT_LOGICAL )
            pData->fStrictStruct = zh_itemGetL( pItem );
         zh_itemPutL( pItem, fStrictStruct );
         break;
      }

      case RDDI_MULTIKEY:
      {
         ZH_BOOL fMultiKey = pData->fMultiKey;
         if( zh_itemType( pItem ) & ZH_IT_LOGICAL )
            pData->fMultiKey = zh_itemGetL( pItem );
         zh_itemPutL( pItem, fMultiKey );
         break;
      }

      default:
         return SUPER_RDDINFO( pRDD, uiIndex, ulConnect, pItem );
   }

   return ZH_SUCCESS;
}

static const RDDFUNCS nsxTable = {
                             NULL,
                             NULL,
                             NULL,
              ( DBENTRYP_V ) zh_nsxGoBottom,
                             NULL,
                             NULL,
              ( DBENTRYP_V ) zh_nsxTop,
            ( DBENTRYP_BIB ) zh_nsxSeek,
                             NULL,
                             NULL,
              ( DBENTRYP_L ) zh_nsxSkipRaw,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
              ( DBENTRYP_V ) zh_nsxFlush,
                             NULL,
                             NULL,
                             NULL,
              ( DBENTRYP_V ) zh_nsxGoCold,
              ( DBENTRYP_V ) zh_nsxGoHot,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
              ( DBENTRYP_V ) zh_nsxClose,
                             NULL,
                             NULL,
                             NULL,
             ( DBENTRYP_VO ) zh_nsxOpen,
                             NULL,
             ( DBENTRYP_SP ) zh_nsxStructSize,
                             NULL,
                             NULL,
              ( DBENTRYP_V ) zh_nsxPack,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
              ( DBENTRYP_V ) zh_nsxZap,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
            ( DBENTRYP_VOI ) zh_nsxOrderListAdd,
              ( DBENTRYP_V ) zh_nsxOrderListClear,
            ( DBENTRYP_VOI ) zh_nsxOrderListDelete,
            ( DBENTRYP_VOI ) zh_nsxOrderListFocus,
              ( DBENTRYP_V ) zh_nsxOrderListRebuild,
                             NULL,
            ( DBENTRYP_VOC ) zh_nsxOrderCreate,
            ( DBENTRYP_VOI ) zh_nsxOrderDestroy,
           ( DBENTRYP_SVOI ) zh_nsxOrderInfo,
                             NULL,
                             NULL,
                             NULL,
           ( DBENTRYP_VPLP ) zh_nsxCountScope,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             zh_nsxRddInfo,
                             NULL
                           };

ZH_FUNC_TRANSLATE( DBFNSX, _DBF )

ZH_FUNC_STATIC( DBFNSX_GETFUNCTABLE )
{
   RDDFUNCS * pTable;
   ZH_USHORT * puiCount, uiRddId, * puiSuperRddId;

   puiCount = ( ZH_USHORT * ) zh_parptr( 1 );
   pTable = ( RDDFUNCS * ) zh_parptr( 2 );
   uiRddId = ( ZH_USHORT ) zh_parni( 4 );
   puiSuperRddId = ( ZH_USHORT * ) zh_parptr( 5 );

   if( pTable )
   {
      ZH_ERRCODE errCode;

      if( puiCount )
         *puiCount = RDDFUNCSCOUNT;
      errCode = zh_rddInheritEx( pTable, &nsxTable, &nsxSuper, "DBFFPT", puiSuperRddId );
      if( errCode != ZH_SUCCESS )
         errCode = zh_rddInheritEx( pTable, &nsxTable, &nsxSuper, "DBF", puiSuperRddId );
      if( errCode == ZH_SUCCESS )
      {
         /*
          * we successfully register our RDD so now we can initialize it
          * You may think that this place is RDD init statement, Druzus
          */
         s_uiRddId = uiRddId;
      }
      zh_retni( errCode );
   }
   else
   {
      zh_retni( ZH_FAILURE );
   }
}

static void zh_dbfnsxRddInit( void * cargo )
{
   ZH_SYMBOL_UNUSED( cargo );

   if( zh_rddRegister( "DBF", RDT_FULL ) <= 1 )
   {
      zh_rddRegister( "DBFFPT", RDT_FULL );
      if( zh_rddRegister( "DBFNSX", RDT_FULL ) <= 1 )
         return;
   }

   zh_errInternal( ZH_EI_RDDINVALID, NULL, NULL, NULL );
}

ZH_INIT_SYMBOLS_BEGIN( dbfnsx1__InitSymbols )
{ "DBFNSX",              {ZH_FS_PUBLIC|ZH_FS_LOCAL}, {ZH_FUNCNAME( DBFNSX )}, NULL },
{ "DBFNSX_GETFUNCTABLE", {ZH_FS_PUBLIC|ZH_FS_LOCAL}, {ZH_FUNCNAME( DBFNSX_GETFUNCTABLE )}, NULL }
ZH_INIT_SYMBOLS_END( dbfnsx1__InitSymbols )

ZH_CALL_ON_STARTUP_BEGIN( _zh_dbfnsx_rdd_init_ )
   zh_vmAtInit( zh_dbfnsxRddInit, NULL );
ZH_CALL_ON_STARTUP_END( _zh_dbfnsx_rdd_init_ )

#if defined( ZH_PRAGMA_STARTUP )
   #pragma startup dbfnsx1__InitSymbols
   #pragma startup _zh_dbfnsx_rdd_init_
#elif defined( ZH_DATASEG_STARTUP )
   #define ZH_DATASEG_BODY    ZH_DATASEG_FUNC( dbfnsx1__InitSymbols ) \
                              ZH_DATASEG_FUNC( _zh_dbfnsx_rdd_init_ )
   #include "hbiniseg.h"
#endif
