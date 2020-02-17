/*
 * DBFNTX RDD
 *
 * Copyright 1999 Bruno Cantero <bruno@issnet.net>
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
/*
 * The following functions are added by
 *       Alexander Kresin <alex@belacy.belgorod.su>
 *
 * commonError()
 * zh_IncString()
 * ntxNumToStr()
 * checkLogicalExpr()
 * zh__ntxTagKeyCount()
 * zh_ntxInTopScope()
 * zh_ntxInBottomScope()
 * zh_ntxTagKeyNo()
 * zh_ntxTagKeyCount()
 * zh_ntxClearScope()
 * zh_ntxGoEof()
 * zh_ntxGetKeyType()
 * zh_ntxTagKeyFind()
 * zh_ntxPageKeySearch()
 * zh_ntxTagFindCurrentKey()
 * zh_ntxIsRecBad()
 * zh_ntxPageFindCurrentKey()
 * zh_ntxGetCurrentKey()
 * zh_ntxTagGoToNextKey()
 * zh_ntxTagGoToPrevKey()
 * zh_ntxTagGoToTopKey()
 * zh_ntxTagGoToBottomKey()
 * zh_ntxTagKeyGoTo()
 * zh_ntxPageRelease()
 * zh_ntxKeysMove()
 * zh_ntxPageSplit()
 * zh_ntxPageJoin()
 * zh_ntxPageBalance()
 * zh_ntxTagBalance()
 * zh_ntxPageKeyDel()
 * zh_ntxTagKeyAdd()
 * zh_ntxSwapPageSave()
 * zh_ntxKeysSort()
 * zh_ntxSortKeyAdd()
 * zh_ntxSortKeyEnd()
 * zh_ntxWritePage()
 * zh_ntxRootPage()
 * zh_ntxGetSortedKey()
 * zh_ntxBufferSave()
 * zh_ntxReadBuf()
 * zh_ntxPageFind()
 * ntxFindIndex()
 * zh_ntxOrdKeyAdd()
 * zh_ntxOrdKeyDel()
 * ntxGoBottom()
 * ntxGoTo()
 * ntxGoTop()
 * ntxSeek()
 * ntxSkipRaw()
 * ntxGoCold()
 * ntxGoHot()
 * ntxSysName()
 * ntxPack()
 * ntxZap()
 * ntxClearScope()
 * ntxScopeInfo()
 * ntxOrderListAdd()
 * ntxOrderListClear()
 * ntxOrderListFocus()
 * ntxOrderListRebuild()
 * ntxSetScope()
 */

/*
 * Copyright 2005 Przemyslaw Czerpak <druzus@priv.onet.pl>
 * in practice most of the code rewritten
 */

/* #define ZH_NTX_NOMULTITAG */

/* #define ZH_NTX_EXTERNAL_PAGEBUFFER */

#define ZH_NTX_STRONG_BALANCE

/*
#define ZH_NTX_DEBUG
#define ZH_NTX_DEBUG_EXT
#define ZH_NTX_DEBUG_DISP
*/

#include "zh_api.h"
#include "zh_item_api.h"
#include "zh_init.h"
#include "zh_api_error.h"
#include "zh_lang_api.h"
#include "zh_vm.h"
#include "zh_set.h"
#include "zh_stack.h"
#include "zh_math.h"
#include "hbrddntx.h"
#include "rdd_sys.zhh"
#include "zh_regex.h"
#include "zh_codepage_api.h"

#ifdef ZH_NTX_DEBUG_DISP
   static ZH_ULONG s_rdNO = 0;
   static ZH_ULONG s_wrNO = 0;
#endif

static RDDFUNCS  ntxSuper;
static ZH_USHORT s_uiRddId;

/* temporary casts to suppress 32/64-bit Windows warnings */
#define ZH_SHORTCAST   ZH_SHORT
#define ZH_USHORTCAST  ZH_USHORT
#define ZH_INTCAST     int

#define zh_ntxKeyFree( K )             zh_xfree( K )
#define zh_ntxFileOffset( I, B )       ( ( ZH_FOFFSET ) ( B ) << ( ( I )->LargeFile ? NTXBLOCKBITS : 0 ) )
#define zh_ntxPageBuffer( p )          ( ( p )->buffer )

/*
 * The helper functions (endian dependent) - on big endian machines
 * or RISC with strict alignment it's much better to use functions
 * then macros to inform compiler that can count complex parameters
 * only once.
 * On other machines it should not cause noticeable differences because
 * most of modern C compilers auto inline small functions
 */
#if defined( ZH_LITTLE_ENDIAN ) && ! defined( ZH_STRICT_ALIGNMENT )

#define zh_ntxGetKeyCount( p )         ZH_GET_LE_UINT16( zh_ntxPageBuffer( p ) )
#define zh_ntxSetKeyCount( p, n )      ZH_PUT_LE_UINT16( zh_ntxPageBuffer( p ), ( n ) )

#define zh_ntxGetKeyOffset( p, n )     ZH_GET_LE_UINT16( zh_ntxPageBuffer( p ) + 2 + ( ( n ) << 1 ) )
#define zh_ntxGetKeyPtr( p, n )        ( zh_ntxPageBuffer( p ) + zh_ntxGetKeyOffset( p, n ) )
#define zh_ntxGetKeyPage( p, n )       ZH_GET_LE_UINT32( zh_ntxGetKeyPtr( p, n ) )
#define zh_ntxGetKeyRec( p, n )        ZH_GET_LE_UINT32( zh_ntxGetKeyPtr( p, n ) + 4 )
#define zh_ntxGetKeyVal( p, n )        ( zh_ntxGetKeyPtr( p, n ) + 8 )

#define zh_ntxSetKeyOffset( p, n, u )  ZH_PUT_LE_UINT16( zh_ntxPageBuffer( p ) + 2 + ( ( n ) << 1 ), u )
#define zh_ntxSetKeyPage( p, n, l )    ZH_PUT_LE_UINT32( zh_ntxGetKeyPtr( p, n ), l )
#define zh_ntxSetKeyRec( p, n, l )     ZH_PUT_LE_UINT32( zh_ntxGetKeyPtr( p, n ) + 4, l )

#else

static ZH_USHORT zh_ntxGetKeyCount( LPPAGEINFO pPage )
{
   const char * ptr = zh_ntxPageBuffer( pPage );

   return ZH_GET_LE_UINT16( ptr );
}

static void zh_ntxSetKeyCount( LPPAGEINFO pPage, ZH_USHORT uiKeys )
{
   char * ptr = zh_ntxPageBuffer( pPage );

   ZH_PUT_LE_UINT16( ptr, uiKeys );
}

static ZH_USHORT zh_ntxGetKeyOffset( LPPAGEINFO pPage, ZH_SHORT iKey )
{
   const char * ptr = zh_ntxPageBuffer( pPage ) + 2 + ( iKey << 1 );

   return ZH_GET_LE_UINT16( ptr );
}

static void zh_ntxSetKeyOffset( LPPAGEINFO pPage, ZH_SHORT iKey, ZH_USHORT uiOffset )
{
   char * ptr = zh_ntxPageBuffer( pPage ) + 2 + ( iKey << 1 );

   ZH_PUT_LE_UINT16( ptr, uiOffset );
}

static char * zh_ntxGetKeyPtr( LPPAGEINFO pPage, ZH_SHORT iKey )
{
   return zh_ntxPageBuffer( pPage ) + zh_ntxGetKeyOffset( pPage, iKey );
}

static ZH_ULONG zh_ntxGetKeyPage( LPPAGEINFO pPage, ZH_SHORT iKey )
{
   const char * ptr = zh_ntxGetKeyPtr( pPage, iKey );

   return ZH_GET_LE_UINT32( ptr );
}

static void zh_ntxSetKeyPage( LPPAGEINFO pPage, ZH_SHORT iKey, ZH_ULONG ulPage )
{
   char * ptr = zh_ntxGetKeyPtr( pPage, iKey );

   ZH_PUT_LE_UINT32( ptr, ulPage );
}

static char * zh_ntxGetKeyVal( LPPAGEINFO pPage, ZH_SHORT iKey )
{
   return zh_ntxGetKeyPtr( pPage, iKey ) + 8;
}

static void zh_ntxSetKeyRec( LPPAGEINFO pPage, ZH_SHORT iKey, ZH_ULONG ulRec )
{
   char * ptr = zh_ntxGetKeyPtr( pPage, iKey ) + 4;

   ZH_PUT_LE_UINT32( ptr, ulRec );
}

static ZH_ULONG zh_ntxGetKeyRec( LPPAGEINFO pPage, ZH_SHORT iKey )
{
   const char * ptr = zh_ntxGetKeyPtr( pPage, iKey ) + 4;

   return ZH_GET_LE_UINT32( ptr );
}

#endif

/*
 * generate Run-Time error
 */
static ZH_ERRCODE zh_ntxErrorRT( NTXAREAP pArea,
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

/*
 * convert numeric item into NTX key value
 */
static char * zh_ntxNumToStr( PZH_ITEM pItem, char * szBuffer, ZH_USHORT length, ZH_USHORT dec )
{
   char * ptr = szBuffer;

   zh_itemStrBuf( szBuffer, pItem, length, dec );

   while( *ptr == ' ' )
      *ptr++ = '0';

   if( *ptr == '-' )
   {
      *ptr = '0';
      for( ptr = &szBuffer[ 0 ]; *ptr; ptr++ )
      {
         if( *ptr >= '0' && *ptr <= '9' )
            *ptr = ( char ) ( '0' - ( *ptr - '0' ) - 4 );
            /*
             * I intentionally used the above formula to avoid problems on
             * non ASCII machines though many of other xZiher codes is
             * hard coded to ASCII values and should be fixed. Druzus.
             */
      }
   }

   return szBuffer;
}

/*
 * convert numeric NTX key value into item
 */
static PZH_ITEM zh_ntxStrToNum( PZH_ITEM pItem, const char * szKeyVal, ZH_USHORT length, ZH_USHORT dec )
{
   char szBuffer[ NTX_MAX_KEY + 1 ];
   const char * ptr = szKeyVal;
   int iLen, iDec;
   ZH_MAXINT lValue;
   double dValue;

   ZH_SYMBOL_UNUSED( dec );

   if( *ptr == '0' - 4 ) /* negative number */
   {
      char * ptr2, c;
      ptr2 = szBuffer;
      while( ( c = *ptr++ ) != 0 )
      {
         if( c != '.' )
            c = '0' - ( c - '0' + 4 );
         *ptr2++ = c;
      }
      szBuffer[ 0 ] = '-';
      *ptr2 = '\0';
      ptr = szBuffer;
   }
   if( zh_valStrnToNum( ptr, length, &lValue, &dValue, &iDec, &iLen ) )
      return zh_itemPutNDLen( pItem, dValue, iLen, iDec );
   else
      return zh_itemPutNIntLen( pItem, lValue, length );
}

/*
 * create new index key
 */
static LPKEYINFO zh_ntxKeyNew( LPKEYINFO pKeyFrom, int keylen )
{
   LPKEYINFO pKey;

   pKey = ( LPKEYINFO ) zh_xgrab( sizeof( KEYINFO ) + keylen );
   if( pKeyFrom )
   {
      memcpy( pKey->key, pKeyFrom->key, keylen + 1 );
      pKey->Tag = pKeyFrom->Tag;
      pKey->Xtra = pKeyFrom->Xtra;
   }
   else
   {
      pKey->key[ keylen ] = '\0';
      pKey->Tag = pKey->Xtra = 0;
   }
   return pKey;
}

/*
 * copy index key, if dst is null create new dst key else destroy dst
 */
static LPKEYINFO zh_ntxKeyCopy( LPKEYINFO pKeyDest, LPKEYINFO pKey, int keylen )
{
   if( ! pKeyDest )
      pKeyDest = zh_ntxKeyNew( NULL, keylen );

   memcpy( pKeyDest->key, pKey->key, keylen + 1 );
   pKeyDest->Tag = pKey->Tag;
   pKeyDest->Xtra = pKey->Xtra;

   return pKeyDest;
}

/*
 * get ntx key type for given item
 */
static ZH_BYTE zh_ntxItemType( PZH_ITEM pItem )
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
 * convert key type to comparable type
 */
static ZH_BYTE zh_ntxItemTypeCmp( ZH_BYTE bType )
{
   return bType == 'T' ? 'D' : bType;
}

/*
 * store Item in index key
 * TODO: uiType check and generate RT error if necessary
 *       probably not here or we will have to add parameter
 *       for scope key evaluation
 */
static LPKEYINFO zh_ntxKeyPutItem( LPKEYINFO pKey, PZH_ITEM pItem, ZH_ULONG ulRecNo,
                                   LPTAGINFO pTag, ZH_BOOL fTrans, ZH_USHORT * puiLen )
{
   ZH_SIZE len;

   if( ! pKey )
      pKey = zh_ntxKeyNew( NULL, pTag->KeyLength );

   if( puiLen )
      *puiLen = pTag->KeyLength;

   switch( zh_ntxItemType( pItem ) )
   {
      case 'C':
         if( fTrans )
         {
            len = pTag->KeyLength;
            zh_cdpnDup2( zh_itemGetCPtr( pItem ), zh_itemGetCLen( pItem ),
                         pKey->key, &len,
                         zh_vmCDP(), pTag->pIndex->pArea->dbfarea.area.cdPage );
         }
         else
         {
            len = zh_itemGetCLen( pItem );
            if( len > ( ZH_SIZE ) pTag->KeyLength )
               len = pTag->KeyLength;
            memcpy( pKey->key, zh_itemGetCPtr( pItem ), len );
         }
         if( len < ( ZH_SIZE ) pTag->KeyLength )
         {
            memset( pKey->key + len, ' ', pTag->KeyLength - len );
            if( puiLen )
               *puiLen = ( ZH_USHORT ) len;
         }
         pKey->key[ pTag->KeyLength ] = '\0';
         break;
      case 'N':
         zh_ntxNumToStr( pItem, pKey->key, pTag->KeyLength, pTag->KeyDec );
         break;
      case 'T':
         if( pTag->KeyType == 'T' )
         {
            zh_itemGetTS( pItem, pKey->key );
            break;
         }
         /* fallthrough */
      case 'D':
         if( pTag->KeyLength < 8 )
         {
            char szDate[ 9 ];
            zh_itemGetDS( pItem, szDate );
            memcpy( pKey->key, szDate, pTag->KeyLength );
         }
         else
         {
            zh_itemGetDS( pItem, pKey->key );
            if( pTag->KeyLength > 8 )
            {
               memset( pKey->key + 8, '\0', pTag->KeyLength - 8 );
               if( puiLen )
                  *puiLen = 8;
            }
         }
         pKey->key[ pTag->KeyLength ] = '\0';
         break;
      case 'L':
         pKey->key[ 0 ] = ( zh_itemGetL( pItem ) ? 'T' : 'F' );
         if( pTag->KeyLength > 1 )
            memset( pKey->key + 1, '\0', pTag->KeyLength - 1 );
         pKey->key[ pTag->KeyLength ] = '\0';
         break;
      default:
         memset( pKey->key, '\0', pTag->KeyLength + 1 );
   }
   pKey->Xtra = ulRecNo;
   pKey->Tag = 0;

   return pKey;
}

/*
 * get Item from index key
 */
static PZH_ITEM zh_ntxKeyGetItem( PZH_ITEM pItem, LPKEYINFO pKey,
                                  LPTAGINFO pTag, ZH_BOOL fTrans )
{
   if( pKey )
   {
      switch( pTag->KeyType )
      {
         case 'C':
            if( fTrans )
            {
               ZH_SIZE nLen = pTag->KeyLength;
               char * pszVal = zh_cdpnDup( pKey->key, &nLen,
                                           pTag->pIndex->pArea->dbfarea.area.cdPage, zh_vmCDP() );
               pItem = zh_itemPutCLPtr( pItem, pszVal, nLen );
            }
            else
            {
               pItem = zh_itemPutCL( pItem, pKey->key, pTag->KeyLength );
            }
            break;
         case 'N':
            pItem = zh_ntxStrToNum( pItem, pKey->key, pTag->KeyLength, pTag->KeyDec );
            break;
         case 'D':
            pItem = zh_itemPutDS( pItem, pKey->key );
            break;
         case 'T':
            pItem = zh_itemPutTS( pItem, pKey->key );
            break;
         case 'L':
            pItem = zh_itemPutL( pItem, pKey->key[ 0 ] == 'T' );
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
 * evaluate conditional expression and return the logical result
 */
static ZH_BOOL zh_ntxEvalCond( NTXAREAP pArea, PZH_ITEM pCondItem, ZH_BOOL fSetWA )
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
static ZH_BOOL zh_ntxEvalSeekCond( LPTAGINFO pTag, PZH_ITEM pCondItem )
{
   ZH_BOOL fRet;
   PZH_ITEM pKeyVal, pKeyRec;

   pKeyVal = zh_ntxKeyGetItem( NULL, pTag->CurKeyInfo, pTag, ZH_TRUE );
   pKeyRec = zh_itemPutNInt( NULL, pTag->CurKeyInfo->Xtra );

   fRet = zh_itemGetL( zh_vmEvalBlockV( pCondItem, 2, pKeyVal, pKeyRec ) );

   zh_itemRelease( pKeyVal );
   zh_itemRelease( pKeyRec );

   return fRet;
}

/*
 * get ITEM type of key expression
 */
static ZH_BYTE zh_ntxGetKeyType( LPTAGINFO pTag )
{
   ZH_BYTE bType;

   if( pTag->nField )
   {
      PZH_ITEM pItem = zh_itemNew( NULL );
      SELF_GETVALUE( &pTag->pIndex->pArea->dbfarea.area, pTag->nField, pItem );
      bType = zh_ntxItemType( pItem );
      zh_itemRelease( pItem );
   }
   else
   {
      int iCurrArea = zh_rddGetCurrentWorkAreaNumber();

      if( iCurrArea != pTag->pIndex->pArea->dbfarea.area.uiArea )
         zh_rddSelectWorkAreaNumber( pTag->pIndex->pArea->dbfarea.area.uiArea );
      else
         iCurrArea = 0;

      bType = zh_ntxItemType( zh_vmEvalBlockOrMacro( pTag->pKeyItem ) );

      if( iCurrArea )
         zh_rddSelectWorkAreaNumber( iCurrArea );
   }
   return bType;
}

/*
 * evaluate key expression and create new Key from the result
 */
static LPKEYINFO zh_ntxEvalKey( LPKEYINFO pKey, LPTAGINFO pTag )
{
   NTXAREAP pArea = pTag->pIndex->pArea;
   PZH_ITEM pItem;
   PZH_CODEPAGE cdpTmp = zh_cdpSelect( pArea->dbfarea.area.cdPage );

   if( pTag->nField )
   {
      pItem = zh_itemNew( NULL );
      SELF_GETVALUE( &pArea->dbfarea.area, pTag->nField, pItem );
      pKey = zh_ntxKeyPutItem( pKey, pItem, pArea->dbfarea.ulRecNo, pTag, ZH_FALSE, NULL );
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
      pKey = zh_ntxKeyPutItem( pKey, pItem, pArea->dbfarea.ulRecNo, pTag, ZH_FALSE, NULL );

      if( iCurrArea )
         zh_rddSelectWorkAreaNumber( iCurrArea );
   }

   zh_cdpSelect( cdpTmp );

   return pKey;
}

/*
 * compare two values using Tag conditions (len & type)
 */
static int zh_ntxValCompare( LPTAGINFO pTag, const char * val1, int len1,
                             const char * val2, int len2, ZH_BOOL fExact )
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
            return -zh_cdpcmp( val2, ( ZH_SIZE ) len2, val1, ( ZH_SIZE ) len1,
                               pTag->pIndex->pArea->dbfarea.area.cdPage, 0 );
      }

      if( iResult == 0 )
      {
         if( len1 > len2 )
            iResult = 1;
         else if( len1 < len2 && fExact )
            iResult = -1;
      }
      else if( iResult > 0 )
         iResult = 1;
      else
         iResult = -1;
   }
   else
   {
      if( iLimit <= 0 || ( iResult = memcmp( val1, val2, iLimit ) ) == 0 )
      {
         if( len1 > len2 )
            iResult = 1;
         else if( len1 < len2 && fExact )
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
static ZH_BOOL zh_ntxInTopScope( LPTAGINFO pTag, const char * key )
{
   PZH_NTXSCOPE pScope = pTag->fUsrDescend ? &pTag->bottom : &pTag->top;

   if( pScope->scopeKeyLen )
   {
      int i = zh_ntxValCompare( pTag, pScope->scopeKey->key, pScope->scopeKeyLen,
                                key, pTag->KeyLength, ZH_FALSE );
      return pTag->fUsrDescend ? i >= 0 : i <= 0;
   }
   else
      return ZH_TRUE;
}

/*
 * check if a given key is in bottom scope
 */
static ZH_BOOL zh_ntxInBottomScope( LPTAGINFO pTag, const char * key )
{
   PZH_NTXSCOPE pScope = pTag->fUsrDescend ? &pTag->top : &pTag->bottom;

   if( pScope->scopeKeyLen )
   {
      int i = zh_ntxValCompare( pTag, pScope->scopeKey->key, pScope->scopeKeyLen,
                                key, pTag->KeyLength, ZH_FALSE );
      return pTag->fUsrDescend ? i <= 0 : i >= 0;
   }
   else
      return ZH_TRUE;
}

/*
 * check if a given key is in current scope
 */
static ZH_BOOL zh_ntxKeyInScope( LPTAGINFO pTag, LPKEYINFO pKey )
{
   return zh_ntxInTopScope( pTag, pKey->key ) &&
          zh_ntxInBottomScope( pTag, pKey->key );
}

/*
 * clear top or bottom scope
 */
static void zh_ntxTagClearScope( LPTAGINFO pTag, ZH_USHORT nScope )
{
   NTXAREAP pArea = pTag->pIndex->pArea;
   PZH_NTXSCOPE pScope;

   /* resolve any pending scope relations first */
   if( pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped )
      SELF_FORCEREL( &pArea->dbfarea.area );

   if( pTag->fUsrDescend )
      nScope = ( nScope == 0 ) ? 1 : 0;

   pScope = ( nScope == 0 ) ? &pTag->top : &pTag->bottom;

   if( pScope->scopeKey )
   {
      zh_ntxKeyFree( pScope->scopeKey );
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
static void zh_ntxTagSetScope( LPTAGINFO pTag, ZH_USHORT nScope, PZH_ITEM pItem )
{
   NTXAREAP pArea = pTag->pIndex->pArea;
   PZH_ITEM pScopeVal;

   /* resolve any pending scope relations first */
   if( pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped )
      SELF_FORCEREL( &pArea->dbfarea.area );

   pScopeVal = ( zh_itemType( pItem ) & ZH_IT_BLOCK ) ?
                           zh_vmEvalBlock( pItem ) : pItem;

   if( zh_ntxItemTypeCmp( pTag->KeyType ) == zh_ntxItemTypeCmp( zh_ntxItemType( pScopeVal ) ) )
   {
      PZH_NTXSCOPE pScope;
      ZH_BOOL fTop = ( nScope == 0 );

      if( pTag->fUsrDescend )
         fTop = ! fTop;

      pScope = fTop ? &pTag->top : &pTag->bottom;

      pScope->scopeKey = zh_ntxKeyPutItem( pScope->scopeKey, pScopeVal,
               ( fTop == pTag->AscendKey ) ? NTX_IGNORE_REC_NUM : NTX_MAX_REC_NUM,
               pTag, ZH_TRUE, &pScope->scopeKeyLen );

      if( pScope->scopeItem == NULL )
         pScope->scopeItem = zh_itemNew( NULL );
      zh_itemCopy( pScope->scopeItem, pItem );

      pTag->keyCount = 0;
   }
   else
   {
      zh_ntxTagClearScope( pTag, nScope );
   }
}

/*
 * get top or bottom scope item
 */
static void zh_ntxTagGetScope( LPTAGINFO pTag, ZH_USHORT nScope, PZH_ITEM pItem )
{
   NTXAREAP pArea = pTag->pIndex->pArea;
   PZH_NTXSCOPE pScope;

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
static void zh_ntxTagRefreshScope( LPTAGINFO pTag )
{
   PZH_ITEM pItem;

   /* resolve any pending scope relations first */
   if( pTag->pIndex->pArea->dbfarea.lpdbPendingRel &&
       pTag->pIndex->pArea->dbfarea.lpdbPendingRel->isScoped )
      SELF_FORCEREL( &pTag->pIndex->pArea->dbfarea.area );

   if( zh_itemType( pTag->top.scopeItem ) & ZH_IT_BLOCK )
   {
      pItem = zh_vmEvalBlock( pTag->top.scopeItem );
      pTag->top.scopeKey = zh_ntxKeyPutItem( pTag->top.scopeKey, pItem,
               pTag->top.scopeKey->Xtra, pTag, ZH_TRUE, &pTag->top.scopeKeyLen );
   }
   if( zh_itemType( pTag->bottom.scopeItem ) & ZH_IT_BLOCK )
   {
      pItem = zh_vmEvalBlock( pTag->bottom.scopeItem );
      pTag->bottom.scopeKey = zh_ntxKeyPutItem( pTag->bottom.scopeKey, pItem,
         pTag->bottom.scopeKey->Xtra, pTag, ZH_TRUE, &pTag->bottom.scopeKeyLen );
   }
}

/*
 * an interface for fast check record number in record filter
 */
static ZH_BOOL zh_ntxCheckRecordScope( NTXAREAP pArea, ZH_ULONG ulRec )
{
   ZH_LONG lRecNo = ( ZH_LONG ) ulRec;

   if( SELF_COUNTSCOPE( &pArea->dbfarea.area, NULL, &lRecNo ) == ZH_SUCCESS && lRecNo == 0 )
   {
      return ZH_FALSE;
   }
   return ZH_TRUE;
}

#ifdef ZH_NTX_DEBUG
static void zh_ntxTagCheckBuffers( LPTAGINFO pTag )
{
   LPPAGEINFO pPage;
   ZH_ULONG i;

   if( ( pTag->HdrChanged || pTag->pIndex->Changed ) && ! pTag->pIndex->lockWrite )
      zh_errInternal( 9301, "zh_ntxTagCheckBuffers: tag modified in unlocked index", NULL, NULL );

   for( i = 0; i < pTag->pIndex->ulPages; i++ )
   {
      pPage = pTag->pIndex->pages[ i ];
      if( pPage->Changed && ! pTag->pIndex->lockWrite )
         zh_errInternal( 9302, "zh_ntxTagCheckBuffers: page modified in unlocked index", NULL, NULL );
      if( pPage->iUsed )
         zh_errInternal( 9303, "zh_ntxTagCheckBuffers: page still allocated", NULL, NULL );
   }
}

static void zh_ntxPageCheckKeys( LPPAGEINFO pPage, LPTAGINFO pTag, int iPos, int iType )
{
   ZH_USHORT u;
   int i;

   for( u = 1; u < pPage->uiKeys; u++ )
   {
      i = zh_ntxValCompare( pTag,
                            zh_ntxGetKeyVal( pPage, u - 1 ), pTag->KeyLength,
                            zh_ntxGetKeyVal( pPage, u ), pTag->KeyLength, ZH_TRUE );
      if( ! pTag->AscendKey )
         i = -i;
      if( i > 0 )
      {
         printf( "\r\nuiKeys=%d(%d/%d), (%d)[%.*s]>(%d)[%.*s]", pPage->uiKeys, iPos, iType,
                 u - 1, pTag->KeyLength, zh_ntxGetKeyVal( pPage, u - 1 ),
                 u, pTag->KeyLength, zh_ntxGetKeyVal( pPage, u ) );
         fflush( stdout );
         zh_errInternal( 9304, "zh_ntxPageCheckKeys: keys sorted wrong.", NULL, NULL );
      }
   }
}
#endif

/*
 * read a given block from index file
 */
static ZH_BOOL zh_ntxBlockRead( LPNTXINDEX pIndex, ZH_ULONG ulBlock, void * buffer, int iSize )
{
   if( ! pIndex->lockRead && ! pIndex->lockWrite )
      zh_errInternal( 9103, "zh_ntxBlockRead on not locked index file.", NULL, NULL );

#ifdef ZH_NTX_DEBUG_DISP
   s_rdNO++;
#endif
   if( zh_fileReadAt( pIndex->DiskFile, buffer, iSize,
                      zh_ntxFileOffset( pIndex, ulBlock ) ) != ( ZH_SIZE ) iSize )
   {
      zh_ntxErrorRT( pIndex->pArea, EG_READ, EDBF_READ,
                     pIndex->IndexName, zh_fsError(), 0, NULL );
      return ZH_FALSE;
   }
   return ZH_TRUE;
}

/*
 * write a given block into index file
 */
static ZH_BOOL zh_ntxBlockWrite( LPNTXINDEX pIndex, ZH_ULONG ulBlock, const void * buffer, int iSize )
{
   if( ! pIndex->lockWrite )
      zh_errInternal( 9102, "zh_ntxBlockWrite on not locked index file.", NULL, NULL );

#ifdef ZH_NTX_DEBUG_DISP
   s_wrNO++;
#endif
   if( zh_fileWriteAt( pIndex->DiskFile, buffer, iSize,
                       zh_ntxFileOffset( pIndex, ulBlock ) ) != ( ZH_SIZE ) iSize )
   {
      zh_ntxErrorRT( pIndex->pArea, EG_WRITE, EDBF_WRITE,
                     pIndex->IndexName, zh_fsError(), 0, NULL );
      return ZH_FALSE;
   }
   return ZH_TRUE;
}

/*
 * write a given tag page to file
 */
static ZH_BOOL zh_ntxPageSave( LPNTXINDEX pIndex, LPPAGEINFO pPage )
{
   zh_ntxSetKeyCount( pPage, pPage->uiKeys );
   if( ! zh_ntxBlockWrite( pIndex, pPage->Page,
                           zh_ntxPageBuffer( pPage ), NTXBLOCKSIZE ) )
      return ZH_FALSE;
   pPage->Changed = ZH_FALSE;
   pIndex->fFlush = ZH_TRUE;
   /* In shared mode we have to update counter in version field of
      NTXHEADER to signal for other stations that their index buffers
      has to be discarded */
   if( pIndex->fShared )
      pIndex->Changed = ZH_TRUE;
   return ZH_TRUE;
}

/*
 * discard all index buffers due to concurrent access
 */
static void zh_ntxDiscardBuffers( LPNTXINDEX pIndex )
{
   pIndex->ulPages = pIndex->ulPageLast = 0;
   pIndex->pChanged = pIndex->pFirst = pIndex->pLast = NULL;
   if( pIndex->Compound )
   {
      int i;

      for( i = 0; i < pIndex->iTags; i++ )
      {
         pIndex->lpTags[ i ]->RootBlock  = 0;
         pIndex->lpTags[ i ]->stackLevel = 0;
      }
   }
   else
   {
      pIndex->TagBlock = 0;
      if( pIndex->iTags )
         pIndex->lpTags[ 0 ]->stackLevel = 0;
   }
   zh_fileFlush( pIndex->DiskFile, ZH_FALSE );
}

/*
 * update tag flags
 */
static void zh_ntxTagUpdateFlags( LPTAGINFO pTag )
{
   ZH_USHORT uiSignature = pTag->Signature;

   pTag->Custom   = ( uiSignature & NTX_FLAG_CUSTOM ) != 0;
   pTag->ChgOnly  = ( uiSignature & NTX_FLAG_CHGONLY ) != 0;
   pTag->Partial  = ( uiSignature & NTX_FLAG_PARTIAL ) != 0;
   pTag->Template = ( uiSignature & NTX_FLAG_TEMPLATE ) != 0;
   pTag->MultiKey = ( uiSignature & NTX_FLAG_MULTIKEY ) != 0;
   pTag->fSortRec = ( uiSignature & NTX_FLAG_SORTRECNO ) != 0;
}

/*
 * check tag header in compound index
 */
static ZH_BOOL zh_ntxTagHeaderCheck( LPTAGINFO pTag )
{
   if( ! pTag->RootBlock )
   {
      if( pTag->HeadBlock )
      {
         NTXHEADERUPDT header;
         if( zh_ntxBlockRead( pTag->pIndex, pTag->HeadBlock, &header, sizeof( header ) ) )
         {
            pTag->Signature = ZH_GET_LE_UINT16( header.type );
            pTag->RootBlock = ZH_GET_LE_UINT32( header.root );
            zh_ntxTagUpdateFlags( pTag );
         }
      }
   }
   return pTag->RootBlock != 0;
}

/*
 * free buffers for pages in the tag
 */
static void zh_ntxFreePageBuffer( LPNTXINDEX pIndex )
{
   ZH_ULONG ul, ulMax = pIndex->ulPagesDepth;

   if( ulMax )
   {
      LPPAGEINFO * pPagePtr = pIndex->pages;

      for( ul = 0; ul < ulMax; ul++, pPagePtr++ )
      {
         if( *pPagePtr )
         {
#ifdef ZH_NTX_EXTERNAL_PAGEBUFFER
            if( zh_ntxPageBuffer( *pPagePtr ) )
               zh_xfree( zh_ntxPageBuffer( *pPagePtr ) );
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
static void zh_ntxIndexTrunc( LPNTXINDEX pIndex )
{
   if( ! pIndex->lockWrite )
      zh_errInternal( 9102, "zh_ntxIndexTrunc on not locked index file.", NULL, NULL );

   zh_ntxFreePageBuffer( pIndex );
   pIndex->Update = pIndex->Changed = pIndex->fFlush = ZH_TRUE;
   pIndex->TagBlock = pIndex->NextAvail = 0;
   pIndex->Version = 0;
   zh_fileTruncAt( pIndex->DiskFile, NTXBLOCKSIZE );
}

/*
 * try to find given tag page in the buffer
 */
static LPPAGEINFO zh_ntxPageFind( LPTAGINFO pTag, ZH_ULONG ulPage )
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
static LPPAGEINFO zh_ntxPageGetBuffer( LPTAGINFO pTag, ZH_ULONG ulPage )
{
   LPNTXINDEX pIndex = pTag->pIndex;
   LPPAGEINFO * pPagePtr;

   if( pIndex->ulPages < pIndex->ulPagesDepth )
   {
      pPagePtr = &pIndex->pages[ pIndex->ulPages++ ];
   }
   else if( pIndex->pFirst )
   {
      LPPAGEINFO pPage = pIndex->pFirst;

      if( pPage->iUsed )
         zh_errInternal( 9305, "zh_ntxPageGetBuffer: page used.", NULL, NULL );
      if( pPage->Changed )
         zh_errInternal( 9306, "zh_ntxPageGetBuffer: page changed.", NULL, NULL );

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
      pIndex->ulPagesDepth = NTX_PAGES_PER_TAG;
      pIndex->pages = ( LPPAGEINFO * ) zh_xgrabz( sizeof( LPPAGEINFO ) * NTX_PAGES_PER_TAG );
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
            pIndex->ulPagesDepth += NTX_PAGES_PER_TAG >> 1;
            pIndex->pages = ( LPPAGEINFO * ) zh_xrealloc( pIndex->pages,
                                 sizeof( LPPAGEINFO ) * pIndex->ulPagesDepth );
            memset( pIndex->pages + ul, 0,
                         ( NTX_PAGES_PER_TAG >> 1 ) * sizeof( LPPAGEINFO ) );
            pIndex->ulPages++;
            pPagePtr = &pIndex->pages[ ul ];
            pIndex->ulPageLast = 0;
            break;
         }
      }
   }

   if( ! *pPagePtr )
      *pPagePtr = ( LPPAGEINFO ) zh_xgrabz( sizeof( ZH_PAGEINFO ) );
#ifdef ZH_NTX_EXTERNAL_PAGEBUFFER
   if( ! zh_ntxPageBuffer( *pPagePtr ) )
      zh_ntxPageBuffer( *pPagePtr ) = ( char * ) zh_xgrabz( NTXBLOCKSIZE );
#endif
   ( *pPagePtr )->pPrev = NULL;
   ( *pPagePtr )->Page  = ulPage;
   ( *pPagePtr )->iUsed = 1;
   return *pPagePtr;
}

/*
 * free the index page for future reuse
 */
static void zh_ntxPageFree( LPTAGINFO pTag, LPPAGEINFO pPage )
{
   zh_ntxSetKeyPage( pPage, 0, pTag->pIndex->NextAvail );
   pTag->pIndex->NextAvail = pPage->Page;
   pTag->pIndex->Changed = pPage->Changed = ZH_TRUE;
}

/*
 * mark used page as free
 */
static void zh_ntxPageRelease( LPTAGINFO pTag, LPPAGEINFO pPage )
{
   LPNTXINDEX pIndex = pTag->pIndex;

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
      zh_errInternal( 9307, "zh_ntxPageRelease: unused page freed.", NULL, NULL );
}

/*
 * load page from index file or the buffer
 */
static LPPAGEINFO zh_ntxPageLoad( LPTAGINFO pTag, ZH_ULONG ulPage )
{
   LPPAGEINFO pPage;

   if( ! ulPage )
   {
      if( zh_ntxTagHeaderCheck( pTag ) )
         ulPage = pTag->RootBlock;
      if( ! ulPage )
      {
         zh_ntxErrorRT( pTag->pIndex->pArea, EG_CORRUPTION, EDBF_CORRUPT,
                        pTag->pIndex->IndexName, 0, 0, NULL );
         return NULL;
      }
   }
   pPage = zh_ntxPageFind( pTag, ulPage );
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
      pPage = zh_ntxPageGetBuffer( pTag, ulPage );
      pPage->Changed = ZH_FALSE;
      if( ! zh_ntxBlockRead( pTag->pIndex, ulPage,
                             zh_ntxPageBuffer( pPage ), NTXBLOCKSIZE ) )
      {
         zh_ntxPageRelease( pTag, pPage );
         return NULL;
      }
      pPage->uiKeys = zh_ntxGetKeyCount( pPage );
   }
   return pPage;
}

/*
 * initialize empty page structure
 */
static void zh_ntxPageInit( LPTAGINFO pTag, LPPAGEINFO pPage )
{
   ZH_USHORT u, o = ( pTag->MaxKeys + 2 ) << 1;

   for( u = 0; u <= pTag->MaxKeys; u++, o += pTag->KeyLength + 8 )
      zh_ntxSetKeyOffset( pPage, u, o );
   zh_ntxSetKeyPage( pPage, 0, 0 );
   pPage->uiKeys = 0;
}

/*
 * allocate new page address
 */
static ZH_ULONG zh_ntxPageAlloc( LPNTXINDEX pIndex )
{
   ZH_ULONG ulPage;

   if( ! pIndex->TagBlock )
   {
      ZH_FOFFSET fOffset;
      fOffset = zh_fileSize( pIndex->DiskFile );
      pIndex->TagBlock = ( ZH_ULONG )
                         ( fOffset >> ( pIndex->LargeFile ? NTXBLOCKBITS : 0 ) );
   }
   ulPage = pIndex->TagBlock;
   pIndex->TagBlock += pIndex->LargeFile ? 1 : NTXBLOCKSIZE;
   return ulPage;
}

/*
 * allocate new page in index file - reuse freed one or increase file
 */
static LPPAGEINFO zh_ntxPageNew( LPTAGINFO pTag, ZH_BOOL fNull )
{
   LPPAGEINFO pPage;

   if( pTag->pIndex->NextAvail != 0 )
   {
      /*
         Handling of a pool of empty pages.
         Some sources says that this address is in the first 4 bytes of
         a page ( https://www.clicketyclick.dk/databases/xbase/format/ ).
         But as I understood, studying dumps of Clipper ntx'es, address of the
         next available page is in the address field of a first key item
         in the page - it is done here now in such a way.
         = Alexander Kresin =
       */
      pPage = zh_ntxPageLoad( pTag, pTag->pIndex->NextAvail );
      if( ! pPage )
         return NULL;
      /*
         Unfortunately Clipper does not left unused index pages clean and
         the key counter can be set to non zero value so to make possible
         concurrent index access from Clipper and xZiher it's necessary
         to disable the check code below. [druzus]
       */
#if 0
      if( pPage->uiKeys != 0 )
      {
         zh_ntxErrorRT( pTag->pIndex->pArea, EG_CORRUPTION, EDBF_CORRUPT,
                        pTag->pIndex->IndexName, 0, 0, NULL );
         return NULL;
      }
#endif
      pTag->pIndex->NextAvail = zh_ntxGetKeyPage( pPage, 0 );
#if defined( ZH_NTX_NOMULTITAG )
      zh_ntxSetKeyPage( pPage, 0, 0 );
      pPage->uiKeys = 0;
#else
      zh_ntxPageInit( pTag, pPage );
#endif
   }
   else
   {
      pPage = zh_ntxPageGetBuffer( pTag, fNull ? 0 : zh_ntxPageAlloc( pTag->pIndex ) );
      zh_ntxPageInit( pTag, pPage );
   }
   pTag->pIndex->Changed = pPage->Changed = ZH_TRUE;

   return pPage;
}

/*
 * add given page to list of free pages
 */
static void zh_ntxPageAddFree( LPTAGINFO pTag, ZH_ULONG ulPage )
{
   LPPAGEINFO pPage = zh_ntxPageGetBuffer( pTag, ulPage );

   pPage->Changed = ZH_TRUE;
   zh_ntxPageInit( pTag, pPage );
   zh_ntxPageFree( pTag, pPage );
   zh_ntxPageSave( pTag->pIndex, pPage );
   zh_ntxPageRelease( pTag, pPage );
}

/*
 * get free page in index file
 */
static ZH_ULONG zh_ntxPageGetFree( LPTAGINFO pTag )
{
   LPPAGEINFO pPage = zh_ntxPageNew( pTag, ZH_FALSE );
   ZH_ULONG ulPage = 0;

   if( pPage )
   {
      ulPage = pPage->Page;
      pPage->Changed = ZH_FALSE;
      zh_ntxPageRelease( pTag, pPage );
   }
   return ulPage;
}

/*
 * create the new tag structure
 */
static LPTAGINFO zh_ntxTagNew( LPNTXINDEX pIndex,
                               const char * szTagName, ZH_BOOL fTagName,
                               const char * szKeyExpr, PZH_ITEM pKeyExpr,
                               ZH_BYTE bKeyType, ZH_USHORT uiKeyLen, ZH_USHORT uiKeyDec,
                               const char * szForExpr, PZH_ITEM pForExpr,
                               ZH_BOOL fAscendKey, ZH_BOOL fUnique, ZH_BOOL fCustom,
                               ZH_BOOL fSortRec )
{
   LPTAGINFO pTag;

   pTag = ( LPTAGINFO ) zh_xgrabz( sizeof( TAGINFO ) );
   pTag->TagName = zh_strndup( szTagName, NTX_MAX_TAGNAME );
   pTag->fTagName = fTagName;
   pTag->pIndex = pIndex;
   if( szKeyExpr )
   {
      pTag->KeyExpr = zh_strndup( szKeyExpr, NTX_MAX_EXP );
   }
   if( pForExpr && szForExpr )
   {
      pTag->ForExpr = zh_strndup( szForExpr, NTX_MAX_EXP );
   }
   pTag->nField = zh_rddFieldExpIndex( &pIndex->pArea->dbfarea.area, pTag->KeyExpr );
   pTag->pKeyItem = pKeyExpr;
   pTag->pForItem = pForExpr;
   pTag->AscendKey = fAscendKey;
   pTag->fUsrDescend = ! pTag->AscendKey;
   pTag->UniqueKey = fUnique;
   pTag->Custom = fCustom;
   pTag->MultiKey = fCustom && DBFAREA_DATA( &pIndex->pArea->dbfarea )->fMultiKey;
   pTag->KeyType = bKeyType;
   pTag->KeyLength = uiKeyLen;
   pTag->KeyDec = uiKeyDec;
   pTag->fSortRec = fSortRec;
   /*
    * TODO?: keep during page update the offset to 'MaxKeys' key fixed
    * so we will be able to store 1 key more in the page
    */
   pTag->MaxKeys = ( NTXBLOCKSIZE - 2 ) / ( uiKeyLen + 10 ) - 1;

   /* TODO?: is it necessary? It should not interact with well implemented
      algorithm */
   if( pTag->MaxKeys & 0x01 && pTag->MaxKeys > 2 )
      pTag->MaxKeys--;

   pTag->CurKeyInfo = zh_ntxKeyNew( NULL, pTag->KeyLength );

   return pTag;
}

/*
 * free from memory tag structure
 */
static void zh_ntxTagFree( LPTAGINFO pTag )
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
      zh_ntxKeyFree( pTag->HotKeyInfo );
   zh_ntxKeyFree( pTag->CurKeyInfo );
   zh_ntxTagClearScope( pTag, 0 );
   zh_ntxTagClearScope( pTag, 1 );
   if( pTag->stack )
      zh_xfree( pTag->stack );
   zh_xfree( pTag );
}

/*
 * delete tag from compound index
 */
static void zh_ntxTagDelete( LPTAGINFO pTag )
{
   LPNTXINDEX pIndex = pTag->pIndex;
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
   zh_ntxTagFree( pTag );
   pIndex->pArea->fSetTagNumbers = ZH_TRUE;
}

/*
 * add tag to compound index
 */
static ZH_ERRCODE zh_ntxTagAdd( LPNTXINDEX pIndex, LPTAGINFO pTag )
{
   if( pIndex->iTags >= CTX_MAX_TAGS )
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
static LPTAGINFO zh_ntxTagLoad( LPNTXINDEX pIndex, ZH_ULONG ulBlock,
                                const char * szTagName, ZH_BYTE * buffer )
{
   LPNTXHEADER lpNTX = ( LPNTXHEADER ) buffer;
   LPTAGINFO pTag;
   PZH_ITEM pKeyExp, pForExp = NULL;
   ZH_USHORT usType;
   ZH_BOOL fName;

   usType = ZH_GET_LE_UINT16( lpNTX->type );

   if( ( usType & ~NTX_FLAG_MASK ) ||
       ( ( usType & NTX_FLAG_DEFALUT ) != NTX_FLAG_DEFALUT &&
         usType != NTX_FLAG_OLDDEFALUT ) ||
       lpNTX->key_expr[ 0 ] < 0x20 )
      return NULL;

   if( SELF_COMPILE( &pIndex->pArea->dbfarea.area, ( const char * ) lpNTX->key_expr ) == ZH_FAILURE )
      return NULL;
   pKeyExp = pIndex->pArea->dbfarea.area.valResult;
   pIndex->pArea->dbfarea.area.valResult = NULL;

   if( usType & NTX_FLAG_FORITEM && lpNTX->for_expr[ 0 ] >= 0x20 )
   {
      if( SELF_COMPILE( &pIndex->pArea->dbfarea.area, ( const char * ) lpNTX->for_expr ) == ZH_FAILURE )
      {
         zh_vmDestroyBlockOrMacro( pKeyExp );
         return NULL;
      }
      pForExp = pIndex->pArea->dbfarea.area.valResult;
      pIndex->pArea->dbfarea.area.valResult = NULL;
   }
   fName = ! pIndex->Compound && lpNTX->tag_name[ 0 ] >= 0x20;
   pTag  = zh_ntxTagNew( pIndex,
                         fName ? ( const char * ) lpNTX->tag_name : szTagName, fName,
                         ( const char * ) lpNTX->key_expr, pKeyExp,
                         '\0',
                         ZH_GET_LE_UINT16( lpNTX->key_size ),
                         ZH_GET_LE_UINT16( lpNTX->key_dec ),
                         ( const char * ) lpNTX->for_expr, pForExp,
                         lpNTX->descend[ 0 ] == 0, lpNTX->unique[ 0 ] != 0,
                         ( usType & NTX_FLAG_CUSTOM ) != 0 || lpNTX->custom[ 0 ] != 0,
                         ( usType & NTX_FLAG_SORTRECNO ) != 0 );

   pTag->Signature = usType;
   zh_ntxTagUpdateFlags( pTag );
   pTag->HeadBlock = ulBlock;
   pTag->RootBlock = ZH_GET_LE_UINT32( lpNTX->root );
   pTag->MaxKeys = ZH_GET_LE_UINT16( lpNTX->max_item );
   pTag->KeyType = zh_ntxGetKeyType( pTag );

   pIndex->LargeFile = ( usType & NTX_FLAG_LARGEFILE ) != 0;

   if( ! pIndex->Compound )
   {
      pIndex->Version = ZH_GET_LE_UINT16( lpNTX->version );
      pIndex->NextAvail = ZH_GET_LE_UINT32( lpNTX->next_page );
      pIndex->TagBlock = 0;

      /* TODO: this breaks unlocking !!! */
      if( usType & NTX_FLAG_LARGEFILE )
      {
         pIndex->pArea->dbfarea.bLockType = DB_DBFLOCK_HB64;
      }
      else if( usType & NTX_FLAG_EXTLOCK )
      {
         pIndex->pArea->dbfarea.bLockType = DB_DBFLOCK_CLIPPER2;
      }
      else if( ! pIndex->pArea->dbfarea.bLockType )
      {
         pIndex->pArea->dbfarea.bLockType = ( usType & NTX_FLAG_EXTLOCK ) ?
                           DB_DBFLOCK_CLIPPER2 : DB_DBFLOCK_CLIPPER;
      }
   }
   return pTag;
}

/*
 * add tag into CTX header
 */
static void zh_ntxIndexTagAdd( LPNTXINDEX pIndex, LPTAGINFO pTag )
{
   LPCTXHEADER lpCTX = ( LPCTXHEADER ) pIndex->HeaderBuff;
   int iTags = ZH_GET_LE_UINT16( lpCTX->ntags ), iLen, i;
   LPCTXTAGITEM pTagItem = lpCTX->tags;

   for( i = 0; i < iTags; pTagItem++, i++ )
   {
      if( ! zh_strnicmp( ( const char * ) pTagItem->tag_name, pTag->TagName, NTX_MAX_TAGNAME ) )
         break;
   }
   if( i == iTags )
   {
      ++iTags;
      ZH_PUT_LE_UINT16( lpCTX->ntags, iTags );
      iLen = ( int ) strlen( pTag->TagName );
      if( iLen > NTX_MAX_TAGNAME )
         iLen = NTX_MAX_TAGNAME;
      memcpy( pTagItem->tag_name, pTag->TagName, iLen );
      memset( pTagItem->tag_name + iLen, 0, sizeof( pTagItem->tag_name ) - iLen );
   }
   ZH_PUT_LE_UINT32( pTagItem->tag_header, pTag->HeadBlock );
   pIndex->Update = ZH_TRUE;
}

/*
 * delete tag from CTX header
 */
static void zh_ntxIndexTagDel( LPNTXINDEX pIndex, const char * szTagName )
{
   LPCTXHEADER lpCTX = ( LPCTXHEADER ) pIndex->HeaderBuff;
   int iTags = ZH_GET_LE_UINT16( lpCTX->ntags ), i;
   LPCTXTAGITEM pTagItem = lpCTX->tags;

   for( i = 0; i < iTags; pTagItem++, i++ )
   {
      if( ! zh_strnicmp( ( const char * ) pTagItem->tag_name, szTagName, NTX_MAX_TAGNAME ) )
      {
         memmove( pTagItem, pTagItem + 1, ( iTags - i ) * sizeof( CTXTAGITEM ) );
         memset( pTagItem + iTags - 1, 0, sizeof( CTXTAGITEM ) );
         --iTags;
         ZH_PUT_LE_UINT16( lpCTX->ntags, iTags );
         pIndex->Update = ZH_TRUE;
         break;
      }
   }
}

/*
 * find tag header block in CTX header
 */
static ZH_ULONG zh_ntxIndexTagFind( LPCTXHEADER lpCTX, const char * szTagName )
{
   int iTags = ZH_GET_LE_UINT16( lpCTX->ntags ), i;
   LPCTXTAGITEM pTagItem = lpCTX->tags;

   for( i = 0; i < iTags; pTagItem++, i++ )
   {
      if( ! zh_strnicmp( ( const char * ) pTagItem->tag_name, szTagName, NTX_MAX_TAGNAME ) )
         return ZH_GET_LE_UINT32( pTagItem->tag_header );
   }
   return NTX_DUMMYNODE;
}

/*
 * Write tag header
 */
static ZH_ERRCODE zh_ntxTagHeaderSave( LPTAGINFO pTag )
{
   LPNTXINDEX pIndex = pTag->pIndex;
   NTXHEADER Header;
   int iSize = NTX_ROOTHEAD_HEADSIZE, type, version = 0, iLen;
   ZH_ULONG next = 0;

   if( pIndex->Compound )
   {
      if( ! pTag->HeadBlock )
      {
         pTag->HeadBlock = zh_ntxPageGetFree( pTag );
         if( ! pTag->HeadBlock )
            return ZH_FAILURE;
         zh_ntxIndexTagAdd( pIndex, pTag );
      }
   }
   else
   {
      if( pTag->HeadBlock )
      {
         zh_ntxPageAddFree( pTag, pTag->HeadBlock );
         pTag->HeadBlock = 0;
         pIndex->Update = ZH_TRUE;
      }
      pIndex->Version++;
      version = pIndex->Version &= 0xffff;
      next = pIndex->NextAvail;
   }

   type = NTX_FLAG_DEFALUT |
      ( pTag->ForExpr ? NTX_FLAG_FORITEM : 0 ) |
      ( pTag->Partial ? NTX_FLAG_PARTIAL | NTX_FLAG_FORITEM : 0 ) |
      ( pIndex->pArea->dbfarea.bLockType == DB_DBFLOCK_CLIPPER2 ? NTX_FLAG_EXTLOCK : 0 ) |
      ( pTag->Partial  ? NTX_FLAG_PARTIAL | NTX_FLAG_FORITEM : 0 ) |
      /* non Clipper flags */
      ( pTag->Custom   ? NTX_FLAG_CUSTOM : 0 ) |
      ( pTag->ChgOnly  ? NTX_FLAG_CHGONLY : 0 ) |
      ( pTag->Template ? NTX_FLAG_TEMPLATE : 0 ) |
      ( pTag->MultiKey ? NTX_FLAG_MULTIKEY : 0 ) |
      ( pTag->fSortRec ? NTX_FLAG_SORTRECNO : 0 ) |
      ( pIndex->LargeFile ? NTX_FLAG_LARGEFILE : 0 );

   ZH_PUT_LE_UINT16( Header.type, type );
   ZH_PUT_LE_UINT16( Header.version, version );
   ZH_PUT_LE_UINT32( Header.root, pTag->RootBlock );
   ZH_PUT_LE_UINT32( Header.next_page, next );

   if( pIndex->Update )
   {
      memset( ( ZH_BYTE * ) &Header + NTX_ROOTHEAD_HEADSIZE, 0,
              sizeof( NTXHEADER ) - NTX_ROOTHEAD_HEADSIZE );

      ZH_PUT_LE_UINT16( Header.item_size, pTag->KeyLength + 8 );
      ZH_PUT_LE_UINT16( Header.key_size,  pTag->KeyLength );
      ZH_PUT_LE_UINT16( Header.key_dec,   pTag->KeyDec );
      ZH_PUT_LE_UINT16( Header.max_item,  pTag->MaxKeys );
      ZH_PUT_LE_UINT16( Header.half_page, pTag->MaxKeys >> 1 );
      Header.unique[ 0 ]  = pTag->UniqueKey ? 1 : 0;
      Header.descend[ 0 ] = pTag->AscendKey ? 0 : 1;
      Header.custom[ 0 ]  = pTag->Custom    ? 1 : 0;
      iLen = ( int ) strlen( pTag->KeyExpr );
      if( iLen > NTX_MAX_EXP )
         iLen = NTX_MAX_EXP;
      memcpy( Header.key_expr, pTag->KeyExpr, iLen );
      if( pTag->ForExpr )
      {
         iLen = ( int ) strlen( pTag->ForExpr );
         if( iLen > NTX_MAX_EXP )
            iLen = NTX_MAX_EXP;
         memcpy( Header.for_expr, pTag->ForExpr, iLen );
      }
      if( pTag->fTagName )
      {
         iLen = ( int ) strlen( pTag->TagName );
         if( iLen > NTX_MAX_TAGNAME )
            iLen = NTX_MAX_TAGNAME;
         memcpy( Header.tag_name, pTag->TagName, iLen );
      }
      iSize = sizeof( NTXHEADER );
   }

   if( ! zh_ntxBlockWrite( pIndex, pTag->HeadBlock, &Header, iSize ) )
      return ZH_FAILURE;
   pTag->HdrChanged = ZH_FALSE;
   pIndex->Changed = pIndex->Compound;
   pIndex->fFlush = ZH_TRUE;
   return ZH_SUCCESS;
}

/*
 * create new index structure
 */
static LPNTXINDEX zh_ntxIndexNew( NTXAREAP pArea )
{
   LPNTXINDEX pIndex;

   pIndex = ( LPNTXINDEX ) zh_xgrabz( sizeof( NTXINDEX ) );

   pIndex->DiskFile = NULL;
   pIndex->pArea = pArea;
   return pIndex;
}

/*
 * close the index file and free from memory index and tag structures
 */
static void zh_ntxIndexFree( LPNTXINDEX pIndex )
{
   zh_ntxFreePageBuffer( pIndex );
   if( pIndex->iTags )
   {
      int i;
      for( i = 0; i < pIndex->iTags; i++ )
         zh_ntxTagFree( pIndex->lpTags[ i ] );
      zh_xfree( pIndex->lpTags );
   }
   if( pIndex->HeaderBuff )
      zh_xfree( pIndex->HeaderBuff );
   if( pIndex->DiskFile )
   {
      zh_fileClose( pIndex->DiskFile );
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
static ZH_ERRCODE zh_ntxIndexHeaderSave( LPNTXINDEX pIndex )
{
   if( pIndex->Compound )
   {
      LPCTXHEADER lpCTX = ( LPCTXHEADER ) pIndex->HeaderBuff;
      int iSize = pIndex->Update ? NTXBLOCKSIZE : 16;
      ZH_USHORT type;

      type = NTX_FLAG_COMPOUND | ( pIndex->LargeFile ? NTX_FLAG_LARGEFILE : 0 );

      pIndex->Version++;
      ZH_PUT_LE_UINT16( lpCTX->type, type );
      ZH_PUT_LE_UINT16( lpCTX->ntags, pIndex->iTags );
      ZH_PUT_LE_UINT32( lpCTX->version, pIndex->Version );
      ZH_PUT_LE_UINT32( lpCTX->freepage, pIndex->NextAvail );
      ZH_PUT_LE_UINT32( lpCTX->filesize, pIndex->TagBlock );

      if( ! zh_ntxBlockWrite( pIndex, 0, lpCTX, iSize ) )
         return ZH_FAILURE;
   }
   pIndex->Changed = pIndex->Update = ZH_FALSE;
   return ZH_SUCCESS;
}

/*
 * load new tags from index file
 */
static ZH_ERRCODE zh_ntxIndexLoad( LPNTXINDEX pIndex, const char * szTagName )
{
   LPTAGINFO pTag;
   ZH_USHORT type;

   if( ! pIndex->fValidHeader )
   {
      if( ! pIndex->HeaderBuff )
         pIndex->HeaderBuff = ( ZH_BYTE * ) zh_xgrab( NTXBLOCKSIZE );
      if( ! zh_ntxBlockRead( pIndex, 0, pIndex->HeaderBuff, NTXBLOCKSIZE ) )
         return ZH_FAILURE;
      pIndex->fValidHeader = ZH_TRUE;
   }

   type = ZH_GET_LE_UINT16( pIndex->HeaderBuff );
#if ! defined( ZH_NTX_NOMULTITAG )
   pIndex->Compound = ( type & NTX_FLAG_COMPOUND ) != 0;
   if( pIndex->Compound )
   {
      ZH_BYTE tagbuffer[ NTXBLOCKSIZE ];
      LPCTXHEADER lpCTX = ( LPCTXHEADER ) pIndex->HeaderBuff;
      LPCTXTAGITEM pTagItem = lpCTX->tags;
      int iTags;

      iTags = ZH_GET_LE_UINT16( lpCTX->ntags );
      if( iTags > CTX_MAX_TAGS )
         return ZH_FAILURE;
      pIndex->Version = ZH_GET_LE_UINT32( lpCTX->version );
      pIndex->NextAvail = ZH_GET_LE_UINT32( lpCTX->freepage );
      pIndex->TagBlock = ZH_GET_LE_UINT32( lpCTX->filesize );
      pIndex->LargeFile = ( type & NTX_FLAG_LARGEFILE ) != 0;

      for( pIndex->iTags = 0; pIndex->iTags < iTags; pTagItem++ )
      {
         ZH_ULONG ulBlock = ZH_GET_LE_UINT32( pTagItem->tag_header );
         if( ulBlock == 0 || pTagItem->tag_name[ 0 ] <= 0x20 )
            return ZH_FAILURE;
         if( ! zh_ntxBlockRead( pIndex, ulBlock, tagbuffer, NTXBLOCKSIZE ) )
            return ZH_FAILURE;
         pTag = zh_ntxTagLoad( pIndex, ulBlock, ( const char * ) pTagItem->tag_name, tagbuffer );
         if( ! pTag )
            return ZH_FAILURE;
         zh_ntxTagAdd( pIndex, pTag );
      }
   }
   else
#endif
   {
      pTag = zh_ntxTagLoad( pIndex, 0, szTagName, pIndex->HeaderBuff );
      if( ! pTag )
         return ZH_FAILURE;
      zh_ntxTagAdd( pIndex, pTag );
   }

   return ZH_SUCCESS;
}

/*
 * read index header and check for concurrent access
 */
static ZH_ERRCODE zh_ntxIndexHeaderRead( LPNTXINDEX pIndex )
{
   ZH_USHORT type;

   if( ! pIndex->HeaderBuff )
      pIndex->HeaderBuff = ( ZH_BYTE * ) zh_xgrab( NTXBLOCKSIZE );

   if( ! zh_ntxBlockRead( pIndex, 0, pIndex->HeaderBuff, NTXBLOCKSIZE ) )
      return ZH_FAILURE;

   type = ZH_GET_LE_UINT16( pIndex->HeaderBuff );
   if( ( type & NTX_FLAG_COMPOUND ) != 0 )
   {
#if defined( ZH_NTX_NOMULTITAG )
      zh_ntxErrorRT( pIndex->pArea, EG_CORRUPTION, EDBF_CORRUPT,
                     pIndex->IndexName, 0, 0, NULL );
      return ZH_FAILURE;
#else
      LPCTXHEADER lpCTX = ( LPCTXHEADER ) pIndex->HeaderBuff;
      ZH_ULONG ulVersion, ulNext;
      #if 0
      ZH_USHORT usTags = ZH_GET_LE_UINT16( lpCTX->ntags );
      #endif

      ulVersion = ZH_GET_LE_UINT32( lpCTX->version );
      ulNext = ZH_GET_LE_UINT32( lpCTX->freepage );
      pIndex->TagBlock = ZH_GET_LE_UINT32( lpCTX->filesize );

      if( pIndex->Version != ulVersion || pIndex->NextAvail != ulNext ||
          ! pIndex->Compound )
      {
         int i;
         zh_ntxDiscardBuffers( pIndex );
         pIndex->Version = ulVersion;
         pIndex->NextAvail = ulNext;
         pIndex->Compound = ZH_TRUE;
         for( i = 1; i < pIndex->iTags; i++ )
         {
            pIndex->lpTags[ i ]->HeadBlock =
                     zh_ntxIndexTagFind( lpCTX, pIndex->lpTags[ i ]->TagName );
            if( ! pIndex->lpTags[ i ]->HeadBlock )
               pIndex->lpTags[ i ]->RootBlock = 0;
         }
      }
#endif
   }
   else
   {
      LPNTXHEADER lpNTX = ( LPNTXHEADER ) pIndex->HeaderBuff;
      ZH_ULONG ulRootPage, ulVersion, ulNext;
      LPTAGINFO pTag;

      if( pIndex->Compound )
      {
         zh_ntxErrorRT( pIndex->pArea, EG_CORRUPTION, EDBF_CORRUPT,
                        pIndex->IndexName, 0, 0, NULL );
         return ZH_FAILURE;
      }
      pTag = pIndex->iTags ? pIndex->lpTags[ 0 ] : NULL;

      ulVersion = ZH_GET_LE_UINT16( lpNTX->version );
      ulRootPage = ZH_GET_LE_UINT32( lpNTX->root );
      ulNext = ZH_GET_LE_UINT32( lpNTX->next_page );
      if( pIndex->Version != ulVersion || pIndex->NextAvail != ulNext ||
          ( pTag && ( pTag->Signature != type || ulRootPage != pTag->RootBlock ) ) )
      {
         zh_ntxDiscardBuffers( pIndex );
         pIndex->Version = ulVersion;
         pIndex->NextAvail = ulNext;
         if( pTag )
         {
            pTag->RootBlock = ulRootPage;
            pTag->Signature = type;
            zh_ntxTagUpdateFlags( pTag );
         }
      }
   }
   return ZH_SUCCESS;
}

/*
 * write modified pages to index file
 */
static void zh_ntxIndexFlush( LPNTXINDEX pIndex )
{
   while( pIndex->pChanged )
   {
      LPPAGEINFO pPage = pIndex->pChanged;
      pIndex->pChanged = pPage->pNext;
      if( pPage->Changed )
      {
         zh_ntxPageSave( pIndex, pPage );
         ++pPage->iUsed;
         /* hack */
         zh_ntxPageRelease( pIndex->lpTags[ 0 ], pPage );
      }
      else
         zh_errInternal( 9308, "zh_ntxIndexFlush: unchaged page in the list.", NULL, NULL );
   }

   if( pIndex->Compound )
   {
      int i;

      for( i = 0; i < pIndex->iTags; i++ )
         if( pIndex->lpTags[ i ]->HdrChanged )
            zh_ntxTagHeaderSave( pIndex->lpTags[ i ] );
      if( pIndex->Changed )
         zh_ntxIndexHeaderSave( pIndex );
   }
   else if( pIndex->iTags )
   {
      if( pIndex->Changed || pIndex->lpTags[ 0 ]->HdrChanged )
         zh_ntxTagHeaderSave( pIndex->lpTags[ 0 ] );
   }
}

/*
 * lock index for reading (shared lock)
 */
static ZH_BOOL zh_ntxIndexLockRead( LPNTXINDEX pIndex )
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
      fOK = zh_dbfLockIdxFile( &pIndex->pArea->dbfarea, pIndex->DiskFile,
                               FL_LOCK | FLX_SHARED | FLX_WAIT, ZH_FALSE,
                               &pIndex->lockData );
      /* if fOK then check VERSION field in NTXHEADER and
       * if it has been changed then discard all page buffers
       */
      if( fOK )
      {
         pIndex->lockRead++;
         if( zh_ntxIndexHeaderRead( pIndex ) != ZH_SUCCESS )
         {
            pIndex->lockRead--;
            zh_dbfLockIdxFile( &pIndex->pArea->dbfarea, pIndex->DiskFile,
                               FL_UNLOCK, ZH_FALSE, &pIndex->lockData );
            return ZH_FALSE;
         }
      }
   }
   if( ! fOK )
      zh_ntxErrorRT( pIndex->pArea, EG_LOCK, EDBF_LOCK,
                     pIndex->IndexName, zh_fsError(), 0, NULL );

   return fOK;
}

/*
 * lock index for writing (exclusive lock)
 */
static ZH_BOOL zh_ntxIndexLockWrite( LPNTXINDEX pIndex, ZH_BOOL fCheck )
{
   ZH_BOOL fOK;

   if( pIndex->fReadonly )
      zh_errInternal( 9101, "zh_ntxIndexLockWrite: readonly index.", NULL, NULL );

   if( pIndex->lockRead )
      zh_errInternal( 9105, "zh_ntxIndexLockWrite: writeLock after readLock.", NULL, NULL );

   if( pIndex->lockWrite > 0 || ! pIndex->fShared )
   {
      fOK = ZH_TRUE;
      pIndex->lockWrite++;
   }
   else
   {
      fOK = zh_dbfLockIdxFile( &pIndex->pArea->dbfarea, pIndex->DiskFile,
                               FL_LOCK | FLX_EXCLUSIVE | FLX_WAIT, ZH_FALSE,
                               &pIndex->lockData );
      /* if fOK then check VERSION field in NTXHEADER and
       * if it has been changed then discard all page buffers
       */
      if( fOK )
      {
         pIndex->lockWrite++;
         if( fCheck && zh_ntxIndexHeaderRead( pIndex ) != ZH_SUCCESS )
         {
            pIndex->lockWrite--;
            zh_dbfLockIdxFile( &pIndex->pArea->dbfarea, pIndex->DiskFile,
                               FL_UNLOCK, ZH_FALSE, &pIndex->lockData );
            return ZH_FALSE;
         }
      }
   }
   if( ! fOK )
      zh_ntxErrorRT( pIndex->pArea, EG_LOCK, EDBF_LOCK,
                     pIndex->IndexName, zh_fsError(), 0, NULL );

   return fOK;
}

/*
 * remove index read lock (shared lock)
 */
static ZH_BOOL zh_ntxIndexUnLockRead( LPNTXINDEX pIndex )
{
   ZH_BOOL fOK;

#ifdef ZH_NTX_DEBUG
   int i;
   for( i = 0; i < pIndex->iTags; i++ )
      zh_ntxTagCheckBuffers( pIndex->lpTags[ i ] );
#endif

   pIndex->lockRead--;
   if( pIndex->lockRead < 0 )
      zh_errInternal( 9106, "zh_ntxIndexUnLockRead: bad count of locks.", NULL, NULL );

   if( pIndex->lockRead || pIndex->lockWrite || ! pIndex->fShared ||
       ZH_DIRTYREAD( &pIndex->pArea->dbfarea ) )
   {
      fOK = ZH_TRUE;
   }
   else
   {
      pIndex->fValidHeader = ZH_FALSE;
      fOK = zh_dbfLockIdxFile( &pIndex->pArea->dbfarea, pIndex->DiskFile,
                               FL_UNLOCK, ZH_FALSE, &pIndex->lockData );
   }
   if( ! fOK )
      zh_errInternal( 9108, "zh_ntxIndexUnLockRead: unlock error.", NULL, NULL );

   return fOK;
}

/*
 * remove index write lock (exclusive lock)
 */
static ZH_BOOL zh_ntxIndexUnLockWrite( LPNTXINDEX pIndex )
{
   ZH_BOOL fOK;

#ifdef ZH_NTX_DEBUG
   int i;
   for( i = 0; i < pIndex->iTags; i++ )
      zh_ntxTagCheckBuffers( pIndex->lpTags[ i ] );
#endif

   if( pIndex->lockWrite <= 0 )
      zh_errInternal( 9106, "zh_ntxIndexUnLockWrite: bad count of locks.", NULL, NULL );
   if( pIndex->lockRead )
      zh_errInternal( 9105, "zh_ntxIndexUnLockWrite: writeUnLock before readUnLock.", NULL, NULL );

   zh_ntxIndexFlush( pIndex );
   pIndex->lockWrite--;

   if( pIndex->lockWrite || ! pIndex->fShared )
   {
      fOK = ZH_TRUE;
   }
   else
   {
      zh_fileFlush( pIndex->DiskFile, ZH_TRUE );
      pIndex->fValidHeader = ZH_FALSE;
      fOK = zh_dbfLockIdxFile( &pIndex->pArea->dbfarea, pIndex->DiskFile,
                               FL_UNLOCK, ZH_FALSE, &pIndex->lockData );
   }
   if( ! fOK )
      zh_errInternal( 9108, "zh_ntxIndexUnLockWrite: unlock error.", NULL, NULL );

   return fOK;
}

/*
 * lock tag for reading (shared lock)
 */
static ZH_BOOL zh_ntxTagLockRead( LPTAGINFO pTag )
{
   ZH_BOOL fOK = ZH_FALSE;

   if( zh_ntxIndexLockRead( pTag->pIndex ) )
   {
      fOK = zh_ntxTagHeaderCheck( pTag );
      if( ! fOK )
      {
         zh_ntxIndexUnLockRead( pTag->pIndex );
         zh_ntxErrorRT( pTag->pIndex->pArea, EG_CORRUPTION, EDBF_CORRUPT,
                        pTag->pIndex->IndexName, 0, 0, NULL );
      }
   }
   return fOK;
}

/*
 * lock tag for writing (exclusive lock)
 */
static ZH_BOOL zh_ntxTagLockWrite( LPTAGINFO pTag )
{
   ZH_BOOL fOK = ZH_FALSE;

   if( zh_ntxIndexLockWrite( pTag->pIndex, ZH_TRUE ) )
   {
      fOK = zh_ntxTagHeaderCheck( pTag );
      if( ! fOK )
      {
         zh_ntxIndexUnLockWrite( pTag->pIndex );
         zh_ntxErrorRT( pTag->pIndex->pArea, EG_CORRUPTION, EDBF_CORRUPT,
                        pTag->pIndex->IndexName, 0, 0, NULL );
      }
   }
   return fOK;
}

/*
 * remove tag read lock (shared lock)
 */
static ZH_BOOL zh_ntxTagUnLockRead( LPTAGINFO pTag )
{
   return zh_ntxIndexUnLockRead( pTag->pIndex );
}

/*
 * remove tag write lock (exclusive lock)
 */
static ZH_BOOL zh_ntxTagUnLockWrite( LPTAGINFO pTag )
{
   return zh_ntxIndexUnLockWrite( pTag->pIndex );
}

/*
 * retrive key from page
 */
static void zh_ntxPageGetKey( LPPAGEINFO pPage, ZH_USHORT uiKey, LPKEYINFO pKey, ZH_USHORT uiLen )
{
   if( uiKey < pPage->uiKeys )
   {
      memcpy( pKey->key, zh_ntxGetKeyVal( pPage, uiKey ), uiLen );
      pKey->Xtra = zh_ntxGetKeyRec( pPage, uiKey );
      pKey->Tag = pPage->Page;
   }
   else
   {
      pKey->Xtra = pKey->Tag = 0;
   }
}

/*
 * set next page and key in page path
 */
static void zh_ntxTagSetPageStack( LPTAGINFO pTag, ZH_ULONG ulPage, ZH_USHORT uiKey )
{
   if( pTag->stackLevel == pTag->stackSize )
   {
      if( pTag->stackSize == 0 )
      {
         pTag->stackSize = NTX_STACKSIZE;
         pTag->stack = (LPTREESTACK) zh_xgrab( sizeof(TREE_STACK) * NTX_STACKSIZE );
      }
      else
      {
         pTag->stackSize += NTX_STACKSIZE;
         pTag->stack = ( LPTREESTACK ) zh_xrealloc( pTag->stack,
                                    sizeof( TREE_STACK ) * pTag->stackSize );
      }
   }
   pTag->stack[ pTag->stackLevel ].page = ulPage;
   pTag->stack[ pTag->stackLevel++ ].ikey = uiKey;
}

/*
 * go down from the given index page to the first key
 */
static LPPAGEINFO zh_ntxPageTopMove( LPTAGINFO pTag, ZH_ULONG ulPage )
{
   LPPAGEINFO pPage = NULL;

   do
   {
      if( pPage )
         zh_ntxPageRelease( pTag, pPage );
      pPage = zh_ntxPageLoad( pTag, ulPage );
      if( ! pPage )
         return NULL;
#ifdef ZH_NTX_DEBUG_EXT
      if( pPage->uiKeys == 0 && pTag->stackLevel > 0 )
         zh_errInternal( 9201, "zh_ntxPageTopMove: index corrupted.", NULL, NULL );
#endif
      ulPage = zh_ntxGetKeyPage( pPage, 0 );
      zh_ntxTagSetPageStack( pTag, pPage->Page, 0 );
   }
   while( ulPage );

   return pPage;
}

/*
 * go down from the given index page to the last key
 */
static LPPAGEINFO zh_ntxPageBottomMove( LPTAGINFO pTag, ZH_ULONG ulPage )
{
   LPPAGEINFO pPage = NULL;

   do
   {
      if( pPage )
         zh_ntxPageRelease( pTag, pPage );
      pPage = zh_ntxPageLoad( pTag, ulPage );
      if( ! pPage )
         return NULL;
#ifdef ZH_NTX_DEBUG_EXT
      if( pPage->uiKeys == 0 && pTag->stackLevel > 0 )
         zh_errInternal( 9201, "zh_ntxPageBottomMove: index corrupted.", NULL, NULL );
#endif
      ulPage = zh_ntxGetKeyPage( pPage, pPage->uiKeys );
      zh_ntxTagSetPageStack( pTag, pPage->Page, pPage->uiKeys -
                                    ( ulPage || pPage->uiKeys == 0 ? 0 : 1 ) );
   }
   while( ulPage );

   return pPage;
}

/*
 * set page path to the first key in tag
 */
static ZH_BOOL zh_ntxTagTopKey( LPTAGINFO pTag )
{
   LPPAGEINFO pPage;
   int iKeys;

   pTag->stackLevel = 0;
   pPage = zh_ntxPageTopMove( pTag, 0 );
   if( ! pPage )
      return ZH_FALSE;
   zh_ntxPageGetKey( pPage, 0, pTag->CurKeyInfo, pTag->KeyLength );
   iKeys = pPage->uiKeys;
   zh_ntxPageRelease( pTag, pPage );
   return iKeys != 0;
}

/*
 * set page path to the last key in tag
 */
static ZH_BOOL zh_ntxTagBottomKey( LPTAGINFO pTag )
{
   LPPAGEINFO pPage;
   int iKeys;

   pTag->stackLevel = 0;
   pPage = zh_ntxPageBottomMove( pTag, 0 );
   if( ! pPage )
      return ZH_FALSE;
   zh_ntxPageGetKey( pPage, pTag->stack[ pTag->stackLevel - 1 ].ikey,
                     pTag->CurKeyInfo, pTag->KeyLength );
   iKeys = pPage->uiKeys;
   zh_ntxPageRelease( pTag, pPage );
   return iKeys != 0;
}

/*
 * update page path to the next key in tag
 */
static ZH_BOOL zh_ntxTagNextKey( LPTAGINFO pTag )
{
   int iLevel = pTag->stackLevel - 1;

   if( iLevel >= 0 )
   {
      LPPAGEINFO pPage;
      ZH_ULONG ulPage = 0;

      pPage = zh_ntxPageLoad( pTag, pTag->stack[ iLevel ].page );
      if( ! pPage )
         return ZH_FALSE;
      if( pTag->stack[ iLevel ].ikey < ( ZH_SHORT ) pPage->uiKeys )
         ulPage = zh_ntxGetKeyPage( pPage, pTag->stack[ iLevel ].ikey + 1 );
      if( ulPage || pTag->stack[ iLevel ].ikey + 1 < pPage->uiKeys )
      {
         pTag->stack[ iLevel ].ikey++;
         if( ulPage )
         {
            zh_ntxPageRelease( pTag, pPage );
            pPage = zh_ntxPageTopMove( pTag, ulPage );
            if( ! pPage )
               return ZH_FALSE;
         }
      }
      else
      {
         while( --iLevel >= 0 )
         {
            zh_ntxPageRelease( pTag, pPage );
            pPage = zh_ntxPageLoad( pTag, pTag->stack[ iLevel ].page );
            if( ! pPage )
               return ZH_FALSE;
            if( pTag->stack[ iLevel ].ikey < ( ZH_SHORT ) pPage->uiKeys )
               break;
         }
         if( iLevel < 0 )
         {
            zh_ntxPageRelease( pTag, pPage );
            return ZH_FALSE;
         }
         pTag->stackLevel = ( ZH_USHORTCAST ) ( iLevel + 1 );
      }
      zh_ntxPageGetKey( pPage, pTag->stack[ pTag->stackLevel - 1 ].ikey,
                        pTag->CurKeyInfo, pTag->KeyLength );
      zh_ntxPageRelease( pTag, pPage );
      return ZH_TRUE;
   }
   return ZH_FALSE;
}

/*
 * update page path to the previous key in tag
 */
static ZH_BOOL zh_ntxTagPrevKey( LPTAGINFO pTag )
{
   int iLevel = pTag->stackLevel - 1;

   if( iLevel >= 0 )
   {
      LPPAGEINFO pPage;
      ZH_ULONG ulPage;

      pPage = zh_ntxPageLoad( pTag, pTag->stack[ iLevel ].page );
      if( ! pPage )
         return ZH_FALSE;
      ulPage = zh_ntxGetKeyPage( pPage, pTag->stack[ iLevel ].ikey );
      if( ulPage )
      {
         zh_ntxPageRelease( pTag, pPage );
         pPage = zh_ntxPageBottomMove( pTag, ulPage );
         if( ! pPage )
            return ZH_FALSE;
      }
      else if( pTag->stack[ iLevel ].ikey )
      {
         pTag->stack[ iLevel ].ikey--;
      }
      else
      {
         while( --iLevel >= 0 )
         {
            zh_ntxPageRelease( pTag, pPage );
            pPage = zh_ntxPageLoad( pTag, pTag->stack[ iLevel ].page );
            if( ! pPage )
               return ZH_FALSE;
            if( pTag->stack[ iLevel ].ikey )
            {
               pTag->stack[ iLevel ].ikey--;
               break;
            }
         }
         if( iLevel < 0 )
         {
            zh_ntxPageRelease( pTag, pPage );
            return ZH_FALSE;
         }
         pTag->stackLevel = ( ZH_USHORTCAST ) ( iLevel + 1 );
      }
      zh_ntxPageGetKey( pPage, pTag->stack[ pTag->stackLevel - 1 ].ikey,
                        pTag->CurKeyInfo, pTag->KeyLength );
      zh_ntxPageRelease( pTag, pPage );
      return ZH_TRUE;
   }
   return ZH_FALSE;
}

/*
 * find a key value in page
 */
static int zh_ntxPageKeyFind( LPTAGINFO pTag, LPPAGEINFO pPage,
                              const char * key, ZH_SHORT keylen, ZH_BOOL fNext,
                              ZH_ULONG ulRecNo, ZH_BOOL * fStop )
{
   int iLast = -1, iBegin = 0, iEnd = pPage->uiKeys - 1;

   *fStop = ZH_FALSE;
   while( iBegin <= iEnd )
   {
      int i, k;

      i = ( iBegin + iEnd ) >> 1;
      k = zh_ntxValCompare( pTag, key, keylen, zh_ntxGetKeyVal( pPage, i ),
                            pTag->KeyLength, ZH_FALSE );
      if( k == 0 )
      {
         if( ulRecNo != 0 && pTag->fSortRec )
         {
            ZH_ULONG ulRec = zh_ntxGetKeyRec( pPage, i );
            if( ulRecNo < ulRec )
               k = -1;
            else if( ulRecNo > ulRec )
               k = 1;
            else
            {
               *fStop = ZH_TRUE;
               return i;
            }
         }
      }
      else if( ! pTag->AscendKey )
         k = -k;
      if( fNext ? k >= 0 : k > 0 )
         iBegin = i + 1;
      else
      {
         if( k == 0 && ! ulRecNo )
            *fStop = ZH_TRUE;
         iLast = i;
         iEnd = i - 1;
      }
   }
   return iLast >= 0 ? iLast : ( int ) pPage->uiKeys;
}

/*
 * find a record in page starting from given key
 */
static ZH_BOOL zh_ntxPageFindRecNo( LPPAGEINFO pPage, int * iStart, ZH_ULONG ulRecno )
{
   int iKey = *iStart;

   while( iKey < pPage->uiKeys )
   {
      if( zh_ntxGetKeyRec( pPage, iKey ) == ulRecno )
      {
         *iStart = iKey;
         return ZH_TRUE;
      }
      iKey++;
   }
   return ZH_FALSE;
}

/*
 * set page path to given key in tag
 */
static ZH_BOOL zh_ntxTagKeyFind( LPTAGINFO pTag, LPKEYINFO pKey, ZH_USHORT uiLen )
{
   LPPAGEINFO pPage = NULL;
   ZH_ULONG ulPage = 0, ulRecNo = 0;
   int iKey;
   ZH_BOOL fStop = ZH_FALSE, fNext = ZH_FALSE, fPrev = ZH_FALSE, fOut = ZH_FALSE;

   if( pKey->Tag == NTX_MAX_REC_NUM )          /* for key add */
   {
      if( pTag->fSortRec )
         ulRecNo = pKey->Xtra;
      else
         fNext = ZH_TRUE;
   }
   else if( pKey->Xtra == NTX_MAX_REC_NUM )    /* for seek last */
      fNext = fPrev = ZH_TRUE;
   else if( pKey->Xtra != NTX_IGNORE_REC_NUM ) /* for key del and current key */
      ulRecNo = pKey->Xtra;
   /* else -> normal seek */

   pTag->stackLevel = 0;
   do
   {
      if( pPage )
         zh_ntxPageRelease( pTag, pPage );
      pPage = zh_ntxPageLoad( pTag, ulPage );
      if( ! pPage )
         return ZH_FALSE;
      iKey = zh_ntxPageKeyFind( pTag, pPage, pKey->key, uiLen, fNext, ulRecNo, &fStop );
      zh_ntxTagSetPageStack( pTag, pPage->Page, ( ZH_USHORTCAST ) iKey );
      if( fStop && ulRecNo && pTag->fSortRec )
         break;
      ulPage = zh_ntxGetKeyPage( pPage, iKey );
   }
   while( ulPage != 0 );

   if( ulRecNo && ! pTag->fSortRec ) /* small hack - should speedup in some cases */
   {
      if( zh_ntxPageFindRecNo( pPage, &iKey, ulRecNo ) )
         pTag->stack[ pTag->stackLevel - 1 ].ikey = ( ZH_SHORTCAST ) iKey;
   }

   zh_ntxPageGetKey( pPage, ( ZH_USHORTCAST ) iKey, pTag->CurKeyInfo, pTag->KeyLength );
   zh_ntxPageRelease( pTag, pPage );

   if( ulRecNo )
   {
      if( ! pTag->fSortRec )
      {
         fStop = ZH_TRUE;
         while( fStop && ulRecNo != pTag->CurKeyInfo->Xtra )
         {
            if( ! zh_ntxTagNextKey( pTag ) ) /* Tag EOF */
            {
               fOut = ZH_TRUE;
               fStop = ZH_FALSE;
            }
            else
            {
               fStop = zh_ntxValCompare( pTag, pKey->key, uiLen,
                                         pTag->CurKeyInfo->key, pTag->KeyLength,
                                         ZH_FALSE ) == 0;
            }
         }
      }
   }
   else if( fPrev )
   {
      if( ! zh_ntxTagPrevKey( pTag ) )
      {
         fOut = ZH_TRUE;
         fStop = ZH_FALSE;
      }
      else
      {
         fStop = zh_ntxValCompare( pTag, pKey->key, uiLen, pTag->CurKeyInfo->key,
                                   pTag->KeyLength, ZH_FALSE ) == 0;
      }
   }
   else if( ! fNext && ! fStop && pTag->CurKeyInfo->Xtra == 0 )
   {
      if( ! zh_ntxTagNextKey( pTag ) ) /* Tag EOF */
      {
         fOut = ZH_TRUE;
         fStop = ZH_FALSE;
      }
      else
      {
         fStop = zh_ntxValCompare( pTag, pKey->key, uiLen,
                                   pTag->CurKeyInfo->key, pTag->KeyLength,
                                   ZH_FALSE ) == 0;
      }
   }

   pTag->TagBOF = pTag->TagEOF = fOut || pTag->CurKeyInfo->Xtra == 0;

   return fStop;
}

/*
 * set key in the given tag page
 */
static void zh_ntxPageKeySet( LPTAGINFO pTag, LPPAGEINFO pPage, ZH_USHORT uiPos,
                              ZH_ULONG ulPage, ZH_ULONG ulRec, const char * keyVal )
{
   zh_ntxSetKeyPage( pPage, uiPos, ulPage );
   zh_ntxSetKeyRec( pPage, uiPos, ulRec );
   memcpy( zh_ntxGetKeyVal( pPage, uiPos ), keyVal, pTag->KeyLength );
   pPage->Changed = ZH_TRUE;
}

/*
 * add key to tag page
 */
static void zh_ntxPageKeyAdd( LPTAGINFO pTag, LPPAGEINFO pPage, ZH_USHORT uiPos,
                              ZH_ULONG ulPage, ZH_ULONG ulRec, const char * keyVal )
{
   ZH_USHORT u, ntmp = zh_ntxGetKeyOffset( pPage, pPage->uiKeys + 1 );

   /* TODO?: update to keep last key pointer fixed */
   for( u = pPage->uiKeys + 1; u > uiPos; u-- )
   {
      zh_ntxSetKeyOffset( pPage, u, zh_ntxGetKeyOffset( pPage, u - 1 ) );
   }
   zh_ntxSetKeyOffset( pPage, uiPos, ntmp );
   pPage->uiKeys++;

   zh_ntxPageKeySet( pTag, pPage, uiPos, ulPage, ulRec, keyVal );
#ifdef ZH_NTX_DEBUG
   zh_ntxPageCheckKeys( pPage, pTag, uiPos, 41 );
#endif
}

/*
 * del key from the page
 */
static void zh_ntxPageKeyDel( LPPAGEINFO pPage, ZH_USHORT uiPos )
{
   ZH_USHORT u, ntmp = zh_ntxGetKeyOffset( pPage, uiPos );

   /* TODO?: update to keep last key pointer fixed */
   for( u = uiPos; u < pPage->uiKeys; u++ )
      zh_ntxSetKeyOffset( pPage, u, zh_ntxGetKeyOffset( pPage, u + 1 ) );
   zh_ntxSetKeyOffset( pPage, pPage->uiKeys, ntmp );

   pPage->uiKeys--;
   pPage->Changed = ZH_TRUE;
}

/*
 * split single page into two and return key to the new one
 */
static LPKEYINFO zh_ntxPageSplit( LPTAGINFO pTag, LPPAGEINFO pPage,
                                  LPKEYINFO pKey, ZH_USHORT uiPos )
{
   LPPAGEINFO pNewPage = zh_ntxPageNew( pTag, ZH_FALSE );
   LPKEYINFO pKeyNew;
   ZH_USHORT uiKeys = pPage->uiKeys + 1, uiLen = pTag->KeyLength + 8,
          i, j, u, uiHalf;
   ZH_ULONG ulPage;

   if( ! pNewPage )
      return NULL;
   pKeyNew = zh_ntxKeyNew( NULL, pTag->KeyLength );

   uiHalf = uiKeys >> 1;

   j = 0;
   while( pNewPage->uiKeys < uiHalf )
   {
      if( pNewPage->uiKeys == uiPos )
      {
         zh_ntxSetKeyPage( pNewPage, pNewPage->uiKeys, pKey->Tag );
         zh_ntxSetKeyRec( pNewPage, pNewPage->uiKeys, pKey->Xtra );
         memcpy( zh_ntxGetKeyVal( pNewPage, pNewPage->uiKeys ), pKey->key, pTag->KeyLength );
      }
      else
      {
         memcpy( zh_ntxGetKeyPtr( pNewPage, pNewPage->uiKeys ),
                 zh_ntxGetKeyPtr( pPage, j ), uiLen );
         j++;
      }
      pNewPage->uiKeys++;
   }

   if( uiHalf == uiPos )
   {
      pKeyNew->Xtra = pKey->Xtra;
      memcpy( pKeyNew->key, pKey->key, pTag->KeyLength );
      zh_ntxSetKeyPage( pNewPage, pNewPage->uiKeys, pKey->Tag );
   }
   else
   {
      pKeyNew->Xtra = zh_ntxGetKeyRec( pPage, j );
      memcpy( pKeyNew->key, zh_ntxGetKeyVal( pPage, j ), pTag->KeyLength );
      zh_ntxSetKeyPage( pNewPage, pNewPage->uiKeys, zh_ntxGetKeyPage( pPage, j ) );
      j++;
   }
   pKeyNew->Tag = pNewPage->Page;

   i = 0;
   while( ++uiHalf < uiKeys )
   {
      if( uiHalf == uiPos )
      {
         zh_ntxSetKeyPage( pPage, i, pKey->Tag );
         zh_ntxSetKeyRec( pPage, i, pKey->Xtra );
         memcpy( zh_ntxGetKeyVal( pPage, i ), pKey->key, pTag->KeyLength );
      }
      else
      {
         u = zh_ntxGetKeyOffset( pPage, j );
         zh_ntxSetKeyOffset( pPage, j, zh_ntxGetKeyOffset( pPage, i ) );
         zh_ntxSetKeyOffset( pPage, i, u );
         j++;
      }
      i++;
   }
   ulPage = zh_ntxGetKeyPage( pPage, pPage->uiKeys );
   zh_ntxSetKeyPage( pPage, pPage->uiKeys, 0 );
   zh_ntxSetKeyPage( pPage, i, ulPage );
   pPage->uiKeys = i;

   pPage->Changed = pNewPage->Changed = ZH_TRUE;
#ifdef ZH_NTX_DEBUG
   zh_ntxPageCheckKeys( pNewPage, pTag, uiPos, 1 );
   zh_ntxPageCheckKeys( pPage, pTag, uiPos - pNewPage->uiKeys, 2 );
#endif
   zh_ntxPageRelease( pTag, pNewPage );

   return pKeyNew;
}

/*
 * join two neighbour pages and update the parent page key
 */
static void zh_ntxPageJoin( LPTAGINFO pTag, LPPAGEINFO pBasePage, ZH_USHORT uiPos,
                            LPPAGEINFO pFirst, LPPAGEINFO pLast )
{
   ZH_USHORT uiLen = pTag->KeyLength + 8, i;

   zh_ntxSetKeyRec( pFirst, pFirst->uiKeys, zh_ntxGetKeyRec( pBasePage, uiPos ) );
   memcpy( zh_ntxGetKeyVal( pFirst, pFirst->uiKeys ),
           zh_ntxGetKeyVal( pBasePage, uiPos ), pTag->KeyLength );
   pFirst->uiKeys++;
   zh_ntxPageKeyDel( pBasePage, uiPos );
   zh_ntxSetKeyPage( pBasePage, uiPos, pFirst->Page );
   for( i = 0; i < pLast->uiKeys; i++ )
   {
      memcpy( zh_ntxGetKeyPtr( pFirst, pFirst->uiKeys ),
              zh_ntxGetKeyPtr( pLast, i ), uiLen );
      pFirst->uiKeys++;
   }
   zh_ntxSetKeyPage( pFirst, pFirst->uiKeys, zh_ntxGetKeyPage( pLast, pLast->uiKeys ) );
   pLast->uiKeys = 0;
   zh_ntxPageFree( pTag, pLast );
   pFirst->Changed = pLast->Changed = ZH_TRUE;
#ifdef ZH_NTX_DEBUG
   zh_ntxPageCheckKeys( pBasePage, pTag, uiPos, 11 );
   zh_ntxPageCheckKeys( pFirst, pTag, 0, 12 );
#endif
}

/*
 * balance keys in two neighbour pages and update the parent page key
 */
static void zh_ntxBalancePages( LPTAGINFO pTag, LPPAGEINFO pBasePage, ZH_USHORT uiPos,
                                LPPAGEINFO pFirst, LPPAGEINFO pLast )
{
   ZH_USHORT uiLen = pTag->KeyLength + 8, n;
   int i, j, iMove = ( ( pFirst->uiKeys + pLast->uiKeys + 1 ) >> 1 ) - pFirst->uiKeys;

   /*
    * such situation should not exist even max keys, though it does not cost
    * much and I want to be able to call zh_ntxBalancePages() in any case for
    * some advanced balancing
    */
   if( iMove == 0 )
      return;

#ifdef ZH_NTX_DEBUG
   zh_ntxPageCheckKeys( pBasePage, pTag, uiPos, 31 );
   zh_ntxPageCheckKeys( pFirst, pTag, iMove, 32 );
   zh_ntxPageCheckKeys( pLast, pTag, iMove, 33 );
#endif

   if( iMove > 0 )
   {
      zh_ntxSetKeyRec( pFirst, pFirst->uiKeys, zh_ntxGetKeyRec( pBasePage, uiPos ) );
      memcpy( zh_ntxGetKeyVal( pFirst, pFirst->uiKeys ),
              zh_ntxGetKeyVal( pBasePage, uiPos ), pTag->KeyLength );
      pFirst->uiKeys++;
      i = 0;
      while( --iMove )
      {
         memcpy( zh_ntxGetKeyPtr( pFirst, pFirst->uiKeys ),
                 zh_ntxGetKeyPtr( pLast, i ), uiLen );
         pFirst->uiKeys++;
         i++;
      }
      zh_ntxSetKeyRec( pBasePage, uiPos, zh_ntxGetKeyRec( pLast, i ) );
      memcpy( zh_ntxGetKeyVal( pBasePage, uiPos ),
              zh_ntxGetKeyVal( pLast, i ), pTag->KeyLength );
      zh_ntxSetKeyPage( pFirst, pFirst->uiKeys, zh_ntxGetKeyPage( pLast, i ) );
      i++;
      pLast->uiKeys -= ( ZH_USHORTCAST ) i;
      /* TODO?: update to keep last key pointer fixed */
      for( j = 0; j <= pLast->uiKeys; j++ )
      {
         n = zh_ntxGetKeyOffset( pLast, j );
         zh_ntxSetKeyOffset( pLast, j, zh_ntxGetKeyOffset( pLast, j + i ) );
         zh_ntxSetKeyOffset( pLast, j + i, n );
      }
   }
   else
   {
      /* TODO?: update to keep last key pointer fixed */
      for( j = pLast->uiKeys; j >= 0; j-- )
      {
         n = zh_ntxGetKeyOffset( pLast, j - iMove );
         zh_ntxSetKeyOffset( pLast, j - iMove, zh_ntxGetKeyOffset( pLast, j ) );
         zh_ntxSetKeyOffset( pLast, j, n );
      }
      i = -iMove - 1;
      zh_ntxSetKeyRec( pLast, i, zh_ntxGetKeyRec( pBasePage, uiPos ) );
      memcpy( zh_ntxGetKeyVal( pLast, i ),
              zh_ntxGetKeyVal( pBasePage, uiPos ), pTag->KeyLength );
      zh_ntxSetKeyPage( pLast, i, zh_ntxGetKeyPage( pFirst, pFirst->uiKeys ) );
      while( --i >= 0 )
      {
         pFirst->uiKeys--;
         memcpy( zh_ntxGetKeyPtr( pLast, i ),
                 zh_ntxGetKeyPtr( pFirst, pFirst->uiKeys ), uiLen );
      }
      pLast->uiKeys -= ( ZH_USHORTCAST ) iMove;
      pFirst->uiKeys--;
      zh_ntxSetKeyRec( pBasePage, uiPos, zh_ntxGetKeyRec( pFirst, pFirst->uiKeys ) );
      memcpy( zh_ntxGetKeyVal( pBasePage, uiPos ),
              zh_ntxGetKeyVal( pFirst, pFirst->uiKeys ), pTag->KeyLength );
   }
   pFirst->Changed = pLast->Changed = pBasePage->Changed = ZH_TRUE;
#ifdef ZH_NTX_DEBUG
   zh_ntxPageCheckKeys( pBasePage, pTag, uiPos, 21 );
   zh_ntxPageCheckKeys( pFirst, pTag, iMove, 22 );
   zh_ntxPageCheckKeys( pLast, pTag, iMove, 23 );
#endif
}

/*
 * add key to the index at the current page path
 */
static ZH_BOOL zh_ntxTagKeyAdd( LPTAGINFO pTag, LPKEYINFO pKey )
{
   int iLevel, iKey;
   LPPAGEINFO pPage = NULL;
   LPKEYINFO pNewKey = NULL;
   ZH_BOOL fFound, fBottom = ZH_FALSE;

   if( pTag->UniqueKey )
   {
      ZH_ULONG ulRecNo = pKey->Xtra;

      pKey->Xtra = NTX_IGNORE_REC_NUM;
      fFound = zh_ntxTagKeyFind( pTag, pKey, pTag->KeyLength );
      pKey->Xtra = ulRecNo;
      if( fFound )
         return ZH_FALSE;
      fBottom = ZH_TRUE;
   }
   else
   {
      pKey->Tag = NTX_MAX_REC_NUM;
      fFound = zh_ntxTagKeyFind( pTag, pKey, pTag->KeyLength );
      pKey->Tag = 0;
      if( fFound )
      {
         if( pTag->MultiKey )
            fBottom = ZH_TRUE;
         else
            return ZH_FALSE;
      }
   }

   iLevel = pTag->stackLevel - 1;
   if( fBottom )
   {
      ZH_ULONG ulPage;
      pPage = zh_ntxPageLoad( pTag, pTag->stack[ iLevel ].page );
      if( ! pPage )
         return ZH_FALSE;
      ulPage = zh_ntxGetKeyPage( pPage, pTag->stack[ iLevel ].ikey );
      if( ulPage )
      {
         zh_ntxPageRelease( pTag, pPage );
         pPage = zh_ntxPageBottomMove( pTag, ulPage );
         if( ! pPage )
            return ZH_FALSE;
         iLevel = pTag->stackLevel - 1;
         if( pTag->stack[ iLevel ].ikey < ( ZH_SHORT ) pPage->uiKeys )
            pTag->stack[ iLevel ].ikey++;
      }
   }

   pTag->CurKeyInfo = zh_ntxKeyCopy( pTag->CurKeyInfo, pKey, pTag->KeyLength );

   while( iLevel >= 0 && pKey )
   {
      if( pPage )
         zh_ntxPageRelease( pTag, pPage );
      pPage = zh_ntxPageLoad( pTag, pTag->stack[ iLevel ].page );
      if( ! pPage )
      {
         if( pNewKey )
            zh_ntxKeyFree( pNewKey );
         pTag->stackLevel = 0;
         return ZH_FALSE;
      }
      iKey = pTag->stack[ iLevel ].ikey;
      if( pPage->uiKeys < pTag->MaxKeys )
      {
         zh_ntxPageKeyAdd( pTag, pPage, ( ZH_USHORTCAST ) iKey, pKey->Tag, pKey->Xtra, pKey->key );
         pKey = NULL;
      }
      else
      {
         pTag->stackLevel = 0;
#if defined( ZH_NTX_STRONG_BALANCE )
         if( iLevel > 0 )
         {
            LPPAGEINFO pBasePage;
            ZH_USHORT uiFirst, uiLast, uiBaseKey;
            pBasePage = zh_ntxPageLoad( pTag, pTag->stack[ iLevel - 1 ].page );
            if( ! pBasePage )
            {
               zh_ntxPageRelease( pTag, pPage );
               if( pNewKey )
                  zh_ntxKeyFree( pNewKey );
               return ZH_FALSE;
            }
            uiFirst = uiLast = uiBaseKey = pTag->stack[ iLevel -1 ].ikey;
            if( uiLast < pBasePage->uiKeys && zh_ntxGetKeyPage( pBasePage, uiLast + 1 ) != 0 )
               uiLast++;
            else if( uiFirst > 0 && zh_ntxGetKeyPage( pBasePage, uiFirst - 1 ) != 0 )
               uiFirst--;
            if( uiFirst != uiLast )
            {
               LPPAGEINFO pFirst, pLast;

               if( uiFirst == uiBaseKey )
               {
                  pFirst = pPage;
                  pLast = zh_ntxPageLoad( pTag, zh_ntxGetKeyPage( pBasePage, uiLast ) );
                  if( ! pLast )
                  {
                     zh_ntxPageRelease( pTag, pPage );
                     zh_ntxPageRelease( pTag, pBasePage );
                     if( pNewKey )
                        zh_ntxKeyFree( pNewKey );
                     return ZH_FALSE;
                  }
                  uiBaseKey = ( ZH_USHORTCAST ) iKey;
               }
               else
               {
                  pLast = pPage;
                  pFirst = zh_ntxPageLoad( pTag, zh_ntxGetKeyPage( pBasePage, uiFirst ) );
                  if( ! pFirst )
                  {
                     zh_ntxPageRelease( pTag, pPage );
                     zh_ntxPageRelease( pTag, pBasePage );
                     if( pNewKey )
                        zh_ntxKeyFree( pNewKey );
                     return ZH_FALSE;
                  }
                  uiBaseKey = pFirst->uiKeys + ( ZH_USHORTCAST ) iKey + 1;
               }
               if( ( pFirst->uiKeys + pLast->uiKeys ) <= ( ( pTag->MaxKeys - 1 ) << 1 ) )
               {
                  zh_ntxBalancePages( pTag, pBasePage, uiFirst, pFirst, pLast );
                  if( pFirst->uiKeys >= uiBaseKey )
                     zh_ntxPageKeyAdd( pTag, pFirst, uiBaseKey, pKey->Tag, pKey->Xtra, pKey->key );
                  else
                     zh_ntxPageKeyAdd( pTag, pLast, uiBaseKey - pFirst->uiKeys - 1, pKey->Tag, pKey->Xtra, pKey->key );
                  pKey = NULL;
               }
               if( pFirst != pPage )
                  zh_ntxPageRelease( pTag, pFirst );
               else
                  zh_ntxPageRelease( pTag, pLast );
               zh_ntxPageRelease( pTag, pBasePage );
               if( ! pKey )
                  break;
            }
         }
#endif
         pKey = zh_ntxPageSplit( pTag, pPage, pKey, ( ZH_USHORTCAST ) iKey );
         if( pNewKey )
            zh_ntxKeyFree( pNewKey );
         pNewKey = pKey;
      }
      iLevel--;
   }
   zh_ntxPageRelease( pTag, pPage );
   if( pKey )
   {
      pPage = zh_ntxPageNew( pTag, ZH_FALSE );
      if( ! pPage )
         return ZH_FALSE;
      zh_ntxPageKeyAdd( pTag, pPage, 0, pKey->Tag, pKey->Xtra, pKey->key );
      zh_ntxSetKeyPage( pPage, 1, pTag->RootBlock );
      pTag->RootBlock = pPage->Page;
      pTag->HdrChanged = ZH_TRUE;
      zh_ntxPageRelease( pTag, pPage );
      pTag->stackLevel = 0;
   }
   if( pNewKey )
      zh_ntxKeyFree( pNewKey );
   return ZH_TRUE;
}

/*
 * del key at the current page path from the index
 */
static ZH_BOOL zh_ntxTagKeyDel( LPTAGINFO pTag, LPKEYINFO pKey )
{
   int iLevel, iBaseKey, iKey;
   LPPAGEINFO pBasePage, pPage;
   ZH_ULONG ulPage;

   pKey->Tag = 0;
   if( pTag->stackLevel == 0 || pTag->CurKeyInfo->Xtra != pKey->Xtra ||
       memcmp( pTag->CurKeyInfo->key, pKey->key, pTag->KeyLength ) != 0 )
   {
      if( ! zh_ntxTagKeyFind( pTag, pKey, pTag->KeyLength ) )
         return ZH_FALSE;
   }

   iLevel = pTag->stackLevel - 1;

   pPage = zh_ntxPageLoad( pTag, pTag->stack[ iLevel ].page );
   if( ! pPage )
      return ZH_FALSE;
   iKey = pTag->stack[ iLevel ].ikey;
   ulPage = zh_ntxGetKeyPage( pPage, iKey );

   if( ulPage )
   {
      pBasePage = pPage;
      iBaseKey = iKey;
      pPage = zh_ntxPageBottomMove( pTag, ulPage );
      if( ! pPage )
      {
         zh_ntxPageRelease( pTag, pBasePage );
         return ZH_FALSE;
      }
      iLevel = pTag->stackLevel - 1;
      iKey = pTag->stack[ iLevel ].ikey;

      zh_ntxSetKeyRec( pBasePage, iBaseKey, zh_ntxGetKeyRec( pPage, iKey ) );
      memcpy( zh_ntxGetKeyVal( pBasePage, iBaseKey ),
              zh_ntxGetKeyVal( pPage, iKey ), pTag->KeyLength );
      pBasePage->Changed = ZH_TRUE;
#ifdef ZH_NTX_DEBUG
      zh_ntxPageCheckKeys( pBasePage, pTag, iBaseKey, 61 );
#endif
      zh_ntxPageRelease( pTag, pBasePage );
   }
   zh_ntxPageKeyDel( pPage, ( ZH_USHORTCAST ) iKey );

   while( iLevel > 0 )
   {
      if( pPage->uiKeys < ( pTag->MaxKeys >> 1 ) )
      {
         ZH_USHORT uiFirst, uiLast, uiBaseKey;

         pBasePage = zh_ntxPageLoad( pTag, pTag->stack[ iLevel -1 ].page );
         if( ! pBasePage )
         {
            zh_ntxPageRelease( pTag, pPage );
            return ZH_FALSE;
         }
         uiFirst = uiLast = uiBaseKey = pTag->stack[ iLevel - 1 ].ikey;
         if( uiLast < pBasePage->uiKeys && zh_ntxGetKeyPage( pBasePage, uiLast + 1 ) != 0 )
            uiLast++;
         else if( uiFirst > 0 && zh_ntxGetKeyPage( pBasePage, uiFirst - 1 ) != 0 )
            uiFirst--;

         if( uiFirst == uiLast )
         {
            if( pPage->uiKeys == 0 )
            {
               zh_ntxSetKeyPage( pBasePage, uiBaseKey, 0 );
               zh_ntxPageFree( pTag, pPage );
            }
            zh_ntxPageRelease( pTag, pPage );
         }
         else
         {
            LPPAGEINFO pFirst, pLast;

            if( uiFirst == uiBaseKey )
            {
               pFirst = pPage;
               pLast = zh_ntxPageLoad( pTag, zh_ntxGetKeyPage( pBasePage, uiLast ) );
               if( ! pLast )
               {
                  zh_ntxPageRelease( pTag, pPage );
                  zh_ntxPageRelease( pTag, pBasePage );
                  pTag->stackLevel = 0;
                  return ZH_FALSE;
               }
            }
            else
            {
               pLast = pPage;
               pFirst = zh_ntxPageLoad( pTag, zh_ntxGetKeyPage( pBasePage, uiFirst ) );
               if( ! pFirst )
               {
                  zh_ntxPageRelease( pTag, pPage );
                  zh_ntxPageRelease( pTag, pBasePage );
                  pTag->stackLevel = 0;
                  return ZH_FALSE;
               }
            }
            if( pFirst->uiKeys + pLast->uiKeys < pTag->MaxKeys )
               zh_ntxPageJoin( pTag, pBasePage, uiFirst, pFirst, pLast );
            else
               zh_ntxBalancePages( pTag, pBasePage, uiFirst, pFirst, pLast );
            zh_ntxPageRelease( pTag, pFirst );
            zh_ntxPageRelease( pTag, pLast );
         }
         pPage = pBasePage;
      }
      else
         break;
      iLevel--;
   }

   if( pPage->uiKeys == 0 && pPage->Page == pTag->RootBlock )
   {
      ulPage = zh_ntxGetKeyPage( pPage, 0 );
      if( ulPage != 0 )
      {
         pTag->RootBlock = ulPage;
         pTag->HdrChanged = ZH_TRUE;
         zh_ntxPageFree( pTag, pPage );
      }
   }
   zh_ntxPageRelease( pTag, pPage );
   pTag->stackLevel = 0;
   return ZH_TRUE;
}

/*
 * Skip in tag respecting record filter only
 */
static void zh_ntxTagSkipFilter( LPTAGINFO pTag, ZH_BOOL fForward )
{
   ZH_BOOL fBack, fEof = fForward ? pTag->TagEOF : pTag->TagBOF;

   fBack = pTag->fUsrDescend == pTag->AscendKey ? fForward : ! fForward;

   while( ! fEof && ! zh_ntxCheckRecordScope( pTag->pIndex->pArea,
                                              pTag->CurKeyInfo->Xtra ) )
   {
      if( fBack )
         fEof = ! zh_ntxTagPrevKey( pTag );
      else
         fEof = ! zh_ntxTagNextKey( pTag );

      if( ! fEof && ! zh_ntxKeyInScope( pTag, pTag->CurKeyInfo ) )
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
static void zh_ntxTagGoTop( LPTAGINFO pTag )
{
   PZH_NTXSCOPE pScope = pTag->fUsrDescend ? &pTag->bottom : &pTag->top;

   if( pScope->scopeKeyLen )
      zh_ntxTagKeyFind( pTag, pScope->scopeKey, pScope->scopeKeyLen );
   else if( pTag->fUsrDescend == pTag->AscendKey )
      zh_ntxTagBottomKey( pTag );
   else
      zh_ntxTagTopKey( pTag );

   pTag->TagEOF = pTag->CurKeyInfo->Xtra == 0 ||
                  ! zh_ntxKeyInScope( pTag, pTag->CurKeyInfo );

   if( ! pTag->TagEOF && pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter )
      zh_ntxTagSkipFilter( pTag, ZH_TRUE );

   pTag->TagBOF = pTag->TagEOF;
}

/*
 * go to the last visible record in Tag
 */
static void zh_ntxTagGoBottom( LPTAGINFO pTag )
{
   PZH_NTXSCOPE pScope = pTag->fUsrDescend ? &pTag->top : &pTag->bottom;

   if( pScope->scopeKeyLen )
      zh_ntxTagKeyFind( pTag, pScope->scopeKey, pScope->scopeKeyLen );
   else if( pTag->fUsrDescend == pTag->AscendKey )
      zh_ntxTagTopKey( pTag );
   else
      zh_ntxTagBottomKey( pTag );

   pTag->TagBOF = pTag->CurKeyInfo->Xtra == 0 ||
                  ! zh_ntxKeyInScope( pTag, pTag->CurKeyInfo );

   if( ! pTag->TagBOF && pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter )
      zh_ntxTagSkipFilter( pTag, ZH_FALSE );

   pTag->TagEOF = pTag->TagBOF;
}

/*
 * skip to Next Key in the Tag
 */
static void zh_ntxTagSkipNext( LPTAGINFO pTag )
{
   pTag->TagBOF = ZH_FALSE;

   if( pTag->stackLevel == 0 )
      pTag->TagEOF = ZH_TRUE;
   else if( ! zh_ntxInTopScope( pTag, pTag->CurKeyInfo->key ) )
      zh_ntxTagGoTop( pTag );
   else if( pTag->fUsrDescend == pTag->AscendKey )
      pTag->TagEOF = ! zh_ntxTagPrevKey( pTag );
   else
      pTag->TagEOF = ! zh_ntxTagNextKey( pTag );

   if( ! pTag->TagEOF && ! zh_ntxKeyInScope( pTag, pTag->CurKeyInfo ) )
      pTag->TagEOF = ZH_TRUE;

   if( ! pTag->TagEOF && pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter )
      zh_ntxTagSkipFilter( pTag, ZH_TRUE );
}

/*
 * skip to Previous Key in the Tag
 */
static void zh_ntxTagSkipPrev( LPTAGINFO pTag )
{
   pTag->TagEOF = ZH_FALSE;

   if( pTag->stackLevel == 0 )
      /* TODO?: check if this is NTX behavior,
         for sure CDX works in such way */
      zh_ntxTagGoBottom( pTag );
   else if( pTag->fUsrDescend == pTag->AscendKey )
      pTag->TagBOF = ! zh_ntxTagNextKey( pTag );
   else
      pTag->TagBOF = ! zh_ntxTagPrevKey( pTag );

   if( ! pTag->TagBOF && ! zh_ntxKeyInScope( pTag, pTag->CurKeyInfo ) )
      pTag->TagBOF = ZH_TRUE;

   if( ! pTag->TagBOF && pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter )
      zh_ntxTagSkipFilter( pTag, ZH_FALSE );
}

/*
 * count keys in the given page and all subpages
 */
static ZH_ULONG zh_ntxPageCountKeys( LPTAGINFO pTag, ZH_ULONG ulPage )
{
   LPPAGEINFO pPage = zh_ntxPageLoad( pTag, ulPage );
   ZH_ULONG ulKeys;
   ZH_USHORT u;

   if( ! pPage )
      return 0;

   ulKeys = pPage->uiKeys;
   for( u = 0; u <= pPage->uiKeys; u++ )
   {
      ulPage = zh_ntxGetKeyPage( pPage, u );
      if( ulPage )
         ulKeys += zh_ntxPageCountKeys( pTag, ulPage );
   }
   zh_ntxPageRelease( pTag, pPage );

   return ulKeys;
}

/*
 * count relative position of current location in page stack
 */
static double zh_ntxTagCountRelKeyPos( LPTAGINFO pTag )
{
   int iLevel = pTag->stackLevel;
   double dPos = 1.0;

   while( --iLevel >= 0 )
   {
      LPPAGEINFO pPage = zh_ntxPageLoad( pTag, pTag->stack[ iLevel ].page );
      int iKeys;
      if( ! pPage )
         break;
      iKeys = pPage->uiKeys;
      if( zh_ntxGetKeyPage( pPage, pPage->uiKeys ) )
         ++iKeys;
      else if( iLevel == pTag->stackLevel - 1 )
         dPos = 0.5;
      if( iKeys )
         dPos = ( dPos + pTag->stack[ iLevel ].ikey ) / iKeys;
      zh_ntxPageRelease( pTag, pPage );
   }
   if( pTag->fUsrDescend == pTag->AscendKey )
      dPos = 1.0 - dPos;
   return dPos;
}

static void zh_ntxTagGoToRelKeyPos( LPTAGINFO pTag, double dPos )
{
   LPPAGEINFO pPage = NULL;
   ZH_ULONG ulPage = 0;
   int iKey, iKeys;

   if( pTag->fUsrDescend == pTag->AscendKey )
      dPos = 1.0 - dPos;

   pTag->stackLevel = 0;
   do
   {
      if( pPage )
         zh_ntxPageRelease( pTag, pPage );
      pPage = zh_ntxPageLoad( pTag, ulPage );
      if( ! pPage )
      {
         pTag->stackLevel = 0;
         return;
      }
      if( pPage->uiKeys == 0 )
         iKey = 0;
      else
      {
         iKeys = pPage->uiKeys;
         if( zh_ntxGetKeyPage( pPage, pPage->uiKeys ) )
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
      zh_ntxTagSetPageStack( pTag, pPage->Page, ( ZH_USHORTCAST ) iKey );
      ulPage = zh_ntxGetKeyPage( pPage, iKey );
   }
   while( ulPage != 0 );

   zh_ntxPageGetKey( pPage, ( ZH_USHORTCAST ) iKey, pTag->CurKeyInfo, pTag->KeyLength );
   zh_ntxPageRelease( pTag, pPage );

   if( dPos > 0.75 )
      zh_ntxTagNextKey( pTag );
   else if( dPos < 0.25 )
      zh_ntxTagPrevKey( pTag );
}

/*
 * refresh CurKey value and set proper path from RootPage to LeafPage
 */
static ZH_BOOL zh_ntxCurKeyRefresh( LPTAGINFO pTag )
{
   NTXAREAP pArea = pTag->pIndex->pArea;

   if( pArea->dbfarea.lpdbPendingRel )
      SELF_FORCEREL( &pArea->dbfarea.area );

   if( ! pArea->dbfarea.fPositioned )
   {
      pTag->stackLevel = 0;
      pTag->TagBOF = pTag->TagEOF = ZH_TRUE;
      pTag->CurKeyInfo->Xtra = 0;
      return ZH_FALSE;
   }
   else if( pTag->stackLevel == 0 || pTag->CurKeyInfo->Xtra != pArea->dbfarea.ulRecNo )
   {
      ZH_BYTE buf[ NTX_MAX_KEY ];
      ZH_BOOL fBuf = ZH_FALSE;
      LPKEYINFO pKey = NULL;
      /* Try to find previous if it's key for the same record */
      if( pTag->CurKeyInfo->Xtra == pArea->dbfarea.ulRecNo )
      {
         fBuf = ZH_TRUE;
         memcpy( buf, pTag->CurKeyInfo->key, pTag->KeyLength );
         pKey = zh_ntxKeyCopy( pKey, pTag->CurKeyInfo, pTag->KeyLength );
         zh_ntxTagKeyFind( pTag, pKey, pTag->KeyLength );
      }
      if( pTag->CurKeyInfo->Xtra != pArea->dbfarea.ulRecNo )
      {
         ZH_BOOL fValidBuf = pArea->dbfarea.fValidBuffer;
         /* not found, create new key from DBF and if differs seek again */
         pKey = zh_ntxEvalKey( pKey, pTag );
         if( ! fBuf || memcmp( buf, pKey->key, pTag->KeyLength ) != 0 )
         {
            zh_ntxTagKeyFind( pTag, pKey, pTag->KeyLength );
         }
         /* not found, if key was generated from DBF buffer then force to
          * update it, create the new key and if differs seek again */
         if( pTag->CurKeyInfo->Xtra != pArea->dbfarea.ulRecNo && fValidBuf )
         {
            SELF_GOTO( &pArea->dbfarea.area, pArea->dbfarea.ulRecNo );
            memcpy( buf, pKey->key, pTag->KeyLength );
            pKey = zh_ntxEvalKey( pKey, pTag );
            if( memcmp( buf, pKey->key, pTag->KeyLength ) != 0 )
               zh_ntxTagKeyFind( pTag, pKey, pTag->KeyLength );
         }
         if( pTag->CurKeyInfo->Xtra != pArea->dbfarea.ulRecNo && pTag->Template )
         {
            zh_ntxTagGoTop( pTag );
            while( ! pTag->TagEOF )
            {
               if( pTag->CurKeyInfo->Xtra == pArea->dbfarea.ulRecNo )
                  break;
               zh_ntxTagSkipNext( pTag );
            }
         }
      }
      zh_ntxKeyFree( pKey );
      return pTag->CurKeyInfo->Xtra != 0 && pTag->CurKeyInfo->Xtra == pArea->dbfarea.ulRecNo;
   }
   pTag->TagBOF = pTag->TagEOF = ZH_FALSE;
   return ZH_TRUE;
}

/*
 * free pages allocated by tag
 */
static ZH_BOOL zh_ntxTagPagesFree( LPTAGINFO pTag, ZH_ULONG ulPage )
{
   LPPAGEINFO pPage = zh_ntxPageLoad( pTag, ulPage );
   ZH_BOOL fOK = pPage != NULL;
   ZH_USHORT u;

   for( u = 0; fOK && u <= pPage->uiKeys; u++ )
   {
      ulPage = zh_ntxGetKeyPage( pPage, u );
      if( ulPage )
         fOK = zh_ntxTagPagesFree( pTag, ulPage );
   }

   if( fOK )
   {
      pPage->uiKeys = 0;
      zh_ntxPageFree( pTag, pPage );
      if( ! pPage->pPrev )
         fOK = zh_ntxPageSave( pTag->pIndex, pPage );
   }
   zh_ntxPageRelease( pTag, pPage );

   return fOK;
}

/*
 * free space allocated by tag
 */
static ZH_ERRCODE zh_ntxTagSpaceFree( LPTAGINFO pTag )
{
   if( zh_ntxTagHeaderCheck( pTag ) )
   {
      if( pTag->RootBlock )
      {
         if( ! zh_ntxTagPagesFree( pTag, pTag->RootBlock ) )
            return ZH_FAILURE;
      }
      zh_ntxPageAddFree( pTag, pTag->HeadBlock );
      zh_ntxIndexTagDel( pTag->pIndex, pTag->TagName );
      pTag->pIndex->Changed = ZH_TRUE;
   }
   zh_ntxTagDelete( pTag );
   return ZH_SUCCESS;
}

/*
 * create index file name
 */
static void zh_ntxCreateFName( NTXAREAP pArea, const char * szBagName, ZH_BOOL * fProd,
                               char * szFileName, char * szTagName )
{
   PZH_FNAME pFileName;
   PZH_ITEM pExt = NULL;
   ZH_BOOL fName = szBagName && *szBagName;

   pFileName = zh_fsFNameSplit( fName ? szBagName : pArea->dbfarea.szDataFileName );

   if( szTagName )
   {
      if( pFileName->szName )
         zh_strncpyUpperTrim( szTagName, pFileName->szName, NTX_MAX_TAGNAME );
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
static LPNTXINDEX zh_ntxFindBag( NTXAREAP pArea, const char * szBagName )
{
   LPNTXINDEX pIndex;
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
static int zh_ntxFindTagByName( LPNTXINDEX pIndex, const char * szTag )
{
   int i;

   for( i = 0; i < pIndex->iTags; i++ )
   {
      if( ! zh_strnicmp( pIndex->lpTags[ i ]->TagName, szTag,
                         NTX_MAX_TAGNAME ) )
         return i + 1;
   }
   return 0;
}

/*
 * Find the tag by its name or number
 */
static LPTAGINFO zh_ntxFindTag( NTXAREAP pArea, PZH_ITEM pTagItem,
                                PZH_ITEM pBagItem )
{
   LPNTXINDEX pIndex;
   ZH_BOOL fBag;

   if( ! pTagItem ||
       ( zh_itemType( pTagItem ) & ( ZH_IT_STRING | ZH_IT_NUMERIC ) ) == 0 )
      return pArea->lpCurTag;

   fBag = ZH_IS_STRING( pTagItem ) && zh_itemGetCLen( pBagItem ) > 0;
   if( fBag )
   {
      pIndex = zh_ntxFindBag( pArea, zh_itemGetCPtr( pBagItem ) );
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
            iTag = zh_ntxFindTagByName( pIndex, szTag );
         else
         {
            do
            {
               iTag = zh_ntxFindTagByName( pIndex, szTag );
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
static int zh_ntxFindTagNum( NTXAREAP pArea, LPTAGINFO pTag )
{
   if( pArea->fSetTagNumbers )
   {
      LPNTXINDEX pIndex = pArea->lpIndexes;
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
static int zh_ntxTagCount( NTXAREAP pArea )
{
   LPNTXINDEX pIndex = pArea->lpIndexes;
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
static ZH_ULONG zh_ntxOrdKeyCount( LPTAGINFO pTag )
{
   ZH_ULONG ulKeyCount = 0;

   if( ! pTag->pIndex->fShared && pTag->keyCount &&
       ! pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter )
      return pTag->keyCount;

   if( zh_ntxTagLockRead( pTag ) )
   {
      zh_ntxTagRefreshScope( pTag );

      if( pTag->top.scopeKeyLen || pTag->bottom.scopeKeyLen ||
          pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter )
      {
         zh_ntxTagGoTop( pTag );
         while( ! pTag->TagEOF )
         {
            ulKeyCount++;
            zh_ntxTagSkipNext( pTag );
         }
      }
      else
      {
         ulKeyCount = zh_ntxPageCountKeys( pTag, 0 );
      }
      if( ! pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter )
         pTag->keyCount = ulKeyCount;
      zh_ntxTagUnLockRead( pTag );
   }

   return ulKeyCount;
}

/*
 * get the logical key position in the given tag
 */
static ZH_ULONG zh_ntxOrdKeyNo( LPTAGINFO pTag )
{
   ZH_ULONG ulKeyNo = 0;

   if( zh_ntxTagLockRead( pTag ) )
   {
      zh_ntxTagRefreshScope( pTag );
      if( zh_ntxCurKeyRefresh( pTag ) )
      {
         if( pTag->top.scopeKeyLen || pTag->bottom.scopeKeyLen ||
             pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter )
         {
            if( zh_ntxKeyInScope( pTag, pTag->CurKeyInfo ) )
            {
               do
               {
                  ulKeyNo++;
                  zh_ntxTagSkipPrev( pTag );
               }
               while( ! pTag->TagBOF );
            }
         }
         else
         {
            int iLevel = pTag->stackLevel, iKey, iFirst = 1;
            ZH_BOOL fBack = pTag->fUsrDescend == pTag->AscendKey;
            ZH_ULONG ulPage;

            while( --iLevel >= 0 )
            {
               LPPAGEINFO pPage = zh_ntxPageLoad( pTag, pTag->stack[ iLevel ].page );
               if( ! pPage )
                  break;
               if( fBack )
               {
                  iKey = pTag->stack[ iLevel ].ikey;
                  ulKeyNo += pPage->uiKeys - iKey;
                  while( ++iKey <= pPage->uiKeys )
                  {
                     ulPage = zh_ntxGetKeyPage( pPage, iKey );
                     if( ulPage )
                        ulKeyNo += zh_ntxPageCountKeys( pTag, ulPage );
                  }
               }
               else
               {
                  ulKeyNo += iKey = pTag->stack[ iLevel ].ikey + iFirst;
                  iFirst = 0;
                  while( --iKey >= 0 )
                  {
                     ulPage = zh_ntxGetKeyPage( pPage, iKey );
                     if( ulPage )
                        ulKeyNo += zh_ntxPageCountKeys( pTag, ulPage );
                  }
               }
               zh_ntxPageRelease( pTag, pPage );
            }
         }
      }
      zh_ntxTagUnLockRead( pTag );
   }
   return ulKeyNo;
}

/*
 * set logical key position in given tag
 */
static ZH_BOOL zh_ntxOrdKeyGoto( LPTAGINFO pTag, ZH_ULONG ulKeyNo )
{
   NTXAREAP pArea = pTag->pIndex->pArea;

   if( ! ulKeyNo || ! zh_ntxTagLockRead( pTag ) )
      return ZH_FALSE;

   zh_ntxTagRefreshScope( pTag );
   zh_ntxTagGoTop( pTag );
   while( ! pTag->TagEOF && --ulKeyNo )
   {
      zh_ntxTagSkipNext( pTag );
   }

   if( pTag->TagEOF )
   {
      SELF_GOTO( &pArea->dbfarea.area, 0 );
   }
   else
   {
      LPTAGINFO pSavedTag = pArea->lpCurTag;
      pArea->lpCurTag = pTag;
      if( SELF_GOTO( &pArea->dbfarea.area, pTag->CurKeyInfo->Xtra ) == ZH_SUCCESS )
         SELF_SKIPFILTER( &pArea->dbfarea.area, 1 );
      pArea->lpCurTag = pSavedTag;
   }
   zh_ntxTagUnLockRead( pTag );
   return ZH_TRUE;
}

/*
 * get the relative key position (from 0.0 to 1.0) in the given tag
 */
static double zh_ntxOrdGetRelKeyPos( LPTAGINFO pTag )
{
   double dPos = 0.0, dStart = 0.0, dStop = 1.0, dFact = 0.0000000000001;
   ZH_BOOL fOK = ZH_TRUE, fFilter = pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter;

   if( ! zh_ntxTagLockRead( pTag ) )
      return ZH_FALSE;

   zh_ntxTagRefreshScope( pTag );

   pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter = ZH_FALSE;
   if( pTag->fUsrDescend ? pTag->bottom.scopeKeyLen : pTag->top.scopeKeyLen )
   {
      zh_ntxTagGoTop( pTag );
      if( pTag->TagEOF )
         fOK = ZH_FALSE;
      else
         dStart = zh_ntxTagCountRelKeyPos( pTag );
   }
   if( fOK && ( pTag->fUsrDescend ? pTag->top.scopeKeyLen : pTag->bottom.scopeKeyLen ) )
   {
      zh_ntxTagGoBottom( pTag );
      if( pTag->TagBOF )
         fOK = ZH_FALSE;
      else
         dStop = zh_ntxTagCountRelKeyPos( pTag );
   }
   pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter = fFilter;

   if( fOK )
   {
      if( zh_ntxCurKeyRefresh( pTag ) &&
          zh_ntxKeyInScope( pTag, pTag->CurKeyInfo ) )
      {
         if( dStart >= dStop - dFact )
            dPos = 0.5;
         else
         {
            dPos = zh_ntxTagCountRelKeyPos( pTag );
            dPos = ( dPos - dStart ) / ( dStop - dStart );
            /* fix possible differences in FL representation */
            if( dPos <= 0.0 )
               dPos = 0.0;
            else if( dPos >= 1.0 )
               dPos = 1.0;
         }
      }
   }
   zh_ntxTagUnLockRead( pTag );

   return dPos;
}

/*
 * set the relative key position (from 0.0 to 1.0) in the given tag
 */
static void zh_ntxOrdSetRelKeyPos( LPTAGINFO pTag, double dPos )
{
   if( zh_ntxTagLockRead( pTag ) )
   {
      NTXAREAP pArea = pTag->pIndex->pArea;
      double dStart = 0.0, dStop = 1.0, dFact = 0.0000000000001;
      ZH_BOOL fOK = ZH_TRUE, fFilter = pArea->dbfarea.area.dbfi.fFilter;
      ZH_BOOL fForward = ZH_TRUE, fTop = ZH_FALSE;

      zh_ntxTagRefreshScope( pTag );

      if( dPos >= 1.0 )
         fForward = ZH_FALSE;
      else if( dPos <= 0.0 )
         fTop = ZH_TRUE;
      else
      {
         pArea->dbfarea.area.dbfi.fFilter = ZH_FALSE;
         if( pTag->fUsrDescend ? pTag->bottom.scopeKeyLen : pTag->top.scopeKeyLen )
         {
            zh_ntxTagGoTop( pTag );
            if( pTag->TagEOF )
               fOK = ZH_FALSE;
            else
               dStart = zh_ntxTagCountRelKeyPos( pTag );
         }
         if( fOK && ( pTag->fUsrDescend ? pTag->top.scopeKeyLen : pTag->bottom.scopeKeyLen ) )
         {
            zh_ntxTagGoBottom( pTag );
            if( pTag->TagBOF )
               fOK = ZH_FALSE;
            else
               dStop = zh_ntxTagCountRelKeyPos( pTag );
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
               zh_ntxTagGoToRelKeyPos( pTag, dPos );
               if( pTag->CurKeyInfo->Xtra == 0 )
                  fForward = ZH_FALSE;
               else if( ! zh_ntxInTopScope( pTag, pTag->CurKeyInfo->key ) )
                  fTop = ZH_TRUE;
               else if( ! zh_ntxInBottomScope( pTag, pTag->CurKeyInfo->key ) )
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
               zh_ntxTagGoTop( pTag );
            if( pTag->CurKeyInfo->Xtra != 0 )
            {
               if( SELF_GOTO( &pArea->dbfarea.area, pTag->CurKeyInfo->Xtra ) == ZH_SUCCESS )
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
            zh_ntxTagGoBottom( pTag );
            if( SELF_GOTO( &pArea->dbfarea.area, pTag->CurKeyInfo->Xtra ) == ZH_SUCCESS &&
                pTag->CurKeyInfo->Xtra != 0 )
            {
               pArea->dbfarea.area.fBottom = ZH_TRUE;
               SELF_SKIPFILTER( &pArea->dbfarea.area, -1 );
            }
         }
         pArea->lpCurTag = pSavedTag;
      }
      zh_ntxTagUnLockRead( pTag );
   }
}

/*
 * skip to next/previous unique key
 */
static ZH_BOOL zh_ntxOrdSkipUnique( LPTAGINFO pTag, ZH_LONG lToSkip )
{
   NTXAREAP pArea = pTag->pIndex->pArea;

   if( pArea->dbfarea.lpdbPendingRel )
      SELF_FORCEREL( &pArea->dbfarea.area );

   pArea->dbfarea.area.fTop = pArea->dbfarea.area.fBottom = ZH_FALSE;

   if( zh_ntxTagLockRead( pTag ) )
   {
      ZH_BOOL fOut = ZH_FALSE, fEof = ZH_FALSE, fForward = ( lToSkip >= 0 );

      LPTAGINFO pSavedTag = pArea->lpCurTag;
      pArea->lpCurTag = pTag;

      zh_ntxTagRefreshScope( pTag );
      if( zh_ntxCurKeyRefresh( pTag ) )
      {
         char keyVal[ NTX_MAX_KEY ];
         memcpy( keyVal, pTag->CurKeyInfo->key, pTag->KeyLength );

         do
         {
            if( fForward )
               zh_ntxTagSkipNext( pTag );
            else
               zh_ntxTagSkipPrev( pTag );
            fOut = pTag->TagEOF || pTag->TagBOF;
         }
         while( ! fOut && zh_ntxValCompare( pTag,
                                        pTag->CurKeyInfo->key, pTag->KeyLength,
                                        keyVal, pTag->KeyLength, ZH_TRUE ) == 0 );
      }
      else if( ! fForward && ! pArea->dbfarea.fPositioned )
      {
         zh_ntxTagGoBottom( pTag );
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
            zh_ntxTagGoTop( pTag );
            fEof = pTag->TagEOF;
         }
      }
      zh_ntxTagUnLockRead( pTag );

      if( SELF_GOTO( &pArea->dbfarea.area, fEof ? 0 : pTag->CurKeyInfo->Xtra ) == ZH_SUCCESS &&
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
static ZH_BOOL zh_ntxOrdSkipEval( LPTAGINFO pTag, ZH_BOOL fForward, PZH_ITEM pEval )
{
   NTXAREAP pArea = pTag->pIndex->pArea;
   ZH_BOOL fFound = ZH_FALSE;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ntxOrdSkipEval(%p, %d, %p)", ( void * ) pTag, fForward, ( void * ) pEval ) );

   if( ( zh_itemType( pEval ) & ZH_IT_BLOCK ) == 0 )
   {
      if( SELF_SKIP( &pArea->dbfarea.area, fForward ? 1 : -1 ) != ZH_SUCCESS )
         return ZH_FALSE;
      return fForward ? ! pArea->dbfarea.area.fEof : ! pArea->dbfarea.area.fBof;
   }

   if( pArea->dbfarea.lpdbPendingRel )
      SELF_FORCEREL( &pArea->dbfarea.area );

   pArea->dbfarea.area.fTop = pArea->dbfarea.area.fBottom = ZH_FALSE;

   if( zh_ntxTagLockRead( pTag ) )
   {
      LPTAGINFO pSavedTag = pArea->lpCurTag;
      pArea->lpCurTag = pTag;

      zh_ntxTagRefreshScope( pTag );
      if( zh_ntxCurKeyRefresh( pTag ) )
      {
         if( fForward )
            zh_ntxTagSkipNext( pTag );
         else
            zh_ntxTagSkipPrev( pTag );

         while( fForward ? ! pTag->TagEOF : ! pTag->TagBOF )
         {
            if( SELF_GOTO( &pArea->dbfarea.area, pTag->CurKeyInfo->Xtra ) != ZH_SUCCESS )
               break;
            if( zh_ntxEvalSeekCond( pTag, pEval ) )
            {
               ZH_ULONG ulRecNo = pArea->dbfarea.ulRecNo;
               if( SELF_SKIPFILTER( &pArea->dbfarea.area, fForward ? 1 : -1 ) != ZH_SUCCESS ||
                   pArea->dbfarea.ulRecNo == ulRecNo || zh_ntxEvalSeekCond( pTag, pEval ) )
               {
                  fFound = ZH_TRUE;
                  break;
               }
            }
            if( fForward )
               zh_ntxTagSkipNext( pTag );
            else
               zh_ntxTagSkipPrev( pTag );
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
      zh_ntxTagUnLockRead( pTag );
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
static ZH_BOOL zh_ntxOrdSkipWild( LPTAGINFO pTag, ZH_BOOL fForward, PZH_ITEM pWildItm )
{
   NTXAREAP pArea = pTag->pIndex->pArea;
   const char * szPattern;
   char * szFree = NULL;
   ZH_BOOL fFound = ZH_FALSE;
   int iFixed = 0;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ntxOrdSkipWild(%p, %d, %p)", ( void * ) pTag, fForward, ( void * ) pWildItm ) );

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

   if( zh_ntxTagLockRead( pTag ) )
   {
      LPTAGINFO pSavedTag = pArea->lpCurTag;
      pArea->lpCurTag = pTag;

      zh_ntxTagRefreshScope( pTag );
      if( zh_ntxCurKeyRefresh( pTag ) )
      {
         int iStop = fForward ? -1 : 1;
         if( pTag->fUsrDescend )
            iStop = -iStop;
         if( iFixed && zh_ntxValCompare( pTag, szPattern, iFixed,
                             pTag->CurKeyInfo->key, iFixed, ZH_FALSE ) == -iStop )
         {
            LPKEYINFO pKey;
            pKey = zh_ntxKeyNew( NULL, pTag->KeyLength );
            memcpy( pKey->key, szPattern, iFixed );
            pKey->key[ iFixed ] = '\0';
            pKey->Xtra = pArea->lpCurTag->fUsrDescend ==
                         pArea->lpCurTag->AscendKey ? NTX_MAX_REC_NUM :
                                                      NTX_IGNORE_REC_NUM;
            if( ! zh_ntxTagKeyFind( pTag, pKey, ( ZH_USHORTCAST ) iFixed ) )
            {
               if( fForward )
                  pTag->TagEOF = ZH_TRUE;
               else
                  pTag->TagBOF = ZH_TRUE;
            }
            zh_ntxKeyFree( pKey );
         }
         else if( fForward )
            zh_ntxTagSkipNext( pTag );
         else
            zh_ntxTagSkipPrev( pTag );

         while( fForward ? ! pTag->TagEOF : ! pTag->TagBOF )
         {
            if( zh_strMatchWild( pTag->CurKeyInfo->key, szPattern ) )
            {
               ZH_ULONG ulRecNo = pTag->CurKeyInfo->Xtra;
               if( SELF_GOTO( &pArea->dbfarea.area, ulRecNo ) != ZH_SUCCESS )
                  break;
               if( SELF_SKIPFILTER( &pArea->dbfarea.area, fForward ? 1 : -1 ) != ZH_SUCCESS ||
                   pArea->dbfarea.ulRecNo == ulRecNo ||
                   zh_strMatchWild( pTag->CurKeyInfo->key, szPattern ) )
               {
                  fFound = ZH_TRUE;
                  break;
               }
            }
            if( iFixed && zh_ntxValCompare( pTag, szPattern, iFixed,
                             pTag->CurKeyInfo->key, iFixed, ZH_FALSE ) == iStop )
            {
               break;
            }
            if( fForward )
               zh_ntxTagSkipNext( pTag );
            else
               zh_ntxTagSkipPrev( pTag );
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
      zh_ntxTagUnLockRead( pTag );
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

static ZH_BOOL zh_ntxRegexMatch( LPTAGINFO pTag, PZH_REGEX pRegEx, const char * szKey )
{
   ZH_SIZE nLen = pTag->KeyLength;
   char szBuff[ NTX_MAX_KEY + 1 ];

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
static ZH_BOOL zh_ntxOrdSkipRegEx( LPTAGINFO pTag, ZH_BOOL fForward, PZH_ITEM pRegExItm )
{
   NTXAREAP pArea = pTag->pIndex->pArea;
   ZH_BOOL fFound = ZH_FALSE;
   PZH_REGEX pRegEx;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ntxOrdSkipRegEx(%p, %d, %p)", ( void * ) pTag, fForward, ( void * ) pRegExItm ) );

   if( pTag->KeyType != 'C' || ( pRegEx = zh_regexGet( pRegExItm, 0 ) ) == NULL )
   {
      if( SELF_SKIP( &pArea->dbfarea.area, fForward ? 1 : -1 ) != ZH_SUCCESS )
         return ZH_FALSE;
      return fForward ? ! pArea->dbfarea.area.fEof : ! pArea->dbfarea.area.fBof;
   }

   if( pArea->dbfarea.lpdbPendingRel )
      SELF_FORCEREL( &pArea->dbfarea.area );

   pArea->dbfarea.area.fTop = pArea->dbfarea.area.fBottom = ZH_FALSE;

   if( zh_ntxTagLockRead( pTag ) )
   {
      LPTAGINFO pSavedTag = pArea->lpCurTag;
      pArea->lpCurTag = pTag;

      zh_ntxTagRefreshScope( pTag );
      if( zh_ntxCurKeyRefresh( pTag ) )
      {
         if( fForward )
            zh_ntxTagSkipNext( pTag );
         else
            zh_ntxTagSkipPrev( pTag );

         while( fForward ? ! pTag->TagEOF : ! pTag->TagBOF )
         {
            if( SELF_GOTO( &pArea->dbfarea.area, pTag->CurKeyInfo->Xtra ) != ZH_SUCCESS )
               break;

            if( zh_ntxRegexMatch( pTag, pRegEx, ( const char * ) pTag->CurKeyInfo->key ) )
            {
               ZH_ULONG ulRecNo = pArea->dbfarea.ulRecNo;
               if( SELF_SKIPFILTER( &pArea->dbfarea.area, fForward ? 1 : -1 ) != ZH_SUCCESS ||
                   pArea->dbfarea.ulRecNo == ulRecNo ||
                   zh_ntxRegexMatch( pTag, pRegEx, ( const char * ) pTag->CurKeyInfo->key ) )
               {
                  fFound = ZH_TRUE;
                  break;
               }
            }
            if( fForward )
               zh_ntxTagSkipNext( pTag );
            else
               zh_ntxTagSkipPrev( pTag );
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
      zh_ntxTagUnLockRead( pTag );
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
static ZH_BOOL zh_ntxOrdKeyAdd( LPTAGINFO pTag, PZH_ITEM pItem )
{
   NTXAREAP pArea = pTag->pIndex->pArea;
   ZH_BOOL fResult = ZH_FALSE;
   LPKEYINFO pKey;

   if( pArea->dbfarea.lpdbPendingRel )
      SELF_FORCEREL( &pArea->dbfarea.area );

   if( ! pArea->dbfarea.fPositioned )
      return ZH_FALSE;

   if( pTag->pForItem && ! zh_ntxEvalCond( pArea, pTag->pForItem, ZH_TRUE ) )
      return ZH_FALSE;

   if( pTag->Template && pItem && zh_itemType( pItem ) != ZH_IT_NIL )
   {
      pKey = zh_ntxKeyPutItem( NULL, pItem, pArea->dbfarea.ulRecNo, pTag, ZH_TRUE, NULL );
   }
   else
   {
      pKey = zh_ntxEvalKey( NULL, pTag );
   }

   if( zh_ntxTagLockWrite( pTag ) )
   {
      if( zh_ntxTagKeyAdd( pTag, pKey ) )
      {
         fResult = ZH_TRUE;
         if( ! pTag->pIndex->fShared && pTag->keyCount &&
             zh_ntxKeyInScope( pTag, pKey ) )
            pTag->keyCount++;
      }
      zh_ntxTagUnLockWrite( pTag );
   }
   zh_ntxKeyFree( pKey );
   return fResult;
}

/*
 * del key from custom tag (ordKeyDel())
 * user key value is not implemented
 */
static ZH_BOOL zh_ntxOrdKeyDel( LPTAGINFO pTag, PZH_ITEM pItem )
{
   NTXAREAP pArea = pTag->pIndex->pArea;
   ZH_BOOL fResult = ZH_FALSE;
   LPKEYINFO pKey = NULL;

   if( pArea->dbfarea.lpdbPendingRel )
      SELF_FORCEREL( &pArea->dbfarea.area );

   if( ! pArea->dbfarea.fPositioned )
      return ZH_FALSE;

   if( pTag->pForItem && ! zh_ntxEvalCond( pArea, pTag->pForItem, ZH_TRUE ) )
      return ZH_FALSE;

   if( pTag->Template && pItem && zh_itemType( pItem ) != ZH_IT_NIL )
   {
      pKey = zh_ntxKeyPutItem( NULL, pItem, pArea->dbfarea.ulRecNo, pTag, ZH_TRUE, NULL );
   }

   if( zh_ntxTagLockWrite( pTag ) )
   {
      if( pKey == NULL )
      {
         if( zh_ntxCurKeyRefresh( pTag ) )
            pKey = zh_ntxKeyCopy( NULL, pTag->CurKeyInfo, pTag->KeyLength );
         else
            pKey = zh_ntxEvalKey( NULL, pTag );
      }
      if( zh_ntxTagKeyDel( pTag, pKey ) )
      {
         fResult = ZH_TRUE;
         if( ! pTag->pIndex->fShared && pTag->keyCount &&
             zh_ntxKeyInScope( pTag, pKey ) )
            pTag->keyCount--;
      }
      zh_ntxTagUnLockWrite( pTag );
   }
   zh_ntxKeyFree( pKey );
   return fResult;
}

/*
 * DBOI_FINDREC find a specific record in the tag - it's useful for
 * custom indexes when the same record can be stored more then once
 * or when the used index key is unknown
 */
static ZH_BOOL zh_ntxOrdFindRec( LPTAGINFO pTag, ZH_ULONG ulRecNo, ZH_BOOL fCont )
{
   NTXAREAP pArea = pTag->pIndex->pArea;
   ZH_BOOL fFound = ZH_FALSE;

   if( ulRecNo )
   {
      if( pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped )
         SELF_FORCEREL( &pArea->dbfarea.area );

      if( zh_ntxTagLockRead( pTag ) )
      {
         zh_ntxTagRefreshScope( pTag );
         if( fCont )
         {
            if( ! zh_ntxCurKeyRefresh( pTag ) )
               ulRecNo = 0;
            else
               zh_ntxTagSkipNext( pTag );
         }
         else
         {
            zh_ntxTagGoTop( pTag );
         }
         if( ulRecNo )
         {
            while( ! pTag->TagEOF )
            {
               if( pTag->CurKeyInfo->Xtra == ulRecNo )
               {
                  fFound = ZH_TRUE;
                  break;
               }
               zh_ntxTagSkipNext( pTag );
            }
         }
         zh_ntxTagUnLockRead( pTag );
      }
   }
   SELF_GOTO( &pArea->dbfarea.area, fFound ? ulRecNo : 0 );
   return fFound;
}

/*
 * evaluate given C function in given scope
 */
static ZH_ULONG zh_ntxOrdScopeEval( LPTAGINFO pTag,
                                    ZH_EVALSCOPE_FUNC pFunc, void * pParam,
                                    PZH_ITEM pItemLo, PZH_ITEM pItemHi )
{
   ZH_ULONG ulCount = 0, ulLen = ( ZH_ULONG ) pTag->KeyLength;
   PZH_ITEM pItemTop = zh_itemNew( NULL ), pItemBottom = zh_itemNew( NULL );

   zh_ntxTagGetScope( pTag, 0, pItemTop );
   zh_ntxTagGetScope( pTag, 1, pItemBottom );
   zh_ntxTagSetScope( pTag, 0, pItemLo );
   zh_ntxTagSetScope( pTag, 1, pItemHi );

   if( zh_ntxTagLockRead( pTag ) )
   {
      zh_ntxTagGoTop( pTag );
      while( ! pTag->TagEOF )
      {
         pFunc( pTag->CurKeyInfo->Xtra, ( ZH_BYTE * ) pTag->CurKeyInfo->key, ulLen, pParam );
         ulCount++;
         zh_ntxTagSkipNext( pTag );
      }
      zh_ntxTagUnLockRead( pTag );
   }

   zh_ntxTagSetScope( pTag, 0, pItemTop );
   zh_ntxTagSetScope( pTag, 1, pItemBottom );
   zh_itemRelease( pItemTop );
   zh_itemRelease( pItemBottom );

   return ulCount;
}

/* ************************************************************************* */
/* create index: zh_ntxTagCreate() */
/* ************************************************************************* */

static int zh_ntxQuickSortCompare( LPNTXSORTINFO pSort, ZH_BYTE * pKey1, ZH_BYTE * pKey2 )
{
   int iLen = pSort->keyLen, i;

   i = zh_ntxValCompare( pSort->pTag, ( const char * ) pKey1, iLen, ( const char * ) pKey2, iLen, ZH_TRUE );
   if( i == 0 )
   {
      if( pSort->pTag->fSortRec )
         i = ( ZH_GET_LE_UINT32( pKey1 + iLen ) < ZH_GET_LE_UINT32( pKey2 + iLen ) ) ? -1 : 1;
   }
   else if( ! pSort->pTag->AscendKey )
   {
      i = -i;
   }

   return i;
}

static ZH_BOOL zh_ntxQSort( LPNTXSORTINFO pSort, ZH_BYTE * pSrc, ZH_BYTE * pBuf, ZH_LONG lKeys )
{
   if( lKeys > 1 )
   {
      int iLen = pSort->keyLen + 4;
      ZH_LONG l1, l2;
      ZH_BYTE * pPtr1, * pPtr2, * pDst;
      ZH_BOOL f1, f2;

      l1 = lKeys >> 1;
      l2 = lKeys - l1;
      pPtr1 = &pSrc[ 0 ];
      pPtr2 = &pSrc[ l1 * iLen ];

      f1 = zh_ntxQSort( pSort, pPtr1, &pBuf[ 0 ], l1 );
      f2 = zh_ntxQSort( pSort, pPtr2, &pBuf[ l1 * iLen ], l2 );
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
         if( zh_ntxQuickSortCompare( pSort, pPtr1, pPtr2 ) <= 0 )
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
      {
         memcpy( pDst, pPtr1, iLen * l1 );
      }
      else if( l2 > 0 && f1 == f2 )
      {
         memcpy( pDst, pPtr2, iLen * l2 );
      }
      return ! f1;
   }
   return ZH_TRUE;
}

static void zh_ntxSortSortPage( LPNTXSORTINFO pSort )
{
   ZH_SIZE nSize = ( ZH_SIZE ) pSort->ulKeys * ( pSort->keyLen + 4 );

   if( ! zh_ntxQSort( pSort, pSort->pKeyPool, &pSort->pKeyPool[ nSize ], pSort->ulKeys ) )
      pSort->pStartKey = &pSort->pKeyPool[ nSize ];
   else
      pSort->pStartKey = pSort->pKeyPool;
}

static void zh_ntxSortBufferFlush( LPNTXSORTINFO pSort )
{
   if( pSort->ulPagesIO )
   {
      LPNTXINDEX pIndex = pSort->pTag->pIndex;
      ZH_SIZE nSize = ( ZH_SIZE ) pSort->ulPagesIO * NTXBLOCKSIZE;
      if( zh_fileWriteAt( pIndex->DiskFile, pSort->pBuffIO, nSize,
                     zh_ntxFileOffset( pIndex, pSort->ulFirstIO ) ) != nSize )
      {
         zh_ntxErrorRT( pIndex->pArea, EG_WRITE, EDBF_WRITE,
                        pIndex->IndexName, zh_fsError(), 0, NULL );
      }
      pSort->ulPagesIO = 0;
      pIndex->fFlush = ZH_TRUE;
      if( pIndex->fShared )
         pIndex->Changed = ZH_TRUE;
   }
}

static void zh_ntxSortStorePage( LPNTXSORTINFO pSort, LPPAGEINFO pPage )
{
   LPNTXINDEX pIndex = pSort->pTag->pIndex;

   if( ! pPage->Page )
   {
      pPage->Page = zh_ntxPageAlloc( pIndex );
      if( pSort->ulSizeIO )
      {
         if( pSort->ulPagesIO == pSort->ulSizeIO )
            zh_ntxSortBufferFlush( pSort );
         if( ! pSort->ulPagesIO ||
             zh_ntxFileOffset( pIndex, pSort->ulLastIO ) + NTXBLOCKSIZE ==
             zh_ntxFileOffset( pIndex, pPage->Page ) )
         {
            zh_ntxSetKeyCount( pPage, pPage->uiKeys );
            memcpy( pSort->pBuffIO + pSort->ulPagesIO * NTXBLOCKSIZE,
                    zh_ntxPageBuffer( pPage ), NTXBLOCKSIZE );
            pSort->ulLastIO = pPage->Page;
            if( ! pSort->ulPagesIO++ )
               pSort->ulFirstIO = pPage->Page;
            pPage->Changed = ZH_FALSE;
            return;
         }
      }
   }
   if( ! pPage->pPrev )
      zh_ntxPageSave( pIndex, pPage );
}

static void zh_ntxSortAddNodeKey( LPNTXSORTINFO pSort, ZH_BYTE * pKeyVal, ZH_ULONG ulRec )
{
   LPPAGEINFO pPage;
   ZH_ULONG ulPage = 0;
   int iLevel = 0;

   for( ;; )
   {
      pPage = pSort->NodeList[ iLevel ];
      if( pPage == NULL )
      {
         pPage = pSort->NodeList[ iLevel ] = zh_ntxPageNew( pSort->pTag, ZH_TRUE );
         break;
      }
      else if( pPage->uiKeys >= pSort->pTag->MaxKeys )
      {
         zh_ntxSetKeyPage( pPage, pPage->uiKeys, ulPage );
         zh_ntxSortStorePage( pSort, pPage );
         ulPage = pPage->Page;
         zh_ntxPageRelease( pSort->pTag, pPage );
         pSort->NodeList[ iLevel++ ] = zh_ntxPageNew( pSort->pTag, ZH_TRUE );
      }
      else
         break;
   }

   memcpy( zh_ntxGetKeyVal( pPage, pPage->uiKeys ), pKeyVal, pSort->pTag->KeyLength );
   zh_ntxSetKeyRec( pPage, pPage->uiKeys, ulRec );
   zh_ntxSetKeyPage( pPage, pPage->uiKeys, ulPage );
   pPage->uiKeys++;
}

static void zh_ntxSortWritePage( LPNTXSORTINFO pSort )
{
   ZH_SIZE nSize = ( ZH_SIZE ) pSort->ulKeys * ( pSort->keyLen + 4 );

   zh_ntxSortSortPage( pSort );

   if( pSort->pTempFile == NULL )
   {
      char szName[ ZH_PATH_MAX ];
      pSort->pTempFile = zh_fileCreateTemp( NULL, NULL, FC_NORMAL, szName );
      if( pSort->pTempFile == NULL )
         zh_ntxErrorRT( pSort->pTag->pIndex->pArea, EG_CREATE, EDBF_CREATE_TEMP,
                        szName, zh_fsError(), 0, NULL );
      else
         pSort->szTempFileName = zh_strdup( szName );
   }

   pSort->pSwapPage[ pSort->ulCurPage ].ulKeys = pSort->ulKeys;
   if( pSort->pTempFile != NULL )
   {
      pSort->pSwapPage[ pSort->ulCurPage ].nOffset = zh_fileSize( pSort->pTempFile );
      if( zh_fileWriteAt( pSort->pTempFile, pSort->pStartKey, nSize,
                          pSort->pSwapPage[ pSort->ulCurPage ].nOffset ) != nSize )
         zh_ntxErrorRT( pSort->pTag->pIndex->pArea, EG_WRITE, EDBF_WRITE_TEMP,
                        pSort->szTempFileName, zh_fsError(), 0, NULL );
   }
   else
      pSort->pSwapPage[ pSort->ulCurPage ].nOffset = 0;
   pSort->ulKeys = 0;
   pSort->ulCurPage++;
}

static void zh_ntxSortGetPageKey( LPNTXSORTINFO pSort, ZH_ULONG ulPage,
                                  ZH_BYTE ** pKeyVal, ZH_ULONG * pulRec )
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
         zh_ntxErrorRT( pSort->pTag->pIndex->pArea, EG_READ, EDBF_READ_TEMP,
                        pSort->szTempFileName, zh_fsError(), 0, NULL );
      }
      pSort->pSwapPage[ ulPage ].nOffset += nSize;
      pSort->pSwapPage[ ulPage ].ulKeyBuf = ulKeys;
      pSort->pSwapPage[ ulPage ].ulCurKey = 0;
   }
   *pKeyVal = &pSort->pSwapPage[ ulPage ].pKeyPool[ pSort->pSwapPage[ ulPage ].ulCurKey * ( iLen + 4 ) ];
   *pulRec = ZH_GET_LE_UINT32( *pKeyVal + iLen );
}

static void zh_ntxSortOrderPages( LPNTXSORTINFO pSort )
{
   pSort->ulFirst = 0;
   pSort->pSortedPages = ( ZH_ULONG * ) zh_xgrab( pSort->ulPages * sizeof( ZH_ULONG ) );
   pSort->pSortedPages[ 0 ] = 0;

   if( pSort->ulTotKeys > 0 )
   {
      int iLen = pSort->keyLen;
      ZH_BYTE * pKey = NULL;
      ZH_ULONG n;

      for( n = 0; n < pSort->ulPages; n++ )
      {
         ZH_LONG l, r;
         ZH_ULONG ulRec;

         zh_ntxSortGetPageKey( pSort, n, &pKey, &ulRec );
         l = 0;
         r = n - 1;
         while( l <= r )
         {
            int i;
            ZH_ULONG ulPage;
            ZH_LONG m;
            ZH_BYTE * pTmp;

            m = ( l + r ) >> 1;
            ulPage = pSort->pSortedPages[ m ];
            pTmp = &pSort->pSwapPage[ ulPage ].pKeyPool[ pSort->pSwapPage[ ulPage ].ulCurKey * ( iLen + 4 ) ];
            i = zh_ntxValCompare( pSort->pTag, ( const char * ) pKey, iLen, ( const char * ) pTmp, iLen, ZH_TRUE );
            if( i == 0 )
            {
               if( pSort->pTag->fSortRec )
                  i = ( ulRec < ZH_GET_LE_UINT32( &pTmp[ iLen ] ) ) ? -1 : 1;
            }
            else if( ! pSort->pTag->AscendKey )
               i = -i;
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

static ZH_BOOL zh_ntxSortKeyGet( LPNTXSORTINFO pSort, ZH_BYTE ** pKeyVal, ZH_ULONG * pulRec )
{
   ZH_ULONG ulPage = pSort->pSortedPages[ pSort->ulFirst ];

   /* check if first page has some keys yet */
   if( pSort->pSwapPage[ ulPage ].ulKeys > 0 )
   {
      int iLen = pSort->keyLen;
      ZH_BYTE * pKey;
      ZH_ULONG ulRec;
      ZH_LONG l, r;

      /*
       * last key was taken from this page - we have to resort it.
       * This is done intentionally here to be sure that the key
       * value return by this function will not be overwritten by
       * next keys in page read from temporary file in function
       * zh_ntxSortGetPageKey() - please do not move this part down
       * even it seems to be correct
       */
      zh_ntxSortGetPageKey( pSort, ulPage, &pKey, &ulRec );

      l = pSort->ulFirst + 1;
      r = pSort->ulPages - 1;
      while( l <= r )
      {
         int i;
         ZH_ULONG ulPg;
         ZH_LONG m;
         ZH_BYTE * pTmp;

         m = ( l + r ) >> 1;
         ulPg = pSort->pSortedPages[ m ];
         pTmp = &pSort->pSwapPage[ ulPg ].pKeyPool[ pSort->pSwapPage[ ulPg ].ulCurKey * ( iLen + 4 ) ];
         i = zh_ntxValCompare( pSort->pTag, ( const char * ) pKey, iLen, ( const char * ) pTmp, iLen, ZH_TRUE );
         if( i == 0 )
         {
            if( pSort->pTag->fSortRec )
               i = ( ulRec < ZH_GET_LE_UINT32( &pTmp[ iLen ] ) ) ? -1 : 1;
            else
               i = ( ulPage < ulPg ) ? -1 : 1;
         }
         else if( ! pSort->pTag->AscendKey )
            i = -i;
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
      zh_ntxSortGetPageKey( pSort, ulPage, pKeyVal, pulRec );
      pSort->pSwapPage[ ulPage ].ulCurKey++;
      pSort->pSwapPage[ ulPage ].ulKeys--;
      pSort->pSwapPage[ ulPage ].ulKeyBuf--;
      return ZH_TRUE;
   }

   *pKeyVal = NULL;
   *pulRec = 0;

   return ZH_FALSE;
}

static void zh_ntxSortKeyAdd( LPNTXSORTINFO pSort, ZH_ULONG ulRec, const char * pKeyVal, int iKeyLen )
{
   int iLen = pSort->keyLen;
   ZH_BYTE * pDst;

   if( pSort->ulKeys >= pSort->ulPgKeys )
   {
      zh_ntxSortWritePage( pSort );
   }
   pDst = &pSort->pKeyPool[ pSort->ulKeys * ( iLen + 4 ) ];

   if( iLen > iKeyLen )
   {
      memcpy( pDst, pKeyVal, iKeyLen );
      memset( &pDst[ iKeyLen ], ' ', iLen - iKeyLen );
   }
   else
   {
      memcpy( pDst, pKeyVal, iLen );
   }
   ZH_PUT_LE_UINT32( &pDst[ iLen ], ulRec );
   pSort->ulKeys++;
   pSort->ulTotKeys++;
}

static LPNTXSORTINFO zh_ntxSortNew( LPTAGINFO pTag, ZH_ULONG ulRecCount )
{
   LPNTXSORTINFO pSort;
   ZH_BYTE * pBuf;
   int iLen = pTag->KeyLength;
   ZH_ULONG ulSize, ulMax, ulMin;

   if( ulRecCount == 0 )
      ulRecCount = 1;

   pSort = ( LPNTXSORTINFO ) zh_xgrabz( sizeof( NTXSORTINFO ) );

   ulMin = ( ZH_ULONG ) ceil( sqrt( ( double ) ulRecCount ) );
   ulMax = ( ( ZH_ULONG ) ceil( sqrt( ( double ) ulRecCount / ( iLen + 4 ) ) ) ) << 7;
   /*
    * this effectively increase allocated memory buffer for very large files
    * moving the maximum to: 270'566'400 for 4'294'967'295 records and 256
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
      pBuf = ( ZH_BYTE * ) zh_xalloc( ulSize << 2 );
      if( pBuf )
      {
         zh_xfree( pBuf );
         pBuf = ( ZH_BYTE * ) zh_xalloc( ulSize << 1 );
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
       *    ~ (keySize+4+sizeof(NTXSWAPPAGE)) * sqrt(ulRecCount) * 2
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
      pBuf = ( ZH_BYTE * ) zh_xgrab( ( ulMax << 1 ) * ( iLen + 4 ) );
   }

   pSort->pTag = pTag;
   pSort->pTempFile = NULL;
   pSort->keyLen = iLen;
   pSort->fUnique = pTag->UniqueKey;
   pSort->ulMaxKey = ulMax << 1;
   pSort->ulPgKeys = ulMax;
   pSort->ulMaxRec = ulRecCount;
   pSort->pKeyPool = pBuf;
   pSort->ulPages = ( ulRecCount + pSort->ulPgKeys - 1 ) / pSort->ulPgKeys;
   /* check for overflow on 32-bit machines when number of records is nearly 2^32 */
   if( ! pSort->ulPages )
      pSort->ulPages = ulRecCount / pSort->ulPgKeys + 1;
   pSort->pSwapPage = ( LPNTXSWAPPAGE ) zh_xgrabz( sizeof( NTXSWAPPAGE ) * pSort->ulPages );
   return pSort;
}

static void zh_ntxSortFree( LPNTXSORTINFO pSort, ZH_BOOL fFull )
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

static void zh_ntxSortOut( LPNTXSORTINFO pSort )
{
   ZH_BOOL fUnique = pSort->fUnique, fBalance, fNext;
   LPTAGINFO pTag = pSort->pTag;
   ZH_ULONG ulPage, ulRec, ulKey;
   ZH_USHORT uiHalf;
   ZH_BYTE * pKeyVal;
   int iLen = pSort->keyLen, iLevel;

   pSort->ulPages = pSort->ulCurPage + 1;
   pSort->ulPgKeys = pSort->ulMaxKey / pSort->ulPages;
   if( pSort->ulPages > 1 )
   {
      ZH_BYTE * pBuf = pSort->pKeyPool;
      zh_ntxSortWritePage( pSort );
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
      zh_ntxSortSortPage( pSort );
      pSort->pSwapPage[ 0 ].ulKeys = pSort->ulKeys;
      pSort->pSwapPage[ 0 ].ulKeyBuf = pSort->ulKeys;
      pSort->pSwapPage[ 0 ].ulCurKey = 0;
      pSort->pSwapPage[ 0 ].pKeyPool = pSort->pStartKey;
   }
   #if 0
   printf( "pSort->ulPages=%ld, pSort->ulPgKeys=%ld", pSort->ulPages, pSort->ulPgKeys ); fflush( stdout );
   #endif

   zh_ntxSortOrderPages( pSort );

   if( zh_vmRequestQuery() != 0 )
      return;

   for( ulKey = 0; ulKey < pSort->ulTotKeys; ulKey++ )
   {
      if( ! zh_ntxSortKeyGet( pSort, &pKeyVal, &ulRec ) )
      {
         if( zh_vmRequestQuery() != 0 )
            return;
         zh_errInternal( 9309, "zh_ntxSortOut: memory structure corrupted.", NULL, NULL );
      }
      if( fUnique )
      {
         if( ulKey != 0 && zh_ntxValCompare( pTag, ( const char * ) pSort->pLastKey, iLen, ( const char * ) pKeyVal, iLen, ZH_TRUE ) == 0 )
         {
            continue;
         }
#ifndef ZH_NTX_DEBUG_EXT
         else
         {
            memcpy( pSort->pLastKey, pKeyVal, iLen );
         }
#endif
      }
#ifdef ZH_NTX_DEBUG_EXT
      if( ulKey != 0 )
      {
         int i = zh_ntxValCompare( pTag, ( const char * ) pSort->pLastKey, iLen, ( const char * ) pKeyVal, iLen, ZH_TRUE );
         if( ! pTag->AscendKey )
            i = -i;
         if( i == 0 )
            i = ( pSort->ulLastRec < ulRec ) ? -1 : 1;
         if( i > 0 )
         {
            printf( "\r\nulKey=%ld, pKeyVal=[%s][%ld], pKeyLast=[%s][%ld]\r\n",
                    ulKey, pKeyVal, ulRec, pSort->pLastKey, pSort->ulLastRec ); fflush( stdout );
            if( zh_vmRequestQuery() != 0 )
               return;
            zh_errInternal( 9310, "zh_ntxSortOut: sorting fails.", NULL, NULL );
         }
      }
      memcpy( pSort->pLastKey, pKeyVal, iLen );
      pSort->ulLastRec = ulRec;
#endif
      zh_ntxSortAddNodeKey( pSort, pKeyVal, ulRec );
   }

#ifdef ZH_NTX_DEBUG
   if( zh_ntxSortKeyGet( pSort, &pKeyVal, &ulRec ) )
   {
      if( zh_vmRequestQuery() != 0 )
         return;
      zh_errInternal( 9311, "zh_ntxSortOut: memory structure corrupted(2).", NULL, NULL );
   }
#endif

   if( pSort->NodeList[ 0 ] == NULL )
   {
      pSort->NodeList[ 0 ] = zh_ntxPageNew( pTag, ZH_TRUE );
   }
   zh_ntxSetKeyPage( pSort->NodeList[ 0 ], pSort->NodeList[ 0 ]->uiKeys, 0 );

   iLevel = 0;
   fNext = ZH_TRUE;
   fBalance = ZH_FALSE;
   uiHalf = pTag->MaxKeys >> 1;
   do
   {
      zh_ntxSortStorePage( pSort, pSort->NodeList[ iLevel ] );
      if( iLevel + 1 == NTX_STACKSIZE || pSort->NodeList[ iLevel + 1 ] == NULL )
      {
         pTag->RootBlock = pSort->NodeList[ iLevel ]->Page;
         fNext = ZH_FALSE;
      }
      else
      {
         zh_ntxSetKeyPage( pSort->NodeList[ iLevel + 1 ],
                           pSort->NodeList[ iLevel + 1 ]->uiKeys,
                           pSort->NodeList[ iLevel ]->Page );
         if( pSort->NodeList[ iLevel ]->uiKeys < uiHalf )
         {
            fBalance = ZH_TRUE;
         }
      }
      zh_ntxPageRelease( pTag, pSort->NodeList[ iLevel ] );
      iLevel++;
   }
   while( fNext );

   zh_ntxSortBufferFlush( pSort );
   zh_ntxSortFree( pSort, ZH_FALSE );

   if( fBalance )
   {
      LPPAGEINFO pFirst, pLast;

      ulPage = pTag->RootBlock;
      while( ulPage )
      {
         LPPAGEINFO pPage = zh_ntxPageLoad( pTag, ulPage );
         if( ! pPage )
            return;
         ulPage = zh_ntxGetKeyPage( pPage, pPage->uiKeys );
         if( ulPage && pPage->uiKeys )
         {
            pLast = zh_ntxPageLoad( pTag, ulPage );
            if( ! pLast )
            {
               zh_ntxPageRelease( pTag, pPage );
               return;
            }
            if( pLast->uiKeys < uiHalf )
            {
               pFirst = zh_ntxPageLoad( pTag, zh_ntxGetKeyPage( pPage,
                                                         pPage->uiKeys - 1 ) );
               if( ! pFirst )
               {
                  zh_ntxPageRelease( pTag, pPage );
                  zh_ntxPageRelease( pTag, pLast );
                  return;
               }
               zh_ntxBalancePages( pTag, pPage, pPage->uiKeys - 1, pFirst, pLast );
               zh_ntxPageRelease( pTag, pFirst );
            }
            zh_ntxPageRelease( pTag, pLast );
         }
         zh_ntxPageRelease( pTag, pPage );
      }
   }
}

/*
 * create tag in index file
 */
static ZH_ERRCODE zh_ntxTagCreate( LPTAGINFO pTag, ZH_BOOL fReindex )
{
   LPNTXAREA pArea = pTag->pIndex->pArea;
   PZH_ITEM pWhileItem = NULL, pEvalItem = NULL;
   ZH_ULONG ulRecCount, ulRecNo = pArea->dbfarea.ulRecNo;
   LPNTXSORTINFO pSort;
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
   pArea->pSort = pSort = zh_ntxSortNew( pTag, ulRecCount );
   pSort->fReindex = fReindex;

   if( ulRecCount == 0 )
   {
      LPPAGEINFO pPage = zh_ntxPageNew( pTag, ZH_FALSE );

      if( pPage )
      {
         pTag->RootBlock = pPage->Page;
         zh_ntxPageRelease( pTag, pPage );
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
      char szBuffer[ NTX_MAX_KEY ];
      int iRecBuff = 0, iRecBufSize, iRec;
      PZH_CODEPAGE cdpTmp = zh_cdpSelect( pArea->dbfarea.area.cdPage );
      PZH_ITEM pForItem, pItem = NULL;

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
            if( zh_ntxTagLockRead( pArea->lpCurTag ) )
            {
               zh_ntxTagRefreshScope( pArea->lpCurTag );
               zh_ntxTagGoTop( pArea->lpCurTag );
               if( ! pArea->lpCurTag->TagEOF )
                  ulStartRec = pArea->lpCurTag->CurKeyInfo->Xtra;
               zh_ntxTagUnLockRead( pArea->lpCurTag );
            }
         }
      }

      pSort->ulSizeIO = ( 1 << 16 ) / NTXBLOCKSIZE;
      pSort->pBuffIO = ( ZH_BYTE * ) zh_xgrab( pSort->ulSizeIO * NTXBLOCKSIZE );
      iRecBufSize = ( pSort->ulSizeIO * NTXBLOCKSIZE ) / pArea->dbfarea.uiRecordLen;
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
                  zh_ntxErrorRT( pTag->pIndex->pArea, EG_READ, EDBF_READ,
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

         if( pWhileItem && ! zh_ntxEvalCond( NULL, pWhileItem, ZH_FALSE ) )
            break;

         if( ulRecNo <= ulRecCount &&
             ( pForItem == NULL || zh_ntxEvalCond( pArea, pForItem, ZH_FALSE ) ) )
         {
            if( pTag->nField )
               errCode = SELF_GETVALUE( &pArea->dbfarea.area, pTag->nField, pItem );
            else
               pItem = zh_vmEvalBlockOrMacro( pTag->pKeyItem );

            switch( zh_itemType( pItem ) )
            {
               case ZH_IT_STRING:
               case ZH_IT_MEMO:
                  zh_ntxSortKeyAdd( pSort, pArea->dbfarea.ulRecNo,
                                    zh_itemGetCPtr( pItem ),
                                    ( ZH_INTCAST ) zh_itemGetCLen( pItem ) );
                  break;

               case ZH_IT_INTEGER:
               case ZH_IT_LONG:
               case ZH_IT_DOUBLE:
                  zh_ntxNumToStr( pItem, szBuffer, pTag->KeyLength, pTag->KeyDec );
                  zh_ntxSortKeyAdd( pSort, pArea->dbfarea.ulRecNo, szBuffer, pTag->KeyLength );
                  break;

               case ZH_IT_TIMESTAMP:
                  if( pTag->KeyType == 'T' )
                  {
                     zh_itemGetTS( pItem, szBuffer );
                     zh_ntxSortKeyAdd( pSort, pArea->dbfarea.ulRecNo, szBuffer, 17 );
                     break;
                  }
                  /* fallthrough */
               case ZH_IT_DATE:
                  zh_itemGetDS( pItem, szBuffer );
                  zh_ntxSortKeyAdd( pSort, pArea->dbfarea.ulRecNo, szBuffer, 8 );
                  break;

               case ZH_IT_LOGICAL:
                  szBuffer[ 0 ] = zh_itemGetL( pItem ) ? 'T' : 'F';
                  zh_ntxSortKeyAdd( pSort, pArea->dbfarea.ulRecNo, szBuffer, 1 );
                  break;

               default:
                  zh_ntxErrorRT( pArea, EG_DATATYPE, EDBF_INVALIDKEY,
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
               if( ! zh_ntxEvalCond( pArea, pEvalItem, ZH_FALSE ) )
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
         zh_ntxSortOut( pSort );

      if( pTag->nField )
         zh_itemRelease( pItem );

      pArea->lpCurTag = pSaveTag;
      zh_cdpSelect( cdpTmp );
   }

   zh_ntxSortFree( pSort, ZH_TRUE );
   pArea->pSort = NULL;

   return errCode;
}

/*
 * recreate tags in index file
 */
static ZH_ERRCODE zh_ntxReIndex( LPNTXINDEX pIndex )
{
   ZH_ERRCODE errCode = ZH_FAILURE;
   int i;

   if( zh_ntxIndexLockWrite( pIndex, ZH_FALSE ) )
   {
      errCode = ZH_SUCCESS;
      zh_ntxIndexTrunc( pIndex );

      for( i = 0; i < pIndex->iTags; i++ )
      {
         LPTAGINFO pTag = pIndex->lpTags[ i ];
         pTag->HeadBlock = pTag->RootBlock = pTag->keyCount = 0;
         pTag->HdrChanged = ZH_TRUE;
         errCode = zh_ntxTagCreate( pTag, ZH_TRUE );
         if( errCode != ZH_SUCCESS )
            break;
      }
      zh_ntxIndexUnLockWrite( pIndex );
   }
   return errCode;
}


/* ************************************************************************* */

/* Implementation of exported functions */

#define zh_ntxBof    NULL
#define zh_ntxEof    NULL
#define zh_ntxFound  NULL

static ZH_ERRCODE zh_ntxGoBottom( NTXAREAP pArea )
{
   ZH_ERRCODE retval;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ntxGoBottom(%p)", ( void * ) pArea ) );

   if( SELF_GOCOLD( &pArea->dbfarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   if( ! pArea->lpCurTag )
      return SUPER_GOBOTTOM( &pArea->dbfarea.area );

   if( pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped )
      SELF_FORCEREL( &pArea->dbfarea.area );

   if( ! zh_ntxTagLockRead( pArea->lpCurTag ) )
      return ZH_FAILURE;
   zh_ntxTagRefreshScope( pArea->lpCurTag );

   zh_ntxTagGoBottom( pArea->lpCurTag );

   pArea->dbfarea.area.fTop = ZH_FALSE;
   pArea->dbfarea.area.fBottom = ZH_TRUE;

   if( pArea->lpCurTag->TagEOF )
      retval = SELF_GOTO( &pArea->dbfarea.area, 0 );
   else
   {
      retval = SELF_GOTO( &pArea->dbfarea.area, pArea->lpCurTag->CurKeyInfo->Xtra );
      if( retval != ZH_FAILURE && pArea->dbfarea.fPositioned )
         retval = SELF_SKIPFILTER( &pArea->dbfarea.area, -1 );
   }
   zh_ntxTagUnLockRead( pArea->lpCurTag );

   return retval;
}

#define zh_ntxGoTo    NULL
#define zh_ntxGoToId  NULL

static ZH_ERRCODE zh_ntxGoTop( NTXAREAP pArea )
{
   ZH_ERRCODE retval;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ntxGoTop(%p)", ( void * ) pArea ) );

   if( SELF_GOCOLD( &pArea->dbfarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   if( ! pArea->lpCurTag )
      return SUPER_GOTOP( &pArea->dbfarea.area );

   if( pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped )
      SELF_FORCEREL( &pArea->dbfarea.area );

   if( ! zh_ntxTagLockRead( pArea->lpCurTag ) )
      return ZH_FAILURE;
   zh_ntxTagRefreshScope( pArea->lpCurTag );

   zh_ntxTagGoTop( pArea->lpCurTag );

   pArea->dbfarea.area.fTop = ZH_TRUE;
   pArea->dbfarea.area.fBottom = ZH_FALSE;

   if( pArea->lpCurTag->TagEOF )
      retval = SELF_GOTO( &pArea->dbfarea.area, 0 );
   else
   {
      retval = SELF_GOTO( &pArea->dbfarea.area, pArea->lpCurTag->CurKeyInfo->Xtra );
      if( retval != ZH_FAILURE && pArea->dbfarea.fPositioned )
         retval = SELF_SKIPFILTER( &pArea->dbfarea.area, 1 );
   }
   zh_ntxTagUnLockRead( pArea->lpCurTag );

   return retval;
}

static ZH_ERRCODE zh_ntxSeek( NTXAREAP pArea, ZH_BOOL fSoftSeek, PZH_ITEM pItem, ZH_BOOL fFindLast )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ntxSeek(%p, %d, %p, %d)", ( void * ) pArea, fSoftSeek, ( void * ) pItem, fFindLast ) );

   if( SELF_GOCOLD( &pArea->dbfarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   if( ! pArea->lpCurTag )
   {
      zh_ntxErrorRT( pArea, EG_NOORDER, EDBF_NOTINDEXED, NULL, 0, EF_CANDEFAULT, NULL );
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

      fLast = pArea->lpCurTag->fUsrDescend == pArea->lpCurTag->AscendKey ?
              ! fFindLast : fFindLast;

      pKey = zh_ntxKeyPutItem( NULL, pItem, fLast ? NTX_MAX_REC_NUM :
                         NTX_IGNORE_REC_NUM, pArea->lpCurTag, ZH_TRUE, &uiLen );

      if( ! zh_ntxTagLockRead( pArea->lpCurTag ) )
      {
         zh_ntxKeyFree( pKey );
         return ZH_FAILURE;
      }
      zh_ntxTagRefreshScope( pArea->lpCurTag );

      if( zh_ntxTagKeyFind( pArea->lpCurTag, pKey, uiLen ) )
         ulRec = pArea->lpCurTag->CurKeyInfo->Xtra;
      else
         ulRec = 0;

      if( ( ulRec == 0 && ! fSoftSeek ) || pArea->lpCurTag->TagEOF )
         fEOF = ZH_TRUE;
      else
      {
         if( ! zh_ntxInBottomScope( pArea->lpCurTag, pArea->lpCurTag->CurKeyInfo->key ) )
            fEOF = ZH_TRUE;
         else if( ! zh_ntxInTopScope( pArea->lpCurTag, pArea->lpCurTag->CurKeyInfo->key ) )
         {
            zh_ntxTagGoTop( pArea->lpCurTag );
            if( pArea->lpCurTag->CurKeyInfo->Xtra == 0 ||
                pArea->lpCurTag->TagEOF )
               fEOF = ZH_TRUE;
         }
      }
      zh_ntxTagUnLockRead( pArea->lpCurTag );
      if( ! fEOF )
      {
         retval = SELF_GOTO( &pArea->dbfarea.area, pArea->lpCurTag->CurKeyInfo->Xtra );
         if( retval != ZH_FAILURE && pArea->dbfarea.fPositioned )
         {
            retval = SELF_SKIPFILTER( &pArea->dbfarea.area, fFindLast ? -1 : 1 );
            if( retval != ZH_FAILURE && ulRec && pArea->dbfarea.fPositioned )
            {
               pArea->dbfarea.area.fFound = ( ulRec == pArea->dbfarea.ulRecNo ||
                     zh_ntxValCompare( pArea->lpCurTag, pKey->key, uiLen,
                                       pArea->lpCurTag->CurKeyInfo->key,
                                       pArea->lpCurTag->KeyLength, ZH_FALSE ) == 0 );
               if( ! pArea->dbfarea.area.fFound && ! fSoftSeek )
                  fEOF = ZH_TRUE;
            }
         }
      }
      if( retval != ZH_FAILURE && ( fEOF ||
          ! zh_ntxKeyInScope( pArea->lpCurTag, pArea->lpCurTag->CurKeyInfo ) ) )
      {
         retval = SELF_GOTO( &pArea->dbfarea.area, 0 );
      }
      if( pArea->dbfarea.fPositioned || pArea->dbfarea.ulRecNo != 1 )
         pArea->dbfarea.area.fBof = ZH_FALSE;
      zh_ntxKeyFree( pKey );
      return retval;
   }
}

#define zh_ntxSkip        NULL
#define zh_ntxSkipFilter  NULL

static ZH_ERRCODE zh_ntxSkipRaw( NTXAREAP pArea, ZH_LONG lToSkip )
{
   ZH_ERRCODE retval;
   ZH_BOOL fOut = ZH_FALSE, fForward;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ntxSkipRaw(%p, %ld)", ( void * ) pArea, lToSkip ) );

   if( SELF_GOCOLD( &pArea->dbfarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   if( ! pArea->lpCurTag || lToSkip == 0 )
      return SUPER_SKIPRAW( &pArea->dbfarea.area, lToSkip );

   if( ! zh_ntxTagLockRead( pArea->lpCurTag ) )
      return ZH_FAILURE;
   zh_ntxTagRefreshScope( pArea->lpCurTag );

   fForward = ( lToSkip > 0 );

   if( ! zh_ntxCurKeyRefresh( pArea->lpCurTag ) )
   {
      if( fForward || pArea->dbfarea.fPositioned )
         fOut = ZH_TRUE;
      else
      {
         zh_ntxTagGoBottom( pArea->lpCurTag );
         fOut = pArea->lpCurTag->TagEOF;
         lToSkip++;
      }
   }

   if( fForward )
   {
      while( ! fOut && ! pArea->lpCurTag->TagEOF && lToSkip-- > 0 )
      {
         zh_ntxTagSkipNext( pArea->lpCurTag );
      }
      retval = SELF_GOTO( &pArea->dbfarea.area,
                                    ( pArea->lpCurTag->TagEOF || fOut ) ? 0 :
                                    pArea->lpCurTag->CurKeyInfo->Xtra );
   }
   else /* if( lToSkip < 0 ) */
   {
      while( ! fOut && ! pArea->lpCurTag->TagBOF && lToSkip++ < 0 )
      {
         zh_ntxTagSkipPrev( pArea->lpCurTag );
      }
      if( fOut || pArea->lpCurTag->TagBOF )
      {
         zh_ntxTagGoTop( pArea->lpCurTag );
         fOut = ZH_TRUE;
      }
      retval = SELF_GOTO( &pArea->dbfarea.area, pArea->lpCurTag->TagEOF ? 0 :
                                          pArea->lpCurTag->CurKeyInfo->Xtra );
      pArea->dbfarea.area.fBof = fOut;
   }

   zh_ntxTagUnLockRead( pArea->lpCurTag );
   /* Update Bof and Eof flags */
#if 0
   if( fForward )
      pArea->dbfarea.area.fBof = ZH_FALSE;
   else
      pArea->dbfarea.area.fEof = ZH_FALSE;
#endif
   return retval;
}

#define zh_ntxAddField      NULL
#define zh_ntxAppend        NULL
#define zh_ntxCreateFields  NULL
#define zh_ntxDeleteRec     NULL
#define zh_ntxDeleted       NULL
#define zh_ntxFieldCount    NULL
#define zh_ntxFieldDisplay  NULL
#define zh_ntxFieldInfo     NULL
#define zh_ntxFieldName     NULL

/*
 * Flush _system_ buffers to disk
 */
static ZH_ERRCODE zh_ntxFlush( NTXAREAP pArea )
{
   ZH_ERRCODE errCode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ntxFlush(%p)", ( void * ) pArea ) );

   errCode = SELF_GOCOLD( &pArea->dbfarea.area );
   if( errCode == ZH_SUCCESS )
   {
      errCode = SUPER_FLUSH( &pArea->dbfarea.area );

      if( zh_setGetHardCommit() )
      {
         LPNTXINDEX pIndex = pArea->lpIndexes;
         while( pIndex )
         {
            if( pIndex->fFlush /* && ! pIndex->Temporary */ )
            {
               zh_fileCommit( pIndex->DiskFile );
               pIndex->fFlush = ZH_FALSE;
            }
            pIndex = pIndex->pNext;
         }
      }
   }

   return errCode;
}

#define zh_ntxGetRec     NULL
#define zh_ntxGetValue   NULL
#define zh_ntxGetVarLen  NULL

/*
 * Perform a write of WorkArea memory to the data store.
 */
static ZH_ERRCODE zh_ntxGoCold( NTXAREAP pArea )
{
   ZH_BOOL fRecordChanged = pArea->dbfarea.fRecordChanged;
   ZH_BOOL fAppend = pArea->dbfarea.fAppend;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ntxGoCold(%p)", ( void * ) pArea ) );

   if( SUPER_GOCOLD( &pArea->dbfarea.area ) == ZH_SUCCESS )
   {
      if( fRecordChanged || pArea->fNtxAppend )
      {
         if( fAppend && pArea->dbfarea.fShared )
         {
            if( pArea->fNtxAppend )
               zh_errInternal( 9312, "zh_ntxGoCold: multiple appending without GOCOLD.", NULL, NULL );
            pArea->fNtxAppend = ZH_TRUE;
         }
         else
         {
            LPNTXINDEX pIndex = pArea->lpIndexes;
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
               fAppend = pArea->fNtxAppend;
               pArea->fNtxAppend = ZH_FALSE;
            }

            while( pIndex )
            {
               for( i = 0; i < pIndex->iTags; i++ )
               {
                  pTag = pIndex->lpTags[ i ];
                  if( pIndex->fReadonly || pTag->Custom ||
                      ( pTag->pIndex->Compound && ! pTag->HeadBlock ) ||
                      ( fAppend && pTag->ChgOnly ) )
                     continue;

                  pKey = zh_ntxEvalKey( NULL, pTag );

                  fAdd = ( pTag->pForItem == NULL ||
                           zh_ntxEvalCond( pArea, pTag->pForItem, ZH_TRUE ) );
                  if( fAppend )
                  {
                     fDel = ZH_FALSE;
                  }
                  else
                  {
                     if( zh_ntxValCompare( pTag, pKey->key, pTag->KeyLength,
                          pTag->HotKeyInfo->key, pTag->KeyLength, ZH_TRUE ) == 0 )
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
                        if( ! zh_ntxIndexLockWrite( pIndex, ZH_TRUE ) )
                        {
                           zh_ntxKeyFree( pKey );
                           break;
                        }
                        fLck = ZH_TRUE;
                     }
                     if( ( pTag->pIndex->Compound && ! pTag->HeadBlock ) ||
                         ! zh_ntxTagHeaderCheck( pTag ) )
                        fAdd = fDel = ZH_FALSE;
                     if( fDel )
                     {
                        if( zh_ntxTagKeyDel( pTag, pTag->HotKeyInfo ) )
                        {
                           if( ! pIndex->fShared && pTag->keyCount &&
                               zh_ntxKeyInScope( pTag, pTag->HotKeyInfo ) )
                              pTag->keyCount--;
                        }
                        else
                        {
                           if( pTag->ChgOnly )
                              fAdd = ZH_FALSE;
                           else if( ! pTag->Partial && ! pTag->UniqueKey )
                              zh_ntxErrorRT( pTag->pIndex->pArea,
                                             EG_CORRUPTION, EDBF_CORRUPT,
                                             pTag->pIndex->IndexName, 0, 0, NULL );
                        }
                     }
                     if( fAdd )
                     {
                        if( zh_ntxTagKeyAdd( pTag, pKey ) )
                        {
                           if( ! pIndex->fShared && pTag->keyCount &&
                               zh_ntxKeyInScope( pTag, pKey ) )
                              pTag->keyCount++;
                        }
                     }
                  }
                  zh_ntxKeyFree( pKey );
               }
               if( fLck )
               {
                  zh_ntxIndexUnLockWrite( pIndex );
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
static ZH_ERRCODE zh_ntxGoHot( NTXAREAP pArea )
{
   ZH_ERRCODE errCode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ntxGoHot(%p)", ( void * ) pArea ) );

   errCode = SUPER_GOHOT( &pArea->dbfarea.area );
   if( errCode == ZH_SUCCESS )
   {
      if( ! pArea->fNtxAppend )
      {
         LPNTXINDEX pIndex = pArea->lpIndexes;
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
                     pTag->HotKeyInfo = zh_ntxEvalKey( pTag->HotKeyInfo, pTag );
                     pTag->HotFor = ( pTag->pForItem == NULL ||
                                 zh_ntxEvalCond( pArea, pTag->pForItem, ZH_TRUE ) );
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

#define zh_ntxPutRec           NULL
#define zh_ntxPutValue         NULL
#define zh_ntxRecall           NULL
#define zh_ntxRecCount         NULL
#define zh_ntxRecInfo          NULL
#define zh_ntxRecNo            NULL
#define zh_ntxRecId            NULL
#define zh_ntxSetFieldsExtent  NULL
#define zh_ntxAlias            NULL

/*
 * Close the table in the WorkArea.
 */
static ZH_ERRCODE zh_ntxClose( NTXAREAP pArea )
{
   ZH_ERRCODE errCode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ntxClose(%p)", ( void * ) pArea ) );

   if( SELF_GOCOLD( &pArea->dbfarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   errCode = SUPER_CLOSE( &pArea->dbfarea.area );

   if( errCode == ZH_SUCCESS )
   {
      if( pArea->pSort )
      {
         zh_ntxSortFree( pArea->pSort, ZH_TRUE );
         pArea->pSort = NULL;
      }

      SELF_ORDLSTCLEAR( &pArea->dbfarea.area );

      /* close also production indexes if any */
      while( pArea->lpIndexes )
      {
         LPNTXINDEX pIndex = pArea->lpIndexes;
         pArea->lpIndexes = pIndex->pNext;
         zh_ntxIndexFree( pIndex );
      }

#ifdef ZH_NTX_DEBUG_DISP
      printf( "\r\n#reads=%ld, #writes=%ld\r\n", s_rdNO, s_wrNO ); fflush( stdout );
#endif
   }

   return errCode;
}

#define zh_ntxCreate   NULL
#define zh_ntxInfo     NULL
#define zh_ntxNewArea  NULL

/*
 * Retrieve the size of the WorkArea structure.
 */
static ZH_ERRCODE zh_ntxStructSize( NTXAREAP pArea, ZH_USHORT * uiSize )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ntxStructSize(%p, %p)", ( void * ) pArea, ( void * ) uiSize ) );
   ZH_SYMBOL_UNUSED( pArea );

   *uiSize = sizeof( NTXAREA );
   return ZH_SUCCESS;
}

/*
 * Open a data store in the WorkArea.
 */
static ZH_ERRCODE zh_ntxOpen( NTXAREAP pArea, LPDBOPENINFO pOpenInfo )
{
   ZH_ERRCODE errCode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ntxOpen(%p, %p)", ( void * ) pArea, ( void * ) pOpenInfo ) );

   errCode = SUPER_OPEN( &pArea->dbfarea.area, pOpenInfo );

   if( errCode == ZH_SUCCESS && DBFAREA_DATA( &pArea->dbfarea )->fStruct &&
       ( DBFAREA_DATA( &pArea->dbfarea )->fStrictStruct ?
         pArea->dbfarea.fHasTags : zh_setGetAutOpen() ) )
   {
      char szFileName[ ZH_PATH_MAX ];

      zh_ntxCreateFName( pArea, NULL, NULL, szFileName, NULL );
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

#define zh_ntxRelease  NULL
#define zh_ntxSysName  NULL
#define zh_ntxEval     NULL

static ZH_ERRCODE zh_ntxPack( NTXAREAP pArea )
{
   ZH_ERRCODE errCode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ntxPack(%p)", ( void * ) pArea ) );

   errCode = SUPER_PACK( &pArea->dbfarea.area );
   if( errCode == ZH_SUCCESS )
      return SELF_ORDLSTREBUILD( &pArea->dbfarea.area );

   return errCode;
}

#define ntPackRec       NULL
#define zh_ntxSort      NULL
#define zh_ntxTrans     NULL
#define zh_ntxTransRec  NULL

static ZH_ERRCODE zh_ntxZap( NTXAREAP pArea )
{
   ZH_ERRCODE errCode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ntxZap(%p)", ( void * ) pArea ) );

   errCode = SUPER_ZAP( &pArea->dbfarea.area );
   if( errCode == ZH_SUCCESS )
      return SELF_ORDLSTREBUILD( &pArea->dbfarea.area );

   return errCode;
}

#define zh_ntxchildEnd        NULL
#define zh_ntxchildStart      NULL
#define zh_ntxchildSync       NULL
#define zh_ntxsyncChildren    NULL
#define zh_ntxclearRel        NULL
#define zh_ntxforceRel        NULL
#define zh_ntxrelArea         NULL
#define zh_ntxrelEval         NULL
#define zh_ntxrelText         NULL
#define zh_ntxsetRel          NULL

#define zh_ntxOrderCondition  NULL

static ZH_ERRCODE zh_ntxOrderCreate( NTXAREAP pArea, LPDBORDERCREATEINFO pOrderInfo )
{
   PZH_ITEM pResult, pKeyExp, pForExp = NULL;
   int iLen, iDec, iTag, i;
   char szFileName[ ZH_PATH_MAX ], szTagName[ NTX_MAX_TAGNAME + 1 ];
   const char * szKey, * szFor = NULL;
   LPNTXINDEX pIndex, * pIndexPtr;
   LPTAGINFO pTag = NULL;
   LPDBFDATA pData;
   ZH_ERRCODE errCode;
   ZH_ULONG ulRecNo;
   ZH_BOOL fCompound, fTagName, fBagName, fProd, fLocked = ZH_FALSE,
           fAscend = ZH_TRUE, fCustom = ZH_FALSE, fTemporary = ZH_FALSE,
           fExclusive = ZH_FALSE;
   ZH_BYTE bType;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ntxOrderCreate(%p, %p)", ( void * ) pArea, ( void * ) pOrderInfo ) );

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

   bType = zh_ntxItemType( pResult );
   iLen = iDec = 0;
   switch( bType )
   {
      case 'N':
         zh_itemGetNLen( pResult, &iLen, &iDec );
         if( iDec )
            iLen += iDec + 1;
         break;
      case 'D':
         iLen = 8;
         break;
      case 'T':
         iLen = 17;
         break;
      case 'L':
         iLen = 1;
         break;
      case 'C':
         iLen = ( ZH_INTCAST ) zh_itemGetCLen( pResult );
         if( iLen > NTX_MAX_KEY )
            iLen = NTX_MAX_KEY;
         break;
      default:
         bType = 'U';
   }
   zh_itemRelease( pResult );

   /* Make sure KEY has proper type and iLen is not 0 */
   if( bType == 'U' || iLen == 0 )
   {
      zh_vmDestroyBlockOrMacro( pKeyExp );
      SELF_GOTO( &pArea->dbfarea.area, ulRecNo );
      zh_ntxErrorRT( pArea, bType == 'U' ? EG_DATATYPE : EG_DATAWIDTH,
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
         zh_ntxErrorRT( pArea, EG_DATATYPE, EDBF_INVALIDFOR, NULL, 0, 0, NULL );
         return ZH_FAILURE;
      }
   }

   SELF_GOTO( &pArea->dbfarea.area, ulRecNo );

   pData = DBFAREA_DATA( &pArea->dbfarea );
   /*
    * abBagName -> cBag, atomBagName -> cTag
    * The following scheme implemented:
    * 1. abBagName == NULL   -> add the Tag to the structural index
    *    if no compound index support then create new separate index
    *    with atomBagName
    * 2. atomBagName == NULL -> overwrite any index file of abBagName
    * 3. ads the Tag to index file
    */
   fTagName = pOrderInfo->atomBagName && pOrderInfo->atomBagName[ 0 ];
   fBagName = pOrderInfo->abBagName && pOrderInfo->abBagName[ 0 ];
#if defined( ZH_NTX_NOMULTITAG )
   fCompound = ZH_FALSE;
#else
   fCompound = fTagName && pData->fMultiTag;
#endif
   zh_ntxCreateFName( pArea, ( fBagName || fCompound ) ?
                      pOrderInfo->abBagName : pOrderInfo->atomBagName,
                      &fProd, szFileName, szTagName );
   if( fTagName )
      zh_strncpyUpperTrim( szTagName, pOrderInfo->atomBagName, NTX_MAX_TAGNAME );

   pIndex = zh_ntxFindBag( pArea, szFileName );
   if( pIndex && ! fCompound )
   {
      pIndexPtr = &pArea->lpIndexes;
      while( *pIndexPtr )
      {
         if( pIndex == *pIndexPtr )
         {
            *pIndexPtr = pIndex->pNext;
            zh_ntxIndexFree( pIndex );
            break;
         }
         pIndexPtr = &( *pIndexPtr )->pNext;
      }
      pIndex = NULL;
   }

   if( pIndex )
   {
      if( pIndex->fReadonly )
      {
         zh_vmDestroyBlockOrMacro( pKeyExp );
         if( pForExp != NULL )
            zh_vmDestroyBlockOrMacro( pForExp );
         zh_ntxErrorRT( pArea, EG_READONLY, EDBF_READONLY, pIndex->IndexName, 0, 0, NULL );
         return ZH_FAILURE;
      }
#if 0 /* enable this code if you want to forbid tag deleting in shared mode */
      else if( pIndex->fShared )
      {
         zh_vmDestroyBlockOrMacro( pKeyExp );
         if( pForExp != NULL )
            zh_vmDestroyBlockOrMacro( pForExp );
         zh_ntxErrorRT( pArea, EG_SHARED, EDBF_SHARED, pIndex->IndexName, 0, 0, NULL );
         return ZH_FAILURE;
      }
#endif
   }
   else
   {
      PZH_FILE pFile;
      ZH_BOOL bRetry, fOld, fShared = pArea->dbfarea.fShared && ! fTemporary && ! fExclusive;
      PZH_ITEM pError = NULL;
      char szSpFile[ ZH_PATH_MAX ];

      fOld = fCompound;
      do
      {
         if( fTemporary )
         {
            pFile = zh_fileCreateTemp( NULL, NULL, FC_NORMAL, szSpFile );
            fOld = ZH_FALSE;
         }
         else
         {
            pFile = zh_fileExtOpen( szFileName, NULL, FO_READWRITE |
                                    ( fShared ? FO_DENYNONE : FO_EXCLUSIVE ) |
                                    ( fOld ? FXO_APPEND : FXO_TRUNCATE ) |
                                    FXO_DEFAULTS | FXO_SHARELOCK | FXO_COPYNAME |
                                    FXO_NOSEEKPOS,
                                    NULL, pError );
         }
         if( ! pFile )
            bRetry = zh_ntxErrorRT( pArea, EG_CREATE, EDBF_CREATE, szFileName,
                                    zh_fsError(), EF_CANRETRY | EF_CANDEFAULT,
                                    &pError ) == E_RETRY;
         else
         {
            bRetry = ZH_FALSE;
            if( fOld )
               fOld = zh_fileSize( pFile ) != 0;
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

      pIndex = zh_ntxIndexNew( pArea );
      pIndex->IndexName = zh_strdup( szFileName );
      pIndex->fReadonly = ZH_FALSE;
      pIndex->fShared = fShared;
      pIndex->DiskFile = pFile;
      pIndex->fDelete = fTemporary;
      if( fTemporary )
         pIndex->RealName = zh_strdup( szSpFile );
      else
         pIndex->Production = fProd;

      pIndexPtr = &pArea->lpIndexes;
      while( *pIndexPtr )
         pIndexPtr = &( *pIndexPtr )->pNext;
      *pIndexPtr = pIndex;
      pArea->fSetTagNumbers = ZH_TRUE;
      if( fOld )
      {
         if( ! zh_ntxIndexLockWrite( pIndex, ZH_TRUE ) )
            errCode = ZH_FAILURE;
         else
         {
            errCode = zh_ntxIndexLoad( pIndex, szTagName );
            if( errCode != ZH_SUCCESS )
               zh_ntxIndexUnLockWrite( pIndex );
            else
               fLocked = ZH_TRUE;
         }
         if( errCode != ZH_SUCCESS )
         {
            *pIndexPtr = pIndex->pNext;
            zh_ntxIndexFree( pIndex );
            zh_vmDestroyBlockOrMacro( pKeyExp );
            if( pForExp != NULL )
               zh_vmDestroyBlockOrMacro( pForExp );
            zh_ntxErrorRT( pArea, EG_CORRUPTION, EDBF_CORRUPT, szFileName, 0, 0, NULL );
            return errCode;
         }
      }
      else
      {
         pIndex->LargeFile = ( pIndex->pArea->dbfarea.bLockType == DB_DBFLOCK_HB64 );
      }
   }

   iTag = zh_ntxFindTagByName( pIndex, szTagName );
   fCompound = ( pIndex->iTags > ( iTag ? 1 : 0 ) );

   if( ! iTag && pIndex->iTags == CTX_MAX_TAGS )
   {
      if( fLocked )
         zh_ntxIndexUnLockWrite( pIndex );
      zh_vmDestroyBlockOrMacro( pKeyExp );
      if( pForExp != NULL )
         zh_vmDestroyBlockOrMacro( pForExp );
      zh_ntxErrorRT( pArea, EG_LIMIT, EDBF_LIMITEXCEEDED, pIndex->IndexName, 0, 0, NULL );
      return ZH_FAILURE;
   }

   if( ! fLocked && ! zh_ntxIndexLockWrite( pIndex, fCompound ) )
   {
      errCode = ZH_FAILURE;
   }
   else
   {
      if( pIndex->Compound != fCompound )
      {
         pIndex->Compound = fCompound;
         if( fCompound )
         {
            if( ! pIndex->HeaderBuff )
               pIndex->HeaderBuff = ( ZH_BYTE * ) zh_xgrab( NTXBLOCKSIZE );
            memset( pIndex->HeaderBuff, 0, NTXBLOCKSIZE );
            pIndex->fValidHeader = ZH_TRUE;
         }
         for( i = 0; i < pIndex->iTags; i++ )
         {
            pIndex->lpTags[ i ]->HdrChanged = ZH_TRUE;
            pIndex->lpTags[ i ]->HeadBlock = 0;
            if( fCompound )
               zh_ntxIndexTagAdd( pIndex, pIndex->lpTags[ i ] );
         }
      }
      pTag = zh_ntxTagNew( pIndex, szTagName, fTagName,
                           szKey, pKeyExp, bType, ( ZH_USHORT ) iLen, ( ZH_USHORT ) iDec,
                           szFor, pForExp,
                           fAscend, pOrderInfo->fUnique, fCustom, pData->fSortRecNo );
      pTag->Partial = ( pArea->dbfarea.area.lpdbOrdCondInfo && ! pArea->dbfarea.area.lpdbOrdCondInfo->fAll );

      if( ! pIndex->Compound )
      {
         while( pIndex->iTags )
            zh_ntxTagDelete( pIndex->lpTags[ 0 ] );
         zh_ntxIndexTrunc( pIndex );
         iTag = 0;
      }

      if( iTag )
      {
         pTag->HeadBlock = pIndex->lpTags[ iTag - 1 ]->HeadBlock;
         if( zh_ntxTagHeaderCheck( pIndex->lpTags[ iTag - 1 ] ) &&
             ! zh_ntxTagPagesFree( pIndex->lpTags[ iTag - 1 ],
                                   pIndex->lpTags[ iTag - 1 ]->RootBlock ) )
         {
            errCode = ZH_FAILURE;
         }
         else
         {
            pTag->uiNumber = pIndex->lpTags[ iTag - 1 ]->uiNumber;
            zh_ntxTagFree( pIndex->lpTags[ iTag - 1 ] );
            pIndex->lpTags[ iTag - 1 ] = pTag;
         }
      }
      else
      {
         zh_ntxTagAdd( pIndex, pTag );
         if( pIndex->Compound )
            zh_ntxIndexTagAdd( pIndex, pTag );
      }

      if( errCode == ZH_SUCCESS )
      {
         pIndex->Update = pIndex->Changed = pTag->HdrChanged = ZH_TRUE;
         errCode = zh_ntxTagCreate( pTag, ZH_FALSE );
      }
      zh_ntxIndexUnLockWrite( pIndex );
   }

   pIndexPtr = &pArea->lpIndexes;
   while( *pIndexPtr && *pIndexPtr != pIndex )
      pIndexPtr = &( *pIndexPtr )->pNext;

   /* It should not happen, reentrance? */
   if( ! *pIndexPtr )
      return ZH_FAILURE;

   if( errCode != ZH_SUCCESS )
   {
      *pIndexPtr = pIndex->pNext;
      zh_ntxIndexFree( pIndex );
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
   }
   if( pIndex->Production && ! pArea->dbfarea.fHasTags &&
       pData->fStruct && ( pData->fStrictStruct || zh_setGetAutOpen() ) )
   {
      pArea->dbfarea.fHasTags = ZH_TRUE;
      if( ! pArea->dbfarea.fReadonly && ( pArea->dbfarea.dbfHeader.bHasTags & 0x01 ) == 0 )
         SELF_WRITEDBHEADER( &pArea->dbfarea.area );
   }
   pArea->lpCurTag = pTag;
   SELF_ORDSETCOND( &pArea->dbfarea.area, NULL );
   return SELF_GOTOP( &pArea->dbfarea.area );
}

static ZH_ERRCODE zh_ntxOrderDestroy( NTXAREAP pArea, LPDBORDERINFO pOrderInfo )
{
   ZH_ERRCODE errCode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ntxOrderDestroy(%p, %p)", ( void * ) pArea, ( void * ) pOrderInfo ) );

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
      LPTAGINFO pTag = zh_ntxFindTag( pArea, pOrderInfo->itmOrder, pOrderInfo->atomBagName );

      if( pTag )
      {
         LPNTXINDEX pIndex = pTag->pIndex;

         if( pIndex->iTags == 1 )
         {
            ZH_BOOL fProd = pIndex->Production;
            LPNTXINDEX * pIndexPtr = &pArea->lpIndexes;
            while( *pIndexPtr != pIndex )
               pIndexPtr = &(*pIndexPtr)->pNext;
            *pIndexPtr = pIndex->pNext;
            pIndex->fDelete = ZH_TRUE;
            zh_ntxIndexFree( pIndex );
            if( fProd && pArea->dbfarea.fHasTags &&
                DBFAREA_DATA( &pArea->dbfarea )->fStruct &&
                ( DBFAREA_DATA( &pArea->dbfarea )->fStrictStruct || zh_setGetAutOpen() ) )
            {
               pArea->dbfarea.fHasTags = ZH_FALSE;
               if( ! pArea->dbfarea.fReadonly && ( pArea->dbfarea.dbfHeader.bHasTags & 0x01 ) != 0 )
                  SELF_WRITEDBHEADER( &pArea->dbfarea.area );
            }
         }
         else if( pIndex->fReadonly )
         {
            zh_ntxErrorRT( pArea, EG_READONLY, EDBF_READONLY, pIndex->IndexName, 0, 0, NULL );
            return ZH_FAILURE;
         }
#if 0    /* enable this code if you want to forbid tag deleting in shared mode */
         else if( pIndex->fShared )
         {
            zh_ntxErrorRT( pArea, EG_SHARED, EDBF_SHARED, pIndex->IndexName, 0, 0, NULL );
            return ZH_FAILURE;
         }
#endif
         else if( ! zh_ntxIndexLockWrite( pIndex, ZH_TRUE ) )
         {
            return ZH_FAILURE;
         }
         else
         {
            errCode = zh_ntxTagSpaceFree( pTag );
            zh_ntxIndexUnLockWrite( pIndex );
         }
      }
   }

   return errCode;
}

static ZH_ERRCODE zh_ntxOrderInfo( NTXAREAP pArea, ZH_USHORT uiIndex, LPDBORDERINFO pInfo )
{
   LPTAGINFO pTag;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ntxOrderInfo(%p, %hu, %p)", ( void * ) pArea, uiIndex, ( void * ) pInfo ) );

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
            LPNTXINDEX pIndex = zh_ntxFindBag( pArea,
                                       zh_itemGetCPtr( pInfo->atomBagName ) );
            i = pIndex ? pIndex->iTags : 0;
         }
         else
            i = zh_ntxTagCount( pArea );

         pInfo->itmResult = zh_itemPutNI( pInfo->itmResult, i );
         return ZH_SUCCESS;
      }
      case DBOI_BAGCOUNT:
      {
         int i = 0;
         LPNTXINDEX pIndex = pArea->lpIndexes;
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
         LPNTXINDEX pIndex = pArea->lpIndexes, pIndexSeek = NULL;
         int i = 0;

         if( zh_itemGetCLen( pInfo->atomBagName ) > 0 )
            pIndexSeek = zh_ntxFindBag( pArea,
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
         LPNTXINDEX pIndex = pArea->lpIndexes, pIndexSeek = NULL;
         int i = 0;

         if( zh_itemGetCLen( pInfo->atomBagName ) > 0 )
            pIndexSeek = zh_ntxFindBag( pArea,
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

   pTag = zh_ntxFindTag( pArea, pInfo->itmOrder, pInfo->atomBagName );

   if( pTag )
   {
      switch( uiIndex )
      {
         case DBOI_CONDITION:
            pInfo->itmResult = zh_itemPutC( pInfo->itmResult, pTag->ForExpr ? pTag->ForExpr : NULL );
            if( zh_itemType( pInfo->itmNewVal ) & ZH_IT_STRING )
            {
               const char * szForExpr = zh_itemGetCPtr( pInfo->itmNewVal );
               if( pTag->ForExpr ?
                   strncmp( pTag->ForExpr, szForExpr, NTX_MAX_EXP ) != 0 :
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
                  if( fOK && zh_ntxTagLockWrite( pTag ) )
                  {
                     if( pTag->ForExpr )
                        zh_xfree( pTag->ForExpr );
                     if( pTag->pForItem )
                        zh_vmDestroyBlockOrMacro( pTag->pForItem );
                     if( pForItem )
                     {
                        pTag->ForExpr = zh_strndup( szForExpr, NTX_MAX_EXP );
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
                     zh_ntxTagUnLockWrite( pTag );
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
            pInfo->itmResult = zh_itemPutNI( pInfo->itmResult, zh_ntxFindTagNum( pArea, pTag ) );
            break;
         case DBOI_FILEHANDLE:
            pInfo->itmResult = zh_itemPutNInt( pInfo->itmResult, ( ZH_NHANDLE )
                                               zh_fileHandle( pTag->pIndex->DiskFile ) );
            break;
         case DBOI_FULLPATH:
            pInfo->itmResult = zh_itemPutC( pInfo->itmResult, pTag->pIndex->IndexName );
            break;
         case DBOI_KEYCOUNT:
         case DBOI_KEYCOUNTRAW:
            pInfo->itmResult = zh_itemPutNL( pInfo->itmResult, zh_ntxOrdKeyCount( pTag ) );
            break;
         case DBOI_POSITION:
         case DBOI_KEYNORAW:
         /* case DBOI_RECNO: */
            if( zh_itemType( pInfo->itmNewVal ) & ZH_IT_NUMERIC )
               pInfo->itmResult = zh_itemPutL( pInfo->itmResult,
                  zh_ntxOrdKeyGoto( pTag, zh_itemGetNL( pInfo->itmNewVal ) ) );
            else
               pInfo->itmResult = zh_itemPutNL( pInfo->itmResult, zh_ntxOrdKeyNo( pTag ) );
            break;
         case DBOI_RELKEYPOS:
            if( zh_itemType( pInfo->itmNewVal ) & ZH_IT_NUMERIC )
               zh_ntxOrdSetRelKeyPos( pTag, zh_itemGetND( pInfo->itmNewVal ) );
            else
               pInfo->itmResult = zh_itemPutND( pInfo->itmResult, zh_ntxOrdGetRelKeyPos( pTag ) );
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
                zh_itemType( pInfo->itmNewVal ) & ZH_IT_LOGICAL )
            {
               ZH_BOOL fNewVal = zh_itemGetL( pInfo->itmNewVal );
               if( pTag->Custom ? ! fNewVal : fNewVal )
               {
                  if( zh_ntxTagLockWrite( pTag ) )
                  {
                     if( ! pTag->Template && ( pTag->Custom ? ! fNewVal : fNewVal ) )
                     {
                        pTag->Custom = fNewVal;
                        pTag->Partial = ZH_TRUE;
                        pTag->ChgOnly = ZH_FALSE;
                        pTag->HdrChanged = ZH_TRUE;
                     }
                     zh_ntxTagUnLockWrite( pTag );
                  }
               }
            }
            pInfo->itmResult = zh_itemPutL( pInfo->itmResult, pTag->Custom );
            break;
         case DBOI_CHGONLY:
            if( ! pTag->Custom &&
                zh_itemType( pInfo->itmNewVal ) & ZH_IT_LOGICAL )
            {
               ZH_BOOL fNewVal = zh_itemGetL( pInfo->itmNewVal );
               if( pTag->ChgOnly ? ! fNewVal : fNewVal )
               {
                  if( zh_ntxTagLockWrite( pTag ) )
                  {
                     if( ! pTag->Custom && ( pTag->ChgOnly ? ! fNewVal : fNewVal ) )
                     {
                        pTag->ChgOnly = fNewVal;
                        pTag->Partial = ZH_TRUE;
                        pTag->HdrChanged = ZH_TRUE;
                     }
                     zh_ntxTagUnLockWrite( pTag );
                  }
               }
            }
            pInfo->itmResult = zh_itemPutL( pInfo->itmResult, pTag->ChgOnly );
            break;
         case DBOI_TEMPLATE:
            if( pTag->Custom && ! pTag->Template &&
                zh_itemGetL( pInfo->itmNewVal ) )
            {
               if( zh_ntxTagLockWrite( pTag ) )
               {
                  if( pTag->Custom && ! pTag->Template )
                  {
                     pTag->Template = ZH_TRUE;
                     pTag->HdrChanged = ZH_TRUE;
                  }
                  zh_ntxTagUnLockWrite( pTag );
               }
            }
            pInfo->itmResult = zh_itemPutL( pInfo->itmResult, pTag->Template );
            break;
         case DBOI_MULTIKEY:
            if( pTag->Custom && ! pTag->MultiKey &&
                zh_itemGetL( pInfo->itmNewVal ) )
            {
               if( zh_ntxTagLockWrite( pTag ) )
               {
                  if( pTag->Custom && ! pTag->MultiKey )
                  {
                     pTag->MultiKey = ZH_TRUE;
                     pTag->HdrChanged = ZH_TRUE;
                  }
                  zh_ntxTagUnLockWrite( pTag );
               }
            }
            pInfo->itmResult = zh_itemPutL( pInfo->itmResult, pTag->MultiKey );
            break;
         case DBOI_PARTIAL:
            pInfo->itmResult = zh_itemPutL( pInfo->itmResult, pTag->Partial );
            break;
         case DBOI_SCOPETOP:
            if( pInfo->itmResult )
               zh_ntxTagGetScope( pTag, 0, pInfo->itmResult );
            if( pInfo->itmNewVal )
               zh_ntxTagSetScope( pTag, 0, pInfo->itmNewVal );
            break;
         case DBOI_SCOPEBOTTOM:
            if( pInfo->itmResult )
               zh_ntxTagGetScope( pTag, 1, pInfo->itmResult );
            if( pInfo->itmNewVal )
               zh_ntxTagSetScope( pTag, 1, pInfo->itmNewVal );
            break;
         case DBOI_SCOPESET:
            if( pInfo->itmNewVal )
            {
               zh_ntxTagSetScope( pTag, 0, pInfo->itmNewVal );
               zh_ntxTagSetScope( pTag, 1, pInfo->itmNewVal );
            }
            if( pInfo->itmResult )
               zh_itemClear( pInfo->itmResult );
            break;
         case DBOI_SCOPETOPCLEAR:
            if( pInfo->itmResult )
               zh_ntxTagGetScope( pTag, 0, pInfo->itmResult );
            zh_ntxTagClearScope( pTag, 0 );
            break;
         case DBOI_SCOPEBOTTOMCLEAR:
            if( pInfo->itmResult )
               zh_ntxTagGetScope( pTag, 1, pInfo->itmResult );
            zh_ntxTagClearScope( pTag, 1 );
            break;
         case DBOI_SCOPECLEAR:
            zh_ntxTagClearScope( pTag, 0 );
            zh_ntxTagClearScope( pTag, 1 );
            if( pInfo->itmResult )
               zh_itemClear( pInfo->itmResult );
            break;
         case DBOI_KEYADD:
            if( pTag->pIndex->fReadonly )
            {
               zh_ntxErrorRT( pArea, EG_READONLY, EDBF_READONLY,
                              pTag->pIndex->IndexName, 0, 0, NULL );
               return ZH_FAILURE;
            }
            if( pTag->Custom )
            {
               pInfo->itmResult = zh_itemPutL( pInfo->itmResult,
                            zh_ntxOrdKeyAdd( pTag, pInfo->itmNewVal ) );
            }
            else
            {
               zh_ntxErrorRT( pArea, 0, EDBF_NOTCUSTOM, NULL, 0, 0, NULL );
               return ZH_FAILURE;
            }
            break;
         case DBOI_KEYDELETE:
            if( pTag->pIndex->fReadonly )
            {
               zh_ntxErrorRT( pArea, EG_READONLY, EDBF_READONLY,
                              pTag->pIndex->IndexName, 0, 0, NULL );
               return ZH_FAILURE;
            }
            if( pTag->Custom )
            {
               pInfo->itmResult = zh_itemPutL( pInfo->itmResult,
                            zh_ntxOrdKeyDel( pTag, pInfo->itmNewVal ) );
            }
            else
            {
               zh_ntxErrorRT( pArea, 0, EDBF_NOTCUSTOM, NULL, 0, 0, NULL );
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
            pInfo->itmResult = zh_itemPutNI( pInfo->itmResult, pTag->KeyDec );
            break;
         case DBOI_KEYVAL:
            if( zh_ntxTagLockRead( pTag ) )
            {
               if( zh_ntxCurKeyRefresh( pTag ) )
                  pInfo->itmResult = zh_ntxKeyGetItem( pInfo->itmResult,
                                       pTag->CurKeyInfo, pTag, ZH_TRUE );
               else if( pInfo->itmResult )
                  zh_itemClear( pInfo->itmResult );
               zh_ntxTagUnLockRead( pTag );
            }
            break;
         case DBOI_SKIPUNIQUE:
            pInfo->itmResult = zh_itemPutL( pInfo->itmResult,
               zh_ntxOrdSkipUnique( pTag,
                        pInfo->itmNewVal && ZH_IS_NUMERIC( pInfo->itmNewVal ) ?
                        zh_itemGetNL( pInfo->itmNewVal ) : 1 ) );
            break;
         case DBOI_SKIPEVAL:
         case DBOI_SKIPEVALBACK:
            pInfo->itmResult = zh_itemPutL( pInfo->itmResult,
               zh_ntxOrdSkipEval( pTag, uiIndex == DBOI_SKIPEVAL, pInfo->itmNewVal ) );
            break;
         case DBOI_SKIPWILD:
         case DBOI_SKIPWILDBACK:
            pInfo->itmResult = zh_itemPutL( pInfo->itmResult,
               zh_ntxOrdSkipWild( pTag, uiIndex == DBOI_SKIPWILD, pInfo->itmNewVal ) );
            break;
         case DBOI_SKIPREGEX:
         case DBOI_SKIPREGEXBACK:
            pInfo->itmResult = zh_itemPutL( pInfo->itmResult,
               zh_ntxOrdSkipRegEx( pTag, uiIndex == DBOI_SKIPREGEX, pInfo->itmNewVal ) );
            break;
         case DBOI_FINDREC:
         case DBOI_FINDRECCONT:
            pInfo->itmResult = zh_itemPutL( pInfo->itmResult,
               zh_ntxOrdFindRec( pTag, zh_itemGetNL( pInfo->itmNewVal ),
                                 uiIndex == DBOI_FINDRECCONT ) );
            break;
         case DBOI_SCOPEEVAL:
            if( ( zh_itemType( pInfo->itmNewVal ) & ZH_IT_ARRAY ) &&
                zh_arrayLen( pInfo->itmNewVal ) == DBRMI_SIZE &&
                zh_arrayGetPtr( pInfo->itmNewVal, DBRMI_FUNCTION ) != NULL )
            {
               pInfo->itmResult = zh_itemPutNL( pInfo->itmResult,
                  zh_ntxOrdScopeEval( pTag, ( ZH_EVALSCOPE_FUNC )
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
            if( zh_ntxIndexLockRead( pTag->pIndex ) )
               zh_ntxIndexUnLockRead( pTag->pIndex );
            pInfo->itmResult = zh_itemPutNInt( pInfo->itmResult, pTag->pIndex->Version );
            break;
         case DBOI_READLOCK:
            if( zh_itemType( pInfo->itmNewVal ) & ZH_IT_LOGICAL )
               pInfo->itmResult = zh_itemPutL( pInfo->itmResult,
                            zh_itemGetL( pInfo->itmNewVal ) ?
                                 zh_ntxIndexLockRead( pTag->pIndex ) :
                                 zh_ntxIndexUnLockRead( pTag->pIndex ) );
            else
               pInfo->itmResult = zh_itemPutL( pInfo->itmResult, pTag->pIndex->lockRead > 0 );
            break;
         case DBOI_WRITELOCK:
            if( zh_itemType( pInfo->itmNewVal ) & ZH_IT_LOGICAL )
               pInfo->itmResult = zh_itemPutL( pInfo->itmResult,
                            zh_itemGetL( pInfo->itmNewVal ) ?
                                 zh_ntxIndexLockWrite( pTag->pIndex, ZH_TRUE ) :
                                 zh_ntxIndexUnLockWrite( pTag->pIndex ) );
            else
               pInfo->itmResult = zh_itemPutL( pInfo->itmResult, pTag->pIndex->lockWrite > 0 );
            break;
         case DBOI_ISSORTRECNO:
            pInfo->itmResult = zh_itemPutL( pInfo->itmResult, pTag->fSortRec );
            break;
         case DBOI_ISMULTITAG:
#if defined( ZH_NTX_NOMULTITAG )
            pInfo->itmResult = zh_itemPutL( pInfo->itmResult, ZH_FALSE );
#else
            pInfo->itmResult = zh_itemPutL( pInfo->itmResult, pTag->pIndex->Compound );
#endif
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
#if defined( ZH_NTX_NOMULTITAG )
            pInfo->itmResult = zh_itemPutNI( pInfo->itmResult, DBOI_TYPE_NONCOMPACT );
#else
            pInfo->itmResult = zh_itemPutNI( pInfo->itmResult,
               pTag->pIndex->Compound ? DBOI_TYPE_COMPOUND : DBOI_TYPE_NONCOMPACT );
#endif
            break;
         case DBOI_INDEXPAGESIZE:
            pInfo->itmResult = zh_itemPutNI( pInfo->itmResult, NTXBLOCKSIZE );
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

static ZH_ERRCODE zh_ntxOrderListAdd( NTXAREAP pArea, LPDBORDERINFO pOrderInfo )
{
   PZH_FILE pFile;
   char szFileName[ ZH_PATH_MAX ], szTagName[ NTX_MAX_TAGNAME + 1 ];
   LPNTXINDEX pIndex;
   ZH_ERRCODE errCode;
   ZH_BOOL fProd;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ntxOrderListAdd(%p, %p)", ( void * ) pArea, ( void * ) pOrderInfo ) );

   errCode = SELF_GOCOLD( &pArea->dbfarea.area );
   if( errCode != ZH_SUCCESS )
      return errCode;

   if( zh_itemGetCLen( pOrderInfo->atomBagName ) == 0 )
      return ZH_FAILURE;

   zh_ntxCreateFName( pArea, zh_itemGetCPtr( pOrderInfo->atomBagName ),
                      &fProd, szFileName, szTagName );

#if 0
   if( ! szTagName[ 0 ] )
      return ZH_FAILURE;
#endif

   pIndex = zh_ntxFindBag( pArea, szFileName );

   if( ! pIndex )
   {
      PZH_ITEM pError = NULL;
      LPNTXINDEX * pIndexPtr;
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
            fRetry = zh_ntxErrorRT( pArea, EG_OPEN, EDBF_OPEN_INDEX, szFileName,
                                    zh_fsError(), EF_CANRETRY | EF_CANDEFAULT,
                                    &pError ) == E_RETRY;
         }
      }
      while( fRetry );

      if( pError )
         zh_errRelease( pError );

      if( ! pFile )
         return ZH_FAILURE;

      pIndex = zh_ntxIndexNew( pArea );
      pIndex->IndexName = zh_strdup( szFileName );
      pIndex->fReadonly = fReadonly;
      pIndex->fShared = fShared;
      pIndex->DiskFile = pFile;
      pIndex->Production = fProd;

      pIndexPtr = &pArea->lpIndexes;
      while( *pIndexPtr )
         pIndexPtr = &( *pIndexPtr )->pNext;
      *pIndexPtr = pIndex;

      if( zh_ntxIndexLockRead( pIndex ) )
      {
         errCode = zh_ntxIndexLoad( pIndex, szTagName );
         zh_ntxIndexUnLockRead( pIndex );
      }
      else
         errCode = ZH_FAILURE;

      if( errCode != ZH_SUCCESS )
      {
         *pIndexPtr = pIndex->pNext;
         zh_ntxIndexFree( pIndex );
         zh_ntxErrorRT( pArea, EG_CORRUPTION, EDBF_CORRUPT, szFileName, 0, 0, NULL );
         return errCode;
      }
   }

   if( ! pArea->lpCurTag && pIndex->iTags )
   {
      pArea->lpCurTag = pIndex->lpTags[ 0 ];
      errCode = SELF_GOTOP( &pArea->dbfarea.area );
   }
   return errCode;
}

static ZH_ERRCODE zh_ntxOrderListClear( NTXAREAP pArea )
{
   LPNTXINDEX * pIndexPtr, pIndex;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ntxOrderListClear(%p)", ( void * ) pArea ) );

   if( SELF_GOCOLD( &pArea->dbfarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   pArea->lpCurTag = NULL;
   pIndexPtr = &pArea->lpIndexes;
   while( *pIndexPtr )
   {
      pIndex = *pIndexPtr;
      if( DBFAREA_DATA( &pArea->dbfarea )->fStruct && pIndex->Production &&
          ( DBFAREA_DATA( &pArea->dbfarea )->fStrictStruct ? pArea->dbfarea.fHasTags :
                                                   zh_setGetAutOpen() ) )
      {
         pIndexPtr = &pIndex->pNext;
      }
      else
      {
         *pIndexPtr = pIndex->pNext;
         zh_ntxIndexFree( pIndex );
      }
   }
   return ZH_SUCCESS;
}

static ZH_ERRCODE zh_ntxOrderListDelete( NTXAREAP pArea, LPDBORDERINFO pOrderInfo )
{
   char szTagName[ NTX_MAX_TAGNAME + 1 ];
   char szFileName[ ZH_PATH_MAX ];
   LPNTXINDEX pIndex;
   ZH_BOOL fProd;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ntxOrderListDelete(%p, %p)", ( void * ) pArea, ( void * ) pOrderInfo ) );

   if( SELF_GOCOLD( &pArea->dbfarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   zh_ntxCreateFName( pArea, zh_itemGetCPtr( pOrderInfo->atomBagName ), &fProd,
                      szFileName, szTagName );
   pIndex = zh_ntxFindBag( pArea, szFileName );

   if( pIndex && ! ( pIndex->Production && DBFAREA_DATA( &pArea->dbfarea )->fStruct &&
                     ( DBFAREA_DATA( &pArea->dbfarea )->fStrictStruct ?
                       pArea->dbfarea.fHasTags : zh_setGetAutOpen() ) ) )
   {
      LPNTXINDEX * pIndexPtr = &pArea->lpIndexes;
      while( *pIndexPtr )
      {
         if( pIndex == *pIndexPtr )
         {
            *pIndexPtr = pIndex->pNext;
            zh_ntxIndexFree( pIndex );
            break;
         }
         pIndexPtr = &( *pIndexPtr )->pNext;
      }
   }
   return ZH_SUCCESS;
}

static ZH_ERRCODE zh_ntxOrderListFocus( NTXAREAP pArea, LPDBORDERINFO pOrderInfo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ntxOrderListFocus(%p, %p)", ( void * ) pArea, ( void * ) pOrderInfo ) );

   pOrderInfo->itmResult = zh_itemPutC( pOrderInfo->itmResult,
                             pArea->lpCurTag ? pArea->lpCurTag->TagName : NULL );

   if( pOrderInfo->itmOrder )
   {
      LPTAGINFO pTag = zh_ntxFindTag( pArea, pOrderInfo->itmOrder,
                                      pOrderInfo->atomBagName );
      /*
       * In Clipper tag is not changed when bad name is given in DBFNTX
       * but not in DBFCDX. I'd like to keep the same behavior in
       * [x]Ziher RDDs and I chosen DBFCDX one as default. [druzus]
       */
      pArea->lpCurTag = pTag;
   }

   return ZH_SUCCESS;
}

static ZH_ERRCODE zh_ntxOrderListRebuild( NTXAREAP pArea )
{
   LPTAGINFO pCurrTag;
   LPNTXINDEX pIndex;
   ZH_ERRCODE errCode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ntxOrderListRebuild(%p)", ( void * ) pArea ) );

   errCode = SELF_GOCOLD( &pArea->dbfarea.area );
   if( errCode != ZH_SUCCESS )
      return errCode;

   if( pArea->dbfarea.fShared )
   {
      zh_ntxErrorRT( pArea, EG_SHARED, EDBF_SHARED, pArea->dbfarea.szDataFileName, 0, 0, NULL );
      return ZH_FAILURE;
   }
   if( pArea->dbfarea.fReadonly )
   {
      zh_ntxErrorRT( pArea, EG_READONLY, EDBF_READONLY, pArea->dbfarea.szDataFileName, 0, 0, NULL );
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
      errCode = zh_ntxReIndex( pIndex );
      pIndex = pIndex->pNext;
   }
   if( errCode == ZH_SUCCESS )
   {
      pArea->lpCurTag = pCurrTag;
      errCode = SELF_GOTOP( &pArea->dbfarea.area );
   }
   return errCode;
}

#define zh_ntxClearFilter  NULL
#define zh_ntxClearLocate  NULL
#define zh_ntxClearScope   NULL

static ZH_ERRCODE zh_ntxCountScope( NTXAREAP pArea, void * pPtr, ZH_LONG * plRecNo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ntxCountScope(%p, %p, %p)", ( void * ) pArea, pPtr, ( void * ) plRecNo ) );

   if( pPtr == NULL )
   {
      return ZH_SUCCESS;
   }
   return SUPER_COUNTSCOPE( &pArea->dbfarea.area, pPtr, plRecNo );
}

#define zh_ntxFilterText     NULL
#define zh_ntxScopeInfo      NULL
#define zh_ntxSetFilter      NULL
#define zh_ntxSetLocate      NULL
#define zh_ntxSetScope       NULL
#define zh_ntxSkipScope      NULL
#define zh_ntxLocate         NULL
#define zh_ntxCompile        NULL
#define zh_ntxError          NULL
#define zh_ntxEvalBlock      NULL
#define zh_ntxRawLock        NULL
#define zh_ntxLock           NULL
#define zh_ntxUnLock         NULL
#define zh_ntxCloseMemFile   NULL
#define zh_ntxCreateMemFile  NULL
#define zh_ntxGetValueFile   NULL
#define zh_ntxOpenMemFile    NULL
#define zh_ntxPutValueFile   NULL
#define zh_ntxReadDBHeader   NULL
#define zh_ntxWriteDBHeader  NULL

#define zh_ntxInit           NULL
#define zh_ntxExit           NULL
#define zh_ntxDrop           NULL
#define zh_ntxExists         NULL
#define zh_ntxRename         NULL

static ZH_ERRCODE zh_ntxRddInfo( LPRDDNODE pRDD, ZH_USHORT uiIndex, ZH_ULONG ulConnect, PZH_ITEM pItem )
{
   LPDBFDATA pData;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ntxRddInfo(%p, %hu, %lu, %p)", ( void * ) pRDD, uiIndex, ulConnect, ( void * ) pItem ) );

   pData = DBFNODE_DATA( pRDD );

   if( pData->bMemoType == 0 )
   {
      pData->bMemoType = DB_MEMO_DBT;
#if ! defined( ZH_NTX_NOMULTITAG )
      pData->fMultiTag = ZH_TRUE;
#endif
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
         zh_itemPutC( pItem, pData->szIndexExt[ 0 ] ? pData->szIndexExt : NTX_INDEXEXT );
         if( szNewVal )
         {
            zh_strncpy( pData->szIndexExt, szNewVal, sizeof( pData->szIndexExt ) - 1 );
            zh_xfree( szNewVal );
         }
         break;
      }

      case RDDI_MULTITAG:
      {
#if defined( ZH_NTX_NOMULTITAG )
         zh_itemPutL( pItem, ZH_FALSE );
#else
         ZH_BOOL fMultiTag = pData->fMultiTag;
         if( zh_itemType( pItem ) & ZH_IT_LOGICAL )
            pData->fMultiTag = zh_itemGetL( pItem );
         zh_itemPutL( pItem, fMultiTag );
#endif
         break;
      }

      case RDDI_SORTRECNO:
      {
         ZH_BOOL fSortRecNo = pData->fSortRecNo;
         if( zh_itemType( pItem ) & ZH_IT_LOGICAL )
            pData->fSortRecNo = zh_itemGetL( pItem );
         zh_itemPutL( pItem, fSortRecNo );
         break;
      }

      case RDDI_STRUCTORD:
      {
         ZH_BOOL fStruct = pData->fStruct;
         if( zh_itemType( pItem ) & ZH_IT_LOGICAL )
            pData->fStruct = zh_itemGetL( pItem );
         zh_itemPutL( pItem, fStruct );
         break;
      }

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

#define zh_ntxWhoCares  NULL

static const RDDFUNCS ntxTable = {
                             zh_ntxBof,
                             zh_ntxEof,
                             zh_ntxFound,
              ( DBENTRYP_V ) zh_ntxGoBottom,
                             zh_ntxGoTo,
                             zh_ntxGoToId,
              ( DBENTRYP_V ) zh_ntxGoTop,
            ( DBENTRYP_BIB ) zh_ntxSeek,
                             zh_ntxSkip,
                             zh_ntxSkipFilter,
              ( DBENTRYP_L ) zh_ntxSkipRaw,
                             zh_ntxAddField,
              ( DBENTRYP_B ) zh_ntxAppend,
                             zh_ntxCreateFields,
                             zh_ntxDeleteRec,
                             zh_ntxDeleted,
                             zh_ntxFieldCount,
                             zh_ntxFieldDisplay,
                             zh_ntxFieldInfo,
                             zh_ntxFieldName,
              ( DBENTRYP_V ) zh_ntxFlush,
                             zh_ntxGetRec,
                             zh_ntxGetValue,
                             zh_ntxGetVarLen,
              ( DBENTRYP_V ) zh_ntxGoCold,
              ( DBENTRYP_V ) zh_ntxGoHot,
                             zh_ntxPutRec,
                             zh_ntxPutValue,
                             zh_ntxRecall,
                             zh_ntxRecCount,
                             zh_ntxRecInfo,
                             zh_ntxRecNo,
                             zh_ntxRecId,
                             zh_ntxSetFieldsExtent,
                             zh_ntxAlias,
              ( DBENTRYP_V ) zh_ntxClose,
                             zh_ntxCreate,
                             zh_ntxInfo,
                             zh_ntxNewArea,
             ( DBENTRYP_VO ) zh_ntxOpen,
                             zh_ntxRelease,
             ( DBENTRYP_SP ) zh_ntxStructSize,
                             zh_ntxSysName,
                             zh_ntxEval,
              ( DBENTRYP_V ) zh_ntxPack,
                             ntPackRec,
                             zh_ntxSort,
                             zh_ntxTrans,
                             zh_ntxTransRec,
              ( DBENTRYP_V ) zh_ntxZap,
                             zh_ntxchildEnd,
                             zh_ntxchildStart,
                             zh_ntxchildSync,
                             zh_ntxsyncChildren,
                             zh_ntxclearRel,
                             zh_ntxforceRel,
                             zh_ntxrelArea,
                             zh_ntxrelEval,
                             zh_ntxrelText,
                             zh_ntxsetRel,
            ( DBENTRYP_VOI ) zh_ntxOrderListAdd,
              ( DBENTRYP_V ) zh_ntxOrderListClear,
            ( DBENTRYP_VOI ) zh_ntxOrderListDelete,
            ( DBENTRYP_VOI ) zh_ntxOrderListFocus,
              ( DBENTRYP_V ) zh_ntxOrderListRebuild,
                             zh_ntxOrderCondition,
            ( DBENTRYP_VOC ) zh_ntxOrderCreate,
            ( DBENTRYP_VOI ) zh_ntxOrderDestroy,
           ( DBENTRYP_SVOI ) zh_ntxOrderInfo,
                             zh_ntxClearFilter,
                             zh_ntxClearLocate,
                             zh_ntxClearScope,
           ( DBENTRYP_VPLP ) zh_ntxCountScope,
                             zh_ntxFilterText,
                             zh_ntxScopeInfo,
                             zh_ntxSetFilter,
                             zh_ntxSetLocate,
                             zh_ntxSetScope,
                             zh_ntxSkipScope,
                             zh_ntxLocate,
                             zh_ntxCompile,
                             zh_ntxError,
                             zh_ntxEvalBlock,
                             zh_ntxRawLock,
                             zh_ntxLock,
                             zh_ntxUnLock,
                             zh_ntxCloseMemFile,
                             zh_ntxCreateMemFile,
                             zh_ntxGetValueFile,
                             zh_ntxOpenMemFile,
                             zh_ntxPutValueFile,
                             zh_ntxReadDBHeader,
                             zh_ntxWriteDBHeader,
                             zh_ntxInit,
                             zh_ntxExit,
                             zh_ntxDrop,
                             zh_ntxExists,
                             zh_ntxRename,
                             zh_ntxRddInfo,
                             zh_ntxWhoCares
                           };

ZH_FUNC_TRANSLATE( DBFNTX, _DBF )

ZH_FUNC_STATIC( DBFNTX_GETFUNCTABLE )
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
      errCode = zh_rddInheritEx( pTable, &ntxTable, &ntxSuper, "DBFFPT", puiSuperRddId );
      if( errCode != ZH_SUCCESS )
         errCode = zh_rddInheritEx( pTable, &ntxTable, &ntxSuper, "DBFDBT", puiSuperRddId );
      if( errCode != ZH_SUCCESS )
         errCode = zh_rddInheritEx( pTable, &ntxTable, &ntxSuper, "DBF", puiSuperRddId );
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

static void zh_dbfntxRddInit( void * cargo )
{
   ZH_SYMBOL_UNUSED( cargo );

   if( zh_rddRegister( "DBF", RDT_FULL ) <= 1 )
   {
      zh_rddRegister( "DBFFPT", RDT_FULL );
      if( zh_rddRegister( "DBFNTX", RDT_FULL ) <= 1 )
         return;
   }

   zh_errInternal( ZH_EI_RDDINVALID, NULL, NULL, NULL );
}

ZH_INIT_SYMBOLS_BEGIN( dbfntx1__InitSymbols )
{ "DBFNTX",              {ZH_FS_PUBLIC|ZH_FS_LOCAL}, {ZH_FUNCNAME( DBFNTX )}, NULL },
{ "DBFNTX_GETFUNCTABLE", {ZH_FS_PUBLIC|ZH_FS_LOCAL}, {ZH_FUNCNAME( DBFNTX_GETFUNCTABLE )}, NULL }
ZH_INIT_SYMBOLS_END( dbfntx1__InitSymbols )

ZH_CALL_ON_STARTUP_BEGIN( _zh_dbfntx_rdd_init_ )
   zh_vmAtInit( zh_dbfntxRddInit, NULL );
ZH_CALL_ON_STARTUP_END( _zh_dbfntx_rdd_init_ )

#if defined( ZH_PRAGMA_STARTUP )
   #pragma startup dbfntx1__InitSymbols
   #pragma startup _zh_dbfntx_rdd_init_
#elif defined( ZH_DATASEG_STARTUP )
   #define ZH_DATASEG_BODY    ZH_DATASEG_FUNC( dbfntx1__InitSymbols ) \
                              ZH_DATASEG_FUNC( _zh_dbfntx_rdd_init_ )
   #include "hbiniseg.h"
#endif
