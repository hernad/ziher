/*
 * DBFCDX RDD (ver.2)
 *
 * Copyright 1999-2002 Bruno Cantero <bruno@issnet.net>
 * Copyright 2000-2003 Horacio Roldan <ziher_ar@yahoo.com.ar> (portions)
 * Copyright 2003 Przemyslaw Czerpak <druzus@priv.onet.pl> - all code except
 * zh_cdxTagDoIndex and related zh_cdxSort* rewritten.
 * Copyright 2004 Przemyslaw Czerpak <druzus@priv.onet.pl> - rest of code rewritten
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

#define ZH_CDX_NEW_SORT

#if ! defined( ZH_SIXCDX )
#  define ZH_CDX_PACKTRAIL
#endif

#define ZH_CDX_DBGCODE
/*
#define ZH_CDX_DBGCODE_EXT
#define ZH_CDX_DSPDBG_INFO
#define ZH_CDX_DBGTIME
#define ZH_CDX_DBGUPDT
*/

#include "zh_api.h"
#include "zh_item_api.h"
#include "zh_init.h"
#include "zh_api_error.h"
#include "zh_lang_api.h"
#include "zh_vm.h"
#include "zh_set.h"
#include "zh_stack.h"
#include "zh_rdd_cdx.h"
#include "zh_math.h"
#include "zh_rtl/string/zh_regex.h"
#include "zh_codepage_api.h"
#include "zh_rtl/rdd_sys.zhh"

#define zh_cdxFilePageOffset( I, B )      ( ( ZH_FOFFSET ) ( B ) << ( ( I )->fLargeFile ? ( I )->uiPageBits : 0 ) )
#define zh_cdxFilePageNum( I, O )         ( ( ZH_ULONG ) ( ( O ) >> ( ( I )->fLargeFile ? ( I )->uiPageBits : 0 ) ) )
#define zh_cdxFilePageNext( I, C )        ( ( C ) << ( ( I )->fLargeFile ? 0 : ( I )->uiPageBits ) )
#define zh_cdxFilePageRootValid( I, B )   ( ( I )->fLargeFile ? ( B ) != CDX_DUMMYNODE : ( ( B ) % ( I )->uiPageLen == 0 ) )
#define zh_cdxPageKeyBufPtr( P )          ( ( ( ZH_BYTE * ) &( P )->node ) + ( P )->TagParent->pIndex->uiPageLen )
#define zh_cdxPageIntKeyPool( P )         ( ( ( ZH_BYTE * ) &( P )->node ) + CDX_INT_HEADSIZE )
#define zh_cdxPageExtKeyPool( P )         ( ( ( ZH_BYTE * ) &( P )->node ) + CDX_EXT_HEADSIZE )

/*
 * Tag->fRePos = ZH_TRUE means that rootPage->...->childLeafPage path is
 * bad and has to be reloaded
 * CurKey->rec == 0 means that there is no correct CurKey
 */

/* create a new Tag (make index) */
static void zh_cdxTagDoIndex( LPCDXTAG pTag, ZH_BOOL fReindex );

/* Close Tag */
static void zh_cdxTagClose( LPCDXTAG pTag );

/* free Tag pages from cache */
static void zh_cdxTagPoolFree( LPCDXTAG pTag, int nPagesLeft );

/* Store tag header to index files */
static void zh_cdxTagHeaderStore( LPCDXTAG pTag );

/* write all changed pages in tag cache */
static void zh_cdxTagPoolFlush( LPCDXTAG pTag );

/* Discard all pages in cache (TagClose and TagPoolFree for all Tags) */
static void zh_cdxIndexDiscardBuffers( LPCDXINDEX pIndex );

/* write all changed pages in cache (pagePool and Tag Header) */
static void zh_cdxIndexFlushBuffers( LPCDXINDEX pIndex );

/* free cached pages of index file */
static void zh_cdxIndexPoolFree( LPCDXINDEX pIndex, int nPagesLeft );

/* split Root Page */
static int zh_cdxPageRootSplit( LPCDXPAGE pPage );

/* free create index structure */
static void zh_cdxSortFree( LPCDXSORTINFO pSort );

static ZH_USHORT s_uiRddId = ( ZH_USHORT ) -1;

static RDDFUNCS cdxSuper;


#ifdef ZH_CDX_DSPDBG_INFO
static void zh_cdxDspTags( LPCDXINDEX pIndex )
{
   LPCDXTAG pTag = NULL;

   printf( "\r\n*TAGS*" );
   while( pIndex )
   {
      printf( "\r\nBAG: [%s] ->", pIndex->szFileName );
      pTag = pIndex->TagList;
      while( pTag )
      {
         printf( " {%s}", pTag->szName );
         pTag = pTag->pNext;
      }
      pIndex = pIndex->pNext;
   }
   printf( "\r\n*END*\r\n" ); fflush( stdout );
}
#endif

#ifdef ZH_CDX_DBGTIME
#include <sys/time.h>
typedef ZH_LONGLONG CDXDBGTIME;

static CDXDBGTIME cdxTimeIntBld  = 0;
static CDXDBGTIME cdxTimeExtBld  = 0;
static CDXDBGTIME cdxTimeIntBlc  = 0;
static CDXDBGTIME cdxTimeExtBlc  = 0;
static CDXDBGTIME cdxTimeGetKey  = 0;
static CDXDBGTIME cdxTimeFreeKey = 0;
static CDXDBGTIME cdxTimeIdxBld  = 0;

static CDXDBGTIME zh_cdxGetTime()
{
   struct timeval tv;

   gettimeofday( &tv, NULL );
   return ( CDXDBGTIME ) tv.tv_sec * 1000000 + ( CDXDBGTIME ) tv.tv_usec;
}
#endif
#ifdef ZH_CDX_DBGUPDT
static ZH_ULONG cdxWriteNO      = 0;
static ZH_ULONG cdxReadNO       = 0;
static ZH_SHORT cdxStackSize    = 0;
static ZH_SHORT cdxTmpStackSize = 0;
#endif


/*
 * internal DBFCDX function
 */


/*
 * generate internal error
 */
static void zh_cdxErrInternal( const char * szMsg )
{
   zh_errInternal( 9201, szMsg ? szMsg : "zh_cdxErrInternal: data integrity error.", NULL, NULL );
}

/*
 * generate Run-Time error
 */
static ZH_ERRCODE zh_cdxErrorRT( CDXAREAP pArea,
                                 ZH_ERRCODE errGenCode, ZH_ERRCODE errSubCode,
                                 const char * filename, ZH_ERRCODE errOsCode,
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
      if( filename )
         zh_errPutFileName( pError, filename );
      if( uiFlags )
         zh_errPutFlags( pError, uiFlags );
      iRet = SELF_ERROR( &pArea->dbfarea.area, pError );
      if( ! pErrorPtr )
         zh_errRelease( pError );
   }
   return iRet;
}

/*
 * create index sort table
 */
static void zh_cdxMakeSortTab( CDXAREAP pArea )
{
   if( pArea->dbfarea.area.cdPage &&
       ! ZH_CODEPAGE_ISBINSORT( pArea->dbfarea.area.cdPage ) &&
       ! ( pArea->fSortCDP || pArea->sortTab ) )
   {
      pArea->sortTab = zh_cdpGetSortTab( pArea->dbfarea.area.cdPage );
      if( ! pArea->sortTab )
         pArea->fSortCDP = ZH_TRUE;
   }
}

/*
 * create new index key
 */
static LPCDXKEY zh_cdxKeyNew( ZH_USHORT uiLen )
{
   LPCDXKEY pKey;

   pKey = ( LPCDXKEY ) zh_xgrabz( sizeof( CDXKEY ) + uiLen );
   pKey->len = uiLen;

   return pKey;
}

/*
 * Free index key
 */
static void zh_cdxKeyFree( LPCDXKEY pKey )
{
   zh_xfree( pKey );
}

/*
 * copy index key, if dst is null create new dst key else destroy dst
 */
static LPCDXKEY zh_cdxKeyCopy( LPCDXKEY pKeyDest, LPCDXKEY pKey )
{
   if( ! pKeyDest )
      pKeyDest = zh_cdxKeyNew( pKey->len );
   else if( pKeyDest->len != pKey->len )
      pKeyDest = ( LPCDXKEY ) zh_xrealloc( pKeyDest, sizeof( CDXKEY ) + pKey->len );

   return ( LPCDXKEY ) memcpy( pKeyDest, pKey, sizeof( CDXKEY ) + pKey->len );
}

/*
 * store bytes value in index key
 */
static LPCDXKEY zh_cdxKeyPut( LPCDXKEY pKey, const ZH_BYTE * pbVal, ZH_USHORT uiLen, ZH_ULONG ulRec )
{
   if( pbVal == NULL )
      uiLen = 0;

   if( ! pKey )
      pKey = zh_cdxKeyNew( uiLen );
   else if( pKey->len != uiLen )
   {
      pKey = ( LPCDXKEY ) zh_xrealloc( pKey, sizeof( CDXKEY ) + uiLen );
      pKey->len = uiLen;
   }

   if( uiLen )
      memcpy( pKey->val, pbVal, uiLen );
   pKey->val[ uiLen ] = '\0';

   pKey->mode = CDX_CMP_EXACT;
   pKey->rec = ulRec;

   return pKey;
}

/*
 * store string value in index key
 */
static LPCDXKEY zh_cdxKeyPutCL( LPCDXKEY pKey, const char * pText, ZH_SIZE nLen, ZH_ULONG ulRec, ZH_USHORT uiKeyLen, int iMode )
{
   if( ! pKey )
      pKey = zh_cdxKeyNew( uiKeyLen );
   else if( pKey->len != uiKeyLen )
   {
      pKey = ( LPCDXKEY ) zh_xrealloc( pKey, sizeof( CDXKEY ) + uiKeyLen );
      pKey->len = uiKeyLen;
   }

   if( nLen > ( ZH_SIZE ) uiKeyLen )
      nLen = uiKeyLen;
   else if( nLen < ( ZH_SIZE ) uiKeyLen )
      memset( &pKey->val[ nLen ], ' ', ( ZH_SIZE ) uiKeyLen - nLen );
   if( nLen )
      memcpy( pKey->val, pText, nLen );
   pKey->val[ uiKeyLen ] = '\0';

   pKey->mode = ( ZH_USHORT ) iMode;
   pKey->rec = ulRec;

   return pKey;
}

/*
 * compare two values using Tag conditions (len & type)
 */
static int zh_cdxValCompare( LPCDXTAG pTag, const ZH_BYTE * val1, int len1,
                             const ZH_BYTE * val2, int len2, int iMode )
{
   int iLimit, iResult = 0;

   iLimit = ( len1 > len2 ) ? len2 : len1;

   if( pTag->uiType == 'C' )
   {
      if( iLimit > 0 )
      {
         if( pTag->pIndex->pArea->sortTab )
         {
            const ZH_UCHAR * sortTab = pTag->pIndex->pArea->sortTab;
            int iPos = 0;
            while( iPos < iLimit )
            {
               iResult = sortTab[ val1[ iPos ] ] - sortTab[ val2[ iPos ] ];
               if( iResult != 0 )
                  break;
               iPos++;
            }
         }
         else if( pTag->pIndex->pArea->fSortCDP )
         {
            return -zh_cdpcmp( ( const char * ) val2, ( ZH_SIZE ) len2,
                               ( const char * ) val1, ( ZH_SIZE ) len1,
                               pTag->pIndex->pArea->dbfarea.area.cdPage, 0 );
         }
         else
            iResult = memcmp( val1, val2, iLimit );
      }

      if( iResult == 0 )
      {
         if( len1 > len2 )
            iResult = 1;
         else if( len1 < len2 && iMode == CDX_CMP_EXACT )
            iResult = -1;
      }
      else if( iResult > 0 )
         iResult = 1;
      else
         iResult = -1;
   }
   else if( iMode == CDX_CMP_DATE && iLimit == 8 )
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
 * get CDX key type for given item
 */
static ZH_BYTE zh_cdxItemType( PZH_ITEM pItem )
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
 * convert internal type of key expression to comparable type
 */
static ZH_BYTE zh_cdxItemTypeCmp( ZH_BYTE bType )
{
   return bType == 'T' ? 'D' : bType;
}

/*
 * store Item in index key
 * TODO: uiType check and generate RT error if necessary
 */
static LPCDXKEY zh_cdxKeyPutItem( LPCDXKEY pKey, PZH_ITEM pItem, ZH_ULONG ulRec, LPCDXTAG pTag, int iMode )
{
   ZH_BYTE buf[ CDX_MAXKEY ];
   const ZH_BYTE * ptr;
   ZH_SIZE nLen;
   double d;

   ptr = &buf[ 0 ];

   switch( zh_cdxItemType( pItem ) )
   {
      case 'C':
      {
         ZH_SIZE nDestLen = pTag->uiLen;
         char * pFree = NULL, * pDest;
         const char * pText;
         PZH_CODEPAGE cdpVM = zh_vmCDP();

         pText = zh_itemGetCPtr( pItem );
         nLen = zh_itemGetCLen( pItem );

         if( cdpVM != pTag->pIndex->pArea->dbfarea.area.cdPage )
         {
            if( nDestLen <= sizeof( buf ) )
               pDest = ( char * ) buf;
            else
               pDest = pFree = ( char * ) zh_xgrab( nDestLen );
            zh_cdpnDup2( pText, nLen,
                         pDest, &nDestLen,
                         cdpVM, pTag->pIndex->pArea->dbfarea.area.cdPage );
            pText = pDest;
            nLen = nDestLen;
            nDestLen = pTag->uiLen;
         }

         if( pTag->IgnoreCase )
         {
            if( pText != ( char * ) buf && nDestLen <= sizeof( buf ) )
               pDest = ( char * ) buf;
            else
               pDest = ( char * ) zh_xgrab( nDestLen );

            nLen = zh_cdpnDup2Upper( pTag->pIndex->pArea->dbfarea.area.cdPage,
                                     pText, nLen, pDest, nDestLen );
            pText = pDest;
            if( pDest != ( char * ) buf )
            {
               if( pFree )
                  zh_xfree( pFree );
               pFree = pDest;
            }
         }

         if( iMode != CDX_CMP_EXACT && nLen < nDestLen )
            nDestLen = nLen;
         pKey = zh_cdxKeyPutCL( pKey, pText, nLen, ulRec, ( ZH_USHORT ) nDestLen, iMode );
         if( pFree )
            zh_xfree( pFree );
         return pKey;
      }
      case 'N':
         if( pTag->uiLen == 4 )
         {
            ZH_U32 uiVal = ( ZH_U32 ) zh_itemGetNI( pItem ) + 0x80000000;
            ZH_PUT_BE_UINT32( buf, uiVal );
            nLen = 4;
         }
         else
         {
            d = zh_itemGetND( pItem );
            ZH_DBL2ORD( &d, buf );
            nLen = 8;
         }
         break;
      case 'D':
         d = ( double ) zh_itemGetDL( pItem );
         ZH_DBL2ORD( &d, buf );
         nLen = 8;
         if( iMode == CDX_CMP_PREFIX && pTag->uiType == 'T' )
            iMode = CDX_CMP_DATE;
         break;
      case 'T':
         if( pTag->uiType == 'D' )
            d = ( double ) zh_itemGetDL( pItem );
         else
            d = zh_itemGetTD( pItem );
         ZH_DBL2ORD( &d, buf );
         nLen = 8;
         break;
      case 'L':
         *buf = ( ZH_BYTE ) ( zh_itemGetL( pItem ) ? 'T' : 'F' );
         nLen = 1;
         break;
      default:
         ptr = NULL;
         nLen = 0;
         zh_cdxErrorRT( pTag->pIndex->pArea, EG_DATATYPE, EDBF_INVALIDKEY, NULL, 0, 0, NULL );
         break;
   }

   pKey = zh_cdxKeyPut( pKey, ptr, ( ZH_USHORT ) nLen, ulRec );
   pKey->mode = ( ZH_USHORT ) iMode;

   return pKey;
}

/*
 * get Item from index key
 */
static PZH_ITEM zh_cdxKeyGetItem( LPCDXKEY pKey, PZH_ITEM pItem, LPCDXTAG pTag )
{
   double d;

   if( pKey )
   {
      switch( pTag->uiType )
      {
         case 'C':
         {
            ZH_SIZE nLen = pKey->len;
            char * pszVal = zh_cdpnDup( ( const char * ) pKey->val, &nLen,
                                        pTag->pIndex->pArea->dbfarea.area.cdPage, zh_vmCDP() );
            pItem = zh_itemPutCLPtr( pItem, pszVal, nLen );
            break;
         }
         case 'N':
            if( pKey->len == 4 )
            {
               ZH_I32 iVal = ( ZH_I32 ) ( ZH_GET_BE_UINT32( pKey->val ) ) - 0x80000000;
               pItem = zh_itemPutNI( pItem, iVal );
            }
            else
            {
               ZH_ORD2DBL( pKey->val, &d );
               pItem = zh_itemPutND( pItem, d );
            }
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
static LPCDXKEY zh_cdxKeyEval( LPCDXKEY pKey, LPCDXTAG pTag )
{
   CDXAREAP pArea = pTag->pIndex->pArea;
   PZH_ITEM pItem;
   PZH_CODEPAGE cdpTmp = zh_cdpSelect( pArea->dbfarea.area.cdPage );

   if( pTag->nField )
   {
      pItem = zh_stackReturnItem();
      SELF_GETVALUE( &pArea->dbfarea.area, pTag->nField, pItem );
      pKey = zh_cdxKeyPutItem( pKey, pItem, pArea->dbfarea.ulRecNo, pTag, CDX_CMP_EXACT );
   }
   else
   {
      int iCurrArea = zh_rddGetCurrentWorkAreaNumber();

      if( iCurrArea != pArea->dbfarea.area.uiArea )
         zh_rddSelectWorkAreaNumber( pArea->dbfarea.area.uiArea );
      else
         iCurrArea = 0;

      pItem = zh_vmEvalBlockOrMacro( pTag->pKeyItem );
      pKey = zh_cdxKeyPutItem( pKey, pItem, pArea->dbfarea.ulRecNo, pTag, CDX_CMP_EXACT );

      if( iCurrArea )
         zh_rddSelectWorkAreaNumber( iCurrArea );
   }

   zh_cdpSelect( cdpTmp );

   return pKey;
}

/*
 * evaluate conditional expression and return the result
 */
static ZH_BOOL zh_cdxEvalCond( CDXAREAP pArea, PZH_ITEM pCondItem, ZH_BOOL fSetWA )
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
static ZH_BOOL zh_cdxEvalSeekCond( LPCDXTAG pTag, PZH_ITEM pCondItem )
{
   ZH_BOOL fRet;
   PZH_ITEM pKeyVal, pKeyRec;

   pKeyVal = zh_cdxKeyGetItem( pTag->CurKey, NULL, pTag );
   pKeyRec = zh_itemPutNInt( NULL, pTag->CurKey->rec );

   fRet = zh_itemGetL( zh_vmEvalBlockV( pCondItem, 2, pKeyVal, pKeyRec ) );

   zh_itemRelease( pKeyVal );
   zh_itemRelease( pKeyRec );

   return fRet;
}

/*
 * check if Key is in top scope
 */
static ZH_BOOL zh_cdxTopScope( LPCDXTAG pTag )
{
   LPCDXKEY pKey;

   if( pTag->UsrAscend )
   {
      pKey = pTag->topScopeKey;
      return ! pKey || ! pKey->len ||
             zh_cdxValCompare( pTag, pKey->val, pKey->len,
                               pTag->CurKey->val, pTag->CurKey->len,
                               pKey->mode ) <= 0;
   }
   else
   {
      pKey = pTag->bottomScopeKey;
      return ! pKey || ! pKey->len ||
             zh_cdxValCompare( pTag, pKey->val, pKey->len,
                               pTag->CurKey->val, pTag->CurKey->len,
                               pKey->mode ) >= 0;
   }
}

/*
 * check if Key is in bottom scope
 */
static ZH_BOOL zh_cdxBottomScope( LPCDXTAG pTag )
{
   LPCDXKEY pKey;

   if( pTag->UsrAscend )
   {
      pKey = pTag->bottomScopeKey;
      return ! pKey || ! pKey->len ||
             zh_cdxValCompare( pTag, pKey->val, pKey->len,
                               pTag->CurKey->val, pTag->CurKey->len,
                               pKey->mode ) >= 0;
   }
   else
   {
      pKey = pTag->topScopeKey;
      return ! pKey || ! pKey->len ||
             zh_cdxValCompare( pTag, pKey->val, pKey->len,
                               pTag->CurKey->val, pTag->CurKey->len,
                               pKey->mode ) <= 0;
   }
}

/*
 * clear top or bottom scope
 */
static void zh_cdxTagClearScope( LPCDXTAG pTag, ZH_USHORT nScope )
{
   CDXAREAP pArea = pTag->pIndex->pArea;
   LPCDXKEY * pScopeKey;
   PZH_ITEM * pScope;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_cdxTagClearScope(%p, %hu)", ( void * ) pTag, nScope ) );

   /* resolve any pending scope relations first */
   if( pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped )
      SELF_FORCEREL( &pArea->dbfarea.area );

   if( pTag->UsrAscend ? nScope == 0 : nScope != 0 )
   {
      pScope    = &pTag->topScope;
      pScopeKey = &pTag->topScopeKey;
   }
   else
   {
      pScope    = &pTag->bottomScope;
      pScopeKey = &pTag->bottomScopeKey;
   }
   if( *pScope )
   {
      zh_itemRelease( *pScope );
      *pScope = NULL;
   }
   if( *pScopeKey )
   {
      zh_cdxKeyFree( *pScopeKey );
      *pScopeKey = NULL;
      pTag->curKeyState &= ~( CDX_CURKEY_RAWCNT | CDX_CURKEY_LOGCNT );
      if( nScope == 0 )
         pTag->curKeyState &= ~( CDX_CURKEY_RAWPOS | CDX_CURKEY_LOGPOS );
   }
}

/*
 * set top or bottom scope
 */
static void zh_cdxTagSetScope( LPCDXTAG pTag, ZH_USHORT nScope, PZH_ITEM pItem )
{
   CDXAREAP pArea = pTag->pIndex->pArea;
   PZH_ITEM pScopeVal;

   /* resolve any pending scope relations first */
   if( pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped )
      SELF_FORCEREL( &pArea->dbfarea.area );

   pScopeVal = ( zh_itemType( pItem ) & ZH_IT_BLOCK ) ?
                           zh_vmEvalBlock( pItem ) : pItem;

   if( zh_cdxItemTypeCmp( ( ZH_BYTE ) pTag->uiType ) == zh_cdxItemTypeCmp( zh_cdxItemType( pScopeVal ) ) )
   {
      PZH_ITEM * pScope;
      LPCDXKEY * pScopeKey;
      ZH_ULONG ulRec;

      if( pTag->UsrAscend ? nScope == 0 : nScope != 0 )
      {
         pScope    = &( pTag->topScope );
         pScopeKey = &( pTag->topScopeKey );
         ulRec = CDX_IGNORE_REC_NUM;
      }
      else
      {
         pScope    = &( pTag->bottomScope );
         pScopeKey = &( pTag->bottomScopeKey );
         ulRec = CDX_MAX_REC_NUM;
      }

      if( *pScope == NULL )
         *pScope = zh_itemNew( NULL );
      zh_itemCopy( *pScope, pItem );
      *pScopeKey = zh_cdxKeyPutItem( *pScopeKey, pScopeVal, ulRec, pTag, CDX_CMP_PREFIX );
      pTag->curKeyState &= ~( CDX_CURKEY_RAWCNT | CDX_CURKEY_LOGCNT );
      if( nScope == 0 )
         pTag->curKeyState &= ~( CDX_CURKEY_RAWPOS | CDX_CURKEY_LOGPOS );
   }
   else
   {
      /* TODO: !!!
       * RT error: DBFCDX/1051  Scope Type Mismatch
       * zh_cdxErrorRT
       */
   }
}

static void zh_cdxTagGetScope( LPCDXTAG pTag, ZH_USHORT nScope, PZH_ITEM pItem )
{
   CDXAREAP pArea = pTag->pIndex->pArea;
   PZH_ITEM * pScope;

   /* resolve any pending scoped relations first */
   if( pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped )
      SELF_FORCEREL( &pArea->dbfarea.area );

   pScope = ( pTag->UsrAscend ? nScope == 0 : nScope != 0 ) ?
            &( pTag->topScope ) : &( pTag->bottomScope );
   if( *pScope )
      zh_itemCopy( pItem, *pScope );
   else
      zh_itemClear( pItem );
}

/*
 * refresh top and bottom scope value if set as codeblock
 */
static void zh_cdxTagRefreshScope( LPCDXTAG pTag )
{
   PZH_ITEM pItem;

   if( pTag->pIndex->pArea->dbfarea.lpdbPendingRel &&
       pTag->pIndex->pArea->dbfarea.lpdbPendingRel->isScoped )
      SELF_FORCEREL( &pTag->pIndex->pArea->dbfarea.area );

   if( zh_itemType( pTag->topScope ) & ZH_IT_BLOCK )
   {
      pItem = zh_vmEvalBlock( pTag->topScope );
      pTag->topScopeKey = zh_cdxKeyPutItem( pTag->topScopeKey, pItem,
                                            pTag->topScopeKey->rec,
                                            pTag, CDX_CMP_PREFIX );
   }
   if( zh_itemType( pTag->bottomScope ) & ZH_IT_BLOCK )
   {
      pItem = zh_vmEvalBlock( pTag->bottomScope );
      pTag->bottomScopeKey = zh_cdxKeyPutItem( pTag->bottomScopeKey, pItem,
                                               pTag->bottomScopeKey->rec,
                                               pTag, CDX_CMP_PREFIX );
   }
}

#ifdef ZH_CDX_DBGCODE_EXT
/*
 * check internal integrity of page pool
 */
static void zh_cdxTagPoolCheck( LPCDXTAG pTag )
{
   LPCDXPAGE pPage, pPrevPage;

   pPage = pTag->pagePool;
   pPrevPage = NULL;
   while( pPage )
   {
      if( pPage->pPoolPrev != pPrevPage || pPage->TagParent != pTag )
         zh_cdxErrInternal( "zh_cdxTagPoolCheck: data integrity error." );
      pPrevPage = pPage;
      pPage = pPage->pPoolNext;
   }
}

/*
 * check if the Tag buffers was not changed without write lock
 */
static void zh_cdxTagCheckBuffers( LPCDXTAG pTag )
{
   ZH_BOOL fChanged = ZH_FALSE;

   zh_cdxTagPoolCheck( pTag );
   if( pTag->TagChanged )
      fChanged = ZH_TRUE;
   else
   {
      LPCDXPAGE pPage = pTag->pagePool;
      while( pPage && ! fChanged )
      {
         fChanged = pPage->fChanged;
         pPage = pPage->pPoolNext;
      }
   }
   if( fChanged )
      zh_cdxErrInternal( "zh_cdxTagCheckBuffers: modification without write lock." );
}

/*
 * check if the Index buffers was not changed without write lock
 */
static void zh_cdxIndexCheckBuffers( LPCDXINDEX pIndex )
{
   LPCDXTAG pTag;

   if( pIndex->fChanged || ( pIndex->freeLst && pIndex->freeLst->fStat ) )
      zh_cdxErrInternal( "zh_cdxIndexCheckBuffers: modification without write lock." );

   if( pIndex->pCompound )
      zh_cdxTagCheckBuffers( pIndex->pCompound );
   pTag = pIndex->TagList;
   while( pTag )
   {
      zh_cdxTagCheckBuffers( pTag );
      pTag = pTag->pNext;
   }
}
#endif

/*
 * lock index for flushing data after (exclusive lock)
 */
static void zh_cdxIndexLockFlush( LPCDXINDEX pIndex )
{
   if( ! zh_dbfLockIdxWrite( &pIndex->pArea->dbfarea, pIndex->pFile,
                             &pIndex->lockData ) )
   {
      zh_errInternal( 9109, "zh_cdxIndexLockFlush: flush lock failed.", NULL, NULL );
   }
}

/*
 * get free index page
 */
static ZH_ULONG zh_cdxIndexGetAvailPage( LPCDXINDEX pIndex, ZH_BOOL fHeader )
{
   PZH_FILE pFile = pIndex->pFile;
   ZH_ULONG ulPage;

   if( pIndex->fReadonly )
      zh_errInternal( 9101, "zh_cdxIndexGetAvailPage on readonly database.", NULL, NULL );

   if( pIndex->fShared && ! pIndex->lockWrite )
      zh_errInternal( 9102, "zh_cdxIndexGetAvailPage on not locked index file.", NULL, NULL );

   if( pIndex->freePage != 0 && pIndex->freePage != CDX_DUMMYNODE && ! fHeader )
   {
      ulPage = pIndex->freePage;
      if( pIndex->freeLst != NULL )
      {
         LPCDXLIST pLst = pIndex->freeLst;
         pIndex->freePage = pLst->nextPage;
         pIndex->freeLst = pLst->pNext;
         zh_xfree( pLst );
      }
      else
      {
         ZH_BYTE byBuf[ 4 ];
         if( zh_fileReadAt( pFile, byBuf, 4, zh_cdxFilePageOffset( pIndex, ulPage ) ) != 4 )
            zh_errInternal( EDBF_READ, "zh_cdxIndexGetAvailPage: Read index page failed.", NULL, NULL );
#ifdef ZH_CDX_DBGUPDT
         cdxReadNO++;
#endif
         pIndex->freePage = ZH_GET_LE_UINT32( byBuf );
      }
   }
   else
   {
      ZH_SIZE nSize = fHeader ? pIndex->uiHeaderLen : pIndex->uiPageLen, nLen = 0;

      if( pIndex->nextAvail == CDX_DUMMYNODE )
         pIndex->nextAvail = zh_cdxFilePageNum( pIndex, zh_fileSize( pFile ) );

      ulPage = pIndex->nextAvail;
      do
      {
         pIndex->nextAvail += zh_cdxFilePageNext( pIndex, 1 );
         nLen += pIndex->uiPageLen;
      }
      while( nLen < nSize );

      /* TODO: ### */
      if( fHeader )
      {
         ZH_BYTE * byPageBuf;

         if( nSize < ( ZH_SIZE ) pIndex->uiPageLen )
            nSize = pIndex->uiPageLen;
         byPageBuf = ( ZH_BYTE * ) zh_xgrabz( nSize );

         zh_cdxIndexLockFlush( pIndex );
         if( zh_fileWriteAt( pFile, byPageBuf, nSize,
                             zh_cdxFilePageOffset( pIndex, ulPage ) ) != nSize )
            zh_errInternal( EDBF_WRITE, "Write in index page failed.", NULL, NULL );
#ifdef ZH_CDX_DBGUPDT
         cdxWriteNO++;
#endif
         pIndex->fChanged = ZH_TRUE;
         zh_xfree( byPageBuf );
      }
   }
   return ulPage;
}

/*
 * free index page
 */
static void zh_cdxIndexPutAvailPage( LPCDXINDEX pIndex, ZH_ULONG ulPage, ZH_BOOL fHeader )
{
   if( ulPage != 0 && ulPage != CDX_DUMMYNODE )
   {
      ZH_SIZE nSize = fHeader ? pIndex->uiHeaderLen : pIndex->uiPageLen, nLen = 0;
      LPCDXLIST pLst;

      if( pIndex->fReadonly )
         zh_errInternal( 9101, "zh_cdxIndexPutAvailPage on readonly database.", NULL, NULL );
      if( pIndex->fShared && ! pIndex->lockWrite )
         zh_errInternal( 9102, "zh_cdxIndexPutAvailPage on not locked index file.", NULL, NULL );

      do
      {
         pLst = ( LPCDXLIST ) zh_xgrab( sizeof( CDXLIST ) );
         pLst->nextPage = pIndex->freePage;
         pIndex->freePage = ulPage;
         pLst->fStat = ZH_TRUE;
         pLst->pNext = pIndex->freeLst;
         pIndex->freeLst = pLst;
         ulPage += zh_cdxFilePageNext( pIndex, 1 );
         nLen += pIndex->uiPageLen;
      }
      while( nLen < nSize );
   }
}

/*
 * flush list of free pages into index file
 */
static void zh_cdxIndexFlushAvailPage( LPCDXINDEX pIndex )
{
   LPCDXLIST pLst = pIndex->freeLst;
   ZH_ULONG ulPage;

   if( pIndex->fReadonly )
      zh_errInternal( 9101, "zh_cdxIndexFlushAvailPage on readonly database.", NULL, NULL );
   if( pIndex->fShared && ! pIndex->lockWrite )
      zh_errInternal( 9102, "zh_cdxIndexFlushAvailPage on not locked index file.", NULL, NULL );
   zh_cdxIndexLockFlush( pIndex );

   ulPage = pIndex->freePage;
   if( pLst && pLst->fStat )
   {
      ZH_BYTE * byPageBuf = ( ZH_BYTE * ) zh_xgrabz( pIndex->uiPageLen );

      do
      {
         ZH_PUT_LE_UINT32( byPageBuf, pLst->nextPage );
         if( zh_fileWriteAt( pIndex->pFile, byPageBuf, pIndex->uiPageLen,
                             zh_cdxFilePageOffset( pIndex, ulPage ) ) !=
             ( ZH_SIZE ) pIndex->uiPageLen )
            zh_errInternal( EDBF_WRITE, "Write in index page failed.", NULL, NULL );
#ifdef ZH_CDX_DBGUPDT
         cdxWriteNO++;
#endif
         pIndex->fChanged = ZH_TRUE;
         ulPage = pLst->nextPage;
         pLst->fStat = ZH_FALSE;
         pLst = pLst->pNext;
      }
      while( pLst && pLst->fStat );
      zh_xfree( byPageBuf );
   }
}

/*
 * drop list of free pages in index file
 */
static void zh_cdxIndexDropAvailPage( LPCDXINDEX pIndex )
{
   LPCDXLIST pLst;

   while( pIndex->freeLst )
   {
      pLst = pIndex->freeLst->pNext;
      zh_xfree( pIndex->freeLst );
      pIndex->freeLst = pLst;
   }
}

/*
 * write index page
 */
static void zh_cdxIndexPageWrite( LPCDXINDEX pIndex, ZH_ULONG ulPage,
                                  const ZH_BYTE * pBuffer, ZH_SIZE nSize )
{
   if( pIndex->fReadonly )
      zh_errInternal( 9101, "zh_cdxIndexPageWrite on readonly database.", NULL, NULL );
   if( pIndex->fShared && ! pIndex->lockWrite )
      zh_errInternal( 9102, "zh_cdxIndexPageWrite on not locked index file.", NULL, NULL );
   zh_cdxIndexLockFlush( pIndex );

   if( zh_fileWriteAt( pIndex->pFile, pBuffer, nSize,
                       zh_cdxFilePageOffset( pIndex, ulPage ) ) != nSize )
      zh_errInternal( EDBF_WRITE, "Write in index page failed.", NULL, NULL );
   pIndex->fChanged = ZH_TRUE;
#ifdef ZH_CDX_DBGUPDT
   cdxWriteNO++;
#endif
}

/*
 * read index page
 */
static void zh_cdxIndexPageRead( LPCDXINDEX pIndex, ZH_ULONG ulPage,
                                 ZH_BYTE * pBuffer, ZH_SIZE nSize )
{
   if( pIndex->fShared && ! ( pIndex->lockRead || pIndex->lockWrite ) )
      zh_errInternal( 9103, "zh_cdxIndexPageRead on not locked index file.", NULL, NULL );

   if( zh_fileReadAt( pIndex->pFile, pBuffer, nSize,
                      zh_cdxFilePageOffset( pIndex, ulPage ) ) != nSize )
      zh_errInternal( EDBF_READ, "zh_cdxIndexPageRead: Read index page failed.", NULL, NULL );
#ifdef ZH_CDX_DBGUPDT
   cdxReadNO++;
#endif
}

/*
 * check if index was updated by other process and if it was discard buffers
 */
static void zh_cdxIndexCheckVersion( LPCDXINDEX pIndex )
{
   ZH_BYTE byBuf[ 8 ];
   ZH_ULONG ulVer, ulFree;

   if( zh_fileReadAt( pIndex->pFile, byBuf, 8, 0x04 ) != 8 )
   {
      if( pIndex->lockWrite > 0 && zh_fileSize( pIndex->pFile ) == 0 )
         memset( byBuf, 0, 8 );
      else
         zh_errInternal( 2155, "zh_cdxIndexCheckVersion: Read error on index heading page.", NULL, NULL );
   }
#ifdef ZH_CDX_DBGUPDT
   cdxReadNO++;
#endif
   ulFree = ZH_GET_LE_UINT32( &byBuf[ 0 ] );
   ulVer  = ZH_GET_BE_UINT32( &byBuf[ 4 ] );
   if( ! pIndex->fShared )
      pIndex->ulVersion = pIndex->freePage;
   else if( ulVer != pIndex->ulVersion || ulFree != pIndex->freePage )
   {
      pIndex->nextAvail = CDX_DUMMYNODE;
      pIndex->ulVersion = ulVer;
      pIndex->freePage = ulFree;
      zh_cdxIndexDiscardBuffers( pIndex );
   }
   #if 0
   zh_cdxIndexDiscardBuffers( pIndex );  /* TODO: !!! ## remove it it's for test only */
   #endif
}

/*
 * lock index for reading (shared lock)
 */
static ZH_BOOL zh_cdxIndexLockRead( LPCDXINDEX pIndex )
{
   ZH_BOOL ret;

   if( pIndex->lockRead > 0 || pIndex->lockWrite > 0 ||
       ! pIndex->pArea->dbfarea.fShared || ! pIndex->fShared ||
       ZH_DIRTYREAD( &pIndex->pArea->dbfarea ) )
   {
      pIndex->lockRead++;
      return ZH_TRUE;
   }
#ifdef ZH_CDX_DBGCODE
   if( pIndex->lockRead != 0 )
      zh_errInternal( 9105, "zh_cdxIndexLockRead: bad count of locks.", NULL, NULL );

   if( pIndex->WrLck || pIndex->RdLck )
      zh_errInternal( 9107, "zh_cdxIndexLockRead: lock failure (*)", NULL, NULL );
   pIndex->RdLck = ZH_TRUE;
#endif

   ret = zh_dbfLockIdxFile( &pIndex->pArea->dbfarea, pIndex->pFile,
                            FL_LOCK | FLX_SHARED | FLX_WAIT, ZH_TRUE,
                            &pIndex->lockData );
   if( ! ret )
      zh_cdxErrorRT( pIndex->pArea, EG_LOCK, EDBF_LOCK, pIndex->szFileName, zh_fsError(), 0, NULL );

   if( ret )
   {
      pIndex->lockRead++;
      zh_cdxIndexCheckVersion( pIndex );
   }
   return ret;
}

/*
 * lock index for writing (exclusive lock)
 */
static ZH_BOOL zh_cdxIndexLockWrite( LPCDXINDEX pIndex )
{
   ZH_BOOL ret;

   if( pIndex->fReadonly )
      zh_errInternal( 9101, "zh_cdxIndexLockWrite: readonly index.", NULL, NULL );
   if( pIndex->lockRead )
      zh_errInternal( 9105, "zh_cdxIndexLockWrite: writeLock after readLock.", NULL, NULL );
   if( pIndex->lockWrite > 0 )
   {
      pIndex->lockWrite++;
      return ZH_TRUE;
   }
   if( pIndex->lockWrite != 0 )
      zh_errInternal( 9105, "zh_cdxIndexLockWrite: bad count of locks.", NULL, NULL );

   if( ! pIndex->pArea->dbfarea.fShared || ! pIndex->fShared )
      ret = ZH_TRUE;
   else
   {
#ifdef ZH_CDX_DBGCODE
      if( pIndex->WrLck || pIndex->RdLck )
         zh_errInternal( 9107, "zh_cdxIndexLockWrite: lock failure (*)", NULL, NULL );
      pIndex->WrLck = ZH_TRUE;
#endif
      ret = zh_dbfLockIdxFile( &pIndex->pArea->dbfarea, pIndex->pFile,
                               FL_LOCK | FLX_EXCLUSIVE | FLX_WAIT, ZH_TRUE,
                               &pIndex->lockData );
   }
   if( ! ret )
      zh_cdxErrorRT( pIndex->pArea, EG_LOCK, EDBF_LOCK, pIndex->szFileName, zh_fsError(), 0, NULL );

   if( ret )
   {
      pIndex->lockWrite++;
      if( pIndex->fShared || pIndex->nextAvail == CDX_DUMMYNODE )
         zh_cdxIndexCheckVersion( pIndex );
   }
   return ret;
}

/*
 * remove index read lock (shared lock)
 */
static ZH_BOOL zh_cdxIndexUnLockRead( LPCDXINDEX pIndex )
{
   pIndex->lockRead--;
   if( pIndex->lockRead < 0 )
   {
      zh_errInternal( 9106, "zh_cdxIndexUnLockRead: bad count of locks.", NULL, NULL );
   }
   if( pIndex->lockRead || pIndex->lockWrite )
   {
      return ZH_TRUE;
   }
#ifdef ZH_CDX_DBGCODE_EXT
   zh_cdxIndexCheckBuffers( pIndex );
#endif

   zh_cdxIndexPoolFree( pIndex, CDX_PAGECACHESIZE );

   if( pIndex->pArea->dbfarea.fShared && pIndex->fShared &&
       ! ZH_DIRTYREAD( &pIndex->pArea->dbfarea ) )
   {
#ifdef ZH_CDX_DBGCODE
      if( pIndex->WrLck || ! pIndex->RdLck )
         zh_errInternal( 9108, "zh_cdxIndexUnLockRead: unlock error (*)", NULL, NULL );
      pIndex->RdLck = ZH_FALSE;
#endif
      if( ! zh_dbfLockIdxFile( &pIndex->pArea->dbfarea, pIndex->pFile, FL_UNLOCK,
                               ZH_TRUE, &pIndex->lockData ) )
      {
         zh_errInternal( 9108, "zh_cdxIndexUnLockRead: unlock error.", NULL, NULL );
      }
   }
   return ZH_TRUE;
}

/*
 * remove index write lock (exclusive lock)
 */
static ZH_BOOL zh_cdxIndexUnLockWrite( LPCDXINDEX pIndex )
{
   if( pIndex->lockWrite > 1 )
   {
      pIndex->lockWrite--;
      return ZH_TRUE;
   }

   if( pIndex->lockWrite < 1 )
   {
      zh_errInternal( 9106, "zh_cdxIndexUnLockWrite: bad count of locks.", NULL, NULL );
   }
   if( pIndex->lockRead )
   {
      zh_errInternal( 9105, "zh_cdxIndexUnLockWrite: writeUnLock before readUnLock.", NULL, NULL );
   }

   zh_cdxIndexFlushBuffers( pIndex );
   zh_cdxIndexPoolFree( pIndex, CDX_PAGECACHESIZE );

   pIndex->lockWrite--;
   if( pIndex->pArea->dbfarea.fShared && pIndex->fShared )
   {
      if( pIndex->fChanged )
      {
         ZH_BYTE byBuf[ 8 ];
         ( pIndex->ulVersion )++;
         ZH_PUT_LE_UINT32( &byBuf[ 0 ], pIndex->freePage );
         ZH_PUT_BE_UINT32( &byBuf[ 4 ], pIndex->ulVersion );
         if( zh_fileWriteAt( pIndex->pFile, byBuf, 8, 0x04 ) != 8 )
         {
            zh_errInternal( EDBF_WRITE, "Write in index page failed (ver)", NULL, NULL );
         }
         pIndex->fFlush = ZH_TRUE;
         pIndex->fChanged = ZH_FALSE;
      }
      zh_fileFlush( pIndex->pFile, ZH_TRUE );
#ifdef ZH_CDX_DBGCODE
      if( ! pIndex->WrLck || pIndex->RdLck )
         zh_errInternal( 9108, "zh_cdxIndexUnLockWrite: unlock error (*)", NULL, NULL );
      pIndex->WrLck = ZH_FALSE;
#endif
      if( ! zh_dbfLockIdxFile( &pIndex->pArea->dbfarea, pIndex->pFile, FL_UNLOCK,
                               ZH_TRUE, &pIndex->lockData ) )
      {
         zh_errInternal( 9108, "zh_cdxIndexUnLockWrite: unlock error.", NULL, NULL );
      }
   }
   else
   {
      if( pIndex->ulVersion != pIndex->freePage )
      {
         ZH_BYTE byBuf[ 4 ];
         ZH_PUT_LE_UINT32( &byBuf[ 0 ], pIndex->freePage );
         if( zh_fileWriteAt( pIndex->pFile, byBuf, 4, 0x04 ) != 4 )
         {
            zh_errInternal( EDBF_WRITE, "Write in index page failed (ver.ex)", NULL, NULL );
         }
         pIndex->ulVersion = pIndex->freePage;
         pIndex->fFlush = ZH_TRUE;
#ifdef ZH_CDX_DBGUPDT
         cdxWriteNO++;
#endif
      }
      else if( pIndex->fChanged )
      {
         pIndex->fFlush = ZH_TRUE;
      }
      pIndex->fChanged = ZH_FALSE;
   }
   return ZH_TRUE;
}

/*
 * discard all pages in cache (TagClose and TagPoolFree for all Tags)
 */
static void zh_cdxIndexDiscardBuffers( LPCDXINDEX pIndex )
{
   LPCDXTAG pTag;

#ifdef ZH_CDX_DBGCODE_EXT
   zh_cdxIndexCheckBuffers( pIndex );
#endif

   zh_cdxIndexDropAvailPage( pIndex );
   if( pIndex->pCompound )
   {
      zh_cdxTagClose( pIndex->pCompound );
      zh_cdxTagPoolFree( pIndex->pCompound, 0 );
      pIndex->pCompound->fRePos = ZH_TRUE;
      pIndex->pCompound->curKeyState = 0;
      if( pIndex->pCompound->CurKey )
         pIndex->pCompound->CurKey->rec = 0;
   }
   pTag = pIndex->TagList;
   while( pTag )
   {
      zh_cdxTagClose( pTag );
      zh_cdxTagPoolFree( pTag, 0 );
      pTag->fRePos = ZH_TRUE;
      pTag->curKeyState = 0;
      if( pTag->CurKey && ! pTag->Custom )
         pTag->CurKey->rec = 0;
      pTag = pTag->pNext;
   }
   zh_fileFlush( pIndex->pFile, ZH_FALSE );
}

/*
 * write all changed pages in cache (pagePool, pages in Tags and Tag Header)
 */
static void zh_cdxIndexFlushBuffers( LPCDXINDEX pIndex )
{
   LPCDXTAG pTag;

   if( pIndex->pCompound )
   {
      zh_cdxTagPoolFlush( pIndex->pCompound );
      if( pIndex->pCompound->TagChanged )
         zh_cdxTagHeaderStore( pIndex->pCompound );
   }
   pTag = pIndex->TagList;
   while( pTag )
   {
      zh_cdxTagPoolFlush( pTag );
      if( pTag->TagChanged )
         zh_cdxTagHeaderStore( pTag );
      pTag = pTag->pNext;
   }
   zh_cdxIndexFlushAvailPage( pIndex );
}

/*
 * free cached pages of index file
 */
static void zh_cdxIndexPoolFree( LPCDXINDEX pIndex, int nPagesLeft )
{
   LPCDXTAG pTag;

   if( pIndex->pCompound )
   {
      zh_cdxTagPoolFree( pIndex->pCompound, nPagesLeft );
   }
   pTag = pIndex->TagList;
   while( pTag )
   {
      zh_cdxTagPoolFree( pTag, nPagesLeft );
      pTag = pTag->pNext;
   }
}

/*
 * get key value ptr from index page
 */
static ZH_BYTE * zh_cdxPageGetKeyVal( LPCDXPAGE pPage, int iKey )
{
#ifdef ZH_CDX_DBGCODE
   if( iKey < 0 || iKey >= pPage->iKeys )
      zh_cdxErrInternal( "zh_cdxPageGetKeyVal: wrong iKey index." );
#endif
   if( pPage->pKeyBuf )
      return &pPage->pKeyBuf[ iKey * ( pPage->TagParent->uiLen + 8 ) ];
   else if( pPage->PageType & CDX_NODE_LEAF )
   {
      int iLen;
      ZH_BYTE bTrail, * pKeyVal;

      pKeyVal = zh_cdxPageKeyBufPtr( pPage );
      iLen = pPage->TagParent->uiLen;
      bTrail = pPage->TagParent->bTrail;
      if( iKey < pPage->bufKeyNum - 1 )
         pPage->bufKeyNum = 0;
      if( pPage->bufKeyNum == 0 )
      {
         pPage->bufKeyPos = ( ZH_SHORT ) ( pPage->TagParent->pIndex->uiPageLen - CDX_EXT_HEADSIZE );
         pPage->bufKeyLen = ( ZH_SHORT ) iLen;
      }
      while( pPage->bufKeyNum <= iKey )
      {
         int iPos, iTmp, iTrl, iDup;

         iPos = pPage->bufKeyNum * pPage->ReqByte;
         iTmp = ZH_GET_LE_UINT32( &zh_cdxPageExtKeyPool( pPage )[ iPos + pPage->ReqByte - 4 ] ) >>
                ( 32 - pPage->TCBits - pPage->DCBits );
         iDup = ( pPage->bufKeyNum == 0 ) ? 0 : ( iTmp & pPage->DCMask );
         iTrl = ( iTmp >> pPage->DCBits ) & pPage->TCMask;
         if( ( iTmp = iLen - iDup - iTrl ) > 0 )
         {
            pPage->bufKeyPos -= ( ZH_SHORT ) iTmp;
            memcpy( &pKeyVal[ iDup ],
                    &zh_cdxPageExtKeyPool( pPage )[ pPage->bufKeyPos ], iTmp );
         }
#ifdef ZH_CDX_DBGCODE
         else if( iTmp < 0 )
         {
            printf( "\r\npPage->Page=%lx, iLen=%d, iDup=%d, iTrl=%d", pPage->Page, iLen, iDup, iTrl ); fflush( stdout );
            zh_cdxErrInternal( "zh_cdxPageGetKeyVal: index corrupted." );
         }
#endif
         if( iTrl > 0 && ( iTmp = pPage->bufKeyLen - iLen + iTrl ) > 0 )
            memset( &pKeyVal[ iLen - iTrl ], bTrail, iTmp );
         pPage->bufKeyLen = ( ZH_SHORT ) ( iLen - iTrl );
         pPage->bufKeyNum++;
#if 0
         printf( "\r\npPage->Page=%lx, iKey=%d, iLen=%d, iDup=%d, iTrl=%d, ulRec=%d, val[%s]", pPage->Page, pPage->bufKeyNum - 1, iLen, iDup, iTrl, ZH_GET_LE_UINT32( &zh_cdxPageExtKeyPool( pPage )[ iPos ] ), pKeyVal ); fflush( stdout );
#endif
      }
      return pKeyVal;
   }
   else
      return &zh_cdxPageIntKeyPool( pPage )[ iKey * ( pPage->TagParent->uiLen + 8 ) ];
}

/*
 * get record number from index page
 */
static ZH_ULONG zh_cdxPageGetKeyRec( LPCDXPAGE pPage, int iKey )
{
#ifdef ZH_CDX_DBGCODE
   if( iKey < 0 || iKey >= pPage->iKeys )
      zh_cdxErrInternal( "zh_cdxPageGetKeyRec: wrong iKey index." );
#endif
   if( pPage->pKeyBuf )
   {
      ZH_BYTE * ptr = &pPage->pKeyBuf[ ( iKey + 1 ) * ( pPage->TagParent->uiLen + 8 ) - 8 ];
      return ZH_GET_LE_UINT32( ptr );
   }
   else if( pPage->PageType & CDX_NODE_LEAF )
   {
      ZH_BYTE * ptr = &zh_cdxPageExtKeyPool( pPage )[ iKey * pPage->ReqByte ];
      return ZH_GET_LE_UINT32( ptr ) & pPage->RNMask;
   }
   else
   {
      ZH_BYTE * ptr = &zh_cdxPageIntKeyPool( pPage )[ ( iKey + 1 ) * ( pPage->TagParent->uiLen + 8 ) - 8 ];
      return ZH_GET_BE_UINT32( ptr );
   }
}

/*
 * get child page number from interior index page
 */
static ZH_ULONG zh_cdxPageGetKeyPage( LPCDXPAGE pPage, int iKey )
{
   ZH_BYTE * ptr;

#ifdef ZH_CDX_DBGCODE
   if( iKey < 0 || iKey >= pPage->iKeys )
      zh_cdxErrInternal( "zh_cdxPageGetKeyPage: wrong iKey index." );
   if( pPage->PageType & CDX_NODE_LEAF )
      zh_cdxErrInternal( "zh_cdxPageGetKeyPage: page is a leaf." );
#endif
   ptr = &zh_cdxPageIntKeyPool( pPage )[
                        ( iKey + 1 ) * ( pPage->TagParent->uiLen + 8 ) - 4 ];
   return ZH_GET_BE_UINT32( ptr );
}

/*
 * get number of duplicated keys from key in leaf index page
 */
static int zh_cdxPageGetKeyTrl( LPCDXPAGE pPage, int iKey )
{
#ifdef ZH_CDX_DBGCODE_EXT
   if( iKey < 0 || iKey >= pPage->iKeys )
      zh_cdxErrInternal( "zh_cdxPageGetKeyTrl: wrong iKey index." );
   if( ( pPage->PageType & CDX_NODE_LEAF ) == 0 )
      zh_cdxErrInternal( "zh_cdxPageGetKeyTrl: page is not a leaf." );
#endif
   if( pPage->pKeyBuf )
   {
      ZH_BYTE * ptr = &pPage->pKeyBuf[ ( iKey + 1 ) * ( pPage->TagParent->uiLen + 8 ) - 2 ];
      return ZH_GET_LE_UINT16( ptr );
   }
   else
   {
      ZH_BYTE * ptr = &zh_cdxPageExtKeyPool( pPage )[ ( iKey + 1 ) * pPage->ReqByte - 4 ];
      return ( ZH_GET_LE_UINT32( ptr ) >> ( 32 - pPage->TCBits ) ) & pPage->TCMask;
   }
}

#ifdef ZH_CDX_DBGCODE_EXT
/*
 * check if keys are sorted in proper order
 */
static void zh_cdxPageCheckKeys( LPCDXPAGE pPage )
{
   if( pPage->iKeys > 1 )
   {
      int i, K, iLen = pPage->TagParent->uiLen;
      ZH_ULONG ulRec, ulRecPrev;
      ZH_BYTE * pbVal, * pbValPrev = ( ZH_BYTE * ) zh_xgrab( iLen );

      pPage->bufKeyNum = 0;
      pbVal = zh_cdxPageGetKeyVal( pPage, 0 );
      ulRec = zh_cdxPageGetKeyRec( pPage, 0 );
      for( i = 1; i < pPage->iKeys; i++ )
      {
         memcpy( pbValPrev, pbVal, iLen );
         ulRecPrev = ulRec;
         pbVal = zh_cdxPageGetKeyVal( pPage, i );
         ulRec = zh_cdxPageGetKeyRec( pPage, i );
         K = zh_cdxValCompare( pPage->TagParent,
                               pbValPrev, iLen,
                               pbVal, iLen, CDX_CMP_EXACT );
         if( K > 0 || ( K == 0 && ulRecPrev >= ulRec ) )
         {
            if( pPage->PageType & CDX_NODE_LEAF )
               printf( "\r\niFree=%d, ReqByte=%d, RNBits=%d, DCBits=%d, TCBits=%d",
                       pPage->iFree, pPage->ReqByte, pPage->RNBits, pPage->DCBits, pPage->TCBits );
            printf( "\r\nikey=%d, pPage->iKeys=%d, K=%d, ulRecPrev=%ld, ulRec=%ld",
                    i, pPage->iKeys, K, ulRecPrev, ulRec );
            printf( "\r\npbValPrev=[%s] pbVal=[%s], [%d], pPage->pKeyBuf=%p, pPage->iCurKey=%d",
                    pbValPrev, pbVal, memcmp( pbValPrev, pbVal, iLen ),
                    pPage->pKeyBuf, pPage->iCurKey );
            fflush( stdout );
            zh_cdxErrInternal( "zh_cdxPageCheckKeys: index corrupted." );
         }
      }
      zh_xfree( pbValPrev );
   }
}

/*
 * Check decoded leaf page if all trailing and duplicate characters are set
 */
static void zh_cdxPageCheckDupTrl( LPCDXPAGE pPage, ZH_BYTE * pKeyBuf, int iKeys, ZH_BOOL fSpc )
{
   int iNum = pPage->TagParent->uiLen, iKey, iPos, iDup, iTrl,
       iFree = pPage->TagParent->pIndex->uiPageLen - CDX_EXT_HEADSIZE;
   int iLen = iNum + 8;
   ZH_BYTE bTrail = pPage->TagParent->bTrail;
   ZH_BOOL bErr = ZH_FALSE;

   for( iKey = 0; iKey < iKeys; iKey++ )
   {
      iPos = iKey * iLen;
      iTrl = iDup = 0;
      while( iTrl < iNum && pKeyBuf[ iPos + iNum - iTrl - 1 ] == bTrail )
         ++iTrl;
      if( iKey > 0 )
      {
         int iMax;
#ifdef ZH_CDX_PACKTRAIL
         iMax = iNum - iTrl;
#else
         iMax = ZH_GET_LE_UINT16( &pKeyBuf[ iPos - 2 ] );
         iMax = iNum - ZH_MAX( iMax, iTrl );
#endif
         while( iDup < iMax && pKeyBuf[ iPos + iDup ] ==
                               pKeyBuf[ iPos - iLen + iDup ] )
            ++iDup;
      }
      if( iTrl != ZH_GET_LE_UINT16( &pKeyBuf[ iPos + iNum + 6 ] ) )
      {
         printf( "\r\niTrl=%d, keybuf->iTrl=%d, iKey=%d/%d\r\n", iTrl, ZH_GET_LE_UINT16( &pKeyBuf[ iPos + iNum + 6 ] ), iKey, iKeys );
         fflush( stdout );
         bErr = ZH_TRUE;
      }
      if( iDup != ( iKey == 0 ? 0 : ZH_GET_LE_UINT16( &pKeyBuf[ iPos + iNum + 4 ] ) ) )
      {
         printf( "\r\niDup=%d, keybuf->iDup=%d (iTrl=%d), iKey=%d/%d\r\n", iDup, ZH_GET_LE_UINT16( &pKeyBuf[ iPos + iNum + 4 ] ), iTrl, iKey, iKeys );
         fflush( stdout );
         bErr = ZH_TRUE;
      }
      if( iKey > 0 )
      {
         int K;
         K = zh_cdxValCompare( pPage->TagParent,
                               &pKeyBuf[ iPos - iLen ], iNum,
                               &pKeyBuf[ iPos ], iNum, CDX_CMP_EXACT );
         if( K > 0 || ( K == 0 &&
                        ZH_GET_LE_UINT32( &pKeyBuf[ iPos + iNum - iLen ] ) >=
                        ZH_GET_LE_UINT32( &pKeyBuf[ iPos + iNum ] ) ) )
         {
            printf( "\r\nikey=%d, iKeys=%d, K=%d, ulRecPrev=%ld, ulRec=%ld",
                    iKey, iKeys, K,
                    ( ZH_ULONG ) ZH_GET_LE_UINT32( &pKeyBuf[ iPos + iNum - iLen ] ),
                    ( ZH_ULONG ) ZH_GET_LE_UINT32( &pKeyBuf[ iPos + iNum ] ) );
            printf( "\r\npbValPrev=[%s] pbVal=[%s], [%d], pKeyBuf=%p",
                    &pKeyBuf[ iPos - iLen ], &pKeyBuf[ iPos ],
                    memcmp( &pKeyBuf[ iPos - iLen ], &pKeyBuf[ iPos ], iNum ),
                    pKeyBuf );
            fflush( stdout );
            bErr = ZH_TRUE;
         }
      }
      iFree -= iNum + pPage->ReqByte - iDup - iTrl;
   }
   if( fSpc && ( iFree != pPage->iFree /* || iFree < 0 */ ) )
   {
      printf( "\r\nFreeSpace calculated wrong! iFree=%d, pPage->iFree=%d, ReqByte=%d, RNBits=%d, DCBits=%d, TCBits=%d",
              iFree, pPage->iFree, pPage->ReqByte, pPage->RNBits, pPage->DCBits, pPage->TCBits );
      fflush( stdout );
      bErr = ZH_TRUE;
   }
   if( bErr )
   {
      printf( "\r\nPage=%lx, Page->iFree=%d, iLen=%d\r\n", pPage->Page, pPage->iFree, iNum );
      fflush( stdout );
      zh_cdxErrInternal( "zh_cdxPageCheckDupTrl: index corrupted." );
   }
}

static void zh_cdxPageLeafDecode( LPCDXPAGE pPage, ZH_BYTE * pKeyBuf );
static void zh_cdxPageCheckDupTrlRaw( LPCDXPAGE pPage )
{
   ZH_BYTE * pKeyBuf = ( ZH_BYTE * ) zh_xgrab( pPage->iKeys * ( pPage->TagParent->uiLen + 8 ) );

   zh_cdxPageLeafDecode( pPage, pKeyBuf );
   zh_cdxPageCheckDupTrl( pPage, pKeyBuf, pPage->iKeys, ZH_TRUE );
   zh_xfree( pKeyBuf );
}

static void zh_cdxChkLeafRecord( const ZH_BYTE * pSrc,
                                 ZH_ULONG ulRec, int iDup, int iTrl,
                                 LPCDXPAGE pPage )
{
   int iTmp = ZH_GET_LE_UINT32( pSrc + pPage->ReqByte - 4 ) >>
              ( 32 - pPage->TCBits - pPage->DCBits );
   ZH_ULONG ulRec2 = ZH_GET_LE_UINT32( pSrc ) & pPage->RNMask;
   int iDup2 = iTmp & pPage->DCMask,
       iTrl2 = ( iTmp >> pPage->DCBits ) & pPage->TCMask;

   if( ulRec != ulRec2 || iDup != iDup2 || iTrl != iTrl2 )
   {
      printf( "\r\nDCBits=%d[%X], TCBits=%d[%X]  ", pPage->DCBits, pPage->DCMask, pPage->TCBits, pPage->TCMask );
      for( iTmp = 0; iTmp < pPage->ReqByte; ++iTmp )
         printf( "%02X ", pSrc[ iTmp ] );
      iTmp = ( ( iTrl << pPage->DCBits ) | iDup ) << ( 24 - pPage->TCBits - pPage->DCBits );
      printf( "  %6X", iTmp );
      printf( "\r\nzh_cdxChkLeafRecord: ReqByte=%d, ulRec[%lu=>%lu], iDup[%d=>%d], iTrl[%d=>%d]\r\n",
              pPage->ReqByte, ulRec, ulRec2, iDup, iDup2, iTrl, iTrl2 );
      fflush( stdout );
      zh_cdxErrInternal( "zh_cdxChkLeafRecord: wrong leaf record." );
   }
}
#endif

/*
 * put record and duplicate + trailing counters into leaf page
 */
static void zh_cdxSetLeafRecord( ZH_BYTE * pDst,
                                 ZH_ULONG ulRec, int iDup, int iTrl,
                                 int iReq, int iDCbits, int iTCbits )
{
   int i, iBits, iFrom;

   iFrom = ( iTCbits + iDCbits + 7 ) >> 3;
   iBits = ( ( iTrl << iDCbits ) | iDup ) << ( ( iFrom << 3 ) - iTCbits - iDCbits );
   iFrom = iReq - iFrom;
   for( i = 0; i < iReq; i++, ulRec >>= 8 )
   {
      pDst[ i ] = ( ZH_BYTE ) ( ulRec & 0xff );
      if( i >= iFrom )
      {
         pDst[ i ] |= ( ZH_BYTE ) ( iBits & 0xff );
         iBits >>= 8;
      }
   }
}

/*
 * encode keys in buffer into cdx leaf node
 */
static void zh_cdxPageLeafEncode( LPCDXPAGE pPage, ZH_BYTE * pKeyBuf, int iKeys )
{
   int iKey, iReq, iNum, iLen;
   ZH_BYTE * pKeyPos, * pRecPos, * pSrc;

#ifdef ZH_CDX_DBGCODE
   if( ( pPage->PageType & CDX_NODE_LEAF ) == 0 )
   {
      printf( "\r\npPage->Page=%lx. left=%lx, right=%lx",
              pPage->Page, pPage->Left, pPage->Right ); fflush( stdout );
      zh_cdxErrInternal( "zh_cdxPageLeafEncode: page is not a leaf." );
   }
#endif
#ifdef ZH_CDX_DBGCODE_EXT
   if( ! pKeyBuf )
      zh_cdxErrInternal( "zh_cdxPageLeafEncode: page has no buffer." );
   zh_cdxPageCheckDupTrl( pPage, pKeyBuf, iKeys, ZH_TRUE );
#endif
   iNum = pPage->TagParent->uiLen;
   iLen = iNum + 8;
   iReq = pPage->ReqByte;
   pRecPos = zh_cdxPageExtKeyPool( pPage );
   pKeyPos = ( ZH_BYTE * ) &pPage->node.extNode + pPage->TagParent->pIndex->uiPageLen;
   pSrc = &pKeyBuf[ 0 ];
   for( iKey = 0; iKey < iKeys; iKey++, pSrc += iLen, pRecPos += iReq )
   {
      int iTrl, iDup, iTmp;
      ZH_ULONG ulRec;

      ulRec = ZH_GET_LE_UINT32( &pSrc[ iNum ] );
      iDup = ZH_GET_LE_UINT16( &pSrc[ iNum + 4 ] );
      iTrl = ZH_GET_LE_UINT16( &pSrc[ iNum + 6 ] );
      iTmp = iNum - iTrl - iDup;
#if 0
      printf( "\r\nKEY=%d, REC=%ld, DUP=%d, TRL=%d, VAL[%s]", iKey, ulRec, iDup, iTrl, pSrc );
#endif
      zh_cdxSetLeafRecord( pRecPos, ulRec, iDup, iTrl,
                           iReq, pPage->DCBits, pPage->TCBits );
#ifdef ZH_CDX_DBGCODE_EXT
      zh_cdxChkLeafRecord( pRecPos, ulRec, iDup, iTrl, pPage );
#endif
      if( iTmp > 0 )
      {
         pKeyPos -= iTmp;
         memcpy( pKeyPos, &pSrc[ iDup ], iTmp );
      }
#ifdef ZH_CDX_DBGCODE
      else if( iTmp < 0 )
      {
         printf( "\r\n[%s][%s]", pSrc - iLen, pSrc );
         printf( "\r\npPage->Page=0x%lx, iKey=%d, iNum=%d, iDup=%d, iTrl=%d", pPage->Page, iKey, iNum, iDup, iTrl ); fflush( stdout );
         zh_cdxErrInternal( "zh_cdxPageLeafEncode: index corrupted." );
      }
#endif
   }
   if( pRecPos < pKeyPos )
      memset( pRecPos, 0, pKeyPos - pRecPos );
#ifdef ZH_CDX_DBGCODE
   if( pKeyPos - pRecPos != pPage->iFree )
   {
      printf( "\r\nPage=0x%lx, calc=%d, iFree=%d, req=%u, keys=%d, keyLen=%d\r\n",
              pPage->Page, ( int ) ( pKeyPos - pRecPos ), pPage->iFree, pPage->ReqByte, iKeys, iNum );
      fflush( stdout );
      zh_cdxErrInternal( "zh_cdxPageLeafEncode: FreeSpace calculated wrong!" );
   }
   if( pPage->iFree < 0 )
      zh_cdxErrInternal( "zh_cdxPageLeafEncode: FreeSpace calculated wrong!!" );
#endif
   pPage->iKeys = iKeys;
   pPage->fChanged = ZH_TRUE;
   pPage->bufKeyNum = 0;
#ifdef ZH_CDX_DBGCODE_EXT
   {
      ZH_BYTE * pKeyBf = pPage->pKeyBuf;
      pPage->pKeyBuf = NULL;
#if 0
      printf( "\r\nzh_cdxPageLeafEncode: check keys" );
      if( iKeys > 0 ) { pPage->bufKeyNum = 0; zh_cdxPageGetKeyVal( pPage, iKeys - 1 ); }
#endif
      zh_cdxPageCheckKeys( pPage );
      pPage->pKeyBuf = pKeyBf;
   }
   zh_cdxPageCheckKeys( pPage );
   zh_cdxPageCheckDupTrl( pPage, pKeyBuf, pPage->iKeys, ZH_TRUE );
#endif
}

/*
 * decode keys in page into buffer
 */
static void zh_cdxPageLeafDecode( LPCDXPAGE pPage, ZH_BYTE * pKeyBuf )
{
   int iKey, iBits, iReq, iLen = pPage->TagParent->uiLen;
   ZH_BYTE * pDst, * pSrc, * pRec, bTrail = pPage->TagParent->bTrail;

#ifdef ZH_CDX_DBGCODE
   if( ( pPage->PageType & CDX_NODE_LEAF ) == 0 )
   {
      printf( "\r\npPage->Page=%lx", pPage->Page ); fflush( stdout );
      zh_cdxErrInternal( "zh_cdxPageLeafDecode: page is not a leaf." );
   }
#endif
   iBits = ( 32 - pPage->TCBits - pPage->DCBits );
   pDst = pKeyBuf;
   pRec = zh_cdxPageExtKeyPool( pPage );
   pSrc = ( ZH_BYTE * ) &pPage->node.extNode + pPage->TagParent->pIndex->uiPageLen;
   iReq = pPage->ReqByte;
   for( iKey = 0; iKey < pPage->iKeys; iKey++, pRec += iReq )
   {
      int iTmp, iDup, iTrl, iNew;
      ZH_ULONG ulRec;
      ZH_BYTE * pTmp;

      pTmp = &pRec[ iReq - 4 ];
      iTmp = ZH_GET_LE_UINT32( pTmp ) >> iBits;
      iDup = ( iKey == 0 ) ? 0 : ( iTmp & pPage->DCMask );
      iTrl = ( iTmp >> pPage->DCBits ) & pPage->TCMask;
      iNew = iLen - iDup - iTrl;
      if( iDup > 0 )
      {
         memcpy( pDst, pDst - iLen - 8, iDup );
         pDst += iDup;
      }
      if( iNew > 0 )
      {
         pSrc -= iNew;
         memcpy( pDst, pSrc, iNew );
         pDst += iNew;
      }
#ifdef ZH_CDX_DBGCODE
      else if( iNew < 0 )
      {
         printf( "\r\npPage->Page=%lx, iLen=%d, iDup=%d, iTrl=%d", pPage->Page, iLen, iDup, iTrl ); fflush( stdout );
         zh_cdxErrInternal( "zh_cdxPageLeafDecode: index corrupted." );
      }
#endif
      if( iTrl > 0 )
      {
         memset( pDst, bTrail, iTrl );
         pDst += iTrl;
      }
      ulRec = ZH_GET_LE_UINT32( pRec ) & pPage->RNMask;
      ZH_PUT_LE_UINT32( pDst, ulRec );
      pDst += 4;
      ZH_PUT_LE_UINT16( pDst, iDup );
      pDst += 2;
      ZH_PUT_LE_UINT16( pDst, iTrl );
      pDst += 2;
   }
#ifdef ZH_CDX_DBGCODE_EXT
   {
      ZH_BOOL fChg = pPage->fChanged;
      zh_cdxPageLeafEncode( pPage, pKeyBuf, pPage->iKeys );
      pPage->fChanged = fChg;
   }
#endif
}

/*
 * init space leaf page
 */
static void zh_cdxPageLeafInitSpace( LPCDXPAGE pPage )
{
   int iLen = pPage->TagParent->uiLen;
   ZH_BYTE bBits;

   for( bBits = 0; iLen; bBits++, iLen >>= 1 )
      ;

   pPage->ReqByte = bBits > 12 ? 5 : ( bBits > 8 ? 4 : 3 );
   pPage->RNBits  = ( pPage->ReqByte << 3 ) - ( bBits << 1 );
   pPage->DCBits  = pPage->TCBits = bBits;
   pPage->DCMask  = pPage->TCMask = ( ZH_USHORT ) ZH_CDXBITMASK( bBits );
   pPage->RNMask  = ZH_CDXBITMASK( pPage->RNBits );
   pPage->iFree   = pPage->TagParent->pIndex->uiPageLen - CDX_EXT_HEADSIZE;
}

/*
 * calculate the size of keys stored in buffer, return
 * the number of keys which can be stored in the page
 */
static void zh_cdxPageCalcLeafSpace( LPCDXPAGE pPage, ZH_BYTE * pKeyBuf, int iKeys )
{
   int iNum = pPage->TagParent->uiLen, iKey, iSize;
   int iLen = iNum + 8;
   ZH_BYTE ReqByte;
   ZH_ULONG ulRec, RNMask;

   zh_cdxPageLeafInitSpace( pPage );
   pPage->iKeys = 0;
   RNMask = pPage->RNMask;
   ReqByte = pPage->ReqByte;
#ifdef ZH_CDX_DBGCODE_EXT
   zh_cdxPageCheckDupTrl( pPage, pKeyBuf, iKeys, ZH_FALSE );
#endif
   /* clear duplicate counter in 1st key */
   ZH_PUT_LE_UINT16( &pKeyBuf[ iNum + 4 ], 0 );
   for( iKey = 0; iKey < iKeys; iKey++ )
   {
      ZH_BYTE * bPtr = &pKeyBuf[ iKey * iLen + iNum ];
      ulRec = ZH_GET_LE_UINT32( bPtr );
      iSize = ReqByte + iNum - ZH_GET_LE_UINT16( &bPtr[ 4 ] ) -
                               ZH_GET_LE_UINT16( &bPtr[ 6 ] );
      if( ulRec > RNMask )
      {
         ZH_BYTE RNBits = pPage->RNBits;
         while( ulRec > RNMask )
         {
            ReqByte++;
            RNBits += 8;
            RNMask = ( RNMask << 8 ) | 0xFF;
            iSize += ( iKey + 1 );
         }
         if( iSize > pPage->iFree )
            break;
#ifdef ZH_CDX_DSPDBG_INFO_X
         printf( "\r\npPage->Page=%lx, ulRec=%lx, RNMask=%lx/%lx, RNBits=%d/%d, DCB=%d, TCB=%d (%lx), iKey=%d/%d",
                 pPage->Page, ulRec, RNMask, pPage->RNMask, RNBits, pPage->RNBits,
                 pPage->DCBits, pPage->TCBits, ZH_CDXBITMASK( RNBits ), iKey, iKeys );
         fflush( stdout );
#endif
         pPage->RNMask = RNMask;
         pPage->RNBits = RNBits;
         pPage->ReqByte = ReqByte;
      }
      else if( iSize > pPage->iFree )
         break;
      pPage->iFree -= ( ZH_SHORT ) iSize;
      pPage->iKeys++;
   }
}

/*
 * remove key from page
 */
static int zh_cdxPageLeafDelKey( LPCDXPAGE pPage )
{
   int iKey = pPage->iCurKey, iLen = pPage->TagParent->uiLen + 8, iSpc;
   int iRet = 0;

#ifdef ZH_CDX_DBGCODE
   if( ( pPage->PageType & CDX_NODE_LEAF ) == 0 )
      zh_cdxErrInternal( "zh_cdxPageLeafDelKey: page is not a leaf." );
   if( iKey < 0 || iKey >= pPage->iKeys )
      zh_cdxErrInternal( "zh_cdxPageLeafDelKey: wrong iKey index." );
#endif
   if( ! pPage->pKeyBuf )
   {
      ZH_BYTE * pKeyBuf = ( ZH_BYTE * ) zh_xgrab( ( pPage->iKeys ) * iLen );
      zh_cdxPageLeafDecode( pPage, pKeyBuf );
      pPage->pKeyBuf = pKeyBuf;
   }
#ifdef ZH_CDX_DSPDBG_INFO
   printf( "\r\ndelkey: Page=%lx, iKey=%d/%d, rec=%ld, iFree=%d",
           pPage->Page, iKey, pPage->iKeys,
           ( ZH_ULONG ) ZH_GET_LE_UINT32( &pPage->pKeyBuf[ ( iKey + 1 ) * iLen - 8 ] ),
           pPage->iFree );
   fflush( stdout );
#endif
   iSpc = pPage->ReqByte + pPage->TagParent->uiLen -
          ZH_GET_LE_UINT16( &pPage->pKeyBuf[ ( iKey + 1 ) * iLen - 4 ] ) -
          ZH_GET_LE_UINT16( &pPage->pKeyBuf[ ( iKey + 1 ) * iLen - 2 ] );
   if( iKey < pPage->iKeys - 1 )
   {
      int iPos = ( iKey + 2 ) * iLen - 4, iDup = 0, iDupNext;
      iDupNext = ZH_GET_LE_UINT16( &pPage->pKeyBuf[ iPos ] );
      iSpc -= iDupNext;
      if( iKey > 0 )
      {
         int iPrev = ( iKey - 1 ) * iLen, iNext = ( iKey + 1 ) * iLen,
             iNum = pPage->TagParent->uiLen, iTrlNext, iDupCurr;
         iTrlNext = ZH_GET_LE_UINT16( &pPage->pKeyBuf[ iNext + iLen - 2 ] );
         iDupCurr = ZH_GET_LE_UINT16( &pPage->pKeyBuf[ iNext - 4 ] );
#ifdef ZH_CDX_PACKTRAIL
         iNum -= iTrlNext;
#else
         {
            int iTrlPrev = ZH_GET_LE_UINT16( &pPage->pKeyBuf[ iPrev + iLen - 2 ] );
            iNum -= ZH_MAX( iTrlNext, iTrlPrev );
         }
#endif
         iDup = ZH_MIN( iDupNext, iDupCurr );
         if( iDup > iNum )
            iDup = iNum;
         else
         {
            while( iDup < iNum && pPage->pKeyBuf[ iPrev + iDup ] ==
                                  pPage->pKeyBuf[ iNext + iDup ] )
               ++iDup;
         }
#ifdef ZH_CDX_DSPDBG_INFO
         printf( "+%d=%d", iSpc + iDup, pPage->iFree + iSpc + iDup );
         if( iSpc + iDup < 0 )
            printf( " iLen=%d, iDup=%d, iNum=%d pd=%d pt=%d cd=%d ct=%d nd=%d nt=%d",
                    iLen - 8, iDup, iNum,
                    ZH_GET_LE_UINT16( &pPage->pKeyBuf[ iPrev + iLen - 4 ] ),
                    ZH_GET_LE_UINT16( &pPage->pKeyBuf[ iPrev + iLen - 2 ] ),
                    iDupCurr,
                    ZH_GET_LE_UINT16( &pPage->pKeyBuf[ iNext - 2 ] ),
                    iDupNext,
                    iTrlNext );
         fflush( stdout );
#endif
      }
      ZH_PUT_LE_UINT16( &pPage->pKeyBuf[ iPos ], iDup );
      iSpc += iDup;
   }
   pPage->iFree += ( ZH_SHORT ) iSpc;
   if( --pPage->iKeys > iKey )
   {
      memmove( &pPage->pKeyBuf[ iKey * iLen ],
               &pPage->pKeyBuf[ ( iKey + 1 ) * iLen ],
               ( pPage->iKeys - iKey ) * iLen );
   }
   pPage->fBufChanged = pPage->fChanged = ZH_TRUE;
#ifdef ZH_CDX_DBGCODE_EXT
   zh_cdxPageCheckKeys( pPage );
   zh_cdxPageCheckDupTrl( pPage, pPage->pKeyBuf, pPage->iKeys, ZH_TRUE );
#endif
   if( iKey >= pPage->iKeys )
      iRet |= NODE_NEWLASTKEY;
   if( pPage->iKeys == 0 )
      iRet |= NODE_JOIN;
   else if( pPage->iFree < 0 )
      iRet |= NODE_SPLIT;
   if( pPage->iFree >= pPage->ReqByte )
      iRet |= NODE_BALANCE;
   return iRet;
}

/*
 * add key to page at current position
 */
static int zh_cdxPageLeafAddKey( LPCDXPAGE pPage, LPCDXKEY pKey )
{
   int iKey, iNum = pPage->TagParent->uiLen;
   int iLen = iNum + 8, iSpc, iTrl, iDup, iMax, iPos;
   ZH_BYTE  bTrail = pPage->TagParent->bTrail;
   int iRet = 0;

#ifdef ZH_CDX_DSPDBG_INFO
   printf( "\r\naddkey: Page=%lx, iKey=%d/%d, rec=%ld, iFree=%d",
           pPage->Page, pPage->iCurKey, pPage->iKeys, pKey->rec,
           pPage->iFree );
   fflush( stdout );
#endif
#ifdef ZH_CDX_DBGCODE
   if( ( pPage->PageType & CDX_NODE_LEAF ) == 0 )
      zh_cdxErrInternal( "zh_cdxPageLeafAddKey: page is not a leaf." );
   if( pPage->iCurKey < 0 || pPage->iCurKey > pPage->iKeys )
      zh_cdxErrInternal( "zh_cdxPageLeafAddKey: wrong iKey index." );
#endif
   if( ! pPage->pKeyBuf )
   {
      ZH_BYTE * pKeyBuf = ( ZH_BYTE * ) zh_xgrab( ( pPage->iKeys + 1 ) * iLen );
      zh_cdxPageLeafDecode( pPage, pKeyBuf );
      pPage->pKeyBuf = pKeyBuf;
   }
   else
   {
      pPage->pKeyBuf = ( ZH_BYTE * ) zh_xrealloc( pPage->pKeyBuf, ( pPage->iKeys + 1 ) * iLen );
   }

#ifdef ZH_CDX_DBGCODE_EXT
   zh_cdxPageCheckKeys( pPage );
   zh_cdxPageCheckDupTrl( pPage, pPage->pKeyBuf, pPage->iKeys, ZH_TRUE );
#endif

   iTrl = iDup = 0;
   iKey = pPage->iCurKey;
   iPos = iKey * iLen;
   if( iKey < pPage->iKeys )
   {
      if( ! pPage->TagParent->pIndex->pArea->fSortCDP )
         iDup = ZH_GET_LE_UINT16( &pPage->pKeyBuf[ iPos + iNum + 4 ] );
      memmove( &pPage->pKeyBuf[ iPos + iLen ], &pPage->pKeyBuf[ iPos ],
               iLen * ( pPage->iKeys - iKey ) );
   }
   if( pKey->len >= iNum )
      memcpy( &pPage->pKeyBuf[ iPos ], pKey->val, iNum );
   else
   {
      memcpy( &pPage->pKeyBuf[ iPos ], pKey->val, pKey->len );
      memset( &pPage->pKeyBuf[ iPos + pKey->len ], bTrail, iNum - pKey->len );
   }
   ZH_PUT_LE_UINT32( &pPage->pKeyBuf[ iPos + iNum ], pKey->rec );
   while( iTrl < iNum && pPage->pKeyBuf[ iPos + iNum - iTrl - 1 ] == bTrail )
      ++iTrl;
   if( iKey > 0 )
   {
#ifdef ZH_CDX_PACKTRAIL
      iMax = iNum - iTrl;
#else
      iMax = ZH_GET_LE_UINT16( &pPage->pKeyBuf[ iPos - 2 ] );
      iMax = iNum - ZH_MAX( iTrl, iMax );
#endif
      if( iDup > iMax )
         iDup = iMax;
      else
      {
         while( iDup < iMax && pPage->pKeyBuf[ iPos + iDup ] ==
                               pPage->pKeyBuf[ iPos + iDup - iLen ] )
            ++iDup;
      }
   }
   ZH_PUT_LE_UINT16( &pPage->pKeyBuf[ iPos + iNum + 4 ], iDup );
   ZH_PUT_LE_UINT16( &pPage->pKeyBuf[ iPos + iNum + 6 ], iTrl );
   iSpc = pPage->ReqByte + iNum - iTrl - iDup;
   if( iKey < pPage->iKeys )
   {
#ifdef ZH_CDX_PACKTRAIL
      iMax = iNum - ZH_GET_LE_UINT16( &pPage->pKeyBuf[ iPos + iLen + iLen - 2 ] );
#else
      iMax = ZH_GET_LE_UINT16( &pPage->pKeyBuf[ iPos + iLen + iLen - 2 ] );
      iMax = iNum - ZH_MAX( iTrl, iMax );
#endif
      iSpc += ZH_GET_LE_UINT16( &pPage->pKeyBuf[ iPos + iLen + iLen - 4 ] );
      iDup = 0;
      while( iDup < iMax && pPage->pKeyBuf[ iPos + iDup ] ==
                            pPage->pKeyBuf[ iPos + iDup + iLen ] )
         ++iDup;
      ZH_PUT_LE_UINT16( &pPage->pKeyBuf[ iPos + iLen + iLen - 4 ], iDup );
      iSpc -= iDup;
   }
   pPage->iKeys++;
   while( pKey->rec > pPage->RNMask )
   {
      pPage->RNMask = ( pPage->RNMask << 8 ) | 0xFF;
      pPage->ReqByte++;
      pPage->RNBits += 8;
      iSpc += pPage->iKeys;
   }
   pPage->iFree -= ( ZH_SHORT ) iSpc;
   pPage->fBufChanged = pPage->fChanged = ZH_TRUE;
#ifdef ZH_CDX_DBGCODE_EXT
   zh_cdxPageCheckKeys( pPage );
   zh_cdxPageCheckDupTrl( pPage, pPage->pKeyBuf, pPage->iKeys, ZH_TRUE );
#endif
   if( iKey >= pPage->iKeys - 1 )
      iRet |= NODE_NEWLASTKEY;
   if( pPage->iFree < 0 )
      iRet |= NODE_SPLIT;
   if( pPage->iFree >= pPage->ReqByte &&
       pPage->Left != CDX_DUMMYNODE && pPage->Right != CDX_DUMMYNODE )
      iRet |= NODE_BALANCE;
   return iRet;
}

/*
 * set (insert) key in interior node record to (with) given value
 */
static void zh_cdxPageIntSetKey( LPCDXPAGE pPage, int iKey, ZH_BOOL fIns, ZH_BYTE * pbVal, ZH_ULONG ulRec, ZH_ULONG ulPag )
{
   int iLen = pPage->TagParent->uiLen;
   int iPos = iKey * ( iLen + 8 );
   ZH_BYTE * pKeyPool = zh_cdxPageIntKeyPool( pPage );

#ifdef ZH_CDX_DSPDBG_INFO
   printf( "\r\nintSetKey (%s): Page=%lx, iKey=%d/%d, ulPag=%lx",
           fIns ? "ins" : "set", pPage->Page, iKey, pPage->iKeys, ulPag );
   fflush( stdout );
#endif
#ifdef ZH_CDX_DBGCODE
   if( ( pPage->PageType & CDX_NODE_LEAF ) != 0 )
      zh_cdxErrInternal( "zh_cdxPageIntSetKey: page is a leaf!" );
   if( iKey < 0 || iKey >= pPage->iKeys + ( fIns ? 1 : 0 ) )
   {
      zh_cdxErrInternal( "zh_cdxPageIntSetKey: wrong iKey index." );
   }
#endif
   if( fIns )
   {
      if( iKey < pPage->iKeys )
      {
         memmove( &pKeyPool[ iPos + iLen + 8 ],
                  &pKeyPool[ iPos ],
                  ( iLen + 8 ) * ( pPage->iKeys - iKey ) );
      }
      pPage->iKeys++;
   }
   if( pbVal )
      memcpy( &pKeyPool[ iPos ], pbVal, iLen );
   else if( fIns )
      memset( &pKeyPool[ iPos ],
              pPage->TagParent->bTrail, iLen );
   if( ulRec )
      ZH_PUT_BE_UINT32( &pKeyPool[ iPos + iLen ], ulRec );
   ZH_PUT_BE_UINT32( &pKeyPool[ iPos + iLen + 4 ], ulPag );
   pPage->fChanged = ZH_TRUE;
}

/*
 * delete key in interior node record
 */
static void zh_cdxPageIntDelKey( LPCDXPAGE pPage, int iKey )
{
   int iLen = pPage->TagParent->uiLen + 8;
   ZH_BYTE * pKeyPool = zh_cdxPageIntKeyPool( pPage );

#ifdef ZH_CDX_DSPDBG_INFO
   printf( "\r\nintDelKey: Page=%lx, iKey=%d/%d, ulPag=%lx",
           pPage->Page, iKey, pPage->iKeys,
           ( ZH_ULONG ) ZH_GET_BE_UINT32( &pKeyPool[ ( iKey + 1 ) * iLen - 4 ] ) );
   fflush( stdout );
#endif
#ifdef ZH_CDX_DBGCODE
   if( ( pPage->PageType & CDX_NODE_LEAF ) != 0 )
      zh_cdxErrInternal( "zh_cdxPageIntDelKey: page is a leaf!" );
   if( iKey < 0 || iKey >= pPage->iKeys )
   {
      zh_cdxErrInternal( "zh_cdxPageIntDelKey: wrong iKey index." );
   }
#endif
   pPage->iKeys--;
   if( pPage->iKeys > iKey )
      memmove( &pKeyPool[ iKey * iLen ],
               &pKeyPool[ ( iKey + 1 ) * iLen ], ( pPage->iKeys - iKey ) * iLen );
   memset( &pKeyPool[ pPage->iKeys * iLen ], 0, iLen );
   pPage->fChanged = ZH_TRUE;
}

/*
 * (re)load CDX page from index file
 */
static void zh_cdxPageLoad( LPCDXPAGE pPage )
{
   if( pPage->pKeyBuf )
   {
      zh_xfree( pPage->pKeyBuf );
      pPage->pKeyBuf = NULL;
      pPage->fBufChanged = ZH_FALSE;
   }
   zh_cdxIndexPageRead( pPage->TagParent->pIndex, pPage->Page,
                        ( ZH_BYTE * ) &pPage->node,
                        pPage->TagParent->pIndex->uiPageLen );
   pPage->PageType = ( ZH_BYTE ) ZH_GET_LE_UINT16( pPage->node.intNode.attr );
   pPage->Left = ZH_GET_LE_UINT32( pPage->node.intNode.leftPtr );
   pPage->Right = ZH_GET_LE_UINT32( pPage->node.intNode.rightPtr );
   pPage->iKeys = ZH_GET_LE_UINT16( pPage->node.intNode.nKeys );
   pPage->fChanged  = ZH_FALSE;

   if( ( pPage->PageType & CDX_NODE_LEAF ) != 0 )
   {
      pPage->RNBits     = pPage->node.extNode.recBits;
      pPage->DCBits     = pPage->node.extNode.dupBits;
      pPage->TCBits     = pPage->node.extNode.trlBits;
      if( pPage->DCBits <= 8 )
      {
         pPage->DCMask = pPage->node.extNode.dupMask;
         pPage->TCMask = pPage->node.extNode.trlMask;
      }
      else
      {
         pPage->DCMask = ( ZH_USHORT ) ZH_CDXBITMASK( pPage->DCBits );
         pPage->TCMask = ( ZH_USHORT ) ZH_CDXBITMASK( pPage->TCBits );
      }
      pPage->RNMask     = ZH_GET_LE_UINT32( pPage->node.extNode.recMask );
      pPage->ReqByte    = pPage->node.extNode.keyBytes;
      pPage->iFree      = ZH_GET_LE_UINT16( pPage->node.extNode.freeSpc );
      pPage->bufKeyNum  = 0;
#if 0
      if( ! pPage->pKeyBuf )
      {
         ZH_BYTE * pKeyBuf = ( ZH_BYTE * ) zh_xgrab( ( pPage->iKeys + 1 ) * ( pPage->TagParent->uiLen + 6 ) );
         zh_cdxPageLeafDecode( pPage, pKeyBuf );
         pPage->pKeyBuf = pKeyBuf;
      }
#endif
   }
#ifdef ZH_CDX_DBGCODE_EXT
   zh_cdxPageCheckKeys( pPage );
#endif
}

/*
 * store page into index file
 */
static void zh_cdxPageStore( LPCDXPAGE pPage )
{
#ifdef ZH_CDX_DBGCODE
   if( pPage->Page == 0 || pPage->Page == CDX_DUMMYNODE )
      zh_cdxErrInternal( "zh_cdxPageStore: Page number wrong!" );
   if( pPage->PageType & CDX_NODE_LEAF )
   {
      if( pPage->iFree < 0 )
         zh_cdxErrInternal( "zh_cdxPageStore: FreeSpace calculated wrong!" );
   }
   else if( pPage->iKeys > pPage->TagParent->MaxKeys )
      zh_cdxErrInternal( "zh_cdxPageStore: number of keys exceed!" );
#endif
   ZH_PUT_LE_UINT16( pPage->node.intNode.attr, ( ZH_U16 ) pPage->PageType );
   ZH_PUT_LE_UINT16( pPage->node.intNode.nKeys, pPage->iKeys );
   ZH_PUT_LE_UINT32( pPage->node.intNode.leftPtr, pPage->Left );
   ZH_PUT_LE_UINT32( pPage->node.intNode.rightPtr, pPage->Right );

   if( ( pPage->PageType & CDX_NODE_LEAF ) != 0 )
   {
      ZH_PUT_LE_UINT16( pPage->node.extNode.freeSpc, pPage->iFree );
      ZH_PUT_LE_UINT32( pPage->node.extNode.recMask, pPage->RNMask );
      if( pPage->DCBits <= 8 )
      {
         pPage->node.extNode.dupMask  = ( ZH_BYTE ) pPage->DCMask;
         pPage->node.extNode.trlMask  = ( ZH_BYTE ) pPage->TCMask;
      }
      else
         ZH_PUT_LE_UINT16( &pPage->node.extNode.dupMask, pPage->DCMask );
      pPage->node.extNode.recBits  = pPage->RNBits;
      pPage->node.extNode.dupBits  = pPage->DCBits;
      pPage->node.extNode.trlBits  = pPage->TCBits;
      pPage->node.extNode.keyBytes = pPage->ReqByte;

      if( pPage->pKeyBuf && pPage->fBufChanged )
      {
         zh_cdxPageLeafEncode( pPage, pPage->pKeyBuf, pPage->iKeys );
         pPage->fBufChanged = ZH_FALSE;
      }
#ifdef ZH_CDX_DBGCODE_EXT
      if( pPage->pKeyBuf )
      {
         zh_xfree( pPage->pKeyBuf );
         pPage->pKeyBuf = NULL;
      }
#endif
   }
   zh_cdxIndexPageWrite( pPage->TagParent->pIndex, pPage->Page,
                         ( const ZH_BYTE * ) &pPage->node,
                         pPage->TagParent->pIndex->uiPageLen );
#ifdef ZH_CDX_DBGCODE_EXT
   zh_cdxPageCheckKeys( pPage );
#endif
   pPage->fChanged = ZH_FALSE;
}

/*
 * create new empty page and allocate space for it in index file if ulPage == 0
 * or load it from index file if ulPage != CDX_DUMMYNODE
 */
static LPCDXPAGE zh_cdxPageNew( LPCDXTAG pTag, LPCDXPAGE pOwnerPage, ZH_ULONG ulPage )
{
   LPCDXPAGE pPage = NULL;

#ifdef ZH_CDX_DBGCODE_EXT
   zh_cdxTagPoolCheck( pTag );
#endif
   if( ulPage && ulPage != CDX_DUMMYNODE && pTag->pagePool )
   {
      pPage = pTag->pagePool;
      while( pPage && pPage->Page != ulPage )
         pPage = pPage->pPoolNext;
   }
   if( pPage )
   {
      if( pPage->pPoolPrev )
      {
         pPage->pPoolPrev->pPoolNext = pPage->pPoolNext;
         if( pPage->pPoolNext )
            pPage->pPoolNext->pPoolPrev = pPage->pPoolPrev;
         pPage->pPoolPrev = NULL;
         pPage->pPoolNext = pTag->pagePool;
         pPage->pPoolNext->pPoolPrev = pPage;
         pTag->pagePool = pPage;
      }
   }
   else
   {
      ZH_SIZE nSize = sizeof( CDXPAGE );

      nSize += pTag->uiLen + 8 + pTag->pIndex->uiPageLen - sizeof( pPage->node );
      pPage = ( LPCDXPAGE ) zh_xgrabz( nSize );

      pPage->PageType = CDX_NODE_UNUSED;
      pPage->Left = pPage->Right = CDX_DUMMYNODE;
      pPage->TagParent = pTag;

      if( ulPage && ulPage != CDX_DUMMYNODE )
      {
         pPage->Page = ulPage;
         zh_cdxPageLoad( pPage );
      }
      else if( ! ulPage )
      {
         pPage->Page = zh_cdxIndexGetAvailPage( pTag->pIndex, ZH_FALSE );
         pPage->fChanged = ZH_TRUE;
      }
      pPage->pPoolPrev = NULL;
      pPage->pPoolNext = pTag->pagePool;
      pTag->pagePool   = pPage;
      if( pPage->pPoolNext )
         pPage->pPoolNext->pPoolPrev = pPage;
   }
   pPage->Owner = pOwnerPage;
   pPage->iCurKey = -1;
   pPage->bUsed = 1;
#ifdef ZH_CDX_DBGCODE_EXT
   zh_cdxTagPoolCheck( pTag );
#endif
   return pPage;
}

/*
 * free single page
 */
static void zh_cdxPageFree( LPCDXPAGE pPage, ZH_BOOL fReal )
{
#ifdef ZH_CDX_DBGCODE_EXT
   LPCDXTAG pTag = pPage->TagParent;
   zh_cdxTagPoolCheck( pTag );
#endif
   if( pPage->Child != NULL )
   {
      zh_cdxPageFree( pPage->Child, fReal );
      pPage->Child = NULL;
   }

   if( pPage->PageType == CDX_NODE_UNUSED )
   {
      fReal = ZH_TRUE;
      pPage->fChanged = ZH_FALSE;
   }

   if( fReal )
   {
      if( pPage->fChanged )
         zh_cdxPageStore( pPage );

#ifdef ZH_CDX_DBGCODE_EXT
      zh_cdxTagPoolCheck( pTag );
#endif
      if( pPage->pPoolPrev )
      {
         pPage->pPoolPrev->pPoolNext = pPage->pPoolNext;
         if( pPage->pPoolNext )
            pPage->pPoolNext->pPoolPrev = pPage->pPoolPrev;
      }
      else
      {
         pPage->TagParent->pagePool = pPage->pPoolNext;
         if( pPage->pPoolNext )
            pPage->pPoolNext->pPoolPrev = NULL;
      }
#ifdef ZH_CDX_DBGCODE_EXT
      zh_cdxTagPoolCheck( pTag );
#endif
   }

   if( pPage->Owner != NULL && pPage->Owner->Child == pPage )
      pPage->Owner->Child = NULL;
   pPage->Owner = NULL;
   pPage->bUsed = 0;

   if( fReal )
   {
      if( pPage->PageType == CDX_NODE_UNUSED )
         zh_cdxIndexPutAvailPage( pPage->TagParent->pIndex, pPage->Page, ZH_FALSE );
      if( pPage->pKeyBuf )
         zh_xfree( pPage->pKeyBuf );
      zh_xfree( pPage );
   }
#ifdef ZH_CDX_DBGCODE_EXT
   zh_cdxTagPoolCheck( pTag );
#endif
}

/*
 * read child page
 */
static void zh_cdxPageGetChild( LPCDXPAGE pPage )
{
   ZH_ULONG ulPage;

#ifdef ZH_CDX_DBGCODE
   if( ( pPage->PageType & CDX_NODE_LEAF ) != 0 )
      zh_cdxErrInternal( "zh_cdxPageGetChild: index corrupted." );
#endif

   ulPage = zh_cdxPageGetKeyPage( pPage, pPage->iCurKey );
   if( pPage->Child != NULL )
   {
      if( pPage->Child->Page != ulPage )
      {
         zh_cdxPageFree( pPage->Child, ZH_FALSE );
         pPage->Child = NULL;
      }
   }
#ifdef ZH_CDX_DSPDBG_INFO
   printf( "\r\nGetChild: Parent=%lx, Child=%lx", pPage->Page, ulPage ); fflush( stdout );
#endif
   if( pPage->Child == NULL )
      pPage->Child = zh_cdxPageNew( pPage->TagParent, pPage, ulPage );
}

static int zh_cdxPageKeyLeafBalance( LPCDXPAGE pPage, int iChildRet )
{
   LPCDXPAGE childs[ CDX_BALANCE_LEAFPAGES + 2 ], lpTmpPage;
   int iChKeys[ CDX_BALANCE_LEAFPAGES + 2 ],
       iChFree[ CDX_BALANCE_LEAFPAGES + 2 ];
   int iFirstKey, iBlncKeys = CDX_BALANCE_LEAFPAGES;
   int iLen = pPage->TagParent->uiLen + 8,
       iKeys = 0, iFree = 0, iSkip = 0, iBufSize = 0;
   ZH_BYTE * pKeyPool = NULL, * pPtr;
   ZH_BOOL fIns;
   int iRet = 0, iDup, iMax, i;
#ifndef ZH_CDX_PACKTRAIL
   int iTmp;
#endif

#ifdef ZH_CDX_DBGCODE_EXT
   zh_cdxPageCheckKeys( pPage );
#endif

   if( pPage->iCurKey > 0 )
      iFirstKey = pPage->iCurKey - 1;
   else
   {
      iFirstKey = 0;
      --iBlncKeys;
      if( pPage->Left != CDX_DUMMYNODE )
         iRet |= NODE_BALANCE;
   }
   if( iBlncKeys > pPage->iKeys - iFirstKey )
   {
      iBlncKeys = pPage->iKeys - iFirstKey;
      if( pPage->Right != CDX_DUMMYNODE )
         iRet |= NODE_BALANCE;
   }

#ifdef ZH_CDX_DSPDBG_INFO
   printf( "\r\nleaf balance: Page=%lx (%d/%d)", pPage->Page, iFirstKey, iBlncKeys );
   fflush( stdout );
#endif

   if( ( iChildRet & ( NODE_SPLIT | NODE_JOIN ) ) == 0 &&
       ( iBlncKeys < 2 || ( iChildRet & NODE_BALANCE ) == 0 ) )
      return iRet;

   for( i = 0; i < iBlncKeys; i++ )
   {
      ZH_ULONG ulPage = zh_cdxPageGetKeyPage( pPage, iFirstKey + i );
      if( pPage->Child && pPage->Child->Page == ulPage )
      {
         childs[ i ]  = pPage->Child;
         pPage->Child = NULL;
      }
      else
         childs[ i ] = zh_cdxPageNew( pPage->TagParent, pPage, ulPage );

#ifdef ZH_CDX_DBGCODE
      if( i > 0 && ( childs[ i ]->Page != childs[ i - 1 ]->Right ||
                     childs[ i ]->Left != childs[ i - 1 ]->Page ) )
      {
         printf( "\r\nchilds[%d]->Page=%lx, childs[%d]->Right=%lx, childs[%d]->Page=%lx, childs[%d]->Left=%lx",
                 i - 1, childs[ i - 1 ]->Page, i - 1, childs[ i - 1 ]->Right,
                 i, childs[ i ]->Page, i, childs[ i ]->Left );
         fflush( stdout );
         zh_cdxErrInternal( "zh_cdxPageKeyLeafBalance: index corrupted." );
      }
#endif
      iChKeys[ i ] = childs[ i ]->iKeys;
      iChFree[ i ] = childs[ i ]->iFree;
      if( childs[ i ]->iFree >= childs[ i ]->ReqByte ) /* TODO: increase limit for last page */
         iFree += childs[ i ]->iFree;
      else if( childs[ i ]->iFree >= 0 )
      {
         if( i == iSkip )
            ++iSkip;
#if 1
         else if( i + 1 == iBlncKeys && ( iChildRet & NODE_SPLIT ) == 0 )
         {
            iBlncKeys--;
            zh_cdxPageFree( childs[ i ], ZH_FALSE );
         }
#endif
      }
      if( i >= iSkip && i < iBlncKeys )
         iKeys += childs[ i ]->iKeys;

#ifdef ZH_CDX_DSPDBG_INFO
      printf( ", childs[%d]->Page=%lx(%d/%d)", i, childs[ i ]->Page, childs[ i ]->iKeys, childs[ i ]->iFree );
      printf( "(%d/%d/%d:%d,%lx)", i, iSkip, iBlncKeys, iKeys, childs[ i ]->Right );
      fflush( stdout );
#endif
   }
   if( ( iChildRet & NODE_SPLIT ) == 0 )
   {
      for( i = iBlncKeys - 1; i > iSkip && childs[ i ]->iFree >= 0 && childs[ i ]->iFree < childs[ i ]->ReqByte; i-- )
      {
         iKeys -= childs[ i ]->iKeys;
         zh_cdxPageFree( childs[ i ], ZH_FALSE );
         iBlncKeys--;
      }
   }
   if( ( iChildRet & ( NODE_SPLIT | NODE_JOIN ) ) == 0 &&
       ( iBlncKeys < 2 ||
         iFree < pPage->TagParent->pIndex->uiPageLen - CDX_EXT_HEADSIZE ) )
   {
      for( i = 0; i < iBlncKeys; i++ )
         zh_cdxPageFree( childs[ i ], ZH_FALSE );
      return iRet;
   }
#ifdef ZH_CDX_DSPDBG_INFO
   printf( "\r\nleaf balance: Page=%lx iKeys=%d", pPage->Page, iKeys );
   fflush( stdout );
#endif
   if( iKeys > 0 )
   {
      iBufSize = iKeys;
      pPtr = pKeyPool = ( ZH_BYTE * ) zh_xgrab( iBufSize * iLen );
      for( i = iSkip; i < iBlncKeys && iKeys > 0; i++ )
      {
         if( childs[ i ]->iKeys > 0 )
         {
            if( childs[ i ]->pKeyBuf )
               memcpy( pPtr, childs[ i ]->pKeyBuf, childs[ i ]->iKeys * iLen );
            else
               zh_cdxPageLeafDecode( childs[ i ], pPtr );
            /* update number of duplicate characters when join pages */
            if( pPtr > pKeyPool )
            {
#ifdef ZH_CDX_PACKTRAIL
               iMax = iLen - 8 - ZH_GET_LE_UINT16( &pPtr[ iLen - 2 ] );
#else
               iTmp = ZH_GET_LE_UINT16( &pPtr[ -2 ] );
               iMax = ZH_GET_LE_UINT16( &pPtr[ iLen - 2 ] );
               iMax = iLen - 8 - ZH_MAX( iMax, iTmp );
#endif
               iDup = 0;
               while( iDup < iMax && pPtr[ iDup ] == pPtr[ iDup - iLen ] )
                  ++iDup;
               ZH_PUT_LE_UINT16( &pPtr[ iLen - 4 ], iDup );
               if( iSkip == i - 1 && childs[ iSkip ]->iFree >= 0 &&
                   iLen - 8 - iDup - ZH_GET_LE_UINT16( &pPtr[ iLen - 2 ] ) >
                   childs[ iSkip ]->iFree - childs[ iSkip ]->ReqByte )
               {
                  memmove( pKeyPool, pPtr, childs[ i ]->iKeys * iLen );
                  pPtr = pKeyPool;
                  iKeys -= childs[ i - 1 ]->iKeys;
                  iSkip++;
#ifdef ZH_CDX_DSPDBG_INFO
                  printf( "\r\niSkip=%d, iBlncKeys=%d", iSkip, iBlncKeys );
                  fflush( stdout );
#endif
               }
            }
            pPtr += childs[ i ]->iKeys * iLen;
#ifdef ZH_CDX_DSPDBG_INFO
            printf( ", childs[%d]->iKeys=%d", i, childs[ i ]->iKeys );
            fflush( stdout );
#endif
         }
      }
   }

#ifdef ZH_CDX_DBGCODE_EXT
   zh_cdxPageCheckDupTrl( pPage, pKeyPool, iKeys, ZH_FALSE );
#endif
   pPtr = pKeyPool;
   fIns = ZH_FALSE;
   i = iSkip;
   while( iKeys > 0 )
   {
      if( i == iBlncKeys )
      {
         if( childs[ i - 1 ]->Right != CDX_DUMMYNODE )
            lpTmpPage = zh_cdxPageNew( pPage->TagParent, pPage, childs[ i - 1 ]->Right );
         else
            lpTmpPage = NULL;

#if 1
         if( ! fIns && lpTmpPage != NULL )
         {
            int j, iSize = 0, iMaxReq;
            ZH_ULONG ulMaxRec = 0, ul;
            ZH_BYTE * pbKey;

            for( j = 0; j < iKeys; j++ )
            {
               if( ulMaxRec < ( ul = ZH_GET_LE_UINT32( &pPtr[ ( j + 1 ) * iLen - 8 ] ) ) )
                  ulMaxRec = ul;
               iSize += iLen - 8 - ( j == 0 ? 0 : ZH_GET_LE_UINT16( &pPtr[ ( j + 1 ) * iLen - 4 ] ) ) -
                                                  ZH_GET_LE_UINT16( &pPtr[ ( j + 1 ) * iLen - 2 ] );
            }
            pbKey = zh_cdxPageGetKeyVal( lpTmpPage, 0 );
            iMax = zh_cdxPageGetKeyTrl( lpTmpPage, 0 );
#ifdef ZH_CDX_PACKTRAIL
            iMax = iLen - 8 - iMax;
#else
            iTmp = ZH_GET_LE_UINT16( &pPtr[ iKeys * iLen - 2 ] );
            iMax = iLen - 8 - ZH_MAX( iTmp, iMax );
#endif
            for( j = 0; j < iMax &&
                         pPtr[ ( iKeys - 1 ) * iLen + j ] == pbKey[ j ]; j++ ) {}
#ifdef ZH_CDX_DSPDBG_INFO
            printf( "\r\niDup=%d, iTrl=%d ", j, iLen - 8 - iMax ); fflush( stdout );
#endif
            iSize -= j;
            iMaxReq = lpTmpPage->ReqByte;
            ul = lpTmpPage->RNMask;
            while( ulMaxRec > ul )
            {
               ++iMaxReq;
               ul = ( ul << 8 ) | 0xFF;
            }
            iSize += iKeys * iMaxReq;
            iSize = lpTmpPage->iFree - iSize -
                     ( iMaxReq - lpTmpPage->ReqByte ) * lpTmpPage->iKeys;
            if( iSize < 0 )
               fIns = ZH_TRUE;
            else
            {
#ifdef ZH_CDX_DSPDBG_INFO
               printf( "\r\ninserting iDup=%d #keys=%d/%d (%d) parent=%lx, child=%lx (%d), rec=%ld",
                       j, iKeys, lpTmpPage->iKeys, i, pPage->Page, lpTmpPage->Page, iSize, ( ZH_ULONG ) ZH_GET_LE_UINT32( pPtr + iLen - 8 ) );
               fflush( stdout );
#endif
               if( iBufSize >= iKeys + lpTmpPage->iKeys )
               {
                  memmove( pKeyPool, pPtr, iKeys * iLen );
               }
               else
               {
                  ZH_BYTE * pTmp;
                  iBufSize = iKeys + lpTmpPage->iKeys;
                  pTmp = ( ZH_BYTE * ) zh_xgrab( iBufSize * iLen );
                  memcpy( pTmp, pPtr, iKeys * iLen );
                  zh_xfree( pKeyPool );
                  pKeyPool = pTmp;
               }
               if( lpTmpPage->iKeys > 0 )
               {
                  pPtr = &pKeyPool[ iKeys * iLen ];
                  if( lpTmpPage->pKeyBuf )
                     memcpy( pPtr, lpTmpPage->pKeyBuf, lpTmpPage->iKeys * iLen );
                  else
                     zh_cdxPageLeafDecode( lpTmpPage, pPtr );
#ifdef ZH_CDX_PACKTRAIL
                  iMax = iLen - 8 - ZH_GET_LE_UINT16( &pPtr[ iLen - 2 ] );
#else
                  iTmp = ZH_GET_LE_UINT16( &pPtr[ -2 ] );
                  iMax = ZH_GET_LE_UINT16( &pPtr[ iLen - 2 ] );
                  iMax = iLen - 8 - ZH_MAX( iMax, iTmp );
#endif
                  iDup = 0;
                  while( iDup < iMax && pPtr[ iDup ] == pPtr[ iDup - iLen ] )
                     ++iDup;
                  ZH_PUT_LE_UINT16( &pPtr[ iLen - 4 ], iDup );
                  iKeys += lpTmpPage->iKeys;
#ifdef ZH_CDX_DSPDBG_INFO
                  printf( " iDup2=%d, iTrl2=%d ", iDup, ZH_GET_LE_UINT16( &pPtr[ iLen - 2 ] ) ); fflush( stdout );
#endif
               }
               pPtr = pKeyPool;
               childs[ i ] = lpTmpPage;
               if( iFirstKey + i >= pPage->iKeys )
                  iRet |= NODE_NEWLASTKEY;
#ifdef ZH_CDX_DBGCODE_EXT
               childs[ i ]->iKeys = 0;
               if( childs[ i ]->pKeyBuf )
               {
                  zh_xfree( childs[ i ]->pKeyBuf );
                  childs[ i ]->pKeyBuf     = NULL;
                  childs[ i ]->fBufChanged = ZH_FALSE;
               }
               zh_cdxPageCalcLeafSpace( childs[ i ], pPtr, iKeys );
               zh_cdxPageLeafEncode( childs[ i ], pPtr, childs[ i ]->iKeys );
               iSize += ( iMaxReq - childs[ i ]->ReqByte ) * childs[ i ]->iKeys;
               if( iSize != childs[ i ]->iFree )
               {
                  printf( "\r\ninserting, iSize=%d, childs[ i ]->iFree=%d", iSize, childs[ i ]->iFree ); fflush( stdout );
                  printf( "\r\niKeys=%d, iMaxReq=%d", iKeys, iMaxReq ); fflush( stdout );
                  zh_cdxErrInternal( "zh_cdxPageGetChild: index corrupted." );
               }
#endif
            }
#endif
         }
         else
            fIns = ZH_TRUE;

         if( fIns )
         {
            childs[ i ] = zh_cdxPageNew( pPage->TagParent, pPage, 0 );
            childs[ i ]->PageType = CDX_NODE_LEAF;
            /* Update siblings links */
            childs[ i ]->Left  = childs[ i - 1 ]->Page;
            childs[ i ]->Right = childs[ i - 1 ]->Right;
            childs[ i - 1 ]->Right = childs[ i ]->Page;
            childs[ i - 1 ]->fChanged = ZH_TRUE;
            if( lpTmpPage != NULL )
            {
               lpTmpPage->Left = childs[ i ]->Page;
               lpTmpPage->fChanged = ZH_TRUE;
               zh_cdxPageFree( lpTmpPage, ZH_FALSE );
            }
            iBlncKeys++;
            iRet |= NODE_BALANCE;
#ifdef ZH_CDX_DSPDBG_INFO
            printf( "\r\nleaf balance: new child[%d]->Page=%lx", i, childs[ i ]->Page );
            fflush( stdout );
#endif
         }
      }
      childs[ i ]->iKeys = 0;
      if( childs[ i ]->pKeyBuf )
      {
         zh_xfree( childs[ i ]->pKeyBuf );
         childs[ i ]->pKeyBuf = NULL;
         childs[ i ]->fBufChanged = ZH_FALSE;
      }
      zh_cdxPageCalcLeafSpace( childs[ i ], pPtr, iKeys );
      if( i == iSkip && i < iBlncKeys && ! childs[ i ]->fChanged &&
          childs[ i ]->iKeys == iChKeys[ i ] &&
          childs[ i ]->iFree == iChFree[ i ] )
      {
#ifdef ZH_CDX_DSPDBG_INFO
         printf( "\r\niskip++\r\n" );
         fflush( stdout );
#endif
         iSkip++;
      }
      else
      {
         zh_cdxPageLeafEncode( childs[ i ], pPtr, childs[ i ]->iKeys );
      }
      pPtr += childs[ i ]->iKeys * iLen;
      iKeys -= childs[ i ]->iKeys;
      /* update parent key */
      if( i < iBlncKeys )
         zh_cdxPageIntSetKey( pPage, iFirstKey + i, fIns,
                              pPtr - iLen, ZH_GET_LE_UINT32( pPtr - 8 ),
                              childs[ i ]->Page );
      else
         iBlncKeys++;
#ifdef ZH_CDX_DSPDBG_INFO
      printf( " (%d/%d)", childs[ i ]->iKeys, childs[ i ]->iFree );
      fflush( stdout );
#endif
#ifdef ZH_CDX_DBGCODE_EXT
      zh_cdxPageCheckKeys( childs[ i ] );
#endif
      i++;
   }
   if( i < iBlncKeys )
   {
      /* Update siblings links */
#if 1
      if( childs[ iBlncKeys - 1 ]->Right != CDX_DUMMYNODE &&
          ( i > 1 || ( i == 1 && childs[ 0 ]->Left == CDX_DUMMYNODE ) ) )
      {
         ZH_ULONG Page;
         Page = childs[ iBlncKeys - 1 ]->Page;
         childs[ iBlncKeys - 1 ]->Page = childs[ i - 1 ]->Page;
         childs[ i - 1 ]->Page = Page;
         zh_cdxPageIntSetKey( pPage, iFirstKey + i - 1, ZH_FALSE, NULL, 0, Page );
         childs[ i - 1 ]->Right = childs[ iBlncKeys - 1 ]->Right;
         childs[ i - 1 ]->fChanged = ZH_TRUE;
         if( i > 1 )
         {
            childs[ i - 2 ]->Right = Page;
            childs[ i - 2 ]->fChanged = ZH_TRUE;
         }
      }
      else
#endif
      {
         ZH_ULONG Left, Right;
         Right = childs[ iBlncKeys - 1 ]->Right;
         if( i > 0 )
         {
            Left = childs[ i - 1 ]->Page;
            childs[ i - 1 ]->Right = Right;
            childs[ i - 1 ]->fChanged  = ZH_TRUE;
         }
         else
         {
            Left = childs[ 0 ]->Left;
            if( Left != CDX_DUMMYNODE )
            {
               lpTmpPage = zh_cdxPageNew( pPage->TagParent, pPage, Left );
               lpTmpPage->Right = Right;
               lpTmpPage->fChanged  = ZH_TRUE;
               zh_cdxPageFree( lpTmpPage, ZH_FALSE );
            }
         }
         if( Right != CDX_DUMMYNODE )
         {
            lpTmpPage = zh_cdxPageNew( pPage->TagParent, pPage, Right );
            lpTmpPage->Left = Left;
            lpTmpPage->fChanged  = ZH_TRUE;
            zh_cdxPageFree( lpTmpPage, ZH_FALSE );
         }
      }
      /* Unlink empty pages from parent */
      while( i < iBlncKeys )
      {
         /* Delete parent key */
         iBlncKeys--;
#ifdef ZH_CDX_DSPDBG_INFO
         printf( "\r\nleaf balance: free child[%d]->Page=%lx", iBlncKeys, childs[ iBlncKeys ]->Page );
         fflush( stdout );
#endif
         if( childs[ iBlncKeys ]->pKeyBuf )
         {
            zh_xfree( childs[ iBlncKeys ]->pKeyBuf );
            childs[ iBlncKeys ]->pKeyBuf = NULL;
            childs[ iBlncKeys ]->fBufChanged = ZH_FALSE;
         }
         zh_cdxPageIntDelKey( pPage, iFirstKey + iBlncKeys );
         childs[ iBlncKeys ]->Owner    = NULL;
         childs[ iBlncKeys ]->fChanged = ZH_FALSE;
         childs[ iBlncKeys ]->PageType = CDX_NODE_UNUSED;
         childs[ iBlncKeys ]->Left     = CDX_DUMMYNODE;
         childs[ iBlncKeys ]->Right    = CDX_DUMMYNODE;
         zh_cdxPageFree( childs[ iBlncKeys ], ZH_FALSE );
      }
      iRet |= NODE_BALANCE;
   }
   for( i = 0; i < iBlncKeys; i++ )
      zh_cdxPageFree( childs[ i ], ZH_FALSE );

   if( pKeyPool )
      zh_xfree( pKeyPool );
   pPage->fChanged = ZH_TRUE;
#ifdef ZH_CDX_DBGCODE_EXT
   zh_cdxPageCheckKeys( pPage );
#endif
   if( pPage->iKeys > pPage->TagParent->MaxKeys )
      iRet |= NODE_SPLIT;
   return iRet;
}

static int zh_cdxPageKeyIntBalance( LPCDXPAGE pPage, int iChildRet )
{
   LPCDXPAGE childs[ CDX_BALANCE_INTPAGES + 2 ], lpTmpPage;
   int iFirstKey, iBlncKeys = CDX_BALANCE_INTPAGES;
   int iLen = pPage->TagParent->uiLen + 8, iKeys = 0, iNeedKeys,
       iMin = pPage->TagParent->MaxKeys, iMax = 0, iDiv;
   ZH_BYTE * pKeyPool = NULL, * pPtr;
   ZH_BOOL fForce = ( iChildRet & ( NODE_SPLIT | NODE_JOIN ) ) != 0;
   int iRet = 0, i;

   if( ! fForce && ( iChildRet & NODE_BALANCE ) == 0 )
      return iRet;

   if( pPage->Child && pPage->Child->Child )
      zh_cdxPageFree( pPage->Child->Child, ZH_FALSE );

#ifdef ZH_CDX_DBGCODE_EXT
   zh_cdxPageCheckKeys( pPage );
#endif

   if( pPage->iKeys <= iBlncKeys || pPage->iCurKey <= iBlncKeys / 2 )
      iFirstKey = 0;
   else if( pPage->iCurKey + ( iBlncKeys >> 1 ) >= pPage->iKeys )
      iFirstKey = pPage->iKeys - iBlncKeys;
   else
      iFirstKey = pPage->iCurKey - ( iBlncKeys >> 1 );
   if( iBlncKeys > pPage->iKeys - iFirstKey )
   {
      iBlncKeys = pPage->iKeys - iFirstKey;
      iRet |= NODE_BALANCE;
   }

#ifdef ZH_CDX_DSPDBG_INFO
   printf( "\r\nbalance: Page=%lx(%d) (%d/%d)", pPage->Page, pPage->iKeys, iFirstKey, iBlncKeys );
   fflush( stdout );
#endif

   if( ! fForce && iBlncKeys < 2 )
      return iRet;

   for( i = 0; i < iBlncKeys; i++ )
   {
      ZH_ULONG ulPage = zh_cdxPageGetKeyPage( pPage, iFirstKey + i );
      if( pPage->Child && pPage->Child->Page == ulPage )
      {
         childs[ i ]  = pPage->Child;
         pPage->Child = NULL;
      }
      else
         childs[ i ] = zh_cdxPageNew( pPage->TagParent, pPage, ulPage );

#ifdef ZH_CDX_DBGCODE
      if( i > 0 && ( childs[ i ]->Page != childs[ i - 1 ]->Right ||
                     childs[ i ]->Left != childs[ i - 1 ]->Page ) )
      {
         printf( "\r\nchilds[%d]->Page=%lx, childs[%d]->Right=%lx, childs[%d]->Page=%lx, childs[%d]->Left=%lx",
                 i - 1, childs[ i - 1 ]->Page, i - 1, childs[ i - 1 ]->Right,
                 i, childs[ i ]->Page, i, childs[ i ]->Left );
         fflush( stdout );
         zh_cdxErrInternal( "zh_cdxPageKeyIntBalance: index corrupted." );
      }
#endif
      iKeys += childs[ i ]->iKeys;

      if( childs[ i ]->iKeys > iMax )
         iMax = childs[ i ]->iKeys;
      if( childs[ i ]->iKeys < iMin )
         iMin = childs[ i ]->iKeys;
#ifdef ZH_CDX_DSPDBG_INFO
      printf( ", childs[%d]->Page=%lx(%d)", i, childs[ i ]->Page, childs[ i ]->iKeys );
      fflush( stdout );
#endif
   }
   iNeedKeys = ( iKeys + pPage->TagParent->MaxKeys - 1 )
                       / pPage->TagParent->MaxKeys;
#if 1
   if( iNeedKeys == 1 && iBlncKeys > 1 && childs[ 0 ]->Left != CDX_DUMMYNODE &&
       childs[ iBlncKeys - 1 ]->Right != CDX_DUMMYNODE &&
       iKeys >= ( CDX_BALANCE_INTPAGES << 1 ) &&
       iKeys > ( ( pPage->TagParent->MaxKeys * 3 ) >> 1 ) )
   {
      iNeedKeys = 2;
   }
#endif
#if 1
   iDiv = ZH_MAX( iMax - iMin - ( pPage->TagParent->MaxKeys >> 1 ) + 1,
                  iBlncKeys - iNeedKeys );
#else
   iDiv = iMax - iMin;
#endif
   if( iKeys > 0 && ( iDiv >= 2 || fForce ) )
   {
#if 1
      if( iBlncKeys == 1 && iKeys > pPage->TagParent->MaxKeys &&
          childs[ 0 ]->Right != CDX_DUMMYNODE )
      {
         lpTmpPage = zh_cdxPageNew( pPage->TagParent, pPage, childs[ 0 ]->Right );
         iKeys += lpTmpPage->iKeys;
         childs[ iBlncKeys++ ] = lpTmpPage;
         if( iFirstKey + iBlncKeys > pPage->iKeys )
            iRet |= NODE_NEWLASTKEY;
         iNeedKeys = ( iKeys + pPage->TagParent->MaxKeys - 1 )
                             / pPage->TagParent->MaxKeys;
      }
      else
#endif
      {
         iMin = ZH_MAX( iKeys / iNeedKeys, 2 );
         iMax = ZH_MAX( ( iKeys + iNeedKeys - 1 ) / iNeedKeys, iMin );
         for( i = iBlncKeys - 1; i > 1 &&
              childs[ i ]->iKeys >= iMin && childs[ i ]->iKeys <= iMax; i-- )
         {
            iKeys -= childs[ i ]->iKeys;
            zh_cdxPageFree( childs[ i ], ZH_FALSE );
            iBlncKeys--;
            iMin = ZH_MAX( iKeys / iNeedKeys, 2 );
            iMax = ZH_MAX( ( iKeys + iNeedKeys - 1 ) / iNeedKeys, iMin );
         }
         while( iBlncKeys > 2 && childs[ 0 ]->iKeys >= iMin && childs[ 0 ]->iKeys <= iMax )
         {
            iKeys -= childs[ 0 ]->iKeys;
            zh_cdxPageFree( childs[ 0 ], ZH_FALSE );
            iBlncKeys--;
            iFirstKey++;
            for( i = 0; i < iBlncKeys; i++ )
            {
               childs[ i ] = childs[ i + 1 ];
            }
            iMin = ZH_MAX( iKeys / iNeedKeys, 2 );
            iMax = ZH_MAX( ( iKeys + iNeedKeys - 1 ) / iNeedKeys, iMin );
         }
      }
   }
   if( ! fForce && ( iBlncKeys < 2 || iDiv < 2 ) )
   {
      for( i = 0; i < iBlncKeys; i++ )
         zh_cdxPageFree( childs[ i ], ZH_FALSE );
      return iRet;
   }

   if( iKeys > 0 )
   {
      pPtr = pKeyPool = ( ZH_BYTE * ) zh_xgrab( iKeys * iLen );
      for( i = 0; i < iBlncKeys; i++ )
      {
         if( childs[ i ]->iKeys > 0 )
         {
            memcpy( pPtr, zh_cdxPageIntKeyPool( childs[ i ] ), childs[ i ]->iKeys * iLen );
            pPtr += childs[ i ]->iKeys * iLen;
         }
      }
   }

   if( iNeedKeys > iBlncKeys )
   {
      if( iBlncKeys < 2 )
         i = iBlncKeys;
      else
      {
         i = iBlncKeys - 1;
         childs[ iBlncKeys ] = childs[ i ];
      }
      childs[ i ] = zh_cdxPageNew( pPage->TagParent, pPage, 0 );
      childs[ i ]->PageType = CDX_NODE_BRANCH;
      childs[ i ]->iKeys    = 0;
      childs[ i ]->fChanged = ZH_TRUE;
      /* Add new parent key */
      zh_cdxPageIntSetKey( pPage, iFirstKey + i, ZH_TRUE,
                           NULL, 0, childs[ iBlncKeys ]->Page );
      /* Update siblings links */
      childs[ i ]->Left      = childs[ i - 1 ]->Page;
      childs[ i ]->Right     = childs[ i - 1 ]->Right;
      childs[ i - 1 ]->Right = childs[ i ]->Page;
      if( i < iBlncKeys )
         childs[ i + 1 ]->Left = childs[ i ]->Page;
      else if( childs[ i ]->Right != CDX_DUMMYNODE )
      {
         lpTmpPage = zh_cdxPageNew( pPage->TagParent, pPage, childs[ iBlncKeys ]->Right );
         lpTmpPage->Left = childs[ i ]->Page;
         lpTmpPage->fChanged = ZH_TRUE;
         zh_cdxPageFree( lpTmpPage, ZH_FALSE );
      }
#ifdef ZH_CDX_DSPDBG_INFO
      printf( "\r\nint balance: new child[%d]->Page=%lx", iBlncKeys, childs[ iBlncKeys ]->Page );
      fflush( stdout );
#endif
      iBlncKeys++;
      iRet |= NODE_BALANCE;
   }
   else if( iNeedKeys < iBlncKeys )
   {
      ZH_ULONG Left, Right;

      /* Update siblings links */
      if( iNeedKeys > 1 )
      {
         childs[ iNeedKeys - 2 ]->Right = childs[ iBlncKeys - 1 ]->Page;
         childs[ iBlncKeys - 1 ]->Left  = childs[ iNeedKeys - 2 ]->Page;
         lpTmpPage = childs[ iBlncKeys - 1 ];
         childs[ iBlncKeys - 1 ] = childs[ iNeedKeys - 1 ];
         childs[ iNeedKeys - 1 ] = lpTmpPage;
      }
      else if( iNeedKeys > 0 && childs[ 0 ]->Left == CDX_DUMMYNODE )
      {
         lpTmpPage = childs[ iBlncKeys - 1 ];
         childs[ iBlncKeys - 1 ] = childs[ 0 ];
         childs[ 0 ]       = lpTmpPage;
         childs[ 0 ]->Left = CDX_DUMMYNODE;
      }
      else
      {
         Right = childs[ iBlncKeys - 1 ]->Right;
         if( iNeedKeys > 0 )
         {
            Left = childs[ iNeedKeys - 1 ]->Page;
            childs[ iNeedKeys - 1 ]->Right = Right;
         }
         else
         {
            Left = childs[ 0 ]->Left;
            if( Left != CDX_DUMMYNODE )
            {
               lpTmpPage = zh_cdxPageNew( pPage->TagParent, pPage, Left );
               lpTmpPage->Right = Right;
               lpTmpPage->fChanged = ZH_TRUE;
               zh_cdxPageFree( lpTmpPage, ZH_FALSE );
            }
         }
         if( Right != CDX_DUMMYNODE )
         {
            lpTmpPage = zh_cdxPageNew( pPage->TagParent, pPage, Right );
            lpTmpPage->Left = Left;
            lpTmpPage->fChanged = ZH_TRUE;
            zh_cdxPageFree( lpTmpPage, ZH_FALSE );
         }
      }
      /* Unlink empty pages from parent */
      for( i = iBlncKeys - 1; i >= iNeedKeys; i-- )
      {
         /* Delete parent key */
#ifdef ZH_CDX_DSPDBG_INFO
         printf( "\r\nbalance: free child[%d]->Page=%lx", i, childs[ i ]->Page );
         fflush( stdout );
#endif
         zh_cdxPageIntDelKey( pPage, iFirstKey + i );
         childs[ i ]->Owner    = NULL;
         childs[ i ]->fChanged = ZH_FALSE;
         childs[ i ]->PageType = CDX_NODE_UNUSED;
         childs[ i ]->Left     = CDX_DUMMYNODE;
         childs[ i ]->Right    = CDX_DUMMYNODE;
         childs[ i ]->iKeys    = 0;
         zh_cdxPageFree( childs[ i ], ZH_FALSE );
      }
      iBlncKeys = iNeedKeys;
      iRet |= NODE_BALANCE;
   }

   /*
    * Redistribute childs internal node's keys and update parent keys
    */
   if( iKeys > 0 )
   {
      fForce = pPage->TagParent->MaxKeys == 2 && iBlncKeys > 2 &&
               iKeys == ( iBlncKeys << 1 ) - 1;
      pPtr = pKeyPool;
      for( i = 0; i < iBlncKeys; i++ )
      {
         int iNodeKeys = ( fForce && i == 1 ) ? 1 :
                         ( ( iKeys + iBlncKeys - i - 1 ) / ( iBlncKeys - i ) );
#ifdef ZH_CDX_DBGCODE
         if( iNodeKeys > pPage->TagParent->MaxKeys )
            zh_cdxErrInternal( "zh_cdxPageKeyIntBalance: iNodeKeys calculated wrong!" );
#endif
         /* TODO: do nothing if iNodeKeys == childs[ i ]->iKeys && i == iSkip */
         memcpy( zh_cdxPageIntKeyPool( childs[ i ] ), pPtr, iNodeKeys * iLen );
         childs[ i ]->iKeys    = iNodeKeys;
         childs[ i ]->fChanged = ZH_TRUE;
         pPtr  += iNodeKeys * iLen;
         iKeys -= iNodeKeys;
         /* update parent key */
         if( iFirstKey + i < pPage->iKeys )
         {
            zh_cdxPageIntSetKey( pPage, iFirstKey + i, ZH_FALSE,
                                 pPtr - iLen, ZH_GET_BE_UINT32( pPtr - 8 ),
                                 childs[ i ]->Page );
         }
#ifdef ZH_CDX_DSPDBG_INFO
         printf( " (%d)", childs[ i ]->iKeys );
#endif
#ifdef ZH_CDX_DBGCODE_EXT
         zh_cdxPageCheckKeys( childs[ i ] );
#endif
         zh_cdxPageFree( childs[ i ], ZH_FALSE );
      }
      zh_xfree( pKeyPool );
   }
   pPage->fChanged = ZH_TRUE;
#ifdef ZH_CDX_DBGCODE_EXT
   zh_cdxPageCheckKeys( pPage );
#endif
   if( pPage->iKeys > pPage->TagParent->MaxKeys )
      iRet |= NODE_SPLIT;
   return iRet;
}

/*
 * balance keys in child pages
 */
static int zh_cdxPageBalance( LPCDXPAGE pPage, int iChildRet )
{
   int iRet = 0;

   if( ( pPage->PageType & CDX_NODE_LEAF ) != 0 )
      iRet = iChildRet;
   else
   {
      if( iChildRet & NODE_NEWLASTKEY )
      {
         if( pPage->Child->iKeys == 0 )
         {
            iChildRet |= NODE_JOIN;
            iRet |= NODE_NEWLASTKEY;
         }
         else
         {
            zh_cdxPageIntSetKey( pPage, pPage->iCurKey, ZH_FALSE,
                                 zh_cdxPageGetKeyVal( pPage->Child, pPage->Child->iKeys - 1 ),
                                 zh_cdxPageGetKeyRec( pPage->Child, pPage->Child->iKeys - 1 ),
                                 pPage->Child->Page );
#ifdef ZH_CDX_DBGCODE_EXT
            zh_cdxPageCheckKeys( pPage );
#endif
            pPage->fChanged = ZH_TRUE;
            if( pPage->iCurKey >= pPage->iKeys - 1 )
               iRet |= NODE_NEWLASTKEY;
         }
      }
      if( ( pPage->Child->PageType & CDX_NODE_LEAF ) != 0 )
         iRet |= zh_cdxPageKeyLeafBalance( pPage, iChildRet );
      else
         iRet |= zh_cdxPageKeyIntBalance( pPage, iChildRet );
   }
   if( ! pPage->Owner )
   {
      if( pPage->iKeys == 0 )
      {
         pPage->PageType |= CDX_NODE_LEAF;
         zh_cdxPageLeafInitSpace( pPage );
      }
      else if( iRet & NODE_SPLIT )
         iRet = zh_cdxPageRootSplit( pPage );
   }
   return iRet;
}

/*
 * split Root Page
 */
static int zh_cdxPageRootSplit( LPCDXPAGE pPage )
{
   LPCDXPAGE pNewRoot;
   ZH_ULONG ulPage;

   pNewRoot = zh_cdxPageNew( pPage->TagParent, NULL, 0 );
   /*
    * do not change root page address if it's unnecessary
    * so we don't have to update Tag header
    */
   pPage->TagParent->RootPage = pNewRoot;
   ulPage = pNewRoot->Page;
   pNewRoot->Page = pPage->Page;
   pPage->Page = ulPage;

   pPage->Owner = pNewRoot;
   pPage->PageType &= ~CDX_NODE_ROOT;
   pNewRoot->PageType = CDX_NODE_ROOT | CDX_NODE_BRANCH;
   pNewRoot->fChanged = ZH_TRUE;
   pNewRoot->Child    = pPage;
   pNewRoot->iCurKey  = 0;
   zh_cdxPageIntSetKey( pNewRoot, 0, ZH_TRUE,
                        zh_cdxPageGetKeyVal( pPage, pPage->iKeys-1 ),
                        zh_cdxPageGetKeyRec( pPage, pPage->iKeys-1 ),
                        pPage->Page );
#ifdef ZH_CDX_DBGCODE_EXT
   zh_cdxPageCheckKeys( pNewRoot );
   zh_cdxTagPoolCheck( pPage->TagParent );
#endif
   zh_cdxPageBalance( pNewRoot, NODE_SPLIT );
   return 0;
}

/*
 * remove current Key from Tag
 */
static int zh_cdxPageKeyRemove( LPCDXPAGE pPage )
{
   int iChildRet;

   if( pPage->PageType & CDX_NODE_LEAF )
      iChildRet = zh_cdxPageLeafDelKey( pPage );
   else /* interior node */
      iChildRet = zh_cdxPageKeyRemove( pPage->Child );
   return zh_cdxPageBalance( pPage, iChildRet );
}

/*
 * add Key to Tag at current position
 */
static int zh_cdxPageKeyInsert( LPCDXPAGE pPage, LPCDXKEY pKey )
{
   int iChildRet;

   if( pPage->PageType & CDX_NODE_LEAF )
      iChildRet = zh_cdxPageLeafAddKey( pPage, pKey );
   else /* interior node */
      iChildRet = zh_cdxPageKeyInsert( pPage->Child, pKey );
#ifdef ZH_CDX_DBGUPDT
   cdxTmpStackSize++;
#endif
   return zh_cdxPageBalance( pPage, iChildRet );
}

/*
 * Store Tag header to index files
 */
static void zh_cdxTagHeaderStore( LPCDXTAG pTag )
{
   ZH_USHORT uiKeyLen, uiForLen;
   CDXTAGHEADER tagHeader;

   if( ! pTag->TagChanged )
      return;

   /*
    * TODO: !!! read the following field from the index file,
    *       at least freePtr has to be read for pTag->TagBlock == 0
    * tagHeader.freePtr  [ 4 ]      offset of list of free pages or -1
    * tagHeader.reserved1[ 4 ]      Version number ???
    * tagHeader.reserved2[ 486 ]
    */

   pTag->TagChanged = ZH_FALSE;
   pTag->OptFlags &= ~( CDX_TYPE_UNIQUE | CDX_TYPE_FORFILTER |
                        CDX_TYPE_PARTIAL | CDX_TYPE_CUSTOM );
   if( pTag->UniqueKey )
      pTag->OptFlags |= CDX_TYPE_UNIQUE;
   if( pTag->pForItem != NULL )
      pTag->OptFlags |= CDX_TYPE_FORFILTER;
#if defined( ZH_SIXCDX )
   if( pTag->Custom )
      pTag->OptFlags |= CDX_TYPE_PARTIAL | CDX_TYPE_CUSTOM;
   else if( pTag->ChgOnly )
      pTag->OptFlags |= CDX_TYPE_CUSTOM;
   else if( pTag->Partial )
      pTag->OptFlags |= CDX_TYPE_PARTIAL;
#else
   if( pTag->Custom )
      pTag->OptFlags |= CDX_TYPE_CUSTOM;
   if( pTag->Partial )
      pTag->OptFlags |= CDX_TYPE_PARTIAL;
#endif

   memset( &tagHeader, 0, sizeof( tagHeader ) );
   ZH_PUT_LE_UINT32( tagHeader.rootPtr, pTag->RootBlock );
   ZH_PUT_LE_UINT16( tagHeader.keySize, pTag->uiLen );
   ZH_PUT_LE_UINT16( tagHeader.headerLen, pTag->pIndex->uiHeaderLen );
   ZH_PUT_LE_UINT16( tagHeader.pageLen, pTag->pIndex->uiPageLen );
   tagHeader.indexOpt = pTag->OptFlags;
   if( pTag->TagBlock == 0 )
   {
      ZH_PUT_BE_UINT32( tagHeader.signature, CDX_ZIHER_SIGNATURE );
      tagHeader.indexSig = pTag->pIndex->fLargeFile ? 0x21 : 0x01;
   }
   else
      tagHeader.indexSig = 0x01;
   if( ! pTag->AscendKey )
      ZH_PUT_LE_UINT16( tagHeader.ascendFlg, 1 );
   if( pTag->IgnoreCase )
      tagHeader.ignoreCase = 1;

   uiKeyLen = pTag->KeyExpr == NULL ? 0 : ( ZH_USHORT ) strlen( pTag->KeyExpr );
   uiForLen = pTag->ForExpr == NULL ? 0 : ( ZH_USHORT ) strlen( pTag->ForExpr );

   if( uiKeyLen + uiForLen > CDX_HEADEREXPLEN - 2 )
      zh_cdxErrorRT( pTag->pIndex->pArea, EG_DATAWIDTH, EDBF_KEYLENGTH, NULL, 0, 0, NULL );
   else
   {
      ZH_PUT_LE_UINT16( tagHeader.keyExpPos, 0 );
      ZH_PUT_LE_UINT16( tagHeader.keyExpLen, uiKeyLen + 1 );
      ZH_PUT_LE_UINT16( tagHeader.forExpPos, uiKeyLen + 1 );
      ZH_PUT_LE_UINT16( tagHeader.forExpLen, uiForLen + 1 );
      if( uiKeyLen > 0 )
         memcpy( tagHeader.keyExpPool, pTag->KeyExpr, uiKeyLen );
      if( uiForLen > 0 )
         memcpy( tagHeader.keyExpPool + uiKeyLen + 1, pTag->ForExpr, uiForLen );
   }
   zh_cdxIndexPageWrite( pTag->pIndex, pTag->TagBlock,
                         ( const ZH_BYTE * ) &tagHeader,
                         sizeof( tagHeader ) );
}

#if defined( ZH_SIXCDX )
static ZH_BOOL zh_cdxIsTemplateFunc( const char * szKeyExpr )
{
   /* For CDX format SIx3 really makes something like that */
   return zh_strnicmp( szKeyExpr, "sxChar(", 7 ) == 0 ||
          zh_strnicmp( szKeyExpr, "sxDate(", 7 ) == 0 ||
          zh_strnicmp( szKeyExpr, "sxNum(", 6 ) == 0 ||
          zh_strnicmp( szKeyExpr, "sxLog(", 6 ) == 0;
}
#endif

static ZH_BOOL zh_cdxSetPageSize( LPCDXINDEX pIndex, ZH_BOOL fLargeFile,
                                  ZH_USHORT uiPageSize, ZH_USHORT uiHeaderSize )
{
   if( uiPageSize >= CDX_PAGELEN && uiPageSize <= CDX_PAGELEN_MAX &&
       ( ( uiPageSize - 1 ) & uiPageSize ) == 0 && uiHeaderSize == CDX_HEADERLEN )
   {
      pIndex->fLargeFile  = fLargeFile;
      pIndex->uiHeaderLen = uiHeaderSize;
      pIndex->uiPageLen   = uiPageSize;
      pIndex->uiPageBits  = CDX_PAGELEN_BITS;
      pIndex->uiMaxKeyLen = CDX_MAXKEY;

      if( uiPageSize > CDX_PAGELEN )
      {
         while( ( ZH_INT ) ( 1 << pIndex->uiPageBits ) < uiPageSize )
            ++pIndex->uiPageBits;
         pIndex->uiMaxKeyLen = ( ( uiPageSize - CDX_INT_HEADSIZE ) >> 1 ) - 8;
      }

      pIndex->nextAvail = CDX_DUMMYNODE;

      return ZH_TRUE;
   }
   return ZH_FALSE;
}

/*
 * Read a tag definition from the index file
 */
static void zh_cdxTagLoad( LPCDXTAG pTag )
{
   CDXTAGHEADER tagHeader;
   ZH_USHORT uiForPos, uiForLen, uiKeyPos, uiKeyLen;
   ZH_ULONG ulRecNo;

   /* read the page from a file */
   zh_cdxIndexPageRead( pTag->pIndex, pTag->TagBlock,
                        ( ZH_BYTE * ) &tagHeader, sizeof( tagHeader ) );

   uiForPos = ZH_GET_LE_UINT16( tagHeader.forExpPos );
   uiForLen = ZH_GET_LE_UINT16( tagHeader.forExpLen );
   uiKeyPos = ZH_GET_LE_UINT16( tagHeader.keyExpPos );
   uiKeyLen = ZH_GET_LE_UINT16( tagHeader.keyExpLen );

   pTag->RootBlock = ZH_GET_LE_UINT32( tagHeader.rootPtr );

   if( pTag->TagBlock == 0 )
   {
      ZH_BOOL fLargeFile = ZH_FALSE;
      ZH_USHORT uiPageLen = CDX_PAGELEN, uiHeaderLen = CDX_HEADERLEN;
      ZH_U32 u32Sig = ZH_GET_BE_UINT32( tagHeader.signature );

      if( u32Sig == CDX_ZIHER_SIGNATURE ||
          u32Sig == ZH_SWAP_UINT32( CDX_ZIHER_SIGNATURE ) )
      {
         fLargeFile  = tagHeader.indexSig == 0x21;
         uiHeaderLen = ZH_GET_LE_UINT16( tagHeader.headerLen );
         uiPageLen   = ZH_GET_LE_UINT16( tagHeader.pageLen );
      }

      if( ! zh_cdxSetPageSize( pTag->pIndex, fLargeFile, uiPageLen, uiHeaderLen ) )
         pTag->RootBlock = 0;
   }

   /* Return if:
    * no root page allocated
    * invalid root page offset (position inside an index file)
    * invalid key value length
    */
   if( pTag->RootBlock == 0 || ! zh_cdxFilePageRootValid( pTag->pIndex, pTag->RootBlock ) ||
       zh_cdxFilePageOffset( pTag->pIndex, pTag->RootBlock ) >= zh_fileSize( pTag->pIndex->pFile ) ||
       ZH_GET_LE_UINT16( tagHeader.keySize ) > pTag->pIndex->uiMaxKeyLen ||
       uiKeyLen + uiForLen > CDX_HEADEREXPLEN ||
       uiForPos + uiForLen > CDX_HEADEREXPLEN ||
       uiKeyPos + uiKeyLen > CDX_HEADEREXPLEN ||
       ( uiKeyPos < uiForPos ? ( uiKeyPos + uiKeyLen > uiForPos && tagHeader.keyExpPool[ uiForPos ] ) :
                               ( uiForPos + uiForLen > uiKeyPos && tagHeader.keyExpPool[ uiForPos ] ) ) )
   {
      pTag->RootBlock = 0; /* To force RT error - index corrupted */
      return;
   }

   /* some wrong RDDs do not set expression length this is workaround for them */
   if( uiKeyPos == 0 && uiKeyLen != 0 && uiForPos == 0 && uiForLen != 0 )
      uiForPos = uiKeyLen;
   if( ! uiKeyLen )
      uiKeyLen = ( uiForPos >= uiKeyPos ? uiForPos : CDX_HEADEREXPLEN ) - uiKeyPos;
   if( ! uiForLen )
      uiForLen = ( uiForPos <= uiKeyPos ? uiKeyPos : CDX_HEADEREXPLEN ) - uiForPos;

   pTag->KeyExpr   = ( char * ) zh_xgrab( uiKeyLen + 1 );
   zh_strncpyTrim( pTag->KeyExpr, ( const char * ) tagHeader.keyExpPool, uiKeyLen );

   pTag->uiLen     = ZH_GET_LE_UINT16( tagHeader.keySize );
   pTag->MaxKeys   = ( pTag->pIndex->uiPageLen - CDX_INT_HEADSIZE ) / ( pTag->uiLen + 8 );

   pTag->OptFlags  = tagHeader.indexOpt;
   pTag->UniqueKey = ( pTag->OptFlags & CDX_TYPE_UNIQUE ) != 0;
#if defined( ZH_SIXCDX )
   pTag->Custom    = ( pTag->OptFlags & CDX_TYPE_CUSTOM ) != 0 &&
                     ( pTag->OptFlags & CDX_TYPE_PARTIAL ) != 0;
   pTag->ChgOnly   = ( pTag->OptFlags & CDX_TYPE_CUSTOM ) != 0 &&
                     ( pTag->OptFlags & CDX_TYPE_PARTIAL ) == 0;
   pTag->Partial   = ( pTag->OptFlags & CDX_TYPE_CUSTOM ) != 0 ||
                     ( pTag->OptFlags & CDX_TYPE_PARTIAL ) != 0;

   pTag->Template  = pTag->Custom && zh_cdxIsTemplateFunc( pTag->KeyExpr );
   /* SIx3 does not support repeated key value for the same record */
   pTag->MultiKey  = ZH_FALSE;
#else
   pTag->Partial   = ( pTag->OptFlags & CDX_TYPE_PARTIAL ) != 0;
   pTag->Custom    = ( pTag->OptFlags & CDX_TYPE_CUSTOM ) != 0;
   pTag->ChgOnly   = ZH_FALSE;
   pTag->Template  = pTag->MultiKey = pTag->Custom;
#endif

   pTag->AscendKey = pTag->UsrAscend = ( ZH_GET_LE_UINT16( tagHeader.ascendFlg ) == 0 );
   pTag->UsrUnique = ZH_FALSE;

   if( tagHeader.indexSig == 0x01 || tagHeader.indexSig == 0x21 )
      pTag->IgnoreCase = tagHeader.ignoreCase == 1;
   else
      pTag->IgnoreCase = ZH_FALSE;

   if( pTag->OptFlags & CDX_TYPE_STRUCTURE )
      return;

   if( ! *pTag->KeyExpr || SELF_COMPILE( &pTag->pIndex->pArea->dbfarea.area, pTag->KeyExpr ) == ZH_FAILURE )
   {
      pTag->RootBlock = 0; /* To force RT error - index corrupted */
      return;
   }
   pTag->pKeyItem = pTag->pIndex->pArea->dbfarea.area.valResult;
   pTag->pIndex->pArea->dbfarea.area.valResult = NULL;

   /* go to a blank record before testing expression */
   ulRecNo = pTag->pIndex->pArea->dbfarea.ulRecNo;
   SELF_GOTO( &pTag->pIndex->pArea->dbfarea.area, 0 );

   pTag->uiType = zh_cdxItemType( zh_vmEvalBlockOrMacro( pTag->pKeyItem ) );
   pTag->bTrail = ( pTag->uiType == 'C' ) ? ' ' : '\0';
   if( pTag->uiType == 'C' )
      zh_cdxMakeSortTab( pTag->pIndex->pArea );
   else
      pTag->IgnoreCase = ZH_FALSE;

   pTag->nField = zh_rddFieldExpIndex( &pTag->pIndex->pArea->dbfarea.area,
                                       pTag->KeyExpr );

   /* Check if there is a FOR expression: pTag->OptFlags & CDX_TYPE_FORFILTER */
   if( tagHeader.keyExpPool[ uiForPos ] != 0 )
   {
      pTag->ForExpr = ( char * ) zh_xgrab( uiForLen + 1 );
      zh_strncpyTrim( pTag->ForExpr, ( const char * ) tagHeader.keyExpPool +
                      uiForPos, uiForLen );
      if( SELF_COMPILE( &pTag->pIndex->pArea->dbfarea.area, pTag->ForExpr ) == ZH_FAILURE )
         pTag->RootBlock = 0;  /* To force RT error - index corrupted */
      else
      {
         pTag->pForItem = pTag->pIndex->pArea->dbfarea.area.valResult;
         pTag->pIndex->pArea->dbfarea.area.valResult = NULL;

         /* CL52 / SIXCDX when index is open evaluates only KEY expression
          * and do not check the FOR one.
          * CL53 / COMIX evaluates both KEY and FOR expressions.
          */
#if ! defined( ZH_SIXCDX )
         if( zh_cdxItemType( zh_vmEvalBlockOrMacro( pTag->pForItem ) ) != 'L' )
         {
            zh_cdxErrorRT( pTag->pIndex->pArea, EG_DATATYPE, EDBF_INVALIDFOR,
                           NULL, 0, 0, NULL );
            pTag->RootBlock = 0; /* To force RT error - index corrupted */
         }
#endif
      }
   }
   SELF_GOTO( &pTag->pIndex->pArea->dbfarea.area, ulRecNo );

   if( pTag->uiLen > pTag->pIndex->uiMaxKeyLen || pTag->uiType == 'U' ||
       ( pTag->uiType == 'N' && pTag->uiLen != 8 && pTag->uiLen != 4 ) ||
       ( pTag->uiType == 'D' && pTag->uiLen != 8 ) ||
       ( pTag->uiType == 'T' && pTag->uiLen != 8 ) ||
       ( pTag->uiType == 'L' && pTag->uiLen != 1 ) )
   {
      zh_cdxErrorRT( pTag->pIndex->pArea,
                     pTag->uiType == 'U' ? EG_DATATYPE : EG_DATAWIDTH,
                     EDBF_INVALIDKEY, NULL, 0, 0, NULL );
      pTag->RootBlock = 0; /* To force RT error - index corrupted */
   }
}

/*
 * release structure with a tag information from memory
 */
static void zh_cdxTagFree( LPCDXTAG pTag )
{
   if( pTag->RootPage != NULL )
   {
      zh_cdxPageFree( pTag->RootPage, ZH_FALSE );
      pTag->RootPage = NULL;
   }
   zh_cdxTagPoolFlush( pTag );
   zh_cdxTagPoolFree( pTag, 0 );
   if( pTag->TagChanged )
      zh_cdxTagHeaderStore( pTag );
   if( pTag->szName != NULL )
      zh_xfree( pTag->szName );
   if( pTag->KeyExpr != NULL )
      zh_xfree( pTag->KeyExpr );
   if( pTag->pKeyItem != NULL )
      zh_vmDestroyBlockOrMacro( pTag->pKeyItem );
   if( pTag->ForExpr != NULL )
      zh_xfree( pTag->ForExpr );
   if( pTag->pForItem != NULL )
      zh_vmDestroyBlockOrMacro( pTag->pForItem );
   zh_cdxKeyFree( pTag->CurKey );
   if( pTag->HotKey )
      zh_cdxKeyFree( pTag->HotKey );
   zh_cdxTagClearScope( pTag, 0 );
   zh_cdxTagClearScope( pTag, 1 );
   zh_xfree( pTag );
}

/*
 * Creates a new structure with a tag information
 * TagHdr = offset of index page where a tag header is stored
 *            if CDX_DUMMYNODE then allocate space for a new tag header
 */
static LPCDXTAG zh_cdxTagNew( LPCDXINDEX pIndex, const char * szTagName, ZH_ULONG TagHdr )
{
   LPCDXTAG pTag;
   char szName[ CDX_MAXTAGNAMELEN + 1 ];

   pTag = ( LPCDXTAG ) zh_xgrab( sizeof( CDXTAG ) );
   memset( pTag, 0, sizeof( CDXTAG ) );
   zh_strncpyUpperTrim( szName, szTagName, sizeof( szName ) - 1 );
   pTag->szName = zh_strdup( szName );
   pTag->pIndex = pIndex;
   pTag->AscendKey = pTag->UsrAscend = ZH_TRUE;
   pTag->UsrUnique = pTag->IgnoreCase = ZH_FALSE;
   pTag->uiType = 'C';
   pTag->bTrail = ' ';
   pTag->CurKey = zh_cdxKeyNew( 0 );
   if( TagHdr == CDX_DUMMYNODE )
   {
      pTag->TagBlock = zh_cdxIndexGetAvailPage( pIndex, ZH_TRUE );
      pTag->TagChanged = ZH_TRUE;
      pTag->OptFlags = CDX_TYPE_COMPACT | CDX_TYPE_COMPOUND;
   }
   else
   {
      pTag->TagBlock = TagHdr;
      zh_cdxTagLoad( pTag );
      if( pTag->RootBlock == 0 )
      {
         /* index file is corrupted */
         zh_cdxTagFree( pTag );
         pTag = NULL;
      }
   }
   return pTag;
}

/*
 * close Tag (free used pages into page pool)
 */
static void zh_cdxTagClose( LPCDXTAG pTag )
{
   if( pTag->RootPage != NULL )
   {
      zh_cdxPageFree( pTag->RootPage, ZH_FALSE );
      pTag->RootPage = NULL;
   }
   if( pTag->TagChanged )
      zh_cdxTagHeaderStore( pTag );

   pTag->fRePos = ZH_TRUE;
}

/*
 * (re)open Tag
 */
static void zh_cdxTagOpen( LPCDXTAG pTag )
{
   CDXTAGHEADER tagHeader;

   if( ! pTag->RootPage )
   {
      zh_cdxIndexPageRead( pTag->pIndex, pTag->TagBlock,
                           ( ZH_BYTE * ) &tagHeader, sizeof( tagHeader ) );
      pTag->RootBlock = ZH_GET_LE_UINT32( tagHeader.rootPtr );
      if( pTag->RootBlock && pTag->RootBlock != CDX_DUMMYNODE )
         pTag->RootPage = zh_cdxPageNew( pTag, NULL, pTag->RootBlock );
      if( ! pTag->RootPage )
         zh_cdxErrInternal( "zh_cdxTagOpen: index corrupted" );
   }
}

/*
 * free Tag pages from cache
 */
static void zh_cdxTagPoolFree( LPCDXTAG pTag, int nPagesLeft )
{
   LPCDXPAGE pPage, pPageNext;

#ifdef ZH_CDX_DBGCODE_EXT
   zh_cdxTagPoolCheck( pTag );
#endif
   pPage = pTag->pagePool;
   while( nPagesLeft && pPage )
   {
      pPage = pPage->pPoolNext;
      nPagesLeft--;
   }
   while( pPage )
   {
      pPageNext = pPage->pPoolNext;
      if( ! pPage->bUsed )
         zh_cdxPageFree( pPage, ZH_TRUE );
      pPage = pPageNext;
   }
#ifdef ZH_CDX_DBGCODE_EXT
   zh_cdxTagPoolCheck( pTag );
#endif
}

/*
 * write all changed pages in tag cache
 */
static void zh_cdxTagPoolFlush( LPCDXTAG pTag )
{
   LPCDXPAGE pPage;

   pPage = pTag->pagePool;
   while( pPage )
   {
      if( pPage->fChanged )
      {
         zh_cdxPageStore( pPage );
      }
      pPage = pPage->pPoolNext;
   }
#ifdef ZH_CDX_DBGCODE_EXT
   zh_cdxTagPoolCheck( pTag );
#endif
}

/*
 * retrieve CurKey from current Tag position
 */
static void zh_cdxSetCurKey( LPCDXPAGE pPage )
{
   while( pPage->Child )
      pPage = pPage->Child;

   pPage->TagParent->CurKey = zh_cdxKeyPut( pPage->TagParent->CurKey,
                                 zh_cdxPageGetKeyVal( pPage, pPage->iCurKey ),
                                 pPage->TagParent->uiLen,
                                 zh_cdxPageGetKeyRec( pPage, pPage->iCurKey ) );
}

/*
 * seek given Key in the Page or in its children
 */
static int zh_cdxPageSeekKey( LPCDXPAGE pPage, LPCDXKEY pKey, ZH_ULONG ulKeyRec )
{
   int l, r, n, k;
   ZH_BOOL fLeaf = ( pPage->PageType & CDX_NODE_LEAF ) != 0;

   if( fLeaf && ! pPage->pKeyBuf && pPage->iKeys > 0 )
   {
      int iLen = pPage->TagParent->uiLen + 8;
      ZH_BYTE * pKeyBuf = ( ZH_BYTE * ) zh_xgrab( pPage->iKeys * iLen );
      zh_cdxPageLeafDecode( pPage, pKeyBuf );
      pPage->pKeyBuf = pKeyBuf;
   }

   k = ( ulKeyRec == CDX_MAX_REC_NUM ) ? -1 : 1;
   n = -1;
   l = 0;
   r = pPage->iKeys - 1;
   while( l < r )
   {
      n = ( l + r ) >> 1;
      k = zh_cdxValCompare( pPage->TagParent, pKey->val, pKey->len,
                            zh_cdxPageGetKeyVal( pPage, n ),
                            pPage->TagParent->uiLen, pKey->mode );
      if( k == 0 )
      {
         if( ulKeyRec == CDX_MAX_REC_NUM )
            k = 1;
         else if( ulKeyRec != CDX_IGNORE_REC_NUM )
         {
            ZH_ULONG ulRec = zh_cdxPageGetKeyRec( pPage, n );
            if( ulKeyRec > ulRec )
               k = 1;
            else if( ulKeyRec < ulRec )
               k = -1;
         }
      }
      if( k > 0 )
         l = n + 1;
      else
         r = n;
   }
   pPage->iCurKey = l;
   if( r < 0 )
      return k;

   if( ! fLeaf )
   {
      zh_cdxPageGetChild( pPage );
#ifdef ZH_CDX_DBGCODE
      if( memcmp( zh_cdxPageGetKeyVal( pPage, pPage->iCurKey ),
                  zh_cdxPageGetKeyVal( pPage->Child, pPage->Child->iKeys - 1 ),
                  pPage->TagParent->uiLen ) != 0 ||
          zh_cdxPageGetKeyRec( pPage, pPage->iCurKey ) !=
          zh_cdxPageGetKeyRec( pPage->Child, pPage->Child->iKeys - 1 ) )
      {
         printf( "\r\nkeyLen=%u", pPage->TagParent->uiLen );
         printf( "\r\nparent=%lx, iKey=%d, rec=%lu", pPage->Page, pPage->iCurKey, zh_cdxPageGetKeyRec( pPage, pPage->iCurKey ) );
         printf( "\r\n child=%lx, iKey=%d, rec=%lu", pPage->Child->Page, pPage->Child->iKeys - 1, zh_cdxPageGetKeyRec( pPage->Child, pPage->Child->iKeys - 1 ) );
         printf( "\r\nparent val=[%s]", zh_cdxPageGetKeyVal( pPage, pPage->iCurKey ) );
         printf( "\r\n child val=[%s]", zh_cdxPageGetKeyVal( pPage->Child, pPage->Child->iKeys - 1 ) );
         fflush( stdout );
         zh_cdxErrInternal( "zh_cdxPageSeekKey: wrong parent key." );
      }
#endif
      k = zh_cdxPageSeekKey( pPage->Child, pKey, ulKeyRec );
   }
   else if( l != n || ulKeyRec == CDX_MAX_REC_NUM )
   {
      k = zh_cdxValCompare( pPage->TagParent, pKey->val, pKey->len,
                            zh_cdxPageGetKeyVal( pPage, pPage->iCurKey ),
                            pPage->TagParent->uiLen, pKey->mode );
      if( k == 0 && ulKeyRec != CDX_MAX_REC_NUM &&
                    ulKeyRec != CDX_IGNORE_REC_NUM )
      {
         ZH_ULONG ulRec = zh_cdxPageGetKeyRec( pPage, pPage->iCurKey );
         if( ulKeyRec > ulRec )
            k = 1;
         else if( ulKeyRec < ulRec )
            k = -1;
      }
   }
   if( ulKeyRec == CDX_MAX_REC_NUM )
   {
      if( pPage->iCurKey > 0 && k < 0 )
      {
         pPage->iCurKey--;
         if( ! fLeaf )
         {
            zh_cdxPageGetChild( pPage );
            k = zh_cdxPageSeekKey( pPage->Child, pKey, ulKeyRec );
         }
         else
            k = zh_cdxValCompare( pPage->TagParent, pKey->val, pKey->len,
                                  zh_cdxPageGetKeyVal( pPage, pPage->iCurKey ),
                                  pPage->TagParent->uiLen, pKey->mode );
      }
   }
   else if( k > 0 && fLeaf )
      pPage->iCurKey++;
   return k;
}

/*
 * an interface for fast check record number in record filter
 */
static ZH_BOOL zh_cdxCheckRecordScope( CDXAREAP pArea, ZH_ULONG ulRec )
{
   ZH_LONG lRecNo = ( ZH_LONG ) ulRec;

   if( SELF_COUNTSCOPE( &pArea->dbfarea.area, NULL, &lRecNo ) == ZH_SUCCESS && lRecNo == 0 )
   {
      return ZH_FALSE;
   }
   return ZH_TRUE;
}

/*
 * check and evaluate record filter
 */
static ZH_BOOL zh_cdxCheckRecordFilter( CDXAREAP pArea, ZH_ULONG ulRecNo )
{
   ZH_BOOL lResult = ZH_FALSE;
   ZH_BOOL fDeleted = zh_setGetDeleted();

   if( pArea->dbfarea.area.dbfi.itmCobExpr || fDeleted )
   {
      if( pArea->dbfarea.ulRecNo != ulRecNo || pArea->dbfarea.lpdbPendingRel )
         SELF_GOTO( &pArea->dbfarea.area, ulRecNo );

      if( fDeleted )
         SELF_DELETED( &pArea->dbfarea.area, &lResult );

      if( ! lResult && pArea->dbfarea.area.dbfi.itmCobExpr )
      {
         PZH_ITEM pResult = zh_vmEvalBlock( pArea->dbfarea.area.dbfi.itmCobExpr );
         lResult = ZH_IS_LOGICAL( pResult ) && ! zh_itemGetL( pResult );
      }
   }
   return ! lResult;
}

/*
 * read Top Key from Page or its children
 */
static ZH_BOOL zh_cdxPageReadTopKey( LPCDXPAGE pPage )
{
   while( ( pPage->PageType & CDX_NODE_LEAF ) == 0 && pPage->iKeys > 0 )
   {
      pPage->iCurKey = 0;
      zh_cdxPageGetChild( pPage );
      pPage = pPage->Child;
   }
   if( pPage->iKeys == 0 )
      return ZH_FALSE;
   pPage->iCurKey = 0;

   zh_cdxSetCurKey( pPage );
   return ZH_TRUE;
}

/*
 * read Bottom Key from Page or its children
 */
static ZH_BOOL zh_cdxPageReadBottomKey( LPCDXPAGE pPage )
{
   while( ( pPage->PageType & CDX_NODE_LEAF ) == 0 && pPage->iKeys > 0 )
   {
      pPage->iCurKey = pPage->iKeys - 1;
      zh_cdxPageGetChild( pPage );
      pPage = pPage->Child;
   }
   if( pPage->iKeys == 0 )
      return ZH_FALSE;
   pPage->iCurKey = pPage->iKeys - 1;

   zh_cdxSetCurKey( pPage );
   return ZH_TRUE;
}

/*
 * read Previous Key from Page or its children
 */
static ZH_BOOL zh_cdxPageReadPrevKey( LPCDXPAGE pPage )
{
   LPCDXPAGE pOwnerPage = NULL;

   while( pPage->Child )
   {
      pOwnerPage = pPage;
      pPage = pPage->Child;
   }

   do
   {
      pPage->iCurKey--;
      while( pPage->iCurKey < 0 )
      {
         if( pPage->Left == CDX_DUMMYNODE || ! pOwnerPage )
         {
            pPage->iCurKey = 0;
            if( pPage->iKeys > 0 )
               zh_cdxSetCurKey( pPage );
            return ZH_FALSE;
         }
         pOwnerPage->Child = zh_cdxPageNew( pPage->TagParent, pPage->Owner, pPage->Left );
         zh_cdxPageFree( pPage, ! pPage->fChanged );
         pPage = pOwnerPage->Child;
         pPage->iCurKey = pPage->iKeys - 1;
      }
      if( pPage->iCurKey == 0 )
      {
         zh_cdxSetCurKey( pPage );
         if( ! zh_cdxTopScope( pPage->TagParent ) ||
             ! zh_cdxBottomScope( pPage->TagParent ) )
            break;
      }
   }
   while( ( pPage->TagParent->OptFlags & CDX_TYPE_STRUCTURE ) == 0 &&
          ! zh_cdxCheckRecordScope( pPage->TagParent->pIndex->pArea,
                                    zh_cdxPageGetKeyRec( pPage, pPage->iCurKey ) ) );
   if( pPage->iCurKey != 0 )
      zh_cdxSetCurKey( pPage );
   return ZH_TRUE;
}

/*
 * read Next Key from Page or its children
 */
static ZH_BOOL zh_cdxPageReadNextKey( LPCDXPAGE pPage )
{
   LPCDXPAGE pOwnerPage = NULL;

   while( pPage->Child )
   {
      pOwnerPage = pPage;
      pPage = pPage->Child;
   }

   do
   {
      pPage->iCurKey++;
      while( pPage->iCurKey >= pPage->iKeys )
      {
         if( pPage->Right == CDX_DUMMYNODE || ! pOwnerPage )
         {
            pPage->iCurKey = pPage->iKeys;
            return ZH_FALSE;
         }
         pOwnerPage->Child = zh_cdxPageNew( pPage->TagParent, pPage->Owner, pPage->Right );
         zh_cdxPageFree( pPage, ! pPage->fChanged );
         pPage = pOwnerPage->Child;
         pPage->iCurKey = 0;
      }
      if( pPage->iCurKey == 0 )
      {
         zh_cdxSetCurKey( pPage );
         if( ! zh_cdxTopScope( pPage->TagParent ) ||
             ! zh_cdxBottomScope( pPage->TagParent ) )
            break;
      }
   }
   while( ( pPage->TagParent->OptFlags & CDX_TYPE_STRUCTURE ) == 0 &&
          ! zh_cdxCheckRecordScope( pPage->TagParent->pIndex->pArea,
                                    zh_cdxPageGetKeyRec( pPage, pPage->iCurKey ) ) );
   if( pPage->iCurKey != 0 )
      zh_cdxSetCurKey( pPage );
   return ZH_TRUE;
}

/*
 * read Previous Unique Key from Page or its children
 */
static ZH_BOOL zh_cdxPageReadPrevUniqKey( LPCDXPAGE pPage )
{
   LPCDXPAGE pOwnerPage = NULL;

   while( pPage->Child )
   {
      pOwnerPage = pPage;
      pPage = pPage->Child;
   }
   while( pPage->iCurKey < 0 || memcmp( pPage->TagParent->CurKey->val, zh_cdxPageGetKeyVal( pPage, pPage->iCurKey ), pPage->TagParent->uiLen ) == 0 )
   {
      if( pPage->iCurKey > 0 )
         pPage->iCurKey--;
      else
      {
         if( pPage->Left == CDX_DUMMYNODE || ! pOwnerPage )
         {
            pPage->iCurKey = 0;
            if( pPage->iKeys > 0 )
               zh_cdxSetCurKey( pPage );
            return ZH_FALSE;
         }
         pOwnerPage->Child = zh_cdxPageNew( pPage->TagParent, pPage->Owner, pPage->Left );
         zh_cdxPageFree( pPage, ! pPage->fChanged );
         pPage = pOwnerPage->Child;
         pPage->iCurKey = pPage->iKeys - 1;
      }
   }

   zh_cdxSetCurKey( pPage );
   return ZH_TRUE;
}

/*
 * read Next Unique Key from Page or its children
 */
static ZH_BOOL zh_cdxPageReadNextUniqKey( LPCDXPAGE pPage )
{
   LPCDXPAGE pOwnerPage = NULL;

   while( pPage->Child )
   {
      pOwnerPage = pPage;
      pPage = pPage->Child;
   }

   while( pPage->iCurKey >= pPage->iKeys || memcmp( pPage->TagParent->CurKey->val, zh_cdxPageGetKeyVal( pPage, pPage->iCurKey ), pPage->TagParent->uiLen ) == 0 )
   {
      if( pPage->iCurKey < pPage->iKeys - 1 )
         pPage->iCurKey++;
      else
      {
         if( pPage->Right == CDX_DUMMYNODE || ! pOwnerPage )
         {
            pPage->iCurKey = pPage->iKeys - 1;
            if( pPage->iKeys > 0 )
               zh_cdxSetCurKey( pPage );
            return ZH_FALSE;
         }
         pOwnerPage->Child = zh_cdxPageNew( pPage->TagParent, pPage->Owner, pPage->Right );
         zh_cdxPageFree( pPage, ! pPage->fChanged );
         pPage = pOwnerPage->Child;
         pPage->iCurKey = 0;
      }
   }
   zh_cdxSetCurKey( pPage );
   return ZH_TRUE;
}

/*
 * read the TOP/BOTTOM/NEXT/PREVIOUS Key from Tag
 */
static void zh_cdxTagKeyRead( LPCDXTAG pTag, ZH_BYTE bTypRead )
{
   ZH_BOOL fAfter = ZH_FALSE, fBof, fEof;

   pTag->CurKey->rec = 0;
   pTag->fRePos = ZH_FALSE;
   zh_cdxTagOpen( pTag );

   if( pTag->UsrUnique )
   {
      switch( bTypRead )
      {
         case NEXT_RECORD:
            bTypRead = NXTU_RECORD;
            break;

         case PREV_RECORD:
            bTypRead = PRVU_RECORD;
            /* fallthrough */
         case BTTM_RECORD:
            fAfter = ZH_TRUE;
            break;
      }
   }
   if( pTag->UsrAscend )
   {
      fBof = pTag->TagBOF;
      fEof = pTag->TagEOF;
   }
   else
   {
      fBof = pTag->TagEOF;
      fEof = pTag->TagBOF;
      switch( bTypRead )
      {
         case TOP_RECORD:
            bTypRead = BTTM_RECORD;
            break;

         case BTTM_RECORD:
            bTypRead = TOP_RECORD;
            break;

         case PREV_RECORD:
            bTypRead = NEXT_RECORD;
            break;

         case NEXT_RECORD:
            bTypRead = PREV_RECORD;
            break;

         case PRVU_RECORD:
            bTypRead = NXTU_RECORD;
            break;

         case NXTU_RECORD:
            bTypRead = PRVU_RECORD;
            break;
      }
   }
   switch( bTypRead )
   {
      case TOP_RECORD:
         fBof = fEof = ! zh_cdxPageReadTopKey( pTag->RootPage );
         break;

      case BTTM_RECORD:
         fBof = fEof = ! zh_cdxPageReadBottomKey( pTag->RootPage );
         break;

      case PREV_RECORD:
         if( ! fBof )
            fBof = ! zh_cdxPageReadPrevKey( pTag->RootPage );
         break;

      case NEXT_RECORD:
         if( ! fEof )
            fEof = ! zh_cdxPageReadNextKey( pTag->RootPage );
         break;

      case PRVU_RECORD:
         if( ! fBof )
            fBof = ! zh_cdxPageReadPrevUniqKey( pTag->RootPage );
         break;

      case NXTU_RECORD:
         if( ! fEof )
            fEof = ! zh_cdxPageReadNextUniqKey( pTag->RootPage );
         break;
   }

   if( fEof )
      pTag->CurKey->rec = 0;
   else if( fAfter && ! fBof )
   {
      if( pTag->UsrAscend )
      {
         if( zh_cdxPageReadPrevUniqKey( pTag->RootPage ) )
            zh_cdxPageReadNextKey( pTag->RootPage );
      }
      else
      {
         if( zh_cdxPageReadNextUniqKey( pTag->RootPage ) )
            zh_cdxPageReadPrevKey( pTag->RootPage );
      }
   }

   if( pTag->UsrAscend )
   {
      pTag->TagBOF = fBof;
      pTag->TagEOF = fEof;
   }
   else
   {
      pTag->TagBOF = fEof;
      pTag->TagEOF = fBof;
   }
}

/*
 * find pKey in pTag return 0 or TagNO
 */
static ZH_ULONG zh_cdxTagKeyFind( LPCDXTAG pTag, LPCDXKEY pKey )
{
   int K;
   ZH_ULONG ulKeyRec = pKey->rec;

   pTag->fRePos = ZH_FALSE;
   zh_cdxTagOpen( pTag );

   pTag->TagBOF = pTag->TagEOF = ZH_FALSE;
   K = zh_cdxPageSeekKey( pTag->RootPage, pKey, ulKeyRec );
   if( ulKeyRec == CDX_MAX_REC_NUM )
      K = -K;

   if( K > 0 )
   {
      pTag->CurKey->rec = 0;
      pTag->TagEOF = ZH_TRUE;
   }
   else
   {
      zh_cdxSetCurKey( pTag->RootPage );
      if( K == 0 )
         return pTag->CurKey->rec;
   }
   return 0;
}

#if 0
/*
 * find pKey in pTag return 0 or record number, respect descend/unique flags
 */
static ZH_ULONG zh_cdxTagKeySeek( LPCDXTAG pTag, LPCDXKEY pKey )
{
   int K;
   ZH_ULONG ulKeyRec = pKey->rec;

   if( pTag->UsrUnique )
   {
      if( pTag->UsrAscend )
      {
         if( ulKeyRec == CDX_MAX_REC_NUM )
            ulKeyRec = CDX_IGNORE_REC_NUM;
      }
      else if( ulKeyRec == CDX_IGNORE_REC_NUM )
         ulKeyRec = CDX_MAX_REC_NUM;
   }
   else if( ! pTag->UsrAscend )
   {
      if( ulKeyRec == CDX_MAX_REC_NUM )
         ulKeyRec = CDX_IGNORE_REC_NUM;
      else if( ulKeyRec == CDX_IGNORE_REC_NUM )
         ulKeyRec = CDX_MAX_REC_NUM;
   }

   pTag->CurKey->rec = 0;
   pTag->fRePos = ZH_FALSE;
   zh_cdxTagOpen( pTag );

   pTag->TagBOF = pTag->TagEOF = ZH_FALSE;
   K = zh_cdxPageSeekKey( pTag->RootPage, pKey, ulKeyRec );
   if( ulKeyRec == CDX_MAX_REC_NUM )
      K = -K;

   if( K > 0 )
      pTag->TagEOF = ZH_TRUE;
   else
   {
      zh_cdxSetCurKey( pTag->RootPage );
      if( K == 0 )
         return pTag->CurKey->rec;
   }
   return 0;
}
#endif

/*
 * add the Key into the Tag
 */
static ZH_BOOL zh_cdxTagKeyAdd( LPCDXTAG pTag, LPCDXKEY pKey )
{
   zh_cdxTagOpen( pTag );
   if( zh_cdxPageSeekKey( pTag->RootPage, pKey,
                          pTag->UniqueKey ? CDX_IGNORE_REC_NUM : pKey->rec ) != 0 ||
       ( pTag->Custom && pTag->MultiKey && ! pTag->UniqueKey ) )
   {
      zh_cdxPageKeyInsert( pTag->RootPage, pKey );
      pTag->curKeyState &= ~( CDX_CURKEY_RAWPOS | CDX_CURKEY_LOGPOS |
                              CDX_CURKEY_RAWCNT | CDX_CURKEY_LOGCNT );
      pTag->fRePos = ZH_TRUE;
      /* TODO: !!! remove when page leaf balance can save CurKey */
      zh_cdxTagKeyFind( pTag, pKey );
      return ZH_TRUE;
   }
   return ZH_FALSE;
}

/*
 * delete the Key from the Tag
 */
static ZH_BOOL zh_cdxTagKeyDel( LPCDXTAG pTag, LPCDXKEY pKey )
{
   if( zh_cdxTagKeyFind( pTag, pKey ) != 0 )
   {
      zh_cdxPageKeyRemove( pTag->RootPage );
      pTag->curKeyState &= ~( CDX_CURKEY_RAWPOS | CDX_CURKEY_LOGPOS |
                              CDX_CURKEY_RAWCNT | CDX_CURKEY_LOGCNT );
      pTag->CurKey->rec = 0;
      return ZH_TRUE;
   }
   return ZH_FALSE;
}

/*
 * Go to the first visible record in Tag
 */
static void zh_cdxTagGoTop( LPCDXTAG pTag )
{
   LPCDXKEY pKey = pTag->UsrAscend ? pTag->topScopeKey : pTag->bottomScopeKey;
   ZH_ULONG ulPos = 1;

   if( pKey )
      zh_cdxTagKeyFind( pTag, pKey );
   else
      zh_cdxTagKeyRead( pTag, TOP_RECORD );

   for( ;; )
   {
      if( pTag->CurKey->rec == 0 || pTag->TagEOF || ! zh_cdxBottomScope( pTag ) )
      {
         pTag->TagBOF = pTag->TagEOF = ZH_TRUE;
         pTag->CurKey->rec = 0;
         break;
      }
      else if( ( pTag->OptFlags & CDX_TYPE_STRUCTURE ) != 0 ||
               zh_cdxCheckRecordScope( pTag->pIndex->pArea, pTag->CurKey->rec ) )
      {
         pTag->rawKeyPos = ulPos;
         CURKEY_SETRAWPOS( pTag );
         break;
      }
      zh_cdxTagKeyRead( pTag, NEXT_RECORD );
      ulPos++;
   }
}

/*
 * Go to the last visible record in Tag
 */
static void zh_cdxTagGoBottom( LPCDXTAG pTag )
{
   LPCDXKEY pKey = pTag->UsrAscend ? pTag->bottomScopeKey : pTag->topScopeKey;
   ZH_ULONG ulPos = 0;

   if( pKey )
      zh_cdxTagKeyFind( pTag, pKey );
   else
      zh_cdxTagKeyRead( pTag, BTTM_RECORD );

   for( ;; )
   {
      if( pTag->CurKey->rec == 0 || pTag->TagBOF || ! zh_cdxTopScope( pTag ) )
      {
         pTag->TagBOF = pTag->TagEOF = ZH_TRUE;
         pTag->CurKey->rec = 0;
         break;
      }
      else if( ( pTag->OptFlags & CDX_TYPE_STRUCTURE ) != 0 ||
               zh_cdxCheckRecordScope( pTag->pIndex->pArea, pTag->CurKey->rec ) )
      {
         if( CURKEY_RAWCNT( pTag ) )
         {
            pTag->rawKeyPos = pTag->rawKeyCount - ulPos;
            CURKEY_SETRAWPOS( pTag );
         }
         break;
      }
      zh_cdxTagKeyRead( pTag, PREV_RECORD );
      ulPos++;
   }
}

/*
 * skip to Next Key in the Tag
 */
static void zh_cdxTagSkipNext( LPCDXTAG pTag )
{
   ZH_BOOL fPos = CURKEY_RAWPOS( pTag ), fEof = ZH_FALSE;
   ZH_ULONG ulSkip = 1;

   if( pTag->CurKey->rec != 0 )
   {
      if( ! zh_cdxTopScope( pTag ) )
      {
         ulSkip = 0;
         zh_cdxTagGoTop( pTag );
      }
      else
         zh_cdxTagKeyRead( pTag, NEXT_RECORD );
   }

   while( ! fEof )
   {
      if( pTag->TagEOF || pTag->CurKey->rec == 0 ||
          ! zh_cdxBottomScope( pTag ) || ! zh_cdxTopScope( pTag ) )
         fEof = ZH_TRUE;
      else if( ( pTag->OptFlags & CDX_TYPE_STRUCTURE ) != 0 ||
               zh_cdxCheckRecordScope( pTag->pIndex->pArea, pTag->CurKey->rec ) )
         break;
      zh_cdxTagKeyRead( pTag, NEXT_RECORD );
      ulSkip++;
   }

   if( fEof )
   {
      pTag->CurKey->rec = 0;
      pTag->TagEOF = ZH_TRUE;
   }
   else if( fPos )
   {
      pTag->rawKeyPos += ulSkip;
      CURKEY_SETRAWPOS( pTag );
   }
}

/*
 * skip to Previous Key in the Tag
 */
static void zh_cdxTagSkipPrev( LPCDXTAG pTag )
{
   ZH_BOOL fPos = CURKEY_RAWPOS( pTag ), fBof = ZH_FALSE;
   ZH_ULONG ulSkip = 1;

   if( pTag->CurKey->rec == 0 )
   {
      ulSkip = 0;
      zh_cdxTagGoBottom( pTag );
   }
   else
      zh_cdxTagKeyRead( pTag, PREV_RECORD );

   while( ! fBof )
   {
      if( pTag->TagBOF || pTag->CurKey->rec == 0 ||
          ! zh_cdxBottomScope( pTag ) || ! zh_cdxTopScope( pTag ) )
         fBof = ZH_TRUE;
      else if( ( pTag->OptFlags & CDX_TYPE_STRUCTURE ) != 0 ||
               zh_cdxCheckRecordScope( pTag->pIndex->pArea, pTag->CurKey->rec ) )
         break;
      zh_cdxTagKeyRead( pTag, PREV_RECORD );
      ulSkip++;
   }


   if( fBof )
   {
      zh_cdxTagGoTop( pTag );
      pTag->TagBOF = ZH_TRUE;
   }
   else if( fPos )
   {
      pTag->rawKeyPos -= ulSkip;
      CURKEY_SETRAWPOS( pTag );
   }
}

static void zh_cdxReorderTagList( LPCDXTAG * TagListPtr )
{
   ZH_BOOL fRepeat = ZH_TRUE;

   while( fRepeat )
   {
      LPCDXTAG * pTagPtr, pTagTmp;

      fRepeat = ZH_FALSE;
      pTagPtr = TagListPtr;
      while( *pTagPtr && ( *pTagPtr )->pNext )
      {
         if( ( *pTagPtr )->TagBlock > ( *pTagPtr )->pNext->TagBlock )
         {
            pTagTmp             = ( *pTagPtr );
            ( *pTagPtr )        = ( *pTagPtr )->pNext;
            pTagTmp->pNext      = ( *pTagPtr )->pNext;
            ( *pTagPtr )->pNext = pTagTmp;
            fRepeat             = ZH_TRUE;
         }
         pTagPtr = &( *pTagPtr )->pNext;
      }
   }
}

/*
 * create new order header, store it and then make an order
 */
static LPCDXTAG zh_cdxIndexCreateTag( ZH_BOOL fStruct, LPCDXINDEX pIndex,
                                      const char * szTagName,
                                      const char * szKeyExp, PZH_ITEM pKeyItem,
                                      ZH_BYTE bType, ZH_USHORT uiLen,
                                      const char * szForExp, PZH_ITEM pForItem,
                                      ZH_BOOL fAscnd, ZH_BOOL fUniq, ZH_BOOL fNoCase,
                                      ZH_BOOL fCustom, ZH_BOOL fReindex )
{
   LPCDXTAG pTag;

   pTag = zh_cdxTagNew( pIndex, szTagName, CDX_DUMMYNODE );

   if( fStruct )
      pTag->OptFlags |= CDX_TYPE_STRUCTURE;

   if( bType == 'C' )
      zh_cdxMakeSortTab( pTag->pIndex->pArea );

   if( szKeyExp != NULL )
   {
      pTag->KeyExpr = zh_strduptrim( szKeyExp );
      pTag->nField = zh_rddFieldExpIndex( &pTag->pIndex->pArea->dbfarea.area,
                                          pTag->KeyExpr );
   }
   pTag->pKeyItem = pKeyItem;
   if( szForExp != NULL )
      pTag->ForExpr = zh_strduptrim( szForExp );

   pTag->pForItem = pForItem;
   pTag->AscendKey = pTag->UsrAscend = fAscnd;
   pTag->UniqueKey = fUniq;
   pTag->UsrUnique = ZH_FALSE;
   pTag->IgnoreCase = fNoCase && bType == 'C';
   pTag->Custom = fCustom;

#if defined( ZH_SIXCDX )
   pTag->Template  = pTag->KeyExpr && zh_cdxIsTemplateFunc( pTag->KeyExpr );
   if( pTag->Template )
      pTag->Custom = ZH_TRUE;
   /* SIx3 does not support repeated key value for the same record */
   pTag->MultiKey  = ZH_FALSE;
#else
   pTag->Template = pTag->MultiKey = pTag->Custom;
#endif
   pTag->Partial = pTag->ChgOnly = ZH_FALSE;
   pTag->uiType = bType;
   pTag->bTrail = ( bType == 'C' ) ? ' ' : '\0';
   pTag->uiLen = uiLen;
   pTag->MaxKeys = ( pIndex->uiPageLen - CDX_INT_HEADSIZE ) / ( uiLen + 8 );
   pTag->TagChanged = ZH_TRUE;
   zh_cdxTagDoIndex( pTag, fReindex );

   return pTag;
}

/*
 * create structural (compound) tag
 */
static void zh_cdxIndexCreateStruct( LPCDXINDEX pIndex, char * szTagName )
{
   /* here we can change default tag name */
   pIndex->pCompound = zh_cdxIndexCreateTag( ZH_TRUE, pIndex, szTagName,
                           NULL, NULL, 'C', CDX_MAXTAGNAMELEN, NULL, NULL,
                           ZH_TRUE, ZH_FALSE, ZH_FALSE, ZH_FALSE, ZH_FALSE );
}

/*
 * free page and all child pages
 */
static void zh_cdxIndexFreePages( LPCDXPAGE pPage )
{
   if( ( pPage->PageType & CDX_NODE_LEAF ) == 0 )
   {
      int iKey;

      for( iKey = 0; iKey < pPage->iKeys; iKey++ )
      {
         LPCDXPAGE pChildPage = zh_cdxPageNew( pPage->TagParent, NULL,
                                               zh_cdxPageGetKeyPage( pPage, iKey ) );
         if( pChildPage )
            zh_cdxIndexFreePages( pChildPage );
      }
   }
   pPage->PageType = CDX_NODE_UNUSED;
   zh_cdxPageFree( pPage, ZH_FALSE );
}

/*
 * remove Tag from Bag
 */
static void zh_cdxIndexDelTag( LPCDXINDEX pIndex, const char * szTagName )
{
   LPCDXTAG * pTagPtr = &pIndex->TagList;

   while( *pTagPtr && zh_stricmp( ( *pTagPtr )->szName, szTagName ) != 0 )
      pTagPtr = &( *pTagPtr )->pNext;

   if( *pTagPtr )
   {
      LPCDXTAG pTag = *pTagPtr;
      LPCDXKEY pKey = zh_cdxKeyPutCL( NULL, pTag->szName, strlen( pTag->szName ),
                                      pTag->TagBlock, pIndex->pCompound->uiLen,
                                      CDX_CMP_EXACT );
      if( zh_cdxTagKeyDel( pIndex->pCompound, pKey ) )
      {
         if( pTag != pIndex->TagList || pTag->pNext != NULL )
         {
            LPCDXPAGE pPage;

            zh_cdxTagOpen( pTag );
            pPage = pTag->RootPage;
            zh_cdxTagClose( pTag );
            if( ! pIndex->fShared )
            {
               if( pPage )
                  zh_cdxIndexFreePages( pPage );
               zh_cdxIndexPutAvailPage( pIndex, pTag->TagBlock, ZH_TRUE );
            }
            pTag->TagChanged = ZH_FALSE;
         }
      }
      *pTagPtr = pTag->pNext;
      zh_cdxTagFree( pTag );
      zh_cdxKeyFree( pKey );
   }
}

/*
 * add tag to order bag
 */
static LPCDXTAG zh_cdxIndexAddTag( LPCDXINDEX pIndex, const char * szTagName,
                                   const char * szKeyExp, PZH_ITEM pKeyItem,
                                   ZH_BYTE bType, ZH_USHORT uiLen,
                                   const char * szForExp, PZH_ITEM pForItem,
                                   ZH_BOOL fAscend, ZH_BOOL fUnique, ZH_BOOL fNoCase,
                                   ZH_BOOL fCustom, ZH_BOOL fReindex )
{
   LPCDXTAG pTag, * pTagPtr;
   LPCDXKEY pKey;

   /* Delete previous tag first to free the place for new one
    * its redundant Tag should be already deleted
    */
   zh_cdxIndexDelTag( pIndex, szTagName );

   /* Create new tag an add to tag list */
   pTag = zh_cdxIndexCreateTag( ZH_FALSE, pIndex, szTagName, szKeyExp, pKeyItem,
                                bType, uiLen, szForExp, pForItem,
                                fAscend, fUnique, fNoCase, fCustom, fReindex );
   pTagPtr = &pIndex->TagList;
   while( *pTagPtr )
      pTagPtr = &( *pTagPtr )->pNext;
   *pTagPtr = pTag;
   pKey = zh_cdxKeyPutCL( NULL, szTagName, strlen( szTagName ),
                          pTag->TagBlock, pIndex->pCompound->uiLen,
                          CDX_CMP_EXACT );
   zh_cdxTagKeyAdd( pIndex->pCompound, pKey );
   zh_cdxKeyFree( pKey );
   return pTag;
}

/*
 * rebuild from scratch all orders in index file
 */
static void zh_cdxIndexReindex( LPCDXINDEX pIndex )
{
   LPCDXTAG pCompound, pTagList, pTag;

   zh_cdxIndexLockWrite( pIndex );
   zh_cdxIndexLockFlush( pIndex );
   zh_cdxIndexDiscardBuffers( pIndex );

   pCompound = pIndex->pCompound;
   pTagList = pIndex->TagList;
   pIndex->pCompound = NULL;
   pIndex->TagList = NULL;

   pIndex->ulVersion = 0;
   pIndex->nextAvail = 0;
   pIndex->freePage = 0;
   zh_fileTruncAt( pIndex->pFile, 0 );
   pIndex->fChanged = ZH_TRUE;

   /* Rebuild the compound (master) tag */
   if( pCompound )
   {
      zh_cdxIndexCreateStruct( pIndex, pCompound->szName );
      zh_cdxTagFree( pCompound );
   }

   /* Rebuild each tag */
   while( pTagList )
   {
      pTag = pTagList;
      zh_cdxIndexAddTag( pIndex, pTag->szName, pTag->KeyExpr, pTag->pKeyItem,
         ( ZH_BYTE ) pTag->uiType, pTag->uiLen, pTag->ForExpr, pTag->pForItem,
         pTag->AscendKey, pTag->UniqueKey, pTag->IgnoreCase, pTag->Custom, ZH_TRUE );
      pTagList = pTag->pNext;
      pTag->pKeyItem = pTag->pForItem = NULL;
      zh_cdxTagFree( pTag );
   }
   zh_cdxIndexUnLockWrite( pIndex );
}

static void zh_cdxIndexInit( LPCDXINDEX pIndex )
{
   ZH_USHORT uiPageSize = DBFAREA_DATA( &pIndex->pArea->dbfarea )->uiIndexPageSize;
   ZH_BOOL fLargeFile;

   if( uiPageSize < CDX_PAGELEN )
      uiPageSize = CDX_PAGELEN;
   else if( uiPageSize > CDX_PAGELEN_MAX )
      uiPageSize = CDX_PAGELEN_MAX;

   fLargeFile = uiPageSize > CDX_PAGELEN ||
                pIndex->pArea->dbfarea.bLockType == DB_DBFLOCK_HB64;
   zh_cdxSetPageSize( pIndex, fLargeFile, uiPageSize, CDX_HEADERLEN );
}

/*
 * create new index structure
 */
static LPCDXINDEX zh_cdxIndexNew( CDXAREAP pArea )
{
   LPCDXINDEX pIndex;

   pIndex = ( LPCDXINDEX ) zh_xgrab( sizeof( CDXINDEX ) );
   memset( pIndex, 0, sizeof( CDXINDEX ) );
   pIndex->pArea = pArea;
   zh_cdxIndexInit( pIndex );

   return pIndex;
}

/*
 * free (close) all tag in index file
 */
static void zh_cdxIndexFreeTags( LPCDXINDEX pIndex )
{
   LPCDXTAG pTag;

   /* Free Compound tag */
   if( pIndex->pCompound != NULL )
   {
      zh_cdxTagFree( pIndex->pCompound );
      pIndex->pCompound = NULL;
   }

   while( pIndex->TagList )
   {
      pTag = pIndex->TagList;
      pIndex->TagList = pTag->pNext;
      zh_cdxTagFree( pTag );
   }
}

/*
 * free (close) index and all tags in it
 */
static void zh_cdxIndexFree( LPCDXINDEX pIndex )
{
   /* Free List of Free Pages */
   zh_cdxIndexDropAvailPage( pIndex );

   /* free all tags */
   zh_cdxIndexFreeTags( pIndex );

   /* Close file */
   if( pIndex->pFile )
   {
      zh_fileClose( pIndex->pFile );
      if( pIndex->fDelete )
         zh_fileDelete( pIndex->szRealName ? pIndex->szRealName : pIndex->szFileName );
   }

#ifdef ZH_CDX_DBGCODE
   if( pIndex->fShared && ( pIndex->lockWrite || pIndex->lockRead ) &&
       zh_vmRequestQuery() == 0 )
      zh_errInternal( 9104, "zh_cdxIndexFree: index file still locked.", NULL, NULL );

   if( ( pIndex->WrLck || pIndex->RdLck ) &&
       zh_vmRequestQuery() == 0 )
      zh_errInternal( 9104, "zh_cdxIndexFree: index file still locked (*)", NULL, NULL );
#endif

   if( pIndex->szFileName != NULL )
      zh_xfree( pIndex->szFileName );
   if( pIndex->szRealName )
      zh_xfree( pIndex->szRealName );

   zh_xfree( pIndex );
}

/*
 * load orders from index file
 */
static ZH_BOOL zh_cdxIndexLoad( LPCDXINDEX pIndex, char * szBaseName )
{
   LPCDXTAG TagList, * pTagPtr;
   ZH_BOOL fResult = ZH_FALSE;

   TagList = NULL;
   pTagPtr = &TagList;

   zh_cdxIndexLockRead( pIndex );
   /* load the tags*/
   pIndex->pCompound = zh_cdxTagNew( pIndex, szBaseName, 0L );

   /* check if index is not corrupted */
   if( pIndex->pCompound )
   {
      pIndex->pCompound->OptFlags = CDX_TYPE_COMPACT | CDX_TYPE_COMPOUND | CDX_TYPE_STRUCTURE;
      zh_cdxTagGoTop( pIndex->pCompound );
      while( ! pIndex->pCompound->TagEOF )
      {
         *pTagPtr = zh_cdxTagNew( pIndex, ( char * ) pIndex->pCompound->CurKey->val,
                                  pIndex->pCompound->CurKey->rec );
         /* tag is corrupted - break tags loading */
         if( *pTagPtr == NULL )
         {
            fResult = ZH_FALSE;
            break;
         }
         fResult = ZH_TRUE;
         pTagPtr = &( *pTagPtr )->pNext;
         zh_cdxTagSkipNext( pIndex->pCompound );
      }
   }

   zh_cdxIndexUnLockRead( pIndex );
   zh_cdxReorderTagList( &TagList );
   pTagPtr = &pIndex->TagList;
   while( *pTagPtr != NULL )
      pTagPtr = &( *pTagPtr )->pNext;
   ( *pTagPtr ) = TagList;

#ifdef ZH_CDX_DSPDBG_INFO
   zh_cdxDspTags( pIndex );
#endif

   return fResult;
}

/*
 * create index file name
 */
static void zh_cdxCreateFName( CDXAREAP pArea, const char * szBagName,
                               ZH_BOOL * fProd,
                               char * szFileName, char * szBaseName )
{
   PZH_FNAME pFileName;
   PZH_ITEM pExt = NULL;
   ZH_BOOL fName = szBagName && *szBagName;

   pFileName = zh_fsFNameSplit( fName ? szBagName : pArea->dbfarea.szDataFileName );

   if( szBaseName )
   {
      if( pFileName->szName )
         zh_strncpyUpperTrim( szBaseName, pFileName->szName, CDX_MAXTAGNAMELEN );
      else
         szBaseName[ 0 ] = '\0';
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
 * free (close) used indexes, if not fAll then keep structure index
 */
static void zh_cdxOrdListClear( CDXAREAP pArea, ZH_BOOL fAll, LPCDXINDEX pKeepInd )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_cdxOrdListClear(%p, %d)", ( void * ) pArea, ( int ) fAll ) );

   if( pArea->lpIndexes )
   {
      LPCDXINDEX pIndex, * pIndexPtr;

      if( ! fAll )
      {
         /* TODO: we have to control this on open */
         PZH_FNAME pFileNameDbf, pFileNameCdx;
         pFileNameDbf = zh_fsFNameSplit( pArea->dbfarea.szDataFileName );
         pFileNameCdx = zh_fsFNameSplit( pArea->lpIndexes->szFileName );
         fAll = zh_stricmp( pFileNameDbf->szName ? pFileNameDbf->szName : "",
                            pFileNameCdx->szName ? pFileNameCdx->szName : "" ) != 0;
         if( ! fAll )
         {
            DBORDERINFO pExtInfo;
            PZH_ITEM pExt;

            memset( &pExtInfo, 0, sizeof( pExtInfo ) );
            pExt = pExtInfo.itmResult = zh_itemPutC( NULL, NULL );
            if( SELF_ORDINFO( &pArea->dbfarea.area, DBOI_BAGEXT, &pExtInfo ) == ZH_SUCCESS )
            {
               fAll = zh_stricmp( pFileNameCdx->szExtension,
                                  zh_itemGetCPtr( pExt ) ) != 0;
            }
            zh_itemRelease( pExt );
         }
         zh_xfree( pFileNameDbf );
         zh_xfree( pFileNameCdx );
      }
      pIndexPtr = fAll ? &pArea->lpIndexes : &pArea->lpIndexes->pNext;
      while( *pIndexPtr )
      {
         pIndex = *pIndexPtr;
         if( pKeepInd == pIndex )
            pIndexPtr = &pIndex->pNext;
         else
         {
            *pIndexPtr = pIndex->pNext;
            zh_cdxIndexFree( pIndex );
         }
      }
   }
}


/*
 * find order bag by its name
 */
static LPCDXINDEX zh_cdxFindBag( CDXAREAP pArea, const char * szBagName )
{
   LPCDXINDEX pIndex;
   PZH_FNAME pFileName;

   pFileName = zh_fsFNameSplit( szBagName );
   pIndex = pArea->lpIndexes;
   while( pIndex )
   {
      PZH_FNAME pIndexName = zh_fsFNameSplit( pIndex->szFileName );
      ZH_BOOL fFound = ( pFileName->szName ? pIndexName->szName &&
           ! zh_stricmp( pIndexName->szName, pFileName->szName ) : ! pIndexName->szName ) &&
         ( ! pFileName->szPath ||
           ( pIndexName->szPath && ! zh_stricmp( pIndexName->szPath, pFileName->szPath ) ) ) &&
         ( ! pFileName->szExtension ||
           ( pIndexName->szExtension && ! zh_stricmp( pIndexName->szExtension, pFileName->szExtension ) ) );
      zh_xfree( pIndexName );
      if( fFound )
         break;
      pIndex = pIndex->pNext;
   }
   zh_xfree( pFileName );
   return pIndex;
}

/*
 * get Tag by number
 */
static LPCDXTAG zh_cdxGetTagByNumber( CDXAREAP pArea, ZH_USHORT uiTag )
{
   LPCDXTAG pTag = NULL;
   LPCDXINDEX pIndex = pArea->lpIndexes;

   while( uiTag && pIndex )
   {
      pTag = pIndex->TagList;
      while( uiTag && pTag )
      {
         if( --uiTag )
            pTag = pTag->pNext;
      }
      pIndex = pIndex->pNext;
   }
   return pTag;
}

/*
 * get Tag number
 */
static ZH_USHORT zh_cdxGetTagNumber( CDXAREAP pArea, LPCDXTAG pFindTag )
{
   ZH_USHORT uiTag = 0;
   LPCDXTAG pTag = NULL;
   LPCDXINDEX pIndex = pArea->lpIndexes;

   if( pFindTag )
   {
      while( pIndex && ( pTag != pFindTag ) )
      {
         pTag = pIndex->TagList;
         while( pTag )
         {
            uiTag++;
            if( pTag == pFindTag )
               break;
            pTag = pTag->pNext;
         }
         pIndex = pIndex->pNext;
      }
      if( ! pTag )
         uiTag = 0;
   }
   return uiTag;
}

/*
 * find Tag in tag list
 */
static LPCDXTAG zh_cdxFindTag( CDXAREAP pArea, PZH_ITEM pTagItem,
                               PZH_ITEM pBagItem, ZH_USHORT * puiTag )
{
   LPCDXTAG pTag = NULL;
   int iTag = 0, iFind = 0;
   char szTag[ CDX_MAXTAGNAMELEN + 1 ];
   LPCDXINDEX pIndex = pArea->lpIndexes;
   ZH_BOOL fBag;

   zh_strncpyUpperTrim( szTag, zh_itemGetCPtr( pTagItem ), sizeof( szTag ) - 1 );
   if( ! szTag[ 0 ] )
      iFind = zh_itemGetNI( pTagItem );

   fBag = szTag[ 0 ] && zh_itemGetCLen( pBagItem ) > 0;
   if( fBag )
   {
      pIndex = zh_cdxFindBag( pArea, zh_itemGetCPtr( pBagItem ) );
   }
   else
   {
      int iBag = zh_itemGetNI( pBagItem );

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

   if( pIndex && ( iFind > 0 || szTag[ 0 ] ) )
   {
      do
      {
         pTag = pIndex->TagList;
         while( pTag )
         {
            iTag++;
            if( ( iFind != 0 ? iTag == iFind : ! zh_stricmp( pTag->szName, szTag ) ) )
               break;
            pTag = pTag->pNext;
         }
         if( pTag || fBag )
            break;
         pIndex = pIndex->pNext;
      }
      while( pIndex );
   }

   if( puiTag )
   {
      if( ! pTag )
         *puiTag = 0;
      else if( fBag )
         *puiTag = zh_cdxGetTagNumber( pArea, pTag );
      else
         *puiTag = ( ZH_USHORT ) iTag;
   }

   return pTag;
}

/*
 * get current active Tag
 */
static LPCDXTAG zh_cdxGetActiveTag( CDXAREAP pArea )
{
   LPCDXTAG pTag;

   if( ! pArea->uiTag )
      return NULL;
   pTag = zh_cdxGetTagByNumber( pArea, pArea->uiTag );
   if( ! pTag )
      pArea->uiTag = 0;
   return pTag;
}

/*
 * refresh CurKey value and set proper path from RootPage to LeafPage
 */
static ZH_BOOL zh_cdxCurKeyRefresh( CDXAREAP pArea, LPCDXTAG pTag )
{
   if( pArea->dbfarea.lpdbPendingRel )
      SELF_FORCEREL( &pArea->dbfarea.area );

   if( ! pArea->dbfarea.fPositioned )
   {
      pTag->TagEOF = ZH_TRUE;
      pTag->fRePos = ZH_FALSE;
      pTag->CurKey->rec = 0;
      return ZH_FALSE;
   }
   else if( pTag->fRePos || pTag->CurKey->rec != pArea->dbfarea.ulRecNo )
   {
      LPCDXKEY pKey = NULL, pKey2 = NULL;

      /* Try to find previous if it's key for the same record */
      if( pTag->CurKey->rec == pArea->dbfarea.ulRecNo )
      {
         pKey = zh_cdxKeyCopy( pKey, pTag->CurKey );
         zh_cdxTagKeyFind( pTag, pKey );
      }
      if( pTag->CurKey->rec != pArea->dbfarea.ulRecNo )
      {
         ZH_BOOL fValidBuf = pArea->dbfarea.fValidBuffer;
         /* not found, create new key from DBF and if differs seek again */
         pKey2 = zh_cdxKeyEval( pKey2, pTag );
         if( pKey == NULL || memcmp( pKey2->val, pKey->val, pKey->len ) != 0 )
            zh_cdxTagKeyFind( pTag, pKey2 );

         /* not found, if key was generated from DBF buffer then force to
          * update it, create the new key and if differs seek again */
         if( pTag->CurKey->rec != pArea->dbfarea.ulRecNo && fValidBuf )
         {
            SELF_GOTO( &pArea->dbfarea.area, pArea->dbfarea.ulRecNo );
            pKey = zh_cdxKeyEval( pKey, pTag );
            if( memcmp( pKey2->val, pKey->val, pKey->len ) != 0 )
               zh_cdxTagKeyFind( pTag, pKey );
         }
         if( pTag->CurKey->rec != pArea->dbfarea.ulRecNo && pTag->Template )
         {
            zh_cdxTagGoTop( pTag );
            while( ! pTag->TagBOF && ! pTag->TagEOF && zh_cdxBottomScope( pTag ) )
            {
               if( pTag->CurKey->rec == pArea->dbfarea.ulRecNo )
                  break;
               zh_cdxTagSkipNext( pTag );
            }
         }
      }
      if( pKey )
         zh_cdxKeyFree( pKey );
      if( pKey2 )
         zh_cdxKeyFree( pKey2 );
      return pTag->CurKey->rec != 0 && pTag->CurKey->rec == pArea->dbfarea.ulRecNo;
   }
   return ZH_TRUE;
}

/*
 * skip to next/previous unique key
 */
static ZH_ERRCODE zh_cdxDBOISkipUnique( CDXAREAP pArea, LPCDXTAG pTag, ZH_LONG lToSkip )
{
   ZH_ERRCODE retval;
   ZH_BOOL fForward;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_cdxDBOISkipUnique(%p, %p, %ld)", ( void * ) pArea, ( void * ) pTag, lToSkip ) );

   if( FAST_GOCOLD( &pArea->dbfarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   if( ! pTag )
      return SELF_SKIP( &pArea->dbfarea.area, lToSkip );

   if( pArea->dbfarea.lpdbPendingRel )
      SELF_FORCEREL( &pArea->dbfarea.area );

   pArea->dbfarea.area.fTop = pArea->dbfarea.area.fBottom = ZH_FALSE;

   /* CL53 DBFCDX when index is active use this parameter
      only to chose forward or backward skipping */
   fForward = lToSkip >= 0;

   if( ! pArea->dbfarea.fPositioned )
   {
      if( fForward )
         retval = SELF_GOTO( &pArea->dbfarea.area, 0 );
      else
         retval = SELF_GOBOTTOM( &pArea->dbfarea.area );
   }
   else
   {
      LPCDXKEY pKey = NULL;
      ZH_BOOL fOut = ZH_FALSE;

      zh_cdxIndexLockRead( pTag->pIndex );
      zh_cdxTagRefreshScope( pTag );
      if( ! zh_cdxCurKeyRefresh( pArea, pTag ) )
      {
         if( pTag->TagEOF || ( fForward ? ! zh_cdxBottomScope( pTag ) :
                                          ! zh_cdxTopScope( pTag ) ) )
            fOut = ZH_TRUE;
         else if( ( fForward ? pTag->UsrAscend && zh_cdxTopScope( pTag ) :
                             ! pTag->UsrAscend && zh_cdxBottomScope( pTag ) ) &&
                   pTag->CurKey->rec != 0 )
         {
            pKey = zh_cdxKeyEval( pKey, pTag );
         }
      }
      if( fForward )
      {
         if( pArea->dbfarea.fPositioned && ! pTag->TagEOF )
         {
            if( ! pKey )
            {
               pKey = zh_cdxKeyCopy( NULL, pTag->CurKey );
               zh_cdxTagSkipNext( pTag );
            }
            while( ! pTag->TagEOF )
            {
               if( zh_cdxValCompare( pTag, pKey->val, pKey->len,
                                     pTag->CurKey->val, pTag->CurKey->len,
                                     CDX_CMP_EXACT ) != 0 )
               {
                  SELF_GOTO( &pArea->dbfarea.area, pTag->CurKey->rec );
                  SELF_SKIPFILTER( &pArea->dbfarea.area, 1 );
                  break;
               }
               zh_cdxTagSkipNext( pTag );
            }
         }
         retval = SELF_GOTO( &pArea->dbfarea.area, ( ! pArea->dbfarea.fPositioned || pTag->TagEOF )
                                              ? 0 : pTag->CurKey->rec );
      }
      else
      {
         if( ! fOut && ! pTag->TagBOF )
         {
            if( ! pKey )
            {
               pKey = zh_cdxKeyCopy( NULL, pTag->CurKey );
               zh_cdxTagSkipPrev( pTag );
            }
            while( ! pTag->TagBOF )
            {
               if( zh_cdxValCompare( pTag, pKey->val, pKey->len,
                                     pTag->CurKey->val, pTag->CurKey->len,
                                     CDX_CMP_EXACT ) != 0 )
               {
                  SELF_GOTO( &pArea->dbfarea.area, pTag->CurKey->rec );
                  SELF_SKIPFILTER( &pArea->dbfarea.area, -1 );
                  break;
               }
               zh_cdxTagSkipPrev( pTag );
            }
         }

         if( fOut || pTag->TagBOF )
         {
            retval = SELF_GOTOP( &pArea->dbfarea.area );
            pArea->dbfarea.area.fBof = ZH_TRUE;
         }
         else
         {
            retval = SELF_GOTO( &pArea->dbfarea.area, pTag->CurKey->rec );
         }
      }
      zh_cdxIndexUnLockRead( pTag->pIndex );
      if( pKey )
         zh_cdxKeyFree( pKey );
   }
   /* Update Bof and Eof flags */
   if( fForward )
      pArea->dbfarea.area.fBof = ZH_FALSE;
   else
      pArea->dbfarea.area.fEof = ZH_FALSE;

   return retval;
}

/*
 * skip while code block doesn't return ZH_TRUE
 */
static ZH_BOOL zh_cdxDBOISkipEval( CDXAREAP pArea, LPCDXTAG pTag, ZH_BOOL fForward,
                                   PZH_ITEM pEval )
{
   ZH_BOOL fFound = ZH_FALSE, fFirst = ZH_TRUE;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_cdxDBOISkipEval(%p, %p, %i, %p)", ( void * ) pArea, ( void * ) pTag, fForward, ( void * ) pEval ) );

   if( FAST_GOCOLD( &pArea->dbfarea.area ) == ZH_FAILURE )
      return ZH_FALSE;

   if( ! pTag || ( zh_itemType( pEval ) & ZH_IT_BLOCK ) == 0 )
   {
      if( SELF_SKIP( &pArea->dbfarea.area, fForward ? 1 : -1 ) == ZH_FAILURE )
         return ZH_FALSE;
      return fForward ? ! pArea->dbfarea.area.fEof : ! pArea->dbfarea.area.fBof;
   }

   if( pArea->dbfarea.lpdbPendingRel )
      SELF_FORCEREL( &pArea->dbfarea.area );

   pArea->dbfarea.area.fTop = pArea->dbfarea.area.fBottom = ZH_FALSE;

   zh_cdxIndexLockRead( pTag->pIndex );
   zh_cdxTagRefreshScope( pTag );
   if( ! zh_cdxCurKeyRefresh( pArea, pTag ) )
   {
      if( ! pTag->TagEOF && pTag->CurKey->rec != 0 &&
          ( fForward ? pTag->UsrAscend : ! pTag->UsrAscend ) &&
          zh_cdxTopScope( pTag ) && zh_cdxBottomScope( pTag ) )
         fFirst = ZH_FALSE;
   }
   if( fForward )
   {
      if( fFirst )
         zh_cdxTagSkipNext( pTag );
      while( ! pTag->TagEOF )
      {
         if( SELF_GOTO( &pArea->dbfarea.area, pTag->CurKey->rec ) == ZH_FAILURE )
            break;
         if( zh_cdxEvalSeekCond( pTag, pEval ) )
         {
            ZH_ULONG ulRecNo = pArea->dbfarea.ulRecNo;
            SELF_SKIPFILTER( &pArea->dbfarea.area, 1 );
            if( pArea->dbfarea.ulRecNo == ulRecNo || zh_cdxEvalSeekCond( pTag, pEval ) )
            {
               fFound = ZH_TRUE;
               break;
            }
         }
         zh_cdxTagSkipNext( pTag );
      }
      if( ! fFound )
         SELF_GOTO( &pArea->dbfarea.area, 0 );
   }
   else
   {
      if( fFirst )
         zh_cdxTagSkipPrev( pTag );
      while( ! pTag->TagBOF )
      {
         if( SELF_GOTO( &pArea->dbfarea.area, pTag->CurKey->rec ) == ZH_FAILURE )
            break;
         if( zh_cdxEvalSeekCond( pTag, pEval ) )
         {
            ZH_ULONG ulRecNo = pArea->dbfarea.ulRecNo;
            SELF_SKIPFILTER( &pArea->dbfarea.area, -1 );
            if( pArea->dbfarea.ulRecNo == ulRecNo || zh_cdxEvalSeekCond( pTag, pEval ) )
            {
               fFound = ZH_TRUE;
               break;
            }
         }
         zh_cdxTagSkipPrev( pTag );
      }
      if( ! fFound )
      {
         SELF_GOTOP( &pArea->dbfarea.area );
         pArea->dbfarea.area.fBof = ZH_TRUE;
      }
   }
   zh_cdxIndexUnLockRead( pTag->pIndex );

   /* Update Bof and Eof flags */
   if( fForward )
      pArea->dbfarea.area.fBof = ZH_FALSE;
   else
      pArea->dbfarea.area.fEof = ZH_FALSE;

   return fFound;
}

/*
 * skip while comparison with given pattern with wildcards doesn't return ZH_TRUE
 */
static ZH_BOOL zh_cdxDBOISkipWild( CDXAREAP pArea, LPCDXTAG pTag, ZH_BOOL fForward,
                                   PZH_ITEM pWildItm )
{
   ZH_BOOL fFound = ZH_FALSE, fFirst = ZH_TRUE;
   const char * szPattern;
   char * szFree = NULL;
   int iFixed = 0, iStop;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_cdxDBOISkipWild(%p, %p, %i, %p)", ( void * ) pArea, ( void * ) pTag, fForward, ( void * ) pWildItm ) );

   if( FAST_GOCOLD( &pArea->dbfarea.area ) == ZH_FAILURE )
      return ZH_FALSE;

   szPattern = zh_itemGetCPtr( pWildItm );

   if( ! pTag || pTag->uiType != 'C' || ! szPattern || ! *szPattern )
   {
      if( SELF_SKIP( &pArea->dbfarea.area, fForward ? 1 : -1 ) == ZH_FAILURE )
         return ZH_FALSE;
      return fForward ? pArea->dbfarea.fPositioned : ! pArea->dbfarea.area.fBof;
   }

   if( pArea->dbfarea.area.cdPage != zh_vmCDP() )
   {
      szPattern = szFree = zh_cdpDup( szPattern, zh_vmCDP(), pArea->dbfarea.area.cdPage );
   }

   while( iFixed < pTag->uiLen && szPattern[ iFixed ] &&
          szPattern[ iFixed ] != '*' && szPattern[ iFixed ] != '?' )
   {
      ++iFixed;
   }

   if( pArea->dbfarea.lpdbPendingRel )
      SELF_FORCEREL( &pArea->dbfarea.area );

   pArea->dbfarea.area.fTop = pArea->dbfarea.area.fBottom = ZH_FALSE;

   zh_cdxIndexLockRead( pTag->pIndex );
   zh_cdxTagRefreshScope( pTag );
   if( ! zh_cdxCurKeyRefresh( pArea, pTag ) )
   {
      if( ! pTag->TagEOF && pTag->CurKey->rec != 0 &&
          ( fForward ? pTag->UsrAscend : ! pTag->UsrAscend ) &&
          zh_cdxTopScope( pTag ) && zh_cdxBottomScope( pTag ) )
         fFirst = ZH_FALSE;
   }

   iStop = pTag->UsrAscend ? -1 : 1;
   if( ! fForward )
      iStop = -iStop;

   if( iFixed && ! pTag->TagEOF && pTag->CurKey->rec != 0 &&
       zh_cdxValCompare( pTag, ( const ZH_BYTE * ) szPattern, iFixed,
                         pTag->CurKey->val, iFixed,
                         CDX_CMP_PREFIX ) == -iStop )
   {
      LPCDXKEY pKey;

      pKey = zh_cdxKeyPut( NULL, ( const ZH_BYTE * ) szPattern, ( ZH_USHORT ) iFixed,
                     pTag->UsrAscend ? CDX_IGNORE_REC_NUM : CDX_MAX_REC_NUM );
      pKey->mode = CDX_CMP_PREFIX;
      if( ! zh_cdxTagKeyFind( pTag, pKey ) )
      {
         if( fForward )
            pTag->TagEOF = ZH_TRUE;
         else
            pTag->TagBOF = ZH_TRUE;
      }
      zh_cdxKeyFree( pKey );
      fFirst = ZH_FALSE;
   }

   if( fForward )
   {
      if( fFirst )
         zh_cdxTagSkipNext( pTag );
      while( ! pTag->TagEOF )
      {
         if( zh_strMatchWild( ( const char * ) pTag->CurKey->val, szPattern ) )
         {
            ZH_ULONG ulRecNo = pTag->CurKey->rec;
            if( SELF_GOTO( &pArea->dbfarea.area, ulRecNo ) != ZH_SUCCESS )
               break;
            SELF_SKIPFILTER( &pArea->dbfarea.area, 1 );
            if( pArea->dbfarea.ulRecNo == ulRecNo ||
                zh_strMatchWild( ( const char * ) pTag->CurKey->val, szPattern ) )
            {
               fFound = ZH_TRUE;
               break;
            }
         }
         if( iFixed && zh_cdxValCompare( pTag, ( const ZH_BYTE * ) szPattern, iFixed,
                                         pTag->CurKey->val, iFixed,
                                         CDX_CMP_PREFIX ) == iStop )
         {
            break;
         }
         zh_cdxTagSkipNext( pTag );
      }
      if( ! fFound )
         SELF_GOTO( &pArea->dbfarea.area, 0 );
   }
   else
   {
      if( fFirst )
         zh_cdxTagSkipPrev( pTag );
      while( ! pTag->TagBOF )
      {
         if( zh_strMatchWild( ( const char * ) pTag->CurKey->val, szPattern ) )
         {
            ZH_ULONG ulRecNo = pTag->CurKey->rec;
            if( SELF_GOTO( &pArea->dbfarea.area, ulRecNo ) != ZH_SUCCESS )
               break;
            SELF_SKIPFILTER( &pArea->dbfarea.area, -1 );
            if( pArea->dbfarea.ulRecNo == ulRecNo ||
                zh_strMatchWild( ( const char * ) pTag->CurKey->val, szPattern ) )
            {
               fFound = ZH_TRUE;
               break;
            }
         }
         if( iFixed && zh_cdxValCompare( pTag, ( const ZH_BYTE * ) szPattern, iFixed,
                                         pTag->CurKey->val, iFixed,
                                         CDX_CMP_PREFIX ) == iStop )
         {
            break;
         }
         zh_cdxTagSkipPrev( pTag );
      }
      if( ! fFound )
      {
         SELF_GOTOP( &pArea->dbfarea.area );
         pArea->dbfarea.area.fBof = ZH_TRUE;
      }
   }
   zh_cdxIndexUnLockRead( pTag->pIndex );

   /* Update Bof and Eof flags */
   if( fForward )
      pArea->dbfarea.area.fBof = ZH_FALSE;
   else
      pArea->dbfarea.area.fEof = ZH_FALSE;

   if( szFree )
      zh_xfree( szFree );

   return fFound;
}

static ZH_BOOL zh_cdxRegexMatch( CDXAREAP pArea, PZH_REGEX pRegEx, LPCDXKEY pKey )
{
   const char * szKey = ( const char * ) pKey->val;
   ZH_SIZE nLen = pKey->len;
   char * pszBuff = NULL;
   ZH_BOOL fResult;

   if( pArea->dbfarea.area.cdPage != zh_vmCDP() )
      szKey = pszBuff = zh_cdpnDup( szKey, &nLen,
                                    pArea->dbfarea.area.cdPage, zh_vmCDP() );
   fResult = zh_regexMatch( pRegEx, szKey, nLen, ZH_FALSE );
   if( pszBuff )
      zh_xfree( pszBuff );

   return fResult;
}

/*
 * skip while regular expression on index key val doesn't return ZH_TRUE
 */
static ZH_BOOL zh_cdxDBOISkipRegEx( CDXAREAP pArea, LPCDXTAG pTag, ZH_BOOL fForward,
                                    PZH_ITEM pRegExItm )
{
   ZH_BOOL fFound = ZH_FALSE, fFirst = ZH_TRUE;
   PZH_REGEX pRegEx;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_cdxDBOISkipRegEx(%p, %p, %i, %p)", ( void * ) pArea, ( void * ) pTag, fForward, ( void * ) pRegExItm ) );

   if( FAST_GOCOLD( &pArea->dbfarea.area ) == ZH_FAILURE )
      return ZH_FALSE;

   if( ! pTag || pTag->uiType != 'C' || ( pRegEx = zh_regexGet( pRegExItm, 0 ) ) == NULL )
   {
      if( SELF_SKIP( &pArea->dbfarea.area, fForward ? 1 : -1 ) == ZH_FAILURE )
         return ZH_FALSE;
      return fForward ? pArea->dbfarea.fPositioned : ! pArea->dbfarea.area.fBof;
   }

   if( pArea->dbfarea.lpdbPendingRel )
      SELF_FORCEREL( &pArea->dbfarea.area );

   pArea->dbfarea.area.fTop = pArea->dbfarea.area.fBottom = ZH_FALSE;

   zh_cdxIndexLockRead( pTag->pIndex );
   zh_cdxTagRefreshScope( pTag );
   if( ! zh_cdxCurKeyRefresh( pArea, pTag ) )
   {
      if( ! pTag->TagEOF && pTag->CurKey->rec != 0 &&
          ( fForward ? pTag->UsrAscend : ! pTag->UsrAscend ) &&
          zh_cdxTopScope( pTag ) && zh_cdxBottomScope( pTag ) )
         fFirst = ZH_FALSE;
   }
   if( fForward )
   {
      if( fFirst )
         zh_cdxTagSkipNext( pTag );
      while( ! pTag->TagEOF )
      {
         if( zh_cdxRegexMatch( pArea, pRegEx, pTag->CurKey ) )
         {
            ZH_ULONG ulRecNo = pArea->dbfarea.ulRecNo;
            SELF_SKIPFILTER( &pArea->dbfarea.area, 1 );
            if( pArea->dbfarea.ulRecNo == ulRecNo ||
                zh_cdxRegexMatch( pArea, pRegEx, pTag->CurKey ) )
            {
               fFound = ZH_TRUE;
               break;
            }
         }
         zh_cdxTagSkipNext( pTag );
      }
      SELF_GOTO( &pArea->dbfarea.area, fFound ? pTag->CurKey->rec : 0 );
   }
   else
   {
      if( fFirst )
         zh_cdxTagSkipPrev( pTag );
      while( ! pTag->TagBOF )
      {
         if( zh_cdxRegexMatch( pArea, pRegEx, pTag->CurKey ) )
         {
            ZH_ULONG ulRecNo = pArea->dbfarea.ulRecNo;
            SELF_SKIPFILTER( &pArea->dbfarea.area, -1 );
            if( pArea->dbfarea.ulRecNo == ulRecNo ||
                zh_cdxRegexMatch( pArea, pRegEx, pTag->CurKey ) )
            {
               fFound = ZH_TRUE;
               break;
            }
         }
         zh_cdxTagSkipPrev( pTag );
      }
      if( fFound )
         SELF_GOTO( &pArea->dbfarea.area, pTag->CurKey->rec );
      else
      {
         SELF_GOTOP( &pArea->dbfarea.area );
         pArea->dbfarea.area.fBof = ZH_TRUE;
      }
   }
   zh_cdxIndexUnLockRead( pTag->pIndex );

   /* Update Bof and Eof flags */
   if( fForward )
      pArea->dbfarea.area.fBof = ZH_FALSE;
   else
      pArea->dbfarea.area.fEof = ZH_FALSE;

   zh_regexFree( pRegEx );

   return fFound;
}

/*
 * evaluate given C function in given scope
 */
static ZH_ULONG zh_cdxDBOIScopeEval( LPCDXTAG pTag, ZH_EVALSCOPE_FUNC pFunc, void * pParam, PZH_ITEM pItemLo, PZH_ITEM pItemHi )
{
   ZH_ULONG ulCount = 0, ulLen = ( ZH_ULONG ) pTag->uiLen;
   LPCDXKEY pCurKey = zh_cdxKeyCopy( NULL, pTag->CurKey ),
            pTopScopeKey = pTag->topScopeKey,
            pBtmScopeKey = pTag->bottomScopeKey;

   /* TODO: RT error when item type differ then Tag type */
   if( ! pItemLo || ZH_IS_NIL( pItemLo ) )
      pTag->topScopeKey = NULL;
   else
      pTag->topScopeKey = zh_cdxKeyPutItem( NULL, pItemLo, CDX_IGNORE_REC_NUM,
                                            pTag, CDX_CMP_PREFIX );

   if( ! pItemHi || ZH_IS_NIL( pItemHi ) )
      pTag->bottomScopeKey = NULL;
   else
      pTag->bottomScopeKey = zh_cdxKeyPutItem( NULL, pItemHi, CDX_MAX_REC_NUM,
                                               pTag, CDX_CMP_PREFIX );

   zh_cdxIndexLockRead( pTag->pIndex );
   zh_cdxTagGoTop( pTag );
   while( ! pTag->TagEOF )
   {
      pFunc( pTag->CurKey->rec, pTag->CurKey->val, ulLen, pParam );
      ulCount++;
      zh_cdxTagSkipNext( pTag );
   }
   zh_cdxIndexUnLockRead( pTag->pIndex );

   if( pTag->topScopeKey )
      zh_cdxKeyFree( pTag->topScopeKey );
   pTag->topScopeKey = pTopScopeKey;
   if( pTag->bottomScopeKey )
      zh_cdxKeyFree( pTag->bottomScopeKey );
   pTag->bottomScopeKey = pBtmScopeKey;
   pTag->curKeyState &= ~( CDX_CURKEY_RAWPOS | CDX_CURKEY_LOGPOS );

   pTag->fRePos = ZH_TRUE;
   zh_cdxKeyFree( pTag->CurKey );
   pTag->CurKey = pCurKey;

   return ulCount;
}

/*
 * return number of keys in order
 */
static ZH_LONG zh_cdxDBOIKeyCount( CDXAREAP pArea, LPCDXTAG pTag, ZH_BOOL fFilters )
{
   ZH_ULONG ulKeyCount = 0;
   ZH_BOOL fLogOpt = pArea->dbfarea.area.dbfi.itmCobExpr || ! pArea->dbfarea.area.dbfi.fFilter;

   if( pTag )
   {
      ZH_BOOL fCheckFilter = ( fLogOpt && fFilters && pArea->dbfarea.area.dbfi.itmCobExpr );
      ZH_ULONG ulRecNo = pArea->dbfarea.ulRecNo;
      LPCDXKEY pCurKey;
      zh_cdxIndexLockRead( pTag->pIndex );
      zh_cdxTagRefreshScope( pTag );

      if( pTag && ( fFilters ? fLogOpt && CURKEY_LOGCNT( pTag ) : CURKEY_RAWCNT( pTag ) ) )
      {
         ulKeyCount = fFilters ? pTag->logKeyCount : pTag->rawKeyCount;
      }
      else
      {
         if( pTag->topScopeKey || pTag->bottomScopeKey || pTag->UsrUnique || pArea->dbfarea.area.dbfi.fFilter )
         {
            pCurKey = zh_cdxKeyCopy( NULL, pTag->CurKey );
            zh_cdxTagGoTop( pTag );
            while( ! pTag->TagEOF )
            {
               if( ! fCheckFilter || zh_cdxCheckRecordFilter( pArea, pTag->CurKey->rec ) )
                  ulKeyCount++;
               zh_cdxTagSkipNext( pTag );
            }
            pTag->fRePos = ZH_TRUE;
            zh_cdxKeyFree( pTag->CurKey );
            pTag->CurKey = pCurKey;
            if( fCheckFilter )
               SELF_GOTO( &pArea->dbfarea.area, ulRecNo );
         }
         else
         {
            LPCDXPAGE pPage;
            pCurKey = zh_cdxKeyCopy( NULL, pTag->CurKey );
            if( pTag->UsrAscend )
               zh_cdxTagGoTop( pTag );
            else
               zh_cdxTagGoBottom( pTag );
            pPage = pTag->RootPage;
            while( pPage->Child )
               pPage = pPage->Child;
            ulKeyCount = pPage->iKeys;
            if( pPage->Right != CDX_DUMMYNODE )
            {
               ZH_ULONG ulPage = pPage->Right;
               pPage = zh_cdxPageNew( pTag, NULL, CDX_DUMMYNODE );
               pPage->Page = ulPage;
               while( pPage->Page != CDX_DUMMYNODE )
               {
                  zh_cdxPageLoad( pPage );
                  ulKeyCount += pPage->iKeys;
                  pPage->Page = pPage->Right;
               }
               zh_cdxPageFree( pPage, ZH_TRUE );
            }
            pTag->fRePos = ZH_TRUE;
            zh_cdxKeyFree( pTag->CurKey );
            pTag->CurKey = pCurKey;
         }
         if( ! fFilters )
         {
            pTag->rawKeyCount = ulKeyCount;
            pTag->curKeyState |= CDX_CURKEY_RAWCNT;
         }
         else if( fLogOpt )
         {
            pTag->logKeyCount = ulKeyCount;
            pTag->curKeyState |= CDX_CURKEY_LOGCNT;
         }
      }
      zh_cdxIndexUnLockRead( pTag->pIndex );
   }
   else  /* no filter, no order */
   {
      if( fLogOpt && fFilters && pArea->dbfarea.area.dbfi.itmCobExpr )
      {
         ZH_ULONG ulRecNo = pArea->dbfarea.ulRecNo;

         if( SELF_GOTOP( &pArea->dbfarea.area ) == ZH_SUCCESS )
         {
            while( ! pArea->dbfarea.area.fEof )
            {
               ulKeyCount++;
               if( SELF_SKIP( &pArea->dbfarea.area, 1 ) != ZH_SUCCESS )
                  break;
            }
            SELF_GOTO( &pArea->dbfarea.area, ulRecNo );
         }
      }
      else
      {
         SELF_RECCOUNT( &pArea->dbfarea.area, &ulKeyCount );
      }
   }
   return ulKeyCount;
}

/*
 * return logical key position in order
 */
static ZH_LONG zh_cdxDBOIKeyNo( CDXAREAP pArea, LPCDXTAG pTag, ZH_BOOL fFilters )
{
   ZH_ULONG ulKeyNo = 0;
   ZH_BOOL fLogOpt = pArea->dbfarea.area.dbfi.itmCobExpr || ! pArea->dbfarea.area.dbfi.fFilter;

   if( pArea->dbfarea.lpdbPendingRel )
      SELF_FORCEREL( &pArea->dbfarea.area );

   if( ! pArea->dbfarea.fPositioned )
      ulKeyNo = 0;
   else if( pTag )
   {
      ZH_BOOL fCheckFilter = ( fLogOpt && fFilters && pArea->dbfarea.area.dbfi.itmCobExpr );
      ZH_ULONG ulRecNo = pArea->dbfarea.ulRecNo;

      zh_cdxIndexLockRead( pTag->pIndex );
      zh_cdxTagRefreshScope( pTag );

      if( fFilters ? ( fLogOpt && CURKEY_LOGPOS( pTag ) ) :
                     ( CURKEY_RAWPOS( pTag ) &&
                                          pTag->rawKeyRec == pArea->dbfarea.ulRecNo ) )
      {
         ulKeyNo = fFilters ? pTag->logKeyPos : pTag->rawKeyPos;
      }
      else
      {
         zh_cdxTagOpen( pTag );
         if( zh_cdxCurKeyRefresh( pArea, pTag ) )
         {
            if( pTag->topScopeKey || pTag->bottomScopeKey || pTag->UsrUnique || pArea->dbfarea.area.dbfi.fFilter )
            {
               if( zh_cdxBottomScope( pTag ) && zh_cdxTopScope( pTag ) &&
                   ( ! fCheckFilter || zh_cdxCheckRecordFilter( pArea, ulRecNo ) ) )
               {
                  LPCDXKEY pCurKey = zh_cdxKeyCopy( NULL, pTag->CurKey );
                  if( ! zh_cdxCheckRecordScope( pArea, pTag->CurKey->rec ) )
                     zh_cdxTagSkipPrev( pTag );
                  while( ! pTag->TagBOF )
                  {
                     if( ! fCheckFilter || zh_cdxCheckRecordFilter( pArea, pTag->CurKey->rec ) )
                        ulKeyNo++;
                     zh_cdxTagSkipPrev( pTag );
                  }
                  pTag->fRePos = ZH_TRUE;
                  zh_cdxKeyFree( pTag->CurKey );
                  pTag->CurKey = pCurKey;
                  if( fCheckFilter )
                     SELF_GOTO( &pArea->dbfarea.area, ulRecNo );
               }
            }
            else
            {
               LPCDXPAGE pPage = pTag->RootPage;
               while( pPage->Child )
                  pPage = pPage->Child;
               if( pTag->UsrAscend )
               {
                  ulKeyNo = pPage->iCurKey + 1;
                  if( pPage->Left != CDX_DUMMYNODE )
                  {
                     ZH_ULONG ulPage = pPage->Left;
                     pPage = zh_cdxPageNew( pTag, NULL, CDX_DUMMYNODE );
                     pPage->Page = ulPage;
                     while( pPage->Page != CDX_DUMMYNODE )
                     {
                        zh_cdxPageLoad( pPage );
                        ulKeyNo += pPage->iKeys;
                        pPage->Page = pPage->Left;
                     }
                     zh_cdxPageFree( pPage, ZH_TRUE );
                  }
               }
               else
               {
                  ulKeyNo = pPage->iKeys - pPage->iCurKey;
                  if( pPage->Right != CDX_DUMMYNODE )
                  {
                     ZH_ULONG ulPage = pPage->Right;
                     pPage = zh_cdxPageNew( pTag, NULL, CDX_DUMMYNODE );
                     pPage->Page = ulPage;
                     while( pPage->Page != CDX_DUMMYNODE )
                     {
                        zh_cdxPageLoad( pPage );
                        ulKeyNo += pPage->iKeys;
                        pPage->Page = pPage->Right;
                     }
                     zh_cdxPageFree( pPage, ZH_TRUE );
                  }
               }
            }
            if( ulKeyNo != 0 )
            {
               if( ! fFilters )
               {
                  pTag->rawKeyPos = ulKeyNo;
                  CURKEY_SETRAWPOS( pTag );
               }
               else if( fLogOpt )
               {
                  pTag->logKeyPos = ulKeyNo;
                  CURKEY_SETLOGPOS( pTag );
               }
            }
         }
      }
      zh_cdxIndexUnLockRead( pTag->pIndex );
   }
   else
   {
      ZH_ULONG ulRecNo = pArea->dbfarea.ulRecNo;

      if( fLogOpt && fFilters && pArea->dbfarea.area.dbfi.itmCobExpr )
      {
         if( zh_cdxCheckRecordFilter( pArea, ulRecNo ) )
         {
            do
            {
               ulKeyNo++;
               if( SELF_SKIP( &pArea->dbfarea.area, -1 ) != ZH_SUCCESS )
                  break;
            }
            while( ! ( &pArea->dbfarea.area )->fBof );
            SELF_GOTO( &pArea->dbfarea.area, ulRecNo );
         }
      }
      else
      {
         ulKeyNo = ulRecNo;
      }
   }
   return ulKeyNo;
}

/*
 * DBOI_KEYGOTO goto specific logical record in the index file
 */
static ZH_ERRCODE zh_cdxDBOIKeyGoto( CDXAREAP pArea, LPCDXTAG pTag, ZH_ULONG ulKeyNo, ZH_BOOL fFilters )
{
   ZH_ERRCODE retval;
   ZH_ULONG ulKeyCnt = ulKeyNo;
   ZH_BOOL fLogOpt = pArea->dbfarea.area.dbfi.itmCobExpr || ! pArea->dbfarea.area.dbfi.fFilter;

   if( ulKeyNo == 0 )
      retval = SELF_GOTO( &pArea->dbfarea.area, 0 );
   else if( pTag )
   {
      ZH_BOOL fCheckFilter = ( fLogOpt && fFilters && pArea->dbfarea.area.dbfi.itmCobExpr );
      zh_cdxIndexLockRead( pTag->pIndex );
      zh_cdxTagRefreshScope( pTag );
      if( ! pArea->dbfarea.lpdbPendingRel && ( fFilters ?
               fLogOpt && CURKEY_LOGPOS( pTag ) && pTag->logKeyPos == ulKeyNo :
               ( CURKEY_RAWPOS( pTag ) && pTag->rawKeyPos == ulKeyNo ) ) )
      {
         retval = SELF_GOTO( &pArea->dbfarea.area, fFilters ? pTag->logKeyRec : pTag->rawKeyRec );
      }
      else
      {
         if( pTag->topScopeKey || pTag->bottomScopeKey || pTag->UsrUnique || pArea->dbfarea.area.dbfi.fFilter )
         {
            zh_cdxTagGoTop( pTag );
            if( fCheckFilter )
               while( ! pTag->TagEOF )
               {
                  if( zh_cdxCheckRecordFilter( pArea, pTag->CurKey->rec ) )
                  {
                     if( ! --ulKeyCnt )
                        break;
                  }
                  zh_cdxTagSkipNext( pTag );
               }
            else
               while( ! pTag->TagEOF && --ulKeyCnt )
                  zh_cdxTagSkipNext( pTag );
         }
         else
         {
            LPCDXPAGE pPage, pOwnerPage = NULL;
            ZH_ULONG ulNextPg;
            zh_cdxTagGoTop( pTag );
            pPage = pTag->RootPage;
            while( pPage->Child )
            {
               pOwnerPage = pPage;
               pPage = pPage->Child;
            }
            while( ( ZH_ULONG ) pPage->iKeys < ulKeyCnt && pOwnerPage &&
                   ( ulNextPg = pTag->UsrAscend ?
                     pPage->Right : pPage->Left ) != CDX_DUMMYNODE )
            {
               ulKeyCnt -= pPage->iKeys;
               pOwnerPage->Child = zh_cdxPageNew( pPage->TagParent, pPage->Owner, ulNextPg );
               zh_cdxPageFree( pPage, ZH_FALSE );
               pPage = pOwnerPage->Child;
            }
            if( ( ZH_ULONG ) pPage->iKeys >= ulKeyCnt )
            {
               pPage->iCurKey = pTag->UsrAscend ? ( int ) ulKeyCnt - 1 : pPage->iKeys - ( int ) ulKeyCnt;
               zh_cdxSetCurKey( pPage );
            }
            else
            {
               pTag->CurKey->rec = 0;
            }
         }
         retval = SELF_GOTO( &pArea->dbfarea.area, pTag->CurKey->rec );
         if( pArea->dbfarea.fPositioned )
         {
            if( ! fFilters )
            {
               pTag->rawKeyPos = ulKeyNo;
               CURKEY_SETRAWPOS( pTag );
            }
            else if( fLogOpt )
            {
               pTag->logKeyPos = ulKeyNo;
               CURKEY_SETLOGPOS( pTag );
            }
         }
      }
      zh_cdxIndexUnLockRead( pTag->pIndex );
   }
   else
   {
      if( fLogOpt && fFilters && pArea->dbfarea.area.dbfi.itmCobExpr )
      {
         retval = SELF_GOTOP( &pArea->dbfarea.area );
         if( retval == ZH_SUCCESS && --ulKeyCnt )
            retval = SELF_SKIP( &pArea->dbfarea.area, ulKeyCnt );
      }
      else
      {
         retval = SELF_GOTO( &pArea->dbfarea.area, ulKeyNo );
      }
   }

   return retval;
}

static double zh_cdxCountRelKeyPos( LPCDXPAGE pPage )
{
   return ( ( pPage->Child ? zh_cdxCountRelKeyPos( pPage->Child ) : 0.5 ) +
            pPage->iCurKey ) / pPage->iKeys;
}

static ZH_BOOL zh_cdxGoToRelKeyPos( LPCDXPAGE pPage, double dPos )
{
   do
   {
      if( pPage->iKeys == 0 )
         return ZH_FALSE;

      pPage->iCurKey = ( int ) ( dPos * pPage->iKeys );
      if( pPage->iCurKey >= pPage->iKeys )
         pPage->iCurKey = pPage->iKeys - 1;

      if( ( pPage->PageType & CDX_NODE_LEAF ) != 0 )
         break;

      dPos = dPos * pPage->iKeys - pPage->iCurKey;
      if( dPos < 0.0 )
         dPos = 0.0;
      else if( dPos >= 1.0 )
         dPos = 1.0;

      zh_cdxPageGetChild( pPage );
      pPage = pPage->Child;
   }
   while( pPage );

   return ZH_TRUE;
}

static double zh_cdxDBOIGetRelKeyPos( CDXAREAP pArea, LPCDXTAG pTag )
{
   ZH_ULONG ulRecNo = 0, ulRecCount = 0;
   double dPos = 0.0;

   /* resolve any pending relations */
   SELF_RECNO( &pArea->dbfarea.area, &ulRecNo );

   if( ! pArea->dbfarea.fPositioned )
   {
      if( ulRecNo > 1 )
         dPos = 1.0;
   }
   else if( ! pTag )
   {
      SELF_RECCOUNT( &pArea->dbfarea.area, &ulRecCount );
      if( ulRecCount != 0 )
         dPos = ( 0.5 + ulRecNo ) / ulRecCount;
   }
   else
   {
      LPCDXKEY pKey;
      double dStart, dStop, dFact = 0.0000000000001;
      ZH_BOOL fOK = ZH_TRUE;

      if( pTag->UsrAscend )
      {
         dStart = 0.0;
         dStop = 1.0;
      }
      else
      {
         dStart = 1.0;
         dStop = 0.0;
      }

      zh_cdxIndexLockRead( pTag->pIndex );
      zh_cdxTagRefreshScope( pTag );

      pKey = pTag->UsrAscend ? pTag->topScopeKey : pTag->bottomScopeKey;
      if( pKey )
      {
         zh_cdxTagKeyFind( pTag, pKey );
         if( pTag->CurKey->rec == 0 || pTag->TagEOF || ! zh_cdxBottomScope( pTag ) )
            fOK = ZH_FALSE;
         else
            dStart = zh_cdxCountRelKeyPos( pTag->RootPage );
      }
      pKey = pTag->UsrAscend ? pTag->bottomScopeKey : pTag->topScopeKey;
      if( pKey && fOK )
      {
         zh_cdxTagKeyFind( pTag, pKey );
         if( pTag->CurKey->rec == 0 || pTag->TagBOF || ! zh_cdxTopScope( pTag ) )
            fOK = ZH_FALSE;
         else
            dStop = zh_cdxCountRelKeyPos( pTag->RootPage );
      }
      if( fOK )
      {
         if( ! pTag->UsrAscend )
         {
            double dTmp = dStart;
            dStart = dStop;
            dStop = dTmp;
         }
         pTag->fRePos = ZH_TRUE;
         if( zh_cdxCurKeyRefresh( pArea, pTag ) &&
             zh_cdxTopScope( pTag ) && zh_cdxBottomScope( pTag ) )
         {
            if( dStart >= dStop - dFact )
               dPos = 0.5;
            else
            {
               dPos = zh_cdxCountRelKeyPos( pTag->RootPage );
               dPos = ( dPos - dStart ) / ( dStop - dStart );
               if( ! pTag->UsrAscend )
                  dPos = 1.0 - dPos;
               /* fix possible differences in FL representation */
               if( dPos <= 0.0 )
                  dPos = 0.0;
               else if( dPos >= 1.0 )
                  dPos = 1.0;
            }
         }
      }
      zh_cdxIndexUnLockRead( pTag->pIndex );
   }

   return dPos;
}

static void zh_cdxDBOISetRelKeyPos( CDXAREAP pArea, LPCDXTAG pTag, double dPos )
{
   if( ! pTag )
   {
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
         SELF_GOTO( &pArea->dbfarea.area, ulRecNo );
         SELF_SKIPFILTER( &pArea->dbfarea.area, 1 );
         if( pArea->dbfarea.area.fEof )
            SELF_GOTOP( &pArea->dbfarea.area );
      }
   }
   else
   {
      ZH_BOOL fForward = ZH_TRUE, fOK = ZH_TRUE, fTop = ZH_FALSE;
      zh_cdxIndexLockRead( pTag->pIndex );
      zh_cdxTagRefreshScope( pTag );

      if( dPos >= 1.0 )
      {
         fForward = ZH_FALSE;
      }
      else if( dPos <= 0.0 )
      {
         fTop = ZH_TRUE;
      }
      else
      {
         LPCDXKEY pKey;
         double dStart, dStop, dFact = 0.0000000000001;

         if( pTag->UsrAscend )
         {
            dStart = 0.0;
            dStop = 1.0;
         }
         else
         {
            dStart = 1.0;
            dStop = 0.0;
         }

         pKey = pTag->UsrAscend ? pTag->topScopeKey : pTag->bottomScopeKey;
         if( pKey )
         {
            zh_cdxTagKeyFind( pTag, pKey );
            if( pTag->CurKey->rec == 0 || pTag->TagEOF || ! zh_cdxBottomScope( pTag ) )
               fOK = ZH_FALSE;
            else
               dStart = zh_cdxCountRelKeyPos( pTag->RootPage );
         }
         pKey = pTag->UsrAscend ? pTag->bottomScopeKey : pTag->topScopeKey;
         if( pKey && fOK )
         {
            zh_cdxTagKeyFind( pTag, pKey );
            if( pTag->CurKey->rec == 0 || pTag->TagBOF || ! zh_cdxTopScope( pTag ) )
               fOK = ZH_FALSE;
            else
               dStop = zh_cdxCountRelKeyPos( pTag->RootPage );
         }
         if( fOK )
         {
            if( ! pTag->UsrAscend )
            {
               double dTmp = dStart;
               dStart = dStop;
               dStop = dTmp;
               dPos = 1.0 - dPos;
            }
            if( dStart >= dStop - dFact )
            {
               fTop = ZH_TRUE;
            }
            else
            {
               dPos = dPos * ( dStop - dStart ) + dStart;
               pTag->fRePos = ZH_FALSE;
               zh_cdxTagOpen( pTag );
               pTag->TagBOF = pTag->TagEOF = ZH_FALSE;
               if( ! zh_cdxGoToRelKeyPos( pTag->RootPage, dPos ) )
               {
                  fTop = ZH_TRUE;
               }
               else
               {
                  zh_cdxSetCurKey( pTag->RootPage );
                  if( ! zh_cdxTopScope( pTag ) )
                     fTop = ZH_TRUE;
                  else if( ! zh_cdxBottomScope( pTag ) )
                     fForward = ZH_FALSE;
               }
            }
         }
      }
      if( ! fOK )
      {
         SELF_GOTO( &pArea->dbfarea.area, 0 );
      }
      else
      {
         if( fForward )
         {
            if( fTop )
               zh_cdxTagGoTop( pTag );
            while( ! pTag->TagEOF )
            {
               if( zh_cdxCheckRecordFilter( pArea, pTag->CurKey->rec ) )
                  break;
               zh_cdxTagSkipNext( pTag );
            }
            if( pTag->TagEOF && ! fTop )
               fForward = ZH_FALSE;
         }
         if( ! fForward )
         {
            zh_cdxTagGoBottom( pTag );
            while( ! pTag->TagBOF )
            {
               if( zh_cdxCheckRecordFilter( pArea, pTag->CurKey->rec ) )
                  break;
               zh_cdxTagSkipPrev( pTag );
            }
            if( pTag->TagBOF )
            {
               pTag->CurKey->rec = 0;
            }
         }
         SELF_GOTO( &pArea->dbfarea.area, pTag->CurKey->rec );
      }
      zh_cdxIndexUnLockRead( pTag->pIndex );
   }
}

/*
 * DBOI_FINDREC find a specific record in the tag - it's useful for
 * custom indexes when the same record can be stored more then once
 * or when the used index key is unknown
 */
static ZH_BOOL zh_cdxDBOIFindRec( CDXAREAP pArea, LPCDXTAG pTag, ZH_ULONG ulRecNo, ZH_BOOL fCont )
{
   ZH_BOOL fFound = ZH_FALSE;

   if( pTag && ulRecNo )
   {
      if( pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped )
         SELF_FORCEREL( &pArea->dbfarea.area );

      zh_cdxIndexLockRead( pTag->pIndex );
      zh_cdxTagRefreshScope( pTag );
      if( fCont )
      {
         if( ! zh_cdxCurKeyRefresh( pArea, pTag ) )
            ulRecNo = 0;
         else
            zh_cdxTagSkipNext( pTag );
      }
      else
      {
         zh_cdxTagGoTop( pTag );
      }
      if( ulRecNo )
      {
         while( ! pTag->TagBOF && ! pTag->TagEOF && zh_cdxBottomScope( pTag ) )
         {
            if( pTag->CurKey->rec == ulRecNo )
            {
               fFound = ZH_TRUE;
               break;
            }
            zh_cdxTagSkipNext( pTag );
         }
      }
      zh_cdxIndexUnLockRead( pTag->pIndex );
   }
   SELF_GOTO( &pArea->dbfarea.area, fFound ? ulRecNo : 0 );
   return fFound;
}

static void zh_cdxClearLogPosInfo( CDXAREAP pArea )
{
   LPCDXINDEX pIndex = pArea->lpIndexes;
   LPCDXTAG pTag;

   while( pIndex )
   {
      pTag = pIndex->TagList;
      while( pTag )
      {
         pTag->curKeyState &= ~( CDX_CURKEY_LOGPOS | CDX_CURKEY_LOGCNT );
         pTag = pTag->pNext;
      }
      pIndex = pIndex->pNext;
   }
}

static void zh_cdxClearPosInfo( CDXAREAP pArea )
{
   LPCDXINDEX pIndex = pArea->lpIndexes;
   LPCDXTAG pTag;

   while( pIndex )
   {
      pTag = pIndex->TagList;
      while( pTag )
      {
         pTag->curKeyState &= ~( CDX_CURKEY_LOGPOS | CDX_CURKEY_LOGCNT |
                                 CDX_CURKEY_RAWPOS | CDX_CURKEY_RAWCNT );
         pTag = pTag->pNext;
      }
      pIndex = pIndex->pNext;
   }
}

/*
 * -- DBFCDX METHODS --
 */

/* ( DBENTRYP_BP )    zh_cdxBof     : NULL */
/* ( DBENTRYP_BP )    zh_cdxEof     : NULL */
/* ( DBENTRYP_BP )    zh_cdxFound   : NULL */

/* ( DBENTRYP_V )     zh_cdxGoBottom */
static ZH_ERRCODE zh_cdxGoBottom( CDXAREAP pArea )
{
   LPCDXTAG pTag;
   ZH_ERRCODE retval;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_cdxGoBottom(%p)", ( void * ) pArea ) );

   if( FAST_GOCOLD( &pArea->dbfarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   pTag = zh_cdxGetActiveTag( pArea );
   if( ! pTag )
      return SUPER_GOBOTTOM( &pArea->dbfarea.area );

   if( pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped )
      SELF_FORCEREL( &pArea->dbfarea.area );

   zh_cdxIndexLockRead( pTag->pIndex );
   zh_cdxTagRefreshScope( pTag );

   zh_cdxTagGoBottom( pTag );

   pArea->dbfarea.area.fTop = ZH_FALSE;
   pArea->dbfarea.area.fBottom = ZH_TRUE;

   retval = SELF_GOTO( &pArea->dbfarea.area, pTag->CurKey->rec );

   if( retval != ZH_FAILURE && pArea->dbfarea.fPositioned )
   {
      retval = SELF_SKIPFILTER( &pArea->dbfarea.area, -1 );

      if( pArea->dbfarea.fPositioned && CURKEY_LOGCNT( pTag ) )
      {
         pTag->logKeyPos = pTag->logKeyCount;
         CURKEY_SETLOGPOS( pTag );
      }
   }
   zh_cdxIndexUnLockRead( pTag->pIndex );

   return retval;
}

/* ( DBENTRYP_UL )    zh_cdxGoTo    : NULL */
/* ( DBENTRYP_I )     zh_cdxGoToId  : NULL */

/* ( DBENTRYP_V )     zh_cdxGoTop */
static ZH_ERRCODE zh_cdxGoTop( CDXAREAP pArea )
{
   LPCDXTAG pTag;
   ZH_ERRCODE retval;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_cdxGoTop(%p)", ( void * ) pArea ) );

   if( FAST_GOCOLD( &pArea->dbfarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   pTag = zh_cdxGetActiveTag( pArea );
   if( ! pTag )
      return SUPER_GOTOP( &pArea->dbfarea.area );

   if( pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped )
      SELF_FORCEREL( &pArea->dbfarea.area );

   zh_cdxIndexLockRead( pTag->pIndex );
   zh_cdxTagRefreshScope( pTag );

   zh_cdxTagGoTop( pTag );

   pArea->dbfarea.area.fTop = ZH_TRUE;
   pArea->dbfarea.area.fBottom = ZH_FALSE;

   retval = SELF_GOTO( &pArea->dbfarea.area, pTag->CurKey->rec );

   if( retval != ZH_FAILURE && pArea->dbfarea.fPositioned )
      retval = SELF_SKIPFILTER( &pArea->dbfarea.area, 1 );

   if( retval != ZH_FAILURE && pArea->dbfarea.fPositioned )
   {
      pTag->logKeyPos = 1;
      CURKEY_SETLOGPOS( pTag );
   }

   zh_cdxIndexUnLockRead( pTag->pIndex );
   return retval;
}

/* ( DBENTRYP_BIB )   zh_cdxSeek */
static ZH_ERRCODE zh_cdxSeek( CDXAREAP pArea, ZH_BOOL fSoftSeek, PZH_ITEM pKeyItm, ZH_BOOL fFindLast )
{
   LPCDXTAG pTag;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_cdxSeek(%p, %d, %p, %d)", ( void * ) pArea, fSoftSeek, ( void * ) pKeyItm, fFindLast ) );

   if( FAST_GOCOLD( &pArea->dbfarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   pTag = zh_cdxGetActiveTag( pArea );

   if( ! pTag )
   {
      zh_cdxErrorRT( pArea, EG_NOORDER, EDBF_NOTINDEXED, NULL, 0, EF_CANDEFAULT, NULL );
      return ZH_FAILURE;
   }
   else
   {
      LPCDXKEY pKey;
      ZH_ERRCODE retval = ZH_SUCCESS;
      ZH_BOOL  fEOF = ZH_FALSE, fLast;
      ZH_ULONG ulRec;

      if( pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped )
         SELF_FORCEREL( &pArea->dbfarea.area );

      pArea->dbfarea.area.fTop = pArea->dbfarea.area.fBottom = ZH_FALSE;
      pArea->dbfarea.area.fEof = ZH_FALSE;

      if( pTag->UsrUnique )
         fLast = ! pTag->UsrAscend;
      else
         fLast = pTag->UsrAscend ? fFindLast : ! fFindLast;

      /* TODO: runtime error if ValType(pKeyItm) != pTag->Type */
      pKey = zh_cdxKeyPutItem( NULL, pKeyItm,
                               fLast ? CDX_MAX_REC_NUM : CDX_IGNORE_REC_NUM,
                               pTag, CDX_CMP_PREFIX );

      zh_cdxIndexLockRead( pTag->pIndex );
      zh_cdxTagRefreshScope( pTag );
      ulRec = zh_cdxTagKeyFind( pTag, pKey );
      if( ( ulRec == 0 && ! fSoftSeek ) || pTag->TagEOF )
         fEOF = ZH_TRUE;
      else /* if( fSoftSeek ) */
      {
         if( ! zh_cdxBottomScope( pTag ) )
            fEOF = ZH_TRUE;
         else if( ! zh_cdxTopScope( pTag ) )
         {
            zh_cdxTagGoTop( pTag );
            if( pTag->CurKey->rec == 0 )
               fEOF = ZH_TRUE;
         }
      }
      zh_cdxIndexUnLockRead( pTag->pIndex );
      if( ! fEOF )
      {
         retval = SELF_GOTO( &pArea->dbfarea.area, pTag->CurKey->rec );
         if( retval != ZH_FAILURE && pArea->dbfarea.fPositioned )
         {
            retval = SELF_SKIPFILTER( &pArea->dbfarea.area, fFindLast ? -1 : 1 );
            if( retval != ZH_FAILURE && ulRec && pArea->dbfarea.fPositioned )
            {
               pArea->dbfarea.area.fFound = ( ulRec == pArea->dbfarea.ulRecNo ||
                        zh_cdxValCompare( pTag, pKey->val, pKey->len,
                                          pTag->CurKey->val, pTag->CurKey->len,
                                          pKey->mode ) == 0 );
               if( ! pArea->dbfarea.area.fFound && ! fSoftSeek )
                  fEOF = ZH_TRUE;
            }
         }
      }
      if( retval != ZH_FAILURE &&
          ( fEOF || ! zh_cdxTopScope( pTag ) ||
                    ! zh_cdxBottomScope( pTag ) ) )
      {
         retval = SELF_GOTO( &pArea->dbfarea.area, 0 );
      }
      pArea->dbfarea.area.fBof = ZH_FALSE;
      zh_cdxKeyFree( pKey );
      return retval;
   }
}

/* ( DBENTRYP_L )     zh_cdxSkip        : NULL */
static ZH_ERRCODE zh_cdxSkip( CDXAREAP pArea, ZH_LONG lToSkip )
{
   LPCDXTAG pTag;
   ZH_ULONG ulPos, ulRec;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_cdxSkip(%p, %ld)", ( void * ) pArea, lToSkip ) );

   if( pArea->dbfarea.lpdbPendingRel )
      SELF_FORCEREL( &pArea->dbfarea.area );

   pTag = lToSkip == 0 ? NULL : zh_cdxGetActiveTag( pArea );
   if( pTag && pArea->dbfarea.fPositioned && CURKEY_LOGPOS( pTag ) )
   {
      ulPos = pTag->logKeyPos;
      ulRec = pTag->logKeyRec;
   }
   else
   {
      ulPos = ulRec = 0;
   }

   if( SUPER_SKIP( &pArea->dbfarea.area, lToSkip ) == ZH_FAILURE )
      return ZH_FAILURE;

   if( pTag )
   {
      if( ulPos && ( pTag->logKeyPos != ulPos || pTag->logKeyRec != ulRec ||
          ( pTag->curKeyState & CDX_CURKEY_LOGPOS ) == 0 ) )
      {
         ulPos = 0;
      }

      if( lToSkip > 0 )
      {
         if( pArea->dbfarea.area.fEof )
         {
            if( lToSkip == 1 && ulPos && ! CURKEY_LOGCNT( pTag ) )
            {
               pTag->logKeyCount = ulPos;
               pTag->curKeyState |= CDX_CURKEY_LOGCNT;
            }
         }
         else if( ulPos )
         {
            pTag->logKeyPos += lToSkip;
            pTag->logKeyRec = pArea->dbfarea.ulRecNo;
         }
      }
      else if( pArea->dbfarea.area.fBof )
      {
         if( pArea->dbfarea.fPositioned )
         {
            pTag->logKeyPos = 1;
            CURKEY_SETLOGPOS( pTag );
         }
      }
      else if( ulPos )
      {
         pTag->logKeyPos += lToSkip;
         pTag->logKeyRec = pArea->dbfarea.ulRecNo;
      }
   }
   return ZH_SUCCESS;
}

/* ( DBENTRYP_L )     zh_cdxSkipFilter  : NULL */

/* ( DBENTRYP_L )     zh_cdxSkipRaw */
static ZH_ERRCODE zh_cdxSkipRaw( CDXAREAP pArea, ZH_LONG lToSkip )
{
   LPCDXTAG pTag;
   ZH_ERRCODE retval;
   ZH_BOOL fOut = ZH_FALSE, fForward;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_cdxSkipRaw(%p, %ld)", ( void * ) pArea, lToSkip ) );

   if( FAST_GOCOLD( &pArea->dbfarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   pTag = zh_cdxGetActiveTag( pArea );

   if( ! pTag || lToSkip == 0 )
      return SUPER_SKIPRAW( &pArea->dbfarea.area, lToSkip );

   if( pArea->dbfarea.lpdbPendingRel )
      SELF_FORCEREL( &pArea->dbfarea.area );

   fForward = ( lToSkip > 0 );

   zh_cdxIndexLockRead( pTag->pIndex );
   zh_cdxTagRefreshScope( pTag );
   if( ! zh_cdxCurKeyRefresh( pArea, pTag ) )
   {
      if( fForward )
      {
         if( pTag->TagEOF || ! zh_cdxBottomScope( pTag ) )
            fOut = ZH_TRUE;
         else if( pTag->UsrAscend && zh_cdxTopScope( pTag ) )
            lToSkip--;
      }
      else if( pArea->dbfarea.fPositioned )
      {
         if( pTag->TagEOF || ! zh_cdxTopScope( pTag ) )
            fOut = ZH_TRUE;
         else if( ! pTag->UsrAscend && zh_cdxBottomScope( pTag ) )
            lToSkip++;
      }
   }
   if( fForward )
   {
      if( ! fOut )
      {
         while( lToSkip-- > 0 )
         {
            zh_cdxTagSkipNext( pTag );
            if( pTag->TagEOF )
            {
               fOut = ZH_TRUE;
               break;
            }
         }
      }
      retval = SELF_GOTO( &pArea->dbfarea.area, ( pTag->TagEOF || fOut )
                                           ? 0 : pTag->CurKey->rec );
   }
   else /* if( lToSkip < 0 ) */
   {
      if( fOut )
         zh_cdxTagGoTop( pTag );
      else
      {
         while( lToSkip++ < 0 )
         {
            zh_cdxTagSkipPrev( pTag );
            if( pTag->TagBOF )
            {
               fOut = ZH_TRUE;
               break;
            }
         }
      }
      retval = SELF_GOTO( &pArea->dbfarea.area, pTag->CurKey->rec );
      pArea->dbfarea.area.fBof = fOut;
   }
   zh_cdxIndexUnLockRead( pTag->pIndex );
   /* Update Bof and Eof flags */
#if 0
   if( fForward )
      pArea->dbfarea.area.fBof = ZH_FALSE;
   else
      pArea->dbfarea.area.fEof = ZH_FALSE;
#endif
   return retval;
}

/* ( DBENTRYP_VF )    zh_cdxAddField        : NULL */
/* ( DBENTRYP_B )     zh_cdxAppend          : NULL */
/* ( DBENTRYP_I )     zh_cdxCreateFields    : NULL */
/* ( DBENTRYP_V )     zh_cdxDeleteRec       : NULL */
/* ( DBENTRYP_BP )    zh_cdxDeleted         : NULL */
/* ( DBENTRYP_SP )    zh_cdxFieldCount      : NULL */
/* ( DBENTRYP_VF )    zh_cdxFieldDisplay    : NULL */
/* ( DBENTRYP_SSI )   zh_cdxFieldInfo       : NULL */
/* ( DBENTRYP_SCP )   zh_cdxFieldName       : NULL */

/* ( DBENTRYP_V )     zh_cdxFlush           : NULL */
/*
 * Flush _system_ buffers to disk
 */
static ZH_ERRCODE zh_cdxFlush( CDXAREAP pArea )
{
   LPCDXINDEX pIndex;
   ZH_ERRCODE errCode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_cdxFlush(%p)", ( void * ) pArea ) );

   if( SELF_GOCOLD( &pArea->dbfarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   errCode = SUPER_FLUSH( &pArea->dbfarea.area );

   if( zh_setGetHardCommit() )
   {
      pIndex = pArea->lpIndexes;
      while( pIndex )
      {
         if( pIndex->pFile && pIndex->fFlush )
         {
            zh_fileCommit( pIndex->pFile );
            pIndex->fFlush = ZH_FALSE;
         }
         pIndex = pIndex->pNext;
      }
   }

   return errCode;
}

/* ( DBENTRYP_PP )    zh_cdxGetRec          : NULL */
/* ( DBENTRYP_SI )    zh_cdxGetValue        : NULL */
/* ( DBENTRYP_SVL )   zh_cdxGetVarLen       : NULL */

/* ( DBENTRYP_V )     zh_cdxGoCold */
/*
 * Perform a write of WorkArea memory to the data store.
 */
static ZH_ERRCODE zh_cdxGoCold( CDXAREAP pArea )
{
   ZH_BOOL fRecordChanged = pArea->dbfarea.fRecordChanged;
   ZH_BOOL fAppend = pArea->dbfarea.fAppend;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_cdxGoCold(%p)", ( void * ) pArea ) );

   if( SUPER_GOCOLD( &pArea->dbfarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   if( ( fRecordChanged || pArea->fCdxAppend ) && pArea->lpIndexes )
   {
      LPCDXTAG pTag = pArea->lpIndexes->TagList;
      LPCDXKEY pKey = NULL;
      ZH_BOOL fAdd, fDel, fLck = ZH_FALSE;
      LPDBRELINFO lpdbPendingRel;

      if( pArea->dbfarea.fShared )
      {
         if( fAppend )
         {
            if( pArea->fCdxAppend )
               zh_cdxErrInternal( "zh_cdxGoCold: multiple appending without GOCOLD." );
            pArea->fCdxAppend = ZH_TRUE;
            return ZH_SUCCESS;
         }
         else
         {
            fAppend = pArea->fCdxAppend;
            pArea->fCdxAppend = ZH_FALSE;
         }
      }

      /* The pending relation may move the record pointer so we should
         disable them for KEY/FOR evaluation */
      lpdbPendingRel = pArea->dbfarea.lpdbPendingRel;
      pArea->dbfarea.lpdbPendingRel = NULL;

      /* TODO:
       * There is possible race condition here but not very dangerous.
       * To avoid it we should Lock all index file before SUPER_GOCOLD
       * but it makes other problem if two stations open the database index
       * files in a different order then they can block each other.
       * Without changes in locking scheme we can do only one thing which
       * is enough if there is only one index file: lock first index only
       * before SUPER_GOCOLD
       * Druzus, 2003-10-05 10:27:52 CEST
       */

      while( pTag )
      {
         if( ! pTag->Custom )
         {
            pKey = zh_cdxKeyEval( pKey, pTag );

            if( pTag->pForItem != NULL )
               fAdd = zh_cdxEvalCond( pArea, pTag->pForItem, ZH_TRUE );
            else
               fAdd = ZH_TRUE;

            if( fAppend )
               fDel = ZH_FALSE;
            else
            {
               if( zh_cdxValCompare( pTag, pKey->val, pKey->len,
                                     pTag->HotKey->val, pTag->HotKey->len,
                                     CDX_CMP_EXACT ) == 0 )
               {
                  fDel = ! fAdd && pTag->HotFor;
                  fAdd = fAdd && ! pTag->HotFor;
               }
               else
               {
                  fDel = pTag->HotFor;
               }
            }
            if( fDel || fAdd )
            {
               if( ! fLck )
               {
                  zh_cdxIndexLockWrite( pTag->pIndex );
                  fLck = ZH_TRUE;
               }
               if( fDel )
                  zh_cdxTagKeyDel( pTag, pTag->HotKey );
               if( fAdd )
                  zh_cdxTagKeyAdd( pTag, pKey );
            }
#if 0
            if( pTag->HotKey )
            {
               zh_cdxKeyFree( pTag->HotKey );
               pTag->HotKey = NULL;
            }
#endif
         }
         if( pTag->pNext )
            pTag = pTag->pNext;
         else
         {
            if( fLck )
            {
               zh_cdxIndexUnLockWrite( pTag->pIndex );
               fLck = ZH_FALSE;
            }
            if( pTag->pIndex->pNext )
               pTag = pTag->pIndex->pNext->TagList;
            else
               pTag = NULL;
         }
      }

      if( pKey )
         zh_cdxKeyFree( pKey );

      /* Restore disabled pending relation */
      pArea->dbfarea.lpdbPendingRel = lpdbPendingRel;
   }

   return ZH_SUCCESS;
}

/* ( DBENTRYP_V )     zh_cdxGoHot */
/*
 * Mark the WorkArea data buffer as hot.
 */
static ZH_ERRCODE zh_cdxGoHot( CDXAREAP pArea )
{

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_cdxGoHot(%p)", ( void * ) pArea ) );

   if( pArea->dbfarea.fRecordChanged )
      zh_cdxErrInternal( "zh_cdxGoHot: multiple marking buffer as hot." );

   if( SUPER_GOHOT( &pArea->dbfarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   if( pArea->lpIndexes && ! pArea->fCdxAppend )
   {
      LPCDXTAG pTag = pArea->lpIndexes->TagList;
      while( pTag )
      {
         if( ! pTag->Custom )
         {
            pTag->HotKey = zh_cdxKeyEval( pTag->HotKey, pTag );
            pTag->HotFor = pTag->pForItem == NULL || zh_cdxEvalCond( pArea, pTag->pForItem, ZH_TRUE );
         }

         if( pTag->pNext )
            pTag = pTag->pNext;
         else
         {
            if( pTag->pIndex->pNext )
               pTag = pTag->pIndex->pNext->TagList;
            else
               pTag = NULL;
         }
      }
   }
   return ZH_SUCCESS;
}

/* ( DBENTRYP_P )     zh_cdxPutRec          : NULL */
/* ( DBENTRYP_SI )    zh_cdxPutValue        : NULL */
/* ( DBENTRYP_V )     zh_cdxRecall          : NULL */
/* ( DBENTRYP_ULP )   zh_cdxRecCount        : NULL */
/* ( DBENTRYP_ISI )   zh_cdxRecInfo         : NULL */
/* ( DBENTRYP_ULP )   zh_cdxRecNo           : NULL */
/* ( DBENTRYP_I )     zh_cdxRecId           : NULL */
/* ( DBENTRYP_S )     zh_cdxSetFieldExtent  : NULL */
/* ( DBENTRYP_CP )    zh_cdxAlias           : NULL */

/* ( DBENTRYP_V )     zh_cdxClose */
/*
 * Close the table in the WorkArea.
 */
static ZH_ERRCODE zh_cdxClose( CDXAREAP pArea )
{
   ZH_ERRCODE errCode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_cdxClose(%p)", ( void * ) pArea ) );

   if( SELF_GOCOLD( &pArea->dbfarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   errCode = SUPER_CLOSE( &pArea->dbfarea.area );

   if( errCode == ZH_SUCCESS )
   {
      if( pArea->pSort )
      {
         zh_cdxSortFree( pArea->pSort );
         pArea->pSort = NULL;
      }

      zh_cdxOrdListClear( pArea, ZH_TRUE, NULL );
#ifdef ZH_CDX_DBGTIME
      printf( "\r\ncdxTimeIntBld=%f, cdxTimeExtBld=%f, cdxTimeBld=%f\r\n"
              "cdxTimeGetKey=%f, cdxTimeFreeKey=%f\r\n"
              "cdxTimeExtBlc=%f, cdxTimeIntBlc=%f\r\n"
              "cdxTimeIdxBld=%f\r\n"
              "cdxTimeTotal=%f\r\n",
              ( double ) cdxTimeIntBld / 1000000, ( double ) cdxTimeExtBld / 1000000,
              ( double ) ( cdxTimeIntBld + cdxTimeExtBld ) / 1000000,
              ( double ) cdxTimeGetKey / 1000000, ( double ) cdxTimeFreeKey / 1000000,
              ( double ) cdxTimeIntBlc / 1000000, ( double ) cdxTimeExtBlc / 1000000,
              ( double ) cdxTimeIdxBld / 1000000,
              ( double ) ( cdxTimeIntBld + cdxTimeExtBld + cdxTimeIdxBld +
                           cdxTimeGetKey + cdxTimeFreeKey +
                           cdxTimeExtBlc + cdxTimeIntBlc ) / 1000000 );
      fflush( stdout );
      cdxTimeIntBld = cdxTimeExtBld = 0;
#endif
#ifdef ZH_CDX_DBGUPDT
      printf( "\r\n#reads=%ld, #writes=%ld, stacksize=%d\r\n", cdxReadNO, cdxWriteNO, cdxStackSize );
      fflush( stdout );
      cdxReadNO = cdxWriteNO = 0;
#endif
   }

   return errCode;
}

/* ( DBENTRYP_VO )    zh_cdxCreate          : NULL */
/* ( DBENTRYP_SI )    zh_cdxInfo            : NULL */
/* ( DBENTRYP_V )     zh_cdxNewArea         : NULL */

/* ( DBENTRYP_VO )    zh_cdxOpen */
/*
 * Open a data store in the WorkArea.
 */
static ZH_ERRCODE zh_cdxOpen( CDXAREAP pArea, LPDBOPENINFO pOpenInfo )
{
   ZH_ERRCODE errCode = ZH_SUCCESS;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_cdxOpen(%p, %p)", ( void * ) pArea, ( void * ) pOpenInfo ) );

   if( ! pArea->dbfarea.bLockType )
   {
      PZH_ITEM pItem = zh_itemNew( NULL );
      if( SELF_INFO( &pArea->dbfarea.area, DBI_LOCKSCHEME, pItem ) != ZH_SUCCESS )
      {
         zh_itemRelease( pItem );
         return ZH_FAILURE;
      }
      pArea->dbfarea.bLockType = ( ZH_BYTE ) zh_itemGetNI( pItem );
      zh_itemRelease( pItem );
      if( pArea->dbfarea.bLockType == 0 )
      {
         pArea->dbfarea.bLockType = DB_DBFLOCK_VFP;
      }
   }
   if( SUPER_OPEN( &pArea->dbfarea.area, pOpenInfo ) == ZH_FAILURE )
   {
      return ZH_FAILURE;
   }

   /* open (production) structural index */
   if( DBFAREA_DATA( &pArea->dbfarea )->fStrictStruct ? pArea->dbfarea.fHasTags : zh_setGetAutOpen() )
   {
      char szFileName[ ZH_PATH_MAX ];

      pArea->dbfarea.fHasTags = ZH_FALSE;
      zh_cdxCreateFName( pArea, NULL, NULL, szFileName, NULL );
      /* CL5.3/COMIX CDX RDDs looks for production indexes
         only in the directory where DBF file is located but
         CL5.2/SIXCDX RDDs also respects SET PATH [druzus] */
      if( DBFAREA_DATA( &pArea->dbfarea )->fStrictStruct ||
#if defined( ZH_SIXCDX )
          zh_fileExists( szFileName, szFileName ) )
#else
          zh_fileExists( szFileName, NULL ) )
#endif
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
   else
   {
      pArea->dbfarea.fHasTags = ZH_FALSE;
   }

   return errCode;
}

/* ( DBENTRYP_V )     zh_cdxRelease         : NULL */

/* ( DBENTRYP_SP )    zh_cdxStructSize */
/*
 * Retrieve the size of the WorkArea structure.
 */
static ZH_ERRCODE zh_cdxStructSize( CDXAREAP pArea, ZH_USHORT * uiSize )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_cdxStrucSize(%p, %p)", ( void * ) pArea, ( void * ) uiSize ) );
   ZH_SYMBOL_UNUSED( pArea );

   *uiSize = sizeof( CDXAREA );
   return ZH_SUCCESS;
}

/* ( DBENTRYP_CP )    zh_cdxSysName         : NULL */

/* ( DBENTRYP_VEI )   zh_cdxEval            : NULL */

/* ( DBENTRYP_V )     zh_cdxPack */
static ZH_ERRCODE zh_cdxPack( CDXAREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_cdxPack(%p)", ( void * ) pArea ) );

   if( FAST_GOCOLD( &pArea->dbfarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   if( SUPER_PACK( &pArea->dbfarea.area ) == ZH_SUCCESS )
   {
      return SELF_ORDLSTREBUILD( &pArea->dbfarea.area );
   }
   else
      return ZH_FAILURE;
}

/* ( DBENTRYP_LSP )   zh_cdxPackRec         : NULL */
/* ( DBENTRYP_VS )    zh_cdxSort            : NULL */
/* ( DBENTRYP_VT )    zh_cdxTrans           : NULL */
/* ( DBENTRYP_VT )    zh_cdxTransRec        : NULL */

/* ( DBENTRYP_V )     zh_cdxZap */
static ZH_ERRCODE zh_cdxZap( CDXAREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "nb_cdxZap(%p)", ( void * ) pArea ) );

   if( FAST_GOCOLD( &pArea->dbfarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   if( SUPER_ZAP( &pArea->dbfarea.area ) == ZH_SUCCESS )
   {
      return SELF_ORDLSTREBUILD( &pArea->dbfarea.area );
   }
   else
      return ZH_FAILURE;
}

/* ( DBENTRYP_VR )    zh_cdxChildEnd        : NULL */
/* ( DBENTRYP_VR )    zh_cdxChildStart      : NULL */
/* ( DBENTRYP_VR )    zh_cdxChildSync       : NULL */
/* ( DBENTRYP_V )     zh_cdxSyncChildren    : NULL */
/* ( DBENTRYP_V )     zh_cdxClearRel        : NULL */
/* ( DBENTRYP_V )     zh_cdxForceRel        : NULL */
/* ( DBENTRYP_SSP )   zh_cdxRelArea         : NULL */
/* ( DBENTRYP_VR )    zh_cdxRelEval         : NULL */
/* ( DBENTRYP_SI )    zh_cdxRelText         : NULL */
/* ( DBENTRYP_VR )    zh_cdxSetRel          : NULL */

/* ( DBENTRYP_VOI )   zh_cdxOrderListAdd */
static ZH_ERRCODE zh_cdxOrderListAdd( CDXAREAP pArea, LPDBORDERINFO pOrderInfo )
{
   ZH_FATTR nFlags;
   PZH_FILE pFile;
   char szBaseName[ CDX_MAXTAGNAMELEN + 1 ];
   char szFileName[ ZH_PATH_MAX ];
   LPCDXINDEX pIndex, * pIndexPtr;
   ZH_BOOL fProd, bRetry;
   PZH_ITEM pError = NULL;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_cdxOrderListAdd(%p, %p)", ( void * ) pArea, ( void * ) pOrderInfo ) );

   if( FAST_GOCOLD( &pArea->dbfarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   if( zh_itemGetCLen( pOrderInfo->atomBagName ) == 0 )
      return ZH_FAILURE;

   zh_cdxCreateFName( pArea, zh_itemGetCPtr( pOrderInfo->atomBagName ),
                      &fProd, szFileName, szBaseName );

#if 0
   if( ! szBaseName[ 0 ] )
      return ZH_FAILURE;
#endif
   pIndex = zh_cdxFindBag( pArea, szFileName );

   if( pIndex )
   {
      /* index already open, do nothing */
      if( ! pArea->uiTag )
      {
         pArea->uiTag = zh_cdxGetTagNumber( pArea, pIndex->TagList );
         SELF_GOTOP( &pArea->dbfarea.area );
      }
      return ZH_SUCCESS;
   }

   nFlags = ( pArea->dbfarea.fReadonly ? FO_READ : FO_READWRITE ) |
            ( pArea->dbfarea.fShared ? FO_DENYNONE : FO_EXCLUSIVE ) |
            FXO_DEFAULTS | FXO_SHARELOCK | FXO_COPYNAME | FXO_NOSEEKPOS;
   do
   {
      pFile = zh_fileExtOpen( szFileName, NULL, nFlags, NULL, pError );
      if( ! pFile )
         bRetry = zh_cdxErrorRT( pArea, EG_OPEN, EDBF_OPEN_INDEX, szFileName,
                                 zh_fsError(), EF_CANRETRY | EF_CANDEFAULT,
                                 &pError ) == E_RETRY;
      else
      {
         if( zh_fileSize( pFile ) <= ( ZH_FOFFSET ) sizeof( CDXTAGHEADER ) )
         {
            zh_fileClose( pFile );
            pFile = NULL;
            zh_cdxErrorRT( pArea, EG_CORRUPTION, EDBF_CORRUPT,
                           szFileName, zh_fsError(), EF_CANDEFAULT, NULL );
         }
         bRetry = ZH_FALSE;
      }

   }
   while( bRetry );

   if( pError )
      zh_errRelease( pError );

   if( ! pFile )
   {
      return ZH_FAILURE;
   }

   pIndex = zh_cdxIndexNew( pArea );
   pIndex->pFile      = pFile;
   pIndex->fShared    = pArea->dbfarea.fShared;
   pIndex->fReadonly  = pArea->dbfarea.fReadonly;
   pIndex->szFileName = zh_strdup( szFileName );

   pIndexPtr = &pArea->lpIndexes;
   while( *pIndexPtr != NULL )
      pIndexPtr = &( *pIndexPtr )->pNext;
   *pIndexPtr = pIndex;

   if( ! zh_cdxIndexLoad( pIndex, szBaseName ) )
   {
      /* index file is corrupted */
      *pIndexPtr = NULL;
      zh_cdxIndexFree( pIndex );
      zh_cdxErrorRT( pArea, EG_CORRUPTION, EDBF_CORRUPT,
                     szFileName, zh_fsError(), EF_CANDEFAULT, NULL );
      return ZH_FAILURE;
   }

   if( fProd )
      pArea->dbfarea.fHasTags = ZH_TRUE;

   if( ! pArea->uiTag )
   {
      pArea->uiTag = zh_cdxGetTagNumber( pArea, pIndex->TagList );
      SELF_GOTOP( &pArea->dbfarea.area );
   }
   return ZH_SUCCESS;
}

/* ( DBENTRYP_V )     zh_cdxOrderListClear */
/*
 * Clear the current order list.
 */
static ZH_ERRCODE zh_cdxOrderListClear( CDXAREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_cdxOrderListClear(%p)", ( void * ) pArea ) );

   if( FAST_GOCOLD( &pArea->dbfarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   zh_cdxOrdListClear( pArea, !( DBFAREA_DATA( &pArea->dbfarea )->fStrictStruct ?
                       pArea->dbfarea.fHasTags : zh_setGetAutOpen() ), NULL );
   pArea->uiTag = 0;

   return ZH_SUCCESS;
}

/* ( DBENTRYP_VOI )   zh_cdxOrderListDelete */
static ZH_ERRCODE zh_cdxOrderListDelete( CDXAREAP pArea, LPDBORDERINFO pOrderInfo )
{
   char szTagName[ CDX_MAXTAGNAMELEN + 1 ];
   char szFileName[ ZH_PATH_MAX ];
   LPCDXINDEX pIndex, * pIndexPtr;
   ZH_BOOL fProd;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_cdxOrderListDelete(%p, %p)", ( void * ) pArea, ( void * ) pOrderInfo ) );

   if( FAST_GOCOLD( &pArea->dbfarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   zh_cdxCreateFName( pArea, zh_itemGetCPtr( pOrderInfo->atomBagName ), &fProd,
                      szFileName, szTagName );

   if( fProd && ( DBFAREA_DATA( &pArea->dbfarea )->fStrictStruct ?
                  pArea->dbfarea.fHasTags : zh_setGetAutOpen() ) )
      pIndex = NULL;
   else
      pIndex = zh_cdxFindBag( pArea, szFileName );

   if( pIndex )
   {
      LPCDXTAG pTag = zh_cdxGetActiveTag( pArea );
      if( pTag && pTag->pIndex == pIndex )
         pArea->uiTag = 0;
      pIndexPtr = &pArea->lpIndexes;
      while( *pIndexPtr )
      {
         if( pIndex == *pIndexPtr )
         {
            *pIndexPtr = pIndex->pNext;
            zh_cdxIndexFree( pIndex );
            break;
         }
         pIndexPtr = &( *pIndexPtr )->pNext;
      }
   }
   return ZH_SUCCESS;
}

/* ( DBENTRYP_VOI )   zh_cdxOrderListFocus */
static ZH_ERRCODE zh_cdxOrderListFocus( CDXAREAP pArea, LPDBORDERINFO pOrderInfo )
{
   LPCDXTAG pTag;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_cdxOrderListFocus(%p, %p)", ( void * ) pArea, ( void * ) pOrderInfo ) );

   if( FAST_GOCOLD( &pArea->dbfarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   if( ! pArea->lpIndexes )
      return ZH_SUCCESS;

   pTag = zh_cdxGetActiveTag( pArea );
   if( pTag )
      pOrderInfo->itmResult = zh_itemPutC( pOrderInfo->itmResult, pTag->szName );

   if( pOrderInfo->itmOrder )
      zh_cdxFindTag( pArea, pOrderInfo->itmOrder, pOrderInfo->atomBagName, &pArea->uiTag );

   return ZH_SUCCESS;
}

/* ( DBENTRYP_V )     zh_cdxOrderListRebuild */
static ZH_ERRCODE zh_cdxOrderListRebuild( CDXAREAP pArea )
{
   LPCDXINDEX pIndex, * pIndexPtr;
   ZH_USHORT uiPrevTag;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_cdxPack(%p)", ( void * ) pArea ) );

   if( FAST_GOCOLD( &pArea->dbfarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   if( pArea->dbfarea.fShared )
   {
      zh_cdxErrorRT( pArea, EG_SHARED, EDBF_SHARED, pArea->dbfarea.szDataFileName, 0, 0, NULL );
      return ZH_FAILURE;
   }
   if( pArea->dbfarea.fReadonly )
   {
      zh_cdxErrorRT( pArea, EG_READONLY, EDBF_READONLY, pArea->dbfarea.szDataFileName, 0, 0, NULL );
      return ZH_FAILURE;
   }

   if( ! pArea->lpIndexes )
      return ZH_SUCCESS;

   uiPrevTag = pArea->uiTag;
   pArea->uiTag = 0;

   pIndex = pArea->lpIndexes;
   pArea->lpIndexes = NULL;
   pIndexPtr = &pArea->lpIndexes;
   while( pIndex )
   {
      ( *pIndexPtr ) = pIndex;
      pIndex = pIndex->pNext;
      ( *pIndexPtr )->pNext = NULL;
      zh_cdxIndexReindex( *pIndexPtr );
      pIndexPtr = &( *pIndexPtr )->pNext;
   }

   pArea->uiTag = uiPrevTag;
   /* Clear pArea->dbfarea.area.lpdbOrdCondInfo */
   SELF_ORDSETCOND( &pArea->dbfarea.area, NULL );

   return SELF_GOTOP( &pArea->dbfarea.area );
}

/* ( DBENTRYP_VOO )   zh_cdxOrderCondition  : NULL */

/* ( DBENTRYP_VOC )   zh_cdxOrderCreate */
/*
 * create new order
 */
static ZH_ERRCODE zh_cdxOrderCreate( CDXAREAP pArea, LPDBORDERCREATEINFO pOrderInfo )
{
   ZH_ULONG ulRecNo;
   ZH_BOOL fNewFile, fOpenedIndex, fProd, fAscend = ZH_TRUE, fNoCase = ZH_FALSE,
        fCustom = ZH_FALSE, fTemporary = ZH_FALSE, fExclusive = ZH_FALSE;
   PZH_ITEM pKeyExp, pForExp = NULL, pResult;
   char szCpndTagName[ CDX_MAXTAGNAMELEN + 1 ], szTagName[ CDX_MAXTAGNAMELEN + 1 ];
   char szFileName[ ZH_PATH_MAX ];
   const char * szFor = NULL;
   LPCDXINDEX pIndex;
   LPCDXTAG pTag;
   ZH_USHORT uiLen;
   ZH_BYTE bType;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_cdxOrderCreate(%p, %p)", ( void * ) pArea, ( void * ) pOrderInfo ) );

   if( SELF_GOCOLD( &pArea->dbfarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   if( pArea->dbfarea.lpdbPendingRel )
      SELF_FORCEREL( &pArea->dbfarea.area );

   if( zh_strlentrim( zh_itemGetCPtr( pOrderInfo->abExpr ) ) +
       ( pArea->dbfarea.area.lpdbOrdCondInfo && pArea->dbfarea.area.lpdbOrdCondInfo->abFor ?
         zh_strlentrim( pArea->dbfarea.area.lpdbOrdCondInfo->abFor ) : 0 ) >
       CDX_HEADEREXPLEN - 2 )
   {
      zh_cdxErrorRT( pArea, EG_DATAWIDTH, EDBF_KEYLENGTH, NULL, 0, 0, NULL );
      return ZH_FAILURE;
   }

   if( SELF_COMPILE( &pArea->dbfarea.area, zh_itemGetCPtr( pOrderInfo->abExpr ) ) == ZH_FAILURE )
   {
      if( pOrderInfo->itmCobExpr )
      {
         pKeyExp = zh_itemNew( pOrderInfo->itmCobExpr );
      }
      else
      {
         zh_cdxErrorRT( pArea, EG_DATATYPE, EDBF_INVALIDKEY, NULL, 0, 0, NULL );
         return ZH_FAILURE;
      }
   }
   else
   {
      pKeyExp = pArea->dbfarea.area.valResult;
      pArea->dbfarea.area.valResult = NULL;
      /* If we have a codeblock for the expression, use it */
      if( pOrderInfo->itmCobExpr )
      {
         zh_vmDestroyBlockOrMacro( pKeyExp );
         pKeyExp = zh_itemNew( pOrderInfo->itmCobExpr );
      }
   }

   /* Get a blank record before testing expression */
   ulRecNo = pArea->dbfarea.ulRecNo;
   SELF_GOTO( &pArea->dbfarea.area, 0 );
   if( SELF_EVALBLOCK( &pArea->dbfarea.area, pKeyExp ) == ZH_FAILURE )
   {
      zh_vmDestroyBlockOrMacro( pKeyExp );
      SELF_GOTO( &pArea->dbfarea.area, ulRecNo );
      return ZH_FAILURE;
   }
   pResult = pArea->dbfarea.area.valResult;
   pArea->dbfarea.area.valResult = NULL;

   bType = zh_cdxItemType( pResult );
   switch( bType )
   {
      case 'N':
      case 'D':
      case 'T':
         uiLen = 8;
         break;
      case 'L':
         uiLen = 1;
         break;
      case 'C':
      {
         ZH_SIZE nLen = zh_itemGetCLen( pResult );
         if( nLen > USHRT_MAX )
            nLen = USHRT_MAX;
         uiLen = ( ZH_USHORT ) nLen;
         break;
      }
      default:
         bType = 'U';
         uiLen = 0;
   }
   zh_itemRelease( pResult );

   /* Make sure KEY has proper type and length */
   if( bType == 'U' || uiLen == 0 )
   {
      zh_vmDestroyBlockOrMacro( pKeyExp );
      SELF_GOTO( &pArea->dbfarea.area, ulRecNo );
      zh_cdxErrorRT( pArea, bType == 'U' ? EG_DATATYPE : EG_DATAWIDTH, EDBF_INVALIDKEY, NULL, 0, 0, NULL );
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
      if( szFor )
      {
         if( SELF_COMPILE( &pArea->dbfarea.area, szFor ) == ZH_FAILURE )
         {
            zh_vmDestroyBlockOrMacro( pKeyExp );
            SELF_GOTO( &pArea->dbfarea.area, ulRecNo );
            zh_cdxErrorRT( pArea, EG_DATATYPE, EDBF_INVALIDFOR, NULL, 0, 0, NULL );
            return ZH_FAILURE;
         }
         pForExp = pArea->dbfarea.area.valResult;
         pArea->dbfarea.area.valResult = NULL;
      }
      /* If we have a codeblock for the conditional expression, use it */
      if( pArea->dbfarea.area.lpdbOrdCondInfo->itmCobFor )
      {
         if( pForExp )
            zh_vmDestroyBlockOrMacro( pForExp );
         pForExp = zh_itemNew( pArea->dbfarea.area.lpdbOrdCondInfo->itmCobFor );
      }
   }

   if( pArea->dbfarea.fTemporary )
      fTemporary = ZH_TRUE;

   /* Test conditional expression */
   if( pForExp )
   {
      ZH_BOOL fOK;

      if( SELF_EVALBLOCK( &pArea->dbfarea.area, pForExp ) == ZH_FAILURE )
      {
         zh_vmDestroyBlockOrMacro( pKeyExp );
         zh_vmDestroyBlockOrMacro( pForExp );
         SELF_GOTO( &pArea->dbfarea.area, ulRecNo );
         return ZH_FAILURE;
      }
      fOK = zh_itemType( pArea->dbfarea.area.valResult ) & ZH_IT_LOGICAL;
      zh_itemRelease( pArea->dbfarea.area.valResult );
      pArea->dbfarea.area.valResult = NULL;
      if( ! fOK )
      {
         zh_vmDestroyBlockOrMacro( pKeyExp );
         zh_vmDestroyBlockOrMacro( pForExp );
         SELF_GOTO( &pArea->dbfarea.area, ulRecNo );
         zh_cdxErrorRT( pArea, EG_DATATYPE, EDBF_INVALIDFOR, NULL, 0, 0, NULL );
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

   zh_cdxCreateFName( pArea, pOrderInfo->abBagName,
                      &fProd, szFileName, szCpndTagName );

   if( pOrderInfo->atomBagName && pOrderInfo->atomBagName[ 0 ] )
   {
      zh_strncpyUpperTrim( szTagName, pOrderInfo->atomBagName, sizeof( szTagName ) - 1 );
      fNewFile = ZH_FALSE;
   }
   else
   {
      zh_strncpy( szTagName, szCpndTagName, sizeof( szTagName ) - 1 );
      fNewFile = ZH_TRUE;
   }

   if( ! pArea->dbfarea.area.lpdbOrdCondInfo ||
       ( pArea->dbfarea.area.lpdbOrdCondInfo->fAll && ! pArea->dbfarea.area.lpdbOrdCondInfo->fAdditive ) )
      zh_cdxOrdListClear( pArea, ! ( DBFAREA_DATA( &pArea->dbfarea )->fStrictStruct ?
                          pArea->dbfarea.fHasTags : zh_setGetAutOpen() ), NULL );

   pIndex = zh_cdxFindBag( pArea, szFileName );

   if( fNewFile && pIndex != NULL )
   {
      LPCDXINDEX * pIndexPtr = &pArea->lpIndexes;
      while( *pIndexPtr )
      {
         if( pIndex == *pIndexPtr )
         {
            *pIndexPtr = pIndex->pNext;
            break;
         }
         pIndexPtr = &( *pIndexPtr )->pNext;
      }
      zh_cdxIndexFree( pIndex );
      pIndex = NULL;
   }
   fOpenedIndex = ( pIndex != NULL );

   if( ! fOpenedIndex )
   {
      char szTempFile[ ZH_PATH_MAX ];
      PZH_FILE pFile;
      ZH_BOOL bRetry, fShared = pArea->dbfarea.fShared && ! fTemporary && ! fExclusive;
      PZH_ITEM pError = NULL;

      do
      {
         if( fTemporary )
         {
            pFile = zh_fileCreateTemp( NULL, NULL, FC_NORMAL, szTempFile );
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
            bRetry = zh_cdxErrorRT( pArea, EG_CREATE, EDBF_CREATE_INDEX, szFileName,
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

      if( pFile )
      {
         pIndex = zh_cdxIndexNew( pArea );
         pIndex->pFile      = pFile;
         pIndex->fShared    = fShared;
         pIndex->fReadonly  = ZH_FALSE;
         pIndex->szFileName = zh_strdup( szFileName );
         pIndex->fDelete    = fTemporary;
         if( fTemporary )
            pIndex->szRealName = zh_strdup( szTempFile );

         if( ! fNewFile )
         {
            /* index file is corrupted? */
            if( ! zh_cdxIndexLoad( pIndex, szCpndTagName ) )
            {
               /* TODO: What should be default? */
#if 0
               zh_cdxIndexFree( pIndex );
               zh_fileClose( pFile );
               pFile = NULL;
               zh_cdxErrorRT( pArea, EG_CORRUPTION, EDBF_CORRUPT,
                              szFileName, zh_fsError(), EF_CANDEFAULT, NULL );
#endif
               zh_cdxIndexFreeTags( pIndex );
               fNewFile = ZH_TRUE;
            }
         }
      }

      if( ! pFile )
      {
         zh_vmDestroyBlockOrMacro( pKeyExp );
         if( pForExp != NULL )
            zh_vmDestroyBlockOrMacro( pForExp );
         return ZH_FAILURE;
      }
   }
   else if( pIndex->fReadonly )
   {
      zh_cdxErrorRT( pArea, EG_READONLY, EDBF_READONLY, pIndex->szFileName, 0, 0, NULL );
      return ZH_FAILURE;
   }

   zh_cdxIndexLockWrite( pIndex );
   zh_cdxIndexLockFlush( pIndex );
   if( ! fNewFile )
   {
      pTag = zh_cdxGetTagByNumber( pArea, pArea->uiTag );
      /* Delete new tag if exist */
      zh_cdxIndexDelTag( pIndex, szTagName );
      pArea->uiTag = zh_cdxGetTagNumber( pArea, pTag );
      fNewFile = ( pIndex->TagList == NULL );
   }

   if( fNewFile )
   {
      zh_fileTruncAt( pIndex->pFile, 0 );
      pIndex->fChanged = ZH_TRUE;
      zh_cdxIndexDropAvailPage( pIndex );
      if( pIndex->pCompound != NULL )
         zh_cdxTagFree( pIndex->pCompound );
      zh_cdxIndexInit( pIndex );
      pIndex->nextAvail = pIndex->freePage = 0;
      zh_cdxIndexCreateStruct( pIndex, szCpndTagName );
   }

   if( uiLen > pIndex->uiMaxKeyLen )
      uiLen = pIndex->uiMaxKeyLen;

   pTag = zh_cdxIndexAddTag( pIndex, szTagName, zh_itemGetCPtr( pOrderInfo->abExpr ),
                             pKeyExp, bType, uiLen, szFor, pForExp,
                             fAscend, pOrderInfo->fUnique, fNoCase, fCustom, ZH_FALSE );

   if( pArea->dbfarea.area.lpdbOrdCondInfo && ( ! pArea->dbfarea.area.lpdbOrdCondInfo->fAll &&
                                                ! pArea->dbfarea.area.lpdbOrdCondInfo->fAdditive ) )
   {
      zh_cdxOrdListClear( pArea, ! ( DBFAREA_DATA( &pArea->dbfarea )->fStrictStruct ?
                          pArea->dbfarea.fHasTags : zh_setGetAutOpen() ), pIndex );
   }
   zh_cdxIndexUnLockWrite( pIndex );
   /* Update DBF header */
   if( ! pArea->dbfarea.fHasTags && ! fOpenedIndex && ! pIndex->fDelete && fProd )
   {
      pArea->dbfarea.fHasTags = ZH_TRUE;
      if( ! pArea->dbfarea.fReadonly && ( pArea->dbfarea.dbfHeader.bHasTags & 0x01 ) == 0 &&
          ( zh_setGetAutOpen() || DBFAREA_DATA( &pArea->dbfarea )->fStrictStruct ) )
         SELF_WRITEDBHEADER( &pArea->dbfarea.area );
   }
   else
   {
      fProd = ZH_FALSE;
   }

   if( ! fOpenedIndex )
   {
      if( fProd || pArea->lpIndexes == NULL )
      {
         pIndex->pNext = pArea->lpIndexes;
         pArea->lpIndexes = pIndex;
      }
      else
      {
         LPCDXINDEX pIndexTmp = pArea->lpIndexes;
         while( pIndexTmp->pNext )
            pIndexTmp = pIndexTmp->pNext;
         pIndexTmp->pNext = pIndex;
      }
   }

   pArea->uiTag = zh_cdxGetTagNumber( pArea, pTag );

   /* Clear pArea->dbfarea.area.lpdbOrdCondInfo */
   SELF_ORDSETCOND( &pArea->dbfarea.area, NULL );

   return SELF_GOTOP( &pArea->dbfarea.area );
}

/* ( DBENTRYP_VOI )   zh_cdxOrderDestroy */
static ZH_ERRCODE zh_cdxOrderDestroy( CDXAREAP pArea, LPDBORDERINFO pOrderInfo )
{
   LPCDXINDEX pIndex, pIndexTmp;
   LPCDXTAG pTag;
   ZH_USHORT uiTag;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_cdxOrderDestroy(%p, %p)", ( void * ) pArea, ( void * ) pOrderInfo ) );

   if( FAST_GOCOLD( &pArea->dbfarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   if( ! pArea->lpIndexes )
      return ZH_SUCCESS;

   if( pOrderInfo->itmOrder )
   {
      pTag = zh_cdxFindTag( pArea, pOrderInfo->itmOrder, pOrderInfo->atomBagName, &uiTag );
      if( pTag )
      {
         pIndex = pTag->pIndex;
         if( /* ! pIndex->fShared && */ ! pIndex->fReadonly )
         {
            zh_cdxIndexLockWrite( pIndex );
            zh_cdxIndexDelTag( pIndex, pTag->szName );
            zh_cdxIndexUnLockWrite( pIndex );
            if( ! pIndex->TagList )
            {
               if( pArea->lpIndexes == pIndex )
               {
                  pArea->lpIndexes = pIndex->pNext;
                  if( pArea->dbfarea.fHasTags )
                  {
                     pArea->dbfarea.fHasTags = ZH_FALSE;
                     if( ! pArea->dbfarea.fReadonly && ( pArea->dbfarea.dbfHeader.bHasTags & 0x01 ) != 0 &&
                         ( zh_setGetAutOpen() || DBFAREA_DATA( &pArea->dbfarea )->fStrictStruct ) )
                        SELF_WRITEDBHEADER( &pArea->dbfarea.area );
                  }
               }
               else
               {
                  pIndexTmp = pArea->lpIndexes;
                  while( pIndexTmp->pNext && ( pIndexTmp->pNext != pIndex ) )
                  {
                     pIndexTmp = pIndexTmp->pNext;
                  }
                  if( pIndexTmp->pNext == pIndex )
                  {
                     pIndexTmp->pNext = pIndex->pNext;
                  }
               }
               pIndex->fDelete = ZH_TRUE;
               zh_cdxIndexFree( pIndex );
            }
            if( uiTag < pArea->uiTag )
               pArea->uiTag--;
            else if( uiTag == pArea->uiTag )
               pArea->uiTag = 0;
         }
         else
         {
            /* TODO: allow this operation for shared mode? */
            zh_errInternal( 1023, "zh_cdxOrderDestroy: exclusive required.", NULL, NULL );
         }
      }
   }
   return ZH_SUCCESS;
}

/* ( DBENTRYP_SVOI )  zh_cdxOrderInfo */
/*
 * Provides information about order management.
 */
static ZH_ERRCODE zh_cdxOrderInfo( CDXAREAP pArea, ZH_USHORT uiIndex, LPDBORDERINFO pInfo )
{
   LPCDXTAG pTag;
   ZH_USHORT uiTag = 0;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_cdxOrderInfo(%p, %hu, %p)", ( void * ) pArea, uiIndex, ( void * ) pInfo ) );

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
                      pArea->pSort ? pArea->pSort->pTag->szName : NULL );
         return ZH_SUCCESS;

      case DBOI_I_BAGNAME:
         pInfo->itmResult = zh_itemPutC( pInfo->itmResult,
                pArea->pSort ? pArea->pSort->pTag->pIndex->szFileName : NULL );
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
         LPCDXINDEX pIndex;
         const char * pszBag = zh_itemGetCLen( pInfo->atomBagName ) > 0 ?
                               zh_itemGetCPtr( pInfo->atomBagName ) : NULL;
         pIndex = pszBag ? zh_cdxFindBag( pArea, pszBag ) : pArea->lpIndexes;
         while( pIndex )
         {
            pTag = pIndex->TagList;
            while( pTag )
            {
               ++uiTag;
               pTag = pTag->pNext;
            }
            pIndex = pszBag ? NULL : pIndex->pNext;
         }
         pInfo->itmResult = zh_itemPutNI( pInfo->itmResult, uiTag );
         return ZH_SUCCESS;
      }

      case DBOI_BAGCOUNT:
      {
         LPCDXINDEX pIndex = pArea->lpIndexes;
         while( pIndex )
         {
            ++uiTag;
            pIndex = pIndex->pNext;
         }
         pInfo->itmResult = zh_itemPutNI( pInfo->itmResult, uiTag );
         return ZH_SUCCESS;
      }

      case DBOI_BAGNUMBER:
      {
         LPCDXINDEX pIndex = pArea->lpIndexes, pIndexSeek;

         if( zh_itemGetCLen( pInfo->atomBagName ) > 0 )
            pIndexSeek = zh_cdxFindBag( pArea,
                                  zh_itemGetCPtr( pInfo->atomBagName ) );
         else
         {
            pTag = zh_cdxGetTagByNumber( pArea, pArea->uiTag );
            pIndexSeek = pTag ? pTag->pIndex : NULL;
         }

         if( pIndexSeek )
         {
            do
            {
               ++uiTag;
               if( pIndex == pIndexSeek )
                  break;
               pIndex = pIndex->pNext;
            }
            while( pIndex );
         }
         pInfo->itmResult = zh_itemPutNI( pInfo->itmResult,
                                               pIndex ? uiTag : 0 );
         return ZH_SUCCESS;
      }

      case DBOI_BAGORDER:
      {
         LPCDXINDEX pIndex = pArea->lpIndexes, pIndexSeek;

         if( zh_itemGetCLen( pInfo->atomBagName ) > 0 )
            pIndexSeek = zh_cdxFindBag( pArea,
                                  zh_itemGetCPtr( pInfo->atomBagName ) );
         else
         {
            pTag = zh_cdxGetTagByNumber( pArea, pArea->uiTag );
            pIndexSeek = pTag ? pTag->pIndex : NULL;
         }

         if( pIndexSeek )
         {
            ++uiTag;
            do
            {
               if( pIndex == pIndexSeek )
                  break;
               pTag = pIndex->TagList;
               while( pTag )
               {
                  ++uiTag;
                  pTag = pTag->pNext;
               }
               pIndex = pIndex->pNext;
            }
            while( pIndex );
         }
         pInfo->itmResult = zh_itemPutNI( pInfo->itmResult,
                                               pIndex ? uiTag : 0 );
         return ZH_SUCCESS;
      }

      case DBOI_RESETPOS:
         zh_cdxClearPosInfo( pArea );
         return ZH_SUCCESS;
   }

   if( FAST_GOCOLD( &pArea->dbfarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   if( pInfo->itmOrder )
   {
      pTag = zh_cdxFindTag( pArea, pInfo->itmOrder, pInfo->atomBagName, &uiTag );
   }
   else
   {
      uiTag = pArea->uiTag;
      pTag = zh_cdxGetTagByNumber( pArea, uiTag );
   }

   switch( uiIndex )
   {
      case DBOI_CONDITION:
         pInfo->itmResult = zh_itemPutC( pInfo->itmResult, pTag ? pTag->ForExpr : NULL );
         if( pTag && pInfo->itmNewVal && ZH_IS_STRING( pInfo->itmNewVal ) )
         {
            if( pTag->ForExpr != NULL )
            {
               zh_xfree( pTag->ForExpr );
               pTag->ForExpr = NULL;
            }
            if( pTag->pForItem != NULL )
            {
               zh_vmDestroyBlockOrMacro( pTag->pForItem );
               pTag->pForItem = NULL;
            }
            if( zh_itemGetCLen( pInfo->itmNewVal ) > 0 )
            {
               const char * pForExpr = zh_itemGetCPtr( pInfo->itmNewVal );

               if( SELF_COMPILE( &pArea->dbfarea.area, pForExpr ) == ZH_SUCCESS )
               {
                  PZH_ITEM pForItem = pArea->dbfarea.area.valResult;
                  pArea->dbfarea.area.valResult = NULL;
                  if( SELF_EVALBLOCK( &pArea->dbfarea.area, pForItem ) == ZH_SUCCESS )
                  {
                     if( zh_itemType( pArea->dbfarea.area.valResult ) & ZH_IT_LOGICAL )
                     {
                        pTag->pForItem = pForItem;
                        pForItem = NULL;
                     }
                     zh_itemRelease( pArea->dbfarea.area.valResult );
                     pArea->dbfarea.area.valResult = NULL;
                  }
                  if( pForItem )
                     zh_vmDestroyBlockOrMacro( pForItem );
               }
            }
         }
         break;

      case DBOI_EXPRESSION:
         pInfo->itmResult = zh_itemPutC( pInfo->itmResult, pTag ? pTag->KeyExpr : NULL );
         break;

      case DBOI_POSITION:
         if( pInfo->itmNewVal && ZH_IS_NUMERIC( pInfo->itmNewVal ) )
         {
            pInfo->itmResult = zh_itemPutL( pInfo->itmResult,
               zh_cdxDBOIKeyGoto( pArea, pTag,
                  zh_itemGetNL( pInfo->itmNewVal ), ZH_TRUE ) == ZH_SUCCESS );
         }
         else
            pInfo->itmResult = zh_itemPutNL( pInfo->itmResult,
                                    zh_cdxDBOIKeyNo( pArea, pTag, ZH_TRUE ) );
         break;

      /* TODO: is this ok?  DBOI_RECNO == DBOI_KEYNORAW ? No, it isn't. */
      /* case DBOI_RECNO: */
      case DBOI_KEYNORAW:
         if( pInfo->itmNewVal && ZH_IS_NUMERIC( pInfo->itmNewVal ) )
         {
            pInfo->itmResult = zh_itemPutL( pInfo->itmResult,
               zh_cdxDBOIKeyGoto( pArea, pTag,
                  zh_itemGetNL( pInfo->itmNewVal ), ZH_FALSE ) == ZH_SUCCESS );
         }
         else
            pInfo->itmResult = zh_itemPutNL( pInfo->itmResult,
                                    zh_cdxDBOIKeyNo( pArea, pTag, ZH_FALSE ) );
         break;

      case DBOI_KEYCOUNT:
         pInfo->itmResult = zh_itemPutNL( pInfo->itmResult,
                                    zh_cdxDBOIKeyCount( pArea, pTag, ZH_TRUE ) );
         break;

      case DBOI_KEYCOUNTRAW:
         pInfo->itmResult = zh_itemPutNL( pInfo->itmResult,
                                    zh_cdxDBOIKeyCount( pArea, pTag, ZH_FALSE ) );
         break;

      case DBOI_RELKEYPOS:
         if( pInfo->itmNewVal && ZH_IS_NUMERIC( pInfo->itmNewVal ) )
            zh_cdxDBOISetRelKeyPos( pArea, pTag,
                                    zh_itemGetND( pInfo->itmNewVal ) );
         else
            pInfo->itmResult = zh_itemPutND( pInfo->itmResult,
                                       zh_cdxDBOIGetRelKeyPos( pArea, pTag ) );
         break;

      case DBOI_FINDREC:
         pInfo->itmResult = zh_itemPutL( pInfo->itmResult,
                  zh_cdxDBOIFindRec( pArea, pTag,
                              zh_itemGetNL( pInfo->itmNewVal ), ZH_FALSE ) );
         break;

      case DBOI_FINDRECCONT:
         pInfo->itmResult = zh_itemPutL( pInfo->itmResult,
                  zh_cdxDBOIFindRec( pArea, pTag,
                              zh_itemGetNL( pInfo->itmNewVal ), ZH_TRUE ) );
         break;

      case DBOI_SKIPUNIQUE:
         pInfo->itmResult = zh_itemPutL( pInfo->itmResult,
                     zh_cdxDBOISkipUnique( pArea, pTag,
                        pInfo->itmNewVal && ZH_IS_NUMERIC( pInfo->itmNewVal ) ?
                        zh_itemGetNL( pInfo->itmNewVal ) : 1 ) == ZH_SUCCESS );
         break;

      case DBOI_SKIPEVAL:
         pInfo->itmResult = zh_itemPutL( pInfo->itmResult,
               zh_cdxDBOISkipEval( pArea, pTag, ZH_TRUE, pInfo->itmNewVal ) );
         break;

      case DBOI_SKIPEVALBACK:
         pInfo->itmResult = zh_itemPutL( pInfo->itmResult,
               zh_cdxDBOISkipEval( pArea, pTag, ZH_FALSE, pInfo->itmNewVal ) );
         break;

      case DBOI_SKIPWILD:
         pInfo->itmResult = zh_itemPutL( pInfo->itmResult,
               zh_cdxDBOISkipWild( pArea, pTag, ZH_TRUE, pInfo->itmNewVal ) );
         break;

      case DBOI_SKIPWILDBACK:
         pInfo->itmResult = zh_itemPutL( pInfo->itmResult,
               zh_cdxDBOISkipWild( pArea, pTag, ZH_FALSE, pInfo->itmNewVal ) );
         break;

      case DBOI_SKIPREGEX:
         pInfo->itmResult = zh_itemPutL( pInfo->itmResult,
            zh_cdxDBOISkipRegEx( pArea, pTag, ZH_TRUE, pInfo->itmNewVal ) );
         break;

      case DBOI_SKIPREGEXBACK:
         pInfo->itmResult = zh_itemPutL( pInfo->itmResult,
            zh_cdxDBOISkipRegEx( pArea, pTag, ZH_FALSE, pInfo->itmNewVal ) );
         break;

      case DBOI_SCOPEEVAL:
         if( pTag && pInfo->itmNewVal &&
             zh_arrayLen( pInfo->itmNewVal ) == DBRMI_SIZE &&
             zh_arrayGetPtr( pInfo->itmNewVal, DBRMI_FUNCTION ) != NULL )
         {
            pInfo->itmResult = zh_itemPutNL( pInfo->itmResult,
                  zh_cdxDBOIScopeEval( pTag, ( ZH_EVALSCOPE_FUNC )
                       zh_arrayGetPtr( pInfo->itmNewVal, DBRMI_FUNCTION ),
                       zh_arrayGetPtr( pInfo->itmNewVal, DBRMI_PARAM ),
                       zh_arrayGetItemPtr( pInfo->itmNewVal, DBRMI_LOVAL ),
                       zh_arrayGetItemPtr( pInfo->itmNewVal, DBRMI_HIVAL ) ) );
         }
         else
         {
            /* TODO: RT error */
            ;
         }
         break;

      case DBOI_NAME:
         pInfo->itmResult = zh_itemPutC( pInfo->itmResult, pTag ? pTag->szName : NULL );
         break;

      case DBOI_NUMBER:
         pInfo->itmResult = zh_itemPutNI( pInfo->itmResult, uiTag );
         break;

      case DBOI_BAGNAME:
         if( pTag )
         {
            PZH_FNAME pFileName = zh_fsFNameSplit( pTag->pIndex->szFileName );
            pInfo->itmResult = zh_itemPutC( pInfo->itmResult, pFileName->szName );
            zh_xfree( pFileName );
         }
         else
            pInfo->itmResult = zh_itemPutC( pInfo->itmResult, NULL );
         break;

      case DBOI_FULLPATH:
         pInfo->itmResult = zh_itemPutC( pInfo->itmResult, pTag ? pTag->pIndex->szFileName : NULL );
         break;

      case DBOI_FILEHANDLE:
         pInfo->itmResult = zh_itemPutNInt( pInfo->itmResult, ( ZH_NHANDLE )
                  ( pTag ? zh_fileHandle( pTag->pIndex->pFile ) : FS_ERROR ) );
         break;

      case DBOI_ISCOND:
         pInfo->itmResult = zh_itemPutL( pInfo->itmResult, pTag && pTag->ForExpr != NULL );
         break;

      case DBOI_ISDESC:
         pInfo->itmResult = zh_itemPutL( pInfo->itmResult, pTag && ! pTag->UsrAscend );
         if( pTag && pInfo->itmNewVal && ZH_IS_LOGICAL( pInfo->itmNewVal ) )
         {
            pTag->UsrAscend = ! zh_itemGetL( pInfo->itmNewVal );
            pTag->curKeyState &= ~( CDX_CURKEY_RAWPOS | CDX_CURKEY_LOGPOS );
         }
         break;

      case DBOI_UNIQUE:
         pInfo->itmResult = zh_itemPutL( pInfo->itmResult, ( pTag ? pTag->UniqueKey || pTag->UsrUnique : ZH_FALSE ) );
         if( pTag && pInfo->itmNewVal && ZH_IS_LOGICAL( pInfo->itmNewVal ) && ! pTag->UniqueKey )
         {
            pTag->UsrUnique = zh_itemGetL( pInfo->itmNewVal );
            pTag->curKeyState &= ~( CDX_CURKEY_RAWPOS | CDX_CURKEY_LOGPOS |
                                    CDX_CURKEY_RAWCNT | CDX_CURKEY_LOGCNT );
         }
         break;

      case DBOI_KEYTYPE:
         if( pTag )
         {
            char szType[ 2 ];
            szType[ 0 ] = ( char ) pTag->uiType;
            szType[ 1 ] = 0;
            pInfo->itmResult = zh_itemPutC( pInfo->itmResult, szType );
         }
         else
            pInfo->itmResult = zh_itemPutC( pInfo->itmResult, NULL );
         break;

      case DBOI_KEYSIZE:
         pInfo->itmResult = zh_itemPutNI( pInfo->itmResult, pTag ? pTag->uiLen : 0 );
         break;

      case DBOI_KEYDEC:
         /* there is no fixed number of decimal places for numeric keys
            in CDX format */
         pInfo->itmResult = zh_itemPutNI( pInfo->itmResult, 0 );
         break;

      case DBOI_KEYVAL:
         zh_itemClear( pInfo->itmResult );
         if( pArea->dbfarea.lpdbPendingRel )
            SELF_FORCEREL( &pArea->dbfarea.area );
         if( pTag && pArea->dbfarea.fPositioned )
         {
            if( pTag->CurKey->rec != pArea->dbfarea.ulRecNo )
            {
               zh_cdxIndexLockRead( pTag->pIndex );
               zh_cdxCurKeyRefresh( pArea, pTag );
               zh_cdxIndexUnLockRead( pTag->pIndex );
            }
            if( pTag->CurKey->rec == pArea->dbfarea.ulRecNo )
               pInfo->itmResult = zh_cdxKeyGetItem( pTag->CurKey,
                                                    pInfo->itmResult, pTag );
         }
         break;

      case DBOI_SCOPETOP:
         if( pTag )
         {
            if( pInfo->itmResult )
               zh_cdxTagGetScope( pTag, 0, pInfo->itmResult );
            if( pInfo->itmNewVal )
               zh_cdxTagSetScope( pTag, 0, pInfo->itmNewVal );
         }
         else if( pInfo->itmResult )
            zh_itemClear( pInfo->itmResult );
         break;

      case DBOI_SCOPEBOTTOM:
         if( pTag )
         {
            if( pInfo->itmResult )
               zh_cdxTagGetScope( pTag, 1, pInfo->itmResult );
            if( pInfo->itmNewVal )
               zh_cdxTagSetScope( pTag, 1, pInfo->itmNewVal );
         }
         else if( pInfo->itmResult )
            zh_itemClear( pInfo->itmResult );
         break;

      case DBOI_SCOPESET:
         if( pTag )
         {
            if( pInfo->itmNewVal )
            {
               zh_cdxTagSetScope( pTag, 0, pInfo->itmNewVal );
               zh_cdxTagSetScope( pTag, 1, pInfo->itmNewVal );
            }
         }
         if( pInfo->itmResult )
            zh_itemClear( pInfo->itmResult );
         break;

      case DBOI_SCOPETOPCLEAR:
         if( pTag )
         {
            if( pInfo->itmResult )
               zh_cdxTagGetScope( pTag, 0, pInfo->itmResult );
            zh_cdxTagClearScope( pTag, 0 );
         }
         else if( pInfo->itmResult )
            zh_itemClear( pInfo->itmResult );
         break;

      case DBOI_SCOPEBOTTOMCLEAR:
         if( pTag )
         {
            if( pInfo->itmResult )
               zh_cdxTagGetScope( pTag, 1, pInfo->itmResult );
            zh_cdxTagClearScope( pTag, 1 );
         }
         else if( pInfo->itmResult )
            zh_itemClear( pInfo->itmResult );
         break;

      case DBOI_SCOPECLEAR:
         if( pTag )
         {
            zh_cdxTagClearScope( pTag, 0 );
            zh_cdxTagClearScope( pTag, 1 );
         }
         if( pInfo->itmResult )
            zh_itemClear( pInfo->itmResult );
         break;

      case DBOI_CUSTOM:
         if( pTag && ! pTag->Template &&
             ( zh_itemType( pInfo->itmNewVal ) & ZH_IT_LOGICAL ) )
         {
            ZH_BOOL fNewVal = zh_itemGetL( pInfo->itmNewVal );
            if( pTag->Custom ? ! fNewVal : fNewVal )
            {
               if( zh_cdxIndexLockWrite( pTag->pIndex ) )
               {
                  if( ! pTag->Template && ( pTag->Custom ? ! fNewVal : fNewVal ) )
                  {
                     pTag->Custom = fNewVal;
                     pTag->Partial = ZH_TRUE;
                     pTag->ChgOnly = ZH_FALSE;
                     pTag->TagChanged = ZH_TRUE;
                     /* This is a hacks to emulate both SIX3 and COMIX behavior
                      * which should be cleaned. I intentionally not used
                      * ZH_SIXCDX macro here [druzus]
                      */
                     if( pTag->Custom )
                        pTag->Template = pTag->MultiKey = ZH_TRUE;
                  }
                  zh_cdxIndexUnLockWrite( pTag->pIndex );
               }
            }
         }
         /* Warning: it's not CL53 compatible. CL53 returns previous
          * CUSTOM flag value not current one. [druzus]
          */
         pInfo->itmResult = zh_itemPutL( pInfo->itmResult, pTag && pTag->Custom );
         break;

      case DBOI_CHGONLY:
         if( pTag && ! pTag->Custom &&
             ( zh_itemType( pInfo->itmNewVal ) & ZH_IT_LOGICAL ) )
         {
            ZH_BOOL fNewVal = zh_itemGetL( pInfo->itmNewVal );
            if( pTag->ChgOnly ? ! fNewVal : fNewVal )
            {
               if( zh_cdxIndexLockWrite( pTag->pIndex ) )
               {
                  if( ! pTag->Custom && ( pTag->ChgOnly ? ! fNewVal : fNewVal ) )
                  {
                     pTag->ChgOnly = fNewVal;
                     pTag->Partial = ZH_TRUE;
                     pTag->TagChanged = ZH_TRUE;
                  }
                  zh_cdxIndexUnLockWrite( pTag->pIndex );
               }
            }
         }
         pInfo->itmResult = zh_itemPutL( pInfo->itmResult, pTag && pTag->ChgOnly );
         break;

      case DBOI_TEMPLATE:
         if( pTag && pTag->Custom && ! pTag->Template &&
             zh_itemGetL( pInfo->itmNewVal ) )
         {
            if( zh_cdxIndexLockWrite( pTag->pIndex ) )
            {
               if( pTag->Custom && ! pTag->Template )
               {
                  pTag->Template = ZH_TRUE;
                  pTag->TagChanged = ZH_TRUE;
               }
               zh_cdxIndexUnLockWrite( pTag->pIndex );
            }
         }
         pInfo->itmResult = zh_itemPutL( pInfo->itmResult, pTag && pTag->Template );
         break;

      case DBOI_MULTIKEY:
         if( pTag && pTag->Custom && ! pTag->MultiKey &&
             zh_itemGetL( pInfo->itmNewVal ) )
         {
            if( zh_cdxIndexLockWrite( pTag->pIndex ) )
            {
               if( pTag->Custom && ! pTag->MultiKey )
               {
                  pTag->MultiKey = ZH_TRUE;
                  pTag->TagChanged = ZH_TRUE;
               }
               zh_cdxIndexUnLockWrite( pTag->pIndex );
            }
         }
         pInfo->itmResult = zh_itemPutL( pInfo->itmResult, pTag && pTag->MultiKey );
         break;

      case DBOI_PARTIAL:
         pInfo->itmResult = zh_itemPutL( pInfo->itmResult, pTag && pTag->Partial );
         break;

      case DBOI_KEYADD:
      {
         ZH_BOOL fResult = ZH_FALSE;
         if( pTag )
         {
            if( pTag->Custom )
            {
               if( pArea->dbfarea.lpdbPendingRel )
                  SELF_FORCEREL( &pArea->dbfarea.area );

               if( pArea->dbfarea.fPositioned &&
                   ( ! pTag->pForItem ||
                     zh_cdxEvalCond( pArea, pTag->pForItem, ZH_TRUE ) ) )
               {
                  LPCDXKEY pKey;
#if defined( ZH_SIXCDX )
                  if( pTag->Template )
                  {
                     if( pTag->uiType == zh_cdxItemType( pInfo->itmNewVal ) )
                        pKey = zh_cdxKeyPutItem( NULL, pInfo->itmNewVal,
                                                 pArea->dbfarea.ulRecNo,
                                                 pTag, CDX_CMP_EXACT );
                     else
                        pKey = NULL;
                  }
#else
                  if( pInfo->itmNewVal && ! ZH_IS_NIL( pInfo->itmNewVal ) &&
                      pTag->Template )
                     pKey = zh_cdxKeyPutItem( NULL, pInfo->itmNewVal,
                                              pArea->dbfarea.ulRecNo,
                                              pTag, CDX_CMP_EXACT );
#endif
                  else
                     pKey = zh_cdxKeyEval( NULL, pTag );

                  if( pKey )
                  {
                     if( zh_cdxIndexLockWrite( pTag->pIndex ) )
                     {
                        fResult = zh_cdxTagKeyAdd( pTag, pKey );
                        zh_cdxIndexUnLockWrite( pTag->pIndex );
                     }
                     zh_cdxKeyFree( pKey );
                  }
               }
            }
#if ! defined( ZH_SIXCDX )
            else
               zh_cdxErrorRT( pArea, EG_ARG, EDBF_NOTCUSTOM, NULL, 0, 0, NULL );
#endif
         }
         pInfo->itmResult = zh_itemPutL( pInfo->itmResult, fResult );
         break;
      }
      case DBOI_KEYDELETE:
      {
         ZH_BOOL fResult = ZH_FALSE;
         if( pTag )
         {
            if( pTag->Custom )
            {
               if( pArea->dbfarea.lpdbPendingRel )
                  SELF_FORCEREL( &pArea->dbfarea.area );

               if( pArea->dbfarea.fPositioned &&
                   ( ! pTag->pForItem ||
                     zh_cdxEvalCond( pArea, pTag->pForItem, ZH_TRUE ) ) )
               {
                  ZH_BOOL fLck = ZH_FALSE;
                  LPCDXKEY pKey;
#if defined( ZH_SIXCDX )
                  if( pTag->Template )
                  {
                     if( pTag->uiType == zh_cdxItemType( pInfo->itmNewVal ) )
                        pKey = zh_cdxKeyPutItem( NULL, pInfo->itmNewVal,
                                                 pArea->dbfarea.ulRecNo,
                                                 pTag, CDX_CMP_EXACT );
                     else
                        pKey = NULL;
                  }
#else
                  if( pInfo->itmNewVal && ! ZH_IS_NIL( pInfo->itmNewVal ) &&
                      pTag->Template )
                     pKey = zh_cdxKeyPutItem( NULL, pInfo->itmNewVal,
                                              pArea->dbfarea.ulRecNo,
                                              pTag, CDX_CMP_EXACT );
#endif
                  else
                  {
                     if( pTag->CurKey->rec != pArea->dbfarea.ulRecNo )
                     {
                        if( zh_cdxIndexLockWrite( pTag->pIndex ) )
                        {
                           fLck = ZH_TRUE;
                           zh_cdxCurKeyRefresh( pArea, pTag );
                        }
                     }

                     if( pTag->CurKey->rec == pArea->dbfarea.ulRecNo )
                        pKey = zh_cdxKeyCopy( NULL, pTag->CurKey );
                     else
                        pKey = zh_cdxKeyEval( NULL, pTag );
                  }
                  if( pKey )
                  {
                     if( !fLck )
                        fLck = zh_cdxIndexLockWrite( pTag->pIndex );
                     if( fLck )
                        fResult = zh_cdxTagKeyDel( pTag, pKey );
                     zh_cdxKeyFree( pKey );
                  }
                  if( fLck )
                     zh_cdxIndexUnLockWrite( pTag->pIndex );
               }
            }
#if ! defined( ZH_SIXCDX )
            else
               zh_cdxErrorRT( pArea, EG_ARG, EDBF_NOTCUSTOM, NULL, 0, 0, NULL );
#endif
         }
         pInfo->itmResult = zh_itemPutL( pInfo->itmResult, fResult );
         break;
      }
      case DBOI_READLOCK:
         if( pTag )
         {
            if( zh_itemType( pInfo->itmNewVal ) & ZH_IT_LOGICAL )
            {
               if( zh_itemGetL( pInfo->itmNewVal ) )
                  zh_cdxIndexLockRead( pTag->pIndex );
               else
                  zh_cdxIndexUnLockRead( pTag->pIndex );
            }
            pInfo->itmResult = zh_itemPutL( pInfo->itmResult,
                                            pTag->pIndex->lockRead > 0 );
         }
         else
            pInfo->itmResult = zh_itemPutL( pInfo->itmResult, ZH_FALSE );
         break;

      case DBOI_WRITELOCK:
         if( pTag )
         {
            if( zh_itemType( pInfo->itmNewVal ) & ZH_IT_LOGICAL )
            {
               if( zh_itemGetL( pInfo->itmNewVal ) )
                  zh_cdxIndexLockWrite( pTag->pIndex );
               else
                  zh_cdxIndexUnLockWrite( pTag->pIndex );
            }
            pInfo->itmResult = zh_itemPutL( pInfo->itmResult,
                                            pTag->pIndex->lockWrite > 0 );
         }
         else
            pInfo->itmResult = zh_itemPutL( pInfo->itmResult, ZH_FALSE );
         break;

      case DBOI_UPDATECOUNTER:
         if( pTag )
         {
            /* refresh update counter */
            if( zh_cdxIndexLockRead( pTag->pIndex ) )
               zh_cdxIndexUnLockRead( pTag->pIndex );
            pInfo->itmResult = zh_itemPutNInt( pInfo->itmResult,
                                               pTag->pIndex->ulVersion );
         }
         else
            pInfo->itmResult = zh_itemPutNI( pInfo->itmResult, 0 );
         break;

      case DBOI_SHARED:
         pInfo->itmResult = zh_itemPutL( pInfo->itmResult,
                                         pTag && pTag->pIndex->fShared );
         break;

      case DBOI_ISREADONLY:
         pInfo->itmResult = zh_itemPutL( pInfo->itmResult,
                                         pTag && pTag->pIndex->fReadonly );
         break;

      case DBOI_ISMULTITAG:
      case DBOI_ISSORTRECNO:
         pInfo->itmResult = zh_itemPutL( pInfo->itmResult, pTag != NULL );
         break;

      case DBOI_LARGEFILE:
         pInfo->itmResult = zh_itemPutL( pInfo->itmResult,
                                         pTag && pTag->pIndex->fLargeFile );
         break;

      case DBOI_INDEXTYPE:
         pInfo->itmResult = zh_itemPutNI( pInfo->itmResult, pTag ?
                                          DBOI_TYPE_COMPOUND : DBOI_TYPE_UNDEF );
         break;

      case DBOI_INDEXPAGESIZE:
         pInfo->itmResult = zh_itemPutNI( pInfo->itmResult, pTag ?
                                          pTag->pIndex->uiPageLen : 0 );
         break;

      default:
         return SUPER_ORDINFO( &pArea->dbfarea.area, uiIndex, pInfo );

   }
   return ZH_SUCCESS;
}

/* ( DBENTRYP_V )     zh_cdxClearFilter */
static ZH_ERRCODE zh_cdxClearFilter( CDXAREAP pArea )
{
   ZH_ERRCODE errCode = SUPER_CLEARFILTER( &pArea->dbfarea.area );

   zh_cdxClearLogPosInfo( pArea );
   return errCode;
}

/* ( DBENTRYP_V )     zh_cdxClearLocate     : NULL */
/* ( DBENTRYP_V )     zh_cdxClearScope      : NULL */

/* ( DBENTRYP_VPLP )  zh_cdxCountScope */
static ZH_ERRCODE zh_cdxCountScope( CDXAREAP pArea, void * pPtr, ZH_LONG * plRec )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_cdxCountScope(%p, %p, %p)", ( void * ) pArea, ( void * ) pPtr, ( void * ) plRec ) );

   if( pPtr == NULL )
   {
      return ZH_SUCCESS;
   }
   return SUPER_COUNTSCOPE( &pArea->dbfarea.area, pPtr, plRec );
}

/* ( DBENTRYP_I )     zh_cdxFilterText      : NULL */
/* ( DBENTRYP_SI )    zh_cdxScopeInfo       : NULL */

/* ( DBENTRYP_VFI )   zh_cdxSetFilter */
static ZH_ERRCODE zh_cdxSetFilter( CDXAREAP pArea, LPDBFILTERINFO pFilterInfo )
{
   ZH_ERRCODE errCode = SUPER_SETFILTER( &pArea->dbfarea.area, pFilterInfo );

   zh_cdxClearLogPosInfo( pArea );
   return errCode;
}

/* ( DBENTRYP_VLO )   zh_cdxSetLocate       : NULL */
/* ( DBENTRYP_VOS )   zh_cdxSetScope        : NULL */
/* ( DBENTRYP_VPL )   zh_cdxSkipScope       : NULL */
/* ( DBENTRYP_B )     zh_cdxLocate          : NULL */

/* ( DBENTRYP_CC )    zh_cdxCompile         : NULL */
/* ( DBENTRYP_I )     zh_cdxError           : NULL */
/* ( DBENTRYP_I )     zh_cdxEvalBlock       : NULL */

/* ( DBENTRYP_VSP )   zh_cdxRawLock         : NULL */
/* ( DBENTRYP_VL )    zh_cdxLock            : NULL */
/* ( DBENTRYP_UL )    zh_cdxUnLock          : NULL */

/* ( DBENTRYP_V )     zh_cdxCloseMemFile    : NULL */
/* ( DBENTRYP_VO )    zh_cdxCreateMemFile   : NULL */
/* ( DBENTRYP_SCCS )  zh_cdxGetValueFile    : NULL */
/* ( DBENTRYP_VO )    zh_cdxOpenMemFile     : NULL */
/* ( DBENTRYP_SCCS )  zh_cdxPutValueFile    : NULL */

/* ( DBENTRYP_V )     zh_cdxReadDBHeader    : NULL */
/* ( DBENTRYP_V )     zh_cdxWriteDBHeader   : NULL */
/* ( DBENTRYP_SVP )   zh_cdxWhoCares        : NULL */

/*
 * Retrieve (set) information about RDD
 * ( DBENTRYP_RSLV )   zh_fptFieldInfo
 */
static ZH_ERRCODE zh_cdxRddInfo( LPRDDNODE pRDD, ZH_USHORT uiIndex, ZH_ULONG ulConnect, PZH_ITEM pItem )
{
   LPDBFDATA pData;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_cdxRddInfo(%p, %hu, %lu, %p)", ( void * ) pRDD, uiIndex, ulConnect, ( void * ) pItem ) );

   pData = DBFNODE_DATA( pRDD );

   switch( uiIndex )
   {
      case RDDI_ORDBAGEXT:
      case RDDI_ORDEREXT:
      case RDDI_ORDSTRUCTEXT:
      {
         const char * szExt = zh_itemGetCPtr( pItem );
         char * szNewVal;

         szNewVal = szExt[ 0 ] == '.' && szExt[ 1 ] ? zh_strdup( szExt ) : NULL;
         zh_itemPutC( pItem, pData->szIndexExt[ 0 ] ? pData->szIndexExt : CDX_INDEXEXT );
         if( szNewVal )
         {
            zh_strncpy( pData->szIndexExt, szNewVal, sizeof( pData->szIndexExt ) - 1 );
            zh_xfree( szNewVal );
         }
         break;
      }

      case RDDI_MULTIKEY:
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

      default:
         return SUPER_RDDINFO( pRDD, uiIndex, ulConnect, pItem );

   }

   return ZH_SUCCESS;
}



/* ######################################################################### */

static int zh_cdxQuickSortCompare( LPCDXSORTINFO pSort, ZH_BYTE * pKey1, ZH_BYTE * pKey2 )
{
   int i, iLen = pSort->keyLen;

   i = zh_cdxValCompare( pSort->pTag, pKey1, iLen, pKey2, iLen, CDX_CMP_EXACT );

   if( i == 0 )
   {
      i = ( ZH_GET_LE_UINT32( pKey1 + iLen ) < ZH_GET_LE_UINT32( pKey2 + iLen ) ) ? -1 : 1;
   }

   return i;
}

static ZH_BOOL zh_cdxQSort( LPCDXSORTINFO pSort, ZH_BYTE * pSrc, ZH_BYTE * pBuf, ZH_LONG lKeys )
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

      f1 = zh_cdxQSort( pSort, pPtr1, &pBuf[ 0 ], l1 );
      f2 = zh_cdxQSort( pSort, pPtr2, &pBuf[ l1 * iLen ], l2 );
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
         if( zh_cdxQuickSortCompare( pSort, pPtr1, pPtr2 ) <= 0 )
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

static void zh_cdxSortSortPage( LPCDXSORTINFO pSort )
{
   ZH_SIZE nSize = ( ZH_SIZE ) pSort->ulKeys * ( pSort->keyLen + 4 );

#ifdef ZH_CDX_DBGTIME
   cdxTimeIdxBld -= zh_cdxGetTime();
#endif
   if( ! zh_cdxQSort( pSort, pSort->pKeyPool, &pSort->pKeyPool[ nSize ], pSort->ulKeys ) )
      pSort->pStartKey = &pSort->pKeyPool[ nSize ];
   else
      pSort->pStartKey = pSort->pKeyPool;
#ifdef ZH_CDX_DBGTIME
   cdxTimeIdxBld += zh_cdxGetTime();
#endif
}

static void zh_cdxSortAddNodeKey( LPCDXSORTINFO pSort, int iLevel, ZH_BYTE * pKeyVal, ZH_ULONG ulRec, ZH_ULONG ulPage )
{
   LPCDXPAGE pPage;
   ZH_BOOL fNew;
   int iLen = pSort->keyLen, iDup = 0, iTrl = 0, iTmp, iPos;
   ZH_BYTE * pTmp;

   pPage = pSort->NodeList[ iLevel ];
   if( iLevel == 0 )
   {
      while( iTrl < iLen && pKeyVal[ iLen - iTrl - 1 ] == pSort->bTrl )
         iTrl++;
      if( pPage != NULL && pPage->iKeys > 0 )
      {
#ifdef ZH_CDX_PACKTRAIL
         int iMax = iLen - iTrl;
#else
         int iMax = iLen - ZH_MAX( iTrl, pSort->iLastTrl );
#endif
         while( pKeyVal[ iDup ] == pSort->pLastKey[ iDup ] && iDup < iMax )
            iDup++;
      }
#ifndef ZH_CDX_PACKTRAIL
      pSort->iLastTrl = iTrl;
#endif
   }
   if( pPage == NULL )
      fNew = ZH_TRUE;
   else
   {
      if( iLevel == 0 )
         fNew = ( pPage->iFree - ( iLen - iDup - iTrl ) - pPage->ReqByte ) < 0;
      else
         fNew = ( pSort->NodeList[ iLevel ]->iKeys >= pSort->pTag->MaxKeys );
   }

   if( fNew )
   {
      pPage = zh_cdxPageNew( pSort->pTag, NULL, 0 );
      pPage->PageType = ( iLevel == 0 ) ? CDX_NODE_LEAF : CDX_NODE_BRANCH;
      if( iLevel == 0 )
      {
         zh_cdxPageLeafInitSpace( pPage );
         iDup = 0;
         while( pSort->ulMaxRec > pPage->RNMask )
         {
            pPage->ReqByte++;
            pPage->RNBits += 8;
            pPage->RNMask = ( pPage->RNMask << 8 ) | 0xFF;
         }
      }
      if( pSort->NodeList[ iLevel ] != NULL )
      {
         pSort->NodeList[ iLevel ]->Right = pPage->Page;
         pPage->Left = pSort->NodeList[ iLevel ]->Page;
         if( iLevel == 0 )
         {
#ifdef ZH_CDX_DBGCODE_EXT
            zh_cdxPageCheckDupTrlRaw( pSort->NodeList[ iLevel ] );
#endif
            zh_cdxSortAddNodeKey( pSort, iLevel + 1, pSort->pLastKey, pSort->ulLastRec, pSort->NodeList[ iLevel ]->Page );
         }
         else
         {
            iPos = ( pSort->NodeList[ iLevel ]->iKeys - 1 ) * ( iLen + 8 );
            pTmp = &zh_cdxPageIntKeyPool( pSort->NodeList[ iLevel ] )[ iPos ];
            zh_cdxSortAddNodeKey( pSort, iLevel + 1, pTmp, ZH_GET_BE_UINT32( &pTmp[ iLen ] ), pSort->NodeList[ iLevel ]->Page );
         }
         zh_cdxPageFree( pSort->NodeList[ iLevel ], ZH_TRUE );
      }
      pSort->NodeList[ iLevel ] = pPage;
   }
   if( iLevel == 0 )
   {
      iPos = pPage->iKeys * pPage->ReqByte;
      zh_cdxSetLeafRecord( &zh_cdxPageExtKeyPool( pPage )[ iPos ],
                           ulRec, iDup, iTrl,
                           pPage->ReqByte, pPage->DCBits, pPage->TCBits );
#ifdef ZH_CDX_DBGCODE_EXT
      zh_cdxChkLeafRecord( &zh_cdxPageExtKeyPool( pPage )[ iPos ],
                           ulRec, iDup, iTrl, pPage );
#endif
      iTmp = iLen - iDup - iTrl;
      if( iTmp > 0 )
         memcpy( &zh_cdxPageExtKeyPool( pPage )[ pPage->iFree + iPos - iTmp ],
                 &pKeyVal[ iDup ], iTmp );
      pPage->iFree -= ( ZH_SHORT ) ( iTmp + pPage->ReqByte );
      pPage->iKeys++;
#ifdef ZH_CDX_DBGCODE_EXT
      zh_cdxPageCheckDupTrlRaw( pSort->NodeList[ iLevel ] );
#endif
   }
   else
   {
      pPage = pSort->NodeList[ iLevel ];
      iPos = pPage->iKeys * ( iLen + 8 );
      pTmp = &zh_cdxPageIntKeyPool( pPage )[ iPos ];
      memcpy( pTmp, pKeyVal, iLen );
      ZH_PUT_BE_UINT32( &pTmp[ iLen ], ulRec );
      ZH_PUT_BE_UINT32( &pTmp[ iLen + 4 ], ulPage );
      pPage->iKeys++;
   }
}

static void zh_cdxSortWritePage( LPCDXSORTINFO pSort )
{
   ZH_SIZE nSize = ( ZH_SIZE ) pSort->ulKeys * ( pSort->keyLen + 4 );

   zh_cdxSortSortPage( pSort );

   if( pSort->pTempFile == NULL )
   {
      char szName[ ZH_PATH_MAX ];
      pSort->pTempFile = zh_fileCreateTemp( NULL, NULL, FC_NORMAL, szName );
      if( pSort->pTempFile == NULL )
         zh_errInternal( 9301, "zh_cdxSortWritePage: Could not create temporary file.", NULL, NULL );
      pSort->szTempFileName = zh_strdup( szName );
   }
   pSort->pSwapPage[ pSort->ulCurPage ].ulKeys = pSort->ulKeys;
   pSort->pSwapPage[ pSort->ulCurPage ].nOffset = zh_fileSize( pSort->pTempFile );
   if( zh_fileWriteAt( pSort->pTempFile, pSort->pStartKey,
                       nSize, pSort->pSwapPage[ pSort->ulCurPage ].nOffset ) != nSize )
      zh_errInternal( 9302, "zh_cdxSortWritePage: Write error in temporary file.", NULL, NULL );
   pSort->ulKeys = 0;
   pSort->ulCurPage++;
}

static void zh_cdxSortGetPageKey( LPCDXSORTINFO pSort, ZH_ULONG ulPage,
                                  ZH_BYTE ** pKeyVal, ZH_ULONG * pulRec )
{
   int iLen = pSort->keyLen;

   if( pSort->pSwapPage[ ulPage ].ulKeyBuf == 0 )
   {
      ZH_ULONG ulKeys = ZH_MIN( pSort->ulPgKeys, pSort->pSwapPage[ ulPage ].ulKeys );
      ZH_SIZE nSize = ( ZH_SIZE ) ulKeys * ( iLen + 4 );

      if( zh_fileReadAt( pSort->pTempFile, pSort->pSwapPage[ ulPage ].pKeyPool,
                         nSize, pSort->pSwapPage[ ulPage ].nOffset ) != nSize )
         zh_errInternal( 9303, "zh_cdxSortGetPageKey: Read error from temporary file.", NULL, NULL );
      pSort->pSwapPage[ ulPage ].nOffset += nSize;
      pSort->pSwapPage[ ulPage ].ulKeyBuf = ulKeys;
      pSort->pSwapPage[ ulPage ].ulCurKey = 0;
   }
   *pKeyVal = &pSort->pSwapPage[ ulPage ].pKeyPool[ pSort->pSwapPage[ ulPage ].ulCurKey * ( iLen + 4 ) ];
   *pulRec = ZH_GET_LE_UINT32( *pKeyVal + iLen );
}

#ifdef ZH_CDX_NEW_SORT
static void zh_cdxSortOrderPages( LPCDXSORTINFO pSort )
{
   pSort->ulFirst = 0;
   pSort->pSortedPages = ( ZH_ULONG * ) zh_xgrab( pSort->ulPages * sizeof( ZH_ULONG ) );
   pSort->pSortedPages[ 0 ] = 0;

   if( pSort->ulTotKeys > 0 )
   {
      int iLen = pSort->keyLen;
      ZH_ULONG n;
      ZH_BYTE * pKey = NULL;

      for( n = 0; n < pSort->ulPages; n++ )
      {
         ZH_ULONG ulRec;
         ZH_LONG l, r;

         zh_cdxSortGetPageKey( pSort, n, &pKey, &ulRec );
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
            i = zh_cdxValCompare( pSort->pTag, pKey, iLen, pTmp, iLen, CDX_CMP_EXACT );
            if( i == 0 )
               i = ( ulRec < ZH_GET_LE_UINT32( &pTmp[ iLen ] ) ) ? -1 : 1;
            if( i > 0 )
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

static ZH_BOOL zh_cdxSortKeyGet( LPCDXSORTINFO pSort, ZH_BYTE ** pKeyVal, ZH_ULONG * pulRec )
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
       * zh_cdxSortGetPageKey() - please do not move this part down
       * even it seems to be correct
       */
      zh_cdxSortGetPageKey( pSort, ulPage, &pKey, &ulRec );

      l = pSort->ulFirst + 1;
      r = pSort->ulPages - 1;
      while( l <= r )
      {
         int i;
         ZH_LONG m;
         ZH_BYTE * pTmp;

         m = ( l + r ) >> 1;
         ulPage = pSort->pSortedPages[ m ];
         pTmp = &pSort->pSwapPage[ ulPage ].pKeyPool[ pSort->pSwapPage[ ulPage ].ulCurKey * ( iLen + 4 ) ];
         i = zh_cdxValCompare( pSort->pTag, pKey, iLen, pTmp, iLen, CDX_CMP_EXACT );
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
      pSort->ulFirst++;

   if( pSort->ulFirst < pSort->ulPages )
   {
      ulPage = pSort->pSortedPages[ pSort->ulFirst ];
      zh_cdxSortGetPageKey( pSort, ulPage, pKeyVal, pulRec );
      pSort->pSwapPage[ ulPage ].ulCurKey++;
      pSort->pSwapPage[ ulPage ].ulKeys--;
      pSort->pSwapPage[ ulPage ].ulKeyBuf--;
      return ZH_TRUE;
   }
   *pulRec = 0;
   *pKeyVal = NULL;
   return ZH_FALSE;
}

#else

static ZH_BOOL zh_cdxSortKeyGet( LPCDXSORTINFO pSort, ZH_BYTE ** pKeyVal, ZH_ULONG * pulRec )
{
   int i, iLen = pSort->keyLen;
   ZH_ULONG ulPage, ulKeyPage = 0, ulRec = 0, ulRecTmp;
   ZH_BYTE * pKey = NULL, * pTmp;

   for( ulPage = 0; ulPage < pSort->ulPages; ulPage++ )
   {
      if( pSort->pSwapPage[ ulPage ].ulKeys > 0 )
      {
         zh_cdxSortGetPageKey( pSort, ulPage, &pTmp, &ulRecTmp );
         if( ! pKey )
            i = 1;
         else
         {
            i = zh_cdxValCompare( pSort->pTag, pKey, iLen, pTmp, iLen, CDX_CMP_EXACT );
            if( i == 0 )
               i = ( ulRec < ulRecTmp ) ? -1 : 1;
         }
         if( i > 0 )
         {
            pKey = pTmp;
            ulRec = ulRecTmp;
            ulKeyPage = ulPage;
         }
      }
   }
   if( pKey )
   {
      pSort->pSwapPage[ ulKeyPage ].ulCurKey++;
      pSort->pSwapPage[ ulKeyPage ].ulKeys--;
      pSort->pSwapPage[ ulKeyPage ].ulKeyBuf--;
      *pulRec = ulRec;
      *pKeyVal = pKey;
      return ZH_TRUE;
   }
   *pulRec = 0;
   *pKeyVal = NULL;
   return ZH_FALSE;
}

#endif

static void zh_cdxSortKeyAdd( LPCDXSORTINFO pSort, ZH_ULONG ulRec, const ZH_BYTE * pKeyVal, int iKeyLen )
{
   int iLen = pSort->keyLen;
   ZH_BYTE * pDst;

   if( pSort->ulKeys >= pSort->ulPgKeys )
      zh_cdxSortWritePage( pSort );
   pDst = &pSort->pKeyPool[ pSort->ulKeys * ( iLen + 4 ) ];

   if( pSort->pTag->IgnoreCase )
   {
      iKeyLen = ( int ) zh_cdpnDup2Upper( pSort->pTag->pIndex->pArea->dbfarea.area.cdPage,
                                          ( const char * ) pKeyVal, iKeyLen,
                                          ( char * ) pDst, iLen );
      if( iLen > iKeyLen )
         memset( &pDst[ iKeyLen ], pSort->bTrl, iLen - iKeyLen );
   }
   else if( iLen > iKeyLen )
   {
      memcpy( pDst, pKeyVal, iKeyLen );
      memset( &pDst[ iKeyLen ], pSort->bTrl, iLen - iKeyLen );
   }
   else
      memcpy( pDst, pKeyVal, iLen );

   ZH_PUT_LE_UINT32( &pDst[ iLen ], ulRec );
   pSort->ulKeys++;
   pSort->ulTotKeys++;
}

static LPCDXSORTINFO zh_cdxSortNew( LPCDXTAG pTag, ZH_ULONG ulRecCount )
{
   LPCDXSORTINFO pSort;
   ZH_BYTE * pBuf;
   int iLen = pTag->uiLen;
   ZH_ULONG ulSize, ulMax, ulMin;

   if( ulRecCount == 0 )
      ulRecCount = 1;

   pSort = ( LPCDXSORTINFO ) zh_xgrab( sizeof( CDXSORTINFO ) );
   memset( pSort, 0, sizeof( CDXSORTINFO ) );
   ulMax = ulMin = ( ZH_ULONG ) ceil( sqrt( ( double ) ulRecCount ) );
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
         ulMax >>= 1;
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
       *    ~ (keySize+4+sizeof(CDXSWAPPAGE)) * sqrt(ulRecCount) * 2
       * so the maximum is for DBF with 2^32 records and keySize 240 ~
       * ~ 2^17 * 268 ~=~ 35 MB
       * this is not a problem for current computers and I do not see
       * any way to use DBFs with four billions records and indexes with
       * such long (240 bytes) keys on the old ones - they will be simply
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
   pSort->bTrl = pTag->bTrail;
   pSort->fUnique = pTag->UniqueKey;
   pSort->ulMaxKey = ulMax << 1;
   pSort->ulPgKeys = ulMax;
   pSort->ulMaxRec = ulRecCount;
   pSort->pKeyPool = pBuf;
   pSort->ulPages = ( ulRecCount + pSort->ulPgKeys - 1 ) / pSort->ulPgKeys;
   pSort->pSwapPage = ( LPCDXSWAPPAGE ) zh_xgrab( sizeof( CDXSWAPPAGE ) * pSort->ulPages );
   memset( pSort->pSwapPage, 0, sizeof( CDXSWAPPAGE ) * pSort->ulPages );
   pSort->pLastKey = ( ZH_BYTE * ) zh_xgrabz( iLen + 1 );

   return pSort;
}

static void zh_cdxSortFree( LPCDXSORTINFO pSort )
{
   if( pSort->pTempFile != NULL )
      zh_fileClose( pSort->pTempFile );
   if( pSort->szTempFileName )
   {
      zh_fileDelete( pSort->szTempFileName );
      zh_xfree( pSort->szTempFileName );
   }
   if( pSort->pLastKey )
      zh_xfree( pSort->pLastKey );
   if( pSort->pKeyPool )
      zh_xfree( pSort->pKeyPool );
   if( pSort->pSwapPage )
      zh_xfree( pSort->pSwapPage );
   if( pSort->pRecBuff )
      zh_xfree( pSort->pRecBuff );
   if( pSort->pSortedPages )
      zh_xfree( pSort->pSortedPages );
   zh_xfree( pSort );
}

static void zh_cdxSortOut( LPCDXSORTINFO pSort )
{
   ZH_BOOL fUnique = pSort->fUnique, fNext;
   ZH_ULONG ulRec, ulKey;
   ZH_BYTE * pKeyVal;
   int iLen = pSort->keyLen, iLevel;

   pSort->ulPages = pSort->ulCurPage + 1;
   pSort->ulPgKeys = pSort->ulMaxKey / pSort->ulPages;
   /*
   printf( "\r\npSort->ulMaxKey=%ld, pSort->ulPages=%ld, pSort->ulPgKeys=%ld, size=%ld\r\n",
           pSort->ulMaxKey, pSort->ulPages, pSort->ulPgKeys,
           pSort->ulMaxKey * ( pSort->keyLen + 4 ) ); fflush(stdout);
   */
   if( pSort->ulPages > 1 )
   {
      ZH_BYTE * pBuf = pSort->pKeyPool;
      ZH_ULONG ulPage;
      zh_cdxSortWritePage( pSort );
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
      zh_cdxSortSortPage( pSort );
      pSort->pSwapPage[ 0 ].ulKeys   = pSort->ulKeys;
      pSort->pSwapPage[ 0 ].ulKeyBuf = pSort->ulKeys;
      pSort->pSwapPage[ 0 ].ulCurKey = 0;
      pSort->pSwapPage[ 0 ].pKeyPool = pSort->pStartKey;
   }

#ifdef ZH_CDX_NEW_SORT
   zh_cdxSortOrderPages( pSort );
#endif

   for( ulKey = 0; ulKey < pSort->ulTotKeys; ulKey++ )
   {
      if( ! zh_cdxSortKeyGet( pSort, &pKeyVal, &ulRec ) )
         zh_errInternal( 9304, "zh_cdxSortOut: memory structure corrupted.", NULL, NULL );

      if( fUnique )
      {
         if( ulKey != 0 && zh_cdxValCompare( pSort->pTag, pSort->pLastKey, iLen, pKeyVal, iLen, CDX_CMP_EXACT ) == 0 )
            continue;
      }
#ifdef ZH_CDX_DBGCODE_EXT
      if( ulKey != 0 )
      {
         int i = zh_cdxValCompare( pSort->pTag, pSort->pLastKey, iLen, pKeyVal, iLen, CDX_CMP_EXACT );
         if( i == 0 )
            i = ( pSort->ulLastRec < ulRec ) ? -1 : 1;
         if( i > 0 )
         {
            printf( "\r\nulKey=%ld, pKeyVal=[%s][%ld], pKeyLast=[%s][%ld]\r\n",
                    ulKey, pKeyVal, ulRec, pSort->pLastKey, pSort->ulLastRec ); fflush( stdout );
            zh_errInternal( 9305, "zh_cdxSortOut: sorting fails.", NULL, NULL );
         }
      }
#endif
      zh_cdxSortAddNodeKey( pSort, 0, pKeyVal, ulRec, 0 );
      memcpy( pSort->pLastKey, pKeyVal, iLen );
      pSort->ulLastRec = ulRec;
   }

#ifdef ZH_CDX_DBGCODE
   if( zh_cdxSortKeyGet( pSort, &pKeyVal, &ulRec ) )
      zh_errInternal( 9306, "zh_cdxSortOut: memory structure corrupted(2).", NULL, NULL );
#endif

   if( pSort->NodeList[ 0 ] == NULL )
   {
      pSort->NodeList[ 0 ] = zh_cdxPageNew( pSort->pTag, NULL, 0 );
      pSort->NodeList[ 0 ]->PageType = CDX_NODE_LEAF;
      zh_cdxPageLeafInitSpace( pSort->NodeList[ 0 ] );
   }

   iLevel = 0;
   fNext = ZH_TRUE;
   do
   {
      if( iLevel + 1 == CDX_STACKSIZE || pSort->NodeList[ iLevel + 1 ] == NULL )
      {
         pSort->NodeList[ iLevel ]->PageType |= CDX_NODE_ROOT;
         pSort->pTag->RootBlock = pSort->NodeList[ iLevel ]->Page;
         fNext = ZH_FALSE;
      }
      else
         zh_cdxSortAddNodeKey( pSort, iLevel + 1, pSort->pLastKey, pSort->ulLastRec, pSort->NodeList[ iLevel ]->Page );
      zh_cdxPageFree( pSort->NodeList[ iLevel ], ZH_TRUE );
      iLevel++;
   }
   while( fNext );
}

static void zh_cdxTagEmptyIndex( LPCDXTAG pTag )
{
   pTag->RootPage  = zh_cdxPageNew( pTag, NULL, 0 );
   pTag->RootBlock = pTag->RootPage->Page;
   pTag->RootPage->PageType = CDX_NODE_ROOT | CDX_NODE_LEAF;
   zh_cdxPageLeafInitSpace( pTag->RootPage );
}

static void zh_cdxTagDoIndex( LPCDXTAG pTag, ZH_BOOL fReindex )
{
   LPCDXAREA pArea = pTag->pIndex->pArea;
   LPCDXSORTINFO pSort;
   PZH_ITEM pWhileItem = NULL, pEvalItem = NULL;
   ZH_ULONG ulRecCount, ulRecNo = pArea->dbfarea.ulRecNo;
   ZH_LONG lStep = 0;
   PZH_CODEPAGE cdpTmp = zh_cdpSelect( pArea->dbfarea.area.cdPage );

   if( pArea->dbfarea.area.lpdbOrdCondInfo )
   {
      pEvalItem = pArea->dbfarea.area.lpdbOrdCondInfo->itmCobEval;
      pWhileItem = pArea->dbfarea.area.lpdbOrdCondInfo->itmCobWhile;
      lStep = pArea->dbfarea.area.lpdbOrdCondInfo->lStep;
      if( pArea->dbfarea.area.lpdbOrdCondInfo->lNextCount ||
          pArea->dbfarea.area.lpdbOrdCondInfo->itmRecID ||
          pArea->dbfarea.area.lpdbOrdCondInfo->fRest ||
          pArea->dbfarea.area.lpdbOrdCondInfo->fUseCurrent ||
          pArea->dbfarea.area.lpdbOrdCondInfo->fUseFilter )
         pTag->Partial = ZH_TRUE;
   }

   if( pTag->Custom || ( pTag->OptFlags & CDX_TYPE_STRUCTURE ) )
      ulRecCount = 0;
   else if( SELF_RECCOUNT( &pArea->dbfarea.area, &ulRecCount ) != ZH_SUCCESS )
      return;

   pArea->pSort = pSort = zh_cdxSortNew( pTag, ulRecCount );
   pSort->fReindex = fReindex;

#if defined( ZH_SIXCDX )
   if( ( pTag->OptFlags & CDX_TYPE_STRUCTURE ) == 0 && pEvalItem )
   {
      SELF_GOTO( &pArea->dbfarea.area, 0 );
      if( ! zh_cdxEvalCond( pArea, pEvalItem, ZH_FALSE ) )
      {
         zh_cdxSortFree( pSort );
         pArea->pSort = NULL;
         SELF_GOTO( &pArea->dbfarea.area, ulRecNo );
         return;
      }
   }
#endif
   if( ulRecCount == 0 )
      zh_cdxTagEmptyIndex( pTag );
   else
   {
      ZH_USHORT uiSaveTag = pArea->uiTag;
      ZH_ULONG ulStartRec = 0, ulNextCount = 0;
      ZH_BOOL fDirectRead, fUseFilter = ZH_FALSE;
      ZH_BYTE * pSaveRecBuff = pArea->dbfarea.pRecord, cTemp[ 8 ];
      int iRecBuff = 0, iRecBufSize, iRec;
      PZH_ITEM pForItem, pItem = NULL;

      pForItem = pTag->pForItem;
      if( pTag->nField )
         pItem = zh_itemNew( NULL );

      if( ! pArea->dbfarea.area.lpdbOrdCondInfo || pArea->dbfarea.area.lpdbOrdCondInfo->fAll )
         pArea->uiTag = 0;
      else
      {
         if( pArea->dbfarea.area.lpdbOrdCondInfo->itmRecID )
            ulStartRec = zh_itemGetNL( pArea->dbfarea.area.lpdbOrdCondInfo->itmRecID );
         if( ulStartRec )
            ulNextCount = 1;
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
            fUseFilter = ZH_TRUE;
         else if( ! pArea->dbfarea.area.lpdbOrdCondInfo->fUseCurrent )
            pArea->uiTag = 0;
         else if( pArea->uiTag != 0 )
         {
            LPCDXTAG pCurrTag = zh_cdxGetActiveTag( pArea );
            if( pCurrTag )
            {
               zh_cdxIndexLockRead( pCurrTag->pIndex );
               zh_cdxTagRefreshScope( pCurrTag );
               zh_cdxTagGoTop( pCurrTag );
               ulStartRec = pCurrTag->CurKey->rec;
               zh_cdxIndexUnLockRead( pCurrTag->pIndex );
            }
         }
      }

      iRecBufSize = ( USHRT_MAX + 1 ) / pArea->dbfarea.uiRecordLen;
      fDirectRead = ! zh_setGetStrictRead() && iRecBufSize > 1 && /* ! pArea->dbfarea.area.lpdbRelations && */
                    ( ! pArea->dbfarea.area.lpdbOrdCondInfo || pArea->dbfarea.area.lpdbOrdCondInfo->fAll ||
                      ( pArea->uiTag == 0 && ! fUseFilter ) );

      if( fDirectRead )
         pSort->pRecBuff = ( ZH_BYTE * ) zh_xgrab( pArea->dbfarea.uiRecordLen * iRecBufSize );

      if( ulStartRec == 0 && pArea->uiTag == 0 )
         ulStartRec = 1;

      if( ulStartRec == 0 )
         SELF_GOTOP( &pArea->dbfarea.area );
      else
      {
         SELF_GOTO( &pArea->dbfarea.area, ulStartRec );
         if( fUseFilter )
            SELF_SKIPFILTER( &pArea->dbfarea.area, 1 );
      }

      ulRecNo = pArea->dbfarea.ulRecNo;

      while( ! pArea->dbfarea.area.fEof )
      {
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
               if( zh_fileReadAt( pArea->dbfarea.pDataFile, pSort->pRecBuff, nSize,
                                  ( ZH_FOFFSET ) pArea->dbfarea.uiHeaderLen +
                                  ( ZH_FOFFSET ) ( ulRecNo - 1 ) *
                                  ( ZH_FOFFSET ) pArea->dbfarea.uiRecordLen ) != nSize )
               {
                  zh_cdxErrorRT( pTag->pIndex->pArea, EG_READ, EDBF_READ,
                                 pTag->pIndex->szFileName, zh_fsError(), 0, NULL );
                  break;
               }
               iRecBuff = 0;
            }
            pArea->dbfarea.pRecord = pSort->pRecBuff + iRecBuff * pArea->dbfarea.uiRecordLen;
            pArea->dbfarea.ulRecNo = ulRecNo;
            if( SELF_GETREC( &pArea->dbfarea.area, NULL ) == ZH_FAILURE )
               break;
            pArea->dbfarea.fValidBuffer = pArea->dbfarea.fPositioned = ZH_TRUE;
            pArea->dbfarea.fDeleted = pArea->dbfarea.pRecord[ 0 ] == '*';
            /* Force relational movement in child WorkAreas */
            if( pArea->dbfarea.area.lpdbRelations )
               if( SELF_SYNCCHILDREN( &pArea->dbfarea.area ) == ZH_FAILURE )
                  break;
            iRecBuff++;
         }

#if ! defined( ZH_SIXCDX )
         if( pEvalItem )
         {
            if( lStep >= pArea->dbfarea.area.lpdbOrdCondInfo->lStep )
            {
               lStep = 0;
               if( ! zh_cdxEvalCond( pArea, pEvalItem, ZH_FALSE ) )
                  break;
            }
            ++lStep;
         }
#endif

         if( pWhileItem && ! zh_cdxEvalCond( NULL, pWhileItem, ZH_FALSE ) )
            break;

         if( ulRecNo <= ulRecCount &&
             ( pForItem == NULL || zh_cdxEvalCond( pArea, pForItem, ZH_FALSE ) ) )
         {
            double d;

            if( pTag->nField )
               SELF_GETVALUE( &pArea->dbfarea.area, pTag->nField, pItem );
            else
               pItem = zh_vmEvalBlockOrMacro( pTag->pKeyItem );

            switch( zh_itemType( pItem ) )
            {
               case ZH_IT_STRING:
               case ZH_IT_MEMO:
                  zh_cdxSortKeyAdd( pSort, pArea->dbfarea.ulRecNo,
                                    ( const ZH_BYTE * ) zh_itemGetCPtr( pItem ),
                                    ( int ) zh_itemGetCLen( pItem ) );
                  break;

               case ZH_IT_INTEGER:
               case ZH_IT_LONG:
               case ZH_IT_DOUBLE:
                  if( pTag->uiLen == 4 )
                  {
                     ZH_U32 uiVal = ( ZH_U32 ) zh_itemGetNI( pItem ) + 0x80000000;
                     ZH_PUT_BE_UINT32( &cTemp[ 0 ], uiVal );
                     zh_cdxSortKeyAdd( pSort, pArea->dbfarea.ulRecNo, cTemp, 4 );
                  }
                  else
                  {
                     d = zh_itemGetND( pItem );
                     ZH_DBL2ORD( &d, &cTemp[ 0 ] );
                     zh_cdxSortKeyAdd( pSort, pArea->dbfarea.ulRecNo, cTemp, 8 );
                  }
                  break;

               case ZH_IT_DATE:
                  d = ( double ) zh_itemGetDL( pItem );
                  ZH_DBL2ORD( &d, &cTemp[ 0 ] );
                  zh_cdxSortKeyAdd( pSort, pArea->dbfarea.ulRecNo, cTemp, 8 );
                  break;

               case ZH_IT_TIMESTAMP:
                  d = zh_itemGetTD( pItem );
                  ZH_DBL2ORD( &d, &cTemp[ 0 ] );
                  zh_cdxSortKeyAdd( pSort, pArea->dbfarea.ulRecNo, cTemp, 8 );
                  break;

               case ZH_IT_LOGICAL:
                  cTemp[ 0 ] = ( ZH_BYTE ) ( zh_itemGetL( pItem ) ? 'T' : 'F' );
                  zh_cdxSortKeyAdd( pSort, pArea->dbfarea.ulRecNo, cTemp, 1 );
                  break;

               default:
                  zh_cdxErrorRT( pArea, EG_DATATYPE, EDBF_INVALIDKEY, NULL, 0, 0, NULL );
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

#if defined( ZH_SIXCDX )
         if( pEvalItem )
         {
            if( lStep >= pArea->dbfarea.area.lpdbOrdCondInfo->lStep )
            {
               lStep = 0;
               if( ! zh_cdxEvalCond( pArea, pEvalItem, ZH_FALSE ) )
                  break;
            }
            ++lStep;
         }
#endif

         if( fDirectRead )
            ulRecNo++;
         else
         {
            if( SELF_SKIPRAW( &pArea->dbfarea.area, 1 ) == ZH_FAILURE )
               break;
            if( fUseFilter && SELF_SKIPFILTER( &pArea->dbfarea.area, 1 ) == ZH_FAILURE )
               break;
            ulRecNo = pArea->dbfarea.ulRecNo;
         }
      }

      zh_cdxSortOut( pSort );
      if( pTag->nField )
         zh_itemRelease( pItem );

      if( fDirectRead )
      {
         pArea->dbfarea.pRecord = pSaveRecBuff;
         SELF_GOTO( &pArea->dbfarea.area, ulRecNo );
      }
      pArea->uiTag = uiSaveTag;

#if ! defined( ZH_SIXCDX )
      if( pEvalItem && lStep )
      {
         #if 0
         pArea->dbfarea.area.fEof = ZH_TRUE;
         #endif
         zh_cdxEvalCond( pArea, pEvalItem, ZH_FALSE );
      }
#endif
   }

#if defined( ZH_SIXCDX )
   if( pEvalItem )
   {
      SELF_GOTO( &pArea->dbfarea.area, 0 );
      pArea->dbfarea.area.fBof = ZH_FALSE;
      zh_cdxEvalCond( pArea, pEvalItem, ZH_FALSE );
   }
#endif

   zh_cdxSortFree( pSort );
   pArea->pSort = NULL;

   zh_cdpSelect( cdpTmp );
}


static const RDDFUNCS cdxTable =
{

   /* Movement and positioning methods */

   ( DBENTRYP_BP )    NULL,   /* zh_cdxBof */
   ( DBENTRYP_BP )    NULL,   /* zh_cdxEof */
   ( DBENTRYP_BP )    NULL,   /* zh_cdxFound */
   ( DBENTRYP_V )     zh_cdxGoBottom,
   ( DBENTRYP_UL )    NULL,   /* zh_cdxGoTo */
   ( DBENTRYP_I )     NULL,   /* zh_cdxGoToId */
   ( DBENTRYP_V )     zh_cdxGoTop,
   ( DBENTRYP_BIB )   zh_cdxSeek,
   ( DBENTRYP_L )     zh_cdxSkip,
   ( DBENTRYP_L )     NULL,   /* zh_cdxSkipFilter */
   ( DBENTRYP_L )     zh_cdxSkipRaw,


   /* Data management */

   ( DBENTRYP_VF )    NULL,   /* zh_cdxAddField */
   ( DBENTRYP_B )     NULL,   /* zh_cdxAppend */
   ( DBENTRYP_I )     NULL,   /* zh_cdxCreateFields */
   ( DBENTRYP_V )     NULL,   /* zh_cdxDeleteRec */
   ( DBENTRYP_BP )    NULL,   /* zh_cdxDeleted */
   ( DBENTRYP_SP )    NULL,   /* zh_cdxFieldCount */
   ( DBENTRYP_VF )    NULL,   /* zh_cdxFieldDisplay */
   ( DBENTRYP_SSI )   NULL,   /* zh_cdxFieldInfo */
   ( DBENTRYP_SCP )   NULL,   /* zh_cdxFieldName */
   ( DBENTRYP_V )     zh_cdxFlush,
   ( DBENTRYP_PP )    NULL,   /* zh_cdxGetRec */
   ( DBENTRYP_SI )    NULL,   /* zh_cdxGetValue */
   ( DBENTRYP_SVL )   NULL,   /* zh_cdxGetVarLen */
   ( DBENTRYP_V )     zh_cdxGoCold,
   ( DBENTRYP_V )     zh_cdxGoHot,
   ( DBENTRYP_P )     NULL,   /* zh_cdxPutRec */
   ( DBENTRYP_SI )    NULL,   /* zh_cdxPutValue */
   ( DBENTRYP_V )     NULL,   /* zh_cdxRecall */
   ( DBENTRYP_ULP )   NULL,   /* zh_cdxRecCount */
   ( DBENTRYP_ISI )   NULL,   /* zh_cdxRecInfo */
   ( DBENTRYP_ULP )   NULL,   /* zh_cdxRecNo */
   ( DBENTRYP_I )     NULL,   /* zh_cdxRecId */
   ( DBENTRYP_S )     NULL,   /* zh_cdxSetFieldExtent */


   /* WorkArea/Database management */

   ( DBENTRYP_CP )    NULL,   /* zh_cdxAlias */
   ( DBENTRYP_V )     zh_cdxClose,
   ( DBENTRYP_VO )    NULL,   /* zh_cdxCreate */
   ( DBENTRYP_SI )    NULL,   /* zh_cdxInfo */
   ( DBENTRYP_V )     NULL,   /* zh_cdxNewArea */
   ( DBENTRYP_VO )    zh_cdxOpen,
   ( DBENTRYP_V )     NULL,   /* zh_cdxRelease */
   ( DBENTRYP_SP )    zh_cdxStructSize,
   ( DBENTRYP_CP )    NULL,   /* zh_cdxSysName */
   ( DBENTRYP_VEI )   NULL,   /* zh_cdxEval */
   ( DBENTRYP_V )     zh_cdxPack,
   ( DBENTRYP_LSP )   NULL,   /* zh_cdxPackRec */
   ( DBENTRYP_VS )    NULL,   /* zh_cdxSort */
   ( DBENTRYP_VT )    NULL,   /* zh_cdxTrans */
   ( DBENTRYP_VT )    NULL,   /* zh_cdxTransRec */
   ( DBENTRYP_V )     zh_cdxZap,


   /* Relational Methods */

   ( DBENTRYP_VR )    NULL,   /* zh_cdxChildEnd */
   ( DBENTRYP_VR )    NULL,   /* zh_cdxChildStart */
   ( DBENTRYP_VR )    NULL,   /* zh_cdxChildSync */
   ( DBENTRYP_V )     NULL,   /* zh_cdxSyncChildren */
   ( DBENTRYP_V )     NULL,   /* zh_cdxClearRel */
   ( DBENTRYP_V )     NULL,   /* zh_cdxForceRel */
   ( DBENTRYP_SSP )   NULL,   /* zh_cdxRelArea */
   ( DBENTRYP_VR )    NULL,   /* zh_cdxRelEval */
   ( DBENTRYP_SI )    NULL,   /* zh_cdxRelText */
   ( DBENTRYP_VR )    NULL,   /* zh_cdxSetRel */


   /* Order Management */

   ( DBENTRYP_VOI )   zh_cdxOrderListAdd,
   ( DBENTRYP_V )     zh_cdxOrderListClear,
   ( DBENTRYP_VOI )   zh_cdxOrderListDelete,
   ( DBENTRYP_VOI )   zh_cdxOrderListFocus,
   ( DBENTRYP_V )     zh_cdxOrderListRebuild,
   ( DBENTRYP_VOO )   NULL,   /* zh_cdxOrderCondition */
   ( DBENTRYP_VOC )   zh_cdxOrderCreate,
   ( DBENTRYP_VOI )   zh_cdxOrderDestroy,
   ( DBENTRYP_SVOI )  zh_cdxOrderInfo,


   /* Filters and Scope Settings */

   ( DBENTRYP_V )     zh_cdxClearFilter,
   ( DBENTRYP_V )     NULL,   /* zh_cdxClearLocate */
   ( DBENTRYP_V )     NULL,   /* zh_cdxClearScope */
   ( DBENTRYP_VPLP )  zh_cdxCountScope,
   ( DBENTRYP_I )     NULL,   /* zh_cdxFilterText */
   ( DBENTRYP_SI )    NULL,   /* zh_cdxScopeInfo */
   ( DBENTRYP_VFI )   zh_cdxSetFilter,
   ( DBENTRYP_VLO )   NULL,   /* zh_cdxSetLocate */
   ( DBENTRYP_VOS )   NULL,   /* zh_cdxSetScope */
   ( DBENTRYP_VPL )   NULL,   /* zh_cdxSkipScope */
   ( DBENTRYP_B )     NULL,   /* zh_cdxLocate */


   /* Miscellaneous */

   ( DBENTRYP_CC )    NULL,   /* zh_cdxCompile */
   ( DBENTRYP_I )     NULL,   /* zh_cdxError */
   ( DBENTRYP_I )     NULL,   /* zh_cdxEvalBlock */


   /* Network operations */

   ( DBENTRYP_VSP )   NULL,   /* zh_cdxRawLock */
   ( DBENTRYP_VL )    NULL,   /* zh_cdxLock */
   ( DBENTRYP_I )     NULL,   /* zh_cdxUnLock */


   /* Memofile functions */

   ( DBENTRYP_V )     NULL,   /* zh_cdxCloseMemFile */
   ( DBENTRYP_VO )    NULL,   /* zh_cdxCreateMemFile */
   ( DBENTRYP_SCCS )  NULL,   /* zh_cdxGetValueFile */
   ( DBENTRYP_VO )    NULL,   /* zh_cdxOpenMemFile */
   ( DBENTRYP_SCCS )  NULL,   /* zh_cdxPutValueFile */


   /* Database file header handling */

   ( DBENTRYP_V )     NULL,   /* zh_cdxReadDBHeader */
   ( DBENTRYP_V )     NULL,   /* zh_cdxWriteDBHeader */


   /* non WorkArea functions       */

   ( DBENTRYP_R )     NULL,   /* zh_cdxInit */
   ( DBENTRYP_R )     NULL,   /* zh_cdxExit */
   ( DBENTRYP_RVVL )  NULL,   /* zh_cdxDrop */
   ( DBENTRYP_RVVL )  NULL,   /* zh_cdxExists */
   ( DBENTRYP_RVVVL ) NULL,   /* zh_cdxRename */
   ( DBENTRYP_RSLV )  zh_cdxRddInfo,


   /* Special and reserved methods */

   ( DBENTRYP_SVP )   NULL    /* zh_cdxWhoCares */
};


#if defined( ZH_SIXCDX )
#  define ZH_CDXRDD  "SIXCDX"
#else
#  define ZH_CDXRDD  "DBFCDX"
#endif

ZH_FUNC_STATIC( _GETFUNCTABLE )
{
   RDDFUNCS * pTable;
   ZH_USHORT * puiCount, uiRddId, * puiSuperRddId;

   puiCount = ( ZH_USHORT * ) zh_parptr( 1 );
   pTable = ( RDDFUNCS * ) zh_parptr( 2 );
   uiRddId = ( ZH_USHORT ) zh_parni( 4 );
   puiSuperRddId = ( ZH_USHORT * ) zh_parptr( 5 );

   ZH_TRACE( ZH_TR_DEBUG, ( ZH_CDXRDD "_GETFUNCTABLE(%p, %p)", ( void * ) puiCount, ( void * ) pTable ) );

   if( pTable )
   {
      ZH_ERRCODE errCode;

      if( puiCount )
         *puiCount = RDDFUNCSCOUNT;
      errCode = zh_rddInheritEx( pTable, &cdxTable, &cdxSuper, "DBFFPT", puiSuperRddId );
      if( errCode != ZH_SUCCESS )
         errCode = zh_rddInheritEx( pTable, &cdxTable, &cdxSuper, "DBFDBT", puiSuperRddId );
      if( errCode != ZH_SUCCESS )
         errCode = zh_rddInheritEx( pTable, &cdxTable, &cdxSuper, "DBF", puiSuperRddId );
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
      zh_retni( ZH_FAILURE );
}

static void zh_cdxRddInit( void * cargo )
{
   ZH_SYMBOL_UNUSED( cargo );

   if( zh_rddRegister( "DBF", RDD_REGISTER_TYPE_FULL ) <= 1 )
   {
      zh_rddRegister( "DBFFPT", RDD_REGISTER_TYPE_FULL );
      if( zh_rddRegister( ZH_CDXRDD, RDD_REGISTER_TYPE_FULL ) <= 1 )
         return;
   }

   zh_errInternal( ZH_EI_RDDINVALID, NULL, NULL, NULL );
}

#if defined( ZH_SIXCDX )

ZH_FUNC_TRANSLATE( SIXCDX, _DBF )

ZH_INIT_SYMBOLS_BEGIN( _zh_sixcdx1_InitSymbols_ )
{ "SIXCDX",              {ZH_FS_PUBLIC|ZH_FS_LOCAL}, {ZH_FUNCNAME( SIXCDX )}, NULL },
{ "SIXCDX_GETFUNCTABLE", {ZH_FS_PUBLIC|ZH_FS_LOCAL}, {ZH_FUNCNAME( _GETFUNCTABLE )}, NULL }
ZH_INIT_SYMBOLS_END( _zh_sixcdx1_InitSymbols_ )

ZH_CALL_ON_STARTUP_BEGIN( _zh_sixcdx_rdd_init_ )
   zh_vmAtInit( zh_cdxRddInit, NULL );
ZH_CALL_ON_STARTUP_END( _zh_sixcdx_rdd_init_ )

#if defined( ZH_PRAGMA_STARTUP )
   #pragma startup _zh_sixcdx1_InitSymbols_
   #pragma startup _zh_sixcdx_rdd_init_
#elif defined( ZH_DATASEG_STARTUP )
   #define ZH_DATASEG_BODY    ZH_DATASEG_FUNC( _zh_sixcdx1_InitSymbols_ ) \
                              ZH_DATASEG_FUNC( _zh_sixcdx_rdd_init_ )
   #include "zh_ini_seg.h"
#endif

#else

ZH_FUNC_TRANSLATE( DBFCDX, _DBF )

ZH_INIT_SYMBOLS_BEGIN( _zh_dbfcdx1_InitSymbols_ )
{ "DBFCDX",              {ZH_FS_PUBLIC|ZH_FS_LOCAL}, {ZH_FUNCNAME( DBFCDX )}, NULL },
{ "DBFCDX_GETFUNCTABLE", {ZH_FS_PUBLIC|ZH_FS_LOCAL}, {ZH_FUNCNAME( _GETFUNCTABLE )}, NULL }
ZH_INIT_SYMBOLS_END( _zh_dbfcdx1_InitSymbols_ )

ZH_CALL_ON_STARTUP_BEGIN( _zh_dbfcdx_rdd_init_ )
   zh_vmAtInit( zh_cdxRddInit, NULL );
ZH_CALL_ON_STARTUP_END( _zh_dbfcdx_rdd_init_ )

#if defined( ZH_PRAGMA_STARTUP )
   #pragma startup _zh_dbfcdx1_InitSymbols_
   #pragma startup _zh_dbfcdx_rdd_init_
#elif defined( ZH_DATASEG_STARTUP )
   #define ZH_DATASEG_BODY    ZH_DATASEG_FUNC( _zh_dbfcdx1_InitSymbols_ ) \
                              ZH_DATASEG_FUNC( _zh_dbfcdx_rdd_init_ )
   #include "zh_ini_seg.h"
#endif

#endif
