/*
 * SDF RDD
 *
 * Copyright 2006 Przemyslaw Czerpak
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
#include "zh_init.h"
#include "zh_vm.h"
#include "zh_set.h"
#include "zh_date.h"
#include "zh_rdd_api.h"
#include "zh_item_api.h"
#include "zh_lang_api.h"
#include "zh_error_api.h"
#include "zh_rdd_sdf.h"
#include "zh_rtl/rdd_sys.zhh"

#include "dbf_cdx/zh_dbf_error.h"

#define SUPERTABLE  ( &sdfSuper )

static RDDFUNCS        sdfSuper;
static const ZH_USHORT s_uiNumLength[ 9 ] = { 0, 4, 6, 8, 11, 13, 16, 18, 20 };

static void zh_sdfInitArea( SDFAREAP pArea, char * szFileName )
{
   const char * szEol;

   /* Allocate only after successfully open file */
   pArea->szFileName = zh_strdup( szFileName );

   /* set line separator: EOL */
   szEol = zh_setGetEOL();
   if( ! szEol || ! szEol[ 0 ] )
      szEol = zh_conNewLine();
   pArea->szEol = zh_strdup( szEol );
   pArea->uiEolLen = ( ZH_USHORT ) strlen( szEol );
   pArea->fAnyEol = ( szEol[ 0 ] == '\n' || szEol[ 0 ] == '\r' ) &&
                    ( pArea->uiEolLen == 1 ||
                      ( pArea->uiEolLen == 2 && szEol[ 0 ] != szEol[ 1 ] &&
                        ( szEol[ 1 ] == '\n' || szEol[ 1 ] == '\r' ) ) );

   /* allocate record buffer, one additional byte is for deleted flag */
   pArea->pRecord = ( ZH_BYTE * ) zh_xgrab( pArea->uiRecordLen + pArea->uiEolLen + 1 );
   /* pseudo deleted flag */
   *pArea->pRecord++ = ' ';
   memcpy( pArea->pRecord + pArea->uiRecordLen,
           pArea->szEol, pArea->uiEolLen );

   if( pArea->fReadonly )
   {
      /* allocate IO buffer */
      pArea->nBufferSize += pArea->fAnyEol ? 2 : pArea->uiEolLen;
      if( pArea->nBufferSize < 8192 )
         pArea->nBufferSize = 8192;
      pArea->pBuffer = ( ZH_BYTE * ) zh_xgrab( pArea->nBufferSize );
   }
   pArea->ulRecCount = 0;
   pArea->nBufferIndex = pArea->nBufferRead = pArea->nBufferSize;
}

static void zh_sdfClearRecordBuffer( SDFAREAP pArea )
{
   memset( pArea->pRecord, ' ', pArea->uiRecordLen );
}

static ZH_ERRCODE zh_sdfReadRecord( SDFAREAP pArea )
{
   ZH_SIZE nRead;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_sdfReadRecord(%p)", ( void * ) pArea ) );

   pArea->area.fEof = ZH_TRUE;

   nRead = 0;
   for( ;; )
   {
      char ch;

      if( pArea->nBufferRead - pArea->nBufferIndex < ( ZH_SIZE ) pArea->uiEolLen + 1 &&
          pArea->nBufferRead == pArea->nBufferSize )
      {
         ZH_SIZE nLeft = pArea->nBufferRead - pArea->nBufferIndex;

         if( nLeft )
            memmove( pArea->pBuffer,
                     pArea->pBuffer + pArea->nBufferIndex, nLeft );
         pArea->nBufferIndex = 0;
         pArea->nBufferRead = zh_fileRead( pArea->pFile,
                                           pArea->pBuffer + nLeft,
                                           pArea->nBufferSize - nLeft, -1 );
         if( pArea->nBufferRead == ( ZH_SIZE ) FS_ERROR )
            pArea->nBufferRead = 0;
         pArea->nBufferRead += nLeft;
      }

      if( pArea->nBufferIndex >= pArea->nBufferRead )
         break;

      ch = pArea->pBuffer[ pArea->nBufferIndex++ ];

      if( pArea->fAnyEol )
      {
         if( ch == '\r' || ch == '\n' )
         {
            if( pArea->nBufferIndex < pArea->nBufferRead &&
                pArea->pBuffer[ pArea->nBufferIndex ] != ch &&
                ( pArea->pBuffer[ pArea->nBufferIndex ] == '\r' ||
                  pArea->pBuffer[ pArea->nBufferIndex ] == '\n' ) )
               pArea->nBufferIndex++;
            pArea->area.fEof = ZH_FALSE;
            break;
         }
      }
      else if( ch == pArea->szEol[ 0 ] )
      {
         if( pArea->uiEolLen == 1 ||
             ( pArea->nBufferRead - pArea->nBufferIndex >=
               ( ZH_SIZE ) pArea->uiEolLen - 1 &&
               memcmp( pArea->pBuffer + pArea->nBufferIndex,
                       pArea->szEol + 1, pArea->uiEolLen - 1 ) == 0 ) )
         {
            pArea->nBufferIndex += pArea->uiEolLen - 1;
            pArea->area.fEof = ZH_FALSE;
            break;
         }
      }
      if( nRead < ( ZH_SIZE ) pArea->uiRecordLen && ch != '\032' )
         pArea->pRecord[ nRead++ ] = ch;
   }

   if( nRead < ( ZH_SIZE ) pArea->uiRecordLen )
      memset( pArea->pRecord + nRead, ' ', pArea->uiRecordLen - nRead );
   if( nRead > 0 )
      pArea->area.fEof = ZH_FALSE;

   pArea->fPositioned = ! pArea->area.fEof;

   return ZH_SUCCESS;
}

static ZH_ERRCODE zh_sdfNextRecord( SDFAREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_sdfNextRecord(%p)", ( void * ) pArea ) );

   if( pArea->fPositioned )
   {
      pArea->ulRecNo++;
      return zh_sdfReadRecord( pArea );
   }
   return ZH_SUCCESS;
}

/*
 * -- SDF METHODS --
 */

/*
 * Position cursor at a specific physical record.
 */
static ZH_ERRCODE zh_sdfGoTo( SDFAREAP pArea, ZH_ULONG ulRecNo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_sdfGoTo(%p, %lu)", ( void * ) pArea, ulRecNo ) );

   if( pArea->fReadonly && ulRecNo >= pArea->ulRecNo )
   {
      while( pArea->ulRecNo < ulRecNo && pArea->fPositioned )
      {
         if( zh_sdfNextRecord( pArea ) != ZH_SUCCESS )
            return ZH_FAILURE;
      }
      return ZH_SUCCESS;
   }
   /* generate RTE */
   return SUPER_GOTO( &pArea->area, ulRecNo );
}

/*
 * Position the cursor to a specific, physical identity.
 */
static ZH_ERRCODE zh_sdfGoToId( SDFAREAP pArea, PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_sdfGoToId(%p, %p)", ( void * ) pArea, ( void * ) pItem ) );

   if( ZH_IS_NUMERIC( pItem ) )
      return SELF_GOTO( &pArea->area, zh_itemGetNL( pItem ) );

   /* generate RTE */
   return SUPER_GOTOID( &pArea->area, pItem );
}

/*
 * Position cursor at the first record.
 */
static ZH_ERRCODE zh_sdfGoTop( SDFAREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_sdfGoTop(%p)", ( void * ) pArea ) );

   if( SELF_GOCOLD( &pArea->area ) != ZH_SUCCESS )
      return ZH_FAILURE;

   pArea->area.fTop = ZH_TRUE;
   pArea->area.fBottom = ZH_FALSE;

   if( pArea->ulRecNo != 1 )
   {
      if( pArea->ulRecNo != 0 || ! pArea->fReadonly )
         /* generate RTE */
         return SUPER_GOTOP( &pArea->area );

      pArea->ulRecNo = 1;
      if( zh_sdfReadRecord( pArea ) != ZH_SUCCESS )
         return ZH_FAILURE;
   }

   return SELF_SKIPFILTER( &pArea->area, 1 );
}

/*
 * Reposition cursor, regardless of filter.
 */
static ZH_ERRCODE zh_sdfSkipRaw( SDFAREAP pArea, ZH_LONG lToSkip )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_sdfSkipRaw(%p,%ld)", ( void * ) pArea, lToSkip ) );

   if( SELF_GOCOLD( &pArea->area ) != ZH_SUCCESS )
      return ZH_FAILURE;

   if( lToSkip != 1 || ! pArea->fReadonly )
      /* generate RTE */
      return SUPER_SKIPRAW( &pArea->area, lToSkip );
   else
      return zh_sdfNextRecord( pArea );
}

/*
 * Determine deleted status for a record.
 */
static ZH_ERRCODE zh_sdfDeleted( SDFAREAP pArea, ZH_BOOL * pDeleted )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_sdfDeleted(%p,%p)", ( void * ) pArea, ( void * ) pDeleted ) );

   ZH_SYMBOL_UNUSED( pArea );

   *pDeleted = ZH_FALSE;

   return ZH_SUCCESS;
}

/*
 * Obtain number of records in WorkArea.
 */
static ZH_ERRCODE zh_sdfRecCount( SDFAREAP pArea, ZH_ULONG * pRecCount )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_sdfRecCount(%p,%p)", ( void * ) pArea, ( void * ) pRecCount ) );

   *pRecCount = pArea->ulRecCount;

   return ZH_SUCCESS;
}

/*
 * Obtain physical row number at current WorkArea cursor position.
 */
static ZH_ERRCODE zh_sdfRecNo( SDFAREAP pArea, ZH_ULONG * pulRecNo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_sdfRecNo(%p,%p)", ( void * ) pArea, ( void * ) pulRecNo ) );

   *pulRecNo = pArea->ulRecNo;

   return ZH_SUCCESS;
}

/*
 * Obtain physical row ID at current WorkArea cursor position.
 */
static ZH_ERRCODE zh_sdfRecId( SDFAREAP pArea, PZH_ITEM pRecNo )
{
   ZH_ERRCODE errCode;
   ZH_ULONG ulRecNo;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_sdfRecId(%p,%p)", ( void * ) pArea, ( void * ) pRecNo ) );

   errCode = SELF_RECNO( &pArea->area, &ulRecNo );

   if( ulRecNo < 10000000 )
   {
      zh_itemPutNLLen( pRecNo, ulRecNo, 7 );
   }
   else
   {
      zh_itemPutNLLen( pRecNo, ulRecNo, 10 );
   }
   return errCode;
}

/*
 * Append a record to the WorkArea.
 */
static ZH_ERRCODE zh_sdfAppend( SDFAREAP pArea, ZH_BOOL fUnLockAll )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_sdfAppend(%p,%d)", ( void * ) pArea, ( int ) fUnLockAll ) );

   ZH_SYMBOL_UNUSED( fUnLockAll );

   if( SELF_GOCOLD( &pArea->area ) != ZH_SUCCESS )
      return ZH_FAILURE;

   if( SELF_GOHOT( &pArea->area ) != ZH_SUCCESS )
      return ZH_FAILURE;

   pArea->ulRecNo = ++pArea->ulRecCount;
   pArea->area.fEof = ZH_FALSE;
   pArea->fPositioned = ZH_TRUE;
   zh_sdfClearRecordBuffer( pArea );

   return ZH_SUCCESS;
}

/*
 * Delete a record.
 */
static ZH_ERRCODE zh_sdfDeleteRec( SDFAREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_sdfDeleteRec(%p)", ( void * ) pArea ) );

   ZH_SYMBOL_UNUSED( pArea );

   /* It's not Cl*pper compatible so I had to disable it [druzus] */
#if 0
   if( pArea->fRecordChanged )
   {
      pArea->ulRecCount--;
      pArea->area.fEof = ZH_TRUE;
      pArea->fPositioned = pArea->fRecordChanged = ZH_FALSE;
      zh_sdfClearRecordBuffer( pArea );
   }
#endif

   return ZH_SUCCESS;
}

/*
 * Undelete the current record.
 */
static ZH_ERRCODE zh_sdfRecall( SDFAREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_sdfRecall(%p)", ( void * ) pArea ) );

   ZH_SYMBOL_UNUSED( pArea );

   return ZH_SUCCESS;
}

/*
 * Obtain the current value of a field.
 */
static ZH_ERRCODE zh_sdfGetValue( SDFAREAP pArea, ZH_USHORT uiIndex, PZH_ITEM pItem )
{
   LPFIELD pField;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_sdfGetValue(%p, %hu, %p)", ( void * ) pArea, uiIndex, ( void * ) pItem ) );

   if( --uiIndex >= pArea->area.uiFieldCount )
      return ZH_FAILURE;

   pField = pArea->area.lpFields + uiIndex;
   switch( pField->uiType )
   {
      case ZH_FT_STRING:
         if( ( pField->uiFlags & ZH_FF_BINARY ) == 0 )
         {
            ZH_SIZE nLen = pField->uiLen;
            char * pszVal = zh_cdpnDup( ( const char * ) pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                                        &nLen, pArea->area.cdPage, zh_vmCDP() );
            zh_itemPutCLPtr( pItem, pszVal, nLen );
         }
         else
         {
            zh_itemPutCL( pItem, ( char * ) pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                          pField->uiLen );
         }
         break;

      case ZH_FT_LOGICAL:
         switch( pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ] )
         {
            case 'T':
            case 't':
            case 'Y':
            case 'y':
               zh_itemPutL( pItem, ZH_TRUE );
               break;
            default:
               zh_itemPutL( pItem, ZH_FALSE );
               break;
         }
         break;

      case ZH_FT_DATE:
         zh_itemPutDS( pItem, ( const char * ) pArea->pRecord + pArea->pFieldOffset[ uiIndex ] );
         break;

      case ZH_FT_TIMESTAMP:
      {
         long lJulian, lMilliSec;
         ZH_BYTE * pFieldPtr = pArea->pRecord + pArea->pFieldOffset[ uiIndex ], bChar;

         bChar = pFieldPtr[ pField->uiLen ];
         pFieldPtr[ pField->uiLen ] = 0;
         zh_timeStampStrGetDT( ( const char * ) pFieldPtr, &lJulian, &lMilliSec );
         pFieldPtr[ pField->uiLen ] = bChar;
         zh_itemPutTDT( pItem, lJulian, lMilliSec );
         break;
      }

      case ZH_FT_LONG:
      {
         ZH_MAXINT lVal;
         double dVal;
         ZH_BOOL fDbl;

         fDbl = zh_strnToNum( ( const char * ) pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                              pField->uiLen, &lVal, &dVal );

         if( pField->uiDec )
            zh_itemPutNDLen( pItem, fDbl ? dVal : ( double ) lVal,
                             ( int ) ( pField->uiLen - pField->uiDec - 1 ),
                             ( int ) pField->uiDec );
         else if( fDbl )
            zh_itemPutNDLen( pItem, dVal, ( int ) pField->uiLen, 0 );
         else
            zh_itemPutNIntLen( pItem, lVal, ( int ) pField->uiLen );
         break;
      }

      case ZH_FT_MEMO:
         zh_itemPutC( pItem, NULL );
         break;

      case ZH_FT_NONE:
         zh_itemClear( pItem );
         break;

      default:
      {
         PZH_ITEM pError = zh_errNew();
         zh_errPutGenCode( pError, EG_DATATYPE );
         zh_errPutDescription( pError, zh_langDGetErrorDesc( EG_DATATYPE ) );
         zh_errPutOperation( pError, zh_dynsymName( ( PZH_DYNS ) pField->sym ) );
         zh_errPutSubCode( pError, EDBF_DATATYPE );
         SELF_ERROR( &pArea->area, pError );
         zh_itemRelease( pError );
         return ZH_FAILURE;
      }
   }

   return ZH_SUCCESS;
}

/*
 * Assign a value to a field.
 */
static ZH_ERRCODE zh_sdfPutValue( SDFAREAP pArea, ZH_USHORT uiIndex, PZH_ITEM pItem )
{
   ZH_ERRCODE errCode;
   LPFIELD pField;
   ZH_SIZE nSize;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_sdfPutValue(%p,%hu,%p)", ( void * ) pArea, uiIndex, ( void * ) pItem ) );

   if( ! pArea->fPositioned )
      return ZH_SUCCESS;

   if( ! pArea->fRecordChanged )
      return ZH_FAILURE;

   if( --uiIndex >= pArea->area.uiFieldCount )
      return ZH_FAILURE;

   errCode = ZH_SUCCESS;
   pField = pArea->area.lpFields + uiIndex;
   if( pField->uiType != ZH_FT_MEMO && pField->uiType != ZH_FT_NONE )
   {
      char szBuffer[ 256 ];

      if( ZH_IS_MEMO( pItem ) || ZH_IS_STRING( pItem ) )
      {
         if( pField->uiType == ZH_FT_STRING )
         {
            if( ( pField->uiFlags & ZH_FF_BINARY ) == 0 )
            {
               nSize = pField->uiLen;
               zh_cdpnDup2( zh_itemGetCPtr( pItem ), zh_itemGetCLen( pItem ),
                            ( char * ) pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                            &nSize, zh_vmCDP(), pArea->area.cdPage );
            }
            else
            {
               nSize = zh_itemGetCLen( pItem );
               if( nSize > ( ZH_SIZE ) pField->uiLen )
                  nSize = pField->uiLen;
               memcpy( pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                       zh_itemGetCPtr( pItem ), nSize );
            }
            if( nSize < ( ZH_SIZE ) pField->uiLen )
               memset( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] + nSize,
                       ' ', pField->uiLen - nSize );
         }
         else
            errCode = EDBF_DATATYPE;
      }
      else if( ZH_IS_DATETIME( pItem ) )
      {
         if( pField->uiType == ZH_FT_DATE )
         {
            zh_itemGetDS( pItem, szBuffer );
            memcpy( pArea->pRecord + pArea->pFieldOffset[ uiIndex ], szBuffer, 8 );
         }
         else if( pField->uiType == ZH_FT_TIMESTAMP &&
                  ( pField->uiLen == 12 || pField->uiLen == 23 ) )
         {
            long lDate, lTime;
            zh_itemGetTDT( pItem, &lDate, &lTime );
            if( pField->uiLen == 12 )
               zh_timeStr( szBuffer, lTime );
            else
               zh_timeStampStr( szBuffer, lDate, lTime );
            memcpy( pArea->pRecord + pArea->pFieldOffset[ uiIndex ], szBuffer, pField->uiLen );
         }
         else
            errCode = EDBF_DATATYPE;
      }
      else if( ZH_IS_NUMBER( pItem ) )
      {
         if( pField->uiType == ZH_FT_LONG )
         {
            if( zh_itemStrBuf( szBuffer, pItem, pField->uiLen, pField->uiDec ) )
            {
               memcpy( pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                       szBuffer, pField->uiLen );
            }
            else
            {
               errCode = EDBF_DATAWIDTH;
               memset( pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                       '*', pField->uiLen );
            }
         }
         else
            errCode = EDBF_DATATYPE;
      }
      else if( ZH_IS_LOGICAL( pItem ) )
      {
         if( pField->uiType == ZH_FT_LOGICAL )
            pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ] = zh_itemGetL( pItem ) ? 'T' : 'F';
         else
            errCode = EDBF_DATATYPE;
      }
      else
         errCode = EDBF_DATATYPE;
   }

   if( errCode != ZH_SUCCESS )
   {
      PZH_ITEM pError = zh_errNew();
      ZH_ERRCODE errGenCode = errCode == EDBF_DATAWIDTH ? EG_DATAWIDTH : EDBF_DATATYPE;

      zh_errPutGenCode( pError, errGenCode );
      zh_errPutDescription( pError, zh_langDGetErrorDesc( errGenCode ) );
      zh_errPutOperation( pError, zh_dynsymName( ( PZH_DYNS ) pField->sym ) );
      zh_errPutSubCode( pError, errCode );
      zh_errPutFlags( pError, EF_CANDEFAULT );
      errCode = SELF_ERROR( &pArea->area, pError );
      zh_itemRelease( pError );
      return errCode == E_DEFAULT ? ZH_SUCCESS : ZH_FAILURE;
   }

   return ZH_SUCCESS;
}

/*
 * Replace the current record.
 */
static ZH_ERRCODE zh_sdfPutRec( SDFAREAP pArea, ZH_BYTE * pBuffer )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_sdfPutRec(%p,%p)", ( void * ) pArea, ( void * ) pBuffer ) );

   if( ! pArea->fPositioned )
      return ZH_SUCCESS;

   if( ! pArea->fRecordChanged )
      return ZH_FAILURE;

   /* Copy data to buffer */
   memcpy( pArea->pRecord, pBuffer + 1, pArea->uiRecordLen );

   return ZH_SUCCESS;
}

/*
 * Retrieve current record buffer
 */
static ZH_ERRCODE zh_sdfGetRec( SDFAREAP pArea, ZH_BYTE ** pBufferPtr )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_sdfGetRec(%p,%p)", ( void * ) pArea, ( void * ) pBufferPtr ) );

   *pBufferPtr = pArea->pRecord - 1;

   return ZH_SUCCESS;
}

/*
 * Copy one or more records from one WorkArea to another.
 */
static ZH_ERRCODE zh_sdfTrans( SDFAREAP pArea, LPDBTRANSINFO pTransInfo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_sdfTrans(%p, %p)", ( void * ) pArea, ( void * ) pTransInfo ) );

   if( pTransInfo->uiFlags & DBTF_MATCH )
   {
      if( ! pArea->fTransRec || pArea->area.cdPage != pTransInfo->lpaDest->cdPage )
         pTransInfo->uiFlags &= ~DBTF_PUTREC;
      else if( pArea->area.rddID == pTransInfo->lpaDest->rddID )
         pTransInfo->uiFlags |= DBTF_PUTREC;
      else
      {
         PZH_ITEM pPutRec = zh_itemPutL( NULL, ZH_FALSE );
         if( SELF_INFO( pTransInfo->lpaDest, DBI_CANPUTREC, pPutRec ) != ZH_SUCCESS )
         {
            zh_itemRelease( pPutRec );
            return ZH_FAILURE;
         }
         if( zh_itemGetL( pPutRec ) )
            pTransInfo->uiFlags |= DBTF_PUTREC;
         else
            pTransInfo->uiFlags &= ~DBTF_PUTREC;
         zh_itemRelease( pPutRec );
      }
   }
   return SUPER_TRANS( &pArea->area, pTransInfo );
}

/*
 * Perform a write of WorkArea memory to the data store.
 */
static ZH_ERRCODE zh_sdfGoCold( SDFAREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_sdfGoCold(%p)", ( void * ) pArea ) );

   if( pArea->fRecordChanged )
   {
      ZH_SIZE nSize = pArea->uiRecordLen + pArea->uiEolLen;

      if( zh_fileWrite( pArea->pFile, pArea->pRecord, nSize, -1 ) != nSize )
      {
         PZH_ITEM pError = zh_errNew();

         zh_errPutGenCode( pError, EG_WRITE );
         zh_errPutDescription( pError, zh_langDGetErrorDesc( EG_WRITE ) );
         zh_errPutSubCode( pError, EDBF_WRITE );
         zh_errPutOsCode( pError, zh_fsError() );
         zh_errPutFileName( pError, pArea->szFileName );
         SELF_ERROR( &pArea->area, pError );
         zh_itemRelease( pError );
         return ZH_FAILURE;
      }
      pArea->fRecordChanged = ZH_FALSE;
      pArea->fFlush = ZH_TRUE;
   }
   return ZH_SUCCESS;
}

/*
 * Mark the WorkArea data buffer as hot.
 */
static ZH_ERRCODE zh_sdfGoHot( SDFAREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_sdfGoHot(%p)", ( void * ) pArea ) );

   if( pArea->fReadonly )
   {
      PZH_ITEM pError = zh_errNew();
      zh_errPutGenCode( pError, EG_READONLY );
      zh_errPutDescription( pError, zh_langDGetErrorDesc( EG_READONLY ) );
      zh_errPutSubCode( pError, EDBF_READONLY );
      SELF_ERROR( &pArea->area, pError );
      zh_itemRelease( pError );
      return ZH_FAILURE;
   }
   pArea->fRecordChanged = ZH_TRUE;
   return ZH_SUCCESS;
}

/*
 * Write data buffer to the data store.
 */
static ZH_ERRCODE zh_sdfFlush( SDFAREAP pArea )
{
   ZH_ERRCODE errCode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_sdfFlush(%p)", ( void * ) pArea ) );

   errCode = SELF_GOCOLD( &pArea->area );

   if( pArea->fFlush && zh_setGetHardCommit() )
   {
      zh_fileCommit( pArea->pFile );
      pArea->fFlush = ZH_FALSE;
   }

   return errCode;
}

/*
 * Retrieve information about the current table/driver.
 */
static ZH_ERRCODE zh_sdfInfo( SDFAREAP pArea, ZH_USHORT uiIndex, PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_sdfInfo(%p,%hu,%p)", ( void * ) pArea, uiIndex, ( void * ) pItem ) );

   switch( uiIndex )
   {
      case DBI_CANPUTREC:
         zh_itemPutL( pItem, pArea->fTransRec );
         break;

      case DBI_GETRECSIZE:
         zh_itemPutNL( pItem, pArea->uiRecordLen );
         break;

      case DBI_FULLPATH:
         zh_itemPutC( pItem, pArea->szFileName );
         break;

      case DBI_FILEHANDLE:
         zh_itemPutNInt( pItem, ( ZH_NHANDLE ) zh_fileHandle( pArea->pFile ) );
         break;

      case DBI_SHARED:
         zh_itemPutL( pItem, pArea->fShared );
         break;

      case DBI_ISREADONLY:
         zh_itemPutL( pItem, pArea->fReadonly );
         break;

      case DBI_POSITIONED:
         zh_itemPutL( pItem, pArea->fPositioned );
         break;

      case DBI_DB_VERSION:
      case DBI_RDD_VERSION:
      {
         char szBuf[ 64 ];
         int iSub = zh_itemGetNI( pItem );

         if( iSub == 1 )
            zh_snprintf( szBuf, sizeof( szBuf ), "%d.%d (%s)", 0, 1, "SDF" );
         else if( iSub == 2 )
            zh_snprintf( szBuf, sizeof( szBuf ), "%d.%d (%s:%d)", 0, 1, "SDF", pArea->area.rddID );
         else
            zh_snprintf( szBuf, sizeof( szBuf ), "%d.%d", 0, 1 );
         zh_itemPutC( pItem, szBuf );
         break;
      }

      default:
         return SUPER_INFO( &pArea->area, uiIndex, pItem );
   }

   return ZH_SUCCESS;
}

/*
 * Add a field to the WorkArea.
 */
static ZH_ERRCODE zh_sdfAddField( SDFAREAP pArea, LPDBFIELDINFO pFieldInfo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_sdfAddField(%p, %p)", ( void * ) pArea, ( void * ) pFieldInfo ) );

   switch( pFieldInfo->uiType )
   {
      case ZH_FT_MEMO:
      case ZH_FT_IMAGE:
      case ZH_FT_BLOB:
      case ZH_FT_OLE:
         pFieldInfo->uiType = ZH_FT_MEMO;
         pFieldInfo->uiLen = 0;
         pArea->fTransRec = ZH_FALSE;
         break;

      case ZH_FT_ANY:
         if( pFieldInfo->uiLen == 3 )
         {
            pFieldInfo->uiType = ZH_FT_DATE;
            pFieldInfo->uiLen = 8;
         }
         else if( pFieldInfo->uiLen < 6 )
         {
            pFieldInfo->uiType = ZH_FT_LONG;
            pFieldInfo->uiLen = s_uiNumLength[ pFieldInfo->uiLen ];
         }
         else
         {
            pFieldInfo->uiType = ZH_FT_MEMO;
            pFieldInfo->uiLen = 0;
         }
         pArea->fTransRec = ZH_FALSE;
         break;

      case ZH_FT_DATE:
         if( pFieldInfo->uiLen != 8 )
         {
            pFieldInfo->uiLen = 8;
            pArea->fTransRec = ZH_FALSE;
         }
         break;

      case ZH_FT_STRING:
      case ZH_FT_LONG:
         break;

      case ZH_FT_FLOAT:
         pFieldInfo->uiType = ZH_FT_LONG;
         break;

      case ZH_FT_INTEGER:
      case ZH_FT_CURRENCY:
      case ZH_FT_ROWVER:
      case ZH_FT_AUTOINC:
         pFieldInfo->uiType = ZH_FT_LONG;
         pFieldInfo->uiLen = s_uiNumLength[ pFieldInfo->uiLen ];
         if( pFieldInfo->uiDec )
            pFieldInfo->uiLen++;
         pArea->fTransRec = ZH_FALSE;
         break;

      case ZH_FT_DOUBLE:
      case ZH_FT_CURDOUBLE:
         pFieldInfo->uiType = ZH_FT_LONG;
         pFieldInfo->uiLen = 20;
         pArea->fTransRec = ZH_FALSE;
         break;

      case ZH_FT_VARLENGTH:
         pFieldInfo->uiType = ZH_FT_STRING;
         pArea->fTransRec = ZH_FALSE;
         break;

      case ZH_FT_LOGICAL:
         if( pFieldInfo->uiLen != 1 )
         {
            pFieldInfo->uiLen = 1;
            pArea->fTransRec = ZH_FALSE;
         }
         break;

      case ZH_FT_TIME:
         pFieldInfo->uiType = ZH_FT_TIMESTAMP;
         pFieldInfo->uiLen = 12;
         pArea->fTransRec = ZH_FALSE;
         break;

      case ZH_FT_TIMESTAMP:
      case ZH_FT_MODTIME:
         pFieldInfo->uiType = ZH_FT_TIMESTAMP;
         pFieldInfo->uiLen = 23;
         pArea->fTransRec = ZH_FALSE;
         break;

      default:
         pFieldInfo->uiType = ZH_FT_NONE;
         pFieldInfo->uiLen = 0;
         pArea->fTransRec = ZH_FALSE;
         break;
   }

   pFieldInfo->uiFlags &= ~ZH_FF_AUTOINC;

   /* Update field offset */
   pArea->pFieldOffset[ pArea->area.uiFieldCount ] = pArea->uiRecordLen;
   pArea->uiRecordLen += pFieldInfo->uiLen;

   return SUPER_ADDFIELD( &pArea->area, pFieldInfo );
}

/*
 * Establish the extent of the array of fields for a WorkArea.
 */
static ZH_ERRCODE zh_sdfSetFieldExtent( SDFAREAP pArea, ZH_USHORT uiFieldExtent )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_sdfSetFieldExtent(%p,%hu)", ( void * ) pArea, uiFieldExtent ) );

   if( SUPER_SETFIELDEXTENT( &pArea->area, uiFieldExtent ) == ZH_FAILURE )
      return ZH_FAILURE;

   /* Alloc field offsets array */
   if( uiFieldExtent )
      pArea->pFieldOffset = ( ZH_USHORT * ) zh_xgrabz( uiFieldExtent * sizeof( ZH_USHORT ) );

   return ZH_SUCCESS;
}

/*
 * Clear the WorkArea for use.
 */
static ZH_ERRCODE zh_sdfNewArea( SDFAREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_sdfNewArea(%p)", ( void * ) pArea ) );

   if( SUPER_NEW( &pArea->area ) == ZH_FAILURE )
      return ZH_FAILURE;

   pArea->pFile = NULL;
   pArea->fTransRec = ZH_TRUE;
   pArea->uiRecordLen = 0;
   pArea->nBufferSize = 0;

   return ZH_SUCCESS;
}

/*
 * Retrieve the size of the WorkArea structure.
 */
static ZH_ERRCODE zh_sdfStructSize( SDFAREAP pArea, ZH_USHORT * uiSize )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_sdfStrucSize(%p,%p)", ( void * ) pArea, ( void * ) uiSize ) );
   ZH_SYMBOL_UNUSED( pArea );

   *uiSize = sizeof( SDFAREA );
   return ZH_SUCCESS;
}

/*
 * Close the table in the WorkArea.
 */
static ZH_ERRCODE zh_sdfClose( SDFAREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_sdfClose(%p)", ( void * ) pArea ) );

   /* Update record and unlock records */
   if( pArea->pFile )
   {
      SELF_GOCOLD( &pArea->area );

      if( ! pArea->fReadonly && zh_setGetEOF() )
      {
         zh_fileWrite( pArea->pFile, "\032", 1, -1 );
         pArea->fFlush = ZH_TRUE;
      }
      SELF_FLUSH( &pArea->area );
      zh_fileClose( pArea->pFile );
      pArea->pFile = NULL;
   }

   SUPER_CLOSE( &pArea->area );

   if( pArea->pFieldOffset )
   {
      zh_xfree( pArea->pFieldOffset );
      pArea->pFieldOffset = NULL;
   }
   if( pArea->pRecord )
   {
      zh_xfree( pArea->pRecord - 1 );
      pArea->pRecord = NULL;
   }
   if( pArea->pBuffer )
   {
      zh_xfree( pArea->pBuffer );
      pArea->pBuffer = NULL;
   }
   if( pArea->szEol )
   {
      zh_xfree( pArea->szEol );
      pArea->szEol = NULL;
   }
   if( pArea->szFileName )
   {
      zh_xfree( pArea->szFileName );
      pArea->szFileName = NULL;
   }

   return ZH_SUCCESS;
}

/*
 * Create a data store in the specified WorkArea.
 */
static ZH_ERRCODE zh_sdfCreate( SDFAREAP pArea, LPDBOPENINFO pCreateInfo )
{
   PZH_ITEM pError = NULL;
   ZH_ERRCODE errCode;
   ZH_BOOL fRetry;
   PZH_FNAME pFileName;
   char szFileName[ ZH_PATH_MAX ];

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_sdfCreate(%p,%p)", ( void * ) pArea, ( void * ) pCreateInfo ) );

   pArea->fShared = ZH_FALSE;    /* pCreateInfo->fShared; */
   pArea->fReadonly = ZH_FALSE;  /* pCreateInfo->fReadonly */

   if( pCreateInfo->cdpId )
   {
      pArea->area.cdPage = zh_cdpFindExt( pCreateInfo->cdpId );
      if( ! pArea->area.cdPage )
         pArea->area.cdPage = zh_vmCDP();
   }
   else
      pArea->area.cdPage = zh_vmCDP();

   pFileName = zh_fsFNameSplit( pCreateInfo->abName );
   if( zh_setGetDefExtension() && ! pFileName->szExtension )
   {
      PZH_ITEM pItem = zh_itemNew( NULL );
      if( SELF_INFO( &pArea->area, DBI_TABLEEXT, pItem ) == ZH_SUCCESS )
      {
         pFileName->szExtension = zh_itemGetCPtr( pItem );
         zh_fsFNameMerge( szFileName, pFileName );
      }
      zh_itemRelease( pItem );
   }
   else
   {
      zh_strncpy( szFileName, pCreateInfo->abName, sizeof( szFileName ) - 1 );
   }
   zh_xfree( pFileName );

   /* Try create */
   do
   {
      pArea->pFile = zh_fileExtOpen( szFileName, NULL,
                                     FO_WRITE | FO_EXCLUSIVE | FXO_TRUNCATE |
                                     FXO_DEFAULTS | FXO_SHARELOCK | FXO_COPYNAME,
                                     NULL, pError );
      if( ! pArea->pFile )
      {
         if( ! pError )
         {
            pError = zh_errNew();
            zh_errPutGenCode( pError, EG_CREATE );
            zh_errPutSubCode( pError, EDBF_CREATE_DBF );
            zh_errPutOsCode( pError, zh_fsError() );
            zh_errPutDescription( pError, zh_langDGetErrorDesc( EG_CREATE ) );
            zh_errPutFileName( pError, szFileName );
            zh_errPutFlags( pError, EF_CANRETRY | EF_CANDEFAULT );
         }
         fRetry = ( SELF_ERROR( &pArea->area, pError ) == E_RETRY );
      }
      else
         fRetry = ZH_FALSE;
   }
   while( fRetry );

   if( pError )
      zh_itemRelease( pError );

   if( ! pArea->pFile )
      return ZH_FAILURE;

   errCode = SUPER_CREATE( &pArea->area, pCreateInfo );
   if( errCode != ZH_SUCCESS )
   {
      SELF_CLOSE( &pArea->area );
      return errCode;
   }

   zh_sdfInitArea( pArea, szFileName );
   pArea->ulRecNo = 1;
   pArea->area.fEof = ZH_TRUE;
   pArea->fPositioned = ZH_FALSE;
   zh_sdfClearRecordBuffer( pArea );

   return ZH_SUCCESS;
}

/*
 * Open a data store in the WorkArea.
 */
static ZH_ERRCODE zh_sdfOpen( SDFAREAP pArea, LPDBOPENINFO pOpenInfo )
{
   PZH_ITEM pError = NULL;
   PZH_FNAME pFileName;
   ZH_ERRCODE errCode;
   ZH_USHORT uiFlags;
   ZH_BOOL fRetry;
   char szFileName[ ZH_PATH_MAX ];
   char szAlias[ ZH_RDD_MAX_ALIAS_LEN + 1 ];

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_sdfOpen(%p,%p)", ( void * ) pArea, ( void * ) pOpenInfo ) );

   pArea->fShared = ZH_TRUE;     /* pOpenInfo->fShared; */
   pArea->fReadonly = ZH_TRUE;   /* pOpenInfo->fReadonly; */

   if( pOpenInfo->cdpId )
   {
      pArea->area.cdPage = zh_cdpFindExt( pOpenInfo->cdpId );
      if( ! pArea->area.cdPage )
         pArea->area.cdPage = zh_vmCDP();
   }
   else
      pArea->area.cdPage = zh_vmCDP();

   uiFlags = ( pArea->fReadonly ? FO_READ : FO_READWRITE ) |
             ( pArea->fShared ? FO_DENYNONE : FO_EXCLUSIVE );

   pFileName = zh_fsFNameSplit( pOpenInfo->abName );
   /* Add default file name extension if necessary */
   if( zh_setGetDefExtension() && ! pFileName->szExtension )
   {
      PZH_ITEM pFileExt = zh_itemNew( NULL );
      if( SELF_INFO( &pArea->area, DBI_TABLEEXT, pFileExt ) == ZH_SUCCESS )
      {
         pFileName->szExtension = zh_itemGetCPtr( pFileExt );
         zh_fsFNameMerge( szFileName, pFileName );
      }
      zh_itemRelease( pFileExt );
   }
   else
   {
      zh_strncpy( szFileName, pOpenInfo->abName, sizeof( szFileName ) - 1 );
   }

   /* Create default alias if necessary */
   if( ! pOpenInfo->atomAlias && pFileName->szName )
   {
      const char * szName = strrchr( pFileName->szName, ':' );
      if( szName == NULL )
         szName = pFileName->szName;
      else
         ++szName;
      zh_strncpyUpperTrim( szAlias, szName, sizeof( szAlias ) - 1 );
      pOpenInfo->atomAlias = szAlias;
   }
   zh_xfree( pFileName );

   /* Try open */
   do
   {
      pArea->pFile = zh_fileExtOpen( szFileName, NULL, uiFlags |
                                     FXO_DEFAULTS | FXO_SHARELOCK |
                                     FXO_COPYNAME, NULL, pError );
      if( ! pArea->pFile )
      {
         if( ! pError )
         {
            pError = zh_errNew();
            zh_errPutGenCode( pError, EG_OPEN );
            zh_errPutSubCode( pError, EDBF_OPEN_DBF );
            zh_errPutOsCode( pError, zh_fsError() );
            zh_errPutDescription( pError, zh_langDGetErrorDesc( EG_OPEN ) );
            zh_errPutFileName( pError, szFileName );
            zh_errPutFlags( pError, EF_CANRETRY | EF_CANDEFAULT );
         }
         fRetry = ( SELF_ERROR( &pArea->area, pError ) == E_RETRY );
      }
      else
         fRetry = ZH_FALSE;
   }
   while( fRetry );

   if( pError )
      zh_itemRelease( pError );

   if( ! pArea->pFile )
      return ZH_FAILURE;

   errCode = SUPER_OPEN( &pArea->area, pOpenInfo );
   if( errCode != ZH_SUCCESS )
   {
      SELF_CLOSE( &pArea->area );
      return ZH_FAILURE;
   }

   zh_sdfInitArea( pArea, szFileName );

   /* Position cursor at the first record */
   return SELF_GOTOP( &pArea->area );
}

/*
 * Retrieve information about the current driver.
 */
static ZH_ERRCODE zh_sdfRddInfo( LPRDDNODE pRDD, ZH_USHORT uiIndex, ZH_ULONG ulConnect, PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_sdfRddInfo(%p,%hu,%lu,%p)", ( void * ) pRDD, uiIndex, ulConnect, ( void * ) pItem ) );

   switch( uiIndex )
   {
      case RDDI_CANPUTREC:
      case RDDI_LOCAL:
         zh_itemPutL( pItem, ZH_TRUE );
         break;

      case RDDI_TABLEEXT:
         zh_itemPutC( pItem, SDF_TABLEEXT );
         break;

      default:
         return SUPER_RDDINFO( pRDD, uiIndex, ulConnect, pItem );

   }

   return ZH_SUCCESS;
}


static const RDDFUNCS sdfTable =
{
   NULL /* zh_sdfBof */,
   NULL /* zh_sdfEof */,
   NULL /* zh_sdfFound */,
   NULL /* zh_sdfGoBottom */,
   ( DBENTRYP_UL ) zh_sdfGoTo,
   ( DBENTRYP_I ) zh_sdfGoToId,
   ( DBENTRYP_V ) zh_sdfGoTop,
   NULL /* zh_sdfSeek */,
   NULL /* zh_sdfSkip */,
   NULL /* zh_sdfSkipFilter */,
   ( DBENTRYP_L ) zh_sdfSkipRaw,
   ( DBENTRYP_VF ) zh_sdfAddField,
   ( DBENTRYP_B ) zh_sdfAppend,
   NULL /* zh_sdfCreateFields */,
   ( DBENTRYP_V ) zh_sdfDeleteRec,
   ( DBENTRYP_BP ) zh_sdfDeleted,
   NULL /* zh_sdfFieldCount */,
   NULL /* zh_sdfFieldDisplay */,
   NULL /* zh_sdfFieldInfo */,
   NULL /* zh_sdfFieldName */,
   ( DBENTRYP_V ) zh_sdfFlush,
   ( DBENTRYP_PP ) zh_sdfGetRec,
   ( DBENTRYP_SI ) zh_sdfGetValue,
   NULL /* zh_sdfGetVarLen */,
   ( DBENTRYP_V ) zh_sdfGoCold,
   ( DBENTRYP_V ) zh_sdfGoHot,
   ( DBENTRYP_P ) zh_sdfPutRec,
   ( DBENTRYP_SI ) zh_sdfPutValue,
   ( DBENTRYP_V ) zh_sdfRecall,
   ( DBENTRYP_ULP ) zh_sdfRecCount,
   NULL /* zh_sdfRecInfo */,
   ( DBENTRYP_ULP ) zh_sdfRecNo,
   ( DBENTRYP_I ) zh_sdfRecId,
   ( DBENTRYP_S ) zh_sdfSetFieldExtent,
   NULL /* zh_sdfAlias */,
   ( DBENTRYP_V ) zh_sdfClose,
   ( DBENTRYP_VO ) zh_sdfCreate,
   ( DBENTRYP_SI ) zh_sdfInfo,
   ( DBENTRYP_V ) zh_sdfNewArea,
   ( DBENTRYP_VO ) zh_sdfOpen,
   NULL /* zh_sdfRelease */,
   ( DBENTRYP_SP ) zh_sdfStructSize,
   NULL /* zh_sdfSysName */,
   NULL /* zh_sdfEval */,
   NULL /* zh_sdfPack */,
   NULL /* zh_sdfPackRec */,
   NULL /* zh_sdfSort */,
   ( DBENTRYP_VT ) zh_sdfTrans,
   NULL /* zh_sdfTransRec */,
   NULL /* zh_sdfZap */,
   NULL /* zh_sdfChildEnd */,
   NULL /* zh_sdfChildStart */,
   NULL /* zh_sdfChildSync */,
   NULL /* zh_sdfSyncChildren */,
   NULL /* zh_sdfClearRel */,
   NULL /* zh_sdfForceRel */,
   NULL /* zh_sdfRelArea */,
   NULL /* zh_sdfRelEval */,
   NULL /* zh_sdfRelText */,
   NULL /* zh_sdfSetRel */,
   NULL /* zh_sdfOrderListAdd */,
   NULL /* zh_sdfOrderListClear */,
   NULL /* zh_sdfOrderListDelete */,
   NULL /* zh_sdfOrderListFocus */,
   NULL /* zh_sdfOrderListRebuild */,
   NULL /* zh_sdfOrderCondition */,
   NULL /* zh_sdfOrderCreate */,
   NULL /* zh_sdfOrderDestroy */,
   NULL /* zh_sdfOrderInfo */,
   NULL /* zh_sdfClearFilter */,
   NULL /* zh_sdfClearLocate */,
   NULL /* zh_sdfClearScope */,
   NULL /* zh_sdfCountScope */,
   NULL /* zh_sdfFilterText */,
   NULL /* zh_sdfScopeInfo */,
   NULL /* zh_sdfSetFilter */,
   NULL /* zh_sdfSetLocate */,
   NULL /* zh_sdfSetScope */,
   NULL /* zh_sdfSkipScope */,
   NULL /* zh_sdfLocate */,
   NULL /* zh_sdfCompile */,
   NULL /* zh_sdfError */,
   NULL /* zh_sdfEvalBlock */,
   NULL /* zh_sdfRawLock */,
   NULL /* zh_sdfLock */,
   NULL /* zh_sdfUnLock */,
   NULL /* zh_sdfCloseMemFile */,
   NULL /* zh_sdfCreateMemFile */,
   NULL /* zh_sdfGetValueFile */,
   NULL /* zh_sdfOpenMemFile */,
   NULL /* zh_sdfPutValueFile */,
   NULL /* zh_sdfReadDBHeader */,
   NULL /* zh_sdfWriteDBHeader */,
   NULL /* zh_sdfInit */,
   NULL /* zh_sdfExit */,
   NULL /* zh_sdfDrop */,
   NULL /* zh_sdfExists */,
   NULL /* zh_sdfRename */,
   ( DBENTRYP_RSLV ) zh_sdfRddInfo,
   NULL /* zh_sdfWhoCares */
};

ZH_FUNC( SDF ) { ; }

ZH_FUNC_STATIC( SDF_GETFUNCTABLE )
{
   RDDFUNCS * pTable;
   ZH_USHORT * puiCount;

   puiCount = ( ZH_USHORT * ) zh_parptr( 1 );
   pTable = ( RDDFUNCS * ) zh_parptr( 2 );

   ZH_TRACE( ZH_TR_DEBUG, ( "SDF_GETFUNCTABLE(%p, %p)", ( void * ) puiCount, ( void * ) pTable ) );

   if( pTable )
   {
      if( puiCount )
         *puiCount = RDDFUNCSCOUNT;
      zh_retni( zh_rddInheritEx( pTable, &sdfTable, &sdfSuper, NULL, NULL ) );
   }
   else
      zh_retni( ZH_FAILURE );
}

static void zh_sdfRddInit( void * cargo )
{
   ZH_SYMBOL_UNUSED( cargo );

   if( zh_rddRegister( "SDF", RDD_REGISTER_TYPE_TRANSFER ) > 1 )
      zh_errInternal( ZH_EI_RDDINVALID, NULL, NULL, NULL );
}

ZH_INIT_SYMBOLS_BEGIN( sdf1__InitSymbols )
{ "SDF",              {ZH_FS_PUBLIC|ZH_FS_LOCAL}, {ZH_FUNCNAME( SDF )}, NULL },
{ "SDF_GETFUNCTABLE", {ZH_FS_PUBLIC|ZH_FS_LOCAL}, {ZH_FUNCNAME( SDF_GETFUNCTABLE )}, NULL }
ZH_INIT_SYMBOLS_END( sdf1__InitSymbols )

ZH_CALL_ON_STARTUP_BEGIN( _zh_sdf_rdd_init_ )
   zh_vmAtInit( zh_sdfRddInit, NULL );
ZH_CALL_ON_STARTUP_END( _zh_sdf_rdd_init_ )

#if defined( ZH_PRAGMA_STARTUP )
   #pragma startup sdf1__InitSymbols
   #pragma startup _zh_sdf_rdd_init_
#elif defined( ZH_DATASEG_STARTUP )
   #define ZH_DATASEG_BODY    ZH_DATASEG_FUNC( sdf1__InitSymbols ) \
                              ZH_DATASEG_FUNC( _zh_sdf_rdd_init_ )
   #include "..\zh_ini_seg.h"
#endif
