/*
 * DELIM RDD
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
#include "zh_stack.h"
#include "zh_date.h"
#include "zh_rdd_api.h"
#include "zh_item_api.h"
#include "zh_lang_api.h"
#include "zh_error_api.h"
#include "rdd_delim.h"
#include "dbf_cdx/zh_dbf_error.h"
#include "zh_rtl/rdd_sys.zhh"

#define SUPERTABLE  ( &delimSuper )

static RDDFUNCS        delimSuper;
static const ZH_USHORT s_uiNumLength[ 9 ] = { 0, 4, 6, 8, 11, 13, 16, 18, 20 };

static void zh_delimInitArea( DELIMAREAP pArea, char * szFileName )
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
   pArea->pRecord = ( ZH_BYTE * ) zh_xgrab( pArea->uiRecordLen + 1 );
   /* pseudo deleted flag */
   *pArea->pRecord++ = ' ';

   /* allocate IO buffer */
   pArea->nBufferSize += pArea->fAnyEol ? 2 : pArea->uiEolLen;
   if( pArea->fReadonly && pArea->nBufferSize < 8192 )
      pArea->nBufferSize = 8192;
   pArea->pBuffer = ( ZH_BYTE * ) zh_xgrab( pArea->nBufferSize );

   pArea->ulRecCount = 0;
   pArea->nBufferIndex = pArea->nBufferRead = pArea->nBufferSize;
   pArea->nBufferAtRead = pArea->nBufferSize - ZH_MAX( pArea->uiEolLen, 2 );
}

static ZH_ERRCODE zh_delimWrite( DELIMAREAP pArea, const void * pBuffer, ZH_SIZE nSize )
{
   if( zh_fileWrite( pArea->pFile, pBuffer, nSize, -1 ) != nSize )
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
   return ZH_SUCCESS;
}

static ZH_ERRCODE zh_delimWriteHeader( DELIMAREAP pArea )
{
   ZH_ERRCODE errCode = ZH_SUCCESS;
   const char * pszFieldName;
   ZH_BYTE * pBuffer;
   ZH_SIZE nSize, nS;
   ZH_USHORT uiCount;

   nSize = 0;
   pBuffer = pArea->pBuffer;

   for( uiCount = 0; uiCount < pArea->area.uiFieldCount; uiCount++ )
   {
      pszFieldName = zh_dynsymName( ( PZH_DYNSYMBOL )
                                     ( pArea->area.lpFields + uiCount )->sym );
      nSize += strlen( pszFieldName ) + 3;
   }
   if( nSize > 0 )
   {
      nSize += pArea->uiEolLen - 1;
      if( nSize > pArea->nBufferSize )
         pBuffer = ( ZH_BYTE * ) zh_xgrab( nSize );

      nSize = 0;
      for( uiCount = 0; uiCount < pArea->area.uiFieldCount; uiCount++ )
      {
         pszFieldName = zh_dynsymName( ( PZH_DYNSYMBOL )
                                     ( pArea->area.lpFields + uiCount )->sym );
         nS = strlen( pszFieldName );
         if( uiCount )
            pBuffer[ nSize++ ] = pArea->cSeparator;
         pBuffer[ nSize++ ] = pArea->cDelim;
         memcpy( pBuffer + nSize, pszFieldName, nS );
         nSize += nS;
         pBuffer[ nSize++ ] = pArea->cDelim;
      }
      memcpy( pBuffer + nSize, pArea->szEol, pArea->uiEolLen );
      nSize += pArea->uiEolLen;
      errCode = zh_delimWrite( pArea, pBuffer, nSize );
      if( pBuffer != pArea->pBuffer )
         zh_xfree( pBuffer );
   }
   return errCode;
}

static void zh_delimClearRecordBuffer( DELIMAREAP pArea )
{
   memset( pArea->pRecord, ' ', pArea->uiRecordLen );
}

static ZH_SIZE zh_delimEncodeBuffer( DELIMAREAP pArea )
{
   ZH_SIZE nSize;
   ZH_USHORT uiField, uiLen;
   LPFIELD pField;
   ZH_BYTE * pBuffer;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_delimEncodeBuffer(%p)", ( void * ) pArea ) );

   /* mark the read buffer as empty */
   pArea->nBufferRead = pArea->nBufferIndex = 0;

   pBuffer = pArea->pBuffer;
   nSize = 0;
   for( uiField = 0; uiField < pArea->area.uiFieldCount; ++uiField )
   {
      ZH_BYTE * pFieldBuf;
      pField = pArea->area.lpFields + uiField;
      pFieldBuf = pArea->pRecord + pArea->pFieldOffset[ uiField ];
      if( nSize )
         pBuffer[ nSize++ ] = pArea->cSeparator;

      switch( pField->uiType )
      {
         case ZH_FT_STRING:
         case ZH_FT_TIMESTAMP:
            uiLen = pField->uiLen;
            while( uiLen && pFieldBuf[ uiLen - 1 ] == ' ' )
               --uiLen;
            if( pArea->cDelim )
            {
               pBuffer[ nSize++ ] = pArea->cDelim;
               memcpy( pBuffer + nSize, pFieldBuf, uiLen );
               nSize += uiLen;
               pBuffer[ nSize++ ] = pArea->cDelim;
            }
            else
            {
               memcpy( pBuffer + nSize, pFieldBuf, uiLen );
               nSize += uiLen;
            }
            break;

         case ZH_FT_LOGICAL:
            pBuffer[ nSize++ ] = ( *pFieldBuf == 'T' || *pFieldBuf == 't' ||
                                   *pFieldBuf == 'Y' || *pFieldBuf == 'y' ) ?
                                  'T' : 'F';
            break;

         case ZH_FT_DATE:
            uiLen = 0;
            while( uiLen < 8 && pFieldBuf[ uiLen ] == ' ' )
               ++uiLen;
            if( uiLen < 8 )
            {
               memcpy( pBuffer + nSize, pFieldBuf, 8 );
               nSize += 8;
            }
            break;

         case ZH_FT_LONG:
            uiLen = 0;
            while( uiLen < pField->uiLen && pFieldBuf[ uiLen ] == ' ' )
               ++uiLen;
            if( uiLen < pField->uiLen )
            {
               memcpy( pBuffer + nSize, pFieldBuf + uiLen, pField->uiLen - uiLen );
               nSize += pField->uiLen - uiLen;
            }
            else
            {
               pBuffer[ nSize++ ] = '0';
               if( pField->uiDec )
               {
                  pBuffer[ nSize++ ] = '.';
                  memset( pBuffer + nSize, '0', pField->uiDec );
                  nSize += pField->uiDec;
               }
            }
            break;

         case ZH_FT_MEMO:
         default:
            if( nSize )
               --nSize;
            break;
      }
   }
   memcpy( pBuffer + nSize, pArea->szEol, pArea->uiEolLen );
   nSize += pArea->uiEolLen;

   return nSize;
}

static int zh_delimNextChar( DELIMAREAP pArea )
{
   for( ;; )
   {
      unsigned char ch;

      if( pArea->nBufferIndex >= pArea->nBufferAtRead &&
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
         return -2;

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
            return -1;
         }
      }
      else if( ch == pArea->szEol[ 0 ] )
      {
         if( pArea->uiEolLen == 1 )
            return -1;
         else if( pArea->nBufferRead - pArea->nBufferIndex >=
                  ( ZH_SIZE ) pArea->uiEolLen - 1 &&
                  memcmp( pArea->pBuffer + pArea->nBufferIndex,
                          pArea->szEol + 1, pArea->uiEolLen - 1 ) == 0 )
         {
            pArea->nBufferIndex += pArea->uiEolLen - 1;
            return -1;
         }
      }
      if( ch != '\032' )
         return ch;

   }
}

/*
 * Read record, decode it to buffer and set next record offset
 */
static ZH_ERRCODE zh_delimReadRecord( DELIMAREAP pArea )
{

   ZH_USHORT uiField;
   int ch = 0;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_delimReadRecord(%p)", ( void * ) pArea ) );

   pArea->area.fEof = ZH_TRUE;

   /* clear the record buffer */
   zh_delimClearRecordBuffer( pArea );

   for( uiField = 0; uiField < pArea->area.uiFieldCount; ++uiField )
   {
      LPFIELD pField = pArea->area.lpFields + uiField;
      ZH_USHORT uiType = pField->uiType;

      if( uiType == ZH_FT_STRING || uiType == ZH_FT_LOGICAL ||
          uiType == ZH_FT_DATE || uiType == ZH_FT_TIMESTAMP ||
          uiType == ZH_FT_LONG )
      {
         ZH_USHORT uiLen = pField->uiLen, uiSize = 0;
         ZH_BYTE * pFieldBuf = pArea->pRecord + pArea->pFieldOffset[ uiField ],
                 buffer[ 256 ];
         char cStop;

         ch = zh_delimNextChar( pArea );
         if( ch != -2 )
            pArea->area.fEof = ZH_FALSE;

         /* ignore leading spaces */
         while( ch == ' ' )
            ch = zh_delimNextChar( pArea );

         /* set the stop character */
         if( pArea->cDelim && ch == pArea->cDelim )
         {
            cStop = pArea->cDelim;
            ch = zh_delimNextChar( pArea );
         }
         else
            cStop = pArea->cSeparator;

         if( pField->uiType == ZH_FT_STRING ||
             ( pField->uiType == ZH_FT_TIMESTAMP && cStop == pArea->cDelim ) )
         {
            while( ch >= 0 && ch != cStop )
            {
               if( uiSize < uiLen )
                  pFieldBuf[ uiSize++ ] = ( ZH_BYTE ) ch;
               ch = zh_delimNextChar( pArea );
            }
         }
         else
         {
            while( ch >= 0 && ch != cStop && uiSize < uiLen )
            {
               if( uiSize < sizeof( buffer ) - 1 )
                  buffer[ uiSize++ ] = ( ZH_BYTE ) ch;
               ch = zh_delimNextChar( pArea );
            }
            buffer[ uiSize ] = '\0';

            if( pField->uiType == ZH_FT_LOGICAL )
            {
               *pFieldBuf = ( *buffer == 'T' || *buffer == 't' ||
                              *buffer == 'Y' || *buffer == 'y' ) ? 'T' : 'F';
            }
            else if( pField->uiType == ZH_FT_DATE )
            {
               if( uiSize == 8 && zh_dateEncStr( ( char * ) buffer ) != 0 )
                  memcpy( pFieldBuf, buffer, 8 );
            }
            else if( pField->uiType == ZH_FT_TIMESTAMP )
            {
               memcpy( pFieldBuf, buffer, uiSize );
               if( uiSize < uiLen )
                  memset( pFieldBuf + uiSize, 0, uiLen - uiSize );
            }
            else
            {
               ZH_MAXINT lVal;
               double dVal;
               ZH_BOOL fDbl;

               fDbl = zh_strnToNum( (const char *) buffer, uiSize, &lVal, &dVal );
               if( fDbl )
                  pArea->area.valResult = zh_itemPutNDLen( pArea->area.valResult, dVal,
                                    uiLen - pField->uiDec - 1, pField->uiDec );
               else
                  pArea->area.valResult = zh_itemPutNIntLen( pArea->area.valResult,
                                                             lVal, uiLen );
               zh_itemStrBuf( ( char * ) buffer, pArea->area.valResult, uiLen,
                              pField->uiDec );
               /* TODO: RT error on width range */
               memcpy( pFieldBuf, buffer, uiLen );
            }
         }

         /* ignore all character to the next field separator */
         while( ch >= 0 && ch != pArea->cSeparator )
            ch = zh_delimNextChar( pArea );

         /* stop reading on EOL */
         if( ch < 0 )
            break;
      }

   }
   /* ignore all character to the end of line */
   while( ch >= 0 )
      ch = zh_delimNextChar( pArea );

   pArea->fPositioned = ! pArea->area.fEof;

   return ZH_SUCCESS;
}

static ZH_ERRCODE zh_delimNextRecord( DELIMAREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_delimNextRecord(%p)", ( void * ) pArea ) );

   if( pArea->fPositioned )
   {
      pArea->ulRecNo++;
      return zh_delimReadRecord( pArea );
   }
   return ZH_SUCCESS;
}

/*
 * -- DELIM METHODS --
 */

/*
 * Position cursor at a specific physical record.
 */
static ZH_ERRCODE zh_delimGoTo( DELIMAREAP pArea, ZH_ULONG ulRecNo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_delimGoTo(%p, %lu)", ( void * ) pArea, ulRecNo ) );

   /* generate RTE */
   return SUPER_GOTO( &pArea->area, ulRecNo );
}

/*
 * Position the cursor to a specific, physical identity.
 */
static ZH_ERRCODE zh_delimGoToId( DELIMAREAP pArea, PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_delimGoToId(%p, %p)", ( void * ) pArea, ( void * ) pItem ) );

   /* generate RTE */
   return SUPER_GOTOID( &pArea->area, pItem );
}

/*
 * Position cursor at the first record.
 */
static ZH_ERRCODE zh_delimGoTop( DELIMAREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_delimGoTop(%p)", ( void * ) pArea ) );

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
      if( zh_delimReadRecord( pArea ) != ZH_SUCCESS )
         return ZH_FAILURE;
   }

   return SELF_SKIPFILTER( &pArea->area, 1 );
}

/*
 * Reposition cursor, regardless of filter.
 */
static ZH_ERRCODE zh_delimSkipRaw( DELIMAREAP pArea, ZH_LONG lToSkip )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_delimSkipRaw(%p,%ld)", ( void * ) pArea, lToSkip ) );

   if( SELF_GOCOLD( &pArea->area ) != ZH_SUCCESS )
      return ZH_FAILURE;

   if( lToSkip != 1 || ! pArea->fReadonly )
      /* generate RTE */
      return SUPER_SKIPRAW( &pArea->area, lToSkip );
   else
      return zh_delimNextRecord( pArea );
}

/*
 * Determine deleted status for a record.
 */
static ZH_ERRCODE zh_delimDeleted( DELIMAREAP pArea, ZH_BOOL * pDeleted )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_delimDeleted(%p,%p)", ( void * ) pArea, ( void * ) pDeleted ) );

   ZH_SYMBOL_UNUSED( pArea );

   *pDeleted = ZH_FALSE;

   return ZH_SUCCESS;
}

/*
 * Obtain number of records in WorkArea.
 */
static ZH_ERRCODE zh_delimRecCount( DELIMAREAP pArea, ZH_ULONG * pRecCount )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_delimRecCount(%p,%p)", ( void * ) pArea, ( void * ) pRecCount ) );

   *pRecCount = pArea->ulRecCount;

   return ZH_SUCCESS;
}

/*
 * Obtain physical row number at current WorkArea cursor position.
 */
static ZH_ERRCODE zh_delimRecNo( DELIMAREAP pArea, ZH_ULONG * pulRecNo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_delimRecNo(%p,%p)", ( void * ) pArea, ( void * ) pulRecNo ) );

   *pulRecNo = pArea->ulRecNo;

   return ZH_SUCCESS;
}

/*
 * Obtain physical row ID at current WorkArea cursor position.
 */
static ZH_ERRCODE zh_delimRecId( DELIMAREAP pArea, PZH_ITEM pRecNo )
{
   ZH_ERRCODE errCode;
   ZH_ULONG ulRecNo;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_delimRecId(%p,%p)", ( void * ) pArea, ( void * ) pRecNo ) );

   errCode = SELF_RECNO( &pArea->area, &ulRecNo );
   zh_itemPutNInt( pRecNo, ulRecNo );

   return errCode;
}

/*
 * Append a record to the WorkArea.
 */
static ZH_ERRCODE zh_delimAppend( DELIMAREAP pArea, ZH_BOOL fUnLockAll )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_delimAppend(%p,%d)", ( void * ) pArea, ( int ) fUnLockAll ) );

   ZH_SYMBOL_UNUSED( fUnLockAll );

   if( SELF_GOCOLD( &pArea->area ) != ZH_SUCCESS )
      return ZH_FAILURE;

   if( SELF_GOHOT( &pArea->area ) != ZH_SUCCESS )
      return ZH_FAILURE;

   pArea->ulRecNo = ++pArea->ulRecCount;
   pArea->area.fEof = ZH_FALSE;
   pArea->fPositioned = ZH_TRUE;
   zh_delimClearRecordBuffer( pArea );

   return ZH_SUCCESS;
}

/*
 * Delete a record.
 */
static ZH_ERRCODE zh_delimDeleteRec( DELIMAREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_delimDeleteRec(%p)", ( void * ) pArea ) );

   ZH_SYMBOL_UNUSED( pArea );

   /* It's not Cl*pper compatible so I had to disable it [druzus] */
#if 0
   if( pArea->fRecordChanged )
   {
      pArea->ulRecCount--;
      pArea->area.fEof = ZH_TRUE;
      pArea->fPositioned = pArea->fRecordChanged = ZH_FALSE;
      zh_delimClearRecordBuffer( pArea );
   }
#endif

   return ZH_SUCCESS;
}

/*
 * Undelete the current record.
 */
static ZH_ERRCODE zh_delimRecall( DELIMAREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_delimRecall(%p)", ( void * ) pArea ) );

   ZH_SYMBOL_UNUSED( pArea );

   return ZH_SUCCESS;
}

/*
 * Obtain the current value of a field.
 */
static ZH_ERRCODE zh_delimGetValue( DELIMAREAP pArea, ZH_USHORT uiIndex, PZH_ITEM pItem )
{
   LPFIELD pField;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_delimGetValue(%p, %hu, %p)", ( void * ) pArea, uiIndex, ( void * ) pItem ) );

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
                                        &nLen, pArea->area.cdPage, zh_vmCodepage() );
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
         zh_errPutOperation( pError, zh_dynsymName( ( PZH_DYNSYMBOL ) pField->sym ) );
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
static ZH_ERRCODE zh_delimPutValue( DELIMAREAP pArea, ZH_USHORT uiIndex, PZH_ITEM pItem )
{
   char szBuffer[ 256 ];
   ZH_ERRCODE errCode;
   LPFIELD pField;
   ZH_SIZE nSize;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_delimPutValue(%p,%hu,%p)", ( void * ) pArea, uiIndex, ( void * ) pItem ) );

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
      if( ZH_IS_MEMO( pItem ) || ZH_IS_STRING( pItem ) )
      {
         if( pField->uiType == ZH_FT_STRING )
         {
            if( ( pField->uiFlags & ZH_FF_BINARY ) == 0 )
            {
               nSize = pField->uiLen;
               zh_cdpnDup2( zh_itemGetCPtr( pItem ), zh_itemGetCLen( pItem ),
                            ( char * ) pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                            &nSize, zh_vmCodepage(), pArea->area.cdPage );
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
      zh_errPutOperation( pError, zh_dynsymName( ( PZH_DYNSYMBOL ) pField->sym ) );
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
static ZH_ERRCODE zh_delimPutRec( DELIMAREAP pArea, ZH_BYTE * pBuffer )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_delimPutRec(%p,%p)", ( void * ) pArea, ( void * ) pBuffer ) );

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
static ZH_ERRCODE zh_delimGetRec( DELIMAREAP pArea, ZH_BYTE ** pBufferPtr )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_delimGetRec(%p,%p)", ( void * ) pArea, ( void * ) pBufferPtr ) );

   *pBufferPtr = pArea->pRecord - 1;

   return ZH_SUCCESS;
}

/*
 * Copy one or more records from one WorkArea to another.
 */
static ZH_ERRCODE zh_delimTrans( DELIMAREAP pArea, LPDBTRANSINFO pTransInfo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_delimTrans(%p, %p)", ( void * ) pArea, ( void * ) pTransInfo ) );

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
static ZH_ERRCODE zh_delimGoCold( DELIMAREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_delimGoCold(%p)", ( void * ) pArea ) );

   if( pArea->fRecordChanged )
   {
      ZH_SIZE nSize = zh_delimEncodeBuffer( pArea );

      if( zh_delimWrite( pArea, pArea->pBuffer, nSize ) != ZH_SUCCESS )
         return ZH_FAILURE;
      pArea->fRecordChanged = ZH_FALSE;
      pArea->fFlush = ZH_TRUE;
   }
   return ZH_SUCCESS;
}

/*
 * Mark the WorkArea data buffer as hot.
 */
static ZH_ERRCODE zh_delimGoHot( DELIMAREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_delimGoHot(%p)", ( void * ) pArea ) );

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
static ZH_ERRCODE zh_delimFlush( DELIMAREAP pArea )
{
   ZH_ERRCODE errCode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_delimFlush(%p)", ( void * ) pArea ) );

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
static ZH_ERRCODE zh_delimInfo( DELIMAREAP pArea, ZH_USHORT uiIndex, PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_delimInfo(%p,%hu,%p)", ( void * ) pArea, uiIndex, ( void * ) pItem ) );

   switch( uiIndex )
   {
      case DBI_CANPUTREC:
         zh_itemPutL( pItem, pArea->fTransRec );
         break;

      case DBI_GETRECSIZE:
         zh_itemPutNL( pItem, pArea->uiRecordLen );
         break;

      case DBI_GETDELIMITER:
      {
         char szDelim[ 2 ];
         szDelim[ 0 ] = pArea->cDelim;
         szDelim[ 1 ] = '\0';
         zh_itemPutC( pItem, szDelim );
         break;
      }
      case DBI_SETDELIMITER:
         if( zh_itemType( pItem ) & ZH_IT_STRING )
         {
            const char * szDelim = zh_itemGetCPtr( pItem );

            if( zh_stricmp( szDelim, "BLANK" ) == 0 )
            {
               pArea->cDelim = '\0';
               pArea->cSeparator = ' ';
            }
            else if( zh_stricmp( szDelim, "PIPE" ) == 0 )
            {
               pArea->cDelim = '\0';
               pArea->cSeparator = '|';
            }
            else if( zh_stricmp( szDelim, "TAB" ) == 0 )
            {
               pArea->cDelim = '\0';
               pArea->cSeparator = '\t';
            }
            else
            {
               pArea->cDelim = *szDelim;
            }
         }
         /*
          * a small trick which allow to set character field delimiter and
          * field separator in COPY TO ... and APPEND FROM ... commands as
          * array. e.g.:
          *    COPY TO test DELIMITED WITH ({"", "|"})
          */
         else if( zh_itemType( pItem ) & ZH_IT_ARRAY )
         {
            char cSeparator;

            if( zh_arrayGetType( pItem, 1 ) & ZH_IT_STRING )
               pArea->cDelim = *zh_arrayGetCPtr( pItem, 1 );

            cSeparator = *zh_arrayGetCPtr( pItem, 2 );
            if( cSeparator )
               pArea->cSeparator = cSeparator;
         }
         break;

      case DBI_SEPARATOR:
      {
         char szSeparator[ 2 ];
         const char * szNew = zh_itemGetCPtr( pItem );
         szSeparator[ 0 ] = pArea->cSeparator;
         szSeparator[ 1 ]  = '\0';
         if( *szNew )
            pArea->cSeparator = *szNew;
         zh_itemPutC( pItem, szSeparator );
         break;
      }
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
            zh_snprintf( szBuf, sizeof( szBuf ), "%d.%d (%s)", 0, 1, "DELIM" );
         else if( iSub == 2 )
            zh_snprintf( szBuf, sizeof( szBuf ), "%d.%d (%s:%d)", 0, 1, "DELIM", pArea->area.rddID );
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
static ZH_ERRCODE zh_delimAddField( DELIMAREAP pArea, LPDBFIELDINFO pFieldInfo )
{
   ZH_USHORT uiDelim = 0;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_delimAddField(%p, %p)", ( void * ) pArea, ( void * ) pFieldInfo ) );

   switch( pFieldInfo->uiType )
   {
      case ZH_FT_STRING:
         uiDelim = 2;
         break;

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
         uiDelim = 2;
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
         uiDelim = 2;
         break;

      case ZH_FT_TIMESTAMP:
      case ZH_FT_MODTIME:
         pFieldInfo->uiType = ZH_FT_TIMESTAMP;
         pFieldInfo->uiLen = 23;
         pArea->fTransRec = ZH_FALSE;
         uiDelim = 2;
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
   pArea->nBufferSize += pFieldInfo->uiLen + uiDelim + 1;

   return SUPER_ADDFIELD( &pArea->area, pFieldInfo );
}

/*
 * Establish the extent of the array of fields for a WorkArea.
 */
static ZH_ERRCODE zh_delimSetFieldExtent( DELIMAREAP pArea, ZH_USHORT uiFieldExtent )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_delimSetFieldExtent(%p,%hu)", ( void * ) pArea, uiFieldExtent ) );

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
static ZH_ERRCODE zh_delimNewArea( DELIMAREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_delimNewArea(%p)", ( void * ) pArea ) );

   if( SUPER_NEW( &pArea->area ) == ZH_FAILURE )
      return ZH_FAILURE;

   pArea->pFile = NULL;
   pArea->fTransRec = ZH_TRUE;
   pArea->uiRecordLen = 0;
   pArea->nBufferSize = 0;

   /* set character field delimiter */
   pArea->cDelim = '"';

   /* set field separator */
   pArea->cSeparator = ',';

   return ZH_SUCCESS;
}

/*
 * Retrieve the size of the WorkArea structure.
 */
static ZH_ERRCODE zh_delimStructSize( DELIMAREAP pArea, ZH_USHORT * uiSize )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_delimStrucSize(%p,%p)", ( void * ) pArea, ( void * ) uiSize ) );
   ZH_SYMBOL_UNUSED( pArea );

   *uiSize = sizeof( DELIMAREA );
   return ZH_SUCCESS;
}

/*
 * Close the table in the WorkArea.
 */
static ZH_ERRCODE zh_delimClose( DELIMAREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_delimClose(%p)", ( void * ) pArea ) );

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
static ZH_ERRCODE zh_delimCreate( DELIMAREAP pArea, LPDBOPENINFO pCreateInfo )
{
   PZH_ITEM pError = NULL;
   ZH_ERRCODE errCode;
   ZH_BOOL fRetry;
   PZH_FNAME pFileName;
   char szFileName[ ZH_PATH_MAX ];

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_delimCreate(%p,%p)", ( void * ) pArea, ( void * ) pCreateInfo ) );

   pArea->fShared = ZH_FALSE;    /* pCreateInfo->fShared; */
   pArea->fReadonly = ZH_FALSE;  /* pCreateInfo->fReadonly */

   if( pCreateInfo->cdpId )
   {
      pArea->area.cdPage = zh_cdpFindExt( pCreateInfo->cdpId );
      if( ! pArea->area.cdPage )
         pArea->area.cdPage = zh_vmCodepage();
   }
   else
      pArea->area.cdPage = zh_vmCodepage();

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
   if( errCode == ZH_SUCCESS )
   {
      PZH_ITEM pItem = zh_itemNew( NULL );

      zh_delimInitArea( pArea, szFileName );

      pArea->ulRecNo = 1;
      pArea->area.fEof = ZH_TRUE;
      pArea->fPositioned = ZH_FALSE;
      zh_delimClearRecordBuffer( pArea );

      if( SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_SETHEADER,
                        pCreateInfo->ulConnection, pItem ) == ZH_SUCCESS &&
          zh_itemGetNI( pItem ) > 0 )
         errCode = zh_delimWriteHeader( pArea );

      zh_itemRelease( pItem );
   }

   if( errCode != ZH_SUCCESS )
      SELF_CLOSE( &pArea->area );

   return errCode;
}

/*
 * Open a data store in the WorkArea.
 */
static ZH_ERRCODE zh_delimOpen( DELIMAREAP pArea, LPDBOPENINFO pOpenInfo )
{
   PZH_ITEM pError = NULL;
   PZH_FNAME pFileName;
   ZH_ERRCODE errCode;
   ZH_USHORT uiFlags;
   ZH_BOOL fRetry;
   char szFileName[ ZH_PATH_MAX ];
   char szAlias[ ZH_RDD_MAX_ALIAS_LEN + 1 ];

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_delimOpen(%p,%p)", ( void * ) pArea, ( void * ) pOpenInfo ) );

   pArea->fShared = ZH_TRUE;     /* pOpenInfo->fShared; */
   pArea->fReadonly = ZH_TRUE;   /* pOpenInfo->fReadonly; */

   if( pOpenInfo->cdpId )
   {
      pArea->area.cdPage = zh_cdpFindExt( pOpenInfo->cdpId );
      if( ! pArea->area.cdPage )
         pArea->area.cdPage = zh_vmCodepage();
   }
   else
      pArea->area.cdPage = zh_vmCodepage();

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

   zh_delimInitArea( pArea, szFileName );

   /* Position cursor at the first record */
   return SELF_GOTOP( &pArea->area );
}

/*
 * RDD init
 */
static ZH_ERRCODE zh_delimInit( LPRDDNODE pRDD )
{
   PZH_TSD pTSD;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_delimInit(%p)", ( void * ) pRDD ) );

   pTSD = ( PZH_TSD ) zh_xgrab( sizeof( ZH_TSD ) );
   ZH_TSD_INIT( pTSD, sizeof( DELIMDATA ), NULL, NULL );
   pRDD->lpvCargo = ( void * ) pTSD;

   if( ISSUPER_INIT( pRDD ) )
      return SUPER_INIT( pRDD );
   else
      return ZH_SUCCESS;
}

/*
 * RDD exit
 */
static ZH_ERRCODE zh_delimExit( LPRDDNODE pRDD )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_delimExit(%p)", ( void * ) pRDD ) );

   if( pRDD->lpvCargo )
   {
      zh_stackReleaseTSD( ( PZH_TSD ) pRDD->lpvCargo );
      zh_xfree( pRDD->lpvCargo );
      pRDD->lpvCargo = NULL;
   }

   if( ISSUPER_EXIT( pRDD ) )
      return SUPER_EXIT( pRDD );
   else
      return ZH_SUCCESS;
}

/*
 * Retrieve information about the current driver.
 */
static ZH_ERRCODE zh_delimRddInfo( LPRDDNODE pRDD, ZH_USHORT uiIndex, ZH_ULONG ulConnect, PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_delimRddInfo(%p,%hu,%lu,%p)", ( void * ) pRDD, uiIndex, ulConnect, ( void * ) pItem ) );

   switch( uiIndex )
   {
      case RDDI_CANPUTREC:
      case RDDI_LOCAL:
         zh_itemPutL( pItem, ZH_TRUE );
         break;

      case RDDI_TABLEEXT:
      {
         LPDELIMDATA pData = DELIMNODE_DATA( pRDD );
         const char * szNew = zh_itemGetCPtr( pItem );
         char * szNewVal;

         szNewVal = szNew[ 0 ] == '.' && szNew[ 1 ] ? zh_strdup( szNew ) : NULL;
         zh_itemPutC( pItem, pData->szTableExt[ 0 ] ? pData->szTableExt : DELIM_TABLEEXT );
         if( szNewVal )
         {
            zh_strncpy( pData->szTableExt, szNewVal, sizeof( pData->szTableExt ) - 1 );
            zh_xfree( szNewVal );
         }
         break;
      }
      case RDDI_SETHEADER:
      {
         LPDELIMDATA pData = DELIMNODE_DATA( pRDD );
         ZH_USHORT uiSetHeader = pData->uiSetHeader;
         if( ZH_IS_NUMERIC( pItem ) )
         {
            int iMode = zh_itemGetNI( pItem );
            if( iMode == 0 || iMode == 1 )
               pData->uiSetHeader = ( ZH_USHORT ) iMode;
         }
         zh_itemPutNI( pItem, uiSetHeader );
         break;
      }
      default:
         return SUPER_RDDINFO( pRDD, uiIndex, ulConnect, pItem );

   }

   return ZH_SUCCESS;
}


static const RDDFUNCS delimTable =
{
   NULL /* zh_delimBof */,
   NULL /* zh_delimEof */,
   NULL /* zh_delimFound */,
   NULL /* zh_delimGoBottom */,
   ( DBENTRYP_UL ) zh_delimGoTo,
   ( DBENTRYP_I ) zh_delimGoToId,
   ( DBENTRYP_V ) zh_delimGoTop,
   NULL /* zh_delimSeek */,
   NULL /* zh_delimSkip */,
   NULL /* zh_delimSkipFilter */,
   ( DBENTRYP_L ) zh_delimSkipRaw,
   ( DBENTRYP_VF ) zh_delimAddField,
   ( DBENTRYP_B ) zh_delimAppend,
   NULL /* zh_delimCreateFields */,
   ( DBENTRYP_V ) zh_delimDeleteRec,
   ( DBENTRYP_BP ) zh_delimDeleted,
   NULL /* zh_delimFieldCount */,
   NULL /* zh_delimFieldDisplay */,
   NULL /* zh_delimFieldInfo */,
   NULL /* zh_delimFieldName */,
   ( DBENTRYP_V ) zh_delimFlush,
   ( DBENTRYP_PP ) zh_delimGetRec,
   ( DBENTRYP_SI ) zh_delimGetValue,
   NULL /* zh_delimGetVarLen */,
   ( DBENTRYP_V ) zh_delimGoCold,
   ( DBENTRYP_V ) zh_delimGoHot,
   ( DBENTRYP_P ) zh_delimPutRec,
   ( DBENTRYP_SI ) zh_delimPutValue,
   ( DBENTRYP_V ) zh_delimRecall,
   ( DBENTRYP_ULP ) zh_delimRecCount,
   NULL /* zh_delimRecInfo */,
   ( DBENTRYP_ULP ) zh_delimRecNo,
   ( DBENTRYP_I ) zh_delimRecId,
   ( DBENTRYP_S ) zh_delimSetFieldExtent,
   NULL /* zh_delimAlias */,
   ( DBENTRYP_V ) zh_delimClose,
   ( DBENTRYP_VO ) zh_delimCreate,
   ( DBENTRYP_SI ) zh_delimInfo,
   ( DBENTRYP_V ) zh_delimNewArea,
   ( DBENTRYP_VO ) zh_delimOpen,
   NULL /* zh_delimRelease */,
   ( DBENTRYP_SP ) zh_delimStructSize,
   NULL /* zh_delimSysName */,
   NULL /* zh_delimEval */,
   NULL /* zh_delimPack */,
   NULL /* zh_delimPackRec */,
   NULL /* zh_delimSort */,
   ( DBENTRYP_VT ) zh_delimTrans,
   NULL /* zh_delimTransRec */,
   NULL /* zh_delimZap */,
   NULL /* zh_delimChildEnd */,
   NULL /* zh_delimChildStart */,
   NULL /* zh_delimChildSync */,
   NULL /* zh_delimSyncChildren */,
   NULL /* zh_delimClearRel */,
   NULL /* zh_delimForceRel */,
   NULL /* zh_delimRelArea */,
   NULL /* zh_delimRelEval */,
   NULL /* zh_delimRelText */,
   NULL /* zh_delimSetRel */,
   NULL /* zh_delimOrderListAdd */,
   NULL /* zh_delimOrderListClear */,
   NULL /* zh_delimOrderListDelete */,
   NULL /* zh_delimOrderListFocus */,
   NULL /* zh_delimOrderListRebuild */,
   NULL /* zh_delimOrderCondition */,
   NULL /* zh_delimOrderCreate */,
   NULL /* zh_delimOrderDestroy */,
   NULL /* zh_delimOrderInfo */,
   NULL /* zh_delimClearFilter */,
   NULL /* zh_delimClearLocate */,
   NULL /* zh_delimClearScope */,
   NULL /* zh_delimCountScope */,
   NULL /* zh_delimFilterText */,
   NULL /* zh_delimScopeInfo */,
   NULL /* zh_delimSetFilter */,
   NULL /* zh_delimSetLocate */,
   NULL /* zh_delimSetScope */,
   NULL /* zh_delimSkipScope */,
   NULL /* zh_delimLocate */,
   NULL /* zh_delimCompile */,
   NULL /* zh_delimError */,
   NULL /* zh_delimEvalBlock */,
   NULL /* zh_delimRawLock */,
   NULL /* zh_delimLock */,
   NULL /* zh_delimUnLock */,
   NULL /* zh_delimCloseMemFile */,
   NULL /* zh_delimCreateMemFile */,
   NULL /* zh_delimGetValueFile */,
   NULL /* zh_delimOpenMemFile */,
   NULL /* zh_delimPutValueFile */,
   NULL /* zh_delimReadDBHeader */,
   NULL /* zh_delimWriteDBHeader */,
   ( DBENTRYP_R ) zh_delimInit,
   ( DBENTRYP_R ) zh_delimExit,
   NULL /* zh_delimDrop */,
   NULL /* zh_delimExists */,
   NULL /* zh_delimRename */,
   ( DBENTRYP_RSLV ) zh_delimRddInfo,
   NULL /* zh_delimWhoCares */
};

ZH_FUNC( DELIM ) { ; }

ZH_FUNC_STATIC( DELIM_GETFUNCTABLE )
{
   RDDFUNCS * pTable;
   ZH_USHORT * puiCount;

   puiCount = ( ZH_USHORT * ) zh_parptr( 1 );
   pTable = ( RDDFUNCS * ) zh_parptr( 2 );

   ZH_TRACE( ZH_TR_DEBUG, ( "DELIM_GETFUNCTABLE(%p, %p)", ( void * ) puiCount, ( void * ) pTable ) );

   if( pTable )
   {
      if( puiCount )
         *puiCount = RDDFUNCSCOUNT;
      zh_retni( zh_rddInheritEx( pTable, &delimTable, &delimSuper, NULL, NULL ) );
   }
   else
      zh_retni( ZH_FAILURE );
}

static void zh_delimRddInit( void * cargo )
{
   ZH_SYMBOL_UNUSED( cargo );

   if( zh_rddRegister( "DELIM", RDD_REGISTER_TYPE_TRANSFER ) > 1 )
      zh_errInternal( ZH_EI_RDDINVALID, NULL, NULL, NULL );
}

ZH_INIT_SYMBOLS_BEGIN( delim1__InitSymbols )
{ "DELIM",              {ZH_FS_PUBLIC|ZH_FS_LOCAL}, {ZH_FUNCNAME( DELIM )}, NULL },
{ "DELIM_GETFUNCTABLE", {ZH_FS_PUBLIC|ZH_FS_LOCAL}, {ZH_FUNCNAME( DELIM_GETFUNCTABLE )}, NULL }
ZH_INIT_SYMBOLS_END( delim1__InitSymbols )

ZH_CALL_ON_STARTUP_BEGIN( _zh_delim_rdd_init_ )
   zh_vmAtInit( zh_delimRddInit, NULL );
ZH_CALL_ON_STARTUP_END( _zh_delim_rdd_init_ )

#if defined( ZH_DATASEG_STARTUP )
   #define ZH_DATASEG_BODY    ZH_DATASEG_FUNC( delim1__InitSymbols ) \
                              ZH_DATASEG_FUNC( _zh_delim_rdd_init_ )
   #include "..\zh_ini_seg.h"
#endif
