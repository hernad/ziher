/*
 * __dbSQL()
 *
 * Copyright 2007 Przemyslaw Czerpak
 * Copyright 2007 Lorenzo Fiorini <lorenzo.fiorini / at / gmail.com>
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
#include "zh_apifs.h"
#include "zh_gt_api.h"
#include "zh_item_api.h"
#include "zh_rdd_api.h"
#include "zh_lang_api.h"
#include "zh_api_error.h"
#include "zh_vm.h"
#include "zh_date.h"
#include "dbf_cdx/zh_dbf_error.h"

#define ZH_FILE_BUF_SIZE  0x10000
typedef struct _ZH_FILEBUF
{
   PZH_FILE   pFile;
   ZH_BYTE *  pBuf;
   ZH_SIZE    nSize;
   ZH_SIZE    nPos;
} ZH_FILEBUF;
typedef ZH_FILEBUF * PZH_FILEBUF;

static void zh_flushFBuffer( PZH_FILEBUF pFileBuf )
{
   if( pFileBuf->nPos > 0 )
   {
      zh_fileWrite( pFileBuf->pFile, pFileBuf->pBuf, pFileBuf->nPos, -1 );
      pFileBuf->nPos = 0;
   }
}

static void zh_addToFBuffer( PZH_FILEBUF pFileBuf, char ch )
{
   if( pFileBuf->nPos == pFileBuf->nSize )
      zh_flushFBuffer( pFileBuf );
   pFileBuf->pBuf[ pFileBuf->nPos++ ] = ( ZH_BYTE ) ch;
}

static void zh_addStrnToFBuffer( PZH_FILEBUF pFileBuf, const char * str, ZH_SIZE nSize )
{
   ZH_SIZE nPos = 0;

   while( nPos < nSize )
   {
      if( pFileBuf->nPos == pFileBuf->nSize )
         zh_flushFBuffer( pFileBuf );
      pFileBuf->pBuf[ pFileBuf->nPos++ ] = ( ZH_BYTE ) str[ nPos++ ];
   }
}

static void zh_addStrToFBuffer( PZH_FILEBUF pFileBuf, const char * szStr )
{
   while( *szStr )
   {
      if( pFileBuf->nPos == pFileBuf->nSize )
         zh_flushFBuffer( pFileBuf );
      pFileBuf->pBuf[ pFileBuf->nPos++ ] = ( ZH_BYTE ) *szStr++;
   }
}

static void zh_destroyFBuffer( PZH_FILEBUF pFileBuf )
{
   zh_flushFBuffer( pFileBuf );
   if( pFileBuf->pBuf )
      zh_xfree( pFileBuf->pBuf );
   zh_xfree( pFileBuf );
}

static PZH_FILEBUF zh_createFBuffer( PZH_FILE pFile, ZH_SIZE nSize )
{
   PZH_FILEBUF pFileBuf = ( PZH_FILEBUF ) zh_xgrab( sizeof( ZH_FILEBUF ) );

   pFileBuf->pFile = pFile;
   pFileBuf->pBuf = ( ZH_BYTE * ) zh_xgrab( nSize );
   pFileBuf->nSize = nSize;
   pFileBuf->nPos = 0;
   return pFileBuf;
}


/* Export field value into the buffer in SQL format */
static ZH_BOOL zh_exportBufSqlVar( PZH_FILEBUF pFileBuf, PZH_ITEM pValue,
                                   const char * szDelim, const char * szEsc )
{
   switch( zh_itemType( pValue ) )
   {
      case ZH_IT_STRING:
      case ZH_IT_MEMO:
      {
         ZH_SIZE nLen = zh_itemGetCLen( pValue );
         ZH_SIZE nCnt = 0;
         const char *szVal = zh_itemGetCPtr( pValue );

         zh_addStrToFBuffer( pFileBuf, szDelim );
         while( nLen && ZH_ISSPACE( szVal[ nLen - 1 ] ) )
            nLen--;

         while( *szVal && nCnt++ < nLen )
         {
            if( *szVal == *szDelim || *szVal == *szEsc )
               zh_addToFBuffer( pFileBuf, *szEsc );
            if( ( ZH_UCHAR ) *szVal >= 32 )
               zh_addToFBuffer( pFileBuf, *szVal );
            else
            {
#if 0
               printf( "%d %c", *szVal, *szVal );
#endif
            }
            szVal++;
         }
         zh_addStrToFBuffer( pFileBuf, szDelim );
         break;
      }

      case ZH_IT_DATE:
      {
         char szDate[ 9 ];

         zh_addStrToFBuffer( pFileBuf, szDelim );
         zh_itemGetDS( pValue, szDate );
         if( szDate[ 0 ] == ' ' )
         {
            zh_addStrToFBuffer( pFileBuf, "0100-01-01" );
         }
         else
         {
            zh_addStrnToFBuffer( pFileBuf, &szDate[ 0 ], 4 );
            zh_addToFBuffer( pFileBuf, '-' );
            zh_addStrnToFBuffer( pFileBuf, &szDate[ 4 ], 2 );
            zh_addToFBuffer( pFileBuf, '-' );
            zh_addStrnToFBuffer( pFileBuf, &szDate[ 6 ], 2 );
         }
         zh_addStrToFBuffer( pFileBuf, szDelim );
         break;
      }

      case ZH_IT_TIMESTAMP:
      {
         long lDate, lTime;
         char szDateTime[ 24 ];

         zh_itemGetTDT( pValue, &lDate, &lTime );
         zh_timeStampStr( szDateTime, lDate, lTime );
         zh_addStrToFBuffer( pFileBuf, szDelim );
         zh_addStrToFBuffer( pFileBuf, szDateTime );
         zh_addStrToFBuffer( pFileBuf, szDelim );
         break;
      }

      case ZH_IT_LOGICAL:
         zh_addStrToFBuffer( pFileBuf, szDelim );
         zh_addToFBuffer( pFileBuf, zh_itemGetL( pValue ) ? 'Y' : 'N' );
         zh_addStrToFBuffer( pFileBuf, szDelim );
         break;

      case ZH_IT_INTEGER:
      case ZH_IT_LONG:
      case ZH_IT_DOUBLE:
      {
         char szResult[ ZH_MAX_DOUBLE_LENGTH ];
         int iSize, iWidth, iDec;

         zh_itemGetNLen( pValue, &iWidth, &iDec );
         iSize = ( iDec > 0 ? iWidth + 1 + iDec : iWidth );
         if( zh_itemStrBuf( szResult, pValue, iSize, iDec ) )
         {
            int iPos = 0;
            while( iSize && ZH_ISSPACE( szResult[ iPos ] ) )
            {
               iPos++;
               iSize--;
            }
            zh_addStrnToFBuffer( pFileBuf, &szResult[ iPos ], iSize );
         }
         else
            zh_addToFBuffer( pFileBuf, '0' );
         break;
      }
      /* an "M" field or the other, might be a "V" in SixDriver */
      default:
         /* We do not want MEMO contents */
         return ZH_FALSE;
   }
   return ZH_TRUE;
}

/* Export DBF content to a SQL script file */
static ZH_ULONG zh_db2Sql( AREAP pArea, PZH_ITEM pFields, ZH_MAXINT llNext,
                           PZH_ITEM pWhile, PZH_ITEM pFor,
                           const char * szDelim, const char * szSep,
                           const char * szEsc, const char * szTable,
                           PZH_FILE pFile, ZH_BOOL fInsert, ZH_BOOL fRecno )
{
   PZH_FILEBUF pFileBuf;
   ZH_ULONG ulRecords = 0;
   ZH_USHORT uiFields = 0, ui;
   PZH_ITEM pTmp;
   ZH_BOOL fWriteSep = ZH_FALSE;
   const char * szNewLine = zh_conNewLine();
   char * szInsert = NULL;
   ZH_BOOL fEof = ZH_TRUE;
   ZH_BOOL fNoFieldPassed = ( pFields == NULL || zh_arrayLen( pFields ) == 0 );

   if( SELF_FIELDCOUNT( pArea, &uiFields ) != ZH_SUCCESS )
      return 0;

   if( fInsert && szTable )
      szInsert = zh_xstrcpy( NULL, "INSERT INTO ", szTable, " VALUES ( ", NULL );

   pFileBuf = zh_createFBuffer( pFile, ZH_FILE_BUF_SIZE );
   pTmp = zh_itemNew( NULL );

   while( llNext-- > 0 )
   {
      if( pWhile )
      {
         if( SELF_EVALBLOCK( pArea, pWhile ) != ZH_SUCCESS ||
             ! zh_itemGetL( pArea->valResult ) )
            break;
      }

      if( SELF_EOF( pArea, &fEof ) != ZH_SUCCESS || fEof )
         break;

      if( pFor )
      {
         if( SELF_EVALBLOCK( pArea, pFor ) != ZH_SUCCESS )
            break;
      }
      if( ! pFor || zh_itemGetL( pArea->valResult ) )
      {
         ++ulRecords;

         if( szInsert )
            zh_addStrToFBuffer( pFileBuf, szInsert );

         if( fRecno )
         {
            ZH_ULONG ulRec = ulRecords;
            char szRecno[ 13 ], * szVal;

            szVal = szRecno + sizeof( szRecno );
            *--szVal = 0;
            do
            {
               *--szVal = ( char ) ( ulRec % 10 ) + '0';
               ulRec /= 10;
            }
            while( ulRec );
            zh_addStrToFBuffer( pFileBuf, szVal );
            zh_addStrToFBuffer( pFileBuf, szSep );
         }

         if( fNoFieldPassed )
         {
            for( ui = 1; ui <= uiFields; ui++ )
            {
               if( SELF_GETVALUE( pArea, ui, pTmp ) != ZH_SUCCESS )
                  break;
               if( fWriteSep )
                  zh_addStrToFBuffer( pFileBuf, szSep );
               fWriteSep = zh_exportBufSqlVar( pFileBuf, pTmp, szDelim, szEsc );
            }
            if( ui <= uiFields )
               break;
         }
         else
         {
            /* TODO: exporting only some fields */
         }

         if( szInsert )
            zh_addStrToFBuffer( pFileBuf, " );" );
         zh_addStrToFBuffer( pFileBuf, szNewLine );
         fWriteSep = ZH_FALSE;
      }

      if( SELF_SKIP( pArea, 1 ) != ZH_SUCCESS )
         break;

      if( ( llNext % 10000 ) == 0 )
         zh_inkeyPoll();
   }

   if( szInsert )
      zh_xfree( szInsert );
   zh_destroyFBuffer( pFileBuf );
   zh_itemRelease( pTmp );

#if 0
   /* Writing EOF */
   zh_fileWrite( pFile, "\x1A", 1, -1 );
#endif

   return ulRecords;
}

ZH_FUNC( __DBSQL )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      ZH_BOOL fExport         = zh_parl( 1 );
      const char * szFileName = zh_parc( 2 );
      const char * szTable    = zh_parc( 3 );
      PZH_ITEM pFields        = zh_param( 4, ZH_IT_ARRAY );
      PZH_ITEM pFor           = zh_param( 5, ZH_IT_BLOCK );
      PZH_ITEM pWhile         = zh_param( 6, ZH_IT_BLOCK );
      PZH_ITEM pNext          = zh_param( 7, ZH_IT_NUMERIC );
      PZH_ITEM pRecord        = ZH_ISNIL( 8 ) ? NULL : zh_param( 8, ZH_IT_ANY );
      ZH_BOOL fRest           = pWhile != NULL || zh_parl( 9 );
      ZH_BOOL fAppend         = zh_parl( 10 );
      ZH_BOOL fInsert         = zh_parl( 11 );
      ZH_BOOL fRecno          = zh_parl( 12 );
      const char * szSep      = zh_parcx( 13 );
      const char * szDelim    = zh_parcx( 14 );
      const char * szEsc      = zh_parcx( 15 );
      ZH_MAXINT llNext        = ZH_VMLONG_MAX;
      ZH_ERRCODE errCode;
      PZH_FILE pFile;

      if( ! szFileName )
         zh_errRT_DBCMD( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, ZH_ERR_FUNCNAME );
      else if( fExport )   /* COPY TO SQL */
      {
         PZH_ITEM pError = NULL;
         ZH_BOOL fRetry;

         /* Try to create Dat file */
         do
         {
            pFile = zh_fileExtOpen( szFileName, NULL,
                                    ( fAppend ? 0 : FXO_TRUNCATE ) |
                                    FO_READWRITE | FO_EXCLUSIVE |
                                    FXO_DEFAULTS | FXO_SHARELOCK,
                                    NULL, pError );
            if( pFile == NULL )
            {
               if( ! pError )
               {
                  pError = zh_errNew();
                  zh_errPutSeverity( pError, ES_ERROR );
                  if( fAppend )
                  {
                     zh_errPutGenCode( pError, EG_OPEN );
                     zh_errPutSubCode( pError, EDBF_OPEN_DBF );
                     zh_errPutDescription( pError, zh_langDGetErrorDesc( EG_OPEN ) );
                  }
                  else
                  {
                     zh_errPutGenCode( pError, EG_CREATE );
                     zh_errPutSubCode( pError, EDBF_CREATE_DBF );
                     zh_errPutDescription( pError, zh_langDGetErrorDesc( EG_CREATE ) );
                  }
                  zh_errPutFileName( pError, szFileName );
                  zh_errPutFlags( pError, EF_CANRETRY | EF_CANDEFAULT );
                  zh_errPutSubSystem( pError, "DBF2SQL" );
                  zh_errPutOsCode( pError, zh_fsError() );
               }
               fRetry = zh_errLaunch( pError ) == E_RETRY;
            }
            else
               fRetry = ZH_FALSE;
         }
         while( fRetry );

         if( pError )
            zh_itemRelease( pError );

         if( pFile != NULL )
         {
            if( fAppend )
               zh_fileSeek( pFile, 0, FS_END );

            errCode = ZH_SUCCESS;
            if( pRecord )
            {
               errCode = SELF_GOTOID( pArea, pRecord );
            }
            else if( pNext )
            {
               llNext = zh_itemGetNInt( pNext );
            }
            else if( ! fRest )
            {
               errCode = SELF_GOTOP( pArea );
            }

            if( errCode == ZH_SUCCESS )
            {
               zh_retnint( zh_db2Sql( pArea, pFields, llNext, pWhile, pFor,
                                      szDelim, szSep, szEsc,
                                      szTable, pFile, fInsert, fRecno ) );
            }
            zh_fileClose( pFile );
         }
      }
      else
      {
         /* TODO: import code */
      }
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}
