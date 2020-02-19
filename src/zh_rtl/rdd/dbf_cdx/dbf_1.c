/*
 * DBF RDD module
 *
 * Copyright 2003-2015 Przemyslaw Czerpak
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

#define ZH_TRIGVAR_BYREF

#include "zh_api.h"
#include "zh_apifs.h"
#include "zh_rdd_dbf.h"
#include "zh_item_api.h"
#include "zh_string_api.h"
#include "zh_api_error.h"
#include "zh_lang_api.h"
#include "zh_set.h"
#include "zh_date.h"
#include "zh_stack.h"
#include "zh_vm.h"
#include "error.zhh"

#include "zh_rtl/rdd_sys.zhh"
#include "zh_rtl/dbf_six.zhh"
#include "zh_codepage_api.h"

#include "../dbf_six/zh_sx_func.h"

static ZH_USHORT s_uiRddId = ( ZH_USHORT ) -1;
static RDDFUNCS  dbfSuper;


/*
 * Common functions.
 */

#define ZH_BLANK_APPEND    1
#define ZH_BLANK_EOF       2
#define ZH_BLANK_ROLLBACK  3

#define ZH_BLANK_SKIP      100
#define ZH_BLANK_AUTOINC   101
#define ZH_BLANK_UNISPACE  102

#define ZH_AUTOINC_NONE    0
#define ZH_AUTOINC_STD     1
#define ZH_AUTOINC_LONG    2

/*
 * generate Run-Time error
 */
static ZH_ERRCODE zh_dbfErrorRT( DBFAREAP pArea,
                                 ZH_ERRCODE errGenCode, ZH_ERRCODE errSubCode,
                                 const char * szFileName, ZH_ERRCODE errOsCode,
                                 ZH_USHORT uiFlags, PZH_ITEM * pErrorPtr )
{
   ZH_ERRCODE errCode = ZH_FAILURE;

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
      errCode = SELF_ERROR( &pArea->area, pError );
      if( ! pErrorPtr )
         zh_errRelease( pError );
   }
   return errCode;
}

static ZH_MAXINT zh_dbfRowVerGet( DBFAREAP pArea, ZH_USHORT uiField, ZH_MAXINT * pValue )
{
   DBFFIELD dbField;
   ZH_BOOL fLck = ZH_FALSE;

   *pValue = 0;
   if( pArea->fShared && ! pArea->fFLocked && ! pArea->fHeaderLocked )
   {
      if( SELF_RAWLOCK( &pArea->area, HEADER_LOCK, 0 ) != ZH_SUCCESS )
         return ZH_FAILURE;
      fLck = ZH_TRUE;
   }

   if( zh_fileReadAt( pArea->pDataFile, &dbField, sizeof( dbField ),
                      sizeof( DBFHEADER ) + uiField * sizeof( DBFFIELD ) ) ==
       sizeof( dbField ) )
   {
      *pValue = ZH_GET_LE_UINT64( dbField.bReserved2 ) + 1;
      ZH_PUT_LE_UINT64( dbField.bReserved2, *pValue );
      zh_fileWriteAt( pArea->pDataFile, &dbField, sizeof( dbField ),
                      sizeof( DBFHEADER ) + uiField * sizeof( DBFFIELD ) );
   }

   if( fLck )
   {
      if( SELF_RAWLOCK( &pArea->area, HEADER_UNLOCK, 0 ) != ZH_SUCCESS )
         return ZH_FAILURE;
   }

   return ZH_SUCCESS;
}

static void zh_dbfRowVerSet( DBFAREAP pArea, ZH_USHORT uiField, ZH_MAXINT nValue )
{
   DBFFIELD dbField;

   if( zh_fileReadAt( pArea->pDataFile, &dbField, sizeof( dbField ),
                      sizeof( DBFHEADER ) + uiField * sizeof( DBFFIELD ) ) ==
       sizeof( dbField ) )
   {
      ZH_PUT_LE_UINT64( dbField.bReserved2, nValue );
      zh_fileWriteAt( pArea->pDataFile, &dbField, sizeof( dbField ),
                      sizeof( DBFHEADER ) + uiField * sizeof( DBFFIELD ) );
   }
}

static ZH_BOOL zh_dbfIsAutoIncField( LPFIELD pField )
{
   if( pField->uiType == ZH_FT_AUTOINC )
      return pField->uiLen - pField->uiDec > 4 ?
             ZH_AUTOINC_LONG : ZH_AUTOINC_STD;
   else if( pField->uiType == ZH_FT_ROWVER )
      return ZH_AUTOINC_LONG;
   else if( ( pField->uiFlags & ZH_FF_AUTOINC ) != 0 )
   {
      switch( pField->uiType )
      {
         case ZH_FT_DOUBLE:
            return ZH_AUTOINC_LONG;
         case ZH_FT_LONG:
         case ZH_FT_FLOAT:
            return pField->uiLen - ( pField->uiDec ? pField->uiDec + 1 : 0 ) > 9 ?
                   ZH_AUTOINC_LONG : ZH_AUTOINC_STD;
         case ZH_FT_INTEGER:
            return pField->uiLen - pField->uiDec > 4 ?
                   ZH_AUTOINC_LONG : ZH_AUTOINC_STD;
      }
   }
   return ZH_AUTOINC_NONE;
}

static void zh_dbfNextValueInit( LPDBFFIELD pDbField, LPFIELD pField )
{
   if( zh_dbfIsAutoIncField( pField ) == ZH_AUTOINC_LONG )
      ZH_PUT_LE_UINT64( pDbField->bReserved2, 1 );
   else
      ZH_PUT_LE_UINT32( pDbField->bCounter, 1 );
   pDbField->bStep = 1;
}

static ZH_MAXINT zh_dbfNextValueGet( DBFAREAP pArea, ZH_USHORT uiField,
                                     ZH_BOOL fUpdate )
{
   ZH_MAXINT nValue = 0;
   DBFFIELD dbField;

   if( zh_fileReadAt( pArea->pDataFile, &dbField, sizeof( dbField ),
                      sizeof( DBFHEADER ) + uiField * sizeof( DBFFIELD ) ) ==
       sizeof( dbField ) )
   {
      int iType = zh_dbfIsAutoIncField( pArea->area.lpFields + uiField );

      if( iType == ZH_AUTOINC_LONG )
         nValue = ZH_GET_LE_UINT64( dbField.bReserved2 );
      else
         nValue = ZH_GET_LE_UINT32( dbField.bCounter );
      if( fUpdate )
      {
         if( iType == ZH_AUTOINC_LONG )
            ZH_PUT_LE_UINT64( dbField.bReserved2, nValue + dbField.bStep );
         else
            ZH_PUT_LE_UINT32( dbField.bCounter, nValue + dbField.bStep );
         zh_fileWriteAt( pArea->pDataFile, &dbField, sizeof( dbField ),
                         sizeof( DBFHEADER ) + uiField * sizeof( DBFFIELD ) );
      }
   }

   return nValue;
}

static ZH_MAXINT zh_dbfNextValueSet( DBFAREAP pArea, ZH_USHORT uiField,
                                     ZH_MAXINT nValue )
{
   DBFFIELD dbField;
   ZH_MAXINT nPrevValue = 0;

   if( zh_fileReadAt( pArea->pDataFile, &dbField, sizeof( dbField ),
                      sizeof( DBFHEADER ) + uiField * sizeof( DBFFIELD ) ) ==
       sizeof( dbField ) )
   {
      if( zh_dbfIsAutoIncField( pArea->area.lpFields + uiField ) == ZH_AUTOINC_LONG )
      {
         nPrevValue = ZH_GET_LE_UINT64( dbField.bReserved2 );
         ZH_PUT_LE_UINT64( dbField.bReserved2, nValue );
      }
      else
      {
         nPrevValue = ZH_GET_LE_UINT32( dbField.bCounter );
         ZH_PUT_LE_UINT32( dbField.bCounter, nValue );
      }
      zh_fileWriteAt( pArea->pDataFile, &dbField, sizeof( dbField ),
                      sizeof( DBFHEADER ) + uiField * sizeof( DBFFIELD ) );
   }
   return nPrevValue;
}

static int zh_dbfNextValueStep( DBFAREAP pArea, ZH_USHORT uiField, int iStep )
{
   DBFFIELD dbField;
   int iPrevStep = 0;

   if( zh_fileReadAt( pArea->pDataFile, &dbField, sizeof( dbField ),
                      sizeof( DBFHEADER ) + uiField * sizeof( DBFFIELD ) ) ==
       sizeof( dbField ) )
   {
      iPrevStep = dbField.bStep;
      if( iStep != 0 )
      {
         dbField.bStep = ( ZH_BYTE ) iStep;
         zh_fileWriteAt( pArea->pDataFile, &dbField, sizeof( dbField ),
                         sizeof( DBFHEADER ) + uiField * sizeof( DBFFIELD ) );
      }
   }

   return iPrevStep;
}

void zh_dbfTransCheckCounters( LPDBTRANSINFO lpdbTransInfo )
{
   ZH_BOOL fCopyCtr = ZH_TRUE;
   ZH_USHORT uiCount, uiDest;
   DBFAREAP pArea = ( DBFAREAP ) lpdbTransInfo->lpaDest;

   if( pArea->ulRecCount > 0 || ( pArea->fShared && ! pArea->fFLocked ) )
      fCopyCtr = ZH_FALSE;
   else
   {
      PZH_ITEM pItem = NULL;

      /* check if counters can be copied for all fields */
      for( uiCount = 0; uiCount < lpdbTransInfo->uiItemCount; ++uiCount )
      {
         ZH_USHORT uiField = lpdbTransInfo->lpTransItems[ uiCount ].uiDest;
         LPFIELD pField = lpdbTransInfo->lpaDest->lpFields + uiField - 1;

         if( zh_dbfIsAutoIncField( pField ) != ZH_AUTOINC_NONE )
         {
            if( pItem == NULL )
               pItem = zh_itemNew( NULL );
            if( SELF_FIELDINFO( lpdbTransInfo->lpaSource,
                                lpdbTransInfo->lpTransItems[ uiCount ].uiSource,
                                DBS_COUNTER, pItem ) != ZH_SUCCESS )
            {
               fCopyCtr = ZH_FALSE;
               break;
            }
         }
      }
      if( pItem != NULL )
         zh_itemRelease( pItem );
   }

   if( fCopyCtr )
      lpdbTransInfo->uiFlags |= DBTF_CPYCTR;
   else
   {
      for( uiCount = uiDest = 0; uiCount < lpdbTransInfo->uiItemCount; ++uiCount )
      {
         ZH_USHORT uiField = lpdbTransInfo->lpTransItems[ uiCount ].uiDest;
         LPFIELD pField = lpdbTransInfo->lpaDest->lpFields + uiField - 1;

         if( zh_dbfIsAutoIncField( pField ) == ZH_AUTOINC_NONE &&
             pField->uiType != ZH_FT_MODTIME )
         {
            if( uiDest != uiCount )
            {
               lpdbTransInfo->lpTransItems[ uiDest ].uiSource =
               lpdbTransInfo->lpTransItems[ uiCount ].uiSource;
               lpdbTransInfo->lpTransItems[ uiDest ].uiDest =
               lpdbTransInfo->lpTransItems[ uiCount ].uiDest;
            }
            ++uiDest;
         }
      }
      if( uiDest < uiCount )
      {
         lpdbTransInfo->uiItemCount = uiDest;
         lpdbTransInfo->uiFlags &= ~( DBTF_MATCH | DBTF_PUTREC );
      }
   }
}

static void zh_dbfUpdateStampFields( DBFAREAP pArea )
{
   long lJulian = 0, lMilliSec = 0;
   ZH_MAXINT nRowVer = 0;
   LPFIELD pField;
   ZH_USHORT uiCount;

   for( uiCount = 0, pField = pArea->area.lpFields; uiCount < pArea->area.uiFieldCount; uiCount++, pField++ )
   {
      switch( pField->uiType )
      {
         case ZH_FT_MODTIME:
         {
            ZH_BYTE * pPtr = pArea->pRecord + pArea->pFieldOffset[ uiCount ];
            if( !pArea->fTransRec || ZH_GET_LE_UINT64( pPtr ) == 0 )
            {
               if( lJulian == 0 )
                  zh_timeStampGet( &lJulian, &lMilliSec );
               ZH_PUT_LE_UINT32( pPtr, lJulian );
               pPtr += 4;
               ZH_PUT_LE_UINT32( pPtr, lMilliSec );
            }
            break;
         }
         case ZH_FT_ROWVER:
         {
            ZH_BYTE * pPtr = pArea->pRecord + pArea->pFieldOffset[ uiCount ];
            if( !pArea->fTransRec || ZH_GET_LE_UINT64( pPtr ) == 0 )
            {
               if( nRowVer == 0 )
                  zh_dbfRowVerGet( pArea, uiCount, &nRowVer );
               ZH_PUT_LE_UINT64( pPtr, nRowVer );
            }
            break;
         }
      }
   }
}

static void zh_dbfSetBlankRecord( DBFAREAP pArea, int iType )
{
   ZH_BYTE * pPtr = pArea->pRecord, bFill = ' ', bNext;
   ZH_SIZE nSize = 1; /* 1 byte ' ' for DELETE flag */
   ZH_USHORT uiCount;
   LPFIELD pField;

   for( uiCount = 0, pField = pArea->area.lpFields; uiCount < pArea->area.uiFieldCount; uiCount++, pField++ )
   {
      ZH_USHORT uiLen = pField->uiLen;

      switch( pField->uiType )
      {
         case ZH_FT_MEMO:
         case ZH_FT_IMAGE:
         case ZH_FT_BLOB:
         case ZH_FT_OLE:
            bNext = uiLen == 10 ? ' ' : '\0';
            break;

         case ZH_FT_DATE:
            bNext = uiLen == 8 ? ' ' : '\0';
            break;

         case ZH_FT_LOGICAL:
            bNext = ' ';
            break;

         case ZH_FT_STRING:
            bNext = ( pField->uiFlags & ZH_FF_UNICODE ) != 0 ?
                    ZH_BLANK_UNISPACE : ' ';
            break;

         case ZH_FT_LONG:
         case ZH_FT_FLOAT:
            if( pField->uiFlags & ZH_FF_AUTOINC )
            {
               if( iType == ZH_BLANK_APPEND )
               {
                  bNext = ZH_BLANK_AUTOINC;
                  break;
               }
               else if( iType == ZH_BLANK_ROLLBACK )
               {
                  bNext = ZH_BLANK_SKIP;
                  break;
               }
            }
            bNext = ' ';
            break;

         case ZH_FT_AUTOINC:
            if( iType == ZH_BLANK_APPEND )
               bNext = ZH_BLANK_AUTOINC;
            else if( iType == ZH_BLANK_ROLLBACK )
               bNext = ZH_BLANK_SKIP;
            else
               bNext = '\0';
            break;

         case ZH_FT_INTEGER:
         case ZH_FT_DOUBLE:
            if( pField->uiFlags & ZH_FF_AUTOINC )
            {
               if( iType == ZH_BLANK_APPEND )
               {
                  bNext = ZH_BLANK_AUTOINC;
                  break;
               }
               else if( iType == ZH_BLANK_ROLLBACK )
               {
                  bNext = ZH_BLANK_SKIP;
                  break;
               }
            }
            bNext = '\0';
            break;

         case ZH_FT_VARLENGTH:
            if( pField->uiFlags & ZH_FF_UNICODE )
               uiLen = ( uiLen + 1 ) << 1;
            /* fallthrough */

         default:
            bNext = '\0';
            break;
      }

      if( bNext == bFill )
      {
         nSize += uiLen;
      }
      else
      {
         if( nSize )
         {
            memset( pPtr, bFill, nSize );
            pPtr += nSize;
            nSize = 0;
         }
         if( bNext == ZH_BLANK_SKIP )
         {
            pPtr += uiLen;
         }
         else if( bNext == ZH_BLANK_UNISPACE )
         {
            while( uiLen-- )
            {
               ZH_PUT_LE_UINT16( pPtr, 0x0020 );
               pPtr += 2;
            }
         }
         else if( bNext == ZH_BLANK_AUTOINC )
         {
            ZH_MAXINT nValue = zh_dbfNextValueGet( pArea, uiCount, ZH_TRUE );
            if( pField->uiType == ZH_FT_INTEGER ||
                pField->uiType == ZH_FT_AUTOINC )
            {
               if( pField->uiDec )
                  nValue = ( ZH_MAXINT ) zh_numDecConv( ( double ) nValue,
                                                        -( int ) pField->uiDec );
               if( uiLen == 1 )
                  *pPtr = ( signed char ) nValue;
               else if( uiLen == 2 )
                  ZH_PUT_LE_UINT16( pPtr, nValue );
               else if( uiLen == 3 )
                  ZH_PUT_LE_UINT24( pPtr, nValue );
               else if( uiLen == 4 )
                  ZH_PUT_LE_UINT32( pPtr, nValue );
               else if( uiLen == 8 )
                  ZH_PUT_LE_UINT64( pPtr, nValue );
            }
            else if( pField->uiType == ZH_FT_DOUBLE )
            {
               ZH_PUT_LE_DOUBLE( pPtr, nValue );
            }
            else
            {
               ZH_USHORT ui = uiLen;
               do
               {
                  pPtr[ --ui ] = ( ZH_BYTE ) nValue % 10 + '0';
                  nValue /= 10;
               }
               while( ui && nValue > 0 );
               while( ui )
                  pPtr[ --ui ] = ' ';
            }
            pPtr += uiLen;
         }
         else
         {
            nSize = uiLen;
            bFill = bNext;
         }
      }
   }
   memset( pPtr, bFill, nSize );

   nSize += pPtr - pArea->pRecord;
   if( nSize < ( ZH_SIZE ) pArea->uiRecordLen )
      memset( pArea->pRecord + nSize, '\0', ( ZH_SIZE ) pArea->uiRecordLen - nSize );

   /* set varlength and nullable bits in _NullFlags */
   if( pArea->uiNullCount )
   {
      memset( pArea->pRecord + pArea->uiNullOffset, 0xff, pArea->uiNullCount >> 3 );
      uiCount = pArea->uiNullCount & 0x07;
      if( uiCount )
         pArea->pRecord[ pArea->uiNullOffset + ( pArea->uiNullCount >> 3 ) ] = ( 1 << uiCount ) - 1;
   }
}

static void zh_dbfAllocNullFlag( DBFAREAP pArea, ZH_USHORT uiField, ZH_BOOL fLength )
{
   if( ! pArea->pFieldBits )
   {
      ZH_SIZE nSize = sizeof( ZH_DBFFIELDBITS ) * pArea->area.uiFieldExtent;
      pArea->pFieldBits = ( PZH_DBFFIELDBITS ) zh_xgrabz( nSize );
   }
   if( fLength )
      pArea->pFieldBits[ uiField ].uiLengthBit = pArea->uiNullCount++;
   else
      pArea->pFieldBits[ uiField ].uiNullBit = pArea->uiNullCount++;
}

static ZH_BOOL zh_dbfGetNullFlag( DBFAREAP pArea, ZH_USHORT uiBit )
{
   return ( pArea->pRecord[ pArea->uiNullOffset + ( uiBit >> 3 ) ] &
            ( 1 << ( uiBit & 0x07 ) ) ) != 0;
}

static void zh_dbfSetNullFlag( ZH_BYTE * pRecord, ZH_USHORT uiNullOffset, ZH_USHORT uiBit )
{
   pRecord[ uiNullOffset + ( uiBit >> 3 ) ] |= 1 << ( uiBit & 0x07 );
}

static void zh_dbfClearNullFlag( ZH_BYTE * pRecord, ZH_USHORT uiNullOffset, ZH_USHORT uiBit )
{
   pRecord[ uiNullOffset + ( uiBit >> 3 ) ] &= ~( 1 << ( uiBit & 0x07 ) );
}

/*
 * Executes user trigger function
 */
static ZH_BOOL zh_dbfTriggerDo( DBFAREAP pArea, int iEvent,
                                int iField, PZH_ITEM pItem )
{
   ZH_BOOL fResult = ZH_TRUE;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfTriggerDo(%p,%d,%d,%p)", ( void * ) pArea, iEvent, iField, pItem ) );

   if( zh_vmRequestQuery() == 0 )
   {
      if( zh_vmRequestReenter() )
      {
         zh_vmPushDynSym( pArea->pTriggerSym );
         zh_vmPushNil();
         /* nEvent */
         zh_vmPushInteger( iEvent );
         /* nArea */
         zh_vmPushInteger( pArea->area.uiArea );
         /* nFieldPos (GET/PUT) */
         zh_vmPushInteger( iField );
         /* xTrigVal (PREUSE/GET/PUT) */
         if( pItem )
         {
#ifdef ZH_TRIGVAR_BYREF
            zh_vmPushItemRef( pItem );
#else
            zh_vmPush( pItem );
#endif
            zh_vmProc( 4 );
         }
         else
         {
            #if 0
            zh_vmPushInteger( 0 );  /* SIx3 makes this */
            #endif
            zh_vmProc( 3 );
         }
         fResult = zh_parl( -1 );
         zh_vmRequestRestore();
      }
   }

   return fResult;
}

/*
 * Set user trigger function
 */
static void zh_dbfTriggerSet( DBFAREAP pArea, PZH_ITEM pTrigger )
{
   const char * szName;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfTriggerSet(%p,%p)", ( void * ) pArea, ( void * ) pTrigger ) );

   szName = zh_itemGetCPtr( pTrigger );
   pArea->pTriggerSym = *szName ? zh_dynsymFindName( szName ) : NULL;
   if( pArea->pTriggerSym && ! zh_dynsymIsFunction( pArea->pTriggerSym ) )
      pArea->pTriggerSym = NULL;
   pArea->fTrigger = pArea->pTriggerSym != NULL;
}

/*
 * Return the total number of records.
 */
static ZH_ULONG zh_dbfCalcRecCount( DBFAREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfCalcRecCount(%p)", ( void * ) pArea ) );

   if( ! pArea->pDataFile )
      return 0;
   else
      return ( ZH_ULONG ) ( ( zh_fileSize( pArea->pDataFile ) -
                              pArea->uiHeaderLen ) / pArea->uiRecordLen );
}

/*
 * Read current record from file.
 */
static ZH_BOOL zh_dbfReadRecord( DBFAREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfReadRecord(%p)", ( void * ) pArea ) );

   if( ! pArea->pRecord )
      return ZH_FALSE;

   if( ! pArea->fPositioned )
   {
      pArea->fValidBuffer = ZH_TRUE;
      return ZH_TRUE;
   }

   if( pArea->ulRecNo > pArea->ulRecCount )
   {
      /* Update record count */
      if( pArea->fShared )
         pArea->ulRecCount = zh_dbfCalcRecCount( pArea );

      if( pArea->ulRecNo > pArea->ulRecCount )
      {
         pArea->area.fEof = pArea->fValidBuffer = ZH_TRUE;
         return ZH_TRUE;
      }
   }

   /* Read data from file */
   if( zh_fileReadAt( pArea->pDataFile, pArea->pRecord, pArea->uiRecordLen,
                      ( ZH_FOFFSET ) pArea->uiHeaderLen +
                      ( ZH_FOFFSET ) ( pArea->ulRecNo - 1 ) *
                      ( ZH_FOFFSET ) pArea->uiRecordLen ) !=
       ( ZH_SIZE ) pArea->uiRecordLen )
   {
      zh_dbfErrorRT( pArea, EG_READ, EDBF_READ,
                     pArea->szDataFileName, zh_fsError(), 0, NULL );
      return ZH_FALSE;
   }

   if( SELF_GETREC( &pArea->area, NULL ) == ZH_FAILURE )
      return ZH_FALSE;

   /* Set flags */
   pArea->fValidBuffer = pArea->fPositioned = ZH_TRUE;
   pArea->fDeleted = pArea->pRecord[ 0 ] == '*';
   return ZH_TRUE;
}

/*
 * Write current record to file.
 */
static ZH_BOOL zh_dbfWriteRecord( DBFAREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfWriteRecord(%p)", ( void * ) pArea ) );

   if( SELF_PUTREC( &pArea->area, NULL ) == ZH_FAILURE )
      return ZH_FALSE;

   pArea->fRecordChanged = ZH_FALSE;
   pArea->fDataFlush = ZH_TRUE;
   return ZH_TRUE;
}

/*
 * Set encryption password
 */
static ZH_BOOL zh_dbfPasswordSet( DBFAREAP pArea, PZH_ITEM pPasswd, ZH_BOOL fRaw )
{
   char pKeyBuffer[ 8 ];
   ZH_SIZE nLen;
   ZH_BOOL fKeySet = ZH_FALSE, fSet;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfPasswordSet(%p,%p,%d)", ( void * ) pArea, ( void * ) pPasswd, fRaw ) );

   nLen = zh_itemGetCLen( pPasswd );

   fSet = ! pArea->fHasMemo && ZH_IS_STRING( pPasswd ) && ( ! fRaw || nLen == 8 );
   if( fSet )
   {
      if( nLen > 0 )
      {
         if( nLen < 8 )
         {
            memcpy( pKeyBuffer, zh_itemGetCPtr( pPasswd ), nLen );
            memset( pKeyBuffer + nLen, '\0', 8 - nLen );
         }
         else
            memcpy( pKeyBuffer, zh_itemGetCPtr( pPasswd ), 8 );
      }
   }

   if( pArea->pCryptKey )
      zh_itemPutCL( pPasswd, pArea->pCryptKey, 8 );
   else
      zh_itemClear( pPasswd );

   if( fSet )
   {
      if( pArea->pRecord && pArea->fPositioned )
      {
         SELF_GOCOLD( &pArea->area );
         pArea->fValidBuffer = ZH_FALSE;
      }
      if( pArea->pCryptKey )
      {
         /* clean the memory with password key - though it's not
          * a serious actions in such case ;-)
          */
         memset( pArea->pCryptKey, '\0', 8 );
         zh_xfree( pArea->pCryptKey );
         pArea->pCryptKey = NULL;
      }
      if( nLen > 0 )
      {
         /* at this moment only one encryption method is used,
            I'll add other later, [druzus] */
         pArea->bCryptType = DB_CRYPT_SIX;
         pArea->pCryptKey = ( char * ) zh_xgrab( 8 );

         /* SIX encode the key with its own value before use */
         if( ! fRaw )
            zh_sxEnCrypt( pKeyBuffer, pArea->pCryptKey, pKeyBuffer, 8 );
         else
            memcpy( pArea->pCryptKey, pKeyBuffer, 8 );
         fKeySet = ZH_TRUE;
      }
   }

   return fKeySet;
}

/*
 * Encrypt/Decrypt table
 */
static void zh_dbfTableCrypt( DBFAREAP pArea, PZH_ITEM pPasswd, ZH_BOOL fEncrypt )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfTableCrypt(%p,%p,%d)", ( void * ) pArea, ( void * ) pPasswd, fEncrypt ) );

   if( ! pArea->fReadonly && ! pArea->fShared &&
       fEncrypt ? ! pArea->fTableEncrypted && ! pArea->fHasMemo :
                    pArea->fTableEncrypted )
   {
      ZH_ULONG ulRecords, ulRecNo;

      if( SELF_RECCOUNT( &pArea->area, &ulRecords ) == ZH_SUCCESS )
      {
         ZH_ERRCODE errCode = ZH_SUCCESS;
         char * pOldCryptKey, * pNewCryptKey;

         pOldCryptKey = pArea->pCryptKey;
         pArea->pCryptKey = NULL;
         zh_dbfPasswordSet( pArea, pPasswd, ZH_FALSE );
         pNewCryptKey = pArea->pCryptKey;
         if( ! fEncrypt )
         {
            if( pNewCryptKey )
            {
               if( pOldCryptKey )
                  zh_xfree( pNewCryptKey );
               else
                  pOldCryptKey = pNewCryptKey;
               pNewCryptKey = NULL;
            }
         }
         else if( ! pNewCryptKey )
            pNewCryptKey = pOldCryptKey;

         for( ulRecNo = 1; ulRecNo <= ulRecords; ++ulRecNo )
         {
            pArea->pCryptKey = pOldCryptKey;
            errCode = SELF_GOTO( &pArea->area, ulRecNo );
            if( errCode != ZH_SUCCESS )
               break;
            if( ! zh_dbfReadRecord( pArea ) )
            {
               errCode = ZH_FAILURE;
               break;
            }
            pArea->pCryptKey = pNewCryptKey;
            /* Buffer is hot? */
            if( ! pArea->fRecordChanged )
            {
               errCode = SELF_GOHOT( &pArea->area );
               if( errCode != ZH_SUCCESS )
                  break;
            }
            /* Force record encryption/decryption */
            pArea->fEncrypted = fEncrypt;
            /* Save encrypted record */
            errCode = SELF_GOCOLD( &pArea->area );
            if( errCode != ZH_SUCCESS )
               break;
         }
         pArea->pCryptKey = pNewCryptKey;
         if( pOldCryptKey && pOldCryptKey != pNewCryptKey )
            zh_xfree( pOldCryptKey );
         if( errCode == ZH_SUCCESS )
         {
            pArea->fTableEncrypted = fEncrypt;
            SELF_WRITEDBHEADER( &pArea->area );
         }
      }
   }
}

/*
 * Unlock all records.
 */
static ZH_ERRCODE zh_dbfUnlockAllRecords( DBFAREAP pArea )
{
   ZH_ERRCODE errCode = ZH_SUCCESS;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfUnlockAllRecords(%p)", ( void * ) pArea ) );

   if( pArea->pLocksPos )
   {
      ZH_ULONG ulCount;

      errCode = SELF_GOCOLD( &pArea->area );
      for( ulCount = 0; ulCount < pArea->ulNumLocksPos; ulCount++ )
         SELF_RAWLOCK( &pArea->area, REC_UNLOCK, pArea->pLocksPos[ ulCount ] );
      zh_xfree( pArea->pLocksPos );
      pArea->pLocksPos = NULL;
   }
   pArea->ulNumLocksPos = 0;
   return errCode;
}

/*
 * Unlock a records.
 */
static ZH_ERRCODE zh_dbfUnlockRecord( DBFAREAP pArea, ZH_ULONG ulRecNo )
{
   ZH_ERRCODE errCode = ZH_SUCCESS;
   ZH_ULONG ulCount;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfUnlockRecord(%p, %lu)", ( void * ) pArea, ulRecNo ) );

   /* Search the locked record */
   for( ulCount = 0; ulCount < pArea->ulNumLocksPos &&
                     pArea->pLocksPos[ ulCount ] != ulRecNo; ulCount++ ) {}

   if( ulCount < pArea->ulNumLocksPos )
   {
      errCode = SELF_GOCOLD( &pArea->area );
      SELF_RAWLOCK( &pArea->area, REC_UNLOCK, ulRecNo );
      if( pArea->ulNumLocksPos == 1 )            /* Delete the list */
      {
         zh_xfree( pArea->pLocksPos );
         pArea->pLocksPos = NULL;
         pArea->ulNumLocksPos = 0;
      }
      else                                       /* Resize the list */
      {
         ZH_ULONG * pList = pArea->pLocksPos + ulCount;
         memmove( pList, pList + 1, ( pArea->ulNumLocksPos - ulCount - 1 ) *
                  sizeof( ZH_ULONG ) );
         pArea->pLocksPos = ( ZH_ULONG * ) zh_xrealloc( pArea->pLocksPos,
                                                        ( pArea->ulNumLocksPos - 1 ) *
                                                        sizeof( ZH_ULONG ) );
         pArea->ulNumLocksPos--;
      }
   }
   return errCode;
}

/*
 * Lock a record.
 */
static ZH_ERRCODE zh_dbfLockRecord( DBFAREAP pArea, ZH_ULONG ulRecNo, ZH_USHORT * pResult,
                                    ZH_BOOL bExclusive )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfLockRecord(%p, %lu, %p, %i)", ( void * ) pArea, ulRecNo,
                            ( void * ) pResult, ( int ) bExclusive ) );

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( &pArea->area ) != ZH_SUCCESS )
         return ZH_FAILURE;
   }

   if( pArea->fFLocked )
   {
      *pResult = ZH_TRUE;
      return ZH_SUCCESS;
   }

   if( ulRecNo == 0 )
      ulRecNo = pArea->ulRecNo;

   if( bExclusive )
   {
      zh_dbfUnlockAllRecords( pArea );
   }
   else if( pArea->ulNumLocksPos > 0 )
   {
      ZH_ULONG ul;
      for( ul = 0; ul < pArea->ulNumLocksPos; ul++ )
      {
         if( pArea->pLocksPos[ ul ] == ulRecNo )
         {
            *pResult = ZH_TRUE;
            return ZH_SUCCESS;
         }
      }
   }

   if( SELF_RAWLOCK( &pArea->area, REC_LOCK, ulRecNo ) == ZH_SUCCESS )
   {
      if( pArea->ulNumLocksPos == 0 )               /* Create the list */
      {
         pArea->pLocksPos = ( ZH_ULONG * ) zh_xgrab( sizeof( ZH_ULONG ) );
      }
      else                                          /* Resize the list */
      {
         pArea->pLocksPos = ( ZH_ULONG * ) zh_xrealloc( pArea->pLocksPos,
                                                      ( pArea->ulNumLocksPos + 1 ) *
                                                        sizeof( ZH_ULONG ) );
      }
      pArea->pLocksPos[ pArea->ulNumLocksPos++ ] = ulRecNo;
      *pResult = ZH_TRUE;
      if( ulRecNo == pArea->ulRecNo )
      {
         if( ! pArea->fPositioned )
         {
            if( SELF_GOTO( &pArea->area, pArea->ulRecNo ) != ZH_SUCCESS )
               return ZH_FAILURE;
         }
         else if( ! pArea->fRecordChanged )
         {
            if( SELF_GOCOLD( &pArea->area ) != ZH_SUCCESS )
               return ZH_FAILURE;
            pArea->fValidBuffer = ZH_FALSE;
         }
      }
   }
   else
      *pResult = ZH_FALSE;
   return ZH_SUCCESS;
}

/*
 * Lock a file.
 */
static ZH_ERRCODE zh_dbfLockFile( DBFAREAP pArea, ZH_USHORT * pResult )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfLockFile(%p, %p)", ( void * ) pArea, ( void * ) pResult ) );

   if( ! pArea->fFLocked )
   {
      if( pArea->lpdbPendingRel )
      {
         if( SELF_FORCEREL( &pArea->area ) != ZH_SUCCESS )
            return ZH_FAILURE;
      }

      zh_dbfUnlockAllRecords( pArea );

      SELF_RAWLOCK( &pArea->area, FILE_LOCK, 0 );
      *pResult = ( ZH_USHORT ) pArea->fFLocked;

      if( ! pArea->fPositioned )
      {
         SELF_GOTO( &pArea->area, pArea->ulRecNo );
      }
      else if( ! pArea->fRecordChanged )
      {
         SELF_GOCOLD( &pArea->area );
         pArea->fValidBuffer = ZH_FALSE;
      }
   }
   else
      *pResult = ZH_TRUE;

   return ZH_SUCCESS;
}

/*
 * Unlock a file.
 */
static ZH_ERRCODE zh_dbfUnlockFile( DBFAREAP pArea )
{
   ZH_ERRCODE errCode = ZH_SUCCESS;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfUnlockFile(%p)", ( void * ) pArea ) );

   if( pArea->fFLocked )
   {
      errCode = SELF_GOCOLD( &pArea->area );
      SELF_RAWLOCK( &pArea->area, FILE_UNLOCK, 0 );
   }
   return errCode;
}

/*
 * Test if a record is locked.
 */
static ZH_BOOL zh_dbfIsLocked( DBFAREAP pArea, ZH_ULONG ulRecNo )
{
   ZH_ULONG ulCount;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfIsLocked(%p)", ( void * ) pArea ) );

   ulCount = pArea->ulNumLocksPos;
   while( ulCount > 0 )
   {
      if( pArea->pLocksPos[ ulCount - 1 ] == ulRecNo )
         return ZH_TRUE;
      ulCount--;
   }

   return ZH_FALSE;
}

/*
 * Return an array filled all locked records.
 */
static void zh_dbfGetLockArray( DBFAREAP pArea, PZH_ITEM pItem )
{
   ZH_ULONG ulCount;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfGetLockArray(%p, %p)", ( void * ) pArea, ( void * ) pItem ) );

   zh_arrayNew( pItem, pArea->ulNumLocksPos );
   for( ulCount = 0; ulCount < pArea->ulNumLocksPos; ulCount++ )
   {
      zh_arraySetNInt( pItem, ulCount + 1, pArea->pLocksPos[ ulCount ] );
   }
}


/*
 * Converts EDBF_* error code into EG_* one.
 * This function is common for different DBF based RDD implementation
 * so I don't make it static
 */
ZH_ERRCODE zh_dbfGetEGcode( ZH_ERRCODE errCode )
{
   ZH_ERRCODE errEGcode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfGetEGcode(%u)", errCode ) );

   switch( errCode )
   {
      case EDBF_OPEN_DBF:
         errEGcode = EG_OPEN;
         break;
      case EDBF_CREATE_DBF:
         errEGcode = EG_CREATE;
         break;
      case EDBF_READ:
         errEGcode = EG_READ;
         break;
      case EDBF_WRITE:
         errEGcode = EG_WRITE;
         break;
      case EDBF_CORRUPT:
         errEGcode = EG_CORRUPTION;
         break;
      case EDBF_DATATYPE:
         errEGcode = EG_DATATYPE;
         break;
      case EDBF_DATAWIDTH:
         errEGcode = EG_DATAWIDTH;
         break;
      case EDBF_UNLOCKED:
         errEGcode = EG_UNLOCKED;
         break;
      case EDBF_SHARED:
         errEGcode = EG_SHARED;
         break;
      case EDBF_APPENDLOCK:
         errEGcode = EG_APPENDLOCK;
         break;
      case EDBF_READONLY:
         errEGcode = EG_READONLY;
         break;
      case EDBF_LOCK:
         errEGcode = EG_LOCK;
         break;
      case EDBF_INVALIDKEY:
      default:
         errEGcode = EG_UNSUPPORTED;
         break;
   }

   return errEGcode;
}

/*
 * Converts memo block offset into ASCII.
 * This function is common for different MEMO implementation
 * so I left it in DBF.
 */
ZH_ULONG zh_dbfGetMemoBlock( DBFAREAP pArea, ZH_USHORT uiIndex )
{
   ZH_ULONG ulBlock = 0;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfGetMemoBlock(%p, %hu)", ( void * ) pArea, uiIndex ) );

   if( pArea->area.lpFields[ uiIndex ].uiLen == 4 )
   {
      ulBlock = ZH_GET_LE_UINT32( &pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ] );
   }
   else
   {
      ZH_USHORT uiCount;

      for( uiCount = 0; uiCount < 10; uiCount++ )
      {
         ZH_BYTE bByte = pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] + uiCount ];
         if( bByte >= '0' && bByte <= '9' )
         {
            ulBlock = ulBlock * 10 + ( bByte - '0' );
         }
      }
   }

   return ulBlock;
}

/*
 * Converts ASCII data into memo block offset.
 * This function is common for different MEMO implementation
 * so I left it in DBF.
 */
void zh_dbfPutMemoBlock( DBFAREAP pArea, ZH_USHORT uiIndex, ZH_ULONG ulBlock )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfPutMemoBlock(%p, %hu, %lu)", ( void * ) pArea, uiIndex, ulBlock ) );

   if( pArea->area.lpFields[ uiIndex ].uiLen == 4 )
   {
      ZH_PUT_LE_UINT32( &pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ], ulBlock );
   }
   else
   {
      ZH_SHORT iCount;

      for( iCount = 9; iCount >= 0; iCount-- )
      {
         if( ulBlock > 0 )
         {
            pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] + iCount ] = ( ZH_BYTE ) ( ulBlock % 10 ) + '0';
            ulBlock /= 10;
         }
         else
         {
            pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] + iCount ] = ' ';
         }
      }
   }
}

/*
 * Retrive memo field information stored in DBF file
 * This function is common for different MEMO implementation
 * so I left it in DBF.
 */
ZH_ERRCODE zh_dbfGetMemoData( DBFAREAP pArea, ZH_USHORT uiIndex,
                              ZH_ULONG * pulBlock, ZH_ULONG * pulSize,
                              ZH_ULONG * pulType )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfGetMemoData(%p, %hu, %p, %p, %p)", ( void * ) pArea, uiIndex, ( void * ) pulBlock, ( void * ) pulSize, ( void * ) pulType ) );

   *pulBlock = *pulSize = *pulType = 0;

   if( uiIndex >= pArea->area.uiFieldCount ||
       ( pArea->area.lpFields[ uiIndex ].uiType != ZH_FT_MEMO &&
         pArea->area.lpFields[ uiIndex ].uiType != ZH_FT_IMAGE &&
         pArea->area.lpFields[ uiIndex ].uiType != ZH_FT_BLOB &&
         pArea->area.lpFields[ uiIndex ].uiType != ZH_FT_OLE ) )
      return ZH_FAILURE;

   if( pArea->area.lpFields[ uiIndex ].uiLen == 4 )
   {
      *pulBlock = ZH_GET_LE_UINT32( &pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ] );
      return ZH_SUCCESS;
   }
   else if( pArea->area.lpFields[ uiIndex ].uiLen == 10 )
   {
      ZH_ULONG ulValue;

      if( pArea->bMemoType == DB_MEMO_SMT )
      {
         LPSMTFIELD pSMTFiled = ( LPSMTFIELD ) &pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ];

         ulValue = ZH_GET_LE_UINT16( pSMTFiled->type );
         if( ulValue != 0x2020 )
         {
            *pulType  = ulValue;
            *pulSize  = ZH_GET_LE_UINT32( pSMTFiled->length );
            *pulBlock = ZH_GET_LE_UINT32( pSMTFiled->block );
         }
      }
      /*
       * check for NULL fields created by Access, they have Chr(0) set
       * in the whole memo block address, [druzus]
       */
      else if( pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ] != 0 )
      {
         ZH_USHORT uiCount;

         ulValue = 0;
         for( uiCount = 0; uiCount < 10; uiCount++ )
         {
            ZH_BYTE bByte = pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] + uiCount ];
            if( bByte >= '0' && bByte <= '9' )
               ulValue = ulValue * 10 + ( bByte - '0' );
            else if( bByte != ' ' || ulValue )
               return zh_dbfErrorRT( pArea, EG_CORRUPTION, EDBF_CORRUPT,
                                     pArea->szDataFileName, 0,
                                     EF_CANDEFAULT, NULL ) == E_DEFAULT ?
                      ZH_SUCCESS : ZH_FAILURE;
         }
         *pulBlock = ulValue;
      }
      return ZH_SUCCESS;
   }

   return ZH_FAILURE;
}

/*
 * Write memo data information into  memo field in DBF file
 * This function is common for different MEMO implementation
 * so I left it in DBF.
 */
ZH_ERRCODE zh_dbfSetMemoData( DBFAREAP pArea, ZH_USHORT uiIndex,
                              ZH_ULONG ulBlock, ZH_ULONG ulSize,
                              ZH_ULONG ulType )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfSetMemoData(%p, %hu, %lu, %lu, %lu)", ( void * ) pArea, uiIndex, ulBlock, ulSize, ulType ) );

   if( uiIndex >= pArea->area.uiFieldCount ||
       ( pArea->area.lpFields[ uiIndex ].uiType != ZH_FT_MEMO &&
         pArea->area.lpFields[ uiIndex ].uiType != ZH_FT_IMAGE &&
         pArea->area.lpFields[ uiIndex ].uiType != ZH_FT_BLOB &&
         pArea->area.lpFields[ uiIndex ].uiType != ZH_FT_OLE ) )
      return ZH_FAILURE;

   if( pArea->area.lpFields[ uiIndex ].uiLen == 4 )
   {
      ZH_PUT_LE_UINT32( &pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ], ulBlock );
      return ZH_SUCCESS;
   }
   else if( pArea->area.lpFields[ uiIndex ].uiLen == 10 )
   {
      if( pArea->bMemoType == DB_MEMO_SMT )
      {
         LPSMTFIELD pSMTFiled = ( LPSMTFIELD ) &pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ];

         ZH_PUT_LE_UINT16( pSMTFiled->type,   ulType );
         ZH_PUT_LE_UINT32( pSMTFiled->length, ulSize );
         ZH_PUT_LE_UINT32( pSMTFiled->block,  ulBlock );
      }
      else
      {
         ZH_SHORT iCount;

         for( iCount = 9; iCount >= 0; iCount-- )
         {
            if( ulBlock > 0 )
            {
               pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] + iCount ] = ( ZH_BYTE )( ulBlock % 10 ) + '0';
               ulBlock /= 10;
            }
            else
            {
               pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] + iCount ] = ' ';
            }
         }
      }
      return ZH_SUCCESS;
   }

   return ZH_FAILURE;
}

/*
 * Get information about locking schemes for additional files (MEMO, INDEX)
 * This function is common for different MEMO implementation
 * so I left it in DBF.
 */
ZH_BOOL zh_dbfLockIdxGetData( ZH_BYTE bScheme, PZH_DBFLOCKDATA pLockData )
{
   pLockData->next = pLockData->tolock = 0;
   pLockData->type = 0;

   switch( bScheme )
   {

      case DB_DBFLOCK_COMIX:
         pLockData->offset = IDX_LOCKPOS_COMIX;
         pLockData->size   = IDX_LOCKPOOL_COMIX;
         break;

      case DB_DBFLOCK_VFP:
         pLockData->offset = IDX_LOCKPOS_VFP;
         pLockData->size   = IDX_LOCKPOOL_VFP;
         break;

      case DB_DBFLOCK_HB32:
         pLockData->offset = IDX_LOCKPOS_HB32;
         pLockData->size   = IDX_LOCKPOOL_HB32;
         break;

#ifndef ZH_LONG_LONG_OFF
      case DB_DBFLOCK_HB64:
         pLockData->offset = IDX_LOCKPOS_HB64;
         pLockData->size   = IDX_LOCKPOOL_HB64;
         break;
#endif

      default:
         pLockData->offset = pLockData->size = 0;
         return ZH_FALSE;
   }
   return ZH_TRUE;
}

static ZH_BOOL zh_dbfLockIdxRepeatFail( DBFAREAP pArea, PZH_DBFLOCKDATA pLockData )
{
   ZH_SYMBOL_UNUSED( pArea );
   ZH_SYMBOL_UNUSED( pLockData );

   /* TODO: call special error handler (LOCKHANDLER) here */

   return ZH_TRUE;
}

/*
 * Set lock using current locking schemes in additional files (MEMO, INDEX)
 * This function is common for different MEMO implementation
 * so I left it in DBF.
 */
ZH_BOOL zh_dbfLockIdxFile( DBFAREAP pArea, PZH_FILE pFile,
                           int iType, ZH_BOOL fLateWrlck,
                           PZH_DBFLOCKDATA pLockData )
{
   ZH_FOFFSET tolock;
   ZH_BOOL fOK;

   switch( iType & FL_MASK )
   {
      case FL_LOCK:
         if( ! zh_dbfLockIdxGetData( pArea->bLockType, pLockData ) )
            return ZH_FALSE;

         if( pLockData->size && ( iType & FLX_SHARED ) != 0 )
         {
            if( ++pLockData->count >= 16 )
            {
               pLockData->size = 0;
               pLockData->count = 0;
               iType &= ~FLX_SHARED;
            }
         }
         else
            pLockData->count = 0;

         tolock = 0;
         for( ;; )
         {
            ZH_FOFFSET size = 1, offset = pLockData->offset;
            if( pLockData->count != 0 )
               offset += ( ZH_FOFFSET ) ( zh_random_num() * pLockData->size ) + 1;
            else if( pLockData->size != 0 )
               size = pLockData->size + 1;
            if( zh_fileLock( pFile, offset, size,
                             size > 1 ? iType & ~FLX_WAIT : iType ) )
            {
               pLockData->offset = offset;
               pLockData->size = size;
               pLockData->tolock = tolock;
               pLockData->type = iType;
               if( ! fLateWrlck && tolock != 0 )
               {
                  if( ! zh_dbfLockIdxWrite( pArea, pFile, pLockData ) )
                  {
                     zh_fileLock( pFile, offset, size, FL_UNLOCK );
                     break;
                  }
               }
               return ZH_TRUE;
            }
            if( ( iType & FLX_WAIT ) == 0 )
               break;
            else if( size > 1 )
            {
               tolock = size - 1;
               pLockData->size = 0;
            }
            else if( ! zh_dbfLockIdxRepeatFail( pArea, pLockData ) )
               break;
            else
               zh_releaseCPU();
         }
         pLockData->offset = pLockData->size =
         pLockData->next = pLockData->tolock = 0;
         pLockData->type = 0;
         break;

      case FL_UNLOCK:
         fOK = zh_fileLock( pFile, pLockData->offset, pLockData->size, iType );
         if( pLockData->next )
         {
            if( ! zh_fileLock( pFile, pLockData->offset + pLockData->size,
                               pLockData->next, iType ) )
               fOK = ZH_FALSE;
         }
         if( fOK )
         {
            pLockData->offset = pLockData->size =
            pLockData->next = pLockData->tolock = 0;
            pLockData->type = 0;
            return ZH_TRUE;
         }
   }
   return ZH_FALSE;
}

ZH_BOOL zh_dbfLockIdxWrite( DBFAREAP pArea, PZH_FILE pFile,
                            PZH_DBFLOCKDATA pLockData )
{
   if( pLockData->tolock )
   {
      /* FL_LOCK | FLX_EXCLUSIVE | FLX_WAIT */
      while( ! zh_fileLock( pFile, pLockData->offset + pLockData->size,
                            pLockData->tolock, pLockData->type ) )
      {
         if( ! zh_dbfLockIdxRepeatFail( pArea, pLockData ) )
            return ZH_FALSE;
         zh_releaseCPU();
      }
      pLockData->next = pLockData->tolock;
      pLockData->tolock = 0;
   }
   return ZH_TRUE;
}

/*
 * Get DBF locking parameters
 */
static ZH_ERRCODE zh_dbfLockData( DBFAREAP pArea,
                                  ZH_FOFFSET * pnPos, ZH_FOFFSET * pnFlSize,
                                  ZH_FOFFSET * pnRlSize, int * iDir )
{
   switch( pArea->bLockType )
   {

      case DB_DBFLOCK_COMIX:
         *pnPos = DBF_LOCKPOS_COMIX;
         *iDir = DBF_LOCKDIR_COMIX;
         *pnFlSize = DBF_FLCKSIZE_COMIX;
         *pnRlSize = DBF_RLCKSIZE_COMIX;
         break;

      case DB_DBFLOCK_VFP:
         if( pArea->fHasTags )
         {
            *pnPos = DBF_LOCKPOS_VFPX;
            *iDir = DBF_LOCKDIR_VFPX;
            *pnFlSize = DBF_FLCKSIZE_VFPX;
            *pnRlSize = DBF_RLCKSIZE_VFPX;
         }
         else
         {
            *pnPos = DBF_LOCKPOS_VFP;
            *iDir = DBF_LOCKDIR_VFP;
            *pnFlSize = DBF_FLCKSIZE_VFP;
            *pnRlSize = DBF_RLCKSIZE_VFP;
         }
         break;

      case DB_DBFLOCK_HB32:
         *pnPos = DBF_LOCKPOS_HB32;
         *iDir = DBF_LOCKDIR_HB32;
         *pnFlSize = DBF_FLCKSIZE_HB32;
         *pnRlSize = DBF_RLCKSIZE_HB32;
         break;

#ifndef ZH_LONG_LONG_OFF
      case DB_DBFLOCK_HB64:
         *pnPos = DBF_LOCKPOS_HB64;
         *iDir = DBF_LOCKDIR_HB64;
         *pnFlSize = DBF_FLCKSIZE_HB64;
         *pnRlSize = DBF_RLCKSIZE_HB64;
         break;
#endif
      default:
         *pnPos = *pnFlSize = *pnRlSize = 0;
         *iDir = 0;
         return ZH_FAILURE;
   }
   return ZH_SUCCESS;
}

static int zh_dbfLockTest( DBFAREAP pArea, ZH_USHORT uiAction, ZH_ULONG ulRecNo )
{
   int iResult = -1;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfLockTest(%p, %hu, %lu)", ( void * ) pArea, uiAction, ulRecNo ) );

   if( ! pArea->fShared || pArea->fFLocked ||
       ( uiAction == REC_LOCK && zh_dbfIsLocked( pArea, ulRecNo ) ) )
      iResult = 0;
   else
   {
      ZH_FOFFSET nPos, nFlSize, nRlSize;
      int iDir;

      if( zh_dbfLockData( pArea, &nPos, &nFlSize, &nRlSize, &iDir ) == ZH_SUCCESS )
      {
         switch( uiAction )
         {
            case FILE_LOCK:
               if( iDir < 0 )
                  nPos -= nFlSize;
               else
                  nPos++;
               iResult = zh_fileLockTest( pArea->pDataFile, nPos, nFlSize, FL_LOCK );
               break;

            case REC_LOCK:
               if( iDir < 0 )
                  nPos -= ulRecNo;
               else if( iDir == 2 )
                  nPos += ( ulRecNo - 1 ) * pArea->uiRecordLen + pArea->uiHeaderLen;
               else
                  nPos += ulRecNo;

               iResult = zh_fileLockTest( pArea->pDataFile, nPos, nRlSize, FL_LOCK );
               break;
         }
      }
   }

   return iResult;
}

/*
 * -- DBF METHODS --
 */

/*
 * Determine logical beginning of file.
 */
static ZH_ERRCODE zh_dbfBof( DBFAREAP pArea, ZH_BOOL * pBof )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfBof(%p, %p)", ( void * ) pArea, ( void * ) pBof ) );

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( &pArea->area ) != ZH_SUCCESS )
         return ZH_FAILURE;
   }

   *pBof = pArea->area.fBof;
   return ZH_SUCCESS;
}

/*
 * Determine logical end of file.
 */
static ZH_ERRCODE zh_dbfEof( DBFAREAP pArea, ZH_BOOL * pEof )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfEof(%p, %p)", ( void * ) pArea, ( void * ) pEof ) );

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( &pArea->area ) != ZH_SUCCESS )
         return ZH_FAILURE;
   }

   *pEof = pArea->area.fEof;
   return ZH_SUCCESS;
}

/*
 * Determine outcome of the last search operation.
 */
static ZH_ERRCODE zh_dbfFound( DBFAREAP pArea, ZH_BOOL * pFound )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfFound(%p, %p)", ( void * ) pArea, ( void * ) pFound ) );

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( &pArea->area ) != ZH_SUCCESS )
         return ZH_FAILURE;
   }

   *pFound = pArea->area.fFound;
   return ZH_SUCCESS;
}

/*
 * Position cursor at the last record.
 */
static ZH_ERRCODE zh_dbfGoBottom( DBFAREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfGoBottom(%p)", ( void * ) pArea ) );

   if( SELF_GOCOLD( &pArea->area ) == ZH_FAILURE )
      return ZH_FAILURE;

   /* Update record count */
   if( pArea->fShared )
      pArea->ulRecCount = zh_dbfCalcRecCount( pArea );

   pArea->area.fTop = ZH_FALSE;
   pArea->area.fBottom = ZH_TRUE;
   if( SELF_GOTO( &pArea->area, pArea->ulRecCount ) != ZH_SUCCESS )
      return ZH_FAILURE;

   return SELF_SKIPFILTER( &pArea->area, -1 );
}

/*
 * Position cursor at a specific physical record.
 */
static ZH_ERRCODE zh_dbfGoTo( DBFAREAP pArea, ZH_ULONG ulRecNo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfGoTo(%p, %lu)", ( void * ) pArea, ulRecNo ) );

   if( SELF_GOCOLD( &pArea->area ) == ZH_FAILURE )
      return ZH_FAILURE;

   if( pArea->lpdbPendingRel )
   {
      if( pArea->lpdbPendingRel->isScoped )
      {
         if( SELF_FORCEREL( &pArea->area ) != ZH_SUCCESS )
            return ZH_FAILURE;
      }
      else /* Reset parent rel struct */
         pArea->lpdbPendingRel = NULL;
   }

   /* Update record count */
   if( ulRecNo > pArea->ulRecCount && pArea->fShared )
      pArea->ulRecCount = zh_dbfCalcRecCount( pArea );

   if( ulRecNo <= pArea->ulRecCount && ulRecNo >= 1 )
   {
      pArea->ulRecNo = ulRecNo;
      pArea->area.fBof = pArea->area.fEof = pArea->fValidBuffer = ZH_FALSE;
      pArea->fPositioned = ZH_TRUE;
   }
   else /* Out of space */
   {
      pArea->ulRecNo = pArea->ulRecCount + 1;
      pArea->area.fBof = pArea->area.fEof = pArea->fValidBuffer = ZH_TRUE;
      pArea->fPositioned = pArea->fDeleted = pArea->fEncrypted = ZH_FALSE;

      /* Clear record buffer */
      zh_dbfSetBlankRecord( pArea, ZH_BLANK_EOF );
   }
   pArea->area.fFound = ZH_FALSE;

   /* Force relational movement in child WorkAreas */
   if( pArea->area.lpdbRelations )
      return SELF_SYNCCHILDREN( &pArea->area );
   else
      return ZH_SUCCESS;
}

/*
 * Position the cursor to a specific, physical identity.
 */
static ZH_ERRCODE zh_dbfGoToId( DBFAREAP pArea, PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfGoToId(%p, %p)", ( void * ) pArea, ( void * ) pItem ) );

   if( ZH_IS_NUMERIC( pItem ) )
      return SELF_GOTO( &pArea->area, zh_itemGetNL( pItem ) );
   else
   {
      zh_dbfErrorRT( pArea, EG_DATATYPE, EDBF_DATATYPE, NULL, 0, 0, NULL );
      return ZH_FAILURE;
   }
}

/*
 * Position cursor at the first record.
 */
static ZH_ERRCODE zh_dbfGoTop( DBFAREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfGoTop(%p)", ( void * ) pArea ) );

   pArea->area.fTop = ZH_TRUE;
   pArea->area.fBottom = ZH_FALSE;

   if( SELF_GOTO( &pArea->area, 1 ) == ZH_FAILURE )
      return ZH_FAILURE;

   return SELF_SKIPFILTER( &pArea->area, 1 );
}

#define zh_dbfSeek  NULL

/*
 * Reposition cursor relative to current position.
 */
static ZH_ERRCODE zh_dbfSkip( DBFAREAP pArea, ZH_LONG lToSkip )
{
   ZH_ERRCODE errCode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfSkip(%p, %ld)", ( void * ) pArea, lToSkip ) );

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( &pArea->area ) != ZH_SUCCESS )
         return ZH_FAILURE;
   }

   pArea->area.fTop = pArea->area.fBottom = ZH_FALSE;

   if( lToSkip == 0 || pArea->area.dbfi.itmCobExpr || pArea->area.dbfi.fFilter ||
       zh_setGetDeleted() )
      return SUPER_SKIP( &pArea->area, lToSkip );

   errCode = SELF_SKIPRAW( &pArea->area, lToSkip );

   /* TODO: remove this hack - it's not necessary if SKIPRAW works
      as it should, Druzus */

   /* Move first record and set Bof flag */
   if( errCode == ZH_SUCCESS && pArea->area.fBof && lToSkip < 0 )
   {
      errCode = SELF_GOTOP( &pArea->area );
      pArea->area.fBof = ZH_TRUE;
   }

   /* Update Bof and Eof flags */
   if( lToSkip < 0 )
      pArea->area.fEof = ZH_FALSE;
   else /* if( lToSkip > 0 ) */
      pArea->area.fBof = ZH_FALSE;

   return errCode;
}

#define zh_dbfSkipFilter  NULL

/*
 * Reposition cursor, regardless of filter.
 */
static ZH_ERRCODE zh_dbfSkipRaw( DBFAREAP pArea, ZH_LONG lToSkip )
{
   ZH_ERRCODE errCode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfSkipRaw(%p, %ld)", ( void * ) pArea, lToSkip ) );

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( &pArea->area ) != ZH_SUCCESS )
         return ZH_FAILURE;
   }

   if( lToSkip == 0 )
   {
      ZH_BOOL bBof, bEof;

      /* Save flags */
      bBof = pArea->area.fBof;
      bEof = pArea->area.fEof;

      errCode = SELF_GOTO( &pArea->area, pArea->ulRecNo );

      /* Restore flags */
      pArea->area.fBof = bBof;
      pArea->area.fEof = bEof;
   }
   else if( lToSkip < 0 && ( ZH_ULONG ) ( -lToSkip ) >= pArea->ulRecNo )
   {
      errCode = SELF_GOTO( &pArea->area, 1 );
      pArea->area.fBof = ZH_TRUE;
   }
   else
   {
      errCode = SELF_GOTO( &pArea->area, pArea->ulRecNo + lToSkip );
   }

   return errCode;
}

/*
 * Add a field to the WorkArea.
 */
static ZH_ERRCODE zh_dbfAddField( DBFAREAP pArea, LPDBFIELDINFO pFieldInfo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfAddField(%p, %p)", ( void * ) pArea, ( void * ) pFieldInfo ) );

   switch( pFieldInfo->uiType )
   {
      case ZH_FT_IMAGE:
      case ZH_FT_BLOB:
      case ZH_FT_OLE:
         pFieldInfo->uiFlags |= ZH_FF_BINARY;
         /* fallthrough */
      case ZH_FT_MEMO:
         if( pArea->bMemoType == DB_MEMO_SMT )
            pFieldInfo->uiLen = 10;
         break;
   }

   /* Update field offset */
   pArea->pFieldOffset[ pArea->area.uiFieldCount ] = pArea->uiRecordLen;
   pArea->uiRecordLen += pFieldInfo->uiLen;
   if( ( pFieldInfo->uiFlags & ZH_FF_UNICODE ) != 0 )
   {
      if( pFieldInfo->uiType == ZH_FT_STRING )
         pArea->uiRecordLen += pFieldInfo->uiLen;
      else if( pFieldInfo->uiType == ZH_FT_VARLENGTH )
         pArea->uiRecordLen += pFieldInfo->uiLen + 2;
   }
   if( pArea->pFieldOffset[ pArea->area.uiFieldCount ] > pArea->uiRecordLen )
      return ZH_FAILURE;
   else
      return SUPER_ADDFIELD( &pArea->area, pFieldInfo );
}

/*
 * Append a record to the WorkArea.
 */
static ZH_ERRCODE zh_dbfAppend( DBFAREAP pArea, ZH_BOOL bUnLockAll )
{
   ZH_USHORT fLocked;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfAppend(%p, %d)", ( void * ) pArea, ( int ) bUnLockAll ) );

   if( SELF_GOCOLD( &pArea->area ) == ZH_FAILURE )
      return ZH_FAILURE;

   if( pArea->fTrigger )
   {
      if( ! zh_dbfTriggerDo( pArea, EVENT_APPEND, 0, NULL ) )
         return ZH_FAILURE;
   }

   if( pArea->fReadonly )
   {
      zh_dbfErrorRT( pArea, EG_READONLY, EDBF_READONLY, NULL, 0, 0, NULL );
      return ZH_FAILURE;
   }

   if( pArea->lpdbPendingRel )
   {
      if( pArea->lpdbPendingRel->isScoped )
      {
         if( SELF_FORCEREL( &pArea->area ) != ZH_SUCCESS )
            return ZH_FAILURE;
      }
      else /* Reset parent rel struct */
         pArea->lpdbPendingRel = NULL;
   }

   if( pArea->fShared )
   {
      fLocked = ZH_FALSE;
      if( SELF_RAWLOCK( &pArea->area, APPEND_LOCK, 0 ) == ZH_SUCCESS )
      {
         ZH_ULONG ulNewRecord;
         /* Update RecCount */
         pArea->ulRecCount = zh_dbfCalcRecCount( pArea );
         ulNewRecord = pArea->ulRecCount + 1;
         if( pArea->fFLocked || zh_dbfIsLocked( pArea, ulNewRecord ) )
            fLocked = ZH_TRUE;
         else if( zh_dbfLockRecord( pArea, ulNewRecord, &fLocked, bUnLockAll ) != ZH_SUCCESS )
         {
            if( fLocked )
               zh_dbfUnlockRecord( pArea, ulNewRecord );
            SELF_RAWLOCK( &pArea->area, APPEND_UNLOCK, 0 );
            return ZH_FAILURE;
         }
      }
      if( ! fLocked )
      {
         SELF_RAWLOCK( &pArea->area, APPEND_UNLOCK, 0 );
         zh_dbfErrorRT( pArea, EG_APPENDLOCK, EDBF_APPENDLOCK, NULL, 0,
                        EF_CANDEFAULT, NULL );
         return ZH_FAILURE;
      }
   }

   /* Clear record buffer and update pArea */
   zh_dbfSetBlankRecord( pArea, ZH_BLANK_APPEND );

   pArea->fValidBuffer = pArea->fUpdateHeader = pArea->fRecordChanged =
   pArea->fAppend = pArea->fPositioned = ZH_TRUE;
   pArea->ulRecCount ++;
   pArea->ulRecNo = pArea->ulRecCount;
   pArea->fDeleted = pArea->area.fBof = pArea->area.fEof =
   pArea->area.fFound = ZH_FALSE;
   pArea->fEncrypted = pArea->pCryptKey != NULL && ! pArea->fHasMemo;

   if( pArea->fShared )
   {
      ZH_ERRCODE errCode = SELF_GOCOLD( &pArea->area );
      SELF_RAWLOCK( &pArea->area, APPEND_UNLOCK, 0 );
      return errCode;
   }
   return ZH_SUCCESS;
}

#define zh_dbfCreateFields  NULL

/*
 * Delete a record.
 */
static ZH_ERRCODE zh_dbfDeleteRec( DBFAREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfDeleteRec(%p)", ( void * ) pArea ) );

   if( pArea->fTrigger )
   {
      if( ! zh_dbfTriggerDo( pArea, EVENT_DELETE, 0, NULL ) )
         return ZH_FAILURE;
   }

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( &pArea->area ) != ZH_SUCCESS )
         return ZH_FAILURE;
   }

   /* Read record */
   if( ! pArea->fValidBuffer && ! zh_dbfReadRecord( pArea ) )
      return ZH_FAILURE;

   if( ! pArea->fPositioned )
      return ZH_SUCCESS;

   /* Buffer is hot? */
   if( ! pArea->fRecordChanged && SELF_GOHOT( &pArea->area ) == ZH_FAILURE )
      return ZH_FAILURE;

   pArea->pRecord[ 0 ] = '*';
   pArea->fDeleted = ZH_TRUE;
   return ZH_SUCCESS;
}

/*
 * Determine deleted status for a record.
 */
static ZH_ERRCODE zh_dbfDeleted( DBFAREAP pArea, ZH_BOOL * pDeleted )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfDeleted(%p, %p)", ( void * ) pArea, ( void * ) pDeleted ) );

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( &pArea->area ) != ZH_SUCCESS )
         return ZH_FAILURE;
   }

   /* Read record */
   if( ! pArea->fValidBuffer && ! zh_dbfReadRecord( pArea ) )
      return ZH_FAILURE;

   *pDeleted = pArea->fDeleted;
   return ZH_SUCCESS;
}

#define zh_dbfFieldCount    NULL
#define zh_dbfFieldDisplay  NULL
#define zh_dbfFieldName     NULL

/*
 * Write data buffer to the data store.
 */
static ZH_ERRCODE zh_dbfFlush( DBFAREAP pArea )
{
   ZH_ERRCODE errCode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfFlush(%p)", ( void * ) pArea ) );

   errCode = SELF_GOCOLD( &pArea->area );
   if( errCode == ZH_SUCCESS )
   {
      if( pArea->fUpdateHeader && ( pArea->uiSetHeader & DB_SETHEADER_COMMIT ) != 0 )
         errCode = SELF_WRITEDBHEADER( &pArea->area );
   }

   if( zh_setGetHardCommit() && errCode == ZH_SUCCESS )
   {
      if( pArea->fDataFlush )
      {
         zh_fileCommit( pArea->pDataFile );
         pArea->fDataFlush = ZH_FALSE;
      }
      if( pArea->fHasMemo && pArea->pMemoFile && pArea->fMemoFlush )
      {
         zh_fileCommit( pArea->pMemoFile );
         pArea->fMemoFlush = ZH_FALSE;
      }
   }

   return errCode;
}

/*
 * Retrieve current record buffer
 */
static ZH_ERRCODE zh_dbfGetRec( DBFAREAP pArea, ZH_BYTE ** pBuffer )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfGetRec(%p, %p)", ( void * ) pArea, ( void * ) pBuffer ) );

   if( pBuffer != NULL )
   {
      /* Read record */
      if( ! pArea->fValidBuffer && ! zh_dbfReadRecord( pArea ) )
         return ZH_FAILURE;

      *pBuffer = pArea->pRecord;
   }
   else
   {
      if( pArea->pRecord[ 0 ] == 'D' || pArea->pRecord[ 0 ] == 'E' )
      {
         pArea->fEncrypted = ZH_TRUE;
         pArea->pRecord[ 0 ] = pArea->pRecord[ 0 ] == 'D' ? '*' : ' ';
         if( pArea->pCryptKey && pArea->bCryptType == DB_CRYPT_SIX )
         {
            zh_sxDeCrypt( ( const char * ) pArea->pRecord + 1,
                          ( char * ) pArea->pRecord + 1,
                          pArea->pCryptKey, pArea->uiRecordLen - 1 );
         }
      }
      else
      {
         pArea->fEncrypted = ZH_FALSE;
      }
   }
   return ZH_SUCCESS;
}

/*
 * Obtain the current value of a field.
 */
static ZH_ERRCODE zh_dbfGetValue( DBFAREAP pArea, ZH_USHORT uiIndex, PZH_ITEM pItem )
{
   LPFIELD pField;
   ZH_BOOL fError;
   char * pszVal;
   double dVal;
   ZH_SIZE nLen;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfGetValue(%p, %hu, %p)", ( void * ) pArea, uiIndex, ( void * ) pItem ) );

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( &pArea->area ) != ZH_SUCCESS )
         return ZH_FAILURE;
   }

   if( --uiIndex >= pArea->area.uiFieldCount )
      return ZH_FAILURE;

   /* Read record */
   if( ! pArea->fValidBuffer && ! zh_dbfReadRecord( pArea ) )
      return ZH_FAILURE;

   fError = ZH_FALSE;
   pField = pArea->area.lpFields + uiIndex;
   switch( pField->uiType )
   {
      case ZH_FT_STRING:
         nLen = pField->uiLen;
         if( ( pField->uiFlags & ZH_FF_UNICODE ) != 0 )
         {
            zh_itemPutStrLenU16( pItem, ZH_CODEPAGE_ENDIAN_LITTLE,
                                 ( const ZH_WCHAR * ) &pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ],
                                 nLen );
         }
         else if( ( pField->uiFlags & ZH_FF_BINARY ) == 0 )
         {
            pszVal = zh_cdpnDup( ( const char * ) pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                                 &nLen, pArea->area.cdPage, zh_vmCDP() );
            zh_itemPutCLPtr( pItem, pszVal, nLen );
         }
         else
         {
            pszVal = ( char * ) pArea->pRecord + pArea->pFieldOffset[ uiIndex ];
            zh_itemPutCL( pItem, pszVal, nLen );
         }
         break;

      case ZH_FT_VARLENGTH:
         nLen = pField->uiLen;
         if( ( pField->uiFlags & ZH_FF_UNICODE ) != 0 )
         {
            nLen = ZH_GET_LE_UINT16( &pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] + ( nLen << 1 ) ] );
            if( nLen == 0xFFFF ||
                nLen > ( ZH_SIZE ) pField->uiLen ) /* protection against corrupted files */
               nLen = 0;
            zh_itemPutStrLenU16( pItem, ZH_CODEPAGE_ENDIAN_LITTLE,
                                 ( const ZH_WCHAR * ) &pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ],
                                 nLen );
         }
         else
         {
            if( zh_dbfGetNullFlag( pArea, pArea->pFieldBits[ uiIndex ].uiLengthBit ) )
            {
               nLen = ( ZH_UCHAR ) pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] + nLen - 1 ];
               /* protection against corrupted files */
               if( nLen > ( ZH_SIZE ) pField->uiLen )
                  nLen = pField->uiLen;
            }
            if( ( pField->uiFlags & ZH_FF_BINARY ) == 0 )
               pszVal = zh_cdpnDup( ( const char * ) pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                                    &nLen, pArea->area.cdPage, zh_vmCDP() );
            else
               pszVal = ( char * ) pArea->pRecord + pArea->pFieldOffset[ uiIndex ];

            zh_itemPutCLPtr( pItem, pszVal, nLen );
         }
         break;

      case ZH_FT_LOGICAL:
         zh_itemPutL( pItem, pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ] == 'T' ||
                      pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ] == 't' ||
                      pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ] == 'Y' ||
                      pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ] == 'y' );
         break;

      case ZH_FT_DATE:
         if( pField->uiLen == 3 )
            zh_itemPutDL( pItem, ZH_GET_LE_UINT24( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] ) );
         else if( pField->uiLen == 4 )
            zh_itemPutDL( pItem, ZH_GET_LE_UINT32( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] ) );
         else
            zh_itemPutDS( pItem, ( char * ) pArea->pRecord + pArea->pFieldOffset[ uiIndex ] );
         break;

      case ZH_FT_TIME:
         if( pField->uiLen == 4 )
         {
            zh_itemPutTDT( pItem, 0, ZH_GET_LE_INT32( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] ) );
            break;
         }
         /* fallthrough */

      case ZH_FT_MODTIME:
      case ZH_FT_TIMESTAMP:
         zh_itemPutTDT( pItem, ZH_GET_LE_INT32( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] ),
                               ZH_GET_LE_INT32( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] + 4 ) );
         break;

      case ZH_FT_INTEGER:
      case ZH_FT_CURRENCY:
      case ZH_FT_AUTOINC:
      case ZH_FT_ROWVER:
         if( pField->uiDec )
         {
            int iLen;

            switch( pField->uiLen )
            {
               case 1:
                  dVal = ( ZH_SCHAR ) pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ];
                  iLen = 4;
                  break;
               case 2:
                  dVal = ZH_GET_LE_INT16( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] );
                  iLen = 6;
                  break;
               case 3:
                  dVal = ZH_GET_LE_INT24( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] );
                  iLen = 10;
                  break;
               case 4:
                  dVal = ZH_GET_LE_INT32( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] );
                  iLen = 10;
                  break;
               case 8:
                  dVal = ( double ) ZH_GET_LE_INT64( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] );
                  iLen = 20;
                  break;
               default:
                  dVal = 0;
                  iLen = 0;
                  fError = ZH_TRUE;
                  break;
            }
            zh_itemPutNDLen( pItem, zh_numDecConv( dVal, ( int ) pField->uiDec ),
                             iLen, ( int ) pField->uiDec );
         }
         else
         {
            switch( pField->uiLen )
            {
               case 1:
                  zh_itemPutNILen( pItem, ( ZH_SCHAR ) pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ], 4 );
                  break;
               case 2:
                  zh_itemPutNILen( pItem, ( int ) ZH_GET_LE_INT16( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] ), 6 );
                  break;
               case 3:
                  zh_itemPutNIntLen( pItem, ( ZH_MAXINT ) ZH_GET_LE_INT24( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] ), 10 );
                  break;
               case 4:
                  zh_itemPutNIntLen( pItem, ( ZH_MAXINT ) ZH_GET_LE_INT32( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] ), 10 );
                  break;
               case 8:
#ifndef ZH_LONG_LONG_OFF
                  zh_itemPutNIntLen( pItem, ( ZH_MAXINT ) ZH_GET_LE_INT64( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] ), 20 );
#else
                  zh_itemPutNLen( pItem, ( double ) ZH_GET_LE_INT64( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] ), 20, 0 );
#endif
                  break;
               default:
                  fError = ZH_TRUE;
                  break;
            }
         }
         break;

      case ZH_FT_DOUBLE:
      case ZH_FT_CURDOUBLE:
         zh_itemPutNDLen( pItem, ZH_GET_LE_DOUBLE( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] ),
                          20 - ( pField->uiDec > 0 ? ( pField->uiDec + 1 ) : 0 ),
                          ( int ) pField->uiDec );
         break;

      case ZH_FT_LONG:
      {
         ZH_MAXINT lVal;
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
      case ZH_FT_FLOAT:
         pszVal = ( char * ) pArea->pRecord + pArea->pFieldOffset[ uiIndex ];
         dVal = zh_strVal( pszVal, pField->uiLen );
         nLen = pField->uiLen;
         while( --nLen && ZH_ISDIGIT( pszVal[ nLen ] ) )
            ;
         if( nLen && ( pszVal[ nLen ] == '+' || pszVal[ nLen ] == '-' ) &&
             ( pszVal[ nLen - 1 ] == 'e' || pszVal[ nLen - 1 ] == 'E' ) )
         {
            ZH_USHORT uiLen = ( ZH_USHORT ) nLen;
            int iExp = 0;

            while( ++uiLen < pField->uiLen )
               iExp = iExp * 10 + ( pszVal[ uiLen ] - '0' );
            if( pszVal[ nLen ] == '-' )
               iExp = -iExp;
            dVal = zh_numExpConv( dVal, -iExp );
         }
         zh_itemPutNDLen( pItem, dVal,
                          ( int ) ( pField->uiLen - pField->uiDec - 1 ),
                          ( int ) pField->uiDec );
         break;

      case ZH_FT_ANY:
         if( pField->uiLen == 3 )
            zh_itemPutDL( pItem, zh_sxPtoD( ( char * ) pArea->pRecord + pArea->pFieldOffset[ uiIndex ] ) );
         else if( pField->uiLen == 4 )
            zh_itemPutNIntLen( pItem, ( ZH_MAXINT ) ZH_GET_LE_INT32( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] ), 10 );
         else
            fError = ZH_TRUE;
         break;

      case ZH_FT_MEMO:
      default:
         fError = ZH_TRUE;
         break;
   }

   /* Any error? */
   if( fError )
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

   if( pArea->fTrigger )
   {
      if( ! zh_dbfTriggerDo( pArea, EVENT_GET, uiIndex + 1, pItem ) )
         return ZH_FAILURE;
   }

   return ZH_SUCCESS;
}

/*
 * Obtain the length of a field value.
 */
static ZH_ERRCODE zh_dbfGetVarLen( DBFAREAP pArea, ZH_USHORT uiIndex, ZH_SIZE * pLength )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfGetVarLen(%p, %hu, %p)", ( void * ) pArea, uiIndex, ( void * ) pLength ) );

   *pLength = pArea->area.lpFields[ uiIndex - 1 ].uiLen;

   return ZH_SUCCESS;
}

/*
 * Perform a write of WorkArea memory to the data store.
 */
static ZH_ERRCODE zh_dbfGoCold( DBFAREAP pArea )
{
   ZH_ERRCODE errCode = ZH_SUCCESS;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfGoCold(%p)", ( void * ) pArea ) );

   if( pArea->fRecordChanged )
   {
      if( pArea->fTrigger )
      {
         /* The pending relation may move the record pointer so we should
            disable them for trigger evaluation */
         LPDBRELINFO lpdbPendingRel = pArea->lpdbPendingRel;
         pArea->lpdbPendingRel = NULL;

         zh_dbfTriggerDo( pArea, EVENT_UPDATE, 0, NULL );

         /* Restore disabled pending relation */
         pArea->lpdbPendingRel = lpdbPendingRel;
      }

      if( pArea->fModStamp )
         zh_dbfUpdateStampFields( pArea );

      /* Write current record */
      if( ! zh_dbfWriteRecord( pArea ) )
         errCode = ZH_FAILURE;
      else
      {
         if( pArea->uiSetHeader & DB_SETHEADER_REPLACE )
            pArea->fUpdateHeader = ZH_TRUE;
         pArea->fAppend = ZH_FALSE;
         if( pArea->fShared && pArea->fUpdateHeader &&
             ( pArea->uiSetHeader & DB_SETHEADER_WRITE ) != 0 )
            errCode = SELF_WRITEDBHEADER( &pArea->area );
      }
   }
   return errCode;
}

/*
 * Mark the WorkArea data buffer as hot.
 */
static ZH_ERRCODE zh_dbfGoHot( DBFAREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfGoHot(%p)", ( void * ) pArea ) );

   if( pArea->fReadonly )
   {
      zh_dbfErrorRT( pArea, EG_READONLY, EDBF_READONLY, NULL, 0, 0, NULL );
      return ZH_FAILURE;
   }
   else if( pArea->fShared && ! pArea->fFLocked &&
            ! zh_dbfIsLocked( pArea, pArea->ulRecNo ) )
   {
      zh_dbfErrorRT( pArea, EG_UNLOCKED, EDBF_UNLOCKED, NULL, 0, 0, NULL );
      return ZH_FAILURE;
   }
   pArea->fRecordChanged = ZH_TRUE;

   return ZH_SUCCESS;
}

/*
 * Replace the current record.
 */
static ZH_ERRCODE zh_dbfPutRec( DBFAREAP pArea, const ZH_BYTE * pBuffer )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfPutRec(%p, %p)", ( void * ) pArea, ( const void * ) pBuffer ) );

   if( pBuffer != NULL )
   {
      if( pArea->lpdbPendingRel )
      {
         if( SELF_FORCEREL( &pArea->area ) != ZH_SUCCESS )
            return ZH_FAILURE;
      }

      if( ! pArea->fPositioned )
         return ZH_SUCCESS;

      if( ! pArea->fRecordChanged && SELF_GOHOT( &pArea->area ) != ZH_SUCCESS )
         return ZH_FAILURE;

      /* Copy data to buffer */
      memcpy( pArea->pRecord, pBuffer, pArea->uiRecordLen );

      /*
       * TODO: such operation should be forbidden
       * maybe it will be good to return ZH_FAILURE when
       *    pArea->pRecord[ 0 ] != '*' && pArea->pRecord[ 0 ] != ' '
       */
      if( pArea->pRecord[ 0 ] == 'D' || pArea->pRecord[ 0 ] == 'E' )
      {
         if( ! pArea->fHasMemo )
            pArea->fEncrypted = ZH_TRUE;
         pArea->pRecord[ 0 ] = pArea->pRecord[ 0 ] == 'D' ? '*' : ' ';
      }

      pArea->fDeleted = pArea->pRecord[ 0 ] == '*';
   }
   else /* if( pArea->fRecordChanged ) */
   {
      ZH_BYTE * pRecord = pArea->pRecord;
      ZH_SIZE nWritten;

      if( pArea->pCryptKey )
      {
         /* This enables record encryption in update operation */
         if( pArea->bCryptType == DB_CRYPT_SIX && ! pArea->fHasMemo )
            pArea->fEncrypted = ZH_TRUE;

         if( pArea->bCryptType == DB_CRYPT_SIX && pArea->fEncrypted )
         {
            pRecord = ( ZH_BYTE * ) zh_xgrab( pArea->uiRecordLen );
            pRecord[ 0 ] = pArea->fDeleted ? 'D' : 'E';
            zh_sxEnCrypt( ( const char * ) pArea->pRecord + 1,
                          ( char * ) pRecord + 1,
                          pArea->pCryptKey, pArea->uiRecordLen - 1 );
         }
      }

      /* Write data to file */
      nWritten = zh_fileWriteAt( pArea->pDataFile, pRecord, pArea->uiRecordLen,
                                 ( ZH_FOFFSET ) pArea->uiHeaderLen +
                                 ( ZH_FOFFSET ) ( pArea->ulRecNo - 1 ) *
                                 ( ZH_FOFFSET ) pArea->uiRecordLen );
      if( pRecord != pArea->pRecord )
         zh_xfree( pRecord );

      if( nWritten != ( ZH_SIZE ) pArea->uiRecordLen )
      {
         zh_dbfErrorRT( pArea, EG_WRITE, EDBF_WRITE, pArea->szDataFileName,
                        zh_fsError(), 0, NULL );
         return ZH_FAILURE;
      }
   }
   return ZH_SUCCESS;
}

/*
 * Assign a value to a field.
 */
static ZH_ERRCODE zh_dbfPutValue( DBFAREAP pArea, ZH_USHORT uiIndex, PZH_ITEM pItem )
{
   LPFIELD pField;
   char szBuffer[ 256 ];
   const char * pszPtr;
   ZH_SIZE nSize, nLen;
   ZH_BYTE * ptr;
   ZH_ERRCODE errCode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfPutValue(%p, %hu, %p)", ( void * ) pArea, uiIndex, ( void * ) pItem ) );

   if( pArea->fTrigger )
   {
      if( ! zh_dbfTriggerDo( pArea, EVENT_PUT, uiIndex, pItem ) )
         return ZH_FAILURE;
   }

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( &pArea->area ) != ZH_SUCCESS )
         return ZH_FAILURE;
   }

   /* Read record */
   if( ! pArea->fValidBuffer && ! zh_dbfReadRecord( pArea ) )
      return ZH_FAILURE;

   if( --uiIndex >= pArea->area.uiFieldCount )
      return ZH_FAILURE;

   if( ! pArea->fPositioned )
      return ZH_SUCCESS;

   /* Buffer is hot? */
   if( ! pArea->fRecordChanged && SELF_GOHOT( &pArea->area ) == ZH_FAILURE )
      return ZH_FAILURE;

   errCode = ZH_SUCCESS;
   pField = pArea->area.lpFields + uiIndex;
   if( pField->uiType == ZH_FT_MEMO ||
       pField->uiType == ZH_FT_IMAGE ||
       pField->uiType == ZH_FT_BLOB ||
       pField->uiType == ZH_FT_OLE )
      errCode = EDBF_DATATYPE;
   else
   {
      if( ZH_IS_MEMO( pItem ) || ZH_IS_STRING( pItem ) )
      {
         nLen = pField->uiLen;
         if( pField->uiType == ZH_FT_STRING )
         {
            if( ( pField->uiFlags & ZH_FF_UNICODE ) != 0 )
            {
               ZH_WCHAR * pwBuffer = ( ZH_WCHAR * ) &pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ];
               nLen = zh_itemCopyStrU16( pItem, ZH_CODEPAGE_ENDIAN_LITTLE,
                                         pwBuffer, nLen );
               while( nLen < ( ZH_SIZE ) pField->uiLen )
               {
                  ZH_PUT_LE_UINT16( &pwBuffer[ nLen ], ' ' );
                  ++nLen;
               }
            }
            else
            {
               pszPtr = zh_itemGetCPtr( pItem );
               nSize = zh_itemGetCLen( pItem );
               if( ( pField->uiFlags & ZH_FF_BINARY ) == 0 )
               {
                  zh_cdpnDup2( pszPtr, nSize,
                               ( char * ) pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                               &nLen, zh_vmCDP(), pArea->area.cdPage );
               }
               else
               {
                  if( nLen > nSize )
                     nLen = nSize;
                  memcpy( pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                          zh_itemGetCPtr( pItem ), nLen );
               }
               if( nLen < ( ZH_SIZE ) pField->uiLen )
                  memset( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] + nLen,
                          ' ', pField->uiLen - nLen );
            }
         }
         else if( pField->uiType == ZH_FT_VARLENGTH )
         {
            if( ( pField->uiFlags & ZH_FF_UNICODE ) != 0 )
            {
               ZH_WCHAR * pwBuffer = ( ZH_WCHAR * ) &pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ];
               nLen = zh_itemCopyStrU16( pItem, ZH_CODEPAGE_ENDIAN_LITTLE,
                                         pwBuffer, nLen );
               ZH_PUT_LE_UINT16( &pwBuffer[ pField->uiLen ], nLen );
            }
            else
            {
               pszPtr = zh_itemGetCPtr( pItem );
               nSize = zh_itemGetCLen( pItem );
               if( ( pField->uiFlags & ZH_FF_BINARY ) == 0 )
               {
                  if( nLen > ( ZH_SIZE ) sizeof( szBuffer ) )
                     nLen = sizeof( szBuffer );
                  pszPtr = zh_cdpnDup2( pszPtr, nSize, szBuffer, &nLen,
                                        zh_vmCDP(), pArea->area.cdPage );
               }
               else
               {
                  if( nLen > nSize )
                     nLen = nSize;
               }
               memcpy( pArea->pRecord + pArea->pFieldOffset[ uiIndex ], pszPtr, nLen );

               if( nLen < ( ZH_SIZE ) pField->uiLen )
               {
                  pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] + pField->uiLen - 1 ] = ( ZH_BYTE ) nLen;
                  zh_dbfSetNullFlag( pArea->pRecord, pArea->uiNullOffset, pArea->pFieldBits[ uiIndex ].uiLengthBit );
               }
               else
                  zh_dbfClearNullFlag( pArea->pRecord, pArea->uiNullOffset, pArea->pFieldBits[ uiIndex ].uiLengthBit );
            }
         }
         else
            errCode = EDBF_DATATYPE;
      }
      else if( ZH_IS_DATETIME( pItem ) )
      {
         if( pField->uiType == ZH_FT_DATE )
         {
            if( pField->uiLen == 3 )
            {
               ZH_PUT_LE_UINT24( pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                                 zh_itemGetDL( pItem ) );
            }
            else if( pField->uiLen == 4 )
            {
               ZH_PUT_LE_UINT32( pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                                 zh_itemGetDL( pItem ) );
            }
            else
            {
               zh_itemGetDS( pItem, szBuffer );
               memcpy( pArea->pRecord + pArea->pFieldOffset[ uiIndex ], szBuffer, 8 );
            }
         }
         else if( pField->uiType == ZH_FT_TIMESTAMP ||
                  pField->uiType == ZH_FT_TIME ||
                  ( pField->uiType == ZH_FT_MODTIME && pArea->fTransRec ) )
         {
            long lDate, lTime;

            zh_itemGetTDT( pItem, &lDate, &lTime );
            ptr = pArea->pRecord + pArea->pFieldOffset[ uiIndex ];
            if( pField->uiType != ZH_FT_TIME )
            {
               ZH_PUT_LE_UINT32( ptr, lDate );
               ptr += 4;
            }
            ZH_PUT_LE_UINT32( ptr, lTime );
         }
         else if( pField->uiType == ZH_FT_ANY && pField->uiLen == 3 )
         {
            zh_sxDtoP( ( char * ) pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                       zh_itemGetDL( pItem ) );
         }
         else
            errCode = EDBF_DATATYPE;
      }
      else if( ZH_IS_NUMBER( pItem ) )
      {
         if( pField->uiType == ZH_FT_LONG || pField->uiType == ZH_FT_FLOAT )
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
         else if( pField->uiType == ZH_FT_INTEGER ||
                  ( pArea->fTransRec && ( pField->uiType == ZH_FT_AUTOINC ||
                                          pField->uiType == ZH_FT_ROWVER ) ) )
         {
            ZH_MAXINT lVal;
            int iSize;

            if( pField->uiDec || ZH_IS_DOUBLE( pItem ) )
            {
               double dVal;
#if 0    /* this version rounds double values to nearest integer */
               dVal = zh_numDecConv( zh_itemGetND( pItem ), -( int ) pField->uiDec );
#else    /* this one truncates double value to integer dropping fractional part */
               dVal = zh_itemGetND( pItem );
               if( pField->uiDec )
                  dVal = zh_numDecConv( dVal, -( int ) pField->uiDec );
#endif
               lVal = ( ZH_MAXINT ) dVal;
               if( ! ZH_DBL_LIM_INT64( dVal ) )
                  iSize = pField->uiLen + 1;
               else
#ifndef ZH_LONG_LONG_OFF
                  iSize = ZH_LIM_INT8( lVal ) ? 1 :
                        ( ZH_LIM_INT16( lVal ) ? 2 :
                        ( ZH_LIM_INT24( lVal ) ? 3 :
                        ( ZH_LIM_INT32( lVal ) ? 4 : 8 ) ) );
#else
                  iSize = ZH_DBL_LIM_INT8( dVal ) ? 1 :
                        ( ZH_DBL_LIM_INT16( dVal ) ? 2 :
                        ( ZH_DBL_LIM_INT24( dVal ) ? 3 :
                        ( ZH_DBL_LIM_INT32( dVal ) ? 4 : 8 ) ) );
#endif
            }
            else
            {
               lVal = ( ZH_MAXINT ) zh_itemGetNInt( pItem );
#ifdef ZH_LONG_LONG_OFF
               dVal = ( double ) lVal;
#endif
               iSize = ZH_LIM_INT8( lVal ) ? 1 :
                     ( ZH_LIM_INT16( lVal ) ? 2 :
                     ( ZH_LIM_INT24( lVal ) ? 3 :
                     ( ZH_LIM_INT32( lVal ) ? 4 : 8 ) ) );
            }

            if( iSize > pField->uiLen )
            {
               errCode = EDBF_DATAWIDTH;
            }
            else
            {
               switch( pField->uiLen )
               {
                  case 1:
                     pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] ] = ( signed char ) lVal;
                     break;
                  case 2:
                     ZH_PUT_LE_UINT16( pArea->pRecord + pArea->pFieldOffset[ uiIndex ], ( ZH_U16 ) lVal );
                     break;
                  case 3:
                     ZH_PUT_LE_UINT24( pArea->pRecord + pArea->pFieldOffset[ uiIndex ], ( ZH_U32 ) lVal );
                     break;
                  case 4:
                     ZH_PUT_LE_UINT32( pArea->pRecord + pArea->pFieldOffset[ uiIndex ], ( ZH_U32 ) lVal );
                     break;
                  case 8:
#ifndef ZH_LONG_LONG_OFF
                     ZH_PUT_LE_UINT64( pArea->pRecord + pArea->pFieldOffset[ uiIndex ], ( ZH_U64 ) lVal );
#else
                     ZH_PUT_LE_UINT64( pArea->pRecord + pArea->pFieldOffset[ uiIndex ], dVal );
#endif
                     break;
                  default:
                     errCode = EDBF_DATATYPE;
                     break;
               }
            }
         }
         else if( pField->uiType == ZH_FT_DOUBLE )
         {
            ZH_PUT_LE_DOUBLE( pArea->pRecord + pArea->pFieldOffset[ uiIndex ], zh_itemGetND( pItem ) );
         }
         else if( pField->uiType == ZH_FT_ANY && pField->uiLen == 4 )
         {
            ZH_MAXINT lVal = zh_itemGetNInt( pItem );
            if( ZH_IS_DOUBLE( pItem ) ?
                        ZH_DBL_LIM_INT32( zh_itemGetND( pItem ) ) :
                        ZH_LIM_INT32( lVal ) )
            {
               ZH_PUT_LE_UINT32( pArea->pRecord + pArea->pFieldOffset[ uiIndex ], ( ZH_U32 ) lVal );
            }
            else
            {
               errCode = EDBF_DATAWIDTH;
            }
         }
         else
         {
            errCode = EDBF_DATATYPE;
         }
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

   /* Exit if any error */
   if( errCode != ZH_SUCCESS )
   {
      PZH_ITEM pError = zh_errNew();
      zh_errPutGenCode( pError, zh_dbfGetEGcode( errCode ) );
      zh_errPutDescription( pError, zh_langDGetErrorDesc( zh_dbfGetEGcode( errCode ) ) );
      zh_errPutOperation( pError, zh_dynsymName( ( PZH_DYNS ) pField->sym ) );
      zh_errPutSubCode( pError, errCode );
      zh_errPutFlags( pError, EF_CANDEFAULT );
      zh_errPutArgs( pError, 1, pItem );
      errCode = SELF_ERROR( &pArea->area, pError );
      zh_itemRelease( pError );
      return errCode == E_DEFAULT ? ZH_SUCCESS : ZH_FAILURE;
   }
   else if( ( pField->uiFlags & ZH_FF_NULLABLE ) != 0 )
   {
      zh_dbfClearNullFlag( pArea->pRecord, pArea->uiNullOffset, pArea->pFieldBits[ uiIndex ].uiNullBit );
   }

   return ZH_SUCCESS;
}

/*
 * Undelete the current record.
 */
static ZH_ERRCODE zh_dbfRecall( DBFAREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfRecall(%p)", ( void * ) pArea ) );

   if( pArea->fTrigger )
   {
      if( ! zh_dbfTriggerDo( pArea, EVENT_RECALL, 0, NULL ) )
         return ZH_FAILURE;
   }

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( &pArea->area ) != ZH_SUCCESS )
         return ZH_FAILURE;
   }

   /* Read record */
   if( ! pArea->fValidBuffer && ! zh_dbfReadRecord( pArea ) )
      return ZH_FAILURE;

   if( ! pArea->fPositioned )
      return ZH_SUCCESS;

   /* Buffer is hot? */
   if( ! pArea->fRecordChanged && SELF_GOHOT( &pArea->area ) != ZH_SUCCESS )
      return ZH_FAILURE;

   pArea->pRecord[ 0 ] = ' ';
   pArea->fDeleted = ZH_FALSE;
   return ZH_SUCCESS;
}

/*
 * Obtain number of records in WorkArea.
 */
static ZH_ERRCODE zh_dbfRecCount( DBFAREAP pArea, ZH_ULONG * pRecCount )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfRecCount(%p, %p)", ( void * ) pArea, ( void * ) pRecCount ) );

   /* Update record count */
   if( pArea->fShared )
      pArea->ulRecCount = zh_dbfCalcRecCount( pArea );

   *pRecCount = pArea->ulRecCount;
   return ZH_SUCCESS;
}

/*
 * Obtain physical row number at current WorkArea cursor position.
 */
static ZH_ERRCODE zh_dbfRecNo( DBFAREAP pArea, ZH_ULONG * pulRecNo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfRecNo(%p, %p)", ( void * ) pArea, ( void * ) pulRecNo ) );

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( &pArea->area ) != ZH_SUCCESS )
         return ZH_FAILURE;
   }

   *pulRecNo = pArea->ulRecNo;
   return ZH_SUCCESS;
}

/*
 * Obtain physical row ID at current WorkArea cursor position.
 */
static ZH_ERRCODE zh_dbfRecId( DBFAREAP pArea, PZH_ITEM pRecNo )
{
   ZH_ERRCODE errCode;
   ZH_ULONG ulRecNo = 0;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfRecId(%p, %p)", ( void * ) pArea, ( void * ) pRecNo ) );

   errCode = SELF_RECNO( &pArea->area, &ulRecNo );
   zh_itemPutNInt( pRecNo, ulRecNo );
   return errCode;
}

/*
 * Establish the extent of the array of fields for a WorkArea.
 */
static ZH_ERRCODE zh_dbfSetFieldExtent( DBFAREAP pArea, ZH_USHORT uiFieldExtent )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfSetFieldExtent(%p, %hu)", ( void * ) pArea, uiFieldExtent ) );

   if( SUPER_SETFIELDEXTENT( &pArea->area, uiFieldExtent ) == ZH_FAILURE )
      return ZH_FAILURE;

   /* Alloc field offsets array */
   if( uiFieldExtent )
      pArea->pFieldOffset = ( ZH_USHORT * ) zh_xgrabz( uiFieldExtent * sizeof( ZH_USHORT ) );

   return ZH_SUCCESS;
}

#define zh_dbfAlias  NULL

/*
 * Close the table in the WorkArea.
 */
static ZH_ERRCODE zh_dbfClose( DBFAREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfClose(%p)", ( void * ) pArea ) );

   if( pArea->fTrigger )
   {
      if( ! zh_dbfTriggerDo( pArea, EVENT_PRECLOSE, 0, NULL ) )
         return ZH_FAILURE;
   }

   /* Reset parent rel struct */
   pArea->lpdbPendingRel = NULL;

   /* Update record and unlock records */
   if( pArea->pDataFile )
   {
      /* update buffers */
      SELF_GOCOLD( &pArea->area );

      /* Unlock all records */
      SELF_UNLOCK( &pArea->area, NULL );

      /* Update header */
      if( pArea->fUpdateHeader )
         SELF_WRITEDBHEADER( &pArea->area );

      if( zh_setGetHardCommit() )
         SELF_FLUSH( &pArea->area );
   }

   SUPER_CLOSE( &pArea->area );

   if( pArea->pDataFile )
   {
      zh_fileClose( pArea->pDataFile );
      pArea->pDataFile = NULL;

      if( pArea->fTemporary )
         zh_fileDelete( pArea->szDataFileName );
   }

   /* Close the memo file */
   if( pArea->fHasMemo && pArea->pMemoFile )
   {
      zh_fileClose( pArea->pMemoFile );
      pArea->pMemoFile = NULL;

      if( pArea->fTemporary )
         zh_fileDelete( pArea->szMemoFileName );
   }

   pArea->fTemporary = ZH_FALSE;

   /* Free field offset array */
   if( pArea->pFieldOffset )
   {
      zh_xfree( pArea->pFieldOffset );
      pArea->pFieldOffset = NULL;
   }

   /* Free field bits array */
   if( pArea->pFieldBits )
   {
      zh_xfree( pArea->pFieldBits );
      pArea->pFieldBits = NULL;
   }

   /* Free buffer */
   if( pArea->pRecord )
   {
      zh_xfree( pArea->pRecord );
      pArea->pRecord = NULL;
   }

   /* Free encryption password key */
   if( pArea->pCryptKey )
   {
      memset( pArea->pCryptKey, '\0', 8 );
      zh_xfree( pArea->pCryptKey );
      pArea->pCryptKey = NULL;
   }

   /* Free all filenames */
   if( pArea->szDataFileName )
   {
      zh_xfree( pArea->szDataFileName );
      pArea->szDataFileName = NULL;
   }
   if( pArea->szMemoFileName )
   {
      zh_xfree( pArea->szMemoFileName );
      pArea->szMemoFileName = NULL;
   }

   if( pArea->fTrigger )
   {
      zh_dbfTriggerDo( pArea, EVENT_POSTCLOSE, 0, NULL );
      pArea->fTrigger = ZH_FALSE;
   }

   return ZH_SUCCESS;
}

/*
 * Create a data store in the specified WorkArea.
 */
static ZH_ERRCODE zh_dbfCreate( DBFAREAP pArea, LPDBOPENINFO pCreateInfo )
{
   ZH_ERRCODE errCode = ZH_SUCCESS, errSubCode = 0;
   ZH_SIZE nSize;
   ZH_USHORT uiCount, uiLen;
   ZH_BOOL fRawBlob;
   DBFFIELD * pThisField;
   ZH_BYTE * pBuffer;
   PZH_FNAME pFileName;
   PZH_ITEM pItem = NULL, pError;
   char szFileName[ ZH_PATH_MAX ];

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfCreate(%p, %p)", ( void * ) pArea, ( void * ) pCreateInfo ) );

   pArea->lpdbOpenInfo = pCreateInfo;

   if( ! pArea->fTemporary )
   {
      pFileName = zh_fsFNameSplit( pCreateInfo->abName );

      if( ! pFileName->szExtension && zh_setGetDefExtension() )
      {
         pItem = zh_itemPutNil( pItem );
         if( SELF_INFO( &pArea->area, DBI_TABLEEXT, pItem ) != ZH_SUCCESS )
         {
            zh_itemRelease( pItem );
            zh_xfree( pFileName );
            pArea->lpdbOpenInfo = NULL;
            return ZH_FAILURE;
         }
         pFileName->szExtension = zh_itemGetCPtr( pItem );
         zh_fsFNameMerge( szFileName, pFileName );
      }
      else
      {
         zh_strncpy( szFileName, pCreateInfo->abName, sizeof( szFileName ) - 1 );
      }
      zh_xfree( pFileName );
   }

   pItem = zh_itemPutL( pItem, ZH_FALSE );
   fRawBlob = SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_BLOB_SUPPORT, pCreateInfo->ulConnection, pItem ) == ZH_SUCCESS &&
              zh_itemGetL( pItem );

   if( pArea->bLockType == 0 )
   {
      pItem = zh_itemPutNil( pItem );
      if( SELF_INFO( &pArea->area, DBI_LOCKSCHEME, pItem ) != ZH_SUCCESS )
      {
         zh_itemRelease( pItem );
         pArea->lpdbOpenInfo = NULL;
         return ZH_FAILURE;
      }
      pArea->bLockType = ( ZH_BYTE ) zh_itemGetNI( pItem );
      if( pArea->bLockType == 0 )
      {
         pArea->bLockType = DB_DBFLOCK_VFP;
      }
   }

   if( pArea->bTableType == DB_DBF_VFP && ! fRawBlob )
   {
      pArea->bMemoType = DB_MEMO_FPT;
   }
   else if( pArea->bMemoType == 0 )
   {
      /* get memo type */
      pItem = zh_itemPutNil( pItem );
      if( SELF_INFO( &pArea->area, DBI_MEMOTYPE, pItem ) != ZH_SUCCESS )
      {
         zh_itemRelease( pItem );
         pArea->lpdbOpenInfo = NULL;
         return ZH_FAILURE;
      }
      pArea->bMemoType = ( ZH_BYTE ) zh_itemGetNI( pItem );
   }

   pArea->bCryptType = DB_CRYPT_NONE;

   if( pItem )
      zh_itemRelease( pItem );

   nSize = ( ZH_SIZE ) pArea->area.uiFieldCount * sizeof( DBFFIELD ) +
           ( pArea->bTableType == DB_DBF_VFP ? 264 : 2 );
   if( nSize + sizeof( DBFHEADER ) > UINT16_MAX )
   {
      zh_dbfErrorRT( pArea, EG_CREATE, EDBF_DATAWIDTH, pCreateInfo->abName, 0, 0, NULL );
      pArea->lpdbOpenInfo = NULL;
      return ZH_FAILURE;
   }

   if( ! fRawBlob )
   {
      pError = NULL;
      /* Try create */
      do
      {
         if( pArea->fTemporary )
            pArea->pDataFile = zh_fileCreateTempEx( szFileName, NULL, NULL, NULL, FC_NORMAL );
         else
            pArea->pDataFile = zh_fileExtOpen( szFileName, NULL,
                                               FO_READWRITE | FO_EXCLUSIVE | FXO_TRUNCATE |
                                               FXO_DEFAULTS | FXO_SHARELOCK | FXO_COPYNAME |
                                               FXO_NOSEEKPOS, NULL, pError );
         if( pArea->pDataFile )
            break;
      }
      while( zh_dbfErrorRT( pArea, EG_CREATE, EDBF_CREATE_DBF, szFileName, zh_fsError(),
                            EF_CANRETRY | EF_CANDEFAULT, &pError ) == E_RETRY );
      if( pError )
         zh_itemRelease( pError );

      if( ! pArea->pDataFile )
      {
         pArea->lpdbOpenInfo = NULL;
         return ZH_FAILURE;
      }
   }

   pArea->szDataFileName = zh_strdup( szFileName );

   pBuffer = ( ZH_BYTE * ) zh_xgrabz( nSize + sizeof( DBFFIELD ) + 1 );
   pThisField = ( DBFFIELD * ) pBuffer;

   pArea->fHasMemo = ZH_FALSE;

   /* Size for deleted flag */
   pArea->uiRecordLen = 1;
   pArea->uiNullCount = 0;
   for( uiCount = 0; uiCount < pArea->area.uiFieldCount; uiCount++ )
   {
      LPFIELD pField = pArea->area.lpFields + uiCount;
      zh_strncpy( ( char * ) pThisField->bName,
                  zh_dynsymName( ( PZH_DYNS ) pField->sym ), sizeof( pThisField->bName ) - 1 );
      pArea->pFieldOffset[ uiCount ] = pArea->uiRecordLen;
      /* field offset */
      if( pArea->bTableType == DB_DBF_VFP )
         ZH_PUT_LE_UINT16( pThisField->bReserved1, pArea->uiRecordLen );
      pThisField->bFieldFlags = ( ZH_BYTE ) pField->uiFlags &
                                ( ZH_FF_HIDDEN | ZH_FF_NULLABLE |
                                  ZH_FF_BINARY | ZH_FF_AUTOINC );
      switch( pField->uiType )
      {
         case ZH_FT_STRING:
            if( ( pField->uiFlags & ZH_FF_UNICODE ) != 0 )
            {
               pThisField->bType = '\x1A';
               if( pField->uiLen > 32767 )
                  pField->uiLen = 32767;
               uiLen = ( pField->uiLen << 1 );
            }
            else
            {
               pThisField->bType = 'C';
               uiLen = pField->uiLen;
            }
            pThisField->bLen = ( ZH_BYTE ) uiLen;
            pThisField->bDec = ( ZH_BYTE ) ( uiLen >> 8 );
            pArea->uiRecordLen += uiLen;
            break;

         case ZH_FT_LOGICAL:
            pThisField->bType = 'L';
            pThisField->bLen = 1;
            pArea->uiRecordLen++;
            break;

         case ZH_FT_MEMO:
            pThisField->bType = ( pField->uiFlags & ZH_FF_UNICODE ) ? '\x1C' : 'M';
            if( pField->uiLen != 4 || pArea->bMemoType == DB_MEMO_SMT )
               pField->uiLen = 10;
            pThisField->bLen = ( ZH_BYTE ) pField->uiLen;
            pArea->uiRecordLen += pField->uiLen;
            pArea->fHasMemo = ZH_TRUE;
            break;

         case ZH_FT_BLOB:
            pThisField->bType = 'W';
            if( pField->uiLen != 4 || pArea->bMemoType == DB_MEMO_SMT )
               pField->uiLen = 10;
            pThisField->bLen = ( ZH_BYTE ) pField->uiLen;
            pThisField->bFieldFlags |= ZH_FF_BINARY;
            pArea->uiRecordLen += pField->uiLen;
            pArea->fHasMemo = ZH_TRUE;
            break;

         case ZH_FT_IMAGE:
            pThisField->bType = 'P';
            if( pField->uiLen != 4 || pArea->bMemoType == DB_MEMO_SMT )
               pField->uiLen = 10;
            pThisField->bLen = ( ZH_BYTE ) pField->uiLen;
            pThisField->bFieldFlags |= ZH_FF_BINARY;
            pArea->uiRecordLen += pField->uiLen;
            pArea->fHasMemo = ZH_TRUE;
            break;

         case ZH_FT_OLE:
            pThisField->bType = 'G';
            if( pField->uiLen != 4 || pArea->bMemoType == DB_MEMO_SMT )
               pField->uiLen = 10;
            pThisField->bLen = ( ZH_BYTE ) pField->uiLen;
            pThisField->bFieldFlags |= ZH_FF_BINARY;
            pArea->uiRecordLen += pField->uiLen;
            pArea->fHasMemo = ZH_TRUE;
            break;

         case ZH_FT_ANY:
            if( pArea->bTableType == DB_DBF_VFP )
               errSubCode = EDBF_DATATYPE;
            else
            {
               pThisField->bType = 'V';
               if( pField->uiLen < 3 || pField->uiLen == 5 )
               {
                  pField->uiLen = 6;
               }
               pThisField->bLen = ( ZH_BYTE ) pField->uiLen;
               pThisField->bDec = ( ZH_BYTE ) ( pField->uiLen >> 8 );
               pArea->uiRecordLen += pField->uiLen;
               if( pThisField->bLen >= 6 )
               {
                  pArea->uiMemoVersion = DB_MEMOVER_SIX;
                  pArea->fHasMemo = ZH_TRUE;
               }
            }
            break;

         case ZH_FT_DATE:
            pThisField->bType = 'D';
            if( pField->uiLen == 3 || pField->uiLen == 4 )
               pThisField->bFieldFlags |= ZH_FF_BINARY;
            else
               pField->uiLen = pThisField->bLen = 8;
            pThisField->bLen = ( ZH_BYTE ) pField->uiLen;
            pArea->uiRecordLen += pField->uiLen;
            break;

         case ZH_FT_LONG:
            pThisField->bType = 'N';
            pThisField->bLen = ( ZH_BYTE ) pField->uiLen;
            pThisField->bDec = ( ZH_BYTE ) pField->uiDec;
            if( ( pField->uiFlags & ZH_FF_AUTOINC ) != 0 )
               zh_dbfNextValueInit( pThisField, pField );
            pArea->uiRecordLen += pField->uiLen;
            break;

         case ZH_FT_FLOAT:
            pThisField->bType = 'F';
            pThisField->bLen = ( ZH_BYTE ) pField->uiLen;
            pThisField->bDec = ( ZH_BYTE ) pField->uiDec;
            if( ( pField->uiFlags & ZH_FF_AUTOINC ) != 0 )
               zh_dbfNextValueInit( pThisField, pField );
            pArea->uiRecordLen += pField->uiLen;
            break;

         case ZH_FT_DOUBLE:
         case ZH_FT_CURDOUBLE:
            pThisField->bType = 'B';
            pField->uiLen = 8;
            pThisField->bLen = ( ZH_BYTE ) pField->uiLen;
            pThisField->bDec = ( ZH_BYTE ) pField->uiDec;
            pThisField->bFieldFlags |= ZH_FF_BINARY;
            if( ( pField->uiFlags & ZH_FF_AUTOINC ) != 0 )
               zh_dbfNextValueInit( pThisField, pField );
            pArea->uiRecordLen += pField->uiLen;
            break;

         case ZH_FT_INTEGER:
         case ZH_FT_CURRENCY:
            pThisField->bType = ( pArea->bTableType == DB_DBF_VFP &&
                                  pField->uiLen == 8 && pField->uiDec == 4 ) ?
                                'Y' : 'I';
            if( ( pField->uiLen > 4 && pField->uiLen != 8 ) ||
                pField->uiLen == 0 )
            {
               pField->uiLen = 4;
            }
            pThisField->bLen = ( ZH_BYTE ) pField->uiLen;
            pThisField->bDec = ( ZH_BYTE ) pField->uiDec;
            pThisField->bFieldFlags |= ZH_FF_BINARY;
            if( ( pField->uiFlags & ZH_FF_AUTOINC ) != 0 )
               zh_dbfNextValueInit( pThisField, pField );
            pArea->uiRecordLen += pField->uiLen;
            break;

         case ZH_FT_VARLENGTH:
            if( pField->uiLen == 0 )
               pField->uiLen = 1;
            if( ( pField->uiFlags & ZH_FF_UNICODE ) != 0 )
            {
               if( pField->uiLen > 32766 )
                  pField->uiLen = 32766;
               pThisField->bType = '\x1B';
               uiLen = ( pField->uiLen + 1 ) << 1;
            }
            else
            {
               if( pField->uiLen > 255 )
                  pField->uiLen = 255;
               if( pArea->bTableType == DB_DBF_VFP && ( pField->uiFlags & ZH_FF_BINARY ) == 0 )
                  pThisField->bType = 'V';
               else
                  pThisField->bType = 'Q';
               uiLen = pField->uiLen;
            }
            pThisField->bLen = ( ZH_BYTE ) uiLen;
            pThisField->bDec = ( ZH_BYTE ) ( uiLen >> 8 );
            pArea->uiRecordLen += uiLen;
            zh_dbfAllocNullFlag( pArea, uiCount, ZH_TRUE );
            break;

         case ZH_FT_TIME:
            pThisField->bType = 'T';
            pField->uiLen = 4;
            pThisField->bLen = ( ZH_BYTE ) pField->uiLen;
            pThisField->bFieldFlags |= ZH_FF_BINARY;
            pArea->uiRecordLen += pField->uiLen;
            break;

         case ZH_FT_TIMESTAMP:
            pThisField->bType = pArea->bTableType == DB_DBF_VFP ? 'T' : '@';
            pField->uiLen = 8;
            pThisField->bLen = ( ZH_BYTE ) pField->uiLen;
            pThisField->bFieldFlags |= ZH_FF_BINARY;
            pArea->uiRecordLen += pField->uiLen;
            break;

         case ZH_FT_MODTIME:
            pThisField->bType = '=';
            pField->uiLen = 8;
            pThisField->bLen = ( ZH_BYTE ) pField->uiLen;
            pThisField->bFieldFlags |= ZH_FF_BINARY;
            pArea->uiRecordLen += pField->uiLen;
            pArea->fModStamp = ZH_TRUE;
            break;

         case ZH_FT_ROWVER:
            pThisField->bType = '^';
            pField->uiLen = 8;
            pThisField->bLen = ( ZH_BYTE ) pField->uiLen;
            pThisField->bFieldFlags |= ZH_FF_BINARY;
            #if 0
            ZH_PUT_LE_UINT64( pThisField->bReserved2, 0 );
            #endif
            pArea->uiRecordLen += pField->uiLen;
            pArea->fModStamp = ZH_TRUE;
            break;

         case ZH_FT_AUTOINC:
            pThisField->bType = '+';
            pField->uiLen = 4;
            pThisField->bLen = ( ZH_BYTE ) pField->uiLen;
            pThisField->bFieldFlags |= ZH_FF_BINARY;
            zh_dbfNextValueInit( pThisField, pField );
            pArea->uiRecordLen += pField->uiLen;
            pArea->fAutoInc = ZH_TRUE;
            break;

         default:
            errSubCode = EDBF_DATATYPE;
      }

      if( pArea->pFieldOffset[ uiCount ] > pArea->uiRecordLen )
         errSubCode = EDBF_DATAWIDTH;
      if( errSubCode != 0 )
         break;

      if( ( pField->uiFlags & ZH_FF_NULLABLE ) != 0 )
         zh_dbfAllocNullFlag( pArea, uiCount, ZH_FALSE );

      pThisField++;
   }

   if( errSubCode == 0 && pArea->uiNullCount )
   {
      zh_strncpy( ( char * ) pThisField->bName, "_NullFlags", sizeof( pThisField->bName ) - 1 );
      ZH_PUT_LE_UINT16( pThisField->bReserved1, pArea->uiRecordLen );
      pThisField->bType = '0';
      pThisField->bFieldFlags = ZH_FF_HIDDEN;
      uiCount = ( pArea->uiNullCount + 7 ) >> 3;
      pThisField->bLen = ( ZH_BYTE ) uiCount;
      pThisField->bDec = ( ZH_BYTE ) ( uiCount >> 8 );
      pArea->uiNullOffset = pArea->uiRecordLen;
      pArea->uiRecordLen += uiCount;
      nSize += sizeof( DBFFIELD );
      pThisField++;
      if( nSize + sizeof( DBFHEADER ) > UINT16_MAX || pArea->uiNullOffset > pArea->uiRecordLen )
         errSubCode = EDBF_DATAWIDTH;
   }

   if( errSubCode != 0 )
   {
      zh_xfree( pBuffer );
      SELF_CLOSE( &pArea->area );
      zh_dbfErrorRT( pArea, EG_CREATE, errSubCode, pCreateInfo->abName, 0, 0, NULL );
      pArea->lpdbOpenInfo = NULL;
      return ZH_FAILURE;
   }

   /* set end of fields marker */
   pThisField->bName[ 0 ] = '\r';

   pArea->fShared = ZH_FALSE;    /* pCreateInfo->fShared */
   pArea->fReadonly = ZH_FALSE;  /* pCreateInfo->fReadonly */
   pArea->ulRecCount = 0;
   pArea->uiHeaderLen = ( ZH_USHORT ) ( sizeof( DBFHEADER ) + nSize );
   if( fRawBlob )
   {
      pArea->fHasMemo = ZH_TRUE;
   }
   if( ! pArea->fHasMemo )
   {
      pArea->bMemoType = DB_MEMO_NONE;
   }
   pArea->ulMemoBlockSize = 0;

   if( pCreateInfo->cdpId )
   {
      pArea->area.cdPage = zh_cdpFindExt( pCreateInfo->cdpId );
      if( ! pArea->area.cdPage )
         pArea->area.cdPage = zh_vmCDP();
   }
   else
      pArea->area.cdPage = zh_vmCDP();

   pItem = zh_itemNew( NULL );
   if( SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_PENDINGPASSWORD,
                     pCreateInfo->ulConnection, pItem ) == ZH_SUCCESS )
   {
      if( zh_dbfPasswordSet( pArea, pItem, ZH_FALSE ) )
         pArea->fTableEncrypted = ZH_TRUE;
   }
   else
   {
      zh_itemClear( pItem );
      if( SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_PASSWORD,
                        pCreateInfo->ulConnection, pItem ) == ZH_SUCCESS )
      {
         if( zh_dbfPasswordSet( pArea, pItem, ZH_FALSE ) )
            pArea->fTableEncrypted = ZH_TRUE;
      }
   }
   zh_itemRelease( pItem );

   if( ! fRawBlob )
   {
      /* Write header */
      errCode = SELF_WRITEDBHEADER( &pArea->area );
      if( errCode != ZH_SUCCESS )
      {
         zh_xfree( pBuffer );
         SELF_CLOSE( &pArea->area );
         pArea->lpdbOpenInfo = NULL;
         return errCode;
      }

      /* Write fields and eof mark */
      pBuffer[ nSize ] = '\032';
      if( zh_fileWriteAt( pArea->pDataFile, pBuffer, nSize + 1,
                          sizeof( DBFHEADER ) ) != nSize + 1 )
      {
         zh_xfree( pBuffer );
         zh_dbfErrorRT( pArea, EG_WRITE, EDBF_WRITE, pArea->szDataFileName,
                        zh_fsError(), 0, NULL );
         SELF_CLOSE( &pArea->area );
         pArea->lpdbOpenInfo = NULL;
         return ZH_FAILURE;
      }
      pArea->fDataFlush = ZH_TRUE;
   }
   zh_xfree( pBuffer );

   /* Create memo file */
   if( pArea->fHasMemo )
   {
      pFileName = zh_fsFNameSplit( szFileName );
      pFileName->szExtension = NULL;
      zh_fsFNameMerge( szFileName, pFileName );
      zh_xfree( pFileName );
      pCreateInfo->abName = szFileName;
      errCode = SELF_CREATEMEMFILE( &pArea->area, pCreateInfo );
   }
   /* If successful call SUPER_CREATE to finish system jobs */
   if( errCode == ZH_SUCCESS )
      errCode = SUPER_CREATE( &pArea->area, pCreateInfo );

   if( errCode != ZH_SUCCESS )
   {
      SELF_CLOSE( &pArea->area );
      pArea->lpdbOpenInfo = NULL;
      return errCode;
   }

   /* Alloc buffer */
   pArea->pRecord = ( ZH_BYTE * ) zh_xgrab( pArea->uiRecordLen );
   pArea->fValidBuffer = ZH_FALSE;

   /* Update the number of record for corrupted headers */
   pArea->ulRecCount = zh_dbfCalcRecCount( pArea );
   pArea->lpdbOpenInfo = NULL;

   /* Position cursor at the first record */
   return SELF_GOTOP( &pArea->area );
}

/*
 * Retrieve information about the current driver.
 */
static ZH_ERRCODE zh_dbfInfo( DBFAREAP pArea, ZH_USHORT uiIndex, PZH_ITEM pItem )
{
   ZH_ERRCODE errCode = ZH_SUCCESS;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfInfo(%p, %hu, %p)", ( void * ) pArea, uiIndex, ( void * ) pItem ) );

   switch( uiIndex )
   {
      case DBI_ISDBF:
      case DBI_CANPUTREC:
         zh_itemPutL( pItem, ZH_TRUE );
         break;

      case DBI_GETHEADERSIZE:
         zh_itemPutNL( pItem, pArea->uiHeaderLen );
         break;

      case DBI_LASTUPDATE:
         zh_itemPutD( pItem, pArea->dbfHeader.bYear > 99 ?
                             1900 + pArea->dbfHeader.bYear :
                             zh_setUpdateEpoch( pArea->dbfHeader.bYear ),
                             pArea->dbfHeader.bMonth,
                             pArea->dbfHeader.bDay );
         break;

      case DBI_GETRECSIZE:
         zh_itemPutNL( pItem, pArea->uiRecordLen );
         break;

      case DBI_GETLOCKARRAY:
         zh_dbfGetLockArray( pArea, pItem );
         break;

      case DBI_TABLEEXT:
         zh_itemClear( pItem );
         return SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_TABLEEXT, 0, pItem );

      case DBI_FULLPATH:
         zh_itemPutC( pItem, pArea->szDataFileName );
         break;

      case DBI_MEMOTYPE:
         zh_itemPutNI( pItem, DB_MEMO_NONE );
         break;

      case DBI_TABLETYPE:
         if( ! pArea->pDataFile )
         {
            zh_itemClear( pItem );
            return SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_TABLETYPE, 0, pItem );
         }
         zh_itemPutNI( pItem, pArea->bTableType );
         break;

      case DBI_FILEHANDLE:
         zh_itemPutNInt( pItem, ! pArea->pDataFile ? FS_ERROR :
               ( ZH_MAXINT ) ( ZH_NHANDLE ) zh_fileHandle( pArea->pDataFile ) );
         break;

      case DBI_MEMOHANDLE:
         zh_itemPutNInt( pItem, ! pArea->pMemoFile ? FS_ERROR :
               ( ZH_MAXINT ) ( ZH_NHANDLE ) zh_fileHandle( pArea->pMemoFile ) );
         break;

      case DBI_TRANSREC:
      {
         ZH_BOOL fTransRec = pArea->fTransRec;

         if( ZH_IS_LOGICAL( pItem ) )
            pArea->fTransRec = zh_itemGetL( pItem );
         else if( ZH_IS_POINTER( pItem ) )
         {
            LPDBTRANSINFO lpdbTransInfo = zh_dbTransInfoGet( pItem );

            if( lpdbTransInfo )
            {
               if( pArea->fShared && pArea->fFLocked )
                  pArea->ulRecCount = zh_dbfCalcRecCount( pArea );

               zh_dbfTransCheckCounters( lpdbTransInfo );
               pArea->fTransRec = ZH_TRUE;
            }
         }
         zh_itemPutL( pItem, fTransRec );
         break;
      }
      case DBI_SHARED:
      {
         ZH_BOOL fShared = pArea->fShared;

         if( ZH_IS_LOGICAL( pItem ) )
            pArea->fShared = zh_itemGetL( pItem );
         zh_itemPutL( pItem, fShared );
         break;
      }
      case DBI_ISFLOCK:
         zh_itemPutL( pItem, pArea->fFLocked );
         break;

      case DBI_ISREADONLY:
         zh_itemPutL( pItem, pArea->fReadonly );
         break;

      case DBI_ISTEMPORARY:
         if( ! pArea->pDataFile && ! pArea->pMemoFile && ZH_IS_LOGICAL( pItem ) )
            pArea->fTemporary = zh_itemGetL( pItem );
         else
            zh_itemPutL( pItem, pArea->fTemporary );
         break;

      case DBI_VALIDBUFFER:
         zh_itemPutL( pItem, pArea->fValidBuffer );
         break;

      case DBI_POSITIONED:
         zh_itemPutL( pItem, pArea->fPositioned );
         break;

      case DBI_ISENCRYPTED:
         zh_itemPutL( pItem, pArea->fTableEncrypted );
         break;

      case DBI_DECRYPT:
         zh_dbfTableCrypt( pArea, pItem, ZH_FALSE );
         zh_itemPutL( pItem, ! pArea->fTableEncrypted );
         break;

      case DBI_ENCRYPT:
         zh_dbfTableCrypt( pArea, pItem, ZH_TRUE );
         zh_itemPutL( pItem, pArea->fTableEncrypted );
         break;

      case DBI_LOCKCOUNT:
         zh_itemPutNL( pItem, pArea->ulNumLocksPos );
         break;

      case DBI_LOCKOFFSET:
      {
         ZH_FOFFSET nPos, nFlSize, nRlSize;
         int iDir;

         zh_dbfLockData( pArea, &nPos, &nFlSize, &nRlSize, &iDir );
         zh_itemPutNInt( pItem, nPos );
         break;
      }

      case DBI_LOCKTEST:
         if( ZH_IS_NUMERIC( pItem ) )
            zh_itemPutNI( pItem, zh_dbfLockTest( pArea, REC_LOCK, zh_itemGetNL( pItem ) ) );
         else
            zh_itemPutNI( pItem, zh_dbfLockTest( pArea, FILE_LOCK, 0 ) );
         break;

      case DBI_LOCKSCHEME:
      {
         int iScheme = zh_itemGetNI( pItem );
         if( pArea->bLockType )
         {
            zh_itemPutNI( pItem, pArea->bLockType );
         }
         else
         {
            zh_itemClear( pItem );
            errCode = SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_LOCKSCHEME, 0, pItem );
         }
         switch( iScheme )
         {
            case DB_DBFLOCK_COMIX:
            case DB_DBFLOCK_VFP:
            case DB_DBFLOCK_HB32:
#ifndef ZH_LONG_LONG_OFF
            case DB_DBFLOCK_HB64:
#endif
               pArea->bLockType = ( ZH_BYTE ) iScheme;
         }
         break;
      }
      case DBI_SETHEADER:
      {
         ZH_UINT uiSetHeader = pArea->uiSetHeader;

         if( ZH_IS_NUMERIC( pItem ) )
         {
            int iMode = zh_itemGetNI( pItem );
            if( ( iMode & ~0xFF ) == 0 )
               pArea->uiSetHeader = iMode;
         }
         zh_itemPutNI( pItem, uiSetHeader );
         break;
      }
      case DBI_ROLLBACK:
         if( pArea->fRecordChanged )
         {
            if( pArea->fAppend )
            {
               zh_dbfSetBlankRecord( pArea, ZH_BLANK_ROLLBACK );
               pArea->fDeleted = ZH_FALSE;
            }
            else
            {
               pArea->fRecordChanged = pArea->fValidBuffer = ZH_FALSE;
            }
         }
         break;

      case DBI_PASSWORD:
         zh_dbfPasswordSet( pArea, pItem, ZH_FALSE );
         break;

      case DBI_TRIGGER:
         if( ZH_IS_LOGICAL( pItem ) )
            pArea->fTrigger = pArea->pTriggerSym && zh_itemGetL( pItem );
         else
         {
            PZH_DYNS pTriggerSym = pArea->pTriggerSym;
            if( ZH_IS_STRING( pItem ) )
               zh_dbfTriggerSet( pArea, pItem );
            zh_itemPutC( pItem, pTriggerSym ? zh_dynsymName( pTriggerSym ) : NULL );
         }
         break;

      case DBI_OPENINFO:
         zh_itemPutPtr( pItem, pArea->lpdbOpenInfo );
         break;

      case DBI_DIRTYREAD:
      {
         ZH_BOOL fDirty = ZH_DIRTYREAD( pArea );

         if( ZH_IS_LOGICAL( pItem ) )
            pArea->uiDirtyRead = zh_itemGetL( pItem ) ?
                                 ZH_IDXREAD_DIRTY : ZH_IDXREAD_CLEAN;
         else if( ! ZH_IS_NIL( pItem ) )
            pArea->uiDirtyRead = ZH_IDXREAD_DEFAULT;

         zh_itemPutL( pItem, fDirty );
         break;
      }
      case DBI_DB_VERSION:
      case DBI_RDD_VERSION:
      {
         char szBuf[ 64 ];
         int iSub = zh_itemGetNI( pItem );

         if( iSub == 1 )
            zh_snprintf( szBuf, sizeof( szBuf ), "%d.%d (%s)", 0, 1, "DBF" );
         else if( iSub == 2 )
            zh_snprintf( szBuf, sizeof( szBuf ), "%d.%d (%s:%d)", 0, 1, "DBF", pArea->area.rddID );
/*
            zh_snprintf( szBuf, sizeof( szBuf ), "%d.%d (%s:%d)", 0, 1, pArea->pRddNode->szName, pArea->area.rddID );
 */
         else
            zh_snprintf( szBuf, sizeof( szBuf ), "%d.%d", 0, 1 );
         zh_itemPutC( pItem, szBuf );
         break;
      }

      default:
         return SUPER_INFO( &pArea->area, uiIndex, pItem );

   }

   return errCode;
}

static ZH_ERRCODE zh_dbfFieldInfo( DBFAREAP pArea, ZH_USHORT uiIndex, ZH_USHORT uiType, PZH_ITEM pItem )
{
   LPFIELD pField;
   ZH_BOOL fLck;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfFieldInfo(%p, %hu, %hu, %p)", ( void * ) pArea, uiIndex, uiType, ( void * ) pItem ) );

   if( uiIndex > pArea->area.uiFieldCount )
      return ZH_FAILURE;

   switch( uiType )
   {
      case DBS_ISNULL:
         pField = pArea->area.lpFields + uiIndex - 1;
         zh_itemPutL( pItem,
            ( pField->uiFlags & ZH_FF_NULLABLE ) != 0 &&
            zh_dbfGetNullFlag( pArea, pArea->pFieldBits[ uiIndex - 1 ].uiNullBit ) );
         return ZH_SUCCESS;
      case DBS_COUNTER:
         if( zh_dbfIsAutoIncField( pArea->area.lpFields + uiIndex - 1 ) != ZH_AUTOINC_NONE )
         {
            ZH_MAXINT nValue;
            fLck = ZH_FALSE;
            if( pArea->fShared && ! pArea->fFLocked && ! pArea->fHeaderLocked )
            {
               if( SELF_RAWLOCK( &pArea->area, HEADER_LOCK, 0 ) != ZH_SUCCESS )
                  return ZH_FAILURE;
               fLck = ZH_TRUE;
            }
            if( ZH_IS_NUMERIC( pItem ) )
               nValue = zh_dbfNextValueSet( pArea, uiIndex - 1,
                                            zh_itemGetNInt( pItem ) );
            else
               nValue = zh_dbfNextValueGet( pArea, uiIndex - 1, ZH_FALSE );

            if( fLck )
               SELF_RAWLOCK( &pArea->area, HEADER_UNLOCK, 0 );
            zh_itemPutNInt( pItem, nValue );
            return ZH_SUCCESS;
         }
         zh_itemClear( pItem );
         return ZH_FAILURE;
      case DBS_STEP:
         if( zh_dbfIsAutoIncField( pArea->area.lpFields + uiIndex - 1 ) != ZH_AUTOINC_NONE )
         {
            int iValue;
            if( ZH_IS_NUMERIC( pItem ) )
            {
               fLck = ZH_FALSE;
               if( pArea->fShared && ! pArea->fFLocked && ! pArea->fHeaderLocked )
               {
                  if( SELF_RAWLOCK( &pArea->area, HEADER_LOCK, 0 ) != ZH_SUCCESS )
                     return ZH_FAILURE;
                  fLck = ZH_TRUE;
               }
               iValue = zh_dbfNextValueStep( pArea, uiIndex - 1,
                                             zh_itemGetNI( pItem ) );
               if( fLck )
                  SELF_RAWLOCK( &pArea->area, HEADER_UNLOCK, 0 );
            }
            else
               iValue = zh_dbfNextValueStep( pArea, uiIndex - 1, 0 );
            zh_itemPutNI( pItem, iValue );
            return ZH_SUCCESS;
         }
         zh_itemClear( pItem );
         return ZH_FAILURE;
      default:
         return SUPER_FIELDINFO( &pArea->area, uiIndex, uiType, pItem );
   }
}

/*
 * Retrieve information about a raw
 */
static ZH_ERRCODE zh_dbfRecInfo( DBFAREAP pArea, PZH_ITEM pRecID, ZH_USHORT uiInfoType, PZH_ITEM pInfo )
{
   ZH_ULONG ulRecNo = zh_itemGetNL( pRecID ), ulPrevRec = 0;
   ZH_ERRCODE errResult = ZH_SUCCESS;
   ZH_BOOL bDeleted;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfRecInfo(%p, %p, %hu, %p)", ( void * ) pArea, ( void * ) pRecID, uiInfoType, ( void * ) pInfo ) );

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( &pArea->area ) != ZH_SUCCESS )
         return ZH_FAILURE;
   }

   if( ulRecNo == 0 )
   {
      ulRecNo = pArea->ulRecNo;
   }
   else if( ulRecNo != pArea->ulRecNo )
   {
      switch( uiInfoType )
      {
         case DBRI_DELETED:
         case DBRI_ENCRYPTED:
         case DBRI_RAWRECORD:
         case DBRI_RAWMEMOS:
         case DBRI_RAWDATA:
            ulPrevRec = pArea->ulRecNo;
            errResult = SELF_GOTO( &pArea->area, ulRecNo );
            if( errResult != ZH_SUCCESS )
               return errResult;
            break;
      }
   }

   switch( uiInfoType )
   {
      case DBRI_DELETED:
         errResult = SELF_DELETED( &pArea->area, &bDeleted );
         if( errResult == ZH_SUCCESS )
            zh_itemPutL( pInfo, bDeleted );
         break;

      case DBRI_LOCKED:
         zh_itemPutL( pInfo, ! pArea->fShared || /* pArea->fFLocked || */
                               zh_dbfIsLocked( pArea, ulRecNo ) );
         break;

      case DBRI_RECSIZE:
         zh_itemPutNL( pInfo, pArea->uiRecordLen );
         break;

      case DBRI_RECNO:
         zh_itemPutNInt( pInfo, ulRecNo );
         break;

      case DBRI_UPDATED:
         zh_itemPutL( pInfo, ulRecNo == pArea->ulRecNo && pArea->fRecordChanged );
         break;

      case DBRI_ENCRYPTED:
         if( ! pArea->fValidBuffer && ! zh_dbfReadRecord( pArea ) )
            errResult = ZH_FAILURE;
         else
            zh_itemPutL( pInfo, pArea->fEncrypted );
         break;

      case DBRI_RAWRECORD:
         if( ! pArea->fValidBuffer && ! zh_dbfReadRecord( pArea ) )
            errResult = ZH_FAILURE;
         else
            zh_itemPutCL( pInfo, ( char * ) pArea->pRecord, pArea->uiRecordLen );
         break;

      case DBRI_RAWMEMOS:
      case DBRI_RAWDATA:
      {
         ZH_BYTE * pResult;
         ZH_SIZE nLength;

         if( ! pArea->fValidBuffer && ! zh_dbfReadRecord( pArea ) )
         {
            errResult = ZH_FAILURE;
            break;
         }
         nLength = uiInfoType == DBRI_RAWDATA ? pArea->uiRecordLen : 0;
         pResult = ( ZH_BYTE * ) zh_xgrab( nLength + 1 );
         if( nLength )
         {
            memcpy( pResult, pArea->pRecord, nLength );
         }

         if( pArea->fHasMemo )
         {
            ZH_USHORT uiFields;
            for( uiFields = 0; uiFields < pArea->area.uiFieldCount; uiFields++ )
            {
               if( pArea->area.lpFields[ uiFields ].uiType == ZH_FT_MEMO ||
                   pArea->area.lpFields[ uiFields ].uiType == ZH_FT_IMAGE ||
                   pArea->area.lpFields[ uiFields ].uiType == ZH_FT_BLOB ||
                   pArea->area.lpFields[ uiFields ].uiType == ZH_FT_OLE )
               {
                  ZH_SIZE nLen;
                  errResult = SELF_GETVALUE( &pArea->area, uiFields + 1, pInfo );
                  if( errResult != ZH_SUCCESS )
                     break;
                  nLen = zh_itemGetCLen( pInfo );
                  if( nLen > 0 )
                  {
                     pResult = ( ZH_BYTE * ) zh_xrealloc( pResult, nLength + nLen + 1 );
                     memcpy( pResult + nLength, zh_itemGetCPtr( pInfo ), nLen );
                     nLength += nLen;
                  }
               }
            }
         }
         zh_itemPutCLPtr( pInfo, ( char * ) pResult, nLength );
         break;
      }

      default:
         errResult = SUPER_RECINFO( &pArea->area, pRecID, uiInfoType, pInfo );
   }
   if( ulPrevRec != 0 )
   {
      if( SELF_GOTO( &pArea->area, ulPrevRec ) != ZH_SUCCESS &&
          errResult == ZH_SUCCESS )
         errResult = ZH_FAILURE;
   }
   return errResult;
}

/*
 * Clear the WorkArea for use.
 */
static ZH_ERRCODE zh_dbfNewArea( DBFAREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfNewArea(%p)", ( void * ) pArea ) );

   if( SUPER_NEW( &pArea->area ) == ZH_FAILURE )
      return ZH_FAILURE;

   /* set maximum field name length to 10 characters */
   pArea->area.uiMaxFieldNameLength = 10;

   pArea->pDataFile = pArea->pMemoFile = pArea->pMemoTmpFile = NULL;
   pArea->fDataFlush = pArea->fMemoFlush = ZH_FALSE;
   /* Index dirty read flag initialized to global RDD setting */
   pArea->uiDirtyRead = ZH_IDXREAD_DEFAULT;
   /* Size for deleted records flag */
   pArea->uiRecordLen = 1;
   /* DBF header update mode */
   pArea->uiSetHeader = DB_SETHEADER_APPENDSYNC;

   {
      PZH_ITEM pItem = zh_itemNew( NULL );
      if( SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_TABLETYPE, 0, pItem ) == ZH_SUCCESS )
         pArea->bTableType = ( ZH_BYTE ) zh_itemGetNI( pItem );
      zh_itemClear( pItem );
      if( SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_SETHEADER, 0, pItem ) == ZH_SUCCESS )
         pArea->uiSetHeader = ( ZH_BYTE ) zh_itemGetNI( pItem );
      zh_itemRelease( pItem );
   }

   return ZH_SUCCESS;
}

/*
 * Open a data store in the WorkArea.
 */
static ZH_ERRCODE zh_dbfOpen( DBFAREAP pArea, LPDBOPENINFO pOpenInfo )
{
   ZH_ERRCODE errCode;
   ZH_USHORT uiFields, uiCount, uiSkip, uiDecimals, uiLen, uiFlags, uiFlagsMask;
   ZH_BOOL fRawBlob;
   PZH_ITEM pError, pItem;
   PZH_FNAME pFileName;
   ZH_BYTE * pBuffer;
   LPDBFFIELD pField;
   DBFIELDINFO dbFieldInfo;
   char szFileName[ ZH_PATH_MAX ];
   char szAlias[ ZH_RDD_MAX_ALIAS_LEN + 1 ];

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfOpen(%p, %p)", ( void * ) pArea, ( void * ) pOpenInfo ) );

   pArea->lpdbOpenInfo = pOpenInfo;

   pItem = zh_itemNew( NULL );

   if( SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_PENDINGTRIGGER,
                     pOpenInfo->ulConnection, pItem ) == ZH_SUCCESS )
   {
      if( ZH_IS_STRING( pItem ) )
         zh_dbfTriggerSet( pArea, pItem );
   }

   if( ! pArea->fTrigger )
   {
      zh_itemClear( pItem );
      if( SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_TRIGGER,
                        pOpenInfo->ulConnection, pItem ) == ZH_SUCCESS )
      {
         if( ZH_IS_STRING( pItem ) )
            zh_dbfTriggerSet( pArea, pItem );
      }
   }

   if( pArea->fTrigger )
   {
      zh_itemPutC( pItem, pOpenInfo->abName );
      if( ! zh_dbfTriggerDo( pArea, EVENT_PREUSE, 0, pItem ) )
      {
         zh_itemRelease( pItem );
         pArea->lpdbOpenInfo = NULL;
         return ZH_FAILURE;
      }
      zh_strncpy( szFileName, zh_itemGetCPtr( pItem ), sizeof( szFileName ) - 1 );
   }
   else
      zh_strncpy( szFileName, pOpenInfo->abName, sizeof( szFileName ) - 1 );

   if( ! pArea->bLockType )
   {
      zh_itemClear( pItem );
      if( SELF_INFO( &pArea->area, DBI_LOCKSCHEME, pItem ) != ZH_SUCCESS )
      {
         zh_itemRelease( pItem );
         pArea->lpdbOpenInfo = NULL;
         return ZH_FAILURE;
      }
      pArea->bLockType = ( ZH_BYTE ) zh_itemGetNI( pItem );
      if( ! pArea->bLockType )
         pArea->bLockType = DB_DBFLOCK_VFP;
   }

   if( pOpenInfo->cdpId )
   {
      pArea->area.cdPage = zh_cdpFindExt( pOpenInfo->cdpId );
      if( ! pArea->area.cdPage )
         pArea->area.cdPage = zh_vmCDP();
   }
   else
      pArea->area.cdPage = zh_vmCDP();

   pArea->fShared = pOpenInfo->fShared;
   pArea->fReadonly = pOpenInfo->fReadonly;
   /* Force exclusive mode
    *   0: AUTOSHARE disabled.
    *   1: AUTOSHARE enabled.
    *   2: force exclusive mode.
    * */
   if( zh_setGetAutoShare() == 2 )
      pArea->fShared = ZH_FALSE;
   uiFlags = ( pArea->fReadonly ? FO_READ : FO_READWRITE ) |
             ( pArea->fShared ? FO_DENYNONE : FO_EXCLUSIVE );
   pError = NULL;

   pFileName = zh_fsFNameSplit( szFileName );
   /* Add default file name extension if necessary */
   if( ! pFileName->szExtension && zh_setGetDefExtension() )
   {
      zh_itemClear( pItem );
      if( SELF_INFO( &pArea->area, DBI_TABLEEXT, pItem ) != ZH_SUCCESS )
      {
         zh_xfree( pFileName );
         zh_itemRelease( pItem );
         pArea->lpdbOpenInfo = NULL;
         return ZH_FAILURE;
      }
      pFileName->szExtension = zh_itemGetCPtr( pItem );
      zh_fsFNameMerge( szFileName, pFileName );
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

   zh_itemClear( pItem );
   fRawBlob = SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_BLOB_SUPPORT, pOpenInfo->ulConnection, pItem ) == ZH_SUCCESS &&
              zh_itemGetL( pItem );
   zh_itemClear( pItem );
   uiDecimals = ( ZH_USHORT ) ( SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_DECIMALS, pOpenInfo->ulConnection, pItem ) == ZH_SUCCESS ?
                                zh_itemGetNI( pItem ) : 0 );
   zh_itemRelease( pItem );
   uiFlagsMask = 0;

   if( fRawBlob )
   {
      uiFields = uiSkip = 0;
      pBuffer = NULL;
      pArea->fHasMemo = ZH_TRUE;
   }
   else
   {
      ZH_ERRCODE errOsCode;
      ZH_SIZE nSize;

      /* Try open */
      do
      {
         pArea->pDataFile = zh_fileExtOpen( szFileName, NULL, uiFlags |
                                            FXO_DEFAULTS | FXO_SHARELOCK |
                                            FXO_COPYNAME | FXO_NOSEEKPOS,
                                            NULL, pError );
         if( pArea->pDataFile )
            break;
      }
      while( zh_dbfErrorRT( pArea, EG_OPEN, EDBF_OPEN_DBF, szFileName, zh_fsError(),
                            EF_CANRETRY | EF_CANDEFAULT, &pError ) == E_RETRY );

      if( pError )
      {
         zh_itemRelease( pError );
         pError = NULL;
      }

      /* Exit if error */
      if( ! pArea->pDataFile )
      {
         SELF_CLOSE( &pArea->area );
         pArea->lpdbOpenInfo = NULL;
         return ZH_FAILURE;
      }

      /* Allocate only after successfully open file */
      pArea->szDataFileName = zh_strdup( szFileName );

      /* Read file header and exit if error */
      errCode = SELF_READDBHEADER( &pArea->area );
      if( errCode != ZH_SUCCESS )
      {
         SELF_CLOSE( &pArea->area );
         pArea->lpdbOpenInfo = NULL;
         return errCode;
      }

      /* Add fields */
      uiSkip = 0;
      uiFields = ( pArea->uiHeaderLen - sizeof( DBFHEADER ) ) / sizeof( DBFFIELD );
      nSize = ( ZH_SIZE ) uiFields * sizeof( DBFFIELD );
      pBuffer = uiFields ? ( ZH_BYTE * ) zh_xgrab( nSize ) : NULL;

      /* Read fields and exit if error */
      do
      {
         if( zh_fileReadAt( pArea->pDataFile, pBuffer, nSize,
                            sizeof( DBFHEADER ) ) == nSize )
         {
            errCode = ZH_SUCCESS;
            break;
         }
         errOsCode = zh_fsError();
         errCode = ZH_FAILURE;
      }
      while( zh_dbfErrorRT( pArea, errOsCode == 0 ? EG_CORRUPTION : EG_READ,
                                   errOsCode == 0 ? EDBF_CORRUPT : EDBF_READ,
                            pArea->szDataFileName, errOsCode,
                            EF_CANRETRY | EF_CANDEFAULT, &pError ) == E_RETRY );
      if( pError )
         zh_itemRelease( pError );

      /* Exit if error */
      if( errCode != ZH_SUCCESS )
      {
         if( pBuffer )
            zh_xfree( pBuffer );
         SELF_CLOSE( &pArea->area );
         pArea->lpdbOpenInfo = NULL;
         return errCode;
      }

      uiFlags = ZH_FF_HIDDEN | ZH_FF_NULLABLE | ZH_FF_BINARY | ZH_FF_AUTOINC;
      if( pArea->bTableType == DB_DBF_VFP )
         uiFlagsMask = uiFlags;

      /* some RDDs use the additional space in the header after field array
       * for private data we should check for 0x0D marker to not use this
       * data as fields description.
       */
      for( uiCount = 0; uiCount < uiFields; uiCount++ )
      {
         pField = ( LPDBFFIELD ) ( pBuffer + uiCount * sizeof( DBFFIELD ) );

         if( uiFlagsMask == 0 )
         {
            switch( pField->bType )
            {
               case 'L':
               case 'D':
                  if( pField->bFieldFlags & ~ZH_FF_NULLABLE )
                  {
                     uiFlags = 0;
                     break;
                  }
                  /* fallthrough */
               case 'N':
                  if( pField->bFieldFlags & ~( ZH_FF_NULLABLE | ZH_FF_AUTOINC ) )
                  {
                     uiFlags = 0;
                     break;
                  }
                  else if( ( pField->bFieldFlags & ZH_FF_AUTOINC ) != 0 )
                  {
                     if( ZH_GET_LE_UINT32( pField->bReserved1 ) != 0 ||
                         ( pField->bLen - ( pField->bDec ? pField->bDec + 1 : 0 ) > 9 ?
                           ZH_GET_LE_UINT32( pField->bCounter ) != 0 :
                           ( ( uiCount > 0 && ZH_GET_LE_UINT32( pField->bReserved2 ) != 0 ) ||
                             ZH_GET_LE_UINT32( &pField->bReserved2[ 4 ] ) != 0 ) ) )
                        uiFlags = 0;
                     break;
                  }
                  /* fallthrough */
               case 'C':
               case 'M':
               case 'V':
                  if( ZH_GET_LE_UINT32( pField->bReserved1 ) != 0 ||
                      ( uiCount > 0 && ZH_GET_LE_UINT32( pField->bReserved2 ) != 0 ) ||
                      ZH_GET_LE_UINT32( &pField->bReserved2[ 4 ] ) != 0 ||
                      ZH_GET_LE_UINT32( pField->bCounter ) != 0 ||
                      pField->bStep != 0 ||
                      ( pField->bFieldFlags & ~( ZH_FF_NULLABLE | ZH_FF_BINARY ) ) != 0 )
                     uiFlags = 0;
                  break;
               default:
                  uiFlagsMask = ZH_FF_HIDDEN | ZH_FF_NULLABLE |
                                ZH_FF_BINARY | ZH_FF_AUTOINC;
            }
         }

         if( pField->bName[ 0 ] == 0x0d )
         {
            uiFields = uiCount;
            break;
         }
         else if( ( pField->bFieldFlags & 0x01 ) != 0 &&
                  ( pField->bType == '0' || pArea->bTableType == DB_DBF_VFP ) )
         {
            uiSkip++;
         }
      }
      uiFlagsMask |= uiFlags;
      uiFields -= uiSkip;
   }


   {
      errCode = SELF_SETFIELDEXTENT( &pArea->area, uiFields );
      if( errCode != ZH_SUCCESS )
      {
         SELF_CLOSE( &pArea->area );
         pArea->lpdbOpenInfo = NULL;
         return errCode;
      }
   }

   /* Clear dbFieldInfo structure */
   memset( &dbFieldInfo, 0, sizeof( dbFieldInfo ) );

   /* Size for deleted flag */
   pArea->uiRecordLen = 1;
   pArea->uiNullCount = 0;
   for( uiCount = 0; uiCount < uiFields + uiSkip; uiCount++ )
   {
      pField = ( LPDBFFIELD ) ( pBuffer + uiCount * sizeof( DBFFIELD ) );
      pField->bName[ 10 ] = '\0';
      #if 0
      zh_strupp( ( char * ) pField->bName );
      #endif
      dbFieldInfo.atomName = ( const char * ) pField->bName;
      dbFieldInfo.uiLen = pField->bLen;
      dbFieldInfo.uiDec = 0;
      dbFieldInfo.uiTypeExtended = 0;
      dbFieldInfo.uiFlags = pField->bFieldFlags & uiFlagsMask;

      switch( pField->bType )
      {
         case 'C':
            dbFieldInfo.uiType = ZH_FT_STRING;
            dbFieldInfo.uiLen = pField->bLen + pField->bDec * 256;
            break;

         case 'L':
            dbFieldInfo.uiType = ZH_FT_LOGICAL;
            dbFieldInfo.uiLen = 1;
            break;

         case 'D':
            dbFieldInfo.uiType = ZH_FT_DATE;
            if( dbFieldInfo.uiLen != 3 && dbFieldInfo.uiLen != 4 )
               dbFieldInfo.uiLen = 8;
            break;

         case 'I':
            dbFieldInfo.uiType = ZH_FT_INTEGER;
            if( ( dbFieldInfo.uiLen > 4 && dbFieldInfo.uiLen != 8 ) ||
                dbFieldInfo.uiLen == 0 )
               dbFieldInfo.uiLen = 4;
            dbFieldInfo.uiDec = pField->bDec;
            break;

         case 'Y':
            dbFieldInfo.uiType = ZH_FT_CURRENCY;
            if( ( dbFieldInfo.uiLen > 4 && dbFieldInfo.uiLen != 8 ) ||
                dbFieldInfo.uiLen == 0 )
               dbFieldInfo.uiLen = 8;
            dbFieldInfo.uiDec = pField->bDec;
            break;

         case '2':
         case '4':
            dbFieldInfo.uiType = ZH_FT_INTEGER;
            dbFieldInfo.uiLen = pField->bType - '0';
            break;

         case 'N':
            dbFieldInfo.uiType = ZH_FT_LONG;
            dbFieldInfo.uiDec = pField->bDec;
            break;

         case 'F':
            dbFieldInfo.uiType = ZH_FT_FLOAT;
            dbFieldInfo.uiDec = pField->bDec;
            /* See note above */
            break;

         case '8':
         case 'B':
            dbFieldInfo.uiType = ZH_FT_DOUBLE;
            dbFieldInfo.uiDec = pField->bDec;
            if( dbFieldInfo.uiLen != 8 )
               errCode = ZH_FAILURE;
            else if( dbFieldInfo.uiDec == 0 )
               dbFieldInfo.uiDec = uiDecimals;
            break;

         case 'T':
            if( dbFieldInfo.uiLen == 8 )
               dbFieldInfo.uiType = ZH_FT_TIMESTAMP;
            else if( dbFieldInfo.uiLen == 4 )
               dbFieldInfo.uiType = ZH_FT_TIME;
            else
               errCode = ZH_FAILURE;
            break;

         /* types which are not supported by VM - mapped to different ones */
         case '@':
            dbFieldInfo.uiType = ZH_FT_TIMESTAMP;
            if( dbFieldInfo.uiLen != 8 )
               errCode = ZH_FAILURE;
            break;

         case '=':
            dbFieldInfo.uiType = ZH_FT_MODTIME;
            if( dbFieldInfo.uiLen != 8 )
               errCode = ZH_FAILURE;
            pArea->fModStamp = ZH_TRUE;
            break;

         case '^':
            dbFieldInfo.uiType = ZH_FT_ROWVER;
            if( dbFieldInfo.uiLen != 8 )
               errCode = ZH_FAILURE;
            pArea->fModStamp = ZH_TRUE;
            break;

         case '+':
            dbFieldInfo.uiType = ZH_FT_AUTOINC;
            if( dbFieldInfo.uiLen != 4 )
               errCode = ZH_FAILURE;
            pArea->fAutoInc = ZH_TRUE;
            break;

         case 'Q':
            dbFieldInfo.uiType = ZH_FT_VARLENGTH;
            if( pArea->bTableType == DB_DBF_VFP )
               dbFieldInfo.uiFlags |= ZH_FF_BINARY;
            else
               dbFieldInfo.uiFlags |= ZH_FF_BINARY & pField->bFieldFlags;
            zh_dbfAllocNullFlag( pArea, uiCount, ZH_TRUE );
            break;

         case 'V':
            if( pArea->bTableType == DB_DBF_VFP )
            {
               dbFieldInfo.uiType = ZH_FT_VARLENGTH;
               #if 0
               dbFieldInfo.uiFlags &= ~ZH_FF_BINARY;
               #endif
               zh_dbfAllocNullFlag( pArea, uiCount, ZH_TRUE );
            }
            else
            {
               dbFieldInfo.uiType = ZH_FT_ANY;
               if( dbFieldInfo.uiLen >= 6 )
               {
                  pArea->uiMemoVersion = DB_MEMOVER_SIX;
                  pArea->fHasMemo = ZH_TRUE;
               }
            }
            break;

         case 'M':
            dbFieldInfo.uiType = ZH_FT_MEMO;
            pArea->fHasMemo = ZH_TRUE;
            break;

         case 'P':
            dbFieldInfo.uiType = ZH_FT_IMAGE;
            dbFieldInfo.uiFlags |= ZH_FF_BINARY;
            pArea->fHasMemo = ZH_TRUE;
            break;

         case 'W':
            dbFieldInfo.uiType = ZH_FT_BLOB;
            dbFieldInfo.uiFlags |= ZH_FF_BINARY;
            pArea->fHasMemo = ZH_TRUE;
            break;

         case 'G':
            dbFieldInfo.uiType = ZH_FT_OLE;
            dbFieldInfo.uiFlags |= ZH_FF_BINARY;
            pArea->fHasMemo = ZH_TRUE;
            break;

         case '\x1A':
            dbFieldInfo.uiType = ZH_FT_STRING;
            dbFieldInfo.uiFlags |= ZH_FF_UNICODE;
            uiLen = pField->bLen + pField->bDec * 256;
            if( uiLen & 1 )
               errCode = ZH_FAILURE;
            dbFieldInfo.uiLen = uiLen >> 1;
            break;

         case '\x1B':
            dbFieldInfo.uiType = ZH_FT_VARLENGTH;
            dbFieldInfo.uiFlags |= ZH_FF_UNICODE;
            uiLen = pField->bLen + pField->bDec * 256;
            if( uiLen & 1 || uiLen < 2 )
               errCode = ZH_FAILURE;
            dbFieldInfo.uiLen = ( uiLen >> 1 ) - 1;
            break;

         case '\x1C':
            dbFieldInfo.uiType = ZH_FT_MEMO;
            dbFieldInfo.uiFlags |= ZH_FF_UNICODE;
            pArea->fHasMemo = ZH_TRUE;
            break;

         case '0':
            if( ( pField->bFieldFlags & ZH_FF_HIDDEN ) != 0 )
            {
               if( memcmp( dbFieldInfo.atomName, "_NullFlags", 10 ) == 0 )
                  pArea->uiNullOffset = pArea->uiRecordLen;
               pArea->uiRecordLen += dbFieldInfo.uiLen;
               if( pArea->uiRecordLen >= dbFieldInfo.uiLen )
                  continue;
            }
            /* fallthrough */

         default:
            errCode = ZH_FAILURE;
            break;
      }

      if( errCode == ZH_SUCCESS )
      {
         if( ( dbFieldInfo.uiFlags & ZH_FF_NULLABLE ) != 0 )
            zh_dbfAllocNullFlag( pArea, uiCount, ZH_FALSE );
         /* Add field */
         errCode = SELF_ADDFIELD( &pArea->area, &dbFieldInfo );
      }

      /* Exit if error */
      if( errCode != ZH_SUCCESS )
         break;
   }
   if( pBuffer )
      zh_xfree( pBuffer );

   if( pArea->uiNullCount > 0 && pArea->uiNullOffset == 0 )
      errCode = ZH_FAILURE;

   /* Exit if error */
   if( errCode != ZH_SUCCESS )
   {
      zh_dbfErrorRT( pArea, EG_CORRUPTION, EDBF_CORRUPT, pArea->szDataFileName,
                     0, EF_CANDEFAULT, NULL );
      SELF_CLOSE( &pArea->area );
      pArea->lpdbOpenInfo = NULL;
      return errCode;
   }

   pItem = zh_itemNew( NULL );
   if( SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_PENDINGPASSWORD,
                     pOpenInfo->ulConnection, pItem ) == ZH_SUCCESS )
   {
      zh_dbfPasswordSet( pArea, pItem, ZH_FALSE );
   }
   else
   {
      zh_itemClear( pItem );
      if( SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_PASSWORD,
                        pOpenInfo->ulConnection, pItem ) == ZH_SUCCESS )
      {
         zh_dbfPasswordSet( pArea, pItem, ZH_FALSE );
      }
   }
   zh_itemRelease( pItem );

   /* Open memo file if exists */
   if( pArea->fHasMemo )
   {
      pFileName = zh_fsFNameSplit( szFileName );
      pFileName->szExtension = NULL;
      zh_fsFNameMerge( szFileName, pFileName );
      zh_xfree( pFileName );
      pOpenInfo->abName = szFileName;
      errCode = SELF_OPENMEMFILE( &pArea->area, pOpenInfo );
   }

   if( errCode == ZH_SUCCESS )
   {
      /* If successful call SUPER_OPEN to finish system jobs */
      errCode = SUPER_OPEN( &pArea->area, pOpenInfo );
   }

   if( errCode != ZH_SUCCESS )
   {
      SELF_CLOSE( &pArea->area );
      pArea->lpdbOpenInfo = NULL;
      return ZH_FAILURE;
   }

   /* Alloc buffer */
   pArea->pRecord = ( ZH_BYTE * ) zh_xgrab( pArea->uiRecordLen );
   pArea->fValidBuffer = ZH_FALSE;

   /* Update the number of record for corrupted headers */
   pArea->ulRecCount = zh_dbfCalcRecCount( pArea );

   /* Position cursor at the first record */
   errCode = SELF_GOTOP( &pArea->area );

   if( pArea->fTrigger )
      zh_dbfTriggerDo( pArea, EVENT_POSTUSE, 0, NULL );

   pArea->lpdbOpenInfo = NULL;

   return errCode;
}

#define zh_dbfRelease  NULL

/*
 * Retrieve the size of the WorkArea structure.
 */
static ZH_ERRCODE zh_dbfStructSize( DBFAREAP pArea, ZH_USHORT * uiSize )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfStrucSize(%p, %p)", ( void * ) pArea, ( void * ) uiSize ) );
   ZH_SYMBOL_UNUSED( pArea );

   *uiSize = sizeof( DBFAREA );
   return ZH_SUCCESS;
}

#define zh_dbfSysName  NULL
#define zh_dbfEval     NULL

/*
 * Pack helper function called for each packed record
 */
static ZH_ERRCODE zh_dbfPackRec( DBFAREAP pArea, ZH_ULONG ulRecNo, ZH_BOOL * fWritten )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfPackRec(%p, %lu, %p)", ( void * ) pArea, ulRecNo, ( void * ) fWritten ) );

   ZH_SYMBOL_UNUSED( ulRecNo );

   *fWritten = ! pArea->fDeleted;

   return ZH_SUCCESS;
}

/*
 * Remove records marked for deletion from a database.
 */
static ZH_ERRCODE zh_dbfPack( DBFAREAP pArea )
{
   ZH_ULONG ulRecIn, ulRecOut, ulEvery, ulUserEvery;
   PZH_ITEM pBlock;
   ZH_BOOL fWritten;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfPack(%p)", ( void * ) pArea ) );

   if( pArea->fReadonly )
   {
      zh_dbfErrorRT( pArea, EG_READONLY, EDBF_READONLY, NULL, 0, 0, NULL );
      return ZH_FAILURE;
   }
   if( pArea->fShared )
   {
      zh_dbfErrorRT( pArea, EG_SHARED, EDBF_SHARED, NULL, 0, 0, NULL );
      return ZH_FAILURE;
   }

   if( pArea->fTrigger )
   {
      if( ! zh_dbfTriggerDo( pArea, EVENT_PACK, 0, NULL ) )
         return ZH_FAILURE;
   }

   if( SELF_GOCOLD( &pArea->area ) != ZH_SUCCESS )
      return ZH_FAILURE;

   /* This is bad hack but looks that people begins to use it :-(
    * so I'll add workaround to make it more safe
    */
   if( pArea->area.valResult && ZH_IS_ARRAY( pArea->area.valResult ) &&
       zh_arrayLen( pArea->area.valResult ) == 2 &&
       ( zh_arrayGetType( pArea->area.valResult, 1 ) & ZH_IT_BLOCK ) != 0 &&
       ( zh_arrayGetType( pArea->area.valResult, 2 ) & ZH_IT_NUMERIC ) != 0 )
   {
      pBlock = zh_itemNew( NULL );
      zh_arrayGet( pArea->area.valResult, 1, pBlock );
      if( zh_arrayGetND( pArea->area.valResult, 2 ) >= 1 )
         ulUserEvery = zh_arrayGetNL( pArea->area.valResult, 2 );
      else
         ulUserEvery = 1;
   }
   else
   {
      pBlock = NULL;
      ulUserEvery = 0;
   }

   ulRecOut = ulEvery = 0;
   ulRecIn = 1;
   while( ulRecIn <= pArea->ulRecCount )
   {
      if( SELF_GOTO( &pArea->area, ulRecIn ) != ZH_SUCCESS )
      {
         if( pBlock )
            zh_itemRelease( pBlock );
         return ZH_FAILURE;
      }
      if( ! zh_dbfReadRecord( pArea ) )
      {
         if( pBlock )
            zh_itemRelease( pBlock );
         return ZH_FAILURE;
      }

      /* Execute the Code Block */
      if( pBlock )
      {
         if( ++ulEvery >= ulUserEvery )
         {
            ulEvery = 0;
            if( SELF_EVALBLOCK( &pArea->area, pBlock ) != ZH_SUCCESS )
            {
               zh_itemRelease( pBlock );
               return ZH_FAILURE;
            }
         }
      }

      if( SELF_PACKREC( &pArea->area, ulRecOut + 1, &fWritten ) != ZH_SUCCESS )
      {
         if( pBlock )
            zh_itemRelease( pBlock );
         return ZH_FAILURE;
      }

      if( fWritten )
      {
         ulRecOut++;
         if( pArea->ulRecNo != ulRecOut || pArea->fRecordChanged )
         {
            pArea->ulRecNo = ulRecOut;
            pArea->fRecordChanged = ZH_TRUE;
            if( ! zh_dbfWriteRecord( pArea ) )
            {
               if( pBlock )
                  zh_itemRelease( pBlock );
               return ZH_FAILURE;
            }
         }
      }
      ulRecIn++;
   }

   /* Execute the Code Block for pending record */
   if( pBlock )
   {
      if( ulEvery > 0 )
      {
         if( SELF_EVALBLOCK( &pArea->area, pBlock ) != ZH_SUCCESS )
         {
            zh_itemRelease( pBlock );
            return ZH_FAILURE;
         }
      }
      zh_itemRelease( pBlock );
   }

   if( pArea->ulRecCount != ulRecOut )
   {
      pArea->ulRecCount = ulRecOut;
      if( SELF_WRITEDBHEADER( &pArea->area ) != ZH_SUCCESS )
         return ZH_FAILURE;
   }
   return SELF_GOTO( &pArea->area, 1 );
}

static ZH_ERRCODE zh_dbfTransCond( DBFAREAP pArea, LPDBTRANSINFO pTransInfo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfTransCond(%p, %p)", ( void * ) pArea, ( void * ) pTransInfo ) );

   if( pTransInfo->uiFlags & DBTF_MATCH )
   {
      if( pArea->fHasMemo || pArea->area.cdPage != pTransInfo->lpaDest->cdPage )
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

   return ZH_SUCCESS;
}

/* NOTE: For large tables the sorting algorithm may access source records
         more then once. It means that results may be wrongly sorted when
         table is changed online by other station during exporting. This
         can be easy eliminated by copping source records to temporary
         file anyhow it will introduce additional overhead in all cases
         and user can easy eliminate the problem by simple FLOCK before
         sort or making export to temporary file and then sorting this
         file so I decided to not implement it.
         I haven't tested what Cl*pper exactly does in such case so
         I cannot say if current behavior is or isn't Cl*pper compatible.
         [druzus]
 */
#define ZH_SORTREC_ARRAYSIZE  0x10000
#define ZH_SORTREC_FIRSTALLOC 0x100
#define ZH_SORTREC_MINRECBUF  0x10

#if ZH_SORTREC_ARRAYSIZE <= 0x10000
   typedef ZH_U16 ZH_SORTIDX;
#else
   typedef ZH_U32 ZH_SORTIDX;
#endif
typedef ZH_U32 ZH_DBRECNO;

typedef struct
{
   ZH_FOFFSET     nOffset;
   ZH_DBRECNO     nCount;
   ZH_DBRECNO     nInBuf;
   ZH_DBRECNO     nCurrent;
   ZH_DBRECNO *   pnRecords;
}
ZH_DBSORTPAGE, * PZH_DBSORTPAGE;

typedef struct
{
   LPDBSORTINFO   pSortInfo;

   PZH_FILE       pTempFile;
   char *         szTempFileName;

   ZH_SORTIDX     nPages;
   ZH_SORTIDX     nMaxPage;
   PZH_DBSORTPAGE pSwapPages;

   ZH_DBRECNO     nCount;
   ZH_DBRECNO     nMaxRec;
   ZH_SORTIDX *   pnIndex;
   ZH_DBRECNO *   pnRecords;
   ZH_DBRECNO *   pnOrder;
   PZH_ITEM       pSortArray;
}
DBSORTREC, * LPDBSORTREC;

static ZH_ERRCODE zh_dbfSortInit( LPDBSORTREC pSortRec, LPDBSORTINFO pSortInfo )
{
   ZH_USHORT uiCount, uiDest;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfSortInit(%p, %p)", ( void * ) pSortInfo, ( void * ) pSortRec ) );

   memset( pSortRec, 0, sizeof( *pSortRec ) );
   pSortRec->pSortInfo = pSortInfo;

   for( uiCount = uiDest = 0; uiCount < pSortInfo->uiItemCount; ++uiCount )
   {
      LPFIELD pField = pSortInfo->dbtri.lpaSource->lpFields +
                       pSortInfo->lpdbsItem[ uiCount ].uiField - 1;

      switch( pField->uiType )
      {
         case ZH_FT_ANY:
            if( pField->uiLen == 4 )
            {
               pSortInfo->lpdbsItem[ uiCount ].uiFlags |= SF_LONG;
               break;
            }
            if( pField->uiLen == 3 )
               break;
            /* fallthrough */
         case ZH_FT_MEMO:
         case ZH_FT_IMAGE:
         case ZH_FT_BLOB:
         case ZH_FT_OLE:
            pSortInfo->lpdbsItem[ uiCount ].uiField = 0;
            break;

         case ZH_FT_INTEGER:
         case ZH_FT_CURRENCY:
         case ZH_FT_AUTOINC:
         case ZH_FT_ROWVER:
            pSortInfo->lpdbsItem[ uiCount ].uiFlags |= pField->uiDec == 0 ?
                                                       SF_LONG : SF_DOUBLE;
            break;
         case ZH_FT_LONG:
            if( pField->uiDec == 0 && pField->uiLen < 19 )
            {
               pSortInfo->lpdbsItem[ uiCount ].uiFlags |= SF_LONG;
               break;
            }
            /* fallthrough */
         case ZH_FT_FLOAT:
         case ZH_FT_DOUBLE:
         case ZH_FT_CURDOUBLE:
            pSortInfo->lpdbsItem[ uiCount ].uiFlags |= SF_DOUBLE;
            break;

         case ZH_FT_STRING:
         case ZH_FT_VARLENGTH:
            break;

         case ZH_FT_DATE:
         case ZH_FT_TIME:
         case ZH_FT_MODTIME:
         case ZH_FT_TIMESTAMP:
         case ZH_FT_LOGICAL:
            break;

         default:
            pSortInfo->lpdbsItem[ uiCount ].uiField = 0;
            break;
      }
      switch( pField->uiType )
      {
         case ZH_FT_STRING:
         case ZH_FT_VARLENGTH:
            break;
         default:
            pSortInfo->lpdbsItem[ uiCount ].uiFlags &= ~SF_CASE;
      }
      if( pSortInfo->lpdbsItem[ uiCount ].uiField != 0 )
      {
         if( uiCount != uiDest )
         {
            pSortInfo->lpdbsItem[ uiDest ].uiField = pSortInfo->lpdbsItem[ uiCount ].uiField;
            pSortInfo->lpdbsItem[ uiDest ].uiFlags = pSortInfo->lpdbsItem[ uiCount ].uiFlags;
         }
         ++uiDest;
      }
   }
   pSortInfo->uiItemCount = uiDest;

   return ZH_SUCCESS;
}

static void zh_dbfSortFree( LPDBSORTREC pSortRec )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfSortFree(%p)", ( void * ) pSortRec ) );

   if( pSortRec->pTempFile != NULL )
      zh_fileClose( pSortRec->pTempFile );
   if( pSortRec->szTempFileName )
   {
      zh_fileDelete( pSortRec->szTempFileName );
      zh_xfree( pSortRec->szTempFileName );
   }

   if( pSortRec->pSortArray )
      zh_itemRelease( pSortRec->pSortArray );
   if( pSortRec->pnIndex )
      zh_xfree( pSortRec->pnIndex );
   if( pSortRec->pnRecords )
      zh_xfree( pSortRec->pnRecords );
   if( pSortRec->pnOrder )
      zh_xfree( pSortRec->pnOrder );
   if( pSortRec->pSwapPages )
      zh_xfree( pSortRec->pSwapPages );
}

static int zh_dbfSortCmp( LPDBSORTREC pSortRec, PZH_ITEM pValue1, PZH_ITEM pValue2 )
{
   ZH_USHORT uiCount;

   for( uiCount = 0; uiCount < pSortRec->pSortInfo->uiItemCount; ++uiCount )
   {
      ZH_USHORT uiFlags = pSortRec->pSortInfo->lpdbsItem[ uiCount ].uiFlags;
      PZH_ITEM pItem1 = zh_arrayGetItemPtr( pValue1, uiCount + 1 );
      PZH_ITEM pItem2 = zh_arrayGetItemPtr( pValue2, uiCount + 1 );
      int i = 0;

      if( uiFlags & SF_DOUBLE )
      {
         double dValue1 = zh_itemGetND( pItem1 ),
                dValue2 = zh_itemGetND( pItem2 );
         i = dValue1 < dValue2 ? -1 : ( dValue1 == dValue2 ? 0 : 1 );
      }
      else if( uiFlags & SF_LONG )
      {
         ZH_MAXINT nValue1 = zh_itemGetNInt( pItem1 ),
                   nValue2 = zh_itemGetNInt( pItem2 );
         i = nValue1 < nValue2 ? -1 : ( nValue1 == nValue2 ? 0 : 1 );
      }
      else if( ZH_IS_STRING( pItem1 ) )
      {
         if( ! ZH_IS_STRING( pItem2 ) )
            i = 1;
         else if( uiFlags & SF_CASE )
            i = zh_itemStrICmp( pItem1, pItem2, ZH_TRUE );
         else
            i = zh_itemStrCmp( pItem1, pItem2, ZH_TRUE );
      }
      else if( ZH_IS_DATETIME( pItem1 ) )
      {
         double dValue1 = zh_itemGetTD( pItem1 ),
                dValue2 = zh_itemGetTD( pItem2 );
         i = dValue1 < dValue2 ? -1 : ( dValue1 == dValue2 ? 0 : 1 );
      }
      else if( ZH_IS_LOGICAL( pItem1 ) )
         i = zh_itemGetL( pItem1 ) ? ( zh_itemGetL( pItem2 ) ? 0 : 1 ) :
                                     ( zh_itemGetL( pItem2 ) ? -1 : 0 );
      if( i != 0 )
         return ( uiFlags & SF_DESCEND ) ? -i : i;
   }

   return 0;
}

static int zh_dbfSortCompare( LPDBSORTREC pSortRec,
                              ZH_SORTIDX nIndex1, ZH_SORTIDX nIndex2 )
{
   int i = zh_dbfSortCmp( pSortRec,
                          zh_arrayGetItemPtr( pSortRec->pSortArray, nIndex1 + 1 ),
                          zh_arrayGetItemPtr( pSortRec->pSortArray, nIndex2 + 1 ) );
   return i == 0 ? ( nIndex1 < nIndex2 ? -1 : 1 ) : i;
}

static ZH_BOOL zh_dbfSortQSort( LPDBSORTREC pSortRec, ZH_SORTIDX * pSrc,
                                ZH_SORTIDX * pBuf, ZH_DBRECNO nKeys )
{
   if( nKeys > 1 )
   {
      ZH_DBRECNO n1, n2;
      ZH_SORTIDX * pPtr1, * pPtr2, * pDst;
      ZH_BOOL f1, f2;

      n1 = nKeys >> 1;
      n2 = nKeys - n1;
      pPtr1 = &pSrc[ 0 ];
      pPtr2 = &pSrc[ n1 ];

      f1 = zh_dbfSortQSort( pSortRec, pPtr1, &pBuf[ 0 ], n1 );
      f2 = zh_dbfSortQSort( pSortRec, pPtr2, &pBuf[ n1 ], n2 );
      if( f1 )
         pDst = pBuf;
      else
      {
         pDst = pSrc;
         pPtr1 = &pBuf[ 0 ];
      }
      if( ! f2 )
         pPtr2 = &pBuf[ n1 ];
      while( n1 > 0 && n2 > 0 )
      {
         if( zh_dbfSortCompare( pSortRec, *pPtr1, *pPtr2 ) <= 0 )
         {
            *pDst++ = *pPtr1++;
            n1--;
         }
         else
         {
            *pDst++ = *pPtr2++;
            n2--;
         }
      }
      if( n1 > 0 )
         memcpy( pDst, pPtr1, n1 * sizeof( ZH_SORTIDX ) );
      else if( n2 > 0 && f1 == f2 )
         memcpy( pDst, pPtr2, n2 * sizeof( ZH_SORTIDX ) );
      return ! f1;
   }
   return ZH_TRUE;
}

static ZH_DBRECNO * zh_dbfSortSort( LPDBSORTREC pSortRec )
{
   ZH_SORTIDX * pOrder;
   ZH_DBRECNO nCount;

   if( pSortRec->pnIndex == NULL )
      pSortRec->pnIndex = ( ZH_SORTIDX * ) zh_xgrab(
            ( ( ZH_SIZE ) pSortRec->nCount << 1 ) * sizeof( ZH_SORTIDX ) );
   for( nCount = 0; nCount < pSortRec->nCount; ++nCount )
      pSortRec->pnIndex[ nCount ] = ( ZH_SORTIDX ) nCount;

   pOrder = pSortRec->pnIndex;
   if( ! zh_dbfSortQSort( pSortRec, pOrder, &pSortRec->pnIndex[ pSortRec->nCount ],
                          pSortRec->nCount ) )
      pOrder += pSortRec->nCount;

   if( pSortRec->pnOrder == NULL )
      pSortRec->pnOrder = ( ZH_DBRECNO * ) zh_xgrab(
            ( ZH_SIZE ) pSortRec->nCount * sizeof( ZH_DBRECNO ) );
   for( nCount = 0; nCount < pSortRec->nCount; ++nCount )
      pSortRec->pnOrder[ nCount ] = pSortRec->pnRecords[ pOrder[ nCount ] ];

   return pSortRec->pnOrder;
}

static void zh_dbfSortInsPage( LPDBSORTREC pSortRec, ZH_SORTIDX * pIndex,
                               ZH_SORTIDX nFirst, ZH_SORTIDX nLast,
                               ZH_SORTIDX nAt )
{
   while( nFirst < nLast )
   {
      ZH_SORTIDX nMiddle = ( nFirst + nLast ) >> 1;
      int i = zh_dbfSortCompare( pSortRec, pIndex[ nAt ], pIndex[ nMiddle ] );

      if( i < 0 )
         nLast = nMiddle;
      else
         nFirst = nMiddle + 1;
   }
   if( nAt == 0 )
   {
      if( nFirst > 1 )
      {
         nLast = pIndex[ 0 ];
         memmove( pIndex, &pIndex[ 1 ], ( nFirst - 1 ) * sizeof( ZH_SORTIDX ) );
         pIndex[ nFirst - 1 ] = nLast;
      }
   }
   else if( nFirst != nAt )
   {
      nLast = pIndex[ nAt ];
      memmove( &pIndex[ nFirst + 1 ], &pIndex[ nFirst ],
               ( nAt - nFirst ) * sizeof( ZH_SORTIDX ) );
      pIndex[ nFirst ] = nLast;
   }
}

static ZH_ERRCODE zh_dbfSortWritePage( LPDBSORTREC pSortRec )
{
   ZH_DBRECNO * pData = zh_dbfSortSort( pSortRec );
   ZH_SIZE nSize = ( ZH_SIZE ) pSortRec->nCount * sizeof( ZH_DBRECNO );
   AREAP pArea = pSortRec->pSortInfo->dbtri.lpaSource;

   if( pSortRec->pTempFile == NULL )
   {
      char szName[ ZH_PATH_MAX ];
      pSortRec->pTempFile = zh_fileCreateTemp( NULL, NULL, FC_NORMAL, szName );
      if( pSortRec->pTempFile == NULL )
      {
         zh_dbfErrorRT( ( DBFAREAP ) pArea, EG_CREATE, EDBF_CREATE_TEMP,
                        szName, zh_fsError(), 0, NULL );
         return ZH_FAILURE;
      }
      pSortRec->szTempFileName = zh_strdup( szName );
   }

   if( pSortRec->nPages == pSortRec->nMaxPage )
   {
      pSortRec->nMaxPage += 8;
      pSortRec->pSwapPages = ( PZH_DBSORTPAGE ) zh_xrealloc( pSortRec->pSwapPages,
                             pSortRec->nMaxPage * sizeof( ZH_DBSORTPAGE ) );
   }
   memset( &pSortRec->pSwapPages[ pSortRec->nPages ], 0, sizeof( ZH_DBSORTPAGE ) );
   pSortRec->pSwapPages[ pSortRec->nPages ].nCount = pSortRec->nCount;
   pSortRec->pSwapPages[ pSortRec->nPages ].nOffset = zh_fileSize( pSortRec->pTempFile );

   if( zh_fileWriteAt( pSortRec->pTempFile, pData, nSize,
                       pSortRec->pSwapPages[ pSortRec->nPages ].nOffset ) != nSize )
   {
      zh_dbfErrorRT( ( DBFAREAP ) pArea, EG_WRITE, EDBF_WRITE_TEMP,
                     pSortRec->szTempFileName, zh_fsError(), 0, NULL );
      return ZH_FAILURE;
   }
   pSortRec->nPages++;
   pSortRec->nCount = 0;

   return ZH_SUCCESS;
}

static ZH_ERRCODE zh_dbfSortReadRec( LPDBSORTREC pSortRec, PZH_ITEM pValue )
{
   AREAP pArea = pSortRec->pSortInfo->dbtri.lpaSource;
   ZH_SHORT uiCount;

   if( ZH_IS_NIL( pValue ) )
      zh_arrayNew( pValue, pSortRec->pSortInfo->uiItemCount );
   else
      zh_arraySize( pValue, pSortRec->pSortInfo->uiItemCount );

   for( uiCount = 0; uiCount < pSortRec->pSortInfo->uiItemCount; uiCount++ )
   {
      PZH_ITEM pItem = zh_arrayGetItemPtr( pValue, uiCount + 1 );
      ZH_USHORT uiField = pSortRec->pSortInfo->lpdbsItem[ uiCount ].uiField;
      if( SELF_GETVALUE( pArea, uiField, pItem ) != ZH_SUCCESS )
         return ZH_FAILURE;
   }

   return ZH_SUCCESS;
}

static ZH_ERRCODE zh_dbfSortReadPage( LPDBSORTREC pSortRec, PZH_DBSORTPAGE pPage )
{
   AREAP pArea = pSortRec->pSortInfo->dbtri.lpaSource;
   ZH_DBRECNO nCount = ZH_MIN( pSortRec->nMaxRec, pPage->nCount );
   ZH_SIZE nSize = ( ZH_SIZE ) nCount * sizeof( ZH_DBRECNO );

   if( zh_fileReadAt( pSortRec->pTempFile, pPage->pnRecords, nSize,
                      pPage->nOffset ) != nSize )
   {
      zh_dbfErrorRT( ( DBFAREAP ) pArea, EG_READ, EDBF_READ_TEMP,
                     pSortRec->szTempFileName, zh_fsError(), 0, NULL );
      return ZH_FAILURE;
   }
   pPage->nOffset += nSize;
   pPage->nInBuf = nCount;
   pPage->nCurrent = 0;

   return ZH_SUCCESS;
}

static ZH_ERRCODE zh_dbfSortGetRec( LPDBSORTREC pSortRec, ZH_DBRECNO * pnRecNo )
{
   ZH_SORTIDX nPage = pSortRec->pnIndex[ 0 ];
   PZH_DBSORTPAGE pPage = &pSortRec->pSwapPages[ nPage ];

   *pnRecNo = pPage->pnRecords[ pPage->nCurrent++ ];
   if( --pPage->nCount == 0 )
   {
      if( --pSortRec->nPages > 0 )
         memmove( pSortRec->pnIndex, &pSortRec->pnIndex[ 1 ],
                  pSortRec->nPages * sizeof( ZH_SORTIDX ) );
   }
   else
   {
      if( pPage->nCurrent == pPage->nInBuf )
      {
         if( zh_dbfSortReadPage( pSortRec, pPage ) != ZH_SUCCESS )
            return ZH_FAILURE;
      }
      if( pSortRec->nPages > 1 )
      {
         AREAP pArea = pSortRec->pSortInfo->dbtri.lpaSource;
         if( SELF_GOTO( pArea, pPage->pnRecords[ pPage->nCurrent ] ) != ZH_SUCCESS ||
             zh_dbfSortReadRec( pSortRec, zh_arrayGetItemPtr( pSortRec->pSortArray,
                                                              nPage + 1 ) ) != ZH_SUCCESS )
            return ZH_FAILURE;
         zh_dbfSortInsPage( pSortRec, pSortRec->pnIndex, 1, pSortRec->nPages, 0 );
      }
   }

   return ZH_SUCCESS;
}

static ZH_ERRCODE zh_dbfSortFinish( LPDBSORTREC pSortRec )
{
   AREAP pArea = pSortRec->pSortInfo->dbtri.lpaSource;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfSortFinish(%p)", ( void * ) pSortRec ) );

   if( pSortRec->nCount > 0 )
   {
      if( pSortRec->nPages > 0 )
      {
         ZH_DBRECNO nCount, * pnOrder;
         ZH_SORTIDX nPage;

         if( zh_dbfSortWritePage( pSortRec ) != ZH_SUCCESS )
            return ZH_FAILURE;

         if( pSortRec->pSortArray )
            zh_itemRelease( pSortRec->pSortArray );
         if( pSortRec->pnRecords )
            zh_xfree( pSortRec->pnRecords );
         if( pSortRec->pnIndex )
            zh_xfree( pSortRec->pnIndex );
         if( pSortRec->pnOrder )
            zh_xfree( pSortRec->pnOrder );

         if( pSortRec->nPages > ZH_SORTREC_MINRECBUF )
            pSortRec->nMaxRec = pSortRec->nMaxRec * ZH_SORTREC_MINRECBUF /
                                pSortRec->nPages;
         nCount = pSortRec->pSwapPages[ pSortRec->nPages - 1 ].nCount;
         if( nCount < pSortRec->nMaxRec )
            nCount = pSortRec->nMaxRec - nCount;
         else
            nCount = 0;
         nCount = pSortRec->nMaxRec * pSortRec->nPages - nCount;

         pSortRec->pSortArray = zh_itemArrayNew( pSortRec->nPages );
         pSortRec->pnRecords = ( ZH_DBRECNO * ) zh_xgrab( pSortRec->nPages *
                                                          sizeof( ZH_DBRECNO ) );
         pSortRec->pnIndex = ( ZH_SORTIDX * ) zh_xgrab( pSortRec->nPages *
                                                        sizeof( ZH_SORTIDX ) );
         pSortRec->pnOrder = pnOrder = ( ZH_DBRECNO * )
                                       zh_xgrab( nCount * sizeof( ZH_DBRECNO ) );
         for( nPage = 0; nPage < pSortRec->nPages; ++nPage, pnOrder += pSortRec->nMaxRec )
         {
            pSortRec->pSwapPages[ nPage ].pnRecords = pnOrder;
            if( zh_dbfSortReadPage( pSortRec, &pSortRec->pSwapPages[ nPage ] ) != ZH_SUCCESS ||
                SELF_GOTO( pArea, pnOrder[ 0 ] ) != ZH_SUCCESS ||
                zh_dbfSortReadRec( pSortRec, zh_arrayGetItemPtr( pSortRec->pSortArray,
                                                                 nPage + 1 ) ) != ZH_SUCCESS )
               return ZH_FAILURE;

            pSortRec->pnIndex[ nPage ] = nPage;
            if( nPage > 0 )
               zh_dbfSortInsPage( pSortRec, pSortRec->pnIndex, 0, nPage, nPage );
         }
      }
      else
      {
         pSortRec->pSwapPages = ( PZH_DBSORTPAGE ) zh_xgrabz( sizeof( ZH_DBSORTPAGE ) );
         pSortRec->pSwapPages[ 0 ].nCount =
         pSortRec->pSwapPages[ 0 ].nInBuf = pSortRec->nCount;
         pSortRec->pSwapPages[ 0 ].pnRecords = zh_dbfSortSort( pSortRec );
         pSortRec->nPages = 1;
         pSortRec->pnIndex = ( ZH_SORTIDX * ) zh_xrealloc( pSortRec->pnIndex, sizeof( ZH_SORTIDX ) );
         pSortRec->pnIndex[ 0 ] = 0;
         if( pSortRec->pSortArray )
         {
            zh_itemRelease( pSortRec->pSortArray );
            pSortRec->pSortArray = NULL;
         }
         if( pSortRec->pnRecords )
         {
            zh_xfree( pSortRec->pnRecords );
            pSortRec->pnRecords = NULL;
         }
      }
   }

   while( pSortRec->nPages > 0 )
   {
      ZH_DBRECNO nRecNo;

      if( zh_dbfSortGetRec( pSortRec, &nRecNo ) != ZH_SUCCESS ||
          SELF_GOTO( pArea, nRecNo ) != ZH_SUCCESS ||
          SELF_TRANSREC( pArea, &pSortRec->pSortInfo->dbtri ) != ZH_SUCCESS )
         return ZH_FAILURE;
   }

   return ZH_SUCCESS;
}

static ZH_ERRCODE zh_dbfSortAdd( LPDBSORTREC pSortRec )
{
   AREAP pArea;
   ZH_ULONG ulRecNo;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfSortAdd(%p)", ( void * ) pSortRec ) );

   pArea = pSortRec->pSortInfo->dbtri.lpaSource;

   if( pSortRec->nCount == pSortRec->nMaxRec )
   {
      if( pSortRec->nMaxRec < ZH_SORTREC_ARRAYSIZE )
      {
         if( pSortRec->nMaxRec == 0 )
            pSortRec->nMaxRec = ZH_SORTREC_FIRSTALLOC;
         else
            pSortRec->nMaxRec <<= 1;
         if( pSortRec->nMaxRec > ZH_SORTREC_ARRAYSIZE )
            pSortRec->nMaxRec = ZH_SORTREC_ARRAYSIZE;

         pSortRec->pnRecords = ( ZH_DBRECNO * ) zh_xrealloc( pSortRec->pnRecords,
               ( ZH_SIZE ) pSortRec->nMaxRec * sizeof( ZH_DBRECNO ) );
         if( pSortRec->pSortArray )
            zh_arraySize( pSortRec->pSortArray, pSortRec->nMaxRec );
         else
            pSortRec->pSortArray = zh_itemArrayNew( pSortRec->nMaxRec );
      }
      if( pSortRec->nCount == pSortRec->nMaxRec )
      {
         if( zh_dbfSortWritePage( pSortRec ) != ZH_SUCCESS )
            return ZH_FAILURE;
      }
   }

   if( SELF_RECNO( pArea, &ulRecNo ) != ZH_SUCCESS ||
       zh_dbfSortReadRec( pSortRec, zh_arrayGetItemPtr( pSortRec->pSortArray,
                                          pSortRec->nCount + 1 ) ) != ZH_SUCCESS )
      return ZH_FAILURE;
   pSortRec->pnRecords[ pSortRec->nCount++ ] = ( ZH_DBRECNO ) ulRecNo;

   return ZH_SUCCESS;
}

/*
 * Export sorted records
 */
static ZH_ERRCODE zh_dbfSort( DBFAREAP pArea, LPDBSORTINFO pSortInfo )
{
   DBSORTREC dbSortRec;
   ZH_BOOL fEof, fFor;
   ZH_ERRCODE errCode;
   ZH_LONG lNext = 1;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfSort(%p, %p)", ( void * ) pArea, ( void * ) pSortInfo ) );

   if( SELF_GOCOLD( &pArea->area ) != ZH_SUCCESS )
      return ZH_FAILURE;

   if( zh_dbfSortInit( &dbSortRec, pSortInfo ) != ZH_SUCCESS )
      return ZH_FAILURE;

   if( pSortInfo->uiItemCount == 0 )
      return SELF_TRANS( &pArea->area, &pSortInfo->dbtri );

   errCode = zh_dbfTransCond( pArea, &pSortInfo->dbtri );
   if( errCode == ZH_SUCCESS )
   {
      if( pSortInfo->dbtri.dbsci.itmRecID )
         errCode = SELF_GOTOID( &pArea->area, pSortInfo->dbtri.dbsci.itmRecID );
      else if( pSortInfo->dbtri.dbsci.lNext )
         lNext = zh_itemGetNL( pSortInfo->dbtri.dbsci.lNext );
      else if( ! pSortInfo->dbtri.dbsci.itmCobWhile &&
               ! zh_itemGetLX( pSortInfo->dbtri.dbsci.fRest ) )
         errCode = SELF_GOTOP( &pArea->area );
   }

   /* TODO: use SKIPSCOPE() method and fRest parameter */

   while( errCode == ZH_SUCCESS && lNext > 0 )
   {
      errCode = SELF_EOF( &pArea->area, &fEof );
      if( errCode != ZH_SUCCESS || fEof )
         break;

      if( pSortInfo->dbtri.dbsci.itmCobWhile )
      {
         errCode = SELF_EVALBLOCK( &pArea->area, pSortInfo->dbtri.dbsci.itmCobWhile );
         if( errCode != ZH_SUCCESS || ! zh_itemGetLX( pArea->area.valResult ) )
            break;
      }

      if( pSortInfo->dbtri.dbsci.itmCobFor )
      {
         errCode = SELF_EVALBLOCK( &pArea->area, pSortInfo->dbtri.dbsci.itmCobFor );
         if( errCode != ZH_SUCCESS )
            break;
         fFor = zh_itemGetLX( pArea->area.valResult );
      }
      else
         fFor = ZH_TRUE;

      if( fFor )
         errCode = zh_dbfSortAdd( &dbSortRec );

      if( errCode != ZH_SUCCESS || pSortInfo->dbtri.dbsci.itmRecID ||
          ( pSortInfo->dbtri.dbsci.lNext && --lNext < 1 ) )
         break;

      errCode = SELF_SKIP( &pArea->area, 1 );
   }

   if( errCode == ZH_SUCCESS )
      errCode = zh_dbfSortFinish( &dbSortRec );

   zh_dbfSortFree( &dbSortRec );

   return errCode;
}

/*
 * Copy one or more records from one WorkArea to another.
 */
static ZH_ERRCODE zh_dbfTrans( DBFAREAP pArea, LPDBTRANSINFO pTransInfo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfTrans(%p, %p)", ( void * ) pArea, ( void * ) pTransInfo ) );

   if( zh_dbfTransCond( pArea, pTransInfo ) != ZH_SUCCESS )
      return ZH_FAILURE;
   else
      return SUPER_TRANS( &pArea->area, pTransInfo );
}

#define zh_dbfTransRec  NULL

/*
 * Physically remove all records from data store.
 */
static ZH_ERRCODE zh_dbfZap( DBFAREAP pArea )
{
   ZH_USHORT uiField;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfZap(%p)", ( void * ) pArea ) );

   if( pArea->fReadonly )
   {
      zh_dbfErrorRT( pArea, EG_READONLY, EDBF_READONLY, NULL, 0, 0, NULL );
      return ZH_FAILURE;
   }
   if( pArea->fShared )
   {
      zh_dbfErrorRT( pArea, EG_SHARED, EDBF_SHARED, NULL, 0, 0, NULL );
      return ZH_FAILURE;
   }

   if( pArea->fTrigger )
   {
      if( ! zh_dbfTriggerDo( pArea, EVENT_ZAP, 0, NULL ) )
         return ZH_FAILURE;
   }

   if( SELF_GOCOLD( &pArea->area ) != ZH_SUCCESS )
      return ZH_FAILURE;

   pArea->ulRecCount = 0;

   if( SELF_WRITEDBHEADER( &pArea->area ) != ZH_SUCCESS )
      return ZH_FAILURE;
   if( SELF_GOTO( &pArea->area, 0 ) != ZH_SUCCESS )
      return ZH_FAILURE;

   /* reset auto-increment and row version fields */
   for( uiField = 0; uiField < pArea->area.uiFieldCount; uiField++ )
   {
      if( pArea->area.lpFields[ uiField ].uiType == ZH_FT_ROWVER )
         zh_dbfRowVerSet( pArea, uiField, 0 );
      else if( zh_dbfIsAutoIncField( &pArea->area.lpFields[ uiField ] ) != ZH_AUTOINC_NONE )
         zh_dbfNextValueSet( pArea, uiField, 1 );
   }

   /* Zap memo file */
   if( pArea->fHasMemo )
   {
      if( SELF_CREATEMEMFILE( &pArea->area, NULL ) != ZH_SUCCESS )
         return ZH_FAILURE;
   }
   return ZH_SUCCESS;
}

/*
 * Report end of relation.
 */
static ZH_ERRCODE zh_dbfChildEnd( DBFAREAP pArea, LPDBRELINFO pRelInfo )
{
   ZH_ERRCODE errCode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfChildEnd(%p, %p)", ( void * ) pArea, ( void * ) pRelInfo ) );

   if( pArea->lpdbPendingRel == pRelInfo )
      errCode = SELF_FORCEREL( &pArea->area );
   else
      errCode = ZH_SUCCESS;
   SUPER_CHILDEND( &pArea->area, pRelInfo );
   return errCode;
}

/*
 * Report initialization of a relation.
 */
static ZH_ERRCODE zh_dbfChildStart( DBFAREAP pArea, LPDBRELINFO pRelInfo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfChildStart(%p, %p)", ( void * ) pArea, ( void * ) pRelInfo ) );

   if( SELF_CHILDSYNC( &pArea->area, pRelInfo ) != ZH_SUCCESS )
      return ZH_FAILURE;
   return SUPER_CHILDSTART( &pArea->area, pRelInfo );
}

/*
 * Post a pending relational movement.
 */
static ZH_ERRCODE zh_dbfChildSync( DBFAREAP pArea, LPDBRELINFO pRelInfo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfChildSync(%p, %p)", ( void * ) pArea, ( void * ) pRelInfo ) );
   /*
    * After some cleanups, the core DBF* code can work with GOCOLD() here
    * and in FORCEREL() without any problems. Because calling GOCOLD() in
    * FORCEREL() may interacts with badly written users RDD which inherits
    * from DBF* RDDs and/or user triggers then I decided to keep it here,
    * Druzus.
    */

   if( SELF_GOCOLD( &pArea->area ) != ZH_SUCCESS )
      return ZH_FAILURE;

   pArea->lpdbPendingRel = pRelInfo;

   if( pArea->area.lpdbRelations )
      return SELF_SYNCCHILDREN( &pArea->area );

   return ZH_SUCCESS;
}

#define zh_dbfSyncChildren  NULL
#define zh_dbfClearRel      NULL

/*
 * Force relational seeks in the specified WorkArea.
 */
static ZH_ERRCODE zh_dbfForceRel( DBFAREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfForceRel(%p)", ( void * ) pArea ) );

   if( pArea->lpdbPendingRel )
   {
      LPDBRELINFO lpdbPendingRel;

      lpdbPendingRel = pArea->lpdbPendingRel;
      pArea->lpdbPendingRel = NULL;

      /* update buffers */
      /* commented out - see comment above in CHILDSYNC() method, Druzus */
      #if 0
      SELF_GOCOLD( &pArea->area );
      #endif

      return SELF_RELEVAL( &pArea->area, lpdbPendingRel );
   }
   return ZH_SUCCESS;
}

#define zh_dbfRelArea           NULL
#define zh_dbfRelEval           NULL
#define zh_dbfRelText           NULL
#define zh_dbfSetRel            NULL

#define zh_dbfOrderListAdd      NULL
#define zh_dbfOrderListClear    NULL
#define zh_dbfOrderListDelete   NULL
#define zh_dbfOrderListFocus    NULL
#define zh_dbfOrderListRebuild  NULL
#define zh_dbfOrderCondition    NULL
#define zh_dbfOrderCreate       NULL
#define zh_dbfOrderDestroy      NULL
#define zh_dbfOrderInfo         NULL

/*
 * Clear the filter condition for the specified WorkArea.
 */
static ZH_ERRCODE zh_dbfClearFilter( DBFAREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfClearFilter(%p)", ( void * ) pArea ) );

   if( pArea->lpdbPendingRel )
      SELF_FORCEREL( &pArea->area );

   return SUPER_CLEARFILTER( &pArea->area );
}

#define zh_dbfClearLocate  NULL
#define zh_dbfClearScope   NULL
#define zh_dbfCountScope   NULL
#define zh_dbfFilterText   NULL
#define zh_dbfScopeInfo    NULL

/*
 * Set the filter condition for the specified WorkArea.
 */
static ZH_ERRCODE zh_dbfSetFilter( DBFAREAP pArea, LPDBFILTERINFO pFilterInfo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfSetFilter(%p, %p)", ( void * ) pArea, ( void * ) pFilterInfo ) );

   if( pArea->lpdbPendingRel )
      SELF_FORCEREL( &pArea->area );

   return SUPER_SETFILTER( &pArea->area, pFilterInfo );
}

#define zh_dbfSetLocate  NULL
#define zh_dbfSetScope   NULL
#define zh_dbfSkipScope  NULL
#define zh_dbfLocate     NULL

#define zh_dbfCompile    NULL
#define zh_dbfError      NULL
#define zh_dbfEvalBlock  NULL

/*
 * Perform a network low-level lock in the specified WorkArea.
 */
static ZH_ERRCODE zh_dbfRawLock( DBFAREAP pArea, ZH_USHORT uiAction, ZH_ULONG ulRecNo )
{
   ZH_ERRCODE errCode = ZH_SUCCESS;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfRawLock(%p, %hu, %lu)", ( void * ) pArea, uiAction, ulRecNo ) );

   if( pArea->fShared )
   {
      ZH_FOFFSET nPos, nFlSize, nRlSize;
      int iDir;
      ZH_BOOL fLck;

      if( zh_dbfLockData( pArea, &nPos, &nFlSize, &nRlSize, &iDir ) == ZH_FAILURE )
         return ZH_FAILURE;

      switch( uiAction )
      {
         case FILE_LOCK:
            if( ! pArea->fFLocked )
            {
               if( iDir < 0 )
                  nPos -= nFlSize;
               else
                  nPos++;

               fLck = zh_fileLock( pArea->pDataFile, nPos, nFlSize, FL_LOCK );
               if( ! fLck )
                  errCode = ZH_FAILURE;
               else
                  pArea->fFLocked = ZH_TRUE;
            }
            break;

         case FILE_UNLOCK:
            if( pArea->fFLocked )
            {
               if( iDir < 0 )
                  nPos -= nFlSize;
               else
                  nPos++;

               fLck = zh_fileLock( pArea->pDataFile, nPos, nFlSize, FL_UNLOCK );
               if( ! fLck )
                  errCode = ZH_FAILURE;
               pArea->fFLocked = ZH_FALSE;
            }
            break;

         case REC_LOCK:
            if( ! pArea->fFLocked )
            {
               if( iDir < 0 )
                  nPos -= ulRecNo;
               else if( iDir == 2 )
                  nPos += ( ulRecNo - 1 ) * pArea->uiRecordLen + pArea->uiHeaderLen;
               else
                  nPos += ulRecNo;

               fLck = zh_fileLock( pArea->pDataFile, nPos, nRlSize, FL_LOCK );
               if( ! fLck )
                  errCode = ZH_FAILURE;
            }
            break;

         case REC_UNLOCK:
            if( ! pArea->fFLocked )
            {
               if( iDir < 0 )
                  nPos -= ulRecNo;
               else if( iDir == 2 )
                  nPos += ( ulRecNo - 1 ) * pArea->uiRecordLen + pArea->uiHeaderLen;
               else
                  nPos += ulRecNo;

               fLck = zh_fileLock( pArea->pDataFile, nPos, nRlSize, FL_UNLOCK );
               if( ! fLck )
                  errCode = ZH_FAILURE;
            }
            break;

         case APPEND_LOCK:
         case HEADER_LOCK:
            if( ! pArea->fHeaderLocked )
            {
               for( ;; )
               {
                  fLck = zh_fileLock( pArea->pDataFile, nPos, 1, FL_LOCK | FLX_WAIT );
                  /* TODO: call special error handler (LOCKHANDLER) if ! fLck */
                  if( fLck )
                     break;
                  zh_releaseCPU();
               }
               if( ! fLck )
                  errCode = ZH_FAILURE;
               else
                  pArea->fHeaderLocked = ZH_TRUE;
            }
            break;

         case APPEND_UNLOCK:
         case HEADER_UNLOCK:
            if( pArea->fHeaderLocked )
            {
               if( ! zh_fileLock( pArea->pDataFile, nPos, 1, FL_UNLOCK ) )
                  errCode = ZH_FAILURE;
               pArea->fHeaderLocked = ZH_FALSE;
            }
            break;
      }
   }
   return errCode;
}

/*
 * Perform a network lock in the specified WorkArea.
 */
static ZH_ERRCODE zh_dbfLock( DBFAREAP pArea, LPDBLOCKINFO pLockInfo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfLock(%p, %p)", ( void * ) pArea, ( void * ) pLockInfo ) );

   if( pArea->fShared )
   {
      switch( pLockInfo->uiMethod )
      {
         case DBLM_EXCLUSIVE:
            return zh_dbfLockRecord( pArea, 0, &pLockInfo->fResult, ZH_TRUE );

         case DBLM_MULTIPLE:
            return zh_dbfLockRecord( pArea, zh_itemGetNL( pLockInfo->itmRecID ),
                                     &pLockInfo->fResult, ZH_FALSE );

         case DBLM_FILE:
            return zh_dbfLockFile( pArea, &pLockInfo->fResult );

         default:
            pLockInfo->fResult = ZH_FALSE;
      }
   }
   else
      pLockInfo->fResult = ZH_TRUE;

   return ZH_SUCCESS;
}

/*
 * Release network locks in the specified WorkArea.
 */
static ZH_ERRCODE zh_dbfUnLock( DBFAREAP pArea, PZH_ITEM pRecNo )
{
   ZH_ERRCODE errCode = ZH_SUCCESS;

   ZH_TRACE( ZH_TR_DEBUG, ( "dbfUnLock(%p, %p)", ( void * ) pArea, ( void * ) pRecNo ) );

   if( pArea->fShared )
   {
      if( pArea->ulNumLocksPos > 0 )
      {
         ZH_ULONG ulRecNo = zh_itemGetNL( pRecNo );
         /* Unlock all records? */
         if( ulRecNo == 0 )
            errCode = zh_dbfUnlockAllRecords( pArea );
         else if( zh_dbfIsLocked( pArea, ulRecNo ) )
            errCode = zh_dbfUnlockRecord( pArea, ulRecNo );
      }
      if( pArea->fFLocked )
      {
         errCode = zh_dbfUnlockFile( pArea );
      }
   }
   return errCode;
}

#define zh_dbfCloseMemFile  NULL

/*
 * Create a memo file in the WorkArea.
 */
static ZH_ERRCODE zh_dbfCreateMemFile( DBFAREAP pArea, LPDBOPENINFO pCreateInfo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfCreateMemFile(%p, %p)", ( void * ) pArea, ( void * ) pCreateInfo ) );

   if( pCreateInfo )
      zh_dbfErrorRT( pArea, EG_CREATE, EDBF_DATATYPE, pCreateInfo->abName, 0, 0, NULL );

   pArea->fHasMemo = ZH_FALSE;

   return ZH_FAILURE;
}

/*
 * BLOB2FILE - retrieve memo contents into file
 */
static ZH_ERRCODE zh_dbfGetValueFile( DBFAREAP pArea, ZH_USHORT uiIndex, const char * szFile, ZH_USHORT uiMode )
{
   ZH_ERRCODE errCode = ZH_SUCCESS;
   LPFIELD pField;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfGetValueFile(%p, %hu, %s, %hu)", ( void * ) pArea, uiIndex, szFile, uiMode ) );

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( &pArea->area ) != ZH_SUCCESS )
         return ZH_FAILURE;
   }

   /* Read record */
   if( ! pArea->fValidBuffer && ! zh_dbfReadRecord( pArea ) )
      return ZH_FAILURE;

   if( --uiIndex >= pArea->area.uiFieldCount )
      return ZH_FAILURE;

   pField = pArea->area.lpFields + uiIndex;
   if( pField->uiType == ZH_FT_STRING )
   {
      PZH_FILE pFile;

      pFile = zh_fileExtOpen( szFile, NULL, FO_WRITE | FO_EXCLUSIVE |
                              FXO_DEFAULTS | FXO_SHARELOCK | FXO_NOSEEKPOS |
                              ( uiMode == FILEGET_APPEND ? FXO_APPEND : FXO_TRUNCATE ),
                              NULL, NULL );
      if( ! pFile )
      {
         errCode = uiMode != FILEGET_APPEND ? EDBF_CREATE : EDBF_OPEN_DBF;
      }
      else
      {
         if( zh_fileWriteAt( pFile, pArea->pRecord + pArea->pFieldOffset[ uiIndex ],
                             pField->uiLen, zh_fileSize( pFile ) ) !=
             ( ZH_SIZE ) pField->uiLen )
         {
            errCode = EDBF_WRITE;
         }
         zh_fileClose( pFile );
      }
   }
   else
   {
      errCode = EDBF_DATATYPE;
   }

   /* Exit if any error */
   if( errCode != ZH_SUCCESS )
   {
      zh_dbfErrorRT( pArea, zh_dbfGetEGcode( errCode ), errCode,
                     errCode != EDBF_DATATYPE ? szFile : NULL,
                     errCode != EDBF_DATATYPE ? zh_fsError() : 0,
                     EF_CANDEFAULT, NULL );
      return ZH_FAILURE;
   }
   return ZH_SUCCESS;
}

/*
 * Open a memo file in the specified WorkArea.
 */
static ZH_ERRCODE zh_dbfOpenMemFile( DBFAREAP pArea, LPDBOPENINFO pOpenInfo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfOpenMemFile(%p, %p)", ( void * ) pArea, ( void * ) pOpenInfo ) );

   zh_dbfErrorRT( pArea, EG_OPEN, EDBF_OPEN_DBF, pOpenInfo->abName, 0, 0, NULL );

   return ZH_FAILURE;
}

/*
 * FILE2BLOB - store file contents in MEMO
 */
static ZH_ERRCODE zh_dbfPutValueFile( DBFAREAP pArea, ZH_USHORT uiIndex, const char * szFile, ZH_USHORT uiMode )
{
   ZH_ERRCODE errCode = ZH_SUCCESS;
   LPFIELD pField;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfPutValueFile(%p, %hu, %s, %hu)", ( void * ) pArea, uiIndex, szFile, uiMode ) );

   ZH_SYMBOL_UNUSED( uiMode );

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( &pArea->area ) != ZH_SUCCESS )
         return ZH_FAILURE;
   }

   /* Read record */
   if( ! pArea->fValidBuffer && ! zh_dbfReadRecord( pArea ) )
      return ZH_FAILURE;

   if( --uiIndex >= pArea->area.uiFieldCount )
      return ZH_FAILURE;

   if( ! pArea->fPositioned )
      return ZH_FAILURE;

   /* Buffer is hot? */
   if( ! pArea->fRecordChanged && SELF_GOHOT( &pArea->area ) == ZH_FAILURE )
      return ZH_FAILURE;

   pField = pArea->area.lpFields + uiIndex;
   if( pField->uiType == ZH_FT_STRING )
   {
      PZH_FILE pFile;

      pFile = zh_fileExtOpen( szFile, NULL, FO_READ | FO_DENYNONE |
                              FXO_DEFAULTS | FXO_SHARELOCK | FXO_NOSEEKPOS,
                              NULL, NULL );
      if( ! pFile )
      {
         errCode = EDBF_OPEN_DBF;
      }
      else
      {
         ZH_SIZE nRead = zh_fileReadAt( pFile, pArea->pRecord +
                                               pArea->pFieldOffset[ uiIndex ],
                                        pField->uiLen, 0 );
         if( nRead != ( ZH_SIZE ) FS_ERROR &&
             nRead < ( ZH_SIZE ) pField->uiLen )
            memset( pArea->pRecord + pArea->pFieldOffset[ uiIndex ] + nRead,
                    ' ', pField->uiLen - nRead );
         zh_fileClose( pFile );
      }
   }
   else
   {
      errCode = EDBF_DATATYPE;
   }

   /* Exit if any error */
   if( errCode != ZH_SUCCESS )
   {
      zh_dbfErrorRT( pArea, zh_dbfGetEGcode( errCode ), errCode,
                     errCode != EDBF_DATATYPE ? szFile : NULL,
                     errCode != EDBF_DATATYPE ? zh_fsError() : 0,
                     EF_CANDEFAULT, NULL );
      return ZH_FAILURE;
   }
   return ZH_SUCCESS;
}

/*
 * Read the database file header record in the WorkArea.
 */
static ZH_ERRCODE zh_dbfReadDBHeader( DBFAREAP pArea )
{
   ZH_ERRCODE errCode;
   PZH_ITEM pError;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfReadDBHeader(%p)", ( void * ) pArea ) );

   pError = NULL;
   do
   {
      errCode = ZH_SUCCESS;

      if( zh_fileReadAt( pArea->pDataFile, &pArea->dbfHeader,
                         sizeof( DBFHEADER ), 0 ) != sizeof( DBFHEADER ) )
      {
         errCode = EDBF_READ;
      }
      else
      {
         pArea->fAutoInc = pArea->fModStamp =
         pArea->fTableEncrypted = pArea->fHasMemo = ZH_FALSE;
         pArea->bMemoType  = DB_MEMO_NONE;
         pArea->bCryptType = DB_CRYPT_NONE;
         if( pArea->bTableType == DB_DBF_VFP )
            pArea->bTableType = DB_DBF_STD;

         pArea->fHasTags = ( pArea->dbfHeader.bHasTags & 0x01 ) != 0;

         switch( pArea->dbfHeader.bVersion )
         {
            case 0x31:
               pArea->fAutoInc = ZH_TRUE;
               /* fallthrough */
            case 0x30:
            case 0x32:
               if( pArea->dbfHeader.bHasTags & 0x02 )
               {
                  pArea->bMemoType = DB_MEMO_FPT;
                  pArea->fHasMemo = ZH_TRUE;
               }
               pArea->bTableType = DB_DBF_VFP;
               break;

            case 0x03:
            case 0x07:  /* DBFNTX and ANSI CP */
               break;

            case 0x83:
            case 0x87:  /* DBFNTX+MEMO and ANSI CP */
               pArea->fHasMemo = ZH_TRUE;
               pArea->bMemoType = DB_MEMO_DBT;
               break;

            case 0xE5:
               pArea->fHasMemo = ZH_TRUE;
               pArea->bMemoType = DB_MEMO_SMT;
               break;

            case 0xF5:
               pArea->fHasMemo = ZH_TRUE;
               pArea->bMemoType = DB_MEMO_FPT;
               break;

            case 0x06:
               pArea->fTableEncrypted = ZH_TRUE;
               pArea->bCryptType = DB_CRYPT_SIX;
               break;

            case 0x86:
               pArea->fTableEncrypted = ZH_TRUE;
               pArea->fHasMemo = ZH_TRUE;
               pArea->bCryptType = DB_CRYPT_SIX;
               pArea->bMemoType = DB_MEMO_DBT;
               break;

            case 0xE6:
               pArea->fHasMemo = ZH_TRUE;
               pArea->fTableEncrypted = ZH_TRUE;
               pArea->bCryptType = DB_CRYPT_SIX;
               pArea->bMemoType = DB_MEMO_SMT;
               break;

            case 0xF6:
               pArea->fHasMemo = ZH_TRUE;
               pArea->fTableEncrypted = ZH_TRUE;
               pArea->bCryptType = DB_CRYPT_SIX;
               pArea->bMemoType = DB_MEMO_FPT;
               break;

            default:
               errCode = EDBF_CORRUPT;

         }
         if( errCode == ZH_SUCCESS )
            break;
      }
   }
   while( zh_dbfErrorRT( pArea, zh_dbfGetEGcode( errCode ), errCode,
                         pArea->szDataFileName, zh_fsError(),
                         EF_CANRETRY | EF_CANDEFAULT, &pError ) == E_RETRY );
   if( pError )
      zh_itemRelease( pError );

   if( errCode != ZH_SUCCESS )
      return ZH_FAILURE;

   pArea->uiHeaderLen = ZH_GET_LE_UINT16( pArea->dbfHeader.uiHeaderLen );
   pArea->ulRecCount  = ZH_GET_LE_UINT32( pArea->dbfHeader.ulRecCount );

   return ZH_SUCCESS;
}

/*
 * Write the database file header record in the WorkArea.
 */
static ZH_ERRCODE zh_dbfWriteDBHeader( DBFAREAP pArea )
{
   int iYear, iMonth, iDay;
   ZH_BOOL fLck = ZH_FALSE;
   ZH_ERRCODE errCode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfWriteDBHeader(%p)", ( void * ) pArea ) );

   if( pArea->fReadonly )
   {
      zh_dbfErrorRT( pArea, EG_READONLY, EDBF_READONLY, NULL, 0, 0, NULL );
      return ZH_FAILURE;
   }

   pArea->dbfHeader.bHasTags = pArea->fHasTags ? 0x01 : 0x00;
   if( pArea->bTableType == DB_DBF_VFP )
   {
      pArea->dbfHeader.bVersion = ( pArea->fAutoInc ? 0x31 : 0x30 );
      if( pArea->fHasMemo && pArea->bMemoType == DB_MEMO_FPT )
         pArea->dbfHeader.bHasTags |= 0x02;
   }
   else
   {
      pArea->dbfHeader.bVersion = 0x03;
      if( pArea->fHasMemo )
      {
         if( pArea->bMemoType == DB_MEMO_DBT )
            pArea->dbfHeader.bVersion = 0x83;
         else if( pArea->bMemoType == DB_MEMO_FPT )
            pArea->dbfHeader.bVersion = 0xF5;
         else if( pArea->bMemoType == DB_MEMO_SMT )
            pArea->dbfHeader.bVersion = 0xE5;
      }
      if( pArea->fTableEncrypted && pArea->bCryptType == DB_CRYPT_SIX )
         pArea->dbfHeader.bVersion = ( pArea->dbfHeader.bVersion & 0xf0 ) | 0x06;
   }

   zh_dateToday( &iYear, &iMonth, &iDay );
   pArea->dbfHeader.bYear = ( ZH_BYTE ) ( pArea->bTableType == DB_DBF_STD &&
                                          ( pArea->uiSetHeader & DB_SETHEADER_YYEAR ) == 0 ?
                                          iYear - 1900 : iYear % 100 );
   pArea->dbfHeader.bMonth = ( ZH_BYTE ) iMonth;
   pArea->dbfHeader.bDay = ( ZH_BYTE ) iDay;

   /* Update record count */
   if( pArea->fShared )
   {
      if( ! pArea->fHeaderLocked )
      {
         if( SELF_RAWLOCK( &pArea->area, HEADER_LOCK, 0 ) != ZH_SUCCESS )
            return ZH_FAILURE;
         fLck = ZH_TRUE;
      }
      pArea->ulRecCount = zh_dbfCalcRecCount( pArea );
   }
   else
   {
      /* Exclusive mode */
      /* write eof mark */
      ZH_FOFFSET nOffset = ( ZH_FOFFSET ) pArea->uiHeaderLen +
                           ( ZH_FOFFSET ) pArea->uiRecordLen *
                           ( ZH_FOFFSET ) pArea->ulRecCount;
      zh_fileWriteAt( pArea->pDataFile, "\032", 1, nOffset );
      zh_fileTruncAt( pArea->pDataFile, nOffset + 1 );
   }

   ZH_PUT_LE_UINT32( pArea->dbfHeader.ulRecCount,  pArea->ulRecCount );
   ZH_PUT_LE_UINT16( pArea->dbfHeader.uiHeaderLen, pArea->uiHeaderLen );
   ZH_PUT_LE_UINT16( pArea->dbfHeader.uiRecordLen, pArea->uiRecordLen );
   if( zh_fileWriteAt( pArea->pDataFile, &pArea->dbfHeader,
                       sizeof( DBFHEADER ), 0 ) == sizeof( DBFHEADER ) )
      errCode = ZH_SUCCESS;
   else
      errCode = ZH_FAILURE;

   pArea->fDataFlush = ZH_TRUE;
   pArea->fUpdateHeader = ZH_FALSE;
   if( fLck )
   {
      if( SELF_RAWLOCK( &pArea->area, HEADER_UNLOCK, 0 ) != ZH_SUCCESS )
         return ZH_FAILURE;
   }

   if( errCode != ZH_SUCCESS )
      zh_dbfErrorRT( pArea, EG_WRITE, EDBF_WRITE, pArea->szDataFileName,
                     zh_fsError(), 0, NULL );

   return errCode;
}

static ZH_ERRCODE zh_dbfDrop( LPRDDNODE pRDD, PZH_ITEM pItemTable, PZH_ITEM pItemIndex, ZH_ULONG ulConnect )
{
   char szFileName[ ZH_PATH_MAX ];
   const char * szFile, * szExt;
   PZH_ITEM pFileExt = NULL;
   PZH_FNAME pFileName;
   ZH_BOOL fTable = ZH_FALSE, fResult = ZH_FALSE;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfDrop(%p,%p,%p,%lu)", ( void * ) pRDD, ( void * ) pItemTable, ( void * ) pItemIndex, ulConnect ) );

   szFile = zh_itemGetCPtr( pItemIndex );
   if( ! szFile[ 0 ] )
   {
      /* Try to delete index file */
      szFile = zh_itemGetCPtr( pItemTable );
      if( ! szFile[ 0 ] )
         return ZH_FAILURE;
      fTable = ZH_TRUE;
   }

   pFileName = zh_fsFNameSplit( szFile );

   if( ! pFileName->szExtension && ( ! fTable || zh_setGetDefExtension() ) )
   {
      /* Add default extension if missing */
      pFileExt = zh_itemPutNil( pFileExt );
      if( SELF_RDDINFO( pRDD, fTable ? RDDI_TABLEEXT : RDDI_ORDBAGEXT, ulConnect, pFileExt ) == ZH_SUCCESS )
         pFileName->szExtension = zh_itemGetCPtr( pFileExt );
   }
   zh_fsFNameMerge( szFileName, pFileName );
   zh_xfree( pFileName );

   /* Use zh_fileExists() first to locate table which can be in different path */
   if( zh_fileExists( szFileName, szFileName ) )
   {
      fResult = zh_fileDelete( szFileName );
      if( fResult && fTable )
      {
         /*
          * Database table file has been deleted, now check if memo is
          * supported and if yes then try to delete memo file if it exists
          * in the same directory as table file
          * zh_fsFNameSplit() repeated intentionally to respect
          * the path set by zh_FileExists()
          */
         pFileName = zh_fsFNameSplit( szFileName );
         pFileExt = zh_itemPutNil( pFileExt );
         if( SELF_RDDINFO( pRDD, RDDI_MEMOEXT, ulConnect, pFileExt ) == ZH_SUCCESS )
         {
            szExt = zh_itemGetCPtr( pFileExt );
            if( szExt[ 0 ] )
            {
               pFileName->szExtension = szExt;
               zh_fsFNameMerge( szFileName, pFileName );
               zh_fileDelete( szFileName );
            }
         }
         /*
          * and try to delete production index also if it exists
          * in the same directory as table file
          */
         zh_itemClear( pFileExt );
         if( SELF_RDDINFO( pRDD, RDDI_ORDSTRUCTEXT, ulConnect, pFileExt ) == ZH_SUCCESS )
         {
            szExt = zh_itemGetCPtr( pFileExt );
            if( szExt[ 0 ] )
            {
               pFileName->szExtension = szExt;
               zh_fsFNameMerge( szFileName, pFileName );
               zh_fileDelete( szFileName );
            }
         }
         zh_xfree( pFileName );
      }
   }

   if( pFileExt )
      zh_itemRelease( pFileExt );

   return fResult ? ZH_SUCCESS : ZH_FAILURE;
}

static ZH_ERRCODE zh_dbfExists( LPRDDNODE pRDD, PZH_ITEM pItemTable, PZH_ITEM pItemIndex, ZH_ULONG ulConnect )
{
   char szFileName[ ZH_PATH_MAX ];
   const char * szFile;
   PZH_ITEM pFileExt = NULL;
   PZH_FNAME pFileName;
   ZH_BOOL fTable = ZH_FALSE;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfExists(%p,%p,%p,%lu)", ( void * ) pRDD, ( void * ) pItemTable, ( void * ) pItemIndex, ulConnect ) );

   szFile = zh_itemGetCPtr( pItemIndex );
   if( ! szFile[ 0 ] )
   {
      szFile = zh_itemGetCPtr( pItemTable );
      if( ! szFile[ 0 ] )
         return ZH_FAILURE;
      fTable = ZH_TRUE;
   }

   pFileName = zh_fsFNameSplit( szFile );

   if( ! pFileName->szExtension && ( ! fTable || zh_setGetDefExtension() ) )
   {
      pFileExt = zh_itemPutNil( pFileExt );
      if( SELF_RDDINFO( pRDD, fTable ? RDDI_TABLEEXT : RDDI_ORDBAGEXT, ulConnect, pFileExt ) == ZH_SUCCESS )
         pFileName->szExtension = zh_itemGetCPtr( pFileExt );
   }
   zh_fsFNameMerge( szFileName, pFileName );
   zh_xfree( pFileName );

   if( pFileExt )
      zh_itemRelease( pFileExt );

   return zh_fileExists( szFileName, szFileName ) ? ZH_SUCCESS : ZH_FAILURE;
}

static ZH_ERRCODE zh_dbfRename( LPRDDNODE pRDD, PZH_ITEM pItemTable, PZH_ITEM pItemIndex, PZH_ITEM pItemNew, ZH_ULONG ulConnect )
{
   char szFileName[ ZH_PATH_MAX ];
   const char * szFile, * szExt;
   PZH_ITEM pFileExt = NULL;
   PZH_FNAME pFileName;
   ZH_BOOL fTable = ZH_FALSE, fResult = ZH_FALSE;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfRename(%p,%p,%p,%p,%lu)", ( void * ) pRDD, ( void * ) pItemTable, ( void * ) pItemIndex, ( void * ) pItemNew, ulConnect ) );

   szFile = zh_itemGetCPtr( pItemIndex );
   if( ! szFile[ 0 ] )
   {
      /* Try to delete index file */
      szFile = zh_itemGetCPtr( pItemTable );
      if( ! szFile[ 0 ] )
         return ZH_FAILURE;
      fTable = ZH_TRUE;
   }

   pFileName = zh_fsFNameSplit( szFile );

   if( ! pFileName->szExtension && ( ! fTable || zh_setGetDefExtension() ) )
   {
      /* Add default extension if missing */
      pFileExt = zh_itemPutNil( pFileExt );
      if( SELF_RDDINFO( pRDD, fTable ? RDDI_TABLEEXT : RDDI_ORDBAGEXT, ulConnect, pFileExt ) == ZH_SUCCESS )
         pFileName->szExtension = zh_itemGetCPtr( pFileExt );
   }
   zh_fsFNameMerge( szFileName, pFileName );
   zh_xfree( pFileName );

   szFile = zh_itemGetCPtr( pItemNew );
   /* Use zh_fileExists() first to locate table which can be in different path */
   if( szFile[ 0 ] && zh_fileExists( szFileName, szFileName ) )
   {
      char szFileNew[ ZH_PATH_MAX ];
      PZH_FNAME pFileNameNew;

      /* zh_fsFNameSplit() repeated intentionally to respect
       * the path set by zh_FileExists()
       */
      pFileName = zh_fsFNameSplit( szFileName );

      pFileNameNew = zh_fsFNameSplit( szFile );
      if( ! pFileNameNew->szExtension && ( ! fTable || zh_setGetDefExtension() ) )
      {
         /* Add default extension if missing */
         pFileExt = zh_itemPutNil( pFileExt );
         if( SELF_RDDINFO( pRDD, fTable ? RDDI_TABLEEXT : RDDI_ORDBAGEXT, ulConnect, pFileExt ) == ZH_SUCCESS )
            pFileNameNew->szExtension = zh_itemGetCPtr( pFileExt );
      }
      if( ! pFileNameNew->szPath )
         pFileNameNew->szPath = pFileName->szPath;
      zh_fsFNameMerge( szFileNew, pFileNameNew );

      fResult = zh_fileRename( szFileName, szFileNew );
      if( fResult && fTable )
      {
         /*
          * Database table file has been renamed, now check if memo is
          * supported and if yes then try to rename memo file if it exists
          * in the same directory as table file
          */
         pFileExt = zh_itemPutNil( pFileExt );
         if( SELF_RDDINFO( pRDD, RDDI_MEMOEXT, ulConnect, pFileExt ) == ZH_SUCCESS )
         {
            szExt = zh_itemGetCPtr( pFileExt );
            if( szExt[ 0 ] )
            {
               pFileName->szExtension = szExt;
               pFileNameNew->szExtension = szExt;
               zh_fsFNameMerge( szFileName, pFileName );
               zh_fsFNameMerge( szFileNew, pFileNameNew );
               zh_fileRename( szFileName, szFileNew );
            }
         }
         /*
          * and try to rename production index also if it exists
          * in the same directory as table file
          */
         zh_itemClear( pFileExt );
         if( SELF_RDDINFO( pRDD, RDDI_ORDSTRUCTEXT, ulConnect, pFileExt ) == ZH_SUCCESS )
         {
            szExt = zh_itemGetCPtr( pFileExt );
            if( szExt[ 0 ] )
            {
               pFileName->szExtension = szExt;
               pFileNameNew->szExtension = szExt;
               zh_fsFNameMerge( szFileName, pFileName );
               zh_fsFNameMerge( szFileNew, pFileNameNew );
               zh_fileRename( szFileName, szFileNew );
            }
         }
      }
      zh_xfree( pFileName );
      zh_xfree( pFileNameNew );
   }

   if( pFileExt )
      zh_itemRelease( pFileExt );

   return fResult ? ZH_SUCCESS : ZH_FAILURE;
}

static void zh_dbfInitTSD( void * Cargo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfInitTSD(%p)", Cargo ) );

   ( ( LPDBFDATA ) Cargo )->bTableType = DB_DBF_STD;
   ( ( LPDBFDATA ) Cargo )->bCryptType = DB_CRYPT_NONE;
   ( ( LPDBFDATA ) Cargo )->uiDirtyRead = ZH_IDXREAD_CLEANMASK;
   ( ( LPDBFDATA ) Cargo )->uiSetHeader = DB_SETHEADER_APPENDSYNC;
}

static void zh_dbfDestroyTSD( void * Cargo )
{
   LPDBFDATA pData;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfDestroyTSD(%p)", Cargo ) );

   pData = ( LPDBFDATA ) Cargo;

   if( pData->szTrigger )
      zh_xfree( pData->szTrigger );
   if( pData->szPendingTrigger )
      zh_xfree( pData->szPendingTrigger );
   if( pData->szPasswd )
      zh_xfree( pData->szPasswd );
   if( pData->szPendingPasswd )
      zh_xfree( pData->szPendingPasswd );
}

static ZH_ERRCODE zh_dbfInit( LPRDDNODE pRDD )
{
   PZH_TSD pTSD;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfInit(%p)", ( void * ) pRDD ) );

   pTSD = ( PZH_TSD ) zh_xgrab( sizeof( ZH_TSD ) );
   ZH_TSD_INIT( pTSD, sizeof( DBFDATA ), zh_dbfInitTSD, zh_dbfDestroyTSD );
   pRDD->lpvCargo = ( void * ) pTSD;

   if( ISSUPER_INIT( pRDD ) )
      return SUPER_INIT( pRDD );
   else
      return ZH_SUCCESS;
}

static ZH_ERRCODE zh_dbfExit( LPRDDNODE pRDD )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfExit(%p)", ( void * ) pRDD ) );

   if( pRDD->lpvCargo )
   {
      zh_stackReleaseTSD( ( PZH_TSD ) pRDD->lpvCargo );
      zh_xfree( pRDD->lpvCargo );
      pRDD->lpvCargo = NULL;
   }
   s_uiRddId = ( ZH_USHORT ) -1;

   if( ISSUPER_EXIT( pRDD ) )
      return SUPER_EXIT( pRDD );
   else
      return ZH_SUCCESS;
}

static ZH_ERRCODE zh_dbfRddInfo( LPRDDNODE pRDD, ZH_USHORT uiIndex, ZH_ULONG ulConnect, PZH_ITEM pItem )
{
   LPDBFDATA pData;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbfRddInfo(%p, %hu, %lu, %p)", ( void * ) pRDD, uiIndex, ulConnect, pItem ) );

   pData = DBFNODE_DATA( pRDD );

   switch( uiIndex )
   {
      case RDDI_ISDBF:
      case RDDI_CANPUTREC:
      case RDDI_LOCAL:
         zh_itemPutL( pItem, ZH_TRUE );
         break;

      case RDDI_TABLEEXT:
      {
         const char * szNew = zh_itemGetCPtr( pItem );
         char * szNewVal;

         szNewVal = szNew[ 0 ] == '.' && szNew[ 1 ] ? zh_strdup( szNew ) : NULL;
         zh_itemPutC( pItem, pData->szTableExt[ 0 ] ? pData->szTableExt : DBF_TABLEEXT );
         if( szNewVal )
         {
            zh_strncpy( pData->szTableExt, szNewVal, sizeof( pData->szTableExt ) - 1 );
            zh_xfree( szNewVal );
         }
         break;
      }
      case RDDI_TABLETYPE:
      {
         int iType = zh_itemGetNI( pItem );
         zh_itemPutNI( pItem, pData->bTableType ? pData->bTableType : DB_DBF_STD );
         switch( iType )
         {
            case DB_DBF_STD:        /* standard DBF file */
            case DB_DBF_VFP:        /* VFP DBF file */
               pData->bTableType = ( ZH_BYTE ) iType;
         }
         break;
      }
      case RDDI_LOCKSCHEME:
      {
         int iScheme = zh_itemGetNI( pItem );

         zh_itemPutNI( pItem, pData->bLockType ? pData->bLockType :
                              zh_setGetDBFLockScheme() );
         switch( iScheme )
         {
            case DB_DBFLOCK_COMIX:
            case DB_DBFLOCK_VFP:
            case DB_DBFLOCK_HB32:
#ifndef ZH_LONG_LONG_OFF
            case DB_DBFLOCK_HB64:
#endif
               pData->bLockType = ( ZH_BYTE ) iScheme;
         }
         break;
      }
      case RDDI_SETHEADER:
      {
         ZH_USHORT uiSetHeader = pData->uiSetHeader;

         if( ZH_IS_NUMERIC( pItem ) )
         {
            int iMode = zh_itemGetNI( pItem );
            if( ( iMode & ~0xFF ) == 0 )
               pData->uiSetHeader = ( ZH_USHORT ) iMode;
         }
         zh_itemPutNI( pItem, uiSetHeader );
         break;
      }
      case RDDI_DIRTYREAD:
      {
         ZH_BOOL fDirty = ( pData->uiDirtyRead == ZH_IDXREAD_DIRTYMASK );
         if( ZH_IS_LOGICAL( pItem ) )
         {
            pData->uiDirtyRead = zh_itemGetL( pItem ) ?
                                 ZH_IDXREAD_DIRTYMASK : ZH_IDXREAD_CLEANMASK;
         }
         zh_itemPutL( pItem, fDirty );
         break;
      }
      case RDDI_INDEXPAGESIZE:
      {
         int iPageSize = zh_itemGetNI( pItem );

         zh_itemPutNI( pItem, pData->uiIndexPageSize );
         if( iPageSize >= 0x200 && iPageSize <= 0x2000 &&
             ( ( iPageSize - 1 ) & iPageSize ) == 0 )
            pData->uiIndexPageSize = ( ZH_USHORT ) iPageSize;
         break;
      }
      case RDDI_DECIMALS:
      {
         int iDecimals = ZH_IS_NUMERIC( pItem ) ? zh_itemGetNI( pItem ) : -1;

         zh_itemPutNI( pItem, pData->bDecimals );
         if( iDecimals >= 0 && iDecimals <= 20 )
            pData->bDecimals = ( ZH_BYTE ) iDecimals;
         break;
      }
      case RDDI_TRIGGER:
      {
         char * szTrigger = pData->szTrigger;
         ZH_BOOL fFree = ZH_FALSE;

         if( ZH_IS_STRING( pItem ) )
         {
            fFree = ZH_TRUE;
            pData->szTrigger = zh_itemGetCLen( pItem ) > 0 ?
                               zh_itemGetC( pItem ) : NULL;
         }

         if( fFree && szTrigger )
            zh_itemPutCPtr( pItem, szTrigger );
         else
            zh_itemPutC( pItem, szTrigger );

         if( ! szTrigger && ! fFree )
            return ZH_FAILURE;

         break;
      }
      case RDDI_PENDINGTRIGGER:
         if( ZH_IS_STRING( pItem ) )
         {
            if( pData->szPendingTrigger )
            {
               zh_xfree( pData->szPendingTrigger );
               pData->szPendingTrigger = NULL;
            }
            if( zh_itemGetCLen( pItem ) > 0 )
               pData->szPendingTrigger = zh_itemGetC( pItem );
         }
         else if( pData->szPendingTrigger )
         {
            zh_itemPutCPtr( pItem, pData->szPendingTrigger );
            pData->szPendingTrigger = NULL;
         }
         else
            return ZH_FAILURE;
         break;

      case RDDI_PASSWORD:
      {
         char * szPasswd = pData->szPasswd;
         ZH_BOOL fFree = ZH_FALSE;

         if( ZH_IS_STRING( pItem ) )
         {
            fFree = ZH_TRUE;
            pData->szPasswd = zh_itemGetCLen( pItem ) > 0 ?
                              zh_itemGetC( pItem ) : NULL;
         }

         if( fFree && szPasswd )
            zh_itemPutCPtr( pItem, szPasswd );
         else
            zh_itemPutC( pItem, szPasswd );

         if( ! szPasswd && ! fFree )
            return ZH_FAILURE;

         break;
      }
      case RDDI_PENDINGPASSWORD:
         if( ZH_IS_STRING( pItem ) )
         {
            if( pData->szPendingPasswd )
            {
               zh_xfree( pData->szPendingPasswd );
               pData->szPendingPasswd = NULL;
            }
            if( zh_itemGetCLen( pItem ) > 0 )
               pData->szPendingPasswd = zh_itemGetC( pItem );
         }
         else if( pData->szPendingPasswd )
         {
            zh_itemPutCPtr( pItem, pData->szPendingPasswd );
            pData->szPendingPasswd = NULL;
         }
         else
            return ZH_FAILURE;
         break;

      default:
         return SUPER_RDDINFO( pRDD, uiIndex, ulConnect, pItem );

   }

   return ZH_SUCCESS;
}

#define zh_dbfWhoCares  NULL


static const RDDFUNCS dbfTable =
{
   ( DBENTRYP_BP ) zh_dbfBof,
   ( DBENTRYP_BP ) zh_dbfEof,
   ( DBENTRYP_BP ) zh_dbfFound,
   ( DBENTRYP_V ) zh_dbfGoBottom,
   ( DBENTRYP_UL ) zh_dbfGoTo,
   ( DBENTRYP_I ) zh_dbfGoToId,
   ( DBENTRYP_V ) zh_dbfGoTop,
   ( DBENTRYP_BIB ) zh_dbfSeek,
   ( DBENTRYP_L ) zh_dbfSkip,
   ( DBENTRYP_L ) zh_dbfSkipFilter,
   ( DBENTRYP_L ) zh_dbfSkipRaw,
   ( DBENTRYP_VF ) zh_dbfAddField,
   ( DBENTRYP_B ) zh_dbfAppend,
   ( DBENTRYP_I ) zh_dbfCreateFields,
   ( DBENTRYP_V ) zh_dbfDeleteRec,
   ( DBENTRYP_BP ) zh_dbfDeleted,
   ( DBENTRYP_SP ) zh_dbfFieldCount,
   ( DBENTRYP_VF ) zh_dbfFieldDisplay,
   ( DBENTRYP_SSI ) zh_dbfFieldInfo,
   ( DBENTRYP_SCP ) zh_dbfFieldName,
   ( DBENTRYP_V ) zh_dbfFlush,
   ( DBENTRYP_PP ) zh_dbfGetRec,
   ( DBENTRYP_SI ) zh_dbfGetValue,
   ( DBENTRYP_SVL ) zh_dbfGetVarLen,
   ( DBENTRYP_V ) zh_dbfGoCold,
   ( DBENTRYP_V ) zh_dbfGoHot,
   ( DBENTRYP_P ) zh_dbfPutRec,
   ( DBENTRYP_SI ) zh_dbfPutValue,
   ( DBENTRYP_V ) zh_dbfRecall,
   ( DBENTRYP_ULP ) zh_dbfRecCount,
   ( DBENTRYP_ISI ) zh_dbfRecInfo,
   ( DBENTRYP_ULP ) zh_dbfRecNo,
   ( DBENTRYP_I ) zh_dbfRecId,
   ( DBENTRYP_S ) zh_dbfSetFieldExtent,
   ( DBENTRYP_CP ) zh_dbfAlias,
   ( DBENTRYP_V ) zh_dbfClose,
   ( DBENTRYP_VO ) zh_dbfCreate,
   ( DBENTRYP_SI ) zh_dbfInfo,
   ( DBENTRYP_V ) zh_dbfNewArea,
   ( DBENTRYP_VO ) zh_dbfOpen,
   ( DBENTRYP_V ) zh_dbfRelease,
   ( DBENTRYP_SP ) zh_dbfStructSize,
   ( DBENTRYP_CP ) zh_dbfSysName,
   ( DBENTRYP_VEI ) zh_dbfEval,
   ( DBENTRYP_V ) zh_dbfPack,
   ( DBENTRYP_LSP ) zh_dbfPackRec,
   ( DBENTRYP_VS ) zh_dbfSort,
   ( DBENTRYP_VT ) zh_dbfTrans,
   ( DBENTRYP_VT ) zh_dbfTransRec,
   ( DBENTRYP_V ) zh_dbfZap,
   ( DBENTRYP_VR ) zh_dbfChildEnd,
   ( DBENTRYP_VR ) zh_dbfChildStart,
   ( DBENTRYP_VR ) zh_dbfChildSync,
   ( DBENTRYP_V ) zh_dbfSyncChildren,
   ( DBENTRYP_V ) zh_dbfClearRel,
   ( DBENTRYP_V ) zh_dbfForceRel,
   ( DBENTRYP_SSP ) zh_dbfRelArea,
   ( DBENTRYP_VR ) zh_dbfRelEval,
   ( DBENTRYP_SI ) zh_dbfRelText,
   ( DBENTRYP_VR ) zh_dbfSetRel,
   ( DBENTRYP_VOI ) zh_dbfOrderListAdd,
   ( DBENTRYP_V ) zh_dbfOrderListClear,
   ( DBENTRYP_VOI ) zh_dbfOrderListDelete,
   ( DBENTRYP_VOI ) zh_dbfOrderListFocus,
   ( DBENTRYP_V ) zh_dbfOrderListRebuild,
   ( DBENTRYP_VOO ) zh_dbfOrderCondition,
   ( DBENTRYP_VOC ) zh_dbfOrderCreate,
   ( DBENTRYP_VOI ) zh_dbfOrderDestroy,
   ( DBENTRYP_SVOI ) zh_dbfOrderInfo,
   ( DBENTRYP_V ) zh_dbfClearFilter,
   ( DBENTRYP_V ) zh_dbfClearLocate,
   ( DBENTRYP_V ) zh_dbfClearScope,
   ( DBENTRYP_VPLP ) zh_dbfCountScope,
   ( DBENTRYP_I ) zh_dbfFilterText,
   ( DBENTRYP_SI ) zh_dbfScopeInfo,
   ( DBENTRYP_VFI ) zh_dbfSetFilter,
   ( DBENTRYP_VLO ) zh_dbfSetLocate,
   ( DBENTRYP_VOS ) zh_dbfSetScope,
   ( DBENTRYP_VPL ) zh_dbfSkipScope,
   ( DBENTRYP_B ) zh_dbfLocate,
   ( DBENTRYP_CC ) zh_dbfCompile,
   ( DBENTRYP_I ) zh_dbfError,
   ( DBENTRYP_I ) zh_dbfEvalBlock,
   ( DBENTRYP_VSP ) zh_dbfRawLock,
   ( DBENTRYP_VL ) zh_dbfLock,
   ( DBENTRYP_I ) zh_dbfUnLock,
   ( DBENTRYP_V ) zh_dbfCloseMemFile,
   ( DBENTRYP_VO ) zh_dbfCreateMemFile,
   ( DBENTRYP_SCCS ) zh_dbfGetValueFile,
   ( DBENTRYP_VO ) zh_dbfOpenMemFile,
   ( DBENTRYP_SCCS ) zh_dbfPutValueFile,
   ( DBENTRYP_V ) zh_dbfReadDBHeader,
   ( DBENTRYP_V ) zh_dbfWriteDBHeader,
   ( DBENTRYP_R ) zh_dbfInit,
   ( DBENTRYP_R ) zh_dbfExit,
   ( DBENTRYP_RVVL ) zh_dbfDrop,
   ( DBENTRYP_RVVL ) zh_dbfExists,
   ( DBENTRYP_RVVVL ) zh_dbfRename,
   ( DBENTRYP_RSLV ) zh_dbfRddInfo,
   ( DBENTRYP_SVP ) zh_dbfWhoCares
};

ZH_FUNC( _DBF ) { ; }

ZH_FUNC_STATIC( DBF_GETFUNCTABLE )
{
   RDDFUNCS * pTable;
   ZH_USHORT * puiCount, uiRddId;

   puiCount = ( ZH_USHORT * ) zh_parptr( 1 );
   pTable = ( RDDFUNCS * ) zh_parptr( 2 );
   uiRddId = ( ZH_USHORT ) zh_parni( 4 );

   ZH_TRACE( ZH_TR_DEBUG, ( "DBF_GETFUNCTABLE(%p, %p)", ( void * ) puiCount, ( void * ) pTable ) );

   if( pTable )
   {
      ZH_ERRCODE errCode;

      if( puiCount )
         *puiCount = RDDFUNCSCOUNT;
      errCode = zh_rddInheritEx( pTable, &dbfTable, &dbfSuper, NULL, NULL );
      zh_retni( errCode );
      if( errCode == ZH_SUCCESS )
      {
         /*
          * we successfully register our RDD so now we can initialize it
          * You may think that this place is RDD init statement, Druzus
          */
         s_uiRddId = uiRddId;
      }
   }
   else
      zh_retni( ZH_FAILURE );
}

static void zh_dbfRddInit( void * cargo )
{
   ZH_SYMBOL_UNUSED( cargo );

   if( zh_rddRegister( "DBF", RDD_REGISTER_TYPE_FULL ) > 1 )
      zh_errInternal( ZH_EI_RDDINVALID, NULL, NULL, NULL );
}

ZH_INIT_SYMBOLS_BEGIN( dbf1__InitSymbols )
{ "_DBF",             {ZH_FS_PUBLIC|ZH_FS_LOCAL}, {ZH_FUNCNAME( _DBF )}, NULL },
{ "DBF_GETFUNCTABLE", {ZH_FS_PUBLIC|ZH_FS_LOCAL}, {ZH_FUNCNAME( DBF_GETFUNCTABLE )}, NULL }
ZH_INIT_SYMBOLS_END( dbf1__InitSymbols )

ZH_CALL_ON_STARTUP_BEGIN( _zh_dbf_rdd_init_ )
   zh_vmAtInit( zh_dbfRddInit, NULL );
ZH_CALL_ON_STARTUP_END( _zh_dbf_rdd_init_ )

#if defined( ZH_PRAGMA_STARTUP )
   #pragma startup dbf1__InitSymbols
   #pragma startup _zh_dbf_rdd_init_
#elif defined( ZH_DATASEG_STARTUP )
   #define ZH_DATASEG_BODY    ZH_DATASEG_FUNC( dbf1__InitSymbols ) \
                              ZH_DATASEG_FUNC( _zh_dbf_rdd_init_ )
   #include "zh_ini_seg.h"
#endif
