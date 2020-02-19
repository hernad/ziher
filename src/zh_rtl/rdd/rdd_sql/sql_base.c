/*
 * SQL Base Database Driver
 *
 * Copyright 2007 Mindaugas Kavaliauskas <dbtopas at dbtopas.lt>
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
#include "zh_vm.h"
#include "zh_set.h"
#include "zh_rdd_sql.h"
#include "zh_rtl/rdd_sys.zhh"

#include "zh_trace.h"

#define SUPERTABLE              ( &sqlbaseSuper )

#define CONNECTION_LIST_EXPAND  4

static ZH_USHORT s_rddidSQLBASE = 0;

static SQLDDCONNECTION ** s_pConnection = NULL;
static ZH_ULONG s_ulConnectionCount     = 0;
static ZH_ULONG s_ulConnectionCurrent   = 0;

static char *     s_szError = NULL;
static ZH_ERRCODE s_errCode = 0;

static char *        s_szQuery        = NULL;
static PZH_ITEM      s_pItemNewID     = NULL;
static unsigned long s_ulAffectedRows = 0;

static RDDFUNCS sqlbaseSuper;


void zh_rddsqlSetError( ZH_ERRCODE errCode, const char * szError, const char * szQuery, PZH_ITEM pItem, unsigned long ulAffectedRows )
{
   s_errCode = errCode;

   if( s_szError )
   {
      zh_xfree( s_szError );
      s_szError = NULL;
   }
   if( szError )
      s_szError = zh_strdup( szError );

   if( s_szQuery )
   {
      zh_xfree( s_szQuery );
      s_szQuery = NULL;
   }
   if( szQuery )
      s_szQuery = zh_strdup( szQuery );

   if( pItem )
      zh_itemCopy( s_pItemNewID, pItem );
   else
      zh_itemClear( s_pItemNewID );

   s_ulAffectedRows = ulAffectedRows;
}


static ZH_ERRCODE zh_errRT_SQLBASE( ZH_ERRCODE errGenCode, ZH_ERRCODE errSubCode, const char * szDescription, const char * szOperation )
{
   ZH_ERRCODE iRet = ZH_FAILURE;

   if( zh_vmRequestQuery() == 0 )
   {
      PZH_ITEM pError;
      pError = zh_errRT_New( ES_ERROR, "SQLBASE", errGenCode, errSubCode, szDescription, szOperation, 0, EF_NONE );
      iRet   = zh_errLaunch( pError );
      zh_itemRelease( pError );
   }
   return iRet;
}


/* --- NULL SDD --- */

static ZH_ERRCODE sddConnect( SQLDDCONNECTION * pConnection, PZH_ITEM pItem );
static ZH_ERRCODE sddDisconnect( SQLDDCONNECTION * pConnection );
static ZH_ERRCODE sddExecute( SQLDDCONNECTION * pConnection, PZH_ITEM pItem );
static ZH_ERRCODE sddOpen( SQLBASEAREAP pArea );
static ZH_ERRCODE sddClose( SQLBASEAREAP pArea );
static ZH_ERRCODE sddGoTo( SQLBASEAREAP pArea, ZH_ULONG ulRecNo );
static ZH_ERRCODE sddGetValue( SQLBASEAREAP pArea, ZH_USHORT uiIndex, PZH_ITEM pItem );
static ZH_ERRCODE sddGetVarLen( SQLBASEAREAP pArea, ZH_USHORT uiIndex, ZH_ULONG * pLength );


static const SDDNODE s_sddNull = {
   NULL,
   "NULL",
   ( SDDFUNC_CONNECT ) sddConnect,
   ( SDDFUNC_DISCONNECT ) sddDisconnect,
   ( SDDFUNC_EXECUTE ) sddExecute,
   ( SDDFUNC_OPEN ) sddOpen,
   ( SDDFUNC_CLOSE ) sddClose,
   ( SDDFUNC_GOTO ) sddGoTo,
   ( SDDFUNC_GETVALUE ) sddGetValue,
   ( SDDFUNC_GETVARLEN ) sddGetVarLen
};


static ZH_ERRCODE sddConnect( SQLDDCONNECTION * pConnection, PZH_ITEM pItem )
{
   ZH_SYMBOL_UNUSED( pConnection );
   ZH_SYMBOL_UNUSED( pItem );
   zh_errRT_SQLBASE( EG_UNSUPPORTED, ESQLDD_NULLSDD, NULL, NULL );
   return ZH_FAILURE;
}


static ZH_ERRCODE sddDisconnect( SQLDDCONNECTION * pConnection )
{
   ZH_SYMBOL_UNUSED( pConnection );
   zh_errRT_SQLBASE( EG_UNSUPPORTED, ESQLDD_NULLSDD, NULL, NULL );
   return ZH_FAILURE;
}


static ZH_ERRCODE sddExecute( SQLDDCONNECTION * pConnection, PZH_ITEM pItem )
{
   ZH_SYMBOL_UNUSED( pConnection );
   ZH_SYMBOL_UNUSED( pItem );
   zh_errRT_SQLBASE( EG_UNSUPPORTED, ESQLDD_NULLSDD, NULL, NULL );
   return ZH_FAILURE;
}


static ZH_ERRCODE sddOpen( SQLBASEAREAP pArea )
{
   ZH_SYMBOL_UNUSED( pArea );
   zh_errRT_SQLBASE( EG_UNSUPPORTED, ESQLDD_NULLSDD, NULL, NULL );
   return ZH_FAILURE;
}


static ZH_ERRCODE sddClose( SQLBASEAREAP pArea )
{
   ZH_SYMBOL_UNUSED( pArea );
   return ZH_SUCCESS;
}


static ZH_ERRCODE sddGoTo( SQLBASEAREAP pArea, ZH_ULONG ulRecNo )
{
   if( ulRecNo == 0 || ulRecNo > pArea->ulRecCount )
   {
      pArea->pRecord      = pArea->pRow[ 0 ];
      pArea->bRecordFlags = pArea->pRowFlags[ 0 ];

      pArea->fPositioned = ZH_FALSE;
   }
   else
   {
      pArea->pRecord      = pArea->pRow[ ulRecNo ];
      pArea->bRecordFlags = pArea->pRowFlags[ ulRecNo ];

      pArea->fPositioned = ZH_TRUE;
   }
   return ZH_SUCCESS;
}


static ZH_ERRCODE sddGetValue( SQLBASEAREAP pArea, ZH_USHORT uiIndex, PZH_ITEM pItem )
{
   ZH_SYMBOL_UNUSED( pArea );
   ZH_SYMBOL_UNUSED( uiIndex );
   ZH_SYMBOL_UNUSED( pItem );
   zh_errRT_SQLBASE( EG_UNSUPPORTED, ESQLDD_NULLSDD, NULL, NULL );
   return ZH_FAILURE;
}


static ZH_ERRCODE sddGetVarLen( SQLBASEAREAP pArea, ZH_USHORT uiIndex, ZH_ULONG * pLength )
{
   ZH_SYMBOL_UNUSED( pArea );
   ZH_SYMBOL_UNUSED( uiIndex );
   ZH_SYMBOL_UNUSED( pLength );
   zh_errRT_SQLBASE( EG_UNSUPPORTED, ESQLDD_NULLSDD, NULL, NULL );
   return ZH_SUCCESS;
}


/* --- SDD registration --- */

static PSDDNODE s_pSdd = NULL;


int zh_sddRegister( PSDDNODE pSdd )
{
   PSDDNODE pNode = s_pSdd;

   /* "Inheritance" from NULL SDD */
   if( pSdd->Connect == NULL )
      pSdd->Connect = s_sddNull.Connect;
   if( pSdd->Disconnect == NULL )
      pSdd->Disconnect = s_sddNull.Disconnect;
   if( pSdd->Execute == NULL )
      pSdd->Execute = s_sddNull.Execute;
   if( pSdd->Open == NULL )
      pSdd->Open = s_sddNull.Open;
   if( pSdd->Close == NULL )
      pSdd->Close = s_sddNull.Close;
   if( pSdd->GoTo == NULL )
      pSdd->GoTo = s_sddNull.GoTo;
   if( pSdd->GetValue == NULL )
      pSdd->GetValue = s_sddNull.GetValue;
   if( pSdd->GetVarLen == NULL )
      pSdd->GetVarLen = s_sddNull.GetVarLen;

   while( pNode )
   {
      if( ! zh_stricmp( pNode->Name, pSdd->Name ) )
         return 0;
      pNode = pNode->pNext;
   }
   pSdd->pNext = s_pSdd;
   s_pSdd      = pSdd;
   return 1;
}


/* --- RDD METHODS --- */

static ZH_ERRCODE sqlbaseGoBottom( SQLBASEAREAP pArea )
{
   if( SELF_GOCOLD( &pArea->area ) == ZH_FAILURE )
      return ZH_FAILURE;


   if( ! pArea->fFetched && pArea->pSDD->GoTo( pArea, ( ZH_ULONG ) -1 ) == ZH_FAILURE )
      return ZH_FAILURE;

   pArea->area.fTop    = ZH_FALSE;
   pArea->area.fBottom = ZH_TRUE;

   if( SELF_GOTO( &pArea->area, pArea->ulRecCount ) != ZH_SUCCESS )
      return ZH_FAILURE;

   return SELF_SKIPFILTER( &pArea->area, -1 );
}


static ZH_ERRCODE sqlbaseGoTo( SQLBASEAREAP pArea, ZH_ULONG ulRecNo )
{
   if( SELF_GOCOLD( &pArea->area ) == ZH_FAILURE )
      return ZH_FAILURE;

   if( pArea->pSDD->GoTo( pArea, ulRecNo ) == ZH_FAILURE )
      return ZH_FAILURE;

   if( pArea->fPositioned )
   {
      pArea->ulRecNo   = ulRecNo;
      pArea->area.fBof = pArea->area.fEof = ZH_FALSE;
   }
   else
   {
      pArea->ulRecNo   = pArea->ulRecCount + 1;
      pArea->area.fBof = pArea->area.fEof = ZH_TRUE;
   }
   pArea->area.fFound = ZH_FALSE;

   return ZH_SUCCESS;
}


static ZH_ERRCODE sqlbaseGoToId( SQLBASEAREAP pArea, PZH_ITEM pItem )
{
   if( ZH_IS_NUMERIC( pItem ) )
      return SELF_GOTO( &pArea->area, zh_itemGetNL( pItem ) );
   else
   {
      PZH_ITEM pError = zh_errNew();
      zh_errPutGenCode( pError, EG_DATATYPE );
      zh_errPutDescription( pError, zh_langDGetErrorDesc( EG_DATATYPE ) );
      zh_errPutSubCode( pError, EDBF_DATATYPE );
      SELF_ERROR( &pArea->area, pError );
      zh_itemRelease( pError );
      return ZH_FAILURE;
   }
}


static ZH_ERRCODE sqlbaseGoTop( SQLBASEAREAP pArea )
{
   pArea->area.fTop    = ZH_TRUE;
   pArea->area.fBottom = ZH_FALSE;

   if( SELF_GOTO( &pArea->area, 1 ) == ZH_FAILURE )
      return ZH_FAILURE;

   return SELF_SKIPFILTER( &pArea->area, 1 );
}


static ZH_ERRCODE sqlbaseSkip( SQLBASEAREAP pArea, ZH_LONG lToSkip )
{
   ZH_ERRCODE errCode;

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( &pArea->area ) != ZH_SUCCESS )
         return ZH_FAILURE;
   }

   pArea->area.fTop = pArea->area.fBottom = ZH_FALSE;

   if( lToSkip == 0 || zh_setGetDeleted() ||
       pArea->area.dbfi.itmCobExpr || pArea->area.dbfi.fFilter )
      return SUPER_SKIP( &pArea->area, lToSkip );

   errCode = SELF_SKIPRAW( &pArea->area, lToSkip );

   /* Move first record and set Bof flag */
   if( errCode == ZH_SUCCESS && pArea->area.fBof && lToSkip < 0 )
   {
      errCode = SELF_GOTOP( &pArea->area );
      pArea->area.fBof = ZH_TRUE;
   }

   if( lToSkip < 0 )
      pArea->area.fEof = ZH_FALSE;
   else /* if( lToSkip > 0 ) */
      pArea->area.fBof = ZH_FALSE;

   return errCode;
}


static ZH_ERRCODE sqlbaseSkipRaw( SQLBASEAREAP pArea, ZH_LONG lToSkip )
{
   ZH_ERRCODE errCode;

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( &pArea->area ) != ZH_SUCCESS )
         return ZH_FAILURE;
   }

   if( lToSkip == 0 )
   {
      /* TODO: maybe gocold is enough here?! */
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
      errCode = SELF_GOTO( &pArea->area, pArea->ulRecNo + lToSkip );

   return errCode;
}


static ZH_ERRCODE sqlbaseAppend( SQLBASEAREAP pArea, ZH_BOOL bUnLockAll )
{
   ZH_SYMBOL_UNUSED( bUnLockAll );

   /* This GOTO is GOCOLD + GOEOF */
   if( SELF_GOTO( &pArea->area, 0 ) == ZH_FAILURE )
      return ZH_FAILURE;

   if( ! pArea->fRecordChanged && SELF_GOHOT( &pArea->area ) == ZH_FAILURE )
      return ZH_FAILURE;

   if( pArea->ulRecCount + 1 >= pArea->ulRecMax )
   {
      pArea->pRow      = ( void ** ) zh_xrealloc( pArea->pRow, ( pArea->ulRecMax + SQLDD_ROWSET_RESIZE ) * sizeof( void * ) );
      pArea->pRowFlags = ( ZH_BYTE * ) zh_xrealloc( pArea->pRowFlags, ( pArea->ulRecMax + SQLDD_ROWSET_RESIZE ) * sizeof( ZH_BYTE ) );
      pArea->ulRecMax += SQLDD_ROWSET_RESIZE;
   }

   pArea->fAppend = pArea->fPositioned = ZH_TRUE;
   pArea->ulRecCount++;
   pArea->ulRecNo   = pArea->ulRecCount;
   pArea->area.fBof = pArea->area.fEof = pArea->area.fFound = ZH_FALSE;
   return ZH_SUCCESS;
}


static ZH_ERRCODE sqlbaseDeleteRec( SQLBASEAREAP pArea )
{
   if( ! pArea->fPositioned )
      return ZH_SUCCESS;

   if( ! pArea->fRecordChanged && SELF_GOHOT( &pArea->area ) == ZH_FAILURE )
      return ZH_FAILURE;

   pArea->bRecordFlags |= SQLDD_FLAG_DELETED;
   return ZH_SUCCESS;
}


static ZH_ERRCODE sqlbaseDeleted( SQLBASEAREAP pArea, ZH_BOOL * pDeleted )
{
   *pDeleted = pArea->bRecordFlags & SQLDD_FLAG_DELETED;
   return ZH_SUCCESS;
}


static ZH_ERRCODE sqlbaseGetValue( SQLBASEAREAP pArea, ZH_USHORT uiIndex, PZH_ITEM pItem )
{
   if( uiIndex == 0 || uiIndex > pArea->area.uiFieldCount )
      return ZH_FAILURE;

   if( pArea->bRecordFlags & SQLDD_FLAG_CACHED )
   {
      zh_arrayGet( ( PZH_ITEM ) pArea->pRecord, uiIndex, pItem );
      return ZH_SUCCESS;
   }
   return pArea->pSDD->GetValue( pArea, uiIndex, pItem );
}


static ZH_ERRCODE sqlbaseGetVarLen( SQLBASEAREAP pArea, ZH_USHORT uiIndex, ZH_ULONG * pLength )
{
   /* TODO: should we use this code? */
#if 0
   if( pArea->area.lpFields[ uiIndex ].uiType == ZH_IT_MEMO )
      return pArea->pSDD->GetVarLen( pArea, uiIndex, pLength );
#endif

   *pLength = pArea->area.lpFields[ uiIndex - 1 ].uiLen;
   return ZH_SUCCESS;
}


static ZH_ERRCODE sqlbaseGoCold( SQLBASEAREAP pArea )
{
   if( pArea->fRecordChanged )
   {
      if( ! pArea->fAppend && pArea->pRowFlags[ pArea->ulRecNo ] & SQLDD_FLAG_CACHED )
         zh_itemRelease( ( PZH_ITEM ) ( pArea->pRow[ pArea->ulRecNo ] ) );
      pArea->pRow[ pArea->ulRecNo ]      = pArea->pRecord;
      pArea->pRowFlags[ pArea->ulRecNo ] = pArea->bRecordFlags;
      pArea->fRecordChanged = ZH_FALSE;
      pArea->fAppend        = ZH_FALSE;
   }
   return ZH_SUCCESS;
}


static ZH_ERRCODE sqlbaseGoHot( SQLBASEAREAP pArea )
{
   PZH_ITEM  pArray, pItem;
   ZH_USHORT us;

   pArray = zh_itemArrayNew( pArea->area.uiFieldCount );
   pItem = zh_itemNew( NULL );
   for( us = 1; us <= pArea->area.uiFieldCount; us++ )
   {
      if( SELF_GETVALUE( &pArea->area, us, pItem ) == ZH_SUCCESS )
         zh_arraySetForward( pArray, us, pItem );
   }
   zh_itemRelease( pItem );
   pArea->pRecord        = pArray;
   pArea->bRecordFlags  |= SQLDD_FLAG_CACHED;
   pArea->fRecordChanged = ZH_TRUE;
   return ZH_SUCCESS;
}


static ZH_ERRCODE sqlbasePutValue( SQLBASEAREAP pArea, ZH_USHORT uiIndex, PZH_ITEM pItem )
{
   LPFIELD    pField;
   ZH_ERRCODE errCode;

   if( uiIndex == 0 || uiIndex > pArea->area.uiFieldCount )
      return ZH_FAILURE;

   if( ! pArea->fPositioned )
      return ZH_SUCCESS;

   if( ! pArea->fRecordChanged && SELF_GOHOT( &pArea->area ) == ZH_FAILURE )
      return ZH_FAILURE;

   errCode = ZH_SUCCESS;
   pField  = pArea->area.lpFields + ( uiIndex - 1 );

   if( ( ( ZH_IS_MEMO( pItem ) || ZH_IS_STRING( pItem ) ) && ( pField->uiType == ZH_FT_STRING || pField->uiType == ZH_FT_MEMO ) ) ||
       ( ZH_IS_DATE( pItem ) && pField->uiType == ZH_FT_DATE ) ||
       ( ZH_IS_TIMESTAMP( pItem ) && pField->uiType == ZH_FT_TIMESTAMP ) ||
       ( ZH_IS_NUMBER( pItem ) && ( pField->uiType == ZH_FT_INTEGER || pField->uiType == ZH_FT_LONG ||
                                    pField->uiType == ZH_FT_FLOAT || pField->uiType == ZH_FT_DOUBLE ) ) ||
       ( ZH_IS_LOGICAL( pItem ) && pField->uiType == ZH_FT_LOGICAL ) ||
       ZH_IS_NIL( pItem ) )
   {
      zh_arraySet( ( PZH_ITEM ) pArea->pRecord, uiIndex, pItem );
   }
   else
   {
      PZH_ITEM pError;

      pError = zh_errNew();
      zh_errPutGenCode( pError, EG_DATATYPE );
      zh_errPutDescription( pError, zh_langDGetErrorDesc( EG_DATATYPE ) );
      zh_errPutOperation( pError, zh_dynsymName( ( PZH_DYNS ) pField->sym ) );
      zh_errPutSubCode( pError, errCode );
      zh_errPutFlags( pError, EF_CANDEFAULT );
      errCode = SELF_ERROR( &pArea->area, pError );
      zh_itemRelease( pError );
      return errCode == E_DEFAULT ? ZH_SUCCESS : ZH_FAILURE;
   }
   return ZH_SUCCESS;
}


static ZH_ERRCODE sqlbaseRecall( SQLBASEAREAP pArea )
{
   if( ! pArea->fPositioned )
      return ZH_SUCCESS;

   if( ! pArea->fRecordChanged && SELF_GOHOT( &pArea->area ) != ZH_SUCCESS )
      return ZH_FAILURE;

   pArea->bRecordFlags &= ~SQLDD_FLAG_DELETED;
   return ZH_SUCCESS;
}


static ZH_ERRCODE sqlbaseRecCount( SQLBASEAREAP pArea, ZH_ULONG * pRecCount )
{
   *pRecCount = pArea->ulRecCount;
   return ZH_SUCCESS;
}


static ZH_ERRCODE sqlbaseRecNo( SQLBASEAREAP pArea, ZH_ULONG * ulRecNo )
{
   *ulRecNo = pArea->ulRecNo;
   return ZH_SUCCESS;
}


static ZH_ERRCODE sqlbaseRecId( SQLBASEAREAP pArea, PZH_ITEM pRecNo )
{
   ZH_ERRCODE errCode;
   ZH_ULONG   ulRecNo;

   errCode = SELF_RECNO( &pArea->area, &ulRecNo );
   zh_itemPutNInt( pRecNo, ulRecNo );
   return errCode;
}


static ZH_ERRCODE sqlbaseClose( SQLBASEAREAP pArea )
{
   if( SELF_GOCOLD( &pArea->area ) == ZH_FAILURE )
      return ZH_FAILURE;

   if( SUPER_CLOSE( &pArea->area ) == ZH_FAILURE )
      return ZH_FAILURE;

   if( pArea->pSDD )
      pArea->pSDD->Close( pArea );

   if( pArea->pRow )
   {
      ZH_ULONG ulIndex;

      for( ulIndex = 0; ulIndex <= pArea->ulRecCount; ulIndex++ )
      {
         if( pArea->pRowFlags[ ulIndex ] & SQLDD_FLAG_CACHED )
            zh_itemRelease( ( PZH_ITEM ) pArea->pRow[ ulIndex ] );
      }
      zh_xfree( pArea->pRow );
      zh_xfree( pArea->pRowFlags );
      pArea->pRow      = NULL;
      pArea->pRowFlags = NULL;
   }

   if( pArea->szQuery )
   {
      zh_xfree( pArea->szQuery );
      pArea->szQuery = NULL;
   }
   if( pArea->pConnection )
   {
      /* It is possible to have areas without connection and SDD driver. Ex., arrayrdd. [Mindaugas] */
      pArea->pConnection->uiAreaCount--;
      pArea->pConnection = NULL;
   }
   return ZH_SUCCESS;
}


static ZH_ERRCODE sqlbaseCreate( SQLBASEAREAP pArea, LPDBOPENINFO pOpenInfo )
{
   PZH_ITEM  pItemEof, pItem;
   ZH_USHORT uiCount;
   ZH_BOOL   bError;

   pArea->ulConnection = pOpenInfo->ulConnection ? pOpenInfo->ulConnection : s_ulConnectionCurrent;

   if( pArea->ulConnection > s_ulConnectionCount ||
       ( pArea->ulConnection && ! s_pConnection[ pArea->ulConnection - 1 ] ) )
   {
      zh_errRT_SQLBASE( EG_OPEN, ESQLDD_NOTCONNECTED, "Not connected", NULL );
      return ZH_FAILURE;
   }

   if( pArea->ulConnection )
   {
      pArea->pConnection = s_pConnection[ pArea->ulConnection - 1 ];
      pArea->pConnection->uiAreaCount++;
      pArea->pSDD = pArea->pConnection->pSDD;
   }
   else
      pArea->pSDD = &s_sddNull;

   pItemEof = zh_itemArrayNew( pArea->area.uiFieldCount );

   bError = ZH_FALSE;
   for( uiCount = 0; uiCount < pArea->area.uiFieldCount; uiCount++ )
   {
      LPFIELD pField = pArea->area.lpFields + uiCount;

      switch( pField->uiType )
      {
         case ZH_FT_STRING:
         {
            char * pStr;

            pStr = ( char * ) zh_xgrab( pField->uiLen + 1 );
            memset( pStr, ' ', pField->uiLen );
            pStr[ pField->uiLen ] = '\0';

            pItem = zh_itemPutCL( NULL, pStr, pField->uiLen );
            zh_xfree( pStr );
            break;
         }

         case ZH_FT_MEMO:
            pItem = zh_itemPutC( NULL, NULL );
            break;

         case ZH_FT_INTEGER:
            if( pField->uiDec )
               pItem = zh_itemPutND( NULL, 0.0 );
            else
               pItem = zh_itemPutNI( NULL, 0 );
            break;

         case ZH_FT_LONG:
            if( pField->uiDec )
               pItem = zh_itemPutND( NULL, 0.0 );
            else
               pItem = zh_itemPutNL( NULL, 0 );
            break;

         case ZH_FT_FLOAT:
            pItem = zh_itemPutND( NULL, 0.0 );
            break;

         case ZH_FT_DOUBLE:
            pItem = zh_itemPutND( NULL, 0.0 );
            break;

         case ZH_FT_DATE:
            pItem = zh_itemPutDS( NULL, NULL );
            break;

         case ZH_FT_LOGICAL:
            pItem = zh_itemPutL( NULL, ZH_FALSE );
            break;

         default:
            pItem  = zh_itemNew( NULL );
            bError = ZH_TRUE;
            break;
      }

      zh_arraySetForward( pItemEof, uiCount + 1, pItem );
      zh_itemRelease( pItem );

      if( bError )
         break;
   }

   if( bError )
   {
      zh_itemClear( pItemEof );
      zh_itemRelease( pItemEof );
      zh_errRT_SQLBASE( EG_CORRUPTION, ESQLDD_INVALIDFIELD, "Invalid field type", NULL );
      SELF_CLOSE( &pArea->area );
      return ZH_FAILURE;
   }

   pArea->ulRecCount = 0;

   pArea->pRow      = ( void ** ) zh_xgrab( SQLDD_ROWSET_RESIZE * sizeof( void * ) );
   pArea->pRowFlags = ( ZH_BYTE * ) zh_xgrab( SQLDD_ROWSET_RESIZE * sizeof( ZH_BYTE ) );
   pArea->ulRecMax  = SQLDD_ROWSET_RESIZE;

   pArea->pRow[ 0 ]      = pItemEof;
   pArea->pRowFlags[ 0 ] = SQLDD_FLAG_CACHED;
   pArea->fFetched       = ZH_TRUE;

   if( SUPER_CREATE( &pArea->area, pOpenInfo ) != ZH_SUCCESS )
   {
      SELF_CLOSE( &pArea->area );
      return ZH_FAILURE;
   }

   return SELF_GOTOP( &pArea->area );
}


static ZH_ERRCODE sqlbaseInfo( SQLBASEAREAP pArea, ZH_USHORT uiIndex, PZH_ITEM pItem )
{
   switch( uiIndex )
   {
      case DBI_QUERY:
         zh_itemPutC( pItem, pArea->szQuery );
         break;

      default:
         return SUPER_INFO( &pArea->area, uiIndex, pItem );
   }

   return ZH_SUCCESS;
}


static ZH_ERRCODE sqlbaseOpen( SQLBASEAREAP pArea, LPDBOPENINFO pOpenInfo )
{
   ZH_ERRCODE errCode;

   pArea->ulConnection = pOpenInfo->ulConnection ? pOpenInfo->ulConnection : s_ulConnectionCurrent;

   if( pArea->ulConnection == 0 || pArea->ulConnection > s_ulConnectionCount ||
       ! s_pConnection[ pArea->ulConnection - 1 ] )
   {
      zh_errRT_SQLBASE( EG_OPEN, ESQLDD_NOTCONNECTED, "Not connected", NULL );
      return ZH_FAILURE;
   }

   if( pArea->area.uiFieldCount )
      /* This should not happen (in __dbTrans()), because RDD is registered with RDD_REGISTER_TYPE_FULL */
      return ZH_FAILURE;

   pArea->pConnection = s_pConnection[ pArea->ulConnection - 1 ];
   pArea->pConnection->uiAreaCount++;
   pArea->pSDD = pArea->pConnection->pSDD;

   /* filename is a query */
   pArea->szQuery = zh_strdup( pOpenInfo->abName );

   errCode = pArea->pSDD->Open( pArea );

   if( errCode == ZH_SUCCESS )
      errCode = SUPER_OPEN( &pArea->area, pOpenInfo );

   if( errCode != ZH_SUCCESS )
   {
      SELF_CLOSE( &pArea->area );
      return ZH_FAILURE;
   }
   return SELF_GOTOP( &pArea->area );
}


static ZH_ERRCODE sqlbaseStructSize( SQLBASEAREAP pArea, ZH_USHORT * uiSize )
{
   ZH_SYMBOL_UNUSED( pArea );

   *uiSize = sizeof( SQLBASEAREA );
   return ZH_SUCCESS;
}


#if 0
static ZH_ERRCODE sqlbaseChildEnd( SQLBASEAREAP pArea, LPDBRELINFO pRelInfo )
{
   ZH_ERRCODE errCode;

   if( pArea->lpdbPendingRel == pRelInfo )
      errCode = SELF_FORCEREL( &pArea->area );
   else
      errCode = ZH_SUCCESS;
   SUPER_CHILDEND( &pArea->area, pRelInfo );
   return errCode;
}


static ZH_ERRCODE sqlbaseChildStart( SQLBASEAREAP pArea, LPDBRELINFO pRelInfo )
{
   if( SELF_CHILDSYNC( &pArea->area, pRelInfo ) != ZH_SUCCESS )
      return ZH_FAILURE;
   return SUPER_CHILDSTART( &pArea->area, pRelInfo );
}


static ZH_ERRCODE sqlbaseChildSync( SQLBASEAREAP pArea, LPDBRELINFO pRelInfo )
{
   if( SELF_GOCOLD( &pArea->area ) != ZH_SUCCESS )
      return ZH_FAILURE;

   pArea->lpdbPendingRel = pRelInfo;

   if( pArea->lpdbRelations )
      return SELF_SYNCCHILDREN( &pArea->area );

   return ZH_SUCCESS;
}


static ZH_ERRCODE sqlbaseForceRel( SQLBASEAREAP pArea )
{
   if( pArea->lpdbPendingRel )
   {
      LPDBRELINFO lpdbPendingRel;

      lpdbPendingRel        = pArea->lpdbPendingRel;
      pArea->lpdbPendingRel = NULL;
      return SELF_RELEVAL( &pArea->area, lpdbPendingRel );
   }
   return ZH_SUCCESS;
}


static ZH_ERRCODE sqlbaseSetFilter( SQLBASEAREAP pArea, LPDBFILTERINFO pFilterInfo )
{
   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( &pArea->area ) != ZH_SUCCESS )
         return ZH_FAILURE;
   }
   return SUPER_SETFILTER( &pArea->area, pFilterInfo );
}
#endif


static ZH_ERRCODE sqlbaseInit( LPRDDNODE pRDD )
{
   ZH_SYMBOL_UNUSED( pRDD );

   s_pItemNewID = zh_itemNew( NULL );
   return ZH_SUCCESS;
}


static ZH_ERRCODE sqlbaseExit( LPRDDNODE pRDD )
{
   ZH_SYMBOL_UNUSED( pRDD );

   if( s_pConnection )
   {
      ZH_ULONG ul;

      /* Disconnect all connections */
      for( ul = 0; ul < s_ulConnectionCount; ul++ )
      {
         if( s_pConnection[ ul ] )
         {
            s_pConnection[ ul ]->pSDD->Disconnect( s_pConnection[ ul ] );
            zh_xfree( s_pConnection[ ul ] );
         }
      }
      zh_xfree( s_pConnection );
      s_pConnection         = NULL;
      s_ulConnectionCount   = 0;
      s_ulConnectionCurrent = 0;
      if( s_szError )
      {
         zh_xfree( s_szError );
         s_szError = NULL;
      }
      if( s_szQuery )
      {
         zh_xfree( s_szQuery );
         s_szQuery = NULL;
      }
      zh_itemRelease( s_pItemNewID );
      s_pItemNewID = NULL;
   }

   return ZH_SUCCESS;
}


static ZH_ERRCODE sqlbaseRddInfo( LPRDDNODE pRDD, ZH_USHORT uiIndex, ZH_ULONG ulConnect, PZH_ITEM pItem )
{
   ZH_ULONG ulConn;
   SQLDDCONNECTION * pConn;

   ZH_SYMBOL_UNUSED( pRDD );

   ulConn = ulConnect ? ulConnect : s_ulConnectionCurrent;
   if( ulConn > 0 && ulConn <= s_ulConnectionCount )
      pConn = s_pConnection[ ulConn - 1 ];
   else
      pConn = NULL;

   switch( uiIndex )
   {
      case RDDI_REMOTE:
         zh_itemPutL( pItem, ZH_TRUE );
         break;

      case RDDI_CONNECTION:
      {
         ZH_ULONG ulNewConnection = 0;

         if( zh_itemType( pItem ) & ZH_IT_NUMERIC )
            ulNewConnection = zh_itemGetNL( pItem );

         zh_itemPutNL( pItem, ulConnect ? ulConnect : s_ulConnectionCurrent );

         if( ulNewConnection )
            s_ulConnectionCurrent = ulNewConnection;
         break;
      }

      case RDDI_ISDBF:
         zh_itemPutL( pItem, ZH_FALSE );
         break;

      case RDDI_CANPUTREC:
         zh_itemPutL( pItem, ZH_TRUE );
         break;

      case RDDI_CONNECT:
      {
         PSDDNODE     pNode = NULL;
         ZH_ULONG     ul;
         const char * pStr;

         pStr = zh_arrayGetCPtr( pItem, 1 );
         if( pStr )
         {
            pNode = s_pSdd;
            while( pNode )
            {
               if( ! zh_stricmp( pNode->Name, pStr ) )
                  break;
               pNode = pNode->pNext;
            }
         }

         zh_rddsqlSetError( 0, NULL, NULL, NULL, 0 );
         pConn = ( SQLDDCONNECTION * ) zh_xgrabz( sizeof( SQLDDCONNECTION ) );
         if( pNode && pNode->Connect( pConn, pItem ) == ZH_SUCCESS )
         {
            pConn->pSDD = pNode;

            /* Find free connection handle */
            for( ul = 0; ul < s_ulConnectionCount; ul++ )
            {
               if( ! s_pConnection[ ul ] )
                  break;
            }
            if( ul >= s_ulConnectionCount )
            {
               /* Realloc connection table */
               if( s_pConnection )
                  s_pConnection = ( SQLDDCONNECTION ** ) zh_xrealloc( s_pConnection, sizeof( SQLDDCONNECTION * ) * ( s_ulConnectionCount + CONNECTION_LIST_EXPAND ) );
               else
                  s_pConnection = ( SQLDDCONNECTION ** ) zh_xgrab( sizeof( SQLDDCONNECTION * ) * CONNECTION_LIST_EXPAND );

               memset( s_pConnection + s_ulConnectionCount, 0, sizeof( SQLDDCONNECTION * ) * CONNECTION_LIST_EXPAND );
               ul = s_ulConnectionCount;
               s_ulConnectionCount += CONNECTION_LIST_EXPAND;
            }
            s_pConnection[ ul ] = pConn;
            ul++;
            s_ulConnectionCurrent = ul;
         }
         else
         {
            zh_xfree( pConn );
            ul = 0;
         }

         zh_itemPutNI( pItem, ul );
         break;
      }

      case RDDI_DISCONNECT:
         zh_rddsqlSetError( 0, NULL, NULL, NULL, 0 );
         if( pConn && ! pConn->uiAreaCount && pConn->pSDD->Disconnect( pConn ) == ZH_SUCCESS )
         {
            zh_xfree( pConn );
            s_pConnection[ ulConn - 1 ] = NULL;
            if( s_ulConnectionCurrent == ulConn )
               s_ulConnectionCurrent = 0;

            zh_itemPutL( pItem, ZH_TRUE );
            return ZH_SUCCESS;
         }
         zh_itemPutL( pItem, ZH_FALSE );
         return ZH_SUCCESS;

      case RDDI_EXECUTE:
         zh_rddsqlSetError( 0, NULL, NULL, NULL, 0 );
         if( pConn )
            zh_itemPutL( pItem, pConn->pSDD->Execute( pConn, pItem ) == ZH_SUCCESS );
         else
            zh_itemPutL( pItem, ZH_FALSE );

         return ZH_SUCCESS;

      case RDDI_ERROR:
         zh_itemPutC( pItem, s_szError );
         return ZH_SUCCESS;

      case RDDI_ERRORNO:
         zh_itemPutNI( pItem, s_errCode );
         return ZH_SUCCESS;

      case RDDI_QUERY:
         zh_itemPutC( pItem, s_szQuery );
         return ZH_SUCCESS;

      case RDDI_NEWID:
         zh_itemCopy( pItem, s_pItemNewID );
         return ZH_SUCCESS;

      case RDDI_AFFECTEDROWS:
         zh_itemPutNInt( pItem, s_ulAffectedRows );
         return ZH_SUCCESS;

#if 0
      default:
         return SUPER_RDDINFO( pRDD, uiIndex, ulConnect, pItem );
#endif

   }

   return ZH_SUCCESS;
}


/* --- */

static RDDFUNCS sqlbaseTable =
{
   ( DBENTRYP_BP ) NULL,             /* sqlbaseBof */
   ( DBENTRYP_BP ) NULL,             /* sqlbaseEof */
   ( DBENTRYP_BP ) NULL,             /* sqlbaseFound */
   ( DBENTRYP_V ) sqlbaseGoBottom,
   ( DBENTRYP_UL ) sqlbaseGoTo,
   ( DBENTRYP_I ) sqlbaseGoToId,
   ( DBENTRYP_V ) sqlbaseGoTop,
   ( DBENTRYP_BIB ) NULL,            /* sqlbaseSeek */
   ( DBENTRYP_L ) sqlbaseSkip,
   ( DBENTRYP_L ) NULL,              /* sqlbaseSkipFilter */
   ( DBENTRYP_L ) sqlbaseSkipRaw,
   ( DBENTRYP_VF ) NULL,             /* sqlbaseAddField */
   ( DBENTRYP_B ) sqlbaseAppend,
   ( DBENTRYP_I ) NULL,              /* sqlbaseCreateFields */
   ( DBENTRYP_V ) sqlbaseDeleteRec,
   ( DBENTRYP_BP ) sqlbaseDeleted,
   ( DBENTRYP_SP ) NULL,             /* sqlbaseFieldCount */
   ( DBENTRYP_VF ) NULL,             /* sqlbaseFieldDisplay */
   ( DBENTRYP_SSI ) NULL,            /* sqlbaseFieldInfo */
   ( DBENTRYP_SCP ) NULL,            /* sqlbaseFieldName */
   ( DBENTRYP_V ) NULL,              /* sqlbaseFlush */
   ( DBENTRYP_PP ) NULL,             /* sqlbaseGetRec */
   ( DBENTRYP_SI ) sqlbaseGetValue,
   ( DBENTRYP_SVL ) sqlbaseGetVarLen,
   ( DBENTRYP_V ) sqlbaseGoCold,
   ( DBENTRYP_V ) sqlbaseGoHot,
   ( DBENTRYP_P ) NULL,              /* sqlbasePutRec */
   ( DBENTRYP_SI ) sqlbasePutValue,
   ( DBENTRYP_V ) sqlbaseRecall,
   ( DBENTRYP_ULP ) sqlbaseRecCount,
   ( DBENTRYP_ISI ) NULL,            /* sqlbaseRecInfo */
   ( DBENTRYP_ULP ) sqlbaseRecNo,
   ( DBENTRYP_I ) sqlbaseRecId,
   ( DBENTRYP_S ) NULL,              /* sqlbaseSetFieldExtent */
   ( DBENTRYP_CP ) NULL,             /* sqlbaseAlias */
   ( DBENTRYP_V ) sqlbaseClose,
   ( DBENTRYP_VO ) sqlbaseCreate,
   ( DBENTRYP_SI ) sqlbaseInfo,
   ( DBENTRYP_V ) NULL,              /* sqlbaseNewArea */
   ( DBENTRYP_VO ) sqlbaseOpen,
   ( DBENTRYP_V ) NULL,              /* sqlbaseRelease */
   ( DBENTRYP_SP ) sqlbaseStructSize,
   ( DBENTRYP_CP ) NULL,             /* sqlbaseSysName */
   ( DBENTRYP_VEI ) NULL,            /* sqlbaseEval */
   ( DBENTRYP_V ) NULL,              /* sqlbasePack */
   ( DBENTRYP_LSP ) NULL,            /* sqlbasePackRec */
   ( DBENTRYP_VS ) NULL,             /* sqlbaseSort */
   ( DBENTRYP_VT ) NULL,             /* sqlbaseTrans */
   ( DBENTRYP_VT ) NULL,             /* sqlbaseTransRec */
   ( DBENTRYP_V ) NULL,              /* sqlbaseZap */
   ( DBENTRYP_VR ) NULL,             /* sqlbaseChildEnd */
   ( DBENTRYP_VR ) NULL,             /* sqlbaseChildStart */
   ( DBENTRYP_VR ) NULL,             /* sqlbaseChildSync */
   ( DBENTRYP_V ) NULL,              /* sqlbaseSyncChildren */
   ( DBENTRYP_V ) NULL,              /* sqlbaseClearRel */
   ( DBENTRYP_V ) NULL,              /* sqlbaseForceRel */
   ( DBENTRYP_SSP ) NULL,            /* sqlbaseRelArea */
   ( DBENTRYP_VR ) NULL,             /* sqlbaseRelEval */
   ( DBENTRYP_SI ) NULL,             /* sqlbaseRelText */
   ( DBENTRYP_VR ) NULL,             /* sqlbaseSetRel */
   ( DBENTRYP_VOI ) NULL,            /* sqlbaseOrderListAdd */
   ( DBENTRYP_V ) NULL,              /* sqlbaseOrderListClear */
   ( DBENTRYP_VOI ) NULL,            /* sqlbaseOrderListDelete */
   ( DBENTRYP_VOI ) NULL,            /* sqlbaseOrderListFocus */
   ( DBENTRYP_V ) NULL,              /* sqlbaseOrderListRebuild */
   ( DBENTRYP_VOO ) NULL,            /* sqlbaseOrderCondition */
   ( DBENTRYP_VOC ) NULL,            /* sqlbaseOrderCreate */
   ( DBENTRYP_VOI ) NULL,            /* sqlbaseOrderDestroy */
   ( DBENTRYP_SVOI ) NULL,           /* sqlbaseOrderInfo */
   ( DBENTRYP_V ) NULL,              /* sqlbaseClearFilter */
   ( DBENTRYP_V ) NULL,              /* sqlbaseClearLocate */
   ( DBENTRYP_V ) NULL,              /* sqlbaseClearScope */
   ( DBENTRYP_VPLP ) NULL,           /* sqlbaseCountScope */
   ( DBENTRYP_I ) NULL,              /* sqlbaseFilterText */
   ( DBENTRYP_SI ) NULL,             /* sqlbaseScopeInfo */
   ( DBENTRYP_VFI ) NULL,            /* sqlbaseSetFilter */
   ( DBENTRYP_VLO ) NULL,            /* sqlbaseSetLocate */
   ( DBENTRYP_VOS ) NULL,            /* sqlbaseSetScope */
   ( DBENTRYP_VPL ) NULL,            /* sqlbaseSkipScope */
   ( DBENTRYP_B ) NULL,              /* sqlbaseLocate */
   ( DBENTRYP_CC ) NULL,             /* sqlbaseCompile */
   ( DBENTRYP_I ) NULL,              /* sqlbaseError */
   ( DBENTRYP_I ) NULL,              /* sqlbaseEvalBlock */
   ( DBENTRYP_VSP ) NULL,            /* sqlbaseRawLock */
   ( DBENTRYP_VL ) NULL,             /* sqlbaseLock */
   ( DBENTRYP_I ) NULL,              /* sqlbaseUnLock */
   ( DBENTRYP_V ) NULL,              /* sqlbaseCloseMemFile */
   ( DBENTRYP_VO ) NULL,             /* sqlbaseCreateMemFile */
   ( DBENTRYP_SCCS ) NULL,           /* sqlbaseGetValueFile */
   ( DBENTRYP_VO ) NULL,             /* sqlbaseOpenMemFile */
   ( DBENTRYP_SCCS ) NULL,           /* sqlbasePutValueFile */
   ( DBENTRYP_V ) NULL,              /* sqlbaseReadDBHeader */
   ( DBENTRYP_V ) NULL,              /* sqlbaseWriteDBHeader */
   ( DBENTRYP_R ) sqlbaseInit,
   ( DBENTRYP_R ) sqlbaseExit,
   ( DBENTRYP_RVVL ) NULL,           /* sqlbaseDrop */
   ( DBENTRYP_RVVL ) NULL,           /* sqlbaseExists */
   ( DBENTRYP_RVVVL ) NULL,          /* sqlbaseRename */
   ( DBENTRYP_RSLV ) sqlbaseRddInfo,
   ( DBENTRYP_SVP ) NULL             /* sqlbaseWhoCares */
};


/* --- Module initialization code --- */

ZH_FUNC( SQLBASE )
{
}

ZH_FUNC_STATIC( SQLBASE_GETFUNCTABLE )
{
   RDDFUNCS *  pTable;
   ZH_USHORT * puiCount, uiRddId;

   puiCount = ( ZH_USHORT * ) zh_parptr( 1 );
   pTable   = ( RDDFUNCS * ) zh_parptr( 2 );
   uiRddId  = ( ZH_USHORT ) zh_parni( 4 );

   if( pTable )
   {
      ZH_ERRCODE errCode;

      if( puiCount )
         *puiCount = RDDFUNCSCOUNT;

      errCode = zh_rddInheritEx( pTable, &sqlbaseTable, &sqlbaseSuper, NULL, NULL );
      if( errCode == ZH_SUCCESS )
         s_rddidSQLBASE = uiRddId;

      zh_retni( errCode );
   }
   else
      zh_retni( ZH_FAILURE );
}

static void zh_sqlbaseInit( void * cargo )
{
   ZH_SYMBOL_UNUSED( cargo );

   if( zh_rddRegister( "SQLBASE", RDD_REGISTER_TYPE_FULL ) > 1 )
      zh_errInternal( ZH_EI_RDDINVALID, NULL, NULL, NULL );
}

ZH_INIT_SYMBOLS_BEGIN( sqlbase__InitSymbols )
{
   "SQLBASE", { ZH_FS_PUBLIC }, { ZH_FUNCNAME( SQLBASE ) }, NULL
},
{ "SQLBASE_GETFUNCTABLE", { ZH_FS_PUBLIC }, { ZH_FUNCNAME( SQLBASE_GETFUNCTABLE ) }, NULL }
ZH_INIT_SYMBOLS_END( sqlbase__InitSymbols )

ZH_CALL_ON_STARTUP_BEGIN( _zh_sqlbase_init_ )
zh_vmAtInit( zh_sqlbaseInit, NULL );
ZH_CALL_ON_STARTUP_END( _zh_sqlbase_init_ )

#if defined( ZH_PRAGMA_STARTUP )
   #pragma startup sqlbase__InitSymbols
   #pragma startup _zh_sqlbase_init_
#elif defined( ZH_DATASEG_STARTUP )
   #define ZH_DATASEG_BODY  ZH_DATASEG_FUNC( sqlbase__InitSymbols ) \
   ZH_DATASEG_FUNC( _zh_sqlbase_init_ )
   #include "zh_ini_seg.h"
#endif
