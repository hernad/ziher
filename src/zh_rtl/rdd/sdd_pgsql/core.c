/*
 * Postgre SQL Database Driver
 *
 * Copyright 
 * 
 * - 2007 Mindaugas Kavaliauskas <dbtopas at dbtopas.lt>
 * - 2014-2019 Ernad Husremovic <hernad@bring.out.ba>
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

#include "zh_rdd_sql.h"

#include "zh_item_api.h"
#include "zh_vm.h"

#include "libpq-fe.h"

#define BOOLOID         16
#define BYTEAOID        17
#define CHAROID         18
#define NAMEOID         19
#define INT8OID         20
#define INT2OID         21
#define INT4OID         23
#define TEXTOID         25
#define OIDOID          26
#define CIDROID         650
#define FLOAT4OID       700
#define FLOAT8OID       701
#define CASHOID         790
#define MACADDROID      829
#define INETOID         869
#define BPCHAROID       1042
#define VARCHAROID      1043
#define DATEOID         1082
#define TIMEOID         1083
#define TIMESTAMPOID    1114
#define TIMESTAMPTZOID  1184
#define TIMETZOID       1266
#define BITOID          1560
#define VARBITOID       1562
#define NUMERICOID      1700
#define UUIDOID         2950

typedef struct
{
   PGconn * pConn;
   ZH_BOOL      fExistingConnection;
} SDDCONN;

typedef struct
{
   PGresult * pResult;
} SDDDATA;


static ZH_ERRCODE pgsqlConnect( SQLDDCONNECTION * pConnection, PZH_ITEM pItem );
static ZH_ERRCODE pgsqlDisconnect( SQLDDCONNECTION * pConnection );
static ZH_ERRCODE pgsqlExecute( SQLDDCONNECTION * pConnection, PZH_ITEM pItem );
static ZH_ERRCODE pgsqlOpen( SQLBASEAREAP pArea );
static ZH_ERRCODE pgsqlClose( SQLBASEAREAP pArea );
static ZH_ERRCODE pgsqlGoto( SQLBASEAREAP pArea, ZH_ULONG ulRecNo );
static ZH_ERRCODE pgsqlGetValue( SQLBASEAREAP pArea, ZH_USHORT uiIndex, PZH_ITEM pItem );

static SDDNODE s_pgsqldd = {
   NULL,
   "POSTGRESQL",
   ( SDDFUNC_CONNECT ) pgsqlConnect,
   ( SDDFUNC_DISCONNECT ) pgsqlDisconnect,
   ( SDDFUNC_EXECUTE ) pgsqlExecute,
   ( SDDFUNC_OPEN ) pgsqlOpen,
   ( SDDFUNC_CLOSE ) pgsqlClose,
   ( SDDFUNC_GOTO ) pgsqlGoto,
   ( SDDFUNC_GETVALUE ) pgsqlGetValue,
   ( SDDFUNC_GETVARLEN ) NULL
};


static void zh_pgsqldd_init( void * cargo )
{
   ZH_SYMBOL_UNUSED( cargo );
   int iRet = zh_sddRegister( &s_pgsqldd );
   if( ! iRet )
      zh_errInternal( ZH_EI_RDDINVALID, sprintf("ERR_PSQLRDD %d", iRet), NULL, NULL );
}

ZH_FUNC( ZH_SDDPG_REGISTER )
{
   zh_pgsqldd_init( NULL );
}

/* force SQLBASE linking */
ZH_FUNC_TRANSLATE( SDDPG, SQLBASE )

//ZH_INIT_SYMBOLS_BEGIN( sddpostgre__InitSymbols )
//{
//  "SDDPG", { ZH_FS_PUBLIC | ZH_FS_LOCAL }, { ZH_FUNCNAME( SDDPG ) }, NULL
//},
//ZH_INIT_SYMBOLS_END( sddpostgre__InitSymbols )

ZH_CALL_ON_STARTUP_BEGIN( _zh_sddpostgre_init_ )
printf("=================== SDD PSQL ===========================\n");
//getchar();

zh_vmAtInit( zh_pgsqldd_init, NULL );
ZH_CALL_ON_STARTUP_END( _zh_sddpostgre_init_ )

#if defined( ZH_DATASEG_STARTUP )
   #define ZH_DATASEG_BODY  \
   ZH_DATASEG_FUNC( sddpostgre__InitSymbols ) \
   ZH_DATASEG_FUNC( _zh_sddpostgre_init_ )
   #include "zh_ini_seg.h"
#endif


/* --- */
static ZH_USHORT zh_errRT_PostgreSQLDD( ZH_ERRCODE errGenCode, ZH_ERRCODE errSubCode, const char * szDescription, const char * szOperation, ZH_ERRCODE errOsCode )
{
   ZH_USHORT uiAction;
   PZH_ITEM  pError;

   pError   = zh_errRT_New( ES_ERROR, "SDDPG", errGenCode, errSubCode, szDescription, szOperation, errOsCode, EF_NONE );
   uiAction = zh_errLaunch( pError );
   zh_itemRelease( pError );
   return uiAction;
}


/* --- SDD METHODS --- */
static ZH_ERRCODE pgsqlConnect( SQLDDCONNECTION * pConnection, PZH_ITEM pItem )
{
   PGconn *       pConn;
   ConnStatusType status;
   const char *   pszHost;
   void **        hbConn;
   PZH_ITEM       pSecond;
   int fExistingConnection = 0;

   pSecond = zh_itemArrayGet( pItem, 2 );
   if( ZH_IS_POINTER( pSecond ) )
   {
      hbConn = ( void ** ) zh_itemGetPtr( pSecond );
      pConn  = ( PGconn * ) *hbConn;
      fExistingConnection = 1;
   }
   else
   {
      pszHost = zh_arrayGetCPtr( pItem, 2 );
      if( pszHost && ( strncmp( pszHost, "postgresql://", 13 ) == 0 || strchr( pszHost, '=' ) ) )
         pConn = PQconnectdb( pszHost );
      else
         pConn = PQsetdbLogin( pszHost, zh_arrayGetCPtr( pItem, 6 ), zh_arrayGetCPtr( pItem, 7 ), zh_arrayGetCPtr( pItem, 8 ), zh_arrayGetCPtr( pItem, 5 ), zh_arrayGetCPtr( pItem, 3 ), zh_arrayGetCPtr( pItem, 4 ) );
   }

   if( ! pConn )   /* Low memory, etc */
   {
      /* TODO: error */
      return ZH_FAILURE;
   }
   status = PQstatus( pConn );
   if( status != CONNECTION_OK )
   {
      /* TODO: error */
      PQfinish( pConn );
      return ZH_FAILURE;
   }
   pConnection->pSDDConn = zh_xgrab( sizeof( SDDCONN ) );
   ( ( SDDCONN * ) pConnection->pSDDConn )->pConn = pConn;
   ( ( SDDCONN * ) pConnection->pSDDConn )->fExistingConnection = fExistingConnection;
   return ZH_SUCCESS;
}


static ZH_ERRCODE pgsqlDisconnect( SQLDDCONNECTION * pConnection )
{
   if( !( ( SDDCONN * ) pConnection->pSDDConn )->fExistingConnection )
      PQfinish( ( ( SDDCONN * ) pConnection->pSDDConn )->pConn );
   zh_xfree( pConnection->pSDDConn );
   return ZH_SUCCESS;
}


static ZH_ERRCODE pgsqlExecute( SQLDDCONNECTION * pConnection, PZH_ITEM pItem )
{
   PGconn *       pConn = ( ( SDDCONN * ) pConnection->pSDDConn )->pConn;
   int            iTuples;
   PGresult *     pResult;
   ExecStatusType status;
   unsigned long  ulAffectedRows;

   pResult = PQexec( pConn, zh_itemGetCPtr( pItem ) );
   if( ! pResult )
   {
      zh_rddsqlSetError( 1, PQerrorMessage( pConn ), zh_itemGetCPtr( pItem ), NULL, 0 );
      return ZH_FAILURE;
   }

   status = PQresultStatus( pResult );
   if( status != PGRES_TUPLES_OK && status != PGRES_COMMAND_OK )
   {
      zh_rddsqlSetError( status, PQresultErrorMessage( pResult ), zh_itemGetCPtr( pItem ), NULL, 0 );
      return ZH_FAILURE;
   }

   iTuples = PQntuples( pResult );
   if( iTuples > 0 )
      ulAffectedRows = ( unsigned long ) iTuples;
   else
      ulAffectedRows = ( unsigned long ) atol( PQcmdTuples( pResult ) );

   zh_rddsqlSetError( 0, NULL, zh_itemGetCPtr( pItem ), NULL, ulAffectedRows );
   PQclear( pResult );
   return ZH_SUCCESS;
}


static ZH_ERRCODE pgsqlOpen( SQLBASEAREAP pArea )
{
   PGconn *       pConn = ( ( SDDCONN * ) pArea->pConnection->pSDDConn )->pConn;
   SDDDATA *      pSDDData;
   PGresult *     pResult;
   ExecStatusType status;
   PZH_ITEM       pItemEof, pItem;
   ZH_USHORT      uiFields, uiCount;
   ZH_BOOL        bError;

   pArea->pSDDData = memset( zh_xgrab( sizeof( SDDDATA ) ), 0, sizeof( SDDDATA ) );
   pSDDData        = ( SDDDATA * ) pArea->pSDDData;

   pResult = PQexec( pConn, pArea->szQuery );
   if( ! pResult )
   {
      zh_errRT_PostgreSQLDD( EG_OPEN, ESQLDD_LOWMEMORY, "Query failed", NULL, 0 );  /* Low memory, etc */
      return ZH_FAILURE;
   }

   status = PQresultStatus( pResult );
   if( status != PGRES_TUPLES_OK && status != PGRES_COMMAND_OK )
   {
      zh_errRT_PostgreSQLDD( EG_OPEN, ESQLDD_INVALIDQUERY, PQresultErrorMessage( pResult ), pArea->szQuery, ( ZH_ERRCODE ) status );
      PQclear( pResult );
      return ZH_FAILURE;
   }

   pSDDData->pResult = pResult;

   uiFields = ( ZH_USHORT ) PQnfields( pResult );
   SELF_SETFIELDEXTENT( &pArea->area, uiFields );

   pItemEof = zh_itemArrayNew( uiFields );
   pItem    = zh_itemNew( NULL );

   bError = ZH_FALSE;
   for( uiCount = 0; uiCount < uiFields; uiCount++ )
   {
      DBFIELDINFO dbFieldInfo;

      memset( &dbFieldInfo, 0, sizeof( dbFieldInfo ) );
      dbFieldInfo.atomName = PQfname( pResult, ( int ) uiCount );

      switch( PQftype( pResult, ( int ) uiCount ) )
      {
         case BPCHAROID:
         case VARCHAROID:
            dbFieldInfo.uiType = ZH_FT_STRING;
            dbFieldInfo.uiLen  = ( ZH_USHORT ) PQfmod( pResult, uiCount ) - 4;
            break;

         case TEXTOID:
            dbFieldInfo.uiType = ZH_FT_MEMO;
            dbFieldInfo.uiLen  = 10;
            break;

         case NUMERICOID:
            dbFieldInfo.uiType = ZH_FT_DOUBLE;
            dbFieldInfo.uiLen  = ( PQfmod( pResult, uiCount ) - 4 ) >> 16;
            dbFieldInfo.uiDec  = ( PQfmod( pResult, uiCount ) - 4 ) & 0xFFFF;
            break;

         case INT2OID:
            dbFieldInfo.uiType = ZH_FT_INTEGER;
            dbFieldInfo.uiLen  = 6;
            break;

         case UUIDOID:
            dbFieldInfo.uiType = ZH_FT_DOUBLE;
            dbFieldInfo.uiLen = 16;
            dbFieldInfo.uiDec = 0;
            break;

         case INT4OID:
            dbFieldInfo.uiType = ZH_FT_INTEGER;
            dbFieldInfo.uiLen  = 11;
            break;

         case INT8OID:
         case OIDOID:
            dbFieldInfo.uiType = ZH_FT_LONG;
            dbFieldInfo.uiLen  = 20;
            break;

         case FLOAT4OID:
         case FLOAT8OID:
         case CASHOID:  /* TODO: ??? */
            dbFieldInfo.uiType = ZH_FT_DOUBLE;
            dbFieldInfo.uiLen  = 16;
            dbFieldInfo.uiDec  = 2;   /* TODO: zh_set.SET_DECIMALS ??? */
            break;

         case BOOLOID:
            dbFieldInfo.uiType = ZH_FT_LOGICAL;
            dbFieldInfo.uiLen  = 1;
            break;

         case DATEOID:
            dbFieldInfo.uiType = ZH_FT_DATE;
            dbFieldInfo.uiLen  = 8;
            break;

         case INETOID:
            dbFieldInfo.uiType = ZH_FT_STRING;
            dbFieldInfo.uiLen  = 29;
            break;

         case CIDROID:
            dbFieldInfo.uiType = ZH_FT_STRING;
            dbFieldInfo.uiLen  = 32;
            break;

         case MACADDROID:
            dbFieldInfo.uiType = ZH_FT_STRING;
            dbFieldInfo.uiLen  = 17;
            break;

         case BITOID:
         case VARBITOID:
            dbFieldInfo.uiType = ZH_FT_STRING;
            dbFieldInfo.uiLen  = ( ZH_USHORT ) PQfsize( pResult, uiCount );
            break;

         case TIMEOID:
            dbFieldInfo.uiType = ZH_FT_STRING;
            dbFieldInfo.uiLen  = 12;
            break;

         case TIMESTAMPOID:
            dbFieldInfo.uiType = ZH_FT_STRING;
            dbFieldInfo.uiLen  = 23;
            break;

         case TIMETZOID:
            dbFieldInfo.uiType = ZH_FT_STRING;
            dbFieldInfo.uiLen  = 15;
            break;

         case TIMESTAMPTZOID:
            dbFieldInfo.uiType = ZH_FT_STRING;
            dbFieldInfo.uiLen  = 26;
            break;

         case NAMEOID:
            dbFieldInfo.uiType = ZH_FT_STRING;
            dbFieldInfo.uiLen  = 63;
            break;

         case BYTEAOID:
            dbFieldInfo.uiType = ZH_FT_STRING;
            break;

         default:
            bError = ZH_TRUE;
            break;
      }
#if 0
      ZH_TRACE( ZH_TR_ALWAYS, ( "field:%s type=%d size=%d format=%d mod=%d err=%d", dbFieldInfo.atomName, PQftype( pResult, ( int ) uiCount ), PQfsize( pResult, uiCount ), PQfformat( pResult, uiCount ), PQfmod( pResult, uiCount ), bError ) );
#endif

      if( ! bError )
      {
         switch( dbFieldInfo.uiType )
         {
            case ZH_FT_STRING:
            {
               char * pStr;

               pStr = ( char * ) zh_xgrab( dbFieldInfo.uiLen + 1 );
               memset( pStr, ' ', dbFieldInfo.uiLen );
               pStr[ dbFieldInfo.uiLen ] = '\0';

               zh_itemPutCL( pItem, pStr, dbFieldInfo.uiLen );
               zh_xfree( pStr );
               break;
            }
            case ZH_FT_MEMO:
               zh_itemPutC( pItem, NULL );
               zh_itemSetCMemo( pItem );
               break;

            case ZH_FT_INTEGER:
               zh_itemPutNI( pItem, 0 );
               break;

            case ZH_FT_LONG:
               zh_itemPutNL( pItem, 0 );
               break;

            case ZH_FT_DOUBLE:
               zh_itemPutND( pItem, 0.0 );
               break;

            case ZH_FT_LOGICAL:
               zh_itemPutL( pItem, ZH_FALSE );
               break;

            case ZH_FT_DATE:
               zh_itemPutDS( pItem, NULL );
               break;

            default:
               zh_itemClear( pItem );
               bError = ZH_TRUE;
               break;
         }

         zh_arraySetForward( pItemEof, uiCount + 1, pItem );

#if 0
         if( dbFieldInfo.uiType == ZH_IT_DOUBLE || dbFieldInfo.uiType == ZH_IT_INTEGER )
            dbFieldInfo.uiType = ZH_IT_LONG;
#endif

         if( ! bError )
            bError = ( SELF_ADDFIELD( &pArea->area, &dbFieldInfo ) == ZH_FAILURE );
      }

      if( bError )
         break;
   }

   zh_itemRelease( pItem );

   if( bError )
   {
      zh_itemClear( pItemEof );
      zh_itemRelease( pItemEof );
      zh_errRT_PostgreSQLDD( EG_CORRUPTION, ESQLDD_INVALIDFIELD, "Invalid field type", pArea->szQuery, 0 );
      return ZH_FAILURE;
   }

   pArea->ulRecCount = ( ZH_ULONG ) PQntuples( pResult );
   pArea->ulRecMax   = pArea->ulRecCount + 1;

   pArea->pRow      = ( void ** ) zh_xgrab( ( pArea->ulRecCount + 1 ) * sizeof( void * ) );
   pArea->pRowFlags = ( ZH_BYTE * ) zh_xgrabz( ( pArea->ulRecCount + 1 ) * sizeof( ZH_BYTE ) );

   pArea->pRow[ 0 ]      = pItemEof;
   pArea->pRowFlags[ 0 ] = SQLDD_FLAG_CACHED;
   pArea->fFetched       = ZH_TRUE;

   return ZH_SUCCESS;
}


static ZH_ERRCODE pgsqlClose( SQLBASEAREAP pArea )
{
   SDDDATA * pSDDData = ( SDDDATA * ) pArea->pSDDData;

   if( pSDDData )
   {
      if( pSDDData->pResult )
         PQclear( pSDDData->pResult );

      zh_xfree( pSDDData );
      pArea->pSDDData = NULL;
   }
   return ZH_SUCCESS;
}


static ZH_ERRCODE pgsqlGoto( SQLBASEAREAP pArea, ZH_ULONG ulRecNo )
{
   if( ulRecNo == 0 || ulRecNo > pArea->ulRecCount )
   {
      pArea->pRecord      = pArea->pRow[ 0 ];
      pArea->bRecordFlags = pArea->pRowFlags[ 0 ];
      pArea->fPositioned  = ZH_FALSE;
   }
   else
   {
      pArea->pRecord      = pArea->pRow[ ulRecNo ];
      pArea->bRecordFlags = pArea->pRowFlags[ ulRecNo ];
      pArea->fPositioned  = ZH_TRUE;
   }

   return ZH_SUCCESS;
}


static ZH_ERRCODE pgsqlGetValue( SQLBASEAREAP pArea, ZH_USHORT uiIndex, PZH_ITEM pItem )
{
   SDDDATA * pSDDData = ( SDDDATA * ) pArea->pSDDData;
   LPFIELD   pField;
   char *    pValue;
   ZH_BOOL   bError;
   ZH_SIZE   nLen;

   char * pszDst;
   ZH_SIZE nDst;
   PZH_CODEPAGE cdpIn;
   PZH_CODEPAGE cdpOut;

   ZH_BOOL fDbl;
   double dVal;
   ZH_MAXINT lVal;

   bError = ZH_FALSE;
   uiIndex--;
   pField = pArea->area.lpFields + uiIndex;

   if( PQgetisnull( pSDDData->pResult, pArea->ulRecNo - 1, uiIndex ) )
   {
      //zh_itemClear( pItem );
      /* TOFIX: it breaks defined field type */
      pValue = NULL;
      //return ZH_SUCCESS;
   }
   else {
       pValue = PQgetvalue( pSDDData->pResult, pArea->ulRecNo - 1, uiIndex );
       nLen  = ( ZH_SIZE ) PQgetlength( pSDDData->pResult, pArea->ulRecNo - 1, uiIndex );
   }

#if 0
   ZH_TRACE( ZH_TR_ALWAYS, ( "fieldget recno=%d index=%d value=%s len=%d", dbFieldInfo.atomName, PQftype( pResult, ( int ) uiCount ), pArea->ulRecNo, uiIndex, pValue, nLen ) );
#endif

   switch( pField->uiType )
   {
      case ZH_FT_STRING:
      case ZH_FT_MEMO:

         if ( !pValue ) { // empty value
            if ( pField->uiType == ZH_FT_MEMO )
                zh_itemPutC( pItem, NULL );
            else {
                char * pStr;
                pStr = ( char * ) zh_xgrab( pField->uiLen + 1 );
                memset( pStr, ' ', pField->uiLen  );
                pStr[ pField->uiLen  ] = '\0';
                zh_itemPutCL( pItem, pStr, pField->uiLen  );
                zh_xfree( pStr );
            }
         }
         else {
            cdpIn = zh_cdpFindExt( "UTF8" );
            cdpOut = zh_cdpFindExt( zh_cdpID() );
            nDst = zh_cdpTransLen( pValue, nLen, 0, cdpIn, cdpOut);
            pszDst = (char *) zh_xgrab( nDst + 1);
            zh_cdpTransTo( pValue, nLen + 1, pszDst, nDst + 1, cdpIn, cdpOut);
            zh_itemPutCL( pItem, pszDst, nDst );
            zh_xfree( pszDst );
         }

         if ( pField->uiType == ZH_FT_MEMO )
             zh_itemSetCMemo( pItem );

         break;

      case ZH_FT_INTEGER:
      case ZH_FT_LONG:
      case ZH_FT_DOUBLE:

          if ( !pValue ) // empty
               zh_itemPutNDLen( pItem, (double) 0.0,
                         ( int ) pField->uiLen - ( ( int ) pField->uiDec + 1 ),
                         ( int ) pField->uiDec );
           else {

                fDbl = zh_strToNum( (const char *) pValue, &lVal,  &dVal);
                zh_itemPutNDLen( pItem, fDbl ?  dVal : (double) lVal ,
                                            ( int ) pField->uiLen - ( ( int ) pField->uiDec + 1 ),
                                            ( int ) pField->uiDec );
            }
            break;

      case ZH_FT_LOGICAL:

        if ( !pValue ) // empty => .F.
            zh_itemPutL( pItem, ZH_FALSE );
        else
           zh_itemPutL( pItem,
             pValue[ 0 ] == 't' ||
             pValue[ 0 ] == 'T' ||
             pValue[ 0 ] == 'y' ||
             pValue[ 0 ] == 'Y' ||
             pValue[ 0 ] == '1' );

         break;

      case ZH_FT_DATE:
      {
         char szDate[ 9 ];

        if ( !pValue ) // empty
            zh_itemPutDS( pItem, NULL );
         else {
            szDate[ 0 ] = pValue[ 0 ];
            szDate[ 1 ] = pValue[ 1 ];
            szDate[ 2 ] = pValue[ 2 ];
            szDate[ 3 ] = pValue[ 3 ];
            szDate[ 4 ] = pValue[ 5 ];
            szDate[ 5 ] = pValue[ 6 ];
            szDate[ 6 ] = pValue[ 8 ];
            szDate[ 7 ] = pValue[ 9 ];
            szDate[ 8 ] = '\0';
            zh_itemPutDS( pItem, szDate );
         }
         break;
      }

      default:
         bError = ZH_TRUE;
         break;
   }

   if( bError )
   {
      PZH_ITEM pError = zh_errNew();
      zh_errPutGenCode( pError, EG_DATATYPE );
      zh_errPutDescription( pError, zh_langDGetErrorDesc( EG_DATATYPE ) );
      zh_errPutSubCode( pError, EDBF_DATATYPE );
      SELF_ERROR( &pArea->area, pError );
      zh_itemRelease( pError );
      return ZH_FAILURE;
   }
   return ZH_SUCCESS;
}
