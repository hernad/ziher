/*
 * SQLite3 Database Driver
 *
 * Copyright 2010-2014 Viktor Szakats (vszakats.net/ziher)
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
#include "zh_string_api.h"
#include "zh_date.h"
#include "zh_set.h"
#include "zh_vm.h"
#include "zh_set.h"

#include <sqlite3.h>

#define S_ZH_ARRAYGETSTR( arr, n, phstr, plen )  zh_arrayGetStrUTF8( arr, n, phstr, plen )
#define S_ZH_ITEMCOPYSTR( itm, str, len )        zh_itemCopyStrUTF8( itm, str, len )
#define S_ZH_ITEMGETSTR( itm, phstr, plen )      zh_itemGetStrUTF8( itm, phstr, plen )
#define S_ZH_ITEMPUTSTR( itm, str )              zh_itemPutStrUTF8( itm, str )
#define S_ZH_ITEMPUTSTRLEN( itm, str, len )      zh_itemPutStrLenUTF8( itm, str, len )

typedef struct
{
   sqlite3 * pDb;
} SDDCONN;

typedef struct
{
   sqlite3_stmt * pStmt;
} SDDDATA;

static ZH_ERRCODE sqlite3Connect( SQLDDCONNECTION * pConnection, PZH_ITEM pItem );
static ZH_ERRCODE sqlite3Disconnect( SQLDDCONNECTION * pConnection );
static ZH_ERRCODE sqlite3Execute( SQLDDCONNECTION * pConnection, PZH_ITEM pItem );
static ZH_ERRCODE sqlite3Open( SQLBASEAREAP pArea );
static ZH_ERRCODE sqlite3Close( SQLBASEAREAP pArea );
static ZH_ERRCODE sqlite3GoTo( SQLBASEAREAP pArea, ZH_ULONG ulRecNo );

static SDDNODE s_sqlt3dd =
{
   NULL,
   "SQLITE3",
   ( SDDFUNC_CONNECT ) sqlite3Connect,
   ( SDDFUNC_DISCONNECT ) sqlite3Disconnect,
   ( SDDFUNC_EXECUTE ) sqlite3Execute,
   ( SDDFUNC_OPEN ) sqlite3Open,
   ( SDDFUNC_CLOSE ) sqlite3Close,
   ( SDDFUNC_GOTO ) sqlite3GoTo,
   ( SDDFUNC_GETVALUE ) NULL,
   ( SDDFUNC_GETVARLEN ) NULL
};

static void zh_sqlt3dd_init( void * cargo )
{
   ZH_SYMBOL_UNUSED( cargo );

   if( ! zh_sddRegister( &s_sqlt3dd ) )
      zh_errInternal( ZH_EI_RDDINVALID, "ERR_SQLITE3", NULL, NULL );
}

static void zh_sqlt3dd_exit( void * cargo )
{
   ZH_SYMBOL_UNUSED( cargo );
}

ZH_FUNC( ZH_SDDSQLITE3_REGISTER )
{
   zh_sqlt3dd_init( NULL );
}

/* force SQLBASE linking */
ZH_FUNC_TRANSLATE( SDDSQLITE3, SQLBASE )

ZH_INIT_SYMBOLS_BEGIN( sqlt3dd__InitSymbols )
{
   "SDDSQLITE3", { ZH_FS_PUBLIC }, { ZH_FUNCNAME( SDDSQLITE3 ) }, NULL
},
ZH_INIT_SYMBOLS_END( sqlt3dd__InitSymbols )

ZH_CALL_ON_STARTUP_BEGIN( _zh_sqlt3dd_init_ )
zh_vmAtInit( zh_sqlt3dd_init, NULL );
zh_vmAtExit( zh_sqlt3dd_exit, NULL );
ZH_CALL_ON_STARTUP_END( _zh_sqlt3dd_init_ )

#if defined( ZH_DATASEG_STARTUP )
   #define ZH_DATASEG_BODY \
   ZH_DATASEG_FUNC( sqlt3dd__InitSymbols ) \
   ZH_DATASEG_FUNC( _zh_sqlt3dd_init_ )
   #include "zh_ini_seg.h"
#endif

/* --- */
static ZH_USHORT zh_errRT_SQLT3DD( ZH_ERRCODE errGenCode, ZH_ERRCODE errSubCode, const char * szDescription, const char * szOperation, ZH_ERRCODE errOsCode )
{
   PZH_ITEM  pError;
   ZH_USHORT uiAction;

   pError   = zh_errRT_New( ES_ERROR, "SDDSQLITE3", errGenCode, errSubCode, szDescription, szOperation, errOsCode, EF_NONE );
   uiAction = zh_errLaunch( pError );
   zh_itemRelease( pError );

   return uiAction;
}

static char * sqlite3GetError( sqlite3 * pDb, ZH_ERRCODE * pErrCode )
{
   char * szRet;
   int iNativeErr;

   if( pDb )
   {
      PZH_ITEM pRet = S_ZH_ITEMPUTSTR( NULL, sqlite3_errmsg( pDb ) );
      szRet = zh_strdup( zh_itemGetCPtr( pRet ) );
      zh_itemRelease( pRet );

      iNativeErr = sqlite3_errcode( pDb );
   }
   else
   {
      szRet = zh_strdup( "Could not get the error message" );
      iNativeErr = 9999;
   }

   if( pErrCode )
      *pErrCode = ( ZH_ERRCODE ) iNativeErr;

   return szRet;
}


static ZH_USHORT sqlite3DeclType(sqlite3_stmt * st, ZH_USHORT uiIndex )
{
   const char * szDeclType;

   szDeclType = sqlite3_column_decltype( st, uiIndex );
   /* the order of comparisons below is important to replicate
    * type precedence used by SQLITE3
    */
   if( szDeclType != NULL )
   {
      ZH_SIZE nLen = strlen( szDeclType );

      if( zh_strAtI( "INT", 3, szDeclType, nLen ) != 0 )
         return ZH_FT_INTEGER;
      if( zh_strAtI( "CHAR", 4, szDeclType, nLen ) != 0 ||
          zh_strAtI( "TEXT", 4, szDeclType, nLen ) != 0 ||
          zh_strAtI( "CLOB", 4, szDeclType, nLen ) != 0 )
         return ZH_FT_STRING;
      if( zh_strAtI( "BLOB", 4, szDeclType, nLen ) != 0 )
         return ZH_FT_ANY;
      if( zh_strAtI( "REAL", 4, szDeclType, nLen ) != 0 ||
          zh_strAtI( "FLOA", 4, szDeclType, nLen ) != 0 ||
          zh_strAtI( "DOUB", 4, szDeclType, nLen ) != 0 )
         return ZH_FT_LONG;
   }

#ifdef ZH_SQLT3_MAP_UNDECLARED_TYPES_AS_ANY
   return ZH_FT_ANY;
#else
   switch( sqlite3_column_type( st, uiIndex ) )
   {
      case SQLITE_TEXT:
         return ZH_FT_STRING;

      case SQLITE_FLOAT:
         return ZH_FT_LONG;

      case SQLITE_INTEGER:
         return ZH_FT_INTEGER;

      case SQLITE_BLOB:
         return ZH_FT_BLOB;

      case SQLITE_NULL:
         return ZH_FT_ANY;
   }

   return ZH_FT_NONE;
#endif
}

/* --- SDD METHODS --- */
static ZH_ERRCODE sqlite3Connect( SQLDDCONNECTION * pConnection, PZH_ITEM pItem )
{
   sqlite3 * db;
   void *    hConn;

   if( sqlite3_open( S_ZH_ARRAYGETSTR( pItem, 2, &hConn, NULL ), &db ) == SQLITE_OK )
   {
      pConnection->pSDDConn = zh_xgrab( sizeof( SDDCONN ) );
      ( ( SDDCONN * ) pConnection->pSDDConn )->pDb = db;
   }
   else
      sqlite3_close( db );

   zh_strfree( hConn );

   return db ? ZH_SUCCESS : ZH_FAILURE;
}

static ZH_ERRCODE sqlite3Disconnect( SQLDDCONNECTION * pConnection )
{
   ZH_ERRCODE errCode;

   errCode = sqlite3_close( ( ( SDDCONN * ) pConnection->pSDDConn )->pDb ) ? ZH_SUCCESS : ZH_FAILURE;
   zh_xfree( pConnection->pSDDConn );
   return errCode;
}

static ZH_ERRCODE sqlite3Execute( SQLDDCONNECTION * pConnection, PZH_ITEM pItem )
{
   sqlite3 *  pDb = ( ( SDDCONN * ) pConnection->pSDDConn )->pDb;
   ZH_ERRCODE errCode;
   int        iRow, iCol;
   void *     hStatement;
   char **    pResult   = NULL;
   char *     pszErrMsg = NULL;

   if( sqlite3_get_table( pDb, S_ZH_ITEMGETSTR( pItem, &hStatement, NULL ), &pResult, &iRow, &iCol, &pszErrMsg ) != SQLITE_OK )
   {
      zh_strfree( hStatement );
      zh_xfree( sqlite3GetError( pDb, &errCode ) );
      zh_errRT_SQLT3DD( EG_OPEN, ESQLDD_STMTALLOC, pszErrMsg, zh_itemGetCPtr( pItem ), errCode );
      zh_xfree( pszErrMsg );
      return ZH_FAILURE;
   }
   else
      zh_strfree( hStatement );

   sqlite3_free_table( pResult );

   /* TODO: new id */
   zh_rddsqlSetError( 0, NULL, zh_itemGetCPtr( pItem ), NULL, ( unsigned long ) iRow );
   return ZH_SUCCESS;
}

static ZH_ERRCODE sqlite3Open( SQLBASEAREAP pArea )
{
   sqlite3 *      pDb = ( ( SDDCONN * ) pArea->pConnection->pSDDConn )->pDb;
   sqlite3_stmt * st  = NULL;
   SDDDATA *      pSDDData;
   const char *   pszQuery;
   ZH_SIZE        nQueryLen;
   void *         hQuery;
   ZH_USHORT      uiFields, uiIndex;
   PZH_ITEM       pItemEof, pItem, pName = NULL;
   ZH_ERRCODE     errCode;
   char *         szError;
   ZH_BOOL        bError;
   int            result;

   pArea->pSDDData = memset( zh_xgrab( sizeof( SDDDATA ) ), 0, sizeof( SDDDATA ) );
   pSDDData        = ( SDDDATA * ) pArea->pSDDData;

   pItem    = zh_itemPutC( NULL, pArea->szQuery );
   pszQuery = S_ZH_ITEMGETSTR( pItem, &hQuery, &nQueryLen );

#if SQLITE_VERSION_NUMBER >= 3020000
   result = sqlite3_prepare_v3( pDb, pszQuery, ( int ) nQueryLen, 0, &st, NULL );
#else
   result = sqlite3_prepare_v2( pDb, pszQuery, ( int ) nQueryLen, &st, NULL );
#endif

   if( result != SQLITE_OK )
   {
      zh_strfree( hQuery );
      zh_itemRelease( pItem );
      szError = sqlite3GetError( pDb, &errCode );
      zh_errRT_SQLT3DD( EG_OPEN, ESQLDD_INVALIDQUERY, szError, pArea->szQuery, errCode );
      sqlite3_finalize( st );
      zh_xfree( szError );
      return ZH_FAILURE;
   }
   else
   {
      zh_strfree( hQuery );
      zh_itemRelease( pItem );
   }

   if( sqlite3_step( st ) != SQLITE_ROW )
   {
      szError = sqlite3GetError( pDb, &errCode );
      zh_errRT_SQLT3DD( EG_OPEN, ESQLDD_INVALIDQUERY, szError, pArea->szQuery, errCode );
      sqlite3_finalize( st );
      zh_xfree( szError );
      return ZH_FAILURE;
   }

   uiFields = ( ZH_USHORT ) sqlite3_column_count( st );
   SELF_SETFIELDEXTENT( &pArea->area, uiFields );

   errCode = 0;
   bError  = ZH_FALSE;
   pItemEof = zh_itemArrayNew( uiFields );
   for( uiIndex = 0; uiIndex < uiFields; ++uiIndex )
   {
      DBFIELDINFO dbFieldInfo;

      memset( &dbFieldInfo, 0, sizeof( dbFieldInfo ) );
      pName = S_ZH_ITEMPUTSTR( pName, sqlite3_column_name( st, uiIndex ) );
      dbFieldInfo.atomName = zh_itemGetCPtr( pName );
      dbFieldInfo.uiType = sqlite3DeclType( st, uiIndex );
      pItem = zh_arrayGetItemPtr( pItemEof, uiIndex + 1 );

      /* There are no field length limits stored in the SQLite3 database,
         so we're resorting to setting some arbitrary default values to
         make apps relying on these (e.g. Browse()/GET) to behave somewhat
         better. For better results, update apps to untie UI metrics from
         any database field/value widths. [vszakats] */

      switch( dbFieldInfo.uiType )
      {
         case ZH_FT_STRING:
         {
            int iSize = sqlite3_column_bytes( st, uiIndex );
            char * pStr;

            dbFieldInfo.uiLen = ( ZH_USHORT ) ZH_MAX( iSize, 10 );
            pStr = ( char * ) zh_xgrab( ( ZH_SIZE ) dbFieldInfo.uiLen + 1 );
            memset( pStr, ' ', dbFieldInfo.uiLen );
            zh_itemPutCLPtr( pItem, pStr, dbFieldInfo.uiLen );
            break;
         }
         case ZH_FT_BLOB:
            dbFieldInfo.uiLen = 4;
            zh_itemPutC( pItem, NULL );
            break;

         case ZH_FT_INTEGER:
            dbFieldInfo.uiLen = 8;
            zh_itemPutNInt( pItem, 0 );
            break;

         case ZH_FT_LONG:
            dbFieldInfo.uiLen = 20;
            dbFieldInfo.uiDec = ( ZH_USHORT ) zh_setGetDecimals();
            zh_itemPutNDDec( pItem, 0.0, dbFieldInfo.uiDec );
            break;

         case ZH_FT_ANY:
            dbFieldInfo.uiLen = 6;
            break;

         default:
            bError = ZH_TRUE;
      }

      if( ! bError )
         bError = ( SELF_ADDFIELD( &pArea->area, &dbFieldInfo ) == ZH_FAILURE );

      if( bError )
         break;
   }
   zh_itemRelease( pName );

   if( bError )
   {
      zh_itemRelease( pItemEof );
      sqlite3_finalize( st );
      zh_errRT_SQLT3DD( EG_CORRUPTION, ESQLDD_INVALIDFIELD, "Invalid field type", pArea->szQuery, errCode );
      return ZH_FAILURE;
   }

   pArea->ulRecCount = 0;
   pArea->ulRecMax   = SQLDD_ROWSET_INIT;

   pArea->pRow = ( void ** ) zh_xgrab( SQLDD_ROWSET_INIT * sizeof( void * ) );
   pArea->pRowFlags = ( ZH_BYTE * ) zh_xgrab( SQLDD_ROWSET_INIT * sizeof( ZH_BYTE ) );

   pArea->pRow[ 0 ]      = pItemEof;
   pArea->pRowFlags[ 0 ] = SQLDD_FLAG_CACHED;

   pSDDData->pStmt = st;
   return ZH_SUCCESS;
}

static ZH_ERRCODE sqlite3Close( SQLBASEAREAP pArea )
{
   SDDDATA * pSDDData = ( SDDDATA * ) pArea->pSDDData;

   if( pSDDData )
   {
      if( pSDDData->pStmt )
         sqlite3_finalize( pSDDData->pStmt );

      zh_xfree( pSDDData );
      pArea->pSDDData = NULL;
   }
   return ZH_SUCCESS;
}

static ZH_ERRCODE sqlite3GoTo( SQLBASEAREAP pArea, ZH_ULONG ulRecNo )
{
   sqlite3_stmt * st = ( ( SDDDATA * ) pArea->pSDDData )->pStmt;

   while( ulRecNo > pArea->ulRecCount && ! pArea->fFetched )
   {
      PZH_ITEM  pArray;
      ZH_USHORT ui;

      pArray = zh_itemArrayNew( pArea->area.uiFieldCount );

      for( ui = 0; ui < pArea->area.uiFieldCount; ++ui )
      {
         PZH_ITEM pItem  = NULL;
         LPFIELD  pField = pArea->area.lpFields + ui;
         ZH_USHORT uiType = pField->uiType;

         if( uiType == ZH_FT_ANY )
         {
            switch( sqlite3_column_type( st, ui ) )
            {
               case SQLITE_TEXT:
                  uiType = ZH_FT_STRING;
                  break;

               case SQLITE_FLOAT:
               case SQLITE_INTEGER:
                  uiType = ZH_FT_LONG;
                  break;

               case SQLITE_BLOB:
                  uiType = ZH_FT_BLOB;
                  break;
            }
         }

         switch( uiType )
         {
            case ZH_FT_STRING:
               pItem = S_ZH_ITEMPUTSTR( NULL, ( const char * ) sqlite3_column_text( st, ui ) );
               break;

            case ZH_FT_INTEGER:
#if ZH_VMLONG_MAX > INT32_MAX && ! defined( ZH_LONG_LONG_OFF )
               pItem = zh_itemPutNInt( NULL, sqlite3_column_int64( st, ui ) );
               break;
#endif
            case ZH_FT_LONG:
               pItem = zh_itemPutNDDec( NULL, sqlite3_column_double( st, ui ), pField->uiDec );
               break;

            case ZH_FT_BLOB:
               pItem = zh_itemPutCL( NULL, ( const char * ) sqlite3_column_blob( st, ui ), sqlite3_column_bytes( st, ui ) );
               break;
         }

         if( pItem )
         {
            zh_arraySetForward( pArray, ui + 1, pItem );
            zh_itemRelease( pItem );
         }
      }
      if( pArea->ulRecCount + 1 >= pArea->ulRecMax )
      {
         pArea->pRow      = ( void ** ) zh_xrealloc( pArea->pRow, ( pArea->ulRecMax + SQLDD_ROWSET_RESIZE ) * sizeof( void * ) );
         pArea->pRowFlags = ( ZH_BYTE * ) zh_xrealloc( pArea->pRowFlags, ( pArea->ulRecMax + SQLDD_ROWSET_RESIZE ) * sizeof( ZH_BYTE ) );
         pArea->ulRecMax += SQLDD_ROWSET_RESIZE;
      }

      pArea->ulRecCount++;
      pArea->pRow[ pArea->ulRecCount ]      = pArray;
      pArea->pRowFlags[ pArea->ulRecCount ] = SQLDD_FLAG_CACHED;

      if( sqlite3_step( st ) != SQLITE_ROW )
      {
         pArea->fFetched = ZH_TRUE;
         break;
      }
   }

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
