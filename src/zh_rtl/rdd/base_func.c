/*
 * Base RDD module
 *
 * Copyright 1999 Bruno Cantero <bruno@issnet.net>
 * Copyright 2004-2007 Przemyslaw Czerpak
 * Copyright 2002 Horacio Roldan <ziher_ar@yahoo.com.ar>
 *   (zh_rddIterateWorkAreas(), zh_rddGetTempAlias())
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
#include "zh_rdd_api.h"
#include "zh_error_api.h"
#include "zh_item_api.h"
#include "zh_vm.h"
#include "zh_set.h"

ZH_FUNC( ALIAS )
{
   AREAP pArea = ( AREAP ) zh_rddGetWorkAreaPointer( ( ZH_AREANO ) zh_parni( 1 ) );

   if( pArea )
   {
      char szAlias[ ZH_RDD_MAX_ALIAS_LEN + 1 ];

      if( SELF_ALIAS( pArea, szAlias ) == ZH_SUCCESS )
      {
         zh_retc( szAlias );
         return;
      }
   }
   zh_retc_null();
}

ZH_FUNC( BOF )
{
   ZH_BOOL bBof = ZH_TRUE;
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_BOF( pArea, &bBof );

   zh_retl( bBof );
}

/* dbAppend( [<lUnLockAll>=.T.] ) --> <lSuccess> */
ZH_FUNC( DBAPPEND )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      ZH_BOOL bUnLockAll = zh_parldef( 1, ZH_TRUE );
      ZH_ERRCODE errCode;

      zh_rddSetNetErr( ZH_FALSE );
      errCode = SELF_APPEND( pArea, bUnLockAll );
      zh_retl( errCode == ZH_SUCCESS );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( DBCLOSEALL )
{
   zh_rddCloseAll();
}

ZH_FUNC( DBCLOSEAREA )
{
   zh_rddReleaseCurrentArea();
}

ZH_FUNC( DBCOMMIT )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_FLUSH( pArea );
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( DBCOMMITALL )
{
   zh_rddFlushAll();
}

/*
 *    dbCreate( cFile, aStruct, cRDD, lKeepOpen, cAlias, cDelimArg, ;
 *              cCodePage, nConnection ) --> <lSuccess>
 */
ZH_FUNC( DBCREATE )
{
   const char * szFileName, * szAlias, * szDriver, * szCpId;
   ZH_USHORT uiSize, uiLen;
   PZH_ITEM pStruct, pDelim;
   ZH_BOOL fKeepOpen, fCurrArea;
   ZH_ULONG ulConnection;

   szFileName = zh_parc( 1 );
   pStruct = zh_param( 2, ZH_IT_ARRAY );
   szDriver = zh_parc( 3 );
   fKeepOpen = ZH_ISLOGICAL( 4 );
   fCurrArea = fKeepOpen && ! zh_parl( 4 );
   szAlias = zh_parc( 5 );
   pDelim = zh_param( 6, ZH_IT_ANY );
   szCpId = zh_parc( 7 );
   ulConnection = zh_parnl( 8 );

   if( ! pStruct ||
       ! szFileName )
   {
      zh_errRT_DBCMD( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, ZH_ERR_FUNCNAME );
      return;
   }
   uiLen = ( ZH_USHORT ) zh_arrayLen( pStruct );

   for( uiSize = 1; uiSize <= uiLen; ++uiSize )
   {
      PZH_ITEM pFieldDesc = zh_arrayGetItemPtr( pStruct, uiSize );

      /* Validate items types of fields */
      if( zh_arrayLen( pFieldDesc ) < 4 ||
          ! ( zh_arrayGetType( pFieldDesc, 1 ) & ZH_IT_STRING ) ||
          ! ( zh_arrayGetType( pFieldDesc, 2 ) & ZH_IT_STRING ) ||
          ! ( zh_arrayGetType( pFieldDesc, 3 ) & ZH_IT_NUMERIC ) ||
          ! ( zh_arrayGetType( pFieldDesc, 4 ) & ZH_IT_NUMERIC ) )
      {
         zh_errRT_DBCMD( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, ZH_ERR_FUNCNAME );
         return;
      }
   }

   zh_retl( zh_rddCreateTable( szFileName, szDriver,
                               fCurrArea ? ( ZH_AREANO ) zh_rddGetCurrentWorkAreaNumber() : 0,
                               szAlias, fKeepOpen,
                               szCpId, ulConnection,
                               pStruct, pDelim ) == ZH_SUCCESS );
}

/*
 * zh_dbCreateTemp( <cAlias>, <aStruct>, <cRDD>, <cCodePage>, <nConnection> ) --> <lSuccess>
 */
ZH_FUNC( ZH_DBCREATETEMP )
{
   const char * szAlias, * szDriver, * szCpId;
   ZH_USHORT uiSize, uiLen;
   PZH_ITEM pStruct;
   ZH_ULONG ulConnection;

   szAlias = zh_parc( 1 );
   pStruct = zh_param( 2, ZH_IT_ARRAY );
   szDriver = zh_parc( 3 );
   szCpId = zh_parc( 4 );
   ulConnection = zh_parnl( 5 );

   if( ! szAlias || ! pStruct )
   {
      zh_errRT_DBCMD( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, ZH_ERR_FUNCNAME );
      return;
   }
   uiLen = ( ZH_USHORT ) zh_arrayLen( pStruct );

   for( uiSize = 1; uiSize <= uiLen; ++uiSize )
   {
      PZH_ITEM pFieldDesc = zh_arrayGetItemPtr( pStruct, uiSize );

      /* Validate items types of fields */
      if( zh_arrayLen( pFieldDesc ) < 4 ||
          ! ( zh_arrayGetType( pFieldDesc, 1 ) & ZH_IT_STRING ) ||
          ! ( zh_arrayGetType( pFieldDesc, 2 ) & ZH_IT_STRING ) ||
          ! ( zh_arrayGetType( pFieldDesc, 3 ) & ZH_IT_NUMERIC ) ||
          ! ( zh_arrayGetType( pFieldDesc, 4 ) & ZH_IT_NUMERIC ) )
      {
         zh_errRT_DBCMD( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, ZH_ERR_FUNCNAME );
         return;
      }
   }

   zh_retl( zh_rddCreateTableTemp( szDriver, szAlias,
                                   szCpId, ulConnection,
                                   pStruct ) == ZH_SUCCESS );
}


ZH_FUNC( DBDELETE )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_DELETE( pArea );
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( DBRECALL )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_RECALL( pArea );
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( DBGOBOTTOM )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_GOBOTTOM( pArea );
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( DBGOTO )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      PZH_ITEM pItem = zh_param( 1, ZH_IT_ANY );
      if( ! pItem )
         zh_errRT_DBCMD( EG_ARG, EDBCMD_NOVAR, NULL, ZH_ERR_FUNCNAME );
      else
         SELF_GOTOID( pArea, pItem );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( DBGOTOP )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_GOTOP( pArea );
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( __DBPACK )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      /*
       * Additional feature: __dbPack( [<bBlock>, [<nEvery>] )
       * Code Block to execute for every record.
       */
      PZH_ITEM pBlock = zh_param( 1, ZH_IT_BLOCK );
      if( pBlock )
      {
         PZH_ITEM pEvery;
         zh_itemRelease( pArea->valResult );
         pArea->valResult = zh_itemArrayNew( 2 );
         zh_arraySet( pArea->valResult, 1, pBlock );
         pEvery = zh_param( 2, ZH_IT_NUMERIC );
         if( pEvery )
            zh_arraySet( pArea->valResult, 2, pEvery );
         else
            zh_arraySetNI( pArea->valResult, 2, 0 );
      }
      else
      {
         if( pArea->valResult )
            zh_itemClear( pArea->valResult );
         else
            pArea->valResult = zh_itemNew( NULL );
      }
      SELF_PACK( pArea );
      if( pBlock )
         zh_itemClear( pArea->valResult );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( DBRLOCK )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBLOCKINFO dbLockInfo;
      dbLockInfo.fResult = ZH_FALSE;
      dbLockInfo.itmRecID = zh_param( 1, ZH_IT_ANY );
      if( ! dbLockInfo.itmRecID || ZH_ISNIL( 1 ) )
         dbLockInfo.uiMethod = DBLM_EXCLUSIVE;
      else
         dbLockInfo.uiMethod = DBLM_MULTIPLE;
      SELF_LOCK( pArea, &dbLockInfo );
      zh_retl( dbLockInfo.fResult );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( DBRLOCKLIST )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      PZH_ITEM pList = zh_itemArrayNew( 0 );
      SELF_INFO( pArea, DBI_GETLOCKARRAY, pList );
      zh_itemReturnRelease( pList );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );

}

ZH_FUNC( DBRUNLOCK )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_UNLOCK( pArea, zh_param( 1, ZH_IT_ANY ) );
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( DBSEEK )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      if( ! ZH_ISNIL( 1 ) )
      {
         PZH_ITEM pKey = zh_param( 1, ZH_IT_ANY );
         ZH_BOOL bSoftSeek = ZH_ISLOGICAL( 2 ) ? ( ZH_BOOL ) zh_parl( 2 ) : zh_setGetSoftSeek();
         ZH_BOOL bFindLast = zh_parl( 3 ) /* ZH_EXTENSION */, fFound = ZH_FALSE;
         if( SELF_SEEK( pArea, bSoftSeek, pKey, bFindLast ) == ZH_SUCCESS )
         {
            if( SELF_FOUND( pArea, &fFound ) != ZH_SUCCESS )
               fFound = ZH_FALSE;
         }
         zh_retl( fFound );
      }
      else
         zh_errRT_DBCMD( EG_ARG, EDBCMD_SEEK_BADPARAMETER, NULL, ZH_ERR_FUNCNAME );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( DBSELECTAREA )
{
   const char * szAlias = zh_parc( 1 );

   if( szAlias )
   {
      zh_rddSelectWorkAreaAlias( szAlias );
   }
   else
   {
      int iNewArea = zh_parni( 1 );

      if( iNewArea < 1 || iNewArea >= ZH_RDD_MAX_AREA_NUM )
      {
         if( zh_rddSelectFirstAvailable() != ZH_SUCCESS )
            zh_rddSelectWorkAreaNumber( 0 );
      }
      else
      {
         zh_rddSelectWorkAreaNumber( iNewArea );
      }
   }
}

ZH_FUNC( DBSKIP )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_SKIP( pArea, zh_parnldef( 1, 1 ) );
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( DBSTRUCT )
{
   PZH_ITEM pStruct = zh_itemArrayNew( 0 );
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
      zh_tblStructure( pArea, pStruct, DBS_ALEN );
   zh_itemReturnRelease( pStruct );
}

ZH_FUNC( DBUNLOCK )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_UNLOCK( pArea, NULL );
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( DBUNLOCKALL )
{
   zh_rddUnLockAll();
}

/* dbUseArea( [<lNewArea>], [<cDriver>], <cName>, [<xcAlias>], ;
              [<lShared>], [<lReadonly>], [<cCodePage>], ;
              [<nConnection>] ) --> <lSuccess> */
ZH_FUNC( DBUSEAREA )
{
   zh_retl( zh_rddOpenTable( zh_parc( 3 ), zh_parc( 2 ),
         zh_parl( 1 ) ? 0 : ( ZH_AREANO ) zh_rddGetCurrentWorkAreaNumber(),
         zh_parc( 4 ), ZH_ISLOGICAL( 5 ) ? zh_parl( 5 ) : ! zh_setGetExclusive(),
         zh_parl( 6 ), zh_parc( 7 ), zh_parnl( 8 ), NULL, NULL ) == ZH_SUCCESS );
}

ZH_FUNC( __DBZAP )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_ZAP( pArea );
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( DELETED )
{
   ZH_BOOL bDeleted = ZH_FALSE;
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_DELETED( pArea, &bDeleted );
   zh_retl( bDeleted );
}

ZH_FUNC( EOF )
{
   ZH_BOOL bEof = ZH_TRUE;
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_EOF( pArea, &bEof );
   zh_retl( bEof );
}

ZH_FUNC( FCOUNT )
{
   ZH_USHORT uiFields = 0;
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_FIELDCOUNT( pArea, &uiFields );
   zh_retni( uiFields );
}

ZH_FUNC( FIELDGET )
{
   PZH_ITEM pItem = zh_itemNew( NULL );
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
   ZH_USHORT uiField = ( ZH_FIELDNO ) zh_parni( 1 );

   if( pArea && uiField )
   {
      SELF_GETVALUE( pArea, uiField, pItem );
   }

   zh_itemReturnRelease( pItem );
}

ZH_FUNC( FIELDNAME )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
   ZH_USHORT uiFields, uiIndex = ( ZH_FIELDNO ) zh_parni( 1 );

   if( pArea && uiIndex )
   {
      if( SELF_FIELDCOUNT( pArea, &uiFields ) == ZH_SUCCESS &&
          uiIndex <= uiFields )
      {
         char * szName = ( char * ) zh_xgrab( pArea->uiMaxFieldNameLength + 1 );
         szName[ 0 ] = '\0';
         SELF_FIELDNAME( pArea, uiIndex, szName );
         zh_retc_buffer( szName );
         return;
      }

   }
   zh_retc_null();
}

ZH_FUNC_TRANSLATE( FIELD, FIELDNAME )

ZH_FUNC( FIELDPOS )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea && zh_parclen( 1 ) > 0 )
      zh_retni( zh_rddFieldIndex( pArea, zh_parc( 1 ) ) );
   else
      zh_retni( 0 );
}

ZH_FUNC( FIELDPUT )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      ZH_USHORT uiIndex = ( ZH_FIELDNO ) zh_parni( 1 );

      if( uiIndex > 0 )
      {
         PZH_ITEM pItem = zh_param( 2, ZH_IT_ANY );
         if( pItem && ! ZH_IS_NIL( pItem ) )
         {
            if( SELF_PUTVALUE( pArea, uiIndex, pItem ) == ZH_SUCCESS )
               zh_itemReturn( pItem );
         }
      }
   }
}

ZH_FUNC( ZH_FIELDPUT )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      ZH_USHORT uiIndex = ( ZH_FIELDNO ) zh_parni( 1 );

      if( uiIndex > 0 )
      {
         PZH_ITEM pItem = zh_param( 2, ZH_IT_ANY );
         if( pItem )
         {
            if( SELF_PUTVALUE( pArea, uiIndex, pItem ) == ZH_SUCCESS )
               zh_itemReturn( pItem );
         }
      }
   }
}

ZH_FUNC( FLOCK )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBLOCKINFO dbLockInfo;
      dbLockInfo.fResult  = ZH_FALSE;
      dbLockInfo.itmRecID = NULL;
      dbLockInfo.uiMethod = DBLM_FILE;
      SELF_LOCK( pArea, &dbLockInfo );
      zh_retl( dbLockInfo.fResult );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( FOUND )
{
   ZH_BOOL bFound = ZH_FALSE;
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_FOUND( pArea, &bFound );
   zh_retl( bFound );
}

ZH_FUNC( LASTREC )
{
   ZH_ULONG ulRecCount = 0;
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_RECCOUNT( pArea, &ulRecCount );

   zh_retnint( ulRecCount );
}

ZH_FUNC( LOCK )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBLOCKINFO dbLockInfo;
      dbLockInfo.fResult  = ZH_FALSE;
      dbLockInfo.itmRecID = NULL;
      dbLockInfo.uiMethod = DBLM_EXCLUSIVE;
      SELF_LOCK( pArea, &dbLockInfo );
      zh_retl( dbLockInfo.fResult );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

/* Same as LastRec() */
ZH_FUNC_TRANSLATE( RECCOUNT, LASTREC )

ZH_FUNC( RECNO )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
   PZH_ITEM pRecNo = zh_itemPutNL( NULL, 0 );

   if( pArea )
   {
      SELF_RECID( pArea, pRecNo );
   }
   zh_itemReturnRelease( pRecNo );
}
