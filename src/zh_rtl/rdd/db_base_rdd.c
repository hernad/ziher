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
#include "zh_api_error.h"
#include "zh_item_api.h"
#include "zh_vm.h"
#include "zh_set.h"

/* The 5th parameter is Ziher extension */
ZH_FUNC( AFIELDS )
{
   ZH_USHORT uiFields, uiCount;
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
   PZH_ITEM pName = zh_param( 1, ZH_IT_ARRAY );
   PZH_ITEM pType = zh_param( 2, ZH_IT_ARRAY );
   PZH_ITEM pLen = zh_param( 3, ZH_IT_ARRAY );
   PZH_ITEM pDec = zh_param( 4, ZH_IT_ARRAY );

   PZH_ITEM pFlags = NULL;

   if( ! pArea || ( ! pName && ! pType && ! pLen && ! pDec && ! pFlags ) )
   {
      zh_retni( 0 );
      return;
   }

   if( SELF_FIELDCOUNT( pArea, &uiFields ) != ZH_SUCCESS )
      return;

   if( pName )
   {
      ZH_USHORT uiArrayLen = ( ZH_USHORT ) zh_arrayLen( pName );
      if( uiArrayLen < uiFields )
         uiFields = uiArrayLen;
   }
   if( pType )
   {
      ZH_USHORT uiArrayLen = ( ZH_USHORT ) zh_arrayLen( pType );
      if( uiArrayLen < uiFields )
         uiFields = uiArrayLen;
   }
   if( pLen )
   {
      ZH_USHORT uiArrayLen = ( ZH_USHORT ) zh_arrayLen( pLen );
      if( uiArrayLen < uiFields )
         uiFields = uiArrayLen;
   }
   if( pDec )
   {
      ZH_USHORT uiArrayLen = ( ZH_USHORT ) zh_arrayLen( pDec );
      if( uiArrayLen < uiFields )
         uiFields = uiArrayLen;
   }


   if( pName )
   {
      for( uiCount = 1; uiCount <= uiFields; ++uiCount )
      {
         if( SELF_FIELDINFO( pArea, uiCount, DBS_NAME, zh_arrayGetItemPtr( pName, uiCount ) ) != ZH_SUCCESS )
            return;
      }
   }
   if( pType )
   {
      for( uiCount = 1; uiCount <= uiFields; ++uiCount )
      {
         if( SELF_FIELDINFO( pArea, uiCount, DBS_TYPE, zh_arrayGetItemPtr( pType, uiCount ) ) != ZH_SUCCESS )
            return;
      }
   }
   if( pLen )
   {
      for( uiCount = 1; uiCount <= uiFields; ++uiCount )
      {
         if( SELF_FIELDINFO( pArea, uiCount, DBS_LEN, zh_arrayGetItemPtr( pLen, uiCount ) ) != ZH_SUCCESS )
            return;
      }
   }
   if( pDec )
   {
      for( uiCount = 1; uiCount <= uiFields; ++uiCount )
      {
         if( SELF_FIELDINFO( pArea, uiCount, DBS_DEC, zh_arrayGetItemPtr( pDec, uiCount ) ) != ZH_SUCCESS )
            return;
      }
   }


   zh_retni( uiFields );
}

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

ZH_FUNC( DBEVAL )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBEVALINFO pEvalInfo;
      memset( &pEvalInfo, 0, sizeof( pEvalInfo ) );
      pEvalInfo.itmBlock = zh_param( 1, ZH_IT_BLOCK );
      if( ! pEvalInfo.itmBlock )
      {
         zh_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, ZH_ERR_FUNCNAME );
         return;
      }

      pEvalInfo.dbsci.itmCobFor = zh_param( 2, ZH_IT_BLOCK );
      if( ! pEvalInfo.dbsci.itmCobFor && ! ZH_ISNIL( 2 ) )
      {
         zh_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, ZH_ERR_FUNCNAME );
         return;
      }

      pEvalInfo.dbsci.itmCobWhile = zh_param( 3, ZH_IT_BLOCK );
      if( ! pEvalInfo.dbsci.itmCobWhile && ! ZH_ISNIL( 3 ) )
      {
         zh_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, ZH_ERR_FUNCNAME );
         return;
      }

      pEvalInfo.dbsci.lNext = zh_param( 4, ZH_IT_NUMERIC );
      if( ! pEvalInfo.dbsci.lNext && ! ZH_ISNIL( 4 ) )
      {
         zh_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, ZH_ERR_FUNCNAME );
         return;
      }

      pEvalInfo.dbsci.itmRecID = zh_param( 5, ZH_IT_NUMERIC );
      if( ! pEvalInfo.dbsci.itmRecID && ! ZH_ISNIL( 5 ) )
      {
         zh_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, ZH_ERR_FUNCNAME );
         return;
      }

      pEvalInfo.dbsci.fRest = zh_param( 6, ZH_IT_LOGICAL );
      if( ! pEvalInfo.dbsci.fRest && ! ZH_ISNIL( 6 ) )
      {
         zh_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, ZH_ERR_FUNCNAME );
         return;
      }

      SELF_DBEVAL( pArea, &pEvalInfo );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( DBF )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

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
   fKeepOpen = ZH_ISLOG( 4 );
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

/* NOTE: The created table will be kept open if lOpenMode parameter
         is of logical type. If .T. it will be opened in a new workarea,
         if .F. it will be opened in the current one. */
/* NOTE: Has an identical parameter list with dbCreate() */

/* __dbOpenSDF( cFile, aStruct, cRDD, lKeepOpen, cAlias, cDelimArg, cCodePage, nConnection ) --> <lSuccess> */
ZH_FUNC( __DBOPENSDF )
{
   const char * szFileName, * szAlias, * szDriver, * szCpId;
   ZH_USHORT uiSize, uiLen;
   PZH_ITEM pStruct, pDelim;
   ZH_BOOL fKeepOpen, fCurrArea;
   ZH_ULONG ulConnection;
   ZH_ERRCODE errCode;


   szFileName = zh_parc( 1 );
   pStruct = zh_param( 2, ZH_IT_ARRAY );
   szDriver = zh_parc( 3 );
   fKeepOpen = ZH_ISLOG( 4 );
   fCurrArea = fKeepOpen && ! zh_parl( 4 );
   szAlias = zh_parc( 5 );
   pDelim = zh_param( 6, ZH_IT_ANY );
   szCpId = zh_parc( 7 );
   ulConnection = zh_parnl( 8 );

   if( ! pStruct ||
       zh_arrayLen( pStruct ) == 0 ||
       ! szFileName || ! szFileName[ 0 ] )
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

   errCode = zh_rddOpenTable( szFileName, szDriver,
                              fCurrArea ? ( ZH_AREANO ) zh_rddGetCurrentWorkAreaNumber() : 0,
                              szAlias, ZH_TRUE, ZH_TRUE,
                              szCpId, ulConnection,
                              pStruct, pDelim );

   if( ! fKeepOpen && errCode == ZH_SUCCESS )
      zh_rddReleaseCurrentArea();

   zh_retl( errCode == ZH_SUCCESS );
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

ZH_FUNC( __DBLOCATE )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBSCOPEINFO dbScopeInfo;

      dbScopeInfo.itmCobFor   = zh_param( 1, ZH_IT_BLOCK );
      dbScopeInfo.lpstrFor    = NULL;
      dbScopeInfo.itmCobWhile = zh_param( 2, ZH_IT_BLOCK );
      dbScopeInfo.lpstrWhile  = NULL;
      dbScopeInfo.lNext       = zh_param( 3, ZH_IT_NUMERIC );
      dbScopeInfo.itmRecID    = zh_param( 4, ZH_IT_NUMERIC );
      dbScopeInfo.fRest       = zh_param( 5, ZH_IT_LOGICAL );

      dbScopeInfo.fIgnoreFilter     = ZH_TRUE;
      dbScopeInfo.fIncludeDeleted   = ZH_TRUE;
      dbScopeInfo.fLast             = ZH_FALSE;
      dbScopeInfo.fIgnoreDuplicates = ZH_FALSE;
      dbScopeInfo.fBackward         = ZH_FALSE;

      if( SELF_SETLOCATE( pArea, &dbScopeInfo ) == ZH_SUCCESS )
         SELF_LOCATE( pArea, ZH_FALSE );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EG_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( __DBSETLOCATE )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      PZH_ITEM pLocate = zh_param( 1, ZH_IT_BLOCK );
      if( pLocate )
      {
         DBSCOPEINFO pScopeInfo;
         memset( &pScopeInfo, 0, sizeof( pScopeInfo ) );
         pScopeInfo.itmCobFor = pLocate;
         SELF_SETLOCATE( pArea, &pScopeInfo );
      }
   }
}

ZH_FUNC( __DBCONTINUE )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_LOCATE( pArea, ZH_TRUE );
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
         ZH_BOOL bSoftSeek = ZH_ISLOG( 2 ) ? ( ZH_BOOL ) zh_parl( 2 ) : zh_setGetSoftSeek();
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

ZH_FUNC( __DBSETFOUND )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      PZH_ITEM pFound = zh_param( 1, ZH_IT_LOGICAL );
      if( pFound )
         pArea->fFound = zh_itemGetL( pFound );
   }
}

ZH_FUNC( DBSETFILTER )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      PZH_ITEM pBlock, pText;
      DBFILTERINFO pFilterInfo;

      pBlock = zh_param( 1, ZH_IT_BLOCK );
      pText = zh_param( 2, ZH_IT_STRING );
      /* Cl*pper allows to set text filter without codeblock. In local
         RDDs it effectively does nothing and only dbFilter() returns it
         but RDDs with automatic filter optimization like CL53/DBFCDX /
         COMIX/ClipMore or RDDs working with remote data base servers
         may use only text version of filter and ignore or use with
         lower priority the codeblock so Ziher has to work like
         Cl*pper here. [druzus] */
      if( pBlock || zh_itemGetCLen( pText ) > 0 )
      {
         pFilterInfo.itmCobExpr = pBlock;
         if( pText )
            pFilterInfo.abFilterText = pText;
         else
            pFilterInfo.abFilterText = zh_itemPutC( NULL, NULL );
         pFilterInfo.fFilter = ZH_TRUE;
         pFilterInfo.lpvCargo = NULL;
         pFilterInfo.fOptimized = ZH_FALSE;
         SELF_SETFILTER( pArea, &pFilterInfo );
         if( ! pText )
            zh_itemRelease( pFilterInfo.abFilterText );
      }
      else
      {
         SELF_CLEARFILTER( pArea );
      }
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( DBCLEARFILTER )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_CLEARFILTER( pArea );
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( DBFILTER )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      PZH_ITEM pFilter = zh_itemPutC( NULL, NULL );
      SELF_FILTERTEXT( pArea, pFilter );
      zh_itemReturnRelease( pFilter );
   }
   else
      zh_retc_null();
}

/* Ziher extension to retrieve filter codeblock */
ZH_FUNC( ZH_DBGETFILTER )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
      zh_itemReturn( pArea->dbfi.itmCobExpr );
   else
      zh_ret();
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

ZH_FUNC( DBTABLEEXT )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
   PZH_ITEM pItem = zh_itemNew( NULL );
   ZH_ERRCODE errCode = ZH_FAILURE;

   if( ! pArea )
   {
      LPRDDNODE pRddNode;
      ZH_USHORT uiRddID;
      pRddNode = zh_rddFindNode( zh_rddDefaultDrv( NULL ), &uiRddID );
      if( pRddNode )
      {
         pArea = ( AREAP ) zh_rddNewAreaNode( pRddNode, uiRddID );
         if( pArea )
         {
            errCode = SELF_INFO( pArea, DBI_TABLEEXT, pItem );
            SELF_RELEASE( pArea );
         }
      }
   }
   else
      errCode = SELF_INFO( pArea, DBI_TABLEEXT, pItem );

   if( errCode != ZH_SUCCESS )
      zh_itemPutC( pItem, NULL );
   zh_itemReturnRelease( pItem );
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
         zh_parc( 4 ), ZH_ISLOG( 5 ) ? zh_parl( 5 ) : ! zh_setGetExclusive(),
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

ZH_FUNC( HEADER )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( ! pArea )
      zh_retni( 0 );
   else
   {
      PZH_ITEM pItem = zh_itemNew( NULL );
      SELF_INFO( pArea, DBI_GETHEADERSIZE, pItem );
      zh_itemReturnRelease( pItem );
   }
}

ZH_FUNC( INDEXORD )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pInfo;
      memset( &pInfo, 0, sizeof( pInfo ) );
      pInfo.itmResult = zh_itemPutNI( NULL, 0 );
      SELF_ORDINFO( pArea, DBOI_NUMBER, &pInfo );
      zh_retni( zh_itemGetNI( pInfo.itmResult ) );
      zh_itemRelease( pInfo.itmResult );
   }
   else
      zh_retni( 0 );
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

ZH_FUNC( LUPDATE )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      PZH_ITEM pItem = zh_itemNew( NULL );

      SELF_INFO( pArea, DBI_LASTUPDATE, pItem );
      zh_itemReturnRelease( pItem );
   }
   else
      zh_retds( NULL );
}

ZH_FUNC( NETERR )
{
   zh_retl( zh_rddGetNetErr() );

   if( ZH_ISLOG( 1 ) )
      zh_rddSetNetErr( zh_parl( 1 ) );
}

ZH_FUNC( ORDBAGEXT )
{
   DBORDERINFO pInfo;
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   memset( &pInfo, 0, sizeof( pInfo ) );
   pInfo.itmResult = zh_itemPutC( NULL, NULL );
   if( ! pArea )
   {
      LPRDDNODE pRddNode;
      ZH_USHORT uiRddID;
      pRddNode = zh_rddFindNode( zh_rddDefaultDrv( NULL ), &uiRddID );
      if( pRddNode )
      {
         pArea = ( AREAP ) zh_rddNewAreaNode( pRddNode, uiRddID );
         if( pArea )
         {
            SELF_ORDINFO( pArea, DBOI_BAGEXT, &pInfo );
            SELF_RELEASE( pArea );
         }
      }
   }
   else
   {
      SELF_ORDINFO( pArea, DBOI_BAGEXT, &pInfo );
   }
   zh_itemReturnRelease( pInfo.itmResult );
}

ZH_FUNC( ORDBAGNAME )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );

      pOrderInfo.itmOrder = zh_param( 1, ZH_IT_ANY );
      if( pOrderInfo.itmOrder && ! ZH_IS_STRING( pOrderInfo.itmOrder ) )
      {
         if( ZH_IS_NIL( pOrderInfo.itmOrder ) )
            pOrderInfo.itmOrder = NULL;
         else if( ZH_IS_NUMERIC( pOrderInfo.itmOrder ) )
         {
            if( zh_itemGetNI( pOrderInfo.itmOrder ) == 0 )
               pOrderInfo.itmOrder = NULL;
         }
         else
         {
            zh_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, ZH_ERR_FUNCNAME );
            return;
         }
      }
      pOrderInfo.itmResult = zh_itemPutC( NULL, NULL );
      SELF_ORDINFO( pArea, DBOI_BAGNAME, &pOrderInfo );
      zh_itemReturnRelease( pOrderInfo.itmResult );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( ORDCONDSET )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      LPDBORDERCONDINFO lpdbOrdCondInfo;
      PZH_ITEM pItem;

      lpdbOrdCondInfo = ( LPDBORDERCONDINFO ) zh_xgrab( sizeof( DBORDERCONDINFO ) );
      lpdbOrdCondInfo->abFor = zh_parclen( 1 ) > 0 ?
                               zh_strdup( zh_parc( 1 ) ) : NULL;
      pItem = zh_param( 2, ZH_IT_BLOCK );
      lpdbOrdCondInfo->itmCobFor = pItem ? zh_itemNew( pItem ) : NULL;

      lpdbOrdCondInfo->fAll = zh_parldef( 3, ZH_TRUE );

      lpdbOrdCondInfo->abWhile = zh_parclen( 17 ) > 0 ?
                                 zh_strdup( zh_parc( 17 ) ) : NULL;
      pItem = zh_param( 4, ZH_IT_BLOCK );
      lpdbOrdCondInfo->itmCobWhile = pItem ? zh_itemNew( pItem ) : NULL;

      pItem = zh_param( 5, ZH_IT_BLOCK );
      lpdbOrdCondInfo->itmCobEval = pItem ? zh_itemNew( pItem ) : NULL;

      lpdbOrdCondInfo->lStep         = zh_parnl( 6 );
      lpdbOrdCondInfo->itmStartRecID = ZH_ISNIL( 7 ) ? NULL : zh_itemNew( zh_param( 7, ZH_IT_ANY ) );
      lpdbOrdCondInfo->lNextCount    = zh_parnl( 8 );
      lpdbOrdCondInfo->itmRecID      = ZH_ISNIL( 9 ) ? NULL : zh_itemNew( zh_param( 9, ZH_IT_ANY ) );
      lpdbOrdCondInfo->fRest         = zh_parl( 10 );
      lpdbOrdCondInfo->fDescending   = zh_parl( 11 );
      lpdbOrdCondInfo->fCompound     = zh_parl( 12 );
      lpdbOrdCondInfo->fAdditive     = zh_parl( 13 );
      lpdbOrdCondInfo->fUseCurrent   = zh_parl( 14 );
      lpdbOrdCondInfo->fCustom       = zh_parl( 15 );
      lpdbOrdCondInfo->fNoOptimize   = zh_parl( 16 );
      /* 18th parameter in [x]Ziher is MEMORY flag added by Alexander for
         DBFNTX, so far it was served in hacked way inside SELF_ORDSETCOND()
         so it was working only if this method was called from ordCondSet()
         function. I also do not like the idea that it was called MEMORY.
         It should be RDD decision how such index will be served on low
         level and it should be IMHO called TEMPORARY - if RDD wants then
         it can make it fully in memory or in temporary file which will
         be removed on index close operation */
      lpdbOrdCondInfo->fTemporary    = zh_parl( 18 );
      /* 19th parameter USEFILTER parameter which means that RDD should respect SET FILTER and SET DELETED flag */
      lpdbOrdCondInfo->fUseFilter    = zh_parl( 19 );
      /* 20th parameter informs RDD that index is not shared between other clients */
      lpdbOrdCondInfo->fExclusive    = zh_parl( 20 );

      if( lpdbOrdCondInfo->itmCobWhile )
         lpdbOrdCondInfo->fRest = ZH_TRUE;
      if( lpdbOrdCondInfo->lNextCount || lpdbOrdCondInfo->itmRecID ||
               lpdbOrdCondInfo->fRest || lpdbOrdCondInfo->fUseCurrent ||
          lpdbOrdCondInfo->fUseFilter )
         lpdbOrdCondInfo->fAll = ZH_FALSE;

      lpdbOrdCondInfo->fActive = ! lpdbOrdCondInfo->fAll ||
               lpdbOrdCondInfo->abFor || lpdbOrdCondInfo->itmCobFor ||
               lpdbOrdCondInfo->abWhile || lpdbOrdCondInfo->itmCobWhile ||
               lpdbOrdCondInfo->fNoOptimize || lpdbOrdCondInfo->itmCobEval ||
               lpdbOrdCondInfo->fTemporary;

      lpdbOrdCondInfo->fScoped  = ! lpdbOrdCondInfo->fAll;
      lpdbOrdCondInfo->lpvCargo = NULL;

      zh_retl( SELF_ORDSETCOND( pArea, lpdbOrdCondInfo ) == ZH_SUCCESS );
   }
   else
      zh_retl( ZH_FALSE );
}

ZH_FUNC( ORDCREATE )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERCREATEINFO dbOrderInfo;
      DBCONSTRAINTINFO dbConstrInfo;

      dbOrderInfo.lpdbOrdCondInfo = pArea->lpdbOrdCondInfo;
      dbOrderInfo.abBagName = zh_parcx( 1 );
      dbOrderInfo.atomBagName = zh_parcx( 2 );
      dbOrderInfo.itmOrder = NULL;
      dbOrderInfo.fUnique = ZH_ISLOG( 5 ) ? ( ZH_BOOL ) zh_parl( 5 ) : zh_setGetUnique();
      dbOrderInfo.abExpr = zh_param( 3, ZH_IT_STRING );
      if( ( ( dbOrderInfo.abBagName == NULL || dbOrderInfo.abBagName[ 0 ] == 0 ) &&
            ( dbOrderInfo.atomBagName == NULL || dbOrderInfo.atomBagName[ 0 ] == 0 ) ) ||
          ! dbOrderInfo.abExpr )
      {
         zh_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, ZH_ERR_FUNCNAME );
         return;
      }
      dbOrderInfo.itmCobExpr = zh_param( 4, ZH_IT_BLOCK );

      dbConstrInfo.abConstrName = zh_parc( 6 );
      dbConstrInfo.abTargetName = zh_parc( 7 );
      dbConstrInfo.itmRelationKey = zh_param( 8, ZH_IT_ARRAY );
      if( dbConstrInfo.abConstrName && dbConstrInfo.abTargetName && dbConstrInfo.itmRelationKey )
      {
         dbConstrInfo.fEnabled = zh_parl( 9 );
         dbOrderInfo.lpdbConstraintInfo = &dbConstrInfo;
      }
      else
      {
         dbOrderInfo.lpdbConstraintInfo = NULL;
      }

      SELF_ORDCREATE( pArea, &dbOrderInfo );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( ORDBAGCLEAR )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
      pOrderInfo.atomBagName = zh_param( 1, ZH_IT_STRING | ZH_IT_NUMERIC );
      zh_retl( SELF_ORDLSTDELETE( pArea, &pOrderInfo ) == ZH_SUCCESS );
   }
   else
      zh_retl( ZH_FALSE );
}

ZH_FUNC( ORDDESTROY )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
      pOrderInfo.itmOrder = zh_param( 1, ZH_IT_STRING | ZH_IT_NUMERIC );
      pOrderInfo.atomBagName = zh_param( 2, ZH_IT_STRING );
      zh_retl( SELF_ORDDESTROY( pArea, &pOrderInfo ) == ZH_SUCCESS );
   }
   else
      zh_retl( ZH_FALSE );
}

ZH_FUNC( ORDFOR )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      pOrderInfo.itmOrder = zh_param( 1, ZH_IT_ANY );
      if( pOrderInfo.itmOrder && ! ZH_IS_STRING( pOrderInfo.itmOrder ) )
      {
         if( ZH_IS_NIL( pOrderInfo.itmOrder ) )
            pOrderInfo.itmOrder = NULL;
         else if( ZH_IS_NUMERIC( pOrderInfo.itmOrder ) )
         {
            if( zh_itemGetNI( pOrderInfo.itmOrder ) == 0 )
               pOrderInfo.itmOrder = NULL;
         }
         else
         {
            zh_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, ZH_ERR_FUNCNAME );
            return;
         }
      }
      pOrderInfo.atomBagName = zh_param( 2, ZH_IT_STRING );
      pOrderInfo.itmNewVal = zh_param( 3, ZH_IT_STRING );
      pOrderInfo.itmResult = zh_itemPutC( NULL, NULL );
      pOrderInfo.itmCobExpr = NULL;
      pOrderInfo.fAllTags = ZH_FALSE;
      SELF_ORDINFO( pArea, DBOI_CONDITION, &pOrderInfo );
      zh_itemReturnRelease( pOrderInfo.itmResult );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( ORDKEY )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
      pOrderInfo.itmOrder = zh_param( 1, ZH_IT_ANY );
      if( pOrderInfo.itmOrder && ! ZH_IS_STRING( pOrderInfo.itmOrder ) )
      {
         if( ZH_IS_NIL( pOrderInfo.itmOrder ) )
            pOrderInfo.itmOrder = NULL;
         else if( ZH_IS_NUMERIC( pOrderInfo.itmOrder ) )
         {
            if( zh_itemGetNI( pOrderInfo.itmOrder ) == 0 )
               pOrderInfo.itmOrder = NULL;
         }
         else
         {
            zh_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, ZH_ERR_FUNCNAME );
            return;
         }
      }
      pOrderInfo.atomBagName = zh_param( 2, ZH_IT_STRING );
      pOrderInfo.itmResult = zh_itemPutC( NULL, NULL );
      SELF_ORDINFO( pArea, DBOI_EXPRESSION, &pOrderInfo );
      zh_itemReturnRelease( pOrderInfo.itmResult );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( ORDLISTADD )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      ZH_ERRCODE errCode;

      zh_rddSetNetErr( ZH_FALSE );

      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
      pOrderInfo.atomBagName = zh_param( 1, ZH_IT_STRING );
      pOrderInfo.itmOrder    = zh_param( 2, ZH_IT_STRING );

      if( ! pOrderInfo.atomBagName )
      {
         if( ! ZH_ISNIL( 1 ) )
            zh_errRT_DBCMD( EG_ARG, EDBCMD_ORDLSTADD_BADPARAMETER, NULL, ZH_ERR_FUNCNAME );
         return;
      }

      pOrderInfo.itmResult = zh_itemNew( NULL );

      errCode = SELF_ORDLSTADD( pArea, &pOrderInfo );

      if( ! pOrderInfo.itmResult || ZH_IS_NIL( pOrderInfo.itmResult ) )
         zh_retl( errCode == ZH_SUCCESS );
      else
         zh_itemReturn( pOrderInfo.itmResult );

      zh_itemRelease( pOrderInfo.itmResult );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( ORDLISTCLEAR )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_ORDLSTCLEAR( pArea );
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( ORDLISTREBUILD )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_ORDLSTREBUILD( pArea );
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( ORDNAME )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
      pOrderInfo.itmOrder = zh_param( 1, ZH_IT_ANY );
      if( pOrderInfo.itmOrder )
      {
         if( ZH_IS_NIL( pOrderInfo.itmOrder ) )
            pOrderInfo.itmOrder = NULL;
         else if( ZH_IS_NUMERIC( pOrderInfo.itmOrder ) )
         {
            if( zh_itemGetNI( pOrderInfo.itmOrder ) == 0 )
               pOrderInfo.itmOrder = NULL;
         }
         else
         {
            zh_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, ZH_ERR_FUNCNAME );
            return;
         }
      }

      pOrderInfo.atomBagName = zh_param( 2, ZH_IT_STRING );
      pOrderInfo.itmResult   = zh_itemPutC( NULL, NULL );
      SELF_ORDINFO( pArea, DBOI_NAME, &pOrderInfo );
      zh_itemReturnRelease( pOrderInfo.itmResult );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( ORDNUMBER )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
      pOrderInfo.itmOrder = zh_param( 1, ZH_IT_STRING );
      pOrderInfo.atomBagName = zh_param( 2, ZH_IT_STRING );
      if( ! ( pOrderInfo.itmOrder || ZH_ISNIL( 1 ) ) ||
          ! ( pOrderInfo.atomBagName || ZH_ISNIL( 2 ) ) )
      {
         zh_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, ZH_ERR_FUNCNAME );
         return;
      }
      pOrderInfo.itmResult = zh_itemPutNI( NULL, 0 );
      SELF_ORDINFO( pArea, DBOI_NUMBER, &pOrderInfo );
      zh_itemReturnRelease( pOrderInfo.itmResult );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( ORDSETFOCUS )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pInfo;
      memset( &pInfo, 0, sizeof( pInfo ) );
      pInfo.itmOrder = zh_param( 1, ZH_IT_STRING | ZH_IT_NUMERIC );
      pInfo.atomBagName = zh_param( 2, ZH_IT_STRING );
      pInfo.itmResult = zh_itemPutC( NULL, NULL );
      SELF_ORDLSTFOCUS( pArea, &pInfo );
      zh_itemReturnRelease( pInfo.itmResult );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( RDDLIST )
{
   zh_itemReturnRelease( zh_rddList( ( ZH_USHORT ) zh_parni( 1 ) ) );
}

ZH_FUNC( RDDNAME )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      char szRddName[ ZH_RDD_MAX_DRIVERNAME_LEN + 1 ];

      SELF_SYSNAME( pArea, szRddName );
      zh_retc( szRddName );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( RDDREGISTER )
{
   ZH_USHORT uiLen = ( ZH_USHORT ) zh_parclen( 1 );

   if( uiLen > 0 )
   {
      char szDriver[ ZH_RDD_MAX_DRIVERNAME_LEN + 1 ];

      if( uiLen > ZH_RDD_MAX_DRIVERNAME_LEN )
         uiLen = ZH_RDD_MAX_DRIVERNAME_LEN;

      zh_strncpyUpper( szDriver, zh_parc( 1 ), uiLen );
      /*
       * zh_rddRegister returns:
       *
       * 0: Ok, RDD registered
       * 1: RDD already registerd
       * > 1: error
       */
      if( zh_rddRegister( szDriver, ( ZH_USHORT ) zh_parni( 2 ) ) > 1 )
      {
         zh_errInternal( ZH_EI_RDDINVALID, NULL, NULL, NULL );
      }
   }
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

ZH_FUNC( RECSIZE )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      PZH_ITEM pItem = zh_itemNew( NULL );
      SELF_INFO( pArea, DBI_GETRECSIZE, pItem );
      zh_itemReturnRelease( pItem );
   }
   else
      zh_retni( 0 );
}

ZH_FUNC( RLOCK )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBLOCKINFO dbLockInfo;
      dbLockInfo.fResult = ZH_FALSE;
      dbLockInfo.itmRecID = NULL;
      dbLockInfo.uiMethod = DBLM_EXCLUSIVE;
      SELF_LOCK( pArea, &dbLockInfo );
      zh_retl( dbLockInfo.fResult );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( SELECT )
{
   if( zh_pcount() == 0 )
   {
      zh_retni( zh_rddGetCurrentWorkAreaNumber() );
   }
   else
   {
      const char * szAlias = zh_parc( 1 );
      int iArea = 0;

      if( szAlias )
      {
         if( zh_rddVerifyAliasName( szAlias ) == ZH_SUCCESS )
            zh_rddGetAliasNumber( szAlias, &iArea );
      }
      zh_retni( iArea );
   }
}

ZH_FUNC( USED )
{
   zh_retl( zh_rddGetCurrentWorkAreaPointer() != NULL );
}

ZH_FUNC( RDDSETDEFAULT )
{
   zh_retc( zh_rddDefaultDrv( NULL ) );

   if( zh_parclen( 1 ) > 0 )
   {
      if( ! zh_rddDefaultDrv( zh_parc( 1 ) ) )
         zh_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, ZH_ERR_FUNCNAME );
   }
}

ZH_FUNC( DBSETDRIVER )
{
   zh_retc( zh_rddDefaultDrv( NULL ) );

   if( zh_parclen( 1 ) > 0 )
   {
      if( ! zh_rddDefaultDrv( zh_parc( 1 ) ) )
         zh_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, ZH_ERR_FUNCNAME );
   }
}

ZH_FUNC( ORDSCOPE )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pInfo;
      ZH_USHORT uiAction;
      int iScope = zh_parni( 1 );

      memset( &pInfo, 0, sizeof( pInfo ) );
      pInfo.itmResult = zh_itemNew( NULL );
      if( iScope == 2 )
      {
         if( zh_pcount() > 1 && ! ZH_ISNIL( 2 ) )
         {
            uiAction = DBOI_SCOPESET;
            pInfo.itmNewVal = zh_param( 2, ZH_IT_ANY);
         }
         else
            uiAction = DBOI_SCOPECLEAR;
      }
      else
      {
         uiAction = ( iScope == 0 ) ? DBOI_SCOPETOP : DBOI_SCOPEBOTTOM;
         if( zh_pcount() > 1 )
         {
            if( ZH_ISNIL( 2 ) )
               uiAction = ( iScope == 0 ) ? DBOI_SCOPETOPCLEAR : DBOI_SCOPEBOTTOMCLEAR;
            else
               pInfo.itmNewVal = zh_param( 2, ZH_IT_ANY );
         }
      }
      SELF_ORDINFO( pArea, uiAction, &pInfo );
      zh_itemReturnRelease( pInfo.itmResult );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( DBRELATION )  /* (<nRelation>) --> cLinkExp */
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      PZH_ITEM pRelExpr = zh_itemPutC( NULL, NULL );
      ZH_USHORT uiRelNo = ( ZH_USHORT ) zh_parni( 1 );
      SELF_RELTEXT( pArea, uiRelNo ? uiRelNo : 1, pRelExpr );
      zh_itemReturnRelease( pRelExpr );
   }
   else
      zh_retc_null();
}

ZH_FUNC( DBRSELECT )  /* (<nRelation>) --> nWorkArea */
{
   ZH_USHORT uiWorkArea = 0, uiRelation = ( ZH_USHORT ) zh_parni( 1 );
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
      /* undocumented CA-Cl*pper behavior:
       * When parameter is missing, wrong or 0 then 1 is used as
       * relation number [druzus]
       */
      SELF_RELAREA( pArea, uiRelation ? uiRelation : 1, &uiWorkArea );

   zh_retni( uiWorkArea );
}

ZH_FUNC( DBCLEARRELATION )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_CLEARREL( pArea );
}

ZH_FUNC( DBSETRELATION )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBRELINFO dbRelations;
      AREAP pChildArea;
      ZH_AREANO uiChildArea;

      if( zh_pcount() < 2 ||
          zh_param( 1, ZH_IT_NUMERIC | ZH_IT_STRING ) == NULL ||
          ! ( ZH_ISNIL( 4 ) || ZH_ISLOG( 4 ) ) )
      {
         zh_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, ZH_ERR_FUNCNAME );
         return;
      }

      if( ZH_IS_PARAM_NUM( 1 ) )
      {
         uiChildArea = ( ZH_AREANO ) zh_parni( 1 );
      }
      else
      {
         int iArea = zh_rddGetCurrentWorkAreaNumber();

         zh_rddSelectWorkAreaAlias( zh_parcx( 1 ) );
         if( zh_vmRequestQuery() )
            return;
         uiChildArea = ( ZH_AREANO ) zh_rddGetCurrentWorkAreaNumber();
         zh_rddSelectWorkAreaNumber( iArea );
      }

      pChildArea = uiChildArea ? ( AREAP ) zh_rddGetWorkAreaPointer( uiChildArea ) : NULL;

      if( ! pChildArea )
      {
         zh_errRT_BASE( EG_NOALIAS, EDBCMD_NOALIAS, NULL, NULL, 0 );
         return;
      }

      dbRelations.itmCobExpr = zh_itemNew( zh_param( 2, ZH_IT_BLOCK ) );
      dbRelations.abKey = zh_itemNew( zh_param( 3, ZH_IT_STRING ) );
      dbRelations.isScoped = zh_parl( 4 );
      dbRelations.isOptimized = ZH_FALSE;
      dbRelations.lpaChild = pChildArea;
      dbRelations.lpaParent = pArea;
      dbRelations.lpdbriNext = NULL;

      SELF_SETREL( pArea, &dbRelations );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

/* __dbArrange( nToArea, aStruct, bFor, bWhile, nNext, nRecord, lRest, aFields ) */
ZH_FUNC( __DBARRANGE )
{
   ZH_ERRCODE errCode = ZH_FAILURE;
   AREAP pSrcArea, pDstArea;

   pSrcArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
   pDstArea = ( AREAP ) zh_rddGetWorkAreaPointer( ( ZH_AREANO ) zh_parni( 1 ) );

   if( pSrcArea && pDstArea && pSrcArea != pDstArea )
   {
      DBSORTINFO dbSortInfo;
      /* structure with fields copied copied from source WorkArea */
      PZH_ITEM pStruct = zh_param( 2, ZH_IT_ARRAY );
      /* array with sorted fields in source WorkArea */
      PZH_ITEM pFields = zh_param( 8, ZH_IT_ARRAY );

      memset( &dbSortInfo, 0, sizeof( dbSortInfo ) );
      errCode = zh_dbTransStruct( pSrcArea, pDstArea, &dbSortInfo.dbtri,
                                  NULL, pStruct );
      if( errCode == ZH_SUCCESS )
      {
         PZH_ITEM pTransItm;

         dbSortInfo.dbtri.dbsci.itmCobFor   = zh_param( 3, ZH_IT_BLOCK );
         dbSortInfo.dbtri.dbsci.lpstrFor    = NULL;
         dbSortInfo.dbtri.dbsci.itmCobWhile = zh_param( 4, ZH_IT_BLOCK );
         dbSortInfo.dbtri.dbsci.lpstrWhile  = NULL;
         dbSortInfo.dbtri.dbsci.lNext       = zh_param( 5, ZH_IT_NUMERIC );
         dbSortInfo.dbtri.dbsci.itmRecID    = ZH_ISNIL( 6 ) ? NULL : zh_param( 6, ZH_IT_ANY );
         dbSortInfo.dbtri.dbsci.fRest       = zh_param( 7, ZH_IT_LOGICAL );

         dbSortInfo.dbtri.dbsci.fIgnoreFilter     =
         dbSortInfo.dbtri.dbsci.fLast             =
         dbSortInfo.dbtri.dbsci.fIgnoreDuplicates =
         dbSortInfo.dbtri.dbsci.fBackward         =
         dbSortInfo.dbtri.dbsci.fOptimized        = ZH_FALSE;
         dbSortInfo.dbtri.dbsci.fIncludeDeleted   = ZH_TRUE;

         /* do not transfer record deleted flag to destination area */
         dbSortInfo.dbtri.uiFlags |= DBTF_RECALL;

         dbSortInfo.uiItemCount = pFields ? ( ZH_USHORT ) zh_arrayLen( pFields ) : 0;
         if( dbSortInfo.uiItemCount > 0 )
         {
            ZH_USHORT uiCount, uiDest;
            char * szFieldLine;
            ZH_SIZE nSize = 0;

            dbSortInfo.lpdbsItem = ( LPDBSORTITEM ) zh_xgrab( dbSortInfo.uiItemCount * sizeof( DBSORTITEM ) );
            for( uiCount = 1; uiCount <= dbSortInfo.uiItemCount; ++uiCount )
            {
               ZH_SIZE nLine = zh_arrayGetCLen( pFields, uiCount );
               if( nLine > nSize )
                  nSize = nLine;
            }
            szFieldLine = ( char * ) zh_xgrab( nSize + 1 );
            for( uiDest = 0, uiCount = 1; uiCount <= dbSortInfo.uiItemCount; ++uiCount )
            {
               char * szPos;
               dbSortInfo.lpdbsItem[ uiDest ].uiFlags = 0;
               zh_strncpyUpper( szFieldLine, zh_arrayGetCPtr( pFields, uiCount ),
                                zh_arrayGetCLen( pFields, uiCount ) );
               szPos = strchr( szFieldLine, '/' );
               if( szPos )
               {
                  *szPos++ = 0;
                  /* It's not Cl*pper compatible, Cl*pper checks only
                     for /D flag and ignores any /A flags [druzus] */
                  if( strchr( szPos, 'D' ) > strchr( szPos, 'A' ) )
                     dbSortInfo.lpdbsItem[ uiDest ].uiFlags |= SF_DESCEND;
                  else
                     dbSortInfo.lpdbsItem[ uiDest ].uiFlags |= SF_ASCEND;
                  if( strchr( szPos, 'C' ) != NULL )
                     dbSortInfo.lpdbsItem[ uiDest ].uiFlags |= SF_CASE;
               }
               else
                  dbSortInfo.lpdbsItem[ uiDest ].uiFlags |= SF_ASCEND;

               /* Cl*pper sorts records using field values from source
                  area only, destination area may not contain sorted
                  fields at all [druzus] */
               dbSortInfo.lpdbsItem[ uiDest ].uiField = zh_rddFieldExpIndex( pSrcArea, szFieldLine );
               /* Field found */
               if( dbSortInfo.lpdbsItem[ uiDest ].uiField != 0 )
                  ++uiDest;
            }
            dbSortInfo.uiItemCount = uiDest;
            zh_xfree( szFieldLine );
         }

         pTransItm = zh_dbTransInfoPut( NULL, &dbSortInfo.dbtri );
         errCode = SELF_INFO( dbSortInfo.dbtri.lpaDest, DBI_TRANSREC, pTransItm );
         if( errCode == ZH_SUCCESS )
         {
            errCode = dbSortInfo.dbtri.uiItemCount == 0 ? ZH_FAILURE :
                      ( dbSortInfo.uiItemCount == 0 ?
                        SELF_TRANS( pSrcArea, &dbSortInfo.dbtri ) :
                        SELF_SORT( pSrcArea, &dbSortInfo ) );
            SELF_INFO( dbSortInfo.dbtri.lpaDest, DBI_TRANSREC, pTransItm );
            if( errCode == ZH_SUCCESS && ( dbSortInfo.dbtri.uiFlags & DBTF_CPYCTR ) )
               errCode = zh_dbTransCounters( &dbSortInfo.dbtri );
         }
         zh_itemRelease( pTransItm );
      }

      /* Free items */
      if( dbSortInfo.lpdbsItem )
         zh_xfree( dbSortInfo.lpdbsItem );
      if( dbSortInfo.dbtri.lpTransItems )
         zh_xfree( dbSortInfo.dbtri.lpTransItems );
   }

   zh_retl( errCode == ZH_SUCCESS );
}

/* __dbTrans( nDstArea, aFieldsStru, bFor, bWhile, nNext, nRecord, lRest ) --> <lSuccess> */
ZH_FUNC( __DBTRANS )
{
   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      ZH_AREANO uiSrcArea, uiDstArea;
      AREAP pSrcArea, pDstArea;

      uiSrcArea = ( ZH_AREANO ) zh_rddGetCurrentWorkAreaNumber();
      pSrcArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
      uiDstArea = ( ZH_AREANO ) zh_parni( 1 );
      zh_rddSelectWorkAreaNumber( uiDstArea );
      pDstArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

      if( pSrcArea && pDstArea )
      {
         DBTRANSINFO dbTransInfo;
         PZH_ITEM pFields = zh_param( 2, ZH_IT_ARRAY );
         ZH_ERRCODE errCode;

         memset( &dbTransInfo, 0, sizeof( dbTransInfo ) );
         errCode = zh_dbTransStruct( pSrcArea, pDstArea, &dbTransInfo,
                                     NULL, pFields );
         if( errCode == ZH_SUCCESS )
         {
            PZH_ITEM pTransItm;

            zh_rddSelectWorkAreaNumber( dbTransInfo.lpaSource->uiArea );

            dbTransInfo.dbsci.itmCobFor   = zh_param( 3, ZH_IT_BLOCK );
            dbTransInfo.dbsci.lpstrFor    = NULL;
            dbTransInfo.dbsci.itmCobWhile = zh_param( 4, ZH_IT_BLOCK );
            dbTransInfo.dbsci.lpstrWhile  = NULL;
            dbTransInfo.dbsci.lNext       = zh_param( 5, ZH_IT_NUMERIC );
            dbTransInfo.dbsci.itmRecID    = ZH_ISNIL( 6 ) ? NULL : zh_param( 6, ZH_IT_ANY );
            dbTransInfo.dbsci.fRest       = zh_param( 7, ZH_IT_LOGICAL );

            dbTransInfo.dbsci.fIgnoreFilter     =
            dbTransInfo.dbsci.fLast             =
            dbTransInfo.dbsci.fIgnoreDuplicates =
            dbTransInfo.dbsci.fBackward         =
            dbTransInfo.dbsci.fOptimized        = ZH_FALSE;
            dbTransInfo.dbsci.fIncludeDeleted   = ZH_TRUE;

            pTransItm = zh_dbTransInfoPut( NULL, &dbTransInfo );
            errCode = SELF_INFO( dbTransInfo.lpaDest, DBI_TRANSREC, pTransItm );
            if( errCode == ZH_SUCCESS )
            {
               errCode = dbTransInfo.uiItemCount == 0 ? ZH_FAILURE :
                         SELF_TRANS( dbTransInfo.lpaSource, &dbTransInfo );
               /* we always call DBI_TRANSREC second time after TRANS() method
                * even if TRANS() failed - it's for RDDs which may need to store
                * pointer to dbTransInfo in first call and then release it and/or
                * clean some structures allocated for transfer operation [druzus]
                */
               SELF_INFO( dbTransInfo.lpaDest, DBI_TRANSREC, pTransItm );
               if( errCode == ZH_SUCCESS && ( dbTransInfo.uiFlags & DBTF_CPYCTR ) )
                  errCode = zh_dbTransCounters( &dbTransInfo );
            }
            zh_itemRelease( pTransItm );
         }

         if( dbTransInfo.lpTransItems )
            zh_xfree( dbTransInfo.lpTransItems );

         zh_retl( errCode == ZH_SUCCESS );
      }
      else
         zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );

      zh_rddSelectWorkAreaNumber( uiSrcArea );
   }
   else
      zh_errRT_DBCMD( EG_ARG, EDBCMD_USE_BADPARAMETER, NULL, ZH_ERR_FUNCNAME );
}

/* __dbApp( <cNameName>, [<aFields>], ;
            [<bFor>], [<bWhile>], [<nNext>], [<nRecord>], [<lRest>], ;
            [<cRDD>], [<nConnection>], [<cCodePage>], ;
            [<xDelimiter>] ) --> <lSuccess> */
ZH_FUNC( __DBAPP )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
      zh_retl( ZH_SUCCESS == zh_rddTransRecords( pArea,
               zh_parc( 1 ),                     /* file name */
               zh_parc( 8 ),                     /* RDD */
               zh_parnl( 9 ),                    /* connection */
               zh_param( 2, ZH_IT_ARRAY ),       /* Fields */
               ZH_FALSE,                         /* Export? */
               zh_param( 3, ZH_IT_BLOCK ),       /* cobFor */
               NULL,                             /* lpStrFor */
               zh_param( 4, ZH_IT_BLOCK ),       /* cobWhile */
               NULL,                             /* lpStrWhile */
               zh_param( 5, ZH_IT_NUMERIC ),     /* Next */
               ZH_ISNIL( 6 ) ? NULL : zh_param( 6, ZH_IT_ANY ),   /* RecID */
               zh_param( 7, ZH_IT_LOGICAL ),     /* Rest */
               zh_parc( 10 ),                    /* Codepage */
               zh_param( 11, ZH_IT_ANY ) ) );    /* Delimiter */
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "APPEND FROM" );
}

/* __dbCoppy( <cNameName>, [<aFields>], ;
              [<bFor>], [<bWhile>], [<nNext>], [<nRecord>], [<lRest>], ;
              [<cRDD>], [<nConnection>], [<cCodePage>], ;
              [<xDelimiter>] ) --> <lSuccess> */
ZH_FUNC( __DBCOPY )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
      zh_retl( ZH_SUCCESS == zh_rddTransRecords( pArea,
               zh_parc( 1 ),                     /* file name */
               zh_parc( 8 ),                     /* RDD */
               zh_parnl( 9 ),                    /* connection */
               zh_param( 2, ZH_IT_ARRAY ),       /* Fields */
               ZH_TRUE,                          /* Export? */
               zh_param( 3, ZH_IT_BLOCK ),       /* cobFor */
               NULL,                             /* lpStrFor */
               zh_param( 4, ZH_IT_BLOCK ),       /* cobWhile */
               NULL,                             /* lpStrWhile */
               zh_param( 5, ZH_IT_NUMERIC ),     /* Next */
               ZH_ISNIL( 6 ) ? NULL : zh_param( 6, ZH_IT_ANY ),   /* RecID */
               zh_param( 7, ZH_IT_LOGICAL ),     /* Rest */
               zh_parc( 10 ),                    /* Codepage */
               zh_param( 11, ZH_IT_ANY ) ) );    /* Delimiter */
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "COPY TO" );
}

ZH_FUNC( ZH_RDDGETTEMPALIAS )
{
   char szAliasTmp[ ZH_RDD_MAX_ALIAS_LEN + 1 ];

   if( zh_rddGetTempAlias( szAliasTmp ) == ZH_SUCCESS )
      zh_retc( szAliasTmp );
}

ZH_FUNC( ZH_RDDINFO )
{
   LPRDDNODE  pRDDNode;
   ZH_USHORT  uiRddID;
   ZH_ULONG   ulConnection;
   PZH_ITEM   pIndex;
   const char * szDriver;

   szDriver = zh_parc( 3 );
   if( ! szDriver ) /* no VIA RDD parameter, use default */
      szDriver = zh_rddDefaultDrv( NULL );

   ulConnection = zh_parnl( 4 );

   pRDDNode = zh_rddFindNode( szDriver, &uiRddID );  /* find the RDDNODE */
   pIndex = zh_param( 1, ZH_IT_NUMERIC );

   if( pRDDNode && pIndex )
   {
      PZH_ITEM pInfo = zh_itemParam( 2 );
      SELF_RDDINFO( pRDDNode, ( ZH_USHORT ) zh_itemGetNI( pIndex ), ulConnection, pInfo );
      zh_itemReturnRelease( pInfo );
   }
   else
      zh_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( ZH_DBDROP )
{
   LPRDDNODE pRDDNode = NULL;
   ZH_ULONG ulConnection = zh_parnl( 4 );
   const char * szName = zh_parc( 1 );

   if( szName )
   {
      const char * szDriver;

      if( ! szName[ 0 ] )
         szName = zh_parc( 2 );
      szDriver = zh_rddFindDrv( zh_parc( 3 ), szName );
      if( szDriver )
         pRDDNode = zh_rddFindNode( szDriver, NULL );  /* find the RDDNODE */
   }

   if( pRDDNode )
      zh_retl( SELF_DROP( pRDDNode,
                          zh_param( 1, ZH_IT_STRING ),
                          zh_param( 2, ZH_IT_STRING ),
                          ulConnection ) == ZH_SUCCESS );
   else
      zh_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( ZH_DBEXISTS )
{
   LPRDDNODE pRDDNode = NULL;
   ZH_ULONG ulConnection = zh_parnl( 4 );
   const char * szName = zh_parc( 1 );

   if( szName )
   {
      const char * szDriver;

      if( ! szName[ 0 ] )
         szName = zh_parc( 2 );
      szDriver = zh_rddFindDrv( zh_parc( 3 ), szName );
      if( szDriver )
         pRDDNode = zh_rddFindNode( szDriver, NULL );  /* find the RDDNODE */
   }
   if( pRDDNode )
      zh_retl( SELF_EXISTS( pRDDNode,
                            zh_param( 1, ZH_IT_STRING ),
                            zh_param( 2, ZH_IT_STRING ),
                            ulConnection ) == ZH_SUCCESS );
   else
      zh_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( ZH_DBRENAME )
{
   LPRDDNODE pRDDNode = NULL;
   ZH_ULONG ulConnection = zh_parnl( 5 );
   const char * szName = zh_parc( 1 );
   PZH_ITEM pTable, pIndex, pNewName;

   if( szName )
   {
      const char * szDriver;

      if( ! szName[ 0 ] )
         szName = zh_parc( 2 );
      szDriver = zh_rddFindDrv( zh_parc( 4 ), szName );
      if( szDriver )
         pRDDNode = zh_rddFindNode( szDriver, NULL );  /* find the RDDNODE */
   }

   pTable = zh_param( 1, ZH_IT_STRING );
   pIndex = zh_param( 2, ZH_IT_STRING );
   pNewName = zh_param( 3, ZH_IT_STRING );
   if( pIndex && ! pNewName )
   {
      pNewName = pIndex;
      pIndex = NULL;
   }

   if( pRDDNode && pTable && pNewName )
      zh_retl( SELF_RENAME( pRDDNode, pTable, pIndex, pNewName,
                            ulConnection ) == ZH_SUCCESS );
   else
      zh_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( ZH_FIELDLEN )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      ZH_USHORT uiIndex;
      const char * szField = zh_parc( 1 );

      if( szField )
         uiIndex = zh_rddFieldIndex( pArea, szField );
      else
         uiIndex = ( ZH_FIELDNO ) zh_parni( 1 );

      if( uiIndex > 0 )
      {
         PZH_ITEM pItem = zh_itemNew( NULL );

         if( SELF_FIELDINFO( pArea, uiIndex, DBS_LEN, pItem ) == ZH_SUCCESS )
         {
            zh_itemReturnRelease( pItem );
            return;
         }
         zh_itemRelease( pItem );
      }
   }

   zh_retni( 0 );
}

ZH_FUNC( ZH_FIELDDEC )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      ZH_USHORT uiIndex;
      const char * szField = zh_parc( 1 );

      if( szField )
         uiIndex = zh_rddFieldIndex( pArea, szField );
      else
         uiIndex = ( ZH_FIELDNO ) zh_parni( 1 );

      if( uiIndex > 0 )
      {
         PZH_ITEM pItem = zh_itemNew( NULL );

         if( SELF_FIELDINFO( pArea, uiIndex, DBS_DEC, pItem ) == ZH_SUCCESS )
         {
            zh_itemReturnRelease( pItem );
            return;
         }
         zh_itemRelease( pItem );
      }
   }

   zh_retni( 0 );
}

ZH_FUNC( ZH_FIELDTYPE )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      ZH_USHORT uiIndex;
      const char * szField = zh_parc( 1 );

      if( szField )
         uiIndex = zh_rddFieldIndex( pArea, szField );
      else
         uiIndex = ( ZH_FIELDNO ) zh_parni( 1 );

      if( uiIndex > 0 )
      {
         PZH_ITEM pItem = zh_itemNew( NULL );

         if( SELF_FIELDINFO( pArea, uiIndex, DBS_TYPE, pItem ) == ZH_SUCCESS )
         {
            zh_itemReturnRelease( pItem );
            return;
         }
         zh_itemRelease( pItem );
      }
   }

   zh_retc_null();
}

ZH_FUNC( ZH_WAEVAL )
{
   PZH_ITEM pBlock = zh_param( 1, ZH_IT_BLOCK );

   if( pBlock )
      zh_rddEvalWA( pBlock );
   else
      zh_errRT_DBCMD( EG_ARG, EDBCMD_USE_BADPARAMETER, NULL, ZH_ERR_FUNCNAME );
}


ZH_FUNC( __DBSKIPPER )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      ZH_LONG lSkipped = 0;
      ZH_BOOL fBEof;
      ZH_ULONG ulRecords = 0;

      if( SELF_RECCOUNT( pArea, &ulRecords ) == ZH_SUCCESS && ulRecords > 0 )
      {
         ZH_LONG lRecs = 1;

         if( ZH_IS_PARAM_NUM( 1 ) )
            lRecs = zh_parnl( 1 );

         if( lRecs == 0 )
            SELF_SKIP( pArea, 0 );
         else if( lRecs > 0 )
         {
            {
               while( lSkipped < lRecs )
               {
                  if( SELF_SKIP( pArea, 1 ) != ZH_SUCCESS )
                     break;
                  if( SELF_EOF( pArea, &fBEof ) != ZH_SUCCESS )
                     break;
                  if( fBEof )
                  {
                     SELF_SKIP( pArea, -1 );
                     break;
                  }
                  lSkipped++;
               }
            }
         }
         else /* if( lRecs < 0 ) */
         {
            while( lSkipped > lRecs )
            {
               if( SELF_SKIP( pArea, -1 ) != ZH_SUCCESS )
                  break;
               if( SELF_BOF( pArea, &fBEof ) != ZH_SUCCESS )
                  break;
               if( fBEof )
                  break;
               lSkipped--;
            }
         }
      }
      zh_retnl( lSkipped );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

