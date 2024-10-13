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

   if( ZH_ISLOGICAL( 1 ) )
      zh_rddSetNetErr( zh_parl( 1 ) );
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


ZH_FUNC( DBSETDRIVER )
{
   zh_retc( zh_rddDefaultDrv( NULL ) );

   if( zh_parclen( 1 ) > 0 )
   {
      if( ! zh_rddDefaultDrv( zh_parc( 1 ) ) )
         zh_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, ZH_ERR_FUNCNAME );
   }
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
          ! ( ZH_ISNIL( 4 ) || ZH_ISLOGICAL( 4 ) ) )
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

