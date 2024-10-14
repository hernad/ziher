/*
 * Base RDD module
 *
 * Copyright 1999 Bruno Cantero <bruno@issnet.net>
 * Copyright 2004-2007 Przemyslaw Czerpak
 * Copyright 2002 Horacio Roldan <ziher_ar@yahoo.com.ar> (ordKeyVal(), ordKeyAdd(), ordKeyDel())
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

ZH_FUNC( ORDKEYCOUNT )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
      pOrderInfo.itmOrder = zh_param( 1, ZH_IT_STRING | ZH_IT_NUMERIC );
      pOrderInfo.atomBagName = zh_param( 2, ZH_IT_STRING );
      /* Either or both may be NIL */

      pOrderInfo.itmResult = zh_itemPutNL( NULL, 0 );
      SELF_ORDINFO( pArea, DBOI_KEYCOUNT, &pOrderInfo );
      zh_itemReturnRelease( pOrderInfo.itmResult );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );

}

ZH_FUNC( ORDKEYNO )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
      pOrderInfo.itmOrder = zh_param( 1, ZH_IT_STRING | ZH_IT_NUMERIC );
      pOrderInfo.atomBagName = zh_param( 2, ZH_IT_STRING );
      /* Either or both may be NIL */
      pOrderInfo.itmNewVal = NULL;
      pOrderInfo.itmResult = zh_itemPutNL( NULL, 0 );
      SELF_ORDINFO( pArea, DBOI_POSITION, &pOrderInfo );
      zh_itemReturnRelease( pOrderInfo.itmResult );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( ORDKEYGOTO )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
      pOrderInfo.itmNewVal = zh_param( 1, ZH_IT_NUMERIC );
      pOrderInfo.itmResult = zh_itemPutL( NULL, ZH_FALSE );
      SELF_ORDINFO( pArea, DBOI_POSITION, &pOrderInfo );
      zh_itemReturnRelease( pOrderInfo.itmResult );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( ORDKEYRELPOS )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
      pOrderInfo.itmNewVal = zh_param( 1, ZH_IT_NUMERIC );
      pOrderInfo.itmResult = zh_itemPutNI( NULL, 0 );
      SELF_ORDINFO( pArea, DBOI_RELKEYPOS, &pOrderInfo );
      zh_itemReturnRelease( pOrderInfo.itmResult );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( ORDFINDREC )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
      pOrderInfo.itmNewVal = zh_param( 1, ZH_IT_NUMERIC );
      pOrderInfo.itmResult = zh_itemPutL( NULL, ZH_FALSE );
      SELF_ORDINFO( pArea, zh_parl( 2 ) ? DBOI_FINDRECCONT :
                                          DBOI_FINDREC, &pOrderInfo );
      zh_itemReturnRelease( pOrderInfo.itmResult );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( ORDSKIPRAW )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_SKIPRAW( pArea, zh_parnldef( 1, 1 ) );
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}


ZH_FUNC( ORDSKIPUNIQUE )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
      pOrderInfo.itmNewVal = zh_param( 1, ZH_IT_ANY );
      pOrderInfo.itmResult = zh_itemPutL( NULL, ZH_FALSE );
      SELF_ORDINFO( pArea, DBOI_SKIPUNIQUE, &pOrderInfo );
      zh_itemReturnRelease( pOrderInfo.itmResult );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( ORDKEYVAL )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
      pOrderInfo.itmResult = zh_itemNew( NULL );
      SELF_ORDINFO( pArea, DBOI_KEYVAL, &pOrderInfo );
      zh_itemReturnRelease( pOrderInfo.itmResult );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( ORDKEYADD )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
      pOrderInfo.itmOrder = zh_param( 1, ZH_IT_STRING | ZH_IT_NUMERIC );
      pOrderInfo.atomBagName = zh_param( 2, ZH_IT_STRING );
      /* Either or both may be NIL */
      pOrderInfo.itmNewVal = zh_param( 3, ZH_IT_ANY );
      pOrderInfo.itmResult = zh_itemPutNL( NULL, 0 );
      SELF_ORDINFO( pArea, DBOI_KEYADD, &pOrderInfo );
      zh_itemReturnRelease( pOrderInfo.itmResult );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( ORDKEYDEL )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
      pOrderInfo.itmOrder = zh_param( 1, ZH_IT_STRING | ZH_IT_NUMERIC );
      pOrderInfo.atomBagName = zh_param( 2, ZH_IT_STRING );
      /* Either or both may be NIL */
      pOrderInfo.itmNewVal = zh_param( 3, ZH_IT_ANY );
      pOrderInfo.itmResult = zh_itemPutNL( NULL, 0 );
      SELF_ORDINFO( pArea, DBOI_KEYDELETE, &pOrderInfo );
      zh_itemReturnRelease( pOrderInfo.itmResult );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( ORDDESCEND )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
      pOrderInfo.itmOrder = zh_param( 1, ZH_IT_STRING | ZH_IT_NUMERIC );
      pOrderInfo.atomBagName = zh_param( 2, ZH_IT_STRING );
      /* Either or both may be NIL */
      pOrderInfo.itmNewVal = zh_param( 3, ZH_IT_LOGICAL );
      pOrderInfo.itmResult = zh_itemPutL( NULL, ZH_FALSE );
      SELF_ORDINFO( pArea, DBOI_ISDESC, &pOrderInfo );
      zh_itemReturnRelease( pOrderInfo.itmResult );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( ORDISUNIQUE )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
      pOrderInfo.itmOrder = zh_param( 1, ZH_IT_STRING | ZH_IT_NUMERIC );
      pOrderInfo.atomBagName = zh_param( 2, ZH_IT_STRING );
      pOrderInfo.itmNewVal = zh_param( 3, ZH_IT_LOGICAL );
      pOrderInfo.itmResult = zh_itemPutL( NULL, ZH_FALSE );
      SELF_ORDINFO( pArea, DBOI_UNIQUE, &pOrderInfo );
      zh_itemReturnRelease( pOrderInfo.itmResult );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( ORDCUSTOM )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
      pOrderInfo.itmOrder = zh_param( 1, ZH_IT_STRING | ZH_IT_NUMERIC );
      pOrderInfo.atomBagName = zh_param( 2, ZH_IT_STRING );
      /* Either or both may be NIL */
      pOrderInfo.itmNewVal = zh_param( 3, ZH_IT_LOGICAL );
      pOrderInfo.itmResult = zh_itemPutL( NULL, ZH_FALSE );
      SELF_ORDINFO( pArea, DBOI_CUSTOM, &pOrderInfo );
      zh_itemReturnRelease( pOrderInfo.itmResult );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( DBINFO )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      PZH_ITEM pIndex;

      pIndex = zh_param( 1, ZH_IT_NUMERIC );
      if( pIndex )
      {
         PZH_ITEM pInfo = zh_itemParam( 2 );

         SELF_INFO( pArea, ( ZH_USHORT ) zh_itemGetNI( pIndex ), pInfo );
         zh_itemReturnRelease( pInfo );
      }
      else
         zh_errRT_DBCMD( EG_ARG, EDBCMD_DBINFOBADPARAMETER, NULL, ZH_ERR_FUNCNAME );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( DBORDERINFO )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      PZH_ITEM pType = zh_param( 1, ZH_IT_NUMERIC );
      if( pType )
      {
         DBORDERINFO pOrderInfo;

         /* atomBagName may be NIL */
         pOrderInfo.atomBagName = zh_param( 2, ZH_IT_STRING | ZH_IT_NUMERIC );
         pOrderInfo.itmOrder = zh_param( 3, ZH_IT_STRING | ZH_IT_NUMERIC );

         pOrderInfo.itmNewVal  = zh_param( 4, ZH_IT_ANY );
         pOrderInfo.itmResult  = zh_itemNew( NULL );
         pOrderInfo.itmCobExpr = NULL;
         pOrderInfo.fAllTags   = ZH_FALSE;
         SELF_ORDINFO( pArea, ( ZH_USHORT ) zh_itemGetNI( pType ), &pOrderInfo );
         zh_itemReturnRelease( pOrderInfo.itmResult );
      }
      else
         zh_errRT_DBCMD( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, ZH_ERR_FUNCNAME );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
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
      dbOrderInfo.fUnique = ZH_ISLOGICAL( 5 ) ? ( ZH_BOOL ) zh_parl( 5 ) : zh_setGetUnique();
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

ZH_FUNC( ORDCOUNT )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
      pOrderInfo.atomBagName = zh_param( 1, ZH_IT_STRING );
      pOrderInfo.itmResult = zh_itemPutNI( NULL, 0 );
      SELF_ORDINFO( pArea, DBOI_ORDERCOUNT, &pOrderInfo );
      zh_itemReturnRelease( pOrderInfo.itmResult );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}
