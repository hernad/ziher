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
      /* Ziher extension: NewVal to set/reset unique flag */
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

ZH_FUNC( DBFIELDINFO )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      ZH_USHORT uiFields, uiIndex;
      PZH_ITEM pType;

      pType = zh_param( 1, ZH_IT_NUMERIC );
      uiIndex = ( ZH_FIELDNO ) zh_parni( 2 );
      if( pType && SELF_FIELDCOUNT( pArea, &uiFields ) == ZH_SUCCESS &&
          uiIndex > 0 && uiIndex <= uiFields )
      {
         PZH_ITEM pInfo = zh_itemNew( zh_param( 3, ZH_IT_ANY ) );

         SELF_FIELDINFO( pArea, uiIndex, ( ZH_USHORT ) zh_itemGetNI( pType ), pInfo );
         zh_itemReturnRelease( pInfo );
      }
      else
         zh_errRT_DBCMD( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, ZH_ERR_FUNCNAME );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

ZH_FUNC( DBRECORDINFO )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      PZH_ITEM pType, pRecNo;

      pType = zh_param( 1, ZH_IT_NUMERIC );
      pRecNo = zh_param( 2, ZH_IT_ANY );
      if( pType )
      {
         PZH_ITEM pInfo = zh_itemParam( 3 );

         SELF_RECINFO( pArea, pRecNo, ( ZH_USHORT ) zh_itemGetNI( pType ), pInfo );
         zh_itemReturnRelease( pInfo );
      }
      else
         zh_errRT_DBCMD( EG_ARG, EDBCMD_INFOBADPARAMETER, NULL, ZH_ERR_FUNCNAME );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

/*
 * dbFileGet()/Blob2File() - retrieve memo contents into file
 */
ZH_FUNC( DBFILEGET )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      ZH_USHORT uiFields, uiIndex;
      PZH_ITEM pMode;
      const char * szField = zh_parc( 1 );

      if( szField )
         uiIndex = zh_rddFieldIndex( pArea, szField );
      else
         uiIndex = ( ZH_FIELDNO ) zh_parni( 1 );

      pMode = zh_param( 3, ZH_IT_NUMERIC );
      if( uiIndex > 0 && pMode && zh_parclen( 2 ) > 0 &&
          SELF_FIELDCOUNT( pArea, &uiFields ) == ZH_SUCCESS &&
          uiIndex <= uiFields )
      {
         zh_retl( SELF_GETVALUEFILE( pArea, uiIndex, zh_parc( 2 ),
                                     ( ZH_USHORT ) zh_itemGetNI( pMode ) ) == ZH_SUCCESS );
      }
      else
         zh_errRT_DBCMD( EG_ARG, EDBCMD_DBFILEGETBADPARAMETER, NULL, ZH_ERR_FUNCNAME );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

/*
 * dbFilePut()/File2Blob() - store file contents in MEMO
 */
ZH_FUNC( DBFILEPUT )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      ZH_USHORT uiFields, uiIndex;
      const char * szField = zh_parc( 1 );

      if( szField )
         uiIndex = zh_rddFieldIndex( pArea, szField );
      else
         uiIndex = ( ZH_FIELDNO ) zh_parni( 1 );
      if( uiIndex > 0 && zh_parclen( 2 ) > 0 &&
          SELF_FIELDCOUNT( pArea, &uiFields ) == ZH_SUCCESS &&
          uiIndex <= uiFields )
      {
         zh_retl( SELF_PUTVALUEFILE( pArea, uiIndex, zh_parc( 2 ),
                                     ( ZH_USHORT ) zh_parni( 3 ) ) == ZH_SUCCESS );
      }
      else
         zh_errRT_DBCMD( EG_ARG, EDBCMD_DBFILEPUTBADPARAMETER, NULL, ZH_ERR_FUNCNAME );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}
