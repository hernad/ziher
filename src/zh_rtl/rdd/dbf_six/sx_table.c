/*
 * SIX compatible functions:
 *       sx_GetLocks()
 *       sx_IsFLocked()
 *       sx_IsReadonly()
 *       sx_IsShared()
 *       sx_IDType()
 *       sx_TableType()
 *       sx_TableName()
 *       sx_Rollback()
 *       sx_Rlock()
 *       sx_Unlock()
 *       sx_SetPass()
 *       sx_DbfEncrypt()
 *       sx_DbfDecrypt()
 *       sx_MemoPack()
 *       sx_SetTurbo()
 *       sx_TurboArea()
 *       _sxOpenInit() (internal function used by _sx_IniInit())
 *
 * Copyright 2007 Przemyslaw Czerpak
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
#include "zh_fs_api.h"
#include "zh_rdd_api.h"
#include "zh_error_api.h"


ZH_FUNC( SX_GETLOCKS )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      PZH_ITEM pList = zh_itemArrayNew( 0 );
      SELF_INFO( pArea, DBI_GETLOCKARRAY, pList );
      zh_itemReturnRelease( pList );
   }
}

ZH_FUNC( SX_ISFLOCKED )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
   ZH_BOOL fLocked = ZH_FALSE;

   if( pArea )
   {
      PZH_ITEM pItem = zh_itemNew( NULL );
      SELF_INFO( pArea, DBI_ISFLOCK, pItem );
      fLocked = zh_itemGetL( pItem );
      zh_itemRelease( pItem );
   }

   zh_retl( fLocked );
}

ZH_FUNC( SX_ISREADONLY )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
   ZH_BOOL fReadOnly = ZH_FALSE;

   if( pArea )
   {
      PZH_ITEM pItem = zh_itemNew( NULL );
      SELF_INFO( pArea, DBI_ISREADONLY, pItem );
      fReadOnly = zh_itemGetL( pItem );
      zh_itemRelease( pItem );
   }

   zh_retl( fReadOnly );
}

ZH_FUNC( SX_ISSHARED )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
   ZH_BOOL fShared = ZH_FALSE;

   if( pArea )
   {
      PZH_ITEM pItem = zh_itemNew( NULL );
      SELF_INFO( pArea, DBI_SHARED, pItem );
      fShared = zh_itemGetL( pItem );
      zh_itemRelease( pItem );
   }

   zh_retl( fShared );
}

ZH_FUNC( SX_IDTYPE )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
   int iType = 0;

   if( pArea )
   {
      PZH_ITEM pItem = zh_itemNew( NULL );
      if( SELF_RECINFO( pArea, NULL, DBRI_ENCRYPTED, pItem ) == ZH_SUCCESS )
         iType = zh_itemGetL( pItem ) ? 2 : 1;
      zh_itemRelease( pItem );
   }

   zh_retni( iType );
}

ZH_FUNC( SX_TABLETYPE )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
   int iType = 0;

   if( pArea )
   {
      PZH_ITEM pItem = zh_itemNew( NULL );
      if( SELF_INFO( pArea, DBI_ISENCRYPTED, pItem ) == ZH_SUCCESS )
         iType = zh_itemGetL( pItem ) ? 2 : 1;
      zh_itemRelease( pItem );
   }

   zh_retni( iType );
}

ZH_FUNC( SX_TABLENAME )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      PZH_ITEM pList = zh_itemNew( NULL );
      SELF_INFO( pArea, DBI_FULLPATH, pList );
      zh_itemReturnRelease( pList );
   }
   else
      zh_retc_null();
}

static void zh_sxRollBackChild( AREAP pArea, PZH_ITEM pItem )
{
   LPDBRELINFO lpdbRelation = pArea->lpdbRelations;

   while( lpdbRelation )
   {
      if( SELF_INFO( lpdbRelation->lpaChild, DBI_ROLLBACK, pItem ) != ZH_SUCCESS )
         break;
      zh_sxRollBackChild( lpdbRelation->lpaChild, pItem );
      lpdbRelation = lpdbRelation->lpdbriNext;
   }
}

ZH_FUNC( SX_ROLLBACK )
{
   ZH_BOOL fResult = ZH_FALSE, fRollChild = ZH_FALSE;
   int iArea = 0;
   AREAP pArea;

   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      iArea = zh_parni( 1 );
      fRollChild = iArea == 0;
   }

   if( iArea )
      pArea = ( AREAP ) zh_rddGetWorkAreaPointer( iArea );
   else
      pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      PZH_ITEM pItem = zh_itemNew( NULL );
      fResult = SELF_INFO( pArea, DBI_ROLLBACK, pItem ) == ZH_SUCCESS;
      if( fResult && fRollChild )
         zh_sxRollBackChild( pArea, pItem );
      zh_itemRelease( pItem );
   }

   zh_retl( fResult );
}

ZH_FUNC( SX_RLOCK )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
   ZH_BOOL fResult = ZH_FALSE;
   PZH_ITEM pResult = NULL;

   if( pArea )
   {
      PZH_ITEM pRecords = zh_param( 1, ZH_IT_ARRAY );
      DBLOCKINFO dbLockInfo;
      dbLockInfo.fResult = ZH_FALSE;
      dbLockInfo.uiMethod = DBLM_MULTIPLE;
      if( pRecords )
      {
         ZH_SIZE nPos, nLen = zh_arrayLen( pRecords );
         pResult = zh_itemArrayNew( nLen );
         for( nPos = 1; nPos <= nLen; ++nPos )
         {
            dbLockInfo.itmRecID = zh_arrayGetItemPtr( pRecords, nPos );
            SELF_LOCK( pArea, &dbLockInfo );
            zh_arraySetL( pResult, nPos, dbLockInfo.fResult );
         }
      }
      else
      {
         dbLockInfo.itmRecID = zh_param( 1, ZH_IT_ANY );
         SELF_LOCK( pArea, &dbLockInfo );
         fResult = dbLockInfo.fResult;
      }
   }

   if( pResult )
      zh_itemReturnRelease( pResult );
   else
      zh_retl( fResult );
}

ZH_FUNC( SX_UNLOCK )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      PZH_ITEM pRecords = zh_param( 1, ZH_IT_ARRAY );
      if( pRecords )
      {
         ZH_SIZE nPos, nLen = zh_arrayLen( pRecords );
         for( nPos = 1; nPos <= nLen; ++nPos )
         {
            SELF_UNLOCK( pArea, zh_arrayGetItemPtr( pRecords, nPos ) );
         }
      }
      else
      {
         SELF_UNLOCK( pArea, zh_param( 1, ZH_IT_ANY ) );
      }
   }
}

ZH_FUNC( SX_SETPASS )
{
   int iPCount = zh_pcount();
   ZH_BOOL fResult = ZH_FALSE;
   PZH_ITEM pItem;

   if( iPCount == 1 )
   {
      if( ZH_ISCHAR( 1 ) )
      {
         AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
         if( pArea )
         {
            pItem = zh_itemParam( 1 );
            if( SELF_INFO( pArea, DBI_PASSWORD, pItem ) == ZH_SUCCESS )
               fResult = ZH_TRUE;
            zh_itemRelease( pItem );
         }
      }
   }
   else if( iPCount >= 2 && iPCount <= 4 )
   {
      if( ZH_ISCHAR( 1 ) && ZH_IS_PARAM_NUM( 2 ) && ( iPCount < 3 || ZH_ISCHAR( 3 ) ) &&
          ( iPCount < 4 || ZH_IS_PARAM_NUM( 4 ) ) )
      {
         /* Set pending password for table which will be open
          * 3rd and 4th parameters are optional Ziher extensions
          * with RDD name and connection number.
          */
         LPRDDNODE pRDDNode;
         ZH_USHORT uiRddID;
         const char * szDriver;

         if( iPCount == 2 ) /* no RDD parameter, use default */
            szDriver = zh_rddDefaultDrv( NULL );
         else
            szDriver = zh_parc( 3 );
         pRDDNode = zh_rddFindNode( szDriver, &uiRddID );   /* find the RDDNODE */
         if( pRDDNode )
         {
            pItem = zh_itemParam( 1 );
            if( SELF_RDDINFO( pRDDNode, RDDI_PENDINGPASSWORD, zh_parnl( 4 ), pItem ) == ZH_SUCCESS )
               fResult = ZH_TRUE;
            zh_itemRelease( pItem );
         }
      }
      else if( iPCount == 2 && ZH_IS_PARAM_NUM( 1 ) && ZH_ISCHAR( 2 ) )
      {
         AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
         if( pArea )
         {
            /* Undocumented SIX3 extension */
            switch( zh_parni( 1 ) )
            {
               case 1:  /* return current password key in raw form */
                  pItem = zh_itemNew( NULL );
                  if( SELF_INFO( pArea, DBI_PASSWORD, pItem ) == ZH_SUCCESS )
                     zh_itemReturn( pItem );
                  zh_itemRelease( pItem );
                  break;
               case 2:  /* set raw password key */
                  /* not implemented */
                  break;
               case 3:  /* mark table as encrypted */
                  /* intentionally not implemented */
                  break;
               case 4:  /* mark table as decrypted */
                  /* intentionally not implemented */
                  break;
            }
            return;
         }
      }
   }

   zh_retl( fResult );
}

ZH_FUNC( SX_DBFENCRYPT )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
   ZH_BOOL fResult = ZH_FALSE;

   if( pArea )
   {
      PZH_ITEM pItem = zh_itemParam( 1 );

      if( SELF_INFO( pArea, DBI_ENCRYPT, pItem ) == ZH_SUCCESS )
         fResult = zh_itemGetL( pItem );
      zh_itemRelease( pItem );
   }
   zh_retl( fResult );
}

ZH_FUNC( SX_DBFDECRYPT )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
   ZH_BOOL fResult = ZH_FALSE;

   if( pArea )
   {
      PZH_ITEM pItem = zh_itemParam( 1 );
      if( SELF_INFO( pArea, DBI_DECRYPT, pItem ) == ZH_SUCCESS )
         fResult = zh_itemGetL( pItem );
      zh_itemRelease( pItem );
   }
   zh_retl( fResult );
}

ZH_FUNC( SX_MEMOPACK )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
   ZH_BOOL fResult = ZH_FALSE;

   if( pArea )
   {
      PZH_ITEM pItem = zh_itemArrayNew( 3 );
      int i, iPCount = zh_pcount();
      for( i = 1; i <= iPCount; ++i )
         zh_arraySet( pItem, i, zh_param( i, ZH_IT_ANY ) );
      fResult = SELF_INFO( pArea, DBI_MEMOPACK, pItem ) == ZH_SUCCESS;
      zh_itemRelease( pItem );
   }
   zh_retl( fResult );
}

ZH_FUNC( SX_TURBOAREA )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      PZH_ITEM pItem = zh_itemParam( 1 );
      if( zh_pcount() > 0 && ZH_IS_NIL( pItem ) )
         zh_itemPutNI( pItem, 0 );
      if( SELF_INFO( pArea, DBI_DIRTYREAD, pItem ) != ZH_SUCCESS )
         zh_itemPutL( pItem, ZH_FALSE );
      zh_itemReturnRelease( pItem );
   }
   else
      zh_retl( ZH_FALSE );
}

ZH_FUNC( SX_SETTURBO )
{
   LPRDDNODE pRDDNode;
   ZH_USHORT uiRddID;
   const char * szDriver;

   szDriver = zh_parc( 2 );
   if( ! szDriver ) /* no VIA RDD parameter, use default */
      szDriver = zh_rddDefaultDrv( NULL );

   pRDDNode = zh_rddFindNode( szDriver, &uiRddID );  /* find the RDDNODE */
   if( ! pRDDNode )
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME,
                            ZH_ERR_ARGS_BASEPARAMS );
   else
   {
      PZH_ITEM pItem = zh_itemParam( 1 );
      if( zh_pcount() > 0 && ZH_IS_NIL( pItem ) )
         zh_itemPutNI( pItem, 0 );
      if( SELF_RDDINFO( pRDDNode, RDDI_DIRTYREAD, 0, pItem ) != ZH_SUCCESS )
         zh_itemPutL( pItem, ZH_FALSE );
      zh_itemReturnRelease( pItem );
   }
}

/*
 * _sxOpenInit( nArea, xShared, xReadOnly, xAlias )
 */
ZH_FUNC( _SXOPENINIT )
{
   AREAP pArea = NULL;
   int iArea = zh_parni( 1 );

   if( iArea )
      pArea = ( AREAP ) zh_rddGetWorkAreaPointer( iArea );

   if( pArea )
   {
      LPDBOPENINFO pInfo = NULL;
      PZH_ITEM pItem = zh_itemNew( NULL );

      if( SELF_INFO( pArea, DBI_OPENINFO, pItem ) )
         pInfo = ( LPDBOPENINFO ) zh_itemGetPtr( pItem );
      zh_itemRelease( pItem );
      if( pInfo )
      {
         if( ZH_ISLOG( 2 ) )
            pInfo->fShared = zh_parl( 2 );
         if( ZH_ISLOG( 3 ) )
            pInfo->fReadonly = zh_parl( 2 );
         if( ZH_ISCHAR( 4 ) )
         {
            const char * szAlias = zh_parc( 1 );
            if( szAlias && szAlias[ 0 ] )
               pInfo->atomAlias = zh_dynsymName( zh_dynsymGet( szAlias ) );
            else
               pInfo->atomAlias = "";
         }
      }
   }
}
