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
