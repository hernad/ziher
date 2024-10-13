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

