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
