/*
 * Base RDD module
 *
 * Copyright 1999 Bruno Cantero <bruno@issnet.net>
 * Copyright 2004-2007 Przemyslaw Czerpak
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


/* NOTE: This function is a new Ziher function implemented in the
         original CA-Cl*pper namespace. This should have been
         marked as ZH_EXTENSION, but it's not. */

ZH_FUNC( ORDWILDSEEK )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      const char * szPattern = zh_parc( 1 );

      if( szPattern )
      {
         ZH_BOOL fCont = zh_parl( 2 ), fBack = zh_parl( 3 ), fFound = ZH_FALSE;
         DBORDERINFO OrderInfo;
         ZH_ERRCODE errCode = ZH_SUCCESS;

         memset( &OrderInfo, 0, sizeof( OrderInfo ) );
         OrderInfo.itmResult = zh_itemNew( NULL );

         if( ! fCont )
         {
            if( fBack )
               errCode = SELF_GOBOTTOM( pArea );
            else
               errCode = SELF_GOTOP( pArea );

            if( errCode == ZH_SUCCESS )
            {
               errCode = SELF_ORDINFO( pArea, DBOI_KEYVAL, &OrderInfo );
               if( errCode == ZH_SUCCESS )
                  fFound = zh_strMatchWild( zh_itemGetCPtr( OrderInfo.itmResult ), szPattern );
            }
         }
         if( ! fFound && errCode == ZH_SUCCESS )
         {
            OrderInfo.itmNewVal = zh_param( 1, ZH_IT_STRING );
            if( SELF_ORDINFO( pArea, fBack ? DBOI_SKIPWILDBACK : DBOI_SKIPWILD,
                              &OrderInfo ) == ZH_SUCCESS )
               fFound = zh_itemGetL( OrderInfo.itmResult );
         }
         zh_itemRelease( OrderInfo.itmResult );
         zh_retl( fFound );
      }
      else
         zh_errRT_DBCMD( EG_ARG, EDBCMD_SEEK_BADPARAMETER, NULL, ZH_ERR_FUNCNAME );
   }
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}
