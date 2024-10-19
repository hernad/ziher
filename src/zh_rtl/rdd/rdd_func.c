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
         zh_errInternal( ZH_EI_RDDINVALID, szDriver, NULL, NULL );
      }
   }
}

ZH_FUNC( RDDSETDEFAULT )
{
   int parSize1 = zh_parclen( 1 );
   printf("rddsetdefault step-1  %d\n", parSize1 );
   getchar();

   zh_retc( zh_rddDefaultDrv( NULL ) );

   if( parSize1 > 0 )
   {
      char *szDrv = zh_parc( 1 );
      printf("rddsetdefault step-2:  %s\n", szDrv);
      if( ! zh_rddDefaultDrv( szDrv ) ) {
         printf("rddsetdefault step-3:  %s\n", szDrv);
         getchar();
         zh_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, ZH_ERR_FUNCNAME );
      }
   }
}
