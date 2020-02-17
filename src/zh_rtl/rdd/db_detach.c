/*
 * .prg functions for workarea detaching
 *
 * Copyright 2008 Przemyslaw Czerpak
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
#include "zh_item_api.h"
#include "zh_api_error.h"
#include "zh_thread.h"

/*
 * zh_dbDetach( [<nWorkArea>|<cAlias>], [<xCargo>] ) --> <lSuccess>
 */
ZH_FUNC( ZH_DBDETACH )
{
   PZH_ITEM pAlias = zh_param( 1, ZH_IT_ANY );
   PZH_ITEM pCargo = zh_param( 2, ZH_IT_ANY ); /* ZH_IT_BLOCK in Xbase++ */
   AREAP pArea = NULL;
   int iArea;

   if( ! pAlias || ZH_IS_NIL( pAlias ) )
   {
      pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
   }
   else if( ZH_IS_STRING( pAlias ) )
   {
      const char * szAlias = zh_itemGetCPtr( pAlias );
      zh_rddGetAliasNumber( szAlias, &iArea );
      if( iArea > 0 )
         pArea = ( AREAP ) zh_rddGetWorkAreaPointer( iArea );
   }
   else if( ZH_IS_NUMBER( pAlias ) )
   {
      iArea = zh_itemGetNI( pAlias );
      if( iArea > 0 )
         pArea = ( AREAP ) zh_rddGetWorkAreaPointer( iArea );
   }
   else
   {
      zh_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, ZH_ERR_FUNCNAME );
      return;
   }

   if( pArea )
      zh_retl( zh_rddDetachArea( pArea, pCargo ) == ZH_SUCCESS );
   else
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
}

/*
 * zh_dbRequest( [<cAlias>], [<lFreeArea>], [<@xCargo>], [<nTimeOut>|<lWait>] )
 *          --> <lSuccess>
 */
ZH_FUNC( ZH_DBREQUEST )
{
   if( ZH_ISNIL( 1 ) || ZH_ISCHAR( 1 ) )
   {
      const char * szAlias = zh_parc( 1 );
      ZH_BOOL fNewArea = zh_parl( 2 );
      PZH_ITEM pCargo = ZH_ISBYREF( 3 ) ? zh_itemNew( NULL ) : NULL;
      ZH_ULONG ulMilliSec = ZH_THREAD_INFINITE_WAIT;
      AREAP pArea;

      if( ZH_IS_PARAM_NUM( 4 ) )
      {
         double dTimeOut = zh_parnd( 4 );
         ulMilliSec = dTimeOut > 0 ? ( ZH_ULONG ) ( dTimeOut * 1000 ) : 0;
      }
      else if( ! zh_parl( 4 ) )
         ulMilliSec = 0;

      pArea = zh_rddRequestArea( szAlias, pCargo, fNewArea, ulMilliSec );
      if( pArea )
         zh_rddSelectWorkAreaNumber( pArea->uiArea );

      if( pCargo )
      {
         zh_itemParamStoreForward( 3, pCargo );
         zh_itemRelease( pCargo );
      }

      zh_retl( pArea != NULL );
   }
   else
      zh_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, ZH_ERR_FUNCNAME );
}
