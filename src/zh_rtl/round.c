/*
 * Round(), Int() functions
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 * Copyright 1999 Matthew Hamilton <mhamilton@bunge.com.au> (Int())
 * Copyright 2003 Vicente Aranzana <varanzana@gruposp.com> (zh_numRound())
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
#include "zh_api_error.h"

ZH_FUNC( INT )
{
   PZH_ITEM pNumber = zh_param( 1, ZH_IT_NUMERIC );

   if( pNumber )
   {
      if( ZH_IS_NUMINT( pNumber ) )
         zh_itemReturn( pNumber );
      else
      {
         int iWidth;

         zh_itemGetNLen( pNumber, &iWidth, NULL );
         zh_retnlen( zh_numInt( zh_itemGetND( pNumber ) ), iWidth, 0 );
      }
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1090, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ROUND )
{
   PZH_ITEM pNumber = zh_param( 1, ZH_IT_NUMERIC );

   if( pNumber && ZH_IS_PARAM_NUM( 2 ) )
   {
      int iDec = zh_parni( 2 );

      if( iDec == 0 && ZH_IS_NUMINT( pNumber ) )
         zh_retnint( zh_itemGetNInt( pNumber ) );
      else
         zh_retnlen( zh_numRound( zh_itemGetND( pNumber ), iDec ), 0, ZH_MAX( iDec, 0 ) );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1094, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}
