/*
 * Mod() function
 *
 * Copyright 1999 Matthew Hamilton <mhamilton@bunge.com.au>
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
#include "zh_math.h"


ZH_FUNC( MOD )
{
   PZH_ITEM pNumber = zh_param( 1, ZH_IT_NUMERIC );
   PZH_ITEM pBase   = zh_param( 2, ZH_IT_NUMERIC );

   if( pNumber && pBase )
   {
      double dNumber = zh_itemGetND( pNumber );
      double dBase   = zh_itemGetND( pBase ); /* dBase! Cool! */

      if( dBase )
      {
         double dResult = fmod( dNumber, dBase );

         if( dResult && ( dNumber > 0 ? dBase < 0 : dBase > 0 ) )
            dResult += dBase;
         zh_retnd( dResult );
      }
      else
      {
         PZH_ITEM pResult = zh_errRT_BASE_Subst( EG_ZERODIV, 1341, NULL, "%", ZH_ERR_ARGS_BASEPARAMS );

         if( pResult )
         {
            zh_itemReturn( pNumber );
            zh_itemRelease( pResult );
         }
      }
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1085, NULL, "%", 2, zh_param( 1, ZH_IT_ANY ), zh_param( 2, ZH_IT_ANY ) );
}

