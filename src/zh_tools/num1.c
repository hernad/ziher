/*
 * CT3 Numeric functions - PART 1
 *   - Celsius()
 *   - Fahrenheit()
 *   - Infinity()
 *
 * Copyright 2001 Alejandro de Garate <alex_degarate@hotmail.com>
 * Copyright 2001 IntTec GmbH, Freiburg, Germany
 *        Author: Martin Vogel <vogel@inttec.de> (Documentation and changes concerning error handling)
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

#include "ct.h"
#include "ctmath.h"
#include <float.h>

ZH_FUNC( CELSIUS )
{
   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      double dInput = zh_parnd( 1 );
      double dResult;

      dResult = ( 5.0 / 9.0 ) * ( dInput - 32.0 );
      zh_retnd( dResult );
   }
   else
   {
      PZH_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_CELSIUS, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else
         zh_retnd( 0.0 );
   }
}

ZH_FUNC( FAHRENHEIT )
{
   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      double dInput = zh_parnd( 1 );
      double dResult;

      dResult = ( ( 9.0 / 5.0 ) * dInput ) + 32.0;
      zh_retnd( dResult );
   }
   else
   {
      PZH_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_FAHRENHEIT, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else
         zh_retnd( 0.0 );
   }
}

ZH_FUNC( INFINITY )
{
   if( zh_parl( 1 ) )
      zh_retnd( DBL_MAX );
   else
      zh_retnd( 93786976294838206460.00 );
}
