/*
 * CT3 mathematical functions
 *   - Floor()
 *   - Ceiling()
 *   - Sign()
 *   - Log10()
 *   - Fact()
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
#include "zh_mather.h"

ZH_FUNC( FLOOR )
{
   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      ZH_MATH_EXCEPTION zh_exc;
      double dResult, dArg = zh_parnd( 1 );

      zh_mathResetError( &zh_exc );
      dResult = floor( dArg );
      if( zh_mathGetError( &zh_exc, ZH_ERR_FUNCNAME, dArg, 0.0, dResult ) )
      {
         if( zh_exc.handled )
            zh_retndlen( zh_exc.retval, zh_exc.retvalwidth, zh_exc.retvaldec );
         else
            zh_retnlen( 0, 0, 0 );
      }
      else
         zh_retnlen( dResult, 0, 0 );
   }
   else
   {
      PZH_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst =
            ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_FLOOR, NULL, ZH_ERR_FUNCNAME, 0,
                            EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else
         zh_retnd( 0.0 );
   }
}

ZH_FUNC( CEILING )
{
   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      ZH_MATH_EXCEPTION zh_exc;
      double dResult, dArg = zh_parnd( 1 );

      zh_mathResetError( &zh_exc );
      dResult = ceil( dArg );
      if( zh_mathGetError( &zh_exc, ZH_ERR_FUNCNAME, dArg, 0.0, dResult ) )
      {
         if( zh_exc.handled )
            zh_retndlen( zh_exc.retval, zh_exc.retvalwidth, zh_exc.retvaldec );
         else
            zh_retnlen( 0, 0, 0 );
      }
      else
         zh_retnlen( dResult, 0, 0 );
   }
   else
   {
      PZH_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_CEILING, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else
         zh_retnd( 0.0 );
   }
}

ZH_FUNC( SIGN )
{
   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      double dInput = zh_parnd( 1 );
      int iResult;

      if( dInput == 0.00 )
         iResult = 0;
      else if( dInput > 0.00 )
         iResult = 1;
      else
         iResult = -1;

      zh_retni( iResult );
   }
   else
   {
      PZH_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_SIGN, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else
         zh_retni( 0 );
   }
}

ZH_FUNC( LOG10 )
{
   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      ZH_MATH_EXCEPTION zh_exc;
      double dResult, dArg = zh_parnd( 1 );

      zh_mathResetError( &zh_exc );
      dResult = log10( dArg );
      if( zh_mathGetError( &zh_exc, ZH_ERR_FUNCNAME, dArg, 0.0, dResult ) )
      {
         if( zh_exc.handled )
            zh_retndlen( zh_exc.retval, zh_exc.retvalwidth, zh_exc.retvaldec );
         else
         {
            /* math exception is up to the Ziher function, so do this as Clipper compatible as possible */
            switch( zh_exc.type )
            {
               case ZH_MATH_ERR_SING:               /* argument to log was 0.0 */
               case ZH_MATH_ERR_DOMAIN:             /* argument to log was < 0.0 */
                  zh_retndlen( -HUGE_VAL, -1, -1 ); /* return -infinity */
                  break;

               default:
                  zh_retnd( 0.0 );
                  break;
            }
         }
      }
      else
         zh_retnd( dResult );
   }
   else
   {
      PZH_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_LOG10, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else
         zh_retni( 0 );
   }
}

ZH_FUNC( FACT )
{
   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      int iInput = zh_parni( 1 );

      if( iInput >= 0 && iInput < 22 )
      {
         double dResult = 1.0;
         int i;

         for( i = 1; i <= iInput; i++ )
            dResult *= ( double ) i;
         zh_retnd( dResult );
      }
      else
         zh_retnd( -1.0 );
   }
   else
   {
      PZH_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_FACT, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else
         zh_retnd( 0.0 );
   }
}
