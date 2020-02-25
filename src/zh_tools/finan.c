/*
 * CT3 Financial functions
 *     - PV()
 *     - FV()
 *     - Payment()
 *     - Periods()
 *     - Rate()
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

ZH_FUNC( FV )
{
   if( ZH_IS_PARAM_NUM( 1 ) && ZH_IS_PARAM_NUM( 2 ) && ZH_IS_PARAM_NUM( 3 ) )
   {
      double dPayment = zh_parnd( 1 );
      double dRate = zh_parnd( 2 );
      double dTime = zh_parnd( 3 );
      double dResult;

      if( dRate == 0.0 )
      {
         /* NOTE: CT3 crashes with dRate == 0.0 */
         dResult = dPayment * dTime;
      }
      else
      {
         ZH_MATH_EXCEPTION zh_exc;
         double dBase = 1.0 + dRate;

         zh_mathResetError( &zh_exc );
         dResult = pow( dBase, dTime );

         if( zh_mathGetError( &zh_exc, ZH_ERR_FUNCNAME, dBase, dTime, dResult ) )
            dResult = zh_exc.handled ? zh_exc.retval : 0.0;

         dResult = dPayment * ( dResult - 1.0 ) / dRate;
      }

      zh_retnd( dResult );
   }
   else
   {
      PZH_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_FV, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else
         zh_retnd( 0.0 );
   }
}

ZH_FUNC( PV )
{
   if( ZH_IS_PARAM_NUM( 1 ) && ZH_IS_PARAM_NUM( 2 ) && ZH_IS_PARAM_NUM( 3 ) )
   {
      double dPayment = zh_parnd( 1 );
      double dRate = zh_parnd( 2 );
      double dTime = zh_parnd( 3 );
      double dResult;

      if( dRate == 0.0 )
      {
         /* NOTE: CT3 crashes with dRate == 0.0 */
         dResult = dPayment * dTime;
      }
      else
      {
         ZH_MATH_EXCEPTION zh_exc;
         double dBase = 1.0 + dRate;

         zh_mathResetError( &zh_exc );
         dResult = pow( dBase, -dTime );

         if( zh_mathGetError( &zh_exc, ZH_ERR_FUNCNAME, dBase, -dTime, dResult ) )
            dResult = zh_exc.handled ? zh_exc.retval : 0.0;

         dResult = dPayment * ( 1.0 - dResult ) / dRate;
      }

      zh_retnd( dResult );
   }
   else
   {
      PZH_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_PV, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else
         zh_retnd( 0.0 );
   }
}

ZH_FUNC( PAYMENT )
{
   if( ZH_IS_PARAM_NUM( 1 ) && ZH_IS_PARAM_NUM( 2 ) && ZH_IS_PARAM_NUM( 3 ) )
   {
      double dCapital = zh_parnd( 1 );
      double dRate = zh_parnd( 2 );
      double dTime = zh_parnd( 3 );
      double dResult;

      if( dRate == 0.0 )
      {
         /* NOTE: CT3 crashes with dRate == 0.0 */
         dResult = dCapital / dTime;
      }
      else
      {
         ZH_MATH_EXCEPTION zh_exc;
         double dBase = 1.0 + dRate;

         zh_mathResetError( &zh_exc );
         dResult = pow( dBase, -dTime );

         if( zh_mathGetError( &zh_exc, ZH_ERR_FUNCNAME, dBase, -dTime, dResult ) )
            dResult = zh_exc.handled ? zh_exc.retval : 0.0;

         dResult = dCapital * dRate / ( 1.0 - dResult );
      }

      zh_retnd( dResult );
   }
   else
   {
      PZH_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_PAYMENT, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else
         zh_retnd( 0.0 );
   }
}

ZH_FUNC( PERIODS )
{
   if( ZH_IS_PARAM_NUM( 1 ) && ZH_IS_PARAM_NUM( 2 ) && ZH_IS_PARAM_NUM( 3 ) )
   {
      double dCapital = zh_parnd( 1 );
      double dPayment = zh_parnd( 2 );
      double dRate = zh_parnd( 3 );
      double dResult;

      if( dPayment <= dCapital * dRate )
      {
         /* in this case infinite time is needed to cancel the loan */
         dResult = -1.0;
      }
      else if( dRate == 0.0 )
      {
         /* NOTE: CT3 crashes with dRate == 0.0 */
         dResult = dCapital / dPayment;
      }
      else
      {
         ZH_MATH_EXCEPTION zh_exc;
         double dBase = 1.0 + dRate;

         zh_mathResetError( &zh_exc );
         dResult = log( dBase );
         if( zh_mathGetError( &zh_exc, ZH_ERR_FUNCNAME, dBase, 0.0, dResult ) )
            dResult = zh_exc.handled ? zh_exc.retval : 0.0;

         if( dResult )
         {
            double dResult2;
            zh_mathResetError( &zh_exc );
            dBase = 1.0 - ( dCapital * dRate / dPayment );
            dResult2 = log( dBase );

            if( zh_mathGetError( &zh_exc, ZH_ERR_FUNCNAME, dBase, 0.0, dResult2 ) )
               dResult2 = zh_exc.handled ? zh_exc.retval : 0.0;

            dResult = -dResult2 / dResult;
         }
      }

      zh_retnd( dResult );
   }
   else
   {
      PZH_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_PERIODS, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else
         zh_retnd( 0.0 );
   }
}

ZH_FUNC( RATE )
{
   if( ZH_IS_PARAM_NUM( 1 ) && ZH_IS_PARAM_NUM( 2 ) && ZH_IS_PARAM_NUM( 3 ) )
   {
      double dCapital = zh_parnd( 1 );
      double dPayment = zh_parnd( 2 );
      double dTime = zh_parnd( 3 );
      double dEpsilon = 0.00001;        /* minimal to consider 2 numbers as equal */
      double dScale = 1.0;      /* fractional step */
      double j = 1.0;           /* index */

      while( j < 1020.0 )       /* maximum annual rate */
      {
         double dAux;           /* estimated payment to compare for */
         double r;              /* temptative rate */
         double dExp;

         ZH_MATH_EXCEPTION zh_exc;
         double dBase;

         r = j * 0.000833333;   /* j * ( 0.01 / 12.0)  mensual's rate */

         /* replace Payment() function overhead */

         zh_mathResetError( &zh_exc );
         dBase = 1.0 + r;
         dExp = pow( dBase, dTime );
         if( zh_mathGetError( &zh_exc, ZH_ERR_FUNCNAME, dBase, dTime, dExp ) )
         {
            /* TODO: Check if this is a correct default correction value for pow() */
            dExp = zh_exc.handled ? zh_exc.retval : 0.0;
         }

         dAux = dCapital * ( ( dExp * r ) / ( dExp - 1.0 ) );

         if( dAux > dPayment )
         {
            j -= dScale;
            dScale = dScale * 0.10;

            if( ( dAux - dPayment ) < dEpsilon )
               break;
         }
         else
            j += dScale;
      }

      zh_retnd( j * 0.000833333 );      /* return as mensual's rate */
   }
   else
   {
      PZH_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_RATE, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else
         zh_retnd( 0.0 );
   }
}
