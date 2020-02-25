/*
 * CT3 trigonometric functions
 *     - Pi()
 *     - Sin()
 *     - Cos()
 *     - Tan()
 *     - Cot()
 *     - Asin()
 *     - Acos()
 *     - Atan()
 *     - Sinh()
 *     - Cosh()
 *     - Tanh()
 *     - Atn2()
 *     - RToD()
 *     - DToR()
 *
 * Copyright 2001 Alejandro de Garate <alex_degarate@hotmail.com>
 *
 * Documentation and changes concerning error handling Copyright 2001
 *   IntTec GmbH, Freiburg, Germany, Author: Martin Vogel <vogel@inttec.de>
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

ZH_FUNC( PI )
{
   zh_retnd( CT_PI );
}

ZH_FUNC( SIN )
{
   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      ZH_MATH_EXCEPTION zh_exc;
      double dResult, dArg = zh_parnd( 1 );

      zh_mathResetError( &zh_exc );
      dResult = sin( dArg );
      if( zh_mathGetError( &zh_exc, ZH_ERR_FUNCNAME, dArg, 0.0, dResult ) )
      {
         if( zh_exc.handled )
            zh_retndlen( zh_exc.retval, zh_exc.retvalwidth, zh_exc.retvaldec );
         else
            zh_retndlen( HUGE_VAL, -1, -1 );
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
                                  CT_ERROR_SIN, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else
         zh_retnd( 0.0 );
   }
}

ZH_FUNC( COS )
{
   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      ZH_MATH_EXCEPTION zh_exc;
      double dResult, dArg = zh_parnd( 1 );

      zh_mathResetError( &zh_exc );
      dResult = cos( dArg );
      if( zh_mathGetError( &zh_exc, ZH_ERR_FUNCNAME, dArg, 0.0, dResult ) )
      {
         if( zh_exc.handled )
            zh_retndlen( zh_exc.retval, zh_exc.retvalwidth, zh_exc.retvaldec );
         else
            zh_retndlen( HUGE_VAL, -1, -1 );
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
                                  CT_ERROR_COS, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else
         zh_retnd( 0.0 );
   }
}

ZH_FUNC( TAN )
{
   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      ZH_MATH_EXCEPTION zh_exc;
      double dResult, dArg = zh_parnd( 1 );

      zh_mathResetError( &zh_exc );
      dResult = tan( dArg );
      if( zh_mathGetError( &zh_exc, ZH_ERR_FUNCNAME, dArg, 0.0, dResult ) )
      {
         if( zh_exc.handled )
            zh_retndlen( zh_exc.retval, zh_exc.retvalwidth, zh_exc.retvaldec );
         else
            zh_retndlen( HUGE_VAL, -1, -1 );
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
                                  CT_ERROR_TAN, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else
         zh_retnd( 0.0 );
   }
}

ZH_FUNC( COT )
{
   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      ZH_MATH_EXCEPTION zh_exc;
      double dResult, dArg = zh_parnd( 1 );

      zh_mathResetError( &zh_exc );
      dResult = tan( dArg );
      if( zh_mathGetError( &zh_exc, ZH_ERR_FUNCNAME, dArg, 0.0, dResult ) )
         dResult = zh_exc.handled ? zh_exc.retval : 0.0;

      dResult = dResult ? 1 / dResult : HUGE_VAL;
      zh_retnd( dResult );
   }
   else
   {
      PZH_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_COT, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else
         zh_retnd( 0.0 );
   }
}

ZH_FUNC( ASIN )
{
   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      ZH_MATH_EXCEPTION zh_exc;
      double dResult, dArg = zh_parnd( 1 );

      zh_mathResetError( &zh_exc );
      dResult = asin( dArg );
      if( zh_mathGetError( &zh_exc, ZH_ERR_FUNCNAME, dArg, 0.0, dResult ) )
      {
         if( zh_exc.handled )
            zh_retndlen( zh_exc.retval, zh_exc.retvalwidth, zh_exc.retvaldec );
         else
            zh_retndlen( HUGE_VAL, -1, -1 );
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
                                  CT_ERROR_ASIN, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else
         zh_retnd( 0.0 );
   }
}

ZH_FUNC( ACOS )
{
   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      ZH_MATH_EXCEPTION zh_exc;
      double dResult, dArg = zh_parnd( 1 );

      zh_mathResetError( &zh_exc );
      dResult = acos( dArg );
      if( zh_mathGetError( &zh_exc, ZH_ERR_FUNCNAME, dArg, 0.0, dResult ) )
      {
         if( zh_exc.handled )
            zh_retndlen( zh_exc.retval, zh_exc.retvalwidth, zh_exc.retvaldec );
         else
            zh_retndlen( HUGE_VAL, -1, -1 );
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
                                  CT_ERROR_ACOS, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else
         zh_retnd( 0.0 );
   }
}

ZH_FUNC( ATAN )
{
   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      ZH_MATH_EXCEPTION zh_exc;
      double dResult, dArg = zh_parnd( 1 );

      zh_mathResetError( &zh_exc );
      dResult = atan( dArg );
      if( zh_mathGetError( &zh_exc, ZH_ERR_FUNCNAME, dArg, 0.0, dResult ) )
      {
         if( zh_exc.handled )
            zh_retndlen( zh_exc.retval, zh_exc.retvalwidth, zh_exc.retvaldec );
         else
         {
            /* atan() normally doesn't error, but it's safe to return Pi() / 2
               or -Pi() / 2, respectively, as these
               are the boundary result values */
            if( dArg < 0.0 )
               zh_retnd( -CT_PI / 2.0 );
            else
               zh_retnd( CT_PI / 2.0 );
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
                                  CT_ERROR_ATAN, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else
         zh_retnd( 0.0 );
   }
}

ZH_FUNC( ATN2 )
{
   if( ZH_IS_PARAM_NUM( 1 ) && ZH_IS_PARAM_NUM( 2 ) )
   {
      ZH_MATH_EXCEPTION zh_exc;
      double dY = zh_parnd( 1 );
      double dX = zh_parnd( 2 );
      double dResult;

      zh_mathResetError( &zh_exc );
      dResult = atan2( dY, dX );
      if( zh_mathGetError( &zh_exc, ZH_ERR_FUNCNAME, dY, dX, dResult ) )
      {
         if( zh_exc.handled )
            zh_retndlen( zh_exc.retval, zh_exc.retvalwidth, zh_exc.retvaldec );
         else
         {
            /* DOMAIN error: both arguments to atan2() have been 0 */
            /* CT3 behaves very strange here:
               Atn2( 0.0, 0.0 ) == -Pi()
               Atn2( 0.0, -0.0 ) == 0.0
               Atn2( -0.0, 0.0 ) == -PI
               Atn2( -0.0, -0.0 ) == -2 * Pi() */
            if( dX >= 0.0 )
               zh_retnd( -CT_PI );
            else if( dY < 0.0 )
               zh_retnd( -2.0 * CT_PI );
            else
               zh_retnd( 0.0 );
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
                                  CT_ERROR_ATN2, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else
         zh_retnd( 0.0 );
   }
}

ZH_FUNC( SINH )
{
   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      ZH_MATH_EXCEPTION zh_exc;
      double dResult, dArg = zh_parnd( 1 );

      zh_mathResetError( &zh_exc );
      dResult = sinh( dArg );
      if( zh_mathGetError( &zh_exc, ZH_ERR_FUNCNAME, dArg, 0.0, dResult ) )
      {
         if( zh_exc.handled )
            zh_retndlen( zh_exc.retval, zh_exc.retvalwidth, zh_exc.retvaldec );
         else
         {
            /* OVERFLOW error: we have no CT3 behaviour to follow,
               so return +INF or -INF, respectively */
            if( dArg < 0.0 )
               zh_retndlen( -HUGE_VAL, -1, -1 );
            else
               zh_retndlen( HUGE_VAL, -1, -1 );
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
                                  CT_ERROR_SINH, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else
         zh_retnd( 0.0 );
   }
}

ZH_FUNC( COSH )
{
   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      ZH_MATH_EXCEPTION zh_exc;
      double dResult, dArg = zh_parnd( 1 );

      zh_mathResetError( &zh_exc );
      dResult = cosh( dArg );
      if( zh_mathGetError( &zh_exc, ZH_ERR_FUNCNAME, dArg, 0.0, dResult ) )
      {
         if( zh_exc.handled )
            zh_retndlen( zh_exc.retval, zh_exc.retvalwidth, zh_exc.retvaldec );
         else
            /* OVERFLOW error: we have no CT3 behaviour to follow,
               so return +INF */
            zh_retndlen( HUGE_VAL, -1, -1 );
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
                                  CT_ERROR_COSH, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else
         zh_retnd( 0.0 );
   }
}

ZH_FUNC( TANH )
{
   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      ZH_MATH_EXCEPTION zh_exc;
      double dResult, dArg = zh_parnd( 1 );

      zh_mathResetError( &zh_exc );
      dResult = tanh( dArg );
      if( zh_mathGetError( &zh_exc, ZH_ERR_FUNCNAME, dArg, 0.0, dResult ) )
      {
         if( zh_exc.handled )
            zh_retndlen( zh_exc.retval, zh_exc.retvalwidth, zh_exc.retvaldec );
         else
         {
            /* normally, Tanh() doesn't give errors, but let's return -1 or +1,
               respectively, as these are the boundary result values */
            if( dArg < 0.0 )
               zh_retnd( -1.0 );
            else
               zh_retnd( 1.0 );
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
                                  CT_ERROR_TANH, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else
         zh_retnd( 0.0 );
   }
}

ZH_FUNC( RTOD )
{
   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      double dInput = zh_parnd( 1 );
      double dResult = ( 180.0 / CT_PI ) * dInput;

      zh_retnd( dResult );
   }
   else
   {
      PZH_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_RTOD, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else
         zh_retnd( 0.0 );
   }
}

ZH_FUNC( DTOR )
{
   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      double dInput = zh_parnd( 1 );
      double dResult = ( CT_PI / 180.0 ) * dInput;

      zh_retnd( dResult );
   }
   else
   {
      PZH_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_DTOR, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else
         zh_retnd( 0.0 );
   }
}
