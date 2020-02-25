/*
 * Initialization and switch functions for CT3 math functions
 *
 * Copyright 2001 IntTec GmbH, Neunlindenstr 32, 79106 Freiburg, Germany
 *        Author: Martin Vogel <vogel@inttec.de>
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

#include "zh_stack.h"

/* --- math precision --- */
static void s_iPrecision_init( void * cargo )
{
   int * piPrecision = ( int * ) cargo;

   *piPrecision = 16;
}

static ZH_TSD_NEW( s_iPrecision, sizeof( int ), s_iPrecision_init, NULL );

void ct_setprecision( int iPrecision )
{
   int * piPrecision = ( int * ) zh_stackGetTSD( &s_iPrecision );

   ZH_TRACE( ZH_TR_DEBUG, ( "ct_setprecision (%i)", iPrecision ) );

   *piPrecision = iPrecision;
}

int ct_getprecision( void )
{
   int * piPrecision = ( int * ) zh_stackGetTSD( &s_iPrecision );

   ZH_TRACE( ZH_TR_DEBUG, ( "ct_getprecision()" ) );

   return *piPrecision;
}

ZH_FUNC( SETPREC )
{
   int iPrec = zh_parni( 1 );

   if( iPrec >= 1 && iPrec <= 16 )
      ct_setprecision( iPrec );
   else
   {
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         ct_error( ( ZH_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_SETPREC, NULL,
                   ZH_ERR_FUNCNAME, 0, EF_CANDEFAULT, ZH_ERR_ARGS_BASEPARAMS );
   }
   zh_retc_null();
}

ZH_FUNC( GETPREC )
{
   zh_retni( ct_getprecision() );
   if( zh_pcount() > 0 )
   {
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         ct_error( ( ZH_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_GETPREC, NULL,
                   ZH_ERR_FUNCNAME, 0, EF_CANDEFAULT, ZH_ERR_ARGS_BASEPARAMS );
   }
}
