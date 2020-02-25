/*
 * CT3 string function
 *     - AscPos()
 *     - ValPos()
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

/* defines */
#define DO_ASCPOS_ASCPOS  0
#define DO_ASCPOS_VALPOS  1

/* helper function */
static void do_ascpos( int iSwitch )
{
   if( ZH_ISCHAR( 1 ) )
   {
      ZH_SIZE sStrSize = zh_parclen( 1 );
      const ZH_BYTE * pcString = ( const ZH_BYTE * ) zh_parc( 1 );
      ZH_SIZE sPos = zh_parnsdef( 2, sStrSize );

      if( sPos == 0 || sPos > sStrSize )
         zh_retni( 0 );
      else
      {
         if( iSwitch == DO_ASCPOS_VALPOS )
         {
            if( ZH_ISDIGIT( ( ZH_UCHAR ) pcString[ sPos - 1 ] ) )
               zh_retni( pcString[ sPos - 1 ] - '0' );
            else
               zh_retni( 0 );
         }
         else
            zh_retni( pcString[ sPos - 1 ] );
      }
   }
   else
   {
      PZH_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                  iSwitch == DO_ASCPOS_VALPOS ?
                                  CT_ERROR_VALPOS : CT_ERROR_ASCPOS, NULL,
                                  ZH_ERR_FUNCNAME, 0, EF_CANSUBSTITUTE,
                                  ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else
         zh_retni( 0 );
   }
}

ZH_FUNC( ASCPOS )
{
   do_ascpos( DO_ASCPOS_ASCPOS );
}

ZH_FUNC( VALPOS )
{
   do_ascpos( DO_ASCPOS_VALPOS );
}
