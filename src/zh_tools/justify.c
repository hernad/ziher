/*
 * JustLeft() and JustRight() CT3 string functions
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
#define DO_JUSTIFY_JUSTLEFT   0
#define DO_JUSTIFY_JUSTRIGHT  1

/* helper function for the Just*() functions */
static void do_justify( int iSwitch )
{
   int iNoRet = ct_getref() && ZH_ISBYREF( 1 );

   if( ZH_ISCHAR( 1 ) )
   {
      const char * pcString = zh_parc( 1 );
      ZH_SIZE sStrLen = zh_parclen( 1 );
      char cJustChar;
      const char * pc;
      char * pcRet, * pcw;
      ZH_SIZE sJustOffset;

      if( sStrLen == 0 )
      {
         if( iNoRet )
            zh_ret();
         else
            zh_retc_null();
         return;
      }

      if( zh_parclen( 2 ) > 0 )
         cJustChar = *( zh_parc( 2 ) );
      else if( ZH_IS_PARAM_NUM( 2 ) )
         cJustChar = ( char ) ( zh_parnl( 2 ) % 256 );
      else
         cJustChar = 0x20;

      pcRet = ( char * ) zh_xgrab( sStrLen + 1 );

      switch( iSwitch )
      {
         case DO_JUSTIFY_JUSTLEFT:
            pc = pcString;
            sJustOffset = 0;
            while( *pc == cJustChar && pc < ( pcString + sStrLen ) )
            {
               sJustOffset++;
               pc++;
            }

            zh_xmemcpy( pcRet, pcString + sJustOffset, sStrLen - sJustOffset );

            for( pcw = pcRet + sStrLen - sJustOffset; pcw < pcRet + sStrLen; pcw++ )
               *pcw = cJustChar;

            break;

         case DO_JUSTIFY_JUSTRIGHT:
            pc = pcString + sStrLen - 1;
            sJustOffset = 0;
            while( *pc == cJustChar && pc >= pcString )
            {
               sJustOffset++;
               pc--;
            }

            for( pcw = pcRet; pcw < pcRet + sJustOffset; pcw++ )
               *pcw = cJustChar;

            zh_xmemcpy( pcRet + sJustOffset, pcString, sStrLen - sJustOffset );
            break;
      }

      zh_storclen( pcRet, sStrLen, 1 );

      if( iNoRet )
      {
         zh_ret();
         zh_xfree( pcRet );
      }
      else
         zh_retclen_buffer( pcRet, sStrLen );
   }
   else
   {
      PZH_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                  iSwitch == DO_JUSTIFY_JUSTLEFT ?
                                  CT_ERROR_JUSTLEFT : CT_ERROR_JUSTRIGHT,
                                  NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else if( iNoRet )
         zh_ret();
      else
         zh_retc_null();
   }
}

ZH_FUNC( JUSTLEFT )
{
   do_justify( DO_JUSTIFY_JUSTLEFT );
}

ZH_FUNC( JUSTRIGHT )
{
   do_justify( DO_JUSTIFY_JUSTRIGHT );
}
