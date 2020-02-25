/*
 * CT3 string functions
 *     - CharSwap()
 *     - WordSwap()
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
#define DO_CHARSWAP_CHARSWAP           0
#define DO_CHARSWAP_WORDSWAP           1
#define DO_CHARSWAP_WORDSWAP_CHARSWAP  2

/* helper function for the CharSwap() and WordSwap() functions */
static void do_charswap( int iSwitch )
{
   /* suppress return value ? */
   int iNoRet = ct_getref() && ZH_ISBYREF( 1 );

   /* param check */
   if( ZH_ISCHAR( 1 ) )
   {
      const char * pcString = zh_parc( 1 );
      ZH_SIZE sStrLen = zh_parclen( 1 );
      char * pcRet;
      ZH_SIZE sRetIndex = 0;
      int iShift, iMod;
      const char * pcSub;

      if( sStrLen == 0 )
      {
         if( iNoRet )
            zh_ret();
         else
            zh_retc_null();
         return;
      }

      if( iSwitch == DO_CHARSWAP_WORDSWAP )
      {
         iShift = 4;
         if( zh_parl( 2 ) )
            iSwitch = DO_CHARSWAP_WORDSWAP_CHARSWAP;
      }
      else
         iShift = 2;

      pcRet = ( char * ) zh_xgrab( sStrLen );

      for( pcSub = pcString; pcSub < pcString + sStrLen + 1 - iShift; pcSub += iShift )
      {
         switch( iSwitch )
         {
            case DO_CHARSWAP_WORDSWAP:
               pcRet[ sRetIndex++ ] = pcSub[ 2 ];
               pcRet[ sRetIndex++ ] = pcSub[ 3 ];
               pcRet[ sRetIndex++ ] = pcSub[ 0 ];
               pcRet[ sRetIndex++ ] = pcSub[ 1 ];
               break;

            case DO_CHARSWAP_WORDSWAP_CHARSWAP:
               pcRet[ sRetIndex++ ] = pcSub[ 3 ];
               pcRet[ sRetIndex++ ] = pcSub[ 2 ];
               /* fallthrough */
            case DO_CHARSWAP_CHARSWAP:
               pcRet[ sRetIndex++ ] = pcSub[ 1 ];
               pcRet[ sRetIndex++ ] = pcSub[ 0 ];
         }
      }

      /* copy rest of string */
      if( iSwitch == DO_CHARSWAP_WORDSWAP || iSwitch == DO_CHARSWAP_WORDSWAP_CHARSWAP )
         iMod = sStrLen % 4;
      else
         iMod = sStrLen % 2;

      for( pcSub = pcString + sStrLen - iMod; pcSub < pcString + sStrLen; pcSub++ )
         pcRet[ sRetIndex++ ] = *pcSub;

      /* return string */
      zh_storclen( pcRet, sRetIndex, 1 );

      if( iNoRet )
         zh_retl( ZH_FALSE );
      else
         zh_retclen( pcRet, sRetIndex );
      zh_xfree( pcRet );
   }
   else
   {
      PZH_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
      {
         if( iSwitch == DO_CHARSWAP_CHARSWAP )
            pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                     CT_ERROR_CHARSWAP,
                                     NULL, ZH_ERR_FUNCNAME, 0, EF_CANSUBSTITUTE,
                                     ZH_ERR_ARGS_BASEPARAMS );
         else
            pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                     CT_ERROR_WORDSWAP,
                                     NULL, ZH_ERR_FUNCNAME, 0, EF_CANSUBSTITUTE,
                                     ZH_ERR_ARGS_BASEPARAMS );
      }

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else if( iNoRet )
         zh_retl( ZH_FALSE );
      else
         zh_retc_null();
   }
}

ZH_FUNC( CHARSWAP )
{
   do_charswap( DO_CHARSWAP_CHARSWAP );
}

ZH_FUNC( WORDSWAP )
{
   do_charswap( DO_CHARSWAP_WORDSWAP );
}
