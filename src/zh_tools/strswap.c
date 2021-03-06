/*
 * StrSwap() CT3 string function
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

ZH_FUNC( STRSWAP )
{
   ZH_SIZE sStrLen1, sStrLen2;

   /* param check */
   if( ( sStrLen1 = zh_parclen( 1 ) ) > 0 &&
       ( sStrLen2 = zh_parclen( 2 ) ) > 0 )
   {
      /* get parameters */
      const char * pcString1 = zh_parc( 1 );
      const char * pcString2 = zh_parc( 2 );
      char * pcRet1 = NULL, * pcRet2 = NULL;
      int iChange1, iChange2;
      ZH_SIZE sIndex, sCmpLen;

      if( ( iChange1 = ZH_ISBYREF( 1 ) ) != 0 )
      {
         pcRet1 = ( char * ) zh_xgrab( sStrLen1 );
         zh_xmemcpy( pcRet1, pcString1, sStrLen1 );
      }

      if( ( iChange2 = ZH_ISBYREF( 2 ) ) != 0 )
      {
         pcRet2 = ( char * ) zh_xgrab( sStrLen2 );
         zh_xmemcpy( pcRet2, pcString2, sStrLen2 );
      }

      sCmpLen = ( sStrLen1 < sStrLen2 ? sStrLen1 : sStrLen2 );
      for( sIndex = 0; sIndex < sCmpLen; sIndex++ )
      {
         if( iChange1 )
         {
            char cExchange = *( pcString1 + sIndex );
            *( pcRet1 + sIndex ) = *( pcString2 + sIndex );
            if( iChange2 )
               *( pcRet2 + sIndex ) = cExchange;
         }
         else if( iChange2 )
            *( pcRet2 + sIndex ) = *( pcString1 + sIndex );
      }

      /* strings */
      if( iChange1 )
      {
         zh_storclen( pcRet1, sStrLen1, 1 );
         zh_xfree( pcRet1 );
      }

      if( iChange2 )
      {
         zh_storclen( pcRet2, sStrLen2, 2 );
         zh_xfree( pcRet2 );
      }

      zh_retc_null();
   }
   else
   {
      PZH_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_STRSWAP, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else
         zh_retc_null();
   }
}
