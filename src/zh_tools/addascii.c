/*
 * AddAscii() CT3 string function
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

ZH_FUNC( ADDASCII )
{
   /* suppressing return value ? */
   int iNoRet = ct_getref() && ZH_ISBYREF( 1 );

   if( ZH_ISCHAR( 1 ) )
   {
      const char * pcSource = zh_parc( 1 );
      ZH_SIZE sLen = zh_parclen( 1 );
      char * pcResult;
      ZH_SIZE sPos = zh_parnsdef( 3, sLen );
      ZH_LONG lValue;
      int iCarryOver;

      if( sPos > sLen || ! ZH_IS_PARAM_NUM( 2 ) || sLen == 0 )
      {
         int iArgErrorMode = ct_getargerrormode();

         if( iArgErrorMode != CT_ARGERR_IGNORE )
            ct_error( ( ZH_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_ADDASCII, NULL,
                      ZH_ERR_FUNCNAME, 0, EF_CANDEFAULT, ZH_ERR_ARGS_BASEPARAMS );

         /* return string unchanged */
         if( iNoRet )
            zh_retl( ZH_FALSE );
         else
            zh_retclen( pcSource, sLen );

         return;
      }

      pcResult = ( char * ) zh_xgrab( sLen + 1 );
      zh_xmemcpy( pcResult, pcSource, sLen );

      lValue = zh_parnl( 2 );
      iCarryOver = zh_parl( 4 );

      if( iCarryOver )
      {
         ZH_SIZE sCurrent;

         for( sCurrent = sPos; sCurrent > 0 && lValue != 0; sCurrent-- )
         {
            ZH_LONG lResult = ( ZH_LONG ) pcSource[ sCurrent - 1 ] + ( lValue % 256 );

            lValue /= 256;
            if( lResult > 255 )
               lValue++;
            else if( lResult < 0 )
               lValue--;

            pcResult[ sCurrent - 1 ] = ( char ) ( lResult % 256 );
         }
      }
      else
         pcResult[ sPos - 1 ] = ( char ) ( ( ( ZH_LONG ) pcResult[ sPos - 1 ] + lValue ) % 256 );

      zh_storclen( pcResult, sLen, 1 );

      if( iNoRet )
      {
         zh_retl( ZH_FALSE );
         zh_xfree( pcResult );
      }
      else
         zh_retclen_buffer( pcResult, sLen );
   }
   else
   {
      PZH_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_ADDASCII,
                                  NULL, ZH_ERR_FUNCNAME, 0, EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else if( iNoRet )
         zh_retl( ZH_FALSE );
      else
         zh_retc_null();
   }
}
