/*
 * NumAt() CT3 string function
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

ZH_FUNC( NUMAT )
{
   if( ZH_ISCHAR( 1 ) && ZH_ISCHAR( 2 ) )
   {
      const char * pcStringToMatch = zh_parc( 1 );
      ZH_SIZE sStrToMatchLen = zh_parclen( 1 );
      const char * pcString = zh_parc( 2 );
      ZH_SIZE sStrLen = zh_parclen( 2 );
      int iMultiPass = ct_getatmupa();
      int iAtLike = ct_getatlike();
      char cAtLike = ct_getatlikechar();
      ZH_SIZE sIgnore, sMatchStrLen = 0, sSubStrLen;
      ZH_SIZE nCounter;
      const char * pc, * pcSubStr;

      /* eventually ignore some characters */
      sIgnore = zh_parnsdef( 3, 0 );

      if( sIgnore >= sStrLen )
      {
         int iArgErrorMode = ct_getargerrormode();

         if( iArgErrorMode != CT_ARGERR_IGNORE )
            ct_error( ( ZH_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_NUMAT, NULL,
                      ZH_ERR_FUNCNAME, 0, EF_CANDEFAULT, ZH_ERR_ARGS_BASEPARAMS );

         zh_retni( 0 );
         return;
      }
      else
      {
         pcString += sIgnore;
         sStrLen  -= sIgnore;
      }

      nCounter = 0;
      pcSubStr = pcString;
      sSubStrLen = sStrLen;

      do
      {
         switch( iAtLike )
         {
            case CT_SETATLIKE_EXACT:
               pc = ct_at_exact_forward( pcSubStr, sSubStrLen, pcStringToMatch,
                                         sStrToMatchLen, &sMatchStrLen );
               break;

            case CT_SETATLIKE_WILDCARD:
               pc = ct_at_wildcard_forward( pcSubStr, sSubStrLen,
                                            pcStringToMatch, sStrToMatchLen,
                                            cAtLike, &sMatchStrLen );
               break;

            default:
               pc = NULL;
         }
         nCounter++;
         if( iMultiPass )
            pcSubStr = pc + 1;
         else
            pcSubStr = pc + sMatchStrLen;
         sSubStrLen = sStrLen - ( pcSubStr - pcString );
      }
      while( pc != NULL );

      zh_retns( nCounter - 1 );
   }
   else
   {
      PZH_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_NUMAT, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else
         zh_retni( 0 );
   }
}
