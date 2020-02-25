/*
 * PosDiff() and PosEqual() CT3 string functions
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

ZH_FUNC( POSDIFF )
{
   if( ZH_ISCHAR( 1 ) && ZH_ISCHAR( 2 ) )
   {
      const char * pcString1 = zh_parc( 1 );
      ZH_SIZE sStrLen1 = zh_parclen( 1 );
      const char * pcString2 = zh_parc( 2 );
      ZH_SIZE sStrLen2 = zh_parclen( 2 );
      const char * pc1, * pc2;
      ZH_SIZE sIgnore = zh_parnsdef( 3, 0 );

      if( sIgnore > sStrLen1 || sIgnore > sStrLen2 )
      {
         int iArgErrorMode = ct_getargerrormode();

         if( iArgErrorMode != CT_ARGERR_IGNORE )
            ct_error( ( ZH_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_POSDIFF, NULL,
                      ZH_ERR_FUNCNAME, 0, EF_CANDEFAULT, ZH_ERR_ARGS_BASEPARAMS );

         zh_retns( 0 );
         return;
      }

      pc1 = pcString1 + sIgnore;
      pc2 = pcString2 + sIgnore;

      while( pc1 < ( pcString1 + sStrLen1 ) && pc2 < ( pcString2 + sStrLen2 ) )
      {
         if( *pc1 != *pc2 )
         {
            zh_retns( ( pc1 - pcString1 ) + 1 );
            return;
         }
         pc1++;
         pc2++;
      }

      if( sStrLen1 != sStrLen2 )
         zh_retns( ( sStrLen1 < sStrLen2 ? sStrLen1 : sStrLen2 ) + 1 );
      else
         zh_retns( 0 );
   }
   else
   {
      PZH_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_POSDIFF, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else if( ZH_ISCHAR( 1 ) || ZH_ISCHAR( 2 ) )
         zh_retns( 1 );
      else
         zh_retns( 0 );
   }
}

ZH_FUNC( POSEQUAL )
{
   if( ZH_ISCHAR( 1 ) && ZH_ISCHAR( 2 ) )
   {
      const char * pcString1 = zh_parc( 1 );
      ZH_SIZE sStrLen1 = zh_parclen( 1 );
      const char * pcString2 = zh_parc( 2 );
      ZH_SIZE sStrLen2 = zh_parclen( 2 );
      const char * pc1, * pc2;
      ZH_SIZE sIgnore = zh_parnsdef( 4, 0 );
      ZH_SIZE sCompare, sCompareCnt, sRet = 0;

      if( ZH_IS_PARAM_NUM( 3 ) )
         sCompare = zh_parns( 3 );
      else
         sCompare = ( sStrLen1 < sStrLen2 ? sStrLen1 : sStrLen2 ) - sIgnore;

      if( sCompare == 0 || sIgnore > sStrLen1 || sIgnore > sStrLen2 )
      {
         int iArgErrorMode = ct_getargerrormode();

         if( iArgErrorMode != CT_ARGERR_IGNORE )
            ct_error( ( ZH_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_POSEQUAL, NULL,
                      ZH_ERR_FUNCNAME, 0, EF_CANDEFAULT, ZH_ERR_ARGS_BASEPARAMS );

         zh_retns( 0 );
         return;
      }

      if( sStrLen1 < ( sCompare + sIgnore ) || sStrLen2 < ( sCompare + sIgnore ) )
      {
         int iArgErrorMode = ct_getargerrormode();

         if( iArgErrorMode != CT_ARGERR_IGNORE )
            ct_error( ( ZH_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_POSEQUAL, NULL,
                      ZH_ERR_FUNCNAME, 0, EF_CANDEFAULT, ZH_ERR_ARGS_BASEPARAMS );

         zh_retns( 0 );
         return;
      }

      pc1 = pcString1 + sIgnore;
      pc2 = pcString2 + sIgnore;
      sCompareCnt = 0;

      while( pc1 < pcString1 + sStrLen1 )
      {
         if( *pc1 == *pc2 )
         {
            /* save possible return value */
            if( sCompareCnt == 0 )
               sRet = pc1 - pcString1 + 1;

            sCompareCnt++;
            if( sCompareCnt == sCompare )
            {
               zh_retns( sRet );
               return;
            }
         }
         else
         {
            /* reset compare counter */
            sCompareCnt = 0;
         }
         pc1++;
         pc2++;
      }
      zh_retns( 0 );
   }
   else
   {
      PZH_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_POSEQUAL, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else
         zh_retns( 0 );
   }
}
