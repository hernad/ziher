/*
 * WordToChar() CT3 string function
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

ZH_FUNC( WORDTOCHAR )
{
   int iMultiPass;
   ZH_SIZE sSearchLen, sStrLen, sReplaceLen;

   iMultiPass = ct_getatmupa();

   /* param check */
   if( ( sSearchLen = zh_parclen( 1 ) ) / 2 > 0 &&
       ( sStrLen = zh_parclen( 2 ) ) / 2 > 0 &&
       ( sReplaceLen = zh_parclen( 3 ) ) > 0 )
   {
      /* get parameters */
      const char * pcSearch = zh_parc( 1 );
      const char * pcString = zh_parc( 2 );
      const char * pcReplace = zh_parc( 3 );
      char * pcRet;
      ZH_SIZE sRetIndex, sIndex;
      int iNoReplace;

      pcRet = ( char * ) zh_xgrab( sStrLen );
      sRetIndex = 0;
      sIndex = 0;
      iNoReplace = 0;

      *pcRet = *pcString;       /* copy first char */

      do
      {
         ZH_SIZE sMatchStrLen;
         const char * pc;
         ZH_SIZE sReplIndex;

         *( pcRet + sRetIndex + 1 ) = *( pcString + sIndex + 1 );

         if( ! iNoReplace &&
             ( ( pc = ct_at_exact_forward( pcSearch, sSearchLen,
                                           pcRet + sRetIndex, 2,
                                           &sMatchStrLen ) ) != NULL ) &&
             ( ( ( sReplIndex = ( pc - pcSearch ) ) & 1 ) != 1 ) )
         {
            sReplIndex /= 2;
            if( sReplIndex >= sReplaceLen )
               sReplIndex = sReplaceLen - 1;

            *( pcRet + sRetIndex ) = *( pcReplace + sReplIndex );

            if( ! iMultiPass )
               iNoReplace = 1;  /* just copy next char without searching & replacing */
         }
         else
         {
            iNoReplace = 0;
            sRetIndex++;
         }
         sIndex++;
      }
      while( sIndex < sStrLen - 1 );

      /* return string */
      zh_retclen( pcRet, sRetIndex + 1 );
      zh_xfree( pcRet );
   }
   else
   {
      PZH_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_WORDTOCHAR, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else if( ZH_ISCHAR( 2 ) )
         zh_retclen( zh_parc( 2 ), zh_parclen( 2 ) );
      else
         zh_retc_null();
   }
}
