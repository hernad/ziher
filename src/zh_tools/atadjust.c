/*
 * AtAdjust() CT3 string function
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

ZH_FUNC( ATADJUST )
{
   if( ZH_ISCHAR( 1 ) && ZH_ISCHAR( 2 ) && ZH_IS_PARAM_NUM( 3 ) )
   {
      const char * pcStringToMatch = zh_parc( 1 );
      ZH_SIZE sStrToMatchLen = zh_parclen( 1 );
      const char * pcString = zh_parc( 2 );
      ZH_SIZE sStrLen = zh_parclen( 2 );
      ZH_SIZE sAdjustPosition = zh_parns( 3 );

      int iMultiPass = ct_getatmupa();
      int iAtLike = ct_getatlike();
      char cAtLike = ct_getatlikechar();
      ZH_SIZE sIgnore, sMatchStrLen = 0;
      ZH_SIZE nCounter;
      const char * pc = NULL;

      char cFillChar;
      const char * pcCheckFill;
      char * pcRetStr;
      ZH_SIZE sRetStrLen;

      /* eventually ignore some characters */
      sIgnore = zh_parnsdef( 5, 0 );

      if( sIgnore >= sStrLen )
      {
         int iArgErrorMode = ct_getargerrormode();

         if( iArgErrorMode != CT_ARGERR_IGNORE )
            ct_error( ( ZH_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_ATADJUST, NULL, ZH_ERR_FUNCNAME, 0,
                      EF_CANDEFAULT, ZH_ERR_ARGS_BASEPARAMS );

         zh_retclen( pcString, sStrLen );
         return;
      }
      else
      {
         pcString += sIgnore;
         sStrLen -= sIgnore;
      }

      /* check for wrong adjust position */
      if( sAdjustPosition == 0 )
      {
         int iArgErrorMode = ct_getargerrormode();

         if( iArgErrorMode != CT_ARGERR_IGNORE )
            ct_error( ( ZH_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_ATADJUST, NULL, ZH_ERR_FUNCNAME, 0,
                      EF_CANDEFAULT, ZH_ERR_ARGS_BASEPARAMS );

         zh_retclen( pcString, sStrLen );
         return;
      }
      else
         sAdjustPosition--;     /* makes live easier since C indices start at zero ! */

      /* nth match or last match ? */
      if( ZH_IS_PARAM_NUM( 4 ) && ( nCounter = zh_parns( 4 ) ) != 0 )
      {
         /* find the <nCounter>th match */
         const char * pcSubStr;
         ZH_SIZE sSubStrLen;
         ZH_SIZE nMatchCounter = 0;

         pcSubStr = pcString;
         sSubStrLen = sStrLen;

         while( nMatchCounter < nCounter )
         {
            switch( iAtLike )
            {
               case CT_SETATLIKE_EXACT:
                  pc = ct_at_exact_forward( pcSubStr, sSubStrLen, pcStringToMatch,
                                            sStrToMatchLen, &sMatchStrLen );
                  break;

               case CT_SETATLIKE_WILDCARD:
                  pc = ct_at_wildcard_forward( pcSubStr, sSubStrLen, pcStringToMatch,
                                               sStrToMatchLen, cAtLike, &sMatchStrLen );
                  break;

               default:
                  pc = NULL;
            }

            if( pc == NULL )
            {
               /* no match found; if this happens at this point,
                  there are no <nCounter> matches, so return */
               zh_retclen( pcString, sStrLen );
               return;
            }

            nMatchCounter++;
            if( iMultiPass )
               pcSubStr = pc + 1;
            else
               pcSubStr = pc + sMatchStrLen;
            sSubStrLen = sStrLen - ( pcSubStr - pcString );
         }
      }
      else
      {
         /* we have to find the last match */
         switch( iAtLike )
         {
            case CT_SETATLIKE_EXACT:
               pc = ct_at_exact_backward( pcString, sStrLen, pcStringToMatch,
                                          sStrToMatchLen, &sMatchStrLen );
               break;

            case CT_SETATLIKE_WILDCARD:
               pc = ct_at_wildcard_backward( pcString, sStrLen, pcStringToMatch,
                                             sStrToMatchLen, cAtLike, &sMatchStrLen );
               break;

            default:
               pc = NULL;
         }

         if( pc == NULL )
         {
            /* no matches found */
            zh_retclen( pcString, sStrLen );
            return;
         }
      }

      /* adjust string */
      if( ZH_ISCHAR( 6 ) )
      {
         if( zh_parclen( 6 ) > 0 )
            cFillChar = *( zh_parc( 6 ) );
         else
            cFillChar = 0x20;
      }
      else if( ZH_IS_PARAM_NUM( 6 ) )
         cFillChar = ( char ) ( zh_parnl( 6 ) % 256 );
      else
         cFillChar = 0x20;

      /* position of pc == adjust position ? */
      if( pc == pcString + sAdjustPosition )
      {
         /* do nothing */
         zh_retclen( pcString, sStrLen );
      }
      else
      {
         if( pc > pcString + sAdjustPosition )
         {
            /* adjust to left */
            /* check if we only delete cFillChar characters */
            for( pcCheckFill = pcString + sAdjustPosition; pcCheckFill < pc; pcCheckFill++ )
               if( *pcCheckFill != cFillChar )
               {
                  /* no -> return string unchanged */
                  zh_retclen( pcString, sStrLen );
                  return;
               }

            /* ok -> calculate new string size */
            sRetStrLen = sStrLen - ( pc - ( pcString + sAdjustPosition ) );
            pcRetStr = ( char * ) zh_xgrab( sRetStrLen + 1 );

            /* copy first portion of string */
            if( sAdjustPosition > 0 )
               zh_xmemcpy( pcRetStr, pcString, sAdjustPosition );

            /* copy second portion of string */
            if( sRetStrLen > sAdjustPosition )
               zh_xmemcpy( pcRetStr + sAdjustPosition, pc, sRetStrLen - sAdjustPosition );

            zh_retclen_buffer( pcRetStr, sRetStrLen );
         }
         else
         {
            ZH_SIZE nLen;

            /* adjust to right */
            sRetStrLen = sStrLen + ( pcString + sAdjustPosition ) - pc;
            pcRetStr = ( char * ) zh_xgrab( sRetStrLen + 1 );

            /* copy first portion of string */
            if( pc > pcString )
               zh_xmemcpy( pcRetStr, pcString, pc - pcString );

            /* fill characters */
            nLen = sAdjustPosition - ( pc - pcString );
            if( nLen > 0 )
               zh_xmemset( pcRetStr + ( pc - pcString ), cFillChar, nLen );

            /* copy second portion of string */
            if( sRetStrLen > sAdjustPosition )
               zh_xmemcpy( pcRetStr + sAdjustPosition, pc, sRetStrLen - sAdjustPosition );

            zh_retclen_buffer( pcRetStr, sRetStrLen );
         }
      }
   }
   else
   {
      PZH_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_ATADJUST, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else if( ZH_ISCHAR( 2 ) )
         zh_retclen( zh_parc( 2 ), zh_parclen( 2 ) );
      else
         zh_retc_null();
   }
}
