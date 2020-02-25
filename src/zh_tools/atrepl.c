/*
 * AtRepl() CT3 string function
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

ZH_FUNC( ATREPL )
{
   if( ZH_ISCHAR( 1 ) && ZH_ISCHAR( 2 ) )
   {
      const char * pcStringToMatch = zh_parc( 1 );
      ZH_SIZE nStrToMatchLen = zh_parclen( 1 );
      const char * pcString = zh_parc( 2 );
      ZH_SIZE nStrLen = zh_parclen( 2 );
      int iMultiPass = ct_getatmupa();
      int iAtLike = ct_getatlike();
      char cAtLike = ct_getatlikechar();
      ZH_SIZE nIgnore, nMatchStrLen = 0;
      ZH_SIZE nCounter;
      char * pc;

      const char * pcReplacement;
      ZH_SIZE nReplaceLen;
      int iReplaceMode;
      char * pcRetStr;
      ZH_SIZE nRetStrLen;

      /* eventually ignore some characters */
      nIgnore = zh_parns( 6 );

      if( nIgnore >= nStrLen )
      {
         int iArgErrorMode = ct_getargerrormode();

         if( iArgErrorMode != CT_ARGERR_IGNORE )
            ct_error( ( ZH_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_ATREPL, NULL,
                      ZH_ERR_FUNCNAME, 0, EF_CANDEFAULT, ZH_ERR_ARGS_BASEPARAMS );

         zh_retclen( pcString, nStrLen );
         return;
      }

      /* replacement */
      pcReplacement = zh_parc( 3 );
      nReplaceLen = pcReplacement ? zh_parclen( 3 ) : 0;

      /* replace mode */
      iReplaceMode = zh_parl( 5 );

      /* n-th match or last match ? */
      nCounter = zh_parns( 4 );

      /* little trick: */
      if( iReplaceMode == 0 && nCounter == 0 )
         nCounter = ZH_SIZE_MAX;

      if( nCounter != 0 )
      {
         /* depending on iReplaceMode: replace all occurrences including the nth one
            or only the nth occurrence
            NOTE: if iReplaceMode = false and the nth occurrence does not exist,
            all occurrences are replaced */
         char * pcRetSubStr;
         ZH_SIZE sRetSubStrLen;
         ZH_SIZE nMatchCounter = 0;

         nRetStrLen = nStrLen;
         pcRetStr = ( char * ) zh_xgrab( nRetStrLen + 1 );
         zh_xmemcpy( pcRetStr, pcString, nRetStrLen );

         pcRetSubStr = pcRetStr + nIgnore;
         sRetSubStrLen = nRetStrLen - nIgnore;

         while( nMatchCounter < nCounter )
         {
            switch( iAtLike )
            {
               case CT_SETATLIKE_EXACT:
                  pc = ( char * ) ZH_UNCONST( ct_at_exact_forward( pcRetSubStr, sRetSubStrLen, pcStringToMatch,
                                                                   nStrToMatchLen, &nMatchStrLen ) );
                  break;

               case CT_SETATLIKE_WILDCARD:
                  pc = ( char * ) ZH_UNCONST( ct_at_wildcard_forward( pcRetSubStr, sRetSubStrLen, pcStringToMatch,
                                                                      nStrToMatchLen, cAtLike, &nMatchStrLen ) );
                  break;

               default:
                  pc = NULL;
            }

            if( pc == NULL )
            {
               zh_retclen_buffer( pcRetStr, nRetStrLen );
               return;
            }

            nMatchCounter++;

            /* replace match ? */
            if( iReplaceMode == 0 || nMatchCounter == nCounter )
            {
               if( nMatchStrLen < nReplaceLen )
               {
                  /* pcRetStr grows, so realloc memory */
                  /* save pc pointer */
                  ZH_SIZE sPCPos = pc - pcRetStr;

                  pcRetStr = ( char * ) zh_xrealloc( pcRetStr,
                                 nRetStrLen + ( nReplaceLen - nMatchStrLen ) + 1 );
                  pc = pcRetStr + sPCPos;
               }

               if( nReplaceLen != nMatchStrLen )
                  memmove( pc + nReplaceLen, pc + nMatchStrLen,
                           nRetStrLen - ( ( pc + nMatchStrLen ) - pcRetStr ) );
               if( nReplaceLen > 0 )
                  zh_xmemcpy( pc, pcReplacement, nReplaceLen );

               if( iMultiPass )
                  pcRetSubStr = pc + 1;
               else
                  pcRetSubStr = pc + nReplaceLen;

               nRetStrLen += nReplaceLen - nMatchStrLen;
            }
            else
            {
               if( iMultiPass )
                  pcRetSubStr = pc + 1;
               else
                  pcRetSubStr = pc + nMatchStrLen;
            }
            sRetSubStrLen = nRetStrLen - ( pcRetSubStr - pcRetStr );
         }
      }
      else
      {
         /* find and replace last match */
         nRetStrLen = nStrLen;
         pcRetStr = ( char * ) zh_xgrab( nRetStrLen + 1 );
         zh_xmemcpy( pcRetStr, pcString, nRetStrLen );

         /* we have to find the last match and replace it */
         switch( iAtLike )
         {
            case CT_SETATLIKE_EXACT:
               pc = ( char * ) ZH_UNCONST( ct_at_exact_backward( pcRetStr + nIgnore, nRetStrLen - nIgnore,
                                                                 pcStringToMatch, nStrToMatchLen, &nMatchStrLen ) );
               break;

            case CT_SETATLIKE_WILDCARD:
               pc = ( char * ) ZH_UNCONST( ct_at_wildcard_backward( pcRetStr + nIgnore, nRetStrLen - nIgnore,
                                                                    pcStringToMatch, nStrToMatchLen,
                                                                    cAtLike, &nMatchStrLen ) );
               break;

            default:
               pc = NULL;
         }

         if( pc == NULL )
         {
            zh_retclen_buffer( pcRetStr, nRetStrLen );
            return;
         }

         /* replace match */
         if( nMatchStrLen < nReplaceLen )
         {
            /* pcRetStr grows, so realloc memory */
            /* save pc pointer */
            ZH_SIZE sPCPos = pc - pcRetStr;

            pcRetStr = ( char * ) zh_xrealloc( pcRetStr,
                                 nRetStrLen + ( nReplaceLen - nMatchStrLen ) + 1 );
            pc = pcRetStr + sPCPos;
         }

         if( nReplaceLen != nMatchStrLen )
            memmove( pc + nReplaceLen, pc + nMatchStrLen,
                     nRetStrLen - ( ( pc + nMatchStrLen ) - pcRetStr ) );
         if( nReplaceLen > 0 )
            zh_xmemcpy( pc, pcReplacement, nReplaceLen );

         nRetStrLen += ( nReplaceLen - nMatchStrLen );
      }

      zh_retclen_buffer( pcRetStr, nRetStrLen );
   }
   else
   {
      PZH_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_ATREPL, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else
         zh_retclen( zh_parc( 2 ), zh_parclen( 2 ) );
   }
}
