/*
 * CharRelA() and CharRelRep() CT3 string functions
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

ZH_FUNC( CHARRELA )
{
   if( ZH_ISCHAR( 1 ) && ZH_ISCHAR( 2 ) && ZH_ISCHAR( 3 ) && ZH_ISCHAR( 4 ) )
   {
      const char * pcStringToMatch1 = zh_parc( 1 );
      ZH_SIZE sStrToMatchLen1 = zh_parclen( 1 );
      const char * pcString1 = zh_parc( 2 );
      ZH_SIZE sStrLen1 = zh_parclen( 2 );
      const char * pcStringToMatch2 = zh_parc( 3 );
      ZH_SIZE sStrToMatchLen2 = zh_parclen( 3 );
      const char * pcString2 = zh_parc( 4 );
      ZH_SIZE sStrLen2 = zh_parclen( 4 );

      const char * pc1, * pc2;
      ZH_SIZE sOffset1, sOffset2;
      ZH_SIZE sMatchStrLen;

      /* check for empty strings */
      if( sStrToMatchLen1 == 0 || sStrToMatchLen2 == 0 )
      {
         zh_retns( 0 );
         return;
      }

      sOffset1 = 0;
      sOffset2 = 0;

      /* NOTE: this algorithm is not the best since the search that gave
         the larger relative position in the step before is repeated;
         try a search algorithm alternating between both strings */
      while( sOffset1 < sStrLen1 && sOffset2 < sStrLen2 )
      {
         pc1 = ct_at_exact_forward( pcStringToMatch1, sStrToMatchLen1,
                                    pcString1 + sOffset1, sStrLen1 - sOffset1, &sMatchStrLen );
         pc2 = ct_at_exact_forward( pcStringToMatch2, sStrToMatchLen2,
                                    pcString2 + sOffset2, sStrLen2 - sOffset2, &sMatchStrLen );
         if( pc1 != NULL && pc2 != NULL )
         {
            if( pc1 - pcString1 == pc2 - pcString2 )
            {
               /* correlation found */
               zh_retns( ( pc1 - pcString1 ) + 1 );
               return;
            }
            else
            {
               if( pc1 - pcString1 > pc2 - pcString2 )
                  sOffset1 = sOffset2 = pc1 - pcString1;
               else
                  sOffset1 = sOffset2 = pc2 - pcString2;
            }
         }
         else
            sOffset1 = sOffset2 = sStrLen1 < sStrLen2 ? sStrLen1 : sStrLen2;
      }

      zh_retns( 0 );
   }
   else
   {
      PZH_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_CHARRELA, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else
         zh_retns( 0 );
   }
}

ZH_FUNC( CHARRELREP )
{
   int iNoRet = ct_getref() && ZH_ISBYREF( 4 );

   if( ZH_ISCHAR( 1 ) && ZH_ISCHAR( 2 ) && ZH_ISCHAR( 3 ) &&
       ZH_ISCHAR( 4 ) && ZH_ISCHAR( 5 ) )
   {
      const char * pcStringToMatch1 = zh_parc( 1 );
      ZH_SIZE sStrToMatchLen1 = zh_parclen( 1 );
      const char * pcString1 = zh_parc( 2 );
      ZH_SIZE sStrLen1 = zh_parclen( 2 );
      const char * pcStringToMatch2 = zh_parc( 3 );
      ZH_SIZE sStrToMatchLen2 = zh_parclen( 3 );
      const char * pcString2 = zh_parc( 4 );
      ZH_SIZE sStrLen2 = zh_parclen( 4 );
      const char * pcReplace = zh_parc( 5 );
      ZH_SIZE sReplaceLen = zh_parclen( 5 );
      char * pcRet;
      const char * pc1, * pc2;
      ZH_SIZE sOffset1, sOffset2;
      ZH_SIZE sMatchStrLen;

      /* check for empty strings */
      if( sStrToMatchLen1 == 0 ||
          sStrToMatchLen2 == 0 || sReplaceLen == 0 || sStrLen2 == 0 )
      {
         if( iNoRet )
            zh_ret();
         else
            zh_retclen( pcString2, sStrLen2 );
         return;
      }

      pcRet = ( char * ) zh_xgrab( sStrLen2 + 1 );
      zh_xmemcpy( pcRet, pcString2, sStrLen2 );

      sOffset1 = 0;
      sOffset2 = 0;

      /* NOTE: this algorithm is not the best since the search that gave
         the larger relative position in the step before is repeated;
         try a search algorithm alternating between both strings */
      while( sOffset1 < sStrLen1 && sOffset2 < sStrLen2 )
      {
         pc1 = ct_at_exact_forward( pcStringToMatch1, sStrToMatchLen1,
                                    pcString1 + sOffset1, sStrLen1 - sOffset1,
                                    &sMatchStrLen );
         pc2 = ct_at_exact_forward( pcStringToMatch2, sStrToMatchLen2,
                                    pcString2 + sOffset2, sStrLen2 - sOffset2,
                                    &sMatchStrLen );
         if( pc1 != NULL && pc2 != NULL )
         {
            if( pc1 - pcString1 == pc2 - pcString2 )
            {
               /* correlation found -> start replacement */
               ZH_SIZE sCurr;

               for( sCurr = 1; sCurr <= sStrToMatchLen1; sCurr++ )
               {
                  /* check if pcString2 is long enough */
                  if( ( pc2 - pcString2 ) + sCurr >= sStrLen2 )
                  {
                     ZH_SIZE sStr2Offset, sReplOffset;

                     sStr2Offset = sStrToMatchLen2 < sCurr ? sStrToMatchLen2 : sCurr;
                     sReplOffset = sReplaceLen < sCurr ? sReplaceLen : sCurr;

                     /* do the characters in pcString2 and pcStrToMatch2 match ? */
                     if( *( pc2 + sCurr - 1 ) ==
                         *( pcStringToMatch2 + sStr2Offset - 1 ) )
                     {
                        *( pcRet + ( pc2 - pcString2 ) + sCurr - 1 ) =
                           *( pcReplace + sReplOffset - 1 );
                     }
                  }
               }
               sOffset1 = sOffset2 = ( pc1 - pcString1 ) + 1;
            }
            else
            {
               if( pc1 - pcString1 > pc2 - pcString2 )
                  sOffset1 = sOffset2 = pc1 - pcString1;
               else
                  sOffset1 = sOffset2 = pc2 - pcString2;
            }
         }
         else
            sOffset1 = sOffset2 = sStrLen1 < sStrLen2 ? sStrLen1 : sStrLen2;
      }

      zh_storclen( pcRet, sStrLen2, 4 );

      if( iNoRet )
      {
         zh_xfree( pcRet );
         zh_ret();
      }
      else
         zh_retclen_buffer( pcRet, sStrLen2 );
   }
   else
   {
      PZH_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_CHARRELREP, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else if( iNoRet )
         zh_ret();
      else
         zh_retc_null();
   }
}
