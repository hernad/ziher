/*
 * CT3 string functions
 *   - AfterAtNum()
 *   - BeforAtNum()
 *   - AtNum()
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

#define DO_ATNUM_AFTERATNUM  0
#define DO_ATNUM_BEFORATNUM  1
#define DO_ATNUM_ATNUM       2

/* helper function */
static void do_atnum( int iSwitch )
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
      ZH_SIZE sIgnore = zh_parnsdef( 4, 0 ); /* eventually ignore some characters */
      ZH_SIZE sMatchStrLen = 0;
      ZH_SIZE nCounter;
      const char * pc = NULL;

      if( sIgnore >= sStrLen )
      {
         switch( iSwitch )
         {
            case DO_ATNUM_AFTERATNUM:
            {
               /* AFTERATNUM */
               int iArgErrorMode = ct_getargerrormode();

               if( iArgErrorMode != CT_ARGERR_IGNORE )
                  ct_error( ( ZH_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_AFTERATNUM, NULL,
                            ZH_ERR_FUNCNAME, 0, EF_CANDEFAULT, ZH_ERR_ARGS_BASEPARAMS );

               zh_retc_null();
               break;
            }
            case DO_ATNUM_BEFORATNUM:
            {
               /* BEFORATNUM */
               int iArgErrorMode = ct_getargerrormode();

               if( iArgErrorMode != CT_ARGERR_IGNORE )
                  ct_error( ( ZH_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_BEFORATNUM, NULL,
                            ZH_ERR_FUNCNAME, 0, EF_CANDEFAULT, ZH_ERR_ARGS_BASEPARAMS );

               zh_retc_null();
               break;
            }
            case DO_ATNUM_ATNUM:
            {
               /* ATNUM */
               int iArgErrorMode = ct_getargerrormode();

               if( iArgErrorMode != CT_ARGERR_IGNORE )
                  ct_error( ( ZH_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_ATNUM, NULL, ZH_ERR_FUNCNAME, 0,
                            EF_CANDEFAULT, ZH_ERR_ARGS_BASEPARAMS );

               zh_retns( 0 );
               break;
            }
         }
         return;
      }
      else
      {
         pcString += sIgnore;
         sStrLen -= sIgnore;
      }

      /* nth match or last match ? */
      if( ZH_IS_PARAM_NUM( 3 ) && ( nCounter = zh_parns( 3 ) ) != 0 )
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
                  there are no <nCounter> matches, so return an empty string */
               switch( iSwitch )
               {
                  case DO_ATNUM_AFTERATNUM:
                  case DO_ATNUM_BEFORATNUM:
                     /* AFTERATNUM */
                     /* BEFORATNUM */
                     zh_retc_null();
                     break;

                  case DO_ATNUM_ATNUM:
                     /* ATNUM */
                     zh_retns( 0 );
                     break;
               }
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
         /* we have to find the last match and return the
            string after that last match */
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
            switch( iSwitch )
            {
               case DO_ATNUM_AFTERATNUM:
               case DO_ATNUM_BEFORATNUM:
                  /* AFTERATNUM */
                  /* BEFORATNUM */
                  zh_retc_null();
                  break;

               case DO_ATNUM_ATNUM:
                  /* ATNUM */
                  zh_retns( 0 );
                  break;
            }
            return;
         }
      }

      switch( iSwitch )
      {
         case DO_ATNUM_AFTERATNUM:
            /* AFTERATNUM */
            if( pc + sMatchStrLen >= pcString + sStrLen )
               zh_retc_null();
            else
               zh_retclen( pc + sMatchStrLen, sStrLen - ( pc + sMatchStrLen - pcString ) );
            break;

         case DO_ATNUM_BEFORATNUM:
            /* BEFORATNUM */
            zh_retclen( pcString - sIgnore, pc - ( pcString - sIgnore ) );
            break;

         case DO_ATNUM_ATNUM:
            /* ATNUM */
#if defined( __POCC__ ) && ( __POCC__ >= 500 ) && defined( ZH_OS_WIN_64 )
            /* NOTE: Workaround for Pelles C 5.00.13 AMD64 mode internal error:
                     'fatal error: Internal error: reduce_tree()' [vszakats]. */
            zh_retns( pc - pcString + sIgnore + 1 );
#else
            zh_retns( pc - ( pcString - sIgnore ) + 1 );
#endif
            break;
      }
   }
   else
   {
      switch( iSwitch )
      {
         case DO_ATNUM_AFTERATNUM:
         case DO_ATNUM_BEFORATNUM:
         {
            /* AFTERATNUM */
            PZH_ITEM pSubst = NULL;
            int iArgErrorMode = ct_getargerrormode();

            if( iArgErrorMode != CT_ARGERR_IGNORE )
               pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                        iSwitch ==
                                        DO_ATNUM_AFTERATNUM ? CT_ERROR_AFTERATNUM :
                                        CT_ERROR_BEFORATNUM, NULL, ZH_ERR_FUNCNAME, 0,
                                        EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

            if( pSubst != NULL )
               zh_itemReturnRelease( pSubst );
            else
               zh_retc_null();
            break;
         }
         case DO_ATNUM_ATNUM:
         {
            /* ATNUM */
            PZH_ITEM pSubst = NULL;
            int iArgErrorMode = ct_getargerrormode();

            if( iArgErrorMode != CT_ARGERR_IGNORE )
               pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_ATNUM,
                                        NULL, ZH_ERR_FUNCNAME, 0, EF_CANSUBSTITUTE,
                                        ZH_ERR_ARGS_BASEPARAMS );

            if( pSubst != NULL )
               zh_itemReturnRelease( pSubst );
            else
               zh_retns( 0 );
            break;
         }
      }
   }
}

ZH_FUNC( AFTERATNUM )
{
   do_atnum( DO_ATNUM_AFTERATNUM );
}

ZH_FUNC( BEFORATNUM )
{
   do_atnum( DO_ATNUM_BEFORATNUM );
}

ZH_FUNC( ATNUM )
{
   do_atnum( DO_ATNUM_ATNUM );
}
