/*
 * PosChar(), PosDel(), PosIns() and PosRepl() CT3 functions
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

ZH_FUNC( POSCHAR )
{
   int iNoRet = ct_getref() && ZH_ISBYREF( 1 );

   if( zh_parclen( 1 ) > 0 )
   {
      if( zh_parclen( 2 ) > 0 || ZH_IS_PARAM_NUM( 2 ) )
      {
         const char * pcString = zh_parc( 1 );
         ZH_SIZE sStrLen = zh_parclen( 1 );
         char * pcRet;
         char cReplace;
         ZH_SIZE sPosition;

         if( ZH_ISCHAR( 2 ) )
            cReplace = *( zh_parc( 2 ) );
         else
            cReplace = ( char ) ( zh_parns( 2 ) % 256 );

         if( ZH_IS_PARAM_NUM( 3 ) )
         {
            sPosition = zh_parns( 3 );
            if( sPosition == 0 )
               sPosition = sStrLen;
         }
         else
            sPosition = sStrLen;

         pcRet = ( char * ) zh_xgrab( sStrLen + 1 );
         zh_xmemcpy( pcRet, pcString, sStrLen );
         *( pcRet + sPosition - 1 ) = cReplace;

         zh_storclen( pcRet, sStrLen, 1 );

         if( iNoRet )
         {
            zh_ret();
            zh_xfree( pcRet );
         }
         else
            zh_retclen_buffer( pcRet, sStrLen );
      }
      else
      {
         PZH_ITEM pSubst = NULL;
         int iArgErrorMode = ct_getargerrormode();

         if( iArgErrorMode != CT_ARGERR_IGNORE )
            pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                     CT_ERROR_POSCHAR, NULL, ZH_ERR_FUNCNAME, 0,
                                     EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

         if( pSubst != NULL )
            zh_itemReturnRelease( pSubst );
         else if( iNoRet )
            zh_ret();
         else
            zh_retclen( zh_parc( 1 ), zh_parclen( 1 ) );
      }
   }
   else
   {
      PZH_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_POSCHAR, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else if( iNoRet )
         zh_ret();
      else
         zh_retc_null();
   }
}

ZH_FUNC( POSDEL )
{
   if( ZH_ISCHAR( 1 ) )
   {
      const char * pcString = zh_parc( 1 );
      ZH_SIZE sStrLen = zh_parclen( 1 );
      ZH_SIZE sStartPos;
      ZH_SIZE sDelLen = zh_parnsdef( 3, 1 );
      char * pcRet;

      if( ZH_IS_PARAM_NUM( 2 ) )
      {
         sStartPos = zh_parns( 2 );
         if( sStartPos == 0 || sStartPos > sStrLen - sDelLen + 1 )
            sStartPos = sStrLen - sDelLen + 1;
      }
      else
         sStartPos = sStrLen - sDelLen + 1;

      if( sStrLen <= sDelLen )
      {
         zh_retc_null();
         return;
      }

      pcRet = ( char * ) zh_xgrab( sStrLen - sDelLen + 1 );

      /* copy first part */
      if( sStartPos > 1 )
         zh_xmemcpy( pcRet, pcString, sStartPos - 1 );

      /* copy second part */
      if( sStrLen > ( sStartPos - 1 + sDelLen ) )
         zh_xmemcpy( pcRet + sStartPos - 1, pcString + sStartPos - 1 + sDelLen,
                     sStrLen - ( sStartPos - 1 + sDelLen ) );

      zh_retclen_buffer( pcRet, sStrLen - sDelLen );
   }
   else
   {
      PZH_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_POSDEL, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else
         zh_retc_null();
   }
}

ZH_FUNC( POSINS )
{
   if( ZH_ISCHAR( 1 ) )
   {
      const char * pcString = zh_parc( 1 );
      ZH_SIZE sStrLen = zh_parclen( 1 );
      ZH_SIZE sInsLen;

      if( ( sInsLen = zh_parclen( 2 ) ) > 0 )
      {
         ZH_SIZE sStartPos;
         char * pcRet;

         const char * pcInsert = zh_parc( 2 );

         if( ZH_IS_PARAM_NUM( 3 ) )
         {
            sStartPos = zh_parns( 3 );
            if( sStartPos == 0 )
               sStartPos = sStrLen;
         }
         else
            sStartPos = sStrLen;

         /* check for false sStartPos */
         if( sStartPos > sStrLen + 1 )
         {
            int iArgErrorMode = ct_getargerrormode();

            if( iArgErrorMode != CT_ARGERR_IGNORE )
               ct_error( ( ZH_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_POSINS,
                         NULL, ZH_ERR_FUNCNAME, 0, EF_CANDEFAULT,
                         ZH_ERR_ARGS_BASEPARAMS );

            zh_retclen( pcString, sStrLen );
            return;
         }

         pcRet = ( char * ) zh_xgrab( sStrLen + sInsLen + 1 );

         /* copy first part */
         if( sStartPos > 1 )
            zh_xmemcpy( pcRet, pcString, sStartPos - 1 );

         /* insert string */
         zh_xmemcpy( pcRet + sStartPos - 1, pcInsert, sInsLen );

         /* copy second part */
         if( sStrLen > ( sStartPos - 1 ) )
            zh_xmemcpy( pcRet + sStartPos - 1 + sInsLen, pcString + sStartPos - 1,
                        sStrLen - ( sStartPos - 1 ) );

         zh_retclen_buffer( pcRet, sStrLen + sInsLen );
      }
      else
         zh_retclen( pcString, sStrLen );
   }
   else
   {
      PZH_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_POSINS, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else
         zh_retc_null();
   }
}

ZH_FUNC( POSREPL )
{
   int iNoRet = ct_getref() && ZH_ISBYREF( 1 );

   if( ZH_ISCHAR( 1 ) )
   {
      const char * pcString = zh_parc( 1 );
      ZH_SIZE sStrLen = zh_parclen( 1 );
      ZH_SIZE sReplLen;

      if( ( sReplLen = zh_parclen( 2 ) ) > 0 )
      {
         ZH_SIZE sStartPos;
         char * pcRet;
         ZH_SIZE sRetLen;

         const char * pcReplace = zh_parc( 2 );

         if( ZH_IS_PARAM_NUM( 3 ) )
         {
            sStartPos = zh_parns( 3 );
            if( sStartPos == 0 )
            {
               if( sReplLen > sStrLen )
                  sStartPos = 1;
               else
                  sStartPos = sStrLen - sReplLen + 1;
            }
         }
         else
         {
            if( sReplLen > sStrLen )
               sStartPos = 1;
            else
               sStartPos = sStrLen - sReplLen + 1;
         }

         /* check for false sStartPos */
         if( sStartPos > sStrLen + 1 )
         {
            int iArgErrorMode = ct_getargerrormode();

            if( iArgErrorMode != CT_ARGERR_IGNORE )
               ct_error( ( ZH_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_POSREPL,
                         NULL, ZH_ERR_FUNCNAME, 0, EF_CANDEFAULT,
                         ZH_ERR_ARGS_BASEPARAMS );

            if( iNoRet )
               zh_ret();
            else
               zh_retclen( pcString, sStrLen );
            return;
         }

         if( sStrLen > ( sStartPos + sReplLen - 1 ) )
            sRetLen = sStrLen;
         else
            sRetLen = sStartPos + sReplLen - 1;

         pcRet = ( char * ) zh_xgrab( sRetLen + 1 );

         /* copy first part */
         if( sStartPos > 1 )
            zh_xmemcpy( pcRet, pcString, sStartPos - 1 );

         /* insert replacement string */
         zh_xmemcpy( pcRet + sStartPos - 1, pcReplace, sReplLen );

         /* copy second part */
         if( sStrLen > ( sStartPos - 1 + sReplLen ) )
            zh_xmemcpy( pcRet + sStartPos - 1 + sReplLen, pcString + sStartPos - 1 + sReplLen,
                        sStrLen - ( sStartPos - 1 + sReplLen ) );

         zh_storclen( pcRet, sRetLen, 1 );

         if( iNoRet )
         {
            zh_xfree( pcRet );
            zh_ret();
         }
         else
            zh_retclen_buffer( pcRet, sRetLen );
      }
      else
      {
         PZH_ITEM pSubst = NULL;
         int iArgErrorMode = ct_getargerrormode();

         if( iArgErrorMode != CT_ARGERR_IGNORE )
            pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                     CT_ERROR_POSREPL, NULL, ZH_ERR_FUNCNAME, 0,
                                     EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

         if( pSubst != NULL )
            zh_itemReturnRelease( pSubst );
         else if( iNoRet )
            zh_ret();
         else
            zh_retclen( pcString, sStrLen );
      }
   }
   else
   {
      PZH_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_POSREPL, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else if( iNoRet )
         zh_ret();
      else
         zh_retc_null();
   }
}
