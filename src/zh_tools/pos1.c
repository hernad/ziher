/*
 * PosAlpha(), PosLower(), PosRange() and PosUpper() CT3 string functions
 *
 * Copyright 1999-2001 Viktor Szakats (vszakats.net/ziher) (PosUpper())
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

/* defines */
#define DO_POS1_POSALPHA  0
#define DO_POS1_POSLOWER  1
#define DO_POS1_POSRANGE  2
#define DO_POS1_POSUPPER  3

/* helper function for the Pos*() functions */
static void do_pos1( int iSwitch )
{
   if( ZH_ISCHAR( 1 ) &&                  /* all functions need string as 1st param */
       ( iSwitch != DO_POS1_POSRANGE ||   /* that's the only condition for all functions _except_ PosRange() */
         ( iSwitch == DO_POS1_POSRANGE && /* In addition, PosRange() needs .. */
           ZH_ISCHAR( 2 ) &&              /* .. string as 2nd .. */
           ZH_ISCHAR( 3 ) ) ) )           /* .. and 3rd param */
   {
      const unsigned char * pcString, * puc;
      ZH_SIZE sStrLen;
      unsigned char ucChar1 = ' ', ucChar2 = ' ';
      int iMode;
      ZH_SIZE sIgnore;
      int iParamShift = 0;

      if( iSwitch == DO_POS1_POSRANGE )
      {
         if( zh_parclen( 1 ) == 0 )
         {
            zh_retns( 0 );
            return;
         }
         else
            ucChar1 = *( zh_parc( 1 ) );

         if( zh_parclen( 2 ) == 0 )
         {
            zh_retns( 0 );
            return;
         }
         else
            ucChar2 = *( zh_parc( 2 ) );

         iParamShift += 2;
      }

      pcString = ( const unsigned char * ) zh_parc( iParamShift + 1 );
      sStrLen = zh_parclen( iParamShift + 1 );

      iMode = zh_parl( iParamShift + 2 );
      sIgnore = zh_parnsdef( iParamShift + 3, 0 );

      for( puc = pcString + sIgnore; puc < pcString + sStrLen; puc++ )
      {
         int iDoRet = 0;

         switch( iSwitch )
         {
            case DO_POS1_POSALPHA:
               iDoRet = zh_charIsAlpha( ( ZH_UCHAR ) *puc );
               break;

            case DO_POS1_POSLOWER:
               iDoRet = zh_charIsLower( ( ZH_UCHAR ) *puc );
               break;

            case DO_POS1_POSRANGE:
               iDoRet = ( ucChar1 <= *puc && ucChar2 >= *puc );
               break;

            case DO_POS1_POSUPPER:
               iDoRet = zh_charIsUpper( ( ZH_UCHAR ) *puc );
               break;
         }

         if( ( iMode && ! iDoRet ) || ( ! iMode && iDoRet ) )
         {
            zh_retns( puc - pcString + 1 );
            return;
         }
      }
      zh_retns( 0 );
   }
   else
   {
      PZH_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
      {
         ZH_ERRCODE iError = 0;

         switch( iSwitch )
         {
            case DO_POS1_POSALPHA:
               iError = CT_ERROR_POSALPHA;
               break;

            case DO_POS1_POSLOWER:
               iError = CT_ERROR_POSLOWER;
               break;

            case DO_POS1_POSRANGE:
               iError = CT_ERROR_POSRANGE;
               break;

            case DO_POS1_POSUPPER:
               iError = CT_ERROR_POSUPPER;
               break;
         }
         pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG, iError,
                                  NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );
      }

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else
         zh_retns( 0 );
   }
}

ZH_FUNC( POSALPHA )
{
   do_pos1( DO_POS1_POSALPHA );
}

ZH_FUNC( POSLOWER )
{
   do_pos1( DO_POS1_POSLOWER );
}

ZH_FUNC( POSRANGE )
{
   do_pos1( DO_POS1_POSRANGE );
}

ZH_FUNC( POSUPPER )
{
   do_pos1( DO_POS1_POSUPPER );
}
