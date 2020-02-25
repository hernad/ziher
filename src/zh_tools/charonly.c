/*
 * CT3 string functions
 *     - CharOnly()
 *     - CharRem()
 *     - WordOnly()
 *     - WordRem()
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

/* defines */
#define DO_CHARONLY_CHARONLY  0
#define DO_CHARONLY_WORDONLY  1
#define DO_CHARONLY_CHARREM   2
#define DO_CHARONLY_WORDREM   3

/* helper function for the *one functions */
static void do_charonly( int iSwitch )
{
   /* param check */
   if( ZH_ISCHAR( 1 ) && ZH_ISCHAR( 2 ) )
   {
      const char * pcString = zh_parc( 2 );
      ZH_SIZE sStrLen = zh_parclen( 2 );
      const char * pcOnlySet = zh_parc( 1 );
      ZH_SIZE sOnlySetLen = zh_parclen( 1 );
      char * pcRet;
      ZH_SIZE sRetStrLen = 0;
      int iShift;
      const char * pcSub;

      /* check for zero-length strings */
      switch( iSwitch )
      {
         case DO_CHARONLY_CHARONLY:
         case DO_CHARONLY_WORDONLY:
            if( sStrLen == 0 || sOnlySetLen == 0 )
            {
               zh_retc_null();
               return;
            }
            break;

         case DO_CHARONLY_CHARREM:
         case DO_CHARONLY_WORDREM:
            if( sStrLen == 0 )
            {
               zh_retc_null();
               return;
            }
            if( sOnlySetLen == 0 )
            {
               zh_retclen( pcString, sStrLen );
               return;
            }
            break;
      }

      if( iSwitch == DO_CHARONLY_WORDONLY ||
          iSwitch == DO_CHARONLY_WORDREM )
         iShift = 2;
      else
         iShift = 1;

      pcRet = ( char * ) zh_xgrab( sStrLen );

      for( pcSub = pcString; pcSub < pcString + sStrLen + 1 - iShift; pcSub += iShift )
      {
         const char * pc = ct_at_exact_forward( pcOnlySet, sOnlySetLen, pcSub, iShift, NULL );
         ZH_BOOL fBool = ( pc != NULL && ( ( pc - pcOnlySet ) % iShift ) == 0 );
         if( fBool ? iSwitch == DO_CHARONLY_CHARONLY || iSwitch == DO_CHARONLY_WORDONLY
                   : iSwitch == DO_CHARONLY_CHARREM  || iSwitch == DO_CHARONLY_WORDREM )
         {
            for( pc = pcSub; pc < pcSub + iShift; pc++ )
               pcRet[ sRetStrLen++ ] = *pc;
         }
      }

      /* copy last character if string length is odd */
      if( iShift == 2 && sStrLen % 2 == 1 )
         pcRet[ sRetStrLen++ ] = pcString[ sStrLen - 1 ];

      zh_retclen( pcRet, sRetStrLen );
      zh_xfree( pcRet );
   }
   else
   {
      PZH_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
      {
         int iError = 0;

         switch( iSwitch )
         {
            case DO_CHARONLY_CHARONLY:
               iError = CT_ERROR_CHARONLY;
               break;

            case DO_CHARONLY_WORDONLY:
               iError = CT_ERROR_WORDONLY;
               break;

            case DO_CHARONLY_CHARREM:
               iError = CT_ERROR_CHARREM;
               break;

            case DO_CHARONLY_WORDREM:
               iError = CT_ERROR_WORDREM;
               break;
         }
         pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG, iError,
                                  NULL, ZH_ERR_FUNCNAME, 0, EF_CANSUBSTITUTE,
                                  ZH_ERR_ARGS_BASEPARAMS );
      }

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else
         zh_retc_null();
   }
}

ZH_FUNC( CHARONLY )
{
   do_charonly( DO_CHARONLY_CHARONLY );
}

ZH_FUNC( WORDONLY )
{
   do_charonly( DO_CHARONLY_WORDONLY );
}

ZH_FUNC( CHARREM )
{
   do_charonly( DO_CHARONLY_CHARREM );
}

ZH_FUNC( WORDREM )
{
   do_charonly( DO_CHARONLY_WORDREM );
}
