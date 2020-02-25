/*
 * RangeRem() and RangeRepl() CT3 string functions
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

ZH_FUNC( RANGEREM )
{
   if( ( zh_parclen( 1 ) > 0 || ZH_IS_PARAM_NUM( 1 ) ) &&
       ( zh_parclen( 2 ) > 0 || ZH_IS_PARAM_NUM( 2 ) ) && ZH_ISCHAR( 3 ) )
   {
      const char * pcString = zh_parc( 3 );
      ZH_SIZE sStrLen = zh_parclen( 3 );
      char * pcRet;
      const unsigned char * pc;
      unsigned char ucChar1, ucChar2;
      ZH_SIZE sRetIndex;
      int iMode;

      if( ZH_ISCHAR( 1 ) )
         ucChar1 = *( ( const unsigned char * ) zh_parc( 1 ) );
      else
         ucChar1 = ( unsigned char ) ( zh_parni( 1 ) % 256 );

      if( ZH_ISCHAR( 2 ) )
         ucChar2 = *( ( const unsigned char * ) zh_parc( 2 ) );
      else
         ucChar2 = ( unsigned char ) ( zh_parni( 2 ) % 256 );

      iMode = ( ucChar2 < ucChar1 );

      pcRet = ( char * ) zh_xgrab( sStrLen + 1 );
      sRetIndex = 0;
      for( pc = ( const unsigned char * ) pcString; pc < ( const unsigned char * ) pcString + sStrLen; pc++ )
      {
         int iBool = ( ( *pc ) >= ucChar1 );

         if( iMode )
            iBool |= ( ( *pc ) <= ucChar2 );
         else
            iBool &= ( ( *pc ) <= ucChar2 );

         if( ! iBool )
         {
            *( pcRet + sRetIndex ) = *pc;
            sRetIndex++;
         }
      }

      zh_retclen( pcRet, sRetIndex );
      zh_xfree( pcRet );
   }
   else
   {
      PZH_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_RANGEREM, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else if( ZH_ISCHAR( 3 ) )
         zh_retclen( zh_parc( 3 ), zh_parclen( 3 ) );
      else
         zh_retc_null();
   }
}

ZH_FUNC( RANGEREPL )
{
   int iNoRef = ct_getref() && ZH_ISBYREF( 3 );

   if( ( zh_parclen( 1 ) > 0 || ZH_IS_PARAM_NUM( 1 ) ) &&
       ( zh_parclen( 2 ) > 0 || ZH_IS_PARAM_NUM( 2 ) ) &&
       ZH_ISCHAR( 3 ) && ( zh_parclen( 4 ) > 0 || ZH_IS_PARAM_NUM( 4 ) ) )
   {
      const char * pcString = zh_parc( 3 );
      ZH_SIZE sStrLen = zh_parclen( 3 );
      char * pcRet;
      const unsigned char * pc;
      unsigned char ucChar1, ucChar2, ucReplace;
      ZH_SIZE sRetIndex;
      int iMode;

      if( ZH_ISCHAR( 1 ) )
         ucChar1 = *( ( const unsigned char * ) zh_parc( 1 ) );
      else
         ucChar1 = ( unsigned char ) ( zh_parni( 1 ) % 256 );

      if( ZH_ISCHAR( 2 ) )
         ucChar2 = *( ( const unsigned char * ) zh_parc( 2 ) );
      else
         ucChar2 = ( unsigned char ) ( zh_parni( 2 ) % 256 );

      if( ZH_ISCHAR( 4 ) )
         ucReplace = *( ( const unsigned char * ) zh_parc( 4 ) );
      else
         ucReplace = ( unsigned char ) ( zh_parni( 4 ) % 256 );

      iMode = ( ucChar2 < ucChar1 );

      pcRet = ( char * ) zh_xgrab( sStrLen + 1 );
      sRetIndex = 0;
      for( pc = ( const unsigned char * ) pcString; pc < ( const unsigned char * ) pcString + sStrLen; pc++ )
      {
         int iBool = ( ( *pc ) >= ucChar1 );

         if( iMode )
            iBool |= ( ( *pc ) <= ucChar2 );
         else
            iBool &= ( ( *pc ) <= ucChar2 );

         if( iBool )
         {
            *( pcRet + sRetIndex ) = ucReplace;
            sRetIndex++;
         }
         else
         {
            *( pcRet + sRetIndex ) = *pc;
            sRetIndex++;
         }
      }

      zh_storclen( pcRet, sStrLen, 3 );

      if( iNoRef )
         /* Contrary to the official documentation, RangeRepl() returns NIL instead of .F.
          * in this situation. If the string is not passed by reference, it returns the
          * string regardless of iNoRef. */
         zh_ret();
      else
         zh_retclen( pcRet, sStrLen );

      zh_xfree( pcRet );
   }
   else
   {
      PZH_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_RANGEREPL, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else if( iNoRef )
         zh_ret();
      else if( ZH_ISCHAR( 3 ) )
         zh_retclen( zh_parc( 3 ), zh_parclen( 3 ) );
      else
         zh_retc_null();
   }
}
