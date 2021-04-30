/*
 * SIX compatible function:
 *       sx_SlimFast()
 *       sx_WildMatch()
 *       sx_Version()
 *       sx_Error()
 *
 * Copyright 2007 Przemyslaw Czerpak
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

#include "zh_api.h"
#include "zh_codepage_api.h"

ZH_FUNC( SX_SLIMFAST )
{
   const char * szExp = zh_parc( 1 );

   if( szExp && *szExp )
   {
      char * szDst, cQuote = 0, c;
      ZH_SIZE nDst;

      szDst = zh_cdpnDupUpper( zh_vmCodepage(), szExp, NULL );
      szExp = szDst;
      nDst = 0;

      while( ( c = *szExp++ ) != 0 )
      {
         if( c == cQuote )
            cQuote = 0;
         else if( ! cQuote )
         {
            if( c == '"' || c == '\'' )
               cQuote = c;
            else if( c == ' ' && nDst && szDst[ nDst - 1 ] == ' ' )
               continue;
         }
         szDst[ nDst++ ] = c;
      }

      zh_retclen_buffer( szDst, nDst );
   }
   else
      zh_retc_null();
}

ZH_FUNC( SX_WILDMATCH )
{
   const char * szPattern = zh_parc( 1 ), * szValue = zh_parc( 2 );
   ZH_BOOL fMatch = ZH_FALSE;

   if( szPattern && szPattern[ 0 ] && szValue )
      fMatch = zh_strMatchWild( szValue, szPattern );

   zh_retl( fMatch );
}

#define ZH_SX_VER   "1.00.00"
#define ZH_SX_DAY   "20070530"
#define ZH_SX_TIME  "01:00"
#define ZH_SX_FULL  "Ziher SIx3 compatible library, 1.00.00 2007/05/30 01:00"

ZH_FUNC( SX_VERSION )
{
   switch( zh_parni( 1 ) )
   {
      case 1:
         zh_retds( ZH_SX_DAY );
         break;
      case 2:
         zh_retc( ZH_SX_TIME );
         break;
      case 3:
         zh_retc( ZH_SX_FULL );
         break;
      default:
         zh_retc( ZH_SX_VER );
         break;
   }
}

ZH_FUNC( SX_ERROR )
{
   /* not use by Ziher */
   zh_retni( 0 );
}
