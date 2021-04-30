/*
 * HardCR() function
 *
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
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
#include "zh_item_api.h"
#include "zh_codepage_api.h"

static char * zh_strHardCR( char * pszString, ZH_SIZE nStringLen )
{
   ZH_SIZE nStringPos;
   PZH_CODEPAGE cdp;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_strHardCR(%s, %" ZH_PFS "u)", pszString, nStringLen ) );

   cdp = zh_vmCodepage();
   if( ZH_CODEPAGE_ISCUSTOM( cdp ) )
   {
      ZH_WCHAR wc;

      nStringPos = 0;
      while( nStringPos < nStringLen )
      {
         if( pszString[ nStringPos ]     == ZH_CHAR_SOFT1 &&
             pszString[ nStringPos + 1 ] == ZH_CHAR_SOFT2 )
         {
            pszString[ nStringPos ] = ZH_CHAR_HARD1;
            nStringPos += 2;
         }
         else if( ! ZH_CODEPAGE_CHAR_GET( cdp, pszString, nStringLen, &nStringPos, &wc ) )
            break;
      }
   }
   else
   {
      for( nStringPos = 0; nStringPos < nStringLen; nStringPos++ )
      {
         if( pszString[ nStringPos ]     == ZH_CHAR_SOFT1 &&
             pszString[ nStringPos + 1 ] == ZH_CHAR_SOFT2 )
         {
            pszString[ nStringPos++ ] = ZH_CHAR_HARD1;
         }
      }
   }
   return pszString;
}

ZH_FUNC( HARDCR )
{
   PZH_ITEM pString = zh_param( 1, ZH_IT_STRING );

   if( pString )
   {
      char * pszBuffer = zh_itemGetC( pString );
      ZH_SIZE nStringLen = zh_itemGetCLen( pString );

      zh_retclen_buffer( zh_strHardCR( pszBuffer, nStringLen ), nStringLen );
   }
   else
      zh_retc_null();
}
