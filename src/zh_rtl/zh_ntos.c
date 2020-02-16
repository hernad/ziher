/*
 * zh_ntos() function
 *
 * Copyright 2008 Viktor Szakats
 * Copyright 2016 Przemyslaw Czerpak
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

ZH_FUNC( ZH_NTOS )
{
   PZH_ITEM pNumber = zh_param( 1, ZH_IT_NUMERIC );

   if( pNumber )
   {
      char * szResult = zh_itemStr( pNumber, NULL, NULL );

      if( szResult )
      {
         ZH_SIZE nToTrim = 0;

         while( szResult[ nToTrim ] == ' ' )
            ++nToTrim;

         if( nToTrim )
            memmove( szResult, szResult + nToTrim, strlen( szResult + nToTrim ) + 1 );

         zh_retc_buffer( szResult );
         return;
      }
   }

   zh_retc_null();
}

ZH_FUNC( ZH_NTOC )
{
   PZH_ITEM pNumber = zh_param( 1, ZH_IT_NUMERIC );

   if( pNumber )
   {
      char szBuffer[ ZH_MAX_DOUBLE_LENGTH ];

      if( ! ZH_IS_DOUBLE( pNumber ) )
      {
         ZH_MAXINT nNumber = zh_itemGetNInt( pNumber );
         int iPos = sizeof( szBuffer );
         ZH_BOOL fNeg = nNumber < 0;

         if( fNeg )
            nNumber = -nNumber;
         szBuffer[ --iPos ] = '\0';
         do
         {
            szBuffer[ --iPos ] = '0' + ( char ) ( nNumber % 10 );
            nNumber /= 10;
         }
         while( nNumber != 0 );
         if( fNeg )
            szBuffer[ --iPos ] = '-';

         zh_retc( szBuffer + iPos );
      }
      else
         zh_retc( zh_dblToStr( szBuffer, sizeof( szBuffer ),
                               zh_itemGetND( pNumber ), zh_parnidef( 2, -1 ) ) );
   }
   else
      zh_retc_null();
}
