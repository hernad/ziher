/*
 * CT3 Number and bit manipulation functions:
 *       CToBit(), BitToC()
 *
 * Copyright 2011 Przemyslaw Czerpak
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

ZH_FUNC( CTOBIT )
{
   ZH_SIZE nString = zh_parclen( 1 );
   int iResult = 0;

   if( nString > 0 )
   {
      ZH_SIZE nPattern = zh_parclen( 2 );

      if( nPattern >= 1 && nPattern <= 16 )
      {
         ZH_SIZE n;

         const char * pszString = zh_parc( 1 ),
                    * pszPattern = zh_parc( 2 );

         for( n = 0; n < nString; ++n )
         {
            char c = pszString[ n ];
            int i = 0;

            do
            {
               if( pszPattern[ i ] == c )
               {
                  iResult |= 1 << ( ( int ) nPattern - i - 1 );
                  break;
               }
            }
            while( ++i < ( int ) nPattern );
         }
      }
      else
         iResult = -1;
   }
   zh_retni( iResult );
}

ZH_FUNC( BITTOC )
{
   ZH_SIZE nPattern = zh_parclen( 2 );

   if( nPattern >= 1 && nPattern <= 16 )
   {
      const char * pszPattern = zh_parc( 2 );
      char szBuffer[ 16 ];
      char * pszResult = &szBuffer[ sizeof( szBuffer ) ];
      int iValue, iLen = 0;

      iValue = zh_parnidef( 1, -1 );
      if( iValue > 0xFFFF || iValue < 0 )
         iValue = 0;

      if( zh_parl( 3 ) )
      {
         while( nPattern-- > 0 )
         {
            *--pszResult = ( iValue & 1 ) ? pszPattern[ nPattern ] : ' ';
            ++iLen;
            iValue >>= 1;
         }
      }
      else
      {
         while( iValue != 0 && nPattern-- > 0 )
         {
            if( iValue & 1 )
            {
               *--pszResult = pszPattern[ nPattern ];
               ++iLen;
            }
            iValue >>= 1;
         }
      }
      zh_retclen( pszResult, iLen );
   }
   else
      zh_retc_null();
}
