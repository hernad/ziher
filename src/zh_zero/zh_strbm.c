/*
 * Turbo Boyer-Moore (Crochemore) string search
 *    Based on this code:
 *       https://web.archive.org/web/www-igm.univ-mlv.fr/~lecroq/string/node15.html
 *    Authors:
 *       Christian Charras, Thierry Lecroq
 *
 * Copyright 2010 Viktor Szakats
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

#define ASIZE  UCHAR_MAX

static void preBmBc( const char * needle, ZH_I_SIZE m, ZH_I_SIZE bmBc[] )
{
   ZH_I_SIZE i;

   for( i = 0; i < ASIZE; ++i )
      bmBc[ i ] = m;
   for( i = 0; i < m - 1; ++i )
      bmBc[ ( ZH_UCHAR ) needle[ i ] ] = m - i - 1;
}

static void suffixes( const char * needle, ZH_I_SIZE m, ZH_I_SIZE * suff )
{
   ZH_I_SIZE f, g, i;

   f = 0; /* NOTE: Fix added by me [vszakats] */
   suff[ m - 1 ] = m;
   g = m - 1;
   for( i = m - 2; i >= 0; --i )
   {
      if( i > g && suff[ i + m - 1 - f ] < i - g )
         suff[ i ] = suff[ i + m - 1 - f ];
      else
      {
         if( i < g )
            g = i;
         f = i;
         while( g >= 0 && needle[ g ] == needle[ g + m - 1 - f ] )
            --g;
         suff[ i ] = f - g;
      }
   }
}

static void preBmGs( const char * needle, ZH_I_SIZE m, ZH_I_SIZE bmGs[] )
{
   ZH_I_SIZE i, j;
   ZH_I_SIZE * suff = ( ZH_I_SIZE * ) zh_xgrab( m * sizeof( ZH_I_SIZE ) );

   suffixes( needle, m, suff );

   for( i = 0; i < m; ++i )
      bmGs[ i ] = m;

   j = 0;

   for( i = m - 1; i >= 0; --i )
      if( suff[ i ] == i + 1 )
         for( ; j < m - 1 - i; ++j )
            if( bmGs[ j ] == m )
               bmGs[ j ] = m - 1 - i;

   for( i = 0; i <= m - 2; ++i )
      bmGs[ m - 1 - suff[ i ] ] = m - 1 - i;

   zh_xfree( suff );
}

ZH_I_SIZE zh_strAtTBM( const char * needle, ZH_I_SIZE m, const char * haystack, ZH_I_SIZE n )
{
   ZH_I_SIZE r = 0, j, shift, u;
   ZH_I_SIZE bmBc[ ASIZE ];

   ZH_I_SIZE * bmGs = ( ZH_I_SIZE * ) zh_xgrab( m * sizeof( ZH_I_SIZE ) );

   /* Preprocessing */
   preBmGs( needle, m, bmGs );
   preBmBc( needle, m, bmBc );

   /* Searching */
   j = u = 0;
   shift = m;
   while( j <= n - m )
   {
      ZH_I_SIZE i = m - 1;
      while( i >= 0 && needle[ i ] == haystack[ i + j ] )
      {
         --i;
         if( u != 0 && i == m - 1 - shift )
            i -= u;
      }

      if( i < 0 )
      {
         r = j + 1;
         break;
#if 0 /* To continue search */
         shift = bmGs[ 0 ];
         u = m - shift;
#endif
      }
      else
      {
         ZH_I_SIZE v = m - 1 - i;
         ZH_I_SIZE turboShift = u - v;
         ZH_I_SIZE bcShift = bmBc[ ( ZH_UCHAR ) haystack[ i + j ] ] - m + 1 + i;
         shift = ZH_MAX( turboShift, bcShift );
         shift = ZH_MAX( shift, bmGs[ i ] );
         if( shift == bmGs[ i ] )
            u = ZH_MIN( m - shift, v );
         else
         {
            if( turboShift < bcShift )
               shift = ZH_MAX( shift, u + 1 );
            u = 0;
         }
      }
      j += shift;
   }

   zh_xfree( bmGs );

   return r;
}
