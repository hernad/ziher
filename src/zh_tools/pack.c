/*
 * CT3 CharPack() and CharUnpack() functions.
 *
 * Copyright 2004 Phil Krylov <phil@newstar.rinet.ru>
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

ZH_FUNC( CHARPACK )
{
   ZH_SIZE len = zh_parclen( 1 );
   const ZH_UCHAR * in = ( const ZH_UCHAR * ) zh_parcx( 1 );

   if( zh_parni( 2 ) == 0 )
   {
      ZH_UCHAR * out = ( ZH_UCHAR * ) zh_xgrab( len * 3 + 2 );
      ZH_SIZE n_in = 0, n_out = 0;

      out[ n_out++ ] = 158;
      out[ n_out++ ] = 158;

      while( n_in < len )
      {
         ZH_ISIZ n_count = 1, n_max = ZH_MIN( 255, len - n_in );
         ZH_UCHAR c = in[ n_in ];

         while( n_count < n_max && in[ n_in + n_count ] == c )
            n_count++;
         out[ n_out++ ] = 0;
         out[ n_out++ ] = ( ZH_UCHAR ) n_count;
         out[ n_out++ ] = c;
         n_in += n_count;
      }
      if( n_out < len )
         zh_retclen( ( const char * ) out, n_out );
      zh_xfree( out );
      if( n_out < len )
         return;
   }
   zh_retclen( ( const char * ) in, len );
}

static ZH_UCHAR * buf_append( ZH_UCHAR * buf, ZH_SIZE * buf_size, ZH_SIZE count,
                              ZH_UCHAR c, ZH_SIZE * buf_len )
{
   if( *buf_len + count > *buf_size )
   {
      *buf_size = ZH_MAX( *buf_len + count, *buf_size + 32768 );
      buf = ( ZH_UCHAR * ) zh_xrealloc( buf, *buf_size );
   }
   memset( buf + *buf_len, c, count );
   *buf_len += count;
   return buf;
}

ZH_FUNC( CHARUNPACK )
{
   ZH_SIZE len = zh_parclen( 1 );
   const ZH_UCHAR * in = ( const ZH_UCHAR * ) zh_parcx( 1 );

   if( zh_parni( 2 ) == 0 )
   {
      ZH_UCHAR * out;
      ZH_SIZE out_len = 0;
      ZH_SIZE buf_size = 32768;
      ZH_SIZE i;

      if( ! ( in[ 0 ] == 158 && in[ 1 ] == 158 ) )
      {
         zh_retclen( ( const char * ) in, len );
         return;
      }
      out = ( ZH_UCHAR * ) zh_xgrab( buf_size );
      for( i = 2; i <= len - 3; i += 3 )
      {
         if( in[ i ] != 0 )
         {
            zh_xfree( out );
            zh_retclen( ( const char * ) in, len );
            return;
         }
         out = buf_append( out, &buf_size, in[ i + 1 ], in[ i + 2 ], &out_len );
      }
      zh_retclen( ( const char * ) out, out_len );
      zh_xfree( out );
      return;
   }
   zh_retclen( ( const char * ) in, len );
}
