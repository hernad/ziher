/*
 * adler32 checksum function
 *
 * Copyright 2007 Przemyslaw Czerpak
 * Algorithm taken from adler32.c Copyright (C) 1995-2002 Mark Adler
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
#include "zh_error_api.h"
#include "zh_chksum.h"

#define BASE  65521  /* largest prime smaller than 65536 */
#define NMAX  5552   /* largest n such that 255n(n+1)/2 + (n+1)(BASE-1) <= 2^32-1 */

#define LOOP_DO1( buf, i )   { s1 += buf[ i ]; s2 += s1; }
#define LOOP_DO2( buf, i )   { LOOP_DO1( buf, i ) LOOP_DO1( buf, i + 1 ) }
#define LOOP_DO4( buf, i )   { LOOP_DO2( buf, i ) LOOP_DO2( buf, i + 2 ) }
#define LOOP_DO8( buf, i )   { LOOP_DO4( buf, i ) LOOP_DO4( buf, i + 4 ) }
#define LOOP_DO16( buf, i )  { LOOP_DO8( buf, i ) LOOP_DO8( buf, i + 8 ) }

ZH_U32 zh_adler32( ZH_U32 adler, const void * buf, ZH_SIZE len )
{
   ZH_U32 s1 = adler & 0xffff;
   ZH_U32 s2 = ( adler >> 16 ) & 0xffff;

   if( buf && len )
   {
      const unsigned char * ucbuf = ( const unsigned char * ) buf;
      do
      {
         ZH_ISIZ n = len < NMAX ? len : NMAX;
         len -= n;
         if( n >= 16 )
         {
            do
            {
               LOOP_DO16( ucbuf, 0 )
               ucbuf += 16;
               n -= 16;
            }
            while( n >= 16 );
         }
         if( n )
         {
            do
            {
               s1 += *ucbuf++;
               s2 += s1;
            }
            while( --n );
         }
         s1 %= BASE;
         s2 %= BASE;
      }
      while( len );
   }

   return ( s2 << 16 ) | s1;
}

ZH_FUNC( ZH_ADLER32 )
{
   const char * szString = zh_parc( 1 );

   if( szString )
      zh_retnint( zh_adler32( ( ZH_U32 ) zh_parnl( 2 ), szString, zh_parclen( 1 ) ) );
   else
      zh_errRT_BASE( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}
