/*
 * HMAC-SHA2 Ziher wrappers
 *
 * Copyright 2009 Viktor Szakats
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

#include "zh_crypto.h"

ZH_FUNC( ZH_HMAC_SHA224 )
{
   unsigned char mac[ ZH_SHA224_DIGEST_SIZE ];

   zh_hmac_sha224( zh_parcx( 2 ), zh_parclen( 2 ), zh_parcx( 1 ), zh_parclen( 1 ), mac, sizeof( mac ) );

   if( ! zh_parl( 3 ) )
   {
      char hex[ ( sizeof( mac ) * 2 ) + 1 ];
      zh_strtohex( ( char * ) mac, sizeof( mac ), hex );
      zh_retclen( hex, ZH_SIZEOFARRAY( hex ) - 1 );
   }
   else
      zh_retclen( ( char * ) mac, sizeof( mac ) );
}

ZH_FUNC( ZH_HMAC_SHA256 )
{
   unsigned char mac[ ZH_SHA256_DIGEST_SIZE ];

   zh_hmac_sha256( zh_parcx( 2 ), zh_parclen( 2 ), zh_parcx( 1 ), zh_parclen( 1 ), mac, sizeof( mac ) );

   if( ! zh_parl( 3 ) )
   {
      char hex[ ( sizeof( mac ) * 2 ) + 1 ];
      zh_strtohex( ( char * ) mac, sizeof( mac ), hex );
      zh_retclen( hex, ZH_SIZEOFARRAY( hex ) - 1 );
   }
   else
      zh_retclen( ( char * ) mac, sizeof( mac ) );
}

ZH_FUNC( ZH_HMAC_SHA384 )
{
   unsigned char mac[ ZH_SHA384_DIGEST_SIZE ];

   zh_hmac_sha384( zh_parcx( 2 ), zh_parclen( 2 ), zh_parcx( 1 ), zh_parclen( 1 ), mac, sizeof( mac ) );

   if( ! zh_parl( 3 ) )
   {
      char hex[ ( sizeof( mac ) * 2 ) + 1 ];
      zh_strtohex( ( char * ) mac, sizeof( mac ), hex );
      zh_retclen( hex, ZH_SIZEOFARRAY( hex ) - 1 );
   }
   else
      zh_retclen( ( char * ) mac, sizeof( mac ) );
}

ZH_FUNC( ZH_HMAC_SHA512 )
{
   unsigned char mac[ ZH_SHA512_DIGEST_SIZE ];

   zh_hmac_sha512( zh_parcx( 2 ), zh_parclen( 2 ), zh_parcx( 1 ), zh_parclen( 1 ), mac, sizeof( mac ) );

   if( ! zh_parl( 3 ) )
   {
      char hex[ ( sizeof( mac ) * 2 ) + 1 ];
      zh_strtohex( ( char * ) mac, sizeof( mac ), hex );
      zh_retclen( hex, ZH_SIZEOFARRAY( hex ) - 1 );
   }
   else
      zh_retclen( ( char * ) mac, sizeof( mac ) );
}
