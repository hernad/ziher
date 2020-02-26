/*
 * OpenSSL API (EVP) - Ziher interface.
 *
 * Copyright 2009 Viktor Szakats (vszakats.net/ziher)
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

#include "zh_ssl.h"

#include <openssl/evp.h>

char * zh_openssl_strdup( const char * pszText )
{
   char * pszDup;
   size_t len = strlen( pszText ) + 1;

   pszDup = ( char * ) OPENSSL_malloc( len );
   memcpy( pszDup, pszText, len );

   return pszDup;
}

ZH_FUNC( OPENSSL_ADD_ALL_ALGORITHMS )
{
   OpenSSL_add_all_algorithms();
}

ZH_FUNC( EVP_CLEANUP )
{
#if OPENSSL_VERSION_NUMBER < 0x10100000L
   EVP_cleanup();
#endif
}

ZH_FUNC( ERR_LOAD_EVP_STRINGS )
{
   ERR_load_EVP_strings();
}

ZH_FUNC( EVP_PKEY_FREE )
{
   EVP_PKEY * key = ( EVP_PKEY * ) zh_parptr( 1 );

   if( key )
      EVP_PKEY_free( key );
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_BYTESTOKEY )
{
   const EVP_CIPHER * cipher = zh_EVP_CIPHER_par( 1 );
   const EVP_MD *     md     = zh_EVP_MD_par( 2 );

   if( cipher && md && ( ! ZH_ISCHAR( 3 ) || zh_parclen( 3 ) == 8 ) )
   {
      unsigned char key[ EVP_MAX_KEY_LENGTH ];
      unsigned char iv[ EVP_MAX_IV_LENGTH ];

      zh_retni( EVP_BytesToKey( cipher,
                                ( ZH_SSL_CONST EVP_MD * ) md,
                                ( ZH_SSL_CONST unsigned char * ) zh_parc( 3 ) /* salt */,
                                ( ZH_SSL_CONST unsigned char * ) zh_parcx( 4 ) /* data */,
                                ( int ) zh_parclen( 4 ),
                                zh_parni( 5 ) /* count */,
                                key,
                                iv ) );

      zh_storc( ( char * ) key, 6 );
      zh_storc( ( char * ) iv, 7 );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}
