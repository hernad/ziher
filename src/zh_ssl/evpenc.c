/*
 * OpenSSL API (EVP ENCODE) - Ziher interface.
 *
 * Copyright 2009-2016 Viktor Szakats (vszakats.net/ziher)
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

#include "zh_item_api.h"

#include <openssl/evp.h>

static ZH_GARBAGE_FUNC( EVP_ENCODE_CTX_release )
{
   void ** ph = ( void ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ph && *ph )
   {
#if OPENSSL_VERSION_NUMBER >= 0x10100000L && \
    ! defined( LIBRESSL_VERSION_NUMBER )
      EVP_ENCODE_CTX_free( ( EVP_ENCODE_CTX * ) *ph );
#else
      /* Destroy the object */
      zh_xfree( *ph );
#endif

      /* set pointer to NULL just in case */
      *ph = NULL;
   }
}

static const ZH_GC_FUNCS s_gcEVP_ENCODE_CTX_funcs =
{
   EVP_ENCODE_CTX_release,
   zh_gcDummyMark
};

static ZH_BOOL zh_EVP_ENCODE_CTX_is( int iParam )
{
   return zh_parptrGC( &s_gcEVP_ENCODE_CTX_funcs, iParam ) != NULL;
}

static EVP_ENCODE_CTX * zh_EVP_ENCODE_CTX_par( int iParam )
{
   void ** ph = ( void ** ) zh_parptrGC( &s_gcEVP_ENCODE_CTX_funcs, iParam );

   return ph ? ( EVP_ENCODE_CTX * ) *ph : NULL;
}

ZH_FUNC( EVP_ENCODE_CTX_NEW )
{
   void ** ph = ( void ** ) zh_gcAllocate( sizeof( EVP_ENCODE_CTX * ), &s_gcEVP_ENCODE_CTX_funcs );
   EVP_ENCODE_CTX * ctx;

#if OPENSSL_VERSION_NUMBER >= 0x10100000L && \
    ! defined( LIBRESSL_VERSION_NUMBER )
   ctx = EVP_ENCODE_CTX_new();
#else
   ctx = ( EVP_ENCODE_CTX * ) zh_xgrabz( sizeof( EVP_ENCODE_CTX ) );
#endif

   *ph = ctx;

   zh_retptrGC( ph );
}


ZH_FUNC( EVP_ENCODEINIT )
{
   if( zh_EVP_ENCODE_CTX_is( 1 ) )
   {
      EVP_ENCODE_CTX * ctx = zh_EVP_ENCODE_CTX_par( 1 );

      if( ctx )
         EVP_EncodeInit( ctx );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_ENCODEUPDATE )
{
   if( zh_EVP_ENCODE_CTX_is( 1 ) )
   {
      EVP_ENCODE_CTX * ctx = zh_EVP_ENCODE_CTX_par( 1 );

      if( ctx )
      {
         int size = 512;
         unsigned char * buffer = ( unsigned char * ) zh_xgrab( size + 1 );
         int result;

#if OPENSSL_VERSION_NUMBER >= 0x10100000L && \
    ! defined( LIBRESSL_VERSION_NUMBER )
         result = EVP_EncodeUpdate( ctx,
                           buffer,
                           &size,
                           ( ZH_SSL_CONST unsigned char * ) zh_parcx( 3 ),
                           ( int ) zh_parclen( 3 ) );
#else
         EVP_EncodeUpdate( ctx,
                           buffer,
                           &size,
                           ( ZH_SSL_CONST unsigned char * ) zh_parcx( 3 ),
                           ( int ) zh_parclen( 3 ) );
         result = 1;  /* Success */
#endif
         zh_retni( result );

         if( size > 0 )
         {
            if( ! zh_storclen_buffer( ( char * ) buffer, size, 2 ) )
               zh_xfree( buffer );
         }
         else
         {
            zh_xfree( buffer );
            zh_storc( NULL, 2 );
         }
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_ENCODEFINAL )
{
   if( zh_EVP_ENCODE_CTX_is( 1 ) )
   {
      EVP_ENCODE_CTX * ctx = zh_EVP_ENCODE_CTX_par( 1 );

      if( ctx )
      {
         int size = 512;
         unsigned char * buffer = ( unsigned char * ) zh_xgrab( size + 1 );

         EVP_EncodeFinal( ctx, buffer, &size );

         if( size > 0 )
         {
            if( ! zh_storclen_buffer( ( char * ) buffer, size, 2 ) )
               zh_xfree( buffer );
         }
         else
         {
            zh_xfree( buffer );
            zh_storc( NULL, 2 );
         }
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_DECODEINIT )
{
   if( zh_EVP_ENCODE_CTX_is( 1 ) )
   {
      EVP_ENCODE_CTX * ctx = zh_EVP_ENCODE_CTX_par( 1 );

      if( ctx )
         EVP_DecodeInit( ctx );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_DECODEUPDATE )
{
   if( zh_EVP_ENCODE_CTX_is( 1 ) )
   {
      EVP_ENCODE_CTX * ctx = zh_EVP_ENCODE_CTX_par( 1 );

      if( ctx )
      {
         int size = 512;
         unsigned char * buffer = ( unsigned char * ) zh_xgrab( size + 1 );

         EVP_DecodeUpdate( ctx,
                           buffer,
                           &size,
                           ( ZH_SSL_CONST unsigned char * ) zh_parcx( 3 ),
                           ( int ) zh_parclen( 3 ) );

         if( size > 0 )
         {
            if( ! zh_storclen_buffer( ( char * ) buffer, size, 2 ) )
               zh_xfree( buffer );
         }
         else
         {
            zh_xfree( buffer );
            zh_storc( NULL, 2 );
         }
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_DECODEFINAL )
{
   if( zh_EVP_ENCODE_CTX_is( 1 ) )
   {
      EVP_ENCODE_CTX * ctx = zh_EVP_ENCODE_CTX_par( 1 );

      if( ctx )
      {
         int size = 512;
         unsigned char * buffer = ( unsigned char * ) zh_xgrab( size + 1 );

         EVP_DecodeFinal( ctx, buffer, &size );

         if( size > 0 )
         {
            if( ! zh_storclen_buffer( ( char * ) buffer, size, 2 ) )
               zh_xfree( buffer );
         }
         else
         {
            zh_xfree( buffer );
            zh_storc( NULL, 2 );
         }
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}
