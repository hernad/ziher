/*
 * OpenSSL API (EVP PKEY) - Ziher interface.
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

#include "zh_item_api.h"

#include <openssl/evp.h>

static ZH_GARBAGE_FUNC( EVP_PKEY_release )
{
   void ** ph = ( void ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ph && *ph )
   {
      /* Destroy the object */
      EVP_PKEY_free( ( EVP_PKEY * ) *ph );

      /* set pointer to NULL just in case */
      *ph = NULL;
   }
}

static const ZH_GC_FUNCS s_gcEVP_PKEY_funcs =
{
   EVP_PKEY_release,
   zh_gcDummyMark
};

ZH_BOOL zh_EVP_PKEY_is( int iParam )
{
   return zh_parptrGC( &s_gcEVP_PKEY_funcs, iParam ) != NULL;
}

EVP_PKEY * zh_EVP_PKEY_par( int iParam )
{
   void ** ph = ( void ** ) zh_parptrGC( &s_gcEVP_PKEY_funcs, iParam );

   return ph ? ( EVP_PKEY * ) *ph : NULL;
}

void zh_EVP_PKEY_ret( EVP_PKEY * pkey )
{
   void ** ph = ( void ** ) zh_gcAllocate( sizeof( EVP_PKEY * ), &s_gcEVP_PKEY_funcs );

   *ph = pkey;

   zh_retptrGC( ph );
}

ZH_FUNC( EVP_PKEY_NEW )
{
   zh_EVP_PKEY_ret( EVP_PKEY_new() );
}

ZH_FUNC( EVP_PKEY_TYPE )
{
   zh_retni( EVP_PKEY_type( zh_parni( 1 ) ) );
}

ZH_FUNC( EVP_PKEY_SIZE )
{
   if( zh_EVP_PKEY_is( 1 ) )
   {
      EVP_PKEY * pkey = zh_EVP_PKEY_par( 1 );

      if( pkey )
         zh_retni( EVP_PKEY_size( pkey ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_PKEY_BITS )
{
   if( zh_EVP_PKEY_is( 1 ) )
   {
      EVP_PKEY * pkey = zh_EVP_PKEY_par( 1 );

      if( pkey )
         zh_retni( EVP_PKEY_bits( pkey ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_PKEY_ASSIGN )
{
   if( zh_EVP_PKEY_is( 1 ) )
   {
      EVP_PKEY * pkey = zh_EVP_PKEY_par( 1 );

      if( pkey )
         /* QUESTION: Is zh_openssl_strdup() okay here? [vszakats] */
         zh_retni( EVP_PKEY_assign( pkey, zh_parni( 2 ), zh_openssl_strdup( zh_parcx( 3 ) ) ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_PKEY_ASSIGN_RSA )
{
#ifndef OPENSSL_NO_RSA
   if( zh_EVP_PKEY_is( 1 ) && ZH_ISPOINTER( 2 ) )
   {
      EVP_PKEY * pkey = zh_EVP_PKEY_par( 1 );
      RSA *      key  = ( RSA * ) zh_parptr( 2 );

      if( pkey && key )
         zh_retni( EVP_PKEY_assign_RSA( pkey, key ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
#else
   zh_errRT_BASE( EG_NOFUNC, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
#endif
}

ZH_FUNC( EVP_PKEY_ASSIGN_DSA )
{
#ifndef OPENSSL_NO_DSA
   if( zh_EVP_PKEY_is( 1 ) && ZH_ISPOINTER( 2 ) )
   {
      EVP_PKEY * pkey = zh_EVP_PKEY_par( 1 );
      DSA *      key  = ( DSA * ) zh_parptr( 2 );

      if( pkey && key )
         zh_retni( EVP_PKEY_assign_DSA( pkey, key ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
#else
   zh_errRT_BASE( EG_NOFUNC, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
#endif
}

ZH_FUNC( EVP_PKEY_ASSIGN_DH )
{
#ifndef OPENSSL_NO_RSA
   if( zh_EVP_PKEY_is( 1 ) && ZH_ISPOINTER( 2 ) )
   {
      EVP_PKEY * pkey = zh_EVP_PKEY_par( 1 );
      DH *       key  = ( DH * ) zh_parptr( 2 );

      if( pkey && key )
         zh_retni( EVP_PKEY_assign_DH( pkey, key ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
#else
   zh_errRT_BASE( EG_NOFUNC, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
#endif
}

#if 0

int EVP_PKEY_set1_RSA( EVP_PKEY * pkey, RSA * key );
int EVP_PKEY_set1_DSA( EVP_PKEY * pkey, DSA * key );
int EVP_PKEY_set1_DH( EVP_PKEY * pkey, DH * key );
int EVP_PKEY_set1_EC_KEY( EVP_PKEY * pkey, EC_KEY * key );

RSA *    EVP_PKEY_get1_RSA( EVP_PKEY * pkey );
DSA *    EVP_PKEY_get1_DSA( EVP_PKEY * pkey );
DH *     EVP_PKEY_get1_DH( EVP_PKEY * pkey );
EC_KEY * EVP_PKEY_get1_EC_KEY( EVP_PKEY * pkey );

/* These changed in 0.9.9 to something different, they weren't probably documented before. */
int EVP_PKEY_decrypt( unsigned char * dec_key, const unsigned char * enc_key, int enc_key_len, EVP_PKEY * private_key );
int EVP_PKEY_encrypt( unsigned char * enc_key, const unsigned char * key, int key_len, EVP_PKEY * pub_key     );

/* 1.0.0 */
int EVP_PKEY_sign_init( EVP_PKEY_CTX * ctx );
int EVP_PKEY_sign( EVP_PKEY_CTX * ctx,
                   unsigned char * sig, size_t * siglen,
                   const unsigned char * tbs, size_t tbslen );

int EVP_PKEY_verify_init( EVP_PKEY_CTX * ctx );
int EVP_PKEY_verify( EVP_PKEY_CTX * ctx,
                     const unsigned char * sig, size_t siglen,
                     const unsigned char * tbs, size_t tbslen );

int EVP_PKEY_verify_recover_init( EVP_PKEY_CTX * ctx );
int EVP_PKEY_verify_recover( EVP_PKEY_CTX * ctx,
                             unsigned char * rout, size_t * routlen,
                             const unsigned char * sig, size_t siglen );

int EVP_PKEY_encrypt_init( EVP_PKEY_CTX * ctx );
int EVP_PKEY_encrypt( EVP_PKEY_CTX * ctx,
                      unsigned char * out, size_t * outlen,
                      const unsigned char * in, size_t inlen );

int EVP_PKEY_decrypt_init( EVP_PKEY_CTX * ctx );
int EVP_PKEY_decrypt( EVP_PKEY_CTX * ctx,
                      unsigned char * out, size_t * outlen,
                      const unsigned char * in, size_t inlen );

#endif
