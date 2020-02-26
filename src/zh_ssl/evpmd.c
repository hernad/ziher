/*
 * OpenSSL API (EVP MD) - Ziher interface.
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

#if OPENSSL_VERSION_NUMBER < 0x10100000L
   #define EVP_MD_CTX_new   EVP_MD_CTX_create
   #define EVP_MD_CTX_free  EVP_MD_CTX_destroy
#endif

ZH_FUNC( OPENSSL_ADD_ALL_DIGESTS )
{
   OpenSSL_add_all_digests();
}

static ZH_GARBAGE_FUNC( EVP_MD_CTX_release )
{
   void ** ph = ( void ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ph && *ph )
   {
      /* Destroy the object */
#if defined( LIBRESSL_VERSION_NUMBER )
      EVP_MD_CTX_destroy( ( EVP_MD_CTX * ) *ph );
#elif OPENSSL_VERSION_NUMBER >= 0x00907000L
      EVP_MD_CTX_free( ( EVP_MD_CTX * ) *ph );
#else
      zh_xfree( *ph );
#endif

      /* set pointer to NULL just in case */
      *ph = NULL;
   }
}

static const ZH_GC_FUNCS s_gcEVP_MD_CTX_funcs =
{
   EVP_MD_CTX_release,
   zh_gcDummyMark
};

static ZH_BOOL zh_EVP_MD_CTX_is( int iParam )
{
   return zh_parptrGC( &s_gcEVP_MD_CTX_funcs, iParam ) != NULL;
}

static EVP_MD_CTX * zh_EVP_MD_CTX_par( int iParam )
{
   void ** ph = ( void ** ) zh_parptrGC( &s_gcEVP_MD_CTX_funcs, iParam );

   return ph ? ( EVP_MD_CTX * ) *ph : NULL;
}

ZH_BOOL zh_EVP_MD_is( int iParam )
{
   return ZH_ISCHAR( iParam ) || ZH_IS_PARAM_NUM( iParam );
}

const EVP_MD * zh_EVP_MD_par( int iParam )
{
   const EVP_MD * p;

   if( ZH_ISCHAR( iParam ) )
      return EVP_get_digestbyname( zh_parc( iParam ) );

   switch( zh_parni( iParam ) )
   {
      case ZH_EVP_MD_MD_NULL:    p = EVP_md_null();   break;
#ifndef OPENSSL_NO_MD4
      case ZH_EVP_MD_MD4:        p = EVP_md4();       break;
#endif
#ifndef OPENSSL_NO_MD5
      case ZH_EVP_MD_MD5:        p = EVP_md5();       break;
#endif
#ifndef OPENSSL_NO_SHA
#if OPENSSL_VERSION_NUMBER < 0x10100000L && \
    ! defined( LIBRESSL_VERSION_NUMBER )
      case ZH_EVP_MD_SHA:        p = EVP_sha();       break;
#endif
      case ZH_EVP_MD_SHA1:       p = EVP_sha1();      break;
#if OPENSSL_VERSION_NUMBER < 0x10100000L
      case ZH_EVP_MD_DSS:        p = EVP_dss();       break;
      case ZH_EVP_MD_DSS1:       p = EVP_dss1();      break;
#endif
#if OPENSSL_VERSION_NUMBER >= 0x00908000L && \
    OPENSSL_VERSION_NUMBER < 0x10100000L
      case ZH_EVP_MD_ECDSA:      p = EVP_ecdsa();     break;
#endif
#endif
#ifndef OPENSSL_NO_SHA256
      case ZH_EVP_MD_SHA224:     p = EVP_sha224();    break;
      case ZH_EVP_MD_SHA256:     p = EVP_sha256();    break;
#endif
#ifndef OPENSSL_NO_SHA512
      case ZH_EVP_MD_SHA384:     p = EVP_sha384();    break;
      case ZH_EVP_MD_SHA512:     p = EVP_sha512();    break;
#endif
#ifndef OPENSSL_NO_RIPEMD
      case ZH_EVP_MD_RIPEMD160:  p = EVP_ripemd160(); break;
#endif
      default:                   p = NULL;
   }

   return p;
}

static int zh_EVP_MD_ptr_to_id( const EVP_MD * p )
{
   int n;

   if(      p == EVP_md_null()   ) n = ZH_EVP_MD_MD_NULL;
#ifndef OPENSSL_NO_MD4
   else if( p == EVP_md4()       ) n = ZH_EVP_MD_MD4;
#endif
#ifndef OPENSSL_NO_MD5
   else if( p == EVP_md5()       ) n = ZH_EVP_MD_MD5;
#endif
#ifndef OPENSSL_NO_SHA
#if OPENSSL_VERSION_NUMBER < 0x10100000L && \
    ! defined( LIBRESSL_VERSION_NUMBER )
   else if( p == EVP_sha()       ) n = ZH_EVP_MD_SHA;
#endif
   else if( p == EVP_sha1()      ) n = ZH_EVP_MD_SHA1;
#if OPENSSL_VERSION_NUMBER < 0x10100000L
   else if( p == EVP_dss()       ) n = ZH_EVP_MD_DSS;
   else if( p == EVP_dss1()      ) n = ZH_EVP_MD_DSS1;
#endif
#if OPENSSL_VERSION_NUMBER >= 0x00908000L && \
    OPENSSL_VERSION_NUMBER < 0x10100000L
   else if( p == EVP_ecdsa()     ) n = ZH_EVP_MD_ECDSA;
#endif
#endif
#ifndef OPENSSL_NO_SHA256
   else if( p == EVP_sha224()    ) n = ZH_EVP_MD_SHA224;
   else if( p == EVP_sha256()    ) n = ZH_EVP_MD_SHA256;
#endif
#ifndef OPENSSL_NO_SHA512
   else if( p == EVP_sha384()    ) n = ZH_EVP_MD_SHA384;
   else if( p == EVP_sha512()    ) n = ZH_EVP_MD_SHA512;
#endif
#ifndef OPENSSL_NO_RIPEMD
   else if( p == EVP_ripemd160() ) n = ZH_EVP_MD_RIPEMD160;
#endif
   else                            n = ZH_EVP_MD_UNSUPPORTED;

   return n;
}

ZH_FUNC( EVP_GET_DIGESTBYNAME )
{
   if( ZH_ISCHAR( 1 ) )
      zh_retni( zh_EVP_MD_ptr_to_id( EVP_get_digestbyname( zh_parc( 1 ) ) ) );
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_GET_DIGESTBYNID )
{
   if( ZH_IS_PARAM_NUM( 1 ) )
      zh_retni( zh_EVP_MD_ptr_to_id( EVP_get_digestbynid( zh_parni( 1 ) ) ) );
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_MD_TYPE )
{
   const EVP_MD * md = zh_EVP_MD_par( 1 );

   zh_retni( md ? EVP_MD_type( md ) : 0 );
}

ZH_FUNC( EVP_MD_NID )
{
   const EVP_MD * md = zh_EVP_MD_par( 1 );

#if OPENSSL_VERSION_NUMBER >= 0x00907000L
   zh_retni( md ? EVP_MD_nid( md ) : 0 );
#else
   zh_retni( md ? EVP_MD_type( md ) : 0 );
#endif
}

ZH_FUNC( EVP_MD_PKEY_TYPE )
{
   const EVP_MD * md = zh_EVP_MD_par( 1 );

   zh_retni( md ? EVP_MD_pkey_type( md ) : 0 );
}

ZH_FUNC( EVP_MD_SIZE )
{
   const EVP_MD * md = zh_EVP_MD_par( 1 );

   zh_retni( md ? EVP_MD_size( md ) : 0 );
}

ZH_FUNC( EVP_MD_BLOCK_SIZE )
{
   const EVP_MD * md = zh_EVP_MD_par( 1 );

   zh_retni( md ? EVP_MD_block_size( md ) : 0 );
}

ZH_FUNC( EVP_MD_CTX_NEW )
{
   void ** ph = ( void ** ) zh_gcAllocate( sizeof( EVP_MD_CTX * ), &s_gcEVP_MD_CTX_funcs );
   EVP_MD_CTX * ctx;

#if OPENSSL_VERSION_NUMBER >= 0x00907000L && \
    ! defined( LIBRESSL_VERSION_NUMBER )
   ctx = EVP_MD_CTX_new();
#else
   ctx = ( EVP_MD_CTX * ) zh_xgrabz( sizeof( EVP_MD_CTX ) );
#endif

   *ph = ctx;

   zh_retptrGC( ph );
}

ZH_FUNC( EVP_MD_CTX_RESET )
{
   if( zh_EVP_MD_CTX_is( 1 ) )
   {
      EVP_MD_CTX * ctx = zh_EVP_MD_CTX_par( 1 );

      if( ctx )
      {
#if defined( LIBRESSL_VERSION_NUMBER )
         zh_retni( EVP_MD_CTX_cleanup( ctx ) );
#elif OPENSSL_VERSION_NUMBER >= 0x10100000L
         zh_retni( EVP_MD_CTX_reset( ctx ) );
#elif OPENSSL_VERSION_NUMBER >= 0x00907000L
         zh_retni( EVP_MD_CTX_cleanup( ctx ) );
#else
         zh_retni( 0 );
#endif
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}


ZH_FUNC( EVP_MD_CTX_MD )
{
   if( zh_EVP_MD_CTX_is( 1 ) )
   {
      EVP_MD_CTX * ctx = zh_EVP_MD_CTX_par( 1 );

      if( ctx )
         zh_retni( zh_EVP_MD_ptr_to_id( EVP_MD_CTX_md( ctx ) ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_MD_CTX_COPY )
{
   if( zh_EVP_MD_CTX_is( 1 ) && zh_EVP_MD_CTX_is( 2 ) )
   {
      EVP_MD_CTX * ctx_out = zh_EVP_MD_CTX_par( 1 );
      EVP_MD_CTX * ctx_in  = zh_EVP_MD_CTX_par( 2 );

      if( ctx_out && ctx_in )
         zh_retni( EVP_MD_CTX_copy( ctx_out, ctx_in ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_MD_CTX_COPY_EX )
{
   if( zh_EVP_MD_CTX_is( 1 ) && zh_EVP_MD_CTX_is( 2 ) )
   {
      EVP_MD_CTX * ctx_out = zh_EVP_MD_CTX_par( 1 );
      EVP_MD_CTX * ctx_in  = zh_EVP_MD_CTX_par( 2 );

      if( ctx_out && ctx_in )
      {
#if OPENSSL_VERSION_NUMBER >= 0x00907000L
         zh_retni( EVP_MD_CTX_copy_ex( ctx_out, ctx_in ) );
#else
         zh_retni( 0 );
#endif
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_DIGESTINIT )
{
   const EVP_MD * md = zh_EVP_MD_par( 2 );

   if( zh_EVP_MD_CTX_is( 1 ) && md )
   {
      EVP_MD_CTX * ctx = zh_EVP_MD_CTX_par( 1 );

      if( ctx )
      {
#if OPENSSL_VERSION_NUMBER >= 0x00907000L
         zh_retni( EVP_DigestInit( ctx, md ) );
#else
         EVP_DigestInit( ctx, md );
         zh_retni( 1 );
#endif
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_DIGESTINIT_EX )
{
   const EVP_MD * md = zh_EVP_MD_par( 2 );

   if( zh_EVP_MD_CTX_is( 1 ) && md )
   {
      EVP_MD_CTX * ctx = zh_EVP_MD_CTX_par( 1 );

      if( ctx )
      {
#if OPENSSL_VERSION_NUMBER >= 0x00907000L
         zh_retni( EVP_DigestInit_ex( ctx, md, ( ENGINE * ) zh_parptr( 3 ) ) );
#else
         zh_retni( 0 );
#endif
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_DIGESTUPDATE )
{
   if( zh_EVP_MD_CTX_is( 1 ) )
   {
      EVP_MD_CTX * ctx = zh_EVP_MD_CTX_par( 1 );

      if( ctx )
      {
#if OPENSSL_VERSION_NUMBER >= 0x00907000L
         zh_retni( EVP_DigestUpdate( ctx, zh_parcx( 2 ), ( size_t ) zh_parclen( 2 ) ) );
#else
         EVP_DigestUpdate( ctx, zh_parcx( 2 ), ( size_t ) zh_parclen( 2 ) );
         zh_retni( 1 );
#endif
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_DIGESTFINAL )
{
   if( zh_EVP_MD_CTX_is( 1 ) )
   {
      EVP_MD_CTX * ctx = zh_EVP_MD_CTX_par( 1 );

      if( ctx )
      {
         unsigned char * buffer = ( unsigned char * ) zh_xgrab( EVP_MAX_MD_SIZE + 1 );
         unsigned int    size   = 0;

#if OPENSSL_VERSION_NUMBER >= 0x00907000L
         zh_retni( EVP_DigestFinal( ctx, buffer, &size ) );
#else
         EVP_DigestFinal( ctx, buffer, &size );
         zh_retni( 1 );
#endif

         if( size > 0 )
         {
            if( ! zh_storclen_buffer( ( char * ) buffer, ( ZH_SIZE ) size, 2 ) )
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

ZH_FUNC( EVP_DIGESTFINAL_EX )
{
   if( zh_EVP_MD_CTX_is( 1 ) )
   {
      EVP_MD_CTX * ctx = zh_EVP_MD_CTX_par( 1 );

      if( ctx )
      {
#if OPENSSL_VERSION_NUMBER >= 0x00907000L
         unsigned char * buffer = ( unsigned char * ) zh_xgrab( EVP_MAX_MD_SIZE + 1 );
         unsigned int    size   = 0;

         zh_retni( EVP_DigestFinal_ex( ctx, buffer, &size ) );

         if( size > 0 )
         {
            if( ! zh_storclen_buffer( ( char * ) buffer, ( ZH_SIZE ) size, 2 ) )
               zh_xfree( buffer );
         }
         else
         {
            zh_xfree( buffer );
            zh_storc( NULL, 2 );
         }
#endif
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_SIGNINIT )
{
   const EVP_MD * md = zh_EVP_MD_par( 2 );

   if( zh_EVP_MD_CTX_is( 1 ) && md )
   {
      EVP_MD_CTX * ctx = zh_EVP_MD_CTX_par( 1 );

      if( ctx )
         EVP_SignInit( ctx, md );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_SIGNINIT_EX )
{
   const EVP_MD * md = zh_EVP_MD_par( 2 );

   if( zh_EVP_MD_CTX_is( 1 ) && md )
   {
      EVP_MD_CTX * ctx = zh_EVP_MD_CTX_par( 1 );

      if( ctx )
      {
#if OPENSSL_VERSION_NUMBER >= 0x00907000L
         zh_retni( EVP_SignInit_ex( ctx, md, ( ENGINE * ) zh_parptr( 3 ) ) );
#else
         zh_retni( 0 );
#endif
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_SIGNUPDATE )
{
   if( zh_EVP_MD_CTX_is( 1 ) )
   {
      EVP_MD_CTX * ctx = zh_EVP_MD_CTX_par( 1 );

      if( ctx )
      {
#if OPENSSL_VERSION_NUMBER >= 0x00907000L
         zh_retni( EVP_SignUpdate( ctx, zh_parcx( 2 ), ( size_t ) zh_parclen( 2 ) ) );
#else
         EVP_SignUpdate( ctx, zh_parcx( 2 ), ( size_t ) zh_parclen( 2 ) );
         zh_retni( 1 );
#endif
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_SIGNFINAL )
{
   if( zh_EVP_MD_CTX_is( 1 ) && zh_EVP_PKEY_is( 3 ) )
   {
      EVP_MD_CTX * ctx = zh_EVP_MD_CTX_par( 1 );

      if( ctx )
      {
         unsigned char * buffer = ( unsigned char * ) zh_xgrab( EVP_PKEY_size( zh_EVP_PKEY_par( 3 ) ) + 1 );
         unsigned int    size   = 0;

         zh_retni( EVP_SignFinal( ctx, buffer, &size, zh_EVP_PKEY_par( 3 ) ) );

         if( size > 0 )
         {
            if( ! zh_storclen_buffer( ( char * ) buffer, ( ZH_SIZE ) size, 2 ) )
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

ZH_FUNC( EVP_VERIFYINIT )
{
   const EVP_MD * md = zh_EVP_MD_par( 2 );

   if( zh_EVP_MD_CTX_is( 1 ) && md )
   {
      EVP_MD_CTX * ctx = zh_EVP_MD_CTX_par( 1 );

      if( ctx )
      {
#if OPENSSL_VERSION_NUMBER >= 0x00907000L
         zh_retni( EVP_VerifyInit( ctx, md ) );
#else
         EVP_VerifyInit( ctx, md );
         zh_retni( 1 );
#endif
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_VERIFYINIT_EX )
{
   const EVP_MD * md = zh_EVP_MD_par( 2 );

   if( zh_EVP_MD_CTX_is( 1 ) && md )
   {
      EVP_MD_CTX * ctx = zh_EVP_MD_CTX_par( 1 );

      if( ctx )
      {
#if OPENSSL_VERSION_NUMBER >= 0x00907000L
         zh_retni( EVP_VerifyInit_ex( ctx, md, ( ENGINE * ) zh_parptr( 3 ) ) );
#else
         zh_retni( 0 );
#endif
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_VERIFYUPDATE )
{
   if( zh_EVP_MD_CTX_is( 1 ) )
   {
      EVP_MD_CTX * ctx = zh_EVP_MD_CTX_par( 1 );

      if( ctx )
      {
#if OPENSSL_VERSION_NUMBER >= 0x00907000L
         zh_retni( EVP_VerifyUpdate( ctx, zh_parcx( 2 ), ( size_t ) zh_parclen( 2 ) ) );
#else
         EVP_VerifyUpdate( ctx, zh_parcx( 2 ), ( size_t ) zh_parclen( 2 ) );
         zh_retni( 1 );
#endif
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( EVP_VERIFYFINAL )
{
   if( zh_EVP_MD_CTX_is( 1 ) && zh_EVP_PKEY_is( 3 ) )
   {
      EVP_MD_CTX * ctx = zh_EVP_MD_CTX_par( 1 );

      if( ctx )
         zh_retni( EVP_VerifyFinal( ctx, ( ZH_SSL_CONST unsigned char * ) zh_parcx( 2 ), ( unsigned int ) zh_parclen( 2 ), zh_EVP_PKEY_par( 3 ) ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}
