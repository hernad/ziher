/*
 * OpenSSL API - C header.
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

#ifndef HBSSL_H_
#define HBSSL_H_

#include "zh_api.h"
#include "zh_error_api.h"
#include "zh_socket.h"

#include <openssl/ssl.h>

#include "ssl.zhh"

#if OPENSSL_VERSION_NUMBER < 0x00906000L
   #error "unsupported OpenSSL version, required 0.9.6 or higher"
#endif

#if OPENSSL_VERSION_NUMBER < 0x00908030L
   #ifndef OPENSSL_NO_CAMELLIA
      #define OPENSSL_NO_CAMELLIA
   #endif
#endif
#if OPENSSL_VERSION_NUMBER < 0x00908000L
   #ifndef OPENSSL_NO_DGRAM
      #define OPENSSL_NO_DGRAM
   #endif
#endif
#if OPENSSL_VERSION_NUMBER < 0x00907000L
   #ifndef OPENSSL_NO_AES
      #define OPENSSL_NO_AES
   #endif
   #if ! defined( SSLEAY_DIR )
      #define SSLEAY_DIR   5
   #endif
   #if ! defined( SSL_ERROR_WANT_ACCEPT )
      #define SSL_ERROR_WANT_ACCEPT    8
   #endif

   /* translate old configuration macros to new ones */
   #if defined( NO_BF ) && ! defined( OPENSSL_NO_BF )
      #define OPENSSL_NO_BF
   #endif
   #if defined( NO_DES ) && ! defined( OPENSSL_NO_DES )
      #define OPENSSL_NO_DES
   #endif
   #if defined( NO_DSA ) && ! defined( OPENSSL_NO_DSA )
      #define OPENSSL_NO_DSA
   #endif
   #if defined( NO_MD4 ) && ! defined( OPENSSL_NO_MD4 )
      #define OPENSSL_NO_MD4
   #endif
   #if defined( NO_MD5 ) && ! defined( OPENSSL_NO_MD5 )
      #define OPENSSL_NO_MD5
   #endif
   #if defined( NO_RC2 ) && ! defined( OPENSSL_NO_RC2 )
      #define OPENSSL_NO_RC2
   #endif
   #if defined( NO_RC4 ) && ! defined( OPENSSL_NO_RC4 )
      #define OPENSSL_NO_RC4
   #endif
   #if defined( NO_RIPEMD ) && ! defined( OPENSSL_NO_RIPEMD )
      #define OPENSSL_NO_RIPEMD
   #endif
   #if defined( NO_RSA ) && ! defined( OPENSSL_NO_RSA )
      #define OPENSSL_NO_RSA
   #endif
   #if defined( NO_FP_API ) && ! defined( OPENSSL_NO_FP_API )
      #define OPENSSL_NO_FP_API
   #endif
   #if defined( NO_STDIO ) && ! defined( OPENSSL_NO_STDIO )
      #define OPENSSL_NO_STDIO
   #endif

#endif
#if OPENSSL_VERSION_NUMBER < 0x00908000L
   #if OPENSSL_VERSION_NUMBER < 0x00907080L || ! defined( OPENSSL_FIPS )
      #ifndef OPENSSL_NO_SHA256
         #define OPENSSL_NO_SHA256
      #endif
      #ifndef OPENSSL_NO_SHA512
         #define OPENSSL_NO_SHA512
      #endif
   #endif
#endif
#if OPENSSL_VERSION_NUMBER < 0x00906030L
   #define SSL_get_rfd  SSL_get_fd
   #define SSL_get_wfd  SSL_get_fd
#endif

#if ! defined( OPENSSL_VERSION )
   #define OPENSSL_VERSION   SSLEAY_VERSION
   #define OPENSSL_CFLAGS    SSLEAY_CFLAGS
   #define OPENSSL_BUILT_ON  SSLEAY_BUILT_ON
   #define OPENSSL_PLATFORM  SSLEAY_PLATFORM
   #define OPENSSL_DIR       SSLEAY_DIR
#endif

/* use macro to pacify warnings with missing 'const' in some function
   declarations in OpenSSL prior 0.9.8 */
#if OPENSSL_VERSION_NUMBER < 0x0090800fL
   #define ZH_SSL_CONST
#else
   #define ZH_SSL_CONST const
#endif

ZH_EXTERN_BEGIN

struct _ZH_SSLSTREAM;
typedef struct _ZH_SSLSTREAM * PZH_SSLSTREAM;

extern PZH_SOCKEX         zh_sockexNewSSL( ZH_SOCKET sd, SSL * ssl, ZH_BOOL fServer,
                                           ZH_MAXINT timeout, PZH_ITEM pSSL );
extern PZH_SSLSTREAM      zh_ssl_socketNew( ZH_SOCKET sd, SSL * ssl, ZH_BOOL fServer,
                                            ZH_MAXINT timeout, PZH_ITEM pSSL, int * piResult );
extern void               zh_ssl_socketClose( PZH_SSLSTREAM pStream );
extern const char *       zh_ssl_socketErrorStr( int iError );
extern long               zh_ssl_socketRead( PZH_SSLSTREAM pStream, ZH_SOCKET sd,
                                             void * buffer, long len, ZH_MAXINT timeout );
extern long               zh_ssl_socketWrite( PZH_SSLSTREAM pStream, ZH_SOCKET sd,
                                              const void * buffer, long len,
                                              ZH_MAXINT timeout, long * plast );

extern const SSL_METHOD * zh_ssl_method_id_to_ptr( int n );

extern ZH_BOOL            zh_BIO_is( int iParam );
extern BIO *              zh_BIO_par( int iParam );

extern ZH_BOOL            zh_SSL_CTX_is( int iParam );
extern SSL_CTX *          zh_SSL_CTX_par( int iParam );
extern SSL_CTX *          zh_SSL_CTX_itemGet( PZH_ITEM pItem );

extern ZH_BOOL            zh_SSL_is( int iParam );
extern SSL *              zh_SSL_par( int iParam );
extern SSL *              zh_SSL_itemGet( PZH_ITEM pItem );

extern ZH_BOOL            zh_SSL_SESSION_is( int iParam );
extern SSL_SESSION *      zh_SSL_SESSION_par( int iParam );

extern ZH_BOOL            zh_X509_is( int iParam );
extern X509 *             zh_X509_par( int iParam );
extern void               zh_X509_ret( X509 * x509, ZH_BOOL fRelease );

extern ZH_BOOL            zh_EVP_MD_is( int iParam );
extern const EVP_MD *     zh_EVP_MD_par( int iParam );

extern ZH_BOOL            zh_EVP_CIPHER_is( int iParam );
extern const EVP_CIPHER * zh_EVP_CIPHER_par( int iParam );

extern ZH_BOOL            zh_EVP_PKEY_is( int iParam );
extern EVP_PKEY *         zh_EVP_PKEY_par( int iParam );
extern void               zh_EVP_PKEY_ret( EVP_PKEY * pkey );

extern char *             zh_openssl_strdup( const char * pszText );

ZH_EXTERN_END

#endif /* HBSSL_H_ */
