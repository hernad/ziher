/*
 * OpenSSL API (SSL) - Ziher interface.
 *
 * Copyright 2009-2017 Viktor Szakats (vszakats.net/ziher)
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

/* for applink.c */
#if defined( _MSC_VER )
   #ifndef _CRT_SECURE_NO_WARNINGS
   #define _CRT_SECURE_NO_WARNINGS
   #endif
#elif defined( __BORLANDC__ )
   /* NOTE: To avoid these with BCC 5.5:
            Warning W8065 openssl/applink.c 40: Call to function '_setmode' with no prototype in function app_fsetmod
            Error E2451 openssl/applink.c 82: Undefined symbol '_lseek' in function OPENSSL_Applink
    */
   #include "io.h"
   #define _setmode  setmode
   #undef _lseek
   #define _lseek    lseek
#endif

/* This must come before #include "zh_ssl.h".
   OpenSSL 1.1.x and upper don't require Windows headers anymore,
   but if #included, it still must come before its own headers.
   The Ziher wrapper code doesn't need the Windows headers, so
   they will be dropped once 1.0.2 is EOLed in 2019-12-31. */
#include "zh_defs.h"
#if defined( ZH_OS_WIN )
   #include <windows.h>
   #include <wincrypt.h>
#endif

#include "zh_ssl.h"

#include "zh_item_api.h"
#include "zh_vm.h"

typedef struct _ZH_SSL
{
   SSL * ssl;
   PZH_ITEM pCallbackArg;
} ZH_SSL, * PZH_SSL;

#if ! defined( ZH_OPENSSL_NO_APPLINK ) && \
    defined( ZH_OS_WIN ) && \
    defined( ZH_CPU_X86 ) && \
    OPENSSL_VERSION_NUMBER >= 0x00908000L
   /* Enable this to add support for various scenarios when
      OpenSSL is build with OPENSSL_USE_APPLINK (the default).
      In such case care must be taken to initialize pointers
      to C RTL function to avoid crashes. */
   #define ZH_OPENSSL_HAS_APPLINK
#endif

/* NOTE: See: https://www.openssl.org/support/faq.html#PROG2
         Application must call SSL_init(), so that this module gets linked.
         [vszakats] */
#if defined( ZH_OPENSSL_HAS_APPLINK )
   /* Pull a stub that returns a table with some selected
      C RTL function pointers. When linking to OpenSSL shared
      libraries, the function OPENSSL_Applink() exported from
      the application executable will be dynamically called
      from the OpenSSL crypto .dll. When linking OpenSSL statically,
      we will call it manually from SSL_init(). This will not
      work when using 'hbssl' as a dynamic lib, because
      OPENSSL_Applink() must be exported from the main executable.
      Consequently 'hbrun' will fail with operations that require
      C RTL calls internally. Such calls are currently made when
      using BIO_new_fd() BIO_new_file() IO API. */
   #if defined( ZH_GCC_HAS_DIAG ) && defined( __clang__ )
      #pragma GCC diagnostic push
      #pragma GCC diagnostic ignored "-Wpedantic"
   #endif
   #include "openssl/applink.c"
   #if defined( ZH_GCC_HAS_DIAG ) && defined( __clang__ )
      #pragma GCC diagnostic pop
   #endif
#endif

ZH_FUNC( SSL_INIT )
{
   SSL_library_init();
   SSL_load_error_strings();
}

ZH_FUNC( ZH_SSL_APPLINK )
{
#if defined( ZH_OPENSSL_HAS_APPLINK )
   zh_retl( ZH_TRUE );
#else
   zh_retl( ZH_FALSE );
#endif
}

ZH_FUNC( ZH_SSL_STATIC )
{
#if defined( ZH_DYNLIB )
   zh_retl( ZH_FALSE );
#else
   zh_retl( ZH_TRUE );
#endif
}

ZH_FUNC( OPENSSL_VERSION )
{
   int value = zh_parni( 1 );

   switch( value )
   {
      case ZH_OPENSSL_VERSION:   value = OPENSSL_VERSION;  break;
      case ZH_OPENSSL_CFLAGS:    value = OPENSSL_CFLAGS;   break;
      case ZH_OPENSSL_BUILT_ON:  value = OPENSSL_BUILT_ON; break;
      case ZH_OPENSSL_PLATFORM:  value = OPENSSL_PLATFORM; break;
      case ZH_OPENSSL_DIR:       value = OPENSSL_DIR;      break;
   }

#if OPENSSL_VERSION_NUMBER >= 0x10100000L && \
    ! defined( LIBRESSL_VERSION_NUMBER )
   zh_retc( OpenSSL_version( value ) );
#else
   zh_retc( SSLeay_version( value ) );
#endif
}

ZH_FUNC( OPENSSL_VERSION_NUMBER )
{
   zh_retnint( OPENSSL_VERSION_NUMBER );
}

ZH_FUNC( OPENSSL_VERSION_NUM )
{
#if OPENSSL_VERSION_NUMBER >= 0x10100000L && \
    ! defined( LIBRESSL_VERSION_NUMBER )
   zh_retnint( OpenSSL_version_num() );
#else
   zh_retnint( SSLeay() );
#endif
}


static ZH_GARBAGE_FUNC( PZH_SSL_release )
{
   PZH_SSL * ph = ( PZH_SSL * ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ph && *ph )
   {
      PZH_SSL zh_ssl = *ph;

      /* Destroy the object */
      if( zh_ssl->ssl )
      {
         SSL_free( zh_ssl->ssl );
         zh_ssl->ssl = NULL;
      }

      if( zh_ssl->pCallbackArg )
      {
         zh_itemRelease( zh_ssl->pCallbackArg );
         zh_ssl->pCallbackArg = NULL;
      }

      zh_xfree( zh_ssl );

      /* set pointer to NULL just in case */
      *ph = NULL;
   }
}

static const ZH_GC_FUNCS s_gcSSL_funcs =
{
   PZH_SSL_release,
   zh_gcDummyMark
};

ZH_BOOL zh_SSL_is( int iParam )
{
   void ** ph = ( void ** ) zh_parptrGC( &s_gcSSL_funcs, iParam );

   return ph && *ph && ( ( PZH_SSL ) *ph )->ssl;
}

static PZH_SSL zh_SSL_par_raw( int iParam )
{
   void ** ph = ( void ** ) zh_parptrGC( &s_gcSSL_funcs, iParam );

   return ph ? ( PZH_SSL ) *ph : NULL;
}

SSL * zh_SSL_par( int iParam )
{
   void ** ph = ( void ** ) zh_parptrGC( &s_gcSSL_funcs, iParam );

   return ph ? ( ( PZH_SSL ) *ph )->ssl : NULL;
}

SSL * zh_SSL_itemGet( PZH_ITEM pItem )
{
   void ** ph = ( void ** ) zh_itemGetPtrGC( pItem, &s_gcSSL_funcs );

   return ph ? ( ( PZH_SSL ) *ph )->ssl : NULL;
}

ZH_FUNC( SSL_NEW )
{
   if( zh_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx )
      {
         PZH_SSL * ph = ( PZH_SSL * ) zh_gcAllocate( sizeof( PZH_SSL ), &s_gcSSL_funcs );

         PZH_SSL zh_ssl = ( PZH_SSL ) zh_xgrabz( sizeof( ZH_SSL ) );

         zh_ssl->ssl = SSL_new( ctx );

         *ph = zh_ssl;

         zh_retptrGC( ph );
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_DUP )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl_par = zh_SSL_par( 1 );

      if( ssl_par )
      {
         PZH_SSL * ph = ( PZH_SSL * ) zh_gcAllocate( sizeof( PZH_SSL ), &s_gcSSL_funcs );

         PZH_SSL zh_ssl = ( PZH_SSL ) zh_xgrabz( sizeof( ZH_SSL ) );

         zh_ssl->ssl = SSL_dup( ssl_par );

         *ph = zh_ssl;

         zh_retptrGC( ph );
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_ACCEPT )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retni( SSL_accept( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CLEAR )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         SSL_clear( ssl );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}


ZH_FUNC( SSL_PENDING )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retni( SSL_pending( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_SET_BIO )
{
   BIO * rbio = zh_BIO_par( 2 );
   BIO * wbio = zh_BIO_par( 3 );

   if( zh_SSL_is( 1 ) && rbio && wbio )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         SSL_set_bio( ssl, rbio, wbio );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_GET_RBIO )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retptr( SSL_get_rbio( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_GET_WBIO )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retptr( SSL_get_wbio( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CONNECT )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retni( SSL_connect( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_SHUTDOWN )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retni( SSL_shutdown( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_VERSION )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retni( SSL_version( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_GET_VERSION )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retc( SSL_get_version( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_GET_CIPHER )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retc( SSL_get_cipher( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_DO_HANDSHAKE )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retni( SSL_do_handshake( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_RENEGOTIATE )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retni( SSL_renegotiate( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_TOTAL_RENEGOTIATIONS )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retnl( SSL_total_renegotiations( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_SET_FD )
{
   int iSD;

   if( zh_SSL_is( 1 ) && ( iSD = zh_parnidef( 2, -1 ) ) != -1 )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retni( SSL_set_fd( ssl, iSD ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_SET_RFD )
{
   int iSD;

   if( zh_SSL_is( 1 ) && ( iSD = zh_parnidef( 2, -1 ) ) != -1 )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retni( SSL_set_rfd( ssl, iSD ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_SET_WFD )
{
   int iSD;

   if( zh_SSL_is( 1 ) && ( iSD = zh_parnidef( 2, -1 ) ) != -1 )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retni( SSL_set_wfd( ssl, iSD ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_WANT )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retni( SSL_want( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_WANT_NOTHING )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retni( SSL_want_nothing( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_WANT_X509_LOOKUP )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retni( SSL_want_x509_lookup( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_WANT_READ )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retni( SSL_want_read( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_READ )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
      {
         PZH_ITEM pItem = zh_param( 2, ZH_IT_STRING );
         char *   pBuffer;
         ZH_SIZE  nLen;
         int      nRead = 0;

         if( pItem && ZH_ISBYREF( 2 ) &&
             zh_itemGetWriteCL( pItem, &pBuffer, &nLen ) )
         {
            if( ZH_IS_PARAM_NUM( 3 ) )
            {
               nRead = zh_parni( 3 );
               if( nRead >= 0 && nRead < ( int ) nLen )
                  nLen = nRead;
            }
            nRead = nLen >= INT_MAX ? INT_MAX : ( int ) nLen;

            nRead = SSL_read( ssl, pBuffer, nRead );
         }

         zh_retni( nRead );
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_PEEK )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
      {
         PZH_ITEM pItem = zh_param( 2, ZH_IT_STRING );
         char *   pBuffer;
         ZH_SIZE  nLen;
         int      nRead = 0;

         if( pItem && ZH_ISBYREF( 2 ) &&
             zh_itemGetWriteCL( pItem, &pBuffer, &nLen ) )
         {
            if( ZH_IS_PARAM_NUM( 3 ) )
            {
               nRead = zh_parni( 3 );
               if( nRead >= 0 && nRead < ( int ) nLen )
                  nLen = nRead;
            }
            nRead = nLen >= INT_MAX ? INT_MAX : ( int ) nLen;

            nRead = SSL_peek( ssl, pBuffer, nRead );
         }

         zh_retni( nRead );
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_WANT_WRITE )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retni( SSL_want_write( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_WRITE )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
      {
         PZH_ITEM pBuffer = zh_param( 2, ZH_IT_STRING );
         ZH_SIZE  nLen    = zh_itemGetCLen( pBuffer );

         if( ZH_IS_PARAM_NUM( 3 ) )
         {
            ZH_SIZE nWrite = ( ZH_SIZE ) zh_parnl( 3 );
            if( nWrite < nLen )
               nLen = nWrite;
         }

         zh_retni( SSL_write( ssl, zh_itemGetCPtr( pBuffer ), ( int ) nLen ) );
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_SET_SSL_METHOD )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
#if OPENSSL_VERSION_NUMBER < 0x10000000L
         zh_retni( SSL_set_ssl_method( ssl, ( SSL_METHOD * ) zh_ssl_method_id_to_ptr( zh_parni( 2 ) ) ) );
#else
         zh_retni( SSL_set_ssl_method( ssl, zh_ssl_method_id_to_ptr( zh_parni( 2 ) ) ) );
#endif
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_GET_SSL_METHOD )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
      {
#if OPENSSL_VERSION_NUMBER < 0x10000000L
         SSL_METHOD * p = SSL_get_ssl_method( ssl );
#else
         const SSL_METHOD * p = SSL_get_ssl_method( ssl );
#endif
         int n;

#if OPENSSL_VERSION_NUMBER >= 0x10100000L
         if(      p == TLS_method()           ) n = ZH_SSL_CTX_NEW_METHOD_TLS;
         else if( p == TLS_server_method()    ) n = ZH_SSL_CTX_NEW_METHOD_TLS_SERVER;
         else if( p == TLS_client_method()    ) n = ZH_SSL_CTX_NEW_METHOD_TLS_CLIENT;
#else
         if(      p == TLSv1_method()         ) n = ZH_SSL_CTX_NEW_METHOD_TLSV1;
         else if( p == TLSv1_server_method()  ) n = ZH_SSL_CTX_NEW_METHOD_TLSV1_SERVER;
         else if( p == TLSv1_client_method()  ) n = ZH_SSL_CTX_NEW_METHOD_TLSV1_CLIENT;
         else if( p == SSLv23_method()        ) n = ZH_SSL_CTX_NEW_METHOD_TLS;
         else if( p == SSLv23_server_method() ) n = ZH_SSL_CTX_NEW_METHOD_TLS_SERVER;
         else if( p == SSLv23_client_method() ) n = ZH_SSL_CTX_NEW_METHOD_TLS_CLIENT;
#endif
         else                                   n = ZH_SSL_CTX_NEW_METHOD_UNKNOWN;

         zh_retni( n );
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_GET_CURRENT_CIPHER )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retptr( ZH_UNCONST( SSL_get_current_cipher( ssl ) ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_GET_CIPHER_BITS )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
      {
         int alg_bits = 0;

         zh_retni( SSL_get_cipher_bits( ssl, &alg_bits ) );

         zh_storni( alg_bits, 2 );
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_GET_CIPHER_LIST )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retc( SSL_get_cipher_list( ssl, zh_parni( 2 ) ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_SET_CIPHER_LIST )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl && zh_parclen( 2 ) <= 255 )
         zh_retni( SSL_set_cipher_list( ssl, zh_parcx( 2 ) ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_GET_CIPHER_NAME )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retc( SSL_get_cipher_name( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_GET_CIPHER_VERSION )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retc( SSL_get_cipher_version( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_COPY_SESSION_ID )
{
   if( zh_SSL_is( 1 ) && zh_SSL_is( 2 ) )
   {
      SSL * ssl1 = zh_SSL_par( 1 );
      SSL * ssl2 = zh_SSL_par( 2 );

      if( ssl1 && ssl2 )
         SSL_copy_session_id( ssl1, ssl2 );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_GET_SHARED_CIPHERS )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
      {
         char buffer[ 128 + 1 ]; /* See: CVE-2006-3738 */

         buffer[ 0 ] = '\0';

         zh_retc( SSL_get_shared_ciphers( ssl, buffer, sizeof( buffer ) - 1 ) );
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_SET_TLSEXT_HOST_NAME )
{
   if( zh_SSL_is( 1 ) )
   {
#if defined( SSL_CTRL_SET_TLSEXT_HOSTNAME )
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retni( SSL_set_tlsext_host_name( ssl, ZH_UNCONST( zh_parc( 2 ) ) ) );
#endif
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_ALERT_DESC_STRING )
{
   zh_retc( SSL_alert_desc_string( zh_parni( 1 ) ) );
}

ZH_FUNC( SSL_ALERT_DESC_STRING_LONG )
{
   zh_retc( SSL_alert_desc_string_long( zh_parni( 1 ) ) );
}

ZH_FUNC( SSL_ALERT_TYPE_STRING )
{
   zh_retc( SSL_alert_type_string( zh_parni( 1 ) ) );
}

ZH_FUNC( SSL_ALERT_TYPE_STRING_LONG )
{
   zh_retc( SSL_alert_type_string_long( zh_parni( 1 ) ) );
}

ZH_FUNC( SSL_RSTATE_STRING )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retc( SSL_rstate_string( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_RSTATE_STRING_LONG )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retc( SSL_rstate_string( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_STATE_STRING )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retc( SSL_rstate_string( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_STATE_STRING_LONG )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retc( SSL_rstate_string( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

#if 0

ZH_FUNC( SSL_GET_PSK_IDENTITY_HINT )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retc( SSL_get_psk_identity_hint( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_GET_PSK_IDENTITY )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retc( SSL_get_psk_identity( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

#endif

ZH_FUNC( SSL_CHECK_PRIVATE_KEY )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retni( SSL_check_private_key( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_GET_ERROR )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retni( SSL_get_error( ssl, zh_parni( 2 ) ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_GET_FD )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retni( SSL_get_fd( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_GET_RFD )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retni( SSL_get_rfd( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_GET_WFD )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retni( SSL_get_wfd( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_GET_QUIET_SHUTDOWN )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retni( SSL_get_quiet_shutdown( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_GET_SHUTDOWN )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retni( SSL_get_shutdown( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_GET_READ_AHEAD )
{
   if( zh_SSL_is( 1 ) )
   {
#if defined( __BORLANDC__ ) /* FIXME: SSL_get_read_ahead is an unresolved external when trying to link with BCC */
      zh_retni( 0 );
#else
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retni( SSL_get_read_ahead( ssl ) );
#endif
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_GET_STATE )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retni( SSL_get_state( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_GET_VERIFY_MODE )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retni( SSL_get_verify_mode( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_IN_ACCEPT_INIT )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retni( SSL_in_accept_init( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_IN_BEFORE )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retni( SSL_in_before( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_IN_CONNECT_INIT )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retni( SSL_in_connect_init( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_IN_INIT )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retni( SSL_in_init( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_IS_INIT_FINISHED )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retni( SSL_is_init_finished( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_NUM_RENEGOTIATIONS )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retnl( SSL_num_renegotiations( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CLEAR_NUM_RENEGOTIATIONS )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retnl( SSL_clear_num_renegotiations( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_GET_DEFAULT_TIMEOUT )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retnl( SSL_get_default_timeout( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_GET_VERIFY_RESULT )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retnl( SSL_get_verify_result( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_SESSION_REUSED )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retnl( SSL_session_reused( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_SET_ACCEPT_STATE )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         SSL_set_accept_state( ssl );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_SET_CONNECT_STATE )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         SSL_set_connect_state( ssl );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_GET_OPTIONS )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retnl( SSL_get_options( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_SET_OPTIONS )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         SSL_set_options( ssl, ( unsigned long ) zh_parnl( 2 ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_SET_QUIET_SHUTDOWN )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         SSL_set_quiet_shutdown( ssl, zh_parni( 2 ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_SET_READ_AHEAD )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         SSL_set_read_ahead( ssl, zh_parni( 2 ) /* yes */ );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_SET_SHUTDOWN )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         SSL_set_shutdown( ssl, zh_parni( 2 ) /* mode */ );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_SET_VERIFY_RESULT )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         SSL_set_verify_result( ssl, zh_parnl( 2 ) /* arg */ );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_SET_MODE )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         SSL_set_mode( ssl, zh_parnl( 2 ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_GET_MODE )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retnl( SSL_get_mode( ssl ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_SET_MTU )
{
   if( zh_SSL_is( 1 ) )
   {
#if OPENSSL_VERSION_NUMBER >= 0x00908000L
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         SSL_set_mtu( ssl, zh_parnl( 2 ) );
#endif
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_GET_CERTIFICATE )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_X509_ret( SSL_get_certificate( ssl ), ZH_FALSE );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_GET_PEER_CERTIFICATE )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_X509_ret( SSL_get_peer_certificate( ssl ), ZH_TRUE );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_USE_CERTIFICATE )
{
   if( zh_SSL_is( 1 ) && zh_X509_is( 2 ) )
   {
      SSL *  ssl  = zh_SSL_par( 1 );
      X509 * x509 = zh_X509_par( 2 );

      if( ssl && x509 )
         zh_retni( SSL_use_certificate( ssl, x509 ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_ADD_CLIENT_CA )
{
   if( zh_SSL_is( 1 ) && zh_X509_is( 2 ) )
   {
      SSL *  ssl  = zh_SSL_par( 1 );
      X509 * x509 = zh_X509_par( 2 );

      if( ssl && x509 )
         zh_retni( SSL_add_client_CA( ssl, x509 ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_USE_CERTIFICATE_FILE )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retni( SSL_use_certificate_file( ssl, zh_parc( 2 ), zh_parni( 3 ) ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_USE_PRIVATEKEY_FILE )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retni( SSL_use_PrivateKey_file( ssl, zh_parc( 2 ), zh_parni( 3 ) ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_USE_RSAPRIVATEKEY_FILE )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retni( SSL_use_RSAPrivateKey_file( ssl, zh_parc( 2 ), zh_parni( 3 ) ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_GET_CIPHERS )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
      {
         STACK_OF( SSL_CIPHER ) * stack = SSL_get_ciphers( ssl );
         int len = sk_SSL_CIPHER_num( stack );

         if( len > 0 )
         {
            PZH_ITEM pArray = zh_itemArrayNew( len );
            int      tmp;

            for( tmp = 0; tmp < len; tmp++ )
               zh_arraySetPtr( pArray, tmp + 1, ZH_UNCONST( sk_SSL_CIPHER_value( stack, tmp ) ) );

            zh_itemReturnRelease( pArray );
         }
         else
            zh_reta( 0 );
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_GET_CLIENT_CA_LIST )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
      {
         STACK_OF( X509_NAME ) * stack = SSL_get_client_CA_list( ssl );
         int len = sk_X509_NAME_num( stack );

         if( len > 0 )
         {
            PZH_ITEM pArray = zh_itemArrayNew( len );
            int      tmp;

            for( tmp = 0; tmp < len; tmp++ )
               zh_arraySetPtr( pArray, tmp + 1, sk_X509_NAME_value( stack, tmp ) );

            zh_itemReturnRelease( pArray );
         }
         else
            zh_reta( 0 );
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_LOAD_CLIENT_CA_FILE )
{
   if( ZH_ISCHAR( 1 ) )
   {
      STACK_OF( X509_NAME ) * stack = SSL_load_client_CA_file( zh_parc( 1 ) );
      int len = sk_X509_NAME_num( stack );

      if( len > 0 )
      {
         PZH_ITEM pArray = zh_itemArrayNew( len );
         int      tmp;

         for( tmp = 0; tmp < len; tmp++ )
            zh_arraySetPtr( pArray, tmp + 1, sk_X509_NAME_value( stack, tmp ) );

         zh_itemReturnRelease( pArray );
      }
      else
         zh_reta( 0 );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_USE_RSAPRIVATEKEY_ASN1 )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
#if OPENSSL_VERSION_NUMBER >= 0x10100000L && \
    ! defined( LIBRESSL_VERSION_NUMBER )
         zh_retni( SSL_use_RSAPrivateKey_ASN1( ssl, ( const unsigned char * ) zh_parc( 2 ), ( int ) zh_parclen( 2 ) ) );
#else
         /* 'const' not used in 2nd param because ssh.h misses it, too.
             Bug reported: #1988 [Fixed in 1.1.0 after submitting patch]
             [vszakats] */
         zh_retni( SSL_use_RSAPrivateKey_ASN1( ssl, ( unsigned char * ) ZH_UNCONST( zh_parc( 2 ) ), ( int ) zh_parclen( 2 ) ) );
#endif
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_USE_PRIVATEKEY_ASN1 )
{
   if( zh_SSL_is( 2 ) )
   {
      SSL * ssl = zh_SSL_par( 2 );

      if( ssl )
         zh_retni( SSL_use_PrivateKey_ASN1( zh_parni( 1 ), ssl, ( ZH_SSL_CONST unsigned char * ) zh_parc( 3 ), ( int ) zh_parclen( 3 ) ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_USE_CERTIFICATE_ASN1 )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         zh_retni( SSL_use_certificate_ASN1( ssl, ( ZH_SSL_CONST unsigned char * ) zh_parc( 2 ), ( int ) zh_parclen( 2 ) ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_USE_PRIVATEKEY )
{
   if( zh_SSL_is( 1 ) && zh_EVP_PKEY_is( 2 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
         /* QUESTION: It's unclear whether we should pass a copy here,
                      and who should free such passed EVP_PKEY object.
                      [vszakats] */
         zh_retni( SSL_use_PrivateKey( ssl, zh_EVP_PKEY_par( 2 ) ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/* Callback */

#if OPENSSL_VERSION_NUMBER >= 0x00907000L
static void zh_ssl_msg_callback( int write_p, int version, int content_type, const void * buf, size_t len, SSL * ssl, void * userdata )
{
   ZH_SYMBOL_UNUSED( ssl );

   if( userdata && zh_vmRequestReenter() )
   {
      zh_vmPushEvalSym();
      zh_vmPush( ( PZH_ITEM ) userdata );
      zh_vmPushLogical( write_p );
      zh_vmPushInteger( version );
      zh_vmPushInteger( content_type );
      zh_vmPushString( ( const char * ) buf, ( ZH_SIZE ) len );
      zh_vmSend( 4 );

      zh_vmRequestRestore();
   }
}
#endif

ZH_FUNC( SSL_SET_MSG_CALLBACK )
{
   if( zh_SSL_is( 1 ) )
   {
      PZH_SSL zh_ssl = zh_SSL_par_raw( 1 );

      if( zh_ssl )
      {
#if OPENSSL_VERSION_NUMBER >= 0x00907000L
         PZH_ITEM pCallback = zh_param( 2, ZH_IT_EVALITEM );

         if( zh_ssl->pCallbackArg )
         {
            SSL_set_msg_callback_arg( zh_ssl->ssl, NULL );
            zh_itemRelease( zh_ssl->pCallbackArg );
            zh_ssl->pCallbackArg = NULL;
         }

         if( pCallback )
         {
            zh_ssl->pCallbackArg = zh_itemNew( pCallback );
            SSL_set_msg_callback_arg( zh_ssl->ssl, zh_ssl->pCallbackArg );
            SSL_set_msg_callback( zh_ssl->ssl, zh_ssl_msg_callback );
         }
         else
            SSL_set_msg_callback( zh_ssl->ssl, NULL );
#endif
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

#if 0

void         SSL_set_psk_client_callback( SSL * ssl, unsigned int ( * callback )( SSL * ssl, const char * hint, char * identity, unsigned int max_identity_len, unsigned char * psk, unsigned int max_psk_len ) );
void         SSL_set_psk_server_callback( SSL * ssl, unsigned int ( * callback )( SSL * ssl, const char * identity, unsigned char * psk, int max_psk_len ) );

EVP_PKEY *   SSL_get_privatekey( SSL * ssl );

STACK *      SSL_get_peer_cert_chain( const SSL * ssl );
int          SSL_use_RSAPrivateKey( SSL * ssl, RSA * rsa );
void         SSL_set_app_data( SSL * ssl, char * arg );
int          SSL_set_ex_data( SSL * ssl, int idx, char * arg );
char *       SSL_get_app_data( SSL * ssl );
char *       SSL_get_ex_data( ssl, int );
int          SSL_add_dir_cert_subjects_to_stack( STACK * stack, const char * dir );
int          SSL_add_file_cert_subjects_to_stack( STACK * stack, const char * file );
STACK *      SSL_dup_CA_list( STACK * sk );
SSL_CTX *    SSL_get_SSL_CTX( const SSL * ssl );
int          SSL_get_ex_data_X509_STORE_CTX_idx( void );
int          SSL_get_ex_new_index( long argl, char * argp, int ( *new_func ); ( void ), int ( * dup_func )( void ), void ( * free_func )( void ) )
void( *SSL_get_info_callback( const SSL * ssl ); )()
SSL_SESSION * SSL_get_session( const SSL * ssl );
int( *SSL_get_verify_callback( const SSL * ssl ) )( int, X509_STORE_CTX * )
void         SSL_set_client_CA_list( SSL * ssl, STACK * list );
void         SSL_set_info_callback( SSL * ssl, void ( *cb ); ( void ) )
void         SSL_set_verify( SSL * ssl, int mode, int ( *callback ); ( void ) )

#endif
