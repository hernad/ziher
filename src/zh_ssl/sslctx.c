/*
 * OpenSSL API (SSL_CTX) - Ziher interface.
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

static ZH_GARBAGE_FUNC( SSL_CTX_release )
{
   void ** ph = ( void ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ph && *ph )
   {
      /* Destroy the object */
      SSL_CTX_free( ( SSL_CTX * ) *ph );

      /* set pointer to NULL just in case */
      *ph = NULL;
   }
}

static const ZH_GC_FUNCS s_gcSSL_CTX_funcs =
{
   SSL_CTX_release,
   zh_gcDummyMark
};

ZH_BOOL zh_SSL_CTX_is( int iParam )
{
   return zh_parptrGC( &s_gcSSL_CTX_funcs, iParam ) != NULL;
}

SSL_CTX * zh_SSL_CTX_par( int iParam )
{
   void ** ph = ( void ** ) zh_parptrGC( &s_gcSSL_CTX_funcs, iParam );

   return ph ? ( SSL_CTX * ) *ph : NULL;
}

SSL_CTX * zh_SSL_CTX_itemGet( PZH_ITEM pItem )
{
   void ** ph = ( void ** ) zh_itemGetPtrGC( pItem, &s_gcSSL_CTX_funcs );

   return ph ? ( SSL_CTX * ) *ph : NULL;
}

const SSL_METHOD * zh_ssl_method_id_to_ptr( int n )
{
   const SSL_METHOD * p;

   switch( n )
   {
#if OPENSSL_VERSION_NUMBER >= 0x10100000L
      case ZH_SSL_CTX_NEW_METHOD_TLS:           p = TLS_method();           break;
      case ZH_SSL_CTX_NEW_METHOD_TLS_SERVER:    p = TLS_server_method();    break;
      case ZH_SSL_CTX_NEW_METHOD_TLS_CLIENT:    p = TLS_client_method();    break;
#else
      case ZH_SSL_CTX_NEW_METHOD_TLSV1:         p = TLSv1_method();         break;
      case ZH_SSL_CTX_NEW_METHOD_TLSV1_SERVER:  p = TLSv1_server_method();  break;
      case ZH_SSL_CTX_NEW_METHOD_TLSV1_CLIENT:  p = TLSv1_client_method();  break;
      case ZH_SSL_CTX_NEW_METHOD_TLS:           p = SSLv23_method();        break;
      case ZH_SSL_CTX_NEW_METHOD_TLS_SERVER:    p = SSLv23_server_method(); break;
      case ZH_SSL_CTX_NEW_METHOD_TLS_CLIENT:    p = SSLv23_client_method(); break;
#endif
      default: p = SSLv23_method();
   }

   return p;
}

ZH_FUNC( SSL_CTX_NEW )
{
   void ** ph = ( void ** ) zh_gcAllocate( sizeof( SSL_CTX * ), &s_gcSSL_CTX_funcs );

#if OPENSSL_VERSION_NUMBER < 0x10000000L
   SSL_CTX * ctx = SSL_CTX_new( ( SSL_METHOD * ) zh_ssl_method_id_to_ptr( zh_parnidef( 1, ZH_SSL_CTX_NEW_METHOD_DEFAULT ) ) );
#else
   SSL_CTX * ctx = SSL_CTX_new( zh_ssl_method_id_to_ptr( zh_parnidef( 1, ZH_SSL_CTX_NEW_METHOD_DEFAULT ) ) );
#endif

   *ph = ( void * ) ctx;

   zh_retptrGC( ph );
}

ZH_FUNC( SSL_CTX_SET_SSL_VERSION )
{
   if( zh_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx )
#if OPENSSL_VERSION_NUMBER < 0x10000000L
         zh_retni( SSL_CTX_set_ssl_version( ctx, ( SSL_METHOD * ) zh_ssl_method_id_to_ptr( zh_parni( 2 ) ) ) );
#else
         zh_retni( SSL_CTX_set_ssl_version( ctx, zh_ssl_method_id_to_ptr( zh_parni( 2 ) ) ) );
#endif
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_GET_TIMEOUT )
{
   if( zh_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx )
         zh_retnl( SSL_CTX_get_timeout( ctx ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_SET_TIMEOUT )
{
   if( zh_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx )
         SSL_CTX_set_timeout( ctx, zh_parnl( 2 ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_SET_CIPHER_LIST )
{
   if( zh_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx && zh_parclen( 2 ) <= 255 )
         SSL_CTX_set_cipher_list( ctx, zh_parcx( 2 ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_ADD_SESSION )
{
   if( zh_SSL_CTX_is( 1 ) && zh_SSL_SESSION_is( 2 ) )
   {
      SSL_CTX *     ctx     = zh_SSL_CTX_par( 1 );
      SSL_SESSION * session = zh_SSL_SESSION_par( 2 );

      if( ctx && session )
         zh_retni( SSL_CTX_add_session( ctx, session ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_REMOVE_SESSION )
{
   if( zh_SSL_CTX_is( 1 ) && zh_SSL_SESSION_is( 2 ) )
   {
      SSL_CTX *     ctx     = zh_SSL_CTX_par( 1 );
      SSL_SESSION * session = zh_SSL_SESSION_par( 2 );

      if( ctx && session )
         zh_retni( SSL_CTX_remove_session( ctx, session ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_FLUSH_SESSIONS )
{
   if( zh_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx )
         SSL_CTX_flush_sessions( ctx, zh_parnl( 2 ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_GET_SESSION_CACHE_MODE )
{
   if( zh_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx )
         zh_retni( SSL_CTX_get_session_cache_mode( ctx ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_SET_SESSION_CACHE_MODE )
{
   if( zh_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx )
         SSL_CTX_set_session_cache_mode( ctx, zh_parni( 2 ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_CHECK_PRIVATE_KEY )
{
   if( zh_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx )
         zh_retni( SSL_CTX_check_private_key( ctx ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_GET_QUIET_SHUTDOWN )
{
   if( zh_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx )
         zh_retni( SSL_CTX_get_quiet_shutdown( ctx ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_GET_VERIFY_MODE )
{
   if( zh_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx )
         zh_retni( SSL_CTX_get_verify_mode( ctx ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_SESS_ACCEPT )
{
   if( zh_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx )
         zh_retni( SSL_CTX_sess_accept( ctx ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_SESS_ACCEPT_GOOD )
{
   if( zh_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx )
         zh_retni( SSL_CTX_sess_accept_good( ctx ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_SESS_ACCEPT_RENEGOTIATE )
{
   if( zh_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx )
         zh_retni( SSL_CTX_sess_accept_renegotiate( ctx ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_SESS_CACHE_FULL )
{
   if( zh_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx )
         zh_retni( SSL_CTX_sess_cache_full( ctx ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_SESS_CB_HITS )
{
   if( zh_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx )
         zh_retni( SSL_CTX_sess_cb_hits( ctx ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_SESS_CONNECT )
{
   if( zh_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx )
         zh_retni( SSL_CTX_sess_connect( ctx ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_SESS_CONNECT_GOOD )
{
   if( zh_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx )
         zh_retni( SSL_CTX_sess_connect_good( ctx ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_SESS_CONNECT_RENEGOTIATE )
{
   if( zh_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx )
         zh_retni( SSL_CTX_sess_connect_renegotiate( ctx ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_SESS_GET_CACHE_SIZE )
{
   if( zh_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx )
         zh_retni( SSL_CTX_sess_get_cache_size( ctx ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_SESS_HITS )
{
   if( zh_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx )
         zh_retni( SSL_CTX_sess_hits( ctx ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_SESS_MISSES )
{
   if( zh_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx )
         zh_retni( SSL_CTX_sess_misses( ctx ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_SESS_NUMBER )
{
   if( zh_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx )
         zh_retni( SSL_CTX_sess_number( ctx ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_SESS_TIMEOUTS )
{
   if( zh_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx )
         zh_retni( SSL_CTX_sess_timeouts( ctx ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_NEED_TMP_RSA )
{
   if( zh_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx )
         zh_retnl( SSL_CTX_need_tmp_RSA( ctx ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_SESS_SET_CACHE_SIZE )
{
   if( zh_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx )
         SSL_CTX_sess_set_cache_size( ctx, zh_parni( 2 ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_SET_DEFAULT_READ_AHEAD )
{
   if( zh_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx )
         SSL_CTX_set_default_read_ahead( ctx, zh_parni( 2 ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_GET_OPTIONS )
{
   if( zh_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx )
         zh_retnl( SSL_CTX_get_options( ctx ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_SET_OPTIONS )
{
   if( zh_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx )
         SSL_CTX_set_options( ctx, ( unsigned long ) zh_parnl( 2 ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_SET_QUIET_SHUTDOWN )
{
   if( zh_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx )
         SSL_CTX_set_quiet_shutdown( ctx, zh_parni( 2 ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_SET_MODE )
{
   if( zh_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx )
         SSL_CTX_set_mode( ctx, zh_parnl( 2 ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_GET_MODE )
{
   if( zh_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx )
         zh_parnl( SSL_CTX_get_mode( ctx ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_USE_CERTIFICATE )
{
   if( zh_SSL_CTX_is( 1 ) && zh_X509_is( 2 ) )
   {
      SSL_CTX * ctx  = zh_SSL_CTX_par( 1 );
      X509 *    x509 = zh_X509_par( 2 );

      if( ctx && x509 )
         zh_retni( SSL_CTX_use_certificate( ctx, x509 ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_ADD_CLIENT_CA )
{
   if( zh_SSL_CTX_is( 1 ) && zh_X509_is( 2 ) )
   {
      SSL_CTX * ctx  = zh_SSL_CTX_par( 1 );
      X509 *    x509 = zh_X509_par( 2 );

      if( ctx && x509 )
         zh_retni( SSL_CTX_add_client_CA( ctx, x509 ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_GET_CLIENT_CA_LIST )
{
   if( zh_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx )
      {
#if OPENSSL_VERSION_NUMBER < 0x10000000L || OPENSSL_VERSION_NUMBER >= 0x1000000FL /* NOTE: Compilation error when tried with 1.0.0beta5 */
         STACK_OF( X509_NAME ) * stack = SSL_CTX_get_client_CA_list( ctx );
         int len = sk_X509_NAME_num( stack );

         if( len > 0 )
         {
            PZH_ITEM pArray = zh_itemArrayNew( sk_X509_NAME_num( stack ) );
            int      tmp;

            for( tmp = 0; tmp < len; tmp++ )
               zh_arraySetPtr( pArray, tmp + 1, sk_X509_NAME_value( stack, tmp ) );

            zh_itemReturnRelease( pArray );
         }
         else
#endif
         zh_reta( 0 );
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_ADD_EXTRA_CHAIN_CERT )
{
   if( zh_SSL_CTX_is( 1 ) && zh_X509_is( 2 ) )
   {
      SSL_CTX * ctx  = zh_SSL_CTX_par( 1 );
      X509 *    x509 = zh_X509_par( 2 );

      if( ctx && x509 )
         zh_retnl( SSL_CTX_add_extra_chain_cert( ctx, x509 ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_USE_CERTIFICATE_FILE )
{
   if( zh_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx )
         zh_retni( SSL_CTX_use_certificate_file( ctx, zh_parc( 2 ), zh_parni( 3 ) ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_USE_CERTIFICATE_CHAIN_FILE )
{
   if( zh_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx )
         zh_retni( SSL_CTX_use_certificate_chain_file( ctx, zh_parc( 2 ) ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_USE_PRIVATEKEY_FILE )
{
   if( zh_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx )
         zh_retni( SSL_CTX_use_PrivateKey_file( ctx, zh_parc( 2 ), zh_parni( 3 ) ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_USE_RSAPRIVATEKEY_FILE )
{
   if( zh_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx )
         zh_retni( SSL_CTX_use_RSAPrivateKey_file( ctx, zh_parc( 2 ), zh_parni( 3 ) ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_USE_RSAPRIVATEKEY_ASN1 )
{
   if( zh_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx )
         zh_retni( SSL_CTX_use_RSAPrivateKey_ASN1( ctx, ( ZH_SSL_CONST unsigned char * ) zh_parc( 2 ), ( int ) zh_parclen( 2 ) ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_USE_PRIVATEKEY_ASN1 )
{
   if( zh_SSL_CTX_is( 2 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 2 );

      if( ctx )
         zh_retni( SSL_CTX_use_PrivateKey_ASN1( zh_parni( 1 ), ctx, ( ZH_SSL_CONST unsigned char * ) zh_parc( 3 ), ( int ) zh_parclen( 3 ) ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_USE_CERTIFICATE_ASN1 )
{
   if( zh_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx )
         zh_retni( SSL_CTX_use_certificate_ASN1( ctx, ( int ) zh_parclen( 2 ), ( ZH_SSL_CONST unsigned char * ) zh_parc( 2 ) ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_USE_PRIVATEKEY )
{
   if( zh_SSL_CTX_is( 1 ) && zh_EVP_PKEY_is( 2 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx )
         /* QUESTION: It's unclear whether we should pass a copy here,
                      and who should free such passed EV_PKEY object.
                      [vszakats] */
         zh_retni( SSL_CTX_use_PrivateKey( ctx, zh_EVP_PKEY_par( 2 ) ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_CTX_LOAD_VERIFY_LOCATIONS )
{
#ifndef OPENSSL_NO_STDIO
   if( zh_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx )
         zh_retni( SSL_CTX_load_verify_locations( ctx, zh_parc( 2 ) /* CAfile */, zh_parc( 3 ) /* CApath */ ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
#else
   zh_errRT_BASE( EG_NOFUNC, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
#endif
}

ZH_FUNC( SSL_CTX_SET_DEFAULT_VERIFY_PATHS )
{
#ifndef OPENSSL_NO_STDIO
   if( zh_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = zh_SSL_CTX_par( 1 );

      if( ctx )
         zh_retni( SSL_CTX_set_default_verify_paths( ctx ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
#else
   zh_errRT_BASE( EG_NOFUNC, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
#endif
}

#if 0

#define sk_X509_NAME_new_null()       SKM_sk_new_null( X509_NAME )
#define sk_X509_NAME_push( st, val )  SKM_sk_push( X509_NAME, ( st ), ( val ) )
#define sk_X509_NAME_free( st )       SKM_sk_free( X509_NAME, ( st ) )

X509_STORE * SSL_CTX_get_cert_store( const SSL_CTX * );
void SSL_CTX_set_cert_store( SSL_CTX *, X509_STORE * );
void SSL_CTX_set_cert_store( SSL_CTX * ctx, X509_STORE * cs );
int  SSL_CTX_use_RSAPrivateKey( SSL_CTX * ctx, RSA * rsa );
long SSL_CTX_ctrl( SSL_CTX * ctx, int cmd, long larg, char * parg );

void SSL_CTX_set_app_data( SSL_CTX * ctx, void * arg );
int SSL_CTX_set_ex_data( SSL_CTX * s, int idx, char * arg );
char * SSL_CTX_get_app_data( ctx );
char * SSL_CTX_get_ex_data( ctx, int );

int( *SSL_CTX_get_client_cert_cb( SSL_CTX * ctx ) )( SSL * ssl, X509 * *x509, EVP_PKEY * *pkey );
int SSL_CTX_get_ex_new_index( long argl, char * argp, int ( *new_func ); ( void ), int ( * dup_func )( void ), void ( * free_func )( void ) )
void( *SSL_CTX_get_info_callback( SSL_CTX * ctx ) )( SSL * ssl, int cb, int ret );
int( *SSL_CTX_get_verify_callback( const SSL_CTX * ctx ) )( int ok, X509_STORE_CTX * ctx );
SSL_SESSION *( *SSL_CTX_sess_get_get_cb( SSL_CTX * ctx ) )( SSL * ssl, unsigned char * data, int len, int * copy );
int ( *SSL_CTX_sess_get_new_cb( SSL_CTX * ctx )( SSL * ssl, SSL_SESSION * sess );
void ( *SSL_CTX_sess_get_remove_cb( SSL_CTX * ctx )( SSL_CTX * ctx, SSL_SESSION * sess );
void SSL_CTX_sess_set_get_cb( SSL_CTX * ctx, SSL_SESSION * ( *cb )( SSL * ssl, unsigned char * data, int len, int * copy ) );
void SSL_CTX_sess_set_new_cb( SSL_CTX * ctx, int ( * cb )( SSL * ssl, SSL_SESSION * sess ) );
void SSL_CTX_sess_set_remove_cb( SSL_CTX * ctx, void ( * cb )( SSL_CTX * ctx, SSL_SESSION * sess ) );
LHASH * SSL_CTX_sessions( SSL_CTX * ctx );
void SSL_CTX_set_cert_verify_cb( SSL_CTX * ctx, int ( * cb )(), char * arg )
void SSL_CTX_set_client_CA_list( SSL_CTX * ctx, STACK * list );
void SSL_CTX_set_client_cert_cb( SSL_CTX * ctx, int ( * cb )( SSL * ssl, X509 ** x509, EVP_PKEY ** pkey ) );
void SSL_CTX_set_default_passwd_cb( SSL_CTX * ctx, int ( *cb ); ( void ) )
void SSL_CTX_set_info_callback( SSL_CTX * ctx, void ( * cb )( SSL * ssl, int cb, int ret ) );
void SSL_CTX_set_msg_callback( SSL_CTX * ctx, void ( * cb )( int write_p, int version, int content_type, const void * buf, size_t len, SSL * ssl, void * arg ) );
void SSL_CTX_set_msg_callback_arg( SSL_CTX * ctx, void * arg );
long SSL_CTX_set_tmp_dh( SSL_CTX * ctx, DH * dh );
long SSL_CTX_set_tmp_dh_callback( SSL_CTX * ctx, DH * ( *cb )( void ) );
long SSL_CTX_set_tmp_rsa( SSL_CTX * ctx, RSA * rsa );
/* SSL_CTX_set_tmp_rsa_callback */
long SSL_CTX_set_tmp_rsa_callback( SSL_CTX * ctx, RSA * ( *cb )( SSL * ssl, int export, int keylength ) );
long SSL_set_tmp_rsa_callback( SSL * ssl, RSA * ( *cb )( SSL * ssl, int export, int keylength ) );
The same as SSL_CTX_set_tmp_rsa_callback, except it operates on an SSL session instead of a context.
void SSL_CTX_set_verify( SSL_CTX * ctx, int mode, int ( *cb ); ( void ) )
void SSL_CTX_set_psk_client_callback( SSL_CTX * ctx, unsigned int ( * callback )( SSL * ssl, const char * hint, char * identity, unsigned int max_identity_len, unsigned char * psk, unsigned int max_psk_len ) );
void SSL_CTX_set_psk_server_callback( SSL_CTX * ctx, unsigned int ( * callback )( SSL * ssl, const char * identity, unsigned char * psk, int max_psk_len ) );

#endif
