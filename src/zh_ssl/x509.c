/*
 * OpenSSL API (X509) - Ziher interface.
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

typedef struct
{
   X509 *  pX509;
   ZH_BOOL fRelease;
} ZH_X509, * PZH_X509;

static ZH_GARBAGE_FUNC( X509_release )
{
   PZH_X509 ph = ( PZH_X509 ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ph && ph->pX509 )
   {
      /* Destroy the object */
      if( ph->fRelease )
         X509_free( ( X509 * ) ph->pX509 );

      /* set pointer to NULL just in case */
      ph->pX509 = NULL;
   }
}

static const ZH_GC_FUNCS s_gcX509_funcs =
{
   X509_release,
   zh_gcDummyMark
};

ZH_BOOL zh_X509_is( int iParam )
{
   return zh_parptrGC( &s_gcX509_funcs, iParam ) != NULL;
}

X509 * zh_X509_par( int iParam )
{
   PZH_X509 ph = ( PZH_X509 ) zh_parptrGC( &s_gcX509_funcs, iParam );

   return ph ? ph->pX509 : NULL;
}

void zh_X509_ret( X509 * x509, ZH_BOOL fRelease )
{
   PZH_X509 ph = ( PZH_X509 ) zh_gcAllocate( sizeof( ZH_X509 ), &s_gcX509_funcs );

   ph->pX509    = x509;
   ph->fRelease = fRelease;

   zh_retptrGC( ( void * ) ph );
}

ZH_FUNC( X509_GET_SUBJECT_NAME )
{
   if( zh_X509_is( 1 ) )
   {
      X509 * x509 = zh_X509_par( 1 );

      if( x509 )
         zh_retptr( X509_get_subject_name( x509 ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( X509_GET_ISSUER_NAME )
{
   if( zh_X509_is( 1 ) )
   {
      X509 * x509 = zh_X509_par( 1 );

      if( x509 )
         zh_retptr( X509_get_issuer_name( x509 ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( X509_NAME_ONELINE )
{
#if OPENSSL_VERSION_NUMBER < 0x10000000L || OPENSSL_VERSION_NUMBER >= 0x1000000FL /* NOTE: Compilation error when tried with 1.0.0beta5 */
   X509_NAME * x509_name = ( X509_NAME * ) zh_parptr( 1 );

   if( x509_name )
   {
      char buffer[ 1024 ];
      X509_NAME_oneline( x509_name, buffer, sizeof( buffer ) );
      zh_retc( buffer );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
#endif
}

ZH_FUNC( X509_GET_PUBKEY )
{
   if( zh_X509_is( 1 ) )
   {
      X509 * x509 = zh_X509_par( 1 );

      if( x509 )
         zh_retptr( X509_get_pubkey( x509 ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}
