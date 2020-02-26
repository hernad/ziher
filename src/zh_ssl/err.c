/*
 * OpenSSL API (ERR) - Ziher interface.
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

#include <openssl/err.h>

ZH_FUNC( ERR_LOAD_CRYPTO_STRINGS )
{
   ERR_load_crypto_strings();
}

ZH_FUNC( ERR_PRINT_ERRORS )
{
   BIO * bio = zh_BIO_par( 1 );

   if( bio )
      ERR_print_errors( bio );
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ERR_GET_ERROR )
{
   zh_retnint( ERR_get_error() );
}

ZH_FUNC( ERR_PEEK_ERROR )
{
   zh_retnint( ERR_peek_error() );
}

ZH_FUNC( ERR_PEEK_LAST_ERROR )
{
#if OPENSSL_VERSION_NUMBER >= 0x00907000L
   zh_retnint( ERR_peek_last_error() );
#else
   zh_retnint( -1 );
#endif
}

ZH_FUNC( ERR_ERROR_STRING )
{
   char buffer[ 120 + 1 ];

   buffer[ 0 ] = '\0';

   ERR_error_string_n( ( unsigned long ) zh_parnint( 1 ), buffer, sizeof( buffer ) );

   zh_retc( buffer );
}

ZH_FUNC( ERR_LIB_ERROR_STRING )
{
   zh_retc( ERR_lib_error_string( ( unsigned long ) zh_parnint( 1 ) ) );
}

ZH_FUNC( ERR_FUNC_ERROR_STRING )
{
   zh_retc( ERR_lib_error_string( ( unsigned long ) zh_parnint( 1 ) ) );
}

ZH_FUNC( ERR_REASON_ERROR_STRING )
{
   zh_retc( ERR_lib_error_string( ( unsigned long ) zh_parnint( 1 ) ) );
}

ZH_FUNC( ERR_GET_ERROR_LINE )
{
   const char * file = NULL;
   int          line = 0;

   zh_retnint( ERR_get_error_line( &file, &line ) );

   zh_storc( file, 1 );
   zh_storni( line, 2 );
}

ZH_FUNC( ERR_PEEK_ERROR_LINE )
{
   const char * file = NULL;
   int          line = 0;

   zh_retnint( ERR_peek_error_line( &file, &line ) );

   zh_storc( file, 1 );
   zh_storni( line, 2 );
}

ZH_FUNC( ERR_PEEK_LAST_ERROR_LINE )
{
#if OPENSSL_VERSION_NUMBER >= 0x00907000L
   const char * file = NULL;
   int          line = 0;

   zh_retnint( ERR_peek_last_error_line( &file, &line ) );

   zh_storc( file, 1 );
   zh_storni( line, 2 );
#else
   zh_retnint( -1 );

   zh_storc( NULL, 1 );
   zh_storni( 0, 2 );
#endif
}

ZH_FUNC( ERR_GET_ERROR_LINE_DATA )
{
   const char * file  = NULL;
   int          line  = 0;
   const char * data  = NULL;
   int          flags = 0;

   zh_retnint( ERR_get_error_line_data( &file, &line, &data, &flags ) );

   zh_storc( file, 1 );
   zh_storni( line, 2 );
   zh_storc( data, 3 );
   zh_storni( flags, 4 );
}

ZH_FUNC( ERR_PEEK_ERROR_LINE_DATA )
{
   const char * file  = NULL;
   int          line  = 0;
   const char * data  = NULL;
   int          flags = 0;

   zh_retnint( ERR_peek_error_line_data( &file, &line, &data, &flags ) );

   zh_storc( file, 1 );
   zh_storni( line, 2 );
   zh_storc( data, 3 );
   zh_storni( flags, 4 );
}

ZH_FUNC( ERR_PEEK_LAST_ERROR_LINE_DATA )
{
#if OPENSSL_VERSION_NUMBER >= 0x00907000L
   const char * file  = NULL;
   int          line  = 0;
   const char * data  = NULL;
   int          flags = 0;

   zh_retnint( ERR_peek_last_error_line_data( &file, &line, &data, &flags ) );

   zh_storc( file, 1 );
   zh_storni( line, 2 );
   zh_storc( data, 3 );
   zh_storni( flags, 4 );
#else
   zh_retnint( -1 );

   zh_storc( NULL, 1 );
   zh_storni( 0, 2 );
   zh_storc( NULL, 3 );
   zh_storni( 0, 4 );
#endif
}

ZH_FUNC( ERR_FREE_STRINGS )
{
#if OPENSSL_VERSION_NUMBER < 0x10100000L
   ERR_free_strings();
#endif
}
