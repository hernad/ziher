/*
 * OpenSSL API (SSL_SESSION) - Ziher interface.
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

static ZH_GARBAGE_FUNC( SSL_SESSION_release )
{
   void ** ph = ( void ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ph && *ph )
   {
      /* Destroy the object */
      SSL_SESSION_free( ( SSL_SESSION * ) *ph );

      /* set pointer to NULL just in case */
      *ph = NULL;
   }
}

static const ZH_GC_FUNCS s_gcSSL_SESSION_funcs =
{
   SSL_SESSION_release,
   zh_gcDummyMark
};

ZH_BOOL zh_SSL_SESSION_is( int iParam )
{
   return zh_parptrGC( &s_gcSSL_SESSION_funcs, iParam ) != NULL;
}

SSL_SESSION * zh_SSL_SESSION_par( int iParam )
{
   void ** ph = ( void ** ) zh_parptrGC( &s_gcSSL_SESSION_funcs, iParam );

   return ph ? ( SSL_SESSION * ) *ph : NULL;
}

ZH_FUNC( SSL_SESSION_NEW )
{
   void ** ph = ( void ** ) zh_gcAllocate( sizeof( SSL_SESSION * ), &s_gcSSL_SESSION_funcs );

   SSL_SESSION * session = SSL_SESSION_new();

   *ph = ( void * ) session;

   zh_retptrGC( ph );
}

ZH_FUNC( SSL_SESSION_CMP )
{
   if( zh_SSL_SESSION_is( 1 ) && zh_SSL_SESSION_is( 2 ) )
   {
#if OPENSSL_VERSION_NUMBER < 0x10000000L
      SSL_SESSION * session1 = zh_SSL_SESSION_par( 1 );
      SSL_SESSION * session2 = zh_SSL_SESSION_par( 2 );

      if( session1 && session2 )
         zh_retni( SSL_SESSION_cmp( session1, session2 ) );
#endif
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_SESSION_SET_TIME )
{
   if( zh_SSL_SESSION_is( 1 ) )
   {
      SSL_SESSION * session = zh_SSL_SESSION_par( 1 );

      if( session )
         zh_retnl( SSL_SESSION_set_time( session, zh_parnl( 2 ) ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_SESSION_SET_TIMEOUT )
{
   if( zh_SSL_SESSION_is( 1 ) )
   {
      SSL_SESSION * session = zh_SSL_SESSION_par( 1 );

      if( session )
         zh_retnl( SSL_SESSION_set_timeout( session, zh_parnl( 2 ) ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_SESSION_GET_TIME )
{
   if( zh_SSL_SESSION_is( 1 ) )
   {
      SSL_SESSION * session = zh_SSL_SESSION_par( 1 );

      if( session )
         zh_retnl( SSL_SESSION_get_time( session ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_SESSION_GET_TIMEOUT )
{
   if( zh_SSL_SESSION_is( 1 ) )
   {
      SSL_SESSION * session = zh_SSL_SESSION_par( 1 );

      if( session )
         zh_retnl( SSL_SESSION_get_timeout( session ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SSL_SESSION_HASH )
{
   if( zh_SSL_SESSION_is( 1 ) )
   {
#if OPENSSL_VERSION_NUMBER < 0x10000000L
      SSL_SESSION * session = zh_SSL_SESSION_par( 1 );

      if( session )
         zh_retnl( SSL_SESSION_hash( session ) );
#endif
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

#if 0

char * SSL_SESSION_get_app_data( SSL_SESSION * s );
char * SSL_SESSION_get_ex_data( const SSL_SESSION * s, int idx );
void SSL_SESSION_set_app_data( SSL_SESSION * s, char * a );
int SSL_SESSION_set_ex_data( SSL_SESSION * s, int idx, char * arg );

int SSL_SESSION_get_ex_new_index( long argl, char * argp, int ( * new_func )( void ), int ( * dup_func )( void ), void ( * free_func )( void ) )
int SSL_SESSION_print( BIO * bp, const SSL_SESSION * x );
int SSL_SESSION_print_fp( FILE * fp, const SSL_SESSION * x );

#endif
