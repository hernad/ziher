/*
 * OpenSSL API (BIO) - Ziher interface.
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

/* BIO GC handler */

/* BIO destructor, it's executed automatically */
static ZH_GARBAGE_FUNC( ZH_BIO_Destructor )
{
   void ** ph = ( void ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ph && *ph )
   {
      /* Destroy the object */
      BIO_free( ( BIO * ) *ph );

      /* set pointer to NULL just in case */
      *ph = NULL;
   }
}

static const ZH_GC_FUNCS s_gcBIOFuncs =
{
   ZH_BIO_Destructor,
   zh_gcDummyMark
};

BIO * zh_BIO_par( int iParam )
{
   void ** ph = ( void ** ) zh_parptrGC( &s_gcBIOFuncs, iParam );

   return ph ? ( BIO * ) *ph : NULL;
}

ZH_BOOL zh_BIO_is( int iParam )
{
   void ** ph = ( void ** ) zh_parptrGC( &s_gcBIOFuncs, iParam );

   return ph && *ph;
}

static void zh_BIO_ret( BIO * bio )
{
   void ** ph = ( void ** ) zh_gcAllocate( sizeof( BIO * ), &s_gcBIOFuncs );

   *ph = ( void * ) bio;

   zh_retptrGC( ph );
}

/* */

static ZH_BOOL zh_BIO_METHOD_is( int iParam )
{
   return ZH_ISCHAR( iParam );
}

#if OPENSSL_VERSION_NUMBER >= 0x10100000L
static const BIO_METHOD * zh_BIO_METHOD_par( int iParam )
#else
static BIO_METHOD * zh_BIO_METHOD_par( int iParam )
#endif
{
#if OPENSSL_VERSION_NUMBER >= 0x10100000L
   const BIO_METHOD * p;
#else
   BIO_METHOD * p;
#endif

   switch( zh_parni( iParam ) )
   {
      case ZH_BIO_METHOD_S_NULL:        p = BIO_s_null();       break;
#ifndef OPENSSL_NO_FP_API
      case ZH_BIO_METHOD_S_FILE:        p = BIO_s_file();       break;
#endif
      case ZH_BIO_METHOD_S_MEM:         p = BIO_s_mem();        break;
      case ZH_BIO_METHOD_S_SOCKET:      p = BIO_s_socket();     break;
      case ZH_BIO_METHOD_S_CONNECT:     p = BIO_s_connect();    break;
      case ZH_BIO_METHOD_S_ACCEPT:      p = BIO_s_accept();     break;
      case ZH_BIO_METHOD_S_FD:          p = BIO_s_fd();         break;
#if 0 /* BIO_s_log() isn't exported via implibs on Windows at version 0.9.8k. [vszakats] */
#ifndef OPENSSL_SYS_OS2
      case ZH_BIO_METHOD_S_LOG:         p = BIO_s_log();        break;
#endif
#endif
      case ZH_BIO_METHOD_S_BIO:         p = BIO_s_bio();        break;
#ifndef OPENSSL_NO_DGRAM
      case ZH_BIO_METHOD_S_DATAGRAM:    p = BIO_s_datagram();   break;
#endif
      case ZH_BIO_METHOD_F_NULL:        p = BIO_f_null();       break;
      case ZH_BIO_METHOD_F_BUFFER:      p = BIO_f_buffer();     break;
#ifdef OPENSSL_SYS_VMS
      case ZH_BIO_METHOD_F_LINEBUFFER:  p = BIO_f_linebuffer(); break;
#endif
      case ZH_BIO_METHOD_F_NBIO_TEST:   p = BIO_f_nbio_test();  break;
      default:                          p = NULL;
   }

   return p;
}

#if 0
/* NOTE: Unused yet. Commented to avoid warning */
static int zh_BIO_METHOD_ptr_to_id( const BIO_METHOD * p )
{
   int n;

   if(      p == BIO_s_null()       ) n = ZH_BIO_METHOD_S_NULL;
#ifndef OPENSSL_NO_FP_API
   else if( p == BIO_s_file()       ) n = ZH_BIO_METHOD_S_FILE;
#endif
   else if( p == BIO_s_mem()        ) n = ZH_BIO_METHOD_S_MEM;
   else if( p == BIO_s_socket()     ) n = ZH_BIO_METHOD_S_SOCKET;
   else if( p == BIO_s_connect()    ) n = ZH_BIO_METHOD_S_CONNECT;
   else if( p == BIO_s_accept()     ) n = ZH_BIO_METHOD_S_ACCEPT;
   else if( p == BIO_s_fd()         ) n = ZH_BIO_METHOD_S_FD;
#if 0 /* BIO_s_log() isn't exported via implibs on Windows at version 0.9.8k. [vszakats] */
#ifndef OPENSSL_SYS_OS2
   else if( p == BIO_s_log()        ) n = ZH_BIO_METHOD_S_LOG;
#endif
#endif
   else if( p == BIO_s_bio()        ) n = ZH_BIO_METHOD_S_BIO;
#ifndef OPENSSL_NO_DGRAM
   else if( p == BIO_s_datagram()   ) n = ZH_BIO_METHOD_S_DATAGRAM;
#endif
   else if( p == BIO_f_null()       ) n = ZH_BIO_METHOD_F_NULL;
   else if( p == BIO_f_buffer()     ) n = ZH_BIO_METHOD_F_BUFFER;
#ifdef OPENSSL_SYS_VMS
   else if( p == BIO_f_linebuffer() ) n = ZH_BIO_METHOD_F_LINEBUFFER;
#endif
   else if( p == BIO_f_nbio_test()  ) n = ZH_BIO_METHOD_F_NBIO_TEST;
   else                               n = ZH_BIO_METHOD_UNSUPPORTED;

   return n;
}
#endif

ZH_FUNC( BIO_NEW )
{
   if( zh_BIO_METHOD_is( 1 ) )
      zh_BIO_ret( BIO_new( zh_BIO_METHOD_par( 1 ) ) );
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( BIO_CLEAR_FLAGS )
{
   BIO * bio = zh_BIO_par( 1 );

   if( bio )
      BIO_clear_flags( bio, zh_parni( 2 ) );
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( BIO_SET_FLAGS )
{
   BIO * bio = zh_BIO_par( 1 );

   if( bio )
      BIO_set_flags( bio, zh_parni( 2 ) );
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( BIO_GET_FLAGS )
{
   BIO * bio = zh_BIO_par( 1 );

   if( bio )
      zh_retni( BIO_get_flags( bio ) );
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( BIO_TEST_FLAGS )
{
   BIO * bio = zh_BIO_par( 1 );

   if( bio )
#if OPENSSL_VERSION_NUMBER >= 0x00908050L
      zh_retni( BIO_test_flags( bio, zh_parni( 2 ) ) );
#else
      zh_retni( 0 );
#endif
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( BIO_SET_FD )
{
   BIO * bio = zh_BIO_par( 1 );

   if( bio )
      zh_retni( BIO_set_fd( bio, zh_parnl( 2 ), zh_parnidef( 3, BIO_NOCLOSE ) ) );
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( BIO_GET_FD )
{
   BIO * bio = zh_BIO_par( 1 );

   if( bio )
      zh_retnl( BIO_get_fd( bio, NULL ) );
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( BIO_GET_RETRY_REASON )
{
   BIO * bio = zh_BIO_par( 1 );

   if( bio )
      zh_retni( BIO_get_retry_reason( bio ) );
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( BIO_SET_RETRY_SPECIAL )
{
   BIO * bio = zh_BIO_par( 1 );

   if( bio )
      BIO_set_retry_special( bio );
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( BIO_SET_RETRY_READ )
{
   BIO * bio = zh_BIO_par( 1 );

   if( bio )
      BIO_set_retry_read( bio );
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( BIO_SET_RETRY_WRITE )
{
   BIO * bio = zh_BIO_par( 1 );

   if( bio )
      BIO_set_retry_write( bio );
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( BIO_SHOULD_READ )
{
   BIO * bio = zh_BIO_par( 1 );

   if( bio )
      zh_retni( BIO_should_read( bio ) );
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( BIO_SHOULD_WRITE )
{
   BIO * bio = zh_BIO_par( 1 );

   if( bio )
      zh_retni( BIO_should_write( bio ) );
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( BIO_SHOULD_IO_SPECIAL )
{
   BIO * bio = zh_BIO_par( 1 );

   if( bio )
      zh_retni( BIO_should_io_special( bio ) );
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( BIO_RETRY_TYPE )
{
   BIO * bio = zh_BIO_par( 1 );

   if( bio )
      zh_retni( BIO_retry_type( bio ) );
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( BIO_SHOULD_RETRY )
{
   BIO * bio = zh_BIO_par( 1 );

   if( bio )
      zh_retni( BIO_should_retry( bio ) );
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( BIO_CTRL_PENDING )
{
   BIO * bio = zh_BIO_par( 1 );

   if( bio )
      zh_retnint( BIO_ctrl_pending( bio ) );
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( BIO_CTRL_WPENDING )
{
   BIO * bio = zh_BIO_par( 1 );

   if( bio )
      zh_retnint( BIO_ctrl_wpending( bio ) );
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( BIO_FLUSH )
{
   BIO * bio = zh_BIO_par( 1 );

   if( bio )
      zh_retni( BIO_flush( bio ) );
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( BIO_SEEK )
{
   BIO * bio = zh_BIO_par( 1 );

   if( bio )
      zh_retnl( BIO_seek( bio, zh_parnl( 2 ) ) );
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( BIO_TELL )
{
   BIO * bio = zh_BIO_par( 1 );

   if( bio )
      zh_retnl( BIO_tell( bio ) );
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( BIO_RESET )
{
   BIO * bio = zh_BIO_par( 1 );

   if( bio )
      zh_retni( BIO_reset( bio ) );
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( BIO_EOF )
{
   BIO * bio = zh_BIO_par( 1 );

   if( bio )
      zh_retni( BIO_eof( bio ) );
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( BIO_SET_CLOSE )
{
   BIO * bio = zh_BIO_par( 1 );

   if( bio )
      zh_retni( BIO_set_close( bio, zh_parnidef( 2, BIO_NOCLOSE ) ) );
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( BIO_GET_CLOSE )
{
   BIO * bio = zh_BIO_par( 1 );

   if( bio )
      zh_retni( BIO_get_close( bio ) );
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( BIO_NEW_SOCKET )
{
   if( ZH_IS_PARAM_NUM( 1 ) )
      zh_BIO_ret( BIO_new_socket( zh_parni( 1 ), zh_parnidef( 2, BIO_NOCLOSE ) ) );
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( BIO_NEW_DGRAM )
{
#ifndef OPENSSL_NO_DGRAM
   if( ZH_IS_PARAM_NUM( 1 ) )
      zh_BIO_ret( BIO_new_dgram( zh_parni( 1 ), zh_parnidef( 2, BIO_NOCLOSE ) ) );
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
#else
   zh_errRT_BASE( EG_NOFUNC, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
#endif
}

ZH_FUNC( BIO_NEW_FD )
{
   if( ZH_IS_PARAM_NUM( 1 ) )
      zh_BIO_ret( BIO_new_fd( zh_parnl( 1 ), zh_parnidef( 2, BIO_NOCLOSE ) ) );
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}


ZH_FUNC( BIO_NEW_MEM_BUF )
{
   PZH_ITEM pBuffer = zh_param( 1, ZH_IT_STRING );

   if( pBuffer )
   {
#if ( OPENSSL_VERSION_NUMBER >= 0x10100000L || \
      OPENSSL_VERSION_NUMBER >  0x1000206fL /* 1.0.2g or upper */ ) && \
      ! defined( LIBRESSL_VERSION_NUMBER )
      zh_BIO_ret( BIO_new_mem_buf( zh_itemGetCPtr( pBuffer ), ( int ) zh_itemGetCLen( pBuffer ) ) );
#else
      zh_BIO_ret( BIO_new_mem_buf( ZH_UNCONST( zh_itemGetCPtr( pBuffer ) ), ( int ) zh_itemGetCLen( pBuffer ) ) );
#endif
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( BIO_READ )
{
   BIO * bio = zh_BIO_par( 1 );

   if( bio )
   {
      int size = ZH_IS_PARAM_NUM( 3 ) ? zh_parni( 3 ) : ( int ) zh_parclen( 2 );

      if( size > 0 )
      {
         char * buffer = ( char * ) zh_xgrab( size + 1 );

         zh_retni( size = BIO_read( bio, buffer, size ) );

         if( ! zh_storclen( buffer, size, 2 ) )
            zh_xfree( buffer );
      }
      else
      {
         zh_retni( 0 );
         zh_storc( NULL, 2 );
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( BIO_GETS )
{
   BIO * bio = zh_BIO_par( 1 );

   if( bio )
   {
      int size = ZH_IS_PARAM_NUM( 3 ) ? zh_parni( 3 ) : ( int ) zh_parclen( 2 );

      if( size > 0 )
      {
         char * buffer = ( char * ) zh_xgrab( size + 1 );

         zh_retni( size = BIO_gets( bio, buffer, size ) );

         if( ! zh_storclen( buffer, size, 2 ) )
            zh_xfree( buffer );
      }
      else
      {
         zh_retni( 0 );
         zh_storc( NULL, 2 );
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( BIO_WRITE )
{
   BIO * bio = zh_BIO_par( 1 );

   if( bio )
   {
      int size = ( int ) zh_parclen( 2 );

      if( ZH_IS_PARAM_NUM( 3 ) )
      {
         int towrite = zh_parni( 3 );
         if( towrite >= 0 && towrite < size )
            size = towrite;
      }

      zh_retni( BIO_write( bio, zh_parcx( 2 ), size ) );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( BIO_PUTS )
{
   BIO * bio = zh_BIO_par( 1 );

   if( bio )
      zh_retni( BIO_puts( bio, zh_parcx( 2 ) ) );
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}


/* --- connect --- */

ZH_FUNC( BIO_NEW_CONNECT )
{
   if( ZH_ISCHAR( 1 ) )
#if OPENSSL_VERSION_NUMBER >= 0x10002000L && \
    ! defined( LIBRESSL_VERSION_NUMBER )
      zh_BIO_ret( BIO_new_connect( zh_parc( 1 ) ) );
#else
      /* NOTE: Discarding 'const', OpenSSL will strdup() */
      zh_BIO_ret( BIO_new_connect( ( char * ) ZH_UNCONST( zh_parc( 1 ) ) ) );
#endif
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( BIO_NEW_ACCEPT )
{
   if( ZH_ISCHAR( 1 ) )
#if OPENSSL_VERSION_NUMBER >= 0x10002000L && \
    ! defined( LIBRESSL_VERSION_NUMBER )
      zh_BIO_ret( BIO_new_accept( zh_parc( 1 ) ) );
#else
      /* NOTE: Discarding 'const', OpenSSL will strdup() */
      zh_BIO_ret( BIO_new_accept( ( char * ) ZH_UNCONST( zh_parc( 1 ) ) ) );
#endif
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( BIO_SET_CONN_HOSTNAME )
{
   BIO * bio = zh_BIO_par( 1 );

   if( bio && ZH_ISCHAR( 2 ) )
      zh_retnl( BIO_set_conn_hostname( bio, ZH_UNCONST( zh_parc( 2 ) ) ) );
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( BIO_SET_CONN_PORT )
{
   BIO * bio = zh_BIO_par( 1 );

   if( bio && ZH_ISCHAR( 2 ) )
      zh_retnl( BIO_set_conn_port( bio, ZH_UNCONST( zh_parc( 2 ) ) ) );
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}


ZH_FUNC( BIO_SET_CONN_IP )
{
   BIO * bio = zh_BIO_par( 1 );

   if( bio && ZH_ISCHAR( 2 ) )
   {
#if OPENSSL_VERSION_NUMBER >= 0x10100000L
      ZH_SYMBOL_UNUSED( bio );  /* TODO: reimplement using BIO_set_conn_address() */
      zh_retnl( 0 );
#else
      if( zh_parclen( 2 ) == 4 )
         zh_retnl( BIO_set_conn_ip( bio, ZH_UNCONST( zh_parc( 2 ) ) ) );
      else
         zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
#endif
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( BIO_GET_CONN_HOSTNAME )
{
   BIO * bio = zh_BIO_par( 1 );

   if( bio )
      zh_retc( BIO_get_conn_hostname( bio ) );
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( BIO_GET_CONN_PORT )
{
   BIO * bio = zh_BIO_par( 1 );

   if( bio )
      zh_retc( BIO_get_conn_port( bio ) );
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( BIO_GET_CONN_IP )
{
   BIO * bio = zh_BIO_par( 1 );

   if( bio )
   {
#if OPENSSL_VERSION_NUMBER >= 0x10100000L
      ZH_SYMBOL_UNUSED( bio );  /* TODO: reimplement using BIO_get_conn_address() */
      zh_retc_null();
#elif OPENSSL_VERSION_NUMBER >= 0x00906040L
      zh_retc( BIO_get_conn_ip( bio ) );
#else
      zh_retc( BIO_get_conn_ip( bio, 0 ) );
#endif
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}


ZH_FUNC( BIO_SET_NBIO )
{
   BIO * bio = zh_BIO_par( 1 );

   if( bio )
      zh_retnl( BIO_set_nbio( bio, zh_parni( 2 ) ) );
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( BIO_DO_CONNECT )
{
   BIO * bio = zh_BIO_par( 1 );

   if( bio )
      zh_retni( BIO_do_connect( bio ) );
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ERR_LOAD_BIO_STRINGS )
{
   ERR_load_BIO_strings();
}

#if 0

#define BIO_set_url( b, url )                 BIO_ctrl( b, BIO_C_SET_PROXY_PARAM, 0, ( char * ) ( url ) )
#define BIO_set_proxies( b, p )               BIO_ctrl( b, BIO_C_SET_PROXY_PARAM, 1, ( char * ) ( p ) )
/* BIO_set_nbio(b,n) */
#define BIO_set_filter_bio( b, s )            BIO_ctrl( b, BIO_C_SET_PROXY_PARAM, 2, ( char * ) ( s ) )
/* BIO *BIO_get_filter_bio(BIO *bio); */
#define BIO_set_proxy_cb( b, cb )             BIO_callback_ctrl( b, BIO_C_SET_PROXY_PARAM, 3, ( void *( *cb )( ) ) )
#define BIO_set_proxy_header( b, sk )         BIO_ctrl( b, BIO_C_SET_PROXY_PARAM, 4, ( char * ) sk )
#define BIO_set_no_connect_return( b, bool )  BIO_int_ctrl( b, BIO_C_SET_PROXY_PARAM, 5, bool )

#define BIO_get_proxy_header( b, skp )        BIO_ctrl( b, BIO_C_GET_PROXY_PARAM, 0, ( char * ) skp )
#define BIO_get_proxies( b, pxy_p )           BIO_ctrl( b, BIO_C_GET_PROXY_PARAM, 1, ( char * ) ( pxy_p ) )
#define BIO_get_url( b, url )                 BIO_ctrl( b, BIO_C_GET_PROXY_PARAM, 2, ( char * ) ( url ) )
#define BIO_get_no_connect_return( b )        BIO_ctrl( b, BIO_C_GET_PROXY_PARAM, 5, NULL )

#define BIO_set_fp( b, fp, c )                BIO_ctrl( b, BIO_C_SET_FILE_PTR, c, ( char * ) fp )
#define BIO_get_fp( b, fpp )                  BIO_ctrl( b, BIO_C_GET_FILE_PTR, 0, ( char * ) fpp )

int   BIO_indent( BIO * b, int indent, int max );
long  BIO_ctrl( BIO * bp, int cmd, long larg, void * parg );
long  BIO_callback_ctrl( BIO * b, int cmd, void ( * fp )( struct bio_st *, int, const char *, int, long, long ) );
char * BIO_ptr_ctrl( BIO * bp, int cmd, long larg );
long  BIO_int_ctrl( BIO * bp, int cmd, long larg, int iarg );
BIO * BIO_push( BIO * b, BIO * append );
BIO * BIO_pop( BIO * b );
BIO * BIO_find_type( BIO * b, int bio_type );
BIO * BIO_next( BIO * b );
BIO * BIO_get_retry_BIO( BIO * bio, int * reason );
BIO * BIO_dup_chain( BIO * in );

int BIO_nread0( BIO * bio, char ** buf );
int BIO_nread( BIO * bio, char ** buf, int num );
int BIO_nwrite0( BIO * bio, char ** buf );
int BIO_nwrite( BIO * bio, char ** buf, int num );

BIO_METHOD *   BIO_s_mem( void );

BIO_set_mem_eof_return( BIO * b, int v )
long BIO_get_mem_data( BIO * b, char ** pp )
BIO_set_mem_buf( BIO * b, BUF_MEM * bm, int c )
BIO_get_mem_ptr( BIO * b, BUF_MEM * *pp )

BIO * BIO_new_mem_buf( void * buf, int len );

#endif
