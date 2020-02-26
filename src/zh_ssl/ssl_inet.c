/*
 * SSL encryption for Ziher zh_inet*() connections
 *
 * Copyright 2014 Przemyslaw Czerpak
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

#define _ZH_ZNET_INTERNAL_

#include "zh_ssl.h"

#include "zh_item_api.h"
#include "zh_date.h"
#include "zlib/zh_znet.h"

static long zh_inetReadSSL( PZH_ZNETSTREAM pStream, ZH_SOCKET sd,
                            void * buffer, long len, ZH_MAXINT timeout )
{
   return zh_ssl_socketRead( ( PZH_SSLSTREAM ) pStream, sd, buffer, len, timeout );
}

static long zh_inetWriteSSL( PZH_ZNETSTREAM pStream, ZH_SOCKET sd,
                             const void * buffer, long len, ZH_MAXINT timeout,
                             long * plast )
{
   return zh_ssl_socketWrite( ( PZH_SSLSTREAM ) pStream, sd, buffer, len, timeout, plast );
}

static void zh_inetCloseSSL( PZH_ZNETSTREAM pStream )
{
   zh_ssl_socketClose( ( PZH_SSLSTREAM ) pStream );
}

static long zh_inetFlushSSL( PZH_ZNETSTREAM pStream, ZH_SOCKET sd,
                             ZH_MAXINT timeout, ZH_BOOL fSync )
{
   ZH_SYMBOL_UNUSED( pStream );
   ZH_SYMBOL_UNUSED( sd );
   ZH_SYMBOL_UNUSED( timeout );
   ZH_SYMBOL_UNUSED( fSync );

   return 0;
}

static int zh_inetErrorSSL( PZH_ZNETSTREAM pStream )
{
   ZH_SYMBOL_UNUSED( pStream );

   return zh_socketGetError();
}

static const char * zh_inetErrStrSSL( PZH_ZNETSTREAM pStream, int iError )
{
   ZH_SYMBOL_UNUSED( pStream );

   return zh_ssl_socketErrorStr( iError );
}

static void zh_inetStartSSL( ZH_BOOL fServer )
{
   PZH_ITEM pItem = zh_param( 1, ZH_IT_POINTER );
   ZH_SOCKET sd = zh_znetInetFD( pItem, ZH_TRUE );

   if( sd != ZH_NO_SOCKET )
   {
      if( zh_SSL_is( 2 ) )
      {
         int iResult = -2;
         SSL * ssl = zh_SSL_par( 2 );

         if( ssl )
         {
            ZH_MAXINT timeout = ZH_IS_PARAM_NUM( 3 ) ? zh_parnint( 3 ) :
                                zh_znetInetTimeout( pItem, ZH_FALSE );
            PZH_SSLSTREAM pStream = zh_ssl_socketNew( sd, ssl, fServer, timeout,
                                                      zh_param( 2, ZH_IT_POINTER ),
                                                      &iResult );
            if( pStream )
            {
               if( ! zh_znetInetInitialize( pItem, ( PZH_ZNETSTREAM ) pStream,
                                            zh_inetReadSSL, zh_inetWriteSSL,
                                            zh_inetFlushSSL, zh_inetCloseSSL,
                                            zh_inetErrorSSL, zh_inetErrStrSSL ) )
               {
                  zh_ssl_socketClose( pStream );
                  iResult = -3;
               }
            }
         }
         zh_retni( iResult );
      }
      else
         zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
   }
}

/* zh_inetSSL_connect( <pSocket>, <pSSL> [, <nTimeout> ] ) */
ZH_FUNC( ZH_INETSSL_CONNECT )
{
   zh_inetStartSSL( ZH_FALSE );
}

/* zh_inetSSL_accept( <pSocket>, <pSSL> [, <nTimeout> ] ) */
ZH_FUNC( ZH_INETSSL_ACCEPT )
{
   zh_inetStartSSL( ZH_TRUE );
}
