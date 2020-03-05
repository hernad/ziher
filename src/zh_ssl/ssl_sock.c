/*
 * Ziher extended socket filter with SSL encryption
 *
 * Copyright 2015 Przemyslaw Czerpak
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

/* this has to be declared before zh_socket.h is included */
#define _ZH_SOCKEX_IMPLEMENTATION_

#include "zh_ssl.h"

#include "zh_item_api.h"
#include "zh_vm.h"
#include "zh_date.h"
#include "zh_init.h"

typedef struct _ZH_SSLSTREAM
{
   SSL *       ssl;
   PZH_ITEM    pSSL;
   ZH_BOOL     blocking;
}
ZH_SSLSTREAM;

const char * zh_ssl_socketErrorStr( int iError )
{
   if( iError >= ZH_SSL_SOCK_ERROR_BASE )
   {
      switch( iError - ZH_SSL_SOCK_ERROR_BASE )
      {
         case SSL_ERROR_NONE:
            return "SSL_ERROR_NONE";
         case SSL_ERROR_ZERO_RETURN:
            return "SSL_ERROR_ZERO_RETURN";
         case SSL_ERROR_WANT_READ:
            return "SSL_ERROR_WANT_READ";
         case SSL_ERROR_WANT_WRITE:
            return "SSL_ERROR_WANT_WRITE";
         case SSL_ERROR_WANT_CONNECT:
            return "SSL_ERROR_WANT_CONNECT";
         case SSL_ERROR_WANT_ACCEPT:
            return "SSL_ERROR_WANT_ACCEPT";
         case SSL_ERROR_WANT_X509_LOOKUP:
            return "SSL_ERROR_WANT_X509_LOOKUP";
         case SSL_ERROR_SYSCALL:
            return "SSL_ERROR_SYSCALL";
         case SSL_ERROR_SSL:
            return "SSL_ERROR_SSL";
      }
   }

   return zh_socketErrorStr( iError );
}

long zh_ssl_socketRead( PZH_SSLSTREAM pStream, ZH_SOCKET sd,
                        void * buffer, long len, ZH_MAXINT timeout )
{
   long lRead = -1;
   int iToRead = -1;
   ZH_MAXUINT timer;

   #if 0
   sd = SSL_get_rfd( pStream->ssl );
   #endif

#if LONG_MAX > INT_MAX
   if( len > INT_MAX )
      len = INT_MAX;
#endif

#if 0
   while( ERR_get_error() != 0 ) { /* eat pending errors */ }
#endif

   if( pStream->blocking ? timeout >= 0 : timeout < 0 )
   {
      if( zh_socketSetBlockingIO( sd, timeout < 0 ) >= 0 )
         pStream->blocking = ! pStream->blocking;
   }

   timer = zh_timerInit( timeout );

   if( len > 0 )
   {
      iToRead = SSL_pending( pStream->ssl );
      if( iToRead <= 0 )
      {
         iToRead = timeout < 0 ? 1 : zh_socketSelectRead( sd, timeout );
         if( iToRead > 0 )
            iToRead = ( int ) len;
         else if( iToRead == 0 )
            zh_socketSetError( ZH_SOCKET_ERR_TIMEOUT );
      }
      else if( iToRead > len )
         iToRead = ( int ) len;
   }

   while( iToRead > 0 )
   {
      lRead = SSL_read( pStream->ssl, buffer, iToRead );
      if( lRead > 0 )
         zh_socketSetError( 0 );
      else
      {
         int iError = SSL_get_error( pStream->ssl, ( int ) lRead );
         switch( iError )
         {
            case SSL_ERROR_ZERO_RETURN:
               zh_socketSetError( ZH_SOCKET_ERR_PIPE );
               lRead = 0;
               break;
            case SSL_ERROR_WANT_READ:
            case SSL_ERROR_WANT_WRITE:
               if( zh_vmRequestQuery() == 0 )
               {
                  if( timeout > 0 )
                  {
                     if( ( timeout = zh_timerTest( timeout, &timer ) ) != 0 )
                     {
                        if( iError == SSL_ERROR_WANT_READ )
                           iError = zh_socketSelectRead( sd, timeout );
                        else
                           iError = zh_socketSelectWrite( sd, timeout );
                        if( iError > 0 )
                           continue;
                        else if( iError < 0 )
                           break;
                     }
                  }
                  zh_socketSetError( ZH_SOCKET_ERR_TIMEOUT );
                  break;
               }
               /* fallthrough */
            default:
               zh_socketSetError( ZH_SSL_SOCK_ERROR_BASE + iError );
         }
      }
      break;
   }

   return lRead;
}

long zh_ssl_socketWrite( PZH_SSLSTREAM pStream, ZH_SOCKET sd,
                         const void * buffer, long len, ZH_MAXINT timeout,
                         long * plast )
{
   long lWritten = 0, lWr = 0;
   ZH_MAXUINT timer;

   #if 0
   sd = SSL_get_wfd( pStream->ssl );
   #endif

#if LONG_MAX > INT_MAX
   if( len > INT_MAX )
      len = INT_MAX;
#endif

#if 0
   while( ERR_get_error() != 0 ) { /* eat pending errors */ }
#endif

   if( pStream->blocking ? timeout >= 0 : timeout < 0 )
   {
      if( zh_socketSetBlockingIO( sd, timeout < 0 ) >= 0 )
         pStream->blocking = ! pStream->blocking;
   }

   timer = zh_timerInit( timeout );

   while( len > 0 )
   {
      lWr = SSL_write( pStream->ssl, buffer, ( int ) len );

      if( plast )
         *plast = lWr;

      if( lWr > 0 )
      {
         lWritten += lWr;
         len -= lWr;
         buffer = ( const char * ) buffer + lWr;
         zh_socketSetError( 0 );
      }
      else
      {
         int iError = SSL_get_error( pStream->ssl, ( int ) lWr );
         switch( iError )
         {
            case SSL_ERROR_WANT_READ:
            case SSL_ERROR_WANT_WRITE:
               if( zh_vmRequestQuery() == 0 )
               {
                  if( timeout > 0 )
                  {
                     if( ( timeout = zh_timerTest( timeout, &timer ) ) != 0 )
                     {
                        if( iError == SSL_ERROR_WANT_READ )
                           iError = zh_socketSelectRead( sd, timeout );
                        else
                           iError = zh_socketSelectWrite( sd, timeout );
                        if( iError > 0 )
                           continue;
                     }
                     else
                        iError = 0;
                  }
                  else
                     iError = 0;
                  if( lWritten == 0 && iError == 0 )
                     zh_socketSetError( ZH_SOCKET_ERR_TIMEOUT );
                  break;
               }
               /* fallthrough */
            default:
               zh_socketSetError( ZH_SSL_SOCK_ERROR_BASE + iError );
         }
         break;
      }
   }

   return lWritten != 0 ? lWritten : lWr;
}

void zh_ssl_socketClose( PZH_SSLSTREAM pStream )
{
   SSL_shutdown( pStream->ssl );
   if( pStream->pSSL )
      zh_itemRelease( pStream->pSSL );
   else
      SSL_free( pStream->ssl );
   zh_xfree( pStream );
}

PZH_SSLSTREAM zh_ssl_socketNew( ZH_SOCKET sd, SSL * ssl, ZH_BOOL fServer,
                                ZH_MAXINT timeout, PZH_ITEM pSSL,
                                int * piResult )
{
   PZH_SSLSTREAM pStream;
   ZH_MAXUINT timer;
   int iResult;

   pStream = ( ZH_SSLSTREAM * ) zh_xgrabz( sizeof( ZH_SSLSTREAM ) );

   pStream->ssl = ssl;
   pStream->pSSL = pSSL ? zh_itemNew( pSSL ) : NULL;
   pStream->blocking = timeout < 0;
   if( zh_socketSetBlockingIO( sd, pStream->blocking ) < 0 )
      pStream->blocking = ! pStream->blocking;

   SSL_set_mode( ssl, ZH_SSL_MODE_AUTO_RETRY );
   iResult = SSL_set_fd( ssl, sd );  /* Truncates `sd` on win64. OpenSSL bug: https://rt.openssl.org/Ticket/Display.html?id=1928&user=guest&pass=guest */

   timer = zh_timerInit( timeout );

   while( iResult == 1 )
   {
      if( fServer )
         iResult = SSL_accept( ssl );
      else
         iResult = SSL_connect( ssl );

      if( iResult != 1 && zh_vmRequestQuery() == 0 )
      {
         int iError = SSL_get_error( ssl, iResult );
         if( iError == SSL_ERROR_WANT_READ ||
             iError == SSL_ERROR_WANT_WRITE )
         {
            if( timeout < 0 )
            {
               iResult = 1;
               continue;
            }
            else if( timeout > 0 )
            {
               if( ( timeout = zh_timerTest( timeout, &timer ) ) != 0 )
               {
                  if( iError == SSL_ERROR_WANT_READ )
                     iError = zh_socketSelectRead( sd, timeout );
                  else
                     iError = zh_socketSelectWrite( sd, timeout );
                  if( iError > 0 )
                  {
                     iResult = 1;
                     continue;
                  }
               }
            }
            zh_socketSetError( ZH_SOCKET_ERR_TIMEOUT );
         }
      }
      break;
   }

   if( iResult != 1 )
   {
      zh_ssl_socketClose( pStream );
      pStream = NULL;
   }
   else
      pStream->blocking = zh_socketSetBlockingIO( sd, ZH_FALSE ) < 0;

   if( piResult )
      *piResult = iResult;

   return pStream;
}

/* socket filter */

static SSL * s_SSL_itemGet( PZH_ITEM pItem, PZH_ITEM * pSSL, ZH_BOOL * pfFree )
{
   SSL * ssl = NULL;

   if( pItem )
   {
      PZH_ITEM pRelease = NULL;

      if( ZH_IS_EVALITEM( pItem ) )
         pItem = pRelease = zh_itemDo( pItem, 0 );

      ssl = zh_SSL_itemGet( pItem );
      if( ssl == NULL )
      {
         SSL_CTX * ssl_ctx = zh_SSL_CTX_itemGet( pItem );
         if( ssl_ctx )
         {
            ssl = SSL_new( ssl_ctx );
            if( pRelease )
               zh_itemRelease( pRelease );
            pItem = pRelease = NULL;
         }
      }
      if( ssl )
      {
         * pSSL = pItem;
         * pfFree = pRelease != NULL;
      }
      else if( pRelease )
         zh_itemRelease( pRelease );
   }
   return ssl;
}

#define ZH_SSLSOCK_GET( p )   ( ( PZH_SSLSTREAM ) p->cargo )
#define ZH_SSLSOCK_READAHEAD  0x40

static PZH_SOCKEX s_sockexNew( ZH_SOCKET sd, PZH_ITEM pParams )
{
   PZH_SOCKEX pSock;
   ZH_BOOL fServer = ZH_FALSE, fFree = ZH_FALSE;
   ZH_MAXINT timeout = -1;
   PZH_ITEM pSSL = NULL;
   SSL * ssl = NULL;

   if( pParams && ZH_IS_HASH( pParams ) )
   {
      PZH_ITEM pItem;

      if( ssl == NULL )
         ssl = s_SSL_itemGet( zh_hashGetCItemPtr( pParams, "ssl" ), &pSSL, &fFree );
      if( ssl == NULL )
         ssl = s_SSL_itemGet( zh_hashGetCItemPtr( pParams, "ctx" ), &pSSL, &fFree );
      if( ssl == NULL )
         ssl = s_SSL_itemGet( zh_hashGetCItemPtr( pParams, "key" ), &pSSL, &fFree );

      if( ( pItem = zh_hashGetCItemPtr( pParams, "timeout" ) ) != NULL &&
          ZH_IS_NUMERIC( pItem ) )
         timeout = zh_itemGetNInt( pItem );
      if( ( pItem = zh_hashGetCItemPtr( pParams, "server" ) ) != NULL &&
          ZH_IS_LOGICAL( pItem ) )
         fServer = zh_itemGetL( pItem );
      else if( ( pItem = zh_hashGetCItemPtr( pParams, "client" ) ) != NULL &&
               ZH_IS_LOGICAL( pItem ) )
         fServer = ! zh_itemGetL( pItem );
   }

   pSock = zh_sockexNewSSL( sd, ssl, fServer, timeout, pSSL );
   if( pSock )
      zh_socekxParamsInit( pSock, pParams );
   if( fFree )
      zh_itemRelease( pSSL );

   return pSock;
}

/* this wrapper does not support multilevel filtering so
   it destroys previous wrappers if any and create new one.
 */
static PZH_SOCKEX s_sockexNext( PZH_SOCKEX pSock, PZH_ITEM pParams )
{
   PZH_SOCKEX pSockNew = NULL;

   if( pSock && pSock->sd != ZH_NO_SOCKET )
   {
      pSockNew = s_sockexNew( pSock->sd, pParams );
      if( pSockNew )
         zh_sockexClose( pSock, ZH_FALSE );
   }

   return pSockNew;
}

static int s_sockexClose( PZH_SOCKEX pSock, ZH_BOOL fClose )
{
   int iResult;

   if( pSock->cargo )
      zh_ssl_socketClose( ZH_SSLSOCK_GET( pSock ) );

   iResult = zh_sockexRawClear( pSock, fClose );
   zh_xfree( pSock );

   return iResult;
}

static long s_sockexRead( PZH_SOCKEX pSock, void * data, long len, ZH_MAXINT timeout )
{
   long lRead = ZH_MIN( pSock->inbuffer, len );

   if( lRead > 0 )
   {
      memcpy( data, pSock->buffer + pSock->posbuffer, lRead );
      pSock->inbuffer -= lRead;
      if( pSock->inbuffer )
         pSock->posbuffer += lRead;
      else
         pSock->posbuffer = 0;
      return lRead;
   }
   else if( pSock->sd == ZH_NO_SOCKET )
   {
      zh_socketSetError( ZH_SOCKET_ERR_INVALIDHANDLE );
      return -1;
   }
   return zh_ssl_socketRead( ZH_SSLSOCK_GET( pSock ), pSock->sd, data, len, timeout );
}

static long s_sockexWrite( PZH_SOCKEX pSock, const void * data, long len, ZH_MAXINT timeout )
{
   if( pSock->sd == ZH_NO_SOCKET )
   {
      zh_socketSetError( ZH_SOCKET_ERR_INVALIDHANDLE );
      return -1;
   }
   return zh_ssl_socketWrite( ZH_SSLSOCK_GET( pSock ), pSock->sd, data, len, timeout, NULL );
}

static long s_sockexFlush( PZH_SOCKEX pSock, ZH_MAXINT timeout, ZH_BOOL fSync )
{
   ZH_SYMBOL_UNUSED( pSock );
   ZH_SYMBOL_UNUSED( timeout );
   ZH_SYMBOL_UNUSED( fSync );

   return 0;
}

static int s_sockexCanRead( PZH_SOCKEX pSock, ZH_BOOL fBuffer, ZH_MAXINT timeout )
{
   if( pSock->inbuffer )
      return 1;
   else if( pSock->sd == ZH_NO_SOCKET )
   {
      zh_socketSetError( ZH_SOCKET_ERR_INVALIDHANDLE );
      return -1;
   }
   else if( SSL_pending( ZH_SSLSOCK_GET( pSock )->ssl ) )
   {
      long len;

      if( pSock->buffer == NULL )
      {
         if( pSock->readahead <= 0 )
            pSock->readahead = ZH_SSLSOCK_READAHEAD;
         pSock->buffer = ( ZH_BYTE * ) zh_xgrab( pSock->readahead );
      }
      len = zh_ssl_socketRead( ZH_SSLSOCK_GET( pSock ), pSock->sd,
                               pSock->buffer, pSock->readahead, 0 );
      if( len > 0 )
      {
         pSock->inbuffer = len;
         len = 1;
      }
      return ( int ) len;
   }
   return fBuffer ? 0 : zh_socketSelectRead( pSock->sd, timeout );
}

static int s_sockexCanWrite( PZH_SOCKEX pSock, ZH_BOOL fBuffer, ZH_MAXINT timeout )
{
   if( pSock->sd == ZH_NO_SOCKET )
   {
      zh_socketSetError( ZH_SOCKET_ERR_INVALIDHANDLE );
      return -1;
   }
   return fBuffer ? 0 : zh_socketSelectWrite( pSock->sd, timeout );
}

static char * s_sockexName( PZH_SOCKEX pSock )
{
   return zh_strdup( pSock->pFilter->pszName );
}

static const char * s_sockexErrorStr( PZH_SOCKEX pSock, int iError )
{
   ZH_SYMBOL_UNUSED( pSock );

   return zh_ssl_socketErrorStr( iError );
}

static const ZH_SOCKET_FILTER s_sockFilter =
{
   "SSL",
   s_sockexNew,
   s_sockexNext,
   s_sockexClose,
   s_sockexRead,
   s_sockexWrite,
   s_sockexFlush,
   s_sockexCanRead,
   s_sockexCanWrite,
   s_sockexName,
   s_sockexErrorStr
};

PZH_SOCKEX zh_sockexNewSSL( ZH_SOCKET sd, SSL * ssl, ZH_BOOL fServer,
                            ZH_MAXINT timeout, PZH_ITEM pSSL )
{
   PZH_SOCKEX pSock = NULL;

   if( sd != ZH_NO_SOCKET && ssl )
   {
      PZH_SSLSTREAM pStream = zh_ssl_socketNew( sd, ssl, fServer, timeout, pSSL, NULL );
      if( pStream )
      {
         pSock = ( PZH_SOCKEX ) zh_xgrabz( sizeof( ZH_SOCKEX ) );
         pSock->sd = sd;
         pSock->fRedirAll = ZH_TRUE;
         pSock->fShutDown = ZH_TRUE;
         pSock->pFilter = &s_sockFilter;
         pSock->cargo = ( void * ) pStream;
      }
   }

   return pSock;
}

static void s_sslSocketNew( ZH_BOOL fServer )
{
   ZH_SOCKET sd = zh_socketParam( 1 );

   if( sd != ZH_NO_SOCKET )
   {
      PZH_SOCKEX pSock = NULL;
      SSL * ssl = zh_SSL_par( 2 );

      if( ssl )
         pSock = zh_sockexNewSSL( sd, ssl, fServer, zh_parnintdef( 3, - 1 ), zh_param( 2, ZH_IT_ANY ) );
      else if( ZH_ISHASH( 2 ) )
         pSock = zh_sockexNew( sd, s_sockFilter.pszName, zh_param( 2, ZH_IT_ANY ) );
      else
         zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );

      if( pSock )
      {
         PZH_ITEM pSockItm = zh_param( 1, ZH_IT_POINTER );

         if( ZH_ISBYREF( 1 ) && zh_sockexItemReplace( pSockItm, pSock ) )
            zh_itemReturn( pSockItm );
         else
         {
            zh_socketItemClear( pSockItm );
            zh_sockexItemPut( zh_param( -1, ZH_IT_ANY ), pSock );
         }
      }
   }
}

/* zh_socketNewSSL_connect( [@]<pSocket>, <pSSL> [, <nTimeout> ] ) */
ZH_FUNC( ZH_SOCKETNEWSSL_CONNECT )
{
   s_sslSocketNew( ZH_FALSE );
}

/* zh_socketNewSSL_accept( [@]<pSocket>, <pSSL> [, <nTimeout> ] ) */
ZH_FUNC( ZH_SOCKETNEWSSL_ACCEPT )
{
   s_sslSocketNew( ZH_TRUE );
}

ZH_CALL_ON_STARTUP_BEGIN( _zh_sslsock_init_ )
   zh_sockexRegister( &s_sockFilter );
ZH_CALL_ON_STARTUP_END( _zh_sslsock_init_ )

#if defined( ZH_PRAGMA_STARTUP )
   #pragma startup _zh_sslsock_init_
#elif defined( ZH_DATASEG_STARTUP )
   #define ZH_DATASEG_BODY  ZH_DATASEG_FUNC( _zh_sslsock_init_ )
   #include "zh_ini_seg.h"
#endif
