/*
 * The internet protocol / TCP support
 *
 * Copyright 2002 Giancarlo Niccolai <gian@niccolai.ws>
 *                Ron Pinkas [Ron@RonPinkas.com]
 *                Marcelo Lombardo [marcelo.lombardo@newage-software.com.br]
 * Copyright 2007 Przemyslaw Czerpak
 *    updated and ported to Ziher
 * Copyright 2008 Miguel Angel marchuet <miguelangel@marchuet.net>
 *    added dynamic system buffer
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

#include "zh_api.h"
#include "zh_item_api.h"
#include "../zh_socket.h"
#include "zh_error_api.h"
#include "zh_vm.h"
#include "zh_thread.h"
#include "zh_znet.h"

typedef struct
{
   ZH_SOCKET      sd;
   void *         remote;
   unsigned       remotelen;
   char *         buffer;
   long           inbuffer;
   long           posbuffer;
   long           readahead;
   int            iError;
   int            iCount;
   int            iTimeout;
   int            iTimeLimit;
   PZH_ITEM       pPeriodicBlock;
   PZH_ZNETSTREAM stream;
   ZH_INET_RDFUNC recvFunc;
   ZH_INET_WRFUNC sendFunc;
   ZH_INET_FLFUNC flushFunc;
   ZH_INET_CLFUNC cleanFunc;
   ZH_INET_ERFUNC errorFunc;
   ZH_INET_ESFUNC errstrFunc;
} ZH_SOCKET_STRUCT, * PZH_SOCKET_STRUCT;

#define ZH_INET_BUFFER_LEN  1500

#define ZH_INET_INITIALIZE()  if( s_initialize ) zh_inetAutoInit()

#define ZH_PARSOCKET( n )     ( ( PZH_SOCKET_STRUCT ) zh_parptrGC( &s_gcInetFuncs, n ) )

#define ZH_SOCKET_INIT( s, p ) \
   do \
   { \
      ZH_INET_INITIALIZE(); \
      s = ( PZH_SOCKET_STRUCT ) zh_gcAllocate( sizeof( *s ), &s_gcInetFuncs ); \
      memset( s, 0, sizeof( *s ) ); \
      s->sd         = ZH_NO_SOCKET; \
      s->readahead  = ZH_INET_BUFFER_LEN; \
      s->iTimeout   = -1; \
      s->iTimeLimit = -1; \
      s->iError     = ZH_INET_ERR_OK; \
      p = zh_itemPutPtrGC( p, s ); \
   } while( 0 )

static const char * const s_inetCRLF = "\r\n";

static ZH_COUNTER s_initialize = 1;

#if defined( ZH_OS_LINUX )
/* #define ZH_INET_LINUX_INTERRUPT     SIGUSR1 + 90 */
#  ifdef ZH_INET_LINUX_INTERRUPT
#     include <signal.h>

static void zh_inetLinuxSigusrHandle( int sig )
{
   /* nothing to do */
   ZH_SYMBOL_UNUSED( sig );
}
#  endif
#endif

static void zh_inetErrRT( void )
{
   zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

static ZH_BOOL zh_inetIsOpen( PZH_SOCKET_STRUCT socket )
{
   if( socket->sd == ZH_NO_SOCKET )
   {
      socket->iError = ZH_INET_ERR_CLOSEDSOCKET;
      return ZH_FALSE;
   }
   return ZH_TRUE;
}

static int s_inetGetError( PZH_SOCKET_STRUCT socket )
{
   int iError;

   iError = socket->errorFunc ? socket->errorFunc( socket->stream ) :
                                zh_socketGetError();

   if( iError == ZH_SOCKET_ERR_TIMEOUT )
      iError = ZH_INET_ERR_TIMEOUT;

   return iError;
}

static ZH_BOOL s_inetIsTimeout( PZH_SOCKET_STRUCT socket )
{
   return s_inetGetError( socket ) == ZH_INET_ERR_TIMEOUT;
}

static void zh_inetGetError( PZH_SOCKET_STRUCT socket )
{
   socket->iError = s_inetGetError( socket );
}

static void zh_inetCloseStream( PZH_SOCKET_STRUCT socket )
{
   if( socket->flushFunc && socket->sd != ZH_NO_SOCKET )
      socket->flushFunc( socket->stream, socket->sd,
                         ZH_MAX( socket->iTimeout, 10000 ), ZH_TRUE );

   if( socket->cleanFunc )
      socket->cleanFunc( socket->stream );

   socket->recvFunc = NULL;
   socket->sendFunc = NULL;
   socket->flushFunc = NULL;
   socket->cleanFunc = NULL;
   socket->stream = NULL;
}

static int zh_inetCloseSocket( PZH_SOCKET_STRUCT socket, ZH_BOOL fShutDown )
{
   int ret;

   zh_inetCloseStream( socket );

   if( fShutDown )
      zh_socketShutdown( socket->sd, ZH_SOCKET_SHUT_RDWR );

   ret = zh_socketClose( socket->sd );

   socket->sd       = ZH_NO_SOCKET;
   socket->inbuffer = 0;
   return ret;
}

static ZH_GARBAGE_FUNC( zh_inetSocketFinalize )
{
   PZH_SOCKET_STRUCT socket = ( PZH_SOCKET_STRUCT ) Cargo;

   if( socket->sd != ZH_NO_SOCKET )
      zh_inetCloseSocket( socket, ZH_TRUE );
   else
      zh_inetCloseStream( socket );

   if( socket->pPeriodicBlock )
   {
      zh_itemRelease( socket->pPeriodicBlock );
      socket->pPeriodicBlock = NULL;
   }
   if( socket->remote )
   {
      zh_xfree( socket->remote );
      socket->remote = NULL;
   }
   if( socket->buffer )
   {
      zh_xfree( socket->buffer );
      socket->buffer = NULL;
   }
}

static ZH_GARBAGE_FUNC( zh_inetSocketMark )
{
   PZH_SOCKET_STRUCT socket = ( PZH_SOCKET_STRUCT ) Cargo;

   if( socket->pPeriodicBlock )
      zh_gcMark( socket->pPeriodicBlock );
}

static const ZH_GC_FUNCS s_gcInetFuncs =
{
   zh_inetSocketFinalize,
   zh_inetSocketMark
};

/* Socket Initialization */

static void zh_inetAutoInit( void )
{
   if( s_initialize )
   {
      if( zh_atomic_dec( &s_initialize ) )
      {
         zh_socketInit();
#if defined( ZH_INET_LINUX_INTERRUPT )
         signal( ZH_INET_LINUX_INTERRUPT, zh_inetLinuxSigusrHandle );
#endif
      }
   }
}

ZH_SOCKET zh_znetInetFD( PZH_ITEM pItem, ZH_BOOL fError )
{
   PZH_SOCKET_STRUCT socket = ( PZH_SOCKET_STRUCT ) zh_itemGetPtrGC( pItem, &s_gcInetFuncs );

   if( socket )
      return socket->sd;
   else if( fError )
      zh_inetErrRT();

   return ZH_NO_SOCKET;
}

ZH_MAXINT zh_znetInetTimeout( PZH_ITEM pItem, ZH_BOOL fError )
{
   PZH_SOCKET_STRUCT socket = ( PZH_SOCKET_STRUCT ) zh_itemGetPtrGC( pItem, &s_gcInetFuncs );

   if( socket )
      return socket->iTimeout; /* socket->pPeriodicBlock ? socket->iTimeLimit */
   else if( fError )
      zh_inetErrRT();

   return -1;
}

ZH_BOOL zh_znetInetInitialize( PZH_ITEM pItem, PZH_ZNETSTREAM pStream,
                               ZH_INET_RDFUNC recvFunc,
                               ZH_INET_WRFUNC sendFunc,
                               ZH_INET_FLFUNC flushFunc,
                               ZH_INET_CLFUNC cleanFunc,
                               ZH_INET_ERFUNC errorFunc,
                               ZH_INET_ESFUNC errstrFunc )
{
   PZH_SOCKET_STRUCT socket = ( PZH_SOCKET_STRUCT ) zh_itemGetPtrGC( pItem, &s_gcInetFuncs );

   if( socket )
   {
      zh_inetCloseStream( socket );

      socket->recvFunc = recvFunc;
      socket->sendFunc = sendFunc;
      socket->flushFunc = flushFunc;
      socket->cleanFunc = cleanFunc;
      socket->errorFunc = errorFunc;
      socket->errstrFunc = errstrFunc;
      socket->stream = pStream;
      return ZH_TRUE;
   }

   zh_inetErrRT();
   return ZH_FALSE;
}

ZH_FUNC( ZH_INETINIT )
{
   int ret;

   zh_atomic_set( &s_initialize, 0 );
   ret = zh_socketInit();
   if( ret == 0 )
   {
#if defined( ZH_INET_LINUX_INTERRUPT )
      signal( ZH_INET_LINUX_INTERRUPT, zh_inetLinuxSigusrHandle );
#endif
   }
   zh_retl( ret == 0 );
}

ZH_FUNC( ZH_INETCLEANUP )
{
   zh_socketCleanup();
}

/* Socket Creation and destruction */

ZH_FUNC( ZH_INETCREATE )
{
   PZH_ITEM pSocket = NULL;
   PZH_SOCKET_STRUCT socket;

   ZH_SOCKET_INIT( socket, pSocket );

   if( ZH_IS_PARAM_NUM( 1 ) )
      socket->iTimeout = zh_parni( 1 );

   zh_itemReturnRelease( pSocket );
}

ZH_FUNC( ZH_INETCLOSE )
{
   PZH_SOCKET_STRUCT socket = ZH_PARSOCKET( 1 );

   if( socket )
   {
      if( socket->sd != ZH_NO_SOCKET )
      {
         zh_retni( zh_inetCloseSocket( socket, ZH_TRUE ) );
#ifdef ZH_INET_LINUX_INTERRUPT
         kill( 0, ZH_INET_LINUX_INTERRUPT );
#endif
      }
      else
         zh_retni( -1 );
   }
   else
      zh_inetErrRT();
}

ZH_FUNC( ZH_INETFD )
{
   PZH_SOCKET_STRUCT socket = ZH_PARSOCKET( 1 );

   if( socket )
   {
      zh_retnint( socket->sd );

      if( zh_parl( 2 ) )
         socket->sd = ZH_NO_SOCKET;
   }
   else
      zh_inetErrRT();
}

/* Socket data access & management */

ZH_FUNC( ZH_INETSTATUS )
{
   PZH_SOCKET_STRUCT socket = ZH_PARSOCKET( 1 );

   if( socket )
      zh_retni( socket->sd == ZH_NO_SOCKET ? -1 : 1 );  /* TODO: zh_retni( socket->status ); */
   else
      zh_inetErrRT();
}

/* Prepared, but still not used; being in wait for comments */
#if 0
ZH_FUNC( ZH_INETSTATUSDESC )
{
   PZH_SOCKET_STRUCT socket = ZH_PARSOCKET( 1 );

   if( socket )
   {
      switch( socket->status )
      {
         case 0:  zh_retc_const( "Connection not opened" ); return;
         case 1:  zh_retc_const( "Connection alive" ); return;
         case 2:  zh_retc_const( "Last operation error" ); return;
         case 3:  zh_retc_const( "Last operation timeout" ); return;
         default: zh_retc_const( "unknown" );
      }
   }
   else
      zh_inetErrRT();
}
#endif

ZH_FUNC( ZH_INETERRORCODE )
{
   PZH_SOCKET_STRUCT socket = ZH_PARSOCKET( 1 );

   if( socket )
      zh_retni( socket->iError );
   else
      zh_inetErrRT();
}

ZH_FUNC( ZH_INETERRORDESC )
{
   PZH_SOCKET_STRUCT socket = ZH_PARSOCKET( 1 );

   if( socket )
   {
      switch( socket->iError )
      {
         case ZH_INET_ERR_OK           : zh_retc_null(); return;
         case ZH_INET_ERR_TIMEOUT      : zh_retc_const( "Timeout" ); return;
         case ZH_INET_ERR_CLOSEDCONN   : zh_retc_const( "Connection closed" ); return;
         case ZH_INET_ERR_CLOSEDSOCKET : zh_retc_const( "Closed socket" ); return;
         case ZH_INET_ERR_BUFFOVERRUN  : zh_retc_const( "Buffer overrun" ); return;
         default:
            if( socket->errstrFunc )
               zh_retc( socket->errstrFunc( socket->stream, socket->iError ) );
            else
               zh_retc( zh_socketErrorStr( socket->iError ) );
      }
   }
   else
      zh_inetErrRT();
}

ZH_FUNC( ZH_INETCLEARERROR )
{
   PZH_SOCKET_STRUCT socket = ZH_PARSOCKET( 1 );

   if( socket )
      socket->iError = ZH_INET_ERR_OK;
   else
      zh_inetErrRT();
}

ZH_FUNC( ZH_INETCOUNT )
{
   PZH_SOCKET_STRUCT socket = ZH_PARSOCKET( 1 );

   if( socket )
      zh_retni( socket->iCount );
   else
      zh_inetErrRT();
}

ZH_FUNC( ZH_INETADDRESS )
{
   PZH_SOCKET_STRUCT socket = ZH_PARSOCKET( 1 );

   if( socket )
   {
      char * szAddr = socket->remote ?
            zh_socketAddrGetName( socket->remote, socket->remotelen ) : NULL;
      if( szAddr )
         zh_retc_buffer( szAddr );
      else
         zh_retc_null();
   }
   else
      zh_inetErrRT();
}

ZH_FUNC( ZH_INETPORT )
{
   PZH_SOCKET_STRUCT socket = ZH_PARSOCKET( 1 );

   if( socket )
      zh_retni( socket->remote ?
                zh_socketAddrGetPort( socket->remote, socket->remotelen ) : 0 );
   else
      zh_inetErrRT();
}

ZH_FUNC( ZH_INETTIMEOUT )
{
   PZH_SOCKET_STRUCT socket = ZH_PARSOCKET( 1 );

   if( socket )
   {
      zh_retni( socket->iTimeout );

      if( ZH_IS_PARAM_NUM( 2 ) )
         socket->iTimeout = zh_parni( 2 );
   }
   else
      zh_inetErrRT();
}

ZH_FUNC( ZH_INETCLEARTIMEOUT )
{
   PZH_SOCKET_STRUCT socket = ZH_PARSOCKET( 1 );

   if( socket )
      socket->iTimeout = -1;
   else
      zh_inetErrRT();
}

ZH_FUNC( ZH_INETTIMELIMIT )
{
   PZH_SOCKET_STRUCT socket = ZH_PARSOCKET( 1 );

   if( socket )
   {
      zh_retni( socket->iTimeLimit );

      if( ZH_IS_PARAM_NUM( 2 ) )
         socket->iTimeLimit = zh_parni( 2 );
   }
   else
      zh_inetErrRT();
}

ZH_FUNC( ZH_INETCLEARTIMELIMIT )
{
   PZH_SOCKET_STRUCT socket = ZH_PARSOCKET( 1 );

   if( socket )
      socket->iTimeLimit = -1;
   else
      zh_inetErrRT();
}

ZH_FUNC( ZH_INETPERIODCALLBACK )
{
   PZH_SOCKET_STRUCT socket = ZH_PARSOCKET( 1 );

   if( socket )
   {
      PZH_ITEM pExec = zh_param( 2, ZH_IT_ARRAY | ZH_IT_EVALITEM );

      if( socket->pPeriodicBlock )
         zh_itemReturn( socket->pPeriodicBlock );

      if( pExec )
      {
         if( socket->pPeriodicBlock )
            zh_itemRelease( socket->pPeriodicBlock );
         socket->pPeriodicBlock = zh_itemClone( pExec );
         zh_gcUnlock( socket->pPeriodicBlock );
      }
   }
   else
      zh_inetErrRT();
}

ZH_FUNC( ZH_INETCLEARPERIODCALLBACK )
{
   PZH_SOCKET_STRUCT socket = ZH_PARSOCKET( 1 );

   if( socket )
   {
      if( socket->pPeriodicBlock )
      {
         zh_itemRelease( socket->pPeriodicBlock );
         socket->pPeriodicBlock = NULL;
      }
   }
   else
      zh_inetErrRT();
}

ZH_FUNC( ZH_INETGETSNDBUFSIZE )
{
   PZH_SOCKET_STRUCT socket = ZH_PARSOCKET( 1 );

   if( socket )
   {
      int iSize = -1;
      if( zh_inetIsOpen( socket ) )
      {
         if( zh_socketGetSndBufSize( socket->sd, &iSize ) != 0 )
            iSize = -1;
      }
      zh_retni( iSize );
   }
   else
      zh_inetErrRT();
}

ZH_FUNC( ZH_INETGETRCVBUFSIZE )
{
   PZH_SOCKET_STRUCT socket = ZH_PARSOCKET( 1 );

   if( socket )
   {
      int iSize = -1;
      if( zh_inetIsOpen( socket ) )
      {
         if( zh_socketGetRcvBufSize( socket->sd, &iSize ) != 0 )
            iSize = -1;
      }
      zh_retni( iSize );
   }
   else
      zh_inetErrRT();
}

ZH_FUNC( ZH_INETSETSNDBUFSIZE )
{
   PZH_SOCKET_STRUCT socket = ZH_PARSOCKET( 1 );

   if( socket )
   {
      int iSize = -1;
      if( zh_inetIsOpen( socket ) )
      {
         iSize = zh_parni( 2 );
         zh_socketSetSndBufSize( socket->sd, iSize );
      }
      zh_retni( iSize );
   }
   else
      zh_inetErrRT();
}

ZH_FUNC( ZH_INETSETRCVBUFSIZE )
{
   PZH_SOCKET_STRUCT socket = ZH_PARSOCKET( 1 );

   if( socket )
   {
      int iSize = -1;
      if( zh_inetIsOpen( socket ) )
      {
         iSize = zh_parni( 2 );
         zh_socketSetRcvBufSize( socket->sd, iSize );
      }
      zh_retni( iSize );
   }
   else
      zh_inetErrRT();
}



/* TCP receive and send functions */

static long s_inetRecv( PZH_SOCKET_STRUCT socket, char * buffer, long size,
                        ZH_BOOL readahead, ZH_MAXINT timeout )
{
   long rec = 0;

   if( readahead && socket->inbuffer == 0 && socket->readahead > size )
   {
      if( socket->buffer == NULL )
         socket->buffer = ( char * ) zh_xgrab( socket->readahead );
      socket->posbuffer = 0;
      if( socket->recvFunc )
         rec = socket->recvFunc( socket->stream, socket->sd,
                                 socket->buffer, socket->readahead,
                                 timeout );
      else
         rec = zh_socketRecv( socket->sd, socket->buffer, socket->readahead,
                              0, timeout );
      socket->inbuffer = ZH_MAX( 0, rec );
   }
   else
      readahead = ZH_FALSE;

   if( socket->inbuffer > 0 )
   {
      rec = ZH_MIN( size, socket->inbuffer );
      memcpy( buffer, socket->buffer + socket->posbuffer, rec );
      socket->posbuffer += rec;
      socket->inbuffer -= rec;
   }

   if( size > rec && ! readahead )
   {
      if( socket->recvFunc )
         size = socket->recvFunc( socket->stream, socket->sd,
                                  buffer + rec, size - rec,
                                  rec ? 0 : timeout );
      else
         size = zh_socketRecv( socket->sd, buffer + rec, size - rec, 0,
                               rec ? 0 : timeout );

      if( rec == 0 )
         rec = size;
      else if( size > 0 )
         rec += size;
   }

   return rec;
}

static void s_inetRecvInternal( int iMode )
{
   PZH_SOCKET_STRUCT socket = ZH_PARSOCKET( 1 );
   PZH_ITEM pBuffer = zh_param( 2, ZH_IT_STRING );

   if( socket == NULL || pBuffer == NULL || ! ZH_ISBYREF( 2 ) )
      zh_inetErrRT();
   else if( ! zh_inetIsOpen( socket ) )
      zh_retni( -1 );
   else
   {
      int iLen, iMaxLen, iReceived, iTimeElapsed;
      char * buffer;
      ZH_SIZE nLen;

      if( zh_itemGetWriteCL( pBuffer, &buffer, &nLen ) )
         iLen = ( int ) nLen;
      else
      {
         iLen = 0;
         buffer = NULL;
      }

      if( ZH_IS_PARAM_NUM( 3 ) )
      {
         iMaxLen = zh_parni( 3 );
         if( iMaxLen < 0 )
            iMaxLen = 0;
         else if( iLen < iMaxLen )
            iMaxLen = iLen;
      }
      else
         iMaxLen = iLen;

      iReceived = iTimeElapsed = 0;
      socket->iError = ZH_INET_ERR_OK;
      do
      {
         iLen = s_inetRecv( socket, buffer + iReceived, iMaxLen - iReceived,
                            ZH_FALSE, socket->iTimeout );
         if( iLen >= 0 )
         {
            iReceived += iLen;
            if( iMode == 0 ) /* Called from zh_inetRecv()? */
               break;
         }
         else if( iLen == -1 && s_inetIsTimeout( socket ) )
         {
            /* if we have a pPeriodicBlock, timeLimit is our REAL timeout */
            if( socket->pPeriodicBlock )
            {
               /* timed out; let's see if we have to run a cb routine */
               iTimeElapsed += socket->iTimeout;
               zh_execFromArray( socket->pPeriodicBlock );
               /* do we continue? */
               if( zh_parl( -1 ) && zh_vmRequestQuery() == 0 &&
                   ( socket->iTimeLimit == -1 || iTimeElapsed < socket->iTimeLimit ) )
                  iLen = 1;   /* Declare success to continue loop */
            }
         }
      }
      while( iReceived < iMaxLen && iLen > 0 );

      socket->iCount = iReceived;

      if( iLen == 0 )
         socket->iError = ZH_INET_ERR_CLOSEDCONN;
      else if( iLen < 0 )
         zh_inetGetError( socket );

      zh_retni( iReceived > 0 ? iReceived : iLen );
   }
}

ZH_FUNC( ZH_INETRECV )
{
   s_inetRecvInternal( 0 );
}


ZH_FUNC( ZH_INETRECVALL )
{
   s_inetRecvInternal( 1 );
}


static void s_inetRecvPattern( const char * const * patterns, int * patternsizes,
                               int iPatternsCount, int iParam )
{
   PZH_SOCKET_STRUCT socket = ZH_PARSOCKET( 1 );
   PZH_ITEM pResult         = zh_param( iParam, ZH_IT_BYREF );
   PZH_ITEM pMaxSize        = zh_param( iParam + 1, ZH_IT_NUMERIC );
   PZH_ITEM pBufferSize     = zh_param( iParam + 2, ZH_IT_NUMERIC );

   char cChar = '\0';
   char * buffer;
   int iPaternFound = 0;
   int iTimeElapsed = 0;
   int iPos = 0;
   int iLen;
   int iAllocated, iBufferSize, iMax;
   int i;

   if( socket == NULL )
   {
      zh_inetErrRT();
      return;
   }
   else if( ! zh_inetIsOpen( socket ) )
   {
      if( pResult )
         zh_itemPutNI( pResult, -1 );
      zh_retc_null();
      return;
   }


   iBufferSize = pBufferSize ? zh_itemGetNI( pBufferSize ) : 80;
   iMax = pMaxSize ? zh_itemGetNI( pMaxSize ) : 0;

   socket->iError = ZH_INET_ERR_OK;

   buffer = ( char * ) zh_xgrab( iBufferSize );
   iAllocated = iBufferSize;

   do
   {
      if( iPos == iAllocated - 1 )
      {
         iAllocated += iBufferSize;
         buffer = ( char * ) zh_xrealloc( buffer, iAllocated );
      }

      iLen = s_inetRecv( socket, &cChar, 1, ZH_TRUE, socket->iTimeout );
      if( iLen == -1 && s_inetIsTimeout( socket ) )
      {
         iLen = -2;     /* this signals timeout */
         if( socket->pPeriodicBlock )
         {
            ZH_BOOL fResult;

            iTimeElapsed += socket->iTimeout;
            zh_execFromArray( socket->pPeriodicBlock );
            fResult = zh_parl( -1 ) && zh_vmRequestQuery() == 0;

            if( fResult &&
                ( socket->iTimeLimit == -1 || iTimeElapsed < socket->iTimeLimit ) )
               iLen = 1;
         }
      }
      else if( iLen > 0 )
      {
         buffer[ iPos++ ] = cChar;
         for( i = 0; i < iPatternsCount; ++i )
         {
            if( patternsizes[ i ] <= iPos &&
                cChar == patterns[ i ][ patternsizes[ i ] - 1 ] )
            {
               if( memcmp( buffer + iPos - patternsizes[ i ],
                           patterns[ i ], patternsizes[ i ] ) == 0 )
               {
                  iPaternFound = i + 1;
                  break;
               }
            }
         }
      }
   }
   while( iLen > 0 && iPaternFound == 0 && ( iMax == 0 || iPos < iMax ) );

   if( iPaternFound )
   {
      socket->iCount = iPos;
      if( pResult )
         zh_itemPutNI( pResult, iPos );
      zh_retclen_buffer( buffer, iPos - patternsizes[ iPaternFound - 1 ] );
   }
   else
   {
      if( iLen == 0 )
         socket->iError = ZH_INET_ERR_CLOSEDCONN;
      else if( iLen < 0 )
         zh_inetGetError( socket );
      else
      {
         socket->iError = ZH_INET_ERR_BUFFOVERRUN;
         iLen = -1;
      }
      if( pResult )
         zh_itemPutNI( pResult, iLen );
      zh_xfree( buffer );
      zh_retc_null();
   }
}

ZH_FUNC( ZH_INETRECVLINE )
{
   int iEolLen = ( int ) strlen( s_inetCRLF );

   s_inetRecvPattern( &s_inetCRLF, &iEolLen, 1, 2 );
}

#define ZH_PATERN_BUF_SIZE  16

ZH_FUNC( ZH_INETRECVENDBLOCK )
{
   PZH_ITEM pProto = zh_param( 2, ZH_IT_ARRAY | ZH_IT_STRING );
   const char * patterns_buf[ ZH_PATERN_BUF_SIZE ];
   const char ** patterns = patterns_buf;
   int patternsizes_buf[ ZH_PATERN_BUF_SIZE ];
   int * patternsizes = patternsizes_buf;
   int iPatternsCount = 0;
   int iLen;

   if( pProto && ZH_IS_ARRAY( pProto ) )
   {
      int iPatternsMax = ( int ) zh_arrayLen( pProto ), i;

      for( i = 1; i <= iPatternsMax; i++ )
      {
         iLen = ( int ) zh_arrayGetCLen( pProto, i );
         if( iLen > 0 )
            ++iPatternsCount;
      }
      if( iPatternsCount > 0 )
      {
         if( iPatternsCount > ZH_PATERN_BUF_SIZE )
         {
            patterns = ( const char ** ) zh_xgrab( sizeof( char * ) * iPatternsCount );
            patternsizes = ( int * ) zh_xgrab( sizeof( int ) * iPatternsCount );
         }
         iPatternsCount = 0;
         for( i = 1; i <= iPatternsMax; i++ )
         {
            iLen = ( int ) zh_arrayGetCLen( pProto, i );
            if( iLen > 0 )
            {
               patterns[ iPatternsCount ]     = zh_arrayGetCPtr( pProto, i );
               patternsizes[ iPatternsCount ] = iLen;
               ++iPatternsCount;
            }
         }
      }
   }

   if( iPatternsCount == 0 )
   {
      iLen = ( int ) zh_itemGetCLen( pProto );
      if( iLen > 0 )
      {
         patterns[ 0 ]     = zh_itemGetCPtr( pProto );
         patternsizes[ 0 ] = iLen;
      }
      else
      {
         patterns[ 0 ]     = s_inetCRLF;
         patternsizes[ 0 ] = ( int ) strlen( s_inetCRLF );
      }
      iPatternsCount = 1;
   }

   s_inetRecvPattern( patterns, patternsizes, iPatternsCount, 3 );

   if( iPatternsCount > ZH_PATERN_BUF_SIZE )
   {
      zh_xfree( ( void * ) patterns );
      zh_xfree( patternsizes );
   }
}


ZH_FUNC( ZH_INETDATAREADY )
{
   PZH_SOCKET_STRUCT socket = ZH_PARSOCKET( 1 );

   if( socket == NULL || ( zh_pcount() >= 2 && ! ZH_IS_PARAM_NUM( 2 ) ) )
      zh_inetErrRT();
   else if( ! zh_inetIsOpen( socket ) )
      zh_retni( -1 );
   else
   {
      int iVal;

      socket->iError = ZH_INET_ERR_OK;
      if( socket->inbuffer > 0 )
         iVal = 1;
      else
      {
         ZH_MAXINT timeout = zh_parnint( 2 );   /* default to 0 */

         if( socket->readahead > 0 && socket->recvFunc )
         {
            char buffer[ 1 ];

            iVal = ( int ) s_inetRecv( socket, buffer, 1, ZH_TRUE, timeout );
            if( iVal == 1 )
            {
               socket->posbuffer--;
               socket->inbuffer++;
            }
         }
         else
            iVal = zh_socketSelectRead( socket->sd, timeout );

         if( iVal < 0 )
            zh_inetGetError( socket );
      }
      zh_retni( iVal );
   }
}


static void s_inetSendInternal( ZH_BOOL lAll )
{
   PZH_SOCKET_STRUCT socket = ZH_PARSOCKET( 1 );
   PZH_ITEM pBuffer = zh_param( 2, ZH_IT_STRING );
   const char * buffer;
   int iLen, iSent, iSend;
   long lLastSnd = 1;

   if( socket == NULL || pBuffer == NULL )
      zh_inetErrRT();
   else if( ! zh_inetIsOpen( socket ) )
      zh_retni( -1 );
   else
   {
      buffer = zh_itemGetCPtr( pBuffer );
      iSend = ( int ) zh_itemGetCLen( pBuffer );
      if( ZH_IS_PARAM_NUM( 3 ) )
      {
         iLen = zh_parni( 3 );
         if( iLen < iSend )
            iSend = iLen;
      }

      socket->iError = ZH_INET_ERR_OK;

      iSent = iLen = 0;
      while( iSent < iSend )
      {
         if( socket->sendFunc )
         {
            iLen = socket->sendFunc( socket->stream, socket->sd,
                                     buffer + iSent, iSend - iSent,
                                     socket->iTimeout, &lLastSnd );
            if( lLastSnd <= 0 && iLen > 0 )
            {
               iSent += iLen;
               iLen = ( int ) lLastSnd;
            }
         }
         else
            iLen = zh_socketSend( socket->sd, buffer + iSent, iSend - iSent,
                                  0, socket->iTimeout );
         if( iLen > 0 )
         {
            iSent += iLen;
            if( ! lAll )
               break;
         }
         else
         {
            zh_inetGetError( socket );
            break;
         }
      }
      socket->iCount = iSent;

      if( socket->flushFunc && ( lLastSnd > 0 || ( lLastSnd == -1 &&
             socket->iTimeout >= 0 && socket->iTimeout < 10000 &&
             s_inetIsTimeout( socket ) ) ) )
      {
         /* TODO: safe information about unflushed data and try to call
                  flush before entering receive wait sate */
         socket->flushFunc( socket->stream, socket->sd, socket->iTimeout < 0 ?
                            socket->iTimeout : ZH_MAX( socket->iTimeout, 10000 ),
                            ZH_FALSE );
      }

      zh_retni( iSent > 0 ? iSent : iLen );
   }
}

ZH_FUNC( ZH_INETSEND )
{
   s_inetSendInternal( ZH_FALSE );
}

ZH_FUNC( ZH_INETSENDALL )
{
   s_inetSendInternal( ZH_TRUE );
}


/* Name resolution interface functions */

ZH_FUNC( ZH_INETGETHOSTS )
{
   const char * szHost = zh_parc( 1 );

   if( szHost )
   {
      PZH_ITEM pHosts;

      ZH_INET_INITIALIZE();
      pHosts = zh_socketGetHosts( szHost, ZH_SOCKET_PF_INET );
      if( pHosts )
         zh_itemReturnRelease( pHosts );
      else
         zh_reta( 0 );
   }
   else
      zh_inetErrRT();
}

ZH_FUNC( ZH_INETGETALIAS )
{
   const char * szHost = zh_parc( 1 );

   if( szHost )
   {
      PZH_ITEM pHosts;

      ZH_INET_INITIALIZE();
      pHosts = zh_socketGetAliases( szHost, ZH_SOCKET_PF_INET );
      if( pHosts )
         zh_itemReturnRelease( pHosts );
      else
         zh_reta( 0 );
   }
   else
      zh_inetErrRT();
}


/* Interface information function */

ZH_FUNC( ZH_INETIFINFO )
{
   PZH_ITEM pInfo;

   ZH_INET_INITIALIZE();
   pInfo = zh_socketGetIFaces( zh_parnidef( 2, ZH_SOCKET_PF_INET ),
                               zh_parl( 1 ) );
   if( pInfo )
      zh_itemReturnRelease( pInfo );
   else
      zh_reta( 0 );
}

/* Server Specific functions */

static int s_inetBind( PZH_SOCKET_STRUCT socket, const void * pSockAddr, unsigned uiLen )
{
#if ! defined( ZH_OS_WIN )
   zh_socketSetReuseAddr( socket->sd, ZH_TRUE );
#endif
   return zh_socketBind( socket->sd, pSockAddr, uiLen );
}

ZH_FUNC( ZH_INETSERVER )
{
   PZH_SOCKET_STRUCT socket = ZH_PARSOCKET( 2 );
   PZH_ITEM pSocket = NULL;

   if( ! ZH_IS_PARAM_NUM( 1 ) || ( socket == NULL && ! ZH_ISNIL( 2 ) ) )
   {
      zh_inetErrRT();
      return;
   }

   if( ! socket )
      ZH_SOCKET_INIT( socket, pSocket );
   else if( socket->sd != ZH_NO_SOCKET )
      zh_inetCloseSocket( socket, ZH_FALSE );
   socket->sd = zh_socketOpen( ZH_SOCKET_PF_INET, ZH_SOCKET_PT_STREAM, 0 );
   if( socket->sd == ZH_NO_SOCKET )
      zh_inetGetError( socket );
   else
   {
      int iPort = zh_parni( 1 );
      const char * szAddress = zh_parc( 3 );
      int iListen = zh_parnidef( 4, 10 );

      if( socket->remote )
         zh_xfree( socket->remote );
      if( ! zh_socketInetAddr( &socket->remote, &socket->remotelen, szAddress, iPort ) ||
          s_inetBind( socket, socket->remote, socket->remotelen ) != 0 ||
          zh_socketListen( socket->sd, iListen ) != 0 )
      {
         zh_inetGetError( socket );
         zh_inetCloseSocket( socket, ZH_FALSE );
      }
      else
         socket->iError = ZH_INET_ERR_OK;
   }
   if( pSocket )
      zh_itemReturnRelease( pSocket );
   else
      zh_itemReturn( zh_param( 2, ZH_IT_ANY ) );
}

ZH_FUNC( ZH_INETACCEPT )
{
   PZH_SOCKET_STRUCT socket = ZH_PARSOCKET( 1 );

   if( socket == NULL )
      zh_inetErrRT();
   else if( zh_inetIsOpen( socket ) )
   {
      do
      {
         void * sa;
         unsigned len;
         ZH_SOCKET incoming = zh_socketAccept( socket->sd, &sa, &len, socket->iTimeout );

         if( incoming == ZH_NO_SOCKET )
            zh_inetGetError( socket );
         else
         {
            PZH_SOCKET_STRUCT new_socket;
            PZH_ITEM pSocket = NULL;
            ZH_SOCKET_INIT( new_socket, pSocket );
            new_socket->remote = sa;
            new_socket->remotelen = len;
            new_socket->sd = incoming;
            zh_itemReturnRelease( pSocket );
            socket->iError = ZH_INET_ERR_OK;
            break;
         }
      }
      while( socket->iError == ZH_SOCKET_ERR_AGAIN &&
             zh_vmRequestQuery() == 0 );
   }
}

/* Client specific (connection functions) */

static void zh_inetConnectInternal( ZH_BOOL fResolve )
{
   const char * szHost = zh_parc( 1 );
   char * szAddr = NULL;
   PZH_SOCKET_STRUCT socket = ZH_PARSOCKET( 3 );
   int iPort = zh_parni( 2 );

   if( szHost == NULL || iPort == 0 || ( socket == NULL && ! ZH_ISNIL( 3 ) ) )
      zh_inetErrRT();
   else
   {
      PZH_ITEM pSocket = NULL;

      if( ! socket )
         ZH_SOCKET_INIT( socket, pSocket );
      else if( socket->sd != ZH_NO_SOCKET )
         zh_inetCloseSocket( socket, ZH_FALSE );

      if( fResolve )
         szHost = szAddr = zh_socketResolveAddr( szHost, ZH_SOCKET_AF_INET );

      if( fResolve && ! szAddr )
      {
         zh_inetGetError( socket );
         if( socket->iError == 0 )
            socket->iError = ZH_SOCKET_ERR_WRONGADDR;
      }
      else
      {
         /* Creates comm socket */
         socket->sd = zh_socketOpen( ZH_SOCKET_PF_INET, ZH_SOCKET_PT_STREAM, 0 );
         if( socket->sd == ZH_NO_SOCKET )
            zh_inetGetError( socket );
         else
         {
            if( socket->remote )
               zh_xfree( socket->remote );
            if( zh_socketInetAddr( &socket->remote, &socket->remotelen,
                                   szHost, iPort ) )
            {
               zh_socketSetKeepAlive( socket->sd, ZH_TRUE );
               if( zh_socketConnect( socket->sd, socket->remote, socket->remotelen,
                                     socket->iTimeout ) != 0 )
                  zh_inetGetError( socket );
               else
                  socket->iError = ZH_INET_ERR_OK;
            }
            else
               zh_inetGetError( socket );
         }
         if( szAddr )
            zh_xfree( szAddr );
      }
      if( pSocket )
         zh_itemReturnRelease( pSocket );
      else
         zh_itemReturn( zh_param( 3, ZH_IT_ANY ) );
   }
}

ZH_FUNC( ZH_INETCONNECT )
{
   zh_inetConnectInternal( ZH_TRUE );
}

ZH_FUNC( ZH_INETCONNECTIP )
{
   zh_inetConnectInternal( ZH_FALSE );
}

/* Datagram functions */

ZH_FUNC( ZH_INETDGRAMBIND )
{
   PZH_SOCKET_STRUCT socket;
   PZH_ITEM pSocket = NULL;
   int iPort = zh_parni( 1 );
   const char * szAddress;

   /* Parameter error checking */
   if( iPort == 0 || ( zh_pcount() >= 4 && ! ZH_ISCHAR( 4 ) ) )
   {
      zh_inetErrRT();
      return;
   }

   ZH_SOCKET_INIT( socket, pSocket );

   /* Creates comm socket */
   socket->sd = zh_socketOpen( ZH_SOCKET_PF_INET, ZH_SOCKET_PT_DGRAM, ZH_SOCKET_IPPROTO_UDP );
   if( socket->sd == ZH_NO_SOCKET )
   {
      zh_inetGetError( socket );
      zh_itemReturnRelease( pSocket );
      return;
   }

   /* Setting broadcast if needed. */
   if( zh_parl( 3 ) )
      zh_socketSetBroadcast( socket->sd, ZH_TRUE );

   szAddress = zh_parc( 2 );
   if( socket->remote )
      zh_xfree( socket->remote );
   if( ! zh_socketInetAddr( &socket->remote, &socket->remotelen,
                            szAddress, iPort ) ||
       s_inetBind( socket, socket->remote, socket->remotelen ) != 0 )
   {
      zh_inetGetError( socket );
      zh_inetCloseSocket( socket, ZH_FALSE );
   }
   else if( zh_pcount() >= 4 )
   {
      if( zh_socketSetMulticast( socket->sd, ZH_SOCKET_PF_INET, zh_parc( 4 ) ) != 0 )
         zh_inetGetError( socket );
   }

   zh_itemReturnRelease( pSocket );
}

ZH_FUNC( ZH_INETDGRAM )
{
   PZH_SOCKET_STRUCT socket;
   PZH_ITEM pSocket = NULL;

   ZH_SOCKET_INIT( socket, pSocket );

   /* Creates comm socket */
   socket->sd = zh_socketOpen( ZH_SOCKET_PF_INET, ZH_SOCKET_PT_DGRAM, ZH_SOCKET_IPPROTO_UDP );
   if( socket->sd == ZH_NO_SOCKET )
   {
      zh_inetGetError( socket );
      zh_itemReturnRelease( pSocket );
      return;
   }

   /* Setting broadcast if needed. */
   if( zh_parl( 1 ) )
      zh_socketSetBroadcast( socket->sd, ZH_TRUE );

   zh_itemReturnRelease( pSocket );
}

ZH_FUNC( ZH_INETDGRAMSEND )
{
   PZH_SOCKET_STRUCT socket = ZH_PARSOCKET( 1 );
   const char * szAddress = zh_parc( 2 );
   int iPort = zh_parni( 3 );
   PZH_ITEM pBuffer = zh_param( 4, ZH_IT_STRING );
   int iLen;
   const char * szBuffer;

   if( socket == NULL || szAddress == NULL || iPort == 0 || pBuffer == NULL )
      zh_inetErrRT();
   else if( ! zh_inetIsOpen( socket ) )
   {
      socket->iCount = 0;
      zh_retni( -1 );
   }
   else
   {
      socket->iCount = 0;
      if( socket->remote )
         zh_xfree( socket->remote );
      if( ! zh_socketInetAddr( &socket->remote, &socket->remotelen, szAddress, iPort ) )
      {
         zh_inetGetError( socket );
         iLen = -1;
      }
      else
      {
         szBuffer = zh_itemGetCPtr( pBuffer );
         iLen = ( int ) zh_itemGetCLen( pBuffer );
         if( ZH_IS_PARAM_NUM( 5 ) )
         {
            int iMaxLen = zh_parni( 5 );
            if( iMaxLen < iLen )
               iLen = ZH_MAX( iMaxLen, 0 );
         }
         iLen = zh_socketSendTo( socket->sd, szBuffer, iLen, 0,
                                 socket->remote, socket->remotelen,
                                 socket->iTimeout );
         if( iLen == -1 )
            zh_inetGetError( socket );
         else
         {
            socket->iError = ZH_INET_ERR_OK;
            socket->iCount = iLen;
         }
      }
      zh_retni( iLen );
   }
}

ZH_FUNC( ZH_INETDGRAMRECV )
{
   PZH_SOCKET_STRUCT socket = ZH_PARSOCKET( 1 );
   PZH_ITEM pBuffer = zh_param( 2, ZH_IT_STRING );
   int iTimeElapsed = 0;
   int iLen = 0, iMax;
   char * buffer = NULL;
   ZH_SIZE nLen;
   ZH_BOOL fRepeat;

   if( socket == NULL || pBuffer == NULL || ! ZH_ISBYREF( 2 ) )
      zh_inetErrRT();
   else if( ! zh_inetIsOpen( socket ) )
   {
      socket->iCount = 0;
      zh_retni( -1 );
   }
   else
   {
      socket->iCount = 0;
      if( zh_itemGetWriteCL( pBuffer, &buffer, &nLen ) )
         iLen = ( int ) nLen;
      if( ZH_IS_PARAM_NUM( 3 ) )
      {
         iMax = zh_parni( 3 );
         if( iMax < iLen )
            iLen = ZH_MAX( iMax, 0 );
      }

      do
      {
         fRepeat = ZH_FALSE;
         if( socket->remote )
            zh_xfree( socket->remote );  /* FIXME: double free */
         iMax = zh_socketRecvFrom( socket->sd, buffer, iLen, 0,
                                   &socket->remote, &socket->remotelen,
                                   socket->iTimeout );
         if( socket->pPeriodicBlock )
         {
            iTimeElapsed += socket->iTimeout;
            zh_execFromArray( socket->pPeriodicBlock );
            /* do we continue? */
            fRepeat = zh_parl( -1 ) && zh_vmRequestQuery() == 0 &&
                      ( socket->iTimeLimit == -1 || iTimeElapsed < socket->iTimeLimit );
         }
      }
      while( fRepeat );

      if( iMax < 0 )
         zh_inetGetError( socket );
      else
         socket->iError = iMax == 0 ? ZH_INET_ERR_CLOSEDCONN : ZH_INET_ERR_OK;

      zh_retni( iMax );
   }
}

/* Generic utility(?) functions */

ZH_FUNC( ZH_INETCRLF )
{
   zh_retc_const( s_inetCRLF );
}

ZH_FUNC( ZH_INETISSOCKET )
{
   zh_retl( ZH_PARSOCKET( 1 ) != NULL );
}
