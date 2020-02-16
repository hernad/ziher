/*
 * Socket C API
 *
 * Copyright 2009 Przemyslaw Czerpak
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

#ifndef ZH_SOCKET_H_
#define ZH_SOCKET_H_

#include "zh_api.h"
#include "socket.zhh"

ZH_EXTERN_BEGIN

#if defined( ZH_OS_WIN ) && ! defined( ZH_OS_UNIX )
   typedef ZH_PTRUINT   ZH_SOCKET;
#else
   typedef int          ZH_SOCKET;
#endif

typedef ZH_SOCKET ( * ZH_SOCKET_FUNC )( PZH_ITEM );

#define ZH_NO_SOCKET          ( ( ZH_SOCKET ) -1 )

extern ZH_EXPORT void         zh_socketAutoInit( void );
extern ZH_EXPORT int          zh_socketInit( void );
extern ZH_EXPORT void         zh_socketCleanup( void );
extern ZH_EXPORT int          zh_socketGetError( void );
extern ZH_EXPORT int          zh_socketGetOsError( void );
extern ZH_EXPORT const char * zh_socketErrorStr( int iError );
extern ZH_EXPORT void         zh_socketSetError( int iError );
extern ZH_EXPORT int          zh_socketGetAddrFamily( const void * pSockAddr, unsigned len );
extern ZH_EXPORT ZH_BOOL      zh_socketLocalAddr( void ** pSockAddr, unsigned * puiLen, const char * szAddr );
extern ZH_EXPORT ZH_BOOL      zh_socketInetAddr( void ** pSockAddr, unsigned * puiLen, const char * szAddr, int iPort );
extern ZH_EXPORT ZH_BOOL      zh_socketInet6Addr( void ** pSockAddr, unsigned * puiLen, const char * szAddr, int iPort );
extern ZH_EXPORT char *       zh_socketAddrGetName( const void * pSockAddr, unsigned len );
extern ZH_EXPORT ZH_BOOL      zh_socketResolveInetAddr( void ** pSockAddr, unsigned * puiLen, const char * szAddr, int iPort );
extern ZH_EXPORT char *       zh_socketResolveAddr( const char * szAddr, int af );
extern ZH_EXPORT PZH_ITEM     zh_socketGetHosts( const char * szAddr, int af );
extern ZH_EXPORT PZH_ITEM     zh_socketGetAliases( const char * szAddr, int af );
extern ZH_EXPORT char *       zh_socketGetHostName( const void * pSockAddr, unsigned len );
extern ZH_EXPORT PZH_ITEM     zh_socketGetIFaces( int af, ZH_BOOL fNoAliases );
extern ZH_EXPORT int          zh_socketAddrGetPort( const void * pSockAddr, unsigned len );
extern ZH_EXPORT ZH_BOOL      zh_socketAddrFromItem( void ** pSockAddr, unsigned * puiLen, PZH_ITEM pAddrItm );
extern ZH_EXPORT PZH_ITEM     zh_socketAddrToItem( const void * pSockAddr, unsigned len );
extern ZH_EXPORT int          zh_socketGetSockName( ZH_SOCKET sd, void ** pSockAddr, unsigned * puiLen );
extern ZH_EXPORT int          zh_socketGetPeerName( ZH_SOCKET sd, void ** pSockAddr, unsigned * puiLen );
extern ZH_EXPORT ZH_SOCKET    zh_socketOpen( int domain, int type, int protocol );
extern ZH_EXPORT int          zh_socketClose( ZH_SOCKET sd );
extern ZH_EXPORT int          zh_socketShutdown( ZH_SOCKET sd, int iMode );
extern ZH_EXPORT int          zh_socketBind( ZH_SOCKET sd, const void * pSockAddr, unsigned uiLen );
extern ZH_EXPORT int          zh_socketListen( ZH_SOCKET sd, int iBacklog );
extern ZH_EXPORT ZH_SOCKET    zh_socketAccept( ZH_SOCKET sd, void ** pSockAddr, unsigned * puiLen, ZH_MAXINT timeout );
extern ZH_EXPORT int          zh_socketConnect( ZH_SOCKET sd, const void * pSockAddr, unsigned uiLen, ZH_MAXINT timeout );
extern ZH_EXPORT long         zh_socketSend( ZH_SOCKET sd, const void * data, long len, int flags, ZH_MAXINT timeout );
extern ZH_EXPORT long         zh_socketSendTo( ZH_SOCKET sd, const void * data, long len, int flags, const void * pSockAddr, unsigned uiSockLen, ZH_MAXINT timeout );
extern ZH_EXPORT long         zh_socketRecv( ZH_SOCKET sd, void * data, long len, int flags, ZH_MAXINT timeout );
extern ZH_EXPORT long         zh_socketRecvFrom( ZH_SOCKET sd, void * data, long len, int flags, void ** pSockAddr, unsigned * puiSockLen, ZH_MAXINT timeout );
extern ZH_EXPORT int          zh_socketSetBlockingIO( ZH_SOCKET sd, ZH_BOOL fBlocking );
extern ZH_EXPORT int          zh_socketSetNoDelay( ZH_SOCKET sd, ZH_BOOL fNoDelay );
extern ZH_EXPORT int          zh_socketSetExclusiveAddr( ZH_SOCKET sd, ZH_BOOL fExclusive );
extern ZH_EXPORT int          zh_socketSetReuseAddr( ZH_SOCKET sd, ZH_BOOL fReuse );
extern ZH_EXPORT int          zh_socketSetKeepAlive( ZH_SOCKET sd, ZH_BOOL fKeepAlive );
extern ZH_EXPORT int          zh_socketSetBroadcast( ZH_SOCKET sd, ZH_BOOL fBroadcast );
extern ZH_EXPORT int          zh_socketSetSndBufSize( ZH_SOCKET sd, int iSize );
extern ZH_EXPORT int          zh_socketSetRcvBufSize( ZH_SOCKET sd, int iSize );
extern ZH_EXPORT int          zh_socketGetRcvBufSize( ZH_SOCKET sd, int * piSize );
extern ZH_EXPORT int          zh_socketGetSndBufSize( ZH_SOCKET sd, int * piSize );
extern ZH_EXPORT int          zh_socketSetMulticast( ZH_SOCKET sd, int af, const char * szAddr );
extern ZH_EXPORT int          zh_socketSelectRead( ZH_SOCKET sd, ZH_MAXINT timeout );
extern ZH_EXPORT int          zh_socketSelectWrite( ZH_SOCKET sd, ZH_MAXINT timeout );
extern ZH_EXPORT int          zh_socketSelectWriteEx( ZH_SOCKET sd, ZH_MAXINT timeout );
extern ZH_EXPORT int          zh_socketSelect( PZH_ITEM pArrayRD, ZH_BOOL fSetRD,
                                               PZH_ITEM pArrayWR, ZH_BOOL fSetWR,
                                               PZH_ITEM pArrayEX, ZH_BOOL fSetEX,
                                               ZH_MAXINT timeout, ZH_SOCKET_FUNC pFunc );

/* Ziher level socket item API functions */
extern ZH_EXPORT ZH_SOCKET zh_socketParam( int iParam );
extern ZH_EXPORT ZH_SOCKET zh_socketItemGet( PZH_ITEM pItem );
extern ZH_EXPORT PZH_ITEM  zh_socketItemPut( PZH_ITEM pItem, ZH_SOCKET sd );
extern ZH_EXPORT void      zh_socketItemClear( PZH_ITEM pItem );

#define ZH_SOCKET_FILTER_MAX  128

struct _ZH_SOCKEX;
typedef struct _ZH_SOCKEX * PZH_SOCKEX;

#if defined( _ZH_SOCKEX_IMPLEMENTATION_ )

typedef struct
{
   const char * pszName;
   PZH_SOCKEX  ( * New )      ( ZH_SOCKET sd, PZH_ITEM pParams );
   PZH_SOCKEX  ( * Next )     ( PZH_SOCKEX pSock, PZH_ITEM pParams );
   int         ( * Close )    ( PZH_SOCKEX pSock, ZH_BOOL fClose );
   long        ( * Read )     ( PZH_SOCKEX pSock, void * data, long len, ZH_MAXINT timeout );
   long        ( * Write )    ( PZH_SOCKEX pSock, const void * data, long len, ZH_MAXINT timeout );
   long        ( * Flush )    ( PZH_SOCKEX pSock, ZH_MAXINT timeout, ZH_BOOL fSync );
   int         ( * CanRead )  ( PZH_SOCKEX pSock, ZH_BOOL fBuffer, ZH_MAXINT timeout );
   int         ( * CanWrite ) ( PZH_SOCKEX pSock, ZH_BOOL fBuffer, ZH_MAXINT timeout );
   char *      ( * Name )     ( PZH_SOCKEX pSock );
   const char *( * ErrorStr ) ( PZH_SOCKEX pSock, int iError );
}
ZH_SOCKET_FILTER, * PZH_SOCKET_FILTER;

typedef struct _ZH_SOCKEX
{
   ZH_SOCKET sd;
   ZH_BOOL   fRedirAll;
   ZH_BOOL   fShutDown;
   int       iAutoFlush;
   long      inbuffer;
   long      posbuffer;
   long      readahead;
   ZH_BYTE * buffer;
   void *    cargo;
   const ZH_SOCKET_FILTER * pFilter;
}
ZH_SOCKEX;

extern ZH_EXPORT int  zh_sockexRegister( const ZH_SOCKET_FILTER * pFilter );

#endif /* _ZH_SOCKEX_IMPLEMENTATION_ */

extern ZH_EXPORT int  zh_sockexClose( PZH_SOCKEX pSock, ZH_BOOL fClose );
extern ZH_EXPORT long zh_sockexRead ( PZH_SOCKEX pSock, void * data, long len, ZH_MAXINT timeout );
extern ZH_EXPORT long zh_sockexWrite( PZH_SOCKEX pSock, const void * data, long len, ZH_MAXINT timeout );
extern ZH_EXPORT long zh_sockexFlush( PZH_SOCKEX pSock, ZH_MAXINT timeout, ZH_BOOL fSync );

extern ZH_EXPORT int  zh_sockexCanRead ( PZH_SOCKEX pSock, ZH_BOOL fBuffer, ZH_MAXINT timeout );
extern ZH_EXPORT int  zh_sockexCanWrite( PZH_SOCKEX pSock, ZH_BOOL fBuffer, ZH_MAXINT timeout );
extern ZH_EXPORT int  zh_sockexSelect( PZH_ITEM pArrayRD, ZH_BOOL fSetRD,
                                       PZH_ITEM pArrayWR, ZH_BOOL fSetWR,
                                       PZH_ITEM pArrayEX, ZH_BOOL fSetEX,
                                       ZH_MAXINT timeout, ZH_SOCKET_FUNC pFunc );

extern ZH_EXPORT ZH_BOOL    zh_sockexIsRaw( PZH_SOCKEX pSock );
extern ZH_EXPORT int        zh_sockexRawClear( PZH_SOCKEX pSock, ZH_BOOL fClear );
extern ZH_EXPORT ZH_SOCKET  zh_sockexGetHandle( PZH_SOCKEX pSock );
extern ZH_EXPORT void       zh_sockexClearHandle( PZH_SOCKEX pSock );
extern ZH_EXPORT void       zh_sockexSetShutDown( PZH_SOCKEX pSock, ZH_BOOL fShutDown );
extern ZH_EXPORT ZH_BOOL    zh_sockexGetShutDown( PZH_SOCKEX pSock );
extern ZH_EXPORT void       zh_sockexSetAutoFlush( PZH_SOCKEX pSock, int iAutoFlush );
extern ZH_EXPORT int        zh_sockexGetAutoFlush( PZH_SOCKEX pSock );

extern ZH_EXPORT void       zh_socekxParamsInit( PZH_SOCKEX pSock, PZH_ITEM pParams );
extern ZH_EXPORT void       zh_socekxParamsGetStd( PZH_ITEM pParams,
                                                   const void ** pKeydata, int * pKeylen,
                                                   const void ** pIV, int * pIVlen,
                                                   int * pLevel, int * pStrategy );
extern ZH_EXPORT PZH_SOCKEX zh_sockexNew( ZH_SOCKET sd, const char * pszFilter, PZH_ITEM pParams );
extern ZH_EXPORT PZH_SOCKEX zh_sockexNext( PZH_SOCKEX pSock, const char * pszFilter, PZH_ITEM pParams );
extern ZH_EXPORT char *     zh_sockexName( PZH_SOCKEX pSock );
extern ZH_EXPORT const char * zh_sockexErrorStr( PZH_SOCKEX pSock, int iError );
extern ZH_EXPORT PZH_SOCKEX zh_sockexParam( int iParam );
extern ZH_EXPORT PZH_SOCKEX zh_sockexItemGet( PZH_ITEM pItem );
extern ZH_EXPORT PZH_ITEM   zh_sockexItemPut( PZH_ITEM pItem, PZH_SOCKEX pSock );
extern ZH_EXPORT void       zh_sockexItemClear( PZH_ITEM pItem );
extern ZH_EXPORT ZH_BOOL    zh_sockexItemReplace( PZH_ITEM pItem, PZH_SOCKEX pSock );
extern ZH_EXPORT ZH_BOOL    zh_sockexItemSetFilter( PZH_ITEM pItem, const char * pszFilter, PZH_ITEM pParams );

ZH_EXTERN_END

#endif /* ZH_SOCKET_H_ */
