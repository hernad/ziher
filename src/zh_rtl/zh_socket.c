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

#include "zh_api.h"
#include "zh_socket.h"

#if ! defined( ZH_SOCKET_OFF )

/* we do not use autoconf so we can only guess what is supported
 * by platform and/or CRTL :-(
 */
/* we are using following macros:

   platform supports inet_aton() function:
      #define ZH_HAS_INET_ATON

   platform supports inet_pton() function:
      #define ZH_HAS_INET_PTON

   platform supports inet_ntop() function:
      #define ZH_HAS_INET_NTOP

   platform supports thread safe (using TLS) inet_ntoa() function:
      #define ZH_IS_INET_NTOA_MT_SAFE

   platform supports getaddrinfo()/freeaddrinfo() functions:
      #define ZH_HAS_ADDRINFO

   platform supports getnameinfo() function:
      #define ZH_HAS_NAMEINFO

   platform supports gethostbyaddr function:
      #define ZH_HAS_GETHOSTBYADDR

   platform supports poll() function:
      #define ZH_HAS_POLL

   platform uses sockaddr structure which contains sa_len member:
      #define ZH_HAS_SOCKADDR_SA_LEN

   platform supports constant inet6 addresses in6addr_any and in6addr_loopback:
      #define ZH_HAS_INET6_ADDR_CONST

   platform supports IP6 protocol:
      #define ZH_HAS_INET6

   platform supports unix/local protocol:
      #define ZH_HAS_UNIX

   platform supports 'struct sockaddr_storage' which can be used as holder
   for all socket address structures in all supported protocols,
   simple 'struct sockaddr' is not large enough for such usage:
      #define ZH_HAS_SOCKADDR_STORAGE

   timeval parameter used in Select() function is updated by kernel/CRTL
   and decreased by the amount of time the function was waiting:
      #define ZH_HAS_SELECT_TIMER

   all implementations should use BSD compatible constant values but
   if it's not guarantied then these two macros can be used.
   protocol families have to be translated:
      #define ZH_SOCKET_TRANSLATE_DOMAIN

   protocol types have to be translated:
      #define ZH_SOCKET_TRANSLATE_TYPE
*/

#if defined( ZH_OS_UNIX )
#  define ZH_HAS_UNIX
#     define ZH_HAS_INET_ATON
#     define ZH_HAS_INET_PTON
#     define ZH_HAS_INET_NTOP
#     define ZH_HAS_SOCKADDR_STORAGE
#     define ZH_HAS_ADDRINFO
#     define ZH_HAS_NAMEINFO
#     define ZH_HAS_GETHOSTBYADDR
#     if ! defined( ZH_HAS_POLL ) && ! defined( ZH_NO_POLL ) && \
         defined( _POSIX_C_SOURCE ) && _POSIX_C_SOURCE >= 200112L
         /* use poll() instead of select() to avoid FD_SETSIZE (1024 in Linux)
            file handle limit */
#        define ZH_HAS_POLL
#     endif
#     define ZH_HAS_INET6
#     define ZH_HAS_INET6_ADDR_CONST
#  if defined( ZH_OS_LINUX )
#     define ZH_HAS_SELECT_TIMER
#     if defined( ZH_CPU_MIPS )
#        define ZH_SOCKET_TRANSLATE_TYPE
#     endif
#  endif
#elif defined( ZH_OS_WIN )
#  if defined( __MINGW32__ )
#     define ZH_HAS_SOCKADDR_STORAGE
#  elif defined( _MSC_VER )
#     if _MSC_VER >= 1800 && ! defined( ZH_WINSOCK_USE_OLDFUNC )
#        define ZH_HAS_INET_PTON
#        define ZH_HAS_INET_NTOP
#        define ZH_HAS_ADDRINFO
#        define ZH_HAS_NAMEINFO
#     else
#        define _WINSOCK_DEPRECATED_NO_WARNINGS
#     endif
#  endif
#  define ZH_IS_INET_NTOA_MT_SAFE
#  define ZH_HAS_GETHOSTBYADDR
#  define zh_socketSetResolveError( err ) zh_socketSetOsError( err )
#endif

#if defined( ZH_HAS_NAMEINFO ) && ! defined( ZH_HAS_ADDRINFO )
#  undef ZH_HAS_NAMEINFO
#endif


#if defined( ZH_OS_WIN )
#  include <winsock2.h>
#  include <ws2tcpip.h>
#  include <iphlpapi.h>
#else
#  include <errno.h>
#  include <sys/time.h>
#  include <sys/types.h>
#  include <sys/socket.h>
#  include <sys/ioctl.h>
#  include <netdb.h>
#  include <netinet/in.h>
#  include <arpa/inet.h>
#  if defined( ZH_HAS_UNIX )
#     include <sys/un.h>
#  endif
#  if defined( ZH_HAS_POLL )
#     include <poll.h>
#  endif
#  include <netinet/tcp.h>
#     include <net/if.h>
#  include <unistd.h>
#  include <fcntl.h>
#endif

#if defined( ZH_OS_WIN )
#  define socklen_t int
#endif

#if ! defined( INET_ADDRSTRLEN )
#  define INET_ADDRSTRLEN  16
#endif

#if ! defined( SHUT_RD )
#  define SHUT_RD       0
#  define SHUT_WR       1
#  define SHUT_RDWR     2
#endif

#if defined( ZH_OS_WIN )
   typedef SOCKET       ZH_SOCKET_T;
#else
   typedef ZH_SOCKET    ZH_SOCKET_T;
#endif

#endif /* ZH_SOCKET_OFF */

#include "zh_api.h"
#include "zh_item_api.h"
#include "zh_vm.h"
#include "zh_stack.h"
#include "zh_thread.h"
#include "zh_date.h"

/* TODO change error description to something more user friendly */
static const char * s_socketErrors[] = {
   "OK",
   "EPIPE",
   "ETIMEOUT",
   "EWRONGADDR",
   "EAFNOSUPPORT",
   "EPFNOSUPPORT",
   "EPROTONOSUPPORT",
   "EPARAMVALUE",
   "ENOSUPPORT",
   "ENORESOURCE",
   "EACCESS",
   "EADDRINUSE",
   "EINTERRUPT",
   "EALREADYCONNECTED",
   "ECONNREFUSED",
   "ECONNABORTED",
   "ECONNRESET",
   "ENETUNREACH",
   "ENETDOWN",
   "ENETRESET",
   "EINPROGRESS",
   "EALREADY",
   "EADDRNOTAVAIL",
   "EREADONLY",
   "EAGAIN",
   "EINVALIDHANDLE",
   "EINVAL",
   "EPROTO",
   "EPROTOTYPE",
   "ENOFILE",
   "ENOBUFS",
   "ENOMEM",
   "EFAULT",
   "ENAMETOOLONG",
   "ENOENT",
   "ENOTDIR",
   "ELOOP",
   "EMSGSIZE",
   "EDESTADDRREQ",
   "ENOPROTOOPT",
   "ENOTCONN",
   "ESHUTDOWN",
   "ETOOMANYREFS",
   "ERESTARTSYS",
   "ENOSR",
   "EHOSTDOWN",
   "EHOSTUNREACH",
   "ENOTEMPTY",
   "EUSERS",
   "EDQUOT",
   "ESTALE",
   "EREMOTE",
   "EPROCLIM",
   "EDISCON",
   "ENOMORE",
   "ECANCELLED",
   "EINVALIDPROCTABLE",
   "EINVALIDPROVIDER",
   "EPROVIDERFAILEDINIT",
   "EREFUSED",
   "ESYSNOTREADY",
   "EVERNOTSUPPORTED",
   "ENOTINITIALISED",
   "TRYAGAIN",
   "HOSTNOTFOUND",
   "NORECOVERY",
   "NODATA",
   "ESYSCALLFAILURE",
   "ESERVICENOTFOUND",
   "ETYPENOTFOUND",
   "EOTHER"
};

int zh_socketGetError( void )
{
   return zh_stackIOErrors()->uiSocketError;
}

int zh_socketGetOsError( void )
{
   return zh_stackIOErrors()->iSocketOsError;
}

const char * zh_socketErrorStr( int iError )
{
   if( iError >= 0 && iError <= ZH_SOCKET_ERR_OTHER )
      return s_socketErrors[ iError ];
   else
      return "";
}

void zh_socketSetError( int err )
{
   PZH_IOERRORS pError = zh_stackIOErrors();

   pError->uiSocketError = ( ZH_ERRCODE ) err;
   pError->iSocketOsError = 0;
}

#if defined( ZH_SOCKET_OFF )

int zh_socketInit( void ) { return -1; }

void zh_socketCleanup( void ) { ; }

int zh_socketGetAddrFamily( const void * pSockAddr, unsigned len )
{
   ZH_SYMBOL_UNUSED( pSockAddr );
   ZH_SYMBOL_UNUSED( len );
   return -1;
}

ZH_BOOL zh_socketLocalAddr( void ** pSockAddr, unsigned * puiLen,
                            const char * szAddr )
{
   ZH_SYMBOL_UNUSED( szAddr );
   zh_socketSetError( ZH_SOCKET_ERR_AFNOSUPPORT );
   *pSockAddr = NULL;
   *puiLen = 0;
   return ZH_FALSE;
}

ZH_BOOL zh_socketInetAddr( void ** pSockAddr, unsigned * puiLen,
                           const char * szAddr, int iPort )
{
   ZH_SYMBOL_UNUSED( szAddr );
   ZH_SYMBOL_UNUSED( iPort );
   zh_socketSetError( ZH_SOCKET_ERR_AFNOSUPPORT );
   *pSockAddr = NULL;
   *puiLen = 0;
   return ZH_FALSE;
}

ZH_BOOL zh_socketInet6Addr( void ** pSockAddr, unsigned * puiLen,
                            const char * szAddr, int iPort )
{
   ZH_SYMBOL_UNUSED( szAddr );
   ZH_SYMBOL_UNUSED( iPort );
   zh_socketSetError( ZH_SOCKET_ERR_AFNOSUPPORT );
   *pSockAddr = NULL;
   *puiLen = 0;
   return ZH_FALSE;
}

char * zh_socketAddrGetName( const void * pSockAddr, unsigned len )
{
   ZH_SYMBOL_UNUSED( pSockAddr );
   ZH_SYMBOL_UNUSED( len );
   zh_socketSetError( ZH_SOCKET_ERR_AFNOSUPPORT );
   return NULL;
}

int zh_socketAddrGetPort( const void * pSockAddr, unsigned len )
{
   ZH_SYMBOL_UNUSED( pSockAddr );
   ZH_SYMBOL_UNUSED( len );
   zh_socketSetError( ZH_SOCKET_ERR_AFNOSUPPORT );
   return -1;
}

ZH_BOOL zh_socketAddrFromItem( void ** pSockAddr, unsigned * puiLen, PZH_ITEM pAddrItm )
{
   ZH_SYMBOL_UNUSED( pAddrItm );
   zh_socketSetError( ZH_SOCKET_ERR_AFNOSUPPORT );
   *pSockAddr = NULL;
   *puiLen = 0;
   return ZH_FALSE;
}

PZH_ITEM zh_socketAddrToItem( const void * pSockAddr, unsigned len )
{
   ZH_SYMBOL_UNUSED( pSockAddr );
   ZH_SYMBOL_UNUSED( len );
   zh_socketSetError( ZH_SOCKET_ERR_AFNOSUPPORT );
   return NULL;
}

int zh_socketGetSockName( ZH_SOCKET sd, void ** pSockAddr, unsigned * puiLen )
{
   ZH_SYMBOL_UNUSED( sd );
   zh_socketSetError( ZH_SOCKET_ERR_INVALIDHANDLE );
   *pSockAddr = NULL;
   *puiLen = 0;
   return -1;
}

int zh_socketGetPeerName( ZH_SOCKET sd, void ** pSockAddr, unsigned * puiLen )
{
   ZH_SYMBOL_UNUSED( sd );
   zh_socketSetError( ZH_SOCKET_ERR_INVALIDHANDLE );
   *pSockAddr = NULL;
   *puiLen = 0;
   return -1;
}

ZH_SOCKET zh_socketOpen( int domain, int type, int protocol )
{
   ZH_SYMBOL_UNUSED( domain );
   ZH_SYMBOL_UNUSED( type );
   ZH_SYMBOL_UNUSED( protocol );
   zh_socketSetError( ZH_SOCKET_ERR_PFNOSUPPORT );
   return ZH_NO_SOCKET;
}

int zh_socketClose( ZH_SOCKET sd )
{
   ZH_SYMBOL_UNUSED( sd );
   zh_socketSetError( ZH_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int zh_socketShutdown( ZH_SOCKET sd, int iMode )
{
   ZH_SYMBOL_UNUSED( sd );
   ZH_SYMBOL_UNUSED( iMode );
   zh_socketSetError( ZH_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int zh_socketBind( ZH_SOCKET sd, const void * pSockAddr, unsigned uiLen )
{
   ZH_SYMBOL_UNUSED( sd );
   ZH_SYMBOL_UNUSED( pSockAddr );
   ZH_SYMBOL_UNUSED( uiLen );
   zh_socketSetError( ZH_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int zh_socketListen( ZH_SOCKET sd, int iBacklog )
{
   ZH_SYMBOL_UNUSED( sd );
   ZH_SYMBOL_UNUSED( iBacklog );
   zh_socketSetError( ZH_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

ZH_SOCKET zh_socketAccept( ZH_SOCKET sd, void ** pSockAddr, unsigned * puiLen, ZH_MAXINT timeout )
{
   ZH_SYMBOL_UNUSED( sd );
   ZH_SYMBOL_UNUSED( timeout );
   zh_socketSetError( ZH_SOCKET_ERR_INVALIDHANDLE );
   *pSockAddr = NULL;
   *puiLen = 0;
   return ZH_NO_SOCKET;
}

int zh_socketConnect( ZH_SOCKET sd, const void * pSockAddr, unsigned uiLen, ZH_MAXINT timeout )
{
   ZH_SYMBOL_UNUSED( sd );
   ZH_SYMBOL_UNUSED( pSockAddr );
   ZH_SYMBOL_UNUSED( uiLen );
   ZH_SYMBOL_UNUSED( timeout );
   zh_socketSetError( ZH_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

long zh_socketSend( ZH_SOCKET sd, const void * data, long len, int flags, ZH_MAXINT timeout )
{
   ZH_SYMBOL_UNUSED( sd );
   ZH_SYMBOL_UNUSED( data );
   ZH_SYMBOL_UNUSED( len );
   ZH_SYMBOL_UNUSED( flags );
   ZH_SYMBOL_UNUSED( timeout );
   zh_socketSetError( ZH_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

long zh_socketSendTo( ZH_SOCKET sd, const void * data, long len, int flags, const void * pSockAddr, unsigned uiSockLen, ZH_MAXINT timeout )
{
   ZH_SYMBOL_UNUSED( sd );
   ZH_SYMBOL_UNUSED( data );
   ZH_SYMBOL_UNUSED( len );
   ZH_SYMBOL_UNUSED( flags );
   ZH_SYMBOL_UNUSED( pSockAddr );
   ZH_SYMBOL_UNUSED( uiSockLen );
   ZH_SYMBOL_UNUSED( timeout );
   zh_socketSetError( ZH_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

long zh_socketRecv( ZH_SOCKET sd, void * data, long len, int flags, ZH_MAXINT timeout )
{
   ZH_SYMBOL_UNUSED( sd );
   ZH_SYMBOL_UNUSED( data );
   ZH_SYMBOL_UNUSED( len );
   ZH_SYMBOL_UNUSED( flags );
   ZH_SYMBOL_UNUSED( timeout );
   zh_socketSetError( ZH_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

long zh_socketRecvFrom( ZH_SOCKET sd, void * data, long len, int flags, void ** pSockAddr, unsigned * puiSockLen, ZH_MAXINT timeout )
{
   ZH_SYMBOL_UNUSED( sd );
   ZH_SYMBOL_UNUSED( data );
   ZH_SYMBOL_UNUSED( len );
   ZH_SYMBOL_UNUSED( flags );
   ZH_SYMBOL_UNUSED( timeout );
   zh_socketSetError( ZH_SOCKET_ERR_INVALIDHANDLE );
   *pSockAddr = NULL;
   *puiSockLen = 0;
   return -1;
}

int zh_socketSetBlockingIO( ZH_SOCKET sd, ZH_BOOL fBlocking )
{
   ZH_SYMBOL_UNUSED( sd );
   ZH_SYMBOL_UNUSED( fBlocking );
   zh_socketSetError( ZH_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int zh_socketSetNoDelay( ZH_SOCKET sd, ZH_BOOL fNoDelay )
{
   ZH_SYMBOL_UNUSED( sd );
   ZH_SYMBOL_UNUSED( fNoDelay );
   zh_socketSetError( ZH_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int zh_socketSetExclusiveAddr( ZH_SOCKET sd, ZH_BOOL fExclusive )
{
   ZH_SYMBOL_UNUSED( sd );
   ZH_SYMBOL_UNUSED( fExclusive );
   zh_socketSetError( ZH_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int zh_socketSetReuseAddr( ZH_SOCKET sd, ZH_BOOL fReuse )
{
   ZH_SYMBOL_UNUSED( sd );
   ZH_SYMBOL_UNUSED( fReuse );
   zh_socketSetError( ZH_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int zh_socketSetKeepAlive( ZH_SOCKET sd, ZH_BOOL fKeepAlive )
{
   ZH_SYMBOL_UNUSED( sd );
   ZH_SYMBOL_UNUSED( fKeepAlive );
   zh_socketSetError( ZH_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int zh_socketSetBroadcast( ZH_SOCKET sd, ZH_BOOL fBroadcast )
{
   ZH_SYMBOL_UNUSED( sd );
   ZH_SYMBOL_UNUSED( fBroadcast );
   zh_socketSetError( ZH_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int zh_socketSetSndBufSize( ZH_SOCKET sd, int iSize )
{
   ZH_SYMBOL_UNUSED( sd );
   ZH_SYMBOL_UNUSED( iSize );
   zh_socketSetError( ZH_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int zh_socketSetRcvBufSize( ZH_SOCKET sd, int iSize )
{
   ZH_SYMBOL_UNUSED( sd );
   ZH_SYMBOL_UNUSED( iSize );
   zh_socketSetError( ZH_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int zh_socketGetSndBufSize( ZH_SOCKET sd, int * piSize )
{
   ZH_SYMBOL_UNUSED( sd );
   ZH_SYMBOL_UNUSED( piSize );
   zh_socketSetError( ZH_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int zh_socketGetRcvBufSize( ZH_SOCKET sd, int * piSize )
{
   ZH_SYMBOL_UNUSED( sd );
   ZH_SYMBOL_UNUSED( piSize );
   zh_socketSetError( ZH_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int zh_socketSetMulticast( ZH_SOCKET sd, int af, const char * szAddr )
{
   ZH_SYMBOL_UNUSED( sd );
   ZH_SYMBOL_UNUSED( af );
   ZH_SYMBOL_UNUSED( szAddr );
   zh_socketSetError( ZH_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int zh_socketSelectRead( ZH_SOCKET sd, ZH_MAXINT timeout )
{
   ZH_SYMBOL_UNUSED( sd );
   ZH_SYMBOL_UNUSED( timeout );
   zh_socketSetError( ZH_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int zh_socketSelectWrite( ZH_SOCKET sd, ZH_MAXINT timeout )
{
   ZH_SYMBOL_UNUSED( sd );
   ZH_SYMBOL_UNUSED( timeout );
   zh_socketSetError( ZH_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}


int zh_socketSelectWriteEx( ZH_SOCKET sd, ZH_MAXINT timeout )
{
   ZH_SYMBOL_UNUSED( sd );
   ZH_SYMBOL_UNUSED( timeout );
   zh_socketSetError( ZH_SOCKET_ERR_INVALIDHANDLE );
   return -1;
}

int zh_socketSelect( PZH_ITEM pArrayRD, ZH_BOOL fSetRD,
                     PZH_ITEM pArrayWR, ZH_BOOL fSetWR,
                     PZH_ITEM pArrayEX, ZH_BOOL fSetEX,
                     ZH_MAXINT timeout, ZH_SOCKET_FUNC pFunc )
{
   ZH_SYMBOL_UNUSED( pArrayRD );
   ZH_SYMBOL_UNUSED( fSetRD );
   ZH_SYMBOL_UNUSED( pArrayWR );
   ZH_SYMBOL_UNUSED( fSetWR );
   ZH_SYMBOL_UNUSED( pArrayEX );
   ZH_SYMBOL_UNUSED( fSetEX );
   ZH_SYMBOL_UNUSED( timeout );
   ZH_SYMBOL_UNUSED( pFunc );
   zh_socketSetError( ZH_SOCKET_ERR_NOSUPPORT );
   return -1;
}

ZH_BOOL zh_socketResolveInetAddr( void ** pSockAddr, unsigned * puiLen, const char * szAddr, int iPort )
{
   ZH_SYMBOL_UNUSED( szAddr );
   ZH_SYMBOL_UNUSED( iPort );
   zh_socketSetError( ZH_SOCKET_ERR_AFNOSUPPORT );
   *pSockAddr = NULL;
   *puiLen = 0;
   return ZH_FALSE;
}

char * zh_socketResolveAddr( const char * szAddr, int af )
{
   ZH_SYMBOL_UNUSED( szAddr );
   ZH_SYMBOL_UNUSED( af );
   zh_socketSetError( ZH_SOCKET_ERR_AFNOSUPPORT );
   return NULL;
}

PZH_ITEM zh_socketGetHosts( const char * szAddr, int af )
{
   ZH_SYMBOL_UNUSED( szAddr );
   ZH_SYMBOL_UNUSED( af );
   zh_socketSetError( ZH_SOCKET_ERR_AFNOSUPPORT );
   return NULL;
}

PZH_ITEM zh_socketGetAliases( const char * szAddr, int af )
{
   ZH_SYMBOL_UNUSED( szAddr );
   ZH_SYMBOL_UNUSED( af );
   zh_socketSetError( ZH_SOCKET_ERR_AFNOSUPPORT );
   return NULL;
}

char * zh_socketGetHostName( const void * pSockAddr, unsigned len )
{
   ZH_SYMBOL_UNUSED( pSockAddr );
   ZH_SYMBOL_UNUSED( len );
   zh_socketSetError( ZH_SOCKET_ERR_AFNOSUPPORT );
   return NULL;
}

PZH_ITEM zh_socketGetIFaces( int af, ZH_BOOL fNoAliases )
{
   ZH_SYMBOL_UNUSED( af );
   ZH_SYMBOL_UNUSED( fNoAliases );
   zh_socketSetError( ZH_SOCKET_ERR_AFNOSUPPORT );
   return NULL;
}

#else

#define ZH_SOCKADDR_MAX_LEN   256

#if defined( ZH_OS_WIN )
#  define ZH_SOCK_GETERROR()              WSAGetLastError()
#  define ZH_SOCK_GETHERROR()             WSAGetLastError()
#  define ZH_SOCK_IS_EINTR( err )         ( (err) == WSAEINTR )
#  define ZH_SOCK_IS_EINPROGRES( err )    ( (err) == WSAEWOULDBLOCK )
#else
#  define ZH_SOCK_GETERROR()              errno
#  define ZH_SOCK_GETHERROR()             h_errno
#  define ZH_SOCK_IS_EINTR( err )         ( (err) == EINTR )
#  define ZH_SOCK_IS_EINPROGRES( err )    ( (err) == EINPROGRESS )
#endif

typedef union
{
#if defined( ZH_HAS_SOCKADDR_STORAGE )
   struct sockaddr_storage st;
#else
   char st[ ZH_SOCKADDR_MAX_LEN ];
#endif
   struct sockaddr sa;
} ZH_SOCKADDR_STORAGE;

/* MT macros */
#define ZH_SOCKET_LOCK()        zh_threadEnterCriticalSection( &s_sockMtx )
#define ZH_SOCKET_UNLOCK()      zh_threadLeaveCriticalSection( &s_sockMtx )
static ZH_CRITICAL_NEW( s_sockMtx );

static int s_iSessions;


#if defined( ZH_HAS_INET6 ) && ! defined( ZH_HAS_INET6_ADDR_CONST ) && \
    defined( IN6ADDR_ANY_INIT )
   static const struct in6_addr s_in6addr_any = IN6ADDR_ANY_INIT;
#endif

#if ! defined( ZH_HAS_INET_NTOP ) && ! defined( ZH_IS_INET_NTOA_MT_SAFE ) && defined( AF_INET )
static const char * zh_inet_ntoa( const struct in_addr * addr, char * pBuffer )
{
   /* dirty hack to make inet_ntoa() MT safe,
    * in many systems inet_ntoa() returns pointer to
    * static buffer and is not MT safe.
    */
   ZH_ULONG u = ntohl( addr->s_addr );

   zh_snprintf( pBuffer, INET_ADDRSTRLEN, "%hd.%hd.%hd.%hd",
                ZH_UHBYTE( u ), ZH_ULBYTE( u ), ZH_HIBYTE( u ), ZH_LOBYTE( u ) );
   return pBuffer;
}
#endif

int zh_socketInit( void )
{
   int ret = 0;

   ZH_SOCKET_LOCK();
   if( ++s_iSessions == 1 )
   {
#if defined( ZH_OS_WIN )
      WSADATA wsadata;
      ret = WSAStartup( ZH_MKUSHORT( 1, 1 ), &wsadata );
#endif
   }
   ZH_SOCKET_UNLOCK();

   return ret;
}

void zh_socketCleanup( void )
{
   ZH_SOCKET_LOCK();
   if( --s_iSessions == 0 )
   {
#if defined( ZH_OS_WIN )
      WSACleanup();
#elif defined( ZH_OS_DOS )
      sock_exit();
#endif
   }
   ZH_SOCKET_UNLOCK();
}

static void zh_socketSetOsError( int err )
{
   PZH_IOERRORS pError = zh_stackIOErrors();
   ZH_ERRCODE uiErr;

#if defined( ZH_OS_WIN )
   switch( err )
   {
      case 0:
         uiErr = 0;
         break;
      case WSAEINTR:
         uiErr = ZH_SOCKET_ERR_INTERRUPT;
         break;
      case WSAEBADF:
      case WSAENOTSOCK:
         uiErr = ZH_SOCKET_ERR_INVALIDHANDLE;
         break;
      case WSAEACCES:
         uiErr = ZH_SOCKET_ERR_ACCESS;
         break;
      case WSAEFAULT:
         uiErr = ZH_SOCKET_ERR_FAULT;
         break;
      case WSAEINVAL:
         uiErr = ZH_SOCKET_ERR_INVAL;
         break;
      case WSAEMFILE:
         uiErr = ZH_SOCKET_ERR_NOFILE;
         break;
      case WSAEWOULDBLOCK:
         uiErr = ZH_SOCKET_ERR_AGAIN;
         break;
      case WSAEINPROGRESS:
         uiErr = ZH_SOCKET_ERR_INPROGRESS;
         break;
      case WSAEALREADY:
         uiErr = ZH_SOCKET_ERR_ALREADY;
         break;
      case WSAEDESTADDRREQ:
         uiErr = ZH_SOCKET_ERR_DESTADDRREQ;
         break;
      case WSAEMSGSIZE:
         uiErr = ZH_SOCKET_ERR_MSGSIZE;
         break;
      case WSAEPROTOTYPE:
         uiErr = ZH_SOCKET_ERR_PROTOTYPE;
         break;
      case WSAENOPROTOOPT:
         uiErr = ZH_SOCKET_ERR_NOPROTOOPT;
         break;
      case WSAEPROTONOSUPPORT:
         uiErr = ZH_SOCKET_ERR_PROTONOSUPPORT;
         break;
      case WSAEOPNOTSUPP:
      case WSAESOCKTNOSUPPORT:
         uiErr = ZH_SOCKET_ERR_NOSUPPORT;
         break;
      case WSAEPFNOSUPPORT:
         uiErr = ZH_SOCKET_ERR_PFNOSUPPORT;
         break;
      case WSAEAFNOSUPPORT:
         uiErr = ZH_SOCKET_ERR_AFNOSUPPORT;
         break;
      case WSAEADDRINUSE:
         uiErr = ZH_SOCKET_ERR_ADDRINUSE;
         break;
      case WSAEADDRNOTAVAIL:
         uiErr = ZH_SOCKET_ERR_ADDRNOTAVAIL;
         break;
      case WSAENETDOWN:
         uiErr = ZH_SOCKET_ERR_NETDOWN;
         break;
      case WSAENETUNREACH:
         uiErr = ZH_SOCKET_ERR_NETUNREACH;
         break;
      case WSAENETRESET:
         uiErr = ZH_SOCKET_ERR_NETRESET;
         break;
      case WSAECONNREFUSED:
         uiErr = ZH_SOCKET_ERR_CONNREFUSED;
         break;
      case WSAECONNABORTED:
         uiErr = ZH_SOCKET_ERR_CONNABORTED;
         break;
      case WSAECONNRESET:
         uiErr = ZH_SOCKET_ERR_CONNRESET;
         break;
      case WSAENOBUFS:
         uiErr = ZH_SOCKET_ERR_NOBUFS;
         break;
      case WSAEISCONN:
         uiErr = ZH_SOCKET_ERR_ALREADYCONNECTED;
         break;
      case WSAENOTCONN:
         uiErr = ZH_SOCKET_ERR_NOTCONN;
         break;
      case WSAESHUTDOWN:
         uiErr = ZH_SOCKET_ERR_SHUTDOWN;
         break;
      case WSAETOOMANYREFS:
         uiErr = ZH_SOCKET_ERR_TOOMANYREFS;
         break;
      case WSAETIMEDOUT:
         uiErr = ZH_SOCKET_ERR_TIMEOUT;
         break;
      case WSAELOOP:
         uiErr = ZH_SOCKET_ERR_LOOP;
         break;
      case WSAENAMETOOLONG:
         uiErr = ZH_SOCKET_ERR_NAMETOOLONG;
         break;
      case WSAEHOSTDOWN:
         uiErr = ZH_SOCKET_ERR_HOSTDOWN;
         break;
      case WSAEHOSTUNREACH:
         uiErr = ZH_SOCKET_ERR_HOSTUNREACH;
         break;
      case WSAENOTEMPTY:
         uiErr = ZH_SOCKET_ERR_NOTEMPTY;
         break;
      case WSAEUSERS:
         uiErr = ZH_SOCKET_ERR_USERS;
         break;
      case WSAEDQUOT:
         uiErr = ZH_SOCKET_ERR_DQUOT;
         break;
      case WSAESTALE:
         uiErr = ZH_SOCKET_ERR_STALE;
         break;
      case WSAEREMOTE:
         uiErr = ZH_SOCKET_ERR_REMOTE;
         break;
      case WSAEPROCLIM:
         uiErr = ZH_SOCKET_ERR_PROCLIM;
         break;
      case WSAEDISCON:
         uiErr = ZH_SOCKET_ERR_DISCON;
         break;
      case WSAENOMORE:
         uiErr = ZH_SOCKET_ERR_NOMORE;
         break;
      case WSAECANCELLED:
         uiErr = ZH_SOCKET_ERR_CANCELLED;
         break;
      case WSAEINVALIDPROCTABLE:
         uiErr = ZH_SOCKET_ERR_INVALIDPROCTABLE;
         break;
      case WSAEINVALIDPROVIDER:
         uiErr = ZH_SOCKET_ERR_INVALIDPROVIDER;
         break;
      case WSAEPROVIDERFAILEDINIT:
         uiErr = ZH_SOCKET_ERR_PROVIDERFAILEDINIT;
         break;
      case WSAEREFUSED:
         uiErr = ZH_SOCKET_ERR_REFUSED;
         break;
      case WSATRY_AGAIN:
         uiErr = ZH_SOCKET_ERR_TRYAGAIN;
         break;
      case WSASYSNOTREADY:
         uiErr = ZH_SOCKET_ERR_SYSNOTREADY;
         break;
      case WSAVERNOTSUPPORTED:
         uiErr = ZH_SOCKET_ERR_VERNOTSUPPORTED;
         break;
      case WSANOTINITIALISED:
         uiErr = ZH_SOCKET_ERR_NOTINITIALISED;
         break;
      case WSAHOST_NOT_FOUND:
         uiErr = ZH_SOCKET_ERR_HOSTNOTFOUND;
         break;
      case WSANO_RECOVERY:
         uiErr = ZH_SOCKET_ERR_NORECOVERY;
         break;
      case WSANO_DATA:
         uiErr = ZH_SOCKET_ERR_NODATA;
         break;
      case WSASYSCALLFAILURE:
         uiErr = ZH_SOCKET_ERR_SYSCALLFAILURE;
         break;
      case WSASERVICE_NOT_FOUND:
         uiErr = ZH_SOCKET_ERR_SERVICENOTFOUND;
         break;
      case WSATYPE_NOT_FOUND:
         uiErr = ZH_SOCKET_ERR_TYPENOTFOUND;
         break;
      case WSA_E_NO_MORE:
         uiErr = ZH_SOCKET_ERR_NOMORE;
         break;
      case WSA_E_CANCELLED:
         uiErr = ZH_SOCKET_ERR_CANCELLED;
         break;
      case WSA_NOT_ENOUGH_MEMORY:
         uiErr = ZH_SOCKET_ERR_NOMEM;
         break;
      default:
         uiErr = ZH_SOCKET_ERR_OTHER;
         break;
   }
#else
   switch( err )
   {
      case 0:
         uiErr = 0;
         break;
#if defined( EPFNOSUPPORT )
      case EPFNOSUPPORT:
         uiErr = ZH_SOCKET_ERR_PFNOSUPPORT;
         break;
#endif
#if defined( EAFNOSUPPORT )
      case EAFNOSUPPORT:
         uiErr = ZH_SOCKET_ERR_AFNOSUPPORT;
         break;
#endif
#if defined( EPROTONOSUPPORT )
      case EPROTONOSUPPORT:
         uiErr = ZH_SOCKET_ERR_PROTONOSUPPORT;
         break;
#endif
      case EADDRINUSE:
         uiErr = ZH_SOCKET_ERR_ADDRINUSE;
         break;
      case EINTR:
         uiErr = ZH_SOCKET_ERR_INTERRUPT;
         break;
      case ETIMEDOUT:
         uiErr = ZH_SOCKET_ERR_TIMEOUT;
         break;
      case EISCONN:
         uiErr = ZH_SOCKET_ERR_ALREADYCONNECTED;
         break;
      case ENOTCONN:
         uiErr = ZH_SOCKET_ERR_NOTCONN;
         break;
#if defined( ECONNABORTED )
      case ECONNABORTED:
         uiErr = ZH_SOCKET_ERR_CONNABORTED;
         break;
#endif
      case ECONNRESET:
         uiErr = ZH_SOCKET_ERR_CONNRESET;
         break;
      case ECONNREFUSED:
         uiErr = ZH_SOCKET_ERR_CONNREFUSED;
         break;
      case ENETUNREACH:
         uiErr = ZH_SOCKET_ERR_NETUNREACH;
         break;
      case ENETDOWN:
         uiErr = ZH_SOCKET_ERR_NETDOWN;
         break;
#if defined( ENETRESET )
      case ENETRESET:
         uiErr = ZH_SOCKET_ERR_NETRESET;
         break;
#endif
      case EINPROGRESS:
         uiErr = ZH_SOCKET_ERR_INPROGRESS;
         break;
      case EALREADY:
         uiErr = ZH_SOCKET_ERR_ALREADY;
         break;
      case EADDRNOTAVAIL:
         uiErr = ZH_SOCKET_ERR_ADDRNOTAVAIL;
         break;
      case EROFS:
         uiErr = ZH_SOCKET_ERR_READONLY;
         break;
      case EAGAIN:
#if defined( EWOULDBLOCK )
#  if EWOULDBLOCK != EAGAIN
      case EWOULDBLOCK:
#  endif
#endif
         uiErr = ZH_SOCKET_ERR_AGAIN;
         break;
      case EPIPE:
         uiErr = ZH_SOCKET_ERR_PIPE;
         break;
      case EPERM:
      case EACCES:
         uiErr = ZH_SOCKET_ERR_ACCESS;
         break;
      case EBADF:
      case ENOTSOCK:
         uiErr = ZH_SOCKET_ERR_INVALIDHANDLE;
         break;
      case EINVAL:
         uiErr = ZH_SOCKET_ERR_INVAL;
         break;
#if defined( EPROTO )
      case EPROTO:
         uiErr = ZH_SOCKET_ERR_PROTO;
         break;
#endif
      case EPROTOTYPE:
         uiErr = ZH_SOCKET_ERR_PROTOTYPE;
         break;
      case EOPNOTSUPP:
#if defined( ESOCKTNOSUPPORT )
      case ESOCKTNOSUPPORT:
#endif
         uiErr = ZH_SOCKET_ERR_NOSUPPORT;
         break;
      case EMFILE:
      case ENFILE:
         uiErr = ZH_SOCKET_ERR_NOFILE;
         break;
      case ENOBUFS:
         uiErr = ZH_SOCKET_ERR_NOBUFS;
         break;
      case ENOMEM:
         uiErr = ZH_SOCKET_ERR_NOMEM;
         break;
      case EFAULT:
         uiErr = ZH_SOCKET_ERR_FAULT;
         break;
      case ENAMETOOLONG:
         uiErr = ZH_SOCKET_ERR_NAMETOOLONG;
         break;
      case ENOENT:
         uiErr = ZH_SOCKET_ERR_NOENT;
         break;
      case ENOTDIR:
         uiErr = ZH_SOCKET_ERR_NOTDIR;
         break;
      case ELOOP:
         uiErr = ZH_SOCKET_ERR_LOOP;
         break;
#if defined( ENOSR )
      case ENOSR:
         uiErr = ZH_SOCKET_ERR_NOSR;
         break;
#endif
#if defined( ERESTARTSYS )
      case ERESTARTSYS:
         uiErr = ZH_SOCKET_ERR_RESTARTSYS;
         break;
#endif
      case EDESTADDRREQ:
         uiErr = ZH_SOCKET_ERR_DESTADDRREQ;
         break;
      case EMSGSIZE:
         uiErr = ZH_SOCKET_ERR_MSGSIZE;
         break;
      case ENOPROTOOPT:
         uiErr = ZH_SOCKET_ERR_NOPROTOOPT;
         break;
      case ESHUTDOWN:
         uiErr = ZH_SOCKET_ERR_SHUTDOWN;
         break;
#if defined( ETOOMANYREFS )
      case ETOOMANYREFS:
         uiErr = ZH_SOCKET_ERR_TOOMANYREFS;
         break;
#endif
#if defined( EHOSTDOWN )
      case EHOSTDOWN:
         uiErr = ZH_SOCKET_ERR_HOSTDOWN;
         break;
#endif
      case EHOSTUNREACH:
         uiErr = ZH_SOCKET_ERR_HOSTUNREACH;
         break;
      case ENOTEMPTY:
         uiErr = ZH_SOCKET_ERR_NOTEMPTY;
         break;
#if defined( EUSERS )
      case EUSERS:
         uiErr = ZH_SOCKET_ERR_USERS;
         break;
#endif
#if defined( EDQUOT )
      case EDQUOT:
         uiErr = ZH_SOCKET_ERR_DQUOT;
         break;
#endif
#if defined( ESTALE )
      case ESTALE:
         uiErr = ZH_SOCKET_ERR_STALE;
         break;
#endif
#if defined( EREMOTE )
      case EREMOTE:
         uiErr = ZH_SOCKET_ERR_REMOTE;
         break;
#endif
#if defined( EPROCLIM )
      case EPROCLIM:
         uiErr = ZH_SOCKET_ERR_PROCLIM;
         break;
#endif
#if defined( EDISCON )
      case EDISCON:
         uiErr = ZH_SOCKET_ERR_DISCON;
         break;
#endif
#if defined( ENOMORE )
      case ENOMORE:
         uiErr = ZH_SOCKET_ERR_NOMORE;
         break;
#endif
#if defined( ECANCELLED )
      case ECANCELLED:
         uiErr = ZH_SOCKET_ERR_CANCELLED;
         break;
#endif
#if defined( EINVALIDPROCTABLE )
      case EINVALIDPROCTABLE:
         uiErr = ZH_SOCKET_ERR_INVALIDPROCTABLE;
         break;
#endif
#if defined( EINVALIDPROVIDER )
      case EINVALIDPROVIDER:
         uiErr = ZH_SOCKET_ERR_INVALIDPROVIDER;
         break;
#endif
#if defined( EPROVIDERFAILEDINIT )
      case EPROVIDERFAILEDINIT:
         uiErr = ZH_SOCKET_ERR_PROVIDERFAILEDINIT;
         break;
#endif
#if defined( EREFUSED )
#  if EREFUSED != ECONNREFUSED
      case EREFUSED:
         uiErr = ZH_SOCKET_ERR_REFUSED;
         break;
#  endif
#endif
      default:
         uiErr = ZH_SOCKET_ERR_OTHER;
         break;
   }
#endif

   pError->uiSocketError = uiErr;
   pError->iSocketOsError = err;
}

#if ! defined( ZH_OS_WIN )
static void zh_socketSetResolveError( int err )
{
   PZH_IOERRORS pError = zh_stackIOErrors();
   ZH_ERRCODE uiErr;

   switch( err )
   {
      case 0:
         uiErr = 0;
         break;

#if defined( ZH_HAS_ADDRINFO ) || defined( ZH_HAS_NAMEINFO )

      /* getaddrinfo() / getnameinfo() */
#if defined( EAI_AGAIN )
      case EAI_AGAIN:
         uiErr = ZH_SOCKET_ERR_TRYAGAIN;
         break;
#endif
#if defined( EAI_BADFLAGS )
      case EAI_BADFLAGS:
         uiErr = ZH_SOCKET_ERR_INVAL;
         break;
#endif
#if defined( EAI_FAIL )
      case EAI_FAIL:
         uiErr = ZH_SOCKET_ERR_NORECOVERY;
         break;
#endif
#if defined( EAI_ADDRFAMILY )
      case EAI_ADDRFAMILY:
#endif
#if defined( EAI_FAMILY )
      case EAI_FAMILY:
         uiErr = ZH_SOCKET_ERR_AFNOSUPPORT;
         break;
#endif
#if defined( EAI_MEMORY )
      case EAI_MEMORY:
         uiErr = ZH_SOCKET_ERR_NOMEM;
         break;
#endif
#if defined( EAI_NODATA )
      case EAI_NODATA:
         uiErr = ZH_SOCKET_ERR_NODATA;
         break;
#endif
#if defined( EAI_NONAME )
      case EAI_NONAME:
         uiErr = ZH_SOCKET_ERR_HOSTNOTFOUND;
         break;
#endif
#if defined( EAI_OVERFLOW )
      case EAI_OVERFLOW:
         uiErr = ZH_SOCKET_ERR_NAMETOOLONG;
         break;
#endif
#if defined( EAI_SERVICE )
      case EAI_SERVICE:
         uiErr = ZH_SOCKET_ERR_TYPENOTFOUND;
         break;
#endif
#if defined( EAI_SOCKTYPE )
      case EAI_SOCKTYPE:
         uiErr = ZH_SOCKET_ERR_NOSUPPORT;
         break;
#endif
#if defined( EAI_SYSTEM )
      case EAI_SYSTEM:
         uiErr = ZH_SOCKET_ERR_SYSCALLFAILURE;
         break;
#endif

#else /* ! ZH_HAS_ADDRINFO && ! ZH_HAS_NAMEINFO */

      /* gethostbyname() / gethostbyaddr() */
#if defined( TRY_AGAIN )
      case TRY_AGAIN:
         uiErr = ZH_SOCKET_ERR_TRYAGAIN;
         break;
#endif
#if defined( HOST_NOT_FOUND )
      case HOST_NOT_FOUND:
         uiErr = ZH_SOCKET_ERR_HOSTNOTFOUND;
         break;
#endif
#if defined( NO_RECOVERY )
      case NO_RECOVERY:
         uiErr = ZH_SOCKET_ERR_NORECOVERY;
         break;
#endif
#if defined( NO_DATA ) || defined( NO_ADDRESS )
#if defined( NO_DATA )
      case NO_DATA:
#endif
#if defined( NO_ADDRESS ) && \
    ( ! defined( NO_DATA ) || NO_ADDRESS != NO_DATA )
      case NO_ADDRESS:
#endif
         uiErr = ZH_SOCKET_ERR_NODATA;
         break;
#endif

#endif /* ! ZH_HAS_ADDRINFO && ! ZH_HAS_NAMEINFO */
      default:
         uiErr = ZH_SOCKET_ERR_WRONGADDR;
         break;
   }

   pError->uiSocketError = uiErr;
   pError->iSocketOsError = err;
}
#endif

#if defined( ZH_SOCKET_TRANSLATE_DOMAIN )
static int zh_socketTransDomain( int domain, int *err )
{
   switch( domain )
   {
      case ZH_SOCKET_AF_INET:
#if   defined( AF_INET )
         domain = AF_INET;
#elif defined( PF_INET )
         domain = PF_INET;
#else
         if( err )
            *err = ZH_SOCKET_ERR_PFNOSUPPORT;
#endif
         break;

      case ZH_SOCKET_AF_INET6:
#if   defined( AF_INET6 )
         domain = AF_INET6;
#elif defined( PF_INET6 )
         domain = PF_INET6;
#else
         if( err )
            *err = ZH_SOCKET_ERR_PFNOSUPPORT;
#endif
         break;

      case ZH_SOCKET_AF_LOCAL:
#if   defined( AF_LOCAL )
         domain = AF_LOCAL;
#elif defined( AF_UNIX )
         domain = AF_UNIX;
#elif defined( PF_LOCAL )
         domain = PF_LOCAL;
#elif defined( PF_UNIX )
         domain = PF_UNIX;
#else
         if( err )
            *err = ZH_SOCKET_ERR_PFNOSUPPORT;
#endif
         break;

      case ZH_SOCKET_AF_PACKET:
#if   defined( AF_PACKET )
         domain = AF_PACKET;
#elif defined( PF_PACKET )
         domain = PF_PACKET;
#else
         if( err )
            *err = ZH_SOCKET_ERR_PFNOSUPPORT;
#endif
         break;

      case ZH_SOCKET_AF_IPX:
#if   defined( AF_IPX )
         domain = AF_IPX;
#elif defined( PF_IPX )
         domain = PF_IPX;
#else
         if( err )
            *err = ZH_SOCKET_ERR_PFNOSUPPORT;
#endif
         break;

      default:
         if( err )
            *err = ZH_SOCKET_ERR_PFNOSUPPORT;
   }
   return domain;
}
#endif

#if defined( ZH_SOCKET_TRANSLATE_TYPE )
static int zh_socketTransType( int type, int *err )
{
   switch( type )
   {
      case ZH_SOCKET_PT_STREAM:
#if   defined( SOCK_STREAM )
         type = SOCK_STREAM;
#else
         if( err )
            *err = ZH_SOCKET_ERR_PROTONOSUPPORT;
#endif
         break;

      case ZH_SOCKET_PT_DGRAM:
#if   defined( SOCK_DGRAM )
         type = SOCK_DGRAM;
#else
         if( err )
            *err = ZH_SOCKET_ERR_PROTONOSUPPORT;
#endif
         break;

      case ZH_SOCKET_PT_SEQPACKET:
#if   defined( SOCK_SEQPACKET )
         type = SOCK_SEQPACKET;
#else
         if( err )
            *err = ZH_SOCKET_ERR_PROTONOSUPPORT;
#endif
         break;

      case ZH_SOCKET_PT_RAW:
#if   defined( SOCK_RAW )
         type = SOCK_RAW;
#else
         if( err )
            *err = ZH_SOCKET_ERR_PROTONOSUPPORT;
#endif
         break;

      case ZH_SOCKET_PT_RDM:
#if   defined( SOCK_RDM )
         type = SOCK_RDM;
#else
         if( err )
            *err = ZH_SOCKET_ERR_PROTONOSUPPORT;
#endif
         break;

      default:
         if( err )
            *err = ZH_SOCKET_ERR_PROTONOSUPPORT;
   }
   return type;
}
#endif

static int zh_socketTransFlags( int flags )
{
   int iResult = 0;

   if( flags )
   {
#ifdef MSG_OOB
      if( flags & ZH_SOCKET_MSG_OOB )
         iResult |= MSG_OOB;
#endif
#ifdef MSG_PEEK
      if( flags & ZH_SOCKET_MSG_PEEK )
         iResult |= MSG_PEEK;
#endif
#ifdef MSG_DONTROUTE
      if( flags & ZH_SOCKET_MSG_DONTROUTE )
         iResult |= MSG_DONTROUTE;
#endif
#ifdef MSG_WAITALL
      if( flags & ZH_SOCKET_MSG_WAITALL )
         iResult |= MSG_WAITALL;
#endif
   }
   return iResult;
}

static int zh_socketSelectRD( ZH_SOCKET sd, ZH_MAXINT timeout )
{
#if defined( ZH_HAS_POLL )
   ZH_MAXUINT timer = zh_timerInit( timeout );
   struct pollfd fds;
   int iResult;

   fds.fd = sd;
   fds.events = POLLIN;
   fds.revents = 0;

   do
   {
      int tout = timeout < 0 || timeout > 1000 ? 1000 : ( int ) timeout, iError;
      iResult = poll( &fds, 1, tout );
      iError = iResult >= 0 ? 0 : ZH_SOCK_GETERROR();
      if( iResult > 0 && ( fds.revents & POLLIN ) == 0 )
      {
         if( fds.revents & POLLNVAL )
         {
            iError = EBADF;
            iResult = -1;
         }
         else
         {
            timeout = 0;
            iResult = fds.revents & ( POLLHUP | POLLERR ) ? 1 : 0;
         }
      }
      zh_socketSetOsError( iError );
      if( iResult == -1 && ZH_SOCK_IS_EINTR( iError ) )
         iResult = 0;
   }
   while( iResult == 0 && ( timeout = zh_timerTest( timeout, &timer ) ) != 0 &&
          zh_vmRequestQuery() == 0 );

   return iResult;
#else /* ! ZH_HAS_POLL */
   int iResult;
   struct timeval tv;
   fd_set rfds;
#  if ! defined( ZH_HAS_SELECT_TIMER )
   ZH_MAXUINT timer = zh_timerInit( timeout );
#  else
   tv.tv_sec = ( long ) ( timeout / 1000 );
   tv.tv_usec = ( long ) ( timeout % 1000 ) * 1000;
#  endif

   for( ;; )
   {
      int iError;

      if( timeout < 0 )
      {
         tv.tv_sec = 1;
         tv.tv_usec = 0;
      }
#  if ! defined( ZH_HAS_SELECT_TIMER )
      else
      {
         tv.tv_sec = ( long ) ( timeout / 1000 );
         tv.tv_usec = ( long ) ( timeout % 1000 ) * 1000;
      }
#  endif

      FD_ZERO( &rfds );
      FD_SET( ( ZH_SOCKET_T ) sd, &rfds );
      iResult = select( ( int ) ( sd + 1 ), &rfds, NULL, NULL, &tv );
      iError = iResult >= 0 ? 0 : ZH_SOCK_GETERROR();
      zh_socketSetOsError( iError );

      if( iResult == -1 && ZH_SOCK_IS_EINTR( iError ) )
         iResult = 0;

#  if defined( ZH_HAS_SELECT_TIMER )
      if( iResult != 0 || timeout >= 0 || zh_vmRequestQuery() != 0 )
         break;
#  else
      if( iResult != 0 || ( timeout = zh_timerTest( timeout, &timer ) ) == 0 ||
          zh_vmRequestQuery() != 0 )
         break;
#  endif
   }

   return iResult < 0 ? -1 :
          ( iResult > 0 && FD_ISSET( ( ZH_SOCKET_T ) sd, &rfds ) ? 1 : 0 );
#endif /* ! ZH_HAS_POLL */
}

static int zh_socketSelectWR( ZH_SOCKET sd, ZH_MAXINT timeout )
{
#if defined( ZH_HAS_POLL )
   ZH_MAXUINT timer = zh_timerInit( timeout );
   struct pollfd fds;
   int iResult;

   fds.fd = sd;
   fds.events = POLLOUT;
   fds.revents = 0;

   do
   {
      int tout = timeout < 0 || timeout > 1000 ? 1000 : ( int ) timeout, iError;
      iResult = poll( &fds, 1, tout );
      iError = iResult >= 0 ? 0 : ZH_SOCK_GETERROR();
      if( iResult > 0 && ( fds.revents & POLLOUT ) == 0 )
      {
         if( fds.revents & POLLNVAL )
         {
            iError = EBADF;
            iResult = -1;
         }
         else if( fds.revents & ( POLLHUP | POLLERR ) )
         {
            iError = EPIPE;
            iResult = -1;
         }
         else
         {
            timeout = 0;
            iResult = 0;
         }
      }
      zh_socketSetOsError( iError );
      if( iResult == -1 && ZH_SOCK_IS_EINTR( iError ) )
         iResult = 0;
   }
   while( iResult == 0 && ( timeout = zh_timerTest( timeout, &timer ) ) != 0 &&
          zh_vmRequestQuery() == 0 );

   return iResult;
#else /* ! ZH_HAS_POLL */
   int iResult;
   struct timeval tv;
   fd_set wfds;
#  if ! defined( ZH_HAS_SELECT_TIMER )
   ZH_MAXUINT timer = zh_timerInit( timeout );
#  else
   tv.tv_sec = ( long ) ( timeout / 1000 );
   tv.tv_usec = ( long ) ( timeout % 1000 ) * 1000;
#  endif

   for( ;; )
   {
      int iError;

      if( timeout < 0 )
      {
         tv.tv_sec = 1;
         tv.tv_usec = 0;
      }
#  if ! defined( ZH_HAS_SELECT_TIMER )
      else
      {
         tv.tv_sec = ( long ) ( timeout / 1000 );
         tv.tv_usec = ( long ) ( timeout % 1000 ) * 1000;
      }
#  endif

      FD_ZERO( &wfds );
      FD_SET( ( ZH_SOCKET_T ) sd, &wfds );
      iResult = select( ( int ) ( sd + 1 ), NULL, &wfds, NULL, &tv );
      iError = iResult >= 0 ? 0 : ZH_SOCK_GETERROR();
      zh_socketSetOsError( iError );

      if( iResult == -1 && ZH_SOCK_IS_EINTR( iError ) )
         iResult = 0;

#  if defined( ZH_HAS_SELECT_TIMER )
      if( iResult != 0 || timeout >= 0 || zh_vmRequestQuery() != 0 )
         break;
#  else
      if( iResult != 0 || ( timeout = zh_timerTest( timeout, &timer ) ) == 0 ||
          zh_vmRequestQuery() != 0 )
         break;
#  endif
   }

   return iResult < 0 ? -1 :
          ( iResult > 0 && FD_ISSET( ( ZH_SOCKET_T ) sd, &wfds ) ? 1 : 0 );
#endif /* ! ZH_HAS_POLL */
}

static int zh_socketSelectWRE( ZH_SOCKET sd, ZH_MAXINT timeout )
{
#if defined( ZH_HAS_POLL )
   ZH_MAXUINT timer = zh_timerInit( timeout );
   int iResult, iError, tout;
   struct pollfd fds;

   fds.fd = sd;
   fds.events = POLLOUT;
   fds.revents = 0;

   do
   {
      tout = timeout < 0 || timeout > 1000 ? 1000 : ( int ) timeout;
      iResult = poll( &fds, 1, tout );
      iError = iResult >= 0 ? 0 : ZH_SOCK_GETERROR();
      if( iResult > 0 && ( fds.revents & POLLOUT ) == 0 )
      {
         if( fds.revents & POLLNVAL )
         {
            iError = EBADF;
            iResult = -1;
         }
         else if( fds.revents & ( POLLHUP | POLLERR ) )
         {
            iError = EPIPE;
            iResult = -1;
         }
         else
         {
            timeout = 0;
            iResult = 0;
         }
      }
      zh_socketSetOsError( iError );
      if( iResult == -1 && ZH_SOCK_IS_EINTR( iError ) )
         iResult = 0;
   }
   while( iResult == 0 && ( timeout = zh_timerTest( timeout, &timer ) ) != 0 &&
          zh_vmRequestQuery() == 0 );

   if( iResult > 0 )
   {
      socklen_t len = sizeof( iError );
      if( getsockopt( sd, SOL_SOCKET, SO_ERROR, ( char * ) &iError, &len ) != 0 )
      {
         iResult = -1;
         iError = ZH_SOCK_GETERROR();
      }
      else if( iError != 0 )
         iResult = -1;
      zh_socketSetOsError( iError );
   }
   return iResult;
#else /* ! ZH_HAS_POLL */
   int iResult, iError;
   struct timeval tv;
   fd_set wfds;
#  if defined( ZH_OS_WIN )
   fd_set efds;
#  endif
   socklen_t len;
#  if ! defined( ZH_HAS_SELECT_TIMER )
   ZH_MAXUINT timer = zh_timerInit( timeout );
#  else
   tv.tv_sec = ( long ) ( timeout / 1000 );
   tv.tv_usec = ( long ) ( timeout % 1000 ) * 1000;
#  endif

   for( ;; )
   {
      fd_set * pefds;

      if( timeout < 0 )
      {
         tv.tv_sec = 1;
         tv.tv_usec = 0;
      }
#  if ! defined( ZH_HAS_SELECT_TIMER )
      else
      {
         tv.tv_sec = ( long ) ( timeout / 1000 );
         tv.tv_usec = ( long ) ( timeout % 1000 ) * 1000;
      }
#  endif

      FD_ZERO( &wfds );
      FD_SET( ( ZH_SOCKET_T ) sd, &wfds );
#  if defined( ZH_OS_WIN )
      FD_ZERO( &efds );
      FD_SET( ( ZH_SOCKET_T ) sd, &efds );
      pefds = &efds;
#  else
      pefds = NULL;
#  endif

      iResult = select( ( int ) ( sd + 1 ), NULL, &wfds, pefds, &tv );
      iError = iResult >= 0 ? 0 : ZH_SOCK_GETERROR();
      zh_socketSetOsError( iError );
#  if defined( ZH_OS_WIN )
      if( iResult > 0 && FD_ISSET( ( ZH_SOCKET_T ) sd, pefds ) )
      {
         iResult = -1;
         len = sizeof( iError );
         if( getsockopt( sd, SOL_SOCKET, SO_ERROR, ( char * ) &iError, &len ) != 0 )
            iError = ZH_SOCK_GETERROR();
         zh_socketSetOsError( iError );
      }
      else
#  endif
      if( iResult == -1 && ZH_SOCK_IS_EINTR( iError ) )
         iResult = 0;

#  if defined( ZH_HAS_SELECT_TIMER )
      if( iResult != 0 || timeout >= 0 || zh_vmRequestQuery() != 0 )
         break;
#  else
      if( iResult != 0 || ( timeout = zh_timerTest( timeout, &timer ) ) == 0 ||
          zh_vmRequestQuery() != 0 )
         break;
#  endif
      break;
   }
#if ! defined( ZH_OS_WIN )
   if( iResult > 0 && FD_ISSET( ( ZH_SOCKET_T ) sd, &wfds ) )
   {
      len = sizeof( iError );
      if( getsockopt( sd, SOL_SOCKET, SO_ERROR, ( char * ) &iError, &len ) != 0 )
      {
         iResult = -1;
         iError = ZH_SOCK_GETERROR();
      }
#if defined( ZH_OS_DOS )
      else if( iError == EISCONN )
         iError = 0;
#endif
      else if( iError != 0 )
         iResult = -1;

      zh_socketSetOsError( iError );
   }
#endif

   return iResult < 0 ? -1 :
          ( iResult > 0 && FD_ISSET( ( ZH_SOCKET_T ) sd, &wfds ) ? 1 : 0 );
#endif /* ! ZH_HAS_POLL */
}

int zh_socketGetAddrFamily( const void * pSockAddr, unsigned len )
{
   return pSockAddr && len ? ( ( const struct sockaddr * ) pSockAddr )->sa_family : -1;
}

ZH_BOOL zh_socketLocalAddr( void ** pSockAddr, unsigned * puiLen,
                            const char * szAddr )
{
#if defined( ZH_HAS_UNIX )
   struct sockaddr_un sa;
   memset( &sa, 0, sizeof( sa ) );
#if defined( AF_UNIX )
   sa.sun_family = AF_UNIX;
#else
   sa.sun_family = AF_LOCAL;
#endif
   zh_strncpy( sa.sun_path, szAddr, sizeof( sa.sun_path ) - 1 );
   *pSockAddr = memcpy( zh_xgrab( sizeof( sa ) + 1 ), &sa, sizeof( sa ) );
   *puiLen = ( unsigned ) sizeof( sa );
   return ZH_TRUE;
#else
   ZH_SYMBOL_UNUSED( szAddr );
   *pSockAddr = NULL;
   *puiLen = 0;
   zh_socketSetError( ZH_SOCKET_ERR_AFNOSUPPORT );
   return ZH_FALSE;
#endif
}

ZH_BOOL zh_socketInetAddr( void ** pSockAddr, unsigned * puiLen,
                           const char * szAddr, int iPort )
{
#if defined( AF_INET )
   struct sockaddr_in sa;

   memset( &sa, 0, sizeof( sa ) );
   sa.sin_family = AF_INET;
   sa.sin_port = htons( ( ZH_U16 ) iPort );
   if( ! szAddr || ! *szAddr )
   {
      sa.sin_addr.s_addr = htonl( INADDR_ANY );
      *pSockAddr = memcpy( zh_xgrab( sizeof( sa ) + 1 ), &sa, sizeof( sa ) );
      *puiLen = ( unsigned ) sizeof( sa );
      return ZH_TRUE;
   }
   else
   {
#if defined( ZH_HAS_INET_PTON )
      if( inet_pton( AF_INET, szAddr, &sa.sin_addr ) > 0 )
#elif defined( ZH_HAS_INET_ATON )
      if( inet_aton( szAddr, &sa.sin_addr ) != 0 )
#else
      sa.sin_addr.s_addr = inet_addr( szAddr );
      if( sa.sin_addr.s_addr != INADDR_NONE ||
          strcmp( "255.255.255.255", szAddr ) == 0 )  /* dirty hack */
#endif
      {
         *pSockAddr = memcpy( zh_xgrab( sizeof( sa ) + 1 ), &sa, sizeof( sa ) );
         *puiLen = ( unsigned ) sizeof( sa );
         return ZH_TRUE;
      }
      else
         zh_socketSetError( ZH_SOCKET_ERR_WRONGADDR );
   }
#else
   ZH_SYMBOL_UNUSED( szAddr );
   ZH_SYMBOL_UNUSED( iPort );
   zh_socketSetError( ZH_SOCKET_ERR_AFNOSUPPORT );
#endif
   *pSockAddr = NULL;
   *puiLen = 0;
   return ZH_FALSE;
}

ZH_BOOL zh_socketInet6Addr( void ** pSockAddr, unsigned * puiLen,
                            const char * szAddr, int iPort )
{
#if defined( ZH_HAS_INET6 )
   struct sockaddr_in6 sa;

   memset( &sa, 0, sizeof( sa ) );
   sa.sin6_family = AF_INET6;
   sa.sin6_port = htons( ( ZH_U16 ) iPort );
   if( ! szAddr || ! *szAddr )
   {
#if defined( ZH_HAS_INET6_ADDR_CONST )
      memcpy( &sa.sin6_addr, &in6addr_any, sizeof( struct in6_addr ) );
#elif defined( IN6ADDR_ANY_INIT )
      memcpy( &sa.sin6_addr, &s_in6addr_any, sizeof( struct in6_addr ) );
#else
      int iTODO;
#endif
      *pSockAddr = memcpy( zh_xgrab( sizeof( sa ) + 1 ), &sa, sizeof( sa ) );
      *puiLen = ( unsigned ) sizeof( sa );
      return ZH_TRUE;
   }
   else
   {
#if defined( ZH_HAS_INET_PTON )
      int err = inet_pton( AF_INET6, szAddr, &sa.sin6_addr );
      if( err > 0 )
      {
         *pSockAddr = memcpy( zh_xgrab( sizeof( sa ) + 1 ), &sa, sizeof( sa ) );
         *puiLen = ( unsigned ) sizeof( sa );
         return ZH_TRUE;
      }
      else if( err == 0 )
         zh_socketSetError( ZH_SOCKET_ERR_WRONGADDR );
      else
         zh_socketSetError( ZH_SOCKET_ERR_AFNOSUPPORT );
#else
      int iTODO;
#endif
   }
#else
   ZH_SYMBOL_UNUSED( szAddr );
   ZH_SYMBOL_UNUSED( iPort );
   zh_socketSetError( ZH_SOCKET_ERR_AFNOSUPPORT );
#endif
   *pSockAddr = NULL;
   *puiLen = 0;
   return ZH_FALSE;
}

/* caller must free the buffer if not NULL */
char * zh_socketAddrGetName( const void * pSockAddr, unsigned len )
{
   char * szName = NULL;

   switch( zh_socketGetAddrFamily( pSockAddr, len ) )
   {
#if defined( AF_INET )
      case AF_INET:
         if( len >= sizeof( struct sockaddr_in ) )
         {
            const struct sockaddr_in * sa = ( const struct sockaddr_in * ) pSockAddr;
            const char * szAddr;
#  if defined( ZH_HAS_INET_NTOP )
            char buf[ INET_ADDRSTRLEN ];
            szAddr = inet_ntop( AF_INET, ZH_UNCONST( &sa->sin_addr ), buf, sizeof( buf ) );
#  elif defined( ZH_IS_INET_NTOA_MT_SAFE )
            szAddr = inet_ntoa( sa->sin_addr );
#  else
            char buf[ INET_ADDRSTRLEN ];
            szAddr = zh_inet_ntoa( &sa->sin_addr, buf );
#  endif
            if( szAddr )
               szName = zh_strdup( szAddr );
         }
         break;
#endif
#if defined( ZH_HAS_INET6 )
      case AF_INET6:
         if( len >= sizeof( struct sockaddr_in6 ) )
         {
            const struct sockaddr_in6 * sa = ( const struct sockaddr_in6 * ) pSockAddr;
            const char * szAddr;
#  if defined( ZH_HAS_INET_NTOP )
            char buf[ INET6_ADDRSTRLEN ];
            szAddr = inet_ntop( AF_INET6, ZH_UNCONST( &sa->sin6_addr ), buf, sizeof( buf ) );
#  else
            {
               int iTODO;
               szAddr = NULL;
               ZH_SYMBOL_UNUSED( sa );
            }
#  endif
            if( szAddr )
               szName = zh_strdup( szAddr );
         }
         break;
#endif
#if defined( ZH_HAS_UNIX )
#  if defined( AF_UNIX )
      case AF_UNIX:
#  else
      case AF_LOCAL:
#  endif
         if( len >= sizeof( struct sockaddr_un ) )
         {
            const struct sockaddr_un * sa = ( const struct sockaddr_un * ) pSockAddr;
            szName = zh_strdup( sa->sun_path );
         }
         break;
#endif
#if defined( AF_IPX )
      case AF_IPX:
         break;
#endif
#if defined( AF_PACKET )
      case AF_PACKET:
         break;
#endif
   }
   zh_socketSetError( szName ? 0 : ZH_SOCKET_ERR_AFNOSUPPORT );
   return szName;
}

int zh_socketAddrGetPort( const void * pSockAddr, unsigned len )
{
   int iPort = -1;

   switch( zh_socketGetAddrFamily( pSockAddr, len ) )
   {
#if defined( AF_INET )
      case AF_INET:
         if( len >= sizeof( struct sockaddr_in ) )
            iPort = ntohs( ( ( const struct sockaddr_in * ) pSockAddr )->sin_port );
         break;
#endif
#if defined( ZH_HAS_INET6 )
      case AF_INET6:
         if( len >= sizeof( struct sockaddr_in6 ) )
            iPort = ntohs( ( ( const struct sockaddr_in6 * ) pSockAddr )->sin6_port );
         break;
#endif
#if defined( ZH_HAS_UNIX )
#  if defined( AF_UNIX )
      case AF_UNIX:
#  else
      case AF_LOCAL:
#  endif
         break;
#endif
#if defined( AF_IPX )
      case AF_IPX:
         break;
#endif
#if defined( AF_PACKET )
      case AF_PACKET:
         break;
#endif
   }
   zh_socketSetError( iPort != -1 ? 0 : ZH_SOCKET_ERR_AFNOSUPPORT );
   return iPort;
}

ZH_BOOL zh_socketAddrFromItem( void ** pSockAddr, unsigned * puiLen, PZH_ITEM pAddrItm )
{
   ZH_BOOL fOK = ZH_FALSE;

   *pSockAddr = NULL;
   *puiLen = 0;

   if( pAddrItm && ZH_IS_ARRAY( pAddrItm ) )
   {
      if( zh_arrayLen( pAddrItm ) >= 2 &&
          ( zh_arrayGetType( pAddrItm, 1 ) & ZH_IT_NUMERIC ) != 0 )
      {
         switch( zh_arrayGetNI( pAddrItm, 1 ) )
         {
            case ZH_SOCKET_AF_INET:
               fOK = zh_socketInetAddr( pSockAddr, puiLen,
                                        zh_arrayGetCPtr( pAddrItm, 2 ),
                                        zh_arrayGetNI( pAddrItm, 3 ) );
               break;
            case ZH_SOCKET_AF_INET6:
               fOK = zh_socketInet6Addr( pSockAddr, puiLen,
                                         zh_arrayGetCPtr( pAddrItm, 2 ),
                                         zh_arrayGetNI( pAddrItm, 3 ) );
               break;
            case ZH_SOCKET_AF_LOCAL:
               fOK = zh_socketLocalAddr( pSockAddr, puiLen,
                                         zh_arrayGetCPtr( pAddrItm, 2 ) );
               break;
            case ZH_SOCKET_AF_PACKET:
            case ZH_SOCKET_AF_IPX:
               break;
         }
      }
   }
   zh_socketSetError( fOK ? 0 : ZH_SOCKET_ERR_AFNOSUPPORT );
   return fOK;
}

PZH_ITEM zh_socketAddrToItem( const void * pSockAddr, unsigned len )
{
   PZH_ITEM pAddrItm = NULL;

   switch( zh_socketGetAddrFamily( pSockAddr, len ) )
   {
#if defined( AF_INET )
      case AF_INET:
         if( len >= sizeof( struct sockaddr_in ) )
         {
            const struct sockaddr_in * sa = ( const struct sockaddr_in * ) pSockAddr;
            const char * szAddr;
#  if defined( ZH_HAS_INET_NTOP )
            char buf[ INET_ADDRSTRLEN ];
            szAddr = inet_ntop( AF_INET, ZH_UNCONST( &sa->sin_addr ), buf, sizeof( buf ) );
#  elif defined( ZH_IS_INET_NTOA_MT_SAFE )
            szAddr = inet_ntoa( sa->sin_addr );
#  else
            char buf[ INET_ADDRSTRLEN ];
            szAddr = zh_inet_ntoa( &sa->sin_addr, buf );
#  endif
            if( szAddr )
            {
               pAddrItm = zh_itemArrayNew( 3 );
               zh_arraySetNI( pAddrItm, 1, ZH_SOCKET_AF_INET );
               zh_arraySetC( pAddrItm, 2, szAddr );
               zh_arraySetNI( pAddrItm, 3, ntohs( sa->sin_port ) );
            }
         }
         break;
#endif
#if defined( ZH_HAS_INET6 )
      case AF_INET6:
         if( len >= sizeof( struct sockaddr_in6 ) )
         {
            const struct sockaddr_in6 * sa = ( const struct sockaddr_in6 * ) pSockAddr;
            const char * szAddr;
#  if defined( ZH_HAS_INET_NTOP )
            char buf[ INET6_ADDRSTRLEN ];
            szAddr = inet_ntop( AF_INET6, ZH_UNCONST( &sa->sin6_addr ), buf, sizeof( buf ) );
#  else
            {
               int iTODO;
               szAddr = NULL;
            }
#  endif
            if( szAddr )
            {
               pAddrItm = zh_itemArrayNew( 3 );
               zh_arraySetNI( pAddrItm, 1, ZH_SOCKET_AF_INET6 );
               zh_arraySetC( pAddrItm, 2, szAddr );
               zh_arraySetNI( pAddrItm, 3, ntohs( sa->sin6_port ) );
            }
         }
         break;
#endif
#if defined( ZH_HAS_UNIX )
#  if defined( AF_UNIX )
      case AF_UNIX:
#  else
      case AF_LOCAL:
#  endif
         if( len >= sizeof( struct sockaddr_un ) )
         {
            const struct sockaddr_un * sa = ( const struct sockaddr_un * ) pSockAddr;
            pAddrItm = zh_itemArrayNew( 2 );
            zh_arraySetNI( pAddrItm, 1, ZH_SOCKET_AF_LOCAL );
            zh_arraySetC( pAddrItm, 2, sa->sun_path );
         }
         break;
#endif
#if defined( AF_IPX )
      case AF_IPX:
         break;
#endif
#if defined( AF_PACKET )
      case AF_PACKET:
         break;
#endif
   }
   zh_socketSetError( pAddrItm ? 0 : ZH_SOCKET_ERR_AFNOSUPPORT );
   return pAddrItm;
}

int zh_socketGetSockName( ZH_SOCKET sd, void ** pSockAddr, unsigned * puiLen )
{
   ZH_SOCKADDR_STORAGE st;
   socklen_t len = sizeof( st );
   int ret;

   ret = getsockname( sd, &st.sa, &len );
   zh_socketSetOsError( ret == 0 ? 0 : ZH_SOCK_GETERROR() );
   if( ret == 0 )
   {
      *pSockAddr = memcpy( zh_xgrab( len + 1 ), &st.sa, len );
      *puiLen = ( unsigned ) len;
   }
   else
   {
      *pSockAddr = NULL;
      *puiLen = 0;
   }

   return ret;
}

int zh_socketGetPeerName( ZH_SOCKET sd, void ** pSockAddr, unsigned * puiLen )
{
   int ret;

#if defined( ZH_OS_LINUX ) && defined( __WATCOMC__ ) && ( __WATCOMC__ <= 1290 )
   /* it's still not supported by Linux OpenWatcom port :-( */
   ret = -1;
   zh_socketSetError( ZH_SOCKET_ERR_NOSUPPORT );
#else
   ZH_SOCKADDR_STORAGE st;
   socklen_t len = sizeof( st );

   ret = getpeername( sd, &st.sa, &len );
   zh_socketSetOsError( ret == 0 ? 0 : ZH_SOCK_GETERROR() );
   if( ret == 0 )
   {
      *pSockAddr = memcpy( zh_xgrab( len + 1 ), &st.sa, len );
      *puiLen = ( unsigned ) len;
   }
   else
#endif
   {
      *pSockAddr = NULL;
      *puiLen = 0;
   }

   return ret;
}

ZH_SOCKET zh_socketOpen( int domain, int type, int protocol )
{
   ZH_SOCKET sd = ZH_NO_SOCKET;
   int err = 0;

#if defined( ZH_SOCKET_TRANSLATE_DOMAIN )
   domain = zh_socketTransDomain( domain, &err );
#endif

#if defined( ZH_SOCKET_TRANSLATE_TYPE )
   if( err == 0 )
      type = zh_socketTransType( type, &err );
#endif

   if( err == 0 )
   {
      sd = socket( domain, type, protocol );
      zh_socketSetOsError( sd != ZH_NO_SOCKET ? 0 : ZH_SOCK_GETERROR() );
   }
   else
      zh_socketSetError( err );

   return sd;
}

int zh_socketClose( ZH_SOCKET sd )
{
   int ret;

   zh_vmUnlock();
#if defined( ZH_OS_WIN )
   ret = closesocket( sd );
#elif defined( ZH_OS_DOS )
   ret = close_s( sd );
#else
#  if defined( EINTR )
   {
      /* ignoring EINTR in close() it's quite common bug when sockets or
       * pipes are used. Without such protection it's not safe to use
       * signals in user code.
       */
      do
      {
         ret = close( sd );
      }
      while( ret == -1 && errno == EINTR );
   }
#  else
   ret = close( sd );
#  endif
#endif
   zh_socketSetOsError( ret == 0 ? 0 : ZH_SOCK_GETERROR() );
   zh_vmLock();

   return ret;
}

int zh_socketShutdown( ZH_SOCKET sd, int iMode )
{
   int ret;

#if defined( ZH_OS_WIN )
   if( iMode == ZH_SOCKET_SHUT_RD )
      iMode = SD_RECEIVE;
   else if( iMode == ZH_SOCKET_SHUT_WR )
      iMode = SD_SEND;
   else if( iMode == ZH_SOCKET_SHUT_RDWR )
      iMode = SD_BOTH;
#else
   if( iMode == ZH_SOCKET_SHUT_RD )
      iMode = SHUT_RD;
   else if( iMode == ZH_SOCKET_SHUT_WR )
      iMode = SHUT_WR;
   else if( iMode == ZH_SOCKET_SHUT_RDWR )
      iMode = SHUT_RDWR;
#endif
   else
   {
      zh_socketSetError( ZH_SOCKET_ERR_PARAMVALUE );
      return -1;
   }

#if defined( ZH_OS_LINUX ) && defined( __WATCOMC__ ) && ( __WATCOMC__ <= 1290 )
{
   int iTODO;
   /* it's still not supported by Linux OpenWatcom port :-( */
   ret = -1;
   zh_socketSetError( ZH_SOCKET_ERR_NOSUPPORT );
}
#else
   zh_vmUnlock();
   ret = shutdown( sd, iMode );
   zh_socketSetOsError( ret == 0 ? 0 : ZH_SOCK_GETERROR() );
   zh_vmLock();
#endif
   return ret;
}

int zh_socketBind( ZH_SOCKET sd, const void * pSockAddr, unsigned uiLen )
{
   int ret;

#if defined( ZH_OS_LINUX ) && defined( __WATCOMC__ ) && ( __WATCOMC__ <= 1290 )
   ret = bind( sd, ( struct sockaddr * ) pSockAddr, ( socklen_t ) uiLen );
#else
   ret = bind( sd, ( const struct sockaddr * ) pSockAddr, ( socklen_t ) uiLen );
#endif
   zh_socketSetOsError( ret == 0 ? 0 : ZH_SOCK_GETERROR() );

   return ret;
}

int zh_socketListen( ZH_SOCKET sd, int iBacklog )
{
   int ret;

   ret = listen( sd, iBacklog );
   zh_socketSetOsError( ret == 0 ? 0 : ZH_SOCK_GETERROR() );

   return ret;
}

ZH_SOCKET zh_socketAccept( ZH_SOCKET sd, void ** pSockAddr, unsigned * puiLen, ZH_MAXINT timeout )
{
   ZH_SOCKET newsd = ZH_NO_SOCKET;
   ZH_SOCKADDR_STORAGE st;
   socklen_t len = sizeof( st );
   int ret;

   zh_vmUnlock();
   if( pSockAddr && puiLen )
   {
      *pSockAddr = NULL;
      *puiLen = 0;
   }
   ret = zh_socketSelectRD( sd, timeout );
   if( ret > 0 )
   {
      int err;

      /* it's necessary to set non blocking IO to be sure that application
       * will not be frozen inside accept(). It may happen if some asynchronous
       * network error appear after above Select() or when other thread
       * accepts incoming connection (concurrent calls).
       */
      ret = timeout < 0 ? 0 : zh_socketSetBlockingIO( sd, ZH_FALSE );
      newsd = accept( sd, &st.sa, &len );
      err = newsd != ZH_NO_SOCKET ? 0 : ZH_SOCK_GETERROR();

      if( ret > 0 )
         zh_socketSetBlockingIO( sd, ZH_TRUE );
      if( newsd != ZH_NO_SOCKET )
      {
         if( pSockAddr && puiLen )
         {
            *pSockAddr = memcpy( zh_xgrab( len + 1 ), &st.sa, len );
            *puiLen = ( unsigned ) len;
         }
         /* it's not guarantied that socket returned by accept will use
          * blocking IO operations. On some systems it inherits blocking
          * IO from parent handler so we have to force blocking IO mode
          * explicitly.
          */
         zh_socketSetBlockingIO( newsd, ZH_TRUE );
      }

      zh_socketSetOsError( err );
   }
   else if( ret == 0 )
      zh_socketSetError( ZH_SOCKET_ERR_TIMEOUT );
   zh_vmLock();
   return newsd;
}

int zh_socketConnect( ZH_SOCKET sd, const void * pSockAddr, unsigned uiLen, ZH_MAXINT timeout )
{
   int ret, blk, err;

   zh_vmUnlock();

   /* set not blocking IO to implement timeout in connect() operation in
    * portable way without using signals
    */
   blk = timeout < 0 ? 0 : zh_socketSetBlockingIO( sd, ZH_FALSE );
   ret = connect( sd, ( const struct sockaddr * ) pSockAddr, ( socklen_t ) uiLen );
   err = ret == 0 ? 0 : ZH_SOCK_GETERROR();
   zh_socketSetOsError( err );
   if( ret != 0 && timeout >= 0 && ZH_SOCK_IS_EINPROGRES( err ) )
   {
      /* inside zh_socketSelectWRE() we have code which hides differences
       * between Windows and POSIX platforms in error detection.
       */
      ret = zh_socketSelectWRE( sd, timeout );
      if( ret > 0 )
      {
         zh_socketSetError( 0 );
         ret = 0;
      }
      else if( ret == 0 )
      {
         zh_socketSetError( ZH_SOCKET_ERR_TIMEOUT );
         ret = -1;
      }
   }

   if( blk > 0 )
   {
      int rawerr;
      err = zh_socketGetOsError();
      rawerr = err ? 0 : zh_socketGetError();

      zh_socketSetBlockingIO( sd, ZH_TRUE );

      if( err )
         zh_socketSetOsError( err );
      else
         zh_socketSetError( rawerr );
   }

   zh_vmLock();
   return ret;
}

long zh_socketSend( ZH_SOCKET sd, const void * data, long len, int flags, ZH_MAXINT timeout )
{
   long lSent = 0;

   zh_vmUnlock();

   if( timeout >= 0 )
   {
      lSent = zh_socketSelectWR( sd, timeout );
      if( lSent == 0 )
      {
         zh_socketSetError( ZH_SOCKET_ERR_TIMEOUT );
         lSent = -1;
      }
   }
   if( lSent >= 0 )
   {
      int iError;

      flags = zh_socketTransFlags( flags );
      /* in POSIX systems writing data to broken connection stream causes
       * that system generates SIGPIPE which has to be caught by application
       * otherwise the default action for SIGPIPE is application termination.
       * we do not want to generate it so we are setting MSG_NOSIGNAL flag.
       */
#if defined( MSG_NOSIGNAL )
      flags |= MSG_NOSIGNAL;
#endif
      do
      {
         lSent = send( sd, ( const char * ) data, len, flags );
         iError = lSent > 0 ? 0 : ZH_SOCK_GETERROR();
         zh_socketSetOsError( iError );
      }
      while( lSent == -1 && ZH_SOCK_IS_EINTR( iError ) &&
             zh_vmRequestQuery() == 0 );
   }
   zh_vmLock();

   return lSent;
}

long zh_socketSendTo( ZH_SOCKET sd, const void * data, long len, int flags,
                      const void * pSockAddr, unsigned uiSockLen, ZH_MAXINT timeout )
{
   long lSent = 0;

   zh_vmUnlock();

   if( timeout >= 0 )
   {
      lSent = zh_socketSelectWR( sd, timeout );
      if( lSent == 0 )
      {
         zh_socketSetError( ZH_SOCKET_ERR_TIMEOUT );
         lSent = -1;
      }
   }
   if( lSent >= 0 )
   {
      int iError;

      flags = zh_socketTransFlags( flags );
      /* see note above about SIGPIPE */
#if defined( MSG_NOSIGNAL )
      flags |= MSG_NOSIGNAL;
#endif
      do
      {
         lSent = sendto( sd, ( const char * ) data, len, flags,
                         ( const struct sockaddr * ) pSockAddr, ( socklen_t ) uiSockLen );
         iError = lSent > 0 ? 0 : ZH_SOCK_GETERROR();
         zh_socketSetOsError( iError );
      }
      while( lSent == -1 && ZH_SOCK_IS_EINTR( iError ) &&
             zh_vmRequestQuery() == 0 );
   }
   zh_vmLock();

   return lSent;
}

long zh_socketRecv( ZH_SOCKET sd, void * data, long len, int flags, ZH_MAXINT timeout )
{
   long lReceived = 0;

   zh_vmUnlock();

   if( timeout >= 0 )
   {
      lReceived = zh_socketSelectRD( sd, timeout );
      if( lReceived == 0 )
      {
         zh_socketSetError( ZH_SOCKET_ERR_TIMEOUT );
         lReceived = -1;
      }
   }
   if( lReceived >= 0 )
   {
      int iError;

      flags = zh_socketTransFlags( flags );
      do
      {
         lReceived = recv( sd, ( char * ) data, len, flags );
         iError = lReceived > 0 ? 0 : ZH_SOCK_GETERROR();
         zh_socketSetOsError( iError );
      }
      while( lReceived == -1 && ZH_SOCK_IS_EINTR( iError ) &&
             zh_vmRequestQuery() == 0 );
   }
   zh_vmLock();

   return lReceived;
}

long zh_socketRecvFrom( ZH_SOCKET sd, void * data, long len, int flags, void ** pSockAddr, unsigned * puiSockLen, ZH_MAXINT timeout )
{
   long lReceived = 0;

   zh_vmUnlock();

   if( pSockAddr && puiSockLen )
   {
      *pSockAddr = NULL;
      *puiSockLen = 0;
   }

   if( timeout >= 0 )
   {
      lReceived = zh_socketSelectRD( sd, timeout );
      if( lReceived == 0 )
      {
         zh_socketSetError( ZH_SOCKET_ERR_TIMEOUT );
         lReceived = -1;
      }
   }
   if( lReceived >= 0 )
   {
      ZH_SOCKADDR_STORAGE st;
      socklen_t salen = sizeof( st );
      int iError;

      flags = zh_socketTransFlags( flags );
      do
      {
         lReceived = recvfrom( sd, ( char * ) data, len, flags, &st.sa, &salen );
         iError = lReceived > 0 ? 0 : ZH_SOCK_GETERROR();
         zh_socketSetOsError( iError );
      }
      while( lReceived == -1 && ZH_SOCK_IS_EINTR( iError ) &&
             zh_vmRequestQuery() == 0 );

      if( lReceived != -1 && pSockAddr && puiSockLen )
      {
         *pSockAddr = memcpy( zh_xgrab( salen + 1 ), &st.sa, salen );
         *puiSockLen = ( unsigned ) salen;
      }
   }
   zh_vmLock();

   return lReceived;
}

int zh_socketSetBlockingIO( ZH_SOCKET sd, ZH_BOOL fBlocking )
{
   int ret;

#if defined( ZH_OS_WIN )
   u_long mode = fBlocking ? 0 : 1;
   ret = ioctlsocket( sd, FIONBIO, &mode );
   zh_socketSetOsError( ret != -1 ? 0 : ZH_SOCK_GETERROR() );
   if( ret == 0 )
      ret = 1;




#elif defined( ZH_OS_DOS )
   int mode = fBlocking ? 0 : 1;
   ret = ioctlsocket( sd, FIONBIO, ( char * ) &mode );
   zh_socketSetOsError( ret != -1 ? 0 : ZH_SOCK_GETERROR() );
   if( ret == 0 )
      ret = 1;
#elif defined( O_NONBLOCK )
   ret = fcntl( sd, F_GETFL, 0 );
   if( ret != -1 )
   {
      ZH_BOOL fBlocked;
      int flags;
      fBlocked = ( ret & O_NONBLOCK ) == 0;
      if( fBlocking ? ! fBlocked : fBlocked )
      {
         if( fBlocking )
            flags = ret & ~O_NONBLOCK;
         else
            flags = ret | O_NONBLOCK;
         ret = fcntl( sd, F_SETFL, flags );
         if( ret == 0 )
            ret = 1;
      }
      else
         ret = 0;
   }
   zh_socketSetOsError( ret != -1 ? 0 : ZH_SOCK_GETERROR() );
#else
   int iTODO;
   ZH_SYMBOL_UNUSED( sd );
   ZH_SYMBOL_UNUSED( fBlocking );
   zh_socketSetError( ZH_SOCKET_ERR_NOSUPPORT );
   ret = -1;
#endif
   return ret;
}

int zh_socketSetNoDelay( ZH_SOCKET sd, ZH_BOOL fNoDelay )
{
   int ret;

#if defined( TCP_NODELAY )
   /*
    * Turn off the Nagle algorithm for the specified socket.
    * The Nagle algorithm says that we should delay sending
    * partial packets in the hopes of getting more data.
    * There are bad interactions between persistent connections and
    * Nagle's algorithm that have severe performance penalties.
    */
   int val = fNoDelay ? 1 : 0;
   ret = setsockopt( sd, IPPROTO_TCP, TCP_NODELAY, ( const char * ) &val, sizeof( val ) );
   zh_socketSetOsError( ret != -1 ? 0 : ZH_SOCK_GETERROR() );
#else
   int iTODO;
   ZH_SYMBOL_UNUSED( sd );
   ZH_SYMBOL_UNUSED( fNoDelay );
   zh_socketSetError( ZH_SOCKET_ERR_NOSUPPORT );
   ret = -1;
#endif
   return ret;
}

/* NOTE: https://msdn.microsoft.com/library/ms740621 [vszakats] */
int zh_socketSetExclusiveAddr( ZH_SOCKET sd, ZH_BOOL fExclusive )
{
   int ret;

   #if defined( ZH_OS_WIN )
      #if defined( SO_EXCLUSIVEADDRUSE )
         int val = fExclusive ? 1 : 0;
         ret = setsockopt( sd, SOL_SOCKET, SO_EXCLUSIVEADDRUSE, ( const char * ) &val, sizeof( val ) );
         zh_socketSetOsError( ret != -1 ? 0 : ZH_SOCK_GETERROR() );
      #else
         ZH_SYMBOL_UNUSED( sd );
         ZH_SYMBOL_UNUSED( fExclusive );
         zh_socketSetError( ZH_SOCKET_ERR_NOSUPPORT );
         ret = -1;
      #endif
   #else
      ZH_SYMBOL_UNUSED( sd );
      ZH_SYMBOL_UNUSED( fExclusive );
      zh_socketSetOsError( 0 );
      ret = 0;
   #endif
   return ret;
}

int zh_socketSetReuseAddr( ZH_SOCKET sd, ZH_BOOL fReuse )
{
   int ret;

   /* it allows to reuse port immediately without timeout used to
    * clean all pending connections addressed to previous port owner
    */
   #if defined( ZH_OS_WIN )
      /* SO_REUSEADDR in MS-Windows makes something completely different
       * then in other OS-es
       */
      ZH_SYMBOL_UNUSED( sd );
      ZH_SYMBOL_UNUSED( fReuse );
      zh_socketSetError( ZH_SOCKET_ERR_NOSUPPORT );
      ret = -1;
   #else
   {
      int val = fReuse ? 1 : 0;
      ret = setsockopt( sd, SOL_SOCKET, SO_REUSEADDR, ( const char * ) &val, sizeof( val ) );
      zh_socketSetOsError( ret != -1 ? 0 : ZH_SOCK_GETERROR() );
   }
   #endif
   return ret;
}

int zh_socketSetKeepAlive( ZH_SOCKET sd, ZH_BOOL fKeepAlive )
{
   int val = fKeepAlive ? 1 : 0, ret;

   ret = setsockopt( sd, SOL_SOCKET, SO_KEEPALIVE, ( const char * ) &val, sizeof( val ) );
   zh_socketSetOsError( ret != -1 ? 0 : ZH_SOCK_GETERROR() );
   return ret;
}

int zh_socketSetBroadcast( ZH_SOCKET sd, ZH_BOOL fBroadcast )
{
#if defined( SO_BROADCAST )
   int val = fBroadcast ? 1 : 0, ret;
   ret = setsockopt( sd, SOL_SOCKET, SO_BROADCAST, ( const char * ) &val, sizeof( val ) );
   zh_socketSetOsError( ret != -1 ? 0 : ZH_SOCK_GETERROR() );
   return ret;
#else
   ZH_SYMBOL_UNUSED( sd );
   ZH_SYMBOL_UNUSED( fBroadcast );

   return -1;
#endif
}

int zh_socketSetSndBufSize( ZH_SOCKET sd, int iSize )
{
   int ret = setsockopt( sd, SOL_SOCKET, SO_SNDBUF, ( const char * ) &iSize, sizeof( iSize ) );

   zh_socketSetOsError( ret != -1 ? 0 : ZH_SOCK_GETERROR() );
   return ret;
}

int zh_socketSetRcvBufSize( ZH_SOCKET sd, int iSize )
{
   int ret = setsockopt( sd, SOL_SOCKET, SO_RCVBUF, ( const char * ) &iSize, sizeof( iSize ) );

   zh_socketSetOsError( ret != -1 ? 0 : ZH_SOCK_GETERROR() );
   return ret;
}

int zh_socketGetSndBufSize( ZH_SOCKET sd, int * piSize )
{
   socklen_t len = sizeof( * piSize );
   int ret = getsockopt( sd, SOL_SOCKET, SO_SNDBUF, ( char * ) piSize, &len );

   zh_socketSetOsError( ret != -1 ? 0 : ZH_SOCK_GETERROR() );
   return ret;
}

int zh_socketGetRcvBufSize( ZH_SOCKET sd, int * piSize )
{
   socklen_t len = sizeof( * piSize );
   int ret = getsockopt( sd, SOL_SOCKET, SO_RCVBUF, ( char * ) piSize, &len );

   zh_socketSetOsError( ret != -1 ? 0 : ZH_SOCK_GETERROR() );
   return ret;
}

int zh_socketSetMulticast( ZH_SOCKET sd, int af, const char * szAddr )
{
   if( af == ZH_SOCKET_AF_INET )
   {
#if defined( IP_ADD_MEMBERSHIP ) /* && defined( IPPROTO_IP ) */
      struct ip_mreq mreq;
      int ret;

#if defined( ZH_HAS_INET_PTON )
      ret = inet_pton( AF_INET, szAddr, &mreq.imr_multiaddr ) > 0 ? 0 : -1;
#elif defined( ZH_HAS_INET_ATON )
      ret = inet_aton( szAddr, &mreq.imr_multiaddr ) != 0 ? 0 : -1;
#else
      mreq.imr_multiaddr.s_addr = inet_addr( szAddr );
      ret = ( mreq.imr_multiaddr.s_addr != INADDR_NONE ||
              strcmp( "255.255.255.255", szAddr ) == 0 ) ? 0 : -1; /* dirty hack */
#endif
      mreq.imr_interface.s_addr = htonl( INADDR_ANY );

      if( ret == 0 )
         ret = setsockopt( sd, IPPROTO_IP, IP_ADD_MEMBERSHIP, ( const char * ) &mreq, sizeof( mreq ) );
      zh_socketSetOsError( ret != -1 ? 0 : ZH_SOCK_GETERROR() );
      return ret;
#else
      int iTODO;
#endif
   }
#if defined( ZH_HAS_INET6 )
   else if( af == ZH_SOCKET_AF_INET6 )
   {
#if defined( ZH_HAS_INET_PTON )
      struct ipv6_mreq mreq;
      int err = inet_pton( AF_INET6, szAddr, &mreq.ipv6mr_multiaddr );
      if( err > 0 )
      {
         int ret;
         mreq.ipv6mr_interface = 0;
#if ! defined( IPV6_JOIN_GROUP ) && defined( IPV6_ADD_MEMBERSHIP )
#  define IPV6_JOIN_GROUP  IPV6_ADD_MEMBERSHIP
#endif
         ret = setsockopt( sd, IPPROTO_IPV6, IPV6_JOIN_GROUP, ( const char * ) &mreq, sizeof( mreq ) );
         zh_socketSetOsError( ret != -1 ? 0 : ZH_SOCK_GETERROR() );
         return ret;
      }
      else if( err == 0 )
         zh_socketSetError( ZH_SOCKET_ERR_WRONGADDR );
      else
         zh_socketSetError( ZH_SOCKET_ERR_AFNOSUPPORT );
      return -1;
#else
      int iTODO;
#endif
   }
#endif

   zh_socketSetError( ZH_SOCKET_ERR_AFNOSUPPORT );
   return -1;
}

int zh_socketSelectRead( ZH_SOCKET sd, ZH_MAXINT timeout )
{
   int ret;

   zh_vmUnlock();
   ret = zh_socketSelectRD( sd, timeout );
   zh_vmLock();

   return ret;
}

int zh_socketSelectWrite( ZH_SOCKET sd, ZH_MAXINT timeout )
{
   int ret;

   zh_vmUnlock();
   ret = zh_socketSelectWR( sd, timeout );
   zh_vmLock();

   return ret;
}

int zh_socketSelectWriteEx( ZH_SOCKET sd, ZH_MAXINT timeout )
{
   int ret;

   zh_vmUnlock();
   ret = zh_socketSelectWRE( sd, timeout );
   zh_vmLock();

   return ret;
}

static ZH_SOCKET s_socketSelectCallback( PZH_ITEM pItem )
{
   ZH_SOCKET sd = ZH_NO_SOCKET;

   if( pItem )
   {
      if( ZH_IS_NUMERIC( pItem ) )
         sd = ( ZH_SOCKET ) zh_itemGetNInt( pItem );
      else if( ZH_IS_POINTER( pItem ) )
      {
         sd = zh_socketItemGet( pItem );
         if( sd == ZH_NO_SOCKET )
            sd = ( ZH_SOCKET ) ( ZH_PTRUINT ) zh_itemGetPtr( pItem );
      }
   }
   return sd;
}

#if defined( ZH_HAS_POLL )
static int s_socketPollCheck( ZH_SOCKET sd, struct pollfd * pfds, nfds_t nfds )
{
   nfds_t npos;

   for( npos = 0; npos < nfds; ++npos )
   {
      if( pfds[ npos ].fd == sd )
         return ( int ) npos;
   }
   return -1;
}
#endif /* ZH_HAS_POLL */

int zh_socketSelect( PZH_ITEM pArrayRD, ZH_BOOL fSetRD,
                     PZH_ITEM pArrayWR, ZH_BOOL fSetWR,
                     PZH_ITEM pArrayEX, ZH_BOOL fSetEX,
                     ZH_MAXINT timeout, ZH_SOCKET_FUNC pFunc )
{
#if defined( ZH_HAS_POLL )
   ZH_SOCKET sd;
   ZH_SIZE nLen, nPos, ul;
   int iResult, iError, tout, iPos, i;
   PZH_ITEM pItemSets[ 3 ];
   ZH_BOOL pSet[ 3 ];
   int pEvents[ 3 ];
   struct pollfd * pfds = NULL;
   nfds_t nfds = 0, ncnt = 0;

   if( pFunc == NULL )
      pFunc = s_socketSelectCallback;

   pItemSets[ 0 ] = pArrayRD;
   pItemSets[ 1 ] = pArrayWR;
   pItemSets[ 2 ] = pArrayEX;
   pSet[ 0 ] = fSetRD;
   pSet[ 1 ] = fSetWR;
   pSet[ 2 ] = fSetEX;
   pEvents[ 0 ] = POLLIN;
   pEvents[ 1 ] = POLLOUT;
   pEvents[ 2 ] = POLLPRI;

   for( i = 0; i < 3; i++ )
   {
      if( pItemSets[ i ] )
         ncnt += ( nfds_t ) zh_arrayLen( pItemSets[ i ] );
   }

   if( ncnt > 0 )
      pfds = ( struct pollfd * ) zh_xgrab( ncnt * sizeof( struct pollfd ) );

   for( i = 0; i < 3; i++ )
   {
      nLen = pItemSets[ i ] ? zh_arrayLen( pItemSets[ i ] ) : 0;
      for( ul = 1; ul <= nLen && nfds < ncnt; ul++ )
      {
         sd = pFunc( zh_arrayGetItemPtr( pItemSets[ i ], ul ) );
         if( sd != ZH_NO_SOCKET )
         {
            iPos = s_socketPollCheck( sd, pfds, nfds );
            if( iPos < 0 )
            {
               iPos = ( int ) nfds++;
               pfds[ iPos ].fd = sd;
               pfds[ iPos ].revents = pfds[ iPos ].events = 0;
            }
            pfds[ iPos ].events |= pEvents[ i ];
         }
      }
   }

   if( zh_vmRequestQuery() == 0 )
   {
      ZH_MAXUINT timer = zh_timerInit( timeout );

      zh_vmUnlock();
      do
      {
         tout = timeout < 0 || timeout > 1000 ? 1000 : ( int ) timeout;
         iResult = poll( pfds, nfds, tout );
         iError = iResult >= 0 ? 0 : ZH_SOCK_GETERROR();
         zh_socketSetOsError( iError );
         if( iResult == -1 && ZH_SOCK_IS_EINTR( iError ) )
            iResult = 0;
      }
      while( iResult == 0 && ( timeout = zh_timerTest( timeout, &timer ) ) != 0 &&
             zh_vmRequestQuery() == 0 );
      zh_vmLock();

      pEvents[ 0 ] |= POLLHUP | POLLPRI;
      pEvents[ 2 ] |= POLLHUP | POLLERR | POLLNVAL;
      for( i = 0; i < 3; i++ )
      {
         if( pItemSets[ i ] && pSet[ i ] )
         {
            nPos = 0;
            if( iResult > 0 )
            {
               nLen = zh_arrayLen( pItemSets[ i ] );
               for( ul = 1; ul <= nLen; ul++ )
               {
                  sd = pFunc( zh_arrayGetItemPtr( pItemSets[ i ], ul ) );
                  if( sd != ZH_NO_SOCKET )
                  {
                     iPos = s_socketPollCheck( sd, pfds, nfds );
                     if( iPos >= 0 &&
                         ( pfds[ iPos ].revents & pEvents[ i ] ) != 0 )
                     {
                        if( ++nPos != ul )
                           zh_itemCopy( zh_arrayGetItemPtr( pItemSets[ i ], nPos ),
                                        zh_arrayGetItemPtr( pItemSets[ i ], ul ) );
                     }
                  }
               }
            }
            zh_arraySize( pItemSets[ i ], nPos );
         }
      }
   }
   else
   {
      zh_socketSetError( ZH_SOCKET_ERR_INVALIDHANDLE );
      iResult = -1;
   }

   if( pfds )
      zh_xfree( pfds );

   return iResult;
#else /* ! ZH_HAS_POLL */
   ZH_SOCKET maxsd, sd;
   int i, ret, iError;
   ZH_SIZE nLen, nPos, ul;
   PZH_ITEM pItemSets[ 3 ];
   ZH_BOOL pSet[ 3 ];
   fd_set fds[ 3 ], * pfds[ 3 ];
   struct timeval tv;
   ZH_MAXUINT timer;

   if( pFunc == NULL )
      pFunc = s_socketSelectCallback;

   pItemSets[ 0 ] = pArrayRD;
   pItemSets[ 1 ] = pArrayWR;
   pItemSets[ 2 ] = pArrayEX;
   pSet[ 0 ] = fSetRD;
   pSet[ 1 ] = fSetWR;
   pSet[ 2 ] = fSetEX;

   timer = zh_timerInit( timeout );

   do
   {
      maxsd = 0;
      for( i = 0; i < 3; i++ )
      {
         ret = 0;
         nLen = pItemSets[ i ] ? zh_arrayLen( pItemSets[ i ] ) : 0;
         if( nLen > 0 )
         {
            FD_ZERO( &fds[ i ] );
            for( ul = 1; ul <= nLen; ul++ )
            {
               sd = pFunc( zh_arrayGetItemPtr( pItemSets[ i ], ul ) );
               if( sd != ZH_NO_SOCKET )
               {
                  if( maxsd < sd )
                     maxsd = sd;
                  FD_SET( ( ZH_SOCKET_T ) sd, &fds[ i ] );
                  ret = 1;
               }
            }
         }
         pfds[ i ] = ret ? &fds[ i ] : NULL;
      }

      if( zh_vmRequestQuery() != 0 )
      {
         zh_socketSetError( ZH_SOCKET_ERR_INVALIDHANDLE );
         pSet[ 0 ] = pSet[ 1 ] = pSet[ 2 ] = ZH_FALSE;
         ret = -1;
         break;
      }

      if( timeout < 0 || timeout >= 1000 )
      {
         tv.tv_sec = 1;
         tv.tv_usec = 0;
      }
      else
      {
         tv.tv_sec = ( long ) ( timeout / 1000 );
         tv.tv_usec = ( long ) ( timeout % 1000 ) * 1000;
      }

      zh_vmUnlock();

      ret = select( ( int ) ( maxsd + 1 ), pfds[ 0 ], pfds[ 1 ], pfds[ 2 ], &tv );
      iError = ret >= 0 ? 0 : ZH_SOCK_GETERROR();
      zh_socketSetOsError( iError );
      if( ret == -1 && ZH_SOCK_IS_EINTR( iError ) )
         ret = 0;

      zh_vmLock();
   }
   while( ret == 0 && ( timeout = zh_timerTest( timeout, &timer ) ) != 0 &&
          zh_vmRequestQuery() == 0 );

   for( i = 0; i < 3; i++ )
   {
      if( pfds[ i ] && pSet[ i ] )
      {
         nPos = 0;
         if( ret > 0 )
         {
            nLen = zh_arrayLen( pItemSets[ i ] );
            for( ul = 1; ul <= nLen; ul++ )
            {
               sd = pFunc( zh_arrayGetItemPtr( pItemSets[ i ], ul ) );
               if( sd != ZH_NO_SOCKET && FD_ISSET( ( ZH_SOCKET_T ) sd, pfds[ i ] ) )
               {
                  if( ++nPos != ul )
                  {
                     zh_itemCopy( zh_arrayGetItemPtr( pItemSets[ i ], nPos ),
                                  zh_arrayGetItemPtr( pItemSets[ i ], ul ) );
                  }
               }
            }
         }
         zh_arraySize( pItemSets[ i ], nPos );
      }
   }

   return ret;
#endif /* ! ZH_HAS_POLL */
}


/*
 * DNS functions
 */
ZH_BOOL zh_socketResolveInetAddr( void ** pSockAddr, unsigned * puiLen, const char * szAddr, int iPort )
{
#if defined( AF_INET )
   struct sockaddr_in sa;
   ZH_BOOL fTrans;

   memset( &sa, 0, sizeof( sa ) );
   sa.sin_family = AF_INET;
   sa.sin_port = htons( ( ZH_U16 ) iPort );
   if( ! szAddr || ! *szAddr )
   {
      sa.sin_addr.s_addr = htonl( INADDR_ANY );
      *pSockAddr = memcpy( zh_xgrab( sizeof( sa ) + 1 ), &sa, sizeof( sa ) );
      *puiLen = ( unsigned ) sizeof( sa );
      return ZH_TRUE;
   }

#if defined( ZH_HAS_INET_PTON )
   fTrans = inet_pton( AF_INET, szAddr, &sa.sin_addr ) > 0;
#elif defined( ZH_HAS_INET_ATON )
   fTrans = inet_aton( szAddr, &sa.sin_addr ) != 0;
#else
   sa.sin_addr.s_addr = inet_addr( szAddr );
   fTrans = sa.sin_addr.s_addr != INADDR_NONE ||
            strcmp( "255.255.255.255", szAddr ) == 0; /* dirty hack */
#endif

   if( ! fTrans )
   {
#if defined( ZH_HAS_ADDRINFO )
      struct addrinfo hints, * res = NULL;
      int iError;

      zh_vmUnlock();
      memset( &hints, 0, sizeof( hints ) );
      hints.ai_family = AF_INET;
      iError = getaddrinfo( szAddr, NULL, &hints, &res );
      zh_socketSetResolveError( iError );
      if( iError == 0 )
      {
         if( ( int ) res->ai_addrlen >= ( int ) sizeof( struct sockaddr_in ) &&
             zh_socketGetAddrFamily( res->ai_addr, ( unsigned ) res->ai_addrlen ) == AF_INET )
         {
            sa.sin_addr.s_addr = ( ( struct sockaddr_in * ) res->ai_addr )->sin_addr.s_addr;
            fTrans = ZH_TRUE;
         }
         freeaddrinfo( res );
      }
      zh_vmLock();
#else
      struct hostent * he;

      zh_vmUnlock();
      he = gethostbyname( szAddr );
      zh_socketSetResolveError( he == NULL ? ZH_SOCK_GETHERROR() : 0 );
      if( he && he->h_addr_list[ 0 ] )
      {
         sa.sin_addr.s_addr = ( ( struct in_addr * ) he->h_addr_list[ 0 ] )->s_addr;
         fTrans = ZH_TRUE;
      }
      zh_vmLock();
#endif
   }

   if( fTrans )
   {
      *pSockAddr = memcpy( zh_xgrab( sizeof( sa ) + 1 ), &sa, sizeof( sa ) );
      *puiLen = ( unsigned ) sizeof( sa );
      return ZH_TRUE;
   }
#else
   ZH_SYMBOL_UNUSED( szAddr );
   ZH_SYMBOL_UNUSED( iPort );
   zh_socketSetError( ZH_SOCKET_ERR_AFNOSUPPORT );
#endif
   *pSockAddr = NULL;
   *puiLen = 0;
   return ZH_FALSE;
}

char * zh_socketResolveAddr( const char * szAddr, int af )
{
   char * szResult = NULL;
   ZH_BOOL fTrans = ZH_FALSE;
   int iError = 0;

   if( ! szAddr || ! *szAddr )
      return NULL;

   if( af == ZH_SOCKET_AF_INET )
   {
      struct in_addr sin;
#if defined( ZH_HAS_INET_PTON )
      fTrans = inet_pton( AF_INET, szAddr, &sin ) > 0;
#elif defined( ZH_HAS_INET_ATON )
      fTrans = inet_aton( szAddr, &sin ) != 0;
#else
      sin.s_addr = inet_addr( szAddr );
      fTrans = sin.s_addr != INADDR_NONE ||
            strcmp( "255.255.255.255", szAddr ) == 0; /* dirty hack */
#endif

#if ! defined( ZH_HAS_ADDRINFO )
      if( ! fTrans )
      {
         struct hostent * he;

         zh_vmUnlock();
         he = gethostbyname( szAddr );
         if( he && he->h_addr_list[ 0 ] )
         {
            sin.s_addr = ( ( struct in_addr * ) he->h_addr_list[ 0 ] )->s_addr;
            fTrans = ZH_TRUE;
         }
         else
            iError = ZH_SOCK_GETHERROR();
         zh_vmLock();
      }
#endif

      if( fTrans )
      {
#  if defined( ZH_HAS_INET_NTOP )
         char buf[ INET_ADDRSTRLEN ];
         szAddr = inet_ntop( AF_INET, &sin, buf, sizeof( buf ) );
#  elif defined( ZH_IS_INET_NTOA_MT_SAFE )
         szAddr = inet_ntoa( sin );
#  else
         char buf[ INET_ADDRSTRLEN ];
         szAddr = zh_inet_ntoa( &sin, buf );
#  endif
         szResult = zh_strdup( szAddr );
      }
   }
#if defined( ZH_HAS_INET6 )
   else if( af == ZH_SOCKET_AF_INET6 )
   {
#if defined( ZH_HAS_INET_PTON )
      struct in6_addr sin;
      fTrans = inet_pton( AF_INET6, szAddr, &sin ) > 0;
      if( fTrans )
      {
#  if defined( ZH_HAS_INET_NTOP )
         char buf[ INET6_ADDRSTRLEN ];
         szAddr = inet_ntop( AF_INET6, &sin, buf, sizeof( buf ) );
#  else
         int iTODO;
#  endif
         szResult = zh_strdup( szAddr );
      }
#else
      int iTODO;
      fTrans = ZH_FALSE;
#endif
   }
#endif

   if( ! fTrans )
   {
#if defined( ZH_HAS_ADDRINFO )
      struct addrinfo hints, * res = NULL;

      zh_vmUnlock();
#  if defined( ZH_SOCKET_TRANSLATE_DOMAIN )
      af = zh_socketTransDomain( af, NULL );
#  endif
      memset( &hints, 0, sizeof( hints ) );
      hints.ai_family = af;
      iError = getaddrinfo( szAddr, NULL, &hints, &res );
      if( iError == 0 )
      {
         szResult = zh_socketAddrGetName( res->ai_addr, ( unsigned ) res->ai_addrlen );
         freeaddrinfo( res );
      }
      zh_vmLock();
#endif
   }
   zh_socketSetResolveError( iError );

   return szResult;
}

PZH_ITEM zh_socketGetHosts( const char * szAddr, int af )
{
   PZH_ITEM pItem = NULL;

#if defined( ZH_HAS_ADDRINFO )
   struct addrinfo hints, * res = NULL, * ai;
   int iResult;

   zh_vmUnlock();
#if defined( ZH_SOCKET_TRANSLATE_DOMAIN )
   af = zh_socketTransDomain( af, NULL );
#endif
   memset( &hints, 0, sizeof( hints ) );
   hints.ai_family = af;
   iResult = getaddrinfo( szAddr, NULL, &hints, &res );
   zh_socketSetResolveError( iResult );
   zh_vmLock();

   if( iResult == 0 )
   {
      int iCount = 0;
      ai = res;
      while( ai )
      {
         ++iCount;
         ai = ai->ai_next;
      }
      if( iCount )
      {
         pItem = zh_itemArrayNew( iCount );
         ai = res;
         iCount = 0;
         while( ai )
         {
            char * szResult = zh_socketAddrGetName( res->ai_addr, ( unsigned ) res->ai_addrlen );
            if( szResult )
            {
               int i;
               for( i = 1; i <= iCount; ++i )
               {
                  if( strcmp( zh_arrayGetCPtr( pItem, i ), szResult ) == 0 )
                  {
                     zh_xfree( szResult );
                     szResult = NULL;
                     break;
                  }
               }
               if( szResult )
               {
                  ++iCount;
                  if( ! zh_arraySetCLPtr( pItem, iCount, szResult, strlen( szResult ) ) )
                     zh_xfree( szResult );
               }
            }
            ai = ai->ai_next;
         }
         zh_arraySize( pItem, iCount );
      }
      freeaddrinfo( res );
   }
#else /* ! ZH_HAS_ADDRINFO */

   if( af == ZH_SOCKET_AF_INET )
   {
      struct hostent * he = NULL;
      int iCount = 0;

      zh_vmUnlock();

      /* gethostbyname() in Windows and OS/2 does not accept direct IP
       * addresses
       */
#if defined( ZH_OS_WIN ) && defined( ZH_HAS_GETHOSTBYADDR )
      {
         struct in_addr sia;

#if defined( ZH_HAS_INET_PTON )
         if( inet_pton( AF_INET, szAddr, &sia ) > 0 )
#elif defined( ZH_HAS_INET_ATON )
         if( inet_aton( szAddr, &sia ) != 0 )
#else
         sia.s_addr = inet_addr( szAddr );
         if( sia.s_addr != INADDR_NONE ||
             strcmp( "255.255.255.255", szAddr ) == 0 )  /* dirty hack */
#endif
         {
            he = gethostbyaddr( ( const char * ) &sia, sizeof( sia ), AF_INET );
         }
      }
#endif
      if( he == NULL )
         he = gethostbyname( szAddr );

      zh_socketSetResolveError( he == NULL ? ZH_SOCK_GETHERROR() : 0 );

      zh_vmLock();

      if( he )
      {
         while( he->h_addr_list[ iCount ] )
            ++iCount;
      }
      if( iCount > 0 )
      {
         pItem = zh_itemArrayNew( iCount );
         do
         {
            struct in_addr * sin = ( struct in_addr * ) he->h_addr_list[ iCount - 1 ];
#  if defined( ZH_HAS_INET_NTOP )
            char buf[ INET_ADDRSTRLEN ];
            szAddr = inet_ntop( AF_INET, sin, buf, sizeof( buf ) );
#  elif defined( ZH_IS_INET_NTOA_MT_SAFE )
            szAddr = inet_ntoa( *sin );
#  else
            char buf[ INET_ADDRSTRLEN ];
            szAddr = zh_inet_ntoa( sin, buf );
#  endif
            zh_arraySetC( pItem, iCount, szAddr );
         }
         while( --iCount );
      }
   }
#if defined( ZH_HAS_INET6 )
   else if( af == ZH_SOCKET_AF_INET6 )
   {
      int iTODO;
   }
#endif

#endif

   return pItem;
}

PZH_ITEM zh_socketGetAliases( const char * szAddr, int af )
{
   /* TODO: implement it */
   ZH_SYMBOL_UNUSED( szAddr );
   ZH_SYMBOL_UNUSED( af );
   zh_socketSetError( ZH_SOCKET_ERR_AFNOSUPPORT );
   return NULL;
}

char * zh_socketGetHostName( const void * pSockAddr, unsigned len )
{
   char * szResult = NULL;
   int af = zh_socketGetAddrFamily( pSockAddr, len );

   if( af != -1 )
   {
#if defined( ZH_HAS_NAMEINFO )
      #if ! defined( NI_MAXHOST )
         #define NI_MAXHOST  1025
      #endif
      char szHost[ NI_MAXHOST ];
      int iResult;

      zh_vmUnlock();
      iResult = getnameinfo( ( const struct sockaddr * ) pSockAddr, len, szHost, NI_MAXHOST, NULL, 0, 0 );
      zh_socketSetResolveError( iResult );
      zh_vmLock();
      if( iResult == 0 )
         szResult = zh_strdup( szHost );
#elif defined( ZH_HAS_ADDRINFO )
      char * szAddr = zh_socketAddrGetName( pSockAddr, len );
      if( szAddr )
      {
         struct addrinfo hints, * res = NULL;
         int iError;

         zh_vmUnlock();
         memset( &hints, 0, sizeof( hints ) );
         hints.ai_family = af;
         hints.ai_flags = AI_CANONNAME;
         iError = getaddrinfo( szAddr, NULL, &hints, &res );
         zh_socketSetResolveError( iError );
         if( iError == 0 )
         {
            if( res->ai_canonname )
               szResult = zh_strdup( res->ai_canonname );
            freeaddrinfo( res );
         }
         zh_vmLock();
      }
#else /* ! ZH_HAS_ADDRINFO */
      struct hostent * he = NULL;

      if( af == AF_INET )
      {
#if defined( ZH_HAS_GETHOSTBYADDR )
         const struct sockaddr_in * sa = ( const struct sockaddr_in * ) pSockAddr;
         zh_vmUnlock();
         he = gethostbyaddr( ( const char * ) &sa->sin_addr, sizeof( sa->sin_addr ), af );
         zh_socketSetResolveError( he == NULL ? ZH_SOCK_GETHERROR() : 0 );
         zh_vmLock();
#else
         char * szAddr = zh_socketAddrGetName( pSockAddr, len );
         if( szAddr )
         {
            zh_vmUnlock();
            he = gethostbyname( szAddr );
            zh_socketSetResolveError( he == NULL ? ZH_SOCK_GETHERROR() : 0 );
            zh_vmLock();
         }
#endif
      }
#if defined( ZH_HAS_INET6 ) && defined( ZH_HAS_GETHOSTBYADDR )
      else if( af == AF_INET6 )
      {
         const struct sockaddr_in6 * sa = ( const struct sockaddr_in6 * ) pSockAddr;
         zh_vmUnlock();
         he = gethostbyaddr( ( const char * ) &sa->sin6_addr, sizeof( sa->sin6_addr ), af );
         zh_socketSetResolveError( he == NULL ? ZH_SOCK_GETHERROR() : 0 );
         zh_vmLock();
      }
#endif
      if( he && he->h_name )
         szResult = zh_strdup( he->h_name );
#endif /* ! ZH_HAS_ADDRINFO */
   }
   return szResult;
}


/*
 * IFACEs
 */
#if defined( ZH_OS_WIN ) || defined( SIOCGIFCONF )
static void zh_socketArraySetInetAddr( PZH_ITEM pItem, ZH_SIZE nPos,
                                       const void * pSockAddr, unsigned len )
{
   char * szAddr = zh_socketAddrGetName( pSockAddr, len );

   if( szAddr )
   {
      if( ! zh_arraySetCLPtr( pItem, nPos, szAddr, strlen( szAddr ) ) )
         zh_xfree( szAddr );
   }
}
#endif
#if defined( ZH_OS_WIN ) && ! defined( SIOCGIFCONF )
static ZH_SIZE zh_socketArrayFindInetAddr( const char * szAddr,
                                           PZH_ITEM pArray, ZH_SIZE nPos )
{
   ZH_SIZE nLen = zh_arrayLen( pArray );

   for( ; nPos <= nLen; ++nPos )
   {
      PZH_ITEM pIfItem = zh_arrayGetItemPtr( pArray, nPos );

      if( strcmp( zh_arrayGetCPtr( pIfItem, ZH_SOCKET_IFINFO_ADDR ), szAddr ) == 0 )
         return nPos;
   }
   return 0;
}
#endif

PZH_ITEM zh_socketGetIFaces( int af, ZH_BOOL fNoAliases )
{
   PZH_ITEM pArray = NULL;
   PZH_ITEM pItem = NULL;
   int iError = 0;

/*
 * TODO: add support for alternative long interface introduced in some
 *       new systems using 'struct lifreq' with SIOCGLIF* ioctls instead
 *       of 'struct ifreq' and SIOCGIF*
 */
#if defined( SIOCGIFCONF )
   ZH_SOCKET sd;

   sd = zh_socketOpen( af ? af : ZH_SOCKET_AF_INET, ZH_SOCKET_PT_DGRAM, 0 );
   if( sd != ZH_NO_SOCKET )
   {
      struct ifconf ifc;
      struct ifreq * pifr;
      char * buf, * ptr;
      const char * pLastName = NULL;
      int len = 0, size, iLastName = 0, iLastFamily = 0, flags, family;

#  if defined( ZH_SOCKET_TRANSLATE_DOMAIN )
      af = zh_socketTransDomain( af, NULL );
#  endif
#  ifdef SIOCGIFNUM
      if( ioctl( sd, SIOCGIFNUM, &len ) == -1 )
         len = 0;
#  endif
      if( len <= 0 )
         len = 0x8000;
      len *= sizeof( struct ifreq );
      buf = ( char * ) zh_xgrab( len );

      ifc.ifc_len = len;
      ifc.ifc_buf = ( caddr_t ) buf;

      /* Warning: On some platforms this code can effectively work only with
       *          IP4 interfaces and IP6 will need different implementation.
       */

      if( ioctl( sd, SIOCGIFCONF, &ifc ) != -1 )
      {
         for( ptr = ( char * ) ifc.ifc_buf, size = ifc.ifc_len; size > 0; )
         {
            pifr = ( struct ifreq * ) ptr;
            family = pifr->ifr_addr.sa_family;
#  if defined( ZH_HAS_SOCKADDR_SA_LEN )
            len = pifr->ifr_addr.sa_len;
            if( len < ( int ) sizeof( struct sockaddr ) )
               len = sizeof( struct sockaddr );
#  else
            switch( family )
            {
#     if defined( ZH_HAS_INET6 )
               case AF_INET6:
                  len = sizeof( struct sockaddr_in6 );
                  break;
#     endif
#     if defined( AF_INET )
               case AF_INET:
#     endif
               default:
                  len = sizeof( struct sockaddr );
                  break;
            }
#  endif
            len += sizeof( pifr->ifr_name );
            if( len < ( int ) sizeof( struct ifreq ) )
               len = ( int ) sizeof( struct ifreq );
            ptr += len;
            size -= len;

            if( af && family != af )
               continue;

            /* skip alias devices */
            if( fNoAliases )
            {
               const char * cptr = strchr( pifr->ifr_name, ':' );

               len = cptr ? ( int ) ( cptr - pifr->ifr_name ) :
                            ( int ) strlen( pifr->ifr_name );
               if( pLastName && len == iLastName && family == iLastFamily &&
                   memcmp( pLastName, pifr->ifr_name, len ) == 0 )
                  continue;
               pLastName = pifr->ifr_name;
               iLastName = len;
               iLastFamily = family;
            }

            {
               struct ifreq ifr = *pifr;
               if( ioctl(  sd, SIOCGIFFLAGS, &ifr ) == -1 )
                  continue;
               flags = ifr.ifr_flags;
            }

            if( ( flags & IFF_UP ) == 0 )
               continue;

            if( pItem == NULL )
               pItem = zh_itemNew( NULL );

            zh_arrayNew( pItem, ZH_SOCKET_IFINFO_LEN );

            pifr->ifr_name[ sizeof( pifr->ifr_name ) - 1 ] = '\0';
            zh_arraySetC( pItem, ZH_SOCKET_IFINFO_NAME, pifr->ifr_name );

            switch( family )
            {
#  if defined( ZH_HAS_INET6 )
               case AF_INET6:
                  len = sizeof( struct sockaddr_in6 );
                  family = ZH_SOCKET_AF_INET6;
                  break;
#  endif
#  if defined( AF_INET )
               case AF_INET:
                  len = sizeof( struct sockaddr_in );
                  family = ZH_SOCKET_AF_INET;
                  break;
#  endif
               default:
                  len = 0;
                  break;
            }
            zh_arraySetNI( pItem, ZH_SOCKET_IFINFO_FAMILY, family );

            if( len )
            {
               zh_socketArraySetInetAddr( pItem, ZH_SOCKET_IFINFO_ADDR,
                                          &pifr->ifr_addr, len );

#  if defined( SIOCGIFNETMASK )
#     ifndef ifr_netmask
#        define ifr_netmask   ifr_addr
#     endif
               if( ioctl( sd, SIOCGIFNETMASK, pifr ) != -1 )
                  zh_socketArraySetInetAddr( pItem, ZH_SOCKET_IFINFO_NETMASK,
                                             &pifr->ifr_netmask, len );
#  endif
#  if defined( SIOCGIFBRDADDR )
               if( flags & IFF_BROADCAST )
               {
                  if( ioctl( sd, SIOCGIFBRDADDR, pifr ) != -1 )
                     zh_socketArraySetInetAddr( pItem, ZH_SOCKET_IFINFO_BROADCAST,
                                                &pifr->ifr_broadaddr, len );
               }
#  endif
#  if defined( SIOCGIFDSTADDR )
               if( flags & IFF_POINTOPOINT )
               {
                  if( ioctl( sd, SIOCGIFDSTADDR, pifr ) != -1 )
                     zh_socketArraySetInetAddr( pItem, ZH_SOCKET_IFINFO_P2PADDR,
                                                &pifr->ifr_dstaddr, len );
               }
#  endif
#  if defined( SIOCGIFHWADDR )
#     ifndef ifr_hwaddr
#        define ifr_hwaddr    ifr_addr
#     endif
               if( ioctl( sd, SIOCGIFHWADDR, pifr ) != -1 )
               {
                  char hwaddr[ 24 ];
                  unsigned char * data;
                  data = ( unsigned char * ) &pifr->ifr_hwaddr.sa_data[ 0 ];
                  zh_snprintf( hwaddr, sizeof( hwaddr ),
                               "%02X:%02X:%02X:%02X:%02X:%02X",
                               data[ 0 ], data[ 1 ], data[ 2 ],
                               data[ 3 ], data[ 4 ], data[ 5 ] );
                  zh_arraySetC( pItem, ZH_SOCKET_IFINFO_HWADDR, hwaddr );
               }
#  elif defined( SIOCGENADDR )
               if( ioctl( sd, SIOCGENADDR, pifr ) != -1 )
               {
                  char hwaddr[ 24 ];
                  unsigned char * data;
                  data = ( unsigned char * ) &pifr->ifr_enaddr[ 0 ];
                  zh_snprintf( hwaddr, sizeof( hwaddr ),
                               "%02X:%02X:%02X:%02X:%02X:%02X",
                               data[ 0 ], data[ 1 ], data[ 2 ],
                               data[ 3 ], data[ 4 ], data[ 5 ] );
                  zh_arraySetC( pItem, ZH_SOCKET_IFINFO_HWADDR, hwaddr );
               }
#  endif
            }

            flags = ( ( flags & IFF_UP ) ?
                      ZH_SOCKET_IFF_UP : 0 ) |
                    ( ( flags & IFF_BROADCAST ) ?
                      ZH_SOCKET_IFF_BROADCAST : 0 ) |
                    ( ( flags & IFF_LOOPBACK ) ?
                      ZH_SOCKET_IFF_LOOPBACK : 0 ) |
                    ( ( flags & IFF_POINTOPOINT ) ?
                      ZH_SOCKET_IFF_POINTOPOINT : 0 ) |
                    ( ( flags & IFF_MULTICAST ) ?
                      ZH_SOCKET_IFF_MULTICAST : 0 );
            zh_arraySetNI( pItem, ZH_SOCKET_IFINFO_FLAGS, flags );

            if( pArray == NULL )
               pArray = zh_itemArrayNew( 0 );
            zh_arrayAddForward( pArray, pItem );
         }
      }
      else
         iError = ZH_SOCK_GETERROR();
      zh_xfree( buf );
      zh_socketClose( sd );
   }
#elif defined( ZH_OS_WIN )
   ZH_SOCKET sd;

   ZH_SYMBOL_UNUSED( fNoAliases );

   sd = zh_socketOpen( af ? af : ZH_SOCKET_AF_INET, ZH_SOCKET_PT_DGRAM, 0 );
   if( sd != ZH_NO_SOCKET )
   {
      DWORD dwBuffer = 0x8000 * sizeof( INTERFACE_INFO );
      void * pBuffer = zh_xgrab( dwBuffer );
      LPINTERFACE_INFO pIfInfo = ( LPINTERFACE_INFO ) pBuffer;

      if( WSAIoctl( sd, SIO_GET_INTERFACE_LIST, NULL, 0, pIfInfo, dwBuffer,
                    &dwBuffer, 0, 0 ) != SOCKET_ERROR )
      {
         int iCount = dwBuffer / sizeof( INTERFACE_INFO );

         while( iCount-- )
         {
            u_long flags = pIfInfo->iiFlags;

            if( flags & IFF_UP )
            {
               if( pItem == NULL )
                  pItem = zh_itemNew( NULL );
               zh_arrayNew( pItem, ZH_SOCKET_IFINFO_LEN );

               zh_arraySetNI( pItem, ZH_SOCKET_IFINFO_FAMILY,
                              pIfInfo->iiAddress.Address.sa_family );

               zh_socketArraySetInetAddr( pItem, ZH_SOCKET_IFINFO_ADDR,
                                          &pIfInfo->iiAddress,
                                          sizeof( pIfInfo->iiAddress ) );
               zh_socketArraySetInetAddr( pItem, ZH_SOCKET_IFINFO_NETMASK,
                                          &pIfInfo->iiNetmask,
                                          sizeof( pIfInfo->iiNetmask ) );
               if( flags & IFF_BROADCAST )
                  zh_socketArraySetInetAddr( pItem, ZH_SOCKET_IFINFO_BROADCAST,
                                             &pIfInfo->iiBroadcastAddress,
                                             sizeof( pIfInfo->iiBroadcastAddress ) );
               if( flags & IFF_POINTTOPOINT )
                  zh_socketArraySetInetAddr( pItem, ZH_SOCKET_IFINFO_P2PADDR,
                                             &pIfInfo->iiBroadcastAddress,
                                             sizeof( pIfInfo->iiBroadcastAddress ) );

               flags = ( ( flags & IFF_UP ) ?
                         ZH_SOCKET_IFF_UP : 0 ) |
                       ( ( flags & IFF_BROADCAST ) ?
                         ZH_SOCKET_IFF_BROADCAST : 0 ) |
                       ( ( flags & IFF_LOOPBACK ) ?
                         ZH_SOCKET_IFF_LOOPBACK : 0 ) |
                       ( ( flags & IFF_POINTTOPOINT ) ?
                         ZH_SOCKET_IFF_POINTOPOINT : 0 ) |
                       ( ( flags & IFF_MULTICAST ) ?
                         ZH_SOCKET_IFF_MULTICAST : 0 );
               zh_arraySetNI( pItem, ZH_SOCKET_IFINFO_FLAGS, flags );

               /* Windows does not support interface names like other OS-es
                * use interface IP address instead
                */
               zh_arraySet( pItem, ZH_SOCKET_IFINFO_NAME,
                            zh_arrayGetItemPtr( pItem, ZH_SOCKET_IFINFO_ADDR ) );

               if( pArray == NULL )
                  pArray = zh_itemArrayNew( 0 );
               zh_arrayAddForward( pArray, pItem );
            }
            pIfInfo++;
         }

         if( pArray && zh_arrayLen( pArray ) > 0 )
         {
            PIP_ADAPTER_INFO pAdapterInfo;
            ULONG ulBufLen = sizeof( IP_ADAPTER_INFO );
            DWORD dwResult;

            pAdapterInfo = ( PIP_ADAPTER_INFO ) zh_xgrab( ulBufLen );
            dwResult = GetAdaptersInfo( pAdapterInfo, &ulBufLen );
            if( dwResult == ERROR_BUFFER_OVERFLOW )
            {
               zh_xfree( pAdapterInfo );
               pAdapterInfo = ( PIP_ADAPTER_INFO ) zh_xgrab( ulBufLen );
               dwResult = GetAdaptersInfo( pAdapterInfo, &ulBufLen );
            }
            if( dwResult == NO_ERROR )
            {
               PIP_ADAPTER_INFO pAdapter = pAdapterInfo;

               do
               {
                  PIP_ADDR_STRING pIpAddress = &pAdapter->IpAddressList;

                  do
                  {
                     ZH_SIZE nPos = 0;

                     while( ( nPos = zh_socketArrayFindInetAddr( pIpAddress->IpAddress.String,
                                                                 pArray, nPos + 1 ) ) != 0 )
                     {
                        PZH_ITEM pIfItem = zh_arrayGetItemPtr( pArray, nPos );
                        if( ! zh_arrayGetCPtr( pIfItem, ZH_SOCKET_IFINFO_HWADDR )[ 0 ] )
                        {
                           char hwaddr[ 3 * MAX_ADAPTER_ADDRESS_LENGTH ];
                           UINT count, size = 0;

                           for( count = 0; count < pAdapter->AddressLength; ++count )
                           {
                              if( count )
                                 hwaddr[ size++ ] = ':';
                              size += zh_snprintf( hwaddr + size, sizeof( hwaddr ) - size,
                                                   "%02X", ( int ) pAdapter->Address[ count ] );
                           }
                           zh_arraySetCL( pIfItem, ZH_SOCKET_IFINFO_HWADDR, hwaddr, size );
                        }
                     }
                     pIpAddress = pIpAddress->Next;
                  }
                  while( pIpAddress );

                  pAdapter = pAdapter->Next;
               }
               while( pAdapter );
            }
         }
      }
      else
         iError = ZH_SOCK_GETERROR();

      zh_xfree( pBuffer );
      zh_socketClose( sd );
   }
#else
   int iTODO;
   ZH_SYMBOL_UNUSED( af );
   ZH_SYMBOL_UNUSED( fNoAliases );
   zh_socketSetError( ZH_SOCKET_ERR_AFNOSUPPORT );
#endif

   if( pItem )
      zh_itemRelease( pItem );

   if( iError != 0 )
      zh_socketSetOsError( iError );

   return pArray;
}
#endif /* ! ZH_SOCKET_OFF */
