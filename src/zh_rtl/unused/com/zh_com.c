/*
 * Serial communication functions
 *
 * Copyright 2010 Przemyslaw Czerpak
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

#ifndef _GNU_SOURCE
#  define _GNU_SOURCE
#endif

#include "zh_api.h"

#if defined( ZH_OS_UNIX )
#     if ! defined( ZH_HAS_TERMIOS )
#        define ZH_HAS_TERMIOS
#     endif
#endif


#if defined( ZH_HAS_TERMIOS )
#  include <termios.h>
#  include <fcntl.h>
#  include <sys/ioctl.h>
#  include <unistd.h>
#  include <errno.h>
#  if defined( ZH_OS_UNIX )
#     include <sys/time.h>
#     include <sys/types.h>
#     if ! defined( ZH_HAS_POLL ) && ! defined( ZH_NO_POLL ) && \
         defined( _POSIX_C_SOURCE ) && _POSIX_C_SOURCE >= 200112L
         /* use poll() instead of select() to avoid FD_SETSIZE (1024 in Linux)
            file handle limit */
#        define ZH_HAS_POLL
#     endif
#     if defined( ZH_HAS_POLL )
#        include <poll.h>
#     endif
#  endif
#  if defined( ZH_OS_HPUX )
#     include <sys/modem.h>
#  endif
#elif defined( ZH_HAS_SIOLIB )
#  include <sioLib.h>
#elif defined( ZH_HAS_PMCOM )
#  include "../../src/3rd/hbpmcom/com.h"
#elif defined( ZH_OS_WIN )
#  include <windows.h>
#  include "zh_winuni.h"
#endif

#include "zh_apifs.h"
#include "zh_item_api.h"
#include "zh_apicom.h"
#include "zh_vm.h"
#include "zh_init.h"
#include "zh_date.h"
#include "zh_thread.h"

typedef struct
{
#if defined( ZH_HAS_TERMIOS )
   ZH_FHANDLE     fd;
#  if ! defined( ZH_OS_UNIX )
   ZH_MAXINT      rdtimeout;
#  endif
#elif defined( ZH_OS_WIN )
   HANDLE         hComm;
   ZH_MAXINT      rdtimeout;
   ZH_MAXINT      wrtimeout;
#endif
   int            status;
   int            error;
   int            oserr;
   int            port;
   char *         name;
}
ZH_COM, * PZH_COM;

static ZH_CRITICAL_NEW( s_comMtx );
#define ZH_COM_LOCK()      do { zh_threadEnterCriticalSection( &s_comMtx )
#define ZH_COM_UNLOCK()    zh_threadLeaveCriticalSection( &s_comMtx ); } while( 0 )

static ZH_COM s_comList[ ZH_COM_PORT_MAX ];

static void zh_comCloseAll( void )
{
   int iPort;

   for( iPort = 0; iPort < ZH_COM_PORT_MAX; ++iPort )
   {
      if( s_comList[ iPort ].status & ZH_COM_OPEN )
         zh_comClose( iPort + 1 );

      if( s_comList[ iPort ].name )
      {
         zh_xfree( s_comList[ iPort ].name );
         s_comList[ iPort ].name = NULL;
      }
   }
}

static void zh_comSetComError( PZH_COM pCom, int iError )
{
   pCom->error = iError;
   pCom->oserr = 0;
}

static PZH_COM zh_comGetPort( int iPort, int iStatus )
{
   if( iPort >= 1 && iPort <= ZH_COM_PORT_MAX )
   {
      PZH_COM pCom = &s_comList[ iPort - 1 ];
      if( iStatus == ZH_COM_ANY || ( iStatus & pCom->status ) != 0 )
         return pCom;
      if( iStatus & ZH_COM_ENABLED )
         zh_comSetComError( pCom, ZH_COM_ERR_WRONGPORT );
      else
         zh_comSetComError( pCom, ZH_COM_ERR_CLOSED );
   }
   return NULL;
}

static const char * zh_comGetNameRaw( PZH_COM pCom, char * buffer, int size )
{
   const char * name = pCom->name;

   if( name == NULL )
   {
#if defined( ZH_OS_UNIX )
#  if defined( ZH_OS_SUNOS )
      zh_snprintf( buffer, size, "/dev/tty%c", pCom->port + 'a' - 1 );
#  elif defined( ZH_OS_HPUX )
      zh_snprintf( buffer, size, "/dev/tty%dp0", pCom->port );
#  elif defined( ZH_OS_AIX )
      zh_snprintf( buffer, size, "/dev/tty%d", pCom->port );
#  elif defined( ZH_OS_MINIX )
      zh_snprintf( buffer, size, "/dev/tty%02d", pCom->port - 1 );
#  elif defined( ZH_OS_IRIX )
      zh_snprintf( buffer, size, "/dev/ttyf%d", pCom->port );
#  elif defined( ZH_OS_DIGITAL_UNIX )
      zh_snprintf( buffer, size, "/dev/ttyf%02d", pCom->port );
#  elif defined( ZH_OS_DARWIN )
      zh_snprintf( buffer, size, "/dev/cuaa%d", pCom->port - 1 );
#  else /* defined( ZH_OS_LINUX ) || defined( ZH_OS_CYGWIN ) || ... */
      zh_snprintf( buffer, size, "/dev/ttyS%d", pCom->port - 1 );
#  endif
#else
      if( zh_iswinnt() )
         zh_snprintf( buffer, size, "\\\\.\\COM%d", pCom->port );
      else
         zh_snprintf( buffer, size, "COM%d", pCom->port );
#endif
      name = buffer;
   }
   return name;
}

static const char * zh_comGetName( PZH_COM pCom, char * buffer, int size )
{
   const char * name;

   ZH_COM_LOCK();
   name = zh_comGetNameRaw( pCom, buffer, size );
   if( name != buffer )
         name = zh_strncpy( buffer, name, size - 1 );
   ZH_COM_UNLOCK();

   return name;
}

static int zh_comGetPortNum( const char * pszName )
{
   int iPort = 0;

#if defined( ZH_OS_UNIX )
#  if defined( ZH_OS_SUNOS )
   if( strncmp( pszName, "/dev/tty", 8 ) == 0 &&
       pszName[ 8 ] >= 'a' && pszName[ 9 ] == '\0' )
      iPort = pszName[ 8 ] - 'a' + 1;
#  else
   int iLen = 0;
#     if defined( ZH_OS_HPUX ) || defined( ZH_OS_AIX ) || defined( ZH_OS_MINIX )
   if( strncmp( pszName, "/dev/tty", 8 ) == 0 )
      iLen = 8;
#     elif defined( ZH_OS_IRIX ) || defined( ZH_OS_DIGITAL_UNIX )
   if( strncmp( pszName, "/dev/ttyf", 9 ) == 0 )
      iLen = 9;
#     elif defined( ZH_OS_DARWIN )
   if( strncmp( pszName, "/dev/cuaa", 9 ) == 0 )
      iLen = 9;
#     else /* defined( ZH_OS_LINUX ) || defined( ZH_OS_CYGWIN ) || ... */
   if( strncmp( pszName, "/dev/ttyS", 9 ) == 0 )
      iLen = 9;
#     endif
   if( iLen > 0 )
   {
      pszName += iLen;
      while( ZH_ISDIGIT( *pszName ) )
         iPort = iPort * 10 + ( *pszName++ - '0' );

#     if ! defined( ZH_OS_HPUX ) && \
         ! defined( ZH_OS_AIX ) && \
         ! defined( ZH_OS_IRIX ) && \
         ! defined( ZH_OS_DIGITAL_UNIX )
      ++iPort;
#     endif

#     if defined( ZH_OS_HPUX )
      if( strcmp( pszName, "p0" ) != 0 )
#     else
      if( *pszName != '\0' )
#     endif
         iPort = 0;
   }
#  endif
#else
   if( pszName[ 0 ] == '\\' && pszName[ 1 ] == '\\' &&
       pszName[ 2 ] == '.'  && pszName[ 3 ] == '\\' )
      pszName += 4;
   if( ZH_TOUPPER( pszName[ 0 ] ) == 'C' &&
       ZH_TOUPPER( pszName[ 1 ] ) == 'O' &&
       ZH_TOUPPER( pszName[ 2 ] ) == 'M' &&
       pszName[ 3 ] >= '1' && pszName[ 3 ] <= '9' )
   {
      pszName += 3;
      while( ZH_ISDIGIT( *pszName ) )
         iPort = iPort * ( 10 + *pszName++ - '0' );
      if( *pszName != '\0' )
         iPort = 0;
   }
#endif

   return iPort;
}

static ZH_BOOL zh_comPortCmp( const char * pszDevName1, const char * pszDevName2 )
{
#if defined( ZH_OS_UNIX )
   return strcmp( pszDevName1, pszDevName2 ) == 0;
#else
#  if defined( ZH_OS_WIN )
   if( pszDevName1[ 0 ] == '\\' && pszDevName1[ 1 ] == '\\' &&
       pszDevName1[ 2 ] == '.'  && pszDevName1[ 3 ] == '\\' )
      pszDevName1 += 4;
   if( pszDevName2[ 0 ] == '\\' && pszDevName2[ 1 ] == '\\' &&
       pszDevName2[ 2 ] == '.'  && pszDevName2[ 3 ] == '\\' )
      pszDevName2 += 4;
#  endif
   return zh_stricmp( pszDevName1, pszDevName2 ) == 0;
#endif
}

int zh_comFindPort( const char * pszDevName, ZH_BOOL fCreate )
{
   char buffer[ ZH_COM_DEV_NAME_MAX ];
   PZH_COM pCom;
   int iPort;

   if( pszDevName == NULL || *pszDevName == '\0' )
      return 0;

   iPort = zh_comGetPortNum( pszDevName );
   ZH_COM_LOCK();
   if( iPort > 0 )
   {
      pCom = zh_comGetPort( iPort, ZH_COM_ANY );
      if( pCom == NULL ||
          ! zh_comPortCmp( zh_comGetNameRaw( pCom, buffer, sizeof( buffer ) ),
                           pszDevName ) )
         iPort = 0;
   }

   if( iPort == 0 )
   {
      int iPortFree = 0;

      for( iPort = ZH_COM_PORT_MAX; iPort > 0; --iPort )
      {
         pCom = &s_comList[ iPort - 1 ];
         if( pCom->name == NULL )
         {
            if( iPortFree == 0 && iPort > 16 )
               iPortFree = iPort;
         }
         else if( zh_comPortCmp( pCom->name, pszDevName ) )
            break;
      }
#if defined( ZH_OS_UNIX )
      if( iPort == 0 && fCreate && access( pszDevName, F_OK ) == 0 )
#else
      if( iPort == 0 && fCreate )
#endif
      {
         if( iPortFree != 0 )
            iPort = iPortFree;
         else
         {
            for( iPort = ZH_COM_PORT_MAX; iPort > 0; --iPort )
            {
               pCom = &s_comList[ iPort - 1 ];
               if( ( pCom->status & ZH_COM_OPEN ) == 0 )
               {
                  if( pCom->name )
                  {
                     zh_xfree( pCom->name );
                     pCom->name = NULL;
                  }
                  break;
               }
            }
         }
         if( iPort != 0 )
         {
            pCom = &s_comList[ iPort - 1 ];
            if( ! zh_comPortCmp( zh_comGetNameRaw( pCom, buffer, sizeof( buffer ) ),
                                 pszDevName ) )
               pCom->name = zh_strdup( pszDevName );
         }
      }
   }
   ZH_COM_UNLOCK();

   return iPort;
}

const char * zh_comGetDevice( int iPort, char * buffer, int size )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_ANY );
   const char * pszName = NULL;

   if( pCom )
   {
      if( buffer && size > 0 )
         pszName = zh_comGetName( pCom, buffer, size );
      else
         pszName = pCom->name;
   }

   return pszName;
}

int zh_comSetDevice( int iPort, const char * szDevName )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_ANY );

   if( pCom )
   {
      ZH_COM_LOCK();
      if( pCom->name )
         zh_xfree( pCom->name );
      pCom->name = szDevName && *szDevName ? zh_strdup( szDevName ) : NULL;
      ZH_COM_UNLOCK();
   }

   return pCom ? 0 : -1;
}

ZH_FHANDLE zh_comGetDeviceHandle( int iPort )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_ANY );
   ZH_FHANDLE hFile = FS_ERROR;

   if( pCom )
   {
#if defined( ZH_HAS_TERMIOS )
      hFile = pCom->fd;
#elif defined( ZH_OS_WIN )
      hFile = ( ZH_FHANDLE ) pCom->hComm;
#endif
   }

   return hFile;
}

void zh_comSetError( int iPort, int iError )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_ANY );

   if( pCom )
      pCom->error = iError;
}

int zh_comGetError( int iPort )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_ANY );

   return pCom ? pCom->error : ZH_COM_ERR_WRONGPORT;
}

int zh_comGetOsError( int iPort )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_ANY );

   return pCom ? pCom->oserr : 0;
}

int zh_comLastNum( void )
{
   int iPort;

   for( iPort = ZH_COM_PORT_MAX; iPort; --iPort )
   {
      if( s_comList[ iPort - 1 ].status & ZH_COM_ENABLED )
         break;
   }
   return iPort;
}

#if defined( ZH_HAS_TERMIOS )

#define ZH_COM_IS_EINTR( pCom )  ( ( pCom )->oserr == EINTR )
#define ZH_COM_IS_EBADF( pCom )  ( ( pCom )->oserr  == EBADF )
#define ZH_COM_GETERROR()        ( errno )

#if defined( ZH_OS_LINUX )
#  define ZH_HAS_SELECT_TIMER
#endif

static void zh_comSetOsError( PZH_COM pCom, ZH_BOOL fError )
{
   pCom->oserr = fError ? ZH_COM_GETERROR() : 0;
   switch( pCom->oserr )
   {
      case 0:
         pCom->error = 0;
         break;
      case EIO:
         pCom->error = ZH_COM_ERR_IO;
         break;
      case EPIPE:
         pCom->error = ZH_COM_ERR_PIPE;
         break;
      case EBUSY:
         pCom->error = ZH_COM_ERR_BUSY;
         break;
      case EAGAIN:
         pCom->error = ZH_COM_ERR_TIMEOUT;
         break;
      case EACCES:
#if defined( ETXTBSY )
      case ETXTBSY:
#endif
#if defined( EPERM )
      case EPERM:
#endif
         pCom->error = ZH_COM_ERR_ACCESS;
         break;
      case ENOTTY:
      case ENOENT:
#if defined( ENOTDIR )
      case ENOTDIR:
#endif
         pCom->error = ZH_COM_ERR_NOCOM;
         break;
      default:
         pCom->error = ZH_COM_ERR_OTHER;
         break;
   }
}

#if defined( ZH_OS_UNIX )
static int zh_comCanRead( PZH_COM pCom, ZH_MAXINT timeout )
{
   int iResult;

#if defined( ZH_HAS_POLL )
   ZH_MAXUINT timer = zh_timerInit( timeout );
   struct pollfd fds;

   fds.fd = pCom->fd;
   fds.events = POLLIN;
   fds.revents = 0;

   do
   {
      int tout = timeout < 0 || timeout > 1000 ? 1000 : ( int ) timeout;
      iResult = poll( &fds, 1, tout );
      zh_comSetOsError( pCom, iResult == -1 );
      if( iResult > 0 && ( fds.revents & POLLIN ) == 0 )
      {
         if( ( fds.revents & ( POLLHUP | POLLNVAL | POLLERR ) ) != 0 )
         {
            if( fds.revents & POLLNVAL )
               pCom->fd = -1;
            zh_comSetComError( pCom, ZH_COM_ERR_PIPE );
            iResult = -1;
            break;
         }
         iResult = 0;
      }
      else if( iResult == -1 && ZH_COM_IS_EINTR( pCom ) )
         iResult = 0;
   }
   while( iResult == 0 && ( timeout = zh_timerTest( timeout, &timer ) ) != 0 &&
          zh_vmRequestQuery() == 0 );
#else /* ! ZH_HAS_POLL */
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
      FD_SET( pCom->fd, &rfds );
      iResult = select( ( int ) ( pCom->fd + 1 ), &rfds, NULL, NULL, &tv );
      zh_comSetOsError( pCom, iResult == -1 );
      if( iResult > 0 && ! FD_ISSET( pCom->fd, &rfds ) )
         iResult = 0;
      else if( iResult == -1 && ZH_COM_IS_EINTR( pCom ) )
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
#endif /* ! ZH_HAS_POLL */

   return iResult;
}

static int zh_comCanWrite( PZH_COM pCom, ZH_MAXINT timeout )
{
   int iResult;

#if defined( ZH_HAS_POLL )
   ZH_MAXUINT timer = zh_timerInit( timeout );
   struct pollfd fds;

   fds.fd = pCom->fd;
   fds.events = POLLOUT;
   fds.revents = 0;

   do
   {
      int tout = timeout < 0 || timeout > 1000 ? 1000 : ( int ) timeout;
      iResult = poll( &fds, 1, tout );
      zh_comSetOsError( pCom, iResult == -1 );
      if( iResult > 0 && ( fds.revents & POLLOUT ) == 0 )
      {
         if( ( fds.revents & ( POLLHUP | POLLNVAL | POLLERR ) ) != 0 )
         {
            if( fds.revents & POLLNVAL )
               pCom->fd = -1;
            zh_comSetComError( pCom, ZH_COM_ERR_PIPE );
            iResult = -1;
            break;
         }
         iResult = 0;
      }
      else if( iResult == -1 && ZH_COM_IS_EINTR( pCom ) )
         iResult = 0;
   }
   while( iResult == 0 && ( timeout = zh_timerTest( timeout, &timer ) ) != 0 &&
          zh_vmRequestQuery() == 0 );
#else /* ! ZH_HAS_POLL */
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
      FD_SET( pCom->fd, &wfds );
      iResult = select( ( int ) ( pCom->fd + 1 ), NULL, &wfds, NULL, &tv );
      zh_comSetOsError( pCom, iResult == -1 );
      if( iResult > 0 && ! FD_ISSET( pCom->fd, &wfds ) )
         iResult = 0;
      else if( iResult == -1 && ZH_COM_IS_EINTR( pCom ) )
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
#endif /* ! ZH_HAS_POLL */

   return iResult;
}
#endif

int zh_comInputCount( int iPort )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   int iCount = 0;

   if( pCom )
   {
#if defined( TIOCINQ )
      int iResult = ioctl( pCom->fd, TIOCINQ, &iCount );
      if( iResult == -1 )
         iCount = 0;
      zh_comSetOsError( pCom, iResult == -1 );
#elif defined( FIONREAD ) && ! defined( ZH_OS_CYGWIN )
      /* Cygwin sys/termios.h explicitly says that "TIOCINQ is
       * utilized instead of FIONREAD which has been occupied for
       * other purposes under CYGWIN", so don't give Cygwin
       * even a chance to hit this code path. */
      int iResult = ioctl( pCom->fd, FIONREAD, &iCount );
      if( iResult == -1 )
         iCount = 0;
      zh_comSetOsError( pCom, iResult == -1 );
#else
      int TODO_TIOCINQ;
      zh_comSetComError( pCom, ZH_COM_ERR_NOSUPPORT );
#endif
   }
   else
      iCount = -1;

   return iCount;
}

int zh_comOutputCount( int iPort )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   int iCount = 0;

   if( pCom )
   {
#if defined( TIOCOUTQ )
      int iResult = ioctl( pCom->fd, TIOCOUTQ, &iCount );
      if( iResult == -1 )
         iCount = 0;
      zh_comSetOsError( pCom, iResult == -1 );
#elif defined( FIONWRITE )
      int iResult = ioctl( pCom->fd, FIONWRITE, &iCount );
      if( iResult == -1 )
         iCount = 0;
      zh_comSetOsError( pCom, iResult == -1 );
#else
      int TODO_TIOCOUTQ;
      zh_comSetComError( pCom, ZH_COM_ERR_NOSUPPORT );
#endif
   }
   else
      iCount = -1;

   return iCount;
}

int zh_comFlush( int iPort, int iType )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      switch( iType )
      {
         case ZH_COM_IFLUSH:
            iResult = tcflush( pCom->fd, TCIFLUSH );
            zh_comSetOsError( pCom, iResult == -1 );
            break;
         case ZH_COM_OFLUSH:
            iResult = tcflush( pCom->fd, TCOFLUSH );
            zh_comSetOsError( pCom, iResult == -1 );
            break;
         case ZH_COM_IOFLUSH:
            iResult = tcflush( pCom->fd, TCIOFLUSH );
            zh_comSetOsError( pCom, iResult == -1 );
            break;
         default:
            iResult = -1;
            zh_comSetComError( pCom, ZH_COM_ERR_PARAMVALUE );
            break;
      }
   }
   return iResult;
}

/*
   TIOCM_LE          DSR (data set ready/line enable)
   TIOCM_DTR         DTR (data terminal ready)
   TIOCM_RTS         RTS (request to send)
   TIOCM_ST          Secondary TXD (transmit)
   TIOCM_SR          Secondary RXD (receive)
   TIOCM_CTS         CTS (clear to send)
   TIOCM_CAR         DCD (data carrier detect)
   TIOCM_CD           see TIOCM_CAR
   TIOCM_RNG         RNG (ring)
   TIOCM_RI           see TIOCM_RNG
   TIOCM_DSR         DSR (data set ready)

   supported only by few platforms (i.e. newer Linux kernels >= 2.4)
   TIOCM_OUT1        OUT 1 (auxiliary user-designated output 2)
   TIOCM_OUT2        OUT 2 (auxiliary user-designated output 1)
   TIOCM_LOOP        LOOP (loopback mode)
 */

#ifdef ZH_OS_LINUX
   /* hack for missing definitions in standard header files */
#  ifndef TIOCM_OUT1
#     define TIOCM_OUT1    0x2000
#  endif
#  ifndef TIOCM_OUT2
#     define TIOCM_OUT2    0x4000
#  endif
#  ifndef TIOCM_LOOP
#     define TIOCM_LOOP    0x8000
#  endif
#endif

int zh_comMCR( int iPort, int * piValue, int iClr, int iSet )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   int iResult = -1, iValue = 0;

   if( pCom )
   {
#if defined( TIOCMGET ) && defined( TIOCMSET )
      int iRawVal, iOldVal;

      iResult = ioctl( pCom->fd, TIOCMGET, &iRawVal );
      if( iResult == 0 )
      {
         if( iRawVal & TIOCM_DTR )
            iValue |= ZH_COM_MCR_DTR;
         if( iRawVal & TIOCM_RTS )
            iValue |= ZH_COM_MCR_RTS;
#ifdef TIOCM_OUT1
         if( iRawVal & TIOCM_OUT1 )
            iValue |= ZH_COM_MCR_OUT1;
#endif
#ifdef TIOCM_OUT2
         if( iRawVal & TIOCM_OUT2 )
            iValue |= ZH_COM_MCR_OUT2;
#endif
#ifdef TIOCM_LOOP
         if( iRawVal & TIOCM_LOOP )
            iValue |= ZH_COM_MCR_LOOP;
#endif

         iOldVal = iRawVal;

         if( iSet & ZH_COM_MCR_DTR )
            iRawVal |= TIOCM_DTR;
         else if( iClr & ZH_COM_MCR_DTR )
            iRawVal &= ~TIOCM_DTR;

         if( iSet & ZH_COM_MCR_RTS )
            iRawVal |= TIOCM_RTS;
         else if( iClr & ZH_COM_MCR_RTS )
            iRawVal &= ~TIOCM_RTS;

#ifdef TIOCM_OUT1
         if( iSet & ZH_COM_MCR_OUT1 )
            iRawVal |= TIOCM_OUT1;
         else if( iClr & ZH_COM_MCR_OUT1 )
            iRawVal &= ~TIOCM_OUT1;
#endif
#ifdef TIOCM_OUT2
         if( iSet & ZH_COM_MCR_OUT2 )
            iRawVal |= TIOCM_OUT2;
         else if( iClr & ZH_COM_MCR_OUT2 )
            iRawVal &= ~TIOCM_OUT2;
#endif
#ifdef TIOCM_LOOP
         if( iSet & ZH_COM_MCR_LOOP )
            iRawVal |= TIOCM_LOOP;
         else if( iClr & ZH_COM_MCR_LOOP )
            iRawVal &= ~TIOCM_LOOP;
#endif

         if( iRawVal != iOldVal )
            iResult = ioctl( pCom->fd, TIOCMSET, &iRawVal );
      }
      zh_comSetOsError( pCom, iResult == -1 );
#else
      int TODO_TIOCMGET_MCR;
      ZH_SYMBOL_UNUSED( iClr );
      ZH_SYMBOL_UNUSED( iSet );
      zh_comSetComError( pCom, ZH_COM_ERR_NOSUPPORT );
#endif
   }

   if( piValue )
      *piValue = iValue;

   return iResult;
}

int zh_comMSR( int iPort, int * piValue )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   int iResult = -1, iValue = 0;

   if( pCom )
   {
#if defined( TIOCMGET ) && defined( TIOCMSET )
      int iRawVal;

      iResult = ioctl( pCom->fd, TIOCMGET, &iRawVal );
      zh_comSetOsError( pCom, iResult == -1 );
      if( iResult == 0 )
      {
         if( iRawVal & TIOCM_CTS )
            iValue |= ZH_COM_MSR_CTS;
         if( iRawVal & TIOCM_DSR )
            iValue |= ZH_COM_MSR_DSR;
         if( iRawVal & TIOCM_RI )
            iValue |= ZH_COM_MSR_RI;
         if( iRawVal & TIOCM_CD )
            iValue |= ZH_COM_MSR_DCD;
      }
#else
      int TODO_TIOCMGET_MSR;
      zh_comSetComError( pCom, ZH_COM_ERR_NOSUPPORT );
#endif
   }

   if( piValue )
      *piValue = iValue;

   return iResult;
}

int zh_comLSR( int iPort, int * piValue )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   int iResult = -1, iValue = 0;

   if( pCom )
   {
#ifdef TIOCSERGETLSR
      iResult = ioctl( pCom->fd, TIOCSERGETLSR, &iValue );
      zh_comSetOsError( pCom, iResult == -1 );
#else
      /* NOTE: most of systems do not give access to the
       *       Line Status Register (LSR)
       */
      zh_comSetComError( pCom, ZH_COM_ERR_NOSUPPORT );
#endif
   }

   if( piValue )
      *piValue = iValue;

   return iResult;
}

int zh_comSendBreak( int iPort, int iDurationInMilliSecs )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      /* NOTE: duration is implementation defined non portable extension
       *       we use 0 what means 'transmit zero-valued bits for at
       *       least 0.25 seconds, and not more that 0.5 seconds'
       */
      ZH_SYMBOL_UNUSED( iDurationInMilliSecs );

      zh_vmUnlock();

      iResult = tcsendbreak( pCom->fd, 0 );
      zh_comSetOsError( pCom, iResult == -1 );

      zh_vmLock();
   }
   return iResult;
}

#if defined( CCTS_OFLOW ) && defined( CRTS_IFLOW )
   #define _ZH_OCRTSCTS       CCTS_OFLOW
   #define _ZH_ICRTSCTS       CRTS_IFLOW
#elif defined( CRTSCTS ) && defined( CRTSXOFF )
   #define _ZH_OCRTSCTS       CRTSCTS
   #define _ZH_ICRTSCTS       CRTSXOFF
#elif defined( CRTSCTS )
   #define _ZH_OCRTSCTS       CRTSCTS
   #define _ZH_ICRTSCTS       CRTSCTS
#elif defined( CNEW_RTSCTS )
   #define _ZH_OCRTSCTS       CNEW_RTSCTS
   #define _ZH_ICRTSCTS       CNEW_RTSCTS
#elif defined( CCTS_OFLOW )
   #define _ZH_OCRTSCTS       CCTS_OFLOW
   #define _ZH_ICRTSCTS       0
#elif defined( CRTS_IFLOW )
   #define _ZH_OCRTSCTS       0
   #define _ZH_ICRTSCTS       CCTS_IFLOW
#elif defined( CRTSXOFF )
   #define _ZH_OCRTSCTS       0
   #define _ZH_ICRTSCTS       CRTSXOFF
#endif

int zh_comFlowControl( int iPort, int * piFlow, int iFlow )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   int iResult = -1, iValue = 0;

   if( pCom )
   {
      /* NOTE: there is no support for DTR/DSR so we cannot use
       *       DTR/DSR handshake instead of the RTS/CTS handshake
       *       BSD systems support MDMBUF flags which enable output
       *       flow control using CD (Carrier Detect) flag.
       *       In SunOS TIOCSSOFTCAR can be used to control CLOCAL flag.
       *       CLOCAL termios structure c_cflag can be used to enable CD
       *       flow control in most portable way.
       */
      struct termios tio;

      iResult = tcgetattr( pCom->fd, &tio );
      zh_comSetOsError( pCom, iResult == -1 );
      if( iResult == 0 )
      {
         tcflag_t c_cflag = tio.c_cflag;
         tcflag_t c_iflag = tio.c_iflag;

#if defined( _ZH_OCRTSCTS )
         if( ( tio.c_cflag & _ZH_OCRTSCTS ) == _ZH_OCRTSCTS )
            iValue |= ZH_COM_FLOW_ORTSCTS;
         if( ( tio.c_cflag & _ZH_ICRTSCTS ) == _ZH_ICRTSCTS )
            iValue |= ZH_COM_FLOW_IRTSCTS;

         if( iFlow >= 0 )
         {
            if( iFlow & ZH_COM_FLOW_ORTSCTS )
               tio.c_cflag |= _ZH_OCRTSCTS;
            else
               tio.c_cflag &= ~_ZH_OCRTSCTS;
            if( iFlow & ZH_COM_FLOW_IRTSCTS )
               tio.c_cflag |= _ZH_ICRTSCTS;
            else
               tio.c_cflag &= ~_ZH_ICRTSCTS;
         }
#else
         {
            int TODO_CRTSCTS;
         }
#endif

         if( ( tio.c_cflag & CLOCAL ) != CLOCAL )
            iValue |= ZH_COM_FLOW_DCD;

         if( iFlow >= 0 )
         {
            if( iFlow & ZH_COM_FLOW_DCD )
               tio.c_cflag &= ~CLOCAL;
            else
               tio.c_cflag |= CLOCAL;
         }


         if( ( tio.c_iflag & IXON ) == IXON )
            iValue |= ZH_COM_FLOW_XON;
         if( ( tio.c_iflag & IXOFF ) == IXOFF )
            iValue |= ZH_COM_FLOW_XOFF;

         if( iFlow >= 0 )
         {
            if( iFlow & ZH_COM_FLOW_XON )
               tio.c_iflag |= IXON;
            else
               tio.c_iflag &= ~IXON;
            if( iFlow & ZH_COM_FLOW_XOFF )
               tio.c_iflag |= IXOFF;
            else
               tio.c_iflag &= ~IXOFF;
         }

         if( c_cflag != tio.c_cflag || c_iflag != tio.c_iflag )
         {
            iResult = tcsetattr( pCom->fd, TCSANOW, &tio );
            zh_comSetOsError( pCom, iResult == -1 );
         }
      }
   }

   if( piFlow )
      *piFlow = iValue;

   return iResult;
}

int zh_comFlowSet( int iPort, int iFlow )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      /* NOTE: ZH_COM_FL_SOFT is ignored, we assume that user chose
       *       correct hardware/software flow control type which is
       *       the same as set in terminal device parameters
       */
      if( iFlow & ZH_COM_FL_OON )
         iResult = tcflow( pCom->fd, TCOON );
      else if( iFlow & ZH_COM_FL_OOFF )
         iResult = tcflow( pCom->fd, TCOOFF );
      else
         iResult = 0;

      if( iFlow & ZH_COM_FL_ION )
      {
         if( tcflow( pCom->fd, TCION ) == -1 )
            iResult = -1;
      }
      else if( iFlow & ZH_COM_FL_IOFF )
      {
         if( tcflow( pCom->fd, TCIOFF ) == -1 )
            iResult = -1;
      }
      zh_comSetOsError( pCom, iResult == -1 );
   }

   return iResult;
}

int zh_comFlowChars( int iPort, int iXONchar, int iXOFFchar )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      iResult = 0;
      if( iXONchar >= 0 || iXOFFchar >= 0 )
      {
         struct termios tio;

         iResult = tcgetattr( pCom->fd, &tio );
         if( iResult == 0 )
         {
            if( iXONchar >= 0 )
               tio.c_cc[ VSTART ] = iXONchar;
            if( iXOFFchar >= 0 )
               tio.c_cc[ VSTOP ] = iXOFFchar;
            iResult = tcsetattr( pCom->fd, TCSANOW, &tio );
         }
      }
      zh_comSetOsError( pCom, iResult == -1 );
   }
   return iResult;
}

#if ! defined( _POSIX_VDISABLE )
#  define _POSIX_VDISABLE  '\0'
#endif

int zh_comDiscardChar( int iPort, int iChar )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
#if defined( VDISCARD ) && defined( IEXTEN )
      struct termios tio;

      iResult = tcgetattr( pCom->fd, &tio );
      zh_comSetOsError( pCom, iResult == -1 );
      if( iResult == 0 )
      {
         if( ( tio.c_lflag & IEXTEN ) != 0 &&
             tio.c_cc[ VDISCARD ] != _POSIX_VDISABLE )
            iResult = 1;

         if( iChar == -1 ? iResult != 0 :
             ( iResult == 0 || tio.c_cc[ VDISCARD ] != iChar ) )
         {
            if( iChar == -1 )
            {
               tio.c_lflag &= ~IEXTEN;
               tio.c_cc[ VDISCARD ] = _POSIX_VDISABLE;
            }
            else
            {
               tio.c_lflag |= IEXTEN;
               tio.c_cc[ VDISCARD ] = iChar;
#if defined( VLNEXT )
               tio.c_cc[ VLNEXT ] = _POSIX_VDISABLE;
#endif
            }
            if( tcsetattr( pCom->fd, TCSANOW, &tio ) == -1 )
            {
               zh_comSetOsError( pCom, ZH_TRUE );
               iResult = -1;
            }
         }
      }
#else
      int TODO_VDISCARD;
      ZH_SYMBOL_UNUSED( iChar );
      zh_comSetComError( pCom, ZH_COM_ERR_NOSUPPORT );
#endif
   }

   return iResult;
}

int zh_comErrorChar( int iPort, int iChar )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      /* NOTE: there is no support for setting user defined error character
       */

      ZH_SYMBOL_UNUSED( iChar );
      zh_comSetComError( pCom, ZH_COM_ERR_NOSUPPORT );
   }

   return iResult;
}

int zh_comOutputState( int iPort )
{
   /* NOTE: checking ZH_COM_TX_* output flow states is unsupported */
   int iResult = zh_comOutputCount( iPort );

   if( iResult == 0 )
      iResult = ZH_COM_TX_EMPTY;
   else if( iResult > 0 )
      iResult = 0;

   return iResult;
}

int zh_comInputState( int iPort )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      /* NOTE: checking ZH_COM_RX_* input flow states is unsupported */
      zh_comSetComError( pCom, ZH_COM_ERR_NOSUPPORT );
   }

   return iResult;
}

long zh_comSend( int iPort, const void * data, long len, ZH_MAXINT timeout )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   long lSent = -1;

   if( pCom )
   {
      zh_vmUnlock();

#if defined( ZH_OS_UNIX )
      if( timeout >= 0 )
      {
         lSent = zh_comCanWrite( pCom, timeout );
         if( lSent == 0 )
         {
            zh_comSetComError( pCom, ZH_COM_ERR_TIMEOUT );
            lSent = -1;
         }
      }
      else
         lSent = 0;
#else
      /* NOTE: write timeout is unsupported */
      ZH_SYMBOL_UNUSED( timeout );
      lSent = 0;
#endif

      if( lSent >= 0 )
      {
         do
         {
            lSent = write( pCom->fd, data, len );
            zh_comSetOsError( pCom, lSent == -1 );
         }
         while( lSent == -1 && ZH_COM_IS_EINTR( pCom ) && zh_vmRequestQuery() == 0 );
      }
      zh_vmLock();
   }

   return lSent;
}

long zh_comRecv( int iPort, void * data, long len, ZH_MAXINT timeout )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   long lReceived = -1;

   if( pCom )
   {
      zh_vmUnlock();

#if defined( ZH_OS_UNIX )
      if( timeout >= 0 )
      {
         lReceived = zh_comCanRead( pCom, timeout );
         if( lReceived == 0 )
         {
            zh_comSetComError( pCom, ZH_COM_ERR_TIMEOUT );
            lReceived = -1;
         }
      }
      else
         lReceived = 0;
#else
      if( timeout != pCom->rdtimeout )
      {
         /* TODO: implement timeout settings
          *          tio.c_cc[ VTIME ] = ( timeout + 50 ) / 100;
          *          tio.c_cc[ VMIN ]  = 0;
          *       in DJGPP builds
          */
      }
      lReceived = 0;
#endif

      if( lReceived >= 0 )
      {
         do
         {
            lReceived = read( pCom->fd, ( char * ) data, len );
            zh_comSetOsError( pCom, lReceived == -1 );
         }
         while( lReceived == -1 && ZH_COM_IS_EINTR( pCom ) && zh_vmRequestQuery() == 0 );
      }
      zh_vmLock();
   }

   return lReceived;
}

int zh_comInit( int iPort, int iBaud, int iParity, int iSize, int iStop )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      struct termios tio;

      iResult = tcgetattr( pCom->fd, &tio );
      zh_comSetOsError( pCom, iResult == -1 );
      if( iResult == 0 )
      {
#if defined( cfmakeraw ) || defined( ZH_OS_LINUX )
         /* Raw input from device */
         cfmakeraw( &tio );
#endif
         tio.c_iflag &= ~( IGNBRK | IGNPAR | BRKINT | PARMRK | ISTRIP |
                           INLCR | IGNCR | ICRNL | IXON | IXANY | IXOFF );
         tio.c_oflag &= ~OPOST;
         tio.c_lflag &= ~( ECHO | ECHONL | ICANON | ISIG | IEXTEN );
         tio.c_cflag &= ~( CSIZE | PARENB );
         /* Enable the receiver and set local mode... */
         tio.c_cflag |= ( CLOCAL | CREAD );

         tio.c_cc[ VTIME ] = 0;  /* inter-character timer in 1/10 sec. */

         /* workaround for bug in some Linux kernels (i.e. 3.13.0-64-generic
            *buntu) in which select() unconditionally accepts stdin for
            reading if c_cc[ VMIN ] = 0 [druzus] */
         tio.c_cc[ VMIN ] = 1;

         if( iBaud )
         {
            switch( iBaud )
            {
               case        0: iBaud =      B0; break;
               case       50: iBaud =     B50; break;
               case       75: iBaud =     B75; break;
               case      110: iBaud =    B110; break;
               case      150: iBaud =    B150; break;
               case      200: iBaud =    B200; break;
               case      300: iBaud =    B300; break;
               case      600: iBaud =    B600; break;
               case     1200: iBaud =   B1200; break;
               case     1800: iBaud =   B1800; break;
               case     2400: iBaud =   B2400; break;
               case     4800: iBaud =   B4800; break;
               case     9600: iBaud =   B9600; break;
               case    19200: iBaud =  B19200; break;
               case    38400: iBaud =  B38400; break;
#ifdef B57600
               case    57600: iBaud =  B57600; break;
#endif
#ifdef B115200
               case   115200: iBaud = B115200; break;
#endif
#ifdef B230400
               case   230400: iBaud = B230400; break;
#endif
#ifdef B460800
               case   460800: iBaud = B460800; break;
#endif
#ifdef B500000
               case   500000: iBaud = B500000; break;
#endif
#ifdef B576000
               case   576000: iBaud = B576000; break;
#endif
#ifdef B921600
               case   921600: iBaud = B921600; break;
#endif
#ifdef B1000000
               case  1000000: iBaud = B1000000; break;
#endif
               default:
                  iResult = -1;
            }
         }
         switch( iParity )
         {
            case 0:
            case 'N':
            case 'n':
               tio.c_cflag &= ~( PARENB | PARODD );
               tio.c_iflag &= ~INPCK;
               break;
            case 'E':
            case 'e':
               tio.c_cflag |= PARENB;
               tio.c_cflag &= ~PARODD;
               tio.c_iflag |= INPCK;
               break;
            case 'O':
            case 'o':
               tio.c_cflag |= PARENB | PARODD;
               tio.c_iflag |= INPCK;
               break;
#if defined( CMSPAR )
            case 'S':
            case 's':
               tio.c_cflag |= CMSPAR | PARENB;
               tio.c_cflag &= ~PARODD;
               tio.c_iflag |= INPCK;
               break;
            case 'M':
            case 'm':
               tio.c_cflag |= CMSPAR | PARENB | PARODD;
               tio.c_iflag |= INPCK;
               break;
#endif
            default:
               iResult = -1;
         }
         switch( iSize )
         {
            case 0:
            case 8: tio.c_cflag |= CS8; break;
            case 7: tio.c_cflag |= CS7; break;
            case 6: tio.c_cflag |= CS6; break;
            case 5: tio.c_cflag |= CS5; break;
            default:
               iResult = -1;
         }
         switch( iStop )
         {
            case 0:
            case 1: tio.c_cflag &= ~CSTOPB; break;
            case 2: tio.c_cflag |= CSTOPB; break;
            default:
               iResult = -1;
         }

         if( iResult == 0 )
         {
            if( iBaud )
            {
               cfsetispeed( &tio, iBaud );
               cfsetospeed( &tio, iBaud );
            }

            iResult = tcsetattr( pCom->fd, TCSAFLUSH, &tio );
#if ! defined( ZH_OS_UNIX )
            if( iResult == 0 )
               pCom->rdtimeout = 0;
#endif
            zh_comSetOsError( pCom, iResult == -1 );
         }
         else
            zh_comSetComError( pCom, ZH_COM_ERR_PARAMVALUE );
      }
   }

   return iResult;
}

int zh_comClose( int iPort )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      zh_vmUnlock();
#if defined( TIOCNXCL )
      ioctl( pCom->fd, TIOCNXCL, 0 );
#endif
      do
      {
         iResult = close( pCom->fd );
         zh_comSetOsError( pCom, iResult == -1 );
      }
      while( iResult == -1 && ZH_COM_IS_EINTR( pCom ) && zh_vmRequestQuery() == 0 );

      if( iResult != -1 || ZH_COM_IS_EBADF( pCom ) )
      {
         pCom->fd = ( ZH_FHANDLE ) FS_ERROR;
         pCom->status &= ~ZH_COM_OPEN;
      }
      zh_vmLock();
   }

   return iResult;
}

int zh_comOpen( int iPort )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_ENABLED );
   int iResult = -1;

   if( pCom )
   {
      if( pCom->status & ZH_COM_OPEN )
      {
         zh_comSetComError( pCom, ZH_COM_ERR_ALREADYOPEN );
      }
      else
      {
         char buffer[ ZH_COM_DEV_NAME_MAX ];
         const char * name = zh_comGetName( pCom, buffer, sizeof( buffer ) );

         zh_vmUnlock();

         pCom->fd = open( name, O_RDWR | O_NOCTTY );
         if( pCom->fd != -1 )
         {
#if defined( TIOCEXCL ) /* TIOCNXCL */
            iResult = ioctl( pCom->fd, TIOCEXCL, 0 );
            if( iResult != 0 )
            {
               close( pCom->fd );
               pCom->fd = -1;
               zh_comSetComError( pCom, ZH_COM_ERR_BUSY );
            }
            else
#else
            iResult = 0;
#endif
            pCom->status |= ZH_COM_OPEN;
         }
         zh_comSetOsError( pCom, iResult == -1 );

         zh_vmLock();
      }
   }

   return iResult;
}

/* end of ZH_HAS_TERMIOS */

#elif defined( ZH_OS_WIN )

static void zh_comSetOsError( PZH_COM pCom, BOOL fError )
{
   pCom->oserr = fError ? GetLastError() : 0;

   switch( pCom->oserr )
   {
      case 0:
         pCom->error = 0;
         break;
      case ERROR_TIMEOUT:
         pCom->error = ZH_COM_ERR_TIMEOUT;
         break;
      case ERROR_ACCESS_DENIED:
      case ERROR_SHARING_VIOLATION:
         pCom->error = ZH_COM_ERR_BUSY;
         break;
      case ERROR_FILE_NOT_FOUND:
      case ERROR_PATH_NOT_FOUND:
         pCom->error = ZH_COM_ERR_NOCOM;
         break;
      default:
         pCom->error = ZH_COM_ERR_OTHER;
         break;
   }
}

int zh_comInputCount( int iPort )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   int iCount = 0;

   if( pCom )
   {
      COMSTAT comStat;

      if( ClearCommError( pCom->hComm, NULL, &comStat ) )
      {
         iCount = comStat.cbInQue;
         zh_comSetOsError( pCom, ZH_FALSE );
      }
      else
         zh_comSetOsError( pCom, ZH_TRUE );
   }
   else
      iCount = -1;

   return iCount;
}

int zh_comOutputCount( int iPort )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   int iCount = 0;

   if( pCom )
   {
      COMSTAT comStat;

      if( ClearCommError( pCom->hComm, NULL, &comStat ) )
      {
         iCount = comStat.cbOutQue;
         zh_comSetOsError( pCom, ZH_FALSE );
      }
      else
         zh_comSetOsError( pCom, ZH_TRUE );
   }
   else
      iCount = -1;

   return iCount;
}

int zh_comFlush( int iPort, int iType )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   BOOL fResult = FALSE;

   if( pCom )
   {
      switch( iType )
      {
         case ZH_COM_IFLUSH:
            fResult = PurgeComm( pCom->hComm, PURGE_RXCLEAR );
            zh_comSetOsError( pCom, ! fResult );
            break;
         case ZH_COM_OFLUSH:
            fResult = PurgeComm( pCom->hComm, PURGE_TXCLEAR );
            zh_comSetOsError( pCom, ! fResult );
            break;
         case ZH_COM_IOFLUSH:
            fResult = PurgeComm( pCom->hComm, PURGE_TXCLEAR | PURGE_RXCLEAR );
            zh_comSetOsError( pCom, ! fResult );
            break;
         default:
            zh_comSetComError( pCom, ZH_COM_ERR_PARAMVALUE );
            break;
      }
   }
   return fResult ? 0 : -1;
}

int zh_comMCR( int iPort, int * piValue, int iClr, int iSet )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   BOOL fResult = FALSE;
   int iValue = 0;

   if( pCom )
   {
      if( iSet & ZH_COM_MCR_DTR )
         fResult = EscapeCommFunction( pCom->hComm, SETDTR );
      else if( iClr & ZH_COM_MCR_DTR )
         fResult = EscapeCommFunction( pCom->hComm, CLRDTR );

      if( iSet & ZH_COM_MCR_RTS )
         fResult = EscapeCommFunction( pCom->hComm, SETRTS );
      else if( iClr & ZH_COM_MCR_RTS )
         fResult = EscapeCommFunction( pCom->hComm, CLRRTS );

      /* MCR_OUT1, MCR_OUT2, MCR_LOOP and reading current state
       * is unsupported
       */
      zh_comSetOsError( pCom, ! fResult );
   }

   if( piValue )
      *piValue = iValue;

   return fResult ? 0 : -1;
}

int zh_comMSR( int iPort, int * piValue )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   BOOL fResult = FALSE;
   int iValue = 0;

   if( pCom )
   {
      DWORD dwModemStat = 0;

      fResult = GetCommModemStatus( pCom->hComm, &dwModemStat );
      if( fResult )
      {
         if( dwModemStat & MS_CTS_ON )
            iValue |= ZH_COM_MSR_CTS;
         if( dwModemStat & MS_DSR_ON )
            iValue |= ZH_COM_MSR_DSR;
         if( dwModemStat & MS_RING_ON )
            iValue |= ZH_COM_MSR_RI;
         if( dwModemStat & MS_RLSD_ON )
            iValue |= ZH_COM_MSR_DCD;

         /* MSR_DELTA_CTS, MSR_DELTA_DSR, MSR_TERI, MSR_DELTA_DCD
          * are unsupported
          */

      }
      zh_comSetOsError( pCom, ! fResult );
   }

   if( piValue )
      *piValue = iValue;

   return fResult ? 0 : -1;
}

int zh_comLSR( int iPort, int * piValue )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   BOOL fResult = FALSE;
   int iValue = 0;

   if( pCom )
   {
      DWORD dwErrors = 0;

      fResult = ClearCommError( pCom->hComm, &dwErrors, NULL );
      if( fResult )
      {
         if( dwErrors & CE_BREAK )
            iValue |= ZH_COM_LSR_BREAK;
         if( dwErrors & CE_FRAME )
            iValue |= ZH_COM_LSR_FRAMING_ERR;
         if( dwErrors & CE_OVERRUN )
            iValue |= ZH_COM_LSR_OVERRUN_ERR;
         if( dwErrors & CE_RXPARITY )
            iValue |= ZH_COM_LSR_PARITY_ERR;

         /* LSR_DATA_READY, LSR_TRANS_HOLD_EMPTY, LSR_TRANS_EMPTY
          * are unsupported
          */
      }
      zh_comSetOsError( pCom, ! fResult );
   }

   if( piValue )
      *piValue = iValue;

   return fResult ? 0 : -1;
}

int zh_comSendBreak( int iPort, int iDurationInMilliSecs )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   BOOL fResult = FALSE;

   if( pCom )
   {
      zh_vmUnlock();

      fResult = SetCommBreak( pCom->hComm );
      if( fResult )
      {
         Sleep( iDurationInMilliSecs );
         fResult = ClearCommBreak( pCom->hComm );
      }
      zh_comSetOsError( pCom, ! fResult );

      zh_vmLock();
   }
   return fResult ? 0 : -1;
}

int zh_comFlowControl( int iPort, int * piFlow, int iFlow )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   BOOL fResult = FALSE;
   int iValue = 0;

   if( pCom )
   {
      DCB dcb;

      dcb.DCBlength = sizeof( DCB );
      fResult = GetCommState( pCom->hComm, &dcb );
      if( fResult )
      {
         if( dcb.fRtsControl == RTS_CONTROL_HANDSHAKE )
            iValue |= ZH_COM_FLOW_IRTSCTS;
         if( dcb.fOutxCtsFlow )
            iValue |= ZH_COM_FLOW_ORTSCTS;

         if( dcb.fDtrControl == DTR_CONTROL_HANDSHAKE )
            iValue |= ZH_COM_FLOW_IDTRDSR;
         if( dcb.fOutxDsrFlow )
            iValue |= ZH_COM_FLOW_ODTRDSR;

         if( dcb.fInX )
            iValue |= ZH_COM_FLOW_XOFF;
         if( dcb.fOutX )
            iValue |= ZH_COM_FLOW_XON;

         if( iFlow >= 0 )
         {
            if( iFlow & ZH_COM_FLOW_IRTSCTS )
               dcb.fRtsControl = RTS_CONTROL_HANDSHAKE;
            else if( dcb.fRtsControl == RTS_CONTROL_HANDSHAKE )
               dcb.fRtsControl = RTS_CONTROL_ENABLE;
            dcb.fOutxCtsFlow = ( iFlow & ZH_COM_FLOW_ORTSCTS ) != 0;

            if( iFlow & ZH_COM_FLOW_IDTRDSR )
               dcb.fDtrControl = DTR_CONTROL_HANDSHAKE;
            else if( dcb.fDtrControl == DTR_CONTROL_HANDSHAKE )
               dcb.fDtrControl = DTR_CONTROL_ENABLE;
            dcb.fOutxDsrFlow = ( iFlow & ZH_COM_FLOW_ODTRDSR ) != 0;

            dcb.fInX = ( iFlow & ZH_COM_FLOW_XOFF ) != 0;
            dcb.fOutX = ( iFlow & ZH_COM_FLOW_XON ) != 0;

            fResult = SetCommState( pCom->hComm, &dcb );
         }
      }
      zh_comSetOsError( pCom, ! fResult );
   }

   if( piFlow )
      *piFlow = iValue;

   return fResult ? 0 : -1;
}

int zh_comFlowSet( int iPort, int iFlow )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   BOOL fResult = FALSE, fNotSup = FALSE;

   if( pCom )
   {
      if( iFlow & ZH_COM_FL_SOFT )
      {
         if( iFlow & ZH_COM_FL_OOFF )
            fResult = EscapeCommFunction( pCom->hComm, SETXOFF );
         else if( iFlow & ZH_COM_FL_OON )
            fResult = EscapeCommFunction( pCom->hComm, SETXON );
         else
            fNotSup = TRUE;
         zh_comSetOsError( pCom, ! fResult );
      }
      else if( iFlow & ZH_COM_FL_RTSCTS )
      {
         if( iFlow & ZH_COM_FL_IOFF )
            fResult = EscapeCommFunction( pCom->hComm, CLRRTS );
         else if( iFlow & ZH_COM_FL_ION )
            fResult = EscapeCommFunction( pCom->hComm, SETRTS );
         else
            fNotSup = TRUE;
      }
      else if( iFlow & ZH_COM_FL_DTRDSR )
      {
         if( iFlow & ZH_COM_FL_IOFF )
            fResult = EscapeCommFunction( pCom->hComm, CLRDTR );
         else if( iFlow & ZH_COM_FL_ION )
            fResult = EscapeCommFunction( pCom->hComm, SETDTR );
         else
            fNotSup = TRUE;
      }
      else
         fNotSup = TRUE;

      if( fNotSup )
         zh_comSetComError( pCom, ZH_COM_ERR_NOSUPPORT );
      else
         zh_comSetOsError( pCom, ! fResult );
   }

   return fResult ? 0 : -1;
}

int zh_comFlowChars( int iPort, int iXONchar, int iXOFFchar )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   BOOL fResult = FALSE;

   if( pCom )
   {
      fResult = TRUE;
      if( iXONchar >= 0 || iXOFFchar >= 0 )
      {
         DCB dcb;

         dcb.DCBlength = sizeof( DCB );
         fResult = GetCommState( pCom->hComm, &dcb );
         if( fResult )
         {
            if( iXONchar >= 0 )
               dcb.XonChar = ( char ) iXONchar;
            if( iXOFFchar >= 0 )
               dcb.XoffChar = ( char ) iXOFFchar;
            fResult = SetCommState( pCom->hComm, &dcb );
         }
      }
      zh_comSetOsError( pCom, ! fResult );
   }
   return fResult ? 0 : -1;
}

int zh_comDiscardChar( int iPort, int iChar )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      /* NOTE: there is no support for setting user defined character
       * discarding input buffer
       */
      ZH_SYMBOL_UNUSED( iChar );
      zh_comSetComError( pCom, ZH_COM_ERR_NOSUPPORT );
   }

   return iResult;
}

int zh_comErrorChar( int iPort, int iChar )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   BOOL fResult = FALSE;

   if( pCom )
   {
      DCB dcb;

      dcb.DCBlength = sizeof( DCB );
      fResult = GetCommState( pCom->hComm, &dcb );
      if( fResult )
      {
         if( iChar >= 0 )
         {
            dcb.fErrorChar = TRUE;
            dcb.ErrorChar = ( char ) iChar;
         }
         else
            dcb.fErrorChar = FALSE;
         fResult = SetCommState( pCom->hComm, &dcb );
      }
      zh_comSetOsError( pCom, ! fResult );
   }
   return fResult ? 0 : -1;
}

int zh_comOutputState( int iPort )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   BOOL fResult = FALSE;
   int iValue = 0;

   if( pCom )
   {
      COMSTAT comStat;

      fResult = ClearCommError( pCom->hComm, NULL, &comStat );
      if( fResult )
      {
         /* NOTE: ZH_COM_TX_RFLUSH is unsupported */

         if( comStat.fCtsHold )
            iValue |= ZH_COM_TX_CTS;
         if( comStat.fDsrHold )
            iValue |= ZH_COM_TX_DSR;
         if( comStat.fRlsdHold )
            iValue |= ZH_COM_TX_DCD;
         if( comStat.fXoffHold )
            iValue |= ZH_COM_TX_XOFF;
         if( comStat.cbOutQue == 0 )
            iValue |= ZH_COM_TX_EMPTY;
      }
      zh_comSetOsError( pCom, ! fResult );
   }

   return fResult ? iValue : -1;
}

int zh_comInputState( int iPort )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   BOOL fResult = FALSE;
   int iValue = 0;

   if( pCom )
   {
      COMSTAT comStat;

      fResult = ClearCommError( pCom->hComm, NULL, &comStat );
      if( fResult )
      {
         if( comStat.fXoffSent )
            iValue |= ZH_COM_RX_XOFF;
      }
      zh_comSetOsError( pCom, ! fResult );
   }

   return fResult ? iValue : -1;
}

static BOOL zh_comSetTimeouts( PZH_COM pCom, ZH_MAXINT rdtimeout,
                                             ZH_MAXINT wrtimeout )
{
   COMMTIMEOUTS timeouts;
   BOOL fResult;

   if( rdtimeout == 0 )
   {
      timeouts.ReadIntervalTimeout = MAXDWORD;
      timeouts.ReadTotalTimeoutMultiplier = 0;
      timeouts.ReadTotalTimeoutConstant = 0;
   }
   else
   {
      timeouts.ReadIntervalTimeout = MAXDWORD;
      timeouts.ReadTotalTimeoutMultiplier = MAXDWORD;
      timeouts.ReadTotalTimeoutConstant = ( DWORD ) rdtimeout;
   }
   timeouts.WriteTotalTimeoutMultiplier = 0;
   timeouts.WriteTotalTimeoutConstant = ( DWORD ) ZH_MAX( wrtimeout, 1 );

   fResult = SetCommTimeouts( pCom->hComm, &timeouts );
   if( fResult )
   {
      pCom->rdtimeout = rdtimeout;
      pCom->wrtimeout = wrtimeout;
   }

   return fResult;
}

long zh_comSend( int iPort, const void * data, long len, ZH_MAXINT timeout )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   long lSent = -1;

   if( pCom )
   {
      zh_vmUnlock();

      if( timeout < 0 )
         timeout = 0;

      if( pCom->wrtimeout == timeout ||
          zh_comSetTimeouts( pCom, pCom->rdtimeout, timeout ) )
      {
         DWORD dwWritten = 0;
         BOOL fResult;

         fResult = WriteFile( pCom->hComm, data, ( DWORD ) len, &dwWritten, NULL );
         lSent = fResult ? ( long ) dwWritten : -1;
         if( lSent == 0 )
         {
            zh_comSetComError( pCom, ZH_COM_ERR_TIMEOUT );
            lSent = -1;
         }
         else
            zh_comSetOsError( pCom, ! fResult );
      }
      else
         zh_comSetOsError( pCom, ZH_TRUE );

      zh_vmLock();
   }

   return lSent;
}

long zh_comRecv( int iPort, void * data, long len, ZH_MAXINT timeout )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   long lReceived = -1;

   if( pCom )
   {
      zh_vmUnlock();

      if( timeout < 0 )
         timeout = 0;

      if( pCom->rdtimeout == timeout ||
          zh_comSetTimeouts( pCom, timeout, pCom->wrtimeout ) )
      {
         DWORD dwRead = 0;
         BOOL fResult;

         fResult = ReadFile( pCom->hComm, data, ( DWORD ) len, &dwRead, NULL );
         lReceived = fResult ? ( long ) dwRead : -1;
         if( lReceived == 0 )
         {
            zh_comSetComError( pCom, ZH_COM_ERR_TIMEOUT );
            lReceived = -1;
         }
         else
            zh_comSetOsError( pCom, ! fResult );
      }
      else
         zh_comSetOsError( pCom, ZH_TRUE );

      zh_vmLock();
   }

   return lReceived;
}

int zh_comInit( int iPort, int iBaud, int iParity, int iSize, int iStop )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   BOOL fResult = FALSE;

   if( pCom )
   {
      DCB dcb;

      dcb.DCBlength = sizeof( DCB );
      fResult = GetCommState( pCom->hComm, &dcb );
      if( fResult )
      {
         switch( iParity )
         {
            case 0:
            case 'N':
            case 'n':
               iParity = NOPARITY;
               break;
            case 'E':
            case 'e':
               iParity = EVENPARITY;
               break;
            case 'O':
            case 'o':
               iParity = ODDPARITY;
               break;
            case 'S':
            case 's':
               iParity = SPACEPARITY;
               break;
            case 'M':
            case 'm':
               iParity = MARKPARITY;
               break;
            default:
               fResult = FALSE;
         }
         switch( iSize )
         {
            case 0:
               iSize = 8;
            case 8:
            case 7:
            case 6:
            case 5:
               break;
            default:
               fResult = FALSE;
         }
         switch( iStop )
         {
            case 0:
            case 1: iStop = ONESTOPBIT; break;
            case 2: iStop = TWOSTOPBITS; break;
            default:
               fResult = FALSE;
         }
         if( fResult )
         {
            if( iBaud )
               dcb.BaudRate = ( DWORD ) iBaud;
            dcb.fBinary = 1;
            dcb.fParity = 0;
            dcb.fOutxCtsFlow = 0;
            dcb.fOutxDsrFlow = 0;
            dcb.fDtrControl = DTR_CONTROL_ENABLE;
            dcb.fDsrSensitivity = 0;
            dcb.fTXContinueOnXoff = 1;
            dcb.fOutX = 0;
            dcb.fInX = 0;
            dcb.fErrorChar = 0;
            dcb.fNull = 0;
            dcb.fRtsControl = RTS_CONTROL_ENABLE;
            dcb.fAbortOnError = 0;
          /*dcb.XonLim*/
          /*dcb.XoffLim*/
            dcb.ByteSize = ( BYTE ) iSize;
            dcb.Parity = ( BYTE ) iParity;
            dcb.StopBits = ( BYTE ) iStop;
          /*dcb.XonChar*/
          /*dcb.XoffChar*/
            dcb.ErrorChar = '?';
          /*dcb.EofChar*/
          /*dcb.EvtChar*/

            fResult = SetCommState( pCom->hComm, &dcb );
            if( fResult )
               fResult = zh_comSetTimeouts( pCom, 0, 0 );

            zh_comSetOsError( pCom, ! fResult );
         }
         else
            zh_comSetComError( pCom, ZH_COM_ERR_PARAMVALUE );
      }
      else
         zh_comSetOsError( pCom, ! fResult );
   }

   return fResult ? 0 : -1;
}

int zh_comClose( int iPort )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   BOOL fResult = FALSE;

   if( pCom )
   {
      zh_vmUnlock();
      #if 0
      FlushFileBuffers( pCom->hComm );
      #endif
      fResult = CloseHandle( pCom->hComm );
      pCom->hComm = INVALID_HANDLE_VALUE;
      pCom->status &= ~ZH_COM_OPEN;
      zh_comSetOsError( pCom, ! fResult );
      zh_vmLock();
   }

   return fResult ? 0 : -1;
}

int zh_comOpen( int iPort )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_ENABLED );
   BOOL fResult = FALSE;

   if( pCom )
   {
      if( pCom->status & ZH_COM_OPEN )
      {
         zh_comSetComError( pCom, ZH_COM_ERR_ALREADYOPEN );
      }
      else
      {
         char buffer[ ZH_COM_DEV_NAME_MAX ];
         const char * szName = zh_comGetName( pCom, buffer, sizeof( buffer ) );
         LPCTSTR lpName;
         LPTSTR lpFree;

         lpName = ZH_FSNAMECONV( szName, &lpFree );

         zh_vmUnlock();

         pCom->hComm = CreateFile( lpName,
                                   GENERIC_READ | GENERIC_WRITE,
                                   0,
                                   NULL,
                                   OPEN_EXISTING,
                                   FILE_FLAG_NO_BUFFERING, NULL );
         if( pCom->hComm != INVALID_HANDLE_VALUE )
         {
            fResult = TRUE;
            pCom->status |= ZH_COM_OPEN;
         }
         zh_comSetOsError( pCom, ! fResult );

         zh_vmLock();

         if( lpFree )
            zh_xfree( lpFree );
      }
   }

   return fResult ? 0 : -1;
}

/* end of ZH_OS_WIN */


#elif defined( ZH_HAS_PMCOM )

static void zh_comSetOsError( PZH_COM pCom, int iError )
{
   pCom->oserr = iError;

   switch( iError )
   {
      case COMERR_NOCHIP:
         pCom->error = ZH_COM_ERR_WRONGPORT;
         break;
      case COMERR_RXOVERFLOW:
      case COMERR_NOMEMORY:
      case COMERR_GENERAL:
      default:
         pCom->error = ZH_COM_ERR_OTHER;
   }
}

int zh_comInputCount( int iPort )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   int iCount = -1;

   if( pCom )
      iCount = COMTXBufferUsed( iPort - 1 );

   return iCount;
}

int zh_comOutputCount( int iPort )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   int iCount = -1;

   if( pCom )
      iCount = COMRXBufferUsed( iPort - 1 );

   return iCount;
}

int zh_comFlush( int iPort, int iType )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      iResult = 0;
      zh_comSetOsError( pCom, 0 );
      switch( iType )
      {
         case ZH_COM_IFLUSH:
         case ZH_COM_IOFLUSH:
            COMClearRXBuffer( iPort - 1 );
            if( iType == ZH_COM_IFLUSH )
               break;
            break;
         case ZH_COM_OFLUSH:
            COMClearTXBuffer( iPort - 1 );
            break;
         default:
            iResult = -1;
            zh_comSetComError( pCom, ZH_COM_ERR_PARAMVALUE );
            break;
      }
   }

   return iResult;
}

int zh_comMCR( int iPort, int * piValue, int iClr, int iSet )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   int iResult = -1;
   int iValue = 0;

   if( pCom )
   {
      /* MCR_OUT1, MCR_OUT2, MCR_LOOP and reading current state
       * is unsupported
       */
      if( iSet & ZH_COM_MCR_DTR )
         COMSetDtr( iPort - 1, 1 );
      else if( iClr & ZH_COM_MCR_DTR )
         COMSetDtr( iPort - 1, 0 );

      if( iSet & ZH_COM_MCR_RTS )
         COMSetRts( iPort - 1, 1 );
      else if( iClr & ZH_COM_MCR_RTS )
         COMSetRts( iPort - 1, 0 );

      iResult = 0;
   }

   if( piValue )
      *piValue = iValue;

   return iResult;
}

int zh_comMSR( int iPort, int * piValue )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   int iResult = -1;
   int iValue = 0;

   if( pCom )
   {
      int iMSR = COMGetModemStatus( iPort - 1 );

      if( iMSR & DELTA_CTS )
         iValue |= ZH_COM_MSR_DELTA_CTS;
      if( iMSR & DELTA_DSR )
         iValue |= ZH_COM_MSR_DELTA_DSR;
      if( iMSR & DELTA_RI )
         iValue |= ZH_COM_MSR_TERI;
      if( iMSR & DELTA_CD )
         iValue |= ZH_COM_MSR_DELTA_DCD;

      if( iMSR & CTS_LINE )
         iValue |= ZH_COM_MSR_CTS;
      if( iMSR & DSR_LINE )
         iValue |= ZH_COM_MSR_DSR;
      if( iMSR & RI_LINE )
         iValue |= ZH_COM_MSR_RI;
      if( iMSR & CD_LINE )
         iValue |= ZH_COM_MSR_DCD;

      iResult = 0;
   }

   if( piValue )
      *piValue = iValue;

   return iResult;
}

int zh_comLSR( int iPort, int * piValue )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   int iResult = -1, iValue = 0;

   if( pCom )
      zh_comSetComError( pCom, ZH_COM_ERR_NOSUPPORT );

   if( piValue )
      *piValue = iValue;

   return iResult;
}

int zh_comSendBreak( int iPort, int iDurationInMilliSecs )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );

   ZH_SYMBOL_UNUSED( iDurationInMilliSecs );

   if( pCom )
      zh_comSetComError( pCom, ZH_COM_ERR_NOSUPPORT );

   return -1;
}

int zh_comFlowControl( int iPort, int *piFlow, int iFlow )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   int iResult = -1, iValue = 0;

   if( pCom )
   {
      int iFlowVal = COMGetFlowControl( iPort - 1 );

      if( iFlowVal & FLOW_RTS )
         iValue |= ZH_COM_FLOW_IRTSCTS;
      if( iFlowVal & FLOW_CTS )
         iValue |= ZH_COM_FLOW_ORTSCTS;
      if( iFlowVal & FLOW_DTR )
         iValue |= ZH_COM_FLOW_IDTRDSR;
      if( iFlowVal & FLOW_DSR )
         iValue |= ZH_COM_FLOW_ODTRDSR;
      if( iFlowVal & FLOW_DCD )
         iValue |= ZH_COM_FLOW_DCD;
      if( iFlowVal & FLOW_XOFF )
         iValue |= ZH_COM_FLOW_XOFF;
      if( iFlowVal & FLOW_XON )
         iValue |= ZH_COM_FLOW_XON;

      if( iFlow >= 0 )
      {
         iFlowVal = 0;
         if( iFlow & ZH_COM_FLOW_IRTSCTS )
            iFlowVal |= FLOW_RTS;
         if( iFlow & ZH_COM_FLOW_ORTSCTS )
            iFlowVal |= FLOW_CTS;
         if( iFlow & ZH_COM_FLOW_IDTRDSR )
            iFlowVal |= FLOW_DTR;
         if( iFlow & ZH_COM_FLOW_ODTRDSR )
            iFlowVal |= FLOW_DSR;
         if( iFlow & ZH_COM_FLOW_DCD )
            iFlowVal |= FLOW_DCD;
         if( iFlow & ZH_COM_FLOW_XOFF )
            iFlowVal |= FLOW_XOFF;
         if( iFlow & ZH_COM_FLOW_XON )
            iFlowVal |= FLOW_XON;

         COMSetFlowControl( iPort - 1, iFlowVal );
      }

      iResult = 0;
   }

   if( piFlow )
      *piFlow = iValue;

   return iResult;
}

int zh_comFlowSet( int iPort, int iFlow )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );

   ZH_SYMBOL_UNUSED( iFlow );

   if( pCom )
      zh_comSetComError( pCom, ZH_COM_ERR_NOSUPPORT );

   return -1;
}

int zh_comFlowChars( int iPort, int iXONchar, int iXOFFchar )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      COMSetFlowChars( iPort - 1, iXONchar, iXOFFchar );
      iResult = 0;
   }

   return iResult;
}

int zh_comDiscardChar( int iPort, int iChar )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );

   ZH_SYMBOL_UNUSED( iChar );

   if( pCom )
      zh_comSetComError( pCom, ZH_COM_ERR_NOSUPPORT );

   return -1;
}

int zh_comErrorChar( int iPort, int iChar )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );

   ZH_SYMBOL_UNUSED( iChar );

   if( pCom )
      zh_comSetComError( pCom, ZH_COM_ERR_NOSUPPORT );

   return -1;
}

int zh_comOutputState( int iPort )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      iResult = COMRXBufferUsed( iPort - 1 );
      if( iResult == 0 )
         iResult = ZH_COM_TX_EMPTY;
      else if( iResult > 0 )
         iResult = 0;
   }

   return iResult;
}

int zh_comInputState( int iPort )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   int iResult = -1;

   if( pCom )
      zh_comSetComError( pCom, ZH_COM_ERR_NOSUPPORT );

   return iResult;
}

long zh_comSend( int iPort, const void * data, long len, ZH_MAXINT timeout )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   long lSent = -1;

   if( pCom )
   {
      const char * buffer = ( const char * ) data;
      ZH_MAXUINT timer = zh_timerInit( timeout );

      zh_comSetOsError( pCom, 0 );
      lSent = 0;

      zh_vmUnlock();

      while( len > 0 )
      {
         int iSent, iErr;

         iErr = COMWriteBuffer( iPort - 1, buffer, NULL, len, &iSent );
         lSent += iSent;
         if( iErr == COMERR_TXOVERFLOW )
         {
            buffer += iSent;
            len -= iSent;
            if( ( timeout = zh_timerTest( timeout, &timer ) ) == 0 )
            {
               if( lSent == 0 )
               {
                  zh_comSetComError( pCom, ZH_COM_ERR_TIMEOUT );
                  lSent = -1;
               }
               break;
            }
            zh_releaseCPU();
         }
         else
            break;
      }

      zh_vmLock();
   }

   return lSent;
}

long zh_comRecv( int iPort, void * data, long len, ZH_MAXINT timeout )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   long lReceived = -1;

   if( pCom )
   {
      char * buffer = ( char * ) data;
      ZH_MAXUINT timer = zh_timerInit( timeout );

      zh_comSetOsError( pCom, 0 );
      lReceived = 0;

      zh_vmUnlock();

      while( len > 0 )
      {
         int iErr = COMReadChar( iPort - 1, buffer, NULL );

         if( iErr == 0 )
         {
            ++buffer;
            --len;
            ++lReceived;
         }
         else if( iErr == COM_BUFEMPTY )
         {
            if( lReceived > 0 || ( timeout = zh_timerTest( timeout, &timer ) ) == 0 )
            {
               if( lReceived == 0 )
               {
                  zh_comSetComError( pCom, ZH_COM_ERR_TIMEOUT );
                  lReceived = -1;
               }
               break;
            }
            zh_releaseCPU();
         }
         else
         {
            zh_comSetOsError( pCom, iErr );
            break;
         }
      }

      zh_vmLock();
   }

   return lReceived;
}

static int s_comChkPortParam( int *piBaud, int *piParity,
                              int *piSize, int *piStop )
{
   int iResult = 0;

   if( *piBaud == 0 )
      *piBaud = 9600;

   *piParity = ZH_TOUPPER( *piParity );
   switch( *piParity )
   {
      case 0:
         *piParity = 'N';
      case 'N':
      case 'E':
      case 'O':
      case 'S':
      case 'M':
         break;

      default:
         iResult = -1;
   }

   switch( *piSize )
   {
      case 0:
         *piSize = 8;
      case 8:
      case 7:
      case 6:
      case 5:
         break;
      default:
         iResult = -1;
   }

   switch( *piStop )
   {
      case 0:
         *piStop = 1;
      case 1:
      case 2:
         break;
      default:
         iResult = -1;
   }

   return iResult;
}

int zh_comInit( int iPort, int iBaud, int iParity, int iSize, int iStop )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      iResult = s_comChkPortParam( &iBaud, &iParity, &iSize, &iStop );
      if( iResult == 0 )
      {
         COMSetTransmitParameters( iPort - 1, iBaud, iSize, iParity, iStop );
         zh_comSetOsError( pCom, 0 );
      }
      else
         zh_comSetComError( pCom, ZH_COM_ERR_PARAMVALUE );
   }

   return iResult;
}

int zh_comClose( int iPort )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      zh_vmUnlock();
      COMPortClose( iPort - 1 );
      pCom->status &= ~ZH_COM_OPEN;
      zh_comSetOsError( pCom, 0 );
      iResult = 0;
      zh_vmLock();
   }

   return iResult;
}

int zh_comOpen( int iPort )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_ENABLED );
   int iResult = -1;

   if( pCom )
   {
      if( pCom->status & ZH_COM_OPEN )
      {
         zh_comSetComError( pCom, ZH_COM_ERR_ALREADYOPEN );
      }
      else
      {
         int iBaud, iParity, iSize, iStop, iFlowControl;

         zh_vmUnlock();

         iBaud = iParity = iSize = iStop = 0;
         iFlowControl = 0;
         s_comChkPortParam( &iBaud, &iParity, &iSize, &iStop );
         iResult = COMPortOpen( iPort - 1, iBaud, iSize, iParity, iStop,
                                iFlowControl, NULL );
         if( iResult == 0 )
         {
            pCom->status |= ZH_COM_OPEN;
            zh_comSetOsError( pCom, 0 );
         }
         else
         {
            zh_comSetOsError( pCom, iResult );
            iResult = -1;
         }

         zh_vmLock();
      }
   }

   return iResult;
}

/* end of ZH_HAS_PMCOM */

#else

int zh_comInputCount( int iPort )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );

   if( pCom )
      zh_comSetComError( pCom, ZH_COM_ERR_NOSUPPORT );

   return -1;
}

int zh_comOutputCount( int iPort )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );

   if( pCom )
      zh_comSetComError( pCom, ZH_COM_ERR_NOSUPPORT );

   return -1;
}

int zh_comFlush( int iPort, int iType )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );

   ZH_SYMBOL_UNUSED( iType );

   if( pCom )
      zh_comSetComError( pCom, ZH_COM_ERR_NOSUPPORT );

   return -1;
}

int zh_comMCR( int iPort, int * piValue, int iClr, int iSet )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );

   ZH_SYMBOL_UNUSED( piValue );
   ZH_SYMBOL_UNUSED( iClr );
   ZH_SYMBOL_UNUSED( iSet );

   if( pCom )
      zh_comSetComError( pCom, ZH_COM_ERR_NOSUPPORT );

   return -1;
}

int zh_comMSR( int iPort, int * piValue )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );

   ZH_SYMBOL_UNUSED( piValue );

   if( pCom )
      zh_comSetComError( pCom, ZH_COM_ERR_NOSUPPORT );

   return -1;
}

int zh_comLSR( int iPort, int * piValue )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );

   ZH_SYMBOL_UNUSED( piValue );

   if( pCom )
      zh_comSetComError( pCom, ZH_COM_ERR_NOSUPPORT );

   return -1;
}

int zh_comSendBreak( int iPort, int iDurationInMilliSecs )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );

   ZH_SYMBOL_UNUSED( iDurationInMilliSecs );

   if( pCom )
      zh_comSetComError( pCom, ZH_COM_ERR_NOSUPPORT );

   return -1;
}

int zh_comFlowControl( int iPort, int *piFlow, int iFlow )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );

   ZH_SYMBOL_UNUSED( piFlow );
   ZH_SYMBOL_UNUSED( iFlow );

   if( pCom )
      zh_comSetComError( pCom, ZH_COM_ERR_NOSUPPORT );

   return -1;
}

int zh_comFlowSet( int iPort, int iFlow )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );

   ZH_SYMBOL_UNUSED( iFlow );

   if( pCom )
      zh_comSetComError( pCom, ZH_COM_ERR_NOSUPPORT );

   return -1;
}

int zh_comFlowChars( int iPort, int iXONchar, int iXOFFchar )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );

   ZH_SYMBOL_UNUSED( iXONchar );
   ZH_SYMBOL_UNUSED( iXOFFchar );

   if( pCom )
      zh_comSetComError( pCom, ZH_COM_ERR_NOSUPPORT );

   return -1;
}

int zh_comDiscardChar( int iPort, int iChar )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );

   ZH_SYMBOL_UNUSED( iChar );

   if( pCom )
      zh_comSetComError( pCom, ZH_COM_ERR_NOSUPPORT );

   return -1;
}

int zh_comErrorChar( int iPort, int iChar )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );

   ZH_SYMBOL_UNUSED( iChar );

   if( pCom )
      zh_comSetComError( pCom, ZH_COM_ERR_NOSUPPORT );

   return -1;
}

int zh_comOutputState( int iPort )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );

   if( pCom )
      zh_comSetComError( pCom, ZH_COM_ERR_NOSUPPORT );

   return -1;
}

int zh_comInputState( int iPort )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );

   if( pCom )
      zh_comSetComError( pCom, ZH_COM_ERR_NOSUPPORT );

   return -1;
}

long zh_comSend( int iPort, const void * data, long len, ZH_MAXINT timeout )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );

   ZH_SYMBOL_UNUSED( data );
   ZH_SYMBOL_UNUSED( len );
   ZH_SYMBOL_UNUSED( timeout );

   if( pCom )
      zh_comSetComError( pCom, ZH_COM_ERR_NOSUPPORT );

   return -1;
}

long zh_comRecv( int iPort, void * data, long len, ZH_MAXINT timeout )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );

   ZH_SYMBOL_UNUSED( data );
   ZH_SYMBOL_UNUSED( len );
   ZH_SYMBOL_UNUSED( timeout );

   if( pCom )
      zh_comSetComError( pCom, ZH_COM_ERR_NOSUPPORT );

   return -1;
}

int zh_comInit( int iPort, int iBaud, int iParity, int iSize, int iStop )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );

   ZH_SYMBOL_UNUSED( iBaud );
   ZH_SYMBOL_UNUSED( iParity );
   ZH_SYMBOL_UNUSED( iSize );
   ZH_SYMBOL_UNUSED( iStop );

   if( pCom )
      zh_comSetComError( pCom, ZH_COM_ERR_NOSUPPORT );

   return -1;
}

int zh_comClose( int iPort )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_OPEN );

   if( pCom )
      zh_comSetComError( pCom, ZH_COM_ERR_NOSUPPORT );

   return -1;
}

int zh_comOpen( int iPort )
{
   PZH_COM pCom = zh_comGetPort( iPort, ZH_COM_ENABLED );
   int iTODO_serial_port_support;

   if( pCom )
      zh_comSetComError( pCom, ZH_COM_ERR_NOSUPPORT );

   return -1;
}

#endif


static int s_iComInit = 0;

static void zh_com_exit( void* cargo )
{
   ZH_SYMBOL_UNUSED( cargo );

   if( s_iComInit )
   {
      zh_comCloseAll();
      s_iComInit = 0;
   }
}

static void zh_com_init( void* cargo )
{
   ZH_SYMBOL_UNUSED( cargo );

   if( ! s_iComInit )
   {
      int iPort;

      for( iPort = 0; iPort < ZH_COM_PORT_MAX; ++iPort )
      {
         s_comList[ iPort ].port = iPort + 1;
         s_comList[ iPort ].status = ZH_COM_ENABLED;
      }

      zh_vmAtQuit( zh_com_exit, NULL );

      s_iComInit = 1;
   }
}

ZH_CALL_ON_STARTUP_BEGIN( _zh_com_init_ )
   zh_vmAtInit( zh_com_init, NULL );
ZH_CALL_ON_STARTUP_END( _zh_com_init_ )

#if defined( ZH_PRAGMA_STARTUP )
   #pragma startup _zh_com_init_
#elif defined( ZH_DATASEG_STARTUP )
   #define ZH_DATASEG_BODY    ZH_DATASEG_FUNC( _zh_com_init_ )
   #include "zh_ini_seg.h"
#endif
