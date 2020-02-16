/*
 * The FileSys API (C level)
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
 * Copyright 1999-2010 Viktor Szakats
 *    zh_fsSetError(), zh_fsSetDevMode(), zh_fsReadLarge(), zh_fsWriteLarge()
 *    zh_fsCurDirBuff(), zh_fsBaseDirBuff()
 *    fs_win_get_drive(), fs_win_set_drive()
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
 *    zh_fsChDrv(), zh_fsCurDrv(), zh_fsIsDrv(), zh_fsIsDevice()
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>, David G. Holm <dholm@jsd-llc.com>
 *    zh_fsEof()
 * Copyright 2001 Jose Gimenez (JFG) <jfgimenez@wanadoo.es>, <tecnico.sireinsa@ctv.es>
 *    Added platform check for any compiler to use the Windows
 *    API calls to allow opening an unlimited number of files
 *    simultaneously.
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

/* NOTE: In DOS/DJGPP under WinNT4 zh_fsSeek( fhnd, offset < 0, FS_SET ) will
         set the file pointer to the passed negative value and the subsequent
         zh_fsWrite() call will fail. In CA-Cl*pper, _fsSeek() will fail,
         the pointer will not be moved and thus the _fsWrite() call will
         successfully write the buffer to the current file position. [vszakats]

   This has been corrected by ptucker
 */

/* *nixes */
#if ! defined( _LARGEFILE64_SOURCE )
#  define _LARGEFILE64_SOURCE  1
#endif
#if ! defined( _GNU_SOURCE )
#  define _GNU_SOURCE
#endif

#include "zh_api.h"
#include "zh_vm.h"
#include "zh_stack.h"
#include "zh_apifs.h"
#include "zh_gt_api.h"
#include "zh_api_error.h"
#include "zh_codepage_api.h"
#include "zh_date.h"
#include "zh_io.h"
#include "zh_set.h"

#if defined( ZH_OS_UNIX )
   #include <unistd.h>
   #include <time.h>
   #include <utime.h>
   #include <sys/types.h>
   #include <sys/wait.h>
   #include <sys/time.h>
   #if ! defined( ZH_HAS_POLL ) && ! defined( ZH_NO_POLL ) && \
         defined( _POSIX_C_SOURCE ) && _POSIX_C_SOURCE >= 200112L
      /* use poll() instead of select() to avoid FD_SETSIZE (1024 in Linux)
         file handle limit */
      #define ZH_HAS_POLL
   #endif
   #if defined( ZH_HAS_POLL )
      #include <poll.h>
   #endif
#endif
#if ! defined( ZH_OS_WIN )
#  include <errno.h>
#endif

#if ( defined( _MSC_VER ) || defined( __MINGW32__ ) ) && ! defined( ZH_OS_UNIX )
   #include <sys/stat.h>
   #include <fcntl.h>
   #include <process.h>
   #include <direct.h>

   #if defined( _MSC_VER ) || defined( __MINGW32__ )
      #include <sys/locking.h>
      #define ftruncate _chsize
      #if defined( __MINGW32__ ) && ! defined( _LK_UNLCK )
         #define _LK_UNLCK _LK_UNLOCK
      #endif
   #else
      #define ftruncate chsize
   #endif
   #if ! defined( HAVE_POSIX_IO )
      #define HAVE_POSIX_IO
   #endif
#elif defined( __GNUC__ ) || defined( ZH_OS_UNIX )
   #include <sys/types.h>
   #include <sys/stat.h>
   #include <fcntl.h>
   #if ! defined( HAVE_POSIX_IO )
      #define HAVE_POSIX_IO
   #endif
#endif

#if defined( ZH_OS_WIN )
   #include <windows.h>
   #include "hbwinuni.h"
   #if ! defined( INVALID_SET_FILE_POINTER ) && defined( _MSC_VER )
      #define INVALID_SET_FILE_POINTER ( ( DWORD ) -1 )
   #endif
   #if ! defined( INVALID_FILE_ATTRIBUTES )
      #define INVALID_FILE_ATTRIBUTES     ( ( DWORD ) -1 )
   #endif
   #if defined( ZH_OS_WIN_64 )
      #if ! defined( ZH_WIN_IOREAD_LIMIT )
         #define ZH_WIN_IOREAD_LIMIT      ZH_U32_MAX
      #endif
      #if ! defined( ZH_WIN_IOWRITE_LIMIT )
         #define ZH_WIN_IOWRITE_LIMIT     ZH_U32_MAX
      #endif
   #endif
#endif
#if defined( ZH_USE_SHARELOCKS ) && defined( ZH_USE_BSDLOCKS )
   #include <sys/file.h>
#endif
#if defined( ZH_OS_LINUX )
#  define ZH_HAS_SELECT_TIMER
#endif

#if ! defined( ZH_USE_LARGEFILE64 ) && defined( ZH_OS_UNIX )
   #if defined( __USE_LARGEFILE64 )
      /*
       * The macro: __USE_LARGEFILE64 is set when _LARGEFILE64_SOURCE is
       * defined and effectively enables lseek64()/flock64()/ftruncate64()
       * functions on 32-bit machines.
       */
      #define ZH_USE_LARGEFILE64
   #elif defined( ZH_OS_UNIX ) && defined( O_LARGEFILE ) && ! defined( __WATCOMC__ )
      #define ZH_USE_LARGEFILE64
   #endif
#endif

#if defined( ZH_OS_HAS_DRIVE_LETTER )
/* 2004-08-27 - <maurilio.longo@libero.it>
                ZH_FS_GETDRIVE() should return a number in the range 0..25 ('A'..'Z')
                ZH_FS_SETDRIVE() should accept a number inside same range.

                If a particular platform/compiler returns/accepts different ranges of
                values, simply define a branch for that platform.

                NOTE: There is not an implicit "current disk", ALWAYS use

                        my_func( zh_fsCurDrv(), ...)

                      to refer to current disk
 */

#if defined( ZH_OS_WIN )

   #define ZH_FS_GETDRIVE(n)  do { n = fs_win_get_drive(); } while( 0 )
   #define ZH_FS_SETDRIVE(n)  fs_win_set_drive( n )

#else /* _MSC_VER */
   /* 1 based version */

   #define ZH_FS_GETDRIVE(n)  do { n = _getdrive() - 1; } while( 0 )
   #define ZH_FS_SETDRIVE(n)  _chdrive( ( n ) + 1 )

#endif
#endif /* ZH_OS_HAS_DRIVE_LETTER */

#ifndef O_BINARY
   #define O_BINARY     0       /* O_BINARY not defined on Linux */
#endif

#ifndef O_LARGEFILE
   #define O_LARGEFILE  0       /* O_LARGEFILE is used for LFS in 32-bit Linux */
#endif

#if ! defined( ZH_OS_UNIX )
   #if ! defined( S_IREAD ) && defined( S_IRUSR )
      #define S_IREAD   S_IRUSR
   #endif
   #if ! defined( S_IWRITE ) && defined( S_IWUSR )
      #define S_IWRITE  S_IWUSR
   #endif
   #if ! defined( S_IEXEC ) && defined( S_IXUSR )
      #define S_IEXEC   S_IXUSR
   #endif
#endif


#if defined( _MSC_VER ) || defined( __MINGW32__ )
/* These compilers use sopen() rather than open(), because their
   versions of open() do not support combined O_ and SH_ flags */
   #define ZH_FS_SOPEN
#endif

#if defined( ZH_OS_ANDROID )
   /* hack for missing functions in android libc library */
   #define fdatasync          fsync
   #define ftruncate64        ftruncate
   #define pread64            pread
   #define pwrite64(f,b,s,o)  pwrite(f,(void*)b,s,o)
#elif defined( ZH_OS_MINIX )
   /* hack for functions missing from the Minix C library */
   #define fdatasync          fsync
   #define ftruncate64        ftruncate
#endif

#if UINT_MAX == USHRT_MAX
   #define ZH_FS_IO_16BIT
#endif

#if defined( ZH_OS_UNIX ) && defined( EINTR )
#  define ZH_FAILURE_RETRY( ret, exp ) \
   do \
   { \
      ( ret ) = ( exp ); \
      zh_fsSetIOError( ( ret ) != -1, 0 ); \
   } \
   while( ( ret ) == -1 && zh_fsOsError() == ( ZH_ERRCODE ) EINTR && \
          zh_vmRequestQuery() == 0 )
#else
#  define ZH_FAILURE_RETRY( ret, exp ) \
   do \
   { \
      ( ret ) = ( exp ); \
      zh_fsSetIOError( ( ret ) != -1, 0 ); \
   } \
   while( 0 )
#endif

static ZH_BOOL s_fUseWaitLocks = ZH_TRUE;

#if defined( ZH_OS_WIN ) && defined( ZH_OS_HAS_DRIVE_LETTER )

static int fs_win_get_drive( void )
{
   TCHAR pBuffer[ ZH_PATH_MAX ];
   LPTSTR lpBuffer = pBuffer;
   DWORD dwResult, dwSize;
   int iDrive = 0;

   dwSize = ZH_SIZEOFARRAY( pBuffer );
   dwResult = GetCurrentDirectory( dwSize, lpBuffer );
   if( dwResult > dwSize )
   {
      dwSize = dwResult;
      lpBuffer = ( TCHAR * ) zh_xgrab( dwSize * sizeof( TCHAR ) );
      dwResult = GetCurrentDirectory( dwSize, lpBuffer );
   }
   zh_fsSetIOError( dwResult != 0, 0 );
   if( dwResult >= 2 && dwResult < dwSize &&
       lpBuffer[ 1 ] == ZH_OS_DRIVE_DELIM_CHR )
   {
      iDrive = ZH_TOUPPER( lpBuffer[ 0 ] );
      if( iDrive >= 'A' && iDrive <= 'Z' )
         iDrive -= 'A';
      else
         iDrive = 0;
   }
   if( lpBuffer != pBuffer )
      zh_xfree( lpBuffer );
   return iDrive;
}

static void fs_win_set_drive( int iDrive )
{
   if( iDrive >= 0 && iDrive <= 25 )
   {
      TCHAR szBuffer[ 3 ];
      ZH_BOOL fResult;
      UINT uiErrMode;

      szBuffer[ 0 ] = ( TCHAR ) ( iDrive + 'A' );
      szBuffer[ 1 ] = TEXT( ':' );
      szBuffer[ 2 ] = TEXT( '\0' );

      uiErrMode = SetErrorMode( SEM_FAILCRITICALERRORS );
      fResult = SetCurrentDirectory( szBuffer ) != FALSE;
      SetErrorMode( uiErrMode );

      zh_fsSetIOError( fResult, 0 );
   }
}

#endif

#if defined( ZH_OS_WIN )

static HANDLE DosToWinHandle( ZH_FHANDLE fHandle )
{
   switch( fHandle )
   {
      case ( ZH_FHANDLE ) FS_ERROR:
         return NULL;
      case ( ZH_FHANDLE ) ZH_STDIN_HANDLE:
         return GetStdHandle( STD_INPUT_HANDLE );
      case ( ZH_FHANDLE ) ZH_STDOUT_HANDLE:
         return GetStdHandle( STD_OUTPUT_HANDLE );
      case ( ZH_FHANDLE ) ZH_STDERR_HANDLE:
         return GetStdHandle( STD_ERROR_HANDLE );
   }
   return ( HANDLE ) fHandle;
}

static void convert_open_flags( ZH_BOOL fCreate, ZH_FATTR nAttr, ZH_USHORT uiFlags,
                                DWORD * dwMode, DWORD * dwShare,
                                DWORD * dwCreat, DWORD * dwAttr )
{
   if( fCreate )
   {
      *dwCreat = ( uiFlags & FO_EXCL ) ? CREATE_NEW : CREATE_ALWAYS;
      *dwMode = GENERIC_READ | GENERIC_WRITE;
   }
   else
   {
      if( uiFlags & FO_CREAT )
      {
         if( uiFlags & FO_EXCL )
            *dwCreat = CREATE_NEW;
         else if( uiFlags & FO_TRUNC )
            *dwCreat = CREATE_ALWAYS;
         else
            *dwCreat = OPEN_ALWAYS;
      }
      else if( uiFlags & FO_TRUNC )
         *dwCreat = TRUNCATE_EXISTING;
      else
         *dwCreat = OPEN_EXISTING;

      *dwMode = 0;
      switch( uiFlags & ( FO_READ | FO_WRITE | FO_READWRITE ) )
      {
         case FO_READWRITE:
            *dwMode |= GENERIC_READ | GENERIC_WRITE;
            break;
         case FO_WRITE:
            *dwMode |= GENERIC_WRITE;
            break;
         case FO_READ:
            *dwMode |= GENERIC_READ;
            break;
      }
   }

   /* shared flags */
   switch( uiFlags & ( FO_DENYREAD | FO_DENYWRITE | FO_EXCLUSIVE | FO_DENYNONE ) )
   {
      case FO_DENYREAD:
         *dwShare = FILE_SHARE_WRITE;
         break;
      case FO_DENYWRITE:
         *dwShare = FILE_SHARE_READ;
         break;
      case FO_EXCLUSIVE:
         *dwShare = 0;
         break;
      default:
         *dwShare = FILE_SHARE_WRITE | FILE_SHARE_READ;
         break;
   }

   /* file attributes flags */
   if( nAttr == FC_NORMAL )
      *dwAttr = FILE_ATTRIBUTE_NORMAL;
   else
   {
      *dwAttr = FILE_ATTRIBUTE_ARCHIVE;
      if( nAttr & FC_READONLY )
         *dwAttr |= FILE_ATTRIBUTE_READONLY;
      if( nAttr & FC_HIDDEN )
         *dwAttr |= FILE_ATTRIBUTE_HIDDEN;
      if( nAttr & FC_SYSTEM )
         *dwAttr |= FILE_ATTRIBUTE_SYSTEM;
   }
}


#else

static void convert_open_flags( ZH_BOOL fCreate, ZH_FATTR nAttr, ZH_USHORT uiFlags,
                                int * flags, unsigned * mode,
                                int * share, int * attr )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "convert_open_flags(%d, %u, %hu, %p, %p, %p, %p)", fCreate, nAttr, uiFlags, ( void * ) flags, ( void * ) mode, ( void * ) share, ( void * ) attr ) );

   /* file access mode */
#if defined( ZH_OS_UNIX )
   *mode = ZH_FA_POSIX_ATTR( nAttr );
   if( *mode == 0 )
   {
      *mode = S_IRUSR | S_IRGRP | S_IROTH;
      if( ! ( nAttr & ZH_FA_READONLY ) )
         *mode |= S_IWUSR | S_IWGRP | S_IWOTH;
      if( nAttr & ZH_FA_SYSTEM )
         *mode |= S_IXUSR | S_IXGRP | S_IXOTH;
      if( nAttr & ZH_FA_HIDDEN )
         *mode &= S_IRUSR | S_IWUSR | S_IXUSR;
   }
#else
   *mode = S_IREAD |
           ( ( nAttr & FC_READONLY ) ? 0 : S_IWRITE ) |
           ( ( nAttr & FC_SYSTEM ) ? S_IEXEC : 0 );
#endif

   *attr = 0;

   if( fCreate )
   {
      *flags = O_RDWR | O_CREAT | O_TRUNC | O_BINARY | O_LARGEFILE |
               ( ( uiFlags & FO_EXCL ) ? O_EXCL : 0 );
   }
   else
   {
      *flags = O_BINARY | O_LARGEFILE;
      switch( uiFlags & ( FO_READ | FO_WRITE | FO_READWRITE ) )
      {
         case FO_READ:
            *flags |= O_RDONLY;
            break;
         case FO_WRITE:
            *flags |= O_WRONLY;
            break;
         case FO_READWRITE:
            *flags |= O_RDWR;
            break;
         default:
            /* this should not happen and it's here to force default OS behavior */
            *flags |= ( O_RDONLY | O_WRONLY | O_RDWR );
            break;
      }

      if( uiFlags & FO_CREAT ) *flags |= O_CREAT;
      if( uiFlags & FO_TRUNC ) *flags |= O_TRUNC;
      if( uiFlags & FO_EXCL  ) *flags |= O_EXCL;
   }

   /* shared flags (ZH_FS_SOPEN) */
#if defined( _MSC_VER )
   if( ( uiFlags & FO_DENYREAD ) == FO_DENYREAD )
      *share = _SH_DENYRD;
   else if( uiFlags & FO_EXCLUSIVE )
      *share = _SH_DENYRW;
   else if( uiFlags & FO_DENYWRITE )
      *share = _SH_DENYWR;
   else if( uiFlags & FO_DENYNONE )
      *share = _SH_DENYNO;
   else
      *share = _SH_COMPAT;
#elif ! defined( ZH_OS_UNIX )
   if( ( uiFlags & FO_DENYREAD ) == FO_DENYREAD )
      *share = SH_DENYRD;
   else if( uiFlags & FO_EXCLUSIVE )
      *share = SH_DENYRW;
   else if( uiFlags & FO_DENYWRITE )
      *share = SH_DENYWR;
   else if( uiFlags & FO_DENYNONE )
      *share = SH_DENYNO;
   else
      *share = SH_COMPAT;
#else
   *share = 0;
#endif

   ZH_TRACE( ZH_TR_INFO, ( "convert_open_flags: flags=0x%04x, mode=0x%04x, share=0x%04x, attr=0x%04x", *flags, *mode, *share, *attr ) );

}
#endif

static ZH_USHORT convert_seek_flags( ZH_USHORT uiFlags )
{
   /* by default FS_SET is set */
   ZH_USHORT result_flags = SEEK_SET;

   ZH_TRACE( ZH_TR_DEBUG, ( "convert_seek_flags(%hu)", uiFlags ) );

   if( uiFlags & FS_RELATIVE )
      result_flags = SEEK_CUR;

   if( uiFlags & FS_END )
      result_flags = SEEK_END;

   return result_flags;
}


/*
 * filesys.api functions:
 */

ZH_FHANDLE zh_fsGetOsHandle( ZH_FHANDLE hFileHandle )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsGetOsHandle(%p)", ( void * ) ( ZH_PTRUINT ) hFileHandle ) );

#if defined( ZH_OS_WIN )
   return ( ZH_FHANDLE ) DosToWinHandle( hFileHandle );
#else
   return hFileHandle;
#endif
}

#if defined( ZH_OS_UNIX )
/* for POSIX systems only, hides low-level select()/poll() access,
   intentionally covered by ZH_OS_UNIX macro to generate compile time
   error in code which tries to use it on other platforms */

static int zh_fsCanAccess( ZH_FHANDLE hFile, ZH_MAXINT nTimeOut, ZH_BOOL fRead )
{
   int iResult;

   zh_vmUnlock();

#if defined( ZH_HAS_POLL )
{
   ZH_MAXUINT timer = zh_timerInit( nTimeOut );
   struct pollfd fds;
   short int events = fRead ? POLLIN : POLLOUT;

   fds.fd = hFile;
   fds.events = events;
   fds.revents = 0;

   for( ;; )
   {
      ZH_BOOL fLast = nTimeOut >= 0 && nTimeOut <= 1000;
      int tout = fLast ? ( int ) nTimeOut : 1000;

      iResult = poll( &fds, 1, tout );
      zh_fsSetIOError( iResult >= 0, 0 );
      if( iResult > 0 && ( fds.revents & events ) == 0 )
      {
         if( ( fds.revents & ( POLLHUP | POLLNVAL | POLLERR ) ) != 0 )
         {
            iResult = -1;
            break;
         }
         iResult = 0;
      }
      else if( iResult == -1 && zh_fsOsError() == ( ZH_ERRCODE ) EINTR )
      {
         iResult = 0;
         fLast = ZH_FALSE;
      }

      if( iResult == 0 && ! fLast && zh_vmRequestQuery() == 0 &&
          ( nTimeOut = zh_timerTest( nTimeOut, &timer ) ) != 0 )
         continue;

      break;
   }
}
#elif 1  /* ! ZH_HAS_POLL */
{
#  if ! defined( ZH_HAS_SELECT_TIMER )
   ZH_MAXUINT timer = zh_timerInit( nTimeOut );
#  endif

   for( ;; )
   {
      struct timeval tv;
      fd_set fds;

      if( nTimeOut < 0 || nTimeOut >= 1000 )
      {
         tv.tv_sec = 1;
         tv.tv_usec = 0;
      }
      else
      {
         tv.tv_sec = ( long ) nTimeOut / 1000;
         tv.tv_usec = ( long ) ( nTimeOut % 1000 ) * 1000;
      }

      FD_ZERO( &fds );
      FD_SET( hFile, &fds );
      iResult = select( hFile + 1, fRead ? &fds : NULL,
                                   fRead ? NULL : &fds, NULL, &tv );
      zh_fsSetIOError( iResult >= 0, 0 );

      if( iResult == -1 && zh_fsOsError() == ( ZH_ERRCODE ) EINTR )
      {
         iResult = 0;
#  if defined( ZH_HAS_SELECT_TIMER )
         if( nTimeOut > 0 )
            nTimeOut += tv.tv_sec * 1000 + tv.tv_usec / 1000;
#  endif
      }
#  if defined( ZH_HAS_SELECT_TIMER )
      if( iResult == 0 && nTimeOut > 0 )
      {
         if( ( nTimeOut -= 1000 ) < 0 )
            break;
      }

      if( iResult != 0 || nTimeOut == 0 || zh_vmRequestQuery() != 0 )
         break;
#  else
      if( iResult != 0 || ( nTimeOut = zh_timerTest( nTimeOut, &timer ) ) == 0 ||
          zh_vmRequestQuery() != 0 )
         break;
#  endif
   }
}
#else
{
   int iTODO; /* TODO: for given platform */

   ZH_SYMBOL_UNUSED( hFile );
   ZH_SYMBOL_UNUSED( nTimeOut );
   ZH_SYMBOL_UNUSED( fRead );
   iResult = -1;
}
#endif /* ! ZH_HAS_POLL */

   zh_vmLock();

   return iResult;
}

int zh_fsCanRead( ZH_FHANDLE hFileHandle, ZH_MAXINT nTimeOut )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsCanRead(%p, %" PFHL "d)", ( void * ) ( ZH_PTRUINT ) hFileHandle, nTimeOut ) );

   return zh_fsCanAccess( hFileHandle, nTimeOut, ZH_TRUE );
}

int zh_fsCanWrite( ZH_FHANDLE hFileHandle, ZH_MAXINT nTimeOut )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsCanWrite(%p, %" PFHL "d)", ( void * ) ( ZH_PTRUINT ) hFileHandle, nTimeOut ) );

   return zh_fsCanAccess( hFileHandle, nTimeOut, ZH_FALSE );
}

int zh_fsPoll( PZH_POLLFD pPollSet, int iCount, ZH_MAXINT nTimeOut )
{
   int iResult;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsPoll(%p, %d, %" PFHL "d)", ( void * ) pPollSet, iCount, nTimeOut ) );

   zh_vmUnlock();

#if defined( ZH_HAS_POLL )
{
   struct pollfd fds[ 16 ], * pfds;
   static const ZH_BOOL s_fSamePoll =
                  sizeof( struct pollfd ) == sizeof( ZH_POLLFD ) &&
                  sizeof( pPollSet->fd ) == sizeof( fds[ 0 ].fd ) &&
                  sizeof( pPollSet->events ) == sizeof( fds[ 0 ].events ) &&
                  sizeof( pPollSet->revents ) == sizeof( fds[ 0 ].revents ) &&
                  ZH_POLLIN == POLLIN && ZH_POLLPRI == POLLPRI &&
                  ZH_POLLOUT == POLLOUT && ZH_POLLERR == POLLERR &&
                  ZH_POLLHUP == POLLHUP && ZH_POLLNVAL == POLLNVAL;

   ZH_MAXUINT timer;
   void * pFree = NULL;
   int i;

   if( s_fSamePoll )
      pfds = ( struct pollfd * ) pPollSet;
   else
   {
      if( iCount <= ( int ) ZH_SIZEOFARRAY( fds ) )
         pfds = fds;
      else
         pfds = ( struct pollfd * ) ( pFree = zh_xgrab( sizeof( struct pollfd ) * iCount ) );

      for( i = 0; i < iCount; ++i )
      {
         pfds[ i ].fd = pPollSet[ i ].fd;
         pfds[ i ].events = ( ( pPollSet[ i ].events & ZH_POLLIN   ) ? POLLIN   : 0 ) |
                            ( ( pPollSet[ i ].events & ZH_POLLPRI  ) ? POLLPRI  : 0 ) |
                            ( ( pPollSet[ i ].events & ZH_POLLOUT  ) ? POLLOUT  : 0 ) |
                            ( ( pPollSet[ i ].events & ZH_POLLERR  ) ? POLLERR  : 0 ) |
                            ( ( pPollSet[ i ].events & ZH_POLLHUP  ) ? POLLHUP  : 0 ) |
                            ( ( pPollSet[ i ].events & ZH_POLLNVAL ) ? POLLNVAL : 0 );
         pfds[ i ].revents = 0;
      }
   }

   timer = zh_timerInit( nTimeOut );
   for( ;; )
   {
      ZH_BOOL fLast = nTimeOut >= 0 && nTimeOut <= 1000;
      int tout = fLast ? ( int ) nTimeOut : 1000;

      iResult = poll( pfds, iCount, tout );
      zh_fsSetIOError( iResult >= 0, 0 );
      if( iResult == -1 && zh_fsOsError() == ( ZH_ERRCODE ) EINTR )
      {
         iResult = 0;
         fLast = ZH_FALSE;
      }
      if( iResult == 0 && ! fLast && zh_vmRequestQuery() == 0 &&
          ( nTimeOut = zh_timerTest( nTimeOut, &timer ) ) != 0 )
         continue;

      break;
   }

   if( ! s_fSamePoll )
   {
      for( i = 0; i < iCount; ++i )
      {
         pPollSet[ i ].revents = ( ( pfds[ i ].revents & POLLIN   ) ? ZH_POLLIN   : 0 ) |
                                 ( ( pfds[ i ].revents & POLLPRI  ) ? ZH_POLLPRI  : 0 ) |
                                 ( ( pfds[ i ].revents & POLLOUT  ) ? ZH_POLLOUT  : 0 ) |
                                 ( ( pfds[ i ].revents & POLLERR  ) ? ZH_POLLERR  : 0 ) |
                                 ( ( pfds[ i ].revents & POLLHUP  ) ? ZH_POLLHUP  : 0 ) |
                                 ( ( pfds[ i ].revents & POLLNVAL ) ? ZH_POLLNVAL : 0 );
      }
   }

   if( pFree )
      zh_xfree( pFree );
}
#elif 1  /* ! ZH_HAS_POLL */
{
#  if ! defined( ZH_HAS_SELECT_TIMER )
   ZH_MAXUINT timer = zh_timerInit( nTimeOut );
#  endif
   fd_set rfds, wfds, efds;
   int i;

   for( ;; )
   {
      struct timeval tv;
      int iMaxFD = 0;
      ZH_BOOL fLast = nTimeOut >= 0 && nTimeOut <= 1000;

      if( fLast )
      {
         tv.tv_sec = ( long ) ( nTimeOut / 1000 );
         tv.tv_usec = ( long ) ( nTimeOut % 1000 ) * 1000;
      }
      else
      {
         tv.tv_sec = 1;
         tv.tv_usec = 0;
      }

      FD_ZERO( &rfds );
      FD_ZERO( &wfds );
      FD_ZERO( &efds );

      for( i = 0; i < iCount; ++i )
      {
         PZH_POLLFD pSet = pPollSet + i;
         if( pSet->fd >= 0 &&
             ( pSet->events & ( ZH_POLLIN | ZH_POLLOUT | ZH_POLLPRI ) ) )
         {
            if( pSet->events & ZH_POLLIN )
               FD_SET( pSet->fd, &rfds );
            if( pSet->events & ZH_POLLOUT )
               FD_SET( pSet->fd, &wfds );
            if( pSet->events & ZH_POLLPRI )
               FD_SET( pSet->fd, &efds );
            if( pSet->fd > iMaxFD )
               iMaxFD = pSet->fd;
         }
      }

      iResult = select( iMaxFD + 1, &rfds, &wfds, &efds, &tv );
      zh_fsSetIOError( iResult >= 0, 0 );

      if( iResult == -1 && zh_fsOsError() == ( ZH_ERRCODE ) EINTR )
      {
         iResult = 0;
         fLast = ZH_FALSE;
#  if defined( ZH_HAS_SELECT_TIMER )
         if( nTimeOut > 0 )
            nTimeOut += tv.tv_sec * 1000 + tv.tv_usec / 1000;
#  endif
      }
#  if defined( ZH_HAS_SELECT_TIMER )
      if( iResult == 0 && nTimeOut > 0 )
      {
         if( ( nTimeOut -= 1000 ) < 0 )
            break;
      }

      if( iResult != 0 || fLast || nTimeOut == 0 || zh_vmRequestQuery() != 0 )
         break;
#  else
      if( iResult != 0 || fLast || ( nTimeOut = zh_timerTest( nTimeOut, &timer ) ) == 0 ||
          zh_vmRequestQuery() != 0 )
         break;
#  endif
   }
   if( iResult > 0 )
   {
      iResult = 0;
      for( i = 0; i < iCount; ++i )
      {
         PZH_POLLFD pSet = pPollSet + i;
         pSet->revents = 0;
         if( pSet->fd >= 0 )
         {
            if( FD_ISSET( pSet->fd, &rfds ) )
               pSet->revents |= ZH_POLLIN;
            if( FD_ISSET( pSet->fd, &wfds ) )
               pSet->revents |= ZH_POLLOUT;
            if( FD_ISSET( pSet->fd, &efds ) )
               pSet->revents |= ZH_POLLPRI;
            if( pSet->revents != 0 )
               ++iResult;
         }
      }
   }
}
#else
{
   int iTODO; /* TODO: for given platform */

   ZH_SYMBOL_UNUSED( pPollSet );
   ZH_SYMBOL_UNUSED( iCount );
   ZH_SYMBOL_UNUSED( nTimeOut );
   iResult = -1;
}
#endif /* ! ZH_HAS_POLL */

   zh_vmLock();

   return iResult;
}
#endif /* ZH_OS_UNIX */

ZH_FHANDLE zh_fsPOpen( const char * pszFileName, const char * pszMode )
{
   ZH_FHANDLE hFileHandle = FS_ERROR;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsPOpen(%p, %s)", ( const void * ) pszFileName, pszMode ) );

#if defined( ZH_OS_UNIX )
   {
      ZH_FHANDLE hPipeHandle[ 2 ];
      pid_t pid;
      char * pszTmp;
      ZH_BOOL fRead;
      ZH_SIZE nLen;

      nLen = strlen( pszFileName );
      if( pszMode && ( *pszMode == 'r' || *pszMode == 'w' ) )
         fRead = ( *pszMode == 'r' );
      else
      {
         if( pszFileName[ 0 ] == '|' )
            fRead = ZH_FALSE;
         else if( pszFileName[ nLen - 1 ] == '|' )
            fRead = ZH_TRUE;
         else
            fRead = ZH_FALSE;
      }

      if( pszFileName[ 0 ] == '|' )
      {
         ++pszFileName;
         --nLen;
      }
      if( pszFileName[ nLen - 1 ] == '|' )
      {
         pszTmp = zh_strdup( pszFileName );
         pszTmp[ --nLen ] = 0;
         pszFileName        = pszTmp;
      }
      else
         pszTmp = NULL;

      zh_vmUnlock();
      if( pipe( hPipeHandle ) == 0 )
      {
         if( ( pid = fork() ) != -1 )
         {
            if( pid != 0 )
            {
               int iResult, iStatus = 0;

               ZH_FAILURE_RETRY( iResult, waitpid( pid, &iStatus, 0 ) );

               iResult = iResult == pid &&
                         WIFEXITED( iStatus ) &&
                         WEXITSTATUS( iStatus ) == 0 ? 0 : -1;

               if( iResult != 0 )
               {
                  zh_fsClose( hPipeHandle[ 0 ] );
                  zh_fsClose( hPipeHandle[ 1 ] );
               }
               else if( fRead )
               {
                  zh_fsClose( hPipeHandle[ 1 ] );
                  hFileHandle = hPipeHandle[ 0 ];
               }
               else
               {
                  zh_fsClose( hPipeHandle[ 0 ] );
                  hFileHandle = hPipeHandle[ 1 ];
               }
            }
            else
            {
               ZH_FHANDLE hNullHandle;
               int iMaxFD, iResult;

               ZH_FAILURE_RETRY( hNullHandle, open( "/dev/null", O_RDWR ) );
               if( fRead )
               {
                  zh_fsClose( hPipeHandle[ 0 ] );
                  ZH_FAILURE_RETRY( iResult, dup2( hPipeHandle[ 1 ], 1 ) );
                  ZH_FAILURE_RETRY( iResult, dup2( hNullHandle, 0 ) );
                  ZH_FAILURE_RETRY( iResult, dup2( hNullHandle, 2 ) );
               }
               else
               {
                  zh_fsClose( hPipeHandle[ 1 ] );
                  ZH_FAILURE_RETRY( iResult, dup2( hPipeHandle[ 0 ], 0 ) );
                  ZH_FAILURE_RETRY( iResult, dup2( hNullHandle, 1 ) );
                  ZH_FAILURE_RETRY( iResult, dup2( hNullHandle, 2 ) );
               }
               iMaxFD = sysconf( _SC_OPEN_MAX );
               if( iMaxFD < 3 )
                  iMaxFD = 1024;
               for( hNullHandle = 3; hNullHandle < iMaxFD; ++hNullHandle )
                  zh_fsClose( hNullHandle );

               pid = fork();
               if( pid == 0 )
               {
                  const char * argv[ 4 ];

                  argv[ 0 ] = "sh";
                  argv[ 1 ] = "-c";
                  argv[ 2 ] = pszFileName;
                  argv[ 3 ] = 0;

                  if( setuid( getuid() ) == -1 ) {}
                  if( setgid( getgid() ) == -1 ) {}
                  ZH_FAILURE_RETRY( iResult, execv( "/bin/sh", ( char ** ) ZH_UNCONST( argv ) ) );
               }
               exit( pid > 0 ? 0 : 1 );
            }
         }
         else
         {
            zh_fsSetIOError( hFileHandle != FS_ERROR, 0 );
            zh_fsCloseRaw( hPipeHandle[ 0 ] );
            zh_fsCloseRaw( hPipeHandle[ 1 ] );
         }
      }
      else
         zh_fsSetIOError( hFileHandle != FS_ERROR, 0 );
      zh_vmLock();

      if( pszTmp )
         zh_xfree( pszTmp );
   }
#else

   ZH_SYMBOL_UNUSED( pszFileName );
   ZH_SYMBOL_UNUSED( pszMode );

   zh_fsSetError( ( ZH_ERRCODE ) FS_ERROR );

#endif

   return hFileHandle;
}


ZH_BOOL zh_fsPipeCreate( ZH_FHANDLE hPipe[ 2 ] )
{
   ZH_BOOL fResult;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsPipeCreate(%p)", ( void * ) hPipe ) );

#if defined( ZH_OS_WIN )
{
   SECURITY_ATTRIBUTES sa;
   HANDLE hPipeRd, hPipeWr;

   memset( &sa, 0, sizeof( sa ) );
   sa.nLength = sizeof( sa );
   sa.bInheritHandle = TRUE;

   fResult = CreatePipe( &hPipeRd, &hPipeWr, &sa, 0 ) != 0;
   if( fResult )
   {
      hPipe[ 0 ] = ( ZH_FHANDLE ) hPipeRd;
      hPipe[ 1 ] = ( ZH_FHANDLE ) hPipeWr;
   }
   else
      hPipe[ 0 ] = hPipe[ 1 ] = FS_ERROR;
   zh_fsSetIOError( fResult, 0 );
}
#elif defined( ZH_OS_UNIX )
{
   fResult = pipe( hPipe ) == 0;
   if( ! fResult )
      hPipe[ 0 ] = hPipe[ 1 ] = FS_ERROR;
   zh_fsSetIOError( fResult, 0 );
}
#else
{
      int iTODO; /* TODO: for given platform */

   hPipe[ 0 ] = hPipe[ 1 ] = FS_ERROR;
   zh_fsSetError( ( ZH_ERRCODE ) FS_ERROR );
   fResult = ZH_FALSE;
}
#endif

   return fResult;
}

int zh_fsIsPipeOrSock( ZH_FHANDLE hPipeHandle )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsIsPipeOrSock(%p)", ( void * ) ( ZH_PTRUINT ) hPipeHandle ) );

#if defined( ZH_OS_UNIX )
{
#  if defined( ZH_USE_LARGEFILE64 )
   struct stat64 statbuf;
   int ret = fstat64( hPipeHandle, &statbuf );
#  else
   struct stat statbuf;
   int ret = fstat( hPipeHandle, &statbuf );
#  endif
   zh_fsSetIOError( ret == 0, 0 );
   return ret == 0 &&
          ( S_ISFIFO( statbuf.st_mode ) || S_ISSOCK( statbuf.st_mode ) ) ? 1 : 0;
}
#elif defined( ZH_OS_WIN )
{
   DWORD type = GetFileType( ( HANDLE ) zh_fsGetOsHandle( hPipeHandle ) );
   zh_fsSetIOError( type != FILE_TYPE_UNKNOWN || GetLastError() == NO_ERROR, 0 );
   return type == FILE_TYPE_PIPE ? 1 : 0;
}
#else
   int iTODO; /* TODO: for given platform */
   ZH_SYMBOL_UNUSED( hPipeHandle );
   zh_fsSetError( ( ZH_ERRCODE ) FS_ERROR );
   return 0;
#endif
}

ZH_BOOL zh_fsPipeUnblock( ZH_FHANDLE hPipeHandle )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsPipeUnblock(%p)", ( void * ) ( ZH_PTRUINT ) hPipeHandle ) );

#if defined( ZH_OS_WIN )
   {
      DWORD dwMode = PIPE_NOWAIT;
      ZH_BOOL fResult;

      fResult = SetNamedPipeHandleState( ( HANDLE ) zh_fsGetOsHandle( hPipeHandle ),
                                         &dwMode, NULL, NULL ) != 0;
      zh_fsSetIOError( fResult, 0 );
      return fResult;
   }
#elif defined( ZH_OS_UNIX )
   {
      int ret = fcntl( hPipeHandle, F_GETFL, 0 );

      if( ret != -1 && ( ret & O_NONBLOCK ) == 0 )
         ret = fcntl( hPipeHandle, F_SETFL, ret | O_NONBLOCK );
      zh_fsSetIOError( ret != -1, 0 );

      return ret != -1;
   }
#else
   {
      int iTODO; /* TODO: for given platform */
      ZH_SYMBOL_UNUSED( hPipeHandle );
      zh_fsSetError( ( ZH_ERRCODE ) FS_ERROR );
      return ZH_FALSE;
   }
#endif
}

ZH_SIZE zh_fsPipeIsData( ZH_FHANDLE hPipeHandle, ZH_SIZE nBufferSize,
                         ZH_MAXINT nTimeOut )
{
   ZH_SIZE nToRead = 0;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsPipeIsData(%p,%" ZH_PFS "u,%" PFHL "d)", ( void * ) ( ZH_PTRUINT ) hPipeHandle, nBufferSize, nTimeOut ) );

   zh_vmUnlock();

#if defined( ZH_OS_WIN )
{
   ZH_MAXUINT timer = zh_timerInit( nTimeOut );
   ZH_BOOL fResult = ZH_FALSE;
   DWORD dwAvail;

   do
   {
      if( fResult )
         zh_releaseCPU();

      dwAvail = 0;
      fResult = PeekNamedPipe( ( HANDLE ) zh_fsGetOsHandle( hPipeHandle ),
                               NULL, 0, NULL, &dwAvail, NULL ) != 0;
      if( ! fResult && GetLastError() == ERROR_BROKEN_PIPE )
      {
         zh_fsSetError( 0 );
         break;
      }
      zh_fsSetIOError( fResult, 0 );
   }
   while( fResult && dwAvail == 0 &&
          ( nTimeOut = zh_timerTest( nTimeOut, &timer ) ) != 0 &&
          zh_vmRequestQuery() == 0 );

   if( ! fResult )
      nToRead = ( ZH_SIZE ) FS_ERROR;
   else if( dwAvail > 0 )
      nToRead = ( ( ZH_SIZE ) dwAvail < nBufferSize ) ? dwAvail : nBufferSize;
}
#elif defined( ZH_OS_UNIX )
{
   int iResult = zh_fsCanRead( hPipeHandle, nTimeOut );

   if( iResult > 0 )
      nToRead = nBufferSize;
   else if( iResult < 0 )
      nToRead = ( ZH_SIZE ) FS_ERROR;
}
#else
{
      int iTODO; /* TODO: for given platform */
   ZH_SYMBOL_UNUSED( hPipeHandle );
   ZH_SYMBOL_UNUSED( nBufferSize );
   ZH_SYMBOL_UNUSED( nTimeOut );
   zh_fsSetError( ( ZH_ERRCODE ) FS_ERROR );
}
#endif

   zh_vmLock();

   return nToRead;
}

ZH_SIZE zh_fsPipeRead( ZH_FHANDLE hPipeHandle, void * buffer, ZH_SIZE nSize,
                       ZH_MAXINT nTimeOut )
{
   ZH_SIZE nRead;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsPipeRead(%p,%p,%" ZH_PFS "u,%" PFHL "d)", ( void * ) ( ZH_PTRUINT ) hPipeHandle, buffer, nSize, nTimeOut ) );

   nRead = zh_fsPipeIsData( hPipeHandle, nSize, nTimeOut );
   if( nRead != ( ZH_SIZE ) FS_ERROR && nRead > 0 )
   {
      nRead = zh_fsReadLarge( hPipeHandle, buffer, nRead );
      if( nRead == 0 )
         nRead = ( ZH_SIZE ) FS_ERROR;
   }

   return nRead;
}

ZH_SIZE zh_fsPipeWrite( ZH_FHANDLE hPipeHandle, const void * buffer, ZH_SIZE nSize,
                        ZH_MAXINT nTimeOut )
{
   ZH_SIZE nWritten;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsPipeWrite(%p,%p,%" ZH_PFS "u,%" PFHL "d)", ( void * ) ( ZH_PTRUINT ) hPipeHandle, buffer, nSize, nTimeOut ) );

   zh_vmUnlock();

#if defined( ZH_OS_WIN )
{
   HANDLE hPipe = ( HANDLE ) zh_fsGetOsHandle( hPipeHandle );
   DWORD dwMode = 0;

   if( GetNamedPipeHandleState( hPipe, &dwMode, NULL, NULL, NULL, NULL, 0 ) )
   {
      ZH_MAXUINT timer = zh_timerInit( nTimeOut );
      ZH_BOOL fResult = ZH_FALSE;

      if( ( dwMode & PIPE_NOWAIT ) == 0 )
      {
         DWORD dwNewMode = dwMode | PIPE_NOWAIT;
         SetNamedPipeHandleState( hPipe, &dwNewMode, NULL, NULL );
      }

      nWritten = 0;
      do
      {
         DWORD dwWritten, dwToWrite;

         if( fResult )
            zh_releaseCPU();

         dwToWrite = ( DWORD ) ( nSize - nWritten );
         /* real life tests show that MSDN is wrong and MS-Windows
            refuse to accept even single byte if data is longer then
            size of PIPE buffer in unblocking mode [druzus] */
         if( dwToWrite > 4096 )
            dwToWrite = 4096;
         fResult = WriteFile( hPipe, ( const ZH_BYTE * ) buffer + nWritten, dwToWrite, &dwWritten, NULL ) != 0;
         if( fResult )
            nWritten += ( ZH_SIZE ) dwWritten;
         else if( nWritten == 0 )
            nWritten = ( ZH_SIZE ) FS_ERROR;
         zh_fsSetIOError( fResult, 0 );
      }
      while( fResult && nWritten < nSize &&
             ( nTimeOut = zh_timerTest( nTimeOut, &timer ) ) != 0 &&
             zh_vmRequestQuery() == 0 );

      if( ( dwMode & PIPE_NOWAIT ) == 0 )
         SetNamedPipeHandleState( hPipe, &dwMode, NULL, NULL );
   }
   else
   {
      zh_fsSetIOError( ZH_FALSE, 0 );
      nWritten = ( ZH_SIZE ) FS_ERROR;
   }
}
#elif defined( ZH_OS_UNIX )
{
   int iResult = zh_fsCanWrite( hPipeHandle, nTimeOut );

   if( iResult > 0 )
   {
      int iFlags = -1;

      iResult = fcntl( hPipeHandle, F_GETFL, 0 );
      if( iResult != -1 && ( iResult & O_NONBLOCK ) == 0 )
      {
         iFlags = iResult;
         iResult = fcntl( hPipeHandle, F_SETFL, iResult | O_NONBLOCK );
      }
      if( iResult == -1 )
      {
         zh_fsSetIOError( ZH_FALSE, 0 );
         nWritten = ( ZH_SIZE ) FS_ERROR;
      }
      else
         nWritten = zh_fsWriteLarge( hPipeHandle, buffer, nSize );
      if( iFlags != -1 )
         fcntl( hPipeHandle, F_SETFL, iFlags );
   }
   else
      nWritten = ( ZH_SIZE ) iResult;
}
#else
{
      int iTODO; /* TODO: for given platform */
   ZH_SYMBOL_UNUSED( nTimeOut );
   nWritten = zh_fsWriteLarge( hPipeHandle, buffer, nSize );
}
#endif

   zh_vmLock();

   return nWritten;
}

ZH_FHANDLE zh_fsCreate( const char * pszFileName, ZH_FATTR nAttr )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsCreate(%s, %u)", pszFileName, nAttr ) );

   return zh_fsOpenEx( pszFileName, FO_READWRITE | FO_CREAT | FO_TRUNC | FO_EXCLUSIVE, nAttr );
}

ZH_FHANDLE zh_fsCreateEx( const char * pszFileName, ZH_FATTR nAttr, ZH_USHORT uiFlags )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsCreateEx(%s, %u, %hu)", pszFileName, nAttr, uiFlags ) );

   uiFlags &= ~( FO_READ | FO_WRITE | FO_READWRITE );

   return zh_fsOpenEx( pszFileName, FO_READWRITE | FO_CREAT | FO_TRUNC | uiFlags, nAttr );
}

ZH_FHANDLE zh_fsOpen( const char * pszFileName, ZH_USHORT uiFlags )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsOpen(%s, %hu)", pszFileName, uiFlags ) );

   return zh_fsOpenEx( pszFileName, uiFlags, FC_NORMAL );
}

ZH_FHANDLE zh_fsOpenEx( const char * pszFileName, ZH_USHORT uiFlags, ZH_FATTR nAttr )
{
   ZH_FHANDLE hFileHandle;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsOpenEx(%s, %hu, %u)", pszFileName, uiFlags, nAttr ) );

#if defined( ZH_OS_WIN )
   {
      LPCTSTR lpFileName;
      LPTSTR lpFree;
      DWORD dwMode, dwShare, dwCreat, dwAttr;
      HANDLE hFile;

      lpFileName = ZH_FSNAMECONV( pszFileName, &lpFree );

      convert_open_flags( ZH_FALSE, nAttr, uiFlags, &dwMode, &dwShare, &dwCreat, &dwAttr );

      zh_vmUnlock();
      hFile = CreateFile( lpFileName, dwMode, dwShare, NULL, dwCreat, dwAttr, NULL );
      zh_fsSetIOError( hFile != ( HANDLE ) INVALID_HANDLE_VALUE, 0 );
      zh_vmLock();

      if( lpFree )
         zh_xfree( lpFree );

      hFileHandle = ( ZH_FHANDLE ) hFile;
   }
#else
   {
      char * pszFree;
      int flags, share, attr;
      unsigned mode;

      pszFileName = zh_fsNameConv( pszFileName, &pszFree );

      convert_open_flags( ZH_FALSE, nAttr, uiFlags, &flags, &mode, &share, &attr );

      zh_vmUnlock();


#if defined( _MSC_VER )
      if( share )
         hFileHandle = _sopen( pszFileName, flags, share, mode );
      else
         hFileHandle = _open( pszFileName, flags, mode );
      zh_fsSetIOError( hFileHandle != FS_ERROR, 0 );
#elif defined( ZH_FS_SOPEN )
      if( share )
         hFileHandle = sopen( pszFileName, flags, share, mode );
      else
         hFileHandle = open( pszFileName, flags, mode );
      zh_fsSetIOError( hFileHandle != FS_ERROR, 0 );
#else
      ZH_FAILURE_RETRY( hFileHandle, open( pszFileName, flags | share, mode ) );
#endif

      zh_vmLock();

      if( pszFree )
         zh_xfree( pszFree );
   }
#endif

   return hFileHandle;
}

void zh_fsCloseRaw( ZH_FHANDLE hFileHandle )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsCloseRaw(%p)", ( void * ) ( ZH_PTRUINT ) hFileHandle ) );

   zh_vmUnlock();
#if defined( ZH_OS_WIN )
   CloseHandle( DosToWinHandle( hFileHandle ) );
#else
   {
#  if defined( EINTR )
      int ret;
      /* ignoring EINTR in close() it's quite common bug when sockets or
       * pipes are used. Without such protection it's not safe to use
       * signals in user code.
       */
      do
      {
         ret = close( hFileHandle );
      }
      while( ret == -1 && errno == EINTR );
#  else
      close( hFileHandle );
#  endif
   }
#endif
   zh_vmLock();
}

void zh_fsClose( ZH_FHANDLE hFileHandle )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsClose(%p)", ( void * ) ( ZH_PTRUINT ) hFileHandle ) );

   zh_vmUnlock();
#if defined( ZH_OS_WIN )
   zh_fsSetIOError( CloseHandle( DosToWinHandle( hFileHandle ) ) != 0, 0 );
#elif defined( ZH_OS_OS2 )
   {
      APIRET ret = DosClose( hFileHandle );

      zh_fsSetError( ( ZH_ERRCODE ) ret );
   }
#else
   {
      int ret;
#  if defined( EINTR )
      /* ignoring EINTR in close() it's quite common bug when sockets or
       * pipes are used. Without such protection it's not safe to use
       * signals in user code.
       */
      do
      {
         ret = close( hFileHandle );
      }
      while( ret == -1 && errno == EINTR );
#  else
      ret = close( hFileHandle );
#  endif
      zh_fsSetIOError( ret == 0, 0 );
   }
#endif
   zh_vmLock();
}


#define FD_TEST  0

int zh_fsSetDevMode( ZH_FHANDLE hFileHandle, int iDevMode )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsSetDevMode(%p, %d)", ( void * ) ( ZH_PTRUINT ) hFileHandle, iDevMode ) );


   ZH_SYMBOL_UNUSED( hFileHandle );

   zh_fsSetError( ( ZH_ERRCODE ) ( iDevMode == FD_TEXT ? FS_ERROR : 0 ) );
   return FD_BINARY;

}

ZH_BOOL zh_fsGetFileTime( const char * pszFileName, long * plJulian, long * plMillisec )
{
   ZH_BOOL fResult;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsGetFileTime(%s, %p, %p)", pszFileName, ( void * ) plJulian, ( void * ) plMillisec ) );

   fResult = ZH_FALSE;
   *plJulian = *plMillisec = 0;

   zh_vmUnlock();

#if defined( ZH_OS_WIN )
   {
      typedef BOOL ( WINAPI * _ZH_GETFILEATTRIBUTESEX )( LPCTSTR, GET_FILEEX_INFO_LEVELS, LPVOID );
      static _ZH_GETFILEATTRIBUTESEX s_pGetFileAttributesEx = ( _ZH_GETFILEATTRIBUTESEX ) -1;

      if( s_pGetFileAttributesEx == ( _ZH_GETFILEATTRIBUTESEX ) -1 )
      {
         HMODULE hModule = GetModuleHandle( TEXT( "kernel32.dll" ) );
         if( hModule )
            s_pGetFileAttributesEx = ( _ZH_GETFILEATTRIBUTESEX )
               ZH_WINAPI_GETPROCADDRESST( hModule, "GetFileAttributesEx" );
         else
            s_pGetFileAttributesEx = NULL;
      }

      if( s_pGetFileAttributesEx )
      {
         LPCTSTR lpFileName;
         LPTSTR lpFree;
         WIN32_FILE_ATTRIBUTE_DATA attrex;

         lpFileName = ZH_FSNAMECONV( pszFileName, &lpFree );

         memset( &attrex, 0, sizeof( attrex ) );

         if( s_pGetFileAttributesEx( lpFileName, GetFileExInfoStandard, &attrex ) )
         {
            FILETIME local_ft;
            SYSTEMTIME st;

            if( FileTimeToLocalFileTime( &attrex.ftLastWriteTime, &local_ft ) &&
                FileTimeToSystemTime( &local_ft, &st ) )
            {
               *plJulian = zh_dateEncode( st.wYear, st.wMonth, st.wDay );
               *plMillisec = zh_timeEncode( st.wHour, st.wMinute, st.wSecond, st.wMilliseconds );

               fResult = ZH_TRUE;
            }
         }
         zh_fsSetIOError( fResult, 0 );

         if( lpFree )
            zh_xfree( lpFree );
      }
      else
      {
         ZH_FHANDLE hFile = zh_fsOpen( pszFileName, FO_READ | FO_SHARED );
         FILETIME ft, local_ft;
         SYSTEMTIME st;

         if( hFile != FS_ERROR )
         {
            if( GetFileTime( DosToWinHandle( hFile ), NULL, NULL, &ft ) &&
                FileTimeToLocalFileTime( &ft, &local_ft ) &&
                FileTimeToSystemTime( &local_ft, &st ) )
            {
               *plJulian = zh_dateEncode( st.wYear, st.wMonth, st.wDay );
               *plMillisec = zh_timeEncode( st.wHour, st.wMinute, st.wSecond, st.wMilliseconds );

               fResult = ZH_TRUE;
            }
            zh_fsSetIOError( fResult, 0 );
            zh_fsClose( hFile );
         }
         else
         {
            WIN32_FIND_DATA findFileData;
            HANDLE hFindFile;
            LPCTSTR lpFileName;
            LPTSTR lpFree;

            lpFileName = ZH_FSNAMECONV( pszFileName, &lpFree );
            hFindFile = FindFirstFile( lpFileName, &findFileData );
            if( lpFree )
               zh_xfree( lpFree );

            if( hFindFile != INVALID_HANDLE_VALUE )
            {
               if( FileTimeToLocalFileTime( &findFileData.ftLastWriteTime, &local_ft ) &&
                   FileTimeToSystemTime( &local_ft, &st ) )
               {
                  *plJulian = zh_dateEncode( st.wYear, st.wMonth, st.wDay );
                  *plMillisec = zh_timeEncode( st.wHour, st.wMinute, st.wSecond, st.wMilliseconds );

                  fResult = ZH_TRUE;
               }
               zh_fsSetIOError( fResult, 0 );
               FindClose( hFindFile );
            }
         }
      }
   }
#elif defined( ZH_OS_UNIX ) || defined( __GNUC__ )
   {
      char * pszFree;
#  if defined( ZH_USE_LARGEFILE64 )
      struct stat64 statbuf;
      if( stat64( zh_fsNameConv( pszFileName, &pszFree ), &statbuf ) == 0 )
#  else
      struct stat statbuf;
      if( stat( zh_fsNameConv( pszFileName, &pszFree ), &statbuf ) == 0 )
#  endif
      {
         time_t ftime;
         struct tm ft;

         ftime = statbuf.st_mtime;
#  if defined( ZH_HAS_LOCALTIME_R )
         localtime_r( &ftime, &ft );
#  else
         ft = *localtime( &ftime );
#  endif

         *plJulian = zh_dateEncode( ft.tm_year + 1900, ft.tm_mon + 1, ft.tm_mday );
#if defined( ZH_OS_LINUX ) && ( defined( _BSD_SOURCE ) || defined( _SVID_SOURCE ) ) && \
    defined( __GLIBC__ ) && defined( __GLIBC_MINOR__ ) && \
           ( __GLIBC__ > 2 || ( __GLIBC__ == 2 && __GLIBC_MINOR__ >= 6 ) )
         *plMillisec = zh_timeEncode( ft.tm_hour, ft.tm_min, ft.tm_sec, statbuf.st_mtim.tv_nsec / 1000000 );
#else
         *plMillisec = zh_timeEncode( ft.tm_hour, ft.tm_min, ft.tm_sec, 0 );
#endif
         fResult = ZH_TRUE;
      }
      zh_fsSetIOError( fResult, 0 );

      if( pszFree )
         zh_xfree( pszFree );
   }
#else
   {
      int iTODO; /* TODO: for given platform */

      ZH_SYMBOL_UNUSED( pszFileName );
   }
#endif

   zh_vmLock();

   return fResult;
}

ZH_BOOL zh_fsGetAttr( const char * pszFileName, ZH_FATTR * pnAttr )
{
   ZH_BOOL fResult;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsGetAttr(%s, %p)", pszFileName, ( void * ) pnAttr ) );

   zh_vmUnlock();

   *pnAttr = 0;
   fResult = ZH_FALSE;
#if defined( ZH_OS_WIN )
   {
      LPCTSTR lpFileName;
      LPTSTR lpFree;
      DWORD dwAttr;

      lpFileName = ZH_FSNAMECONV( pszFileName, &lpFree );

      dwAttr = GetFileAttributes( lpFileName );

      if( dwAttr != INVALID_FILE_ATTRIBUTES )
      {
         *pnAttr = zh_fsAttrFromRaw( dwAttr );
         fResult = ZH_TRUE;
      }
      zh_fsSetIOError( fResult, 0 );

      if( lpFree )
         zh_xfree( lpFree );
   }
#else
   {
      char * pszFree;
      pszFileName = zh_fsNameConv( pszFileName, &pszFree );

#  if defined( ZH_OS_UNIX )
      {
#     if defined( ZH_USE_LARGEFILE64 )
         struct stat64 statbuf;
         if( stat64( pszFileName, &statbuf ) == 0 )
#     else
         struct stat statbuf;
         if( stat( pszFileName, &statbuf ) == 0 )
#     endif
         {
            *pnAttr = zh_fsAttrFromRaw( statbuf.st_mode );
            fResult = ZH_TRUE;
         }
         zh_fsSetIOError( fResult, 0 );
      }
#  else
      {
         int iTODO; /* TODO: for given platform */

         ZH_SYMBOL_UNUSED( pszFileName );
      }
#  endif
      if( pszFree )
         zh_xfree( pszFree );
   }
#endif

   zh_vmLock();

   return fResult;
}

ZH_BOOL zh_fsSetFileTime( const char * pszFileName, long lJulian, long lMillisec )
{
   ZH_BOOL fResult;
   int iYear, iMonth, iDay;
   int iHour, iMinute, iSecond, iMSec;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsSetFileTime(%s, %ld, %ld)", pszFileName, lJulian, lMillisec ) );

   zh_dateDecode( lJulian, &iYear, &iMonth, &iDay );
   zh_timeDecode( lMillisec, &iHour, &iMinute, &iSecond, &iMSec );

   zh_vmUnlock();

#if defined( ZH_OS_WIN )
   {
      LPCTSTR lpFileName;
      LPTSTR lpFree;
      HANDLE hFile;

      lpFileName = ZH_FSNAMECONV( pszFileName, &lpFree );

      zh_vmUnlock();
      hFile = CreateFile( lpFileName, GENERIC_WRITE, FILE_SHARE_WRITE | FILE_SHARE_READ,
                          NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL );
      fResult = hFile != ( HANDLE ) INVALID_HANDLE_VALUE;
      zh_fsSetIOError( fResult, 0 );
      zh_vmLock();

      if( fResult )
      {
         FILETIME local_ft;
         SYSTEMTIME st;

         if( lJulian <= 0 || lMillisec < 0 )
            GetLocalTime( &st );
         else
            memset( &st, 0, sizeof( st ) );

         if( lJulian > 0 )
         {
            st.wYear = ( WORD ) iYear;
            st.wMonth = ( WORD ) iMonth;
            st.wDay = ( WORD ) iDay;
         }
         if( lMillisec >= 0 )
         {
            st.wHour = ( WORD ) iHour;
            st.wMinute = ( WORD ) iMinute;
            st.wSecond = ( WORD ) iSecond;
            st.wMilliseconds = ( WORD ) iMSec;
         }

         if( SystemTimeToFileTime( &st, &local_ft ) )
         {
            FILETIME ft;
            LocalFileTimeToFileTime( &local_ft, &ft );
            fResult = SetFileTime( hFile, NULL, &ft, &ft ) != 0;
         }
         else
            fResult = ZH_FALSE;

         zh_fsSetIOError( fResult, 0 );
         CloseHandle( hFile );
      }

      if( lpFree )
         zh_xfree( lpFree );
   }
#elif defined( ZH_OS_UNIX )
   {
      char * pszFree;

      pszFileName = zh_fsNameConv( pszFileName, &pszFree );

      if( lJulian <= 0 && lMillisec < 0 )
      {
#  if defined( ZH_OS_LINUX ) && ! defined( __WATCOMC__ )
         fResult = utimes( pszFileName, NULL ) == 0;
#  else
         fResult = utime( pszFileName, NULL ) == 0;
#  endif
      }
      else
      {
         struct tm new_value;

         if( lJulian <= 0 || lMillisec < 0 )
         {
            time_t current_time;

            current_time = time( NULL );
#  if defined( ZH_HAS_LOCALTIME_R )
            localtime_r( &current_time, &new_value );
#  else
            new_value = *localtime( &current_time );
#  endif
         }
         else
            memset( &new_value, 0, sizeof( new_value ) );

         if( lJulian > 0 )
         {
            new_value.tm_year = iYear - 1900;
            new_value.tm_mon = iMonth - 1;
            new_value.tm_mday = iDay;
         }
         if( lMillisec >= 0 )
         {
            new_value.tm_hour = iHour;
            new_value.tm_min = iMinute;
            new_value.tm_sec = iSecond;
         }
         new_value.tm_isdst = -1;

#  if defined( ZH_OS_LINUX )
         {
            struct timeval times[ 2 ];
            times[ 0 ].tv_sec = times[ 1 ].tv_sec = mktime( &new_value );
            times[ 0 ].tv_usec = times[ 1 ].tv_usec = iMSec * 1000;
            fResult = utimes( pszFileName, times ) == 0;
         }
#  else
         {
            struct utimbuf buf;
            buf.actime = buf.modtime = mktime( &new_value );
            fResult = utime( pszFileName, &buf ) == 0;
         }
#  endif
      }
      zh_fsSetIOError( fResult, 0 );
      if( pszFree )
         zh_xfree( pszFree );
   }
#else
   {
      int iTODO; /* To force warning */

      fResult = ZH_FALSE;
      zh_fsSetError( ( ZH_ERRCODE ) FS_ERROR );
   }
#endif

   zh_vmLock();

   return fResult;
}

ZH_BOOL zh_fsSetAttr( const char * pszFileName, ZH_FATTR nAttr )
{
   ZH_BOOL fResult;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsSetAttr(%s, %u)", pszFileName, nAttr ) );

   zh_vmUnlock();

#if defined( ZH_OS_WIN )
   {
      LPCTSTR lpFileName;
      LPTSTR lpFree;
      DWORD dwFlags = 0;

      lpFileName = ZH_FSNAMECONV( pszFileName, &lpFree );

      if( nAttr & ZH_FA_READONLY )
         dwFlags |= FILE_ATTRIBUTE_READONLY;
      if( nAttr & ZH_FA_HIDDEN )
         dwFlags |= FILE_ATTRIBUTE_HIDDEN;
      if( nAttr & ZH_FA_SYSTEM )
         dwFlags |= FILE_ATTRIBUTE_SYSTEM;
      if( nAttr & ZH_FA_ARCHIVE )
         dwFlags |= FILE_ATTRIBUTE_ARCHIVE;
      if( dwFlags == 0 )
         dwFlags = FILE_ATTRIBUTE_NORMAL;
      fResult = SetFileAttributes( lpFileName, dwFlags ) != 0;
      zh_fsSetIOError( fResult, 0 );

      if( lpFree )
         zh_xfree( lpFree );
   }
#else
   {
      char * pszFree;

      pszFileName = zh_fsNameConv( pszFileName, &pszFree );


#  if defined( ZH_OS_UNIX )
      {
         int iAttr = ZH_FA_POSIX_ATTR( nAttr ), iResult;
         if( iAttr == 0 )
         {
            iAttr = S_IRUSR | S_IRGRP | S_IROTH;
            if( ! ( nAttr & ZH_FA_READONLY ) )
               iAttr |= S_IWUSR | S_IWGRP | S_IWOTH;
            if( nAttr & ZH_FA_SYSTEM )
               iAttr |= S_IXUSR | S_IXGRP | S_IXOTH;
            if( nAttr & ZH_FA_HIDDEN )
               iAttr &= S_IRUSR | S_IWUSR | S_IXUSR;
         }
         ZH_FAILURE_RETRY( iResult, chmod( pszFileName, iAttr ) );
         fResult = iResult != -1;
      }
#  else
      {
         int iTODO; /* To force warning */

         fResult = ZH_FALSE;
         zh_fsSetError( ( ZH_ERRCODE ) FS_ERROR );
      }
#  endif
      if( pszFree )
         zh_xfree( pszFree );
   }
#endif

   zh_vmLock();

   return fResult;
}

ZH_USHORT zh_fsRead( ZH_FHANDLE hFileHandle, void * pBuff, ZH_USHORT uiCount )
{
   ZH_USHORT uiRead;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsRead(%p, %p, %hu)", ( void * ) ( ZH_PTRUINT ) hFileHandle, pBuff, uiCount ) );

   zh_vmUnlock();

#if defined( ZH_OS_WIN )
   {
      DWORD dwRead;
      BOOL bResult;

      bResult = ReadFile( DosToWinHandle( hFileHandle ), pBuff, ( DWORD ) uiCount, &dwRead, NULL );
      zh_fsSetIOError( bResult != 0, 0 );

      uiRead = bResult ? ( ZH_USHORT ) dwRead : 0;
   }
#else
   {
      long lRead;
      ZH_FAILURE_RETRY( lRead, read( hFileHandle, pBuff, uiCount ) );
      uiRead = lRead == -1 ? 0 : ( ZH_USHORT ) lRead;
   }
#endif

   zh_vmLock();

   return uiRead;
}

ZH_USHORT zh_fsWrite( ZH_FHANDLE hFileHandle, const void * pBuff, ZH_USHORT uiCount )
{
   ZH_USHORT uiWritten = 0;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsWrite(%p, %p, %hu)", ( void * ) ( ZH_PTRUINT ) hFileHandle, pBuff, uiCount ) );

   zh_vmUnlock();

#if defined( ZH_OS_WIN )
   {
      BOOL bResult;

      if( uiCount )
      {
         DWORD dwWritten = 0;
         bResult = WriteFile( DosToWinHandle( hFileHandle ), pBuff, uiCount, &dwWritten, NULL );
         uiWritten = bResult ? ( ZH_USHORT ) dwWritten : 0;
      }
      else
          bResult = SetEndOfFile( DosToWinHandle( hFileHandle ) );
      zh_fsSetIOError( bResult != 0, 0 );

   }
#else
   if( uiCount )
   {
      long lWritten;
      ZH_FAILURE_RETRY( lWritten, write( hFileHandle, pBuff, uiCount ) );
      uiWritten = lWritten == -1 ? 0 : ( ZH_USHORT ) lWritten;
   }
   else
   {
      int iResult;
#  if defined( ZH_USE_LARGEFILE64 )
      ZH_FAILURE_RETRY( iResult, ftruncate64( hFileHandle, lseek64( hFileHandle, 0L, SEEK_CUR ) ) );
#  else
      ZH_FAILURE_RETRY( iResult, ftruncate( hFileHandle, lseek( hFileHandle, 0L, SEEK_CUR ) ) );
#  endif
   }
#endif

   zh_vmLock();

   return uiWritten;
}

ZH_SIZE zh_fsReadLarge( ZH_FHANDLE hFileHandle, void * pBuff, ZH_SIZE nCount )
{
   ZH_SIZE nRead;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsReadLarge(%p, %p, %" ZH_PFS "u)", ( void * ) ( ZH_PTRUINT ) hFileHandle, pBuff, nCount ) );

   zh_vmUnlock();

#if defined( ZH_OS_WIN )
   {
#  if defined( ZH_WIN_IOREAD_LIMIT )
      HANDLE hWFileHandle = DosToWinHandle( hFileHandle );
      BOOL bResult = TRUE;

      nRead = 0;

      while( nCount )
      {
         DWORD dwToRead;
         DWORD dwRead;

         /* Determine how much to read this time */
         if( nCount > ( ZH_SIZE ) ZH_WIN_IOREAD_LIMIT )
         {
            dwToRead = ZH_WIN_IOREAD_LIMIT;
            nCount -= ( ZH_SIZE ) dwToRead;
         }
         else
         {
            dwToRead = ( DWORD ) nCount;
            nCount = 0;
         }

         bResult = ReadFile( hWFileHandle, ( ZH_UCHAR * ) pBuff + nRead,
                             dwToRead, &dwRead, NULL );
         if( ! bResult )
            break;

         nRead += ( ZH_SIZE ) dwRead;

         if( dwRead != dwToRead )
            break;
      }
#  else
      DWORD dwRead;
      BOOL bResult;

      bResult = ReadFile( DosToWinHandle( hFileHandle ), pBuff, nCount, &dwRead, NULL );
      nRead = bResult ? ( ZH_SIZE ) dwRead : 0;
#  endif
      zh_fsSetIOError( bResult != 0, 0 );
   }
#elif defined( ZH_FS_IO_16BIT )
   {
      nRead = 0;

      while( nCount )
      {
         unsigned int uiToRead;
         long lRead;

         /* Determine how much to read this time */
         if( nCount > ( ZH_SIZE ) INT_MAX )
         {
            uiToRead = INT_MAX;
            nCount -= ( ZH_SIZE ) uiToRead;
         }
         else
         {
            uiToRead = ( unsigned int ) nCount;
            nCount = 0;
         }

         ZH_FAILURE_RETRY( lRead, read( hFileHandle, ( ZH_UCHAR * ) pBuff + nRead, uiToRead ) );

         if( lRead <= 0 )
            break;

         nRead += lRead;

         if( lRead != ( long ) uiToRead )
            break;
      }
   }
#else
   {
      long lRead;
      ZH_FAILURE_RETRY( lRead, read( hFileHandle, pBuff, nCount ) );
      nRead = lRead == -1 ? 0 : lRead;
   }
#endif

   zh_vmLock();

   return nRead;
}

ZH_SIZE zh_fsWriteLarge( ZH_FHANDLE hFileHandle, const void * pBuff, ZH_SIZE nCount )
{
   ZH_SIZE nWritten = 0;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsWriteLarge(%p, %p, %" ZH_PFS "u)", ( void * ) ( ZH_PTRUINT ) hFileHandle, pBuff, nCount ) );

   zh_vmUnlock();

#if defined( ZH_OS_WIN )

   if( nCount )
   {
#  if defined( ZH_WIN_IOWRITE_LIMIT )
      HANDLE hWFileHandle = DosToWinHandle( hFileHandle );
      BOOL bResult = TRUE;

      while( nCount )
      {
         DWORD dwToWrite;
         DWORD dwWritten;

         /* Determine how much to write this time */
         if( nCount > ( ZH_SIZE ) ZH_WIN_IOWRITE_LIMIT )
         {
            dwToWrite = ZH_WIN_IOWRITE_LIMIT;
            nCount -= ( ZH_SIZE ) dwToWrite;
         }
         else
         {
            dwToWrite = ( DWORD ) nCount;
            nCount = 0;
         }

         bResult = WriteFile( hWFileHandle, ( const ZH_UCHAR * ) pBuff + nWritten,
                              dwToWrite, &dwWritten, NULL );
         if( ! bResult )
            break;

         nWritten += ( ZH_SIZE ) dwWritten;

         if( dwWritten != dwToWrite )
            break;
      }
#  else
      DWORD dwWritten;
      BOOL bResult;
      bResult = WriteFile( DosToWinHandle( hFileHandle ), pBuff,
                           nCount, &dwWritten, NULL );
      if( bResult )
         nWritten = ( ZH_SIZE ) dwWritten;
#  endif
      zh_fsSetIOError( bResult != 0, 0 );
   }
   else
      zh_fsSetIOError( SetEndOfFile( DosToWinHandle( hFileHandle ) ) != 0, 0 );


#else

   if( nCount )
   {
#  if defined( ZH_FS_IO_16BIT )
      while( nCount )
      {
         unsigned int uiToWrite;
         long lWritten;

         /* Determine how much to write this time */
         if( nCount > ( ZH_SIZE ) INT_MAX )
         {
            uiToWrite = INT_MAX;
            nCount -= ( ZH_SIZE ) uiToWrite;
         }
         else
         {
            uiToWrite = ( unsigned int ) nCount;
            nCount = 0;
         }

         ZH_FAILURE_RETRY( lWritten, write( hFileHandle,
                                            ( const ZH_UCHAR * ) pBuff + nWritten,
                                            uiToWrite ) );

         if( lWritten <= 0 )
            break;

         nWritten += lWritten;

         if( lWritten != ( long ) uiToWrite )
            break;
      }
#  else
      long lWritten;
      ZH_FAILURE_RETRY( lWritten, write( hFileHandle, pBuff, nCount ) );
      nWritten = lWritten == -1 ? 0 : lWritten;
#  endif
   }
   else
   {
      int iResult;
#  if defined( ZH_USE_LARGEFILE64 )
      ZH_FAILURE_RETRY( iResult, ftruncate64( hFileHandle, lseek64( hFileHandle, 0L, SEEK_CUR ) ) );
#  else
      ZH_FAILURE_RETRY( iResult, ftruncate( hFileHandle, lseek( hFileHandle, 0L, SEEK_CUR ) ) );
#  endif
   }
#endif

   zh_vmLock();

   return nWritten;
}

ZH_SIZE zh_fsReadAt( ZH_FHANDLE hFileHandle, void * pBuff, ZH_SIZE nCount, ZH_FOFFSET nOffset )
{
   ZH_SIZE nRead;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsReadAt(%p, %p, %" ZH_PFS "u, %" PFHL "i)", ( void * ) ( ZH_PTRUINT ) hFileHandle, pBuff, nCount, nOffset ) );

   zh_vmUnlock();

#if defined( ZH_OS_UNIX )
   {
      long lRead;
#  if defined( ZH_USE_LARGEFILE64 )
      ZH_FAILURE_RETRY( lRead, pread64( hFileHandle, pBuff, nCount, nOffset ) );
#  else
      ZH_FAILURE_RETRY( lRead, pread( hFileHandle, pBuff, nCount, nOffset ) );
#  endif
      nRead = lRead == -1 ? 0 : lRead;
   }
#elif defined( ZH_OS_WIN )
#  if defined( ZH_WIN_IOREAD_LIMIT )
   {
      HANDLE hWFileHandle = DosToWinHandle( hFileHandle );
      OVERLAPPED Overlapped;
      BOOL bResult = TRUE;

      memset( &Overlapped, 0, sizeof( Overlapped ) );
      Overlapped.Offset     = ( DWORD ) ( nOffset & 0xFFFFFFFF );
      Overlapped.OffsetHigh = ( DWORD ) ( nOffset >> 32 );

      nRead = 0;
      while( nCount )
      {
         DWORD dwToRead;
         DWORD dwRead;

         if( nCount > ( ZH_SIZE ) ZH_WIN_IOREAD_LIMIT )
         {
            dwToRead = ZH_WIN_IOREAD_LIMIT;
            nCount -= ( ZH_SIZE ) dwToRead;
         }
         else
         {
            dwToRead = ( DWORD ) nCount;
            nCount = 0;
         }

         bResult = ReadFile( hWFileHandle, ( ZH_UCHAR * ) pBuff + nRead,
                             dwToRead, &dwRead, &Overlapped );

         if( ! bResult )
            break;

         nRead += ( ZH_SIZE ) dwRead;

         if( dwRead != dwToRead )
            break;
      }
      zh_fsSetIOError( bResult != 0, 0 );
   }
#  else
   if( zh_iswinnt() )
   {
      DWORD dwRead = 0;
      OVERLAPPED Overlapped;
      memset( &Overlapped, 0, sizeof( Overlapped ) );
      Overlapped.Offset     = ( DWORD ) ( nOffset & 0xFFFFFFFF );
      Overlapped.OffsetHigh = ( DWORD ) ( nOffset >> 32 );
      zh_fsSetIOError( ReadFile( DosToWinHandle( hFileHandle ),
                                 pBuff, ( DWORD ) nCount, &dwRead, &Overlapped ) != 0, 0 );
      nRead = dwRead;
   }
   else
   {
      ULONG ulOffsetLow  = ( ULONG ) ( nOffset & 0xFFFFFFFF ),
            ulOffsetHigh = ( ULONG ) ( nOffset >> 32 );
      ulOffsetLow = SetFilePointer( DosToWinHandle( hFileHandle ),
                                    ulOffsetLow, ( PLONG ) &ulOffsetHigh,
                                    SEEK_SET );
      if( ulOffsetLow == ( ULONG ) INVALID_SET_FILE_POINTER && GetLastError() != NO_ERROR )
      {
         zh_fsSetIOError( ZH_FALSE, 0 );
         nRead = 0;
      }
      else
      {
         DWORD dwRead = 0;
         zh_fsSetIOError( ReadFile( DosToWinHandle( hFileHandle ),
                                    pBuff, ( DWORD ) nCount, &dwRead, NULL ) != 0, 0 );
         nRead = dwRead;
      }
   }
#  endif /* ZH_WIN_IOREAD_LIMIT */


#elif defined( ZH_FS_IO_16BIT )
   if( zh_fsSeekLarge( hFileHandle, nOffset, FS_SET ) == nOffset )
      nRead = zh_fsReadLarge( hFileHandle, pBuff, nCount );
   else
      nRead = 0;
#else
   {
#  if defined( ZH_USE_LARGEFILE64 )
      ZH_FOFFSET nPos = lseek64( hFileHandle, nOffset, SEEK_SET );
#  else
      ZH_FOFFSET nPos = lseek( hFileHandle, nOffset, SEEK_SET );
#  endif
      if( nPos == ( ZH_FOFFSET ) -1 )
      {
         zh_fsSetIOError( ZH_FALSE, 0 );
         nRead = 0;
      }
      else
      {
         long lRead;
         ZH_FAILURE_RETRY( lRead, read( hFileHandle, pBuff, nCount ) );
         nRead = lRead == -1 ? 0 : lRead;
      }
   }
#endif

   zh_vmLock();

   return nRead;
}

ZH_SIZE zh_fsWriteAt( ZH_FHANDLE hFileHandle, const void * pBuff, ZH_SIZE nCount, ZH_FOFFSET nOffset )
{
   ZH_SIZE nWritten;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsWriteAt(%p, %p, %" ZH_PFS "u, %" PFHL "i)", ( void * ) ( ZH_PTRUINT ) hFileHandle, pBuff, nCount, nOffset ) );

   zh_vmUnlock();

#if defined( ZH_OS_UNIX )
   {
      long lWritten;
#  if defined( ZH_USE_LARGEFILE64 )
      ZH_FAILURE_RETRY( lWritten, pwrite64( hFileHandle, pBuff, nCount, nOffset ) );
#  else
      ZH_FAILURE_RETRY( lWritten, pwrite( hFileHandle, pBuff, nCount, nOffset ) );
#  endif
      nWritten = lWritten == -1 ? 0 : lWritten;
   }
#elif defined( ZH_OS_WIN )
#  if defined( ZH_WIN_IOWRITE_LIMIT )
   {
      HANDLE hWFileHandle = DosToWinHandle( hFileHandle );
      OVERLAPPED Overlapped;
      BOOL bResult = TRUE;

      memset( &Overlapped, 0, sizeof( Overlapped ) );
      Overlapped.Offset     = ( DWORD ) ( nOffset & 0xFFFFFFFF );
      Overlapped.OffsetHigh = ( DWORD ) ( nOffset >> 32 );

      nWritten = 0;
      while( nCount )
      {
         DWORD dwToWrite;
         DWORD dwWritten;

         if( nCount > ( ZH_SIZE ) ZH_WIN_IOWRITE_LIMIT )
         {
            dwToWrite = ZH_WIN_IOWRITE_LIMIT;
            nCount -= ( ZH_SIZE ) dwToWrite;
         }
         else
         {
            dwToWrite = ( DWORD ) nCount;
            nCount = 0;
         }

         bResult = WriteFile( hWFileHandle, ( const ZH_UCHAR * ) pBuff + nWritten,
                              dwToWrite, &dwWritten, &Overlapped );

         if( ! bResult )
            break;

         nWritten += ( ZH_SIZE ) dwWritten;

         if( dwWritten != dwToWrite )
            break;
      }
      zh_fsSetIOError( bResult != 0, 0 );
   }
#  else
   if( zh_iswinnt() )
   {
      DWORD dwWritten = 0;
      OVERLAPPED Overlapped;
      memset( &Overlapped, 0, sizeof( Overlapped ) );
      Overlapped.Offset     = ( DWORD ) ( nOffset & 0xFFFFFFFF );
      Overlapped.OffsetHigh = ( DWORD ) ( nOffset >> 32 );
      zh_fsSetIOError( WriteFile( DosToWinHandle( hFileHandle ),
                                  pBuff, ( DWORD ) nCount, &dwWritten, &Overlapped ) != 0, 0 );
      nWritten = dwWritten;
   }
   else
   {
      ULONG ulOffsetLow  = ( ULONG ) ( nOffset & 0xFFFFFFFF ),
            ulOffsetHigh = ( ULONG ) ( nOffset >> 32 );
      ulOffsetLow = SetFilePointer( DosToWinHandle( hFileHandle ),
                                    ulOffsetLow, ( PLONG ) &ulOffsetHigh,
                                    SEEK_SET );
      if( ulOffsetLow == ( ULONG ) INVALID_SET_FILE_POINTER && GetLastError() != NO_ERROR )
      {
         zh_fsSetIOError( ZH_FALSE, 0 );
         nWritten = 0;
      }
      else
      {
         DWORD dwWritten = 0;
         zh_fsSetIOError( WriteFile( DosToWinHandle( hFileHandle ),
                                     pBuff, ( DWORD ) nCount, &dwWritten, NULL ) != 0, 0 );
         nWritten = dwWritten;
      }
   }
#  endif /* ZH_WIN_IOWRITE_LIMIT */


#elif defined( ZH_FS_IO_16BIT )
   if( zh_fsSeekLarge( hFileHandle, nOffset, FS_SET ) == nOffset )
      nWritten = zh_fsWriteLarge( hFileHandle, pBuff, nCount );
   else
      nWritten = 0;
#else
   {
#  if defined( ZH_USE_LARGEFILE64 )
      ZH_FOFFSET nPos = lseek64( hFileHandle, nOffset, SEEK_SET );
#  else
      ZH_FOFFSET nPos = lseek( hFileHandle, nOffset, SEEK_SET );
#  endif
      if( nPos == ( ZH_FOFFSET ) -1 )
      {
         zh_fsSetIOError( ZH_FALSE, 0 );
         nWritten = 0;
      }
      else
      {
         long lWritten;
         ZH_FAILURE_RETRY( lWritten, write( hFileHandle, pBuff, nCount ) );
         nWritten = lWritten == -1 ? 0 : lWritten;
      }
   }
#endif

   zh_vmLock();

   return nWritten;
}

ZH_BOOL zh_fsTruncAt( ZH_FHANDLE hFileHandle, ZH_FOFFSET nOffset )
{
   ZH_BOOL fResult;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsTruncAt(%p, %" PFHL "i)", ( void * ) ( ZH_PTRUINT ) hFileHandle, nOffset ) );

   zh_vmUnlock();

#if defined( ZH_OS_WIN )
   {
      ULONG ulOffsetLow  = ( ULONG ) ( nOffset & 0xFFFFFFFF ),
            ulOffsetHigh = ( ULONG ) ( nOffset >> 32 );

      /* This is not atom operation anyhow if someone want to truncate
       * file then he has to made necessary synchronizations in upper level
       * code. We have such situation in our RDD drivers and for us such
       * version is enough. [druzus]
       */
      ulOffsetLow = SetFilePointer( DosToWinHandle( hFileHandle ),
                                    ulOffsetLow, ( PLONG ) &ulOffsetHigh,
                                    ( DWORD ) SEEK_SET );
      if( ( ( ( ZH_FOFFSET ) ulOffsetHigh << 32 ) | ulOffsetLow ) == nOffset )
         fResult = SetEndOfFile( DosToWinHandle( hFileHandle ) ) != 0;
      else
         fResult = ZH_FALSE;

      zh_fsSetIOError( fResult, 0 );
   }
#else
   {
      int iResult;
#  if defined( ZH_USE_LARGEFILE64 )
      ZH_FAILURE_RETRY( iResult, ftruncate64( hFileHandle, nOffset ) );
#  else
      ZH_FAILURE_RETRY( iResult, ftruncate( hFileHandle, nOffset ) );
#  endif
      fResult = iResult != -1;
   }
#endif

   zh_vmLock();

   return fResult;
}

void zh_fsCommit( ZH_FHANDLE hFileHandle )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsCommit(%p)", ( void * ) ( ZH_PTRUINT ) hFileHandle ) );

   zh_vmUnlock();

#if defined( ZH_OS_WIN )

   zh_fsSetIOError( FlushFileBuffers( DosToWinHandle( hFileHandle ) ) != 0, 0 );

#elif defined( ZH_OS_UNIX )
{
   int iResult;
   /* We should check here only for _POSIX_SYNCHRONIZED_IO defined
    * and it should be enough to test if fdatasync() declaration
    * exists in <unistd.h>. Unfortunately on some OS-es like Darwin
    * _POSIX_SYNCHRONIZED_IO is defined but fdatasync() does not exists.
    * As workaround we are using this trick to check non zero version
    * number but on some systems it may disable using fdatasync() [druzus]
    */
#  if defined( _POSIX_SYNCHRONIZED_IO ) && _POSIX_SYNCHRONIZED_IO - 0 > 0
      /* faster - flushes data buffers only, without updating directory info
       */
      ZH_FAILURE_RETRY( iResult, fdatasync( hFileHandle ) );
#  else
      /* slower - flushes all file data buffers and i-node info
       */
      ZH_FAILURE_RETRY( iResult, fsync( hFileHandle ) );
#  endif
}
#else

   /* NOTE: close() functions releases all locks regardless if it is an
    * original or duplicated file handle
    */
   /* This hack is very dangerous. POSIX standard define that if _ANY_
    * file handle is closed all locks set by the process on the file
    * pointed by this descriptor are removed. It doesn't matter they
    * were done using different descriptor. It means that we now clean
    * all locks on hFileHandle with the code below if the OS is POSIX
    * compliant. I vote to disable it. [druzus]
    */
   {
      int dup_handle;
      ZH_BOOL fResult = ZH_FALSE;

      dup_handle = dup( hFileHandle );
      if( dup_handle != -1 )
      {
         close( dup_handle );
         fResult = ZH_TRUE;
      }
      zh_fsSetIOError( fResult, 0 );
   }

#endif

   zh_vmLock();
}

ZH_BOOL zh_fsLock( ZH_FHANDLE hFileHandle, ZH_ULONG ulStart,
                   ZH_ULONG ulLength, ZH_USHORT uiMode )
{
   ZH_BOOL fResult;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsLock(%p, %lu, %lu, %hu)", ( void * ) ( ZH_PTRUINT ) hFileHandle, ulStart, ulLength, uiMode ) );

   zh_vmUnlock();

#if defined( ZH_OS_WIN )
   switch( uiMode & FL_MASK )
   {
      case FL_LOCK:

         if( zh_iswinnt() )
         {
            OVERLAPPED sOlap;
            DWORD dwFlags;
            memset( &sOlap, 0, sizeof( sOlap ) );
            sOlap.Offset = ( DWORD ) ulStart;
            dwFlags = ( uiMode & FLX_SHARED ) ? 0 : LOCKFILE_EXCLUSIVE_LOCK;
            if( ! s_fUseWaitLocks || ! ( uiMode & FLX_WAIT ) )
            {
               dwFlags |= LOCKFILE_FAIL_IMMEDIATELY;
            }
            fResult = LockFileEx( DosToWinHandle( hFileHandle ), dwFlags, 0, ulLength, 0, &sOlap ) != 0;
         }
         else
         {
            fResult = LockFile( DosToWinHandle( hFileHandle ), ulStart, 0, ulLength, 0 ) != 0;
         }
         break;

      case FL_UNLOCK:

         if( zh_iswinnt() )
         {
            OVERLAPPED sOlap;
            memset( &sOlap, 0, sizeof( sOlap ) );
            sOlap.Offset = ( DWORD ) ulStart;
            fResult = UnlockFileEx( DosToWinHandle( hFileHandle ), 0, ulLength, 0, &sOlap ) != 0;
         }
         else
         {
            fResult = UnlockFile( DosToWinHandle( hFileHandle ), ulStart, 0, ulLength, 0 ) != 0;
         }
         break;

      default:
         fResult = ZH_FALSE;
   }
   zh_fsSetIOError( fResult, 0 );
#elif defined( _MSC_VER )
   {
      ZH_ULONG ulOldPos;

      ulOldPos = lseek( hFileHandle, 0L, SEEK_CUR );
      lseek( hFileHandle, ulStart, SEEK_SET );
      switch( uiMode & FL_MASK )
      {
         case FL_LOCK:
            fResult = ( locking( hFileHandle, _LK_NBLCK, ulLength ) == 0 );
            break;

         case FL_UNLOCK:
            fResult = ( locking( hFileHandle, _LK_UNLCK, ulLength ) == 0 );
            break;

         default:
            fResult = ZH_FALSE;
      }
      zh_fsSetIOError( fResult, 0 );
      lseek( hFileHandle, ulOldPos, SEEK_SET );
   }
#elif defined( __MINGW32__ )
   {
      ZH_ULONG ulOldPos;

      ulOldPos = lseek( hFileHandle, 0L, SEEK_CUR );
      lseek( hFileHandle, ulStart, SEEK_SET );
      switch( uiMode & FL_MASK )
      {
         case FL_LOCK:
            fResult = ( _locking( hFileHandle, _LK_LOCK, ulLength ) == 0 );
            break;

         case FL_UNLOCK:
            fResult = ( _locking( hFileHandle, _LK_UNLCK, ulLength ) == 0 );
            break;

         default:
            fResult = ZH_FALSE;
      }
      zh_fsSetIOError( fResult, 0 );
      lseek( hFileHandle, ulOldPos, SEEK_SET );
   }
#elif defined( ZH_OS_UNIX )
   {
      struct flock lock_info;
      int iResult;

      switch( uiMode & FL_MASK )
      {
         case FL_LOCK:

            lock_info.l_type   = ( uiMode & FLX_SHARED ) ? F_RDLCK : F_WRLCK;
            lock_info.l_start  = ulStart;
            lock_info.l_len    = ulLength;
            lock_info.l_whence = SEEK_SET;   /* start from the beginning of the file */
            lock_info.l_pid    = 0;

            ZH_FAILURE_RETRY( iResult, fcntl( hFileHandle,
                               ( uiMode & FLX_WAIT ) ? F_SETLKW: F_SETLK,
                               &lock_info ) );
            fResult = iResult != -1;
            break;

         case FL_UNLOCK:

            lock_info.l_type   = F_UNLCK;   /* unlock */
            lock_info.l_start  = ulStart;
            lock_info.l_len    = ulLength;
            lock_info.l_whence = SEEK_SET;
            lock_info.l_pid    = 0;

            ZH_FAILURE_RETRY( iResult, fcntl( hFileHandle, F_SETLK, &lock_info ) );
            fResult = iResult != -1;
            break;

         default:
            fResult = ZH_FALSE;
      }
      zh_fsSetIOError( fResult, 0 );
   }
#else

   switch( uiMode & FL_MASK )
   {
      case FL_LOCK:
         fResult = ( lock( hFileHandle, ulStart, ulLength ) == 0 );
         break;

      case FL_UNLOCK:
         fResult = ( unlock( hFileHandle, ulStart, ulLength ) == 0 );
         break;

      default:
         fResult = ZH_FALSE;
   }
   zh_fsSetIOError( fResult, 0 );

#endif

   zh_vmLock();

   return fResult;
}

ZH_BOOL zh_fsLockLarge( ZH_FHANDLE hFileHandle, ZH_FOFFSET nStart,
                        ZH_FOFFSET nLength, ZH_USHORT uiMode )
{
   ZH_BOOL fResult;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsLockLarge(%p, %" PFHL "u, %" PFHL "i, %hu)", ( void * ) ( ZH_PTRUINT ) hFileHandle, nStart, nLength, uiMode ) );

#if defined( ZH_OS_WIN )
   {
      DWORD dwOffsetLo = ( DWORD ) ( nStart & 0xFFFFFFFF ),
            dwOffsetHi = ( DWORD ) ( nStart >> 32 ),
            dwLengthLo = ( DWORD ) ( nLength & 0xFFFFFFFF ),
            dwLengthHi = ( DWORD ) ( nLength >> 32 );

      zh_vmUnlock();
      switch( uiMode & FL_MASK )
      {
         case FL_LOCK:
            if( zh_iswinnt() )
            {
               OVERLAPPED sOlap;
               DWORD dwFlags;

               dwFlags = ( ( uiMode & FLX_SHARED ) ? 0 : LOCKFILE_EXCLUSIVE_LOCK );
               if( ! s_fUseWaitLocks || ! ( uiMode & FLX_WAIT ) )
               {
                  dwFlags |= LOCKFILE_FAIL_IMMEDIATELY;
               }

               memset( &sOlap, 0, sizeof( sOlap ) );
               sOlap.Offset = dwOffsetLo;
               sOlap.OffsetHigh = dwOffsetHi;

               fResult = LockFileEx( DosToWinHandle( hFileHandle ), dwFlags, 0,
                                     dwLengthLo, dwLengthHi, &sOlap ) != 0;
            }
            else
            {
               fResult = LockFile( DosToWinHandle( hFileHandle ),
                                   dwOffsetLo, dwOffsetHi,
                                   dwLengthLo, dwLengthHi ) != 0;
            }
            break;

         case FL_UNLOCK:
            if( zh_iswinnt() )
            {
               OVERLAPPED sOlap;

               memset( &sOlap, 0, sizeof( sOlap ) );
               sOlap.Offset = dwOffsetLo;
               sOlap.OffsetHigh = dwOffsetHi;

               fResult = UnlockFileEx( DosToWinHandle( hFileHandle ), 0,
                                       dwLengthLo, dwLengthHi, &sOlap ) != 0;
            }
            else
            {
               fResult = UnlockFile( DosToWinHandle( hFileHandle ),
                                     dwOffsetLo, dwOffsetHi,
                                     dwLengthLo, dwLengthHi ) != 0;
            }
            break;

         default:
            fResult = ZH_FALSE;
      }
      zh_fsSetIOError( fResult, 0 );
      zh_vmLock();
   }
#elif defined( ZH_USE_LARGEFILE64 )
   {
      struct flock64 lock_info;
      int iResult;

      zh_vmUnlock();
      switch( uiMode & FL_MASK )
      {
         case FL_LOCK:

            lock_info.l_type   = ( uiMode & FLX_SHARED ) ? F_RDLCK : F_WRLCK;
            lock_info.l_start  = nStart;
            lock_info.l_len    = nLength;
            lock_info.l_whence = SEEK_SET;   /* start from the beginning of the file */
            lock_info.l_pid    = 0;

            ZH_FAILURE_RETRY( iResult, fcntl( hFileHandle,
                               ( uiMode & FLX_WAIT ) ? F_SETLKW64: F_SETLK64,
                               &lock_info ) );
            fResult = iResult != -1;
            break;

         case FL_UNLOCK:

            lock_info.l_type   = F_UNLCK;   /* unlock */
            lock_info.l_start  = nStart;
            lock_info.l_len    = nLength;
            lock_info.l_whence = SEEK_SET;
            lock_info.l_pid    = 0;

            ZH_FAILURE_RETRY( iResult, fcntl( hFileHandle, F_SETLK64, &lock_info ) );
            fResult = iResult != -1;
            break;

         default:
            fResult = ZH_FALSE;
      }
      zh_fsSetIOError( fResult, 0 );
      zh_vmLock();
   }
#else
   fResult = zh_fsLock( hFileHandle, ( ZH_SIZE ) nStart, ( ZH_SIZE ) nLength, uiMode );
#endif

   return fResult;
}

int zh_fsLockTest( ZH_FHANDLE hFileHandle, ZH_FOFFSET nStart,
                   ZH_FOFFSET nLength, ZH_USHORT uiMode )
{
   int iResult;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsLockTest(%p, %" PFHL "u, %" PFHL "i, %hu)", ( void * ) ( ZH_PTRUINT ) hFileHandle, nStart, nLength, uiMode ) );

#if defined( ZH_OS_UNIX )
{
#  if defined( ZH_USE_LARGEFILE64 )
      struct flock64 lock_info;

      lock_info.l_type   = ( uiMode & FLX_SHARED ) ? F_RDLCK : F_WRLCK;
      lock_info.l_start  = nStart;
      lock_info.l_len    = nLength;
      lock_info.l_whence = SEEK_SET;
      lock_info.l_pid    = 0;
      iResult = fcntl( hFileHandle, F_GETLK64, &lock_info ) != -1 ?
                ( int ) lock_info.l_pid : -1;
#  else
      struct flock lock_info;

      lock_info.l_type   = ( uiMode & FLX_SHARED ) ? F_RDLCK : F_WRLCK;
      lock_info.l_start  = nStart;
      lock_info.l_len    = nLength;
      lock_info.l_whence = SEEK_SET;
      lock_info.l_pid    = 0;
      iResult = fcntl( hFileHandle, F_GETLK, &lock_info ) != -1 ?
                ( int ) lock_info.l_pid : -1;
#  endif
}
#else
   if( zh_fsLockLarge( hFileHandle, nStart, nLength, ( uiMode & FLX_SHARED ) | FL_LOCK ) )
   {
      if( ! zh_fsLockLarge( hFileHandle, nStart, nLength, FL_UNLOCK ) )
         iResult = -1;
      else
         iResult = 0;
   }
   else
      iResult = 1;
#endif

   return iResult;
}

ZH_ULONG zh_fsSeek( ZH_FHANDLE hFileHandle, ZH_LONG lOffset, ZH_USHORT uiFlags )
{
   ZH_ULONG ulPos;
   ZH_USHORT nFlags;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsSeek(%p, %ld, %hu)", ( void * ) ( ZH_PTRUINT ) hFileHandle, lOffset, uiFlags ) );

   nFlags = convert_seek_flags( uiFlags );

   zh_vmUnlock();
#if defined( ZH_OS_WIN )
   /* This DOS hack creates 2 GiB file size limit, Druzus */
   if( lOffset < 0 && nFlags == SEEK_SET )
   {
      ulPos = ( ULONG ) INVALID_SET_FILE_POINTER;
      zh_fsSetError( 25 ); /* 'Seek Error' */
   }
   else
   {
      ulPos = ( ULONG ) SetFilePointer( DosToWinHandle( hFileHandle ), lOffset, NULL, ( DWORD ) nFlags );
      zh_fsSetIOError( ulPos != ( ULONG ) INVALID_SET_FILE_POINTER, 0 );
   }

   if( ulPos == ( ULONG ) INVALID_SET_FILE_POINTER )
   {
      ulPos = ( ULONG ) SetFilePointer( DosToWinHandle( hFileHandle ), 0, NULL, SEEK_CUR );
      if( ulPos == ( ULONG ) INVALID_SET_FILE_POINTER )
         ulPos = 0;
   }
#else
   /* This DOS hack creates 2 GiB file size limit, Druzus */
   if( lOffset < 0 && nFlags == SEEK_SET )
   {
      ulPos = ( ZH_ULONG ) -1;
      zh_fsSetError( 25 ); /* 'Seek Error' */
   }
   else
   {
      ulPos = lseek( hFileHandle, lOffset, nFlags );
      zh_fsSetIOError( ulPos != ( ZH_ULONG ) -1, 0 );
#  if defined( ZH_OS_UNIX )
      /* small trick to resolve problem with position reported for directories */
      if( ulPos == LONG_MAX && lOffset == 0 && nFlags == SEEK_END )
      {
         /* we do not need to use fstat64() here on 32-bit platforms, [druzus] */
         struct stat st;

         if( fstat( hFileHandle, &st ) == 0 )
            ulPos = st.st_size;
      }
#  endif
   }

   if( ulPos == ( ZH_ULONG ) -1 )
   {
      ulPos = lseek( hFileHandle, 0L, SEEK_CUR );
      if( ulPos == ( ZH_ULONG ) -1 )
         ulPos = 0;
   }
#endif
   zh_vmLock();

   return ulPos;
}

ZH_FOFFSET zh_fsSeekLarge( ZH_FHANDLE hFileHandle, ZH_FOFFSET nOffset, ZH_USHORT uiFlags )
{
   ZH_FOFFSET nPos;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsSeekLarge(%p, %" PFHL "i, %hu)", ( void * ) ( ZH_PTRUINT ) hFileHandle, nOffset, uiFlags ) );

#if defined( ZH_OS_WIN )
   {
      ZH_USHORT nFlags = convert_seek_flags( uiFlags );

      ULONG ulOffsetLow  = ( ULONG ) ( nOffset & 0xFFFFFFFF ),
            ulOffsetHigh = ( ULONG ) ( nOffset >> 32 );

      zh_vmUnlock();
      if( nOffset < 0 && nFlags == SEEK_SET )
      {
         nPos = ( ZH_FOFFSET ) -1;
         zh_fsSetError( 25 ); /* 'Seek Error' */
      }
      else
      {
         ulOffsetLow = SetFilePointer( DosToWinHandle( hFileHandle ),
                                       ulOffsetLow, ( PLONG ) &ulOffsetHigh,
                                       ( DWORD ) nFlags );
         if( ulOffsetLow == ( ULONG ) INVALID_SET_FILE_POINTER && GetLastError() != NO_ERROR )
            nPos = ( ZH_FOFFSET ) -1;
         else
            nPos = ( ( ZH_FOFFSET ) ulOffsetHigh << 32 ) | ulOffsetLow;
         zh_fsSetIOError( nPos != ( ZH_FOFFSET ) -1, 0 );
      }

      if( nPos == ( ZH_FOFFSET ) -1 )
      {
         ulOffsetHigh = 0;
         ulOffsetLow = SetFilePointer( DosToWinHandle( hFileHandle ),
                                       0, ( PLONG ) &ulOffsetHigh, SEEK_CUR );
         if( ulOffsetLow == ( ULONG ) INVALID_SET_FILE_POINTER && GetLastError() != NO_ERROR )
            nPos = 0;
         else
            nPos = ( ( ZH_FOFFSET ) ulOffsetHigh << 32 ) | ulOffsetLow;
      }
      zh_vmLock();
   }
#elif defined( ZH_USE_LARGEFILE64 )
   {
      ZH_USHORT nFlags = convert_seek_flags( uiFlags );

      zh_vmUnlock();
      if( nOffset < 0 && nFlags == SEEK_SET )
      {
         nPos = ( ZH_FOFFSET ) -1;
         zh_fsSetError( 25 ); /* 'Seek Error' */
      }
      else
      {
         nPos = lseek64( hFileHandle, nOffset, nFlags );
         zh_fsSetIOError( nPos != ( ZH_FOFFSET ) -1, 0 );
#  if defined( ZH_OS_UNIX )
         /* small trick to resolve problem with position reported for directories */
         if( nPos == LONG_MAX && nOffset == 0 && nFlags == SEEK_END )
         {
            struct stat64 st;

            if( fstat64( hFileHandle, &st ) == 0 )
               nPos = st.st_size;
         }
#  endif
      }

      if( nPos == ( ZH_FOFFSET ) -1 )
      {
         nPos = lseek64( hFileHandle, 0L, SEEK_CUR );
         if( nPos == ( ZH_FOFFSET ) -1 )
            nPos = 0;
      }
      zh_vmLock();
   }
#else
   nPos = ( ZH_FOFFSET ) zh_fsSeek( hFileHandle, ( ZH_ISIZ ) nOffset, uiFlags );
#endif

   return nPos;
}

ZH_FOFFSET zh_fsTell( ZH_FHANDLE hFileHandle )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsTell(%p)", ( void * ) ( ZH_PTRUINT ) hFileHandle ) );

   return zh_fsSeekLarge( hFileHandle, 0, FS_RELATIVE );
}

ZH_FOFFSET zh_fsGetSize( ZH_FHANDLE hFileHandle )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsGetSize(%p)", ( void * ) ( ZH_PTRUINT ) hFileHandle ) );

#if defined( ZH_OS_WIN )
   {
      DWORD dwFileSizeLow, dwFileSizeHigh = 0;
      ZH_BOOL fOK;

      dwFileSizeLow = GetFileSize( DosToWinHandle( hFileHandle ), &dwFileSizeHigh );
      fOK = dwFileSizeLow != INVALID_FILE_SIZE || GetLastError() == NO_ERROR;
      zh_fsSetIOError( fOK, 0 );

      return fOK ? ( ( ZH_FOFFSET ) dwFileSizeHigh << 32 ) | dwFileSizeLow : 0;
   }
#else
   return zh_fsSeekLarge( hFileHandle, 0, FS_END );
#endif
}

ZH_BOOL zh_fsDelete( const char * pszFileName )
{
   ZH_BOOL fResult;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsDelete(%s)", pszFileName ) );

#if defined( ZH_OS_WIN )
   {
      LPCTSTR lpFileName;
      LPTSTR lpFree;

      lpFileName = ZH_FSNAMECONV( pszFileName, &lpFree );

      zh_vmUnlock();

      fResult = DeleteFile( lpFileName ) != 0;
      zh_fsSetIOError( fResult, 0 );

      zh_vmLock();

      if( lpFree )
         zh_xfree( lpFree );
   }
#else
   {
      char * pszFree;

      pszFileName = zh_fsNameConv( pszFileName, &pszFree );

      zh_vmUnlock();

      fResult = ( remove( pszFileName ) == 0 );
      zh_fsSetIOError( fResult, 0 );

      zh_vmLock();

      if( pszFree )
         zh_xfree( pszFree );
   }
#endif

   return fResult;
}

ZH_BOOL zh_fsRename( const char * pOldName, const char * pNewName )
{
   ZH_BOOL fResult;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsRename(%s, %s)", pOldName, pNewName ) );

#if defined( ZH_OS_WIN )
   {
      LPCTSTR lpOldName, lpNewName;
      LPTSTR lpOldFree, lpNewFree;

      lpOldName = ZH_FSNAMECONV( pOldName, &lpOldFree );
      lpNewName = ZH_FSNAMECONV( pNewName, &lpNewFree );

      zh_vmUnlock();

      fResult = MoveFile( lpOldName, lpNewName ) != 0;
      zh_fsSetIOError( fResult, 0 );

      zh_vmLock();

      if( lpOldFree )
         zh_xfree( lpOldFree );
      if( lpNewFree )
         zh_xfree( lpNewFree );
   }
#else
   {
      char * pszFreeOld, * pszFreeNew;

      pOldName = zh_fsNameConv( pOldName, &pszFreeOld );
      pNewName = zh_fsNameConv( pNewName, &pszFreeNew );

      zh_vmUnlock();

      fResult = ( rename( pOldName, pNewName ) == 0 );
      zh_fsSetIOError( fResult, 0 );

      zh_vmLock();

      if( pszFreeOld )
         zh_xfree( pszFreeOld );
      if( pszFreeNew )
         zh_xfree( pszFreeNew );
   }
#endif

   return fResult;
}

ZH_BOOL zh_fsMkDir( const char * pszDirName )
{
   ZH_BOOL fResult;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsMkDir(%s)", pszDirName ) );

#if defined( ZH_OS_WIN )
   {
      LPCTSTR lpDirName;
      LPTSTR lpFree;

      lpDirName = ZH_FSNAMECONV( pszDirName, &lpFree );

      zh_vmUnlock();

      fResult = CreateDirectory( lpDirName, NULL ) != 0;
      zh_fsSetIOError( fResult, 0 );

      zh_vmLock();

      if( lpFree )
         zh_xfree( lpFree );
   }
#else
   {
      char * pszFree;

      pszDirName = zh_fsNameConv( pszDirName, &pszFree );

      zh_vmUnlock();

#  if ! defined( ZH_OS_UNIX ) &&  defined( __MINGW32__ )
      fResult = ( mkdir( pszDirName ) == 0 );
#  else
      fResult = ( mkdir( pszDirName, S_IRWXU | S_IRWXG | S_IRWXO ) == 0 );
#  endif
      zh_fsSetIOError( fResult, 0 );

      zh_vmLock();

      if( pszFree )
         zh_xfree( pszFree );
   }
#endif

   return fResult;
}

ZH_BOOL zh_fsChDir( const char * pszDirName )
{
   ZH_BOOL fResult;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsChDir(%s)", pszDirName ) );

#if defined( ZH_OS_WIN )
   {
      LPCTSTR lpDirName;
      LPTSTR lpFree;
      UINT uiErrMode;

      lpDirName = ZH_FSNAMECONV( pszDirName, &lpFree );

      zh_vmUnlock();

      uiErrMode = SetErrorMode( SEM_FAILCRITICALERRORS );
      fResult = SetCurrentDirectory( lpDirName ) != FALSE;
      SetErrorMode( uiErrMode );
      zh_fsSetIOError( fResult, 0 );

      zh_vmLock();

      if( lpFree )
         zh_xfree( lpFree );
   }

#else
   {
      char * pszFree;

      pszDirName = zh_fsNameConv( pszDirName, &pszFree );

      zh_vmUnlock();

      fResult = ( chdir( pszDirName ) == 0 );
      zh_fsSetIOError( fResult, 0 );

      zh_vmLock();

      if( pszFree )
         zh_xfree( pszFree );
   }
#endif

   return fResult;
}

ZH_BOOL zh_fsRmDir( const char * pszDirName )
{
   ZH_BOOL fResult;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsRmDir(%s)", pszDirName ) );

#if defined( ZH_OS_WIN )
   {
      LPCTSTR lpDirName;
      LPTSTR lpFree;

      lpDirName = ZH_FSNAMECONV( pszDirName, &lpFree );

      zh_vmUnlock();

      fResult = RemoveDirectory( lpDirName ) != 0;
      zh_fsSetIOError( fResult, 0 );

      zh_vmLock();

      if( lpFree )
         zh_xfree( lpFree );
   }
#else
   {
      char * pszFree;

      pszDirName = zh_fsNameConv( pszDirName, &pszFree );

      zh_vmUnlock();

      fResult = ( rmdir( pszDirName ) == 0 );
      zh_fsSetIOError( fResult, 0 );

      zh_vmLock();

      if( pszFree )
         zh_xfree( pszFree );
   }
#endif

   return fResult;
}

/* NOTE: This is not thread safe function, it's there for compatibility. */
/* NOTE: 0 = current drive, 1 = A, 2 = B, 3 = C, etc. */

const char * zh_fsCurDir( int iDrive )
{
   char * pszDirBuffer;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsCurDir(%d)", iDrive ) );

   pszDirBuffer = zh_stackDirBuffer();
   zh_fsCurDirBuff( iDrive, pszDirBuffer, ZH_PATH_MAX );

   return pszDirBuffer;
}

/* NOTE: Thread safe version of zh_fsCurDir() */
/* NOTE: 0 = current drive, 1 = A, 2 = B, 3 = C, etc. */

ZH_ERRCODE zh_fsCurDirBuff( int iDrive, char * pszBuffer, ZH_SIZE nSize )
{
   int iCurDrv = iDrive;
   ZH_ERRCODE nResult;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsCurDirBuff(%d)", iDrive ) );

   pszBuffer[ 0 ] = '\0';

   /*
    * do not cover this code by ZH_OS_HAS_DRIVE_LETTER macro
    * It will allow us to add drive emulation in zh_fsCurDrv()/zh_fsChDrv()
    * and zh_fsNameConv()
    */
#if defined( ZH_OS_WIN ) || ! defined( __MINGW32__ )
   if( iDrive > 0 )
   {
      iCurDrv = zh_fsCurDrv() + 1;
      if( iDrive != iCurDrv )
         zh_fsChDrv( iDrive - 1 );
   }
#endif

   zh_vmUnlock();

#if defined( ZH_OS_WIN )
   {
      DWORD dwSize = ( DWORD ) nSize;
      LPTSTR lpBuffer = ( LPTSTR ) zh_xgrab( dwSize * sizeof( TCHAR ) );
      lpBuffer[ 0 ] = TEXT( '\0' );
      zh_fsSetIOError( ( GetCurrentDirectory( dwSize, lpBuffer ) != 0 ), 0 );
      lpBuffer[ dwSize - 1 ] = TEXT( '\0' );
      ZH_OSSTRDUP2( lpBuffer, pszBuffer, nSize - 1 );
      zh_xfree( lpBuffer );
   }

#elif defined( __MINGW32__ )

   if( iDrive >= 0 )
      zh_fsSetIOError( ( _getdcwd( iDrive, pszBuffer, nSize ) != NULL ), 0 );
   else
      zh_fsSetError( ( ZH_ERRCODE ) FS_ERROR );

#else

   zh_fsSetIOError( ( getcwd( pszBuffer, nSize ) != NULL ), 0 );

#endif

   zh_vmLock();

   nResult = zh_fsError();

   if( iDrive != iCurDrv )
   {
      zh_fsChDrv( iCurDrv - 1 );
      zh_fsSetError( nResult );
   }

   pszBuffer[ nSize - 1 ] = '\0';

   if( nResult == 0 && pszBuffer[ 0 ] )
   {
      char * pszStart;
      ZH_SIZE nLen;

      /* Strip the leading drive spec, and leading backslash if there's one. */
      nLen = strlen( pszBuffer );
      pszStart = pszBuffer;

#if defined( ZH_OS_HAS_DRIVE_LETTER )
      if( pszStart[ 1 ] == ZH_OS_DRIVE_DELIM_CHR )
      {
         pszStart += 2;
         nLen -= 2;
      }
#endif
      if( strchr( ZH_OS_PATH_DELIM_CHR_LIST, ( ZH_UCHAR ) pszStart[ 0 ] ) )
      {
         pszStart++;
         nLen--;
      }

      /* Strip the trailing (back)slash if there's one */
      if( nLen && strchr( ZH_OS_PATH_DELIM_CHR_LIST, ( ZH_UCHAR ) pszStart[ nLen - 1 ] ) )
         nLen--;

      if( nLen && pszBuffer != pszStart )
         memmove( pszBuffer, pszStart, nLen );

      pszBuffer[ nLen ] = '\0';

#if ! defined( ZH_OS_WIN )
      /* Convert from OS codepage */
      {
         char * pszFree = NULL;
         const char * pszResult;

         nLen = nSize;
         pszResult = zh_osDecodeCP( pszBuffer, &pszFree, &nLen );

         if( pszResult != pszBuffer )
            zh_strncpy( pszBuffer, pszResult, nSize - 1 );
         if( pszFree )
            zh_xfree( pszFree );
      }
#endif
   }

   return nResult;
}

ZH_BOOL zh_fsGetCWD( char * pszBuffer, ZH_SIZE nSize )
{
   ZH_BOOL fResult;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsGetCWD(%p,%" ZH_PFS "u)", ( void * ) pszBuffer, nSize ) );

   pszBuffer[ 0 ] = '\0';

   zh_vmUnlock();

#if defined( ZH_OS_WIN )
   {
      DWORD dwSize = ( DWORD ) nSize;
      LPTSTR lpBuffer = ( LPTSTR ) zh_xgrab( dwSize * sizeof( TCHAR ) );
      lpBuffer[ 0 ] = TEXT( '\0' );
      fResult = GetCurrentDirectory( dwSize, lpBuffer ) != 0;
      zh_fsSetIOError( fResult, 0 );
      lpBuffer[ dwSize - 1 ] = TEXT( '\0' );
      ZH_OSSTRDUP2( lpBuffer, pszBuffer, nSize - 1 );
      zh_xfree( lpBuffer );
   }
#else

   fResult = getcwd( pszBuffer, nSize ) != NULL;
   zh_fsSetIOError( fResult, 0 );

#endif

   zh_vmLock();

   pszBuffer[ nSize - 1 ] = '\0';

   if( fResult && pszBuffer[ 0 ] )
   {
      ZH_SIZE nLen;
      nLen = strlen( pszBuffer );

      /* add the trailing (back)slash if there's no one */
      if( nLen + 1 < nSize &&
          strchr( ZH_OS_PATH_DELIM_CHR_LIST, ( ZH_UCHAR ) pszBuffer[ nLen - 1 ] ) == 0 )
      {
         pszBuffer[ nLen++ ] = ZH_OS_PATH_DELIM_CHR;
         pszBuffer[ nLen ] = '\0';
      }

#if ! defined( ZH_OS_WIN )
      /* Convert from OS codepage */
      {
         char * pszFree = NULL;
         const char * pszResult;

         nLen = nSize;
         pszResult = zh_osDecodeCP( pszBuffer, &pszFree, &nLen );

         if( pszResult != pszBuffer )
            zh_strncpy( pszBuffer, pszResult, nSize - 1 );
         if( pszFree )
            zh_xfree( pszFree );
      }
#endif
   }

   return fResult;
}

ZH_BOOL zh_fsSetCWD( const char * pszDirName )
{
   ZH_BOOL fResult;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsSetCWD(%s)", pszDirName ) );

#if defined( ZH_OS_WIN )
   {
      LPCTSTR lpDirName;
      LPTSTR lpFree;
      UINT uiErrMode;

      lpDirName = ZH_FSNAMECONV( pszDirName, &lpFree );

      zh_vmUnlock();

      uiErrMode = SetErrorMode( SEM_FAILCRITICALERRORS );
      fResult = SetCurrentDirectory( lpDirName ) != FALSE;
      zh_fsSetIOError( fResult, 0 );
      SetErrorMode( uiErrMode );

      zh_vmLock();

      if( lpFree )
         zh_xfree( lpFree );
   }
#else
   {
      char * pszFree;

      pszDirName = zh_fsNameConv( pszDirName, &pszFree );

      zh_vmUnlock();

      fResult = ( chdir( pszDirName ) == 0 );
      zh_fsSetIOError( fResult, 0 );

#if defined( ZH_OS_HAS_DRIVE_LETTER )
      if( fResult && pszDirName[ 0 ] != 0 &&
          pszDirName[ 1 ] == ZH_OS_DRIVE_DELIM_CHR )
      {
         int iDrive = pszDirName[ 0 ];

         if( iDrive >= 'A' && iDrive <= 'Z' )
            iDrive -= 'A';
         else if( iDrive >= 'a' && iDrive <= 'z' )
            iDrive -= 'a';
         else
            iDrive = 0;

         if( iDrive )
            ZH_FS_SETDRIVE( iDrive );
      }
#endif

      zh_vmLock();

      if( pszFree )
         zh_xfree( pszFree );
   }
#endif

   return fResult;
}

/* NOTE: 0=A:, 1=B:, 2=C:, 3=D:, ... */

ZH_ERRCODE zh_fsChDrv( int iDrive )
{
   ZH_ERRCODE nResult;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsChDrv(%d)", iDrive ) );

#if defined( ZH_OS_HAS_DRIVE_LETTER )
   {
      int iSave, iNewDrive;

      zh_vmUnlock();

      ZH_FS_GETDRIVE( iSave );
      ZH_FS_SETDRIVE( iDrive );
      ZH_FS_GETDRIVE( iNewDrive );

      if( iDrive == iNewDrive )
      {
         nResult = 0;
         zh_fsSetError( 0 );
      }
      else
      {
         ZH_FS_SETDRIVE( iSave );

         nResult = ( ZH_ERRCODE ) FS_ERROR;
         zh_fsSetError( ( ZH_ERRCODE ) FS_ERROR );
      }
      zh_vmLock();
   }
#else

   ZH_SYMBOL_UNUSED( iDrive );
   nResult = ( ZH_ERRCODE ) FS_ERROR;
   zh_fsSetError( ( ZH_ERRCODE ) FS_ERROR );

#endif

   return nResult;
}

/* NOTE: 0=A:, 1=B:, 2=C:, 3=D:, ... */

int zh_fsCurDrv( void )
{
   int iDrive;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsCurDrv()" ) );

#if defined( ZH_OS_HAS_DRIVE_LETTER )

   zh_vmUnlock();
   ZH_FS_GETDRIVE( iDrive );
   zh_fsSetError( 0 );
   zh_vmLock();

#else

   iDrive = 0;
   zh_fsSetError( ( ZH_ERRCODE ) FS_ERROR );

#endif

   return iDrive; /* Return the drive number, base 0. */
}

/* NOTE: 0=A:, 1=B:, 2=C:, 3=D:, ... */

ZH_ERRCODE zh_fsIsDrv( int iDrive )
{
   ZH_ERRCODE nResult;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsIsDrv(%d)", iDrive ) );

   if( iDrive >= 0 )
#if defined( ZH_OS_WIN )
   {
      zh_vmUnlock();
      nResult = ( ( GetLogicalDrives() >> iDrive ) & 1 ) ? 0 : ( ZH_ERRCODE ) F_ERROR;
      zh_vmLock();
      zh_fsSetError( 0 );
   }
#elif defined( ZH_OS_HAS_DRIVE_LETTER )
   {
      int iSave, iNewDrive;

      zh_vmUnlock();

      ZH_FS_GETDRIVE( iSave );
      ZH_FS_SETDRIVE( iDrive );
      ZH_FS_GETDRIVE( iNewDrive );
      nResult = ( iDrive == iNewDrive ) ? 0 : ( ZH_ERRCODE ) FS_ERROR;
      ZH_FS_SETDRIVE( iSave );
      zh_fsSetError( 0 );

      zh_vmLock();
   }
#else
   {
      ZH_SYMBOL_UNUSED( iDrive );
      nResult = ( ZH_ERRCODE ) FS_ERROR;
      zh_fsSetError( 0 );
   }
#endif
   else
   {
      nResult = ( ZH_ERRCODE ) FS_ERROR;
      zh_fsSetError( 0 );
   }

   return nResult;
}

ZH_BOOL zh_fsIsDevice( ZH_FHANDLE hFileHandle )
{
   ZH_BOOL fResult;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsIsDevice(%p)", ( void * ) ( ZH_PTRUINT ) hFileHandle ) );

   zh_vmUnlock();

#if defined( ZH_OS_WIN )

   fResult = GetFileType( DosToWinHandle( hFileHandle ) ) == FILE_TYPE_CHAR;
   zh_fsSetIOError( fResult, 0 );

#else

#  if defined( _MSC_VER ) || defined( __MINGW32__ )
      fResult = _isatty( hFileHandle ) != 0;
#  else
      fResult = isatty( hFileHandle ) != 0;
#  endif
   zh_fsSetIOError( fResult, 0 );

#endif

   zh_vmLock();

   return fResult;
}

/* convert file name for zh_fsExtOpen()
 * caller must free the returned buffer
 */
char * zh_fsExtName( const char * pszFileName, const char * pDefExt,
                     ZH_FATTR nExFlags, const char * pPaths )
{
   ZH_PATHNAMES * pNextPath;
   PZH_FNAME pFilepath;
   ZH_BOOL fIsFile = ZH_FALSE;
   char * szPath;

   szPath = ( char * ) zh_xgrab( ZH_PATH_MAX );

   pFilepath = zh_fsFNameSplit( pszFileName );

   if( pDefExt && ( ( nExFlags & FXO_FORCEEXT ) || ! pFilepath->szExtension ) )
      pFilepath->szExtension = pDefExt;

   if( pFilepath->szPath )
   {
      zh_fsFNameMerge( szPath, pFilepath );
   }
   else if( nExFlags & FXO_DEFAULTS )
   {
      const char * szDefault = zh_setGetDefault();
      if( szDefault )
      {
         pFilepath->szPath = szDefault;
         zh_fsFNameMerge( szPath, pFilepath );
         fIsFile = zh_fsFileExists( szPath );
      }
      if( ! fIsFile &&
          ( nExFlags & ( FXO_TRUNCATE | FXO_APPEND | FXO_UNIQUE ) ) == 0 &&
          zh_setGetPath() )
      {
         pNextPath = zh_setGetFirstSetPath();
         while( ! fIsFile && pNextPath )
         {
            pFilepath->szPath = pNextPath->szPath;
            zh_fsFNameMerge( szPath, pFilepath );
            fIsFile = zh_fsFileExists( szPath );
            pNextPath = pNextPath->pNext;
         }
      }
      if( ! fIsFile )
      {
         pFilepath->szPath = szDefault ? szDefault : NULL;
         zh_fsFNameMerge( szPath, pFilepath );
      }
   }
   else if( pPaths && *pPaths )
   {
      ZH_PATHNAMES * pSearchPath = NULL;
      zh_fsAddSearchPath( pPaths, &pSearchPath );
      pNextPath = pSearchPath;
      while( ! fIsFile && pNextPath )
      {
         pFilepath->szPath = pNextPath->szPath;
         zh_fsFNameMerge( szPath, pFilepath );
         fIsFile = zh_fsFileExists( szPath );
         pNextPath = pNextPath->pNext;
      }
      zh_fsFreeSearchPath( pSearchPath );
      if( ! fIsFile )
      {
         pFilepath->szPath = NULL;
         zh_fsFNameMerge( szPath, pFilepath );
      }
   }
   else
      zh_fsFNameMerge( szPath, pFilepath );

   zh_xfree( pFilepath );

   return szPath;
}

ZH_FHANDLE zh_fsExtOpen( const char * pszFileName, const char * pDefExt,
                         ZH_FATTR nExFlags, const char * pPaths,
                         PZH_ITEM pError )
{
   ZH_FHANDLE hFile;
   ZH_USHORT uiFlags;
   const char * szPath;
   char * szFree = NULL;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsExtOpen(%s, %s, %u, %p, %p)", pszFileName, pDefExt, nExFlags, ( const void * ) pPaths, ( void * ) pError ) );


   if( pDefExt || pPaths || pError ||
       ( nExFlags & ( FXO_DEFAULTS | FXO_COPYNAME ) ) != 0 )
      szPath = szFree = zh_fsExtName( pszFileName, pDefExt, nExFlags, pPaths );
   else
      szPath = pszFileName;

   uiFlags = ( ZH_USHORT ) ( nExFlags & 0xff );
   if( nExFlags & ( FXO_TRUNCATE | FXO_APPEND | FXO_UNIQUE ) )
   {
      uiFlags |= FO_CREAT;
      if( nExFlags & FXO_UNIQUE )
         uiFlags |= FO_EXCL;
#if defined( ZH_USE_SHARELOCKS )
      else if( ( nExFlags & ( FXO_TRUNCATE | FXO_SHARELOCK ) ) == FXO_TRUNCATE )
#else
      else if( nExFlags & FXO_TRUNCATE )
#endif
         uiFlags |= FO_TRUNC;
   }

   hFile = zh_fsOpenEx( szPath, uiFlags, FC_NORMAL );

#if defined( ZH_USE_SHARELOCKS )
   if( hFile != FS_ERROR && ( nExFlags & FXO_SHARELOCK ) != 0 )
   {
#if defined( ZH_USE_BSDLOCKS )
      int iLock, iResult;
      if( /* ( uiFlags & ( FO_READ | FO_WRITE | FO_READWRITE ) ) == FO_READ || */
          ( uiFlags & ( FO_DENYREAD | FO_DENYWRITE | FO_EXCLUSIVE ) ) == 0 )
         iLock = LOCK_SH | LOCK_NB;
      else
         iLock = LOCK_EX | LOCK_NB;
      zh_vmUnlock();
      ZH_FAILURE_RETRY( iResult, flock( hFile, iLock ) );
      zh_vmLock();
      if( iResult != 0 )
#else
      ZH_USHORT uiLock;
      if( ( uiFlags & ( FO_READ | FO_WRITE | FO_READWRITE ) ) == FO_READ ||
          ( uiFlags & ( FO_DENYREAD | FO_DENYWRITE | FO_EXCLUSIVE ) ) == 0 )
         uiLock = FL_LOCK | FLX_SHARED;
      else
         uiLock = FL_LOCK | FLX_EXCLUSIVE;

      if( ! zh_fsLockLarge( hFile, ZH_SHARELOCK_POS, ZH_SHARELOCK_SIZE, uiLock ) )
#endif
      {
         zh_fsClose( hFile );
         hFile = FS_ERROR;
         /*
          * fix for NetErr() support and Clipper compatibility,
          * should be revised with a better multi platform solution.
          */
         zh_fsSetError( ( nExFlags & FXO_TRUNCATE ) ? 5 : 32 );
      }
      else if( nExFlags & FXO_TRUNCATE )
      {
         /* truncate the file only if properly locked */
         zh_fsSeek( hFile, 0, FS_SET );
         zh_fsTruncAt( hFile, 0 );
         if( zh_fsError() != 0 )
         {
            zh_fsClose( hFile );
            hFile = FS_ERROR;
            zh_fsSetError( 5 );
         }
      }
   }
#else
   /*
    * Temporary fix for NetErr() support and Clipper compatibility,
    * should be revised with a better solution.
    */
   if( ( nExFlags & ( FXO_TRUNCATE | FXO_APPEND | FXO_UNIQUE ) ) == 0 &&
       zh_fsError() == 5 )
   {
      zh_fsSetError( 32 );
   }
#endif

   if( pError )
   {
      zh_errPutFileName( pError, szPath );
      if( hFile == FS_ERROR )
      {
         zh_errPutOsCode( pError, zh_fsError() );
         zh_errPutGenCode( pError, ( ZH_ERRCODE ) ( ( nExFlags & FXO_TRUNCATE ) ? EG_CREATE : EG_OPEN ) );
      }
   }

   if( nExFlags & FXO_COPYNAME && hFile != FS_ERROR )
      zh_strncpy( ( char * ) ZH_UNCONST( pszFileName ), szPath, ZH_PATH_MAX - 1 );

   if( szFree )
      zh_xfree( szFree );

   return hFile;
}

ZH_BOOL zh_fsEof( ZH_FHANDLE hFileHandle )
{
   ZH_BOOL fResult;

   zh_vmUnlock();


{
   ZH_FOFFSET curPos;
   ZH_FOFFSET endPos;

   curPos = zh_fsSeekLarge( hFileHandle, 0L, FS_RELATIVE );
   if( curPos != -1 )
   {
      ZH_FOFFSET newPos;
      endPos = zh_fsSeekLarge( hFileHandle, 0L, FS_END );
      newPos = zh_fsSeekLarge( hFileHandle, curPos, FS_SET );
      fResult = ( endPos != -1 && newPos == curPos );
   }
   else
   {
      endPos = -1;
      fResult = ZH_FALSE;
   }
   zh_fsSetIOError( fResult, 0 );
   fResult = ! fResult || curPos >= endPos;
}


   zh_vmLock();

   return fResult;
}

const char * zh_fsNameConv( const char * pszFileName, char ** pszFree )
{
   int iFileCase, iDirCase;
   char cDirSep;
   ZH_BOOL fTrim, fEncodeCP;

/*
   Convert file and dir case. The allowed SET options are:
      LOWER - Convert all characters of file to lower
      UPPER - Convert all characters of file to upper
      MIXED - Leave as is

   The allowed environment options are:
      FILECASE - define the case of file
      DIRCASE - define the case of path
      DIRSEPARATOR - define separator of path (Ex. "/")
      TRIMFILENAME - strip trailing and leading spaces (also from extension)
 */

   if( pszFree )
      *pszFree = NULL;

   if( ! zh_vmIsReady() )
      return pszFileName;

   fTrim = zh_setGetTrimFileName();
   fEncodeCP = zh_osUseCP();
   cDirSep = ( char ) zh_setGetDirSeparator();
   iFileCase = zh_setGetFileCase();
   iDirCase = zh_setGetDirCase();
   if( fTrim )
   {
      if( strchr( pszFileName, ' ' ) == NULL )
         fTrim = ZH_FALSE;
   }
   if( cDirSep != ZH_OS_PATH_DELIM_CHR )
   {
      if( strchr( pszFileName, ( ZH_UCHAR ) cDirSep ) == NULL )
         cDirSep = ZH_OS_PATH_DELIM_CHR;
   }

   if( fTrim || fEncodeCP ||
       cDirSep != ZH_OS_PATH_DELIM_CHR ||
       iFileCase != ZH_SET_CASE_MIXED ||
       iDirCase != ZH_SET_CASE_MIXED )
   {
      PZH_FNAME pFileName;
      ZH_SIZE nLen;
      char * pszPath = NULL, * pszName = NULL, * pszExt = NULL;

      if( pszFree )
      {
         pszFileName = *pszFree = zh_strncpy( ( char * ) zh_xgrab( ZH_PATH_MAX ),
                                              pszFileName, ZH_PATH_MAX - 1 );
      }

      if( cDirSep != ZH_OS_PATH_DELIM_CHR )
      {
         char * p = ( char * ) ZH_UNCONST( pszFileName );
         while( *p )
         {
            if( *p == cDirSep )
               *p = ZH_OS_PATH_DELIM_CHR;
            p++;
         }
      }

      pFileName = zh_fsFNameSplit( pszFileName );

      /* strip trailing and leading spaces */
      if( fTrim )
      {
         if( pFileName->szName )
         {
            nLen = strlen( pFileName->szName );
            nLen = zh_strRTrimLen( pFileName->szName, nLen, ZH_FALSE );
            pFileName->szName = zh_strLTrim( pFileName->szName, &nLen );
            ( ( char * ) ZH_UNCONST( pFileName->szName ) )[ nLen ] = '\0';
         }
         if( pFileName->szExtension )
         {
            nLen = strlen( pFileName->szExtension );
            nLen = zh_strRTrimLen( pFileName->szExtension, nLen, ZH_FALSE );
            pFileName->szExtension = zh_strLTrim( pFileName->szExtension, &nLen );
            ( ( char * ) ZH_UNCONST( pFileName->szExtension ) )[ nLen ] = '\0';
         }
      }

      /* FILECASE */
      if( iFileCase == ZH_SET_CASE_LOWER )
      {
         if( pFileName->szName )
            pFileName->szName = pszName = zh_cdpnDupLower( zh_vmCDP(), pFileName->szName, NULL );
         if( pFileName->szExtension )
            pFileName->szExtension = pszExt = zh_cdpnDupLower( zh_vmCDP(), pFileName->szExtension, NULL );
      }
      else if( iFileCase == ZH_SET_CASE_UPPER )
      {
         if( pFileName->szName )
            pFileName->szName = pszName = zh_cdpnDupUpper( zh_vmCDP(), pFileName->szName, NULL );
         if( pFileName->szExtension )
            pFileName->szExtension = pszExt = zh_cdpnDupUpper( zh_vmCDP(), pFileName->szExtension, NULL );
      }

      /* DIRCASE */
      if( pFileName->szPath )
      {
         if( iDirCase == ZH_SET_CASE_LOWER )
            pFileName->szPath = pszPath = zh_cdpnDupLower( zh_vmCDP(), pFileName->szPath, NULL );
         else if( iDirCase == ZH_SET_CASE_UPPER )
            pFileName->szPath = pszPath = zh_cdpnDupUpper( zh_vmCDP(), pFileName->szPath, NULL );
      }

      zh_fsFNameMerge( ( char * ) ZH_UNCONST( pszFileName ), pFileName );
      zh_xfree( pFileName );
      if( pszPath )
         zh_xfree( pszPath );
      if( pszName )
         zh_xfree( pszName );
      if( pszExt )
         zh_xfree( pszExt );

      if( fEncodeCP )
      {
         const char * pszPrev = pszFileName;
         nLen = ZH_PATH_MAX;
         pszFileName = zh_osEncodeCP( pszFileName, pszFree, &nLen );
         if( pszFree == NULL && pszFileName != pszPrev )
         {
            zh_strncpy( ( char * ) ZH_UNCONST( pszPrev ), pszFileName, ZH_PATH_MAX - 1 );
            zh_xfree( ZH_UNCONST( pszFileName ) );
            pszFileName = pszPrev;
         }
      }
   }

   return pszFileName;
}

#if defined( ZH_OS_WIN )
ZH_WCHAR * zh_fsNameConvU16( const char * pszFileName )
{
   char * pszBuffer = NULL;
   ZH_WCHAR * lpwFileName;
   ZH_SIZE nLen;
   PZH_CODEPAGE cdp;
   int iFileCase, iDirCase;
   char cDirSep;
   ZH_BOOL fTrim;

/*
   Convert file and dir case. The allowed SET options are:
      LOWER - Convert all characters of file to lower
      UPPER - Convert all characters of file to upper
      MIXED - Leave as is

   The allowed environment options are:
      FILECASE - define the case of file
      DIRCASE - define the case of path
      DIRSEPARATOR - define separator of path (Ex. "/")
      TRIMFILENAME - strip trailing and leading spaces (also from extension)
 */

   if( ! zh_vmIsReady() )
      return zh_mbtowc( pszFileName );  /* No HVM stack */

   cdp = zh_vmCDP();
   fTrim = zh_setGetTrimFileName();
   cDirSep = ( char ) zh_setGetDirSeparator();
   iFileCase = zh_setGetFileCase();
   iDirCase = zh_setGetDirCase();
   if( fTrim )
   {
      if( strchr( pszFileName, ' ' ) == NULL )
         fTrim = ZH_FALSE;
   }
   if( cDirSep != ZH_OS_PATH_DELIM_CHR )
   {
      if( strchr( pszFileName, ( ZH_UCHAR ) cDirSep ) == NULL )
         cDirSep = ZH_OS_PATH_DELIM_CHR;
   }

   if( fTrim ||
       cDirSep != ZH_OS_PATH_DELIM_CHR ||
       iFileCase != ZH_SET_CASE_MIXED ||
       iDirCase != ZH_SET_CASE_MIXED )
   {
      char * pszPath = NULL, * pszName = NULL, * pszExt = NULL;
      PZH_FNAME pFileName;

      pszFileName = pszBuffer = zh_strncpy( ( char * ) zh_xgrab( ZH_PATH_MAX ),
                                            pszFileName, ZH_PATH_MAX - 1 );

      if( cDirSep != ZH_OS_PATH_DELIM_CHR )
      {
         char * p = pszBuffer;
         while( *p )
         {
            if( *p == cDirSep )
               *p = ZH_OS_PATH_DELIM_CHR;
            p++;
         }
      }

      pFileName = zh_fsFNameSplit( pszBuffer );

      /* strip trailing and leading spaces */
      if( fTrim )
      {
         if( pFileName->szName )
         {
            nLen = strlen( pFileName->szName );
            nLen = zh_strRTrimLen( pFileName->szName, nLen, ZH_FALSE );
            pFileName->szName = zh_strLTrim( pFileName->szName, &nLen );
            ( ( char * ) ZH_UNCONST( pFileName->szName ) )[ nLen ] = '\0';
         }
         if( pFileName->szExtension )
         {
            nLen = strlen( pFileName->szExtension );
            nLen = zh_strRTrimLen( pFileName->szExtension, nLen, ZH_FALSE );
            pFileName->szExtension = zh_strLTrim( pFileName->szExtension, &nLen );
            ( ( char * ) ZH_UNCONST( pFileName->szExtension ) )[ nLen ] = '\0';
         }
      }

      /* FILECASE */
      if( iFileCase == ZH_SET_CASE_LOWER )
      {
         if( pFileName->szName )
            pFileName->szName = pszName = zh_cdpnDupLower( cdp, pFileName->szName, NULL );
         if( pFileName->szExtension )
            pFileName->szExtension = pszExt = zh_cdpnDupLower( cdp, pFileName->szExtension, NULL );
      }
      else if( iFileCase == ZH_SET_CASE_UPPER )
      {
         if( pFileName->szName )
            pFileName->szName = pszName = zh_cdpnDupUpper( cdp, pFileName->szName, NULL );
         if( pFileName->szExtension )
            pFileName->szExtension = pszExt = zh_cdpnDupUpper( cdp, pFileName->szExtension, NULL );
      }

      /* DIRCASE */
      if( pFileName->szPath )
      {
         if( iDirCase == ZH_SET_CASE_LOWER )
            pFileName->szPath = pszPath = zh_cdpnDupLower( cdp, pFileName->szPath, NULL );
         else if( iDirCase == ZH_SET_CASE_UPPER )
            pFileName->szPath = pszPath = zh_cdpnDupUpper( cdp, pFileName->szPath, NULL );
      }

      zh_fsFNameMerge( pszBuffer, pFileName );
      zh_xfree( pFileName );
      if( pszPath )
         zh_xfree( pszPath );
      if( pszName )
         zh_xfree( pszName );
      if( pszExt )
         zh_xfree( pszExt );
   }

   lpwFileName = zh_cdpStrDupU16( cdp, ZH_CODEPAGE_ENDIAN_NATIVE, pszFileName );
   if( pszBuffer )
      zh_xfree( pszBuffer );

   return lpwFileName;
}
#endif /* ZH_OS_WIN */

/* NOTE: pszBuffer must be ZH_PATH_MAX long. */
void zh_fsBaseDirBuff( char * pszBuffer )
{
   char * pszBaseName = zh_cmdargProgName();

   if( pszBaseName )
   {
      PZH_FNAME pFileName = zh_fsFNameSplit( pszBaseName );
      pFileName->szName = NULL;
      pFileName->szExtension = NULL;
      zh_fsFNameMerge( pszBuffer, pFileName );
      zh_xfree( pFileName );
      zh_xfree( pszBaseName );
   }
   else
      pszBuffer[ 0 ] = '\0';
}

static ZH_BOOL zh_fsDisableWaitLocks( int iSet )
{
   ZH_BOOL fRetVal = s_fUseWaitLocks;

   if( iSet >= 0 )
      s_fUseWaitLocks = ( iSet == 0 );

   return fRetVal;
}

ZH_FUNC( ZH_DISABLEWAITLOCKS )
{
   zh_retl( zh_fsDisableWaitLocks( zh_parldef( 1, -1 ) ) );
}
