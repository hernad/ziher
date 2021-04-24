/*
 * Low-level functions to create, wait and terminate processes
 *
 * Copyright 2009 Przemyslaw Czerpak
 * based on xZiher code by
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
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
#include "zh_fs_api.h"
#include "zh_vm.h"

#if defined( ZH_OS_UNIX )
#  include <unistd.h>
#  include <sys/time.h>
#  include <sys/types.h>
#  include <sys/wait.h>
#  include <sys/stat.h>
#  include <fcntl.h>
#  include <signal.h>
#  include <errno.h>
#  if ! defined( ZH_HAS_POLL ) && ! defined( ZH_NO_POLL ) && \
      defined( _POSIX_C_SOURCE ) && _POSIX_C_SOURCE >= 200112L
      /* use poll() instead of select() to avoid FD_SETSIZE (1024 in Linux)
         file handle limit */
#     define ZH_HAS_POLL
#  endif
#  if defined( ZH_HAS_POLL )
#     include <poll.h>
#  endif
#elif defined( ZH_OS_WIN )
#  include <windows.h>
#  include "zh_win_unicode.h"
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




#if defined( ZH_PROCESS_USEFILES ) || defined( ZH_OS_UNIX )

/* convert command to argument list using standard Bourne shell encoding:
 * "" and '' can be used to group parameters with blank characters,
 * the escape character is '\', quoting by '' disables escape character.
 */
static char ** zh_buildArgs( const char *pszFileName )
{
   const char * src;
   char ** argv, * dst, cQuote = 0, * pszFree = NULL;
   int argc = 0;

   while( ZH_ISSPACE( *pszFileName ) )
      ++pszFileName;

   pszFileName = zh_osEncodeCP( pszFileName, &pszFree, NULL );
   dst = pszFree ? pszFree : zh_strdup( pszFileName );

   src = dst;
   while( *src )
   {
#if defined( ZH_OS_UNIX )
      if( *src == '\\' && cQuote != '\'' )
      {
         if( src[ 1 ] )
            ++src;
      }
      else
#endif
      if( *src == cQuote )
         cQuote = 0;
      else if( cQuote == 0 )
      {
#if defined( ZH_OS_UNIX )
         if( *src == '"' || *src == '\'' )
#else
         if( *src == '"' )
#endif
            cQuote = *src;
         else if( ZH_ISSPACE( *src ) )
         {
            while( ZH_ISSPACE( src[ 1 ] ) )
               ++src;
            if( src[ 1 ] )
               ++argc;
         }
      }
      ++src;
   }

   argv = ( char ** ) zh_xgrab( ( argc + 2 ) * sizeof( char * ) );
   argv[ 0 ] = dst;
   argv[ argc + 1 ] = NULL;
   argc = 0;

   cQuote = 0;
   src = dst;
   while( *src )
   {
#if defined( ZH_OS_UNIX )
      if( *src == '\\' && cQuote != '\'' )
      {
         if( src[ 1 ] )
         {
            *dst++ = src[ 1 ];
            ++src;
         }
      }
      else
#endif
      if( *src == cQuote )
         cQuote = 0;
      else if( cQuote != 0 )
         *dst++ = *src;
      else
      {
#if defined( ZH_OS_UNIX )
         if( *src == '"' || *src == '\'' )
#else
         if( *src == '"' )
#endif
            cQuote = *src;
         else if( ZH_ISSPACE( *src ) )
         {
            *dst++ = '\0';
            while( ZH_ISSPACE( src[ 1 ] ) )
               ++src;
            if( src[ 1 ] )
               argv[ ++argc ] = dst;
         }
         else
            *dst++ = *src;
      }
      ++src;
   }
   *dst = 0;

   return argv;
}

static void zh_freeArgs( char ** argv )
{
   zh_xfree( argv[ 0 ] );
   zh_xfree( argv );
}

#endif

#if defined( ZH_PROCESS_USEFILES )
static int zh_fsProcessExec( const char * pszFileName,
                             ZH_FHANDLE hStdin, ZH_FHANDLE hStdout,
                             ZH_FHANDLE hStderr )
{
   int iResult = FS_ERROR;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsProcessExec(%s, %p, %p, %p)", pszFileName, ( void * ) ( ZH_PTRUINT ) hStdin, ( void * ) ( ZH_PTRUINT ) hStdout, ( void * ) ( ZH_PTRUINT ) hStderr ) );

#if defined( ZH_OS_WIN ) || defined( ZH_OS_UNIX )
{
   int iStdIn, iStdOut, iStdErr;
   char ** argv;

   argv = zh_buildArgs( pszFileName );

   zh_vmUnlock();

   iStdIn = iStdOut = iStdErr = FS_ERROR;

   if( hStdin != FS_ERROR )
   {
      iStdIn  = dup( 0 );
      dup2( hStdin,  0 );
   }
   if( hStdout != FS_ERROR )
   {
      iStdOut = dup( 1 );
      dup2( hStdout, 1 );
   }
   if( hStderr != FS_ERROR )
   {
      iStdErr = dup( 2 );
      dup2( hStderr, 2 );
   }
#if defined( ZH_OS_UNIX )
   {
      pid_t pid = fork();
      if( pid == 0 )
      {
         /* close all non std* handles */
         {
            int iMaxFD, i;
            iMaxFD = sysconf( _SC_OPEN_MAX );
            if( iMaxFD < 3 )
               iMaxFD = 1024;
            for( i = 3; i < iMaxFD; ++i )
               zh_fsClose( i );
         }
         /* reset extended process attributes */
         ( void ) setuid( getuid() );
         ( void ) setgid( getgid() );

         /* execute command */
         execvp( argv[ 0 ], argv );
         exit( -1 );
      }
      else if( pid != -1 )
      {
         int iStatus;
         ZH_FAILURE_RETRY( iResult, waitpid( pid, &iStatus, 0 ) );
#ifdef ERESTARTSYS
         if( iResult < 0 && errno != ERESTARTSYS )
#else
         if( iResult < 0 )
#endif
            iResult = -2;
         else if( iResult == 0 )
            iResult = -1;
         else
            iResult = WIFEXITED( iStatus ) ? WEXITSTATUS( iStatus ) : 0;
      }
      else
         zh_fsSetIOError( ZH_FALSE, 0 );
   }
#else
#  if defined( _MSC_VER )
      iResult = _spawnvp( _P_WAIT, argv[ 0 ], argv );
#  else
      iResult = spawnvp( P_WAIT, argv[ 0 ], ( char * const * ) argv );
#  endif
   zh_fsSetIOError( iResult >= 0, 0 );
#endif

   if( iStdIn != FS_ERROR )
   {
      dup2( iStdIn,  0 );
      zh_fsCloseRaw( iStdIn );
   }
   if( iStdOut != FS_ERROR )
   {
      dup2( iStdOut, 1 );
      zh_fsCloseRaw( iStdOut );
   }
   if( iStdErr != FS_ERROR )
   {
      dup2( iStdErr, 2 );
      zh_fsCloseRaw( iStdErr );
   }

   zh_vmLock();
   zh_freeArgs( argv );
}
#else
{
   int iTODO; /* TODO: for given platform */

   ZH_SYMBOL_UNUSED( pszFileName );
   ZH_SYMBOL_UNUSED( hStdin );
   ZH_SYMBOL_UNUSED( hStdout );
   ZH_SYMBOL_UNUSED( hStderr );

   zh_fsSetError( ( ZH_ERRCODE ) FS_ERROR );
}
#endif

   return iResult;
}
#endif /* ZH_PROCESS_USEFILES */

ZH_FHANDLE zh_fsProcessOpen( const char * pszFileName,
                             ZH_FHANDLE * phStdin, ZH_FHANDLE * phStdout,
                             ZH_FHANDLE * phStderr,
                             ZH_BOOL fDetach, ZH_ULONG * pulPID )
{
   ZH_FHANDLE hPipeIn [ 2 ] = { FS_ERROR, FS_ERROR },
              hPipeOut[ 2 ] = { FS_ERROR, FS_ERROR },
              hPipeErr[ 2 ] = { FS_ERROR, FS_ERROR };
   ZH_FHANDLE hResult = FS_ERROR;
   ZH_BOOL fError = ZH_FALSE;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsProcessOpen(%s, %p, %p, %p, %d, %p)", pszFileName, ( void * ) phStdin, ( void * ) phStdout, ( void * ) phStderr, fDetach, ( void * ) pulPID ) );

   if( phStdin != NULL )
      fError = ! zh_fsPipeCreate( hPipeIn );
   if( ! fError && phStdout != NULL )
      fError = ! zh_fsPipeCreate( hPipeOut );
   if( ! fError && phStderr != NULL )
   {
      if( phStdout == phStderr )
      {
         hPipeErr[ 0 ] = hPipeOut[ 0 ];
         hPipeErr[ 1 ] = hPipeOut[ 1 ];
      }
      else
         fError = ! zh_fsPipeCreate( hPipeErr );
   }

   if( ! fError )
   {
#if defined( ZH_OS_WIN )

      PROCESS_INFORMATION pi;
      STARTUPINFO si;
      DWORD dwFlags = 0;
      LPTSTR lpCommand = ZH_CHARDUP( pszFileName );

      if( phStdin != NULL )
         SetHandleInformation( ( HANDLE ) zh_fsGetOsHandle( hPipeIn [ 1 ] ), HANDLE_FLAG_INHERIT, 0 );
      if( phStdout != NULL )
         SetHandleInformation( ( HANDLE ) zh_fsGetOsHandle( hPipeOut[ 0 ] ), HANDLE_FLAG_INHERIT, 0 );
      if( phStderr != NULL && phStdout != phStderr )
         SetHandleInformation( ( HANDLE ) zh_fsGetOsHandle( hPipeErr[ 0 ] ), HANDLE_FLAG_INHERIT, 0 );

      memset( &pi, 0, sizeof( pi ) );
      memset( &si, 0, sizeof( si ) );
      si.cb = sizeof( si );
#  ifdef STARTF_USESTDHANDLES
      si.dwFlags = STARTF_USESTDHANDLES;
#  endif
      if( fDetach )
      {
#  ifdef STARTF_USESHOWWINDOW
         si.dwFlags |= STARTF_USESHOWWINDOW;
#  endif
         si.wShowWindow = SW_HIDE;
         si.hStdInput  = ( HANDLE ) zh_fsGetOsHandle( hPipeIn [ 0 ] );
         si.hStdOutput = ( HANDLE ) zh_fsGetOsHandle( hPipeOut[ 1 ] );
         si.hStdError  = ( HANDLE ) zh_fsGetOsHandle( hPipeErr[ 1 ] );
#  ifdef DETACHED_PROCESS
         dwFlags |= DETACHED_PROCESS;
#  endif
      }
      else
      {
         si.hStdInput  = phStdin  ? ( HANDLE ) zh_fsGetOsHandle( hPipeIn [ 0 ] ) : GetStdHandle( STD_INPUT_HANDLE );
         si.hStdOutput = phStdout ? ( HANDLE ) zh_fsGetOsHandle( hPipeOut[ 1 ] ) : GetStdHandle( STD_OUTPUT_HANDLE );
         si.hStdError  = phStderr ? ( HANDLE ) zh_fsGetOsHandle( hPipeErr[ 1 ] ) : GetStdHandle( STD_ERROR_HANDLE );
      }
      fError = ! CreateProcess( NULL,           /* lpAppName */
                                lpCommand,
                                NULL,           /* lpProcessAttr */
                                NULL,           /* lpThreadAttr */
                                TRUE,           /* bInheritHandles */
                                dwFlags,        /* dwCreationFlags */
                                NULL,           /* lpEnvironment */
                                NULL,           /* lpCurrentDirectory */
                                &si,
                                &pi );
      zh_fsSetIOError( ! fError, 0 );
      zh_xfree( lpCommand );
      if( ! fError )
      {
         if( phStdin != NULL )
         {
            *phStdin = ( ZH_FHANDLE ) hPipeIn[ 1 ];
            hPipeIn[ 1 ] = FS_ERROR;
         }
         if( phStdout != NULL )
         {
            *phStdout = ( ZH_FHANDLE ) hPipeOut[ 0 ];
            hPipeOut[ 0 ] = FS_ERROR;
         }
         if( phStderr != NULL )
         {
            *phStderr = ( ZH_FHANDLE ) hPipeErr[ 0 ];
            hPipeErr[ 0 ] = FS_ERROR;
         }
         if( pulPID )
            *pulPID = pi.dwProcessId;
         CloseHandle( pi.hThread );
         hResult = ( ZH_FHANDLE ) pi.hProcess;
      }


#elif defined( ZH_OS_UNIX )

      char ** argv = zh_buildArgs( pszFileName );
      pid_t pid = fork();

      if( pid == -1 )
         fError = ZH_TRUE;
      else if( pid != 0 )    /* parent process */
      {
         if( phStdin != NULL )
         {
            *phStdin = ( ZH_FHANDLE ) hPipeIn[ 1 ];
            hPipeIn[ 1 ] = FS_ERROR;
         }
         if( phStdout != NULL )
         {
            *phStdout = ( ZH_FHANDLE ) hPipeOut[ 0 ];
            hPipeOut[ 0 ] = FS_ERROR;
         }
         if( phStderr != NULL )
         {
            *phStderr = ( ZH_FHANDLE ) hPipeErr[ 0 ];
            hPipeErr[ 0 ] = FS_ERROR;
         }
         if( pulPID )
            *pulPID = pid;
         hResult = ( ZH_FHANDLE ) pid;
      }
      else                    /* child process */
      {
         if( fDetach && ( ! phStdin || ! phStdout || ! phStderr ) )
         {
            ZH_FHANDLE hNull = open( "/dev/null", O_RDWR );

            if( ! phStdin )
               dup2( hNull, 0 );
            if( ! phStdout )
               dup2( hNull, 1 );
            if( ! phStderr )
               dup2( hNull, 2 );

            if( hNull != FS_ERROR )
               zh_fsClose( hNull );
         }

         if( phStdin != NULL )
         {
            dup2( hPipeIn[ 0 ], 0 );
            zh_fsClose( hPipeIn[ 1 ] );
         }
         if( phStdout != NULL )
         {
            dup2( hPipeOut[ 1 ], 1 );
            zh_fsClose( hPipeOut[ 0 ] );
         }
         if( phStderr != NULL )
         {
            dup2( hPipeErr[ 1 ], 2 );
            if( phStdout != phStderr )
               zh_fsClose( hPipeErr[ 0 ] );
         }

         /* close all non std* handles */
         {
            int iMaxFD, i;
            iMaxFD = sysconf( _SC_OPEN_MAX );
            if( iMaxFD < 3 )
               iMaxFD = 1024;
            for( i = 3; i < iMaxFD; ++i )
               zh_fsClose( i );
         }

         /* reset extended process attributes */
         if( setuid( getuid() ) == -1 ) {}
         if( setgid( getgid() ) == -1 ) {}

         /* execute command */
         {
            execvp( argv[ 0 ], argv );
            exit( -1 );
         }
      }
      zh_fsSetIOError( ! fError, 0 );

      zh_freeArgs( argv );

#elif defined( ZH_OS_WIN )

      int hStdIn, hStdOut, hStdErr;
      char ** argv;
      int pid;

      hStdIn  = dup( 0 );
      hStdOut = dup( 1 );
      hStdErr = dup( 2 );

      if( fDetach && ( ! phStdin || ! phStdout || ! phStderr ) )
      {
         ZH_FHANDLE hNull = open( "NUL:", O_RDWR );

         if( ! phStdin )
            dup2( hNull, 0 );
         if( ! phStdout )
            dup2( hNull, 1 );
         if( ! phStderr )
            dup2( hNull, 2 );

         if( hNull != FS_ERROR )
            close( hNull );
      }

      if( phStdin != NULL )
         dup2( hPipeIn[ 0 ], 0 );
      if( phStdout != NULL )
         dup2( hPipeOut[ 1 ], 1 );
      if( phStderr != NULL )
         dup2( hPipeErr[ 1 ], 2 );

      argv = zh_buildArgs( pszFileName );

#if defined( _MSC_VER )
      pid = _spawnvp( _P_NOWAIT, argv[ 0 ], argv );
#elif defined( __MINGW32__ )
      pid = spawnvp( P_NOWAIT, argv[ 0 ], ( const char * const * ) argv );
#else
      pid = spawnvp( P_NOWAIT, argv[ 0 ], ( char * const * ) argv );
#endif
      zh_freeArgs( argv );

      dup2( hStdIn,  0 );
      close( hStdIn );

      dup2( hStdOut, 1 );
      close( hStdOut );

      dup2( hStdErr, 2 );
      close( hStdErr );

      if( pid < 0 )
         fError = ZH_TRUE;
      else if( pid != 0 )    /* parent process */
      {
         if( phStdin != NULL )
         {
            *phStdin = ( ZH_FHANDLE ) hPipeIn[ 1 ];
            hPipeIn[ 1 ] = FS_ERROR;
         }
         if( phStdout != NULL )
         {
            *phStdout = ( ZH_FHANDLE ) hPipeOut[ 0 ];
            hPipeOut[ 0 ] = FS_ERROR;
         }
         if( phStderr != NULL )
         {
            *phStderr = ( ZH_FHANDLE ) hPipeErr[ 0 ];
            hPipeErr[ 0 ] = FS_ERROR;
         }
         if( pulPID )
            *pulPID = pid;
         hResult = ( ZH_FHANDLE ) pid;
      }

      zh_fsSetIOError( ! fError, 0 );

#else
      int iTODO; /* TODO: for given platform */

      ZH_SYMBOL_UNUSED( pszFileName );
      ZH_SYMBOL_UNUSED( fDetach );
      ZH_SYMBOL_UNUSED( pulPID );

      zh_fsSetError( ( ZH_ERRCODE ) FS_ERROR );
#endif
   }

   if( hPipeIn[ 0 ] != FS_ERROR )
      zh_fsCloseRaw( hPipeIn[ 0 ] );
   if( hPipeIn[ 1 ] != FS_ERROR )
      zh_fsCloseRaw( hPipeIn[ 1 ] );
   if( hPipeOut[ 0 ] != FS_ERROR )
      zh_fsCloseRaw( hPipeOut[ 0 ] );
   if( hPipeOut[ 1 ] != FS_ERROR )
      zh_fsCloseRaw( hPipeOut[ 1 ] );
   if( phStdout != phStderr )
   {
      if( hPipeErr[ 0 ] != FS_ERROR )
         zh_fsCloseRaw( hPipeErr[ 0 ] );
      if( hPipeErr[ 1 ] != FS_ERROR )
         zh_fsCloseRaw( hPipeErr[ 1 ] );
   }

   return hResult;
}

int zh_fsProcessValue( ZH_FHANDLE hProcess, ZH_BOOL fWait )
{
   int iRetStatus = -1;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsProcessValue(%p, %d)", ( void * ) ( ZH_PTRUINT ) hProcess, fWait ) );

#if defined( ZH_OS_WIN )
{
   ZH_BOOL fError = ZH_TRUE;
   DWORD dwResult;
   HANDLE hProc = ( HANDLE ) zh_fsGetOsHandle( hProcess );

   if( hProc )
   {
      zh_vmUnlock();
      dwResult = WaitForSingleObject( hProc, fWait ? INFINITE : 0 );
      if( dwResult == WAIT_OBJECT_0 )
      {
         fError = ! GetExitCodeProcess( hProc, &dwResult );
         iRetStatus = ! fError ? ( int ) dwResult : -2;
      }
      zh_fsSetIOError( ! fError, 0 );
      if( ! fError )
         CloseHandle( hProc );
      zh_vmLock();
   }
   else
      zh_fsSetError( ( ZH_ERRCODE ) FS_ERROR );
}
#elif defined( ZH_OS_UNIX )
{
   int iStatus;
   pid_t pid = ( pid_t ) hProcess;

   if( pid > 0 )
   {
      zh_vmUnlock();
      ZH_FAILURE_RETRY( iRetStatus, waitpid( pid, &iStatus, fWait ? 0 : WNOHANG ) );
#ifdef ERESTARTSYS
      if( iRetStatus < 0 && zh_fsOsError() != ( ZH_ERRCODE ) ERESTARTSYS )
#else
      if( iRetStatus < 0 )
#endif
         iRetStatus = -2;
      else if( iRetStatus == 0 )
         iRetStatus = -1;
      else
         iRetStatus = WIFEXITED( iStatus ) ? WEXITSTATUS( iStatus ) : 0;
      zh_vmLock();
   }
   else
      zh_fsSetError( ( ZH_ERRCODE ) FS_ERROR );
}
#else
{
   int iTODO; /* TODO: for given platform */

   ZH_SYMBOL_UNUSED( hProcess );
   ZH_SYMBOL_UNUSED( fWait );
   zh_fsSetError( ( ZH_ERRCODE ) FS_ERROR );
}
#endif
   return iRetStatus;
}

/* Closes/kills process. The handle is still valid until you
 * catch it with zh_fsProcessValue.
 */
ZH_BOOL zh_fsProcessClose( ZH_FHANDLE hProcess, ZH_BOOL fGentle )
{
   ZH_BOOL fResult = ZH_FALSE;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsProcessClose(%p, %d)", ( void * ) ( ZH_PTRUINT ) hProcess, fGentle ) );

#if defined( ZH_OS_WIN )
{
   HANDLE hProc = ( HANDLE ) zh_fsGetOsHandle( hProcess );

   if( hProc )
   {
      fResult = TerminateProcess( hProc, fGentle ? 0 : 1 ) != 0;
      zh_fsSetIOError( fResult, 0 );
      /* hProc has to be closed by zh_fsProcessValue() */
      #if 0
      CloseHandle( hProc );
      #endif
   }
   else
      zh_fsSetError( ( ZH_ERRCODE ) FS_ERROR );
}
#elif defined( ZH_OS_UNIX )
{
   pid_t pid = ( pid_t ) hProcess;
   if( pid > 0 )
   {
      fResult = kill( pid, fGentle ? SIGTERM : SIGKILL ) == 0;
      zh_fsSetIOError( fResult, 0 );
   }
   else
      zh_fsSetError( ( ZH_ERRCODE ) FS_ERROR );
}
#else
{
   int iTODO; /* TODO: for given platform */

   ZH_SYMBOL_UNUSED( hProcess );
   ZH_SYMBOL_UNUSED( fGentle );
   zh_fsSetError( ( ZH_ERRCODE ) FS_ERROR );
}
#endif
   return fResult;
}

#define ZH_STD_BUFFER_SIZE    4096

int zh_fsProcessRun( const char * pszFileName,
                     const char * pStdInBuf, ZH_SIZE nStdInLen,
                     char ** pStdOutPtr, ZH_SIZE * pulStdOut,
                     char ** pStdErrPtr, ZH_SIZE * pulStdErr,
                     ZH_BOOL fDetach )
{
   ZH_FHANDLE hStdin, hStdout, hStderr, *phStdin, *phStdout, *phStderr;
   char * pOutBuf, *pErrBuf;
   ZH_SIZE nOutSize, nErrSize, nOutBuf, nErrBuf;
   int iResult;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsProcessRun(%s, %p, %" ZH_PFS "u, %p, %p, %p, %p, %d)", pszFileName, ( const void * ) pStdInBuf, nStdInLen, ( void * ) pStdOutPtr, ( void * ) pulStdOut, ( void * ) pStdErrPtr, ( void * ) pulStdErr, fDetach ) );

   nOutBuf = nErrBuf = nOutSize = nErrSize = 0;
   pOutBuf = pErrBuf = NULL;
   hStdin = hStdout = hStderr = FS_ERROR;
   phStdin = pStdInBuf ? &hStdin : NULL;
   phStdout = pStdOutPtr && pulStdOut ? &hStdout : NULL;
   phStderr = pStdErrPtr && pulStdErr ?
              ( pStdOutPtr == pStdErrPtr ? phStdout : &hStderr ) : NULL;

#if defined( ZH_PROCESS_USEFILES )
{


#if defined( ZH_OS_UNIX )
#  define _ZH_NULLHANDLE()    open( "/dev/null", O_RDWR )
#else
#  define _ZH_NULLHANDLE()    open( "NUL:", O_RDWR )
#endif
   char sTmpIn[ ZH_PATH_MAX ];
   char sTmpOut[ ZH_PATH_MAX ];
   char sTmpErr[ ZH_PATH_MAX ];

   ZH_SYMBOL_UNUSED( phStdin );
   ZH_SYMBOL_UNUSED( nOutSize );
   ZH_SYMBOL_UNUSED( nErrSize );

   sTmpIn[ 0 ] = sTmpOut[ 0 ] = sTmpErr[ 0 ] = '\0';
   if( pStdInBuf )
   {
      hStdin = zh_fsCreateTempEx( sTmpIn, NULL, NULL, NULL, FC_NORMAL );
      if( nStdInLen )
      {
         zh_fsWriteLarge( hStdin, pStdInBuf, nStdInLen );
         zh_fsSeek( hStdin, 0, FS_SET );
      }
   }
   else if( fDetach )
      hStdin = _ZH_NULLHANDLE();

   if( pStdOutPtr && pulStdOut )
      hStdout = zh_fsCreateTempEx( sTmpOut, NULL, NULL, NULL, FC_NORMAL );
   else if( fDetach )
      hStdout = _ZH_NULLHANDLE();

   if( pStdErrPtr && pulStdErr )
   {
      if( phStdout == phStderr )
         hStderr = hStdout;
      else
         hStderr = zh_fsCreateTempEx( sTmpErr, NULL, NULL, NULL, FC_NORMAL );
   }
   else if( fDetach )
      hStderr = _ZH_NULLHANDLE();

   iResult = zh_fsProcessExec( pszFileName, hStdin, hStdout, hStderr );

   if( hStdin != FS_ERROR )
   {
      zh_fsClose( hStdin );
      if( sTmpIn[ 0 ] )
         zh_fsDelete( sTmpIn );
   }
   if( hStdout != FS_ERROR )
   {
      if( pStdOutPtr && pulStdOut )
      {
         nOutBuf = zh_fsSeek( hStdout, 0, FS_END );
         if( nOutBuf )
         {
            pOutBuf = ( char * ) zh_xgrab( nOutBuf + 1 );
            zh_fsSeek( hStdout, 0, FS_SET );
            nOutBuf = zh_fsReadLarge( hStdout, pOutBuf, nOutBuf );
         }
      }
      zh_fsClose( hStdout );
      if( sTmpOut[ 0 ] )
         zh_fsDelete( sTmpOut );
   }
   if( hStderr != FS_ERROR && hStderr != hStdout )
   {
      if( pStdErrPtr && pulStdErr )
      {
         nErrBuf = zh_fsSeek( hStderr, 0, FS_END );
         if( nErrBuf )
         {
            pErrBuf = ( char * ) zh_xgrab( nErrBuf + 1 );
            zh_fsSeek( hStderr, 0, FS_SET );
            nErrBuf = zh_fsReadLarge( hStderr, pErrBuf, nErrBuf );
         }
      }
      zh_fsClose( hStderr );
      if( sTmpErr[ 0 ] )
         zh_fsDelete( sTmpErr );
   }
}

#else /* ! ZH_PROCESS_USEFILES */
{
   ZH_FHANDLE hProcess;

   zh_vmUnlock();

   iResult = -1;
   hProcess = zh_fsProcessOpen( pszFileName, phStdin, phStdout, phStderr,
                                fDetach, NULL );
   if( hProcess != FS_ERROR )
   {
#if defined( ZH_OS_WIN )

      ZH_BOOL fFinished = ZH_FALSE, fBlocked;
      int iPipeCount = 0;

      if( nStdInLen == 0 && hStdin != FS_ERROR )
      {
         zh_fsClose( hStdin );
         hStdin = FS_ERROR;
      }
      if( hStdout == hStderr )
         hStderr = FS_ERROR;

      if( hStdin != FS_ERROR )
         ++iPipeCount;
      if( hStdout != FS_ERROR )
         ++iPipeCount;
      if( hStderr != FS_ERROR )
         ++iPipeCount;

      fBlocked = iPipeCount <= 1;
      if( ! fBlocked )
      {
         if( hStdin != FS_ERROR )
            zh_fsPipeUnblock( hStdin );
         if( hStdout != FS_ERROR )
            zh_fsPipeUnblock( hStdout );
         if( hStderr != FS_ERROR )
            zh_fsPipeUnblock( hStderr );
      }

      for( ;; )
      {
         DWORD dwResult, dwWait;
         ZH_SIZE nLen;

         dwWait = 1000;

         if( hStdout != FS_ERROR )
         {
            if( nOutBuf == nOutSize )
            {
               if( nOutSize == 0 )
                  nOutSize = ZH_STD_BUFFER_SIZE;
               else
                  nOutSize += nOutSize >> 1;
               pOutBuf = ( char * ) zh_xrealloc( pOutBuf, nOutSize + 1 );
            }
            nLen = zh_fsReadLarge( hStdout, pOutBuf + nOutBuf, nOutSize - nOutBuf );
            if( nLen > 0 )
               nOutBuf += nLen;
            else if( fBlocked )
            {
               zh_fsClose( hStdout );
               hStdout = FS_ERROR;
               --iPipeCount;
            }
            dwWait = nLen > 0 ? 0 : 10;
         }

         if( hStderr != FS_ERROR )
         {
            if( nErrBuf == nErrSize )
            {
               if( nErrSize == 0 )
                  nErrSize = ZH_STD_BUFFER_SIZE;
               else
                  nErrSize += nErrSize >> 1;
               pErrBuf = ( char * ) zh_xrealloc( pErrBuf, nErrSize + 1 );
            }
            nLen = zh_fsReadLarge( hStderr, pErrBuf + nErrBuf, nErrSize - nErrBuf );
            if( nLen > 0 )
               nErrBuf += nLen;
            else if( fBlocked )
            {
               zh_fsClose( hStderr );
               hStderr = FS_ERROR;
               --iPipeCount;
            }
            if( dwWait )
               dwWait = nLen > 0 ? 0 : 10;
         }

         if( fFinished )
         {
            if( dwWait != 0 )
               break;
         }
         else if( hStdin != FS_ERROR )
         {
            nLen = ! fBlocked && nStdInLen > 4096 ? 4096 : nStdInLen;
            nLen = zh_fsWriteLarge( hStdin, pStdInBuf, nLen );
            pStdInBuf += nLen;
            nStdInLen -= nLen;
            if( nStdInLen == 0 || ( fBlocked && nLen == 0 ) )
            {
               zh_fsClose( hStdin );
               hStdin = FS_ERROR;
               --iPipeCount;
            }
            else if( dwWait )
               dwWait = nLen > 0 ? 0 : 10;
         }

         if( iPipeCount == 0 )
            dwWait = INFINITE;
         dwResult = WaitForSingleObject( ( HANDLE ) zh_fsGetOsHandle( hProcess ), dwWait );
         if( dwResult == WAIT_OBJECT_0 )
         {
            if( GetExitCodeProcess( ( HANDLE ) zh_fsGetOsHandle( hProcess ), &dwResult ) )
               iResult = ( int ) dwResult;
            else
               iResult = -2;
            fFinished = ZH_TRUE;
         }
      }

      if( hStdin != FS_ERROR )
         zh_fsClose( hStdin );
      if( hStdout != FS_ERROR )
         zh_fsClose( hStdout );
      if( hStderr != FS_ERROR )
         zh_fsClose( hStderr );

      CloseHandle( ( HANDLE ) zh_fsGetOsHandle( hProcess ) );

#elif defined( ZH_OS_WIN )

      ZH_MAXINT nTimeOut = 0;
      int iPipeCount = 0;

      if( nStdInLen == 0 && hStdin != FS_ERROR )
      {
         zh_fsClose( hStdin );
         hStdin = FS_ERROR;
      }
      if( hStdout == hStderr )
         hStderr = FS_ERROR;

      if( hStdin != FS_ERROR )
         ++iPipeCount;
      if( hStdout != FS_ERROR )
         ++iPipeCount;
      if( hStderr != FS_ERROR )
         ++iPipeCount;

      while( iPipeCount > 0 )
      {
         ZH_MAXINT nNextTOut = 10;
         ZH_SIZE nLen;

         if( hStdin != FS_ERROR )
         {
            if( iPipeCount == 1 )
               nLen = zh_fsWriteLarge( hStdin, pStdInBuf, nStdInLen );
            else
               nLen = zh_fsPipeWrite( hStdin, pStdInBuf, nStdInLen, nTimeOut );
            if( nLen == ( ZH_SIZE ) ( iPipeCount == 1 ? 0 : FS_ERROR ) )
               nStdInLen = 0;
            else if( nLen > 0 )
            {
               pStdInBuf += nLen;
               nStdInLen -= nLen;
               nNextTOut = 0;
            }
            if( nStdInLen == 0 )
            {
               zh_fsClose( hStdin );
               hStdin = FS_ERROR;
               --iPipeCount;
            }
         }

         if( hStdout != FS_ERROR )
         {
            if( nOutBuf == nOutSize )
            {
               if( nOutSize == 0 )
                  nOutSize = ZH_STD_BUFFER_SIZE;
               else
                  nOutSize += nOutSize >> 1;
               pOutBuf = ( char * ) zh_xrealloc( pOutBuf, nOutSize + 1 );
            }
            if( iPipeCount == 1 )
               nLen = zh_fsReadLarge( hStdout, pOutBuf + nOutBuf, nOutSize - nOutBuf );
            else
               nLen = zh_fsPipeRead( hStdout, pOutBuf + nOutBuf, nOutSize - nOutBuf, nTimeOut );
            if( nLen == ( ZH_SIZE ) ( iPipeCount == 1 ? 0 : FS_ERROR ) )
            {
               zh_fsClose( hStdout );
               hStdout = FS_ERROR;
               --iPipeCount;
            }
            else if( nLen > 0 )
            {
               nOutBuf += nLen;
               nNextTOut = 0;
            }
         }

         if( hStderr != FS_ERROR )
         {
            if( nErrBuf == nErrSize )
            {
               if( nErrSize == 0 )
                  nErrSize = ZH_STD_BUFFER_SIZE;
               else
                  nErrSize += nErrSize >> 1;
               pErrBuf = ( char * ) zh_xrealloc( pErrBuf, nErrSize + 1 );
            }
            if( iPipeCount == 1 )
               nLen = zh_fsReadLarge( hStderr, pErrBuf + nErrBuf, nErrSize - nErrBuf );
            else
               nLen = zh_fsPipeRead( hStderr, pErrBuf + nErrBuf, nErrSize - nErrBuf, nTimeOut );
            if( nLen == ( ZH_SIZE ) ( iPipeCount == 1 ? 0 : FS_ERROR ) )
            {
               zh_fsClose( hStderr );
               hStderr = FS_ERROR;
               --iPipeCount;
            }
            else if( nLen > 0 )
            {
               nErrBuf += nLen;
               nNextTOut = 0;
            }
         }

         nTimeOut = nNextTOut;
      }

      if( hStdin != FS_ERROR )
         zh_fsClose( hStdin );
      if( hStdout != FS_ERROR )
         zh_fsClose( hStdout );
      if( hStderr != FS_ERROR )
         zh_fsClose( hStderr );

      iResult = zh_fsProcessValue( hProcess, ZH_TRUE );

#elif defined( ZH_OS_UNIX )

      if( nStdInLen == 0 && hStdin != FS_ERROR )
      {
         zh_fsClose( hStdin );
         hStdin = FS_ERROR;
      }
      if( hStdout == hStderr )
         hStderr = FS_ERROR;

      if( hStdin != FS_ERROR )
         zh_fsPipeUnblock( hStdin );
      if( hStdout != FS_ERROR )
         zh_fsPipeUnblock( hStdout );
      if( hStderr != FS_ERROR )
         zh_fsPipeUnblock( hStderr );

      for( ;; )
      {
         ZH_BOOL fStdout, fStderr, fStdin;
         ZH_SIZE nLen;

#if defined( ZH_HAS_POLL )
         {
            struct pollfd fds[ 3 ];
            nfds_t nfds = 0;

            if( hStdout != FS_ERROR )
            {
               fds[ nfds ].fd = hStdout;
               fds[ nfds ].events = POLLIN;
               fds[ nfds++ ].revents = 0;
            }
            if( hStderr != FS_ERROR )
            {
               fds[ nfds ].fd = hStderr;
               fds[ nfds ].events = POLLIN;
               fds[ nfds++ ].revents = 0;
            }
            if( hStdin != FS_ERROR )
            {
               fds[ nfds ].fd = hStdin;
               fds[ nfds ].events = POLLOUT;
               fds[ nfds++ ].revents = 0;
            }
            if( nfds == 0 )
               break;

            iResult = poll( fds, nfds, -1 );
            zh_fsSetIOError( iResult >= 0, 0 );
            if( iResult == -1 && zh_fsOsError() == ( ZH_ERRCODE ) EINTR &&
                zh_vmRequestQuery() == 0 )
               continue;
            else if( iResult <= 0 )
               break;

            nfds = 0;
            fStdout = fStderr = fStdin = ZH_FALSE;
            if( hStdout != FS_ERROR )
            {
               if( ( fds[ nfds ].revents & POLLIN ) != 0 )
                  fStdout = ZH_TRUE;
               else if( ( fds[ nfds ].revents & ( POLLHUP | POLLNVAL | POLLERR ) ) != 0 )
               {
                  zh_fsClose( hStdout );
                  hStdout = FS_ERROR;
               }
               nfds++;
            }
            if( hStderr != FS_ERROR )
            {
               if( ( fds[ nfds ].revents & POLLIN ) != 0 )
                  fStderr = ZH_TRUE;
               else if( ( fds[ nfds ].revents & ( POLLHUP | POLLNVAL | POLLERR ) ) != 0 )
               {
                  zh_fsClose( hStderr );
                  hStderr = FS_ERROR;
               }
               nfds++;
            }
            if( hStdin != FS_ERROR )
            {
               if( ( fds[ nfds ].revents & POLLOUT ) != 0 )
                  fStdin = ZH_TRUE;
               else if( ( fds[ nfds ].revents & ( POLLHUP | POLLNVAL | POLLERR ) ) != 0 )
               {
                  zh_fsClose( hStdin );
                  hStderr = FS_ERROR;
               }
            }
         }
#else /* ! ZH_HAS_POLL */
         {
            fd_set rfds, wfds, *prfds, *pwfds;
            ZH_FHANDLE fdMax;

            fdMax = 0;
            prfds = pwfds = NULL;
            if( hStdout != FS_ERROR || hStderr != FS_ERROR )
            {
               FD_ZERO( &rfds );
               if( hStdout != FS_ERROR )
               {
                  FD_SET( hStdout, &rfds );
                  if( hStdout > fdMax )
                     fdMax = hStdout;
               }
               if( hStderr != FS_ERROR )
               {
                  FD_SET( hStderr, &rfds );
                  if( hStderr > fdMax )
                     fdMax = hStderr;
               }
               prfds = &rfds;
            }
            if( hStdin != FS_ERROR )
            {
               FD_ZERO( &wfds );
               FD_SET( hStdin, &wfds );
               if( hStdin > fdMax )
                  fdMax = hStdin;
               pwfds = &wfds;
            }
            if( prfds == NULL && pwfds == NULL )
               break;

            iResult = select( fdMax + 1, prfds, pwfds, NULL, NULL );
            zh_fsSetIOError( iResult >= 0, 0 );
            if( iResult == -1 && zh_fsOsError() != ( ZH_ERRCODE ) EINTR &&
                zh_vmRequestQuery() == 0 )
               continue;
            else if( iResult <= 0 )
               break;
            fStdout = hStdout != FS_ERROR && FD_ISSET( hStdout, &rfds );
            fStderr = hStderr != FS_ERROR && FD_ISSET( hStderr, &rfds );
            fStdin = hStdin != FS_ERROR && FD_ISSET( hStdin, &wfds );
         }
#endif /* ! ZH_HAS_POLL */

         if( fStdout )
         {
            if( nOutBuf == nOutSize )
            {
               if( nOutSize == 0 )
                  nOutSize = ZH_STD_BUFFER_SIZE;
               else
                  nOutSize += nOutSize >> 1;
               pOutBuf = ( char * ) zh_xrealloc( pOutBuf, nOutSize + 1 );
            }
            nLen = zh_fsReadLarge( hStdout, pOutBuf + nOutBuf, nOutSize - nOutBuf );
            if( nLen == 0 )
            {
               /* zero bytes read after positive Select()
                * - writing process closed the pipe
                */
               zh_fsClose( hStdout );
               hStdout = FS_ERROR;
            }
            else
               nOutBuf += nLen;
         }

         if( fStderr )
         {
            if( nErrBuf == nErrSize )
            {
               if( nErrSize == 0 )
                  nErrSize = ZH_STD_BUFFER_SIZE;
               else
                  nErrSize += nErrSize >> 1;
               pErrBuf = ( char * ) zh_xrealloc( pErrBuf, nErrSize + 1 );
            }
            nLen = zh_fsReadLarge( hStderr, pErrBuf + nErrBuf, nErrSize - nErrBuf );
            if( nLen == 0 )
            {
               /* zero bytes read after positive Select()
                * - writing process closed the pipe
                */
               zh_fsClose( hStderr );
               hStderr = FS_ERROR;
            }
            else
               nErrBuf += nLen;
         }

         if( fStdin )
         {
            nLen = zh_fsWriteLarge( hStdin, pStdInBuf, nStdInLen );
            pStdInBuf += nLen;
            nStdInLen -= nLen;
            if( nStdInLen == 0 )
            {
               zh_fsClose( hStdin );
               hStdin = FS_ERROR;
            }
         }
      }

      if( hStdin != FS_ERROR )
         zh_fsClose( hStdin );
      if( hStdout != FS_ERROR )
         zh_fsClose( hStdout );
      if( hStderr != FS_ERROR )
         zh_fsClose( hStderr );

      iResult = zh_fsProcessValue( hProcess, ZH_TRUE );

#else

      int iTODO;

      ZH_SYMBOL_UNUSED( nStdInLen );
      ZH_SYMBOL_UNUSED( nOutSize );
      ZH_SYMBOL_UNUSED( nErrSize );

#endif
   }
   zh_vmLock();
}
#endif /* ! ZH_PROCESS_USEFILES */

   if( phStdout )
   {
      *pStdOutPtr = pOutBuf;
      *pulStdOut = nOutBuf;
   }
   if( phStderr && phStdout != phStderr )
   {
      *pStdErrPtr = pErrBuf;
      *pulStdErr = nErrBuf;
   }

   return iResult;
}
