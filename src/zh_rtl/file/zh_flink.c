/*
 * zh_fsLink*(), zh_FLink*() functions
 *
 * Copyright 2010 Viktor Szakats
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

#if defined( ZH_OS_WIN )
   #include <windows.h>
   #if ! defined( INVALID_FILE_ATTRIBUTES )
      #define INVALID_FILE_ATTRIBUTES  ( ( DWORD ) -1 )
   #endif
   #include "zh_win_uni.h"
#elif defined( ZH_OS_UNIX )
   #include <unistd.h>
#endif

ZH_BOOL zh_fsLink( const char * pszExisting, const char * pszNewFile )
{
   ZH_BOOL fResult;

   if( pszExisting && pszNewFile )
   {
      zh_vmUnlock();

#if defined( ZH_OS_WIN )
      {
         typedef BOOL ( WINAPI * _ZH_CREATEHARDLINK )( LPCTSTR, LPCTSTR, LPSECURITY_ATTRIBUTES );

         static _ZH_CREATEHARDLINK s_pCreateHardLink = ( _ZH_CREATEHARDLINK ) -1;

         if( s_pCreateHardLink == ( _ZH_CREATEHARDLINK ) -1 )
         {
            HMODULE hModule = GetModuleHandle( TEXT( "kernel32.dll" ) );
            if( hModule )
               s_pCreateHardLink = ( _ZH_CREATEHARDLINK )
                  ZH_WINAPI_GETPROCADDRESST( hModule, "CreateHardLink" );
            else
               s_pCreateHardLink = NULL;
         }

         if( s_pCreateHardLink )
         {
            LPCTSTR lpFileName, lpExistingFileName;
            LPTSTR lpFileNameFree, lpExistingFileNameFree;

            lpFileName = ZH_FSNAMECONV( pszNewFile, &lpFileNameFree );
            lpExistingFileName = ZH_FSNAMECONV( pszExisting, &lpExistingFileNameFree );

            fResult = s_pCreateHardLink( lpFileName, lpExistingFileName, NULL ) != 0;
            zh_fsSetIOError( fResult, 0 );

            if( lpFileNameFree )
               zh_xfree( lpFileNameFree );
            if( lpExistingFileNameFree )
               zh_xfree( lpExistingFileNameFree );
         }
         else
         {
            zh_fsSetError( 1 );
            fResult = ZH_FALSE;
         }
      }
#elif defined( ZH_OS_UNIX )
      {
         char * pszExistingFree;
         char * pszNewFileFree;

         pszExisting = zh_fsNameConv( pszExisting, &pszExistingFree );
         pszNewFile = zh_fsNameConv( pszNewFile, &pszNewFileFree );

         fResult = ( link( pszExisting, pszNewFile ) == 0 );
         zh_fsSetIOError( fResult, 0 );

         if( pszExistingFree )
            zh_xfree( pszExistingFree );
         if( pszNewFileFree )
            zh_xfree( pszNewFileFree );
      }
#else
      {
         zh_fsSetError( 1 );
         fResult = ZH_FALSE;
      }
#endif

      zh_vmLock();
   }
   else
   {
      zh_fsSetError( 2 );
      fResult = ZH_FALSE;
   }

   return fResult;
}

ZH_BOOL zh_fsLinkSym( const char * pszTarget, const char * pszNewFile )
{
   ZH_BOOL fResult;

   if( pszTarget && pszNewFile )
   {
      zh_vmUnlock();

#if defined( ZH_OS_WIN ) && ! defined( ZH_OS_WIN_CE )
      {
         typedef BOOL ( WINAPI * _ZH_CREATESYMBOLICLINK )( LPCTSTR, LPCTSTR, DWORD );

         static _ZH_CREATESYMBOLICLINK s_pCreateSymbolicLink = ( _ZH_CREATESYMBOLICLINK ) -1;

         #ifndef SYMBOLIC_LINK_FLAG_DIRECTORY
         #define SYMBOLIC_LINK_FLAG_DIRECTORY 0x1
         #endif
         /* Requires Windows 10 Insiders Build 14972 or newer
            https://blogs.windows.com/buildingapps/2016/12/02/symlinks-windows-10/ */
         #ifndef SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE
         #define SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE 0x2
         #endif

         if( s_pCreateSymbolicLink == ( _ZH_CREATESYMBOLICLINK ) -1 )
         {
            HMODULE hModule = GetModuleHandle( TEXT( "kernel32.dll" ) );
            if( hModule )
               s_pCreateSymbolicLink = ( _ZH_CREATESYMBOLICLINK )
                  ZH_WINAPI_GETPROCADDRESST( hModule, "CreateSymbolicLink" );
            else
               s_pCreateSymbolicLink = NULL;
         }

         if( s_pCreateSymbolicLink )
         {
            LPCTSTR lpSymlinkFileName, lpTargetFileName;
            LPTSTR lpSymlinkFileNameFree, lpTargetFileNameFree;
            DWORD dwAttr;
            ZH_BOOL fDir;

            lpSymlinkFileName = ZH_FSNAMECONV( pszNewFile, &lpSymlinkFileNameFree );
            lpTargetFileName = ZH_FSNAMECONV( pszTarget, &lpTargetFileNameFree );

            dwAttr = GetFileAttributes( lpTargetFileName );
            fDir = ( dwAttr != INVALID_FILE_ATTRIBUTES ) &&
                   ( dwAttr & FILE_ATTRIBUTE_DIRECTORY );

            fResult = s_pCreateSymbolicLink( lpSymlinkFileName, lpTargetFileName,
               ( fDir ? SYMBOLIC_LINK_FLAG_DIRECTORY : 0 ) | SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE ) != 0;
            zh_fsSetIOError( fResult, 0 );

            if( lpSymlinkFileNameFree )
               zh_xfree( lpSymlinkFileNameFree );
            if( lpTargetFileNameFree )
               zh_xfree( lpTargetFileNameFree );
         }
         else
         {
            zh_fsSetError( 1 );
            fResult = ZH_FALSE;
         }
      }
#elif defined( ZH_OS_UNIX )
      {
         char * pszTargetFree;
         char * pszNewFileFree;

         pszTarget = zh_fsNameConv( pszTarget, &pszTargetFree );
         pszNewFile = zh_fsNameConv( pszNewFile, &pszNewFileFree );

         fResult = ( symlink( pszTarget, pszNewFile ) == 0 );
         zh_fsSetIOError( fResult, 0 );

         if( pszTargetFree )
            zh_xfree( pszTargetFree );
         if( pszNewFileFree )
            zh_xfree( pszNewFileFree );
      }
#else
      {
         zh_fsSetError( 1 );
         fResult = ZH_FALSE;
      }
#endif

      zh_vmLock();
   }
   else
   {
      zh_fsSetError( 2 );
      fResult = ZH_FALSE;
   }

   return fResult;
}

/* NOTE: Caller must free the pointer, if not NULL */
char * zh_fsLinkRead( const char * pszFile )
{
   char * pszLink = NULL;

   if( pszFile )
   {
      zh_vmUnlock();

#if defined( ZH_OS_WIN )
      {
         typedef DWORD ( WINAPI * _ZH_GETFINALPATHNAMEBYHANDLE )( HANDLE, LPTSTR, DWORD, DWORD );

         static _ZH_GETFINALPATHNAMEBYHANDLE s_pGetFinalPathNameByHandle = ( _ZH_GETFINALPATHNAMEBYHANDLE ) -1;

         #ifndef VOLUME_NAME_DOS
         #define VOLUME_NAME_DOS       0x0
         #endif
         #ifndef VOLUME_NAME_GUID
         #define VOLUME_NAME_GUID      0x1
         #endif
         #ifndef VOLUME_NAME_NT
         #define VOLUME_NAME_NT        0x2
         #endif
         #ifndef VOLUME_NAME_NONE
         #define VOLUME_NAME_NONE      0x4
         #endif
         #ifndef FILE_NAME_NORMALIZED
         #define FILE_NAME_NORMALIZED  0x0
         #endif
         #ifndef FILE_NAME_OPENED
         #define FILE_NAME_OPENED      0x8
         #endif

         if( s_pGetFinalPathNameByHandle == ( _ZH_GETFINALPATHNAMEBYHANDLE ) -1 )
         {
            HMODULE hModule = GetModuleHandle( TEXT( "kernel32.dll" ) );
            if( hModule )
               s_pGetFinalPathNameByHandle = ( _ZH_GETFINALPATHNAMEBYHANDLE )
                  ZH_WINAPI_GETPROCADDRESST( hModule, "GetFinalPathNameByHandle" );
            else
               s_pGetFinalPathNameByHandle = NULL;
         }

         if( s_pGetFinalPathNameByHandle )
         {
            LPCTSTR lpFileName;
            LPTSTR lpFileNameFree;
            HANDLE hFile;
            DWORD dwAttr;
            ZH_BOOL fDir;

            lpFileName = ZH_FSNAMECONV( pszFile, &lpFileNameFree );

            dwAttr = GetFileAttributes( lpFileName );
            fDir = ( dwAttr != INVALID_FILE_ATTRIBUTES ) &&
                   ( dwAttr & FILE_ATTRIBUTE_DIRECTORY );

            hFile = CreateFile( lpFileName,
                                GENERIC_READ,
                                FILE_SHARE_READ,
                                NULL,
                                OPEN_EXISTING,
                                fDir ? ( FILE_ATTRIBUTE_DIRECTORY | FILE_FLAG_BACKUP_SEMANTICS ) : FILE_ATTRIBUTE_NORMAL,
                                NULL );

            if( hFile == INVALID_HANDLE_VALUE )
               zh_fsSetIOError( ZH_FALSE, 0 );
            else
            {
               DWORD size;
               TCHAR lpLink[ ZH_PATH_MAX ];
               size = s_pGetFinalPathNameByHandle( hFile, lpLink, ZH_PATH_MAX, VOLUME_NAME_DOS );
               if( size < ZH_PATH_MAX )
               {
                  if( size > 0 )
                  {
                     lpLink[ size ] = TEXT( '\0' );
                     pszLink = ZH_OSSTRDUP( lpLink );
                  }

                  zh_fsSetIOError( ZH_TRUE, 0 );
               }
               else
                  zh_fsSetError( 9 );
            }

            if( lpFileNameFree )
               zh_xfree( lpFileNameFree );
         }
         else
            zh_fsSetError( 1 );
      }
#elif defined( ZH_OS_UNIX )
      {
         char * pszFileFree;
         size_t size;

         pszFile = zh_fsNameConv( pszFile, &pszFileFree );

         pszLink = ( char * ) zh_xgrab( ZH_PATH_MAX + 1 );
         size = readlink( pszFile, pszLink, ZH_PATH_MAX );
         zh_fsSetIOError( size != ( size_t ) -1, 0 );
         if( size == ( size_t ) -1 )
         {
            zh_xfree( pszLink );
            pszLink = NULL;
         }
         else
         {
            pszLink[ size ] = '\0';
            /* Convert from OS codepage */
            pszLink = ( char * ) ZH_UNCONST( zh_osDecodeCP( pszLink, NULL, NULL ) );
         }

         if( pszFileFree )
            zh_xfree( pszFileFree );
      }
#else
      {
         zh_fsSetError( 1 );
      }
#endif

      zh_vmLock();
   }
   else
      zh_fsSetError( 2 );

   return pszLink;
}

ZH_FUNC( ZH_FLINK )
{
   ZH_ERRCODE uiError = 2;
   ZH_BOOL fResult = ZH_FALSE;
   const char * pszExisting = zh_parc( 1 ), * pszNewFile = zh_parc( 2 );

   if( pszExisting && pszNewFile )
   {
      fResult = zh_fsLink( pszExisting, pszNewFile );
      uiError = zh_fsError();
   }
   zh_retni( fResult ? 0 : F_ERROR );
   zh_fsSetFError( uiError );
}

ZH_FUNC( ZH_FLINKSYM )
{
   ZH_ERRCODE uiError = 2;
   ZH_BOOL fResult = ZH_FALSE;
   const char * pszTarget = zh_parc( 1 ), * pszNewFile = zh_parc( 2 );

   if( pszTarget && pszNewFile )
   {
      fResult = zh_fsLinkSym( pszTarget, pszNewFile );
      uiError = zh_fsError();
   }
   zh_retni( fResult ? 0 : F_ERROR );
   zh_fsSetFError( uiError );
}

ZH_FUNC( ZH_FLINKREAD )
{
   ZH_ERRCODE uiError = 2;
   char * pszResult = NULL;
   const char * pszFile = zh_parc( 1 );

   if( pszFile )
   {
      pszResult = zh_fsLinkRead( pszFile );
      uiError = zh_fsError();
   }
   zh_retc_buffer( pszResult );
   zh_fsSetFError( uiError );
}
