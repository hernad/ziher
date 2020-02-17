/*
 * zh_fsFSize() function
 *
 * Copyright 2000-2001 Jose Lalin <dezac@corevia.com>
 * Copyright 2000-2001 Viktor Szakats
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

#if ! defined( _LARGEFILE64_SOURCE )
#  define _LARGEFILE64_SOURCE  1
#endif

#include "zh_api.h"
#include "zh_apifs.h"
#include "zh_vm.h"

#if defined( ZH_OS_WIN )
#  include <windows.h>
#  include "zh_win_uni.h"
#else
#  include <sys/types.h>
#  include <sys/stat.h>
#endif

#if ! defined( ZH_USE_LARGEFILE64 ) && defined( ZH_OS_UNIX )
   #if defined( __USE_LARGEFILE64 )
      /*
       * The macro: __USE_LARGEFILE64 is set when _LARGEFILE64_SOURCE is
       * defined and effectively enables lseek64()/flock64()/ftruncate64()
       * functions on 32-bit machines.
       */
      #define ZH_USE_LARGEFILE64
   #elif defined( ZH_OS_UNIX ) && defined( O_LARGEFILE )
      #define ZH_USE_LARGEFILE64
   #endif
#endif


ZH_FOFFSET zh_fsFSize( const char * pszFileName, ZH_BOOL bUseDirEntry )
{
   if( bUseDirEntry )
   {
#if defined( ZH_OS_WIN )
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
         ZH_BOOL fResult;

         lpFileName = ZH_FSNAMECONV( pszFileName, &lpFree );
         memset( &attrex, 0, sizeof( attrex ) );
         fResult = s_pGetFileAttributesEx( lpFileName, GetFileExInfoStandard, &attrex ) &&
                   ( attrex.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY ) == 0;
         zh_fsSetIOError( fResult, 0 );
         if( lpFree )
            zh_xfree( lpFree );
         if( fResult )
            return ( ZH_FOFFSET ) attrex.nFileSizeLow +
                 ( ( ZH_FOFFSET ) attrex.nFileSizeHigh << 32 );
      }
      else
      {
         PZH_FFIND ffind = zh_fsFindFirst( pszFileName, ZH_FA_ALL );
         zh_fsSetIOError( ffind != NULL, 0 );
         if( ffind )
         {
            ZH_FOFFSET size = ffind->size;
            zh_fsFindClose( ffind );
            return size;
         }
      }

#elif defined( ZH_USE_LARGEFILE64 )
      char * pszFree;
      ZH_BOOL fResult;
      struct stat64 statbuf;
      pszFileName = zh_fsNameConv( pszFileName, &pszFree );
      statbuf.st_size = 0;
      zh_vmUnlock();
      fResult = stat64( pszFileName, &statbuf ) == 0;
      zh_fsSetIOError( fResult, 0 );
      zh_vmLock();
      if( pszFree )
         zh_xfree( pszFree );
      if( fResult )
         return ( ZH_FOFFSET ) statbuf.st_size;
#else
      char * pszFree;
      ZH_BOOL fResult;
      struct stat statbuf;
      pszFileName = zh_fsNameConv( pszFileName, &pszFree );
      statbuf.st_size = 0;
      zh_vmUnlock();
      fResult = stat( ( char * ) ZH_UNCONST( pszFileName ), &statbuf ) == 0;
      zh_fsSetIOError( fResult, 0 );
      zh_vmLock();
      if( pszFree )
         zh_xfree( pszFree );
      if( fResult )
         return ( ZH_FOFFSET ) statbuf.st_size;
#endif
   }
   else
   {
      ZH_FHANDLE hFileHandle = zh_fsOpen( pszFileName, FO_READ | FO_COMPAT );

      if( hFileHandle != FS_ERROR )
      {
         ZH_FOFFSET nPos = zh_fsSeekLarge( hFileHandle, 0, FS_END );
         zh_fsClose( hFileHandle );
         return nPos;
      }
   }

   return 0;
}

ZH_FUNC( ZH_FSIZE )
{
   const char * pszFile = zh_parc( 1 );

   zh_retnint( pszFile ? zh_fsFSize( pszFile, zh_parldef( 2, ZH_TRUE ) ) : 0 );
}
