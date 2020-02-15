/*
 * zh_fopen() function
 *
 * Copyright 2007 Przemyslaw Czerpak
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

/* NOTE: To avoid warnings with MSVC. For our purpose fopen_s() is not a good
         alternative because it only opens files in non-shared mode. [vszakats] */
#ifndef _CRT_SECURE_NO_WARNINGS
#define _CRT_SECURE_NO_WARNINGS
#endif

#include "zh_api.h"
#include "zh_apifs.h"
#include "zh_vm.h"
#if defined( ZH_OS_WIN )
   #include <windows.h>
   #include "zh_winuni.h"
#endif

#if defined( ZH_OS_WIN )
   #define ZH_USE_FSOPEN
   #include <share.h>
   #if ! defined( SH_DENYNO ) && defined( _SH_DENYNO )
      #define SH_DENYNO _SH_DENYNO
   #endif
#endif



FILE * zh_fopen( const char * path, const char * mode )
{
   FILE * file;

#if defined( ZH_OS_WIN ) && defined( UNICODE )
   LPCTSTR lpPath, lpMode;
   LPTSTR lpFreeP, lpFreeM;

   lpPath = ZH_FSNAMECONV( path, &lpFreeP );
   lpMode = ZH_FSNAMECONV( mode, &lpFreeM );

   zh_vmUnlock();
   #if defined( ZH_USE_FSOPEN )
      file = _wfsopen( lpPath, lpMode, SH_DENYNO );
   #elif defined( _MSC_VER ) && _MSC_VER >= 1400 && ! defined( _CRT_SECURE_NO_WARNINGS )
      if( _wfopen_s( &file, lpPath, lpMode ) != 0 )
         file = NULL;
   #else
      file = _wfopen( lpPath, lpMode );
   #endif
   zh_vmLock();

   if( lpFreeP )
      zh_xfree( lpFreeP );
   if( lpFreeM )
      zh_xfree( lpFreeM );
#else
   char * pszFree = NULL;

   path = zh_fsNameConv( path, &pszFree );

   zh_vmUnlock();
   #if defined( ZH_USE_FSOPEN )
      file = _fsopen( path, mode, SH_DENYNO );
   #elif defined( _MSC_VER ) && _MSC_VER >= 1400 && ! defined( _CRT_SECURE_NO_WARNINGS )
      if( fopen_s( &file, path, mode ) != 0 )
         file = NULL;
   #else
      file = fopen( path, mode );
   #endif
   zh_vmLock();

   if( pszFree )
      zh_xfree( pszFree );
#endif

   return file;
}
