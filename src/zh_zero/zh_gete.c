/*
 * environment variables access
 *
 * Copyright 2001-2002 Antonio Linares <alinares@fivetech.com>
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

/* NOTE: Notice that this code is needed as ANSI C getenv() crashes
         badly when used from a Windows DLL. */

#include "zh_api.h"

#if defined( ZH_OS_WIN )
   #include <windows.h>
   #include "zh_winuni.h"
#endif

/* NOTE: Warning, this function _may_ return NULL as a result if
         the environment variable reading fails form some reason.
         If the return value is not NULL, the caller must free
         the pointer. [vszakats] */

char * zh_getenv( const char * szName )
{
   char * pszBuffer = NULL;

#if defined( ZH_OS_WIN )
   {
      LPTSTR lpName = ZH_CHARDUP( szName );
      DWORD size = GetEnvironmentVariable( lpName, NULL, 0 );

      if( size != 0 )
      {
         LPTSTR lpBuffer = ( LPTSTR ) zh_xgrab( size * sizeof( TCHAR ) );
         GetEnvironmentVariable( lpName, lpBuffer, size );
         pszBuffer = ZH_OSSTRDUP( lpBuffer );
         zh_xfree( lpBuffer );
      }
      zh_xfree( lpName );
   }
#else
   {
      char * pszTemp, * pszNameFree = NULL;

      szName = zh_osEncodeCP( szName, &pszNameFree, NULL );
      pszTemp = getenv( szName );
      if( pszNameFree )
         zh_xfree( pszNameFree );

      if( pszTemp != NULL )
         pszBuffer = zh_osStrDecode( pszTemp );
   }
#endif

   return pszBuffer;
}


ZH_BOOL zh_getenv_buffer( const char * szName, char * szBuffer, int nSize )
{
   ZH_BOOL fRetVal;

#if defined( ZH_OS_WIN )
   {
      LPTSTR lpName = ZH_CHARDUP( szName ), lpBuffer;

      if( szBuffer != NULL || nSize > 0 )
         lpBuffer = ( LPTSTR ) zh_xgrab( nSize * sizeof( TCHAR ) );
      else
         lpBuffer = NULL;

      fRetVal = GetEnvironmentVariable( lpName, lpBuffer, nSize ) != 0;

      if( lpBuffer )
      {
         if( fRetVal )
         {
            lpBuffer[ nSize - 1 ] = TEXT( '\0' );
            ZH_OSSTRDUP2( lpBuffer, szBuffer, nSize - 1 );
         }
         zh_xfree( lpBuffer );
      }
      zh_xfree( lpName );
   }
#else
   {
      char * pszTemp, * pszNameFree = NULL;

      szName = zh_osEncodeCP( szName, &pszNameFree, NULL );
      pszTemp = getenv( szName );
      if( pszNameFree )
         zh_xfree( pszNameFree );

      if( pszTemp != NULL )
      {
         fRetVal = ZH_TRUE;
         if( szBuffer != NULL && nSize != 0 )
            zh_osStrDecode2( pszTemp, szBuffer, nSize - 1 );
      }
      else
         fRetVal = ZH_FALSE;
   }
#endif

   if( ! fRetVal && szBuffer != NULL && nSize != 0 )
      szBuffer[ 0 ] = '\0';

   return fRetVal;
}

/* set current process environment variable, if szValue is NULL delete
 * environment variable
 */
ZH_BOOL zh_setenv( const char * szName, const char * szValue )
{
   if( szName == NULL )
      return ZH_FALSE;

#if defined( ZH_OS_WIN )
   {
      LPTSTR lpName = ZH_CHARDUP( szName );
      LPTSTR lpValue = szValue ? ZH_CHARDUP( szValue ) : NULL;
      ZH_BOOL fResult = ( SetEnvironmentVariable( lpName, lpValue ) != 0 );
      if( lpValue )
         zh_xfree( lpValue );
      zh_xfree( lpName );
      return fResult;
   }

// https://stackoverflow.com/questions/48332332/what-does-define-posix-source-mean
#elif  _POSIX_C_SOURCE >= 200112L || defined( ZH_OS_DARWIN ) || defined( ZH_OS_ANDROID )
   {
      ZH_BOOL fResult;
      char * pszNameFree = NULL, * pszValueFree = NULL;

      szName = zh_osEncodeCP( szName, &pszNameFree, NULL );
      if( szValue )
      {
         szValue = zh_osEncodeCP( szValue, &pszValueFree, NULL );
         fResult = setenv( szName, szValue, 1 ) == 0;
         if( pszValueFree )
            zh_xfree( pszValueFree );
      }
      else
      {
         fResult = unsetenv( szName ) == 0;
      }

      if( pszNameFree )
         zh_xfree( pszNameFree );

      return fResult;
   }
#elif defined( _ZH_NO_SETENV_ )

   ZH_SYMBOL_UNUSED( szValue );

   return ZH_FALSE;

#else
   /* please add support for other C compilers
    * if such functionality does not exists for given platform/C compiler
    * then please simply added C compiler with necessary OS/version checking
    * to the above #elif ... to eliminate warning [druzus]
    */

   int iTODO;

   ZH_SYMBOL_UNUSED( szValue );

   return ZH_FALSE;

#endif
}
