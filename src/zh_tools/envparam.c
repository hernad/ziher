/*
 * EnvParam()
 *
 * Copyright 2012 Przemyslaw Czerpak
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

#if defined( ZH_OS_UNIX ) && ! defined( ZH_OS_IOS )
#  include <unistd.h>
#  if defined( ZH_OS_DARWIN )
#     include <crt_externs.h>
#     define environ  ( *_NSGetEnviron() )
#  else
      extern char ** environ;
#  endif
#elif defined( ZH_OS_WIN )
#  include "zh_win_unicode.h"
#  include <windows.h>
#endif

ZH_FUNC( ENVPARAM )
{
#if ( defined( ZH_OS_UNIX ) && ! defined( ZH_OS_IOS ) ) || \
    defined( ZH_OS_DOS ) || defined( ZH_OS_OS2 )
   char * const * pEnviron = environ, * const * pEnv;
   char * pResult = NULL, * pDst;

   if( pEnviron )
   {
      ZH_SIZE nSize = 0;

      for( pEnv = pEnviron; *pEnv; pEnv++ )
         nSize += strlen( *pEnv ) + 2;

      if( nSize > 0 )
      {
         pResult = ( char * ) zh_xgrab( ( nSize + 1 ) * sizeof( char ) );
         for( pEnv = pEnviron, pDst = pResult; *pEnv; pEnv++ )
         {
            ZH_SIZE n = strlen( *pEnv );
            memcpy( pDst, *pEnv, n );
            pDst += n;
            *pDst++ = '\r';
            *pDst++ = '\n';
         }
         *pDst++ = '\0';
      }
   }

   if( pResult )
      zh_retc_buffer( ( char * ) ZH_UNCONST( zh_osDecodeCP( pResult, NULL, NULL ) ) );
   else
      zh_retc_null();
#elif defined( ZH_OS_WIN )
   LPTCH lpEnviron = GetEnvironmentStrings(), lpEnv;
   LPTSTR lpResult = NULL;
   ZH_SIZE nSize = 0;

   if( lpEnviron )
   {
      for( lpEnv = lpEnviron; *lpEnv; lpEnv++ )
      {
         while( *++lpEnv )
            ++nSize;
         nSize += 3;
      }
      if( nSize > 0 )
      {
         LPTSTR lpDst;

         lpResult = ( LPTSTR ) zh_xgrab( ( nSize + 1 ) * sizeof( TCHAR ) );
         for( lpEnv = lpEnviron, lpDst = lpResult; *lpEnv; lpEnv++ )
         {
            do
            {
               *lpDst++ = *lpEnv++;
            }
            while( *lpEnv );
            *lpDst++ = '\r';
            *lpDst++ = '\n';
         }
      }

      FreeEnvironmentStrings( lpEnviron );
   }

   if( lpResult )
   {
      ZH_RETSTRLEN( lpResult, nSize );
      zh_xfree( lpResult );
   }
   else
      zh_retc_null();
#else
   zh_retc_null();
#endif
}
