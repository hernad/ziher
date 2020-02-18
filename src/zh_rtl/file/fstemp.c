/*
 * zh_FTempCreate() function
 *
 * Copyright 2000-2001 Jose Lalin <dezac@corevia.com>
 * Copyright 2000-2010 Viktor Szakats
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

/* *nixes */
#if ! defined( _LARGEFILE64_SOURCE )
#  define _LARGEFILE64_SOURCE  1
#endif
#if ! defined( _GNU_SOURCE )
#  define _GNU_SOURCE
#endif

#include "zh_api.h"
#include "zh_codepage_api.h"
#include "zh_apifs.h"
#include "zh_vm.h"
#include "zh_math.h"
#include "zh_set.h"

#if defined( ZH_OS_UNIX )
   #include <stdlib.h>
   #include <unistd.h>  /* We need for mkstemp() on BSD */
#endif

#if defined( ZH_OS_WIN )
   #include <windows.h>
   #include "zh_win_uni.h"
#endif

#if defined( ZH_OS_LINUX )  || \
    defined( ZH_OS_BSD ) || defined( ZH_OS_DARWIN ) || defined( ZH_OS_SUNOS )
#  define ZH_HAS_MKSTEMP
#  if ( defined( ZH_OS_BSD ) && ! defined( __NetBSD__ ) ) || defined( ZH_OS_DARWIN )
#     define ZH_HAS_MKSTEMPS
#  elif defined( ZH_OS_LINUX ) && \
        ( defined( _BSD_SOURCE ) || defined( _SVID_SOURCE ) ) && \
        defined( __GLIBC_PREREQ )
#     if __GLIBC_PREREQ( 2, 12 )
#        define ZH_HAS_MKSTEMPS
#     endif
#  endif
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

#if ! defined( ZH_OS_WIN )
static ZH_BOOL fsGetTempDirByCase( char * pszName, const char * pszTempDir, ZH_BOOL fTrans )
{
   ZH_BOOL fOK = ZH_FALSE;

   if( pszTempDir && *pszTempDir != '\0' )
   {
      char * pTmp;

      if( fTrans )
         zh_osStrDecode2( pszTempDir, pszName, ZH_PATH_MAX - 1 );
      else
         zh_strncpy( pszName, pszTempDir, ZH_PATH_MAX - 1 );

      switch( zh_setGetDirCase() )
      {
         case ZH_SET_CASE_LOWER:
            pTmp = zh_cdpnDupLower( zh_vmCDP(), pszName, NULL );
            fOK = strcmp( pszName, pTmp ) == 0;
            zh_xfree( pTmp );
            break;
         case ZH_SET_CASE_UPPER:
            pTmp = zh_cdpnDupUpper( zh_vmCDP(), pszName, NULL );
            fOK = strcmp( pszName, pTmp ) == 0;
            zh_xfree( pTmp );
            break;
         default:
            fOK = ZH_TRUE;
            break;
      }
   }

   if( fOK )
   {
      if( ! zh_fsDirExists( pszTempDir ) )
         fOK = ZH_FALSE;
   }

   return fOK;
}
#endif

ZH_FHANDLE zh_fsCreateTempEx( char * pszName, const char * pszDir, const char * pszPrefix, const char * pszExt, ZH_FATTR ulAttr )
{
   /* less attemps */
   int iAttemptLeft = 99, iLen;
   ZH_FHANDLE fd;

   do
   {
      pszName[ 0 ] = '\0';

      if( pszDir && pszDir[ 0 ] != '\0' )
      {
         zh_strncpy( pszName, pszDir, ZH_PATH_MAX - 1 );
         iLen = ( int ) strlen( pszName );
         if( pszName[ iLen - 1 ] != ZH_OS_PATH_DELIM_CHR &&
             iLen < ZH_PATH_MAX - 1 )
         {
            pszName[ iLen ] = ZH_OS_PATH_DELIM_CHR;
            pszName[ iLen + 1 ] = '\0';
         }
      }
      else
         zh_fsTempDir( pszName );

      if( pszPrefix )
         zh_strncat( pszName, pszPrefix, ZH_PATH_MAX - 1 );

      iLen = ( int ) strlen( pszName );
      if( iLen > ( ZH_PATH_MAX - 1 ) - 6 -
                 ( pszExt ? ( int ) strlen( pszExt ) : 0 ) )
      {
         fd = FS_ERROR;
         break;
      }

#if defined( ZH_HAS_MKSTEMP )
      if( zh_setGetFileCase() != ZH_SET_CASE_LOWER &&
          zh_setGetFileCase() != ZH_SET_CASE_UPPER &&
          zh_setGetDirCase() != ZH_SET_CASE_LOWER &&
          zh_setGetDirCase() != ZH_SET_CASE_UPPER
#if ! defined( ZH_HAS_MKSTEMPS )
          && ( pszExt == NULL || *pszExt == 0 )
#endif
        )
      {
         zh_vmUnlock();
         zh_strncat( pszName, "XXXXXX", ZH_PATH_MAX - 1 );
#if defined( ZH_HAS_MKSTEMPS )
         if( pszExt && *pszExt )
         {
            zh_strncat( pszName, pszExt, ZH_PATH_MAX - 1 );
#if defined( ZH_USE_LARGEFILE64 )
            fd = ( ZH_FHANDLE ) mkstemps64( pszName, ( int ) strlen( pszExt ) );
#else
            fd = ( ZH_FHANDLE ) mkstemps( pszName, ( int ) strlen( pszExt ) );
#endif
         }
         else
#endif
#if defined( ZH_USE_LARGEFILE64 )
            fd = ( ZH_FHANDLE ) mkstemp64( pszName );
#else
            fd = ( ZH_FHANDLE ) mkstemp( pszName );
#endif
         zh_fsSetIOError( fd != ( ZH_FHANDLE ) -1, 0 );
         zh_vmLock();
      }
      else
#endif /* ZH_HAS_MKSTEMP */
      {
         int i;
         double d = zh_random_num_secure(), x;

         for( i = 0; i < 6; i++ )
         {
            int n;
            d = d * 36;
            n = ( int ) d;
            d = modf( d, &x );
            pszName[ iLen++ ] = ( char ) ( n + ( n > 9 ? 'a' - 10 : '0' ) );
         }
         pszName[ iLen ] = '\0';
         if( pszExt )
            zh_strncat( pszName, pszExt, ZH_PATH_MAX - 1 );
         fd = zh_fsCreateEx( pszName, ulAttr, FO_EXCLUSIVE | FO_EXCL );
      }

      if( fd != ( ZH_FHANDLE ) FS_ERROR )
         break;
   }
   while( --iAttemptLeft );

   return fd;
}

/* NOTE: The buffer must be at least ZH_PATH_MAX chars long */
#if ! defined( ZH_OS_UNIX )

static ZH_BOOL zh_fsTempName( char * pszBuffer, const char * pszDir, const char * pszPrefix )
{
   ZH_BOOL fResult;

   pszBuffer[ 0 ] = '\0';

   zh_vmUnlock();

#if defined( ZH_OS_WIN )
   {
      LPCTSTR lpPrefix, lpDir;
      LPTSTR lpPrefixFree = NULL, lpDirFree = NULL;

      TCHAR lpBuffer[ ZH_PATH_MAX ];
      TCHAR lpTempDir[ ZH_PATH_MAX ];

      lpPrefix = pszPrefix ? ZH_FSNAMECONV( pszPrefix, &lpPrefixFree ) : NULL;

      if( pszDir && pszDir[ 0 ] != '\0' )
         lpDir = ZH_FSNAMECONV( pszDir, &lpDirFree );
      else
      {
         if( ! GetTempPath( ZH_PATH_MAX, lpTempDir ) )
         {
            zh_fsSetIOError( ZH_FALSE, 0 );
            return ZH_FALSE;
         }
         lpTempDir[ ZH_PATH_MAX - 1 ] = TEXT( '\0' );
         lpDir = lpTempDir;
      }

      fResult = GetTempFileName( lpDir, lpPrefix ? lpPrefix : TEXT( "hb" ), 0, lpBuffer );

      if( fResult )
         ZH_OSSTRDUP2( lpBuffer, pszBuffer, ZH_PATH_MAX - 1 );

      if( lpPrefixFree )
         zh_xfree( lpPrefixFree );
      if( lpDirFree )
         zh_xfree( lpDirFree );
   }
#else
   {
      char * pTmpBuffer = ( char * ) zh_xgrab( L_tmpnam + 1 );

      /* TODO: Implement these: */
      ZH_SYMBOL_UNUSED( pszDir );
      ZH_SYMBOL_UNUSED( pszPrefix );

      pTmpBuffer[ 0 ] = '\0';
      fResult = ( tmpnam( pszBuffer ) != NULL );
      pTmpBuffer[ L_tmpnam ] = '\0';

      if( fResult )
      {
         zh_osStrDecode2( pTmpBuffer, pszBuffer, ZH_PATH_MAX - 1 );
      }
      zh_xfree( pTmpBuffer );
   }
#endif

   zh_fsSetIOError( fResult, 0 );
   zh_vmLock();

   return fResult;
}

#endif

/* NOTE: The pszName buffer must be at least ZH_PATH_MAX chars long */

ZH_FHANDLE zh_fsCreateTemp( const char * pszDir, const char * pszPrefix, ZH_FATTR ulAttr, char * pszName )
{
#if defined( ZH_OS_UNIX )
   return zh_fsCreateTempEx( pszName, pszDir, pszPrefix, NULL, ulAttr );
#else
   /* If there was no special extension requested, we're using
      native temp file generation functions on systems where such
      API exist. */
   int iAttemptLeft = 999;

   while( --iAttemptLeft )
   {
      if( zh_fsTempName( pszName, pszDir, pszPrefix ) )
      {
#if defined( ZH_OS_WIN )
         /* Using FO_TRUNC on win platforms as zh_fsTempName() uses GetTempFileName(),
            which creates the file, so FO_EXCL would fail at this point. [vszakats] */
         ZH_FHANDLE fhnd = zh_fsCreateEx( pszName, ulAttr, FO_EXCLUSIVE | FO_TRUNC );
#else
         ZH_FHANDLE fhnd = zh_fsCreateEx( pszName, ulAttr, FO_EXCLUSIVE | FO_EXCL );
#endif

         /* This function may fail, if the generated filename got
            used between generation and the file creation. */

         if( fhnd != FS_ERROR )
            return fhnd;
      }
      else
      {
         /* Don't attempt to retry if the filename generator is
            failing for some reason. */
         break;
      }
   }

   return FS_ERROR;
#endif
}

/* NOTE: pszTempDir must be at least ZH_PATH_MAX long. */
ZH_ERRCODE zh_fsTempDir( char * pszTempDir )
{
   ZH_ERRCODE nResult = ( ZH_ERRCODE ) FS_ERROR;

   pszTempDir[ 0 ] = '\0';

#if defined( ZH_OS_UNIX )
   {
      char * pszTempDirEnv = zh_getenv( "TMPDIR" );

      if( fsGetTempDirByCase( pszTempDir, pszTempDirEnv, ZH_FALSE ) )
         nResult = 0;
#ifdef P_tmpdir
      else if( fsGetTempDirByCase( pszTempDir, P_tmpdir, ZH_TRUE ) )
         nResult = 0;
#endif
      else if( fsGetTempDirByCase( pszTempDir, "/tmp", ZH_TRUE ) )
         nResult = 0;

      if( pszTempDirEnv )
         zh_xfree( pszTempDirEnv );
   }
#elif defined( ZH_OS_WIN )
   {
      TCHAR lpDir[ ZH_PATH_MAX ];

      if( GetTempPath( ZH_PATH_MAX, lpDir ) )
      {
         nResult = 0;
         lpDir[ ZH_PATH_MAX - 1 ] = TEXT( '\0' );
         ZH_OSSTRDUP2( lpDir, pszTempDir, ZH_PATH_MAX - 1 );
      }
   }
#else
   {
      char szBuffer[ L_tmpnam ];

      if( tmpnam( szBuffer ) != NULL )
      {
         PZH_FNAME pTempName = zh_fsFNameSplit( szBuffer );
         if( fsGetTempDirByCase( pszTempDir, pTempName->szPath, ZH_TRUE ) )
            nResult = 0;
         zh_xfree( pTempName );
      }
      if( nResult != 0 )
      {
         static const char * env_tmp[] = { "TEMP", "TMP", "TMPDIR", NULL };

         const char ** tmp = env_tmp;

         while( *tmp && nResult != 0 )
         {
            char * pszTempDirEnv = zh_getenv( *tmp++ );

            if( pszTempDirEnv )
            {
               if( fsGetTempDirByCase( pszTempDir, pszTempDirEnv, ZH_FALSE ) )
                  nResult = 0;
               zh_xfree( pszTempDirEnv );
            }
         }
      }
   }
#endif

   if( nResult == 0 && pszTempDir[ 0 ] != '\0' )
   {
      int len = ( int ) strlen( pszTempDir );
      if( pszTempDir[ len - 1 ] != ZH_OS_PATH_DELIM_CHR &&
          len < ZH_PATH_MAX - 1 )
      {
         pszTempDir[ len ] = ZH_OS_PATH_DELIM_CHR;
         pszTempDir[ len + 1 ] = '\0';
      }
   }
   else
   {
      pszTempDir[ 0 ] = '.';
      pszTempDir[ 1 ] = ZH_OS_PATH_DELIM_CHR;
      pszTempDir[ 2 ] = '\0';
   }

   return nResult;
}

ZH_FUNC( ZH_FTEMPCREATE )
{
   char szName[ ZH_PATH_MAX ];

   zh_retnint( ( ZH_NHANDLE ) zh_fsCreateTemp( zh_parc( 1 ),
                                               zh_parc( 2 ),
                                               ( ZH_FATTR ) zh_parnldef( 3, FC_NORMAL ),
                                               szName ) );

   zh_storc( szName, 4 );
}

ZH_FUNC( ZH_FTEMPCREATEEX )
{
   char szName[ ZH_PATH_MAX ];

   zh_retnint( ( ZH_NHANDLE ) zh_fsCreateTempEx( szName,
                                                 zh_parc( 2 ),
                                                 zh_parc( 3 ),
                                                 zh_parc( 4 ),
                                                 ( ZH_FATTR ) zh_parnldef( 5, FC_NORMAL ) ) );

   zh_storc( szName, 1 );
}

ZH_FUNC( ZH_DIRTEMP )
{
   char szTempDir[ ZH_PATH_MAX ];

   if( zh_fsTempDir( szTempDir ) != ( ZH_ERRCODE ) FS_ERROR )
      zh_retc( szTempDir );
   else
      zh_retc_null();
}
