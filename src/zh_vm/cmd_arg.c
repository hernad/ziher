/*
 * Command-line and environment argument management
 *
 * Copyright 1999-2001 Viktor Szakats
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

/* NOTE: Need to have these before Ziher headers,
         because in MT mode, they will automatically #include <os2.h>. */
#define INCL_DOSPROCESS
#define INCL_DOSERRORS
#define INCL_DOSMODULEMGR

#include "zh_api.h"
#include "zh_item_api.h"
#include "zh_apifs.h"
#include "zh_codepage_api.h"
#include "zh_vm.h"
#include "memory.zhh"
#include "zh_stack.h"
#include "zh_ver_bld.h"


/* Command-line argument management */
static int     s_argc = 0;
static char ** s_argv = NULL;

#if ! defined( ZH_OS_WIN )

static char    s_szAppName[ ZH_PATH_MAX ];

#else

#include "hbwinuni.h"
#include <windows.h>

static LPTSTR * s_lpArgV = NULL;
#if defined( UNICODE )
static LPSTR * s_lpArgVStr = NULL;
#endif

static HANDLE  s_hInstance     = 0;
static HANDLE  s_hPrevInstance = 0;
static int     s_iCmdShow      = 0;
static ZH_BOOL s_WinMainParam  = ZH_FALSE;

#define ZH_WINARG_ALLOC( n )  HeapAlloc( GetProcessHeap(), 0, ( n ) )
#define ZH_WINARG_FREE( p )   HeapFree( GetProcessHeap(), 0, ( p ) )

void zh_winmainArgVBuild( void )
{
   LPCTSTR lpCmdLine;
   LPTSTR * lpArgV;
   LPTSTR lpDst, lpArg, lpModuleName;
   ZH_SIZE nSize, nModuleName;
   int iArgC;

   /* NOTE: MAX_PATH used intentionally instead of ZH_MAX_PATH */
   lpModuleName = ( LPTSTR ) ZH_WINARG_ALLOC( ( MAX_PATH + 1 ) * sizeof( TCHAR ) );
   nModuleName = GetModuleFileName( NULL, lpModuleName, MAX_PATH + 1 );
   if( nModuleName )
      nModuleName++;
   ZH_WINARG_FREE( lpModuleName );

   lpCmdLine = GetCommandLine();
   lpArgV = NULL;
   nSize = 0;
   iArgC = -1;

   while( lpCmdLine && ! lpArgV && iArgC != 0 )
   {
      ZH_BOOL fQuoted;
      LPCTSTR lpSrc;

      if( nSize != 0 )
      {
         lpArgV = ( LPTSTR * ) ZH_WINARG_ALLOC( iArgC * sizeof( LPTSTR ) +
                                                nSize * sizeof( TCHAR ) );
         lpDst = ( LPTSTR ) ( lpArgV + iArgC );
         lpArgV[ 0 ] = lpDst;
         lpDst += nModuleName;
      }
      else
      {
         lpDst = ( LPTSTR ) ZH_UNCONST( lpCmdLine );
         nSize = nModuleName;
      }

      lpSrc = lpCmdLine;
      lpArg = NULL;
      iArgC = 0;
      fQuoted = ZH_FALSE;

      while( *lpSrc != 0 )
      {
         if( *lpSrc == TEXT( '"' ) )
         {
            if( lpArg == NULL )
               lpArg = lpDst;
            fQuoted = ! fQuoted;
         }
         else if( fQuoted || ! ZH_ISSPACE( *lpSrc ) )
         {
            if( lpArg == NULL )
               lpArg = lpDst;
            if( iArgC > 0 || nModuleName == 0 )
            {
               if( lpArgV )
                  *lpDst++ = *lpSrc;
               else
                  nSize++;
            }
         }
         else
         {
            if( lpArg )
            {
               if( iArgC > 0 || nModuleName == 0 )
               {
                  if( lpArgV )
                  {
                     *lpDst++ = '\0';
                     lpArgV[ iArgC ] = lpArg;
                  }
                  else
                     nSize++;
               }
               iArgC++;
               lpArg = NULL;
            }
         }
         ++lpSrc;
      }
      if( lpArg )
      {
         if( iArgC > 0 || nModuleName == 0 )
         {
            if( lpArgV )
            {
               *lpDst = '\0';
               lpArgV[ iArgC ] = lpArg;
            }
            else
               nSize++;
         }
         iArgC++;
      }
   }

   if( iArgC <= 0 )
   {
      if( nModuleName != 0 )
      {
         iArgC = 1;
         lpArgV = ( LPTSTR * ) ZH_WINARG_ALLOC( iArgC * sizeof( LPTSTR ) +
                                                nModuleName * sizeof( TCHAR ) );
         lpArgV[ 0 ] = ( LPTSTR ) ( lpArgV + iArgC );
      }
      else
         iArgC = 0;
   }
   if( iArgC > 0 && nModuleName != 0 )
   {
      /* NOTE: Manually setup the executable name in Windows,
               because in console apps the name may be truncated
               in some cases, and in GUI apps it's not filled
               at all. [vszakats] */
      if( GetModuleFileName( NULL, lpArgV[ 0 ], ( DWORD ) nModuleName ) != 0 )
      {
         /* Windows XP does not set trailing 0 if buffer is not large enough [druzus] */
         lpArgV[ 0 ][ nModuleName - 1 ] = 0;
      }
   }

   zh_winmainArgVFree();

   if( iArgC > 0 )
   {
      s_lpArgV = lpArgV;
      s_argc = iArgC;
#if defined( UNICODE )
      {
         LPSTR lpStr;

         nSize = 0;
         for( iArgC = 0; iArgC < s_argc; ++iArgC )
            nSize += zh_wctomblen( s_lpArgV[ iArgC ] ) + 1;

         s_lpArgVStr = ( LPSTR * ) ZH_WINARG_ALLOC( s_argc * sizeof( LPSTR ) +
                                                    nSize * sizeof( char ) );
         lpStr = ( LPSTR ) ( s_lpArgVStr + s_argc );
         for( iArgC = 0; iArgC < s_argc; ++iArgC )
         {
            nSize = zh_wctomblen( s_lpArgV[ iArgC ] ) + 1;
            zh_wcntombcpy( lpStr, s_lpArgV[ iArgC ], nSize - 1 );
            s_lpArgVStr[ iArgC ] = lpStr;
            lpStr += nSize;
         }
         s_argv = s_lpArgVStr;
      }
#else
      s_argv = s_lpArgV;
#endif
   }
}

void zh_winmainArgVFree( void )
{
   if( s_lpArgV )
   {
#if defined( UNICODE )
      if( s_lpArgVStr )
      {
         if( s_argv == s_lpArgVStr )
            s_argv = NULL;
         ZH_WINARG_FREE( s_lpArgVStr );
         s_lpArgVStr = NULL;
      }
#else
      if( s_argv == s_lpArgV )
         s_argv = NULL;
#endif

      ZH_WINARG_FREE( s_lpArgV );
      s_lpArgV = NULL;
      s_argc = 0;
   }
}

void zh_winmainArgInit( void * hInstance, void * hPrevInstance, int iCmdShow )
{
   s_hInstance = ( HANDLE ) hInstance;
   s_hPrevInstance = ( HANDLE ) hPrevInstance;
   s_iCmdShow = iCmdShow;
   s_WinMainParam = ZH_TRUE;
}

ZH_BOOL zh_winmainArgGet( void * phInstance, void * phPrevInstance, int * piCmdShow )
{
   if( phInstance )
      *( ( HANDLE * ) phInstance ) = s_hInstance;
   if( phPrevInstance )
      *( ( HANDLE * ) phPrevInstance ) = s_hPrevInstance;
   if( piCmdShow )
      *piCmdShow = s_iCmdShow;

   return s_WinMainParam;
}

#endif

void zh_cmdargInit( int argc, char * argv[] )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_cmdargInit(%d, %p)", argc, ( void * ) argv ) );

#if defined( ZH_OS_WIN )
   if( s_lpArgV )
      return;
#endif

   if( argc == 0 || argv == NULL )
   {
      s_argc = 0;
      s_argv = NULL;
   }
   else
   {
      s_argc = argc;
      s_argv = argv;
   }
}

int zh_cmdargARGC( void )
{
   return s_argc;
}

char ** zh_cmdargARGV( void )
{
   return s_argv;
}

const char * zh_cmdargARGVN( int argc )
{
   return argc >= 0 && argc < s_argc ? s_argv[ argc ] : NULL;
}

/* NOTE: Pointer must be freed with zh_xfree() if not NULL */

static char * zh_cmdargDup( int argc )
{
#if defined( ZH_OS_WIN )
   if( s_lpArgV )
      return argc >= 0 && argc < s_argc ? ZH_OSSTRDUP( s_lpArgV[ argc ] ) : NULL;
#endif
   return argc >= 0 && argc < s_argc ? zh_osStrDecode( s_argv[ argc ] ) : NULL;
}

void zh_cmdargUpdate( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_cmdargUpdate()" ) );

#if ! defined( ZH_OS_WIN )
   if( s_argc > 0 )
   {
#  if defined( ZH_OS_OS2 )
      {
         PPIB ppib = NULL;
         APIRET ulrc;

         ulrc = DosGetInfoBlocks( NULL, &ppib );
         if( ulrc == NO_ERROR )
         {
            ulrc = DosQueryModuleName( ppib->pib_hmte,
                                       ZH_SIZEOFARRAY( s_szAppName ),
                                       s_szAppName );
            if( ulrc == NO_ERROR )
               s_argv[ 0 ] = s_szAppName;
         }
      }
#  else
      /* NOTE: try to create absolute path from s_argv[ 0 ] if necessary */
      {
         PZH_FNAME pFName = zh_fsFNameSplit( s_argv[ 0 ] );
         ZH_BOOL fInPath = ZH_FALSE;

         if( ! pFName->szPath )
         {
            char * pszPATH = zh_getenv( "PATH" );

            if( pszPATH && *pszPATH )
            {
               ZH_PATHNAMES * pSearchPath = NULL, * pNextPath;
               zh_fsAddSearchPath( pszPATH, &pSearchPath );
               pNextPath = pSearchPath;

               while( pNextPath )
               {
                  pFName->szPath = pNextPath->szPath;
                  zh_fsFNameMerge( s_szAppName, pFName );
                  if( zh_fsFileExists( s_szAppName ) )
                  {
                     /* even if the file is located using PATH then it does
                      * not mean we will have absolute path here. It's not
                      * good idea but PATH envvar can also contain relative
                      * directories, e.g. "." or "bin" so we should add
                      * current directory if necessary in code below.
                      */
                     zh_xfree( pFName );
                     pFName = zh_fsFNameSplit( s_szAppName );
                     fInPath = ZH_TRUE;
                     break;
                  }
                  pNextPath = pNextPath->pNext;
               }
               zh_fsFreeSearchPath( pSearchPath );
               if( ! fInPath )
                  pFName->szPath = NULL;
            }
            if( pszPATH )
               zh_xfree( pszPATH );
         }
         if( pFName->szPath )
         {
#     if defined( ZH_OS_HAS_DRIVE_LETTER )
            if( pFName->szPath[ 0 ] != ZH_OS_PATH_DELIM_CHR && ! pFName->szDrive )
#     else
            if( pFName->szPath[ 0 ] != ZH_OS_PATH_DELIM_CHR )
#     endif
            {
               if( pFName->szPath[ 0 ] == '.' &&
                   pFName->szPath[ 1 ] == ZH_OS_PATH_DELIM_CHR )
                  pFName->szPath += 2;
               s_szAppName[ 0 ] = ZH_OS_PATH_DELIM_CHR;
               zh_fsCurDirBuff( 0, s_szAppName + 1, ZH_PATH_MAX - 1 );
               if( s_szAppName[ 1 ] != 0 )
               {
                  zh_strncat( s_szAppName, ZH_OS_PATH_DELIM_CHR_STRING, ZH_PATH_MAX - 1 );
                  zh_strncat( s_szAppName, pFName->szPath, ZH_PATH_MAX - 1 );
                  pFName->szPath = zh_strdup( s_szAppName );
                  zh_fsFNameMerge( s_szAppName, pFName );
                  zh_xfree( ZH_UNCONST( pFName->szPath ) );
                  s_argv[ 0 ] = s_szAppName;
               }
            }
            else if( fInPath )
               s_argv[ 0 ] = s_szAppName;
         }
         zh_xfree( pFName );
      }
#  endif
   }
#endif
}

/* places application parameters on the HVM stack */

int zh_cmdargPushArgs( void )
{
   int iArgCount = 0, i;

   for( i = 1; i < s_argc; i++ )
   {
      /* Filter out any parameters beginning with //, like //INFO */
      if( ! zh_cmdargIsInternal( s_argv[ i ], NULL ) )
      {
#if defined( ZH_OS_WIN )
         if( s_lpArgV )
            ZH_ITEMPUTSTR( zh_stackAllocItem(), s_lpArgV[ i ] );
         else
#endif
            zh_vmPushString( s_argv[ i ], strlen( s_argv[ i ] ) );
         iArgCount++;
      }
   }

   return iArgCount;
}

ZH_BOOL zh_cmdargIsInternal( const char * szArg, int * piLen )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_cmdargIsInternal(%s, %p)", szArg, ( void * ) piLen ) );

   /* NOTE: Not checking for '--' here, as it would filter out
            valid command-line options used by applications. [vszakats] */

   if( zh_strnicmp( szArg, "--hb:", 5 ) == 0 ||
       zh_strnicmp( szArg, "//hb:", 5 ) == 0 )
   {
      if( piLen )
         *piLen = 5;

      return ZH_TRUE;
   }
   else if( strlen( szArg ) >= 2 &&
            szArg[ 0 ] == '/' &&
            szArg[ 1 ] == '/' )
   {
      if( piLen )
         *piLen = 2;

      return ZH_TRUE;
   }

   return ZH_FALSE;
}

static char * zh_cmdargGet( const char * pszName, ZH_BOOL bRetValue )
{
   char * pszRetVal = NULL;
   char * pszEnvVar;
   int i;
   int iPrefixLen;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_cmdargGet(%s, %d)", pszName, ( int ) bRetValue ) );

   /* Check the command-line first */

   for( i = 1; i < s_argc; i++ )
   {
      if( zh_cmdargIsInternal( s_argv[ i ], &iPrefixLen ) &&
          zh_strnicmp( s_argv[ i ] + iPrefixLen, pszName, strlen( pszName ) ) == 0 )
      {
         if( bRetValue )
         {
#if defined( ZH_OS_WIN )
            if( s_lpArgV )
            {
               LPCTSTR lpPos = s_lpArgV[ i ] + iPrefixLen + strlen( pszName );

               if( *lpPos == TEXT( ':' ) )
                  lpPos++;
               return ZH_OSSTRDUP( lpPos );
            }
            else
#endif
            {
               char * pszPos = s_argv[ i ] + iPrefixLen + strlen( pszName );

               if( *pszPos == ':' )
                  pszPos++;

               return zh_osStrDecode( pszPos );
            }
         }
         else
            return ( char * ) "";
      }
   }

   /* Check the environment variable */
   pszEnvVar = zh_getenv( "ZIHER" );
   if( ! pszEnvVar || pszEnvVar[ 0 ] == '\0' )
   {
      if( pszEnvVar )
         zh_xfree( pszEnvVar );

      pszEnvVar = NULL;
   }

   if( pszEnvVar && pszEnvVar[ 0 ] != '\0' )
   {
      char * pszNext = pszEnvVar;

      /* Step through all envvar switches. */

      /* NOTE: CA-Cl*pper doesn't need the switches to be separated by any
               chars at all, Ziher is more strict/standard in this respect,
               it requires the switches to be separated. */

      i = ( int ) strlen( pszName );
      while( *pszNext )
      {
         static const char * s_szSeparator = " ;,\t";
         char * pszEnd;

         /* Skip the separators */
         while( *pszNext && strchr( s_szSeparator, *pszNext ) )
            pszNext++;

         /* The // is optional in the envvar */
         if( zh_cmdargIsInternal( pszNext, &iPrefixLen ) )
            pszNext += iPrefixLen;

         pszEnd = pszNext;
         /* Search for the end of this switch */
         while( *pszEnd && strchr( s_szSeparator, *pszEnd ) == NULL )
            pszEnd++;

         /* Check the switch */
         if( zh_strnicmp( pszNext, pszName, i ) == 0 )
         {
            if( bRetValue )
            {
               ZH_SIZE nLen;
               pszNext += i;

               /* Skip value separator colon. */
               if( *pszNext == ':' )
                  pszNext++;

               nLen = pszEnd > pszNext ? pszEnd - pszNext : 0;
               pszRetVal = ( char * ) zh_xgrab( nLen + 1 );
               zh_strncpy( pszRetVal, pszNext, nLen );
            }
            else
               pszRetVal = ( char * ) "";
            break;
         }

         /* Step to the next switch */
         pszNext = pszEnd;
      }
   }

   if( pszEnvVar )
      zh_xfree( pszEnvVar );

   return pszRetVal;
}

ZH_BOOL zh_cmdargCheck( const char * pszName )
{
   return zh_cmdargGet( pszName, ZH_FALSE ) != NULL;
}

/* NOTE: Pointer must be freed with zh_xfree() if not NULL */

char * zh_cmdargString( const char * pszName )
{
   return zh_cmdargGet( pszName, ZH_TRUE );
}

int zh_cmdargNum( const char * pszName )
{
   char * pszValue;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_cmdargNum(%s)", pszName ) );

   pszValue = zh_cmdargGet( pszName, ZH_TRUE );
   if( pszValue )
   {
      int iValue = atoi( pszValue );

      zh_xfree( pszValue );

      return iValue;
   }
   else
      return -1;
}

/* NOTE: Pointer must be freed with zh_xfree() if not NULL */

char * zh_cmdargProgName( void )
{
   return zh_cmdargDup( 0 );
}

/* NOTE: Pointer must be freed with zh_xfree() if not NULL */

char * zh_cmdargBaseProgName( void )
{
   char * pszProgName, * pszBaseProgName = NULL;

   pszProgName = zh_cmdargProgName();
   if( pszProgName )
   {
      PZH_FNAME pFileName = zh_fsFNameSplit( pszProgName );

      pszBaseProgName = zh_strdup( pFileName->szName );
      zh_xfree( pFileName );
      zh_xfree( pszProgName );
   }

   return pszBaseProgName;
}

/* Check if an internal switch has been set */

ZH_FUNC( ZH_ARGCHECK )
{
   zh_retl( ZH_ISCHAR( 1 ) ? zh_cmdargCheck( zh_parc( 1 ) ) : ZH_FALSE );
}

/* Returns the value of an internal switch */

ZH_FUNC( ZH_ARGSTRING )
{
   const char * pszName = zh_parc( 1 );

   if( pszName )
   {
      char * pszValue = zh_cmdargString( pszName );

      if( pszValue )
      {
         zh_retc_buffer( pszValue );
         return;
      }
   }

   zh_retc_null();
}

/* Returns the number of command-line arguments passed to the application, this
   also includes the internal arguments. */

ZH_FUNC( ZH_ARGC )
{
   zh_retni( s_argc - 1 );
}

/* Returns a command-line argument passed to the application. Calling it with
   the parameter zero or no parameter, it will return the name of the executable,
   as written in the command-line. */

ZH_FUNC( ZH_ARGV )
{
   char * pszArg = zh_cmdargDup( zh_parni( 1 ) );

   if( pszArg )
      zh_retc_buffer( pszArg );
   else
      zh_retc_null();
}

ZH_FUNC( ZH_ARGSHIFT )
{
   int iArg = 1;

   if( zh_parl( 1 ) )
   {
      while( iArg < s_argc )
      {
         if( ! zh_cmdargIsInternal( s_argv[ iArg ], NULL ) )
         {
            s_argv[ 0 ] = s_argv[ iArg ];
#if defined( ZH_OS_WIN )
            if( s_lpArgV )
               s_lpArgV[ 0 ] = s_lpArgV[ iArg ];
#endif
            break;
         }
         ++iArg;
      }
   }
   if( iArg < s_argc )
   {
      --s_argc;
      while( iArg < s_argc )
      {
         s_argv[ iArg ] = s_argv[ iArg + 1 ];
#if defined( ZH_OS_WIN )
         if( s_lpArgV )
            s_lpArgV[ iArg ] = s_lpArgV[ iArg + 1 ];
#endif
         ++iArg;
      }
   }
}

ZH_FUNC( ZH_ACMDLINE )
{
   if( s_argc > 1 )
   {
      int iPos, iLen = s_argc - 1;
      PZH_ITEM pArray = zh_itemArrayNew( iLen );

      for( iPos = 1; iPos <= iLen; ++iPos )
         zh_arraySetCPtr( pArray, iPos, zh_cmdargDup( iPos ) );

      zh_itemReturnRelease( pArray );
   }
   else
      zh_reta( 0 );
}

ZH_FUNC( ZH_CMDLINE )
{
   if( s_argc > 1 )
   {
      ZH_SIZE nLen = 0;
      int iArg;

#if defined( ZH_OS_WIN )
      if( s_lpArgV )
      {
         LPTSTR lpBuffer, ptr;

         for( iArg = 1; iArg < s_argc; iArg++ )
            nLen += ZH_STRLEN( s_lpArgV[ iArg ] ) + 1;

         ptr = lpBuffer = ( LPTSTR ) zh_xgrab( nLen * sizeof( TCHAR ) );
         for( iArg = 1; iArg < s_argc; iArg++ )
         {
            nLen = ZH_STRLEN( s_lpArgV[ iArg ] );
            memcpy( ptr, s_lpArgV[ iArg ], nLen * sizeof( TCHAR ) );
            ptr += nLen;
            *ptr++ = TEXT( ' ' );
         }
         *--ptr = TEXT( '\0' );

         /* Convert from OS codepage */
#if defined( UNICODE )
         ZH_RETSTR( lpBuffer );
         zh_xfree( lpBuffer );
#else
         zh_retc_buffer( ( char * ) zh_osDecodeCP( lpBuffer, NULL, NULL ) );
#endif
      }
      else
#endif
      {
         char * pszBuffer, * ptr;

         for( iArg = 1; iArg < s_argc; iArg++ )
            nLen += strlen( s_argv[ iArg ] ) + 1;

         ptr = pszBuffer = ( char * ) zh_xgrab( nLen );
         for( iArg = 1; iArg < s_argc; iArg++ )
         {
            nLen = strlen( s_argv[ iArg ] );
            memcpy( ptr, s_argv[ iArg ], nLen );
            ptr += nLen;
            *ptr++ = ' ';
         }
         *--ptr = '\0';

         /* Convert from OS codepage */
         zh_retc_buffer( ( char * ) ZH_UNCONST( zh_osDecodeCP( pszBuffer, NULL, NULL ) ) );
      }
   }
   else
      zh_retc_null();
}

/* Check for command-line internal arguments */
void zh_cmdargProcess( void )
{
   int iHandles;

   if( zh_cmdargCheck( "INFO" ) )
   {
      {
         char * pszVersion = zh_verZiher();
         zh_conOutErr( pszVersion, 0 );
         zh_conOutErr( zh_conNewLine(), 0 );
         zh_xfree( pszVersion );
      }

      {
         char * pszVersion = zh_verPlatform();
         zh_conOutErr( pszVersion, 0 );
         zh_conOutErr( zh_conNewLine(), 0 );
         zh_xfree( pszVersion );
      }

      {
         char buffer[ 128 ];
         zh_snprintf( buffer, sizeof( buffer ), "DS avail=%" ZH_PFS "uKB  OS avail=%" ZH_PFS "uKB  EMM avail=%" ZH_PFS "uKB  MemStat:%s  MT:%s", zh_xquery( ZH_MEM_BLOCK ), zh_xquery( ZH_MEM_VM ), zh_xquery( ZH_MEM_EMS ), zh_xquery( ZH_MEM_USEDMAX ) ? "On" : "Off", zh_vmIsMt() ? "On" : "Off" );
         zh_conOutErr( buffer, 0 );
         zh_conOutErr( zh_conNewLine(), 0 );
      }
   }

   if( zh_cmdargCheck( "BUILD" ) )
      zh_verBuildInfoCB( zh_conOutErr );

   iHandles = zh_cmdargNum( "F" );
   if( iHandles > 20 )
   {
      #if defined( __WATCOMC__ )
         #if defined( ZH_OS_OS2 )
            DosSetMaxFH( iHandles );
         #elif defined( ZH_OS_DOS )
            _grow_handles( iHandles );
         #endif
      #endif
   }
   else if( iHandles < 0 )
   {
      #if defined( __WATCOMC__ )
         #if defined( ZH_OS_OS2 )
            DosSetMaxFH( 256 );
         #endif
      #endif
   }
}


/* Last commit string */
const char * zh_verCommitInfo( void )
{
   return ZH_VER_COMMIT_INFO;
}



/* build time Ziher platform setting */
const char * zh_verZH_PLAT( void )
{
#ifdef ZH_PLATFORM
   return ZH_PLATFORM;
#else
   return "";
#endif
}

/* build time Ziher compiler setting */
const char * zh_verZH_COMP( void )
{
#ifdef ZH_COMPILER
   return ZH_COMPILER;
#else
   return "";
#endif
}
