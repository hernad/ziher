/*
 * Version detection functions
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
 * Copyright 1999 Luiz Rafael Culik <culik@sl.conex.net>
 *    zh_verPlatform() (support for determining the Windows version)
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
 *    zh_verPlatform() (support for determining many Windows flavours)
 *    zh_verCompiler() (support for determining some compiler version/revision)
 * Copyright 2000-2014 Viktor Szakats
 *    zh_verCPU(), zh_verHostBitWidth(), zh_iswinver(), zh_iswinsp()
 *    zh_verPlatform() (support for detecting Windows NT on DOS, Wine, post-Windows 8, cleanups)
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
#include "memory.zhh"

#if defined( ZH_OS_WIN )

   #include <windows.h>
   #include "zh_win_unicode.h"


   #ifndef VER_PLATFORM_WIN32_WINDOWS
   #define VER_PLATFORM_WIN32_WINDOWS  1
   #endif
   #ifndef VER_PLATFORM_WIN32_CE
   #define VER_PLATFORM_WIN32_CE  3
   #endif

   #ifndef VER_NT_WORKSTATION
   #define VER_NT_WORKSTATION  0x0000001
   #endif
   #ifndef VER_NT_DOMAIN_CONTROLLER
   #define VER_NT_DOMAIN_CONTROLLER  0x0000002
   #endif
   #ifndef VER_NT_SERVER
   #define VER_NT_SERVER  0x0000003
   #endif

   #ifndef VER_MINORVERSION
   #define VER_MINORVERSION  0x0000001
   #endif
   #ifndef VER_MAJORVERSION
   #define VER_MAJORVERSION  0x0000002
   #endif
   #ifndef VER_SERVICEPACKMINOR
   #define VER_SERVICEPACKMINOR  0x0000010
   #endif
   #ifndef VER_SERVICEPACKMAJOR
   #define VER_SERVICEPACKMAJOR  0x0000020
   #endif

   #ifndef VER_PRODUCT_TYPE
   #define VER_PRODUCT_TYPE  0x0000080
   #endif
   #ifndef VER_EQUAL
   #define VER_EQUAL  1
   #endif
   #ifndef VER_GREATER_EQUAL
   #define VER_GREATER_EQUAL  3
   #endif

   #ifndef SM_SERVERR2
   #define SM_SERVERR2  89
   #endif

#elif defined( ZH_OS_UNIX )
   #include <sys/utsname.h>
#endif

const char * zh_verCPU( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_verCPU()" ) );

#if   defined( ZH_CPU_X86 )
   return "x86";
#elif defined( ZH_CPU_X86_64 )
   return "x86-64";
#elif defined( ZH_CPU_ARM )
   return "ARM";
#elif defined( ZH_CPU_ARM_64 )
   return "ARM64";
#else
   return "(unrecognized)";
#endif
}

static ZH_BOOL s_win_iswow64( void )
{
   ZH_BOOL bRetVal = ZH_FALSE;

   #if defined( ZH_OS_WIN ) && ! defined( ZH_OS_WIN_64 )
   {
      typedef BOOL ( WINAPI * P_ISWOW64PROCESS )( HANDLE, PBOOL );

      P_ISWOW64PROCESS pIsWow64Process;

      HMODULE hModule = GetModuleHandle( TEXT( "kernel32" ) );

      if( hModule )
         pIsWow64Process = ( P_ISWOW64PROCESS ) ZH_WINAPI_GETPROCADDRESS( hModule, "IsWow64Process" );
      else
         pIsWow64Process = NULL;

      if( pIsWow64Process )
      {
         BOOL bIsWow64 = FALSE;

         if( ! pIsWow64Process( GetCurrentProcess(), &bIsWow64 ) )
         {
            /* Try alternative method? */
         }

         if( bIsWow64 )
            bRetVal = ZH_TRUE;
      }
   }
   #endif

   return bRetVal;
}

const char * zh_verHostCPU( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_verHostCPU()" ) );

   if( s_win_iswow64() )
      return "x86-64";

   return zh_verCPU();
}

int zh_verHostBitWidth( void )
{
   int nBits;

   /* Inherit the bit width we're building for */
   #if   defined( ZH_ARCH_64BIT )
      nBits = 64;
   #elif defined( ZH_ARCH_32BIT )
      nBits = 32;
   #elif defined( ZH_ARCH_16BIT )
      nBits = 16;
   #else
      nBits = 0;
   #endif

   if( s_win_iswow64() )
      nBits = 64;

   return nBits;
}

/* NOTE: OS() function, as a primary goal will detect the version number
         of the target platform. As an extra it may also detect the host OS.
         The latter is mainly an issue in DOS, where the host OS can be OS/2
         WinNT/2K, Win3x, Win9x, DOSEMU, Desqview, etc. [vszakats] */

/* NOTE: The caller must free the returned buffer. [vszakats] */

/* NOTE: The first word of the returned string must describe
         the OS family as used in __PLATFORM__*. Latter macro
         will in fact be formed from the string returned
         by this function. [vszakats] */

/* NOTE: As it appears in __PLATFORM__* macro */
const char * zh_verPlatformMacro( void )
{
#if defined( ZH_OS_WIN )
   return "WINDOWS";          /* TODO: Change this to WIN for consistency? */
#elif defined( ZH_OS_LINUX )
   return "LINUX";
#elif defined( ZH_OS_DARWIN )
   return "DARWIN";
#else
   return NULL;
#endif
}

#if defined( ZH_OS_WIN )

static ZH_BOOL s_fWinVerInit = ZH_FALSE;

static ZH_BOOL s_fWin10    = ZH_FALSE;
static ZH_BOOL s_fWin81    = ZH_FALSE;
static ZH_BOOL s_fWin8     = ZH_FALSE;
static ZH_BOOL s_fWin7     = ZH_FALSE;
static ZH_BOOL s_fWinVista = ZH_FALSE;
static ZH_BOOL s_fWin2K3   = ZH_FALSE;
static ZH_BOOL s_fWin2K    = ZH_FALSE;
static int     s_iWinNT    = 0;
static int     s_iWin9x    = 0;
static int     s_iWine     = 0;

#if ( defined( _MSC_VER ) && _MSC_VER < 1400 )

   typedef struct _OSVERSIONINFOEXW
   {
      DWORD dwOSVersionInfoSize;
      DWORD dwMajorVersion;
      DWORD dwMinorVersion;
      DWORD dwBuildNumber;
      DWORD dwPlatformId;
      WCHAR szCSDVersion[ 128 ];
      WORD  wServicePackMajor;
      WORD  wServicePackMinor;
      WORD  wSuiteMask;
      BYTE  wProductType;
      BYTE  wReserved;
   } OSVERSIONINFOEXW, * LPOSVERSIONINFOEXW;
#endif

typedef BOOL ( WINAPI * _ZH_VERIFYVERSIONINFO )( LPOSVERSIONINFOEXW, DWORD, DWORDLONG );
typedef ULONGLONG ( WINAPI * _ZH_VERSETCONDITIONMASK )( ULONGLONG, DWORD, BYTE );

static ZH_BOOL s_fVerInfoInit = ZH_TRUE;
static _ZH_VERIFYVERSIONINFO   s_pVerifyVersionInfo   = NULL;
static _ZH_VERSETCONDITIONMASK s_pVerSetConditionMask = NULL;

static ZH_BOOL s_zh_winVerifyVersionInit( void )
{
   if( s_fVerInfoInit )
   {
      HMODULE hModule = GetModuleHandle( TEXT( "kernel32.dll" ) );
      if( hModule )
      {
         s_pVerifyVersionInfo = ( _ZH_VERIFYVERSIONINFO ) ZH_WINAPI_GETPROCADDRESS( hModule, "VerifyVersionInfoW" );
         s_pVerSetConditionMask = ( _ZH_VERSETCONDITIONMASK ) ZH_WINAPI_GETPROCADDRESS( hModule, "VerSetConditionMask" );
      }
      s_fVerInfoInit = ZH_FALSE;
   }

   return s_pVerifyVersionInfo &&
          s_pVerSetConditionMask;
}



static void s_zh_winVerInit( void )
{

   s_fWin10    = zh_iswinver( 10, 0, 0, ZH_TRUE );
   s_fWin81    = zh_iswinver( 6, 3, 0, ZH_TRUE );
   s_fWin8     = zh_iswinver( 6, 2, 0, ZH_TRUE );
   s_fWin7     = zh_iswinver( 6, 1, 0, ZH_TRUE );
   s_fWinVista = zh_iswinver( 6, 0, 0, ZH_TRUE );
   s_fWin2K3   = zh_iswinver( 5, 2, VER_NT_SERVER, ZH_TRUE ) || zh_iswinver( 5, 2, VER_NT_DOMAIN_CONTROLLER, ZH_TRUE );
   s_fWin2K    = zh_iswinver( 5, 0, 0, ZH_TRUE );

#if !( defined( ZH_OS_WIN_64 ) || ( defined( _MSC_VER ) && _MSC_VER > 1310 ) )
   {
      OSVERSIONINFO osvi;
      osvi.dwOSVersionInfoSize = sizeof( osvi );
      if( GetVersionEx( &osvi ) )
      {
         /* NOTE: Value is VER_PLATFORM_WIN32_CE on WinCE */
         if( osvi.dwPlatformId != VER_PLATFORM_WIN32_WINDOWS )
            s_iWin9x = 0;
         else if( osvi.dwMajorVersion == 4 && osvi.dwMinorVersion < 10 )
            s_iWin9x = 5;  /* 95 */
         else if( osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 10 )
            s_iWin9x = 8;  /* 98 */
         else
            s_iWin9x = 9;  /* ME */

         if( osvi.dwPlatformId != VER_PLATFORM_WIN32_NT )
            s_iWinNT = 0;
         else if( osvi.dwMajorVersion == 3 && osvi.dwMinorVersion == 51 )
            s_iWinNT = 3;  /* 3.51 */
         else if( osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 0 )
            s_iWinNT = 4;  /* 4.0 */
         else
            s_iWinNT = 5;  /* newer */
      }
   }
#endif

   {
      /* NOTE: Unofficial Wine detection.
               https://www.mail-archive.com/wine-devel@winehq.org/msg48659.html */
      HMODULE hntdll = GetModuleHandle( TEXT( "ntdll.dll" ) );
      if( hntdll && ZH_WINAPI_GETPROCADDRESS( hntdll, "wine_get_version" ) )
         s_iWine = 1;
   }

   if( s_fWin2K )
      s_iWinNT = 5;


   s_fWinVerInit = ZH_TRUE;
}

#endif

/* NOTE: Must be larger than 128, which is the maximum size of
         osvi.szCSDVersion (Windows). [vszakats] */
#define PLATFORM_BUF_SIZE  255

char * zh_verPlatform( void )
{
   char * pszPlatform;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_verPlatform()" ) );

   pszPlatform = ( char * ) zh_xgrab( PLATFORM_BUF_SIZE + 1 );


#if defined( ZH_OS_WIN )

   {
      const char * pszName = "";

      OSVERSIONINFO osvi;

      memset( &osvi, 0, sizeof( osvi ) );


      /* Detection of legacy Windows versions */
      switch( zh_iswin9x() )
      {
         case 5:
            osvi.dwMajorVersion = 4;
            osvi.dwMinorVersion = 0;
            pszName = " 95";
            break;
         case 8:
            osvi.dwMajorVersion = 4;
            osvi.dwMinorVersion = 10;
            pszName = " 98";
            break;
         case 9:
            osvi.dwMajorVersion = 4;
            osvi.dwMinorVersion = 90;
            pszName = " ME";
            break;
      }

      if( pszName[ 0 ] == '\0' )
      {

         if( zh_iswinver( 11, 0, 0, ZH_TRUE ) )
         {
            osvi.dwMajorVersion = 11;
            osvi.dwMinorVersion = 0;
            pszName = " 11 or newer";
         }
         else if( zh_iswin10() )
         {
            osvi.dwMajorVersion = 10;
            osvi.dwMinorVersion = 0;
            if( zh_iswinver( 10, 0, VER_NT_WORKSTATION, ZH_FALSE ) )
               pszName = " 10";
            else
               pszName = " Server 2016";
         }
         else if( zh_iswin81() )
         {
            osvi.dwMajorVersion = 6;
            osvi.dwMinorVersion = 3;
            if( zh_iswinver( 6, 3, VER_NT_WORKSTATION, ZH_FALSE ) )
               pszName = " 8.1";
            else
               pszName = " Server 2012 R2";
         }
         else if( zh_iswinvista() )
         {
            if( zh_iswin8() )
            {
               osvi.dwMajorVersion = 6;
               osvi.dwMinorVersion = 2;
               if( zh_iswinver( 6, 2, VER_NT_WORKSTATION, ZH_FALSE ) )
                  pszName = " 8";
               else
                  pszName = " Server 2012";
            }
            else if( zh_iswinver( 6, 1, 0, ZH_FALSE ) )
            {
               osvi.dwMajorVersion = 6;
               osvi.dwMinorVersion = 1;
               if( zh_iswinver( 6, 1, VER_NT_WORKSTATION, ZH_FALSE ) )
                  pszName = " 7";
               else
                  pszName = " Server 2008 R2";
            }
            else
            {
               osvi.dwMajorVersion = 6;
               osvi.dwMinorVersion = 0;
               if( zh_iswinver( 6, 0, VER_NT_WORKSTATION, ZH_FALSE ) )
                  pszName = " Vista";
               else
                  pszName = " Server 2008";
            }
         }
         else if( zh_iswinver( 5, 2, 0, ZH_FALSE ) )
         {
            osvi.dwMajorVersion = 5;
            osvi.dwMinorVersion = 2;
            if( zh_iswinver( 5, 2, VER_NT_WORKSTATION, ZH_FALSE ) )
               pszName = " XP x64";
            else if( GetSystemMetrics( SM_SERVERR2 ) != 0 )
               pszName = " Server 2003 R2";
            else
               pszName = " Server 2003";
         }
         else if( zh_iswinver( 5, 1, 0, ZH_FALSE ) )
         {
            osvi.dwMajorVersion = 5;
            osvi.dwMinorVersion = 1;
            pszName = " XP";
         }
         else if( zh_iswin2k() )
         {
            osvi.dwMajorVersion = 5;
            osvi.dwMinorVersion = 0;
            pszName = " 2000";
         }
         else
            pszName = " NT";
      }

      zh_snprintf( pszPlatform, PLATFORM_BUF_SIZE + 1, "Windows%s%s %lu.%lu",
                   pszName,
                   s_iWine ? " (Wine)" : "",
                   osvi.dwMajorVersion,
                   osvi.dwMinorVersion );

      /* Add service pack/other info */

      if( zh_iswin2k() )
      {
         int tmp;

         for( tmp = 5; tmp > 0; --tmp )
         {
            if( zh_iswinsp( tmp, ZH_TRUE ) )
            {
               char szServicePack[ 8 ];
               zh_snprintf( szServicePack, sizeof( szServicePack ), " SP%u", tmp );
               zh_strncat( pszPlatform, szServicePack, PLATFORM_BUF_SIZE );
               break;
            }
         }
      }

   }

#elif defined( ZH_OS_UNIX )

   {
      struct utsname un;

      uname( &un );
      zh_snprintf( pszPlatform, PLATFORM_BUF_SIZE + 1, "%s %s %s", un.sysname, un.release, un.machine );
   }

#else

   {
      zh_strncpy( pszPlatform, "(unrecognized)", PLATFORM_BUF_SIZE );
   }

#endif

   return pszPlatform;
}

ZH_BOOL zh_iswinver( int iMajor, int iMinor, int iType, ZH_BOOL fOrUpper )
{
#if defined( ZH_OS_WIN )
   if( s_zh_winVerifyVersionInit() )
   {
      OSVERSIONINFOEXW ver;
      DWORD dwTypeMask = VER_MAJORVERSION | VER_MINORVERSION;
      DWORDLONG dwlConditionMask = 0;

      memset( &ver, 0, sizeof( ver ) );
      ver.dwOSVersionInfoSize = sizeof( ver );
      ver.dwMajorVersion = ( DWORD ) iMajor;
      ver.dwMinorVersion = ( DWORD ) iMinor;

      dwlConditionMask = s_pVerSetConditionMask( dwlConditionMask, VER_MAJORVERSION, fOrUpper ? VER_GREATER_EQUAL : VER_EQUAL );
      dwlConditionMask = s_pVerSetConditionMask( dwlConditionMask, VER_MINORVERSION, fOrUpper ? VER_GREATER_EQUAL : VER_EQUAL );

      /* MSDN says in https://msdn.microsoft.com/library/ms725492
           "If you are testing the major version, you must also test the
            minor version and the service pack major and minor versions."
         However, Wine (as of 1.7.53) breaks on this. Since native Windows
         apparently doesn't care, we're not doing it for now.
         Wine (emulating Windows 7) will erroneously return ZH_FALSE from
         these calls:
           zh_iswinver( 6, 1, 0, ZH_FALSE );
           zh_iswinver( 6, 1, VER_NT_WORKSTATION, ZH_FALSE );
         Removing the Service Pack check, or changing ZH_FALSE to ZH_TRUE
         in above calls, both fixes the problem. [vszakats] */
#if defined( __ZH_DISABLE_WINE_VERIFYVERSIONINFO_BUG_WORKAROUND )
      ver.wServicePackMajor =
      ver.wServicePackMinor = ( WORD ) 0;
      dwTypeMask |= VER_SERVICEPACKMAJOR | VER_SERVICEPACKMINOR;
      dwlConditionMask = s_pVerSetConditionMask( dwlConditionMask, VER_SERVICEPACKMAJOR, VER_GREATER_EQUAL );
      dwlConditionMask = s_pVerSetConditionMask( dwlConditionMask, VER_SERVICEPACKMINOR, VER_GREATER_EQUAL );
#endif

      if( iType )
      {
         dwTypeMask |= VER_PRODUCT_TYPE;
         ver.wProductType = ( BYTE ) iType;
         dwlConditionMask = s_pVerSetConditionMask( dwlConditionMask, VER_PRODUCT_TYPE, VER_EQUAL );
      }

      return ( ZH_BOOL ) s_pVerifyVersionInfo( &ver, dwTypeMask, dwlConditionMask );
   }
#else
   ZH_SYMBOL_UNUSED( iMajor );
   ZH_SYMBOL_UNUSED( iMinor );
   ZH_SYMBOL_UNUSED( iType );
   ZH_SYMBOL_UNUSED( fOrUpper );
#endif
   return ZH_FALSE;
}

ZH_BOOL zh_iswinsp( int iServicePackMajor, ZH_BOOL fOrUpper )
{
#if defined( ZH_OS_WIN )
   if( s_zh_winVerifyVersionInit() )
   {
      OSVERSIONINFOEXW ver;
      DWORDLONG dwlConditionMask = 0;

      memset( &ver, 0, sizeof( ver ) );
      ver.dwOSVersionInfoSize = sizeof( ver );
      ver.wServicePackMajor = ( WORD ) iServicePackMajor;

      dwlConditionMask = s_pVerSetConditionMask( dwlConditionMask, VER_SERVICEPACKMAJOR, fOrUpper ? VER_GREATER_EQUAL : VER_EQUAL );

      return ( ZH_BOOL ) s_pVerifyVersionInfo( &ver, VER_SERVICEPACKMAJOR, dwlConditionMask );
   }
#else
   ZH_SYMBOL_UNUSED( iServicePackMajor );
   ZH_SYMBOL_UNUSED( fOrUpper );
#endif
   return ZH_FALSE;
}

int zh_iswine( void )
{
#if defined( ZH_OS_WIN )
   if( ! s_fWinVerInit )
      s_zh_winVerInit();
   return s_iWine;
#else
   return 0;
#endif
}

ZH_BOOL zh_iswin10( void )
{
#if defined( ZH_OS_WIN )
   if( ! s_fWinVerInit )
      s_zh_winVerInit();
   return s_fWin10;
#else
   return ZH_FALSE;
#endif
}

ZH_BOOL zh_iswin81( void )
{
#if defined( ZH_OS_WIN )
   if( ! s_fWinVerInit )
      s_zh_winVerInit();
   return s_fWin81;
#else
   return ZH_FALSE;
#endif
}

ZH_BOOL zh_iswin8( void )
{
#if defined( ZH_OS_WIN )
   if( ! s_fWinVerInit )
      s_zh_winVerInit();
   return s_fWin8;
#else
   return ZH_FALSE;
#endif
}

ZH_BOOL zh_iswin7( void )
{
#if defined( ZH_OS_WIN )
   if( ! s_fWinVerInit )
      s_zh_winVerInit();
   return s_fWin7;
#else
   return ZH_FALSE;
#endif
}

ZH_BOOL zh_iswinvista( void )
{
#if defined( ZH_OS_WIN )
   if( ! s_fWinVerInit )
      s_zh_winVerInit();
   return s_fWinVista;
#else
   return ZH_FALSE;
#endif
}

ZH_BOOL zh_iswin2k3( void )
{
#if defined( ZH_OS_WIN )
   if( ! s_fWinVerInit )
      s_zh_winVerInit();
   return s_fWin2K3;
#else
   return ZH_FALSE;
#endif
}

ZH_BOOL zh_iswin2k( void )
{
#if defined( ZH_OS_WIN )
   if( ! s_fWinVerInit )
      s_zh_winVerInit();
   return s_fWin2K;
#else
   return ZH_FALSE;
#endif
}

int zh_iswinnt( void )
{
#if defined( ZH_OS_WIN )
   if( ! s_fWinVerInit )
      s_zh_winVerInit();
   return s_iWinNT;
#else
   return 0;
#endif
}

int zh_iswin9x( void )
{
#if defined( ZH_OS_WIN )
   if( ! s_fWinVerInit )
      s_zh_winVerInit();
   return s_iWin9x;
#else
   return 0;
#endif
}

ZH_BOOL zh_iswince( void )
{
   return ZH_FALSE;
}

/* NOTE: The caller must free the returned buffer. [vszakats] */

#define COMPILER_BUF_SIZE  80

char * zh_verCompiler( void )
{
   char * pszCompiler;
   const char * pszName;
   char szSub[ 64 ];
   int iVerMajor;
   int iVerMinor;
   int iVerPatch;
   int iVerMicro = 0;
   int iElements = 0;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_verCompiler()" ) );

   pszCompiler = ( char * ) zh_xgrab( COMPILER_BUF_SIZE );
   szSub[ 0 ] = '\0';


#if defined( __clang__ ) && defined( __clang_major__ )

   /* NOTE: keep clang detection before msvc detection. */

   pszName = "LLVM/Clang C";

   #if defined( __cplusplus )
      zh_strncpy( szSub, "++", sizeof( szSub ) - 1 );
   #endif

   iVerMajor = __clang_major__;
   iVerMinor = __clang_minor__;
   iVerPatch = __clang_patchlevel__;

#elif defined( __clang__ )

   pszName = "LLVM/Clang C";

   #if defined( __cplusplus )
      zh_strncpy( szSub, "++", sizeof( szSub ) - 1 );
   #endif

   zh_strncat( szSub, " 1.x", sizeof( szSub ) - 1 );

   iVerMajor = iVerMinor = iVerPatch = 0;

#elif defined( __llvm__ ) && defined( __GNUC__ )

   pszName = "LLVM/GNU C";

   #if defined( __cplusplus )
      zh_strncpy( szSub, "++", sizeof( szSub ) - 1 );
   #endif

   iVerMajor = __GNUC__;
   iVerMinor = __GNUC_MINOR__;
   #if defined( __GNUC_PATCHLEVEL__ )
      iVerPatch = __GNUC_PATCHLEVEL__;
   #else
      iVerPatch = 0;
   #endif

#elif defined( _MSC_VER )

   #if _MSC_VER >= 800
      pszName = "Microsoft Visual C";
   #else
      pszName = "Microsoft C";
   #endif

   #if defined( __cplusplus )
      zh_strncpy( szSub, "++", sizeof( szSub ) - 1 );
   #endif

   iVerMajor = _MSC_VER / 100;
   iVerMinor = _MSC_VER % 100;

   #if defined( _MSC_FULL_VER )
      #if _MSC_VER >= 1400
         iVerPatch = _MSC_FULL_VER - ( _MSC_VER * 100000 );
      #else
         iVerPatch = _MSC_FULL_VER - ( _MSC_VER * 10000 );
      #endif
   #else
      iVerPatch = 0;
   #endif


#elif defined( __GNUC__ )

   #if defined( __MINGW32__ )
      pszName = "MinGW GNU C";
   #else
      pszName = "GNU C";
   #endif

   #if defined( __cplusplus )
      zh_strncpy( szSub, "++", sizeof( szSub ) - 1 );
   #endif

   iVerMajor = __GNUC__;
   iVerMinor = __GNUC_MINOR__;
   #if defined( __GNUC_PATCHLEVEL__ )
      iVerPatch = __GNUC_PATCHLEVEL__;
   #else
      iVerPatch = 0;
   #endif

#else

   pszName = NULL;
   iVerMajor = iVerMinor = iVerPatch = 0;

#endif

   if( pszName )
   {
      if( iElements == 4 )
         zh_snprintf( pszCompiler, COMPILER_BUF_SIZE, "%s%s %d.%d.%d.%d", pszName, szSub, iVerMajor, iVerMinor, iVerPatch, iVerMicro );
      else if( iVerPatch != 0 )
         zh_snprintf( pszCompiler, COMPILER_BUF_SIZE, "%s%s %d.%d.%d", pszName, szSub, iVerMajor, iVerMinor, iVerPatch );
      else if( iVerMajor != 0 || iVerMinor != 0 )
         zh_snprintf( pszCompiler, COMPILER_BUF_SIZE, "%s%s %d.%d", pszName, szSub, iVerMajor, iVerMinor );
      else
         zh_snprintf( pszCompiler, COMPILER_BUF_SIZE, "%s%s", pszName, szSub );
   }
   else
      zh_strncpy( pszCompiler, "(unrecognized)", COMPILER_BUF_SIZE - 1 );

#if defined( __clang_version__ )
   if( strstr( __clang_version__, "(" ) )
      /* "2.0 (trunk 103176)" -> "(trunk 103176)" */
      zh_snprintf( szSub, sizeof( szSub ), " %s", strstr( __clang_version__, "(" ) );
   else
      zh_snprintf( szSub, sizeof( szSub ), " (%s)", __clang_version__ );
   zh_strncat( pszCompiler, szSub, COMPILER_BUF_SIZE - 1 );
#endif

   #if defined( ZH_ARCH_32BIT )
      zh_strncat( pszCompiler, " (32-bit)", COMPILER_BUF_SIZE - 1 );
   #elif defined( ZH_ARCH_64BIT )
      zh_strncat( pszCompiler, " (64-bit)", COMPILER_BUF_SIZE - 1 );
   #endif

   return pszCompiler;
}

/* NOTE: The caller must free the returned buffer. [vszakats] */

char * zh_verZiher( void )
{
   char * pszVersion;
   char szDateRaw[ 11 ];
   char szDate[ 17 ];

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_verZiher()" ) );

   zh_snprintf( szDateRaw, sizeof( szDateRaw ), "%d", zh_verCommitRev() );

   szDate[ 0 ] = '2';
   szDate[ 1 ] = '0';
   szDate[ 2 ] = szDateRaw[ 0 ];
   szDate[ 3 ] = szDateRaw[ 1 ];
   szDate[ 4 ] = '-';
   szDate[ 5 ] = szDateRaw[ 2 ];
   szDate[ 6 ] = szDateRaw[ 3 ];
   szDate[ 7 ] = '-';
   szDate[ 8 ] = szDateRaw[ 4 ];
   szDate[ 9 ] = szDateRaw[ 5 ];
   szDate[ 10 ] = ' ';
   szDate[ 11 ] = szDateRaw[ 6 ];
   szDate[ 12 ] = szDateRaw[ 7 ];
   szDate[ 13 ] = ':';
   szDate[ 14 ] = szDateRaw[ 8 ];
   szDate[ 15 ] = szDateRaw[ 9 ];
   szDate[ 16 ] = '\0';

   pszVersion = ( char * ) zh_xgrab( 80 );
   zh_snprintf( pszVersion, 80, "Ziher %d.%d.%d%s (%s) (%s)",
                ZH_VER_MAJOR, ZH_VER_MINOR, ZH_VER_RELEASE, ZH_VER_STATUS,
                zh_verCommitIDShort(), szDate );

   return pszVersion;
}

char * zh_verPCode( void )
{
   char * pszPCode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_verPCode()" ) );

   pszPCode = ( char * ) zh_xgrab( 24 );
   zh_snprintf( pszPCode, 24, "PCode version: %d.%d",
                ZH_PCODE_VER >> 8, ZH_PCODE_VER & 0xFF );

   return pszPCode;
}
