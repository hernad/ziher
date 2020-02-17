/*
 * Version related functions
 *
 * Copyright 1999-2015 Viktor Szakats
 * Copyright 2013 Przemyslaw Czerpak (timestamp conversion)
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
#include "zh_vm.h"
#include "zh_date.h"

#include "ver.zhh"
#include "zh_ver_bld.h"

ZH_FUNC( OS )
{
   zh_retc_buffer( zh_verPlatform() );
}

ZH_FUNC( VERSION )
{
   zh_retc_buffer( zh_verZiher() );
}

ZH_FUNC( ZH_VERSION )
{
   switch( zh_parni( 1 ) )
   {
      case ZH_VERSION_URL_BASE:       zh_retc_const( ZH_VER_ORIGIN_URL ); break;
      case ZH_VERSION_URL_SOURCE:
      {
         char * pszVersion = zh_xstrcpy( NULL, ZH_VER_ORIGIN_URL,
            strlen( zh_verCommitID() ) ? "tree/" : NULL, zh_verCommitID(), NULL );

         zh_retclen_buffer( pszVersion, strlen( pszVersion ) );
         break;
      }
      case ZH_VERSION_ZIHER:        zh_retc_buffer( zh_verZiher() ); break;
      case ZH_VERSION_COMPILER:       zh_retc_buffer( zh_verCompiler() ); break;
      case ZH_VERSION_MAJOR:          zh_retni( ZH_VER_MAJOR ); break;
      case ZH_VERSION_MINOR:          zh_retni( ZH_VER_MINOR ); break;
      case ZH_VERSION_RELEASE:        zh_retni( ZH_VER_RELEASE ); break;
      case ZH_VERSION_STATUS:         zh_retc_const( ZH_VER_STATUS ); break;
      case ZH_VERSION_REVISION:       zh_retni( zh_verCommitRev() ); break;
      case ZH_VERSION_COMMIT_INFO:    zh_retc_const( zh_verCommitInfo() ); break;
      case ZH_VERSION_ID:             zh_retc_const( zh_verCommitID() ); break;
      case ZH_VERSION_ID_SHORT:       zh_retc_const( zh_verCommitIDShort() ); break;
      case ZH_VERSION_PCODE_VER:      zh_retni( ZH_PCODE_VER ); break;
      case ZH_VERSION_PCODE_VER_STR:  zh_retc_buffer( zh_verPCode() ); break;
      case ZH_VERSION_BUILD_PLAT:     zh_retc_const( zh_verZH_PLAT() ); break;
      case ZH_VERSION_BUILD_COMP:     zh_retc_const( zh_verZH_COMP() ); break;
      case ZH_VERSION_BUILD_DATE_STR: zh_retc_const( zh_verCommitInfo() ); break;
      case ZH_VERSION_BUILD_DATE:
      {
         const char * pszBuildDate = zh_verCommitInfo();

         if( strlen( pszBuildDate ) >= 10 )
         {
            char szDate[ 9 ];

            szDate[ 0 ] = pszBuildDate[ 0 ];
            szDate[ 1 ] = pszBuildDate[ 1 ];
            szDate[ 2 ] = pszBuildDate[ 2 ];
            szDate[ 3 ] = pszBuildDate[ 3 ];
            szDate[ 4 ] = pszBuildDate[ 5 ];
            szDate[ 5 ] = pszBuildDate[ 6 ];
            szDate[ 6 ] = pszBuildDate[ 8 ];
            szDate[ 7 ] = pszBuildDate[ 9 ];
            szDate[ 8 ] = '\0';

            zh_retds( szDate );
         }
         else
            zh_retds( NULL );

         break;
      }
      case ZH_VERSION_BUILD_TIME:
      {
         const char * pszBuildDate = zh_verCommitInfo();
         if( strlen( pszBuildDate ) >= 19 )
            zh_retclen( pszBuildDate + 11, 8 );
         else
            zh_retc_null();
         break;
      }
      case ZH_VERSION_BUILD_TIMESTAMP_UTC:
      {
         char * pszBuildDate = zh_strdup( zh_verCommitInfo() );

         if( strlen( pszBuildDate ) >= 19 )
         {
            long lJulian = 0, lMilliSec = 0;
            int iUTC = 0;

            if( strlen( pszBuildDate ) >= 25 &&
                ( pszBuildDate[ 20 ] == '+' || pszBuildDate[ 20 ] == '-' ) &&
                ZH_ISDIGIT( pszBuildDate[ 21 ] ) && ZH_ISDIGIT( pszBuildDate[ 22 ] ) &&
                ZH_ISDIGIT( pszBuildDate[ 23 ] ) && ZH_ISDIGIT( pszBuildDate[ 24 ] ) )
            {
               iUTC = ( ( int ) ( pszBuildDate[ 21 ] - '0' ) * 10 +
                        ( int ) ( pszBuildDate[ 22 ] - '0' ) ) * 60 +
                        ( int ) ( pszBuildDate[ 23 ] - '0' ) * 10 +
                        ( int ) ( pszBuildDate[ 24 ] - '0' );
               if( pszBuildDate[ 20 ] == '-' )
                  iUTC *= -1;
            }
            pszBuildDate[ 19 ] = '\0';
            zh_timeStampStrGetDT( pszBuildDate, &lJulian, &lMilliSec );
            if( iUTC != 0 )
               zh_timeStampUnpackDT( zh_timeStampPackDT( lJulian, lMilliSec ) -
                                     ( double ) iUTC / ( 24 * 60 ),
                                     &lJulian, &lMilliSec );
            zh_rettdt( lJulian, lMilliSec );
         }
         else
            zh_rettdt( 0, 0 );

         zh_xfree( pszBuildDate );

         break;
      }
      case ZH_VERSION_FLAG_PRG:       zh_retc_const( zh_verFlagsPRG() ); break;
      case ZH_VERSION_FLAG_C:         zh_retc_const( zh_verFlagsC() ); break;
      case ZH_VERSION_FLAG_LINKER:    zh_retc_const( zh_verFlagsL() ); break;
      case ZH_VERSION_OPTIONS:
      {
         char pszOptions[ 64 ];

         pszOptions[ 0 ] = '\0';

         #if defined( ZH_HAS_PCRE2 )
            zh_strncat( pszOptions, " pcre2", sizeof( pszOptions ) - 1 );
         #elif defined( ZH_POSIX_REGEX )
            zh_strncat( pszOptions, " posix_regex", sizeof( pszOptions ) - 1 );
         #endif
         #if defined( ZH_HAS_ZLIB )
            zh_strncat( pszOptions, " zlib", sizeof( pszOptions ) - 1 );
         #endif
         #if defined( ZH_HAS_GPM )
            zh_strncat( pszOptions, " gpm", sizeof( pszOptions ) - 1 );
         #endif
        

         zh_retc( pszOptions + ( pszOptions[ 0 ] == ' ' ? 1 : 0 ) );

         break;
      }
      case ZH_VERSION_BITWIDTH:       zh_retni( ( int ) sizeof( void * ) * 8 ); break;
      case ZH_VERSION_MT:             zh_retl( zh_vmIsMt() ); break;

      case ZH_VERSION_SHARED:  /* FIXME: This only works when platforms has separate
                                         compilation pass for ziher dynlib build -
                                         it is 32-bit Windows. */

      #if defined( ZH_DYNLIB )
         zh_retl( ZH_TRUE );
      #else
         zh_retl( ZH_FALSE );
      #endif
         break;

      case ZH_VERSION_UNIX_COMPAT:
      #if defined( ZH_OS_UNIX )
         zh_retl( ZH_TRUE );
      #else
         zh_retl( ZH_FALSE );
      #endif
         break;

      case ZH_VERSION_COMPILER_CPP:
      #if defined( __cplusplus )
         zh_retl( ZH_TRUE );
      #else
         zh_retl( ZH_FALSE );
      #endif
         break;

      case ZH_VERSION_PLATFORM:

      #if defined( ZH_OS_WIN )
         zh_retc_const( "WIN" );
      #else
         zh_retc_const( zh_verPlatformMacro() );
      #endif
         break;

      case ZH_VERSION_CPU:
         zh_retc_const( zh_verCPU() );
         break;

      case ZH_VERSION_ENDIANNESS:
      #if defined( ZH_LITTLE_ENDIAN )
         zh_retni( ZH_VERSION_ENDIAN_LITTLE );
      #elif defined( ZH_BIG_ENDIAN )
         zh_retni( ZH_VERSION_ENDIAN_BIG );
      #elif defined( ZH_PDP_ENDIAN )
         zh_retni( ZH_VERSION_ENDIAN_PDP );
      #else
         zh_retni( 0 );
      #endif
         break;
   }
}

ZH_FUNC( ZH_OSCPU )
{
   zh_retc_const( zh_verHostCPU() );
}

ZH_FUNC( ZH_OSIS64BIT )
{
   zh_retl( zh_verHostBitWidth() >= 64 );
}

ZH_FUNC( ZH_OSISWIN9X )
{
   zh_retl( zh_iswin9x() );
}

ZH_FUNC( ZH_OSISWINNT )
{
   zh_retl( zh_iswinnt() );
}

ZH_FUNC( ZH_OSISWIN2K )
{
   zh_retl( zh_iswin2k() );
}

ZH_FUNC( ZH_OSISWINVISTA )
{
   zh_retl( zh_iswinvista() );
}

ZH_FUNC( ZH_OSISWIN7 )
{
   zh_retl( zh_iswin7() );
}

ZH_FUNC( ZH_OSISWINCE )
{
   zh_retl( zh_iswince() );
}

/* Legacy functions */

ZH_FUNC( ZH_COMPILER )
{
   zh_retc_buffer( zh_verCompiler() );
}

