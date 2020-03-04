/*
 * DiskSpace() function
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

/* NOTE: DiskSpace() supports larger disks than 2 GiB. CA-Cl*pper will always
         return a (long) value, Ziher may return a (double) for large
         values, the decimal places are always set to zero, though. */

#include "zh_api.h"
#include "zh_error_api.h"
#include "zh_fs_api.h"

#if defined( ZH_OS_UNIX )
#  include <unistd.h>
#  include <sys/types.h>
#  if defined( ZH_OS_ANDROID )
#     include <sys/statfs.h>
#  elif defined( ZH_OS_DARWIN )
#     include <sys/param.h>
#     include <sys/mount.h>
#  else
#     include <sys/statvfs.h>
#  endif
#elif defined( ZH_OS_WIN )
#  include <windows.h>
#  include "zh_win_unicode.h"
#endif

ZH_FUNC( DISKSPACE )
{
   double dSpace = 0.0;
   ZH_BOOL bError;

#if defined( ZH_OS_WIN )
   {
#if defined( _MSC_VER ) || defined( __GNUC__ )
#  define ZH_GET_LARGE_UINT( v )  ( ( double ) (v).LowPart + \
                                    ( double ) (v).HighPart * \
                                    ( ( ( double ) 0xFFFFFFFF ) + 1 ) )

#else
   /* NOTE: For compilers that don't seem to deal with the
            unnamed struct that is part of ULARGE_INTEGER [pt] */
#  define ZH_GET_LARGE_UINT( v )  ( ( double ) (v).u.LowPart + \
                                    ( double ) (v).u.HighPart * \
                                    ( ( ( double ) 0xFFFFFFFF ) + 1 ) )
#endif

      int iDrive = zh_parni( 1 );

      if( iDrive >= 0 )
      {
         ULARGE_INTEGER i64FreeBytesToCaller, i64TotalBytes, i64FreeBytes;
         UINT uiErrMode = SetErrorMode( SEM_FAILCRITICALERRORS );

         TCHAR lpPath[ 4 ];

         if( iDrive == 0 )
            iDrive = zh_fsCurDrv() + 1;

         lpPath[ 0 ] = ( TCHAR ) ( iDrive + 'A' - 1 );
         lpPath[ 1 ] = TEXT( ':' );
         lpPath[ 2 ] = TEXT( '\\' );
         lpPath[ 3 ] = TEXT( '\0' );


         /* NOTE: We need to call this function dynamically to maintain support
                  Win95 first edition. It was introduced in Win95B (aka OSR2) [vszakats] */
         {
            typedef BOOL ( WINAPI * P_GDFSE )( LPCTSTR, PULARGE_INTEGER,
                                               PULARGE_INTEGER, PULARGE_INTEGER );

            static P_GDFSE s_pGetDiskFreeSpaceEx = ( P_GDFSE ) -1;

            if( s_pGetDiskFreeSpaceEx == ( P_GDFSE ) -1 )
            {
               HMODULE hModule = GetModuleHandle( ZH_WINAPI_KERNEL32_DLL() );
               if( hModule )
                  s_pGetDiskFreeSpaceEx = ( P_GDFSE )
                     ZH_WINAPI_GETPROCADDRESST( hModule, "GetDiskFreeSpaceEx" );
               else
                  s_pGetDiskFreeSpaceEx = NULL;
            }

            if( s_pGetDiskFreeSpaceEx )
            {
               bError = s_pGetDiskFreeSpaceEx( lpPath,
                                               ( PULARGE_INTEGER ) &i64FreeBytesToCaller,
                                               ( PULARGE_INTEGER ) &i64TotalBytes,
                                               ( PULARGE_INTEGER ) &i64FreeBytes ) ? ZH_FALSE : ZH_TRUE;
               if( ! bError )
                  dSpace = ZH_GET_LARGE_UINT( i64FreeBytesToCaller );
            }
            else
            {
               DWORD dwSectorsPerCluster;
               DWORD dwBytesPerSector;
               DWORD dwNumberOfFreeClusters;
               DWORD dwTotalNumberOfClusters;

               bError = GetDiskFreeSpace( lpPath,
                                          &dwSectorsPerCluster,
                                          &dwBytesPerSector,
                                          &dwNumberOfFreeClusters,
                                          &dwTotalNumberOfClusters ) ? ZH_FALSE : ZH_TRUE;
               if( ! bError )
                  dSpace = ( double ) dwNumberOfFreeClusters *
                           ( double ) dwSectorsPerCluster *
                           ( double ) dwBytesPerSector;
            }
         }
         SetErrorMode( uiErrMode );
      }
      else
         bError = ZH_TRUE;
   }

#elif defined( ZH_OS_UNIX )
   {
      const char * szName = zh_parc( 1 );
      char * pszFree = NULL;

      if( ! szName )
         szName = "/";
      else
         szName = zh_fsNameConv( szName, &pszFree );

      {

#if defined( ZH_OS_DARWIN ) || defined( ZH_OS_ANDROID )
         struct statfs st;
         bError = statfs( szName, &st ) != 0;
#else
         struct statvfs st;
         bError = statvfs( szName, &st ) != 0;
#endif
         if( ! bError )
         {
            if( getuid() == 0 )
               dSpace = ( double ) st.f_bfree * ( double ) st.f_bsize;
            else
               dSpace = ( double ) st.f_bavail * ( double ) st.f_bsize;
         }
      }

      if( pszFree )
         zh_xfree( pszFree );
   }
#else
   bError = ZH_FALSE;
#endif

   if( bError )
      zh_errRT_BASE_Ext1( EG_OPEN, 2018, NULL, NULL, 0, EF_CANDEFAULT, ZH_ERR_ARGS_BASEPARAMS );

   zh_retnlen( dSpace, -1, 0 );
}
