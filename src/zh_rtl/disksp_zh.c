/*
 * zh_DiskSpace() function
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

#include "zh_api.h"
#include "zh_api_error.h"
#include "zh_apifs.h"

#if defined( ZH_OS_DARWIN )
#  include <sys/param.h>
#  include <sys/mount.h>
#elif defined( ZH_OS_ANDROID )
#  include <sys/statfs.h>
#elif defined( ZH_OS_UNIX )
#     include <sys/statvfs.h>
#elif defined( ZH_OS_WIN )
#  include <windows.h>
#  include "hbwinuni.h"
#endif

double zh_fsDiskSpace( const char * pszPath, ZH_USHORT uiType )
{
   char szPathBuf[ 2 ];
   double dSpace = 0.0;

   if( uiType > ZH_DISK_TOTAL )
      uiType = ZH_DISK_AVAIL;

   if( ! pszPath || pszPath[ 0 ] == '\0' )
   {
      szPathBuf[ 0 ] = ZH_OS_PATH_DELIM_CHR;
      szPathBuf[ 1 ] = '\0';
      pszPath = szPathBuf;
   }

#if defined( ZH_OS_WIN )
   {
      LPCTSTR lpPath;
      LPTSTR lpFree;

      lpPath = ZH_FSNAMECONV( pszPath, &lpFree );

      {
         UINT uiErrMode = SetErrorMode( SEM_FAILCRITICALERRORS );
         ZH_BOOL fResult;

#if ! defined( ZH_OS_WIN_64 )
         /* NOTE: We need to call this function dynamically to maintain support
                  Win95 first edition. It was introduced in Win95B (aka OSR2) [vszakats] */
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

         if( ! s_pGetDiskFreeSpaceEx )
         {
            DWORD dwSectorsPerCluster;
            DWORD dwBytesPerSector;
            DWORD dwNumberOfFreeClusters;
            DWORD dwTotalNumberOfClusters;

            fResult = GetDiskFreeSpace( lpPath,
                                        &dwSectorsPerCluster,
                                        &dwBytesPerSector,
                                        &dwNumberOfFreeClusters,
                                        &dwTotalNumberOfClusters ) ? ZH_TRUE : ZH_FALSE;
            zh_fsSetIOError( fResult, 0 );

            if( fResult )
            {
               switch( uiType )
               {
                  case ZH_DISK_AVAIL:
                  case ZH_DISK_FREE:
                     dSpace = ( double ) dwNumberOfFreeClusters *
                              ( double ) dwSectorsPerCluster *
                              ( double ) dwBytesPerSector;
                     break;

                  case ZH_DISK_USED:
                  case ZH_DISK_TOTAL:
                     dSpace = ( double ) dwTotalNumberOfClusters *
                              ( double ) dwSectorsPerCluster *
                              ( double ) dwBytesPerSector;

                     if( uiType == ZH_DISK_USED )
                        dSpace -= ( double ) dwNumberOfFreeClusters *
                                  ( double ) dwSectorsPerCluster *
                                  ( double ) dwBytesPerSector;
                     break;
               }
            }
         }
         else
#endif
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

            ULARGE_INTEGER i64FreeBytesToCaller, i64TotalBytes, i64FreeBytes;

#if ! defined( ZH_OS_WIN_64 )
            fResult = s_pGetDiskFreeSpaceEx( lpPath,
                                             ( PULARGE_INTEGER ) &i64FreeBytesToCaller,
                                             ( PULARGE_INTEGER ) &i64TotalBytes,
                                             ( PULARGE_INTEGER ) &i64FreeBytes );
#else
            fResult = GetDiskFreeSpaceEx( lpPath,
                                          ( PULARGE_INTEGER ) &i64FreeBytesToCaller,
                                          ( PULARGE_INTEGER ) &i64TotalBytes,
                                          ( PULARGE_INTEGER ) &i64FreeBytes );
#endif
            zh_fsSetIOError( fResult, 0 );

            if( fResult )
            {
               switch( uiType )
               {
                  case ZH_DISK_AVAIL:
                     dSpace = ZH_GET_LARGE_UINT( i64FreeBytesToCaller );
                     break;

                  case ZH_DISK_FREE:
                     dSpace = ZH_GET_LARGE_UINT( i64FreeBytes );
                     break;

                  case ZH_DISK_TOTAL:
                     dSpace = ZH_GET_LARGE_UINT( i64TotalBytes );
                     break;

                  case ZH_DISK_USED:
                     dSpace = ZH_GET_LARGE_UINT( i64TotalBytes ) -
                              ZH_GET_LARGE_UINT( i64FreeBytes );
                     break;
               }
            }
         }
         SetErrorMode( uiErrMode );
      }
      if( lpFree )
         zh_xfree( lpFree );
   }

#elif defined( ZH_OS_UNIX )
   {
#if defined( ZH_OS_DARWIN ) || defined( ZH_OS_ANDROID )
      struct statfs sf;
#else
      struct statvfs sf;
#endif
      char * pszFree;

      pszPath = zh_fsNameConv( pszPath, &pszFree );

#if defined( ZH_OS_DARWIN ) || defined( ZH_OS_ANDROID )
      if( statfs( pszPath, &sf ) == 0 )
#else
      if( statvfs( pszPath, &sf ) == 0 )
#endif
      {
         switch( uiType )
         {
            case ZH_DISK_AVAIL:
               dSpace = ( double ) sf.f_bavail * ( double ) sf.f_bsize;
               break;

            case ZH_DISK_FREE:
               dSpace = ( double ) sf.f_bfree * ( double ) sf.f_bsize;
               break;

            case ZH_DISK_USED:
               dSpace = ( double ) ( sf.f_blocks - sf.f_bfree ) *
                        ( double ) sf.f_bsize;
               break;

            case ZH_DISK_TOTAL:
               dSpace = ( double ) sf.f_blocks * ( double ) sf.f_bsize;
               break;
         }
         zh_fsSetIOError( ZH_TRUE, 0 );
      }
      else
         zh_fsSetIOError( ZH_FALSE, 0 );

      if( pszFree )
         zh_xfree( pszFree );
   }
#else
   {
      int iTODO;

      ZH_SYMBOL_UNUSED( uiType );
   }
#endif

   return dSpace;
}

ZH_FUNC( ZH_DISKSPACE )
{
   const char * pszPath = zh_parc( 1 );
   ZH_USHORT uiType = ( ZH_USHORT ) zh_parnidef( 2, ZH_DISK_AVAIL );

#ifdef ZH_OS_HAS_DRIVE_LETTER
   char szPathBuf[ 4 ];

   if( ! pszPath )
   {
      int iDrive = zh_parni( 1 );

      if( iDrive >= 1 && iDrive < 32 )
      {
         szPathBuf[ 0 ] = ( char ) iDrive + 'A' - 1;
         szPathBuf[ 1 ] = ZH_OS_DRIVE_DELIM_CHR;
         szPathBuf[ 2 ] = ZH_OS_PATH_DELIM_CHR;
         szPathBuf[ 3 ] = '\0';
         pszPath = szPathBuf;
      }
   }
#endif

   zh_retnlen( zh_fsDiskSpace( pszPath, uiType ), -1, 0 );
}
