/*
 * CT (Clipper Tools) Disk, File and Directory management.
 *
 * Copyright 2004-2005 Eduardo Fernandes <modalsist@yahoo.com.br>
 *    DirMake(), DirName(), DriveType(), Volume(), VolSerial()
 * Copyright 2004 Phil Krylov <phil@newstar.rinet.ru> (NumDiskL())
 * Copyright 2006 Pavel Tsarenko <tpe2@mail.ru> (TrueName())
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
#include "zh_error_api.h"
#include "zh_fs_api.h"
#include "zh_vm.h"
#include "ctstrfil.h"


#if defined( ZH_OS_WIN )
#include "zh_win_unicode.h"
#  include <windows.h>
#endif

ZH_FUNC( DIRMAKE )
{
   if( zh_fileDirMake( zh_parcx( 1 ) ) )
      zh_retni( 0 );
   else
      zh_retnint( -( ZH_MAXINT ) zh_fsOsError() );
}

ZH_FUNC( DIRNAME )
{
   char * pbyBuffer = ( char * ) zh_xgrab( ZH_PATH_MAX );
   const char * pszDrive = zh_parc( 1 );
   int iDrive = 0;

   if( pszDrive )
   {
      ZH_UCHAR uc = ( ZH_UCHAR ) *pszDrive;
      /* some network drivers (e.g. NETX from Novell NetWare) allow
       * to create drives after 'Z' letter.
       */
      if( uc >= 'A' && uc < 'A' + 32 )
         iDrive = uc - ( 'A' - 1 );
      else if( uc >= 'a' && uc < 'a' + 32 )
         iDrive = uc - ( 'a' - 1 );
   }
   pbyBuffer[ 0 ] = ZH_OS_PATH_DELIM_CHR;
   zh_fsCurDirBuff( iDrive, pbyBuffer + 1, ZH_PATH_MAX - 1 );

   zh_retc_buffer( pbyBuffer );
}

ZH_FUNC( DRIVETYPE )
{
#if defined( ZH_OS_WIN ) && ! defined( ZH_OS_WIN_CE )
   ZH_SIZE nSize = zh_parclen( 1 ) + 2;  /* allow space for '\0' & ":\" */
   char * pszDrive = ( char * ) zh_xgrab( nSize + 1 );
   LPCTSTR lpDrive;
   LPTSTR lpFree;
   UINT uiType;

   zh_strncpy( pszDrive, zh_parcx( 1 ), nSize );

   if( strstr( pszDrive, ":" ) == NULL )
      zh_strncat( pszDrive, ":", nSize );

   if( strstr( pszDrive, "\\" ) == NULL )
      zh_strncat( pszDrive, "\\", nSize );

   lpDrive = ZH_FSNAMECONV( pszDrive, &lpFree );
   zh_vmUnlock();
   uiType = GetDriveType( lpDrive );
   zh_vmLock();
   if( lpFree )
      zh_xfree( lpFree );
   zh_xfree( pszDrive );

   switch( uiType )
   {
      case DRIVE_RAMDISK:
         uiType = 0;           /* RAM Drive - Clipper compatible */
         break;
      case DRIVE_REMOVABLE:
         uiType = 2;           /* Floppy Drive - Clipper compatible */
         break;
      case DRIVE_FIXED:
         uiType = 3;           /* Hard Drive  - Clipper compatible */
         break;
      case DRIVE_CDROM:
         uiType = 4;           /* CD-Rom Drive - xZiher extension */ /* ZH_EXTENSION */
         break;
      case DRIVE_REMOTE:
         uiType = 5;           /* Network Drive - xZiher extension */ /* ZH_EXTENSION */
         break;
      default:
         uiType = 9;           /* Unknown Drive - xZiher extension */ /* ZH_EXTENSION */
         break;
   }
   zh_retni( uiType );
#else
   zh_retni( 9 );
#endif

}

ZH_FUNC( NUMDISKL )
{
#if defined( ZH_OS_DOS )
#if defined( __DJGPP__ ) || defined( __WATCOMC__ )
   unsigned cur_drive, n_drives;

   _dos_getdrive( &cur_drive );
   _dos_setdrive( cur_drive, &n_drives );
   zh_retni( n_drives );
#else
   /* should be easily implementable somehow similar to DJGPP */
   zh_retni( 26 );
#endif
#elif defined( ZH_OS_WIN )
   /* LASTDRIVE does not affect Windows apps, they always have 26 letters avail */
   zh_retni( 26 );
#else
   /* For Unix, return the most harmless value... or not? */
   zh_retni( 1 );
#endif
}

/*
 * Volume() depends of the CSetSafety() setting and, if is true, does not
 * overwrite an existing label.
 *
 * Syntax is: Volume("X:test") or Volume("X:\test"), where "x" is the
 * any drive letter and "test" will be the new volume name.
 *
 * Notes:
 * 1) if the drive letter is not supplied, then the current drive will
 *    be used to change volume name.
 * 2) if Volume("X:") or Volume("X:\") then the volume name of the drive
 *    "X:" will be erased.
 * 3) if Volume("") or Volume() then the volume name of the current drive
 *   will be erased.
 */

ZH_FUNC( VOLUME )
{
   ZH_BOOL bReturn = ZH_FALSE;

#if defined( ZH_OS_WIN ) && ! defined( ZH_OS_WIN_CE )
   if( ! ct_getsafety() )
   {
      const char * pszRoot = NULL;
      const char * pszVolName = NULL;
      char szRootBuf[ 4 ], szVolNameBuf[ 12 ];
      LPCTSTR lpRoot, lpVolName;
      LPTSTR lpRootFree = NULL, lpVolNameFree = NULL;

      if( zh_parclen( 1 ) > 0 )
      {
         PZH_FNAME fname = zh_fsFNameSplit( zh_parc( 1 ) );

         if( fname->szPath )
            pszRoot = zh_strncpy( szRootBuf, fname->szPath, sizeof( szRootBuf ) - 1 );
         if( fname->szName )
            pszVolName = zh_strncpy( szVolNameBuf, fname->szName, sizeof( szVolNameBuf ) - 1 );
         zh_xfree( fname );
      }

      lpRoot = pszRoot ? ZH_FSNAMECONV( pszRoot, &lpRootFree ) : NULL;
      lpVolName = pszVolName ? ZH_FSNAMECONV( pszVolName, &lpVolNameFree ) : NULL;
      zh_vmUnlock();
      bReturn = SetVolumeLabel( lpRoot, lpVolName ) != 0;
      zh_vmLock();
      if( lpRootFree )
         zh_xfree( lpRootFree );
      if( lpVolNameFree )
         zh_xfree( lpVolNameFree );
   }
#endif
   zh_retl( bReturn );
}

/*
 * VolSerial() function returns the volume serial number of an drive letter like
 * floppy, Hard-disk, CD or mapped network drive. The return value is a numeric
 * type. If the drive is not available, VolSerial() returns -1.
 *
 * Syntax is: VolSerial( "X:\" )
 * Note that the trailing backslash is required.
 *
 * To convert in the hex format, call zh_NumToHex() function.
 * Example: zh_NumToHex( VolSerial( "C:\" ) ).
 */

ZH_FUNC( VOLSERIAL )
{
#if defined( ZH_OS_WIN ) && ! defined( ZH_OS_WIN_CE )
   DWORD dwSerial = 0;
   void * hDrive;
   ZH_SIZE nLen;
   LPCTSTR lpRootPath = ZH_PARSTR( 1, &hDrive, &nLen );

   if( GetVolumeInformation( nLen > 0 ? lpRootPath : NULL, /* RootPathName */
                             NULL,      /* VolumeName */
                             0,         /* VolumeNameSize */
                             &dwSerial, /* VolumeSerialNumber */
                             NULL,      /* MaxComponentLength */
                             NULL,      /* FileSystemFlags */
                             NULL,      /* FileSystemName */
                             0 ) )      /* FileSystemSize */
      zh_retnint( dwSerial );
   else
      zh_retni( -1 );

   zh_strfree( hDrive );
#else
   zh_retni( -1 );
#endif
}

ZH_FUNC( TRUENAME )
{
   if( ZH_ISCHAR( 1 ) )
   {
#if defined( ZH_OS_WIN ) && ! defined( ZH_OS_WIN_CE )
      void * hFile;
      TCHAR buffer[ MAX_PATH + 1 ];

      buffer[ 0 ] = buffer[ MAX_PATH ] = TEXT( '\0' );

      GetFullPathName( ZH_PARSTR( 1, &hFile, NULL ),
                       ZH_SIZEOFARRAY( buffer ) - 1,
                       buffer, NULL );

      ZH_RETSTR( buffer );
      zh_strfree( hFile );
#else
      zh_retc( zh_parc( 1 ) );
#endif
   }
   else
      zh_retc_null();
}
