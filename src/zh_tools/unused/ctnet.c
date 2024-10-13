/*
 * CT3 NET functions to PC-LAN/MS-NET.
 *
 * Copyright 2004 Eduardo Fernandes <eduardo@modalsistemas.com.br>
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

/*
 * CT3 NET Functions Comments:
 *
 * NetCancel( <cLocalDevice> ) --> lReleased
 * Return true if <cLocalDevice> was disconnected.
 *
 * NetDisk( cDrive ) --> lSuccess
 * Return true if <cDrive> is a network drive, otherwise return false if is a local drive.
 *
 * NetLocName( cSahredDevice ) --> cLocalDevice
 * Not implemented yet.
 *
 * NetPrinter() --> lSuccess
 * Return true if a current local printer set by SET PRINTER TO was connected to a
 * network printer.
 *
 * NetRedir( cLocalDevice, cSharedDevice, [ cPassword ], [ lShowError] ) --> lSuccess
 * Return true if <cLocalDevice> was connected to <cSharedDevice> with <cPassword>, if any.
 *
 * NetRmtname( cLocalDevice ) --> cSharedName
 * Return the shared resource name connected to a <cLocalDevice>.
 * 
 * Network() --> lSuccess
 * Return true if a PC-LAN/MS-NET or NetWare type is active.
 *
 * NNetwork() --> lSuccess
 * Return true if a NetWare type is active.
 *
 */

#include "zh_api.h"
#include "zh_item_api.h"
#include "zh_vm.h"
#include "zh_set.h"
#include "zh_error_api.h"
#include "zh_win_unicode.h"

#if defined( ZH_OS_WIN )
   #include <windows.h>
   #include <winnetwk.h>
#endif

#if defined( ZH_OS_WIN )
static ZH_BOOL zh_IsNetShared( const char * szLocalDevice )
{
   TCHAR lpRemoteDevice[ ZH_PATH_MAX ];
   LPCTSTR lpLocalDevice;
   LPTSTR lpFree;
   DWORD dwLen = ZH_SIZEOFARRAY( lpRemoteDevice );
   DWORD dwResult;

   lpLocalDevice = ZH_FSNAMECONV( szLocalDevice, &lpFree );
   zh_vmUnlock();
   dwResult = WNetGetConnection( lpLocalDevice, lpRemoteDevice, &dwLen );
   zh_vmLock();
   if( lpFree )
      zh_xfree( lpFree );

   return dwResult == NO_ERROR;
}
#endif

ZH_FUNC( NETCANCEL )
{
#if defined( ZH_OS_WIN )
   void * hDevice;

   DWORD dwResult = WNetCancelConnection( ZH_PARSTRDEF( 1, &hDevice, NULL ), TRUE );  /* FALSE = fail if exist open files or print jobs. */

   zh_strfree( hDevice );
   /* TRUE = force cancel connection even if exist
    *        open files or print jobs.
    */
   zh_retl( dwResult == NO_ERROR );
#else
   zh_retl( ZH_FALSE );
#endif
}

ZH_FUNC( NETPRINTER )
{
#if defined( ZH_OS_WIN )
   const char * cPrn = zh_setGetCPtr( ZH_SET_PRINTFILE );  /* query default local printer port. */

   if( ! cPrn || ! *cPrn || zh_stricmp( cPrn, "PRN" ) == 0 )
      cPrn = "LPT1";
   zh_retl( zh_IsNetShared( cPrn ) );
#else
   zh_retl( ZH_FALSE );
#endif
}

ZH_FUNC( NETDISK )
{
#if defined( ZH_OS_WIN )
   const char * pszDrive = zh_parc( 1 );

   if( pszDrive )
   {
      char szDrive[ 3 ];

      szDrive[ 0 ] = pszDrive[ 0 ];
      szDrive[ 1 ] = ':';
      szDrive[ 2 ] = '\0';

      zh_retl( zh_IsNetShared( szDrive ) );
   }
   else
#endif
      zh_retl( ZH_FALSE );
}

ZH_FUNC( NETREDIR )
{
#if defined( ZH_OS_WIN )
   void * hLocalDev;
   void * hSharedRes;
   void * hPassword;

   DWORD dwResult = WNetAddConnection( ZH_PARSTRDEF( 2, &hSharedRes, NULL ),
                                       ZH_PARSTR( 3, &hPassword, NULL ),
                                       ZH_PARSTRDEF( 1, &hLocalDev, NULL ) );

   zh_strfree( hLocalDev  );
   zh_strfree( hSharedRes );
   zh_strfree( hPassword  );

   zh_retl( dwResult == NO_ERROR );
#else
   zh_retl( ZH_FALSE );
#endif
}

ZH_FUNC( NETRMTNAME )
{
#if defined( ZH_OS_WIN )
   void * hLocalDev;

   TCHAR lpRemoteDevice[ 128 ];
   DWORD dwLen = ZH_SIZEOFARRAY( lpRemoteDevice );
   DWORD dwSize = 0;
   LPCTSTR lpLocalName = ZH_PARSTRDEF( 1, &hLocalDev, NULL );

   if( WNetGetConnection( lpLocalName, lpRemoteDevice, &dwSize ) == ERROR_MORE_DATA )
   {
      if( dwSize > 0 && dwSize <= dwLen && WNetGetConnection( lpLocalName, lpRemoteDevice, &dwSize ) == NO_ERROR )
         ZH_RETSTRLEN( lpRemoteDevice, ( ZH_SIZE ) ( dwSize - 1 ) );
      else
         zh_retc_null();
   }
   else
      zh_retc_null();

   zh_strfree( hLocalDev );
#else
   zh_retc_null();
#endif
}

ZH_FUNC( NETWORK )
{
#if defined( ZH_OS_WIN )
   DWORD dwResult;
   TCHAR lpProviderName[ 128 ];
   DWORD dwLen = ZH_SIZEOFARRAY( lpProviderName );

   dwResult = WNetGetProviderName( WNNC_NET_MSNET, lpProviderName, &dwLen );

   if( dwResult != NO_ERROR )
   {
      dwResult = WNetGetProviderName( WNNC_NET_LANMAN, lpProviderName, &dwLen );

      if( dwResult != NO_ERROR )
         dwResult = WNetGetProviderName( WNNC_NET_NETWARE, lpProviderName, &dwLen );
   }

   zh_retl( dwResult == NO_ERROR );
#else
   zh_retl( ZH_FALSE );
#endif
}

ZH_FUNC( NNETWORK )
{
#if defined( ZH_OS_WIN )
   TCHAR lpProviderName[ 128 ];
   DWORD dwLen = ZH_SIZEOFARRAY( lpProviderName );

   zh_retl( WNetGetProviderName( WNNC_NET_NETWARE, lpProviderName, &dwLen ) == NO_ERROR );
#else
   zh_retl( ZH_FALSE );
#endif
}
