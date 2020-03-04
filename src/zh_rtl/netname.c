/*
 * NetName() function
 *
 * Copyright 1999-2001 Viktor Szakats
 * Copyright 2001 Luiz Rafael Culik <culik@sl.conex.net> (Support for DJGPP/GCC/OS2 for NetName())
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

#if defined( ZH_OS_WIN )

   #include <windows.h>
   #include "zh_win_unicode.h"
   #if defined( ZH_OS_WIN_CE )
      #include "hbwince.h"
   #endif


#elif defined( __GNUC__ )

   #include "zh_io.h"

   /* 2004-03-25 - <maurilio.longo@libero.it>
      not needed anymore as of GCC 3.2.2 */

   #if defined( __EMX__ ) && __GNUC__ * 1000 + __GNUC_MINOR__ < 3002
      #include <emx/syscalls.h>
      #define gethostname __gethostname
   #endif

#elif defined( ZH_OS_UNIX )

   #include <unistd.h>

#endif

#if ! defined( MAXGETHOSTNAME ) &&  defined( ZH_OS_UNIX )
   #define MAXGETHOSTNAME 256      /* should be enough for a host name */
#endif

/* NOTE: The caller must free the returned buffer. [vszakats] */

char * zh_netname( void )
{
#if defined( ZH_OS_WIN )

   DWORD dwLen = MAX_COMPUTERNAME_LENGTH + 1;
   TCHAR lpValue[ MAX_COMPUTERNAME_LENGTH + 1 ];

   lpValue[ 0 ] = TEXT( '\0' );
   GetComputerName( lpValue, &dwLen );
   lpValue[ MAX_COMPUTERNAME_LENGTH ] = TEXT( '\0' );

   if( lpValue[ 0 ] )
      return ZH_OSSTRDUP( lpValue );


#elif defined( ZH_OS_UNIX )

   char szValue[ MAXGETHOSTNAME + 1 ];
   szValue[ 0 ] = szValue[ MAXGETHOSTNAME ] = '\0';
   gethostname( szValue, MAXGETHOSTNAME );
   if( szValue[ 0 ] )
      return zh_osStrDecode( szValue );

#endif

   return zh_getenv( "HOSTNAME" );
}

ZH_FUNC( NETNAME )
{
   char * buffer = zh_netname();

   if( buffer )
      zh_retc_buffer( buffer );
   else
      zh_retc_null();
}
