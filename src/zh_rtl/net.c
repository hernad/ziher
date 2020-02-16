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
   #include "hbwinuni.h"
   #if defined( ZH_OS_WIN_CE )
      #include "hbwince.h"
   #endif

#elif defined( ZH_OS_DOS )

   #include "zh_io.h"
   #if defined( __DJGPP__ ) || defined( __GNUC__ )
      #include <sys/param.h>
   #endif

#elif defined( ZH_OS_OS2 ) && defined( __GNUC__ )

   #include "zh_io.h"

   /* 2004-03-25 - <maurilio.longo@libero.it>
      not needed anymore as of GCC 3.2.2 */

   #if defined( __EMX__ ) && __GNUC__ * 1000 + __GNUC_MINOR__ < 3002
      #include <emx/syscalls.h>
      #define gethostname __gethostname
   #endif

#elif defined( ZH_OS_UNIX ) && ! defined( __WATCOMC__ )

   #if defined( ZH_OS_VXWORKS )
      #include <hostLib.h>
   #endif
   #include <unistd.h>

#endif

#if ! defined( MAXGETHOSTNAME ) && ( defined( ZH_OS_UNIX ) || \
      ( ( defined( ZH_OS_OS2 ) || defined( ZH_OS_DOS ) ) && \
        defined( __GNUC__ ) ) )
   #define MAXGETHOSTNAME 256      /* should be enough for a host name */
#endif

/* NOTE: Clipper will only return a maximum of 15 bytes from this function.
         And it will be padded with spaces. Ziher does the same on the
         MS-DOS platform.
         [vszakats] */

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

#elif defined( ZH_OS_DOS )

#  if defined( __DJGPP__ ) || defined( __GNUC__ )
      char szValue[ MAXGETHOSTNAME + 1 ];
      szValue[ 0 ] = szValue[ MAXGETHOSTNAME ] = '\0';
      gethostname( szValue, MAXGETHOSTNAME );
      if( szValue[ 0 ] )
         return zh_osStrDecode( szValue );
#  else
      union REGS regs;
      struct SREGS sregs;
      char szValue[ 16 ];
      szValue[ 0 ] = szValue[ 15 ] = '\0';

      regs.ZH_XREGS.ax = 0x5E00;
      regs.ZH_XREGS.dx = FP_OFF( szValue );
      sregs.ds = FP_SEG( szValue );

      ZH_DOS_INT86X( 0x21, &regs, &regs, &sregs );

      if( regs.h.ch != 0 && szValue[ 0 ] )
         return zh_osStrDecode( szValue );
#  endif

#elif ( defined( ZH_OS_UNIX ) && ! defined( __WATCOMC__ ) ) || \
      ( defined( ZH_OS_OS2 ) && defined( __GNUC__ ) )

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
