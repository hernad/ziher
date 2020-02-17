/*
 * __Run() function
 *
 * Copyright 1999 Eddie Runia <eddie@runia.com>
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
#include "zh_gt_api.h"
#include "zh_item_api.h"
#include "zh_apifs.h"

#if defined( ZH_OS_WIN )
   #include "zh_win_uni.h"
   #include <windows.h>
#endif

/* FIXME: The screen buffer handling is not right for all platforms (Windows)
          The output of the launched (MS-DOS?) app is not visible. */

ZH_FUNC( __RUN )
{
   const char * pszCommand = zh_parc( 1 );

   if( pszCommand && zh_gtSuspend() == ZH_SUCCESS )
   {
#if defined( ZH_OS_WIN_CE )
      zh_fsProcessRun( pszCommand, NULL, 0, NULL, NULL, NULL, NULL, ZH_FALSE );
#elif defined( ZH_OS_WIN )
      LPTSTR lpCommand = ZH_CHARDUP( pszCommand );
      ( void ) ZH_WINAPI_SYSTEM( lpCommand );
      zh_xfree( lpCommand );
#else
      char * pszFree = NULL;

      if( system( zh_osEncodeCP( pszCommand, &pszFree, NULL ) ) != 0 ) {}

      if( pszFree )
         zh_xfree( pszFree );
#endif

      if( zh_gtResume() != ZH_SUCCESS )
      {
         /* an error should be generated here !! Something like */
         #if 0
         zh_errRT_BASE_Ext1( EG_GTRESUME, 6002, NULL, ZH_ERR_FUNCNAME, 0, EF_CANDEFAULT );
         #endif
      }
   }
}
