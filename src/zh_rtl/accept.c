/*
 * ACCEPT command related functions
 *
 * Copyright 1999-2001 Viktor Szakats (__AcceptStr())
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
#include "zh_vm.h"
#include "zh_gt_api.h"
#include "zh_stack.h"
#include "inkey.zhh"

ZH_FUNC_EXTERN( QOUT );

#define ACCEPT_BUFFER_LEN  256  /* length of input buffer for ACCEPT command */


static ZH_TSD_NEW( s_szAcceptResult, ACCEPT_BUFFER_LEN, NULL, NULL );

static char * zh_acceptBuffer( void )
{
   return ( char * ) zh_stackGetTSD( &s_szAcceptResult );
}

ZH_FUNC( __ACCEPTSTR )
{
   zh_retc( zh_acceptBuffer() );
}

ZH_FUNC( __ACCEPT )
{
   PZH_CODEPAGE cdp = zh_vmCodepage();
   char         szAcceptResult[ ACCEPT_BUFFER_LEN ];
   char         szKey[ ZH_MAX_CHAR_LEN ];
   ZH_SIZE      nLen  = 0, nChar;
   int          input = 0;

   /* cPrompt(s) passed ? */
   if( zh_pcount() >= 1 )
      ZH_FUNC_EXEC( QOUT );

   szAcceptResult[ 0 ] = '\0';

   while( input != K_ENTER && zh_vmRequestQuery() == 0 )
   {
      /* Wait forever, for keyboard events only */
      input = zh_inkey( ZH_TRUE, 0.0, INKEY_KEYBOARD );
      switch( input )
      {
         case K_BS:
         case K_LEFT:
            if( nLen > 0 )
            {
               nChar = zh_cdpTextLen( cdp, szAcceptResult, nLen );
               if( nChar > 0 )
                  nLen = zh_cdpTextPos( cdp, szAcceptResult, nLen, nChar - 1 );
               else
                  nLen = 0;

               szKey[ 0 ] = ZH_CHAR_BS;

               nChar = 1;
            }
            else
               nChar = 0;
            break;

         default:
            nChar = zh_inkeyKeyString( input, szKey, sizeof( szKey ) );
            if( nChar > 0 && nLen + nChar < ACCEPT_BUFFER_LEN )
            {
               memcpy( &szAcceptResult[ nLen ], szKey, nChar );
               nLen += nChar;
            }
      }
      if( nChar > 0 )
         zh_conOutAlt( szKey, nChar );
   }

   szAcceptResult[ nLen ] = '\0';

   zh_strncpy( zh_acceptBuffer(), szAcceptResult, ACCEPT_BUFFER_LEN - 1 );

   zh_retclen( szAcceptResult, nLen );
}
