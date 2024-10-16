/*
 * WinMain() to main() wrapper
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include <windows.h>


#  define ZH_LPSTR  LPSTR


int WINAPI WinMain( HINSTANCE hInstance,      /* handle to current instance */
                    HINSTANCE hPrevInstance,  /* handle to previous instance */
                    ZH_LPSTR  lpCmdLine,      /* pointer to command-line */
                    int iCmdShow )            /* show state of window */
{
   int iErrorCode;

#if defined( ZH_VM_STARTUP )

   ZH_SYMBOL_UNUSED( lpCmdLine );

   #if 0
   ZH_TRACE( ZH_TR_DEBUG, ("WinMain(%p, %p, %s, %d)", hInstance, hPrevInstance, lpCmdLine, iCmdShow ) );
   #endif

   zh_winmainArgInit( hInstance, hPrevInstance, iCmdShow );

   zh_vmInit( ZH_TRUE, ZH_TRUE, ZH_TRUE);
   iErrorCode = zh_vmQuit( ZH_TRUE );

#else
#  define ZH_MAX_ARGS   256

   int argc = 0;
   char * argv[ ZH_MAX_ARGS ];

   LPSTR pArgs, pArg, pDst, pSrc;
   ZH_BOOL fQuoted;
   HANDLE hHeap;


   argv[ argc++ ] = ( char * ) "";

   pArg = NULL;

   pSrc = lpCmdLine;
   hHeap = GetProcessHeap();
   pDst = pArgs = ( LPSTR ) HeapAlloc( hHeap, 0, strlen( pSrc ) + 1 );
   fQuoted = ZH_FALSE;

   while( *pSrc != 0 && argc < ZH_MAX_ARGS )
   {
      if( *pSrc == '"' )
      {
         if( pArg == NULL )
            pArg = pDst;
         fQuoted = ! fQuoted;
      }
      else if( fQuoted || ! ZH_ISSPACE( *pSrc ) )
      {
         if( pArg == NULL )
            pArg = pDst;
         *pDst++ = *pSrc;
      }
      else
      {
         if( pArg )
         {
            *pDst++ = '\0';
            argv[ argc++ ] = pArg;
            pArg = NULL;
         }
      }
      ++pSrc;
   }
   if( pArg )
   {
      *pDst = '\0';
      argv[ argc++ ] = pArg;
   }

   ZH_SYMBOL_UNUSED( hInstance );
   ZH_SYMBOL_UNUSED( hPrevInstance );
   ZH_SYMBOL_UNUSED( iCmdShow );

   iErrorCode = main( argc, argv );

   HeapFree( hHeap, 0, ( void * ) pArgs );
#endif

   return iErrorCode;
}
