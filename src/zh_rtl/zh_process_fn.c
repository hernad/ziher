/*
 * .zh level functions to create, wait and terminate processes
 *
 * Copyright 2009 Przemyslaw Czerpak
 * based on xZiher code by
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
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
#include "zh_apifs.h"
#include "zh_api_error.h"

ZH_FUNC( ZH_PROCESSOPEN )
{
   const char * szName = zh_parc( 1 );
   PZH_ITEM pStdIn  = zh_param( 2, ZH_IT_BYREF );
   PZH_ITEM pStdOut = zh_param( 3, ZH_IT_BYREF );
   PZH_ITEM pStdErr = zh_param( 4, ZH_IT_BYREF );

   if( szName &&
       ( pStdIn  || ZH_ISNIL( 2 ) ) &&
       ( pStdOut || ZH_ISNIL( 3 ) ) &&
       ( pStdErr || ZH_ISNIL( 4 ) ) &&
       ( ZH_ISLOG( 5 ) || ZH_ISNIL( 5 ) ) &&
       ( ZH_ISBYREF( 6 ) || ZH_ISNIL( 6 ) ) &&
       ( ! pStdIn || ( pStdIn != pStdOut && pStdIn != pStdErr ) ) )
   {
      ZH_BOOL fDetach = zh_parl( 5 );
      ZH_FHANDLE hStdIn, *phStdIn, hStdOut, *phStdOut, hStdErr, *phStdErr;
      ZH_FHANDLE hProcess;
      ZH_ULONG ulPID;

      phStdIn  = pStdIn  ? &hStdIn  : NULL;
      phStdOut = pStdOut ? &hStdOut : NULL;
      phStdErr = pStdErr ? ( pStdOut == pStdErr ? phStdOut : &hStdErr ) : NULL;

      hProcess = zh_fsProcessOpen( szName, phStdIn, phStdOut, phStdErr,
                                   fDetach, &ulPID );
      zh_fsSetFError( zh_fsError() );
      if( hProcess != FS_ERROR )
      {
         if( phStdIn )
            zh_stornint( ( ZH_NHANDLE ) *phStdIn, 2 );
         if( phStdOut )
            zh_stornint( ( ZH_NHANDLE ) *phStdOut, 3 );
         if( phStdErr && phStdOut != phStdErr )
            zh_stornint( ( ZH_NHANDLE ) *phStdErr, 4 );
         zh_stornint( ulPID, 6 );
      }
      zh_retnint( ( ZH_NHANDLE ) hProcess );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 4001, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_PROCESSVALUE )
{
   ZH_FHANDLE hProcess = zh_numToHandle( zh_parnint( 1 ) );

   if( hProcess != 0 && hProcess != FS_ERROR && ( zh_pcount() < 2 || ZH_ISLOG( 2 ) ) )
   {
      int iResult = zh_fsProcessValue( hProcess, zh_pcount() < 2 || zh_parl( 2 ) );
      zh_fsSetFError( zh_fsError() );
      zh_retni( iResult );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 4001, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_PROCESSCLOSE )
{
   ZH_FHANDLE hProcess = zh_numToHandle( zh_parnint( 1 ) );

   if( hProcess != 0 && hProcess != FS_ERROR && ( zh_pcount() < 2 || ZH_ISLOG( 2 ) ) )
   {
      ZH_BOOL fResult = zh_fsProcessClose( hProcess, zh_pcount() < 2 || zh_parl( 2 ) );
      zh_fsSetFError( zh_fsError() );
      zh_retl( fResult );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 4001, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/* zh_processRun( <cCommand>, [ <cStdIn> ], [ @<cStdOut> ], [ @<cStdErr> ], ;
                  [ <lDetach> ] ) --> <nResult> */
ZH_FUNC( ZH_PROCESSRUN )
{
   const char * szName = zh_parc( 1 );
   const char * szStdIn = zh_parc( 2 );
   PZH_ITEM pStdOut = zh_param( 3, ZH_IT_BYREF );
   PZH_ITEM pStdErr = zh_param( 4, ZH_IT_BYREF );
   ZH_BOOL fDetach = zh_parl( 5 );

   if( szName &&
       ( szStdIn || ZH_ISNIL( 2 ) ) &&
       ( pStdOut || ZH_ISNIL( 3 ) ) &&
       ( pStdErr || ZH_ISNIL( 4 ) ) &&
       ( ZH_ISLOG( 5 ) || ZH_ISNIL( 5 ) ) )
   {
      ZH_SIZE nStdOut, nStdErr;
      char * pStdOutBuf, * pStdErrBuf;
      char ** pStdOutPtr, ** pStdErrPtr;
      int iResult;

      nStdOut = nStdErr = 0;
      pStdOutBuf = pStdErrBuf = NULL;
      pStdOutPtr = pStdOut ? &pStdOutBuf : NULL;
      pStdErrPtr = pStdErr ? ( pStdOut == pStdErr ? pStdOutPtr : &pStdErrBuf ) : NULL;

      iResult = zh_fsProcessRun( szName, szStdIn, zh_parclen( 2 ),
                                 pStdOutPtr, &nStdOut, pStdErrPtr, &nStdErr,
                                 fDetach );
      zh_fsSetFError( zh_fsError() );

      if( pStdOutBuf )
      {
         if( ! zh_storclen_buffer( pStdOutBuf, nStdOut, 3 ) )
            zh_xfree( pStdOutBuf );
      }
      else if( pStdOut )
         zh_storc( NULL, 3 );

      if( pStdErrBuf )
      {
         if( ! zh_storclen_buffer( pStdErrBuf, nStdErr, 4 ) )
            zh_xfree( pStdErrBuf );
      }
      else if( pStdErr && pStdOut != pStdErr )
         zh_storc( NULL, 4 );

      zh_retni( iResult );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 4001, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}
