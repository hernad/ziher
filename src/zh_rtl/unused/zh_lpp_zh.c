/*
 * Length Prefix Protocol
 *
 * Copyright 2010 Mindaugas Kavaliauskas <dbtopas / at / dbtopas.lt>
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

/* Idea and protocol
   =================
   Very often it is required to accept the whole data message from
   TCP connection. Because of stream nature of TCP, this requires
   additional steps from application like start/end marker, or sending
   length of structure before the structure. The latter simple approach
   was used in Length Prefix Protocol (LPP). Protocol can easily be
   described by simple Ziher expression:
    Bin2L( zh_BLen( cData ) ) + cData

   Future extensions: Protocol is limited to 4 GiB size for a single LPP
   message. This can be extended in future to use highest bit of length
   (or some highest length values 2^32-1, etc) as a special marker for
   64-bit or similar length encoding.

   Public functions and procedures
   ===============================
   zh_lppCreate( hSocket ) --> hLPP
   zh_lppDestroy( hNSTP )
    Destroys only LPP related structures. Socket remains open and
    it is possible to continue data transfers using zh_socket*()
    functions.
   zh_lppError( hLPP ) --> nError
    nError value is compatible with Ziher socket error API,
    the only new error code (until now) is ZH_LPP_ERROR_TOOLARGE
   zh_lppSetLimit( hLPP, nLimit )
    Sets size limit for receiving data. Sending 4 bytes containing
    large 32-bit value makes receiving application to allocate a
    large memory block for storage of data to be received. It is very
    easy to crash  application (or system) using such protocol and
    logic. zh_lppSetLimit() helps to protect against such attacks.
    On zh_lppCreate() limit is set to 1024 bytes. This is enough
    for server/client authentication. After successful
    authentication server can increase size limit and large LPP
    packets can be used.
   zh_lppSend( hLPP, cBuf [, nTimeout = FOREVER ] ) --> lSuccess
   zh_lppRecv( hLPP, @cBuf [, nTimeout = FOREVER ] ) --> lSuccess
   zh_lppSendLen( hLPP ) --> nBytesSent
    Useful for drawing progress bars, etc.
   zh_lppRecvLen( hLPP ) --> nBytesReceived
    Useful for drawing progress bars, etc.

   Sample code
   ===========
   // send sample
   hLPP := zh_lppCreate( hSocket )
   DO WHILE ! ( lI := zh_lppSend( hLPP, cData, nTimeout ) ) .AND. ;
          zh_lppError( hLPP ) == ZH_SOCKET_ERR_TIMEOUT )
      // draw progress bar using zh_lppSendLen( hLPP )
   ENDDO
   IF lI  // or zh_lppError( hLPP ) == 0
      // Sent OK
   ELSE
      // error
   ENDIF
   zh_hsctpDestroy( hLPP )


   // recv sample
   DO WHILE ! ( lI := zh_lppRecv( hLPP, @cData, nTimeout ) ) .AND. ;
          zh_lppError( hLPP ) == ZH_SOCKET_ERR_TIMEOUT )
      // draw progress bar using zh_lppRecvLen( hLPP )
   ENDDO
   IF lI
      // Rcvd OK, data in cData
   ELSEIF zh_lppError( hLPP ) == 0
      // remote side shutdown connection
   ELSE
      // error
   ENDIF
 */

#include "zh_api.h"
#include "zh_item_api.h"
#include "zh_api_error.h"
#include "zh_lpp.h"

typedef struct
{
   PZH_LPP  pSocket;
   PZH_ITEM pItemSocket;
} ZH_LPP_GC, * PZH_LPP_GC;


static ZH_GARBAGE_FUNC( zh_lpp_destructor )
{
   PZH_LPP_GC pGC = ( PZH_LPP_GC ) Cargo;

   if( pGC->pSocket )
   {
      zh_lppDestroy( pGC->pSocket );
      pGC->pSocket = NULL;
   }
   if( pGC->pItemSocket )
   {
      zh_itemRelease( pGC->pItemSocket );
      pGC->pItemSocket = NULL;
   }
}


static ZH_GARBAGE_FUNC( zh_lpp_mark )
{
   PZH_LPP_GC pGC = ( PZH_LPP_GC ) Cargo;

   if( pGC->pItemSocket )
      zh_gcMark( pGC->pItemSocket );
}


static const ZH_GC_FUNCS s_gcPSocketFuncs =
{
   zh_lpp_destructor,
   zh_lpp_mark
};


ZH_FUNC( ZH_LPPCREATE )
{
   ZH_SOCKET sd;
   PZH_LPP_GC pGC;
   PZH_ITEM pItem;

   pItem = zh_param( 1, ZH_IT_POINTER );
   if( ! pItem || ( sd = zh_socketItemGet( pItem ) ) == ZH_NO_SOCKET )
   {
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
      return;
   }

   pGC = ( PZH_LPP_GC ) zh_gcAllocate( sizeof( ZH_LPP_GC ), &s_gcPSocketFuncs );
   pGC->pSocket = zh_lppCreate( sd );
   pGC->pItemSocket = zh_itemNew( pItem );
   zh_gcUnlock( pGC->pItemSocket );
   zh_retptrGC( pGC );
}


ZH_FUNC( ZH_LPPDESTROY )
{
   PZH_LPP_GC pGC;

   pGC = ( PZH_LPP_GC ) zh_parptrGC( &s_gcPSocketFuncs, 1 );
   if( ! pGC || ! pGC->pSocket )
   {
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
      return;
   }
   zh_lppDestroy( pGC->pSocket );
   pGC->pSocket = NULL;
   zh_itemRelease( pGC->pItemSocket );
   pGC->pItemSocket = NULL;
}


ZH_FUNC( ZH_LPPERROR )
{
   PZH_LPP_GC pGC;

   pGC = ( PZH_LPP_GC ) zh_parptrGC( &s_gcPSocketFuncs, 1 );
   if( ! pGC || ! pGC->pSocket )
   {
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
      return;
   }
   zh_retni( zh_lppError( pGC->pSocket ) );
}


ZH_FUNC( ZH_LPPSETLIMIT )
{
   PZH_LPP_GC pGC;

   pGC = ( PZH_LPP_GC ) zh_parptrGC( &s_gcPSocketFuncs, 1 );
   if( ! pGC || ! pGC->pSocket )
   {
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
      return;
   }
   zh_lppSetLimit( pGC->pSocket, zh_parns( 2 ) );
}


ZH_FUNC( ZH_LPPSEND )
{
   PZH_LPP_GC pGC;
   PZH_ITEM pData;

   pGC = ( PZH_LPP_GC ) zh_parptrGC( &s_gcPSocketFuncs, 1 );
   if( ! pGC || ! pGC->pSocket || zh_socketItemGet( pGC->pItemSocket ) == ZH_NO_SOCKET )
   {
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
      return;
   }

   pData = zh_param( 2, ZH_IT_STRING );
   zh_retl( zh_lppSend( pGC->pSocket, pData ? zh_itemGetCPtr( pData ) : "",
                        zh_itemGetCLen( pData ), zh_parnintdef( 3, -1 ) ) );
}


ZH_FUNC( ZH_LPPRECV )
{
   PZH_LPP_GC pGC;
   ZH_BOOL    bRet;
   void *     data;
   ZH_SIZE    len;

   pGC = ( PZH_LPP_GC ) zh_parptrGC( &s_gcPSocketFuncs, 1 );
   if( ! pGC || ! pGC->pSocket || zh_socketItemGet( pGC->pItemSocket ) == ZH_NO_SOCKET )
   {
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
      return;
   }

   bRet = zh_lppRecv( pGC->pSocket, &data, &len, zh_parnintdef( 3, -1 ) );
   if( bRet )
   {
      if( ZH_ISBYREF( 2 ) )
         zh_storclen( ( char * ) data, len, 2 );
      zh_xfree( data );
   }
   zh_retl( bRet );
}


ZH_FUNC( ZH_LPPSENDLEN )
{
   PZH_LPP_GC pGC;

   pGC = ( PZH_LPP_GC ) zh_parptrGC( &s_gcPSocketFuncs, 1 );
   if( ! pGC || ! pGC->pSocket )
   {
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
      return;
   }
   zh_retns( zh_lppSendLen( pGC->pSocket ) );
}



ZH_FUNC( ZH_LPPRECVLEN )
{
   PZH_LPP_GC pGC;

   pGC = ( PZH_LPP_GC ) zh_parptrGC( &s_gcPSocketFuncs, 1 );
   if( ! pGC || ! pGC->pSocket )
   {
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
      return;
   }
   zh_retns( zh_lppRecvLen( pGC->pSocket ) );
}
