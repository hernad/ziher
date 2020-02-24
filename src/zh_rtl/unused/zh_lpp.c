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

#include "zh_api.h"
#include "zh_item_api.h"
#include "zh_error_api.h"
#include "zh_date.h"
#include "zh_lpp.h"

PZH_LPP zh_lppCreate( ZH_SOCKET sd )
{
   PZH_LPP pSocket;

   pSocket         = ( PZH_LPP ) zh_xgrabz( sizeof( ZH_LPP ) );
   pSocket->sd     = sd;
   pSocket->nLimit = 1024;
   return pSocket;
}

void zh_lppDestroy( PZH_LPP pSocket )
{
   if( pSocket->pSendBuffer )
      zh_xfree( pSocket->pSendBuffer );

   if( pSocket->pRecvBuffer )
      zh_xfree( pSocket->pRecvBuffer );

   zh_xfree( pSocket );
}

void zh_lppSetLimit( PZH_LPP pSocket, ZH_SIZE nLimit )
{
   pSocket->nLimit = nLimit;
}

int zh_lppError( PZH_LPP pSocket )
{
   return pSocket->iError;
}

ZH_BOOL zh_lppSend( PZH_LPP pSocket, const void * data, ZH_SIZE len, ZH_MAXINT timeout )
{
   ZH_MAXUINT timer;
   long       lSend;

   if( ! pSocket->pSendBuffer )
   {
      pSocket->pSendBuffer = ( char * ) zh_xgrab( len + 4 );
      ZH_PUT_LE_UINT32( pSocket->pSendBuffer, len );
      zh_xmemcpy( pSocket->pSendBuffer + 4, data, len );
      pSocket->nSendLen = len + 4;
      pSocket->nSendPos = 0;
   }

   timer = zh_timerInit( timeout );

   for( ;; )
   {
      if( pSocket->nSendLen - pSocket->nSendPos < ( ZH_SIZE ) LONG_MAX )
         lSend = ( long ) ( pSocket->nSendLen - pSocket->nSendPos );
      else
         lSend = LONG_MAX;

      lSend = zh_socketSend( pSocket->sd, pSocket->pSendBuffer + pSocket->nSendPos, lSend, 0, timeout );
      if( lSend == -1 )
      {
         pSocket->iError = zh_socketGetError();
         return ZH_FALSE;
      }
      pSocket->nSendPos += lSend;
      if( pSocket->nSendPos == pSocket->nSendLen )
      {
         zh_xfree( pSocket->pSendBuffer );
         pSocket->pSendBuffer = NULL;
         pSocket->iError      = 0;
         return ZH_TRUE;
      }
      if( ( timeout = zh_timerTest( timeout, &timer ) ) == 0 )
      {
         pSocket->iError = ZH_SOCKET_ERR_TIMEOUT;
         return ZH_FALSE;
      }
   }
}

ZH_BOOL zh_lppRecv( PZH_LPP pSocket, void ** data, ZH_SIZE * len, ZH_MAXINT timeout )
{
   ZH_MAXUINT timer;
   long       lRecv;

   if( ! pSocket->pRecvBuffer )
   {
      pSocket->pRecvBuffer  = ( char * ) zh_xgrab( 4 );
      pSocket->nRecvLen     = 0;
      pSocket->fRecvHasSize = ZH_FALSE;
   }

   timer = zh_timerInit( timeout );

   for( ;; )
   {
      if( ! pSocket->fRecvHasSize )
      {
         lRecv = ( long ) ( 4 - pSocket->nRecvLen );
         lRecv = zh_socketRecv( pSocket->sd, pSocket->pRecvBuffer + pSocket->nRecvLen, lRecv, 0, timeout );
         if( lRecv == -1 )
         {
            pSocket->iError = zh_socketGetError();
            return ZH_FALSE;
         }
         else if( lRecv == 0 )
         {
            /* peer closed connection */
            pSocket->iError = 0;
            return ZH_FALSE;
         }

         pSocket->nRecvLen += lRecv;
         if( pSocket->nRecvLen < 4 )
         {
            pSocket->iError = ZH_SOCKET_ERR_TIMEOUT;
            return ZH_FALSE;
         }

         pSocket->nRecvSize = ZH_GET_UINT32( pSocket->pRecvBuffer );

         if( pSocket->nLimit && pSocket->nRecvSize > pSocket->nLimit )
         {
            /* protection against remote memory exhaust attack */
            pSocket->iError = ZH_LPP_ERR_TOOLARGE;
            zh_xfree( pSocket->pRecvBuffer );
            pSocket->pRecvBuffer = NULL;
            return ZH_FALSE;
         }

         pSocket->nRecvLen     = 0;
         pSocket->fRecvHasSize = ZH_TRUE;
         if( pSocket->nRecvSize != 4 )
            pSocket->pRecvBuffer = ( char * ) zh_xrealloc( pSocket->pRecvBuffer, pSocket->nRecvSize );
      }

      if( pSocket->nRecvSize - pSocket->nRecvLen < ( ZH_SIZE ) LONG_MAX )
         lRecv = ( long ) ( pSocket->nRecvSize - pSocket->nRecvLen );
      else
         lRecv = LONG_MAX;

      lRecv = zh_socketRecv( pSocket->sd, pSocket->pRecvBuffer + pSocket->nRecvLen, lRecv, 0, timeout );
      if( lRecv == -1 )
      {
         pSocket->iError = zh_socketGetError();
         return ZH_FALSE;
      }
      else if( lRecv == 0 )
      {
         /* peer closed connection */
         pSocket->iError = 0;
         return ZH_FALSE;
      }

      pSocket->nRecvLen += lRecv;
      if( pSocket->nRecvSize == pSocket->nRecvLen )
      {
         *data = pSocket->pRecvBuffer;
         *len  = pSocket->nRecvLen;
         pSocket->pRecvBuffer = NULL;
         pSocket->iError      = 0;
         return ZH_TRUE;
      }
      if( ( timeout = zh_timerTest( timeout, &timer ) ) == 0 )
      {
         pSocket->iError = ZH_SOCKET_ERR_TIMEOUT;
         return ZH_FALSE;
      }
   }
}

ZH_SIZE zh_lppSendLen( PZH_LPP pSocket )
{
   return pSocket->nSendLen - pSocket->nSendPos;
}

ZH_SIZE zh_lppRecvLen( PZH_LPP pSocket )
{
   return pSocket->nRecvLen;
}
