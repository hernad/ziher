/*
 * CT3 serial communication com_*() functions
 *
 * Copyright 2010 Przemyslaw Czerpak
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
#include "zh_com_api.h"
#include "ctcom.zhh"

static int zh_ctComCharParam( int iParam )
{
   const char * pszParam = zh_parc( iParam );

   if( pszParam )
   {
      if( zh_parclen( iParam ) > 0 )
         return ( unsigned char ) pszParam[ 0 ];
   }
   else if( ZH_IS_PARAM_NUM( iParam ) )
      return ( unsigned char ) zh_parni( iParam );

   return -1;
}

static void zh_ctComTestMSR( int iLine )
{
   ZH_BOOL fResult;
   int iMSR;

   if( zh_comMSR( zh_parni( 1 ), &iMSR ) != -1 )
      fResult = ( iMSR & iLine ) != 0;
   else
      fResult = ZH_FALSE;

   zh_retl( fResult );
}

/* com_Count( <nComPort> ) --> <nCharactersInInputBuffer>
 */
ZH_FUNC( COM_COUNT )
{
   zh_retni( zh_comInputCount( zh_parni( 1 ) ) );
}

/* com_SCount( <nComPort> ) --> <nCharactersInOutputBuffer>
 */
ZH_FUNC( COM_SCOUNT )
{
   zh_retni( zh_comOutputCount( zh_parni( 1 ) ) );
}

/* com_Flush( <nComPort> ) --> <lInputBufferCleared>
 */
ZH_FUNC( COM_FLUSH )
{
   zh_retl( zh_comFlush( zh_parni( 1 ), ZH_COM_IFLUSH ) != -1 );
}

/* com_SFlush( <nComPort> ) --> <lOutputBufferCleared>
 */
ZH_FUNC( COM_SFLUSH )
{
   zh_retl( zh_comFlush( zh_parni( 1 ), ZH_COM_OFLUSH ) != -1 );
}

/* com_CTS( <nComPort> ) --> <lCTSActive>
 */
ZH_FUNC( COM_CTS )
{
   zh_ctComTestMSR( ZH_COM_MSR_CTS );
}

/* com_DCD( <nComPort> ) --> <lDCDActive>
 */
ZH_FUNC( COM_DCD )
{
   zh_ctComTestMSR( ZH_COM_MSR_DCD );
}

/* com_DSR( <nComPort> ) --> <lDSRActive>
 */
ZH_FUNC( COM_DSR )
{
   zh_ctComTestMSR( ZH_COM_MSR_DSR );
}

/* com_Ring( <nComPort> ) --> <lActiveRing>
 */
ZH_FUNC( COM_RING )
{
   zh_ctComTestMSR( ZH_COM_MSR_RI );
}

/* com_RTS( <nComPort>, [<lNewRTSStatus>] ) --> <lOldRTSStatus>
 */
ZH_FUNC( COM_RTS )
{
   int iMCR, iClr = 0, iSet = 0;

   if( ZH_ISLOGICAL( 2 ) )
   {
      if( zh_parl( 2 ) )
         iSet = ZH_COM_MCR_RTS;
      else
         iClr = ZH_COM_MCR_RTS;
   }
   zh_comMCR( zh_parni( 1 ), &iMCR, iClr, iSet );
   zh_retl( ( iMCR & ZH_COM_MCR_RTS ) != 0 );
}

/* com_DTR( <nComPort>, [<lNewDTRStatus>] ) --> <lOldDTRStatus>
 */
ZH_FUNC( COM_DTR )
{
   int iMCR, iClr = 0, iSet = 0;

   if( ZH_ISLOGICAL( 2 ) )
   {
      if( zh_parl( 2 ) )
         iSet = ZH_COM_MCR_DTR;
      else
         iClr = ZH_COM_MCR_DTR;
   }
   zh_comMCR( zh_parni( 1 ), &iMCR, iClr, iSet );
   zh_retl( ( iMCR & ZH_COM_MCR_DTR ) != 0 );
}

/* com_MCR( <nComPort>, [<nMCR>] ) --> <nMCR> (MCR_*)
 */
ZH_FUNC( COM_MCR )
{
   int iMCR, iClr, iSet;

   if( ZH_IS_PARAM_NUM( 2 ) )
   {
      iClr = 0xff;
      iSet = zh_parni( 2 ) & 0xff;
   }
   else
      iClr = iSet = 0;

   if( zh_comMCR( zh_parni( 1 ), &iMCR, iClr, iSet ) == -1 )
      iMCR = MCR_ERROR;

   zh_retni( iMCR );
}

/* com_MSR( <nComPort> ) --> <nMSR> (MSR_*)
 */
ZH_FUNC( COM_MSR )
{
   int iMSR;

   if( zh_comMSR( zh_parni( 1 ), &iMSR ) == -1 )
      iMSR = MSR_ERROR;

   zh_retni( iMSR );
}

/* com_LSR( <nComPort> ) --> <nLSR> (LSR_*)
 */
ZH_FUNC( COM_LSR )
{
   int iLSR;

   if( zh_comLSR( zh_parni( 1 ), &iLSR ) == -1 )
      iLSR = LSR_ERROR;

   zh_retni( iLSR );
}

/* com_Break( <nComPort>, <nDurationInMilliSecs>=100 ) --> <lSuccess>
 */
ZH_FUNC( COM_BREAK )
{
   zh_retl( zh_comSendBreak( zh_parni( 1 ), zh_parnidef( 2, 100 ) ) != 0 );
}

/* com_Hard( <nComPort>, [<lNewHandshake>], [<lDTR/DSR>] ) --> <lOldHandshake>
 */
ZH_FUNC( COM_HARD )
{
   int iPort = zh_parni( 1 ), iFlow, iMask;
   ZH_BOOL fResult = ZH_FALSE;

   if( zh_comFlowControl( iPort, &iFlow, -1 ) != -1 )
   {
      iMask = zh_parl( 3 ) ? ( ZH_COM_FLOW_IDTRDSR | ZH_COM_FLOW_ODTRDSR ) :
                             ( ZH_COM_FLOW_IRTSCTS | ZH_COM_FLOW_ORTSCTS );
      fResult = ( iFlow & iMask ) == iMask;

      if( ZH_ISLOGICAL( 2 ) )
      {
         iFlow &= ~( ZH_COM_FLOW_IDTRDSR | ZH_COM_FLOW_ODTRDSR |
                     ZH_COM_FLOW_IRTSCTS | ZH_COM_FLOW_ORTSCTS );
         if( zh_parl( 2 ) )
            iFlow |= iMask;
         zh_comFlowControl( iPort, NULL, iFlow );
      }
   }
   zh_retl( fResult );
}

/* com_Soft( <nComPort>, [<lNewHandshake>],
             [<cXONchar>], [<cXOFFchar>] ) --> <lOldHandshake>
 */
ZH_FUNC( COM_SOFT )
{
   int iPort = zh_parni( 1 ), iFlow, iMask;
   ZH_BOOL fResult = ZH_FALSE;

   if( zh_comFlowControl( iPort, &iFlow, -1 ) != -1 )
   {
      iMask = ( ZH_COM_FLOW_XON | ZH_COM_FLOW_XOFF );
      fResult = ( iFlow & iMask ) == iMask;

      if( ZH_ISLOGICAL( 2 ) )
      {
         if( zh_parl( 2 ) )
            iFlow |= iMask;
         else
            iFlow &= ~iMask;
         zh_comFlowControl( iPort, NULL, iFlow );
      }
      if( zh_pcount() > 2 )
         zh_comFlowChars( iPort, zh_ctComCharParam( 3 ), zh_ctComCharParam( 4 ) );
   }
   zh_retl( fResult );
}

/* com_Soft_R( <nComPort>, [<lXOFFFlag>] ) --> <lXOFFFlag>
 */
ZH_FUNC( COM_SOFT_R )
{
   ZH_BOOL fResult = ZH_FALSE;
   int iPort = zh_parni( 1 ), iMode;

   if( ZH_ISLOGICAL( 2 ) )
      zh_comFlowSet( iPort, ZH_COM_FL_SOFT |
                            ( zh_parl( 2 ) ? ZH_COM_FL_OOFF : ZH_COM_FL_OON ) );

   iMode = zh_comOutputState( iPort );
   if( iMode > 0 )
      fResult = ( iMode & ZH_COM_TX_XOFF ) != 0;

   zh_retl( fResult );
}

/* com_Soft_S( <nComPort> ) --> <lXOFFFlag>
 */
ZH_FUNC( COM_SOFT_S )
{
   ZH_BOOL fResult = ZH_FALSE;
   int iMode = zh_comInputState( zh_parni( 1 ) );

   if( iMode > 0 )
      fResult = ( iMode & ZH_COM_RX_XOFF ) != 0;

   zh_retl( fResult );
}

/* com_ErrChr( <nComPort>, [<nErrorCharacter|cErrorCharacter>] ) --> <lChanged>
 */
ZH_FUNC( COM_ERRCHR )
{
   zh_retl( zh_comErrorChar( zh_parni( 1 ), zh_ctComCharParam( 2 ) ) != -1 );
}

/* com_Remote( <nComPort>, [<nCharacter|cCharacter>] ) --> <lActive>
 */
ZH_FUNC( COM_REMOTE )
{
   zh_retl( zh_comDiscardChar( zh_parni( 1 ), zh_ctComCharParam( 2 ) ) > 0 );
}

/* com_SMode( <nComPort> ) --> <nSendMode>
 */
ZH_FUNC( COM_SMODE )
{
   int iMode = zh_comOutputState( zh_parni( 1 ) ), iResult = 0;

   if( iMode > 0 )
   {
      if( iMode & ZH_COM_TX_EMPTY )
         iResult |= SMODE_EMPTY;
      if( iMode & ZH_COM_TX_XOFF )
         iResult |= SMODE_SOFT;
      if( iMode & ( ZH_COM_TX_CTS | ZH_COM_TX_DSR | ZH_COM_TX_DCD ) )
         iResult |= SMODE_HARD;
      if( iMode & ZH_COM_TX_RFLUSH )
         iResult |= SMODE_RFLUSH;
   }

   zh_retni( iResult );
}

/* com_Event( <nComPort>, <nMode> ) --> <nCode>
 */
ZH_FUNC( COM_EVENT )
{
   /* TODO: unsupported */
   zh_retni( 0 );
}

/* com_Key( <nComPort>, [<nKeyValue1>], [<nKeyValue2>] ) --> <lActive>
 */
ZH_FUNC( COM_KEY )
{
   /* TODO: unsupported */
   zh_retl( ZH_FALSE );
}

/* com_SKey( [<nComPort>], [<nKeyValue1|cKeyValue1>],
 *                         [<nKeyValue2|cKeyValue2>] ) --> <lActive>
 */
ZH_FUNC( COM_SKEY )
{
   /* TODO: unsupported */
   zh_retl( ZH_FALSE );
}

/* com_Init( <nComPort>, [<nBaudRate>=300], [<cParity:E,O,M,S,N>=N],
 *           [<nDataLength:7,8>=8], [<nStopBits:1,2>=1] ) --> <lInitialized>
 */
ZH_FUNC( COM_INIT )
{
   int iPort = zh_parni( 1 ),
       iBaud = zh_parnidef( 2, 300 ),
       iParity = zh_parcx( 3 )[ 0 ],
       iSize = zh_parnidef( 4, 8 ),
       iStop = zh_parnidef( 5, 1 );

   zh_retl( zh_comInit( iPort, iBaud, iParity, iSize, iStop ) != -1 );
}

/* com_Open( <nComPort>, [<nBufferIn>=100] [, <nBufferOut>=0],
 *           [<lTrapMode>] ) --> <lStatus>
 */
ZH_FUNC( COM_OPEN )
{
   int iPort = zh_parni( 1 );

   /* TODO: add support for <nBufferIn> */
   /* TODO: add support for <nBufferOut> */
   /* TODO: add support for <lTrapMode> */
   zh_comClose( iPort );
   zh_retl( zh_comOpen( iPort ) != -1 );
}

/* com_Close( <nComPort> ) --> <lClosed>
 */
ZH_FUNC( COM_CLOSE )
{
   int iPort = zh_parni( 1 );

   zh_comFlush( iPort, ZH_COM_IOFLUSH );
   zh_retl( zh_comClose( iPort ) != -1 );
}

/* com_Read( <nComPort>, [<nLength>], [<lNoDelete>] ) --> <cCharacterstring>
 */
ZH_FUNC( COM_READ )
{
   char buffer[ 1024 ];
   char * data;
   long lLen, lRecv;
   int iPort = zh_parni( 1 );

   /* TODO: add support for <lNoDelete> */

   if( ZH_IS_PARAM_NUM( 2 ) )
      lLen = zh_parnl( 2 );
   else
   {
      lLen = zh_comInputCount( iPort );
      if( lLen < ( long ) ( sizeof( buffer ) >> 1 ) )
         lLen = sizeof( buffer );
      else
         lLen <<= 2;
   }
   if( lLen <= ( long ) sizeof( buffer ) )
      data = buffer;
   else
      data = ( char * ) zh_xgrab( lLen + 1 );

   lRecv = zh_comRecv( iPort, buffer, lLen, 0 );
   if( lRecv < 0 )
      lRecv = 0;

   if( data == buffer )
      zh_retclen( data, lRecv );
   else if( lLen > 16 && ( lLen >> 2 ) > lRecv )
   {
      zh_retclen( data, lRecv );
      zh_xfree( data );
   }
   else
      zh_retclen_buffer( data, lRecv );
}

/* com_Send( <nComPort>, <cString|nChar> ) --> <nNotSendLength>
 */
ZH_FUNC( COM_SEND )
{
   const char * data = zh_parc( 2 );
   long lLen = 0;
   char buffer;

   /* TODO: add automatic drain call for ports open without send buffer */

   if( data )
      lLen = ( long ) zh_parclen( 2 );
   else if( ZH_IS_PARAM_NUM( 2 ) )
   {
      buffer = ( unsigned char ) zh_parni( 2 );
      data = &buffer;
      lLen = 1;
   }

   if( lLen )
   {
      long lResult = zh_comSend( zh_parni( 1 ), data, lLen, 0 );
      if( lResult > 0 )
         lLen -= lResult;
   }

   zh_retnl( lLen );
}

/* com_Num() --> <nMaxCom>
 */
ZH_FUNC( COM_NUM )
{
   zh_retni( zh_comLastNum() );
}

/* com_GetIO( <nComPort> ) --> <nIOPort> | -1
 */
ZH_FUNC( COM_GETIO )
{
   /* TODO! */
}

/* com_SetIO( <nComPort>, <nIOPort|cIOPort> ) --> <lChanged>
 */
ZH_FUNC( COM_SETIO )
{
   /* TODO! */
}

/* com_GetIRQ( <nComPort> ) --> <nIRQ> | -1
 */
ZH_FUNC( COM_GETIRQ )
{
   /* TODO! */
}

/* com_SetIRQ( <nComPort>, <nIRQ|cIRQ> ) --> <lChanged>
 */
ZH_FUNC( COM_SETIRQ )
{
   /* TODO! */
}

/* com_DevName( <nComPort> [, <cNewName> ] ) --> <cPrevName>
 */
ZH_FUNC( COM_DEVNAME )
{
   int iPort = zh_parni( 1 );
   const char * szDevName = zh_parc( 2 );
   char buffer[ ZH_COM_DEV_NAME_MAX ];

   zh_retc( zh_comGetDevice( iPort, buffer, sizeof( buffer ) ) );
   if( szDevName )
      zh_comSetDevice( iPort, szDevName );
}
