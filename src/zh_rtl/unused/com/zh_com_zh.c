/*
 * Serial communication port API wrapper functions
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

/*
 * zh_comClose( nPort )  --> lSuccess
 * zh_comDiscardChar( nPort, nChar | cChar ) --> lSuccess
 * zh_comErrorChar( nPort, nChar | cChar ) --> lSuccess
 * zh_comFlowChars( nPort, nXONchar | cXONchar, nXOFFchar | cXOFFchar ) --> lSuccess
 * zh_comFlowControl( nPort, @nOldFlow [, nNewFlow] ) --> lSuccess
 * zh_comFlowSet( nPort, nFlow ) --> lSuccess
 * zh_comFlush( nPort, [ nType = ZH_COM_IOFLUSH ] ) --> lSuccess
 * zh_comGetDevice( nPort )  --> cDeviceName
 * zh_comGetDeviceHandle( nPort )  --> nHandle | F_ERROR
 * zh_comGetError( nPort ) --> nError
 * zh_comGetOSError( nPort ) --> nError
 * zh_comFindPort( cDeviceName [, lCreate = .F. ] ) --> nPort
 * zh_comInit( nPort, nBaud, cParity, nSize, nStop ) --> lSuccess
 * zh_comInputCount( nPort ) --> nCount
 * zh_comInputState( nPort ) --> nState
 * zh_comLastNum() --> nLastPortNumber
 * zh_comLSR( nPort, @nValue ) --> lSuccess
 * zh_comMCR( nPort, @nValue, nClear, nSet ) --> lSuccess
 * zh_comMSR( nPort, @nValue ) --> lSuccess
 * zh_comOpen( nPort ) --> lSuccess
 * zh_comOutputCount( nPort ) --> nCount
 * zh_comOutputState( nPort ) --> nState
 * zh_comSendBreak( nPort, [ nDuration = 50 ] ) --> lSuccess
 * zh_comSetDevice( nPort, cDeviceName ) --> lSuccess
 * zh_comSetError( nPort, nError ) --> NIL
 * zh_comRecv( nPort, @cBuffer, [ nLen = Len( cBuffer ) ], [ nTimeout = 0 ] ) --> nBytesRecv
 * zh_comSend( nPort, cBuffer, [ nLen = Len( cBuffer ) ], [ nTimeout = 0 ] ) --> nBytesSent
 */

#include "zh_api.h"
#include "zh_item_api.h"
#include "zh_api_error.h"
#include "zh_vm.h"
#include "zh_apicom.h"


ZH_FUNC( ZH_COMCLOSE )
{
   zh_retl( zh_comClose( zh_parni( 1 ) ) == 0 );
}

ZH_FUNC( ZH_COMDISCARDCHAR )
{
   zh_retl( zh_comDiscardChar( zh_parni( 1 ), ZH_ISCHAR( 2 ) ? zh_parc( 2 )[ 0 ] : zh_parni( 2 ) ) == 0 );
}

ZH_FUNC( ZH_COMERRORCHAR )
{
   zh_retl( zh_comErrorChar( zh_parni( 1 ), ZH_ISCHAR( 2 ) ? zh_parc( 2 )[ 0 ] : zh_parni( 2 ) ) == 0 );
}

ZH_FUNC( ZH_COMFLOWCHARS )
{
   zh_retl( zh_comFlowChars( zh_parni( 1 ), ZH_ISCHAR( 2 ) ? zh_parc( 2 )[ 0 ] : zh_parni( 2 ),
                             ZH_ISCHAR( 3 ) ? zh_parc( 3 )[ 0 ] : zh_parni( 3 ) ) == 0 );
}

ZH_FUNC( ZH_COMFLOWCONTROL )
{
   int iValue = 0;

   zh_retl( zh_comFlowControl( zh_parni( 1 ), &iValue, zh_parnidef( 3, -1 ) ) == 0 );
   zh_storni( iValue, 2 );
}

ZH_FUNC( ZH_COMFLOWSET )
{
   zh_retl( zh_comFlowSet( zh_parni( 1 ), zh_parni( 2 ) ) == 0 );
}

ZH_FUNC( ZH_COMFLUSH )
{
   zh_retl( zh_comFlush( zh_parni( 1 ), zh_parnidef( 2, ZH_COM_IOFLUSH ) ) == 0 );
}

ZH_FUNC( ZH_COMGETDEVICE )
{
   char buffer[ ZH_COM_DEV_NAME_MAX ];
   const char * name = zh_comGetDevice( zh_parni( 1 ), buffer, sizeof( buffer ) );

   zh_retc( name );
}

ZH_FUNC( ZH_COMGETDEVICEHANDLE )
{
   zh_retnint( ( ZH_NHANDLE ) zh_comGetDeviceHandle( zh_parni( 1 ) ) );
}

ZH_FUNC( ZH_COMGETERROR )
{
   zh_retni( zh_comGetError( zh_parni( 1 ) ) );
}

ZH_FUNC( ZH_COMGETOSERROR )
{
   zh_retni( zh_comGetOsError( zh_parni( 1 ) ) );
}

ZH_FUNC( ZH_COMFINDPORT )
{
   zh_retni( zh_comFindPort( zh_parc( 1 ), zh_parl( 2 ) ) );
}


ZH_FUNC( ZH_COMINIT )
{
   zh_retl( zh_comInit( zh_parni( 1 ), zh_parni( 2 ), ZH_ISCHAR( 3 ) ? zh_parc( 3 )[ 0 ] : 0,
                        zh_parni( 4 ), zh_parni( 5 ) ) == 0 );
}

ZH_FUNC( ZH_COMINPUTCOUNT )
{
   zh_retni( zh_comInputCount( zh_parni( 1 ) ) );
}

ZH_FUNC( ZH_COMINPUTSTATE )
{
   zh_retni( zh_comInputState( zh_parni( 1 ) ) );
}

ZH_FUNC( ZH_COMLASTNUM )
{
   zh_retni( zh_comLastNum() );
}

ZH_FUNC( ZH_COMLSR )
{
   int iValue = 0;

   zh_retl( zh_comLSR( zh_parni( 1 ), &iValue ) == 0 );
   zh_storni( iValue, 2 );
}

ZH_FUNC( ZH_COMMCR )
{
   int iValue = 0;

   zh_retl( zh_comMCR( zh_parni( 1 ), &iValue, zh_parni( 3 ), zh_parni( 4 ) ) == 0 );
   zh_storni( iValue, 2 );
}

ZH_FUNC( ZH_COMMSR )
{
   int iValue = 0;

   zh_retl( zh_comMSR( zh_parni( 1 ), &iValue ) == 0 );
   zh_storni( iValue, 2 );
}

ZH_FUNC( ZH_COMOPEN )
{
   zh_retl( zh_comOpen( zh_parni( 1 ) ) == 0 );
}

ZH_FUNC( ZH_COMOUTPUTCOUNT )
{
   zh_retni( zh_comOutputCount( zh_parni( 1 ) ) );
}

ZH_FUNC( ZH_COMOUTPUTSTATE )
{
   zh_retni( zh_comOutputState( zh_parni( 1 ) ) );
}

ZH_FUNC( ZH_COMSENDBREAK )
{
   /* 50ms break is enough for baud-rate 300 and higher */
   zh_retl( zh_comSendBreak( zh_parni( 1 ), zh_parnidef( 2, 50 ) ) == 0 );
}

ZH_FUNC( ZH_COMSETDEVICE )
{
   zh_retl( zh_comSetDevice( zh_parni( 1 ), zh_parc( 2 ) ) == 0 );
}

ZH_FUNC( ZH_COMSETERROR )
{
   zh_comSetError( zh_parni( 1 ), zh_parni( 2 ) );
}

ZH_FUNC( ZH_COMRECV )
{
   PZH_ITEM pItem = zh_param( 2, ZH_IT_STRING );
   char * pBuffer;
   ZH_SIZE nLen;

   if( pItem && ZH_ISBYREF( 2 ) && zh_itemGetWriteCL( pItem, &pBuffer, &nLen ) )
   {
      if( ZH_IS_PARAM_NUM( 3 ) )
      {
         long lRead = zh_parnl( 3 );
         if( lRead >= 0 && lRead < ( long ) nLen )
            nLen = lRead;
      }
      zh_retnl( zh_comRecv( zh_parni( 1 ), pBuffer, ( long ) nLen, zh_parnint( 4 ) ) );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_COMSEND )
{
   long  lLen = ( long ) zh_parclen( 2 );

   if( ZH_IS_PARAM_NUM( 3 ) )
   {
      long lParam = zh_parnl( 3 );

      if( lParam >= 0 && lParam < lLen )
         lLen = lParam;
   }
   zh_retnl( zh_comSend( zh_parni( 1 ), zh_parc( 2 ), lLen, zh_parnint( 4 ) ) );
}
