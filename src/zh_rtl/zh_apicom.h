/*
 * Serial communication functions and constant values
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

#ifndef ZH_APICOM_H_
#define ZH_APICOM_H_

#include "com.zhh"
#include "zh_api.h"

ZH_EXTERN_BEGIN

#define ZH_COM_PORT_MAX       256

#define ZH_COM_DEV_NAME_MAX   64

#define ZH_COM_ANY            -1
#define ZH_COM_DISABLED       0
#define ZH_COM_ENABLED        1
#define ZH_COM_OPEN           2

extern ZH_EXPORT int  zh_comLastNum( void );
extern ZH_EXPORT int  zh_comFindPort( const char * pszDevName, ZH_BOOL fCreate );
extern ZH_EXPORT int  zh_comOpen( int iPort );
extern ZH_EXPORT int  zh_comClose( int iPort );
extern ZH_EXPORT int  zh_comInit( int iPort, int iBaud, int iParity, int iSize, int iStop );
extern ZH_EXPORT long zh_comSend( int iPort, const void * data, long len, ZH_MAXINT timeout );
extern ZH_EXPORT long zh_comRecv( int iPort, void * data, long len, ZH_MAXINT timeout );
extern ZH_EXPORT void zh_comSetError( int iPort, int iError );
extern ZH_EXPORT int  zh_comGetError( int iPort );
extern ZH_EXPORT int  zh_comGetOsError( int iPort );
extern ZH_EXPORT int  zh_comInputCount( int iPort );
extern ZH_EXPORT int  zh_comOutputCount( int iPort );
extern ZH_EXPORT int  zh_comFlush( int iPort, int iType );
extern ZH_EXPORT int  zh_comMCR( int iPort, int * piValue, int iClr, int iSet );
extern ZH_EXPORT int  zh_comMSR( int iPort, int * piValue );
extern ZH_EXPORT int  zh_comLSR( int iPort, int * piValue );
extern ZH_EXPORT int  zh_comSendBreak( int iPort, int iDurationInMilliSecs );
extern ZH_EXPORT int  zh_comFlowControl( int iPort, int *piFlow, int iFlow );
extern ZH_EXPORT int  zh_comFlowSet( int iPort, int iFlow );
extern ZH_EXPORT int  zh_comFlowChars( int iPort, int iXONchar, int iXOFFchar );
extern ZH_EXPORT int  zh_comDiscardChar( int iPort, int iChar );
extern ZH_EXPORT int  zh_comErrorChar( int iPort, int iChar );
extern ZH_EXPORT int  zh_comOutputState( int iPort );
extern ZH_EXPORT int  zh_comInputState( int iPort );
extern ZH_EXPORT int  zh_comSetDevice( int iPort, const char * pszDevName );
extern ZH_EXPORT const char * zh_comGetDevice( int iPort, char * buffer, int size );
extern ZH_EXPORT ZH_FHANDLE zh_comGetDeviceHandle( int iPort );

ZH_EXTERN_END

#endif /* ZH_APICOM_H_ */
