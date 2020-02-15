/*
 * Windows UNICODE conversion functions
 *
 * Copyright 2007 Przemyslaw Czerpak
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

static ZH_SIZE zh_wcnlen( const wchar_t * szText, ZH_SIZE nCount )
{
   ZH_SIZE nLen = 0;

   while( nCount-- && szText[ nLen ] )
      ++nLen;

   return nLen;
}

int zh_wctomblen( const wchar_t * szText )
{
   return WideCharToMultiByte( CP_ACP, 0, szText, -1, NULL, 0, NULL, NULL ) - 1;
}

void zh_wcntombcpy( char * dstA, const wchar_t * srcW, ZH_SIZE nLen )
{
   WideCharToMultiByte( CP_ACP, 0, srcW, -1, dstA, ( int ) nLen, NULL, NULL );
   dstA[ ( int ) nLen ] = '\0';
}

void zh_mbntowccpy( wchar_t * dstW, const char * srcA, ZH_SIZE nLen )
{
   MultiByteToWideChar( CP_ACP, 0, srcA, -1, dstW, ( int ) nLen );
   dstW[ ( int ) nLen ] = L'\0';
}

wchar_t * zh_mbtowc( const char * srcA )
{
   int length;
   wchar_t *dstW;

   length = MultiByteToWideChar( CP_ACP, 0, srcA, -1, NULL, 0 );
   dstW = ( wchar_t * ) zh_xgrab( length * sizeof( wchar_t ) );
   MultiByteToWideChar( CP_ACP, 0, srcA, -1, dstW, length );

   return dstW;
}

char * zh_wctomb( const wchar_t * srcW )
{
   int length;
   char *dstA;

   length = WideCharToMultiByte( CP_ACP, 0, srcW, -1, NULL, 0, NULL, NULL );
   dstA = ( char * ) zh_xgrab( length );
   WideCharToMultiByte( CP_ACP, 0, srcW, -1, dstA, length, NULL, NULL );

   return dstA;
}

wchar_t * zh_mbntowc( const char * srcA, ZH_SIZE nLen )
{
   int length;
   wchar_t *dstW;

   nLen = zh_strnlen( srcA, nLen );
   length = MultiByteToWideChar( CP_ACP, 0, srcA, ( int ) nLen, NULL, 0 );
   dstW = ( wchar_t * ) zh_xgrab( ( length + 1 ) * sizeof( wchar_t ) );
   MultiByteToWideChar( CP_ACP, 0, srcA, ( int ) nLen, dstW, length );
   dstW[ length ] = L'\0';

   return dstW;
}

char * zh_wcntomb( const wchar_t * srcW, ZH_SIZE nLen )
{
   int length;
   char *dstA;

   nLen = zh_wcnlen( srcW, nLen );
   length = WideCharToMultiByte( CP_ACP, 0, srcW, ( int ) nLen, NULL, 0, NULL, NULL );
   dstA = ( char * ) zh_xgrab( length + 1 );
   WideCharToMultiByte( CP_ACP, 0, srcW, ( int ) nLen, dstA, length, NULL, NULL );
   dstA[ length ] = '\0';

   return dstA;
}

#endif /* ZH_OS_WIN */
