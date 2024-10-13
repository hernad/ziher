/*

 * Copyright 1999-2016 Viktor Szakats
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

/* NOTE: szTime must be 9 chars large. */

static char * zh_SecToTimeStr( char * pszTime, long lTime )
{
   int iValue;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_SecToTimeStr(%p, %ld)", ( void * ) pszTime, lTime ) );

   iValue = ( int ) ( ( lTime / 3600 ) % 24 );
   pszTime[ 0 ] = ( char ) ( iValue / 10 ) + '0';
   pszTime[ 1 ] = ( char ) ( iValue % 10 ) + '0';
   pszTime[ 2 ] = ':';
   iValue = ( int ) ( ( lTime / 60 ) % 60 );
   pszTime[ 3 ] = ( char ) ( iValue / 10 ) + '0';
   pszTime[ 4 ] = ( char ) ( iValue % 10 ) + '0';
   pszTime[ 5 ] = ':';
   iValue = ( int ) ( lTime % 60 );
   pszTime[ 6 ] = ( char ) ( iValue / 10 ) + '0';
   pszTime[ 7 ] = ( char ) ( iValue % 10 ) + '0';
   pszTime[ 8 ] = '\0';

   return pszTime;
}

static long zh_TimeStrToSec( const char * pszTime )
{
   ZH_SIZE nLen;
   long lTime = 0;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_TimeStrToSec(%s)", pszTime ) );

   nLen = strlen( pszTime );

   if( nLen >= 1 )
      lTime += ( long ) zh_strVal( pszTime, nLen ) * 3600;

   if( nLen >= 4 )
      lTime += ( long ) zh_strVal( pszTime + 3, nLen - 3 ) * 60;

   if( nLen >= 7 )
      lTime += ( long ) zh_strVal( pszTime + 6, nLen - 6 );

   return lTime;
}

ZH_FUNC( DAYS )
{
   zh_retnl( zh_parnl( 1 ) / 86400 );
}

ZH_FUNC( ELAPTIME )
{
   long lStart = zh_TimeStrToSec( zh_parcx( 1 ) );
   long lEnd   = zh_TimeStrToSec( zh_parcx( 2 ) );
   char szTime[ 9 ];

   zh_retc( zh_SecToTimeStr( szTime, ( lEnd < lStart ? 86400 : 0 ) + lEnd - lStart ) );
}

ZH_FUNC( SECS )
{
   zh_retnl( zh_TimeStrToSec( zh_parcx( 1 ) ) );
}

ZH_FUNC( TSTRING )
{
   char szTime[ 9 ];

   zh_retc( zh_SecToTimeStr( szTime, zh_parnl( 1 ) ) );
}
