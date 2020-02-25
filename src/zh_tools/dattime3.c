/*
 * CT3 Date & Time functions:
 *   WaitPeriod(), TimeValid(), SetTime(), SetDate()
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

/* stime() exists only in SVr4, SVID, X/OPEN and Linux */
#ifndef _SVID_SOURCE
#define _SVID_SOURCE
#endif

#include "zh_api.h"
#include "zh_date.h"
#include "zh_stack.h"

#if defined( ZH_OS_WIN )
#  include <windows.h>
#elif defined( ZH_OS_DOS )
#  include <dos.h>
#endif
#include <time.h>

typedef struct
{
   /* even if these are chars, variable must be int, since we need an extra -1 */
   double dTimeSet;
   double dTimeCounter;
} CT_DATE, * PCT_DATE;

static void s_ct_date_init( void * cargo )
{
   PCT_DATE ct_date = ( PCT_DATE ) cargo;

   ct_date->dTimeSet     = 0;
   ct_date->dTimeCounter = 0;
}

static ZH_TSD_NEW( s_ct_date, sizeof( CT_DATE ), s_ct_date_init, NULL );

ZH_FUNC( WAITPERIOD )
{
   PCT_DATE ct_date = ( PCT_DATE ) zh_stackGetTSD( &s_ct_date );

   double d = zh_dateSeconds();

   if( zh_pcount() > 0 )
   {
      ct_date->dTimeSet     = d;
      ct_date->dTimeCounter = d + zh_parnd( 1 ) / 100.0;
   }

   if( d < ct_date->dTimeSet )
      d += 86400.0;

   zh_retl( d < ct_date->dTimeCounter );
}

static ZH_BOOL _zh_timeValid( const char * szTime, ZH_SIZE nLen, int * piDecode )
{
   ZH_BOOL fValid = ZH_FALSE;

   if( nLen == 2 || nLen == 5 || nLen == 8 || nLen == 11 )
   {
      static const int sc_iMax[] = { 23, 59, 59, 99 };
      int     i;
      ZH_SIZE nPos;

      fValid = ZH_TRUE;
      for( nPos = 0; fValid && nPos < nLen; ++nPos )
      {
         fValid = nPos % 3 == 2 ? szTime[ nPos ] == ':' :
                  ( szTime[ nPos ] >= '0' && szTime[ nPos ] <= '9' );
      }
      for( nPos = 0, i = 0; fValid && nPos < nLen; nPos += 3, ++i )
      {
         int iVal;
         iVal   = 10 * ( szTime[ nPos ] - '0' ) + ( szTime[ nPos + 1 ] - '0' );
         fValid = iVal <= sc_iMax[ i ];
         if( piDecode )
            piDecode[ i ] = iVal;
      }
   }

   return fValid;
}

ZH_FUNC( TIMEVALID )
{
   zh_retl( _zh_timeValid( zh_parc( 1 ), zh_parclen( 1 ), NULL ) );
}

ZH_FUNC( SETTIME )
{
   ZH_BOOL fResult = ZH_FALSE;
   int     iTime[ 4 ];

   iTime[ 0 ] = iTime[ 1 ] = iTime[ 2 ] = iTime[ 3 ] = 0;
   if( _zh_timeValid( zh_parc( 1 ), zh_parclen( 1 ), iTime ) )
   {
#if defined( ZH_OS_WIN )
      SYSTEMTIME st;
      GetLocalTime( &st );
      st.wHour         = ( WORD ) iTime[ 0 ];
      st.wMinute       = ( WORD ) iTime[ 1 ];
      st.wSecond       = ( WORD ) iTime[ 2 ];
      st.wMilliseconds = ( WORD ) iTime[ 3 ] * 10;
      fResult = SetLocalTime( &st );
#elif defined( ZH_OS_LINUX ) && ! defined( ZH_OS_ANDROID ) && ! defined( __WATCOMC__ )
      /* stime() exists only in SVr4, SVID, X/OPEN and Linux */
      ZH_ULONG lNewTime;
      time_t   tm;

      lNewTime = iTime[ 0 ] * 3600 + iTime[ 1 ] * 60 + iTime[ 2 ];
      tm       = time( NULL );
      tm      += lNewTime - ( tm % 86400 );
      fResult  = stime( &tm ) == 0;
#elif defined( ZH_OS_DOS )
      union REGS regs;
      regs.h.ah = 45;
      regs.h.ch = iTime[ 0 ];
      regs.h.cl = iTime[ 1 ];
      regs.h.dh = iTime[ 2 ];
      ZH_DOS_INT86( 0x21, &regs, &regs );
      fResult = regs.h.al == 0;
#endif
   }

   zh_retl( fResult );
}

ZH_FUNC( SETDATE )
{
   ZH_BOOL fResult = ZH_FALSE;
   long    lDate   = zh_pardl( 1 );

   if( lDate )
   {
      int iYear, iMonth, iDay;

      zh_dateDecode( lDate, &iYear, &iMonth, &iDay );
      if( iYear >= 1970 )
      {
#if defined( ZH_OS_WIN )
         SYSTEMTIME st;
         GetLocalTime( &st );
         st.wYear      = ( WORD ) iYear;
         st.wMonth     = ( WORD ) iMonth;
         st.wDay       = ( WORD ) iDay;
         st.wDayOfWeek = ( WORD ) zh_dateJulianDOW( lDate );
         fResult       = SetLocalTime( &st );
#elif defined( ZH_OS_LINUX ) && ! defined( ZH_OS_ANDROID ) && ! defined( __WATCOMC__ )
         /* stime() exists only in SVr4, SVID, X/OPEN and Linux */
         long   lNewDate;
         time_t tm;

         lNewDate = lDate - zh_dateEncode( 1970, 1, 1 );
         tm       = time( NULL );
         tm       = lNewDate * 86400 + ( tm % 86400 );
         fResult  = stime( &tm ) == 0;
#elif defined( ZH_OS_DOS )
         union REGS regs;
         regs.h.ah        = 43;
         regs.ZH_XREGS.cx = iYear;
         regs.h.dh        = iMonth;
         regs.h.dl        = iDay;
         ZH_DOS_INT86( 0x21, &regs, &regs );
         fResult = regs.h.al == 0;
#endif
      }
   }

   zh_retl( fResult );
}
