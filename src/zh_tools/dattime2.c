/*
 * CT3 Date & Time functions, part II:
 *   AddMonth()
 *   DMY()
 *   DoY()
 *   IsLeap()
 *   LastDayOM()
 *   MDY()
 *   NToCDoW()
 *   NToCMonth()
 *   Quarter()
 *   Week()
 *
 * Copyright 2006 Pavel Tsarenko <tpe2@mail.ru>
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
#include "zh_codepage_api.h"
#include "zh_lang_api.h"
#include "zh_date.h"
#include "zh_set.h"

static ZH_BOOL ct_isleap( int iYear )
{
   return iYear != 0 && ( ( ( iYear & 3 ) == 0 && iYear % 100 != 0 ) ||
                          iYear % 400 == 0 );
}

static int ct_daysinmonth( int iMonth, ZH_BOOL bLeap )
{
   if( iMonth == 2 )
      return bLeap ? 29 : 28;
   else if( iMonth == 4 || iMonth == 6 || iMonth == 9 || iMonth == 11 )
      return 30;
   else
      return 31;
}

static int ct_daystomonth( int iMonth, ZH_BOOL bLeap )
{
   static const int sc_iMonths[] = {
      0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 };

   return ( iMonth < 1 || iMonth > 12 ) ? 0 : sc_iMonths[ iMonth - 1 ] +
          ( ( bLeap && iMonth > 2 ) ? 1 : 0 );
}

static int ct_doy( long lDate )
{
   int iYear, iMonth, iDay;
   long lFirst;

   zh_dateDecode( lDate, &iYear, &iMonth, &iDay );
   lFirst = zh_dateEncode( iYear, 1, 1 );
   return ( int ) ( lDate - lFirst + 1 );
}

ZH_FUNC( CTODOW )
{
   ZH_SIZE nLen = zh_parclen( 1 );
   int iDow = 0;

   if( nLen )
   {
      PZH_CODEPAGE cdp = zh_vmCodepage();
      const char * szParam = zh_parc( 1 );

      for( iDow = 1; iDow <= 7; ++iDow )
      {
         const char * szDow = zh_langDGetItem( ZH_LANG_ITEM_BASE_DAY + iDow - 1 );
         if( zh_cdpicmp( szDow, strlen( szDow ), szParam, nLen, cdp, ZH_FALSE ) == 0 )
            break;
      }
   }

   zh_retnl( iDow );
}

ZH_FUNC( CTOMONTH )
{
   ZH_SIZE nLen = zh_parclen( 1 );
   int iMonth = 0;

   if( nLen )
   {
      PZH_CODEPAGE cdp = zh_vmCodepage();
      const char * szParam = zh_parc( 1 );
      for( iMonth = 1; iMonth <= 12; ++iMonth )
      {
         const char * szMonth = zh_langDGetItem( ZH_LANG_ITEM_BASE_MONTH + iMonth - 1 );
         if( zh_cdpicmp( szMonth, strlen( szMonth ), szParam, nLen, cdp, ZH_FALSE ) == 0 )
            break;
      }
   }

   zh_retnl( iMonth );
}

ZH_FUNC( DMY )
{
   int iYear, iMonth, iDay;
   ZH_BOOL bMode = ZH_FALSE;

   if( ZH_ISDATETIME( 1 ) )
      zh_dateDecode( zh_pardl( 1 ), &iYear, &iMonth, &iDay );
   else
      zh_dateToday( &iYear, &iMonth, &iDay );

   if( ZH_ISLOGICAL( 2 ) )
      bMode = zh_parl( 2 );

   if( iMonth >= 1 && iMonth <= 12 )
   {
      const char * szMonth = zh_langDGetItem( ZH_LANG_ITEM_BASE_MONTH + iMonth - 1 );
      int iMonLen = ( int ) strlen( szMonth );
      int iLen = 0, iBufLen = iMonLen + 10;
      char * szMDY = ( char * ) zh_xgrab( iBufLen );

      if( iDay < 10 )
      {
         szMDY[ iLen ] = ( char ) iDay + 0x30;
         iLen++;
      }
      else
      {
         zh_snprintf( szMDY + iLen, 3, "%02d", iDay );
         iLen += 2;
      }

      if( bMode )
      {
         szMDY[ iLen ] = '.';
         iLen++;
      }
      szMDY[ iLen ] = ' ';
      iLen++;

      zh_strncpy( szMDY + iLen, szMonth, iBufLen - iLen - 1 );
      iLen += iMonLen;
      szMDY[ iLen ] = ' ';
      iLen++;

      if( zh_setGetCentury() )
      {
         zh_snprintf( szMDY + iLen, 5, "%04d", iYear );
         iLen += 4;
      }
      else
      {
         zh_snprintf( szMDY + iLen, 3, "%02d", iYear % 100 );
         iLen += 2;
      }

      zh_retclen( szMDY, iLen );
      zh_xfree( szMDY );
   }
   else
      zh_retc_null();
}

ZH_FUNC( MDY )
{
   int iYear, iMonth, iDay;

   if( ZH_ISDATETIME( 1 ) )
      zh_dateDecode( zh_pardl( 1 ), &iYear, &iMonth, &iDay );
   else
      zh_dateToday( &iYear, &iMonth, &iDay );

   if( iMonth >= 1 && iMonth <= 12 )
   {
      const char * szMonth = zh_langDGetItem( ZH_LANG_ITEM_BASE_MONTH + iMonth - 1 );
      int iLen = ( int ) strlen( szMonth );
      int iBufLen = iLen + 9;
      char * szMDY = ( char * ) zh_xgrab( iBufLen );

      zh_strncpy( szMDY, szMonth, iBufLen - 1 );
      szMDY[ iLen++ ] = ' ';
      if( iDay < 10 )
      {
         szMDY[ iLen ] = ( char ) iDay + 0x30;
         iLen++;
      }
      else
      {
         zh_snprintf( szMDY + iLen, 3, "%02d", iDay );
         iLen += 2;
      }
      szMDY[ iLen++ ] = ' ';

      if( zh_setGetCentury() )
      {
         zh_snprintf( szMDY + iLen, 5, "%04d", iYear );
         iLen += 4;
      }
      else
      {
         zh_snprintf( szMDY + iLen, 3, "%02d", iYear % 100 );
         iLen += 2;
      }

      zh_retclen( szMDY, iLen );
      zh_xfree( szMDY );
   }
   else
      zh_retc_null();
}

ZH_FUNC( ADDMONTH )
{
   long lJulian, lMillisec = 0;
   int iYear, iMonth, iDay, iNum, iDays;
   ZH_BOOL fTimeStamp = ZH_FALSE;

   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      iNum = zh_parni( 1 );
      zh_dateToday( &iYear, &iMonth, &iDay );
   }
   else
   {
      if( ZH_ISTIMESTAMP( 1 ) )
      {
         fTimeStamp = ZH_TRUE;
         if( ! zh_partdt( &lJulian, &lMillisec, 1 ) )
            lJulian = lMillisec = 0;  /* to silence Coverity analyzer */
         zh_dateDecode( lJulian, &iYear, &iMonth, &iDay );
      }
      else if( ZH_ISDATE( 1 ) )
         zh_dateDecode( zh_pardl( 1 ), &iYear, &iMonth, &iDay );
      else
         zh_dateToday( &iYear, &iMonth, &iDay );
      iNum = zh_parni( 2 );
   }

   iMonth += iNum;
   while( iMonth <= 0 )
   {
      iMonth += 12;
      iYear--;
   }
   while( iMonth > 12 )
   {
      iMonth -= 12;
      iYear++;
   }

   iDays = ct_daysinmonth( iMonth, ct_isleap( iYear ) );
   if( iDay > iDays )
      iDay = iDays;

   lJulian = zh_dateEncode( iYear, iMonth, iDay );
   if( fTimeStamp )
      zh_rettdt( lJulian, lMillisec );
   else
      zh_retdl( lJulian );
}

ZH_FUNC( DOY )
{
   long lDate;

   if( ZH_ISDATETIME( 1 ) )
      lDate = zh_pardl( 1 );
   else
   {
      int iYear, iMonth, iDay;

      zh_dateToday( &iYear, &iMonth, &iDay );
      lDate = zh_dateEncode( iYear, iMonth, iDay );
   }

   zh_retni( ct_doy( lDate ) );
}

ZH_FUNC( ISLEAP )
{
   int iYear, iMonth, iDay;

   if( ZH_ISDATETIME( 1 ) )
      zh_dateDecode( zh_pardl( 1 ), &iYear, &iMonth, &iDay );
   else
      zh_dateToday( &iYear, &iMonth, &iDay );

   zh_retl( ct_isleap( iYear ) );
}

ZH_FUNC( QUARTER )
{
   int iYear, iMonth, iDay;

   if( ZH_ISDATETIME( 1 ) )
      zh_dateDecode( zh_pardl( 1 ), &iYear, &iMonth, &iDay );
   else
      zh_dateToday( &iYear, &iMonth, &iDay );

   zh_retni( ( iMonth + 2 ) / 3 );
}

ZH_FUNC( LASTDAYOM )
{
   ZH_BOOL bLeap = ZH_FALSE;
   int iYear, iMonth, iDay;

   if( ZH_IS_PARAM_NUM( 1 ) )
      iMonth = zh_parni( 1 );
   else
   {
      if( ZH_ISDATETIME( 1 ) )
         zh_dateDecode( zh_pardl( 1 ), &iYear, &iMonth, &iDay );
      else
         zh_dateToday( &iYear, &iMonth, &iDay );

      bLeap = ct_isleap( iYear );
   }

   zh_retni( ( iMonth && iMonth <= 12 ) ? ct_daysinmonth( iMonth, bLeap ) : 0 );
}

ZH_FUNC( NTOCDOW )
{
   zh_retc( zh_dateCDOW( zh_parni( 1 ) ) );
}

ZH_FUNC( NTOCMONTH )
{
   zh_retc( zh_dateCMonth( zh_parni( 1 ) ) );
}

ZH_FUNC( WEEK )
{
   int iYear, iMonth, iDay, iWeek;
   long lDate;
   ZH_BOOL bSWN = zh_parl( 2 );

   if( ZH_ISDATETIME( 1 ) )
   {
      lDate = zh_pardl( 1 );
      zh_dateDecode( lDate, &iYear, &iMonth, &iDay );
   }
   else
   {
      zh_dateToday( &iYear, &iMonth, &iDay );
      lDate = zh_dateEncode( iYear, iMonth, iDay );
   }

   if( ! lDate )
   {
      iWeek = 0;
   }
   else if( bSWN )
   {
      int iDays = ct_daystomonth( iMonth, ct_isleap( iYear ) ) + iDay;
      int iPart = ( iDays % 7 );

      iWeek = iDays / 7;
      if( iPart > 0 )
         iWeek++;
   }
   else
   {
      long lDate2;

      if( zh_setGetCPtr( ZH_SET_DATEFORMAT ) && ( zh_setGetCPtr( ZH_SET_DATEFORMAT )[ 0 ] == 'd' ||
                                                  zh_setGetCPtr( ZH_SET_DATEFORMAT )[ 0 ] == 'D' ) )
         lDate2 = lDate + 3 - ( zh_dateDOW( iYear, iMonth, iDay ) + 5 ) % 7;
      else
         lDate2 = lDate + 4 - zh_dateDOW( iYear, iMonth, iDay );

      iWeek = ( ct_doy( lDate2 ) - 1 ) / 7 + 1;
   }

   zh_retni( iWeek );
}
