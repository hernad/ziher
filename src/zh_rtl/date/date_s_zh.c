/*
 * The Date API (Ziher level)
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * Copyright 1999 Jose Lalin <dezac@corevia.com> (Day(), Month(), Year(), DoW())
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com> (CToD(), Date())
 * Copyright 1999-2001 Viktor Szakats (zh_SToD())
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
#include "zh_api_error.h"
#include "zh_item_api.h"
#include "zh_set.h"
#include "zh_date.h"

ZH_FUNC( CTOD )
{
   if( ZH_ISCHAR( 1 ) )
      zh_retdl( zh_dateUnformat( zh_parc( 1 ), zh_setGetDateFormat() ) );
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1119, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_CTOD )
{
   if( ZH_ISCHAR( 1 ) )
   {
      const char * szFormat = zh_parc( 2 );

      if( ! szFormat )
         szFormat = zh_setGetDateFormat();
      zh_retdl( zh_dateUnformat( zh_parc( 1 ), szFormat ) );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1119, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( DTOC )
{
   if( ZH_ISDATETIME( 1 ) )
   {
      char szDate[ 9 ];
      char szFormatted[ 11 ];

      zh_retc( zh_dateFormat( zh_pardsbuff( szDate, 1 ), szFormatted, zh_setGetDateFormat() ) );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1118, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_DTOC )
{
   if( ZH_ISDATETIME( 1 ) )
   {
      char szDate[ 9 ];
      char szFormatted[ 11 ];
      const char * szFormat = zh_parc( 2 );

      if( ! szFormat )
         szFormat = zh_setGetDateFormat();
      zh_retc( zh_dateFormat( zh_pardsbuff( szDate, 1 ), szFormatted, szFormat ) );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1118, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( DTOS )
{
   if( ZH_ISDATETIME( 1 ) )
   {
      char szDate[ 9 ];

      zh_retc( zh_pardsbuff( szDate, 1 ) );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1120, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}


ZH_FUNC( ZH_STOD )
{
   PZH_ITEM pDateString = zh_param( 1, ZH_IT_STRING );

   zh_retds( zh_itemGetCLen( pDateString ) >= 7 ? zh_itemGetCPtr( pDateString ) : NULL );
}

ZH_FUNC( YEAR )
{
   PZH_ITEM pDate = zh_param( 1, ZH_IT_DATETIME );

   if( pDate )
   {
      int iYear, iMonth, iDay;

      zh_dateDecode( zh_itemGetDL( pDate ), &iYear, &iMonth, &iDay );

      zh_retnilen( iYear, 5 );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1112, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( MONTH )
{
   PZH_ITEM pDate = zh_param( 1, ZH_IT_DATETIME );

   if( pDate )
   {
      int iYear, iMonth, iDay;

      zh_dateDecode( zh_itemGetDL( pDate ), &iYear, &iMonth, &iDay );

      zh_retnilen( iMonth, 3 );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1113, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( DAY )
{
   PZH_ITEM pDate = zh_param( 1, ZH_IT_DATETIME );

   if( pDate )
   {
      int iYear, iMonth, iDay;

      zh_dateDecode( zh_itemGetDL( pDate ), &iYear, &iMonth, &iDay );

      zh_retnilen( iDay, 3 );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1114, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( DOW )
{
   PZH_ITEM pDate = zh_param( 1, ZH_IT_DATETIME );

   if( pDate )
      zh_retnilen( zh_dateJulianDOW( zh_itemGetDL( pDate ) ), 3 );
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1115, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( TIME )
{
   char szResult[ 9 ];

   zh_dateTimeStr( szResult );
   zh_retclen( szResult, 8 );
}

ZH_FUNC( DATE )
{
   int iYear, iMonth, iDay;

   zh_dateToday( &iYear, &iMonth, &iDay );
   zh_retd( iYear, iMonth, iDay );
}

ZH_FUNC( ZH_DATE )
{
   if( zh_pcount() == 0 )
   {
      int iYear, iMonth, iDay;
      zh_dateToday( &iYear, &iMonth, &iDay );
      zh_retd( iYear, iMonth, iDay );
   }
   else
      zh_retd( zh_parni( 1 ), zh_parni( 2 ), zh_parni( 3 ) );
}

ZH_FUNC( ZH_DATETIME )
{
   if( zh_pcount() == 0 )
   {
      long lDate, lTime;
      zh_timeStampGet( &lDate, &lTime );
      zh_rettdt( lDate, lTime );
   }
   else
      zh_rettdt( zh_dateEncode( zh_parni( 1 ), zh_parni( 2 ), zh_parni( 3 ) ),
                 zh_timeEncode( zh_parni( 4 ), zh_parni( 5 ), zh_parni( 6 ), zh_parni( 7 ) ) );
}

ZH_FUNC( ZH_DTOT )
{
   long lDate, lTime, lDate2;

   if( zh_partdt( &lDate, &lTime, 1 ) )
   {
      const char * szTime = zh_parc( 2 );
      if( szTime )
         zh_timeStampStrGetDT( szTime, &lDate2, &lTime );
      else if( ZH_IS_PARAM_NUM( 2 ) )
      {
         lTime = ( long ) ( zh_parnd( 2 ) * 1000 );
         if( lTime < 0 )
            lTime = 0;
      }
      else
         lTime = 0;
      zh_rettdt( lDate, lTime );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_TTOD )
{
   long lDate, lTime;

   if( zh_partdt( &lDate, &lTime, 1 ) )
   {
      zh_retdl( lDate );
      if( ZH_ISBYREF( 2 ) )
      {
         const char * szTimeFormat = zh_parc( 3 );
         if( szTimeFormat )
         {
            char szBuffer[ 27 ];
            if( *szTimeFormat == '\0' )
               szTimeFormat = zh_setGetTimeFormat();
            zh_storc( zh_timeFormat( szBuffer, szTimeFormat, lTime ), 2 );
         }
         else
            zh_stornd( ( double ) lTime / 1000, 2 );
      }
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_TTON )
{
   PZH_ITEM pTime = zh_param( 1, ZH_IT_DATETIME );

   if( pTime )
      zh_retnd( zh_itemGetTD( pTime ) );
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_NTOT )
{
   PZH_ITEM pNum = zh_param( 1, ZH_IT_NUMERIC );

   if( pNum )
      zh_rettd( zh_itemGetND( pNum ) );
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_NTOMSEC )
{
   PZH_ITEM pNum = zh_param( 1, ZH_IT_NUMERIC );

   if( pNum )
      zh_retnint( ( ZH_MAXINT ) ( zh_itemGetND( pNum ) * ZH_MILLISECS_PER_DAY ) );
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_NTOSEC )
{
   PZH_ITEM pNum = zh_param( 1, ZH_IT_NUMERIC );

   if( pNum )
      zh_retnd( zh_itemGetND( pNum ) * ZH_SECONDS_PER_DAY );
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_NTOMIN )
{
   PZH_ITEM pNum = zh_param( 1, ZH_IT_NUMERIC );

   if( pNum )
      zh_retnd( zh_itemGetND( pNum ) * ZH_MINUTES_PER_DAY );
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_NTOHOUR )
{
   PZH_ITEM pNum = zh_param( 1, ZH_IT_NUMERIC );

   if( pNum )
      zh_retnd( zh_itemGetND( pNum ) * 24 );
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_TTOSEC )
{
   long lDate, lTime;

   if( zh_partdt( &lDate, &lTime, 1 ) )
      zh_retnd( ( double ) lDate * ZH_SECONDS_PER_DAY + ( double ) lTime / 1000 );
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_SECTOT )
{
   PZH_ITEM pNum = zh_param( 1, ZH_IT_NUMERIC );

   if( pNum )
      zh_rettd( zh_itemGetND( pNum ) / ZH_SECONDS_PER_DAY );
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_MSECTOT )
{
   PZH_ITEM pNum = zh_param( 1, ZH_IT_NUMERIC );

   if( pNum )
      zh_rettd( zh_itemGetND( pNum ) / ZH_MILLISECS_PER_DAY );
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_TTOMSEC )
{
   long lDate, lTime;

   if( zh_partdt( &lDate, &lTime, 1 ) )
      zh_retnd( ( double ) lDate * ZH_MILLISECS_PER_DAY + lTime );
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_TTOMIN )
{
   long lDate, lTime;

   if( zh_partdt( &lDate, &lTime, 1 ) )
      zh_retnd( ( double ) lDate * ZH_MINUTES_PER_DAY + ( double ) lTime / ( 60 * 1000 ) );
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_TTOHOUR )
{
   long lDate, lTime;

   if( zh_partdt( &lDate, &lTime, 1 ) )
      zh_retnd( ( double ) lDate * 24 + ( double ) lTime / ( 60 * 60 * 1000 ) );
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_TTOC )
{
   long lDate, lTime;

   if( zh_partdt( &lDate, &lTime, 1 ) )
   {
      const char * szDateFormat = zh_parc( 2 );
      const char * szTimeFormat = zh_parc( 3 );
      char szBuffer[ 27 ];

      if( ! szDateFormat )
         szDateFormat = zh_setGetDateFormat();
      if( ! szTimeFormat )
         szTimeFormat = zh_setGetTimeFormat();

      zh_retc( zh_timeStampFormat( szBuffer, szDateFormat, szTimeFormat,
                                   lDate, lTime ) );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_CTOT )
{
   const char * szDateTime = zh_parc( 1 );

   if( szDateTime )
   {
      long lDate, lTime;
      const char * szDateFormat = zh_parc( 2 );
      const char * szTimeFormat = zh_parc( 3 );

      if( ! szDateFormat )
         szDateFormat = zh_setGetDateFormat();
      if( ! szTimeFormat )
         szTimeFormat = zh_setGetTimeFormat();

      zh_timeStampUnformat( szDateTime, szDateFormat, szTimeFormat, &lDate, &lTime );
      zh_rettdt( lDate, lTime );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_TTOS )
{
   long lDate, lTime;

   if( zh_partdt( &lDate, &lTime, 1 ) )
   {
      char szBuffer[ 18 ];

      zh_retc( zh_timeStampStrRawPut( szBuffer, lDate, lTime ) );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_STOT )
{
   const char * szDateTime = zh_parc( 1 );

   if( szDateTime )
   {
      long lDate, lTime;

      zh_timeStampStrRawGet( szDateTime, &lDate, &lTime );
      zh_rettdt( lDate, lTime );
   }
   else
      zh_rettdt( 0, 0 );
}

ZH_FUNC( ZH_HOUR )
{
   long lDate, lTime;

   if( zh_partdt( &lDate, &lTime, 1 ) )
   {
      int iHour, iMinutes, iSeconds, iMSec;

      zh_timeDecode( lTime, &iHour, &iMinutes, &iSeconds, &iMSec );
      zh_retnilen( iHour, 3 );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_MINUTE )
{
   long lDate, lTime;

   if( zh_partdt( &lDate, &lTime, 1 ) )
   {
      int iHour, iMinutes, iSeconds, iMSec;

      zh_timeDecode( lTime, &iHour, &iMinutes, &iSeconds, &iMSec );
      zh_retnilen( iMinutes, 3 );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_SEC )
{
   long lDate, lTime;

   if( zh_partdt( &lDate, &lTime, 1 ) )
   {
      int iHour, iMinutes, iSeconds, iMSec;

      zh_timeDecode( lTime, &iHour, &iMinutes, &iSeconds, &iMSec );
      zh_retndlen( ( double ) ( iSeconds * 1000 + iMSec ) / 1000, 3, 3 );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_TSTOSTR )
{
   long lDate, lTime;

   if( zh_partdt( &lDate, &lTime, 1 ) )
   {
      char szBuffer[ 24 ];

      zh_timeStampStr( szBuffer, lDate, lTime );
      if( zh_parl( 2 ) )
      {
         if( lTime == 0 )
         {
            if( lDate == 0 )
               zh_retc_const( "00:00" );
            else
               zh_retclen( szBuffer, 10 );
         }
         else
         {
            int i = 23;
            while( szBuffer[ i - 1 ] == '0' )
               --i;
            if( szBuffer[ i - 1 ] == '.' )
            {
               --i;
               if( szBuffer[ i - 1 ] == '0' && szBuffer[ i - 2 ] == '0' )
                  i -= 3;
            }
            if( lDate == 0 )
               zh_retclen( szBuffer + 11, i - 11 );
            else
               zh_retclen( szBuffer, i );
         }
      }
      else
         zh_retclen( szBuffer, 23 );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_STRTOTS )
{
   const char * szDateTime = zh_parc( 1 );

   if( szDateTime )
   {
      long lDate, lTime;

      zh_timeStampStrGetDT( szDateTime, &lDate, &lTime );
      zh_rettdt( lDate, lTime );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/* get week number and other parts ISO 8601 week date:
   zh_Week( <dDate>, [@<nYear>], [@<nDayOfWeek>] ) --> <nWeek> */
ZH_FUNC( ZH_WEEK )
{
   PZH_ITEM pDate = zh_param( 1, ZH_IT_DATETIME );

   if( pDate )
   {
      int iYear, iWeek, iDay;

      zh_dateDecWeek( zh_itemGetDL( pDate ), &iYear, &iWeek, &iDay );
      zh_storni( iYear, 2 );
      zh_storni( iDay, 3 );
      zh_retni( iWeek );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_UTCOFFSET )
{
   if( ZH_ISDATETIME( 1 ) )
   {
      int iYear, iMonth, iDay, iHour, iMinute, iSecond, iMSec;

      zh_timeStampUnpack( zh_partd( 1 ), &iYear, &iMonth, &iDay, &iHour, &iMinute, &iSecond, &iMSec );
      zh_retnl( zh_timeStampUTCOffset( iYear, iMonth, iDay, iHour, iMinute, iSecond ) );
   }
   else
      zh_retnl( zh_timeUTCOffset() );
}

ZH_FUNC( ZH_TSTOUTC )
{
   if( ZH_ISTIMESTAMP( 1 ) )
      zh_rettd( zh_timeLocalToUTC( zh_partd( 1 ) ) );
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}
