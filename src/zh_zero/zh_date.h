/*
 * Header file for the Date API
 *
 * Copyright 1999-2001 Viktor Szakats
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

#ifndef ZH_DATE_H_
#define ZH_DATE_H_

#include "zh_defs.h"

ZH_EXTERN_BEGIN

extern ZH_EXPORT double zh_secondsCPU( int n );

extern ZH_EXPORT double zh_dateSeconds( void );
/* return UTC Julian timestamp in milliseconds */
extern ZH_EXPORT ZH_MAXUINT zh_dateMilliSeconds( void );

/* functions to operate on Julian date values */

extern ZH_EXPORT void   zh_dateTimeStr( char * pszTime );
extern ZH_EXPORT void   zh_dateToday( int * piYear, int * piMonth, int * piDay );

extern ZH_EXPORT long   zh_dateEncode( int iYear, int iMonth, int iDay );
extern ZH_EXPORT void   zh_dateDecode( long julian, int * piYear, int * piMonth, int * piDay );
extern ZH_EXPORT void   zh_dateStrPut( char * szDate, int iYear, int iMonth, int iDay );
extern ZH_EXPORT void   zh_dateStrGet( const char * szDate, int * piYear, int * piMonth, int * piDay );
extern ZH_EXPORT char * zh_dateDecStr( char * szDate, long lJulian );
extern ZH_EXPORT long   zh_dateEncStr( const char * szDate );
extern ZH_EXPORT int    zh_dateDOW( int iYear, int iMonth, int iDay );
extern ZH_EXPORT int    zh_dateJulianDOW( long lJulian );
extern ZH_EXPORT ZH_BOOL zh_dateDecWeek( long lJulian, int * piYear, int * piWeek, int * piDay );
extern ZH_EXPORT long   zh_dateEncWeek( int iYear, int iWeek, int iDay );

/* RTL functions */
extern ZH_EXPORT const char * zh_dateCMonth( int iMonth );
extern ZH_EXPORT const char * zh_dateCDOW( int iDay );
extern ZH_EXPORT char *       zh_dateFormat( const char * szDate, char * szFormattedDate, const char * szDateFormat );
extern ZH_EXPORT long         zh_dateUnformat( const char * szDate, const char * szDateFormat );

/* functions to operate on time values */

extern ZH_EXPORT long   zh_timeEncode( int iHour, int iMinutes, int iSeconds, int iMSec );
extern ZH_EXPORT void   zh_timeDecode( long lMilliSec, int * piHour, int * piMinutes,
                                       int * piSeconds, int * piMSec );
extern ZH_EXPORT char * zh_timeStr( char * szTime, long lMilliSec );
extern ZH_EXPORT ZH_BOOL zh_timeStrGet( const char * szTime,
                                        int * piHour, int * piMinutes,
                                        int * piSeconds, int * piMSec );

extern ZH_EXPORT void   zh_timeStrRawGet( const char * szTime,
                                          int * piHour, int * piMinutes,
                                          int * piSeconds, int * piMSec );

/* functions to operate on date and time values */

extern ZH_EXPORT void zh_timeStampGetLocal( int * piYear, int * piMonth, int * piDay,
                                            int * piHour, int * piMinutes,
                                            int * piSeconds, int * piMSec );
extern ZH_EXPORT void   zh_timeStampGet( long * plJulian, long * plMilliSec );

extern ZH_EXPORT long   zh_timeUTCOffset( void ); /* in seconds */
extern ZH_EXPORT double zh_timeLocalToUTC( double dTimeStamp );
extern ZH_EXPORT long zh_timeStampUTCOffset( int iYear, int iMonth, int iDay,
                                             int iHour, int iMinutes, int iSeconds );

extern ZH_EXPORT char * zh_timeStampStrRawPut( char * szDateTime, long lJulian, long lMilliSec );
extern ZH_EXPORT void   zh_timeStampStrRawGet( const char * szDateTime, long * plJulian, long * plMilliSec );

extern ZH_EXPORT char * zh_timeStampStr( char * szDateTime, long lJulian, long lMilliSec );
extern ZH_EXPORT ZH_BOOL zh_timeStampStrGet( const char * szDateTime,
                                            int * piYear, int * piMonth, int * piDay,
                                            int * piHour, int * piMinutes, int * piSeconds,
                                            int * piMSec );
extern ZH_EXPORT ZH_BOOL zh_timeStampStrGetDT( const char * szDateTime,
                                              long * plJulian, long * plMilliSec );

extern ZH_EXPORT double zh_timeStampPackDT( long lJulian, long lMilliSec );
extern ZH_EXPORT void   zh_timeStampUnpackDT( double dTimeStamp,
                                              long * plJulian, long * plMilliSec );
extern ZH_EXPORT double zh_timeStampPack( int iYear, int iMonth, int iDay,
                                          int iHour, int iMinutes, int iSeconds, int iMSec );
extern ZH_EXPORT void   zh_timeStampUnpack( double dTimeStamp,
                                            int * piYear, int * piMonth, int * piDay,
                                            int * piHour, int * piMinutes, int * piSeconds,
                                            int * piMSec );
extern ZH_EXPORT double zh_timeStampPackD( int iYear, int iMonth, int iDay,
                                           int iHour, int iMinutes, double dSeconds );
extern ZH_EXPORT void   zh_timeStampUnpackD( double dTimeStamp,
                                             int * piYear, int * piMonth, int * piDay,
                                             int * piHour, int * piMinutes, double * pdSeconds );

/* RTL functions */
extern ZH_EXPORT char * zh_timeFormat( char * szBuffer, const char * szTimeFormat, long lMilliSec );
extern ZH_EXPORT char * zh_timeStampFormat( char * szBuffer,
                                            const char * szDateFormat, const char * szTimeFormat,
                                            long lJulian, long lMilliSec );

extern ZH_EXPORT long   zh_timeUnformat( const char * szTime, const char * szTimeFormat );
extern ZH_EXPORT void   zh_timeStampUnformat( const char * szDateTime,
                                              const char * szDateFormat, const char * szTimeFormat,
                                              long * plJulian, long * plMilliSec );

extern ZH_EXPORT ZH_MAXUINT zh_timerGet( void );
extern ZH_EXPORT ZH_MAXUINT zh_timerInit( ZH_MAXINT nTimeOut );
extern ZH_EXPORT ZH_MAXINT  zh_timerTest( ZH_MAXINT nTimeOut, ZH_MAXUINT * pnTimer );

ZH_EXTERN_END

#define ZH_MINUTES_PER_DAY    ( 24 * 60 )
#define ZH_SECONDS_PER_DAY    ( ZH_MINUTES_PER_DAY * 60 )
#define ZH_MILLISECS_PER_DAY  ( ZH_SECONDS_PER_DAY * 1000 )

#define ZH_TIMEDIFF_DEC       6     /* default number of decimal places in numeric timestamp diff values */

#if ! defined( ZH_HAS_LOCALTIME_R )
#  if ( defined( _POSIX_C_SOURCE ) || defined( _XOPEN_SOURCE ) || \
        defined( _BSD_SOURCE ) || defined( _SVID_SOURCE ) || \
        defined( ZH_OS_SUNOS ) || defined( ZH_OS_BEOS ) || \
        defined( ZH_OS_ANDROID ) ) && \
      ! defined( ZH_OS_DARWIN_5 )
#     define ZH_HAS_LOCALTIME_R
#  elif defined( __WATCOMC__ )
#     if defined( __STDC_WANT_LIB_EXT1__ ) && __STDC_WANT_LIB_EXT1__ == 1
#        define ZH_HAS_LOCALTIME_R
#        define localtime_r   localtime_s
#        define gmtime_r      gmtime_s
#     elif ! defined( NO_EXT_KEYS )
#        define ZH_HAS_LOCALTIME_R
#        define localtime_r   _localtime
#        define gmtime_r      _gmtime
#     endif
#  endif
#endif

#endif /* ZH_DATE_H_ */
