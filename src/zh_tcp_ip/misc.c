/*
 * TIP Class oriented Internet protocol library
 *
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
 * Copyright 1999-2017 Viktor Szakats (vszakats.net/ziher)
 *    (tip_TimeStamp() rework, cleanups)
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
#include "zh_error_api.h"
#include "zh_date.h"

/* Internet timestamp based on:
   https://tools.ietf.org/html/rfc822
   https://tools.ietf.org/html/rfc2822 */
ZH_FUNC( TIP_TIMESTAMP )
{
   static const char * s_days[]   = { "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" };
   static const char * s_months[] = { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };

   char szRet[ 32 ];
   int  iYear, iMonth, iDay, iHour, iMinute, iSecond, iMSec;
   long lOffset;

   if( ZH_ISDATE( 1 ) )
   {
      zh_dateDecode( zh_pardl( 1 ), &iYear, &iMonth, &iDay );

      /* For compatibility, Seconds() value */
      if( ZH_IS_PARAM_NUM( 2 ) )
         zh_timeDecode( ( long ) ( zh_parnd( 2 ) * 1000 ),
                        &iHour, &iMinute, &iSecond, &iMSec );
      else
         iHour = iMinute = iSecond = 0;
   }
   else if( ZH_ISDATETIME( 1 ) )
      zh_timeStampUnpack( zh_partd( 1 ), &iYear, &iMonth, &iDay, &iHour, &iMinute, &iSecond, &iMSec );
   else
      zh_timeStampGetLocal( &iYear, &iMonth, &iDay, &iHour, &iMinute, &iSecond, &iMSec );

   lOffset = zh_timeStampUTCOffset( iYear, iMonth, iDay, iHour, iMinute, iSecond );

   zh_snprintf( szRet, sizeof( szRet ), "%s, %02d %s %04d %02d:%02d:%02d %+03d%02d",
                s_days[ zh_dateDOW( iYear, iMonth, iDay ) - 1 ],
                iDay, s_months[ iMonth == 0 ? 0 : iMonth - 1 ], iYear,
                iHour, iMinute, iSecond,
                ( int ) ( lOffset / 3600 ),
                ( int ) ( ( lOffset % 3600 ) / 60 ) );

   zh_retc( szRet );
}

ZH_FUNC( TIP_HTMLSPECIALCHARS )
{
   if( ZH_ISCHAR( 1 ) )
   {
      ZH_I_SIZE nLen = zh_parclen( 1 );

      if( nLen )
      {
         const char * pszData = zh_parc( 1 );
         char *       pszRet;
         ZH_I_SIZE      nPos    = 0;
         ZH_I_SIZE      nPosRet = 0;

         while( nLen && ZH_ISSPACE( pszData[ nLen - 1 ] ) )
            nLen--;

         /* Giving maximum final length possible */
         pszRet = ( char * ) zh_xgrab( nLen * 6 + 1 );

         while( nPos < nLen )
         {
            ZH_BYTE cElem = ( ZH_BYTE ) pszData[ nPos ];

            if( cElem == '&' )
            {
               pszRet[ nPosRet++ ] = '&';
               pszRet[ nPosRet++ ] = 'a';
               pszRet[ nPosRet++ ] = 'm';
               pszRet[ nPosRet++ ] = 'p';
               pszRet[ nPosRet++ ] = ';';
            }
            else if( cElem == '<' )
            {
               pszRet[ nPosRet++ ] = '&';
               pszRet[ nPosRet++ ] = 'l';
               pszRet[ nPosRet++ ] = 't';
               pszRet[ nPosRet++ ] = ';';
            }
            else if( cElem == '>' )
            {
               pszRet[ nPosRet++ ] = '&';
               pszRet[ nPosRet++ ] = 'g';
               pszRet[ nPosRet++ ] = 't';
               pszRet[ nPosRet++ ] = ';';
            }
            else if( cElem == '"' )
            {
               pszRet[ nPosRet++ ] = '&';
               pszRet[ nPosRet++ ] = 'q';
               pszRet[ nPosRet++ ] = 'u';
               pszRet[ nPosRet++ ] = 'o';
               pszRet[ nPosRet++ ] = 't';
               pszRet[ nPosRet++ ] = ';';
            }
            else if( cElem == '\'' )
            {
               pszRet[ nPosRet++ ] = '&';
               pszRet[ nPosRet++ ] = '#';
               pszRet[ nPosRet++ ] = '0';
               pszRet[ nPosRet++ ] = '3';
               pszRet[ nPosRet++ ] = '9';
               pszRet[ nPosRet++ ] = ';';
            }
            else if( cElem == '\r' )
            {
               pszRet[ nPosRet++ ] = '&';
               pszRet[ nPosRet++ ] = '#';
               pszRet[ nPosRet++ ] = '0';
               pszRet[ nPosRet++ ] = '1';
               pszRet[ nPosRet++ ] = '3';
               pszRet[ nPosRet++ ] = ';';
            }
            else if( cElem == '\n' )
            {
               pszRet[ nPosRet++ ] = '&';
               pszRet[ nPosRet++ ] = '#';
               pszRet[ nPosRet++ ] = '0';
               pszRet[ nPosRet++ ] = '1';
               pszRet[ nPosRet++ ] = '0';
               pszRet[ nPosRet++ ] = ';';
            }
            else if( cElem >= ' ' )
            {
               pszRet[ nPosRet++ ] = cElem;
            }

            nPos++;
         }

         zh_retclen_buffer( ( char * ) zh_xrealloc( pszRet, nPosRet + 1 ), nPosRet );
      }
      else
         zh_retc_null();
   }
   else
      zh_errRT_BASE( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( TIP_CRLF )
{
   zh_retc_const( "\r\n" );
}

ZH_FUNC( TIP_JSONSPECIALCHARS )
{
   if( ZH_ISCHAR( 1 ) )
   {
      ZH_I_SIZE nLen = zh_parclen( 1 );

      if( nLen )
      {
         const char * pszData = zh_parc( 1 );
         char *       pszRet;
         ZH_I_SIZE      nPos    = 0;
         ZH_I_SIZE      nPosRet = 0;

         while( nLen && ZH_ISSPACE( pszData[ nLen - 1 ] ) )
            nLen--;

         /* Giving maximum final length possible */
         pszRet = ( char * ) zh_xgrab( nLen * 2 + 1 );

         while( nPos < nLen )
         {
            ZH_BYTE cElem = ( ZH_BYTE ) pszData[ nPos ];

            if( cElem == '"' )
            {
               pszRet[ nPosRet++ ] = '\\';
               pszRet[ nPosRet++ ] = '"';
            }
            else if( cElem == '\\' )
            {
               pszRet[ nPosRet++ ] = '\\';
               pszRet[ nPosRet++ ] = '\\';
            }
            else if( cElem == '/' )
            {
               pszRet[ nPosRet++ ] = '\\';
               pszRet[ nPosRet++ ] = '/';
            }
            else if( cElem == '\b' )
            {
               pszRet[ nPosRet++ ] = '\\';
               pszRet[ nPosRet++ ] = 'b';
            }
            else if( cElem == '\f' )
            {
               pszRet[ nPosRet++ ] = '\\';
               pszRet[ nPosRet++ ] = 'f';
            }
            else if( cElem == '\r' )
            {
               pszRet[ nPosRet++ ] = '\\';
               pszRet[ nPosRet++ ] = 'r';
            }
            else if( cElem == '\n' )
            {
               pszRet[ nPosRet++ ] = '\\';
               pszRet[ nPosRet++ ] = 'n';
            }
            else if( cElem == '\t' )
            {
               pszRet[ nPosRet++ ] = '\\';
               pszRet[ nPosRet++ ] = 't';
            }
            else if( cElem >= ' ' )
            {
               pszRet[ nPosRet++ ] = cElem;
            }

            nPos++;
         }

         zh_retclen_buffer( ( char * ) zh_xrealloc( pszRet, nPosRet + 1 ), nPosRet );
      }
      else
         zh_retc_null();
   }
   else
      zh_errRT_BASE( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}
