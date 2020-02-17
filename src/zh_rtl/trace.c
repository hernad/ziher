/*
 * The Ziher tracing API
 *
 * Copyright 2009 Viktor Szakats
 * Copyright 1999 Gonzalo A. Diethelm <gonzalo.diethelm@iname.com>
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
#include "zh_trace.h"

static int s_traceLogLevel = ZH_TR_DEFAULT;

static void zh_trace_message( char * buffer, ZH_SIZE nSize, int iParam, int iCount )
{
   int iFirst = iParam;

   buffer[ 0 ] = '\0';

   while( iParam <= iCount && nSize > 1 )
   {
      char * pszString;
      ZH_SIZE nLen;
      ZH_BOOL fFree;

      if( iParam > iFirst )
      {
         *buffer++ = ' ';
         --nSize;
      }
      pszString = zh_itemString( zh_param( iParam, ZH_IT_ANY ), &nLen, &fFree );
      zh_strncpy( buffer, pszString, nSize );
      nLen = strlen( buffer );
      nSize -= nLen;
      buffer += nLen;
      if( fFree )
         zh_xfree( pszString );
      iParam++;
   }
}

ZH_FUNC( ZH_TRACESTATE )
{
   zh_retl( zh_tracestate( ZH_ISLOG( 1 ) ? zh_parl( 1 ) :
                                           zh_parnidef( 1, -1 ) ) );
}

ZH_FUNC( ZH_TRACESYSOUT )
{
   zh_retl( zh_tracesysout( ZH_ISLOG( 1 ) ? zh_parl( 1 ) :
                                            zh_parnidef( 1, -1 ) ) );
}

ZH_FUNC( ZH_TRACEFLUSH )
{
   zh_retl( zh_traceflush( ZH_ISLOG( 1 ) ? zh_parl( 1 ) :
                                           zh_parnidef( 1, -1 ) ) );
}

ZH_FUNC( ZH_TRACEMODE )
{
   zh_retc( zh_tracemode( zh_parc( 1 ) ) );
}

ZH_FUNC( ZH_TRACEFILE )
{
   zh_retl( zh_tracefile( zh_parc( 1 ) ) );
}

ZH_FUNC( ZH_TRACELEVEL )
{
   zh_retni( zh_tracelevel( zh_parnidef( 1, -1 ) ) );
}

ZH_FUNC( ZH_TRACELOGLEVEL )
{
   int iOldLevel = s_traceLogLevel;

   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      int iLevel = zh_parni( 1 );
      if( iLevel >= ZH_TR_ALWAYS && iLevel < ZH_TR_LAST )
         s_traceLogLevel = iLevel;
   }
   zh_retni( iOldLevel );
}

ZH_FUNC( ZH_TRACELOG )
{
   char message[ 1024 ];
   char procname[ ZH_SYMBOL_NAME_LEN + ZH_SYMBOL_NAME_LEN + 5 ];
   char file[ ZH_PATH_MAX ];
   ZH_USHORT line;

   zh_trace_message( message, sizeof( message ) - 1, 1, zh_pcount() );
   zh_procinfo( 1, procname, &line, file );
   zh_tracelog( s_traceLogLevel, file, line, procname, "%s", message );
}

ZH_FUNC( ZH_TRACELOGAT )
{
   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      int iLevel = zh_parni( 1 );

      if( iLevel <= zh_tr_level() )
      {
         char message[ 1024 ];
         char procname[ ZH_SYMBOL_NAME_LEN + ZH_SYMBOL_NAME_LEN + 5 ];
         char file[ ZH_PATH_MAX ];
         ZH_USHORT line;

         zh_trace_message( message, sizeof( message ) - 1, 2, zh_pcount() );
         zh_procinfo( 1, procname, &line, file );
         zh_tracelog( iLevel, file, line, procname, "%s", message );
      }
   }
}

ZH_FUNC( ZH_TRACESTRING )
{
   int iPCount = zh_pcount();

   if( iPCount > 0 )
   {
      char message[ 1024 ];

      zh_trace_message( message, sizeof( message ) - 1, 1, iPCount );

      zh_traceset( ZH_TR_ALWAYS, "", 0, NULL );
      zh_tr_trace( "%s", message );
   }
}
