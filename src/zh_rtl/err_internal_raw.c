/*
 * The Error API (internal error)
 *
 * Copyright 1999-2004 Viktor Szakats
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
#include "zh_lang_api.h"
#include "zh_date.h"
#include "zh_set.h"
#include "zh_stack.h"

/* NOTE: Use as minimal calls from here, as possible.
         Don't allocate memory from this function. [vszakats] */

void zh_errInternalRaw( ZH_ERRCODE errCode, const char * szText, const char * szPar1, const char * szPar2 )
{
   char buffer[ 8192 ];
   char file[ ZH_PATH_MAX ];
   const char * szFile;
   ZH_BOOL fStack, fLang;
   ZH_USHORT uiLine;
   FILE * hLog;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_errInternal(%d, %s, %s, %s)", errCode, szText, szPar1, szPar2 ) );

   if( szPar1 == NULL )
      szPar1 = "";

   if( szPar2 == NULL )
      szPar2 = "";

   fStack = zh_stackId() != NULL;
   fLang = fStack && zh_langID() != NULL;

   szFile = fStack ? zh_setGetCPtr( ZH_SET_HBOUTLOG ) : NULL;
   if( ! szFile )
      szFile = "zh_out.log";

   hLog = zh_fopen( szFile, "a+" );
   if( hLog )
   {
      const char * szInfo;

      char szTime[ 9 ];
      int  iYear, iMonth, iDay;

      zh_dateToday( &iYear, &iMonth, &iDay );
      zh_dateTimeStr( szTime );

      fprintf( hLog, "Application Internal Error - %s\n", zh_cmdargARGVN( 0 ) );
      fprintf( hLog, "Terminated at: %04d-%02d-%02d %s\n", iYear, iMonth, iDay, szTime );
      szInfo = fStack ? zh_setGetCPtr( ZH_SET_ZHOUTLOGINFO ) : NULL;
      if( szInfo && *szInfo )
         fprintf( hLog, "Info: %s\n", szInfo );
   }

   zh_conOutErr( zh_conNewLine(), 0 );
   if( fLang )
      zh_snprintf( buffer, sizeof( buffer ), zh_langDGetItem( ZH_LANG_ITEM_BASE_ERRINTR ), errCode );
   else
      zh_snprintf( buffer, sizeof( buffer ), "Unrecoverable error %d: ", errCode );

   zh_conOutErr( buffer, 0 );
   if( hLog )
      fprintf( hLog, "%s", buffer );

   if( ! szText && fLang )
      szText = zh_langDGetItem( ZH_LANG_ITEM_BASE_ERRINTR + errCode - 9000 );

   if( szText )
      zh_snprintf( buffer, sizeof( buffer ), szText, szPar1, szPar2 );
   else
      buffer[ 0 ] = '\0';

   zh_conOutErr( buffer, 0 );
   zh_conOutErr( zh_conNewLine(), 0 );
   if( hLog )
      fprintf( hLog, "%s\n", buffer );

   if( fStack && zh_stackTotalItems() )
   {
      int iLevel = 0;
      while( zh_procinfo( iLevel++, buffer, &uiLine, file ) )
      {
         char msg[ ZH_SYMBOL_NAME_LEN + ZH_SYMBOL_NAME_LEN + 32 ];

         zh_snprintf( msg, sizeof( msg ), "Called from %s(%hu)%s%s\n", buffer, uiLine, *file ? " in " : "", file );

         zh_conOutErr( msg, 0 );
         if( hLog )
            fprintf( hLog, "%s", msg );
      }
   }

   if( hLog )
   {
      fprintf( hLog, "------------------------------------------------------------------------\n" );
      fclose( hLog );
   }
}
