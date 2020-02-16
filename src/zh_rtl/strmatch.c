/*
 * String matching functions
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

#include "zh_api.h"
#include "zh_regex.h"

ZH_BOOL zh_strMatchRegExp( const char * szString, const char * szPattern )
{
   PZH_REGEX pRegEx;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_strMatchRegExp(%s, %s)", szString, szPattern ) );

   pRegEx = zh_regexCompile( szPattern, strlen( szPattern ), HBREG_EXTENDED );
   if( pRegEx )
   {
      ZH_BOOL fMatch;
      fMatch = zh_regexMatch( pRegEx, szString, strlen( szString ), ZH_TRUE );
      zh_regexFree( pRegEx );
      return fMatch;
   }
   else
      return zh_strMatchWildExact( szString, szPattern );
}

/*
 * WildMatch( cPattern, cValue [, lExact] ) compares
 * cValue with cPattern, cPattern * may contain wildcard characters (?*)
 * When lExact is TRUE then it will check if whole cValue is covered by
 * cPattern else it will check if cPattern is a prefix of cValue
 */

/* NOTE: This function is compatible with sx_WildMatch(), except when
         the pattern is an empty string where zh_WildMatch() returns
         .T., while sx_WildMatch() returns .F. [vszakats] */

ZH_FUNC( ZH_WILDMATCH )
{
   const char * szPattern = zh_parc( 1 ),
              * szText = zh_parc( 2 );

   zh_retl( szText && szPattern &&
            ( zh_parl( 3 ) ? zh_strMatchWildExact( szText, szPattern ) :
                             zh_strMatchWild( szText, szPattern ) ) );
}

ZH_FUNC( ZH_WILDMATCHI )
{
   const char * szPattern = zh_parc( 1 ),
              * szText = zh_parc( 2 );

   zh_retl( szText && szPattern &&
            zh_strMatchCaseWildExact( szText, szPattern ) );
}

ZH_FUNC( ZH_FILEMATCH )
{
   const char * szText = zh_parc( 1 ),
              * szPattern = zh_parc( 2 );

   zh_retl( szText && szPattern &&
            zh_strMatchFile( szText, szPattern ) );
}
