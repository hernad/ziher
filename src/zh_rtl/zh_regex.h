/*
 * Regex header
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

#ifndef ZH_REGEX_H_
#define ZH_REGEX_H_

#include "zh_api.h"

#if defined( _ZH_REGEX_INTERNAL_ )

#if defined( ZH_HAS_PCRE2 )
#  include <pcre2.h>
#  undef ZH_POSIX_REGEX
#elif defined( ZH_HAS_PCRE )
#  include <pcre.h>
#  undef ZH_POSIX_REGEX
#elif defined( ZH_OS_UNIX )
#  include <sys/types.h>
#  include <regex.h>
#  define ZH_POSIX_REGEX
#else
#  error pcre component required, but not available
#endif

typedef struct
{
   ZH_BOOL     fFree;
   int         iFlags;
   int         iEFlags;
#if defined( ZH_HAS_PCRE2 )
   pcre2_code  * re_pcre;
#elif defined( ZH_HAS_PCRE )
   pcre        * re_pcre;
#elif defined( ZH_POSIX_REGEX )
   regex_t     reg;
#endif
} ZH_REGEX;
typedef ZH_REGEX * PZH_REGEX;

#if defined( ZH_HAS_PCRE2 )
   #define ZH_REGMATCH              pcre2_match_data
   #define ZH_REGMATCH_SIZE( n )    ( n )
   #define ZH_REGMATCH_SO( p, n )   pcre2_get_ovector_pointer( p )[ ( n ) * 2 ]
   #define ZH_REGMATCH_EO( p, n )   pcre2_get_ovector_pointer( p )[ ( n ) * 2 + 1 ]
   #define ZH_REGMATCH_UNSET        PCRE2_UNSET
#elif defined( ZH_HAS_PCRE )
   #define ZH_REGMATCH              int
   #define ZH_REGMATCH_SIZE( n )    ( ( n ) * 3 )
   #define ZH_REGMATCH_SO( p, n )   ( p )[ ( n ) * 2 ]
   #define ZH_REGMATCH_EO( p, n )   ( p )[ ( n ) * 2 + 1 ]
   #define ZH_REGMATCH_UNSET        ( -1 )
#elif defined( ZH_POSIX_REGEX )
   #define ZH_REGMATCH              regmatch_t
   #define ZH_REGMATCH_SIZE( n )    ( n )
   #define ZH_REGMATCH_SO( p, n )   ( p )[ n ].rm_so
   #define ZH_REGMATCH_EO( p, n )   ( p )[ n ].rm_eo
   #define ZH_REGMATCH_UNSET        ( -1 )
#else
   #define ZH_REGMATCH              int
   #define ZH_REGMATCH_SIZE( n )    ( ( n ) * 2 )
   #define ZH_REGMATCH_SO( p, n )   ( p )[ ( n ) * 2 ]
   #define ZH_REGMATCH_EO( p, n )   ( p )[ ( n ) * 2 + 1 ]
   #define ZH_REGMATCH_UNSET        ( -1 )
#endif

typedef void ( * ZH_REG_FREE )( PZH_REGEX );
typedef int  ( * ZH_REG_COMP )( PZH_REGEX, const char * );
typedef int  ( * ZH_REG_EXEC )( PZH_REGEX, const char *, ZH_SIZE, int, ZH_REGMATCH * );

extern void zh_regexInit( ZH_REG_FREE pFree, ZH_REG_COMP pComp, ZH_REG_EXEC pExec );
extern ZH_BOOL zh_regexIs( PZH_ITEM pItem );

#ifndef REG_EXTENDED
#define REG_EXTENDED  0x00
#endif
#ifndef REG_NOSUB
#define REG_NOSUB     0x00
#endif

#else

typedef void * PZH_REGEX;

#endif /* _ZH_REGEX_INTERNAL_ */

#define HBREG_ICASE     0x01
#define HBREG_NEWLINE   0x02
#define HBREG_NOTBOL    0x04
#define HBREG_NOTEOL    0x08
#define HBREG_EXTENDED  0x10
#define HBREG_NOSUB     0x20
#define HBREG_DOTALL    0x40

#ifndef REGEX_MAX_GROUPS
#define REGEX_MAX_GROUPS 16
#endif

ZH_EXTERN_BEGIN

extern ZH_EXPORT PZH_REGEX zh_regexCompile( const char * szRegEx, ZH_SIZE nLen, int iFlags );
extern ZH_EXPORT PZH_REGEX zh_regexGet( PZH_ITEM pRegExItm, int iFlags );
extern ZH_EXPORT void      zh_regexFree( PZH_REGEX pRegEx );
extern ZH_EXPORT ZH_BOOL   zh_regexMatch( PZH_REGEX pRegEx, const char * szString, ZH_SIZE nLen, ZH_BOOL fFull );

ZH_EXTERN_END

#endif /* ZH_REGEX_H_ */
