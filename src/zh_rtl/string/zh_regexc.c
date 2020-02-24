/*
 * Regex functions
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

#define _ZH_REGEX_INTERNAL_
#include "zh_api.h"
#include "zh_regex.h"
#include "zh_item_api.h"
#include "zh_error_api.h"

static void zh_regfree( PZH_REGEX pRegEx )
{
   ZH_SYMBOL_UNUSED( pRegEx );
}

static int zh_regcomp( PZH_REGEX pRegEx, const char * szRegEx )
{
   ZH_SYMBOL_UNUSED( pRegEx );
   ZH_SYMBOL_UNUSED( szRegEx );
   return -1;
}

static int zh_regexec( PZH_REGEX pRegEx, const char * szString, ZH_SIZE nLen,
                       int iMatches, ZH_REGMATCH * aMatches )
{
   ZH_SYMBOL_UNUSED( pRegEx );
   ZH_SYMBOL_UNUSED( szString );
   ZH_SYMBOL_UNUSED( nLen );
   ZH_SYMBOL_UNUSED( iMatches );
   ZH_SYMBOL_UNUSED( aMatches );
   return -1;
}

static ZH_REG_FREE s_reg_free = zh_regfree;
static ZH_REG_COMP s_reg_comp = zh_regcomp;
static ZH_REG_EXEC s_reg_exec = zh_regexec;

void zh_regexInit( ZH_REG_FREE pFree, ZH_REG_COMP pComp, ZH_REG_EXEC pExec )
{
   s_reg_free = pFree;
   s_reg_comp = pComp;
   s_reg_exec = pExec;
}

/* This releases regex when called from the garbage collector */
static ZH_GARBAGE_FUNC( zh_regexRelease )
{
   ( s_reg_free )( ( PZH_REGEX ) Cargo );
}

static const ZH_GC_FUNCS s_gcRegexFuncs =
{
   zh_regexRelease,
   zh_gcDummyMark
};

ZH_BOOL zh_regexIs( PZH_ITEM pItem )
{
   return zh_itemGetPtrGC( pItem, &s_gcRegexFuncs ) != NULL;
}

PZH_REGEX zh_regexCompile( const char * szRegEx, ZH_SIZE nLen, int iFlags )
{
   PZH_REGEX pRegEx;

   ZH_SYMBOL_UNUSED( nLen );

   pRegEx = ( PZH_REGEX ) zh_gcAllocate( sizeof( *pRegEx ), &s_gcRegexFuncs );
   memset( pRegEx, 0, sizeof( *pRegEx ) );
   pRegEx->fFree = ZH_TRUE;
   pRegEx->iFlags = iFlags;

   if( ( s_reg_comp )( pRegEx, szRegEx ) != 0 )
   {
      zh_gcFree( pRegEx );
      pRegEx = NULL;
   }

   return pRegEx;
}

PZH_REGEX zh_regexGet( PZH_ITEM pRegExItm, int iFlags )
{
   PZH_REGEX pRegEx = NULL;
   ZH_BOOL fArgError = ZH_TRUE;

   if( pRegExItm )
   {
      if( ZH_IS_POINTER( pRegExItm ) )
      {
         pRegEx = ( PZH_REGEX ) zh_itemGetPtrGC( pRegExItm, &s_gcRegexFuncs );
         if( pRegEx )
            fArgError = ZH_FALSE;
      }
      else if( ZH_IS_STRING( pRegExItm ) )
      {
         ZH_SIZE nLen = zh_itemGetCLen( pRegExItm );
         const char * szRegEx = zh_itemGetCPtr( pRegExItm );
         if( nLen > 0 )
         {
            fArgError = ZH_FALSE;
            pRegEx = zh_regexCompile( szRegEx, nLen, iFlags );
         }
      }
   }

   if( fArgError )
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, 1, pRegExItm );
   else if( ! pRegEx ) /* zh_regexCompile() failed */
      zh_errRT_BASE_SubstR( EG_ARG, 3015, NULL, ZH_ERR_FUNCNAME, 1, pRegExItm );

   return pRegEx;
}

void zh_regexFree( PZH_REGEX pRegEx )
{
   if( pRegEx && pRegEx->fFree )
   {
      ( s_reg_free )( pRegEx );
      zh_gcFree( pRegEx );
   }
}

ZH_BOOL zh_regexMatch( PZH_REGEX pRegEx, const char * szString, ZH_SIZE nLen, ZH_BOOL fFull )
{
#if defined( ZH_HAS_PCRE2 )
   ZH_REGMATCH * aMatches = pcre2_match_data_create( 1, NULL );
#else
   ZH_REGMATCH aMatches[ ZH_REGMATCH_SIZE( 1 ) ];
#endif
   ZH_BOOL fMatch;

   fMatch = ( s_reg_exec )( pRegEx, szString, nLen, 1, aMatches ) > 0;
   fMatch = fMatch && ( ! fFull ||
            ( ZH_REGMATCH_SO( aMatches, 0 ) == 0 &&
              ( ZH_SIZE ) ZH_REGMATCH_EO( aMatches, 0 ) == nLen ) );

#if defined( ZH_HAS_PCRE2 )
   pcre2_match_data_free( aMatches );
#endif

   return fMatch;
}
