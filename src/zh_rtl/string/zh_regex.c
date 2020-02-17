/*
 * Regex functions
 *
 * Copyright 2007 Przemyslaw Czerpak
 * Copyright 2015 Viktor Szakats (PCRE2 support)
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
#include "zh_codepage_api.h"
#include "zh_item_api.h"
#include "zh_api_error.h"
#include "zh_init.h"
#if defined( ZH_HAS_PCRE2 )
#include "zh_vm.h"
#endif

#if defined( ZH_HAS_PCRE ) || defined( ZH_HAS_PCRE2 )
   static int s_iUTF8Enabled;
#endif

#if defined( ZH_HAS_PCRE2 )
   static pcre2_general_context * s_re_ctxg;
   static pcre2_compile_context * s_re_ctxc;
   static pcre2_match_context *   s_re_ctxm;
#endif

static void zh_regfree( PZH_REGEX pRegEx )
{
#if defined( ZH_HAS_PCRE2 )
   pcre2_code_free( pRegEx->re_pcre );
#elif defined( ZH_HAS_PCRE )
   ( pcre_free )( pRegEx->re_pcre );
#elif defined( ZH_POSIX_REGEX )
   regfree( &pRegEx->reg );
#else
   ZH_SYMBOL_UNUSED( pRegEx );
#endif
}

static int zh_regcomp( PZH_REGEX pRegEx, const char * szRegEx )
{
#if defined( ZH_HAS_PCRE2 )
   int iError = 0;
   PCRE2_SIZE iErrOffset = 0;
   ZH_U32 uiCFlags = ( ( pRegEx->iFlags & HBREG_ICASE   ) ? PCRE2_CASELESS  : 0 ) |
                     ( ( pRegEx->iFlags & HBREG_NEWLINE ) ? PCRE2_MULTILINE : 0 ) |
                     ( ( pRegEx->iFlags & HBREG_DOTALL  ) ? PCRE2_DOTALL    : 0 );

   pRegEx->iEFlags = ( ( pRegEx->iFlags & HBREG_NOTBOL ) ? PCRE2_NOTBOL : 0 ) |
                     ( ( pRegEx->iFlags & HBREG_NOTEOL ) ? PCRE2_NOTEOL : 0 );

   /* use UTF-8 in PCRE2 when available and HVM CP is also UTF-8. */
   if( s_iUTF8Enabled && zh_cdpIsUTF8( NULL ) )
      uiCFlags |= PCRE2_UTF;

   pRegEx->re_pcre = pcre2_compile( ( PCRE2_SPTR ) szRegEx,
                                    ( PCRE2_SIZE ) strlen( szRegEx ),
                                    uiCFlags,
                                    &iError,
                                    &iErrOffset, s_re_ctxc );
   return pRegEx->re_pcre ? 0 : -1;
#elif defined( ZH_HAS_PCRE )
   const unsigned char * pCharTable = NULL;
   const char * szError = NULL;
   int iErrOffset = 0;
   int iCFlags = ( ( pRegEx->iFlags & HBREG_ICASE   ) ? PCRE_CASELESS  : 0 ) |
                 ( ( pRegEx->iFlags & HBREG_NEWLINE ) ? PCRE_MULTILINE : 0 ) |
                 ( ( pRegEx->iFlags & HBREG_DOTALL  ) ? PCRE_DOTALL    : 0 );

   pRegEx->iEFlags = ( ( pRegEx->iFlags & HBREG_NOTBOL ) ? PCRE_NOTBOL : 0 ) |
                     ( ( pRegEx->iFlags & HBREG_NOTEOL ) ? PCRE_NOTEOL : 0 );

   /* use UTF-8 in PCRE1 when available and HVM CP is also UTF-8. */
   if( s_iUTF8Enabled && zh_cdpIsUTF8( NULL ) )
      iCFlags |= PCRE_UTF8;

   pRegEx->re_pcre = pcre_compile( szRegEx, iCFlags, &szError,
                                   &iErrOffset, pCharTable );
   return pRegEx->re_pcre ? 0 : -1;
#elif defined( ZH_POSIX_REGEX )
   int iCFlags = REG_EXTENDED |
                 ( ( pRegEx->iFlags & HBREG_ICASE   ) ? REG_ICASE   : 0 ) |
                 ( ( pRegEx->iFlags & HBREG_NEWLINE ) ? REG_NEWLINE : 0 ) |
                 ( ( pRegEx->iFlags & HBREG_NOSUB   ) ? REG_NOSUB   : 0 );
   pRegEx->iEFlags = ( ( pRegEx->iFlags & HBREG_NOTBOL ) ? REG_NOTBOL : 0 ) |
                     ( ( pRegEx->iFlags & HBREG_NOTEOL ) ? REG_NOTEOL : 0 );
   return regcomp( &pRegEx->reg, szRegEx, iCFlags );
#else
   ZH_SYMBOL_UNUSED( pRegEx );
   ZH_SYMBOL_UNUSED( szRegEx );
   return -1;
#endif
}

static int zh_regexec( PZH_REGEX pRegEx, const char * szString, ZH_SIZE nLen,
                       int iMatches, ZH_REGMATCH * aMatches )
{
#if defined( ZH_HAS_PCRE2 )
   PCRE2_SIZE iResult = pcre2_match( pRegEx->re_pcre,
                                     ( PCRE2_SPTR ) szString,
                                     ( PCRE2_SIZE ) nLen,
                                     ( PCRE2_SIZE ) 0 /* startoffset */,
                                     ( ZH_U32 ) pRegEx->iEFlags,
                                     aMatches, s_re_ctxm );
   if( iResult == 0 )
   {
      PCRE2_SIZE i;
      for( i = 0; i < ( PCRE2_SIZE ) iMatches; i++ )
      {
         if( ZH_REGMATCH_EO( aMatches, i ) != ZH_REGMATCH_UNSET )
            iResult = i + 1;
      }
   }
   return ( int ) iResult;
#elif defined( ZH_HAS_PCRE )
   int iResult = pcre_exec( pRegEx->re_pcre, NULL /* pcre_extra */,
                            szString, ( int ) nLen, 0 /* startoffset */,
                            pRegEx->iEFlags, aMatches, ZH_REGMATCH_SIZE( iMatches ) );
   if( iResult == 0 )
   {
      int i;
      for( i = 0; i < iMatches; i++ )
      {
         if( ZH_REGMATCH_EO( aMatches, i ) != ZH_REGMATCH_UNSET )
            iResult = i + 1;
      }
   }
   return iResult;
#elif defined( ZH_POSIX_REGEX )
   char * szBuffer = NULL;
   int iResult, i;

   if( szString[ nLen ] != 0 )
   {
      szBuffer = zh_strndup( szString, nLen );
      szString = szBuffer;
   }
   for( i = 0; i < iMatches; i++ )
      ZH_REGMATCH_EO( aMatches, i ) = ZH_REGMATCH_UNSET;
   iResult = regexec( &pRegEx->reg, szString, iMatches, aMatches, pRegEx->iEFlags );
   if( iResult == 0 )
   {
      for( i = 0; i < iMatches; i++ )
      {
         if( ZH_REGMATCH_EO( aMatches, i ) != ZH_REGMATCH_UNSET )
            iResult = i + 1;
      }
   }
   else
      iResult = -1;
   if( szBuffer )
      zh_xfree( szBuffer );
   return iResult;
#else
   ZH_SYMBOL_UNUSED( pRegEx );
   ZH_SYMBOL_UNUSED( szString );
   ZH_SYMBOL_UNUSED( nLen );
   ZH_SYMBOL_UNUSED( iMatches );
   ZH_SYMBOL_UNUSED( aMatches );
   return -1;
#endif
}


ZH_FUNC( ZH_REGEXCOMP )
{
   ZH_SIZE nLen = zh_parclen( 1 );

   if( nLen == 0 )
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
   else
   {
      int iFlags = HBREG_EXTENDED;
      PZH_REGEX pRegEx;

      if( ! zh_parldef( 2, ZH_TRUE ) )
         iFlags |= HBREG_ICASE;
      if( zh_parl( 3 ) )
         iFlags |= HBREG_NEWLINE;

      pRegEx = zh_regexCompile( zh_parc( 1 ), nLen, iFlags );
      if( pRegEx )
      {
         pRegEx->fFree = ZH_FALSE;
         zh_retptrGC( pRegEx );
      }
   }
}

ZH_FUNC( ZH_ISREGEX )
{
   zh_retl( zh_regexIs( zh_param( 1, ZH_IT_ANY ) ) );
}

ZH_FUNC( ZH_ATX )
{
   PZH_ITEM pString = zh_param( 2, ZH_IT_STRING );

   if( pString )
   {
      PZH_REGEX pRegEx = zh_regexGet( zh_param( 1, ZH_IT_ANY ),
                                      ! zh_parldef( 3, ZH_TRUE ) ? HBREG_ICASE : 0 );

      if( pRegEx )
      {
         ZH_SIZE nLen = zh_itemGetCLen( pString );
         ZH_SIZE nStart = zh_parns( 4 );
         ZH_SIZE nEnd = zh_parnsdef( 5, nLen );

         if( nLen && nStart <= nLen && nStart <= nEnd )
         {
#if defined( ZH_HAS_PCRE2 )
            ZH_REGMATCH * aMatches = pcre2_match_data_create( 1, NULL );

            if( aMatches )
            {
#else
            ZH_REGMATCH aMatches[ ZH_REGMATCH_SIZE( 1 ) ];
#endif
               const char * pszString = zh_itemGetCPtr( pString );

               if( nEnd < nLen )
                  nLen = nEnd;
               if( nStart )
               {
                  --nStart;
                  nLen -= nStart;
               }

               if( zh_regexec( pRegEx, pszString + nStart, nLen, 1, aMatches ) > 0 )
               {
                  nStart += ( ZH_SIZE ) ZH_REGMATCH_SO( aMatches, 0 ) + 1;
                  nLen = ( ZH_SIZE ) ZH_REGMATCH_EO( aMatches, 0 ) -
                         ( ZH_SIZE ) ZH_REGMATCH_SO( aMatches, 0 );
                  zh_retclen( pszString + nStart - 1, nLen );
               }
               else
                  nStart = nLen = 0;

#if defined( ZH_HAS_PCRE2 )
               pcre2_match_data_free( aMatches );
            }
            else
               nStart = nLen = 0;
#endif
         }
         else
            nStart = nLen = 0;

         zh_regexFree( pRegEx );

         zh_storns( nStart, 4 );
         zh_storns( nLen, 5 );
      }
      else
      {
         zh_storns( 0, 4 );
         zh_storns( 0, 5 );
      }
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3013, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

static ZH_BOOL zh_regex( int iRequest )
{
#if defined( ZH_HAS_PCRE2 )
   ZH_REGMATCH * aMatches;
#else
   ZH_REGMATCH aMatches[ ZH_REGMATCH_SIZE( REGEX_MAX_GROUPS ) ];
#endif
   PZH_ITEM pRetArray, pString;
   int iMatches, iMaxMatch;
   ZH_BOOL fResult = ZH_FALSE;
   PZH_REGEX pRegEx;
   const char * pszString;
   ZH_SIZE nLen;

   pString = zh_param( 2, ZH_IT_STRING );
   if( ! pString )
   {
      zh_errRT_BASE_SubstR( EG_ARG, 3014, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
      return ZH_FALSE;
   }
   pRegEx = zh_regexGet( zh_param( 1, ZH_IT_ANY ),
                         ( ! zh_parldef( 3, ZH_TRUE ) ? HBREG_ICASE : 0 ) |
                         ( zh_parl( 4 ) ? HBREG_NEWLINE : 0 ) );
   if( ! pRegEx )
      return ZH_FALSE;

#if defined( ZH_HAS_PCRE2 )
   aMatches = pcre2_match_data_create( REGEX_MAX_GROUPS, NULL );
   if( ! aMatches )
      return ZH_FALSE;
#endif

   pszString = zh_itemGetCPtr( pString );
   nLen      = zh_itemGetCLen( pString );
   iMaxMatch = iRequest == 0 || iRequest == 4 || iRequest == 5 ?
               REGEX_MAX_GROUPS : 1;
   iMatches = zh_regexec( pRegEx, pszString, nLen, iMaxMatch, aMatches );
   if( iMatches > 0 )
   {
      PZH_ITEM pMatch;
      int i;

      switch( iRequest )
      {
         case 0:
            pRetArray = zh_itemArrayNew( iMatches );
            for( i = 0; i < iMatches; i++ )
            {
               if( ZH_REGMATCH_EO( aMatches, i ) != ZH_REGMATCH_UNSET )
                  zh_arraySetCL( pRetArray, i + 1,
                                 pszString + ( ZH_SIZE ) ZH_REGMATCH_SO( aMatches, i ),
                                 ( ZH_SIZE ) ZH_REGMATCH_EO( aMatches, i ) -
                                 ( ZH_SIZE ) ZH_REGMATCH_SO( aMatches, i ) );
               else
                  zh_arraySetCL( pRetArray, i + 1, NULL, 0 );
            }
            zh_itemReturnRelease( pRetArray );
            fResult = ZH_TRUE;
            break;

         case 1: /* LIKE */
            fResult = ZH_REGMATCH_SO( aMatches, 0 ) == 0 &&
                      ( ZH_SIZE ) ZH_REGMATCH_EO( aMatches, 0 ) == nLen;
            break;

         case 2: /* HAS */
            fResult = ZH_TRUE;
            break;

         case 3: /* SPLIT */
            iMaxMatch = zh_parni( 5 );
            pRetArray = zh_itemArrayNew( 0 );
            pMatch = zh_itemNew( NULL );
            iMatches = 0;
            do
            {
               zh_itemPutCL( pMatch, pszString, ( ZH_SIZE ) ZH_REGMATCH_SO( aMatches, 0 ) );
               zh_arrayAddForward( pRetArray, pMatch );
               nLen -= ( ZH_SIZE ) ZH_REGMATCH_EO( aMatches, 0 );
               pszString += ( ZH_SIZE ) ZH_REGMATCH_EO( aMatches, 0 );
               iMatches++;
            }
            while( ZH_REGMATCH_EO( aMatches, 0 ) > 0 && nLen &&
                   ( iMaxMatch == 0 || iMatches < iMaxMatch ) &&
                   zh_regexec( pRegEx, pszString, nLen, 1, aMatches ) > 0 );

            /* last match must be done also in case that pszString is empty;
               this would mean an empty split field at the end of the string */
#if 0
            if( nLen )
#endif
            {
               zh_itemPutCL( pMatch, pszString, nLen );
               zh_arrayAddForward( pRetArray, pMatch );
            }
            zh_itemRelease( pMatch );

            zh_itemReturnRelease( pRetArray );
            fResult = ZH_TRUE;
            break;

         case 4: /* results AND positions */
            pRetArray = zh_itemArrayNew( iMatches );

            for( i = 0; i < iMatches; i++ )
            {
               ZH_SIZE nSO = ( ZH_SIZE ) ZH_REGMATCH_SO( aMatches, i ),
                       nEO = ( ZH_SIZE ) ZH_REGMATCH_EO( aMatches, i );
               pMatch = zh_arrayGetItemPtr( pRetArray, i + 1 );
               zh_arrayNew( pMatch, 3 );
               if( nEO != ( ZH_SIZE ) ZH_REGMATCH_UNSET )
               {
                  zh_arraySetCL( pMatch, 1, pszString + nSO, nEO - nSO );  /* matched string */
                  zh_arraySetNS( pMatch, 2, nSO + 1 );  /* begin of match */
                  zh_arraySetNS( pMatch, 3, nEO );  /* End of match */
               }
               else
               {
                  zh_arraySetCL( pMatch, 1, NULL, 0 );
                  zh_arraySetNS( pMatch, 2, 0 );
                  zh_arraySetNS( pMatch, 3, 0 );
               }
            }
            zh_itemReturnRelease( pRetArray );
            fResult = ZH_TRUE;
            break;

         case 5: /* _ALL_ results AND positions */
         {
            PZH_ITEM pAtxArray;
            int      iMax       = zh_parni( 5 );  /* Max number of matches I want, 0 = unlimited */
            int      iGetMatch  = zh_parni( 6 );  /* Gets if want only one single match or a sub-match */
            ZH_BOOL  fOnlyMatch = zh_parldef( 7, ZH_TRUE );  /* If ZH_TRUE, returns only matches and sub-matches, not positions */
            ZH_SIZE  nOffset    = 0;
            int      iCount     = 0;
            ZH_SIZE  nSO, nEO;

            /* Set new array */
            pRetArray = zh_itemArrayNew( 0 );
            do
            {
               /* If I want all matches */
               if( iGetMatch == 0 || /* Check boundaries */
                   ( iGetMatch < 0 || iGetMatch > iMatches ) )
               {
                  pAtxArray = zh_itemArrayNew( iMatches );
                  for( i = 0; i < iMatches; i++ )
                  {
                     nSO = ( ZH_SIZE ) ZH_REGMATCH_SO( aMatches, i );
                     nEO = ( ZH_SIZE ) ZH_REGMATCH_EO( aMatches, i );
                     pMatch = zh_arrayGetItemPtr( pAtxArray, i + 1 );
                     if( ! fOnlyMatch )
                     {
                        zh_arrayNew( pMatch, 3 );
                        if( nEO != ( ZH_SIZE ) ZH_REGMATCH_UNSET )
                        {
                           zh_arraySetCL( pMatch, 1, pszString + nSO, nEO - nSO );  /* matched string */
                           zh_arraySetNS( pMatch, 2, nOffset + nSO + 1 );  /* begin of match */
                           zh_arraySetNS( pMatch, 3, nOffset + nEO );  /* End of match */
                        }
                        else
                        {
                           zh_arraySetCL( pMatch, 1, NULL, 0 );
                           zh_arraySetNS( pMatch, 2, 0 );
                           zh_arraySetNS( pMatch, 3, 0 );
                        }
                     }
                     else
                     {
                        if( nEO != ( ZH_SIZE ) ZH_REGMATCH_UNSET )
                           zh_itemPutCL( pMatch, pszString + nSO, nEO - nSO );  /* matched string */
                        else
                           zh_itemPutC( pMatch, NULL );
                     }
                  }
                  zh_arrayAddForward( pRetArray, pAtxArray );
                  zh_itemRelease( pAtxArray );
               }
               else /* Here I get only single matches */
               {
                  i = iGetMatch - 1;
                  nSO = ( ZH_SIZE ) ZH_REGMATCH_SO( aMatches, i );
                  nEO = ( ZH_SIZE ) ZH_REGMATCH_EO( aMatches, i );
                  pMatch = zh_itemNew( NULL );
                  if( ! fOnlyMatch )
                  {
                     zh_arrayNew( pMatch, 3 );
                     if( nEO != ( ZH_SIZE ) ZH_REGMATCH_UNSET )
                     {
                        zh_arraySetCL( pMatch, 1, pszString + nSO, nEO - nSO );  /* matched string */
                        zh_arraySetNS( pMatch, 2, nOffset + nSO + 1 );  /* begin of match */
                        zh_arraySetNS( pMatch, 3, nOffset + nEO );  /* End of match */
                     }
                     else
                     {
                        zh_arraySetCL( pMatch, 1, NULL, 0 );
                        zh_arraySetNS( pMatch, 2, 0 );
                        zh_arraySetNS( pMatch, 3, 0 );
                     }
                  }
                  else
                  {
                     if( nEO != ( ZH_SIZE ) ZH_REGMATCH_UNSET )
                        zh_itemPutCL( pMatch, pszString + nSO, nEO - nSO );  /* matched string */
                     else
                        zh_itemPutC( pMatch, NULL );
                  }
                  zh_arrayAddForward( pRetArray, pMatch );
                  zh_itemRelease( pMatch );
               }

               nEO = ( ZH_SIZE ) ZH_REGMATCH_EO( aMatches, 0 );
               if( nEO == ( ZH_SIZE ) ZH_REGMATCH_UNSET )
                  break;
               nLen -= nEO;
               pszString += nEO;
               nOffset += nEO;
               iCount++;
            }
            while( nEO && nLen && ( iMax == 0 || iCount < iMax ) &&
                   ( iMatches = zh_regexec( pRegEx, pszString, nLen, iMaxMatch, aMatches ) ) > 0 );
            zh_itemReturnRelease( pRetArray );
            fResult = ZH_TRUE;
            break;
         }
      }
   }
   else if( iRequest == 3 )
   {
      pRetArray = zh_itemArrayNew( 1 );
      zh_arraySet( pRetArray, 1, pString );
      zh_itemReturnRelease( pRetArray );
      fResult = ZH_TRUE;
   }

#if defined( ZH_HAS_PCRE2 )
   pcre2_match_data_free( aMatches );
#endif

   zh_regexFree( pRegEx );
   return fResult;
}

/* Returns array of Match + Sub-Matches. */
ZH_FUNC( ZH_REGEX )
{
   if( ! zh_regex( 0 ) )
      zh_reta( 0 );
}

/* Returns just .T. if match found or .F. otherwise. */
/* NOTE: Deprecated compatibility function.
         Please use zh_regexLike() and zh_regexHas() instead. */


ZH_FUNC( ZH_REGEXLIKE )
{
   zh_retl( zh_regex( 1 ) );
}

ZH_FUNC( ZH_REGEXHAS )
{
   zh_retl( zh_regex( 2 ) );
}

/* Splits the string in an array of matched expressions */
ZH_FUNC( ZH_REGEXSPLIT )
{
   if( ! zh_regex( 3 ) )
      zh_reta( 0 );
}

/* Returns array of { Match, start, end }, { Sub-Matches, start, end } */
ZH_FUNC( ZH_REGEXATX )
{
   if( ! zh_regex( 4 ) )
      zh_reta( 0 );
}

/* 2005-12-16 - Francesco Saverio Giudice
   zh_regexAll( cRegex, cString, lCaseSensitive, lNewLine, nMaxMatches, nGetMatch, lOnlyMatch ) --> aAllRegexMatches

   This function return all matches from a Regex search.
   It is a mix from zh_regex() and zh_regexAtX()

   PARAMETERS:
    cRegex         - Regex pattern string or precompiled Regex
    cString        - The string you want to search
    lCaseSensitive - default = FALSE
    lNewLine       - default = FALSE
    nMaxMatches    - default = unlimited, this limit number of matches that have to return
    nGetMatch      - default = unlimited, this returns only one from Match + Sub-Matches
    lOnlyMatch     - default = TRUE, if TRUE returns Matches, otherwise it returns also start and end positions
 */

ZH_FUNC( ZH_REGEXALL )
{
   if( ! zh_regex( 5 ) )
      zh_reta( 0 );
}

#if defined( ZH_HAS_PCRE2 )
static void * zh_pcre2_grab( PCRE2_SIZE size, void * data )
{
   ZH_SYMBOL_UNUSED( data );
   return size > 0 ? zh_xgrab( size ) : NULL;
}
static void zh_pcre2_free( void * ptr, void * data )
{
   ZH_SYMBOL_UNUSED( data );
   if( ptr )
      zh_xfree( ptr );
}
static void zh_pcre2_exit( void * cargo )
{
   ZH_SYMBOL_UNUSED( cargo );

   pcre2_match_context_free( s_re_ctxm );
   pcre2_compile_context_free( s_re_ctxc );
   pcre2_general_context_free( s_re_ctxg );
}
#elif defined( ZH_HAS_PCRE )
static void * zh_pcre_grab( size_t size )
{
   return size > 0 ? zh_xgrab( size ) : NULL;
}
static void zh_pcre_free( void * ptr )
{
   if( ptr )
      zh_xfree( ptr );
}
#endif

ZH_CALL_ON_STARTUP_BEGIN( _zh_regex_init_ )
#if defined( ZH_HAS_PCRE2 )
   /* detect UTF-8 support. */
   if( pcre2_config( PCRE2_CONFIG_UNICODE, &s_iUTF8Enabled ) != 0 )
      s_iUTF8Enabled = 0;

   s_re_ctxg = pcre2_general_context_create( zh_pcre2_grab, zh_pcre2_free, NULL );
   s_re_ctxc = pcre2_compile_context_create( s_re_ctxg );
   s_re_ctxm = pcre2_match_context_create( s_re_ctxg );

   zh_vmAtExit( zh_pcre2_exit, NULL );
#elif defined( ZH_HAS_PCRE )
   /* detect UTF-8 support.
    * In BCC builds this code also forces linking newer PCRE versions
    * then the one included in BCC RTL.
    */
   if( pcre_config( PCRE_CONFIG_UTF8, &s_iUTF8Enabled ) != 0 )
      s_iUTF8Enabled = 0;

   pcre_malloc = zh_pcre_grab;
   pcre_free = zh_pcre_free;
   pcre_stack_malloc = zh_pcre_grab;
   pcre_stack_free = zh_pcre_free;
#endif
   zh_regexInit( zh_regfree, zh_regcomp, zh_regexec );
ZH_CALL_ON_STARTUP_END( _zh_regex_init_ )

#if defined( ZH_PRAGMA_STARTUP )
   #pragma startup _zh_regex_init_
#elif defined( ZH_DATASEG_STARTUP )
   #define ZH_DATASEG_BODY    ZH_DATASEG_FUNC( _zh_regex_init_ )
   #include "zh_ini_seg.h"
#endif
