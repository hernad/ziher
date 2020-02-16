/*
 * zh_token*() functions
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

#include "zh_api.h"
#include "zh_item_api.h"
#include "zh_api_error.h"

#define _ZH_TOK_QUOTE_MASK       0x07
#define _ZH_TOK_RESPECT_DQUOTE   0x01
#define _ZH_TOK_RESPECT_SQUOTE   0x02
#define _ZH_TOK_RESPECT_BQUOTE   0x04
#define _ZH_TOK_ISDELIM          0x08
#define _ZH_TOK_EOL_DELIM        0x10
#define _ZH_TOK_STRIP_QUOTE      0x20

static ZH_SIZE zh_tokenCount( const char * szLine, ZH_SIZE nLen,
                              const char * szDelim, ZH_SIZE nDelim,
                              int iFlags )
{
   ZH_SIZE nPos = 0, nTokens = 1;
   char cQuote = 0;

   while( nPos < nLen )
   {
      char ch = szLine[ nPos ];

      if( cQuote )
      {
         if( ch == cQuote )
            cQuote = 0;
      }
      else if( ( iFlags & _ZH_TOK_QUOTE_MASK ) != 0 &&
               ( ( ch == '"' && ( iFlags & _ZH_TOK_RESPECT_DQUOTE ) ) ||
                 ( ch == '\'' && ( iFlags & _ZH_TOK_RESPECT_SQUOTE ) ) ||
                 ( ch == '`' && ( iFlags & _ZH_TOK_RESPECT_BQUOTE ) ) ) )
         cQuote = ch;
      else if( ( iFlags & _ZH_TOK_EOL_DELIM ) != 0 &&
               ( ch == '\n' || ch == '\r' ) )
      {
         ++nTokens;
         if( nPos + 1 < nLen && szLine[ nPos + 1 ] == ( ch == '\n' ? '\r' : '\n' ) )
            ++nPos;
      }
      else if( nDelim && ch == szDelim[ 0 ] &&
               ( nDelim == 1 || ! memcmp( szLine + nPos, szDelim, nDelim ) ) )
      {
         ++nTokens;
         if( ( iFlags & _ZH_TOK_ISDELIM ) == 0 )
         {
            while( nPos + 1 < nLen && szLine[ nPos + 1 ] == szDelim[ 0 ] )
               ++nPos;
         }
         nPos += nDelim - 1;
      }
      ++nPos;
   }

   return nTokens;
}

static const char * zh_tokenGet( const char * szLine, ZH_SIZE nLen,
                                 const char * szDelim, ZH_SIZE * pnDelim,
                                 int iFlags, ZH_SIZE nToken, ZH_SIZE * pnLen )
{
   ZH_SIZE nPos, nStart, nDelim = *pnDelim;
   char cQuote = 0;

   for( nPos = nStart = 0; nPos < nLen; ++nPos )
   {
      char ch = szLine[ nPos ];

      if( cQuote )
      {
         if( ch == cQuote )
            cQuote = 0;
      }
      else if( ( iFlags & _ZH_TOK_QUOTE_MASK ) != 0 &&
               ( ( ch == '"' && ( iFlags & _ZH_TOK_RESPECT_DQUOTE ) ) ||
                 ( ch == '\'' && ( iFlags & _ZH_TOK_RESPECT_SQUOTE ) ) ||
                 ( ch == '`' && ( iFlags & _ZH_TOK_RESPECT_BQUOTE ) ) ) )
         cQuote = ch;
      else if( ( iFlags & _ZH_TOK_EOL_DELIM ) != 0 &&
               ( ch == '\n' || ch == '\r' ) )
      {
         ZH_SIZE nL = ( nPos + 1 < nLen &&
                        szLine[ nPos + 1 ] == ( ch == '\n' ? '\r' : '\n' ) ) ? 1 : 0;
         if( --nToken == 0 )
         {
            *pnDelim = nL + 1;
            *pnLen = nPos - nStart;
            return szLine + nStart;
         }
         nPos += nL;
         nStart = nPos + 1;
      }
      else if( nDelim && ch == szDelim[ 0 ] &&
               ( nDelim == 1 || ! memcmp( szLine + nPos, szDelim, nDelim ) ) )
      {
         if( --nToken == 0 )
         {
            *pnLen = nPos - nStart;
            return szLine + nStart;
         }
         if( ( iFlags & _ZH_TOK_ISDELIM ) == 0 )
         {
            while( nPos + 1 < nLen && szLine[ nPos + 1 ] == szDelim[ 0 ] )
               ++nPos;
         }
         nPos += nDelim - 1;
         nStart = nPos + 1;
      }
   }
   if( --nToken == 0 )
   {
      *pnLen = nPos - nStart;
      return szLine + nStart;
   }
   *pnLen = 0;
   return NULL;
}

static PZH_ITEM zh_tokenArray( const char * szLine, ZH_SIZE nLen,
                               const char * szDelim, ZH_SIZE nDelim,
                               int iFlags )
{
   ZH_SIZE nTokens = zh_tokenCount( szLine, nLen, szDelim, nDelim, iFlags );
   PZH_ITEM pArray = zh_itemArrayNew( nTokens );

   if( nTokens )
   {
      ZH_SIZE nPos, nStart, nToken;
      char cQuote = 0;

      for( nPos = nStart = nToken = 0; nPos < nLen; ++nPos )
      {
         char ch = szLine[ nPos ];

         if( cQuote )
         {
            if( ch == cQuote )
               cQuote = 0;
         }
         else if( ( iFlags & _ZH_TOK_QUOTE_MASK ) != 0 &&
                  ( ( ch == '"' && ( iFlags & _ZH_TOK_RESPECT_DQUOTE ) ) ||
                    ( ch == '\'' && ( iFlags & _ZH_TOK_RESPECT_SQUOTE ) ) ||
                    ( ch == '`' && ( iFlags & _ZH_TOK_RESPECT_BQUOTE ) ) ) )
            cQuote = ch;
         else if( ( iFlags & _ZH_TOK_EOL_DELIM ) != 0 &&
                  ( ch == '\n' || ch == '\r' ) )
         {
            zh_arraySetCL( pArray, ++nToken, szLine + nStart, nPos - nStart );
            if( nPos + 1 < nLen && szLine[ nPos + 1 ] == ( ch == '\n' ? '\r' : '\n' ) )
               ++nPos;
            nStart = nPos + 1;
         }
         else if( nDelim && ch == szDelim[ 0 ] &&
                  ( nDelim == 1 || ! memcmp( szLine + nPos, szDelim, nDelim ) ) )
         {
            zh_arraySetCL( pArray, ++nToken, szLine + nStart, nPos - nStart );
            if( ( iFlags & _ZH_TOK_ISDELIM ) == 0 )
            {
               while( nPos + 1 < nLen && szLine[ nPos + 1 ] == szDelim[ 0 ] )
                  ++nPos;
            }
            nPos += nDelim - 1;
            nStart = nPos + 1;
         }
      }
      zh_arraySetCL( pArray, ++nToken, szLine + nStart, nPos - nStart );
   }

   return pArray;
}

static ZH_BOOL zh_tokenParam( int iParam, ZH_SIZE nSkip,
                              const char ** pszLine, ZH_SIZE * pnLen,
                              const char ** pszDelim, ZH_SIZE * pnDelim,
                              int * piFlags )
{
   const char * szLine = zh_parc( 1 ), * szDelim = NULL;
   ZH_SIZE nLen = zh_parclen( 1 ), nDelim = 0;
   int iFlags = 0;

   if( nLen )
   {
      if( nSkip )
      {
         szLine += nSkip;
         if( nLen <= nSkip )
            nLen = 0;
         else
            nLen -= nSkip;
      }

      nDelim = zh_parclen( iParam );
      if( nDelim )
      {
         szDelim = zh_parc( iParam );
         iFlags |= _ZH_TOK_ISDELIM;
      }
      else if( zh_parl( iParam ) )
      {
         iFlags |= _ZH_TOK_EOL_DELIM;
      }
      else
      {
         szDelim = " ";
         nDelim = 1;
      }

      if( nDelim && ( iFlags & _ZH_TOK_ISDELIM ) == 0 )
      {
         while( nLen && *szLine == szDelim[ 0 ] )
         {
            ++szLine;
            --nLen;
         }
         while( nLen && szLine[ nLen - 1 ] == szDelim[ 0 ] )
            --nLen;
      }
      if( zh_parl( iParam + 1 ) )
      {
         iFlags |= _ZH_TOK_RESPECT_DQUOTE | _ZH_TOK_RESPECT_SQUOTE;
         if( zh_parl( iParam + 2 ) )
            iFlags &= ~_ZH_TOK_RESPECT_SQUOTE;
      }
      else
         iFlags |= zh_parni( iParam + 1 );
   }

   *pnLen = nLen;
   *pnDelim = nDelim;
   *pszLine = szLine;
   *pszDelim = szDelim;
   *piFlags = iFlags;

   return szLine != NULL;
}

ZH_FUNC( ZH_TOKENCOUNT )
{
   const char * szLine, * szDelim;
   ZH_SIZE nLen, nDelim;
   int iFlags;

   if( zh_tokenParam( 2, 0, &szLine, &nLen, &szDelim, &nDelim, &iFlags ) )
      zh_retns( zh_tokenCount( szLine, nLen, szDelim, nDelim, iFlags ) );
   else
      zh_retns( 0 );
}

ZH_FUNC( ZH_TOKENGET )
{
   const char * szLine, * szDelim;
   ZH_SIZE nLen, nDelim;
   int iFlags;

   if( zh_tokenParam( 3, 0, &szLine, &nLen, &szDelim, &nDelim, &iFlags ) )
   {
      szLine = zh_tokenGet( szLine, nLen, szDelim, &nDelim, iFlags,
                            zh_parns( 2 ), &nLen );
      zh_retclen( szLine, nLen );
   }
   else
      zh_retc_null();
}

/* like zh_tokenGet() but returns next token starting from passed position
 * (0 based) inside string, e.g.:
 *    zh_tokenPtr( cString, @nTokPos, Chr( 9 ) ) --> cToken
 */
ZH_FUNC( ZH_TOKENPTR )
{
   const char * szLine, * szDelim;
   ZH_SIZE nLen, nDelim;
   int iFlags;

   if( zh_tokenParam( 3, zh_parns( 2 ), &szLine, &nLen, &szDelim, &nDelim, &iFlags ) )
   {
      const char * szToken;
      ZH_SIZE nSkip, nToken;

      szToken = zh_tokenGet( szLine, nLen, szDelim, &nDelim, iFlags,
                             1, &nToken );
      if( szToken && nLen > nToken )
         nSkip = szToken - zh_parc( 1 ) + nToken + nDelim;
      else
         nSkip = zh_parclen( 1 ) + 1;

      /* return position to start next search from */
      zh_storns( nSkip, 2 );
      /* return token */
      zh_retclen( szToken, nToken );
   }
   else
   {
      zh_storns( 0, 2 );
      zh_retc_null();
   }
}

ZH_FUNC( ZH_ATOKENS )
{
   const char * szLine, * szDelim;
   ZH_SIZE nLen, nDelim;
   int iFlags;

   if( zh_tokenParam( 2, 0, &szLine, &nLen, &szDelim, &nDelim, &iFlags ) )
      zh_itemReturnRelease( zh_tokenArray( szLine, nLen, szDelim, nDelim, iFlags ) );
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1123, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}
