/*
 * The CodePages API
 *
 * Copyright 2002 Alexander S.Kresin <alex@belacy.belgorod.su>
 * Copyright 2009-2012 Przemyslaw Czerpak
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
#include "zh_error_api.h"
#include "zh_codepage_api.h"

static ZH_SIZE utf8pos( const char * szUTF8, ZH_SIZE nLen, ZH_SIZE nUTF8Pos )
{
   if( nUTF8Pos > 0 && nUTF8Pos <= nLen )
   {
      ZH_SIZE n1, n2;
      ZH_WCHAR uc;
      int n = 0;

      for( n1 = n2 = 0; n1 < nLen; )
      {
         if( zh_cdpUTF8ToU16NextChar( ( ZH_UCHAR ) szUTF8[ n1 ], &n, &uc ) )
            ++n1;

         if( n == 0 )
         {
            if( --nUTF8Pos == 0 )
               return n2 + 1;
            n2 = n1;
         }
      }
   }
   return 0;
}

ZH_FUNC( ZH_UTF8CHR )
{
   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      char utf8Char[ ZH_MAX_CHAR_LEN ];
      int iLen;

      iLen = zh_cdpU16CharToUTF8( utf8Char, ( ZH_WCHAR ) zh_parni( 1 ) );
      zh_retclen( utf8Char, iLen );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_UTF8ASC )
{
   const char * pszString = zh_parc( 1 );

   if( pszString )
   {
      ZH_SIZE nLen = zh_parclen( 1 );
      ZH_WCHAR wc = 0;
      int n = 0;

      while( nLen )
      {
         if( ! zh_cdpUTF8ToU16NextChar( ( unsigned char ) *pszString, &n, &wc ) )
            break;
         if( n == 0 )
            break;
         pszString++;
         nLen--;
      }
      zh_retnint( wc );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_UTF8AT )
{
   PZH_ITEM pSub = zh_param( 1, ZH_IT_STRING );
   PZH_ITEM pText = zh_param( 2, ZH_IT_STRING );

   if( pText && pSub )
   {
      ZH_SIZE nTextLength = zh_itemGetCLen( pText );
      ZH_SIZE nStart = zh_parnsdef( 3, 1 );
      ZH_SIZE nEnd = zh_parnsdef( 4, nTextLength ); /* nTextLength can be > UTF-8 len. No problem.*/

      if( nEnd < nStart )
         zh_retns( 0 );
      else
         zh_retns( zh_cdpUTF8StringAt( zh_itemGetCPtr( pSub ), zh_itemGetCLen( pSub ),
                                       zh_itemGetCPtr( pText ), nTextLength, nStart, nEnd, ZH_FALSE ) );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/*
 * NOTE: In zh_utf8RAt() we are still traversing from
 *       left to right, as it would be required anyway to
 *       determine the real string length. [bacco]
 */

ZH_FUNC( ZH_UTF8RAT )
{
   PZH_ITEM pSub = zh_param( 1, ZH_IT_STRING );
   PZH_ITEM pText = zh_param( 2, ZH_IT_STRING );

   if( pText && pSub )
   {
      ZH_SIZE nTextLength = zh_itemGetCLen( pText );
      ZH_SIZE nStart = zh_parnsdef( 3, 1 );
      ZH_SIZE nEnd = zh_parnsdef( 4, nTextLength ); /* nTextLength can be > UTF-8 len. No problem.*/

      if( nEnd < nStart )
         zh_retns( 0 );
      else
         zh_retns( zh_cdpUTF8StringAt( zh_itemGetCPtr( pSub ), zh_itemGetCLen( pSub ),
                                       zh_itemGetCPtr( pText ), nTextLength, nStart, nEnd, ZH_TRUE ) );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_UTF8SUBSTR )
{
   const char * szString = zh_parc( 1 );
   int iPCount = zh_pcount();

   if( szString && ( iPCount < 2 || ( ZH_IS_PARAM_NUM( 2 ) && ( iPCount < 3 || ZH_IS_PARAM_NUM( 3 ) ) ) ) )
   {
      char * szDest = NULL;
      ZH_SIZE nLen = zh_parclen( 1 ), nDest = 0;
      ZH_I_SIZE nFrom = zh_parns( 2 );
      ZH_I_SIZE nCount = iPCount < 3 ? ( ZH_I_SIZE ) nLen : zh_parns( 3 );

      if( nFrom < 0 )
      {
         nFrom += zh_cdpUTF8StringLength( szString, nLen );
         if( nFrom < 0 )
            nFrom = 0;
      }
      else if( nFrom )
         --nFrom;

      if( nLen > ( ZH_SIZE ) nFrom && nCount > 0 )
         szDest = zh_cdpUTF8StringSubstr( szString, nLen,
                                          nFrom, nCount, &nDest );
      if( szDest )
         zh_retclen_buffer( szDest, nDest );
      else
         zh_retc_null();
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_UTF8LEFT )
{
   const char * szString = zh_parc( 1 );

   if( szString && ZH_IS_PARAM_NUM( 2 ) )
   {
      ZH_I_SIZE nLenReq = zh_parns( 2 );
      ZH_SIZE nDest = 0;
      char * szDest = NULL;

      if( nLenReq > 0 )
         szDest = zh_cdpUTF8StringSubstr( szString, zh_parclen( 1 ),
                                          0, nLenReq, &nDest );

      if( szDest )
         zh_retclen_buffer( szDest, nDest );
      else
         zh_retc_null();
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_UTF8RIGHT )
{
   const char * szString = zh_parc( 1 );

   if( szString && ZH_IS_PARAM_NUM( 2 ) )
   {
      ZH_I_SIZE nLenReq = zh_parns( 2 );
      ZH_SIZE nLen = zh_parclen( 1 ), nDest = 0;
      char * szDest = NULL;

      if( nLen && nLenReq > 0 )
      {
         ZH_I_SIZE nFrom = zh_cdpUTF8StringLength( szString, nLen ) - nLenReq;
         if( nFrom < 0 )
            nFrom = 0;
         szDest = zh_cdpUTF8StringSubstr( szString, nLen,
                                          nFrom, nLenReq, &nDest );
      }

      if( szDest )
         zh_retclen_buffer( szDest, nDest );
      else
         zh_retc_null();
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_UTF8PEEK )
{
   const char * szString = zh_parc( 1 );

   if( szString && ZH_IS_PARAM_NUM( 2 ) )
   {
      ZH_SIZE nPos = zh_parns( 2 );
      ZH_SIZE nLen = zh_parclen( 1 );

      if( nPos > 0 && nPos <= nLen )
         zh_retnint( zh_cdpUTF8StringPeek( szString, nLen, nPos - 1 ) );
      else
         zh_retni( 0 );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_UTF8POKE )
{
   PZH_ITEM pText = zh_param( 1, ZH_IT_STRING );

   if( pText && ZH_IS_PARAM_NUM( 2 ) && ZH_IS_PARAM_NUM( 3 ) )
   {
      const char * szString = zh_itemGetCPtr( pText );
      ZH_SIZE nLen = zh_itemGetCLen( pText ), nPos;

      nPos = utf8pos( szString, nLen, zh_parns( 2 ) );
      if( nPos )
      {
         ZH_WCHAR uc, uc2;
         int n, n2;

         --nPos;
         uc = ( ZH_WCHAR ) zh_parni( 3 );
         n = zh_cdpUTF8CharSize( uc );
         n2 = 0;
         zh_cdpUTF8ToU16NextChar( szString[ nPos ], &n2, &uc2 );
         ++n2;
         if( n == n2 )
         {
            char * szText;
            if( zh_itemGetWriteCL( pText, &szText, &nLen ) &&
                nPos + n <= nLen )
            {
               zh_cdpU16CharToUTF8( &szText[ nPos ], uc );
            }
            zh_itemReturn( pText );
         }
         else
         {
            char * szResult = ( char * ) zh_xgrab( nLen - n2 + n + 1 );

            memcpy( szResult, szString, nPos );
            zh_cdpU16CharToUTF8( &szResult[ nPos ], uc );
            memcpy( szResult + nPos + n, szString + nPos + n2, nLen - nPos - n2 );
            if( ZH_ISBYREF( 1 ) )
               zh_storclen( szResult, nLen - n2 + n, 1 );
            zh_retclen_buffer( szResult, nLen - n2 + n );
         }
      }
      else
         zh_itemReturn( pText );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_UTF8STUFF )
{
   const char * szText = zh_parc( 1 );
   const char * szIns = zh_parc( 4 );

   if( szText && szIns && ZH_IS_PARAM_NUM( 2 ) && ZH_IS_PARAM_NUM( 3 ) )
   {
      ZH_SIZE nLen = zh_parclen( 1 );
      ZH_SIZE nPos = zh_parns( 2 );
      ZH_SIZE nDel = zh_parns( 3 );
      ZH_SIZE nIns = zh_parclen( 4 );
      ZH_SIZE nTot;

      if( nPos )
      {
         nPos = utf8pos( szText, nLen, nPos );
         if( nPos == 0 )
            nPos = nLen;
         else
            nPos--;
      }
      if( nDel )
      {
         if( nPos < nLen )
         {
            nDel = utf8pos( szText + nPos, nLen - nPos, nDel + 1 );
            if( nDel == 0 )
               nDel = nLen - nPos;
            else
               nDel--;
         }
         else
            nDel = 0;
      }

      if( ( nTot = nLen + nIns - nDel ) > 0 )
      {
         char * szResult = ( char * ) zh_xgrab( nTot + 1 );

         zh_xmemcpy( szResult, szText, nPos );
         zh_xmemcpy( szResult + nPos, szIns, nIns );
         zh_xmemcpy( szResult + nPos + nIns, szText + nPos + nDel,
                     nLen - ( nPos + nDel ) );
         zh_retclen_buffer( szResult, nTot );
      }
      else
         zh_retc_null();
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_UTF8LEN )
{
   const char * szText = zh_parc( 1 );

   if( szText )
      zh_retnint( zh_cdpUTF8StringLength( szText, zh_parclen( 1 ) ) );
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/* none of numeric parameters in StrTran() (4th and 5th) refers to
 * character position in string so we do not need to create new
 * zh_utf8StrTran() but we can safely use normal StrTran() function
 */
ZH_FUNC_TRANSLATE( ZH_UTF8STRTRAN, STRTRAN )
