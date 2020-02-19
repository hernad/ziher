/*
 * Binary and unicode string functions:
 *    zh_UChar(), zh_UCode(), zh_ULen(), zh_UPeek(), zh_UPoke()
 *    zh_BChar(), zh_BCode(), zh_BLen(), zh_BPeek(), zh_BPoke()
 *
 * Copyright 2012 Przemyslaw Czerpak
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
#include "zh_codepage_api.h"
#include "zh_item_api.h"
#include "zh_api_error.h"

/* zh_UChar( <nCode> ) --> <cText>
 * return string with U+nCode character in ZHVM CP encoding
 */
ZH_FUNC( ZH_UCHAR )
{
   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      char szChar[ ZH_MAX_CHAR_LEN ];
      ZH_SIZE nLen;

      nLen = zh_cdpTextPutU16( zh_vmCDP(), szChar, sizeof( szChar ),
                                           ( ZH_WCHAR ) zh_parni( 1 ) );
      zh_retclen( szChar, nLen );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1111, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/* zh_BChar( <nCode> ) --> <cText>
 * return 1 byte string with <nCode> value
 */
ZH_FUNC( ZH_BCHAR )
{
   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      char c = ( char ) zh_parni( 1 );

      zh_retclen( &c, 1 );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1111, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/* zh_UCode( <cText> ) --> <nCode>
 * return unicode value of 1st character (not byte) in given string
 */
ZH_FUNC( ZH_UCODE )
{
   PZH_ITEM pText = zh_param( 1, ZH_IT_STRING );

   if( pText )
      zh_retni( zh_cdpTextGetU16( zh_vmCDP(), zh_itemGetCPtr( pText ),
                                              zh_itemGetCLen( pText ) ) );
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1111, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/* zh_BCode( <cText> ) --> <nCode>
 * return value of 1st byte in given string
 */
ZH_FUNC( ZH_BCODE )
{
   const char * szText = zh_parc( 1 );

   if( szText )
      zh_retni( ( ZH_UCHAR ) szText[ 0 ] );
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1111, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/* zh_ULen( <cText> ) --> <nChars>
 * return string length in characters
 */
ZH_FUNC( ZH_ULEN )
{
   PZH_ITEM pText = zh_param( 1, ZH_IT_STRING );

   if( pText )
      zh_retns( zh_cdpTextLen( zh_vmCDP(), zh_itemGetCPtr( pText ),
                                           zh_itemGetCLen( pText ) ) );
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1111, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/* zh_BLen( <cText> ) --> <nBytes>
 * return string length in bytes
 */
ZH_FUNC( ZH_BLEN )
{
   PZH_ITEM pText = zh_param( 1, ZH_IT_STRING );

   if( pText )
      zh_retns( zh_itemGetCLen( pText ) );
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1111, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/* zh_BEmpty( <cText> ) --> <lEmpty>
 * return string length in bytes
 */
ZH_FUNC( ZH_BEMPTY )
{
   PZH_ITEM pText = zh_param( 1, ZH_IT_STRING );

   if( pText )
      zh_retl( zh_itemGetCLen( pText ) == 0 );
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1111, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/* zh_UPeek( <cText>, <n> ) --> <nCode>
 * return unicode value of <n>th character in given string
 */
ZH_FUNC( ZH_UPEEK )
{
   PZH_ITEM pText = zh_param( 1, ZH_IT_STRING );

   if( pText && ZH_IS_PARAM_NUM( 2 ) )
   {
      PZH_CODEPAGE cdp = zh_vmCDP();
      const char * szText = zh_itemGetCPtr( pText );
      ZH_SIZE nLen = zh_itemGetCLen( pText );
      ZH_SIZE nPos = zh_parns( 2 );
      ZH_WCHAR wc = 0;

      if( nPos > 0 && nPos <= nLen )
      {
         nPos = zh_cdpTextPos( cdp, szText, nLen, nPos - 1 );
         nLen -= nPos;
         if( nLen > 0 )
            wc = zh_cdpTextGetU16( cdp, szText + nPos, nLen );
      }

      zh_retni( wc );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1111, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/* zh_BPeek( <cText>, <n> ) --> <nCode>
 * return value of <n>th byte in given string
 */
ZH_FUNC( ZH_BPEEK )
{
   PZH_ITEM pText = zh_param( 1, ZH_IT_STRING );

   if( pText && ZH_IS_PARAM_NUM( 2 ) )
   {
      ZH_SIZE nPos = zh_parns( 2 );

      zh_retni( ( nPos > 0 && nPos <= zh_itemGetCLen( pText ) ) ?
                ( ZH_UCHAR ) zh_itemGetCPtr( pText )[ nPos - 1 ] : 0 );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1111, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/* zh_UPoke( [@]<cText>, <n>, <nVal> ) --> <cText>
 * change <n>th character in given string to unicode <nVal> one and return modified text
 */
ZH_FUNC( ZH_UPOKE )
{
   PZH_ITEM pText = zh_param( 1, ZH_IT_STRING );

   if( pText && ZH_IS_PARAM_NUM( 2 ) && ZH_IS_PARAM_NUM( 3 ) )
   {
      PZH_CODEPAGE cdp = zh_vmCDP();
      const char * szText = zh_itemGetCPtr( pText );
      ZH_SIZE nLen = zh_itemGetCLen( pText );
      ZH_SIZE nPos = zh_parns( 2 );

      if( nPos > 0 && nPos <= nLen )
      {
         nPos = zh_cdpTextPos( cdp, szText, nLen, nPos - 1 );
         if( nPos < nLen )
         {
            char szChar[ ZH_MAX_CHAR_LEN ], * pszText;
            ZH_SIZE nChar, nOldChar;

            nChar = zh_cdpTextPutU16( cdp, szChar, sizeof( szChar ),
                                      ( ZH_WCHAR ) zh_parni( 3 ) );
            nOldChar = zh_cdpTextPos( cdp, szText + nPos, nLen - nPos, 1 );
            if( nChar == nOldChar )
            {
               if( zh_itemGetWriteCL( pText, &pszText, &nLen ) &&
                   nPos + nChar <= nLen )
                  memcpy( pszText + nPos, szChar, nChar );
            }
            else
            {
               pszText = ( char * ) zh_xgrab( nLen - nOldChar + nChar + 1 );

               memcpy( pszText, szText, nPos );
               memcpy( pszText + nPos, szChar, nChar );
               memcpy( pszText + nPos + nChar, szText + nPos + nOldChar,
                       nLen - nPos - nOldChar );
               if( ZH_ISBYREF( 1 ) )
                  zh_storclen( pszText, nLen - nOldChar + nChar, 1 );
               zh_retclen_buffer( pszText, nLen - nOldChar + nChar );
               return;
            }
         }
      }
      zh_itemReturn( pText );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1111, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/* zh_BPoke( [@]<cText>, <n>, <nVal> ) --> <cText>
 * change <n>th byte in given string to <nVal> and return modified text
 */
ZH_FUNC( ZH_BPOKE )
{
   PZH_ITEM pText = zh_param( 1, ZH_IT_STRING );

   if( pText && ZH_IS_PARAM_NUM( 2 ) && ZH_IS_PARAM_NUM( 3 ) )
   {
      ZH_SIZE nPos = zh_parns( 2 ), nLen;
      char * pszText;

      if( nPos > 0 && zh_itemGetWriteCL( pText, &pszText, &nLen ) &&
          nPos <= nLen )
      {
         pszText[ nPos - 1 ] = ( char ) ( zh_parni( 3 ) & 0xff );
      }
      zh_itemReturn( pText );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1111, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/* zh_USubStr( <cString>, <nStart>, <nCount> ) --> <cSubstring>
 */
ZH_FUNC( ZH_USUBSTR )
{
   PZH_ITEM pText = zh_param( 1, ZH_IT_STRING );
   int iPCount = zh_pcount();

   if( pText && ZH_IS_PARAM_NUM( 2 ) && ( iPCount < 3 || ZH_IS_PARAM_NUM( 3 ) ) )
   {
      PZH_CODEPAGE cdp = zh_vmCDP();
      const char * pszText = zh_itemGetCPtr( pText );
      ZH_ISIZ nSize = zh_itemGetCLen( pText );
      ZH_ISIZ nFrom = zh_parns( 2 );
      ZH_ISIZ nCount = iPCount < 3 ? nSize : zh_parns( 3 );

      if( nFrom > 0 )
      {
         if( --nFrom > nSize )
            nCount = 0;
      }

      if( nCount > 0 )
      {
         if( nFrom < 0 )
            nFrom += zh_cdpTextLen( cdp, pszText, nSize );
         if( nFrom > 0 )
         {
            nFrom = zh_cdpTextPos( cdp, pszText, nSize, nFrom );
            pszText += nFrom;
            nSize -= nFrom;
         }
         nCount = zh_cdpTextPos( cdp, pszText, nSize, nCount );
      }

      if( nCount > 0 )
      {
         if( nFrom <= 0 && nCount == nSize )
            zh_itemReturn( pText );
         else
            zh_retclen( pszText, nCount );
      }
      else
         zh_retc_null();
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1110, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/* zh_BSubStr( <cString>, <nStart>, <nCount> ) --> <cSubstring>
 */
ZH_FUNC( ZH_BSUBSTR )
{
   PZH_ITEM pText = zh_param( 1, ZH_IT_STRING );
   int iPCount = zh_pcount();

   if( pText && ZH_IS_PARAM_NUM( 2 ) && ( iPCount < 3 || ZH_IS_PARAM_NUM( 3 ) ) )
   {
      const char * pszText = zh_itemGetCPtr( pText );
      ZH_ISIZ nSize = zh_itemGetCLen( pText );
      ZH_ISIZ nFrom = zh_parns( 2 );
      ZH_ISIZ nCount = iPCount < 3 ? nSize : zh_parns( 3 );

      if( nFrom > 0 )
      {
         if( --nFrom > nSize )
            nCount = 0;
      }
      if( nCount > 0 )
      {
         if( nFrom < 0 )
            nFrom += nSize;
         if( nFrom > 0 )
         {
            pszText += nFrom;
            nSize -= nFrom;
         }
         if( nCount > nSize )
            nCount = nSize;
      }

      if( nCount > 0 )
      {
         if( nFrom <= 0 && nCount == nSize )
            zh_itemReturn( pText );
         else
            zh_retclen( pszText, nCount );
      }
      else
         zh_retc_null();
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1110, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/* zh_ULeft( <cString>, <nCount> ) --> <cSubstring>
 */
ZH_FUNC( ZH_ULEFT )
{
   PZH_ITEM pText = zh_param( 1, ZH_IT_STRING );

   if( pText && ZH_IS_PARAM_NUM( 2 ) )
   {
      ZH_ISIZ nLen = zh_parns( 2 );
      if( nLen <= 0 )
         zh_retc_null();
      else
      {
         ZH_SIZE nText = zh_itemGetCLen( pText );
         if( ( ZH_SIZE ) nLen < nText )
            nLen = zh_cdpTextPos( zh_vmCDP(), zh_itemGetCPtr( pText ), nText, nLen );
         if( ( ZH_SIZE ) nLen >= nText )
            zh_itemReturn( pText );
         else
            zh_retclen( zh_itemGetCPtr( pText ), nLen );
      }
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1124, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/* zh_BLeft( <cString>, <nCount> ) --> <cSubstring>
 */
ZH_FUNC( ZH_BLEFT )
{
   PZH_ITEM pText = zh_param( 1, ZH_IT_STRING );

   if( pText && ZH_IS_PARAM_NUM( 2 ) )
   {
      ZH_ISIZ nLen = zh_parns( 2 );
      if( nLen <= 0 )
         zh_retc_null();
      else
      {
         ZH_SIZE nText = zh_itemGetCLen( pText );
         if( ( ZH_SIZE ) nLen >= nText )
            zh_itemReturn( pText );
         else
            zh_retclen( zh_itemGetCPtr( pText ), nLen );
      }
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1124, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/* zh_URight( <cString>, <nCount> ) --> <cSubstring>
 */
ZH_FUNC( ZH_URIGHT )
{
   PZH_ITEM pText = zh_param( 1, ZH_IT_STRING );
   ZH_SIZE nText = zh_itemGetCLen( pText );
   ZH_ISIZ nLen = zh_parns( 2 );

   if( nLen > 0 && nText > 0 )
   {
      if( ( ZH_SIZE ) nLen < nText )
      {
         PZH_CODEPAGE cdp = zh_vmCDP();
         ZH_SIZE nChars = zh_cdpTextLen( cdp, zh_itemGetCPtr( pText ), nText );
         if( nChars > ( ZH_SIZE ) nLen )
            nLen = nText - zh_cdpTextPos( cdp, zh_itemGetCPtr( pText ), nText, nChars - nLen );
         else
            nLen = nText;
      }
      if( ( ZH_SIZE ) nLen >= nText )
         zh_itemReturn( pText );
      else
         zh_retclen( zh_itemGetCPtr( pText ) + nText - nLen, nLen );
   }
   else
      zh_retc_null();
}

/* zh_BRight( <cString>, <nCount> ) --> <cSubstring>
 */
ZH_FUNC( ZH_BRIGHT )
{
   PZH_ITEM pText = zh_param( 1, ZH_IT_STRING );
   ZH_SIZE nText = zh_itemGetCLen( pText );
   ZH_ISIZ nLen = zh_parns( 2 );

   if( nLen > 0 && nText > 0 )
   {
      if( ( ZH_SIZE ) nLen >= nText )
         zh_itemReturn( pText );
      else
         zh_retclen( zh_itemGetCPtr( pText ) + nText - nLen, nLen );
   }
   else
      zh_retc_null();
}


/* zh_UAt( <cSubString>, <cString>, [<nFrom>], [<nTo>] ) --> <nAt>
 */
ZH_FUNC( ZH_UAT )
{
   PZH_ITEM pSub = zh_param( 1, ZH_IT_STRING );
   PZH_ITEM pText = zh_param( 2, ZH_IT_STRING );

   if( pText && pSub )
   {
      PZH_CODEPAGE cdp = zh_vmCDP();
      const char * pszText = zh_itemGetCPtr( pText );
      ZH_SIZE nTextLength = zh_itemGetCLen( pText );
      ZH_SIZE nStart = zh_parns( 3 );
      ZH_SIZE nFrom, nPos = 0;

      if( nStart <= 1 )
         nStart = nFrom = 0;
      else
         nFrom = zh_cdpTextPos( cdp, pszText, nTextLength, --nStart );

      if( nFrom < nTextLength )
      {
         ZH_SIZE nTo;

         pszText += nFrom;
         nTextLength -= nFrom;
         if( ZH_IS_PARAM_NUM( 4 ) )
         {
            nTo = zh_parns( 4 );
            if( nTo <= nStart )
               nTo = 0;
            else
            {
               nTo -= nStart;
               nTo = zh_cdpTextPos( cdp, pszText, nTextLength, nTo );
               if( nTo > nTextLength )
                  nTo = nTextLength;
            }
         }
         else
            nTo = nTextLength;

         if( nTo > 0 )
         {
            nPos = zh_strAt( zh_itemGetCPtr( pSub ), zh_itemGetCLen( pSub ),
                             pszText, nTo );
            if( nPos > 0 )
               nPos = zh_cdpTextLen( cdp, pszText, nPos - 1 ) + 1 + nStart;
         }
      }
      zh_retns( nPos );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1108, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/* zh_BAt( <cSubString>, <cString>, [<nFrom>], [<nTo>] ) --> <nAt>
 */
ZH_FUNC( ZH_BAT )
{
   PZH_ITEM pSub = zh_param( 1, ZH_IT_STRING );
   PZH_ITEM pText = zh_param( 2, ZH_IT_STRING );

   if( pText && pSub )
   {
      const char * pszText = zh_itemGetCPtr( pText );
      ZH_SIZE nTextLength = zh_itemGetCLen( pText );
      ZH_SIZE nStart = zh_parns( 3 );
      ZH_SIZE nFrom, nPos = 0;

      if( nStart <= 1 )
         nStart = nFrom = 0;
      else
         nFrom = --nStart;

      if( nFrom < nTextLength )
      {
         ZH_SIZE nTo;

         pszText += nFrom;
         nTextLength -= nFrom;
         if( ZH_IS_PARAM_NUM( 4 ) )
         {
            nTo = zh_parns( 4 );
            if( nTo <= nStart )
               nTo = 0;
            else
            {
               nTo -= nStart;
               if( nTo > nTextLength )
                  nTo = nTextLength;
            }
         }
         else
            nTo = nTextLength;

         if( nTo > 0 )
         {
            nPos = zh_strAt( zh_itemGetCPtr( pSub ), zh_itemGetCLen( pSub ),
                             pszText, nTo );
            if( nPos > 0 )
               nPos += nFrom;
         }
      }
      zh_retns( nPos );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1108, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/* zh_BRAt( <cSubString>, <cString>, [<nFrom>], [<nTo>] ) --> <nAt>
 */
ZH_FUNC( ZH_BRAT )
{
   ZH_SIZE nSubLen = zh_parclen( 1 );
   ZH_SIZE nPos = 0;

   if( nSubLen )
   {
      ZH_SIZE nLen = zh_parclen( 2 );
      ZH_ISIZ nTo = nLen - nSubLen;

      if( nTo >= 0 )
      {
         const char * pszSub = zh_parc( 1 );
         const char * pszText = zh_parc( 2 );
         ZH_ISIZ nStart = zh_parns( 3 );
         ZH_ISIZ nFrom;

         if( nStart <= 1 )
            nFrom = 0;
         else
            nFrom = --nStart;

         if( nTo >= nFrom )
         {
            if( ZH_IS_PARAM_NUM( 4 ) )
            {
               ZH_ISIZ nEnd = zh_parns( 4 ) - nSubLen;

               if( nEnd < nTo )
                  nTo = nEnd;
            }

            if( nTo >= nFrom )
            {
               do
               {
                  if( pszText[ nTo ] == *pszSub &&
                      memcmp( pszSub, pszText + nTo, nSubLen ) == 0 )
                  {
                     nPos = nTo + 1;
                     break;
                  }
               }
               while( --nTo >= nFrom );
            }
         }
      }
   }

   zh_retns( nPos );
}

/* zh_BStuff( <cString>, <nAt>, <nDel>, <cIns> ) --> <cResult>
 */
ZH_FUNC( ZH_BSTUFF )
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
         if( nPos < 1 || nPos > nLen )
            nPos = nLen;
         else
            nPos--;
      }
      if( nDel )
      {
         if( nDel < 1 || nDel > nLen - nPos )
            nDel = nLen - nPos;
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
      zh_retc_null();
}

/* zh_UStuff( <cString>, <nAt>, <nDel>, <cIns> ) --> <cResult>
 */
ZH_FUNC( ZH_USTUFF )
{
   const char * szText = zh_parc( 1 );
   const char * szIns = zh_parc( 4 );

   if( szText && szIns && ZH_IS_PARAM_NUM( 2 ) && ZH_IS_PARAM_NUM( 3 ) )
   {
      PZH_CODEPAGE cdp = zh_vmCDP();
      ZH_SIZE nLen = zh_parclen( 1 );
      ZH_SIZE nPos = zh_parns( 2 );
      ZH_SIZE nDel = zh_parns( 3 );
      ZH_SIZE nIns = zh_parclen( 4 );
      ZH_SIZE nTot;

      if( nPos )
         nPos = nPos < 1 ? nLen : zh_cdpTextPos( cdp, szText, nLen, nPos - 1 );
      if( nDel )
      {
         if( nPos < nLen )
         {
            nDel = zh_cdpTextPos( cdp, szText + nPos, nLen - nPos, nDel );
            if( nDel == 0 )
               nDel = nLen - nPos;
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
      zh_retc_null();
}
