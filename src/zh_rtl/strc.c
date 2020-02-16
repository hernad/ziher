/*
 * zh_StrDecodEscape() - decode string with \ escape sequences
 * zh_StrCDecode() - decode string using C compiler rules
 *
 * Copyright 2009 Przemyslaw Czerpak
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

/* zh_StrDecodEscape( <cEscSeqStr> ) --> <cStr>
 * decode string with \ escape sequences
 */
ZH_FUNC( ZH_STRDECODESCAPE )
{
   PZH_ITEM pText = zh_param( 1, ZH_IT_STRING );

   if( pText )
   {
      ZH_SIZE nLen = zh_itemGetCLen( pText );
      if( nLen > 0 )
      {
         char * str = ( char * ) zh_xgrab( nLen + 1 );
         zh_xmemcpy( str, zh_itemGetCPtr( pText ), nLen + 1 );
         zh_strRemEscSeq( str, &nLen );
         zh_retclen_buffer( str, nLen );
      }
      else
         zh_itemReturn( pText );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1099, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/* zh_StrCDecode( <cStr> [, @<lCont> ] ) --> <cResult> | NIL
 * decode string using C compiler rules
 * if second parameter <lCont> is passed by reference then it allows
 * to decode multiline strings. In such case <lCont> is set to .T.
 * if string ends with unclosed "" quoting.
 * Function returns decoded string or NIL on syntax error.
 */
ZH_FUNC( ZH_STRCDECODE )
{
   PZH_ITEM pText = zh_param( 1, ZH_IT_STRING );

   if( pText )
   {
      ZH_SIZE nLen = zh_itemGetCLen( pText );
      ZH_BOOL fCont = zh_parl( 2 );
      if( nLen > 0 )
      {
         const char * pszSrc = zh_itemGetCPtr( pText );
         char * pszDst = ( char * ) zh_xgrab( nLen + 1 );
         ZH_SIZE nDst = 0, n;

         for( ;; )
         {
            if( ! fCont )
            {
               while( nLen && ZH_ISSPACE( *pszSrc ) )
               {
                  ++pszSrc;
                  --nLen;
               }
               if( nLen && *pszSrc == '"' )
               {
                  ++pszSrc;
                  --nLen;
                  fCont = ZH_TRUE;
               }
            }
            if( ! fCont || ! nLen )
               break;

            n = 0;
            while( n < nLen )
            {
               char c = pszSrc[ n ];
               if( c == '"' )
               {
                  fCont = ZH_FALSE;
                  break;
               }
               pszDst[ nDst + n ] = c;
               if( ++n < nLen && c == '\\' )
               {
                  pszDst[ nDst + n ] = pszSrc[ n ];
                  ++n;
               }
            }
            if( n > 0 )
            {
               pszSrc += n;
               nLen -= n;
               zh_strRemEscSeq( pszDst + nDst, &n );
               nDst += n;
            }
            if( ! fCont )
            {
               ++pszSrc;
               --nLen;
            }
         }
         if( nLen == 0 && ( ! fCont || ZH_ISBYREF( 2 ) ) )
         {
            zh_retclen_buffer( pszDst, nDst );
            zh_storl( fCont, 2 );
         }
         else
            zh_xfree( pszDst );
      }
      else if( fCont )
         zh_itemReturn( pText );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1099, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}
