/*
 * zh_StrReplace()
 *
 * Copyright 2013 Przemyslaw Czerpak
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

/* zh_StrReplace( <cString>, [ <cSource> | <acSource> | <hReplace> ], [ <cDest> | <acDest> ] )
 *    --> <cResult>
 */
ZH_FUNC( ZH_STRREPLACE )
{
   PZH_ITEM pText = zh_param( 1, ZH_IT_STRING );
   PZH_ITEM pSrc = zh_param( 2, ZH_IT_STRING | ZH_IT_ARRAY |
                                ( ZH_ISNIL( 3 ) ? ZH_IT_HASH : 0 ) );

   if( pText && pSrc )
   {
      ZH_SIZE nText = zh_itemGetCLen( pText ),
              nSrc = zh_itemSize( pSrc );

      if( nText > 0 && nSrc > 0 )
      {
         PZH_ITEM pDst = zh_param( 3, ZH_IT_STRING | ZH_IT_ARRAY );
         const char * pszDst = pDst && ZH_IS_STRING( pDst ) ?
                               zh_itemGetCPtr( pDst ) : NULL;
         const char * pszSrc = ZH_IS_STRING( pSrc ) ?
                               zh_itemGetCPtr( pSrc ) : NULL;
         const char * pszText = zh_itemGetCPtr( pText );
         const char * ptr;
         char * pszResult = NULL;
         ZH_SIZE * ptrOpt = NULL;
         ZH_BOOL fNext = ZH_FALSE;
         ZH_SIZE nDst, nSize, nPos, nAt, nSkip, nTmp;

         nDst = zh_itemSize( ZH_IS_HASH( pSrc ) ? pSrc : pDst );
         if( nText > 1024 )
         {
            ptrOpt = ( ZH_SIZE * ) zh_xgrabz( 256 * sizeof( ZH_SIZE ) );
            for( nAt = 0; nAt < nSrc; ++nAt )
            {
               ZH_UCHAR uc;

               if( pszSrc )
                  uc = ( ZH_UCHAR ) pszSrc[ nAt ];
               else
               {
                  PZH_ITEM pItem = ZH_IS_HASH( pSrc ) ?
                                   zh_hashGetKeyAt( pSrc, nAt + 1 ) :
                                   zh_arrayGetItemPtr( pSrc, nAt + 1 );
                  if( zh_itemGetCLen( pItem ) == 0 )
                     continue;
                  uc = ( ZH_UCHAR ) zh_itemGetCPtr( pItem )[ 0 ];
               }
               if( ptrOpt[ uc ] == 0 )
                  ptrOpt[ uc ] = nAt + 1;
               else if( pszSrc == NULL )
                  fNext = ZH_TRUE;
            }
         }

         nSize = nPos = nSkip = 0;
         while( nPos < nText )
         {
            if( ptrOpt )
            {
               nAt = ptrOpt[ ( ZH_UCHAR ) pszText[ nPos ] ];
               if( nAt == 0 || pszSrc )
                  nSkip = 1;
               else
               {
                  for( ; nAt <= nSrc; ++nAt )
                  {
                     if( ZH_IS_HASH( pSrc ) )
                     {
                        pDst = zh_hashGetKeyAt( pSrc, nAt );
                        nSkip = zh_itemGetCLen( pDst );
                        ptr = zh_itemGetCPtr( pDst );
                     }
                     else
                     {
                        nSkip = zh_arrayGetCLen( pSrc, nAt );
                        ptr = zh_arrayGetCPtr( pSrc, nAt );
                     }
                     if( nSkip > 0 && nSkip <= nText - nPos &&
                         memcmp( pszText + nPos, ptr, nSkip ) == 0 )
                        break;
                     if( !fNext )
                        nAt = nSrc;
                  }
                  if( nAt > nSrc )
                  {
                     nAt = 0;
                     nSkip = 1;
                  }
               }
            }
            else if( pszSrc )
            {
               ptr = ( const char * )
                     memchr( pszSrc, ( ZH_UCHAR ) pszText[ nPos ], nSrc );
               nAt = ptr ? ptr - pszSrc + 1 : 0;
               nSkip = 1;
            }
            else
            {
               for( nAt = 1; nAt <= nSrc; ++nAt )
               {
                  if( ZH_IS_HASH( pSrc ) )
                  {
                     pDst = zh_hashGetKeyAt( pSrc, nAt );
                     nSkip = zh_itemGetCLen( pDst );
                     ptr = zh_itemGetCPtr( pDst );
                  }
                  else
                  {
                     nSkip = zh_arrayGetCLen( pSrc, nAt );
                     ptr = zh_arrayGetCPtr( pSrc, nAt );
                  }
                  if( nSkip > 0 && nSkip <= nText - nPos &&
                      memcmp( pszText + nPos, ptr, nSkip ) == 0 )
                     break;
               }
               if( nAt > nSrc )
               {
                  nAt = 0;
                  nSkip = 1;
               }
            }

            if( pszResult )
            {
               if( nAt != 0 )
               {
                  if( nAt <= nDst )
                  {
                     if( pszDst )
                        pszResult[ nSize++ ] = pszDst[ nAt - 1 ];
                     else
                     {
                        if( ZH_IS_HASH( pSrc ) )
                        {
                           pDst = zh_hashGetValueAt( pSrc, nAt );
                           nTmp = zh_itemGetCLen( pDst );
                           ptr = zh_itemGetCPtr( pDst );
                        }
                        else
                        {
                           nTmp = zh_arrayGetCLen( pDst, nAt );
                           ptr = zh_arrayGetCPtr( pDst, nAt );
                        }
                        memcpy( &pszResult[ nSize ], ptr, nTmp );
                        nSize += nTmp;
                     }
                  }
               }
               else
                  pszResult[ nSize++ ] = pszText[ nPos ];
               nPos += nSkip;
            }
            else
            {
               if( nAt != 0 )
               {
                  if( nAt <= nDst )
                  {
                     if( pszDst )
                        nSize++;
                     else if( ZH_IS_HASH( pSrc ) )
                        nSize += zh_itemGetCLen( zh_hashGetValueAt( pSrc, nAt ) );
                     else
                        nSize += zh_arrayGetCLen( pDst, nAt );
                  }
               }
               else
                  nSize++;
               nPos += nSkip;
               if( nPos == nText )
               {
                  pszResult = ( char * ) zh_xgrab( nSize + 1 );
                  nSize = nPos = 0;
               }
            }
         }
         if( ptrOpt )
            zh_xfree( ptrOpt );
         zh_retclen_buffer( pszResult, nSize );
      }
      else
         zh_itemReturn( pText );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}
