/*
 * At() function
 *
 * Copyright 2012 Przemyslaw Czerpak
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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
#include "zh_codepage_api.h"
#include "zh_error_api.h"

/* locates a substring in a string */

ZH_FUNC( ZH_AT )
{
   PZH_ITEM pSub  = zh_param( 1, ZH_IT_STRING );
   PZH_ITEM pText = zh_param( 2, ZH_IT_STRING );

   if( pText && pSub )
   {
      PZH_CODEPAGE cdp         = zh_vmCDP();
      const char * pszText     = zh_itemGetCPtr( pText );
      ZH_SIZE      nTextLength = zh_itemGetCLen( pText );
      ZH_SIZE      nStart      = zh_parns( 3 );
      ZH_SIZE      nFrom, nPos = 0;

      if( nStart <= 1 )
         nStart = nFrom = 0;
      else if( ZH_CODEPAGE_ISCHARIDX( cdp ) )
         nFrom = zh_cdpTextPos( cdp, pszText, nTextLength, --nStart );
      else
         nFrom = --nStart;

      if( nFrom < nTextLength )
      {
         ZH_SIZE nTo;

         pszText     += nFrom;
         nTextLength -= nFrom;
         if( ZH_IS_PARAM_NUM( 4 ) )
         {
            nTo = zh_parns( 4 );
            if( nTo <= nStart )
               nTo = 0;
            else
            {
               nTo -= nStart;
               if( ZH_CODEPAGE_ISCHARIDX( cdp ) )
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
            {
               if( ZH_CODEPAGE_ISCHARIDX( cdp ) )
                  nPos = zh_cdpTextLen( cdp, pszText, nPos - 1 ) + 1 + nStart;
               else
                  nPos += nFrom;
            }
         }
      }
      zh_retns( nPos );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1108, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( AT )
{
   PZH_ITEM pSub  = zh_param( 1, ZH_IT_STRING );
   PZH_ITEM pText = zh_param( 2, ZH_IT_STRING );

   if( pText && pSub )
   {
      ZH_SIZE nPos = zh_strAt( zh_itemGetCPtr( pSub ), zh_itemGetCLen( pSub ),
                               zh_itemGetCPtr( pText ), zh_itemGetCLen( pText ) );
      if( nPos )
      {
         PZH_CODEPAGE cdp = zh_vmCDP();
         if( ZH_CODEPAGE_ISCHARIDX( cdp ) )
            nPos = zh_cdpTextLen( cdp, zh_itemGetCPtr( pText ), nPos - 1 ) + 1;
      }
      zh_retns( nPos );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1108, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}
