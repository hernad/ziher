/*
 * zh_AtI() function
 *
 * Copyright 2012 Przemyslaw Czerpak
 * Copyright 1999-2009 Viktor Szakats
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

static ZH_SIZE s_strAtI( PZH_CODEPAGE cdp, const char * szSub, ZH_SIZE nSubLen, const char * szText, ZH_SIZE nLen )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "s_strAtI(%p, %s, %" ZH_PFS "u, %s, %" ZH_PFS "u)", ( void * ) cdp, szSub, nSubLen, szText, nLen ) );

   if( nSubLen > 0 && nLen >= nSubLen )
   {
      ZH_SIZE nPos = 0, nIndex = 0;
      do
      {
         ZH_SIZE nSubPos = 0, nPrev = nPos;
         if( zh_cdpCharCaseEq( cdp, szText, nLen, &nPos, szSub, nSubLen, &nSubPos ) )
         {
            ZH_SIZE nBack = nPos;
            do
            {
               if( nSubPos >= nSubLen )
                  return ( ZH_CODEPAGE_ISCHARIDX( cdp ) ? nIndex : nPrev ) + 1;
            }
            while( zh_cdpCharCaseEq( cdp, szText, nLen, &nPos, szSub, nSubLen, &nSubPos ) );
            nPos = nBack;
         }
         ++nIndex;
      }
      while( nPos < nLen );
   }

   return 0;
}

ZH_FUNC( ZH_ATI )
{
   PZH_ITEM pSub  = zh_param( 1, ZH_IT_STRING );
   PZH_ITEM pText = zh_param( 2, ZH_IT_STRING );

   if( pText && pSub )
   {
      PZH_CODEPAGE cdp         = zh_vmCodepage();
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
            nPos = s_strAtI( cdp, zh_itemGetCPtr( pSub ), zh_itemGetCLen( pSub ),
                             pszText, nTo );
            if( nPos > 0 )
               nPos += ZH_CODEPAGE_ISCHARIDX( cdp ) ? nStart : nFrom;
         }
      }
      zh_retns( nPos );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1108, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}
