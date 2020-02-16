/*
 * RAt() function
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
#include "zh_codepage_api.h"

ZH_FUNC( RAT )
{
   ZH_SIZE nSubLen = zh_parclen( 1 );
   ZH_SIZE nPos = 0;

   if( nSubLen )
   {
      ZH_ISIZ nTo = zh_parclen( 2 ) - nSubLen;

      if( nTo >= 0 )
      {
         PZH_CODEPAGE cdp = zh_vmCDP();
         const char * pszSub = zh_parc( 1 );
         const char * pszText = zh_parc( 2 );

         do
         {
            if( pszText[ nTo ] == *pszSub &&
                memcmp( pszSub, pszText + nTo, nSubLen ) == 0 )
            {
               if( ZH_CODEPAGE_ISCHARIDX( cdp ) )
                  nPos = zh_cdpTextLen( cdp, pszText, nTo ) + 1;
               else
                  nPos = nTo + 1;
               break;
            }
         }
         while( --nTo >= 0 );
      }
   }
   /* This function never seems to raise an error */
   zh_retns( nPos );
}

ZH_FUNC( ZH_RAT )
{
   ZH_SIZE nSubLen = zh_parclen( 1 );
   ZH_SIZE nPos = 0;

   if( nSubLen )
   {
      ZH_SIZE nLen = zh_parclen( 2 );
      ZH_ISIZ nTo = nLen - nSubLen;

      if( nTo >= 0 )
      {
         PZH_CODEPAGE cdp = zh_vmCDP();
         const char * pszSub = zh_parc( 1 );
         const char * pszText = zh_parc( 2 );
         ZH_ISIZ nStart = zh_parns( 3 );
         ZH_ISIZ nFrom;

         if( nStart <= 1 )
            nFrom = 0;
         else if( ZH_CODEPAGE_ISCHARIDX( cdp ) )
            nFrom = zh_cdpTextPos( cdp, pszText, nLen, --nStart );
         else
            nFrom = --nStart;

         if( nTo >= nFrom )
         {
            if( ZH_IS_PARAM_NUM( 4 ) )
            {
               ZH_ISIZ nEnd = zh_parns( 4 ) - 1;

               if( nEnd > 0 && ZH_CODEPAGE_ISCHARIDX( cdp ) )
                  nEnd = zh_cdpTextPos( cdp, pszText, nLen, nEnd );
               nEnd -= nSubLen - 1;

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
                     if( ZH_CODEPAGE_ISCHARIDX( cdp ) )
                        nPos = zh_cdpTextLen( cdp, pszText, nTo ) + 1;
                     else
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
