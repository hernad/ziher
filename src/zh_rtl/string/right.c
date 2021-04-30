/*
 * Right() function
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

/* returns the right-most n characters in string */

ZH_FUNC( RIGHT )
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
         {
            PZH_CODEPAGE cdp = zh_vmCodepage();
            if( ZH_CODEPAGE_ISCHARIDX( cdp ) )
            {
               ZH_SIZE nChars = zh_cdpTextLen( cdp, zh_itemGetCPtr( pText ), nText );
               if( nChars > ( ZH_SIZE ) nLen )
                  nLen = nText - zh_cdpTextPos( cdp, zh_itemGetCPtr( pText ), nText, nChars - nLen );
               else
                  nLen = nText;
            }
         }
         if( ( ZH_SIZE ) nLen >= nText )
            zh_itemReturn( pText );
         else
            zh_retclen( zh_itemGetCPtr( pText ) + nText - nLen, nLen );
      }
   }
   else
      zh_retc_null();
}
