/*
 * DispBox() function
 *
 * Copyright 1999-2001 Viktor Szakats
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
#include "zh_gt_api.h"
#include "zh_item_api.h"

ZH_FUNC( DISPBOX )
{
   PZH_ITEM pTop    = zh_param( 1, ZH_IT_NUMERIC );
   PZH_ITEM pLeft   = zh_param( 2, ZH_IT_NUMERIC );
   PZH_ITEM pBottom = zh_param( 3, ZH_IT_NUMERIC );
   PZH_ITEM pRight  = zh_param( 4, ZH_IT_NUMERIC );

   if( pTop && pLeft && pBottom && pRight )
   {
      const char * pszBox   = zh_parc( 5 );
      const char * pszColor = zh_parc( 6 );

      if( pszBox )
      {
         int iColor;

         if( pszColor )
            iColor = zh_gtColorToN( pszColor );
         else if( ZH_IS_PARAM_NUM( 6 ) )
            iColor = zh_parni( 6 );
         else
            iColor = -1;
         zh_gtBoxEx( zh_itemGetNI( pTop ),
                     zh_itemGetNI( pLeft ),
                     zh_itemGetNI( pBottom ),
                     zh_itemGetNI( pRight ),
                     pszBox,
                     iColor );
      }
      else
      {
         char szOldColor[ ZH_CLRSTR_LEN ];

         if( pszColor )
         {
            zh_gtGetColorStr( szOldColor );
            zh_gtSetColorStr( pszColor );
         }

         if( zh_parni( 5 ) == 2 )
            zh_gtBoxD( zh_itemGetNI( pTop ),
                       zh_itemGetNI( pLeft ),
                       zh_itemGetNI( pBottom ),
                       zh_itemGetNI( pRight ) );

         else
            zh_gtBoxS( zh_itemGetNI( pTop ),
                       zh_itemGetNI( pLeft ),
                       zh_itemGetNI( pBottom ),
                       zh_itemGetNI( pRight ) );

         if( pszColor )
            zh_gtSetColorStr( szOldColor );
      }
   }
}

ZH_FUNC( ZH_DISPBOX )
{
   PZH_ITEM pTop    = zh_param( 1, ZH_IT_NUMERIC );
   PZH_ITEM pLeft   = zh_param( 2, ZH_IT_NUMERIC );
   PZH_ITEM pBottom = zh_param( 3, ZH_IT_NUMERIC );
   PZH_ITEM pRight  = zh_param( 4, ZH_IT_NUMERIC );

   if( pTop && pLeft && pBottom && pRight )
   {
      const char * pszBox   = zh_parc( 5 );
      const char * pszColor = zh_parc( 6 );
      int          iColor   = pszColor ? zh_gtColorToN( pszColor ) : zh_parnidef( 6, -1 );

      zh_gtDrawBox( zh_itemGetNI( pTop ),
                    zh_itemGetNI( pLeft ),
                    zh_itemGetNI( pBottom ),
                    zh_itemGetNI( pRight ),
                    pszBox,
                    iColor );
   }
}
