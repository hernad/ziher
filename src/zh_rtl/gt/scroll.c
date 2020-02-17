/*
 * Scroll() function
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
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
#include "zh_gt_api.h"

/* Scrolls a screen region */

ZH_FUNC( SCROLL )
{
   int iMaxRow = zh_gtMaxRow();
   int iMaxCol = zh_gtMaxCol();

   int iTop;
   int iLeft;
   int iBottom;
   int iRight;

   /* Enforce limits of (0,0) to (MaxRow(),MaxCol()) */

   iTop = zh_parni( 1 ); /* Defaults to zero on bad type */
   if( iTop < 0 )
      iTop = 0;
   else if( iTop > iMaxRow )
      iTop = iMaxRow;

   iLeft = zh_parni( 2 ); /* Defaults to zero on bad type */
   if( iLeft < 0 )
      iLeft = 0;
   else if( iLeft > iMaxCol )
      iLeft = iMaxCol;

   if( ZH_IS_PARAM_NUM( 3 ) )
   {
      iBottom = zh_parni( 3 );
      if( iBottom < 0 )
         iBottom = 0;
      else if( iBottom > iMaxRow )
         iBottom = iMaxRow;
   }
   else
      iBottom = iMaxRow;

   if( ZH_IS_PARAM_NUM( 4 ) )
   {
      iRight = zh_parni( 4 );
      if( iRight < 0 )
         iRight = 0;
      else if( iRight > iMaxCol )
         iRight = iMaxCol;
   }
   else
      iRight = iMaxCol;

   zh_gtScroll( iTop,
                iLeft,
                iBottom,
                iRight,
                zh_parni( 5 ),   /* Defaults to zero on bad type */
                zh_parni( 6 ) ); /* Defaults to zero on bad type */
}

ZH_FUNC( ZH_SCROLL )
{
   int iMaxRow = zh_gtMaxRow();
   int iMaxCol = zh_gtMaxCol();

   int iTop;
   int iLeft;
   int iBottom;
   int iRight;
   int iColor;
   int iChar;

   /* Enforce limits of (0,0) to (MaxRow(),MaxCol()) */

   iTop = zh_parni( 1 ); /* Defaults to zero on bad type */
   if( iTop < 0 )
      iTop = 0;
   else if( iTop > iMaxRow )
      iTop = iMaxRow;

   iLeft = zh_parni( 2 ); /* Defaults to zero on bad type */
   if( iLeft < 0 )
      iLeft = 0;
   else if( iLeft > iMaxCol )
      iLeft = iMaxCol;

   if( ZH_IS_PARAM_NUM( 3 ) )
   {
      iBottom = zh_parni( 3 );
      if( iBottom < 0 )
         iBottom = 0;
      else if( iBottom > iMaxRow )
         iBottom = iMaxRow;
   }
   else
      iBottom = iMaxRow;

   if( ZH_IS_PARAM_NUM( 4 ) )
   {
      iRight = zh_parni( 4 );
      if( iRight < 0 )
         iRight = 0;
      else if( iRight > iMaxCol )
         iRight = iMaxCol;
   }
   else
      iRight = iMaxCol;

   if( ZH_IS_PARAM_NUM( 7 ) )
      iColor = zh_parni( 7 );
   else if( ZH_ISCHAR( 7 ) )
      iColor = zh_gtColorToN( zh_parc( 7 ) );
   else
      iColor = -1;

   if( ZH_IS_PARAM_NUM( 8 ) )
   {
      iChar = zh_parni( 8 );
      if( iChar > 0 && iChar <= 255 )
      {
         PZH_CODEPAGE cdp = zh_vmCDP();
         if( ! ZH_CODEPAGE_ISCHARUNI( cdp ) )
            iChar = zh_cdpGetU16( cdp, ( ZH_UCHAR ) iChar );
      }
   }
   else if( ZH_ISCHAR( 8 ) )
      iChar = zh_cdpTextGetU16( zh_vmCDP(), zh_parc( 8 ), zh_parclen( 8 ) );
   else
      iChar = -1;

   zh_gtScrollEx( iTop,
                  iLeft,
                  iBottom,
                  iRight,
                  iColor,
                  iChar,
                  zh_parni( 5 ),   /* Defaults to zero on bad type */
                  zh_parni( 6 ) ); /* Defaults to zero on bad type */
}
