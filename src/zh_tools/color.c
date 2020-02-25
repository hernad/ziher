/*
 * CT3 video function:
 *    InvertAttr(), ColorToN(), NToColor(),
 *    Enhanced(), Standard(), Unselected()
 *
 * Copyright 2007 Przemyslaw Czerpak
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

ZH_FUNC( INVERTATTR )
{
   int iAttr;

   if( ZH_ISCHAR( 1 ) )
   {
      iAttr = zh_gtColorToN( zh_parc( 1 ) );
      if( iAttr == -1 )
         iAttr = 0;
   }
   else
      iAttr = zh_parni( 1 );

   zh_retni( ( iAttr & 0x88 ) |
             ( ( iAttr & 0x07 ) << 4 ) |
             ( ( iAttr >> 4 ) & 0x07 ) );
}

ZH_FUNC( COLORTON )
{
   if( ZH_ISCHAR( 1 ) )
   {
      int iColor = zh_gtColorToN( zh_parc( 1 ) );
      zh_retni( iColor == -1 ? 0 : iColor );
   }
   else
      zh_retni( zh_parni( 1 ) );
}

ZH_FUNC( NTOCOLOR )
{
   int iColor = zh_parnidef( 1, -1 );

   if( iColor >= 0x00 && iColor <= 0xff )
   {
      char szColorString[ 10 ];

      if( zh_parl( 2 ) )
         zh_gtColorsToString( &iColor, 1, szColorString, sizeof( szColorString ) );
      else
         zh_snprintf( szColorString, sizeof( szColorString ), "%02d/%02d", iColor & 0x0f, iColor >> 4 );
      zh_retc( szColorString );
   }
   else
      zh_retc_null();
}

ZH_FUNC( ENHANCED )
{
   zh_gtColorSelect( ZH_CLR_ENHANCED );
   zh_retc_null();
}

ZH_FUNC( STANDARD )
{
   zh_gtColorSelect( ZH_CLR_STANDARD );
   zh_retc_null();
}

ZH_FUNC( UNSELECTED )
{
   zh_gtColorSelect( ZH_CLR_UNSELECTED );
   zh_retc_null();
}
