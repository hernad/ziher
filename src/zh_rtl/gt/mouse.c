/*
 * Mouse API
 *
 * Copyright 1999-2009 Viktor Szakats
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

/* Ziher callable interface */

ZH_FUNC( MPRESENT )
{
   zh_retl( zh_mouseIsPresent() );
}

ZH_FUNC( MHIDE )
{
   zh_mouseSetCursor( ZH_FALSE );
}

ZH_FUNC( MSHOW )
{
   zh_mouseSetCursor( ZH_TRUE );
}

ZH_FUNC( MSETCURSOR )
{
   zh_retl( zh_mouseGetCursor() );

   if( ZH_ISLOGICAL( 1 ) )
      zh_mouseSetCursor( zh_parl( 1 ) );
}

ZH_FUNC( MROW )
{
   if( zh_parl( 1 ) )
   {
      int iRow, iCol;

      zh_mouseGetPos( &iRow, &iCol );
      zh_retni( iRow );
   }
   else
      zh_retni( zh_mouseRow() );
}

ZH_FUNC( MCOL )
{
   if( zh_parl( 1 ) )
   {
      int iRow, iCol;

      zh_mouseGetPos( &iRow, &iCol );
      zh_retni( iCol );
   }
   else
      zh_retni( zh_mouseCol() );
}

ZH_FUNC( MSETPOS )
{
   if( ZH_IS_PARAM_NUM( 1 ) && ZH_IS_PARAM_NUM( 2 ) )
      zh_mouseSetPos( zh_parni( 1 ), zh_parni( 2 ) );
}

ZH_FUNC( MLEFTDOWN )
{
   zh_retl( zh_mouseButtonState( ZH_MBUTTON_LEFT ) );
}

ZH_FUNC( MRIGHTDOWN )
{
   zh_retl( zh_mouseButtonState( ZH_MBUTTON_RIGHT ) );
}

ZH_FUNC( MDBLCLK )
{
   zh_retni( zh_mouseGetDoubleClickSpeed() );

   if( ZH_IS_PARAM_NUM( 1 ) )
      zh_mouseSetDoubleClickSpeed( zh_parni( 1 ) );
}

ZH_FUNC( MSAVESTATE )
{
   int iLen = zh_mouseStorageSize();

   if( iLen > 0 )
   {
      void * pBuffer = zh_xgrab( iLen + 1 );

      zh_mouseSaveState( pBuffer );
      zh_retclen_buffer( ( char * ) pBuffer, iLen );
   }
   else
      zh_retc_null();
}

ZH_FUNC( MRESTSTATE )
{
   if( zh_parclen( 1 ) == ( ZH_SIZE ) zh_mouseStorageSize() )
      zh_mouseRestoreState( zh_parc( 1 ) );
}

ZH_FUNC( MSETBOUNDS )
{
   zh_mouseSetBounds( zh_parni( 1 ), /* Defaults to zero on bad type */
                      zh_parni( 2 ), /* Defaults to zero on bad type */
                      ZH_IS_PARAM_NUM( 3 ) ? zh_parni( 3 ) : zh_gtMaxRow(),
                      ZH_IS_PARAM_NUM( 4 ) ? zh_parni( 4 ) : zh_gtMaxCol() );
}

ZH_FUNC( ZH_MGETBOUNDS )
{
   int iTop, iLeft, iBottom, iRight;

   zh_mouseGetBounds( &iTop, &iLeft, &iBottom, &iRight );

   zh_storni( iTop, 1 );
   zh_storni( iLeft, 2 );
   zh_storni( iBottom, 3 );
   zh_storni( iRight, 4 );
}

ZH_FUNC( ZH_MMIDDLEDOWN )
{
   zh_retl( zh_mouseButtonState( ZH_MBUTTON_MIDDLE ) );
}
