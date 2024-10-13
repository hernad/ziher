/*
 * The Terminal API (undocumented part)
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

#include "zh_gt_api.h"

void zh_gtWCreate( ZH_GT_RECT * rect, ZH_GT_WND ** wnd )
{
   /* TODO: */

   ZH_SYMBOL_UNUSED( rect );
   ZH_SYMBOL_UNUSED( wnd );
}

void zh_gtWDestroy( ZH_GT_WND * wnd )
{
   /* TODO: */

   ZH_SYMBOL_UNUSED( wnd );
}

ZH_BOOL zh_gtWFlash( void )
{
   /* TODO: */

   return ZH_FALSE;
}

void zh_gtWApp( ZH_GT_WND ** wnd )
{
   /* TODO: */

   ZH_SYMBOL_UNUSED( wnd );
}

void zh_gtWCurrent( ZH_GT_WND * wnd )
{
   /* TODO: */

   ZH_SYMBOL_UNUSED( wnd );
}

void zh_gtWPos( ZH_GT_WND * wnd, ZH_GT_RECT * rect )
{
   /* TODO: */

   ZH_SYMBOL_UNUSED( wnd );
   ZH_SYMBOL_UNUSED( rect );
}

ZH_BOOL zh_gtWVis( ZH_GT_WND * wnd, ZH_USHORT iStatus )
{
   /* TODO: */

   ZH_SYMBOL_UNUSED( wnd );
   ZH_SYMBOL_UNUSED( iStatus );

   return ZH_FALSE;
}

ZH_ERRCODE zh_gtSLR( ZH_GT_SLR * pSLR ) /* System-Level Request */
{

   ZH_SYMBOL_UNUSED( pSLR );

   return ZH_FAILURE;
}

ZH_ERRCODE zh_gtModalRead( void * dummy )
{
   /* TODO: */

   ZH_SYMBOL_UNUSED( dummy );

   return ZH_FAILURE;
}

ZH_ERRCODE zh_gtFlushCursor( void )
{
   /* TODO: */

   return ZH_FAILURE;
}

ZH_ERRCODE zh_gtSetColor( ZH_GT_RGB * color )
{
   /* TODO: */

   ZH_SYMBOL_UNUSED( color );

   return ZH_FAILURE;
}

ZH_ERRCODE zh_gtGetColor( ZH_GT_RGB * color )
{
   /* TODO: */

   ZH_SYMBOL_UNUSED( color );

   return ZH_FAILURE;
}

ZH_ERRCODE zh_gtSetBorder( ZH_GT_RGB * color )
{
   /* TODO: */

   ZH_SYMBOL_UNUSED( color );

   return ZH_FAILURE;
}
