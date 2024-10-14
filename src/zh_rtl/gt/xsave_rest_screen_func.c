/*
 * __XSaveScreen()/__XRestScreen() functions
 *
 * Copyright 1999-2001 Viktor Szakats (Rewritten in C)
 * Copyright 1999 Paul Tucker <ptucker@sympatico.ca>
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
#include "zh_stack.h"

typedef struct
{
   int    row;
   int    col;
   int    maxrow;
   int    maxcol;
   void * buffer;
} ZH_SCRDATA, * PZH_SCRDATA;

static void zh_xSaveRestRelease( void * cargo )
{
   PZH_SCRDATA pScrData = ( PZH_SCRDATA ) cargo;

   if( pScrData->buffer )
      zh_xfree( pScrData->buffer );
}

static ZH_TSD_NEW( s_scrData, sizeof( ZH_SCRDATA ), NULL, zh_xSaveRestRelease );

ZH_FUNC( __XSAVESCREEN )
{
   PZH_SCRDATA pScrData = ( PZH_SCRDATA ) zh_stackGetTSD( &s_scrData );
   ZH_SIZE nSize;

   zh_gtGetPos( &pScrData->row, &pScrData->col );
   pScrData->maxrow = zh_gtMaxRow();
   pScrData->maxcol = zh_gtMaxCol();
   zh_gtRectSize( 0, 0, pScrData->maxrow, pScrData->maxcol, &nSize );
   if( pScrData->buffer )
      zh_xfree( pScrData->buffer );
   pScrData->buffer = zh_xgrab( nSize );
   zh_gtSave( 0, 0, pScrData->maxrow, pScrData->maxcol, pScrData->buffer );
}

/* NOTE: There's no check about the screen size on restore, so this will
         fail if the user has changed the screen resolution between calling
         save and restore.
         [vszakats] */

ZH_FUNC( __XRESTSCREEN )
{
   PZH_SCRDATA pScrData = ( PZH_SCRDATA ) zh_stackTestTSD( &s_scrData );

   if( pScrData && pScrData->buffer )
   {
      zh_gtRest( 0, 0, pScrData->maxrow, pScrData->maxcol, pScrData->buffer );
      zh_xfree( pScrData->buffer );
      pScrData->buffer = NULL;

      zh_gtSetPos( pScrData->row, pScrData->col );
   }
}
