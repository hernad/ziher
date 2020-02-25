/*
 * CT3 string functions
 *     - CharList()
 *     - CharNoList()
 *
 * Copyright 2001 IntTec GmbH, Neunlindenstr 32, 79106 Freiburg, Germany
 *        Author: Martin Vogel <vogel@inttec.de>
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

#include "ct.h"

/* helper function for the list function */
void ct_charlist( int iMode )
{
   const char * pcString = zh_parc( 1 );
   ZH_SIZE sStrLen = zh_parclen( 1 );
   ZH_SIZE sCnt;

   if( iMode == CT_CHARLIST_CHARHIST )
   {
      ZH_SIZE asCharCnt[ UCHAR_MAX ];
      PZH_ITEM pArray = zh_itemArrayNew( ZH_SIZEOFARRAY( asCharCnt ) );

      for( sCnt = 0; sCnt < ZH_SIZEOFARRAY( asCharCnt ); ++sCnt )
         asCharCnt[ sCnt ] = 0;

      for( sCnt = 0; sCnt < sStrLen; ++sCnt )
         asCharCnt[ ( ZH_UCHAR ) pcString[ sCnt ] ]++;

      for( sCnt = 0; sCnt < ZH_SIZEOFARRAY( asCharCnt ); ++sCnt )
         zh_arraySetNS( pArray, sCnt + 1, asCharCnt[ sCnt ] );

      zh_itemReturnRelease( pArray );
   }
   else
   {
      char acCharCnt[ UCHAR_MAX ];
      ZH_SIZE sRetStrLen = 0;

      memset( acCharCnt, 0, sizeof( acCharCnt ) );

      if( iMode == CT_CHARLIST_CHARLIST )
      {
         char acMark[ UCHAR_MAX ];

         memset( acMark, 0, sizeof( acMark ) );

         for( sCnt = 0; sCnt < sStrLen; ++sCnt )
         {
            ZH_UCHAR uc = ( ZH_UCHAR ) pcString[ sCnt ];

            if( acMark[ uc ] == 0 )
            {
               acCharCnt[ sRetStrLen++ ] = uc;
               acMark[ uc ] = 1;
            }
         }
      }
      else if( iMode == CT_CHARLIST_CHARSLIST || iMode == CT_CHARLIST_CHARNOLIST )
      {
         char cScan = iMode == CT_CHARLIST_CHARSLIST ? 1 : 0;

         for( sCnt = 0; sCnt < sStrLen; ++sCnt )
            acCharCnt[ ( ZH_UCHAR ) pcString[ sCnt ] ] = 1;

         for( sCnt = 0; sCnt < ZH_SIZEOFARRAY( acCharCnt ); ++sCnt )
         {
            if( acCharCnt[ sCnt ] == cScan )
               acCharCnt[ sRetStrLen++ ] = ( ZH_UCHAR ) sCnt;
         }
      }
      zh_retclen( acCharCnt, sRetStrLen );
   }
}

ZH_FUNC( CHARLIST )
{
   ct_charlist( CT_CHARLIST_CHARLIST );
}

ZH_FUNC( CHARNOLIST )
{
   ct_charlist( CT_CHARLIST_CHARNOLIST );
}
