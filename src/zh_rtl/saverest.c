/*
 * SaveScreen(), RestScreen() functions
 *
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
#include "zh_gt_api.h"

static void zh_getScreenRange( int * piMin, int * piMax,
                               ZH_BOOL fNoCheck, ZH_BOOL fVertical )
{
   int iFrom, iTo, iMax;

   if( fVertical )
   {
      iMax  = zh_gtMaxRow();
      iFrom = zh_parni( 1 );
      iTo   = zh_parnidef( 3, iMax );
   }
   else
   {
      iMax  = zh_gtMaxCol();
      iFrom = zh_parni( 2 );
      iTo   = zh_parnidef( 4, iMax );
   }

   if( iFrom < 0 )
      iFrom = 0;
   else if( iFrom > iMax && ! fNoCheck )
      iFrom = iMax;

   if( iTo < 0 )
      iTo = 0;
   else if( iTo > iMax && ! fNoCheck )
      iTo = iMax;

   if( iFrom > iTo )
   {
      *piMin = iTo;
      *piMax = iFrom;
   }
   else
   {
      *piMin = iFrom;
      *piMax = iTo;
   }
}

ZH_FUNC( SAVESCREEN )
{
   int iTop, iLeft, iBottom, iRight;
   ZH_SIZE nSize;
   void * pBuffer;
   ZH_BOOL fNoCheck = ZH_FALSE;

   zh_getScreenRange( &iTop, &iBottom, fNoCheck, ZH_TRUE );
   zh_getScreenRange( &iLeft, &iRight, fNoCheck, ZH_FALSE );

   zh_gtRectSize( iTop, iLeft, iBottom, iRight, &nSize );
   pBuffer = zh_xgrab( nSize + 1 );

   zh_gtSave( iTop, iLeft, iBottom, iRight, pBuffer );
   zh_retclen_buffer( ( char * ) pBuffer, nSize );
}

ZH_FUNC( RESTSCREEN )
{
   if( ZH_ISCHAR( 5 ) )
   {
      int iTop, iLeft, iBottom, iRight;
      ZH_SIZE nSize, nLen;
      void * pBuffer = NULL;
      const char * pBufStr = zh_parc( 5 );
      ZH_BOOL fNoCheck = ZH_FALSE;

      zh_getScreenRange( &iTop, &iBottom, fNoCheck, ZH_TRUE );
      zh_getScreenRange( &iLeft, &iRight, fNoCheck, ZH_FALSE );

      nLen = zh_parclen( 5 );
      zh_gtRectSize( iTop, iLeft, iBottom, iRight, &nSize );
      if( nLen < nSize )
      {
         pBuffer = zh_xgrab( nSize );
         memcpy( pBuffer, pBufStr, nLen );
         memset( ( char * ) pBuffer + nLen, 0, nSize - nLen );
         pBufStr = ( const char * ) pBuffer;
      }

      zh_gtRest( iTop, iLeft, iBottom, iRight, pBufStr );

      if( pBuffer )
         zh_xfree( pBuffer );
   }
}
