/*
 * CT3 video functions:
 * SayDown(), SaySpread(), SayMoveIn(), ScreenStr(), StrScreen()
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
#include "zh_string_api.h"
#include "zh_date.h"

ZH_FUNC( SAYDOWN )
{
   ZH_SIZE nLen = zh_parclen( 1 );

   if( nLen )
   {
      int iRow, iCol, iMaxRow, iMaxCol;
      long lDelay = zh_parnldef( 2, 4 );

      zh_gtGetPos( &iRow, &iCol );
      if( ZH_IS_PARAM_NUM( 3 ) )
         iRow = zh_parni( 3 );
      if( ZH_IS_PARAM_NUM( 4 ) )
         iCol = zh_parni( 4 );
      iMaxRow = zh_gtMaxRow();
      iMaxCol = zh_gtMaxCol();

      if( iRow >= 0 && iCol >= 0 && iRow <= iMaxRow && iCol <= iMaxCol )
      {
         const char * szText = zh_parc( 1 );
         ZH_SIZE nTextLen = zh_parclen( 1 );

         ZH_WCHAR wc;
         PZH_CODEPAGE cdp = zh_gtHostCodepage();
         ZH_SIZE nIndex = 0;

         int iColor = zh_gtGetCurrColor();

         if( nLen > ( ZH_SIZE ) ( iMaxRow - iRow + 1 ) )
            nLen = ( ZH_SIZE ) ( iMaxRow - iRow + 1 );

         zh_gtBeginWrite();
         while( nLen-- )
         {
            if( ZH_CODEPAGE_CHAR_GET( cdp, szText, nTextLen, &nIndex, &wc ) )
               zh_gtPutChar( iRow++, iCol, iColor, 0, wc );
            else
               break;

            if( lDelay )
            {
               zh_gtEndWrite();
               zh_idleSleep( ( double ) lDelay / 1000 );
               zh_gtBeginWrite();
            }
         }
         zh_gtEndWrite();
      }
   }

   zh_retc_null();
}

ZH_FUNC( SAYSPREAD )
{
   ZH_SIZE nLen;
   void * hText;
   const ZH_WCHAR * pwText = zh_parstr_u16( 1, ZH_CODEPAGE_ENDIAN_NATIVE, &hText, &nLen );

   if( nLen )
   {
      int iRow, iCol, iMaxRow, iMaxCol;
      long lDelay;

      lDelay = zh_parnldef( 2, 4 );

      iMaxRow = zh_gtMaxRow();
      iMaxCol = zh_gtMaxCol();
      zh_gtGetPos( &iRow, &iCol );
      if( ZH_IS_PARAM_NUM( 3 ) )
         iRow = zh_parni( 3 );
      else
         zh_gtGetPos( &iRow, &iCol );
      iCol = ZH_IS_PARAM_NUM( 4 ) ? zh_parni( 4 ) : ( iMaxCol >> 1 );

      if( iRow >= 0 && iCol >= 0 && iRow <= iMaxRow && iCol <= iMaxCol )
      {
         ZH_SIZE nPos;
         int iColor = zh_gtGetCurrColor();

         nPos = nLen >> 1;
         nLen = nLen & 1;
         if( ! nLen )
         {
            nLen = 2;
            --nPos;
         }

         zh_gtBeginWrite();
         do
         {
            ZH_SIZE nPos2;
            for( nPos2 = 0; nPos2 < nLen && iCol + ( int ) nPos2 <= iMaxCol; ++nPos2 )
               zh_gtPutChar( iRow, iCol + ( int ) nPos2, iColor, 0, pwText[ nPos + nPos2 ] );
            nLen += 2;
            if( lDelay )
            {
               zh_gtEndWrite();
               zh_idleSleep( ( double ) lDelay / 1000 );
               zh_gtBeginWrite();
            }
         }
         while( nPos-- && iCol-- );
         /* CT3 does not respect iCol in the above condition */
         zh_gtEndWrite();
      }
   }
   zh_strfree( hText );

   zh_retc_null();
}

ZH_FUNC( SAYMOVEIN )
{
   ZH_SIZE nLen;
   void * hText;
   const ZH_WCHAR * pwText = zh_parstr_u16( 1, ZH_CODEPAGE_ENDIAN_NATIVE, &hText, &nLen );

   if( nLen )
   {
      int iRow, iCol, iMaxRow, iMaxCol;
      long lDelay;
      ZH_BOOL fBack;

      lDelay = zh_parnldef( 2, 4 );
      fBack = zh_parl( 5 );

      iMaxRow = zh_gtMaxRow();
      iMaxCol = zh_gtMaxCol();
      zh_gtGetPos( &iRow, &iCol );
      if( ZH_IS_PARAM_NUM( 3 ) )
         iRow = zh_parni( 3 );
      if( ZH_IS_PARAM_NUM( 4 ) )
         iCol = zh_parni( 4 );

      if( iRow >= 0 && iCol >= 0 && iRow <= iMaxRow && iCol <= iMaxCol )
      {
         ZH_SIZE nChars;
         int iColor = zh_gtGetCurrColor();
         int iNewCol;

         iNewCol = iCol + ( int ) nLen;
         if( fBack )
            iCol += ( int ) nLen - 1;
         else
            pwText += ( int ) nLen - 1;
         nChars = 1;

         zh_gtBeginWrite();
         do
         {
            ZH_SIZE nPos;

            if( fBack )
            {
               if( iCol <= iMaxCol )
               {
                  for( nPos = 0; nPos < nChars; ++nPos )
                     zh_gtPutChar( iRow, iCol + ( int ) nPos, iColor, 0, pwText[ nPos ] );
               }
               --iCol;
            }
            else
            {
               for( nPos = 0; nPos < nChars; ++nPos )
                  zh_gtPutChar( iRow, iCol + ( int ) nPos, iColor, 0, pwText[ nPos ] );
               --pwText;
            }
            if( ( int ) nChars + iCol <= iMaxCol )
               ++nChars;

            if( lDelay )
            {
               zh_gtEndWrite();
               zh_idleSleep( ( double ) lDelay / 1000 );
               zh_gtBeginWrite();
            }
         }
         while( --nLen );
         zh_gtSetPos( iRow, iNewCol );
         zh_gtEndWrite();
      }
   }
   zh_strfree( hText );

   zh_retc_null();
}

ZH_FUNC( CLEARSLOW )  /* TODO: Unicode support */
{
   int iMaxRow = zh_gtMaxRow();
   int iMaxCol = zh_gtMaxCol();
   long lDelay = zh_parnl( 1 );
   int iTop    = zh_parni( 2 );
   int iLeft   = zh_parni( 3 );
   int iBottom = zh_parnidef( 4, iMaxRow );
   int iRight  = zh_parnidef( 5, iMaxCol );
   ZH_UCHAR ucChar;

   if( ZH_IS_PARAM_NUM( 6 ) )
      ucChar = ( ZH_UCHAR ) zh_parni( 6 );
   else if( ZH_ISCHAR( 6 ) )
      ucChar = ( ZH_UCHAR ) zh_parc( 6 )[ 0 ];
   else
      ucChar = ( ZH_UCHAR ) zh_gtGetClearChar();

   if( iTop >= 0 && iLeft >= 0 && iTop <= iBottom && iLeft <= iRight )
   {
      char pszFrame[ 2 ];
      int iColor = zh_gtGetCurrColor();
      double dX, dY, dXX, dYY;

      pszFrame[ 0 ] = ( char ) ucChar;
      pszFrame[ 1 ] = '\0';

      dX = iRight - iLeft + 1;
      dY = iBottom - iTop + 1;
      if( dX > dY )
      {
         dY /= dX;
         dX = 1;
      }
      else
      {
         dX /= dY;
         dY = 1;
      }
      dXX = dYY = 0;

      zh_gtBeginWrite();
      for( ;; )
      {
         zh_gtBoxEx( iTop, iLeft, iBottom, iRight, pszFrame, iColor );
         if( lDelay )
         {
            zh_gtEndWrite();
            zh_idleSleep( ( double ) lDelay / 1000 );
            zh_gtBeginWrite();
         }

         if( iTop >= iBottom && iLeft >= iRight )
            break;

         if( iTop < iBottom )
         {
            dYY += dY;
            if( dYY >= 1 )
            {
               iTop++;
               if( iBottom > iTop )
                  iBottom--;
               dYY -= 1;
            }
         }
         if( iLeft < iRight )
         {
            dXX += dX;
            if( dXX >= 1 )
            {
               iLeft++;
               if( iRight > iLeft )
                  iRight--;
            }
         }
      }
      zh_gtEndWrite();
   }

   zh_retc_null();
}

ZH_FUNC( SCREENSTR )  /* TODO: Unicode support */
{
   int iRow, iCol, iMaxRow, iMaxCol;
   char * pBuffer;
   ZH_SIZE nCount = ZH_SIZE_MAX;

   zh_gtGetPos( &iRow, &iCol );
   if( ZH_IS_PARAM_NUM( 1 ) )
      iRow = zh_parni( 1 );
   if( ZH_IS_PARAM_NUM( 2 ) )
      iCol = zh_parni( 2 );
   if( ZH_IS_PARAM_NUM( 3 ) )
      nCount = zh_parns( 3 );
   iMaxRow = zh_gtMaxRow();
   iMaxCol = zh_gtMaxCol();

   if( iRow >= 0 && iRow <= iMaxRow && iCol >= 0 && iCol <= iMaxCol && nCount )
   {
      char * szText;
      ZH_SIZE nSize = ( ZH_SIZE ) ( iMaxRow - iRow + 1 ) * ( iMaxCol - iCol + 1 );
      if( nSize > nCount )
         nSize = nCount;
      nCount = nSize;
      nSize <<= 1;
      szText = pBuffer = ( char * ) zh_xgrab( nSize + 1 );
      do
      {
         int iC = iCol;
         do
         {
            int iColor;
            ZH_BYTE bAttr;
            ZH_USHORT usChar;
            zh_gtGetChar( iRow, iC, &iColor, &bAttr, &usChar );
            *szText++ = ( char ) usChar;
            *szText++ = ( char ) iColor;
         }
         while( --nCount && ++iC <= iMaxCol );
      }
      while( nCount && ++iRow <= iMaxRow );

      zh_retclen_buffer( pBuffer, nSize );
   }
   else
      zh_retc_null();
}

ZH_FUNC( STRSCREEN )  /* TODO: Unicode support */
{
   ZH_SIZE nLen = zh_parclen( 1 );

   if( nLen & 1 )
      nLen--;

   if( nLen )
   {
      const char * szText = zh_parc( 1 );
      int iRow, iCol, iMaxRow, iMaxCol;

      zh_gtGetPos( &iRow, &iCol );
      if( ZH_IS_PARAM_NUM( 2 ) )
         iRow = zh_parni( 2 );
      if( ZH_IS_PARAM_NUM( 3 ) )
         iCol = zh_parni( 3 );
      iMaxRow = zh_gtMaxRow();
      iMaxCol = zh_gtMaxCol();

      if( iRow >= 0 && iRow <= iMaxRow && iCol >= 0 && iCol <= iMaxCol )
      {
         zh_gtBeginWrite();
         do
         {
            int iC = iCol;
            do
            {
               ZH_USHORT usChar = ( ZH_UCHAR ) *szText++;
               int iColor = ( ZH_UCHAR ) *szText++;
               zh_gtPutChar( iRow, iC, iColor, 0, usChar );
               nLen -= 2;
            }
            while( nLen && ++iC <= iMaxCol );
         }
         while( nLen && ++iRow <= iMaxRow );
         zh_gtEndWrite();
      }
   }

   zh_retc_null();
}

ZH_FUNC( __ZHCT_DSPTIME )  /* Helper function for ShowTime() */
{
   int iRow, iCol;
   int iColor, iLen;
   char szTime[ 10 ];

   iRow = zh_parni( 1 );
   iCol = zh_parni( 2 );
   if( ZH_IS_PARAM_NUM( 4 ) )
      iColor = zh_parni( 4 );
   else if( ZH_ISCHAR( 4 ) )
   {
      iColor = zh_gtColorToN( zh_parc( 4 ) );
      if( iColor == -1 )
         iColor = 0;
   }
   else
      iColor = zh_gtGetClearColor();

   zh_dateTimeStr( szTime );
   iLen = 8;

   if( zh_parl( 3 ) )
      iLen -= 3;

   if( zh_parl( 5 ) )
   {
      int iHour = ( szTime[ 0 ] - '0' ) * 10 + ( szTime[ 1 ] - '0' );

      if( zh_parl( 6 ) )
         szTime[ iLen++ ] = iHour >= 12 ? 'p' : 'a';
      if( iHour > 12 )
         iHour -= 12;
      else if( iHour == 0 )
         iHour = 12;
      szTime[ 0 ] = ( char ) ( iHour / 10 ) + '0';
      szTime[ 1 ] = ( char ) ( iHour % 10 ) + '0';
   }

   if( szTime[ 0 ] == '0' )
      szTime[ 0 ] = ' ';

   zh_gtPutText( iRow, iCol, szTime, iLen, iColor );
}
