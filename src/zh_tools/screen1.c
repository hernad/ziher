/*
 * CT3 video functions:
 * ScreenAttr(), ScreenMix(), SayScreen(),
 * ClearWin(), InvertWin(), UnTextWin(), CharWin(), ColorWin(), ColorRepl()
 * and Ziher extension: ScreenText()
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

ZH_FUNC( SCREENATTR )
{
   int iRow, iCol;
   int iColor;
   ZH_BYTE bAttr;
   ZH_USHORT usChar;

   zh_gtGetPos( &iRow, &iCol );
   if( ZH_IS_PARAM_NUM( 1 ) )
      iRow = zh_parni( 1 );
   if( ZH_IS_PARAM_NUM( 2 ) )
      iCol = zh_parni( 2 );

   if( zh_gtGetChar( iRow, iCol, &iColor, &bAttr, &usChar ) != ZH_SUCCESS )
      iColor = 0;

   zh_retni( iColor );
}

ZH_FUNC( SCREENMIX )
{
   ZH_SIZE nLen = zh_parclen( 1 );

   if( nLen )
   {
      const char * szText = zh_parc( 1 );
      const char * szAttr;
      ZH_SIZE nAttr = zh_parclen( 2 );
      int iRow, iCol;

      if( nAttr == 0 )
      {
         szAttr = " ";
         nAttr = 1;
      }
      else
         szAttr = zh_parc( 2 );

      zh_gtGetPos( &iRow, &iCol );
      if( ZH_IS_PARAM_NUM( 3 ) )
         iRow = zh_parni( 3 );
      if( ZH_IS_PARAM_NUM( 4 ) )
         iCol = zh_parni( 4 );

      if( iRow >= 0 && iCol >= 0 &&
          iRow <= zh_gtMaxRow() && iCol <= zh_gtMaxCol() )
      {
         int iColor;
         ZH_BYTE bAttr;
         ZH_USHORT usChar;
         ZH_WCHAR wc;
         PZH_CODEPAGE cdp = zh_gtHostCP();
         ZH_SIZE nIndex = 0, ul = 0;
         int i;

         zh_gtBeginWrite();
         i = iCol;
         for( ;; )
         {
            if( zh_gtGetChar( iRow, i, &iColor, &bAttr, &usChar ) != ZH_SUCCESS )
            {
               if( ++iRow > zh_gtMaxRow() )
                  break;
               i = iCol;
            }
            else if( ZH_CODEPAGE_CHAR_GET( cdp, szText, nLen, &nIndex, &wc ) )
               zh_gtPutChar( iRow, i++, ( ZH_UCHAR ) szAttr[ ul ], 0, wc );
            else
               break;
            if( ++ul == nAttr )
               ul = 0;
         }
         zh_gtEndWrite();
      }
   }

   zh_retc_null();
}

ZH_FUNC( SAYSCREEN )
{
   ZH_SIZE nLen = zh_parclen( 1 );

   if( nLen )
   {
      const char * szText = zh_parc( 1 );
      int iRow, iCol;

      zh_gtGetPos( &iRow, &iCol );
      if( ZH_IS_PARAM_NUM( 2 ) )
         iRow = zh_parni( 2 );
      if( ZH_IS_PARAM_NUM( 3 ) )
         iCol = zh_parni( 3 );

      if( iRow >= 0 && iCol >= 0 &&
          iRow <= zh_gtMaxRow() && iCol <= zh_gtMaxCol() )
      {
         PZH_CODEPAGE cdp = zh_gtHostCP();
         ZH_SIZE nIndex = 0;
         int i;

         zh_gtBeginWrite();
         i = iCol;
         for( ;; )
         {
            int iColor;
            ZH_BYTE bAttr;
            ZH_USHORT usChar;
            ZH_WCHAR wc;
            if( zh_gtGetChar( iRow, i, &iColor, &bAttr, &usChar ) != ZH_SUCCESS )
            {
               if( ++iRow > zh_gtMaxRow() )
                  break;
               i = iCol;
            }
            else if( ZH_CODEPAGE_CHAR_GET( cdp, szText, nLen, &nIndex, &wc ) )
               zh_gtPutChar( iRow, i++, iColor, bAttr, wc );
            else
               break;
         }
         zh_gtEndWrite();
      }
   }

   zh_retc_null();
}

static ZH_BOOL zh_ctGetWinCord( int * piTop, int * piLeft,
                                int * piBottom, int * piRight )
{
   int iMaxRow = zh_gtMaxRow();
   int iMaxCol = zh_gtMaxCol();

   zh_gtGetPos( piTop, piLeft );

   if( ZH_IS_PARAM_NUM( 1 ) )
      *piTop = zh_parni( 1 );
   if( ZH_IS_PARAM_NUM( 2 ) )
      *piLeft   = zh_parni( 2 );
   if( ZH_IS_PARAM_NUM( 3 ) )
   {
      *piBottom = zh_parni( 3 );
      if( *piBottom > iMaxRow )
         *piBottom = iMaxRow;
   }
   else
      *piBottom = iMaxRow;
   if( ZH_IS_PARAM_NUM( 4 ) )
   {
      *piRight = zh_parni( 4 );
      if( *piRight > iMaxCol )
         *piRight = iMaxCol;
   }
   else
      *piRight = iMaxCol;

   return *piTop >= 0 && *piLeft >= 0 &&
          *piTop <= *piBottom && *piLeft <= *piRight;
}

static int zh_ctGetClearChar( int iParam )
{
   int iChar;

   if( ZH_IS_PARAM_NUM( iParam ) )
      iChar = zh_parni( iParam );
   else if( ZH_ISCHAR( iParam ) )
      iChar = ( ZH_UCHAR ) zh_parc( iParam )[ 0 ];
   else
      iChar = ( int ) zh_gtGetClearChar();

   return iChar;
}

static int zh_ctGetClearColor( int iParam )
{
   int iColor;

   if( ZH_IS_PARAM_NUM( iParam ) )
      iColor = zh_parni( iParam );
   else if( ZH_ISCHAR( iParam ) )
   {
      iColor = zh_gtColorToN( zh_parc( iParam ) );
      if( iColor == -1 )
         iColor = 0;
   }
   else
      iColor = zh_gtGetClearColor();

   return iColor;
}

ZH_FUNC( CLEARWIN )
{
   int iTop, iLeft, iBottom, iRight;

   if( zh_ctGetWinCord( &iTop, &iLeft, &iBottom, &iRight ) )
   {
      int iColor, iChar;

      iColor = zh_ctGetClearColor( 5 );
      iChar = zh_ctGetClearChar( 6 );

      zh_gtScrollEx( iTop, iLeft, iBottom, iRight, iColor, iChar, 0, 0 );
   }

   zh_retc_null();
}

ZH_FUNC( INVERTWIN )
{
   int iTop, iLeft, iBottom, iRight;

   if( zh_ctGetWinCord( &iTop, &iLeft, &iBottom, &iRight ) )
   {
      zh_gtBeginWrite();
      while( iTop <= iBottom )
      {
         int iCol = iLeft;
         while( iCol <= iRight )
         {
            int iColor;
            ZH_BYTE bAttr;
            ZH_USHORT usChar;

            zh_gtGetChar( iTop, iCol, &iColor, &bAttr, &usChar );
            iColor = ( iColor & 0x88 ) |
                     ( ( iColor & 0x07 ) << 4 ) |
                     ( ( iColor >> 4 ) & 0x07 );
            zh_gtPutChar( iTop, iCol, iColor, bAttr, usChar );
            ++iCol;
         }
         ++iTop;
      }
      zh_gtEndWrite();
   }

   zh_retc_null();
}

ZH_FUNC( UNTEXTWIN )
{
   int iTop, iLeft, iBottom, iRight;

   if( zh_ctGetWinCord( &iTop, &iLeft, &iBottom, &iRight ) )
   {
      ZH_USHORT usRepl, usInit, usEnd;

      usRepl = ( ZH_USHORT ) zh_ctGetClearChar( 5 );

      if( ZH_IS_PARAM_NUM( 6 ) )
         usInit = ( ZH_USHORT ) zh_parni( 6 );
      else if( zh_parclen( 6 ) > 0 )
         usInit = ( ZH_UCHAR ) zh_parc( 6 )[ 0 ];
      else
         usInit = 176;

      if( ZH_IS_PARAM_NUM( 7 ) )
         usEnd = ( ZH_USHORT ) zh_parni( 7 );
      else if( zh_parclen( 7 ) > 0 )
         usEnd = ( ZH_UCHAR ) zh_parc( 7 )[ 0 ];
      else
         usEnd = 223;

      zh_gtBeginWrite();
      while( iTop <= iBottom )
      {
         int iCol = iLeft;
         while( iCol <= iRight )
         {
            int iColor;
            ZH_BYTE bAttr;
            ZH_USHORT usChar;

            zh_gtGetChar( iTop, iCol, &iColor, &bAttr, &usChar );
            if( usInit <= usEnd ? ( usChar < usInit || usChar > usEnd ) :
                                  ( usChar > usEnd && usChar < usInit ) )
               zh_gtPutChar( iTop, iCol, iColor, bAttr, usRepl );
            ++iCol;
         }
         ++iTop;
      }
      zh_gtEndWrite();
   }

   zh_retc_null();
}

ZH_FUNC( CHARWIN )
{
   int iTop, iLeft, iBottom, iRight;

   if( zh_ctGetWinCord( &iTop, &iLeft, &iBottom, &iRight ) )
   {
      ZH_USHORT usNewChar, usOldChar = 0;
      ZH_BOOL fAll = ZH_FALSE;

      usNewChar = ( ZH_USHORT ) zh_ctGetClearChar( 5 );

      if( ZH_IS_PARAM_NUM( 6 ) )
         usOldChar = ( ZH_USHORT ) zh_parni( 6 );
      else if( zh_parclen( 6 ) > 0 )
         usOldChar = ( ZH_UCHAR ) zh_parc( 6 )[ 0 ];
      else
         fAll = ZH_TRUE;

      zh_gtBeginWrite();
      while( iTop <= iBottom )
      {
         int iCol = iLeft;
         while( iCol <= iRight )
         {
            int iColor;
            ZH_BYTE bAttr;
            ZH_USHORT usChar;

            zh_gtGetChar( iTop, iCol, &iColor, &bAttr, &usChar );
            if( fAll || usChar == usOldChar )
               zh_gtPutChar( iTop, iCol, iColor, bAttr, usNewChar );
            ++iCol;
         }
         ++iTop;
      }
      zh_gtEndWrite();
   }

   zh_retc_null();
}

ZH_FUNC( COLORWIN )
{
   int iTop, iLeft, iBottom, iRight;

   if( zh_ctGetWinCord( &iTop, &iLeft, &iBottom, &iRight ) )
   {
      int iNewColor, iOldColor = 0;
      ZH_BOOL fAll = ZH_FALSE;

      iNewColor = zh_ctGetClearColor( 5 );

      if( ZH_IS_PARAM_NUM( 6 ) || ZH_ISCHAR( 6 ) )
         iOldColor = zh_ctGetClearColor( 6 );
      else
         fAll = ZH_TRUE;

      zh_gtBeginWrite();
      while( iTop <= iBottom )
      {
         int iCol = iLeft;
         while( iCol <= iRight )
         {
            int iColor;
            ZH_BYTE bAttr;
            ZH_USHORT usChar;

            zh_gtGetChar( iTop, iCol, &iColor, &bAttr, &usChar );
            if( fAll || iColor == iOldColor )
               zh_gtPutChar( iTop, iCol, iNewColor, bAttr, usChar );
            ++iCol;
         }
         ++iTop;
      }
      zh_gtEndWrite();
   }

   zh_retc_null();
}

ZH_FUNC( SCREENTEXT )  /* ZH_EXTENSION */
{
   int iTop, iLeft, iBottom, iRight;

   if( zh_ctGetWinCord( &iTop, &iLeft, &iBottom, &iRight ) )
   {
      char * pBuffer;
      char * szText;
      ZH_SIZE nSize = ( ZH_SIZE ) ( iBottom - iTop + 1 ) * ( iRight - iLeft + 1 );
      szText = pBuffer = ( char * ) zh_xgrab( nSize + 1 );
      while( iTop <= iBottom )
      {
         int iCol = iLeft;
         while( iCol <= iRight )
         {
            int iColor;
            ZH_BYTE bAttr;
            ZH_USHORT usChar;
            zh_gtGetChar( iTop, iCol, &iColor, &bAttr, &usChar );
            *szText++ = ( char ) usChar;
            ++iCol;
         }
         ++iTop;
      }
      zh_retclen_buffer( pBuffer, nSize );
   }
   else
      zh_retc_null();
}

ZH_FUNC( COLORREPL )
{
   int iMaxRow = zh_gtMaxRow();
   int iMaxCol = zh_gtMaxCol();
   int iRow = 0;
   int iNewColor, iOldColor = 0;
   ZH_BOOL fAll = ZH_FALSE;

   iNewColor = zh_ctGetClearColor( 1 );

   if( ZH_IS_PARAM_NUM( 2 ) || ZH_ISCHAR( 2 ) )
      iOldColor = zh_ctGetClearColor( 2 );
   else
      fAll = ZH_TRUE;

   zh_gtBeginWrite();
   while( iRow <= iMaxRow )
   {
      int iCol = 0;
      while( iCol <= iMaxCol )
      {
         int iColor;
         ZH_BYTE bAttr;
         ZH_USHORT usChar;

         zh_gtGetChar( iRow, iCol, &iColor, &bAttr, &usChar );
         if( fAll || iColor == iOldColor )
            zh_gtPutChar( iRow, iCol, iNewColor, bAttr, usChar );
         ++iCol;
      }
      ++iRow;
   }
   zh_gtEndWrite();

   zh_retc_null();
}
