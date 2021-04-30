/*
 * Clipper Tools like window system
 *
 * Copyright 2006 Przemyslaw Czerpak
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
#include "ctwin.h"

static int zh_ctColorParam( int iParam, int iDefault )
{
   int iColor;

   if( ZH_IS_PARAM_NUM( iParam ) )
      iColor = zh_parni( iParam );
   else if( zh_parclen( iParam ) > 0 )
   {
      iColor = zh_gtColorToN( zh_parc( iParam ) );
      if( iColor == -1 )
         iColor = iDefault;
   }
   else
      iColor = iDefault;

   return iColor;
}

ZH_FUNC( CTWINIT )
{
   zh_retl( zh_ctwInit() );
}

ZH_FUNC( GETCLEARA )
{
   zh_retni( zh_gtGetClearColor() );
}

ZH_FUNC( SETCLEARA )
{
   int iColor = zh_ctColorParam( 1, -1 );

   if( iColor >= 0 )
      zh_gtSetClearColor( iColor );

   zh_retc_null();
}

ZH_FUNC( SETCLEARB )
{
   ZH_USHORT usNew;

   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      int iChar = zh_parni( 1 );
      PZH_CODEPAGE cdp = zh_vmCodepage();
      if( ! ZH_CODEPAGE_ISCHARUNI( cdp ) )
         iChar = zh_cdpGetU16( cdp, ( ZH_UCHAR ) iChar );
      usNew = ( ZH_USHORT ) iChar;
   }
   else if( ZH_ISCHAR( 1 ) )
      usNew = zh_cdpTextGetU16( zh_vmCodepage(), zh_parc( 1 ), zh_parclen( 1 ) );
   else
      usNew = ' ';  /* CT uses 255 => U+00A0 in CP437 */

   zh_gtSetClearChar( usNew );

   zh_retc_null();
}

ZH_FUNC( GETCLEARB )
{
   int iChar = zh_gtGetClearChar();
   PZH_CODEPAGE cdp = zh_vmCodepage();

   if( ! ZH_CODEPAGE_ISCHARUNI( cdp ) )
   {
      ZH_UCHAR uc = zh_cdpGetUC( cdp, ( ZH_WCHAR ) iChar, 0 );
      if( uc )
         iChar = uc;
   }

   zh_retni( iChar );
}

ZH_FUNC( WSETSHADOW )
{
   zh_retni( zh_ctwSetShadowAttr( zh_ctColorParam( 1, ZH_CTW_SHADOW_UNDEF ) ) );
}

ZH_FUNC( WSETMOVE )
{
   zh_retl( zh_ctwSetMoveMode( zh_parldef( 1, -1 ) ) != 0 );
}

ZH_FUNC( WSTEP )
{
   if( ZH_IS_PARAM_NUM( 1 ) && ZH_IS_PARAM_NUM( 2 ) )
      zh_retni( zh_ctwSetMoveStep( zh_parni( 1 ), zh_parni( 2 ) ) );
   else
      zh_retni( -1 );
}

ZH_FUNC( WMODE )
{
   zh_retni( zh_ctwSetBorderMode( zh_parldef( 1, -1 ),
                                  zh_parldef( 2, -1 ),
                                  zh_parldef( 3, -1 ),
                                  zh_parldef( 4, -1 ) ) );
}

ZH_FUNC( WBOARD )
{
   zh_retni( zh_ctwSetWindowBoard( zh_parni( 1 ), zh_parni( 2 ),
                                   ZH_IS_PARAM_NUM( 3 ) ? zh_parni( 3 ) : zh_gtMaxRow(),
                                   ZH_IS_PARAM_NUM( 4 ) ? zh_parni( 4 ) : zh_gtMaxCol() ) );
}

ZH_FUNC( WOPEN )
{
   int iColor;

   /* 6th (color) and 7th (lVisible) parameters are Ziher extensions */
   iColor = zh_ctColorParam( 6, -1 );   /* Ziher extension */ /* ZH_EXTENSION */
   zh_retni( zh_ctwCreateWindow( zh_parni( 1 ), zh_parni( 2 ),
                                 zh_parni( 3 ), zh_parni( 4 ),
                                 zh_parl( 5 ), iColor,
                                 zh_parldef( 7, ZH_TRUE ) ) ); /* ZH_EXTENSION */
}

ZH_FUNC( WCLOSE )
{
   /* 1st parameter (window handle) is Ziher extension */
   zh_retni( zh_ctwCloseWindow( ZH_IS_PARAM_NUM( 1 ) ? zh_parni( 1 ) : /* ZH_EXTENSION */
                                             zh_ctwCurrentWindow() ) );
}

ZH_FUNC( WACLOSE )
{
   zh_retni( zh_ctwCloseAllWindows() );
}

ZH_FUNC( WSELECT )  /* 2nd parameter (fBringToTop) is Ziher extension */
{
   zh_retni( ZH_IS_PARAM_NUM( 1 ) ? zh_ctwSelectWindow( zh_parni( 1 ),
                                                 zh_parldef( 2, ZH_TRUE ) ) : /* ZH_EXTENSION */
                          zh_ctwCurrentWindow() );
}

ZH_FUNC( WNUM )
{
   zh_retni( zh_ctwMaxWindow() );
}

ZH_FUNC( WBOX )
{
   static const ZH_WCHAR s_pWBoxFrames[ 16 ][ 9 ] = {
      ZH_B_DOUBLE_W,          /* 0  WB_DOUBLE_CLEAR */
      ZH_B_SINGLE_W,          /* 1  WB_SINGLE_CLEAR */
      ZH_B_DOUBLE_SINGLE_W,   /* 2  WB_DOUBLE_SINGLE_CLEAR */
      ZH_B_SINGLE_DOUBLE_W,   /* 3  WB_SINGLE_DOUBLE_CLEAR */

      ZH_B_DOUBLE_W,          /* 4  WB_DOUBLE */
      ZH_B_SINGLE_W,          /* 5  WB_SINGLE */
      ZH_B_DOUBLE_SINGLE_W,   /* 6  WB_DOUBLE_SINGLE */
      ZH_B_SINGLE_DOUBLE_W,   /* 7  WB_SINGLE_DOUBLE */

      ZH_B_HALF_FULL_W,       /* 8  WB_HALF_FULL_CLEAR */
      ZH_B_HALF_W,            /* 9  WB_HALF_CLEAR */
      ZH_B_FULL_HALF_W,       /* 10 WB_FULL_HALF_CLEAR */
      ZH_B_FULL_W,            /* 11 WB_FULL_CLEAR */

      ZH_B_HALF_FULL_W,       /* 12 WB_HALF_FULL */
      ZH_B_HALF_W,            /* 13 WB_HALF */
      ZH_B_FULL_HALF_W,       /* 14 WB_FULL_HALF */
      ZH_B_FULL_W
   };                         /* 15 WB_FULL */

   ZH_WCHAR szBoxBuf[ 10 ], wc;
   const char * pszBoxFrame = zh_parc( 1 );
   int iColor;

   if( pszBoxFrame )
   {
      ZH_SIZE nLen = zh_parclen( 1 ), nIndex = 0, nSize = 0;
      PZH_CODEPAGE cdp = zh_gtBoxCodepage();

      while( nSize < ZH_SIZEOFARRAY( szBoxBuf ) - 1 &&
             ZH_CODEPAGE_CHAR_GET( cdp, pszBoxFrame, nLen, &nIndex, &wc ) )
         szBoxBuf[ nSize++ ] = wc;
      szBoxBuf[ nSize ] = 0;
   }
   else
   {
      int iFrame = zh_parni( 1 );

      if( iFrame < 0 || iFrame > 15 )
         iFrame = 0;
      memcpy( szBoxBuf, s_pWBoxFrames[ iFrame ], 9 * sizeof( ZH_WCHAR ) );
      if( ( iFrame & 4 ) == 0 )
         szBoxBuf[ 8 ] = zh_gtGetClearChar();
      szBoxBuf[ 9 ] = '\0';
   }

   iColor = zh_ctColorParam( 2, -1 );   /* Ziher extension */ /* ZH_EXTENSION */
   zh_retni( zh_ctwAddWindowBox( zh_ctwCurrentWindow(), szBoxBuf, iColor ) );
}

ZH_FUNC( WFORMAT )
{
   int iWindow = zh_ctwCurrentWindow();
   int iTop, iLeft, iBottom, iRight;

   if( zh_pcount() == 0 )
   {
      zh_ctwGetFormatCords( iWindow, ZH_TRUE, &iTop, &iLeft, &iBottom, &iRight );
      iTop    = -iTop;
      iLeft   = -iLeft;
      iBottom = -iBottom;
      iRight  = -iRight;
   }
   else
   {
      iTop    = zh_parni( 1 );
      iLeft   = zh_parni( 2 );
      iBottom = zh_parni( 3 );
      iRight  = zh_parni( 4 );
   }
   zh_retni( zh_ctwChangeMargins( iWindow, iTop, iLeft, iBottom, iRight ) );
}

ZH_FUNC( WROW )
{
   int iTop, iLeft, iBottom, iRight;

   zh_ctwGetWindowCords( zh_ctwCurrentWindow(), zh_parl( 1 ), &iTop, &iLeft, &iBottom, &iRight );
   zh_retni( iTop );
}

ZH_FUNC( WCOL )
{
   int iTop, iLeft, iBottom, iRight;

   zh_ctwGetWindowCords( zh_ctwCurrentWindow(), zh_parl( 1 ), &iTop, &iLeft, &iBottom, &iRight );
   zh_retni( iLeft );
}

ZH_FUNC( WLASTROW )
{
   int iTop, iLeft, iBottom, iRight;

   zh_ctwGetWindowCords( zh_ctwCurrentWindow(), zh_parl( 1 ), &iTop, &iLeft, &iBottom, &iRight );
   zh_retni( iBottom );
}

ZH_FUNC( WLASTCOL )
{
   int iTop, iLeft, iBottom, iRight;

   zh_ctwGetWindowCords( zh_ctwCurrentWindow(), zh_parl( 1 ), &iTop, &iLeft, &iBottom, &iRight );
   zh_retni( iRight );
}

ZH_FUNC( WFROW )
{
   int iTop, iLeft, iBottom, iRight;

   zh_ctwGetFormatCords( zh_ctwCurrentWindow(), zh_parl( 1 ), &iTop, &iLeft, &iBottom, &iRight );
   zh_retni( iTop );
}

ZH_FUNC( WFCOL )
{
   int iTop, iLeft, iBottom, iRight;

   zh_ctwGetFormatCords( zh_ctwCurrentWindow(), zh_parl( 1 ), &iTop, &iLeft, &iBottom, &iRight );
   zh_retni( iLeft );
}

ZH_FUNC( WFLASTROW )
{
   int iTop, iLeft, iBottom, iRight;

   zh_ctwGetFormatCords( zh_ctwCurrentWindow(), zh_parl( 1 ), &iTop, &iLeft, &iBottom, &iRight );
   zh_retni( iBottom );
}

ZH_FUNC( WFLASTCOL )
{
   int iTop, iLeft, iBottom, iRight;

   zh_ctwGetFormatCords( zh_ctwCurrentWindow(), zh_parl( 1 ), &iTop, &iLeft, &iBottom, &iRight );
   zh_retni( iRight );
}

ZH_FUNC( WCENTER )
{
   zh_retni( zh_ctwCenterWindow( zh_ctwCurrentWindow(), zh_parl( 1 ) ) );
}

ZH_FUNC( WMOVE )
{
   zh_retni( zh_ctwMoveWindow( zh_ctwCurrentWindow(),
                               zh_parni( 1 ), zh_parni( 2 ) ) );
}

ZH_FUNC( CTWLASTKEY )
{
   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      int iNewKey = zh_parni( 1 );
      zh_retni( zh_ctwLastKey( &iNewKey ) );
   }
   else
      zh_retni( zh_ctwLastKey( NULL ) );
}

/* NOTE: These two functions are emulating the MaxRow()/MaxCol() core functions
         "overloaded" by the CT3 library. */

ZH_FUNC( HBCT_MAXROW ) /* Return the maximum screen/window row number (zero origin) */
{
   if( zh_parl( 1 ) )
   {
      int iRows, iCols;
      zh_gtScrDim( &iRows, &iCols );
      zh_retni( iRows - 1 );
   }
   else
      zh_retni( zh_gtMaxRow() );
}

ZH_FUNC( HBCT_MAXCOL ) /* Return the maximum screen/window column number (zero origin) */
{
   if( zh_parl( 1 ) )
   {
      int iRows, iCols;
      zh_gtScrDim( &iRows, &iCols );
      zh_retni( iCols - 1 );
   }
   else
      zh_retni( zh_gtMaxCol() );
}

/* Undocumented CT3 window functions
 */

/*
   WAlias( <nHandle> ) --> <nHandle> | -1
   change current window handle to <nHandle>
   if <nHandle> is not used by other window
   or is current window.
 */
ZH_FUNC( WALIAS )
{
   int iWindow = zh_parnidef( 1, -1 );

   /* 255 is original CT3 limit,
    * Ziher CTWIN does not have such internal limits
    */
   if( iWindow >= 0 && iWindow <= 255 )
      iWindow = zh_ctwChangeWindowHandle( iWindow );
   else
      iWindow = -1;

   zh_retni( iWindow );
}

/*
   WList() --> <cHandleList>
   _WStack() --> <cHandleList>
   return string with window handles in each character,
   the last character is the top window.

   Warning: this is compatibility only function
            which works correctly only for 255 windows.
 */

ZH_FUNC( WLIST )
{
   const int * piStack;
   int iWindows, iFrom, i;

   iWindows = zh_ctwGetWindowStack( &piStack );
   if( iWindows < 0 )
      zh_retc_null();
   else if( iWindows == 0 )
      zh_retclen( "\000", 1 );
   else
   {
      char * pszWindows = ( char * ) zh_xgrab( iWindows + 2 );

      iFrom = 0;
      if( zh_ctwCurrentWindow() == 0 )
         pszWindows[ iWindows ] = 0;
      else
         pszWindows[ iFrom++ ] = 0;

      for( i = 0; i < iWindows; ++i )
         pszWindows[ iFrom + i ] = ( char ) piStack[ i ];

      zh_retclen_buffer( pszWindows, iWindows + 1 );
   }
}

ZH_FUNC_TRANSLATE( _WSTACK, WLIST )

/* Temporary Ziher extensions to test some extended CTW functionality
 */

ZH_FUNC( WHIDE ) /* ZH_EXTENSION */
{
   zh_ctwVisible( ZH_IS_PARAM_NUM( 1 ) ? zh_parni( 1 ) : zh_ctwCurrentWindow(),
                  ZH_CTW_HIDDEN );
}

ZH_FUNC( WSHOW ) /* ZH_EXTENSION */
{
   zh_ctwVisible( ZH_IS_PARAM_NUM( 1 ) ? zh_parni( 1 ) : zh_ctwCurrentWindow(),
                  ZH_CTW_VISIBLE );
}

ZH_FUNC( WSHADOW ) /* ZH_EXTENSION */
{
   zh_retni( zh_ctwSetWindowShadow( zh_ctwCurrentWindow(),
                                    zh_parnidef( 1, ZH_CTW_SHADOW_UNDEF ) /* nAttr */ ) );
}

ZH_FUNC( WLEVEL ) /* ZH_EXTENSION */
{
   zh_retni( zh_ctwSetWindowLevel( zh_ctwCurrentWindow(),
                                   zh_parnidef( 1, ZH_CTW_UNDEF ) /* nLevel */ ) );
}
