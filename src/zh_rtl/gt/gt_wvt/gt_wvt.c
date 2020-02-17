/*
 * Video subsystem for Windows using GDI windows instead of Console
 *     Copyright 2003 Peter Rees <peter@rees.co.nz>
 *                    Rees Software & Systems Ltd
 * based on
 *   Bcc ConIO Video subsystem by
 *     Copyright 2002 Marek Paliwoda <paliwoda@inteia.pl>
 *     Copyright 2002 Przemyslaw Czerpak <druzus@polbox.com>
 *   Video subsystem for Windows compilers
 *     Copyright 1999-2000 Paul Tucker <ptucker@sympatico.ca>
 *     Copyright 2002 Przemyslaw Czerpak <druzus@polbox.com>
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus /at/ priv.onet.pl>
 *    Adopted to new GT API
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    zh_gt_Tone()
 *
 * Copyright 2003-2004 Giancarlo Niccolai <gc@niccolai.ws>
 *         Standard xplatform GT Info system,
 *         Graphical object system and event system.
 *         zh_gtInfo() And GTO_* implementation.
 *
 * Copyright 2004 Mauricio Abre <maurifull@datafull.com>
 *         Cross-GT, multi-platform Graphics API
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

#include "gtwvt.h"

#ifndef WS_EX_COMPOSITED
#define WS_EX_COMPOSITED  0x02000000
#endif

#ifndef LWA_ALPHA
#define LWA_ALPHA         0x00000002
#endif

#ifndef SM_REMOTESESSION
#define SM_REMOTESESSION  0x1000
#endif

static int s_GtId;
static ZH_GT_FUNCS SuperTable;
#define ZH_GTSUPER        ( &SuperTable )
#define ZH_GTID_PTR       ( &s_GtId )

#define ZH_GTWVT_GET( p )  ( ( PZH_GTWVT ) ZH_GTLOCAL( p ) )

static ZH_CRITICAL_NEW( s_wvtMtx );
#define ZH_WVT_LOCK()      zh_threadEnterCriticalSection( &s_wvtMtx )
#define ZH_WVT_UNLOCK()    zh_threadLeaveCriticalSection( &s_wvtMtx )


#if defined( _MSC_VER ) && ( _MSC_VER <= 1200 || defined( ZH_OS_WIN_CE ) ) && ! defined( ZH_ARCH_64BIT )
#  ifndef GetWindowLongPtr
#     define GetWindowLongPtr   GetWindowLong
#  endif
#  ifndef SetWindowLongPtr
#     define SetWindowLongPtr   SetWindowLong
#  endif
#  define ZH_GTWVT_LONG_PTR     LONG
#else
#  define ZH_GTWVT_LONG_PTR     LONG_PTR
#endif

#ifndef WS_OVERLAPPEDWINDOW
   #define WS_OVERLAPPEDWINDOW  ( WS_OVERLAPPED | WS_CAPTION | WS_SYSMENU | WS_THICKFRAME | WS_MINIMIZEBOX | WS_MAXIMIZEBOX )
#endif

#define _WVT_WS_DEF             ( WS_OVERLAPPEDWINDOW )
#define _WVT_WS_NORESIZE        ( WS_OVERLAPPEDWINDOW & ~( WS_THICKFRAME ) )
#define _WVT_WS_MAXED           ( WS_OVERLAPPEDWINDOW & ~( WS_MAXIMIZEBOX ) )

/* Left for testing to someone with multi monitor workspace on older platforms */
#if 0
#ifndef NO_MULTIMON
   #if WINVER < 0x0500
      #define COMPILE_MULTIMON_STUBS
      #include <multimon.h>
   #endif
#endif
#endif

#define ZH_KF_ALTGR             0x10

static PZH_GTWVT s_wvtWindows[ WVT_MAX_WINDOWS ];
static int       s_wvtCount = 0;

static const TCHAR s_szClassName[] = TEXT( "Ziher_WVT_Class" );

static LRESULT CALLBACK zh_gt_wvt_WndProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );
static ZH_BOOL zh_gt_wvt_FullScreen( PZH_GT pGT );
#if defined( UNICODE )
static void zh_gt_wvt_ResetBoxCharBitmaps( PZH_GTWVT pWVT );
#endif

static void zh_gt_wvt_RegisterClass( HINSTANCE hInstance )
{
   WNDCLASS wndclass;

   memset( &wndclass, 0, sizeof( wndclass ) );
   wndclass.style         = CS_HREDRAW | CS_VREDRAW | CS_DBLCLKS;
   wndclass.lpfnWndProc   = zh_gt_wvt_WndProc;
   wndclass.hInstance     = hInstance;
   wndclass.hCursor       = LoadCursor( NULL, IDC_ARROW );
#if 0
   wndclass.cbClsExtra    = 0;
   wndclass.cbWndExtra    = 0;
   wndclass.hIcon         = NULL;
   wndclass.hbrBackground = NULL;
   wndclass.lpszMenuName  = NULL;
#endif
   wndclass.lpszClassName = s_szClassName;

   if( ! RegisterClass( &wndclass ) )
   {
      if( GetLastError() != ERROR_CLASS_ALREADY_EXISTS )
         zh_errInternal( 10001, "Failed to register WVT window class", NULL, NULL );
   }
}

static PZH_GTWVT zh_gt_wvt_Find( HWND hWnd )
{
   int iCount = s_wvtCount, iPos = 0;
   PZH_GTWVT pWVT = NULL;

   ZH_WVT_LOCK();

   while( iCount && iPos < ( int ) ZH_SIZEOFARRAY( s_wvtWindows ) )
   {
      if( s_wvtWindows[ iPos ] )
      {
         if( s_wvtWindows[ iPos ]->hWnd == hWnd )
         {
            pWVT = s_wvtWindows[ iPos ];
            break;
         }
         --iCount;
      }
      ++iPos;
   }

   ZH_WVT_UNLOCK();

   return pWVT;
}

static ZH_BOOL zh_gt_wvt_Alloc( PZH_GTWVT pWVT )
{
   ZH_BOOL fOK = ZH_FALSE;

   ZH_WVT_LOCK();

   if( s_wvtCount < ( int ) ZH_SIZEOFARRAY( s_wvtWindows ) )
   {
      int iPos = 0;
      do
      {
         if( s_wvtWindows[ iPos ] == NULL )
         {
            s_wvtWindows[ iPos ] = pWVT;
            pWVT->iHandle = iPos;
            if( ++s_wvtCount == 1 )
               zh_gt_wvt_RegisterClass( pWVT->hInstance );
            fOK = ZH_TRUE;
            break;
         }
         ++iPos;
      }
      while( iPos < ( int ) ZH_SIZEOFARRAY( s_wvtWindows ) );
   }

   ZH_WVT_UNLOCK();

   return fOK;
}

static void zh_gt_wvt_Free( PZH_GTWVT pWVT )
{
   ZH_WVT_LOCK();

   s_wvtWindows[ pWVT->iHandle ] = NULL;

   if( --s_wvtCount == 0 )
   {
      if( pWVT->hInstance )
         UnregisterClass( s_szClassName, pWVT->hInstance );
   }

   ZH_WVT_UNLOCK();

   while( pWVT->pMenu )
   {
      PZH_GTWVT_MNU pMenu = pWVT->pMenu;

      pWVT->pMenu = pMenu->pNext;
      zh_strfree( pWVT->hSelectCopy );
      zh_xfree( pMenu );
   }

   if( pWVT->hSelectCopy )
      zh_strfree( pWVT->hSelectCopy );

   if( pWVT->hWindowTitle )
      zh_strfree( pWVT->hWindowTitle );

#if ! defined( UNICODE )
   if( pWVT->hFontBox && pWVT->hFontBox != pWVT->hFont )
      DeleteObject( pWVT->hFontBox );
#else
   if( pWVT->wcTrans )
      zh_itemFreeC( ( char * ) pWVT->wcTrans );

   zh_gt_wvt_ResetBoxCharBitmaps( pWVT );

   if( pWVT->hBmpDC )
      DeleteDC( pWVT->hBmpDC );
   if( pWVT->hPen )
      DeleteObject( pWVT->hPen );
   if( pWVT->hBrush )
      DeleteObject( pWVT->hBrush );
#endif
   if( pWVT->hFont )
      DeleteObject( pWVT->hFont );

   if( pWVT->hWnd )
      DestroyWindow( pWVT->hWnd );

   if( pWVT->hIconToFree )
      DestroyIcon( pWVT->hIconToFree );

   if( pWVT->TextLine )
      zh_xfree( pWVT->TextLine );

   if( pWVT->FixedSize )
      zh_xfree( pWVT->FixedSize );

   zh_xfree( pWVT );
}

static PZH_GTWVT zh_gt_wvt_New( PZH_GT pGT, HINSTANCE hInstance, int iCmdShow )
{
   PZH_GTWVT pWVT;

   pWVT = ( PZH_GTWVT ) zh_xgrabz( sizeof( ZH_GTWVT ) );

   pWVT->pGT               = pGT;

   if( ! zh_gt_wvt_Alloc( pWVT ) )
   {
      zh_xfree( pWVT );
      return NULL;
   }

   pWVT->hInstance         = hInstance;
   pWVT->iCmdShow          = iCmdShow;

   pWVT->ROWS              = WVT_DEFAULT_ROWS;
   pWVT->COLS              = WVT_DEFAULT_COLS;

   pWVT->TextLine          = ( TCHAR * ) zh_xgrab( pWVT->COLS * sizeof( TCHAR ) );
   pWVT->FixedSize         = ( int * ) zh_xgrab( pWVT->COLS * sizeof( int ) );

   pWVT->COLORS[ 0 ]       = BLACK;
   pWVT->COLORS[ 1 ]       = BLUE;
   pWVT->COLORS[ 2 ]       = GREEN;
   pWVT->COLORS[ 3 ]       = CYAN;
   pWVT->COLORS[ 4 ]       = RED;
   pWVT->COLORS[ 5 ]       = MAGENTA;
   pWVT->COLORS[ 6 ]       = BROWN;
   pWVT->COLORS[ 7 ]       = LIGHT_GRAY;
   pWVT->COLORS[ 8 ]       = GRAY;
   pWVT->COLORS[ 9 ]       = BRIGHT_BLUE;
   pWVT->COLORS[ 10 ]      = BRIGHT_GREEN;
   pWVT->COLORS[ 11 ]      = BRIGHT_CYAN;
   pWVT->COLORS[ 12 ]      = BRIGHT_RED;
   pWVT->COLORS[ 13 ]      = BRIGHT_MAGENTA;
   pWVT->COLORS[ 14 ]      = YELLOW;
   pWVT->COLORS[ 15 ]      = WHITE;

   /* These are the default font parameters, if not changed by user */
   pWVT->PTEXTSIZE.x       = WVT_DEFAULT_FONT_WIDTH;
   pWVT->PTEXTSIZE.y       = WVT_DEFAULT_FONT_HEIGHT;
   pWVT->fontWidth         = WVT_DEFAULT_FONT_WIDTH;
   pWVT->fontHeight        = WVT_DEFAULT_FONT_HEIGHT;
   pWVT->fontWeight        = FW_NORMAL;
   pWVT->fontQuality       = DEFAULT_QUALITY;
   pWVT->fontAttribute     = WVT_DEFAULT_FONT_ATTR;
   ZH_STRNCPY( pWVT->fontFace, WVT_DEFAULT_FONT_NAME, ZH_SIZEOFARRAY( pWVT->fontFace ) - 1 );

   pWVT->CaretExist        = ZH_FALSE;
   pWVT->CaretHidden       = ZH_TRUE;
   pWVT->CaretSize         = 0;
   pWVT->CaretWidth        = 0;
   pWVT->MousePos.x        = 0;
   pWVT->MousePos.y        = 0;
   pWVT->hWnd              = NULL;
   pWVT->keyPointerIn      = 0;
   pWVT->keyPointerOut     = 0;
   pWVT->keyLastPos        = 0;

   pWVT->CentreWindow      = ZH_TRUE;         /* Default is to always display window in centre of screen */
   pWVT->CodePage          = OEM_CHARSET;     /* GetACP(); - set code page to default system */
#if ! defined( UNICODE )
   pWVT->boxCodePage       = OEM_CHARSET;     /* GetACP(); - set code page to default system */
#else
   pWVT->wcTrans           = NULL;
   pWVT->wcTransLen        = 0;
#endif

   pWVT->Win9X             = zh_iswin9x();

   pWVT->IgnoreWM_SYSCHAR  = ZH_FALSE;

   pWVT->bMaximized        = ZH_FALSE;
   pWVT->bBeingMarked      = ZH_FALSE;
   pWVT->bBeginMarked      = ZH_FALSE;
   pWVT->bFullScreen       = ZH_FALSE;
   pWVT->bAltEnter         = ZH_FALSE;

   pWVT->MarginTop         = 0;
   pWVT->MarginLeft        = 0;

   pWVT->iNewPosX          = -1;
   pWVT->iNewPosY          = -1;

   pWVT->lpSelectCopy      = TEXT( "Mark and Copy" );
   pWVT->hSelectCopy       = NULL;
   pWVT->bSelectCopy       = ZH_TRUE;

   pWVT->pMenu             = NULL;

   {
      PZH_ITEM pItem = zh_itemPutCPtr( NULL, zh_cmdargBaseProgName() );

      pWVT->lpWindowTitle = ZH_ITEMGETSTR( pItem, &pWVT->hWindowTitle, NULL );
      zh_itemRelease( pItem );
   }

   pWVT->bResizable        = ZH_TRUE;
   pWVT->CloseMode         = 0;
   pWVT->ResizeMode        = ZH_GTI_RESIZEMODE_FONT;
   pWVT->bResizing         = ZH_FALSE;
   pWVT->bAlreadySizing    = ZH_FALSE;
   pWVT->bQuickEdit        = ZH_FALSE;

   pWVT->bComposited       = ZH_FALSE;

   return pWVT;
}

#if defined( UNICODE )

#define zh_bm_line( x1, y1, x2, y2 )      do { \
               MoveToEx( pWVT->hBmpDC, x1, y1, NULL ); \
               LineTo( pWVT->hBmpDC, x2, y2 ); \
               SetPixel( pWVT->hBmpDC, x2, y2, BLACK ); \
            } while( 0 )
#define zh_bm_point( x, y )         SetPixel( pWVT->hBmpDC, x, y, BLACK )
#define zh_bm_rect( x, y, w, h )    Rectangle( pWVT->hBmpDC, x, y, (x)+(w), (y)+(h) )
#define zh_bm_polygon( pts, n )     Polygon( pWVT->hBmpDC, pts, n )
#define zh_bm_invertrect( x, y, w, h )    do { \
               SetRect( &rc, 0, 0, cellx, celly ); \
               InvertRect( pWVT->hBmpDC, &rc ); \
            } while( 0 )
#define zh_bm_text( ch )                  do { \
               SetTextAlign( pWVT->hBmpDC, TA_LEFT ); \
               SetRect( &rc, 0, 0, cellx, celly ); \
               ExtTextOut( pWVT->hBmpDC, 0, 0, ETO_CLIPPED | ETO_OPAQUE, &rc, \
                           ch, 1, pWVT->FixedFont ? NULL : pWVT->FixedSize ); \
            } while( 0 )

static HBITMAP zh_gt_wvt_bitmap_char( PZH_GTWVT pWVT, int cellx, int celly )
{
   HBITMAP hBitMap = CreateBitmap( cellx + 1, celly + 1, 1, 1, NULL );
   HBRUSH hBrush;
   RECT rc;

   if( !pWVT->hBmpDC )
   {
      HDC hdc = GetDC( pWVT->hWnd );
      pWVT->hBmpDC = CreateCompatibleDC( hdc );
      ReleaseDC( pWVT->hWnd, hdc );
   }

   SelectObject( pWVT->hBmpDC, hBitMap );

   rc.left   = 0;
   rc.top    = 0;
   rc.right  = cellx + 1;
   rc.bottom = celly + 1;
   hBrush = CreateSolidBrush( GetBkColor( pWVT->hBmpDC ) );
   FillRect( pWVT->hBmpDC, &rc, hBrush );
   DeleteObject( hBrush );

   if( !pWVT->hPen )
   {
      pWVT->hPen = CreatePen( PS_SOLID, 0, BLACK );
      SelectObject( pWVT->hBmpDC, pWVT->hPen );
   }

   if( !pWVT->hBrush )
   {
      pWVT->hBrush = CreateSolidBrush( BLACK );
      SelectObject( pWVT->hBmpDC, pWVT->hBrush );
   }

   SelectObject( pWVT->hBmpDC, pWVT->hFont );

   return hBitMap;
}

static HBITMAP zh_gt_wvt_DefineBoxButtonL( PZH_GTWVT pWVT, int cellx, int celly )
{
   HBITMAP hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

   MoveToEx( pWVT->hBmpDC, cellx - 1, 0, NULL );
   LineTo( pWVT->hBmpDC, 0, 0 );
   LineTo( pWVT->hBmpDC, 0, celly - 1 );
   LineTo( pWVT->hBmpDC, cellx, celly - 1 );

   MoveToEx( pWVT->hBmpDC, 2, celly - 2, NULL );
   LineTo( pWVT->hBmpDC, cellx, celly - 2 );

   return hBitMap;
}

static HBITMAP zh_gt_wvt_DefineBoxButtonR( PZH_GTWVT pWVT, int cellx, int celly )
{
   HBITMAP hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

   MoveToEx( pWVT->hBmpDC, 0, 0, NULL );
   LineTo( pWVT->hBmpDC, cellx - 1, 0 );
   LineTo( pWVT->hBmpDC, cellx - 1, celly - 1 );
   LineTo( pWVT->hBmpDC, -1, celly - 1 );

   MoveToEx( pWVT->hBmpDC, cellx - 2, 3, NULL );
   LineTo( pWVT->hBmpDC, cellx - 2, celly - 2 );
   LineTo( pWVT->hBmpDC, -1, celly - 2 );

   return hBitMap;
}

static HBITMAP zh_gt_wvt_DefineBoxChar( PZH_GTWVT pWVT, ZH_USHORT usCh )
{
   HBITMAP hBitMap = NULL;
   int cellx = pWVT->PTEXTSIZE.x;
   int celly = pWVT->PTEXTSIZE.y;
   int i, y, x, yy, xx;
   POINT pts[ 3 ];
   RECT rc;

   if( usCh >= ZH_BOXCH_RC_MIN && usCh <= ZH_BOXCH_RC_MAX )
      switch( usCh )
      {
         case ZH_BOXCH_RC_ARROW_DL:
            hBitMap = zh_gt_wvt_DefineBoxButtonL( pWVT, cellx, celly );
            yy = celly / 2 - 1;
            for( y = celly - 4, x = cellx - 1; x >= 3 && y >= yy; --x, --y )
               zh_bm_line( x, y, cellx - 1, y );
            xx = ZH_MAX( cellx * 2 / 5, 3 ) | 1;
            for( x = cellx - xx / 2 - 1; y >= 3; --y )
               zh_bm_line( x, y, cellx - 1, y );
            break;

         case ZH_BOXCH_RC_ARROW_DR:
            hBitMap = zh_gt_wvt_DefineBoxButtonR( pWVT, cellx, celly );
            yy = ( celly + 1 ) / 2;
            for( y = celly - 5, x = 0; x < cellx - 4 && y >= yy; ++x, --y )
               zh_bm_line( 0, y, x, y );
            xx = ZH_MAX( cellx * 2 / 5, 3 ) | 1;
            for( x = xx / 2 - 1; y >= 3; --y )
               zh_bm_line( 0, y, x, y );
            break;

         case ZH_BOXCH_RC_ARROW_UL:
            hBitMap = zh_gt_wvt_DefineBoxButtonL( pWVT, cellx, celly );
            yy = ( celly + 1 ) / 2;
            for( y = 3, x = cellx - 1; x >= 3 && y <= yy; --x, ++y )
               zh_bm_line( x, y, cellx - 1, y );
            xx = ZH_MAX( cellx * 2 / 5, 3 ) | 1;
            for( x = cellx - xx / 2 - 1; y < celly - 3; ++y )
               zh_bm_line( x, y, cellx - 1, y );
            break;

         case ZH_BOXCH_RC_ARROW_UR:
            hBitMap = zh_gt_wvt_DefineBoxButtonR( pWVT, cellx, celly );
            yy = ( celly + 1 ) / 2;
            for( y = 4, x = 0; x < cellx - 4 && y <= yy; ++x, ++y )
               zh_bm_line( 0, y, x, y );
            xx = ZH_MAX( cellx * 2 / 5, 3 ) | 1;
            for( x = xx / 2 - 1; y < celly - 3; ++y )
               zh_bm_line( 0, y, x, y );
            break;

         case ZH_BOXCH_RC_ARROW_VL:
            hBitMap = zh_gt_wvt_DefineBoxButtonL( pWVT, cellx, celly );
            yy = ( celly - 1 ) / 2;
            for( y = 3, x = cellx - 1; x >= 3 && y < yy; --x, ++y )
               zh_bm_line( x, y, cellx - 1, y );
            for( y = yy + 2, ++x; x <= cellx - 1 && y < celly - 3; ++x, ++y )
               zh_bm_line( x, y, cellx - 1, y );
            break;

         case ZH_BOXCH_RC_ARROW_VR:
            hBitMap = zh_gt_wvt_DefineBoxButtonR( pWVT, cellx, celly );
            yy = ( celly - 1 ) / 2;
            for( y = 4, x = 0; x < cellx - 4 && y < yy; ++x, ++y )
               zh_bm_line( 0, y, x, y );
            for( y = yy + 2, --x; x >= 0 && y < celly - 3; --x, ++y )
               zh_bm_line( 0, y, x, y );
            break;

         case ZH_BOXCH_RC_BUTTON_L:
            hBitMap = zh_gt_wvt_DefineBoxButtonL( pWVT, cellx, celly );
            break;

         case ZH_BOXCH_RC_BUTTON_R:
            hBitMap = zh_gt_wvt_DefineBoxButtonR( pWVT, cellx, celly );
            break;

         case ZH_BOXCH_RC_ARROW_LL:
            hBitMap = zh_gt_wvt_DefineBoxButtonL( pWVT, cellx, celly );
            yy = ( celly - 1 ) / 2;
            for( x = 3, y = 0; x < cellx; ++x, ++y )
               zh_bm_line( x, yy - y, x, yy + y );
            break;

         case ZH_BOXCH_RC_ARROW_LR:
            hBitMap = zh_gt_wvt_DefineBoxButtonR( pWVT, cellx, celly );
            yy = ZH_MAX( celly / 5, 3 ) | 1;
            for( y = ( celly - yy ) / 2; yy--; ++y )
               zh_bm_line( 0, y, cellx - 4, y );
            break;

         case ZH_BOXCH_RC_ARROW_RL:
            hBitMap = zh_gt_wvt_DefineBoxButtonL( pWVT, cellx, celly );
            yy = ZH_MAX( celly / 5, 3 ) | 1;
            for( y = ( celly - yy ) / 2; yy--; ++y )
               zh_bm_line( 3, y, cellx - 1, y );
            break;

         case ZH_BOXCH_RC_ARROW_RR:
            hBitMap = zh_gt_wvt_DefineBoxButtonR( pWVT, cellx, celly );
            yy = ( celly - 1 ) / 2;
            for( x = cellx - 4, y = 0; x >= 0; --x, ++y )
               zh_bm_line( x, yy - y, x, yy + y );
            break;

         case ZH_BOXCH_RC_ENTER1:
            /* TODO */
            break;
         case ZH_BOXCH_RC_ENTER2:
            /* TODO */
            break;
         case ZH_BOXCH_RC_ENTER3:
            /* TODO */
            break;

         case ZH_BOXCH_RC_VSCRL_LD:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( 0, 0, 0, celly - 1 );
            zh_bm_line( 0, celly / 2, cellx - 1, celly / 2 );
            zh_bm_line( 2, celly / 2 - 1, cellx - 1, celly / 2 - 1 );

            for( y = celly / 2 + 1; y < celly; y++ )
            {
               for( x = ( y & 1 ) + 2; x < cellx; x += 2 )
                  zh_bm_point( x, y );
            }
            break;

         case ZH_BOXCH_RC_VSCRL_RD:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( cellx - 1, 0, cellx - 1, celly - 1 );
            zh_bm_line( cellx - 2, 0, cellx - 2, celly / 2 - 1 );
            zh_bm_line( 0, celly / 2, cellx - 1, celly / 2 );
            zh_bm_line( 0, celly / 2 - 1, cellx - 1, celly / 2 - 1 );

            for( y = celly / 2 + 1; y < celly; y++ )
            {
               for( x = ( y ^ cellx ) & 1; x < cellx - 2; x += 2 )
                  zh_bm_point( x, y );
            }
            break;

         case ZH_BOXCH_RC_VSCRL_LU:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( 0, 0, 0, celly - 1 );
            zh_bm_line( 0, celly / 2, cellx - 1, celly / 2 );

            for( y = 0; y < celly / 2; y++ )
            {
               for( x = ( y & 1 ) + 2; x < cellx; x += 2 )
                  zh_bm_point( x, y );
            }
            break;

         case ZH_BOXCH_RC_VSCRL_RU:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( cellx - 1, 0, cellx - 1, celly - 1 );
            zh_bm_line( 0, celly / 2, cellx - 1, celly / 2 );
            zh_bm_line( cellx - 2, celly / 2 + 3, cellx - 2, celly - 1 );

            for( y = 0; y < celly / 2; y++ )
            {
               for( x = ( y ^ cellx ) & 1; x < cellx - 2; x += 2 )
                  zh_bm_point( x, y );
            }
            break;

         case ZH_BOXCH_RC_VSCRL_L:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( 0, 0, 0, celly - 1 );

            for( y = 0; y < celly; y++ )
            {
               for( x = ( y & 1 ) + 2; x < cellx; x += 2 )
                  zh_bm_point( x, y );
            }
            break;

         case ZH_BOXCH_RC_VSCRL_R:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( cellx - 1, 0, cellx - 1, celly - 1 );

            for( y = 0; y < celly; y++ )
            {
               for( x = ( y ^ cellx ) & 1; x < cellx - 2; x += 2 )
                  zh_bm_point( x, y );
            }
            break;

         case ZH_BOXCH_RC_HSCRL:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( 0, 0, cellx - 1, 0 );
            zh_bm_line( 0, celly - 1, cellx - 1, celly - 1 );

            for( y = 2; y < celly - 2; y++ )
            {
               for( x = y & 1; x < cellx; x += 2 )
                  zh_bm_point( x, y );
            }
            break;

         case ZH_BOXCH_RC_0:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );
            zh_bm_text( TEXT( "0" ) );
            break;

         case ZH_BOXCH_RC_1:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );
            zh_bm_text( TEXT( "1" ) );
            break;

         case ZH_BOXCH_RC_2:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );
            zh_bm_text( TEXT( "2" ) );
            break;

         case ZH_BOXCH_RC_3:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );
            zh_bm_text( TEXT( "3" ) );
            break;

         case ZH_BOXCH_RC_4:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );
            zh_bm_text( TEXT( "4" ) );
            break;

         case ZH_BOXCH_RC_5:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );
            zh_bm_text( TEXT( "5" ) );
            break;

         case ZH_BOXCH_RC_6:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );
            zh_bm_text( TEXT( "6" ) );
            break;

         case ZH_BOXCH_RC_7:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );
            zh_bm_text( TEXT( "7" ) );
            break;

         case ZH_BOXCH_RC_8:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );
            zh_bm_text( TEXT( "8" ) );
            break;

         case ZH_BOXCH_RC_9:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );
            zh_bm_text( TEXT( "9" ) );
            break;

         case ZH_BOXCH_RC_DOT:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );
            zh_bm_text( TEXT( "." ) );
            break;

         case ZH_BOXCH_RC_ACC:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );
            zh_bm_text( TEXT( "'" ) );
            break;

         case ZH_BOXCH_RC_BOX_ML:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( 0, celly / 2, cellx - 1, celly / 2 );
            zh_bm_line( 0, 0, 0, celly - 1 );
            break;

         case ZH_BOXCH_RC_BOX_MR:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( 0, celly / 2, cellx - 1, celly / 2 );
            zh_bm_line( cellx - 1, 0, cellx - 1, celly - 1 );
            break;

         case ZH_BOXCH_RC_HWND_L:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( cellx - 1, 0, 0, 0 );
            zh_bm_line( 0, 0, 0, celly - 1 );
            zh_bm_line( 0, celly - 1, cellx - 1, celly - 1 );
            zh_bm_line( cellx - 1, celly / 4 + 2, cellx / 4 + 1, celly / 4 + 2 );
            zh_bm_line( cellx / 4 + 1, celly / 4 + 2, cellx / 4 + 1, celly - 4 - celly / 4 );
            zh_bm_line( cellx / 4 + 1, celly - 4 - celly / 4, cellx - 1, celly - 4 - celly / 4 );
            zh_bm_line( cellx / 4 + 2, celly - 3 - celly / 4, cellx - 1, celly - 3 - celly / 4 );
            break;

         case ZH_BOXCH_RC_HWND_R:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( 0, 0, cellx - 1, 0 );
            zh_bm_line( cellx - 1, 0, cellx - 1, celly - 1 );
            zh_bm_line( cellx - 1, celly - 1, 0, celly - 1 );
            zh_bm_line( 0, celly / 4 + 2, cellx - cellx / 4 - 2, celly / 4 + 2 );
            zh_bm_line( cellx - cellx / 4 - 2, celly / 4 + 2, cellx - cellx / 4 - 2, celly - 4 - celly / 4 );
            zh_bm_line( cellx - cellx / 4 - 2, celly - 4 - celly / 4, 0, celly - 4 - celly / 4 );
            zh_bm_line( 0, celly - 3 - celly / 4, cellx - cellx / 4 - 1, celly - 3 - celly / 4 );
            zh_bm_line( cellx - cellx / 4 - 1, celly - 3 - celly / 4, cellx - cellx / 4 - 1, celly / 4 + 2 );
            break;

         case ZH_BOXCH_RC_BOX_TL:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( 0, 0, cellx - 1, 0 );
            zh_bm_line( 0, 0, 0, celly - 1 );
            break;

         case ZH_BOXCH_RC_BOX_T:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( 0, 0, cellx - 1, 0 );
            break;

         case ZH_BOXCH_RC_BOX_TR:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( 0, 0, cellx - 1, 0 );
            zh_bm_line( cellx - 1, 0, cellx - 1, celly - 1 );
            break;

         case ZH_BOXCH_RC_BOX_R:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( cellx - 1, 0, cellx - 1, celly - 1 );
            break;

         case ZH_BOXCH_RC_BOX_BR:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( cellx - 1, 0, cellx - 1, celly - 1 );
            zh_bm_line( 0, celly - 1, cellx - 1, celly - 1 );
            break;

         case ZH_BOXCH_RC_BOX_B:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( 0, celly - 1, cellx - 1, celly - 1 );
            break;

         case ZH_BOXCH_RC_BOX_BL:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( 0, 0, 0, celly - 1 );
            zh_bm_line( 0, celly - 1, cellx - 1, celly - 1 );
            break;

         case ZH_BOXCH_RC_BOX_L:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( 0, 0, 0, celly - 1 );
            break;

         case ZH_BOXCH_RC_BOX_MT:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( cellx / 2, 0, cellx / 2, celly - 1 );
            zh_bm_line( 0, 0, cellx - 1, 0 );
            break;

         case ZH_BOXCH_RC_BOX_MB:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( cellx / 2, 0, cellx / 2, celly - 1 );
            zh_bm_line( 0, celly - 1, cellx - 1, celly - 1 );
            break;

         case ZH_BOXCH_RC_BUTTON_CL:
            hBitMap = zh_gt_wvt_DefineBoxButtonL( pWVT, cellx, celly );
            yy = celly - 2 / 3;
            xx = cellx - 4;
            if( yy > xx )
               yy = xx;
            xx = ( xx * 2 + 1 ) / 3;
            if( xx < 2 )
               xx = 2;
            for( y = celly - yy - 3 - xx, i = 0; i < xx; ++y, ++i )
               zh_bm_line( 3, y, 3 + yy - 1, y + yy - 1 );
            y = celly - 5 - xx;
            zh_bm_line( cellx - 1, y, cellx - 1, y + xx - 1 );
            break;

         case ZH_BOXCH_RC_BUTTON_CR:
            hBitMap = zh_gt_wvt_DefineBoxButtonR( pWVT, cellx, celly );
            yy = celly - 2 / 3;
            xx = cellx - 4;
            if( yy > xx )
               yy = xx;
            xx = ( xx * 2 + 1 ) / 3;
            if( xx < 2 )
               xx = 2;
            for( y = celly - 6 - xx, i = 0; i < xx; ++y, ++i )
               zh_bm_line( 0, y, yy, y - yy );
            break;

         case ZH_BOXCH_RC_FARROW_DL:
            hBitMap = zh_gt_wvt_DefineBoxButtonL( pWVT, cellx, celly );
            yy = ( celly - cellx ) / 2 + 1;
            yy = ZH_MAX( yy, 2 );
            for( y = celly - yy - 1, x = cellx - 1; x >= 2 && y >= 3; --x, --y )
               zh_bm_line( x, y, cellx - 1, y );
            break;

         case ZH_BOXCH_RC_FARROW_DR:
            hBitMap = zh_gt_wvt_DefineBoxButtonR( pWVT, cellx, celly );
            yy = ( celly - cellx ) / 2 + 1;
            yy = ZH_MAX( yy, 2 );
            for( y = celly - yy - 2, x = 0; x < cellx - 3 && y >= 3; ++x, --y )
               zh_bm_line( 0, y, x, y );
            break;

         case ZH_BOXCH_RC_DOTS:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            for( x = 1; x < cellx; x += 2 )
               zh_bm_point( x, celly / 2 );
            break;

         case ZH_BOXCH_RC_DOTS_L:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            i = cellx / 2;
            xx = i - i / 2;
            yy = ZH_MAX( 2, xx - 1 );

            zh_bm_rect( cellx - xx / 2 - i, celly / 3 * 2, xx    , yy );
            zh_bm_rect( cellx - xx / 2    , celly / 3 * 2, xx / 2, yy );
            break;

         case ZH_BOXCH_RC_DOTS_R:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            i = cellx / 2;
            xx = i - i / 2;
            yy = ZH_MAX( 2, xx - 1 );

            zh_bm_rect( 0         , celly / 3 * 2, xx - xx / 2, yy );
            zh_bm_rect( i - xx / 2, celly / 3 * 2, xx         , yy );
            break;
      }
   else
      switch( usCh )
      {
         case ZH_BOXCH_FILLER1:
         case ZH_BOXCH_FILLER2:
         case ZH_BOXCH_FILLER3:
         {
            int skip, start, mod;

            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            if( usCh == ZH_BOXCH_FILLER1 )
            {
               skip = 4;
               start = mod = 1;
            }
            else if( usCh == ZH_BOXCH_FILLER2 )
            {
               skip = 2;
               start = 0;
               mod = 1;
            }
            else
            {
               skip = 4;
               start = mod = 0;
            }
            for( y = 0; y < celly; y++ )
            {
               for( x = start + ( skip >> 1 ) * ( ( y & 1 ) ^ mod ); x < cellx; x += skip )
                  zh_bm_point( x, y );
            }
            if( usCh == ZH_BOXCH_FILLER3 )
               zh_bm_invertrect( 0, 0, cellx, celly );
            break;
         }
         case ZH_BOXCH_ARROW_R:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            i = ZH_MIN( ( celly >> 1 ), cellx ) - 3;
            pts[ 0 ].x = ( ( cellx - i ) >> 1 );
            pts[ 0 ].y = ( celly >> 1 ) - i;
            pts[ 1 ].x = pts[ 0 ].x + i;
            pts[ 1 ].y = pts[ 0 ].y + i;
            pts[ 2 ].x = pts[ 1 ].x - i;
            pts[ 2 ].y = pts[ 1 ].y + i;
            zh_bm_polygon( pts, 3 );
            break;

         case ZH_BOXCH_ARROW_L:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            i = ZH_MIN( ( celly >> 1 ), cellx ) - 3;
            pts[ 0 ].x = ( ( cellx - i ) >> 1 ) + i;
            pts[ 0 ].y = ( celly >> 1 ) - i;
            pts[ 1 ].x = pts[ 0 ].x - i;
            pts[ 1 ].y = pts[ 0 ].y + i;
            pts[ 2 ].x = pts[ 1 ].x + i;
            pts[ 2 ].y = pts[ 1 ].y + i;
            zh_bm_polygon( pts, 3 );
            break;

         case ZH_BOXCH_ARROW_U:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            i = ZH_MIN( celly, cellx >> 1 );
            pts[ 0 ].x = ( cellx >> 1 ) - i;
            pts[ 0 ].y = ( ( celly - i ) >> 1 ) + i;
            pts[ 1 ].x = pts[ 0 ].x + i;
            pts[ 1 ].y = pts[ 0 ].y - i;
            pts[ 2 ].x = pts[ 1 ].x + i;
            pts[ 2 ].y = pts[ 1 ].y + i;
            zh_bm_polygon( pts, 3 );
            break;

         case ZH_BOXCH_ARROW_D:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            i = ZH_MIN( celly, cellx >> 1 );
            pts[ 0 ].x = ( cellx >> 1 ) - i;
            pts[ 0 ].y = ( ( celly - i ) >> 1 );
            pts[ 1 ].x = pts[ 0 ].x + i;
            pts[ 1 ].y = pts[ 0 ].y + i;
            pts[ 2 ].x = pts[ 1 ].x + i;
            pts[ 2 ].y = pts[ 1 ].y - i;
            zh_bm_polygon( pts, 3 );
            break;

         case ZH_BOXCH_FULL:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_rect( 0, 0, cellx, celly );
            break;

         case ZH_BOXCH_FULL_B:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_rect( 0, celly / 2 + 1, cellx, ( celly + 1 ) / 2 );
            break;

         case ZH_BOXCH_FULL_T:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_rect( 0, 0, cellx, celly / 2 );
            break;

         case ZH_BOXCH_FULL_R:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_rect( cellx / 2 + 1, 0, ( cellx + 1 ) / 2, celly );
            break;

         case ZH_BOXCH_FULL_L:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_rect( 0, 0, cellx / 2, celly );
            break;

         case ZH_BOXCH_SNG_LT:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( cellx / 2, celly - 1, cellx / 2, celly / 2 );
            zh_bm_line( cellx / 2, celly / 2, cellx - 1, celly / 2 );
            break;

         case ZH_BOXCH_SNG_TD:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( 0, celly / 2, cellx - 1, celly / 2 );
            zh_bm_line( cellx / 2, celly / 2, cellx / 2, celly - 1 );
            break;

         case ZH_BOXCH_SNG_RT:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( cellx / 2, celly - 1, cellx / 2, celly / 2 );
            zh_bm_line( cellx / 2, celly / 2, 0, celly / 2 );
            break;

         case ZH_BOXCH_SNG_LB:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( cellx / 2, 0, cellx / 2, celly / 2 );
            zh_bm_line( cellx / 2, celly / 2, cellx - 1, celly / 2 );
            break;

         case ZH_BOXCH_SNG_BU:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( cellx / 2, 0, cellx / 2, celly / 2 );
            zh_bm_line( 0, celly / 2, cellx - 1, celly / 2 );
            break;

         case ZH_BOXCH_SNG_RB:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( cellx / 2, 0, cellx / 2, celly / 2 );
            zh_bm_line( cellx / 2, celly / 2, 0, celly / 2 );
            break;

         case ZH_BOXCH_SNG_VL:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( cellx / 2, 0, cellx / 2, celly - 1 );
            zh_bm_line( cellx / 2, celly / 2, cellx - 1, celly / 2 );
            break;

         case ZH_BOXCH_SNG_VR:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( cellx / 2, 0, cellx / 2, celly - 1 );
            zh_bm_line( cellx / 2, celly / 2, 0, celly / 2 );
            break;

         case ZH_BOXCH_SNG_CRS:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( cellx / 2, 0, cellx / 2, celly - 1 );
            zh_bm_line( 0, celly / 2, cellx - 1, celly / 2 );
            break;

         case ZH_BOXCH_SNG_HOR:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( 0, celly / 2, cellx - 1, celly / 2 );
            break;

         case ZH_BOXCH_SNG_VRT:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( cellx / 2, 0, cellx / 2, celly - 1 );
            break;

         case ZH_BOXCH_DBL_LT:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( cellx / 2 - 1, celly - 1, cellx / 2 - 1, celly / 2 - 1 );
            zh_bm_line( cellx / 2 - 1, celly / 2 - 1, cellx - 1, celly / 2 - 1 );
            zh_bm_line( cellx / 2 + 1, celly - 1, cellx / 2 + 1, celly / 2 + 1 );
            zh_bm_line( cellx / 2 + 1, celly / 2 + 1, cellx - 1, celly / 2 + 1 );
            break;

         case ZH_BOXCH_DBL_TD:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( 0, celly / 2 - 1, cellx - 1, celly / 2 - 1 );
            zh_bm_line( 0, celly / 2 + 1, cellx / 2 - 1, celly / 2 + 1 );
            zh_bm_line( cellx / 2 - 1, celly / 2 + 1, cellx / 2 - 1, celly - 1 );
            zh_bm_line( cellx / 2 + 1, celly / 2 + 1, cellx - 1, celly / 2 + 1 );
            zh_bm_line( cellx / 2 + 1, celly / 2 + 1, cellx / 2 + 1, celly - 1 );
            break;

         case ZH_BOXCH_DBL_RT:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( cellx / 2 - 1, celly - 1, cellx / 2 - 1, celly / 2 + 1 );
            zh_bm_line( cellx / 2 - 1, celly / 2 + 1, 0, celly / 2 + 1 );
            zh_bm_line( cellx / 2 + 1, celly - 1, cellx / 2 + 1, celly / 2 - 1 );
            zh_bm_line( cellx / 2 + 1, celly / 2 - 1, 0, celly / 2 - 1 );
            break;

         case ZH_BOXCH_DBL_LB:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( cellx / 2 - 1, 0, cellx / 2 - 1, celly / 2 + 1 );
            zh_bm_line( cellx / 2 - 1, celly / 2 + 1, cellx - 1, celly / 2 + 1 );
            zh_bm_line( cellx / 2 + 1, 0, cellx / 2 + 1, celly / 2 - 1 );
            zh_bm_line( cellx / 2 + 1, celly / 2 - 1, cellx - 1, celly / 2 - 1 );
            break;

         case ZH_BOXCH_DBL_BU:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( 0, celly / 2 + 1, cellx - 1, celly / 2 + 1 );
            zh_bm_line( 0, celly / 2 - 1, cellx / 2 - 1, celly / 2 - 1 );
            zh_bm_line( cellx / 2 + 1, celly / 2 - 1, cellx - 1, celly / 2 - 1 );
            zh_bm_line( cellx / 2 - 1, celly / 2 - 1, cellx / 2 - 1, 0 );
            zh_bm_line( cellx / 2 + 1, celly / 2 - 1, cellx / 2 + 1, 0 );
            break;

         case ZH_BOXCH_DBL_RB:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( cellx / 2 - 1, 0, cellx / 2 - 1, celly / 2 - 1 );
            zh_bm_line( cellx / 2 - 1, celly / 2 - 1, 0, celly / 2 - 1 );
            zh_bm_line( cellx / 2 + 1, 0, cellx / 2 + 1, celly / 2 + 1 );
            zh_bm_line( cellx / 2 + 1, celly / 2 + 1, 0, celly / 2 + 1 );
            break;

         case ZH_BOXCH_DBL_VL:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( cellx / 2 - 1, 0, cellx / 2 - 1, celly - 1 );
            zh_bm_line( cellx / 2 + 1, 0, cellx / 2 + 1, celly / 2 - 1 );
            zh_bm_line( cellx / 2 + 1, celly / 2 + 1, cellx / 2 + 1, celly - 1 );
            zh_bm_line( cellx / 2 + 1, celly / 2 - 1, cellx - 1, celly / 2 - 1 );
            zh_bm_line( cellx / 2 + 1, celly / 2 + 1, cellx - 1, celly / 2 + 1 );
            break;

         case ZH_BOXCH_DBL_VR:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( cellx / 2 + 1, 0, cellx / 2 + 1, celly - 1 );
            zh_bm_line( cellx / 2 - 1, 0, cellx / 2 - 1, celly / 2 - 1 );
            zh_bm_line( cellx / 2 - 1, celly / 2 + 1, cellx / 2 - 1, celly - 1 );
            zh_bm_line( cellx / 2 - 1, celly / 2 - 1, 0, celly / 2 - 1 );
            zh_bm_line( cellx / 2 - 1, celly / 2 + 1, 0, celly / 2 + 1 );
            break;

         case ZH_BOXCH_DBL_CRS:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( cellx / 2 - 1, 0, cellx / 2 - 1, celly / 2 - 1 );
            zh_bm_line( cellx / 2 - 1, celly / 2 + 1, cellx / 2 - 1, celly - 1 );
            zh_bm_line( cellx / 2 - 1, celly / 2 - 1, 0, celly / 2 - 1 );
            zh_bm_line( cellx / 2 - 1, celly / 2 + 1, 0, celly / 2 + 1 );
            zh_bm_line( cellx / 2 + 1, 0, cellx / 2 + 1, celly / 2 - 1 );
            zh_bm_line( cellx / 2 + 1, celly / 2 + 1, cellx / 2 + 1, celly - 1 );
            zh_bm_line( cellx / 2 + 1, celly / 2 - 1, cellx - 1, celly / 2 - 1 );
            zh_bm_line( cellx / 2 + 1, celly / 2 + 1, cellx - 1, celly / 2 + 1 );
            break;

         case ZH_BOXCH_DBL_HOR:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( 0, celly / 2 - 1, cellx - 1, celly / 2 - 1 );
            zh_bm_line( 0, celly / 2 + 1, cellx - 1, celly / 2 + 1 );
            break;

         case ZH_BOXCH_DBL_VRT:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( cellx / 2 - 1, 0, cellx / 2 - 1, celly - 1 );
            zh_bm_line( cellx / 2 + 1, 0, cellx / 2 + 1, celly - 1 );
            break;

         case ZH_BOXCH_SNG_L_DBL_T:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( cellx / 2, celly / 2 - 1, cellx - 1, celly / 2 - 1 );
            zh_bm_line( cellx / 2, celly / 2 + 1, cellx - 1, celly / 2 + 1 );
            zh_bm_line( cellx / 2, celly / 2 - 1, cellx / 2, celly - 1 );
            break;

         case ZH_BOXCH_SNG_T_DBL_D:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( 0, celly / 2, cellx - 1, celly / 2 );
            zh_bm_line( cellx / 2 - 1, celly / 2, cellx / 2 - 1, celly - 1 );
            zh_bm_line( cellx / 2 + 1, celly / 2, cellx / 2 + 1, celly - 1 );
            break;

         case ZH_BOXCH_SNG_R_DBL_T:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( 0, celly / 2, cellx / 2 + 1, celly / 2 );
            zh_bm_line( cellx / 2 - 1, celly / 2, cellx / 2 - 1, celly - 1 );
            zh_bm_line( cellx / 2 + 1, celly / 2, cellx / 2 + 1, celly - 1 );
            break;

         case ZH_BOXCH_SNG_L_DBL_B:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( cellx / 2, celly / 2 - 1, cellx - 1, celly / 2 - 1 );
            zh_bm_line( cellx / 2, celly / 2 + 1, cellx - 1, celly / 2 + 1 );
            zh_bm_line( cellx / 2, 0, cellx / 2, celly / 2 + 1 );
            break;

         case ZH_BOXCH_SNG_B_DBL_U:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( 0, celly / 2, cellx - 1, celly / 2 );
            zh_bm_line( cellx / 2 - 1, 0, cellx / 2 - 1, celly / 2 );
            zh_bm_line( cellx / 2 + 1, 0, cellx / 2 + 1, celly / 2 );
            break;

         case ZH_BOXCH_SNG_R_DBL_B:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( 0, celly / 2, cellx / 2 + 1, celly / 2 );
            zh_bm_line( cellx / 2 - 1, 0, cellx / 2 - 1, celly / 2 );
            zh_bm_line( cellx / 2 + 1, 0, cellx / 2 + 1, celly / 2 );
            break;

         case ZH_BOXCH_SNG_V_DBL_L:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( cellx / 2, 0, cellx / 2, celly - 1 );
            zh_bm_line( cellx / 2, celly / 2 - 1, cellx - 1, celly / 2 - 1 );
            zh_bm_line( cellx / 2, celly / 2 + 1, cellx - 1, celly / 2 + 1 );
            break;

         case ZH_BOXCH_SNG_V_DBL_R:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( cellx / 2, 0, cellx / 2, celly - 1 );
            zh_bm_line( 0, celly / 2 - 1, cellx / 2, celly / 2 - 1 );
            zh_bm_line( 0, celly / 2 + 1, cellx / 2, celly / 2 + 1 );
            break;

         case ZH_BOXCH_SNG_DBL_CRS:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( cellx / 2, 0, cellx / 2, celly - 1 );
            zh_bm_line( 0, celly / 2 - 1, cellx - 1, celly / 2 - 1 );
            zh_bm_line( 0, celly / 2 + 1, cellx - 1, celly / 2 + 1 );
            break;

         case ZH_BOXCH_DBL_L_SNG_T:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( cellx / 2 - 1, celly / 2, cellx / 2 - 1, celly - 1 );
            zh_bm_line( cellx / 2 + 1, celly / 2, cellx / 2 + 1, celly - 1 );
            zh_bm_line( cellx / 2 - 1, celly / 2, cellx - 1, celly / 2 );
            break;

         case ZH_BOXCH_DBL_T_SNG_D:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( 0, celly / 2 - 1, cellx - 1, celly / 2 - 1 );
            zh_bm_line( 0, celly / 2 + 1, cellx - 1, celly / 2 + 1 );
            zh_bm_line( cellx / 2, celly / 2 + 1, cellx / 2, celly - 1 );
            break;

         case ZH_BOXCH_DBL_R_SNG_T:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( 0, celly / 2 - 1, cellx / 2, celly / 2 - 1 );
            zh_bm_line( 0, celly / 2 + 1, cellx / 2, celly / 2 + 1 );
            zh_bm_line( cellx / 2, celly / 2 - 1, cellx / 2, celly - 1 );
            break;

         case ZH_BOXCH_DBL_L_SNG_B:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( cellx / 2 - 1, 0, cellx / 2 - 1, celly / 2 );
            zh_bm_line( cellx / 2 + 1, 0, cellx / 2 + 1, celly / 2 );
            zh_bm_line( cellx / 2 - 1, celly / 2, cellx - 1, celly / 2 );
            break;

         case ZH_BOXCH_DBL_B_SNG_U:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( 0, celly / 2 - 1, cellx - 1, celly / 2 - 1 );
            zh_bm_line( 0, celly / 2 + 1, cellx - 1, celly / 2 + 1 );
            zh_bm_line( cellx / 2, 0, cellx / 2, celly / 2 - 1 );
            break;

         case ZH_BOXCH_DBL_R_SNG_B:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( 0, celly / 2 - 1, cellx / 2, celly / 2 - 1 );
            zh_bm_line( 0, celly / 2 + 1, cellx / 2, celly / 2 + 1 );
            zh_bm_line( cellx / 2, 0, cellx / 2, celly / 2 + 1 );
            break;

         case ZH_BOXCH_DBL_V_SNG_L:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( cellx / 2 - 1, 0, cellx / 2 - 1, celly - 1 );
            zh_bm_line( cellx / 2 + 1, 0, cellx / 2 + 1, celly - 1 );
            zh_bm_line( cellx / 2 + 1, celly / 2, cellx - 1, celly / 2 );
            break;

         case ZH_BOXCH_DBL_V_SNG_R:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( cellx / 2 - 1, 0, cellx / 2 - 1, celly - 1 );
            zh_bm_line( cellx / 2 + 1, 0, cellx / 2 + 1, celly - 1 );
            zh_bm_line( 0, celly / 2, cellx / 2 - 1, celly / 2 );
            break;

         case ZH_BOXCH_DBL_SNG_CRS:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            zh_bm_line( cellx / 2 - 1, 0, cellx / 2 - 1, celly - 1 );
            zh_bm_line( cellx / 2 + 1, 0, cellx / 2 + 1, celly - 1 );
            zh_bm_line( 0, celly / 2, cellx - 1, celly / 2 );
            break;

         case ZH_BOXCH_SQUARE:
            hBitMap = zh_gt_wvt_bitmap_char( pWVT, cellx, celly );

            xx = yy = cellx - ZH_MAX( cellx >> 2, 2 );
            zh_bm_rect( ( cellx - xx ) >> 1, ( celly - yy ) >> 1, xx, yy );
            break;
      }

   return hBitMap;
}

/* *********************************************************************** */

#if defined( UNICODE )
static void zh_gt_wvt_ResetBoxCharBitmaps( PZH_GTWVT pWVT )
{
   int i;

   for( i = 1; i <= pWVT->boxCount; i++ )
      DeleteObject( pWVT->boxImage[ i ] );

   memset( pWVT->boxImage, 0, sizeof( pWVT->boxImage ) );
   pWVT->boxCount = 0;

   for( i = 0; i < ZH_BOXCH_TRANS_COUNT; ++i )
      pWVT->boxIndex[ i ] = ZH_BOXCH_TRANS_MAX;
}
#endif

/* *********************************************************************** */

static HBITMAP zh_gt_wvt_GetBoxChar( PZH_GTWVT pWVT, ZH_USHORT * puc16 )
{
   ZH_USHORT uc16 = *puc16;
   int iPos, iTrans;

   if( ( pWVT->fontAttribute & ZH_GTI_FONTA_DRAWBOX ) == 0 )
      return NULL;

   if( uc16 >= ZH_BOXCH_RC_0 && uc16 <= ZH_BOXCH_RC_ACC )
   {
      switch( uc16 )
      {
         case ZH_BOXCH_RC_0:
            *puc16 = '0';
            break;
         case ZH_BOXCH_RC_1:
            *puc16 = '1';
            break;
         case ZH_BOXCH_RC_2:
            *puc16 = '2';
            break;
         case ZH_BOXCH_RC_3:
            *puc16 = '3';
            break;
         case ZH_BOXCH_RC_4:
            *puc16 = '4';
            break;
         case ZH_BOXCH_RC_5:
            *puc16 = '5';
            break;
         case ZH_BOXCH_RC_6:
            *puc16 = '6';
            break;
         case ZH_BOXCH_RC_7:
            *puc16 = '7';
            break;
         case ZH_BOXCH_RC_8:
            *puc16 = '8';
            break;
         case ZH_BOXCH_RC_9:
            *puc16 = '9';
            break;
         case ZH_BOXCH_RC_DOT:
            *puc16 = '.';
            break;
         case ZH_BOXCH_RC_ACC:
            *puc16 = '\'';
            break;
      }
      return NULL;
   }

   if     ( uc16 == ZH_BOXCH_ARROW_R )
      iPos = 0;
   else if( uc16 == ZH_BOXCH_ARROW_L )
      iPos = 1;
   else if( uc16 == ZH_BOXCH_ARROW_U )
      iPos = 2;
   else if( uc16 == ZH_BOXCH_ARROW_D )
      iPos = 3;
   else if( uc16 >= ZH_BOXCH_BOX_MIN && uc16 <= ZH_BOXCH_BOX_MAX )
      iPos = ZH_BOXCH_CHR_BASE +
             ( uc16 - ZH_BOXCH_BOX_MIN );
   else if( uc16 >= ZH_BOXCH_RC_MIN && uc16 <= ZH_BOXCH_RC_MAX )
      iPos = ZH_BOXCH_CHR_BASE + ( ZH_BOXCH_BOX_MAX - ZH_BOXCH_BOX_MIN + 1 ) +
             ( uc16 - ZH_BOXCH_RC_MIN );
   else
      return NULL;

   iTrans = pWVT->boxIndex[ iPos ];
   if( iTrans == ZH_BOXCH_TRANS_MAX )
   {
      if( pWVT->boxCount < ZH_BOXCH_TRANS_MAX - 1 )
      {
         iTrans = pWVT->boxCount + 1;
         pWVT->boxImage[ iTrans ] = zh_gt_wvt_DefineBoxChar( pWVT, uc16 );
         if( pWVT->boxImage[ iTrans ] )
            pWVT->boxCount = iTrans;
         else
            iTrans = 0;
      }
      else
         iTrans = 0;
      pWVT->boxIndex[ iPos ] = ( ZH_UCHAR ) iTrans;
   }

   return pWVT->boxImage[ iTrans ];
}
#endif /* UNICODE */

/*
 * use the standard fixed OEM font, unless the caller has requested set size fonts
 */
static HFONT zh_gt_wvt_GetFont( LPCTSTR lpFace, int iHeight, int iWidth, int iWeight, int iQuality, int iCodePage )
{
   if( iHeight > 0 )
   {
      LOGFONT logfont;

      memset( &logfont, 0, sizeof( logfont ) );
      logfont.lfEscapement     = 0;
      logfont.lfOrientation    = 0;
      logfont.lfWeight         = iWeight;
      logfont.lfItalic         = 0;
      logfont.lfUnderline      = 0;
      logfont.lfStrikeOut      = 0;
      logfont.lfCharSet        = ( BYTE ) iCodePage;      /* OEM_CHARSET */
      logfont.lfOutPrecision   = 0;
      logfont.lfClipPrecision  = 0;
      logfont.lfQuality        = ( BYTE ) iQuality;       /* DEFAULT_QUALITY, DRAFT_QUALITY or PROOF_QUALITY */
      logfont.lfPitchAndFamily = FIXED_PITCH | FF_MODERN; /* all mapping depends on fixed width fonts! */
      logfont.lfHeight         = iHeight;
      logfont.lfWidth          = iWidth < 0 ? -iWidth : iWidth;

      ZH_STRNCPY( logfont.lfFaceName, lpFace, ZH_SIZEOFARRAY( logfont.lfFaceName ) - 1 );

      return CreateFontIndirect( &logfont );
   }
   else
      return ( HFONT ) GetStockObject( OEM_FIXED_FONT /* SYSTEM_FIXED_FONT */ );
}

static POINT zh_gt_wvt_GetXYFromColRow( PZH_GTWVT pWVT, int col, int row )
{
   POINT xy;

   xy.x = col * pWVT->PTEXTSIZE.x + pWVT->MarginLeft;
   xy.y = row * pWVT->PTEXTSIZE.y + pWVT->MarginTop;

   return xy;
}

static RECT zh_gt_wvt_GetXYFromColRowRect( PZH_GTWVT pWVT, RECT colrow )
{
   RECT xy;

   xy.left   = colrow.left * pWVT->PTEXTSIZE.x + pWVT->MarginLeft;
   xy.top    = colrow.top  * pWVT->PTEXTSIZE.y + pWVT->MarginTop;
   xy.right  = ( colrow.right  + 1 ) * pWVT->PTEXTSIZE.x + pWVT->MarginLeft;
   xy.bottom = ( colrow.bottom + 1 ) * pWVT->PTEXTSIZE.y + pWVT->MarginTop;

   return xy;
}

static void zh_gt_wvt_UpdateCaret( PZH_GTWVT pWVT )
{
   int iRow, iCol, iStyle, iCaretSize;

   ZH_GTSELF_GETSCRCURSOR( pWVT->pGT, &iRow, &iCol, &iStyle );

   if( iRow < 0 || iCol < 0 || iRow >= pWVT->ROWS || iCol >= pWVT->COLS )
      iCaretSize = 0;
   else switch( iStyle )
   {
      case SC_INSERT:
         iCaretSize = pWVT->PTEXTSIZE.y >> 1;
         break;
      case SC_SPECIAL1:
         iCaretSize = pWVT->PTEXTSIZE.y;
         break;
      case SC_SPECIAL2:
         iCaretSize = - ( pWVT->PTEXTSIZE.y >> 1 );
         break;
      case SC_NORMAL:
         iCaretSize = ZH_MAX( ( pWVT->PTEXTSIZE.y >> 2 ) - 1, 1 );
         break;
      default:
         iCaretSize = 0;
         break;
   }

   if( iCaretSize == 0 )
   {
      if( pWVT->CaretExist && ! pWVT->CaretHidden )
      {
         HideCaret( pWVT->hWnd );
         pWVT->CaretHidden = ZH_TRUE;
      }
   }
   else
   {
      if( iCaretSize != pWVT->CaretSize || pWVT->PTEXTSIZE.x != pWVT->CaretWidth ||
          ! pWVT->CaretExist )
      {
         pWVT->CaretSize = iCaretSize;
         pWVT->CaretWidth = pWVT->PTEXTSIZE.x;
         pWVT->CaretExist = CreateCaret( pWVT->hWnd, NULL, pWVT->PTEXTSIZE.x,
                                         pWVT->CaretSize < 0 ? - pWVT->CaretSize : pWVT->CaretSize );
      }
      if( pWVT->CaretExist )
      {
         POINT xy;
         xy = zh_gt_wvt_GetXYFromColRow( pWVT, iCol, iRow );
         SetCaretPos( xy.x, pWVT->CaretSize < 0 ?
                      xy.y : xy.y + pWVT->PTEXTSIZE.y - pWVT->CaretSize );
         ShowCaret( pWVT->hWnd );
         pWVT->CaretHidden = ZH_FALSE;
      }
   }
}

static void zh_gt_wvt_KillCaret( PZH_GTWVT pWVT )
{
   if( pWVT->CaretExist )
   {
      DestroyCaret();
      pWVT->CaretExist = ZH_FALSE;
   }
}

/*
 * functions for handling the input queues for the mouse and keyboard
 */
static void zh_gt_wvt_AddCharToInputQueue( PZH_GTWVT pWVT, int iKey )
{
   int iPos = pWVT->keyPointerIn;

   if( pWVT->keyPointerIn != pWVT->keyPointerOut &&
       ZH_INKEY_ISMOUSEPOS( iKey ) )
   {
      int iLastKey = pWVT->Keys[ pWVT->keyLastPos ];
      if( ZH_INKEY_ISMOUSEPOS( iLastKey ) )
      {
         pWVT->Keys[ pWVT->keyLastPos ] = iKey;
         return;
      }
   }

   pWVT->Keys[ pWVT->keyLastPos = iPos ] = iKey;
   if( ++iPos >= ( int ) ZH_SIZEOFARRAY( pWVT->Keys ) )
      iPos = 0;
   if( iPos != pWVT->keyPointerOut )
      pWVT->keyPointerIn = iPos;
}

static ZH_BOOL zh_gt_wvt_GetCharFromInputQueue( PZH_GTWVT pWVT, int * iKey )
{
   if( pWVT->keyPointerOut != pWVT->keyPointerIn )
   {
      *iKey = pWVT->Keys[ pWVT->keyPointerOut ];
      if( ++pWVT->keyPointerOut >= ( int ) ZH_SIZEOFARRAY( pWVT->Keys ) )
         pWVT->keyPointerOut = 0;

      return ZH_TRUE;
   }

   *iKey = 0;
   return ZH_FALSE;
}

#if ! defined( UNICODE )
static int zh_gt_wvt_key_ansi_to_oem( int c )
{
   BYTE pszSrc[ 2 ];
   wchar_t pszWide[ 1 ];
   BYTE pszDst[ 2 ];

   pszSrc[ 0 ] = ( CHAR ) c;
   pszSrc[ 1 ] =
   pszDst[ 0 ] =
   pszDst[ 1 ] = 0;

   if( MultiByteToWideChar( CP_ACP, MB_PRECOMPOSED, ( LPCSTR ) pszSrc, 1, ( LPWSTR ) pszWide, 1 ) &&
       WideCharToMultiByte( CP_OEMCP, 0, ( LPCWSTR ) pszWide, 1, ( LPSTR ) pszDst, 1, NULL, NULL ) )
      return pszDst[ 0 ];
   else
      return c;
}
#endif

static void zh_gt_wvt_FitRows( PZH_GTWVT pWVT )
{
   RECT ci;
   int maxWidth;
   int maxHeight;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_wvt_FitRows()" ) );

#if defined( ZH_OS_WIN_CE )
   pWVT->bMaximized = ZH_FALSE;
#else
   pWVT->bMaximized = IsZoomed( pWVT->hWnd );
#endif

   GetClientRect( pWVT->hWnd, &ci );
   maxWidth = ci.right;
   maxHeight = ci.bottom;

   if( maxHeight > 0 )
   {
      ZH_BOOL bOldCentre = pWVT->CentreWindow;
      pWVT->CentreWindow = ZH_FALSE;
      ZH_GTSELF_SETMODE( pWVT->pGT, maxHeight / pWVT->PTEXTSIZE.y, maxWidth / pWVT->PTEXTSIZE.x );
      pWVT->CentreWindow = bOldCentre;
   }
}

static void zh_gt_wvt_FitSize( PZH_GTWVT pWVT )
{
   RECT wi;
   RECT ci;
   int maxWidth;
   int maxHeight;
   int borderWidth;
   int borderHeight;
   int left;
   int top;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_wvt_FitSize()" ) );

#if defined( ZH_OS_WIN_CE )
   pWVT->bMaximized = ZH_FALSE;
#else
   pWVT->bMaximized = IsZoomed( pWVT->hWnd );
#endif

   GetClientRect( pWVT->hWnd, &ci );
   GetWindowRect( pWVT->hWnd, &wi );

   borderWidth = ( wi.right - wi.left ) - ci.right;
   borderHeight = ( wi.bottom - wi.top ) - ci.bottom;

   maxWidth  = ci.right;
   maxHeight = ci.bottom;

   left = wi.left;
   top  = wi.top;

   {
      HFONT hOldFont;
      HFONT hFont;
      int   fontHeight;
      int   fontWidth;
      int   n;

      int i = 0;
      int j = 0;
      int iCalcWidth = 0;
      int iCalcHeight = 0;

      fontHeight = maxHeight / pWVT->ROWS;
      fontWidth  = maxWidth  / pWVT->COLS;

      for( ;; )
      {
         hFont = zh_gt_wvt_GetFont( pWVT->fontFace, fontHeight, fontWidth, pWVT->fontWeight, pWVT->fontQuality, pWVT->CodePage );
         if( hFont )
         {
            HDC        hdc;
            int        width;
            int        height;
            TEXTMETRIC tm;

            hdc       = GetDC( pWVT->hWnd );
            hOldFont  = ( HFONT ) SelectObject( hdc, hFont );
            SetTextCharacterExtra( hdc, 0 );
            GetTextMetrics( hdc, &tm );
            SelectObject( hdc, hOldFont );
            ReleaseDC( pWVT->hWnd, hdc );

            width     = tm.tmAveCharWidth * pWVT->COLS;
            height    = tm.tmHeight       * pWVT->ROWS;

            if( width <= maxWidth &&
                height <= maxHeight &&
                tm.tmAveCharWidth >= 4 &&
                tm.tmHeight >= 8 )
            {
#if ! defined( UNICODE )
               if( pWVT->hFontBox && pWVT->hFontBox != pWVT->hFont )
                  DeleteObject( pWVT->hFontBox );

               if( pWVT->CodePage == pWVT->boxCodePage )
                  pWVT->hFontBox = hFont;
               else
               {
                  pWVT->hFontBox = zh_gt_wvt_GetFont( pWVT->fontFace, fontHeight, fontWidth, pWVT->fontWeight, pWVT->fontQuality, pWVT->boxCodePage );
                  if( ! pWVT->hFontBox )
                     pWVT->hFontBox = hFont;
               }
#endif
               if( pWVT->hFont )
                  DeleteObject( pWVT->hFont );

               pWVT->hFont       = hFont;
               pWVT->fontHeight  = tm.tmHeight;
               pWVT->fontWidth   = tm.tmAveCharWidth;
               pWVT->PTEXTSIZE.x = tm.tmAveCharWidth;
               pWVT->PTEXTSIZE.y = tm.tmHeight;

#if defined( UNICODE )
               /* reset character bitmap tables (after font selection) */
               zh_gt_wvt_ResetBoxCharBitmaps( pWVT );
#endif

#if defined( ZH_OS_WIN_CE )
               pWVT->FixedFont = ZH_FALSE;
#else
               pWVT->FixedFont = ! pWVT->Win9X && pWVT->fontWidth >= 0 &&
                           ( tm.tmPitchAndFamily & TMPF_FIXED_PITCH ) == 0 &&
                           ( pWVT->PTEXTSIZE.x == tm.tmMaxCharWidth );
#endif
               for( n = 0; n < pWVT->COLS; n++ )
                  pWVT->FixedSize[ n ] = pWVT->PTEXTSIZE.x;

               width  = ( ( int ) ( pWVT->PTEXTSIZE.x * pWVT->COLS ) ) + borderWidth;
               height = ( ( int ) ( pWVT->PTEXTSIZE.y * pWVT->ROWS ) ) + borderHeight;

               if( pWVT->bMaximized )
               {
                  pWVT->MarginLeft = ( wi.right - wi.left - width  ) / 2;
                  pWVT->MarginTop  = ( wi.bottom - wi.top - height ) / 2;
               }
               else
               {
                  pWVT->MarginLeft = 0;
                  pWVT->MarginTop  = 0;
                  if( wi.right - wi.left != width || wi.bottom - wi.top != height )
                      /* above condition is necessary to avoid infinite
                       * recursive in WinCE builds
                       */
                     SetWindowPos( pWVT->hWnd, NULL, left, top, width, height, SWP_NOZORDER );
               }

               if( pWVT->CaretExist && ! pWVT->CaretHidden )
                  zh_gt_wvt_UpdateCaret( pWVT );
            }
            else
            {
               /* I did it this way, so that "Courier New" would size and maximize as expected.
                * "Courier New"  appears to not scale linearly, sometimes by just decreasing the
                * font width by one with some font heights makes it all work out?
                * This code never seems to get executed with "Lucida Console"
                * Width scaling with some Heights is an issue with Courier New and Terminal
                * Height scaling with some Widths is an issue with Consolas and Terminal
                * but this code lets us adjust it here and try creating the font again. [HVB]
                */

               if( iCalcWidth == 0 && iCalcHeight == 0 )
               {
                  iCalcWidth  = fontWidth;
                  iCalcHeight = fontHeight;
               }

               if( i == j )
               {
                  j = 0;
                  i++;
               }
               else if( i > j )
               {
                  j++;
                  if( j == i )
                     i = 0;
               }
               else
                  i++;

               fontWidth  = iCalcWidth - i;
               fontHeight = iCalcHeight - j;

               if( fontWidth < 4 || fontHeight < 8 )
               {
                  width  = ( ( int ) ( pWVT->PTEXTSIZE.x * pWVT->COLS ) ) + borderWidth;
                  height = ( ( int ) ( pWVT->PTEXTSIZE.y * pWVT->ROWS ) ) + borderHeight;
                  SetWindowPos( pWVT->hWnd, NULL, left, top, width, height, SWP_NOZORDER );
                  break;
               }

               continue;
            }

            ZH_GTSELF_EXPOSEAREA( pWVT->pGT, 0, 0, pWVT->ROWS, pWVT->COLS );
         }

         break;
      }
   }
}

static void zh_gt_wvt_ResetWindowSize( PZH_GTWVT pWVT, HFONT hFont )
{
   HDC        hdc;
   HFONT      hOldFont;
   int        height, width;
   RECT       wi, ci;
   TEXTMETRIC tm;
   int        n;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_wvt_ResetWindowSize(%p,%p)", ( void * ) pWVT, ( void * ) hFont ) );

   if( ! pWVT->hFont || hFont )
   {
      if( ! hFont )
         hFont = zh_gt_wvt_GetFont( pWVT->fontFace, pWVT->fontHeight, pWVT->fontWidth,
                                    pWVT->fontWeight, pWVT->fontQuality, pWVT->CodePage );
#if ! defined( UNICODE )
      if( pWVT->hFont )
         DeleteObject( pWVT->hFont );
      if( pWVT->hFontBox && pWVT->hFontBox != pWVT->hFont )
         DeleteObject( pWVT->hFontBox );
      if( pWVT->CodePage == pWVT->boxCodePage )
         pWVT->hFontBox = hFont;
      else
      {
         pWVT->hFontBox = zh_gt_wvt_GetFont( pWVT->fontFace, pWVT->fontHeight, pWVT->fontWidth,
                                             pWVT->fontWeight, pWVT->fontQuality, pWVT->boxCodePage );
         if( ! pWVT->hFontBox )
            pWVT->hFontBox = hFont;
      }
#endif
      pWVT->hFont = hFont;
   }
   else
      hFont = pWVT->hFont;

   hdc      = GetDC( pWVT->hWnd );
   hOldFont = ( HFONT ) SelectObject( hdc, hFont );
   GetTextMetrics( hdc, &tm );
   SetTextCharacterExtra( hdc, 0 ); /* do not add extra char spacing even if bold */
   SelectObject( hdc, hOldFont );
   ReleaseDC( pWVT->hWnd, hdc );

   /*
    * we will need to use the font size to handle the transformations from
    * row column space in the future, so we keep it around in a static!
    */

   pWVT->PTEXTSIZE.x = pWVT->fontWidth < 0 ? -pWVT->fontWidth :
                    tm.tmAveCharWidth; /* For fixed FONT should == tm.tmMaxCharWidth */
   pWVT->PTEXTSIZE.y = tm.tmHeight;    /* but seems to be a problem on Win9X so */
                                       /* assume proportional fonts always for Win9X */
#if defined( UNICODE )
   /* reset character bitmaps (after font selection) */
   zh_gt_wvt_ResetBoxCharBitmaps( pWVT );
#endif

#if defined( ZH_OS_WIN_CE )
   pWVT->FixedFont = ZH_FALSE;
#else
   pWVT->FixedFont = ! pWVT->Win9X && pWVT->fontWidth >= 0 &&
                     ( tm.tmPitchAndFamily & TMPF_FIXED_PITCH ) == 0 &&
                     ( pWVT->PTEXTSIZE.x == tm.tmMaxCharWidth );
#endif

   /* pWVT->FixedSize[] is used by ExtTextOut() to emulate
      fixed font when a proportional font is used */
   for( n = 0; n < pWVT->COLS; n++ )
      pWVT->FixedSize[ n ] = pWVT->PTEXTSIZE.x;

   /* resize the window to get the specified number of rows and columns */
   GetWindowRect( pWVT->hWnd, &wi );
   GetClientRect( pWVT->hWnd, &ci );

   height = ( int ) ( pWVT->PTEXTSIZE.y * pWVT->ROWS );
   width  = ( int ) ( pWVT->PTEXTSIZE.x * pWVT->COLS );

   width  += ( int ) ( wi.right - wi.left - ci.right );
   height += ( int ) ( wi.bottom - wi.top - ci.bottom );

   /* Center the window within the CLIENT area on the screen
      but only if pWVT->CentreWindow == ZH_TRUE */
   if( pWVT->bMaximized )
   {
      pWVT->MarginLeft = ( wi.right - wi.left - width  ) / 2;
      pWVT->MarginTop  = ( wi.bottom - wi.top - height ) / 2;
   }
   else
   {
      RECT rcWorkArea;
      pWVT->MarginLeft = 0;
      pWVT->MarginTop  = 0;

      if( pWVT->CentreWindow && SystemParametersInfo( SPI_GETWORKAREA, 0, &rcWorkArea, 0 ) )
      {
         int bRecenter = ZH_FALSE;

         if( width > rcWorkArea.right - rcWorkArea.left )
         {
            /* New window width is larger than monitor workarea, force to fit and adjusts Font size */
            width = rcWorkArea.right - rcWorkArea.left;
            bRecenter = ZH_TRUE;
         }

         if( height > rcWorkArea.bottom - rcWorkArea.top )
         {
            /* New window height is larger than monitor workarea, force to fit and adjusts Font height */
            height = rcWorkArea.bottom - rcWorkArea.top;
            bRecenter = ZH_TRUE;
         }

         if( bRecenter ||
             rcWorkArea.left + width > rcWorkArea.right ||
             rcWorkArea.top + height > rcWorkArea.bottom ||
             wi.left != pWVT->iNewPosX || wi.top != pWVT->iNewPosY )
         {
            wi.left = rcWorkArea.left + ( ( rcWorkArea.right - rcWorkArea.left - width  ) / 2 );
            wi.top  = rcWorkArea.top  + ( ( rcWorkArea.bottom - rcWorkArea.top - height ) / 2 );
         }

         if( pWVT->ResizeMode == ZH_GTI_RESIZEMODE_ROWS )
         {
            pWVT->ResizeMode = ZH_GTI_RESIZEMODE_FONT;
            SetWindowPos( pWVT->hWnd, NULL, wi.left, wi.top, width, height, SWP_NOZORDER );
            pWVT->ResizeMode = ZH_GTI_RESIZEMODE_ROWS;
         }
         else
            SetWindowPos( pWVT->hWnd, NULL, wi.left, wi.top, width, height, SWP_NOZORDER );

         if( bRecenter )
         {
            GetWindowRect( pWVT->hWnd, &wi );
            width = wi.right - wi.left;
            height = wi.bottom - wi.top;
            wi.left = rcWorkArea.left + ( ( rcWorkArea.right - rcWorkArea.left - width  ) / 2 );
            wi.top  = rcWorkArea.top  + ( ( rcWorkArea.bottom - rcWorkArea.top - height ) / 2 );
            SetWindowPos( pWVT->hWnd, NULL, wi.left, wi.top, width, height, SWP_NOSIZE | SWP_NOZORDER );
         }
      }
#if ! defined( ZH_OS_WIN_CE )
      /* This code creates infinite recursive calls in WinCE */
      else
      {
         /* Will resize window without moving left/top origin */
         SetWindowPos( pWVT->hWnd, NULL, wi.left, wi.top, width, height, SWP_NOZORDER );
      }
#endif
   }

   ZH_GTSELF_EXPOSEAREA( pWVT->pGT, 0, 0, pWVT->ROWS, pWVT->COLS );

   if( pWVT->CaretExist && ! pWVT->CaretHidden )
      zh_gt_wvt_UpdateCaret( pWVT );
}

static ZH_BOOL zh_gt_wvt_SetWindowSize( PZH_GTWVT pWVT, int iRows, int iCols )
{
   if( ZH_GTSELF_RESIZE( pWVT->pGT, iRows, iCols ) )
   {
      if( pWVT->COLS != iCols )
      {
         pWVT->TextLine = ( TCHAR * ) zh_xrealloc( pWVT->TextLine,
                                                   iCols * sizeof( TCHAR ) );
         pWVT->FixedSize = ( int * ) zh_xrealloc( pWVT->FixedSize,
                                                  iCols * sizeof( int ) );
      }
      if( pWVT->hWnd && ( iRows != pWVT->ROWS || iCols != pWVT->COLS ) )
         zh_gt_wvt_AddCharToInputQueue( pWVT, ZH_K_RESIZE );

      pWVT->ROWS = iRows;
      pWVT->COLS = iCols;
      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}

static ZH_BOOL zh_gt_wvt_InitWindow( PZH_GTWVT pWVT, int iRow, int iCol, HFONT hFont )
{
   ZH_BOOL fRet = zh_gt_wvt_SetWindowSize( pWVT, iRow, iCol );

   zh_gt_wvt_ResetWindowSize( pWVT, hFont );

   return fRet;
}

/*
 * get the row and column from xy pixel client coordinates
 * This works because we are using the FIXED system font
 */
static POINT zh_gt_wvt_GetColRowFromXY( PZH_GTWVT pWVT, LONG x, LONG y )
{
   POINT colrow;

   colrow.x = ( x - pWVT->MarginLeft ) / pWVT->PTEXTSIZE.x;
   colrow.y = ( y - pWVT->MarginTop ) / pWVT->PTEXTSIZE.y;

   return colrow;
}

static RECT zh_gt_wvt_GetColRowFromXYRect( PZH_GTWVT pWVT, RECT xy )
{
   RECT colrow;

   if( pWVT->bMaximized )
   {
      if( xy.left >= pWVT->MarginLeft )
         xy.left   = xy.left - pWVT->MarginLeft;
      else
         xy.left   = 0;

      if( xy.right >= pWVT->MarginLeft )
         xy.right  = xy.right - pWVT->MarginLeft;
      else
         xy.right  = 0;

      if( xy.top >= pWVT->MarginTop )
         xy.top    = xy.top - pWVT->MarginTop;
      else
         xy.top    = 0;

      if( xy.bottom >= pWVT->MarginTop )
         xy.bottom = xy.bottom - pWVT->MarginTop;
      else
         xy.bottom = 0;
   }

   colrow.left   = xy.left   / pWVT->PTEXTSIZE.x;
   colrow.top    = xy.top    / pWVT->PTEXTSIZE.y;
   colrow.right  = xy.right  / pWVT->PTEXTSIZE.x -
                   ( ( xy.right  % pWVT->PTEXTSIZE.x ) ? 0 : 1 ); /* Adjust for when rectangle */
   colrow.bottom = xy.bottom / pWVT->PTEXTSIZE.y -
                   ( ( xy.bottom % pWVT->PTEXTSIZE.y ) ? 0 : 1 ); /* EXACTLY overlaps characters */

   return colrow;
}

static ZH_BOOL zh_gt_wvt_SetMousePos( PZH_GTWVT pWVT, int iRow, int iCol )
{
   if( pWVT->MousePos.y != iRow || pWVT->MousePos.x != iCol )
   {
      pWVT->MousePos.y = iRow;
      pWVT->MousePos.x = iCol;
      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}

static int zh_gt_wvt_GetKeyFlags( void )
{
   int iFlags = 0;
   if( GetKeyState( VK_SHIFT ) & 0x8000 )
      iFlags |= ZH_KF_SHIFT;
   if( GetKeyState( VK_CONTROL ) & 0x8000 )
      iFlags |= ZH_KF_CTRL;
   if( GetKeyState( VK_LMENU ) & 0x8000 )
      iFlags |= ZH_KF_ALT;
   if( GetKeyState( VK_RMENU ) & 0x8000 )
      iFlags |= ZH_KF_ALTGR;

   return iFlags;
}

static int zh_gt_wvt_UpdateKeyFlags( int iFlags )
{
   if( iFlags & ZH_KF_ALTGR )
   {
      iFlags |= ZH_KF_ALT;
      iFlags &= ~ZH_KF_ALTGR;
   }

   return iFlags;
}

static void zh_gt_wvt_Composited( PZH_GTWVT pWVT, ZH_BOOL fEnable )
{
#if defined( ZH_OS_WIN_CE )
   ZH_SYMBOL_UNUSED( pWVT );
   ZH_SYMBOL_UNUSED( fEnable );
#else
   if( zh_iswinvista() && ! GetSystemMetrics( SM_REMOTESESSION ) )
   {
      pWVT->bComposited = fEnable;
      if( fEnable )
         SetWindowLongPtr( pWVT->hWnd, GWL_EXSTYLE, GetWindowLongPtr( pWVT->hWnd, GWL_EXSTYLE ) | WS_EX_COMPOSITED );
      else
         SetWindowLongPtr( pWVT->hWnd, GWL_EXSTYLE, GetWindowLongPtr( pWVT->hWnd, GWL_EXSTYLE ) & ~WS_EX_COMPOSITED );
   }
#endif
}

static void zh_gt_wvt_SetCloseButton( PZH_GTWVT pWVT )
{
   HMENU hSysMenu = GetSystemMenu( pWVT->hWnd, FALSE );

   if( hSysMenu )
      EnableMenuItem( hSysMenu, SC_CLOSE, MF_BYCOMMAND |
                      ( pWVT->CloseMode < 2 ? MF_ENABLED : MF_GRAYED ) );
}

static void zh_gt_wvt_MouseEvent( PZH_GTWVT pWVT, UINT message, WPARAM wParam, LPARAM lParam )
{
   SHORT keyCode = 0;
   POINT xy, colrow;

   xy.x = LOWORD( lParam );
   xy.y = HIWORD( lParam );

   if( message == WM_MOUSEWHEEL )
      ScreenToClient( pWVT->hWnd, &xy );

   colrow = zh_gt_wvt_GetColRowFromXY( pWVT, xy.x, xy.y );
   if( ! pWVT->bBeingMarked &&
       zh_gt_wvt_SetMousePos( pWVT, colrow.y, colrow.x ) )
      zh_gt_wvt_AddCharToInputQueue( pWVT,
                     ZH_INKEY_NEW_MPOS( pWVT->MousePos.x, pWVT->MousePos.y ) );

   switch( message )
   {
      case WM_LBUTTONDBLCLK:
         keyCode = K_LDBLCLK;
         break;

      case WM_RBUTTONDBLCLK:
         keyCode = K_RDBLCLK;
         break;

      case WM_LBUTTONDOWN:
         if( pWVT->bBeginMarked || pWVT->bQuickEdit )
         {
            pWVT->bBeingMarked = ZH_TRUE;

            pWVT->sRectNew.left   = xy.x;
            pWVT->sRectNew.top    = xy.y;
            pWVT->sRectNew.right  = xy.x;
            pWVT->sRectNew.bottom = xy.y;

            pWVT->sRectOld.left   = 0;
            pWVT->sRectOld.top    = 0;
            pWVT->sRectOld.right  = 0;
            pWVT->sRectOld.bottom = 0;

            zh_gt_wvt_Composited( pWVT, ZH_FALSE );

            return;
         }
         keyCode = K_LBUTTONDOWN;
         break;

      case WM_RBUTTONDOWN:
         keyCode = K_RBUTTONDOWN;
         break;

      case WM_RBUTTONUP:
         if( pWVT->bQuickEdit )
         {
            ZH_GT_INFO gtInfo;

            memset( &gtInfo, 0, sizeof( gtInfo ) );

            zh_gtInfo( ZH_GTI_CLIPBOARDPASTE, &gtInfo );

            zh_gt_wvt_Composited( pWVT, ZH_FALSE );

            return;
         }

         keyCode = K_RBUTTONUP;
         break;

      case WM_LBUTTONUP:

         if( pWVT->bBeingMarked || pWVT->bQuickEdit )
         {
            pWVT->bBeginMarked = ZH_FALSE;
            pWVT->bBeingMarked = ZH_FALSE;

            RedrawWindow( pWVT->hWnd, NULL, NULL, RDW_INVALIDATE | RDW_UPDATENOW );

            {
#if ! defined( UNICODE )
               PZH_CODEPAGE cdpHost = ZH_GTSELF_HOSTCP( pWVT->pGT ),
                            cdpBox  = ZH_GTSELF_BOXCP( pWVT->pGT );
#endif
               TCHAR * sBuffer;
               ZH_SIZE nSize, n;
               int     row, col;
               RECT    rect;

               rect.left   = ZH_MIN( pWVT->sRectNew.left, pWVT->sRectNew.right  );
               rect.top    = ZH_MIN( pWVT->sRectNew.top , pWVT->sRectNew.bottom );
               rect.right  = ZH_MAX( pWVT->sRectNew.left, pWVT->sRectNew.right  );
               rect.bottom = ZH_MAX( pWVT->sRectNew.top , pWVT->sRectNew.bottom );

               rect = zh_gt_wvt_GetColRowFromXYRect( pWVT, rect );

               nSize = ( rect.bottom - rect.top + 1 ) *
                       ( rect.right - rect.left + 1 + 2 );
               sBuffer = ( TCHAR * ) zh_xgrab( nSize * sizeof( TCHAR ) + 1 );

               for( n = 0, row = rect.top; row <= rect.bottom; row++ )
               {
                  for( col = rect.left; col <= rect.right; col++ )
                  {
                     int iColor;
                     ZH_BYTE bAttr;
                     ZH_USHORT usChar;

                     if( ! ZH_GTSELF_GETSCRCHAR( pWVT->pGT, row, col, &iColor, &bAttr, &usChar ) )
                        break;
#if defined( UNICODE )
                     usChar = zh_cdpGetU16Ctrl( usChar );
#else
                     usChar = zh_cdpGetUC( ( bAttr & ZH_GT_ATTR_BOX ) ? cdpBox : cdpHost, usChar, '?' );
#endif
                     sBuffer[ n++ ] = ( TCHAR ) usChar;
                  }
                  if( rect.top < rect.bottom )
                  {
                     sBuffer[ n++ ] = '\r';
                     sBuffer[ n++ ] = '\n';
                  }
               }

#if defined( UNICODE )
               if( n > 0 )
               {
                  PZH_ITEM pItem = zh_itemPutStrLenU16( NULL, ZH_CODEPAGE_ENDIAN_NATIVE, sBuffer, n );
                  zh_gt_winapi_setClipboard( CF_UNICODETEXT, pItem );
                  zh_itemRelease( pItem );
               }
               zh_xfree( sBuffer );
#else
               if( n > 0 )
               {
                  PZH_ITEM pItem = zh_itemPutCLPtr( NULL, sBuffer, n );
                  zh_gt_winapi_setClipboard( pWVT->CodePage == OEM_CHARSET ?
                                             CF_OEMTEXT : CF_TEXT, pItem );
                  zh_itemRelease( pItem );
               }
               else
                  zh_xfree( sBuffer );
#endif
            }

            zh_gt_wvt_Composited( pWVT, ZH_TRUE );
            return;
         }
         keyCode = K_LBUTTONUP;
         break;

      case WM_MBUTTONDOWN:
         keyCode = K_MBUTTONDOWN;
         break;

      case WM_MBUTTONUP:
         keyCode = K_MBUTTONUP;
         break;

      case WM_MBUTTONDBLCLK:
         keyCode = K_MDBLCLK;
         break;

      case WM_MOUSEMOVE:

         if( pWVT->bBeingMarked )
         {
            RECT rect;

            pWVT->sRectNew.right  = xy.x;
            pWVT->sRectNew.bottom = xy.y;

            rect.left   = ZH_MIN( pWVT->sRectNew.left, pWVT->sRectNew.right  );
            rect.top    = ZH_MIN( pWVT->sRectNew.top , pWVT->sRectNew.bottom );
            rect.right  = ZH_MAX( pWVT->sRectNew.left, pWVT->sRectNew.right  );
            rect.bottom = ZH_MAX( pWVT->sRectNew.top , pWVT->sRectNew.bottom );
            /* out of band cords may appear due to margins in maximized mode */
            if( rect.left < 0 )
               rect.left = 0;
            if( rect.top < 0 )
               rect.top = 0;
            if( rect.right > pWVT->COLS * pWVT->PTEXTSIZE.x )
               rect.right = pWVT->COLS * pWVT->PTEXTSIZE.x;
            if( rect.bottom > pWVT->ROWS * pWVT->PTEXTSIZE.y )
               rect.bottom = pWVT->ROWS * pWVT->PTEXTSIZE.y;

            rect = zh_gt_wvt_GetXYFromColRowRect( pWVT,
                                 zh_gt_wvt_GetColRowFromXYRect( pWVT, rect ) );

            if( rect.left   != pWVT->sRectOld.left   ||
                rect.top    != pWVT->sRectOld.top    ||
                rect.right  != pWVT->sRectOld.right  ||
                rect.bottom != pWVT->sRectOld.bottom )
            {
#if ! defined( ZH_OS_WIN_CE )  /* WinCE does not support InvertRgn */
               /* Concept forwarded by Andy Wos - thanks. */
               HRGN rgn1 = CreateRectRgn( pWVT->sRectOld.left, pWVT->sRectOld.top, pWVT->sRectOld.right, pWVT->sRectOld.bottom );
               HRGN rgn2 = CreateRectRgn( rect.left, rect.top, rect.right, rect.bottom );
               HRGN rgn3 = CreateRectRgn( 0, 0, 0, 0 );

               if( CombineRgn( rgn3, rgn1, rgn2, RGN_XOR ) != 0 )
               {
                  HDC hdc = GetDC( pWVT->hWnd );
                  InvertRgn( hdc, rgn3 );
                  ReleaseDC( pWVT->hWnd, hdc );
               }

               DeleteObject( rgn1 );
               DeleteObject( rgn2 );
               DeleteObject( rgn3 );
#else
               HDC hdc = GetDC( pWVT->hWnd );
               InvertRect( hdc, &pWVT->sRectOld );
               InvertRect( hdc, &rect );
               ReleaseDC( pWVT->hWnd, hdc );
#endif
               pWVT->sRectOld.left   = rect.left;
               pWVT->sRectOld.top    = rect.top;
               pWVT->sRectOld.right  = rect.right;
               pWVT->sRectOld.bottom = rect.bottom;
            }
            return;
         }
         break;

      case WM_MOUSEWHEEL:
         keyCode = ( SHORT ) HIWORD( wParam ) > 0 ? K_MWFORWARD : K_MWBACKWARD;
         break;
   }

   if( keyCode != 0 )
      zh_gt_wvt_AddCharToInputQueue( pWVT,
                  ZH_INKEY_NEW_MKEY( keyCode,
                        zh_gt_wvt_UpdateKeyFlags( zh_gt_wvt_GetKeyFlags() ) ) );
}

static ZH_BOOL zh_gt_wvt_KeyEvent( PZH_GTWVT pWVT, UINT message, WPARAM wParam, LPARAM lParam )
{
   int iKey = 0, iFlags = pWVT->keyFlags, iKeyPad = 0;

   switch( message )
   {
      case WM_KEYDOWN:
      case WM_SYSKEYDOWN:
         pWVT->IgnoreWM_SYSCHAR = ZH_FALSE;
         iFlags = zh_gt_wvt_GetKeyFlags();
         switch( wParam )
         {
            case VK_BACK:
               pWVT->IgnoreWM_SYSCHAR = ZH_TRUE;
               iKey = ZH_KX_BS;
               break;
            case VK_TAB:
               pWVT->IgnoreWM_SYSCHAR = ZH_TRUE;
               iKey = ZH_KX_TAB;
               break;
            case VK_RETURN:
               pWVT->IgnoreWM_SYSCHAR = ZH_TRUE;
               if( pWVT->bAltEnter && ( iFlags & ZH_KF_ALT ) != 0 )
                  zh_gt_wvt_FullScreen( pWVT->pGT );
               else
               {
                  iKey = ZH_KX_ENTER;
                  if( lParam & WVT_EXTKEY_FLAG )
                     iFlags |= ZH_KF_KEYPAD;
               }
               break;
            case VK_ESCAPE:
               pWVT->IgnoreWM_SYSCHAR = ZH_TRUE;
               iKey = ZH_KX_ESC;
               break;

            case VK_PRIOR:
               iKeyPad = ZH_KX_PGUP;
               break;
            case VK_NEXT:
               iKeyPad = ZH_KX_PGDN;
               break;
            case VK_END:
               iKeyPad = ZH_KX_END;
               break;
            case VK_HOME:
               iKeyPad = ZH_KX_HOME;
               break;
            case VK_LEFT:
               iKeyPad = ZH_KX_LEFT;
               break;
            case VK_UP:
               iKeyPad = ZH_KX_UP;
               break;
            case VK_RIGHT:
               iKeyPad = ZH_KX_RIGHT;
               break;
            case VK_DOWN:
               iKeyPad = ZH_KX_DOWN;
               break;
            case VK_INSERT:
               iKeyPad = ZH_KX_INS;
               break;
            case VK_DELETE:
               iKey = ZH_KX_DEL;
               if( ( lParam & WVT_EXTKEY_FLAG ) == 0 )
                  iFlags |= ZH_KF_KEYPAD;
               break;

            case VK_F1:
               iKey = ZH_KX_F1;
               break;
            case VK_F2:
               iKey = ZH_KX_F2;
               break;
            case VK_F3:
               iKey = ZH_KX_F3;
               break;
            case VK_F4:
               iKey = ZH_KX_F4;
               break;
            case VK_F5:
               iKey = ZH_KX_F5;
               break;
            case VK_F6:
               iKey = ZH_KX_F6;
               break;
            case VK_F7:
               iKey = ZH_KX_F7;
               break;
            case VK_F8:
               iKey = ZH_KX_F8;
               break;
            case VK_F9:
               iKey = ZH_KX_F9;
               break;
            case VK_F10:
               iKey = ZH_KX_F10;
               break;
            case VK_F11:
               iKey = ZH_KX_F11;
               break;
            case VK_F12:
               iKey = ZH_KX_F12;
               break;

            case VK_SNAPSHOT:
               iKey = ZH_KX_PRTSCR;
               break;
            case VK_CANCEL:
               if( ( lParam & WVT_EXTKEY_FLAG ) == 0 )
                  break;
               iFlags |= ZH_KF_CTRL;
               /* fallthrough */
            case VK_PAUSE:
               pWVT->IgnoreWM_SYSCHAR = ZH_TRUE;
               iKey = ZH_KX_PAUSE;
               break;

            case VK_CLEAR:
               iKeyPad = ZH_KX_CENTER;
               break;

            case VK_NUMPAD0:
            case VK_NUMPAD1:
            case VK_NUMPAD2:
            case VK_NUMPAD3:
            case VK_NUMPAD4:
            case VK_NUMPAD5:
            case VK_NUMPAD6:
            case VK_NUMPAD7:
            case VK_NUMPAD8:
            case VK_NUMPAD9:
               if( iFlags & ZH_KF_CTRL )
               {
                  pWVT->IgnoreWM_SYSCHAR = ZH_TRUE;
                  iKey = ( int ) wParam - VK_NUMPAD0 + '0';
               }
               else if( iFlags == ZH_KF_ALT || iFlags == ZH_KF_ALTGR )
                  iFlags = 0; /* for ALT + <ASCII_VALUE_FROM_KEYPAD> */
               iFlags |= ZH_KF_KEYPAD;
               break;
            case VK_DECIMAL:
            case VK_SEPARATOR:
               iFlags |= ZH_KF_KEYPAD;
               if( iFlags & ZH_KF_CTRL )
               {
                  pWVT->IgnoreWM_SYSCHAR = ZH_TRUE;
                  iKey = '.';
               }
               break;

            case VK_DIVIDE:
               iFlags |= ZH_KF_KEYPAD;
               if( iFlags & ZH_KF_CTRL )
                  iKey = '/';
               break;
            case VK_MULTIPLY:
               iFlags |= ZH_KF_KEYPAD;
               if( iFlags & ZH_KF_CTRL )
                  iKey = '*';
               break;
            case VK_SUBTRACT:
               iFlags |= ZH_KF_KEYPAD;
               if( iFlags & ZH_KF_CTRL )
                  iKey = '-';
               break;
            case VK_ADD:
               iFlags |= ZH_KF_KEYPAD;
               if( iFlags & ZH_KF_CTRL )
                  iKey = '+';
               break;
#ifdef VK_OEM_2
            case VK_OEM_2:
               if( ( iFlags & ZH_KF_CTRL ) != 0 && ( iFlags & ZH_KF_SHIFT ) != 0 )
                  iKey = '?';
               break;
#endif
#ifdef VK_APPS
            case VK_APPS:
               iKey = ZH_K_MENU;
               break;
#endif
         }
         if( iKeyPad != 0 )
         {
            iKey = iKeyPad;
            if( ( lParam & WVT_EXTKEY_FLAG ) == 0 )
            {
               if( iFlags == ZH_KF_ALT || iFlags == ZH_KF_ALTGR )
                  iFlags = iKey = 0; /* for ALT + <ASCII_VALUE_FROM_KEYPAD> */
               else
                  iFlags |= ZH_KF_KEYPAD;
            }
         }
         pWVT->keyFlags = iFlags;
         if( iKey != 0 )
            iKey = ZH_INKEY_NEW_KEY( iKey, zh_gt_wvt_UpdateKeyFlags( iFlags ) );
         break;

      case WM_CHAR:
         if( ( ( iFlags & ZH_KF_CTRL ) != 0 && ( iFlags & ZH_KF_ALT ) != 0 ) ||
             ( iFlags & ZH_KF_ALTGR ) != 0 )
            /* workaround for AltGR and some German/Italian keyboard */
            iFlags &= ~( ZH_KF_CTRL | ZH_KF_ALT | ZH_KF_ALTGR );
         /* fallthrough */
      case WM_SYSCHAR:
         iFlags = zh_gt_wvt_UpdateKeyFlags( iFlags );
         if( ! pWVT->IgnoreWM_SYSCHAR )
         {
            iKey = ( int ) wParam;

            if( ( iFlags & ZH_KF_CTRL ) != 0 && iKey >= 0 && iKey < 32 )
            {
               iKey += 'A' - 1;
               iKey = ZH_INKEY_NEW_KEY( iKey, iFlags );
            }
            else
            {
               if( message == WM_SYSCHAR && ( iFlags & ZH_KF_ALT ) != 0 )
               {
                  switch( HIWORD( lParam ) & 0xFF )
                  {
                     case  2:
                        iKey = '1';
                        break;
                     case  3:
                        iKey = '2';
                        break;
                     case  4:
                        iKey = '3';
                        break;
                     case  5:
                        iKey = '4';
                        break;
                     case  6:
                        iKey = '5';
                        break;
                     case  7:
                        iKey = '6';
                        break;
                     case  8:
                        iKey = '7';
                        break;
                     case  9:
                        iKey = '8';
                        break;
                     case 10:
                        iKey = '9';
                        break;
                     case 11:
                        iKey = '0';
                        break;
                     case 13:
                        iKey = '=';
                        break;
                     case 14:
                        iKey = ZH_KX_BS;
                        break;
                     case 16:
                        iKey = 'Q';
                        break;
                     case 17:
                        iKey = 'W';
                        break;
                     case 18:
                        iKey = 'E';
                        break;
                     case 19:
                        iKey = 'R';
                        break;
                     case 20:
                        iKey = 'T';
                        break;
                     case 21:
                        iKey = 'Y';
                        break;
                     case 22:
                        iKey = 'U';
                        break;
                     case 23:
                        iKey = 'I';
                        break;
                     case 24:
                        iKey = 'O';
                        break;
                     case 25:
                        iKey = 'P';
                        break;
                     case 30:
                        iKey = 'A';
                        break;
                     case 31:
                        iKey = 'S';
                        break;
                     case 32:
                        iKey = 'D';
                        break;
                     case 33:
                        iKey = 'F';
                        break;
                     case 34:
                        iKey = 'G';
                        break;
                     case 35:
                        iKey = 'H';
                        break;
                     case 36:
                        iKey = 'J';
                        break;
                     case 37:
                        iKey = 'K';
                        break;
                     case 38:
                        iKey = 'L';
                        break;
                     case 44:
                        iKey = 'Z';
                        break;
                     case 45:
                        iKey = 'X';
                        break;
                     case 46:
                        iKey = 'C';
                        break;
                     case 47:
                        iKey = 'V';
                        break;
                     case 48:
                        iKey = 'B';
                        break;
                     case 49:
                        iKey = 'N';
                        break;
                     case 50:
                        iKey = 'M';
                        break;
                  }
               }
#if defined( UNICODE )
               if( iKey >= 127 )
                  iKey = ZH_INKEY_NEW_UNICODEF( iKey, iFlags );
               else if( iFlags & ( ZH_KF_CTRL | ZH_KF_ALT ) )
                  iKey = ZH_INKEY_NEW_KEY( iKey, iFlags );
               else
                  iKey = ZH_INKEY_NEW_CHARF( iKey, iFlags );
#else
               {
                  int u = ZH_GTSELF_KEYTRANS( pWVT->pGT, iKey );
                  if( u )
                     iKey = ZH_INKEY_NEW_UNICODEF( u, iFlags );
                  else if( iKey < 127 && ( iFlags & ( ZH_KF_CTRL | ZH_KF_ALT ) ) )
                     iKey = ZH_INKEY_NEW_KEY( iKey, iFlags );
                  else
                  {
                     if( pWVT->CodePage == OEM_CHARSET )
                        iKey = zh_gt_wvt_key_ansi_to_oem( iKey );
                     iKey = ZH_INKEY_NEW_CHARF( iKey, iFlags );
                  }
               }
#endif
            }
         }
         pWVT->IgnoreWM_SYSCHAR = ZH_FALSE;
         break;
   }

   if( iKey != 0 )
      zh_gt_wvt_AddCharToInputQueue( pWVT, iKey );

   return 0;
}

/*
 * Convert col and row to x and y ( pixels ) and calls
 * the Windows function TextOut with the expected coordinates
 */
static void zh_gt_wvt_TextOut( PZH_GTWVT pWVT, HDC hdc, int col, int row, int iColor, LPCTSTR lpString, UINT cbString )
{
   POINT xy;
   RECT  rClip;
   UINT  fuOptions = ETO_CLIPPED;

   xy = zh_gt_wvt_GetXYFromColRow( pWVT, col, row );
   SetRect( &rClip, xy.x, xy.y, xy.x + cbString * pWVT->PTEXTSIZE.x, xy.y + pWVT->PTEXTSIZE.y );

   if( ( pWVT->fontAttribute & ZH_GTI_FONTA_CLRBKG ) != 0 )
   {
      HBRUSH hBrush = CreateSolidBrush( pWVT->COLORS[ ( iColor >> 4 ) & 0x0F ] );
      FillRect( hdc, &rClip, hBrush );
      DeleteObject( hBrush );
   }
   else
      fuOptions |= ETO_OPAQUE;

   /* set background color */
   SetBkColor( hdc, pWVT->COLORS[ ( iColor >> 4 ) & 0x0F ] );
   /* set foreground color */
   SetTextColor( hdc, pWVT->COLORS[ iColor & 0x0F ] );

   SetTextAlign( hdc, TA_LEFT );

   ExtTextOut( hdc, xy.x, xy.y, fuOptions, &rClip,
               lpString, cbString, pWVT->FixedFont ? NULL : pWVT->FixedSize );
}

static void zh_gt_wvt_PaintText( PZH_GTWVT pWVT )
{
   PAINTSTRUCT ps;
   HDC         hdc;
   RECT        rcRect;
   int         iRow;
   int         iColor, iOldColor = 0;
   ZH_BYTE     bAttr;
   ZH_BOOL     fFixMetric = ( pWVT->fontAttribute & ZH_GTI_FONTA_FIXMETRIC ) != 0;

#if ! defined( UNICODE )
   HFONT       hFont, hOldFont = NULL;
#endif

   hdc = BeginPaint( pWVT->hWnd, &ps );

   /* for sure there is a better method for repainting not used screen area
    * ExcludeClipRect()?
    */
   if( pWVT->bMaximized )
   {
      HBRUSH hBrush = CreateSolidBrush( pWVT->COLORS[ 0 ] );
      RECT ciNew = ps.rcPaint;
      RECT ciTemp = ps.rcPaint;
      if( pWVT->MarginLeft > 0 )
      {
         ciTemp.right = pWVT->MarginLeft;
         FillRect( hdc, &ciTemp, hBrush );
         ciTemp.right = ciNew.right;
         ciTemp.left = ciTemp.right - pWVT->MarginLeft;
         FillRect( hdc, &ciTemp, hBrush );
         ciTemp.left = ciNew.left;
      }
      if( pWVT->MarginTop > 0 )
      {
         ciTemp.bottom = pWVT->MarginTop;
         FillRect( hdc, &ciTemp, hBrush );
         ciTemp.bottom = ciNew.bottom;
         ciTemp.top = ciTemp.bottom - pWVT->MarginTop;
         FillRect( hdc, &ciTemp, hBrush );
      }
      /*FillRect( hdc, &ps.rcPaint, hBrush);
       * ^^^ this was previous code, caused entire window/screen to be erased on every
       * WM_PAINT message which caused a Browse scroll to flicker badly under XP.
       * Now, only repaints the margin areas as required.
       */
      DeleteObject( hBrush );
   }
   else if( pWVT->bAlreadySizing )
   {
      /* This code solves issue with XP when resizing with mouse on window borders,
       * only applicable when ResizeMode is by ROWS, depending on Windows settings
       * to "Show window content while dragging." would cause border artifacts or
       * content smearing when sizing. Now will paint new area black until mouse
       * button is released, much like Windows 7 behaviour.
       * One issue here is that I need a static variable RECT ciLast to store the
       * Client area coordinates before Window sized, this is set in the WM_ENTERSIZEMOVE
       * message and then used here to calculate the size changed areas to paint black.
       */
      HBRUSH hBrush = CreateSolidBrush( pWVT->COLORS[ 0 ] );
      RECT ciNew = ps.rcPaint;
      RECT ciTemp = ps.rcPaint;
      if( ciNew.bottom > pWVT->ciLast.bottom )
      {
         ciTemp.top = pWVT->ciLast.bottom;
         FillRect( hdc, &ciTemp, hBrush );
         ciTemp.top = ciNew.top;
      }
      if( ciNew.right > pWVT->ciLast.right )
      {
         ciTemp.left = pWVT->ciLast.right;
         FillRect( hdc, &ciTemp, hBrush );
      }
      DeleteObject( hBrush );
   }

#if defined( UNICODE )
   SelectObject( hdc, pWVT->hFont );
#endif

   rcRect = zh_gt_wvt_GetColRowFromXYRect( pWVT, ps.rcPaint );

   for( iRow = rcRect.top; iRow <= rcRect.bottom; ++iRow )
   {
      int iCol, startCol, len;

      iCol = startCol = rcRect.left;
      len = 0;

      while( iCol <= rcRect.right )
      {
#if defined( UNICODE )
         HBITMAP hBitMap;
         ZH_USHORT usChar;

         if( ! ZH_GTSELF_GETSCRCHAR( pWVT->pGT, iRow, iCol, &iColor, &bAttr, &usChar ) )
            break;
         if( ( pWVT->fontAttribute & ZH_GTI_FONTA_CTRLCHARS ) == 0 )
            usChar = zh_cdpGetU16Ctrl( usChar );

         if( pWVT->wcTrans )
         {
            if( pWVT->wcTransLen == 0x100 && ( usChar >> 8 ) == 0xFF )
               usChar &= 0x00FF;
            if( ( ZH_SIZE ) usChar < pWVT->wcTransLen && pWVT->wcTrans[ usChar ] )
               usChar = pWVT->wcTrans[ usChar ];
         }

         /* as long as GTWVT uses only 16 colors we can ignore other bits
          * and not divide output when it does not change anythings
          */
         iColor &= 0xff;
         hBitMap = zh_gt_wvt_GetBoxChar( pWVT, &usChar );
         if( len > 0 && ( iColor != iOldColor || fFixMetric || hBitMap ) )
         {
            zh_gt_wvt_TextOut( pWVT, hdc, startCol, iRow, iOldColor, pWVT->TextLine, ( UINT ) len );
            len = 0;
         }
         if( hBitMap )
         {
            POINT xy;
            /* set foreground color */
            SetTextColor( hdc, pWVT->COLORS[ iColor & 0x0F ] );
            /* set background color */
            SetBkColor( hdc, pWVT->COLORS[ ( iColor >> 4 ) & 0x0F ] );
            xy = zh_gt_wvt_GetXYFromColRow( pWVT, iCol, iRow );
            SelectObject( pWVT->hBmpDC, hBitMap );
            BitBlt( hdc, xy.x, xy.y, pWVT->PTEXTSIZE.x + 1, pWVT->PTEXTSIZE.y + 1,
                    pWVT->hBmpDC, 0, 0, SRCCOPY );
         }
         else
         {
            if( len == 0 )
            {
               iOldColor = iColor;
               startCol = iCol;
            }
            pWVT->TextLine[ len++ ] = ( TCHAR ) usChar;
         }
#else
         ZH_UCHAR uc;
         if( ! ZH_GTSELF_GETSCRUC( pWVT->pGT, iRow, iCol, &iColor, &bAttr, &uc, ZH_TRUE ) )
            break;
         hFont = ( bAttr & ZH_GT_ATTR_BOX ) ? pWVT->hFontBox : pWVT->hFont;
         if( len == 0 )
         {
            if( hFont != hOldFont )
            {
               SelectObject( hdc, hFont );
               hOldFont = hFont;
            }
            iOldColor = iColor;
         }
         else if( iColor != iOldColor || hFont != hOldFont || fFixMetric )
         {
            zh_gt_wvt_TextOut( pWVT, hdc, startCol, iRow, iOldColor, pWVT->TextLine, ( UINT ) len );
            if( hFont != hOldFont )
            {
               SelectObject( hdc, hFont );
               hOldFont = hFont;
            }
            iOldColor = iColor;
            startCol = iCol;
            len = 0;
         }
         pWVT->TextLine[ len++ ] = ( TCHAR ) uc;
#endif
         iCol++;
      }
      if( len > 0 )
         zh_gt_wvt_TextOut( pWVT, hdc, startCol, iRow, iOldColor, pWVT->TextLine, ( UINT ) len );
   }
   EndPaint( pWVT->hWnd, &ps );
}

static LRESULT CALLBACK zh_gt_wvt_WndProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   PZH_GTWVT pWVT = zh_gt_wvt_Find( hWnd );

   if( ! pWVT )
   {
      if( message == WM_CREATE )
      {
         pWVT = ( PZH_GTWVT ) ( ( LPCREATESTRUCT ) lParam )->lpCreateParams;
         if( pWVT )
         {
            if( s_wvtWindows[ pWVT->iHandle ] == pWVT )
               pWVT->hWnd = hWnd;
            else
               pWVT = NULL;
         }
      }
   }

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_wvt_WndProc(%p,%u)", ( void * ) hWnd, message ) );

   if( pWVT ) switch( message )
   {
      case WM_CREATE:
         return zh_gt_wvt_InitWindow( pWVT, pWVT->ROWS, pWVT->COLS, NULL ) ? 0 : -1;

      case WM_PAINT:
         if( GetUpdateRect( hWnd, NULL, FALSE ) )
            zh_gt_wvt_PaintText( pWVT );
         return 0;

      case WM_MY_UPDATE_CARET:
         zh_gt_wvt_UpdateCaret( pWVT );
         return 0;

      case WM_SETFOCUS:
         zh_gt_wvt_UpdateCaret( pWVT );
         return 0;

      case WM_KILLFOCUS:
         zh_gt_wvt_KillCaret( pWVT );
         return 0;

      case WM_KEYDOWN:
      case WM_SYSKEYDOWN:
      case WM_CHAR:
      case WM_SYSCHAR:
         return zh_gt_wvt_KeyEvent( pWVT, message, wParam, lParam );

      case WM_RBUTTONDOWN:
      case WM_LBUTTONDOWN:
      case WM_RBUTTONUP:
      case WM_LBUTTONUP:
      case WM_RBUTTONDBLCLK:
      case WM_LBUTTONDBLCLK:
      case WM_MBUTTONDOWN:
      case WM_MBUTTONUP:
      case WM_MBUTTONDBLCLK:
      case WM_MOUSEMOVE:
      case WM_MOUSEWHEEL:
      case WM_NCMOUSEMOVE:
         zh_gt_wvt_MouseEvent( pWVT, message, wParam, lParam );
         return 0;

      case WM_QUERYENDSESSION: /* check if we can shutdown or logoff */
         return 1;

#if defined( WM_ENDSESSION )
      case WM_ENDSESSION: /* shutdown started */
         if( wParam )
            zh_vmRequestQuit();
         return 0;
#endif

      case WM_CLOSE:  /* Clicked 'X' on system menu */
         if( pWVT->CloseMode == 0 )
            zh_vmRequestQuit();
         else
            zh_gt_wvt_AddCharToInputQueue( pWVT, ZH_K_CLOSE );
         return 0;

      case WM_QUIT:
      case WM_DESTROY:
         return 0;

      case WM_ENTERIDLE:
         /* FSG - 2004-05-12 - Signal than i'm on idle */
         zh_idleState();
         return 0;

      /* Pritpal Bedi - 2008-06-06 */
      case WM_ACTIVATE:
         zh_gt_wvt_AddCharToInputQueue( pWVT, LOWORD( wParam ) == WA_INACTIVE ? ZH_K_LOSTFOCUS : ZH_K_GOTFOCUS );
         return 0;

      case WM_ENTERSIZEMOVE:
         GetClientRect( pWVT->hWnd, &pWVT->ciLast );  /* need in Paint function, client area before sizing started */
         pWVT->bResizing = ZH_TRUE;
         return 0;

      case WM_EXITSIZEMOVE:
         pWVT->bResizing = ZH_FALSE;
         if( pWVT->bAlreadySizing )
         /* user was resizing as opposed to moving window */
         {
            zh_gt_wvt_FitRows( pWVT );
            pWVT->bAlreadySizing = ZH_FALSE;
         }
         return 0;

      case WM_SIZE:
         if( ! pWVT->bFullScreen )
         {
            if( pWVT->bResizing && pWVT->ResizeMode == ZH_GTI_RESIZEMODE_ROWS )
               pWVT->bAlreadySizing = ZH_TRUE;
            else if( pWVT->ResizeMode == ZH_GTI_RESIZEMODE_FONT )
            {
               if( ! pWVT->bAlreadySizing )
                  zh_gt_wvt_FitSize( pWVT );
            }
            else
               /* resize came from Maximize, Restore, other than mouse resizing... */
               zh_gt_wvt_FitRows( pWVT );
         }
         return 0;

      case WM_DPICHANGED:
         /* TODO: implement */
         break;

      case WM_SYSCOMMAND:
         if( wParam == SYS_EV_MARK )
         {
            pWVT->bBeginMarked = ZH_TRUE;
            return 0;
         }
         else if( wParam > SYS_EV_MARK )
         {
            PZH_GTWVT_MNU pMenu = pWVT->pMenu;
            while( pMenu )
            {
               if( ( WPARAM ) pMenu->iEvent == wParam )
               {
                  zh_gt_wvt_AddCharToInputQueue( pWVT, pMenu->iKey );
                  return 0;
               }
               pMenu = pMenu->pNext;
            }
         }
         break;
   }

   return DefWindowProc( hWnd, message, wParam, lParam );
}

static WPARAM zh_gt_wvt_ProcessMessages( void )
{
   MSG msg;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_wvt_ProcessMessages()" ) );

   while( PeekMessage( &msg, NULL, 0, 0, PM_REMOVE ) )
   {
      TranslateMessage( &msg );
      DispatchMessage( &msg );
   }
   return msg.wParam;
}


static void zh_gt_wvt_CreateWindow( PZH_GTWVT pWVT )
{
   DWORD dwStyle;

   #if 0
   InitCommonControls();
   #endif

   dwStyle = pWVT->bResizable ? _WVT_WS_DEF : _WVT_WS_NORESIZE;

   pWVT->hWnd = CreateWindow( s_szClassName,       /* classname */
                              pWVT->lpWindowTitle, /* window name */
                              dwStyle,             /* style */
                              0,                   /* x */
                              0,                   /* y */
                              CW_USEDEFAULT,       /* width */
                              CW_USEDEFAULT,       /* height */
                              NULL,                /* window parent */
                              NULL,                /* menu */
                              pWVT->hInstance,     /* instance */
                              ( LPVOID ) pWVT );   /* lpParam */
}

static ZH_BOOL zh_gt_wvt_CreateConsoleWindow( PZH_GTWVT pWVT )
{
   if( ! pWVT->hWnd )
   {
      zh_gt_wvt_CreateWindow( pWVT );
      if( pWVT->hWnd )
      {
         zh_gt_wvt_Composited( pWVT, ZH_TRUE );

         /* Set icon */
         if( pWVT->hIcon )
         {
            SendNotifyMessage( pWVT->hWnd, WM_SETICON, ICON_SMALL, ( LPARAM ) pWVT->hIcon ); /* Set Title Bar Icon */
            SendNotifyMessage( pWVT->hWnd, WM_SETICON, ICON_BIG  , ( LPARAM ) pWVT->hIcon ); /* Set Task List Icon */
         }

         {
            HMENU hSysMenu = GetSystemMenu( pWVT->hWnd, FALSE );

            if( hSysMenu )
            {
               PZH_GTWVT_MNU pMenu = pWVT->pMenu;

               /* Create "Mark" prompt in SysMenu to allow console type copy operation */
               AppendMenu( hSysMenu, MF_STRING, SYS_EV_MARK, pWVT->lpSelectCopy );
               /* CloseButton [x] and "Close" window menu item */
               EnableMenuItem( hSysMenu, SC_CLOSE, MF_BYCOMMAND |
                               ( pWVT->CloseMode < 2 ? MF_ENABLED : MF_GRAYED ) );

               /* Create user menu */
               while( pMenu )
               {
                  AppendMenu( hSysMenu, MF_STRING, pMenu->iEvent, pMenu->lpName );
                  pMenu = pMenu->pNext;
               }
            }
         }
         if( pWVT->bFullScreen )
         {
            pWVT->bMaximized = ZH_FALSE;
            pWVT->bFullScreen = ZH_FALSE;
            zh_gt_wvt_FullScreen( pWVT->pGT );
         }
         else
         {
            if( pWVT->iNewPosX >= 0 && pWVT->iNewPosY >= 0 )
            {
               RECT wi = { 0, 0, 0, 0 };
               GetWindowRect( pWVT->hWnd, &wi );
               SetWindowPos( pWVT->hWnd, NULL, pWVT->iNewPosX, pWVT->iNewPosY,
                             wi.right - wi.left, wi.bottom - wi.top,
                             SWP_NOSIZE | SWP_NOZORDER );
            }
            ShowWindow( pWVT->hWnd, pWVT->bMaximized ? SW_SHOWMAXIMIZED : pWVT->iCmdShow );
            UpdateWindow( pWVT->hWnd );
         }
      }
      else
         zh_errInternal( 10001, "Failed to create WVT window", NULL, NULL );
   }

   return ZH_TRUE;
}

static ZH_BOOL zh_gt_wvt_FullScreen( PZH_GT pGT )
{
   PZH_GTWVT pWVT;
   RECT rt;

   ZH_GTWVT_LONG_PTR nStyle;
   ZH_GTWVT_LONG_PTR nExtendedStyle;

/* Don't need this as Windows automatically maximizes to nearest [HVB] */
#if defined( MONITOR_DEFAULTTONEAREST ) && 0
   HMONITOR mon;
   MONITORINFO mi;
   typedef HMONITOR ( WINAPI * P_MFW )( HWND, DWORD );
   typedef BOOL ( WINAPI * P_GMI )( HMONITOR, LPMONITORINFO );
   P_MFW pMonitorFromWindow;
   P_GMI pGetMonitorInfo;
#endif

   pWVT = ZH_GTWVT_GET( pGT );

   nStyle = GetWindowLongPtr( pWVT->hWnd, GWL_STYLE );
   nExtendedStyle = GetWindowLongPtr( pWVT->hWnd, GWL_EXSTYLE );

   if( pWVT->bFullScreen )
   {
      nStyle |= WS_CAPTION | WS_BORDER;
      nExtendedStyle |= WS_EX_TOPMOST;

      if( pWVT->bResizable )
         nStyle |= WS_THICKFRAME;

      pWVT->MarginLeft = 0;
      pWVT->MarginTop = 0;
      pWVT->bFullScreen = ZH_FALSE;
   }
   else
   {
      nStyle &= ~( WS_CAPTION | WS_BORDER | WS_THICKFRAME );
      nExtendedStyle &= ~WS_EX_TOPMOST;
      pWVT->bFullScreen = ZH_TRUE;
   }

   SetWindowLongPtr( pWVT->hWnd, GWL_STYLE, nStyle );
   SetWindowLongPtr( pWVT->hWnd, GWL_EXSTYLE, nExtendedStyle );

   if( ! pWVT->bFullScreen )
   {
      ShowWindow( pWVT->hWnd, SW_RESTORE );
      return ZH_FALSE;
   }

   if( ! pWVT->bMaximized )
      ShowWindow( pWVT->hWnd, SW_SHOWMAXIMIZED );

/* Don't need as Windows automatically maximizes to nearest.
 * That is as long as we use the RECT that Windows provides
 * and don't handle WM_MINMAXWINDOW or other related messages
 * and don't change the RECT coordinates (may have negative
 * numbers for top or left depending on how user configured
 * monitors relationship and which on is the primary). [HVB]
 */
#if 0
#ifdef MONITOR_DEFAULTTONEAREST
   {
      HMODULE hModule = GetModuleHandle( TEXT( "user32.dll" ) );

      if( hModule )
      {
         pMonitorFromWindow = ( P_MFW ) ZH_WINAPI_GETPROCADDRESS( hModule, "MonitorFromWindow" );
         pGetMonitorInfo = ( P_GMI ) ZH_WINAPI_GETPROCADDRESS( hModule, "GetMonitorInfo" );
      }
      else
      {
         pMonitorFromWindow = NULL;
         pGetMonitorInfo = NULL;
      }

      if( pMonitorFromWindow && pGetMonitorInfo )
      {
         mon = pMonitorFromWindow( pWVT->hWnd, MONITOR_DEFAULTTONEAREST );
         mi.cbSize = sizeof( mi );
         pGetMonitorInfo( mon, &mi );
         rt = mi.rcMonitor;
      }
      else
         GetClientRect( GetDesktopWindow(), &rt );
   }
#else
   GetClientRect( GetDesktopWindow(), &rt );
#endif
#endif

   GetClientRect( GetDesktopWindow(), &rt );

   SetWindowPos( pWVT->hWnd, HWND_TOP, rt.left, rt.top,
                 rt.right - rt.left,
                 rt.bottom - rt.top,
                 SWP_FRAMECHANGED );

   if( pWVT->ResizeMode == ZH_GTI_RESIZEMODE_FONT )
      zh_gt_wvt_FitSize( pWVT );
   else
      zh_gt_wvt_FitRows( pWVT );

   return ZH_TRUE;
}

/* ********************************************************************** */
/*
 * GT Specific Functions
 */
/* ********************************************************************** */

static void zh_gt_wvt_Init( PZH_GT pGT, ZH_FHANDLE hFilenoStdin, ZH_FHANDLE hFilenoStdout, ZH_FHANDLE hFilenoStderr )
{
   HINSTANCE hInstance;
   int       iCmdShow;
   PZH_GTWVT pWVT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_wvt_Init(%p,%p,%p,%p)", ( void * ) pGT, ( void * ) ( ZH_PTRUINT ) hFilenoStdin, ( void * ) ( ZH_PTRUINT ) hFilenoStdout, ( void * ) ( ZH_PTRUINT ) hFilenoStderr ) );

   if( ! zh_winmainArgGet( &hInstance, NULL, &iCmdShow ) )
   {
      hInstance = ( HINSTANCE ) GetModuleHandle( NULL );
      iCmdShow = 1;
   }

   pWVT = zh_gt_wvt_New( pGT, hInstance, iCmdShow );
   if( pWVT )
   {
      ZH_GTLOCAL( pGT ) = ( void * ) pWVT;

      /* SUPER GT initialization */
      ZH_GTSUPER_INIT( pGT, hFilenoStdin, hFilenoStdout, hFilenoStderr );
      ZH_GTSELF_RESIZE( pGT, pWVT->ROWS, pWVT->COLS );
      ZH_GTSELF_SETFLAG( pGT, ZH_GTI_REDRAWMAX, 1 );
      ZH_GTSELF_SEMICOLD( pGT );

      #if 0
      zh_gt_wvt_CreateConsoleWindow( pWVT );
      #endif
   }
   else
      zh_errInternal( 10001, "Maximum number of WVT windows reached, cannot create another one", NULL, NULL );
}

/* ********************************************************************** */

static void zh_gt_wvt_Exit( PZH_GT pGT )
{
   PZH_GTWVT pWVT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_wvt_Exit(%p)", ( void * ) pGT ) );

   pWVT = ZH_GTWVT_GET( pGT );
   ZH_GTSUPER_EXIT( pGT );

   if( pWVT )
      zh_gt_wvt_Free( pWVT );
}

/* ********************************************************************** */

static ZH_BOOL zh_gt_wvt_SetMode( PZH_GT pGT, int iRow, int iCol )
{
   PZH_GTWVT pWVT;
   ZH_BOOL fResult = ZH_FALSE;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_wvt_SetMode(%p,%d,%d)", ( void * ) pGT, iRow, iCol ) );

   pWVT = ZH_GTWVT_GET( pGT );

   if( pWVT->hWnd ) /* Is the window already open? */
   {
      if( pWVT->bResizable && ! pWVT->bMaximized )
      {
         fResult = zh_gt_wvt_InitWindow( pWVT, iRow, iCol, NULL );
         ZH_GTSELF_REFRESH( pGT );
      }
      else
      {
         /* We're Maximized, Fullscreen or in an unsizable window
          * Change Font size to fit new mode settings into the window
          */
         HFONT hFont = zh_gt_wvt_GetFont( pWVT->fontFace, ( pWVT->fontHeight * pWVT->ROWS ) / iRow, ( pWVT->fontWidth * pWVT->COLS ) / iCol,
                                          pWVT->fontWeight, pWVT->fontQuality, pWVT->CodePage );
         if( hFont )
         {
            fResult = zh_gt_wvt_InitWindow( pWVT, iRow, iCol, hFont );
            zh_gt_wvt_FitSize( pWVT );
            if( pWVT->bMaximized )
            {
               /* Window size not changed, but Margins may have changed, repaint the client area [HVB] */
               InvalidateRect( pWVT->hWnd, NULL, TRUE );
               UpdateWindow( pWVT->hWnd );
            }

            ZH_GTSELF_REFRESH( pGT );
         }
      }
   }
   else
   {
      fResult = zh_gt_wvt_SetWindowSize( pWVT, iRow, iCol );
      ZH_GTSELF_SEMICOLD( pGT );
   }

   return fResult;
}

/* ********************************************************************** */

static const char * zh_gt_wvt_Version( PZH_GT pGT, int iType )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_wvt_Version(%p,%d)", ( void * ) pGT, iType ) );

   ZH_SYMBOL_UNUSED( pGT );

   if( iType == 0 )
      return ZH_GT_DRVNAME( ZH_GT_NAME );

   return "Terminal: Windows native GDI (WVT)";
}

/* ********************************************************************** */

static int zh_gt_wvt_ReadKey( PZH_GT pGT, int iEventMask )
{
   PZH_GTWVT pWVT;
   int c = 0;
   ZH_BOOL fKey;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_wvt_ReadKey(%p,%d)", ( void * ) pGT, iEventMask ) );

   ZH_SYMBOL_UNUSED( iEventMask ); /* we ignore the eventmask! */

   pWVT = ZH_GTWVT_GET( pGT );

   if( pWVT->hWnd ) /* Is the window already open? */
      zh_gt_wvt_ProcessMessages();

   fKey = zh_gt_wvt_GetCharFromInputQueue( pWVT, &c );

   return fKey ? c : 0;
}

/* ********************************************************************** */
/* dDuration is in 'Ticks' (18.2 per second) */
static void zh_gt_wvt_Tone( PZH_GT pGT, double dFrequency, double dDuration )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_wvt_Tone(%p,%lf,%lf)", ( void * ) pGT, dFrequency, dDuration ) );

   ZH_SYMBOL_UNUSED( pGT );

   zh_gt_BaseUnlock( pGT );
   zh_gt_winapi_tone( dFrequency, dDuration );
   zh_gt_BaseLock( pGT );
}

/* ********************************************************************** */

static ZH_BOOL zh_gt_wvt_mouse_IsPresent( PZH_GT pGT )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_wvt_mouse_IsPresent(%p)", ( void * ) pGT ) );

   ZH_SYMBOL_UNUSED( pGT );

   return ZH_TRUE;
}

static void zh_gt_wvt_mouse_GetPos( PZH_GT pGT, int * piRow, int * piCol )
{
   PZH_GTWVT pWVT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_wvt_mouse_GetPos(%p,%p,%p)", ( void * ) pGT, ( void * ) piRow, ( void * ) piCol ) );

   pWVT = ZH_GTWVT_GET( pGT );
   *piRow = pWVT->MousePos.y;
   *piCol = pWVT->MousePos.x;
}

static void zh_gt_wvt_mouse_SetPos( PZH_GT pGT, int iRow, int iCol )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_wvt_mouse_SetPos(%p,%i,%i)", ( void * ) pGT, iRow, iCol ) );

   zh_gt_wvt_SetMousePos( ZH_GTWVT_GET( pGT ), iRow, iCol );
}

static ZH_BOOL zh_gt_wvt_mouse_ButtonState( PZH_GT pGT, int iButton )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_wvt_mouse_ButtonState(%p,%i)", ( void * ) pGT, iButton ) );

   ZH_SYMBOL_UNUSED( pGT );

   switch( iButton )
   {
      case 0:
         return ( GetKeyState( VK_LBUTTON ) & 0x8000 ) != 0;
      case 1:
         return ( GetKeyState( VK_RBUTTON ) & 0x8000 ) != 0;
      case 2:
         return ( GetKeyState( VK_MBUTTON ) & 0x8000 ) != 0;
   }
   return ZH_FALSE;
}

static int zh_gt_wvt_mouse_CountButton( PZH_GT pGT )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_wvt_mouse_CountButton(%p)", ( void * ) pGT ) );

   ZH_SYMBOL_UNUSED( pGT );

   return GetSystemMetrics( SM_CMOUSEBUTTONS );
}

/* ********************************************************************** */

static ZH_BOOL zh_gt_wvt_Info( PZH_GT pGT, int iType, PZH_GT_INFO pInfo )
{
   PZH_GTWVT pWVT;
   int iVal;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_wvt_Info(%p,%d,%p)", ( void * ) pGT, iType, ( void * ) pInfo ) );

   pWVT = ZH_GTWVT_GET( pGT );

   switch( iType )
   {
      case ZH_GTI_MAXIMIZED:
         pInfo->pResult = zh_itemPutL( pInfo->pResult, pWVT->bMaximized );
         if( zh_itemType( pInfo->pNewVal ) & ZH_IT_LOGICAL )
         {
            if( zh_itemGetL( pInfo->pNewVal ) != pWVT->bMaximized && ! pWVT->bFullScreen )
            {
               if( ! pWVT->hWnd )
                  pWVT->bMaximized = zh_itemGetL( pInfo->pNewVal );
               else if( pWVT->bMaximized )
                  /* Restore Window */
                  ShowWindow( pWVT->hWnd, SW_RESTORE );
               else
                  /* Maximize Window */
                  ShowWindow( pWVT->hWnd, SW_SHOWMAXIMIZED );
            }
         }
         break;

      case ZH_GTI_ISFULLSCREEN:
         pInfo->pResult = zh_itemPutL( pInfo->pResult, pWVT->bFullScreen );
         if( zh_itemType( pInfo->pNewVal ) & ZH_IT_LOGICAL )
         {
            if( zh_itemGetL( pInfo->pNewVal ) != pWVT->bFullScreen )
            {
               if( pWVT->hWnd )
                  zh_gt_wvt_FullScreen( pGT );
               else
                  pWVT->bFullScreen = zh_itemGetL( pInfo->pNewVal );
            }
         }
         break;

      case ZH_GTI_ALTENTER:
         pInfo->pResult = zh_itemPutL( pInfo->pResult, pWVT->bAltEnter );
         if( zh_itemType( pInfo->pNewVal ) & ZH_IT_LOGICAL )
            pWVT->bAltEnter = zh_itemGetL( pInfo->pNewVal );
         break;

      case ZH_GTI_ISSCREENPOS:
      case ZH_GTI_KBDSUPPORT:
      case ZH_GTI_ISGRAPHIC:
         pInfo->pResult = zh_itemPutL( pInfo->pResult, ZH_TRUE );
         break;

      case ZH_GTI_ONLINE:
         pInfo->pResult = zh_itemPutL( pInfo->pResult, pWVT->hWnd != NULL );
         break;

      case ZH_GTI_ISUNICODE:
#if defined( UNICODE )
         pInfo->pResult = zh_itemPutL( pInfo->pResult, ZH_TRUE );
#else
         pInfo->pResult = zh_itemPutL( pInfo->pResult, ZH_FALSE );
#endif
         break;

      case ZH_GTI_INPUTFD:
         pInfo->pResult = zh_itemPutNInt( pInfo->pResult,
                                          ( ZH_PTRUINT ) GetStdHandle( STD_INPUT_HANDLE ) );
         break;

      case ZH_GTI_OUTPUTFD:
         pInfo->pResult = zh_itemPutNInt( pInfo->pResult,
                              ( ZH_PTRUINT ) GetStdHandle( STD_OUTPUT_HANDLE ) );
         break;

      case ZH_GTI_ERRORFD:
         pInfo->pResult = zh_itemPutNInt( pInfo->pResult,
                              ( ZH_PTRUINT ) GetStdHandle( STD_ERROR_HANDLE ) );
         break;

      case ZH_GTI_FONTSIZE:
         pInfo->pResult = zh_itemPutNI( pInfo->pResult, pWVT->PTEXTSIZE.y );
         iVal = zh_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 )
         {
            HFONT hFont = zh_gt_wvt_GetFont( pWVT->fontFace, iVal, pWVT->fontWidth, pWVT->fontWeight, pWVT->fontQuality, pWVT->CodePage );
            if( hFont )
            {
               pWVT->fontHeight = iVal;
               if( pWVT->hWnd )
               {
                  zh_gt_wvt_ResetWindowSize( pWVT, hFont );
                  ZH_GTSELF_REFRESH( pGT );
               }
               else
               {
                  TEXTMETRIC tm;
                  HWND       hDesk    = GetDesktopWindow();
                  HDC        hdc      = GetDC( hDesk );
                  HFONT      hOldFont = ( HFONT ) SelectObject( hdc, hFont );

                  SetTextCharacterExtra( hdc, 0 );
                  GetTextMetrics( hdc, &tm );
                  SelectObject( hdc, hOldFont );
                  ReleaseDC( hDesk, hdc );

                  pWVT->PTEXTSIZE.x = tm.tmAveCharWidth;
                  pWVT->PTEXTSIZE.y = tm.tmHeight;

                  DeleteObject( hFont );
               }
            }
         }
         break;

      case ZH_GTI_FONTWIDTH:
         pInfo->pResult = zh_itemPutNI( pInfo->pResult, pWVT->fontWidth );
         iVal = zh_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 )
            pWVT->fontWidth = iVal;  /* store font status for next operation on fontsize */
         break;

      case ZH_GTI_FONTNAME:
         pInfo->pResult = ZH_ITEMPUTSTR( pInfo->pResult, pWVT->fontFace );
         if( zh_itemType( pInfo->pNewVal ) & ZH_IT_STRING )
         {
            ZH_ITEMCOPYSTR( pInfo->pNewVal, pWVT->fontFace, ZH_SIZEOFARRAY( pWVT->fontFace ) );
            pWVT->fontFace[ ZH_SIZEOFARRAY( pWVT->fontFace ) - 1 ] = TEXT( '\0' );
         }
         break;

      case ZH_GTI_FONTWEIGHT:
         switch( pWVT->fontWeight )
         {
            case FW_THIN:
            case FW_EXTRALIGHT:
            case FW_LIGHT:
               iVal = ZH_GTI_FONTW_THIN;
               break;

            case FW_DONTCARE:
            case FW_NORMAL:
            case FW_MEDIUM:
               iVal = ZH_GTI_FONTW_NORMAL;
               break;

            case FW_SEMIBOLD:
            case FW_BOLD:
            case FW_EXTRABOLD:
            case FW_HEAVY:
               iVal = ZH_GTI_FONTW_BOLD;
               break;

            default:
               iVal = 0;
               break;
         }
         pInfo->pResult = zh_itemPutNI( pInfo->pResult, iVal );
         if( zh_itemType( pInfo->pNewVal ) & ZH_IT_NUMERIC )
         {
            /* store font status for next operation on fontsize */
            switch( zh_itemGetNI( pInfo->pNewVal ) )
            {
               case ZH_GTI_FONTW_THIN:
                  pWVT->fontWeight = FW_LIGHT;
                  break;
               case ZH_GTI_FONTW_NORMAL:
                  pWVT->fontWeight = FW_NORMAL;
                  break;
               case ZH_GTI_FONTW_BOLD:
                  pWVT->fontWeight = FW_BOLD;
                  break;
            }
         }
         break;

      case ZH_GTI_FONTQUALITY:
         switch( pWVT->fontQuality )
         {
            case ANTIALIASED_QUALITY:
               iVal = ZH_GTI_FONTQ_HIGH;
               break;
            case DEFAULT_QUALITY:
            case DRAFT_QUALITY:
               iVal = ZH_GTI_FONTQ_NORMAL;
               break;
            case NONANTIALIASED_QUALITY:
            case PROOF_QUALITY:
               iVal = ZH_GTI_FONTQ_DRAFT;
               break;
            default:
               iVal = 0;
               break;
         }
         pInfo->pResult = zh_itemPutNI( pInfo->pResult, iVal );
         if( zh_itemType( pInfo->pNewVal ) & ZH_IT_NUMERIC )
         {
            switch( zh_itemGetNI( pInfo->pNewVal ) )
            {
               case ZH_GTI_FONTQ_HIGH:
                  pWVT->fontQuality = ANTIALIASED_QUALITY;
                  break;
               case ZH_GTI_FONTQ_NORMAL:
                  pWVT->fontQuality = DEFAULT_QUALITY;
                  break;
               case ZH_GTI_FONTQ_DRAFT:
                  pWVT->fontQuality = DRAFT_QUALITY;
                  break;
            }
         }
         break;

      case ZH_GTI_FONTATTRIBUTE:
         pInfo->pResult = zh_itemPutNI( pInfo->pResult, pWVT->fontAttribute );
         if( zh_itemType( pInfo->pNewVal ) & ZH_IT_NUMERIC )
            pWVT->fontAttribute = zh_itemGetNI( pInfo->pNewVal ) &
                                           ( ZH_GTI_FONTA_FIXMETRIC |
                                             ZH_GTI_FONTA_CLRBKG    |
                                             ZH_GTI_FONTA_CTRLCHARS |
                                             ZH_GTI_FONTA_DRAWBOX );
         break;

      case ZH_GTI_FONTSEL:
         pInfo->pResult = zh_itemPutC( pInfo->pResult, NULL );
         break;

      case ZH_GTI_SCREENHEIGHT:
         pInfo->pResult = zh_itemPutNI( pInfo->pResult, pWVT->PTEXTSIZE.y * pWVT->ROWS );
         iVal = zh_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 && ! pWVT->bMaximized && ! pWVT->bFullScreen && pWVT->hWnd )  /* Don't allow if Maximized or FullScreen */
         {
            /* Now conforms to pWVT->ResizeMode setting, resize by FONT or ROWS as applicable [HVB] */
            RECT ci;
            GetClientRect( pWVT->hWnd, &ci );
            if( ci.bottom != iVal )
            {
               RECT wi;
               GetWindowRect( pWVT->hWnd, &wi );
               iVal += wi.bottom - wi.top - ci.bottom;
               SetWindowPos( pWVT->hWnd, NULL, wi.left, wi.top, wi.right - wi.left, iVal, SWP_NOZORDER );
            }
         }
         break;

      case ZH_GTI_SCREENWIDTH:
         pInfo->pResult = zh_itemPutNI( pInfo->pResult, pWVT->PTEXTSIZE.x * pWVT->COLS );
         iVal = zh_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 && ! pWVT->bMaximized && ! pWVT->bFullScreen && pWVT->hWnd )  /* Don't allow if Maximized or FullScreen */
         {
            /* Now conforms to pWVT->ResizeMode setting, resize by FONT or ROWS as applicable [HVB] */
            RECT ci;
            GetClientRect( pWVT->hWnd, &ci );
            if( ci.right != iVal )
            {
               RECT wi;
               GetWindowRect( pWVT->hWnd, &wi );
               iVal += wi.right - wi.left - ci.right;
               SetWindowPos( pWVT->hWnd, NULL, wi.left, wi.top, iVal, wi.bottom - wi.top, SWP_NOZORDER );
            }
         }
         break;

      case ZH_GTI_DESKTOPWIDTH:
      {
         RECT rDesk;
         HWND hDesk = GetDesktopWindow();
         GetWindowRect( hDesk, &rDesk );
         pInfo->pResult = zh_itemPutNI( pInfo->pResult, rDesk.right - rDesk.left );
         break;
      }
      case ZH_GTI_DESKTOPHEIGHT:
      {
         RECT rDesk;
         HWND hDesk = GetDesktopWindow();
         GetWindowRect( hDesk, &rDesk );
         pInfo->pResult = zh_itemPutNI( pInfo->pResult, rDesk.bottom - rDesk.top );
         break;
      }
      case ZH_GTI_DESKTOPCOLS:
      {
         RECT rDesk;
         HWND hDesk = GetDesktopWindow();
         GetClientRect( hDesk, &rDesk );
         pInfo->pResult = zh_itemPutNI( pInfo->pResult,
                              ( rDesk.right - rDesk.left ) / pWVT->PTEXTSIZE.x );
         break;
      }
      case ZH_GTI_DESKTOPROWS:
      {
         RECT rDesk;
         HWND hDesk = GetDesktopWindow();
         GetClientRect( hDesk, &rDesk );
         pInfo->pResult = zh_itemPutNI( pInfo->pResult,
                              ( rDesk.bottom - rDesk.top ) / pWVT->PTEXTSIZE.y );
         break;
      }
      case ZH_GTI_WINTITLE:
         pInfo->pResult = ZH_ITEMPUTSTR( pInfo->pResult, pWVT->lpWindowTitle );
         if( zh_itemType( pInfo->pNewVal ) & ZH_IT_STRING )
         {
            zh_strfree( pWVT->hWindowTitle );
            pWVT->lpWindowTitle = ZH_ITEMGETSTR( pInfo->pNewVal, &pWVT->hWindowTitle, NULL );
            if( pWVT->hWnd )
               SetWindowText( pWVT->hWnd, pWVT->lpWindowTitle );
         }
         break;

      case ZH_GTI_CODEPAGE:
         pInfo->pResult = zh_itemPutNI( pInfo->pResult, pWVT->CodePage );
         if( zh_itemType( pInfo->pNewVal ) & ZH_IT_NUMERIC )
         {
            iVal = zh_itemGetNI( pInfo->pNewVal );
            if( iVal != pWVT->CodePage )
            {
               if( ! pWVT->hWnd )
                  pWVT->CodePage = iVal;
#if ! defined( UNICODE )
               else if( iVal == pWVT->boxCodePage )
               {
                  if( pWVT->hFont != pWVT->hFontBox )
                  {
                     if( pWVT->hFont )
                        DeleteObject( pWVT->hFont );
                     pWVT->hFont = pWVT->hFontBox;
                  }
                  pWVT->CodePage = iVal;
               }
#endif
               else
               {
                  HFONT hFont = zh_gt_wvt_GetFont( pWVT->fontFace, pWVT->fontHeight, pWVT->fontWidth,
                                                   pWVT->fontWeight, pWVT->fontQuality, iVal );
                  if( hFont )
                  {
#if ! defined( UNICODE )
                     if( pWVT->hFont && pWVT->hFont != pWVT->hFontBox )
#else
                     if( pWVT->hFont )
#endif
                        DeleteObject( pWVT->hFont );
                     pWVT->hFont = hFont;
                     pWVT->CodePage = iVal;
                  }
               }
            }
         }
         break;

#if ! defined( UNICODE )
      case ZH_GTI_BOXCP:
         pInfo->pResult = zh_itemPutNI( pInfo->pResult, pWVT->boxCodePage );
         if( zh_itemType( pInfo->pNewVal ) & ZH_IT_NUMERIC )
         {
            iVal = zh_itemGetNI( pInfo->pNewVal );
            if( iVal != pWVT->boxCodePage )
            {
               if( ! pWVT->hWnd )
                  pWVT->boxCodePage = iVal;
               else if( iVal == pWVT->CodePage )
               {
                  if( pWVT->hFontBox != pWVT->hFont )
                  {
                     if( pWVT->hFontBox )
                        DeleteObject( pWVT->hFontBox );
                     pWVT->hFontBox = pWVT->hFont;
                  }
                  pWVT->boxCodePage = iVal;
               }
               else
               {
                  HFONT hFont = zh_gt_wvt_GetFont( pWVT->fontFace, pWVT->fontHeight, pWVT->fontWidth,
                                                   pWVT->fontWeight, pWVT->fontQuality, iVal );
                  if( hFont )
                  {
                     if( pWVT->hFontBox && pWVT->hFontBox != pWVT->hFont )
                        DeleteObject( pWVT->hFontBox );
                     pWVT->hFontBox = hFont;
                     pWVT->boxCodePage = iVal;
                  }
               }
            }
         }
         break;

      case ZH_GTI_UNITRANS:
         break;
#else
      case ZH_GTI_UNITRANS:
         if( pWVT->wcTrans )
            pInfo->pResult = zh_itemPutCL( pInfo->pResult, ( char * ) pWVT->wcTrans,
                                           pWVT->wcTransLen * sizeof( ZH_WCHAR ) );
         if( zh_itemType( pInfo->pNewVal ) & ZH_IT_STRING )
         {
            if( pWVT->wcTrans )
            {
               zh_itemFreeC( ( char * ) pWVT->wcTrans );
               pWVT->wcTrans = NULL;
            }
            pWVT->wcTransLen = zh_itemGetCLen( pInfo->pNewVal ) / sizeof( ZH_WCHAR );
            if( pWVT->wcTransLen > 0 )
               pWVT->wcTrans = pWVT->wcTransLen == 0 ? NULL :
                                 ( ZH_WCHAR * ) zh_itemGetC( pInfo->pNewVal );
         }
         break;
#endif
      case ZH_GTI_ICONFILE:
      case ZH_GTI_ICONRES:
      {
         HICON hIcon = NULL, hIconToFree = NULL;

         if( zh_itemType( pInfo->pNewVal ) & ZH_IT_STRING )
         {
            void * hIconName;

            if( iType == ZH_GTI_ICONFILE )
#if defined( ZH_OS_WIN_CE )
               hIcon = hIconToFree = ( HICON )
                       LoadImage( ( HINSTANCE ) NULL,
                                  ZH_ITEMGETSTR( pInfo->pNewVal, &hIconName, NULL ),
                                  IMAGE_ICON, 0, 0, LR_LOADFROMFILE );
#else
               hIcon = hIconToFree = ( HICON )
                       LoadImage( ( HINSTANCE ) NULL,
                                  ZH_ITEMGETSTR( pInfo->pNewVal, &hIconName, NULL ),
                                  IMAGE_ICON, 0, 0, LR_LOADFROMFILE | LR_DEFAULTSIZE );
#endif
            else
               hIcon = LoadIcon( pWVT->hInstance,
                                 ZH_ITEMGETSTR( pInfo->pNewVal, &hIconName, NULL ) );
            zh_strfree( hIconName );
         }
         else if( zh_itemType( pInfo->pNewVal ) & ZH_IT_NUMERIC )
         {
            hIcon = LoadIcon( pWVT->hInstance,
                              MAKEINTRESOURCE( zh_itemGetNI( pInfo->pNewVal ) ) );
         }
         if( hIcon != pWVT->hIcon )
         {
            if( pWVT->hWnd )
            {
               SendNotifyMessage( pWVT->hWnd, WM_SETICON, ICON_SMALL, ( LPARAM ) hIcon ); /* Set Title Bar Icon */
               SendNotifyMessage( pWVT->hWnd, WM_SETICON, ICON_BIG  , ( LPARAM ) hIcon ); /* Set Task List Icon */
            }
            if( pWVT->hIconToFree )
               DestroyIcon( pWVT->hIconToFree );
            pWVT->hIconToFree = hIconToFree;
            pWVT->hIcon = hIcon;
         }
         pInfo->pResult = zh_itemPutPtr( pInfo->pResult, ( void * ) ( ZH_PTRUINT ) pWVT->hIcon );
         break;
      }
      case ZH_GTI_VIEWPORTWIDTH:
      case ZH_GTI_VIEWMAXWIDTH:
         pInfo->pResult = zh_itemPutNI( pInfo->pResult, pWVT->COLS );
         break;

      case ZH_GTI_VIEWPORTHEIGHT:
      case ZH_GTI_VIEWMAXHEIGHT:
         pInfo->pResult = zh_itemPutNI( pInfo->pResult, pWVT->ROWS );
         break;

      case ZH_GTI_KBDSHIFTS:
         pInfo->pResult = zh_itemPutNI( pInfo->pResult, zh_gt_winapi_getKbdState() );
         if( zh_itemType( pInfo->pNewVal ) & ZH_IT_NUMERIC )
            zh_gt_winapi_setKbdState( zh_itemGetNI( pInfo->pNewVal ) );
         break;

      case ZH_GTI_CLIPBOARDDATA:
         if( zh_itemType( pInfo->pNewVal ) & ZH_IT_STRING )
#if defined( UNICODE )
            zh_gt_winapi_setClipboard( CF_UNICODETEXT, pInfo->pNewVal );
#else
            zh_gt_winapi_setClipboard( pWVT->CodePage == OEM_CHARSET ?
                                       CF_OEMTEXT : CF_TEXT, pInfo->pNewVal );
#endif
         else
         {
            if( pInfo->pResult == NULL )
               pInfo->pResult = zh_itemNew( NULL );
#if defined( UNICODE )
            zh_gt_winapi_getClipboard( CF_UNICODETEXT, pInfo->pResult );
#else
            zh_gt_winapi_getClipboard( pWVT->CodePage == OEM_CHARSET ?
                                       CF_OEMTEXT : CF_TEXT, pInfo->pResult );
#endif
         }
         break;

      case ZH_GTI_CURSORBLINKRATE:
         pInfo->pResult = zh_itemPutNI( pInfo->pResult, GetCaretBlinkTime() );
         if( zh_itemType( pInfo->pNewVal ) & ZH_IT_NUMERIC )
         {
            iVal = zh_itemGetNI( pInfo->pNewVal );
            SetCaretBlinkTime( ZH_MAX( iVal, 0 ) );
         }
         break;

      case ZH_GTI_SCREENSIZE:
         if( ! pInfo->pResult )
            pInfo->pResult = zh_itemNew( NULL );

         zh_arrayNew( pInfo->pResult, 2 );
         zh_arraySetNI( pInfo->pResult, 2, pWVT->PTEXTSIZE.y * pWVT->ROWS );
         zh_arraySetNI( pInfo->pResult, 1, pWVT->PTEXTSIZE.x * pWVT->COLS );

         if( ( zh_itemType( pInfo->pNewVal ) & ZH_IT_ARRAY ) && zh_arrayLen( pInfo->pNewVal ) == 2 )
         {
            int iX, iY;

            iY = zh_arrayGetNI( pInfo->pNewVal, 2 );
            iX = zh_arrayGetNI( pInfo->pNewVal, 1 );

            if( iY > 0 && iX > 0 && ! pWVT->bMaximized && ! pWVT->bFullScreen && pWVT->hWnd )  /* Don't allow if Maximized or FullScreen */
            {
               /* Now conforms to pWVT->ResizeMode setting, resize by FONT or ROWS as applicable [HVB] */
               RECT ci;
               GetClientRect( pWVT->hWnd, &ci );
               if( ci.right != iX || ci.bottom != iY )
               {
                  RECT wi;
                  GetWindowRect( pWVT->hWnd, &wi );
                  iX += wi.right - wi.left - ci.right;
                  iY += wi.bottom - wi.top - ci.bottom;
                  SetWindowPos( pWVT->hWnd, NULL, wi.left, wi.top, iX, iY, SWP_NOZORDER );
               }
            }
         }
         break;

      case ZH_GTI_RESIZABLE:
         pInfo->pResult = zh_itemPutL( pInfo->pResult, pWVT->bResizable );
         if( pInfo->pNewVal )
         {
            ZH_BOOL bNewValue = zh_itemGetL( pInfo->pNewVal );
            if( bNewValue != pWVT->bResizable )
            {
               pWVT->bResizable = bNewValue;
               if( pWVT->hWnd )
               {
                  SetWindowLongPtr( pWVT->hWnd, GWL_STYLE, pWVT->bResizable ? _WVT_WS_DEF : _WVT_WS_NORESIZE );
                  SetWindowPos( pWVT->hWnd, NULL, 0, 0, 0, 0,
                                SWP_NOACTIVATE | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER | SWP_DEFERERASE );
                  ShowWindow( pWVT->hWnd, SW_HIDE );
                  ShowWindow( pWVT->hWnd, SW_NORMAL );
               }
            }
         }
         break;

      case ZH_GTI_SYSMENUADD:
         pInfo->pResult = zh_itemPutL( pInfo->pResult, ZH_FALSE );
         if( zh_itemType( pInfo->pNewVal ) & ZH_IT_NUMERIC )
         {
            iVal = zh_itemGetNI( pInfo->pNewVal );
            if( iVal != 0 )
            {
               ZH_BOOL fAdd = ( zh_itemType( pInfo->pNewVal2 ) & ZH_IT_STRING ) != 0;
               PZH_GTWVT_MNU * pMenu = &pWVT->pMenu;
               int iEvent = SYS_EV_MARK;

               while( * pMenu )
               {
                  if( ( * pMenu )->iKey == iVal )
                     break;
                  iEvent = ZH_MAX( iEvent, ( * pMenu )->iEvent );
                  pMenu = &( * pMenu )->pNext;
               }
               if( * pMenu )
               {
                  zh_strfree( ( * pMenu )->hName );
                  iEvent = ( * pMenu )->iEvent;
                  if( ! fAdd )
                  {
                     PZH_GTWVT_MNU pFree = * pMenu;
                     * pMenu = ( * pMenu )->pNext;
                     zh_xfree( pFree );
                  }
               }
               else
               {
                  if( fAdd )
                  {
                     * pMenu = ( PZH_GTWVT_MNU ) zh_xgrab( sizeof( ZH_GTWVT_MNU ) );
                     ( * pMenu )->iKey   = iVal;
                     ( * pMenu )->iEvent = iEvent + 1;
                     ( * pMenu )->pNext  = NULL;
                  }
                  iEvent = 0;
               }
               if( fAdd )
                  ( * pMenu )->lpName = ZH_ITEMGETSTR( pInfo->pNewVal2, &( * pMenu )->hName, NULL );
               if( pWVT->hWnd )
               {
                  HMENU hSysMenu = GetSystemMenu( pWVT->hWnd, FALSE );
                  if( hSysMenu )
                  {
                     if( iEvent != 0 )
                        DeleteMenu( hSysMenu, iEvent, MF_BYCOMMAND );
                     if( fAdd )
                        AppendMenu( hSysMenu, MF_STRING, ( * pMenu )->iEvent, ( * pMenu )->lpName );
                  }
               }
               pInfo->pResult = zh_itemPutL( pInfo->pResult, fAdd || iEvent != 0 );
            }
         }
         break;

      case ZH_GTI_SELECTCOPY:
         if( zh_itemType( pInfo->pNewVal ) & ZH_IT_STRING )
         {
            pInfo->pResult = ZH_ITEMPUTSTR( pInfo->pResult, pWVT->lpSelectCopy );

            if( zh_itemGetCLen( pInfo->pNewVal ) )
            {
               HMENU hSysMenu = pWVT->hWnd ? GetSystemMenu( pWVT->hWnd, FALSE ) : NULL;
               if( hSysMenu || ! pWVT->hWnd )
               {
                  zh_strfree( pWVT->hSelectCopy );
                  pWVT->lpSelectCopy = ZH_ITEMGETSTR( pInfo->pNewVal, &pWVT->hSelectCopy, NULL );
                  pWVT->bSelectCopy = ZH_TRUE;
                  if( hSysMenu )
                  {
#if defined( ZH_OS_WIN_CE )  /* WinCE does not support ModifyMenu */
                     DeleteMenu( hSysMenu, SYS_EV_MARK, MF_BYCOMMAND );
                     AppendMenu( hSysMenu, MF_STRING, SYS_EV_MARK, pWVT->lpSelectCopy );
#else
                     ModifyMenu( hSysMenu, SYS_EV_MARK, MF_BYCOMMAND | MF_STRING | MF_ENABLED, SYS_EV_MARK, pWVT->lpSelectCopy );
#endif
                  }
               }
            }
         }
         else
         {
            pInfo->pResult = zh_itemPutL( pInfo->pResult, pWVT->bSelectCopy );
            if( pInfo->pNewVal )
            {
               ZH_BOOL bNewValue = zh_itemGetL( pInfo->pNewVal );
               if( bNewValue != pWVT->bSelectCopy )
               {
                  if( pWVT->hWnd )
                  {
                     HMENU hSysMenu = GetSystemMenu( pWVT->hWnd, FALSE );
                     if( hSysMenu )
                     {
                        EnableMenuItem( hSysMenu, SYS_EV_MARK, MF_BYCOMMAND | ( bNewValue ? MF_ENABLED : MF_GRAYED ) );
                        pWVT->bSelectCopy = bNewValue;
                     }
                  }
                  else
                     pWVT->bSelectCopy = bNewValue;
               }
            }
         }
         break;

      case ZH_GTI_CLOSABLE:
         pInfo->pResult = zh_itemPutL( pInfo->pResult, pWVT->CloseMode == 0 );
         if( ( zh_itemType( pInfo->pNewVal ) & ZH_IT_LOGICAL ) &&
             ( zh_itemGetL( pInfo->pNewVal ) ? ( pWVT->CloseMode != 0 ) :
                                               ( pWVT->CloseMode == 0 ) ) )
         {
            iVal = pWVT->CloseMode;
            pWVT->CloseMode = iVal == 0 ? 1 : 0;
            if( pWVT->hWnd )
               zh_gt_wvt_SetCloseButton( pWVT );
         }
         break;

      case ZH_GTI_CLOSEMODE:
         pInfo->pResult = zh_itemPutNI( pInfo->pResult, pWVT->CloseMode );
         if( zh_itemType( pInfo->pNewVal ) & ZH_IT_NUMERIC )
         {
            iVal = zh_itemGetNI( pInfo->pNewVal );
            if( iVal >= 0 && iVal <= 2 && pWVT->CloseMode != iVal )
            {
               pWVT->CloseMode = iVal;
               if( pWVT->hWnd )
                  zh_gt_wvt_SetCloseButton( pWVT );
            }
         }
         break;

      case ZH_GTI_PALETTE:
         if( zh_itemType( pInfo->pNewVal ) & ZH_IT_NUMERIC )
         {
            int iIndex = zh_itemGetNI( pInfo->pNewVal );

            if( iIndex >= 0 && iIndex < 16 )
            {
               pInfo->pResult = zh_itemPutNL( pInfo->pResult, pWVT->COLORS[ iIndex ] );

               if( zh_itemType( pInfo->pNewVal2 ) & ZH_IT_NUMERIC )
               {
                  pWVT->COLORS[ iIndex ] = zh_itemGetNL( pInfo->pNewVal2 );

                  if( pWVT->hWnd )
                     ZH_GTSELF_EXPOSEAREA( pWVT->pGT, 0, 0, pWVT->ROWS, pWVT->COLS );
               }
            }
         }
         else
         {
            int i;
            if( ! pInfo->pResult )
               pInfo->pResult = zh_itemNew( NULL );
            zh_arrayNew( pInfo->pResult, 16 );
            for( i = 0; i < 16; i++ )
               zh_arraySetNL( pInfo->pResult, i + 1, pWVT->COLORS[ i ] );

            if( zh_itemType( pInfo->pNewVal ) & ZH_IT_ARRAY )
            {
               if( zh_arrayLen( pInfo->pNewVal ) == 16 )
               {
                  for( i = 0; i < 16; i++ )
                     pWVT->COLORS[ i ] = zh_arrayGetNL( pInfo->pNewVal, i + 1 );

                  if( pWVT->hWnd )
                     ZH_GTSELF_EXPOSEAREA( pWVT->pGT, 0, 0, pWVT->ROWS, pWVT->COLS );
               }
            }
         }
         break;

      case ZH_GTI_RESIZEMODE:
         pInfo->pResult = zh_itemPutNI( pInfo->pResult, pWVT->ResizeMode );
         if( zh_itemType( pInfo->pNewVal ) & ZH_IT_NUMERIC )
         {
            iVal = zh_itemGetNI( pInfo->pNewVal );
            switch( iVal )
            {
               case ZH_GTI_RESIZEMODE_FONT:
               case ZH_GTI_RESIZEMODE_ROWS:
                  pWVT->ResizeMode = iVal;
                  break;
            }
         }
         break;

      case ZH_GTI_SETPOS_XY:
      case ZH_GTI_SETPOS_ROWCOL:
      {
         RECT wi = { 0, 0, 0, 0 };
         int x = 0, y = 0;

         if( pWVT->hWnd )
         {
            GetWindowRect( pWVT->hWnd, &wi );
            if( iType == ZH_GTI_SETPOS_ROWCOL )
            {
               y = wi.left / pWVT->PTEXTSIZE.x;
               x = wi.top / pWVT->PTEXTSIZE.y;
            }
            else
            {
               x = wi.left;
               y = wi.top;
            }
         }

         if( ! pInfo->pResult )
            pInfo->pResult = zh_itemNew( NULL );
         zh_arrayNew( pInfo->pResult, 2 );

         zh_arraySetNI( pInfo->pResult, 1, x );
         zh_arraySetNI( pInfo->pResult, 2, y );

         if( ( zh_itemType( pInfo->pNewVal ) & ZH_IT_NUMERIC ) &&
             ( zh_itemType( pInfo->pNewVal2 ) & ZH_IT_NUMERIC ) )
         {
            x = zh_itemGetNI( pInfo->pNewVal );
            y = zh_itemGetNI( pInfo->pNewVal2 );
         }
         else if( ( zh_itemType( pInfo->pNewVal ) & ZH_IT_ARRAY ) &&
                  zh_arrayLen( pInfo->pNewVal ) == 2 )
         {
            x = zh_arrayGetNI( pInfo->pNewVal, 1 );
            y = zh_arrayGetNI( pInfo->pNewVal, 2 );
         }
         else
            break;

         if( iType == ZH_GTI_SETPOS_ROWCOL )
         {
            int c = y;
            y = x * pWVT->PTEXTSIZE.y;
            x = c * pWVT->PTEXTSIZE.x;
         }
         if( pWVT->hWnd )
         {
            SetWindowPos( pWVT->hWnd, NULL,
                          x, y, wi.right - wi.left, wi.bottom - wi.top,
                          SWP_NOSIZE | SWP_NOZORDER );
         }
         else
         {
            pWVT->iNewPosX = x;
            pWVT->iNewPosY = y;
         }
         break;
      }

      case ZH_GTI_WINHANDLE:
         pInfo->pResult = zh_itemPutPtr( pInfo->pResult, pWVT->hWnd );
         break;

      case ZH_GTI_QUICKEDIT:
         pInfo->pResult = zh_itemPutL( pInfo->pResult, pWVT->bQuickEdit );
         if( pInfo->pNewVal )
         {
            ZH_BOOL bNewValue = zh_itemGetL( pInfo->pNewVal );
            if( bNewValue != pWVT->bQuickEdit )
               pWVT->bQuickEdit = bNewValue;
         }
         break;

      default:
         return ZH_GTSUPER_INFO( pGT, iType, pInfo );
   }

   return ZH_TRUE;
}

/* ********************************************************************** */

/* ********** Graphics API ********** */
/*
 * NOTE:
 *      GfxPrimitive() parameters may have different meanings
 *      ie: - Desired color is 'iBottom' for PUTPIXEL and 'iRight' for CIRCLE
 *          - Red is iTop, Green iLeft and Blue is iBottom for MAKECOLOR
 *
 */

#define SetGFXContext( c ) \
   do { \
      COLORREF color = RGB( ZH_ULBYTE( c ), ZH_HIBYTE( c ), ZH_LOBYTE( c ) ); \
      hdc       = GetDC( pWVT->hWnd ); \
      hPen      = CreatePen( PS_SOLID, 1, color ); \
      hOldPen   = ( HPEN ) SelectObject( hdc, hPen ); \
      hBrush    = CreateSolidBrush( color ); \
      hOldBrush = ( HBRUSH ) SelectObject( hdc, hBrush ); \
   } while( 0 )

#define ClearGFXContext() \
   do { \
      SelectObject( hdc, hOldPen ); \
      SelectObject( hdc, hOldBrush ); \
      DeleteObject( hBrush ); \
      DeleteObject( hPen ); \
      ReleaseDC( pWVT->hWnd, hdc ); \
   } while( 0 )

static int zh_gt_wvt_gfx_Primitive( PZH_GT pGT, int iType, int iTop, int iLeft, int iBottom, int iRight, int iColor )
{
   PZH_GTWVT pWVT;
   RECT      r;
   int       iRet = 0;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_wvt_gfx_Primitive(%p,%d,%d,%d,%d,%d,%d)", ( void * ) pGT, iType, iTop, iLeft, iBottom, iRight, iColor ) );

   pWVT = ZH_GTWVT_GET( pGT );

   if( pWVT->hWnd )
   {
      HDC    hdc;
      HPEN   hPen, hOldPen;
      HBRUSH hBrush, hOldBrush;

      if( pWVT->bComposited )
         zh_gt_wvt_Composited( pWVT, ZH_FALSE );

      switch( iType )
      {
         case ZH_GFX_ACQUIRESCREEN:
         case ZH_GFX_RELEASESCREEN:
            iRet = 1;
            break;

         case ZH_GFX_MAKECOLOR:
            iRet = ( iTop << 16 ) | ( iLeft << 8 ) | iBottom;
            break;

         case ZH_GFX_PUTPIXEL:
            SetGFXContext( iBottom );

            iRet = ( MoveToEx( hdc, iLeft, iTop, NULL ) &&
                     LineTo( hdc, iLeft, iTop ) ) ? 1 : 0;

            ClearGFXContext();
            break;

         case ZH_GFX_LINE:
            SetGFXContext( iColor );

            iRet = ( MoveToEx( hdc, iLeft, iTop, NULL ) &&
                     LineTo( hdc, iRight, iBottom ) ) ? 1 : 0;

            ClearGFXContext();
            break;

         case ZH_GFX_RECT:
            r.left   = ZH_MIN( iLeft, iRight );
            r.top    = ZH_MIN( iTop, iBottom );
            r.right  = ZH_MAX( iLeft, iRight );
            r.bottom = ZH_MAX( iTop, iBottom );

            SetGFXContext( iColor );

            iRet = FrameRect( hdc, &r, hBrush ) ? 1 : 0;

            ClearGFXContext();
            break;

         case ZH_GFX_FILLEDRECT:
            SetGFXContext( iColor );

            r.left   = ZH_MIN( iLeft, iRight );
            r.top    = ZH_MIN( iTop, iBottom );
            r.right  = ZH_MAX( iLeft, iRight );
            r.bottom = ZH_MAX( iTop, iBottom );

            iRet = Rectangle( hdc, r.left, r.top, r.right, r.bottom ) ? 1 : 0;

            ClearGFXContext();
            break;

         case ZH_GFX_CIRCLE:
            SetGFXContext( iRight );

            iRet = Arc( hdc, iLeft - iBottom, iTop - iBottom, iLeft + iBottom, iTop + iBottom, 0, 0, 0, 0 ) ? 1 : 0;

            ClearGFXContext();
            break;

         case ZH_GFX_FILLEDCIRCLE:
            SetGFXContext( iRight );

            iRet = Ellipse( hdc, iLeft - iBottom, iTop - iBottom, iLeft + iBottom, iTop + iBottom ) ? 1 : 0;

            ClearGFXContext();
            break;

         case ZH_GFX_ELLIPSE:
            SetGFXContext( iColor );

            iRet = Arc( hdc, iLeft - iRight, iTop - iBottom, iLeft + iRight, iTop + iBottom, 0, 0, 0, 0 ) ? 1 : 0;

            ClearGFXContext();
            break;

         case ZH_GFX_FILLEDELLIPSE:
            SetGFXContext( iColor );

            iRet = Ellipse( hdc, iLeft - iRight, iTop - iBottom, iLeft + iRight, iTop + iBottom ) ? 1 : 0;

            ClearGFXContext();
            break;

         case ZH_GFX_FLOODFILL:
            SetGFXContext( iBottom );

            iRet = FloodFill( hdc, iLeft, iTop, iColor ) ? 1 : 0;

            ClearGFXContext();
            break;
      }
   }

   return iRet;
}

#if 0
static void zh_gt_wvt_gfx_Text( PZH_GT pGT, int iTop, int iLeft, const char *cBuf, int iColor, int iSize, int iWidth )
{
   ZH_SYMBOL_UNUSED( pGT );
   ZH_SYMBOL_UNUSED( iTop );
   ZH_SYMBOL_UNUSED( iLeft );
   ZH_SYMBOL_UNUSED( cBuf );
   ZH_SYMBOL_UNUSED( iColor );
   ZH_SYMBOL_UNUSED( iSize );
   ZH_SYMBOL_UNUSED( iWidth );
}
#endif

/* ********************************************************************** */

static void zh_gt_wvt_Redraw( PZH_GT pGT, int iRow, int iCol, int iSize )
{
   PZH_GTWVT pWVT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_wvt_Redraw(%p,%d,%d,%d)", ( void * ) pGT, iRow, iCol, iSize ) );

   pWVT = ZH_GTWVT_GET( pGT );
   if( pWVT )
   {
      if( pWVT->hWnd )
      {
         RECT rect;

         rect.top = rect.bottom = iRow;
         rect.left = iCol;
         rect.right = iCol + iSize - 1;

         rect = zh_gt_wvt_GetXYFromColRowRect( pWVT, rect );

         InvalidateRect( pWVT->hWnd, &rect, FALSE );
      }
      else
         pWVT->fInit = ZH_TRUE;
   }
}

/* ********************************************************************** */

static void zh_gt_wvt_Refresh( PZH_GT pGT )
{
   PZH_GTWVT pWVT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_wvt_Refresh(%p)", ( void * ) pGT ) );

   ZH_GTSUPER_REFRESH( pGT );

   pWVT = ZH_GTWVT_GET( pGT );
   if( pWVT )
   {
      if( ! pWVT->hWnd && pWVT->fInit )
         zh_gt_wvt_CreateConsoleWindow( pWVT );

      if( pWVT->hWnd )
      {
         SendNotifyMessage( pWVT->hWnd, WM_MY_UPDATE_CARET, 0, 0 );
         zh_gt_wvt_ProcessMessages();
      }
   }
}

/* ********************************************************************** */

static ZH_BOOL zh_gt_FuncInit( PZH_GT_FUNCS pFuncTable )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_FuncInit(%p)", ( void * ) pFuncTable ) );

   pFuncTable->Init                 = zh_gt_wvt_Init;
   pFuncTable->Exit                 = zh_gt_wvt_Exit;
   pFuncTable->SetMode              = zh_gt_wvt_SetMode;
   pFuncTable->Redraw               = zh_gt_wvt_Redraw;
   pFuncTable->Refresh              = zh_gt_wvt_Refresh;
   pFuncTable->Version              = zh_gt_wvt_Version;
   pFuncTable->Tone                 = zh_gt_wvt_Tone;
   pFuncTable->Info                 = zh_gt_wvt_Info;
   pFuncTable->ReadKey              = zh_gt_wvt_ReadKey;

   pFuncTable->MouseIsPresent       = zh_gt_wvt_mouse_IsPresent;
   pFuncTable->MouseGetPos          = zh_gt_wvt_mouse_GetPos;
   pFuncTable->MouseSetPos          = zh_gt_wvt_mouse_SetPos;
   pFuncTable->MouseButtonState     = zh_gt_wvt_mouse_ButtonState;
   pFuncTable->MouseCountButton     = zh_gt_wvt_mouse_CountButton;

   pFuncTable->GfxPrimitive         = zh_gt_wvt_gfx_Primitive;

   return ZH_TRUE;
}

/* *********************************************************************** */

#include "zh_gt_reg.h"

/* *********************************************************************** */
