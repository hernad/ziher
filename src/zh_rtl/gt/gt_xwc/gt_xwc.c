/*
 * X11 (X Window System) console
 *
 * Copyright 2003 Giancarlo Niccolai <antispam /at/ niccolai.ws>
 * Copyright 2004-2006 Przemyslaw Czerpak <druzus /at/ priv.onet.pl>
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

/* NOTE: User programs should never call this layer directly! */

/* #define XWC_DEBUG */
/* #define ZH_XWC_USE_LOCALE */


#include "gt_xwc.h"
#ifdef ZH_XWC_USE_LOCALE
#  include <locale.h>
#endif

/* #undef X_HAVE_UTF8_STRING */

static int s_GtId;
static ZH_GT_FUNCS SuperTable;
#define ZH_GTSUPER            ( &SuperTable )
#define ZH_GTID_PTR           ( &s_GtId )

#define ZH_GTXWC_GET( p )  ( ( PXWND_DEF ) ZH_GTLOCAL( p ) )

#if defined( ZH_XWC_XLIB_NEEDLOCKS )
   static ZH_CRITICAL_NEW( s_xwcMtx );
#  define ZH_XWC_XLIB_LOCK( dpy )   do { zh_threadEnterCriticalSection( &s_xwcMtx )
#  define ZH_XWC_XLIB_UNLOCK( dpy ) zh_threadLeaveCriticalSection( &s_xwcMtx ); } while( 0 )
#elif ! defined( ZH_XWC_XLOCK_OFF )
#  define ZH_XWC_XLIB_LOCK( dpy )   do { XLockDisplay( dpy )
#  define ZH_XWC_XLIB_UNLOCK( dpy ) XUnlockDisplay( dpy ); } while( 0 )
#else
#  define ZH_XWC_XLIB_LOCK( dpy )   do {
#  define ZH_XWC_XLIB_UNLOCK( dpy ) } while( 0 )
#endif
#define ZH_XWC_XLIB_UNLOCKRAW( dpy ) do { ZH_XWC_XLIB_UNLOCK( dpy )

static const int s_mousePressKeys   [ XWC_MAX_BUTTONS ] = { K_LBUTTONDOWN, K_MBUTTONDOWN, K_RBUTTONDOWN, K_MWFORWARD, K_MWBACKWARD };
static const int s_mouseReleaseKeys [ XWC_MAX_BUTTONS ] = { K_LBUTTONUP,   K_MBUTTONUP,   K_RBUTTONUP   };
static const int s_mouseDblPressKeys[ XWC_MAX_BUTTONS ] = { K_LDBLCLK,     K_MDBLCLK,     K_RDBLCLK    , K_MWFORWARD, K_MWBACKWARD };
static const int s_mouseButtonBits  [ XWC_MAX_BUTTONS ] = { 1 << ZH_MBUTTON_LEFT, 1 << ZH_MBUTTON_MIDDLE, 1 << ZH_MBUTTON_RIGHT };

/* these are standard PC console colors in RGB */
static const int s_rgb_values[] = {
   0x000000,   /* black         "rgb:00/00/00" */
   0xAA0000,   /* blue          "rgb:00/00/AA" */
   0x00AA00,   /* green         "rgb:00/AA/00" */
   0xAAAA00,   /* cyan          "rgb:00/AA/AA" */
   0x0000AA,   /* red           "rgb:AA/00/00" */
   0xAA00AA,   /* magenta       "rgb:AA/00/AA" */
   0x0055AA,   /* brown         "rgb:AA/55/00" */
   0xAAAAAA,   /* light gray    "rgb:AA/AA/AA" */
   0x555555,   /* gray          "rgb:55/55/55" */
   0xFF5555,   /* light blue    "rgb:55/55/FF" */
   0x55FF55,   /* light green   "rgb:55/FF/55" */
   0xFFFF55,   /* light cyan    "rgb:55/FF/FF" */
   0x5555FF,   /* light red     "rgb:FF/55/55" */
   0xFF55FF,   /* light magenta "rgb:FF/55/FF" */
   0x55FFFF,   /* yellow        "rgb:FF/FF/55" */
   0xFFFFFF    /* white         "rgb:FF/FF/FF" */
};

static Atom s_atomDelWin;
static Atom s_atomTimestamp;
static Atom s_atomAtom;
static Atom s_atomInteger;
static Atom s_atomString;
static Atom s_atomUTF8String;
static Atom s_atomPrimary;
static Atom s_atomSecondary;
static Atom s_atomClipboard;
static Atom s_atomTargets;
static Atom s_atomCutBuffer0;
static Atom s_atomText;
static Atom s_atomCompoundText;
static Atom s_atomFullScreen;
static Atom s_atomMaximizedX;
static Atom s_atomMaximizedY;
static Atom s_atomActivate;
static Atom s_atomState;
static Atom s_atomMotifHints;
static Atom s_atomFrameExtends;
static Atom s_atomCardinal;


typedef struct
{
   int top;
   int left;
   int right;
   int bottom;
} XWC_RECT;

typedef struct
{
   ZH_BOOL bCtrl;
   ZH_BOOL bAlt;
   ZH_BOOL bAltGr;
   ZH_BOOL bShift;
} MODIFIERS;

typedef struct
{
   ZH_GT_PIXELTYPE pixel;
   int value;
   ZH_BOOL set;
} WND_COLORS;

typedef struct
{
   PZH_GT pGT;

   Display *dpy;
   Window window;
   GC gc;
   Colormap colorsmap;
   WND_COLORS colors[ 16 ];
   Pixmap pm;
   Drawable drw;

   void ( * evt_callback )( void );

   /* is main window initialized */
   ZH_BOOL fInit;
   /* is anything written to screen */
   ZH_BOOL fData;

   /* block recursive refresh calls */
   ZH_BOOL fRefresh;

   /* window size in character cells */
   ZH_USHORT cols;
   ZH_USHORT rows;

   /* window size in pixels */
   ZH_USHORT width;
   ZH_USHORT height;

   int iNewPosX;
   int iNewPosY;

   int iCordLeft;
   int iCordTop;
   ZH_BOOL fCordsInited;

   /* Set to true when Windows is resized */
   ZH_BOOL fWinResize;
   ZH_USHORT newWidth;
   ZH_USHORT newHeight;
   ZH_USHORT oldWidth;
   ZH_USHORT oldHeight;

   int     iCloseMode;
   ZH_BOOL fResizable;
   ZH_BOOL fFullScreen;
   ZH_BOOL fMaximized;
   ZH_BOOL fMinimized;
   ZH_BOOL fAltEnter;

   /* mark & copy */
   ZH_BOOL fSelectCopy;
   ZH_BOOL fMarkMode;
   int iMarkCol;
   int iMarkRow;
   int markLeft;
   int markTop;
   int markRight;
   int markBottom;

   /* window title */
   char * szTitle;
   ZH_BOOL fDspTitle;

   /* used font informations */
   XFontStruct * xfs;
   char * szFontName;
   char * szFontEncoding;
   char * szFontSel;
   int fontWeight;
   int fontHeight;
   int fontWidth;
   /* if font has bad metric then try to fix it and display only single
      char at cell position at once */
   ZH_BOOL fFixMetric;
   /* if font has bad size and doesn't clear background cell we can
      try to fix it and clear background drawing rectangle before
      displaying font */
   ZH_BOOL fClearBkg;
   /* if ZH_TRUE then BOX characters will be drawn by GTXWC instead of
      using build in font ones */
   ZH_BOOL fDrawBox;

   /* locale set to UTF-8 or X_HAVE_UTF8_STRING */
   ZH_BOOL fUTF8key;
   PZH_CODEPAGE utf8CDP;

#ifdef X_HAVE_UTF8_STRING
   XIM im;
   XIC ic;
#endif

   /* current cursor and color settings */
   int col;
   int row;
   int cursorType;

   /* last cursor position and shape */
   int lastCursorCol;
   int lastCursorRow;
   int lastCursorType;

   ZH_BOOL cursorState;
   ZH_ULONG cursorBlinkRate;
   ZH_ULONG cursorStateTime;

   /* Mouse informations */
   int mouseCol;
   int mouseRow;
   int mouseColPxl;
   int mouseRowPxl;
   int mouseGotoCol;
   int mouseGotoRow;
   int mouseNumButtons;
   int mouseButtonsState;
   unsigned char mouseButtonsMap[ XWC_MAX_BUTTONS ];
   Time mouseButtonsTime[ XWC_MAX_BUTTONS ];

   /* current screen contents (attr<<24)|(color<<16)|char */
   ZH_U32 * pCurrScr;

   /* character translation table, it changes some characters in screen buffer into graphs primitives */
   XWC_CharTrans boxTrans[ ZH_BOXCH_TRANS_MAX ];
   ZH_UCHAR boxIndex[ ZH_BOXCH_TRANS_COUNT ];
   int boxCount;

   ZH_BOOL fInvalidChr;
   XWC_RECT rInvalidChr;

   ZH_BOOL fInvalidPts;
   XWC_RECT rInvalidPts;

   /* Keyboard buffer */
   int keyBuffPointer;
   int keyBuffNO;
   int KeyBuff[ XWC_CHAR_QUEUE_SIZE ];
   MODIFIERS keyModifiers;

   /* Clipboard buffer */
   unsigned char * ClipboardData;
   ZH_SIZE ClipboardSize;
   Atom ClipboardRequest;
   Time ClipboardTime;
   ZH_BOOL ClipboardOwner;
   ZH_BOOL ClipboardRcvd;

   /* Clipping */
   XRectangle ClipRect;

   /* Keep last event time */
   Time lastEventTime;

} XWND_DEF, * PXWND_DEF;

/******************************************************************/

static void zh_gt_xwc_ProcessMessages( PXWND_DEF wnd, ZH_BOOL fSync );
static void zh_gt_xwc_InvalidatePts( PXWND_DEF wnd, int left, int top, int right, int bottom );
static void zh_gt_xwc_InvalidateChar( PXWND_DEF wnd, int left, int top, int right, int bottom );
static void zh_gt_xwc_SetSelection( PXWND_DEF wnd, const char * szData, ZH_SIZE nSize, ZH_BOOL fCopy );

/************************ globals ********************************/

static PXWND_DEF s_wnd = NULL;
static ZH_BOOL s_fNoXServer = ZH_FALSE;

#if 1
static int s_updateMode = XWC_SYNC_UPDATE;
#else
static int s_updateMode = XWC_ASYNC_UPDATE;
#endif
static int s_iUpdateCounter;

static ZH_BOOL s_fIgnoreErrors = ZH_FALSE;

/* *********************************************************************** */

static int s_errorHandler( Display * dpy, XErrorEvent * e )
{
   char errorText[ 1024 ];

   zh_strncpy( errorText, "Xlib error: ", sizeof( errorText ) - 1 );
   XGetErrorText( dpy, e->error_code, errorText + strlen( errorText ),
                  sizeof( errorText ) - strlen( errorText ) );

   if( ! s_fIgnoreErrors )
   {
      s_fNoXServer = ZH_TRUE;
      zh_errInternal( 10001, errorText, NULL, NULL );
   }

   fprintf( stderr, "%s\n", errorText );

   return 1;
}

/* *********************************************************************** */

static void zh_gt_xwc_SigHandler( int iSig )
{
   PXWND_DEF wnd = s_wnd;

   ZH_SYMBOL_UNUSED( iSig );

   if( s_updateMode == XWC_ASYNC_UPDATE && wnd && wnd->fInit )
   {
      if( s_iUpdateCounter )
         --s_iUpdateCounter;
      zh_gt_xwc_ProcessMessages( wnd, ZH_FALSE );
   }
}

/* *********************************************************************** */

static void zh_gt_xwc_Disable( void )
{
   if( s_updateMode == XWC_ASYNC_UPDATE )
   {
      signal( SIGALRM, SIG_IGN );
   }
}

/* *********************************************************************** */

static void zh_gt_xwc_Enable( void )
{
   if( s_updateMode == XWC_ASYNC_UPDATE )
   {
      struct itimerval itv;

      signal( SIGALRM, zh_gt_xwc_SigHandler );
      itv.it_interval.tv_sec = 0;
      itv.it_interval.tv_usec = 25000;
      itv.it_value = itv.it_interval;
      setitimer( ITIMER_REAL, &itv, NULL );
   }
}


/* *********************************************************************** */

/*
 *  functions for building character conversion and box chars shapes
 */

/* *********************************************************************** */

static int zh_gt_xwc_DefineBoxButtonL( XSegment * segs, int cellx, int celly )
{
   segs[ 0 ].x1 = cellx - 1;
   segs[ 0 ].y1 = 0;
   segs[ 0 ].x2 = 0;
   segs[ 0 ].y2 = segs[ 0 ].y1;

   segs[ 1 ].x1 = 0;
   segs[ 1 ].y1 = 0;
   segs[ 1 ].x2 = segs[ 1 ].x1;
   segs[ 1 ].y2 = celly - 1;

   segs[ 2 ].x1 = 0;
   segs[ 2 ].y1 = celly - 1;
   segs[ 2 ].x2 = cellx - 1;
   segs[ 2 ].y2 = segs[ 2 ].y1;

   segs[ 3 ].x1 = segs[ 2 ].x1 + 2;
   segs[ 3 ].y1 = segs[ 2 ].y1 - 1;
   segs[ 3 ].x2 = cellx - 1;
   segs[ 3 ].y2 = segs[ 3 ].y1;

   return 4;
}

static int zh_gt_xwc_DefineBoxButtonR( XSegment * segs, int cellx, int celly )
{
   segs[ 0 ].x1 = 0;
   segs[ 0 ].y1 = 0;
   segs[ 0 ].x2 = cellx - 1;
   segs[ 0 ].y2 = segs[ 0 ].y1;

   segs[ 1 ].x1 = segs[ 0 ].x2;
   segs[ 1 ].y1 = segs[ 0 ].y2;
   segs[ 1 ].x2 = segs[ 1 ].x1;
   segs[ 1 ].y2 = celly - 1;

   segs[ 2 ].x1 = segs[ 1 ].x2;
   segs[ 2 ].y1 = segs[ 1 ].y2;
   segs[ 2 ].x2 = 0;
   segs[ 2 ].y2 = segs[ 2 ].y1;

   segs[ 3 ].x1 = segs[ 1 ].x1 - 1;
   segs[ 3 ].y1 = segs[ 1 ].y1 + 3;
   segs[ 3 ].x2 = segs[ 3 ].x1;
   segs[ 3 ].y2 = segs[ 1 ].y2 - 1;

   segs[ 4 ].x1 = segs[ 3 ].x2;
   segs[ 4 ].y1 = segs[ 3 ].y2;
   segs[ 4 ].x2 = 0;
   segs[ 4 ].y2 = segs[ 3 ].y2;

   return 5;
}

static ZH_BOOL zh_gt_xwc_DefineBoxChar( PXWND_DEF wnd, ZH_USHORT usCh, XWC_CharTrans * bxCh )
{
   typedef union
   {
      XSegment   segs[ XWC_MAX_CHAR_SEGS ];
      XRectangle rect[ XWC_MAX_CHAR_RECTS ];
      XPoint     pts[ XWC_MAX_CHAR_POINTS ];
   } ZH_XWC_CHDEF;
   ZH_XWC_CHDEF   chdef;
   XSegment     * segs = chdef.segs;
   XRectangle   * rect = chdef.rect;
   XPoint       * pts  = chdef.pts;
   XWC_CharType   type = CH_UNDEF;
   int            size = 0;
   ZH_BOOL        inverse = ZH_FALSE;

   int cellx = wnd->fontWidth;
   int celly = wnd->fontHeight;
   int i, y, x, yy, xx;

   if( usCh >= ZH_BOXCH_RC_MIN && usCh <= ZH_BOXCH_RC_MAX )
      switch( usCh )
      {
         case ZH_BOXCH_RC_ARROW_DL:
            size = zh_gt_xwc_DefineBoxButtonL( segs, cellx, celly );
            yy = celly / 2 - 1;
            for( y = celly - 4, x = cellx - 1; x >= 3 && y >= yy && size < XWC_MAX_CHAR_SEGS; --x, --y )
            {
               segs[ size ].x1 = x;
               segs[ size ].y1 = y;
               segs[ size ].x2 = cellx - 1;
               segs[ size ].y2 = y;
               size++;
            }
            xx = ZH_MAX( cellx * 2 / 5, 3 ) | 1;
            for( x = cellx - xx / 2 - 1; y >= 3 && size < XWC_MAX_CHAR_SEGS; --y )
            {
               segs[ size ].x1 = x;
               segs[ size ].y1 = y;
               segs[ size ].x2 = cellx - 1;
               segs[ size ].y2 = y;
               size++;
            }
            type = CH_SEG;
            break;

         case ZH_BOXCH_RC_ARROW_DR:
            size = zh_gt_xwc_DefineBoxButtonR( segs, cellx, celly );
            yy = ( celly + 1 ) / 2;
            for( y = celly - 5, x = 0; x < cellx - 4 && y >= yy && size < XWC_MAX_CHAR_SEGS; ++x, --y )
            {
               segs[ size ].x1 = 0;
               segs[ size ].y1 = y;
               segs[ size ].x2 = x;
               segs[ size ].y2 = y;
               size++;
            }
            xx = ZH_MAX( cellx * 2 / 5, 3 ) | 1;
            for( x = xx / 2 - 1; y >= 3 && size < XWC_MAX_CHAR_SEGS; --y )
            {
               segs[ size ].x1 = 0;
               segs[ size ].y1 = y;
               segs[ size ].x2 = x;
               segs[ size ].y2 = y;
               size++;
            }
            type = CH_SEG;
            break;

         case ZH_BOXCH_RC_ARROW_UL:
            size = zh_gt_xwc_DefineBoxButtonL( segs, cellx, celly );
            yy = ( celly + 1 ) / 2;
            for( y = 3, x = cellx - 1; x >= 3 && y <= yy && size < XWC_MAX_CHAR_SEGS; --x, ++y )
            {
               segs[ size ].x1 = x;
               segs[ size ].y1 = y;
               segs[ size ].x2 = cellx - 1;
               segs[ size ].y2 = y;
               size++;
            }
            xx = ZH_MAX( cellx * 2 / 5, 3 ) | 1;
            for( x = cellx - xx / 2 - 1; y < celly - 3 && size < XWC_MAX_CHAR_SEGS; ++y )
            {
               segs[ size ].x1 = x;
               segs[ size ].y1 = y;
               segs[ size ].x2 = cellx - 1;
               segs[ size ].y2 = y;
               size++;
            }
            type = CH_SEG;
            break;

         case ZH_BOXCH_RC_ARROW_UR:
            size = zh_gt_xwc_DefineBoxButtonR( segs, cellx, celly );
            yy = ( celly + 1 ) / 2;
            for( y = 4, x = 0; x < cellx - 4 && y <= yy && size < XWC_MAX_CHAR_SEGS; ++x, ++y )
            {
               segs[ size ].x1 = 0;
               segs[ size ].y1 = y;
               segs[ size ].x2 = x;
               segs[ size ].y2 = y;
               size++;
            }
            xx = ZH_MAX( cellx * 2 / 5, 3 ) | 1;
            for( x = xx / 2 - 1; y < celly - 3 && size < XWC_MAX_CHAR_SEGS; ++y )
            {
               segs[ size ].x1 = 0;
               segs[ size ].y1 = y;
               segs[ size ].x2 = x;
               segs[ size ].y2 = y;
               size++;
            }
            type = CH_SEG;
            break;

         case ZH_BOXCH_RC_ARROW_VL:
            size = zh_gt_xwc_DefineBoxButtonL( segs, cellx, celly );
            yy = ( celly - 1 ) / 2;
            for( y = 3, x = cellx - 1; x >= 3 && y < yy && size < XWC_MAX_CHAR_SEGS; --x, ++y )
            {
               segs[ size ].x1 = x;
               segs[ size ].y1 = y;
               segs[ size ].x2 = cellx - 1;
               segs[ size ].y2 = y;
               size++;
            }
            for( y = yy + 2, ++x; x <= cellx - 1 && y < celly - 3 && size < XWC_MAX_CHAR_SEGS; ++x, ++y )
            {
               segs[ size ].x1 = x;
               segs[ size ].y1 = y;
               segs[ size ].x2 = cellx - 1;
               segs[ size ].y2 = y;
               size++;
            }
            type = CH_SEG;
            break;

         case ZH_BOXCH_RC_ARROW_VR:
            size = zh_gt_xwc_DefineBoxButtonR( segs, cellx, celly );
            yy = ( celly - 1 ) / 2;
            for( y = 4, x = 0; x < cellx - 4 && y < yy && size < XWC_MAX_CHAR_SEGS; ++x, ++y )
            {
               segs[ size ].x1 = 0;
               segs[ size ].y1 = y;
               segs[ size ].x2 = x;
               segs[ size ].y2 = y;
               size++;
            }
            for( y = yy + 2, --x; x >= 0 && y < celly - 3 && size < XWC_MAX_CHAR_SEGS; --x, ++y )
            {
               segs[ size ].x1 = 0;
               segs[ size ].y1 = y;
               segs[ size ].x2 = x;
               segs[ size ].y2 = y;
               size++;
            }
            type = CH_SEG;
            break;

         case ZH_BOXCH_RC_BUTTON_L:
            size = zh_gt_xwc_DefineBoxButtonL( segs, cellx, celly );
            type = CH_SEG;
            break;

         case ZH_BOXCH_RC_BUTTON_R:
            size = zh_gt_xwc_DefineBoxButtonR( segs, cellx, celly );
            type = CH_SEG;
            break;

         case ZH_BOXCH_RC_ARROW_LL:
            size = zh_gt_xwc_DefineBoxButtonL( segs, cellx, celly );
            yy = ( celly - 1 ) / 2;
            for( x = 3, y = 0; x < cellx && size < XWC_MAX_CHAR_SEGS; ++x, ++y )
            {
               segs[ size ].x1 = x;
               segs[ size ].y1 = yy - y;
               segs[ size ].x2 = x;
               segs[ size ].y2 = yy + y;
               size++;
            }
            type = CH_SEG;
            break;

         case ZH_BOXCH_RC_ARROW_LR:
            size = zh_gt_xwc_DefineBoxButtonR( segs, cellx, celly );
            yy = ZH_MAX( celly / 5, 3 ) | 1;
            for( y = ( celly - yy ) / 2; yy-- && size < XWC_MAX_CHAR_SEGS; ++y )
            {
               segs[ size ].x1 = 0;
               segs[ size ].y1 = y;
               segs[ size ].x2 = cellx - 4;
               segs[ size ].y2 = y;
               size++;
            }
            type = CH_SEG;
            break;

         case ZH_BOXCH_RC_ARROW_RL:
            size = zh_gt_xwc_DefineBoxButtonL( segs, cellx, celly );
            yy = ZH_MAX( celly / 5, 3 ) | 1;
            for( y = ( celly - yy ) / 2; yy-- && size < XWC_MAX_CHAR_SEGS; ++y )
            {
               segs[ size ].x1 = 3;
               segs[ size ].y1 = y;
               segs[ size ].x2 = cellx - 1;
               segs[ size ].y2 = y;
               size++;
            }
            type = CH_SEG;
            break;

         case ZH_BOXCH_RC_ARROW_RR:
            size = zh_gt_xwc_DefineBoxButtonR( segs, cellx, celly );
            yy = ( celly - 1 ) / 2;
            for( x = cellx - 4, y = 0; x >= 0 && size < XWC_MAX_CHAR_SEGS; --x, ++y )
            {
               segs[ size ].x1 = x;
               segs[ size ].y1 = yy - y;
               segs[ size ].x2 = x;
               segs[ size ].y2 = yy + y;
               size++;
            }
            type = CH_SEG;
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
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = 0;
            segs[ 1 ].y1 = celly / 2;
            segs[ 1 ].x2 = cellx - 1;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            segs[ 2 ].x1 = 2;
            segs[ 2 ].y1 = celly / 2 - 1;
            segs[ 2 ].x2 = cellx - 1;
            segs[ 2 ].y2 = segs[ 2 ].y1;

            size = 3;
            type = CH_SEG;

            for( y = celly / 2 + 1; y < celly; y++ )
            {
               for( x = ( y & 1 ) + 2; x < cellx && size < XWC_MAX_CHAR_SEGS; x += 2 )
               {
                  segs[ size ].x1 = x;
                  segs[ size ].y1 = y;
                  segs[ size ].x2 = x;
                  segs[ size ].y2 = y;
                  size++;
               }
            }
            break;

         case ZH_BOXCH_RC_VSCRL_RD:
            segs[ 0 ].x1 = cellx - 1;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = cellx - 2;
            segs[ 1 ].y1 = 0;
            segs[ 1 ].x2 = segs[ 1 ].x1;
            segs[ 1 ].y2 = celly / 2 - 1;

            segs[ 2 ].x1 = 0;
            segs[ 2 ].y1 = celly / 2;
            segs[ 2 ].x2 = cellx - 1;
            segs[ 2 ].y2 = segs[ 2 ].y1;

            segs[ 3 ].x1 = 0;
            segs[ 3 ].y1 = celly / 2 - 1;
            segs[ 3 ].x2 = cellx - 1;
            segs[ 3 ].y2 = segs[ 3 ].y1;

            size = 4;
            type = CH_SEG;

            for( y = celly / 2 + 1; y < celly; y++ )
            {
               for( x = ( y ^ cellx ) & 1; x < cellx - 2 && size < XWC_MAX_CHAR_SEGS; x += 2 )
               {
                  segs[ size ].x1 = x;
                  segs[ size ].y1 = y;
                  segs[ size ].x2 = x;
                  segs[ size ].y2 = y;
                  size++;
               }
            }
            break;

         case ZH_BOXCH_RC_VSCRL_LU:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = 0;
            segs[ 1 ].y1 = celly / 2;
            segs[ 1 ].x2 = cellx - 1;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            size = 2;
            type = CH_SEG;

            for( y = 0; y < celly / 2; y++ )
            {
               for( x = ( y & 1 ) + 2; x < cellx && size < XWC_MAX_CHAR_SEGS; x += 2 )
               {
                  segs[ size ].x1 = x;
                  segs[ size ].y1 = y;
                  segs[ size ].x2 = x;
                  segs[ size ].y2 = y;
                  size++;
               }
            }
            break;

         case ZH_BOXCH_RC_VSCRL_RU:
            segs[ 0 ].x1 = cellx - 1;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = 0;
            segs[ 1 ].y1 = celly / 2;
            segs[ 1 ].x2 = cellx - 1;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            segs[ 2 ].x1 = cellx - 2;
            segs[ 2 ].y1 = celly / 2 + 3;
            segs[ 2 ].x2 = segs[ 2 ].x1;
            segs[ 2 ].y2 = celly - 1;

            size = 3;
            type = CH_SEG;

            for( y = 0; y < celly / 2; y++ )
            {
               for( x = ( y ^ cellx ) & 1; x < cellx - 2 && size < XWC_MAX_CHAR_SEGS; x += 2 )
               {
                  segs[ size ].x1 = x;
                  segs[ size ].y1 = y;
                  segs[ size ].x2 = x;
                  segs[ size ].y2 = y;
                  size++;
               }
            }
            break;

         case ZH_BOXCH_RC_VSCRL_L:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            size = 1;
            type = CH_SEG;

            for( y = 0; y < celly; y++ )
            {
               for( x = ( y & 1 ) + 2; x < cellx && size < XWC_MAX_CHAR_SEGS; x += 2 )
               {
                  segs[ size ].x1 = x;
                  segs[ size ].y1 = y;
                  segs[ size ].x2 = x;
                  segs[ size ].y2 = y;
                  size++;
               }
            }
            break;

         case ZH_BOXCH_RC_VSCRL_R:
            segs[ 0 ].x1 = cellx - 1;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            size = 1;
            type = CH_SEG;

            for( y = 0; y < celly; y++ )
            {
               for( x = ( y ^ cellx ) & 1; x < cellx - 2 && size < XWC_MAX_CHAR_SEGS; x += 2 )
               {
                  segs[ size ].x1 = x;
                  segs[ size ].y1 = y;
                  segs[ size ].x2 = x;
                  segs[ size ].y2 = y;
                  size++;
               }
            }
            break;

         case ZH_BOXCH_RC_HSCRL:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = cellx - 1;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            segs[ 1 ].x1 = 0;
            segs[ 1 ].y1 = celly - 1;
            segs[ 1 ].x2 = cellx - 1;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            size = 2;
            type = CH_SEG;

            for( y = 2; y < celly - 2; y++ )
            {
               for( x = y & 1; x < cellx && size < XWC_MAX_CHAR_SEGS; x += 2 )
               {
                  segs[ size ].x1 = x;
                  segs[ size ].y1 = y;
                  segs[ size ].x2 = x;
                  segs[ size ].y2 = y;
                  size++;
               }
            }
            break;

         case ZH_BOXCH_RC_0:
            type = CH_CHAR;
            usCh = '0';
            /* TODO */
            break;
         case ZH_BOXCH_RC_1:
            type = CH_CHAR;
            usCh = '1';
            /* TODO */
            break;
         case ZH_BOXCH_RC_2:
            type = CH_CHAR;
            usCh = '2';
            /* TODO */
            break;
         case ZH_BOXCH_RC_3:
            type = CH_CHAR;
            usCh = '3';
            /* TODO */
            break;
         case ZH_BOXCH_RC_4:
            type = CH_CHAR;
            usCh = '4';
            /* TODO */
            break;
         case ZH_BOXCH_RC_5:
            type = CH_CHAR;
            usCh = '5';
            /* TODO */
            break;
         case ZH_BOXCH_RC_6:
            type = CH_CHAR;
            usCh = '6';
            /* TODO */
            break;
         case ZH_BOXCH_RC_7:
            type = CH_CHAR;
            usCh = '7';
            /* TODO */
            break;
         case ZH_BOXCH_RC_8:
            type = CH_CHAR;
            usCh = '8';
            /* TODO */
            break;
         case ZH_BOXCH_RC_9:
            type = CH_CHAR;
            usCh = '9';
            /* TODO */
            break;
         case ZH_BOXCH_RC_DOT:
            type = CH_CHAR;
            usCh = '.';
            /* TODO */
            break;
         case ZH_BOXCH_RC_ACC:
            type = CH_CHAR;
            usCh = '\'';
            /* TODO */
            break;

         case ZH_BOXCH_RC_BOX_ML:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = celly / 2;
            segs[ 0 ].x2 = cellx - 1;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            segs[ 1 ].x1 = 0;
            segs[ 1 ].y1 = 0;
            segs[ 1 ].x2 = segs[ 1 ].x1;
            segs[ 1 ].y2 = celly - 1;

            size = 2;
            type = CH_SEG;
            break;

         case ZH_BOXCH_RC_BOX_MR:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = celly / 2;
            segs[ 0 ].x2 = cellx - 1;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            segs[ 1 ].x1 = cellx - 1;
            segs[ 1 ].y1 = 0;
            segs[ 1 ].x2 = segs[ 1 ].x1;
            segs[ 1 ].y2 = celly - 1;

            size = 2;
            type = CH_SEG;
            break;

         case ZH_BOXCH_RC_HWND_L:
            segs[ 0 ].x1 = cellx - 1;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = 0;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            segs[ 1 ].x1 = 0;
            segs[ 1 ].y1 = 0;
            segs[ 1 ].x2 = segs[ 1 ].x1;
            segs[ 1 ].y2 = celly - 1;

            segs[ 2 ].x1 = 0;
            segs[ 2 ].y1 = celly - 1;
            segs[ 2 ].x2 = cellx - 1;
            segs[ 2 ].y2 = segs[ 2 ].y1;

            segs[ 3 ].x1 = cellx - 1;
            segs[ 3 ].y1 = celly / 4 + 2;
            segs[ 3 ].x2 = cellx / 4 + 1;
            segs[ 3 ].y2 = segs[ 3 ].y1;

            segs[ 4 ].x1 = segs[ 3 ].x2;
            segs[ 4 ].y1 = segs[ 3 ].y2;
            segs[ 4 ].x2 = segs[ 4 ].x1;
            segs[ 4 ].y2 = celly - 4 - celly / 4;

            segs[ 5 ].x1 = segs[ 4 ].x2;
            segs[ 5 ].y1 = segs[ 4 ].y2;
            segs[ 5 ].x2 = cellx - 1;
            segs[ 5 ].y2 = segs[ 5 ].y1;

            segs[ 6 ].x1 = segs[ 5 ].x1 + 1;
            segs[ 6 ].y1 = segs[ 5 ].y1 + 1;
            segs[ 6 ].x2 = segs[ 5 ].x2;
            segs[ 6 ].y2 = segs[ 6 ].y1;

            size = 7;
            type = CH_SEG;
            break;

         case ZH_BOXCH_RC_HWND_R:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = cellx - 1;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            segs[ 1 ].x1 = segs[ 0 ].x2;
            segs[ 1 ].y1 = segs[ 0 ].y2;
            segs[ 1 ].x2 = segs[ 1 ].x1;
            segs[ 1 ].y2 = celly - 1;

            segs[ 2 ].x1 = segs[ 1 ].x2;
            segs[ 2 ].y1 = segs[ 1 ].y2;
            segs[ 2 ].x2 = 0;
            segs[ 2 ].y2 = segs[ 2 ].y1;

            segs[ 3 ].x1 = 0;
            segs[ 3 ].y1 = celly / 4 + 2;
            segs[ 3 ].x2 = cellx - cellx / 4 - 2;
            segs[ 3 ].y2 = segs[ 3 ].y1;

            segs[ 4 ].x1 = segs[ 3 ].x2;
            segs[ 4 ].y1 = segs[ 3 ].y2;
            segs[ 4 ].x2 = segs[ 4 ].x1;
            segs[ 4 ].y2 = celly - 4 - celly / 4;

            segs[ 5 ].x1 = segs[ 4 ].x2;
            segs[ 5 ].y1 = segs[ 4 ].y2;
            segs[ 5 ].x2 = 0;
            segs[ 5 ].y2 = segs[ 5 ].y1;

            segs[ 6 ].x1 = segs[ 5 ].x2;
            segs[ 6 ].y1 = segs[ 5 ].y2 + 1;
            segs[ 6 ].x2 = segs[ 5 ].x1 + 1;
            segs[ 6 ].y2 = segs[ 6 ].y1;

            segs[ 7 ].x1 = segs[ 6 ].x2;
            segs[ 7 ].y1 = segs[ 6 ].y2;
            segs[ 7 ].x2 = segs[ 7 ].x1;
            segs[ 7 ].y2 = segs[ 4 ].y1 + 1;

            size = 8;
            type = CH_SEG;
            break;

         case ZH_BOXCH_RC_BOX_TL:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = cellx - 1;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            segs[ 1 ].x1 = 0;
            segs[ 1 ].y1 = 0;
            segs[ 1 ].x2 = segs[ 1 ].x1;
            segs[ 1 ].y2 = celly - 1;

            size = 2;
            type = CH_SEG;
            break;

         case ZH_BOXCH_RC_BOX_T:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = cellx - 1;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            size = 1;
            type = CH_SEG;
            break;

         case ZH_BOXCH_RC_BOX_TR:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = cellx - 1;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            segs[ 1 ].x1 = cellx - 1;
            segs[ 1 ].y1 = 0;
            segs[ 1 ].x2 = segs[ 1 ].x1;
            segs[ 1 ].y2 = celly - 1;

            size = 2;
            type = CH_SEG;
            break;

         case ZH_BOXCH_RC_BOX_R:
            segs[ 0 ].x1 = cellx - 1;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            size = 1;
            type = CH_SEG;
            break;

         case ZH_BOXCH_RC_BOX_BR:
            segs[ 0 ].x1 = cellx - 1;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = cellx - 1;
            segs[ 1 ].y1 = celly - 1;
            segs[ 1 ].x2 = 0;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            size = 2;
            type = CH_SEG;
            break;

         case ZH_BOXCH_RC_BOX_B:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = celly - 1;
            segs[ 0 ].x2 = cellx - 1;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            size = 1;
            type = CH_SEG;
            break;

         case ZH_BOXCH_RC_BOX_BL:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = 0;
            segs[ 1 ].y1 = celly - 1;
            segs[ 1 ].x2 = cellx - 1;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            size = 2;
            type = CH_SEG;
            break;

         case ZH_BOXCH_RC_BOX_L:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            size = 1;
            type = CH_SEG;
            break;

         case ZH_BOXCH_RC_BOX_MT:
            segs[ 0 ].x1 = cellx / 2;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = 0;
            segs[ 1 ].y1 = 0;
            segs[ 1 ].x2 = cellx - 1;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            size = 2;
            type = CH_SEG;
            break;

         case ZH_BOXCH_RC_BOX_MB:
            segs[ 0 ].x1 = cellx / 2;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = 0;
            segs[ 1 ].y1 = celly - 1;
            segs[ 1 ].x2 = cellx - 1;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            size = 2;
            type = CH_SEG;
            break;

         case ZH_BOXCH_RC_BUTTON_CL:
            size = zh_gt_xwc_DefineBoxButtonL( segs, cellx, celly );
            yy = celly - 2 / 3;
            xx = cellx - 4;
            if( yy > xx )
               yy = xx;
            xx = ( xx * 2 + 1 ) / 3;
            if( xx < 2 )
               xx = 2;
            for( y = celly - yy - 3 - xx, i = 0; i < xx && size < XWC_MAX_CHAR_SEGS; ++y, ++i )
            {
               segs[ size ].x1 = 3;
               segs[ size ].y1 = y;
               segs[ size ].x2 = 3 + yy - 1;
               segs[ size ].y2 = y + yy - 1;
               size++;
            }
            if( size < XWC_MAX_CHAR_SEGS )
            {
               y = celly - 5 - xx;
               segs[ size ].x1 = cellx - 1;
               segs[ size ].y1 = y;
               segs[ size ].x2 = cellx - 1;
               segs[ size ].y2 = y + xx - 1;
               size++;
            }
            type = CH_SEG;
            break;

         case ZH_BOXCH_RC_BUTTON_CR:
            size = zh_gt_xwc_DefineBoxButtonR( segs, cellx, celly );
            yy = celly - 2 / 3;
            xx = cellx - 4;
            if( yy > xx )
               yy = xx;
            xx = ( xx * 2 + 1 ) / 3;
            if( xx < 2 )
               xx = 2;
            for( y = celly - 6 - xx, i = 0; i < xx && size < XWC_MAX_CHAR_SEGS; ++y, ++i )
            {
               segs[ size ].x1 = 0;
               segs[ size ].y1 = y;
               segs[ size ].x2 = yy;
               segs[ size ].y2 = y - yy;
               size++;
            }
            type = CH_SEG;
            break;

         case ZH_BOXCH_RC_FARROW_DL:
            size = zh_gt_xwc_DefineBoxButtonL( segs, cellx, celly );
            yy = ( celly - cellx ) / 2 + 1;
            yy = ZH_MAX( yy, 2 );
            for( y = celly - yy - 1, x = cellx - 1; x >= 2 && y >= 3 && size < XWC_MAX_CHAR_SEGS; --x, --y )
            {
               segs[ size ].x1 = x;
               segs[ size ].y1 = y;
               segs[ size ].x2 = cellx - 1;
               segs[ size ].y2 = y;
               size++;
            }
            type = CH_SEG;
            break;

         case ZH_BOXCH_RC_FARROW_DR:
            size = zh_gt_xwc_DefineBoxButtonR( segs, cellx, celly );
            yy = ( celly - cellx ) / 2 + 1;
            yy = ZH_MAX( yy, 2 );
            for( y = celly - yy - 2, x = 0; x < cellx - 3 && y >= 3 && size < XWC_MAX_CHAR_SEGS; ++x, --y )
            {
               segs[ size ].x1 = 0;
               segs[ size ].y1 = y;
               segs[ size ].x2 = x;
               segs[ size ].y2 = y;
               size++;
            }
            type = CH_SEG;
            break;

         case ZH_BOXCH_RC_DOTS:
            pts[ 0 ].x = 1;
            pts[ 0 ].y = celly / 2;
            size++;
            for( i = 3; i < cellx && size < XWC_MAX_CHAR_POINTS; i += 2 )
            {
               pts[ size ].x = 2;
               pts[ size ].y = 0;
               size++;
            }
            type = CH_PTS;
            break;

         case ZH_BOXCH_RC_DOTS_L:
            i = cellx / 2;
            xx = i - i / 2;
            yy = ZH_MAX( 2, xx - 1 );

            rect[ 1 ].x = cellx - xx / 2;
            rect[ 1 ].y = celly / 3 * 2;
            rect[ 1 ].width = cellx - rect[ 1 ].x;
            rect[ 1 ].height = yy;

            rect[ 0 ].x = rect[ 1 ].x - i;
            rect[ 0 ].y = rect[ 1 ].y;
            rect[ 0 ].width = xx;
            rect[ 0 ].height = yy;

            size = 2;
            type = CH_RECT;
            break;

         case ZH_BOXCH_RC_DOTS_R:
            i = cellx / 2;
            xx = i - i / 2;
            yy = ZH_MAX( 2, xx - 1 );

            rect[ 0 ].x = 0;
            rect[ 0 ].y = celly / 3 * 2;
            rect[ 0 ].width = xx - xx / 2;
            rect[ 0 ].height = yy;

            rect[ 1 ].x = rect[ 0 ].width + i - xx;
            rect[ 1 ].y = rect[ 0 ].y;
            rect[ 1 ].width = xx;
            rect[ 1 ].height = yy;

            size = 2;
            type = CH_RECT;
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
               inverse = ZH_TRUE;
            }
            xx = yy = 0;
            for( y = 0; y < celly; y++ )
            {
               for( x = start + ( skip >> 1 ) * ( ( y & 1 ) ^ mod ); x < cellx; x += skip )
               {
                  /* this is font size dependent, we have to add this checking
                   * to at least avoid GPF for if user set very large font though
                   * character definition will not be finished
                   */
                  if( size >= XWC_MAX_CHAR_POINTS )
                     break;
                  pts[ size ].x = x - xx;
                  pts[ size ].y = y - yy;
                  xx = x;
                  yy = y;
                  size++;
               }
            }
            type = size == 0 ? CH_NONE : CH_PTS;
            break;
         }
         case ZH_BOXCH_ARROW_R:
            i = ZH_MIN( ( celly >> 1 ), cellx ) - 3;
            pts[ 0 ].x = ( ( cellx - i ) >> 1 );
            pts[ 0 ].y = ( celly >> 1 ) - i;
            pts[ 1 ].x = i;
            pts[ 1 ].y = i;
            pts[ 2 ].x = -i;
            pts[ 2 ].y = i;
            size = 3;
            type = CH_POLY;
            break;

         case ZH_BOXCH_ARROW_L:
            i = ZH_MIN( ( celly >> 1 ), cellx ) - 3;
            pts[ 0 ].x = ( ( cellx - i ) >> 1 ) + i;
            pts[ 0 ].y = ( celly >> 1 ) - i;
            pts[ 1 ].x = - i;
            pts[ 1 ].y = i;
            pts[ 2 ].x = i;
            pts[ 2 ].y = i;
            size = 3;
            type = CH_POLY;
            break;

         case ZH_BOXCH_ARROW_U:
            i = ZH_MIN( celly, cellx >> 1 );
            pts[ 0 ].x = ( cellx >> 1 ) - i;
            pts[ 0 ].y = ( ( celly - i ) >> 1 ) + i;
            pts[ 1 ].x = i;
            pts[ 1 ].y = -i;
            pts[ 2 ].x = i;
            pts[ 2 ].y = i;
            size = 3;
            type = CH_POLY;
            break;

         case ZH_BOXCH_ARROW_D:
            i = ZH_MIN( celly, cellx >> 1 );
            pts[ 0 ].x = ( cellx >> 1 ) - i;
            pts[ 0 ].y = ( ( celly - i ) >> 1 );
            pts[ 1 ].x = i;
            pts[ 1 ].y = i;
            pts[ 2 ].x = i;
            pts[ 2 ].y = -i;
            size = 3;
            type = CH_POLY;
            break;

         case ZH_BOXCH_FULL:
            inverse = ZH_TRUE;
            type = CH_NONE;
            break;

         case ZH_BOXCH_FULL_B:
            inverse = ZH_TRUE;
            /* fallthrough */

         case ZH_BOXCH_FULL_T:
            rect[ 0 ].x = 0;
            rect[ 0 ].y = 0;
            rect[ 0 ].width = cellx;
            rect[ 0 ].height = celly / 2;
            size = 1;
            type = CH_RECT;
            break;

         case ZH_BOXCH_FULL_R:
            inverse = ZH_TRUE;
            /* fallthrough */

         case ZH_BOXCH_FULL_L:
            rect[ 0 ].x = 0;
            rect[ 0 ].y = 0;
            rect[ 0 ].width = cellx / 2;
            rect[ 0 ].height = celly;
            size = 1;
            type = CH_RECT;
            break;

         case ZH_BOXCH_SNG_LT:
            segs[ 0 ].x1 = cellx / 2;
            segs[ 0 ].y1 = celly - 1;
            segs[ 0 ].x2 = cellx / 2;
            segs[ 0 ].y2 = celly / 2;

            segs[ 1 ].x1 = segs[ 0 ].x2;
            segs[ 1 ].y1 = segs[ 0 ].y2;
            segs[ 1 ].x2 = cellx - 1;
            segs[ 1 ].y2 = celly / 2;

            size = 2;
            type = CH_SEG;
            break;

         case ZH_BOXCH_SNG_TD:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = celly / 2;
            segs[ 0 ].x2 = cellx - 1;
            segs[ 0 ].y2 = celly / 2;

            segs[ 1 ].x1 = cellx / 2;
            segs[ 1 ].y1 = segs[ 0 ].y1;
            segs[ 1 ].x2 = segs[ 1 ].x1;
            segs[ 1 ].y2 = celly - 1;

            size = 2;
            type = CH_SEG;
            break;

         case ZH_BOXCH_SNG_RT:
            segs[ 0 ].x1 = cellx / 2;
            segs[ 0 ].y1 = celly - 1;
            segs[ 0 ].x2 = cellx / 2;
            segs[ 0 ].y2 = celly / 2;

            segs[ 1 ].x1 = segs[ 0 ].x2;
            segs[ 1 ].y1 = segs[ 0 ].y2;
            segs[ 1 ].x2 = 0;
            segs[ 1 ].y2 = celly / 2;

            size = 2;
            type = CH_SEG;
            break;

         case ZH_BOXCH_SNG_LB:
            segs[ 0 ].x1 = cellx / 2;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = cellx / 2;
            segs[ 0 ].y2 = celly / 2;

            segs[ 1 ].x1 = segs[ 0 ].x2;
            segs[ 1 ].y1 = segs[ 0 ].y2;
            segs[ 1 ].x2 = cellx - 1;
            segs[ 1 ].y2 = celly / 2;

            size = 2;
            type = CH_SEG;
            break;

         case ZH_BOXCH_SNG_BU:
            segs[ 0 ].x1 = cellx / 2;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = cellx / 2;
            segs[ 0 ].y2 = celly / 2;

            segs[ 1 ].x1 = 0;
            segs[ 1 ].y1 = segs[ 0 ].y2;
            segs[ 1 ].x2 = cellx - 1;
            segs[ 1 ].y2 = celly / 2;

            size = 2;
            type = CH_SEG;
            break;

         case ZH_BOXCH_SNG_RB:
            segs[ 0 ].x1 = cellx / 2;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = cellx / 2;
            segs[ 0 ].y2 = celly / 2;

            segs[ 1 ].x1 = segs[ 0 ].x2;
            segs[ 1 ].y1 = segs[ 0 ].y2;
            segs[ 1 ].x2 = 0;
            segs[ 1 ].y2 = celly / 2;

            size = 2;
            type = CH_SEG;
            break;

         case ZH_BOXCH_SNG_VL:
            segs[ 0 ].x1 = cellx / 2;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = segs[ 0 ].x1;
            segs[ 1 ].y1 = celly / 2;
            segs[ 1 ].x2 = cellx - 1;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            size = 2;
            type = CH_SEG;
            break;

         case ZH_BOXCH_SNG_VR:
            segs[ 0 ].x1 = cellx / 2;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = segs[ 0 ].x1;
            segs[ 1 ].y1 = celly / 2;
            segs[ 1 ].x2 = 0;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            size = 2;
            type = CH_SEG;
            break;

         case ZH_BOXCH_SNG_CRS:
            segs[ 0 ].x1 = cellx / 2;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = 0;
            segs[ 1 ].y1 = celly / 2;
            segs[ 1 ].x2 = cellx - 1;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            size = 2;
            type = CH_SEG;
            break;

         case ZH_BOXCH_SNG_HOR:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = celly / 2;
            segs[ 0 ].x2 = cellx - 1;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            size = 1;
            type = CH_SEG;
            break;

         case ZH_BOXCH_SNG_VRT:
            segs[ 0 ].x1 = cellx / 2;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            size = 1;
            type = CH_SEG;
            break;

         case ZH_BOXCH_DBL_LT:
            segs[ 0 ].x1 = cellx / 2 - 1;
            segs[ 0 ].y1 = celly - 1;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly / 2 - 1;

            segs[ 1 ].x1 = segs[ 0 ].x2;
            segs[ 1 ].y1 = segs[ 0 ].y2;
            segs[ 1 ].x2 = cellx - 1;
            segs[ 1 ].y2 = segs[ 0 ].y2;

            segs[ 2 ].x1 = cellx / 2 + 1;
            segs[ 2 ].y1 = celly - 1;
            segs[ 2 ].x2 = segs[ 2 ].x1;
            segs[ 2 ].y2 = celly / 2 + 1;

            segs[ 3 ].x1 = segs[ 2 ].x2;
            segs[ 3 ].y1 = segs[ 2 ].y2;
            segs[ 3 ].x2 = cellx - 1;
            segs[ 3 ].y2 = segs[ 2 ].y2;

            size = 4;
            type = CH_SEG;
            break;

         case ZH_BOXCH_DBL_TD:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = celly / 2 - 1;
            segs[ 0 ].x2 = cellx - 1;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            segs[ 1 ].x1 = segs[ 0 ].x1;
            segs[ 1 ].y1 = celly / 2 + 1;
            segs[ 1 ].x2 = cellx / 2 - 1;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            segs[ 2 ].x1 = cellx / 2 + 1;
            segs[ 2 ].y1 = celly / 2 + 1;
            segs[ 2 ].x2 = cellx - 1;
            segs[ 2 ].y2 = segs[ 2 ].y1;

            segs[ 3 ].x1 = segs[ 1 ].x2;
            segs[ 3 ].y1 = segs[ 1 ].y1;
            segs[ 3 ].x2 = segs[ 1 ].x2;
            segs[ 3 ].y2 = celly - 1;

            segs[ 4 ].x1 = segs[ 2 ].x1;
            segs[ 4 ].y1 = segs[ 2 ].y1;
            segs[ 4 ].x2 = segs[ 2 ].x1;
            segs[ 4 ].y2 = celly - 1;

            size = 5;
            type = CH_SEG;
            break;

         case ZH_BOXCH_DBL_RT:
            segs[ 0 ].x1 = cellx / 2 - 1;
            segs[ 0 ].y1 = celly - 1;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly / 2 + 1;

            segs[ 1 ].x1 = segs[ 0 ].x2;
            segs[ 1 ].y1 = segs[ 0 ].y2;
            segs[ 1 ].x2 = 0;
            segs[ 1 ].y2 = segs[ 0 ].y2;

            segs[ 2 ].x1 = cellx / 2 + 1;
            segs[ 2 ].y1 = celly - 1;
            segs[ 2 ].x2 = segs[ 2 ].x1;
            segs[ 2 ].y2 = celly / 2 - 1;

            segs[ 3 ].x1 = segs[ 2 ].x2;
            segs[ 3 ].y1 = segs[ 2 ].y2;
            segs[ 3 ].x2 = 0;
            segs[ 3 ].y2 = segs[ 2 ].y2;

            size = 4;
            type = CH_SEG;
            break;

         case ZH_BOXCH_DBL_LB:
            segs[ 0 ].x1 = cellx / 2 - 1;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly / 2 + 1;

            segs[ 1 ].x1 = segs[ 0 ].x2;
            segs[ 1 ].y1 = segs[ 0 ].y2;
            segs[ 1 ].x2 = cellx - 1;
            segs[ 1 ].y2 = segs[ 0 ].y2;

            segs[ 2 ].x1 = cellx / 2 + 1;
            segs[ 2 ].y1 = 0;
            segs[ 2 ].x2 = segs[ 2 ].x1;
            segs[ 2 ].y2 = celly / 2 - 1;

            segs[ 3 ].x1 = segs[ 2 ].x2;
            segs[ 3 ].y1 = segs[ 2 ].y2;
            segs[ 3 ].x2 = cellx - 1;
            segs[ 3 ].y2 = segs[ 2 ].y2;

            size = 4;
            type = CH_SEG;
            break;

         case ZH_BOXCH_DBL_BU:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = celly / 2 + 1;
            segs[ 0 ].x2 = cellx - 1;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            segs[ 1 ].x1 = segs[ 0 ].x1;
            segs[ 1 ].y1 = celly / 2 - 1;
            segs[ 1 ].x2 = cellx / 2 - 1;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            segs[ 2 ].x1 = cellx / 2 + 1;
            segs[ 2 ].y1 = celly / 2 - 1;
            segs[ 2 ].x2 = cellx - 1;
            segs[ 2 ].y2 = segs[ 2 ].y1;

            segs[ 3 ].x1 = segs[ 1 ].x2;
            segs[ 3 ].y1 = segs[ 1 ].y1;
            segs[ 3 ].x2 = segs[ 1 ].x2;
            segs[ 3 ].y2 = 0;

            segs[ 4 ].x1 = segs[ 2 ].x1;
            segs[ 4 ].y1 = segs[ 2 ].y1;
            segs[ 4 ].x2 = segs[ 2 ].x1;
            segs[ 4 ].y2 = 0;

            size = 5;
            type = CH_SEG;
            break;

         case ZH_BOXCH_DBL_RB:
            segs[ 0 ].x1 = cellx / 2 - 1;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly / 2 - 1;

            segs[ 1 ].x1 = segs[ 0 ].x2;
            segs[ 1 ].y1 = segs[ 0 ].y2;
            segs[ 1 ].x2 = 0;
            segs[ 1 ].y2 = segs[ 0 ].y2;

            segs[ 2 ].x1 = cellx / 2 + 1;
            segs[ 2 ].y1 = 0;
            segs[ 2 ].x2 = segs[ 2 ].x1;
            segs[ 2 ].y2 = celly / 2 + 1;

            segs[ 3 ].x1 = segs[ 2 ].x2;
            segs[ 3 ].y1 = segs[ 2 ].y2;
            segs[ 3 ].x2 = 0;
            segs[ 3 ].y2 = segs[ 2 ].y2;

            size = 4;
            type = CH_SEG;
            break;

         case ZH_BOXCH_DBL_VL:
            segs[ 0 ].x1 = cellx / 2 - 1;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = cellx / 2 + 1;
            segs[ 1 ].y1 = 0;
            segs[ 1 ].x2 = segs[ 1 ].x1;
            segs[ 1 ].y2 = celly / 2 - 1;

            segs[ 2 ].x1 = segs[ 1 ].x1;
            segs[ 2 ].y1 = celly / 2 + 1;
            segs[ 2 ].x2 = segs[ 1 ].x1;
            segs[ 2 ].y2 = celly - 1;

            segs[ 3 ].x1 = segs[ 1 ].x1;
            segs[ 3 ].y1 = segs[ 1 ].y2;
            segs[ 3 ].x2 = cellx - 1;
            segs[ 3 ].y2 = segs[ 3 ].y1;

            segs[ 4 ].x1 = segs[ 2 ].x1;
            segs[ 4 ].y1 = segs[ 2 ].y1;
            segs[ 4 ].x2 = cellx - 1;
            segs[ 4 ].y2 = segs[ 2 ].y1;

            size = 5;
            type = CH_SEG;
            break;


         case ZH_BOXCH_DBL_VR:
            segs[ 0 ].x1 = cellx / 2 + 1;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = cellx / 2 - 1;
            segs[ 1 ].y1 = 0;
            segs[ 1 ].x2 = segs[ 1 ].x1;
            segs[ 1 ].y2 = celly / 2 - 1;

            segs[ 2 ].x1 = segs[ 1 ].x1;
            segs[ 2 ].y1 = celly / 2 + 1;
            segs[ 2 ].x2 = segs[ 1 ].x1;
            segs[ 2 ].y2 = celly - 1;

            segs[ 3 ].x1 = segs[ 1 ].x1;
            segs[ 3 ].y1 = segs[ 1 ].y2;
            segs[ 3 ].x2 = 0;
            segs[ 3 ].y2 = segs[ 3 ].y1;

            segs[ 4 ].x1 = segs[ 2 ].x1;
            segs[ 4 ].y1 = segs[ 2 ].y1;
            segs[ 4 ].x2 = 0;
            segs[ 4 ].y2 = segs[ 2 ].y1;

            size = 5;
            type = CH_SEG;
            break;

         case ZH_BOXCH_DBL_CRS:
            segs[ 0 ].x1 = cellx / 2 - 1;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly / 2 - 1;

            segs[ 1 ].x1 = segs[ 0 ].x1;
            segs[ 1 ].y1 = celly / 2 + 1;
            segs[ 1 ].x2 = segs[ 0 ].x1;
            segs[ 1 ].y2 = celly - 1;

            segs[ 2 ].x1 = segs[ 0 ].x1;
            segs[ 2 ].y1 = segs[ 0 ].y2;
            segs[ 2 ].x2 = 0;
            segs[ 2 ].y2 = segs[ 2 ].y1;

            segs[ 3 ].x1 = segs[ 1 ].x1;
            segs[ 3 ].y1 = segs[ 1 ].y1;
            segs[ 3 ].x2 = 0;
            segs[ 3 ].y2 = segs[ 1 ].y1;

            segs[ 4 ].x1 = cellx / 2 + 1;
            segs[ 4 ].y1 = 0;
            segs[ 4 ].x2 = segs[ 4 ].x1;
            segs[ 4 ].y2 = celly / 2 - 1;

            segs[ 5 ].x1 = segs[ 4 ].x1;
            segs[ 5 ].y1 = celly / 2 + 1;
            segs[ 5 ].x2 = segs[ 4 ].x1;
            segs[ 5 ].y2 = celly - 1;

            segs[ 6 ].x1 = segs[ 4 ].x1;
            segs[ 6 ].y1 = segs[ 4 ].y2;
            segs[ 6 ].x2 = cellx - 1;
            segs[ 6 ].y2 = segs[ 6 ].y1;

            segs[ 7 ].x1 = segs[ 5 ].x1;
            segs[ 7 ].y1 = segs[ 5 ].y1;
            segs[ 7 ].x2 = cellx - 1;
            segs[ 7 ].y2 = segs[ 5 ].y1;

            size = 8;
            type = CH_SEG;
            break;

         case ZH_BOXCH_DBL_HOR:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = celly / 2 + 1;
            segs[ 0 ].x2 = cellx - 1;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            segs[ 1 ].x1 = 0;
            segs[ 1 ].y1 = celly / 2 - 1;
            segs[ 1 ].x2 = cellx - 1;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            size = 2;
            type = CH_SEG;
            break;

         case ZH_BOXCH_DBL_VRT:
            segs[ 0 ].x1 = cellx / 2 - 1;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = cellx / 2 + 1;
            segs[ 1 ].y1 = 0;
            segs[ 1 ].x2 = segs[ 1 ].x1;
            segs[ 1 ].y2 = celly - 1;

            size = 2;
            type = CH_SEG;
            break;

         case ZH_BOXCH_SNG_L_DBL_T:
            segs[ 0 ].x1 = cellx / 2;
            segs[ 0 ].y1 = celly / 2 - 1;
            segs[ 0 ].x2 = cellx - 1;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            segs[ 1 ].x1 = segs[ 0 ].x1;
            segs[ 1 ].y1 = celly / 2 + 1;
            segs[ 1 ].x2 = segs[ 0 ].x2;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            segs[ 2 ].x1 = cellx / 2;
            segs[ 2 ].y1 = celly / 2 - 1;
            segs[ 2 ].x2 = segs[ 2 ].x1;
            segs[ 2 ].y2 = celly - 1;

            size = 3;
            type = CH_SEG;
            break;

         case ZH_BOXCH_SNG_T_DBL_D:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = celly / 2;
            segs[ 0 ].x2 = cellx - 1;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            segs[ 1 ].x1 = cellx / 2 - 1;
            segs[ 1 ].y1 = celly / 2;
            segs[ 1 ].x2 = segs[ 1 ].x1;
            segs[ 1 ].y2 = celly - 1;

            segs[ 2 ].x1 = cellx / 2 + 1;
            segs[ 2 ].y1 = segs[ 1 ].y1;
            segs[ 2 ].x2 = segs[ 2 ].x1;
            segs[ 2 ].y2 = segs[ 1 ].y2;

            size = 3;
            type = CH_SEG;
            break;

         case ZH_BOXCH_SNG_R_DBL_T:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = celly / 2;
            segs[ 0 ].x2 = cellx / 2 + 1;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            segs[ 1 ].x1 = cellx / 2 + 1;
            segs[ 1 ].y1 = segs[ 0 ].y1;
            segs[ 1 ].x2 = segs[ 0 ].x2;
            segs[ 1 ].y2 = celly - 1;

            segs[ 2 ].x1 = cellx / 2 - 1;
            segs[ 2 ].y1 = celly / 2;
            segs[ 2 ].x2 = segs[ 2 ].x1;
            segs[ 2 ].y2 = celly - 1;

            size = 3;
            type = CH_SEG;
            break;

         case ZH_BOXCH_SNG_L_DBL_B:
            segs[ 0 ].x1 = cellx / 2;
            segs[ 0 ].y1 = celly / 2 - 1;
            segs[ 0 ].x2 = cellx - 1;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            segs[ 1 ].x1 = segs[ 0 ].x1;
            segs[ 1 ].y1 = celly / 2 + 1;
            segs[ 1 ].x2 = segs[ 0 ].x2;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            segs[ 2 ].x1 = cellx / 2;
            segs[ 2 ].y1 = 0;
            segs[ 2 ].x2 = segs[ 2 ].x1;
            segs[ 2 ].y2 = celly / 2 + 1;

            size = 3;
            type = CH_SEG;
            break;

         case ZH_BOXCH_SNG_B_DBL_U:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = celly / 2;
            segs[ 0 ].x2 = cellx - 1;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            segs[ 1 ].x1 = cellx / 2 - 1;
            segs[ 1 ].y1 = 0;
            segs[ 1 ].x2 = segs[ 1 ].x1;
            segs[ 1 ].y2 = celly / 2;

            segs[ 2 ].x1 = cellx / 2 + 1;
            segs[ 2 ].y1 = 0;
            segs[ 2 ].x2 = segs[ 2 ].x1;
            segs[ 2 ].y2 = celly / 2;

            size = 3;
            type = CH_SEG;
            break;

         case ZH_BOXCH_SNG_R_DBL_B:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = celly / 2;
            segs[ 0 ].x2 = cellx / 2 + 1;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            segs[ 1 ].x1 = cellx / 2 + 1;
            segs[ 1 ].y1 = 0;
            segs[ 1 ].x2 = segs[ 1 ].x1;
            segs[ 1 ].y2 = celly / 2;

            segs[ 2 ].x1 = cellx / 2 - 1;
            segs[ 2 ].y1 = 0;
            segs[ 2 ].x2 = segs[ 2 ].x1;
            segs[ 2 ].y2 = celly / 2;

            size = 3;
            type = CH_SEG;
            break;

         case ZH_BOXCH_SNG_V_DBL_L:
            segs[ 0 ].x1 = cellx / 2;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = segs[ 0 ].x1;
            segs[ 1 ].y1 = celly / 2 - 1;
            segs[ 1 ].x2 = cellx - 1;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            segs[ 2 ].x1 = segs[ 0 ].x1;
            segs[ 2 ].y1 = celly / 2 + 1;
            segs[ 2 ].x2 = segs[ 1 ].x2;
            segs[ 2 ].y2 = segs[ 2 ].y1;

            size = 3;
            type = CH_SEG;
            break;

         case ZH_BOXCH_SNG_V_DBL_R:
            segs[ 0 ].x1 = cellx / 2;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = 0;
            segs[ 1 ].y1 = celly / 2 - 1;
            segs[ 1 ].x2 = segs[ 0 ].x1;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            segs[ 2 ].x1 = 0;
            segs[ 2 ].y1 = celly / 2 + 1;
            segs[ 2 ].x2 = segs[ 0 ].x1;
            segs[ 2 ].y2 = segs[ 2 ].y1;

            size = 3;
            type = CH_SEG;
            break;

         case ZH_BOXCH_SNG_DBL_CRS:
            segs[ 0 ].x1 = cellx / 2;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = 0;
            segs[ 1 ].y1 = celly / 2 - 1;
            segs[ 1 ].x2 = cellx - 1;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            segs[ 2 ].x1 = 0;
            segs[ 2 ].y1 = celly / 2 + 1;
            segs[ 2 ].x2 = segs[ 1 ].x2;
            segs[ 2 ].y2 = segs[ 2 ].y1;

            size = 3;
            type = CH_SEG;
            break;

         case ZH_BOXCH_DBL_L_SNG_T:
            segs[ 0 ].x1 = cellx / 2 - 1;
            segs[ 0 ].y1 = celly / 2;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = cellx / 2 + 1;
            segs[ 1 ].y1 = segs[ 0 ].y1;
            segs[ 1 ].x2 = segs[ 1 ].x1;
            segs[ 1 ].y2 = segs[ 0 ].y2;

            segs[ 2 ].x1 = segs[ 0 ].x1;
            segs[ 2 ].y1 = segs[ 0 ].y1;
            segs[ 2 ].x2 = cellx - 1;
            segs[ 2 ].y2 = segs[ 0 ].y1;

            size = 3;
            type = CH_SEG;
            break;

         case ZH_BOXCH_DBL_T_SNG_D:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = celly / 2 - 1;
            segs[ 0 ].x2 = cellx - 1;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            segs[ 1 ].x1 = segs[ 0 ].x1;
            segs[ 1 ].y1 = celly / 2 + 1;
            segs[ 1 ].x2 = segs[ 0 ].x2;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            segs[ 2 ].x1 = cellx / 2;
            segs[ 2 ].y1 = celly / 2 + 1;
            segs[ 2 ].x2 = segs[ 2 ].x1;
            segs[ 2 ].y2 = celly - 1;

            size = 3;
            type = CH_SEG;
            break;

         case ZH_BOXCH_DBL_R_SNG_T:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = celly / 2 - 1;
            segs[ 0 ].x2 = cellx / 2;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            segs[ 1 ].x1 = segs[ 0 ].x1;
            segs[ 1 ].y1 = celly / 2 + 1;
            segs[ 1 ].x2 = segs[ 0 ].x2;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            segs[ 2 ].x1 = cellx / 2;
            segs[ 2 ].y1 = celly / 2 - 1;
            segs[ 2 ].x2 = segs[ 2 ].x1;
            segs[ 2 ].y2 = celly - 1;

            size = 3;
            type = CH_SEG;
            break;

         case ZH_BOXCH_DBL_L_SNG_B:
            segs[ 0 ].x1 = cellx / 2 - 1;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly / 2;

            segs[ 1 ].x1 = cellx / 2 + 1;
            segs[ 1 ].y1 = segs[ 0 ].y1;
            segs[ 1 ].x2 = segs[ 1 ].x1;
            segs[ 1 ].y2 = segs[ 0 ].y2;

            segs[ 2 ].x1 = cellx / 2 - 1;
            segs[ 2 ].y1 = segs[ 0 ].y2;
            segs[ 2 ].x2 = cellx - 1;
            segs[ 2 ].y2 = segs[ 0 ].y2;

            size = 3;
            type = CH_SEG;
            break;

         case ZH_BOXCH_DBL_B_SNG_U:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = celly / 2 - 1;
            segs[ 0 ].x2 = cellx - 1;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            segs[ 1 ].x1 = segs[ 0 ].x1;
            segs[ 1 ].y1 = celly / 2 + 1;
            segs[ 1 ].x2 = segs[ 0 ].x2;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            segs[ 2 ].x1 = cellx / 2;
            segs[ 2 ].y1 = 0;
            segs[ 2 ].x2 = segs[ 2 ].x1;
            segs[ 2 ].y2 = celly / 2 - 1;

            size = 3;
            type = CH_SEG;
            break;

         case ZH_BOXCH_DBL_R_SNG_B:
            segs[ 0 ].x1 = 0;
            segs[ 0 ].y1 = celly / 2 - 1;
            segs[ 0 ].x2 = cellx / 2;
            segs[ 0 ].y2 = segs[ 0 ].y1;

            segs[ 1 ].x1 = segs[ 0 ].x1;
            segs[ 1 ].y1 = celly / 2 + 1;
            segs[ 1 ].x2 = segs[ 0 ].x2;
            segs[ 1 ].y2 = segs[ 1 ].y1;

            segs[ 2 ].x1 = cellx / 2;
            segs[ 2 ].y1 = 0;
            segs[ 2 ].x2 = segs[ 2 ].x1;
            segs[ 2 ].y2 = celly / 2 + 1;

            size = 3;
            type = CH_SEG;
            break;

         case ZH_BOXCH_DBL_V_SNG_L:
            segs[ 0 ].x1 = cellx / 2 - 1;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = cellx / 2 + 1;
            segs[ 1 ].y1 = segs[ 0 ].y1;
            segs[ 1 ].x2 = segs[ 1 ].x1;
            segs[ 1 ].y2 = segs[ 0 ].y2;

            segs[ 2 ].x1 = segs[ 1 ].x1;
            segs[ 2 ].y1 = celly / 2;
            segs[ 2 ].x2 = cellx - 1;
            segs[ 2 ].y2 = segs[ 2 ].y1;

            size = 3;
            type = CH_SEG;
            break;

         case ZH_BOXCH_DBL_V_SNG_R:
            segs[ 0 ].x1 = cellx / 2 - 1;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = cellx / 2 + 1;
            segs[ 1 ].y1 = segs[ 0 ].y1;
            segs[ 1 ].x2 = segs[ 1 ].x1;
            segs[ 1 ].y2 = segs[ 0 ].y2;

            segs[ 2 ].x1 = 0;
            segs[ 2 ].y1 = celly / 2;
            segs[ 2 ].x2 = segs[ 0 ].x1;
            segs[ 2 ].y2 = segs[ 2 ].y1;

            size = 3;
            type = CH_SEG;
            break;

         case ZH_BOXCH_DBL_SNG_CRS:
            segs[ 0 ].x1 = cellx / 2 - 1;
            segs[ 0 ].y1 = 0;
            segs[ 0 ].x2 = segs[ 0 ].x1;
            segs[ 0 ].y2 = celly - 1;

            segs[ 1 ].x1 = cellx / 2 + 1;
            segs[ 1 ].y1 = segs[ 0 ].y1;
            segs[ 1 ].x2 = segs[ 1 ].x1;
            segs[ 1 ].y2 = segs[ 0 ].y2;

            segs[ 2 ].x1 = 0;
            segs[ 2 ].y1 = celly / 2;
            segs[ 2 ].x2 = cellx - 1;
            segs[ 2 ].y2 = segs[ 2 ].y1;

            size = 3;
            type = CH_SEG;
            break;

         case ZH_BOXCH_SQUARE:
            rect[ 0 ].width = cellx - ZH_MAX( cellx >> 2, 2 );
            rect[ 0 ].height = rect[ 0 ].width;
            rect[ 0 ].x = ( ( cellx - rect[ 0 ].width ) >> 1 );
            rect[ 0 ].y = ( ( celly - rect[ 0 ].height ) >> 1 );
            size = 1;
            type = CH_RECT;
            break;
#if 0
         default:
            rect[ 0 ].x = 1;
            rect[ 0 ].y = 1;
            rect[ 0 ].width = cellx - 2;
            rect[ 0 ].height = celly - 2;
            size = 1;
            type = CH_RECT;
            break;
#endif
      }

   if( type != CH_UNDEF )
   {
      bxCh->type = type;
      bxCh->u.ch16 = usCh;
      bxCh->size = size;
      bxCh->inverse = inverse;
      switch( type )
      {
         case CH_SEG:
            bxCh->u.seg = ( XSegment * ) zh_xgrab( sizeof( XSegment ) * size );
            memcpy( bxCh->u.seg, segs, sizeof( XSegment ) * size );
            break;
         case CH_RECT:
            bxCh->u.rect = ( XRectangle * ) zh_xgrab( sizeof( XRectangle ) * size );
            memcpy( bxCh->u.rect, rect, sizeof( XRectangle ) * size );
            break;
         case CH_PTS:
         case CH_LINE:
         case CH_POLY:
            bxCh->u.pts = ( XPoint * ) zh_xgrab( sizeof( XPoint ) * size );
            memcpy( bxCh->u.pts, pts, sizeof( XPoint ) * size );
            break;
         case CH_UNDEF:
         case CH_CHAR:
         case CH_CHBX:
         case CH_NONE:
         case CH_IMG:
            break;
      }
      return ZH_TRUE;
   }
   return ZH_FALSE;
}

/* *********************************************************************** */

static void zh_gt_xwc_ResetCharTrans( PXWND_DEF wnd )
{
   int i;

   for( i = 0; i <= wnd->boxCount; i++ )
   {
      switch( wnd->boxTrans[ i ].type )
      {
         case CH_IMG:
            XDestroyImage( wnd->boxTrans[ i ].u.img );
            break;
         case CH_SEG:
            zh_xfree( wnd->boxTrans[ i ].u.seg );
            break;
         case CH_RECT:
            zh_xfree( wnd->boxTrans[ i ].u.rect );
            break;
         case CH_PTS:
         case CH_LINE:
         case CH_POLY:
            zh_xfree( wnd->boxTrans[ i ].u.pts );
            break;
         case CH_UNDEF:
         case CH_CHAR:
         case CH_CHBX:
         case CH_NONE:
            break;
      }
   }
   memset( wnd->boxTrans, 0, sizeof( wnd->boxTrans ) );
   wnd->boxCount = 0;

   wnd->boxTrans[ 0 ].type = CH_CHAR;
   wnd->boxTrans[ 0 ].u.ch16 = 0;
   wnd->boxTrans[ 0 ].size = 0;
   wnd->boxTrans[ 0 ].inverse = ZH_FALSE;

   for( i = 0; i < ZH_BOXCH_TRANS_COUNT; ++i )
      wnd->boxIndex[ i ] = ZH_BOXCH_TRANS_MAX;
}

/* *********************************************************************** */

static XWC_CharTrans * zh_gt_xwc_GetBoxChar( PXWND_DEF wnd, ZH_USHORT uc16 )
{
   int iPos, iTrans;

   if( ! wnd->fDrawBox )
   {
      wnd->boxTrans[ 0 ].u.ch16 = zh_cdpGetU16Ctrl( uc16 );
      return &wnd->boxTrans[ 0 ];
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
   {
      wnd->boxTrans[ 0 ].u.ch16 = zh_cdpGetU16Ctrl( uc16 );
      return &wnd->boxTrans[ 0 ];
   }

   iTrans = wnd->boxIndex[ iPos ];
   if( iTrans == ZH_BOXCH_TRANS_MAX )
   {
      if( wnd->boxCount < ZH_BOXCH_TRANS_MAX - 1 )
      {
         iTrans = wnd->boxCount + 1;
         if( zh_gt_xwc_DefineBoxChar( wnd, uc16, &wnd->boxTrans[ iTrans ] ) )
            wnd->boxCount = iTrans;
         else
            iTrans = 0;
      }
      else
         iTrans = 0;
      wnd->boxIndex[ iPos ] = iTrans;
   }

   if( iTrans == 0 )
   {
      wnd->boxTrans[ 0 ].u.ch16 = zh_cdpGetU16Ctrl( uc16 );
      return &wnd->boxTrans[ 0 ];
   }

   return &wnd->boxTrans[ iTrans ];
}

/* *********************************************************************** */

/*
 *  functions for handling the input queues for the mouse and keyboard
 */

/* *********************************************************************** */

static void zh_gt_xwc_MouseInit( PXWND_DEF wnd )
{
   wnd->mouseNumButtons = XGetPointerMapping( wnd->dpy,
                                              wnd->mouseButtonsMap,
                                              XWC_MAX_BUTTONS );

   if( wnd->mouseNumButtons > XWC_MAX_BUTTONS )
   {
      wnd->mouseNumButtons = XWC_MAX_BUTTONS;
   }
   wnd->mouseButtonsState = 0;
   wnd->mouseGotoCol = -1;
   wnd->mouseGotoRow = -1;
}

static void zh_gt_xwc_AddCharToInputQueue( PXWND_DEF wnd, int keyCode )
{
   if( wnd->keyBuffNO > 0 && ZH_INKEY_ISMOUSEPOS( keyCode ) )
   {
      int keyBuffPtr = wnd->keyBuffPointer - 1;
      if( keyBuffPtr < 0 )
         keyBuffPtr += XWC_CHAR_QUEUE_SIZE;
      if( ZH_INKEY_ISMOUSEPOS( wnd->KeyBuff[ keyBuffPtr ] ) )
      {
         wnd->KeyBuff[ keyBuffPtr ] = keyCode;
         return;
      }
   }

   if( wnd->keyBuffNO < XWC_CHAR_QUEUE_SIZE )
   {
      wnd->KeyBuff[ wnd->keyBuffPointer++ ] = keyCode;
      if( wnd->keyBuffPointer == XWC_CHAR_QUEUE_SIZE )
         wnd->keyBuffPointer = 0;
      wnd->keyBuffNO++;
   }
}

/* *********************************************************************** */

static ZH_BOOL zh_gt_xwc_GetCharFromInputQueue( PXWND_DEF wnd, int * keyCode )
{
   *keyCode = 0;
   if( wnd->keyBuffNO > 0 )
   {
      int keyBuffPtr = wnd->keyBuffPointer - wnd->keyBuffNO;
      if( keyBuffPtr < 0 )
         keyBuffPtr += XWC_CHAR_QUEUE_SIZE;
      *keyCode = wnd->KeyBuff[ keyBuffPtr ];
      wnd->keyBuffNO--;
      return ZH_TRUE;
   }
   return ZH_FALSE;
}

/* *********************************************************************** */

static int zh_gt_xwc_keyFlags( PXWND_DEF wnd, int flags )
{
   if( wnd->keyModifiers.bShift )
      flags |= ZH_KF_SHIFT;
   if( wnd->keyModifiers.bCtrl )
      flags |= ZH_KF_CTRL;
   if( wnd->keyModifiers.bAlt )
      flags |= ZH_KF_ALT;

   return flags;
}

/* *********************************************************************** */

static void zh_gt_xwc_FullScreen( PXWND_DEF wnd )
{
   XEvent evt;

   memset( &evt, 0, sizeof( evt ) );
   evt.xclient.type = ClientMessage;
   evt.xclient.message_type = s_atomState;
   evt.xclient.display = wnd->dpy;
   evt.xclient.window = wnd->window;
   evt.xclient.format = 32;
   evt.xclient.data.l[ 0 ] = wnd->fFullScreen ? 1 : 0;
   evt.xclient.data.l[ 1 ] = s_atomFullScreen;

   XSendEvent( wnd->dpy, DefaultRootWindow( wnd->dpy ), False,
               SubstructureRedirectMask, &evt );
}

static void zh_gt_xwc_MaximizeScreen( PXWND_DEF wnd )
{
   XEvent evt;

   memset( &evt, 0, sizeof( evt ) );
   evt.xclient.type = ClientMessage;
   evt.xclient.message_type = s_atomState;
   evt.xclient.display = wnd->dpy;
   evt.xclient.window = wnd->window;
   evt.xclient.format = 32;
   evt.xclient.data.l[ 0 ] = wnd->fMaximized ? 1 : 0;
   evt.xclient.data.l[ 1 ] = s_atomMaximizedX;
   evt.xclient.data.l[ 2 ] = s_atomMaximizedY;

   XSendEvent( wnd->dpy, DefaultRootWindow( wnd->dpy ), False,
               SubstructureRedirectMask, &evt );
}

/* after de-iconifying set input focus back */
static void zh_gt_xwc_ActivateScreen( PXWND_DEF wnd )
{
   XEvent evt;

   memset( &evt, 0, sizeof( evt ) );
   evt.xclient.type = ClientMessage;
   evt.xclient.message_type = s_atomActivate;
   evt.xclient.display = wnd->dpy;
   evt.xclient.window = wnd->window;
   evt.xclient.format = 32;
   evt.xclient.data.l[ 0 ] = 1;
   evt.xclient.data.l[ 1 ] = CurrentTime;
   evt.xclient.data.l[ 2 ] = wnd->window;

   XSendEvent( wnd->dpy, DefaultRootWindow( wnd->dpy ), False,
               SubstructureRedirectMask, &evt );
}

/* *********************************************************************** */

/* X11 Motif WM Properties and Resources */

#define MWM_HINTS_FUNCTIONS     (1L << 0)
#define MWM_FUNC_ALL            (1L << 0)
#define MWM_FUNC_RESIZE         (1L << 1)
#define MWM_FUNC_MOVE           (1L << 2)
#define MWM_FUNC_MINIMIZE       (1L << 3)
#define MWM_FUNC_MAXIMIZE       (1L << 4)
#define MWM_FUNC_CLOSE          (1L << 5)
#define MWM_FUNC_ALL_BITS       ( MWM_FUNC_RESIZE | MWM_FUNC_MOVE | MWM_FUNC_MINIMIZE | \
                                  MWM_FUNC_MAXIMIZE | MWM_FUNC_CLOSE )

#define MWM_HINTS_DECORATIONS   (1L << 1)
#define MWM_DECOR_ALL           (1L << 0)
#define MWM_DECOR_BORDER        (1L << 1)
#define MWM_DECOR_RESIZEH       (1L << 2)
#define MWM_DECOR_TITLE         (1L << 3)
#define MWM_DECOR_MENU          (1L << 4)
#define MWM_DECOR_MINIMIZE      (1L << 5)
#define MWM_DECOR_MAXIMIZE      (1L << 6)
#define MWM_DECOR_ALL_BITS      ( MWM_DECOR_BORDER | MWM_DECOR_RESIZEH | MWM_DECOR_TITLE | \
                                  MWM_DECOR_MENU | MWM_DECOR_MINIMIZE | MWM_DECOR_MAXIMIZE )

#define MWM_HINTS_INPUT_MODE    (1L << 2)
#define MWM_INPUT_MODELESS      0L
#define MWM_INPUT_MODAL         1L
#define MWM_INPUT_NONMODAL      3L

#define MWM_HINTS_ITEMS         5

typedef struct
{
   unsigned long  flags;
   unsigned long  functions;
   unsigned long  decorations;
   long           input_mode;
   unsigned long  status;
} XWC_MWMHints, * PXWC_MWMHints;

static void zh_gt_xwc_MotifWmHints( PXWND_DEF wnd )
{
   XWC_MWMHints mwmhints;
   Atom actual_type_return = 0;
   int actual_format_return = 0;
   unsigned long nitems_return = 0, bytes_after_return = 0;
   unsigned char * prop_return = NULL;
   unsigned long functions, decorations;
   int result;

   memset( &mwmhints, 0, sizeof( mwmhints ) );

   result = XGetWindowProperty( wnd->dpy, wnd->window, s_atomMotifHints,
                                0, 20, ZH_FALSE, s_atomMotifHints,
                                &actual_type_return, &actual_format_return,
                                &nitems_return, &bytes_after_return,
                                &prop_return );
   if( result == Success && actual_type_return == s_atomMotifHints &&
       actual_format_return == 32 && nitems_return >= MWM_HINTS_ITEMS )
   {
      PXWC_MWMHints pmwmhints = ( PXWC_MWMHints ) prop_return;

      mwmhints.flags       = pmwmhints->flags;
      mwmhints.functions   = pmwmhints->functions;
      mwmhints.decorations = pmwmhints->decorations;
      mwmhints.input_mode  = pmwmhints->input_mode;
      mwmhints.status      = pmwmhints->status;
   }

   if( prop_return )
      XFree( prop_return );

   if( mwmhints.functions == MWM_FUNC_ALL )
      mwmhints.functions = MWM_FUNC_ALL_BITS;

   if( mwmhints.decorations == MWM_DECOR_ALL )
      mwmhints.decorations = MWM_DECOR_ALL_BITS;

   functions   = mwmhints.functions;
   decorations = mwmhints.decorations;

   /* enable border */
   mwmhints.decorations |= MWM_DECOR_BORDER;
   /* if WindowTitleHint */
   mwmhints.decorations |= MWM_DECOR_TITLE;
   /* if WindowSystemMenuHint */
   mwmhints.decorations |= MWM_DECOR_MENU;
   /* if WindowMinimizeButtonHint */
   mwmhints.decorations |= MWM_DECOR_MINIMIZE;
   mwmhints.functions   |= MWM_FUNC_MINIMIZE;

   if( wnd->fResizable )
   {
      mwmhints.decorations |= MWM_DECOR_RESIZEH | MWM_DECOR_MAXIMIZE;
      mwmhints.functions   |= MWM_FUNC_MAXIMIZE | MWM_FUNC_RESIZE;
   }
   else
   {
      mwmhints.decorations &= ~( MWM_DECOR_RESIZEH | MWM_DECOR_MAXIMIZE );
      mwmhints.functions   &= ~( MWM_FUNC_MAXIMIZE | MWM_FUNC_RESIZE );
   }
   /* enable window moving */
   mwmhints.functions |= MWM_FUNC_MOVE;

   if( wnd->iCloseMode == 2 )
      mwmhints.functions &= ~MWM_FUNC_CLOSE;
   else
      mwmhints.functions |= MWM_FUNC_CLOSE;

   if( decorations != mwmhints.decorations )
      mwmhints.flags |= MWM_HINTS_DECORATIONS;
   if( functions != mwmhints.functions )
      mwmhints.flags |= MWM_HINTS_FUNCTIONS;

   XChangeProperty( wnd->dpy, wnd->window,
                    s_atomMotifHints, s_atomMotifHints, 32, PropModeReplace,
                    ( unsigned char * ) &mwmhints, MWM_HINTS_ITEMS );
}

/* *********************************************************************** */

/* update returned cords for NorthWestGravity */
static void zh_gt_xwc_UpdateWindowCords( PXWND_DEF wnd, int * pX, int * pY )
{
   if( ! wnd->fCordsInited )
   {
      Atom actual_type_return = 0;
      int actual_format_return = 0;
      unsigned long nitems_return = 0, bytes_after_return = 0;
      unsigned char * prop_return = NULL;

      if( XGetWindowProperty( wnd->dpy, wnd->window, s_atomFrameExtends,
                              0, 4, False, s_atomCardinal, &actual_type_return,
                              &actual_format_return, &nitems_return,
                              &bytes_after_return, &prop_return ) == Success )
      {
         if( prop_return )
         {
            if( actual_type_return == s_atomCardinal && nitems_return == 4 &&
                actual_format_return == 32 )
            {
               /* _NET_FRAME_EXTENTS: left, right, top, bottom, CARDINAL[4]/32 */
               long * fe = ( long * ) prop_return;

               wnd->iCordLeft = fe[ 0 ];
               wnd->iCordTop  = fe[ 2 ];
            }
            XFree( prop_return );
         }
      }
      wnd->fCordsInited = ZH_TRUE;
   }

   *pX -= wnd->iCordLeft;
   *pY -= wnd->iCordTop;
}

/* *********************************************************************** */

static void zh_gt_xwc_ProcessKey( PXWND_DEF wnd, XKeyEvent * evt )
{
   char buf[ 32 ];
   KeySym outISO = 0, out = XLookupKeysym( evt, 0 );
   int ikey = 0, flags = zh_gt_xwc_keyFlags( wnd, 0 ), i;
#ifdef X_HAVE_UTF8_STRING
   Status status_return = 0;
#endif


#ifdef XWC_DEBUG
#  ifdef X_HAVE_UTF8_STRING
   if( wnd->ic )
   {
      i = Xutf8LookupString( wnd->ic, evt, buf, ( int ) sizeof( buf ), &outISO, &status_return );
      buf[ ZH_MAX( i, 0 ) ] = '\0';
      printf( "UTF-8: KeySym=%lx, keySymISO=%lx, keystr[%d]='%s'\n", out, outISO, i, buf ); fflush( stdout );
   }
   else
#  endif
   {
      i = XLookupString( evt, buf, ( int ) sizeof( buf ), &outISO, NULL );
      buf[ ZH_MAX( i, 0 ) ] = '\0';
      printf( "KeySym=%lx, keySymISO=%lx, keystr[%d]='%s'\n", out, outISO, i, buf ); fflush( stdout );
   }
#endif

   /* First look for keys which should be processed before XLookupKeysym */
   switch( out )
   {
      /* First of all, let's scan for special codes */
      case XK_Shift_L:
      case XK_Shift_R:
         wnd->keyModifiers.bShift = ZH_TRUE;
         return;

      case XK_Control_L:
      case XK_Control_R:
         wnd->keyModifiers.bCtrl = ZH_TRUE;
         return;

      case XK_Meta_L:
      case XK_Alt_L:
         wnd->keyModifiers.bAlt = ZH_TRUE;
         return;

      case XK_Meta_R:
      case XK_Alt_R:
         wnd->keyModifiers.bAltGr = ZH_TRUE;
         return;

      /* Then we scan for movement */
      case XK_Left:
         ikey = ZH_KX_LEFT;
         break;
      case XK_Right:
         ikey = ZH_KX_RIGHT;
         break;
      case XK_Up:
         ikey = ZH_KX_UP;
         break;
      case XK_Down:
         ikey = ZH_KX_DOWN;
         break;
      /* case XK_Begin: case XK_KP_Begin: */
      case XK_Home:
         ikey = ZH_KX_HOME;
         break;
      case XK_End:
         ikey = ZH_KX_END;
         break;
      case XK_Page_Up:
         ikey = ZH_KX_PGUP;
         break;
      case XK_Page_Down:
         ikey = ZH_KX_PGDN;
         break;

      /* Special cursor operations */
      case XK_Delete:
         ikey = ZH_KX_DEL;
         break;
      case XK_Insert:
         ikey = ZH_KX_INS;
         break;
      case XK_BackSpace:
         ikey = ZH_KX_BS;
         break;
      case XK_Tab:
         ikey = ZH_KX_TAB;
         break;
      case XK_Linefeed:
      case XK_Return:
         if( wnd->keyModifiers.bAlt && wnd->fAltEnter )
         {
            wnd->fFullScreen = ! wnd->fFullScreen;
            zh_gt_xwc_FullScreen( wnd );
            return;
         }
         ikey = ZH_KX_ENTER;
         break;
      case XK_KP_Enter:
         ikey = ZH_KX_ENTER;
         flags |= ZH_KF_KEYPAD;
         break;
      case XK_Escape:
         ikey = ZH_KX_ESC;
         break;

      /* then we scan for function keys */
      case XK_F1:
         ikey = ZH_KX_F1;
         break;
      case XK_F2:
         ikey = ZH_KX_F2;
         break;
      case XK_F3:
         ikey = ZH_KX_F3;
         break;
      case XK_F4:
         ikey = ZH_KX_F4;
         break;
      case XK_F5:
         ikey = ZH_KX_F5;
         break;
      case XK_F6:
         ikey = ZH_KX_F6;
         break;
      case XK_F7:
         ikey = ZH_KX_F7;
         break;
      case XK_F8:
         ikey = ZH_KX_F8;
         break;
      case XK_F9:
         ikey = ZH_KX_F9;
         break;
      case XK_F10:
         ikey = ZH_KX_F10;
         break;
      case XK_F11:
         ikey = ZH_KX_F11;
         break;
      case XK_F12:
         ikey = ZH_KX_F12;
         break;

      /* Keys with special meanings to Cl*pper */
      case XK_Pause:
         ikey = ZH_KX_PAUSE;
         break;
      case XK_Print:
         ikey = ZH_KX_PRTSCR;
         break;

      case XK_Menu:
         ikey = ZH_K_MENU;
         break;
   }
   if( ikey )
   {
      zh_gt_xwc_AddCharToInputQueue( wnd, ZH_INKEY_NEW_KEY( ikey, flags ) );
      return;
   }

   /* look for keypad keys if they haven't been processed by XLookupString */
   flags |= ZH_KF_KEYPAD;
   switch( out )
   {
      case XK_KP_Left:
         ikey = ZH_KX_LEFT;
         break;
      case XK_KP_Right:
         ikey = ZH_KX_RIGHT;
         break;
      case XK_KP_Up:
         ikey = ZH_KX_UP;
         break;
      case XK_KP_Down:
         ikey = ZH_KX_DOWN;
         break;
      case XK_KP_Home:
         ikey = ZH_KX_HOME;
         break;
      case XK_KP_End:
         ikey = ZH_KX_END;
         break;
      case XK_KP_Page_Up:
         ikey = ZH_KX_PGUP;
         break;
      case XK_KP_Page_Down:
         ikey = ZH_KX_PGDN;
         break;
      case XK_KP_Begin:
      case XK_KP_5:
         ikey = ZH_KX_CENTER;
         break;
      case XK_KP_Insert:
         ikey = ZH_KX_INS;
         break;
      case XK_KP_Delete:
         ikey = ZH_KX_DEL;
         break;
      case XK_KP_Enter:
         ikey = ZH_KX_ENTER;
         break;
      case XK_KP_Multiply:
         ikey = '*';
         break;
      case XK_KP_Add:
         ikey = '+';
         break;
      case XK_KP_Subtract:
         ikey = '-';
         break;
      case XK_KP_Divide:
         ikey = '/';
         break;
      default:
         flags ^= ZH_KF_KEYPAD;
         break;
   }

   /* First check if there is no string bound with with a key, because
      we not check all modifiers in all possible keyboards */
#ifdef X_HAVE_UTF8_STRING
   if( wnd->ic )
      i = Xutf8LookupString( wnd->ic, evt, buf, ( int ) sizeof( buf ), &outISO, &status_return );
   else
#endif
   {
      i = XLookupString( evt, buf, ( int ) sizeof( buf ), &outISO, NULL );
#ifndef ZH_XWC_USE_LOCALE
      if( i <= 0 )
      {
         /*
          * This is a temporary hack for Latin-x input see gt_SetKeyCP()
          */
         if( outISO >= 0x0100 && outISO <= 0x0fff && ( outISO & 0x80 ) == 0x80 )
         {
            buf[ 0 ] = ( char ) ( outISO & 0xff );
            i = 1;
         }
         /* hack for euro sign */
         else if( outISO == 0x20ac )
         {
            ikey = zh_cdpGetChar( ZH_GTSELF_HOSTCP( wnd->pGT ), ( ZH_WCHAR ) outISO );
            if( ikey )
               zh_gt_xwc_AddCharToInputQueue( wnd, ZH_INKEY_NEW_CHAR( ikey ) );
            return;
         }
      }
#endif
   }

   if( i > 0 )
   {
      PZH_CODEPAGE cdp = wnd->fUTF8key ? wnd->utf8CDP : ZH_GTSELF_INCP( wnd->pGT );
      ZH_WCHAR wc;
      ZH_SIZE nI = 0;

      while( ZH_CODEPAGE_CHAR_GET( cdp, buf, i, &nI, &wc ) )
      {
         if( wc < 32 ||
             ( wc < 128 && ( flags & ( ZH_KF_CTRL | ZH_KF_ALT | ZH_KF_KEYPAD ) ) ) )
         {
            int fl = flags;
            if( wc > 0 && wc < 32 )
            {
               wc += 'A' - 1;
               fl |= ZH_KF_CTRL;
            }
            zh_gt_xwc_AddCharToInputQueue( wnd, ZH_INKEY_NEW_KEY( wc, fl ) );
         }
         else
            zh_gt_xwc_AddCharToInputQueue( wnd, ZH_INKEY_NEW_UNICODEF( wc, flags ) );
      }
      return;
   }

   if( ikey )
      zh_gt_xwc_AddCharToInputQueue( wnd, ZH_INKEY_NEW_KEY( ikey, flags ) );
}

/* *********************************************************************** */

static void zh_gt_xwc_WndProc( PXWND_DEF wnd, XEvent * evt )
{
   KeySym out;

#ifdef XWC_DEBUG
   if( wnd->window != evt->xany.window )
   {
      printf( "Event: #%d window=%ld (wnd->window=%ld)\n", evt->type, evt->xany.window, wnd->window ); fflush( stdout );
   }
#endif

   switch( evt->type )
   {
      case Expose:
#ifdef XWC_DEBUG
         printf( "Event: Expose\n" ); fflush( stdout );
#endif
         zh_gt_xwc_InvalidatePts( wnd,
                                  evt->xexpose.x, evt->xexpose.y,
                                  evt->xexpose.x + evt->xexpose.width,
                                  evt->xexpose.y + evt->xexpose.height );
         break;

      case NoExpose:
#ifdef XWC_DEBUG
         printf( "Event: NoExpose\n" ); fflush( stdout );
#endif
         break;

      case KeyPress:
#ifdef XWC_DEBUG
         printf( "Event: KeyPress\n" ); fflush( stdout );
#endif
         if( evt->xkey.time != CurrentTime )
            wnd->lastEventTime = evt->xkey.time;
         zh_gt_xwc_ProcessKey( wnd, &evt->xkey );
         break;

      case KeyRelease:
#ifdef XWC_DEBUG
         printf( "Event: KeyRelease\n" ); fflush( stdout );
#endif
         if( evt->xkey.time != CurrentTime )
            wnd->lastEventTime = evt->xkey.time;
         out = XLookupKeysym( &evt->xkey, 0 );
         switch( out )
         {
            case XK_Shift_L:
            case XK_Shift_R:
               wnd->keyModifiers.bShift = ZH_FALSE;
               break;

            case XK_Control_L:
            case XK_Control_R:
               wnd->keyModifiers.bCtrl = ZH_FALSE;
               break;

            case XK_Meta_L:
            case XK_Alt_L:
               wnd->keyModifiers.bAlt = ZH_FALSE;
               break;

            case XK_Meta_R:
            case XK_Alt_R:
               wnd->keyModifiers.bAltGr = ZH_FALSE;
               break;
         }
         break;

      case MotionNotify:
#ifdef XWC_DEBUG
         printf( "Event: MotionNotify\n" ); fflush( stdout );
#endif
         if( evt->xmotion.time != CurrentTime )
            wnd->lastEventTime = evt->xmotion.time;

         wnd->mouseColPxl = evt->xmotion.x;
         wnd->mouseRowPxl = evt->xmotion.y;
         wnd->mouseCol = wnd->mouseColPxl / wnd->fontWidth;
         wnd->mouseRow = wnd->mouseRowPxl / wnd->fontHeight;
         if( wnd->fMarkMode )
         {
            zh_gt_xwc_InvalidateChar( wnd, wnd->markLeft, wnd->markTop,
                                           wnd->markRight, wnd->markBottom );
            if( wnd->iMarkCol < wnd->mouseCol )
            {
               wnd->markLeft = wnd->iMarkCol;
               wnd->markRight = wnd->mouseCol;
            }
            else
            {
               wnd->markLeft = wnd->mouseCol;
               wnd->markRight = wnd->iMarkCol;
            }
            if( wnd->iMarkRow < wnd->mouseRow )
            {
               wnd->markTop = wnd->iMarkRow;
               wnd->markBottom = wnd->mouseRow;
            }
            else
            {
               wnd->markTop = wnd->mouseRow;
               wnd->markBottom = wnd->iMarkRow;
            }
            zh_gt_xwc_InvalidateChar( wnd, wnd->markLeft, wnd->markTop,
                                           wnd->markRight, wnd->markBottom );
         }
         else
            zh_gt_xwc_AddCharToInputQueue( wnd,
                           ZH_INKEY_NEW_MPOS( wnd->mouseCol, wnd->mouseRow ) );
         break;

      case ButtonPress:
      case ButtonRelease:
      {
         int button = evt->xbutton.button - 1;

#ifdef XWC_DEBUG
         printf( "Event: %s, button=%d\n", evt->type == ButtonPress ? "ButtonPress" : "ButtonRelease", button ); fflush( stdout );
#endif
         if( evt->xbutton.time != CurrentTime )
            wnd->lastEventTime = evt->xbutton.time;
         if( button >= 0 && button < XWC_MAX_BUTTONS )
         {
            button = wnd->mouseButtonsMap[ button ] - 1;
         }
         if( button >= 0 && button < wnd->mouseNumButtons )
         {
            int key = 0;

            if( evt->type == ButtonPress )
            {
               if( wnd->keyModifiers.bShift && button == 0 &&
                   wnd->fSelectCopy && ! wnd->fMarkMode )
               {
                  wnd->fMarkMode = ZH_TRUE;
                  wnd->iMarkCol = wnd->mouseCol;
                  wnd->iMarkRow = wnd->mouseRow;
               }
               else
               {
                  Time evtTime = ( ( XButtonEvent * ) evt )->time;
                  if( evtTime - wnd->mouseButtonsTime[ button ] <
                      ( Time ) ZH_GTSELF_MOUSEGETDOUBLECLICKSPEED( wnd->pGT ) )
                     key = s_mouseDblPressKeys[ button ];
                  else
                     key = s_mousePressKeys[ button ];
                  wnd->mouseButtonsState |= s_mouseButtonBits[ button ];
                  wnd->mouseButtonsTime[ button ] = evtTime;
               }
            }
            else if( wnd->fMarkMode && button == 0 )
            {
               int top = wnd->markTop, bottom = wnd->markBottom,
                   left = wnd->markLeft, right = wnd->markRight;
               char * pBuffer;
               ZH_SIZE nSize, nI;

               wnd->fMarkMode = ZH_FALSE;
               zh_gt_xwc_InvalidateChar( wnd, left, top, right, bottom );

               nSize = ( bottom - top + 1 ) * ( right - left + 2 ) * 3;
               pBuffer = ( char * ) zh_xgrab( nSize + 1 );
               nI = 0;
               while( top <= bottom )
               {
                  for( left = wnd->markLeft; left <= right; ++left )
                  {
                     int iColor;
                     ZH_BYTE bAttr;
                     ZH_USHORT usChar;

                     if( ! ZH_GTSELF_GETSCRCHAR( wnd->pGT, top, left, &iColor, &bAttr, &usChar ) )
                        break;

                     nI += zh_cdpTextPutU16( wnd->utf8CDP, pBuffer + nI, nSize - nI, usChar );
                     #if 0
                     nI += zh_cdpU16CharToUTF8( pBuffer + nI, &usChar );
                     #endif
                  }
                  if( wnd->markTop < wnd->markBottom )
                     pBuffer[ nI++ ] = '\n';
                  ++top;
               }
               if( nI > 0 )
               {
                  pBuffer[ nI ] = '\0';
                  zh_gt_xwc_SetSelection( wnd, pBuffer, nI, ZH_FALSE );
               }
               else
                  zh_xfree( pBuffer );
            }
            else
            {
               key = s_mouseReleaseKeys[ button ];
               wnd->mouseButtonsState &= ~s_mouseButtonBits[ button ];
            }
            if( key != 0 )
            {
               zh_gt_xwc_AddCharToInputQueue( wnd,
                     ZH_INKEY_NEW_MKEY( key, zh_gt_xwc_keyFlags( wnd, 0 ) ) );
            }
         }
         break;
      }

      case CreateNotify:
#ifdef XWC_DEBUG
         printf( "Event: CreateNotify\n" ); fflush( stdout );
#endif
         wnd->iNewPosX = evt->xcreatewindow.x;
         wnd->iNewPosY = evt->xcreatewindow.y;
         break;

      case MappingNotify:
#ifdef XWC_DEBUG
         printf( "Event: MappingNotify\n" ); fflush( stdout );
#endif
         XRefreshKeyboardMapping( &evt->xmapping );
         break;

      case FocusIn:
#ifdef XWC_DEBUG
         printf( "Event: FocusIn\n" ); fflush( stdout );
#endif
         XRefreshKeyboardMapping( &evt->xmapping );
         wnd->keyModifiers.bCtrl  =
         wnd->keyModifiers.bAlt   =
         wnd->keyModifiers.bAltGr =
         wnd->keyModifiers.bShift = ZH_FALSE;
         zh_gt_xwc_AddCharToInputQueue( wnd, ZH_K_GOTFOCUS );
         break;

      case FocusOut:
#ifdef XWC_DEBUG
         printf( "Event: FocusOut\n" ); fflush( stdout );
#endif
         zh_gt_xwc_AddCharToInputQueue( wnd, ZH_K_LOSTFOCUS );
         break;

      case ConfigureNotify:
#ifdef XWC_DEBUG
         printf( "Event: ConfigureNotify (x=%d, y=%d, w=%d, h=%d, or=%d)\n",
                 evt->xconfigure.x, evt->xconfigure.y,
                 evt->xconfigure.width, evt->xconfigure.height,
                 evt->xconfigure.override_redirect ); fflush( stdout );
#endif
         wnd->iNewPosX   = evt->xconfigure.x;
         wnd->iNewPosY   = evt->xconfigure.y;
         wnd->newWidth   = evt->xconfigure.width;
         wnd->newHeight  = evt->xconfigure.height;
         wnd->fWinResize = ZH_TRUE;
         break;

      case ClientMessage:
#ifdef XWC_DEBUG
         printf( "Event: ClientMessage:%ld (%s)\n", evt->xclient.data.l[ 0 ], XGetAtomName( wnd->dpy, ( Atom ) evt->xclient.data.l[ 0 ] ) ); fflush( stdout );
#endif
         if( ( Atom ) evt->xclient.data.l[ 0 ] == s_atomDelWin )
         {
            if( wnd->iCloseMode == 0 )
               zh_vmRequestQuit();
            else
               zh_gt_xwc_AddCharToInputQueue( wnd, ZH_K_CLOSE );
         }
         break;

      case SelectionNotify:
      {
         Atom aNextRequest = None;
#ifdef XWC_DEBUG
         printf( "Event: SelectionNotify: selection=%ld (%s), property=%ld (%s), target=%ld (%s) => %ld (%s)\n",
                     evt->xselection.selection,
                     evt->xselection.selection == None ? "None" : XGetAtomName(wnd->dpy, evt->xselection.selection),
                     evt->xselection.property,
                     evt->xselection.property == None ? "None" : XGetAtomName(wnd->dpy, evt->xselection.property),
                     evt->xselection.target,
                     evt->xselection.target == None ? "None" : XGetAtomName(wnd->dpy, evt->xselection.target),
                     wnd->ClipboardRequest,
                     wnd->ClipboardRequest == None ? "None" : XGetAtomName(wnd->dpy, wnd->ClipboardRequest) ); fflush(stdout);
#endif
         if( evt->xselection.property != None )
         {
            XTextProperty text;

            if( XGetTextProperty( wnd->dpy, wnd->window, &text,
                                  evt->xselection.property ) != 0 )
            {
#ifdef XWC_DEBUG
               printf( "xselection.target.target=%ld (%s), TextProperty.format='%d'\n",
                       evt->xselection.target,
                       evt->xselection.target == None ? "None" : XGetAtomName( wnd->dpy, evt->xselection.target ),
                       text.format ); fflush( stdout );
#endif
               if( evt->xselection.target == s_atomUTF8String && text.format == 8 )
               {
#ifdef XWC_DEBUG
                  printf( "UTF8String='%s'\n", text.value ); fflush( stdout );
#endif
                  if( wnd->ClipboardData != NULL )
                     zh_xfree( wnd->ClipboardData );

                  wnd->ClipboardSize = text.nitems;
                  wnd->ClipboardData = ( unsigned char * )
                                       zh_xmemdup( text.value, text.nitems + 1 );
                  wnd->ClipboardData[ wnd->ClipboardSize ] = '\0';
                  wnd->ClipboardTime = evt->xselection.time;
                  wnd->ClipboardRcvd = ZH_TRUE;
               }
               else if( evt->xselection.target == s_atomString && text.format == 8 )
               {
#ifdef XWC_DEBUG
                  printf( "String='%s'\n", text.value ); fflush( stdout );
#endif
                  if( wnd->ClipboardData != NULL )
                     zh_xfree( wnd->ClipboardData );

                  wnd->ClipboardSize = text.nitems;
                  wnd->ClipboardData = ( unsigned char * )
                     zh_cdpnDup( ( const char * ) text.value, &wnd->ClipboardSize,
                                 ZH_GTSELF_INCP( wnd->pGT ), wnd->utf8CDP );
                  wnd->ClipboardTime = evt->xselection.time;
                  wnd->ClipboardRcvd = ZH_TRUE;
               }
               else if( evt->xselection.target == s_atomTargets && text.format == 32 )
               {
                  Atom aValue;
                  unsigned long nItem;
#ifdef XWC_DEBUG
                  printf( "text.nitems=%lu, text.format=%d\n", text.nitems, text.format ); fflush( stdout );
#endif
                  for( nItem = 0; nItem < text.nitems; ++nItem )
                  {
                     aValue = ( Atom ) ( ( long * ) text.value )[ nItem ];
                     if( aValue == s_atomUTF8String )
                        aNextRequest = s_atomUTF8String;
                     else if( aValue == s_atomString && aNextRequest != s_atomUTF8String )
                        aNextRequest = s_atomString;
                     else if( aValue == s_atomText && aNextRequest == None )
                        aNextRequest = s_atomText;
#ifdef XWC_DEBUG
                     if( aValue )
                        printf( "%lu, %8lx (%s)\n", nItem, aValue, XGetAtomName( wnd->dpy, aValue ) );
                     else
                        printf( "%lu, %8lx (NULL)\n", nItem, aValue );
                     fflush( stdout );
#endif
                  }
               }
               if( text.value )
                  XFree( text.value );
            }
         }
         else if( wnd->ClipboardRequest == s_atomTargets )
            aNextRequest = s_atomUTF8String;
         else if( wnd->ClipboardRequest == s_atomUTF8String )
            aNextRequest = s_atomString;
         wnd->ClipboardRequest = aNextRequest;
         break;
      }

      case SelectionRequest:
      {
         XSelectionRequestEvent * req = &evt->xselectionrequest;
         XEvent respond;

#ifdef XWC_DEBUG
         printf( "Event: SelectionRequest: %ld (%s)\n", req->target,
                 XGetAtomName( wnd->dpy, req->target ) ); fflush( stdout );
#endif
         respond.xselection.property = req->property;

         if( req->target == s_atomTimestamp )
         {
            long timeStamp = wnd->ClipboardTime;
            XChangeProperty( wnd->dpy, req->requestor, req->property,
                             s_atomInteger, 32, PropModeReplace,
                             ( unsigned char * ) &timeStamp, 1 );
         }
         else if( req->target == s_atomTargets )
         {
            Atom aProp[] = { s_atomTimestamp, s_atomTargets,
                             s_atomString,    s_atomUTF8String,
                             s_atomText };
            XChangeProperty( wnd->dpy, req->requestor, req->property,
                             s_atomAtom, 32, PropModeReplace,
                             ( unsigned char * ) aProp, ZH_SIZEOFARRAY( aProp ) );
         }
         else if( req->target == s_atomString || req->target == s_atomText )
         {
            /* TODO: for s_atomString convert data to ISO-8859-1 */
            PZH_CODEPAGE cdpin = ZH_GTSELF_INCP( wnd->pGT );

            if( cdpin && cdpin != wnd->utf8CDP )
            {
               ZH_SIZE nLen = wnd->ClipboardSize;
               unsigned char * pBuffer = ( unsigned char * )
                     zh_cdpnDup( ( const char * ) wnd->ClipboardData, &nLen,
                                 wnd->utf8CDP, cdpin );

               XChangeProperty( wnd->dpy, req->requestor, req->property,
                                s_atomString, 8, PropModeReplace,
                                pBuffer, nLen );
               zh_xfree( pBuffer );
            }
            else
            {
               XChangeProperty( wnd->dpy, req->requestor, req->property,
                                s_atomString, 8, PropModeReplace,
                                wnd->ClipboardData, wnd->ClipboardSize );
            }
         }
         else if( req->target == s_atomUTF8String )
         {
            XChangeProperty( wnd->dpy, req->requestor, req->property,
                             s_atomUTF8String, 8, PropModeReplace,
                             wnd->ClipboardData, wnd->ClipboardSize );
         }
         else
         {
            respond.xselection.property = None;
         }

         respond.xselection.type = SelectionNotify;
         respond.xselection.display = req->display;
         respond.xselection.requestor = req->requestor;
         respond.xselection.selection = req->selection;
         respond.xselection.target = req->target;
         respond.xselection.time = req->time;

         XSendEvent( wnd->dpy, req->requestor, 0, 0, &respond );
         break;
      }

      case SelectionClear:
#ifdef XWC_DEBUG
         printf( "Event: SelectionClear\n" ); fflush( stdout );
#endif
         wnd->ClipboardOwner = ZH_FALSE;
         break;

      case PropertyNotify:
#ifdef XWC_DEBUG
         printf( "Event: PropertyNotify\n" ); fflush( stdout );
#endif
         if( evt->xproperty.time != CurrentTime )
            wnd->lastEventTime = evt->xproperty.time;
         break;

      case VisibilityNotify:
#ifdef XWC_DEBUG
         printf( "Event: VisibilityNotify\n" ); fflush( stdout );
#endif
         wnd->fCordsInited = ZH_FALSE;
         break;

      case GraphicsExpose:
#ifdef XWC_DEBUG
         printf( "Event: GraphicsExpose\n" ); fflush( stdout );
#endif
         wnd->fCordsInited = ZH_FALSE;
         break;

#ifdef XWC_DEBUG
      case GravityNotify:
         printf( "Event: GravityNotify (%d, %d)\n", evt->xgravity.x, evt->xgravity.y ); fflush( stdout );
         break;

      case ResizeRequest:
         printf( "Event: ResizeRequest\n" ); fflush( stdout );
         break;

      case KeymapNotify:
         printf( "Event: KeymapNotify\n" ); fflush( stdout );
         break;

      case EnterNotify:
         printf( "Event: EnterNotify\n" ); fflush( stdout );
         break;

      case LeaveNotify:
         printf( "Event: LeaveNotify\n" ); fflush( stdout );
         break;

      case DestroyNotify:
         printf( "Event: DestroyNotify\n" ); fflush( stdout );
         break;

      case UnmapNotify:
         printf( "Event: UnmapNotify\n" ); fflush( stdout );
         break;

      case MapNotify:
         printf( "Event: MapNotify\n" ); fflush( stdout );
         break;

      case MapRequest:
         printf( "Event: MapRequest\n" ); fflush( stdout );
         break;

      case ReparentNotify:
         printf( "Event: ReparentNotify\n" ); fflush( stdout );
         break;

      case ConfigureRequest:
         printf( "Event: ConfigureRequest\n" ); fflush( stdout );
         break;

      case CirculateNotify:
         printf( "Event: CirculateNotify\n" ); fflush( stdout );
         break;

      case CirculateRequest:
         printf( "Event: CirculateRequest\n" ); fflush( stdout );
         break;

      case ColormapNotify:
         printf( "Event: ColormapNotify\n" ); fflush( stdout );
         break;

      case GenericEvent:
         printf( "Event: GenericEvent\n" ); fflush( stdout );
         break;

      default:
         printf( "Event: #%d\n", evt->type ); fflush( stdout );
         break;
#endif
   }
}

/* *********************************************************************** */

/*
 * functions for drawing on the virtual screen (pixmap) and window updating
 */

/* *********************************************************************** */
/* color allocation */
static int zh_gt_xwc_GetColormapSize( PXWND_DEF wnd )
{
   XVisualInfo visInfo, *visInfoPtr;
   int iCMapSize = -1, nItems;

   visInfo.visualid = XVisualIDFromVisual( DefaultVisual( wnd->dpy,
                                           DefaultScreen( wnd->dpy ) ) );
   visInfoPtr = XGetVisualInfo( wnd->dpy, ( long ) VisualIDMask,
                                &visInfo, &nItems );
   if( nItems >= 1 )
   {
      iCMapSize = visInfoPtr->colormap_size;
   }
   XFree( ( char * ) visInfoPtr );

   return iCMapSize;
}

/* *********************************************************************** */

static ZH_BOOL zh_gt_xwc_AllocColor( PXWND_DEF wnd, XColor * pColor )
{
   ZH_BOOL fOK = ZH_FALSE;
   int iCMapSize;

   if( XAllocColor( wnd->dpy, wnd->colorsmap, pColor ) != 0 )
   {
      /* the exact color allocated */
      fOK = ZH_TRUE;
   }
   else if( ( iCMapSize = zh_gt_xwc_GetColormapSize( wnd ) ) > 0 )
   {
      /* try to find the best approximation of chosen color in
       * already allocated colors
       * Based on xterm "find_closest_color()" which was based on
       * Monish Shah's "find_closest_color()" for Vim 6.0, modified
       * with ideas from David Tong's "noflash" library ;-)
       */
      int     i, iClosestColor;
      double  dDiff, dDistance;
      XColor *colorTable;
      ZH_BYTE *checkTable;

      colorTable = ( XColor * ) zh_xgrab( iCMapSize * sizeof( XColor ) );
      checkTable = ( ZH_BYTE * ) zh_xgrab( iCMapSize * sizeof( ZH_BYTE ) );
      for( i = 0; i < iCMapSize; i++ )
      {
         colorTable[ i ].pixel = ( ZH_GT_PIXELTYPE ) i;
         checkTable[ i ]       = ZH_FALSE;
      }
      XQueryColors( wnd->dpy, wnd->colorsmap, colorTable, iCMapSize );

      do
      {
         double dClosestColorDist;

         iClosestColor = -1;
         /*
          * Look for the color that best approximates the desired one
          * and has not been checked so far and try to allocate it.
          * If allocation fails, it must mean that the color was read-write
          * (so we cannot use it, since its owner might change it) or else
          * it was already freed. Repeat until something succeeds or
          * we test all colors in given maximum of approximation.
          *
          * set the maximum for accepted approximation,
          * now we accept any valid color MAX_INT * MAX_INT * 3 < 1e20
          */
         dClosestColorDist = 1e20;
         for( i = 0; i < iCMapSize; i++ )
         {
            if( ! checkTable[ iClosestColor ] )
            {
               /*
                * Use Euclidean distance in RGB space, weighted by Y (of YIQ)
                * as the objective function, this accounts for differences
                * in the color sensitivity of the eye.
                */
               dDiff = 0.30 * ( ( ( int ) pColor->red   ) - ( int ) colorTable[ i ].red );
               dDistance = dDiff * dDiff;
               dDiff = 0.61 * ( ( ( int ) pColor->green ) - ( int ) colorTable[ i ].green );
               dDistance += dDiff * dDiff;
               dDiff = 0.11 * ( ( ( int ) pColor->blue  ) - ( int ) colorTable[ i ].blue );
               dDistance += dDiff * dDiff;
               if( dDistance < dClosestColorDist )
               {
                  iClosestColor = i;
                  dClosestColorDist = dDistance;
               }
            }
         }
         if( iClosestColor > 0 )
         {
            if( XAllocColor( wnd->dpy, wnd->colorsmap, &colorTable[ iClosestColor ] ) != 0 )
            {
               *pColor = colorTable[iClosestColor];
               fOK = ZH_TRUE;
               break;
            }
            checkTable[ iClosestColor ] = ZH_TRUE;
         }
      }
      while( iClosestColor > 0 );

      zh_xfree( colorTable );
      zh_xfree( checkTable );
   }

   return fOK;
}

/* *********************************************************************** */

static ZH_BOOL zh_gt_xwc_setPalette( PXWND_DEF wnd )
{
   char rgb_color[ 13 ];
   XColor color, dummy;
   ZH_BOOL fSet = ZH_FALSE;
   int i;

   /* Set standard colors */
   wnd->colorsmap = DefaultColormap( wnd->dpy, DefaultScreen( wnd->dpy ) );
   for( i = 0; i < 16; i++ )
   {
      if( ! wnd->colors[ i ].set )
      {
         if( wnd->colors[ i ].pixel )
            XFreeColors( wnd->dpy, wnd->colorsmap, &wnd->colors[ i ].pixel, 1, 0 );
         zh_snprintf( rgb_color, sizeof( rgb_color ),
                      "rgb:%02X/%02X/%02X",
                      ( wnd->colors[ i ].value ) & 0xFF,
                      ( wnd->colors[ i ].value >> 8 ) & 0xFF,
                      ( wnd->colors[ i ].value >> 16 ) & 0xFF );
         if( XLookupColor( wnd->dpy, wnd->colorsmap, rgb_color, &dummy, &color ) != 0 )
         {
            if( zh_gt_xwc_AllocColor( wnd, &color ) )
            {
               wnd->colors[ i ].pixel = color.pixel;
#ifdef XWC_DEBUG
               printf( "zh_gt_xwc_AllocColor[%d]='%x/%x/%x'\n", i, color.red, color.green, color.blue ); fflush( stdout );
#endif
            }
         }
         fSet = wnd->colors[ i ].set = ZH_TRUE;
      }
   }
   return fSet;
}

/* *********************************************************************** */

static void zh_gt_xwc_DrawString( PXWND_DEF wnd, int col, int row, ZH_BYTE color, ZH_USHORT * usChBuf, int len )
{
   if( wnd->fClearBkg )
   {
      XSetForeground( wnd->dpy, wnd->gc, wnd->colors[ color >> 4 ].pixel );
      XFillRectangle( wnd->dpy, wnd->drw, wnd->gc,
                      col * wnd->fontWidth, row * wnd->fontHeight,
                      wnd->fontWidth * len, wnd->fontHeight );
      XSetForeground( wnd->dpy, wnd->gc, wnd->colors[ color & 0x0F ].pixel );
      XDrawString16( wnd->dpy, wnd->drw, wnd->gc,
                     col * wnd->fontWidth,
                     row * wnd->fontHeight + wnd->xfs->ascent,
                     ( XChar2b * ) usChBuf, len );
   }
   else
   {
      XSetBackground( wnd->dpy, wnd->gc, wnd->colors[ color >> 4 ].pixel );
      XSetForeground( wnd->dpy, wnd->gc, wnd->colors[ color & 0x0F ].pixel );
      XDrawImageString16( wnd->dpy, wnd->drw, wnd->gc,
                          col * wnd->fontWidth,
                          row * wnd->fontHeight + wnd->xfs->ascent,
                          ( XChar2b * ) usChBuf, len );
   }
}

/* *********************************************************************** */

static ZH_U32 zh_gt_xwc_HashCurrChar( ZH_BYTE attr, ZH_BYTE color, ZH_USHORT chr )
{
   return ( ( ZH_U32 ) attr << 24 ) | ( ( ZH_U32 ) color << 16 ) | ( ZH_U32 ) chr;
}

/* *********************************************************************** */

static void zh_gt_xwc_RepaintChar( PXWND_DEF wnd, int colStart, int rowStart, int colStop, int rowStop )
{
   ZH_USHORT irow, startCol = 0, basex, basey, nsize;
   ZH_BYTE oldColor = 0, color, attr;
   ZH_USHORT usCh16, usChBuf[ XWC_MAX_COLS ];
   ZH_U32 u32Curr = 0xFFFFFFFF;
   int i, iColor;
   XWC_CharTrans * chTrans;

#ifdef XWC_DEBUG
   printf( "Repaint(%d,%d,%d,%d)[%dx%d]\n", rowStart, colStart, rowStop, colStop, wnd->fontHeight, wnd->fontWidth ); fflush( stdout );
#endif

   if( rowStop >= wnd->rows )
      rowStop = wnd->rows - 1;
   if( colStop >= wnd->cols )
      colStop = wnd->cols - 1;
   if( colStart < 0 )
      colStart = 0;
   if( rowStart < 0 )
      rowStart = 0;

   for( irow = rowStart; irow <= rowStop; irow++ )
   {
      ZH_USHORT icol, len;
      int scridx;

      icol = colStart;
      scridx = icol + irow * wnd->cols;
      len = 0;
      /* attribute may change mid line...
       * so buffer up text with same attrib, and output it
       * then do next section with same attrib, etc
       */
      while( icol <= colStop )
      {
         if( ! ZH_GTSELF_GETSCRCHAR( wnd->pGT, irow, icol, &iColor, &attr, &usCh16 ) )
         {
            color = 0x07;
            attr = 0;
            usCh16 = ' ';
         }
         else
         {
            color = ( ZH_BYTE ) iColor;
            if( wnd->fMarkMode &&
                irow >= wnd->markTop && irow <= wnd->markBottom &&
                icol >= wnd->markLeft && icol <= wnd->markRight )
            {
               color = ( color << 4 ) | ( color >> 4 );
            }
         }
         u32Curr = zh_gt_xwc_HashCurrChar( attr, color, usCh16 );
         chTrans = zh_gt_xwc_GetBoxChar( wnd, usCh16 );
         if( chTrans->inverse )
         {
            color = ( color << 4 ) | ( color >> 4 );
         }
         if( len > 0 && ( chTrans->type != CH_CHAR ||
                          color != oldColor || u32Curr == wnd->pCurrScr[ scridx ] ) )
         {
            zh_gt_xwc_DrawString( wnd, startCol, irow, oldColor, usChBuf, len );
            len = 0;
         }
         if( wnd->pCurrScr[ scridx ] != u32Curr )
         {
            switch( chTrans->type )
            {
               case CH_CHAR:
                  if( wnd->fFixMetric )
                  {
                     ZH_PUT_BE_UINT16( &usChBuf[ 0 ], chTrans->u.ch16 );
                     zh_gt_xwc_DrawString( wnd, icol, irow, color, usChBuf, 1 );
                  }
                  else
                  {
                     if( len == 0 )
                     {
                        oldColor = color;
                        startCol = icol;
                     }
                     ZH_PUT_BE_UINT16( &usChBuf[ len ], chTrans->u.ch16 );
                     len++;
                  }
                  break;

               case CH_CHBX:
                  #if 0
                  zh_gt_xwc_DrawBoxChar( wnd, icol, irow, chTrans->u.ch16, color );
                  #endif
                  break;

               case CH_NONE:
                  XSetForeground( wnd->dpy, wnd->gc, wnd->colors[ color >> 4 ].pixel );
                  XFillRectangle( wnd->dpy, wnd->drw, wnd->gc,
                                  icol * wnd->fontWidth, irow * wnd->fontHeight,
                                  wnd->fontWidth, wnd->fontHeight );
                  break;

               case CH_IMG:
                  XSetBackground( wnd->dpy, wnd->gc, wnd->colors[ color >> 4 ].pixel );
                  XSetForeground( wnd->dpy, wnd->gc, wnd->colors[ color & 0x0F ].pixel );
                  XPutImage( wnd->dpy, wnd->drw, wnd->gc,
                             chTrans->u.img, 0, 0,
                             icol * wnd->fontWidth, irow * wnd->fontHeight,
                             wnd->fontWidth, wnd->fontHeight );
                  break;

               case CH_PTS:
                  /* we use CoordModePrevious so only first point position has to be updated */
                  chTrans->u.pts[ 0 ].x = ( chTrans->u.pts[ 0 ].x % wnd->fontWidth ) + icol * wnd->fontWidth;
                  chTrans->u.pts[ 0 ].y = ( chTrans->u.pts[ 0 ].y % wnd->fontHeight ) + irow * wnd->fontHeight;
                  XSetForeground( wnd->dpy, wnd->gc, wnd->colors[ color >> 4 ].pixel );
                  XFillRectangle( wnd->dpy, wnd->drw, wnd->gc,
                                  icol * wnd->fontWidth, irow * wnd->fontHeight,
                                  wnd->fontWidth, wnd->fontHeight );
                  XSetForeground( wnd->dpy, wnd->gc, wnd->colors[ color & 0x0F ].pixel );
                  XDrawPoints( wnd->dpy, wnd->drw, wnd->gc,
                               chTrans->u.pts,
                               chTrans->size, CoordModePrevious );
                  break;

               case CH_LINE:
                  /* we use CoordModePrevious so only first point position has to be updated */
                  chTrans->u.pts[ 0 ].x = ( chTrans->u.pts[ 0 ].x % wnd->fontWidth ) + icol * wnd->fontWidth;
                  chTrans->u.pts[ 0 ].y = ( chTrans->u.pts[ 0 ].y % wnd->fontHeight ) + irow * wnd->fontHeight;
                  XSetForeground( wnd->dpy, wnd->gc, wnd->colors[ color >> 4 ].pixel );
                  XFillRectangle( wnd->dpy, wnd->drw, wnd->gc,
                                  icol * wnd->fontWidth, irow * wnd->fontHeight,
                                  wnd->fontWidth, wnd->fontHeight );
                  XSetForeground( wnd->dpy, wnd->gc, wnd->colors[ color & 0x0F ].pixel );
                  XDrawLines( wnd->dpy, wnd->drw, wnd->gc,
                              chTrans->u.pts,
                              chTrans->size, CoordModePrevious );
                  break;

               case CH_POLY:
                  /* we use CoordModePrevious so only first point position has to be updated */
                  chTrans->u.pts[ 0 ].x = ( chTrans->u.pts[ 0 ].x % wnd->fontWidth ) + icol * wnd->fontWidth;
                  chTrans->u.pts[ 0 ].y = ( chTrans->u.pts[ 0 ].y % wnd->fontHeight ) + irow * wnd->fontHeight;
                  XSetForeground( wnd->dpy, wnd->gc, wnd->colors[ color >> 4 ].pixel );
                  XFillRectangle( wnd->dpy, wnd->drw, wnd->gc,
                                  icol * wnd->fontWidth, irow * wnd->fontHeight,
                                  wnd->fontWidth, wnd->fontHeight );
                  XSetForeground( wnd->dpy, wnd->gc, wnd->colors[ color & 0x0F ].pixel );
                  XFillPolygon( wnd->dpy, wnd->drw, wnd->gc,
                                chTrans->u.pts,
                                chTrans->size,
                                Convex, CoordModePrevious );
                  break;

               case CH_SEG:
                  basex = icol * wnd->fontWidth;
                  basey = irow * wnd->fontHeight;
                  nsize = chTrans->size;
                  for( i = 0; i < nsize; i++ )
                  {
                     chTrans->u.seg[ i ].x1 = ( chTrans->u.seg[ i ].x1 % wnd->fontWidth ) + basex;
                     chTrans->u.seg[ i ].y1 = ( chTrans->u.seg[ i ].y1 % wnd->fontHeight ) + basey;
                     chTrans->u.seg[ i ].x2 = ( chTrans->u.seg[ i ].x2 % wnd->fontWidth ) + basex;
                     chTrans->u.seg[ i ].y2 = ( chTrans->u.seg[ i ].y2 % wnd->fontHeight ) + basey;
                  }
                  XSetForeground( wnd->dpy, wnd->gc, wnd->colors[ color >> 4 ].pixel );
                  XFillRectangle( wnd->dpy, wnd->drw, wnd->gc,
                                  basex, basey, wnd->fontWidth, wnd->fontHeight );
                  XSetForeground( wnd->dpy, wnd->gc, wnd->colors[ color & 0x0F ].pixel );
                  XDrawSegments( wnd->dpy, wnd->drw, wnd->gc,
                                 chTrans->u.seg, nsize );
                  break;

               case CH_RECT:
                  basex = icol * wnd->fontWidth;
                  basey = irow * wnd->fontHeight;
                  nsize = chTrans->size;
                  for( i = 0; i < nsize; i++ )
                  {
                     chTrans->u.rect[ i ].x = ( chTrans->u.rect[ i ].x % wnd->fontWidth ) + basex;
                     chTrans->u.rect[ i ].y = ( chTrans->u.rect[ i ].y % wnd->fontHeight ) + basey;
                  }
                  XSetForeground( wnd->dpy, wnd->gc, wnd->colors[ color >> 4 ].pixel );
                  XFillRectangle( wnd->dpy, wnd->drw, wnd->gc,
                                  basex, basey, wnd->fontWidth, wnd->fontHeight );
                  XSetForeground( wnd->dpy, wnd->gc, wnd->colors[ color & 0x0F ].pixel );
                  XFillRectangles( wnd->dpy, wnd->drw, wnd->gc,
                                   chTrans->u.rect, nsize );
                  break;

               case CH_UNDEF:
                  break;

            }
            wnd->pCurrScr[ scridx ] = u32Curr;
         }
         icol++;
         scridx++;
      }
      if( len > 0 )
      {
         zh_gt_xwc_DrawString( wnd, startCol, irow, oldColor, usChBuf, len );
      }
   }
}

/* *********************************************************************** */

static void zh_gt_xwc_RestoreArea( PXWND_DEF wnd,
                                   int left, int top, int right, int bottom )
{
   XCopyArea( wnd->dpy, wnd->pm, wnd->window, wnd->gc,
              wnd->fontWidth * left, wnd->fontHeight * top,
              wnd->fontWidth * ( right - left + 1 ), wnd->fontHeight * ( bottom - top + 1 ),
              wnd->fontWidth * left, wnd->fontHeight * top );
}

/* *********************************************************************** */

static void zh_gt_xwc_InvalidateChar( PXWND_DEF wnd,
                                      int left, int top, int right, int bottom )
{
   if( ! wnd->fInvalidChr )
   {
      wnd->rInvalidChr.top    = top;
      wnd->rInvalidChr.left   = left;
      wnd->rInvalidChr.bottom = bottom;
      wnd->rInvalidChr.right  = right;
   }
   else
   {
      if( wnd->rInvalidChr.top    > top    ) wnd->rInvalidChr.top    = top;
      if( wnd->rInvalidChr.left   > left   ) wnd->rInvalidChr.left   = left;
      if( wnd->rInvalidChr.right  < right  ) wnd->rInvalidChr.right  = right;
      if( wnd->rInvalidChr.bottom < bottom ) wnd->rInvalidChr.bottom = bottom;
   }
   /*
    * It's a race condition in async update mode.
    * wnd->fInvalidChr has to be set _always_ after update region is defined
    * to make sure that screen will be updated (sometimes maybe twice but
    * it shouldn't hurt us)
    */
   wnd->fInvalidChr = ZH_TRUE;
}

static void zh_gt_xwc_InvalidateFull( PXWND_DEF wnd )
{
   ZH_SIZE nSize = wnd->cols * wnd->rows;

   while( nSize-- )
      wnd->pCurrScr[ nSize ] = 0xFFFFFFFF;

   zh_gt_xwc_InvalidateChar( wnd, 0, 0, wnd->cols - 1, wnd->rows - 1 );
}

static void zh_gt_xwc_InvalidatePart( PXWND_DEF wnd,
                                      int left, int top, int right, int bottom )
{
   int row, col;

   for( row = top; row <= bottom; row++ )
   {
      int scridx = row * wnd->cols + left;
      for( col = left; col <= right; col++, scridx++ )
         wnd->pCurrScr[ scridx ] = 0xFFFFFFFF;
   }

   zh_gt_xwc_InvalidateChar( wnd, left, top, right, bottom );
}

/* *********************************************************************** */

static void zh_gt_xwc_InvalidatePts( PXWND_DEF wnd,
                                     int left, int top, int right, int bottom )
{
   if( ! wnd->fInvalidPts )
   {
      wnd->rInvalidPts.top    = top;
      wnd->rInvalidPts.left   = left;
      wnd->rInvalidPts.bottom = bottom;
      wnd->rInvalidPts.right  = right;
   }
   else
   {
      if( wnd->rInvalidPts.top > top )
         wnd->rInvalidPts.top = top;
      if( wnd->rInvalidPts.left > left )
         wnd->rInvalidPts.left = left;
      if( wnd->rInvalidPts.right < right )
         wnd->rInvalidPts.right = right;
      if( wnd->rInvalidPts.bottom < bottom )
         wnd->rInvalidPts.bottom = bottom;
   }
   wnd->fInvalidPts = ZH_TRUE;
}

/* *********************************************************************** */

static void zh_gt_xwc_UpdateCursor( PXWND_DEF wnd )
{
   int cursorType = wnd->cursorState ? wnd->cursorType : SC_NONE;

   /* must the mouse cursor be positioned? */
   if( wnd->mouseGotoRow >= 0 && wnd->mouseGotoCol >= 0 )
   {
      XWarpPointer( wnd->dpy, None, wnd->window, 0, 0, 0, 0,
                    wnd->mouseGotoCol * wnd->fontWidth + ( wnd->fontWidth >> 1 ),
                    wnd->mouseGotoRow * wnd->fontHeight + ( wnd->fontHeight >> 1 ) );
      wnd->mouseGotoRow = -1;
   }

   /* must the screen cursor be repainted? */
   if( cursorType != wnd->lastCursorType ||
       wnd->lastCursorCol != wnd->col || wnd->lastCursorRow != wnd->row )
   {
      if( wnd->lastCursorType != SC_NONE )
      {
         /* restore character under previous cursor position */
         zh_gt_xwc_RestoreArea( wnd, wnd->lastCursorCol, wnd->lastCursorRow,
                                     wnd->lastCursorCol, wnd->lastCursorRow );
      }
      if( cursorType != SC_NONE )
      {
         ZH_USHORT basex = wnd->col * wnd->fontWidth,
                   basey = wnd->row * wnd->fontHeight,
                   size;

         switch( cursorType )
         {
            case SC_NORMAL:
               size = 2;
               basey += wnd->fontHeight - 3;
               break;
            case SC_INSERT:
               size = ( wnd->fontHeight - 2 ) >> 1;
               basey += wnd->fontHeight - size - 1;
               break;
            case SC_SPECIAL1:
               size = wnd->fontHeight - 2;
               basey += 1;
               break;
            case SC_SPECIAL2:
               size = ( wnd->fontHeight - 2 ) >> 1;
               basey += 1;
               break;
            default:
               size = 0;
               break;
         }
         if( size )
         {
            int color;
            ZH_BYTE attr;
            ZH_USHORT usChar;

            ZH_GTSELF_GETSCRCHAR( wnd->pGT, wnd->row, wnd->col, &color, &attr, &usChar );
            XSetForeground( wnd->dpy, wnd->gc, wnd->colors[color & 0x0f].pixel );
            XFillRectangle( wnd->dpy, wnd->window, wnd->gc,
                            basex, basey, wnd->fontWidth, size );
         }
      }
      wnd->lastCursorType = cursorType;
      wnd->lastCursorCol = wnd->col;
      wnd->lastCursorRow = wnd->row;
   }
}

/* *********************************************************************** */

static void zh_gt_xwc_UpdatePts( PXWND_DEF wnd )
{
   if( wnd->fInvalidPts )
   {
      int left, top, right, bottom;

      left   = wnd->rInvalidPts.left;
      top    = wnd->rInvalidPts.top;
      right  = wnd->rInvalidPts.right;
      bottom = wnd->rInvalidPts.bottom;
      wnd->fInvalidPts = ZH_FALSE;
      XCopyArea( wnd->dpy, wnd->pm, wnd->window, wnd->gc,
                 left, top, right - left + 1, bottom - top + 1, left, top );

      /* if we've just overwritten the cursor then set it last state as SC_NONE */
      if( wnd->lastCursorType != SC_NONE )
      {
         int col = wnd->lastCursorCol * wnd->fontWidth,
             row = wnd->lastCursorRow * wnd->fontHeight;
         if( left <= col && top <= row && right >= col && bottom >= row )
         {
            wnd->lastCursorType = SC_NONE;
         }
      }
   }
}

/* *********************************************************************** */

static void zh_gt_xwc_UpdateChr( PXWND_DEF wnd )
{
   if( wnd->fInvalidChr )
   {
      int left, top, right, bottom;

      left   = wnd->rInvalidChr.left;
      top    = wnd->rInvalidChr.top;
      right  = wnd->rInvalidChr.right;
      bottom = wnd->rInvalidChr.bottom;
      wnd->fInvalidChr = ZH_FALSE;

      zh_gt_xwc_RepaintChar( wnd, left, top, right, bottom );

      left   = wnd->fontWidth  * left;
      top    = wnd->fontHeight * top;
      right  = wnd->fontWidth  * ( right + 1 ) - 1;
      bottom = wnd->fontHeight * ( bottom + 1 ) - 1;

      zh_gt_xwc_InvalidatePts( wnd, left, top, right, bottom );
   }
}

/* *********************************************************************** */

static ZH_BOOL zh_gt_xwc_SetScrBuff( PXWND_DEF wnd, ZH_USHORT cols, ZH_USHORT rows )
{
   if( rows <= XWC_MAX_ROWS && cols <= XWC_MAX_COLS &&
       ( wnd->cols != cols || wnd->rows != rows || wnd->pCurrScr == NULL ) )
   {
      if( ZH_GTSELF_RESIZE( wnd->pGT, rows, cols ) )
      {
         wnd->cols = cols;
         wnd->rows = rows;

         if( wnd->pCurrScr != NULL )
            zh_xfree( wnd->pCurrScr );
         wnd->pCurrScr = ( ZH_U32 * ) zh_xgrab( cols * rows * sizeof( ZH_U32 ) );
         zh_gt_xwc_InvalidateFull( wnd );

         return ZH_TRUE;
      }
   }

   return ZH_FALSE;
}

/* *********************************************************************** */

static ZH_BOOL zh_gt_xwc_CreatePixmap( PXWND_DEF wnd )
{
   unsigned width, height;

   width  = wnd->cols * wnd->fontWidth;
   height = wnd->rows * wnd->fontHeight;

   if( ! wnd->pm || wnd->width != width || wnd->height != height )
   {
      if( wnd->pm )
         XFreePixmap( wnd->dpy, wnd->pm );

      wnd->pm = XCreatePixmap( wnd->dpy, wnd->window, width, height,
                               DefaultDepth( wnd->dpy, DefaultScreen( wnd->dpy ) ) );
      wnd->drw = wnd->pm;
      wnd->width = width;
      wnd->height = height;

      zh_gt_xwc_InvalidateFull( wnd );

      return ZH_TRUE;
   }
   return ZH_FALSE;
}

/* *********************************************************************** */

static void zh_gt_xwc_ResizeRequest( PXWND_DEF wnd, ZH_USHORT cols, ZH_USHORT rows )
{
   unsigned width, height;

   width  = cols * wnd->fontWidth;
   height = rows * wnd->fontHeight;

   XResizeWindow( wnd->dpy, wnd->window, width, height );
}

/* *********************************************************************** */

static void zh_gt_xwc_UpdateSize( PXWND_DEF wnd )
{
   if( wnd->fWinResize )
   {
      ZH_USHORT rows, cols;

      wnd->fWinResize = ZH_FALSE;

      rows = wnd->newHeight / wnd->fontHeight;
      if( rows > XWC_MAX_ROWS )
         rows = XWC_MAX_ROWS;
      cols = wnd->newWidth / wnd->fontWidth;
      if( cols > XWC_MAX_COLS )
         cols = XWC_MAX_COLS;

#ifdef XWC_DEBUG
      printf( "zh_gt_xwc_UpdateSize() %dx%d => %dx%d\n",
              wnd->rows, wnd->cols, rows, cols ); fflush( stdout );
#endif

      if( zh_gt_xwc_SetScrBuff( wnd, cols, rows ) )
      {
         zh_gt_xwc_CreatePixmap( wnd );
         zh_gt_xwc_AddCharToInputQueue( wnd, ZH_K_RESIZE );
      }
      if( ( wnd->width != wnd->newWidth || wnd->height != wnd->newHeight ) &&
          ( wnd->oldWidth != wnd->newWidth || wnd->oldHeight != wnd->newHeight ) &&
          ! wnd->fFullScreen )
      {
         wnd->fCordsInited = ZH_FALSE;
         wnd->oldWidth = wnd->newWidth;
         wnd->oldHeight = wnd->newHeight;
         XResizeWindow( wnd->dpy, wnd->window, wnd->width, wnd->height );
      }
   }
}

/* *********************************************************************** */

static ZH_ULONG zh_gt_xwc_CurrentTime( void )
{
   struct timeval tv;

   gettimeofday( &tv, NULL );
   return tv.tv_sec * 1000 + tv.tv_usec / 1000;
}

/* *********************************************************************** */

static void zh_gt_xwc_SetTitle( PXWND_DEF wnd, const char * szTitle )
{
   if( szTitle && * szTitle )
   {
      XTextProperty text;
      char * pBuffer;

      pBuffer = zh_cdpDup( szTitle, ZH_GTSELF_HOSTCP( wnd->pGT ), wnd->utf8CDP );
      text.value = ( unsigned char * ) pBuffer;
      text.encoding = s_atomUTF8String;
      text.format = 8;
      text.nitems = strlen( pBuffer );
      XSetWMName( wnd->dpy, wnd->window, &text );
      zh_xfree( pBuffer );
   }
   else
      XStoreName( wnd->dpy, wnd->window, "" );
}

/* *********************************************************************** */

static void zh_gt_xwc_ProcessMessages( PXWND_DEF wnd, ZH_BOOL fSync )
{
   if( wnd->cursorType != SC_NONE )
   {
      if( wnd->cursorBlinkRate == 0 )
      {
         wnd->cursorState = ZH_TRUE;
      }
      else
      {
         ZH_ULONG ulCurrentTime = zh_gt_xwc_CurrentTime();

         if( ulCurrentTime - wnd->cursorStateTime > wnd->cursorBlinkRate >> 1 )
         {
            wnd->cursorState = ! wnd->cursorState;
            wnd->cursorStateTime = ulCurrentTime;
         }
      }
   }

   ZH_XWC_XLIB_LOCK( wnd->dpy );

   zh_gt_xwc_UpdateChr( wnd );

   if( wnd->fDspTitle )
   {
      wnd->fDspTitle = ZH_FALSE;
      zh_gt_xwc_SetTitle( wnd, wnd->szTitle );
   }

   for( ;; )
   {
      const int event_types[] = { 0, ClientMessage, MappingNotify,
                                  SelectionClear, SelectionNotify, SelectionRequest };
      ZH_BOOL fRepeat = ZH_FALSE;
      XEvent evt;
      int i;

      zh_gt_xwc_UpdateSize( wnd );
      zh_gt_xwc_UpdatePts( wnd );
      zh_gt_xwc_UpdateCursor( wnd );

      if( fSync )
         XSync( wnd->dpy, False );

      for( i = 0; i < ( int ) ZH_SIZEOFARRAY( event_types ); ++i )
      {
         if( event_types[ i ] == 0 ?
             XCheckWindowEvent( wnd->dpy, wnd->window, XWC_STD_MASK, &evt ) :
             XCheckTypedWindowEvent( wnd->dpy, wnd->window, event_types[ i ], &evt ) )
         {
            zh_gt_xwc_WndProc( wnd, &evt );
            fRepeat = ZH_TRUE;
         }
      }

      if( !fRepeat )
         break;
   }

   ZH_XWC_XLIB_UNLOCK( wnd->dpy );

}

static ZH_BOOL zh_gt_xwc_SetFont( PXWND_DEF wnd, const char * fontFace,
                                  int weight, int size, const char * encoding )
{
   char fontString[ 250 ];
   XFontStruct * xfs;

   if( weight || size )
   {
      const char * szWeight;

      switch( weight )
      {
         case ZH_GTI_FONTW_BOLD:
            szWeight = "bold";
            break;
         case ZH_GTI_FONTW_THIN:
         case ZH_GTI_FONTW_NORMAL:
         default:
            szWeight = "medium";
            break;
      }
/*
      "-*-%s-%s-r-normal-*-%d-*-*-*-*-*-%s"
 */
      zh_snprintf( fontString, sizeof( fontString ),
                   "-*-%s-%s-r-*-*-%d-*-*-*-*-*-%s",
                   fontFace, szWeight, size, encoding == NULL ? "*-*" : encoding );
   }
   else
      zh_strncpy( fontString, fontFace, sizeof( fontString ) - 1 );

   xfs = XLoadQueryFont( wnd->dpy, fontString );

   if( xfs == NULL )
      return ZH_FALSE;

   if( wnd->szFontSel )
      zh_xfree( wnd->szFontSel );
   wnd->szFontSel = zh_strdup( fontString );

   /* a shortcut for window height and width */
   wnd->fontHeight = xfs->max_bounds.ascent + xfs->max_bounds.descent;
   #if 0
   wnd->fontWidth = xfs->max_bounds.rbearing - xfs->min_bounds.lbearing;
   #endif
   wnd->fontWidth = xfs->max_bounds.width;

   if( wnd->xfs )
      XFreeFont( wnd->dpy, wnd->xfs );
   wnd->xfs = xfs;

   return ZH_TRUE;
}

/* *********************************************************************** */

static void zh_gt_xwc_ClearSelection( PXWND_DEF wnd )
{
   if( wnd->ClipboardOwner )
   {
      XSetSelectionOwner( wnd->dpy, s_atomPrimary, None, wnd->ClipboardTime );
      XSetSelectionOwner( wnd->dpy, s_atomClipboard, None, wnd->ClipboardTime );
      wnd->ClipboardOwner = ZH_FALSE;
   }
}

/* *********************************************************************** */

static void zh_gt_xwc_SetSelection( PXWND_DEF wnd, const char * szData, ZH_SIZE nSize, ZH_BOOL fCopy )
{
   ZH_XWC_XLIB_LOCK( wnd->dpy );

   if( nSize == 0 )
      zh_gt_xwc_ClearSelection( wnd );

   if( wnd->ClipboardData != NULL )
   {
      zh_xfree( wnd->ClipboardData );
      wnd->ClipboardData = NULL;
   }

   wnd->ClipboardSize = nSize;
   wnd->ClipboardTime = wnd->lastEventTime;
   wnd->ClipboardOwner = ZH_FALSE;

   if( nSize > 0 )
   {
      if( fCopy )
      {
         wnd->ClipboardData = ( unsigned char * ) zh_xgrab( nSize + 1 );
         memcpy( wnd->ClipboardData, szData, nSize );
         wnd->ClipboardData[ nSize ] = '\0';
      }
      else
         wnd->ClipboardData = ( unsigned char * ) ZH_UNCONST( szData );

      XSetSelectionOwner( wnd->dpy, s_atomPrimary, wnd->window, wnd->ClipboardTime );
      if( XGetSelectionOwner( wnd->dpy, s_atomPrimary ) == wnd->window )
      {
         wnd->ClipboardOwner = ZH_TRUE;
         XSetSelectionOwner( wnd->dpy, s_atomClipboard, wnd->window, wnd->ClipboardTime );
      }
      else
      {
         const char * cMsg = "Cannot set primary selection\n";
         zh_gt_xwc_ClearSelection( wnd );
         ZH_GTSELF_OUTERR( wnd->pGT, cMsg, strlen( cMsg ) );
      }
   }

   ZH_XWC_XLIB_UNLOCK( wnd->dpy );
}

/* *********************************************************************** */

static void zh_gt_xwc_RequestSelection( PXWND_DEF wnd )
{
   if( ! wnd->ClipboardOwner )
   {
      Atom aRequest;
      ZH_ULONG ulCurrentTime = zh_gt_xwc_CurrentTime();
      int iConnFD = ConnectionNumber( wnd->dpy );

      wnd->ClipboardRcvd = ZH_FALSE;
      wnd->ClipboardRequest = s_atomTargets;
      aRequest = None;

      if( s_updateMode == XWC_ASYNC_UPDATE )
         s_iUpdateCounter = 150;

      do
      {
         if( aRequest != wnd->ClipboardRequest )
         {
            aRequest = wnd->ClipboardRequest;
            if( aRequest == None )
               break;

            ZH_XWC_XLIB_LOCK( wnd->dpy );

#ifdef XWC_DEBUG
            printf( "XConvertSelection: %ld (%s)\n", aRequest,
                    XGetAtomName( wnd->dpy, aRequest ) ); fflush( stdout );
#endif
            XConvertSelection( wnd->dpy, s_atomPrimary, aRequest,
                               s_atomCutBuffer0, wnd->window, wnd->lastEventTime );

            ZH_XWC_XLIB_UNLOCK( wnd->dpy );
         }

         if( s_updateMode == XWC_ASYNC_UPDATE )
         {
            if( s_iUpdateCounter == 0 )
               break;
            sleep( 1 );
         }
         else
         {
            zh_gt_xwc_ProcessMessages( wnd, ZH_TRUE );
            if( ! wnd->ClipboardRcvd && wnd->ClipboardRequest == aRequest )
            {
               ZH_ULONG ulTime = zh_gt_xwc_CurrentTime() - ulCurrentTime;

               if( ulTime > 3000 || zh_fsCanRead( iConnFD, 3000 - ulTime ) <= 0 )
                  break;
            }
         }
      }
      while( ! wnd->ClipboardRcvd && wnd->ClipboardRequest != None );

      wnd->ClipboardRequest = None;
   }
}

/* *********************************************************************** */

#ifdef ZH_XWC_USE_LOCALE
static ZH_BOOL zh_gt_xwc_isUTF8( void )
{
   ZH_BOOL fUTF8 = ZH_FALSE;
   const char * szLang = setlocale( LC_CTYPE, NULL );

   if( szLang )
   {
      int i = ( int ) strlen( szLang );

      if( i > 5 )
      {
         fUTF8 = zh_stricmp( szLang + i - 5, ".UTF8" ) ||
                 zh_stricmp( szLang + i - 6, ".UTF-8" );
      }
   }

   return fUTF8;

}
#endif

/* *********************************************************************** */

static PXWND_DEF zh_gt_xwc_CreateWndDef( PZH_GT pGT )
{
   PXWND_DEF wnd = ( PXWND_DEF ) zh_xgrabz( sizeof( XWND_DEF ) );
   int i;

   wnd->pGT = pGT;
   wnd->dpy = NULL;
   wnd->fInit = wnd->fData = ZH_FALSE;
   zh_gt_xwc_SetScrBuff( wnd, XWC_DEFAULT_COLS, XWC_DEFAULT_ROWS );
   wnd->iNewPosX = wnd->iNewPosY = -1;
   wnd->iCloseMode = 0;
   wnd->fResizable = ZH_TRUE;
   wnd->fWinResize = ZH_FALSE;
   wnd->fFullScreen = ZH_FALSE;
   wnd->fMaximized = ZH_FALSE;
   wnd->fMinimized = ZH_FALSE;
   wnd->fAltEnter = ZH_FALSE;
#if defined( ZH_XWC_USE_LOCALE )
   wnd->fUTF8key = zh_gt_xwc_isUTF8();
#endif
   wnd->utf8CDP = zh_cdpFindExt( "UTF8" );
   wnd->cursorType = SC_NORMAL;

   /* Window Title */
   wnd->szTitle = zh_cmdargBaseProgName();
   wnd->fDspTitle = ZH_TRUE;

   /* Font parameters */
   wnd->fontHeight = XWC_DEFAULT_FONT_HEIGHT;
   wnd->fontWidth = XWC_DEFAULT_FONT_WIDTH;
   wnd->fontWeight = XWC_DEFAULT_FONT_WEIGHT;
   wnd->szFontName = zh_strdup( XWC_DEFAULT_FONT_NAME );
   wnd->szFontEncoding = zh_strdup( XWC_DEFAULT_FONT_ENCODING );
   /* set GTXWC extension for chosen font */
   wnd->fFixMetric = XWC_DEFAULT_FONT_FIXMETRIC;
   wnd->fClearBkg = XWC_DEFAULT_FONT_CLRBKG;
   wnd->fDrawBox = XWC_DEFAULT_FONT_DRAWBOX;

   /* Clear keyboard buffer */
   wnd->keyBuffNO = 0;
   wnd->keyBuffPointer = 0;
   wnd->keyModifiers.bCtrl  = ZH_FALSE;
   wnd->keyModifiers.bAlt   = ZH_FALSE;
   wnd->keyModifiers.bAltGr = ZH_FALSE;
   wnd->keyModifiers.bShift = ZH_FALSE;

   for( i = 0; i < 16; i++ )
      wnd->colors[ i ].value = s_rgb_values[ i ];

   wnd->lastEventTime = CurrentTime;

   return wnd;
}

/* *********************************************************************** */

static ZH_BOOL zh_gt_xwc_ConnectX( PXWND_DEF wnd, ZH_BOOL fExit )
{
   if( wnd->dpy != NULL )
      return ZH_TRUE;

   /* with NULL, it gets the DISPLAY environment variable. */
   wnd->dpy = XOpenDisplay( NULL );

   if( wnd->dpy == NULL )
   {
      if( fExit )
      {
         /* TODO: a standard Ziher error should be generated here when
                  it can run without console!
         zh_errRT_TERM( EG_CREATE, 10001, NULL, "Could not connect to X server", 0, 0 );
         */
         s_fNoXServer = ZH_TRUE;
         zh_errInternal( 10001, "Could not connect to X server.", NULL, NULL );
      }
      return ZH_FALSE;
   }

   ZH_XWC_XLIB_LOCK( wnd->dpy );

   XSetErrorHandler( s_errorHandler );
   zh_gt_xwc_MouseInit( wnd );

   /* set atom identifiers for atom names we will use */
   s_atomDelWin         = XInternAtom( wnd->dpy, "WM_DELETE_WINDOW", True );
   s_atomTimestamp      = XInternAtom( wnd->dpy, "TIMESTAMP", False );
   s_atomAtom           = XInternAtom( wnd->dpy, "ATOM", False );
   s_atomInteger        = XInternAtom( wnd->dpy, "INTEGER", False );
   s_atomString         = XInternAtom( wnd->dpy, "STRING", False );
   s_atomUTF8String     = XInternAtom( wnd->dpy, "UTF8_STRING", False );
   s_atomPrimary        = XInternAtom( wnd->dpy, "PRIMARY", False );
   s_atomSecondary      = XInternAtom( wnd->dpy, "SECONDARY", False );
   s_atomClipboard      = XInternAtom( wnd->dpy, "CLIPBOARD", False );
   s_atomTargets        = XInternAtom( wnd->dpy, "TARGETS", False );
   s_atomCutBuffer0     = XInternAtom( wnd->dpy, "CUT_BUFFER0", False );
   s_atomText           = XInternAtom( wnd->dpy, "TEXT", False );
   s_atomCompoundText   = XInternAtom( wnd->dpy, "COMPOUND_TEXT", False );
   s_atomFullScreen     = XInternAtom( wnd->dpy, "_NET_WM_STATE_FULLSCREEN", False );
   s_atomMaximizedY     = XInternAtom( wnd->dpy, "_NET_WM_STATE_MAXIMIZED_VERT", False );
   s_atomMaximizedX     = XInternAtom( wnd->dpy, "_NET_WM_STATE_MAXIMIZED_HORZ", False );
   s_atomActivate       = XInternAtom( wnd->dpy, "_NET_ACTIVE_WINDOW", False );
   s_atomState          = XInternAtom( wnd->dpy, "_NET_WM_STATE", False );
   s_atomMotifHints     = XInternAtom( wnd->dpy, "_MOTIF_WM_HINTS", False );
   s_atomFrameExtends   = XInternAtom( wnd->dpy, "_NET_FRAME_EXTENTS", False );
   s_atomCardinal       = XInternAtom( wnd->dpy, "CARDINAL", False );

   ZH_XWC_XLIB_UNLOCK( wnd->dpy );

   return ZH_TRUE;
}

static void zh_gt_xwc_DissConnectX( PXWND_DEF wnd )
{
   if( wnd->dpy != NULL )
   {
      ZH_XWC_XLIB_LOCK( wnd->dpy );

      zh_gt_xwc_ClearSelection( wnd );
      zh_gt_xwc_ResetCharTrans( wnd );

#ifdef X_HAVE_UTF8_STRING
      if( wnd->ic )
      {
         XDestroyIC( wnd->ic );
         wnd->ic = NULL;
      }
      if( wnd->im )
      {
         XCloseIM( wnd->im );
         wnd->im = NULL;
      }
#endif
      if( wnd->pm )
      {
         XFreePixmap( wnd->dpy, wnd->pm );
         wnd->pm = 0;
      }
      if( wnd->xfs )
      {
         XFreeFont( wnd->dpy, wnd->xfs );
         wnd->xfs = NULL;
      }
      if( wnd->gc )
      {
         XFreeGC( wnd->dpy, wnd->gc );
         wnd->gc = 0;
      }
      if( wnd->window )
      {
         XDestroyWindow( wnd->dpy, wnd->window );
         wnd->window = 0;
      }

      XSync( wnd->dpy, True );

      ZH_XWC_XLIB_UNLOCK( wnd->dpy );

      XCloseDisplay( wnd->dpy );
      wnd->dpy = NULL;

      /* Hack to avoid race condition inside some XLIB library - it looks
       * in heavy stress MT tests that it can receive some events bound with
       * destroyed objects and executes our error handler.
       */
      s_fIgnoreErrors = ZH_TRUE;
   }

}

/* *********************************************************************** */

static void zh_gt_xwc_DestroyWndDef( PXWND_DEF wnd )
{
   zh_gt_xwc_DissConnectX( wnd );

   if( wnd->szTitle )
      zh_xfree( wnd->szTitle );
   if( wnd->szFontName )
      zh_xfree( wnd->szFontName );
   if( wnd->szFontEncoding )
      zh_xfree( wnd->szFontEncoding );
   if( wnd->szFontSel )
      zh_xfree( wnd->szFontSel );
   if( wnd->pCurrScr )
      zh_xfree( wnd->pCurrScr );
   if( wnd->ClipboardData )
      zh_xfree( wnd->ClipboardData );

   zh_xfree( wnd );
}

/* *********************************************************************** */

static void zh_gt_xwc_SetResizing( PXWND_DEF wnd )
{
   XSizeHints xsize;

   memset( &xsize, 0, sizeof( xsize ) );

   xsize.flags = PWinGravity | PResizeInc | PMinSize | PMaxSize | PBaseSize;

   /* with StaticGravity XMoveWindow expects upper left corner of client area
    * and with NorthWestGravity it expect upper left corner of window with
    * frame and title bar. ConfigureNotify always returns client area position
    * so working with NorthWestGravity it's necessary to update cords returned
    * to user in zh_gt_xwc_UpdateWindowCords()
    */
   xsize.win_gravity = NorthWestGravity;

   if( wnd->fResizable )
   {
      xsize.width_inc = wnd->fontWidth;
      xsize.height_inc = wnd->fontHeight;
      xsize.min_width = wnd->fontWidth * XWC_MIN_COLS;
      xsize.min_height = wnd->fontHeight * XWC_MIN_ROWS;
      xsize.max_width = wnd->fontWidth * XWC_MAX_COLS;
      xsize.max_height = wnd->fontHeight * XWC_MAX_ROWS;
      xsize.base_width = wnd->width;
      xsize.base_height = wnd->height;
   }
   else
   {
      xsize.width_inc = xsize.height_inc = 0;
      xsize.min_width = xsize.max_width = xsize.base_width = wnd->width;
      xsize.min_height = xsize.max_height = xsize.base_height = wnd->height;
   }
   XSetWMNormalHints( wnd->dpy, wnd->window, &xsize );
}

/* *********************************************************************** */

static void zh_gt_xwc_CreateWindow( PXWND_DEF wnd )
{
   ZH_BOOL fResizable = wnd->fResizable, fReset = ZH_FALSE;

   ZH_XWC_XLIB_LOCK( wnd->dpy );

   /* load the standard font */
   if( ! wnd->szFontSel )
   {
      if( ! zh_gt_xwc_SetFont( wnd, wnd->szFontName, wnd->fontWeight, wnd->fontHeight, wnd->szFontEncoding ) )
      {
         if( ! zh_gt_xwc_SetFont( wnd, XWC_DEFAULT_FONT_NAME, XWC_DEFAULT_FONT_WEIGHT, XWC_DEFAULT_FONT_HEIGHT, XWC_DEFAULT_FONT_ENCODING ) )
         {
            ZH_XWC_XLIB_UNLOCKRAW( wnd->dpy );

            /* TODO: a standard Ziher error should be generated here when
                     it can run without console!
            zh_errRT_TERM( EG_CREATE, 10001, NULL, "Cannot load 'fixed' font", 0, 0 );
            return;
            */
            s_fNoXServer = ZH_TRUE;
            zh_errInternal( 10001, "Cannot load 'fixed' font", NULL, NULL );
         }
      }
   }

   /* reset character translation table (after font selection) */
   zh_gt_xwc_ResetCharTrans( wnd );

   if( ! wnd->window )
   {
      int blackColor;

      /* Set standard colors */
      zh_gt_xwc_setPalette( wnd );

      blackColor = BlackPixel( wnd->dpy, DefaultScreen( wnd->dpy ) );
      wnd->window = XCreateSimpleWindow( wnd->dpy, DefaultRootWindow( wnd->dpy ),
                              0, 0,
                              wnd->fontWidth * wnd->cols,
                              wnd->fontHeight * wnd->rows,
                              0, blackColor, blackColor );
      XSelectInput( wnd->dpy, wnd->window, XWC_STD_MASK );
      wnd->gc = XCreateGC( wnd->dpy, wnd->window, 0, NULL );

      /* Line width 2 */
      XSetLineAttributes( wnd->dpy, wnd->gc, 1, LineSolid, CapRound, JoinBevel );
      zh_gt_xwc_SetTitle( wnd, wnd->szTitle );
   }

   XSetFont( wnd->dpy, wnd->gc, wnd->xfs->fid );

   if( zh_gt_xwc_CreatePixmap( wnd ) &&
       ! ( wnd->fFullScreen || wnd->fMaximized || wnd->fMinimized ) )
   {
      wnd->fResizable = fReset = ZH_TRUE;
      zh_gt_xwc_SetResizing( wnd );
   }
   zh_gt_xwc_ResizeRequest( wnd, wnd->cols, wnd->rows );
   XMapWindow( wnd->dpy, wnd->window );

   /* enable FullScreen mode if set by user */
   if( wnd->fFullScreen )
   {
      zh_gt_xwc_FullScreen( wnd );
      wnd->fResizable = ZH_TRUE;
   }
   else if( wnd->fMinimized )
      XIconifyWindow( wnd->dpy, wnd->window, DefaultScreen( wnd->dpy ) );
   else if( wnd->fMaximized )
      zh_gt_xwc_MaximizeScreen( wnd );
   else if( wnd->iNewPosX >= 0 && wnd->iNewPosY >= 0 )
      XMoveWindow( wnd->dpy, wnd->window, wnd->iNewPosX, wnd->iNewPosY );

   /* Request WM to deliver destroy event */
   XSetWMProtocols( wnd->dpy, wnd->window, &s_atomDelWin, 1 );

   zh_gt_xwc_MotifWmHints( wnd );

   /* ok, now we can inform the X manager about our new status: */
   zh_gt_xwc_SetResizing( wnd );

#ifdef X_HAVE_UTF8_STRING
   if( ! wnd->im )
   {
      wnd->im = XOpenIM( wnd->dpy, NULL, NULL, NULL );
      if( wnd->im )
      {
         wnd->ic = XCreateIC( wnd->im,
                              XNInputStyle, XIMPreeditNothing | XIMStatusNothing,
                              XNClientWindow, wnd->window,
                              XNFocusWindow, wnd->window,
                              NULL );
         if( ! wnd->ic )
         {
            XCloseIM( wnd->im );
            wnd->im = NULL;
         }
         else
            wnd->fUTF8key = ZH_TRUE;
      }
   }
#ifdef XWC_DEBUG
   printf( "\nXIC=%p, XIC=%p\n", wnd->im, wnd->ic ); fflush( stdout );
#endif
#endif

#ifdef XWC_DEBUG
   printf( "Window created\n" ); fflush( stdout );
#endif

   ZH_XWC_XLIB_UNLOCK( wnd->dpy );

   if( wnd->fResizable != fResizable || fReset )
   {
      zh_gt_xwc_ProcessMessages( wnd, ZH_TRUE );
      ZH_XWC_XLIB_LOCK( wnd->dpy );
      wnd->fResizable = fResizable;
      zh_gt_xwc_MotifWmHints( wnd );
      zh_gt_xwc_SetResizing( wnd );
      ZH_XWC_XLIB_UNLOCK( wnd->dpy );
   }

}

/* *********************************************************************** */

/* *********************************************************************** */

static void zh_gt_xwc_RealRefresh( PXWND_DEF wnd, ZH_BOOL fSync )
{
   if( ! wnd->fInit )
   {
      if( zh_gt_xwc_ConnectX( wnd, ZH_TRUE ) )
      {
         zh_gt_xwc_CreateWindow( wnd );
         wnd->fInit = ZH_TRUE;
         zh_gt_xwc_Enable();
         fSync = ZH_TRUE;
      }
      else
         return;
   }

   if( s_updateMode == XWC_SYNC_UPDATE && ! wnd->fRefresh )
   {
      wnd->fRefresh = ZH_TRUE;
      zh_gt_xwc_ProcessMessages( wnd, fSync );
      wnd->fRefresh = ZH_FALSE;
   }
}

/* *********************************************************************** */

static void zh_gt_xwc_LateRefresh( PXWND_DEF wnd )
{
   if( wnd->fInit )
      zh_gt_xwc_RealRefresh( wnd, ZH_FALSE );
}

/* *********************************************************************** */

/* *********************************************************************** */

static void zh_gt_xwc_Init( PZH_GT pGT, ZH_FHANDLE hFilenoStdin, ZH_FHANDLE hFilenoStdout, ZH_FHANDLE hFilenoStderr )
{
   PXWND_DEF wnd;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_xwc_Init(%p,%p,%p,%p)", ( void * ) pGT, ( void * ) ( ZH_PTRUINT ) hFilenoStdin, ( void * ) ( ZH_PTRUINT ) hFilenoStdout, ( void * ) ( ZH_PTRUINT ) hFilenoStderr ) );

#ifdef ZH_XWC_USE_LOCALE
   setlocale( LC_CTYPE, "" );
#endif

   ZH_GTSUPER_INIT( pGT, hFilenoStdin, hFilenoStdout, hFilenoStderr );

#if !defined( ZH_XWC_XLIB_NEEDLOCKS ) && ! defined( ZH_XWC_XLOCK_OFF )
   if( zh_vmIsMt() )
   {
      if( ! XInitThreads() )
         zh_errInternal( 10002, "XInitThreads() failed !!!", NULL, NULL );
   }
#endif

   wnd = zh_gt_xwc_CreateWndDef( pGT );
   ZH_GTLOCAL( pGT ) = wnd;

   /* for signal handler */
   s_wnd = wnd;

   wnd->cursorState = ZH_TRUE;
   wnd->cursorBlinkRate = 700;
   wnd->cursorStateTime = 0;

   ZH_GTSELF_RESIZE( pGT, wnd->rows, wnd->cols );
   ZH_GTSELF_SEMICOLD( pGT );

   /* For immediate connection to XSarver and screen Window show */
   #if 0
   zh_gt_xwc_RealRefresh( wnd, ZH_TRUE );
   #endif

   /* For connection to XSarver only */
   #if 0
   zh_gt_xwc_ConnectX( wnd, ZH_TRUE );
   #endif
}

/* *********************************************************************** */

static void zh_gt_xwc_Exit( PZH_GT pGT )
{
   PXWND_DEF wnd;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_xwc_Exit(%p)", ( void * ) pGT ) );

   wnd = ZH_GTXWC_GET( pGT );

   zh_gt_xwc_Disable();
   ZH_GTSUPER_EXIT( pGT );

   if( wnd )
   {
      zh_gt_xwc_DestroyWndDef( wnd );
   }
}

/* *********************************************************************** */

static ZH_BOOL zh_gt_xwc_SetMode( PZH_GT pGT, int iRow, int iCol )
{
   ZH_BOOL fResult = ZH_FALSE;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_xwc_SetMode(%p,%d,%d)", ( void * ) pGT, iRow, iCol ) );

   if( iCol >= XWC_MIN_COLS && iRow >= XWC_MIN_ROWS &&
       iCol <= XWC_MAX_COLS && iRow <= XWC_MAX_ROWS )
   {
      PXWND_DEF wnd = ZH_GTXWC_GET( pGT );

      if( iCol == wnd->cols && iRow == wnd->rows )
      {
         fResult = ZH_GTSELF_RESIZE( pGT, wnd->rows, wnd->cols );
         if( ! wnd->fInit )
            ZH_GTSELF_SEMICOLD( pGT );
      }
      else if( ! wnd->fInit )
      {
         zh_gt_xwc_SetScrBuff( wnd, iCol, iRow );
         ZH_GTSELF_SEMICOLD( pGT );
         fResult = ZH_TRUE;
      }
      else
      {
         ZH_BOOL fResizable = wnd->fResizable;
         ZH_MAXINT timeout;
         ZH_MAXUINT timer;

#ifdef XWC_DEBUG
         printf( "SetMode(%d,%d) begin\n", iRow, iCol ); fflush( stdout );
#endif

         ZH_XWC_XLIB_LOCK( wnd->dpy );
         if( ! fResizable )
         {
            wnd->fResizable = ZH_TRUE;
            zh_gt_xwc_SetResizing( wnd );
         }
         zh_gt_xwc_ResizeRequest( wnd, iCol, iRow );
         ZH_XWC_XLIB_UNLOCK( wnd->dpy );

         timeout = 1000;
         timer = zh_timerInit( timeout );

         do
         {
            zh_gt_xwc_RealRefresh( wnd, ZH_TRUE );
            if( iCol == wnd->cols && iRow == wnd->rows )
               fResult = ZH_TRUE;
            else if( ( timeout = zh_timerTest( timeout, &timer ) ) == 0 )
               break;
            else
               zh_releaseCPU();
         }
         while( !fResult );

         if( ! fResizable )
         {
            wnd->fResizable = ZH_FALSE;
            ZH_XWC_XLIB_LOCK( wnd->dpy );
            zh_gt_xwc_SetResizing( wnd );
            ZH_XWC_XLIB_UNLOCK( wnd->dpy );
         }

#ifdef XWC_DEBUG
         printf( "SetMode(%d,%d) => %d\n", iRow, iCol, fResult ); fflush( stdout );
#endif
      }
   }

   return fResult;
}

/* *********************************************************************** */

static ZH_BOOL zh_gt_xwc_GetBlink( PZH_GT pGT )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_xwc_GetBlink(%p)", ( void * ) pGT ) );

   ZH_SYMBOL_UNUSED( pGT );

   return ZH_FALSE;
}

/* *********************************************************************** */

static const char * zh_gt_xwc_Version( PZH_GT pGT, int iType )
{
   ZH_SYMBOL_UNUSED( pGT );

   if( iType == 0 )
      return ZH_GT_DRVNAME( ZH_GT_NAME );

#ifdef X_HAVE_UTF8_STRING
   return "Terminal: X11 (XWC) (UTF-8)";
#else
   return "Terminal: X11 (XWC)";
#endif
}

/* *********************************************************************** */

static int zh_gt_xwc_ReadKey( PZH_GT pGT, int iEventMask )
{
   PXWND_DEF wnd;
   int c = 0;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_xwc_ReadKey(%p,%d)", ( void * ) pGT, iEventMask ) );

   ZH_SYMBOL_UNUSED( iEventMask );

   wnd = ZH_GTXWC_GET( pGT );
   zh_gt_xwc_LateRefresh( wnd );
   #if 0
   zh_gt_xwc_RealRefresh( wnd, ZH_FALSE );
   #endif

   if( zh_gt_xwc_GetCharFromInputQueue( wnd, &c ) )
      return c;
   else
      return 0;
}


/* *********************************************************************** */

static void zh_gt_xwc_Tone( PZH_GT pGT, double dFrequency, double dDuration )
{
   PXWND_DEF wnd;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_xwc_Tone(%p,%lf,%lf)", ( void * ) pGT, dFrequency, dDuration ) );

   /* The conversion from (DOS) timer tick units to
      milliseconds is * 1000.0 / 18.2. */
   dDuration /= 18.2;

   wnd = ZH_GTXWC_GET( pGT );
   if( wnd->dpy != NULL )
   {
      XKeyboardControl XkbCtrl;

      XkbCtrl.bell_pitch    = ( int ) dFrequency;
      XkbCtrl.bell_duration = ( int ) ( dDuration * 1000 );

      ZH_XWC_XLIB_LOCK( wnd->dpy );
      XChangeKeyboardControl( wnd->dpy, KBBellPitch | KBBellDuration, &XkbCtrl );
      XBell( wnd->dpy, 0 );
      XSync( wnd->dpy, False );
      ZH_XWC_XLIB_UNLOCK( wnd->dpy );
   }
   zh_gtSleep( pGT, dDuration );
}

/* *********************************************************************** */

static ZH_BOOL zh_gt_xwc_mouse_IsPresent( PZH_GT pGT )
{
   PXWND_DEF wnd;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_xwc_mouse_IsPresent(%p)", ( void * ) pGT ) );

   wnd = ZH_GTXWC_GET( pGT );
   zh_gt_xwc_ConnectX( wnd, ZH_TRUE );
   return wnd->mouseNumButtons > 0;
}

/* *********************************************************************** */

static void zh_gt_xwc_mouse_GetPos( PZH_GT pGT, int * piRow, int * piCol )
{
   PXWND_DEF wnd;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_xwc_mouse_GetPos(%p,%p,%p)", ( void * ) pGT, ( void * ) piRow, ( void * ) piCol ) );

   wnd = ZH_GTXWC_GET( pGT );
   if( wnd )
   {
      zh_gt_xwc_LateRefresh( wnd );
      *piRow = wnd->mouseRow;
      *piCol = wnd->mouseCol;
   }
}

/* *********************************************************************** */

static void zh_gt_xwc_mouse_SetPos( PZH_GT pGT, int iRow, int iCol )
{
   PXWND_DEF wnd;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_xwc_mouse_SetPos(%p,%d,%d)", ( void * ) pGT, iRow, iCol ) );

   wnd = ZH_GTXWC_GET( pGT );
   wnd->mouseGotoRow = iRow;
   wnd->mouseGotoCol = iCol;
   zh_gt_xwc_LateRefresh( wnd );
}

/* *********************************************************************** */

static ZH_BOOL zh_gt_xwc_mouse_ButtonState( PZH_GT pGT, int iButton )
{
   PXWND_DEF wnd;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_xwc_mouse_ButtonState(%p,%i)", ( void * ) pGT, iButton ) );

   wnd = ZH_GTXWC_GET( pGT );
   zh_gt_xwc_ConnectX( wnd, ZH_TRUE );
   if( iButton >= 0 && iButton < wnd->mouseNumButtons )
      return ( wnd->mouseButtonsState & 1 << iButton ) != 0;
   else
      return ZH_FALSE;
}

/* *********************************************************************** */

static int zh_gt_xwc_mouse_CountButton( PZH_GT pGT )
{
   PXWND_DEF wnd;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_xwc_mouse_CountButton(%p)", ( void * ) pGT ) );

   wnd = ZH_GTXWC_GET( pGT );
   zh_gt_xwc_ConnectX( wnd, ZH_TRUE );

   return wnd->mouseNumButtons;
}

/* *********************************************************************** */

static int zh_gt_xwc_getKbdState( PXWND_DEF wnd )
{
   int iKbdState = 0;

   if( wnd->keyModifiers.bShift ) iKbdState |= ZH_GTI_KBD_SHIFT;
   if( wnd->keyModifiers.bCtrl  ) iKbdState |= ZH_GTI_KBD_CTRL;
   if( wnd->keyModifiers.bAlt   ) iKbdState |= ZH_GTI_KBD_ALT;

   return iKbdState;
}

static ZH_BOOL zh_gt_xwc_Info( PZH_GT pGT, int iType, PZH_GT_INFO pInfo )
{
   PXWND_DEF wnd;
   int iVal;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_xwc_Info(%p,%d,%p)", ( void * ) pGT, iType, ( void * ) pInfo ) );

   wnd = ZH_GTXWC_GET( pGT );
   if( ! wnd->dpy )
   {
      switch( iType )
      {
         case ZH_GTI_ISSCREENPOS:
         case ZH_GTI_KBDSUPPORT:
         case ZH_GTI_ISGRAPHIC:
            zh_gt_xwc_ConnectX( wnd, ZH_FALSE );
            break;

         case ZH_GTI_INPUTFD:
         case ZH_GTI_SCREENDEPTH:
         case ZH_GTI_DESKTOPDEPTH:
         case ZH_GTI_DESKTOPWIDTH:
         case ZH_GTI_DESKTOPHEIGHT:
         case ZH_GTI_DESKTOPCOLS:
         case ZH_GTI_DESKTOPROWS:
         case ZH_GTI_CLIPBOARDDATA:
         case ZH_GTI_FONTSEL:
            zh_gt_xwc_ConnectX( wnd, ZH_TRUE );
            break;
      }
   }

   switch( iType )
   {
      case ZH_GTI_ISSCREENPOS:
      case ZH_GTI_KBDSUPPORT:
      case ZH_GTI_ISGRAPHIC:
         pInfo->pResult = zh_itemPutL( pInfo->pResult, wnd->dpy != NULL );
         break;

      case ZH_GTI_ONLINE:
         pInfo->pResult = zh_itemPutL( pInfo->pResult, wnd->fInit );
         break;

      case ZH_GTI_ISUNICODE:
         pInfo->pResult = zh_itemPutL( pInfo->pResult, ZH_TRUE );
         break;

      case ZH_GTI_INPUTFD:
         pInfo->pResult = zh_itemPutNI( pInfo->pResult, ConnectionNumber( wnd->dpy ) );
         break;

      case ZH_GTI_SCREENWIDTH:
         pInfo->pResult = zh_itemPutNI( pInfo->pResult, wnd->width );
         break;

      case ZH_GTI_SCREENHEIGHT:
         pInfo->pResult = zh_itemPutNI( pInfo->pResult, wnd->height );
         break;

      case ZH_GTI_VIEWMAXWIDTH:
         pInfo->pResult = zh_itemPutNI( pInfo->pResult, wnd->cols );
         break;

      case ZH_GTI_VIEWMAXHEIGHT:
         pInfo->pResult = zh_itemPutNI( pInfo->pResult, wnd->rows );
         break;

      case ZH_GTI_DESKTOPWIDTH:
      case ZH_GTI_DESKTOPHEIGHT:
      case ZH_GTI_DESKTOPCOLS:
      case ZH_GTI_DESKTOPROWS:
      {
         XWindowAttributes wndAttr;
         ZH_XWC_XLIB_LOCK( wnd->dpy );
         XGetWindowAttributes( wnd->dpy, DefaultRootWindow( wnd->dpy ), &wndAttr );
         ZH_XWC_XLIB_UNLOCK( wnd->dpy );
         switch( iType )
         {
            case ZH_GTI_DESKTOPWIDTH:
               iVal = wndAttr.width;
               break;
            case ZH_GTI_DESKTOPHEIGHT:
               iVal = wndAttr.height;
               break;
            case ZH_GTI_DESKTOPCOLS:
               iVal = wndAttr.width / wnd->fontWidth;
               break;
            case ZH_GTI_DESKTOPROWS:
               iVal = wndAttr.height / wnd->fontHeight;
               break;
            default:
               iVal = 0;
         }
         pInfo->pResult = zh_itemPutNI( pInfo->pResult, iVal );
         break;
      }

      case ZH_GTI_SCREENDEPTH:
      case ZH_GTI_DESKTOPDEPTH:
         ZH_XWC_XLIB_LOCK( wnd->dpy );
         pInfo->pResult = zh_itemPutNI( pInfo->pResult,
                     DefaultDepth( wnd->dpy, DefaultScreen( wnd->dpy ) ) );
         ZH_XWC_XLIB_UNLOCK( wnd->dpy );
         break;

      case ZH_GTI_FONTWEIGHT:
         pInfo->pResult = zh_itemPutNI( pInfo->pResult, wnd->fontWeight );
         if( pInfo->pNewVal && ZH_IS_NUMERIC( pInfo->pNewVal ) )
         {
            iVal = zh_itemGetNI( pInfo->pNewVal );
            switch( iVal )
            {
               case ZH_GTI_FONTW_THIN:
               case ZH_GTI_FONTW_NORMAL:
               case ZH_GTI_FONTW_BOLD:
                  wnd->fontWeight = iVal;
            }
         }
         break;

      case ZH_GTI_FONTWIDTH:
         pInfo->pResult = zh_itemPutNI( pInfo->pResult, wnd->fontWidth );
         iVal = zh_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 ) /* TODO */
            wnd->fontWidth = iVal;
         break;

      case ZH_GTI_FONTSIZE:
         pInfo->pResult = zh_itemPutNI( pInfo->pResult, wnd->fontHeight );
         iVal = zh_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 ) /* TODO */
         {
            wnd->fontHeight = iVal;
            if( wnd->fInit )
            {
               ZH_BOOL fInit;
               ZH_XWC_XLIB_LOCK( wnd->dpy );
               fInit = zh_gt_xwc_SetFont( wnd, wnd->szFontName, wnd->fontWeight,
                                          wnd->fontHeight, wnd->szFontEncoding );
               ZH_XWC_XLIB_UNLOCK( wnd->dpy );
               if( fInit )
                  zh_gt_xwc_CreateWindow( wnd );
            }
            else if( wnd->xfs )
            {
               ZH_XWC_XLIB_LOCK( wnd->dpy );
               XFreeFont( wnd->dpy, wnd->xfs );
               wnd->xfs = NULL;
               ZH_XWC_XLIB_UNLOCK( wnd->dpy );
            }
         }
         break;

      case ZH_GTI_FONTNAME:
         pInfo->pResult = zh_itemPutC( pInfo->pResult, wnd->szFontName );
         if( pInfo->pNewVal && ZH_IS_STRING( pInfo->pNewVal ) ) /* TODO */
         {
            if( wnd->szFontName )
               zh_xfree( wnd->szFontName );
            wnd->szFontName = zh_strdup( zh_itemGetCPtr( pInfo->pNewVal ) );
         }
         break;

      case ZH_GTI_FONTSEL:
         pInfo->pResult = zh_itemPutC( pInfo->pResult, wnd->szFontSel );
         if( pInfo->pNewVal && ZH_IS_STRING( pInfo->pNewVal ) )
         {
            ZH_BOOL fInit;
            ZH_XWC_XLIB_LOCK( wnd->dpy );
            fInit = zh_gt_xwc_SetFont( wnd, zh_itemGetCPtr( pInfo->pNewVal ), 0, 0, NULL ) &&
                    wnd->fInit;
            ZH_XWC_XLIB_UNLOCK( wnd->dpy );
            if( fInit )
               zh_gt_xwc_CreateWindow( wnd );
         }
         break;

      case ZH_GTI_FONTATTRIBUTE:
         pInfo->pResult = zh_itemPutNI( pInfo->pResult,
                           ( wnd->fFixMetric ? ZH_GTI_FONTA_FIXMETRIC : 0 ) |
                           ( wnd->fClearBkg  ? ZH_GTI_FONTA_CLRBKG    : 0 ) |
                           ( wnd->fDrawBox   ? ZH_GTI_FONTA_DRAWBOX   : 0 ) );
         if( pInfo->pNewVal && ZH_IS_NUMERIC( pInfo->pNewVal ) )
         {
            iVal = zh_itemGetNI( pInfo->pNewVal );
            wnd->fFixMetric = ( iVal & ZH_GTI_FONTA_FIXMETRIC ) != 0;
            wnd->fClearBkg  = ( iVal & ZH_GTI_FONTA_CLRBKG    ) != 0;
            wnd->fDrawBox   = ( iVal & ZH_GTI_FONTA_DRAWBOX   ) != 0;
            if( wnd->fInit )
            {
               ZH_XWC_XLIB_LOCK( wnd->dpy );
               zh_gt_xwc_ResetCharTrans( wnd );
               ZH_XWC_XLIB_UNLOCK( wnd->dpy );
               zh_gt_xwc_InvalidateFull( wnd );
            }
         }
         break;

      case ZH_GTI_WINTITLE:
         pInfo->pResult = wnd->szTitle ?
                           zh_itemPutStrLenUTF8( pInfo->pResult, wnd->szTitle,
                                                 strlen( wnd->szTitle ) ) :
                           zh_itemPutC( pInfo->pResult, NULL );
         if( pInfo->pNewVal && ZH_IS_STRING( pInfo->pNewVal ) )
         {
            void * hString;
            ZH_SIZE nLen;
            const char * pszTitle = zh_itemGetStrUTF8( pInfo->pNewVal, &hString, &nLen );

            if( wnd->szTitle )
               zh_xfree( wnd->szTitle );
            wnd->szTitle = nLen > 0 ? zh_strdup( pszTitle ) : NULL;
            zh_strfree( hString );

            wnd->fDspTitle = ZH_TRUE;
            if( wnd->window )
               zh_gt_xwc_ProcessMessages( wnd, ZH_FALSE );
         }
         break;

      case ZH_GTI_CLIPBOARDDATA:
      {
         void * hString;
         ZH_SIZE nLen;
         const char * pszClipboardData = zh_itemGetStrUTF8( pInfo->pNewVal, &hString, &nLen );

         if( pszClipboardData )
         {
            zh_gt_xwc_RealRefresh( wnd, ZH_FALSE );
            zh_gt_xwc_SetSelection( wnd, pszClipboardData, nLen, ZH_TRUE );
            zh_gt_xwc_RealRefresh( wnd, ZH_FALSE );
            zh_strfree( hString );
         }
         else
         {
            zh_gt_xwc_RealRefresh( wnd, ZH_FALSE );
            zh_gt_xwc_RequestSelection( wnd );
            pInfo->pResult = zh_itemPutStrLenUTF8( pInfo->pResult,
                                                   ( char * ) wnd->ClipboardData,
                                                   wnd->ClipboardSize );
         }
         break;
      }

      case ZH_GTI_SELECTCOPY:
         pInfo->pResult = zh_itemPutL( pInfo->pResult, wnd->fSelectCopy );
         if( pInfo->pNewVal && ZH_IS_LOGICAL( pInfo->pNewVal ) )
            wnd->fSelectCopy = zh_itemGetL( pInfo->pNewVal );
         break;

      case ZH_GTI_CURSORBLINKRATE:
         pInfo->pResult = zh_itemPutNI( pInfo->pResult, wnd->cursorBlinkRate );
         if( pInfo->pNewVal && ZH_IS_NUMERIC( pInfo->pNewVal ) )
         {
            iVal = zh_itemGetNI( pInfo->pNewVal );
            wnd->cursorBlinkRate = ZH_MAX( iVal, 0 );
         }
         break;

      case ZH_GTI_KBDSHIFTS:
         pInfo->pResult = zh_itemPutNI( pInfo->pResult,
                                        zh_gt_xwc_getKbdState( wnd ) );
         break;

      case ZH_GTI_ALTENTER:
         pInfo->pResult = zh_itemPutL( pInfo->pResult, wnd->fAltEnter );
         if( pInfo->pNewVal && ZH_IS_LOGICAL( pInfo->pNewVal ) )
            wnd->fAltEnter = zh_itemGetL( pInfo->pNewVal );
         break;

      case ZH_GTI_ISFULLSCREEN:
         pInfo->pResult = zh_itemPutL( pInfo->pResult, wnd->fFullScreen );
         if( pInfo->pNewVal && ZH_IS_LOGICAL( pInfo->pNewVal ) &&
             zh_itemGetL( pInfo->pNewVal ) != wnd->fFullScreen )
         {
            wnd->fFullScreen = ! wnd->fFullScreen;
            if( wnd->fInit )
            {
               ZH_XWC_XLIB_LOCK( wnd->dpy );
               zh_gt_xwc_FullScreen( wnd );
               ZH_XWC_XLIB_UNLOCK( wnd->dpy );
            }
         }
         break;

      case ZH_GTI_MAXIMIZED:
         pInfo->pResult = zh_itemPutL( pInfo->pResult, wnd->fMaximized );
         if( pInfo->pNewVal && ZH_IS_LOGICAL( pInfo->pNewVal ) &&
             zh_itemGetL( pInfo->pNewVal ) != wnd->fMaximized )
         {
            wnd->fMaximized = ! wnd->fMaximized;
            if( wnd->fInit )
            {
               ZH_XWC_XLIB_LOCK( wnd->dpy );
               zh_gt_xwc_MaximizeScreen( wnd );
               ZH_XWC_XLIB_UNLOCK( wnd->dpy );
            }
         }
         break;

      case ZH_GTI_MINIMIZED:
         pInfo->pResult = zh_itemPutL( pInfo->pResult, wnd->fMinimized );
         if( pInfo->pNewVal && ZH_IS_LOGICAL( pInfo->pNewVal ) &&
             zh_itemGetL( pInfo->pNewVal ) != wnd->fMinimized )
         {
            wnd->fMinimized = ! wnd->fMinimized;
            if( wnd->fInit )
            {
               ZH_XWC_XLIB_LOCK( wnd->dpy );
               if( wnd->fMinimized )
                  XIconifyWindow( wnd->dpy, wnd->window, DefaultScreen( wnd->dpy ) );
               else
               {
                  XMapWindow( wnd->dpy, wnd->window );
                  XRaiseWindow( wnd->dpy, wnd->window );
                  zh_gt_xwc_ActivateScreen( wnd );
               }
               ZH_XWC_XLIB_UNLOCK( wnd->dpy );
            }
         }
         break;

      case ZH_GTI_CLOSABLE:
         pInfo->pResult = zh_itemPutL( pInfo->pResult, wnd->iCloseMode == 0 );
         if( pInfo->pNewVal && ZH_IS_LOGICAL( pInfo->pNewVal ) &&
             ( zh_itemGetL( pInfo->pNewVal ) ? ( wnd->iCloseMode != 0 ) :
                                               ( wnd->iCloseMode == 0 ) ) )
         {
            iVal = wnd->iCloseMode;
            wnd->iCloseMode = iVal == 0 ? 1 : 0;
            if( iVal == 2 && wnd->fInit )
            {
               ZH_XWC_XLIB_LOCK( wnd->dpy );
               zh_gt_xwc_MotifWmHints( wnd );
               ZH_XWC_XLIB_UNLOCK( wnd->dpy );
            }
         }
         break;

      case ZH_GTI_CLOSEMODE:
         pInfo->pResult = zh_itemPutNI( pInfo->pResult, wnd->iCloseMode );
         if( pInfo->pNewVal && ZH_IS_NUMERIC( pInfo->pNewVal ) )
         {
            iVal = zh_itemGetNI( pInfo->pNewVal );
            if( iVal >= 0 && iVal <= 2 && wnd->iCloseMode != iVal )
            {
               if( ( iVal == 2 || wnd->iCloseMode == 2 ) && wnd->fInit )
               {
                  wnd->iCloseMode = iVal;
                  ZH_XWC_XLIB_LOCK( wnd->dpy );
                  zh_gt_xwc_MotifWmHints( wnd );
                  ZH_XWC_XLIB_UNLOCK( wnd->dpy );
               }
               else
                  wnd->iCloseMode = iVal;
            }
         }
         break;

      case ZH_GTI_RESIZABLE:
         pInfo->pResult = zh_itemPutL( pInfo->pResult, wnd->fResizable );
         if( pInfo->pNewVal && ZH_IS_LOGICAL( pInfo->pNewVal ) &&
             zh_itemGetL( pInfo->pNewVal ) != wnd->fResizable )
         {
            wnd->fResizable = ! wnd->fResizable;
            if( wnd->fInit )
            {
               ZH_XWC_XLIB_LOCK( wnd->dpy );
               zh_gt_xwc_MotifWmHints( wnd );
               zh_gt_xwc_SetResizing( wnd );
               ZH_XWC_XLIB_UNLOCK( wnd->dpy );
            }
         }
         break;

      case ZH_GTI_RESIZEMODE:
         pInfo->pResult = zh_itemPutNI( pInfo->pResult, ZH_GTI_RESIZEMODE_ROWS );
         if( pInfo->pNewVal && ZH_IS_NUMERIC( pInfo->pNewVal ) )
         {
            iVal = zh_itemGetNI( pInfo->pNewVal );
            switch( iVal )
            {
               case ZH_GTI_RESIZEMODE_FONT:
                  /* this mode is not supported yet by GTXWC */
                  break;
               case ZH_GTI_RESIZEMODE_ROWS:
                  #if 0
                  wnd->iResizeMode = iVal;
                  #endif
                  break;
            }
         }
         break;

      case ZH_GTI_MOUSEPOS_XY:
         if( ! pInfo->pResult )
            pInfo->pResult = zh_itemNew( NULL );
         zh_arrayNew( pInfo->pResult, 2 );
         zh_arraySetNI( pInfo->pResult, 1, wnd->mouseColPxl );
         zh_arraySetNI( pInfo->pResult, 2, wnd->mouseRowPxl );
         break;

      case ZH_GTI_SETPOS_XY:
      case ZH_GTI_SETPOS_ROWCOL:
      {
         int x = wnd->iNewPosX, y = wnd->iNewPosY;

         if( wnd->window )
         {
            XWindowAttributes wndAttr;

            ZH_XWC_XLIB_LOCK( wnd->dpy );
            if( XGetWindowAttributes( wnd->dpy, wnd->window, &wndAttr ) )
            {
               Window wndChild;
               if( ! XTranslateCoordinates( wnd->dpy, wnd->window, wndAttr.root,
                                            wndAttr.x, wndAttr.y, &x, &y,
                                            &wndChild ) )
               {
                  x = wndAttr.x;
                  y = wndAttr.y;
               }
            }
            ZH_XWC_XLIB_UNLOCK( wnd->dpy );
         }

         if( ! pInfo->pResult )
            pInfo->pResult = zh_itemNew( NULL );
         zh_arrayNew( pInfo->pResult, 2 );

         if( wnd->fInit )
         {
            ZH_XWC_XLIB_LOCK( wnd->dpy );
            zh_gt_xwc_UpdateWindowCords( wnd, &x, &y );
            ZH_XWC_XLIB_UNLOCK( wnd->dpy );
         }

         if( iType == ZH_GTI_SETPOS_ROWCOL )
         {
            iVal = x;
            x = y / wnd->fontHeight;
            y = iVal / wnd->fontWidth;
         }
         zh_arraySetNI( pInfo->pResult, 1, x );
         zh_arraySetNI( pInfo->pResult, 2, y );

         if( pInfo->pNewVal && ZH_IS_NUMERIC( pInfo->pNewVal ) &&
             pInfo->pNewVal2 && ZH_IS_NUMERIC( pInfo->pNewVal2 ) )
         {
            x = zh_itemGetNI( pInfo->pNewVal );
            y = zh_itemGetNI( pInfo->pNewVal2 );
         }
         else if( pInfo->pNewVal && ZH_IS_ARRAY( pInfo->pNewVal ) &&
                  zh_arrayLen( pInfo->pNewVal ) == 2 )
         {
            x = zh_arrayGetNI( pInfo->pNewVal, 1 );
            y = zh_arrayGetNI( pInfo->pNewVal, 2 );
         }
         else
            break;

         if( iType == ZH_GTI_SETPOS_ROWCOL )
         {
            iVal = x;
            x = y * wnd->fontWidth;
            y = iVal * wnd->fontHeight;
         }
         if( wnd->fInit )
         {
            ZH_XWC_XLIB_LOCK( wnd->dpy );
            XMoveWindow( wnd->dpy, wnd->window, x, y );
            ZH_XWC_XLIB_UNLOCK( wnd->dpy );
         }
         else
         {
            wnd->iNewPosX = x;
            wnd->iNewPosY = y;
         }
         break;
      }
      case ZH_GTI_PALETTE:
         if( pInfo->pNewVal && ZH_IS_NUMERIC( pInfo->pNewVal ) )
         {
            iVal = zh_itemGetNI( pInfo->pNewVal );
            if( iVal >= 0 && iVal < 16 )
            {
               pInfo->pResult = zh_itemPutNI( pInfo->pResult, wnd->colors[ iVal ].value );
               if( pInfo->pNewVal2 && ZH_IS_NUMERIC( pInfo->pNewVal2 ) )
               {
                  int iColor = zh_itemGetNI( pInfo->pNewVal2 );
                  if( iColor != wnd->colors[ iVal ].value )
                  {
                     wnd->colors[ iVal ].value = iColor;
                     wnd->colors[ iVal ].set = ZH_FALSE;
                     if( wnd->fInit )
                     {
                        ZH_XWC_XLIB_LOCK( wnd->dpy );
                        if( zh_gt_xwc_setPalette( wnd ) )
                           zh_gt_xwc_InvalidateFull( wnd );
                        ZH_XWC_XLIB_UNLOCK( wnd->dpy );
                     }
                  }
               }
            }
         }
         else
         {
            if( ! pInfo->pResult )
               pInfo->pResult = zh_itemNew( NULL );
            zh_arrayNew( pInfo->pResult, 16 );
            for( iVal = 0; iVal < 16; iVal++ )
               zh_arraySetNI( pInfo->pResult, iVal + 1, wnd->colors[ iVal ].value );
            if( pInfo->pNewVal && ZH_IS_ARRAY( pInfo->pNewVal ) &&
                zh_arrayLen( pInfo->pNewVal ) == 16 )
            {
               for( iVal = 0; iVal < 16; iVal++ )
               {
                  int iColor = zh_arrayGetNI( pInfo->pNewVal, iVal + 1 );
                  if( iColor != wnd->colors[ iVal ].value )
                  {
                     wnd->colors[ iVal ].value = iColor;
                     wnd->colors[ iVal ].set = ZH_FALSE;
                  }
               }
               if( wnd->fInit )
               {
                  ZH_XWC_XLIB_LOCK( wnd->dpy );
                  if( zh_gt_xwc_setPalette( wnd ) )
                     zh_gt_xwc_InvalidateFull( wnd );
                  ZH_XWC_XLIB_UNLOCK( wnd->dpy );
               }
            }
         }
         break;

      case ZH_GTI_DISPIMAGE:
         if( wnd->window && pInfo->pNewVal &&
             ( ( ZH_IS_ARRAY( pInfo->pNewVal ) &&
                 zh_arrayLen( pInfo->pNewVal ) == ( ZH_SIZE )
                 ( ( zh_arrayGetType( pInfo->pNewVal, 4 ) & ZH_IT_NUMERIC ) ? 4 : 3 ) ) ||
               ZH_IS_STRING( pInfo->pNewVal ) ) )
         {
            XImage * xImage = NULL;
            XWC_RECT rx;

            /* { pBitmap, iWidth, iHeight [, iDepth ] } */
            if( ( zh_arrayGetType( pInfo->pNewVal, 1 ) & ( ZH_IT_POINTER | ZH_IT_STRING ) ) &&
                ( zh_arrayGetType( pInfo->pNewVal, 2 ) & ZH_IT_NUMERIC ) &&
                ( zh_arrayGetType( pInfo->pNewVal, 3 ) & ZH_IT_NUMERIC ) )
            {
               ZH_SIZE nSize = zh_arrayGetCLen( pInfo->pNewVal, 1 );
               int iWidth  = zh_arrayGetNI( pInfo->pNewVal, 2 );
               int iHeight = zh_arrayGetNI( pInfo->pNewVal, 3 );
               int iDepth  = zh_arrayGetNI( pInfo->pNewVal, 4 );
               int iPad    = 32;
               const char * pFreeImage = NULL;

               ZH_XWC_XLIB_LOCK( wnd->dpy );
               if( iDepth == 0 )
                  iDepth = DefaultDepth( wnd->dpy, DefaultScreen( wnd->dpy ) );
               if( iWidth > 0 && iHeight > 0 && iDepth > 0 )
               {
                  if( nSize > 0 )
                  {
                     while( pFreeImage == NULL && iPad >= 8 )
                     {
                        int iPitch = ( iWidth * iDepth + iPad - 1 ) / iPad;
                        if( nSize == ( ZH_SIZE ) ( iHeight * iPitch ) )
                           pFreeImage = zh_arrayGetCPtr( pInfo->pNewVal, 1 );
                        else
                           iPad >>= 1;
                     }
                  }
                  else
                     pFreeImage = ( const char * ) zh_arrayGetPtr( pInfo->pNewVal, 1 );
               }
               if( pFreeImage != NULL )
                  xImage = XCreateImage( wnd->dpy, DefaultVisual( wnd->dpy, DefaultScreen( wnd->dpy ) ),
                                         iDepth, ZPixmap, 0, ( char * ) ZH_UNCONST( pFreeImage ),
                                         iWidth, iHeight, iPad, 0 );
               ZH_XWC_XLIB_UNLOCK( wnd->dpy );
            }

            rx.left = rx.top = 0;
            if( xImage )
            {
               rx.right = xImage->width - 1;
               rx.bottom = xImage->height - 1;
            }
            else
            {
               rx.right = wnd->width - 1;
               rx.bottom = wnd->height - 1;
            }

            /* fetch & validate area for displaying */
            if( pInfo->pNewVal2 && ZH_IS_ARRAY( pInfo->pNewVal2 ) )
            {
               switch( zh_arrayLen( pInfo->pNewVal2 ) )
               {
                  case 2:
                     rx.left   = zh_arrayGetNI( pInfo->pNewVal2, 1 );
                     rx.top    = zh_arrayGetNI( pInfo->pNewVal2, 2 );
                     if( xImage )
                     {
                        rx.right  += rx.left;
                        rx.bottom += rx.top;
                     }
                     break;
                  case 4:
                     rx.left   = zh_arrayGetNI( pInfo->pNewVal2, 1 );
                     rx.top    = zh_arrayGetNI( pInfo->pNewVal2, 2 );
                     rx.right  = zh_arrayGetNI( pInfo->pNewVal2, 3 );
                     rx.bottom = zh_arrayGetNI( pInfo->pNewVal2, 4 );
                     if( xImage )
                     {
                        if( rx.right >= rx.left + xImage->width )
                           rx.right = rx.left + xImage->width - 1;
                        if( rx.bottom >= rx.top + xImage->height )
                           rx.bottom = rx.top + xImage->height - 1;
                     }
                     break;
               }
            }

            if( rx.right >= wnd->width )
               rx.right = wnd->width - 1;
            if( rx.bottom >= wnd->height )
               rx.bottom = wnd->height - 1;

            if( rx.left >= 0 && rx.top >= 0 &&
                rx.left <= rx.right && rx.top <= rx.bottom )
            {
               ZH_GTSELF_REFRESH( pGT );
               if( xImage )
               {
                  ZH_XWC_XLIB_LOCK( wnd->dpy );
                  XPutImage( wnd->dpy, wnd->pm, wnd->gc, xImage, 0, 0,
                             rx.left, rx.top, rx.right - rx.left + 1, rx.bottom - rx.top + 1 );
                  ZH_XWC_XLIB_UNLOCK( wnd->dpy );
                  zh_gt_xwc_InvalidatePts( wnd, rx.left, rx.top, rx.right, rx.bottom );
               }
               else
                  zh_gt_xwc_InvalidatePart( wnd, rx.left / wnd->fontWidth,
                                                 rx.top / wnd->fontHeight,
                                                 rx.right / wnd->fontWidth,
                                                 rx.bottom / wnd->fontHeight );
               if( ZH_GTSELF_DISPCOUNT( pGT ) == 0 )
                  zh_gt_xwc_RealRefresh( wnd, ZH_FALSE );
            }

            if( xImage )
            {
               ZH_XWC_XLIB_LOCK( wnd->dpy );
               /* !NOT! use XDestroyImage(), char * xImage->data is [ eg hbfimage ] external managed */
               if( xImage->obdata )
                  XFree( xImage->obdata );
               XFree( xImage );
               ZH_XWC_XLIB_UNLOCK( wnd->dpy );
            }
         }
         break;

      default:
         return ZH_GTSUPER_INFO( pGT, iType, pInfo );
   }

   return ZH_TRUE;
}

#define zh_gfx_cord( t, l, b, r, tmp )    \
               do { \
                     if( l > r ) { tmp = r; r = l; l = tmp; } \
                     if( t > b ) { tmp = b; b = t; t = tmp; } \
               } while( 0 )

static int zh_gt_xwc_gfx_Primitive( PZH_GT pGT, int iType, int iTop, int iLeft, int iBottom, int iRight, int iColor )
{
   PXWND_DEF wnd;
   int iRet = 1, iTmp;
   XColor color;
   XImage * image;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_xwc_gfx_Primitive(%p,%d,%d,%d,%d,%d,%d)", ( void * ) pGT, iType, iTop, iLeft, iBottom, iRight, iColor ) );

   wnd = ZH_GTXWC_GET( pGT );
   if( ! wnd->fInit )
      zh_gt_xwc_RealRefresh( wnd, ZH_TRUE );
   ZH_GTSELF_REFRESH( pGT );

   switch( iType )
   {
      case ZH_GFX_ACQUIRESCREEN:
         /* TODO: */
         break;

      case ZH_GFX_RELEASESCREEN:
         /* TODO: */
         break;

      case ZH_GFX_MAKECOLOR:
         color.red = iTop * 256;
         color.green = iLeft * 256;
         color.blue = iBottom * 256;
         color.flags = DoRed | DoGreen | DoBlue;
         ZH_XWC_XLIB_LOCK( wnd->dpy );
         zh_gt_xwc_AllocColor( wnd, &color );
         ZH_XWC_XLIB_UNLOCK( wnd->dpy );
         iRet = color.pixel;
         break;

      case ZH_GFX_CLIPTOP:
         iRet = wnd->ClipRect.y;
         break;

      case ZH_GFX_CLIPLEFT:
         iRet = wnd->ClipRect.x;
         break;

      case ZH_GFX_CLIPBOTTOM:
         iRet = wnd->ClipRect.y + wnd->ClipRect.height - 1;
         break;

      case ZH_GFX_CLIPRIGHT:
         iRet = wnd->ClipRect.x + wnd->ClipRect.width - 1;
         break;

      case ZH_GFX_SETCLIP:
         wnd->ClipRect.y = iTop;
         wnd->ClipRect.x = iLeft;
         wnd->ClipRect.width = iBottom;
         wnd->ClipRect.height = iRight;
         ZH_XWC_XLIB_LOCK( wnd->dpy );
         XSetClipRectangles( wnd->dpy, wnd->gc, 0, 0, &wnd->ClipRect, 1, YXBanded );
         ZH_XWC_XLIB_UNLOCK( wnd->dpy );
         break;

      case ZH_GFX_DRAWINGMODE:
         iRet = ZH_GFX_MODE_SOLID;
         break;

      case ZH_GFX_GETPIXEL:
         ZH_XWC_XLIB_LOCK( wnd->dpy );
         image = XGetImage( wnd->dpy, wnd->drw, iLeft, iTop, 1, 1, AllPlanes, XYPixmap );
         if( image )
         {
            color.pixel = XGetPixel( image, 0, 0 );
            XQueryColor( wnd->dpy, wnd->colorsmap, &color );
            iRet = ( ( color.red >> 8 ) & 0xFF ) << 16 |
                   ( ( color.green >> 8 ) & 0xFF ) << 8 |
                   ( ( color.blue >> 8 ) & 0xFF );
            XDestroyImage( image );
         }
         else
            iRet = 0;
         ZH_XWC_XLIB_UNLOCK( wnd->dpy );
         break;

      case ZH_GFX_PUTPIXEL:
         ZH_XWC_XLIB_LOCK( wnd->dpy );
         XSetForeground( wnd->dpy, wnd->gc, iBottom );
         XDrawPoint( wnd->dpy, wnd->drw, wnd->gc, iLeft, iTop );
         ZH_XWC_XLIB_UNLOCK( wnd->dpy );
         zh_gt_xwc_InvalidatePts( wnd, iLeft, iTop, iLeft, iTop );
         break;

      case ZH_GFX_LINE:
         ZH_XWC_XLIB_LOCK( wnd->dpy );
         XSetForeground( wnd->dpy, wnd->gc, iColor );
         XDrawLine( wnd->dpy, wnd->drw, wnd->gc,
                    iLeft, iTop, iRight, iBottom );
         ZH_XWC_XLIB_UNLOCK( wnd->dpy );
         zh_gfx_cord( iTop, iLeft, iBottom, iRight, iTmp );
         zh_gt_xwc_InvalidatePts( wnd, iLeft, iTop, iRight, iBottom );
         break;

      case ZH_GFX_RECT:
         zh_gfx_cord( iTop, iLeft, iBottom, iRight, iTmp );
         ZH_XWC_XLIB_LOCK( wnd->dpy );
         XSetForeground( wnd->dpy, wnd->gc, iColor );
         XDrawRectangle( wnd->dpy, wnd->drw, wnd->gc,
                         iLeft, iTop, iRight - iLeft, iBottom - iTop );
         ZH_XWC_XLIB_UNLOCK( wnd->dpy );
         zh_gt_xwc_InvalidatePts( wnd, iLeft, iTop, iRight, iBottom );
         break;

      case ZH_GFX_FILLEDRECT:
         zh_gfx_cord( iTop, iLeft, iBottom, iRight, iTmp );
         ZH_XWC_XLIB_LOCK( wnd->dpy );
         XSetForeground( wnd->dpy, wnd->gc, iColor );
         XFillRectangle( wnd->dpy, wnd->drw, wnd->gc,
                         iLeft, iTop, iRight - iLeft, iBottom - iTop );
         ZH_XWC_XLIB_UNLOCK( wnd->dpy );
         zh_gt_xwc_InvalidatePts( wnd, iLeft, iTop, iRight, iBottom );
         break;

      case ZH_GFX_CIRCLE:
         iTop -= iBottom;
         iLeft -= iBottom;
         iBottom <<= 1;
         ZH_XWC_XLIB_LOCK( wnd->dpy );
         XSetForeground( wnd->dpy, wnd->gc, iRight );
         XDrawArc( wnd->dpy, wnd->drw, wnd->gc,
                   iLeft, iTop, iBottom, iBottom, 0, 360 * 64 );
         ZH_XWC_XLIB_UNLOCK( wnd->dpy );
         zh_gt_xwc_InvalidatePts( wnd, iLeft, iTop,
                                       iLeft + iBottom, iTop + iBottom );
         break;

      case ZH_GFX_FILLEDCIRCLE:
         iTop -= iBottom;
         iLeft -= iBottom;
         iBottom <<= 1;
         ZH_XWC_XLIB_LOCK( wnd->dpy );
         XSetForeground( wnd->dpy, wnd->gc, iRight );
         XFillArc( wnd->dpy, wnd->drw, wnd->gc,
                   iLeft, iTop, iBottom, iBottom, 0, 360 * 64 );
         ZH_XWC_XLIB_UNLOCK( wnd->dpy );
         zh_gt_xwc_InvalidatePts( wnd, iLeft, iTop,
                                       iLeft + iBottom, iTop + iBottom );
         break;

      case ZH_GFX_ELLIPSE:
         iTop -= iBottom;
         iLeft -= iRight;
         iBottom <<= 1;
         iRight <<= 1;
         ZH_XWC_XLIB_LOCK( wnd->dpy );
         XSetForeground( wnd->dpy, wnd->gc, iColor );
         XDrawArc( wnd->dpy, wnd->drw, wnd->gc,
                   iLeft, iTop, iRight, iBottom, 0, 360 * 64 );
         ZH_XWC_XLIB_UNLOCK( wnd->dpy );
         zh_gt_xwc_InvalidatePts( wnd, iLeft, iTop,
                                       iLeft + iRight, iTop + iBottom );
         break;

      case ZH_GFX_FILLEDELLIPSE:
         iTop -= iBottom;
         iLeft -= iRight;
         iBottom <<= 1;
         iRight <<= 1;
         ZH_XWC_XLIB_LOCK( wnd->dpy );
         XSetForeground( wnd->dpy, wnd->gc, iColor );
         XFillArc( wnd->dpy, wnd->drw, wnd->gc,
                   iLeft, iTop, iRight, iBottom, 0, 360 * 64 );
         ZH_XWC_XLIB_UNLOCK( wnd->dpy );
         zh_gt_xwc_InvalidatePts( wnd, iLeft, iTop,
                                       iLeft + iRight, iTop + iBottom );
         break;

      case ZH_GFX_FLOODFILL:
         /* TODO: */
         zh_gt_xwc_InvalidatePts( wnd, 0, 0, wnd->width, wnd->height );
         break;

      default:
         return ZH_GTSUPER_GFXPRIMITIVE( pGT, iType, iTop, iLeft, iBottom, iRight, iColor );
   }

   if( ZH_GTSELF_DISPCOUNT( pGT ) == 0 )
   {
      zh_gt_xwc_RealRefresh( wnd, ZH_FALSE );
   }

   return iRet;
}

/* *********************************************************************** */

static void zh_gt_xwc_Redraw( PZH_GT pGT, int iRow, int iCol, int iSize )
{
   PXWND_DEF wnd;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_xwc_Redraw(%p,%d,%d,%d)", ( void * ) pGT, iRow, iCol, iSize ) );

   wnd = ZH_GTXWC_GET( pGT );
   if( wnd && ! s_fNoXServer )
   {
      if( wnd->fInit )
      {
#if 1
         zh_gt_xwc_InvalidateChar( wnd, iCol, iRow, iCol + iSize - 1, iRow );
#else
         zh_gt_xwc_RepaintChar( wnd, iCol, iRow, iCol + iSize - 1, iRow );
         iCol *= wnd->fontWidth;
         iRow *= wnd->fontHeight;
         zh_gt_xwc_InvalidatePts( wnd, iCol, iRow,
                                  iCol + iSize * wnd->fontWidth - 1,
                                  iRow + wnd->fontHeight - 1 );
#endif
      }
#if 0
      else if( ! wnd->fData )
      {
         int iDefColor = ZH_GTSELF_GETCOLOR( pGT );
         int iColor;
         ZH_BYTE bAttr;
         ZH_USHORT usChar;

         while( iSize-- )
         {
            if( ! ZH_GTSELF_GETSCRCHAR( pGT, iRow, iCol++, &iColor, &bAttr, &usChar ) )
               break;
            if( iColor != iDefColor || usChar != ' ' )
            {
               wnd->fData = ZH_TRUE;
               break;
            }
         }
      }
#else
      else
         wnd->fData = ZH_TRUE;
#endif
   }
}

/* *********************************************************************** */

static void zh_gt_xwc_Refresh( PZH_GT pGT )
{
   PXWND_DEF wnd;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_xwc_Refresh(%p)", ( void * ) pGT ) );

   wnd = ZH_GTXWC_GET( pGT );
   ZH_GTSUPER_REFRESH( pGT );

   if( wnd && ! s_fNoXServer )
   {
      ZH_GTSELF_GETSCRCURSOR( pGT, &wnd->row, &wnd->col, &wnd->cursorType );

      if( wnd->fInit || wnd->fData )
         zh_gt_xwc_RealRefresh( wnd, ZH_FALSE );
   }
}

/* *********************************************************************** */

static ZH_BOOL zh_gt_FuncInit( PZH_GT_FUNCS pFuncTable )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_FuncInit(%p)", ( void * ) pFuncTable ) );

   pFuncTable->Init                       = zh_gt_xwc_Init;
   pFuncTable->Exit                       = zh_gt_xwc_Exit;
   pFuncTable->SetMode                    = zh_gt_xwc_SetMode;
   pFuncTable->Redraw                     = zh_gt_xwc_Redraw;
   pFuncTable->Refresh                    = zh_gt_xwc_Refresh;
   pFuncTable->GetBlink                   = zh_gt_xwc_GetBlink;
   pFuncTable->Version                    = zh_gt_xwc_Version;
   pFuncTable->Tone                       = zh_gt_xwc_Tone;
   pFuncTable->Info                       = zh_gt_xwc_Info;

   pFuncTable->ReadKey                    = zh_gt_xwc_ReadKey;

   pFuncTable->MouseIsPresent             = zh_gt_xwc_mouse_IsPresent;
   pFuncTable->MouseGetPos                = zh_gt_xwc_mouse_GetPos;
   pFuncTable->MouseSetPos                = zh_gt_xwc_mouse_SetPos;
   pFuncTable->MouseButtonState           = zh_gt_xwc_mouse_ButtonState;
   pFuncTable->MouseCountButton           = zh_gt_xwc_mouse_CountButton;

   pFuncTable->GfxPrimitive               = zh_gt_xwc_gfx_Primitive;

   return ZH_TRUE;
}


#include "../zh_gt_reg.h"
