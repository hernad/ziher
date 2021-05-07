/*
 * XWindow Console
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

#ifndef ZH_GTXWC_H
#define ZH_GTXWC_H

#define ZH_GT_NAME  XWC

#include "zh_api.h"
#include "../zh_gt_core.h"
#include "zh_init.h"
#include "zh_set.h"
#include "zh_vm.h"
#include "zh_item_api.h"
#include "zh_error_api.h"
#include "inkey.zhh"
#include "../gfx_def.zhh"
#include "zh_codepage_api.h"
#include "zh_string_api.h"
#include "zh_thread.h"
#include "zh_date.h"

#include <unistd.h>
#include <signal.h>
#include <sys/time.h>

#include <X11/Xlib.h>
#include <X11/Xcms.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>

/************************************************************/
/* Utility functions                                        */
typedef unsigned long ZH_GT_PIXELTYPE;
typedef ZH_USHORT ZH_GT_CELLTYPE;

#define XWC_CHAR_QUEUE_SIZE         128
#define XWC_CHAR_BUFFER             1024
#define XWC_MIN_ROWS                1
#define XWC_MIN_COLS                1
#define XWC_MAX_ROWS                256
#define XWC_MAX_COLS                256
#define XWC_DEFAULT_ROWS            25
#define XWC_DEFAULT_COLS            80
#define XWC_MAX_BUTTONS             8
#define XWC_MAX_CHAR_POINTS         1024
#define XWC_MAX_CHAR_RECTS          ( XWC_MAX_CHAR_POINTS / 2 )
#define XWC_MAX_CHAR_SEGS           ( XWC_MAX_CHAR_POINTS / 2 )

/* Font definition */
#define XWC_DEFAULT_FONT_HEIGHT     18
#define XWC_DEFAULT_FONT_WIDTH      9
#define XWC_DEFAULT_FONT_WEIGHT     ZH_GTI_FONTW_NORMAL
#define XWC_DEFAULT_FONT_NAME       "lucidatypewriter"
#define XWC_DEFAULT_FONT_ENCODING   "iso10646-1"

#define XWC_DEFAULT_FONT_FIXMETRIC  ZH_FALSE
#define XWC_DEFAULT_FONT_CLRBKG     ZH_FALSE
#define XWC_DEFAULT_FONT_DRAWBOX    ZH_TRUE


#define XWC_SYNC_UPDATE             0
#define XWC_ASYNC_UPDATE            1

#define XWC_STD_MASK                ( ExposureMask | StructureNotifyMask | FocusChangeMask | \
                                      ButtonPressMask | ButtonReleaseMask | PointerMotionMask | \
                                      KeyPressMask | KeyReleaseMask | VisibilityChangeMask )

/* Box char unicode values */
#define ZH_BOXCH_ARROW_R            0x0010 /* ARROW RIGHT */
#define ZH_BOXCH_ARROW_L            0x0011 /* ARROW LEFT */
#define ZH_BOXCH_ARROW_U            0x001E /* ARROW UP */
#define ZH_BOXCH_ARROW_D            0x001F /* ARROW DOWN */

#define ZH_BOXCH_CHR_BASE           4

#define ZH_BOXCH_DBL_LT             0x2554 /* BOX DRAWINGS DOUBLE DOWN AND RIGHT (Double left top angle) */
#define ZH_BOXCH_DBL_TD             0x2566 /* BOX DRAWINGS DOUBLE DOWN AND HORIZONTAL (Double top with junction down) */
#define ZH_BOXCH_DBL_RT             0x2557 /* BOX DRAWINGS DOUBLE DOWN AND LEFT (Double right top angle) */

#define ZH_BOXCH_DBL_LB             0x255A /* BOX DRAWINGS DOUBLE UP AND RIGHT (Double left bottom angle) */
#define ZH_BOXCH_DBL_BU             0x2569 /* BOX DRAWINGS DOUBLE UP AND HORIZONTAL (Double bottom with junction up) */
#define ZH_BOXCH_DBL_RB             0x255D /* BOX DRAWINGS DOUBLE DOWN AND LEFT (Double right bottom angle) */

#define ZH_BOXCH_DBL_VL             0x2560 /* BOX DRAWINGS DOUBLE VERTICAL AND RIGHT (Double Vertical with left junction) */
#define ZH_BOXCH_DBL_VR             0x2563 /* BOX DRAWINGS DOUBLE VERTICAL AND LEFT (Double vertical with right junction) */
#define ZH_BOXCH_DBL_CRS            0x256C /* BOX DRAWINGS DOUBLE VERTICAL AND HORIZONTAL (Double cross) */

#define ZH_BOXCH_DBL_HOR            0x2550 /* BOX DRAWINGS DOUBLE HORIZONTAL (Double Horizontal bar) */
#define ZH_BOXCH_DBL_VRT            0x2551 /* BOX DRAWINGS DOUBLE VERTICAL (Double Vertical bar) */

#define ZH_BOXCH_SNG_LT             0x250C /* BOX DRAWINGS LIGHT DOWN AND RIGHT (Single left top angle) */
#define ZH_BOXCH_SNG_TD             0x252C /* BOX DRAWINGS LIGHT DOWN AND HORIZONTAL (Single top with junction down) */
#define ZH_BOXCH_SNG_RT             0x2510 /* BOX DRAWINGS LIGHT DOWN AND LEFT (Single right top angle) */

#define ZH_BOXCH_SNG_LB             0x2514 /* BOX DRAWINGS LIGHT UP AND RIGHT (Single left bottom angle) */
#define ZH_BOXCH_SNG_BU             0x2534 /* BOX DRAWINGS LIGHT UP AND HORIZONTAL (Single bottom with junction up) */
#define ZH_BOXCH_SNG_RB             0x2518 /* BOX DRAWINGS LIGHT UP AND LEFT (Single right bottom angle) */

#define ZH_BOXCH_SNG_VL             0x251C /* BOX DRAWINGS LIGHT VERTICAL AND RIGHT (Single Vertical with left junction) */
#define ZH_BOXCH_SNG_VR             0x2524 /* BOX DRAWINGS LIGHT VERTICAL AND LEFT (Single vertical with right junction) */
#define ZH_BOXCH_SNG_CRS            0x253C /* BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL (Single cross) */

#define ZH_BOXCH_SNG_HOR            0x2500 /* BOX DRAWINGS LIGHT HORIZONTAL (Single Horizontal bar) */
#define ZH_BOXCH_SNG_VRT            0x2502 /* BOX DRAWINGS LIGHT VERTICAL (Single Vertical bar) */


#define ZH_BOXCH_SNG_L_DBL_T        0x2552 /* BOX DRAWINGS DOWN SINGLE AND RIGHT DOUBLE (Single left double top angle) */
#define ZH_BOXCH_SNG_T_DBL_D        0x2565 /* BOX DRAWINGS DOWN DOUBLE AND HORIZONTAL SINGLE (Single top with double junction down) */
#define ZH_BOXCH_SNG_R_DBL_T        0x2556 /* BOX DRAWINGS DOWN DOUBLE AND LEFT SINGLE (Single right double top angle) */

#define ZH_BOXCH_SNG_L_DBL_B        0x2558 /* BOX DRAWINGS UP SINGLE AND RIGHT DOUBLE (Single left double bottom angle) */
#define ZH_BOXCH_SNG_B_DBL_U        0x2568 /* BOX DRAWINGS UP DOUBLE AND HORIZONTAL SINGLE (Single bottom double with junction up) */
#define ZH_BOXCH_SNG_R_DBL_B        0x255C /* BOX DRAWINGS UP DOUBLE AND LEFT SINGLE (Single right double bottom angle) */

#define ZH_BOXCH_SNG_V_DBL_L        0x255E /* BOX DRAWINGS VERTICAL SINGLE AND RIGHT DOUBLE (Single Vertical double left junction) */
#define ZH_BOXCH_SNG_V_DBL_R        0x2561 /* BOX DRAWINGS VERTICAL SINGLE AND LEFT DOUBLE (Single vertical double right junction) */
#define ZH_BOXCH_SNG_DBL_CRS        0x256A /* BOX DRAWINGS VERTICAL SINGLE AND HORIZONTAL DOUBLE (Single cross (double horiz) */


#define ZH_BOXCH_DBL_L_SNG_T        0x2553 /* BOX DRAWINGS DOWN DOUBLE AND RIGHT SINGLE (Double left single top angle) */
#define ZH_BOXCH_DBL_T_SNG_D        0x2564 /* BOX DRAWINGS DOWN SINGLE AND HORIZONTAL DOUBLE (Double top signle junction down) */
#define ZH_BOXCH_DBL_R_SNG_T        0x2555 /* BOX DRAWINGS DOWN SINGLE AND LEFT DOUBLE (Double right single top angle) */

#define ZH_BOXCH_DBL_L_SNG_B        0x2559 /* BOX DRAWINGS UP DOUBLE AND RIGHT SINGLE (Double left single bottom angle) */
#define ZH_BOXCH_DBL_B_SNG_U        0x2567 /* BOX DRAWINGS UP SINGLE AND HORIZONTAL DOUBLE (Double bottom single junction up) */
#define ZH_BOXCH_DBL_R_SNG_B        0x255B /* BOX DRAWINGS UP SINGLE AND LEFT DOUBLE (Double right single bottom angle) */

#define ZH_BOXCH_DBL_V_SNG_R        0x2562 /* BOX DRAWINGS VERTICAL DOUBLE AND LEFT SINGLE (Double Vertical single left junction) */
#define ZH_BOXCH_DBL_V_SNG_L        0x255F /* BOX DRAWINGS VERTICAL DOUBLE AND RIGHT SINGLE (Double vertical single right junction) */
#define ZH_BOXCH_DBL_SNG_CRS        0x256B /* BOX DRAWINGS VERTICAL DOUBLE AND HORIZONTAL SINGLE (Double cross (single horiz) */

#define ZH_BOXCH_FULL               0x2588 /* FULL BLOCK */
#define ZH_BOXCH_FULL_B             0x2584 /* LOWER HALF BLOCK */
#define ZH_BOXCH_FULL_L             0x258C /* LEFT HALF BLOCK */
#define ZH_BOXCH_FULL_R             0x2590 /* RIGHT HALF BLOCK */
#define ZH_BOXCH_FULL_T             0x2580 /* UPPER HALF BLOCK */

#define ZH_BOXCH_FILLER1            0x2591 /* LIGHT SHADE */
#define ZH_BOXCH_FILLER2            0x2592 /* MEDIUM SHADE */
#define ZH_BOXCH_FILLER3            0x2593 /* DARK SHADE */

#define ZH_BOXCH_SQUARE             0x25A0 /* BLACK SQUARE */

#define ZH_BOXCH_BOX_MIN            0x2500
#define ZH_BOXCH_BOX_MAX            0x25A0

#define ZH_BOXCH_RC_ARROW_DL        0xE040 /* USER ARROW DOWN (LEFT) */
#define ZH_BOXCH_RC_ARROW_DR        0xE041 /* USER ARROW DOWN (RIGHT) */
#define ZH_BOXCH_RC_ARROW_UL        0xE042 /* USER ARROW UP (LEFT) */
#define ZH_BOXCH_RC_ARROW_UR        0xE043 /* USER ARROW UP (RIGHT) */
#define ZH_BOXCH_RC_ARROW_VL        0xE044 /* USER ARROWS UP/DOWN (LEFT) */
#define ZH_BOXCH_RC_ARROW_VR        0xE045 /* USER ARROWS UP/DOWN (RIGHT) */
#define ZH_BOXCH_RC_BUTTON_L        0xE046 /* USER BUTTON (LEFT) */
#define ZH_BOXCH_RC_BUTTON_R        0xE047 /* USER BUTTON (RIGHT) */
#define ZH_BOXCH_RC_ARROW_LL        0xE048 /* USER ARROW LEFT (LEFT) */
#define ZH_BOXCH_RC_ARROW_LR        0xE049 /* USER ARROW LEFT (RIGHT) */
#define ZH_BOXCH_RC_ARROW_RL        0xE04A /* USER ARROW RIGHT (LEFT) */
#define ZH_BOXCH_RC_ARROW_RR        0xE04B /* USER ARROW RIGHT (RIGHT) */
#define ZH_BOXCH_RC_ENTER1          0xE04C /* USER ENTER (LEFT) */
#define ZH_BOXCH_RC_ENTER2          0xE04D /* USER ENTER (MIDDLE) */
#define ZH_BOXCH_RC_ENTER3          0xE04E /* USER ENTER (RIGHT) */
#define ZH_BOXCH_RC_VSCRL_LD        0xE04F /* USER VERTICAL SCROLL (LEFT-DOWN) */
#define ZH_BOXCH_RC_VSCRL_RD        0xE050 /* USER VERTICAL SCROLL (RIGHT-DOWN) */
#define ZH_BOXCH_RC_VSCRL_LU        0xE051 /* USER VERTICAL SCROLL (LEFT-UP) */
#define ZH_BOXCH_RC_VSCRL_RU        0xE052 /* USER VERTICAL SCROLL (RIGHT-UP) */
#define ZH_BOXCH_RC_VSCRL_L         0xE053 /* USER VERTICAL SCROLL (LEFT) */
#define ZH_BOXCH_RC_VSCRL_R         0xE054 /* USER VERTICAL SCROLL (RIGHT) */
#define ZH_BOXCH_RC_HSCRL           0xE055 /* USER HORIZONTAL SCROLL */

#define ZH_BOXCH_RC_0               0xE056 /* USER BIG 0 */
#define ZH_BOXCH_RC_1               0xE057 /* USER BIG 1 */
#define ZH_BOXCH_RC_2               0xE058 /* USER BIG 2 */
#define ZH_BOXCH_RC_3               0xE059 /* USER BIG 3 */
#define ZH_BOXCH_RC_4               0xE05A /* USER BIG 4 */
#define ZH_BOXCH_RC_5               0xE05B /* USER BIG 5 */
#define ZH_BOXCH_RC_6               0xE05C /* USER BIG 6 */
#define ZH_BOXCH_RC_7               0xE05D /* USER BIG 7 */
#define ZH_BOXCH_RC_8               0xE05E /* USER BIG 8 */
#define ZH_BOXCH_RC_9               0xE05F /* USER BIG 9 */
#define ZH_BOXCH_RC_DOT             0xE060 /* USER BIG DOT */
#define ZH_BOXCH_RC_ACC             0xE061 /* USER BIG ACCENT */

#define ZH_BOXCH_RC_BOX_ML          0xE062 /* USER BOX MIDDLE LEFT */
#define ZH_BOXCH_RC_BOX_MR          0xE063 /* USER BOX MIDDLE RIGHT */
#define ZH_BOXCH_RC_HWND_L          0xE064 /* USER WINDOW HANDLE LEFT */
#define ZH_BOXCH_RC_HWND_R          0xE065 /* USER WINDOW HANDLE RIGHT */
#define ZH_BOXCH_RC_BOX_TL          0xE066 /* USER BOX TOP LEFT */
#define ZH_BOXCH_RC_BOX_T           0xE067 /* USER BOX TOP */
#define ZH_BOXCH_RC_BOX_TR          0xE068 /* USER BOX TOP RIGHT */
#define ZH_BOXCH_RC_BOX_R           0xE069 /* USER BOX RIGHT */
#define ZH_BOXCH_RC_BOX_BR          0xE06A /* USER BOX BOTTOM RIGHT */
#define ZH_BOXCH_RC_BOX_B           0xE06B /* USER BOX BOTTOM */
#define ZH_BOXCH_RC_BOX_BL          0xE06C /* USER BOX BOTTOM LEFT */
#define ZH_BOXCH_RC_BOX_L           0xE06D /* USER BOX LEFT */
#define ZH_BOXCH_RC_BOX_MT          0xE06E /* USER BOX MIDDLE TOP */
#define ZH_BOXCH_RC_BOX_MB          0xE06F /* USER BOX MIDDLE BOTTOM */

#define ZH_BOXCH_RC_BUTTON_CL       0xE070 /* USER BUTTON CHECK (LEFT) */
#define ZH_BOXCH_RC_BUTTON_CR       0xE071 /* USER BUTTON CHECK (RIGHT) */

#define ZH_BOXCH_RC_FARROW_DL       0xE072 /* USER FAT ARROW DOWN (LEFT) */
#define ZH_BOXCH_RC_FARROW_DR       0xE073 /* USER FAT ARROW DOWN (RIGHT) */

#define ZH_BOXCH_RC_DOTS            0xE074 /* USER DOTS */
#define ZH_BOXCH_RC_DOTS_L          0xE075 /* USER BIG DOTS (LEFT) */
#define ZH_BOXCH_RC_DOTS_R          0xE076 /* USER BIG DOTS (RIGHT) */

#define ZH_BOXCH_RC_MIN             0xE040
#define ZH_BOXCH_RC_MAX             0xE076

#define ZH_BOXCH_TRANS_COUNT        ( ZH_BOXCH_CHR_BASE + ( ZH_BOXCH_BOX_MAX - ZH_BOXCH_BOX_MIN + 1 ) + ( ZH_BOXCH_RC_MAX - ZH_BOXCH_RC_MIN + 1 ) )
#define ZH_BOXCH_TRANS_MAX          0xFF

/********************** Unix to graphic box translation ******************/

typedef enum
{
   CH_UNDEF,         /* undefined */
   CH_CHAR,          /* normal U16 character */
   CH_CHBX,          /* U16 character built by DrawBoxChar */
   CH_NONE,          /* no character share */
   CH_IMG,           /* character built from image */
   CH_PTS,           /* character built from relative points */
   CH_LINE,          /* character built from relative lines */
   CH_SEG,           /* character built from lines (segments) */
   CH_RECT,          /* character built from rectangles */
   CH_POLY           /* character built by polygon */
} XWC_CharType;

typedef struct
{
   XWC_CharType type;
   union
   {
      XImage *     img;
      XPoint *     pts;
      XSegment *   seg;
      XRectangle * rect;
      ZH_USHORT    ch16;
   } u;
   ZH_BYTE size;
   ZH_BOOL inverse;
} XWC_CharTrans;

#endif
