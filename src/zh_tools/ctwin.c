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

/* NOTE: User programs should never call this layer directly! */

/* This definition has to be placed before #include "zh_gt_api.h" */
#define ZH_GT_NAME  CTW

#include "zh_api.h"
#include "zh_gt_core.h"
#include "zh_stack.h"
#include "zh_init.h"
#include "zh_item_api.h"
#include "zh_string_api.h"

#include "ctwin.h"

static int s_GtId;
#undef  ZH_GTSUPERTABLE
#define ZH_GTSUPERTABLE( g )       ( &( ZH_GTCTW_GET( g )->SuperTable ) )
#define ZH_GTID_PTR         ( &s_GtId )

#define ZH_GTCTW_GET( p )          ( ( PZH_GTCTW ) ZH_GTLOCAL( p ) )
#define ZH_CTW_TSD( p )            ( ( PZH_CTWDATA ) zh_stackGetTSD( &( p )->TSD ) )

#define ZH_CTW_GETCURRENT( p )     zh_ctw_CurrentWindow( p )
#define ZH_CTW_SETCURRENT( p, n )  ( ZH_CTW_TSD( p )->iCurrWindow = ( n ) )

#define ZH_CTWIN_ALLOC      16

#define ZH_CTW_SHADOW_MASK  0x8000000

typedef struct
{
   int iCurrWindow;
}
ZH_CTWDATA, * PZH_CTWDATA;

typedef struct
{
   int iHandle;

   ZH_BOOL fHidden;
   int iLevel;

   int iShadowAttr;
   int iCursorStyle;

   int iRow;
   int iCol;

   int iTopMargin;
   int iLeftMargin;
   int iBottomMargin;
   int iRightMargin;

   ZH_BOOL fClip;
   int iCliTop;
   int iCliLeft;
   int iCliBottom;
   int iCliRight;

   int iHeight;
   int iWidth;

   int iFirstRow;
   int iFirstCol;

   int iColorIndex;
   int iColorCount;
   int * piColors;

   PZH_SCREENCELL screenBuffer;

} ZH_CT_WND, * PZH_CT_WND;

typedef struct
{
   PZH_GT      pGT;
   ZH_GT_FUNCS SuperTable;

   ZH_TSD TSD;

   int iShadowWidth;
   int iShadowAttr;

   int iOpenWindows;
   int iMaxWindow;

   int fBoardSet;
   int iBoardTop;
   int iBoardLeft;
   int iBoardBottom;
   int iBoardRight;

   int fBoardTop;
   int fBoardLeft;
   int fBoardBottom;
   int fBoardRight;

   int iMoveMode;
   int iVerticalStep;
   int iHorizontalStep;

   PZH_CT_WND * windows;
   int * windowStack;
   int * pWindowMap;
   int * pShadowMap;
   int iMapWidth;
   int iMapHeight;

   int iLastKey;

} ZH_GTCTW, * PZH_GTCTW;

static const ZH_WCHAR sc_szFrameW[] = ZH_B_SINGLE_W;

static int zh_ctw_CalcShadowWidth( int iRows, int iCols )
{
   if( iRows + iRows >= iCols )
      return 1;
   else
      return 2;
}

static void zh_ctw_SetMap( PZH_GTCTW pCTW, int * piMap, int iWindow, int iTop, int iLeft, int iBottom, int iRight, int iNested )
{
   ZH_SIZE nIndex;
   int i;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_SetMap(%p,%p,%d,%d,%d,%d,%d,%d)", ( void * ) pCTW, ( void * ) piMap, iWindow, iTop, iLeft, iBottom, iRight, iNested ) );

   if( iTop < 0 )
      iTop = 0;
   if( iBottom >= pCTW->iMapHeight )
      iBottom = pCTW->iMapHeight - 1;
   if( iLeft < 0 )
      iLeft = 0;
   if( iRight >= pCTW->iMapWidth )
      iRight = pCTW->iMapWidth - 1;

   if( iNested == 0 )
   {
      while( iTop <= iBottom )
      {
         nIndex = iTop * pCTW->iMapWidth + iLeft;
         for( i = iLeft; i <= iRight; ++i, ++nIndex )
            piMap[ nIndex ] = iWindow;
         ++iTop;
      }
   }
   else
   {
      while( iTop <= iBottom )
      {
         nIndex = iTop * pCTW->iMapWidth + iLeft;
         for( i = iLeft; i <= iRight; ++i, ++nIndex )
            piMap[ nIndex ] = iWindow |
                     ( ( piMap[ nIndex ] != 0 && piMap[ nIndex ] != iWindow ) ? iNested : 0 );
         ++iTop;
      }
   }
}

static void zh_ctw_ClearMap( PZH_GTCTW pCTW )
{
   ZH_SIZE nSize;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_ClearMap(%p)", ( void * ) pCTW ) );

   nSize = ( ZH_SIZE ) pCTW->iMapHeight * pCTW->iMapWidth * sizeof( int );
   memset( pCTW->pWindowMap, 0, nSize );
   memset( pCTW->pShadowMap, 0, nSize );
}

static void zh_ctw_TouchLines( PZH_GTCTW pCTW, int iFrom, int iTo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_TouchLines(%p,%d,%d)", ( void * ) pCTW, iFrom, iTo ) );

   while( iFrom <= iTo )
   {
      ZH_GTSELF_TOUCHLINE( pCTW->pGT, iFrom );
      ++iFrom;
   }
}

static void zh_ctw_WindowMap( PZH_GTCTW pCTW, int iWindow, ZH_BOOL fExpose )
{
   PZH_CT_WND pWnd;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_WindowMap(%p,%d,%d)", ( void * ) pCTW, iWindow, ( int ) fExpose ) );

   pWnd = pCTW->windows[ iWindow ];

   if( ! pWnd->fHidden )
   {
      int iLastRow = pWnd->iFirstRow + pWnd->iHeight - 1,
          iLastCol = pWnd->iFirstCol + pWnd->iWidth - 1;

      zh_ctw_SetMap( pCTW, pCTW->pWindowMap, iWindow,
                     pWnd->iFirstRow, pWnd->iFirstCol,
                     iLastRow, iLastCol, 0 );
      zh_ctw_SetMap( pCTW, pCTW->pShadowMap, 0,
                     pWnd->iFirstRow, pWnd->iFirstCol,
                     iLastRow, iLastCol, 0 );
      if( pWnd->iShadowAttr != ZH_CTW_SHADOW_OFF &&
          iLastRow >= pCTW->iBoardTop && iLastCol >= pCTW->iBoardLeft &&
          pWnd->iFirstRow <= pCTW->iBoardBottom && pWnd->iFirstCol <= pCTW->iBoardRight )
      {
         iLastRow += 1;
         iLastCol += pCTW->iShadowWidth;
         zh_ctw_SetMap( pCTW, pCTW->pShadowMap, iWindow,
                        iLastRow, pWnd->iFirstCol + pCTW->iShadowWidth,
                        iLastRow, iLastCol,
                        pWnd->iShadowAttr == ZH_CTW_SHADOW_EXT2 ? ZH_CTW_SHADOW_MASK : 0 );
         zh_ctw_SetMap( pCTW, pCTW->pShadowMap, iWindow,
                        pWnd->iFirstRow + 1, pWnd->iFirstCol + pWnd->iWidth,
                        iLastRow - 1, iLastCol,
                        pWnd->iShadowAttr == ZH_CTW_SHADOW_EXT2 ? ZH_CTW_SHADOW_MASK : 0 );
      }
      if( fExpose )
         zh_ctw_TouchLines( pCTW, pWnd->iFirstRow, iLastRow );
   }
}

static void zh_ctw_RemapAllWindows( PZH_GTCTW pCTW, int iFrom, ZH_BOOL fExpose )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_RemapAllWindows(%p,%d,%d)", ( void * ) pCTW, iFrom, ( int ) fExpose ) );

   if( pCTW->iMaxWindow )
   {
      int i;

      if( iFrom == 0 )
         zh_ctw_ClearMap( pCTW );
      for( i = iFrom; i < pCTW->iOpenWindows; ++i )
         zh_ctw_WindowMap( pCTW, pCTW->windowStack[ i ], ZH_FALSE );
      if( fExpose )
         zh_ctw_TouchLines( pCTW, 0, pCTW->iMapHeight );
   }
}

static int zh_ctw_SetShadowAttr( PZH_GTCTW pCTW, int iAttr )
{
   int iOldAttr;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_SetShadowAttr(%p,%d)", ( void * ) pCTW, iAttr ) );

   iOldAttr = pCTW->iShadowAttr;
   if( iAttr >= 0 ||
       iAttr == ZH_CTW_SHADOW_OFF ||
       iAttr == ZH_CTW_SHADOW_EXT ||
       iAttr == ZH_CTW_SHADOW_EXT2 )
      pCTW->iShadowAttr = iAttr;

   return iOldAttr;
}

static int zh_ctw_SetMoveMode( PZH_GTCTW pCTW, int iMode )
{
   int iOldMode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_SetMoveMode(%p,%d)", ( void * ) pCTW, iMode ) );

   iOldMode = pCTW->iMoveMode;
   if( iMode >= 0 )
      pCTW->iMoveMode = iMode;

   return iOldMode;
}

static int zh_ctw_SetMoveStep( PZH_GTCTW pCTW, int iVertical, int iHorizontal )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_SetMoveStep(%p,%d,%d)", ( void * ) pCTW, iVertical, iHorizontal ) );

   if( iVertical < pCTW->iMapHeight && iHorizontal < pCTW->iMapWidth )
   {
      pCTW->iVerticalStep   = iVertical;
      pCTW->iHorizontalStep = iHorizontal;

      return 0;
   }

   return -1;
}

static int zh_ctw_SetWindowBoard( PZH_GTCTW pCTW, int iTop, int iLeft, int iBottom, int iRight )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_SetWindowBoard(%p,%d,%d,%d,%d)", ( void * ) pCTW, iTop, iLeft, iBottom, iRight ) );

   if( iTop >= 0 && iLeft >= 0 && iTop < iBottom && iLeft < iRight )
   {
      pCTW->iBoardTop    = iTop;
      pCTW->iBoardLeft   = iLeft;
      pCTW->iBoardBottom = iBottom;
      pCTW->iBoardRight  = iRight;
      pCTW->fBoardSet    = ZH_TRUE;
      zh_ctw_RemapAllWindows( pCTW, 0, ZH_TRUE );

      return 0;
   }

   return -1;
}

static int  zh_ctw_SetBorderMode( PZH_GTCTW pCTW, int iTop, int iLeft, int iBottom, int iRight )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_SetBorderMode(%p,%d,%d,%d,%d)", ( void * ) pCTW, iTop, iLeft, iBottom, iRight ) );

   if( iTop >= 0 )
      pCTW->fBoardTop     = iTop != 0;
   if( iLeft >= 0 )
      pCTW->fBoardLeft    = iLeft != 0;
   if( iBottom >= 0 )
      pCTW->fBoardBottom  = iBottom != 0;
   if( iRight >= 0 )
      pCTW->fBoardRight   = iRight != 0;

   return 0;
}

static int zh_ctw_CurrentWindow( PZH_GTCTW pCTW )
{
   PZH_CTWDATA pTSD;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_CurrentWindow(%p)", ( void * ) pCTW ) );

   pTSD = ZH_CTW_TSD( pCTW );

   /* because other threads can close current window we need additional
    * protection here and we have to check if current handle is still
    * valid [druzus]
    */
   if( pTSD->iCurrWindow > 0 )
   {
      if( pTSD->iCurrWindow > pCTW->iMaxWindow ||
          pCTW->windows[ pTSD->iCurrWindow ] == NULL )
         pTSD->iCurrWindow = pCTW->iOpenWindows > 0 ?
                             pCTW->windowStack[ pCTW->iOpenWindows - 1 ] : 0;
   }

   return pTSD->iCurrWindow;
}

static int zh_ctw_SelectWindow( PZH_GTCTW pCTW, int iWindow, ZH_BOOL fToTop )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_SelectWindow(%p,%d,%d)", ( void * ) pCTW, iWindow, fToTop ) );

   if( iWindow == 0 )
      ZH_CTW_SETCURRENT( pCTW, 0 );
   else if( iWindow > 0 && iWindow <= pCTW->iMaxWindow &&
            pCTW->windows[ iWindow ] != NULL )
   {
      ZH_CTW_SETCURRENT( pCTW, iWindow );
      if( fToTop )
      {
         int i;

         /* update window level */
         i = pCTW->iOpenWindows - 1;
         while( i >= 0 )
         {
            if( pCTW->windowStack[ i ] == iWindow )
            {
               int iPos = i;
               while( i < pCTW->iOpenWindows - 1 &&
                      pCTW->windows[ pCTW->windowStack[ i + 1 ] ]->iLevel <=
                      pCTW->windows[ iWindow ]->iLevel )
               {
                  pCTW->windowStack[ i ] = pCTW->windowStack[ i + 1 ];
                  ++i;
               }
               pCTW->windowStack[ i ] = iWindow;

               if( iPos != i && ! pCTW->windows[ iWindow ]->fHidden )
               {
                  /* INFO: CT effectively calls zh_ctw_RemapAllWindows() here */
                  if( i < pCTW->iOpenWindows - 1 )
                     zh_ctw_RemapAllWindows( pCTW, i, ZH_TRUE );
                  else
                     zh_ctw_WindowMap( pCTW, iWindow, ZH_TRUE );
               }
               break;
            }
            --i;
         }
      }
   }
   else
      iWindow = ZH_CTW_GETCURRENT( pCTW );

   return iWindow;
}

static int zh_ctw_ChangeWindowHandle( PZH_GTCTW pCTW, int iNewWindow )
{
   int iWindow;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_ChangeWindowHandle(%p,%d)", ( void * ) pCTW, iNewWindow ) );

   iWindow = ZH_CTW_GETCURRENT( pCTW );
   if( iWindow != iNewWindow )
   {
      if( iWindow > 0 && iNewWindow > 0 && iNewWindow <= 255 &&
          ( iNewWindow > pCTW->iMaxWindow ||
            pCTW->windows[ iNewWindow ] == NULL ) &&
          pCTW->windows[ iWindow ] )
      {
         PZH_CT_WND pWnd = pCTW->windows[ iWindow ];
         int i;

         if( iNewWindow > pCTW->iMaxWindow )
         {
            i = pCTW->iMaxWindow;
            while( iNewWindow > pCTW->iMaxWindow )
               pCTW->iMaxWindow += ZH_CTWIN_ALLOC;
            pCTW->windows = ( PZH_CT_WND * ) zh_xrealloc( pCTW->windows, ( pCTW->iMaxWindow + 1 ) * sizeof( PZH_CT_WND ) );
            pCTW->windowStack = ( int * ) zh_xrealloc( pCTW->windowStack, pCTW->iMaxWindow * sizeof( int ) );
            do
            {
               pCTW->windows[ i + 1 ] = NULL;
               pCTW->windowStack[ i ] = 0;
            }
            while( ++i < pCTW->iMaxWindow );
         }
         pWnd->iHandle = iNewWindow;
         pCTW->windows[ iWindow ] = NULL;
         pCTW->windows[ iNewWindow ] = pWnd;

         i = pCTW->iOpenWindows - 1;
         while( i >= 0 && pCTW->windowStack[ i ] != iWindow )
            --i;
         if( i >= 0 )
         {
            pCTW->windowStack[ i ] = iNewWindow;
            if( ! pWnd->fHidden )
            {
               if( pWnd->iShadowAttr == ZH_CTW_SHADOW_EXT2 )
                  i = 0;
               zh_ctw_RemapAllWindows( pCTW, i, ZH_FALSE );
            }
         }
      }
      else
         iNewWindow = -1;
   }
   return iNewWindow;
}

static int zh_ctw_GetWindowStack( PZH_GTCTW pCTW, const int ** piStack )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_GetWindowStack(%p,%p)", ( void * ) pCTW, ( const void * ) piStack ) );

   *piStack = pCTW->windowStack;

   return pCTW->iOpenWindows;
}

static int zh_ctw_Visible( PZH_GTCTW pCTW, int iWindow, int iVisible )
{
   int iResult = ZH_CTW_UNDEF;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_Visible(%p,%d,%d)", ( void * ) pCTW, iWindow, iVisible ) );

   if( iWindow == 0 )
      iResult = ZH_CTW_VISIBLE;
   else if( iWindow > 0 && iWindow <= pCTW->iMaxWindow &&
            pCTW->windows[ iWindow ] != NULL )
   {
      PZH_CT_WND pWnd = pCTW->windows[ iWindow ];

      iResult = pWnd->fHidden ? ZH_CTW_HIDDEN : ZH_CTW_VISIBLE;
      if( iVisible != ZH_CTW_UNDEF &&
          pWnd->fHidden != ( iVisible == ZH_CTW_HIDDEN ) )
      {
         pWnd->fHidden = ( iVisible == ZH_CTW_HIDDEN );
         zh_ctw_RemapAllWindows( pCTW, 0, ZH_TRUE );
      }
   }

   return iResult;
}

static int zh_ctw_SetWindowLevel( PZH_GTCTW pCTW, int iWindow, int iLevel )
{
   int iResult = -1;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_SetWindowLevel(%p,%d,%d)", ( void * ) pCTW, iWindow, iLevel ) );

   if( iWindow > 0 && iWindow <= pCTW->iMaxWindow &&
       pCTW->windows[ iWindow ] != NULL )
   {
      PZH_CT_WND pWnd = pCTW->windows[ iWindow ];

      iResult = pWnd->iLevel;
      if( iLevel >= ZH_CTW_BOTTOM && iLevel <= ZH_CTW_TOP &&
          pWnd->iLevel != iLevel )
      {
         ZH_BOOL fToTop;
         int i;

         /* update window level */
         fToTop = pWnd->iLevel < iLevel;
         pWnd->iLevel = iLevel;

         i = pCTW->iOpenWindows - 1;
         if( i > 0 )
         {
            while( i >= 0 && pCTW->windowStack[ i ] != iWindow )
               --i;
            if( i >= 0 )
            {
               int iPos = i;
               if( fToTop )
               {
                  while( i < pCTW->iOpenWindows - 1 && pWnd->iLevel >=
                         pCTW->windows[ pCTW->windowStack[ i + 1 ] ]->iLevel )
                  {
                     pCTW->windowStack[ i ] = pCTW->windowStack[ i + 1 ];
                     ++i;
                  }
                  pCTW->windowStack[ i ] = iWindow;
               }
               else
               {
                  while( i > 0 && pWnd->iLevel <=
                         pCTW->windows[ pCTW->windowStack[ i - 1 ] ]->iLevel )
                  {
                     pCTW->windowStack[ i ] = pCTW->windowStack[ i - 1 ];
                     --i;
                  }
                  pCTW->windowStack[ i ] = iWindow;
               }
               if( ! pWnd->fHidden && iPos != i )
                  zh_ctw_RemapAllWindows( pCTW, ZH_MIN( iPos, i ), ZH_TRUE );
            }
         }
      }
   }

   return iResult;
}

static int zh_ctw_SetWindowShadow( PZH_GTCTW pCTW, int iWindow, int iAttr )
{
   int iResult = -1;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_SetWindowShadow(%p,%d,%d)", ( void * ) pCTW, iWindow, iAttr ) );

   if( iWindow > 0 && iWindow <= pCTW->iMaxWindow &&
       pCTW->windows[ iWindow ] != NULL )
   {
      PZH_CT_WND pWnd = pCTW->windows[ iWindow ];

      iResult = pWnd->iShadowAttr;
      if( ( iAttr >= 0 ||
            iAttr == ZH_CTW_SHADOW_OFF ||
            iAttr == ZH_CTW_SHADOW_EXT ||
            iAttr == ZH_CTW_SHADOW_EXT2 ) &&
          pWnd->iShadowAttr != iAttr )
      {
         pWnd->iShadowAttr = iAttr;
         if( ! pWnd->fHidden )
            zh_ctw_RemapAllWindows( pCTW, 0, ZH_TRUE );
      }
   }

   return iResult;
}

static int zh_ctw_MaxWindow( PZH_GTCTW pCTW )
{
   int i, iMaxHandle = 0;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_MaxWindow(%p)", ( void * ) pCTW ) );

   for( i = 0; i < pCTW->iOpenWindows; ++i )
   {
      if( iMaxHandle < pCTW->windowStack[ i ] )
         iMaxHandle = pCTW->windowStack[ i ];
   }

   return iMaxHandle;
}

static int zh_ctw_CreateWindow( PZH_GTCTW pCTW, int iTop, int iLeft, int iBottom, int iRight, ZH_BOOL fClear, int iColor, ZH_BOOL fVisible )
{
   PZH_CT_WND pWnd;
   ZH_BYTE bAttr;
   ZH_USHORT usChar;
   int iRow, iCol, iHeight, iWidth, iTmp;
   long lIndex;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_CreateWindow(%p,%d,%d,%d,%d,%d,%d,%d)", ( void * ) pCTW, iTop, iLeft, iBottom, iRight, ( int ) fClear, iColor, ( int ) fVisible ) );

   if( pCTW->iOpenWindows == pCTW->iMaxWindow )
   {
      int i = pCTW->iMaxWindow;

      if( pCTW->iMaxWindow == 0 )
      {
         ZH_SIZE nSize;

         ZH_GTSELF_GETSIZE( pCTW->pGT, &pCTW->iMapHeight, &pCTW->iMapWidth );
         pCTW->iShadowWidth = zh_ctw_CalcShadowWidth( pCTW->iMapHeight, pCTW->iMapWidth );
         if( ! pCTW->fBoardSet )
            zh_ctw_SetWindowBoard( pCTW, 0, 0, pCTW->iMapHeight - 1, pCTW->iMapWidth - 1 );
         nSize = ( ZH_SIZE ) pCTW->iMapHeight * pCTW->iMapWidth * sizeof( int );
         pCTW->pWindowMap = ( int * ) zh_xgrab( nSize );
         pCTW->pShadowMap = ( int * ) zh_xgrab( nSize );
         zh_ctw_ClearMap( pCTW );

         pCTW->iMaxWindow = ZH_CTWIN_ALLOC;
         pCTW->windows = ( PZH_CT_WND * ) zh_xgrab( ( ZH_CTWIN_ALLOC + 1 ) * sizeof( PZH_CT_WND ) );
         pCTW->windowStack = ( int * ) zh_xgrab( ZH_CTWIN_ALLOC * sizeof( int ) );
         pCTW->windows[ 0 ] = NULL;
      }
      else
      {
         pCTW->iMaxWindow += ZH_CTWIN_ALLOC;
         pCTW->windows = ( PZH_CT_WND * ) zh_xrealloc( pCTW->windows, ( pCTW->iMaxWindow + 1 ) * sizeof( PZH_CT_WND ) );
         pCTW->windowStack = ( int * ) zh_xrealloc( pCTW->windowStack, pCTW->iMaxWindow * sizeof( int ) );
      }
      do
      {
         pCTW->windows[ i + 1 ] = NULL;
         pCTW->windowStack[ i ] = 0;
      }
      while( ++i < pCTW->iMaxWindow );
   }

   iHeight = iBottom - iTop + 1;
   iWidth  = iRight - iLeft + 1;
   iRow = iTop;
   iCol = iLeft;

   if( iHeight > pCTW->iBoardBottom - pCTW->iBoardTop + 1 )
      iHeight = pCTW->iBoardBottom - pCTW->iBoardTop + 1;
   if( iWidth > pCTW->iBoardRight - pCTW->iBoardLeft + 1 )
      iWidth = pCTW->iBoardRight - pCTW->iBoardLeft + 1;


   iTop    = pCTW->iBoardTop - ( pCTW->fBoardTop ? iHeight : 0 );
   iBottom = pCTW->iBoardBottom + 1 - ( pCTW->fBoardBottom ? 0 : iHeight );
   iLeft   = pCTW->iBoardLeft - ( pCTW->fBoardLeft ? iWidth : 0 );
   iRight  = pCTW->iBoardRight + 1 - ( pCTW->fBoardRight ? 0 : iWidth );

   if( iRow < iTop )
      iRow = iTop;
   else if( iRow > iBottom )
      iRow = iBottom;
   if( iCol < iLeft )
      iCol = iLeft;
   else if( iCol > iRight )
      iCol = iRight;

   pWnd = ( PZH_CT_WND ) zh_xgrabz( sizeof( ZH_CT_WND ) );

   pWnd->fHidden = ! fVisible;
   pWnd->iLevel = ZH_CTW_DEFAULT;
   pWnd->iShadowAttr = pCTW->iShadowAttr;
   pWnd->iCursorStyle = ZH_GTSELF_GETCURSORSTYLE( pCTW->pGT );

   pWnd->iHeight = iHeight;
   pWnd->iWidth  = iWidth;
   pWnd->iFirstRow = iRow;
   pWnd->iFirstCol = iCol;

   ZH_GTSELF_GETCOLORDATA( pCTW->pGT, &pWnd->piColors, &pWnd->iColorCount, &pWnd->iColorIndex );

   pWnd->screenBuffer = ( PZH_SCREENCELL ) zh_xgrab( ( ZH_SIZE ) pWnd->iHeight *
                                    pWnd->iWidth * sizeof( ZH_SCREENCELL ) );

   if( pWnd->iShadowAttr >= 0 )
      fClear = ZH_TRUE;
   bAttr = 0;
   if( iColor < 0 )
      iColor = ZH_GTSELF_GETCOLOR( pCTW->pGT );
   usChar = ZH_GTSELF_GETCLEARCHAR( pCTW->pGT );

   lIndex = 0;
   for( iRow = pWnd->iFirstRow; iRow < pWnd->iFirstRow + pWnd->iHeight; ++iRow )
   {
      for( iCol = pWnd->iFirstCol; iCol < pWnd->iFirstCol + pWnd->iWidth; ++iCol )
      {
         if( ! fClear && ! ZH_GTSELF_GETSCRCHAR( pCTW->pGT, iRow, iCol, &iColor, &bAttr, &usChar ) )
         {
            usChar = ZH_GTSELF_GETCLEARCHAR( pCTW->pGT );
            iColor = ZH_GTSELF_GETCOLOR( pCTW->pGT );
            bAttr  = 0;
         }
         pWnd->screenBuffer[ lIndex ].c.usChar = usChar;
         pWnd->screenBuffer[ lIndex ].c.bColor = ( ZH_BYTE ) iColor;
         pWnd->screenBuffer[ lIndex ].c.bAttr  = 0;
         ++lIndex;
      }
   }

   for( iTmp = 1; iTmp < pCTW->iMaxWindow; ++iTmp )
   {
      if( pCTW->windows[ iTmp ] == NULL )
         break;
   }
   pWnd->iHandle = iTmp;

   pCTW->windows[ pWnd->iHandle ] = pWnd;
   /* update window level */
   iTmp = pCTW->iOpenWindows++;
   while( iTmp > 0 && pCTW->windows[ pCTW->windowStack[ iTmp - 1 ] ]->iLevel >
                      pWnd->iLevel )
   {
      pCTW->windowStack[ iTmp ] = pCTW->windowStack[ iTmp - 1 ];
      --iTmp;
   }
   pCTW->windowStack[ iTmp ] = pWnd->iHandle;
   ZH_CTW_SETCURRENT( pCTW, pWnd->iHandle );
   if( ! pWnd->fHidden )
   {
      if( iTmp < pCTW->iOpenWindows - 1 )
         zh_ctw_RemapAllWindows( pCTW, iTmp, ZH_TRUE );
      else
         zh_ctw_WindowMap( pCTW, pWnd->iHandle, ZH_TRUE );
   }

   return pWnd->iHandle;
}

static int zh_ctw_CloseWindow( PZH_GTCTW pCTW, int iWindow )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_CloseWindow(%p,%d)", ( void * ) pCTW, iWindow ) );

   if( iWindow > 0 && iWindow <= pCTW->iMaxWindow && pCTW->windows[ iWindow ] )
   {
      PZH_CT_WND pWnd = pCTW->windows[ iWindow ];
      int i, iWnd, iLast;
      ZH_BOOL fHidden = pWnd->fHidden;

      zh_xfree( pWnd->screenBuffer );
      if( pWnd->iColorCount )
         zh_xfree( pWnd->piColors );
      zh_xfree( pWnd );
      pCTW->windows[ iWindow ] = NULL;

      iWnd = 0;
      i = --pCTW->iOpenWindows;
      do
      {
         iLast = pCTW->windowStack[ i ];
         pCTW->windowStack[ i ] = iWnd;
         if( iLast == iWindow )
            break;
         iWnd = iLast;
      }
      while( --i >= 0 );

      iLast = ZH_CTW_GETCURRENT( pCTW );
      if( iWindow == iLast )
      {
         iLast = pCTW->iOpenWindows > 0 ? pCTW->windowStack[ pCTW->iOpenWindows - 1 ] : 0;
         ZH_CTW_SETCURRENT( pCTW, iLast );
      }

      if( ! fHidden )
         zh_ctw_RemapAllWindows( pCTW, 0, ZH_TRUE );

      return iLast;
   }

   return -1;
}

static int zh_ctw_CloseAllWindows( PZH_GTCTW pCTW )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_CloseAllWindows(%p)", ( void * ) pCTW ) );

   if( pCTW->iOpenWindows > 0 )
   {
      int i;

      for( i = 0; i < pCTW->iOpenWindows; ++i )
      {
         int iWindow = pCTW->windowStack[ i ];
         PZH_CT_WND pWnd = pCTW->windows[ iWindow ];
         pCTW->windowStack[ i ] = 0;
         pCTW->windows[ iWindow ] = NULL;
         if( pWnd )
         {
            zh_xfree( pWnd->screenBuffer );
            if( pWnd->iColorCount )
               zh_xfree( pWnd->piColors );
            zh_xfree( pWnd );
         }
      }
      pCTW->iOpenWindows = 0;
      ZH_CTW_SETCURRENT( pCTW, 0 );
      zh_ctw_RemapAllWindows( pCTW, 0, ZH_TRUE );
      return 0;
   }

   return -1;
}

static int zh_ctw_CenterWindow( PZH_GTCTW pCTW, int iWindow, ZH_BOOL fCenter )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_CenterWindow(%p,%d,%d)", ( void * ) pCTW, iWindow, ( int ) fCenter ) );

   if( iWindow > 0 && iWindow <= pCTW->iOpenWindows )
   {
      PZH_CT_WND pWnd = pCTW->windows[ iWindow ];

      if( pWnd )
      {
         int iRow = pWnd->iFirstRow, iCol = pWnd->iFirstCol;

         if( fCenter )
         {
            int iHeight = pCTW->iBoardBottom - pCTW->iBoardTop + 1,
                iWidth = pCTW->iBoardRight - pCTW->iBoardLeft + 1;

            pWnd->iFirstRow = pCTW->iBoardTop;
            pWnd->iFirstCol = pCTW->iBoardLeft;

            if( iHeight > pWnd->iHeight )
               pWnd->iFirstRow += ( iHeight - pWnd->iHeight ) >> 1;
            if( iWidth > pWnd->iWidth )
               pWnd->iFirstCol += ( iWidth - pWnd->iWidth ) >> 1;
         }
         else
         {
            if( pWnd->iFirstRow > pCTW->iBoardBottom - pWnd->iHeight + 1 )
               pWnd->iFirstRow = pCTW->iBoardBottom - pWnd->iHeight + 1;
            if( pWnd->iFirstRow < pCTW->iBoardTop )
               pWnd->iFirstRow = pCTW->iBoardTop;
            if( pWnd->iFirstCol > pCTW->iBoardRight - pWnd->iWidth + 1 )
               pWnd->iFirstCol = pCTW->iBoardRight - pWnd->iWidth + 1;
            if( pWnd->iFirstCol < pCTW->iBoardLeft )
               pWnd->iFirstCol = pCTW->iBoardLeft;
         }

         if( ! pWnd->fHidden &&
             ( iRow != pWnd->iFirstRow || iCol != pWnd->iFirstCol ) )
            zh_ctw_RemapAllWindows( pCTW, 0, ZH_TRUE );

         return iWindow;
      }
   }

   return -1;
}

static int zh_ctw_MoveWindow( PZH_GTCTW pCTW, int iWindow, int iRow, int iCol )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_MoveWindow(%p,%d,%d,%d)", ( void * ) pCTW, iWindow, iRow, iCol ) );

   if( iWindow > 0 && iWindow <= pCTW->iOpenWindows )
   {
      PZH_CT_WND pWnd = pCTW->windows[ iWindow ];

      if( pWnd )
      {
         if( ( iRow + ( pCTW->fBoardTop ? pWnd->iHeight : 0 ) >= pCTW->iBoardTop ) &&
             ( iRow + ( pCTW->fBoardBottom ? 0 : pWnd->iHeight ) <= pCTW->iBoardBottom + 1 ) &&
             ( iCol + ( pCTW->fBoardLeft ? pWnd->iWidth : 0 ) >= pCTW->iBoardLeft ) &&
             ( iCol + ( pCTW->fBoardRight ? 0 : pWnd->iWidth ) <= pCTW->iBoardRight + 1 ) )
         {
            pWnd->iFirstRow = iRow;
            pWnd->iFirstCol = iCol;
            if( ! pWnd->fHidden )
               zh_ctw_RemapAllWindows( pCTW, 0, ZH_TRUE );
            return iWindow;
         }
      }
   }

   return -1;
}

static int zh_ctw_ChangeMargins( PZH_GTCTW pCTW, int iWindow, int iTop, int iLeft, int iBottom, int iRight )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_ChangeMargins(%p,%d,%d,%d,%d,%d)", ( void * ) pCTW, iWindow, iTop, iLeft, iBottom, iRight ) );

   if( iWindow > 0 && iWindow <= pCTW->iOpenWindows )
   {
      PZH_CT_WND pWnd = pCTW->windows[ iWindow ];

      if( pWnd )
      {
         if( ( iTop += pWnd->iTopMargin ) < 0 )
            iTop = 0;
         if( ( iLeft += pWnd->iLeftMargin ) < 0 )
            iLeft = 0;
         if( ( iBottom += pWnd->iBottomMargin ) < 0 )
            iBottom = 0;
         if( ( iRight += pWnd->iRightMargin ) < 0 )
            iRight = 0;

         if( iTop + iBottom < pWnd->iHeight && iLeft + iRight < pWnd->iWidth )
         {
            pWnd->iTopMargin    = iTop;
            pWnd->iLeftMargin   = iLeft;
            pWnd->iBottomMargin = iBottom;
            pWnd->iRightMargin  = iRight;

            return iWindow;
         }
      }
   }

   return -1;
}

static int zh_ctw_SetWindowClip( PZH_GTCTW pCTW, int iWindow, int iTop, int iLeft, int iBottom, int iRight )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_SetWindowClip(%p,%d,%d,%d,%d,%d)", ( void * ) pCTW, iWindow, iTop, iLeft, iBottom, iRight ) );

   if( iWindow > 0 && iWindow <= pCTW->iOpenWindows )
   {
      PZH_CT_WND pWnd = pCTW->windows[ iWindow ];

      if( pWnd )
      {
         if( iTop < 0 )
            iTop = 0;
         if( iLeft < 0 )
            iLeft = 0;
         if( iBottom >= pWnd->iHeight )
            iBottom = pWnd->iHeight - 1;
         if( iRight >= pWnd->iWidth )
            iRight = pWnd->iWidth - 1;

         if( iTop > iBottom || iLeft > iRight ||
             ( iTop == 0 && iLeft == 0 &&
               iBottom == pWnd->iHeight - 1 && iRight == pWnd->iWidth - 1 ) )
         {
            pWnd->fClip = ZH_FALSE;
         }
         else
         {
            pWnd->fClip      = ZH_TRUE;
            pWnd->iCliTop    = iTop;
            pWnd->iCliLeft   = iLeft;
            pWnd->iCliBottom = iBottom;
            pWnd->iCliRight  = iRight;
         }

         return iWindow;
      }
   }
   return -1;
}

static int zh_ctw_GetWindowCords( PZH_GTCTW pCTW, int iWindow, ZH_BOOL fCenter, int * piTop, int * piLeft, int * piBottom, int * piRight )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_GetWindowCords(%p,%d,%d,%p,%p,%p,%p)", ( void * ) pCTW, iWindow, ( int ) fCenter, ( void * ) piTop, ( void * ) piLeft, ( void * ) piBottom, ( void * ) piRight ) );

   if( iWindow > 0 && iWindow <= pCTW->iOpenWindows )
   {
      PZH_CT_WND pWnd = pCTW->windows[ iWindow ];

      if( pWnd )
      {
         if( fCenter )
         {
            int iHeight = pCTW->iBoardBottom - pCTW->iBoardTop + 1,
                iWidth = pCTW->iBoardRight - pCTW->iBoardLeft + 1;

            *piTop  = pCTW->iBoardTop;
            *piLeft = pCTW->iBoardLeft;

            if( iHeight > pWnd->iHeight )
               *piTop += ( iHeight - pWnd->iHeight ) >> 1;
            if( iWidth > pWnd->iWidth )
               *piLeft += ( iWidth - pWnd->iWidth ) >> 1;
         }
         else
         {
            *piTop  = pWnd->iFirstRow;
            *piLeft = pWnd->iFirstCol;
         }
         *piBottom = *piTop + pWnd->iHeight - 1;
         *piRight  = *piLeft + pWnd->iWidth - 1;

         return iWindow;
      }
   }

   *piTop = *piLeft = 0;
   *piBottom = ZH_GTSELF_MAXROW( pCTW->pGT );
   *piRight  = ZH_GTSELF_MAXCOL( pCTW->pGT );

   return -1;
}

static int zh_ctw_GetFormatCords( PZH_GTCTW pCTW, int iWindow, ZH_BOOL fRelative, int * piTop, int * piLeft, int * piBottom, int * piRight )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_GetFormatCords(%p,%d,%d,%p,%p,%p,%p)", ( void * ) pCTW, iWindow, ( int ) fRelative, ( void * ) piTop, ( void * ) piLeft, ( void * ) piBottom, ( void * ) piRight ) );

   if( iWindow > 0 && iWindow <= pCTW->iOpenWindows )
   {
      PZH_CT_WND pWnd = pCTW->windows[ iWindow ];

      if( pWnd )
      {
         if( fRelative )
         {
            *piTop    = pWnd->iTopMargin;
            *piLeft   = pWnd->iLeftMargin;
            *piBottom = pWnd->iBottomMargin;
            *piRight  = pWnd->iRightMargin;
         }
         else
         {
            *piTop    = pWnd->iFirstRow + pWnd->iTopMargin;
            *piLeft   = pWnd->iFirstCol + pWnd->iLeftMargin;
            *piBottom = pWnd->iFirstRow + pWnd->iHeight - pWnd->iBottomMargin - 1;
            *piRight  = pWnd->iFirstCol + pWnd->iWidth - pWnd->iRightMargin - 1;
         }
         return iWindow;
      }
   }

   if( fRelative )
   {
      *piTop = *piLeft = *piBottom = *piRight = 0;
   }
   else
   {
      *piTop = *piLeft = 0;
      *piBottom = ZH_GTSELF_MAXROW( pCTW->pGT );
      *piRight  = ZH_GTSELF_MAXCOL( pCTW->pGT );
   }

   return -1;
}

static int zh_ctw_AddWindowBox( PZH_GTCTW pCTW, int iWindow, const ZH_WCHAR * szBoxW, int iColor )
{
   int iMaxRow, iMaxCol;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_AddWindowBox(%p,%d,%p,%d)", ( void * ) pCTW, iWindow, ( const void * ) szBoxW, iColor ) );

   iMaxRow = ZH_GTSELF_MAXROW( pCTW->pGT );
   iMaxCol = ZH_GTSELF_MAXCOL( pCTW->pGT );

   if( iMaxRow > 1 && iMaxCol > 1 )
   {
      if( iColor < 0 )
         iColor = ZH_GTSELF_GETCOLOR( pCTW->pGT );
      ZH_GTSELF_BOXW( pCTW->pGT, 0, 0, iMaxRow, iMaxCol, szBoxW, iColor );
      if( iWindow > 0 && iWindow <= pCTW->iOpenWindows &&
          pCTW->windows[ iWindow ] != NULL )
      {
         ZH_GTSELF_SETPOS( pCTW->pGT, 0, 0 );
         zh_ctw_ChangeMargins( pCTW, iWindow, 1, 1, 1, 1 );
      }
      else
         ZH_GTSELF_SETPOS( pCTW->pGT, 1, 1 );

      return 0;
   }

   return -1;
}

static int zh_ctw_SwapWindows( PZH_GTCTW pCTW, int iWindow1, int iWindow2 )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_SwapWindows(%p,%d,%d)", ( void * ) pCTW, iWindow1, iWindow2 ) );

   if( iWindow1 > 0 && iWindow1 <= pCTW->iOpenWindows &&
       pCTW->windows[ iWindow1 ] != NULL &&
       iWindow2 > 0 && iWindow2 <= pCTW->iOpenWindows &&
       pCTW->windows[ iWindow2 ] != NULL )
   {
      PZH_CT_WND pWnd;
      int iLevel;
      ZH_BOOL fHidden;

      pWnd = pCTW->windows[ iWindow1 ];
      pCTW->windows[ iWindow1 ] = pCTW->windows[ iWindow2 ];
      pCTW->windows[ iWindow2 ] = pWnd;

      iLevel = pWnd->iLevel;
      pWnd->iLevel = pCTW->windows[ iWindow1 ]->iLevel;
      pCTW->windows[ iWindow1 ]->iLevel = iLevel;

      fHidden = pWnd->fHidden;
      pWnd->fHidden = pCTW->windows[ iWindow1 ]->fHidden;
      pCTW->windows[ iWindow1 ]->fHidden = fHidden;

      if( ! fHidden || ! pWnd->fHidden )
         zh_ctw_RemapAllWindows( pCTW, 0, ZH_TRUE );
      return iWindow1;
   }

   return -1;
}

/* --- */

static void zh_ctw_Init( PZH_GTCTW pCTW )
{
   int iRow, iCol;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_Init(%p)", ( void * ) pCTW ) );

   pCTW->iShadowWidth    = 2;
   pCTW->iShadowAttr     = -1;
   pCTW->iMoveMode       = 1;
   pCTW->iVerticalStep   = 2;
   pCTW->iHorizontalStep = 5;

   /* initialize thread local storage for current window number */
   ZH_TSD_INIT( &pCTW->TSD, sizeof( ZH_CTWDATA ), NULL, NULL );

   ZH_GTSELF_GETSIZE( pCTW->pGT, &pCTW->iMapHeight, &pCTW->iMapWidth );

   /* update cursor position to the rules used by CTWIN */
   ZH_GTSELF_GETPOS( pCTW->pGT, &iRow, &iCol );
   ZH_GTSELF_SETPOS( pCTW->pGT, iRow, iCol );
}

/* --- */

static PZH_GTCTW zh_ctw_base( void )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_base()" ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      if( ZH_GTCTW_GET( pGT ) )
         return ZH_GTCTW_GET( pGT );
      else
      {
         PZH_GTCTW pCTW = ( PZH_GTCTW ) zh_xgrabz( sizeof( ZH_GTCTW ) );

         ZH_GTLOCAL( pGT ) = pCTW;
         pCTW->pGT = pGT;

         if( zh_gtLoad( ZH_GT_DRVNAME( ZH_GT_NAME ), pGT, ZH_GTSUPERTABLE( pGT ) ) )
         {
            zh_ctw_Init( pCTW );
            return pCTW;
         }

         ZH_GTLOCAL( pGT ) = NULL;
         zh_xfree( pCTW );
      }
      zh_gt_BaseFree( pGT );
   }

   return NULL;
}

static void zh_ctw_gt_Exit( PZH_GT pGT )
{
   PZH_GTCTW pCTW;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_gt_Exit(%p)", ( void * ) pGT ) );

   ZH_GTSELF_REFRESH( pGT );

   pCTW = ZH_GTCTW_GET( pGT );
   zh_ctw_CloseAllWindows( pCTW );

   ZH_GTSUPER_EXIT( pGT );

   if( pCTW )
   {
      if( pCTW->iMaxWindow > 0 )
      {
         zh_xfree( pCTW->windows );
         zh_xfree( pCTW->windowStack );
         zh_xfree( pCTW->pWindowMap );
         zh_xfree( pCTW->pShadowMap );
      }
      /* release thread local storage for current window number */
      zh_stackReleaseTSD( &pCTW->TSD );
      zh_xfree( pCTW );
   }
}

static int zh_ctw_MouseRow( PZH_GT pGT )
{
   PZH_GTCTW pCTW;
   int iRow, iWindow;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_MouseRow(%p)", ( void * ) pGT ) );

   iRow = ZH_GTSUPER_MOUSEROW( pGT );

   pCTW = ZH_GTCTW_GET( pGT );
   iWindow = ZH_CTW_GETCURRENT( pCTW );
   if( iWindow > 0 )
      iRow -= pCTW->windows[ iWindow ]->iFirstRow +
              pCTW->windows[ iWindow ]->iTopMargin;

   return iRow;
}

static int zh_ctw_MouseCol( PZH_GT pGT )
{
   PZH_GTCTW pCTW;
   int iCol, iWindow;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_MouseCol(%p)", ( void * ) pGT ) );

   iCol = ZH_GTSUPER_MOUSECOL( pGT );

   pCTW = ZH_GTCTW_GET( pGT );
   iWindow = ZH_CTW_GETCURRENT( pCTW );
   if( iWindow > 0 )
      iCol -= pCTW->windows[ iWindow ]->iFirstCol +
              pCTW->windows[ iWindow ]->iLeftMargin;

   return iCol;
}

static void zh_ctw_gt_GetPos( PZH_GT pGT, int * piRow, int * piCol )
{
   PZH_GTCTW pCTW;
   int iWindow;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_gt_GetPos(%p,%p,%p)", ( void * ) pGT, ( void * ) piRow, ( void * ) piCol ) );

   pCTW = ZH_GTCTW_GET( pGT );
   iWindow = ZH_CTW_GETCURRENT( pCTW );
   if( iWindow > 0 )
   {
      *piRow = pCTW->windows[ iWindow ]->iRow;
      *piCol = pCTW->windows[ iWindow ]->iCol;
   }
   else
      ZH_GTSUPER_GETPOS( pGT, piRow, piCol );
}

/*
 * CTWIN uses differ rules when set cursor position out of screen visible
 * area then standard Clipper's GT drivers so we have to replicate it in
 * SetPos() method, [druzus]
 */
static void zh_ctw_gt_SetPos( PZH_GT pGT, int iRow, int iCol )
{
   PZH_GTCTW pCTW;
   int iHeight, iWidth, iWindow;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_gt_GetPos(%p,%d,%d)", ( void * ) pGT, iRow, iCol ) );

   iHeight = ZH_GTSELF_MAXROW( pGT ) + 1;
   iWidth  = ZH_GTSELF_MAXCOL( pGT ) + 1;

   if( iCol > iWidth )
      iCol = iWidth;
   else if( iCol < 0 )
   {
      iRow += iCol / iWidth - 1;
      iCol = iWidth + iCol % iWidth;
   }
   if( iRow > iHeight )
      iRow = iHeight;

   pCTW = ZH_GTCTW_GET( pGT );
   iWindow = ZH_CTW_GETCURRENT( pCTW );
   if( iWindow > 0 )
   {
      if( iRow < -pCTW->windows[ iWindow ]->iTopMargin )
         iRow = -pCTW->windows[ iWindow ]->iTopMargin;
      pCTW->windows[ iWindow ]->iRow = iRow;
      pCTW->windows[ iWindow ]->iCol = iCol;
   }
   else
   {
      if( iRow < 0 )
         iRow = 0;
      ZH_GTSUPER_SETPOS( pGT, iRow, iCol );
   }
}

static int zh_ctw_gt_MaxCol( PZH_GT pGT )
{
   PZH_GTCTW pCTW;
   int iWindow;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_gt_MaxCol(%p)", ( void * ) pGT ) );

   pCTW = ZH_GTCTW_GET( pGT );
   iWindow = ZH_CTW_GETCURRENT( pCTW );
   if( iWindow > 0 )
      return pCTW->windows[ iWindow ]->iWidth -
             pCTW->windows[ iWindow ]->iLeftMargin -
             pCTW->windows[ iWindow ]->iRightMargin - 1;
   else
      return ZH_GTSUPER_MAXCOL( pGT );
}

static int zh_ctw_gt_MaxRow( PZH_GT pGT )
{
   PZH_GTCTW pCTW;
   int iWindow;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_gt_MaxRow(%p)", ( void * ) pGT ) );

   pCTW = ZH_GTCTW_GET( pGT );
   iWindow = ZH_CTW_GETCURRENT( pCTW );
   if( iWindow > 0 )
      return pCTW->windows[ iWindow ]->iHeight -
             pCTW->windows[ iWindow ]->iTopMargin -
             pCTW->windows[ iWindow ]->iBottomMargin - 1;
   else
      return ZH_GTSUPER_MAXROW( pGT );
}

/*
 * CTWIN uses differ rules in console output then standard Clipper's
 * GT drivers so we have to overload WRITECON() method, [druzus]
 */
#define WRITECON_BUFFER_SIZE  512

static void zh_ctw_gt_WriteCon( PZH_GT pGT, const char * szText, ZH_SIZE nLength )
{
   int iLen = 0;
   ZH_BOOL bDisp = ZH_FALSE;
   ZH_BOOL bBell = ZH_FALSE;
   int iRow, iCol, iMaxRow, iMaxCol;
   ZH_WCHAR szString[ WRITECON_BUFFER_SIZE ];
   PZH_CODEPAGE cdp = ZH_GTSELF_HOSTCP( pGT );
   ZH_SIZE nIndex = 0;
   ZH_WCHAR wc;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_gt_WriteCon(%p,%p,%" ZH_PFS "u)", ( void * ) pGT, ( const void * ) szText, nLength ) );

   iMaxRow = ZH_GTSELF_MAXROW( pGT );
   iMaxCol = ZH_GTSELF_MAXCOL( pGT );

   /* small hack for scrolling console output when client area is set */
   {
      PZH_GTCTW pCTW = ZH_GTCTW_GET( pGT );
      int iWindow = ZH_CTW_GETCURRENT( pCTW );
      if( iWindow > 0 && pCTW->windows[ iWindow ]->fClip )
         iMaxRow = pCTW->windows[ iWindow ]->iCliBottom;
   }

   ZH_GTSELF_GETPOS( pGT, &iRow, &iCol );

   if( iRow > iMaxRow || iCol > iMaxCol )
   {
      if( iRow > iMaxRow )
         iRow = iMaxRow;
      if( iCol > iMaxCol )
         iCol = iMaxCol;
      ZH_GTSELF_SETPOS( pGT, iRow, iCol );
   }

   while( ZH_CODEPAGE_CHAR_GET( cdp, szText, nLength, &nIndex, &wc ) )
   {
      switch( wc )
      {
         case ZH_CHAR_BEL:
            bDisp = bBell = ZH_TRUE;
            break;

         case ZH_CHAR_BS:
            if( iCol > 0 )
            {
               --iCol;
               bDisp = ZH_TRUE;
            }
            else if( iRow > 0 )
            {
               iCol = iMaxCol;
               --iRow;
               bDisp = ZH_TRUE;
            }
            if( bDisp )
            {
               if( iLen )
                  szString[ iLen - 1 ] = ' ';
               else
               {
                  ZH_GTSELF_SETPOS( pGT, iRow, iCol );
                  szString[ iLen++ ] = ' ';
               }
            }
            break;

         case ZH_CHAR_LF:
            iCol = 0;
            ++iRow;
            bDisp = ZH_TRUE;
            break;

         case ZH_CHAR_CR:
            iCol = 0;
            if( nIndex < nLength && szText[ nIndex ] == ZH_CHAR_LF )
            {
               ++iRow;
               ++nIndex;
            }
            bDisp = ZH_TRUE;
            break;

         default:
            szString[ iLen++ ] = wc;
            if( ++iCol > iMaxCol )
            {
               iCol = 0;
               ++iRow;
               bDisp = ZH_TRUE;
            }
            else if( iLen >= WRITECON_BUFFER_SIZE )
               bDisp = ZH_TRUE;
      }

      if( bDisp || nIndex == nLength )
      {
         if( iLen )
            ZH_GTSELF_WRITEW( pGT, szString, iLen );

         iLen = 0;
         if( iRow > iMaxRow )
         {
            ZH_GTSELF_SCROLL( pGT, 0, 0, iMaxRow, iMaxCol,
                              ZH_GTSELF_GETCOLOR( pGT ),
                              ZH_GTSELF_GETCLEARCHAR( pGT ),
                              iRow - iMaxRow, 0 );
            iRow = iMaxRow;
            iCol = 0;
         }
         ZH_GTSELF_SETPOS( pGT, iRow, iCol );
         bDisp = ZH_FALSE;

         /* To emulate scrolling */
         ZH_GTSELF_FLUSH( pGT );

         if( bBell )
         {
            ZH_GTSELF_BELL( pGT );
            bBell = ZH_FALSE;
         }
      }
   }
}

static void zh_ctw_gt_WriteConW( PZH_GT pGT, const ZH_WCHAR * szText, ZH_SIZE nLength )
{
   int iLen = 0;
   ZH_BOOL bDisp = ZH_FALSE;
   ZH_BOOL bBell = ZH_FALSE;
   int iRow, iCol, iMaxRow, iMaxCol;
   ZH_WCHAR szString[ WRITECON_BUFFER_SIZE ];
   ZH_SIZE nIndex = 0;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_gt_WriteConW(%p,%p,%" ZH_PFS "u)", ( void * ) pGT, ( const void * ) szText, nLength ) );

   iMaxRow = ZH_GTSELF_MAXROW( pGT );
   iMaxCol = ZH_GTSELF_MAXCOL( pGT );

   /* small hack for scrolling console output when client area is set */
   {
      PZH_GTCTW pCTW = ZH_GTCTW_GET( pGT );
      int iWindow = ZH_CTW_GETCURRENT( pCTW );
      if( iWindow > 0 && pCTW->windows[ iWindow ]->fClip )
         iMaxRow = pCTW->windows[ iWindow ]->iCliBottom;
   }

   ZH_GTSELF_GETPOS( pGT, &iRow, &iCol );

   if( iRow > iMaxRow || iCol > iMaxCol )
   {
      if( iRow > iMaxRow )
         iRow = iMaxRow;
      if( iCol > iMaxCol )
         iCol = iMaxCol;
      ZH_GTSELF_SETPOS( pGT, iRow, iCol );
   }

   while( nIndex < nLength )
   {
      ZH_WCHAR wc = szText[ nIndex++ ];

      switch( wc )
      {
         case ZH_CHAR_BEL:
            bDisp = bBell = ZH_TRUE;
            break;

         case ZH_CHAR_BS:
            if( iCol > 0 )
            {
               --iCol;
               bDisp = ZH_TRUE;
            }
            else if( iRow > 0 )
            {
               iCol = iMaxCol;
               --iRow;
               bDisp = ZH_TRUE;
            }
            if( bDisp )
            {
               if( iLen )
                  szString[ iLen - 1 ] = ' ';
               else
               {
                  ZH_GTSELF_SETPOS( pGT, iRow, iCol );
                  szString[ iLen++ ] = ' ';
               }
            }
            break;

         case ZH_CHAR_LF:
            iCol = 0;
            ++iRow;
            bDisp = ZH_TRUE;
            break;

         case ZH_CHAR_CR:
            iCol = 0;
            if( nIndex < nLength && szText[ nIndex ] == ZH_CHAR_LF )
            {
               ++iRow;
               ++nIndex;
            }
            bDisp = ZH_TRUE;
            break;

         default:
            szString[ iLen++ ] = wc;
            if( ++iCol > iMaxCol )
            {
               iCol = 0;
               ++iRow;
               bDisp = ZH_TRUE;
            }
            else if( iLen >= WRITECON_BUFFER_SIZE )
               bDisp = ZH_TRUE;
      }

      if( bDisp || nIndex == nLength )
      {
         if( iLen )
            ZH_GTSELF_WRITEW( pGT, szString, iLen );

         iLen = 0;
         if( iRow > iMaxRow )
         {
            ZH_GTSELF_SCROLL( pGT, 0, 0, iMaxRow, iMaxCol,
                              ZH_GTSELF_GETCOLOR( pGT ),
                              ZH_GTSELF_GETCLEARCHAR( pGT ),
                              iRow - iMaxRow, 0 );
            iRow = iMaxRow;
            iCol = 0;
         }
         ZH_GTSELF_SETPOS( pGT, iRow, iCol );
         bDisp = ZH_FALSE;

         /* To emulate scrolling */
         ZH_GTSELF_FLUSH( pGT );

         if( bBell )
         {
            ZH_GTSELF_BELL( pGT );
            bBell = ZH_FALSE;
         }
      }
   }
}

static int zh_ctw_gt_GetCursorStyle( PZH_GT pGT )
{
   PZH_GTCTW pCTW;
   int iWindow;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_gt_GetCursorStyle(%p)", ( void * ) pGT ) );

   pCTW = ZH_GTCTW_GET( pGT );
   iWindow = ZH_CTW_GETCURRENT( pCTW );
   if( iWindow > 0 )
      return pCTW->windows[ iWindow ]->iCursorStyle;
   else
      return ZH_GTSUPER_GETCURSORSTYLE( pGT );
}

static void zh_ctw_gt_SetCursorStyle( PZH_GT pGT, int iStyle )
{
   PZH_GTCTW pCTW;
   int iWindow;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_gt_SetCursorStyle(%p,%d)", ( void * ) pGT, iStyle ) );

   pCTW = ZH_GTCTW_GET( pGT );
   iWindow = ZH_CTW_GETCURRENT( pCTW );
   if( iWindow > 0 )
   {
      switch( iStyle )
      {
         case SC_NONE:
         case SC_NORMAL:
         case SC_INSERT:
         case SC_SPECIAL1:
         case SC_SPECIAL2:
            pCTW->windows[ iWindow ]->iCursorStyle = iStyle;
            break;
         default:
            pCTW->windows[ iWindow ]->iCursorStyle = SC_NORMAL;
            break;
      }
   }
   else
      ZH_GTSUPER_SETCURSORSTYLE( pGT, iStyle );
}

static void zh_ctw_gt_GetColorStr( PZH_GT pGT, char * pszColorString )
{
   PZH_GTCTW pCTW;
   int iWindow;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_gt_GetColorStr(%p,%p)", ( void * ) pGT, ( void * ) pszColorString ) );

   pCTW = ZH_GTCTW_GET( pGT );
   iWindow = ZH_CTW_GETCURRENT( pCTW );
   if( iWindow > 0 )
   {
      PZH_CT_WND pWnd = pCTW->windows[ iWindow ];
      ZH_GTSUPER_COLORSTOSTRING( pGT, pWnd->piColors, pWnd->iColorCount, pszColorString, ZH_CLRSTR_LEN );
   }
   else
      ZH_GTSUPER_GETCOLORSTR( pGT, pszColorString );
}

static void zh_ctw_gt_SetColorStr( PZH_GT pGT, const char * szColorString )
{
   PZH_GTCTW pCTW;
   int iWindow;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_gt_SetColorStr(%p,%s)", ( void * ) pGT, szColorString ) );

   pCTW = ZH_GTCTW_GET( pGT );
   iWindow = ZH_CTW_GETCURRENT( pCTW );
   if( iWindow > 0 )
   {
      PZH_CT_WND pWnd = pCTW->windows[ iWindow ];
      ZH_GTSUPER_STRINGTOCOLORS( pGT, szColorString, &pWnd->piColors, &pWnd->iColorCount );
      pWnd->iColorIndex = ZH_CLR_STANDARD;
   }
   else
      ZH_GTSUPER_SETCOLORSTR( pGT, szColorString );
}

static void zh_ctw_gt_ColorSelect( PZH_GT pGT, int iColorIndex )
{
   PZH_GTCTW pCTW;
   int iWindow;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_gt_ColorSelect(%p,%d)", ( void * ) pGT, iColorIndex ) );

   pCTW = ZH_GTCTW_GET( pGT );
   iWindow = ZH_CTW_GETCURRENT( pCTW );
   if( iWindow > 0 )
   {
      PZH_CT_WND pWnd = pCTW->windows[ iWindow ];
      if( iColorIndex >= 0 && iColorIndex < pWnd->iColorCount )
         pWnd->iColorIndex = iColorIndex;
   }
   else
      ZH_GTSUPER_COLORSELECT( pGT, iColorIndex );
}

static int zh_ctw_gt_GetColor( PZH_GT pGT )
{
   PZH_GTCTW pCTW;
   int iWindow;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_gt_GetColor(%p)", ( void * ) pGT ) );

   pCTW = ZH_GTCTW_GET( pGT );
   iWindow = ZH_CTW_GETCURRENT( pCTW );
   if( iWindow > 0 )
   {
      PZH_CT_WND pWnd = pCTW->windows[ iWindow ];
      return pWnd->piColors[ pWnd->iColorIndex ];
   }
   else
      return ZH_GTSUPER_GETCOLOR( pGT );
}

static void zh_ctw_gt_GetColorData( PZH_GT pGT, int ** pColorsPtr, int * piColorCount, int * piColorIndex )
{
   PZH_GTCTW pCTW;
   int iWindow;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_gt_GetColor(%p,%p,%p,%p)", ( void * ) pGT, ( void * ) pColorsPtr, ( void * ) piColorCount, ( void * ) piColorIndex ) );

   pCTW = ZH_GTCTW_GET( pGT );
   iWindow = ZH_CTW_GETCURRENT( pCTW );
   if( iWindow > 0 )
   {
      PZH_CT_WND pWnd = pCTW->windows[ iWindow ];

      *pColorsPtr = ( int * ) zh_xgrab( pWnd->iColorCount * sizeof( int ) );
      memcpy( *pColorsPtr, pWnd->piColors, pWnd->iColorCount * sizeof( int ) );
      *piColorCount = pWnd->iColorCount;
      *piColorIndex = pWnd->iColorIndex;
   }
   else
      ZH_GTSUPER_GETCOLORDATA( pGT, pColorsPtr, piColorCount, piColorIndex );
}

static void zh_ctw_gt_GetScrCursor( PZH_GT pGT, int * piRow, int * piCol, int * piStyle )
{
   PZH_GTCTW pCTW;
   int iWindow;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_gt_GetScrCursor(%p,%p,%p,%p)", ( void * ) pGT, ( void * ) piRow, ( void * ) piCol, ( void * ) piStyle ) );

   ZH_GTSUPER_GETSCRCURSOR( pGT, piRow, piCol, piStyle );
   pCTW = ZH_GTCTW_GET( pGT );
   iWindow = ZH_CTW_GETCURRENT( pCTW );
   if( iWindow > 0 )
   {
      PZH_CT_WND pWnd = pCTW->windows[ iWindow ];

      *piRow += pWnd->iFirstRow +
                pWnd->iTopMargin;
      *piCol += pWnd->iFirstCol +
                pWnd->iLeftMargin;
      if( *piStyle != SC_NONE )
      {
         if( *piRow < pCTW->iBoardTop  || *piRow > pCTW->iBoardBottom ||
             *piCol < pCTW->iBoardLeft || *piCol > pCTW->iBoardRight )
            *piStyle = SC_NONE;
         else
         {
            long lIndex = ( long ) *piRow * pCTW->iMapWidth + *piCol;
            if( pCTW->pWindowMap[ lIndex ] != iWindow )
               *piStyle = SC_NONE;
         }
      }
   }
}

static ZH_BOOL zh_ctw_gt_GetScrChar( PZH_GT pGT, int iRow, int iCol,
                                     int * piColor, ZH_BYTE * pbAttr, ZH_USHORT * pusChar )
{
   PZH_GTCTW pCTW;
   int iWindow, iShadow;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_gt_GetScrChar(%p,%d,%d,%p,%p,%p)", ( void * ) pGT, iRow, iCol, ( void * ) piColor, ( void * ) pbAttr, ( void * ) pusChar ) );

   pCTW = ZH_GTCTW_GET( pGT );
   iWindow = iShadow = 0;
   if( pCTW->iOpenWindows > 0 &&
       iRow >= pCTW->iBoardTop  && iRow <= pCTW->iBoardBottom &&
       iCol >= pCTW->iBoardLeft && iCol <= pCTW->iBoardRight )
   {
      long lIndex = ( long ) iRow * pCTW->iMapWidth + iCol;
      iWindow = pCTW->pWindowMap[ lIndex ];
      iShadow = pCTW->pShadowMap[ lIndex ];
   }

   if( iWindow > 0 )
   {
      PZH_CT_WND pWnd = pCTW->windows[ iWindow ];
      iRow -= pWnd->iFirstRow;
      iCol -= pWnd->iFirstCol;
      if( iCol >= 0 && iRow >= 0 && iRow < pWnd->iHeight && iCol < pWnd->iWidth )
      {
         long lIndex = ( long ) iRow * pWnd->iWidth + iCol;
         *pusChar = pWnd->screenBuffer[ lIndex ].c.usChar;
         *piColor = pWnd->screenBuffer[ lIndex ].c.bColor;
         *pbAttr  = pWnd->screenBuffer[ lIndex ].c.bAttr;
      }
      else
         return ZH_FALSE;
   }
   else if( ! ZH_GTSUPER_GETSCRCHAR( pGT, iRow, iCol, piColor, pbAttr, pusChar ) )
      return ZH_FALSE;

   if( iShadow > 0 )
   {
      PZH_CT_WND pWnd = pCTW->windows[ iShadow & ~ZH_CTW_SHADOW_MASK ];
      if( pWnd->iShadowAttr >= 0 )
         *piColor = pWnd->iShadowAttr;
      else if( pWnd->iShadowAttr == ZH_CTW_SHADOW_EXT ||
               pWnd->iShadowAttr == ZH_CTW_SHADOW_EXT2 )
      {
         if( ( *piColor & 0x80 ) == 0 )
            *piColor &= 0x0F;
         if( ( *piColor & 0x08 ) == 0 )
            *piColor &= 0xF0;
         if( ( *piColor &= 0x77 ) == 0 || ( iShadow & ZH_CTW_SHADOW_MASK ) )
            *piColor = 0x07;
      }
      *pbAttr |= ZH_GT_ATTR_SHADOW;
   }

   return ZH_TRUE;
}

static ZH_BOOL zh_ctw_gt_GetScrUC( PZH_GT pGT, int iRow, int iCol,
                                   int * piColor, ZH_BYTE * pbAttr,
                                   ZH_UCHAR * puChar, ZH_BOOL fTerm )
{
   ZH_USHORT usChar;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_gt_GetScrUC(%p,%d,%d,%p,%p,%p,%d)", ( void * ) pGT, iRow, iCol, ( void * ) piColor, ( void * ) pbAttr, ( void * ) puChar, fTerm ) );

   if( zh_ctw_gt_GetScrChar( pGT, iRow, iCol, piColor, pbAttr, &usChar ) )
   {
      ZH_UCHAR uc = 0;
      if( usChar )
      {
         if( fTerm && pGT->cdpTerm )
            uc = zh_cdpGetUC( pGT->cdpTerm, usChar, 0 );
         if( uc == 0 )
         {
            if( pGT->cdpBox && ( ! fTerm || pGT->cdpBox != pGT->cdpTerm ) &&
                pGT->cdpBox != pGT->cdpHost && ( *pbAttr & ZH_GT_ATTR_BOX ) )
               uc = zh_cdpGetUC( pGT->cdpBox, usChar, 0 );
            if( uc == 0 )
            {
               if( pGT->cdpHost && pGT->cdpTerm != pGT->cdpHost )
                  uc = zh_cdpGetUC( pGT->cdpHost, usChar, 0 );
               if( uc == 0 )
                  uc = zh_cdpGetUC( zh_vmCodepage(), usChar, usChar < 32 ? ( ZH_UCHAR ) usChar : '?' );
            }
         }
      }
      *puChar = uc;
      return ZH_TRUE;
   }
   return ZH_FALSE;
}

static ZH_BOOL zh_ctw_gt_GetChar( PZH_GT pGT, int iRow, int iCol,
                                  int * piColor, ZH_BYTE * pbAttr, ZH_USHORT * pusChar )
{
   PZH_GTCTW pCTW;
   PZH_CT_WND pWnd;
   int iWindow;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_gt_GetChar(%p,%d,%d,%p,%p,%p)", ( void * ) pGT, iRow, iCol, ( void * ) piColor, ( void * ) pbAttr, ( void * ) pusChar ) );

   pCTW = ZH_GTCTW_GET( pGT );
   iWindow = ZH_CTW_GETCURRENT( pCTW );
   if( iWindow == 0 )
      return ZH_GTSELF_GETSCRCHAR( pGT, iRow, iCol, piColor, pbAttr, pusChar );

   pWnd = pCTW->windows[ iWindow ];
   iRow += pWnd->iTopMargin;
   iCol += pWnd->iLeftMargin;

   if( iCol >= 0 && iRow >= 0 && iRow < pWnd->iHeight && iCol < pWnd->iWidth )
   {
      long lIndex = ( long ) iRow * pWnd->iWidth + iCol;
      *pusChar = pWnd->screenBuffer[ lIndex ].c.usChar;
      *piColor = pWnd->screenBuffer[ lIndex ].c.bColor;
      *pbAttr  = pWnd->screenBuffer[ lIndex ].c.bAttr;
      return ZH_TRUE;
   }

   return ZH_FALSE;
}

static ZH_BOOL zh_ctw_gt_PutChar( PZH_GT pGT, int iRow, int iCol,
                                  int iColor, ZH_BYTE bAttr, ZH_USHORT usChar )
{
   PZH_GTCTW pCTW;
   int iWindow, iCurrWindow;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_gt_PutChar(%p,%d,%d,%d,%d,%d)", ( void * ) pGT, iRow, iCol, iColor, ( int ) bAttr, ( int ) usChar ) );

   pCTW = ZH_GTCTW_GET( pGT );
   iWindow = iCurrWindow = ZH_CTW_GETCURRENT( pCTW );
   if( iWindow == 0 && pCTW->iOpenWindows > 0 )
   {
      if( iRow >= pCTW->iBoardTop  && iRow <= pCTW->iBoardBottom &&
          iCol >= pCTW->iBoardLeft && iCol <= pCTW->iBoardRight )
      {
         long lIndex = ( long ) iRow * pCTW->iMapWidth + iCol;
         iWindow = pCTW->pWindowMap[ lIndex ];
#if 0
         /* When window with shadow is closed CT3 restores attributes
          * which existed before shadow was displayed. In some application
          * which switches to window 0 for pass-throw output it causes that
          * wrong attributes appears after this operation. In Ziher it's
          * fixed so such problem do not exist. Anyhow some code may switch
          * to window 0, make SaveScreen()/RestScreen() and in such case
          * all shadow attributes are copied to window 0 buffer. The code
          * below is workaround for it. [druzus]
          */
         if( pCTW->pShadowMap[ lIndex ] != 0 )
         {
            int iShadow = pCTW->pShadowMap[ lIndex ] & ~ZH_CTW_SHADOW_MASK;
            if( pCTW->windows[ iShadow ]->iShadowAttr >= 0 &&
                pCTW->windows[ iShadow ]->iShadowAttr == iColor )
            {
               int iClr;
               ZH_BYTE bAtr;
               ZH_USHORT usCh;
               if( ZH_GTSELF_GETSCRCHAR( pGT, iRow, iCol, &iClr, &bAtr, &usCh ) )
               {
                  if( usCh == usChar && iClr == iColor )
                     return ZH_TRUE;
               }
            }
         }
#endif
         pCTW->pShadowMap[ lIndex ] = 0;
      }
   }

   if( iWindow > 0 )
   {
      int iWndRow, iWndCol, iWndHeight, iWndWidth;
      PZH_CT_WND pWnd = pCTW->windows[ iWindow ];

      if( iCurrWindow == 0 )
      {
         iWndRow = iRow - pWnd->iFirstRow;
         iWndCol = iCol - pWnd->iFirstCol;
         iWndHeight = pWnd->iHeight;
         iWndWidth  = pWnd->iWidth;
      }
      else if( pWnd->fClip &&
               ( iRow < pWnd->iCliTop    || iCol < pWnd->iCliLeft ||
                 iRow > pWnd->iCliBottom || iCol > pWnd->iCliRight ) )
         return ZH_TRUE;
      else
      {
         iWndRow = iRow + pWnd->iTopMargin;
         iWndCol = iCol + pWnd->iLeftMargin;
         iRow = iWndRow + pWnd->iFirstRow;
         iCol = iWndCol + pWnd->iFirstCol;
         iWndHeight = pWnd->iHeight - pWnd->iBottomMargin;
         iWndWidth  = pWnd->iWidth - pWnd->iRightMargin;
      }
      if( iWndRow >= 0 && iWndCol >= 0 &&
          iWndRow < iWndHeight && iWndCol < iWndWidth )
      {
         long lIndex = ( long ) iWndRow * pWnd->iWidth + iWndCol;

         pWnd->screenBuffer[ lIndex ].c.usChar = usChar;
         pWnd->screenBuffer[ lIndex ].c.bColor = ( ZH_BYTE ) iColor;
         pWnd->screenBuffer[ lIndex ].c.bAttr  = bAttr;
         if( ! pWnd->fHidden )
         {
            if( iCurrWindow == 0 ||
                ( iRow >= pCTW->iBoardTop  && iRow <= pCTW->iBoardBottom &&
                  iCol >= pCTW->iBoardLeft && iCol <= pCTW->iBoardRight ) )
            {
               ZH_GTSELF_TOUCHLINE( pGT, iRow );
            }
         }
         return ZH_TRUE;
      }
      return ZH_FALSE;
   }

   return ZH_GTSUPER_PUTCHAR( pGT, iRow, iCol, iColor, bAttr, usChar );
}

static ZH_BOOL zh_ctw_gt_Resize( PZH_GT pGT, int iRows, int iCols )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_gt_Resize(%p,%d,%d)", ( void * ) pGT, iRows, iCols ) );

   if( ZH_GTSUPER_RESIZE( pGT, iRows, iCols ) )
   {
      PZH_GTCTW pCTW = ZH_GTCTW_GET( pGT );

      if( pCTW->iMaxWindow > 0 )
      {
         ZH_SIZE nSize;

         pCTW->iMapHeight = iRows;
         pCTW->iMapWidth  = iCols;
         pCTW->iShadowWidth = zh_ctw_CalcShadowWidth( pCTW->iMapHeight, pCTW->iMapWidth );
         nSize = ( ZH_SIZE ) pCTW->iMapHeight * pCTW->iMapWidth * sizeof( int );
         pCTW->pWindowMap = ( int * ) zh_xrealloc( pCTW->pWindowMap, nSize );
         pCTW->pShadowMap = ( int * ) zh_xrealloc( pCTW->pShadowMap, nSize );
      }
      if( pCTW->fBoardSet )
         zh_ctw_SetWindowBoard( pCTW, 0, 0, iRows - 1, iCols - 1 );
      return ZH_TRUE;
   }
   return ZH_FALSE;
}

static ZH_BOOL zh_ctw_gt_Info( PZH_GT pGT, int iType, PZH_GT_INFO pInfo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_gt_Info(%p,%d,%p)", ( void * ) pGT, iType, ( void * ) pInfo ) );

   switch( iType )
   {
      case ZH_GTI_ISCTWIN:
         pInfo->pResult = zh_itemPutL( pInfo->pResult, ZH_TRUE );
         break;

      case ZH_GTI_NEWWIN:
      {
         ZH_BOOL fResult;

         zh_ctw_SelectWindow( ZH_GTCTW_GET( pGT ), 0, ZH_TRUE );
         fResult = ZH_GTSUPER_INFO( pGT, iType, pInfo );

         if( fResult && zh_arrayLen( pInfo->pResult ) >= 7 )
         {
            PZH_GTCTW pCTW = ZH_GTCTW_GET( pGT );
            zh_arraySetNI( pInfo->pResult, 7, ZH_CTW_GETCURRENT( pCTW ) );
         }
         return fResult;
      }
      case ZH_GTI_GETWIN:
      {
         ZH_BOOL fResult;
         PZH_GTCTW pCTW = ZH_GTCTW_GET( pGT );
         int iWindow = ZH_CTW_GETCURRENT( pCTW );

         zh_ctw_SelectWindow( pCTW, 0, ZH_TRUE );
         fResult = ZH_GTSUPER_INFO( pGT, iType, pInfo );
         if( fResult && zh_arrayLen( pInfo->pResult ) >= 7 )
            zh_arraySetNI( pInfo->pResult, 7, iWindow );
         return fResult;
      }
      case ZH_GTI_SETWIN:
      {
         ZH_BOOL fResult;
         PZH_GTCTW pCTW = ZH_GTCTW_GET( pGT );

         zh_ctw_SelectWindow( pCTW, 0, ZH_TRUE );
         fResult = ZH_GTSUPER_INFO( pGT, iType, pInfo );
         if( fResult && zh_arrayLen( pInfo->pNewVal ) >= 7 )
            zh_ctw_SelectWindow( pCTW, zh_arrayGetNI( pInfo->pNewVal, 7 ),
                                 ZH_TRUE );
         return fResult;
      }
      default:
         return ZH_GTSUPER_INFO( pGT, iType, pInfo );
   }

   return ZH_TRUE;
}

static int zh_ctw_gt_Alert( PZH_GT pGT, PZH_ITEM pMessage, PZH_ITEM pOptions,
                            int iClrNorm, int iClrHigh, double dDelay )
{
   int iOptions;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_gt_Alert(%p,%p,%p,%d,%d,%f)", ( void * ) pGT, ( void * ) pMessage, ( void * ) pOptions, iClrNorm, iClrHigh, dDelay ) );

   if( pMessage && ZH_IS_STRING( pMessage ) &&
       pOptions && ( iOptions = ( int ) zh_arrayLen( pOptions ) ) > 0 )
   {
      int iRows, iCols;
      ZH_BOOL fScreen;

      ZH_GTSELF_GETSIZE( pGT, &iRows, &iCols );
      if( iCols <= 4 || iRows <= 4 )
         fScreen = ZH_FALSE;
      else
      {
         ZH_GT_INFO gtInfo;
         memset( &gtInfo, 0, sizeof( gtInfo ) );
         ZH_GTSELF_INFO( pGT, ZH_GTI_ISSCREENPOS, &gtInfo );
         fScreen = gtInfo.pResult && zh_itemGetL( gtInfo.pResult );
         ZH_GTSELF_INFO( pGT, ZH_GTI_KBDSUPPORT, &gtInfo );
         if( gtInfo.pResult )
         {
            if( ! zh_itemGetL( gtInfo.pResult ) )
               fScreen = ZH_FALSE;
            zh_itemRelease( gtInfo.pResult );
         }
      }
      if( fScreen )
      {
         int iRet = 0;

         PZH_GTCTW pCTW = ZH_GTCTW_GET( pGT );
         ZH_UINT ulWidth = 0, ulCurrWidth = 0, ulMsg = 0, ul2, ulMaxWidth, ulLast;
         char szKey[ ZH_MAX_CHAR_LEN ];
         ZH_SIZE nChar;
         int iDspCount, iLines = 0, iTop, iLeft, iBottom, iRight, iPos, iClr, iWnd, iPrevWnd, i;
         ZH_SIZE nLen, nOptLen;
         void * hMessage, * hOpt;
         const ZH_WCHAR * szMessageW = zh_itemGetStrU16( pMessage, ZH_CODEPAGE_ENDIAN_NATIVE, &hMessage, &nLen ),
                        * szOptW;

         ulMaxWidth = iCols - 4;
         while( ulMsg < nLen )
         {
            if( szMessageW[ ulMsg ] == '\n' )
            {
               ++iLines;
               if( ulCurrWidth > ulWidth )
                  ulWidth = ulCurrWidth;
               ulCurrWidth = 0;
            }
            else
               ++ulCurrWidth;
            ++ulMsg;
         }
         if( ulCurrWidth )
            ++iLines;
         if( ulCurrWidth > ulWidth )
            ulWidth = ulCurrWidth;
         ulCurrWidth = 0;
         for( i = 1; i <= iOptions; ++i )
         {
            nOptLen = zh_itemCopyStrU16( zh_arrayGetItemPtr( pOptions, i ), ZH_CODEPAGE_ENDIAN_NATIVE, NULL, 0 );
            ulCurrWidth += ( ZH_UINT ) nOptLen + ( i > 1 ? 3 : 0 );
         }
         if( ulCurrWidth > ulWidth )
            ulWidth = ulCurrWidth;
         if( ulWidth > ulMaxWidth )
            ulWidth = ulMaxWidth;
         if( iRows < iLines + 4 )
            iLines = iRows - 4;
         iTop = ( iRows - iLines - 4 ) >> 1;
         iLeft = ( iCols - ulWidth - 4 ) >> 1;
         iBottom = iTop + iLines + 3;
         iRight = iLeft + ulWidth + 3;
         if( iClrNorm <= 0 )
            iClrNorm = 79;
         if( iClrHigh <= 0 )
            iClrHigh = 31;

         iDspCount = ZH_GTSELF_DISPCOUNT( pGT );
         if( iDspCount == 0 )
            ZH_GTSELF_DISPBEGIN( pGT );

         iPrevWnd = zh_ctw_CurrentWindow( pCTW );
         iWnd = zh_ctw_CreateWindow( pCTW, iTop, iLeft, iBottom, iRight, ZH_TRUE, iClrNorm, ZH_TRUE );
         zh_ctw_AddWindowBox( pCTW, iWnd, sc_szFrameW, iClrNorm );
         ZH_GTSELF_SETCURSORSTYLE( pGT, SC_NONE );
         ulLast = 0;
         i = 0;
         for( ulMsg = 0; ulMsg < nLen; ++ulMsg )
         {
            if( szMessageW[ ulMsg ] == '\n' )
            {
               if( ulMsg > ulLast )
               {
                  ul2 = ulMsg - ulLast;
                  if( ul2 > ulWidth )
                     ul2 = ulWidth;
                  ZH_GTSELF_PUTTEXTW( pGT, i, ( ( ulWidth - ul2 + 1 ) >> 1 ) + 1, iClrNorm,
                                      szMessageW + ulLast, ul2 );
               }
               ulLast = ulMsg + 1;
               if( ++i >= iLines )
                  break;
            }
         }
         if( ulMsg > ulLast && i < iLines )
         {
            ul2 = ulMsg - ulLast;
            if( ul2 > ulWidth )
               ul2 = ulWidth;
            ZH_GTSELF_PUTTEXTW( pGT, i, ( ( ulWidth - ul2 + 1 ) >> 1 ) + 1, iClrNorm,
                                szMessageW + ulLast, ul2 );
         }
         zh_strfree( hMessage );

         iPos = 1;
         while( iRet == 0 )
         {
            int iKey, iMnuCol;

            ZH_GTSELF_DISPBEGIN( pGT );
            iMnuCol = ( ( ulWidth - ulCurrWidth ) >> 1 ) + 1;
            for( i = 1; i <= iOptions; ++i )
            {
               iClr = i == iPos ? iClrHigh : iClrNorm;
               szOptW = zh_arrayGetStrU16( pOptions, i, ZH_CODEPAGE_ENDIAN_NATIVE, &hOpt, &nLen );
               ZH_GTSELF_PUTTEXTW( pGT, iLines + 1, iMnuCol, iClr, szOptW, nLen );
               zh_strfree( hOpt );
               iMnuCol += ( int ) nLen + 3;
            }
            while( ZH_GTSELF_DISPCOUNT( pGT ) )
               ZH_GTSELF_DISPEND( pGT );
            ZH_GTSELF_REFRESH( pGT );

            iKey = ZH_GTSELF_INKEYGET( pGT, ZH_TRUE, dDelay, INKEY_ALL );
            /* TODO: add support for SET KEY blocks */

            if( iKey == K_ESC )
               break;
            else if( iKey == K_ENTER || iKey == K_SPACE || iKey == 0 )
            {
               iRet = iPos;
            }
            else if( iKey == K_LEFT || iKey == K_SH_TAB )
            {
               if( --iPos == 0 )
                  iPos = iOptions;
               dDelay = 0.0;
            }
            else if( iKey == K_RIGHT || iKey == K_TAB )
            {
               if( ++iPos > iOptions )
                  iPos = 1;
               dDelay = 0.0;
            }
            else if( iKey == K_LBUTTONDOWN )
            {
               int iMRow = ZH_GTSELF_MOUSEROW( pGT ),
                   iMCol = ZH_GTSELF_MOUSECOL( pGT );
               if( iMRow == iLines + 1 )
               {
                  iMnuCol = ( ( ulWidth - ulCurrWidth ) >> 1 ) + 1;
                  for( i = 1; i <= iOptions; ++i )
                  {
                     nLen = zh_itemCopyStrU16( zh_arrayGetItemPtr( pOptions, i ), ZH_CODEPAGE_ENDIAN_NATIVE, NULL, 0 );
                     if( iMCol >= iMnuCol && iMCol < iMnuCol + ( int ) nLen )
                     {
                        iRet = i;
                        break;
                     }
                     iMnuCol += ( int ) nLen + 3;
                  }
               }
            }
            else if( ( nChar = zh_inkeyKeyString( iKey, szKey, sizeof( szKey ) ) ) > 0 )
            {
               PZH_CODEPAGE cdp = zh_vmCodepage();
               for( i = 1; i <= iOptions; ++i )
               {
                  nOptLen = zh_arrayGetCLen( pOptions, i );
                  if( nOptLen > 0 )
                  {
                     ZH_SIZE nIdx1 = 0, nIdx2 = 0;
                     if( zh_cdpCharCaseEq( cdp, szKey, nChar, &nIdx1,
                           zh_arrayGetCPtr( pOptions, i ), nOptLen, &nIdx2 ) )
                     {
                        iRet = i;
                        break;
                     }
                  }
               }
            }
         }

         zh_ctw_CloseWindow( pCTW, iWnd );
         zh_ctw_SelectWindow( pCTW, iPrevWnd, ZH_TRUE );
         ZH_GTSELF_REFRESH( pGT );

         while( ZH_GTSELF_DISPCOUNT( pGT ) < iDspCount )
            ZH_GTSELF_DISPBEGIN( pGT );

         return iRet;
      }
   }

   return ZH_GTSUPER_ALERT( pGT, pMessage, pOptions, iClrNorm, iClrHigh, dDelay );
}

static int zh_ctw_gt_ReadKey( PZH_GT pGT, int iEventMask )
{
   int iKey;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ctw_gt_ReadKey(%p,%d)", ( void * ) pGT, iEventMask ) );

   iKey = ZH_GTSUPER_READKEY( pGT, iEventMask );

   if( iKey != 0 )
      ZH_GTCTW_GET( pGT )->iLastKey = iKey;

   return iKey;
}

/* helper function */
static ZH_U32 zh_ctw_gt_cellValue( PZH_GT pGT, int iRow, int iCol )
{
   ZH_SCREENCELL cell;
   int iColor;

   cell.uiValue = 0;
   ZH_GTSELF_GETSCRCHAR( pGT, iRow, iCol,
                         &iColor, &cell.c.bAttr, &cell.c.usChar );
   cell.c.bColor = ( ZH_BYTE ) iColor;

   return cell.uiValue;
}

static void zh_ctw_gt_RedrawDiff( PZH_GT pGT )
{
   if( ZH_GTCTW_GET( pGT )->iOpenWindows == 0 )
      ZH_GTSUPER_REDRAWDIFF( pGT );
   else if( pGT->fRefresh )
   {
      int i, l, r, s;
      long lIndex;
      ZH_U32 uiValue;

      for( i = 0; i < pGT->iHeight; ++i )
      {
         if( pGT->pLines[ i ] )
         {
            lIndex = ( long ) i * pGT->iWidth;
            for( l = 0; l < pGT->iWidth; ++l, ++lIndex )
            {
               if( pGT->prevBuffer[ lIndex ].uiValue !=
                   ( uiValue = zh_ctw_gt_cellValue( pGT, i, l ) ) )
               {
                  pGT->prevBuffer[ lIndex ].uiValue = uiValue;
                  s = r = l;
                  while( ++l < pGT->iWidth )
                  {
                     ++lIndex;
                     if( pGT->prevBuffer[ lIndex ].uiValue !=
                         ( uiValue = zh_ctw_gt_cellValue( pGT, i, l ) ) )
                     {
                        pGT->prevBuffer[ lIndex ].uiValue = uiValue;
                        r = l;
                     }
                     else if( pGT->iRedrawMax != 0 && l - r >= pGT->iRedrawMax )
                        break;
                  }
                  ZH_GTSELF_REDRAW( pGT, i, s, r - s + 1 );
               }
            }
            pGT->pLines[ i ] = ZH_FALSE;
         }
      }
      pGT->fRefresh = ZH_FALSE;
   }
}

/* Public functions */

ZH_BOOL zh_ctwInit( void )
{
   PZH_GTCTW pCTW = zh_ctw_base();

   if( pCTW )
      zh_gt_BaseFree( pCTW->pGT );
   return pCTW != NULL;
}

int  zh_ctwSetShadowAttr( int iAttr )
{
   int iResult = -1;
   PZH_GTCTW pCTW = zh_ctw_base();

   if( pCTW )
   {
      iResult = zh_ctw_SetShadowAttr( pCTW, iAttr );
      zh_gt_BaseFree( pCTW->pGT );
   }
   return iResult;
}

int  zh_ctwSetMoveMode( int iMode )
{
   int iResult = -1;
   PZH_GTCTW pCTW = zh_ctw_base();

   if( pCTW )
   {
      iResult = zh_ctw_SetMoveMode( pCTW, iMode );
      zh_gt_BaseFree( pCTW->pGT );
   }
   return iResult;
}

int  zh_ctwSetMoveStep( int iVertical, int iHorizontal )
{
   int iResult = -1;
   PZH_GTCTW pCTW = zh_ctw_base();

   if( pCTW )
   {
      iResult = zh_ctw_SetMoveStep( pCTW, iVertical, iHorizontal );
      zh_gt_BaseFree( pCTW->pGT );
   }
   return iResult;
}

int  zh_ctwSetWindowBoard( int iTop, int iLeft, int iBottom, int iRight )
{
   int iResult = -1;
   PZH_GTCTW pCTW = zh_ctw_base();

   if( pCTW )
   {
      iResult = zh_ctw_SetWindowBoard( pCTW, iTop, iLeft, iBottom, iRight );
      ZH_GTSELF_FLUSH( pCTW->pGT );
      zh_gt_BaseFree( pCTW->pGT );
   }
   return iResult;
}

int  zh_ctwSetBorderMode( int iTop, int iLeft, int iBottom, int iRight )
{
   int iResult = -1;
   PZH_GTCTW pCTW = zh_ctw_base();

   if( pCTW )
   {
      iResult = zh_ctw_SetBorderMode( pCTW, iTop, iLeft, iBottom, iRight );
      zh_gt_BaseFree( pCTW->pGT );
   }
   return iResult;
}

int  zh_ctwCreateWindow( int iTop, int iLeft, int iBottom, int iRight, ZH_BOOL fClear, int iColor, ZH_BOOL fVisible )
{
   int iResult = -1;

   if( iTop <= iBottom && iLeft <= iRight )
   {
      PZH_GTCTW pCTW = zh_ctw_base();

      if( pCTW )
      {
         iResult = zh_ctw_CreateWindow( pCTW, iTop, iLeft, iBottom, iRight, fClear, iColor, fVisible );
         ZH_GTSELF_FLUSH( pCTW->pGT );
         zh_gt_BaseFree( pCTW->pGT );
      }
   }
   return iResult;
}

int  zh_ctwCloseAllWindows( void )
{
   int iResult = -1;
   PZH_GTCTW pCTW = zh_ctw_base();

   if( pCTW )
   {
      iResult = zh_ctw_CloseAllWindows( pCTW );
      ZH_GTSELF_FLUSH( pCTW->pGT );
      zh_gt_BaseFree( pCTW->pGT );
   }
   return iResult;
}

int  zh_ctwCloseWindow( int iWindow )
{
   int iResult = -1;
   PZH_GTCTW pCTW = zh_ctw_base();

   if( pCTW )
   {
      iResult = zh_ctw_CloseWindow( pCTW, iWindow );
      ZH_GTSELF_FLUSH( pCTW->pGT );
      zh_gt_BaseFree( pCTW->pGT );
   }
   return iResult;
}

int  zh_ctwCurrentWindow( void )
{
   int iResult = 0;
   PZH_GTCTW pCTW = zh_ctw_base();

   if( pCTW )
   {
      iResult = zh_ctw_CurrentWindow( pCTW );
      zh_gt_BaseFree( pCTW->pGT );
   }
   return iResult;
}

int  zh_ctwSelectWindow( int iWindow, ZH_BOOL fToTop )
{
   int iResult = 0;
   PZH_GTCTW pCTW = zh_ctw_base();

   if( pCTW )
   {
      iResult = zh_ctw_SelectWindow( pCTW, iWindow, fToTop );
      ZH_GTSELF_FLUSH( pCTW->pGT );
      zh_gt_BaseFree( pCTW->pGT );
   }
   return iResult;
}

int  zh_ctwChangeWindowHandle( int iNewWindow )
{
   int iResult = -1;
   PZH_GTCTW pCTW = zh_ctw_base();

   if( pCTW )
   {
      iResult = zh_ctw_ChangeWindowHandle( pCTW, iNewWindow );
      zh_gt_BaseFree( pCTW->pGT );
   }
   return iResult;
}

int  zh_ctwGetWindowStack( const int ** piStack )
{
   int iResult = -1;
   PZH_GTCTW pCTW = zh_ctw_base();

   if( pCTW )
   {
      iResult = zh_ctw_GetWindowStack( pCTW, piStack );
      zh_gt_BaseFree( pCTW->pGT );
   }
   return iResult;
}

int  zh_ctwVisible( int iWindow, int iVisible )
{
   int iResult = ZH_CTW_UNDEF;
   PZH_GTCTW pCTW = zh_ctw_base();

   if( pCTW )
   {
      iResult = zh_ctw_Visible( pCTW, iWindow, iVisible );
      ZH_GTSELF_FLUSH( pCTW->pGT );
      zh_gt_BaseFree( pCTW->pGT );
   }
   return iResult;
}

int  zh_ctwSetWindowLevel( int iWindow, int iLevel )
{
   int iResult = ZH_CTW_UNDEF;
   PZH_GTCTW pCTW = zh_ctw_base();

   if( pCTW )
   {
      iResult = zh_ctw_SetWindowLevel( pCTW, iWindow, iLevel );
      ZH_GTSELF_FLUSH( pCTW->pGT );
      zh_gt_BaseFree( pCTW->pGT );
   }
   return iResult;
}

int  zh_ctwSetWindowShadow( int iWindow, int iAttr )
{
   int iResult = ZH_CTW_UNDEF;
   PZH_GTCTW pCTW = zh_ctw_base();

   if( pCTW )
   {
      iResult = zh_ctw_SetWindowShadow( pCTW, iWindow, iAttr );
      ZH_GTSELF_FLUSH( pCTW->pGT );
      zh_gt_BaseFree( pCTW->pGT );
   }
   return iResult;
}

int  zh_ctwMaxWindow( void )
{
   int iResult = 0;
   PZH_GTCTW pCTW = zh_ctw_base();

   if( pCTW )
   {
      iResult = zh_ctw_MaxWindow( pCTW );
      zh_gt_BaseFree( pCTW->pGT );
   }
   return iResult;
}

int  zh_ctwChangeMargins( int iWindow, int iTop, int iLeft, int iBottom, int iRight )
{
   int iResult = -1;
   PZH_GTCTW pCTW = zh_ctw_base();

   if( pCTW )
   {
      iResult = zh_ctw_ChangeMargins( pCTW, iWindow, iTop, iLeft, iBottom, iRight );
      zh_gt_BaseFree( pCTW->pGT );
   }
   return iResult;
}

int  zh_ctwSetWindowClip( int iWindow, int iTop, int iLeft, int iBottom, int iRight )
{
   int iResult = -1;
   PZH_GTCTW pCTW = zh_ctw_base();

   if( pCTW )
   {
      iResult = zh_ctw_SetWindowClip( pCTW, iWindow, iTop, iLeft, iBottom, iRight );
      ZH_GTSELF_FLUSH( pCTW->pGT );
      zh_gt_BaseFree( pCTW->pGT );
   }
   return iResult;
}

int  zh_ctwGetWindowCords( int iWindow, ZH_BOOL fCenter, int * piTop, int * piLeft, int * piBottom, int * piRight )
{
   int iResult = -1;
   PZH_GTCTW pCTW = zh_ctw_base();

   if( pCTW )
   {
      iResult = zh_ctw_GetWindowCords( pCTW, iWindow, fCenter, piTop, piLeft, piBottom, piRight );
      zh_gt_BaseFree( pCTW->pGT );
   }
   return iResult;
}

int  zh_ctwGetFormatCords( int iWindow, ZH_BOOL fRelative, int * piTop, int * piLeft, int * piBottom, int * piRight )
{
   int iResult = -1;
   PZH_GTCTW pCTW = zh_ctw_base();

   if( pCTW )
   {
      iResult = zh_ctw_GetFormatCords( pCTW, iWindow, fRelative, piTop, piLeft, piBottom, piRight );
      zh_gt_BaseFree( pCTW->pGT );
   }
   return iResult;
}

int  zh_ctwMoveWindow( int iWindow, int iRow, int iCol )
{
   int iResult = -1;
   PZH_GTCTW pCTW = zh_ctw_base();

   if( pCTW )
   {
      iResult = zh_ctw_MoveWindow( pCTW, iWindow, iRow, iCol );
      ZH_GTSELF_FLUSH( pCTW->pGT );
      zh_gt_BaseFree( pCTW->pGT );
   }
   return iResult;
}

int  zh_ctwCenterWindow( int iWindow, ZH_BOOL fCenter )
{
   int iResult = -1;
   PZH_GTCTW pCTW = zh_ctw_base();

   if( pCTW )
   {
      iResult = zh_ctw_CenterWindow( pCTW, iWindow, fCenter );
      ZH_GTSELF_FLUSH( pCTW->pGT );
      zh_gt_BaseFree( pCTW->pGT );
   }
   return iResult;
}

int  zh_ctwAddWindowBox( int iWindow, const ZH_WCHAR * szBoxW, int iColor )
{
   int iResult = -1;
   PZH_GTCTW pCTW = zh_ctw_base();

   if( pCTW )
   {
      iResult = zh_ctw_AddWindowBox( pCTW, iWindow, szBoxW, iColor );
      ZH_GTSELF_FLUSH( pCTW->pGT );
      zh_gt_BaseFree( pCTW->pGT );
   }
   return iResult;
}

int  zh_ctwSwapWindows( int iWindow1, int iWindow2 )
{
   int iResult = -1;
   PZH_GTCTW pCTW = zh_ctw_base();

   if( pCTW )
   {
      iResult = zh_ctw_SwapWindows( pCTW, iWindow1, iWindow2 );
      ZH_GTSELF_FLUSH( pCTW->pGT );
      zh_gt_BaseFree( pCTW->pGT );
   }
   return iResult;
}

int  zh_ctwGetPosWindow( int iRow, int iCol )
{
   int iResult = -1;
   PZH_GTCTW pCTW = zh_ctw_base();

   if( pCTW )
   {
      if( pCTW->iMaxWindow &&
          iRow >= pCTW->iBoardTop  && iRow <= pCTW->iBoardBottom &&
          iCol >= pCTW->iBoardLeft && iCol <= pCTW->iBoardRight )
      {
         long lIndex = ( long ) iRow * pCTW->iMapWidth + iCol;
         iResult = pCTW->pWindowMap[ lIndex ];
      }
      zh_gt_BaseFree( pCTW->pGT );
   }
   return iResult;
}

int  zh_ctwLastKey( int * piNewKey )
{
   /* keyread() in CT3 uses 64512 bytes length buffer
    * when it reach this limit and new key is added the
    * buffer size is decreased by 1024 to 63488 bytes
    * before adding key. TODO: check if buffer is shifted
    */
   int iResult = 0;
   PZH_GTCTW pCTW = zh_ctw_base();

   if( pCTW )
   {
      iResult = pCTW->iLastKey;
      if( piNewKey )
         pCTW->iLastKey = *piNewKey;
      zh_gt_BaseFree( pCTW->pGT );
   }
   return iResult;
}

static ZH_BOOL zh_gt_FuncInit( PZH_GT_FUNCS pFuncTable )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_FuncInit(%p)", ( void * ) pFuncTable ) );

   pFuncTable->Exit           = zh_ctw_gt_Exit;
   pFuncTable->MouseRow       = zh_ctw_MouseRow;
   pFuncTable->MouseCol       = zh_ctw_MouseCol;
   pFuncTable->MaxCol         = zh_ctw_gt_MaxCol;
   pFuncTable->MaxRow         = zh_ctw_gt_MaxRow;
   pFuncTable->GetPos         = zh_ctw_gt_GetPos;
   pFuncTable->SetPos         = zh_ctw_gt_SetPos;
   pFuncTable->WriteCon       = zh_ctw_gt_WriteCon;
   pFuncTable->WriteConW      = zh_ctw_gt_WriteConW;
   pFuncTable->GetCursorStyle = zh_ctw_gt_GetCursorStyle;
   pFuncTable->SetCursorStyle = zh_ctw_gt_SetCursorStyle;
   pFuncTable->GetColorStr    = zh_ctw_gt_GetColorStr;
   pFuncTable->SetColorStr    = zh_ctw_gt_SetColorStr;
   pFuncTable->ColorSelect    = zh_ctw_gt_ColorSelect;
   pFuncTable->GetColor       = zh_ctw_gt_GetColor;
   pFuncTable->GetColorData   = zh_ctw_gt_GetColorData;
   pFuncTable->GetScrCursor   = zh_ctw_gt_GetScrCursor;
   pFuncTable->GetScrChar     = zh_ctw_gt_GetScrChar;
   pFuncTable->GetScrUC       = zh_ctw_gt_GetScrUC;
   pFuncTable->GetChar        = zh_ctw_gt_GetChar;
   pFuncTable->PutChar        = zh_ctw_gt_PutChar;
   pFuncTable->Resize         = zh_ctw_gt_Resize;
   pFuncTable->Info           = zh_ctw_gt_Info;
   pFuncTable->Alert          = zh_ctw_gt_Alert;
   pFuncTable->ReadKey        = zh_ctw_gt_ReadKey;
   pFuncTable->RedrawDiff     = zh_ctw_gt_RedrawDiff;

   return ZH_TRUE;
}

/* --- */

#define ZH_GTSUPER  NULL
#include "zh_gt_reg.h"

/* --- */
