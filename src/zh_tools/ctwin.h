/*
 * Header file for Clipper Tools like window system
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

#ifndef ZH_CTWIN_H_
#define ZH_CTWIN_H_

#define ZH_CTW_UNDEF         -1
#define ZH_CTW_HIDDEN        0
#define ZH_CTW_VISIBLE       1

#define ZH_CTW_SHADOW_OFF    -1
#define ZH_CTW_SHADOW_UNDEF  -2
#define ZH_CTW_SHADOW_EXT    -3
#define ZH_CTW_SHADOW_EXT2   -4

#define ZH_CTW_BOTTOM        0
#define ZH_CTW_DEFAULT       1
#define ZH_CTW_TOP           2

ZH_EXTERN_BEGIN

extern ZH_EXPORT ZH_BOOL zh_ctwInit( void );
extern ZH_EXPORT int     zh_ctwSetShadowAttr( int iAttr );
extern ZH_EXPORT int     zh_ctwSetMoveMode( int iMode );
extern ZH_EXPORT int     zh_ctwSetMoveStep( int iVertical, int iHorizontal );
extern ZH_EXPORT int     zh_ctwSetWindowBoard( int iTop, int iLeft, int iBottom, int iRight );
extern ZH_EXPORT int     zh_ctwSetBorderMode( int iTop, int iLeft, int iBottom, int iRight );
extern ZH_EXPORT int     zh_ctwCreateWindow( int iTop, int iLeft, int iBottom, int iRight, ZH_BOOL fClear, int iColor, ZH_BOOL fVisible );
extern ZH_EXPORT int     zh_ctwCloseAllWindows( void );
extern ZH_EXPORT int     zh_ctwCloseWindow( int iWindow );
extern ZH_EXPORT int     zh_ctwCurrentWindow( void );
extern ZH_EXPORT int     zh_ctwSelectWindow( int iWindow, ZH_BOOL fToTop );
extern ZH_EXPORT int     zh_ctwChangeWindowHandle( int iNewWindow );
extern ZH_EXPORT int     zh_ctwGetWindowStack( const int ** piStack );
extern ZH_EXPORT int     zh_ctwVisible( int iWindow, int iVisible );
extern ZH_EXPORT int     zh_ctwSetWindowShadow( int iWindow, int iAttr );
extern ZH_EXPORT int     zh_ctwSetWindowLevel( int iWindow, int iLevel );
extern ZH_EXPORT int     zh_ctwMaxWindow( void );
extern ZH_EXPORT int     zh_ctwChangeMargins( int iWindow, int iTop, int iLeft, int iBottom, int iRight );
extern ZH_EXPORT int     zh_ctwSetWindowClip( int iWindow, int iTop, int iLeft, int iBottom, int iRight );
extern ZH_EXPORT int     zh_ctwGetWindowCords( int iWindow, ZH_BOOL fCenter, int * piTop, int * piLeft, int * piBottom, int * piRight );
extern ZH_EXPORT int     zh_ctwGetFormatCords( int iWindow, ZH_BOOL fRelative, int * piTop, int * piLeft, int * piBottom, int * piRight );
extern ZH_EXPORT int     zh_ctwMoveWindow( int iWindow, int iRow, int iCol );
extern ZH_EXPORT int     zh_ctwCenterWindow( int iWindow, ZH_BOOL fCenter );
extern ZH_EXPORT int     zh_ctwAddWindowBox( int iWindow, const ZH_WCHAR * szBoxW, int iColor );
extern ZH_EXPORT int     zh_ctwSwapWindows( int iWindow1, int iWindow2 );
extern ZH_EXPORT int     zh_ctwGetPosWindow( int iRow, int iCol );
extern ZH_EXPORT int     zh_ctwLastKey( int * piNewKey );

ZH_EXTERN_END

#endif /* ZH_CTWIN_H_ */
