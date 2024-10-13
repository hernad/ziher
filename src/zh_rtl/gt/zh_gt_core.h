/*
 * Header file for the Internal Terminal API
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


#ifndef ZH_GTCORE_H_
#define ZH_GTCORE_H_

#include "zh_gt_api.h"
#include "zh_codepage_api.h"

ZH_EXTERN_BEGIN

/* convert lower case suffixes to upper */
#define ZH_GT_nul    ZH_GT_NUL
#define ZH_GT_std    ZH_GT_STD
#define ZH_GT_cgi    ZH_GT_CGI
#define ZH_GT_win    ZH_GT_WIN
#define ZH_GT_wvt    ZH_GT_WVT
#define ZH_GT_tpl    ZH_GT_TPL
#define ZH_GT_trm    ZH_GT_TRM
#define ZH_GT_ele    ZH_GT_ELE
#define ZH_GT_xvt    ZH_GT_XVT
#define ZH_GT_xwc    ZH_GT_XWC
#define ZH_GT_gui    ZH_GT_GUI

/* These hacks are needed to force preprocessing if id/x is also a macro */
#define _ZH_GT_PREF_( id )      _ZH_GT_PREF__( id )
#define _ZH_GT_PREF__( id )     ZH_GT_##id

#define ZH_GT_REQUEST( id )      ZH_GT_REQUEST_( _ZH_GT_PREF_( id ) )
#define ZH_GT_REQUEST_( id )     ZH_GT_REQUEST__( id )
#define ZH_GT_REQUEST__( id )    ZH_FUNC_EXTERN( id ); \
                                 extern void zh_gt_ForceLink_##id( void ); \
                                 void zh_gt_ForceLink_##id( void ) \
                                 { \
                                    ZH_FUNC_EXEC( id ); \
                                 }

#define ZH_GT_ANNOUNCE( id )     ZH_GT_ANNOUNCE_( _ZH_GT_PREF_( id ) )
#define ZH_GT_ANNOUNCE_( id )    ZH_GT_ANNOUNCE__( id )
#define ZH_GT_ANNOUNCE__( id )   ZH_FUNC( id ) {} ZH_FUNC( id##_DEFAULT ) {}

#define ZH_GT_DRVNAME( id )      ZH_MACRO2STRING( id )

#define ZH_GT_FUNC( x )          ZH_GT_FUNC_( x, ZH_GT_NAME )
#define ZH_GT_FUNC_( x, id )     ZH_GT_FUNC__( x, id )
#define ZH_GT_FUNC__( x, id )    zh##_##id##_##x

/* forward declaration */
struct _ZH_GT_BASE;

#define ZH_GT_PTR       struct _ZH_GT_BASE *

typedef struct
{
   ZH_BOOL   (* Lock) ( ZH_GT_PTR );
   void      (* Unlock) ( ZH_GT_PTR );
   void      (* Init) ( ZH_GT_PTR, ZH_FHANDLE, ZH_FHANDLE, ZH_FHANDLE );
   void      (* Exit) ( ZH_GT_PTR );
   void *    (* New) ( ZH_GT_PTR );
   void      (* Free) ( ZH_GT_PTR );
   void      (* Mark) ( ZH_GT_PTR );
   ZH_BOOL   (* Resize) ( ZH_GT_PTR, int, int );
   ZH_BOOL   (* SetMode) ( ZH_GT_PTR, int, int );
   void      (* GetSize) ( ZH_GT_PTR, int *, int * );
   void      (* SemiCold) ( ZH_GT_PTR );
   void      (* ColdArea) ( ZH_GT_PTR, int, int, int, int );
   void      (* ExposeArea) ( ZH_GT_PTR, int, int, int, int );
   void      (* ScrollArea) ( ZH_GT_PTR, int, int, int, int, int, ZH_USHORT, int, int );
   void      (* TouchLine) ( ZH_GT_PTR, int );
   void      (* TouchCell) ( ZH_GT_PTR, int, int );
   void      (* Redraw) ( ZH_GT_PTR, int, int, int );
   void      (* RedrawDiff) ( ZH_GT_PTR );
   void      (* Refresh) ( ZH_GT_PTR );
   void      (* Flush) ( ZH_GT_PTR );
   int       (* MaxCol) ( ZH_GT_PTR );
   int       (* MaxRow) ( ZH_GT_PTR );
   ZH_BOOL   (* CheckPos) ( ZH_GT_PTR, int, int, long * );
   void      (* SetPos) ( ZH_GT_PTR, int, int );
   void      (* GetPos) ( ZH_GT_PTR, int *, int * );
   ZH_BOOL   (* IsColor) ( ZH_GT_PTR );
   void      (* GetColorStr) ( ZH_GT_PTR, char * );
   void      (* SetColorStr) ( ZH_GT_PTR, const char * );
   void      (* ColorSelect) ( ZH_GT_PTR, int );
   int       (* GetColor) ( ZH_GT_PTR );
   int       (* ColorNum) ( ZH_GT_PTR, const char * );
   void      (* ColorsToString) ( ZH_GT_PTR, int *, int, char *, int );
   void      (* StringToColors) ( ZH_GT_PTR, const char *, int **, int * );
   void      (* GetColorData) ( ZH_GT_PTR, int **, int *, int * );
   int       (* GetClearColor) ( ZH_GT_PTR );
   void      (* SetClearColor) ( ZH_GT_PTR, int );
   ZH_USHORT (* GetClearChar) ( ZH_GT_PTR );
   void      (* SetClearChar) ( ZH_GT_PTR, ZH_USHORT );
   int       (* GetCursorStyle) ( ZH_GT_PTR );
   void      (* SetCursorStyle) ( ZH_GT_PTR, int );
   void      (* GetScrCursor) ( ZH_GT_PTR, int *, int *, int * );
   ZH_BOOL   (* GetScrChar) ( ZH_GT_PTR, int, int, int *, ZH_BYTE *, ZH_USHORT * );
   ZH_BOOL   (* PutScrChar) ( ZH_GT_PTR, int, int, int, ZH_BYTE, ZH_USHORT );
   ZH_BOOL   (* GetScrUC) ( ZH_GT_PTR, int, int, int *, ZH_BYTE *, ZH_UCHAR *, ZH_BOOL );
   void      (* DispBegin) ( ZH_GT_PTR );
   void      (* DispEnd) ( ZH_GT_PTR );
   int       (* DispCount) ( ZH_GT_PTR );
   ZH_BOOL   (* GetChar) ( ZH_GT_PTR, int, int, int *, ZH_BYTE *, ZH_USHORT * );
   ZH_BOOL   (* PutChar) ( ZH_GT_PTR, int, int, int, ZH_BYTE, ZH_USHORT );
   long      (* RectSize) ( ZH_GT_PTR, int, int, int, int );
   void      (* Save) ( ZH_GT_PTR, int, int, int, int, void * );
   void      (* Rest) ( ZH_GT_PTR, int, int, int, int, const void * );
   int       (* PutText) ( ZH_GT_PTR, int, int, int, const char *, ZH_SIZE );
   int       (* PutTextW) ( ZH_GT_PTR, int, int, int, const ZH_WCHAR *, ZH_SIZE );
   void      (* Replicate) ( ZH_GT_PTR, int, int, int, ZH_BYTE, ZH_USHORT, ZH_SIZE );
   void      (* WriteAt) ( ZH_GT_PTR, int, int, const char *, ZH_SIZE );
   void      (* WriteAtW) ( ZH_GT_PTR, int, int, const ZH_WCHAR *, ZH_SIZE );
   void      (* Write) ( ZH_GT_PTR, const char *, ZH_SIZE );
   void      (* WriteW) ( ZH_GT_PTR, const ZH_WCHAR *, ZH_SIZE );
   void      (* WriteCon) ( ZH_GT_PTR, const char *, ZH_SIZE );
   void      (* WriteConW) ( ZH_GT_PTR, const ZH_WCHAR *, ZH_SIZE );
   void      (* SetAttribute) ( ZH_GT_PTR, int, int, int, int, int );
   void      (* DrawShadow) ( ZH_GT_PTR, int, int, int, int, int );
   void      (* Scroll) ( ZH_GT_PTR, int, int, int, int, int, ZH_USHORT, int, int );
   void      (* ScrollUp) ( ZH_GT_PTR, int, int, ZH_USHORT );
   void      (* Box) ( ZH_GT_PTR, int, int, int, int, const char *, int );
   void      (* BoxW) ( ZH_GT_PTR, int, int, int, int, const ZH_WCHAR *, int );
   void      (* BoxD) ( ZH_GT_PTR, int, int, int, int, const char *, int );
   void      (* BoxS) ( ZH_GT_PTR, int, int, int, int, const char *, int );
   void      (* HorizLine) ( ZH_GT_PTR, int, int, int, ZH_USHORT, int );
   void      (* VertLine) ( ZH_GT_PTR, int, int, int, ZH_USHORT, int );
   ZH_BOOL   (* GetBlink) ( ZH_GT_PTR );
   void      (* SetBlink) ( ZH_GT_PTR, ZH_BOOL );
   void      (* SetSnowFlag) ( ZH_GT_PTR, ZH_BOOL );
   const char * (* Version) ( ZH_GT_PTR, int );
   ZH_BOOL   (* Suspend) ( ZH_GT_PTR );
   ZH_BOOL   (* Resume) ( ZH_GT_PTR );
   ZH_BOOL   (* PreExt) ( ZH_GT_PTR );
   ZH_BOOL   (* PostExt) ( ZH_GT_PTR );
   void      (* OutStd) ( ZH_GT_PTR, const char *, ZH_SIZE );
   void      (* OutErr) ( ZH_GT_PTR, const char *, ZH_SIZE );
   void      (* Tone) ( ZH_GT_PTR, double, double );
   void      (* Bell) ( ZH_GT_PTR );
   ZH_BOOL   (* Info) ( ZH_GT_PTR, int, PZH_GT_INFO );
   int       (* Alert) ( ZH_GT_PTR, PZH_ITEM, PZH_ITEM, int, int, double );
   int       (* SetFlag) ( ZH_GT_PTR, int, int );

   /* internationalization */
   ZH_BOOL   (* SetDispCP) ( ZH_GT_PTR, const char *, const char *, ZH_BOOL );
   ZH_BOOL   (* SetKeyCP) ( ZH_GT_PTR, const char *, const char * );

   /* keyboard */
   int       (* ReadKey) ( ZH_GT_PTR, int );

   int       (* InkeyGet) ( ZH_GT_PTR, ZH_BOOL fWait, double dSeconds, int iEventMask );
   void      (* InkeyPut) ( ZH_GT_PTR, int iKey );
   void      (* InkeyIns) ( ZH_GT_PTR, int iKey );
   int       (* InkeyLast) ( ZH_GT_PTR, int iEventMask );
   int       (* InkeyNext) ( ZH_GT_PTR, int iEventMask );
   void      (* InkeyPoll) ( ZH_GT_PTR );
   void      (* InkeySetText) ( ZH_GT_PTR, const char * szText, ZH_SIZE nLen );
   int       (* InkeySetLast) ( ZH_GT_PTR, int iKey );
   void      (* InkeyReset) ( ZH_GT_PTR );
   void      (* InkeyExit) ( ZH_GT_PTR );

   /* mouse */
   void      (* MouseInit) ( ZH_GT_PTR );
   void      (* MouseExit) ( ZH_GT_PTR );
   ZH_BOOL   (* MouseIsPresent) ( ZH_GT_PTR );
   void      (* MouseShow) ( ZH_GT_PTR );
   void      (* MouseHide) ( ZH_GT_PTR );
   ZH_BOOL   (* MouseGetCursor) ( ZH_GT_PTR );
   void      (* MouseSetCursor) ( ZH_GT_PTR, ZH_BOOL );
   int       (* MouseCol) ( ZH_GT_PTR );
   int       (* MouseRow) ( ZH_GT_PTR );
   void      (* MouseGetPos) ( ZH_GT_PTR, int *, int * );
   void      (* MouseSetPos) ( ZH_GT_PTR, int, int );
   void      (* MouseSetBounds) ( ZH_GT_PTR, int, int, int, int );
   void      (* MouseGetBounds) ( ZH_GT_PTR, int *, int *, int *, int * );
   int       (* MouseStorageSize) ( ZH_GT_PTR );
   void      (* MouseSaveState) ( ZH_GT_PTR, void * );
   void      (* MouseRestoreState) ( ZH_GT_PTR, const void * );
   int       (* MouseGetDoubleClickSpeed) ( ZH_GT_PTR );
   void      (* MouseSetDoubleClickSpeed) ( ZH_GT_PTR, int );
   int       (* MouseCountButton) ( ZH_GT_PTR );
   ZH_BOOL   (* MouseButtonState) ( ZH_GT_PTR, int );
   ZH_BOOL   (* MouseButtonPressed) ( ZH_GT_PTR, int, int *, int * );
   ZH_BOOL   (* MouseButtonReleased) ( ZH_GT_PTR, int, int *, int * );
   int       (* MouseReadKey) ( ZH_GT_PTR, int );

   /* Graphics API */
   int       (* GfxPrimitive) ( ZH_GT_PTR, int, int, int, int, int, int );
   void      (* GfxText) ( ZH_GT_PTR, int, int, const char *, int, int, int );

   void      (* WhoCares) ( ZH_GT_PTR, void * );

} ZH_GT_FUNCS, * PZH_GT_FUNCS;

typedef int ( * GTENTRYP_V )( void );

#define GTFUNCSCOUNT   ( sizeof( ZH_GT_FUNCS ) / sizeof( GTENTRYP_V ) )

#define ZH_GT_MAX_      32
#define ZH_GT_NAME_MAX_ 8

typedef struct _ZH_GT_INIT
{
   const char     * id;
   ZH_BOOL        (* init) ( PZH_GT_FUNCS );
   PZH_GT_FUNCS   pSuperTable;
   int *          pGtId;
} ZH_GT_INIT, * PZH_GT_INIT;


typedef union
{
   struct
   {
      ZH_U16   usChar;
      ZH_BYTE  bColor;
      ZH_BYTE  bAttr;
   } c;
   ZH_U32   uiValue;
} ZH_SCREENCELL;
typedef ZH_SCREENCELL * PZH_SCREENCELL;


typedef struct _ZH_GT_BASE
{
   PZH_GT_FUNCS   pFuncTable;

   PZH_ITEM       pMutex;
   int            iUsed;

   int            iRow;             /* cursor row position */
   int            iCol;             /* cursor column position */

   int            iHeight;          /* window height */
   int            iWidth;           /* window width */

   PZH_SCREENCELL screenBuffer;     /* window foreground (board) current buffer */
   PZH_SCREENCELL prevBuffer;       /* window foreground (board) previous buffer */

   ZH_BOOL *      pLines;           /* touched Window lines */
   ZH_BOOL        fRefresh;         /* should Window be refreshed */
   int            iRedrawMax;       /* maximum number of unchanged neigzhoring chars in redrawn line */

   ZH_BOOL        fVgaCell;
   ZH_BOOL        fIsColor;
   ZH_BOOL        fBlinking;
   ZH_BOOL        fStdOutCon;
   ZH_BOOL        fStdErrCon;
   int            iCursorShape;
   int            iDispCount;
   int            iExtCount;
   ZH_USHORT      usClearChar;
   int            iClearColor;
   ZH_FHANDLE     hStdIn;
   ZH_FHANDLE     hStdOut;
   ZH_FHANDLE     hStdErr;

   ZH_BOOL        fDispTrans;
   PZH_CODEPAGE   cdpTerm;
   PZH_CODEPAGE   cdpHost;
   PZH_CODEPAGE   cdpBox;
   PZH_CODEPAGE   cdpIn;

   int            iColorIndex;
   int            iColorCount;
   int *          pColor;

   int            iDoubleClickSpeed; /* In milliseconds */
   ZH_BOOL        fMouseVisible;
   int            iMouseLastRow;
   int            iMouseLastCol;
   ZH_MAXINT      nMouseLeftTimer;
   ZH_MAXINT      nMouseRightTimer;
   ZH_MAXINT      nMouseMiddleTimer;

   int            defaultKeyBuffer[ ZH_DEFAULT_INKEY_BUFSIZE + 1 ];

   int *          inkeyBuffer;
   int            inkeyBufferSize;
   int            inkeyHead;
   int            inkeyTail;
   int            inkeyLastPos;
   int            inkeyLast;
   ZH_WCHAR *     StrBuffer;
   ZH_SIZE        StrBufferSize;
   ZH_SIZE        StrBufferPos;

   PZH_ITEM       pNotifierBlock;
   PZH_ITEM       pInkeyFilterBlock;
   PZH_ITEM       pInkeyReadBlock;
   PZH_ITEM       pCargo;

   void *         pGTData[ ZH_GT_MAX_ ];    /* local GT data */

} ZH_GT_BASE, * PZH_GT_BASE, * PZH_GT;

extern ZH_EXPORT PZH_GT zh_gt_Base( void );
extern ZH_EXPORT void zh_gt_BaseFree( PZH_GT pGT );
extern ZH_EXPORT void zh_gt_BaseUnlock( PZH_GT pGT );
extern ZH_EXPORT void zh_gt_BaseLock( PZH_GT pGT );
extern ZH_EXPORT void zh_gtSleep( PZH_GT pGT, double dSeconds );
extern ZH_EXPORT PZH_GT zh_gt_ItemBase( PZH_ITEM pItemGT );
extern ZH_EXPORT void zh_gt_gcMark( void );

#define ZH_GTLOCAL(g)   (g)->pGTData[*ZH_GTID_PTR]

#define ZH_GTSELF_TERMCP(g)                     ((g)->cdpTerm ? (g)->cdpTerm : ZH_GTSELF_HOSTCP(g))
#define ZH_GTSELF_HOSTCP(g)                     ((g)->cdpHost ? (g)->cdpHost : zh_vmCodepage())
#define ZH_GTSELF_BOXCP(g)                      ((g)->cdpBox ? (g)->cdpBox : ZH_GTSELF_HOSTCP(g))
#define ZH_GTSELF_INCP(g)                       ((g)->cdpIn ? (g)->cdpIn : zh_vmCodepage())

#define ZH_GTSELF_CPTERM(g)                     ((g)->cdpTerm)
#define ZH_GTSELF_CPHOST(g)                     ((g)->cdpHost)
#define ZH_GTSELF_CPBOX(g)                      ((g)->cdpBox)
#define ZH_GTSELF_CPIN(g)                       ((g)->cdpIn)

#define ZH_GTSELF_KEYTRANS(g,k)                 (((k)>=127 && (k)<=255 && (g)->cdpIn) ? zh_cdpGetWC((g)->cdpIn,(ZH_UCHAR)(k),0) : 0)


#define ZH_GTSELF_LOCK(g)                       (g)->pFuncTable->Lock(g)
#define ZH_GTSELF_UNLOCK(g)                     (g)->pFuncTable->Unlock(g)
#define ZH_GTSELF_INIT(g,i,o,e)                 (g)->pFuncTable->Init(g,i,o,e)
#define ZH_GTSELF_EXIT(g)                       (g)->pFuncTable->Exit(g)
#define ZH_GTSELF_NEW(g)                        (g)->pFuncTable->New(g)
#define ZH_GTSELF_FREE(g)                       (g)->pFuncTable->Free(g)
#define ZH_GTSELF_MARK(g)                       (g)->pFuncTable->Mark(g)
#define ZH_GTSELF_RESIZE(g,r,c)                 (g)->pFuncTable->Resize(g,r,c)
#define ZH_GTSELF_SETMODE(g,r,c)                (g)->pFuncTable->SetMode(g,r,c)
#define ZH_GTSELF_GETSIZE(g,pr,pc)              (g)->pFuncTable->GetSize(g,pr,pc)
#define ZH_GTSELF_SEMICOLD(g)                   (g)->pFuncTable->SemiCold(g)
#define ZH_GTSELF_COLDAREA(g,t,l,b,r)           (g)->pFuncTable->ColdArea(g,t,l,b,r)
#define ZH_GTSELF_EXPOSEAREA(g,t,l,b,r)         (g)->pFuncTable->ExposeArea(g,t,l,b,r)
#define ZH_GTSELF_SCROLLAREA(g,t,l,b,r,m,u,v,h) (g)->pFuncTable->ScrollArea(g,t,l,b,r,m,u,v,h)
#define ZH_GTSELF_TOUCHLINE(g,r)                (g)->pFuncTable->TouchLine(g,r)
#define ZH_GTSELF_TOUCHCELL(g,r,c)              (g)->pFuncTable->TouchCell(g,r,c)
#define ZH_GTSELF_REDRAW(g,r,c,l)               (g)->pFuncTable->Redraw(g,r,c,l)
#define ZH_GTSELF_REDRAWDIFF(g)                 (g)->pFuncTable->RedrawDiff(g)
#define ZH_GTSELF_REFRESH(g)                    (g)->pFuncTable->Refresh(g)
#define ZH_GTSELF_FLUSH(g)                      (g)->pFuncTable->Flush(g)
#define ZH_GTSELF_MAXCOL(g)                     (g)->pFuncTable->MaxCol(g)
#define ZH_GTSELF_MAXROW(g)                     (g)->pFuncTable->MaxRow(g)
#define ZH_GTSELF_CHECKPOS(g,r,c,l)             (g)->pFuncTable->CheckPos(g,r,c,l)
#define ZH_GTSELF_SETPOS(g,r,c)                 (g)->pFuncTable->SetPos(g,r,c)
#define ZH_GTSELF_GETPOS(g,pr,pc)               (g)->pFuncTable->GetPos(g,pr,pc)
#define ZH_GTSELF_ISCOLOR(g)                    (g)->pFuncTable->IsColor(g)
#define ZH_GTSELF_GETCOLORSTR(g,s)              (g)->pFuncTable->GetColorStr(g,s)
#define ZH_GTSELF_SETCOLORSTR(g,s)              (g)->pFuncTable->SetColorStr(g,s)
#define ZH_GTSELF_COLORSELECT(g,c)              (g)->pFuncTable->ColorSelect(g,c)
#define ZH_GTSELF_GETCOLOR(g)                   (g)->pFuncTable->GetColor(g)
#define ZH_GTSELF_COLORNUM(g,s)                 (g)->pFuncTable->ColorNum(g,s)
#define ZH_GTSELF_COLORSTOSTRING(g,pc,i,ps,n)   (g)->pFuncTable->ColorsToString(g,pc,i,ps,n)
#define ZH_GTSELF_STRINGTOCOLORS(g,ps,pc,pi)    (g)->pFuncTable->StringToColors(g,ps,pc,pi)
#define ZH_GTSELF_GETCOLORDATA(g,pc,pn,pi)      (g)->pFuncTable->GetColorData(g,pc,pn,pi)
#define ZH_GTSELF_GETCLEARCOLOR(g)              (g)->pFuncTable->GetClearColor(g)
#define ZH_GTSELF_SETCLEARCOLOR(g,c)            (g)->pFuncTable->SetClearColor(g,c)
#define ZH_GTSELF_GETCLEARCHAR(g)               (g)->pFuncTable->GetClearChar(g)
#define ZH_GTSELF_SETCLEARCHAR(g,c)             (g)->pFuncTable->SetClearChar(g,c)
#define ZH_GTSELF_GETCURSORSTYLE(g)             (g)->pFuncTable->GetCursorStyle(g)
#define ZH_GTSELF_SETCURSORSTYLE(g,s)           (g)->pFuncTable->SetCursorStyle(g,s)
#define ZH_GTSELF_GETSCRCURSOR(g,pr,pc,ps)      (g)->pFuncTable->GetScrCursor(g,pr,pc,ps)
#define ZH_GTSELF_GETSCRCHAR(g,r,c,pm,pa,pc)    (g)->pFuncTable->GetScrChar(g,r,c,pm,pa,pc)
#define ZH_GTSELF_PUTSCRCHAR(g,r,c,m,a,u)       (g)->pFuncTable->PutScrChar(g,r,c,m,a,u)
#define ZH_GTSELF_GETSCRUC(g,r,c,pm,pa,pc,f)    (g)->pFuncTable->GetScrUC(g,r,c,pm,pa,pc,f)
#define ZH_GTSELF_DISPBEGIN(g)                  (g)->pFuncTable->DispBegin(g)
#define ZH_GTSELF_DISPEND(g)                    (g)->pFuncTable->DispEnd(g)
#define ZH_GTSELF_DISPCOUNT(g)                  (g)->pFuncTable->DispCount(g)
#define ZH_GTSELF_GETCHAR(g,r,c,pm,pa,pc)       (g)->pFuncTable->GetChar(g,r,c,pm,pa,pc)
#define ZH_GTSELF_PUTCHAR(g,r,c,m,a,u)          (g)->pFuncTable->PutChar(g,r,c,m,a,u)
#define ZH_GTSELF_RECTSIZE(g,t,l,b,r)           (g)->pFuncTable->RectSize(g,t,l,b,r)
#define ZH_GTSELF_SAVE(g,t,l,b,r,p)             (g)->pFuncTable->Save(g,t,l,b,r,p)
#define ZH_GTSELF_REST(g,t,l,b,r,p)             (g)->pFuncTable->Rest(g,t,l,b,r,p)
#define ZH_GTSELF_PUTTEXT(g,r,c,m,s,l)          (g)->pFuncTable->PutText(g,r,c,m,s,l)
#define ZH_GTSELF_PUTTEXTW(g,r,c,m,s,l)         (g)->pFuncTable->PutTextW(g,r,c,m,s,l)
#define ZH_GTSELF_REPLICATE(g,r,c,m,a,u,l)      (g)->pFuncTable->Replicate(g,r,c,m,a,u,l)
#define ZH_GTSELF_WRITEAT(g,r,c,s,l)            (g)->pFuncTable->WriteAt(g,r,c,s,l)
#define ZH_GTSELF_WRITEATW(g,r,c,s,l)           (g)->pFuncTable->WriteAtW(g,r,c,s,l)
#define ZH_GTSELF_WRITE(g,s,l)                  (g)->pFuncTable->Write(g,s,l)
#define ZH_GTSELF_WRITEW(g,s,l)                 (g)->pFuncTable->WriteW(g,s,l)
#define ZH_GTSELF_WRITECON(g,s,l)               (g)->pFuncTable->WriteCon(g,s,l)
#define ZH_GTSELF_WRITECONW(g,s,l)              (g)->pFuncTable->WriteConW(g,s,l)
#define ZH_GTSELF_SETATTRIBUTE(g,t,l,b,r,m)     (g)->pFuncTable->SetAttribute(g,t,l,b,r,m)
#define ZH_GTSELF_DRAWSHADOW(g,t,l,b,r,m)       (g)->pFuncTable->DrawShadow(g,t,l,b,r,m)
#define ZH_GTSELF_SCROLL(g,t,l,b,r,m,u,v,h)     (g)->pFuncTable->Scroll(g,t,l,b,r,m,u,v,h)
#define ZH_GTSELF_SCROLLUP(g,r,m,u)             (g)->pFuncTable->ScrollUp(g,r,m,u)
#define ZH_GTSELF_BOX(g,t,l,b,r,f,m)            (g)->pFuncTable->Box(g,t,l,b,r,f,m)
#define ZH_GTSELF_BOXW(g,t,l,b,r,f,m)           (g)->pFuncTable->BoxW(g,t,l,b,r,f,m)
#define ZH_GTSELF_BOXD(g,t,l,b,r,f,m)           (g)->pFuncTable->BoxD(g,t,l,b,r,f,m)
#define ZH_GTSELF_BOXS(g,t,l,b,r,f,m)           (g)->pFuncTable->BoxS(g,t,l,b,r,f,m)
#define ZH_GTSELF_HORIZLINE(g,h,l,r,u,m)        (g)->pFuncTable->HorizLine(g,h,l,r,u,m)
#define ZH_GTSELF_VERTLINE(g,c,t,b,u,m)         (g)->pFuncTable->VertLine(g,c,t,b,u,m)
#define ZH_GTSELF_GETBLINK(g)                   (g)->pFuncTable->GetBlink(g)
#define ZH_GTSELF_SETBLINK(g,b)                 (g)->pFuncTable->SetBlink(g,b)
#define ZH_GTSELF_SETSNOWFLAG(g,b)              (g)->pFuncTable->SetSnowFlag(g,b)
#define ZH_GTSELF_VERSION(g,i)                  (g)->pFuncTable->Version(g,i)
#define ZH_GTSELF_SUSPEND(g)                    (g)->pFuncTable->Suspend(g)
#define ZH_GTSELF_RESUME(g)                     (g)->pFuncTable->Resume(g)
#define ZH_GTSELF_PREEXT(g)                     (g)->pFuncTable->PreExt(g)
#define ZH_GTSELF_POSTEXT(g)                    (g)->pFuncTable->PostExt(g)
#define ZH_GTSELF_OUTSTD(g,s,l)                 (g)->pFuncTable->OutStd(g,s,l)
#define ZH_GTSELF_OUTERR(g,s,l)                 (g)->pFuncTable->OutErr(g,s,l)
#define ZH_GTSELF_TONE(g,f,d)                   (g)->pFuncTable->Tone(g,f,d)
#define ZH_GTSELF_BELL(g)                       (g)->pFuncTable->Bell(g)
#define ZH_GTSELF_INFO(g,i,p)                   (g)->pFuncTable->Info(g,i,p)
#define ZH_GTSELF_ALERT(g,m,o,n,h,d)            (g)->pFuncTable->Alert(g,m,o,n,h,d)
#define ZH_GTSELF_SETFLAG(g,i,f)                (g)->pFuncTable->SetFlag(g,i,f)
#define ZH_GTSELF_SETDISPCP(g,t,h,b)            (g)->pFuncTable->SetDispCP(g,t,h,b)
#define ZH_GTSELF_SETKEYCP(g,t,h)               (g)->pFuncTable->SetKeyCP(g,t,h)
#define ZH_GTSELF_READKEY(g,m)                  (g)->pFuncTable->ReadKey(g,m)
#define ZH_GTSELF_INKEYGET(g,w,d,m)             (g)->pFuncTable->InkeyGet(g,w,d,m)
#define ZH_GTSELF_INKEYPUT(g,k)                 (g)->pFuncTable->InkeyPut(g,k)
#define ZH_GTSELF_INKEYINS(g,k)                 (g)->pFuncTable->InkeyIns(g,k)
#define ZH_GTSELF_INKEYLAST(g,m)                (g)->pFuncTable->InkeyLast(g,m)
#define ZH_GTSELF_INKEYNEXT(g,m)                (g)->pFuncTable->InkeyNext(g,m)
#define ZH_GTSELF_INKEYPOLL(g)                  (g)->pFuncTable->InkeyPoll(g)
#define ZH_GTSELF_INKEYSETTEXT(g,s,l)           (g)->pFuncTable->InkeySetText(g,s,l)
#define ZH_GTSELF_INKEYSETLAST(g,k)             (g)->pFuncTable->InkeySetLast(g,k)
#define ZH_GTSELF_INKEYRESET(g)                 (g)->pFuncTable->InkeyReset(g)
#define ZH_GTSELF_INKEYEXIT(g)                  (g)->pFuncTable->InkeyExit(g)
#define ZH_GTSELF_MOUSEINIT(g)                  (g)->pFuncTable->MouseInit(g)
#define ZH_GTSELF_MOUSEEXIT(g)                  (g)->pFuncTable->MouseExit(g)
#define ZH_GTSELF_MOUSEISPRESENT(g)             (g)->pFuncTable->MouseIsPresent(g)
#define ZH_GTSELF_MOUSESHOW(g)                  (g)->pFuncTable->MouseShow(g)
#define ZH_GTSELF_MOUSEHIDE(g)                  (g)->pFuncTable->MouseHide(g)
#define ZH_GTSELF_MOUSEGETCURSOR(g)             (g)->pFuncTable->MouseGetCursor(g)
#define ZH_GTSELF_MOUSESETCURSOR(g,v)           (g)->pFuncTable->MouseSetCursor(g,v)
#define ZH_GTSELF_MOUSECOL(g)                   (g)->pFuncTable->MouseCol(g)
#define ZH_GTSELF_MOUSEROW(g)                   (g)->pFuncTable->MouseRow(g)
#define ZH_GTSELF_MOUSEGETPOS(g,pr,pc)          (g)->pFuncTable->MouseGetPos(g,pr,pc)
#define ZH_GTSELF_MOUSESETPOS(g,r,c)            (g)->pFuncTable->MouseSetPos(g,r,c)
#define ZH_GTSELF_MOUSESETBOUNDS(g,t,l,b,r)     (g)->pFuncTable->MouseSetBounds(g,t,l,b,r)
#define ZH_GTSELF_MOUSEGETBOUNDS(g,t,l,b,r)     (g)->pFuncTable->MouseGetBounds(g,t,l,b,r)
#define ZH_GTSELF_MOUSESTORAGESIZE(g)           (g)->pFuncTable->MouseStorageSize(g)
#define ZH_GTSELF_MOUSESAVESTATE(g,p)           (g)->pFuncTable->MouseSaveState(g,p)
#define ZH_GTSELF_MOUSERESTORESTATE(g,p)        (g)->pFuncTable->MouseRestoreState(g,p)
#define ZH_GTSELF_MOUSEGETDOUBLECLICKSPEED(g)   (g)->pFuncTable->MouseGetDoubleClickSpeed(g)
#define ZH_GTSELF_MOUSESETDOUBLECLICKSPEED(g,i) (g)->pFuncTable->MouseSetDoubleClickSpeed(g,i)
#define ZH_GTSELF_MOUSECOUNTBUTTON(g)           (g)->pFuncTable->MouseCountButton(g)
#define ZH_GTSELF_MOUSEBUTTONSTATE(g,b)         (g)->pFuncTable->MouseButtonState(g,b)
#define ZH_GTSELF_MOUSEBUTTONPRESSED(g,b,r,c)   (g)->pFuncTable->MouseButtonPressed(g,b,r,c)
#define ZH_GTSELF_MOUSEBUTTONRELEASED(g,b,r,c)  (g)->pFuncTable->MouseButtonReleased(g,b,r,c)
#define ZH_GTSELF_MOUSEREADKEY(g,m)             (g)->pFuncTable->MouseReadKey(g,m)
#define ZH_GTSELF_GFXPRIMITIVE(g,i,t,l,b,r,c)   (g)->pFuncTable->GfxPrimitive(g,i,t,l,b,r,c)
#define ZH_GTSELF_GFXTEXT(g,t,l,s,c,h,w)        (g)->pFuncTable->GfxText(g,t,l,s,c,h,w)
#define ZH_GTSELF_WHOCARES(g,p)                 (g)->pFuncTable->WhoCares(g,p)

#ifndef ZH_GTSUPERTABLE
#define ZH_GTSUPERTABLE(g)  ZH_GTSUPER
#endif

#define ZH_GTSUPER_LOCK(g)                       (ZH_GTSUPERTABLE(g))->Lock(g)
#define ZH_GTSUPER_UNLOCK(g)                     (ZH_GTSUPERTABLE(g))->Unlock(g)
#define ZH_GTSUPER_INIT(g,i,o,e)                 (ZH_GTSUPERTABLE(g))->Init(g,i,o,e)
#define ZH_GTSUPER_EXIT(g)                       (ZH_GTSUPERTABLE(g))->Exit(g)
#define ZH_GTSUPER_NEW(g)                        (ZH_GTSUPERTABLE(g))->New(g)
#define ZH_GTSUPER_FREE(g)                       (ZH_GTSUPERTABLE(g))->Free(g)
#define ZH_GTSUPER_MARK(g)                       (ZH_GTSUPERTABLE(g))->Mark(g)
#define ZH_GTSUPER_RESIZE(g,r,c)                 (ZH_GTSUPERTABLE(g))->Resize(g,r,c)
#define ZH_GTSUPER_SETMODE(g,r,c)                (ZH_GTSUPERTABLE(g))->SetMode(g,r,c)
#define ZH_GTSUPER_GETSIZE(g,pr,pc)              (ZH_GTSUPERTABLE(g))->GetSize(g,pr,pc)
#define ZH_GTSUPER_SEMICOLD(g)                   (ZH_GTSUPERTABLE(g))->SemiCold(g)
#define ZH_GTSUPER_COLDAREA(g,t,l,b,r)           (ZH_GTSUPERTABLE(g))->ColdArea(g,t,l,b,r)
#define ZH_GTSUPER_EXPOSEAREA(g,t,l,b,r)         (ZH_GTSUPERTABLE(g))->ExposeArea(g,t,l,b,r)
#define ZH_GTSUPER_SCROLLAREA(g,t,l,b,r,m,u,v,h) (ZH_GTSUPERTABLE(g))->ScrollArea(g,t,l,b,r,m,u,v,h)
#define ZH_GTSUPER_TOUCHLINE(g,r)                (ZH_GTSUPERTABLE(g))->TouchLine(g,r)
#define ZH_GTSUPER_TOUCHCELL(g,r,c)              (ZH_GTSUPERTABLE(g))->TouchCell(g,r,c)
#define ZH_GTSUPER_REDRAW(g,r,c,l)               (ZH_GTSUPERTABLE(g))->Redraw(g,r,c,l)
#define ZH_GTSUPER_REDRAWDIFF(g)                 (ZH_GTSUPERTABLE(g))->RedrawDiff(g)
#define ZH_GTSUPER_REFRESH(g)                    (ZH_GTSUPERTABLE(g))->Refresh(g)
#define ZH_GTSUPER_FLUSH(g)                      (ZH_GTSUPERTABLE(g))->Flush(g)
#define ZH_GTSUPER_MAXCOL(g)                     (ZH_GTSUPERTABLE(g))->MaxCol(g)
#define ZH_GTSUPER_MAXROW(g)                     (ZH_GTSUPERTABLE(g))->MaxRow(g)
#define ZH_GTSUPER_CHECKPOS(g,r,c,l)             (ZH_GTSUPERTABLE(g))->CheckPos(g,r,c,l)
#define ZH_GTSUPER_SETPOS(g,r,c)                 (ZH_GTSUPERTABLE(g))->SetPos(g,r,c)
#define ZH_GTSUPER_GETPOS(g,pr,pc)               (ZH_GTSUPERTABLE(g))->GetPos(g,pr,pc)
#define ZH_GTSUPER_ISCOLOR(g)                    (ZH_GTSUPERTABLE(g))->IsColor(g)
#define ZH_GTSUPER_GETCOLORSTR(g,s)              (ZH_GTSUPERTABLE(g))->GetColorStr(g,s)
#define ZH_GTSUPER_SETCOLORSTR(g,s)              (ZH_GTSUPERTABLE(g))->SetColorStr(g,s)
#define ZH_GTSUPER_COLORSELECT(g,c)              (ZH_GTSUPERTABLE(g))->ColorSelect(g,c)
#define ZH_GTSUPER_GETCOLOR(g)                   (ZH_GTSUPERTABLE(g))->GetColor(g)
#define ZH_GTSUPER_COLORNUM(g,s)                 (ZH_GTSUPERTABLE(g))->ColorNum(g,s)
#define ZH_GTSUPER_COLORSTOSTRING(g,pc,i,ps,n)   (ZH_GTSUPERTABLE(g))->ColorsToString(g,pc,i,ps,n)
#define ZH_GTSUPER_STRINGTOCOLORS(g,ps,pc,pi)    (ZH_GTSUPERTABLE(g))->StringToColors(g,ps,pc,pi)
#define ZH_GTSUPER_GETCOLORDATA(g,pc,pn,pi)      (ZH_GTSUPERTABLE(g))->GetColorData(g,pc,pn,pi)
#define ZH_GTSUPER_GETCLEARCOLOR(g)              (ZH_GTSUPERTABLE(g))->GetClearColor(g)
#define ZH_GTSUPER_SETCLEARCOLOR(g,c)            (ZH_GTSUPERTABLE(g))->SetClearColor(g,c)
#define ZH_GTSUPER_GETCLEARCHAR(g)               (ZH_GTSUPERTABLE(g))->GetClearChar(g)
#define ZH_GTSUPER_SETCLEARCHAR(g,c)             (ZH_GTSUPERTABLE(g))->SetClearChar(g,c)
#define ZH_GTSUPER_GETCURSORSTYLE(g)             (ZH_GTSUPERTABLE(g))->GetCursorStyle(g)
#define ZH_GTSUPER_SETCURSORSTYLE(g,s)           (ZH_GTSUPERTABLE(g))->SetCursorStyle(g,s)
#define ZH_GTSUPER_GETSCRCURSOR(g,pr,pc,ps)      (ZH_GTSUPERTABLE(g))->GetScrCursor(g,pr,pc,ps)
#define ZH_GTSUPER_GETSCRCHAR(g,r,c,pm,pa,pc)    (ZH_GTSUPERTABLE(g))->GetScrChar(g,r,c,pm,pa,pc)
#define ZH_GTSUPER_PUTSCRCHAR(g,r,c,m,a,u)       (ZH_GTSUPERTABLE(g))->PutScrChar(g,r,c,m,a,u)
#define ZH_GTSUPER_GETSCRUC(g,r,c,pm,pa,pc,f)    (ZH_GTSUPERTABLE(g))->GetScrUC(g,r,c,pm,pa,pc,f)
#define ZH_GTSUPER_DISPBEGIN(g)                  (ZH_GTSUPERTABLE(g))->DispBegin(g)
#define ZH_GTSUPER_DISPEND(g)                    (ZH_GTSUPERTABLE(g))->DispEnd(g)
#define ZH_GTSUPER_DISPCOUNT(g)                  (ZH_GTSUPERTABLE(g))->DispCount(g)
#define ZH_GTSUPER_GETCHAR(g,r,c,pm,pa,pc)       (ZH_GTSUPERTABLE(g))->GetChar(g,r,c,pm,pa,pc)
#define ZH_GTSUPER_PUTCHAR(g,r,c,m,a,u)          (ZH_GTSUPERTABLE(g))->PutChar(g,r,c,m,a,u)
#define ZH_GTSUPER_RECTSIZE(g,t,l,b,r)           (ZH_GTSUPERTABLE(g))->RectSize(g,t,l,b,r)
#define ZH_GTSUPER_SAVE(g,t,l,b,r,p)             (ZH_GTSUPERTABLE(g))->Save(g,t,l,b,r,p)
#define ZH_GTSUPER_REST(g,t,l,b,r,p)             (ZH_GTSUPERTABLE(g))->Rest(g,t,l,b,r,p)
#define ZH_GTSUPER_PUTTEXT(g,r,c,m,s,l)          (ZH_GTSUPERTABLE(g))->PutText(g,r,c,m,s,l)
#define ZH_GTSUPER_PUTTEXTW(g,r,c,m,s,l)         (ZH_GTSUPERTABLE(g))->PutTextW(g,r,c,m,s,l)
#define ZH_GTSUPER_REPLICATE(g,r,c,m,a,u,l)      (ZH_GTSUPERTABLE(g))->Replicate(g,r,c,m,a,u,l)
#define ZH_GTSUPER_WRITEAT(g,r,c,s,l)            (ZH_GTSUPERTABLE(g))->WriteAt(g,r,c,s,l)
#define ZH_GTSUPER_WRITEATW(g,r,c,s,l)           (ZH_GTSUPERTABLE(g))->WriteAtW(g,r,c,s,l)
#define ZH_GTSUPER_WRITE(g,s,l)                  (ZH_GTSUPERTABLE(g))->Write(g,s,l)
#define ZH_GTSUPER_WRITEW(g,s,l)                 (ZH_GTSUPERTABLE(g))->WriteW(g,s,l)
#define ZH_GTSUPER_WRITECON(g,s,l)               (ZH_GTSUPERTABLE(g))->WriteCon(g,s,l)
#define ZH_GTSUPER_WRITECONW(g,s,l)              (ZH_GTSUPERTABLE(g))->WriteConW(g,s,l)
#define ZH_GTSUPER_SETATTRIBUTE(g,t,l,b,r,m)     (ZH_GTSUPERTABLE(g))->SetAttribute(g,t,l,b,r,m)
#define ZH_GTSUPER_DRAWSHADOW(g,t,l,b,r,m)       (ZH_GTSUPERTABLE(g))->DrawShadow(g,t,l,b,r,m)
#define ZH_GTSUPER_SCROLL(g,t,l,b,r,m,u,v,h)     (ZH_GTSUPERTABLE(g))->Scroll(g,t,l,b,r,m,u,v,h)
#define ZH_GTSUPER_SCROLLUP(g,r,m,u)             (ZH_GTSUPERTABLE(g))->ScrollUp(g,r,m,u)
#define ZH_GTSUPER_BOX(g,t,l,b,r,f,m)            (ZH_GTSUPERTABLE(g))->Box(g,t,l,b,r,f,m)
#define ZH_GTSUPER_BOXW(g,t,l,b,r,f,m)           (ZH_GTSUPERTABLE(g))->BoxW(g,t,l,b,r,f,m)
#define ZH_GTSUPER_BOXD(g,t,l,b,r,f,m)           (ZH_GTSUPERTABLE(g))->BoxD(g,t,l,b,r,f,m)
#define ZH_GTSUPER_BOXS(g,t,l,b,r,f,m)           (ZH_GTSUPERTABLE(g))->BoxS(g,t,l,b,r,f,m)
#define ZH_GTSUPER_HORIZLINE(g,h,l,r,u,m)        (ZH_GTSUPERTABLE(g))->HorizLine(g,h,l,r,u,m)
#define ZH_GTSUPER_VERTLINE(g,c,t,b,u,m)         (ZH_GTSUPERTABLE(g))->VertLine(g,c,t,b,u,m)
#define ZH_GTSUPER_GETBLINK(g)                   (ZH_GTSUPERTABLE(g))->GetBlink(g)
#define ZH_GTSUPER_SETBLINK(g,b)                 (ZH_GTSUPERTABLE(g))->SetBlink(g,b)
#define ZH_GTSUPER_SETSNOWFLAG(g,b)              (ZH_GTSUPERTABLE(g))->SetSnowFlag(g,b)
#define ZH_GTSUPER_VERSION(g,i)                  (ZH_GTSUPERTABLE(g))->Version(g,i)
#define ZH_GTSUPER_SUSPEND(g)                    (ZH_GTSUPERTABLE(g))->Suspend(g)
#define ZH_GTSUPER_RESUME(g)                     (ZH_GTSUPERTABLE(g))->Resume(g)
#define ZH_GTSUPER_PREEXT(g)                     (ZH_GTSUPERTABLE(g))->PreExt(g)
#define ZH_GTSUPER_POSTEXT(g)                    (ZH_GTSUPERTABLE(g))->PostExt(g)
#define ZH_GTSUPER_OUTSTD(g,s,l)                 (ZH_GTSUPERTABLE(g))->OutStd(g,s,l)
#define ZH_GTSUPER_OUTERR(g,s,l)                 (ZH_GTSUPERTABLE(g))->OutErr(g,s,l)
#define ZH_GTSUPER_TONE(g,f,d)                   (ZH_GTSUPERTABLE(g))->Tone(g,f,d)
#define ZH_GTSUPER_BELL(g)                       (ZH_GTSUPERTABLE(g))->Bell(g)
#define ZH_GTSUPER_INFO(g,i,p)                   (ZH_GTSUPERTABLE(g))->Info(g,i,p)
#define ZH_GTSUPER_ALERT(g,m,o,n,h,d)            (ZH_GTSUPERTABLE(g))->Alert(g,m,o,n,h,d)
#define ZH_GTSUPER_SETFLAG(g,i,f)                (ZH_GTSUPERTABLE(g))->SetFlag(g,i,f)
#define ZH_GTSUPER_SETDISPCP(g,t,h,b)            (ZH_GTSUPERTABLE(g))->SetDispCP(g,t,h,b)
#define ZH_GTSUPER_SETKEYCP(g,t,h)               (ZH_GTSUPERTABLE(g))->SetKeyCP(g,t,h)
#define ZH_GTSUPER_READKEY(g,m)                  (ZH_GTSUPERTABLE(g))->ReadKey(g,m)
#define ZH_GTSUPER_INKEYGET(g,w,d,m)             (ZH_GTSUPERTABLE(g))->InkeyGet(g,w,d,m)
#define ZH_GTSUPER_INKEYPUT(g,k)                 (ZH_GTSUPERTABLE(g))->InkeyPut(g,k)
#define ZH_GTSUPER_INKEYINS(g,k)                 (ZH_GTSUPERTABLE(g))->InkeyIns(g,k)
#define ZH_GTSUPER_INKEYLAST(g,m)                (ZH_GTSUPERTABLE(g))->InkeyLast(g,m)
#define ZH_GTSUPER_INKEYNEXT(g,m)                (ZH_GTSUPERTABLE(g))->InkeyNext(g,m)
#define ZH_GTSUPER_INKEYPOLL(g)                  (ZH_GTSUPERTABLE(g))->InkeyPoll(g)
#define ZH_GTSUPER_INKEYSETTEXT(g,s,l)           (ZH_GTSUPERTABLE(g))->InkeySetText(g,s,l)
#define ZH_GTSUPER_INKEYSETLAST(g,k)             (ZH_GTSUPERTABLE(g))->InkeySetLast(g,k)
#define ZH_GTSUPER_INKEYRESET(g)                 (ZH_GTSUPERTABLE(g))->InkeyReset(g)
#define ZH_GTSUPER_INKEYEXIT(g)                  (ZH_GTSUPERTABLE(g))->InkeyExit(g)
#define ZH_GTSUPER_MOUSEINIT(g)                  (ZH_GTSUPERTABLE(g))->MouseInit(g)
#define ZH_GTSUPER_MOUSEEXIT(g)                  (ZH_GTSUPERTABLE(g))->MouseExit(g)
#define ZH_GTSUPER_MOUSEISPRESENT(g)             (ZH_GTSUPERTABLE(g))->MouseIsPresent(g)
#define ZH_GTSUPER_MOUSESHOW(g)                  (ZH_GTSUPERTABLE(g))->MouseShow(g)
#define ZH_GTSUPER_MOUSEHIDE(g)                  (ZH_GTSUPERTABLE(g))->MouseHide(g)
#define ZH_GTSUPER_MOUSEGETCURSOR(g)             (ZH_GTSUPERTABLE(g))->MouseGetCursor(g)
#define ZH_GTSUPER_MOUSESETCURSOR(g,v)           (ZH_GTSUPERTABLE(g))->MouseSetCursor(g,v)
#define ZH_GTSUPER_MOUSECOL(g)                   (ZH_GTSUPERTABLE(g))->MouseCol(g)
#define ZH_GTSUPER_MOUSEROW(g)                   (ZH_GTSUPERTABLE(g))->MouseRow(g)
#define ZH_GTSUPER_MOUSEGETPOS(g,pr,pc)          (ZH_GTSUPERTABLE(g))->MouseGetPos(g,pr,pc)
#define ZH_GTSUPER_MOUSESETPOS(g,r,c)            (ZH_GTSUPERTABLE(g))->MouseSetPos(g,r,c)
#define ZH_GTSUPER_MOUSESETBOUNDS(g,t,l,b,r)     (ZH_GTSUPERTABLE(g))->MouseSetBounds(g,t,l,b,r)
#define ZH_GTSUPER_MOUSEGETBOUNDS(g,t,l,b,r)     (ZH_GTSUPERTABLE(g))->MouseGetBounds(g,t,l,b,r)
#define ZH_GTSUPER_MOUSESTORAGESIZE(g)           (ZH_GTSUPERTABLE(g))->MouseStorageSize(g)
#define ZH_GTSUPER_MOUSESAVESTATE(g,p)           (ZH_GTSUPERTABLE(g))->MouseSaveState(g,p)
#define ZH_GTSUPER_MOUSERESTORESTATE(g,p)        (ZH_GTSUPERTABLE(g))->MouseRestoreState(g,p)
#define ZH_GTSUPER_MOUSEGETDOUBLECLICKSPEED(g)   (ZH_GTSUPERTABLE(g))->MouseGetDoubleClickSpeed(g)
#define ZH_GTSUPER_MOUSESETDOUBLECLICKSPEED(g,i) (ZH_GTSUPERTABLE(g))->MouseSetDoubleClickSpeed(g,i)
#define ZH_GTSUPER_MOUSECOUNTBUTTON(g)           (ZH_GTSUPERTABLE(g))->MouseCountButton(g)
#define ZH_GTSUPER_MOUSEBUTTONSTATE(g,b)         (ZH_GTSUPERTABLE(g))->MouseButtonState(g,b)
#define ZH_GTSUPER_MOUSEBUTTONPRESSED(g,b,r,c)   (ZH_GTSUPERTABLE(g))->MouseButtonPressed(g,b,r,c)
#define ZH_GTSUPER_MOUSEBUTTONRELEASED(g,b,r,c)  (ZH_GTSUPERTABLE(g))->MouseButtonReleased(g,b,r,c)
#define ZH_GTSUPER_MOUSEREADKEY(g,m)             (ZH_GTSUPERTABLE(g))->MouseReadKey(g,m)
#define ZH_GTSUPER_GFXPRIMITIVE(g,i,t,l,b,r,c)   (ZH_GTSUPERTABLE(g))->GfxPrimitive(g,i,t,l,b,r,c)
#define ZH_GTSUPER_GFXTEXT(g,t,l,s,c,h,w)        (ZH_GTSUPERTABLE(g))->GfxText(g,t,l,s,c,h,w)
#define ZH_GTSUPER_WHOCARES(g,p)                 (ZH_GTSUPERTABLE(g))->WhoCares(g,p)

extern ZH_EXPORT ZH_BOOL zh_gtRegister( const ZH_GT_INIT * gtInit );
extern ZH_EXPORT PZH_GT  zh_gtLoad( const char * szGtName, PZH_GT pGT, PZH_GT_FUNCS pSuperTable );

/* low-level GT functions common to different GTs supported by RTL */
extern int  zh_gt_chrmapinit( int * piTransTbl, const char * pszTerm, ZH_BOOL fSetACSC );
extern ZH_BOOL zh_gt_setClipboard( const char * szClipData, ZH_SIZE nLen );
extern ZH_BOOL zh_gt_getClipboard( char ** pszClipData, ZH_SIZE * pnLen );
#if defined( ZH_OS_WIN )
extern ZH_EXPORT ZH_BOOL zh_gt_winapi_setClipboard( ZH_UINT uFormat, PZH_ITEM pItem );
extern ZH_EXPORT ZH_BOOL zh_gt_winapi_setClipboardRaw( ZH_UINT uFormat, void * pData, ZH_SIZE nSize );
extern ZH_EXPORT ZH_BOOL zh_gt_winapi_getClipboard( ZH_UINT uFormat, PZH_ITEM pItem );
extern ZH_EXPORT int     zh_gt_winapi_getKbdState( void );
extern ZH_EXPORT void    zh_gt_winapi_setKbdState( int kbdShifts );
extern ZH_EXPORT void    zh_gt_winapi_tone( double dFrequency, double dDuration );
#endif /* ZH_OS_WIN */
#if defined( ZH_OS_WIN )
extern int zh_gt_dos_keyCodeTranslate( int iKey, int iFlags, PZH_CODEPAGE cdp );
#endif

ZH_EXTERN_END

#endif /* ZH_GTCORE_H_ */
