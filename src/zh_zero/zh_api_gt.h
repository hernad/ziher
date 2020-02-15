/*
 * Header file for the Terminal API
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com> (Keyboard related declarations, Cursor declarations)
 * Copyright 1999-2001 Viktor Szakats (Mouse related declarations, Undocumented GT API declarations)
 * Copyright 2005 Przemyslaw Czerpak < druzus /at/ priv.onet.pl > (Internal GT code reimplemented in different way)
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

#ifndef ZH_APIGT_H_
#define ZH_APIGT_H_

#include "zh_api.h"
#include "zh_codepage_api.h"

ZH_EXTERN_BEGIN

#include "inkey.zhh"
#include "set_cursor.zhh"
#include "gt_info.zhh"

/* maximum length of color string */
#define ZH_CLRSTR_LEN           64

/* attributes for color strings, these are the same as the ones in color.ch
   but prefixed with ZH_ to avoid collision. */
#define ZH_CLR_STANDARD         0
#define ZH_CLR_ENHANCED         1
#define ZH_CLR_BORDER           2
#define ZH_CLR_BACKGROUND       3
#define ZH_CLR_UNSELECTED       4
#define ZH_CLR_MAX_             ZH_CLR_UNSELECTED

/* extended attributes used by core screen buffer */
#define ZH_GT_ATTR_BOX        0x01
#define ZH_GT_ATTR_SHADOW     0x02
#define ZH_GT_ATTR_UNDEF      0x40
#define ZH_GT_ATTR_REFRESH    0x80

/* strings for borders (same as box.ch, but defined for use by C) */
#define _B_SINGLE              "\xDA\xC4\xBF\xB3\xD9\xC4\xC0\xB3"  /* "┌─┐│┘─└│" */
#define _B_DOUBLE              "\xC9\xCD\xBB\xBA\xBC\xCD\xC8\xBA"  /* "╔═╗║╝═╚║" */
#define _B_SINGLE_DOUBLE       "\xD6\xC4\xB7\xBA\xBD\xC4\xD3\xBA"  /* "╓─╖║╜─╙║" */
#define _B_DOUBLE_SINGLE       "\xD5\xCD\xB8\xB3\xBE\xCD\xD4\xB3"  /* "╒═╕│╛═╘│" */
#define ZH_B_SINGLE_V          '\xB3'                              /* '│' */
#define ZH_B_SINGLE_H          '\xC4'                              /* '─' */
#define ZH_B_DOUBLE_V          '\xBA'                              /* '║' */
#define ZH_B_DOUBLE_H          '\xCD'                              /* '═' */

#define ZH_B_SINGLE_W         { 0x250C, 0x2500, 0x2510, 0x2502, 0x2518, 0x2500, 0x2514, 0x2502, 0x0000 }
#define ZH_B_DOUBLE_W         { 0x2554, 0x2550, 0x2557, 0x2551, 0x255D, 0x2550, 0x255A, 0x2551, 0x0000 }
#define ZH_B_SINGLE_DOUBLE_W  { 0x2553, 0x2500, 0x2556, 0x2551, 0x255C, 0x2500, 0x2559, 0x2551, 0x0000 }
#define ZH_B_DOUBLE_SINGLE_W  { 0x2552, 0x2550, 0x2555, 0x2502, 0x255B, 0x2550, 0x2558, 0x2502, 0x0000 }
#define ZH_B_HALF_FULL_W      { 0x2588, 0x2580, 0x2588, 0x2588, 0x2588, 0x2584, 0x2588, 0x2588, 0x0000 }
#define ZH_B_HALF_W           { 0x2590, 0x2580, 0x258C, 0x258C, 0x258C, 0x2584, 0x2590, 0x2590, 0x0000 }
#define ZH_B_FULL_HALF_W      { 0x2590, 0x2588, 0x258C, 0x258C, 0x258C, 0x2588, 0x2590, 0x2590, 0x0000 }
#define ZH_B_FULL_W           { 0x2588, 0x2588, 0x2588, 0x2588, 0x2588, 0x2588, 0x2588, 0x2588, 0x0000 }



#  define ZH_DEFAULT_INKEY_BUFSIZE  50

#define ZH_STDIN_HANDLE    0
#define ZH_STDOUT_HANDLE   1
#define ZH_STDERR_HANDLE   2


/* structure used to pass/receive parameters in zh_gtInfo() */

typedef struct
{
   PZH_ITEM pNewVal;
   PZH_ITEM pResult;
   PZH_ITEM pNewVal2;
} ZH_GT_INFO, * PZH_GT_INFO;

/* Public interface. These should never change, only be added to. */

extern void zh_gtIsGtRef( void * );

extern ZH_EXPORT void   zh_gtStartupInit( void );
extern ZH_EXPORT void   zh_gtSetDefault( const char * szGtName );
extern ZH_EXPORT void * zh_gtAlloc( void * hGT );
extern ZH_EXPORT void   zh_gtRelease( void * hGT );
extern ZH_EXPORT void   zh_gtAttach( void * hGT );
extern ZH_EXPORT void * zh_gtSwap( void * hGT );
extern ZH_EXPORT ZH_BOOL   zh_gtReload( const char * szGtName,
                                     ZH_FHANDLE hFilenoStdin,
                                     ZH_FHANDLE hFilenoStdout,
                                     ZH_FHANDLE hFilenoStderr );
extern ZH_EXPORT void * zh_gtCreate( const char * szGtName,
                                     ZH_FHANDLE hFilenoStdin,
                                     ZH_FHANDLE hFilenoStdout,
                                     ZH_FHANDLE hFilenoStderr );

extern ZH_EXPORT ZH_ERRCODE zh_gtInit( ZH_FHANDLE hFilenoStdin, ZH_FHANDLE hFilenoStdout, ZH_FHANDLE hFilenoStderr );
extern ZH_EXPORT ZH_ERRCODE zh_gtExit( void );
extern ZH_EXPORT ZH_ERRCODE zh_gtLock( void );
extern ZH_EXPORT ZH_ERRCODE zh_gtUnlock( void );
extern ZH_EXPORT ZH_ERRCODE zh_gtBox( int iTop, int iLeft, int iBottom, int iRight, const char * szFrame );
extern ZH_EXPORT ZH_ERRCODE zh_gtBoxD( int iTop, int iLeft, int iBottom, int iRight );
extern ZH_EXPORT ZH_ERRCODE zh_gtBoxS( int iTop, int iLeft, int iBottom, int iRight );
extern ZH_EXPORT ZH_ERRCODE zh_gtDrawBox( int iTop, int iLeft, int iBottom, int iRight, const char * szFrame, int iColor );
extern ZH_EXPORT ZH_ERRCODE zh_gtColorSelect( int iColorIndex );
extern ZH_EXPORT int        zh_gtColorToN( const char * szColorString );
extern ZH_EXPORT ZH_ERRCODE zh_gtColorsToString( int * pColors, int iColorCount, char * pszColorString, int iBufSize );
extern ZH_EXPORT ZH_ERRCODE zh_gtDispBegin( void );
extern ZH_EXPORT int        zh_gtDispCount( void );
extern ZH_EXPORT ZH_ERRCODE zh_gtDispEnd( void );
extern ZH_EXPORT ZH_ERRCODE zh_gtDrawShadow( int iTop, int iLeft, int iBottom, int iRight, int iColor );
extern ZH_EXPORT ZH_ERRCODE zh_gtGetBlink( ZH_BOOL * pbBlink );
extern ZH_EXPORT ZH_ERRCODE zh_gtGetColorStr( char * pszColorString );
extern ZH_EXPORT ZH_ERRCODE zh_gtGetCursor( int * piCursorShape );
extern ZH_EXPORT ZH_ERRCODE zh_gtGetPos( int * piRow, int * piCol );
extern ZH_EXPORT ZH_BOOL    zh_gtIsColor( void );
extern ZH_EXPORT int        zh_gtMaxCol( void );
extern ZH_EXPORT int        zh_gtMaxRow( void );
extern ZH_EXPORT ZH_ERRCODE zh_gtPostExt( void );
extern ZH_EXPORT ZH_ERRCODE zh_gtPreExt( void );
extern ZH_EXPORT ZH_ERRCODE zh_gtSuspend( void ); /* prepare the terminal for shell output */
extern ZH_EXPORT ZH_ERRCODE zh_gtResume( void ); /* resume the terminal after the shell output */
extern ZH_EXPORT int        zh_gtReadKey( int iEventMask );
extern ZH_EXPORT ZH_ERRCODE zh_gtRectSize( int iTop, int iLeft, int iBottom, int iRight, ZH_SIZE * pnBuffSize );
extern ZH_EXPORT ZH_ERRCODE zh_gtRepChar( int iRow, int iCol, ZH_USHORT usChar, ZH_SIZE nCount );
extern ZH_EXPORT ZH_ERRCODE zh_gtSave( int iTop, int iLeft, int iBottom, int iRight, void * pScrBuff );
extern ZH_EXPORT ZH_ERRCODE zh_gtRest( int iTop, int iLeft, int iBottom, int iRight, const void * pScrBuff );
extern ZH_EXPORT ZH_ERRCODE zh_gtGetChar( int iRow, int iCol, int * piColor, ZH_BYTE * pbAttr, ZH_USHORT * pusChar );
extern ZH_EXPORT ZH_ERRCODE zh_gtPutChar( int iRow, int iCol, int iColor, ZH_BYTE bAttr, ZH_USHORT usChar );
extern ZH_EXPORT ZH_ERRCODE zh_gtBeginWrite( void );
extern ZH_EXPORT ZH_ERRCODE zh_gtEndWrite( void );
extern ZH_EXPORT ZH_ERRCODE zh_gtScrDim( int * piHeight, int * piWidth );
extern ZH_EXPORT ZH_ERRCODE zh_gtScroll( int iTop, int iLeft, int iBottom, int iRight, int iRows, int iCols );
extern ZH_EXPORT ZH_ERRCODE zh_gtScrollUp( int iRows );
extern ZH_EXPORT ZH_ERRCODE zh_gtSetAttribute( int iTop, int iLeft, int iBottom, int iRight, int iColor );
extern ZH_EXPORT ZH_ERRCODE zh_gtSetBlink( ZH_BOOL bBlink );
extern ZH_EXPORT ZH_ERRCODE zh_gtSetColorStr( const char * pszColorString );
extern ZH_EXPORT ZH_ERRCODE zh_gtSetCursor( int iCursorShape );
extern ZH_EXPORT ZH_ERRCODE zh_gtSetMode( int iRows, int iCols );
extern ZH_EXPORT ZH_ERRCODE zh_gtSetPos( int iRow, int iCol );
extern ZH_EXPORT ZH_ERRCODE zh_gtSetSnowFlag( ZH_BOOL bNoSnow );
extern ZH_EXPORT ZH_ERRCODE zh_gtTone( double dFrequency, double dDuration );
extern ZH_EXPORT ZH_ERRCODE zh_gtWrite( const char * szStr, ZH_SIZE nLen );
extern ZH_EXPORT ZH_ERRCODE zh_gtWriteAt( int iRow, int iCol, const char * szStr, ZH_SIZE nLen );
extern ZH_EXPORT ZH_ERRCODE zh_gtWriteCon( const char * szStr, ZH_SIZE nLen );
extern ZH_EXPORT ZH_ERRCODE zh_gtPutText( int iRow, int iCol, const char * szStr, ZH_SIZE nLength, int iColor );
extern ZH_EXPORT const char * zh_gtVersion( int iType );
extern ZH_EXPORT ZH_ERRCODE zh_gtOutStd( const char * szStr, ZH_SIZE nLen );
extern ZH_EXPORT ZH_ERRCODE zh_gtOutErr( const char * szStr, ZH_SIZE nLen );
extern ZH_EXPORT ZH_ERRCODE zh_gtSetDispCP( const char * pszTermCDP, const char * pszHostCDP, ZH_BOOL fBox );
extern ZH_EXPORT ZH_ERRCODE zh_gtSetKeyCP( const char * pszTermCDP, const char * pszHostCDP );
extern ZH_EXPORT PZH_CODEPAGE zh_gtHostCP( void );
extern ZH_EXPORT PZH_CODEPAGE zh_gtBoxCP( void );
extern ZH_EXPORT ZH_ERRCODE zh_gtInfo( int iType, PZH_GT_INFO pInfo );
extern ZH_EXPORT int        zh_gtAlert( PZH_ITEM pMessage, PZH_ITEM pOptions, int iClrNorm, int iClrHigh, double dDelay );
extern ZH_EXPORT int        zh_gtSetFlag( int iType, int iNewValue );
extern ZH_EXPORT int        zh_gtGetCurrColor( void );
extern ZH_EXPORT int        zh_gtGetClearColor( void );
extern ZH_EXPORT ZH_ERRCODE zh_gtSetClearColor( int iColor );
extern ZH_EXPORT ZH_USHORT  zh_gtGetClearChar( void );
extern ZH_EXPORT ZH_ERRCODE zh_gtSetClearChar( ZH_USHORT usChar );
extern ZH_EXPORT ZH_ERRCODE zh_gtGetScrChar( int iRow, int iCol, int * piColor, ZH_BYTE * pbAttr, ZH_USHORT * pusChar );
extern ZH_EXPORT ZH_ERRCODE zh_gtPutScrChar( int iRow, int iCol, int iColor, ZH_BYTE bAttr, ZH_USHORT usChar );
extern ZH_EXPORT ZH_ERRCODE zh_gtFlush( void );
extern ZH_EXPORT ZH_ERRCODE zh_gtScrollEx( int iTop, int iLeft, int iBottom, int iRight, int iColor, int iChar, int iRows, int iCols );
extern ZH_EXPORT ZH_ERRCODE zh_gtBoxEx( int iTop, int iLeft, int iBottom, int iRight, const char * szFrame, int iColor );
extern ZH_EXPORT int        zh_gtGfxPrimitive( int iType, int iTop, int iLeft, int iBottom, int iRight, int iColor );
extern ZH_EXPORT ZH_ERRCODE zh_gtGfxText( int iTop, int iLeft, const char * szText, int iColor, int iSize, int iWidth );

extern ZH_EXPORT ZH_BOOL    zh_mouseIsPresent( void );
extern ZH_EXPORT ZH_BOOL    zh_mouseGetCursor( void );
extern ZH_EXPORT void       zh_mouseSetCursor( ZH_BOOL bVisible );
extern ZH_EXPORT int        zh_mouseCol( void );
extern ZH_EXPORT int        zh_mouseRow( void );
extern ZH_EXPORT void       zh_mouseGetPos( int * piRow, int * piCol );
extern ZH_EXPORT void       zh_mouseSetPos( int iRow, int iCol );
extern ZH_EXPORT void       zh_mouseSetBounds( int iTop, int iLeft, int iBottom, int iRight );
extern ZH_EXPORT void       zh_mouseGetBounds( int * piTop, int * piLeft, int * piBottom, int * piRight );
extern ZH_EXPORT int        zh_mouseStorageSize( void );
extern ZH_EXPORT void       zh_mouseSaveState( void * pBuffer );
extern ZH_EXPORT void       zh_mouseRestoreState( const void * pBuffer );
extern ZH_EXPORT int        zh_mouseGetDoubleClickSpeed( void );
extern ZH_EXPORT void       zh_mouseSetDoubleClickSpeed( int iSpeed );
extern ZH_EXPORT int        zh_mouseCountButton( void );
extern ZH_EXPORT ZH_BOOL    zh_mouseButtonState( int iButton );
extern ZH_EXPORT ZH_BOOL    zh_mouseButtonPressed( int iButton, int * piRow, int * piCol );
extern ZH_EXPORT ZH_BOOL    zh_mouseButtonReleased( int iButton, int * piRow, int * piCol );
extern ZH_EXPORT int        zh_mouseReadKey( int iEventMask );

typedef struct
{
   int   iTop;
   int   iLeft;
   int   iBottom;
   int   iRight;
} ZH_GT_RECT;
typedef ZH_GT_RECT * PZH_GT_RECT;

typedef struct
{
   int   iRow;
   int   iCol;
} ZH_GT_CORD;
typedef ZH_GT_CORD * PZH_GT_CORD;

/* Undocumented CA-Cl*pper 5.x GT API calls */

#define ZH_GT_WND void
#define ZH_GT_RGB void
#define ZH_GT_SLR void

extern ZH_EXPORT void       zh_gtWCreate( ZH_GT_RECT * rect, ZH_GT_WND ** wnd );
extern ZH_EXPORT void       zh_gtWDestroy( ZH_GT_WND * wnd );
extern ZH_EXPORT ZH_BOOL    zh_gtWFlash( void );
extern ZH_EXPORT void       zh_gtWApp( ZH_GT_WND ** wnd );
extern ZH_EXPORT void       zh_gtWCurrent( ZH_GT_WND * wnd );
extern ZH_EXPORT void       zh_gtWPos( ZH_GT_WND * wnd, ZH_GT_RECT * rect );
extern ZH_EXPORT ZH_BOOL    zh_gtWVis( ZH_GT_WND * wnd, ZH_USHORT uiStatus );

extern ZH_EXPORT ZH_ERRCODE zh_gtSLR( ZH_GT_SLR * pSLR ); /* System-Level Request */
extern ZH_EXPORT ZH_ERRCODE zh_gtModalRead( void * );
extern ZH_EXPORT ZH_ERRCODE zh_gtFlushCursor( void );
extern ZH_EXPORT ZH_ERRCODE zh_gtSetColor( ZH_GT_RGB * color );
extern ZH_EXPORT ZH_ERRCODE zh_gtGetColor( ZH_GT_RGB * color );
extern ZH_EXPORT ZH_ERRCODE zh_gtSetBorder( ZH_GT_RGB * color );


/* Keyboard related declarations */

#define ZH_BREAK_FLAG 256

/* mouse buttons */
#define ZH_MBUTTON_LEFT         0
#define ZH_MBUTTON_RIGHT        1
#define ZH_MBUTTON_MIDDLE       2


/* Ziher keyboard support functions */
extern ZH_EXPORT int        zh_inkey( ZH_BOOL bWait, double dSeconds, int iEvenMask ); /* Wait for keyboard input */
extern ZH_EXPORT void       zh_inkeyPut( int ch );          /* Inserts an inkey code into the keyboard buffer */
extern ZH_EXPORT void       zh_inkeyIns( int ch );          /* Inserts an inkey code into the keyboard buffer */
extern ZH_EXPORT int        zh_inkeyLast( int iEvenMask );  /* Return the value of the last key that was extracted */
extern ZH_EXPORT int        zh_inkeyNext( int iEvenMask );  /* Return the next key without extracting it */
extern ZH_EXPORT void       zh_inkeyPoll( void );           /* Poll the console keyboard to stuff the Ziher buffer */
extern ZH_EXPORT void       zh_inkeyReset( void );          /* Reset the Ziher keyboard buffer */
extern ZH_EXPORT void       zh_inkeySetText( const char * szText, ZH_SIZE nLen ); /* Set text into inkey buffer */
extern ZH_EXPORT int        zh_inkeySetLast( int iKey );    /* Set new LastKey() value, return previous one */
extern ZH_EXPORT void       zh_inkeyExit( void );           /* reset inkey pool to default state and free any allocated resources */

extern ZH_EXPORT ZH_SIZE    zh_inkeyKeyString( int iKey, char * buffer, ZH_SIZE nSize ); /* convert key value to string */
extern ZH_EXPORT int        zh_inkeyKeyStd( int iKey );     /* convert Ziher extended key code to Cl*pper inkey code */
extern ZH_EXPORT int        zh_inkeyKeyExt( int iKey );     /* extract function/edit key code value ZH_KX_* from Ziher extended key code */
extern ZH_EXPORT int        zh_inkeyKeyMod( int iKey );     /* extract keyboard modifiers ZH_KF_* from Ziher extended key code */
extern ZH_EXPORT int        zh_inkeyKeyVal( int iKey );     /* extract key/character code from Ziher extended key code */

/* macros to manipulate Ziher extended key codes */
#define ZH_INKEY_EXT_MASK           0xF8000000
#define ZH_INKEY_EXT_BIT            0x40000000
#define ZH_INKEY_EXT_TYPEMASK       0xFF000000
#define ZH_INKEY_EXT_VALBITS        16
#define ZH_INKEY_EXT_VALMASK        ( ( 1 << ZH_INKEY_EXT_VALBITS ) - 1 )
#define ZH_INKEY_EXT_FLAGMASK       ( 0xFF << ZH_INKEY_EXT_VALBITS )
#define ZH_INKEY_EXT_KEY            0x01000000
#define ZH_INKEY_EXT_CHAR           0x02000000
#define ZH_INKEY_EXT_UNICODE        0x03000000
#define ZH_INKEY_EXT_MOUSEKEY       0x04000000
#define ZH_INKEY_EXT_MOUSEPOS       0x05000000
#define ZH_INKEY_EXT_EVENT          0x06000000
#define ZH_INKEY_EXT_POSBITS        12
#define ZH_INKEY_EXT_POSMASK        ( ( 1 << ZH_INKEY_EXT_POSBITS ) - 1 )

#define ZH_INKEY_ISEXT( n )         ( ( ( n ) & ZH_INKEY_EXT_MASK ) == ZH_INKEY_EXT_BIT )
#define ZH_INKEY_TYPE( n )          ( ( ( n ) ^ ZH_INKEY_EXT_BIT ) & ZH_INKEY_EXT_TYPEMASK )
#define ZH_INKEY_ISKEY( n )         ( ZH_INKEY_TYPE( n ) == ZH_INKEY_EXT_KEY )
#define ZH_INKEY_ISCHAR( n )        ( ZH_INKEY_TYPE( n ) == ZH_INKEY_EXT_CHAR )
#define ZH_INKEY_ISUNICODE( n )     ( ZH_INKEY_TYPE( n ) == ZH_INKEY_EXT_UNICODE )
#define ZH_INKEY_ISMOUSEKEY( n )    ( ZH_INKEY_TYPE( n ) == ZH_INKEY_EXT_MOUSEKEY )
#define ZH_INKEY_ISMOUSEPOS( n )    ( ZH_INKEY_TYPE( n ) == ZH_INKEY_EXT_MOUSEPOS )
#define ZH_INKEY_ISEVENT( n )       ( ZH_INKEY_TYPE( n ) == ZH_INKEY_EXT_EVENT )

#define ZH_INKEY_NEW_VALF( v, f )   ( ( ( v ) & ZH_INKEY_EXT_VALMASK ) | \
                                      ( ( ( f ) << ZH_INKEY_EXT_VALBITS ) & ZH_INKEY_EXT_FLAGMASK ) )

#define ZH_INKEY_NEW_MKEY( k, f )   ( ZH_INKEY_NEW_VALF( k, f ) | ZH_INKEY_EXT_BIT | ZH_INKEY_EXT_MOUSEKEY )
#define ZH_INKEY_NEW_KEY( k, f )    ( ZH_INKEY_NEW_VALF( k, f ) | ZH_INKEY_EXT_BIT | ZH_INKEY_EXT_KEY )
#define ZH_INKEY_NEW_CHAR( b )      ( ( b ) | ( ZH_INKEY_EXT_BIT | ZH_INKEY_EXT_CHAR ) )
#define ZH_INKEY_NEW_CHARF( b, f )  ( ZH_INKEY_NEW_VALF( b, f ) | ( ZH_INKEY_EXT_BIT | ZH_INKEY_EXT_CHAR ) )
#define ZH_INKEY_NEW_UNICODE( b )   ( ( b ) | ( ZH_INKEY_EXT_BIT | ZH_INKEY_EXT_UNICODE ) )
#define ZH_INKEY_NEW_UNICODEF( b, f ) ( ZH_INKEY_NEW_VALF( b, f ) | ( ZH_INKEY_EXT_BIT | ZH_INKEY_EXT_UNICODE ) )

#define ZH_INKEY_NEW_MPOS( x, y )   ( ( ( ( y ) & ZH_INKEY_EXT_POSMASK ) << ZH_INKEY_EXT_POSBITS ) | \
                                      ( ( x ) & ZH_INKEY_EXT_POSMASK ) | \
                                      ( ZH_INKEY_EXT_BIT | ZH_INKEY_EXT_MOUSEPOS ) )
#define ZH_INKEY_NEW_EVENT( e )     ( ( e ) | ( ZH_INKEY_EXT_BIT | ZH_INKEY_EXT_EVENT ) )

#define ZH_INKEY_MOUSEPOSX( n )     ( ( n ) & ZH_INKEY_EXT_POSMASK )
#define ZH_INKEY_MOUSEPOSY( n )     ( ( ( n ) >> ZH_INKEY_EXT_POSBITS ) & ZH_INKEY_EXT_POSMASK )

#define ZH_INKEY_VALUE( n )         ( ( n ) & ZH_INKEY_EXT_VALMASK )
#define ZH_INKEY_FLAGS( n )         ( ( ( n ) & ZH_INKEY_EXT_FLAGMASK ) >> ZH_INKEY_EXT_VALBITS )

ZH_EXTERN_END

#endif /* ZH_APIGT_H_ */
