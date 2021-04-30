/*
 * The Terminal API
 *
 * Copyright 1999 Bil Simser <bsimser@home.com>
 * Copyright 1999 Paul Tucker <ptucker@sympatico.ca>
 *    zh_gtInit(), zh_gtExit()
 *    zh_gtDispBegin(), zh_gtDispEnd()
 *    zh_gtPreExt(), zh_gtPostExt()
 *    zh_gtGetColorStr(), zh_gtSetColorStr(), zh_gtSetMode()
 * Copyright 1999-2001 Viktor Szakats
 *    zh_gtDrawShadow()
 * Copyright 2006 Przemyslaw Czerpak
 *    The body of these functions which were usable in new GT API
 *    have been moved to zh_gt_core.c to zh_gt_def_*() functions
 *    some of my modifications.
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
#include "zh_gt_core.h"
#include "zh_set.h"

/* gt API functions */

ZH_ERRCODE zh_gtInit( ZH_FHANDLE hFilenoStdin, ZH_FHANDLE hFilenoStdout, ZH_FHANDLE hFilenoStderr )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtInit()" ) );

   zh_gtStartupInit();

   pGT = zh_gt_Base();
   if( ! pGT )
      return ZH_FAILURE;

   ZH_GTSELF_INIT( pGT, hFilenoStdin, hFilenoStdout, hFilenoStderr );
   ZH_GTSELF_SETCOLORSTR( pGT, zh_setGetColor() );
   ZH_GTSELF_SETCURSORSTYLE( pGT, SC_NORMAL );
   ZH_GTSELF_FLUSH( pGT );
   zh_gt_BaseFree( pGT );

   return ZH_SUCCESS;
}

ZH_ERRCODE zh_gtExit( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtExit()" ) );

   zh_gtRelease( NULL );

   /* clear internal clipboard data */
   zh_gt_setClipboard( NULL, 0 );

   return ZH_SUCCESS;
}

ZH_ERRCODE zh_gtLock( void )
{
   ZH_ERRCODE errCode = ZH_FAILURE;
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtLock()" ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      if( ZH_GTSELF_LOCK( pGT ) )
         errCode = ZH_SUCCESS;
      zh_gt_BaseFree( pGT );
   }
   return errCode;
}

ZH_ERRCODE zh_gtUnlock( void )
{
   ZH_ERRCODE errCode = ZH_FAILURE;
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtUnlock()" ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_UNLOCK( pGT );
      errCode = ZH_SUCCESS;
      zh_gt_BaseFree( pGT );
   }
   return errCode;
}

int zh_gtReadKey( int iEventMask )
{
   int iKey = 0;
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtReadKey(%d)", iEventMask ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      iKey = ZH_GTSELF_READKEY( pGT, iEventMask );
      zh_gt_BaseFree( pGT );
   }
   return iKey;
}

ZH_ERRCODE zh_gtBox( int iTop, int iLeft, int iBottom, int iRight, const char * szFrame )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtBox(%d, %d, %d, %d, %p)", iTop, iLeft, iBottom, iRight, ( const void * ) szFrame ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_BOX( pGT, iTop, iLeft, iBottom, iRight, szFrame, ZH_GTSELF_GETCOLOR( pGT ) );
      ZH_GTSELF_SETPOS( pGT, iTop + 1, iLeft + 1 );
      ZH_GTSELF_FLUSH( pGT );
      zh_gt_BaseFree( pGT );
      return ZH_SUCCESS;
   }
   return ZH_FAILURE;
}

ZH_ERRCODE zh_gtBoxEx( int iTop, int iLeft, int iBottom, int iRight, const char * szFrame, int iColor )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtBoxEx(%d, %d, %d, %d, %p, %d)", iTop, iLeft, iBottom, iRight, ( const void * ) szFrame, iColor ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      if( iColor == -1 )
         iColor = ZH_GTSELF_GETCOLOR( pGT );
      ZH_GTSELF_BOX( pGT, iTop, iLeft, iBottom, iRight, szFrame, iColor );
      ZH_GTSELF_SETPOS( pGT, iTop + 1, iLeft + 1 );
      ZH_GTSELF_FLUSH( pGT );
      zh_gt_BaseFree( pGT );
      return ZH_SUCCESS;
   }
   return ZH_FAILURE;
}

ZH_ERRCODE zh_gtBoxD( int iTop, int iLeft, int iBottom, int iRight )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtBoxD(%d, %d, %d, %d)", iTop, iLeft, iBottom, iRight ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_BOXD( pGT, iTop, iLeft, iBottom, iRight, NULL, ZH_GTSELF_GETCOLOR( pGT ) );
      ZH_GTSELF_SETPOS( pGT, iTop + 1, iLeft + 1 );
      ZH_GTSELF_FLUSH( pGT );
      zh_gt_BaseFree( pGT );
      return ZH_SUCCESS;
   }
   return ZH_FAILURE;
}

ZH_ERRCODE zh_gtBoxS( int iTop, int iLeft, int iBottom, int iRight )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtBoxS(%d, %d, %d, %d)", iTop, iLeft, iBottom, iRight ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_BOXS( pGT, iTop, iLeft, iBottom, iRight, NULL, ZH_GTSELF_GETCOLOR( pGT ) );
      ZH_GTSELF_SETPOS( pGT, iTop + 1, iLeft + 1 );
      ZH_GTSELF_FLUSH( pGT );
      zh_gt_BaseFree( pGT );
      return ZH_SUCCESS;
   }
   return ZH_FAILURE;
}

ZH_ERRCODE zh_gtDrawBox( int iTop, int iLeft, int iBottom, int iRight, const char * szFrame, int iColor )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtDrawBox(%d, %d, %d, %d, %p, %d)", iTop, iLeft, iBottom, iRight, ( const void * ) szFrame, iColor ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      if( iColor == -1 )
         iColor = ZH_GTSELF_GETCOLOR( pGT );

      ZH_GTSELF_BOX( pGT, iTop, iLeft, iBottom, iRight, szFrame, iColor );
      ZH_GTSELF_FLUSH( pGT );
      zh_gt_BaseFree( pGT );
      return ZH_SUCCESS;
   }
   return ZH_FAILURE;
}

ZH_ERRCODE zh_gtColorSelect( int iColorIndex )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtColorSelect(%d)", iColorIndex ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_COLORSELECT( pGT, iColorIndex );
      zh_gt_BaseFree( pGT );
      return ZH_SUCCESS;
   }
   return ZH_FAILURE;
}

ZH_ERRCODE zh_gtDispBegin( void )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtDispBegin()" ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_DISPBEGIN( pGT );
      zh_gt_BaseFree( pGT );
      return ZH_SUCCESS;
   }
   return ZH_FAILURE;
}

int zh_gtDispCount( void )
{
   int    iCount = 0;
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtDispCount()" ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      iCount = ZH_GTSELF_DISPCOUNT( pGT );
      zh_gt_BaseFree( pGT );
   }
   return iCount;
}

ZH_ERRCODE zh_gtDispEnd( void )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtDispEnd()" ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_DISPEND( pGT );
      ZH_GTSELF_FLUSH( pGT );
      zh_gt_BaseFree( pGT );
      return ZH_SUCCESS;
   }
   return ZH_FAILURE;
}

ZH_ERRCODE zh_gtPreExt( void )
{
   ZH_ERRCODE errCode = ZH_FAILURE;
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtPreExt()" ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      if( ZH_GTSELF_PREEXT( pGT ) )
         errCode = ZH_SUCCESS;
      zh_gt_BaseFree( pGT );
   }
   return errCode;
}

ZH_ERRCODE zh_gtPostExt( void )
{
   ZH_ERRCODE errCode = ZH_FAILURE;
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtPostExt()" ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      if( ZH_GTSELF_POSTEXT( pGT ) )
         errCode = ZH_SUCCESS;
      zh_gt_BaseFree( pGT );
   }
   return errCode;
}

/* NOTE: szColorString must be at least ZH_CLRSTR_LEN wide by the NG. It seems
         that CA-Cl*pper SetColor() will return string lengths up to 131+EOF.
         That seems like a 127+1 buffer size, plus lazy overflow checking.
         [vszakats] */

ZH_ERRCODE zh_gtGetColorStr( char * pszColorString )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtGetColorStr(%s)", pszColorString ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_GETCOLORSTR( pGT, pszColorString );
      zh_gt_BaseFree( pGT );
      return ZH_SUCCESS;
   }
   pszColorString[ 0 ] = '\0';
   return ZH_FAILURE;
}

int zh_gtColorToN( const char * szColorString )
{
   int iColor = 0;
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtColorToN(%s)", szColorString ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      iColor = ZH_GTSELF_COLORNUM( pGT, szColorString );
      zh_gt_BaseFree( pGT );
   }
   return iColor;
}

ZH_ERRCODE zh_gtColorsToString( int * pColors, int iColorCount, char * pszColorString, int iBufSize )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtColorsToString(%p, %d, %p, %d)", ( void * ) pColors, iColorCount, ( void * ) pszColorString, iBufSize ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_COLORSTOSTRING( pGT, pColors, iColorCount, pszColorString, iBufSize );
      zh_gt_BaseFree( pGT );
      return ZH_SUCCESS;
   }
   pszColorString[ 0 ] = '\0';
   return ZH_FAILURE;
}

ZH_ERRCODE zh_gtSetColorStr( const char * szColorString )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtSetColorStr(%s)", szColorString ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_SETCOLORSTR( pGT, szColorString );
      zh_gt_BaseFree( pGT );
      return ZH_SUCCESS;
   }
   return ZH_FAILURE;
}

ZH_ERRCODE zh_gtGetCursor( int * piCursorStyle )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtGetCursor(%p)", ( void * ) piCursorStyle ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      *piCursorStyle = ZH_GTSELF_GETCURSORSTYLE( pGT );
      zh_gt_BaseFree( pGT );
      return ZH_SUCCESS;
   }
   *piCursorStyle = SC_NONE;
   return ZH_FAILURE;
}

ZH_ERRCODE zh_gtSetCursor( int iCursorStyle )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtSetCursor(%d)", iCursorStyle ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_SETCURSORSTYLE( pGT, iCursorStyle );
      ZH_GTSELF_FLUSH( pGT );
      zh_gt_BaseFree( pGT );
      return ZH_SUCCESS;
   }
   return ZH_FAILURE;
}

ZH_ERRCODE zh_gtGetPos( int * piRow, int * piCol )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtGetPos(%p, %p)", ( void * ) piRow, ( void * ) piCol ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_GETPOS( pGT, piRow, piCol );
      zh_gt_BaseFree( pGT );
      return ZH_SUCCESS;
   }
   *piRow = *piCol = 0;
   return ZH_FAILURE;
}


/* NOTE: Should be exactly the same as zh_gtSetPosContext(), but without the
         additional third parameter. */

ZH_ERRCODE zh_gtSetPos( int iRow, int iCol )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtSetPos(%d, %d)", iRow, iCol ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_SETPOS( pGT, iRow, iCol );
      ZH_GTSELF_FLUSH( pGT );
      zh_gt_BaseFree( pGT );
      return ZH_SUCCESS;
   }
   return ZH_FAILURE;
}

int zh_gtMaxCol( void )
{
   PZH_GT pGT;
   int iMaxCol;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtMaxCol()" ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      iMaxCol = ZH_GTSELF_MAXCOL( pGT );
      zh_gt_BaseFree( pGT );
   }
   else
      iMaxCol = 79;

   return iMaxCol;
}

int zh_gtMaxRow( void )
{
   PZH_GT pGT;
   int iMaxRow;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtMaxRow()" ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      iMaxRow = ZH_GTSELF_MAXROW( pGT );
      zh_gt_BaseFree( pGT );
   }
   else
      iMaxRow = 24;

   return iMaxRow;
}

ZH_ERRCODE zh_gtScrDim( int * piHeight, int * piWidth )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtScrDim(%p, %p)", ( void * ) piHeight, ( void * ) piWidth ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_GETSIZE( pGT, piHeight, piWidth );
      zh_gt_BaseFree( pGT );
      return ZH_SUCCESS;
   }
   *piHeight = *piWidth = 0;
   return ZH_FAILURE;
}

ZH_ERRCODE zh_gtSetSnowFlag( ZH_BOOL fNoSnow )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtSetSnowFlag(%d)", ( int ) fNoSnow ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_SETSNOWFLAG( pGT, fNoSnow );
      zh_gt_BaseFree( pGT );
      return ZH_SUCCESS;
   }
   return ZH_FAILURE;
}

ZH_ERRCODE zh_gtRectSize( int iTop, int iLeft, int iBottom, int iRight, ZH_SIZE * pulBuffSize )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtRectSize(%d, %d, %d, %d, %p)", iTop, iLeft, iBottom, iRight, ( void * ) pulBuffSize ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      *pulBuffSize = ZH_GTSELF_RECTSIZE( pGT, iTop, iLeft, iBottom, iRight );
      zh_gt_BaseFree( pGT );
      return ZH_SUCCESS;
   }
   *pulBuffSize = 0;
   return ZH_FAILURE;
}

ZH_BOOL zh_gtIsColor( void )
{
   ZH_BOOL fColor = ZH_TRUE;
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtIsColor()" ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      fColor = ZH_GTSELF_ISCOLOR( pGT );
      zh_gt_BaseFree( pGT );
   }
   return fColor;
}

ZH_ERRCODE zh_gtRepChar( int iRow, int iCol, ZH_USHORT usChar, ZH_SIZE nCount )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtRepChar(%d, %d, %hu, %" ZH_PFS "u)", iRow, iCol, usChar, nCount ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_REPLICATE( pGT, iRow, iCol, ZH_GTSELF_GETCOLOR( pGT ), 0,
                           usChar, nCount );
      ZH_GTSELF_FLUSH( pGT );
      zh_gt_BaseFree( pGT );
      return ZH_SUCCESS;
   }
   return ZH_FAILURE;
}

ZH_ERRCODE zh_gtSave( int iTop, int iLeft, int iBottom, int iRight, void * pScrBuff )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtSave(%d, %d, %d, %d, %p)", iTop, iLeft, iBottom, iRight, pScrBuff ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_SAVE( pGT, iTop, iLeft, iBottom, iRight, pScrBuff );
      zh_gt_BaseFree( pGT );
      return ZH_SUCCESS;
   }
   return ZH_FAILURE;
}

ZH_ERRCODE zh_gtRest( int iTop, int iLeft, int iBottom, int iRight, const void * pScrBuff )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtRest(%d, %d, %d, %d, %p)", iTop, iLeft, iBottom, iRight, pScrBuff ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_REST( pGT, iTop, iLeft, iBottom, iRight, pScrBuff );
      ZH_GTSELF_FLUSH( pGT );
      zh_gt_BaseFree( pGT );
      return ZH_SUCCESS;
   }
   return ZH_FAILURE;
}

ZH_ERRCODE zh_gtGetChar( int iRow, int iCol, int * piColor, ZH_BYTE * pbAttr, ZH_USHORT * pusChar )
{
   ZH_ERRCODE errCode = ZH_FAILURE;
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtGetChar(%d, %d, %p, %p, %p)", iRow, iCol, ( void * ) piColor, ( void * ) pbAttr, ( void * ) pusChar ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      if( ZH_GTSELF_GETCHAR( pGT, iRow, iCol, piColor, pbAttr, pusChar ) )
         errCode = ZH_SUCCESS;
      zh_gt_BaseFree( pGT );
   }
   return errCode;
}

ZH_ERRCODE zh_gtPutChar( int iRow, int iCol, int iColor, ZH_BYTE bAttr, ZH_USHORT usChar )
{
   ZH_ERRCODE errCode = ZH_FAILURE;
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtPutChar(%d, %d, %d, %u, %hu)", iRow, iCol, iColor, bAttr, usChar ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      if( ZH_GTSELF_PUTCHAR( pGT, iRow, iCol, iColor, bAttr, usChar ) )
         errCode = ZH_SUCCESS;
      zh_gt_BaseFree( pGT );
   }
   return errCode;
}

ZH_ERRCODE zh_gtBeginWrite( void )
{
   ZH_ERRCODE errCode = ZH_FAILURE;
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtBeginWrite()" ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      if( ZH_GTSELF_LOCK( pGT ) )
         errCode = ZH_SUCCESS;
      zh_gt_BaseFree( pGT );
   }

   return errCode;
}

ZH_ERRCODE zh_gtEndWrite( void )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtEndWrite()" ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_FLUSH( pGT );
      ZH_GTSELF_UNLOCK( pGT );
      zh_gt_BaseFree( pGT );
      return ZH_SUCCESS;
   }
   return ZH_FAILURE;
}

ZH_ERRCODE zh_gtGetBlink( ZH_BOOL * bpBlink )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtGetBlink(%p)", ( void * ) bpBlink ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      *bpBlink = ZH_GTSELF_GETBLINK( pGT );
      zh_gt_BaseFree( pGT );
      return ZH_SUCCESS;
   }
   *bpBlink = 0;
   return ZH_FAILURE;
}

ZH_ERRCODE zh_gtSetBlink( ZH_BOOL fBlink )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtSetBlink(%d)", ( int ) fBlink ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_SETBLINK( pGT, fBlink );
      zh_gt_BaseFree( pGT );
      return ZH_SUCCESS;
   }
   return ZH_FAILURE;
}

ZH_ERRCODE zh_gtSetMode( int iRows, int iCols )
{
   ZH_ERRCODE errCode = ZH_FAILURE;
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtSetMode(%d, %d)", iRows, iCols ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      if( ZH_GTSELF_SETMODE( pGT, iRows, iCols ) )
         errCode = ZH_SUCCESS;
      zh_gt_BaseFree( pGT );
   }
   return errCode;
}

ZH_ERRCODE zh_gtPutText( int iRow, int iCol,
                         const char * szStr, ZH_SIZE nLength, int iColor )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtPutText(%d, %d, %p, %" ZH_PFS "u, %d)", iRow, iCol, ( const void * ) szStr, nLength, iColor ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      if( iColor == -1 )
         iColor = ZH_GTSELF_GETCOLOR( pGT );

      ZH_GTSELF_PUTTEXT( pGT, iRow, iCol, iColor, szStr, nLength );
      ZH_GTSELF_FLUSH( pGT );

      zh_gt_BaseFree( pGT );
      return ZH_SUCCESS;
   }
   return ZH_FAILURE;
}

ZH_ERRCODE zh_gtWriteAt( int iRow, int iCol, const char * szStr, ZH_SIZE nLength )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtWriteAt(%d, %d, %p, %" ZH_PFS "u)", iRow, iCol, ( const void * ) szStr, nLength ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_WRITEAT( pGT, iRow, iCol, szStr, nLength );
      ZH_GTSELF_FLUSH( pGT );
      zh_gt_BaseFree( pGT );
      return ZH_SUCCESS;
   }
   return ZH_FAILURE;
}

ZH_ERRCODE zh_gtWrite( const char * szStr, ZH_SIZE nLength )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtWrite(%p, %" ZH_PFS "u)", ( const void * ) szStr, nLength ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_WRITE( pGT, szStr, nLength );
      ZH_GTSELF_FLUSH( pGT );
      zh_gt_BaseFree( pGT );
      return ZH_SUCCESS;
   }
   return ZH_FAILURE;
}

ZH_ERRCODE zh_gtWriteCon( const char * szStr, ZH_SIZE nLength )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtWriteCon(%p, %" ZH_PFS "u)", ( const void * ) szStr, nLength ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_WRITECON( pGT, szStr, nLength );
      ZH_GTSELF_FLUSH( pGT );
      zh_gt_BaseFree( pGT );
      return ZH_SUCCESS;
   }
   return ZH_FAILURE;
}

ZH_ERRCODE zh_gtScroll( int iTop, int iLeft, int iBottom, int iRight, int iRows, int iCols )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtScroll(%d, %d, %d, %d, %d, %d)", iTop, iLeft, iBottom, iRight, iRows, iCols ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_SCROLL( pGT, iTop, iLeft, iBottom, iRight,
                        ZH_GTSELF_GETCOLOR( pGT ), ' ', iRows, iCols );
      ZH_GTSELF_FLUSH( pGT );
      zh_gt_BaseFree( pGT );
      return ZH_SUCCESS;
   }
   return ZH_FAILURE;
}

ZH_ERRCODE zh_gtScrollEx( int iTop, int iLeft, int iBottom, int iRight, int iColor, int iChar, int iRows, int iCols )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtScrollEx(%d, %d, %d, %d, %d, %d, %d, %d)", iTop, iLeft, iBottom, iRight, iColor, iChar, iRows, iCols ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      if( iColor == -1 )
         iColor = ZH_GTSELF_GETCOLOR( pGT );
      if( iChar < 0 )
         iChar = ZH_GTSELF_GETCLEARCHAR( pGT );
      ZH_GTSELF_SCROLL( pGT, iTop, iLeft, iBottom, iRight,
                        iColor, ( ZH_USHORT ) iChar, iRows, iCols );
      ZH_GTSELF_FLUSH( pGT );
      zh_gt_BaseFree( pGT );
      return ZH_SUCCESS;
   }

   return ZH_FAILURE;
}

ZH_ERRCODE zh_gtScrollUp( int iRows )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtScrollUp(%d)", iRows ) );

   if( iRows != 0 )
   {
      PZH_GT pGT = zh_gt_Base();
      if( pGT )
      {
         ZH_GTSELF_SCROLLUP( pGT, iRows, ZH_GTSELF_GETCOLOR( pGT ), ' ' );
         ZH_GTSELF_FLUSH( pGT );
         zh_gt_BaseFree( pGT );
         return ZH_SUCCESS;
      }
   }
   return ZH_FAILURE;
}

ZH_ERRCODE zh_gtDrawShadow( int iTop, int iLeft, int iBottom, int iRight, int iColor )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtDrawShadow(%d, %d, %d, %d, %d)", iTop, iLeft, iBottom, iRight, iColor ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_DRAWSHADOW( pGT, iTop, iLeft, iBottom, iRight, iColor );
      ZH_GTSELF_FLUSH( pGT );
      zh_gt_BaseFree( pGT );
      return ZH_SUCCESS;
   }
   return ZH_FAILURE;
}

ZH_ERRCODE zh_gtTone( double dFrequency, double dDuration )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtTone(%lf, %lf)", dFrequency, dDuration ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_TONE( pGT, dFrequency, dDuration );
      zh_gt_BaseFree( pGT );
      return ZH_SUCCESS;
   }
   return ZH_FAILURE;
}

const char * zh_gtVersion( int iType )
{
   const char * szVersion = "";
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtVersion(%d)", iType ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      szVersion = ZH_GTSELF_VERSION( pGT, iType );
      zh_gt_BaseFree( pGT );
   }
   return szVersion;
}

ZH_ERRCODE zh_gtSetAttribute( int iTop, int iLeft, int iBottom, int iRight, int iColor )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtSetAttribute(%d, %d, %d, %d, %d)", iTop, iLeft, iBottom, iRight, iColor ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_SETATTRIBUTE( pGT, iTop, iLeft, iBottom, iRight, iColor );
      ZH_GTSELF_FLUSH( pGT );
      zh_gt_BaseFree( pGT );
      return ZH_SUCCESS;
   }
   return ZH_FAILURE;
}

/* prepare the terminal for system call */
ZH_ERRCODE zh_gtSuspend( void )
{
   ZH_ERRCODE errCode = ZH_FAILURE;
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtSuspend()" ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      if( ZH_GTSELF_SUSPEND( pGT ) )
         errCode = ZH_SUCCESS;
      zh_gt_BaseFree( pGT );
   }
   return errCode;
}

ZH_ERRCODE zh_gtResume( void )
{
   ZH_ERRCODE errCode = ZH_FAILURE;
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtResume()" ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      if( ZH_GTSELF_RESUME( pGT ) )
         errCode = ZH_SUCCESS;
      zh_gt_BaseFree( pGT );
   }
   return errCode;
}

ZH_ERRCODE zh_gtOutStd( const char * szStr, ZH_SIZE nLen )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtOutStd(%p, %" ZH_PFS "u)", ( const void * ) szStr, nLen ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_OUTSTD( pGT, szStr, nLen );
      zh_gt_BaseFree( pGT );
   }
   else
      zh_fsWriteLarge( ( ZH_FHANDLE ) ZH_STDOUT_HANDLE, szStr, nLen );

   return ZH_SUCCESS;
}

ZH_ERRCODE zh_gtOutErr( const char * szStr, ZH_SIZE nLen )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtOutErr(%p, %" ZH_PFS "u)", ( const void * ) szStr, nLen ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_OUTERR( pGT, szStr, nLen );
      zh_gt_BaseFree( pGT );
   }
   else
      zh_fsWriteLarge( ( ZH_FHANDLE ) ZH_STDERR_HANDLE, szStr, nLen );

   return ZH_SUCCESS;
}

ZH_ERRCODE zh_gtSetDisplayCodepage( const char * pszTermCDP, const char * pszHostCDP, ZH_BOOL fBox )
{
   ZH_ERRCODE errCode = ZH_FAILURE;
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtSetDisplayCodepage(%s, %s, %d)", pszTermCDP, pszHostCDP, fBox ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      if( ZH_GTSELF_SETDISPCP( pGT, pszTermCDP, pszHostCDP, fBox ) )
         errCode = ZH_SUCCESS;
      zh_gt_BaseFree( pGT );
   }
   return errCode;
}

ZH_ERRCODE zh_gtSetKeyboardCodepage( const char * pszTermCDP, const char * pszHostCDP )
{
   ZH_ERRCODE errCode = ZH_FAILURE;
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtSetKeyboardCodepage(%s, %s)", pszTermCDP, pszHostCDP ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      if( ZH_GTSELF_SETKEYCP( pGT, pszTermCDP, pszHostCDP ) )
         errCode = ZH_SUCCESS;
      zh_gt_BaseFree( pGT );
   }
   return errCode;
}

PZH_CODEPAGE zh_gtHostCodepage( void )
{
   PZH_CODEPAGE cdp = NULL;
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtHostCodepage()" ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      cdp = ZH_GTSELF_HOSTCP( pGT );
      zh_gt_BaseFree( pGT );
   }
   return cdp;
}

PZH_CODEPAGE zh_gtBoxCodepage( void )
{
   PZH_CODEPAGE cdp = NULL;
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtBoxCodepage()" ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      cdp = ZH_GTSELF_BOXCP( pGT );
      zh_gt_BaseFree( pGT );
   }
   return cdp;
}

ZH_ERRCODE zh_gtInfo( int iType, PZH_GT_INFO pInfo )
{
   ZH_ERRCODE errCode = ZH_FAILURE;
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtInfo(%d, %p)", iType, ( void * ) pInfo ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      if( ZH_GTSELF_INFO( pGT, iType, pInfo ) )
         errCode = ZH_SUCCESS;
      zh_gt_BaseFree( pGT );
   }
   return errCode;
}

int zh_gtAlert( PZH_ITEM pMessage, PZH_ITEM pOptions,
                int iClrNorm, int iClrHigh, double dDelay )
{
   int iResult = 0;
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtAlert(%p, %p, %d, %d, %f)", ( void * ) pMessage, ( void * ) pOptions, iClrNorm, iClrHigh, dDelay ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      iResult = ZH_GTSELF_ALERT( pGT, pMessage, pOptions, iClrNorm,
                                 iClrHigh, dDelay );
      zh_gt_BaseFree( pGT );
   }
   return iResult;
}

int zh_gtSetFlag( int iType, int iNewValue )
{
   int iFlag = 0;
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtSetFlag(%d, %d)", iType, iNewValue ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      iFlag = ZH_GTSELF_SETFLAG( pGT, iType, iNewValue );
      zh_gt_BaseFree( pGT );
   }
   return iFlag;
}

int zh_gtGetCurrColor( void )
{
   int iColor;
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtGetCurrColor()" ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      iColor = ZH_GTSELF_GETCOLOR( pGT );
      zh_gt_BaseFree( pGT );
   }
   else
      iColor = 0x07;

   return iColor;
}

int zh_gtGetClearColor( void )
{
   int iColor;
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtGetClearColor()" ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      iColor = ZH_GTSELF_GETCLEARCOLOR( pGT );
      zh_gt_BaseFree( pGT );
   }
   else
      iColor = 0x07;

   return iColor;
}

ZH_ERRCODE zh_gtSetClearColor( int iColor )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtSetClearColor(%d)", iColor ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_SETCLEARCOLOR( pGT, iColor );
      zh_gt_BaseFree( pGT );
      return ZH_SUCCESS;
   }
   return ZH_FAILURE;
}

ZH_USHORT zh_gtGetClearChar( void )
{
   ZH_USHORT usChar;
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtGetClearChar()" ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      usChar = ZH_GTSELF_GETCLEARCHAR( pGT );
      zh_gt_BaseFree( pGT );
   }
   else
      usChar = ( ZH_USHORT ) ' ';

   return usChar;
}

ZH_ERRCODE zh_gtSetClearChar( ZH_USHORT usChar )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtSetClearChar(%hu)", usChar ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_SETCLEARCHAR( pGT, usChar );
      zh_gt_BaseFree( pGT );
      return ZH_SUCCESS;
   }
   return ZH_FAILURE;
}

ZH_ERRCODE zh_gtGetScrChar( int iRow, int iCol, int * piColor, ZH_BYTE * pbAttr, ZH_USHORT * pusChar )
{
   ZH_ERRCODE errCode = ZH_FAILURE;
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtGetScrChar(%d, %d, %p, %p, %p)", iRow, iCol, ( void * ) piColor, ( void * ) pbAttr, ( void * ) pusChar ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      if( ZH_GTSELF_GETSCRCHAR( pGT, iRow, iCol, piColor, pbAttr, pusChar ) )
         errCode = ZH_SUCCESS;
      zh_gt_BaseFree( pGT );
   }
   return errCode;
}

ZH_ERRCODE zh_gtPutScrChar( int iRow, int iCol, int iColor, ZH_BYTE bAttr, ZH_USHORT usChar )
{
   ZH_ERRCODE errCode = ZH_FAILURE;
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtPutScrChar(%d, %d, %d, %d, %hu)", iRow, iCol, iColor, ( int ) bAttr, usChar ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      if( ZH_GTSELF_PUTSCRCHAR( pGT, iRow, iCol, iColor, bAttr, usChar ) )
         errCode = ZH_SUCCESS;
      zh_gt_BaseFree( pGT );
   }
   return errCode;
}

ZH_ERRCODE zh_gtFlush( void )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtFlush()" ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_FLUSH( pGT );
      zh_gt_BaseFree( pGT );
      return ZH_SUCCESS;
   }
   return ZH_FAILURE;
}

int zh_gtGfxPrimitive( int iType, int iTop, int iLeft, int iBottom, int iRight, int iColor )
{
   PZH_GT pGT;
   int iResult = 0;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtGfxText(%d, %d, %d, %d, %d, %d)", iType, iTop, iLeft, iBottom, iRight, iColor ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      iResult = ZH_GTSELF_GFXPRIMITIVE( pGT, iType, iTop, iLeft, iBottom, iRight, iColor );
      ZH_GTSELF_FLUSH( pGT );
      zh_gt_BaseFree( pGT );
   }
   return iResult;
}

ZH_ERRCODE zh_gtGfxText( int iTop, int iLeft, const char * cBuf, int iColor, int iSize, int iWidth )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gtGfxText(%d, %d, %s, %d, %d, %d)", iTop, iLeft, cBuf, iColor, iSize, iWidth ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_GFXTEXT( pGT, iTop, iLeft, cBuf, iColor, iSize, iWidth );
      ZH_GTSELF_FLUSH( pGT );
      zh_gt_BaseFree( pGT );
      return ZH_SUCCESS;
   }
   return ZH_FAILURE;
}
