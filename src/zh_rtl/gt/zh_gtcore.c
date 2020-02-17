/*
 * Ziher Graphic Terminal low-level code
 *
 * Copyright 2006 Przemyslaw Czerpak
 *
 * part of the code in zh_gt_def_* functions is based on the code
 * from old zh_api.c copyrighted by:
 * Copyright 1999 Bil Simser <bsimser@home.com>
 * Copyright 1999 Paul Tucker <ptucker@sympatico.ca>
 * Copyright 1999-2001 Viktor Szakats
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

#define ZH_GT_NAME  NUL

#include "zh_api.h"
#include "zh_gt_core.h"
#include "zh_item_api.h"
#include "zh_string_api.h"
#include "zh_apifs.h"
#include "zh_api_error.h"
#include "zh_codepage_api.h"
#include "zh_date.h"
#include "zh_set.h"
#include "zh_vm.h"
#include "zh_thread.h"
#include "zh_stack.h"

static const ZH_WCHAR s_szSpaceW[] = { ' ', 0 };

PZH_GT zh_gt_Base( void )
{
   PZH_GT pGT = ( PZH_GT ) zh_stackGetGT();

   if( pGT && ZH_GTSELF_LOCK( pGT ) )
      return pGT;
   else
      return NULL;
}

void zh_gt_BaseFree( PZH_GT pGT )
{
   if( pGT )
      ZH_GTSELF_UNLOCK( pGT );
}

void zh_gt_BaseUnlock( PZH_GT pGT )
{
   ZH_GTSELF_UNLOCK( pGT );
}

void zh_gt_BaseLock( PZH_GT pGT )
{
   ZH_GTSELF_LOCK( pGT );
}

void zh_gtSleep( PZH_GT pGT, double dSeconds )
{
   ZH_GTSELF_UNLOCK( pGT );
   zh_idleSleep( dSeconds );
   ZH_GTSELF_LOCK( pGT );
}

/* helper internal function */
static void zh_gt_def_BaseInit( PZH_GT_BASE pGT )
{
   pGT->fVgaCell     = ZH_TRUE;
   pGT->fIsColor     = ZH_TRUE;
   pGT->fBlinking    = ZH_TRUE;
   pGT->fStdOutCon   = ZH_FALSE;
   pGT->fStdErrCon   = ZH_FALSE;
   pGT->iCursorShape = SC_NORMAL;
   pGT->iDispCount   = 0;
   pGT->iExtCount    = 0;
   pGT->usClearChar  = ' ';
   pGT->iClearColor  = 0x07;
   pGT->iHeight      = 24;
   pGT->iWidth       = 80;
   pGT->hStdIn       = ZH_STDIN_HANDLE;
   pGT->hStdOut      = ZH_STDOUT_HANDLE;
   pGT->hStdErr      = ZH_STDERR_HANDLE;

   pGT->iDoubleClickSpeed = 168; /* In milliseconds */

   pGT->inkeyBuffer     = pGT->defaultKeyBuffer;
   pGT->inkeyBufferSize = ZH_DEFAULT_INKEY_BUFSIZE;

   pGT->cdpTerm      = NULL;
   pGT->cdpHost      = NULL;
   pGT->cdpIn        = NULL;
   pGT->cdpBox       = zh_cdpFind( "EN" );

   pGT->pMutex       = zh_threadMutexCreate();
   if( pGT->pMutex )
      zh_gcUnlock( pGT->pMutex );
}

static void * zh_gt_def_New( PZH_GT pGT )
{
   ZH_SIZE nSize, nIndex;
   ZH_USHORT usChar;
   int iColor;
   ZH_BYTE bAttr;
   int i;

   zh_gt_def_BaseInit( pGT );

   ZH_GTSELF_GETSIZE( pGT, &pGT->iHeight, &pGT->iWidth );
   nSize = ( ZH_SIZE ) pGT->iHeight * pGT->iWidth;

   pGT->screenBuffer =
            ( PZH_SCREENCELL ) zh_xgrab( sizeof( ZH_SCREENCELL ) * nSize );
   pGT->prevBuffer =
            ( PZH_SCREENCELL ) zh_xgrabz( sizeof( ZH_SCREENCELL ) * nSize );
   pGT->pLines = ( ZH_BOOL * ) zh_xgrab( sizeof( ZH_BOOL ) * pGT->iHeight );

   for( i = 0; i < pGT->iHeight; ++i )
      pGT->pLines[ i ] = ZH_TRUE;

   usChar = ZH_GTSELF_GETCLEARCHAR( pGT );
   iColor = ZH_GTSELF_GETCLEARCOLOR( pGT );
   bAttr  = 0;
   for( nIndex = 0; nIndex < nSize; ++nIndex )
   {
      pGT->screenBuffer[ nIndex ].c.usChar = usChar;
      pGT->screenBuffer[ nIndex ].c.bColor = ( ZH_BYTE ) iColor;
      pGT->screenBuffer[ nIndex ].c.bAttr = bAttr;
      pGT->prevBuffer[ nIndex ].c.bAttr = ZH_GT_ATTR_REFRESH;
   }

   return pGT;
}

static void zh_gt_def_Free( PZH_GT pGT )
{
   if( pGT == ( PZH_GT ) zh_stackGetGT() )
      zh_stackSetGT( NULL );

   if( pGT->pNotifierBlock )
   {
      zh_itemRelease( pGT->pNotifierBlock );
      pGT->pNotifierBlock = NULL;
   }
   if( pGT->pInkeyFilterBlock )
   {
      zh_itemRelease( pGT->pInkeyFilterBlock );
      pGT->pInkeyFilterBlock = NULL;
   }
   if( pGT->pInkeyReadBlock )
   {
      zh_itemRelease( pGT->pInkeyReadBlock );
      pGT->pInkeyReadBlock = NULL;
   }
   if( pGT->pCargo )
   {
      zh_itemRelease( pGT->pCargo );
      pGT->pCargo = NULL;
   }
   if( pGT->pMutex )
   {
      zh_itemRelease( pGT->pMutex );
      pGT->pMutex = NULL;
   }

   if( pGT->screenBuffer )
      zh_xfree( pGT->screenBuffer );
   if( pGT->prevBuffer )
      zh_xfree( pGT->prevBuffer );
   if( pGT->pLines )
      zh_xfree( pGT->pLines );
   if( pGT->iColorCount > 0 )
      zh_xfree( pGT->pColor );
   if( pGT->pFuncTable )
      zh_xfree( pGT->pFuncTable );

   zh_xfree( pGT );
}

static void zh_gt_def_Mark( PZH_GT pGT )
{
   if( pGT->pNotifierBlock )
      zh_gcMark( pGT->pNotifierBlock );
   if( pGT->pInkeyFilterBlock )
      zh_gcMark( pGT->pInkeyFilterBlock );
   if( pGT->pInkeyReadBlock )
      zh_gcMark( pGT->pInkeyReadBlock );
   if( pGT->pCargo )
      zh_gcMark( pGT->pCargo );
   if( pGT->pMutex )
      zh_gcMark( pGT->pMutex );
}

static ZH_BOOL zh_gt_def_Lock( PZH_GT pGT )
{
   return ! pGT->pMutex || zh_threadMutexLock( pGT->pMutex );
}

static void zh_gt_def_Unlock( PZH_GT pGT )
{
   if( pGT->pMutex )
      zh_threadMutexUnlock( pGT->pMutex );
}

static void zh_gt_def_Init( PZH_GT pGT, ZH_FHANDLE hStdIn, ZH_FHANDLE hStdOut, ZH_FHANDLE hStdErr )
{
   ZH_GTSELF_NEW( pGT );

   pGT->hStdIn  = hStdIn;
   pGT->hStdOut = hStdOut;
   pGT->hStdErr = hStdErr;

   ZH_GTSELF_RESIZE( pGT, pGT->iHeight, pGT->iWidth );
   ZH_GTSELF_MOUSEINIT( pGT );
   ZH_GTSELF_MOUSEGETPOS( pGT, &pGT->iMouseLastRow, &pGT->iMouseLastCol );
}

static void zh_gt_def_Exit( PZH_GT pGT )
{
   ZH_GTSELF_MOUSEEXIT( pGT );
   ZH_GTSELF_INKEYEXIT( pGT );

   ZH_GTSELF_FREE( pGT );
}

static ZH_BOOL zh_gt_def_CheckPos( PZH_GT pGT, int iRow, int iCol, long * plIndex )
{
   if( iRow >= 0 && iCol >= 0 )
   {
      int iHeight, iWidth;

      ZH_GTSELF_GETSIZE( pGT, &iHeight, &iWidth );
      if( iRow < iHeight && iCol < iWidth )
      {
         if( plIndex )
            *plIndex = ( long ) iRow * iWidth + iCol;
         return ZH_TRUE;
      }
   }
   return ZH_FALSE;
}

static void zh_gt_def_GetPos( PZH_GT pGT, int * piRow, int * piCol )
{
   *piRow = pGT->iRow;
   *piCol = pGT->iCol;
}

static void zh_gt_def_SetPos( PZH_GT pGT, int iRow, int iCol )
{
   pGT->iRow = iRow;
   pGT->iCol = iCol;
}

static int zh_gt_def_MaxCol( PZH_GT pGT )
{
   return pGT->iWidth - 1;
}

static int zh_gt_def_MaxRow( PZH_GT pGT )
{
   return pGT->iHeight - 1;
}

static ZH_BOOL zh_gt_def_IsColor( PZH_GT pGT )
{
   return pGT->fIsColor;
}

/* NOTE: szColorString must be at least ZH_CLRSTR_LEN wide by the NG. It seems
         that CA-Cl*pper SetColor() will return string lengths up to 131+EOF.
         That seems like a 127+1 buffer size, plus lazy overflow checking.
         [vszakats] */
static void zh_gt_def_GetColorStr( PZH_GT pGT, char * pszColorString )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_def_GetColorStr(%p,%s)", ( void * ) pGT, pszColorString ) );

   ZH_GTSELF_COLORSTOSTRING( pGT, pGT->pColor, pGT->iColorCount,
                             pszColorString, ZH_CLRSTR_LEN );
}

static void zh_gt_def_SetColorStr( PZH_GT pGT, const char * szColorString )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_def_SetColorStr(%p,%s)", ( void * ) pGT, szColorString ) );

   ZH_GTSELF_STRINGTOCOLORS( pGT, szColorString, &pGT->pColor, &pGT->iColorCount );
   pGT->iColorIndex = ZH_CLR_STANDARD; /* ZH_GTSELF_COLORSELECT( pGT, ZH_CLR_STANDARD ); */
}

static void zh_gt_def_ColorSelect( PZH_GT pGT, int iColorIndex )
{
   if( iColorIndex >= 0 && iColorIndex < pGT->iColorCount )
      pGT->iColorIndex = iColorIndex;
}

static int zh_gt_def_GetColor( PZH_GT pGT )
{
   if( pGT->iColorCount )
      return pGT->pColor[ pGT->iColorIndex ];
   else
      return ZH_GTSELF_GETCLEARCOLOR( pGT );
}

static void zh_gt_def_GetColorData( PZH_GT pGT, int ** pColorsPtr, int * piColorCount, int * piColorIndex )
{
   if( pGT->iColorCount )
   {
      *pColorsPtr = ( int * ) zh_xgrab( pGT->iColorCount * sizeof( int ) );
      memcpy( *pColorsPtr, pGT->pColor, pGT->iColorCount * sizeof( int ) );
      *piColorCount = pGT->iColorCount;
      *piColorIndex = pGT->iColorIndex;
   }
   else
   {
      *pColorsPtr = ( int * ) zh_xgrab( sizeof( int ) );
      *pColorsPtr[ 0 ] = 0;
      *piColorCount = 1;
      *piColorIndex = 0;
   }
}

static int zh_gt_def_GetClearColor( PZH_GT pGT )
{
   return pGT->iClearColor;
}

static void zh_gt_def_SetClearColor( PZH_GT pGT, int iColor )
{
   pGT->iClearColor = ( iColor & 0xFF );
}

static ZH_USHORT zh_gt_def_GetClearChar( PZH_GT pGT )
{
   return pGT->usClearChar;
}

static void zh_gt_def_SetClearChar( PZH_GT pGT, ZH_USHORT usChar )
{
   pGT->usClearChar = usChar;
}

/* helper internal function */
/* masks: 0x0007     Foreground
          0x0070     Background
          0x0008     Bright
          0x0080     Blink
          0x0800     Underline foreground
          0x8000     Underline background
 */
static const char * zh_gt_def_ColorDecode( const char * szColorString, int * piColor )
{
   char c;
   int nColor = 0, iCount = 0;
   ZH_BOOL bFore = ZH_TRUE;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_def_ColorDecode(%s,%p)", szColorString, ( void * ) piColor ) );

   while( ( c = *szColorString++ ) != 0 )
   {
      switch( c )
      {
         case '*':
            nColor |= 0x80;
            break;

         case '+':
            nColor |= 0x08;
            break;

         case '/':
            if( ! bFore )
               nColor = ( ( nColor >> 4 ) & 0x0F07 ) | ( nColor & 0x88 );
            else
               bFore = ZH_FALSE;
            break;

         case 'b':
         case 'B':
            nColor |= bFore ? 0x01 : 0x10;
            break;

         case 'g':
         case 'G':
            nColor |= bFore ? 0x02 : 0x20;
            break;

         case 'r':
         case 'R':
            nColor |= bFore ? 0x04 : 0x40;
            break;

         case 'w':
         case 'W':
            nColor |= bFore ? 0x07 : 0x70;
            break;

         case 'n':
         case 'N':
            nColor &= bFore ? 0xFFF8 : 0xFF8F;
            break;

         case 'i':
         case 'I':
            bFore = ZH_FALSE;
            nColor &= 0x88;
            nColor |= 0x70;
            break;

         case 'x':
         case 'X':
            nColor &= 0x88;
            break;

         case 'u':
         case 'U':
            if( bFore )
               nColor = ( nColor & 0xF0F8 ) | 0x0801;
            else
               nColor = ( nColor & 0x0F8F ) | 0x8010;
            break;

         case ',':
            *piColor = iCount == 0 ? -1 : nColor;
            return szColorString;

         default:
            if( c >= '0' && c <= '9' )
            {
               int iColor = c - '0';
               while( *szColorString >= '0' && *szColorString <= '9' )
                  iColor = iColor * 10 + ( *szColorString++ - '0' );
               iColor &= 0x0f;
               if( bFore )
                  nColor = ( nColor & 0xF0F8 ) | iColor;
               else
                  nColor = ( nColor & 0x0F8F ) | ( iColor << 4 );
            }
            else
               --iCount;
      }
      ++iCount;
   }

   *piColor = iCount == 0 ? -1 : nColor;
   return NULL;
}

static int zh_gt_def_ColorNum( PZH_GT pGT, const char * szColorString )
{
   int nColor;

   ZH_SYMBOL_UNUSED( pGT );
   zh_gt_def_ColorDecode( szColorString, &nColor );

   return nColor;
}

static void zh_gt_def_StringToColors( PZH_GT pGT, const char * szColorString, int ** pColorsPtr, int * piColorCount )
{
   int * pColors;
   int nColor;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_def_StringToColors(%p,%s,%p,%p)", ( void * ) pGT, szColorString, ( void * ) pColorsPtr, ( void * ) piColorCount ) );

   ZH_SYMBOL_UNUSED( pGT );

   if( *piColorCount == 0 )
   {
      *piColorCount = ZH_CLR_MAX_ + 1;
      *pColorsPtr = ( int * ) zh_xgrabz( *piColorCount * sizeof( int ) );
   }

   pColors = *pColorsPtr;

   if( ! szColorString || ! *szColorString )
   {
      pColors[ ZH_CLR_STANDARD ]   = 0x07;
      pColors[ ZH_CLR_ENHANCED ]   = 0x70;
      pColors[ ZH_CLR_BORDER ]     = 0;
      pColors[ ZH_CLR_BACKGROUND ] = 0;
      pColors[ ZH_CLR_UNSELECTED ] = 0x70;
   }
   else
   {
      int nPos = 0;

      do
      {
         szColorString = zh_gt_def_ColorDecode( szColorString, &nColor );

         if( nPos == *piColorCount )
         {
            ++*piColorCount;
            pColors = *pColorsPtr = ( int * ) zh_xrealloc( pColors, *piColorCount * sizeof( int ) );
            pColors[ nPos ] = 0;
         }
         if( nColor != -1 )
         {
            pColors[ nPos ] = nColor;
            if( nPos == ZH_CLR_ENHANCED && *piColorCount > ZH_CLR_UNSELECTED )
               pColors[ ZH_CLR_UNSELECTED ] = nColor;
         }
         ++nPos;
      }
      while( szColorString );
   }
}

static void zh_gt_def_ColorsToString( PZH_GT pGT, int * pColors, int iColorCount, char * pszColorString, int iBufSize )
{
   int iColorIndex, iPos;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_def_ColorsToString(%p,%p,%d,%p,%d)", ( void * ) pGT, ( void * ) pColors, iColorCount, ( void * ) pszColorString, iBufSize ) );

   ZH_SYMBOL_UNUSED( pGT );

   /* Go on if there's space left for the largest color string plus EOF */
   for( iColorIndex = iPos = 0; iColorIndex < iColorCount && iPos < iBufSize - 8; ++iColorIndex )
   {
      int nColor = pColors[ iColorIndex ] & 7;
      int j;

      if( iColorIndex > 0 )
         pszColorString[ iPos++ ] = ',';

      for( j = 0; j <= 1; j++ )
      {
         if( ( pColors[ iColorIndex ] & ( j ? 0x8000 : 0x0800 ) ) == 0 )
         {
            if( nColor == 7 )
               pszColorString[ iPos++ ] = 'W';
            else if( nColor == 0 )
               pszColorString[ iPos++ ] = 'N';
            else
            {
               if( ( nColor & 1 ) != 0 )
                  pszColorString[ iPos++ ] = 'B';

               if( ( nColor & 2 ) != 0 )
                  pszColorString[ iPos++ ] = 'G';

               if( ( nColor & 4 ) != 0 )
                  pszColorString[ iPos++ ] = 'R';
            }
         }
         else
            pszColorString[ iPos++ ] = 'U';

         if( j == 0 )
         {
            /* NOTE: When STRICT is on, Ziher will put both the "*" and "+"
                     chars to the first half of the colorspec (like "W*+/B"),
                     which is quite ugly, otherwise it will put the "+" to the
                     first half and the "*" to the second (like "W+/B*"), which
                     is how it should be done. [vszakats] */

            if( ( pColors[ iColorIndex ] & 0x08 ) != 0 )
               pszColorString[ iPos++ ] = '+';

            pszColorString[ iPos++ ] = '/';
         }
         else
         {
            if( ( pColors[ iColorIndex ] & 0x80 ) != 0 )
               pszColorString[ iPos++ ] = '*';
         }
         nColor = ( pColors[ iColorIndex ] >> 4 ) & 7;
      }
   }

   pszColorString[ iPos ] = '\0';
}


static int zh_gt_def_GetCursorStyle( PZH_GT pGT )
{
   return pGT->iCursorShape;
}

static void zh_gt_def_SetCursorStyle( PZH_GT pGT, int iStyle )
{
   switch( iStyle )
   {
      case SC_NONE:
      case SC_NORMAL:
      case SC_INSERT:
      case SC_SPECIAL1:
      case SC_SPECIAL2:
         pGT->iCursorShape = iStyle;
         break;
      default:
         pGT->iCursorShape = SC_NORMAL;
         break;
   }
}

static void zh_gt_def_GetScrCursor( PZH_GT pGT, int * piRow, int * piCol, int * piStyle )
{
   ZH_GTSELF_GETPOS( pGT, piRow, piCol );
   if( *piRow < 0 || *piCol < 0 ||
       *piRow > ZH_GTSELF_MAXROW( pGT ) || *piCol > ZH_GTSELF_MAXCOL( pGT ) )
      *piStyle = SC_NONE;
   else
      *piStyle = ZH_GTSELF_GETCURSORSTYLE( pGT );
}

static ZH_BOOL zh_gt_def_GetBlink( PZH_GT pGT )
{
   return pGT->fBlinking;
}

static void zh_gt_def_SetBlink( PZH_GT pGT, ZH_BOOL fBlink )
{
   pGT->fBlinking = fBlink;
}

static void zh_gt_def_SetSnowFlag( PZH_GT pGT, ZH_BOOL fNoSnow )
{
   /*
    * NOTE: This is a compatibility function which have to be implemented
    *       in low-level GT driver.
    *       If you're running on a CGA and snow is a problem speak up!
    */

   ZH_SYMBOL_UNUSED( pGT );
   ZH_SYMBOL_UNUSED( fNoSnow );
}

static void zh_gt_def_DispBegin( PZH_GT pGT )
{
   pGT->iDispCount++;
}

static void zh_gt_def_DispEnd( PZH_GT pGT )
{
   if( pGT->iDispCount > 0 )
      pGT->iDispCount--;
}

static int zh_gt_def_DispCount( PZH_GT pGT )
{
   return pGT->iDispCount;
}

static ZH_BOOL zh_gt_def_PreExt( PZH_GT pGT )
{
   if( pGT->iExtCount == 0 )
      ZH_GTSELF_REFRESH( pGT );
   pGT->iExtCount++;

   return ZH_TRUE;
}

static ZH_BOOL zh_gt_def_PostExt( PZH_GT pGT )
{
   if( pGT->iExtCount )
      pGT->iExtCount--;

   return ZH_TRUE;
}

static ZH_BOOL zh_gt_def_Suspend( PZH_GT pGT )
{
   return ZH_GTSELF_PREEXT( pGT );
}

static ZH_BOOL zh_gt_def_Resume( PZH_GT pGT )
{
   return ZH_GTSELF_POSTEXT( pGT );
}

static void zh_gt_def_OutStd( PZH_GT pGT, const char * szStr, ZH_SIZE nLen )
{
   if( nLen )
   {
      if( pGT->fStdOutCon )
         ZH_GTSELF_WRITECON( pGT, szStr, nLen );
      else
      {
         ZH_GTSELF_PREEXT( pGT );
         if( pGT->fDispTrans )
         {
            char * szStrBuff = zh_cdpnDup( szStr, &nLen,
                                           pGT->cdpHost, pGT->cdpTerm );
            zh_fsWriteLarge( pGT->hStdOut, szStrBuff, nLen );
            zh_xfree( szStrBuff );
         }
         else
            zh_fsWriteLarge( pGT->hStdOut, szStr, nLen );
         ZH_GTSELF_POSTEXT( pGT );
      }
   }
}

static void zh_gt_def_OutErr( PZH_GT pGT, const char * szStr, ZH_SIZE nLen )
{
   if( nLen )
   {
      if( pGT->fStdErrCon )
         ZH_GTSELF_WRITECON( pGT, szStr, nLen );
      else
      {
         ZH_GTSELF_PREEXT( pGT );
         if( pGT->fDispTrans )
         {
            char * szStrBuff = zh_cdpnDup( szStr, &nLen,
                                           pGT->cdpHost, pGT->cdpTerm );
            zh_fsWriteLarge( pGT->hStdErr, szStrBuff, nLen );
            zh_xfree( szStrBuff );
         }
         else
            zh_fsWriteLarge( pGT->hStdErr, szStr, nLen );
         ZH_GTSELF_POSTEXT( pGT );
      }
   }
}

static void zh_gt_def_Tone( PZH_GT pGT, double dFrequency, double dDuration )
{
   ZH_SYMBOL_UNUSED( dFrequency );

   zh_gtSleep( pGT, dDuration / 18.2 );
}

static void zh_gt_def_Bell( PZH_GT pGT )
{
   ZH_GTSELF_TONE( pGT, 700.0, 3.0 );
}

static const char * zh_gt_def_Version( PZH_GT pGT, int iType )
{
   ZH_SYMBOL_UNUSED( pGT );

   if( iType == 0 )
      return "NUL";

   return "Terminal: NULL";
}

static ZH_BOOL zh_gt_def_GetChar( PZH_GT pGT, int iRow, int iCol,
                                  int * piColor, ZH_BYTE * pbAttr, ZH_USHORT * pusChar )
{
   long lIndex;

   if( ZH_GTSELF_CHECKPOS( pGT, iRow, iCol, &lIndex ) )
   {
      *pusChar = pGT->screenBuffer[ lIndex ].c.usChar;
      *piColor = pGT->screenBuffer[ lIndex ].c.bColor;
      *pbAttr  = pGT->screenBuffer[ lIndex ].c.bAttr;
      return ZH_TRUE;
   }
   return ZH_FALSE;
}

static ZH_BOOL zh_gt_def_GetUC( PZH_GT pGT, int iRow, int iCol,
                                int * piColor, ZH_BYTE * pbAttr,
                                ZH_UCHAR * puChar, ZH_BOOL fTerm )
{
   long lIndex;

   if( ZH_GTSELF_CHECKPOS( pGT, iRow, iCol, &lIndex ) )
   {
      ZH_WCHAR wc = pGT->screenBuffer[ lIndex ].c.usChar;
      ZH_UCHAR uc = 0;

      *piColor = pGT->screenBuffer[ lIndex ].c.bColor;
      *pbAttr  = pGT->screenBuffer[ lIndex ].c.bAttr;

      if( wc )
      {
         if( fTerm && pGT->cdpTerm )
            uc = zh_cdpGetUC( pGT->cdpTerm, wc, 0 );
         if( uc == 0 )
         {
            if( pGT->cdpBox && ( ! fTerm || pGT->cdpBox != pGT->cdpTerm ) &&
                pGT->cdpBox != pGT->cdpHost && ( *pbAttr & ZH_GT_ATTR_BOX ) )
               uc = zh_cdpGetUC( pGT->cdpBox, wc, 0 );
            if( uc == 0 )
            {
               if( pGT->cdpHost && pGT->cdpTerm != pGT->cdpHost )
                  uc = zh_cdpGetUC( pGT->cdpHost, wc, 0 );
               if( uc == 0 )
                  uc = zh_cdpGetUC( zh_vmCDP(), wc, wc < 32 ? ( ZH_UCHAR ) wc : '?' );
            }
         }
      }
      *puChar = uc;
      return ZH_TRUE;
   }
   return ZH_FALSE;
}

static ZH_BOOL zh_gt_def_PutChar( PZH_GT pGT, int iRow, int iCol,
                                  int iColor, ZH_BYTE bAttr, ZH_USHORT usChar )
{
   long lIndex;

   if( ZH_GTSELF_CHECKPOS( pGT, iRow, iCol, &lIndex ) )
   {
      pGT->screenBuffer[ lIndex ].c.usChar = usChar;
      pGT->screenBuffer[ lIndex ].c.bColor = ( ZH_BYTE ) iColor;
      pGT->screenBuffer[ lIndex ].c.bAttr  = bAttr;
      pGT->pLines[ iRow ] = ZH_TRUE;
      pGT->fRefresh = ZH_TRUE;
      return ZH_TRUE;
   }
   return ZH_FALSE;
}

static int zh_gt_def_PutText( PZH_GT pGT, int iRow, int iCol, int iColor, const char * szText, ZH_SIZE nLen )
{
   PZH_CODEPAGE cdp = ZH_GTSELF_HOSTCP( pGT );
   ZH_SIZE nIndex = 0;
   ZH_WCHAR wc;

   while( ZH_CODEPAGE_CHAR_GET( cdp, szText, nLen, &nIndex, &wc ) )
   {
      if( ! ZH_GTSELF_PUTCHAR( pGT, iRow, iCol++, iColor, 0, wc ) )
      {
         while( ZH_CODEPAGE_CHAR_GET( cdp, szText, nLen, &nIndex, &wc ) )
            ++iCol;
         break;
      }
   }
   return iCol;
}

static int zh_gt_def_PutTextW( PZH_GT pGT, int iRow, int iCol, int iColor, const ZH_WCHAR * szText, ZH_SIZE nLen )
{
   if( nLen )
   {
      do
      {
         if( ! ZH_GTSELF_PUTCHAR( pGT, iRow, iCol, iColor, 0, *szText++ ) )
            break;
         ++iCol;
      }
      while( --nLen );
   }

   return iCol + ( int ) nLen;
}

static void zh_gt_def_Replicate( PZH_GT pGT, int iRow, int iCol, int iColor,
                                 ZH_BYTE bAttr, ZH_USHORT usChar, ZH_SIZE nLen )
{
   if( iCol < 0 )
   {
      if( nLen < ( ZH_SIZE ) -iCol )
         nLen = 0;
      else
         nLen += iCol;
      iCol = 0;
   }
   while( nLen-- )
   {
      if( ! ZH_GTSELF_PUTCHAR( pGT, iRow, iCol, iColor, bAttr, usChar ) )
         break;
      ++iCol;
   }
}

static void zh_gt_def_WriteAt( PZH_GT pGT, int iRow, int iCol, const char * szText, ZH_SIZE nLength )
{
   int iMaxCol;

   iCol = ZH_GTSELF_PUTTEXT( pGT, iRow, iCol, ZH_GTSELF_GETCOLOR( pGT ), szText, nLength );
   iMaxCol = ZH_GTSELF_MAXCOL( pGT );
   if( iCol > iMaxCol + 1 )
      iCol = iMaxCol + 1;
   ZH_GTSELF_SETPOS( pGT, iRow, iCol );
}

static void zh_gt_def_WriteAtW( PZH_GT pGT, int iRow, int iCol, const ZH_WCHAR * szText, ZH_SIZE nLength )
{
   int iMaxCol = ZH_GTSELF_MAXCOL( pGT );

   /* Truncate the text if the cursor will end up off the right edge */
   iCol = ZH_GTSELF_PUTTEXTW( pGT, iRow, iCol, ZH_GTSELF_GETCOLOR( pGT ), szText,
                              ZH_MIN( nLength, ( ZH_SIZE ) ( iMaxCol - iCol + 1 ) ) );

   /* Finally, save the new cursor position, even if off-screen */
   ZH_GTSELF_SETPOS( pGT, iRow, iCol );
}

static void zh_gt_def_Write( PZH_GT pGT, const char * szText, ZH_SIZE nLength )
{
   int iRow, iCol;

   ZH_GTSELF_GETPOS( pGT, &iRow, &iCol );
   ZH_GTSELF_WRITEAT( pGT, iRow, iCol, szText, nLength );
}

static void zh_gt_def_WriteW( PZH_GT pGT, const ZH_WCHAR * szText, ZH_SIZE nLength )
{
   int iRow, iCol;

   ZH_GTSELF_GETPOS( pGT, &iRow, &iCol );
   ZH_GTSELF_WRITEATW( pGT, iRow, iCol, szText, nLength );
}

#define WRITECON_BUFFER_SIZE  512

static void zh_gt_def_WriteCon( PZH_GT pGT, const char * szText, ZH_SIZE nLength )
{
   int iLen = 0;
   ZH_BOOL bDisp = ZH_FALSE;
   ZH_BOOL bBell = ZH_FALSE;
   ZH_BOOL bNewLine = ZH_FALSE;
   int iRow, iCol, iMaxRow, iMaxCol;
   ZH_WCHAR szString[ WRITECON_BUFFER_SIZE ];
   PZH_CODEPAGE cdp = ZH_GTSELF_HOSTCP( pGT );
   ZH_SIZE nIndex = 0;
   ZH_WCHAR wc;

   iMaxRow = ZH_GTSELF_MAXROW( pGT );
   iMaxCol = ZH_GTSELF_MAXCOL( pGT );

   ZH_GTSELF_GETPOS( pGT, &iRow, &iCol );

   /* Limit the starting cursor position to MaxRow(),MaxCol()
      on the high end, but don't limit it on the low end. */

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
            else if( iCol == 0 && iRow > 0 )
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
            if( iRow >= 0 )
               ++iRow;
            bDisp    = ZH_TRUE;
            bNewLine = ZH_TRUE;
            break;

         case ZH_CHAR_CR:
            iCol = 0;
            if( nIndex < nLength && szText[ nIndex ] == ZH_CHAR_LF )
            {
               if( iRow >= 0 )
                  ++iRow;
               bNewLine = ZH_TRUE;
               ++nIndex;
            }
            bDisp = ZH_TRUE;
            break;

         default:
            ++iCol;
            if( iCol > iMaxCol || iCol <= 0 )
            {
               /* If the cursor position started off the left edge,
                  don't display the first character of the string */
               if( iCol > 0 )
                  szString[ iLen++ ] = wc;
               /* Always advance to the first column of the next row
                  when the right edge is reached or when the cursor
                  started off the left edge, unless the cursor is off
                  the top edge, in which case only change the column */
               iCol = 0;
               if( iRow >= 0 )
                  ++iRow;
               bDisp    = ZH_TRUE;
               bNewLine = ZH_TRUE;
            }
            else
               szString[ iLen++ ] = wc;

            /* Special handling for a really wide screen or device */
            if( iLen >= WRITECON_BUFFER_SIZE )
               bDisp = ZH_TRUE;
      }

      if( bDisp || nIndex == nLength )
      {
         if( iLen )
            ZH_GTSELF_WRITEW( pGT, szString, iLen );

         iLen = 0;
         if( iRow > iMaxRow )
         {
            /* Normal scroll */
            ZH_GTSELF_SCROLL( pGT, 0, 0, iMaxRow, iMaxCol, ZH_GTSELF_GETCOLOR( pGT ),
                              ZH_GTSELF_GETCLEARCHAR( pGT ), iRow - iMaxRow, 0 );
            iRow = iMaxRow;
            iCol = 0;
         }
         else if( iRow < 0 && bNewLine )
         {
            /* Special case scroll when newline
               and cursor off top edge of display */
            ZH_GTSELF_SCROLL( pGT, 0, 0, iMaxRow, iMaxCol, ZH_GTSELF_GETCOLOR( pGT ),
                              ZH_GTSELF_GETCLEARCHAR( pGT ), 1, 0 );
         }
         ZH_GTSELF_SETPOS( pGT, iRow, iCol );
         bDisp = ZH_FALSE;
         bNewLine = ZH_FALSE;

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

static void zh_gt_def_WriteConW( PZH_GT pGT, const ZH_WCHAR * szText, ZH_SIZE nLength )
{
   int iLen = 0;
   ZH_BOOL bDisp = ZH_FALSE;
   ZH_BOOL bBell = ZH_FALSE;
   ZH_BOOL bNewLine = ZH_FALSE;
   int iRow, iCol, iMaxRow, iMaxCol;
   ZH_WCHAR szString[ WRITECON_BUFFER_SIZE ];
   ZH_SIZE nIndex = 0;

   iMaxRow = ZH_GTSELF_MAXROW( pGT );
   iMaxCol = ZH_GTSELF_MAXCOL( pGT );

   ZH_GTSELF_GETPOS( pGT, &iRow, &iCol );

   /* Limit the starting cursor position to MaxRow(),MaxCol()
      on the high end, but don't limit it on the low end. */

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
            else if( iCol == 0 && iRow > 0 )
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
            if( iRow >= 0 )
               ++iRow;
            bDisp    = ZH_TRUE;
            bNewLine = ZH_TRUE;
            break;

         case ZH_CHAR_CR:
            iCol = 0;
            if( nIndex < nLength && szText[ nIndex ] == ZH_CHAR_LF )
            {
               if( iRow >= 0 )
                  ++iRow;
               bNewLine = ZH_TRUE;
               ++nIndex;
            }
            bDisp = ZH_TRUE;
            break;

         default:
            ++iCol;
            if( iCol > iMaxCol || iCol <= 0 )
            {
               /* If the cursor position started off the left edge,
                  don't display the first character of the string */
               if( iCol > 0 )
                  szString[ iLen++ ] = wc;
               /* Always advance to the first column of the next row
                  when the right edge is reached or when the cursor
                  started off the left edge, unless the cursor is off
                  the top edge, in which case only change the column */
               iCol = 0;
               if( iRow >= 0 )
                  ++iRow;
               bDisp    = ZH_TRUE;
               bNewLine = ZH_TRUE;
            }
            else
               szString[ iLen++ ] = wc;

            /* Special handling for a really wide screen or device */
            if( iLen >= WRITECON_BUFFER_SIZE )
               bDisp = ZH_TRUE;
      }

      if( bDisp || nIndex == nLength )
      {
         if( iLen )
            ZH_GTSELF_WRITEW( pGT, szString, iLen );

         iLen = 0;
         if( iRow > iMaxRow )
         {
            /* Normal scroll */
            ZH_GTSELF_SCROLL( pGT, 0, 0, iMaxRow, iMaxCol, ZH_GTSELF_GETCOLOR( pGT ),
                              ZH_GTSELF_GETCLEARCHAR( pGT ), iRow - iMaxRow, 0 );
            iRow = iMaxRow;
            iCol = 0;
         }
         else if( iRow < 0 && bNewLine )
         {
            /* Special case scroll when newline
               and cursor off top edge of display */
            ZH_GTSELF_SCROLL( pGT, 0, 0, iMaxRow, iMaxCol, ZH_GTSELF_GETCOLOR( pGT ),
                              ZH_GTSELF_GETCLEARCHAR( pGT ), 1, 0 );
         }
         ZH_GTSELF_SETPOS( pGT, iRow, iCol );
         bDisp = ZH_FALSE;
         bNewLine = ZH_FALSE;

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

static long zh_gt_def_RectSize( PZH_GT pGT, int iTop, int iLeft, int iBottom, int iRight )
{
   int iRows, iCols;

   iRows = iBottom - iTop + 1;
   iCols = iRight - iLeft + 1;

   if( iCols <= 0 || iRows <= 0 )
      return 0;
   else
      return ( ( long ) iRows * iCols ) << ( pGT->fVgaCell ? 1 : 2 );
}

static void zh_gt_def_Save( PZH_GT pGT, int iTop, int iLeft, int iBottom, int iRight,
                            void * pBuffer )
{
   ZH_BYTE * pbyBuffer = ( ZH_BYTE * ) pBuffer;
   PZH_CODEPAGE cdp = pGT->fVgaCell ? ZH_GTSELF_HOSTCP( pGT ) : NULL;

   while( iTop <= iBottom )
   {
      int iCol;

      for( iCol = iLeft; iCol <= iRight; ++iCol )
      {
         int iColor;
         ZH_BYTE bAttr;
         ZH_USHORT usChar;

         if( ! ZH_GTSELF_GETCHAR( pGT, iTop, iCol, &iColor, &bAttr, &usChar ) )
         {
            usChar = ZH_GTSELF_GETCLEARCHAR( pGT );
            iColor = ZH_GTSELF_GETCLEARCOLOR( pGT );
            bAttr  = 0x00;
         }

         if( pGT->fVgaCell )
         {
            *pbyBuffer++ = zh_cdpGetChar( cdp, usChar );
            *pbyBuffer++ = ( ZH_BYTE ) iColor;
         }
         else
         {
            ZH_PUT_LE_UINT16( pbyBuffer, usChar );
            pbyBuffer += 2;
            *pbyBuffer++ = ( ZH_BYTE ) iColor;
            *pbyBuffer++ = bAttr;
         }
      }
      ++iTop;
   }
}

static void zh_gt_def_Rest( PZH_GT pGT, int iTop, int iLeft, int iBottom, int iRight,
                            const void * pBuffer )
{
   const ZH_BYTE * pbyBuffer = ( const ZH_BYTE * ) pBuffer;
   PZH_CODEPAGE cdp = pGT->fVgaCell ? ZH_GTSELF_HOSTCP( pGT ) : NULL;

   while( iTop <= iBottom )
   {
      int iCol;

      for( iCol = iLeft; iCol <= iRight; ++iCol )
      {
         int iColor;
         ZH_BYTE bAttr;
         ZH_USHORT usChar;

         if( pGT->fVgaCell )
         {
            usChar = zh_cdpGetU16( cdp, *pbyBuffer++ );
            iColor = *pbyBuffer++;
            bAttr  = 0;
         }
         else
         {
            usChar = ZH_GET_LE_UINT16( pbyBuffer );
            pbyBuffer += 2;
            iColor = *pbyBuffer++;
            bAttr  = *pbyBuffer++;
         }
         ZH_GTSELF_PUTCHAR( pGT, iTop, iCol, iColor, bAttr, usChar );
      }
      ++iTop;
   }
}

static void zh_gt_def_SetAttribute( PZH_GT pGT, int iTop, int iLeft, int iBottom, int iRight,
                                    int iColor )
{
   while( iTop <= iBottom )
   {
      int iCol;

      for( iCol = iLeft; iCol <= iRight; ++iCol )
      {
         int iColorOld;
         ZH_BYTE bAttr;
         ZH_USHORT usChar;

         if( ! ZH_GTSELF_GETCHAR( pGT, iTop, iCol, &iColorOld, &bAttr, &usChar ) )
            break;
         if( ! ZH_GTSELF_PUTCHAR( pGT, iTop, iCol, iColor, bAttr, usChar ) )
            break;
      }
      ++iTop;
   }
}

static void zh_gt_def_DrawShadow( PZH_GT pGT, int iTop, int iLeft, int iBottom, int iRight,
                                  int iColor )
{
   int iMaxRow, iMaxCol, i;

   if( iTop > iBottom )
   {
      i = iTop;
      iTop = iBottom;
      iBottom = i;
   }
   if( iLeft > iRight )
   {
      i = iLeft;
      iLeft = iRight;
      iRight = i;
   }

   iLeft += 2;
   ++iBottom;

   iMaxRow = ZH_GTSELF_MAXROW( pGT );
   iMaxCol = ZH_GTSELF_MAXCOL( pGT );

   /* Draw the bottom edge */
   if( iBottom <= iMaxRow && iLeft <= iMaxCol )
      ZH_GTSELF_SETATTRIBUTE( pGT, iBottom, iLeft, iBottom, ZH_MIN( iRight, iMaxCol ), iColor );

   ++iRight;
   ++iTop;

   /* Draw the right edge */
   if( iTop <= iMaxRow && iRight <= iMaxCol )
      ZH_GTSELF_SETATTRIBUTE( pGT, iTop, iRight, iBottom, ZH_MIN( iRight + 1, iMaxCol ), iColor );
}

static void zh_gt_def_Scroll( PZH_GT pGT, int iTop, int iLeft, int iBottom, int iRight,
                              int iColor, ZH_USHORT usChar, int iRows, int iCols )
{
   int iColOld, iColNew, iColSize, iColClear, iClrs, iLength;

   iColSize = iRight - iLeft;
   iLength = iColSize + 1;
   iColOld = iColNew = iLeft;
   if( iCols >= 0 )
   {
      iColOld += iCols;
      iColSize -= iCols;
      iColClear = iColNew + iColSize + 1;
      iClrs = iCols;
   }
   else
   {
      iColNew -= iCols;
      iColSize += iCols;
      iColClear = iColOld;
      iClrs = -iCols;
   }

   if( iLength > 0 && iTop <= iBottom )
   {
      void * pBuffer = NULL;
      int iFlag = 0;

      if( ( iRows || iCols ) && iColSize >= 0 && ( iBottom - iTop >= iRows ) )
      {
         ZH_SIZE nSize;

         iFlag = ZH_GTSELF_SETFLAG( pGT, ZH_GTI_COMPATBUFFER, 0 );
         nSize = ZH_GTSELF_RECTSIZE( pGT, iTop, iColOld, iTop, iColOld + iColSize );
         if( nSize )
            pBuffer = zh_xgrab( nSize );
      }

      while( iTop <= iBottom )
      {
         int iRowPos;

         if( iRows >= 0 )
            iRowPos = iTop++;
         else
            iRowPos = iBottom--;

         if( pBuffer && ( iRows == 0 ||
             ( iRowPos + iRows >= iTop && iRowPos + iRows <= iBottom ) ) )
         {
            ZH_GTSELF_SAVE( pGT, iRowPos + iRows, iColOld, iRowPos + iRows, iColOld + iColSize, pBuffer );
            ZH_GTSELF_REST( pGT, iRowPos, iColNew, iRowPos, iColNew + iColSize, pBuffer );
            if( iClrs )
               ZH_GTSELF_REPLICATE( pGT, iRowPos, iColClear, iColor, 0, usChar, iClrs );
         }
         else
            ZH_GTSELF_REPLICATE( pGT, iRowPos, iLeft, iColor, 0, usChar, iLength );
      }

      if( pBuffer )
         zh_xfree( pBuffer );
      if( iFlag != 0 )
         ZH_GTSELF_SETFLAG( pGT, ZH_GTI_COMPATBUFFER, iFlag );
   }
}

static void zh_gt_def_ScrollArea( PZH_GT pGT, int iTop, int iLeft, int iBottom, int iRight,
                                  int iColor, ZH_USHORT usChar, int iRows, int iCols )
{
   if( iRows || iCols )
   {
      int iColNew, iColSize, iColClear, iClrs, iLength, iHeight, iWidth;

      ZH_GTSELF_GETSIZE( pGT, &iHeight, &iWidth );
      if( iTop < 0 )
         iTop = 0;
      if( iLeft < 0 )
         iLeft = 0;
      if( iBottom >= iHeight )
         iBottom = iHeight -1;
      if( iRight >= iWidth )
         iRight = iWidth -1;

      iColSize = iRight - iLeft;
      iLength = iColSize + 1;
      iColNew = iLeft;

      if( iCols >= 0 )
      {
         iColSize -= iCols;
         iColClear = iColNew + iColSize + 1;
         iClrs = iCols;
      }
      else
      {
         iColClear = iColNew;
         iColNew -= iCols;
         iColSize += iCols;
         iClrs = -iCols;
      }

      if( iLength > 0 )
      {
         long lIndex, lOffset = ( long ) iRows * iWidth + iCols;
         ZH_BOOL fMove = ( iRows || iCols ) && iColSize >= 0 &&
                         ( iBottom - iTop >= iRows );

         while( iTop <= iBottom )
         {
            int iRowPos;

            if( iRows >= 0 )
               iRowPos = iTop++;
            else
               iRowPos = iBottom--;

            if( fMove && ( iRows == 0 ||
                ( iRowPos + iRows >= iTop && iRowPos + iRows <= iBottom ) ) )
            {
               int i;

               lIndex = ( long ) iRowPos * iWidth + iColNew;
               if( lOffset < 0 )
               {
                  for( i = 0; i <= iColSize; ++i, ++lIndex )
                  {
                     pGT->screenBuffer[ lIndex ].uiValue =
                        pGT->screenBuffer[ lIndex + lOffset ].uiValue;
                     pGT->prevBuffer[ lIndex ].uiValue =
                        pGT->prevBuffer[ lIndex + lOffset ].uiValue;
                  }
               }
               else
               {
                  for( i = iColSize, lIndex += iColSize; i >= 0; --i, --lIndex )
                  {
                     pGT->screenBuffer[ lIndex ].uiValue =
                        pGT->screenBuffer[ lIndex + lOffset ].uiValue;
                     pGT->prevBuffer[ lIndex ].uiValue =
                        pGT->prevBuffer[ lIndex + lOffset ].uiValue;
                  }
               }
               if( iClrs )
                  ZH_GTSELF_REPLICATE( pGT, iRowPos, iColClear, iColor, 0, usChar, iClrs );
            }
            else
               ZH_GTSELF_REPLICATE( pGT, iRowPos, iLeft, iColor, 0, usChar, iLength );
         }
      }
   }
}

static void zh_gt_def_ScrollUp( PZH_GT pGT, int iRows, int iColor, ZH_USHORT usChar )
{
   if( iRows > 0 )
   {
      int i, j, iHeight, iWidth;
      long lIndex = 0, lOffset;
      ZH_BYTE bAttr = 0;

      ZH_GTSELF_GETSIZE( pGT, &iHeight, &iWidth );
      lOffset = ( long ) iRows * iWidth;
      for( i = iRows; i < iHeight; ++i )
      {
         pGT->pLines[ i - iRows ] = pGT->pLines[ i ];
         for( j = 0; j < iWidth; ++j )
         {
            pGT->screenBuffer[ lIndex ].uiValue =
               pGT->screenBuffer[ lIndex + lOffset ].uiValue;
            pGT->prevBuffer[ lIndex ].uiValue =
               pGT->prevBuffer[ lIndex + lOffset ].uiValue;
            ++lIndex;
         }
      }
      for( i = ZH_MAX( 0, iHeight - iRows ); i < iHeight; ++i )
      {
         for( j = 0; j < iWidth; ++j )
         {
            pGT->screenBuffer[ lIndex ].c.usChar = usChar;
            pGT->screenBuffer[ lIndex ].c.bColor = ( ZH_BYTE ) iColor;
            pGT->screenBuffer[ lIndex ].c.bAttr  = bAttr;
            ++lIndex;
         }
         pGT->pLines[ i ] = ZH_TRUE;
      }
      pGT->fRefresh = ZH_TRUE;
   }
}

static void zh_gt_def_BoxW( PZH_GT pGT, int iTop, int iLeft, int iBottom, int iRight,
                            const ZH_WCHAR * szFrame, int iColor )
{
   int iMaxRow, iMaxCol, i;

   if( iTop > iBottom )
   {
      i = iTop;
      iTop = iBottom;
      iBottom = i;
   }
   if( iLeft > iRight )
   {
      i = iLeft;
      iLeft = iRight;
      iRight = i;
   }
   iMaxRow = ZH_GTSELF_MAXROW( pGT );
   iMaxCol = ZH_GTSELF_MAXCOL( pGT );

   if( iTop <= iMaxRow && iLeft <= iMaxCol && iBottom >= 0 && iRight >= 0 )
   {
      ZH_WCHAR szBoxW[ 10 ];
      ZH_WCHAR wcPadCh = ( ZH_WCHAR ) ZH_GTSELF_GETCLEARCHAR( pGT );

      if( szFrame && *szFrame )
      {
         for( i = 0; *szFrame && i < 9; ++i )
            wcPadCh = szBoxW[ i ] = *szFrame++;
         while( i < 8 )
            szBoxW[ i++ ] = wcPadCh;
      }
      else
      {
         for( i = 0; i < 9; ++i )
            szBoxW[ i ] = ' ';
      }

      szBoxW[ i ] = '\0';

      if( iTop == iBottom )
         ZH_GTSELF_HORIZLINE( pGT, iTop, iLeft, iRight, szBoxW[ 1 ], iColor );
      else if( iLeft == iRight )
         ZH_GTSELF_VERTLINE( pGT, iLeft, iTop, iBottom, szBoxW[ 3 ], iColor );
      else
      {
         ZH_BYTE bAttr = ZH_GT_ATTR_BOX;

         int iRows  = ( iBottom > iMaxRow ? iMaxRow + 1 : iBottom ) -
                      ( iTop < 0 ? -1 : iTop ) - 1;
         int iCols  = ( iRight > iMaxCol ? iMaxCol + 1 : iRight ) -
                      ( iLeft < 0 ? -1 : iLeft ) - 1;
         int iFirst = iLeft < 0 ? 0 : iLeft + 1;

         if( iTop >= 0 )
         {
            if( iLeft >= 0 )
               ZH_GTSELF_PUTCHAR( pGT, iTop, iLeft, iColor, bAttr, szBoxW[ 0 ] );
            if( iCols )
               ZH_GTSELF_REPLICATE( pGT, iTop, iFirst, iColor, bAttr, szBoxW[ 1 ], iCols );
            if( iRight <= iMaxCol )
               ZH_GTSELF_PUTCHAR( pGT, iTop, iFirst + iCols, iColor, bAttr, szBoxW[ 2 ] );
            iTop++;
         }
         else
            iTop = 0;
         for( i = 0; i < iRows; ++i )
         {
            if( iLeft >= 0 )
               ZH_GTSELF_PUTCHAR( pGT, iTop + i, iLeft, iColor, bAttr, szBoxW[ 7 ] );
            if( iCols && szBoxW[ 8 ] )
               ZH_GTSELF_REPLICATE( pGT, iTop + i, iFirst, iColor, bAttr, szBoxW[ 8 ], iCols );
            if( iRight <= iMaxCol )
               ZH_GTSELF_PUTCHAR( pGT, iTop + i, iFirst + iCols, iColor, bAttr, szBoxW[ 3 ] );
         }
         if( iBottom <= iMaxRow )
         {
            if( iLeft >= 0 )
               ZH_GTSELF_PUTCHAR( pGT, iBottom, iLeft, iColor, bAttr, szBoxW[ 6 ] );
            if( iCols )
               ZH_GTSELF_REPLICATE( pGT, iBottom, iFirst, iColor, bAttr, szBoxW[ 5 ], iCols );
            if( iRight <= iMaxCol )
               ZH_GTSELF_PUTCHAR( pGT, iBottom, iFirst + iCols, iColor, bAttr, szBoxW[ 4 ] );
         }
      }
   }
}

static void zh_gt_def_Box( PZH_GT pGT, int iTop, int iLeft, int iBottom, int iRight,
                           const char * szFrame, int iColor )
{
   if( szFrame )
   {
      PZH_CODEPAGE cdp = ZH_GTSELF_BOXCP( pGT );
      ZH_WCHAR szFrameW[ 10 ], wc;
      ZH_SIZE nLen = strlen( szFrame ), nIndex = 0, nPos = 0;

      while( nPos < 9 && ZH_CODEPAGE_CHAR_GET( cdp, szFrame, nLen, &nIndex, &wc ) )
         szFrameW[ nPos++ ] = wc;

      szFrameW[ nPos ] = 0;

      ZH_GTSELF_BOXW( pGT, iTop, iLeft, iBottom, iRight, szFrameW, iColor );
   }
   else
      ZH_GTSELF_BOXW( pGT, iTop, iLeft, iBottom, iRight, NULL, iColor );
}

static void zh_gt_def_BoxS( PZH_GT pGT, int iTop, int iLeft, int iBottom, int iRight,
                            const char * szFrame, int iColor )
{
   static const ZH_WCHAR s_szFrameW[] = ZH_B_SINGLE_W;

   if( szFrame )
      ZH_GTSELF_BOX( pGT, iTop, iLeft, iBottom, iRight, szFrame, iColor );
   else
      ZH_GTSELF_BOXW( pGT, iTop, iLeft, iBottom, iRight, s_szFrameW, iColor );
}

static void zh_gt_def_BoxD( PZH_GT pGT, int iTop, int iLeft, int iBottom, int iRight,
                            const char * szFrame, int iColor )
{
   static const ZH_WCHAR s_szFrameW[] = ZH_B_DOUBLE_W;

   if( szFrame )
      ZH_GTSELF_BOX( pGT, iTop, iLeft, iBottom, iRight, szFrame, iColor );
   else
      ZH_GTSELF_BOXW( pGT, iTop, iLeft, iBottom, iRight, s_szFrameW, iColor );
}

static void zh_gt_def_HorizLine( PZH_GT pGT, int iRow, int iLeft, int iRight,
                                 ZH_USHORT usChar, int iColor )
{
   int iLength, iCol;

   if( iLeft <= iRight )
   {
      iLength = iRight - iLeft + 1;
      iCol = iLeft;
   }
   else
   {
      iLength = iLeft - iRight + 1;
      iCol = iRight;
   }

   ZH_GTSELF_REPLICATE( pGT, iRow, iCol, iColor, ZH_GT_ATTR_BOX, usChar, iLength );
}

static void zh_gt_def_VertLine( PZH_GT pGT, int iCol, int iTop, int iBottom,
                                ZH_USHORT usChar, int iColor )
{
   int iLength, iRow;

   if( iTop <= iBottom )
   {
      iLength = iBottom - iTop + 1;
      iRow = iTop;
   }
   else
   {
      iLength = iTop - iBottom + 1;
      iRow = iBottom;
   }

   if( iRow < 0 )
   {
      iLength += iRow;
      iRow = 0;
   }

   while( --iLength >= 0 )
   {
      if( ! ZH_GTSELF_PUTCHAR( pGT, iRow, iCol, iColor, ZH_GT_ATTR_BOX, usChar ) )
         break;
      ++iRow;
   }
}

static ZH_BOOL zh_gt_def_SetDispCP( PZH_GT pGT, const char * pszTermCDP, const char * pszHostCDP, ZH_BOOL fBox )
{
   if( ! pszHostCDP )
      pszHostCDP = zh_cdpID();
   if( ! pszTermCDP )
      pszTermCDP = pszHostCDP;

   if( pszTermCDP && pszHostCDP )
   {
      pGT->cdpTerm = zh_cdpFindExt( pszTermCDP );
      pGT->cdpHost = zh_cdpFindExt( pszHostCDP );
      pGT->cdpBox  = fBox ? pGT->cdpHost : zh_cdpFind( "EN" );
      pGT->fDispTrans = pGT->cdpTerm && pGT->cdpHost &&
                        pGT->cdpTerm != pGT->cdpHost;
      return ZH_TRUE;
   }

   return ZH_FALSE;
}

static ZH_BOOL zh_gt_def_SetKeyCP( PZH_GT pGT, const char * pszTermCDP, const char * pszHostCDP )
{
   if( ! pszHostCDP )
      pszHostCDP = zh_cdpID();
   if( ! pszTermCDP )
      pszTermCDP = pszHostCDP;

   if( pszTermCDP )
   {
      pGT->cdpIn = zh_cdpFindExt( pszTermCDP );
      return ZH_TRUE;
   }

   return ZH_FALSE;
}

static void zh_gt_def_SetBlock( PZH_ITEM * pItemPtr, PZH_GT_INFO pInfo )
{
   if( *pItemPtr )
   {
      if( pInfo->pResult )
         zh_itemCopy( pInfo->pResult, *pItemPtr );
      else
         pInfo->pResult = zh_itemNew( *pItemPtr );
   }
   if( pInfo->pNewVal )
   {
      if( *pItemPtr )
      {
         zh_itemRelease( *pItemPtr );
         *pItemPtr = NULL;
      }
      if( ZH_IS_EVALITEM( pInfo->pNewVal ) )
      {
         *pItemPtr = zh_itemNew( pInfo->pNewVal );
         zh_gcUnlock( *pItemPtr );
      }
   }
}

static ZH_BOOL zh_gt_def_Info( PZH_GT pGT, int iType, PZH_GT_INFO pInfo )
{
   switch( iType )
   {
      case ZH_GTI_ALTENTER:
      case ZH_GTI_ISFULLSCREEN:
      case ZH_GTI_ISGRAPHIC:
      case ZH_GTI_ISSCREENPOS:
      case ZH_GTI_KBDSUPPORT:
      case ZH_GTI_ISCTWIN:
      case ZH_GTI_ISMULTIWIN:
      case ZH_GTI_ISUNICODE:
         pInfo->pResult = zh_itemPutL( pInfo->pResult, ZH_FALSE );
         break;

      case ZH_GTI_KBDSHIFTS:
         pInfo->pResult = zh_itemPutNI( pInfo->pResult, 0 );
         break;

      case ZH_GTI_INPUTFD:
         pInfo->pResult = zh_itemPutNInt( pInfo->pResult, ( ZH_NHANDLE ) pGT->hStdIn );
         break;

      case ZH_GTI_OUTPUTFD:
         pInfo->pResult = zh_itemPutNInt( pInfo->pResult, ( ZH_NHANDLE ) pGT->hStdOut );
         break;

      case ZH_GTI_ERRORFD:
         pInfo->pResult = zh_itemPutNInt( pInfo->pResult, ( ZH_NHANDLE ) pGT->hStdErr );
         break;

      case ZH_GTI_COMPATBUFFER:
         pInfo->pResult = zh_itemPutL( pInfo->pResult, pGT->fVgaCell );
         if( zh_itemType( pInfo->pNewVal ) & ZH_IT_LOGICAL )
            pGT->fVgaCell = zh_itemGetL( pInfo->pNewVal );
         break;

      case ZH_GTI_REDRAWMAX:
         pInfo->pResult = zh_itemPutNI( pInfo->pResult, pGT->iRedrawMax );
         if( zh_itemType( pInfo->pNewVal ) & ZH_IT_NUMERIC )
            pGT->iRedrawMax = zh_itemGetNI( pInfo->pNewVal );
         break;

      case ZH_GTI_BOXCP:
         pInfo->pResult = zh_itemPutC( pInfo->pResult,
                                       pGT->cdpBox ? pGT->cdpBox->id : NULL );
         if( zh_itemType( pInfo->pNewVal ) & ZH_IT_STRING )
         {
            if( zh_itemGetCLen( pInfo->pNewVal ) > 0 )
            {
               PZH_CODEPAGE cdpBox = zh_cdpFind( zh_itemGetCPtr( pInfo->pNewVal ) );
               if( cdpBox )
                  pGT->cdpBox = cdpBox;
            }
            else
               pGT->cdpBox = NULL;
         }
         break;

      case ZH_GTI_VIEWMAXWIDTH:
         pInfo->pResult = zh_itemPutNInt( pInfo->pResult, ZH_GTSELF_MAXCOL( pGT ) );
         break;

      case ZH_GTI_VIEWMAXHEIGHT:
         pInfo->pResult = zh_itemPutNInt( pInfo->pResult, ZH_GTSELF_MAXROW( pGT ) );
         break;

      case ZH_GTI_NEWWIN:  /* clear screen area, set default cursor shape and position */

         /* Clear screen */
         ZH_GTSELF_DISPBEGIN( pGT );
         ZH_GTSELF_SCROLL( pGT, 0, 0, ZH_GTSELF_MAXROW( pGT ), ZH_GTSELF_MAXCOL( pGT ),
                           ZH_GTSELF_GETCOLOR( pGT ), ZH_GTSELF_GETCLEARCHAR( pGT ), 0, 0 );
         ZH_GTSELF_SETPOS( pGT, 0, 0 );
         ZH_GTSELF_SETCURSORSTYLE( pGT, SC_NORMAL );
         ZH_GTSELF_DISPEND( pGT );
         ZH_GTSELF_FLUSH( pGT );
         /* fallthrough */

      case ZH_GTI_GETWIN:  /* save screen buffer, cursor shape and position */
      {
         int iRow, iCol, iFlag;
         ZH_SIZE nSize;

         if( ! pInfo->pResult )
            pInfo->pResult = zh_itemNew( NULL );
         zh_arrayNew( pInfo->pResult, 7 );
         /* 7th item is allocated for GTCTW window number */
         ZH_GTSELF_GETPOS( pGT, &iRow, &iCol );
         zh_arraySetNI( pInfo->pResult, 1, iRow );
         zh_arraySetNI( pInfo->pResult, 2, iCol );
         zh_arraySetNI( pInfo->pResult, 3, ZH_GTSELF_GETCURSORSTYLE( pGT ) );

         iRow = ZH_GTSELF_MAXROW( pGT );
         iCol = ZH_GTSELF_MAXCOL( pGT );
         zh_arraySetNI( pInfo->pResult, 4, iRow );
         zh_arraySetNI( pInfo->pResult, 5, iCol );

         iFlag = ZH_GTSELF_SETFLAG( pGT, ZH_GTI_COMPATBUFFER, 0 );
         nSize = ZH_GTSELF_RECTSIZE( pGT, 0, 0, iRow, iCol );
         if( nSize )
         {
            void * pBuffer = zh_xgrab( nSize + 1 );
            ZH_GTSELF_SAVE( pGT, 0, 0, iRow, iCol, pBuffer );
            zh_arraySetCLPtr( pInfo->pResult, 6, ( char * ) pBuffer, nSize );
         }
         if( iFlag != 0 )
            ZH_GTSELF_SETFLAG( pGT, ZH_GTI_COMPATBUFFER, iFlag );
         break;
      }
      case ZH_GTI_SETWIN:  /* restore screen buffer, cursor shape and position */
         if( ( zh_itemType( pInfo->pNewVal ) & ZH_IT_ARRAY ) &&
             zh_arrayLen( pInfo->pNewVal ) == 7 )
         {
            ZH_GTSELF_DISPBEGIN( pGT );
            if( zh_arrayGetCLen( pInfo->pNewVal, 6 ) > 0 )
            {
               int iFlag = ZH_GTSELF_SETFLAG( pGT, ZH_GTI_COMPATBUFFER, 0 );
               ZH_GTSELF_REST( pGT, 0, 0, zh_arrayGetNI( pInfo->pNewVal, 4 ),
                               zh_arrayGetNI( pInfo->pNewVal, 5 ),
                               zh_arrayGetCPtr( pInfo->pNewVal, 6 ) );
               ZH_GTSELF_SETFLAG( pGT, ZH_GTI_COMPATBUFFER, iFlag );
            }
            ZH_GTSELF_SETPOS( pGT, zh_arrayGetNI( pInfo->pNewVal, 1 ),
                                   zh_arrayGetNI( pInfo->pNewVal, 2 ) );
            ZH_GTSELF_SETCURSORSTYLE( pGT, zh_arrayGetNI( pInfo->pNewVal, 3 ) );
            ZH_GTSELF_DISPEND( pGT );
            ZH_GTSELF_FLUSH( pGT );
         }
         break;

      case ZH_GTI_CLIPBOARDDATA:
         if( zh_itemType( pInfo->pNewVal ) & ZH_IT_STRING )
         {
            /* set new Clipboard value */
            zh_gt_setClipboard( zh_itemGetCPtr( pInfo->pNewVal ),
                                zh_itemGetCLen( pInfo->pNewVal ) );
         }
         else
         {
            /* get Clipboard value */
            char * pszClipData;
            ZH_SIZE nLen;

            if( zh_gt_getClipboard( &pszClipData, &nLen ) )
               pInfo->pResult = zh_itemPutCLPtr( pInfo->pResult, pszClipData, nLen );
            else
               pInfo->pResult = zh_itemPutC( pInfo->pResult, NULL );
         }
         break;

      case ZH_GTI_CLIPBOARDPASTE:
         if( ZH_GTSELF_INFO( pGT, ZH_GTI_CLIPBOARDDATA, pInfo ) )
            ZH_GTSELF_INKEYSETTEXT( pGT, zh_itemGetCPtr( pInfo->pResult ),
                                         zh_itemGetCLen( pInfo->pResult ) );
         break;

      case ZH_GTI_NOTIFIERBLOCK:
         zh_gt_def_SetBlock( &pGT->pNotifierBlock, pInfo );
         break;

      case ZH_GTI_INKEYFILTER:
         zh_gt_def_SetBlock( &pGT->pInkeyFilterBlock, pInfo );
         break;

      case ZH_GTI_INKEYREAD:
         zh_gt_def_SetBlock( &pGT->pInkeyReadBlock, pInfo );
         break;

      case ZH_GTI_CARGO:
         if( pGT->pCargo )
         {
            if( pInfo->pResult )
               zh_itemCopy( pInfo->pResult, pGT->pCargo );
            else
               pInfo->pResult = zh_itemNew( pGT->pCargo );
         }
         if( pInfo->pNewVal )
         {
            if( pGT->pCargo )
            {
               zh_itemRelease( pGT->pCargo );
               pGT->pCargo = NULL;
            }
            pGT->pCargo = zh_itemNew( pInfo->pNewVal );
            zh_gcUnlock( pGT->pCargo );
         }
         break;

      case ZH_GTI_RESIZEMODE:
         pInfo->pResult = zh_itemPutNInt( pInfo->pResult, ZH_GTI_RESIZEMODE_FONT );
         break;

      case ZH_GTI_RESIZABLE:
         pInfo->pResult = zh_itemPutL( pInfo->pResult, ZH_FALSE );
         break;

      case ZH_GTI_CLOSABLE:
         pInfo->pResult = zh_itemPutL( pInfo->pResult, ZH_TRUE );
         break;

      case ZH_GTI_FONTNAME:
         pInfo->pResult = zh_itemPutC( pInfo->pResult, NULL );
         break;

      case ZH_GTI_FONTSIZE:
      case ZH_GTI_FONTWIDTH:
      case ZH_GTI_FONTWEIGHT:
      case ZH_GTI_FONTQUALITY:
      case ZH_GTI_FONTATTRIBUTE:
         pInfo->pResult = zh_itemPutNI( pInfo->pResult, 0 );
         break;

      case ZH_GTI_FONTSEL:
         pInfo->pResult = zh_itemPutC( pInfo->pResult, NULL );
         break;

      case ZH_GTI_VERSION:
         pInfo->pResult = zh_itemPutC( pInfo->pResult,
                  ZH_GTSELF_VERSION( pGT, zh_itemGetNI( pInfo->pNewVal ) ) );
         break;

      default:
         return ZH_FALSE;
   }

   return ZH_TRUE;
}

static int zh_gt_def_Alert( PZH_GT pGT, PZH_ITEM pMessage, PZH_ITEM pOptions,
                            int iClrNorm, int iClrHigh, double dDelay )
{
   int iRet = 0, iOptions;

   if( pMessage && ZH_IS_STRING( pMessage ) &&
       pOptions && ( iOptions = ( int ) zh_arrayLen( pOptions ) ) > 0 )
   {
      ZH_SIZE nLen;
      void * hMessage;
      const ZH_WCHAR * szMessageW = zh_itemGetStrU16( pMessage, ZH_CODEPAGE_ENDIAN_NATIVE, &hMessage, &nLen );
      ZH_BOOL fScreen = ZH_FALSE, fKeyBoard = ZH_FALSE;
      PZH_CODEPAGE cdp = ZH_GTSELF_HOSTCP( pGT );
      char szKey[ ZH_MAX_CHAR_LEN ];
      ZH_SIZE nChar;
      int iKey, i, iRows, iCols;
      ZH_GT_INFO gtInfo;

      memset( &gtInfo, 0, sizeof( gtInfo ) );

      ZH_GTSELF_INFO( pGT, ZH_GTI_ISSCREENPOS, &gtInfo );
      if( gtInfo.pResult )
      {
         fScreen = zh_itemGetL( gtInfo.pResult );
      }
      ZH_GTSELF_INFO( pGT, ZH_GTI_KBDSUPPORT, &gtInfo );
      if( gtInfo.pResult )
      {
         fKeyBoard = zh_itemGetL( gtInfo.pResult );
         zh_itemRelease( gtInfo.pResult );
      }
      ZH_GTSELF_GETSIZE( pGT, &iRows, &iCols );
      if( iCols <= 4 || iRows <= 4 )
         fScreen = ZH_FALSE;

      if( fScreen )
      {
         void * pBuffer = NULL;
         int iDspCount, iStyle, iRow, iCol, iTop, iLeft, iBottom, iRight, iPos, iClr;
         ZH_UINT ulLines = 0, ulWidth = 0, ulCurrWidth = 0, ulMsg = 0, ulDst = 0,
                 ulLast = 0, ulSpace1 = 0, ulSpace2 = 0, ulDefWidth, ulMaxWidth;
         ZH_WCHAR * szMsgDsp;
         int iFlag;

         ulMaxWidth = iCols - 4;
         ulDefWidth = ( ulMaxWidth * 3 ) >> 2;
         if( ulDefWidth == 0 )
            ulDefWidth = 1;
         szMsgDsp = ( ZH_WCHAR * ) zh_xgrab( ( nLen + ( nLen / ulDefWidth ) + 1 ) * sizeof( ZH_WCHAR ) );

         while( ulMsg < nLen )
         {
            if( szMessageW[ ulMsg ] == '\n' )
            {
               if( ulCurrWidth > ulMaxWidth )
               {
                  ulDst = ulLast;
               }
               else
               {
                  ++ulLines;
                  if( ulCurrWidth > ulWidth )
                     ulWidth = ulCurrWidth;
                  ulCurrWidth = ulSpace1 = ulSpace2 = 0;
                  szMsgDsp[ ulDst++ ] = '\n';
                  ulLast = ulDst;
               }
            }
            else
            {
               if( szMessageW[ ulMsg ] == ' ' )
               {
                  if( ulCurrWidth <= ulDefWidth )
                     ulSpace1 = ulMsg;
                  else if( ulCurrWidth <= ulMaxWidth && ! ulSpace2 )
                     ulSpace2 = ulMsg;
               }
               szMsgDsp[ ulDst++ ] = szMessageW[ ulMsg ];
               ++ulCurrWidth;
               if( ulCurrWidth > ulDefWidth && ulSpace1 )
               {
                  ulCurrWidth -= ulMsg - ulSpace1 + 1;
                  ulDst -= ulMsg - ulSpace1 + 1;
                  ulMsg = ulSpace1;
                  ++ulLines;
                  if( ulCurrWidth > ulWidth )
                     ulWidth = ulCurrWidth;
                  ulCurrWidth = ulSpace1 = ulSpace2 = 0;
                  szMsgDsp[ ulDst++ ] = '\n';
                  ulLast = ulDst;
               }
               else if( ulCurrWidth > ulMaxWidth )
               {
                  if( ulSpace2 )
                  {
                     ulCurrWidth -= ulMsg - ulSpace2 + 1;
                     ulDst -= ulMsg - ulSpace2 + 1;
                     ulMsg = ulSpace2;
                     ++ulLines;
                     if( ulCurrWidth > ulWidth )
                        ulWidth = ulCurrWidth;
                     ulCurrWidth = ulSpace1 = ulSpace2 = 0;
                     szMsgDsp[ ulDst++ ] = '\n';
                     ulLast = ulDst;
                  }
                  else
                  {
                     ulCurrWidth--;
                     ulDst--;
                     ulMsg--;
                     szMsgDsp[ ulDst++ ] = '\n';
                     ulLast = ulDst;
                     ++ulLines;
                     if( ulCurrWidth > ulWidth )
                        ulWidth = ulCurrWidth;
                     ulCurrWidth = ulSpace1 = ulSpace2 = 0;
                  }
               }
            }
            ++ulMsg;
         }
         ulLines++;
         if( ulCurrWidth > ulWidth )
            ulWidth = ulCurrWidth;
         if( ulLines == 1 && ulWidth < ulDefWidth )
            ulWidth += ZH_MIN( 4, ulDefWidth - ulWidth );

         ulCurrWidth = 0;
         for( i = 1; i <= iOptions; ++i )
         {
            nLen = zh_itemCopyStrU16( zh_arrayGetItemPtr( pOptions, i ), ZH_CODEPAGE_ENDIAN_NATIVE, NULL, 0 );
            ulCurrWidth += ( ZH_UINT ) nLen + 4;
         }
         if( ulCurrWidth > ulMaxWidth )
            ulCurrWidth = ulMaxWidth;
         if( ulCurrWidth > ulWidth )
            ulWidth = ulCurrWidth;
         if( ( ZH_SIZE ) iRows < ulLines + 4 )
            ulLines = iRows - 4;
         iTop = ( iRows - ulLines - 4 ) >> 1;
         iLeft = ( iCols - ulWidth - 4 ) >> 1;
         iBottom = iTop + ulLines + 3;
         iRight = iLeft + ulWidth + 3;

         if( iClrNorm == 0 )
            iClrNorm = 0x4F;
         if( iClrHigh == 0 )
            iClrHigh = 0x1F;
         iDspCount = ZH_GTSELF_DISPCOUNT( pGT );
         if( iDspCount == 0 )
            ZH_GTSELF_DISPBEGIN( pGT );
         ZH_GTSELF_GETPOS( pGT, &iRow, &iCol );
         iStyle = ZH_GTSELF_GETCURSORSTYLE( pGT );
         ZH_GTSELF_SETCURSORSTYLE( pGT, SC_NONE );
         iFlag = ZH_GTSELF_SETFLAG( pGT, ZH_GTI_COMPATBUFFER, 0 );
         nLen = ZH_GTSELF_RECTSIZE( pGT, iTop, iLeft, iBottom, iRight );
         if( nLen )
         {
            pBuffer = zh_xgrab( nLen );
            ZH_GTSELF_SAVE( pGT, iTop, iLeft, iBottom, iRight, pBuffer );
         }
         ZH_GTSELF_BOXS( pGT, iTop, iLeft, iBottom, iRight, NULL, iClrNorm );
         ZH_GTSELF_BOX( pGT, iTop + 1, iLeft + 1, iBottom - 1, iRight - 1, NULL, iClrNorm );
         ulLast = 0;
         i = iTop + 1;
         for( ulMsg = 0; ulMsg < ulDst; ++ulMsg )
         {
            if( szMsgDsp[ ulMsg ] == '\n' )
            {
               if( ulMsg > ulLast )
               {
                  nLen = ulMsg - ulLast;
                  if( nLen > ulWidth )
                     nLen = ulWidth;
                  ZH_GTSELF_PUTTEXTW( pGT, i, iLeft + ( int ) ( ( ulWidth - nLen + 1 ) >> 1 ) + 2,
                                      iClrNorm, szMsgDsp + ulLast, nLen );
               }
               ulLast = ulMsg + 1;
               if( ++i >= iBottom - 1 )
                  break;
            }
         }
         if( ulMsg > ulLast && i < iBottom - 1 )
         {
            nLen = ulMsg - ulLast;
            if( nLen > ulWidth )
               nLen = ulWidth;
            ZH_GTSELF_PUTTEXTW( pGT, i, iLeft + ( int ) ( ( ulWidth - nLen + 1 ) >> 1 ) + 2,
                                iClrNorm, szMsgDsp + ulLast, nLen );
         }
         zh_xfree( szMsgDsp );

         iPos = 1;
         while( iRet == 0 )
         {
            int iMnuCol;
            ZH_GTSELF_DISPBEGIN( pGT );
            iMnuCol = iLeft + ( ( ulWidth - ulCurrWidth ) >> 1 ) + 3;
            for( i = 1; i <= iOptions; ++i )
            {
               void * hOpt;
               const ZH_WCHAR * szOptW;
               iClr = i == iPos ? iClrHigh : iClrNorm;
               szOptW = zh_arrayGetStrU16( pOptions, i, ZH_CODEPAGE_ENDIAN_NATIVE, &hOpt, &nLen );
               ZH_GTSELF_PUTTEXTW( pGT, iBottom - 1, iMnuCol, iClr, s_szSpaceW, 1 );
               ZH_GTSELF_PUTTEXTW( pGT, iBottom - 1, iMnuCol + 1, iClr, szOptW, nLen );
               ZH_GTSELF_PUTTEXTW( pGT, iBottom - 1, iMnuCol + 1 + ( int ) nLen, iClr, s_szSpaceW, 1 );
               zh_strfree( hOpt );
               iMnuCol += ( int ) nLen + 4;
            }
            while( ZH_GTSELF_DISPCOUNT( pGT ) )
               ZH_GTSELF_DISPEND( pGT );
            ZH_GTSELF_REFRESH( pGT );

            iKey = fKeyBoard ? ZH_GTSELF_INKEYGET( pGT, ZH_TRUE, dDelay, INKEY_ALL ) : 0;

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
               int iMRow, iMCol;
               ZH_GTSELF_MOUSEGETPOS( pGT, &iMRow, &iMCol );
               if( iMRow == iBottom - 1 )
               {
                  iMnuCol = iLeft + ( ( ulWidth - ulCurrWidth ) >> 1 ) + 4;
                  for( i = 1; i <= iOptions; ++i )
                  {
                     nLen = zh_itemCopyStrU16( zh_arrayGetItemPtr( pOptions, i ), ZH_CODEPAGE_ENDIAN_NATIVE, NULL, 0 );
                     if( iMCol >= iMnuCol && iMCol < iMnuCol + ( int ) nLen )
                     {
                        iRet = i;
                        break;
                     }
                     iMnuCol += ( int ) nLen + 4;
                  }
               }
            }
            else if( ( nChar = zh_inkeyKeyString( iKey, szKey, sizeof( szKey ) ) ) > 0 )
            {
               for( i = 1; i <= iOptions; ++i )
               {
                  nLen = zh_arrayGetCLen( pOptions, i );
                  if( nLen > 0 )
                  {
                     ZH_SIZE nIdx1 = 0, nIdx2 = 0;
                     if( zh_cdpCharCaseEq( cdp, szKey, nChar, &nIdx1,
                           zh_arrayGetCPtr( pOptions, i ), nLen, &nIdx2 ) )
                     {
                        iRet = i;
                        break;
                     }
                  }
               }
            }
         }

         if( pBuffer )
         {
            ZH_GTSELF_REST( pGT, iTop, iLeft, iBottom, iRight, pBuffer );
            zh_xfree( pBuffer );
         }
         if( iFlag != 0 )
            ZH_GTSELF_SETFLAG( pGT, ZH_GTI_COMPATBUFFER, iFlag );
         ZH_GTSELF_SETPOS( pGT, iRow, iCol );
         ZH_GTSELF_SETCURSORSTYLE( pGT, iStyle );
         ZH_GTSELF_REFRESH( pGT );
         while( ZH_GTSELF_DISPCOUNT( pGT ) < iDspCount )
            ZH_GTSELF_DISPBEGIN( pGT );
      }
      else
      {
         ZH_SIZE nMsg, nStart = 0;
         const char *szEol = zh_conNewLine();

         for( nMsg = 0; nMsg < nLen; ++nMsg )
         {
            if( szMessageW[ nMsg ] == '\n' )
            {
               if( nMsg > nStart )
                  ZH_GTSELF_WRITECONW( pGT, szMessageW + nStart, nMsg - nStart );
               ZH_GTSELF_WRITECON( pGT, szEol, strlen( szEol ) );
               nStart = nMsg + 1;
            }
         }
         if( nMsg > nStart )
            ZH_GTSELF_WRITECONW( pGT, szMessageW + nStart, nMsg - nStart );
         ZH_GTSELF_WRITECON( pGT, " (", 2 );
         for( i = 1; i <= iOptions; ++i )
         {
            if( i > 1 )
               ZH_GTSELF_WRITECON( pGT, ", ", 2 );
            ZH_GTSELF_WRITECON( pGT, zh_arrayGetCPtr( pOptions, i ),
                                     zh_arrayGetCLen( pOptions, i ) );
         }
         ZH_GTSELF_WRITECON( pGT, ") ", 2 );
         nChar = 0;
         while( iRet == 0 )
         {
            iKey = fKeyBoard ? ZH_GTSELF_INKEYGET( pGT, ZH_TRUE, dDelay, INKEY_ALL ) : 0;
            if( iKey == 0 )
               iRet = 1;
            else if( iKey == K_ESC )
               break;
            else if( ( nChar = zh_inkeyKeyString( iKey, szKey, sizeof( szKey ) ) ) > 0 )
            {
               for( i = 1; i <= iOptions; ++i )
               {
                  nLen = zh_arrayGetCLen( pOptions, i );
                  if( nLen > 0 )
                  {
                     ZH_SIZE nIdx1 = 0, nIdx2 = 0;
                     if( zh_cdpCharCaseEq( cdp, szKey, nChar, &nIdx1,
                           zh_arrayGetCPtr( pOptions, i ), nLen, &nIdx2 ) )
                     {
                        iRet = i;
                        break;
                     }
                  }
               }
            }
         }
         if( iRet > 0 && nChar > 0 )
            ZH_GTSELF_WRITECON( pGT, szKey, nChar );
      }
      zh_strfree( hMessage );
   }

   return iRet;
}

static int zh_gt_def_SetFlag( PZH_GT pGT, int iType, int iNewValue )
{
   int iPrevValue = 0;

   switch( iType )
   {
      case ZH_GTI_COMPATBUFFER:
         iPrevValue = pGT->fVgaCell;
         pGT->fVgaCell = iNewValue != 0;
         break;

      case ZH_GTI_STDOUTCON:
         iPrevValue = pGT->fStdOutCon;
         pGT->fStdOutCon = iNewValue != 0;
         break;

      case ZH_GTI_STDERRCON:
         iPrevValue = pGT->fStdErrCon;
         pGT->fStdErrCon = iNewValue != 0;
         break;

      case ZH_GTI_REDRAWMAX:
         iPrevValue = pGT->iRedrawMax;
         pGT->iRedrawMax = iNewValue;
         break;
   }

   return iPrevValue;
}

static ZH_BOOL zh_gt_def_SetMode( PZH_GT pGT, int iRows, int iCols )
{
   return ZH_GTSELF_RESIZE( pGT, iRows, iCols );
}

static ZH_BOOL zh_gt_def_Resize( PZH_GT pGT, int iRows, int iCols )
{
   if( iRows > 0 && iCols > 0 && pGT->screenBuffer )
   {
      if( pGT->iHeight != iRows || pGT->iWidth != iCols )
      {
         void * pBuffer = NULL;
         ZH_SIZE nLen = ( ZH_SIZE ) iRows * iCols, nIndex;
         ZH_SIZE nSize;
         int iFlag, i;

         iFlag = ZH_GTSELF_SETFLAG( pGT, ZH_GTI_COMPATBUFFER, 0 );
         nSize = ZH_GTSELF_RECTSIZE( pGT, 0, 0, iRows - 1, iCols - 1 );
         if( nSize )
         {
            pBuffer = zh_xgrab( nSize );
            ZH_GTSELF_SAVE( pGT, 0, 0, iRows - 1, iCols - 1, pBuffer );
         }

         pGT->screenBuffer =
               ( PZH_SCREENCELL ) zh_xreallocz( pGT->screenBuffer,
                                             sizeof( ZH_SCREENCELL ) * nLen );
         pGT->prevBuffer =
               ( PZH_SCREENCELL ) zh_xreallocz( pGT->prevBuffer,
                                             sizeof( ZH_SCREENCELL ) * nLen );
         pGT->pLines =
               ( ZH_BOOL * ) zh_xrealloc( pGT->pLines,
                                             sizeof( ZH_BOOL ) * iRows );

         for( i = 0; i < iRows; ++i )
            pGT->pLines[ i ] = ZH_TRUE;
         for( nIndex = 0; nIndex < nLen; ++nIndex )
         {
            pGT->screenBuffer[ nIndex ].c.usChar = ZH_GTSELF_GETCLEARCHAR( pGT );
            pGT->screenBuffer[ nIndex ].c.bColor = ( ZH_BYTE ) ZH_GTSELF_GETCLEARCOLOR( pGT );
            pGT->screenBuffer[ nIndex ].c.bAttr  = 0x00;
            pGT->prevBuffer[ nIndex ].c.bAttr = ZH_GT_ATTR_REFRESH;
         }

         pGT->iHeight = iRows;
         pGT->iWidth = iCols;

         if( pGT->iRow >= pGT->iHeight )
            pGT->iRow = pGT->iHeight - 1;
         if( pGT->iCol >= pGT->iWidth )
            pGT->iCol = pGT->iWidth - 1;

         pGT->fRefresh = ZH_TRUE;

         if( nSize )
         {
            ZH_GTSELF_REST( pGT, 0, 0, iRows - 1, iCols - 1, pBuffer );
            zh_xfree( pBuffer );
         }
         if( iFlag != 0 )
            ZH_GTSELF_SETFLAG( pGT, ZH_GTI_COMPATBUFFER, iFlag );
      }

      return ZH_TRUE;
   }

   return ZH_FALSE;
}

static void zh_gt_def_GetSize( PZH_GT pGT, int * piRows, int  * piCols )
{
   *piRows = pGT->iHeight;
   *piCols = pGT->iWidth;
}

static void zh_gt_def_SemiCold( PZH_GT pGT )
{
   int i;

   for( i = 0; i < pGT->iHeight; ++i )
      pGT->pLines[ i ] = ZH_FALSE;
   pGT->fRefresh = ZH_FALSE;
}

static void zh_gt_def_ColdArea( PZH_GT pGT, int iTop, int iLeft, int iBottom, int iRight )
{
   long lIndex;
   int i;

   if( iTop > iBottom )
   {
      i = iTop;
      iTop = iBottom;
      iBottom = i;
   }
   if( iLeft > iRight )
   {
      i = iLeft;
      iLeft = iRight;
      iRight = i;
   }
   while( iTop <= iBottom )
   {
      for( i = iLeft; i <= iRight; ++i )
      {
         if( ZH_GTSELF_CHECKPOS( pGT, iTop, i, &lIndex ) )
         {
            pGT->screenBuffer[ lIndex ].c.bAttr &= ~ZH_GT_ATTR_REFRESH;
            pGT->prevBuffer[ lIndex ].uiValue = pGT->screenBuffer[ lIndex ].uiValue;
         }
      }
      if( iLeft == 0 && iRight == pGT->iWidth - 1 )
         pGT->pLines[ iTop ] = ZH_FALSE;
      ++iTop;
   }
}

static void zh_gt_def_ExposeArea( PZH_GT pGT, int iTop, int iLeft, int iBottom, int iRight )
{
   long lIndex;
   int i;

   if( iTop > iBottom )
   {
      i = iTop;
      iTop = iBottom;
      iBottom = i;
   }
   if( iLeft > iRight )
   {
      i = iLeft;
      iLeft = iRight;
      iRight = i;
   }
   while( iTop <= iBottom )
   {
      for( i = iLeft; i <= iRight; ++i )
      {
         if( ZH_GTSELF_CHECKPOS( pGT, iTop, i, &lIndex ) )
         {
            pGT->prevBuffer[ lIndex ].c.bAttr = ZH_GT_ATTR_REFRESH;
            pGT->pLines[ iTop ] = ZH_TRUE;
            pGT->fRefresh = ZH_TRUE;
         }
      }
      ++iTop;
   }
}

static void zh_gt_def_TouchLine( PZH_GT pGT, int iRow )
{
   if( iRow >= 0 && iRow < pGT->iHeight )
   {
      pGT->pLines[ iRow ] = ZH_TRUE;
      pGT->fRefresh = ZH_TRUE;
   }
}

static void zh_gt_def_TouchCell( PZH_GT pGT, int iRow, int iCol )
{
   long lIndex;

   if( ZH_GTSELF_CHECKPOS( pGT, iRow, iCol, &lIndex ) )
   {
      pGT->prevBuffer[ lIndex ].c.bAttr = ZH_GT_ATTR_REFRESH;
      pGT->pLines[ iRow ] = ZH_TRUE;
      pGT->fRefresh = ZH_TRUE;
   }
}

static void zh_gt_def_Redraw( PZH_GT pGT, int iRow, int iCol, int iSize )
{
   ZH_SYMBOL_UNUSED( pGT );
   ZH_SYMBOL_UNUSED( iRow );
   ZH_SYMBOL_UNUSED( iCol );
   ZH_SYMBOL_UNUSED( iSize );
}

static void zh_gt_def_RedrawDiff( PZH_GT pGT )
{
   if( pGT->fRefresh )
   {
      int i, l, r, s;
      long lIndex;

      for( i = 0; i < pGT->iHeight; ++i )
      {
         if( pGT->pLines[ i ] )
         {
            lIndex = ( long ) i * pGT->iWidth;
            for( l = 0; l < pGT->iWidth; ++l, ++lIndex )
            {
               if( pGT->prevBuffer[ lIndex ].uiValue !=
                   pGT->screenBuffer[ lIndex ].uiValue )
               {
                  pGT->prevBuffer[ lIndex ].uiValue =
                     pGT->screenBuffer[ lIndex ].uiValue;
                  s = r = l;
                  while( ++l < pGT->iWidth )
                  {
                     ++lIndex;
                     if( pGT->prevBuffer[ lIndex ].uiValue !=
                         pGT->screenBuffer[ lIndex ].uiValue )
                     {
                        pGT->prevBuffer[ lIndex ].uiValue =
                           pGT->screenBuffer[ lIndex ].uiValue;
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

static void zh_gt_def_Refresh( PZH_GT pGT )
{
   ZH_GTSELF_REDRAWDIFF( pGT );
}

static void zh_gt_def_Flush( PZH_GT pGT )
{
   if( ZH_GTSELF_DISPCOUNT( pGT ) == 0 )
      ZH_GTSELF_REFRESH( pGT );
}

static int zh_gt_def_ReadKey( PZH_GT pGT, int iEventMask )
{
   return ZH_GTSELF_MOUSEREADKEY( pGT, iEventMask );
}

/* helper internal function */
static int zh_gt_def_InkeyFilter( PZH_GT pGT, int iKey, int iEventMask )
{
   int iMask;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_def_InkeyFilter(%p,%d,%d)", ( void * ) pGT, iKey, iEventMask ) );

   ZH_SYMBOL_UNUSED( pGT );

   if( ZH_INKEY_ISEXT( iKey ) )
   {
      if( ZH_INKEY_ISEVENT( iKey ) )
         iMask = ZH_INKEY_GTEVENT;
      else if( ZH_INKEY_ISMOUSEPOS( iKey ) )
         iMask = INKEY_MOVE;
      else if( ZH_INKEY_ISMOUSEKEY( iKey ) )
      {
         switch( ZH_INKEY_VALUE( iKey ) )
         {
            case K_MOUSEMOVE:
            case K_MMLEFTDOWN:
            case K_MMRIGHTDOWN:
            case K_MMMIDDLEDOWN:
            case K_NCMOUSEMOVE:
               iMask = INKEY_MOVE;
               break;
            case K_LBUTTONDOWN:
            case K_LDBLCLK:
               iMask = INKEY_LDOWN;
               break;
            case K_LBUTTONUP:
               iMask = INKEY_LUP;
               break;
            case K_RBUTTONDOWN:
            case K_RDBLCLK:
               iMask = INKEY_RDOWN;
               break;
            case K_RBUTTONUP:
               iMask = INKEY_RUP;
               break;
            case K_MBUTTONDOWN:
            case K_MBUTTONUP:
            case K_MDBLCLK:
               iMask = INKEY_MMIDDLE;
               break;
            case K_MWFORWARD:
            case K_MWBACKWARD:
               iMask = INKEY_MWHEEL;
               break;
            default:
               iMask = INKEY_KEYBOARD;
         }
      }
      else
         iMask = INKEY_KEYBOARD;
   }
   else
   {
      switch( iKey )
      {
         case K_MOUSEMOVE:
         case K_MMLEFTDOWN:
         case K_MMRIGHTDOWN:
         case K_MMMIDDLEDOWN:
         case K_NCMOUSEMOVE:
            iMask = INKEY_MOVE;
            break;
         case K_LBUTTONDOWN:
         case K_LDBLCLK:
            iMask = INKEY_LDOWN;
            break;
         case K_LBUTTONUP:
            iMask = INKEY_LUP;
            break;
         case K_RBUTTONDOWN:
         case K_RDBLCLK:
            iMask = INKEY_RDOWN;
            break;
         case K_RBUTTONUP:
            iMask = INKEY_RUP;
            break;
         case K_MBUTTONDOWN:
         case K_MBUTTONUP:
         case K_MDBLCLK:
            iMask = INKEY_MMIDDLE;
            break;
         case K_MWFORWARD:
         case K_MWBACKWARD:
            iMask = INKEY_MWHEEL;
            break;
         case ZH_K_RESIZE:
         case ZH_K_CLOSE:
         case ZH_K_GOTFOCUS:
         case ZH_K_LOSTFOCUS:
         case ZH_K_CONNECT:
         case ZH_K_DISCONNECT:
         case ZH_K_TERMINATE:
         case ZH_K_MENU:
            iMask = ZH_INKEY_GTEVENT;
            break;
         default:
            iMask = INKEY_KEYBOARD;
            break;
      }
   }

   if( ( iMask & iEventMask ) == 0 )
      return 0;

   if( ZH_INKEY_ISEXT( iKey ) && ( iEventMask & ZH_INKEY_EXT ) == 0 )
      iKey = zh_inkeyKeyStd( iKey );

   return iKey;
}

/* helper internal function: drop the next key in keyboard buffer */
static void zh_gt_def_InkeyPop( PZH_GT pGT )
{
   if( pGT->StrBuffer )
   {
      if( ++pGT->StrBufferPos >= pGT->StrBufferSize )
      {
         zh_xfree( pGT->StrBuffer );
         pGT->StrBuffer = NULL;
      }
   }
   else if( pGT->inkeyHead != pGT->inkeyTail )
   {
      if( ++pGT->inkeyTail >= pGT->inkeyBufferSize )
         pGT->inkeyTail = 0;
   }
}

/* Put the key into keyboard buffer */
static void zh_gt_def_InkeyPut( PZH_GT pGT, int iKey )
{
   int iHead;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_def_InkeyPut(%p,%d)", ( void * ) pGT, iKey ) );

   iHead = pGT->inkeyHead;

   if( pGT->inkeyHead != pGT->inkeyTail && pGT->inkeyLastPos >= 0 &&
       ( iKey == K_MOUSEMOVE || ZH_INKEY_ISMOUSEPOS( iKey ) ) )
   {

      int iLastKey = pGT->inkeyBuffer[ pGT->inkeyLastPos ];

      if( iLastKey == K_MOUSEMOVE || ZH_INKEY_ISMOUSEPOS( iLastKey ) )
      {
         if( ZH_INKEY_ISMOUSEPOS( iKey ) )
            pGT->inkeyBuffer[ pGT->inkeyLastPos ] = iKey;
         return;
      }
   }

   pGT->inkeyBuffer[ pGT->inkeyLastPos = iHead++ ] = iKey;
   if( iHead >= pGT->inkeyBufferSize )
      iHead = 0;

   if( iHead != pGT->inkeyTail )
      pGT->inkeyHead = iHead;
}

/* Inset the key into head of keyboard buffer */
static void zh_gt_def_InkeyIns( PZH_GT pGT, int iKey )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_def_InkeyIns(%p,%d)", ( void * ) pGT, iKey ) );

   if( --pGT->inkeyTail < 0 )
      pGT->inkeyTail = pGT->inkeyBufferSize - 1;

   pGT->inkeyBuffer[ pGT->inkeyTail ] = iKey;

   /* When the buffer is full new event overwrite the last one
    * in the buffer. [druzus]
    */
   if( pGT->inkeyHead == pGT->inkeyTail )
   {
      if( --pGT->inkeyHead < 0 )
         pGT->inkeyHead = pGT->inkeyBufferSize - 1;
      pGT->inkeyLastPos = -1;
   }
}

/* helper internal function */
static ZH_BOOL zh_gt_def_InkeyNextCheck( PZH_GT pGT, int iEventMask, int * iKey )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_def_InkeyNextCheck(%p,%p)", ( void * ) pGT, ( void * ) iKey ) );

   if( pGT->StrBuffer )
   {
      *iKey = pGT->StrBuffer[ pGT->StrBufferPos ];
      if( *iKey >= 128 )
      {
         *iKey = ZH_INKEY_NEW_UNICODE( *iKey );
         if( ( iEventMask & ZH_INKEY_EXT ) == 0 )
            *iKey = zh_inkeyKeyStd( *iKey );
      }
   }
   else if( pGT->inkeyHead != pGT->inkeyTail )
   {
      *iKey = zh_gt_def_InkeyFilter( pGT, pGT->inkeyBuffer[ pGT->inkeyTail ], iEventMask );
   }
   else
   {
      return ZH_FALSE;
   }

   if( *iKey == 0 )
   {
      zh_gt_def_InkeyPop( pGT );
      return ZH_FALSE;
   }

   return ZH_TRUE;
}

/* helper internal function */
static void zh_gt_def_InkeyPollDo( PZH_GT pGT )
{
   int iKey;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_def_InkeyPollDo(%p)", ( void * ) pGT ) );

   iKey = ZH_GTSELF_READKEY( pGT, ZH_INKEY_ALL | ZH_INKEY_EXT );

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_def_InkeyPollDo iKey=%d inkey_value=%c", iKey, ZH_INKEY_VALUE( iKey ) ) );

   if( iKey )
   {
      if( ZH_INKEY_ISEXT( iKey ) )
      {
         if( ZH_INKEY_FLAGS( iKey ) & ZH_KF_ALT )
         {
            switch( ZH_INKEY_VALUE( iKey ) )
            {
               case 'C':
               case 'c':
                  if( zh_setGetCancel() )
                  {
                     zh_vmRequestCancel();   /* Request cancellation */
                     return;
                  }
                  break;
               case 'D':
               case 'd':
                  if( zh_setGetDebug() )
                  {
                     zh_vmRequestDebug();    /* Request the debugger */
                     return;
                  }
            }
         }
      }
      else
      {
         switch( iKey )
         {
            case ZH_BREAK_FLAG:           /* Check for Ctrl+Break */
            case K_ALT_C:                 /* Check for normal Alt+C */
               if( zh_setGetCancel() )
               {
                  zh_vmRequestCancel();   /* Request cancellation */
                  return;
               }
               break;
            case K_ALT_D:                 /* Check for Alt+D */
               if( zh_setGetDebug() )
               {
                  zh_vmRequestDebug();    /* Request the debugger */
                  return;
               }
         }
      }
      ZH_GTSELF_INKEYPUT( pGT, iKey );
   }
}

/* Poll the console keyboard to stuff the Ziher buffer */
static void zh_gt_def_InkeyPoll( PZH_GT pGT )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_def_InkeyPoll(%p)", ( void * ) pGT ) );

   if( zh_setGetTypeAhead() )
      zh_gt_def_InkeyPollDo( pGT );
}

/* Return the next key without extracting it */
static int zh_gt_def_InkeyNext( PZH_GT pGT, int iEventMask )
{
   int iKey = 0;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_def_InkeyNext(%p,%d)", ( void * ) pGT, iEventMask ) );

   ZH_GTSELF_INKEYPOLL( pGT );
   zh_gt_def_InkeyNextCheck( pGT, iEventMask, &iKey );

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_def_InkeyNext iKey=%d", iKey ) );
   return iKey;
}

/* Wait for keyboard input */
static int zh_gt_def_InkeyGet( PZH_GT pGT, ZH_BOOL fWait, double dSeconds, int iEventMask )
{
   ZH_MAXUINT timer;
   ZH_MAXINT timeout;
   PZH_ITEM pKey;
   ZH_BOOL fPop;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_def_InkeyGet(%p,%d,%f,%d)", ( void * ) pGT, ( int ) fWait, dSeconds, iEventMask ) );

   pKey = NULL;

   if( pGT->pInkeyReadBlock )
   {
      int iKey;
      ZH_GTSELF_UNLOCK( pGT );
      iKey = zh_itemGetNI( zh_vmEvalBlock( pGT->pInkeyReadBlock ) );
      ZH_GTSELF_LOCK( pGT );
      if( iKey != 0 )
         return iKey;
   }

   timeout = ( fWait && dSeconds * 100 >= 1 ) ? ( ZH_MAXINT ) ( dSeconds * 1000 ) : -1;
   timer = zh_timerInit( timeout );

   for( ;; )
   {
      zh_gt_def_InkeyPollDo( pGT );
      fPop = zh_gt_def_InkeyNextCheck( pGT, iEventMask, &pGT->inkeyLast );

      if( fPop )
      {
         zh_gt_def_InkeyPop( pGT );
         if( ! pGT->pInkeyFilterBlock )
            break;
         pKey = zh_itemPutNI( pKey, pGT->inkeyLast );
         ZH_GTSELF_UNLOCK( pGT );
         pGT->inkeyLast = zh_itemGetNI( zh_vmEvalBlockV( pGT->pInkeyFilterBlock, 1, pKey ) );
         ZH_GTSELF_LOCK( pGT );
         if( pGT->inkeyLast != 0 )
            break;
      }

      /* immediately break if a VM request is pending. */
      if( ! fWait || ( timeout = zh_timerTest( timeout, &timer ) ) == 0 ||
          zh_vmRequestQuery() != 0 )
         break;

      ZH_GTSELF_UNLOCK( pGT );
      zh_idleState();
      ZH_GTSELF_LOCK( pGT );
   }

   if( pKey )
      zh_itemRelease( pKey );

   zh_idleReset();

   return fPop ? pGT->inkeyLast : 0;
}

/* Return the value of the last key that was extracted */
static int zh_gt_def_InkeyLast( PZH_GT pGT, int iEventMask )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_def_InkeyLast(%p,%d)", ( void * ) pGT, iEventMask ) );

   ZH_GTSELF_INKEYPOLL( pGT );

   return zh_gt_def_InkeyFilter( pGT, pGT->inkeyLast, iEventMask );
}

/* Set LastKey() value and return previous value */
static int zh_gt_def_InkeySetLast( PZH_GT pGT, int iKey )
{
   int iLast;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_def_InkeySetLast(%p,%d)", ( void * ) pGT, iKey ) );

   iLast = pGT->inkeyLast;
   pGT->inkeyLast = iKey;

   return iLast;
}

/* Set text into inkey buffer */
static void zh_gt_def_InkeySetText( PZH_GT pGT, const char * szText, ZH_SIZE nLen )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_def_InkeySetText(%p,%s,%" ZH_PFS "u)", ( void * ) pGT, szText, nLen ) );

   if( pGT->StrBuffer )
   {
      zh_xfree( pGT->StrBuffer );
      pGT->StrBuffer = NULL;
   }

   if( szText && nLen )
   {
      PZH_CODEPAGE cdp = zh_vmCDP();
      ZH_SIZE nIndex = 0;
      ZH_WCHAR wc;

      pGT->StrBufferSize = pGT->StrBufferPos = 0;
      pGT->StrBuffer = ( ZH_WCHAR * ) zh_xgrab( nLen * sizeof( ZH_WCHAR ) );
      while( ZH_CODEPAGE_CHAR_GET( cdp, szText, nLen, &nIndex, &wc ) )
         pGT->StrBuffer[ pGT->StrBufferSize++ ] = wc == ';' ? ZH_CHAR_CR : wc;

      if( pGT->StrBufferSize == 0 )
      {
         zh_xfree( pGT->StrBuffer );
         pGT->StrBuffer = NULL;
      }
   }
}

/* Reset the keyboard buffer */
static void zh_gt_def_InkeyReset( PZH_GT pGT )
{
   int iTypeAhead;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_def_InkeyReset(%p)", ( void * ) pGT ) );

   if( pGT->StrBuffer )
   {
      zh_xfree( pGT->StrBuffer );
      pGT->StrBuffer = NULL;
   }

   pGT->inkeyHead = 0;
   pGT->inkeyTail = 0;
   pGT->inkeyLastPos = -1;

   iTypeAhead = zh_setGetTypeAhead();

   if( iTypeAhead != pGT->inkeyBufferSize )
   {
      if( pGT->inkeyBufferSize > ZH_DEFAULT_INKEY_BUFSIZE )
         zh_xfree( pGT->inkeyBuffer );

      if( iTypeAhead > ZH_DEFAULT_INKEY_BUFSIZE )
      {
         pGT->inkeyBufferSize = iTypeAhead;
         pGT->inkeyBuffer = ( int * ) zh_xgrab( pGT->inkeyBufferSize * sizeof( int ) );
      }
      else
      {
         pGT->inkeyBufferSize = ZH_DEFAULT_INKEY_BUFSIZE;
         pGT->inkeyBuffer = pGT->defaultKeyBuffer;
      }
   }
}

/* reset inkey pool to default state and free any allocated resources */
static void zh_gt_def_InkeyExit( PZH_GT pGT )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_def_InkeyExit(%p)", ( void * ) pGT ) );

   if( pGT->StrBuffer )
   {
      zh_xfree( pGT->StrBuffer );
      pGT->StrBuffer = NULL;
   }

   if( pGT->inkeyBufferSize > ZH_DEFAULT_INKEY_BUFSIZE )
   {
      zh_xfree( pGT->inkeyBuffer );
      pGT->inkeyBufferSize = ZH_DEFAULT_INKEY_BUFSIZE;
      pGT->inkeyBuffer = pGT->defaultKeyBuffer;
   }
}

static void zh_gt_def_MouseInit( PZH_GT pGT )
{
   ZH_SYMBOL_UNUSED( pGT );
}

static void zh_gt_def_MouseExit( PZH_GT pGT )
{
   ZH_SYMBOL_UNUSED( pGT );
}

static ZH_BOOL zh_gt_def_MouseIsPresent( PZH_GT pGT )
{
   ZH_SYMBOL_UNUSED( pGT );
   return ZH_FALSE;
}

static void zh_gt_def_MouseShow( PZH_GT pGT )
{
   ZH_SYMBOL_UNUSED( pGT );
}

static void zh_gt_def_MouseHide( PZH_GT pGT )
{
   ZH_SYMBOL_UNUSED( pGT );
}

static ZH_BOOL zh_gt_def_MouseGetCursor( PZH_GT pGT )
{
   return pGT->fMouseVisible;
}

static void zh_gt_def_MouseSetCursor( PZH_GT pGT, ZH_BOOL fVisible )
{
   if( fVisible )
   {
      ZH_GTSELF_MOUSESHOW( pGT );
      pGT->fMouseVisible = ZH_TRUE;
   }
   else if( pGT->fMouseVisible )
   {
      ZH_GTSELF_MOUSEHIDE( pGT );
      pGT->fMouseVisible = ZH_FALSE;
   }
}

static int zh_gt_def_MouseRow( PZH_GT pGT )
{
   int iRow, iCol;

   ZH_GTSELF_MOUSEGETPOS( pGT, &iRow, &iCol );
   return iRow;
}

static int zh_gt_def_MouseCol( PZH_GT pGT )
{
   int iRow, iCol;

   ZH_GTSELF_MOUSEGETPOS( pGT, &iRow, &iCol );
   return iCol;
}

static void zh_gt_def_MouseGetPos( PZH_GT pGT, int * piRow, int * piCol )
{
   *piRow = pGT->iMouseLastRow;
   *piCol = pGT->iMouseLastCol;
}

static void zh_gt_def_MouseSetPos( PZH_GT pGT, int iRow, int iCol )
{
   pGT->iMouseLastRow = iRow;
   pGT->iMouseLastCol = iCol;
}

static void zh_gt_def_MouseSetBounds( PZH_GT pGT, int iTop, int iLeft, int iBottom, int iRight )
{
   ZH_SYMBOL_UNUSED( pGT );
   ZH_SYMBOL_UNUSED( iTop );
   ZH_SYMBOL_UNUSED( iLeft );
   ZH_SYMBOL_UNUSED( iBottom );
   ZH_SYMBOL_UNUSED( iRight );
}

static void zh_gt_def_MouseGetBounds( PZH_GT pGT, int * piTop, int * piLeft, int * piBottom, int * piRight )
{
   *piTop = *piLeft = 0;
   ZH_GTSELF_GETSIZE( pGT, piBottom, piRight );
   --( *piBottom );
   --( *piRight );
}

typedef struct
{
   int iRow;
   int iCol;
   int fVisible;
   int iTop;
   int iLeft;
   int iBottom;
   int iRight;
} _ZH_MOUSE_STORAGE;

static int zh_gt_def_mouseStorageSize( PZH_GT pGT )
{
   ZH_SYMBOL_UNUSED( pGT );

   return sizeof( _ZH_MOUSE_STORAGE );
}

static void zh_gt_def_mouseSaveState( PZH_GT pGT, void * pBuffer )
{
   _ZH_MOUSE_STORAGE * pStore = ( _ZH_MOUSE_STORAGE * ) pBuffer;
   int iRow, iCol, iTop, iLeft, iBottom, iRight;

   ZH_GTSELF_MOUSEGETPOS( pGT, &iRow, &iCol );
   ZH_GTSELF_MOUSEGETBOUNDS( pGT, &iTop, &iLeft, &iBottom, &iRight );

   pStore->iRow     = iRow;
   pStore->iCol     = iCol;
   pStore->fVisible = ZH_GTSELF_MOUSEGETCURSOR( pGT );
   pStore->iTop     = iTop;
   pStore->iLeft    = iLeft;
   pStore->iBottom  = iBottom;
   pStore->iRight   = iRight;
}

static void zh_gt_def_mouseRestoreState( PZH_GT pGT, const void * pBuffer )
{
   const _ZH_MOUSE_STORAGE * pStore = ( const _ZH_MOUSE_STORAGE * ) pBuffer;

   ZH_GTSELF_MOUSESETBOUNDS( pGT, pStore->iTop, pStore->iLeft, pStore->iBottom, pStore->iRight );
   ZH_GTSELF_MOUSESETPOS( pGT, pStore->iRow, pStore->iCol );
   ZH_GTSELF_MOUSESETCURSOR( pGT, pStore->fVisible );
}

static int zh_gt_def_mouseGetDoubleClickSpeed( PZH_GT pGT )
{
   return pGT->iDoubleClickSpeed;
}

static void zh_gt_def_mouseSetDoubleClickSpeed( PZH_GT pGT, int iSpeed )
{
   if( iSpeed > 0 )
      pGT->iDoubleClickSpeed = iSpeed;
}

static int zh_gt_def_MouseCountButton( PZH_GT pGT )
{
   ZH_SYMBOL_UNUSED( pGT );

   return 0;
}

static ZH_BOOL zh_gt_def_MouseButtonState( PZH_GT pGT, int iButton )
{
   ZH_SYMBOL_UNUSED( pGT );
   ZH_SYMBOL_UNUSED( iButton );

   return ZH_FALSE;
}

static ZH_BOOL zh_gt_def_MouseButtonPressed( PZH_GT pGT, int iButton, int * piRow, int * piCol )
{
   ZH_SYMBOL_UNUSED( pGT );
   ZH_SYMBOL_UNUSED( iButton );
   ZH_SYMBOL_UNUSED( piRow );
   ZH_SYMBOL_UNUSED( piCol );

   return ZH_FALSE;
}

static ZH_BOOL zh_gt_def_MouseButtonReleased( PZH_GT pGT, int iButton, int * piRow, int * piCol )
{
   ZH_SYMBOL_UNUSED( pGT );
   ZH_SYMBOL_UNUSED( iButton );
   ZH_SYMBOL_UNUSED( piRow );
   ZH_SYMBOL_UNUSED( piCol );

   return ZH_FALSE;
}

static int zh_gt_def_MouseReadKey( PZH_GT pGT, int iEventMask )
{
   int iKey = 0, iRow, iCol;

   if( ZH_GTSELF_MOUSEISPRESENT( pGT ) )
   {
      if( iEventMask & INKEY_LDOWN && ZH_GTSELF_MOUSEBUTTONPRESSED( pGT, 0, &iRow, &iCol ) )
      {
         ZH_MAXUINT timer = zh_timerGet();
         if( timer - pGT->nMouseLeftTimer <= ( ZH_MAXUINT ) ZH_GTSELF_MOUSEGETDOUBLECLICKSPEED( pGT ) )
            iKey = K_LDBLCLK;
         else
            iKey = K_LBUTTONDOWN;
         pGT->nMouseLeftTimer = timer;
      }
      else if( iEventMask & INKEY_LUP && ZH_GTSELF_MOUSEBUTTONRELEASED( pGT, 0, &iRow, &iCol ) )
      {
         iKey = K_LBUTTONUP;
      }
      else if( iEventMask & INKEY_RDOWN && ZH_GTSELF_MOUSEBUTTONPRESSED( pGT, 1, &iRow, &iCol ) )
      {
         ZH_MAXUINT timer = zh_timerGet();
         if( timer - pGT->nMouseRightTimer <= ( ZH_MAXUINT ) ZH_GTSELF_MOUSEGETDOUBLECLICKSPEED( pGT ) )
            iKey = K_RDBLCLK;
         else
            iKey = K_RBUTTONDOWN;
         pGT->nMouseRightTimer = timer;
      }
      else if( iEventMask & INKEY_RUP && ZH_GTSELF_MOUSEBUTTONRELEASED( pGT, 1, &iRow, &iCol ) )
      {
         iKey = K_RBUTTONUP;
      }
      else if( iEventMask & INKEY_MMIDDLE && ZH_GTSELF_MOUSEBUTTONPRESSED( pGT, 2, &iRow, &iCol ) )
      {
         ZH_MAXUINT timer = zh_timerGet();
         if( timer - pGT->nMouseMiddleTimer <= ( ZH_MAXUINT ) ZH_GTSELF_MOUSEGETDOUBLECLICKSPEED( pGT ) )
            iKey = K_MDBLCLK;
         else
            iKey = K_MBUTTONDOWN;
         pGT->nMouseMiddleTimer = timer;
      }
      else if( iEventMask & INKEY_MMIDDLE && ZH_GTSELF_MOUSEBUTTONRELEASED( pGT, 2, &iRow, &iCol ) )
      {
         iKey = K_MBUTTONUP;
      }
      else if( iEventMask & INKEY_MOVE )
      {
         ZH_GTSELF_MOUSEGETPOS( pGT, &iRow, &iCol );
         if( iRow != pGT->iMouseLastRow || iCol != pGT->iMouseLastCol )
         {
            pGT->iMouseLastRow = iRow;
            pGT->iMouseLastCol = iCol;
            iKey = ZH_INKEY_NEW_MPOS( iCol, iRow );
         }
      }
   }
   return iKey;
}

static int zh_gt_def_GfxPrimitive( PZH_GT pGT, int iType, int iTop, int iLeft, int iBottom, int iRight, int iColor )
{
   ZH_SYMBOL_UNUSED( pGT );
   ZH_SYMBOL_UNUSED( iType );
   ZH_SYMBOL_UNUSED( iTop );
   ZH_SYMBOL_UNUSED( iLeft );
   ZH_SYMBOL_UNUSED( iBottom );
   ZH_SYMBOL_UNUSED( iRight );
   ZH_SYMBOL_UNUSED( iColor );

   return 0;
}

static void zh_gt_def_GfxText( PZH_GT pGT, int iTop, int iLeft, const char * szText, int iColor, int iSize, int iWidth )
{
   ZH_SYMBOL_UNUSED( pGT );
   ZH_SYMBOL_UNUSED( iTop );
   ZH_SYMBOL_UNUSED( iLeft );
   ZH_SYMBOL_UNUSED( szText );
   ZH_SYMBOL_UNUSED( iColor );
   ZH_SYMBOL_UNUSED( iSize );
   ZH_SYMBOL_UNUSED( iWidth );
}

static void zh_gt_def_WhoCares( PZH_GT pGT, void * pCargo )
{
   ZH_SYMBOL_UNUSED( pGT );
   ZH_SYMBOL_UNUSED( pCargo );
}

/* - */

#if defined( __GNUC__ ) && 0
static const ZH_GT_FUNCS s_gtCoreFunc =
{
   Lock                       : zh_gt_def_Lock                          ,
   Unlock                     : zh_gt_def_Unlock                        ,
   Init                       : zh_gt_def_Init                          ,
   Exit                       : zh_gt_def_Exit                          ,
   New                        : zh_gt_def_New                           ,
   Free                       : zh_gt_def_Free                          ,
   Mark                       : zh_gt_def_Mark                          ,
   Resize                     : zh_gt_def_Resize                        ,
   SetMode                    : zh_gt_def_SetMode                       ,
   GetSize                    : zh_gt_def_GetSize                       ,
   SemiCold                   : zh_gt_def_SemiCold                      ,
   ColdArea                   : zh_gt_def_ColdArea                      ,
   ExposeArea                 : zh_gt_def_ExposeArea                    ,
   ScrollArea                 : zh_gt_def_ScrollArea                    ,
   TouchLine                  : zh_gt_def_TouchLine                     ,
   TouchCell                  : zh_gt_def_TouchCell                     ,
   Redraw                     : zh_gt_def_Redraw                        ,
   RedrawDiff                 : zh_gt_def_RedrawDiff                    ,
   Refresh                    : zh_gt_def_Refresh                       ,
   Flush                      : zh_gt_def_Flush                         ,
   MaxCol                     : zh_gt_def_MaxCol                        ,
   MaxRow                     : zh_gt_def_MaxRow                        ,
   CheckPos                   : zh_gt_def_CheckPos                      ,
   SetPos                     : zh_gt_def_SetPos                        ,
   GetPos                     : zh_gt_def_GetPos                        ,
   IsColor                    : zh_gt_def_IsColor                       ,
   GetColorStr                : zh_gt_def_GetColorStr                   ,
   SetColorStr                : zh_gt_def_SetColorStr                   ,
   ColorSelect                : zh_gt_def_ColorSelect                   ,
   GetColor                   : zh_gt_def_GetColor                      ,
   ColorNum                   : zh_gt_def_ColorNum                      ,
   ColorsToString             : zh_gt_def_ColorsToString                ,
   StringToColors             : zh_gt_def_StringToColors                ,
   GetColorData               : zh_gt_def_GetColorData                  ,
   GetClearColor              : zh_gt_def_GetClearColor                 ,
   SetClearColor              : zh_gt_def_SetClearColor                 ,
   GetClearChar               : zh_gt_def_GetClearChar                  ,
   SetClearChar               : zh_gt_def_SetClearChar                  ,
   GetCursorStyle             : zh_gt_def_GetCursorStyle                ,
   SetCursorStyle             : zh_gt_def_SetCursorStyle                ,
   GetScrCursor               : zh_gt_def_GetScrCursor                  ,
   GetScrChar                 : zh_gt_def_GetChar                       ,
   PutScrChar                 : zh_gt_def_PutChar                       ,
   GetScrUC                   : zh_gt_def_GetUC                         ,
   DispBegin                  : zh_gt_def_DispBegin                     ,
   DispEnd                    : zh_gt_def_DispEnd                       ,
   DispCount                  : zh_gt_def_DispCount                     ,
   GetChar                    : zh_gt_def_GetChar                       ,
   PutChar                    : zh_gt_def_PutChar                       ,
   RectSize                   : zh_gt_def_RectSize                      ,
   Save                       : zh_gt_def_Save                          ,
   Rest                       : zh_gt_def_Rest                          ,
   PutText                    : zh_gt_def_PutText                       ,
   PutTextW                   : zh_gt_def_PutTextW                      ,
   Replicate                  : zh_gt_def_Replicate                     ,
   WriteAt                    : zh_gt_def_WriteAt                       ,
   WriteAtW                   : zh_gt_def_WriteAtW                      ,
   Write                      : zh_gt_def_Write                         ,
   WriteW                     : zh_gt_def_WriteW                        ,
   WriteCon                   : zh_gt_def_WriteCon                      ,
   WriteConW                  : zh_gt_def_WriteConW                     ,
   SetAttribute               : zh_gt_def_SetAttribute                  ,
   DrawShadow                 : zh_gt_def_DrawShadow                    ,
   Scroll                     : zh_gt_def_Scroll                        ,
   ScrollUp                   : zh_gt_def_ScrollUp                      ,
   Box                        : zh_gt_def_Box                           ,
   BoxW                       : zh_gt_def_BoxW                          ,
   BoxD                       : zh_gt_def_BoxD                          ,
   BoxS                       : zh_gt_def_BoxS                          ,
   HorizLine                  : zh_gt_def_HorizLine                     ,
   VertLine                   : zh_gt_def_VertLine                      ,
   GetBlink                   : zh_gt_def_GetBlink                      ,
   SetBlink                   : zh_gt_def_SetBlink                      ,
   SetSnowFlag                : zh_gt_def_SetSnowFlag                   ,
   Version                    : zh_gt_def_Version                       ,
   Suspend                    : zh_gt_def_Suspend                       ,
   Resume                     : zh_gt_def_Resume                        ,
   PreExt                     : zh_gt_def_PreExt                        ,
   PostExt                    : zh_gt_def_PostExt                       ,
   OutStd                     : zh_gt_def_OutStd                        ,
   OutErr                     : zh_gt_def_OutErr                        ,
   Tone                       : zh_gt_def_Tone                          ,
   Bell                       : zh_gt_def_Bell                          ,
   Info                       : zh_gt_def_Info                          ,
   Alert                      : zh_gt_def_Alert                         ,
   SetFlag                    : zh_gt_def_SetFlag                       ,
   SetDispCP                  : zh_gt_def_SetDispCP                     ,
   SetKeyCP                   : zh_gt_def_SetKeyCP                      ,
   ReadKey                    : zh_gt_def_ReadKey                       ,
   InkeyGet                   : zh_gt_def_InkeyGet                      ,
   InkeyPut                   : zh_gt_def_InkeyPut                      ,
   InkeyIns                   : zh_gt_def_InkeyIns                      ,
   InkeyLast                  : zh_gt_def_InkeyLast                     ,
   InkeyNext                  : zh_gt_def_InkeyNext                     ,
   InkeyPoll                  : zh_gt_def_InkeyPoll                     ,
   InkeySetText               : zh_gt_def_InkeySetText                  ,
   InkeySetLast               : zh_gt_def_InkeySetLast                  ,
   InkeyReset                 : zh_gt_def_InkeyReset                    ,
   InkeyExit                  : zh_gt_def_InkeyExit                     ,
   MouseInit                  : zh_gt_def_MouseInit                     ,
   MouseExit                  : zh_gt_def_MouseExit                     ,
   MouseIsPresent             : zh_gt_def_MouseIsPresent                ,
   MouseShow                  : zh_gt_def_MouseShow                     ,
   MouseHide                  : zh_gt_def_MouseHide                     ,
   MouseGetCursor             : zh_gt_def_MouseGetCursor                ,
   MouseSetCursor             : zh_gt_def_MouseSetCursor                ,
   MouseCol                   : zh_gt_def_MouseCol                      ,
   MouseRow                   : zh_gt_def_MouseRow                      ,
   MouseGetPos                : zh_gt_def_MouseGetPos                   ,
   MouseSetPos                : zh_gt_def_MouseSetPos                   ,
   MouseSetBounds             : zh_gt_def_MouseSetBounds                ,
   MouseGetBounds             : zh_gt_def_MouseGetBounds                ,
   MouseStorageSize           : zh_gt_def_mouseStorageSize              ,
   MouseSaveState             : zh_gt_def_mouseSaveState                ,
   MouseRestoreState          : zh_gt_def_mouseRestoreState             ,
   MouseGetDoubleClickSpeed   : zh_gt_def_mouseGetDoubleClickSpeed      ,
   MouseSetDoubleClickSpeed   : zh_gt_def_mouseSetDoubleClickSpeed      ,
   MouseCountButton           : zh_gt_def_MouseCountButton              ,
   MouseButtonState           : zh_gt_def_MouseButtonState              ,
   MouseButtonPressed         : zh_gt_def_MouseButtonPressed            ,
   MouseButtonReleased        : zh_gt_def_MouseButtonReleased           ,
   MouseReadKey               : zh_gt_def_MouseReadKey                  ,
   GfxPrimitive               : zh_gt_def_GfxPrimitive                  ,
   GfxText                    : zh_gt_def_GfxText                       ,
   WhoCares                   : zh_gt_def_WhoCares
};
#else
static const ZH_GT_FUNCS s_gtCoreFunc =
{
   zh_gt_def_Lock                         ,
   zh_gt_def_Unlock                       ,
   zh_gt_def_Init                         ,
   zh_gt_def_Exit                         ,
   zh_gt_def_New                          ,
   zh_gt_def_Free                         ,
   zh_gt_def_Mark                         ,
   zh_gt_def_Resize                       ,
   zh_gt_def_SetMode                      ,
   zh_gt_def_GetSize                      ,
   zh_gt_def_SemiCold                     ,
   zh_gt_def_ColdArea                     ,
   zh_gt_def_ExposeArea                   ,
   zh_gt_def_ScrollArea                   ,
   zh_gt_def_TouchLine                    ,
   zh_gt_def_TouchCell                    ,
   zh_gt_def_Redraw                       ,
   zh_gt_def_RedrawDiff                   ,
   zh_gt_def_Refresh                      ,
   zh_gt_def_Flush                        ,
   zh_gt_def_MaxCol                       ,
   zh_gt_def_MaxRow                       ,
   zh_gt_def_CheckPos                     ,
   zh_gt_def_SetPos                       ,
   zh_gt_def_GetPos                       ,
   zh_gt_def_IsColor                      ,
   zh_gt_def_GetColorStr                  ,
   zh_gt_def_SetColorStr                  ,
   zh_gt_def_ColorSelect                  ,
   zh_gt_def_GetColor                     ,
   zh_gt_def_ColorNum                     ,
   zh_gt_def_ColorsToString               ,
   zh_gt_def_StringToColors               ,
   zh_gt_def_GetColorData                 ,
   zh_gt_def_GetClearColor                ,
   zh_gt_def_SetClearColor                ,
   zh_gt_def_GetClearChar                 ,
   zh_gt_def_SetClearChar                 ,
   zh_gt_def_GetCursorStyle               ,
   zh_gt_def_SetCursorStyle               ,
   zh_gt_def_GetScrCursor                 ,
   zh_gt_def_GetChar                      , /* intentionally mapped to GetScrChar */
   zh_gt_def_PutChar                      , /* intentionally mapped to PutScrChar */
   zh_gt_def_GetUC                        , /* intentionally mapped to GetScrUC */
   zh_gt_def_DispBegin                    ,
   zh_gt_def_DispEnd                      ,
   zh_gt_def_DispCount                    ,
   zh_gt_def_GetChar                      ,
   zh_gt_def_PutChar                      ,
   zh_gt_def_RectSize                     ,
   zh_gt_def_Save                         ,
   zh_gt_def_Rest                         ,
   zh_gt_def_PutText                      ,
   zh_gt_def_PutTextW                     ,
   zh_gt_def_Replicate                    ,
   zh_gt_def_WriteAt                      ,
   zh_gt_def_WriteAtW                     ,
   zh_gt_def_Write                        ,
   zh_gt_def_WriteW                       ,
   zh_gt_def_WriteCon                     ,
   zh_gt_def_WriteConW                    ,
   zh_gt_def_SetAttribute                 ,
   zh_gt_def_DrawShadow                   ,
   zh_gt_def_Scroll                       ,
   zh_gt_def_ScrollUp                     ,
   zh_gt_def_Box                          ,
   zh_gt_def_BoxW                         ,
   zh_gt_def_BoxD                         ,
   zh_gt_def_BoxS                         ,
   zh_gt_def_HorizLine                    ,
   zh_gt_def_VertLine                     ,
   zh_gt_def_GetBlink                     ,
   zh_gt_def_SetBlink                     ,
   zh_gt_def_SetSnowFlag                  ,
   zh_gt_def_Version                      ,
   zh_gt_def_Suspend                      ,
   zh_gt_def_Resume                       ,
   zh_gt_def_PreExt                       ,
   zh_gt_def_PostExt                      ,
   zh_gt_def_OutStd                       ,
   zh_gt_def_OutErr                       ,
   zh_gt_def_Tone                         ,
   zh_gt_def_Bell                         ,
   zh_gt_def_Info                         ,
   zh_gt_def_Alert                        ,
   zh_gt_def_SetFlag                      ,
   zh_gt_def_SetDispCP                    ,
   zh_gt_def_SetKeyCP                     ,
   zh_gt_def_ReadKey                      ,
   zh_gt_def_InkeyGet                     ,
   zh_gt_def_InkeyPut                     ,
   zh_gt_def_InkeyIns                     ,
   zh_gt_def_InkeyLast                    ,
   zh_gt_def_InkeyNext                    ,
   zh_gt_def_InkeyPoll                    ,
   zh_gt_def_InkeySetText                 ,
   zh_gt_def_InkeySetLast                 ,
   zh_gt_def_InkeyReset                   ,
   zh_gt_def_InkeyExit                    ,
   zh_gt_def_MouseInit                    ,
   zh_gt_def_MouseExit                    ,
   zh_gt_def_MouseIsPresent               ,
   zh_gt_def_MouseShow                    ,
   zh_gt_def_MouseHide                    ,
   zh_gt_def_MouseGetCursor               ,
   zh_gt_def_MouseSetCursor               ,
   zh_gt_def_MouseCol                     ,
   zh_gt_def_MouseRow                     ,
   zh_gt_def_MouseGetPos                  ,
   zh_gt_def_MouseSetPos                  ,
   zh_gt_def_MouseSetBounds               ,
   zh_gt_def_MouseGetBounds               ,
   zh_gt_def_mouseStorageSize             ,
   zh_gt_def_mouseSaveState               ,
   zh_gt_def_mouseRestoreState            ,
   zh_gt_def_mouseGetDoubleClickSpeed     ,
   zh_gt_def_mouseSetDoubleClickSpeed     ,
   zh_gt_def_MouseCountButton             ,
   zh_gt_def_MouseButtonState             ,
   zh_gt_def_MouseButtonPressed           ,
   zh_gt_def_MouseButtonReleased          ,
   zh_gt_def_MouseReadKey                 ,
   zh_gt_def_GfxPrimitive                 ,
   zh_gt_def_GfxText                      ,
   zh_gt_def_WhoCares
};
#endif

/* - */

static char s_gtNameBuf[ ZH_GT_NAME_MAX_ + 1 ];

/* NOTE: Must be in sync with gtsys.c */
#if defined( ZH_GT_LIB )
   static const char * s_szNameDefault = ZH_GT_DRVNAME( ZH_GT_LIB );
#elif defined( ZH_OS_WIN )
   static const char * s_szNameDefault = "win";
#elif defined( ZH_OS_DOS )
   static const char * s_szNameDefault = "dos";
#elif defined( ZH_OS_OS2 )
   static const char * s_szNameDefault = "os2";
#elif defined( ZH_OS_VXWORKS )
   static const char * s_szNameDefault = "std";
#elif defined( ZH_OS_UNIX )
   static const char * s_szNameDefault = "trm";
#else
   static const char * s_szNameDefault = "std";
#endif

static const ZH_GT_INIT * s_gtInit[ ZH_GT_MAX_ ];
static int s_iGtCount = 0;

ZH_FUNC_EXTERN( ZH_GTSYS );

static const char * zh_gt_FindDefault( void )
{
   char szFuncName[ 15 + ZH_GT_NAME_MAX_ ];
   int iPos;

   for( iPos = 0; iPos < s_iGtCount; iPos++ )
   {
      zh_snprintf( szFuncName, sizeof( szFuncName ),
                   "ZH_GT_%s_DEFAULT", s_gtInit[ iPos ]->id );
      if( zh_dynsymFind( szFuncName ) )
         return s_gtInit[ iPos ]->id;
   }

   if( zh_dynsymFind( "ZH_GT_NUL_DEFAULT" ) )
      return "NUL";
   else
      return NULL;
}

static int zh_gt_FindEntry( const char * pszID )
{
   ZH_BOOL fGt = zh_strnicmp( pszID, "gt", 2 ) == 0;
   int iPos;

   for( iPos = -1; iPos < s_iGtCount; iPos++ )
   {
      const char * id = iPos < 0 ? "NUL" : s_gtInit[ iPos ]->id;

      if( zh_stricmp( pszID, id ) == 0 ||
          ( fGt && zh_stricmp( pszID + 2, id ) == 0 ) )
         return iPos;
   }

   return zh_stricmp( pszID + ( fGt ? 2 : 0 ), "null" ) == 0 ? -1 : -2;
}

void zh_gtSetDefault( const char * szGtName )
{
   zh_strncpy( s_gtNameBuf, szGtName, sizeof( s_gtNameBuf ) - 1 );
   s_szNameDefault = s_gtNameBuf;
}

ZH_BOOL zh_gtRegister( const ZH_GT_INIT * gtInit )
{
   if( s_iGtCount < ZH_GT_MAX_ && zh_gt_FindEntry( gtInit->id ) < -1 )
   {
      if( gtInit->pGtId )
         *gtInit->pGtId = s_iGtCount;
      s_gtInit[ s_iGtCount++ ] = gtInit;
      return ZH_TRUE;
   }
   return ZH_FALSE;
}

PZH_GT zh_gtLoad( const char * szGtName, PZH_GT pGT, PZH_GT_FUNCS pSuperTable )
{
   if( szGtName )
   {
      int iPos = zh_gt_FindEntry( szGtName );

      if( iPos == -1 )
      {
         if( pGT || pSuperTable )
            zh_errInternal( 9996, "Ziher terminal (GT) initialization failure", NULL, NULL );

         pGT = ( PZH_GT_BASE ) zh_xgrabz( sizeof( ZH_GT_BASE ) );
         pGT->pFuncTable = ( PZH_GT_FUNCS ) zh_xgrab( sizeof( ZH_GT_FUNCS ) );
         memcpy( pGT->pFuncTable, &s_gtCoreFunc, sizeof( ZH_GT_FUNCS ) );
         pGT->iUsed++;
         return pGT;
      }
      else if( iPos >= 0 )
      {
         ZH_BOOL fNew = pGT == NULL;

         if( fNew )
         {
            pGT = ( PZH_GT_BASE ) zh_xgrabz( sizeof( ZH_GT_BASE ) );
            pGT->pFuncTable = ( PZH_GT_FUNCS ) zh_xgrab( sizeof( ZH_GT_FUNCS ) );
            memcpy( pGT->pFuncTable, &s_gtCoreFunc, sizeof( ZH_GT_FUNCS ) );
            pGT->iUsed++;
         }

         if( pSuperTable == NULL )
            pSuperTable = s_gtInit[ iPos ]->pSuperTable;
         if( pSuperTable != NULL )
            memcpy( pSuperTable, pGT->pFuncTable, sizeof( ZH_GT_FUNCS ) );

         if( s_gtInit[ iPos ]->init( pGT->pFuncTable ) )
            return pGT;
         else if( fNew )
         {
            zh_xfree( pGT->pFuncTable );
            zh_xfree( pGT );
         }
      }
   }
   return NULL;
}

void zh_gtIsGtRef( void * hGT )
{
   PZH_GT pGT = ( PZH_GT ) hGT;

   if( pGT )
      ZH_GTSELF_MARK( pGT );
}

void * zh_gtAlloc( void * hGT )
{
   PZH_GT pGT;

   if( hGT )
   {
      pGT = ( PZH_GT ) hGT;
      if( ! ZH_GTSELF_LOCK( pGT ) )
         pGT = NULL;
   }
   else
      pGT = zh_gt_Base();

   if( pGT )
   {
      pGT->iUsed++;
      zh_gt_BaseFree( pGT );
   }

   return ( void * ) pGT;
}

void zh_gtRelease( void * hGT )
{
   PZH_GT pGT;

   if( hGT )
   {
      pGT = ( PZH_GT ) hGT;
      if( ! ZH_GTSELF_LOCK( pGT ) )
         pGT = NULL;
   }
   else
      pGT = zh_gt_Base();

   if( pGT )
   {
      if( --pGT->iUsed == 0 )
      {
         while( ZH_GTSELF_DISPCOUNT( pGT ) )
            ZH_GTSELF_DISPEND( pGT );
         ZH_GTSELF_FLUSH( pGT );
         ZH_GTSELF_EXIT( pGT );
      }
      else
         zh_gt_BaseFree( pGT );
   }
}

void zh_gtAttach( void * hGT )
{
   if( hGT && hGT != zh_stackGetGT() )
   {
      zh_gtRelease( NULL );
      zh_stackSetGT( hGT );
   }
}

void * zh_gtSwap( void * hGT )
{
   void * hCurrGT = zh_stackGetGT();

   zh_stackSetGT( hGT );

   return hCurrGT;
}

ZH_BOOL zh_gtReload( const char * szGtName,
                     ZH_FHANDLE hFilenoStdin,
                     ZH_FHANDLE hFilenoStdout,
                     ZH_FHANDLE hFilenoStderr )
{
   ZH_BOOL fResult = ZH_FALSE;

   if( szGtName && zh_gt_FindEntry( szGtName ) >= -1 )
   {
      zh_gtRelease( NULL );
      zh_stackSetGT( zh_gtLoad( szGtName, NULL, NULL ) );
      fResult = zh_stackGetGT() != NULL;
      zh_gtInit( hFilenoStdin, hFilenoStdout, hFilenoStderr );
   }
   return fResult;
}

void * zh_gtCreate( const char * szGtName,
                    ZH_FHANDLE hFilenoStdin,
                    ZH_FHANDLE hFilenoStdout,
                    ZH_FHANDLE hFilenoStderr )
{
   void * hCurrGT = zh_gtSwap( NULL );

   if( szGtName && zh_gt_FindEntry( szGtName ) >= -1 )
   {
      PZH_GT pGT = zh_gtLoad( szGtName, NULL, NULL );
      if( pGT )
      {
         zh_stackSetGT( pGT );
         zh_gtInit( hFilenoStdin, hFilenoStdout, hFilenoStderr );
      }
   }
   return zh_gtSwap( hCurrGT );
}

static ZH_BOOL zh_gtTryInit( const char * szGtName, ZH_BOOL fFree )
{
   if( szGtName )
   {
      if( zh_stackGetGT() == NULL )
      {
         if( fFree )
         {
            char * pszStr = ( char * ) strchr( szGtName, ':' );
            if( pszStr != NULL )
               * pszStr = '\0';
         }

         zh_stackSetGT( zh_gtLoad( szGtName, NULL, NULL ) );
      }

      if( fFree )
         zh_xfree( ZH_UNCONST( szGtName ) );
   }

   return zh_stackGetGT() != NULL;
}

void zh_gtStartupInit( void )
{
   if( zh_gtTryInit( zh_cmdargString( "GT" ), ZH_TRUE ) )
      return;
   if( zh_gtTryInit( zh_getenv( "ZH_GT" ), ZH_TRUE ) )
      return;
   if( zh_gtTryInit( zh_gt_FindDefault(), ZH_FALSE ) )
      return;
   if( zh_gtTryInit( s_szNameDefault, ZH_FALSE ) )
      return;

   if( zh_dynsymFind( "ZH_GT_NUL" ) ) /* GTNUL was explicitly REQUESTed */
   {
      if( zh_gtTryInit( "NUL", ZH_FALSE ) )
         return;
   }

   zh_errInternal( 9998, "Ziher terminal (GT) initialization failure", NULL, NULL );

   /* not executed, only to force linking zh_GTSYS() */
   ZH_FUNC_EXEC( ZH_GTSYS );
}

ZH_GT_ANNOUNCE( ZH_GT_NAME )

static ZH_GARBAGE_FUNC( zh_gt_Destructor )
{
   void ** gtHolder = ( void ** ) Cargo;

   if( *gtHolder )
   {
      zh_gtRelease( *gtHolder );
      *gtHolder = NULL;
   }
}

static ZH_GARBAGE_FUNC( zh_gt_Mark )
{
   void ** gtHolder = ( void ** ) Cargo;

   if( *gtHolder )
      ZH_GTSELF_MARK( ( PZH_GT ) *gtHolder );
}

static const ZH_GC_FUNCS s_gcGTFuncs =
{
   zh_gt_Destructor,
   zh_gt_Mark
};

static void * zh_gtParam( int iParam )
{
   void ** gtHolder = ( void ** ) zh_parptrGC( &s_gcGTFuncs, iParam );

   if( gtHolder && *gtHolder )
      return *gtHolder;

   zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
   return NULL;
}

PZH_GT zh_gt_ItemBase( PZH_ITEM pItemGT )
{
   void ** gtHolder = ( void ** ) zh_itemGetPtrGC( pItemGT, &s_gcGTFuncs );

   if( gtHolder && *gtHolder )
   {
      PZH_GT pGT = ( PZH_GT ) *gtHolder;
      if( ZH_GTSELF_LOCK( pGT ) )
         return pGT;
   }
   return NULL;
}

ZH_FUNC( ZH_GTRELOAD )
{
   zh_retl( zh_gtReload( zh_parc( 1 ),
            ZH_IS_PARAM_NUM( 2 ) ? zh_numToHandle( zh_parnint( 1 ) ) : ZH_STDIN_HANDLE,
            ZH_IS_PARAM_NUM( 3 ) ? zh_numToHandle( zh_parnint( 2 ) ) : ZH_STDOUT_HANDLE,
            ZH_IS_PARAM_NUM( 4 ) ? zh_numToHandle( zh_parnint( 3 ) ) : ZH_STDERR_HANDLE ) );
}

ZH_FUNC( ZH_GTCREATE )
{
   void * hGT;

   hGT = zh_gtCreate( zh_parc( 1 ),
            ZH_IS_PARAM_NUM( 2 ) ? zh_numToHandle( zh_parnint( 1 ) ) : ZH_STDIN_HANDLE,
            ZH_IS_PARAM_NUM( 3 ) ? zh_numToHandle( zh_parnint( 2 ) ) : ZH_STDOUT_HANDLE,
            ZH_IS_PARAM_NUM( 4 ) ? zh_numToHandle( zh_parnint( 3 ) ) : ZH_STDERR_HANDLE );

   if( hGT )
   {
      void ** gtHolder = ( void ** ) zh_gcAllocate( sizeof( void * ), &s_gcGTFuncs );
      *gtHolder = hGT;
      zh_retptrGC( gtHolder );
   }
}

ZH_FUNC( ZH_GTSELECT )
{
   void * hGT;

   if( zh_pcount() > 0 )
   {
      hGT = zh_gtParam( 1 );
      if( hGT )
      {
         hGT = zh_gtAlloc( hGT );
         if( hGT )
            hGT = zh_gtSwap( hGT );
      }
   }
   else
      hGT = zh_gtAlloc( NULL );

   if( hGT )
   {
      void ** gtHolder = ( void ** ) zh_gcAllocate( sizeof( void * ), &s_gcGTFuncs );
      *gtHolder = hGT;
      zh_retptrGC( gtHolder );
   }
}
