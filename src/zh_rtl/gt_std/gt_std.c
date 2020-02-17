/*
 * Video subsystem for plain ANSI C stream IO
 *
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

/* NOTE: User programs should never call this layer directly! */

#define ZH_GT_NAME  STD

#include "zh_api.h"
#include "zh_gt_core.h"
#include "zh_init.h"
#include "zh_apifs.h"
#include "zh_codepage_api.h"
#include "zh_item_api.h"
#include "zh_date.h"
#include "zh_io.h"

#if defined( ZH_OS_UNIX )
#  if ! defined( ZH_HAS_TERMIOS )
#     define ZH_HAS_TERMIOS
#  endif
#endif

#if defined( ZH_OS_UNIX )
#  if defined( ZH_HAS_TERMIOS )
#     include <unistd.h>
#     include <termios.h>
#     include <sys/ioctl.h>
#     include <signal.h>
#     include <errno.h>
#     include <sys/time.h>
#     include <sys/types.h>
#     include <sys/wait.h>
#  endif
#else
#  if defined( ZH_OS_WIN )
#     include <windows.h>
#  endif
#  if defined( _MSC_VER )
#     include <conio.h>
#  endif
#endif

static int s_GtId;
static ZH_GT_FUNCS SuperTable;
#define ZH_GTSUPER   ( &SuperTable )
#define ZH_GTID_PTR  ( &s_GtId )

#define ZH_GTSTD_GET( p )  ( ( PZH_GTSTD ) ZH_GTLOCAL( p ) )

static const char s_szBell[] = { ZH_CHAR_BEL, 0 };

typedef struct _ZH_GTSTD
{
   ZH_FHANDLE     hStdin;
   ZH_FHANDLE     hStdout;
   ZH_FHANDLE     hStderr;
   ZH_BOOL        fStdinConsole;
   ZH_BOOL        fStdoutConsole;
   ZH_BOOL        fStderrConsole;

   int            iRow;
   int            iCol;
   int            iLastCol;

   int            iWidth;
   int            iLineBufSize;
   char *         sLineBuf;
   ZH_SIZE        nTransBufSize;
   char *         sTransBuf;
   ZH_BOOL        fFullRedraw;
   char *         szCrLf;
   ZH_SIZE        nCrLf;

#if defined( ZH_HAS_TERMIOS )
   struct termios saved_TIO;
   struct termios curr_TIO;
   ZH_BOOL        fRestTTY;
#endif

   double dToneSeconds;

} ZH_GTSTD, * PZH_GTSTD;


#if defined( ZH_HAS_TERMIOS )

static volatile ZH_BOOL s_fRestTTY = ZH_FALSE;

#if defined( SIGTTOU )
static void sig_handler( int iSigNo )
{
   switch( iSigNo )
   {
#ifdef SIGCHLD
      case SIGCHLD:
      {
         int e = errno, stat;
         pid_t pid;
         while( ( pid = waitpid( -1, &stat, WNOHANG ) ) > 0 );
         errno = e;
         break;
      }
#endif
#ifdef SIGWINCH
      case SIGWINCH:
         #if 0
         s_WinSizeChangeFlag = ZH_TRUE;
         #endif
         break;
#endif
#ifdef SIGINT
      case SIGINT:
         #if 0
         s_InetrruptFlag = ZH_TRUE;
         #endif
         break;
#endif
#ifdef SIGQUIT
      case SIGQUIT:
         #if 0
         s_BreakFlag = ZH_TRUE;
         #endif
         break;
#endif
#ifdef SIGTSTP
      case SIGTSTP:
         #if 0
         s_DebugFlag = ZH_TRUE;
         #endif
         break;
#endif
#ifdef SIGTSTP
      case SIGTTOU:
         s_fRestTTY = ZH_FALSE;
         break;
#endif
   }
}
#endif

#endif

static void zh_gt_std_termOut( PZH_GTSTD pGTSTD, const char * szStr, ZH_SIZE nLen )
{
   zh_fsWriteLarge( pGTSTD->hStdout, szStr, nLen );
}

static void zh_gt_std_newLine( PZH_GTSTD pGTSTD )
{
   zh_gt_std_termOut( pGTSTD, pGTSTD->szCrLf, pGTSTD->nCrLf );
}


static void zh_gt_std_Init( PZH_GT pGT, ZH_FHANDLE hFilenoStdin, ZH_FHANDLE hFilenoStdout, ZH_FHANDLE hFilenoStderr )
{
   PZH_GTSTD pGTSTD;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_std_Init(%p,%p,%p,%p)", ( void * ) pGT, ( void * ) ( ZH_PTRUINT ) hFilenoStdin, ( void * ) ( ZH_PTRUINT ) hFilenoStdout, ( void * ) ( ZH_PTRUINT ) hFilenoStderr ) );

   ZH_GTLOCAL( pGT ) = pGTSTD = ( PZH_GTSTD ) zh_xgrabz( sizeof( ZH_GTSTD ) );

   pGTSTD->hStdin  = hFilenoStdin;
   pGTSTD->hStdout = hFilenoStdout;
   pGTSTD->hStderr = hFilenoStderr;

   pGTSTD->fStdinConsole  = zh_fsIsDevice( pGTSTD->hStdin );
   pGTSTD->fStdoutConsole = zh_fsIsDevice( pGTSTD->hStdout );
   pGTSTD->fStderrConsole = zh_fsIsDevice( pGTSTD->hStderr );

   pGTSTD->szCrLf = zh_strdup( zh_conNewLine() );
   pGTSTD->nCrLf = strlen( pGTSTD->szCrLf );

   zh_fsSetDevMode( pGTSTD->hStdout, FD_BINARY );
   ZH_GTSUPER_INIT( pGT, hFilenoStdin, hFilenoStdout, hFilenoStderr );

/* SA_NOCLDSTOP in #if is a hack to detect POSIX compatible environment */
#if defined( ZH_HAS_TERMIOS ) && \
    defined( SA_NOCLDSTOP )

   if( pGTSTD->fStdinConsole )
   {
#if defined( SIGTTOU )
      struct sigaction act, old;

      /* if( pGTSTD->saved_TIO.c_lflag & TOSTOP ) != 0 */
      sigaction( SIGTTOU, NULL, &old );
      memcpy( &act, &old, sizeof( struct sigaction ) );
      act.sa_handler = sig_handler;
      /* do not use SA_RESTART - new Linux kernels will repeat the operation */
#if defined( SA_ONESHOT )
      act.sa_flags = SA_ONESHOT;
#elif defined( SA_RESETHAND )
      act.sa_flags = SA_RESETHAND;
#else
      act.sa_flags = 0;
#endif
      sigaction( SIGTTOU, &act, 0 );
#endif

      s_fRestTTY = ZH_TRUE;

      tcgetattr( pGTSTD->hStdin, &pGTSTD->saved_TIO );
      memcpy( &pGTSTD->curr_TIO, &pGTSTD->saved_TIO, sizeof( struct termios ) );
      #if 0
      atexit( restore_input_mode );
      #endif
      pGTSTD->curr_TIO.c_lflag &= ~( ICANON | ECHO );
      pGTSTD->curr_TIO.c_iflag &= ~ICRNL;

      /* workaround for bug in some Linux kernels (i.e. 3.13.0-64-generic
         *buntu) in which select() unconditionally accepts stdin for
         reading if c_cc[ VMIN ] = 0 [druzus] */
      pGTSTD->curr_TIO.c_cc[ VMIN ] = 1;
      pGTSTD->curr_TIO.c_cc[ VTIME ] = 0;
      tcsetattr( pGTSTD->hStdin, TCSAFLUSH, &pGTSTD->curr_TIO );

#if defined( SIGTTOU )
      act.sa_handler = SIG_DFL;
      sigaction( SIGTTOU, &old, NULL );
#endif
      pGTSTD->fRestTTY = s_fRestTTY;
   }

#ifdef TIOCGWINSZ
   if( pGTSTD->fStdoutConsole )
   {
      struct winsize win;

      if( ioctl( pGTSTD->hStdout, TIOCGWINSZ, ( char * ) &win ) != -1 )
      {
         ZH_GTSELF_RESIZE( pGT, win.ws_row, win.ws_col );
      }
   }
#endif
#elif defined( ZH_OS_WIN )
   if( pGTSTD->fStdinConsole )
   {
      SetConsoleMode( ( HANDLE ) zh_fsGetOsHandle( pGTSTD->hStdin ), 0x0000 );
   }
#endif
   ZH_GTSELF_SETFLAG( pGT, ZH_GTI_STDOUTCON, pGTSTD->fStdoutConsole );
   ZH_GTSELF_SETFLAG( pGT, ZH_GTI_STDERRCON, pGTSTD->fStderrConsole &&
                                             pGTSTD->fStdoutConsole );
}

static void zh_gt_std_Exit( PZH_GT pGT )
{
   PZH_GTSTD pGTSTD;
   int iRow, iCol;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_std_Exit(%p)", ( void * ) pGT ) );

   ZH_GTSELF_REFRESH( pGT );
   ZH_GTSELF_GETPOS( pGT, &iRow, &iCol );

   pGTSTD = ZH_GTSTD_GET( pGT );

   ZH_GTSUPER_EXIT( pGT );

   if( pGTSTD )
   {
      /* update cursor position on exit */
      if( pGTSTD->fStdoutConsole && pGTSTD->iLastCol > 0 )
      {
         zh_gt_std_newLine( pGTSTD );
         ++pGTSTD->iRow;
      }

      while( ++pGTSTD->iRow <= iRow )
         zh_gt_std_newLine( pGTSTD );

#if defined( ZH_HAS_TERMIOS )
      if( pGTSTD->fRestTTY )
         tcsetattr( pGTSTD->hStdin, TCSANOW, &pGTSTD->saved_TIO );
#endif
      if( pGTSTD->iLineBufSize > 0 )
         zh_xfree( pGTSTD->sLineBuf );
      if( pGTSTD->nTransBufSize > 0 )
         zh_xfree( pGTSTD->sTransBuf );
      if( pGTSTD->szCrLf )
         zh_xfree( pGTSTD->szCrLf );
      zh_xfree( pGTSTD );
   }
}

static int zh_gt_std_ReadKey( PZH_GT pGT, int iEventMask )
{
   PZH_GTSTD pGTSTD;
   int ch = 0;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_std_ReadKey(%p,%d)", ( void * ) pGT, iEventMask ) );

   ZH_SYMBOL_UNUSED( iEventMask );

   pGTSTD = ZH_GTSTD_GET( pGT );

#if defined( ZH_HAS_TERMIOS )
   if( zh_fsCanRead( pGTSTD->hStdin, 0 ) > 0 )
   {
      ZH_BYTE bChar;
      if( zh_fsRead( pGTSTD->hStdin, &bChar, 1 ) == 1 )
         ch = bChar;
   }
#elif defined( _MSC_VER )
   if( pGTSTD->fStdinConsole )
   {
      if( _kbhit() )
      {
         ch = _getch();
         if( ( ch == 0 || ch == 224 ) && _kbhit() )
         {
            /* It was a function key lead-in code, so read the actual
               function key and then offset it by 256 */
            ch = _getch();
            if( ch != -1 )
               ch += 256;
         }
         ch = zh_gt_dos_keyCodeTranslate( ch, 0, ZH_GTSELF_CPIN( pGT ) );
      }
   }
   else if( ! _eof( ( int ) pGTSTD->hStdin ) )
   {
      ZH_BYTE bChar;
      if( _read( ( int ) pGTSTD->hStdin, &bChar, 1 ) == 1 )
         ch = bChar;
   }
#elif defined( ZH_OS_WIN )
   if( ! pGTSTD->fStdinConsole )
   {
      ZH_BYTE bChar;
      if( zh_fsRead( pGTSTD->hStdin, &bChar, 1 ) == 1 )
         ch = bChar;
   }
   else if( WaitForSingleObject( ( HANDLE ) zh_fsGetOsHandle( pGTSTD->hStdin ), 0 ) == WAIT_OBJECT_0 )
   {

      INPUT_RECORD  ir;
      DWORD         dwEvents;
      while( PeekConsoleInput( ( HANDLE ) zh_fsGetOsHandle( pGTSTD->hStdin ), &ir, 1, &dwEvents ) && dwEvents == 1 )
      {
         if( ir.EventType == KEY_EVENT && ir.Event.KeyEvent.bKeyDown )
         {
            ZH_BYTE bChar;
            if( zh_fsRead( pGTSTD->hStdin, &bChar, 1 ) == 1 )
               ch = bChar;
         }
         else /* Remove from the input queue */
            ReadConsoleInput( ( HANDLE ) zh_fsGetOsHandle( pGTSTD->hStdin ), &ir, 1, &dwEvents );
      }
   }
#else
   {
      if( ! pGTSTD->fStdinConsole )
      {
         ZH_BYTE bChar;
         if( zh_fsRead( pGTSTD->hStdin, &bChar, 1 ) == 1 )
            ch = bChar;
      }
      else
      {
         int iTODO; /* TODO: */
      }
   }
#endif

   if( ch )
   {
      int u = ZH_GTSELF_KEYTRANS( pGT, ch );
      if( u )
         ch = ZH_INKEY_NEW_UNICODE( u );
   }

   return ch;
}

static ZH_BOOL zh_gt_std_IsColor( PZH_GT pGT )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_std_IsColor(%p)", ( void * ) pGT ) );

   ZH_SYMBOL_UNUSED( pGT );

   return ZH_FALSE;
}

static void zh_gt_std_Tone( PZH_GT pGT, double dFrequency, double dDuration )
{
   double dCurrentSeconds;
   PZH_GTSTD pGTSTD;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_std_Tone(%p,%lf,%lf)", ( void * ) pGT, dFrequency, dDuration ) );

   pGTSTD = ZH_GTSTD_GET( pGT );

   /* Output an ASCII BEL character to cause a sound */
   /* but throttle to max once per second, in case of sound */
   /* effects prgs calling lots of short tone sequences in */
   /* succession leading to BEL hell on the terminal */

   dCurrentSeconds = zh_dateSeconds();
   if( dCurrentSeconds < pGTSTD->dToneSeconds ||
       dCurrentSeconds - pGTSTD->dToneSeconds > 0.5 )
   {
      zh_gt_std_termOut( pGTSTD, s_szBell, 1 );
      pGTSTD->dToneSeconds = dCurrentSeconds;
   }

   ZH_SYMBOL_UNUSED( dFrequency );
   zh_gtSleep( pGT, dDuration / 18.2 );
}

static void zh_gt_std_Bell( PZH_GT pGT )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_std_Bell(%p)", ( void * ) pGT ) );

   zh_gt_std_termOut( ZH_GTSTD_GET( pGT ), s_szBell, 1 );
}

static const char * zh_gt_std_Version( PZH_GT pGT, int iType )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_std_Version(%p,%d)", ( void * ) pGT, iType ) );

   ZH_SYMBOL_UNUSED( pGT );

   if( iType == 0 )
      return ZH_GT_DRVNAME( ZH_GT_NAME );

   return "Terminal: Standard stream I/O";
}

static ZH_BOOL zh_gt_std_Suspend( PZH_GT pGT )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_std_Suspend(%p)", ( void * ) pGT ) );

#if defined( ZH_HAS_TERMIOS )
   {
      PZH_GTSTD pGTSTD = ZH_GTSTD_GET( pGT );
      if( pGTSTD->fRestTTY )
         tcsetattr( pGTSTD->hStdin, TCSANOW, &pGTSTD->saved_TIO );
   }
#endif

   return ZH_GTSUPER_SUSPEND( pGT );
}

static ZH_BOOL zh_gt_std_Resume( PZH_GT pGT )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_std_Resume(%p)", ( void * ) pGT ) );


#if defined( ZH_HAS_TERMIOS )
   {
      PZH_GTSTD pGTSTD = ZH_GTSTD_GET( pGT );
      if( pGTSTD->fRestTTY )
         tcsetattr( pGTSTD->hStdin, TCSANOW, &pGTSTD->curr_TIO );
   }
#endif
   return ZH_GTSUPER_RESUME( pGT );
}

static void zh_gt_std_Scroll( PZH_GT pGT, int iTop, int iLeft, int iBottom, int iRight,
                              int iColor, ZH_USHORT usChar, int iRows, int iCols )
{
   int iHeight, iWidth;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_std_Scroll(%p,%d,%d,%d,%d,%d,%d,%d,%d)", ( void * ) pGT, iTop, iLeft, iBottom, iRight, iColor, usChar, iRows, iCols ) );

   /* Provide some basic scroll support for full screen */
   ZH_GTSELF_GETSIZE( pGT, &iHeight, &iWidth );
   if( iCols == 0 && iRows > 0 &&
       iTop == 0 && iLeft == 0 &&
       iBottom >= iHeight - 1 && iRight >= iWidth - 1 )
   {
      PZH_GTSTD pGTSTD;
      /* scroll up the internal screen buffer */
      ZH_GTSELF_SCROLLUP( pGT, iRows, iColor, usChar );
      /* update our internal row position */
      pGTSTD = ZH_GTSTD_GET( pGT );
      pGTSTD->iRow -= iRows;
      if( pGTSTD->iRow < 0 )
         pGTSTD->iRow = 0;
   }
   else
      ZH_GTSUPER_SCROLL( pGT, iTop, iLeft, iBottom, iRight, iColor, usChar, iRows, iCols );
}

static void zh_gt_std_DispLine( PZH_GT pGT, int iRow, int iFrom, int iSize )
{
   int iColor;
   ZH_BYTE bAttr;
   ZH_USHORT usChar;
   int iCol, iLastCol, iAll;
   ZH_SIZE nLen, nI;
   PZH_CODEPAGE cdpTerm = ZH_GTSELF_TERMCP( pGT );
   PZH_GTSTD pGTSTD = ZH_GTSTD_GET( pGT );

   if( iSize < 0 )
   {
      zh_gt_std_newLine( pGTSTD );
      pGTSTD->iLastCol = iAll = 0;
      iSize = pGTSTD->iWidth;
   }
   else
      iAll = iSize;

   for( iCol = iLastCol = iFrom, nLen = nI = 0; iSize > 0; --iSize )
   {
      if( ! ZH_GTSELF_GETSCRCHAR( pGT, iRow, iCol++, &iColor, &bAttr, &usChar ) )
         break;

      if( usChar < 32 || usChar == 127 )
         usChar = '.';
      nI += zh_cdpTextPutU16( cdpTerm, pGTSTD->sLineBuf + nI,
                                       pGTSTD->iLineBufSize - nI, usChar );
      if( iAll || usChar != ' ' )
      {
         nLen = nI;
         iLastCol = iCol;
      }
   }
   if( nLen > 0 )
      zh_gt_std_termOut( pGTSTD, pGTSTD->sLineBuf, nLen );
   pGTSTD->iRow = iRow;
   pGTSTD->iCol = iLastCol;
   if( pGTSTD->iCol > pGTSTD->iLastCol )
      pGTSTD->iLastCol = pGTSTD->iCol;
}

static void zh_gt_std_Redraw( PZH_GT pGT, int iRow, int iCol, int iSize )
{
   int iColor;
   ZH_BYTE bAttr;
   ZH_USHORT usChar;
   int iLineFeed, iBackSpace, iMin;
   PZH_GTSTD pGTSTD;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_std_Redraw(%p,%d,%d,%d)", ( void * ) pGT, iRow, iCol, iSize ) );

   iLineFeed = iBackSpace = 0;
   pGTSTD = ZH_GTSTD_GET( pGT );

   if( pGTSTD->iRow != iRow )
   {
      iLineFeed = pGTSTD->iRow < iRow ? iRow - pGTSTD->iRow : 1;
      iCol = 0;
      iSize = pGTSTD->iWidth;
   }
   else if( pGTSTD->iCol < iCol )
   {
      iSize += iCol - pGTSTD->iCol;
      iCol = pGTSTD->iCol;
   }
   else if( pGTSTD->iCol > iCol )
   {
      if( pGTSTD->fStdoutConsole && pGTSTD->iCol <= pGTSTD->iWidth )
      {
         iBackSpace = pGTSTD->iCol - iCol;
         if( iBackSpace > iSize )
            iSize = iBackSpace;
      }
      else
      {
         iLineFeed = 1;
         iCol = 0;
         iSize = pGTSTD->iWidth;
      }
   }

   iMin = iLineFeed > 0 || pGTSTD->iLastCol <= iCol ? 0 : pGTSTD->iLastCol - iCol;

   while( iSize > iMin &&
          ZH_GTSELF_GETSCRCHAR( pGT, iRow, iCol + iSize - 1, &iColor, &bAttr, &usChar ) )
   {
      if( usChar != ' ' )
         break;
      --iSize;
   }

   if( iSize > 0 )
   {
      if( iLineFeed > 0 )
      {
         /*
          * If you want to disable full screen redrawing in console (TTY)
          * output then comment out the 'if' block below, Druzus
          */
         if( pGTSTD->fStdoutConsole )
         {
            int i;

            if( pGTSTD->iRow > iRow )
            {
               pGTSTD->iRow = -1;
               pGTSTD->fFullRedraw = ZH_TRUE;
            }
            for( i = pGTSTD->iRow + 1; i < iRow; ++i )
               zh_gt_std_DispLine( pGT, i, 0, -1 );
            iLineFeed = 1;
         }

         do
         {
            zh_gt_std_newLine( pGTSTD );
         }
         while( --iLineFeed );
         pGTSTD->iLastCol = 0;
      }
      else if( iBackSpace > 0 )
      {
         memset( pGTSTD->sLineBuf, ZH_CHAR_BS, iBackSpace );
         zh_gt_std_termOut( pGTSTD, pGTSTD->sLineBuf, iBackSpace );
      }

      zh_gt_std_DispLine( pGT, iRow, iCol, iSize );
   }
}

static void zh_gt_std_Refresh( PZH_GT pGT )
{
   int iHeight, iSize;
   PZH_GTSTD pGTSTD;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_std_Refresh(%p)", ( void * ) pGT ) );

   pGTSTD = ZH_GTSTD_GET( pGT );
   ZH_GTSELF_GETSIZE( pGT, &iHeight, &pGTSTD->iWidth );
   iSize = pGTSTD->iWidth * ZH_MAX_CHAR_LEN;

   if( pGTSTD->iLineBufSize != iSize )
   {
      pGTSTD->sLineBuf = ( char * ) zh_xrealloc( pGTSTD->sLineBuf, iSize );
      pGTSTD->iLineBufSize = iSize;
   }
   pGTSTD->fFullRedraw = ZH_FALSE;
   ZH_GTSUPER_REFRESH( pGT );
   if( pGTSTD->fFullRedraw )
   {
      if( pGTSTD->iRow < iHeight - 1 )
      {
         int i;

         for( i = pGTSTD->iRow + 1; i < iHeight; ++i )
            zh_gt_std_DispLine( pGT, i, 0, -1 );
      }
   }
}

static ZH_BOOL zh_gt_std_Info( PZH_GT pGT, int iType, PZH_GT_INFO pInfo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_std_Info(%p,%d,%p)", ( void * ) pGT, iType, ( void * ) pInfo ) );

   switch( iType )
   {
      case ZH_GTI_ISSCREENPOS:
      case ZH_GTI_KBDSUPPORT:
         pInfo->pResult = zh_itemPutL( pInfo->pResult, ZH_TRUE );
         break;

      default:
         return ZH_GTSUPER_INFO( pGT, iType, pInfo );
   }

   return ZH_TRUE;
}


static ZH_BOOL zh_gt_FuncInit( PZH_GT_FUNCS pFuncTable )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_FuncInit(%p)", ( void * ) pFuncTable ) );

   pFuncTable->Init                       = zh_gt_std_Init;
   pFuncTable->Exit                       = zh_gt_std_Exit;
   pFuncTable->IsColor                    = zh_gt_std_IsColor;
   pFuncTable->Redraw                     = zh_gt_std_Redraw;
   pFuncTable->Refresh                    = zh_gt_std_Refresh;
   pFuncTable->Scroll                     = zh_gt_std_Scroll;
   pFuncTable->Version                    = zh_gt_std_Version;
   pFuncTable->Suspend                    = zh_gt_std_Suspend;
   pFuncTable->Resume                     = zh_gt_std_Resume;
   pFuncTable->Tone                       = zh_gt_std_Tone;
   pFuncTable->Bell                       = zh_gt_std_Bell;
   pFuncTable->Info                       = zh_gt_std_Info;

   pFuncTable->ReadKey                    = zh_gt_std_ReadKey;

   return ZH_TRUE;
}

/* *********************************************************************** */

#include "zh_gt_reg.h"

/* *********************************************************************** */
