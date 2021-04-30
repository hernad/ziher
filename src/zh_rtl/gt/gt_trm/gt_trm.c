/*
 * Video subsystem - terminal GT driver
 *
 * Unlike GTSLN and GTCRS this GT driver does not use termcap/terminfo
 * for terminal escape sequences but uses hard coded ones so it
 * can be compiled in any system but supports only terminals which
 * exactly pass given capabilities. To reduce possible problems
 * intentionally only basic capabilities are used. It quite often gives
 * better results then the code using [n]Curses or SLang
 *
 * Now it support the following terminals:
 *   linux, pc-ansi, xterm
 *
 * I used my code from other GT drivers (GTCRS, GTPCA)
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus /at/ priv.onet.pl>
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

#define ZH_GT_NAME  TRM

#define ZH_GT_UNICODE_BUF

#include "zh_api.h"
#include "../zh_gt_core.h"
#include "zh_init.h"
#include "zh_codepage_api.h"
#include "zh_string_api.h"
#include "zh_item_api.h"
#include "zh_fs_api.h"
#include "zh_date.h"
#include "inkey.zhh"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>

#if defined( ZH_OS_UNIX )
# include <errno.h>
# include <time.h>
# include <unistd.h>
# include <signal.h>
# include <termios.h>
# include <sys/stat.h>
# include <sys/types.h>
# include <sys/time.h>
# include <sys/ioctl.h>
# include <sys/wait.h>
#endif
#if defined( ZH_HAS_GPM )
# include <gpm.h>
#  define KG_SHIFT      0
#  define KG_CTRL       2
#  define KG_ALT        3
#endif

#ifndef O_ACCMODE
#  define O_ACCMODE        ( O_RDONLY | O_WRONLY | O_RDWR )
#endif

static int s_GtId;
static ZH_GT_FUNCS SuperTable;
#define ZH_GTSUPER         ( &SuperTable )
#define ZH_GTID_PTR        ( &s_GtId )

#define ZH_GTTRM_ATTR_CHAR 0x00FF
#define ZH_GTTRM_ATTR_STD  0x0000

#define ZH_GTTRM_ATTR_ALT  0x0100
#define ZH_GTTRM_ATTR_PROT 0x0100
#define ZH_GTTRM_ATTR_ACSC 0x0100
#define ZH_GTTRM_ATTR_BOX  0x0800

#define TERM_ANSI          1
#define TERM_LINUX         2
#define TERM_XTERM         3
#define TERM_CONS          4
#define TERM_CYGWIN        5
#define TERM_PUTTY         16

#define ZH_GTTRM_CLRSTD    0
#define ZH_GTTRM_CLRX16    1
#define ZH_GTTRM_CLR256    2
#define ZH_GTTRM_CLRRGB    3
#define ZH_GTTRM_CLRAIX    4

#define STDIN_BUFLEN       128

#define ESC_DELAY          25

#define IS_EVTFDSTAT( x )  ( ( x ) >= 0x01 && ( x ) <= 0x03 )
#define EVTFDSTAT_RUN      0x01
#define EVTFDSTAT_STOP     0x02
#define EVTFDSTAT_DEL      0x03

/* mouse button states */
#define M_BUTTON_LEFT      0x0001
#define M_BUTTON_RIGHT     0x0002
#define M_BUTTON_MIDDLE    0x0004
#define M_BUTTON_LDBLCK    0x0010
#define M_BUTTON_RDBLCK    0x0020
#define M_BUTTON_MDBLCK    0x0040
#define M_BUTTON_WHEELUP   0x0100
#define M_BUTTON_WHEELDOWN 0x0200
#define M_CURSOR_MOVE      0x0400
#define M_BUTTON_KEYMASK   ( M_BUTTON_LEFT | M_BUTTON_RIGHT | M_BUTTON_MIDDLE )
#define M_BUTTON_DBLMASK   ( M_BUTTON_LDBLCK | M_BUTTON_RDBLCK | M_BUTTON_MDBLCK )

#define MOUSE_NONE         0
#define MOUSE_GPM          1
#define MOUSE_XTERM        2

#define KEY_SHIFTMASK      0x01000000
#define KEY_CTRLMASK       0x02000000
#define KEY_ALTMASK        0x04000000
#define KEY_KPADMASK       0x08000000
#define KEY_EXTDMASK       0x10000000
#define KEY_CLIPMASK       0x20000000
/* 0x40000000 reserved for Ziher extended keys */
#define KEY_MASK           0xFF000000

#define CLR_KEYMASK( x )   ( ( x ) & ~KEY_MASK )
#define GET_KEYMASK( x )   ( ( x ) & KEY_MASK )

#define IS_CLIPKEY( x )    ( ( ( ( x ) & ~0xffff ) ^ KEY_CLIPMASK ) == 0 )
#define SET_CLIPKEY( x )   ( ( ( x ) & 0xffff ) | KEY_CLIPMASK )
#define GET_CLIPKEY( x )   ( ( ( ( x ) & 0x8000 ) ? ~0xffff : 0 ) | ( ( x ) & 0xffff ) )

#define CTRL_SEQ           "\036"
#define ALT_SEQ            "\037"
/*#define NATION_SEQ         "\016"*/

#define EXKEY_F1           ( ZH_KX_F1     | KEY_EXTDMASK )
#define EXKEY_F2           ( ZH_KX_F2     | KEY_EXTDMASK )
#define EXKEY_F3           ( ZH_KX_F3     | KEY_EXTDMASK )
#define EXKEY_F4           ( ZH_KX_F4     | KEY_EXTDMASK )
#define EXKEY_F5           ( ZH_KX_F5     | KEY_EXTDMASK )
#define EXKEY_F6           ( ZH_KX_F6     | KEY_EXTDMASK )
#define EXKEY_F7           ( ZH_KX_F7     | KEY_EXTDMASK )
#define EXKEY_F8           ( ZH_KX_F8     | KEY_EXTDMASK )
#define EXKEY_F9           ( ZH_KX_F9     | KEY_EXTDMASK )
#define EXKEY_F10          ( ZH_KX_F10    | KEY_EXTDMASK )
#define EXKEY_F11          ( ZH_KX_F11    | KEY_EXTDMASK )
#define EXKEY_F12          ( ZH_KX_F12    | KEY_EXTDMASK )
#define EXKEY_UP           ( ZH_KX_UP     | KEY_EXTDMASK )
#define EXKEY_DOWN         ( ZH_KX_DOWN   | KEY_EXTDMASK )
#define EXKEY_LEFT         ( ZH_KX_LEFT   | KEY_EXTDMASK )
#define EXKEY_RIGHT        ( ZH_KX_RIGHT  | KEY_EXTDMASK )
#define EXKEY_DEL          ( ZH_KX_DEL    | KEY_EXTDMASK )
#define EXKEY_HOME         ( ZH_KX_HOME   | KEY_EXTDMASK )
#define EXKEY_END          ( ZH_KX_END    | KEY_EXTDMASK )
#define EXKEY_PGUP         ( ZH_KX_PGUP   | KEY_EXTDMASK )
#define EXKEY_PGDN         ( ZH_KX_PGDN   | KEY_EXTDMASK )
#define EXKEY_INS          ( ZH_KX_INS    | KEY_EXTDMASK )
#define EXKEY_BS           ( ZH_KX_BS     | KEY_EXTDMASK )
#define EXKEY_TAB          ( ZH_KX_TAB    | KEY_EXTDMASK )
#define EXKEY_ESC          ( ZH_KX_ESC    | KEY_EXTDMASK )
#define EXKEY_ENTER        ( ZH_KX_ENTER  | KEY_EXTDMASK )
#define EXKEY_CENTER       ( ZH_KX_CENTER | KEY_EXTDMASK )
#define EXKEY_PRTSCR       ( ZH_KX_PRTSCR | KEY_EXTDMASK )
#define EXKEY_PAUSE        ( ZH_KX_PAUSE  | KEY_EXTDMASK )

#define K_UNDEF            0x10000
#define K_METAALT          0x10001
#define K_METACTRL         0x10002
#define K_NATIONAL         0x10003
#define K_MOUSETERM        0x10004
#define K_RESIZE           0x10005

#if defined( ZH_OS_UNIX ) || defined( __DJGPP__ )

#define TIMEVAL_GET( tv )           gettimeofday( &( tv ), NULL )
#define TIMEVAL_LESS( tv1, tv2 )    ( ( ( tv1 ).tv_sec == ( tv2 ).tv_sec ) ? \
                                      ( ( tv1 ).tv_usec < ( tv2 ).tv_usec ) : \
                                      ( ( tv1 ).tv_sec < ( tv2 ).tv_sec ) )
#define TIMEVAL_ADD( dst, src, n )  \
   do { \
      ( dst ).tv_sec = ( src ).tv_sec + ( n ) / 1000; \
      if( ( ( dst ).tv_usec = ( src ).tv_usec + ( ( n ) % 1000 ) * 1000 ) >= 1000000 ) \
      { \
         ( dst ).tv_usec -= 1000000; ( dst ).tv_sec++; \
      } \
   } while( 0 )

#else

#define TIMEVAL_GET( tv )           do { ( tv ) = zh_dateSeconds(); } while( 0 )
#define TIMEVAL_LESS( tv1, tv2 )    ( ( tv1 ) < ( tv2 ) )
#define TIMEVAL_ADD( dst, src, n )  do { ( dst ) = ( src ) + n / 1000; } while( 0 )

#endif

typedef struct
{
   int    fd;
   int    mode;
   int    status;
   int    index;
   void * cargo;
   int ( * eventFunc )( int, int, void * );
} evtFD;

typedef struct
{
   int row, col;
   int buttonstate;
   int lbuttons;
   int flags;
   int lbup_row, lbup_col;
   int lbdn_row, lbdn_col;
   int rbup_row, rbup_col;
   int rbdn_row, rbdn_col;
   int mbup_row, mbup_col;
   int mbdn_row, mbdn_col;
   /* to analize DBLCLK on xterm */
#if defined( ZH_OS_UNIX ) || defined( __DJGPP__ )
   struct timeval BL_time;
   struct timeval BR_time;
   struct timeval BM_time;
#else
   double BL_time;
   double BR_time;
   double BM_time;
#endif
} mouseEvent;

typedef struct _keyTab
{
   int ch;
   int key;
   struct _keyTab * nextCh;
   struct _keyTab * otherCh;
} keyTab;

typedef struct
{
   int key;
   const char * seq;
} keySeq;

#define ZH_GTTRM_PTR  struct _ZH_GTTRM *
#define ZH_GTTRM_GET( p )  ( ( PZH_GTTRM ) ZH_GTLOCAL( p ) )

typedef struct _ZH_GTTRM
{
   PZH_GT     pGT;

   ZH_FHANDLE hFileno;
   ZH_FHANDLE hFilenoStdin;
   ZH_FHANDLE hFilenoStdout;
   ZH_FHANDLE hFilenoStderr;
   int        iRow;
   int        iCol;
   int        iWidth;
   int        iHeight;
   ZH_SIZE    nLineBufSize;
   char *     pLineBuf;
   int        iCurrentSGR, iFgColor, iBgColor, iBold, iBlink, iACSC, iExtColor, iAM;
   int        iAttrMask;
   int        iCursorStyle;
   ZH_BOOL    fAM;

   ZH_BOOL    fOutTTY;
   ZH_BOOL    fStdinTTY;
   ZH_BOOL    fStdoutTTY;
   ZH_BOOL    fStderrTTY;

   ZH_BOOL    fPosAnswer;

   ZH_BOOL    fUTF8;

#ifndef ZH_GT_UNICODE_BUF
   PZH_CODEPAGE cdpIn;
   PZH_CODEPAGE cdpHost;
   PZH_CODEPAGE cdpTerm;
   PZH_CODEPAGE cdpBox;

   ZH_UCHAR   keyTransTbl[ 256 ];
#endif

   int        charmap[ 256 ];

   int        chrattr[ 256 ];
   int        boxattr[ 256 ];

   int        colors[ 16 ];

   char *     szTitle;

   int        iOutBufSize;
   int        iOutBufIndex;
   char *     pOutBuf;

   int        terminal_type;
   int        terminal_ext;

#if defined( ZH_OS_UNIX )
   struct termios saved_TIO, curr_TIO;
   ZH_BOOL    fRestTTY;
#endif

   double     dToneSeconds;

   /* input events */
   keyTab * pKeyTab;
   int key_flag;
   int esc_delay;
   int key_counter;
   int nation_mode;

   int mouse_type;
   int mButtons;
   int nTermMouseChars;
   unsigned char cTermMouseBuf[ 3 ];
   mouseEvent mLastEvt;
#if defined( ZH_HAS_GPM )
   Gpm_Connect Conn;
#endif

   unsigned char stdin_buf[STDIN_BUFLEN];
   int stdin_ptr_l;
   int stdin_ptr_r;
   int stdin_inbuf;

   PZH_POLLFD pPollSet;
   evtFD ** event_fds;
   int efds_size;
   int efds_no;

   /* terminal functions */

   void     (* Init) ( ZH_GTTRM_PTR );
   void     (* Exit) ( ZH_GTTRM_PTR );
   void     (* SetTermMode) ( ZH_GTTRM_PTR, int );
   ZH_BOOL  (* GetCursorPos) ( ZH_GTTRM_PTR, int *, int *, const char * );
   void     (* SetCursorPos) ( ZH_GTTRM_PTR, int , int );
   void     (* SetCursorStyle) ( ZH_GTTRM_PTR, int );
   void     (* SetAttributes) ( ZH_GTTRM_PTR, int );
   ZH_BOOL  (* SetMode) ( ZH_GTTRM_PTR, int *, int * );
   int      (* GetAcsc) ( ZH_GTTRM_PTR, unsigned char );
   void     (* Tone) ( ZH_GTTRM_PTR, double, double );
   void     (* Bell) ( ZH_GTTRM_PTR );
   const char * szAcsc;
} ZH_TERM_STATE, ZH_GTTRM, * PZH_GTTRM;

/* static variables use by signal handler */
#if defined( ZH_OS_UNIX )
   static volatile ZH_BOOL s_WinSizeChangeFlag = ZH_FALSE;
#endif
#if defined( ZH_OS_UNIX ) && defined( SA_NOCLDSTOP )
   static volatile ZH_BOOL s_fRestTTY = ZH_FALSE;
#endif

/* save old hilit tracking & enable mouse tracking */
static const char * s_szMouseOn  = "\033[?1001s\033[?1002h";
/* disable mouse tracking & restore old hilit tracking */
static const char * s_szMouseOff = "\033[?1002l\033[?1001r";
static const char s_szBell[] = { ZH_CHAR_BEL, 0 };

/* conversion table for ANSI color indexes */
static const int  s_AnsiColors[] = { 0, 4, 2, 6, 1, 5, 3, 7, 8, 12, 10, 14, 9, 13, 11, 15 };

static int getClipKey( int nKey )
{
   int nRet = 0, nFlag, n;

   if( IS_CLIPKEY( nKey ) )
      nRet = GET_CLIPKEY( nKey );
   else if( ZH_INKEY_ISEXT( nKey ) )
      nRet = nKey;
   else
   {
      n = GET_KEYMASK( nKey );
      nKey = CLR_KEYMASK( nKey );
      nFlag = 0;
      if( n & KEY_SHIFTMASK )
         nFlag |= ZH_KF_SHIFT;
      if( n & KEY_CTRLMASK )
         nFlag |= ZH_KF_CTRL;
      if( n & KEY_ALTMASK )
         nFlag |= ZH_KF_ALT;
      if( n & KEY_KPADMASK )
         nFlag |= ZH_KF_KEYPAD;

      if( n & KEY_EXTDMASK )
         nRet = ZH_INKEY_NEW_KEY( nKey, nFlag );
      else
      {
         if( nKey > 0 && nKey < 32 )
         {
            nFlag |= ZH_KF_CTRL;
            nKey += ( 'A' - 1 );
         }
         nRet = ZH_INKEY_NEW_KEY( nKey, nFlag );
      }
   }

   return nRet;
}

/* SA_NOCLDSTOP in #if is a hack to detect POSIX compatible environment */
#if defined( ZH_OS_UNIX ) && defined( SA_NOCLDSTOP )

static void sig_handler( int iSigNo )
{
   int e = errno, status;
   pid_t pid;

   switch( iSigNo )
   {
      case SIGCHLD:
         while( ( pid = waitpid( -1, &status, WNOHANG ) ) > 0 )
            ;
         break;
      case SIGWINCH:
         s_WinSizeChangeFlag = ZH_TRUE;
         break;
      case SIGINT:
         #if 0
         s_InetrruptFlag = ZH_TRUE;
         #endif
         break;
      case SIGQUIT:
         #if 0
         s_BreakFlag = ZH_TRUE;
         #endif
         break;
      case SIGTSTP:
         #if 0
         s_DebugFlag = ZH_TRUE;
         #endif
         break;
      case SIGTTOU:
         s_fRestTTY = ZH_FALSE;
         break;
   }
   errno = e;
}

static void set_sig_handler( int iSig )
{
   struct sigaction act;

   sigaction( iSig, 0, &act );
   act.sa_handler = sig_handler;
#if defined( SA_RESTART )
   act.sa_flags = SA_RESTART | ( iSig == SIGCHLD ? SA_NOCLDSTOP : 0 );
#else
   act.sa_flags = ( iSig == SIGCHLD ? SA_NOCLDSTOP : 0 );
#endif
   sigaction( iSig, &act, 0 );
}

static void set_signals( void )
{
   int i, sigs[] = { SIGINT, SIGQUIT, SIGTSTP, SIGWINCH /*, SIGCHLD */, 0 };

   /* Ignore SIGPIPEs so they don't kill us. */
   signal( SIGPIPE, SIG_IGN );
   for( i = 0; sigs[ i ]; ++i )
   {
      set_sig_handler( sigs[ i ] );
   }
}

#endif

static int zh_gt_trm_getKbdState( PZH_GTTRM pTerm )
{
   int iFlags = 0;

   if( pTerm->mLastEvt.flags & ZH_KF_SHIFT ) iFlags |= ZH_GTI_KBD_SHIFT;
   if( pTerm->mLastEvt.flags & ZH_KF_CTRL  ) iFlags |= ZH_GTI_KBD_CTRL;
   if( pTerm->mLastEvt.flags & ZH_KF_ALT   ) iFlags |= ZH_GTI_KBD_ALT;

   return iFlags;
}

static int zh_gt_trm_getSize( PZH_GTTRM pTerm, int * piRows, int * piCols )
{
   *piRows = *piCols = 0;

#if  defined( ZH_OS_UNIX )  && defined( TIOCGWINSZ )
   if( pTerm->fOutTTY )
   {
      struct winsize win;

      if( ioctl( pTerm->hFileno, TIOCGWINSZ, ( char * ) &win ) != -1 )
      {
         *piRows = win.ws_row;
         *piCols = win.ws_col;
      }
   }
#else
   ZH_SYMBOL_UNUSED( pTerm );
#endif

   if( *piRows <= 0 || *piCols <= 0 )
   {
      char * env;
      if( ( env = getenv( "COLUMNS" ) ) != NULL )
         *piCols = atoi( env );
      if( ( env = getenv( "LINES" ) ) != NULL )
         *piRows = atoi( env );
   }

   return *piRows > 0 && *piCols > 0;
}

static void zh_gt_trm_termFlush( PZH_GTTRM pTerm )
{
   if( pTerm->iOutBufIndex > 0 )
   {
      zh_fsWriteLarge( pTerm->hFileno, pTerm->pOutBuf, pTerm->iOutBufIndex );
      pTerm->iOutBufIndex = 0;
   }
}

static void zh_gt_trm_termOut( PZH_GTTRM pTerm, const char * pStr, int iLen )
{
   if( pTerm->iOutBufSize )
   {
      while( iLen > 0 )
      {
         int i;
         if( pTerm->iOutBufSize == pTerm->iOutBufIndex )
            zh_gt_trm_termFlush( pTerm );
         i = pTerm->iOutBufSize - pTerm->iOutBufIndex;
         if( i > iLen )
            i = iLen;
         memcpy( pTerm->pOutBuf + pTerm->iOutBufIndex, pStr, i );
         pTerm->iOutBufIndex += i;
         pStr += i;
         iLen -= i;
      }
   }
}

#ifndef ZH_GT_UNICODE_BUF
static void zh_gt_trm_termOutTrans( PZH_GTTRM pTerm, const char * pStr, int iLen, int iAttr )
{
   if( pTerm->iOutBufSize )
   {
      PZH_CODEPAGE cdp = NULL;

      if( pTerm->fUTF8 )
      {
         if( ( iAttr & ( ZH_GTTRM_ATTR_ACSC | ZH_GTTRM_ATTR_BOX ) ) &&
             pTerm->cdpBox )
            cdp = pTerm->cdpBox;
         else if( pTerm->cdpHost )
            cdp = pTerm->cdpHost;
         else
            cdp = zh_vmCodepage();
      }

      if( cdp )
      {
         while( iLen > 0 )
         {
            int i = ( pTerm->iOutBufSize - pTerm->iOutBufIndex ) >> 2;
            if( i < 4 )
            {
               zh_gt_trm_termFlush( pTerm );
               i = pTerm->iOutBufSize >> 2;
            }
            if( i > iLen )
               i = iLen;
            pTerm->iOutBufIndex += zh_cdpStrToUTF8Disp( cdp, pStr, i,
                                    pTerm->pOutBuf + pTerm->iOutBufIndex,
                                    pTerm->iOutBufSize - pTerm->iOutBufIndex );
            pStr += i;
            iLen -= i;
         }
      }
      else
      {
         zh_gt_trm_termOut( pTerm, pStr, iLen );
      }
   }
}
#endif

/* ************************************************************************* */

/*
 * KEYBOARD and MOUSE
 */

static int add_efds( PZH_GTTRM pTerm, int fd, int mode,
                     int ( * eventFunc )( int, int, void * ), void * cargo )
{
   evtFD *pefd = NULL;
   int i;

   if( eventFunc == NULL && mode != O_RDONLY )
      return -1;

#if defined( ZH_OS_UNIX )
   {
      int fl;
      if( ( fl = fcntl( fd, F_GETFL, 0 ) ) == -1 )
         return -1;

      fl &= O_ACCMODE;
      if( ( fl == O_RDONLY && mode == O_WRONLY ) ||
           ( fl == O_WRONLY && mode == O_RDONLY ) )
         return -1;
   }
#endif

   for( i = 0; i < pTerm->efds_no && ! pefd; i++ )
      if( pTerm->event_fds[ i ]->fd == fd )
         pefd = pTerm->event_fds[ i ];

   if( pefd )
   {
      pefd->mode = mode;
      pefd->cargo = cargo;
      pefd->eventFunc = eventFunc;
      pefd->status = EVTFDSTAT_RUN;
   }
   else
   {
      if( pTerm->efds_size <= pTerm->efds_no )
      {
         pTerm->event_fds = ( evtFD ** )
               zh_xrealloc( pTerm->event_fds,
                            ( pTerm->efds_size += 10 ) * sizeof( evtFD * ) );
         pTerm->pPollSet = ( PZH_POLLFD )
               zh_xrealloc( pTerm->pPollSet,
                            pTerm->efds_size * sizeof( ZH_POLLFD ) );
      }

      pefd = ( evtFD * ) zh_xgrab( sizeof( evtFD ) );
      pefd->fd = fd;
      pefd->mode = mode;
      pefd->cargo = cargo;
      pefd->eventFunc = eventFunc;
      pefd->status = EVTFDSTAT_RUN;
      pTerm->event_fds[ pTerm->efds_no++ ] = pefd;
   }

   return fd;
}

#if defined( ZH_HAS_GPM )
static void del_efds( PZH_GTTRM pTerm, int fd )
{
   int i, n = -1;

   for( i = 0; i < pTerm->efds_no && n == -1; i++ )
      if( pTerm->event_fds[ i ]->fd == fd )
         n = i;

   if( n != -1 )
   {
      zh_xfree( pTerm->event_fds[ n ] );
      pTerm->efds_no--;
      for( i = n; i < pTerm->efds_no; i++ )
         pTerm->event_fds[ i ] = pTerm->event_fds[ i + 1 ];
   }
}
#endif

static void del_all_efds( PZH_GTTRM pTerm )
{
   if( pTerm->event_fds != NULL )
   {
      int i;

      for( i = 0; i < pTerm->efds_no; i++ )
         zh_xfree( pTerm->event_fds[ i ] );

      zh_xfree( pTerm->event_fds );
      zh_xfree( pTerm->pPollSet );

      pTerm->event_fds = NULL;
      pTerm->pPollSet = NULL;
      pTerm->efds_no = pTerm->efds_size = 0;
   }
}

static int getMouseKey( mouseEvent * mEvt )
{
   int nKey = 0;

   if( mEvt->lbuttons != mEvt->buttonstate )
   {
      if( mEvt->buttonstate & M_CURSOR_MOVE )
      {
         nKey = ZH_INKEY_NEW_MPOS( mEvt->col, mEvt->row );
         mEvt->buttonstate &= ~M_CURSOR_MOVE;
      }
      else if( mEvt->buttonstate & M_BUTTON_WHEELUP )
      {
         nKey = ZH_INKEY_NEW_MKEY( K_MWFORWARD, mEvt->flags );
         mEvt->buttonstate &= ~M_BUTTON_WHEELUP;
      }
      else if( mEvt->buttonstate & M_BUTTON_WHEELDOWN )
      {
         nKey = ZH_INKEY_NEW_MKEY( K_MWBACKWARD, mEvt->flags );
         mEvt->buttonstate &= ~M_BUTTON_WHEELDOWN;
      }
      else
      {
         int butt = mEvt->lbuttons ^ mEvt->buttonstate;

         if( butt & M_BUTTON_LEFT )
         {
            if( mEvt->buttonstate & M_BUTTON_LEFT )
            {
               mEvt->lbdn_row = mEvt->row;
               mEvt->lbdn_col = mEvt->col;
            }
            else
            {
               mEvt->lbup_row = mEvt->row;
               mEvt->lbup_col = mEvt->col;
            }
            nKey = ( mEvt->buttonstate & M_BUTTON_LEFT ) ?
               ( ( mEvt->buttonstate & M_BUTTON_LDBLCK ) ? K_LDBLCLK :
                 K_LBUTTONDOWN ) : K_LBUTTONUP;
            nKey = ZH_INKEY_NEW_MKEY( nKey, mEvt->flags );
            mEvt->lbuttons ^= M_BUTTON_LEFT;
            mEvt->buttonstate &= ~M_BUTTON_LDBLCK;
         }
         else if( butt & M_BUTTON_RIGHT )
         {
            if( mEvt->buttonstate & M_BUTTON_RIGHT )
            {
               mEvt->rbdn_row = mEvt->row;
               mEvt->rbdn_col = mEvt->col;
            }
            else
            {
               mEvt->rbup_row = mEvt->row;
               mEvt->rbup_col = mEvt->col;
            }
            nKey = ( mEvt->buttonstate & M_BUTTON_RIGHT ) ?
               ( ( mEvt->buttonstate & M_BUTTON_RDBLCK ) ? K_RDBLCLK :
                 K_RBUTTONDOWN ) : K_RBUTTONUP;
            nKey = ZH_INKEY_NEW_MKEY( nKey, mEvt->flags );
            mEvt->lbuttons ^= M_BUTTON_RIGHT;
            mEvt->buttonstate &= ~M_BUTTON_RDBLCK;
         }
         else if( butt & M_BUTTON_MIDDLE )
         {
            if( mEvt->buttonstate & M_BUTTON_MIDDLE )
            {
               mEvt->mbdn_row = mEvt->row;
               mEvt->mbdn_col = mEvt->col;
            }
            else
            {
               mEvt->mbup_row = mEvt->row;
               mEvt->mbup_col = mEvt->col;
            }
            nKey = ( mEvt->buttonstate & M_BUTTON_MIDDLE ) ?
               ( ( mEvt->buttonstate & M_BUTTON_MDBLCK ) ? K_MDBLCLK :
                 K_MBUTTONDOWN ) : K_MBUTTONUP;
            nKey = ZH_INKEY_NEW_MKEY( nKey, mEvt->flags );
            mEvt->lbuttons ^= M_BUTTON_MIDDLE;
            mEvt->buttonstate &= ~M_BUTTON_MDBLCK;
         }
         else
            mEvt->lbuttons = mEvt->buttonstate;
      }
   }

   return nKey;
}

static void chk_mevtdblck( PZH_GTTRM pTerm )
{
   int newbuttons = ( pTerm->mLastEvt.buttonstate & ~pTerm->mLastEvt.lbuttons ) & M_BUTTON_KEYMASK;

   if( newbuttons != 0 )
   {
#if defined( ZH_OS_UNIX )
      struct timeval tv;
#else
      double tv;
#endif

      TIMEVAL_GET( tv );
      if( newbuttons & M_BUTTON_LEFT )
      {
         if( TIMEVAL_LESS( tv, pTerm->mLastEvt.BL_time ) )
            pTerm->mLastEvt.buttonstate |= M_BUTTON_LDBLCK;
         TIMEVAL_ADD( pTerm->mLastEvt.BL_time, tv,
                      ZH_GTSELF_MOUSEGETDOUBLECLICKSPEED( pTerm->pGT ) );
      }
      if( newbuttons & M_BUTTON_MIDDLE )
      {
         if( TIMEVAL_LESS( tv, pTerm->mLastEvt.BM_time ) )
            pTerm->mLastEvt.buttonstate |= M_BUTTON_MDBLCK;
         TIMEVAL_ADD( pTerm->mLastEvt.BM_time, tv,
                      ZH_GTSELF_MOUSEGETDOUBLECLICKSPEED( pTerm->pGT ) );
      }
      if( newbuttons & M_BUTTON_RIGHT )
      {
         if( TIMEVAL_LESS( tv, pTerm->mLastEvt.BR_time ) )
            pTerm->mLastEvt.buttonstate |= M_BUTTON_RDBLCK;
         TIMEVAL_ADD( pTerm->mLastEvt.BR_time, tv,
                      ZH_GTSELF_MOUSEGETDOUBLECLICKSPEED( pTerm->pGT ) );
      }
   }
}

static void set_tmevt( PZH_GTTRM pTerm, unsigned char * cMBuf, mouseEvent * mEvt )
{
   int row, col;

   mEvt->flags = 0;
   if( cMBuf[ 0 ] & 0x04 )
      mEvt->flags |= ZH_KF_SHIFT;
   if( cMBuf[ 0 ] & 0x08 )
      mEvt->flags |= ZH_KF_ALT;
   if( cMBuf[ 0 ] & 0x10 )
      mEvt->flags |= ZH_KF_CTRL;

   col = cMBuf[ 1 ] - 33;
   row = cMBuf[ 2 ] - 33;
   if( mEvt->row != row || mEvt->col != col )
   {
      mEvt->buttonstate |= M_CURSOR_MOVE;
      mEvt->row = row;
      mEvt->col = col;
   }

   switch( cMBuf[ 0 ] & 0xC3 )
   {
      case 0x0:
         mEvt->buttonstate |= M_BUTTON_LEFT;
         break;
      case 0x1:
         mEvt->buttonstate |= M_BUTTON_MIDDLE;
         break;
      case 0x2:
         mEvt->buttonstate |= M_BUTTON_RIGHT;
         break;
      case 0x3:
         mEvt->buttonstate &= ~( M_BUTTON_KEYMASK | M_BUTTON_DBLMASK );
         break;
      case 0x40:
         if( cMBuf[ 0 ] & 0x20 )
            mEvt->buttonstate |= M_BUTTON_WHEELUP;
         break;
      case 0x41:
         if( cMBuf[ 0 ] & 0x20 )
            mEvt->buttonstate |= M_BUTTON_WHEELDOWN;
         break;
   }
   chk_mevtdblck( pTerm );
   #if 0
   printf( "\r\nmouse event: %02x, %02x, %02x\r\n", cMBuf[ 0 ], cMBuf[ 1 ], cMBuf[ 2 ] );
   #endif
}

#if defined( ZH_HAS_GPM )
static int set_gpmevt( int fd, int mode, void * cargo )
{
   int nKey = 0;
   PZH_GTTRM pTerm;
   Gpm_Event gEvt;

   ZH_SYMBOL_UNUSED( fd );
   ZH_SYMBOL_UNUSED( mode );

   pTerm = ( PZH_GTTRM ) cargo;

   if( Gpm_GetEvent( &gEvt ) > 0 )
   {
      pTerm->mLastEvt.flags = 0;
      if( gEvt.modifiers & ( 1 << KG_SHIFT ) )
         pTerm->mLastEvt.flags |= ZH_KF_SHIFT;
      if( gEvt.modifiers & ( 1 << KG_CTRL ) )
         pTerm->mLastEvt.flags |= ZH_KF_CTRL;
      if( gEvt.modifiers & ( 1 << KG_ALT ) )
         pTerm->mLastEvt.flags |= ZH_KF_ALT;

      pTerm->mLastEvt.row = gEvt.y;
      pTerm->mLastEvt.col = gEvt.x;
      if( gEvt.type & ( GPM_MOVE | GPM_DRAG ) )
         pTerm->mLastEvt.buttonstate |= M_CURSOR_MOVE;
      if( gEvt.type & GPM_DOWN )
      {
         if( gEvt.buttons & GPM_B_LEFT )
            pTerm->mLastEvt.buttonstate |= M_BUTTON_LEFT;
         if( gEvt.buttons & GPM_B_MIDDLE )
            pTerm->mLastEvt.buttonstate |= M_BUTTON_MIDDLE;
         if( gEvt.buttons & GPM_B_RIGHT )
            pTerm->mLastEvt.buttonstate |= M_BUTTON_RIGHT;
      }
      else if( gEvt.type & GPM_UP )
      {
         if( gEvt.buttons & GPM_B_LEFT )
            pTerm->mLastEvt.buttonstate &= ~M_BUTTON_LEFT;
         if( gEvt.buttons & GPM_B_MIDDLE )
            pTerm->mLastEvt.buttonstate &= ~M_BUTTON_MIDDLE;
         if( gEvt.buttons & GPM_B_RIGHT )
            pTerm->mLastEvt.buttonstate &= ~M_BUTTON_RIGHT;
      }
   }
   chk_mevtdblck( pTerm );
   nKey = getMouseKey( &pTerm->mLastEvt );

   return nKey ? ( ZH_INKEY_ISEXT( nKey ) ? nKey : SET_CLIPKEY( nKey ) ) : 0;
}

static void flush_gpmevt( PZH_GTTRM pTerm )
{
   if( gpm_fd >= 0 )
   {
      while( zh_fsCanRead( gpm_fd, 0 ) > 0 )
         set_gpmevt( gpm_fd, O_RDONLY, ( void * ) pTerm );

      while( getMouseKey( &pTerm->mLastEvt ) ) ;
   }
}
#endif

static void disp_mousecursor( PZH_GTTRM pTerm )
{
#if defined( ZH_HAS_GPM )
   if( ( pTerm->mouse_type & MOUSE_GPM ) && gpm_visiblepointer )
   {
      Gpm_DrawPointer( pTerm->mLastEvt.col, pTerm->mLastEvt.row,
                       gpm_consolefd );
   }
#else
   ZH_SYMBOL_UNUSED( pTerm );
#endif
}

static void mouse_init( PZH_GTTRM pTerm )
{
   if( pTerm->terminal_type == TERM_XTERM ||
       pTerm->terminal_type == TERM_LINUX )
   {
      zh_gt_trm_termOut( pTerm, s_szMouseOn, strlen( s_szMouseOn ) );
      zh_gt_trm_termFlush( pTerm );
      memset( ( void * ) &pTerm->mLastEvt, 0, sizeof( pTerm->mLastEvt ) );
      pTerm->mouse_type |= MOUSE_XTERM;
      pTerm->mButtons = 3;
   }
#if defined( ZH_HAS_GPM )
   if( pTerm->terminal_type == TERM_LINUX )
   {
      pTerm->Conn.eventMask =
         GPM_MOVE | GPM_DRAG | GPM_UP | GPM_DOWN | GPM_SINGLE | GPM_DOUBLE;
      /* give me move events but handle them anyway */
      pTerm->Conn.defaultMask = GPM_MOVE | GPM_HARD;
      /* report Ctrl,Alt,Shift events */
      pTerm->Conn.minMod = 0;
      pTerm->Conn.maxMod = ( ( 1 << KG_SHIFT ) | ( 1 << KG_CTRL ) | ( 1 << KG_ALT ) );
      gpm_zerobased = 1;
      gpm_visiblepointer = 0;
      if( Gpm_Open( &pTerm->Conn, 0 ) >= 0 && gpm_fd >= 0 )
      {
         int flags;

         if( ( flags = fcntl( gpm_fd, F_GETFL, 0 ) ) != -1 )
            fcntl( gpm_fd, F_SETFL, flags | O_NONBLOCK );

         pTerm->mouse_type |= MOUSE_GPM;
         memset( ( void * ) &pTerm->mLastEvt, 0, sizeof( pTerm->mLastEvt ) );
         flush_gpmevt( pTerm );
         add_efds( pTerm, gpm_fd, O_RDONLY, set_gpmevt, ( void * ) pTerm );

         /*
          * In recent GPM versions it produce unpleasure noice on the screen
          * so I covered it with this macro, [druzus]
          */
#ifdef ZH_GPM_USE_XTRA
         pTerm->mButtons = Gpm_GetSnapshot( NULL );
#else
         pTerm->mButtons = 3;
#endif
      }
   }
#endif
}

static void mouse_exit( PZH_GTTRM pTerm )
{
   if( pTerm->mouse_type & MOUSE_XTERM )
   {
      zh_gt_trm_termOut( pTerm, s_szMouseOff, strlen( s_szMouseOff ) );
      zh_gt_trm_termFlush( pTerm );
   }
#if defined( ZH_HAS_GPM )
   if( ( pTerm->mouse_type & MOUSE_GPM ) && gpm_fd >= 0 )
   {
      del_efds( pTerm, gpm_fd );
      Gpm_Close();
   }
#endif
}

static int read_bufch( PZH_GTTRM pTerm, int fd )
{
   int n = 0;

   if( STDIN_BUFLEN > pTerm->stdin_inbuf )
   {
      unsigned char buf[ STDIN_BUFLEN ];
      int i;

#if defined( ZH_OS_UNIX ) || defined( __DJGPP__ )
      n = read( fd, buf, STDIN_BUFLEN - pTerm->stdin_inbuf );
#else
      n = zh_fsRead( fd, buf, STDIN_BUFLEN - pTerm->stdin_inbuf );
#endif

      for( i = 0; i < n; i++ )
      {
         pTerm->stdin_buf[ pTerm->stdin_ptr_r++ ] = buf[ i ];
         if( pTerm->stdin_ptr_r == STDIN_BUFLEN )
            pTerm->stdin_ptr_r = 0;
         pTerm->stdin_inbuf++;
      }
   }

   return n;
}

static int get_inch( PZH_GTTRM pTerm, ZH_MAXINT timeout )
{
   int nRet = 0, nNext = 0, npfd = -1, nchk = pTerm->efds_no, lRead = 0;
   int mode, i, n;
   evtFD * pefd = NULL;

   ZH_MAXUINT timer = zh_timerInit( timeout );

   do
   {
      int counter;

      for( i = n = 0; i < pTerm->efds_no; i++ )
      {
         if( pTerm->event_fds[ i ]->status == EVTFDSTAT_RUN )
         {
            pTerm->pPollSet[ n ].fd = pTerm->event_fds[ i ]->fd;
            pTerm->pPollSet[ n ].events = 0;
            pTerm->pPollSet[ n ].revents = 0;
            if( pTerm->event_fds[ i ]->mode == O_RDWR ||
                pTerm->event_fds[ i ]->mode == O_RDONLY )
               pTerm->pPollSet[ n ].events |= ZH_POLLIN;
            if( pTerm->event_fds[ i ]->mode == O_RDWR ||
                pTerm->event_fds[ i ]->mode == O_WRONLY )
               pTerm->pPollSet[ n ].events |= ZH_POLLOUT;
            pTerm->event_fds[ i ]->index = n++;
         }
         else
         {
            pTerm->event_fds[ i ]->index = -1;
            if( pTerm->event_fds[ i ]->status == EVTFDSTAT_STOP &&
                pTerm->event_fds[ i ]->eventFunc == NULL )
               nNext = ZH_INKEY_NEW_EVENT( ZH_K_TERMINATE );
         }
      }

      counter = pTerm->key_counter;
      if( zh_fsPoll( pTerm->pPollSet, n, timeout ) > 0 )
      {
         for( i = 0; i < pTerm->efds_no; i++ )
         {
            n = pTerm->event_fds[ i ]->index;
            if( n < 0 )
               continue;
            n = pTerm->pPollSet[ n ].revents;
            n = ( ( n & ZH_POLLIN ) ? 1 : 0 ) | ( ( n & ZH_POLLOUT ) ? 2 : 0 );
            if( n != 0 )
            {
               if( pTerm->event_fds[ i ]->eventFunc == NULL )
               {
                  lRead = 1;
                  n = read_bufch( pTerm, pTerm->event_fds[ i ]->fd );
                  if( n == 0 )
                  {
                     pTerm->event_fds[ i ]->status = EVTFDSTAT_STOP;
                     nRet = ZH_INKEY_NEW_EVENT( ZH_K_CLOSE ); // emituje inkey event
                  }
               }
               else if( nRet == 0 && counter == pTerm->key_counter )
               {
                  if( n == 3 )
                     mode = O_RDWR;
                  else if( n == 2 )
                     mode = O_WRONLY;
                  else
                     mode = O_RDONLY;
                  pTerm->event_fds[ i ]->status = EVTFDSTAT_STOP;
                  n = ( pTerm->event_fds[ i ]->eventFunc )( pTerm->
                                                            event_fds[ i ]->fd,
                                                            mode,
                                                            pTerm->
                                                            event_fds[ i ]->
                                                            cargo );
                  if( IS_EVTFDSTAT( n ) )
                  {
                     pTerm->event_fds[ i ]->status = n;
                     if( nchk > i )
                        nchk = i;
                  }
                  else
                  {
                     pTerm->event_fds[ i ]->status = EVTFDSTAT_RUN;
                     if( IS_CLIPKEY( n ) || ZH_INKEY_ISEXT( n ) )
                     {
                        nRet = n;
                        npfd = pTerm->event_fds[ i ]->fd;
                        if( nchk > i )
                           nchk = i;
                     }
                  }
               }
            }
         }
      }
      else
         lRead = 1;
   }
   while( nRet == 0 && lRead == 0 &&
          ( timeout = zh_timerTest( timeout, &timer ) ) != 0 );

   for( i = n = nchk; i < pTerm->efds_no; i++ )
   {
      if( pTerm->event_fds[ i ]->status == EVTFDSTAT_DEL )
         zh_xfree( pTerm->event_fds[ i ] );
      else if( pTerm->event_fds[ i ]->fd == npfd )
         pefd = pTerm->event_fds[ i ];
      else
      {
         if( i > n )
            pTerm->event_fds[ n ] = pTerm->event_fds[ i ];
         n++;
      }
   }
   if( pefd )
      pTerm->event_fds[ n++ ] = pefd;
   pTerm->efds_no = n;

   return nRet == 0 ? nNext : nRet;
}

static int test_bufch( PZH_GTTRM pTerm, int n, int delay )
{
   int nKey = 0;

   if( pTerm->stdin_inbuf == n )
      nKey = get_inch( pTerm, delay );

   return ( IS_CLIPKEY( nKey ) || ZH_INKEY_ISEXT( nKey ) ) ? nKey :
          ( pTerm->stdin_inbuf > n ?
            pTerm->stdin_buf[ ( pTerm->stdin_ptr_l + n ) % STDIN_BUFLEN] : -1 );
}

static void free_bufch( PZH_GTTRM pTerm, int n )
{
   if( n > pTerm->stdin_inbuf )
      n = pTerm->stdin_inbuf;
   pTerm->stdin_ptr_l = ( pTerm->stdin_ptr_l + n ) % STDIN_BUFLEN;
   pTerm->stdin_inbuf -= n;
}

static int wait_key( PZH_GTTRM pTerm, int milisec )
{
   int nKey, esc, n, i, ch, counter;
   keyTab * ptr;

#if defined( ZH_OS_UNIX )
   if( s_WinSizeChangeFlag )
   {
      s_WinSizeChangeFlag = ZH_FALSE;
      return K_RESIZE;
   }
#endif

restart:
   counter = ++( pTerm->key_counter );
   nKey = esc = n = i = 0;
again:
   if( ( nKey = getMouseKey( &pTerm->mLastEvt ) ) != 0 )
      return nKey;

   ch = test_bufch( pTerm, i, pTerm->nTermMouseChars ? pTerm->esc_delay : milisec );
   if( counter != pTerm->key_counter )
      goto restart;

   if( ch >= 0 && ch <= 255 )
   {
      ++i;
      if( pTerm->nTermMouseChars )
      {
         pTerm->cTermMouseBuf[ 3 - pTerm->nTermMouseChars ] = ch;
         free_bufch( pTerm, i );
         i = 0;
         if( --pTerm->nTermMouseChars == 0 )
            set_tmevt( pTerm, pTerm->cTermMouseBuf, &pTerm->mLastEvt );
         goto again;
      }

      nKey = ch;
      ptr = pTerm->pKeyTab;
      if( i == 1 && nKey == K_ESC && esc == 0 )
      {
         nKey = EXKEY_ESC;
         esc = 1;
      }
      while( ch >= 0 && ch <= 255 && ptr != NULL )
      {
         if( ptr->ch == ch )
         {
            if( ptr->key != K_UNDEF )
            {
               nKey = ptr->key;
               switch( nKey )
               {
                  case K_METAALT:
                     pTerm->key_flag |= KEY_ALTMASK;
                     break;
                  case K_METACTRL:
                     pTerm->key_flag |= KEY_CTRLMASK;
                     break;
                  case K_NATIONAL:
                     pTerm->nation_mode = ! pTerm->nation_mode;
                     break;
                  case K_MOUSETERM:
                     pTerm->nTermMouseChars = 3;
                     break;
                  default:
                     n = i;
               }
               if( n != i )
               {
                  free_bufch( pTerm, i );
                  i = n = nKey = 0;
                  if( esc == 2 )
                     break;
                  esc = 0;
                  goto again;
               }
            }
            ptr = ptr->nextCh;
            if( ptr )
               if( ( ch = test_bufch( pTerm, i, pTerm->esc_delay ) ) != -1 )
                  ++i;
            if( counter != pTerm->key_counter )
               goto restart;
         }
         else
            ptr = ptr->otherCh;
      }
   }
   if( ch == -1 && pTerm->nTermMouseChars )
      pTerm->nTermMouseChars = 0;

   if( IS_CLIPKEY( ch ) )
      nKey = GET_CLIPKEY( ch );
   else if( ZH_INKEY_ISEXT( ch ) )
      nKey = ch;
   else
   {
      if( esc == 1 && n == 0 && ( ch != -1 || i >= 2 ) )
      {
         nKey = 0;
         esc = 2;
         i = n = 1;
         goto again;
      }
      if( esc == 2 )
      {
         if( nKey != 0 )
            pTerm->key_flag |= KEY_ALTMASK;
         else
            nKey = EXKEY_ESC;
         if( n == 1 && i > 1 )
            n = 2;
      }
      else
      {
         if( nKey != 0 && ( pTerm->key_flag & KEY_CTRLMASK ) != 0 &&
                          ( pTerm->key_flag & KEY_ALTMASK ) != 0 )
         {
            pTerm->key_flag &= ~( KEY_CTRLMASK | KEY_ALTMASK );
            pTerm->key_flag |= KEY_SHIFTMASK;
         }
         if( n == 0 && i > 0 )
            n = 1;
      }

      if( n > 0 )
         free_bufch( pTerm, n );

      if( pTerm->key_flag != 0 && nKey != 0 )
      {
         nKey |= pTerm->key_flag;
         pTerm->key_flag = 0;
      }

#ifdef ZH_GT_UNICODE_BUF
      if( ! pTerm->fUTF8 )
      {
         if( nKey != 0 )
         {
            int u = ZH_GTSELF_KEYTRANS( pTerm->pGT, nKey );
            if( u )
               return ZH_INKEY_NEW_UNICODE( u );
         }
      }
      else if( nKey >= 32 && nKey <= 255 )
      {
         ZH_WCHAR wc = 0;
         n = i = 0;
         if( zh_cdpUTF8ToU16NextChar( ( ZH_UCHAR ) nKey, &n, &wc ) )
         {
            while( n > 0 )
            {
               ch = test_bufch( pTerm, i++, pTerm->esc_delay );
               if( ch < 0 || ch > 255 )
                  break;
               if( ! zh_cdpUTF8ToU16NextChar( ( ZH_UCHAR ) ch, &n, &wc ) )
                  n = -1;
            }
            if( n == 0 )
            {
               free_bufch( pTerm, i );
               return ZH_INKEY_NEW_UNICODE( wc );
            }
         }
      }
#else
      if( nKey >= 32 && nKey <= 255 && pTerm->fUTF8 && pTerm->cdpIn )
      {
         ZH_USHORT uc = 0;
         n = i = 0;
         if( zh_cdpGetFromUTF8( pTerm->cdpIn, ( ZH_UCHAR ) nKey, &n, &uc ) )
         {
            while( n > 0 )
            {
               ch = test_bufch( pTerm, i++, pTerm->esc_delay );
               if( ch < 0 || ch > 255 )
                  break;
               if( ! zh_cdpGetFromUTF8( pTerm->cdpIn, ch, &n, &uc ) )
                  n = -1;
            }
            if( n == 0 )
            {
               free_bufch( pTerm, i );
               nKey = uc;
            }
         }
      }

/*
      if( pTerm->nation_transtbl && pTerm->nation_mode &&
           nKey >= 32 && nKey < 128 && pTerm->nation_transtbl[nKey] )
         nKey = pTerm->nation_transtbl[nKey];
 */
      if( nKey > 0 && nKey <= 255 && pTerm->keyTransTbl[ nKey ] )
         nKey = pTerm->keyTransTbl[ nKey ];
#endif
      if( nKey )
         nKey = getClipKey( nKey );
   }

   return nKey;
}

/* ************************************************************************* */

/*
 * LINUX terminal operations
 */
static void zh_gt_trm_LinuxSetTermMode( PZH_GTTRM pTerm, int iAM )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_LinuxSetTermMode(%p,%d)", ( void * ) pTerm, iAM ) );

   if( iAM != pTerm->iAM )
   {
      if( iAM == 0 )
         zh_gt_trm_termOut( pTerm, "\x1B[m", 3 );

      zh_gt_trm_termOut( pTerm, iAM ? "\x1B[?7h" : "\x1B[?7l", 5 );
      pTerm->iAM = iAM;
   }
}

static void zh_gt_trm_LinuxTone( PZH_GTTRM pTerm, double dFrequency, double dDuration )
{
   char escseq[ 64 ];

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_LinuxTone(%p,%lf,%lf)", ( void * ) pTerm, dFrequency, dDuration ) );

   if( pTerm->iACSC )
   {
      zh_gt_trm_termOut( pTerm, "\033[10m", 5 );
      pTerm->iACSC = 0;
   }
   zh_snprintf( escseq, sizeof( escseq ), "\033[10;%d]\033[11;%d]\007",
                ( int ) dFrequency, ( int ) ( dDuration * 1000.0 / 18.2 ) );
   zh_gt_trm_termOut( pTerm, escseq, strlen( escseq ) );
   zh_gt_trm_termFlush( pTerm );

   zh_gtSleep( pTerm->pGT, dDuration / 18.2 );
}

static void zh_gt_trm_LinuxSetCursorStyle( PZH_GTTRM pTerm, int iStyle )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_LinuxSetCursorStyle(%p,%d)", ( void * ) pTerm, iStyle ) );

   if( pTerm->iCursorStyle != iStyle )
   {
      int lcurs = -1;

      switch( iStyle )
      {
         case SC_NONE:
            lcurs = 1;
            break;
         case SC_NORMAL:
            lcurs = 2;
            break;
         case SC_INSERT:
            lcurs = 4;
            break;
         case SC_SPECIAL1:
            lcurs = 8;
            break;
         case SC_SPECIAL2:
            /* TODO: find a proper sequence to set a cursor
               to SC_SPECIAL2 under Linux console?
               There is no such mode in current stable kernels (2.4.20)
             */
            lcurs = 4;
            break;
      }
      if( lcurs != -1 )
      {
         char escseq[ 64 ];
         zh_snprintf( escseq, sizeof( escseq ), "\033[?25%c\033[?%dc",
                      iStyle == SC_NONE ? 'l' : 'h', lcurs );
         zh_gt_trm_termOut( pTerm, escseq, strlen( escseq ) );
         pTerm->iCursorStyle = iStyle;
      }
   }
}

static void zh_gt_trm_LinuxSetPalette( PZH_GTTRM pTerm, int iIndexFrom, int iIndexTo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_LinuxSetPalette(%p,%d,%d)", ( void * ) pTerm, iIndexFrom, iIndexTo ) );

   if( iIndexFrom < 0 )
      iIndexFrom = 0;
   if( iIndexTo > 15 )
      iIndexTo = 15;

   if( iIndexFrom <= iIndexTo )
   {
      do
      {
         char szColor[ 11 ];
         int iAnsiIndex = s_AnsiColors[ iIndexFrom & 0x0F ];

         zh_snprintf( szColor, sizeof( szColor ), "\033]P%X%02X%02X%02X",
                      iAnsiIndex,
                      ( pTerm->colors[ iIndexFrom ] ) & 0xff,
                      ( pTerm->colors[ iIndexFrom ] >> 8 ) & 0xff,
                      ( pTerm->colors[ iIndexFrom ] >> 16 ) & 0xff );
         zh_gt_trm_termOut( pTerm, szColor, 10 );
      }
      while( ++iIndexFrom <= iIndexTo );

      /* ESC ] is Operating System Command (OSC) which by default should
       * be terminated by ESC \ (ST). Some terminals which sets LINUX
       * TERM envvar but do not correctly understand above palette set
       * sequence may hang waiting for ST. We send ST below to avoid such
       * situation.
       * Linux console simply ignore ST terminator so nothing wrong
       * should happen.
       */
      zh_gt_trm_termOut( pTerm, "\033\\", 2 );
   }
}

static void zh_gt_trm_LinuxResetPalette( PZH_GTTRM pTerm )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_LinuxResetPalette(%p)", ( void * ) pTerm ) );

   zh_gt_trm_termOut( pTerm, "\033]R", 3 );
}

/*
 * XTERM terminal operations
 */
static ZH_BOOL zh_gt_trm_XtermSetMode( PZH_GTTRM pTerm, int * piRows, int * piCols )
{
   int iHeight, iWidth;
   char escseq[ 64 ];

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_XtermSetMode(%p,%d,%d)", ( void * ) pTerm, *piRows, *piCols ) );

   ZH_GTSELF_GETSIZE( pTerm->pGT, &iHeight, &iWidth );
   zh_snprintf( escseq, sizeof( escseq ), "\033[8;%d;%dt", *piRows, *piCols );
   zh_gt_trm_termOut( pTerm, escseq, strlen( escseq ) );
   zh_gt_trm_termFlush( pTerm );

#if defined( ZH_OS_UNIX ) || defined( __DJGPP__ )
   /* dirty hack - wait for SIGWINCH */
   if( *piRows != iHeight || *piCols != iWidth )
      sleep( 3 );
   if( s_WinSizeChangeFlag )
      s_WinSizeChangeFlag = ZH_FALSE;
#endif

   zh_gt_trm_getSize( pTerm, piRows, piCols );

   return ZH_TRUE;
}

static void zh_gt_trm_XtermSetAttributes( PZH_GTTRM pTerm, int iAttr )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_XtermSetAttributes(%p,%d)", ( void * ) pTerm, iAttr ) );

   if( pTerm->iCurrentSGR != iAttr )
   {
      int i, acsc, bg, fg, bold, blink, rgb;
      char buff[ 64 ];

      i = 2;
      buff[ 0 ] = 0x1b;
      buff[ 1 ] = '[';

      acsc  = ( iAttr & ZH_GTTRM_ATTR_ACSC ) && ! pTerm->fUTF8 ? 1 : 0;
      if( pTerm->iExtColor == ZH_GTTRM_CLRSTD )
      {
         bg    = s_AnsiColors[ ( iAttr >> 4 ) & 0x07 ];
         fg    = s_AnsiColors[ iAttr & 0x07 ];
         bold  = ( iAttr & 0x08 ) ? 1 : 0;
         blink = ( iAttr & 0x80 ) ? 1 : 0;
      }
      else
      {
         bg = s_AnsiColors[ ( iAttr >> 4 ) & 0x0F ];
         fg = s_AnsiColors[ iAttr & 0x0F ];
         bold = blink = 0;
      }

      if( pTerm->iCurrentSGR == -1 )
      {
         buff[ i++ ] = 'm';
         buff[ i++ ] = 0x1b;
         buff[ i++ ] = '(';
         buff[ i++ ] = acsc ? '0' : 'B';

         buff[ i++ ] = 0x1b;
         buff[ i++ ] = '[';

         if( pTerm->iExtColor == ZH_GTTRM_CLRSTD )
         {
            if( bold )
            {
               buff[ i++ ] = '1';
               buff[ i++ ] = ';';
            }
            if( blink )
            {
               buff[ i++ ] = '5';
               buff[ i++ ] = ';';
            }
            buff[ i++ ] = '3';
            buff[ i++ ] = '0' + fg;
            buff[ i++ ] = ';';
            buff[ i++ ] = '4';
            buff[ i++ ] = '0' + bg;
         }
         else if( pTerm->iExtColor == ZH_GTTRM_CLRX16 )
         {
            /* ESC [ 38 ; 5 ; <fg> m */
            buff[ i++ ] = '3';
            buff[ i++ ] = '8';
            buff[ i++ ] = ';';
            buff[ i++ ] = '5';
            buff[ i++ ] = ';';
            if( fg >= 10 )
               buff[ i++ ] = '1';
            buff[ i++ ] = '0' + fg % 10;
            buff[ i++ ] = ';';
            /* ESC [ 48 ; 5 ; <bg> m */
            buff[ i++ ] = '4';
            buff[ i++ ] = '8';
            buff[ i++ ] = ';';
            buff[ i++ ] = '5';
            buff[ i++ ] = ';';
            if( bg >= 10 )
               buff[ i++ ] = '1';
            buff[ i++ ] = '0' + bg % 10;
         }
         else if( pTerm->iExtColor == ZH_GTTRM_CLR256 )
         {
            /* ESC [ 38 ; 5 ; <16 + 36 * r + 6 * g + b> m   0 <= r,g,b <= 5 */
            rgb = pTerm->colors[ iAttr & 0x0F ];
            rgb = 16 + 36 * ( (   rgb         & 0xFF ) / 43 ) +
                        6 * ( ( ( rgb >> 8  ) & 0xFF ) / 43 ) +
                            ( ( ( rgb >> 16 ) & 0xFF ) / 43 );
            i += zh_snprintf( buff + i, sizeof( buff ) - i, "38;5;%d", rgb );
            /* ESC [ 48 ; 5 ; <16 + 36 * r + 6 * g + b> m   0 <= r,g,b <= 5 */
            rgb = pTerm->colors[ ( iAttr >> 4 ) & 0x0F ];
            rgb = 16 + 36 * ( (   rgb         & 0xFF ) / 43 ) +
                        6 * ( ( ( rgb >> 8  ) & 0xFF ) / 43 ) +
                            ( ( ( rgb >> 16 ) & 0xFF ) / 43 );
            i += zh_snprintf( buff + i, sizeof( buff ) - i, ";48;5;%d", rgb );
         }
         else if( pTerm->iExtColor == ZH_GTTRM_CLRRGB )
         {
            /* ESC [ 38 ; 2 ; <r> ; <g> ; <b> m */
            rgb = pTerm->colors[ iAttr & 0x0F ];
            i += zh_snprintf( buff + i, sizeof( buff ) - i, "38;2;%d;%d;%d",
                              rgb & 0xFF, ( rgb >> 8 ) & 0xFF, ( rgb >> 16 ) & 0xFF );
            /* ESC [ 48 ; 2 ; <r> ; <g> ; <b> m */
            rgb = pTerm->colors[ ( iAttr >> 4 ) & 0x0F ];
            i += zh_snprintf( buff + i, sizeof( buff ) - i, ";48;2;%d;%d;%d",
                              rgb & 0xFF, ( rgb >> 8 ) & 0xFF, ( rgb >> 16 ) & 0xFF );
         }
         else if( pTerm->iExtColor == ZH_GTTRM_CLRAIX )
         {
            if( fg < 8 )
            {
               buff[ i++ ] = '3';
               buff[ i++ ] = '0' + fg;
            }
            else
            {
               buff[ i++ ] = '9';
               buff[ i++ ] = '0' - 8 + fg;
            }
            buff[ i++ ] = ';';
            if( bg < 8 )
            {
               buff[ i++ ] = '4';
               buff[ i++ ] = '0' + bg;
            }
            else
            {
               buff[ i++ ] = '1';
               buff[ i++ ] = '0';
               buff[ i++ ] = '0' - 8 + bg;
            }
         }
         buff[ i++ ] = 'm';
         pTerm->iACSC    = acsc;
         pTerm->iBold    = bold;
         pTerm->iBlink   = blink;
         pTerm->iFgColor = fg;
         pTerm->iBgColor = bg;
      }
      else
      {
         if( pTerm->iBold != bold )
         {
            if( bold )
               buff[ i++ ] = '1';
            else
            {
               buff[ i++ ] = '2';
               buff[ i++ ] = '2';
            }
            buff[ i++ ] = ';';
            pTerm->iBold = bold;
         }
         if( pTerm->iBlink != blink )
         {
            if( ! blink )
               buff[ i++ ] = '2';
            buff[ i++ ] = '5';
            buff[ i++ ] = ';';
            pTerm->iBlink = blink;
         }
         if( pTerm->iFgColor != fg )
         {
            if( pTerm->iExtColor == ZH_GTTRM_CLRSTD )
            {
               buff[ i++ ] = '3';
               buff[ i++ ] = '0' + fg;
            }
            else if( pTerm->iExtColor == ZH_GTTRM_CLRX16 )
            {
               /* ESC [ 38 ; 5 ; <fg> m */
               buff[ i++ ] = '3';
               buff[ i++ ] = '8';
               buff[ i++ ] = ';';
               buff[ i++ ] = '5';
               buff[ i++ ] = ';';
               if( fg >= 10 )
                  buff[ i++ ] = '1';
               buff[ i++ ] = '0' + fg % 10;
            }
            else if( pTerm->iExtColor == ZH_GTTRM_CLR256 )
            {
               /* ESC [ 38 ; 5 ; <16 + 36 * r + 6 * g + b> m   0 <= r,g,b <= 5 */
               rgb = pTerm->colors[ iAttr & 0x0F ];
               rgb = 16 + 36 * ( (   rgb         & 0xFF ) / 43 ) +
                           6 * ( ( ( rgb >> 8  ) & 0xFF ) / 43 ) +
                               ( ( ( rgb >> 16 ) & 0xFF ) / 43 );
               i += zh_snprintf( buff + i, sizeof( buff ) - i, "38;5;%d", rgb );
            }
            else if( pTerm->iExtColor == ZH_GTTRM_CLRRGB )
            {
               /* ESC [ 38 ; 2 ; <r> ; <g> ; <b> m */
               rgb = pTerm->colors[ iAttr & 0x0F ];
               i += zh_snprintf( buff + i, sizeof( buff ) - i, "38;2;%d;%d;%d",
                                 rgb & 0xFF, ( rgb >> 8 ) & 0xFF, ( rgb >> 16 ) & 0xFF );
            }
            else if( pTerm->iExtColor == ZH_GTTRM_CLRAIX )
            {
               if( fg < 8 )
               {
                  buff[ i++ ] = '3';
                  buff[ i++ ] = '0' + fg;
               }
               else
               {
                  buff[ i++ ] = '9';
                  buff[ i++ ] = '0' - 8 + fg;
               }
            }
            buff[ i++ ] = ';';
            pTerm->iFgColor = fg;
         }
         if( pTerm->iBgColor != bg )
         {
            if( pTerm->iExtColor == ZH_GTTRM_CLRSTD )
            {
               buff[ i++ ] = '4';
               buff[ i++ ] = '0' + bg;
            }
            else if( pTerm->iExtColor == ZH_GTTRM_CLRX16 )
            {
               /* ESC [ 48 ; 5 ; <fg> m */
               buff[ i++ ] = '4';
               buff[ i++ ] = '8';
               buff[ i++ ] = ';';
               buff[ i++ ] = '5';
               buff[ i++ ] = ';';
               if( bg >= 10 )
                  buff[ i++ ] = '1';
               buff[ i++ ] = '0' + bg % 10;
            }
            else if( pTerm->iExtColor == ZH_GTTRM_CLR256 )
            {
               /* ESC [ 48 ; 5 ; <16 + 36 * r + 6 * g + b> m   0 <= r,g,b <= 5 */
               rgb = pTerm->colors[ ( iAttr >> 4 ) & 0x0F ];
               rgb = 16 + 36 * ( (   rgb         & 0xFF ) / 43 ) +
                           6 * ( ( ( rgb >> 8  ) & 0xFF ) / 43 ) +
                               ( ( ( rgb >> 16 ) & 0xFF ) / 43 );
               i += zh_snprintf( buff + i, sizeof( buff ) - i, "48;5;%d", rgb );
            }
            else if( pTerm->iExtColor == ZH_GTTRM_CLRRGB )
            {
               /* ESC [ 48 ; 2 ; <r> ; <g> ; <b> m */
               rgb = pTerm->colors[ ( iAttr >> 4 ) & 0x0F ];
               i += zh_snprintf( buff + i, sizeof( buff ) - i, "48;2;%d;%d;%d",
                                 rgb & 0xFF, ( rgb >> 8 ) & 0xFF, ( rgb >> 16 ) & 0xFF );
            }
            else if( pTerm->iExtColor == ZH_GTTRM_CLRAIX )
            {
               if( bg < 8 )
               {
                  buff[ i++ ] = '4';
                  buff[ i++ ] = '0' + bg;
               }
               else
               {
                  buff[ i++ ] = '1';
                  buff[ i++ ] = '0';
                  buff[ i++ ] = '0' - 8 + bg;
               }
            }
            buff[ i++ ] = ';';
            pTerm->iBgColor = bg;
         }
         buff[ i - 1 ] = 'm';
         if( pTerm->iACSC != acsc )
         {
            if( i <= 2 )
               i = 0;
            buff[ i++ ] = 0x1b;
            buff[ i++ ] = '(';
            buff[ i++ ] = acsc ? '0' : 'B';
            pTerm->iACSC = acsc;
         }
      }
      pTerm->iCurrentSGR = iAttr;
      if( i > 2 )
      {
         zh_gt_trm_termOut( pTerm, buff, i );
      }
   }
}

static void zh_gt_trm_XtermSetTitle( PZH_GTTRM pTerm, const char * szTitle )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_XtermSetTitle(%p,%s)", ( void * ) pTerm, szTitle ) );

   zh_gt_trm_termOut( pTerm, "\033]0;", 4 );
   if( szTitle )
      zh_gt_trm_termOut( pTerm, szTitle, strlen( szTitle ) );
   zh_gt_trm_termOut( pTerm, "\007", 1 );
}


/*
 * BSD console
 */
static ZH_BOOL zh_gt_trm_BsdGetCursorPos( PZH_GTTRM pTerm, int * iRow, int * iCol,
                                          const char * szPost )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_BsdGetCursorPos(%p,%p,%p,%s)", ( void * ) pTerm, ( void * ) iRow, ( void * ) iCol, szPost ) );

   ZH_SYMBOL_UNUSED( szPost );

   if( pTerm->fPosAnswer )
   {
      pTerm->fPosAnswer = ZH_FALSE;
      *iRow = *iCol = -1;
   }

   return ZH_FALSE;
}

static void zh_gt_trm_BsdSetCursorStyle( PZH_GTTRM pTerm, int iStyle )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_BsdSetCursorStyle(%p,%d)", ( void * ) pTerm, iStyle ) );

   if( pTerm->iCursorStyle != iStyle )
   {
      const char * escseq = NULL;

      switch( iStyle )
      {
         case SC_NONE:
            escseq = "\033[=5C";
            break;
         case SC_NORMAL:
            escseq = "\033[=11;13C\033[=2C";
            break;
         case SC_INSERT:
            escseq = "\033[=8;15C\033[=2C";
            break;
         case SC_SPECIAL1:
            escseq = "\033[=0;15C\033[=2C";
            break;
         case SC_SPECIAL2:
            escseq = "\033[=0;7C\033[=2C";
            break;
         default:
            return;
      }

      zh_gt_trm_termOut( pTerm, escseq, strlen( escseq ) );
      pTerm->iCursorStyle = iStyle;
   }
}

static void zh_gt_trm_BsdTone( PZH_GTTRM pTerm, double dFrequency, double dDuration )
{
   char escseq[ 64 ];

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_BsdTone(%p,%lf,%lf)", ( void * ) pTerm, dFrequency, dDuration ) );

   zh_snprintf( escseq, sizeof( escseq ), "\033[=%d;%dB\007",
                ( int ) dFrequency, ( int ) ( dDuration * 10.0 / 18.2 ) );
   zh_gt_trm_termOut( pTerm, escseq, strlen( escseq ) );
   zh_gt_trm_termFlush( pTerm );

   zh_gtSleep( pTerm->pGT, dDuration / 18.2 );
}



/*
 * ANSI terminal operations
 */
static void zh_gt_trm_AnsiSetTermMode( PZH_GTTRM pTerm, int iAM )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_AnsiSetTermMode(%p,%d)", ( void * ) pTerm, iAM ) );

   if( iAM != pTerm->iAM )
   {
      if( iAM == 0 )
      {
         zh_gt_trm_termOut( pTerm, "\x1B[0m", 4 );
      }
      /*
       * disabled until I'll find good PC-ANSI terminal documentation with
       * detail Auto Margin and Auto Line Wrapping description, [druzus]
       */
#if 0
      zh_gt_trm_termOut( pTerm, iAM ? "\x1B[?7h" : "\x1B[?7l", 5 );
#endif
      pTerm->iAM = iAM;
   }
}

static ZH_BOOL zh_gt_trm_AnsiGetCursorPos( PZH_GTTRM pTerm, int * iRow, int * iCol,
                                           const char * szPost )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_AnsiGetCursorPos(%p,%p,%p,%s)", ( void * ) pTerm, ( void * ) iRow, ( void * ) iCol, szPost ) );

   if( pTerm->fPosAnswer )
   {
      char rdbuf[ 64 ];
      int i, j, n, d, y, x;
      ZH_MAXINT timeout;
      ZH_MAXUINT timer;

      zh_gt_trm_termOut( pTerm, "\x1B[6n", 4 );
      if( szPost )
         zh_gt_trm_termOut( pTerm, szPost, strlen( szPost ) );
      zh_gt_trm_termFlush( pTerm );

      n = j = x = y = 0;
      pTerm->fPosAnswer = ZH_FALSE;

      /* wait up to 2 seconds for answer */
      timeout = 2000;
      timer = zh_timerInit( timeout );
      for( ;; )
      {
         /* looking for cursor position in "\033[%d;%dR" */
         while( j < n && rdbuf[ j ] != '\033' )
            ++j;
         if( n - j >= 6 )
         {
            i = j + 1;
            if( rdbuf[ i ] == '[' )
            {
               y = 0;
               d = ++i;
               while( i < n && rdbuf[ i ] >= '0' && rdbuf[ i ] <= '9' )
                  y = y * 10 + ( rdbuf[ i++ ] - '0' );
               if( i < n && i > d && rdbuf[ i ] == ';' )
               {
                  x = 0;
                  d = ++i;
                  while( i < n && rdbuf[ i ] >= '0' && rdbuf[ i ] <= '9' )
                     x = x * 10 + ( rdbuf[ i++ ] - '0' );
                  if( i < n && i > d && rdbuf[ i ] == 'R' )
                  {
                     if( szPost )
                     {
                        while( j >= 5 )
                        {
                           if( zh_strnicmp( rdbuf + j - 5, "PuTTY", 5 ) == 0 )
                           {
                              pTerm->terminal_ext |= TERM_PUTTY;
                              break;
                           }
                           --j;
                        }
                     }
                     pTerm->fPosAnswer = ZH_TRUE;
                     break;
                  }
               }
            }
            if( i < n )
            {
               j = i;
               continue;
            }
         }
         if( n == sizeof( rdbuf ) )
            break;

         if( ( timeout = zh_timerTest( timeout, &timer ) ) == 0 )
            break;
         else
         {
#if defined( ZH_OS_UNIX )
            if( zh_fsCanRead( pTerm->hFilenoStdin, timeout ) <= 0 )
               break;
            i = read( pTerm->hFilenoStdin, rdbuf + n, sizeof( rdbuf ) - n );
#else
            int iTODO;
            break;
#endif

            if( i <= 0 )
               break;
            n += i;
         }
      }

      if( pTerm->fPosAnswer )
      {
         *iRow = y - 1;
         *iCol = x - 1;
      }
      else
      {
         *iRow = *iCol = -1;
      }
   }
   return pTerm->fPosAnswer;
}

static void zh_gt_trm_AnsiSetCursorPos( PZH_GTTRM pTerm, int iRow, int iCol )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_AnsiSetCursorPos(%p,%d,%d)", ( void * ) pTerm, iRow, iCol ) );

   if( pTerm->iRow != iRow || pTerm->iCol != iCol )
   {
      char buff[ 16 ];
      zh_snprintf( buff, sizeof( buff ), "\x1B[%d;%dH", iRow + 1, iCol + 1 );
      zh_gt_trm_termOut( pTerm, buff, strlen( buff ) );
      pTerm->iRow = iRow;
      pTerm->iCol = iCol;
   }
}

static void zh_gt_trm_AnsiSetCursorStyle( PZH_GTTRM pTerm, int iStyle )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_AnsiSetCursorStyle(%p,%d)", ( void * ) pTerm, iStyle ) );

   if( pTerm->iCursorStyle != iStyle )
   {
      zh_gt_trm_termOut( pTerm, iStyle == SC_NONE ?
                                             "\x1B[?25l" : "\x1B[?25h", 6 );
      pTerm->iCursorStyle = iStyle;
   }
}

static void zh_gt_trm_AnsiSetAttributes( PZH_GTTRM pTerm, int iAttr )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_AnsiSetAttributes(%p,%d)", ( void * ) pTerm, iAttr ) );

   if( pTerm->iCurrentSGR != iAttr )
   {
      int i, acsc, bg, fg, bold, blink;
      char buff[ 32 ];

      i = 2;
      buff[ 0 ] = 0x1b;
      buff[ 1 ] = '[';

      acsc  = ( iAttr & ZH_GTTRM_ATTR_ACSC ) ? 1 : 0;
      bg    = s_AnsiColors[ ( iAttr >> 4 ) & 0x07 ];
      fg    = s_AnsiColors[ iAttr & 0x07 ];
      bold  = ( iAttr & 0x08 ) ? 1 : 0;
      blink = ( iAttr & 0x80 ) ? 1 : 0;

      if( pTerm->iCurrentSGR == -1 )
      {
         buff[ i++ ] = '0';
         buff[ i++ ] = ';';
         buff[ i++ ] = '1';
         buff[ i++ ] = acsc ? '1' : '0';
         buff[ i++ ] = ';';
         if( bold )
         {
            buff[ i++ ] = '1';
            buff[ i++ ] = ';';
         }
         if( blink )
         {
            buff[ i++ ] = '5';
            buff[ i++ ] = ';';
         }
         buff[ i++ ] = '3';
         buff[ i++ ] = '0' + fg;
         buff[ i++ ] = ';';
         buff[ i++ ] = '4';
         buff[ i++ ] = '0' + bg;
         buff[ i++ ] = 'm';
         pTerm->iACSC    = acsc;
         pTerm->iBold    = bold;
         pTerm->iBlink   = blink;
         pTerm->iFgColor = fg;
         pTerm->iBgColor = bg;
      }
      else
      {
         if( pTerm->iACSC != acsc )
         {
            buff[ i++ ] = '1';
            buff[ i++ ] = acsc ? '1' : '0';
            buff[ i++ ] = ';';
            pTerm->iACSC = acsc;
         }
         if( pTerm->iBold != bold )
         {
            if( bold )
               buff[ i++ ] = '1';
            else
            {
               buff[ i++ ] = '2';
               buff[ i++ ] = '2';
            }
            buff[ i++ ] = ';';
            pTerm->iBold = bold;
         }
         if( pTerm->iBlink != blink )
         {
            if( ! blink )
               buff[ i++ ] = '2';
            buff[ i++ ] = '5';
            buff[ i++ ] = ';';
            pTerm->iBlink = blink;
         }
         if( pTerm->iFgColor != fg )
         {
            buff[ i++ ] = '3';
            buff[ i++ ] = '0' + fg;
            buff[ i++ ] = ';';
            pTerm->iFgColor = fg;
         }
         if( pTerm->iBgColor != bg )
         {
            buff[ i++ ] = '4';
            buff[ i++ ] = '0' + bg;
            buff[ i++ ] = ';';
            pTerm->iBgColor = bg;
         }
         buff[ i - 1 ] = 'm';
      }
      pTerm->iCurrentSGR = iAttr;
      if( i > 2 )
      {
         zh_gt_trm_termOut( pTerm, buff, i );
      }
   }
}

static int zh_gt_trm_AnsiGetAcsc( PZH_GTTRM pTerm, unsigned char c )
{
   const unsigned char * ptr;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_AnsiGetAcsc(%p,%d)", ( void * ) pTerm, c ) );

   for( ptr = ( const unsigned char * ) pTerm->szAcsc; *ptr && *( ptr + 1 ); ptr += 2 )
   {
      if( *ptr == c )
         return *( ptr + 1 ) | ZH_GTTRM_ATTR_ACSC;
   }

   switch( c )
   {
      case '.':
         return 'v' | ZH_GTTRM_ATTR_STD;
      case ',':
         return '<' | ZH_GTTRM_ATTR_STD;
      case '+':
         return '>' | ZH_GTTRM_ATTR_STD;
      case '-':
         return '^' | ZH_GTTRM_ATTR_STD;
      case 'a':
         return '#' | ZH_GTTRM_ATTR_STD;
      case '0':
      case 'h':
         return zh_gt_trm_AnsiGetAcsc( pTerm, 'a' );
   }

   return c | ZH_GTTRM_ATTR_ALT;
}

static ZH_BOOL zh_gt_trm_AnsiSetMode( PZH_GTTRM pTerm, int * piRows, int * piCols )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_AnsiSetMode(%p,%d,%d)", ( void * ) pTerm, *piRows, *piCols ) );

   //if( pTerm->terminal_ext & TERM_PUTTY )
      return zh_gt_trm_XtermSetMode( pTerm, piRows, piCols );

   //return ZH_FALSE;
}

static void zh_gt_trm_AnsiBell( PZH_GTTRM pTerm )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_AnsiBell(%p)", ( void * ) pTerm ) );

   zh_gt_trm_termOut( pTerm, s_szBell, 1 );
   zh_gt_trm_termFlush( pTerm );
}

static void zh_gt_trm_AnsiTone( PZH_GTTRM pTerm, double dFrequency, double dDuration )
{
   double dCurrentSeconds;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_AnsiTone(%p,%lf,%lf)", ( void * ) pTerm, dFrequency, dDuration ) );

   /* Output an ASCII BEL character to cause a sound */
   /* but throttle to max once per second, in case of sound */
   /* effects prgs calling lots of short tone sequences in */
   /* succession leading to BEL hell on the terminal */

   dCurrentSeconds = zh_dateSeconds();
   if( dCurrentSeconds < pTerm->dToneSeconds ||
       dCurrentSeconds - pTerm->dToneSeconds > 0.5 )
   {
      zh_gt_trm_AnsiBell( pTerm );
      pTerm->dToneSeconds = dCurrentSeconds;
   }

   ZH_SYMBOL_UNUSED( dFrequency );

   zh_gtSleep( pTerm->pGT, dDuration / 18.2 );
}

static void zh_gt_trm_AnsiInit( PZH_GTTRM pTerm )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_AnsiInit(%p)", ( void * ) pTerm ) );

   pTerm->iCurrentSGR = pTerm->iRow = pTerm->iCol =
   pTerm->iCursorStyle = pTerm->iACSC = pTerm->iAM = -1;
}

static void zh_gt_trm_AnsiExit( PZH_GTTRM pTerm )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_AnsiExit(%p)", ( void * ) pTerm ) );

   /* set default color */
   pTerm->SetAttributes( pTerm, 0x07 & pTerm->iAttrMask );
   pTerm->SetCursorStyle( pTerm, SC_NORMAL );
   pTerm->SetTermMode( pTerm, 1 );
   zh_gt_trm_termOut( pTerm, "\x1B[m", 3 );
   if( pTerm->terminal_type == TERM_CONS )
      zh_gt_trm_termOut( pTerm, "\033[=4C", 5 );
}

/* ************************************************************************* */

/*
 * common functions
 */
static ZH_BOOL zh_trm_Param( const char * pszParam, int * piValue )
{
   ZH_BOOL fResult = ZH_FALSE;
   char * pszGtTrmParams = zh_cmdargString( "GTTRM" );

   if( pszGtTrmParams )
   {
      const char * pszAt = strstr( zh_strupr( pszGtTrmParams ), pszParam );

      if( pszAt != NULL )
      {
         fResult = ZH_TRUE;
         if( piValue )
         {
            int iOverflow;

            pszAt += strlen( pszParam );
            if( *pszAt == '=' || *pszAt == ':' )
               ++pszAt;
            * piValue = ZH_ISDIGIT( *pszAt ) ? zh_strValInt( pszAt, &iOverflow ) : 1;
         }
      }
      zh_xfree( pszGtTrmParams );
   }

   return fResult;
}

static ZH_BOOL zh_trm_isUTF8( PZH_GTTRM pTerm )
{
   ZH_BOOL fUTF8 = ZH_FALSE;
   char * szLang;

   if( pTerm->fPosAnswer )
   {
      zh_gt_trm_termOut( pTerm, "\005\r\303\255", 4 );
      if( pTerm->GetCursorPos( pTerm, &pTerm->iRow, &pTerm->iCol, "\r   \r" ) )
      {
         fUTF8 = pTerm->iCol == 1;
         pTerm->iCol = 0;
      }
   }

   if( zh_trm_Param( "UTF8", NULL ) || zh_trm_Param( "UTF-8", NULL ) )
      return ZH_TRUE;
   else if( zh_trm_Param( "ISO", NULL ) )
      return ZH_FALSE;
   else if( pTerm->fPosAnswer )
      return fUTF8;

   szLang = getenv( "LANG" );
   if( szLang && strstr( szLang, "UTF-8" ) != NULL )
      return ZH_TRUE;

#ifdef IUTF8
   if( ( pTerm->curr_TIO.c_iflag & IUTF8 ) != 0 )
      return ZH_TRUE;
#endif

   return ZH_FALSE;
}

static void zh_gt_trm_PutStr( PZH_GTTRM pTerm, int iRow, int iCol, int iAttr, const char * pStr, int iLen, int iChars )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_PutStr(%p,%d,%d,%d,%p,%d,%d)", ( void * ) pTerm, iRow, iCol, iAttr, ( const void * ) pStr, iLen, iChars ) );

   if( pTerm->iOutBufSize )
   {
      pTerm->SetCursorPos( pTerm, iRow, iCol );
      pTerm->SetAttributes( pTerm, iAttr & pTerm->iAttrMask );
#ifdef ZH_GT_UNICODE_BUF
      zh_gt_trm_termOut( pTerm, pStr, iLen );
#else
      zh_gt_trm_termOutTrans( pTerm, pStr, iLen, iAttr );
#endif
   }

   pTerm->iCol += iChars;
}

static void zh_gt_trm_SetPalette( PZH_GTTRM pTerm, int iIndexFrom, int iIndexTo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_SetPalette(%p,%d,%d)", ( void * ) pTerm, iIndexFrom, iIndexTo ) );

   if( pTerm->terminal_type == TERM_LINUX ||
       ( pTerm->terminal_ext & TERM_PUTTY ) )
   {
      zh_gt_trm_LinuxSetPalette( pTerm, iIndexFrom, iIndexTo );
   }
}

static void zh_gt_trm_ResetPalette( PZH_GTTRM pTerm )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_ResetPalette(%p)", ( void * ) pTerm ) );

   if( pTerm->terminal_type == TERM_LINUX ||
       ( pTerm->terminal_ext & TERM_PUTTY ) )
   {
      zh_gt_trm_LinuxResetPalette( pTerm );
   }
}

static void zh_gt_trm_SetTitle( PZH_GTTRM pTerm, const char * szTitle )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_SetTitle(%p,%s)", ( void * ) pTerm, szTitle ) );

   if( pTerm->terminal_type == TERM_XTERM ||
       ( pTerm->terminal_ext & TERM_PUTTY ) )
   {
      zh_gt_trm_XtermSetTitle( pTerm, szTitle );
   }
}

#ifndef ZH_GT_UNICODE_BUF
static void zh_gt_trm_SetKeyTrans( PZH_GTTRM pTerm )
{
   PZH_CODEPAGE cdpTerm = ZH_GTSELF_INCP( pTerm->pGT ),
                cdpHost = ZH_GTSELF_HOSTCP( pTerm->pGT );
   int i;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_SetKeyTrans(%p,%p,%p)", ( void * ) pTerm, ( void * ) cdpTerm, ( void * ) cdpHost ) );

   for( i = 0; i < 256; ++i )
      pTerm->keyTransTbl[ i ] = ( unsigned char )
                           zh_cdpTranslateChar( i, cdpTerm, cdpHost );
}
#endif

static void zh_gt_trm_SetDispTrans( PZH_GTTRM pTerm, int box )
{
   PZH_CODEPAGE cdpTerm = ZH_GTSELF_TERMCP( pTerm->pGT ),
                cdpHost = ZH_GTSELF_HOSTCP( pTerm->pGT );
   int i;

   memset( pTerm->chrattr, 0, sizeof( pTerm->chrattr ) );
   memset( pTerm->boxattr, 0, sizeof( pTerm->boxattr ) );

   for( i = 0; i < 256; i++ )
   {
      int ch = pTerm->charmap[ i ] & 0xffff;
      int mode = ! pTerm->fUTF8 ? ( pTerm->charmap[ i ] >> 16 ) & 0xff : 1;

      switch( mode )
      {
         case 1:
            pTerm->chrattr[ i ] = pTerm->boxattr[ i ] = ZH_GTTRM_ATTR_STD;
            break;
         case 2:
            pTerm->chrattr[ i ] = pTerm->boxattr[ i ] = ZH_GTTRM_ATTR_ALT;
            break;
         case 3:
            pTerm->chrattr[ i ] = pTerm->boxattr[ i ] = ZH_GTTRM_ATTR_PROT;
            break;
         case 4:
            pTerm->chrattr[ i ] = pTerm->boxattr[ i ] = ZH_GTTRM_ATTR_ALT | ZH_GTTRM_ATTR_PROT;
            break;
         case 5:
            ch = pTerm->GetAcsc( pTerm, ch & 0xff );
            pTerm->chrattr[ i ] = pTerm->boxattr[ i ] = ch & ~ZH_GTTRM_ATTR_CHAR;
            break;
         case 0:
         default:
            pTerm->chrattr[ i ] = ZH_GTTRM_ATTR_STD;
            pTerm->boxattr[ i ] = ZH_GTTRM_ATTR_ALT;
            break;
      }
      pTerm->chrattr[ i ] |= ch;
      pTerm->boxattr[ i ] |= ch;
   }

   if( cdpHost && cdpTerm )
   {
      for( i = 0; i < 256; ++i )
      {
         if( zh_cdpIsAlpha( cdpHost, i ) )
         {
            unsigned char uc = ( unsigned char )
                              zh_cdpTranslateDispChar( i, cdpHost, cdpTerm );

            pTerm->chrattr[ i ] = uc | ZH_GTTRM_ATTR_STD;
            if( box )
               pTerm->boxattr[ i ] = uc | ZH_GTTRM_ATTR_STD;
         }
      }
   }
}

static int addKeyMap( PZH_GTTRM pTerm, int nKey, const char * cdesc )
{
   int ret = K_UNDEF, i = 0, c;
   keyTab ** ptr;

   if( cdesc == NULL )
      return ret;

   c   = ( unsigned char ) cdesc[ i++ ];
   ptr = &pTerm->pKeyTab;

   while( c )
   {
      if( *ptr == NULL )
      {
         *ptr = ( keyTab * ) zh_xgrab( sizeof( keyTab ) );
         ( *ptr )->ch = c;
         ( *ptr )->key = K_UNDEF;
         ( *ptr )->nextCh = NULL;
         ( *ptr )->otherCh = NULL;
      }
      if( ( *ptr )->ch == c )
      {
         c = ( unsigned char ) cdesc[ i++ ];
         if( c )
            ptr = &( ( *ptr )->nextCh );
         else
         {
            ret = ( *ptr )->key;
            ( *ptr )->key = nKey;
         }
      }
      else
         ptr = &( ( *ptr )->otherCh );
   }
   return ret;
}

static int removeKeyMap( PZH_GTTRM pTerm, const char * cdesc )
{
   int ret = K_UNDEF, i = 0, c;
   keyTab ** ptr;

   c = ( unsigned char ) cdesc[ i++ ];
   ptr = &pTerm->pKeyTab;

   while( c && *ptr != NULL )
   {
      if( ( *ptr )->ch == c )
      {
         c = ( unsigned char ) cdesc[ i++ ];
         if( ! c )
         {
            ret = ( *ptr )->key;
            ( *ptr )->key = K_UNDEF;
            if( ( *ptr )->nextCh == NULL && ( *ptr )->otherCh == NULL )
            {
               zh_xfree( *ptr );
               *ptr = NULL;
            }
         }
         else
            ptr = &( ( *ptr )->nextCh );
      }
      else
         ptr = &( ( *ptr )->otherCh );
   }
   return ret;
}

static void removeAllKeyMap( PZH_GTTRM pTerm, keyTab ** ptr )
{
   if( ( *ptr )->nextCh != NULL )
      removeAllKeyMap( pTerm, &( ( *ptr )->nextCh ) );
   if( ( *ptr )->otherCh != NULL )
      removeAllKeyMap( pTerm, &( ( *ptr )->otherCh ) );

   zh_xfree( *ptr );
   *ptr = NULL;
}

static void addKeyTab( PZH_GTTRM pTerm, const keySeq * keys )
{
   while( keys->key )
   {
      addKeyMap( pTerm, keys->key, keys->seq );
      ++keys;
   }
}

static void init_keys( PZH_GTTRM pTerm )
{

   static const keySeq stdKeySeq[] = {
      /* virual CTRL/ALT sequences */
      { K_METACTRL  , CTRL_SEQ   },
      { K_METAALT   , ALT_SEQ    },
#ifdef NATION_SEQ
      /* national mode key sequences */
      { K_NATIONAL  , NATION_SEQ },
#endif
      { EXKEY_ENTER , "\r"       },
      /* terminal mouse event */
      { K_MOUSETERM , "\033[M"   },
      { 0, NULL } };

   static const keySeq stdFnKeySeq[] = {

      { EXKEY_F1,  "\033[11~" }, /* kf1  */
      { EXKEY_F2,  "\033[12~" }, /* kf2  */
      { EXKEY_F3,  "\033[13~" }, /* kf3  */
      { EXKEY_F4,  "\033[14~" }, /* kf4  */
      { EXKEY_F5,  "\033[15~" }, /* kf5  */

      { EXKEY_F6 , "\033[17~" }, /* kf6  */
      { EXKEY_F7 , "\033[18~" }, /* kf7  */
      { EXKEY_F8 , "\033[19~" }, /* kf8  */
      { EXKEY_F9 , "\033[20~" }, /* kf9  */
      { EXKEY_F10, "\033[21~" }, /* kf10 */
      { EXKEY_F11, "\033[23~" }, /* kf11 */
      { EXKEY_F12, "\033[24~" }, /* kf12 */

      { EXKEY_F1 |KEY_SHIFTMASK, "\033[25~" }, /* kf13 */
      { EXKEY_F2 |KEY_SHIFTMASK, "\033[26~" }, /* kf14 */
      { EXKEY_F3 |KEY_SHIFTMASK, "\033[28~" }, /* kf15 */
      { EXKEY_F4 |KEY_SHIFTMASK, "\033[29~" }, /* kf16 */
      { EXKEY_F5 |KEY_SHIFTMASK, "\033[31~" }, /* kf17 */
      { EXKEY_F6 |KEY_SHIFTMASK, "\033[32~" }, /* kf18 */
      { EXKEY_F7 |KEY_SHIFTMASK, "\033[33~" }, /* kf19 */
      { EXKEY_F8 |KEY_SHIFTMASK, "\033[34~" }, /* kf20 */
      { EXKEY_F9 |KEY_SHIFTMASK, "\033[35~" }, /* kf21 */
      { EXKEY_F10|KEY_SHIFTMASK, "\033[36~" }, /* kf22 */
      { EXKEY_F11|KEY_SHIFTMASK, "\033[37~" }, /* kf23 */
      { EXKEY_F12|KEY_SHIFTMASK, "\033[38~" }, /* kf24 */

      { EXKEY_F1 |KEY_CTRLMASK, "\033[39~" }, /* kf25 */
      { EXKEY_F2 |KEY_CTRLMASK, "\033[40~" }, /* kf26 */
      { EXKEY_F3 |KEY_CTRLMASK, "\033[41~" }, /* kf27 */
      { EXKEY_F4 |KEY_CTRLMASK, "\033[42~" }, /* kf28 */
      { EXKEY_F5 |KEY_CTRLMASK, "\033[43~" }, /* kf29 */
      { EXKEY_F6 |KEY_CTRLMASK, "\033[44~" }, /* kf30 */
      { EXKEY_F7 |KEY_CTRLMASK, "\033[45~" }, /* kf31 */
      { EXKEY_F8 |KEY_CTRLMASK, "\033[46~" }, /* kf32 */
      { EXKEY_F9 |KEY_CTRLMASK, "\033[47~" }, /* kf33 */
      { EXKEY_F10|KEY_CTRLMASK, "\033[48~" }, /* kf34 */
      { EXKEY_F11|KEY_CTRLMASK, "\033[49~" }, /* kf35 */
      { EXKEY_F12|KEY_CTRLMASK, "\033[50~" }, /* kf36 */

      { EXKEY_F1 |KEY_ALTMASK , "\033[51~" }, /* kf37 */
      { EXKEY_F2 |KEY_ALTMASK , "\033[52~" }, /* kf38 */
      { EXKEY_F3 |KEY_ALTMASK , "\033[53~" }, /* kf39 */
      { EXKEY_F4 |KEY_ALTMASK , "\033[54~" }, /* kf40 */
      { EXKEY_F5 |KEY_ALTMASK , "\033[55~" }, /* kf41 */
      { EXKEY_F6 |KEY_ALTMASK , "\033[56~" }, /* kf42 */
      { EXKEY_F7 |KEY_ALTMASK , "\033[57~" }, /* kf43 */
      { EXKEY_F8 |KEY_ALTMASK , "\033[58~" }, /* kf44 */
      { EXKEY_F9 |KEY_ALTMASK , "\033[59~" }, /* kf45 */
      { EXKEY_F10|KEY_ALTMASK , "\033[70~" }, /* kf46 */
      { EXKEY_F11|KEY_ALTMASK , "\033[71~" }, /* kf47 */
      { EXKEY_F12|KEY_ALTMASK , "\033[72~" }, /* kf48 */

      { 0, NULL } };

   static const keySeq stdCursorKeySeq[] = {
      { EXKEY_HOME,   "\033[1~" }, /* khome */
      { EXKEY_INS,    "\033[2~" }, /* kich1 */
      { EXKEY_DEL,    "\033[3~" }, /* kdch1 */
      { EXKEY_END,    "\033[4~" }, /* kend  */
      { EXKEY_PGUP,   "\033[5~" }, /* kpp   */
      { EXKEY_PGDN,   "\033[6~" }, /* knp   */

      { 0, NULL } };

   static const keySeq puttyKeySeq[] = {
      /* In XTerm (XFree 3.x.x) they are without CTRL,
         kcuu1, kcud1, kcuf1, kcub1 */
      { EXKEY_UP    |KEY_CTRLMASK, "\033OA" },
      { EXKEY_DOWN  |KEY_CTRLMASK, "\033OB" },
      { EXKEY_RIGHT |KEY_CTRLMASK, "\033OC" },
      { EXKEY_LEFT  |KEY_CTRLMASK, "\033OD" },

      { EXKEY_CENTER|KEY_CTRLMASK, "\033OG" },

      { 0, NULL } };


   static const keySeq haikuCtrlKeySeq[] = {
      /* HAIKU/BEOS XTerm CTRL + {UP,DOWN,RIGHT,LEFT} kyes */
      { EXKEY_UP    |KEY_CTRLMASK, "\033O5A" },
      { EXKEY_DOWN  |KEY_CTRLMASK, "\033O5B" },
      { EXKEY_RIGHT |KEY_CTRLMASK, "\033O5C" },
      { EXKEY_LEFT  |KEY_CTRLMASK, "\033O5D" },

      { 0, NULL } };

   static const keySeq rxvtKeySeq[] = {

      { EXKEY_HOME,     "\033[H" },
      { EXKEY_END,      "\033Ow" },

      { 0, NULL } };

   static const keySeq cygwinModKeySeq[] = {
      { EXKEY_F1 |KEY_CTRLMASK, "\033[11^" },
      { EXKEY_F2 |KEY_CTRLMASK, "\033[12^" },
      { EXKEY_F3 |KEY_CTRLMASK, "\033[13^" },
      { EXKEY_F4 |KEY_CTRLMASK, "\033[14^" },
      { EXKEY_F5 |KEY_CTRLMASK, "\033[15^" },
      { EXKEY_F6 |KEY_CTRLMASK, "\033[17^" },
      { EXKEY_F7 |KEY_CTRLMASK, "\033[18^" },
      { EXKEY_F8 |KEY_CTRLMASK, "\033[19^" },
      { EXKEY_F9 |KEY_CTRLMASK, "\033[20^" },
      { EXKEY_F10|KEY_CTRLMASK, "\033[21^" },
      { EXKEY_F11|KEY_CTRLMASK, "\033[23^" },
      { EXKEY_F12|KEY_CTRLMASK, "\033[24^" },

      { EXKEY_F11|KEY_SHIFTMASK, "\033[23$" },
      { EXKEY_F12|KEY_SHIFTMASK, "\033[24$" },

      { 0, NULL } };

   static const keySeq xtermModKeySeq[] = {
      /* XTerm  with modifiers */
      { EXKEY_F1 |KEY_CTRLMASK, "\033O5P" },
      { EXKEY_F2 |KEY_CTRLMASK, "\033O5Q" },
      { EXKEY_F3 |KEY_CTRLMASK, "\033O5R" },
      { EXKEY_F4 |KEY_CTRLMASK, "\033O5S" },

      { EXKEY_F1 |KEY_CTRLMASK, "\033[11;5~" },
      { EXKEY_F2 |KEY_CTRLMASK, "\033[12;5~" },
      { EXKEY_F3 |KEY_CTRLMASK, "\033[13;5~" },
      { EXKEY_F4 |KEY_CTRLMASK, "\033[14;5~" },
      { EXKEY_F5 |KEY_CTRLMASK, "\033[15;5~" },
      { EXKEY_F6 |KEY_CTRLMASK, "\033[17;5~" },
      { EXKEY_F7 |KEY_CTRLMASK, "\033[18;5~" },
      { EXKEY_F8 |KEY_CTRLMASK, "\033[19;5~" },
      { EXKEY_F9 |KEY_CTRLMASK, "\033[20;5~" },
      { EXKEY_F10|KEY_CTRLMASK, "\033[21;5~" },
      { EXKEY_F11|KEY_CTRLMASK, "\033[23;5~" },
      { EXKEY_F12|KEY_CTRLMASK, "\033[24;5~" },

      { EXKEY_HOME  |KEY_CTRLMASK, "\033[1;5~" },
      { EXKEY_INS   |KEY_CTRLMASK, "\033[2;5~" },
      { EXKEY_DEL   |KEY_CTRLMASK, "\033[3;5~" },
      { EXKEY_END   |KEY_CTRLMASK, "\033[4;5~" },
      { EXKEY_PGUP  |KEY_CTRLMASK, "\033[5;5~" },
      { EXKEY_PGDN  |KEY_CTRLMASK, "\033[6;5~" },

      { EXKEY_UP    |KEY_CTRLMASK, "\033[1;5A" },
      { EXKEY_DOWN  |KEY_CTRLMASK, "\033[1;5B" },
      { EXKEY_RIGHT |KEY_CTRLMASK, "\033[1;5C" },
      { EXKEY_LEFT  |KEY_CTRLMASK, "\033[1;5D" },
      { EXKEY_CENTER|KEY_CTRLMASK, "\033[1;5E" },
      { EXKEY_END   |KEY_CTRLMASK, "\033[1;5F" },
      { EXKEY_CENTER|KEY_CTRLMASK, "\033[1;5G" },
      { EXKEY_HOME  |KEY_CTRLMASK, "\033[1;5H" },

      { EXKEY_UP    |KEY_CTRLMASK, "\033[5A" },
      { EXKEY_DOWN  |KEY_CTRLMASK, "\033[5B" },
      { EXKEY_RIGHT |KEY_CTRLMASK, "\033[5C" },
      { EXKEY_LEFT  |KEY_CTRLMASK, "\033[5D" },
      { EXKEY_CENTER|KEY_CTRLMASK, "\033[5E" }, /* --- */
      { EXKEY_END   |KEY_CTRLMASK, "\033[5F" }, /* --- */
      { EXKEY_CENTER|KEY_CTRLMASK, "\033[5G" }, /* --- */
      { EXKEY_HOME  |KEY_CTRLMASK, "\033[5H" }, /* --- */

      { EXKEY_F1 |KEY_ALTMASK, "\033O3P" },
      { EXKEY_F2 |KEY_ALTMASK, "\033O3Q" },
      { EXKEY_F3 |KEY_ALTMASK, "\033O3R" },
      { EXKEY_F4 |KEY_ALTMASK, "\033O3S" },

      { EXKEY_F1 |KEY_ALTMASK, "\033[11;3~" },
      { EXKEY_F2 |KEY_ALTMASK, "\033[12;3~" },
      { EXKEY_F3 |KEY_ALTMASK, "\033[13;3~" },
      { EXKEY_F4 |KEY_ALTMASK, "\033[14;3~" },
      { EXKEY_F5 |KEY_ALTMASK, "\033[15;3~" },
      { EXKEY_F6 |KEY_ALTMASK, "\033[17;3~" },
      { EXKEY_F7 |KEY_ALTMASK, "\033[18;3~" },
      { EXKEY_F8 |KEY_ALTMASK, "\033[19;3~" },
      { EXKEY_F9 |KEY_ALTMASK, "\033[20;3~" },
      { EXKEY_F10|KEY_ALTMASK, "\033[21;3~" },
      { EXKEY_F11|KEY_ALTMASK, "\033[23;3~" },
      { EXKEY_F12|KEY_ALTMASK, "\033[24;3~" },

      { EXKEY_HOME  |KEY_ALTMASK, "\033[1;3~" },
      { EXKEY_INS   |KEY_ALTMASK, "\033[2;3~" },
      { EXKEY_DEL   |KEY_ALTMASK, "\033[3;3~" },
      { EXKEY_END   |KEY_ALTMASK, "\033[4;3~" },
      { EXKEY_PGUP  |KEY_ALTMASK, "\033[5;3~" },
      { EXKEY_PGDN  |KEY_ALTMASK, "\033[6;3~" },

      { EXKEY_UP    |KEY_ALTMASK, "\033[1;3A" },
      { EXKEY_DOWN  |KEY_ALTMASK, "\033[1;3B" },
      { EXKEY_RIGHT |KEY_ALTMASK, "\033[1;3C" },
      { EXKEY_LEFT  |KEY_ALTMASK, "\033[1;3D" },
      { EXKEY_CENTER|KEY_ALTMASK, "\033[1;3E" },
      { EXKEY_END   |KEY_ALTMASK, "\033[1;3F" },
      { EXKEY_CENTER|KEY_ALTMASK, "\033[1;3G" },
      { EXKEY_HOME  |KEY_ALTMASK, "\033[1;3H" },

      { EXKEY_UP    |KEY_ALTMASK, "\033[3A" },
      { EXKEY_DOWN  |KEY_ALTMASK, "\033[3B" },
      { EXKEY_RIGHT |KEY_ALTMASK, "\033[3C" },
      { EXKEY_LEFT  |KEY_ALTMASK, "\033[3D" },
      { EXKEY_CENTER|KEY_ALTMASK, "\033[3E" }, /* --- */
      { EXKEY_END   |KEY_ALTMASK, "\033[3F" }, /* --- */
      { EXKEY_CENTER|KEY_ALTMASK, "\033[3G" }, /* --- */
      { EXKEY_HOME  |KEY_ALTMASK, "\033[3H" }, /* --- */

      { EXKEY_F1 |KEY_SHIFTMASK, "\033O2P" },
      { EXKEY_F2 |KEY_SHIFTMASK, "\033O2Q" },
      { EXKEY_F3 |KEY_SHIFTMASK, "\033O2R" },
      { EXKEY_F4 |KEY_SHIFTMASK, "\033O2S" },

      { EXKEY_F1 |KEY_SHIFTMASK, "\033O1;2P" },
      { EXKEY_F2 |KEY_SHIFTMASK, "\033O1;2Q" },
      { EXKEY_F3 |KEY_SHIFTMASK, "\033O1;2R" },
      { EXKEY_F4 |KEY_SHIFTMASK, "\033O1;2S" },

      { EXKEY_F1 |KEY_SHIFTMASK, "\033[1;2P" },
      { EXKEY_F2 |KEY_SHIFTMASK, "\033[1;2Q" },
      { EXKEY_F3 |KEY_SHIFTMASK, "\033[1;2R" },
      { EXKEY_F4 |KEY_SHIFTMASK, "\033[1;2S" },

      { EXKEY_F1 |KEY_SHIFTMASK, "\033[11;2~" },
      { EXKEY_F2 |KEY_SHIFTMASK, "\033[12;2~" },
      { EXKEY_F3 |KEY_SHIFTMASK, "\033[13;2~" },
      { EXKEY_F4 |KEY_SHIFTMASK, "\033[14;2~" },
      { EXKEY_F5 |KEY_SHIFTMASK, "\033[15;2~" },
      { EXKEY_F6 |KEY_SHIFTMASK, "\033[17;2~" },
      { EXKEY_F7 |KEY_SHIFTMASK, "\033[18;2~" },
      { EXKEY_F8 |KEY_SHIFTMASK, "\033[19;2~" },
      { EXKEY_F9 |KEY_SHIFTMASK, "\033[20;2~" },
      { EXKEY_F10|KEY_SHIFTMASK, "\033[21;2~" },
      { EXKEY_F11|KEY_SHIFTMASK, "\033[23;2~" },
      { EXKEY_F12|KEY_SHIFTMASK, "\033[24;2~" },

      { EXKEY_HOME  |KEY_SHIFTMASK, "\033[1;2~" },
      { EXKEY_INS   |KEY_SHIFTMASK, "\033[2;2~" },
      { EXKEY_DEL   |KEY_SHIFTMASK, "\033[3;2~" },
      { EXKEY_END   |KEY_SHIFTMASK, "\033[4;2~" },
      { EXKEY_PGUP  |KEY_SHIFTMASK, "\033[5;2~" },
      { EXKEY_PGDN  |KEY_SHIFTMASK, "\033[6;2~" },

      { EXKEY_UP    |KEY_SHIFTMASK, "\033[1;2A" },
      { EXKEY_DOWN  |KEY_SHIFTMASK, "\033[1;2B" },
      { EXKEY_RIGHT |KEY_SHIFTMASK, "\033[1;2C" },
      { EXKEY_LEFT  |KEY_SHIFTMASK, "\033[1;2D" },
      { EXKEY_CENTER|KEY_SHIFTMASK, "\033[1;2E" },
      { EXKEY_END   |KEY_SHIFTMASK, "\033[1;2F" },
      { EXKEY_CENTER|KEY_SHIFTMASK, "\033[1;2G" },
      { EXKEY_HOME  |KEY_SHIFTMASK, "\033[1;2H" },

      { EXKEY_UP    |KEY_SHIFTMASK, "\033[2A" },
      { EXKEY_DOWN  |KEY_SHIFTMASK, "\033[2B" },
      { EXKEY_RIGHT |KEY_SHIFTMASK, "\033[2C" },
      { EXKEY_LEFT  |KEY_SHIFTMASK, "\033[2D" },
      { EXKEY_CENTER|KEY_SHIFTMASK, "\033[2E" }, /* --- */
      { EXKEY_END   |KEY_SHIFTMASK, "\033[2F" }, /* --- */
      { EXKEY_CENTER|KEY_SHIFTMASK, "\033[2G" }, /* --- */
      { EXKEY_HOME  |KEY_SHIFTMASK, "\033[2H" }, /* --- */

      { EXKEY_BS |KEY_ALTMASK,     "\033\010" },

      { 0, NULL } };

   static const keySeq xtermFnKeySeq[] = {

      { EXKEY_F1, "\033OP" }, /* kf1  */
      { EXKEY_F2, "\033OQ" }, /* kf2  */
      { EXKEY_F3, "\033OR" }, /* kf3  */
      { EXKEY_F4, "\033OS" }, /* kf4  */

      { 0, NULL } };

   static const keySeq xtermKeySeq[] = {

      { EXKEY_BS,     "\010" }, /* kbs   */
      { EXKEY_TAB,    "\011" }, /* ht    */
      { EXKEY_BS    , "\177" },

      /* cursor keys */
      { EXKEY_UP    , "\033[A" },
      { EXKEY_DOWN  , "\033[B" },
      { EXKEY_RIGHT , "\033[C" },
      { EXKEY_LEFT  , "\033[D" },

      { EXKEY_CENTER, "\033[E" }, /* XTerm */
      { EXKEY_END   , "\033[F" }, /* XTerm */
      { EXKEY_CENTER, "\033[G" }, /* PuTTY */
      { EXKEY_HOME  , "\033[H" }, /* XTerm */

      { EXKEY_TAB   |KEY_SHIFTMASK, "\033[Z" }, /* kcbt, XTerm */

      /* Konsole */
      { EXKEY_ENTER |KEY_SHIFTMASK, "\033OM" },

      { EXKEY_END,    "\033Ow" },  /* rxvt */

      /* gnome-terminal */
      { EXKEY_END,    "\033OF"  }, /* kend  */
      { EXKEY_HOME,   "\033OH"  }, /* khome */
      { EXKEY_ENTER |KEY_ALTMASK, "\033\012" },

      { 0, NULL } };

   static const keySeq linuxKeySeq[] = {

      { EXKEY_TAB,    "\011"    }, /* ht    */
      { EXKEY_BS,     "\177"    }, /* kbs   */

      { EXKEY_UP,     "\033[A"  }, /* kcuu1 */
      { EXKEY_DOWN,   "\033[B"  }, /* kcud1 */
      { EXKEY_RIGHT,  "\033[C"  }, /* kcuf1 */
      { EXKEY_LEFT,   "\033[D"  }, /* kcub1 */
      { EXKEY_CENTER, "\033[G"  }, /* kb2 */

      { EXKEY_F1,     "\033[[A" }, /* kf1  */
      { EXKEY_F2,     "\033[[B" }, /* kf2  */
      { EXKEY_F3,     "\033[[C" }, /* kf3  */
      { EXKEY_F4,     "\033[[D" }, /* kf4  */
      { EXKEY_F5,     "\033[[E" }, /* kf5  */

      { EXKEY_TAB | KEY_ALTMASK, "\033[Z" }, /* kcbt */

      { 0, NULL } };

   static const keySeq ansiKeySeq[] = {

      { EXKEY_BS,     "\010"   }, /* kbs   */
      { EXKEY_TAB,    "\011"   }, /* ht    */
      { EXKEY_DEL,    "\177"   }, /* kdch1 */
      /* cursor keys */
      { EXKEY_UP,     "\033[A" }, /* kcuu1 */
      { EXKEY_DOWN,   "\033[B" }, /* kcud1 */
      { EXKEY_RIGHT,  "\033[C" }, /* kcuf1 */
      { EXKEY_LEFT,   "\033[D" }, /* kcub1 */
      { EXKEY_CENTER, "\033[E" }, /* kb2   */
      { EXKEY_END,    "\033[F" }, /* kend  */
      { EXKEY_PGDN,   "\033[G" }, /* knp   */
      { EXKEY_HOME,   "\033[H" }, /* khome */
      { EXKEY_PGUP,   "\033[I" }, /* kpp   */
      { EXKEY_INS,    "\033[L" }, /* kich1 */

      { EXKEY_F1 , "\033[M" }, /* kf1  */
      { EXKEY_F2 , "\033[N" }, /* kf2  */
      { EXKEY_F3 , "\033[O" }, /* kf3  */
      { EXKEY_F4 , "\033[P" }, /* kf4  */
      { EXKEY_F5 , "\033[Q" }, /* kf5  */
      { EXKEY_F6 , "\033[R" }, /* kf6  */
      { EXKEY_F7 , "\033[S" }, /* kf7  */
      { EXKEY_F8 , "\033[T" }, /* kf8  */
      { EXKEY_F9 , "\033[U" }, /* kf9  */
      { EXKEY_F10, "\033[V" }, /* kf10 */
      { EXKEY_F11, "\033[W" }, /* kf11 */
      { EXKEY_F12, "\033[X" }, /* kf12 */

      { EXKEY_F1 |KEY_SHIFTMASK, "\033[Y" }, /* kf13 */
      { EXKEY_F2 |KEY_SHIFTMASK, "\033[Z" }, /* kf14 */
      { EXKEY_F3 |KEY_SHIFTMASK, "\033[a" }, /* kf15 */
      { EXKEY_F4 |KEY_SHIFTMASK, "\033[b" }, /* kf16 */
      { EXKEY_F5 |KEY_SHIFTMASK, "\033[c" }, /* kf17 */
      { EXKEY_F6 |KEY_SHIFTMASK, "\033[d" }, /* kf18 */
      { EXKEY_F7 |KEY_SHIFTMASK, "\033[e" }, /* kf19 */
      { EXKEY_F8 |KEY_SHIFTMASK, "\033[f" }, /* kf20 */
      { EXKEY_F9 |KEY_SHIFTMASK, "\033[g" }, /* kf21 */
      { EXKEY_F10|KEY_SHIFTMASK, "\033[h" }, /* kf22 */
      { EXKEY_F11|KEY_SHIFTMASK, "\033[i" }, /* kf23 */
      { EXKEY_F12|KEY_SHIFTMASK, "\033[j" }, /* kf24 */

      { EXKEY_F1 |KEY_CTRLMASK, "\033[k" },        /* kf25 */
      { EXKEY_F2 |KEY_CTRLMASK, "\033[l" },        /* kf26 */
      { EXKEY_F3 |KEY_CTRLMASK, "\033[m" },        /* kf27 */
      { EXKEY_F4 |KEY_CTRLMASK, "\033[n" },        /* kf28 */
      { EXKEY_F5 |KEY_CTRLMASK, "\033[o" },        /* kf29 */
      { EXKEY_F6 |KEY_CTRLMASK, "\033[p" },        /* kf30 */
      { EXKEY_F7 |KEY_CTRLMASK, "\033[q" },        /* kf31 */
      { EXKEY_F8 |KEY_CTRLMASK, "\033[r" },        /* kf32 */
      { EXKEY_F9 |KEY_CTRLMASK, "\033[s" },        /* kf33 */
      { EXKEY_F10|KEY_CTRLMASK, "\033[t" },        /* kf34 */
      { EXKEY_F11|KEY_CTRLMASK, "\033[u" },        /* kf35 */
      { EXKEY_F12|KEY_CTRLMASK, "\033[v" },        /* kf36 */

      { EXKEY_F1 |KEY_ALTMASK , "\033[w" },        /* kf37 */
      { EXKEY_F2 |KEY_ALTMASK , "\033[x" },        /* kf38 */
      { EXKEY_F3 |KEY_ALTMASK , "\033[y" },        /* kf39 */
      { EXKEY_F4 |KEY_ALTMASK , "\033[z" },        /* kf40 */
      { EXKEY_F5 |KEY_ALTMASK , "\033[@" },        /* kf41 */
      { EXKEY_F6 |KEY_ALTMASK , "\033[[" },        /* kf42 */
      { EXKEY_F7 |KEY_ALTMASK , "\033[\\"},        /* kf43 */
      { EXKEY_F8 |KEY_ALTMASK , "\033[]" },        /* kf44 */
      { EXKEY_F9 |KEY_ALTMASK , "\033[^" },        /* kf45 */
      { EXKEY_F10|KEY_ALTMASK , "\033[_" },        /* kf46 */
      { EXKEY_F11|KEY_ALTMASK , "\033[`" },        /* kf47 */
      { EXKEY_F12|KEY_ALTMASK , "\033[{" },        /* kf48 */

      { 0, NULL } };

   static const keySeq bsdConsKeySeq[] = {

      { EXKEY_TAB   |KEY_SHIFTMASK, "\033[Z" }, /* SHIFT+TAB */

      { 0, NULL } };


   addKeyTab( pTerm, stdKeySeq );
   if( pTerm->terminal_type == TERM_XTERM )
   {
      addKeyTab( pTerm, xtermKeySeq );
      addKeyTab( pTerm, xtermFnKeySeq );
      addKeyTab( pTerm, stdFnKeySeq );
      addKeyTab( pTerm, stdCursorKeySeq );
      addKeyTab( pTerm, xtermModKeySeq );
      addKeyTab( pTerm, puttyKeySeq );
      addKeyTab( pTerm, haikuCtrlKeySeq );
   }
   else if( pTerm->terminal_type == TERM_LINUX ||
            pTerm->terminal_type == TERM_CYGWIN )
   {
      addKeyTab( pTerm, linuxKeySeq );
      addKeyTab( pTerm, stdFnKeySeq );
      addKeyTab( pTerm, stdCursorKeySeq );
      addKeyTab( pTerm, xtermFnKeySeq );
      addKeyTab( pTerm, xtermModKeySeq );
      addKeyTab( pTerm, puttyKeySeq );
      /* if( pTerm->terminal_ext & TERM_PUTTY ) for PuTTY */
      addKeyTab( pTerm, rxvtKeySeq );
      if( pTerm->terminal_type == TERM_CYGWIN )
         addKeyTab( pTerm, cygwinModKeySeq );
   }
   else if( pTerm->terminal_type == TERM_CONS )
   {
      addKeyTab( pTerm, ansiKeySeq );
      addKeyTab( pTerm, bsdConsKeySeq );
   }
   else if( pTerm->terminal_type == TERM_ANSI )
   {
      addKeyTab( pTerm, ansiKeySeq );
   }

}

static void zh_gt_trm_SetTerm( PZH_GTTRM pTerm )
{
   static const char * szAcsc = "``aaffggiijjkkllmmnnooppqqrrssttuuvvwwxxyyzz{{||}}~~";
   static const char * szExtAcsc = "+\020,\021-\030.\0310\333`\004a\261f\370g\361h\260i\316j\331k\277l\332m\300n\305o~p\304q\304r\304s_t\303u\264v\301w\302x\263y\363z\362{\343|\330}\234~\376";
   const char * szTerm;
   int iValue;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_SetTerm(%p)", ( void * ) pTerm ) );

   if( pTerm->iOutBufSize == 0 )
   {
      pTerm->iOutBufIndex = 0;
      pTerm->iOutBufSize = 16384;
      pTerm->pOutBuf = ( char * ) zh_xgrab( pTerm->iOutBufSize );
   }
   pTerm->mouse_type    = MOUSE_NONE;
   pTerm->esc_delay     = ESC_DELAY;
   pTerm->iAttrMask     = ~ZH_GTTRM_ATTR_BOX;
   pTerm->iExtColor     = ZH_GTTRM_CLRSTD;
   pTerm->terminal_ext  = 0;
   pTerm->fAM           = ZH_FALSE;

   /* standard VGA colors */
   pTerm->colors[ 0x00 ] = 0x000000;
   pTerm->colors[ 0x01 ] = 0xAA0000;
   pTerm->colors[ 0x02 ] = 0x00AA00;
   pTerm->colors[ 0x03 ] = 0xAAAA00;
   pTerm->colors[ 0x04 ] = 0x0000AA;
   pTerm->colors[ 0x05 ] = 0xAA00AA;
   pTerm->colors[ 0x06 ] = 0x0055AA;
   pTerm->colors[ 0x07 ] = 0xAAAAAA;
   pTerm->colors[ 0x08 ] = 0x555555;
   pTerm->colors[ 0x09 ] = 0xFF5555;
   pTerm->colors[ 0x0A ] = 0x55FF55;
   pTerm->colors[ 0x0B ] = 0xFFFF55;
   pTerm->colors[ 0x0C ] = 0x5555FF;
   pTerm->colors[ 0x0D ] = 0xFF55FF;
   pTerm->colors[ 0x0E ] = 0x55FFFF;
   pTerm->colors[ 0x0F ] = 0xFFFFFF;

   if( zh_trm_Param( "PUTTY", NULL ) )
      pTerm->terminal_ext |= TERM_PUTTY;
   if( zh_trm_Param( "EXCLR", &iValue ) )
   {
      switch( iValue )
      {
         case ZH_GTTRM_CLRSTD:
         case ZH_GTTRM_CLRX16:
         case ZH_GTTRM_CLR256:
         case ZH_GTTRM_CLRRGB:
         case ZH_GTTRM_CLRAIX:
            pTerm->iExtColor = iValue;
            break;
      }
   }

   if( zh_trm_Param( "XTERM", NULL ) )
      szTerm = "xterm";
   else if( zh_trm_Param( "LINUX", NULL ) )
      szTerm = "linux";
   else if( zh_trm_Param( "CONS", NULL ) )
      szTerm = "cons";
   else if( zh_trm_Param( "ANSI", NULL ) )
      szTerm = "ansi";
   else
   {
      szTerm = getenv( "ZH_TERM" );
      if( szTerm == NULL || *szTerm == '\0' )
      {
         szTerm = getenv( "TERM" );
         if( szTerm == NULL || *szTerm == '\0' )
            szTerm = "ansi";
      }
   }

   if( ( pTerm->terminal_ext & TERM_PUTTY ) ||
       strstr( szTerm, "xterm" ) != NULL ||
       strncmp( szTerm, "rxvt", 4 ) == 0 ||
       strcmp( szTerm, "putty" ) == 0 ||
       strncmp( szTerm, "screen", 6 ) == 0 )
   {
      pTerm->Init           = zh_gt_trm_AnsiInit;
      pTerm->Exit           = zh_gt_trm_AnsiExit;
      pTerm->SetTermMode    = zh_gt_trm_LinuxSetTermMode;
      pTerm->GetCursorPos   = zh_gt_trm_AnsiGetCursorPos;
      pTerm->SetCursorPos   = zh_gt_trm_AnsiSetCursorPos;
      pTerm->SetCursorStyle = zh_gt_trm_AnsiSetCursorStyle;
      pTerm->SetAttributes  = zh_gt_trm_XtermSetAttributes;
      pTerm->SetMode        = zh_gt_trm_XtermSetMode;
      pTerm->GetAcsc        = zh_gt_trm_AnsiGetAcsc;
      pTerm->Tone           = zh_gt_trm_AnsiTone;
      pTerm->Bell           = zh_gt_trm_AnsiBell;
      pTerm->szAcsc         = szAcsc;
      pTerm->terminal_type  = TERM_XTERM;
   }
   else if( strncmp( szTerm, "linux", 5 ) == 0 ||
            strcmp( szTerm, "cygwin" ) == 0 ||
            strcmp( szTerm, "tterm" ) == 0 ||
            strcmp( szTerm, "teraterm" ) == 0 )
   {
      pTerm->Init           = zh_gt_trm_AnsiInit;
      pTerm->Exit           = zh_gt_trm_AnsiExit;
      pTerm->SetTermMode    = zh_gt_trm_LinuxSetTermMode;
      pTerm->GetCursorPos   = zh_gt_trm_AnsiGetCursorPos;
      pTerm->SetCursorPos   = zh_gt_trm_AnsiSetCursorPos;
      pTerm->SetCursorStyle = zh_gt_trm_LinuxSetCursorStyle;
      pTerm->SetAttributes  = zh_gt_trm_AnsiSetAttributes;
      pTerm->SetMode        = zh_gt_trm_AnsiSetMode;
      pTerm->GetAcsc        = zh_gt_trm_AnsiGetAcsc;
      pTerm->Tone           = zh_gt_trm_LinuxTone;
      pTerm->Bell           = zh_gt_trm_AnsiBell;
      pTerm->szAcsc         = szExtAcsc;
      if( strcmp( szTerm, "cygwin" ) == 0 )
      {
         pTerm->terminal_type  = TERM_CYGWIN;
         pTerm->fAM            = ZH_TRUE;
      }
      else
         pTerm->terminal_type  = TERM_LINUX;
   }
   else if( strncmp( szTerm, "cons", 4 ) == 0 )
   {
      pTerm->Init           = zh_gt_trm_AnsiInit;
      pTerm->Exit           = zh_gt_trm_AnsiExit;
      pTerm->SetTermMode    = zh_gt_trm_AnsiSetTermMode;
      pTerm->GetCursorPos   = zh_gt_trm_BsdGetCursorPos;
      pTerm->SetCursorPos   = zh_gt_trm_AnsiSetCursorPos;
      pTerm->SetCursorStyle = zh_gt_trm_BsdSetCursorStyle;
      pTerm->SetAttributes  = zh_gt_trm_AnsiSetAttributes;
      pTerm->SetMode        = zh_gt_trm_AnsiSetMode;
      pTerm->GetAcsc        = zh_gt_trm_AnsiGetAcsc;
      pTerm->Tone           = zh_gt_trm_BsdTone;
      pTerm->Bell           = zh_gt_trm_AnsiBell;
      pTerm->szAcsc         = szExtAcsc;
      pTerm->terminal_type  = TERM_CONS;
      pTerm->fAM            = ZH_TRUE;
   }
   else
   {
      pTerm->Init           = zh_gt_trm_AnsiInit;
      pTerm->Exit           = zh_gt_trm_AnsiExit;
      pTerm->SetTermMode    = zh_gt_trm_AnsiSetTermMode;
      pTerm->GetCursorPos   = zh_gt_trm_AnsiGetCursorPos;
      pTerm->SetCursorPos   = zh_gt_trm_AnsiSetCursorPos;
      pTerm->SetCursorStyle = zh_gt_trm_AnsiSetCursorStyle;
      pTerm->SetAttributes  = zh_gt_trm_AnsiSetAttributes;
      pTerm->SetMode        = zh_gt_trm_AnsiSetMode;
      pTerm->GetAcsc        = zh_gt_trm_AnsiGetAcsc;
      pTerm->Tone           = zh_gt_trm_AnsiTone;
      pTerm->Bell           = zh_gt_trm_AnsiBell;
      pTerm->szAcsc         = szExtAcsc;
      pTerm->terminal_type  = TERM_ANSI;
   }

   pTerm->fStdinTTY      = zh_fsIsDevice( pTerm->hFilenoStdin );
   pTerm->fStdoutTTY     = zh_fsIsDevice( pTerm->hFilenoStdout );
   pTerm->fStderrTTY     = zh_fsIsDevice( pTerm->hFilenoStderr );
   pTerm->hFileno        = pTerm->hFilenoStdout;
   pTerm->fOutTTY        = pTerm->fStdoutTTY;
   if( ! pTerm->fOutTTY && pTerm->fStdinTTY )
   {
      pTerm->hFileno     = pTerm->hFilenoStdin;
      pTerm->fOutTTY     = ZH_TRUE;
   }
   pTerm->fPosAnswer     = pTerm->fOutTTY && ! zh_trm_Param( "NOPOS", NULL ) &&
                           pTerm->terminal_type != TERM_CYGWIN;
   pTerm->fUTF8          = ZH_FALSE;

   zh_fsSetDevMode( pTerm->hFileno, FD_BINARY );

   zh_gt_chrmapinit( pTerm->charmap, szTerm, pTerm->terminal_type == TERM_XTERM );

#ifndef ZH_GT_UNICODE_BUF
   pTerm->cdpHost = pTerm->cdpIn = NULL;
   pTerm->cdpBox = zh_cdpFind( "EN" );
#endif

   add_efds( pTerm, pTerm->hFilenoStdin, O_RDONLY, NULL, NULL );
   init_keys( pTerm );
   mouse_init( pTerm );
}

static void zh_gt_trm_Init( PZH_GT pGT, ZH_FHANDLE hFilenoStdin, ZH_FHANDLE hFilenoStdout, ZH_FHANDLE hFilenoStderr )
{
   int iRows = 24, iCols = 80;
   PZH_GTTRM pTerm;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_Init(%p,%p,%p,%p)", ( void * ) pGT, ( void * ) ( ZH_PTRUINT ) hFilenoStdin, ( void * ) ( ZH_PTRUINT ) hFilenoStdout, ( void * ) ( ZH_PTRUINT ) hFilenoStderr ) );

   ZH_GTLOCAL( pGT ) = pTerm = ( PZH_GTTRM ) zh_xgrabz( sizeof( ZH_GTTRM ) );

   pTerm->pGT = pGT;
   pTerm->hFilenoStdin  = hFilenoStdin;
   pTerm->hFilenoStdout = hFilenoStdout;
   pTerm->hFilenoStderr = hFilenoStderr;

   zh_gt_trm_SetTerm( pTerm );

/* SA_NOCLDSTOP in #if is a hack to detect POSIX compatible environment */
#if defined( ZH_OS_UNIX ) && defined( SA_NOCLDSTOP )

   if( pTerm->fStdinTTY )
   {
      struct sigaction act, old;

      s_fRestTTY = ZH_TRUE;

      /* if( pTerm->saved_TIO.c_lflag & TOSTOP ) != 0 */
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

      tcgetattr( pTerm->hFilenoStdin, &pTerm->saved_TIO );
      memcpy( &pTerm->curr_TIO, &pTerm->saved_TIO, sizeof( struct termios ) );
      pTerm->curr_TIO.c_lflag &= ~( ECHO | ECHONL | ICANON | ISIG | IEXTEN );
      pTerm->curr_TIO.c_lflag |= NOFLSH;
      pTerm->curr_TIO.c_cflag &= ~( CSIZE | PARENB );
      pTerm->curr_TIO.c_cflag |= CS8 | CREAD;
      pTerm->curr_TIO.c_iflag &= ~( IGNBRK | BRKINT | PARMRK | ISTRIP | INLCR | IGNCR | ICRNL | IXON );
      pTerm->curr_TIO.c_oflag &= ~OPOST;
      /* Enable LF->CR+LF translation */
      pTerm->curr_TIO.c_oflag = ONLCR | OPOST;

      memset( pTerm->curr_TIO.c_cc, 0, NCCS );
      /* workaround for bug in some Linux kernels (i.e. 3.13.0-64-generic
         *buntu) in which select() unconditionally accepts stdin for
         reading if c_cc[ VMIN ] = 0 [druzus] */
      pTerm->curr_TIO.c_cc[ VMIN ] = 1;
      tcsetattr( pTerm->hFilenoStdin, TCSAFLUSH, &pTerm->curr_TIO );
      act.sa_handler = SIG_DFL;

      sigaction( SIGTTOU, &old, NULL );
      pTerm->fRestTTY = s_fRestTTY;
   }
   set_signals();
   if( ! zh_gt_trm_getSize( pTerm, &iRows, &iCols ) )
   {
      iRows = 24;
      iCols = 80;
   }
#endif

   ZH_GTSUPER_INIT( pGT, hFilenoStdin, hFilenoStdout, hFilenoStderr );
   ZH_GTSELF_RESIZE( pGT, iRows, iCols );
   ZH_GTSELF_SETFLAG( pGT, ZH_GTI_REDRAWMAX, 8 );
   ZH_GTSELF_SETFLAG( pGT, ZH_GTI_STDOUTCON, pTerm->fStdoutTTY );
   ZH_GTSELF_SETFLAG( pGT, ZH_GTI_STDERRCON, pTerm->fStderrTTY && pTerm->fOutTTY );
   pTerm->Init( pTerm );
   pTerm->SetTermMode( pTerm, 0 );
#ifdef ZH_GTTRM_CHK_EXACT_POS
   if( pTerm->GetCursorPos( pTerm, &pTerm->iRow, &pTerm->iCol, NULL ) )
      ZH_GTSELF_SETPOS( pGT, pTerm->iRow, pTerm->iCol );
   pTerm->fUTF8 = zh_trm_isUTF8( pTerm );
#else
   pTerm->fUTF8 = zh_trm_isUTF8( pTerm );
   if( pTerm->fPosAnswer )
      ZH_GTSELF_SETPOS( pGT, pTerm->iRow, pTerm->iCol );
#endif
   if( ! pTerm->fUTF8 )
   {
#ifndef ZH_GT_UNICODE_BUF
      zh_gt_trm_SetKeyTrans( pTerm );
#endif
      zh_gt_trm_SetDispTrans( pTerm, 0 );
   }
   ZH_GTSELF_SETBLINK( pGT, ZH_TRUE );
   if( pTerm->fOutTTY )
      ZH_GTSELF_SEMICOLD( pGT );
}

static void zh_gt_trm_Exit( PZH_GT pGT )
{
   PZH_GTTRM pTerm;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_Exit(%p)", ( void * ) pGT ) );

   ZH_GTSELF_REFRESH( pGT );

   pTerm = ZH_GTTRM_GET( pGT );
   if( pTerm )
   {
      mouse_exit( pTerm );
      del_all_efds( pTerm );
      if( pTerm->pKeyTab )
         removeAllKeyMap( pTerm, &pTerm->pKeyTab );

      pTerm->Exit( pTerm );
      zh_gt_trm_ResetPalette( pTerm );
      if( pTerm->fOutTTY && pTerm->iCol > 0 )
         zh_gt_trm_termOut( pTerm, "\r\n", 2 );
      zh_gt_trm_termFlush( pTerm );
   }

   ZH_GTSUPER_EXIT( pGT );

   if( pTerm )
   {
#if defined( ZH_OS_UNIX )
      if( pTerm->fRestTTY )
         tcsetattr( pTerm->hFilenoStdin, TCSANOW, &pTerm->saved_TIO );
#endif
      if( pTerm->nLineBufSize > 0 )
         zh_xfree( pTerm->pLineBuf );
      if( pTerm->iOutBufSize > 0 )
         zh_xfree( pTerm->pOutBuf );
      zh_xfree( pTerm );
   }
}

static ZH_BOOL zh_gt_trm_mouse_IsPresent( PZH_GT pGT )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_mouse_IsPresent(%p)", ( void * ) pGT ) );

   return ZH_GTTRM_GET( pGT )->mouse_type != MOUSE_NONE;
}

static void zh_gt_trm_mouse_Show( PZH_GT pGT )
{
   PZH_GTTRM pTerm;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_mouse_Show(%p)", ( void * ) pGT ) );

   pTerm = ZH_GTTRM_GET( pGT );
#if defined( ZH_HAS_GPM )
   if( pTerm->mouse_type & MOUSE_GPM )
      gpm_visiblepointer = 1;
#endif
   disp_mousecursor( pTerm );
}

static void zh_gt_trm_mouse_Hide( PZH_GT pGT )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_mouse_Hide(%p)", ( void * ) pGT ) );

#if defined( ZH_HAS_GPM )
   if( ZH_GTTRM_GET( pGT )->mouse_type & MOUSE_GPM )
   {
      gpm_visiblepointer = 0;
   }
#else
   ZH_SYMBOL_UNUSED( pGT );
#endif
}

static void zh_gt_trm_mouse_GetPos( PZH_GT pGT, int * piRow, int * piCol )
{
   PZH_GTTRM pTerm;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_mouse_Col(%p,%p,%p)", ( void * ) pGT, ( void * ) piRow, ( void * ) piCol ) );

   pTerm = ZH_GTTRM_GET( pGT );
   *piRow = pTerm->mLastEvt.row;
   *piCol = pTerm->mLastEvt.col;
}

static void zh_gt_trm_mouse_SetPos( PZH_GT pGT, int iRow, int iCol )
{
   PZH_GTTRM pTerm;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_mouse_SetPos(%p,%i,%i)", ( void * ) pGT, iRow, iCol ) );

   pTerm = ZH_GTTRM_GET( pGT );
   /* it does really nothing */
   pTerm->mLastEvt.col = iCol;
   pTerm->mLastEvt.row = iRow;
   disp_mousecursor( pTerm );
}

static ZH_BOOL zh_gt_trm_mouse_ButtonState( PZH_GT pGT, int iButton )
{
   PZH_GTTRM pTerm;
   ZH_BOOL ret = ZH_FALSE;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_mouse_ButtonState(%p,%i)", ( void * ) pGT, iButton ) );

   pTerm = ZH_GTTRM_GET( pGT );
   if( pTerm->mouse_type != MOUSE_NONE )
   {
      int mask;

      if( iButton == 0 )
         mask = M_BUTTON_LEFT;
      else if( iButton == 1 )
         mask = M_BUTTON_RIGHT;
      else if( iButton == 2 )
         mask = M_BUTTON_MIDDLE;
      else
         mask = 0;

      ret = ( pTerm->mLastEvt.buttonstate & mask ) != 0;
   }

   return ret;
}

static int zh_gt_trm_mouse_CountButton( PZH_GT pGT )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_mouse_CountButton(%p)", ( void * ) pGT ) );

   return ZH_GTTRM_GET( pGT )->mButtons;
}

static int zh_gt_trm_ReadKey( PZH_GT pGT, int iEventMask )
{
   int iKey;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_ReadKey(%p,%d)", ( void * ) pGT, iEventMask ) );

   ZH_SYMBOL_UNUSED( iEventMask );

   iKey = wait_key( ZH_GTTRM_GET( pGT ), 0 );

   if( iKey == K_RESIZE )
   {
      int iRows, iCols;

      if( zh_gt_trm_getSize( ZH_GTTRM_GET( pGT ), &iRows, &iCols ) )
      {
         ZH_GTSELF_RESIZE( pGT, iRows, iCols );
         iKey = ZH_INKEY_NEW_EVENT( ZH_K_RESIZE ); // emituje inkey event
      }
      else
         iKey = 0;
   }

   return iKey;
}

static void zh_gt_trm_Tone( PZH_GT pGT, double dFrequency, double dDuration )
{
   PZH_GTTRM pTerm;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_Tone(%p,%lf,%lf)", ( void * ) pGT, dFrequency, dDuration ) );

   pTerm = ZH_GTTRM_GET( pGT );
   pTerm->Tone( pTerm, dFrequency, dDuration );
}

static void zh_gt_trm_Bell( PZH_GT pGT )
{
   PZH_GTTRM pTerm;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_Bell(%p)", ( void * ) pGT ) );

   pTerm = ZH_GTTRM_GET( pGT );
   pTerm->Bell( pTerm );
}

static const char * zh_gt_trm_Version( PZH_GT pGT, int iType )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_Version(%p,%d)", ( void * ) pGT, iType ) );

   ZH_SYMBOL_UNUSED( pGT );

   if( iType == 0 )
      return ZH_GT_DRVNAME( ZH_GT_NAME );

   return "Terminal: *nix native";
}

static ZH_BOOL zh_gt_trm_Suspend( PZH_GT pGT )
{
   PZH_GTTRM pTerm;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_Suspend(%p)", ( void * ) pGT ) );

   pTerm = ZH_GTTRM_GET( pGT );
   if( pTerm->mouse_type & MOUSE_XTERM )
      zh_gt_trm_termOut( pTerm, s_szMouseOff, strlen( s_szMouseOff ) );
#if defined( ZH_OS_UNIX ) || defined( __DJGPP__ )
   if( pTerm->fRestTTY )
      tcsetattr( pTerm->hFilenoStdin, TCSANOW, &pTerm->saved_TIO );
#endif
   /* Enable line wrap when cursor set after last column */
   pTerm->SetTermMode( pTerm, 1 );
   return ZH_TRUE;
}

static ZH_BOOL zh_gt_trm_Resume( PZH_GT pGT )
{
   PZH_GTTRM pTerm;
   int iHeight, iWidth;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_Resume(%p)", ( void * ) pGT ) );

   pTerm = ZH_GTTRM_GET( pGT );
#if defined( ZH_OS_UNIX ) || defined( __DJGPP__ )
   if( pTerm->fRestTTY )
      tcsetattr( pTerm->hFilenoStdin, TCSANOW, &pTerm->curr_TIO );
#endif
   if( pTerm->mouse_type & MOUSE_XTERM )
      zh_gt_trm_termOut( pTerm, s_szMouseOn, strlen( s_szMouseOn ) );

   pTerm->Init( pTerm );

   ZH_GTSELF_GETSIZE( pGT, &iHeight, &iWidth );
   ZH_GTSELF_EXPOSEAREA( pGT, 0, 0, iHeight, iWidth );

   ZH_GTSELF_REFRESH( pGT );

   return ZH_TRUE;
}

static void zh_gt_trm_Scroll( PZH_GT pGT, int iTop, int iLeft, int iBottom, int iRight,
                              int iColor, ZH_USHORT usChar, int iRows, int iCols )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_Scroll(%p,%d,%d,%d,%d,%d,%d,%d,%d)", ( void * ) pGT, iTop, iLeft, iBottom, iRight, iColor, usChar, iRows, iCols ) );

   /* Provide some basic scroll support for full screen */
   if( iCols == 0 && iRows > 0 && iTop == 0 && iLeft == 0 )
   {
      PZH_GTTRM pTerm = ZH_GTTRM_GET( pGT );
      int iHeight, iWidth;

      ZH_GTSELF_GETSIZE( pGT, &iHeight, &iWidth );
      if( iBottom >= iHeight - 1 && iRight >= iWidth - 1 &&
          pTerm->iRow == iHeight - 1 )
      {
         /* scroll up the internal screen buffer */
         ZH_GTSELF_SCROLLUP( pGT, iRows, iColor, usChar );
         /* set default color for terminals which use it to erase
          * scrolled area */
         pTerm->SetAttributes( pTerm, iColor & pTerm->iAttrMask );
         /* update our internal row position */
         do
         {
            zh_gt_trm_termOut( pTerm, "\r\n", 2 );
         }
         while( --iRows > 0 );
         pTerm->iCol = 0;
         return;
      }
   }

   ZH_GTSUPER_SCROLL( pGT, iTop, iLeft, iBottom, iRight, iColor, usChar, iRows, iCols );
}

static ZH_BOOL zh_gt_trm_SetMode( PZH_GT pGT, int iRows, int iCols )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_SetMode(%p,%d,%d)", ( void * ) pGT, iRows, iCols ) );

   if( iRows > 0 && iCols > 0 )
   {
      PZH_GTTRM pTerm = ZH_GTTRM_GET( pGT );
      if( pTerm->SetMode( pTerm, &iRows, &iCols ) )
      {
         ZH_GTSELF_RESIZE( pGT, iRows, iCols );
         return ZH_TRUE;
      }
   }
   return ZH_FALSE;
}

static void zh_gt_trm_SetBlink( PZH_GT pGT, ZH_BOOL fBlink )
{
   PZH_GTTRM pTerm;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_SetBlink(%p,%d)", ( void * ) pGT, ( int ) fBlink ) );

   pTerm = ZH_GTTRM_GET( pGT );

   {
      if( fBlink )
         pTerm->iAttrMask |= 0x0080;
      else
         pTerm->iAttrMask &= ~0x0080;
   }

   ZH_GTSUPER_SETBLINK( pGT, fBlink );
}

static ZH_BOOL zh_gt_trm_SetDispCP( PZH_GT pGT, const char * pszTermCDP, const char * pszHostCDP, ZH_BOOL fBox )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_SetDispCP(%p,%s,%s,%d)", ( void * ) pGT, pszTermCDP, pszHostCDP, ( int ) fBox ) );

   if( ZH_GTSUPER_SETDISPCP( pGT, pszTermCDP, pszHostCDP, fBox ) )
   {
      if( ! ZH_GTTRM_GET( pGT )->fUTF8 )
         zh_gt_trm_SetDispTrans( ZH_GTTRM_GET( pGT ), fBox ? 1 : 0 );
      return ZH_TRUE;
   }
   return ZH_FALSE;
}

#ifndef ZH_GT_UNICODE_BUF
static ZH_BOOL zh_gt_trm_SetKeyCP( PZH_GT pGT, const char * pszTermCDP, const char * pszHostCDP )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_SetKeyCP(%p,%s,%s)", ( void * ) pGT, pszTermCDP, pszHostCDP ) );

   if( ZH_GTSUPER_SETKEYCP( pGT, pszTermCDP, pszHostCDP ) )
   {
      if( ! ZH_GTTRM_GET( pGT )->fUTF8 )
         zh_gt_trm_SetKeyTrans( ZH_GTTRM_GET( pGT ) );
      return ZH_TRUE;
   }
   return ZH_FALSE;
}
#endif

static void zh_gt_trm_Redraw( PZH_GT pGT, int iRow, int iCol, int iSize )
{
   PZH_GTTRM pTerm;
   ZH_BYTE bAttr;
   ZH_USHORT usChar;
   int iLen, iChars, iAttribute, iColor;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_Redraw(%p,%d,%d,%d)", ( void * ) pGT, iRow, iCol, iSize ) );

   iLen = iChars = iAttribute = 0;
   pTerm = ZH_GTTRM_GET( pGT );
   pTerm->SetTermMode( pTerm, 0 );
   if( iRow < pTerm->iRow )
      pTerm->SetCursorStyle( pTerm, SC_NONE );
   if( pTerm->fAM && iRow == pTerm->iHeight - 1 && iCol + iSize == pTerm->iWidth )
      iSize--;
   while( iSize-- )
   {
#ifdef ZH_GT_UNICODE_BUF
      if( pTerm->fUTF8 )
      {
         if( ! ZH_GTSELF_GETSCRCHAR( pGT, iRow, iCol + iChars, &iColor, &bAttr, &usChar ) )
            break;
         if( bAttr & ZH_GT_ATTR_BOX )
            iColor |= ZH_GTTRM_ATTR_BOX;
         usChar = zh_cdpGetU16Ctrl( usChar );
      }
      else
      {
         ZH_UCHAR uc;
         if( ! ZH_GTSELF_GETSCRUC( pGT, iRow, iCol + iChars, &iColor, &bAttr, &uc, ZH_FALSE ) )
            break;
         if( bAttr & ZH_GT_ATTR_BOX )
         {
            iColor |= ( pTerm->boxattr[ uc ] & ~ZH_GTTRM_ATTR_CHAR );
            usChar = pTerm->boxattr[ uc ] & ZH_GTTRM_ATTR_CHAR;
         }
         else
         {
            iColor |= ( pTerm->chrattr[ uc ] & ~ZH_GTTRM_ATTR_CHAR );
            usChar = pTerm->chrattr[ uc ] & ZH_GTTRM_ATTR_CHAR;
         }
      }

      if( iLen == 0 )
         iAttribute = iColor;
      else if( iColor != iAttribute )
      {
         zh_gt_trm_PutStr( pTerm, iRow, iCol, iAttribute, pTerm->pLineBuf, iLen, iChars );
         iCol += iChars;
         iLen = iChars = 0;
         iAttribute = iColor;
      }
      if( pTerm->fUTF8 )
         iLen += zh_cdpU16CharToUTF8( pTerm->pLineBuf + iLen, usChar );
      else
         pTerm->pLineBuf[ iLen++ ] = ( char ) usChar;
      ++iChars;
#else
      if( ! ZH_GTSELF_GETSCRCHAR( pGT, iRow, iCol + iChars, &iColor, &bAttr, &usChar ) )
         break;
      usChar &= 0xff;
      if( bAttr & ZH_GT_ATTR_BOX )
      {
         iColor |= ( pTerm->boxattr[ usChar ] & ~ZH_GTTRM_ATTR_CHAR );
         if( ! pTerm->fUTF8 )
            usChar = pTerm->boxattr[ usChar ] & ZH_GTTRM_ATTR_CHAR;
         else
            iColor |= ZH_GTTRM_ATTR_BOX;
      }
      else
      {
         iColor |= ( pTerm->chrattr[ usChar ] & ~ZH_GTTRM_ATTR_CHAR );
         if( ! pTerm->fUTF8 )
            usChar = pTerm->chrattr[ usChar ] & ZH_GTTRM_ATTR_CHAR;
      }
      if( iLen == 0 )
         iAttribute = iColor;
      else if( iColor != iAttribute )
      {
         zh_gt_trm_PutStr( pTerm, iRow, iCol, iAttribute, pTerm->pLineBuf, iLen, iChars );
         iCol += iChars;
         iLen = iChars = 0;
         iAttribute = iColor;
      }
      pTerm->pLineBuf[ iLen++ ] = ( char ) usChar;
      ++iChars;
#endif
   }
   if( iLen )
      zh_gt_trm_PutStr( pTerm, iRow, iCol, iAttribute, pTerm->pLineBuf, iLen, iChars );
}

static void zh_gt_trm_Refresh( PZH_GT pGT )
{
   int iRow, iCol, iStyle;
   ZH_SIZE nLineBufSize;
   PZH_GTTRM pTerm;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_Refresh(%p)", ( void * ) pGT ) );

   pTerm = ZH_GTTRM_GET( pGT );

   ZH_GTSELF_GETSIZE( pGT, &pTerm->iHeight, &pTerm->iWidth );

#ifdef ZH_GT_UNICODE_BUF
   nLineBufSize = pTerm->iWidth * ( pTerm->fUTF8 ? 3 : 1 );
#else
   nLineBufSize = pTerm->iWidth;
#endif
   if( pTerm->nLineBufSize != nLineBufSize )
   {
      pTerm->pLineBuf = ( char * ) zh_xrealloc( pTerm->pLineBuf, nLineBufSize );
      pTerm->nLineBufSize = nLineBufSize;
   }

   ZH_GTSUPER_REFRESH( pGT );

   ZH_GTSELF_GETSCRCURSOR( pGT, &iRow, &iCol, &iStyle );
   if( iStyle != SC_NONE )
   {
      if( iRow >= 0 && iCol >= 0 &&
          iRow < pTerm->iHeight && iCol < pTerm->iWidth )
         pTerm->SetCursorPos( pTerm, iRow, iCol );
      else
         iStyle = SC_NONE;
   }
   pTerm->SetCursorStyle( pTerm, iStyle );
   zh_gt_trm_termFlush( pTerm );
   disp_mousecursor( pTerm );
}

static ZH_BOOL zh_gt_trm_Info( PZH_GT pGT, int iType, PZH_GT_INFO pInfo )
{
   PZH_GTTRM pTerm;
   const char * szVal;
   void * hVal;
   int iVal;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_trm_Info(%p,%d,%p)", ( void * ) pGT, iType, ( void * ) pInfo ) );

   pTerm = ZH_GTTRM_GET( pGT );
   switch( iType )
   {
      case ZH_GTI_ISSCREENPOS:
      case ZH_GTI_KBDSUPPORT:
         pInfo->pResult = zh_itemPutL( pInfo->pResult, ZH_TRUE );
         break;

      case ZH_GTI_ISUNICODE:
         pInfo->pResult = zh_itemPutL( pInfo->pResult, pTerm->fUTF8 );
         break;

#ifndef ZH_GT_UNICODE_BUF
      case ZH_GTI_BOXCP:
         pInfo->pResult = zh_itemPutC( pInfo->pResult,
                                       pTerm->cdpBox ? pTerm->cdpBox->id : NULL );
         szVal = zh_itemGetCPtr( pInfo->pNewVal );
         if( szVal && *szVal )
         {
            PZH_CODEPAGE cdpBox = zh_cdpFind( szVal );
            if( cdpBox )
               pTerm->cdpBox = cdpBox;
         }
         break;
#endif

      case ZH_GTI_ESCDELAY:
         pInfo->pResult = zh_itemPutNI( pInfo->pResult, pTerm->esc_delay );
         if( zh_itemType( pInfo->pNewVal ) & ZH_IT_NUMERIC )
            pTerm->esc_delay = zh_itemGetNI( pInfo->pNewVal );
         break;

      case ZH_GTI_KBDSHIFTS:
         pInfo->pResult = zh_itemPutNI( pInfo->pResult,
                                        zh_gt_trm_getKbdState( pTerm ) );
         break;

      case ZH_GTI_DELKEYMAP:
         szVal = zh_itemGetCPtr( pInfo->pNewVal );
         if( szVal && *szVal )
            removeKeyMap( pTerm, zh_itemGetCPtr( pInfo->pNewVal ) );
         break;

      case ZH_GTI_ADDKEYMAP:
         if( zh_itemType( pInfo->pNewVal ) & ZH_IT_ARRAY )
         {
            iVal = zh_arrayGetNI( pInfo->pNewVal, 1 );
            szVal = zh_arrayGetCPtr( pInfo->pNewVal, 2 );
            if( iVal && szVal && *szVal )
               addKeyMap( pTerm, ZH_INKEY_ISEXT( iVal ) ?
                                 iVal : SET_CLIPKEY( iVal ), szVal );
         }
         break;

      case ZH_GTI_WINTITLE:
         if( pTerm->fUTF8 )
            pInfo->pResult = zh_itemPutStrUTF8( pInfo->pResult, pTerm->szTitle );
         else
#ifdef ZH_GT_UNICODE_BUF
            pInfo->pResult = zh_itemPutStr( pInfo->pResult, ZH_GTSELF_TERMCP( pGT ), pTerm->szTitle );
#else
            pInfo->pResult = zh_itemPutStr( pInfo->pResult, pTerm->cdpTerm, pTerm->szTitle );
#endif
         if( zh_itemType( pInfo->pNewVal ) & ZH_IT_STRING )
         {
            if( pTerm->fUTF8 )
               szVal = zh_itemGetStrUTF8( pInfo->pNewVal, &hVal, NULL );
            else
#ifdef ZH_GT_UNICODE_BUF
               szVal = zh_itemGetStr( pInfo->pNewVal, ZH_GTSELF_TERMCP( pGT ), &hVal, NULL );
#else
               szVal = zh_itemGetStr( pInfo->pNewVal, pTerm->cdpTerm, &hVal, NULL );
#endif

            if( pTerm->szTitle )
               zh_xfree( pTerm->szTitle );
            pTerm->szTitle = ( szVal && *szVal ) ? zh_strdup( szVal ) : NULL;
            zh_gt_trm_SetTitle( pTerm, pTerm->szTitle );
            zh_gt_trm_termFlush( pTerm );
            zh_strfree( hVal );
         }
         break;


      case ZH_GTI_PALETTE:
         if( zh_itemType( pInfo->pNewVal ) & ZH_IT_NUMERIC )
         {
            iVal = zh_itemGetNI( pInfo->pNewVal );
            if( iVal >= 0 && iVal < 16 )
            {
               pInfo->pResult = zh_itemPutNI( pInfo->pResult, pTerm->colors[ iVal ] );
               if( zh_itemType( pInfo->pNewVal2 ) & ZH_IT_NUMERIC )
               {
                  pTerm->colors[ iVal ] = zh_itemGetNI( pInfo->pNewVal2 );
                  zh_gt_trm_SetPalette( pTerm, iVal, iVal );
                  zh_gt_trm_termFlush( pTerm );
               }
            }
         }
         else
         {
            if( ! pInfo->pResult )
               pInfo->pResult = zh_itemNew( NULL );
            zh_arrayNew( pInfo->pResult, 16 );
            for( iVal = 0; iVal < 16; iVal++ )
               zh_arraySetNI( pInfo->pResult, iVal + 1, pTerm->colors[ iVal ] );
            if( zh_itemType( pInfo->pNewVal ) & ZH_IT_ARRAY &&
                zh_arrayLen( pInfo->pNewVal ) == 16 )
            {
               for( iVal = 0; iVal < 16; iVal++ )
                  pTerm->colors[ iVal ] = zh_arrayGetNI( pInfo->pNewVal, iVal + 1 );
               zh_gt_trm_SetPalette( pTerm, 0, 15 );
               zh_gt_trm_termFlush( pTerm );
            }
         }
         break;

      case ZH_GTI_RESIZABLE:
         pInfo->pResult = zh_itemPutL( pInfo->pResult, ZH_TRUE );
         break;

      case ZH_GTI_CLOSABLE:
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

   pFuncTable->Init                       = zh_gt_trm_Init;
   pFuncTable->Exit                       = zh_gt_trm_Exit;
   pFuncTable->Redraw                     = zh_gt_trm_Redraw;
   pFuncTable->Refresh                    = zh_gt_trm_Refresh;
   pFuncTable->Scroll                     = zh_gt_trm_Scroll;
   pFuncTable->Version                    = zh_gt_trm_Version;
   pFuncTable->Suspend                    = zh_gt_trm_Suspend;
   pFuncTable->Resume                     = zh_gt_trm_Resume;
   pFuncTable->SetMode                    = zh_gt_trm_SetMode;
   pFuncTable->SetBlink                   = zh_gt_trm_SetBlink;
   pFuncTable->SetDispCP                  = zh_gt_trm_SetDispCP;
#ifndef ZH_GT_UNICODE_BUF
   pFuncTable->SetKeyCP                   = zh_gt_trm_SetKeyCP;
#endif
   pFuncTable->Tone                       = zh_gt_trm_Tone;
   pFuncTable->Bell                       = zh_gt_trm_Bell;
   pFuncTable->Info                       = zh_gt_trm_Info;

   pFuncTable->ReadKey                    = zh_gt_trm_ReadKey;

   pFuncTable->MouseIsPresent             = zh_gt_trm_mouse_IsPresent;
   pFuncTable->MouseShow                  = zh_gt_trm_mouse_Show;
   pFuncTable->MouseHide                  = zh_gt_trm_mouse_Hide;
   pFuncTable->MouseGetPos                = zh_gt_trm_mouse_GetPos;
   pFuncTable->MouseSetPos                = zh_gt_trm_mouse_SetPos;
   pFuncTable->MouseButtonState           = zh_gt_trm_mouse_ButtonState;
   pFuncTable->MouseCountButton           = zh_gt_trm_mouse_CountButton;

   return ZH_TRUE;
}


#include "../zh_gt_reg.h"
