/*
 * Not (yet) implemented CT functions
 *
 * Copyright 2010 Viktor Szakats (vszakats.net/ziher)
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

#include "zh_defs.h"

/* Extended Drivers */
ZH_FUNC( DSETNOLINE ) { ; }
ZH_FUNC( DSETQFILE )  { ; }
ZH_FUNC( DSETTYPE )   { ; }
ZH_FUNC( DSETWINDEB ) { ; }
ZH_FUNC( DSETWINDOW ) { ; }
ZH_FUNC( FIRSTCOL )   { ; }
ZH_FUNC( FIRSTROW )   { ; }
ZH_FUNC( GETBOXGROW ) { ; }
ZH_FUNC( GETCURSOR )  { ; }
ZH_FUNC( GETLINES )   { ; }
ZH_FUNC( GETMODE )    { ; }
ZH_FUNC( GETPAGE )    { ; }
ZH_FUNC( GETPBIOS )   { ; }
ZH_FUNC( GETPXLAT )   { ; }
ZH_FUNC( GETSCRMODE ) { ; }
ZH_FUNC( GETTAB )     { ; }
ZH_FUNC( INKEYTRAP )  { ; }
ZH_FUNC( INPUTMODE )  { ; }
ZH_FUNC( KEYREAD )    { ; }
ZH_FUNC( KEYSEND )    { ; }
ZH_FUNC( MAXPAGE )    { ; }
ZH_FUNC( MONOCHROME ) { ; }
ZH_FUNC( PAGECOPY )   { ; }
ZH_FUNC( PRINTERROR ) { ; }
ZH_FUNC( SETBELL )    { ; }
ZH_FUNC( SETBOXGROW ) { ; }

ZH_FUNC( SETLINES )   { ; }
ZH_FUNC( SETMAXCOL )  { ; }
ZH_FUNC( SETMAXROW )  { ; }
ZH_FUNC( SETPAGE )    { ; }
ZH_FUNC( SETPBIOS )   { ; }
ZH_FUNC( SETPXLAT )   { ; }
ZH_FUNC( SETQNAME )   { ; }
ZH_FUNC( SETSCRMODE ) { ; }
ZH_FUNC( SETTAB )     { ; }
ZH_FUNC( TRAPANYKEY ) { ; }
ZH_FUNC( TRAPINPUT )  { ; }
ZH_FUNC( TRAPSHIFT )  { ; }
/* Video Functions */
ZH_FUNC( EGAPALETTE ) { ; }
ZH_FUNC( FONTLOAD )   { ; }
ZH_FUNC( FONTRESET )  { ; }
ZH_FUNC( FONTROTATE ) { ; }
ZH_FUNC( FONTSELECT ) { ; }
ZH_FUNC( GETFONT )    { ; }
ZH_FUNC( GETSCRSTR )  { ; }
ZH_FUNC( GETVGAPAL )  { ; }
ZH_FUNC( MAXFONT )    { ; }
ZH_FUNC( MONISWITCH ) { ; }
ZH_FUNC( NUMCOL )     { ; }
ZH_FUNC( SCREENSIZE ) { ; }
ZH_FUNC( SETSCRSTR )  { ; }
ZH_FUNC( VIDEOINIT )  { ; }
ZH_FUNC( VIDEOSETUP ) { ; }
/* Disk Utilities */
#if 0
ZH_FUNC( DIRCHANGE )  { ; } /* Implemented in Ziher core as C5.3 function. */
ZH_FUNC( DIRREMOVE )  { ; } /* Implemented in Ziher core as C5.3 function. */
ZH_FUNC( DISKCHANGE ) { ; } /* Implemented in Ziher core as C5.3 function. */
#endif
ZH_FUNC( DISKCHECK )  { ; }
ZH_FUNC( DISKFORMAT ) { ; }

ZH_FUNC( DISKREADY )  { ; }
ZH_FUNC( DISKREADYW ) { ; }
ZH_FUNC( DISKSPEED )  { ; }
ZH_FUNC( DISKSTAT )   { ; }
ZH_FUNC( DISKTYPE )   { ; }
ZH_FUNC( FILECHECK )  { ; }
ZH_FUNC( FILEVALID )  { ; }
ZH_FUNC( FLOPPYTYPE ) { ; }
ZH_FUNC( GETSHARE )   { ; }
ZH_FUNC( NUMDISKF )   { ; }
ZH_FUNC( NUMDISKH )   { ; }
ZH_FUNC( RESTFSEEK )  { ; }
ZH_FUNC( SAVEFSEEK )  { ; }
ZH_FUNC( SETSHARE )   { ; }
/* Printer Functions */
ZH_FUNC( NUMPRINTER ) { ; }
ZH_FUNC( FILEPRINT )  { ; }

ZH_FUNC( PRINTINIT )  { ; }
ZH_FUNC( PRINTSCR )   { ; }
ZH_FUNC( PRINTSCRX )  { ; }
ZH_FUNC( SPOOLACTIV ) { ; }
ZH_FUNC( SPOOLADD )   { ; }
ZH_FUNC( SPOOLCOUNT ) { ; }
ZH_FUNC( SPOOLDEL )   { ; }
ZH_FUNC( SPOOLENTRY ) { ; }
ZH_FUNC( SPOOLFLUSH ) { ; }
ZH_FUNC( TOF )        { ; }
/* Database Functions */
ZH_FUNC( DBFDSKSIZE ) { ; }
ZH_FUNC( ISDBT )      { ; }
/* Set Status */
ZH_FUNC( CSETALL )    { ; }
ZH_FUNC( CSETCLIP )   { ; }
ZH_FUNC( CSETDATE )   { ; }
ZH_FUNC( CSETDECI )   { ; }
ZH_FUNC( CSETDEFA )   { ; }
ZH_FUNC( CSETFUNC )   { ; }
ZH_FUNC( CSETLDEL )   { ; }
ZH_FUNC( CSETMARG )   { ; }
ZH_FUNC( CSETPATH )   { ; }
ZH_FUNC( CSETRDEL )   { ; }
ZH_FUNC( CSETRDONLY ) { ; }
ZH_FUNC( CSETSNOW )   { ; }
ZH_FUNC( CSETXXXX )   { ; }
ZH_FUNC( ISDEBUG )    { ; }
ZH_FUNC( LASTKFUNC )  { ; }
ZH_FUNC( LASTKLINE )  { ; }
ZH_FUNC( LASTKPROC )  { ; }
ZH_FUNC( NUMFKEY )    { ; }
/* System Information */
ZH_FUNC( BIOSDATE )   { ; }
ZH_FUNC( BOOTCOLD )   { ; }
ZH_FUNC( BOOTWARM )   { ; }
ZH_FUNC( ERRORACT )   { ; }
ZH_FUNC( ERRORBASE )  { ; }
ZH_FUNC( ERRORCODE )  { ; }
ZH_FUNC( ERRORORG )   { ; }
ZH_FUNC( FILESFREE )  { ; }
ZH_FUNC( GETCOUNTRY ) { ; }
ZH_FUNC( ISANSI )     { ; }
ZH_FUNC( MEMSIZE )    { ; }
ZH_FUNC( NUMBUFFERS ) { ; }
ZH_FUNC( NUMFILES )   { ; }
ZH_FUNC( OSVER )      { ; }
ZH_FUNC( SSETBREAK )  { ; }
ZH_FUNC( SSETVERIFY ) { ; }
/* Miscellaneous Functions */
ZH_FUNC( DATATYPE )   { ; }
ZH_FUNC( GETTIC )     { ; }
ZH_FUNC( KBDDISABLE ) { ; }
ZH_FUNC( KBDEMULATE ) { ; }
ZH_FUNC( KBDSPEED )   { ; }
ZH_FUNC( KBDTYPE )    { ; }
ZH_FUNC( SCANKEY )    { ; }
ZH_FUNC( SETTIC )     { ; }
ZH_FUNC( SHOWKEY )    { ; }
ZH_FUNC( SOUND )      { ; }
ZH_FUNC( SPEED )      { ; }
ZH_FUNC( STACKFREE )  { ; }
/* PEEK/POKE Functions */
ZH_FUNC( INBYTE )     { ; }
ZH_FUNC( INWORD )     { ; }
ZH_FUNC( OUTBYTE )    { ; }
ZH_FUNC( OUTWORD )    { ; }
ZH_FUNC( PEEKBYTE )   { ; }
ZH_FUNC( PEEKSTR )    { ; }
ZH_FUNC( PEEKWORD )   { ; }
ZH_FUNC( POKEBYTE )   { ; }
ZH_FUNC( POKEWORD )   { ; }
