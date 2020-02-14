/*
 * Header file for the Set API
 *
 * Copyright 1999-2003 David G. Holm <dholm@jsd-llc.com>
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

#ifndef ZH_SET_H_
#define ZH_SET_H_

#include "zh_api.h"
#include "zh_api_gt.h"
#include "zh_apifs.h"

ZH_EXTERN_BEGIN

typedef enum
{
   ZH_SET_INVALID_      = 0,

   ZH_SET_EXACT         = 1,
   ZH_SET_FIXED         = 2,
   ZH_SET_DECIMALS      = 3,
   ZH_SET_DATEFORMAT    = 4,
   ZH_SET_EPOCH         = 5,
   ZH_SET_PATH          = 6,
   ZH_SET_DEFAULT       = 7,

   ZH_SET_EXCLUSIVE     = 8,
   ZH_SET_SOFTSEEK      = 9,
   ZH_SET_UNIQUE        = 10,
   ZH_SET_DELETED       = 11,

   ZH_SET_CANCEL        = 12,
   ZH_SET_DEBUG         = 13,
   ZH_SET_TYPEAHEAD     = 14,

   ZH_SET_COLOR         = 15,
   ZH_SET_CURSOR        = 16,
   ZH_SET_CONSOLE       = 17,
   ZH_SET_ALTERNATE     = 18,
   ZH_SET_ALTFILE       = 19,
   ZH_SET_DEVICE        = 20,
   ZH_SET_EXTRA         = 21,
   ZH_SET_EXTRAFILE     = 22,
   ZH_SET_PRINTER       = 23,
   ZH_SET_PRINTFILE     = 24,
   ZH_SET_MARGIN        = 25,

   ZH_SET_BELL          = 26,
   ZH_SET_CONFIRM       = 27,
   ZH_SET_ESCAPE        = 28,
   ZH_SET_INSERT        = 29,
   ZH_SET_EXIT          = 30,
   ZH_SET_INTENSITY     = 31,
   ZH_SET_SCOREBOARD    = 32,
   ZH_SET_DELIMITERS    = 33,
   ZH_SET_DELIMCHARS    = 34,

   ZH_SET_WRAP          = 35,
   ZH_SET_MESSAGE       = 36,
   ZH_SET_MCENTER       = 37,
   ZH_SET_SCROLLBREAK   = 38,

   ZH_SET_EVENTMASK     = 39,

   ZH_SET_VIDEOMODE     = 40,

   ZH_SET_MBLOCKSIZE    = 41,
   ZH_SET_MFILEEXT      = 42,

   ZH_SET_STRICTREAD    = 43,
   ZH_SET_OPTIMIZE      = 44,
   ZH_SET_AUTOPEN       = 45,
   ZH_SET_AUTORDER      = 46,
   ZH_SET_AUTOSHARE     = 47,

   /* Ziher SET extensions start at 100 */
   ZH_SET_LANGUAGE      = 100,
   ZH_SET_IDLEREPEAT    = 101,
   ZH_SET_FILECASE      = 102,
   ZH_SET_DIRCASE       = 103,
   ZH_SET_DIRSEPARATOR  = 104,
   ZH_SET_EOF           = 105,
   ZH_SET_HARDCOMMIT    = 106,
   ZH_SET_FORCEOPT      = 107,
   ZH_SET_DBFLOCKSCHEME = 108,
   ZH_SET_DEFEXTENSIONS = 109,
   ZH_SET_EOL           = 110,
   ZH_SET_TRIMFILENAME  = 111,
   ZH_SET_HBOUTLOG      = 112,
   ZH_SET_HBOUTLOGINFO  = 113,
   ZH_SET_CODEPAGE      = 114,
   ZH_SET_OSCODEPAGE    = 115,
   ZH_SET_TIMEFORMAT    = 116,
   ZH_SET_DBCODEPAGE    = 117

} ZH_set_enum;

#if defined( _ZH_SET_INTERNAL_ ) || defined( _ZH_API_INTERNAL_ )
typedef struct
{
   /* Lower case members are indirectly related to a SET */
   ZH_BOOL        zh_set_century;
   ZH_BOOL        zh_set_prndevice;
   PZH_FILE       zh_set_althan;
   PZH_FILE       zh_set_extrahan;
   PZH_FILE       zh_set_printhan;
   ZH_PATHNAMES * zh_set_path;
   void *         zh_set_oscp;
   void *         zh_set_dbcp;
   void *         zh_set_listener;

   /* Upper case members are directly related to a SET */
   ZH_BOOL    ZH_SET_ALTERNATE;
   char *     ZH_SET_ALTFILE;
   ZH_BOOL    ZH_SET_AUTOPEN;
   int        ZH_SET_AUTORDER;
   int        ZH_SET_AUTOSHARE;
   ZH_BOOL    ZH_SET_BELL;
   ZH_BOOL    ZH_SET_CANCEL;
   char *     ZH_SET_COLOR;
   ZH_BOOL    ZH_SET_CONFIRM;
   ZH_BOOL    ZH_SET_CONSOLE;
   char *     ZH_SET_DATEFORMAT;
   ZH_BOOL    ZH_SET_DEBUG;
   int        ZH_SET_DECIMALS;
   char *     ZH_SET_DEFAULT;
   ZH_BOOL    ZH_SET_DELETED;
   char *     ZH_SET_DELIMCHARS;
   ZH_BOOL    ZH_SET_DELIMITERS;
   char *     ZH_SET_DEVICE;
   ZH_BOOL    ZH_SET_EOF;
   int        ZH_SET_EPOCH;
   ZH_BOOL    ZH_SET_ESCAPE;
   int        ZH_SET_EVENTMASK;
   ZH_BOOL    ZH_SET_EXACT;
   ZH_BOOL    ZH_SET_EXCLUSIVE;
   ZH_BOOL    ZH_SET_EXIT;
   ZH_BOOL    ZH_SET_EXTRA;
   char *     ZH_SET_EXTRAFILE;
   ZH_BOOL    ZH_SET_FIXED;
   ZH_BOOL    ZH_SET_IDLEREPEAT;
   ZH_BOOL    ZH_SET_INSERT;
   ZH_BOOL    ZH_SET_INTENSITY;
   char *     ZH_SET_PATH;
   int        ZH_SET_MARGIN;
   int        ZH_SET_MBLOCKSIZE;
   ZH_BOOL    ZH_SET_MCENTER;
   int        ZH_SET_MESSAGE;
   char *     ZH_SET_MFILEEXT;
   ZH_BOOL    ZH_SET_OPTIMIZE;
   ZH_BOOL    ZH_SET_PRINTER;
   char *     ZH_SET_PRINTFILE;
   ZH_BOOL    ZH_SET_SCOREBOARD;
   ZH_BOOL    ZH_SET_SCROLLBREAK;
   ZH_BOOL    ZH_SET_SOFTSEEK;
   ZH_BOOL    ZH_SET_STRICTREAD;
   int        ZH_SET_TYPEAHEAD;
   ZH_BOOL    ZH_SET_UNIQUE;
   int        ZH_SET_FILECASE;
   int        ZH_SET_DIRCASE;
   int        ZH_SET_DIRSEPARATOR;
   int        ZH_SET_VIDEOMODE;
   ZH_BOOL    ZH_SET_WRAP;
   int        ZH_SET_DBFLOCKSCHEME;
   ZH_BOOL    ZH_SET_HARDCOMMIT;
   ZH_BOOL    ZH_SET_FORCEOPT;
   ZH_BOOL    ZH_SET_DEFEXTENSIONS;
   char *     ZH_SET_EOL;
   ZH_BOOL    ZH_SET_TRIMFILENAME;
   char *     ZH_SET_HBOUTLOG;
   char *     ZH_SET_HBOUTLOGINFO;
   char *     ZH_SET_TIMEFORMAT;

} ZH_SET_STRUCT, * PZH_SET_STRUCT;

extern void zh_setInitialize( PZH_SET_STRUCT pSet );
extern void zh_setRelease( PZH_SET_STRUCT pSet );
extern PZH_SET_STRUCT zh_setClone( PZH_SET_STRUCT pSet );

#else

typedef void * PZH_SET_STRUCT;

#endif /* _ZH_SET_INTERNAL_ || _ZH_API_INTERNAL_ */

#define ZH_SET_CASE_MIXED  0
#define ZH_SET_CASE_LOWER  1
#define ZH_SET_CASE_UPPER  2

#define ZH_SET_PRN_ANY     0
#define ZH_SET_PRN_CON     1
#define ZH_SET_PRN_DEV     2

#define ZH_SET_DBFLOCK_DEFAULT    0
#define ZH_SET_DBFLOCK_CLIP       1
#define ZH_SET_DBFLOCK_CL53       2
#define ZH_SET_DBFLOCK_VFP        3

typedef enum
{
   ZH_SET_LISTENER_BEFORE,
   ZH_SET_LISTENER_AFTER
} ZH_set_listener_enum;
typedef void ZH_SET_LISTENER_CALLBACK( ZH_set_enum, ZH_set_listener_enum );

extern ZH_EXPORT int          zh_setListenerAdd( ZH_SET_LISTENER_CALLBACK * );
extern ZH_EXPORT void         zh_setListenerNotify( ZH_set_enum, ZH_set_listener_enum );
extern ZH_EXPORT int          zh_setListenerRemove( int );

extern ZH_EXPORT ZH_BOOL      zh_setGetL( ZH_set_enum set_specifier );
extern ZH_EXPORT const char * zh_setGetCPtr( ZH_set_enum set_specifier );
extern ZH_EXPORT int          zh_setGetNI( ZH_set_enum set_specifier );
extern ZH_EXPORT long         zh_setGetNL( ZH_set_enum set_specifier );
extern ZH_EXPORT PZH_ITEM     zh_setGetItem( ZH_set_enum set_specifier, PZH_ITEM pResult, PZH_ITEM pArg1, PZH_ITEM pArg2 );

extern ZH_EXPORT ZH_BOOL      zh_setSetItem( ZH_set_enum set_specifier, PZH_ITEM pItem );
extern ZH_EXPORT ZH_BOOL      zh_setSetItem2( ZH_set_enum set_specifier, PZH_ITEM pItem1, PZH_ITEM pItem2 );

extern ZH_EXPORT ZH_PATHNAMES * zh_setGetFirstSetPath( void );

extern ZH_EXPORT int          zh_setUpdateEpoch( int iYear );

extern ZH_EXPORT ZH_BOOL      zh_setGetCentury( void );
extern ZH_EXPORT ZH_BOOL      zh_setSetCentury( ZH_BOOL );

extern ZH_EXPORT int          zh_setGetFileCase( void );
extern ZH_EXPORT void         zh_setSetFileCase( int iFileCase );

extern ZH_EXPORT int          zh_setGetDirCase( void );
extern ZH_EXPORT void         zh_setSetDirCase( int iDirCase );

extern ZH_EXPORT int          zh_setGetDirSeparator( void );
extern ZH_EXPORT void         zh_setSetDirSeparator( int iSeparator );

extern ZH_EXPORT ZH_BOOL      zh_setGetTrimFileName( void );
extern ZH_EXPORT void         zh_setSetTrimFileName( ZH_BOOL fTrim );

extern ZH_EXPORT PZH_FILE     zh_setGetAltHan( void );
extern ZH_EXPORT PZH_FILE     zh_setGetExtraHan( void );
extern ZH_EXPORT PZH_FILE     zh_setGetPrintHan( void );
extern ZH_EXPORT PZH_FILE     zh_setGetPrinterHandle( int );
extern ZH_EXPORT ZH_BOOL      zh_setGetAlternate( void );
extern ZH_EXPORT const char * zh_setGetAltFile( void );
extern ZH_EXPORT ZH_BOOL      zh_setGetAutOpen( void );
extern ZH_EXPORT int          zh_setGetAutOrder( void );
extern ZH_EXPORT int          zh_setGetAutoShare( void );
extern ZH_EXPORT ZH_BOOL      zh_setGetBell( void );
extern ZH_EXPORT ZH_BOOL      zh_setGetCancel( void );
extern ZH_EXPORT char *       zh_setGetColor( void );
extern ZH_EXPORT ZH_BOOL      zh_setGetConfirm( void );
extern ZH_EXPORT ZH_BOOL      zh_setGetConsole( void );
extern ZH_EXPORT const char * zh_setGetDateFormat( void );
extern ZH_EXPORT const char * zh_setGetTimeFormat( void );
extern ZH_EXPORT ZH_BOOL      zh_setGetDebug( void );
extern ZH_EXPORT int          zh_setGetDecimals( void );
extern ZH_EXPORT const char * zh_setGetDefault( void );
extern ZH_EXPORT ZH_BOOL      zh_setGetDeleted( void );
extern ZH_EXPORT const char * zh_setGetDelimChars( void );
extern ZH_EXPORT ZH_BOOL      zh_setGetDelimiters( void );
extern ZH_EXPORT const char * zh_setGetDevice( void );
extern ZH_EXPORT ZH_BOOL      zh_setGetEOF( void );
extern ZH_EXPORT int          zh_setGetEpoch( void );
extern ZH_EXPORT ZH_BOOL      zh_setGetEscape( void );
extern ZH_EXPORT int          zh_setGetEventMask( void );
extern ZH_EXPORT ZH_BOOL      zh_setGetExact( void );
extern ZH_EXPORT ZH_BOOL      zh_setGetExclusive( void );
extern ZH_EXPORT ZH_BOOL      zh_setGetExit( void );
extern ZH_EXPORT ZH_BOOL      zh_setGetExtra( void );
extern ZH_EXPORT const char * zh_setGetExtraFile( void );
extern ZH_EXPORT ZH_BOOL      zh_setGetFixed( void );
extern ZH_EXPORT ZH_BOOL      zh_setGetIdleRepeat( void );
extern ZH_EXPORT ZH_BOOL      zh_setGetInsert( void );
extern ZH_EXPORT ZH_BOOL      zh_setGetIntensity( void );
extern ZH_EXPORT const char * zh_setGetPath( void );
extern ZH_EXPORT int          zh_setGetMargin( void );
extern ZH_EXPORT int          zh_setGetMBlockSize( void );
extern ZH_EXPORT ZH_BOOL      zh_setGetMCenter( void );
extern ZH_EXPORT int          zh_setGetMessage( void );
extern ZH_EXPORT const char * zh_setGetMFileExt( void );
extern ZH_EXPORT ZH_BOOL      zh_setGetOptimize( void );
extern ZH_EXPORT ZH_BOOL      zh_setGetPrinter( void );
extern ZH_EXPORT const char * zh_setGetPrintFile( void );
extern ZH_EXPORT ZH_BOOL      zh_setGetScoreBoard( void );
extern ZH_EXPORT ZH_BOOL      zh_setGetScrollBreak( void );
extern ZH_EXPORT ZH_BOOL      zh_setGetSoftSeek( void );
extern ZH_EXPORT ZH_BOOL      zh_setGetStrictRead( void );
extern ZH_EXPORT int          zh_setGetTypeAhead( void );
extern ZH_EXPORT ZH_BOOL      zh_setGetUnique( void );
extern ZH_EXPORT int          zh_setGetVideoMode( void );
extern ZH_EXPORT ZH_BOOL      zh_setGetWrap( void );
extern ZH_EXPORT int          zh_setGetDBFLockScheme( void );
extern ZH_EXPORT ZH_BOOL      zh_setGetHardCommit( void );
extern ZH_EXPORT ZH_BOOL      zh_setGetForceOpt( void );
extern ZH_EXPORT ZH_BOOL      zh_setGetDefExtension( void );
extern ZH_EXPORT const char * zh_setGetEOL( void );
extern ZH_EXPORT const char * zh_setGetHBOUTLOG( void );
extern ZH_EXPORT const char * zh_setGetHBOUTLOGINFO( void );
extern ZH_EXPORT const char * zh_setGetOSCODEPAGE( void );
extern ZH_EXPORT void *       zh_setGetOSCP( void );
extern ZH_EXPORT const char * zh_setGetDBCODEPAGE( void );

ZH_EXTERN_END

#endif /* ZH_SET_H_ */
