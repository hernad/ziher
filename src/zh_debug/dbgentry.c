/*
 * Debugger entry routine
 *
 * Copyright 2005 Phil Krylov <phil a t newstar.rinet.ru>
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
#include "zh_debug_api.h"
#include "zh_item_api.h"
#include "zh_class_api.h"
#include "zh_rdd_api.h"
#include "zh_stack.h"
#include "zh_vm.h"
#include "zh_thread.h"

#include "debug.zhh"
#include "macro.zhh"
#include "set.zhh"

/* dummy function declaration */
static ZH_BOOL zh_clsSetScope( ZH_BOOL fScope )
{
   return fScope;
}

#define ZH_DBGINFO_DISABLE  ( ( ZH_DEBUGINFO * ) ( ZH_PTRUINT ) 0x01 )

#if defined( ZH_OS_UNIX )
#define FILENAME_EQUAL( s1, s2 )  ( ! strcmp( ( s1 ), ( s2 ) ) )
#else
#define FILENAME_EQUAL( s1, s2 )  ( ! zh_stricmp( ( s1 ), ( s2 ) ) )
#endif

#define ARRAY_ADD( type, array, length ) \
   ( ( ++length == 1 ) ? ( array = ( type * ) zh_xgrab( sizeof( type ) ) ) : \
     ( ( array = ( type * ) zh_xrealloc( array, sizeof( type ) * length ) ) + \
       length - 1 ) )

#define ARRAY_DEL( type, array, length, index ) \
   do { \
      if( ! --length ) \
         zh_xfree( array ); \
      else if( index < length ) \
         memmove( array + index, array + index + 1, sizeof( type ) * ( length - index ) ); \
   } while( 0 )

#define ZH_DBGCOMMON_LOCK()       zh_threadEnterCriticalSectionGC( &s_dbgMtx )
#define ZH_DBGCOMMON_UNLOCK()     zh_threadLeaveCriticalSection( &s_dbgMtx )
static ZH_CRITICAL_NEW( s_dbgMtx );

typedef struct
{
   char * szModule;
   int    nLine;
   char * szFunction;
} ZH_BREAKPOINT;

typedef struct
{
   int      nIndex;
   PZH_ITEM xValue;
} ZH_TRACEPOINT;

typedef struct
{
   const char * szName;
   char cType;
   union
   {
      int      num;
      PZH_ITEM ptr;
   } frame;
   int nIndex;
} ZH_VARINFO;

typedef struct
{
   char *       szExpr;
   PZH_ITEM     pBlock;
   int          nVars;
   char **      aVars;
   ZH_VARINFO * aScopes;
} ZH_WATCHPOINT;


typedef struct
{
   char *       szModule;
   char *       szFunction;
   int          nLine;
   int          nProcLevel;
   int          nLocals;
   ZH_VARINFO * aLocals;
   int          nStatics;
   ZH_VARINFO * aStatics;
} ZH_CALLSTACKINFO;

typedef struct
{
   char *       szModule;
   int          nStatics;
   ZH_VARINFO * aStatics;
   int          nGlobals;
   ZH_VARINFO * aGlobals;
   int          nExternGlobals;
   ZH_VARINFO * aExternGlobals;
} ZH_MODULEINFO;

typedef struct
{
   int nModules;
   ZH_MODULEINFO * aModules;
   PZH_ITEM        pStopLines;
} ZH_DBGCOMMONINFO;

typedef struct
{
   ZH_BOOL bQuit;
   ZH_BOOL bGo;
   ZH_BOOL bInside;
   int     nBreakPoints;
   ZH_BREAKPOINT * aBreak;
   int nTracePoints;
   ZH_TRACEPOINT * aTrace;
   int nWatchPoints;
   ZH_WATCHPOINT * aWatch;
   ZH_BOOL         bTraceOver;
   int     nTraceLevel;
   ZH_BOOL bNextRoutine;
   ZH_BOOL bCodeBlock;
   ZH_BOOL bToCursor;
   int     nToCursorLine;
   char *  szToCursorModule;
   int     nProcLevel;
   int     nCallStackLen;
   ZH_CALLSTACKINFO * aCallStack;
   ZH_BOOL bCBTrace;
   ZH_BOOL ( * pFunInvoke )( void );
   ZH_BOOL bInitGlobals;
   ZH_BOOL bInitStatics;
   ZH_BOOL bInitLines;
   PZH_DYNSYMBOL pDbgEntry;
} ZH_DEBUGINFO;

static ZH_DBGCOMMONINFO s_common = { 0, NULL, NULL };

static void     zh_dbgAddLocal( ZH_DEBUGINFO * info, const char * szName, int nIndex, int nFrame );
static void     zh_dbgAddModule( const char * szName );
static void     zh_dbgAddStack( ZH_DEBUGINFO * info, const char * szName, int nLine, int nProcLevel );
static void     zh_dbgAddStatic( ZH_DEBUGINFO * info, const char * szName, int nIndex, PZH_ITEM pFrame );
static void     zh_dbgAddVar( int * nVars, ZH_VARINFO ** aVars, const char * szName, char cType, int nIndex, int nFrame, PZH_ITEM pFrame );
static void     zh_dbgAddStopLines( PZH_ITEM pItem );
static void     zh_dbgEndProc( ZH_DEBUGINFO * info );
static PZH_ITEM zh_dbgEval( ZH_DEBUGINFO * info, ZH_WATCHPOINT * watch, ZH_BOOL * valid );
static PZH_ITEM zh_dbgEvalMakeBlock( ZH_WATCHPOINT * watch );
static PZH_ITEM zh_dbgEvalResolve( ZH_DEBUGINFO * info, ZH_WATCHPOINT * watch );
static ZH_BOOL  zh_dbgIsAltD( void );
static int      zh_dbgIsBreakPoint( ZH_DEBUGINFO * info, const char * szModule, int nLine );
static ZH_BOOL  zh_dbgEqual( PZH_ITEM pItem1, PZH_ITEM pItem2 );
static void     zh_dbgQuit( ZH_DEBUGINFO * info );
static void     zh_dbgRelease( void );
static PZH_ITEM zh_dbgVarGet( ZH_VARINFO * scope );
static void     zh_dbgVarSet( ZH_VARINFO * scope, PZH_ITEM xNewValue );

static const char * zh_dbgSetName( ZH_set_enum setId )
{
   switch( setId )
   {
      case ZH_SET_EXACT:         return "Exact";
      case ZH_SET_FIXED:         return "Fixed";
      case ZH_SET_DECIMALS:      return "Decimals";
      case ZH_SET_DATEFORMAT:    return "DateFormat";
      case ZH_SET_EPOCH:         return "Epoch";
      case ZH_SET_PATH:          return "Path";
      case ZH_SET_DEFAULT:       return "Default";

      case ZH_SET_EXCLUSIVE:     return "Exclusive";
      case ZH_SET_SOFTSEEK:      return "SoftSeek";
      case ZH_SET_UNIQUE:        return "Unique";
      case ZH_SET_DELETED:       return "Deleted";

      case ZH_SET_CANCEL:        return "Cancel";
      case ZH_SET_DEBUG:         return "Debug";
      case ZH_SET_TYPEAHEAD:     return "TypeAhead";

      case ZH_SET_COLOR:         return "Color";
      case ZH_SET_CURSOR:        return "Cursor";
      case ZH_SET_CONSOLE:       return "Console";
      case ZH_SET_ALTERNATE:     return "Alternate";
      case ZH_SET_ALTFILE:       return "AltFile";
      case ZH_SET_DEVICE:        return "Device";
      case ZH_SET_EXTRA:         return "Extra";
      case ZH_SET_EXTRAFILE:     return "ExtraFile";
      case ZH_SET_PRINTER:       return "Printer";
      case ZH_SET_PRINTFILE:     return "PrintFile";
      case ZH_SET_MARGIN:        return "Margin";

      case ZH_SET_BELL:          return "Bell";
      case ZH_SET_CONFIRM:       return "Confirm";
      case ZH_SET_ESCAPE:        return "Escape";
      case ZH_SET_INSERT:        return "Insert";
      case ZH_SET_EXIT:          return "Exit";
      case ZH_SET_INTENSITY:     return "Intensity";
      case ZH_SET_SCOREBOARD:    return "ScoreBoard";
      case ZH_SET_DELIMITERS:    return "Delimeters";
      case ZH_SET_DELIMCHARS:    return "DelimChars";

      case ZH_SET_WRAP:          return "Wrap";
      case ZH_SET_MESSAGE:       return "Message";
      case ZH_SET_MCENTER:       return "MCenter";
      case ZH_SET_SCROLLBREAK:   return "ScrollBreak";

      case ZH_SET_EVENTMASK:     return "EventMask";

      case ZH_SET_VIDEOMODE:     return "VideoMode";

      case ZH_SET_MBLOCKSIZE:    return "MBlockSize";
      case ZH_SET_MFILEEXT:      return "MFileExt";

      case ZH_SET_STRICTREAD:    return "StrictRead";
      case ZH_SET_OPTIMIZE:      return "Optimize";
      case ZH_SET_AUTOPEN:       return "Autopen";
      case ZH_SET_AUTORDER:      return "Autorder";
      case ZH_SET_AUTOSHARE:     return "AutoShare";

      /* Ziher SET extensions */
      case ZH_SET_LANGUAGE:      return "Language";
      case ZH_SET_IDLEREPEAT:    return "IdleRepeat";
      case ZH_SET_FILECASE:      return "FileCase";
      case ZH_SET_DIRCASE:       return "DirCase";
      case ZH_SET_DIRSEPARATOR:  return "DirSeparator";
      case ZH_SET_EOF:           return "EOF";
      case ZH_SET_HARDCOMMIT:    return "HardCommit";
      case ZH_SET_FORCEOPT:      return "ForceOpt";
      case ZH_SET_DBFLOCKSCHEME: return "DBFLockScheme";
      case ZH_SET_DEFEXTENSIONS: return "DefExtensions";
      case ZH_SET_EOL:           return "EOL";
      case ZH_SET_TRIMFILENAME:  return "TrimFileName";
      case ZH_SET_ZHOUTLOG:      return "OutLogFile";
      case ZH_SET_ZHOUTLOGINFO:  return "OutLogInfo";
      case ZH_SET_CODEPAGE:      return "CodePage";
      case ZH_SET_OSCODEPAGE:    return "OSCodePage";
      case ZH_SET_TIMEFORMAT:    return "TimeFormat";
      case ZH_SET_DBCODEPAGE:    return "DBCodePage";

      case ZH_SET_INVALID_:
         break;
   }

   return NULL;
}

static PZH_ITEM zh_dbgSetArray( void )
{
   PZH_ITEM pArray;
   int iSet, iPos;

   pArray = zh_itemArrayNew( _SET_COUNT + ZH_SET_COUNT );
   iPos = iSet = 1;
   while( iPos <= _SET_COUNT + ZH_SET_COUNT )
   {
      const char * szName = zh_dbgSetName( ( ZH_set_enum ) iSet );
      PZH_ITEM pSet = zh_arrayGetItemPtr( pArray, iPos++ );

      zh_arrayNew( pSet, ZH_DBG_SET_LEN );
      zh_arraySetNI( pSet, ZH_DBG_SET_POS, iSet );
      zh_arraySetC( pSet, ZH_DBG_SET_NAME, szName );
      zh_setGetItem( ( ZH_set_enum ) iSet,
                      zh_arrayGetItemPtr( pSet, ZH_DBG_SET_VALUE ),
                      NULL, NULL );
      if( iSet == _SET_COUNT )
         iSet = ZH_SET_BASE;
      else
         iSet++;
   }

   return pArray;
}


static PZH_ITEM zh_dbgActivateBreakArray( ZH_DEBUGINFO * info )
{
   int i;
   PZH_ITEM pArray = zh_itemArrayNew( info->nBreakPoints );

   for( i = 0; i < info->nBreakPoints; i++ )
   {
      PZH_ITEM pBreak = zh_arrayGetItemPtr( pArray, i + 1 );

      zh_arrayNew( pBreak, ZH_DBG_BP_LEN );
      if( ! info->aBreak[ i ].szFunction )
      {
         zh_arraySetNI( pBreak, ZH_DBG_BP_LINE, info->aBreak[ i ].nLine );
         zh_arraySetC( pBreak, ZH_DBG_BP_MODULE, info->aBreak[ i ].szModule );
      }
      else
         zh_arraySetC( pBreak, ZH_DBG_BP_FUNC, info->aBreak[ i ].szFunction );
   }
   return pArray;
}


static PZH_ITEM zh_dbgActivateWatchArray( ZH_DEBUGINFO * info )
{
   int i, j;
   PZH_ITEM pArray = zh_itemArrayNew( info->nWatchPoints );

   for( i = 0; i < info->nWatchPoints; i++ )
   {
      PZH_ITEM pWatch = zh_arrayGetItemPtr( pArray, i + 1 ), xValue;
      ZH_BOOL fValid;

      for( j = 0; j < info->nTracePoints; j++ )
      {
         if( info->aTrace[ j ].nIndex == i )
            break;
      }
      xValue = zh_dbgEval( info, &info->aWatch[ i ], &fValid );
      zh_arrayNew( pWatch, ZH_DBG_WP_LEN );
      zh_arraySetC( pWatch, ZH_DBG_WP_EXPR, info->aWatch[ i ].szExpr );
      zh_arraySetL( pWatch, ZH_DBG_WP_ISTRACE, j < info->nTracePoints );
      zh_arraySetL( pWatch, ZH_DBG_WP_VALID, fValid );
      if( xValue )
      {
         zh_arraySetForward( pWatch, ZH_DBG_WP_RESULT, xValue );
         zh_itemRelease( xValue );
      }
   }
   return pArray;
}


static PZH_ITEM zh_dbgActivateVarArray( PZH_ITEM pArray, int nVars, ZH_VARINFO * aVars )
{
   int i;

   zh_arrayNew( pArray, nVars );
   for( i = 0; i < nVars; i++ )
   {
      PZH_ITEM aVar = zh_arrayGetItemPtr( pArray, i + 1 );

      zh_arrayNew( aVar, ZH_DBG_VAR_LEN );

      zh_arraySetC( aVar, ZH_DBG_VAR_NAME, aVars[ i ].szName );
      zh_arraySetNL( aVar, ZH_DBG_VAR_INDEX, aVars[ i ].nIndex );
      zh_arraySetCL( aVar, ZH_DBG_VAR_TYPE, &aVars[ i ].cType, 1 );
      if( aVars[ i ].cType == 'S' )
         zh_arraySet( aVar, ZH_DBG_VAR_FRAME, aVars[ i ].frame.ptr );
      else
         zh_arraySetNL( aVar, ZH_DBG_VAR_FRAME, aVars[ i ].frame.num );
   }
   return pArray;
}


static PZH_ITEM zh_dbgActivateModuleArray( void )
{
   PZH_ITEM pArray;
   int i;

   ZH_DBGCOMMON_LOCK();

   pArray = zh_itemArrayNew( s_common.nModules );

   for( i = 0; i < s_common.nModules; i++ )
   {
      PZH_ITEM pModule = zh_arrayGetItemPtr( pArray, i + 1 );

      zh_arrayNew( pModule, ZH_DBG_MOD_LEN );
      zh_arraySetC( pModule, ZH_DBG_MOD_NAME, s_common.aModules[ i ].szModule );
      zh_dbgActivateVarArray( zh_arrayGetItemPtr( pModule, ZH_DBG_MOD_STATICS ),
                              s_common.aModules[ i ].nStatics,
                              s_common.aModules[ i ].aStatics );
      zh_dbgActivateVarArray( zh_arrayGetItemPtr( pModule, ZH_DBG_MOD_GLOBALS ),
                              s_common.aModules[ i ].nGlobals,
                              s_common.aModules[ i ].aGlobals );
      zh_dbgActivateVarArray( zh_arrayGetItemPtr( pModule, ZH_DBG_MOD_EXTGLOBALS ),
                              s_common.aModules[ i ].nExternGlobals,
                              s_common.aModules[ i ].aExternGlobals );
   }

   ZH_DBGCOMMON_UNLOCK();

   return pArray;
}


static PZH_ITEM zh_dbgActivateCallStackArray( ZH_DEBUGINFO * info )
{
   PZH_ITEM aCallStack = zh_itemArrayNew( info->nCallStackLen );
   int i;

   for( i = 0; i < info->nCallStackLen; i++ )
   {
      ZH_CALLSTACKINFO * pEntry = &info->aCallStack[ i ];
      PZH_ITEM aEntry;

      aEntry = zh_arrayGetItemPtr( aCallStack, info->nCallStackLen - i );
      zh_arrayNew( aEntry, ZH_DBG_CS_LEN );

      zh_arraySetC( aEntry, ZH_DBG_CS_MODULE, pEntry->szModule );
      zh_arraySetC( aEntry, ZH_DBG_CS_FUNCTION, pEntry->szFunction );
      zh_arraySetNL( aEntry, ZH_DBG_CS_LINE, pEntry->nLine );
      zh_arraySetNL( aEntry, ZH_DBG_CS_LEVEL, pEntry->nProcLevel );
      zh_dbgActivateVarArray( zh_arrayGetItemPtr( aEntry, ZH_DBG_CS_LOCALS ),
                              pEntry->nLocals, pEntry->aLocals );
      zh_dbgActivateVarArray( zh_arrayGetItemPtr( aEntry, ZH_DBG_CS_STATICS ),
                              pEntry->nStatics, pEntry->aStatics );
   }

   return aCallStack;
}


static void zh_dbgActivate( ZH_DEBUGINFO * info )
{
   if( ! info->pDbgEntry )
   {
      info->pDbgEntry = zh_dynsymFind( "__DBGENTRY" );
      if( info->pDbgEntry && ! zh_dynsymIsFunction( info->pDbgEntry ) )
         info->pDbgEntry = NULL;
   }

   if( info->pDbgEntry )
   {
      PZH_ITEM aCallStack, aModules, aBreak;
      ZH_BOOL bInside = info->bInside;

      aCallStack = zh_dbgActivateCallStackArray( info );
      aModules = zh_dbgActivateModuleArray();
      aBreak = zh_dbgActivateBreakArray( info );

      zh_vmPushDynSym( info->pDbgEntry );
      zh_vmPushNil();
      zh_vmPushLong( ZH_DBG_ACTIVATE );
      zh_vmPushPointer( info );
      zh_vmPushLong( info->nProcLevel );
      zh_vmPush( aCallStack );
      zh_vmPush( aModules );
      zh_vmPush( aBreak );

      zh_itemRelease( aCallStack );
      zh_itemRelease( aModules );
      zh_itemRelease( aBreak );

      info->bInside = ZH_TRUE;
      zh_vmDo( 6 );
      info->bInside = bInside;
   }
}


void zh_dbgEntry( int nMode, int nLine, const char * szName, int nIndex, PZH_ITEM pFrame )
{
   int i;
   char szProcName[ ZH_SYMBOL_NAME_LEN + ZH_SYMBOL_NAME_LEN + 5 ];
   ZH_DEBUGINFO ** infoPtr = ( ZH_DEBUGINFO ** ) zh_stackDebugInfo();
   ZH_DEBUGINFO * info = *infoPtr;
   ZH_USHORT uiLine;

   if( info == ZH_DBGINFO_DISABLE )
      return;
   else if( nMode != ZH_DBG_VMQUIT )
   {
      if( ! info )
      {
         info = *infoPtr = ( ZH_DEBUGINFO * ) zh_xgrabz( sizeof( ZH_DEBUGINFO ) );
         info->bCBTrace = ZH_TRUE;
      }
      else if( info->bInside || info->bQuit )
         return;
   }

   switch( nMode )
   {
      case ZH_DBG_MODULENAME:
         ZH_TRACE( ZH_TR_DEBUG, ( "MODULENAME %s", szName ) );

         if( szName[ strlen( szName ) - 1 ] == ':' )
            return;

         zh_procinfo( 0, szProcName, &uiLine, NULL );
         if( ! strncmp( szProcName, "(_INS", 5 ) )
            info->bInitStatics = ZH_TRUE;
         else if( ! strncmp( szProcName, "(_INITGLOBALS", 13 ) )
            info->bInitGlobals = ZH_TRUE;
         else if( ! strncmp( szProcName, "(_INITLINES", 11 ) )
            info->bInitLines = ZH_TRUE;

         if( info->bInitStatics || info->bInitGlobals )
            zh_dbgAddModule( szName );
         else if( ! strncmp( szProcName, "(b)", 3 ) )
            info->bCodeBlock = ZH_TRUE;
         else if( info->bNextRoutine )
            info->bNextRoutine = ZH_FALSE;

         zh_dbgAddStack( info, szName, uiLine, zh_dbg_ProcLevel() );
         for( i = 0; i < info->nBreakPoints; i++ )
         {
            if( info->aBreak[ i ].szFunction &&
                ! strcmp( info->aBreak[ i ].szFunction, szProcName ) )
            {
               zh_dbg_InvokeDebug( ZH_TRUE );
               break;
            }
         }
         return;

      case ZH_DBG_LOCALNAME:
         ZH_TRACE( ZH_TR_DEBUG, ( "LOCALNAME %s index %d", szName, nIndex ) );

         zh_dbgAddLocal( info, szName, nIndex, zh_dbg_ProcLevel() );
         return;

      case ZH_DBG_STATICNAME:
         ZH_TRACE( ZH_TR_DEBUG, ( "STATICNAME %s index %d frame %p", szName, nIndex, ( void * ) pFrame ) );

         zh_dbgAddStatic( info, szName, nIndex, pFrame );
         return;

      case ZH_DBG_SHOWLINE:
      {
         ZH_CALLSTACKINFO * pTop = &info->aCallStack[ info->nCallStackLen - 1 ];

         ZH_TRACE( ZH_TR_DEBUG, ( "SHOWLINE %d", nLine ) );

         /* Check if we've hit a tracepoint */
         for( i = 0; i < info->nTracePoints; i++ )
         {
            ZH_TRACEPOINT * tp = &info->aTrace[ i ];
            ZH_BOOL bOldClsScope;
            PZH_ITEM xValue;

            bOldClsScope = zh_clsSetScope( ZH_FALSE );
            xValue = zh_dbgEval( info, &info->aWatch[ tp->nIndex ], NULL );
            zh_clsSetScope( bOldClsScope );

            if( xValue != tp->xValue &&
                ( xValue == NULL || tp->xValue == NULL ||
                  ZH_ITEM_TYPE( xValue ) != ZH_ITEM_TYPE( tp->xValue ) ||
                  ! zh_dbgEqual( xValue, tp->xValue ) ) )
            {
               if( tp->xValue )
                  zh_itemRelease( tp->xValue );
               tp->xValue = xValue;

               info->bCodeBlock = ZH_FALSE;
               info->bTraceOver = ZH_FALSE;
               info->bNextRoutine = ZH_FALSE;
               info->bGo = ZH_FALSE;
               if( info->bToCursor )
               {
                  info->bToCursor = ZH_FALSE;
                  zh_xfree( info->szToCursorModule );
               }
               break;
            }
            if( xValue )
               zh_itemRelease( xValue );
         }

         if( i >= info->nTracePoints &&
             ( zh_dbgIsBreakPoint( info, pTop->szModule, nLine ) >= 0 ||
               zh_dbg_InvokeDebug( ZH_FALSE ) ||
               ( info->pFunInvoke && info->pFunInvoke() ) ) )
         {
            info->bTraceOver = ZH_FALSE;
            info->bNextRoutine = ZH_FALSE;
            info->bGo = ZH_FALSE;
            if( info->bToCursor )
            {
               info->bToCursor = ZH_FALSE;
               zh_xfree( info->szToCursorModule );
            }
         }
         /* Check if we must skip every level above info->nTraceLevel */
         else if( info->bTraceOver )
         {
            if( info->nTraceLevel < info->nCallStackLen )
               return;
            info->bTraceOver = ZH_FALSE;
         }

         /* Check if we're skipping to a specific line of source */
         if( info->bToCursor )
         {
            if( nLine == info->nToCursorLine
                && FILENAME_EQUAL( pTop->szModule, info->szToCursorModule ) )
            {
               zh_xfree( info->szToCursorModule );
               info->bToCursor = ZH_FALSE;
            }
            else
               return;
         }

         /* Check if we're skipping to the end of current routine */
         if( info->bNextRoutine )
            return;

         if( info->bCodeBlock )
         {
            info->bCodeBlock = ZH_FALSE;
            if( ! info->bCBTrace )
               return;
         }

         pTop->nLine = nLine;
         if( ! info->bGo )
         {
            info->nProcLevel = zh_dbg_ProcLevel() - ( zh_dbgIsAltD() ? 2 : 0 );
            zh_dbgActivate( info );
         }
         return;
      }

      case ZH_DBG_ENDPROC:
         if( info->bQuit )
            return;

         ZH_TRACE( ZH_TR_DEBUG, ( "ENDPROC %d", nLine ) );

         if( info->bInitLines )
            zh_dbgAddStopLines( zh_stackReturnItem() );

         info->bCodeBlock   = ZH_FALSE;
         info->bInitStatics = ZH_FALSE;
         info->bInitGlobals = ZH_FALSE;
         info->bInitLines   = ZH_FALSE;
         zh_dbgEndProc( info );
         return;

      case ZH_DBG_VMQUIT:
         if( info )
         {
            zh_dbgQuit( info );
            zh_xfree( info );
            *infoPtr = ZH_DBGINFO_DISABLE;
         }
         if( nIndex != 0 )
         {
            /* main thread exit and HVM cleanup, release common module info */
            zh_dbgRelease();
         }
         return;
   }
}


static const char * zh_dbgStripModuleName( const char * szName )
{
   const char * ptr;

   if( ( ptr = strrchr( szName, '/' ) ) != NULL )
      szName = ptr + 1;

   if( ( ptr = strrchr( szName, '\\' ) ) != NULL )
      szName = ptr + 1;

   return szName;
}


void zh_dbgAddBreak( void * handle, const char * szModule, int nLine, const char * szFunction )
{
   if( szModule || szFunction )
   {
      ZH_DEBUGINFO * info = ( ZH_DEBUGINFO * ) handle;
      ZH_BREAKPOINT * pBreak;

      pBreak = ARRAY_ADD( ZH_BREAKPOINT, info->aBreak, info->nBreakPoints );
      if( szModule )
         pBreak->szModule = zh_strdup( zh_dbgStripModuleName( szModule ) );
      else
         pBreak->szModule = NULL;
      pBreak->nLine = nLine;

      if( szFunction )
         pBreak->szFunction = zh_strdup( szFunction );
      else
         pBreak->szFunction = NULL;
   }
}


static void zh_dbgAddLocal( ZH_DEBUGINFO * info, const char * szName, int nIndex, int nFrame )
{
   if( info->bInitGlobals )
   {
      ZH_MODULEINFO * module;

      ZH_DBGCOMMON_LOCK();
      module = &s_common.aModules[ s_common.nModules - 1 ];
      zh_dbgAddVar( &module->nGlobals, &module->aGlobals, szName,
                    'G', nIndex, zh_dbg_vmVarGCount(), NULL );
      ZH_DBGCOMMON_UNLOCK();
   }
   else
   {
      ZH_CALLSTACKINFO * top = &info->aCallStack[ info->nCallStackLen - 1 ];

      zh_dbgAddVar( &top->nLocals, &top->aLocals, szName, 'L', nIndex, nFrame, NULL );
   }
}


static void zh_dbgAddModule( const char * szName )
{
   char * szModuleName;
   const char * szFuncName;
   int iLen;

   szName = zh_dbgStripModuleName( szName );
   szFuncName = strrchr( szName, ':' );
   iLen = szFuncName ? ( int ) ( szFuncName - szName ) : ( int ) strlen( szName );
   szModuleName = zh_strndup( szName, iLen );

   ZH_DBGCOMMON_LOCK();
   if( ! s_common.nModules || !FILENAME_EQUAL( s_common.aModules[ s_common.nModules - 1 ].szModule, szModuleName ) )
   {
      ZH_MODULEINFO * pModule;

      pModule = ARRAY_ADD( ZH_MODULEINFO, s_common.aModules, s_common.nModules );
      pModule->szModule = szModuleName;
      pModule->nStatics = 0;
      pModule->nGlobals = 0;
      pModule->nExternGlobals = 0;

      szModuleName = NULL;
   }
   ZH_DBGCOMMON_UNLOCK();

   if( szModuleName )
      zh_xfree( szModuleName );
}


static void zh_dbgAddStack( ZH_DEBUGINFO * info, const char * szName, int nLine, int nProcLevel )
{
   char szBuff[ ZH_SYMBOL_NAME_LEN + ZH_SYMBOL_NAME_LEN + 5 ];
   ZH_CALLSTACKINFO * top;
   const char * szFunction;

   szName = zh_dbgStripModuleName( szName );

   szFunction = strrchr( szName, ':' );
   if( szFunction )
      szFunction++;

   top = ARRAY_ADD( ZH_CALLSTACKINFO, info->aCallStack, info->nCallStackLen );
   if( info->bCodeBlock )
   {
      memcpy( szBuff, "(b)", 3 );
      zh_strncpy( szBuff + 3, szFunction, sizeof( szBuff ) - 4 );
      top->szFunction = zh_strdup( szBuff );
   }
   else
   {
      if( szFunction )
      {
         top->szFunction = zh_strdup( szFunction );
      }
      else
      {
         /* We're in an (_INSnnnnn) pseudo-function */
         zh_procinfo( 0, szBuff, NULL, NULL );
         top->szFunction = zh_strdup( szBuff );
      }
   }

   if( szFunction )
      top->szModule = zh_strndup( szName, szFunction - szName - 1 );
   else
      top->szModule = zh_strdup( szName );

   top->nProcLevel = nProcLevel;
   top->nLine = nLine;
   top->nLocals = 0;
   top->nStatics = 0;
}


static void zh_dbgAddStatic( ZH_DEBUGINFO * info, const char * szName, int nIndex, PZH_ITEM pFrame )
{
   if( info->bInitGlobals )
   {
      ZH_MODULEINFO * module;

      ZH_DBGCOMMON_LOCK();
      module = &s_common.aModules[ s_common.nModules - 1 ];
      zh_dbgAddVar( &module->nExternGlobals, &module->aExternGlobals, szName,
                    'G', nIndex, zh_dbg_vmVarGCount(), NULL );
      ZH_DBGCOMMON_UNLOCK();
   }
   else if( info->bInitStatics )
   {
      ZH_MODULEINFO * module;

      ZH_DBGCOMMON_LOCK();
      module = &s_common.aModules[ s_common.nModules - 1 ];
      zh_dbgAddVar( &module->nStatics, &module->aStatics, szName,
                    'S', nIndex, 0, pFrame );
      ZH_DBGCOMMON_UNLOCK();
   }
   else
   {
      ZH_CALLSTACKINFO * top = &info->aCallStack[ info->nCallStackLen - 1 ];

      zh_dbgAddVar( &top->nStatics, &top->aStatics, szName, 'S', nIndex, 0, pFrame );
   }
}


static void zh_dbgAddStopLines( PZH_ITEM pItem )
{
   ZH_I_SIZE i, nLinesLen;

   ZH_DBGCOMMON_LOCK();

   if( ! s_common.pStopLines )
   {
      s_common.pStopLines = zh_itemNew( pItem );
   }
   else
   {
      ZH_I_SIZE j;
      ZH_I_SIZE nItemLen = zh_itemSize( pItem );

      nLinesLen = zh_itemSize( s_common.pStopLines );

      for( i = 1; i <= nItemLen; i++ )
      {
         PZH_ITEM pEntry = zh_arrayGetItemPtr( pItem, i );
         const char * szModule = zh_arrayGetCPtr( pEntry, 1 );
         ZH_BOOL bFound = ZH_FALSE;

         szModule = zh_dbgStripModuleName( szModule );
         for( j = 1; j <= nLinesLen; j++ )
         {
            PZH_ITEM pLines = zh_arrayGetItemPtr( s_common.pStopLines, j );

            if( FILENAME_EQUAL( zh_arrayGetCPtr( pLines, 1 ), szModule ) )
            {
               /* Merge stopline info */
               ZH_I_SIZE nOrigMin = zh_arrayGetNS( pLines, 2 );
               ZH_I_SIZE nNewMin = zh_arrayGetNS( pEntry, 2 );
               ZH_I_SIZE nOrigLen = zh_arrayGetCLen( pLines, 3 );
               ZH_I_SIZE nNewLen = zh_arrayGetCLen( pEntry, 3 );
               ZH_I_SIZE nMin = ZH_MIN( nNewMin, nOrigMin );
               ZH_I_SIZE nMax = ZH_MAX( nNewMin + ( nNewLen << 3 ) - 1,
                                      nOrigMin + ( nOrigLen << 3 ) - 1 );
               const char * pOrigBuffer = zh_arrayGetCPtr( pLines, 3 );
               const char * pNewBuffer = zh_arrayGetCPtr( pEntry, 3 );
               ZH_I_SIZE nLen = ( ( nMax - nMin ) >> 3 ) + 1;
               ZH_I_SIZE k;
               char * pBuffer = ( char * ) zh_xgrabz( nLen + 1 );

               /* the bitfields with line numbers should use
                * 8bit alignment so it's safe to use byte copy
                */
               memmove( &pBuffer[ ( nNewMin - nMin ) >> 3 ], pNewBuffer, nNewLen );
               nOrigMin = ( nOrigMin - nMin ) >> 3;
               for( k = 0; k < nOrigLen; k++ )
                  pBuffer[ nOrigMin + k ] |= pOrigBuffer[ k ];

               zh_arraySetNS( pLines, 2, nMin );
               if( ! zh_arraySetCLPtr( pLines, 3, pBuffer, nLen ) )
                  zh_xfree( pBuffer );
               bFound = ZH_TRUE;
               break;
            }
         }

         if( ! bFound )
            zh_arrayAddForward( s_common.pStopLines, pEntry );
      }
   }
   nLinesLen = zh_itemSize( s_common.pStopLines );
   for( i = 1; i <= nLinesLen; i++ )
   {
      PZH_ITEM pEntry = zh_arrayGetItemPtr( s_common.pStopLines, i );
      const char * szModule = zh_arrayGetCPtr( pEntry, 1 );

      if( szModule )
      {
         const char * szName = zh_dbgStripModuleName( szModule );

         if( szName != szModule )
            zh_arraySetCLPtr( pEntry, 1, zh_strdup( szName ), strlen( szName ) );
      }
   }

   ZH_DBGCOMMON_UNLOCK();
}


static void zh_dbgAddVar( int * nVars, ZH_VARINFO ** aVars, const char * szName, char cType, int nIndex, int nFrame, PZH_ITEM pFrame )
{
   ZH_VARINFO * var;

   var = ARRAY_ADD( ZH_VARINFO, *aVars, *nVars );
   var->szName = szName;
   var->cType = cType;
   var->nIndex = nIndex;
   if( cType == 'S' )
      var->frame.ptr = pFrame;
   else
      var->frame.num = nFrame;
}


void zh_dbgAddWatch( void * handle, const char * szExpr, ZH_BOOL bTrace )
{
   if( szExpr )
   {
      ZH_DEBUGINFO * info = ( ZH_DEBUGINFO * ) handle;
      ZH_WATCHPOINT * pWatch;

      pWatch = ARRAY_ADD( ZH_WATCHPOINT, info->aWatch, info->nWatchPoints );
      pWatch->szExpr = zh_strdup( szExpr );
      pWatch->pBlock = NULL;
      pWatch->nVars = 0;

      if( bTrace )
      {
         ZH_TRACEPOINT * pTrace = ARRAY_ADD( ZH_TRACEPOINT, info->aTrace, info->nTracePoints );

         pTrace->nIndex = info->nWatchPoints - 1;
         pTrace->xValue = zh_dbgEval( info, pWatch, NULL );
      }
   }
}


static void zh_dbgClearWatch( ZH_WATCHPOINT * pWatch )
{
   zh_xfree( pWatch->szExpr );

   if( pWatch->pBlock )
      zh_itemRelease( pWatch->pBlock );

   if( pWatch->nVars )
   {
      int i;

      for( i = 0; i < pWatch->nVars; i++ )
         zh_xfree( pWatch->aVars[ i ] );

      zh_xfree( pWatch->aVars );
   }
}


static int zh_dbgCountWatch( void * handle )
{
   ZH_DEBUGINFO * info = ( ZH_DEBUGINFO * ) handle;

   return info->nWatchPoints;
}


void zh_dbgDelBreak( void * handle, int nBreak )
{
   ZH_DEBUGINFO * info = ( ZH_DEBUGINFO * ) handle;

   if( nBreak >= 0 && nBreak < info->nBreakPoints )
   {
      ZH_BREAKPOINT * pBreak = &info->aBreak[ nBreak ];

      if( pBreak->szModule )
         zh_xfree( pBreak->szModule );
      if( pBreak->szFunction )
         zh_xfree( pBreak->szFunction );

      ARRAY_DEL( ZH_BREAKPOINT, info->aBreak, info->nBreakPoints, nBreak );
   }
}


void zh_dbgDelWatch( void * handle, int nWatch )
{
   ZH_DEBUGINFO * info = ( ZH_DEBUGINFO * ) handle;

   if( nWatch >= 0 && nWatch < info->nWatchPoints )
   {
      ZH_WATCHPOINT * pWatch = &info->aWatch[ nWatch ];
      int i;

      zh_dbgClearWatch( pWatch );
      ARRAY_DEL( ZH_WATCHPOINT, info->aWatch, info->nWatchPoints, nWatch );

      for( i = 0; i < info->nTracePoints; i++ )
      {
         ZH_TRACEPOINT * pTrace = &info->aTrace[ i ];

         if( pTrace->nIndex == nWatch )
         {
            if( pTrace->xValue )
               zh_itemRelease( pTrace->xValue );

            ARRAY_DEL( ZH_TRACEPOINT, info->aTrace, info->nTracePoints, i );
            i--;
         }
         else if( pTrace->nIndex > nWatch )
            pTrace->nIndex--;
      }
   }
}

static void zh_dbgEndProc( ZH_DEBUGINFO * info )
{
   ZH_CALLSTACKINFO * top;

   if( ! info->nCallStackLen )
      return;

   top = &info->aCallStack[ --info->nCallStackLen ];
   zh_xfree( top->szFunction );
   zh_xfree( top->szModule );

   if( top->nLocals )
      zh_xfree( top->aLocals );

   if( top->nStatics )
      zh_xfree( top->aStatics );

   if( ! info->nCallStackLen )
   {
      zh_xfree( info->aCallStack );
      info->aCallStack = NULL;
   }
}


static ZH_BOOL zh_dbgEqual( PZH_ITEM pItem1, PZH_ITEM pItem2 )
{
   if( ZH_ITEM_TYPE( pItem1 ) != ZH_ITEM_TYPE( pItem2 ) )
      return ZH_FALSE;
   if( ZH_IS_NIL( pItem1 ) )
      return ZH_IS_NIL( pItem2 );
   if( ZH_IS_LOGICAL( pItem1 ) )
      return zh_itemGetL( pItem1 ) == zh_itemGetL( pItem2 );
   if( ZH_IS_POINTER( pItem1 ) )
      return zh_itemGetPtr( pItem1 ) == zh_itemGetPtr( pItem2 );
   if( ZH_IS_SYMBOL( pItem1 ) )
      return zh_itemGetSymbol( pItem1 ) == zh_itemGetSymbol( pItem2 );
   if( ZH_IS_STRING( pItem1 ) )
      return ! zh_itemStrCmp( pItem1, pItem2, ZH_TRUE );
   if( ZH_IS_NUMINT( pItem1 ) )
      return zh_itemGetNInt( pItem1 ) == zh_itemGetNInt( pItem2 );
   if( ZH_IS_NUMERIC( pItem1 ) )
      return zh_itemGetND( pItem1 ) == zh_itemGetND( pItem2 );
   if( ZH_IS_DATE( pItem1 ) )
      return zh_itemGetDL( pItem1 ) == zh_itemGetDL( pItem2 );
   if( ZH_IS_TIMESTAMP( pItem1 ) )
      return zh_itemGetTD( pItem1 ) == zh_itemGetTD( pItem2 );
   if( ZH_IS_ARRAY( pItem1 ) )
      return zh_arrayId( pItem1 ) == zh_arrayId( pItem2 );
   if( ZH_IS_HASH( pItem1 ) )
      return zh_hashId( pItem1 ) == zh_hashId( pItem2 );
   return ZH_FALSE;
}


static PZH_ITEM zh_dbgEval( ZH_DEBUGINFO * info, ZH_WATCHPOINT * watch, ZH_BOOL * valid )
{
   PZH_ITEM xResult = NULL;

   ZH_TRACE( ZH_TR_DEBUG, ( "expr %s", watch->szExpr ) );

   /* Check if we have a cached pBlock */
   if( ! watch->pBlock )
      watch->pBlock = zh_dbgEvalMakeBlock( watch );

   if( valid != NULL )
      * valid = ZH_FALSE;

   if( watch->pBlock )
   {
      PZH_ITEM aVars = zh_dbgEvalResolve( info, watch );
      PZH_ITEM aNewVars = zh_itemClone( aVars );
      ZH_BOOL bInside = info->bInside;
      int i;

      info->bInside = ZH_TRUE;

      if( zh_vmTryEval( &xResult, watch->pBlock, 1, aNewVars ) )
      {
         if( valid != NULL )
            * valid = ZH_TRUE;
      }
      else if( valid == NULL )
      {
         zh_itemRelease( xResult );
         xResult = NULL;
      }

      info->bInside = bInside;

      for( i = 0; i < watch->nVars; i++ )
      {
         PZH_ITEM xOldValue = zh_arrayGetItemPtr( aVars, i + 1 );
         PZH_ITEM xNewValue = zh_arrayGetItemPtr( aNewVars, i + 1 );

         if( ! zh_dbgEqual( xOldValue, xNewValue ) )
            zh_dbgVarSet( &watch->aScopes[ i ], xNewValue );
      }

      zh_itemRelease( aVars );
      zh_itemRelease( aNewVars );
      if( watch->nVars )
         zh_xfree( watch->aScopes );
   }
   return xResult;
}


static PZH_ITEM zh_dbgEvalMacro( const char * szExpr, PZH_ITEM pItem )
{
   PZH_ITEM pStr;
   const char * type;

   pStr = zh_itemPutC( NULL, szExpr );
   type = zh_macroGetType( pStr );
   zh_itemRelease( pStr );
   if( ! strcmp( type, "U" ) || ! strcmp( type, "UE" ) )
      return NULL;

   zh_vmPushString( szExpr, strlen( szExpr ) );
   zh_macroGetValue( zh_stackItemFromTop( -1 ), 0, ZH_SM_RT_MACRO );
   zh_itemMove( pItem, zh_stackItemFromTop( -1 ) );
   zh_stackPop();

   return pItem;
}


static int zh_dbgEvalSubstituteVar( ZH_WATCHPOINT * watch,
                                    char * szWord, int nStart, int nLen,
                                    char ** pszOrig )
{
   char buf[ 16 ];
   char * szExpr;
   ZH_SIZE n;
   int j;

   for( j = 0; j < watch->nVars; j++ )
   {
      if( ! strcmp( szWord, watch->aVars[ j ] ) )
         break;
   }

   if( j == watch->nVars )
      *ARRAY_ADD( char *, watch->aVars, watch->nVars ) = szWord;
   else
      zh_xfree( szWord );

   n = strlen( watch->szExpr );
   j = zh_snprintf( buf, sizeof( buf ), "__dbg[%d]", j + 1 );
   szExpr = ( char * ) zh_xgrab( n - nLen + j + 1 );
   memcpy( szExpr, watch->szExpr, nStart );
   memcpy( szExpr + nStart, buf, j );
   memcpy( szExpr + nStart + j, watch->szExpr + nStart + nLen, n - nLen - nStart );
   szExpr[ n + j - nLen ] = '\0';
   if( * pszOrig == NULL )
      * pszOrig = watch->szExpr;
   else
      zh_xfree( watch->szExpr );
   watch->szExpr = szExpr;

   return nStart + j;
}


static PZH_ITEM zh_dbgEvalMakeBlock( ZH_WATCHPOINT * watch )
{
   int i = 0;
   PZH_ITEM pBlock;
   ZH_BOOL bAfterId = ZH_FALSE;
   char * szBlock, * szOrig = NULL;
   ZH_I_SIZE buffsize;

   watch->nVars = 0;
   while( watch->szExpr[ i ] )
   {
      char c = watch->szExpr[ i ];

      if( ZH_ISFIRSTIDCHAR( c ) )
      {
         int nStart = i, nLen;
         int j = i;
         char * szWord;

         while( c && ZH_ISNEXTIDCHAR( c ) )
            c = watch->szExpr[ ++j ];

         nLen = j - i;
         i = j;
         if( c )
         {
            while( watch->szExpr[ i ] == ' ' )
               i++;

            if( watch->szExpr[ i ] == '(' ||
                ( nLen == 1 && i == j && watch->szExpr[ i ] == '"' ) )
               continue;

            if( watch->szExpr[ i ] == '-' && watch->szExpr[ i + 1 ] == '>' )
            {
               i += 2;

               while( ( c = watch->szExpr[ i ] ) != '\0' && ZH_ISNEXTIDCHAR( c ) )
                  i++;

               continue;
            }
         }
         szWord = zh_strupr( zh_strndup( watch->szExpr + nStart, nLen ) );
         i = zh_dbgEvalSubstituteVar( watch, szWord, nStart, nLen, &szOrig );
         bAfterId = ZH_TRUE;
         continue;
      }
      if( c == '.' )
      {
         if( watch->szExpr[ i + 1 ] &&
             strchr( "TtFf", watch->szExpr[ i + 1 ] ) &&
             watch->szExpr[ i + 2 ] == '.' )
         {
            i += 3;
         }
         else if( ! zh_strnicmp( watch->szExpr + i + 1, "OR.", 3 ) )
         {
            i += 4;
         }
         else if( ! zh_strnicmp( watch->szExpr + i + 1, "AND.", 4 ) ||
                  ! zh_strnicmp( watch->szExpr + i + 1, "NOT.", 4 ) )
         {
            i += 5;
         }
         else
         {
            i++;
         }
         bAfterId = ZH_FALSE;
         continue;
      }
      if( c == ':' ||
          ( c == '-' && watch->szExpr[ i + 1 ] == '>' &&
            ZH_ISFIRSTIDCHAR( watch->szExpr[ i + 2 ] ) ) )
      {
         if( c == ':' && watch->szExpr[ i + 1 ] == ':' )
         {
            i = zh_dbgEvalSubstituteVar( watch, zh_strdup( "SELF" ), i, 1, &szOrig );
            bAfterId = ZH_TRUE;
            continue;
         }

         if( c == '-' )
            i++;

         i++;

         while( watch->szExpr[ i ] && ZH_ISNEXTIDCHAR( watch->szExpr[ i ] ) )
            i++;

         bAfterId = ZH_TRUE;
         continue;
      }
      if( strchr( " !#$=<>(+-*/%^|,{&", c ) )
      {
         i++;
         bAfterId = ZH_FALSE;
         continue;
      }
      if( c == '\'' || c == '\"' )
      {
         i++;

         while( watch->szExpr[ i ] && watch->szExpr[ i ] != c )
            i++;

         if( watch->szExpr[ i ] )
            i++;

         bAfterId = ZH_TRUE;
         continue;
      }
      if( c == '[' )
      {
         i++;
         if( bAfterId )
            bAfterId = ZH_FALSE;
         else
         {
            while( watch->szExpr[ i ] && watch->szExpr[ i ] != ']' )
               i++;

            if( watch->szExpr[ i ] )
               i++;

            bAfterId = ZH_TRUE;
         }
         continue;
      }
      i++;
   }

   buffsize = 8 + strlen( watch->szExpr ) + 1;

   szBlock = ( char * ) zh_xgrab( buffsize + 1 );
   zh_strncpy( szBlock, "{|__dbg|", buffsize );
   zh_strncat( szBlock, watch->szExpr, buffsize );
   zh_strncat( szBlock, "}", buffsize );
   pBlock = zh_itemNew( NULL );

   if( ! zh_dbgEvalMacro( szBlock, pBlock ) )
   {
      zh_itemRelease( pBlock );
      pBlock = NULL;
   }
   zh_xfree( szBlock );

   if( szOrig != NULL )
   {
      zh_xfree( watch->szExpr );
      watch->szExpr = szOrig;
   }

   return pBlock;
}


static PZH_ITEM zh_dbgEvalResolve( ZH_DEBUGINFO * info, ZH_WATCHPOINT * watch )
{
   int i;
   ZH_CALLSTACKINFO * top = &info->aCallStack[ info->nCallStackLen - 1 ];
   PZH_ITEM aVars = zh_itemArrayNew( watch->nVars );
   ZH_VARINFO * scopes;
   ZH_MODULEINFO * module = NULL;
   int nProcLevel;

   if( ! watch->nVars )
      return aVars;

   scopes = ( ZH_VARINFO * ) zh_xgrab( watch->nVars * sizeof( ZH_VARINFO ) );
   nProcLevel = zh_dbg_ProcLevel();

   ZH_DBGCOMMON_LOCK();

   for( i = 0; i < s_common.nModules; i++ )
   {
      if( FILENAME_EQUAL( s_common.aModules[ i ].szModule, top->szModule ) )
      {
         module = &s_common.aModules[ i ];
         break;
      }
   }

   for( i = 0; i < watch->nVars; i++ )
   {
      char * name = watch->aVars[ i ];
      ZH_VARINFO * var;
      int j;
      PZH_ITEM pItem;

      for( j = 0; j < top->nLocals; j++ )
      {
         var = &top->aLocals[ j ];
         if( ! strcmp( name, var->szName ) )
         {
            scopes[ i ].cType = 'L';
            scopes[ i ].frame.num = nProcLevel - var->frame.num;
            scopes[ i ].nIndex = var->nIndex;
            zh_itemArrayPut( aVars, i + 1, zh_dbgVarGet( &scopes[ i ] ) );
            break;
         }
      }
      if( j < top->nLocals )
         continue;

      for( j = 0; j < top->nStatics; j++ )
      {
         var = &top->aStatics[ j ];
         if( ! strcmp( name, var->szName ) )
         {
            scopes[ i ].cType = 'S';
            scopes[ i ].frame.ptr = var->frame.ptr;
            scopes[ i ].nIndex = var->nIndex;
            zh_itemArrayPut( aVars, i + 1, zh_dbgVarGet( &scopes[ i ] ) );
            break;
         }
      }
      if( j < top->nStatics )
         continue;

      if( module )
      {
         for( j = 0; j < module->nStatics; j++ )
         {
            var = &module->aStatics[ j ];
            if( ! strcmp( name, var->szName ) )
            {
               scopes[ i ].cType = 'S';
               scopes[ i ].frame.ptr = var->frame.ptr;
               scopes[ i ].nIndex = var->nIndex;
               zh_itemArrayPut( aVars, i + 1, zh_dbgVarGet( &scopes[ i ] ) );
               break;
            }
         }
         if( j < module->nStatics )
            continue;

         for( j = 0; j < module->nGlobals; j++ )
         {
            var = &module->aGlobals[ j ];
            if( ! strcmp( name, var->szName ) )
            {
               scopes[ i ].cType = 'G';
               scopes[ i ].frame.num = var->frame.num;
               scopes[ i ].nIndex = var->nIndex;
               zh_itemArrayPut( aVars, i + 1, zh_dbgVarGet( &scopes[ i ] ) );
               break;
            }
         }
         if( j < module->nGlobals )
            continue;

         for( j = 0; j < module->nExternGlobals; j++ )
         {
            var = &module->aExternGlobals[ j ];
            if( ! strcmp( name, var->szName ) )
            {
               scopes[ i ].cType = 'G';
               scopes[ i ].frame.num = var->frame.num;
               scopes[ i ].nIndex = var->nIndex;
               zh_itemArrayPut( aVars, i + 1, zh_dbgVarGet( &scopes[ i ] ) );
               break;
            }
         }
         if( j < module->nExternGlobals )
            continue;
      }

      scopes[ i ].cType  = 'M';
      scopes[ i ].szName = zh_dynsymGetSymbol( name )->szName;

      pItem = zh_dbgVarGet( &scopes[ i ] );

      if( pItem )
         zh_itemArrayPut( aVars, i + 1, pItem );

      if( scopes[ i ].cType == 'F' )
         zh_itemRelease( pItem );
   }
   watch->aScopes = scopes;

   ZH_DBGCOMMON_UNLOCK();

   return aVars;
}


PZH_ITEM zh_dbgGetExpressionValue( void * handle, const char * expression )
{
   ZH_DEBUGINFO * info = ( ZH_DEBUGINFO * ) handle;
   PZH_ITEM result;
   ZH_WATCHPOINT point;

   point.szExpr = zh_strdup( expression );
   point.pBlock = NULL;
   point.nVars = 0;

   result = zh_dbgEval( info, &point, NULL );

   zh_dbgClearWatch( &point );

   return result;
}


PZH_ITEM zh_dbgGetWatchValue( void * handle, int nWatch )
{
   ZH_DEBUGINFO * info = ( ZH_DEBUGINFO * ) handle;

   if( nWatch >= 0 && nWatch < info->nWatchPoints )
      return zh_dbgEval( info, &info->aWatch[ nWatch ], NULL );
   else
      return NULL;
}


PZH_ITEM zh_dbgGetSourceFiles( void * handle )
{
   PZH_ITEM ret;
   ZH_I_SIZE nModules;
   ZH_I_SIZE i;

#if 0
   ZH_DEBUGINFO * info = ( ZH_DEBUGINFO * ) handle;
#endif
   ZH_SYMBOL_UNUSED( handle );

   ZH_DBGCOMMON_LOCK();
   nModules = zh_itemSize( s_common.pStopLines );
   ret = zh_itemArrayNew( nModules );
   for( i = 1; i <= nModules; i++ )
      zh_arraySet( ret, i, zh_arrayGetItemPtr( zh_arrayGetItemPtr( s_common.pStopLines, i ), 1 ) );
   ZH_DBGCOMMON_UNLOCK();

   return ret;
}


static ZH_BOOL zh_dbgIsAltD( void )
{
   ZH_I_SIZE nOffset = zh_stackBaseProcOffset( 1 );

   return nOffset > 0 &&
          ! strcmp( zh_itemGetSymbol( zh_stackItem( nOffset ) )->szName, "ALTD" );
}


static int zh_dbgIsBreakPoint( ZH_DEBUGINFO * info, const char * szModule, int nLine )
{
   int i;

   /* szModule has stripped path here */

   for( i = 0; i < info->nBreakPoints; i++ )
   {
      ZH_BREAKPOINT * point = &info->aBreak[ i ];

      if( point->nLine == nLine && point->szModule &&
          FILENAME_EQUAL( szModule, point->szModule ) )
         return i;
   }
   return -1;
}


ZH_BOOL zh_dbgIsValidStopLine( void * handle, const char * szModule, int nLine )
{
   ZH_BOOL fResult = ZH_FALSE;

   if( szModule )
   {
      ZH_I_SIZE nModules;
      ZH_I_SIZE i;

#if 0
      ZH_DEBUGINFO * info = ( ZH_DEBUGINFO * ) handle;
#endif
      ZH_SYMBOL_UNUSED( handle );

      szModule = zh_dbgStripModuleName( szModule );

      ZH_DBGCOMMON_LOCK();
      nModules = zh_itemSize( s_common.pStopLines );
      for( i = 1; i <= nModules; i++ )
      {
         PZH_ITEM pEntry = zh_arrayGetItemPtr( s_common.pStopLines, i );

         if( FILENAME_EQUAL( zh_arrayGetCPtr( pEntry, 1 ), szModule ) )
         {
            int nMin = zh_arrayGetNL( pEntry, 2 );
            int nOfs = nLine - nMin;

            if( nOfs >= 0 && ( ZH_SIZE ) ( nOfs >> 3 ) < zh_arrayGetCLen( pEntry, 3 ) )
               fResult = ( zh_arrayGetCPtr( pEntry, 3 )[ nOfs >> 3 ] & ( 1 << ( nOfs & 0x07 ) ) ) != 0;

            break;
         }
      }
      ZH_DBGCOMMON_UNLOCK();
   }
   return fResult;
}


const char * zh_dbgGetModuleName( void * handle, const char * szName )
{
   #if 0
   ZH_DEBUGINFO * info = ( ZH_DEBUGINFO * ) handle;
   #endif
   ZH_SYMBOL_UNUSED( handle );

   if( szName )
      szName = zh_dbgStripModuleName( szName );

   return szName;
}


static void zh_dbgQuit( ZH_DEBUGINFO * info )
{
   while( info->nWatchPoints )
   {
      zh_dbgDelWatch( info, info->nWatchPoints - 1 );
   }
   while( info->nBreakPoints )
   {
      zh_dbgDelBreak( info, info->nBreakPoints - 1 );
   }
   while( info->nCallStackLen )
   {
      zh_dbgEndProc( info );
   }
   if( info->bToCursor )
   {
      info->bToCursor = ZH_FALSE;
      zh_xfree( info->szToCursorModule );
   }
}


static void zh_dbgRelease( void )
{
   if( s_common.pStopLines )
   {
      zh_itemRelease( s_common.pStopLines );
      s_common.pStopLines = NULL;
   }
   while( s_common.nModules )
   {
      int nModules = s_common.nModules - 1;
      ZH_MODULEINFO * module = &s_common.aModules[ nModules ];
      if( module->nStatics )
      {
         zh_xfree( module->aStatics );
      }
      if( module->nGlobals )
      {
         zh_xfree( module->aGlobals );
      }
      if( module->nExternGlobals )
      {
         zh_xfree( module->aExternGlobals );
      }
      if( module->szModule )
      {
         zh_xfree( module->szModule );
      }
      ARRAY_DEL( ZH_MODULEINFO, s_common.aModules, s_common.nModules, nModules );
   }
}


void zh_dbgSetCBTrace( void * handle, ZH_BOOL bCBTrace )
{
   ZH_DEBUGINFO * info = ( ZH_DEBUGINFO * ) handle;

   info->bCBTrace = bCBTrace;
}


void zh_dbgSetGo( void * handle )
{
   ZH_DEBUGINFO * info = ( ZH_DEBUGINFO * ) handle;

   info->bGo = ZH_TRUE;
}


void zh_dbgSetInvoke( void * handle, ZH_BOOL ( * pFunInvoke )( void ) )
{
   ZH_DEBUGINFO * info = ( ZH_DEBUGINFO * ) handle;

   info->pFunInvoke = pFunInvoke;
}


void zh_dbgSetNextRoutine( void * handle )
{
   ZH_DEBUGINFO * info = ( ZH_DEBUGINFO * ) handle;

   info->bNextRoutine = ZH_TRUE;
}


void zh_dbgSetQuit( void * handle )
{
   ZH_DEBUGINFO * info = ( ZH_DEBUGINFO * ) handle;

   info->bQuit = ZH_TRUE;
}


void zh_dbgSetToCursor( void * handle, const char * szModule, int nLine )
{
   ZH_DEBUGINFO * info = ( ZH_DEBUGINFO * ) handle;

   if( szModule )
   {
      szModule = zh_dbgStripModuleName( szModule );

      info->bToCursor = ZH_TRUE;
      info->szToCursorModule = zh_strdup( szModule );
      info->nToCursorLine = nLine;
   }
}


void zh_dbgSetTrace( void * handle )
{
   ZH_DEBUGINFO * info = ( ZH_DEBUGINFO * ) handle;

   info->bTraceOver = ZH_TRUE;
   info->nTraceLevel = info->nCallStackLen;
}


void zh_dbgSetWatch( void * handle, int nWatch, const char * szExpr, ZH_BOOL bTrace )
{
   ZH_DEBUGINFO * info = ( ZH_DEBUGINFO * ) handle;

   if( nWatch >= 0 && nWatch < info->nWatchPoints && szExpr )
   {
      ZH_WATCHPOINT * pWatch = &info->aWatch[ nWatch ];
      int i;

      zh_dbgClearWatch( pWatch );
      pWatch->szExpr = zh_strdup( szExpr );
      pWatch->pBlock = NULL;
      for( i = 0; i < info->nTracePoints; i++ )
      {
         ZH_TRACEPOINT * pTrace = &info->aTrace[ i ];

         if( pTrace->nIndex == nWatch )
         {
            if( pTrace->xValue )
               zh_itemRelease( pTrace->xValue );

            ARRAY_DEL( ZH_TRACEPOINT, info->aTrace, info->nTracePoints, i );
            break;
         }
      }
      if( bTrace )
      {
         ZH_TRACEPOINT * pTrace = ARRAY_ADD( ZH_TRACEPOINT, info->aTrace, info->nTracePoints );

         pTrace->nIndex = nWatch;
         pTrace->xValue = zh_dbgEval( info, pWatch, NULL );
      }
   }
}


static PZH_ITEM zh_dbgVarGet( ZH_VARINFO * scope )
{
   switch( scope->cType )
   {
      case 'G':
         return zh_dbg_vmVarGGet( scope->frame.num, scope->nIndex );
      case 'L':
         return zh_dbg_vmVarLGet( scope->frame.num, scope->nIndex );
      case 'S':
         return zh_dbg_vmVarSGet( scope->frame.ptr, scope->nIndex );
      case 'M':
      {
         PZH_DYNSYMBOL pDyn;

         pDyn = zh_dynsymFind( scope->szName );
         if( pDyn != NULL )
         {
            PZH_ITEM pItem = zh_memvarGetValueBySym( pDyn );
            if( ! pItem )
            {
               pItem = zh_itemNew( NULL );
               if( zh_rddFieldGet( pItem, zh_dynsymSymbol( pDyn ) ) == ZH_SUCCESS )
               {
                  scope->cType = 'F';
               }
               else
               {
                  zh_itemRelease( pItem );
                  pItem = NULL;
               }
            }
            return pItem;
         }
      }
   }
   return NULL;
}


static void zh_dbgVarSet( ZH_VARINFO * scope, PZH_ITEM xNewValue )
{
   switch( scope->cType )
   {
      case 'G':
      case 'L':
      case 'S':
         zh_itemCopy( zh_dbgVarGet( scope ), xNewValue );
         break;
      case 'M':
         zh_memvarSetValue( zh_dynsymSymbol( zh_dynsymGet( scope->szName ) ),
                            xNewValue );
         break;
   }
}

/*
 * .prg functions
 */
ZH_FUNC( __DBGSETENTRY )
{
   zh_dbg_SetEntry( zh_dbgEntry );
}

ZH_FUNC( __DBGSETGO )
{
   void * ptr = zh_parptr( 1 );

   if( ptr )
      zh_dbgSetGo( ptr );
}

ZH_FUNC( __DBGSETTRACE )
{
   void * ptr = zh_parptr( 1 );

   if( ptr )
      zh_dbgSetTrace( ptr );
}

ZH_FUNC( __DBGSETCBTRACE )
{
   void * ptr = zh_parptr( 1 );

   if( ptr )
      zh_dbgSetCBTrace( ptr, zh_parl( 2 ) );
}

ZH_FUNC( __DBGSETNEXTROUTINE )
{
   void * ptr = zh_parptr( 1 );

   if( ptr )
      zh_dbgSetNextRoutine( ptr );
}

ZH_FUNC( __DBGSETQUIT )
{
   void * ptr = zh_parptr( 1 );

   if( ptr )
      zh_dbgSetQuit( ptr );
}

ZH_FUNC( __DBGSETTOCURSOR )
{
   void * ptr = zh_parptr( 1 );

   if( ptr )
      zh_dbgSetToCursor( ptr, zh_parc( 2 ), zh_parni( 3 ) );
}

ZH_FUNC( __DBGGETEXPRVALUE )
{
   void * ptr = zh_parptr( 1 );

   if( ptr )
   {
      PZH_ITEM pItem;

      if( ZH_ISCHAR( 2 ) )
         pItem = zh_dbgGetExpressionValue( ptr, zh_parc( 2 ) );
      else
         pItem = zh_dbgGetWatchValue( ptr, zh_parni( 2 ) - 1 );

      if( pItem )
      {
         zh_storl( ZH_TRUE, 3 );
         zh_itemReturnRelease( pItem );
      }
      else
         zh_storl( ZH_FALSE, 3 );
   }
}

ZH_FUNC( __DBGGETSOURCEFILES )
{
   void * ptr = zh_parptr( 1 );

   if( ptr )
      zh_itemReturnRelease( zh_dbgGetSourceFiles( ptr ) );
}

ZH_FUNC( __DBGISVALIDSTOPLINE )
{
   void * ptr = zh_parptr( 1 );

   if( ptr )
      zh_retl( zh_dbgIsValidStopLine( ptr, zh_parc( 2 ), zh_parni( 3 ) ) );
}

ZH_FUNC( __DBGADDBREAK )
{
   void * ptr = zh_parptr( 1 );

   if( ptr )
      zh_dbgAddBreak( ptr, zh_parc( 2 ), zh_parni( 3 ), zh_parc( 4 ) );
}

ZH_FUNC( __DBGDELBREAK )
{
   void * ptr = zh_parptr( 1 );

   if( ptr )
      zh_dbgDelBreak( ptr, zh_parni( 2 ) );
}

ZH_FUNC( __DBGISBREAK )
{
   void * ptr = zh_parptr( 1 );
   const char * szModule = zh_parc( 2 );

   if( ptr && szModule )
      zh_retni( zh_dbgIsBreakPoint( ( ZH_DEBUGINFO * ) ptr,
                                    zh_dbgStripModuleName( szModule ),
                                    zh_parni( 3 ) ) );
}

ZH_FUNC( __DBGGETBREAKPOINTS )
{
   void * ptr = zh_parptr( 1 );

   if( ptr )
      zh_itemReturnRelease( zh_dbgActivateBreakArray( ( ZH_DEBUGINFO * ) ptr ) );
}

ZH_FUNC( __DBGADDWATCH )
{
   void * ptr = zh_parptr( 1 );

   if( ptr )
      zh_dbgAddWatch( ptr, zh_parc( 2 ), zh_parl( 3 ) );
}

ZH_FUNC( __DBGDELWATCH )
{
   void * ptr = zh_parptr( 1 );

   if( ptr )
      zh_dbgDelWatch( ptr, zh_parni( 2 ) );
}

ZH_FUNC( __DBGSETWATCH )
{
   void * ptr = zh_parptr( 1 );

   if( ptr )
      zh_dbgSetWatch( ptr, zh_parni( 2 ), zh_parc( 3 ), zh_parl( 4 ) );
}

ZH_FUNC( __DBGCNTWATCH )
{
   void * ptr = zh_parptr( 1 );

   if( ptr )
      zh_retni( zh_dbgCountWatch( ptr ) );
}

ZH_FUNC( __DBGGETWATCHPOINTS )
{
   void * ptr = zh_parptr( 1 );

   if( ptr )
      zh_itemReturnRelease( zh_dbgActivateWatchArray( ( ZH_DEBUGINFO * ) ptr ) );
}

ZH_FUNC( __DBGGETSETS )
{
   zh_itemReturnRelease( zh_dbgSetArray() );
}

ZH_FUNC( __DBGGETMODULENAME )
{
   void * ptr = zh_parptr( 1 );

   if( ptr )
      zh_retc( zh_dbgGetModuleName( ptr, zh_parc( 2 ) ) );
}

ZH_FUNC( __DBGMODULEMATCH )
{
   void * ptr = zh_parptr( 1 );

   if( ptr )
   {
      const char * szModule1 = zh_parc( 2 ),
                 * szModule2 = zh_parc( 3 );

      zh_retl( szModule1 && szModule2 &&
               FILENAME_EQUAL( zh_dbgStripModuleName( szModule1 ),
                               zh_dbgStripModuleName( szModule2 ) ) );
   }
}

ZH_FUNC( __DBGSENDMSG )
{
   zh_dbg_objSendMessage( zh_parnl( 1 ), zh_param( 2, ZH_IT_ANY ),
                          zh_param( 3, ZH_IT_ANY ), 4 );
}
