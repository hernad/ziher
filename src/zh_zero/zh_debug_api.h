/*
 * Debugger C API
 *
 * Copyright 2007 Przemyslaw Czerpak
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

#ifndef ZH_APIDBG_H_
#define ZH_APIDBG_H_

#include "zh_api.h"

ZH_EXTERN_BEGIN

/* Debugger API */

/* HVM debugger function */
typedef void ( * ZH_DBGENTRY_FUNC )( int nMode, int nLine, const char * szName, int nIndex, PZH_ITEM pFrame );
extern ZH_EXPORT ZH_DBGENTRY_FUNC zh_dbg_SetEntry( ZH_DBGENTRY_FUNC pFunDbgEntry );
extern ZH_EXPORT ZH_BOOL zh_dbg_InvokeDebug( ZH_BOOL bInvoke );
extern ZH_EXPORT ZH_ULONG zh_dbg_ProcLevel( void );
extern ZH_EXPORT PZH_ITEM zh_dbg_vmVarSGet( PZH_ITEM pStaticsBase, int nOffset );
extern ZH_EXPORT PZH_ITEM zh_dbg_vmVarLGet( int iLevel, int iLocal );
extern ZH_EXPORT ZH_ULONG zh_dbg_vmVarGCount( void );
extern ZH_EXPORT PZH_ITEM zh_dbg_vmVarGGet( int nGlobal, int nOffset );

/* internal debugger function */
extern ZH_EXPORT void zh_dbgEntry( int nMode, int nLine, const char * szName, int nIndex, PZH_ITEM pFrame );
extern ZH_EXPORT const char * zh_dbgGetModuleName( void * handle, const char * szName );
extern ZH_EXPORT void zh_dbgAddBreak( void * handle, const char * szModule, int nLine, const char * szFunction );
extern ZH_EXPORT void zh_dbgAddWatch( void * handle, const char * szExpr, ZH_BOOL bTrace );
extern ZH_EXPORT void zh_dbgDelBreak( void * handle, int nBreak );
extern ZH_EXPORT void zh_dbgDelWatch( void * handle, int nWatch );
extern ZH_EXPORT PZH_ITEM zh_dbgGetExpressionValue( void * handle, const char * expression );
extern ZH_EXPORT PZH_ITEM zh_dbgGetSourceFiles( void * handle );
extern ZH_EXPORT PZH_ITEM zh_dbgGetWatchValue( void * handle, int nWatch );
extern ZH_EXPORT ZH_BOOL zh_dbgIsValidStopLine( void * handle, const char * szModule, int nLine );
extern ZH_EXPORT void zh_dbgSetCBTrace( void * handle, ZH_BOOL bCBTrace );
extern ZH_EXPORT void zh_dbgSetGo( void * handle );
extern ZH_EXPORT void zh_dbgSetInvoke( void * handle, ZH_BOOL ( *pFunInvoke )( void ) );
extern ZH_EXPORT void zh_dbgSetNextRoutine( void * handle );
extern ZH_EXPORT void zh_dbgSetQuit( void * handle );
extern ZH_EXPORT void zh_dbgSetToCursor( void * handle, const char * szModule, int nLine );
extern ZH_EXPORT void zh_dbgSetTrace( void * handle );
extern ZH_EXPORT void zh_dbgSetWatch( void * handle, int nWatch, const char * szExpr, ZH_BOOL bTrace );

ZH_EXTERN_END

#endif /* ZH_APIDBG_H_ */
