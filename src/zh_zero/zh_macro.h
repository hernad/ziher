/*
 * Header file for the Macro compiler
 *
 * Copyright 1999 Ryszard Glab
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

#ifndef ZH_MACRO_H_
#define ZH_MACRO_H_

#include "zh_api.h"
#include "zh_comp_macro_shared.h"
#include "zh_item_api.h"
#include "zh_error_api.h"
#include "zh_vm.h"
#include "zh_expr_op.h"
#include "zh_pcode.h"
#include "zh_macro.h"

ZH_EXTERN_BEGIN

/* flags for compilation process
 */
#define ZH_MACRO_GEN_PUSH     1   /* generate PUSH pcodes */
#define ZH_MACRO_GEN_POP      2   /* generate POP pcodes */
#define ZH_MACRO_GEN_ALIASED  4   /* force aliased variable */
#define ZH_MACRO_GEN_TYPE     8   /* check the type of expression (from Type() function) */
#define ZH_MACRO_GEN_PARE     16  /* generate parenthesized list */
#define ZH_MACRO_GEN_LIST     32  /* generate push operation for every comma separated expressions */
#define ZH_MACRO_GEN_REFER    64  /* generate PUSH pcodes for reference to given expression */

/* values returned from compilation process
 */
#define ZH_MACRO_OK           0   /* macro compiled successfully */
#define ZH_MACRO_FAILURE      1   /* syntax error */

/* additional status of compilation
 */
#define ZH_MACRO_CONT         1   /* everything is OK so far */
#define ZH_MACRO_TOO_COMPLEX  2   /* compiled expression is too complex */
#define ZH_MACRO_UDF          4   /* code uses UDF function (info used by Type() function) */
#define ZH_MACRO_UNKN_SYM     8   /* requested symbol was not found in runtime symbol table */
#define ZH_MACRO_UNKN_VAR     16  /* requested variable doesn't exist */

/* Global functions
 */
extern void zh_macroError( int iError, ZH_COMP_DECL );
extern int zh_macroYYParse( PZH_MACRO pMacro );
extern int zh_macroSetMacro( ZH_BOOL fSet, int flag );
extern ZH_ULONG zh_macroAutoSetMacro( ZH_ULONG ulFlag );
extern ZH_BOOL zh_macroLexNew( PZH_MACRO pMacro );
extern void zh_macroLexDelete( PZH_MACRO pMacro );
extern char * zh_macroIdentNew( ZH_COMP_DECL, char * );

extern PZH_EXPR zh_macroExprGenPush( PZH_EXPR, ZH_COMP_DECL );
extern PZH_EXPR zh_macroExprGenPop( PZH_EXPR, ZH_COMP_DECL );

extern PZH_EXPR zh_macroExprNewArrayAt( PZH_EXPR pArray, PZH_EXPR pIndex, ZH_COMP_DECL );
extern PZH_EXPR zh_macroExprNewFunCall( PZH_EXPR pName, PZH_EXPR pParms, ZH_COMP_DECL );

/* Size of pcode buffer incrementation
 */
#define ZH_PCODE_SIZE  512

/* Declarations for functions macro.c */
#if defined( ZH_MACRO_SUPPORT )

extern void zh_macroGenPCode1( ZH_BYTE byte, ZH_COMP_DECL );
extern void zh_macroGenPCode2( ZH_BYTE byte1, ZH_BYTE byte2, ZH_COMP_DECL );
extern void zh_macroGenPCode3( ZH_BYTE byte1, ZH_BYTE byte2, ZH_BYTE byte3, ZH_COMP_DECL );
extern void zh_macroGenPCode4( ZH_BYTE byte1, ZH_BYTE byte2, ZH_BYTE byte3, ZH_BYTE byte4, ZH_COMP_DECL );
extern void zh_macroGenPCodeN( const ZH_BYTE * pBuffer, ZH_SIZE nSize, ZH_COMP_DECL );

extern ZH_SIZE zh_macroGenJump( ZH_I_SIZE nOffset, ZH_COMP_DECL );
extern ZH_SIZE zh_macroGenJumpFalse( ZH_I_SIZE nOffset, ZH_COMP_DECL );
extern void zh_macroGenJumpThere( ZH_SIZE nFrom, ZH_SIZE nTo, ZH_COMP_DECL );
extern void zh_macroGenJumpHere( ZH_SIZE nOffset, ZH_COMP_DECL );
extern ZH_SIZE zh_macroGenJumpTrue( ZH_I_SIZE nOffset, ZH_COMP_DECL );

extern void zh_macroGenPushSymbol( const char * szSymbolName, ZH_BOOL bFunction, ZH_COMP_DECL );
extern void zh_macroGenPushLong( ZH_MAXINT nNumber, ZH_COMP_DECL );
extern void zh_macroGenPushDate( long lDate, ZH_COMP_DECL );
extern void zh_macroGenPushTimeStamp( long lDate, long lTime, ZH_COMP_DECL );
extern void zh_macroGenMessage( const char * szMsgName, ZH_BOOL bIsObject, ZH_COMP_DECL );
extern void zh_macroGenMessageData( const char * szMsg, ZH_BOOL bIsObject, ZH_COMP_DECL );
extern void zh_macroGenPopVar( const char * szVarName, ZH_COMP_DECL );
extern void zh_macroGenPopMemvar( const char * szVarName, ZH_COMP_DECL );
extern void zh_macroGenPopAliasedVar( const char * szVarName,
                                      ZH_BOOL bPushAliasValue,
                                      const char * szAlias,
                                      ZH_MAXINT nWorkarea, ZH_COMP_DECL );
extern void zh_macroGenPushVar( const char * szVarName, ZH_COMP_DECL );
extern void zh_macroGenPushVarRef( const char * szVarName, ZH_COMP_DECL );
extern void zh_macroGenPushMemvarRef( const char * szVarName, ZH_COMP_DECL );
extern void zh_macroGenPushAliasedVar( const char * szVarName,
                                       ZH_BOOL bPushAliasValue,
                                       const char * szAlias,
                                       ZH_MAXINT nWorkarea, ZH_COMP_DECL );
extern void zh_macroGenPushLogical( int iTrueFalse, ZH_COMP_DECL );
extern void zh_macroGenPushDouble( double dNumber, ZH_BYTE bWidth, ZH_BYTE bDec, ZH_COMP_DECL );
extern void zh_macroGenPushFunCall( const char * szFunName, int iFlags, ZH_COMP_DECL );
extern void zh_macroGenPushFunSym( const char * szFunName, int iFlags, ZH_COMP_DECL );
extern void zh_macroGenPushFunRef( const char * szFunName, ZH_COMP_DECL );
extern void zh_macroGenPushString( const char * szText, ZH_SIZE nStrLen, ZH_COMP_DECL );

extern void zh_macroCodeBlockStart( ZH_COMP_DECL );
extern void zh_macroCodeBlockEnd( ZH_COMP_DECL );

extern int zh_macroLocalVarGetPos( const char * szVarName, ZH_COMP_DECL );
extern ZH_BOOL zh_macroIsValidMacroText( const char * szText, ZH_SIZE nLen );

#endif /* ZH_MACRO_SUPPORT */

ZH_EXTERN_END

#endif /* ZH_MACRO_H_ */
