/*
 * Header file for the Ziher Compiler
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

#ifndef ZH_EXPROP_H_
#define ZH_EXPROP_H_

#include "zh_api.h"

ZH_EXTERN_BEGIN

/* Definitions of function templates used in expression's message
 * handling
 */
#define  ZH_EXPR_FUNC( proc )  PZH_EXPR proc( PZH_EXPR pSelf, ZH_EXPR_MESSAGE iMessage, ZH_COMP_DECL )
typedef  ZH_EXPR_FUNC( ( * PZH_EXPR_FUNC ) );

#if defined( ZH_MACRO_SUPPORT )
#define zh_comp_ExprTable     zh_macro_ExprTable
#endif

#if ! defined( ZH_COMMON_SUPPORT )
extern const PZH_EXPR_FUNC zh_comp_ExprTable[ ZH_EXPR_COUNT ];
#define  ZH_EXPR_USE( pSelf, iMessage )  \
         zh_comp_ExprTable[ (pSelf)->ExprType ]( (pSelf), (iMessage), ZH_COMP_PARAM )
#endif

extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewEmpty( ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewNil( ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewDouble( double, ZH_BYTE, ZH_BYTE, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewLong( ZH_MAXINT nValue, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewDate( long lDate, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewTimeStamp( long lDate, long lTime, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewString( const char * szValue, ZH_SIZE nLen, ZH_BOOL fDealloc, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewLogical( int iValue, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewSelf( ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewCodeBlock( char * string, ZH_SIZE nLen, int iFlags, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewVar( const char * szName, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewAliasVar( PZH_EXPR, PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewAliasExpr( PZH_EXPR, PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewMacro( PZH_EXPR, unsigned char cMacroOp, const char * szName, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewFunName( const char * szName, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewRTVar( const char * szName, PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewAlias( const char * szName, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewEQ( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewNE( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewLT( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewLE( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewGT( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewGE( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewIN( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewPlus( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewMinus( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewMult( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewDiv( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewMod( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewPower( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewAssign( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewEqual( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewPlusEq( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewMinusEq( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewMultEq( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewDivEq( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewModEq( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewExpEq( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewPostInc( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewPostDec( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewPreInc( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewPreDec( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewAnd( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewOr( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewNot( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewNegate( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewVarRef( const char * szVarName, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewFunRef( const char * szFunName, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewFunCall( PZH_EXPR, PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewRef( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewCodeblockExpr( PZH_EXPR, PZH_EXPR );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewSend( const char *, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewMacroSend( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewMethodObject( PZH_EXPR, PZH_EXPR );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewMethodCall( PZH_EXPR, PZH_EXPR );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewList( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewArgList( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewArgRef( ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewArray( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewHash( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewArrayAt( PZH_EXPR, PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprAddListExpr( PZH_EXPR, PZH_EXPR );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprCBVarAdd( PZH_EXPR, const char * szVarName, ZH_BYTE bType, ZH_COMP_DECL );
extern ZH_EXPORT_INT void zh_compExprCBVarDel( PZH_CBVAR );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprAddCodeblockExpr( PZH_EXPR, PZH_EXPR );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprSetCodeblockBody( PZH_EXPR pExpr, ZH_BYTE * pCode, ZH_SIZE nLen );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprNewIIF( PZH_EXPR );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprMacroAsAlias( PZH_EXPR );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprAssign( PZH_EXPR, PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprEqual( PZH_EXPR, PZH_EXPR );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprAssignStatic( PZH_EXPR, PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT ZH_BOOL zh_compExprListTypeCheck( PZH_EXPR pExpr, ZH_EXPRTYPE ExprType );
extern ZH_EXPORT_INT ZH_ULONG zh_compExprListLen( PZH_EXPR );
extern ZH_EXPORT_INT ZH_ULONG zh_compExprParamListLen( PZH_EXPR );
extern ZH_EXPORT_INT ZH_SIZE zh_compExprParamListCheck( ZH_COMP_DECL, PZH_EXPR );

extern ZH_EXPORT_INT const char * zh_compExprDescription( PZH_EXPR );
extern ZH_EXPORT_INT int zh_compExprType( PZH_EXPR );
extern ZH_EXPORT_INT int zh_compExprIsInteger( PZH_EXPR );
extern ZH_EXPORT_INT int zh_compExprIsLong( PZH_EXPR );
extern ZH_EXPORT_INT int zh_compExprAsInteger( PZH_EXPR );
extern ZH_EXPORT_INT int zh_compExprAsNumSign( PZH_EXPR );
extern ZH_EXPORT_INT int zh_compExprIsString( PZH_EXPR );
extern ZH_EXPORT_INT ZH_SIZE zh_compExprAsStringLen( PZH_EXPR );
extern ZH_EXPORT_INT ZH_MAXINT zh_compExprAsLongNum( PZH_EXPR );
extern ZH_EXPORT_INT const char * zh_compExprAsString( PZH_EXPR );
extern ZH_EXPORT_INT const char * zh_compExprAsSymbol( PZH_EXPR );
extern ZH_EXPORT_INT ZH_BOOL zh_compExprIsArrayToParams( PZH_EXPR );

extern ZH_EXPORT_INT PZH_EXPR zh_compExprListStrip( PZH_EXPR, ZH_COMP_DECL );

extern ZH_EXPORT_INT PZH_EXPR zh_compExprSetOperand( PZH_EXPR, PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprSetGetBlock( PZH_EXPR pExpr, ZH_COMP_DECL );

extern ZH_EXPORT_INT void zh_compExprDelOperator( PZH_EXPR, ZH_COMP_DECL );

extern ZH_EXPORT_INT PZH_EXPR zh_compExprReducePower( PZH_EXPR pSelf, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprReduceMod( PZH_EXPR pSelf, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprReduceDiv( PZH_EXPR pSelf, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprReduceMult( PZH_EXPR pSelf, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprReduceMinus( PZH_EXPR pSelf, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprReducePlus( PZH_EXPR pSelf, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprReduceNegate( PZH_EXPR pSelf, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprReduceIN( PZH_EXPR pSelf, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprReduceNE( PZH_EXPR pSelf, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprReduceGE( PZH_EXPR pSelf, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprReduceLE( PZH_EXPR pSelf, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprReduceGT( PZH_EXPR pSelf, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprReduceLT( PZH_EXPR pSelf, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprReduceEQ( PZH_EXPR pSelf, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprReduceAnd( PZH_EXPR pSelf, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprReduceOr( PZH_EXPR pSelf, ZH_COMP_DECL );
extern ZH_EXPORT_INT PZH_EXPR zh_compExprReduceIIF( PZH_EXPR, ZH_COMP_DECL );

extern ZH_EXPORT_INT ZH_BOOL zh_compExprReduceAT( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT ZH_BOOL zh_compExprReduceCHR( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT ZH_BOOL zh_compExprReduceBCHAR( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT ZH_BOOL zh_compExprReduceLEN( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT ZH_BOOL zh_compExprReduceASC( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT ZH_BOOL zh_compExprReduceBCODE( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT ZH_BOOL zh_compExprReduceINT( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT ZH_BOOL zh_compExprReduceEMPTY( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT ZH_BOOL zh_compExprReduceSTOT( PZH_EXPR, ZH_USHORT usCount, ZH_COMP_DECL );
extern ZH_EXPORT_INT ZH_BOOL zh_compExprReduceSTOD( PZH_EXPR, ZH_USHORT usCount, ZH_COMP_DECL );
extern ZH_EXPORT_INT ZH_BOOL zh_compExprReduceDTOS( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT ZH_BOOL zh_compExprReduceCTOD( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT ZH_BOOL zh_compExprReduceUPPER( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT ZH_BOOL zh_compExprReduceMIN( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT ZH_BOOL zh_compExprReduceMAX( PZH_EXPR, ZH_COMP_DECL );
extern ZH_EXPORT_INT ZH_BOOL zh_compExprReduceBitFunc( PZH_EXPR, ZH_MAXINT nResult, ZH_BOOL fBool, ZH_COMP_DECL );

ZH_EXTERN_END

#endif  /* ZH_EXPROP_H_ */
