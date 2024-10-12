/*
 * Header file for the Ziher Compiler
 *
 * Copyright 1999 Antonio Linares <alinares@fivetechsoft.com>
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

#ifndef ZH_COMP_H_
#define ZH_COMP_H_

#include "zh_api.h"
#include "macro.zhh"
#include "zh_errors.h"
#include "zh_pp.h"
#include "zh_macro.h"
#include "zh_expr_op.h"
#include "zh_pcode.h"
#include "zh_hash.h"

ZH_EXTERN_BEGIN

extern ZH_I_SIZE zh_compPCodeSize( PZH_ZFUNC, ZH_SIZE );
extern void zh_compPCodeEval( PZH_ZFUNC, const PZH_PCODE_FUNC *, void * );
extern void zh_compPCodeTrace( PZH_ZFUNC, const PZH_PCODE_FUNC *, void * );

extern void zh_compGenLabelTable( PZH_ZFUNC pFunc, PZH_LABEL_INFO label_info );
extern PZH_DEBUGINFO zh_compGetDebugInfo( ZH_COMP_DECL );

extern void zh_compInitPP( ZH_COMP_DECL, PZH_PP_OPEN_FUNC pOpenFunc );
extern void zh_compCompileEnd( ZH_COMP_DECL );

extern int  zh_comp_yyparse( ZH_COMP_DECL );
extern void zh_compParserStop( ZH_COMP_DECL );
extern void zh_compParserRun( ZH_COMP_DECL );

#define ZH_VSCOMP_NONE       0
#define ZH_VSCOMP_LOCAL      1
#define ZH_VSCOMP_STATIC     2
#define ZH_VSCOMP_FIELD      4
#define ZH_VSCOMP_PARAMETER  8
#define ZH_VSCOMP_THREAD     16
#define ZH_VSCOMP_PRIVATE    64
#define ZH_VSCOMP_PUBLIC     128
#define ZH_VSCOMP_MEMVAR     ( ZH_VSCOMP_PUBLIC | ZH_VSCOMP_PRIVATE )
#define ZH_VSCOMP_TH_STATIC  ( ZH_VSCOMP_STATIC | ZH_VSCOMP_THREAD )

/* return detailed information about a class of variable */
#define ZH_VS_UNDECLARED      0
/* variables declared in a current codeblock/function/procedure */
#define ZH_VS_CBLOCAL_VAR     1     /* func/proc local variables and parameters used in codeblock (detached) */
#define ZH_VS_LOCAL_VAR       2     /* local variables and parameters */
#define ZH_VS_LOCAL_MEMVAR    4
#define ZH_VS_LOCAL_FIELD     8
#define ZH_VS_STATIC_VAR     16
/* variables declared outside of a current function/procedure */
#define ZH_VS_FILEWIDE       32
#define ZH_VS_GLOBAL_MEMVAR   ( ZH_VS_FILEWIDE | ZH_VS_LOCAL_MEMVAR )
#define ZH_VS_GLOBAL_FIELD    ( ZH_VS_FILEWIDE | ZH_VS_LOCAL_FIELD )
#define ZH_VS_GLOBAL_STATIC   ( ZH_VS_FILEWIDE | ZH_VS_STATIC_VAR )

#define ZH_VU_NOT_USED           0
#define ZH_VU_INITIALIZED        1
#define ZH_VU_USED               2

#define ZH_VT_OFFSET_BYREF       60
#define ZH_VT_OFFSET_VARIANT     90
#define ZH_VT_OFFSET_OPTIONAL    90

/*
 * flags for funFlags member
 */
#define ZH_FUNF_STATEMENTS        0x0001   /* Function have at least one executable statement */
#define ZH_FUNF_USES_STATICS      0x0002   /* Function uses static variables */
#define ZH_FUNF_PROCEDURE         0x0004   /* This is a procedure that shouldn't return value */
#define ZH_FUNF_BREAK_CODE        0x0008   /* last statement breaks execution flow */
#define ZH_FUNF_USES_LOCAL_PARAMS 0x0010   /* parameters are declared using () */
#define ZH_FUNF_WITH_RETURN       0x0020   /* there was RETURN statement in previous line */
#define ZH_FUNF_EXTBLOCK          0x0040   /* it's extended codeblock */
#define ZH_FUNF_FILE_DECL         0x0080   /* pseudo function with file wide declarations */
#define ZH_FUNF_FILE_FIRST        0x0100   /* 1st real or pseudo function in compiled .zh module */
#define ZH_FUNF_ATTACHED          0x0200   /* function attached to function list */

extern               void         zh_compFunctionAdd( ZH_COMP_DECL, const char * szFunName, ZH_SYMBOLSCOPE cScope, int iType );
extern               PZH_HINLINE  zh_compInlineAdd( ZH_COMP_DECL, const char * szFunName, int iLine );
extern               void         zh_compFunctionMarkStatic( ZH_COMP_DECL, const char * szFunName );
extern ZH_EXPORT_INT const char * zh_compGetFuncID( const char * szFuncName, ZH_FUNC_ID * pFunID, int * piFlags );
extern               ZH_BOOL      zh_compFunCallCheck( ZH_COMP_DECL, const char *, int );

extern PZH_VARTYPE zh_compVarTypeNew( ZH_COMP_DECL, ZH_BYTE cVarType, const char * szFromClass );
extern void zh_compVariableAdd( ZH_COMP_DECL, const char * szVarName, PZH_VARTYPE pVarType ); /* add a new param, local, static variable to a function definition or a public or private */
extern PZH_HVAR zh_compVariableFind( ZH_COMP_DECL, const char * szVarName, int * piPos, int * piScope );
extern const char * zh_compLocalVariableName( PZH_ZFUNC pFunc, ZH_USHORT wVar );   /* returns the name of local variable */
extern const char * zh_compStaticVariableName( ZH_COMP_DECL, ZH_USHORT wVar );   /* returns the name of static variable */

#define ZH_SYM_MEMVAR   ZH_FALSE
#define ZH_SYM_ALIAS    ZH_FALSE
#define ZH_SYM_MSGNAME  ZH_FALSE
#define ZH_SYM_FUNCNAME ZH_TRUE
extern const char * zh_compSymbolName( ZH_COMP_DECL, ZH_USHORT );   /* returns a symbol name based on its index on the symbol table */

extern PZH_HDECLARED zh_compDeclaredAdd( ZH_COMP_DECL, const char * );

extern PZH_HCLASS zh_compClassAdd( ZH_COMP_DECL, const char *, const char * );
extern PZH_HCLASS zh_compClassFind( ZH_COMP_DECL, const char * );
extern PZH_HDECLARED zh_compMethodAdd( ZH_COMP_DECL, PZH_HCLASS pClass, const char * );
extern PZH_HDECLARED zh_compMethodFind( PZH_HCLASS pClass, const char * );
extern void zh_compDeclaredParameterAdd( ZH_COMP_DECL, const char * szVarName, PZH_VARTYPE pVarType );

extern void zh_compGenBreak( ZH_COMP_DECL );  /* generate code for BREAK statement */

extern void zh_compExternAdd( ZH_COMP_DECL, const char * szExternName, ZH_SYMBOLSCOPE cScope ); /* defines a new extern name */

extern void zh_compModuleAdd( ZH_COMP_DECL, const char * szModuleName, ZH_BOOL fForce );

extern void zh_compRTVariableKill( ZH_COMP_DECL, PZH_ZFUNC );
extern void zh_compSwitchKill( ZH_COMP_DECL, PZH_ZFUNC );
extern void zh_compElseIfKill( PZH_ZFUNC );
extern void zh_compLoopKill( PZH_ZFUNC );

extern void zh_compGenError( ZH_COMP_DECL, const char * const szErrors[], char cPrefix, int iError, const char * szError1, const char * szError2 ); /* generic parsing error management function */
extern void zh_compGenWarning( ZH_COMP_DECL, const char * const szWarnings[], char cPrefix, int iWarning, const char * szWarning1, const char * szWarning2); /* generic parsing warning management function */

extern ZH_SIZE zh_compGenJump( ZH_I_SIZE nOffset, ZH_COMP_DECL );             /* generates the pcode to jump to a specific offset */
extern ZH_SIZE zh_compGenJumpFalse( ZH_I_SIZE nOffset, ZH_COMP_DECL );        /* generates the pcode to jump if false */
extern ZH_SIZE zh_compGenJumpTrue( ZH_I_SIZE nOffset, ZH_COMP_DECL );         /* generates the pcode to jump if true */
extern void    zh_compGenJumpHere( ZH_SIZE nOffset, ZH_COMP_DECL );        /* returns the pcode pos where to set a jump offset */
extern void    zh_compGenJumpThere( ZH_SIZE nFrom, ZH_SIZE nTo, ZH_COMP_DECL );   /* sets a jump offset */

extern void zh_compGenModuleName( ZH_COMP_DECL, const char * szFunName );  /* generates the pcode with the currently compiled module and function name */
extern void zh_compLinePush( ZH_COMP_DECL );             /* generates the pcode with the currently compiled source code line */
extern void zh_compLinePushIfDebugger( ZH_COMP_DECL );   /* generates the pcode with the currently compiled source code line */
extern void zh_compLinePushIfInside( ZH_COMP_DECL );     /* generates the pcode with the currently compiled source code line */
extern void zh_compStatmentStart( ZH_COMP_DECL );        /* Check if we can start statement (without line pushing) */

extern void zh_compGenMessage( const char * szMsgName, ZH_BOOL bIsObject, ZH_COMP_DECL );    /* sends a message to an object */
extern void zh_compGenMessageData( const char * szMsg, ZH_BOOL bIsObject, ZH_COMP_DECL );    /* generates an underscore-symbol name for a data assignment */
extern void zh_compGenPopVar( const char * szVarName, ZH_COMP_DECL );                        /* generates the pcode to pop a value from the virtual machine stack onto a variable */
extern void zh_compGenPopMemvar( const char * szVarName, ZH_COMP_DECL );                     /* generates the pcode to pop a value from the virtual machine stack onto a memvar variable */
extern void zh_compGenPushDouble( double dNumber, ZH_BYTE bWidth, ZH_BYTE bDec, ZH_COMP_DECL );    /* Pushes a number on the virtual machine stack */
extern void zh_compGenPushFunCall( const char *, int, ZH_COMP_DECL );                             /* generates the pcode to push function's call */
extern void zh_compGenPushFunSym( const char *, int, ZH_COMP_DECL );                              /* generates the pcode to push function's symbol */
extern void zh_compGenPushFunRef( const char *, ZH_COMP_DECL );                              /* generates the pcode to push function's reference symbol */
extern void zh_compGenPushVar( const char * szVarName, ZH_COMP_DECL );                       /* generates the pcode to push a variable value to the virtual machine stack */
extern void zh_compGenPushVarRef( const char * szVarName, ZH_COMP_DECL );                    /* generates the pcode to push a variable by reference to the virtual machine stack */
extern void zh_compGenPushMemvarRef( const char * szVarName, ZH_COMP_DECL );                 /* generates the pcode to push memvar variable by reference to the virtual machine stack */
extern void zh_compGenPushInteger( int iNumber, ZH_COMP_DECL );                              /* Pushes a integer number on the virtual machine stack */
extern void zh_compGenPushLogical( int iTrueFalse, ZH_COMP_DECL );                           /* pushes a logical value on the virtual machine stack */
extern void zh_compGenPushLong( ZH_MAXINT nNumber, ZH_COMP_DECL );                           /* Pushes a long number on the virtual machine stack */
extern void zh_compGenPushDate( long lDate, ZH_COMP_DECL );                                  /* Pushes a date constant on the virtual machine stack */
extern void zh_compGenPushTimeStamp( long lDate, long lTime, ZH_COMP_DECL );                 /* Pushes a timestamp constant on the virtual machine stack */
extern void zh_compGenPushNil( ZH_COMP_DECL );                                               /* Pushes nil on the virtual machine stack */
extern void zh_compGenPushString( const char * szText, ZH_SIZE nLen, ZH_COMP_DECL );         /* Pushes a string on the virtual machine stack */
extern void zh_compGenPushSymbol( const char * szSymbolName, ZH_BOOL bFunction, ZH_COMP_DECL ); /* Pushes a symbol on to the Virtual machine stack */
extern void zh_compGenPushAliasedVar( const char * szVarName, ZH_BOOL bPushAliasValue, const char * szAlias, ZH_MAXINT nWorkarea, ZH_COMP_DECL );
extern void zh_compGenPopAliasedVar( const char * szVarName, ZH_BOOL bPushAliasValue, const char * szAlias, ZH_MAXINT nWorkarea, ZH_COMP_DECL );
extern void zh_compGenPCode1( ZH_BYTE, ZH_COMP_DECL ); /* generates 1 byte of pcode */
extern void zh_compGenPCode2( ZH_BYTE, ZH_BYTE, ZH_COMP_DECL ); /* generates 2 bytes of pcode + flag for optional StrongType(). */
extern void zh_compGenPCode3( ZH_BYTE, ZH_BYTE, ZH_BYTE, ZH_COMP_DECL ); /* generates 3 bytes of pcode + flag for optional StrongType() */
extern void zh_compGenPCode4( ZH_BYTE, ZH_BYTE, ZH_BYTE, ZH_BYTE, ZH_COMP_DECL ); /* generates 4 bytes of pcode + flag for optional StrongType() */
extern void zh_compGenPCodeN( const ZH_BYTE * pBuffer, ZH_SIZE nSize, ZH_COMP_DECL ); /* copy bytes to a pcode buffer + flag for optional StrongType() */

extern ZH_SIZE zh_compSequenceBegin( ZH_COMP_DECL );
extern ZH_SIZE zh_compSequenceEnd( ZH_COMP_DECL );
extern ZH_SIZE zh_compSequenceAlways( ZH_COMP_DECL );
extern void zh_compSequenceFinish( ZH_COMP_DECL, ZH_SIZE nStartPos, ZH_SIZE nEndPos,
                                   ZH_SIZE nAlways, ZH_BOOL fUsualStmts, ZH_BOOL fRecover,
                                   ZH_BOOL fCanMove );

/* support for FIELD declaration */
extern void zh_compFieldSetAlias( ZH_COMP_DECL, const char * szAlias, int iField );
extern int  zh_compFieldsCount( ZH_COMP_DECL );

/* Static variables */
extern void zh_compStaticDefStart( ZH_COMP_DECL );
extern void zh_compStaticDefEnd( ZH_COMP_DECL, const char * szVarName );

extern ZH_BOOL zh_compCheckUnclosedStru( ZH_COMP_DECL, PZH_ZFUNC );

#define ZH_COMP_ERROR_TYPE( x )     ZH_COMP_PARAM->funcs->ErrorType( ZH_COMP_PARAM, x )
#define ZH_COMP_ERROR_SYNTAX( x )   ZH_COMP_PARAM->funcs->ErrorSyntax( ZH_COMP_PARAM, x )
#define ZH_COMP_ERROR_DUPLVAR( s )  ZH_COMP_PARAM->funcs->ErrorDuplVar( ZH_COMP_PARAM, s )

#define ZH_COMP_EXPR_NEW( i )       ZH_COMP_PARAM->funcs->ExprNew( ZH_COMP_PARAM, i )
#define ZH_COMP_EXPR_FREE( x )      ZH_COMP_PARAM->funcs->ExprFree( ZH_COMP_PARAM, x )
#define ZH_COMP_EXPR_CLEAR( x )     ZH_COMP_PARAM->funcs->ExprClear( ZH_COMP_PARAM, x )

#if defined( ZH_MACRO_SUPPORT )

#define ZH_GEN_FUNC1( func, p1 )          zh_macroGen##func( p1, ZH_COMP_PARAM )
#define ZH_GEN_FUNC2( func, p1,p2 )       zh_macroGen##func( p1, p2, ZH_COMP_PARAM )
#define ZH_GEN_FUNC3( func, p1,p2,p3 )    zh_macroGen##func( p1, p2, p3, ZH_COMP_PARAM )
#define ZH_GEN_FUNC4( func, p1,p2,p3,p4 ) zh_macroGen##func( p1, p2, p3, p4, ZH_COMP_PARAM )

#define zh_compErrorIndex( p, x )         zh_macroError( EG_BOUND, ( p ) )
#define zh_compErrorLValue( p, x )        zh_macroError( EG_SYNTAX, ( p ) )
#define zh_compErrorBound( p, x )         zh_macroError( EG_BOUND, ( p ) )
#define zh_compErrorAlias( p, x )         zh_macroError( EG_NOALIAS, ( p ) )
#define zh_compErrorRefer( p, x, c )      zh_macroError( EG_SYNTAX, ( p ) )
#define zh_compErrorVParams( p, x )       zh_macroError( EG_SYNTAX, ( p ) )
#define zh_compWarnMeaningless( p, x )
#define zh_compErrorMacro( p, x )

#elif ! defined( ZH_COMMON_SUPPORT )

#define ZH_GEN_FUNC1( func, p1 )          zh_compGen##func( p1, ZH_COMP_PARAM )
#define ZH_GEN_FUNC2( func, p1,p2 )       zh_compGen##func( p1, p2, ZH_COMP_PARAM )
#define ZH_GEN_FUNC3( func, p1,p2,p3 )    zh_compGen##func( p1, p2, p3, ZH_COMP_PARAM )
#define ZH_GEN_FUNC4( func, p1,p2,p3,p4 ) zh_compGen##func( p1, p2, p3, p4, ZH_COMP_PARAM )

extern int  zh_compMain( int argc, const char * const argv[] );
extern int  zh_compMainExt( int argc, const char * const argv[], ZH_BYTE ** pBufPtr, ZH_SIZE * pnSize, const char * szSource, int iStartLine, void * cargo, PZH_PP_OPEN_FUNC pOpenFunc, PZH_PP_MSG_FUNC pMsgFunc );
extern void zh_compOutStd( ZH_COMP_DECL, const char * szMessage );
extern void zh_compOutErr( ZH_COMP_DECL, const char * szMessage );

extern PZH_EXPR zh_compExprGenStatement( PZH_EXPR, ZH_COMP_DECL );
extern PZH_EXPR zh_compExprGenPush( PZH_EXPR, ZH_COMP_DECL );
extern PZH_EXPR zh_compExprGenPop( PZH_EXPR, ZH_COMP_DECL );
extern PZH_EXPR zh_compExprReduce( PZH_EXPR, ZH_COMP_DECL );

extern PZH_EXPR zh_compErrorIndex( ZH_COMP_DECL, PZH_EXPR );
extern PZH_EXPR zh_compErrorLValue( ZH_COMP_DECL, PZH_EXPR );
extern PZH_EXPR zh_compErrorBound( ZH_COMP_DECL, PZH_EXPR );
extern PZH_EXPR zh_compErrorAlias( ZH_COMP_DECL, PZH_EXPR );
extern PZH_EXPR zh_compErrorRefer( ZH_COMP_DECL, PZH_EXPR, const char * );
extern PZH_EXPR zh_compWarnMeaningless( ZH_COMP_DECL, PZH_EXPR );
extern void     zh_compErrorMacro( ZH_COMP_DECL, const char * szText );
extern void     zh_compErrorVParams( ZH_COMP_DECL, const char * szFuncOrBlock );

extern PZH_EXPR zh_compErrorStatic( ZH_COMP_DECL, const char *, PZH_EXPR );
extern void     zh_compErrorCodeblockDecl( ZH_COMP_DECL, const char * szVarName );
extern void     zh_compErrorCodeblockWith( ZH_COMP_DECL, const char * szMessage );

extern void     zh_compPushMacroVar( ZH_COMP_DECL, const char * szText );
extern void     zh_compPushMacroText( ZH_COMP_DECL, const char * szText, ZH_SIZE nLen, ZH_BOOL fMacro );

/* Codeblocks */
extern void zh_compCodeBlockStart( ZH_COMP_DECL, int iEarlyEvalPass );  /* starts a codeblock creation */
extern void zh_compCodeBlockEnd( ZH_COMP_DECL );                        /* end of codeblock creation */
extern void zh_compCodeBlockStop( ZH_COMP_DECL );                       /* end of fake codeblock */
extern void zh_compCodeBlockRewind( ZH_COMP_DECL );                     /* restart of fake codeblock */

#endif    /* ZH_MACRO_SUPPORT */


extern ZH_SIZE zh_compExprListEval( ZH_COMP_DECL, PZH_EXPR pExpr, PZH_COMP_CARGO_FUNC pEval );
extern ZH_SIZE zh_compExprListEval2( ZH_COMP_DECL, PZH_EXPR pExpr1, PZH_EXPR pExpr2, PZH_COMP_CARGO2_FUNC pEval );

extern void zh_compChkCommandLine( ZH_COMP_DECL, int argc, const char * const argv[] );
extern void zh_compChkEnvironment( ZH_COMP_DECL );
extern void zh_compChkAddIncPaths( ZH_COMP_DECL );
extern void zh_compChkSetDefines( ZH_COMP_DECL );

extern void zh_compPrintUsage( ZH_COMP_DECL, const char * szSelf );
extern void zh_compPrintCredits( ZH_COMP_DECL );
extern void zh_compPrintLogo( ZH_COMP_DECL );
extern void zh_compPrintModes( ZH_COMP_DECL );

/* Misc functions defined in ziher.c */
extern void zh_compNOOPfill( PZH_ZFUNC pFunc, ZH_SIZE nFrom, ZH_I_SIZE nCount, ZH_BOOL fPop, ZH_BOOL fCheck );
extern ZH_BOOL zh_compHasJump( PZH_ZFUNC pFunc, ZH_SIZE nPos );

/* Misc functions defined in zhfix.c */
extern void zh_compFixFuncPCode( ZH_COMP_DECL, PZH_ZFUNC pFunc );
/* Misc functions defined in zhdead.c */
extern void zh_compCodeTraceMarkDead( ZH_COMP_DECL, PZH_ZFUNC pFunc );
/* Misc functions defined in zhopt.c */
extern void zh_compOptimizePCode( ZH_COMP_DECL, PZH_ZFUNC pFunc );
extern void zh_compPCodeTraceOptimizer( ZH_COMP_DECL );
/* Misc functions defined in zhstripl.c */
extern void zh_compStripFuncLines( ZH_COMP_DECL, PZH_ZFUNC pFunc );

/* output related functions defined in gen*.c */
extern void zh_compGenCCode( ZH_COMP_DECL, PZH_FNAME );      /* generates the C language output */
extern void zh_compGenPortObj( ZH_COMP_DECL, PZH_FNAME );    /* generates the portable objects */

extern void zh_compGenBufPortObj( ZH_COMP_DECL, ZH_BYTE ** pBufPtr, ZH_SIZE * pnSize ); /* generates the portable objects to memory buffer */

extern void zh_compGenCRealCode( ZH_COMP_DECL, PZH_ZFUNC pFunc, FILE * yyc );
extern void zh_compGenCString( FILE * yyc, const ZH_BYTE * pText, ZH_SIZE nLen );

/* zhident.c */
extern const char * zh_compIdentifierNew( ZH_COMP_DECL, const char * szName, int iType ); /* create the reusable identifier */
extern void zh_compIdentifierOpen( ZH_COMP_DECL ); /* prepare the table of identifiers */
extern void zh_compIdentifierClose( ZH_COMP_DECL ); /* release the table of identifiers */

/* compi18n.c */
extern void zh_compI18nFree( ZH_COMP_DECL );
extern ZH_BOOL zh_compI18nSave( ZH_COMP_DECL, ZH_BOOL fFinal );
extern void zh_compI18nAdd( ZH_COMP_DECL, const char * szText, const char * szContext, const char * szModule, ZH_UINT uiLine );
extern void zh_compI18nAddPlural( ZH_COMP_DECL, const char ** szTexts, ZH_ULONG ulCount, const char * szContext, const char * szModule, ZH_UINT uiLine );

/* global readonly variables used by compiler
 */

extern const char * const zh_comp_szErrors[];
extern const char * const zh_comp_szWarnings[];

/* table with PCODEs' length */
extern const ZH_BYTE zh_comp_pcode_len[];

/* identifier types for zh_compIdentifierNew() */
#define ZH_IDENT_STATIC       0
#define ZH_IDENT_FREE         1
#define ZH_IDENT_COPY         2

/* /GC command-line setting types */
#define ZH_COMPGENC_COMPACT     0
#define ZH_COMPGENC_NORMAL      1
#define ZH_COMPGENC_VERBOSE     2
#define ZH_COMPGENC_REALCODE    3

/* /ES command-line setting types */
#define ZH_EXITLEVEL_DEFAULT    0
#define ZH_EXITLEVEL_SETEXIT    1
#define ZH_EXITLEVEL_DELTARGET  2

/* /kx command-line setting types - compatibility modes
 * (turn on a bit in ZH_ULONG word)
*/
#define ZH_COMPFLAG_ZIHER        ZH_SM_ZIHER     /* 1 -kh */
#define ZH_COMPFLAG_XBASE        ZH_SM_XBASE     /* 2 -kh */
#define ZH_COMPFLAG_SHORTCUTS    ZH_SM_SHORTCUTS   /* 8 -z enable shortcuts for logical operators */
#define ZH_COMPFLAG_ARRSTR       ZH_SM_ARRSTR      /* 16 -ks strings as array of bytes */
#define ZH_COMPFLAG_EXTOPT       ZH_SM_EXTOPT      /* 32 -ko Cl*pper incompatible optimizations */
#define ZH_COMPFLAG_RT_MACRO     ZH_SM_RT_MACRO    /* 64 -kr */
#define ZH_COMPFLAG_OPTJUMP      0x0100            /* -kj turn off jump optimization */
#define ZH_COMPFLAG_ZH_INLINE    0x0200            /* -ki zh_inLine(...) { ... } support */
#define ZH_COMPFLAG_MACROTEXT    0x0400            /* -kM turn off macrotext substitution */
#define ZH_COMPFLAG_USERCP       0x0800            /* -ku strings in user encoding */
#define ZH_COMPFLAG_MACRODECL    0x1000            /* -kd accept macros with declared symbols */

#define ZH_COMP_ISSUPPORTED(flag)   ( ZH_COMP_PARAM->supported & (flag) )

#define ZH_SUPPORT_ZIHER            ( ZH_COMP_ISSUPPORTED(ZH_COMPFLAG_ZIHER) )
#define ZH_SUPPORT_XBASE            ( ZH_COMP_ISSUPPORTED(ZH_COMPFLAG_XBASE) )
#define ZH_SUPPORT_ARRSTR           ( ZH_COMP_ISSUPPORTED(ZH_COMPFLAG_ARRSTR) )
#define ZH_SUPPORT_EXTOPT           ( ZH_COMP_ISSUPPORTED(ZH_COMPFLAG_EXTOPT) )
#define ZH_SUPPORT_MACROTEXT        ( ZH_COMP_ISSUPPORTED(ZH_COMPFLAG_MACROTEXT) )
#define ZH_SUPPORT_USERCP           ( ZH_COMP_ISSUPPORTED(ZH_COMPFLAG_USERCP) )
#define ZH_SUPPORT_MACRODECL        ( ZH_COMP_ISSUPPORTED(ZH_COMPFLAG_MACRODECL) )

#if defined( ZH_MACRO_SUPPORT )
#  define ZH_MACRO_GENFLAGS   ZH_COMPFLAG_RT_MACRO
#elif ! defined( ZH_COMMON_SUPPORT )
#  define ZH_MACRO_GENFLAGS   ( ( ( ( ZH_BYTE ) ZH_COMP_PARAM->supported ) & \
                                  ( ZH_COMPFLAG_ZIHER | \
                                    ZH_COMPFLAG_SHORTCUTS | \
                                    ZH_COMPFLAG_ARRSTR | \
                                    ZH_COMPFLAG_EXTOPT | \
                                    ZH_COMPFLAG_RT_MACRO ) ) | \
                                ( ( ZH_COMP_PARAM->supported & \
                                    ZH_COMPFLAG_ZIHER ) == 0 ? \
                                  ZH_COMPFLAG_SHORTCUTS : 0 ) )
#endif

ZH_EXTERN_END

#endif /* ZH_COMP_H_ */
