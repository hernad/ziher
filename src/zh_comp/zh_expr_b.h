/*
 * Compiler Expression Optimizer
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

#include "zh_comp.h"


/* Forward declarations
 */

/* forward declaration of callback functions
 */
static ZH_EXPR_FUNC( zh_compExprUseDummy );
static ZH_EXPR_FUNC( zh_compExprUseNil );
static ZH_EXPR_FUNC( zh_compExprUseNumeric );
static ZH_EXPR_FUNC( zh_compExprUseDate );
static ZH_EXPR_FUNC( zh_compExprUseTimeStamp );
static ZH_EXPR_FUNC( zh_compExprUseString );
static ZH_EXPR_FUNC( zh_compExprUseCodeblock );
static ZH_EXPR_FUNC( zh_compExprUseLogical );
static ZH_EXPR_FUNC( zh_compExprUseSelf );
static ZH_EXPR_FUNC( zh_compExprUseArray );
static ZH_EXPR_FUNC( zh_compExprUseHash );
static ZH_EXPR_FUNC( zh_compExprUseFunRef );
static ZH_EXPR_FUNC( zh_compExprUseVarRef );
static ZH_EXPR_FUNC( zh_compExprUseRef );
static ZH_EXPR_FUNC( zh_compExprUseIIF );
static ZH_EXPR_FUNC( zh_compExprUseList );
static ZH_EXPR_FUNC( zh_compExprUseArgList );
static ZH_EXPR_FUNC( zh_compExprUseMacroArgList );
static ZH_EXPR_FUNC( zh_compExprUseArrayAt );
static ZH_EXPR_FUNC( zh_compExprUseMacro );
static ZH_EXPR_FUNC( zh_compExprUseFunCall );
static ZH_EXPR_FUNC( zh_compExprUseAliasVar );
static ZH_EXPR_FUNC( zh_compExprUseAliasExpr );
static ZH_EXPR_FUNC( zh_compExprUseSetGet );
static ZH_EXPR_FUNC( zh_compExprUseSend );
static ZH_EXPR_FUNC( zh_compExprUseFunName );
static ZH_EXPR_FUNC( zh_compExprUseAlias );
static ZH_EXPR_FUNC( zh_compExprUseRTVariable );
static ZH_EXPR_FUNC( zh_compExprUseVariable );
static ZH_EXPR_FUNC( zh_compExprUseAssign );
static ZH_EXPR_FUNC( zh_compExprUseEqual );
static ZH_EXPR_FUNC( zh_compExprUsePlus );
static ZH_EXPR_FUNC( zh_compExprUseMinus );
static ZH_EXPR_FUNC( zh_compExprUseMult );
static ZH_EXPR_FUNC( zh_compExprUseDiv );
static ZH_EXPR_FUNC( zh_compExprUseMod );
static ZH_EXPR_FUNC( zh_compExprUsePower );
static ZH_EXPR_FUNC( zh_compExprUsePostInc );
static ZH_EXPR_FUNC( zh_compExprUsePostDec );
static ZH_EXPR_FUNC( zh_compExprUsePreInc );
static ZH_EXPR_FUNC( zh_compExprUsePreDec );
static ZH_EXPR_FUNC( zh_compExprUsePlusEq );
static ZH_EXPR_FUNC( zh_compExprUseMinusEq );
static ZH_EXPR_FUNC( zh_compExprUseMultEq );
static ZH_EXPR_FUNC( zh_compExprUseDivEq );
static ZH_EXPR_FUNC( zh_compExprUseModEq );
static ZH_EXPR_FUNC( zh_compExprUseExpEq );
static ZH_EXPR_FUNC( zh_compExprUseAnd );
static ZH_EXPR_FUNC( zh_compExprUseOr );
static ZH_EXPR_FUNC( zh_compExprUseNot );
static ZH_EXPR_FUNC( zh_compExprUseEQ );
static ZH_EXPR_FUNC( zh_compExprUseLT );
static ZH_EXPR_FUNC( zh_compExprUseGT );
static ZH_EXPR_FUNC( zh_compExprUseLE );
static ZH_EXPR_FUNC( zh_compExprUseGE );
static ZH_EXPR_FUNC( zh_compExprUseNE );
static ZH_EXPR_FUNC( zh_compExprUseIN );
static ZH_EXPR_FUNC( zh_compExprUseNegate );

/* other helper functions
 */
#if defined( ZH_MACRO_SUPPORT )
   static void zh_compExprCodeblockPush( PZH_EXPR, ZH_COMP_DECL );
#else
   static ZH_BOOL zh_compExprCodeblockPush( PZH_EXPR, int, ZH_COMP_DECL );
   static void zh_compExprCodeblockEarly( PZH_EXPR, ZH_COMP_DECL );
   static void zh_compExprCodeblockExtPush( PZH_EXPR pSelf, ZH_COMP_DECL );
#endif

static void zh_compExprPushSendPop( PZH_EXPR pSelf, ZH_COMP_DECL );
static void zh_compExprPushSendPush( PZH_EXPR pSelf, ZH_COMP_DECL );
static void zh_compExprPushOperEq( PZH_EXPR pSelf, ZH_BYTE bOpEq, ZH_COMP_DECL );
static void zh_compExprUseOperEq( PZH_EXPR pSelf, ZH_BYTE bOpEq, ZH_COMP_DECL );
static void zh_compExprPushPreOp( PZH_EXPR pSelf, ZH_BYTE bOper, ZH_COMP_DECL );
static void zh_compExprPushPostOp( PZH_EXPR pSelf, ZH_BYTE bOper, ZH_COMP_DECL );
static void zh_compExprUsePreOp( PZH_EXPR pSelf, ZH_BYTE bOper, ZH_COMP_DECL );
static void zh_compExprUseAliasMacro( PZH_EXPR pAliasedVar, ZH_BYTE bAction, ZH_COMP_DECL );
static PZH_EXPR zh_compExprReduceList( PZH_EXPR pExpr, ZH_COMP_DECL );
static PZH_EXPR zh_compExprReduceAliasString( PZH_EXPR pExpr, PZH_EXPR pAlias, ZH_COMP_DECL );
static ZH_BOOL zh_compExprIsMemvarAlias( const char * szAlias );


const PZH_EXPR_FUNC zh_comp_ExprTable[ ZH_EXPR_COUNT ] = {
   zh_compExprUseDummy,
   zh_compExprUseNil,
   zh_compExprUseNumeric,
   zh_compExprUseDate,
   zh_compExprUseTimeStamp,
   zh_compExprUseString,
   zh_compExprUseCodeblock,
   zh_compExprUseLogical,
   zh_compExprUseSelf,
   zh_compExprUseArray,
   zh_compExprUseHash,
   zh_compExprUseFunRef,
   zh_compExprUseVarRef,
   zh_compExprUseRef,
   zh_compExprUseIIF,
   zh_compExprUseList,
   zh_compExprUseArgList,
   zh_compExprUseMacroArgList,
   zh_compExprUseArrayAt,
   zh_compExprUseMacro,
   zh_compExprUseFunCall,
   zh_compExprUseAliasVar,
   zh_compExprUseAliasExpr,
   zh_compExprUseSetGet,
   zh_compExprUseSend,
   zh_compExprUseFunName,
   zh_compExprUseAlias,
   zh_compExprUseRTVariable,
   zh_compExprUseVariable,
   zh_compExprUsePostInc,     /* post-operators -> lowest precedence */
   zh_compExprUsePostDec,
   zh_compExprUseAssign,      /* assignments */
   zh_compExprUsePlusEq,
   zh_compExprUseMinusEq,
   zh_compExprUseMultEq,
   zh_compExprUseDivEq,
   zh_compExprUseModEq,
   zh_compExprUseExpEq,
   zh_compExprUseOr,          /* logical operators */
   zh_compExprUseAnd,
   zh_compExprUseNot,
   zh_compExprUseEqual,       /* relational operators */
   zh_compExprUseEQ,
   zh_compExprUseNE,
   zh_compExprUseIN,
   zh_compExprUseLT,
   zh_compExprUseGT,
   zh_compExprUseLE,
   zh_compExprUseGE,
   zh_compExprUsePlus,        /* addition */
   zh_compExprUseMinus,
   zh_compExprUseMult,        /* multiple */
   zh_compExprUseDiv,
   zh_compExprUseMod,
   zh_compExprUsePower,
   zh_compExprUseNegate,    /* sign operator */
   zh_compExprUsePreInc,
   zh_compExprUsePreDec     /* highest precedence */
};

/* ************************************************************************* */

static ZH_EXPR_FUNC( zh_compExprUseDummy )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         break;
      case ZH_EA_ARRAY_AT:
         ZH_COMP_ERROR_TYPE( pSelf );
      case ZH_EA_ARRAY_INDEX:
         break;
      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;
      case ZH_EA_PUSH_PCODE:
         ZH_GEN_FUNC1( PCode1, ZH_P_PUSHNIL );
         break;
      case ZH_EA_POP_PCODE:
      case ZH_EA_PUSH_POP:
      case ZH_EA_STATEMENT:
      case ZH_EA_DELETE:
         break;
   }
   return pSelf;
}

/* actions for ZH_ET_NIL expression
 */
static ZH_EXPR_FUNC( zh_compExprUseNil )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         break;
      case ZH_EA_ARRAY_AT:
         ZH_COMP_ERROR_TYPE( pSelf );
         break;
      case ZH_EA_ARRAY_INDEX:
         zh_compErrorIndex( ZH_COMP_PARAM, pSelf );     /* NIL cannot be used as index element */
         break;
      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;
      case ZH_EA_PUSH_PCODE:
         ZH_GEN_FUNC1( PCode1, ZH_P_PUSHNIL );
         break;
      case ZH_EA_POP_PCODE:
         break;
      case ZH_EA_PUSH_POP:
      case ZH_EA_STATEMENT:
         zh_compWarnMeaningless( ZH_COMP_PARAM, pSelf );
      case ZH_EA_DELETE:
         break;
   }

   return pSelf;
}

/* actions for ZH_ET_NUMERIC expression
 */
static ZH_EXPR_FUNC( zh_compExprUseNumeric )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         break;
      case ZH_EA_ARRAY_AT:
         ZH_COMP_ERROR_TYPE( pSelf );
         break;
      case ZH_EA_ARRAY_INDEX:
         break;
      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;
      case ZH_EA_PUSH_PCODE:
         if( pSelf->value.asNum.NumType == ZH_ET_DOUBLE )
            ZH_GEN_FUNC3( PushDouble, pSelf->value.asNum.val.d, pSelf->value.asNum.bWidth, pSelf->value.asNum.bDec );
         else
            ZH_GEN_FUNC1( PushLong, pSelf->value.asNum.val.l );
         break;
      case ZH_EA_POP_PCODE:
         break;
      case ZH_EA_PUSH_POP:
      case ZH_EA_STATEMENT:
         zh_compWarnMeaningless( ZH_COMP_PARAM, pSelf );
      case ZH_EA_DELETE:
         break;
   }
   return pSelf;
}

/* actions for ZH_ET_DATE expression
 */
static ZH_EXPR_FUNC( zh_compExprUseDate )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         break;
      case ZH_EA_ARRAY_AT:
         ZH_COMP_ERROR_TYPE( pSelf );
         break;
      case ZH_EA_ARRAY_INDEX:
         break;
      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;
      case ZH_EA_PUSH_PCODE:
         ZH_GEN_FUNC1( PushDate, pSelf->value.asDate.lDate );
         break;
      case ZH_EA_POP_PCODE:
         break;
      case ZH_EA_PUSH_POP:
      case ZH_EA_STATEMENT:
         zh_compWarnMeaningless( ZH_COMP_PARAM, pSelf );
      case ZH_EA_DELETE:
         break;
   }

   return pSelf;
}

/* actions for ZH_ET_TIMESTAMP expression
 */
static ZH_EXPR_FUNC( zh_compExprUseTimeStamp )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         break;
      case ZH_EA_ARRAY_AT:
         ZH_COMP_ERROR_TYPE( pSelf );
         break;
      case ZH_EA_ARRAY_INDEX:
         break;
      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;
      case ZH_EA_PUSH_PCODE:
         ZH_GEN_FUNC2( PushTimeStamp, pSelf->value.asDate.lDate,
                                      pSelf->value.asDate.lTime );
         break;
      case ZH_EA_POP_PCODE:
         break;
      case ZH_EA_PUSH_POP:
      case ZH_EA_STATEMENT:
         zh_compWarnMeaningless( ZH_COMP_PARAM, pSelf );
      case ZH_EA_DELETE:
         break;
   }

   return pSelf;
}

/* actions for ZH_ET_STRING expression
 */
static ZH_EXPR_FUNC( zh_compExprUseString )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         break;
      case ZH_EA_ARRAY_AT:
         ZH_COMP_ERROR_TYPE( pSelf );
         break;
      case ZH_EA_ARRAY_INDEX:
         break;
      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;
      case ZH_EA_PUSH_PCODE:
#if ! defined( ZH_MACRO_SUPPORT )
         if( ! ZH_SUPPORT_MACROTEXT )
            ZH_GEN_FUNC2( PushString, pSelf->value.asString.string,
                          pSelf->nLength + 1 );
         else
            zh_compPushMacroText( ZH_COMP_PARAM,
                                  pSelf->value.asString.string,
                                  pSelf->nLength, ZH_FALSE );
#else
         ZH_GEN_FUNC2( PushString, pSelf->value.asString.string,
                       pSelf->nLength + 1 );
         if( zh_macroIsValidMacroText( pSelf->value.asString.string,
                                       pSelf->nLength ) )
         {
            ZH_GEN_FUNC1( PCode1, ZH_P_MACROTEXT );
         }
#endif
         break;
      case ZH_EA_POP_PCODE:
         break;

      case ZH_EA_PUSH_POP:
      case ZH_EA_STATEMENT:
         zh_compWarnMeaningless( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_DELETE:
         if( pSelf->value.asString.dealloc )
            zh_xfree( pSelf->value.asString.string );
         break;
   }
   return pSelf;
}

/* actions for ZH_ET_CODEBLOCK expression
 */
static ZH_EXPR_FUNC( zh_compExprUseCodeblock )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
      {
         PZH_EXPR pExpr = pSelf->value.asCodeblock.pExprList;

         if( pExpr && pExpr->pNext == NULL && pExpr->ExprType == ZH_ET_FUNCALL &&
             pExpr->value.asFunCall.pFunName->ExprType == ZH_ET_FUNNAME &&
             pExpr->value.asFunCall.pFunName->value.asSymbol.funcid == ZH_F_BREAK &&
             pSelf->value.asCodeblock.pLocals != NULL )
         {
            PZH_EXPR pParms = pExpr->value.asFunCall.pParms;
            if( zh_compExprParamListLen( pParms ) == 1 &&
                pParms->value.asList.pExprList->ExprType == ZH_ET_VARIABLE &&
                strcmp( pSelf->value.asCodeblock.pLocals->szName,
                        pParms->value.asList.pExprList->value.asSymbol.name ) == 0 )
            {
               ZH_COMP_EXPR_FREE( pSelf );
               pSelf = ZH_COMP_EXPR_NEW( ZH_ET_FUNCALL );
               pSelf->value.asFunCall.pParms = NULL;
               pSelf->value.asFunCall.pFunName = zh_compExprNewFunName( "__BREAKBLOCK", ZH_COMP_PARAM );
               break;
            }
         }
         pSelf->value.asCodeblock.flags |= ZH_BLOCK_REDUCE;
         break;
      }
      case ZH_EA_ARRAY_AT:
         ZH_COMP_ERROR_TYPE( pSelf );
         break;
      case ZH_EA_ARRAY_INDEX:
         zh_compErrorIndex( ZH_COMP_PARAM, pSelf );     /* codeblock cannot be used as index element */
         break;
      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;
      case ZH_EA_PUSH_PCODE:
#if defined( ZH_MACRO_SUPPORT )
         zh_compExprCodeblockPush( pSelf, ZH_COMP_PARAM );
#else
         if( pSelf->value.asCodeblock.flags & ZH_BLOCK_EXT )
            zh_compExprCodeblockExtPush( pSelf, ZH_COMP_PARAM );
         else if( ( pSelf->value.asCodeblock.flags & ZH_BLOCK_MACROVAR ) &&
                  ! ( pSelf->value.asCodeblock.flags & ZH_BLOCK_VPARAMS ) )
            /* early evaluation of a macro */
            zh_compExprCodeblockEarly( pSelf, ZH_COMP_PARAM );
         else
            zh_compExprCodeblockPush( pSelf, 0, ZH_COMP_PARAM );
#endif
         break;
      case ZH_EA_POP_PCODE:
         break;
      case ZH_EA_PUSH_POP:
      case ZH_EA_STATEMENT:
         zh_compWarnMeaningless( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_DELETE:
      {
         PZH_EXPR pExpr = pSelf->value.asCodeblock.pExprList;

         zh_compExprCBVarDel( pSelf->value.asCodeblock.pLocals );

         if( pSelf->value.asCodeblock.string )
            zh_xfree( pSelf->value.asCodeblock.string );

         /* Delete all expressions of the block. */
         while( pExpr )
         {
            PZH_EXPR pNext = pExpr->pNext;
            ZH_COMP_EXPR_FREE( pExpr );
            pExpr = pNext;
         }
         break;
      }
   }
   return pSelf;
}

/* actions for ZH_ET_LOGICAL expression
 */
static ZH_EXPR_FUNC( zh_compExprUseLogical )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         break;
      case ZH_EA_ARRAY_AT:
         ZH_COMP_ERROR_TYPE( pSelf );
         break;
      case ZH_EA_ARRAY_INDEX:
         zh_compErrorIndex( ZH_COMP_PARAM, pSelf );     /* logical cannot be used as array index element */
         break;
      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;
      case ZH_EA_PUSH_PCODE:
         ZH_GEN_FUNC1( PushLogical, pSelf->value.asLogical );
         break;
      case ZH_EA_POP_PCODE:
         break;
      case ZH_EA_PUSH_POP:
      case ZH_EA_STATEMENT:
         zh_compWarnMeaningless( ZH_COMP_PARAM, pSelf );
      case ZH_EA_DELETE:
         break;
   }
   return pSelf;
}

/* actions for ZH_ET_SELF expression
 */
static ZH_EXPR_FUNC( zh_compExprUseSelf )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         break;
      case ZH_EA_ARRAY_AT:
         break;
      case ZH_EA_ARRAY_INDEX:
         zh_compErrorIndex( ZH_COMP_PARAM, pSelf );     /* SELF cannot be used as array index element */
         break;
      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;
      case ZH_EA_PUSH_PCODE:
         ZH_GEN_FUNC1( PCode1, ZH_P_PUSHSELF );
         break;
      case ZH_EA_POP_PCODE:
         break;
      case ZH_EA_PUSH_POP:
      case ZH_EA_STATEMENT:
         zh_compWarnMeaningless( ZH_COMP_PARAM, pSelf );
      case ZH_EA_DELETE:
         break;
   }
   return pSelf;
}

/* actions for a literal array { , , , ... }
 */
static ZH_EXPR_FUNC( zh_compExprUseArray )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         pSelf = zh_compExprReduceList( pSelf, ZH_COMP_PARAM );
         break;

      case ZH_EA_ARRAY_AT:
         break;

      case ZH_EA_ARRAY_INDEX:
         zh_compErrorIndex( ZH_COMP_PARAM, pSelf );     /* array cannot be used as index element */
         break;

      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_PUSH_PCODE:
      {
         ZH_USHORT usItems = ( ZH_USHORT ) zh_compExprParamListCheck( ZH_COMP_PARAM, pSelf );

         if( usItems == 0 )
         {
            ZH_GEN_FUNC3( PCode3, ZH_P_ARRAYGEN, 0, 0 );
         }
         else
         {
            ZH_BOOL fArgsList = pSelf->ExprType == ZH_ET_MACROARGLIST;

            if( ! fArgsList )
            {
               /* NOTE: direct type change */
               pSelf->ExprType = ZH_ET_ARGLIST;
            }

            ZH_EXPR_USE( pSelf, ZH_EA_PUSH_PCODE );

            if( ! fArgsList )
            {
               /* NOTE: restore original expression type */
               pSelf->ExprType = ZH_ET_ARRAY;
               ZH_GEN_FUNC3( PCode3, ZH_P_ARRAYGEN,
                             ZH_LOBYTE( usItems ), ZH_HIBYTE( usItems ) );
            }
            else
            {
               ZH_GEN_FUNC3( PCode3, ZH_P_MACRO_ARRAY_GEN,
                             ZH_LOBYTE( usItems ), ZH_HIBYTE( usItems ) );
            }
         }
         break;
      }

      case ZH_EA_POP_PCODE:
         break;

      case ZH_EA_PUSH_POP:
      {
         PZH_EXPR pElem = pSelf->value.asList.pExprList;
         /* Push non-constant values only
          */
         while( pElem )
         {
            ZH_EXPR_USE( pElem, ZH_EA_PUSH_POP );
            pElem = pElem->pNext;
         }
      }
      break;

      case ZH_EA_STATEMENT:
         zh_compWarnMeaningless( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_DELETE:
      {
         PZH_EXPR pElem = pSelf->value.asList.pExprList;
         /* Delete all elements of the array
          */
         while( pElem )
         {
            PZH_EXPR pNext = pElem->pNext;
            ZH_COMP_EXPR_FREE( pElem );
            pElem = pNext;
         }
      }
      break;
   }

   return pSelf;
}

/* actions for ZH_ET_HASH literal hash
 *    { key1=>val1, key2=>val2, ... keyN=>valN }
 */
static ZH_EXPR_FUNC( zh_compExprUseHash )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         pSelf = zh_compExprReduceList( pSelf, ZH_COMP_PARAM );
         break;

      case ZH_EA_ARRAY_AT:
         break;

      case ZH_EA_ARRAY_INDEX:
         zh_compErrorIndex( ZH_COMP_PARAM, pSelf );     /* array cannot be used as index element */
         break;

      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_PUSH_PCODE:
      {
         ZH_USHORT usItems = ( ZH_USHORT ) ( pSelf->nLength >> 1 );
         /* NOTE: direct type change */
         pSelf->ExprType = ZH_ET_ARGLIST;
         ZH_EXPR_USE( pSelf, ZH_EA_PUSH_PCODE );
         /* NOTE: restore original expression type */
         pSelf->ExprType = ZH_ET_HASH;
         ZH_GEN_FUNC3( PCode3, ZH_P_HASHGEN, ZH_LOBYTE( usItems ), ZH_HIBYTE( usItems ) );
         break;
      }

      case ZH_EA_POP_PCODE:
         break;

      case ZH_EA_PUSH_POP:
      {
         PZH_EXPR pElem = pSelf->value.asList.pExprList;
         /* Push non-constant values only */
         while( pElem )
         {
            ZH_EXPR_USE( pElem, ZH_EA_PUSH_POP );
            pElem = pElem->pNext;
         }
      }
      break;

      case ZH_EA_STATEMENT:
         zh_compWarnMeaningless( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_DELETE:
      {
         PZH_EXPR pElem = pSelf->value.asList.pExprList;
         /* Delete all elements of the hash array
          */
         while( pElem )
         {
            PZH_EXPR pNext = pElem->pNext;
            ZH_COMP_EXPR_FREE( pElem );
            pElem = pNext;
         }
      }
      break;
   }

   return pSelf;
}

/* actions for ZH_ET_VARREF expression
 */
static ZH_EXPR_FUNC( zh_compExprUseVarRef )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         break;

      case ZH_EA_ARRAY_AT:
         ZH_COMP_ERROR_TYPE( pSelf );
         break;

      case ZH_EA_ARRAY_INDEX:
         break;

      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_PUSH_PCODE:
         ZH_GEN_FUNC1( PushVarRef, pSelf->value.asSymbol.name );
         break;

      case ZH_EA_POP_PCODE:
         break;

      case ZH_EA_PUSH_POP:
      case ZH_EA_STATEMENT:
         zh_compWarnMeaningless( ZH_COMP_PARAM, pSelf );
      case ZH_EA_DELETE:
         break;
   }
   return pSelf;
}

/* actions for ZH_ET_FUNREF expression
 */
static ZH_EXPR_FUNC( zh_compExprUseFunRef )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         break;

      case ZH_EA_ARRAY_AT:
         ZH_COMP_ERROR_TYPE( pSelf );
         break;

      case ZH_EA_ARRAY_INDEX:
         break;

      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_PUSH_PCODE:
         ZH_GEN_FUNC1( PushFunRef, pSelf->value.asSymbol.name );
         break;

      case ZH_EA_POP_PCODE:
         break;

      case ZH_EA_PUSH_POP:
      case ZH_EA_STATEMENT:
         zh_compWarnMeaningless( ZH_COMP_PARAM, pSelf );
      case ZH_EA_DELETE:
         break;
   }
   return pSelf;
}

/* actions for ZH_ET_REFERENCE expression
 */
static ZH_EXPR_FUNC( zh_compExprUseRef )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         pSelf->value.asReference = ZH_EXPR_USE( pSelf->value.asReference, ZH_EA_REDUCE );
         if( pSelf->value.asReference->ExprType == ZH_ET_IIF )
         {
            PZH_EXPR pCond, pIIF, pFalse;
            pIIF = pSelf->value.asReference;
            pCond = pIIF->value.asList.pExprList;
            pFalse = zh_compExprNewRef( pCond->pNext->pNext, ZH_COMP_PARAM );
            pCond->pNext = zh_compExprNewRef( pCond->pNext, ZH_COMP_PARAM );
            pCond->pNext->pNext = pFalse;
            ZH_COMP_EXPR_CLEAR( pSelf );
            pSelf = pIIF;
         }
         break;
      case ZH_EA_ARRAY_AT:
         ZH_COMP_ERROR_TYPE( pSelf );
         break;
      case ZH_EA_ARRAY_INDEX:
         break;
      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;
      case ZH_EA_PUSH_PCODE:
      {
         PZH_EXPR pExp = pSelf->value.asReference;
         if( pExp->ExprType == ZH_ET_MACRO )
         {
            if( pExp->value.asMacro.SubType & ZH_ET_MACRO_VAR )
            {
               pExp->value.asMacro.SubType |= ZH_ET_MACRO_REFER;
               ZH_EXPR_USE( pExp, ZH_EA_PUSH_PCODE );
               break;
            }
         }
         else if( pExp->ExprType == ZH_ET_ARRAYAT )
         {
            pExp->value.asList.reference = ZH_TRUE;
            ZH_EXPR_USE( pExp, ZH_EA_PUSH_PCODE );
            break;
         }
         else if( pExp->ExprType == ZH_ET_SEND )
         {
            /* PZH_EXPR pSend = pExp->value.asMessage.pObject;
            if( ! pSend || pSend->ExprType == ZH_ET_VARIABLE ) */
            {
               zh_compExprPushSendPop( pExp, ZH_COMP_PARAM );
               ZH_GEN_FUNC1( PCode1, ZH_P_PUSHOVARREF );
               break;
            }
         }
         else if( pExp->ExprType == ZH_ET_VARIABLE )
         {
            /* NOTE: direct type change */
            pExp->ExprType = ZH_ET_VARREF;
            ZH_EXPR_USE( pExp, ZH_EA_PUSH_PCODE );
            /* NOTE: restore original expression type */
            pExp->ExprType = ZH_ET_VARIABLE;
            break;
         }
         else if( pExp->ExprType == ZH_ET_ALIASVAR )
         {
            if( pExp->value.asAlias.pVar->ExprType == ZH_ET_VARIABLE &&
                pExp->value.asAlias.pAlias->ExprType == ZH_ET_ALIAS &&
                zh_compExprIsMemvarAlias( pExp->value.asAlias.pAlias->value.asSymbol.name ) )
            {
               /* @M-> @MEMVAR-> or @MEMVA-> or @MEMV-> */
               ZH_GEN_FUNC1( PushMemvarRef, pExp->value.asAlias.pVar->value.asSymbol.name );
               break;
            }
         }
         else if( pExp->ExprType == ZH_ET_VARREF ||
                  pExp->ExprType == ZH_ET_REFERENCE )
         {
            ZH_EXPR_USE( pExp, ZH_EA_PUSH_PCODE );
            break;
         }

         zh_compErrorRefer( ZH_COMP_PARAM, NULL, zh_compExprDescription( pExp ) );
         break;
      }

      case ZH_EA_POP_PCODE:
         break;
      case ZH_EA_PUSH_POP:
      case ZH_EA_STATEMENT:
         zh_compWarnMeaningless( ZH_COMP_PARAM, pSelf );
         /* fallthrough */
      case ZH_EA_DELETE:
         ZH_COMP_EXPR_FREE( pSelf->value.asReference );
         break;
   }
   return pSelf;
}


/* actions for ZH_ET_IIF expression
 */
static ZH_EXPR_FUNC( zh_compExprUseIIF )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         pSelf = zh_compExprReduceIIF( zh_compExprReduceList( pSelf, ZH_COMP_PARAM ), ZH_COMP_PARAM );
         break;

      case ZH_EA_ARRAY_AT:
      case ZH_EA_ARRAY_INDEX:
         break;

      case ZH_EA_LVALUE:
         if( ZH_SUPPORT_ZIHER )
         {
            PZH_EXPR pExpr = pSelf->value.asList.pExprList->pNext;
            ZH_EXPR_USE( pExpr, ZH_EA_LVALUE );
            ZH_EXPR_USE( pExpr->pNext, ZH_EA_LVALUE );
         }
         else
            zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_PUSH_PCODE:
      {
         /* this is called if all three parts of IIF expression should be generated
          */
         ZH_I_SIZE nPosFalse, nPosEnd;
         PZH_EXPR pExpr = pSelf->value.asList.pExprList;

         ZH_EXPR_USE( pExpr, ZH_EA_PUSH_PCODE );
         nPosFalse = ZH_GEN_FUNC1( JumpFalse, 0 );
         pExpr = pExpr->pNext;

         ZH_EXPR_USE( pExpr, ZH_EA_PUSH_PCODE );
         nPosEnd = ZH_GEN_FUNC1( Jump, 0 );
         pExpr = pExpr->pNext;

         ZH_GEN_FUNC1( JumpHere, nPosFalse );
         ZH_EXPR_USE( pExpr, ZH_EA_PUSH_PCODE );
         ZH_GEN_FUNC1( JumpHere, nPosEnd );
         break;
      }
      case ZH_EA_POP_PCODE:
      {
         /* this is called if all three parts of IIF expression should be generated
          */
         ZH_I_SIZE nPosFalse, nPosEnd;
         PZH_EXPR pExpr = pSelf->value.asList.pExprList;

         ZH_EXPR_USE( pExpr, ZH_EA_PUSH_PCODE );
         nPosFalse = ZH_GEN_FUNC1( JumpFalse, 0 );
         pExpr = pExpr->pNext;

         ZH_EXPR_USE( pExpr, ZH_EA_POP_PCODE );
         nPosEnd = ZH_GEN_FUNC1( Jump, 0 );
         pExpr = pExpr->pNext;

         ZH_GEN_FUNC1( JumpHere, nPosFalse );
         ZH_EXPR_USE( pExpr, ZH_EA_POP_PCODE );
         ZH_GEN_FUNC1( JumpHere, nPosEnd );
         break;
      }

      case ZH_EA_PUSH_POP:
      case ZH_EA_STATEMENT:
      {

         ZH_SIZE nPosFalse, nPosEnd;
         PZH_EXPR pExpr = pSelf->value.asList.pExprList;

         ZH_EXPR_USE( pExpr, ZH_EA_PUSH_PCODE );
         nPosFalse = ZH_GEN_FUNC1( JumpFalse, 0 );
         pExpr = pExpr->pNext;

         /* do not generate warning about meaningless use of expression NIL */
         if( pExpr->ExprType != ZH_ET_NIL )
            ZH_EXPR_USE( pExpr, ZH_EA_PUSH_POP );
         pExpr = pExpr->pNext;

#if defined( ZH_MACRO_SUPPORT )
         if( ZH_PCODE_DATA->nPCodePos == nPosFalse + 3 )
         {
            ZH_PCODE_DATA->pCode[ nPosFalse - 1 ] = ZH_P_JUMPTRUEFAR;
            nPosEnd = nPosFalse;
         }
#else
         if( ZH_COMP_PARAM->functions.pLast->nPCodePos == nPosFalse + 3 )
         {
            ZH_COMP_PARAM->functions.pLast->pCode[ nPosFalse - 1 ] = ZH_P_JUMPTRUEFAR;
            nPosEnd = nPosFalse;
         }
#endif
         else
         {
            nPosEnd = ZH_GEN_FUNC1( Jump, 0 );
            ZH_GEN_FUNC1( JumpHere, nPosFalse );
         }
         /* do not generate warning about meaningless use of expression NIL */
         if( pExpr->ExprType != ZH_ET_NIL )
            ZH_EXPR_USE( pExpr, ZH_EA_PUSH_POP );
         ZH_GEN_FUNC1( JumpHere, nPosEnd );
         break;
      }
      case ZH_EA_DELETE:
         if( pSelf->value.asList.pExprList )
         {
            PZH_EXPR pNext, pExpr = pSelf->value.asList.pExprList;
            while( pExpr )
            {
               pNext = pExpr->pNext;    /* store next expression */
               ZH_COMP_EXPR_FREE( pExpr );
               pExpr = pNext;
            }
            pSelf->value.asList.pExprList = NULL;
         }
         break;
   }
   return pSelf;  /* return self */
}

/* NOTE: In PUSH operation it leaves on the eval stack the last expression only
 */
static ZH_EXPR_FUNC( zh_compExprUseList )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         pSelf = zh_compExprReduceList( pSelf, ZH_COMP_PARAM );

         if( ZH_SUPPORT_XBASE && pSelf->ExprType == ZH_ET_LIST &&
             zh_compExprListLen( pSelf ) == 1 )
         {
            PZH_EXPR pExpr = pSelf->value.asList.pExprList;
            if( pExpr->ExprType == ZH_ET_MACRO &&
                ( pExpr->value.asMacro.SubType & ZH_ET_MACRO_NOPARE ) == 0 )
               pExpr->value.asMacro.SubType |= ZH_ET_MACRO_PARE;
         }
         if( ZH_SUPPORT_ZIHER )
            pSelf = zh_compExprListStrip( pSelf, ZH_COMP_PARAM );

         break;

      case ZH_EA_ARRAY_AT:
      case ZH_EA_ARRAY_INDEX:
         break;

      case ZH_EA_LVALUE:
         if( zh_compExprListLen( pSelf ) == 1 )
         {
            /* For example:
             * ( a ) := 4
             */
            zh_compErrorLValue( ZH_COMP_PARAM, pSelf->value.asList.pExprList );
         }
         else
            zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_PUSH_PCODE:
         {
            PZH_EXPR pExpr = pSelf->value.asList.pExprList;

            if( pExpr->ExprType == ZH_ET_NONE && pExpr->pNext == NULL )
            {
               /* Empty list was used ()
                */
               ZH_COMP_ERROR_SYNTAX( pExpr );
            }
            else
            {
               while( pExpr )
               {
                  if( ZH_SUPPORT_XBASE )
                  {
                     if( pExpr->ExprType == ZH_ET_MACRO &&
                         ( pExpr->value.asMacro.SubType & ZH_ET_MACRO_NOPARE ) == 0 )
                        pExpr->value.asMacro.SubType |= ZH_ET_MACRO_PARE;
                  }

                  if( pExpr->pNext )
                     ZH_EXPR_USE( pExpr, ZH_EA_PUSH_POP );
                  else
                     ZH_EXPR_USE( pExpr, ZH_EA_PUSH_PCODE );   /* the last expression */
                  pExpr = pExpr->pNext;
               }
            }
         }
         break;

      case ZH_EA_POP_PCODE:
         break;

      case ZH_EA_PUSH_POP:
      case ZH_EA_STATEMENT:
         {
            PZH_EXPR pExpr = pSelf->value.asList.pExprList;

            while( pExpr )
            {
               if( ZH_SUPPORT_XBASE )
               {
                  if( pExpr->ExprType == ZH_ET_MACRO &&
                      ( pExpr->value.asMacro.SubType & ZH_ET_MACRO_NOPARE ) == 0 )
                      pExpr->value.asMacro.SubType |= ZH_ET_MACRO_PARE;
               }

               ZH_EXPR_USE( pExpr, ZH_EA_PUSH_POP );
               pExpr = pExpr->pNext;
            }
         }
         break;

      case ZH_EA_DELETE:
         while( pSelf->value.asList.pExprList )
         {
            PZH_EXPR pExpr = pSelf->value.asList.pExprList;
            pSelf->value.asList.pExprList = pExpr->pNext;
            ZH_COMP_EXPR_FREE( pExpr );
         }
         break;
   }
   return pSelf;
}

/* NOTE: In PUSH operation it leaves all expressions on the eval stack
 */
static ZH_EXPR_FUNC( zh_compExprUseArgList )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         pSelf = zh_compExprReduceList( pSelf, ZH_COMP_PARAM );
         break;

      case ZH_EA_ARRAY_AT:
      case ZH_EA_ARRAY_INDEX:
      case ZH_EA_LVALUE:
         break;

      case ZH_EA_PUSH_PCODE:
         if( pSelf->value.asList.reference )
         {
#if defined( ZH_MACRO_SUPPORT )
            if( ! ZH_PCODE_DATA->fVParams )
#else
            if( ! ZH_COMP_PARAM->functions.pLast->fVParams )
#endif
            {
               zh_compErrorVParams( ZH_COMP_PARAM,
                                    ZH_COMP_PARAM->functions.pLast->szName ?
                                    "Function" : "Codeblock" );
            }
            ZH_GEN_FUNC1( PCode1, ZH_P_PUSHVPARAMS );
         }
         else
         {
            PZH_EXPR pExpr = pSelf->value.asList.pExprList;
            while( pExpr )
            {
               ZH_EXPR_USE( pExpr, ZH_EA_PUSH_PCODE );
               pExpr = pExpr->pNext;
            }
         }
         break;

      case ZH_EA_POP_PCODE:
      case ZH_EA_PUSH_POP:
      case ZH_EA_STATEMENT:
         break;

      case ZH_EA_DELETE:
         if( pSelf->value.asList.pExprList )
         {
            PZH_EXPR pNext, pExpr = pSelf->value.asList.pExprList;
            while( pExpr )
            {
               pNext = pExpr->pNext;    /* store next expression */
               ZH_COMP_EXPR_FREE( pExpr );
               pExpr = pNext;
            }
            pSelf->value.asList.pExprList = NULL;
         }
         break;
   }
   return pSelf;
}

/* NOTE: In PUSH operation it leaves all expressions on the eval stack,
 *       the expressions are divided into macro compiled blocks
 */
static ZH_EXPR_FUNC( zh_compExprUseMacroArgList )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         pSelf = zh_compExprReduceList( pSelf, ZH_COMP_PARAM );
         break;

      case ZH_EA_ARRAY_AT:
      case ZH_EA_ARRAY_INDEX:
      case ZH_EA_LVALUE:
         break;

      case ZH_EA_PUSH_PCODE:
         {
            PZH_EXPR pExpr = pSelf->value.asList.pExprList;
            ZH_USHORT usItems = 0;

            while( pExpr )
            {
               if( ( pExpr->ExprType == ZH_ET_MACRO &&
                     ( pExpr->value.asMacro.SubType & ZH_ET_MACRO_LIST ) ) ||
                   ( pExpr->ExprType == ZH_ET_ARGLIST &&
                     pExpr->value.asList.reference ) ||
                   ( pExpr->ExprType == ZH_ET_FUNCALL &&
                     pExpr->value.asFunCall.pFunName->ExprType == ZH_ET_FUNNAME &&
                     pExpr->value.asFunCall.pFunName->value.asSymbol.funcid ==
                     ZH_F_ARRAYTOPARAMS ) )
               {
                  if( usItems )
                  {
                     ZH_GEN_FUNC1( PushLong, usItems );
                     usItems = 0;
                  }
               }
               else
                  ++usItems;
               ZH_EXPR_USE( pExpr, ZH_EA_PUSH_PCODE );
               pExpr = pExpr->pNext;
            }
            if( usItems )
            {
               ZH_GEN_FUNC1( PushLong, usItems );
            }
         }
         break;

      case ZH_EA_POP_PCODE:
      case ZH_EA_PUSH_POP:
      case ZH_EA_STATEMENT:
         break;

      case ZH_EA_DELETE:
         if( pSelf->value.asList.pExprList )
         {
            PZH_EXPR pNext, pExpr = pSelf->value.asList.pExprList;
            while( pExpr )
            {
               pNext = pExpr->pNext;    /* store next expression */
               ZH_COMP_EXPR_FREE( pExpr );
               pExpr = pNext;
            }
            pSelf->value.asList.pExprList = NULL;
         }
         break;
   }
   return pSelf;
}

/* handler for ( ( array[ idx ] )[ idx ] )[ idx ]
 */
static ZH_EXPR_FUNC( zh_compExprUseArrayAt )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
      {
         PZH_EXPR pIdx;

         if( pSelf->value.asList.pExprList->ExprType == ZH_ET_VARIABLE )
         {
#if ! defined( ZH_MACRO_SUPPORT )
            int iScope;
            zh_compVariableFind( ZH_COMP_PARAM, pSelf->value.asList.pExprList->value.asSymbol.name, NULL, &iScope );
            if( iScope == ZH_VS_UNDECLARED )
            {
               zh_compGenWarning( ZH_COMP_PARAM, zh_comp_szWarnings, 'W', ZH_COMP_WARN_MEMVAR_ASSUMED,
                                  pSelf->value.asList.pExprList->value.asSymbol.name, NULL );
#else
            if( zh_macroLocalVarGetPos( pSelf->value.asList.pExprList->value.asSymbol.name, ZH_COMP_PARAM ) == 0 )
            {
#endif
               pSelf->value.asList.pExprList = zh_compExprNewAliasVar(
                              zh_compExprNewAlias( "MEMVAR", ZH_COMP_PARAM ),
                              pSelf->value.asList.pExprList, ZH_COMP_PARAM );
            }
         }
         pSelf->value.asList.pExprList = ZH_EXPR_USE( pSelf->value.asList.pExprList, ZH_EA_REDUCE );
         pSelf->value.asList.pIndex = ZH_EXPR_USE( pSelf->value.asList.pIndex, ZH_EA_REDUCE );
         pIdx = pSelf->value.asList.pIndex;
         if( pIdx->ExprType == ZH_ET_NUMERIC )
         {
            PZH_EXPR pExpr = pSelf->value.asList.pExprList; /* the expression that holds an array */
            ZH_I_SIZE nIndex;

            if( pIdx->value.asNum.NumType == ZH_ET_LONG )
               nIndex = ( ZH_I_SIZE ) pIdx->value.asNum.val.l;
            else
               nIndex = ( ZH_I_SIZE ) pIdx->value.asNum.val.d;

            if( pExpr->ExprType == ZH_ET_ARRAY )   /* is it a literal array */
            {
               ZH_SIZE nSize = zh_compExprParamListCheck( ZH_COMP_PARAM, pExpr );

               if( pExpr->ExprType == ZH_ET_MACROARGLIST )
                  /* restore original expression type */
                  pExpr->ExprType = ZH_ET_ARRAY;
               else if( ! ZH_IS_VALID_INDEX( nIndex, nSize ) )
               {
                  if( ! ZH_SUPPORT_ARRSTR )
                     zh_compErrorBound( ZH_COMP_PARAM, pIdx );
               }
               else
               {
                  pExpr = pExpr->value.asList.pExprList; /* the first element in the array */
                  while( --nIndex && pExpr )
                     pExpr = pExpr->pNext;

                  if( pExpr ) /* found ? */
                  {
                     /* extract a single expression from the array
                      */
                     PZH_EXPR pNew = ZH_COMP_EXPR_NEW( ZH_ET_NONE );
                     memcpy( pNew, pExpr, sizeof( ZH_EXPR ) );
                     /* This will suppress releasing of memory occupied by components of
                      * the expression - we have just copied them into the new expression.
                      * This method is simpler then traversing the list and releasing all
                      * but this chosen one.
                      */
                     pExpr->ExprType = ZH_ET_NONE;
                     /* Here comes the magic */
                     ZH_COMP_EXPR_FREE( pSelf );
                     pSelf = pNew;
                  }
                  else if( ! ZH_SUPPORT_ARRSTR )
                     zh_compErrorBound( ZH_COMP_PARAM, pIdx );
               }
            }
#if 0
            else if( pExpr->ExprType == ZH_ET_STRING && ZH_SUPPORT_ARRSTR )   /* is it a literal string */
            {
               if( ZH_IS_VALID_INDEX( nIndex, pExpr->nLength ) )
               {
                  ZH_UCHAR ucValue = ( ZH_UCHAR ) pExpr->value.asString.string[ nIndex - 1 ];

                  ZH_COMP_EXPR_FREE( pSelf );
                  pSelf = zh_compExprNewLong( ucValue, ZH_COMP_PARAM );
               }
               else
                  zh_compErrorBound( ZH_COMP_PARAM, pIdx );
            }
#endif
            else if( ! ZH_SUPPORT_ARRSTR )
            {
               ZH_EXPR_USE( pExpr, ZH_EA_ARRAY_AT );
            }
         }
         break;
      }

      case ZH_EA_ARRAY_AT:
      case ZH_EA_ARRAY_INDEX:
      case ZH_EA_LVALUE:
         break;

      case ZH_EA_PUSH_PCODE:
      {
         ZH_BOOL fMacroIndex = ZH_FALSE;

         if( pSelf->value.asList.pIndex->ExprType == ZH_ET_MACRO )
         {
            if( ZH_SUPPORT_XBASE )
            {
               if( ( pSelf->value.asList.pIndex->value.asMacro.SubType & ZH_ET_MACRO_NOLIST ) == 0 )
               {
                  pSelf->value.asList.pIndex->value.asMacro.SubType |= ZH_ET_MACRO_LIST;
                  fMacroIndex = ZH_TRUE;
               }
            }
         }
         else if( pSelf->value.asList.pIndex->ExprType == ZH_ET_ARGLIST )
         {
            fMacroIndex = pSelf->value.asList.pIndex->value.asList.reference;
         }
         else if( pSelf->value.asList.pIndex->ExprType == ZH_ET_FUNCALL &&
                  pSelf->value.asList.pIndex->value.asFunCall.pFunName->
                  value.asSymbol.funcid == ZH_F_ARRAYTOPARAMS )
         {
            pSelf->value.asList.pIndex->value.asFunCall.pFunName->
                                        value.asSymbol.flags |= ZH_FN_MULTIARG;
            fMacroIndex = ZH_TRUE;
         }
         if( pSelf->value.asList.reference && ZH_SUPPORT_ARRSTR )
         {
            PZH_EXPR pList = pSelf->value.asList.pExprList;
            if( pList->ExprType == ZH_ET_VARIABLE )
            {
               /* NOTE: direct type change */
               pList->ExprType = ZH_ET_VARREF;
               ZH_EXPR_USE( pList, ZH_EA_PUSH_PCODE );
               /* NOTE: restore original expression type */
               pList->ExprType = ZH_ET_VARIABLE;
            }
            else if( pList->ExprType == ZH_ET_ALIASVAR &&
                     pList->value.asAlias.pVar->ExprType == ZH_ET_VARIABLE &&
                     pList->value.asAlias.pAlias->ExprType == ZH_ET_ALIAS &&
                     zh_compExprIsMemvarAlias( pList->value.asAlias.pAlias->value.asSymbol.name ) )
            {
               ZH_GEN_FUNC1( PushMemvarRef, pList->value.asAlias.pVar->value.asSymbol.name );
            }
            else if( pList->ExprType == ZH_ET_SEND )
            {
               zh_compExprPushSendPop( pList, ZH_COMP_PARAM );
               ZH_GEN_FUNC1( PCode1, ZH_P_PUSHOVARREF );
            }
            else if( pList->ExprType == ZH_ET_ARRAYAT &&
                     ! pList->value.asList.reference )
            {
               pList->value.asList.reference = ZH_TRUE;
               ZH_EXPR_USE( pList, ZH_EA_PUSH_PCODE );
               pList->value.asList.reference = ZH_FALSE;
            }
            else if( pList->ExprType == ZH_ET_MACRO &&
                     pList->value.asMacro.SubType & ZH_ET_MACRO_VAR )
            {
               pList->value.asMacro.SubType |= ZH_ET_MACRO_REFER;
               ZH_EXPR_USE( pList, ZH_EA_PUSH_PCODE );
               pList->value.asMacro.SubType &= ~ZH_ET_MACRO_REFER;
            }
            else
               ZH_EXPR_USE( pList, ZH_EA_PUSH_PCODE );
         }
         else
            ZH_EXPR_USE( pSelf->value.asList.pExprList, ZH_EA_PUSH_PCODE );
         ZH_EXPR_USE( pSelf->value.asList.pIndex, ZH_EA_PUSH_PCODE );
         if( fMacroIndex )
            ZH_GEN_FUNC1( PCode1, ZH_P_MACRO_PUSHINDEX );
         if( pSelf->value.asList.reference )
            ZH_GEN_FUNC1( PCode1, ZH_P_ARRAYPUSHREF );
         else
            ZH_GEN_FUNC1( PCode1, ZH_P_ARRAYPUSH );
         break;
      }

      case ZH_EA_POP_PCODE:
      {
         ZH_BOOL fMacroIndex = ZH_FALSE;

         if( pSelf->value.asList.pIndex->ExprType == ZH_ET_MACRO )
         {
            if( ZH_SUPPORT_XBASE )
            {
               if( ( pSelf->value.asList.pIndex->value.asMacro.SubType & ZH_ET_MACRO_NOLIST ) == 0 )
               {
                  pSelf->value.asList.pIndex->value.asMacro.SubType |= ZH_ET_MACRO_LIST;
                  fMacroIndex = ZH_TRUE;
               }
            }
         }
         else if( pSelf->value.asList.pIndex->ExprType == ZH_ET_ARGLIST )
         {
            fMacroIndex = pSelf->value.asList.pIndex->value.asList.reference;
         }
         else if( pSelf->value.asList.pIndex->ExprType == ZH_ET_FUNCALL &&
                  pSelf->value.asList.pIndex->value.asFunCall.pFunName->
                  value.asSymbol.funcid == ZH_F_ARRAYTOPARAMS )
         {
            pSelf->value.asList.pIndex->value.asFunCall.pFunName->
                                        value.asSymbol.flags |= ZH_FN_MULTIARG;
            fMacroIndex = ZH_TRUE;
         }
         /* to manage strings as bytes arrays, they must be pushed by reference */
         /* arrays also are passed by reference */
         if( ZH_SUPPORT_ARRSTR )
         {
            PZH_EXPR pList = pSelf->value.asList.pExprList;
            if( pList->ExprType == ZH_ET_VARIABLE )
            {
               /* NOTE: direct type change */
               pList->ExprType = ZH_ET_VARREF;
               ZH_EXPR_USE( pList, ZH_EA_PUSH_PCODE );
               /* NOTE: restore original expression type */
               pList->ExprType = ZH_ET_VARIABLE;
            }
            else if( pList->ExprType == ZH_ET_ALIASVAR &&
                     pList->value.asAlias.pVar->ExprType == ZH_ET_VARIABLE &&
                     pList->value.asAlias.pAlias->ExprType == ZH_ET_ALIAS &&
                     zh_compExprIsMemvarAlias( pList->value.asAlias.pAlias->value.asSymbol.name ) )
            {
               ZH_GEN_FUNC1( PushMemvarRef, pList->value.asAlias.pVar->value.asSymbol.name );
            }
            else if( pList->ExprType == ZH_ET_SEND )
            {
               zh_compExprPushSendPop( pList, ZH_COMP_PARAM );
               ZH_GEN_FUNC1( PCode1, ZH_P_PUSHOVARREF );
            }
            else if( pList->ExprType == ZH_ET_ARRAYAT &&
                     ! pList->value.asList.reference )
            {
               pList->value.asList.reference = ZH_TRUE;
               ZH_EXPR_USE( pList, ZH_EA_PUSH_PCODE );
               pList->value.asList.reference = ZH_FALSE;
            }
            else if( pList->ExprType == ZH_ET_MACRO &&
                     pList->value.asMacro.SubType & ZH_ET_MACRO_VAR )
            {
               pList->value.asMacro.SubType |= ZH_ET_MACRO_REFER;
               ZH_EXPR_USE( pList, ZH_EA_PUSH_PCODE );
               pList->value.asMacro.SubType &= ~ZH_ET_MACRO_REFER;
            }
            else
               ZH_EXPR_USE( pList, ZH_EA_PUSH_PCODE );
         }
         else
            ZH_EXPR_USE( pSelf->value.asList.pExprList, ZH_EA_PUSH_PCODE );
         ZH_EXPR_USE( pSelf->value.asList.pIndex, ZH_EA_PUSH_PCODE );
         if( fMacroIndex )
            ZH_GEN_FUNC1( PCode1, ZH_P_MACRO_PUSHINDEX );
         ZH_GEN_FUNC1( PCode1, ZH_P_ARRAYPOP );
         break;
      }

      case ZH_EA_PUSH_POP:
         /* NOTE: This is highly optimized code - this will work even
          * if accessed value isn't an array. It will work also if
          * the index is invalid
          */
         ZH_EXPR_USE( pSelf->value.asList.pExprList, ZH_EA_PUSH_POP );
         ZH_EXPR_USE( pSelf->value.asList.pIndex, ZH_EA_PUSH_POP );
         /* fallthrough */
      case ZH_EA_STATEMENT:
         zh_compWarnMeaningless( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_DELETE:
         ZH_COMP_EXPR_FREE( pSelf->value.asList.pExprList );
         ZH_COMP_EXPR_FREE( pSelf->value.asList.pIndex );
         break;
   }
   return pSelf;
}

static ZH_EXPR_FUNC( zh_compExprUseMacro )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         if( pSelf->value.asMacro.pExprList )
            pSelf->value.asMacro.pExprList = ZH_EXPR_USE( pSelf->value.asMacro.pExprList, ZH_EA_REDUCE );
         break;

      case ZH_EA_ARRAY_AT:
      case ZH_EA_ARRAY_INDEX:
      case ZH_EA_LVALUE:
         break;

      case ZH_EA_PUSH_PCODE:
         if( pSelf->value.asMacro.SubType & ZH_ET_MACRO_ASSIGN )
            ZH_GEN_FUNC2( PushString, "_", 2 );

         if( pSelf->value.asMacro.pExprList )
         {
            /* macro expression: &( expressions_list )
             * NOTE: only the last expression will be macro-compiled
             */
            ZH_EXPR_USE( pSelf->value.asMacro.pExprList, ZH_EA_PUSH_PCODE );
         }
         else
         {
            if( pSelf->value.asMacro.cMacroOp )
            {
               /* simple macro variable expansion: &variable
                * 'szMacro' is a variable name
                */
#if ! defined( ZH_MACRO_SUPPORT )
               zh_compPushMacroVar( ZH_COMP_PARAM, pSelf->value.asMacro.szMacro );
#else
               ZH_GEN_FUNC1( PushVar, pSelf->value.asMacro.szMacro );
#endif
            }
            else
            {
               /* complex macro expression: prefix&var.suffix
                * all components should be placed as a string that will
                * be compiled after text substitution
                */

               /* Check if macrotext variable does not refer to
                * local, static or field.
                */
#if ! defined( ZH_MACRO_SUPPORT )
               zh_compPushMacroText( ZH_COMP_PARAM,
                                     pSelf->value.asMacro.szMacro,
                                     strlen( pSelf->value.asMacro.szMacro ), ZH_TRUE );
#else
               ZH_GEN_FUNC2( PushString, pSelf->value.asMacro.szMacro, strlen( pSelf->value.asMacro.szMacro ) + 1 );
#endif
            }
         }

         if( pSelf->value.asMacro.SubType & ZH_ET_MACRO_ASSIGN )
         {
            ZH_GEN_FUNC1( PCode1, ZH_P_PLUS );
            pSelf->value.asMacro.SubType &= ~ZH_ET_MACRO_ASSIGN;
         }

         /* compile & run - leave a result on the eval stack
          */
         if( pSelf->value.asMacro.SubType & ZH_ET_MACRO_SYMBOL )
            ZH_GEN_FUNC1( PCode1, ZH_P_MACROSYMBOL );

         else if( pSelf->value.asMacro.SubType & ZH_ET_MACRO_REFER )
            ZH_GEN_FUNC1( PCode1, ZH_P_MACRO_PUSHREF );

         else if( pSelf->value.asMacro.SubType & ZH_ET_MACRO_ALIASED )
         {
            /* NOTE: pcode for alias context is generated in
             * zh_compExprUseAliasVar()
             */
         }
         else
         {
            if( ZH_SUPPORT_XBASE )
            {
               if( pSelf->value.asMacro.SubType & ZH_ET_MACRO_LIST )
               {
                  /* { &macro }, funCall( &macro ) or var[ &macro ] */
                  ZH_GEN_FUNC1( PCode1, ZH_P_MACRO_PUSHLIST );
               }
               else if( pSelf->value.asMacro.SubType & ZH_ET_MACRO_PARE )
               {
                  /* var := (somevalue, &macro) - in Xbase++ compatibility mode
                   * Eval( {|| &macro} ) - in all cases
                   */
                  ZH_GEN_FUNC1( PCode1, ZH_P_MACRO_PUSHPARE );
               }
               else
               {
                  /* usual &macro */
                  ZH_GEN_FUNC1( PCode1, ZH_P_MACRO_PUSH );
               }
            }
            else
               /* usual &macro */
               ZH_GEN_FUNC1( PCode1, ZH_P_MACRO_PUSH );

            /* Always add byte to pcode indicating requested macro compiler flag. */
            ZH_GEN_FUNC1( PCode1, ( ZH_BYTE ) ZH_MACRO_GENFLAGS );
         }
         break;

      case ZH_EA_POP_PCODE:
         if( pSelf->value.asMacro.pExprList )
         {
            /* macro expression: &( expressions_list )
             * NOTE: only the last expression will be macro-compiled
             */
            ZH_EXPR_USE( pSelf->value.asMacro.pExprList, ZH_EA_PUSH_PCODE );
         }
         else
         {
            if( pSelf->value.asMacro.cMacroOp )
            {
               /* simple macro variable expansion: &variable
                * 'szMacro' is a variable name
                */
#if ! defined( ZH_MACRO_SUPPORT )
               zh_compPushMacroVar( ZH_COMP_PARAM, pSelf->value.asMacro.szMacro );
#else
               ZH_GEN_FUNC1( PushVar, pSelf->value.asMacro.szMacro );
#endif
            }
            else
            {
               /* complex macro expression: prefix&var.suffix
                * all components should be placed as a string that will
                * be compiled after text substitution
                */

               /* Check if macrotext variable does not refer to
                * local, static or field.
                */
#if ! defined( ZH_MACRO_SUPPORT )
               zh_compPushMacroText( ZH_COMP_PARAM,
                                     pSelf->value.asMacro.szMacro,
                                     strlen( pSelf->value.asMacro.szMacro ), ZH_TRUE );
#else
               ZH_GEN_FUNC2( PushString, pSelf->value.asMacro.szMacro, strlen( pSelf->value.asMacro.szMacro ) + 1 );
#endif
            }
         }
         /* compile & run - macro compiler will generate pcode to pop a value
          * from the eval stack
          */
         if( ( pSelf->value.asMacro.SubType & ZH_ET_MACRO_ALIASED ) == 0 )
         {
            ZH_GEN_FUNC1( PCode1, ZH_P_MACROPOP );

            /* Always add byte to pcode indicating requested macro compiler flag. */
            ZH_GEN_FUNC1( PCode1, ( ZH_BYTE ) ZH_MACRO_GENFLAGS );
         }
         break;

      case ZH_EA_PUSH_POP:
      case ZH_EA_STATEMENT:
         ZH_EXPR_USE( pSelf, ZH_EA_PUSH_PCODE );
         ZH_GEN_FUNC1( PCode1, ZH_P_POP );
         break;

      case ZH_EA_DELETE:
         if( pSelf->value.asMacro.pExprList )
            ZH_COMP_EXPR_FREE( pSelf->value.asMacro.pExprList );
         break;
   }
   return pSelf;
}

static ZH_EXPR_FUNC( zh_compExprUseFunCall )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         /* Reduce the expressions on the list of arguments
          */
         if( pSelf->value.asFunCall.pParms )
            pSelf->value.asFunCall.pParms = ZH_EXPR_USE( pSelf->value.asFunCall.pParms, ZH_EA_REDUCE );

         if( pSelf->value.asFunCall.pFunName->ExprType == ZH_ET_FUNNAME )
         {
            ZH_FUNC_ID funcID = pSelf->value.asFunCall.pFunName->value.asSymbol.funcid;
            PZH_EXPR pParms = pSelf->value.asFunCall.pParms;
            ZH_USHORT usCount = ( ZH_USHORT ) zh_compExprParamListLen( pParms );

#ifndef ZH_MACRO_SUPPORT
            if( zh_compFunCallCheck( ZH_COMP_PARAM, pSelf->value.asFunCall.pFunName->value.asSymbol.name, usCount ) )
#endif
            {
               switch( funcID )
               {
                  case ZH_F_AT:
                     if( usCount == 2 )
                        zh_compExprReduceAT( pSelf, ZH_COMP_PARAM );
                     break;
                  case ZH_F_ASC:
                     if( usCount )
                        zh_compExprReduceASC( pSelf, ZH_COMP_PARAM );
                     break;
                  case ZH_F_CHR:
                     if( usCount )
                        zh_compExprReduceCHR( pSelf, ZH_COMP_PARAM );
                     break;
                  case ZH_F_LEN:
                     if( usCount )
                        zh_compExprReduceLEN( pSelf, ZH_COMP_PARAM );
                     break;
                  case ZH_F_UPPER:
                     if( usCount )
                        zh_compExprReduceUPPER( pSelf, ZH_COMP_PARAM );
                     break;

                  case ZH_F_EMPTY:
                     if( usCount && ZH_SUPPORT_ZIHER )
                        zh_compExprReduceEMPTY( pSelf, ZH_COMP_PARAM );
                     break;
                  case ZH_F_INT:
                     if( usCount == 1 && ZH_SUPPORT_ZIHER )
                        zh_compExprReduceINT( pSelf, ZH_COMP_PARAM );
                     break;
                  case ZH_F_MAX:
                     if( usCount == 2 && ZH_SUPPORT_ZIHER )
                        zh_compExprReduceMAX( pSelf, ZH_COMP_PARAM );
                     break;
                  case ZH_F_MIN:
                     if( usCount == 2 && ZH_SUPPORT_ZIHER )
                        zh_compExprReduceMIN( pSelf, ZH_COMP_PARAM );
                     break;
                  case ZH_F_STOD:
                     if( usCount < 2 && ZH_SUPPORT_ZIHER )
                        zh_compExprReduceSTOD( pSelf, usCount, ZH_COMP_PARAM );
                     break;
                  case ZH_F_STOT:
                     zh_compExprReduceSTOT( pSelf, usCount, ZH_COMP_PARAM );
                     break;
                  case ZH_F_DTOS:
                     if( usCount == 1 )
                        zh_compExprReduceDTOS( pSelf, ZH_COMP_PARAM );
                     break;
                  case ZH_F_CTOD:
                     if( usCount && ZH_SUPPORT_ZIHER )
                        zh_compExprReduceCTOD( pSelf, ZH_COMP_PARAM );
                     break;

                  case ZH_F_BCHAR:
                     if( usCount )
                        zh_compExprReduceBCHAR( pSelf, ZH_COMP_PARAM );
                     break;
                  case ZH_F_BCODE:
                     if( usCount )
                        zh_compExprReduceBCODE( pSelf, ZH_COMP_PARAM );
                     break;

                  case ZH_F_BITAND:
                  case ZH_F_BITOR:
                  case ZH_F_BITXOR:
                  case ZH_F_BITSET:
                  case ZH_F_BITRESET:
                  case ZH_F_BITSHIFT:
                     if( pParms && usCount >= 2 &&
                         pParms->value.asList.pExprList->ExprType == ZH_ET_NUMERIC &&
                         pParms->value.asList.pExprList->pNext->ExprType == ZH_ET_NUMERIC )
                     {
                        PZH_EXPR pArg = pParms->value.asList.pExprList;
                        ZH_MAXINT lResult = zh_compExprAsLongNum( pArg );
                        ZH_BOOL fOptimize = ZH_TRUE;

                        if( funcID == ZH_F_BITAND )
                        {
                           while( --usCount )
                           {
                              pArg = pArg->pNext;
                              if( pArg->ExprType != ZH_ET_NUMERIC )
                              {
                                 fOptimize = ZH_FALSE;
                                 break;
                              }
                              lResult &= zh_compExprAsLongNum( pArg );
                           }
                        }
                        else if( funcID == ZH_F_BITOR )
                        {
                           while( --usCount )
                           {
                              pArg = pArg->pNext;
                              if( pArg->ExprType != ZH_ET_NUMERIC )
                              {
                                 fOptimize = ZH_FALSE;
                                 break;
                              }
                              lResult |= zh_compExprAsLongNum( pArg );
                           }
                        }
                        else if( funcID == ZH_F_BITXOR )
                        {
                           while( --usCount )
                           {
                              pArg = pArg->pNext;
                              if( pArg->ExprType != ZH_ET_NUMERIC )
                              {
                                 fOptimize = ZH_FALSE;
                                 break;
                              }
                              lResult ^= zh_compExprAsLongNum( pArg );
                           }
                        }
                        else if( funcID == ZH_F_BITSET )
                        {
                           ZH_MAXINT lBit = zh_compExprAsLongNum( pArg->pNext );
                           lResult |= ( ZH_MAXINT ) 1 << lBit;
                        }
                        else if( funcID == ZH_F_BITRESET )
                        {
                           ZH_MAXINT lBit = zh_compExprAsLongNum( pArg->pNext );
                           lResult &= ~( ( ZH_MAXINT ) 1 << lBit );
                        }
                        else /* if( funcID == ZH_F_BITSHIFT ) */
                        {
                           ZH_MAXINT lBits = zh_compExprAsLongNum( pArg->pNext );
                           if( lBits < 0 )
                              lResult >>= -lBits;
                           else
                              lResult <<= lBits;
                        }
                        if( fOptimize )
                           zh_compExprReduceBitFunc( pSelf, lResult, ZH_FALSE, ZH_COMP_PARAM );
                     }
                     break;
                  case ZH_F_BITTEST:
                     if( pParms &&
                         usCount >= 2 &&
                         pParms->value.asList.pExprList->ExprType == ZH_ET_NUMERIC &&
                         pParms->value.asList.pExprList->pNext->ExprType == ZH_ET_NUMERIC )
                     {
                        PZH_EXPR pArg = pParms->value.asList.pExprList;
                        ZH_MAXINT lBit = zh_compExprAsLongNum( pArg->pNext );
                        ZH_MAXINT lResult = ( zh_compExprAsLongNum( pArg ) &
                                              ( ( ZH_MAXINT ) 1 << lBit ) ) != 0;
                        zh_compExprReduceBitFunc( pSelf, lResult, ZH_TRUE, ZH_COMP_PARAM );
                     }
                     break;
                  case ZH_F_BITNOT:
                     if( pParms &&
                         usCount &&
                         pParms->value.asList.pExprList->ExprType == ZH_ET_NUMERIC )
                     {
                        ZH_MAXINT lResult = ~zh_compExprAsLongNum( pParms->value.asList.pExprList );
                        zh_compExprReduceBitFunc( pSelf, lResult, ZH_FALSE, ZH_COMP_PARAM );
                     }
                     break;

#ifndef ZH_MACRO_SUPPORT
                  case ZH_F_I18N_GETTEXT:
                  case ZH_F_I18N_GETTEXT_NOOP:
                  case ZH_F_I18N_GETTEXT_STRICT:
                  case ZH_F_I18N_NGETTEXT:
                  case ZH_F_I18N_NGETTEXT_NOOP:
                  case ZH_F_I18N_NGETTEXT_STRICT:
                  {
                     if( pParms )
                     {
                        PZH_EXPR     pCount = NULL, pBadParam = NULL, pArg;
                        int          iWarning = 0;
                        const char * szExpect = NULL;
                        const char * szContext = NULL;
                        ZH_BOOL      fStrict, fNoop, fPlural;

                        pArg = usCount ? pParms->value.asList.pExprList : NULL;

                        fStrict = funcID == ZH_F_I18N_GETTEXT_STRICT ||
                                  funcID == ZH_F_I18N_NGETTEXT_STRICT;
                        fNoop   = funcID == ZH_F_I18N_GETTEXT_NOOP ||
                                  funcID == ZH_F_I18N_NGETTEXT_NOOP;
                        fPlural = funcID == ZH_F_I18N_NGETTEXT ||
                                  funcID == ZH_F_I18N_NGETTEXT_NOOP ||
                                  funcID == ZH_F_I18N_NGETTEXT_STRICT;

                        if( fPlural && usCount )
                        {
                           pCount = pArg;
                           pArg = pArg->pNext;
                           --usCount;
                           if( pCount->ExprType <= ZH_ET_FUNREF &&
                               pCount->ExprType != ZH_ET_NUMERIC )
                           {
                              iWarning = ZH_COMP_WARN_PARAM_TYPE;
                              pBadParam = pCount;
                              szExpect = "Numeric expression";
                           }
                        }
                        if( usCount == 2 )
                        {
                           if( pArg->pNext->ExprType == ZH_ET_STRING && pArg->pNext->nLength > 0 )
                           {
                              szContext = pArg->pNext->value.asString.string;
                              --usCount;
                           }
                           else
                           {
                              iWarning = ZH_COMP_WARN_PARAM_TYPE;
                              pBadParam = pArg->pNext;
                              szExpect = "String";
                           }
                        }
                        if( iWarning == 0 )
                        {
                           if( usCount == 1 )
                           {
                              const char * szPlurals[ ZH_I18N_PLURAL_MAX ];

                              if( pArg->ExprType == ZH_ET_STRING )
                              {
                                 if( ZH_COMP_PARAM->fI18n && pArg->nLength > 0 )
                                 {
                                    if( pCount )
                                    {
                                       szPlurals[ 0 ] = pArg->value.asString.string;
                                       zh_compI18nAddPlural( ZH_COMP_PARAM, szPlurals, 1, szContext,
                                                             ZH_COMP_PARAM->currModule, ZH_COMP_PARAM->currLine );
                                    }
                                    else
                                       zh_compI18nAdd( ZH_COMP_PARAM, pArg->value.asString.string, szContext,
                                                       ZH_COMP_PARAM->currModule, ZH_COMP_PARAM->currLine );
                                 }
                              }
                              else if( pCount && pArg->ExprType == ZH_ET_ARRAY &&
                                       zh_compExprListTypeCheck( pArg, ZH_ET_STRING ) )
                              {
                                 if( ZH_COMP_PARAM->fI18n )
                                 {
                                    ZH_ULONG ulLen = zh_compExprListLen( pArg ), ul;
                                    PZH_EXPR pArgExp = pArg->value.asList.pExprList;

                                    if( ulLen > ZH_I18N_PLURAL_MAX )
                                       ulLen = ZH_I18N_PLURAL_MAX;
                                    for( ul = 0; ul < ulLen; ++ul )
                                    {
                                       szPlurals[ ul ] = pArgExp->value.asString.string;
                                       pArgExp = pArgExp->pNext;
                                    }
                                    zh_compI18nAddPlural( ZH_COMP_PARAM, szPlurals, ulLen, szContext,
                                                          ZH_COMP_PARAM->currModule, ZH_COMP_PARAM->currLine );
                                 }
                              }
                              else if( fStrict || fNoop || pArg->ExprType <= ZH_ET_FUNREF )
                              {
                                 iWarning = ZH_COMP_WARN_PARAM_TYPE;
                                 pBadParam = pArg;
                                 szExpect = fPlural ? "String or Array of Strings" : "String";
                              }
                           }
                           else
                              iWarning = ZH_COMP_WARN_PARAM_COUNT;
                        }
                        if( iWarning != 0 )
                        {
                           /* TODO: warning message does not fit very well, because it requires
                            *       type of used parameter. Let's print "unknown", to avoid deeper
                            *       analysis of parameter.
                            */
                           if( iWarning == ZH_COMP_WARN_PARAM_TYPE )
                              zh_compGenWarning( ZH_COMP_PARAM, zh_comp_szWarnings, 'W', ZH_COMP_WARN_PARAM_TYPE,
                                                 pBadParam && pBadParam->ExprType > ZH_ET_NONE &&
                                                 pBadParam->ExprType <= ZH_ET_FUNREF ?
                                                 zh_compExprDescription( pBadParam ) : "Unknown", szExpect );
                           else
                           {
                              char buf[ 16 ];
                              zh_snprintf( buf, sizeof( buf ), "%d", ( int ) usCount + ( pCount ? 1 : 0 ) + ( szContext ? 1 : 0 ) );
                              zh_compGenWarning( ZH_COMP_PARAM, zh_comp_szWarnings, 'W', ZH_COMP_WARN_PARAM_COUNT, buf, fPlural ? "2 or 3" : "1 or 2" );
                           }
                        }
                        /* zh_i18n_gettext_noop() is not a real function. It is used to
                           force writing of string to .pot file. So, we should try to
                           replace function call by first argument regardless fI18n flag
                           and warnings.
                         */
                        else if( fNoop && usCount )
                        {
                           pParms->value.asList.pExprList = pArg->pNext; /* skip first parameter */
                           pArg->pNext = NULL;
                           ZH_COMP_EXPR_FREE( pParms );
                           ZH_COMP_EXPR_FREE( pSelf->value.asFunCall.pFunName );
                           if( pCount )
                           {
                              if( pArg->ExprType == ZH_ET_ARRAY )
                              {
                                 if( zh_compExprListLen( pArg ) == 1 )
                                 {
                                    ZH_COMP_EXPR_FREE( pCount );
                                    pCount = pArg;
                                    pArg = pArg->value.asList.pExprList;
                                    pCount->value.asList.pExprList = NULL;
                                 }
                                 else
                                 {
                                    /* build expression: pArray[ iif( pCount == 1, 1, 2 ) ] */
                                    PZH_EXPR pIndex;

                                    /* create pCount == 1 */
                                    pIndex = zh_compExprSetOperand( zh_compExprNewEQ( pCount, ZH_COMP_PARAM ),
                                                                    zh_compExprNewLong( 1, ZH_COMP_PARAM ), ZH_COMP_PARAM );
                                    /* create: ( pCount == 1, */
                                    pIndex = zh_compExprNewList( pIndex, ZH_COMP_PARAM );
                                    /* create: ( pCount == 1, 1, */
                                    pIndex = zh_compExprAddListExpr( pIndex, zh_compExprNewLong( 1, ZH_COMP_PARAM ) );
                                    /* create: ( pCount == 1, 1, 2 )*/
                                    pIndex = zh_compExprAddListExpr( pIndex, zh_compExprNewLong( 2, ZH_COMP_PARAM ) );
                                    /* create: IIF() expression */
                                    pIndex = zh_compExprNewIIF( pIndex );
                                    /* create: pArray[ iif( pCount == 1, 1, 2 ) ] */
                                    pArg = zh_compExprNewArrayAt( pArg, pIndex, ZH_COMP_PARAM );
                                    /* reduce the final expression */
                                    pArg = ZH_EXPR_USE( pArg, ZH_EA_REDUCE );
                                    pCount = NULL;
                                 }
                              }
                              if( pCount )
                                 ZH_COMP_EXPR_FREE( pCount );
                           }
                           memcpy( pSelf, pArg, sizeof( ZH_EXPR ) );
                           /* free pArg expression body but without freeing its subexpressions */
                           ZH_COMP_EXPR_CLEAR( pArg );
                        }

                        break;
                     }
                  }
#endif
                  default:
                     /* to pacify enum warning */
                     break;
               }
            }
         }
         break;

      case ZH_EA_ARRAY_AT:
      case ZH_EA_ARRAY_INDEX:
         break;

      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_PUSH_PCODE:
      {
         ZH_BOOL fArgsList = ZH_FALSE;
         ZH_USHORT usCount = 0;

         /* NOTE: pParms will be NULL in 'DO procname' (if there is
          * no WITH keyword)
          */
         if( pSelf->value.asFunCall.pParms )
         {
            usCount = ( ZH_USHORT ) zh_compExprParamListCheck( ZH_COMP_PARAM, pSelf->value.asFunCall.pParms );
            fArgsList = pSelf->value.asFunCall.pParms->ExprType == ZH_ET_MACROARGLIST;
         }

         if( pSelf->value.asFunCall.pFunName->ExprType == ZH_ET_FUNNAME )
         {
            if( ! fArgsList )
            {
               if( pSelf->value.asFunCall.pFunName->value.asSymbol.funcid == ZH_F_ARRAYTOPARAMS &&
                   usCount == 1 &&
                   ( pSelf->value.asFunCall.pFunName->value.asSymbol.flags & ZH_FN_MULTIARG ) != 0 )
               {
                  ZH_EXPR_USE( pSelf->value.asFunCall.pParms, ZH_EA_PUSH_PCODE );
                  ZH_GEN_FUNC1( PCode1, ZH_P_PUSHAPARAMS );
                  break;
               }
               else if( pSelf->value.asFunCall.pFunName->value.asSymbol.funcid == ZH_F_ARRAY &&
                        ZH_SUPPORT_EXTOPT )
               {
                  if( usCount )
                     ZH_EXPR_USE( pSelf->value.asFunCall.pParms, ZH_EA_PUSH_PCODE );
                  ZH_GEN_FUNC3( PCode3, ZH_P_ARRAYDIM, ZH_LOBYTE( usCount ), ZH_HIBYTE( usCount ) );
                  break;
               }
            }
            ZH_GEN_FUNC2( PushFunCall, pSelf->value.asFunCall.pFunName->value.asSymbol.name,
                                       pSelf->value.asFunCall.pFunName->value.asSymbol.flags );
         }
         else
         {
            ZH_EXPR_USE( pSelf->value.asFunCall.pFunName, ZH_EA_PUSH_PCODE );
            ZH_GEN_FUNC1( PCode1, ZH_P_PUSHNIL );
         }

         if( usCount )
            ZH_EXPR_USE( pSelf->value.asFunCall.pParms, ZH_EA_PUSH_PCODE );

         if( fArgsList )
         {
            ZH_GEN_FUNC3( PCode3, ZH_P_MACRO_FUNC, ZH_LOBYTE( usCount ), ZH_HIBYTE( usCount ) );
            /* restore original expression type */
            pSelf->value.asFunCall.pParms->ExprType = ZH_ET_ARGLIST;
         }
         else if( usCount > 255 )
            ZH_GEN_FUNC3( PCode3, ZH_P_FUNCTION, ZH_LOBYTE( usCount ), ZH_HIBYTE( usCount ) );
         else
            ZH_GEN_FUNC2( PCode2, ZH_P_FUNCTIONSHORT, ( ZH_BYTE ) usCount );
         break;
      }
      case ZH_EA_POP_PCODE:
         break;

      case ZH_EA_PUSH_POP:
      case ZH_EA_STATEMENT:
      {
         ZH_BOOL fArgsList = ZH_FALSE;
         ZH_USHORT usCount = 0;

         if( pSelf->value.asFunCall.pFunName->ExprType == ZH_ET_FUNNAME )
         {
            ZH_GEN_FUNC2( PushFunCall, pSelf->value.asFunCall.pFunName->value.asSymbol.name,
                                       pSelf->value.asFunCall.pFunName->value.asSymbol.flags );
         }
         else
         {
            ZH_EXPR_USE( pSelf->value.asFunCall.pFunName, ZH_EA_PUSH_PCODE );
            ZH_GEN_FUNC1( PCode1, ZH_P_PUSHNIL );
         }

         if( pSelf->value.asFunCall.pParms )
         {
            usCount = ( ZH_USHORT ) zh_compExprParamListCheck( ZH_COMP_PARAM, pSelf->value.asFunCall.pParms );
            fArgsList = pSelf->value.asFunCall.pParms->ExprType == ZH_ET_MACROARGLIST;
            if( usCount )
               ZH_EXPR_USE( pSelf->value.asFunCall.pParms, ZH_EA_PUSH_PCODE );
         }

         if( fArgsList )
         {
            ZH_GEN_FUNC3( PCode3, ZH_P_MACRODO, ZH_LOBYTE( usCount ), ZH_HIBYTE( usCount ) );
            /* restore original expression type */
            pSelf->value.asFunCall.pParms->ExprType = ZH_ET_ARGLIST;
         }
         else if( usCount > 255 )
            ZH_GEN_FUNC3( PCode3, ZH_P_DO, ZH_LOBYTE( usCount ), ZH_HIBYTE( usCount ) );
         else
            ZH_GEN_FUNC2( PCode2, ZH_P_DOSHORT, ( ZH_BYTE ) usCount );
         break;
      }
      case ZH_EA_DELETE:
         if( pSelf->value.asFunCall.pParms )
            ZH_COMP_EXPR_FREE( pSelf->value.asFunCall.pParms );
         ZH_COMP_EXPR_FREE( pSelf->value.asFunCall.pFunName );
         break;
   }
   return pSelf;
}

/* handler for expression->identifier syntax
 */
static ZH_EXPR_FUNC( zh_compExprUseAliasVar )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprUseAliasVar()" ) );

   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         /* NOTE: direct reduction not used for ZH_ET_LIST to avoid
          *       list stripping before PUSH/POP operations
          */
         if( pSelf->value.asAlias.pAlias->ExprType == ZH_ET_LIST )
         {
            pSelf->value.asAlias.pAlias = zh_compExprReduceList(
                                 pSelf->value.asAlias.pAlias, ZH_COMP_PARAM );
            if( ZH_SUPPORT_EXTOPT &&
                pSelf->value.asAlias.pAlias->value.asList.pExprList->ExprType == ZH_ET_STRING &&
                pSelf->value.asAlias.pAlias->value.asList.pExprList->pNext == NULL )
            {
               pSelf->value.asAlias.pAlias = zh_compExprReduceAliasString(
                           pSelf->value.asAlias.pAlias,
                           pSelf->value.asAlias.pAlias->value.asList.pExprList,
                           ZH_COMP_PARAM );
            }
         }
         else
            pSelf->value.asAlias.pAlias = ZH_EXPR_USE( pSelf->value.asAlias.pAlias, ZH_EA_REDUCE );
         break;

      case ZH_EA_ARRAY_AT:
      case ZH_EA_ARRAY_INDEX:
      case ZH_EA_LVALUE:
         break;

      case ZH_EA_PUSH_PCODE:
      {
         PZH_EXPR pAlias = pSelf->value.asAlias.pAlias;

         if( pAlias->ExprType == ZH_ET_MACRO ||
             pSelf->value.asAlias.pVar->ExprType == ZH_ET_MACRO )
         {
            /* Macro operator is used on the left or right side of an alias
             * operator - handle it with a special care
             */
            zh_compExprUseAliasMacro( pSelf, ZH_EA_PUSH_PCODE, ZH_COMP_PARAM );
         }
         else if( pAlias->ExprType == ZH_ET_ALIAS )
         {
            /*
             * myalias->var
             * FIELD->var
             * MEMVAR->var
             *
             * NOTE: ZH_TRUE = push also alias
             */
            ZH_GEN_FUNC4( PushAliasedVar, pSelf->value.asAlias.pVar->value.asSymbol.name, ZH_TRUE, pAlias->value.asSymbol.name, 0 );
         }
         else if( pAlias->ExprType == ZH_ET_NUMERIC )
         {
            /* numeric alias
             * 2->var
             *
             * NOTE: only integer (long) values are allowed
             */
            if( pAlias->value.asNum.NumType == ZH_ET_LONG )
               ZH_GEN_FUNC4( PushAliasedVar, pSelf->value.asAlias.pVar->value.asSymbol.name, ZH_TRUE, NULL, pAlias->value.asNum.val.l );
            else
               zh_compErrorAlias( ZH_COMP_PARAM, pAlias );
         }
         else if( pAlias->ExprType == ZH_ET_LIST )
         {
            /*
             * ( expression )->var
             *
             * NOTE: ZH_FALSE = don't push alias value
             */
            ZH_EXPR_USE( pAlias, ZH_EA_PUSH_PCODE );
            ZH_GEN_FUNC4( PushAliasedVar, pSelf->value.asAlias.pVar->value.asSymbol.name, ZH_FALSE, NULL, 0 );
         }
         else
            zh_compErrorAlias( ZH_COMP_PARAM, pAlias );
         break;
      }
      case ZH_EA_POP_PCODE:
      {
         PZH_EXPR pAlias = pSelf->value.asAlias.pAlias;

         if( pAlias->ExprType == ZH_ET_MACRO || pSelf->value.asAlias.pVar->ExprType == ZH_ET_MACRO )
         {
            /* Macro operator is used on the left or right side of an alias
             * operator - handle it with a special care
             * (we need convert to a string the whole expression)
             */
            zh_compExprUseAliasMacro( pSelf, ZH_EA_POP_PCODE, ZH_COMP_PARAM );
         }
         else if( pAlias->ExprType == ZH_ET_ALIAS )
         {
            /*
             * myalias->var
             * FIELD->var
             * MEMVAR->var
             */
            ZH_GEN_FUNC4( PopAliasedVar, pSelf->value.asAlias.pVar->value.asSymbol.name, ZH_TRUE, pAlias->value.asSymbol.name, 0 );
         }
         else if( pAlias->ExprType == ZH_ET_NUMERIC )
         {
            /* numeric alias
             * 2->var
             *
             * NOTE: only integer (long) values are allowed
             */
            if( pAlias->value.asNum.NumType == ZH_ET_LONG )
               ZH_GEN_FUNC4( PopAliasedVar, pSelf->value.asAlias.pVar->value.asSymbol.name, ZH_TRUE, NULL, pAlias->value.asNum.val.l );
            else
               zh_compErrorAlias( ZH_COMP_PARAM, pAlias );
         }
         else if( pAlias->ExprType == ZH_ET_LIST )
         {
            /*
             * ( expression )->var
             *
             * NOTE: ZH_FALSE = don't push alias value
             */
            ZH_EXPR_USE( pAlias, ZH_EA_PUSH_PCODE );
            ZH_GEN_FUNC4( PopAliasedVar, pSelf->value.asAlias.pVar->value.asSymbol.name, ZH_FALSE, NULL, 0 );
         }
         else
            zh_compErrorAlias( ZH_COMP_PARAM, pAlias );
         break;
      }
      case ZH_EA_PUSH_POP:
      case ZH_EA_STATEMENT:
         ZH_EXPR_USE( pSelf, ZH_EA_PUSH_PCODE );
         ZH_GEN_FUNC1( PCode1, ZH_P_POP );
         break;

      case ZH_EA_DELETE:
         ZH_COMP_EXPR_FREE( pSelf->value.asAlias.pAlias );
         if( pSelf->value.asAlias.pVar )
            ZH_COMP_EXPR_FREE( pSelf->value.asAlias.pVar );
         break;
   }
   return pSelf;
}

/* handler for expression->( expression, ... ) syntax
 */
static ZH_EXPR_FUNC( zh_compExprUseAliasExpr )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         pSelf->value.asAlias.pAlias   = ZH_EXPR_USE( pSelf->value.asAlias.pAlias, ZH_EA_REDUCE );
         pSelf->value.asAlias.pExpList = ZH_EXPR_USE( pSelf->value.asAlias.pExpList, ZH_EA_REDUCE );
         if( ZH_SUPPORT_EXTOPT && pSelf->value.asAlias.pAlias->ExprType == ZH_ET_STRING )
            pSelf->value.asAlias.pAlias = zh_compExprReduceAliasString(
                                 pSelf->value.asAlias.pAlias,
                                 pSelf->value.asAlias.pAlias, ZH_COMP_PARAM );
         break;

      case ZH_EA_ARRAY_AT:
      case ZH_EA_ARRAY_INDEX:
         break;

      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_PUSH_PCODE:
         /* save currently selected workarea
          */
         ZH_GEN_FUNC1( PCode1, ZH_P_PUSHALIAS );
         /* push the expression that will return a new workarea
          */
         ZH_EXPR_USE( pSelf->value.asAlias.pAlias, ZH_EA_PUSH_PCODE );
         /* pop the value from the stack and select it as current workarea
          */
         ZH_GEN_FUNC1( PCode1, ZH_P_POPALIAS );
         /* evaluate any expression
          */
         ZH_EXPR_USE( pSelf->value.asAlias.pExpList, ZH_EA_PUSH_PCODE );
         /* swap the two last items on the eval stack: one item is a
          * value returned by evaluated expression and the second item
          * is previously selected workarea. After swapping select again
          * the restored workarea.
          */
         ZH_GEN_FUNC1( PCode1, ZH_P_SWAPALIAS );
         break;

      case ZH_EA_POP_PCODE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_PUSH_POP:
      case ZH_EA_STATEMENT:
         /* save currently selected workarea
          */
         ZH_GEN_FUNC1( PCode1, ZH_P_PUSHALIAS );
         /* push the expression that will return a new workarea
          */
         ZH_EXPR_USE( pSelf->value.asAlias.pAlias, ZH_EA_PUSH_PCODE );
         /* pop the value from the stack and select it as current workarea
          */
         ZH_GEN_FUNC1( PCode1, ZH_P_POPALIAS );
         /* evaluate any expression - it will not leave any return
          * value on the eval stack
          */
         ZH_EXPR_USE( pSelf->value.asAlias.pExpList, ZH_EA_PUSH_POP );
         /* Pop and select again the restored workarea.
          */
         ZH_GEN_FUNC1( PCode1, ZH_P_POPALIAS );
         break;

      case ZH_EA_DELETE:
         ZH_COMP_EXPR_FREE( pSelf->value.asAlias.pAlias );
         ZH_COMP_EXPR_FREE( pSelf->value.asAlias.pExpList );
         break;
   }
   return pSelf;
}

static ZH_EXPR_FUNC( zh_compExprUseAlias )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
      case ZH_EA_ARRAY_AT:
      case ZH_EA_ARRAY_INDEX:
      case ZH_EA_LVALUE:
         break;

      case ZH_EA_PUSH_PCODE:
         ZH_GEN_FUNC2( PushSymbol, pSelf->value.asSymbol.name, ZH_SYM_ALIAS );
         break;

      case ZH_EA_POP_PCODE:
      case ZH_EA_PUSH_POP:
      case ZH_EA_STATEMENT:
         break;

      case ZH_EA_DELETE:
         break;
   }
   return pSelf;
}

static ZH_EXPR_FUNC( zh_compExprUseFunName )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
      case ZH_EA_ARRAY_AT:
      case ZH_EA_ARRAY_INDEX:
      case ZH_EA_LVALUE:
         break;

      case ZH_EA_PUSH_PCODE:
         ZH_GEN_FUNC2( PushFunSym, pSelf->value.asSymbol.name,
                                   pSelf->value.asSymbol.flags );
         break;

      case ZH_EA_POP_PCODE:
      case ZH_EA_PUSH_POP:
      case ZH_EA_STATEMENT:
      case ZH_EA_DELETE:
         break;
   }
   return pSelf;
}

static ZH_EXPR_FUNC( zh_compExprUseRTVariable )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
      case ZH_EA_ARRAY_AT:
      case ZH_EA_ARRAY_INDEX:
      case ZH_EA_LVALUE:
         break;
      case ZH_EA_PUSH_PCODE:
         if( pSelf->value.asRTVar.szName )
            ZH_GEN_FUNC2( PushSymbol, pSelf->value.asRTVar.szName, ZH_SYM_MEMVAR );  /* this is not a function */
         else
            ZH_EXPR_USE( pSelf->value.asRTVar.pMacro, ZH_EA_PUSH_PCODE );
         break;
      case ZH_EA_POP_PCODE:
         if( pSelf->value.asRTVar.szName )
            ZH_GEN_FUNC1( PopMemvar, pSelf->value.asRTVar.szName );
         else
            ZH_EXPR_USE( pSelf->value.asRTVar.pMacro, ZH_EA_POP_PCODE );
         break;

      case ZH_EA_PUSH_POP:
      case ZH_EA_STATEMENT:
         break;
      case ZH_EA_DELETE:
#if ! defined( ZH_MACRO_SUPPORT )
         if( ! pSelf->value.asRTVar.szName )
            ZH_COMP_EXPR_FREE( pSelf->value.asRTVar.pMacro );
#endif
         break;
   }
   return pSelf;
}

static ZH_EXPR_FUNC( zh_compExprUseVariable )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
      case ZH_EA_ARRAY_AT:
      case ZH_EA_ARRAY_INDEX:
      case ZH_EA_LVALUE:
         break;
      case ZH_EA_PUSH_PCODE:
#if defined( ZH_MACRO_SUPPORT )
         /* NOTE: When the following syntax is used:
          *    ( any_expr )->&var2
          * then macro compiler is compiling the right side of alias
          * operator only (if 'any_expr' is not a string) - an alias value
          * is placed on the eval stack before macro compilation.
          * The ZH_MACRO_GEN_ALIASED flag is used to signal that we have to
          * generate alias aware pcode even if we known a variable part only.
          */
         if( ZH_MACRO_DATA->Flags & ZH_MACRO_GEN_ALIASED )
            ZH_GEN_FUNC4( PushAliasedVar, pSelf->value.asSymbol.name, ZH_FALSE, NULL, 0 );
         else
            ZH_GEN_FUNC1( PushVar, pSelf->value.asSymbol.name );
#else
         ZH_GEN_FUNC1( PushVar, pSelf->value.asSymbol.name );
#endif
         break;

      case ZH_EA_POP_PCODE:
#if defined( ZH_MACRO_SUPPORT )
         if( ZH_MACRO_DATA->Flags & ZH_MACRO_GEN_ALIASED )
            ZH_GEN_FUNC4( PopAliasedVar, pSelf->value.asSymbol.name, ZH_FALSE, NULL, 0 );
         else
            ZH_GEN_FUNC1( PopVar, pSelf->value.asSymbol.name );
#else
         ZH_GEN_FUNC1( PopVar, pSelf->value.asSymbol.name );
#endif
         break;

      case ZH_EA_PUSH_POP:
      case ZH_EA_STATEMENT:
         ZH_GEN_FUNC1( PushVar, pSelf->value.asSymbol.name );
         ZH_GEN_FUNC1( PCode1, ZH_P_POP );
         break;

      case ZH_EA_DELETE:
         break;
   }
   return pSelf;
}

/* IIF( <pVar>==NIL, <pExpr>, <pExpr>:=<pVar> ) */
static ZH_EXPR_FUNC( zh_compExprUseSetGet )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         pSelf->value.asSetGet.pVar = ZH_EXPR_USE( pSelf->value.asSetGet.pVar, ZH_EA_REDUCE );
         pSelf->value.asSetGet.pExpr = ZH_EXPR_USE( pSelf->value.asSetGet.pExpr, ZH_EA_REDUCE );
#if ! defined( ZH_MACRO_SUPPORT )
         if( ! ZH_SUPPORT_ZIHER )
            pSelf->value.asSetGet.pVar = zh_compExprListStrip( pSelf->value.asSetGet.pVar, ZH_COMP_PARAM );
#endif
         ZH_EXPR_USE( pSelf->value.asSetGet.pExpr, ZH_EA_LVALUE );
         break;
      case ZH_EA_ARRAY_AT:
      case ZH_EA_ARRAY_INDEX:
      case ZH_EA_LVALUE:
         break;
      case ZH_EA_PUSH_PCODE:
      {
         ZH_I_SIZE nPosFalse, nPosEnd;

         /* <pVar>==NIL */
         ZH_EXPR_USE( pSelf->value.asSetGet.pVar, ZH_EA_PUSH_PCODE );
         ZH_GEN_FUNC1( PCode1, ZH_P_PUSHNIL );
         ZH_GEN_FUNC1( PCode1, ZH_P_EXACTLYEQUAL );
         nPosFalse = ZH_GEN_FUNC1( JumpFalse, 0 );
         /* <pExpr> */
         ZH_EXPR_USE( pSelf->value.asSetGet.pExpr, ZH_EA_PUSH_PCODE );
         nPosEnd = ZH_GEN_FUNC1( Jump, 0 );
         /* <pExpr>:=<pVar> */
         ZH_GEN_FUNC1( JumpHere, nPosFalse );
         if( pSelf->value.asSetGet.pExpr->ExprType == ZH_ET_SEND )
         {
            PZH_EXPR pObj, pParams;
            pObj = pSelf->value.asSetGet.pExpr;
            pParams = pObj->value.asMessage.pParms;
            pObj->value.asMessage.pParms = pSelf->value.asSetGet.pVar;
            ZH_EXPR_USE( pObj, ZH_EA_POP_PCODE );
            pObj->value.asMessage.pParms = pParams;
         }
         else
         {
            ZH_EXPR_USE( pSelf->value.asSetGet.pVar, ZH_EA_PUSH_PCODE );
            ZH_GEN_FUNC1( PCode1, ZH_P_PUSHUNREF );
            ZH_EXPR_USE( pSelf->value.asSetGet.pExpr, ZH_EA_POP_PCODE );
         }
         ZH_GEN_FUNC1( JumpHere, nPosEnd );
         break;
      }
      case ZH_EA_POP_PCODE:
         break;

      case ZH_EA_PUSH_POP:
      case ZH_EA_STATEMENT:
      {
         ZH_I_SIZE nPosFalse, nPosEnd;

         /* <pVar>==NIL */
         ZH_EXPR_USE( pSelf->value.asSetGet.pVar, ZH_EA_PUSH_PCODE );
         ZH_GEN_FUNC1( PCode1, ZH_P_PUSHNIL );
         ZH_GEN_FUNC1( PCode1, ZH_P_EXACTLYEQUAL );
         nPosFalse = ZH_GEN_FUNC1( JumpFalse, 0 );
         /* <pExpr> */
         ZH_EXPR_USE( pSelf->value.asSetGet.pExpr, ZH_EA_PUSH_PCODE );
         nPosEnd = ZH_GEN_FUNC1( Jump, 0 );
         /* <pExpr>:=<pVar> */
         ZH_GEN_FUNC1( JumpHere, nPosFalse );
         if( pSelf->value.asSetGet.pExpr->ExprType == ZH_ET_SEND )
         {
            PZH_EXPR pObj, pParams;
            pObj = pSelf->value.asSetGet.pExpr;
            pParams = pObj->value.asMessage.pParms;
            pObj->value.asMessage.pParms = pSelf->value.asSetGet.pVar;
            ZH_EXPR_USE( pObj, ZH_EA_POP_PCODE );
            pObj->value.asMessage.pParms = pParams;
            /* Remove the return value */
            ZH_GEN_FUNC1( PCode1, ZH_P_POP );
         }
         else
         {
            ZH_EXPR_USE( pSelf->value.asSetGet.pVar, ZH_EA_PUSH_PCODE );
            ZH_EXPR_USE( pSelf->value.asSetGet.pExpr, ZH_EA_POP_PCODE );
         }
         ZH_GEN_FUNC1( JumpHere, nPosEnd );
         break;
      }

      case ZH_EA_DELETE:
         ZH_COMP_EXPR_FREE( pSelf->value.asSetGet.pExpr );
         ZH_COMP_EXPR_FREE( pSelf->value.asSetGet.pVar );
         break;
   }
   return pSelf;
}

static ZH_EXPR_FUNC( zh_compExprUseSend )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         if( pSelf->value.asMessage.pObject &&
             ( ZH_SUPPORT_ZIHER || pSelf->nLength == 1 ) )
            pSelf->value.asMessage.pObject = ZH_EXPR_USE( pSelf->value.asMessage.pObject, ZH_EA_REDUCE );
         if( pSelf->value.asMessage.pParms )  /* Is it a method call ? */
            pSelf->value.asMessage.pParms = ZH_EXPR_USE( pSelf->value.asMessage.pParms, ZH_EA_REDUCE );
         break;

      case ZH_EA_ARRAY_AT:
      case ZH_EA_ARRAY_INDEX:
         break;

      case ZH_EA_LVALUE:
         if( pSelf->value.asMessage.pParms )
            zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_PUSH_PCODE:
         if( pSelf->value.asMessage.pParms )  /* Is it a method call ? */
         {
            ZH_BOOL fArgsList = ZH_FALSE;
            int iParms = ( int ) zh_compExprParamListCheck( ZH_COMP_PARAM, pSelf->value.asMessage.pParms );

            zh_compExprPushSendPush( pSelf, ZH_COMP_PARAM );
            if( iParms )
            {
               ZH_EXPR_USE( pSelf->value.asMessage.pParms, ZH_EA_PUSH_PCODE );
               fArgsList = pSelf->value.asMessage.pParms->ExprType == ZH_ET_MACROARGLIST;
            }

            if( fArgsList )
            {
               ZH_GEN_FUNC3( PCode3, ZH_P_MACRO_SEND, ZH_LOBYTE( iParms ), ZH_HIBYTE( iParms ) );
               /* restore original expression type */
               pSelf->value.asMessage.pParms->ExprType = ZH_ET_ARGLIST;
            }
            else if( iParms > 255 )
               ZH_GEN_FUNC3( PCode3, ZH_P_SEND, ZH_LOBYTE( iParms ), ZH_HIBYTE( iParms ) );
            else
               ZH_GEN_FUNC2( PCode2, ZH_P_SENDSHORT, ( ZH_BYTE ) iParms );
         }
         else
         {
            /* access to instance variable */
            zh_compExprPushSendPush( pSelf, ZH_COMP_PARAM );
            ZH_GEN_FUNC2( PCode2, ZH_P_SENDSHORT, 0 );
         }
         break;

      case ZH_EA_POP_PCODE:
         zh_compExprPushSendPop( pSelf, ZH_COMP_PARAM );
         if( pSelf->value.asMessage.pParms )
         {
            ZH_EXPR_USE( pSelf->value.asMessage.pParms, ZH_EA_PUSH_PCODE );
         }
         else
         {
            /* executed from macro compiler */
            ZH_GEN_FUNC2( PCode2, ZH_P_SWAP, 1 );
            ZH_GEN_FUNC2( PCode2, ZH_P_SWAP, 1 );
         }
         ZH_GEN_FUNC2( PCode2, ZH_P_SENDSHORT, 1 );
         if( ! pSelf->value.asMessage.pParms )
            ZH_GEN_FUNC1( PCode1, ZH_P_POP );
         break;

      case ZH_EA_PUSH_POP:
      case ZH_EA_STATEMENT:
         ZH_EXPR_USE( pSelf, ZH_EA_PUSH_PCODE );
         ZH_GEN_FUNC1( PCode1, ZH_P_POP );
         if( ! pSelf->value.asMessage.pParms )  /* Is it a method call ? */
         {
            /* instance variable */
            /* QUESTION: This warning can be misleading if nested messages
             * are used, e.g. a:b():c - should we generate it ?
             */
            zh_compWarnMeaningless( ZH_COMP_PARAM, pSelf );
         }
         break;

      case ZH_EA_DELETE:
         if( pSelf->value.asMessage.pObject )
            ZH_COMP_EXPR_FREE( pSelf->value.asMessage.pObject );
         if( pSelf->value.asMessage.pParms )
            ZH_COMP_EXPR_FREE( pSelf->value.asMessage.pParms );
         if( pSelf->value.asMessage.pMessage )
            ZH_COMP_EXPR_FREE( pSelf->value.asMessage.pMessage );
         break;
   }
   return pSelf;
}

static ZH_EXPR_FUNC( zh_compExprUsePostInc )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         pSelf->value.asOperator.pLeft = ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_REDUCE );
#if ! defined( ZH_MACRO_SUPPORT )
         if( ! ZH_SUPPORT_ZIHER )
            pSelf->value.asOperator.pLeft = zh_compExprListStrip( pSelf->value.asOperator.pLeft, ZH_COMP_PARAM );
#endif
         ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_LVALUE );
         break;
      case ZH_EA_ARRAY_AT:
      case ZH_EA_ARRAY_INDEX:
         break;

      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_PUSH_PCODE:
         zh_compExprPushPostOp( pSelf, ZH_P_INC, ZH_COMP_PARAM );
         break;

      case ZH_EA_POP_PCODE:
         break;

      case ZH_EA_PUSH_POP:
      case ZH_EA_STATEMENT:
         /* a++ used standalone as a statement is the same as ++a
          */
         zh_compExprUsePreOp( pSelf, ZH_P_INC, ZH_COMP_PARAM );
         break;

      case ZH_EA_DELETE:
         if( pSelf->value.asOperator.pLeft )
            ZH_COMP_EXPR_FREE( pSelf->value.asOperator.pLeft );
         break;
   }
   return pSelf;
}

static ZH_EXPR_FUNC( zh_compExprUsePostDec )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         pSelf->value.asOperator.pLeft = ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_REDUCE );
#if ! defined( ZH_MACRO_SUPPORT )
         if( ! ZH_SUPPORT_ZIHER )
            pSelf->value.asOperator.pLeft = zh_compExprListStrip( pSelf->value.asOperator.pLeft, ZH_COMP_PARAM );
#endif
         ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_LVALUE );
         break;
      case ZH_EA_ARRAY_AT:
      case ZH_EA_ARRAY_INDEX:
         break;

      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_PUSH_PCODE:
         zh_compExprPushPostOp( pSelf, ZH_P_DEC, ZH_COMP_PARAM );
         break;

      case ZH_EA_POP_PCODE:
         break;

      case ZH_EA_PUSH_POP:
      case ZH_EA_STATEMENT:
         zh_compExprUsePreOp( pSelf, ZH_P_DEC, ZH_COMP_PARAM );
         break;

      case ZH_EA_DELETE:
         if( pSelf->value.asOperator.pLeft )
            ZH_COMP_EXPR_FREE( pSelf->value.asOperator.pLeft );
         break;
   }
   return pSelf;
}

static ZH_EXPR_FUNC( zh_compExprUseAssign )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
      {
         PZH_EXPR pExpr;

         pSelf->value.asOperator.pLeft  = ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_REDUCE );
         pSelf->value.asOperator.pRight = ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_REDUCE );
#if ! defined( ZH_MACRO_SUPPORT )
         if( ! ZH_SUPPORT_ZIHER )
            pSelf->value.asOperator.pLeft = zh_compExprListStrip( pSelf->value.asOperator.pLeft, ZH_COMP_PARAM );
#endif
         ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_LVALUE );

         /* optimize:
          *    var := var <op> <exp>
          * to:
          *    var <op>= <exp>
          */
         pExpr = pSelf->value.asOperator.pRight;
         if( ZH_SUPPORT_ZIHER &&
             pSelf->value.asOperator.pLeft->ExprType == ZH_ET_VARIABLE &&
             ( pExpr->ExprType == ZH_EO_PLUS || pExpr->ExprType == ZH_EO_MINUS ||
               pExpr->ExprType == ZH_EO_MULT || pExpr->ExprType == ZH_EO_DIV ||
               pExpr->ExprType == ZH_EO_MOD  || pExpr->ExprType == ZH_EO_POWER ) &&
             pExpr->value.asOperator.pLeft->ExprType == ZH_ET_VARIABLE &&
             strcmp( pSelf->value.asOperator.pLeft->value.asSymbol.name,
                     pExpr->value.asOperator.pLeft->value.asSymbol.name ) == 0 )
         {
            /* NOTE: direct type change */
            switch( pExpr->ExprType )
            {
               case ZH_EO_PLUS:
                  pSelf->ExprType = ZH_EO_PLUSEQ;
                  break;
               case ZH_EO_MINUS:
                  pSelf->ExprType = ZH_EO_MINUSEQ;
                  break;
               case ZH_EO_MULT:
                  pSelf->ExprType = ZH_EO_MULTEQ;
                  break;
               case ZH_EO_DIV:
                  pSelf->ExprType = ZH_EO_DIVEQ;
                  break;
               case ZH_EO_MOD:
                  pSelf->ExprType = ZH_EO_MODEQ;
                  break;
               case ZH_EO_POWER:
                  pSelf->ExprType = ZH_EO_EXPEQ;
                  break;
            }
            pSelf->value.asOperator.pRight = pExpr->value.asOperator.pRight;
            pExpr->value.asOperator.pRight = NULL;
            ZH_COMP_EXPR_FREE( pExpr );
         }
         break;
      }

      case ZH_EA_ARRAY_AT:
      case ZH_EA_ARRAY_INDEX:
      case ZH_EA_LVALUE:
         break;

      case ZH_EA_PUSH_PCODE:
         /* NOTE: assignment to an object instance variable needs special handling
          */
         if( pSelf->value.asOperator.pLeft->ExprType == ZH_ET_SEND )
         {
            PZH_EXPR pObj, pParams;
            pObj = pSelf->value.asOperator.pLeft;
            pParams = pObj->value.asMessage.pParms;
            pObj->value.asMessage.pParms = pSelf->value.asOperator.pRight;
            ZH_EXPR_USE( pObj, ZH_EA_POP_PCODE );
            pObj->value.asMessage.pParms = pParams;
         }
         else
         {
            /* it assigns a value and leaves it on the stack */

            ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_PCODE );
            /* QUESTION: Can  we replace DUPLICATE+POP with a single PUT opcode
             */
            ZH_GEN_FUNC1( PCode1, ZH_P_PUSHUNREF );
            ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_POP_PCODE );
         }
         break;

      case ZH_EA_POP_PCODE:
         break;

      case ZH_EA_PUSH_POP:
      case ZH_EA_STATEMENT:
         /* NOTE: assigment to an object instance variable needs special handling
          */
         if( pSelf->value.asOperator.pLeft->ExprType == ZH_ET_SEND )
         {
            PZH_EXPR pObj, pParams;
            pObj = pSelf->value.asOperator.pLeft;
            pParams = pObj->value.asMessage.pParms;
            pObj->value.asMessage.pParms = pSelf->value.asOperator.pRight;
            ZH_EXPR_USE( pObj, ZH_EA_POP_PCODE );
            pObj->value.asMessage.pParms = pParams;
            /* Remove the return value */
            ZH_GEN_FUNC1( PCode1, ZH_P_POP );
         }
         else
         {
            /* it assigns a value and removes it from the stack */
            ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_PCODE );
            ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_POP_PCODE );
         }
         break;

      case ZH_EA_DELETE:
         zh_compExprDelOperator( pSelf, ZH_COMP_PARAM );
         break;
   }
   return pSelf;
}

static ZH_EXPR_FUNC( zh_compExprUsePlusEq )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_REDUCE );
         pSelf->value.asOperator.pRight = ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_REDUCE );
#if ! defined( ZH_MACRO_SUPPORT )
         if( ! ZH_SUPPORT_ZIHER )
            pSelf->value.asOperator.pLeft = zh_compExprListStrip( pSelf->value.asOperator.pLeft, ZH_COMP_PARAM );
#endif
         ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_LVALUE );
         break;

      case ZH_EA_ARRAY_AT:
         ZH_COMP_ERROR_TYPE( pSelf );
         break;

      case ZH_EA_ARRAY_INDEX:
         break;

      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_PUSH_PCODE:
         zh_compExprPushOperEq( pSelf, ZH_P_PLUS, ZH_COMP_PARAM );
         break;

      case ZH_EA_POP_PCODE:
         break;

      case ZH_EA_PUSH_POP:
      case ZH_EA_STATEMENT:
         zh_compExprUseOperEq( pSelf, ZH_P_PLUS, ZH_COMP_PARAM );
         break;

      case ZH_EA_DELETE:
         zh_compExprDelOperator( pSelf, ZH_COMP_PARAM );
         break;
   }
   return pSelf;
}

static ZH_EXPR_FUNC( zh_compExprUseMinusEq )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_REDUCE );
         pSelf->value.asOperator.pRight = ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_REDUCE );
#if ! defined( ZH_MACRO_SUPPORT )
         if( ! ZH_SUPPORT_ZIHER )
            pSelf->value.asOperator.pLeft = zh_compExprListStrip( pSelf->value.asOperator.pLeft, ZH_COMP_PARAM );
#endif
         ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_LVALUE );
         break;

      case ZH_EA_ARRAY_AT:
         ZH_COMP_ERROR_TYPE( pSelf );
         break;

      case ZH_EA_ARRAY_INDEX:
         break;

      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_PUSH_PCODE:
         zh_compExprPushOperEq( pSelf, ZH_P_MINUS, ZH_COMP_PARAM );
         break;

      case ZH_EA_POP_PCODE:
         break;

      case ZH_EA_PUSH_POP:
      case ZH_EA_STATEMENT:
         zh_compExprUseOperEq( pSelf, ZH_P_MINUS, ZH_COMP_PARAM );
         break;

      case ZH_EA_DELETE:
         zh_compExprDelOperator( pSelf, ZH_COMP_PARAM );
         break;
   }
   return pSelf;
}

static ZH_EXPR_FUNC( zh_compExprUseMultEq )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_REDUCE );
         pSelf->value.asOperator.pRight = ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_REDUCE );
#if ! defined( ZH_MACRO_SUPPORT )
         if( ! ZH_SUPPORT_ZIHER )
            pSelf->value.asOperator.pLeft = zh_compExprListStrip( pSelf->value.asOperator.pLeft, ZH_COMP_PARAM );
#endif
         ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_LVALUE );
         break;

      case ZH_EA_ARRAY_AT:
         ZH_COMP_ERROR_TYPE( pSelf );
         break;

      case ZH_EA_ARRAY_INDEX:
         break;

      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_PUSH_PCODE:
         zh_compExprPushOperEq( pSelf, ZH_P_MULT, ZH_COMP_PARAM );
         break;

      case ZH_EA_POP_PCODE:
         break;

      case ZH_EA_PUSH_POP:
      case ZH_EA_STATEMENT:
         zh_compExprUseOperEq( pSelf, ZH_P_MULT, ZH_COMP_PARAM );
         break;

      case ZH_EA_DELETE:
         zh_compExprDelOperator( pSelf, ZH_COMP_PARAM );
         break;
   }
   return pSelf;
}

static ZH_EXPR_FUNC( zh_compExprUseDivEq )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_REDUCE );
         pSelf->value.asOperator.pRight = ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_REDUCE );
#if ! defined( ZH_MACRO_SUPPORT )
         if( ! ZH_SUPPORT_ZIHER )
            pSelf->value.asOperator.pLeft = zh_compExprListStrip( pSelf->value.asOperator.pLeft, ZH_COMP_PARAM );
#endif
         ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_LVALUE );
         break;

      case ZH_EA_ARRAY_AT:
         ZH_COMP_ERROR_TYPE( pSelf );
         break;

      case ZH_EA_ARRAY_INDEX:
         break;

      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_PUSH_PCODE:
         zh_compExprPushOperEq( pSelf, ZH_P_DIVIDE, ZH_COMP_PARAM );
         break;

      case ZH_EA_POP_PCODE:
         break;

      case ZH_EA_PUSH_POP:
      case ZH_EA_STATEMENT:
         zh_compExprUseOperEq( pSelf, ZH_P_DIVIDE, ZH_COMP_PARAM );
         break;

      case ZH_EA_DELETE:
         zh_compExprDelOperator( pSelf, ZH_COMP_PARAM );
         break;
   }
   return pSelf;
}

static ZH_EXPR_FUNC( zh_compExprUseModEq )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_REDUCE );
         pSelf->value.asOperator.pRight = ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_REDUCE );
#if ! defined( ZH_MACRO_SUPPORT )
         if( ! ZH_SUPPORT_ZIHER )
            pSelf->value.asOperator.pLeft = zh_compExprListStrip( pSelf->value.asOperator.pLeft, ZH_COMP_PARAM );
#endif
         ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_LVALUE );
         break;

      case ZH_EA_ARRAY_AT:
         ZH_COMP_ERROR_TYPE( pSelf );
         break;

      case ZH_EA_ARRAY_INDEX:
         break;

      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_PUSH_PCODE:
         zh_compExprPushOperEq( pSelf, ZH_P_MODULUS, ZH_COMP_PARAM );
         break;

      case ZH_EA_POP_PCODE:
         break;

      case ZH_EA_PUSH_POP:
      case ZH_EA_STATEMENT:
         zh_compExprUseOperEq( pSelf, ZH_P_MODULUS, ZH_COMP_PARAM );
         break;

      case ZH_EA_DELETE:
         zh_compExprDelOperator( pSelf, ZH_COMP_PARAM );
         break;
   }
   return pSelf;
}

static ZH_EXPR_FUNC( zh_compExprUseExpEq )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_REDUCE );
         pSelf->value.asOperator.pRight = ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_REDUCE );
#if ! defined( ZH_MACRO_SUPPORT )
         if( ! ZH_SUPPORT_ZIHER )
            pSelf->value.asOperator.pLeft = zh_compExprListStrip( pSelf->value.asOperator.pLeft, ZH_COMP_PARAM );
#endif
         ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_LVALUE );
         break;

      case ZH_EA_ARRAY_AT:
         ZH_COMP_ERROR_TYPE( pSelf );
         break;

      case ZH_EA_ARRAY_INDEX:
         break;

      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_PUSH_PCODE:
         zh_compExprPushOperEq( pSelf, ZH_P_POWER, ZH_COMP_PARAM );
         break;

      case ZH_EA_POP_PCODE:
         break;

      case ZH_EA_PUSH_POP:
      case ZH_EA_STATEMENT:
         zh_compExprUseOperEq( pSelf, ZH_P_POWER, ZH_COMP_PARAM );
         break;

      case ZH_EA_DELETE:
         zh_compExprDelOperator( pSelf, ZH_COMP_PARAM );
         break;
   }
   return pSelf;
}

static ZH_EXPR_FUNC( zh_compExprUseOr )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_REDUCE );
         pSelf->value.asOperator.pRight = ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_REDUCE );
         pSelf = zh_compExprReduceOr( pSelf, ZH_COMP_PARAM );
         break;

      case ZH_EA_ARRAY_AT:
         ZH_COMP_ERROR_TYPE( pSelf );
         break;

      case ZH_EA_ARRAY_INDEX:
         zh_compErrorIndex( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_PUSH_PCODE:
         if( ZH_COMP_ISSUPPORTED( ZH_COMPFLAG_SHORTCUTS ) )
         {
            ZH_I_SIZE nEndPos;

            ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_PCODE );
            ZH_GEN_FUNC1( PCode1, ZH_P_DUPLICATE );
            nEndPos = ZH_GEN_FUNC1( JumpTrue, 0 );
            ZH_GEN_FUNC1( PCode1, ZH_P_POP );
            ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_PCODE );
            ZH_GEN_FUNC1( JumpHere, nEndPos );
         }
         else
         {
            ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_PCODE );
            ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_PCODE );
            ZH_GEN_FUNC1( PCode1, ZH_P_OR );
         }
         break;

      case ZH_EA_POP_PCODE:
         break;

      case ZH_EA_PUSH_POP:
         if( ZH_COMP_ISSUPPORTED( ZH_COMPFLAG_SHORTCUTS ) )
         {
            ZH_I_SIZE nEndPos;
            ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_PCODE );
            nEndPos = ZH_GEN_FUNC1( JumpTrue, 0 );
            /* NOTE: This will not generate a runtime error if incompatible
             * data type is used
             */
#if defined( ZH_MACRO_SUPPORT )
            ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_POP );
#else
            {
               ZH_BOOL fMeaningful = ZH_COMP_PARAM->fMeaningful;
               /* do not generate warning about meaningless expression usage */
               ZH_COMP_PARAM->fMeaningful = ZH_TRUE;
               ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_POP );
               ZH_COMP_PARAM->fMeaningful = fMeaningful;
            }
#endif
            ZH_GEN_FUNC1( JumpHere, nEndPos );
         }
         else
         {
            ZH_EXPR_USE( pSelf, ZH_EA_PUSH_PCODE );
            ZH_GEN_FUNC1( PCode1, ZH_P_POP );
         }
         break;

      case ZH_EA_STATEMENT:
         ZH_COMP_ERROR_SYNTAX( pSelf );
         break;

      case ZH_EA_DELETE:
         zh_compExprDelOperator( pSelf, ZH_COMP_PARAM );
         break;
   }
   return pSelf;
}

static ZH_EXPR_FUNC( zh_compExprUseAnd )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_REDUCE );
         pSelf->value.asOperator.pRight = ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_REDUCE );
         pSelf = zh_compExprReduceAnd( pSelf, ZH_COMP_PARAM );
         break;

      case ZH_EA_ARRAY_AT:
         ZH_COMP_ERROR_TYPE( pSelf );
         break;

      case ZH_EA_ARRAY_INDEX:
         zh_compErrorIndex( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_PUSH_PCODE:
         if( ZH_COMP_ISSUPPORTED( ZH_COMPFLAG_SHORTCUTS ) )
         {
            ZH_I_SIZE nEndPos;

            ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_PCODE );
            ZH_GEN_FUNC1( PCode1, ZH_P_DUPLICATE );
            nEndPos = ZH_GEN_FUNC1( JumpFalse, 0 );
            ZH_GEN_FUNC1( PCode1, ZH_P_POP );
            ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_PCODE );
            ZH_GEN_FUNC1( JumpHere, nEndPos );
         }
         else
         {
            ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_PCODE );
            ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_PCODE );
            ZH_GEN_FUNC1( PCode1, ZH_P_AND );
         }
         break;

      case ZH_EA_POP_PCODE:
         break;

      case ZH_EA_PUSH_POP:
         if( ZH_COMP_ISSUPPORTED( ZH_COMPFLAG_SHORTCUTS ) )
         {
            ZH_I_SIZE nEndPos;
            ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_PCODE );
            nEndPos = ZH_GEN_FUNC1( JumpFalse, 0 );
            /* NOTE: This will not generate a runtime error if incompatible
             * data type is used
             */
#if defined( ZH_MACRO_SUPPORT )
            ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_POP );
#else
            {
               ZH_BOOL fMeaningful = ZH_COMP_PARAM->fMeaningful;
               /* do not generate warning about meaningless expression usage */
               ZH_COMP_PARAM->fMeaningful = ZH_TRUE;
               ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_POP );
               ZH_COMP_PARAM->fMeaningful = fMeaningful;
            }
#endif
            ZH_GEN_FUNC1( JumpHere, nEndPos );
         }
         else
         {
            ZH_EXPR_USE( pSelf, ZH_EA_PUSH_PCODE );
            ZH_GEN_FUNC1( PCode1, ZH_P_POP );
         }
         break;

      case ZH_EA_STATEMENT:
         ZH_COMP_ERROR_SYNTAX( pSelf );
         break;

      case ZH_EA_DELETE:
         zh_compExprDelOperator( pSelf, ZH_COMP_PARAM );
         break;
   }
   return pSelf;
}

static ZH_EXPR_FUNC( zh_compExprUseNot )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
      {
         PZH_EXPR pExpr;

         pSelf->value.asOperator.pLeft = ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_REDUCE );
         pExpr = pSelf->value.asOperator.pLeft;

         if( pExpr->ExprType == ZH_ET_LOGICAL )
         {
            pExpr->value.asLogical = ! pExpr->value.asLogical;
            ZH_COMP_EXPR_CLEAR( pSelf );
            pSelf = pExpr;
         }
         else if( pExpr->ExprType == ZH_EO_NOT && ZH_SUPPORT_EXTOPT )
         {
            /* NOTE: This will not generate a runtime error if incompatible
             * data type is used
             */
            pExpr->ExprType = ZH_ET_NONE;  /* do not delete operator parameter - we are still using it */
            pExpr = pExpr->value.asOperator.pLeft;
            ZH_COMP_EXPR_FREE( pSelf );
            pSelf = pExpr;
         }
         break;
      }
      case ZH_EA_ARRAY_AT:
         ZH_COMP_ERROR_TYPE( pSelf );
         break;

      case ZH_EA_ARRAY_INDEX:
         zh_compErrorIndex( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_PUSH_PCODE:
         ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_PCODE );
         ZH_GEN_FUNC1( PCode1, ZH_P_NOT );
         break;

      case ZH_EA_POP_PCODE:
         break;

      case ZH_EA_PUSH_POP:
         if( ZH_SUPPORT_ZIHER )
         {
            /* NOTE: This will not generate a runtime error if incompatible
             * data type is used
             */
            ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_POP );
         }
         else
         {
            ZH_EXPR_USE( pSelf, ZH_EA_PUSH_PCODE );
            ZH_GEN_FUNC1( PCode1, ZH_P_POP );
         }
         break;

      case ZH_EA_STATEMENT:
         ZH_COMP_ERROR_SYNTAX( pSelf );
         break;

      case ZH_EA_DELETE:
         ZH_COMP_EXPR_FREE( pSelf->value.asOperator.pLeft );
         break;
   }
   return pSelf;
}

/* handler for = operator
 */
static ZH_EXPR_FUNC( zh_compExprUseEqual )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_REDUCE );
         pSelf->value.asOperator.pRight = ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_REDUCE );
         pSelf = zh_compExprReduceEQ( pSelf, ZH_COMP_PARAM );
         break;

      case ZH_EA_ARRAY_AT:
         ZH_COMP_ERROR_TYPE( pSelf );
         break;

      case ZH_EA_ARRAY_INDEX:
         break;

      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;
      case ZH_EA_PUSH_PCODE:
         ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_PCODE );
         ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_PCODE );
         ZH_GEN_FUNC1( PCode1, ZH_P_EQUAL );
         break;

      case ZH_EA_POP_PCODE:
         break;

      case ZH_EA_PUSH_POP:
         if( ZH_SUPPORT_ZIHER )
         {
            /* NOTE: This will not generate a runtime error if incompatible
             * data type is used
             */
            ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_POP );
            ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_POP );
         }
         else
         {
            ZH_EXPR_USE( pSelf, ZH_EA_PUSH_PCODE );
            ZH_GEN_FUNC1( PCode1, ZH_P_POP );
         }
         break;

      case ZH_EA_STATEMENT:
         ZH_COMP_ERROR_SYNTAX( pSelf );
         break;

      case ZH_EA_DELETE:
         zh_compExprDelOperator( pSelf, ZH_COMP_PARAM );
         break;
   }
   return pSelf;
}

/* handler for == operator
 */
static ZH_EXPR_FUNC( zh_compExprUseEQ )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_REDUCE );
         pSelf->value.asOperator.pRight = ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_REDUCE );
         pSelf = zh_compExprReduceEQ( pSelf, ZH_COMP_PARAM );
         break;

      case ZH_EA_ARRAY_AT:
         ZH_COMP_ERROR_TYPE( pSelf );
         break;

      case ZH_EA_ARRAY_INDEX:
         break;

      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;
      case ZH_EA_PUSH_PCODE:
         ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_PCODE );
         ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_PCODE );
         ZH_GEN_FUNC1( PCode1, ZH_P_EXACTLYEQUAL );
         break;

      case ZH_EA_POP_PCODE:
         break;

      case ZH_EA_PUSH_POP:
         if( ZH_SUPPORT_ZIHER )
         {
            /* NOTE: This will not generate a runtime error if incompatible
             * data type is used
             */
            ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_POP );
            ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_POP );
         }
         else
         {
            ZH_EXPR_USE( pSelf, ZH_EA_PUSH_PCODE );
            ZH_GEN_FUNC1( PCode1, ZH_P_POP );
         }
         break;

      case ZH_EA_STATEMENT:
         ZH_COMP_ERROR_SYNTAX( pSelf );
         break;

      case ZH_EA_DELETE:
         zh_compExprDelOperator( pSelf, ZH_COMP_PARAM );
         break;
   }
   return pSelf;
}

static ZH_EXPR_FUNC( zh_compExprUseLT )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_REDUCE );
         pSelf->value.asOperator.pRight = ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_REDUCE );
         pSelf = zh_compExprReduceLT( pSelf, ZH_COMP_PARAM );
         break;

      case ZH_EA_ARRAY_AT:
         ZH_COMP_ERROR_TYPE( pSelf );
         break;

      case ZH_EA_ARRAY_INDEX:
         break;

      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;
      case ZH_EA_PUSH_PCODE:
         ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_PCODE );
         ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_PCODE );
         ZH_GEN_FUNC1( PCode1, ZH_P_LESS );
         break;

      case ZH_EA_POP_PCODE:
         break;

      case ZH_EA_PUSH_POP:
         if( ZH_SUPPORT_ZIHER )
         {
            /* NOTE: This will not generate a runtime error if incompatible
             * data type is used
             */
            ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_POP );
            ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_POP );
         }
         else
         {
            ZH_EXPR_USE( pSelf, ZH_EA_PUSH_PCODE );
            ZH_GEN_FUNC1( PCode1, ZH_P_POP );
         }
         break;

      case ZH_EA_STATEMENT:
         ZH_COMP_ERROR_SYNTAX( pSelf );
         break;

      case ZH_EA_DELETE:
         zh_compExprDelOperator( pSelf, ZH_COMP_PARAM );
         break;
   }
   return pSelf;
}

static ZH_EXPR_FUNC( zh_compExprUseGT )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_REDUCE );
         pSelf->value.asOperator.pRight = ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_REDUCE );
         pSelf = zh_compExprReduceGT( pSelf, ZH_COMP_PARAM );
         break;

      case ZH_EA_ARRAY_AT:
         ZH_COMP_ERROR_TYPE( pSelf );
         break;

      case ZH_EA_ARRAY_INDEX:
         break;

      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;
      case ZH_EA_PUSH_PCODE:
         ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_PCODE );
         ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_PCODE );
         ZH_GEN_FUNC1( PCode1, ZH_P_GREATER );
         break;

      case ZH_EA_POP_PCODE:
         break;

      case ZH_EA_PUSH_POP:
         if( ZH_SUPPORT_ZIHER )
         {
            /* NOTE: This will not generate a runtime error if incompatible
             * data type is used
             */
            ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_POP );
            ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_POP );
         }
         else
         {
            ZH_EXPR_USE( pSelf, ZH_EA_PUSH_PCODE );
            ZH_GEN_FUNC1( PCode1, ZH_P_POP );
         }
         break;

      case ZH_EA_STATEMENT:
         ZH_COMP_ERROR_SYNTAX( pSelf );
         break;

      case ZH_EA_DELETE:
         zh_compExprDelOperator( pSelf, ZH_COMP_PARAM );
         break;
   }
   return pSelf;
}

static ZH_EXPR_FUNC( zh_compExprUseLE )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_REDUCE );
         pSelf->value.asOperator.pRight = ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_REDUCE );
         pSelf = zh_compExprReduceLE( pSelf, ZH_COMP_PARAM );
         break;

      case ZH_EA_ARRAY_AT:
         ZH_COMP_ERROR_TYPE( pSelf );
         break;

      case ZH_EA_ARRAY_INDEX:
         break;

      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_PUSH_PCODE:
         ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_PCODE );
         ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_PCODE );
         ZH_GEN_FUNC1( PCode1, ZH_P_LESSEQUAL );
         break;

      case ZH_EA_POP_PCODE:
         break;

      case ZH_EA_PUSH_POP:
         if( ZH_SUPPORT_ZIHER )
         {
            /* NOTE: This will not generate a runtime error if incompatible
             * data type is used
             */
            ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_POP );
            ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_POP );
         }
         else
         {
            ZH_EXPR_USE( pSelf, ZH_EA_PUSH_PCODE );
            ZH_GEN_FUNC1( PCode1, ZH_P_POP );
         }
         break;

      case ZH_EA_STATEMENT:
         ZH_COMP_ERROR_SYNTAX( pSelf );
         break;

      case ZH_EA_DELETE:
         zh_compExprDelOperator( pSelf, ZH_COMP_PARAM );
         break;
   }
   return pSelf;
}

static ZH_EXPR_FUNC( zh_compExprUseGE )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_REDUCE );
         pSelf->value.asOperator.pRight = ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_REDUCE );
         pSelf = zh_compExprReduceGE( pSelf, ZH_COMP_PARAM );
         break;

      case ZH_EA_ARRAY_AT:
         ZH_COMP_ERROR_TYPE( pSelf );
         break;

      case ZH_EA_ARRAY_INDEX:
         break;

      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_PUSH_PCODE:
         ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_PCODE );
         ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_PCODE );
         ZH_GEN_FUNC1( PCode1, ZH_P_GREATEREQUAL );
         break;

      case ZH_EA_POP_PCODE:
         break;

      case ZH_EA_PUSH_POP:
         if( ZH_SUPPORT_ZIHER )
         {
            /* NOTE: This will not generate a runtime error if incompatible
             * data type is used
             */
            ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_POP );
            ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_POP );
         }
         else
         {
            ZH_EXPR_USE( pSelf, ZH_EA_PUSH_PCODE );
            ZH_GEN_FUNC1( PCode1, ZH_P_POP );
         }
         break;

      case ZH_EA_STATEMENT:
         ZH_COMP_ERROR_SYNTAX( pSelf );
         break;

      case ZH_EA_DELETE:
         zh_compExprDelOperator( pSelf, ZH_COMP_PARAM );
         break;
   }
   return pSelf;
}

static ZH_EXPR_FUNC( zh_compExprUseNE )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_REDUCE );
         pSelf->value.asOperator.pRight = ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_REDUCE );
         pSelf = zh_compExprReduceNE( pSelf, ZH_COMP_PARAM );
         break;

      case ZH_EA_ARRAY_AT:
         ZH_COMP_ERROR_TYPE( pSelf );
         break;

      case ZH_EA_ARRAY_INDEX:
         break;

      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_PUSH_PCODE:
         ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_PCODE );
         ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_PCODE );
         ZH_GEN_FUNC1( PCode1, ZH_P_NOTEQUAL );
         break;

      case ZH_EA_POP_PCODE:
         break;

      case ZH_EA_PUSH_POP:
         if( ZH_SUPPORT_ZIHER )
         {
            /* NOTE: This will not generate a runtime error if incompatible
             * data type is used
             */
            ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_POP );
            ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_POP );
         }
         else
         {
            ZH_EXPR_USE( pSelf, ZH_EA_PUSH_PCODE );
            ZH_GEN_FUNC1( PCode1, ZH_P_POP );
         }
         break;

      case ZH_EA_STATEMENT:
         ZH_COMP_ERROR_SYNTAX( pSelf );
         break;

      case ZH_EA_DELETE:
         zh_compExprDelOperator( pSelf, ZH_COMP_PARAM );
         break;
   }
   return pSelf;
}

static ZH_EXPR_FUNC( zh_compExprUseIN )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_REDUCE );
         pSelf->value.asOperator.pRight = ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_REDUCE );
         pSelf = zh_compExprReduceIN( pSelf, ZH_COMP_PARAM );
         break;

      case ZH_EA_ARRAY_AT:
         ZH_COMP_ERROR_TYPE( pSelf );
         break;

      case ZH_EA_ARRAY_INDEX:
         break;

      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_PUSH_PCODE:
         ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_PCODE );
         ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_PCODE );
         ZH_GEN_FUNC1( PCode1, ZH_P_INSTRING );
         break;

      case ZH_EA_POP_PCODE:
         break;

      case ZH_EA_PUSH_POP:
         if( ZH_SUPPORT_ZIHER )
         {
            /* NOTE: This will not generate a runtime error if incompatible
             * data type is used
             */
            ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_POP );
            ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_POP );
         }
         else
         {
            ZH_EXPR_USE( pSelf, ZH_EA_PUSH_PCODE );
            ZH_GEN_FUNC1( PCode1, ZH_P_POP );
         }
         break;

      case ZH_EA_STATEMENT:
         ZH_COMP_ERROR_SYNTAX( pSelf );
         break;

      case ZH_EA_DELETE:
         zh_compExprDelOperator( pSelf, ZH_COMP_PARAM );
         break;
   }
   return pSelf;
}

static ZH_EXPR_FUNC( zh_compExprUsePlus )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_REDUCE );
         pSelf->value.asOperator.pRight = ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_REDUCE );
         pSelf = zh_compExprReducePlus( pSelf, ZH_COMP_PARAM );
         break;

      case ZH_EA_ARRAY_AT:
         ZH_COMP_ERROR_TYPE( pSelf );
         break;

      case ZH_EA_ARRAY_INDEX:
         break;

      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_PUSH_PCODE:
         if( ZH_SUPPORT_EXTOPT )
         {
            PZH_EXPR pLeft, pRight;
            pLeft  = pSelf->value.asOperator.pLeft;
            pRight = pSelf->value.asOperator.pRight;
            if( pLeft->ExprType == ZH_ET_NUMERIC )
            {
               if( pLeft->value.asNum.NumType == ZH_ET_LONG ?
                   pLeft->value.asNum.val.l == 1 :
                   pLeft->value.asNum.val.d == 1 )
               {
                  ZH_EXPR_USE( pRight, ZH_EA_PUSH_PCODE );
                  ZH_GEN_FUNC1( PCode1, ZH_P_INC );
                  break;
               }
               else if( pLeft->value.asNum.NumType == ZH_ET_LONG ?
                        pLeft->value.asNum.val.l == -1 :
                        pLeft->value.asNum.val.d == -1 )
               {
                  ZH_EXPR_USE( pRight, ZH_EA_PUSH_PCODE );
                  ZH_GEN_FUNC1( PCode1, ZH_P_DEC );
                  break;
               }
            }
            else if( pRight->ExprType == ZH_ET_NUMERIC )
            {
               if( pRight->value.asNum.NumType == ZH_ET_LONG ?
                   pRight->value.asNum.val.l == 1 :
                   pRight->value.asNum.val.d == 1 )
               {
                  ZH_EXPR_USE( pLeft, ZH_EA_PUSH_PCODE );
                  ZH_GEN_FUNC1( PCode1, ZH_P_INC );
                  break;
               }
               else if( pRight->value.asNum.NumType == ZH_ET_LONG ?
                        pRight->value.asNum.val.l == -1 :
                        pRight->value.asNum.val.d == -1 )
               {
                  ZH_EXPR_USE( pLeft, ZH_EA_PUSH_PCODE );
                  ZH_GEN_FUNC1( PCode1, ZH_P_DEC );
                  break;
               }
            }
         }
         ZH_EXPR_USE( pSelf->value.asOperator.pLeft,  ZH_EA_PUSH_PCODE );
         ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_PCODE );
         ZH_GEN_FUNC1( PCode1, ZH_P_PLUS );
         break;

      case ZH_EA_POP_PCODE:
         break;

      case ZH_EA_PUSH_POP:
         if( ZH_SUPPORT_ZIHER )
         {
            /* NOTE: This will not generate a runtime error if incompatible
             * data type is used
             */
            ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_POP );
            ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_POP );
         }
         else
         {
            ZH_EXPR_USE( pSelf, ZH_EA_PUSH_PCODE );
            ZH_GEN_FUNC1( PCode1, ZH_P_POP );
         }
         break;

      case ZH_EA_STATEMENT:
         ZH_COMP_ERROR_SYNTAX( pSelf );
         break;

      case ZH_EA_DELETE:
         zh_compExprDelOperator( pSelf, ZH_COMP_PARAM );
         break;
   }
   return pSelf;
}

static ZH_EXPR_FUNC( zh_compExprUseMinus )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_REDUCE );
         pSelf->value.asOperator.pRight = ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_REDUCE );
         pSelf = zh_compExprReduceMinus( pSelf, ZH_COMP_PARAM );
         break;

      case ZH_EA_ARRAY_AT:
         ZH_COMP_ERROR_TYPE( pSelf );
         break;

      case ZH_EA_ARRAY_INDEX:
         break;

      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_PUSH_PCODE:
         if( ZH_SUPPORT_EXTOPT )
         {
            PZH_EXPR pRight = pSelf->value.asOperator.pRight;
            if( pRight->ExprType == ZH_ET_NUMERIC )
            {
               if( pRight->value.asNum.NumType == ZH_ET_LONG ?
                   pRight->value.asNum.val.l == 1 :
                   pRight->value.asNum.val.d == 1 )
               {
                  ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_PCODE );
                  ZH_GEN_FUNC1( PCode1, ZH_P_DEC );
                  break;
               }
               else if( pRight->value.asNum.NumType == ZH_ET_LONG ?
                        pRight->value.asNum.val.l == -1 :
                        pRight->value.asNum.val.d == -1 )
               {
                  ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_PCODE );
                  ZH_GEN_FUNC1( PCode1, ZH_P_INC );
                  break;
               }
            }
         }
         ZH_EXPR_USE( pSelf->value.asOperator.pLeft,  ZH_EA_PUSH_PCODE );
         ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_PCODE );
         ZH_GEN_FUNC1( PCode1, ZH_P_MINUS );
         break;

      case ZH_EA_POP_PCODE:
         break;

      case ZH_EA_PUSH_POP:
         if( ZH_SUPPORT_ZIHER )
         {
            /* NOTE: This will not generate a runtime error if incompatible
             * data type is used
             */
            ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_POP );
            ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_POP );
         }
         else
         {
            ZH_EXPR_USE( pSelf, ZH_EA_PUSH_PCODE );
            ZH_GEN_FUNC1( PCode1, ZH_P_POP );
         }
         break;

      case ZH_EA_STATEMENT:
         ZH_COMP_ERROR_SYNTAX( pSelf );
         break;

      case ZH_EA_DELETE:
         zh_compExprDelOperator( pSelf, ZH_COMP_PARAM );
         break;
   }
   return pSelf;
}

static ZH_EXPR_FUNC( zh_compExprUseMult )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_REDUCE );
         pSelf->value.asOperator.pRight = ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_REDUCE );
         pSelf = zh_compExprReduceMult( pSelf, ZH_COMP_PARAM );
         break;

      case ZH_EA_ARRAY_AT:
         ZH_COMP_ERROR_TYPE( pSelf );
         break;

      case ZH_EA_ARRAY_INDEX:
         break;

      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_PUSH_PCODE:
         ZH_EXPR_USE( pSelf->value.asOperator.pLeft,  ZH_EA_PUSH_PCODE );
         ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_PCODE );
         ZH_GEN_FUNC1( PCode1, ZH_P_MULT );
         break;

      case ZH_EA_POP_PCODE:
         break;

      case ZH_EA_PUSH_POP:
         if( ZH_SUPPORT_ZIHER )
         {
            /* NOTE: This will not generate a runtime error if incompatible
             * data type is used
             */
            ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_POP );
            ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_POP );
         }
         else
         {
            ZH_EXPR_USE( pSelf, ZH_EA_PUSH_PCODE );
            ZH_GEN_FUNC1( PCode1, ZH_P_POP );
         }
         break;

      case ZH_EA_STATEMENT:
         ZH_COMP_ERROR_SYNTAX( pSelf );
         break;

      case ZH_EA_DELETE:
         zh_compExprDelOperator( pSelf, ZH_COMP_PARAM );
         break;
   }
   return pSelf;
}

static ZH_EXPR_FUNC( zh_compExprUseDiv )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_REDUCE );
         pSelf->value.asOperator.pRight = ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_REDUCE );
         pSelf = zh_compExprReduceDiv( pSelf, ZH_COMP_PARAM );
         break;

      case ZH_EA_ARRAY_AT:
         ZH_COMP_ERROR_TYPE( pSelf );
         break;

      case ZH_EA_ARRAY_INDEX:
         break;

      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_PUSH_PCODE:
         ZH_EXPR_USE( pSelf->value.asOperator.pLeft,  ZH_EA_PUSH_PCODE );
         ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_PCODE );
         ZH_GEN_FUNC1( PCode1, ZH_P_DIVIDE );
         break;

      case ZH_EA_POP_PCODE:
         break;

      case ZH_EA_PUSH_POP:
         if( ZH_SUPPORT_ZIHER )
         {
            /* NOTE: This will not generate a runtime error if incompatible
             * data type is used
             */
            ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_POP );
            ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_POP );
         }
         else
         {
            ZH_EXPR_USE( pSelf, ZH_EA_PUSH_PCODE );
            ZH_GEN_FUNC1( PCode1, ZH_P_POP );
         }
         break;

      case ZH_EA_STATEMENT:
         ZH_COMP_ERROR_SYNTAX( pSelf );
         break;

      case ZH_EA_DELETE:
         zh_compExprDelOperator( pSelf, ZH_COMP_PARAM );
         break;
   }
   return pSelf;
}

static ZH_EXPR_FUNC( zh_compExprUseMod )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_REDUCE );
         pSelf->value.asOperator.pRight = ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_REDUCE );
         pSelf = zh_compExprReduceMod( pSelf, ZH_COMP_PARAM );
         break;

      case ZH_EA_ARRAY_AT:
         ZH_COMP_ERROR_TYPE( pSelf );
         break;

      case ZH_EA_ARRAY_INDEX:
         break;

      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_PUSH_PCODE:
         ZH_EXPR_USE( pSelf->value.asOperator.pLeft,  ZH_EA_PUSH_PCODE );
         ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_PCODE );
         ZH_GEN_FUNC1( PCode1, ZH_P_MODULUS );
         break;

      case ZH_EA_POP_PCODE:
         break;

      case ZH_EA_PUSH_POP:
         if( ZH_SUPPORT_ZIHER )
         {
            /* NOTE: This will not generate a runtime error if incompatible
             * data type is used
             */
            ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_POP );
            ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_POP );
         }
         else
         {
            ZH_EXPR_USE( pSelf, ZH_EA_PUSH_PCODE );
            ZH_GEN_FUNC1( PCode1, ZH_P_POP );
         }
         break;

      case ZH_EA_STATEMENT:
         ZH_COMP_ERROR_SYNTAX( pSelf );
         break;

      case ZH_EA_DELETE:
         zh_compExprDelOperator( pSelf, ZH_COMP_PARAM );
         break;
   }
   return pSelf;
}

static ZH_EXPR_FUNC( zh_compExprUsePower )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_REDUCE );
         pSelf->value.asOperator.pRight = ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_REDUCE );
         if( ZH_SUPPORT_ZIHER )
            pSelf = zh_compExprReducePower( pSelf, ZH_COMP_PARAM );
         break;

      case ZH_EA_ARRAY_AT:
         ZH_COMP_ERROR_TYPE( pSelf );
         break;

      case ZH_EA_ARRAY_INDEX:
         break;

      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_PUSH_PCODE:
         ZH_EXPR_USE( pSelf->value.asOperator.pLeft,  ZH_EA_PUSH_PCODE );
         ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_PCODE );
         ZH_GEN_FUNC1( PCode1, ZH_P_POWER );
         break;

      case ZH_EA_POP_PCODE:
         break;

      case ZH_EA_PUSH_POP:
         if( ZH_SUPPORT_ZIHER )
         {
            /* NOTE: This will not generate a runtime error if incompatible
             * data type is used
             */
            ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_POP );
            ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_POP );
         }
         else
         {
            ZH_EXPR_USE( pSelf, ZH_EA_PUSH_PCODE );
            ZH_GEN_FUNC1( PCode1, ZH_P_POP );
         }
         break;

      case ZH_EA_STATEMENT:
         ZH_COMP_ERROR_SYNTAX( pSelf );
         break;

      case ZH_EA_DELETE:
         zh_compExprDelOperator( pSelf, ZH_COMP_PARAM );
         break;
   }
   return pSelf;
}

static ZH_EXPR_FUNC( zh_compExprUseNegate )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         pSelf->value.asOperator.pLeft = ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_REDUCE );
         pSelf = zh_compExprReduceNegate( pSelf, ZH_COMP_PARAM );
         break;

      case ZH_EA_ARRAY_AT:
         ZH_COMP_ERROR_TYPE( pSelf );
         break;

      case ZH_EA_ARRAY_INDEX:
         break;

      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_PUSH_PCODE:
         ZH_EXPR_USE( pSelf->value.asOperator.pLeft,  ZH_EA_PUSH_PCODE );
         ZH_GEN_FUNC1( PCode1, ZH_P_NEGATE );
         break;

      case ZH_EA_POP_PCODE:
         break;

      case ZH_EA_PUSH_POP:
         if( ZH_SUPPORT_ZIHER )
         {
            /* NOTE: This will not generate a runtime error if incompatible
             * data type is used
             */
            ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_POP );
         }
         else
         {
            ZH_EXPR_USE( pSelf, ZH_EA_PUSH_PCODE );
            ZH_GEN_FUNC1( PCode1, ZH_P_POP );
         }
         break;

      case ZH_EA_STATEMENT:
         ZH_COMP_ERROR_SYNTAX( pSelf );
         break;

      case ZH_EA_DELETE:
         if( pSelf->value.asOperator.pLeft )
            ZH_COMP_EXPR_FREE( pSelf->value.asOperator.pLeft );
         break;
   }
   return pSelf;
}

static ZH_EXPR_FUNC( zh_compExprUsePreInc )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         pSelf->value.asOperator.pLeft = ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_REDUCE );
#if ! defined( ZH_MACRO_SUPPORT )
         if( ! ZH_SUPPORT_ZIHER )
            pSelf->value.asOperator.pLeft = zh_compExprListStrip( pSelf->value.asOperator.pLeft, ZH_COMP_PARAM );
#endif
         ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_LVALUE );
         break;

      case ZH_EA_ARRAY_AT:
         ZH_COMP_ERROR_TYPE( pSelf );
         break;

      case ZH_EA_ARRAY_INDEX:
         break;

      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_PUSH_PCODE:
         zh_compExprPushPreOp( pSelf, ZH_P_INC, ZH_COMP_PARAM );
         break;

      case ZH_EA_POP_PCODE:
         break;

      case ZH_EA_PUSH_POP:
      case ZH_EA_STATEMENT:
         zh_compExprUsePreOp( pSelf, ZH_P_INC, ZH_COMP_PARAM );
         break;

      case ZH_EA_DELETE:
         if( pSelf->value.asOperator.pLeft )
            ZH_COMP_EXPR_FREE( pSelf->value.asOperator.pLeft );
         break;
   }
   return pSelf;
}

static ZH_EXPR_FUNC( zh_compExprUsePreDec )
{
   switch( iMessage )
   {
      case ZH_EA_REDUCE:
         pSelf->value.asOperator.pLeft = ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_REDUCE );
#if ! defined( ZH_MACRO_SUPPORT )
         if( ! ZH_SUPPORT_ZIHER )
            pSelf->value.asOperator.pLeft = zh_compExprListStrip( pSelf->value.asOperator.pLeft, ZH_COMP_PARAM );
#endif
         ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_LVALUE );
         break;

      case ZH_EA_ARRAY_AT:
         ZH_COMP_ERROR_TYPE( pSelf );
         break;

      case ZH_EA_ARRAY_INDEX:
         break;

      case ZH_EA_LVALUE:
         zh_compErrorLValue( ZH_COMP_PARAM, pSelf );
         break;

      case ZH_EA_PUSH_PCODE:
         zh_compExprPushPreOp( pSelf, ZH_P_DEC, ZH_COMP_PARAM );
         break;

      case ZH_EA_POP_PCODE:
         break;

      case ZH_EA_PUSH_POP:
      case ZH_EA_STATEMENT:
         zh_compExprUsePreOp( pSelf, ZH_P_DEC, ZH_COMP_PARAM );
         break;

      case ZH_EA_DELETE:
         if( pSelf->value.asOperator.pLeft )
            ZH_COMP_EXPR_FREE( pSelf->value.asOperator.pLeft );
         break;
   }
   return pSelf;
}

/* ************************************************************************* */

/* This generates a push pcode for a codeblock (with no macro expression or
   with late evaluation of a macro)
 */
#if defined( ZH_MACRO_SUPPORT )
static void zh_compExprCodeblockPush( PZH_EXPR pSelf, ZH_COMP_DECL )
#else
static ZH_BOOL zh_compExprCodeblockPush( PZH_EXPR pSelf, int iEarlyEvalPass, ZH_COMP_DECL )
#endif
{
   PZH_EXPR pExpr, pNext;
   PZH_EXPR * pPrev;

   /* Define requested local variables
    */
#if defined( ZH_MACRO_SUPPORT )
   zh_macroCodeBlockStart( ZH_COMP_PARAM );
   ZH_PCODE_DATA->pLocals = pSelf->value.asCodeblock.pLocals;
   ZH_PCODE_DATA->fVParams =
                  ( pSelf->value.asCodeblock.flags & ZH_BLOCK_VPARAMS ) != 0;
#else
   zh_compCodeBlockStart( ZH_COMP_PARAM, iEarlyEvalPass );
   ZH_COMP_PARAM->functions.pLast->fVParams =
                  ( pSelf->value.asCodeblock.flags & ZH_BLOCK_VPARAMS ) != 0;

   {
      PZH_CBVAR pVar;

      ZH_COMP_PARAM->iVarScope = ZH_VSCOMP_PARAMETER;
      pVar = pSelf->value.asCodeblock.pLocals;
      while( pVar )
      {
         zh_compVariableAdd( ZH_COMP_PARAM, pVar->szName, zh_compVarTypeNew( ZH_COMP_PARAM, pVar->bType, NULL ) );
         pVar = pVar->pNext;
      }
   }

   zh_compLinePushIfDebugger( ZH_COMP_PARAM );
#endif

   pExpr = pSelf->value.asCodeblock.pExprList;
   pPrev = &pSelf->value.asCodeblock.pExprList;
   while( pExpr )
   {
      if( pExpr->ExprType == ZH_ET_MACRO &&
          ( pExpr->value.asMacro.SubType & ZH_ET_MACRO_NOPARE ) == 0 )
      {
         pExpr->value.asMacro.SubType |= ZH_ET_MACRO_PARE;
      }

      /* store next expression in case the current will be reduced
       * NOTE: During reduction the expression can be replaced by the
       *       new one - this will break the linked list of expressions.
       */
      pNext = pExpr->pNext; /* store next expression in case the current will be reduced */
      if( ( pSelf->value.asCodeblock.flags & ZH_BLOCK_REDUCE ) != 0 ||
          ZH_SUPPORT_ZIHER )
      {
         *pPrev = pExpr = ZH_EXPR_USE( pExpr, ZH_EA_REDUCE );
         pExpr->pNext = pNext;  /* restore the link to next expression */
      }

      /* Generate push/pop pcodes for all expressions except the last one
       * The value of the last expression is used as a return value
       * of a codeblock evaluation
       */
      /* NOTE: This will generate warnings if constant value is
       * used as an expression - some operators will generate it too
       * e.g.
       * Eval( {|| 3+5, func()} )
       */
#if defined( ZH_MACRO_SUPPORT )
      if( pNext )
         ZH_EXPR_USE( pExpr, ZH_EA_PUSH_POP );
      else
         ZH_EXPR_USE( pExpr, ZH_EA_PUSH_PCODE );
#else
      if( pNext && ( iEarlyEvalPass == 0 || ZH_SUPPORT_MACRODECL ) )
         ZH_EXPR_USE( pExpr, ZH_EA_PUSH_POP );
      else
         ZH_EXPR_USE( pExpr, ZH_EA_PUSH_PCODE );
#endif
      pPrev = &pExpr->pNext;
      pExpr = pNext;
   }
#if defined( ZH_MACRO_SUPPORT )
   zh_macroCodeBlockEnd( ZH_COMP_PARAM );
#else
   if( ZH_COMP_PARAM->functions.pLast->iEarlyEvalPass == 0 )
   {
      zh_compCodeBlockEnd( ZH_COMP_PARAM );
      return ZH_TRUE;
   }
   else
   {
      zh_compCodeBlockRewind( ZH_COMP_PARAM );
      return ZH_FALSE;
   }
#endif
}

/* This generates a push pcode for early evaluation of a macro
 */
#if ! defined( ZH_MACRO_SUPPORT )
static void zh_compExprCodeblockExtPush( PZH_EXPR pSelf, ZH_COMP_DECL )
{
   zh_compGenPCodeN( ( ZH_BYTE * ) pSelf->value.asCodeblock.string,
                     pSelf->nLength, ZH_COMP_PARAM );
}

static void zh_compExprCodeblockEarly( PZH_EXPR pSelf, ZH_COMP_DECL )
{
   PZH_EXPR pExpr;

   /* check first expression */
   pExpr = pSelf->value.asCodeblock.pExprList;
   if( pExpr->ExprType == ZH_ET_MACRO && pExpr->value.asMacro.cMacroOp &&
       pExpr->pNext == NULL )
   {
      /* simple macro variable expansion: &variable
       * 'szMacro' is a variable name
       * {|| &variable} => &( '{||' + variable +'}' )
       */
      PZH_EXPR pVar, pNew;

      pVar = zh_compExprNewVar( pExpr->value.asMacro.szMacro, ZH_COMP_PARAM );
      pNew = zh_compExprNewString( "{||", 3, ZH_FALSE, ZH_COMP_PARAM );
      pNew = zh_compExprSetOperand( zh_compExprNewPlus( pNew, ZH_COMP_PARAM ), pVar, ZH_COMP_PARAM );
      pNew = zh_compExprSetOperand( zh_compExprNewPlus( pNew, ZH_COMP_PARAM ), zh_compExprNewString( "}", 1, ZH_FALSE, ZH_COMP_PARAM ), ZH_COMP_PARAM );
      pNew = zh_compExprNewMacro( pNew, 0, NULL, ZH_COMP_PARAM );
      ZH_EXPR_USE( pNew, ZH_EA_PUSH_PCODE );
      ZH_COMP_EXPR_FREE( pNew );
   }
   else
   {
      /* generate code to check if macro-expression refers to local, static
       * or field variables and generate error in such case or disable
       * iEarlyEvalPass when -kd (MACRODECL) switch is used.
       * In the 2nd case zh_compExprCodeblockPush() returns true and generated
       * code is accepted otherwise discarded and we have to generate macro
       * codeblock compiled at runtime.
       */
      if( ! zh_compExprCodeblockPush( pSelf, 1, ZH_COMP_PARAM ) )
      {
         /* -kd is not necessary, everything else is macro compiled at runtime
          * {|| &variable+1} => &( '{|| &variable+1}' )
          */
         ZH_BOOL fMacroText = ( ZH_COMP_PARAM->supported & ZH_COMPFLAG_MACROTEXT ) != 0;
         ZH_COMP_PARAM->supported |= ZH_COMPFLAG_MACROTEXT;
         ZH_COMP_PARAM->functions.pLast->iEarlyEvalPass = 2;
         pExpr = zh_compExprNewMacro( zh_compExprNewString( pSelf->value.asCodeblock.string, pSelf->nLength, ZH_FALSE, ZH_COMP_PARAM ), 0, NULL, ZH_COMP_PARAM );
         ZH_EXPR_USE( pExpr, ZH_EA_PUSH_PCODE );
         ZH_COMP_EXPR_FREE( pExpr );
         zh_compCodeBlockStop( ZH_COMP_PARAM );
         if( ! fMacroText )
            ZH_COMP_PARAM->supported &= ~ZH_COMPFLAG_MACROTEXT;
      }
   }
}
#endif      /*ZH_MACRO_SUPPORT*/


static void zh_compExprPushSendPop( PZH_EXPR pSelf, ZH_COMP_DECL )
{
   if( pSelf->value.asMessage.pObject )
   {
      /* Push _message */
      if( pSelf->value.asMessage.szMessage )
      {
         ZH_GEN_FUNC2( MessageData, pSelf->value.asMessage.szMessage, ZH_TRUE );
      }
      else
      {
         if( pSelf->value.asMessage.pMessage->ExprType == ZH_ET_MACRO )
            /* o:&macro := value
             * set ASSIGN flag in macro expression
             * it's cleared just after use
             */
            pSelf->value.asMessage.pMessage->value.asMacro.SubType |= ZH_ET_MACRO_ASSIGN;

         ZH_EXPR_USE( pSelf->value.asMessage.pMessage, ZH_EA_PUSH_PCODE );
      }
      /* Push object */
      ZH_EXPR_USE( pSelf->value.asMessage.pObject, ZH_EA_PUSH_PCODE );
   }
   else /* WITH OBJECT */
   {
      /* Push _message and object */
      if( pSelf->value.asMessage.szMessage )
      {
         ZH_GEN_FUNC2( MessageData, pSelf->value.asMessage.szMessage, ZH_FALSE );
      }
      else
      {
         if( pSelf->value.asMessage.pMessage->ExprType == ZH_ET_MACRO )
            /* o:&macro := value
             * set ASSIGN flag in macro expression
             * it's cleared just after use
             */
            pSelf->value.asMessage.pMessage->value.asMacro.SubType |= ZH_ET_MACRO_ASSIGN;

         ZH_EXPR_USE( pSelf->value.asMessage.pMessage, ZH_EA_PUSH_PCODE );
         /* Push object using WITHOBJECTMESSAGE pcode */
         ZH_GEN_FUNC2( Message, NULL, ZH_FALSE );
      }
   }
}

static void zh_compExprPushSendPush( PZH_EXPR pSelf, ZH_COMP_DECL )
{
   if( pSelf->value.asMessage.pObject )
   {
      /* Push message */
      if( pSelf->value.asMessage.szMessage )
      {
         ZH_GEN_FUNC2( Message, pSelf->value.asMessage.szMessage, ZH_TRUE );
      }
      else
      {
         ZH_EXPR_USE( pSelf->value.asMessage.pMessage, ZH_EA_PUSH_PCODE );
      }
      /* Push object */
      ZH_EXPR_USE( pSelf->value.asMessage.pObject, ZH_EA_PUSH_PCODE );
   }
   else /* WITH OBJECT */
   {
      if( pSelf->value.asMessage.szMessage )
      {
         /* Push message and object */
         ZH_GEN_FUNC2( Message, pSelf->value.asMessage.szMessage, ZH_FALSE );
      }
      else
      {
         /* Push message */
         ZH_EXPR_USE( pSelf->value.asMessage.pMessage, ZH_EA_PUSH_PCODE );
         /* Push object using WITHOBJECTMESSAGE pcode */
         ZH_GEN_FUNC2( Message, NULL, ZH_FALSE );
      }
   }
}

static void zh_compExprPushSendPopPush( PZH_EXPR pObj, PZH_EXPR pValue,
                                        ZH_BOOL fPreOp, ZH_BYTE bOper, ZH_COMP_DECL )
{
   if( ZH_SUPPORT_ZIHER )
   {
      zh_compExprPushSendPop( pObj, ZH_COMP_PARAM );
      /* duplicate object variable */
      ZH_GEN_FUNC1( PCode1, ZH_P_DUPLICATE );
      /* Push message */
      if( pObj->value.asMessage.szMessage )
      {
         /* ZH_TRUE used intentionally not to push object variable in WITH OBJECT */
         ZH_GEN_FUNC2( Message, pObj->value.asMessage.szMessage, ZH_TRUE );
      }
      else
      {
         ZH_EXPR_USE( pObj->value.asMessage.pMessage, ZH_EA_PUSH_PCODE );
      }
      ZH_GEN_FUNC2( PCode2, ZH_P_SWAP, 0 );
      ZH_GEN_FUNC2( PCode2, ZH_P_SENDSHORT, 0 );
      if( fPreOp )
      {
         /* push the result on the stack */
         ZH_GEN_FUNC1( PCode1, ZH_P_DUPLICATE );
         ZH_GEN_FUNC2( PCode2, ZH_P_SWAP, 2 );
      }
   }
   else
   {
      if( fPreOp )
      {
         /* push current value - it will be a result of whole expression */
         ZH_EXPR_USE( pObj, ZH_EA_PUSH_PCODE );
      }
      zh_compExprPushSendPop( pObj, ZH_COMP_PARAM );
      zh_compExprPushSendPush( pObj, ZH_COMP_PARAM );
      ZH_GEN_FUNC2( PCode2, ZH_P_SENDSHORT, 0 );
   }
   /* push increment value */
   if( pValue )
   {
      ZH_EXPR_USE( pValue, ZH_EA_PUSH_PCODE );
   }
   /* do operation */
   ZH_GEN_FUNC1( PCode1, bOper );
   /* Now do the assignment - call pop message with one argument */
   ZH_GEN_FUNC2( PCode2, ZH_P_SENDSHORT, 1 );
   if( fPreOp )
   {
      /* pop the unneeded value left by assignment message from the stack */
      ZH_GEN_FUNC1( PCode1, ZH_P_POP );
   }
}

/* Generates pcodes for compound operators    += -= *= /= %= ^=
 *
 * pExpr is an expression created by zh_compExprNew<operator>Eq functions
 */


static void zh_compExprPushOperEq( PZH_EXPR pSelf, ZH_BYTE bOpEq, ZH_COMP_DECL )
{
   ZH_BYTE bNewOp;

   if( ZH_SUPPORT_ZIHER )
   {
      switch( bOpEq )
      {
         case ZH_P_PLUS:
            bNewOp = ZH_P_PLUSEQ;
            break;
         case ZH_P_MINUS:
            bNewOp = ZH_P_MINUSEQ;
            break;
         case ZH_P_MULT:
            bNewOp = ZH_P_MULTEQ;
            break;
         case ZH_P_DIVIDE:
            bNewOp = ZH_P_DIVEQ;
            break;
         case ZH_P_MODULUS:
            bNewOp = ZH_P_MODEQ;
            break;
         case ZH_P_POWER:
            bNewOp = ZH_P_EXPEQ;
            break;
         default:
            bNewOp = bOpEq;
            break;
      }
   }
   else
      bNewOp = bOpEq;

   /* NOTE: an object instance variable needs special handling
    */
   if( pSelf->value.asOperator.pLeft->ExprType == ZH_ET_SEND )
   {
      if( ZH_SUPPORT_EXTOPT && bOpEq != bNewOp )
      {
         zh_compExprPushSendPop( pSelf->value.asOperator.pLeft, ZH_COMP_PARAM );
         ZH_GEN_FUNC1( PCode1, ZH_P_PUSHOVARREF );
         /* push increment value */
         ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_PCODE );
         ZH_GEN_FUNC1( PCode1, bNewOp );
      }
      else
      {
         zh_compExprPushSendPopPush( pSelf->value.asOperator.pLeft,
                                     pSelf->value.asOperator.pRight,
                                     ZH_FALSE, bOpEq, ZH_COMP_PARAM );
      }
      return;
   }
   else if( bOpEq != bNewOp )
   {
      if( pSelf->value.asOperator.pLeft->ExprType == ZH_ET_MACRO )
      {
         ZH_USHORT usType = pSelf->value.asOperator.pLeft->value.asMacro.SubType;
         if( usType & ZH_ET_MACRO_VAR )
         {
            /* NOTE: direct type change */
            pSelf->value.asOperator.pLeft->value.asMacro.SubType |= ZH_ET_MACRO_REFER;
            ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_PCODE );
            ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_PCODE );
            ZH_GEN_FUNC1( PCode1, bNewOp );
            /* NOTE: restore original expression type */
            pSelf->value.asOperator.pLeft->value.asMacro.SubType = usType;
            return;
         }
      }
      /* NOTE: code for arrays is differ to correctly handle a[ i++ ]++ */
      else if( pSelf->value.asOperator.pLeft->ExprType == ZH_ET_ARRAYAT )
      {
         /* Note: change type to array reference */
         pSelf->value.asOperator.pLeft->value.asList.reference = ZH_TRUE;
         ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_PCODE );
         pSelf->value.asOperator.pLeft->value.asList.reference = ZH_FALSE;

         ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_PCODE );
         ZH_GEN_FUNC1( PCode1, bNewOp );
         return;
      }
      else if( pSelf->value.asOperator.pLeft->ExprType == ZH_ET_VARIABLE )
      {
#if defined( ZH_MACRO_SUPPORT )
         {
#else
         int iVar, iScope;

         zh_compVariableFind( ZH_COMP_PARAM, pSelf->value.asOperator.pLeft->value.asSymbol.name, &iVar, &iScope );
         if( iScope != ZH_VS_LOCAL_FIELD && iScope != ZH_VS_GLOBAL_FIELD &&
             iScope != ZH_VS_UNDECLARED )
         {
            if( iScope == ZH_VS_LOCAL_VAR &&
                pSelf->value.asOperator.pRight->ExprType == ZH_ET_NUMERIC &&
                ( bOpEq == ZH_P_PLUS || bOpEq == ZH_P_MINUS ) )
            {
               if( zh_compExprIsInteger( pSelf->value.asOperator.pRight ) )
               {
                  short iIncrement = ( short ) pSelf->value.asOperator.pRight->value.asNum.val.l;

                  if( bOpEq != ZH_P_MINUS || iIncrement >= -INT16_MAX )
                  {
                     ZH_BYTE buffer[ 5 ];

                     if( bOpEq == ZH_P_MINUS )
                        iIncrement = -iIncrement;

                     buffer[ 0 ] = ZH_P_LOCALADDINT;
                     buffer[ 1 ] = ZH_LOBYTE( iVar );
                     buffer[ 2 ] = ZH_HIBYTE( iVar );
                     buffer[ 3 ] = ZH_LOBYTE( iIncrement );
                     buffer[ 4 ] = ZH_HIBYTE( iIncrement );
                     ZH_GEN_FUNC2( PCodeN, buffer, 5 );

                     ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_PCODE );
                     return;
                  }
               }
            }
#endif
            /* NOTE: direct type change */
            pSelf->value.asOperator.pLeft->ExprType = ZH_ET_VARREF;

            ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_PCODE );
            ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_PCODE );
            ZH_GEN_FUNC1( PCode1, bNewOp );
            /* NOTE: restore original expression type */
            pSelf->value.asOperator.pLeft->ExprType = ZH_ET_VARIABLE;
            return;
         }
      }
   }
   /* push old value */
   ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_PCODE );
   /* push increment value */
   ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_PCODE );
   /* perform operation and duplicate the new value */
   ZH_GEN_FUNC1( PCode1, bOpEq );
   ZH_GEN_FUNC1( PCode1, ZH_P_DUPLICATE );
   /* pop the new value into variable and leave the copy on the stack */
   ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_POP_PCODE );
}

/* Generates pcodes for <operator>= syntax
 * used standalone as a statement (it cannot leave the value on the stack)
 */
static void zh_compExprUseOperEq( PZH_EXPR pSelf, ZH_BYTE bOpEq, ZH_COMP_DECL )
{
   ZH_BYTE bNewOp;

   if( ZH_SUPPORT_ZIHER )
   {
      switch( bOpEq )
      {
         case ZH_P_PLUS:
            bNewOp = ZH_P_PLUSEQPOP;
            break;
         case ZH_P_MINUS:
            bNewOp = ZH_P_MINUSEQPOP;
            break;
         case ZH_P_MULT:
            bNewOp = ZH_P_MULTEQPOP;
            break;
         case ZH_P_DIVIDE:
            bNewOp = ZH_P_DIVEQPOP;
            break;
         case ZH_P_MODULUS:
            bNewOp = ZH_P_MODEQPOP;
            break;
         case ZH_P_POWER:
            bNewOp = ZH_P_EXPEQPOP;
            break;
         default:
            bNewOp = bOpEq;
            break;
      }
   }
   else
      bNewOp = bOpEq;

   /* NOTE: an object instance variable needs special handling
    */
   if( pSelf->value.asOperator.pLeft->ExprType == ZH_ET_SEND )
   {
      if( ZH_SUPPORT_EXTOPT && bOpEq != bNewOp )
      {
         zh_compExprPushSendPop( pSelf->value.asOperator.pLeft, ZH_COMP_PARAM );
         ZH_GEN_FUNC1( PCode1, ZH_P_PUSHOVARREF );
         /* push increment value */
         ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_PCODE );
         ZH_GEN_FUNC1( PCode1, bNewOp );
      }
      else
      {
         zh_compExprPushSendPopPush( pSelf->value.asOperator.pLeft,
                                     pSelf->value.asOperator.pRight,
                                     ZH_FALSE, bOpEq, ZH_COMP_PARAM );
         /* pop the unneeded value from the stack */
         ZH_GEN_FUNC1( PCode1, ZH_P_POP );
      }
      return;
   }
   else if( bOpEq != bNewOp )
   {
      if( pSelf->value.asOperator.pLeft->ExprType == ZH_ET_MACRO )
      {
         ZH_USHORT usType = pSelf->value.asOperator.pLeft->value.asMacro.SubType;
         if( usType & ZH_ET_MACRO_VAR )
         {
            /* NOTE: direct type change */
            pSelf->value.asOperator.pLeft->value.asMacro.SubType |= ZH_ET_MACRO_REFER;
            ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_PCODE );
            ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_PCODE );
            ZH_GEN_FUNC1( PCode1, bNewOp );
            /* NOTE: restore original expression type */
            pSelf->value.asOperator.pLeft->value.asMacro.SubType = usType;
            return;
         }
      }
      /* NOTE: code for arrays is differ to correctly handle a[ i++ ]++ */
      else if( pSelf->value.asOperator.pLeft->ExprType == ZH_ET_ARRAYAT )
      {
         /* Note: change type to array reference */
         pSelf->value.asOperator.pLeft->value.asList.reference = ZH_TRUE;
         ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_PCODE );
         pSelf->value.asOperator.pLeft->value.asList.reference = ZH_FALSE;

         ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_PCODE );
         ZH_GEN_FUNC1( PCode1, bNewOp );
         return;
      }
      else if( pSelf->value.asOperator.pLeft->ExprType == ZH_ET_VARIABLE )
      {
#if defined( ZH_MACRO_SUPPORT )
         {
#else
         int iVar, iScope;

         zh_compVariableFind( ZH_COMP_PARAM, pSelf->value.asOperator.pLeft->value.asSymbol.name, &iVar, &iScope );
         if( iScope != ZH_VS_LOCAL_FIELD && iScope != ZH_VS_GLOBAL_FIELD &&
             iScope != ZH_VS_UNDECLARED )
         {
            if( iScope == ZH_VS_LOCAL_VAR &&
                pSelf->value.asOperator.pRight->ExprType == ZH_ET_NUMERIC &&
                ( bOpEq == ZH_P_PLUS || bOpEq == ZH_P_MINUS ) )
            {
               if( zh_compExprIsInteger( pSelf->value.asOperator.pRight ) )
               {
                  short iIncrement = ( short ) pSelf->value.asOperator.pRight->value.asNum.val.l;

                  if( bOpEq != ZH_P_MINUS || iIncrement >= -INT16_MAX )
                  {
                     ZH_BYTE buffer[ 5 ];

                     if( bOpEq == ZH_P_MINUS )
                        iIncrement = -iIncrement;

                     buffer[ 0 ] = ZH_P_LOCALADDINT;
                     buffer[ 1 ] = ZH_LOBYTE( iVar );
                     buffer[ 2 ] = ZH_HIBYTE( iVar );
                     buffer[ 3 ] = ZH_LOBYTE( iIncrement );
                     buffer[ 4 ] = ZH_HIBYTE( iIncrement );
                     ZH_GEN_FUNC2( PCodeN, buffer, 5 );
                     return;
                  }
               }
            }
#endif
            /* NOTE: direct type change */
            pSelf->value.asOperator.pLeft->ExprType = ZH_ET_VARREF;
            ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_PCODE );
            ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_PCODE );
            ZH_GEN_FUNC1( PCode1, bNewOp );
            /* NOTE: restore original expression type */
            pSelf->value.asOperator.pLeft->ExprType = ZH_ET_VARIABLE;
            return;
         }
      }
   }
   /* push old value */
   ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_PCODE );
   /* push increment value */
   ZH_EXPR_USE( pSelf->value.asOperator.pRight, ZH_EA_PUSH_PCODE );
   /* add */
   ZH_GEN_FUNC1( PCode1, bOpEq );
   /* pop the new value into variable and remove it from the stack */
   ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_POP_PCODE );
}

/* Generates the pcodes for pre- increment/decrement expressions
 */
static void zh_compExprPushPreOp( PZH_EXPR pSelf, ZH_BYTE bOper, ZH_COMP_DECL )
{
   /* NOTE: an object instance variable needs special handling
    */
   if( pSelf->value.asOperator.pLeft->ExprType == ZH_ET_SEND )
   {
      if( ZH_SUPPORT_EXTOPT )
      {
         zh_compExprPushSendPop( pSelf->value.asOperator.pLeft, ZH_COMP_PARAM );
         ZH_GEN_FUNC1( PCode1, ZH_P_PUSHOVARREF );
         /* increase/decrease operation, leave unreferenced value on stack */
         ZH_GEN_FUNC1( PCode1, ( ZH_BYTE ) ( ( bOper == ZH_P_INC ) ? ZH_P_INCEQ : ZH_P_DECEQ ) );
      }
      else
      {
         zh_compExprPushSendPopPush( pSelf->value.asOperator.pLeft, NULL,
                                     ZH_FALSE, bOper, ZH_COMP_PARAM );
      }
      return;
   }
   else if( ZH_SUPPORT_ZIHER )
   {
      if( pSelf->value.asOperator.pLeft->ExprType == ZH_ET_MACRO &&
          pSelf->value.asOperator.pLeft->value.asMacro.SubType & ZH_ET_MACRO_VAR )
      {
         ZH_USHORT usType = pSelf->value.asOperator.pLeft->value.asMacro.SubType;
         /* NOTE: direct type change */
         pSelf->value.asOperator.pLeft->value.asMacro.SubType |= ZH_ET_MACRO_REFER;
         ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_PCODE );
         /* NOTE: restore original expression type */
         pSelf->value.asOperator.pLeft->value.asMacro.SubType = usType;

         /* increase/decrease operation, leave unreferenced value on stack */
         ZH_GEN_FUNC1( PCode1, ( ZH_BYTE ) ( bOper == ZH_P_INC ? ZH_P_INCEQ : ZH_P_DECEQ ) );
         return;
      }
      /* NOTE: code for arrays is differ to correctly handle a[ i++ ]++ */
      else if( pSelf->value.asOperator.pLeft->ExprType == ZH_ET_ARRAYAT )
      {
         /* push reference to current value */
         /* Note: change type to array reference */
         pSelf->value.asOperator.pLeft->value.asList.reference = ZH_TRUE;
         ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_PCODE );
         pSelf->value.asOperator.pLeft->value.asList.reference = ZH_FALSE;

         /* increase/decrease operation, leave unreferenced value on stack */
         ZH_GEN_FUNC1( PCode1, ( ZH_BYTE ) ( bOper == ZH_P_INC ? ZH_P_INCEQ : ZH_P_DECEQ ) );
         return;
      }
#if ! defined( ZH_MACRO_SUPPORT )
      else if( pSelf->value.asOperator.pLeft->ExprType == ZH_ET_VARIABLE )
      {
         int iVar, iScope;

         zh_compVariableFind( ZH_COMP_PARAM, pSelf->value.asOperator.pLeft->value.asSymbol.name, &iVar, &iScope );
         if( iScope != ZH_VS_LOCAL_FIELD && iScope != ZH_VS_GLOBAL_FIELD &&
             iScope != ZH_VS_UNDECLARED )
         {
            if( iScope == ZH_VS_LOCAL_VAR )
            {
               if( bOper == ZH_P_INC )
               {
                  ZH_GEN_FUNC3( PCode3, ZH_P_LOCALINCPUSH, ZH_LOBYTE( iVar ), ZH_HIBYTE( iVar ) );
               }
               else
               {
                  ZH_GEN_FUNC3( PCode3, ZH_P_LOCALDEC, ZH_LOBYTE( iVar ), ZH_HIBYTE( iVar ) );
                  /* Push current value */
                  ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_PCODE );
               }
            }
            else
            {
               /* NOTE: direct type change */
               pSelf->value.asOperator.pLeft->ExprType = ZH_ET_VARREF;
               ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_PCODE );
               ZH_GEN_FUNC1( PCode1, ( ZH_BYTE ) ( bOper == ZH_P_INC ? ZH_P_INCEQ : ZH_P_DECEQ ) );
               /* NOTE: restore original expression type */
               pSelf->value.asOperator.pLeft->ExprType = ZH_ET_VARIABLE;
            }
            return;
         }
      }
#endif
   }

   /* Push current value */
   ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_PCODE );
   /* Increment */
   ZH_GEN_FUNC1( PCode1, bOper );
   /* duplicate a value */
   ZH_GEN_FUNC1( PCode1, ZH_P_DUPLICATE );
   /* pop new value and leave the duplicated copy of it on the stack */
   ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_POP_PCODE );
}

/* Generates the pcodes for post- increment/decrement expressions
 */
static void zh_compExprPushPostOp( PZH_EXPR pSelf, ZH_BYTE bOper, ZH_COMP_DECL )
{
   /* NOTE: an object instance variable needs special handling
    */
   if( pSelf->value.asOperator.pLeft->ExprType == ZH_ET_SEND )
   {
      if( ZH_SUPPORT_EXTOPT )
      {
         /* push reference to current value */
         zh_compExprPushSendPop( pSelf->value.asOperator.pLeft, ZH_COMP_PARAM );
         ZH_GEN_FUNC1( PCode1, ZH_P_PUSHOVARREF );
         /* Duplicate the reference and unref the original one -
          * it will be the result of whole expression
          */
         ZH_GEN_FUNC1( PCode1, ZH_P_DUPLUNREF );
         /* increment/decrement the value */
         ZH_GEN_FUNC1( PCode1, ( ZH_BYTE ) ( ( bOper == ZH_P_INC ) ? ZH_P_INCEQPOP : ZH_P_DECEQPOP ) );
      }
      else
      {
         zh_compExprPushSendPopPush( pSelf->value.asOperator.pLeft, NULL,
                                     ZH_TRUE, bOper, ZH_COMP_PARAM );
      }
      return;
   }
   else if( ZH_SUPPORT_ZIHER )
   {
      if( pSelf->value.asOperator.pLeft->ExprType == ZH_ET_MACRO &&
          pSelf->value.asOperator.pLeft->value.asMacro.SubType & ZH_ET_MACRO_VAR )
      {
         ZH_USHORT usType = pSelf->value.asOperator.pLeft->value.asMacro.SubType;
         /* NOTE: direct type change */
         pSelf->value.asOperator.pLeft->value.asMacro.SubType |= ZH_ET_MACRO_REFER;
         ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_PCODE );
         /* NOTE: restore original expression type */
         pSelf->value.asOperator.pLeft->value.asMacro.SubType = usType;

         /* Duplicate the reference and unref the original one -
          * it will be the result of whole expression
          */
         ZH_GEN_FUNC1( PCode1, ZH_P_DUPLUNREF );
         /* increase/decrease operation */
         ZH_GEN_FUNC1( PCode1, ( ZH_BYTE ) ( bOper == ZH_P_INC ? ZH_P_INCEQPOP : ZH_P_DECEQPOP ) );
         return;
      }
      /* NOTE: code for arrays is differ to correctly handle a[ i++ ]++ */
      else if( pSelf->value.asOperator.pLeft->ExprType == ZH_ET_ARRAYAT )
      {
         /* push reference to current value */
         /* Note: change type to array reference */
         pSelf->value.asOperator.pLeft->value.asList.reference = ZH_TRUE;
         ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_PCODE );
         pSelf->value.asOperator.pLeft->value.asList.reference = ZH_FALSE;

         /* Duplicate the reference and unref the original one -
          * it will be the result of whole expression
          */
         ZH_GEN_FUNC1( PCode1, ZH_P_DUPLUNREF );
         /* increase/decrease operation */
         ZH_GEN_FUNC1( PCode1, ( ZH_BYTE ) ( bOper == ZH_P_INC ? ZH_P_INCEQPOP : ZH_P_DECEQPOP ) );
         return;
      }
#if ! defined( ZH_MACRO_SUPPORT )
      else if( pSelf->value.asOperator.pLeft->ExprType == ZH_ET_VARIABLE )
      {
         int iVar, iScope;

         zh_compVariableFind( ZH_COMP_PARAM, pSelf->value.asOperator.pLeft->value.asSymbol.name, &iVar, &iScope );
         if( iScope != ZH_VS_LOCAL_FIELD && iScope != ZH_VS_GLOBAL_FIELD &&
             iScope != ZH_VS_UNDECLARED )
         {
            if( iScope == ZH_VS_LOCAL_VAR )
            {
               /* Push current value */
               ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_PCODE );
               ZH_GEN_FUNC3( PCode3, ( ZH_BYTE ) ( bOper == ZH_P_INC ? ZH_P_LOCALINC : ZH_P_LOCALDEC ),
                             ZH_LOBYTE( iVar ), ZH_HIBYTE( iVar ) );
            }
            else
            {
               /* NOTE: direct type change */
               pSelf->value.asOperator.pLeft->ExprType = ZH_ET_VARREF;
               ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_PCODE );
               ZH_GEN_FUNC1( PCode1, ZH_P_DUPLUNREF );
               ZH_GEN_FUNC1( PCode1, ( ZH_BYTE ) ( bOper == ZH_P_INC ? ZH_P_INCEQPOP : ZH_P_DECEQPOP ) );
               /* NOTE: restore original expression type */
               pSelf->value.asOperator.pLeft->ExprType = ZH_ET_VARIABLE;
            }
            return;
         }
      }
#endif
   }

   /* Push current value */
   ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_PCODE );
   /* Duplicate value */
   ZH_GEN_FUNC1( PCode1, ZH_P_DUPLICATE );
   /* Increment */
   ZH_GEN_FUNC1( PCode1, bOper );
   /* pop new value from the stack */
   ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_POP_PCODE );
}

/* Generates the pcodes for increment/decrement operations
 * used standalone as a statement
 */
static void zh_compExprUsePreOp( PZH_EXPR pSelf, ZH_BYTE bOper, ZH_COMP_DECL )
{
   /* NOTE: an object instance variable needs special handling
    */
   if( pSelf->value.asOperator.pLeft->ExprType == ZH_ET_SEND )
   {
      if( ZH_SUPPORT_EXTOPT )
      {
         /* push reference to current value */
         zh_compExprPushSendPop( pSelf->value.asOperator.pLeft, ZH_COMP_PARAM );
         ZH_GEN_FUNC1( PCode1, ZH_P_PUSHOVARREF );
         /* increment/decrement the value */
         ZH_GEN_FUNC1( PCode1, ( ZH_BYTE ) ( ( bOper == ZH_P_INC ) ? ZH_P_INCEQPOP : ZH_P_DECEQPOP ) );
      }
      else
      {
         zh_compExprPushSendPopPush( pSelf->value.asOperator.pLeft, NULL,
                                     ZH_FALSE, bOper, ZH_COMP_PARAM );
         /* pop the value from the stack */
         ZH_GEN_FUNC1( PCode1, ZH_P_POP );
      }
      return;
   }
   else if( ZH_SUPPORT_ZIHER )
   {
      if( pSelf->value.asOperator.pLeft->ExprType == ZH_ET_MACRO &&
          pSelf->value.asOperator.pLeft->value.asMacro.SubType & ZH_ET_MACRO_VAR )
      {
         ZH_USHORT usType = pSelf->value.asOperator.pLeft->value.asMacro.SubType;
         /* NOTE: direct type change */
         pSelf->value.asOperator.pLeft->value.asMacro.SubType |= ZH_ET_MACRO_REFER;
         ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_PCODE );
         /* NOTE: restore original expression type */
         pSelf->value.asOperator.pLeft->value.asMacro.SubType = usType;

         /* increase/decrease operation */
         ZH_GEN_FUNC1( PCode1, ( ZH_BYTE ) ( bOper == ZH_P_INC ? ZH_P_INCEQPOP : ZH_P_DECEQPOP ) );
         return;
      }
      /* NOTE: code for arrays is differ to correctly handle a[ i++ ]++ */
      else if( pSelf->value.asOperator.pLeft->ExprType == ZH_ET_ARRAYAT )
      {
         /* push reference to current value */
         /* Note: change type to array reference */
         pSelf->value.asOperator.pLeft->value.asList.reference = ZH_TRUE;
         ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_PCODE );
         pSelf->value.asOperator.pLeft->value.asList.reference = ZH_FALSE;
         /* increase/decrease operation */
         ZH_GEN_FUNC1( PCode1, ( ZH_BYTE ) ( bOper == ZH_P_INC ? ZH_P_INCEQPOP : ZH_P_DECEQPOP ) );
         return;
      }
#if ! defined( ZH_MACRO_SUPPORT )
      else if( pSelf->value.asOperator.pLeft->ExprType == ZH_ET_VARIABLE )
      {
         int iVar, iScope;

         zh_compVariableFind( ZH_COMP_PARAM, pSelf->value.asOperator.pLeft->value.asSymbol.name, &iVar, &iScope );
         if( iScope != ZH_VS_LOCAL_FIELD && iScope != ZH_VS_GLOBAL_FIELD &&
             iScope != ZH_VS_UNDECLARED )
         {
            if( iScope == ZH_VS_LOCAL_VAR )
            {
               ZH_GEN_FUNC3( PCode3, ( ZH_BYTE ) ( bOper == ZH_P_INC ? ZH_P_LOCALINC : ZH_P_LOCALDEC ),
                             ZH_LOBYTE( iVar ), ZH_HIBYTE( iVar ) );
            }
            else
            {
               /* NOTE: direct type change */
               pSelf->value.asOperator.pLeft->ExprType = ZH_ET_VARREF;
               ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_PCODE );
               ZH_GEN_FUNC1( PCode1, ( ZH_BYTE ) ( bOper == ZH_P_INC ? ZH_P_INCEQPOP : ZH_P_DECEQPOP ) );
               /* NOTE: restore original expression type */
               pSelf->value.asOperator.pLeft->ExprType = ZH_ET_VARIABLE;
            }
            return;
         }
      }
#endif
   }

   /* Push current value */
   ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_PUSH_PCODE );
   /* Increment */
   ZH_GEN_FUNC1( PCode1, bOper );
   /* pop new value from the stack */
   ZH_EXPR_USE( pSelf->value.asOperator.pLeft, ZH_EA_POP_PCODE );
}

/* Generate pcode for aliased expression which contains macro operator on
 * the left or right side of the alias operator
 * expression->&macro or &macro->expression or &macro->&macro
 */
static void zh_compExprUseAliasMacro( PZH_EXPR pAliasedVar, ZH_BYTE bAction, ZH_COMP_DECL )
{
   PZH_EXPR pAlias, pVar;

   /* Alias->Var
    */
   pAlias = pAliasedVar->value.asAlias.pAlias;
   pVar   = pAliasedVar->value.asAlias.pVar;
   if( pAlias->ExprType == ZH_ET_ALIAS )
   {
      /* database alias */
      /* Push alias identifier as string so it can be joined with
       * variable at runtime
       * NOTE:
       *    ALIAS->&var is the same as &( "ALIAS->" + var )
       *
       */
      ZH_GEN_FUNC2( PushString, pAlias->value.asSymbol.name, strlen( pAlias->value.asSymbol.name ) + 1 );
      ZH_EXPR_USE( pVar, ZH_EA_PUSH_PCODE );
   }
   else if( pVar->ExprType == ZH_ET_VARIABLE )
   {
      /* NOTE:
       *    &macro->var is the  same as: &( macro + "->var" )
       */
      ZH_EXPR_USE( pAlias, ZH_EA_PUSH_PCODE );
      ZH_GEN_FUNC2( PushString, pVar->value.asSymbol.name, strlen( pVar->value.asSymbol.name ) + 1 );
   }
   else
   {
      ZH_EXPR_USE( pAlias, ZH_EA_PUSH_PCODE );
      ZH_EXPR_USE( pVar, ZH_EA_PUSH_PCODE );
   }

   if( bAction == ZH_EA_PUSH_PCODE )
      ZH_GEN_FUNC1( PCode1, ZH_P_MACRO_PUSHALIASED );
   else
      ZH_GEN_FUNC1( PCode1, ZH_P_MACROPOPALIASED );

   /* Always add byte to pcode indicating requested macro compiler flag. */
   ZH_GEN_FUNC1( PCode1, ( ZH_BYTE ) ZH_MACRO_GENFLAGS );
}


/* Reduces the list of expressions
 *
 * pExpr is the first expression on the list
 */
static PZH_EXPR zh_compExprReduceList( PZH_EXPR pList, ZH_COMP_DECL )
{
   PZH_EXPR * pExpr;

   /* NOTE: During optimization an expression on the list can be
    * replaced by the new one
    */

   pExpr = &pList->value.asList.pExprList;
   while( *pExpr )
   {
      PZH_EXPR pNext = ( *pExpr )->pNext; /* store next expression in case the current will be reduced */
      *pExpr = ZH_EXPR_USE( *pExpr, ZH_EA_REDUCE );
      ( *pExpr )->pNext = pNext;             /* restore the link to next expression */
      pExpr = &( *pExpr )->pNext;
   }
   return pList;
}

/* reduce ( "alias" )-> to ALIAS->
 */
static PZH_EXPR zh_compExprReduceAliasString( PZH_EXPR pExpr, PZH_EXPR pAlias, ZH_COMP_DECL )
{
   const char * szAlias = pAlias->value.asString.string;

   if( ZH_ISFIRSTIDCHAR( *szAlias ) )
   {
      ZH_SIZE nLen = pAlias->nLength;
      if( nLen <= ZH_SYMBOL_NAME_LEN )
      {
         ZH_BOOL fLower = ZH_FALSE;
         while( nLen )
         {
            char c = szAlias[ nLen - 1 ];
            if( ! ZH_ISNEXTIDCHAR( c ) )
               break;
            if( ZH_ISLOWER( c ) )
               fLower = ZH_TRUE;
            --nLen;
         }
         if( nLen == 0 )
         {
#if defined( ZH_MACRO_SUPPORT )
            if( fLower )
               szAlias = zh_macroIdentNew( ZH_COMP_PARAM, zh_strupr( zh_strdup( szAlias ) ) );
            else if( pAlias->value.asString.dealloc )
               szAlias = zh_macroIdentNew( ZH_COMP_PARAM, zh_strdup( szAlias ) );
#else
            if( fLower )
               szAlias = zh_compIdentifierNew( ZH_COMP_PARAM, zh_strupr( zh_strdup( szAlias ) ), ZH_IDENT_FREE );
            else if( pAlias->value.asString.dealloc )
               szAlias = zh_compIdentifierNew( ZH_COMP_PARAM, szAlias, ZH_IDENT_COPY );
#endif
            ZH_COMP_EXPR_FREE( pExpr );
            pExpr = zh_compExprNewAlias( szAlias, ZH_COMP_PARAM );
         }
      }
   }
   return pExpr;
}

static ZH_BOOL zh_compExprIsMemvarAlias( const char * szAlias )
{
   int iLen = ( int ) strlen( szAlias );

   /* @M-> @MEMVAR-> or @MEMVA-> or @MEMV-> */
   return ( iLen == 1 || ( iLen >= 4 && iLen <= 6 ) ) &&
          memcmp( szAlias, "MEMVAR", iLen ) == 0;
}
