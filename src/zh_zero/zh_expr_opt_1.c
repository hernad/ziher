/*
 * Compiler Expression Optimizer - common expressions
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

/* NOTE: This must be the first definition
 *    This is a common code shared by macro and standalone compiler
 */
#define  ZH_COMMON_SUPPORT

#include "zh_macro.h"
#include "zh_comp.h"

static const char * s_OperTable[ ZH_EXPR_COUNT ] = {
   "",
   "NIL",
   "Numeric",
   "Date",
   "Timestamp",
   "String",
   "Codeblock",
   "Logical",
   "SELF",
   "Array",
   "Hash",
   "@func()",
   "@",
   "@",
   "IIF",
   ",",
   ",",
   ",",
   "[",
   "&",
   "()",
   "->",
   "->",
   "(:=)",     /* setget */
   ":",
   "",         /* symbol */
   "",         /* alias */
   "",         /* RunTime variable */
   "",         /* variable */
   "++",       /* post-operators -> lowest precedence */
   "--",
   ":=",       /* assignments */
   "+=",
   "-=",
   "*=",
   "/=",
   "%=",
   "^=",
   ".OR.",     /* logical operators */
   ".AND.",
   ".NOT.",
   "=",        /* relational operators */
   "==",
   "!=",
   "$",
   "<",
   ">",
   "<=",
   ">=",
   "+",        /* addition */
   "-",
   "*",        /* multiple */
   "/",
   "%",
   "^",
   "-",        /* sign operator */
   "++",
   "--"
};

/* Table with operators precedence
 * NOTE:
 *    ZH_ET_NIL is used for an ordinary values and post- operators
 *    ZH_ET_NONE is used for invalid syntax, e.g. var := var1 += 2
 */
static const ZH_BYTE s_PrecedTable[ ZH_EXPR_COUNT ] = {
   ZH_ET_NIL,                 /* ZH_ET_NONE = 0,    */
   ZH_ET_NIL,                 /* ZH_ET_NIL,         */
   ZH_ET_NIL,                 /* ZH_ET_NUMERIC,     */
   ZH_ET_NIL,                 /* ZH_ET_DATE,        */
   ZH_ET_NIL,                 /* ZH_ET_TIMESTAMP,   */
   ZH_ET_NIL,                 /* ZH_ET_STRING,      */
   ZH_ET_NIL,                 /* ZH_ET_CODEBLOCK,   */
   ZH_ET_NIL,                 /* ZH_ET_LOGICAL,     */
   ZH_ET_NIL,                 /* ZH_ET_SELF,        */
   ZH_ET_NIL,                 /* ZH_ET_ARRAY,       */
   ZH_ET_NIL,                 /* ZH_ET_HASH,        */
   ZH_ET_NIL,                 /* ZH_ET_FUNREF,      */
   ZH_ET_NIL,                 /* ZH_ET_VARREF,      */
   ZH_ET_NIL,                 /* ZH_ET_REFERENCE,   */
   ZH_ET_NIL,                 /* ZH_ET_IIF,         */
   ZH_ET_NIL,                 /* ZH_ET_LIST,        */
   ZH_ET_NIL,                 /* ZH_ET_ARGLIST,     */
   ZH_ET_NIL,                 /* ZH_ET_MACROARGLIST,*/
   ZH_ET_NIL,                 /* ZH_ET_ARRAYAT,     */
   ZH_ET_NIL,                 /* ZH_ET_MACRO,       */
   ZH_ET_NIL,                 /* ZH_ET_FUNCALL,     */
   ZH_ET_NIL,                 /* ZH_ET_ALIASVAR,    */
   ZH_ET_NIL,                 /* ZH_ET_ALIASEXPR,   */
   ZH_ET_NIL,                 /* ZH_ET_SETGET,      */
   ZH_ET_NIL,                 /* ZH_ET_SEND,        */
   ZH_ET_NIL,                 /* ZH_ET_FUNNAME,     */
   ZH_ET_NIL,                 /* ZH_ET_ALIAS,       */
   ZH_ET_NIL,                 /* ZH_ET_RTVARIABLE,  */
   ZH_ET_NIL,                 /* ZH_ET_VARIABLE,    */
   ZH_ET_NIL,                 /* ZH_EO_POSTINC,     post-operators */
   ZH_ET_NIL,                 /* ZH_EO_POSTDEC,     */
   ZH_ET_NONE,                /* ZH_EO_ASSIGN,      assignments */
   ZH_ET_NONE,                /* ZH_EO_PLUSEQ,      Invalid syntax */
   ZH_ET_NONE,                /* ZH_EO_MINUSEQ,     */
   ZH_ET_NONE,                /* ZH_EO_MULTEQ,      */
   ZH_ET_NONE,                /* ZH_EO_DIVEQ,       */
   ZH_ET_NONE,                /* ZH_EO_MODEQ,       */
   ZH_ET_NONE,                /* ZH_EO_EXPEQ,       */
   ZH_EO_OR,                  /* ZH_EO_OR,          logical operators */
   ZH_EO_AND,                 /* ZH_EO_AND,         */
   ZH_ET_NIL,                 /* ZH_EO_NOT,         */
   ZH_EO_EQUAL,               /* ZH_EO_EQUAL,       relational operators */
   ZH_EO_EQUAL,               /* ZH_EO_EQ,          */
   ZH_EO_EQUAL,               /* ZH_EO_NE,          */
   ZH_EO_LT,                  /* ZH_EO_IN,          */
   ZH_EO_LT,                  /* ZH_EO_LT,          */
   ZH_EO_LT,                  /* ZH_EO_GT,          */
   ZH_EO_LT,                  /* ZH_EO_LE,          */
   ZH_EO_LT,                  /* ZH_EO_GE,          */
   ZH_EO_PLUS,                /* ZH_EO_PLUS,        addition */
   ZH_EO_PLUS,                /* ZH_EO_MINUS,       */
   ZH_EO_MULT,                /* ZH_EO_MULT,        multiple */
   ZH_EO_MULT,                /* ZH_EO_DIV,         */
   ZH_EO_MULT,                /* ZH_EO_MOD,         */
   ZH_EO_POWER,               /* ZH_EO_POWER,       */
   ZH_ET_NIL,                 /* ZH_EO_NEGATE,      sign operator */
   ZH_ET_NIL,                 /* ZH_EO_PREINC,      */
   ZH_ET_NIL                  /* ZH_EO_PREDEC,      pre-operators */
};

/* ************************************************************************* */

const char * zh_compExprDescription( PZH_EXPR pExpr )
{
   if( pExpr )
      return s_OperTable[ pExpr->ExprType ];
   else
      return s_OperTable[ 0 ];
}

int zh_compExprType( PZH_EXPR pExpr )
{
   return ( int ) pExpr->ExprType;
}

int zh_compExprIsInteger( PZH_EXPR pExpr )
{
   return pExpr->ExprType == ZH_ET_NUMERIC && pExpr->value.asNum.NumType == ZH_ET_LONG &&
          ZH_LIM_INT16( pExpr->value.asNum.val.l );
}

int zh_compExprIsLong( PZH_EXPR pExpr )
{
   return pExpr->ExprType == ZH_ET_NUMERIC && pExpr->value.asNum.NumType == ZH_ET_LONG;
}

int zh_compExprIsString( PZH_EXPR pExpr )
{
   return pExpr->ExprType == ZH_ET_STRING;
}

const char * zh_compExprAsString( PZH_EXPR pExpr )
{
   if( pExpr->ExprType == ZH_ET_STRING )
      return pExpr->value.asString.string;
   return NULL;
}

ZH_SIZE zh_compExprAsStringLen( PZH_EXPR pExpr )
{
   if( pExpr->ExprType == ZH_ET_STRING )
      return pExpr->nLength;
   return 0;
}

int zh_compExprAsNumSign( PZH_EXPR pExpr )
{
   if( pExpr->ExprType == ZH_ET_NUMERIC )
   {
      if( pExpr->value.asNum.NumType == ZH_ET_DOUBLE )
      {
         if( pExpr->value.asNum.val.d > 0 )
            return 1;
         else if( pExpr->value.asNum.val.d < 0 )
            return -1;
      }
      else
      {
         if( pExpr->value.asNum.val.l > 0 )
            return 1;
         else if( pExpr->value.asNum.val.l < 0 )
            return -1;
      }
   }
   return 0;
}

int zh_compExprAsInteger( PZH_EXPR pExpr )
{
   if( pExpr->ExprType == ZH_ET_NUMERIC && pExpr->value.asNum.NumType == ZH_ET_LONG )
      return ( int ) pExpr->value.asNum.val.l;
   else
      return 0;
}

ZH_MAXINT zh_compExprAsLongNum( PZH_EXPR pExpr )
{
   if( pExpr->ExprType == ZH_ET_NUMERIC )
   {
      if( pExpr->value.asNum.NumType == ZH_ET_LONG )
         return pExpr->value.asNum.val.l;
      else
         return ( ZH_MAXINT ) pExpr->value.asNum.val.d;
   }
   else
      return 0;
}

const char * zh_compExprAsSymbol( PZH_EXPR pExpr )
{
   switch( pExpr->ExprType )
   {
      case ZH_ET_VARIABLE:
      case ZH_ET_VARREF:
      case ZH_ET_FUNNAME:
         return pExpr->value.asSymbol.name;

      case ZH_ET_FUNCALL:
         if( pExpr->value.asFunCall.pFunName->ExprType == ZH_ET_FUNNAME )
            return pExpr->value.asFunCall.pFunName->value.asSymbol.name;
   }
   return NULL;
}

/* ************************************************************************* */

PZH_EXPR zh_compExprNewEmpty( ZH_COMP_DECL )
{
   return ZH_COMP_EXPR_NEW( ZH_ET_NONE );
}

PZH_EXPR zh_compExprNewDouble( double dValue, ZH_BYTE ucWidth, ZH_BYTE ucDec,
                               ZH_COMP_DECL )
{
   PZH_EXPR pExpr;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprNewDouble(%f, %i, %p)", dValue, ucDec, ( void * ) ZH_COMP_PARAM ) );

   pExpr = ZH_COMP_EXPR_NEW( ZH_ET_NUMERIC );

   pExpr->value.asNum.val.d   = dValue;
   pExpr->value.asNum.bWidth  = ucWidth;
   pExpr->value.asNum.bDec    = ucDec;
   pExpr->value.asNum.NumType = ZH_ET_DOUBLE;
   pExpr->ValType = ZH_EV_NUMERIC;

   return pExpr;
}

PZH_EXPR zh_compExprNewLong( ZH_MAXINT nValue, ZH_COMP_DECL )
{
   PZH_EXPR pExpr;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprNewLong(%" PFHL "d, %p)", nValue, ( void * ) ZH_COMP_PARAM ) );

   pExpr = ZH_COMP_EXPR_NEW( ZH_ET_NUMERIC );

   pExpr->value.asNum.val.l   = nValue;
   pExpr->value.asNum.bWidth  = ZH_DEFAULT_WIDTH;
   pExpr->value.asNum.bDec    = 0;
   pExpr->value.asNum.NumType = ZH_ET_LONG;
   pExpr->ValType = ZH_EV_NUMERIC;

   return pExpr;
}

PZH_EXPR zh_compExprNewDate( long lDate, ZH_COMP_DECL )
{
   PZH_EXPR pExpr;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprNewDate(%ld, %p)", lDate, ( void * ) ZH_COMP_PARAM ) );

   pExpr = ZH_COMP_EXPR_NEW( ZH_ET_DATE );

   pExpr->value.asDate.lDate = lDate;
   pExpr->value.asDate.lTime = 0;
   pExpr->ValType = ZH_EV_DATE;

   return pExpr;
}

PZH_EXPR zh_compExprNewTimeStamp( long lDate, long lTime, ZH_COMP_DECL )
{
   PZH_EXPR pExpr;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprNewTimeStamp(%ld, %ld, %p)", lDate, lTime, ( void * ) ZH_COMP_PARAM ) );

   pExpr = ZH_COMP_EXPR_NEW( ZH_ET_TIMESTAMP );

   pExpr->value.asDate.lDate = lDate;
   pExpr->value.asDate.lTime = lTime;
   pExpr->ValType = ZH_EV_TIMESTAMP;

   return pExpr;
}

PZH_EXPR zh_compExprNewString( const char * szValue, ZH_SIZE nLen, ZH_BOOL fDealloc, ZH_COMP_DECL )
{
   PZH_EXPR pExpr;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprNewString(%s)", szValue ) );

   pExpr = ZH_COMP_EXPR_NEW( ZH_ET_STRING );

   pExpr->value.asString.string  = ( char * ) ZH_UNCONST( szValue );
   pExpr->value.asString.dealloc = fDealloc;
   pExpr->nLength = nLen;
   pExpr->ValType = ZH_EV_STRING;

   return pExpr;
}

/* Creates a new literal array { item1, item2, ... itemN }
 *    'pArrList' is a list of array elements
 */
PZH_EXPR zh_compExprNewArray( PZH_EXPR pArrList, ZH_COMP_DECL )
{
   PZH_EXPR pExpr;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprNewArray()" ) );

   pArrList->ExprType = ZH_ET_ARRAY;   /* change type from ET_LIST */
   pArrList->ValType  = ZH_EV_ARRAY;
   pArrList->nLength  = 0;
   pArrList->value.asList.reference = ZH_FALSE;

   pExpr = pArrList->value.asList.pExprList;   /* get first element on the list */
   /* Now we need to replace all EO_NONE expressions with ET_NIL expressions
    * If EO_NONE is the first expression and there is no more expressions
    * then it is an empty array {} and ET_NIL cannot be used
    */
   if( pExpr->ExprType == ZH_ET_NONE && pExpr->pNext == NULL )
   {
      pArrList->value.asList.pExprList = NULL;
      ZH_COMP_EXPR_FREE( pExpr );
   }
   else
   {
      /* there are at least one non-empty element specified
       */
      while( pExpr )
      {
         /* if empty element was specified replace it with NIL value */
         if( pExpr->ExprType == ZH_ET_NONE )
            pExpr->ExprType = ZH_ET_NIL;
         pExpr = pExpr->pNext;
         ++pArrList->nLength;
      }
   }
   pArrList->value.asList.pIndex = NULL;

   return pArrList;
}

/* Creates a new literal hash { key1=>val1, key2=>val2, ... keyN=>valN }
 *    'pHashList' is a list of hash items
 */
PZH_EXPR zh_compExprNewHash( PZH_EXPR pHashList, ZH_COMP_DECL )
{
   PZH_EXPR pExpr;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprNewHash()" ) );

   if( pHashList )
      pHashList->ExprType = ZH_ET_HASH;   /* change type from ET_LIST */
   else
   {
      pHashList = ZH_COMP_EXPR_NEW( ZH_ET_HASH );
      pHashList->value.asList.pExprList = NULL;
   }
   pHashList->ValType = ZH_EV_HASH;
   pHashList->nLength = 0;
   pHashList->value.asList.reference = ZH_FALSE;
   pHashList->value.asList.pIndex    = NULL;

   /*
    * replace all EO_NONE expressions with ET_NIL expressions and
    * calculate the list length
    */
   pExpr = pHashList->value.asList.pExprList;
   while( pExpr )
   {
      if( pExpr->ExprType == ZH_ET_NONE )
         pExpr->ExprType = ZH_ET_NIL;
      pExpr = pExpr->pNext;
      ++pHashList->nLength;
   }

   return pHashList;
}

PZH_EXPR zh_compExprNewCodeBlock( char * string, ZH_SIZE nLen, int iFlags, ZH_COMP_DECL )
{
   PZH_EXPR pExpr;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprNewCodeBlock(%s,%" ZH_PFS "u,%d,%p)", string, nLen, iFlags, ( void * ) ZH_COMP_PARAM ) );

   pExpr = ZH_COMP_EXPR_NEW( ZH_ET_CODEBLOCK );

   pExpr->value.asCodeblock.pExprList = NULL;
   pExpr->value.asCodeblock.pLocals   = NULL;  /* this will hold local variables declarations */
   pExpr->ValType = ZH_EV_CODEBLOCK;
   pExpr->value.asCodeblock.flags  = ( ZH_USHORT ) iFlags;
   pExpr->value.asCodeblock.string = string;
   pExpr->nLength = nLen;
   return pExpr;
}

PZH_EXPR zh_compExprAddCodeblockExpr( PZH_EXPR pList, PZH_EXPR pNewItem )
{
   if( pList->value.asCodeblock.pExprList )
   {
      PZH_EXPR pExpr;

      /* add new item to the end of the list */
      pExpr = pList->value.asCodeblock.pExprList;
      while( pExpr->pNext )
         pExpr = pExpr->pNext;
      pExpr->pNext = pNewItem;
   }
   else
      pList->value.asCodeblock.pExprList = pNewItem;

   return pList;
}

PZH_EXPR zh_compExprNewLogical( int iValue, ZH_COMP_DECL )
{
   PZH_EXPR pExpr;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprNewLogical(%i,%p)", iValue, ( void * ) ZH_COMP_PARAM ) );

   pExpr = ZH_COMP_EXPR_NEW( ZH_ET_LOGICAL );

   pExpr->value.asLogical = iValue;
   pExpr->ValType         = ZH_EV_LOGICAL;

   return pExpr;
}


PZH_EXPR zh_compExprNewNil( ZH_COMP_DECL )
{
   PZH_EXPR pExpr;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprNewNil(%p)", ( void * ) ZH_COMP_PARAM ) );

   pExpr = ZH_COMP_EXPR_NEW( ZH_ET_NIL );

   pExpr->ValType = ZH_EV_NIL;
   return pExpr;
}

PZH_EXPR zh_compExprNewSelf( ZH_COMP_DECL )
{
   PZH_EXPR pExpr;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprNewSelf(%p)", ( void * ) ZH_COMP_PARAM ) );

   pExpr = ZH_COMP_EXPR_NEW( ZH_ET_SELF );

   pExpr->ValType = ZH_EV_OBJECT;
   return pExpr;
}

PZH_EXPR zh_compExprNewVarRef( const char * szVarName, ZH_COMP_DECL )
{
   PZH_EXPR pExpr;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprNewVarRef(%s,%p)", szVarName, ( void * ) ZH_COMP_PARAM ) );

   pExpr = ZH_COMP_EXPR_NEW( ZH_ET_VARREF );

   pExpr->value.asSymbol.name = szVarName;
   pExpr->ValType = ZH_EV_VARREF;
   return pExpr;
}

PZH_EXPR zh_compExprNewFunRef( const char * szFunName, ZH_COMP_DECL )
{
   PZH_EXPR pExpr;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprNewFunRef(%s,%p)", szFunName, ( void * ) ZH_COMP_PARAM ) );

   pExpr = ZH_COMP_EXPR_NEW( ZH_ET_FUNREF );

   pExpr->value.asSymbol.name = zh_compGetFuncID( szFunName,
                                                  &pExpr->value.asSymbol.funcid,
                                                  &pExpr->value.asSymbol.flags );
   pExpr->ValType = ZH_EV_FUNREF;
   return pExpr;
}

PZH_EXPR zh_compExprNewRef( PZH_EXPR pRefer, ZH_COMP_DECL )
{
   PZH_EXPR pExpr;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprNewRef(%p,%p)", ( void * ) pRefer, ( void * ) ZH_COMP_PARAM ) );

   pExpr = ZH_COMP_EXPR_NEW( ZH_ET_REFERENCE );

   pExpr->value.asReference = pRefer;
   pExpr->ValType = ZH_EV_VARREF;
   return pExpr;
}

/* Creates new macro expression
 */
PZH_EXPR zh_compExprNewMacro( PZH_EXPR pMacroExpr,
                              unsigned char cMacroOp, const char * szName,
                              ZH_COMP_DECL )
{
   PZH_EXPR pExpr;

   pExpr = ZH_COMP_EXPR_NEW( ZH_ET_MACRO );
   if( szName )
   {
      ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprNewMacro(%s)", szName ) );

      /* Macro variable is used:  &identifier
       * or macro text: [text]&variable[more_macro_text]
       */
      /*
       * NOTE: Clipper assumes that all variables used in macro expressions
       * are memvar variables
       * NOTE: Clipper pushes the complete macro expression converted
       * to string in case complex expression is used, e.g.
       * My&var.1
       * is pushed as:
       * "MY&VAR.1"
       */
      pExpr->value.asMacro.cMacroOp  = cMacroOp; /* '&' if variable or 0 if text */
      pExpr->value.asMacro.szMacro   = szName;   /* variable name or macro text */
      pExpr->value.asMacro.pExprList = NULL;     /* this is not a parenthesized expressions */
      pExpr->value.asMacro.SubType   = ZH_ET_MACRO_VAR;
   }
   else
   {
      ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprNewMacro(&)" ) );

      /* Macro expression:  &( expression_list )
       */
      pExpr->value.asMacro.cMacroOp  = 0;
      pExpr->value.asMacro.szMacro   = NULL; /* this is used to distinguish &(...) from &ident */
      pExpr->value.asMacro.pExprList = pMacroExpr;
      pExpr->value.asMacro.SubType   = ZH_ET_MACRO_EXPR;
   }

   return pExpr;
}

/* Creates new aliased variable
 *    aliasexpr -> identifier
 */
PZH_EXPR zh_compExprNewAliasVar( PZH_EXPR pAlias, PZH_EXPR pVariable,
                                 ZH_COMP_DECL )
{
   PZH_EXPR pExpr;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprNewAliasVar()" ) );

   pExpr = ZH_COMP_EXPR_NEW( ZH_ET_ALIASVAR );

   pExpr->value.asAlias.pAlias   = pAlias;
   pExpr->value.asAlias.pVar     = pVariable;
   pExpr->value.asAlias.pExpList = NULL;

   /* macro expressions in alias context require a special handling
    */
   if( pAlias->ExprType == ZH_ET_MACRO )
      pAlias->value.asMacro.SubType = ZH_ET_MACRO_ALIASED;
   if( pVariable->ExprType == ZH_ET_MACRO )
      pVariable->value.asMacro.SubType = ZH_ET_MACRO_ALIASED;

   return pExpr;
}

/* Creates new aliased expression
 *    alias_expr -> ( expression )
 */
PZH_EXPR zh_compExprNewAliasExpr( PZH_EXPR pAlias, PZH_EXPR pExpList,
                                  ZH_COMP_DECL )
{
   PZH_EXPR pExpr;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprNewAliasExpr()" ) );

   pExpr = ZH_COMP_EXPR_NEW( ZH_ET_ALIASEXPR );

   pExpr->value.asAlias.pAlias   = pAlias;
   pExpr->value.asAlias.pExpList = pExpList;
   pExpr->value.asAlias.pVar     = NULL;

   if( pAlias->ExprType == ZH_ET_MACRO )
   {
      /* Is it a special case &variable->( expressionList ) */
      if( pAlias->value.asMacro.SubType & ( ZH_ET_MACRO_VAR | ZH_ET_MACRO_EXPR ) )
         pAlias->value.asMacro.SubType = ZH_ET_MACRO_ALIASED;
   }

   return pExpr;
}

/* Creates new send expression
 *    : <msgid> -> ( expression )
 */
PZH_EXPR zh_compExprNewSend( const char * szMessage, ZH_COMP_DECL )
{
   PZH_EXPR pExpr;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprNewSend(%s,%p)", szMessage, ( void * ) ZH_COMP_PARAM ) );

   pExpr = ZH_COMP_EXPR_NEW( ZH_ET_SEND );
   pExpr->value.asMessage.pObject = NULL;
   pExpr->value.asMessage.pParms  = NULL;

   pExpr->value.asMessage.szMessage = szMessage;
   pExpr->value.asMessage.pMessage  = NULL;

   pExpr->nLength = 0;

   return pExpr;
}

/* Creates new macro send expression
 *    : &<msg> -> ( expression )
 */
PZH_EXPR zh_compExprNewMacroSend( PZH_EXPR pMessage, ZH_COMP_DECL )
{
   PZH_EXPR pExpr;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprNewMacroSend(%p,%p)", ( void * ) pMessage, ( void * ) ZH_COMP_PARAM ) );

   pExpr = ZH_COMP_EXPR_NEW( ZH_ET_SEND );
   pExpr->value.asMessage.pObject = NULL;
   pExpr->value.asMessage.pParms  = NULL;

   pExpr->value.asMessage.szMessage = NULL;
   pExpr->value.asMessage.pMessage  = pMessage;

   pExpr->nLength = 0;

   if( pMessage->ExprType == ZH_ET_MACRO )
   {
      /* Signal that macro compiler have to generate a pcode that will
       * return function name as symbol instead of usual value
       */
      pMessage->value.asMacro.SubType = ZH_ET_MACRO_SYMBOL;
   }

   return pExpr;
}

/* Set object in send expression
 *    pObject : pExpr
 *
 *    pExpr   = is an expression returned by zh_compExprNewSend()
 *    pObject = is an object
 */
PZH_EXPR zh_compExprNewMethodObject( PZH_EXPR pExpr, PZH_EXPR pObject )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprNewMethodObject(%p,%p)", ( void * ) pExpr, ( void * ) pObject ) );

   pExpr->value.asMessage.pObject = pObject;

   return pExpr;
}

/* Creates new method call
 *    pObject : identifier ( pArgList )
 *
 *    pObject  = is an expression returned by zh_compExprNewSend()
 *    pArgList = list of passed arguments - it will be ZH_ET_NONE if no arguments
 *                are passed
 */
PZH_EXPR zh_compExprNewMethodCall( PZH_EXPR pObject, PZH_EXPR pArgList )
{
   pObject->value.asMessage.pParms = pArgList;

   return pObject;
}

/* Create a new iif() expression
 * pExpr is a list of three expressions
 */
PZH_EXPR zh_compExprNewIIF( PZH_EXPR pExpr )
{
   pExpr->ExprType = ZH_ET_IIF;

   return pExpr;
}

/* Creates a list - all elements will be used
 * This list can be used to create an array or function's call arguments
 */
PZH_EXPR zh_compExprNewList( PZH_EXPR pFirstItem, ZH_COMP_DECL )
{
   PZH_EXPR pExpr;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprNewList()" ) );

   pExpr = ZH_COMP_EXPR_NEW( ZH_ET_LIST );
   pExpr->value.asList.pExprList = pFirstItem;
   pExpr->value.asList.reference = ZH_FALSE;
   return pExpr;
}

/* Creates a list of function call arguments
 */
PZH_EXPR zh_compExprNewArgList( PZH_EXPR pFirstItem, ZH_COMP_DECL )
{
   PZH_EXPR pExpr;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprNewArgList()" ) );

   pExpr = ZH_COMP_EXPR_NEW( ZH_ET_ARGLIST );
   pExpr->value.asList.pExprList = pFirstItem;
   pExpr->value.asList.reference = ZH_FALSE;
   return pExpr;
}

/* Creates a reference to variable arguments
 */
PZH_EXPR zh_compExprNewArgRef( ZH_COMP_DECL )
{
   PZH_EXPR pExpr;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprNewArgRef()" ) );

   pExpr = ZH_COMP_EXPR_NEW( ZH_ET_ARGLIST );
   pExpr->value.asList.pExprList = NULL;
   pExpr->value.asList.reference = ZH_TRUE;
   return pExpr;
}

/* Adds new element to the list
 */
PZH_EXPR zh_compExprAddListExpr( PZH_EXPR pList, PZH_EXPR pNewItem )
{
   if( pList->value.asList.pExprList )
   {
      PZH_EXPR pExpr;

      /* add new item to the end of the list */
      pExpr = pList->value.asList.pExprList;
      while( pExpr->pNext )
         pExpr = pExpr->pNext;
      pExpr->pNext = pNewItem;
   }
   else
      pList->value.asList.pExprList = pNewItem;

   return pList;
}

PZH_EXPR zh_compExprNewVar( const char * szName, ZH_COMP_DECL )
{
   PZH_EXPR pExpr;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprNewVar(%s,%p)", szName, ( void * ) ZH_COMP_PARAM ) );

   pExpr = ZH_COMP_EXPR_NEW( ZH_ET_VARIABLE );
   pExpr->value.asSymbol.name = szName;
   return pExpr;
}

/* Create a new declaration of PUBLIC or PRIVATE variable.
 *
 * szName is a string with variable name if 'PUBLIC varname' context
 * pMacroVar is a macro expression if 'PUBLIC &varname' context
 */
PZH_EXPR zh_compExprNewRTVar( const char * szName, PZH_EXPR pMacroVar,
                              ZH_COMP_DECL )
{
   PZH_EXPR pExpr;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprNewRTVar(%s, %p, %p)", szName, ( void * ) pMacroVar, ( void * ) ZH_COMP_PARAM ) );

   pExpr = ZH_COMP_EXPR_NEW( ZH_ET_RTVAR );
   pExpr->value.asRTVar.szName = szName;
   pExpr->value.asRTVar.pMacro = pMacroVar;
   if( pMacroVar )
      pMacroVar->value.asMacro.SubType = ZH_ET_MACRO_SYMBOL;
   return pExpr;
}

/* Create a new symbol used in function calls
 */
PZH_EXPR zh_compExprNewFunName( const char * szName, ZH_COMP_DECL )
{
   PZH_EXPR pExpr;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprNewFunName(%s,%p)", szName, ( void * ) ZH_COMP_PARAM ) );

   pExpr = ZH_COMP_EXPR_NEW( ZH_ET_FUNNAME );
   pExpr->value.asSymbol.name = zh_compGetFuncID( szName,
                                                  &pExpr->value.asSymbol.funcid,
                                                  &pExpr->value.asSymbol.flags );
   return pExpr;
}

/* Create a new symbol used in an alias expressions
 */
PZH_EXPR zh_compExprNewAlias( const char * szName, ZH_COMP_DECL )
{
   PZH_EXPR pExpr;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprNewAlias(%s,%p)", szName, ( void * ) ZH_COMP_PARAM ) );

   pExpr = ZH_COMP_EXPR_NEW( ZH_ET_ALIAS );
   pExpr->value.asSymbol.name = szName;
   return pExpr;
}


/* ************************************************************************* */

PZH_EXPR zh_compExprNewEqual( PZH_EXPR pLeftExpr, ZH_COMP_DECL )
{
   PZH_EXPR pExpr = ZH_COMP_EXPR_NEW( ZH_EO_EQUAL );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PZH_EXPR zh_compExprNewPlus( PZH_EXPR pLeftExpr, ZH_COMP_DECL )
{
   PZH_EXPR pExpr = ZH_COMP_EXPR_NEW( ZH_EO_PLUS );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PZH_EXPR zh_compExprNewMinus( PZH_EXPR pLeftExpr, ZH_COMP_DECL )
{
   PZH_EXPR pExpr = ZH_COMP_EXPR_NEW( ZH_EO_MINUS );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PZH_EXPR zh_compExprNewMult( PZH_EXPR pLeftExpr, ZH_COMP_DECL )
{
   PZH_EXPR pExpr = ZH_COMP_EXPR_NEW( ZH_EO_MULT );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PZH_EXPR zh_compExprNewDiv( PZH_EXPR pLeftExpr, ZH_COMP_DECL )
{
   PZH_EXPR pExpr = ZH_COMP_EXPR_NEW( ZH_EO_DIV );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PZH_EXPR zh_compExprNewMod( PZH_EXPR pLeftExpr, ZH_COMP_DECL )
{
   PZH_EXPR pExpr = ZH_COMP_EXPR_NEW( ZH_EO_MOD );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PZH_EXPR zh_compExprNewPower( PZH_EXPR pLeftExpr, ZH_COMP_DECL )
{
   PZH_EXPR pExpr = ZH_COMP_EXPR_NEW( ZH_EO_POWER );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PZH_EXPR zh_compExprNewPostInc( PZH_EXPR pLeftExpr, ZH_COMP_DECL )
{
   PZH_EXPR pExpr = ZH_COMP_EXPR_NEW( ZH_EO_POSTINC );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PZH_EXPR zh_compExprNewPostDec( PZH_EXPR pLeftExpr, ZH_COMP_DECL )
{
   PZH_EXPR pExpr = ZH_COMP_EXPR_NEW( ZH_EO_POSTDEC );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PZH_EXPR zh_compExprNewPreInc( PZH_EXPR pLeftExpr, ZH_COMP_DECL )
{
   PZH_EXPR pExpr = ZH_COMP_EXPR_NEW( ZH_EO_PREINC );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PZH_EXPR zh_compExprNewPreDec( PZH_EXPR pLeftExpr, ZH_COMP_DECL )
{
   PZH_EXPR pExpr = ZH_COMP_EXPR_NEW( ZH_EO_PREDEC );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PZH_EXPR zh_compExprNewPlusEq( PZH_EXPR pLeftExpr, ZH_COMP_DECL )
{
   PZH_EXPR pExpr = ZH_COMP_EXPR_NEW( ZH_EO_PLUSEQ );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PZH_EXPR zh_compExprNewMinusEq( PZH_EXPR pLeftExpr, ZH_COMP_DECL )
{
   PZH_EXPR pExpr = ZH_COMP_EXPR_NEW( ZH_EO_MINUSEQ );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PZH_EXPR zh_compExprNewMultEq( PZH_EXPR pLeftExpr, ZH_COMP_DECL )
{
   PZH_EXPR pExpr = ZH_COMP_EXPR_NEW( ZH_EO_MULTEQ );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PZH_EXPR zh_compExprNewDivEq( PZH_EXPR pLeftExpr, ZH_COMP_DECL )
{
   PZH_EXPR pExpr = ZH_COMP_EXPR_NEW( ZH_EO_DIVEQ );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PZH_EXPR zh_compExprNewModEq( PZH_EXPR pLeftExpr, ZH_COMP_DECL )
{
   PZH_EXPR pExpr = ZH_COMP_EXPR_NEW( ZH_EO_MODEQ );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PZH_EXPR zh_compExprNewExpEq( PZH_EXPR pLeftExpr, ZH_COMP_DECL )
{
   PZH_EXPR pExpr = ZH_COMP_EXPR_NEW( ZH_EO_EXPEQ );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PZH_EXPR zh_compExprNewAnd( PZH_EXPR pLeftExpr, ZH_COMP_DECL )
{
   PZH_EXPR pExpr = ZH_COMP_EXPR_NEW( ZH_EO_AND );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PZH_EXPR zh_compExprNewOr( PZH_EXPR pLeftExpr, ZH_COMP_DECL )
{
   PZH_EXPR pExpr = ZH_COMP_EXPR_NEW( ZH_EO_OR );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PZH_EXPR zh_compExprNewNot( PZH_EXPR pNotExpr, ZH_COMP_DECL )
{
   PZH_EXPR pExpr;

   if( pNotExpr->ExprType == ZH_ET_LOGICAL )
   {
      pNotExpr->value.asLogical = ! pNotExpr->value.asLogical;
      pExpr = pNotExpr;
   }
   else
   {
      pExpr = ZH_COMP_EXPR_NEW( ZH_EO_NOT );
      pExpr->value.asOperator.pLeft  = pNotExpr;
      pExpr->value.asOperator.pRight = NULL;
   }

   return pExpr;
}

PZH_EXPR zh_compExprNewEQ( PZH_EXPR pLeftExpr, ZH_COMP_DECL )
{
   PZH_EXPR pExpr = ZH_COMP_EXPR_NEW( ZH_EO_EQ );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PZH_EXPR zh_compExprNewLT( PZH_EXPR pLeftExpr, ZH_COMP_DECL )
{
   PZH_EXPR pExpr = ZH_COMP_EXPR_NEW( ZH_EO_LT );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PZH_EXPR zh_compExprNewGT( PZH_EXPR pLeftExpr, ZH_COMP_DECL )
{
   PZH_EXPR pExpr = ZH_COMP_EXPR_NEW( ZH_EO_GT );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PZH_EXPR zh_compExprNewLE( PZH_EXPR pLeftExpr, ZH_COMP_DECL )
{
   PZH_EXPR pExpr = ZH_COMP_EXPR_NEW( ZH_EO_LE );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PZH_EXPR zh_compExprNewGE( PZH_EXPR pLeftExpr, ZH_COMP_DECL )
{
   PZH_EXPR pExpr = ZH_COMP_EXPR_NEW( ZH_EO_GE );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PZH_EXPR zh_compExprNewNE( PZH_EXPR pLeftExpr, ZH_COMP_DECL )
{
   PZH_EXPR pExpr = ZH_COMP_EXPR_NEW( ZH_EO_NE );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PZH_EXPR zh_compExprNewIN( PZH_EXPR pLeftExpr, ZH_COMP_DECL )
{
   PZH_EXPR pExpr = ZH_COMP_EXPR_NEW( ZH_EO_IN );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

/* NOTE: all invalid cases are handled by yacc rules
 */
PZH_EXPR zh_compExprNewNegate( PZH_EXPR pNegExpr, ZH_COMP_DECL )
{
   PZH_EXPR pExpr;

   if( pNegExpr->ExprType == ZH_ET_NUMERIC )
   {
      if( pNegExpr->value.asNum.NumType == ZH_ET_DOUBLE )
      {
         pNegExpr->value.asNum.val.d  = -pNegExpr->value.asNum.val.d;
         pNegExpr->value.asNum.bWidth = ( ZH_UCHAR ) ZH_DBL_LENGTH( pNegExpr->value.asNum.val.d );
      }
      else
      {
#if -ZH_VMLONG_MAX > ZH_VMLONG_MIN
         if( pNegExpr->value.asNum.val.l < -ZH_VMLONG_MAX )
         {
            pNegExpr->value.asNum.NumType = ZH_ET_DOUBLE;
            pNegExpr->value.asNum.val.d   = -( double ) pNegExpr->value.asNum.val.l;
            pNegExpr->value.asNum.bWidth  = ( ZH_UCHAR ) ZH_DBL_LENGTH( pNegExpr->value.asNum.val.d );
            pNegExpr->value.asNum.bDec    = 0;
         }
         else
#endif
         {
            pNegExpr->value.asNum.val.l  = -pNegExpr->value.asNum.val.l;
            pNegExpr->value.asNum.bWidth = ZH_DEFAULT_WIDTH;
         }
      }
      pExpr = pNegExpr;
   }
   else
   {
      pExpr = ZH_COMP_EXPR_NEW( ZH_EO_NEGATE );
      pExpr->value.asOperator.pLeft  = pNegExpr;
      pExpr->value.asOperator.pRight = NULL;
   }
   return pExpr;
}

/* Handles (expression := expression) syntax
 */
PZH_EXPR zh_compExprAssign( PZH_EXPR pLeftExpr, PZH_EXPR pRightExpr,
                            ZH_COMP_DECL )
{
   PZH_EXPR pExpr;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprAssign()" ) );

   pExpr = ZH_COMP_EXPR_NEW( ZH_EO_ASSIGN );
   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = pRightExpr;
   return pExpr;
}

void zh_compExprDelOperator( PZH_EXPR pExpr, ZH_COMP_DECL )
{
   if( pExpr->value.asOperator.pLeft )
      ZH_COMP_EXPR_FREE( pExpr->value.asOperator.pLeft );
   if( pExpr->value.asOperator.pRight )
      ZH_COMP_EXPR_FREE( pExpr->value.asOperator.pRight );
}

/* Sets the argument of an operation found previously
 */
PZH_EXPR zh_compExprSetOperand( PZH_EXPR pExpr, PZH_EXPR pItem, ZH_COMP_DECL )
{
   ZH_BYTE ucRight;

   ucRight = s_PrecedTable[ pItem->ExprType ];

   if( ucRight == ZH_ET_NIL )
   {
      /* the right side of an operator is an ordinary value
       * e.g. a := 1
       */
      pExpr->value.asOperator.pRight = pItem;
   }
   else if( ucRight == ZH_ET_NONE )
   {
      /* the right side of an operator is an invalid expression
       * e.g.
       *    a := 1 + b:=2
       *    a := 1 + b += 2
       */

      if( pExpr->ExprType >= ZH_EO_PLUSEQ && pExpr->ExprType <= ZH_EO_EXPEQ )
      {
      }
      else
      {
         ZH_COMP_ERROR_SYNTAX( pItem );
      }
      pExpr->value.asOperator.pRight = pItem; /* set it anyway */
   }
   else
   {
      /* the right side of an operator is an expression with other operator
       * e.g. a := 2 + b * 3
       *   We have to set the proper order of evaluation using
       * precedence rules
       */
      ZH_BYTE ucLeft = s_PrecedTable[ pExpr->ExprType ];
      if( ucLeft < ucRight ||
          ( ucLeft == ucRight && ZH_COMP_ISSUPPORTED( ZH_COMPFLAG_SHORTCUTS ) &&
            ( ucLeft == ZH_EO_OR || ucLeft == ZH_EO_AND ) ) )
      {
         /* Left operator has a lower precedence then the right one
          * e.g.  a + b * c
          *    pItem -> b * c    -> L=b  R=c  O=*
          *    pExpr -> a +      -> l=a  r=   o=+
          *
          *    -> a + (b * c)    -> Left=a  Right=(b * c)  Oper=+
          *             Left  := l
          *             Right := L (O) R  := pItem
          *             Oper  := o
          */
         pExpr->value.asOperator.pRight = pItem;
      }
      else
      {
         /* Left operator has the same or higher precedence then the right one
          * e.g.  a * b + c
          *    pItem -> b + c   -> L=b  R=c  O=+
          *    pExpr -> a *     -> l=a  r=   o=*
          *
          *    -> (a * b) + c    -> Lelf=(a * b)  Right=c  Oper=+
          *             Left  := l (o) L
          *             Right := R
          *             Oper  := O
          */
         pItem->value.asOperator.pLeft = zh_compExprSetOperand( pExpr, pItem->value.asOperator.pLeft, ZH_COMP_PARAM );
         pExpr = pItem;
      }
   }

   return pExpr;
}


/* ************************************************************************* */

/* Handles prefix&macro-> and &macro.sufix-> in macro compiler
 * Clipper uses macro var directly as alias name in such case
 */
PZH_EXPR zh_compExprMacroAsAlias( PZH_EXPR pExpr )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprMacroAsAlias()" ) );

   if( pExpr->ExprType == ZH_ET_VARIABLE )
      pExpr->ExprType = ZH_ET_ALIAS;

   return pExpr;
}

/* Return a number of elements on the linked list
 */
ZH_ULONG zh_compExprListLen( PZH_EXPR pExpr )
{
   ZH_ULONG nLen = 0;

   pExpr = pExpr->value.asList.pExprList;
   while( pExpr )
   {
      pExpr = pExpr->pNext;
      ++nLen;
   }

   return nLen;
}

ZH_BOOL zh_compExprListTypeCheck( PZH_EXPR pExpr, ZH_EXPRTYPE ExprType )
{
   pExpr = pExpr->value.asList.pExprList;
   if( pExpr )
   {
      do
      {
         if( pExpr->ExprType != ExprType )
            break;
         pExpr = pExpr->pNext;
      }
      while( pExpr );

      return pExpr == NULL;
   }
   return ZH_FALSE;
}

/* Return a number of parameters passed to function or method
 */
ZH_ULONG zh_compExprParamListLen( PZH_EXPR pExpr )
{
   ZH_ULONG nLen = 0;

   if( pExpr )
   {
      PZH_EXPR pParam = pExpr->value.asList.pExprList;
      while( pParam )
      {
         pParam = pParam->pNext;
         ++nLen;
      }
      /* NOTE: if method or function with no parameters is called then the
       * list of parameters contain only one expression of type ZH_ET_NONE
       * There is no need to calculate this parameter
       */
      if( nLen == 1 && pExpr->value.asList.pExprList->ExprType == ZH_ET_NONE )
         nLen = 0;
   }

   return nLen;
}

/* Check if expression is zh_ArrayToParams( aParams ) function call
 */
ZH_BOOL zh_compExprIsArrayToParams( PZH_EXPR pExpr )
{
   return pExpr->ExprType == ZH_ET_FUNCALL &&
          pExpr->value.asFunCall.pFunName->ExprType == ZH_ET_FUNNAME &&
          pExpr->value.asFunCall.pFunName->value.asSymbol.funcid == ZH_F_ARRAYTOPARAMS;
}

ZH_SIZE zh_compExprParamListCheck( ZH_COMP_DECL, PZH_EXPR pExpr )
{
   ZH_SIZE nLen = 0;

   if( pExpr )
   {
      ZH_SIZE nItems = 0;
      PZH_EXPR pElem;

      pElem = pExpr->value.asList.pExprList;
      while( pElem )
      {
         if( ( pElem->ExprType == ZH_ET_MACRO &&
               ( pElem->value.asMacro.SubType & ZH_ET_MACRO_NOLIST ) == 0 ) ||
             ( pElem->ExprType == ZH_ET_ARGLIST && pElem->value.asList.reference ) ||
             zh_compExprIsArrayToParams( pElem ) )
         {
            /* &macro was passed
               or optional parameters list passed, e.g.: f( a, b, ... )
               or zh_ArrayToParams( aParams )
               - handle it differently then in a normal statement */
            if( pElem->ExprType == ZH_ET_MACRO )
               pElem->value.asMacro.SubType |= ZH_ET_MACRO_LIST;
            else if( pElem->ExprType == ZH_ET_FUNCALL )
               pElem->value.asFunCall.pFunName->value.asSymbol.flags |= ZH_FN_MULTIARG;
            if( nItems )
            {
               nItems = 0;
               ++nLen;
            }
            ++nLen;
         }
         else
            ++nItems;
         pElem = pElem->pNext;
      }

      if( nLen )
      {
         if( nItems )
            ++nLen;
         /* Note: direct type change */
         pExpr->ExprType = ZH_ET_MACROARGLIST;
      }
      /* NOTE: if method or function with no parameters is called then the
       * list of parameters contain only one expression of type ZH_ET_NONE
       * There is no need to calculate this parameter
       */
      else if( nItems == 1 &&
               pExpr->value.asList.pExprList->ExprType == ZH_ET_NONE )
         nLen = 0;
      else
         nLen = nItems;
   }

   return nLen;
}

/* Create a new declaration for codeblock local variable
 */
static PZH_CBVAR zh_compExprCBVarNew( const char * szVarName, ZH_BYTE bType )
{
   PZH_CBVAR pVar;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprCBVarNew(%s)", szVarName ) );

   pVar = ( PZH_CBVAR ) zh_xgrab( sizeof( ZH_CBVAR ) );

   pVar->szName = szVarName;
   pVar->bType  = bType;
   pVar->pNext  = NULL;
   pVar->bUsed  = ZH_FALSE;

   return pVar;
}

/* Add a new local variable declaration
 */
PZH_EXPR zh_compExprCBVarAdd( PZH_EXPR pCB, const char * szVarName, ZH_BYTE bType,
                              ZH_COMP_DECL )
{
   PZH_CBVAR pVar;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprCBVarAdd(%s)", szVarName ) );

   if( pCB->value.asCodeblock.pLocals )
   {
      /* add it to the end of the list
       */
      pVar = pCB->value.asCodeblock.pLocals;
      while( pVar )
      {
         if( strcmp( szVarName, pVar->szName ) == 0 )
            ZH_COMP_ERROR_DUPLVAR( szVarName );

         if( pVar->pNext )
            pVar = pVar->pNext;
         else
         {
            pVar->pNext = zh_compExprCBVarNew( szVarName, bType );
            break;
         }
      }
   }
   else
      pCB->value.asCodeblock.pLocals = zh_compExprCBVarNew( szVarName, bType );

   return pCB;
}

/* NOTE: This deletes all linked variables
 */
void zh_compExprCBVarDel( PZH_CBVAR pVars )
{
   while( pVars )
   {
      PZH_CBVAR pDel = pVars;
      pVars = pVars->pNext;
      zh_xfree( pDel );
   }
}

/* Creates a set/get codeblock for passed expression used in __GET
 *
 * {| ~1 | iif( ~1 == NIL, <pExpr>, <pExpr> := ~1 ) }
 *
 * NOTE: "~1" is not a valid variable name so there will be no collisions
 */
PZH_EXPR zh_compExprSetGetBlock( PZH_EXPR pExpr, ZH_COMP_DECL )
{
   PZH_EXPR pSet;

   /* create setget expression: IIF( var==NIL, <pExpr>, <pExpr>:=var ) */
   pSet = ZH_COMP_EXPR_NEW( ZH_ET_SETGET );
   pSet->value.asSetGet.pVar  = zh_compExprNewVar( "~1", ZH_COMP_PARAM );
   pSet->value.asSetGet.pExpr = pExpr;
   /* create a codeblock */
   return zh_compExprAddCodeblockExpr( zh_compExprCBVarAdd(
                                          zh_compExprNewCodeBlock( NULL, 0, 0, ZH_COMP_PARAM ),
                                          "~1", ' ', ZH_COMP_PARAM ), pSet );
}
