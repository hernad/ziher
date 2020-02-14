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

#ifndef ZH_MACRO_SUPPORT
ZH_SIZE zh_compExprListEval( ZH_COMP_DECL, PZH_EXPR pExpr, PZH_COMP_CARGO_FUNC pEval )
{
   ZH_SIZE nLen = 0;

   if( pEval && ( pExpr->ExprType == ZH_ET_LIST ||
                  pExpr->ExprType == ZH_ET_ARGLIST ) )
   {
      pExpr = pExpr->value.asList.pExprList;
      while( pExpr )
      {
         ( pEval )( ZH_COMP_PARAM, ( void * ) pExpr );
         pExpr = pExpr->pNext;
         ++nLen;
      }
   }
   return nLen;
}

ZH_SIZE zh_compExprListEval2( ZH_COMP_DECL, PZH_EXPR pExpr1, PZH_EXPR pExpr2, PZH_COMP_CARGO2_FUNC pEval )
{
   ZH_SIZE nLen = 0;

   if( ! pEval )
      return nLen;

   if( ( pExpr1->ExprType == ZH_ET_LIST || pExpr1->ExprType == ZH_ET_ARGLIST )
       &&
       ( pExpr2->ExprType == ZH_ET_LIST || pExpr2->ExprType == ZH_ET_ARGLIST ) )
   {
      pExpr1 = pExpr1->value.asList.pExprList;
      pExpr2 = pExpr2->value.asList.pExprList;
      while( pExpr1 && pExpr2 )
      {
         ( pEval )( ZH_COMP_PARAM, ( void * ) pExpr1, ( void * ) pExpr2 );
         pExpr1 = pExpr1->pNext;
         pExpr2 = pExpr2->pNext;
         ++nLen;
      }
   }
   else if( pExpr1->ExprType == ZH_ET_LIST || pExpr1->ExprType == ZH_ET_ARGLIST )
   {
      pExpr1 = pExpr1->value.asList.pExprList;
      while( pExpr1 )
      {
         ( pEval )( ZH_COMP_PARAM, ( void * ) pExpr1, ( void * ) pExpr2 );
         pExpr1 = pExpr1->pNext;
         ++nLen;
      }
   }
   return nLen;
}
#endif

/* Create function call
 */
#ifdef ZH_MACRO_SUPPORT
PZH_EXPR zh_macroExprNewFunCall( PZH_EXPR pName, PZH_EXPR pParms, ZH_COMP_DECL )
#else
PZH_EXPR zh_compExprNewFunCall( PZH_EXPR pName, PZH_EXPR pParms, ZH_COMP_DECL )
#endif
{
   PZH_EXPR pExpr;

#ifdef ZH_MACRO_SUPPORT
   if( pName->ExprType == ZH_ET_VARIABLE )
   {
      /* My&var.1() executed by macro compiler
       */
      /* NOTE: direct type change */
      pName->ExprType = ZH_ET_FUNNAME;
      pName->value.asSymbol.name =
                              zh_compGetFuncID( pName->value.asSymbol.name,
                                                &pName->value.asSymbol.funcid,
                                                &pName->value.asSymbol.flags );
   }
#endif

   if( pName->ExprType == ZH_ET_FUNNAME )
   {
      /* The name of a function is specified at compile time
       * e.g. MyFunc()
       *
       * NOTE:  'pName' can be a macro expression that will be resolved
       * at runtime - in this case pName is an expression of ZH_ET_MACRO type
       * e.g. &MyVar()
       */

      ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprNewFunCall(%s)", pName->value.asSymbol.name ) );

#if ! defined( ZH_MACRO_SUPPORT ) && defined( ZH_USE_ENUM_FUNCTIONS )
      {
         int iLen = strlen( pName->value.asSymbol.name );
         if( iLen >= 10 && iLen <= 14 && memcmp( "ZH_ENUM", pName->value.asSymbol.name, 7 ) == 0 )
         {
            const char * szMessage = pName->value.asSymbol.name + 7;

            if( iLen == 12 && memcmp( "INDEX", szMessage, 5 ) == 0 )
               szMessage = "__ENUMINDEX";
            else if( iLen == 12 && memcmp( "VALUE", szMessage, 5 ) == 0 )
               szMessage = "__ENUMVALUE";
            else if( iLen == 11 && memcmp( "BASE", szMessage, 4 ) == 0 )
               szMessage = "__ENUMBASE";
            else if( iLen == 10 && memcmp( "KEY", szMessage, 3 ) == 0 )
               szMessage = "__ENUMKEY";
            else if( iLen == 14 && memcmp( "ISFIRST", szMessage, 7 ) == 0 )
               szMessage = "__ENUMISFIRST";
            else if( iLen == 13 && memcmp( "ISLAST", szMessage, 6 ) == 0 )
               szMessage = "__ENUMISLAST";
            else
               szMessage = NULL;

            if( szMessage )
            {
               int iCount = ( int ) zh_compExprParamListLen( pParms );
               PZH_ENUMERATOR pForVar, pEnumVar = NULL;

               pForVar = ZH_COMP_PARAM->functions.pLast->pEnum;

               if( iCount == 0 )
               {
                  while( pForVar )
                  {
                     if( pForVar->iForEachDir != 0 )
                        pEnumVar = pForVar;
                     pForVar = pForVar->pNext;
                  }
               }
               else if( iCount == 1 )
               {
                  if( pParms->value.asList.pExprList->ExprType == ZH_ET_VARIABLE ||
                      pParms->value.asList.pExprList->ExprType == ZH_ET_VARREF )
                  {
                     const char * szName = pParms->value.asList.pExprList->value.asSymbol.name;

                     while( pForVar )
                     {
                        if( pForVar->iForEachDir != 0 &&
                            strcmp( pEnumVar->szName, szName ) == 0 )
                        {
                           pEnumVar = pForVar;
                           break;
                        }
                        pForVar = pForVar->pNext;
                     }
                  }
               }
               if( pEnumVar )
               {

                  if( pParms )
                     ZH_COMP_EXPR_FREE( pParms );
                  ZH_COMP_EXPR_FREE( pName );
                  return zh_compExprNewMethodObject(
                                 zh_compExprNewSend( szMessage, ZH_COMP_PARAM ),
                                 zh_compExprNewVar( pEnumVar->szName, ZH_COMP_PARAM ) );
               }
            }
         }
      }
#endif
      if( pName->value.asSymbol.funcid == ZH_F_EVAL &&
          zh_compExprParamListLen( pParms ) != 0 )
      {
         /* Optimize Eval( bBlock, [ArgList] ) to: bBlock:Eval( [ArgList] ) */
         PZH_EXPR pEval;

         pEval = zh_compExprNewMethodCall(
                        zh_compExprNewMethodObject(
                              zh_compExprNewSend( "EVAL", ZH_COMP_PARAM ),
                              pParms->value.asList.pExprList ),
                        zh_compExprNewArgList(
                              pParms->value.asList.pExprList->pNext,
                              ZH_COMP_PARAM ) );
#if ! defined( ZH_MACRO_SUPPORT )
         /* force reduction */
         pEval->nLength = 1;
#endif
         pParms->value.asList.pExprList = NULL;
         ZH_COMP_EXPR_FREE( pParms );
         ZH_COMP_EXPR_FREE( pName );
         return pEval;
      }
      else if( pName->value.asSymbol.funcid == ZH_F__GET_ &&
               zh_compExprParamListLen( pParms ) != 0 )
      {
         /* Reserved Clipper function used to handle GET variables
          */
         PZH_EXPR pArg, pNext;

         /* pArg has to be reduced to eliminate possible problems with
          * cloned expressions in SETGET block
          */
         
            pParms = ZH_EXPR_USE( pParms, ZH_EA_REDUCE );
            pArg = pParms->value.asList.pExprList;
         
         if( pArg->ExprType == ZH_ET_ARRAYAT )
         {
            ZH_USHORT uiCount;

            /* replace:
               _GET_( a[1], "a[1]", , , )
               into:
               __GetA( {||a }, "a", , , , { 1 } )
             */
            PZH_EXPR pIndex, pVar;
            PZH_EXPR pBase;

            pName->value.asSymbol.name = "__GETA";
            /* NOTE: a[ i, j ] is stored as: (pExprList)->(pIndex)
             * ( ( a->[ i ] )->[ j ] )
             */
            pVar = ZH_EXPR_USE( pArg->value.asList.pExprList, ZH_EA_REDUCE );
            pBase = pVar->ExprType == ZH_ET_ARRAYAT ? pVar : NULL;
            pIndex = ZH_EXPR_USE( pArg->value.asList.pIndex, ZH_EA_REDUCE );
            pIndex->pNext = NULL;
            while( pVar->ExprType == ZH_ET_ARRAYAT )
            {
               /* traverse back to a leftmost expression and build a list
                * of index expressions
                */
               pVar->value.asList.pIndex->pNext = pIndex;
               pIndex = pVar->value.asList.pIndex;
               pVar = pVar->value.asList.pExprList;
            }

            /* create a set only codeblock */
            if( pVar->ExprType == ZH_ET_MACRO )
            {
               /* &var[ 1 ] */
               ZH_COMP_EXPR_FREE( pVar );
               pVar = zh_compExprNewNil( ZH_COMP_PARAM );
            }
            else
            {
               pVar = zh_compExprAddCodeblockExpr( zh_compExprNewCodeBlock( NULL, 0, 0, ZH_COMP_PARAM ), pVar );
            }

            /* pVar will be the first argument now
             */
            pParms->value.asList.pExprList = pVar;
            /* link the rest of parameters
             */
            pVar->pNext = pArg->pNext;
            /* Delete an argument that was the first one
             */
            pArg->value.asList.pIndex = NULL;
            pArg->value.asList.pExprList = NULL;
            ZH_COMP_EXPR_CLEAR( pArg );
            /* Create an array with index elements
             */
            pIndex = zh_compExprNewArray( zh_compExprNewList( pIndex, ZH_COMP_PARAM ), ZH_COMP_PARAM );
            /* The array with index elements have to be the sixth argument
             * of __GetA() call
             */
            uiCount = 1;
            while( ++uiCount < 6 )
            {
               if( pVar->pNext == NULL )
                  pVar->pNext = zh_compExprNewNil( ZH_COMP_PARAM );
               pVar = pVar->pNext;
            }
            if( pVar->pNext ) /* Delete 6th argument if present */
            {
               pIndex->pNext = pVar->pNext->pNext;
               ZH_COMP_EXPR_FREE( pVar->pNext );
            }
            pVar->pNext = pIndex;   /* Set a new 6th argument */

            /* Remove the index expression from a string representation
             */
            pVar = pParms->value.asList.pExprList->pNext;
            if( pVar->ExprType == ZH_ET_STRING )
            {
               ZH_SIZE i = 0;
               char * szVar = pVar->value.asString.string;

               /* NOTE: Clipper strips a string at the first '[' character too
                */
               while( ++i < pVar->nLength )
               {
                  if( szVar[ i ] == '[' )
                  {
                     if( ! pVar->value.asString.dealloc )
                     {
                        szVar = pVar->value.asString.string = ( char * )
                           zh_xmemdup( pVar->value.asString.string, i + 1 );
                        pVar->value.asString.dealloc = ZH_TRUE;
                     }
                     szVar[ i ] = 0;
                     pVar->nLength = i;
                     break;
                  }
               }
            }
            /* clear expressions no longer used */
            if( pBase )
            {
               while( pBase->ExprType == ZH_ET_ARRAYAT )
               {
                  pVar = pBase->value.asList.pExprList;
                  pBase->value.asList.pExprList = NULL;
                  ZH_COMP_EXPR_CLEAR( pBase );
                  pBase = pVar;
               }
            }
         }
         else if( pArg->ExprType == ZH_ET_MACRO ||
                  ( pArg->ExprType == ZH_ET_ALIASVAR &&
                    ( pArg->value.asAlias.pAlias->ExprType == ZH_ET_MACRO ||
                      pArg->value.asAlias.pVar->ExprType == ZH_ET_MACRO ) ) )
         {
            const char * szText = NULL;

            if( pArg->ExprType == ZH_ET_ALIASVAR )
            {
               const char * szAlias = NULL, * szAliasPref = "",
                          * szVar = NULL, * szVarPref = "";

               if( pArg->value.asAlias.pAlias->ExprType == ZH_ET_ALIAS )
                  szAlias = pArg->value.asAlias.pAlias->value.asSymbol.name;
               else if( pArg->value.asAlias.pAlias->ExprType == ZH_ET_MACRO &&
                        pArg->value.asAlias.pAlias->value.asMacro.pExprList == NULL )
               {
                  szAlias = pArg->value.asAlias.pAlias->value.asMacro.szMacro;
                  if( pArg->value.asAlias.pAlias->value.asMacro.cMacroOp == '&' )
                     szAliasPref = "&";
               }

               if( pArg->value.asAlias.pVar->ExprType == ZH_ET_VARIABLE )
                  szVar = pArg->value.asAlias.pVar->value.asSymbol.name;
               else if( pArg->value.asAlias.pVar->ExprType == ZH_ET_MACRO &&
                        pArg->value.asAlias.pVar->value.asMacro.pExprList == NULL )
               {
                  szVar = pArg->value.asAlias.pVar->value.asMacro.szMacro;
                  if( pArg->value.asAlias.pVar->value.asMacro.cMacroOp == '&' )
                     szVarPref = "&";
               }

               if( szAlias != NULL && szVar != NULL )
               {
                  if( pArg->pNext && pArg->pNext->ExprType == ZH_ET_STRING )
                     szText = "";
                  else
                     szText = zh_xstrcpy( NULL, szAliasPref, szAlias, "->", szVarPref, szVar, NULL );
               }
            }
            else if( pArg->value.asMacro.pExprList == NULL )
               /* Simple macro expansion (not a parenthesized expressions) */
               szText = pArg->value.asMacro.szMacro;

            pName->value.asSymbol.name = "__GET";
            if( szText == NULL )
            {
               /* @ 0,0 GET &(var)
                * @ 0,0 GET &(var)->var
                * @ 0,0 GET var->&(var)
                */
#if defined( ZH_MACRO_SUPPORT )
               zh_macroError( EG_SYNTAX, ZH_COMP_PARAM );
#else
               zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_GET_COMPLEX_MACRO, NULL, NULL );
#endif
            }
            else
            {
               /* @ 0,0 GET &var          => __Get( NIL, var,... )
                * @ 0,0 GET var&var       => __Get( NIL, "var&var",... )
                * @ 0,0 GET &var->var     => __Get( NIL, "&var->var",... )
                * @ 0,0 GET var->var&var  => __Get( NIL, "var->var&var",... )
                * @ 0,0 GET var&var->&var => __Get( NIL, "var&var->&var",... )
                */
               PZH_EXPR pFirst = pArg; /* save first argument */

               pArg = zh_compExprNewNil( ZH_COMP_PARAM ); /* replace 1st with NIL */
               if( pFirst->pNext && pFirst->pNext->ExprType == ZH_ET_STRING )
                  pArg->pNext = pFirst->pNext;
               else
               {
                  if( pArg->ExprType == ZH_ET_ALIASVAR )
                     pArg->pNext = zh_compExprNewString( szText, strlen( szText ), ZH_TRUE, ZH_COMP_PARAM );
                  else if( pFirst->value.asMacro.cMacroOp == '&' )
                     /* simple &variable - replace the second argument with
                      * a variable name
                      */
                     pArg->pNext = zh_compExprNewVar( szText, ZH_COMP_PARAM );
                  else
                     /* text substitution text&variable - replace the second
                      * argument with a string
                      */
                     pArg->pNext = zh_compExprNewString( szText, strlen( szText ), ZH_FALSE, ZH_COMP_PARAM );

                  if( pFirst->pNext )
                  {
                     pArg->pNext->pNext = pFirst->pNext->pNext;
                     ZH_COMP_EXPR_FREE( pFirst->pNext ); /* delete a second argument */
                  }
               }
               ZH_COMP_EXPR_FREE( pFirst ); /* delete first argument */
               /* set an updated list of arguments */
               pParms->value.asList.pExprList = pArg;
            }
         }
         else
         {
            pName->value.asSymbol.name = "__GET";

            /* store second and a rest of arguments */
            pNext = pArg->pNext;
            pArg->pNext = NULL;
            /* replace first argument with a set/get codeblock */
#if ! defined( ZH_MACRO_SUPPORT )
            if( pArg->ExprType == ZH_ET_VARIABLE &&
                ! zh_compVariableFind( ZH_COMP_PARAM, pArg->value.asSymbol.name, NULL, NULL ) )
            {
               /* Undeclared variable name - create a set/get codeblock
                * at runtime
                */
               if( ZH_COMP_PARAM->iWarnings >= 2 )
                  zh_compGenWarning( ZH_COMP_PARAM, zh_comp_szWarnings, 'W', ZH_COMP_WARN_AMBIGUOUS_VAR, pArg->value.asSymbol.name, NULL );
               ZH_COMP_EXPR_FREE( pArg );
               pArg = zh_compExprNewNil( ZH_COMP_PARAM );
            }
            else
#endif
            {
               pArg = zh_compExprSetGetBlock( pArg, ZH_COMP_PARAM );
            }
            /* restore next arguments */
            pArg->pNext = pNext;
            /* set an updated list of arguments */
            pParms->value.asList.pExprList = pArg;
         }

         pName->value.asSymbol.name =
                              zh_compGetFuncID( pName->value.asSymbol.name,
                                                &pName->value.asSymbol.funcid,
                                                &pName->value.asSymbol.flags );
      }
   }
   else if( pName->ExprType == ZH_ET_MACRO )
   {
      /* Signal that macro compiler have to generate a pcode that will
       * return function name as symbol instead of usual value
       */
      pName->value.asMacro.SubType = ZH_ET_MACRO_SYMBOL;

      ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprNewFunCall(&)" ) );
   }

   pExpr = ZH_COMP_EXPR_NEW( ZH_ET_FUNCALL );
   pExpr->value.asFunCall.pParms = pParms;
   pExpr->value.asFunCall.pFunName = pName;

   return pExpr;
}

/* Creates new array access expression
 *    pArray[ pIndex ]
 * NOTE: In case of multiple indexes it is called recursively
 *    array[ idx1, idx2 ] => ( array[ idx1 ] )[ idx2 ]
 */
#ifdef ZH_MACRO_SUPPORT
PZH_EXPR zh_macroExprNewArrayAt( PZH_EXPR pArray, PZH_EXPR pIndex, ZH_COMP_DECL )
#else
PZH_EXPR zh_compExprNewArrayAt( PZH_EXPR pArray, PZH_EXPR pIndex, ZH_COMP_DECL )
#endif
{
   PZH_EXPR pExpr;

#ifdef ZH_MACRO_SUPPORT
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_macroExprNewArrayAt()" ) );
#else
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprNewArrayAt()" ) );
#endif

   pExpr = ZH_COMP_EXPR_NEW( ZH_ET_ARRAYAT );

   /* Check if this expression can be indexed */
   if( ! ZH_SUPPORT_ARRSTR )
      ZH_EXPR_USE( pArray, ZH_EA_ARRAY_AT );
   /* Check if this expression can be an index */
   ZH_EXPR_USE( pIndex, ZH_EA_ARRAY_INDEX );
   pExpr->value.asList.pExprList = pArray;
   pExpr->value.asList.pIndex = pIndex;
   pExpr->value.asList.reference = ZH_FALSE;

   return pExpr;
}

/* === */

#ifndef ZH_MACRO_SUPPORT

/* List of functions which can be used as static initializers */
static const char * s_szStaticFun[] = {
   "ZH_MUTEXCREATE",
   "__BREAKBLOCK"
};

static ZH_BOOL zh_compStaticFunction( const char * szName )
{
   unsigned int ui;

   for( ui = 0; ui < ZH_SIZEOFARRAY( s_szStaticFun ); ++ui )
   {
      if( strcmp( szName, s_szStaticFun[ ui ] ) == 0 )
         return ZH_TRUE;
   }
   return ZH_FALSE;
}


static void zh_compExprCheckStaticInitializer( PZH_EXPR pLeftExpr, PZH_EXPR pRightExpr, ZH_COMP_DECL )
{
   if( ( pRightExpr->ExprType > ZH_ET_FUNREF ||
         pRightExpr->ExprType == ZH_ET_SELF ) &&
       ! ( pRightExpr->ExprType == ZH_ET_FUNCALL &&
           pRightExpr->value.asFunCall.pFunName->ExprType == ZH_ET_FUNNAME &&
           zh_compStaticFunction( pRightExpr->value.asFunCall.pFunName->
                                  value.asSymbol.name ) &&
           zh_compExprParamListLen( pRightExpr->value.asFunCall.pParms ) == 0 ) )
   {
      /* Illegal initializer for static variable (not a constant value)
       */
      zh_compErrorStatic( ZH_COMP_PARAM, pLeftExpr->value.asSymbol.name, pRightExpr );
   }
}

static void zh_compExprCheckStaticListInitializers( PZH_EXPR pLeftExpr, PZH_EXPR pRightExpr, ZH_COMP_DECL )
{
   PZH_EXPR * pExpr = &pRightExpr->value.asList.pExprList;

   while( *pExpr )
   {
      
      if( ( *pExpr )->ExprType == ZH_ET_ARRAY ||
          ( *pExpr )->ExprType == ZH_ET_HASH )
      {
         zh_compExprCheckStaticListInitializers( pLeftExpr, *pExpr, ZH_COMP_PARAM );
      }
      else
      {
         zh_compExprCheckStaticInitializer( pLeftExpr, *pExpr, ZH_COMP_PARAM );
      }
      pExpr = &( *pExpr )->pNext;
   }
}

/* It initializes static variable.
 *    It is called in the following context:
 * STATIC sVar := expression
 *
 * pLeftExpr - is a variable name
 * pRightExpr - can be an expression of any type
 */
PZH_EXPR zh_compExprAssignStatic( PZH_EXPR pLeftExpr, PZH_EXPR pRightExpr, ZH_COMP_DECL )
{
   PZH_EXPR pExpr;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprAssignStatic()" ) );

   pExpr = ZH_COMP_EXPR_NEW( ZH_EO_ASSIGN );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   /* Try to reduce the assigned value */
   pRightExpr = ZH_EXPR_USE( pRightExpr, ZH_EA_REDUCE );
   
   pExpr->value.asOperator.pRight = pRightExpr;

   if( pRightExpr->ExprType == ZH_ET_ARGLIST )
   {
      /* ZH_ET_ARGLIST is used in case of STATIC var[dim1, dim2, dimN]
       * was used - we have to check if all array dimensions are
       * constant values
       */
      zh_compExprCheckStaticListInitializers( pLeftExpr, pRightExpr, ZH_COMP_PARAM );
   }
   else if( pRightExpr->ExprType == ZH_ET_ARRAY )
   {
      /* { elem1, elem2, elemN } was used as initializer
       * Scan an array for illegal initializers.
       * An array item have to be a const value too.
       */
      zh_compExprCheckStaticListInitializers( pLeftExpr, pRightExpr, ZH_COMP_PARAM );
   }
   else if( pRightExpr->ExprType == ZH_ET_HASH )
   {
      /* { idx1=>var1, idx2=>var2, idxN=>varN } was used as initializer
       * Scan a hash array for illegal initializers.
       * A hash item have to be a const value too.
       */
      zh_compExprCheckStaticListInitializers( pLeftExpr, pRightExpr, ZH_COMP_PARAM );
   }
   else
   {
      zh_compExprCheckStaticInitializer( pLeftExpr, pRightExpr, ZH_COMP_PARAM );
   }

   return pExpr;
}

PZH_EXPR zh_compExprSetCodeblockBody( PZH_EXPR pExpr, ZH_BYTE * pCode, ZH_SIZE nLen )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprSetCodeblockBody(%p,%p,%" ZH_PFS "u)", ( void * ) pExpr, ( void * ) pCode, nLen ) );

   pExpr->value.asCodeblock.string = ( char * ) zh_xgrab( nLen + 1 );
   memcpy( pExpr->value.asCodeblock.string, pCode, nLen );
   pExpr->value.asCodeblock.string[ nLen ] = '\0';
   pExpr->nLength = nLen;

   return pExpr;
}
#endif

/* ************************************************************************* */

#if defined( ZH_MACRO_SUPPORT )

/* Generates pcode to push an expressions
 * NOTE: It pushes a value on the stack and leaves this value on the stack
 */
PZH_EXPR zh_macroExprGenPush( PZH_EXPR pExpr, ZH_COMP_DECL )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_macroExprGenPush(%i)", pExpr->ExprType ) );

   pExpr = ZH_EXPR_USE( pExpr, ZH_EA_REDUCE );
   return ZH_EXPR_USE( pExpr, ZH_EA_PUSH_PCODE );
}

/* Generates pcode to pop an expressions
 */
PZH_EXPR zh_macroExprGenPop( PZH_EXPR pExpr, ZH_COMP_DECL )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_macroExprGenPop(%i)", pExpr->ExprType ) );

   pExpr = ZH_EXPR_USE( pExpr, ZH_EA_REDUCE );
   ZH_EXPR_USE( pExpr, ZH_EA_LVALUE );
   return ZH_EXPR_USE( pExpr, ZH_EA_POP_PCODE );
}

#else

/* Generates pcode to push an expressions
 * NOTE: It pushes a value on the stack and leaves this value on the stack
 */
PZH_EXPR zh_compExprGenPush( PZH_EXPR pExpr, ZH_COMP_DECL )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprGenPush(%i)", pExpr->ExprType ) );

   pExpr = ZH_EXPR_USE( pExpr, ZH_EA_REDUCE );
   return ZH_EXPR_USE( pExpr, ZH_EA_PUSH_PCODE );
}

/* Generates pcode to pop an expressions
 */
PZH_EXPR zh_compExprGenPop( PZH_EXPR pExpr, ZH_COMP_DECL )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprGenPop(%i)", pExpr->ExprType ) );

   ZH_EXPR_USE( pExpr, ZH_EA_LVALUE );
   return ZH_EXPR_USE( pExpr, ZH_EA_POP_PCODE );
}

/* Generates pcode for inline expression used as a statement
 * NOTE: It doesn't not leave any value on the eval stack
 */
PZH_EXPR zh_compExprGenStatement( PZH_EXPR pExpr, ZH_COMP_DECL )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprGenStatement(%p)", ( void * ) pExpr ) );
   if( pExpr )
   {
      if( pExpr->ExprType == ZH_EO_EQUAL )
      {
         /* NOTE: direct type change */
         pExpr->ExprType = ZH_EO_ASSIGN;
      }

      pExpr = ZH_EXPR_USE( pExpr, ZH_EA_REDUCE );
      ZH_EXPR_USE( pExpr, ZH_EA_STATEMENT );
   }
   return pExpr;
}

PZH_EXPR zh_compExprReduce( PZH_EXPR pExpr, ZH_COMP_DECL )
{
   return ZH_EXPR_USE( pExpr, ZH_EA_REDUCE );
}
#endif
