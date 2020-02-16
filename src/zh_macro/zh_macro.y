%pure-parser
%parse-param { PZH_MACRO pMacro }
%lex-param   { PZH_MACRO pMacro }
%name-prefix "zh_macro_yy"

%{
/*
 * Macro compiler YACC rules and actions
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

/* TODO list
 * 1) Change the pcode generated by ::cVar from Self:cVar to QSELF():cVar
 *    The major problem to solve is how to support QSELF() inside a codeblock.
 */

/* this #define HAVE TO be placed before all #include directives
 */
#define  ZH_MACRO_SUPPORT

#include "zh_macro.h"
#include "zh_comp.h"
#include "zh_date.h"
#include "zh_pp.h"

/* Compile using: bison -d -p zh_comp macro.y */

#if defined( _MSC_VER )
#  pragma warning( disable : 4244 )
#  pragma warning( disable : 4702 )
#endif

#undef alloca
#define alloca  zh_xgrab
#undef malloc
#define malloc  zh_xgrab
#undef realloc
#define realloc zh_xrealloc
#undef free
#define free    zh_xfree

/* NOTE: these symbols are used internally in bison.simple
 */
#undef YYFREE
#define YYFREE zh_xfree
#undef YYMALLOC
#define YYMALLOC zh_xgrab

#define NO_YYERROR

/* NOTE: these symbols are defined explicitly to pacify warnings */
#define YYENABLE_NLS          0
#define YYLTYPE_IS_TRIVIAL    0


/* yacc/lex related definitions
 */


/* Standard checking for valid expression creation
 */
#define ZH_MACRO_CHECK( pExpr ) \
   if( ! ( ZH_MACRO_DATA->status & ZH_MACRO_CONT ) ) \
   { \
      YYABORT; \
   }

#define ZH_MACRO_IFENABLED( pSet, pExpr, flag ) \
   if( ZH_MACRO_DATA->supported & ( flag ) ) \
   { \
      pSet = ( pExpr ); \
   }\
   else \
   { \
      YYABORT; \
   }

#if defined( __BORLANDC__ ) || defined( __WATCOMC__ )
/* The if() inside this macro is always TRUE but it's used to hide BCC warning */
#define ZH_MACRO_ABORT if( !( ZH_MACRO_DATA->status & ZH_MACRO_CONT ) ) { YYABORT; }
#else
#define ZH_MACRO_ABORT { YYABORT; }
#endif

%}

%union                  /* special structure used by lex and yacc to share info */
{
   const char * string; /* to hold a string returned by lex */
   int       iNumber;   /* to hold a temporary integer number */
   ZH_MAXINT lNumber;   /* to hold a temporary long number */
   void *    pVoid;     /* to hold any memory structure we may need */
   PZH_EXPR  asExpr;
   struct
   {
      const char * string;
      ZH_SIZE      length;
   } valChar;
   struct
   {
      int      iNumber; /* to hold a number returned by lex */
   } valInteger;
   struct
   {
      ZH_MAXINT lNumber; /* to hold a long number returned by lex */
      ZH_UCHAR  bWidth;  /* to hold the width of the value */
   } valLong;
   struct
   {
      double   dNumber; /* to hold a double number returned by lex */
      ZH_UCHAR bWidth;  /* to hold the width of the value */
      ZH_UCHAR bDec;    /* to hold the number of decimal points in the value */
   } valDouble;
   struct
   {
      long     date;    /* to hold Julian date */
      long     time;    /* to hold milliseconds */
   } valTimeStamp;
}

%{
/* This must be placed after the above union - the union is
 * typedef-ined to YYSTYPE
 */
extern int  yylex( YYSTYPE *, PZH_MACRO );   /* main lex token function, called by yyparse() */
extern int  yyparse( PZH_MACRO );            /* main yacc parsing function */
extern void yyerror( PZH_MACRO, const char * );    /* parsing error management function */

%}

%token IDENTIFIER NIL NUM_DOUBLE INASSIGN NUM_LONG NUM_DATE TIMESTAMP
%token IIF LITERAL TRUEVALUE FALSEVALUE
%token AND OR NOT EQ NE1 NE2 INC DEC ALIASOP HASHOP SELF
%token LE GE FIELD MACROVAR MACROTEXT
%token PLUSEQ MINUSEQ MULTEQ DIVEQ POWER EXPEQ MODEQ
%token EPSILON

/*the lowest precedence*/
/*post-increment and post-decrement*/
%left   POST
/*assignment - from right to left*/
%right  INASSIGN
%right  PLUSEQ MINUSEQ
%right  MULTEQ DIVEQ MODEQ
%right  EXPEQ
/*logical operators*/
%right  OR
%right  AND
%right  NOT
/*relational operators*/
%right '=' EQ NE1 NE2
%right '<' '>' LE GE '$'
/*mathematical operators*/
%right  '+' '-'
%right  '*' '/' '%'
%right  POWER
%right  UNARY
/*pre-increment and pre-decrement*/
%right  PRE
/*special operators*/
%right  ALIASOP '&' '@'
%right  ','
/*the highest precedence*/

%type <string>    IDENTIFIER MACROVAR MACROTEXT
%type <valChar>   LITERAL
%type <valDouble> NUM_DOUBLE
%type <valLong>   NUM_LONG
%type <valLong>   NUM_DATE
%type <valTimeStamp> TIMESTAMP
%type <asExpr>  Argument ExtArgument RefArgument ArgList ElemList
%type <asExpr>  BlockExpList BlockVarList BlockVars
%type <asExpr>  NumValue NumAlias
%type <asExpr>  NilValue
%type <asExpr>  LiteralValue
%type <asExpr>  CodeBlock
%type <asExpr>  Logical
%type <asExpr>  SelfValue
%type <asExpr>  Array
%type <asExpr>  ArrayAt
%type <asExpr>  Hash HashList
%type <asExpr>  Variable VarAlias
%type <asExpr>  MacroVar MacroVarAlias
%type <asExpr>  MacroExpr MacroExprAlias
%type <asExpr>  AliasId AliasVar AliasExpr
%type <asExpr>  VariableAt
%type <asExpr>  FunCall FunRef
%type <asExpr>  SendId
%type <asExpr>  ObjectData
%type <asExpr>  ObjectMethod
%type <asExpr>  IfInline
%type <asExpr>  ExpList PareExpList PareExpListAlias AsParamList RootParamList
%type <asExpr>  Expression ExtExpression SimpleExpression LeftExpression
%type <asExpr>  EmptyExpression
%type <asExpr>  ExprAssign ExprOperEq ExprPreOp ExprPostOp
%type <asExpr>  ExprMath ExprBool ExprRelation ExprUnary
%type <asExpr>  ExprPlusEq ExprMinusEq ExprMultEq ExprDivEq ExprModEq ExprExpEq
%type <asExpr>  ArrayIndex IndexList
%type <asExpr>  FieldAlias FieldVarAlias
%type <asExpr>  PostOp
%type <asExpr>  DateValue TimeStampValue

%%

Main : Expression       {
                           ZH_MACRO_DATA->exprType = zh_compExprType( $1 );
                           if( ZH_MACRO_DATA->Flags &  ZH_MACRO_GEN_REFER )
                              zh_macroExprGenPush( zh_compExprNewRef( $1, ZH_COMP_PARAM ), ZH_COMP_PARAM );
                           else if( ZH_MACRO_DATA->Flags &  ZH_MACRO_GEN_PUSH )
                              zh_macroExprGenPush( $1, ZH_COMP_PARAM );
                           else
                              zh_macroExprGenPop( $1, ZH_COMP_PARAM );
                           zh_macroGenPCode1( ZH_P_ENDPROC, ZH_COMP_PARAM );
                        }
     | AsParamList      {
                           ZH_MACRO_DATA->exprType = zh_compExprType( $1 );
                           if( ZH_MACRO_DATA->Flags &  ZH_MACRO_GEN_PUSH )
                              zh_macroExprGenPush( $1, ZH_COMP_PARAM );
                           else
                              zh_macroError( EG_SYNTAX, ZH_COMP_PARAM );
                           zh_macroGenPCode1( ZH_P_ENDPROC, ZH_COMP_PARAM );
                        }
     | error   {
                  ZH_TRACE( ZH_TR_DEBUG, ( "macro -> invalid syntax: %s", ZH_MACRO_DATA->string ) );
                  zh_macroError( EG_SYNTAX, ZH_COMP_PARAM );
                  ZH_MACRO_ABORT;
               }
     ;

/* Numeric values
 */
NumValue   : NUM_DOUBLE       { $$ = zh_compExprNewDouble( $1.dNumber, $1.bWidth, $1.bDec, ZH_COMP_PARAM ); }
           | NUM_LONG         { $$ = zh_compExprNewLong( $1.lNumber, ZH_COMP_PARAM ); }
           ;

DateValue  : NUM_DATE         { $$ = zh_compExprNewDate( ( long ) $1.lNumber, ZH_COMP_PARAM ); }
           ;

TimeStampValue : TIMESTAMP    { $$ = zh_compExprNewTimeStamp( $1.date, $1.time, ZH_COMP_PARAM ); }
               ;

NumAlias   : NUM_LONG ALIASOP { $$ = zh_compExprNewLong( $1.lNumber, ZH_COMP_PARAM ); }
           ;

/* NIL value
 */
NilValue   : NIL              { $$ = zh_compExprNewNil( ZH_COMP_PARAM ); }
           ;

/* Literal string value
 */
LiteralValue : LITERAL        { $$ = zh_compExprNewString( $1.string, $1.length, ZH_FALSE, ZH_COMP_PARAM ); }
             ;

/* Logical value
 */
Logical     : TRUEVALUE       { $$ = zh_compExprNewLogical( ZH_TRUE, ZH_COMP_PARAM ); }
            | FALSEVALUE      { $$ = zh_compExprNewLogical( ZH_FALSE, ZH_COMP_PARAM ); }
            ;

/* SELF value and expressions
 */
SelfValue   : SELF            { $$ = zh_compExprNewSelf( ZH_COMP_PARAM ); }
            ;

/* Literal array
 */
Array       : '{' ElemList '}'      { $$ = zh_compExprNewArray( $2, ZH_COMP_PARAM ); }
            ;

/* Literal array access
 */
ArrayAt     : Array ArrayIndex      { $$ = $2; }
            ;

/* Literal hash
 */
Hash        : '{' HASHOP '}'        { $$ = zh_compExprNewHash( NULL, ZH_COMP_PARAM ); }
            | '{' HashList '}'      { $$ = zh_compExprNewHash( $2, ZH_COMP_PARAM ); }
            ;

HashList    : Expression HASHOP EmptyExpression                { $$ = zh_compExprAddListExpr( zh_compExprNewList( $1, ZH_COMP_PARAM ), $3 ); }
            | HashList ',' Expression HASHOP EmptyExpression   { $$ = zh_compExprAddListExpr( zh_compExprAddListExpr( $1, $3 ), $5 ); }
            ;


/* Variables
 */
Variable    : IDENTIFIER            { $$ = zh_compExprNewVar( $1, ZH_COMP_PARAM ); }
            ;

VarAlias    : IDENTIFIER ALIASOP    { $$ = zh_compExprNewAlias( $1, ZH_COMP_PARAM ); }
            ;

/* Macro variables - this can signal compilation errors
 */
MacroVar    : MACROVAR        {  $$ = zh_compExprNewMacro( NULL, '&', $1, ZH_COMP_PARAM );
                                 ZH_MACRO_CHECK( $$ );
                              }
            | MACROTEXT       {  ZH_BOOL fNewString;
                                 char * szVarName = zh_macroTextSymbol( $1, strlen( $1 ), &fNewString );
                                 if( szVarName )
                                 {
                                    if( fNewString )
                                       zh_macroIdentNew( ZH_COMP_PARAM, szVarName );
                                    $$ = zh_compExprNewVar( szVarName, ZH_COMP_PARAM );
                                    ZH_MACRO_CHECK( $$ );
                                 }
                                 else
                                 {
                                    /* invalid variable name
                                     */
                                    ZH_TRACE( ZH_TR_DEBUG, ( "macro -> invalid variable name: %s", $1 ) );
                                    YYABORT;
                                 }
                              }
            ;

MacroVarAlias  : MacroVar ALIASOP   { zh_compExprMacroAsAlias( $1 ); }
               ;

/* Macro expressions
 */
MacroExpr  : '&' PareExpList        { $$ = zh_compExprNewMacro( $2, 0, NULL, ZH_COMP_PARAM ); }
           ;

MacroExprAlias : MacroExpr ALIASOP
               ;

/* Aliased variables
 */
/* special case: _FIELD-> and FIELD-> can be nested
 */
FieldAlias  : FIELD ALIASOP               { $$ = zh_compExprNewAlias( "FIELD", ZH_COMP_PARAM ); }
            | FIELD ALIASOP FieldAlias    { $$ = $3; }
            ;

/* ignore _FIELD-> or FIELD-> if a real alias is specified
 */
FieldVarAlias  : FieldAlias VarAlias            { $$ = $2; }
               | FieldAlias NumAlias            { $$ = $2; }
               | FieldAlias PareExpListAlias    { $$ = $2; }
               | FieldAlias MacroVarAlias       { $$ = $2; }
               | FieldAlias MacroExprAlias      { $$ = $2; }
               ;

AliasId     : IDENTIFIER      { $$ = zh_compExprNewVar( $1, ZH_COMP_PARAM ); }
            | MacroVar
            | MacroExpr
            ;

AliasVar   : NumAlias AliasId          { $$ = zh_compExprNewAliasVar( $1, $2, ZH_COMP_PARAM ); }
           | MacroVarAlias AliasId     { $$ = zh_compExprNewAliasVar( $1, $2, ZH_COMP_PARAM ); }
           | MacroExprAlias AliasId    { $$ = zh_compExprNewAliasVar( $1, $2, ZH_COMP_PARAM ); }
           | PareExpListAlias AliasId  { $$ = zh_compExprNewAliasVar( $1, $2, ZH_COMP_PARAM ); }
           | VarAlias AliasId          { $$ = zh_compExprNewAliasVar( $1, $2, ZH_COMP_PARAM ); }
           | FieldAlias AliasId        { $$ = zh_compExprNewAliasVar( $1, $2, ZH_COMP_PARAM ); }
           | FieldVarAlias AliasId     { $$ = zh_compExprNewAliasVar( $1, $2, ZH_COMP_PARAM ); }
           ;

/* Aliased expressions
 */
/* NOTE: In the case:
 * alias->( Expression )
 * alias always selects a workarea at runtime
 */
AliasExpr  : NumAlias PareExpList         { $$ = zh_compExprNewAliasExpr( $1, $2, ZH_COMP_PARAM ); }
           | VarAlias PareExpList         { $$ = zh_compExprNewAliasExpr( $1, $2, ZH_COMP_PARAM ); }
           | MacroVarAlias PareExpList    { $$ = zh_compExprNewAliasExpr( $1, $2, ZH_COMP_PARAM ); }
           | MacroExprAlias PareExpList   { $$ = zh_compExprNewAliasExpr( $1, $2, ZH_COMP_PARAM ); }
           | PareExpListAlias PareExpList { $$ = zh_compExprNewAliasExpr( $1, $2, ZH_COMP_PARAM ); }
           ;

/* Array expressions access
 */
VariableAt  : NumValue        ArrayIndex  { $$ = $2; }
            | NilValue        ArrayIndex  { $$ = $2; }
            | DateValue       ArrayIndex  { $$ = $2; }
            | TimeStampValue  ArrayIndex  { $$ = $2; }
            | LiteralValue    ArrayIndex  { $$ = $2; }
            | CodeBlock       ArrayIndex  { $$ = $2; }
            | Logical         ArrayIndex  { $$ = $2; }
            | Hash            ArrayIndex  { $$ = $2; }
            | SelfValue       ArrayIndex  { $$ = $2; }
            | Variable        ArrayIndex  { $$ = $2; }
            | AliasVar        ArrayIndex  { $$ = $2; }
            | AliasExpr       ArrayIndex  { $$ = $2; }
            | MacroVar        ArrayIndex  { $$ = $2; }
            | MacroExpr       ArrayIndex  { $$ = $2; }
            | ObjectData      ArrayIndex  { $$ = $2; }
            | ObjectMethod    ArrayIndex  { $$ = $2; }
            | FunCall         ArrayIndex  { $$ = $2; }
            | IfInline        ArrayIndex  { $$ = $2; }
            | PareExpList     ArrayIndex  { $$ = $2; }
            ;

/* Function call
 */
FunCall     : IDENTIFIER '(' ArgList ')'  { $$ = zh_macroExprNewFunCall( zh_compExprNewFunName( $1, ZH_COMP_PARAM ), $3, ZH_COMP_PARAM );
                                            ZH_MACRO_CHECK( $$ );
                                          }
            | MacroVar '(' ArgList ')'    { $$ = zh_macroExprNewFunCall( $1, $3, ZH_COMP_PARAM );
                                            ZH_MACRO_CHECK( $$ );
                                          }
            ;

FunRef      : '@' IDENTIFIER '(' ArgList ')' {  if( zh_compExprParamListLen( $4 ) != 0 )
                                                {
                                                   zh_macroError( EG_SYNTAX, ZH_COMP_PARAM );
                                                   YYABORT;
                                                }
                                                else
                                                   $$ = zh_compExprNewFunRef( $2, ZH_COMP_PARAM );
                                             }
            ;

ArgList     : ExtArgument                 { $$ = zh_compExprNewArgList( $1, ZH_COMP_PARAM ); }
            | ArgList ',' ExtArgument     { $$ = zh_compExprAddListExpr( $1, $3 ); }
            ;

Argument    : EmptyExpression
            | RefArgument
            ;

RefArgument : '@' IDENTIFIER              { $$ = zh_compExprNewVarRef( $2, ZH_COMP_PARAM ); }
            | '@' MacroVar                { $$ = zh_compExprNewRef( $2, ZH_COMP_PARAM ); }
            | '@' AliasVar                { $$ = zh_compExprNewRef( $2, ZH_COMP_PARAM ); }
            | '@' ObjectData              { $$ = zh_compExprNewRef( $2, ZH_COMP_PARAM ); }
            | '@' VariableAt              { $$ = $2; $$->value.asList.reference = ZH_TRUE; }
            ;

ExtArgument : EPSILON  { $$ = zh_compExprNewArgRef( ZH_COMP_PARAM ); }
            | Argument
            ;

/* Object's instance variable
 */
ObjectData  : LeftExpression ':' SendId   { $$ = zh_compExprNewMethodObject( $3, $1 ); }
            ;

SendId      : IDENTIFIER     { $$ = zh_compExprNewSend( $1, ZH_COMP_PARAM ); }
            | MacroVar       { $$ = zh_compExprNewMacroSend( $1, ZH_COMP_PARAM ); }
            | MacroExpr      { $$ = zh_compExprNewMacroSend( $1, ZH_COMP_PARAM ); }
            ;

/* Object's method
 */
ObjectMethod : ObjectData '(' ArgList ')'    { $$ = zh_compExprNewMethodCall( $1, $3 ); }
             ;

SimpleExpression :
              NumValue
            | NilValue
            | DateValue
            | TimeStampValue
            | LiteralValue
            | CodeBlock
            | Logical
            | SelfValue
            | Array
            | ArrayAt
            | Hash
            | AliasVar
            | AliasExpr
            | MacroVar
            | MacroExpr
            | Variable
            | VariableAt
            | FunCall
            | IfInline
            | ObjectData
            | ObjectMethod
            | ExprAssign
            | ExprOperEq            { ZH_MACRO_IFENABLED( $$, $1, ZH_SM_ZIHER ); }
            | ExprPostOp            { ZH_MACRO_IFENABLED( $$, $1, ZH_SM_ZIHER ); }
            | ExprPreOp             { ZH_MACRO_IFENABLED( $$, $1, ZH_SM_ZIHER ); }
            | ExprUnary
            | ExprMath
            | ExprBool
            | ExprRelation
            | FunRef
            ;

Expression  : SimpleExpression      { $$ = $1; ZH_MACRO_CHECK( $$ ); }
            | PareExpList           { $$ = $1; ZH_MACRO_CHECK( $$ ); }
            ;

ExtExpression : EPSILON             { $$ = zh_compExprNewArgRef( ZH_COMP_PARAM ); }
              | Expression
              ;

RootParamList : EmptyExpression ',' {
                                       if( !( ZH_MACRO_DATA->Flags & ZH_MACRO_GEN_LIST ) )
                                       {
                                          ZH_TRACE( ZH_TR_DEBUG, ( "macro -> invalid expression: %s", ZH_MACRO_DATA->string ) );
                                          zh_macroError( EG_SYNTAX, ZH_COMP_PARAM );
                                          YYABORT;
                                       }
                                    }
                EmptyExpression     {
                                       ZH_MACRO_DATA->uiListElements = 1;
                                       $$ = zh_compExprAddListExpr( ( ZH_MACRO_DATA->Flags & ZH_MACRO_GEN_PARE ) ? zh_compExprNewList( $1, ZH_COMP_PARAM ) : zh_compExprNewArgList( $1, ZH_COMP_PARAM ), $4 );
                                    }
              ;

AsParamList : RootParamList
            | AsParamList ',' EmptyExpression   { ZH_MACRO_DATA->uiListElements++;
                                                  $$ = zh_compExprAddListExpr( $1, $3 ); }
            ;

EmptyExpression: /* nothing => nil */        { $$ = zh_compExprNewEmpty( ZH_COMP_PARAM ); }
            | Expression
            ;

LeftExpression : NumValue
               | NilValue
               | DateValue
               | TimeStampValue
               | LiteralValue
               | CodeBlock
               | Logical
               | SelfValue
               | Array
               | ArrayAt
               | Hash
               | AliasVar
               | AliasExpr
               | MacroVar
               | MacroExpr
               | Variable
               | VariableAt
               | PareExpList
               | FunCall
               | IfInline
               | ObjectData         { ZH_MACRO_IFENABLED( $$, $1, ZH_SM_ZIHER ); }
               | ObjectMethod
               ;

/* NOTE: PostOp can be used in one context only - it uses $0 rule
 *    (the rule that stands before PostOp)
 */
PostOp      : INC    { $$ = zh_compExprNewPostInc( $<asExpr>0, ZH_COMP_PARAM ); }
            | DEC    { $$ = zh_compExprNewPostDec( $<asExpr>0, ZH_COMP_PARAM ); }
            ;

/* NOTE: We cannot use 'Expression PostOp' because it caused
 * shift/reduce conflicts
 */
ExprPostOp  : LeftExpression PostOp %prec POST  { $$ = $2; }
            ;

ExprPreOp   : INC Expression  %prec PRE      { $$ = zh_compExprNewPreInc( $2, ZH_COMP_PARAM ); }
            | DEC Expression  %prec PRE      { $$ = zh_compExprNewPreDec( $2, ZH_COMP_PARAM ); }
            ;

ExprUnary   : NOT Expression                 { $$ = zh_compExprNewNot( $2, ZH_COMP_PARAM ); }
            | '-' Expression  %prec UNARY    { $$ = zh_compExprNewNegate( $2, ZH_COMP_PARAM ); }
            | '+' Expression  %prec UNARY    { $$ = $2; }
            ;

ExprAssign  : LeftExpression INASSIGN Expression { $$ = zh_compExprAssign( $1, $3, ZH_COMP_PARAM ); }
            ;

ExprPlusEq  : LeftExpression PLUSEQ   Expression { $$ = zh_compExprSetOperand( zh_compExprNewPlusEq( $1, ZH_COMP_PARAM ), $3, ZH_COMP_PARAM ); }
            ;

ExprMinusEq : LeftExpression MINUSEQ  Expression { $$ = zh_compExprSetOperand( zh_compExprNewMinusEq( $1, ZH_COMP_PARAM ), $3, ZH_COMP_PARAM ); }
            ;

ExprMultEq  : LeftExpression MULTEQ   Expression { $$ = zh_compExprSetOperand( zh_compExprNewMultEq( $1, ZH_COMP_PARAM ), $3, ZH_COMP_PARAM ); }
            ;

ExprDivEq   : LeftExpression DIVEQ    Expression { $$ = zh_compExprSetOperand( zh_compExprNewDivEq( $1, ZH_COMP_PARAM ), $3, ZH_COMP_PARAM ); }
            ;

ExprModEq   : LeftExpression MODEQ    Expression { $$ = zh_compExprSetOperand( zh_compExprNewModEq( $1, ZH_COMP_PARAM ), $3, ZH_COMP_PARAM ); }
            ;

ExprExpEq   : LeftExpression EXPEQ    Expression { $$ = zh_compExprSetOperand( zh_compExprNewExpEq( $1, ZH_COMP_PARAM ), $3, ZH_COMP_PARAM ); }
            ;

ExprOperEq  : ExprPlusEq
            | ExprMinusEq
            | ExprMultEq
            | ExprDivEq
            | ExprModEq
            | ExprExpEq
            ;

ExprMath    : Expression '+' Expression     { $$ = zh_compExprSetOperand( zh_compExprNewPlus( $1, ZH_COMP_PARAM ), $3, ZH_COMP_PARAM ); }
            | Expression '-' Expression     { $$ = zh_compExprSetOperand( zh_compExprNewMinus( $1, ZH_COMP_PARAM ), $3, ZH_COMP_PARAM ); }
            | Expression '*' Expression     { $$ = zh_compExprSetOperand( zh_compExprNewMult( $1, ZH_COMP_PARAM ), $3, ZH_COMP_PARAM ); }
            | Expression '/' Expression     { $$ = zh_compExprSetOperand( zh_compExprNewDiv( $1, ZH_COMP_PARAM ), $3, ZH_COMP_PARAM ); }
            | Expression '%' Expression     { $$ = zh_compExprSetOperand( zh_compExprNewMod( $1, ZH_COMP_PARAM ), $3, ZH_COMP_PARAM ); }
            | Expression POWER Expression   { $$ = zh_compExprSetOperand( zh_compExprNewPower( $1, ZH_COMP_PARAM ), $3, ZH_COMP_PARAM ); }
            ;

ExprBool    : Expression AND Expression   { $$ = zh_compExprSetOperand( zh_compExprNewAnd( $1, ZH_COMP_PARAM ), $3, ZH_COMP_PARAM ); }
            | Expression OR  Expression   { $$ = zh_compExprSetOperand( zh_compExprNewOr( $1, ZH_COMP_PARAM ), $3, ZH_COMP_PARAM ); }
            ;

ExprRelation: Expression EQ  Expression   { $$ = zh_compExprSetOperand( zh_compExprNewEQ( $1, ZH_COMP_PARAM ), $3, ZH_COMP_PARAM ); }
            | Expression '<' Expression   { $$ = zh_compExprSetOperand( zh_compExprNewLT( $1, ZH_COMP_PARAM ), $3, ZH_COMP_PARAM ); }
            | Expression '>' Expression   { $$ = zh_compExprSetOperand( zh_compExprNewGT( $1, ZH_COMP_PARAM ), $3, ZH_COMP_PARAM ); }
            | Expression LE  Expression   { $$ = zh_compExprSetOperand( zh_compExprNewLE( $1, ZH_COMP_PARAM ), $3, ZH_COMP_PARAM ); }
            | Expression GE  Expression   { $$ = zh_compExprSetOperand( zh_compExprNewGE( $1, ZH_COMP_PARAM ), $3, ZH_COMP_PARAM ); }
            | Expression NE1 Expression   { $$ = zh_compExprSetOperand( zh_compExprNewNE( $1, ZH_COMP_PARAM ), $3, ZH_COMP_PARAM ); }
            | Expression NE2 Expression   { $$ = zh_compExprSetOperand( zh_compExprNewNE( $1, ZH_COMP_PARAM ), $3, ZH_COMP_PARAM ); }
            | Expression '$' Expression   { $$ = zh_compExprSetOperand( zh_compExprNewIN( $1, ZH_COMP_PARAM ), $3, ZH_COMP_PARAM ); }
            | Expression '=' Expression   { $$ = zh_compExprSetOperand( zh_compExprNewEqual( $1, ZH_COMP_PARAM ), $3, ZH_COMP_PARAM ); }
            ;

ArrayIndex  : IndexList ']'
            ;

/* NOTE: $0 represents the expression before ArrayIndex
 *    Don't use ArrayIndex in other context than as an array index!
 */
IndexList   : '[' ExtExpression                 { $$ = zh_macroExprNewArrayAt( $<asExpr>0, $2, ZH_COMP_PARAM ); }
            | IndexList ',' ExtExpression       { $$ = zh_macroExprNewArrayAt( $1, $3, ZH_COMP_PARAM ); }
            | IndexList ']' '[' ExtExpression   { $$ = zh_macroExprNewArrayAt( $1, $4, ZH_COMP_PARAM ); }
            ;

ElemList    : ExtArgument              { $$ = zh_compExprNewList( $1, ZH_COMP_PARAM ); }
            | ElemList ',' ExtArgument { $$ = zh_compExprAddListExpr( $1, $3 ); }
            ;

CodeBlock   : '{' '|'
                  { $<asExpr>$ = zh_compExprNewCodeBlock( NULL, 0, 0, ZH_COMP_PARAM ); }
               BlockVars '|' BlockExpList '}'
                  { $$ = $<asExpr>3; }
            ;

/* NOTE: This uses $-2 then don't use BlockExpList in other context
 */
BlockExpList: Expression                  { $$ = zh_compExprAddCodeblockExpr( $<asExpr>-2, $1 ); }
            | BlockExpList ',' Expression { $$ = zh_compExprAddCodeblockExpr( $<asExpr>-2, $3 ); }
            ;

/* NOTE: This uses $0 then don't use BlockVars and BlockVarList in other context
 */
BlockVars   : /* empty list */            { $$ = NULL; }
            | EPSILON                     { $$ = NULL; $<asExpr>0->value.asCodeblock.flags |= ZH_BLOCK_VPARAMS; }
            | BlockVarList                { $$ = $1;   }
            | BlockVarList ',' EPSILON    { $$ = $1;   $<asExpr>0->value.asCodeblock.flags |= ZH_BLOCK_VPARAMS; }
            ;

BlockVarList: IDENTIFIER                  { $$ = zh_compExprCBVarAdd( $<asExpr>0, $1, ' ', ZH_COMP_PARAM ); }
            | BlockVarList ',' IDENTIFIER { $$ = zh_compExprCBVarAdd( $<asExpr>0, $3, ' ', ZH_COMP_PARAM ); ZH_MACRO_CHECK( $$ ); }
            ;

ExpList     : '(' EmptyExpression          { $$ = zh_compExprNewList( $2, ZH_COMP_PARAM ); }
            | ExpList ',' EmptyExpression  { $$ = zh_compExprAddListExpr( $1, $3 ); }
            ;

PareExpList : ExpList ')'
            ;

PareExpListAlias : PareExpList ALIASOP
                 ;

/* Lexer should return IIF for "if" symbol */
IfInline    : IIF '(' Expression ',' Argument ',' Argument ')'
               { $$ = zh_compExprNewIIF( zh_compExprAddListExpr( zh_compExprAddListExpr( zh_compExprNewList( $3, ZH_COMP_PARAM ), $5 ), $7 ) ); }
            ;

%%


/*
 ** ------------------------------------------------------------------------ **
 */

void yyerror( PZH_MACRO pMacro, const char * s )
{
   ZH_SYMBOL_UNUSED( pMacro );
   ZH_SYMBOL_UNUSED( s );
}

/* ************************************************************************* */

#define ZH_MEXPR_PREALLOC 8

typedef struct ZH_MEXPR_
{
   int      count;
   ZH_EXPR  Expressions[ ZH_MEXPR_PREALLOC ];
   struct ZH_MEXPR_ *pPrev;
}
ZH_MEXPR, * PZH_MEXPR;

typedef struct ZH_MIDENT_
{
   char * Identifier;
   struct ZH_MIDENT_ * pPrev;
}
ZH_MIDENT, * PZH_MIDENT;

/* Allocates memory for Expression holder structure and stores it
 * on the linked list
*/
static PZH_EXPR zh_macroExprAlloc( ZH_COMP_DECL )
{
   PZH_MEXPR pMExpr = ( PZH_MEXPR ) ZH_MACRO_DATA->pExprLst;

   if( ! pMExpr || pMExpr->count >= ZH_MEXPR_PREALLOC )
   {
      pMExpr = ( PZH_MEXPR ) zh_xgrab( sizeof( ZH_MEXPR ) );
      pMExpr->pPrev = ( PZH_MEXPR ) ZH_MACRO_DATA->pExprLst;
      pMExpr->count = 0;
      ZH_MACRO_DATA->pExprLst = ( void * ) pMExpr;
   }
   return &pMExpr->Expressions[ pMExpr->count++ ];
}

char * zh_macroIdentNew( ZH_COMP_DECL, char * szIdent )
{
   PZH_MIDENT pMIdent = ( PZH_MIDENT ) zh_xgrab( sizeof( ZH_MIDENT ) );

   pMIdent->Identifier = szIdent;
   pMIdent->pPrev = ( PZH_MIDENT ) ZH_MACRO_DATA->pIdentLst;
   ZH_MACRO_DATA->pIdentLst = ( void * ) pMIdent;

   return szIdent;
}

static PZH_EXPR zh_macroExprNew( ZH_COMP_DECL, ZH_EXPRTYPE iType )
{
   PZH_EXPR pExpr;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_macroExprNew(%p,%i)", ( void * ) ZH_COMP_PARAM, iType ) );

   pExpr = zh_macroExprAlloc( ZH_COMP_PARAM );
   pExpr->ExprType = iType;
   pExpr->pNext    = NULL;
   pExpr->ValType  = ZH_EV_UNKNOWN;

   return pExpr;
}

/* Delete self - all components will be deleted somewhere else
 */
static void zh_macroExprClear( ZH_COMP_DECL, PZH_EXPR pExpr )
{
   ZH_SYMBOL_UNUSED( ZH_COMP_PARAM );

   pExpr->ExprType = ZH_ET_NONE;
}

/* Delete all components and delete self
 */
static void zh_macroExprFree( ZH_COMP_DECL, PZH_EXPR pExpr )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_macroExprFree()" ) );

   ZH_EXPR_USE( pExpr, ZH_EA_DELETE );
   pExpr->ExprType = ZH_ET_NONE;
}

/* Deallocate all memory used by expression optimizer */
static void zh_macroLstFree( PZH_MACRO pMacro )
{
   if( pMacro->pExprLst )
   {
      PZH_MEXPR pMExpr = ( PZH_MEXPR ) pMacro->pExprLst;
      do
      {
         while( pMExpr->count )
            zh_macroExprFree( pMacro, &pMExpr->Expressions[ --pMExpr->count ] );
         pMExpr = pMExpr->pPrev;
      }
      while( pMExpr );
      do
      {
         pMExpr = ( PZH_MEXPR ) pMacro->pExprLst;
         pMacro->pExprLst = ( void * ) pMExpr->pPrev;
         zh_xfree( pMExpr );
      }
      while( pMacro->pExprLst );
   }

   while( pMacro->pIdentLst )
   {
      PZH_MIDENT pMIdent = ( PZH_MIDENT ) ZH_MACRO_DATA->pIdentLst;
      ZH_MACRO_DATA->pIdentLst = ( void * ) pMIdent->pPrev;
      zh_xfree( pMIdent->Identifier );
      zh_xfree( pMIdent );
   }
}

static PZH_EXPR zh_macroErrorType( ZH_COMP_DECL, PZH_EXPR pExpr )
{
   zh_macroError( EG_ARG, ZH_COMP_PARAM );
   return pExpr;
}

static PZH_EXPR zh_macroErrorSyntax( ZH_COMP_DECL, PZH_EXPR pExpr )
{
   zh_macroError( EG_SYNTAX, ZH_COMP_PARAM );
   return pExpr;
}

static void zh_macroErrorDuplVar( ZH_COMP_DECL, const char * szVarName )
{
   ZH_SYMBOL_UNUSED( szVarName );
   zh_macroError( EG_SYNTAX, ZH_COMP_PARAM );
}


static const ZH_COMP_FUNCS s_macro_funcs =
{
   zh_macroExprNew,
   zh_macroExprClear,
   zh_macroExprFree,

   zh_macroErrorType,
   zh_macroErrorSyntax,
   zh_macroErrorDuplVar,
};

int zh_macroYYParse( PZH_MACRO pMacro )
{
   int iResult;

   pMacro->funcs = &s_macro_funcs;

   if( zh_macroLexNew( pMacro ) )
   {
      pMacro->status = ZH_MACRO_CONT;
      pMacro->pExprLst = NULL;
      pMacro->pIdentLst = NULL;

      iResult = yyparse( pMacro );

      zh_macroLstFree( pMacro );
      zh_macroLexDelete( pMacro );
   }
   else
      iResult = ZH_MACRO_FAILURE;

   return iResult;
}


#if defined( ZH_MACRO_PPLEX )

/* it's an example of PP token translator which change tokens generated by
   PP into terminal symbols used by our grammar parser generated by Bison */
ZH_BOOL zh_macroLexNew( PZH_MACRO pMacro )
{
   pMacro->pLex = ( void * ) zh_pp_lexNew( pMacro->string, pMacro->length );
   return pMacro->pLex != NULL;
}

void zh_macroLexDelete( PZH_MACRO pMacro )
{
   if( pMacro->pLex )
   {
      zh_pp_free( ( PZH_PP_STATE ) pMacro->pLex );
      pMacro->pLex = NULL;
   }
}

int zh_macro_yylex( YYSTYPE * yylval_ptr, PZH_MACRO pMacro )
{
   PZH_PP_TOKEN pToken = zh_pp_lexGet( ( PZH_PP_STATE ) pMacro->pLex );

   if( ! pToken )
      return 0;

   switch( ZH_PP_TOKEN_TYPE( pToken->type ) )
   {
      case ZH_PP_TOKEN_KEYWORD:
         if( pToken->len >= 4 && pToken->len <= 6 && pToken->pNext &&
             ZH_PP_TOKEN_TYPE( pToken->pNext->type ) == ZH_PP_TOKEN_ALIAS &&
             ( zh_strnicmp( "_FIELD", pToken->value, pToken->len ) == 0 ||
               zh_strnicmp( "FIELD", pToken->value, pToken->len ) == 0 ) )
         {
            return FIELD;
         }
         else if( pToken->len == 3 && pToken->pNext &&
                  ZH_PP_TOKEN_TYPE( pToken->pNext->type ) == ZH_PP_TOKEN_LEFT_PB &&
                  zh_stricmp( "IIF", pToken->value ) == 0 )
         {
            return IIF;
         }
         else if( pToken->len == 2 && pToken->pNext &&
                  ZH_PP_TOKEN_TYPE( pToken->pNext->type ) == ZH_PP_TOKEN_LEFT_PB &&
                  zh_stricmp( "IF", pToken->value ) == 0 )
            return IIF;
         else if( pToken->len == 3 && zh_stricmp( "NIL", pToken->value ) == 0 )
            return NIL;

         zh_pp_tokenUpper( pToken );
         yylval_ptr->string = pToken->value;
         return IDENTIFIER;

      case ZH_PP_TOKEN_MACROVAR:
         zh_pp_tokenUpper( pToken );
         yylval_ptr->string = pToken->value;
         return MACROVAR;

      case ZH_PP_TOKEN_MACROTEXT:
         zh_pp_tokenUpper( pToken );
         yylval_ptr->string = pToken->value;
         return MACROTEXT;

      case ZH_PP_TOKEN_NUMBER:
      {
         ZH_MAXINT lNumber;
         double dNumber;
         int iDec, iWidth;

         if( zh_compStrToNum( pToken->value, pToken->len, &lNumber, &dNumber, &iDec, &iWidth ) )
         {
            yylval_ptr->valDouble.dNumber = dNumber;
            yylval_ptr->valDouble.bDec    = ( ZH_UCHAR ) iDec;
            yylval_ptr->valDouble.bWidth  = ( ZH_UCHAR ) iWidth;
            return NUM_DOUBLE;
         }
         else
         {
            yylval_ptr->valLong.lNumber = lNumber;
            yylval_ptr->valLong.bWidth  = ( ZH_UCHAR ) iWidth;
            return NUM_LONG;
         }
      }
      case ZH_PP_TOKEN_DATE:
         if( pToken->len == 10 )
         {
            int year, month, day;
            zh_dateStrGet( pToken->value + 2, &year, &month, &day );
            yylval_ptr->valLong.lNumber = zh_dateEncode( year, month, day );
         }
         else
            yylval_ptr->valLong.lNumber = 0;
         return NUM_DATE;

      case ZH_PP_TOKEN_TIMESTAMP:
         if( ! zh_timeStampStrGetDT( pToken->value,
                                     &yylval_ptr->valTimeStamp.date,
                                     &yylval_ptr->valTimeStamp.time ) )
         {
            zh_macroError( EG_SYNTAX, pMacro );
         }
         return TIMESTAMP;

      case ZH_PP_TOKEN_STRING:
         yylval_ptr->valChar.string = pToken->value;
         yylval_ptr->valChar.length = pToken->len;
         return LITERAL;

      case ZH_PP_TOKEN_LOGICAL:
         return pToken->value[ 1 ] == 'T' ? TRUEVALUE : FALSEVALUE;

      case ZH_PP_TOKEN_HASH:
      case ZH_PP_TOKEN_DIRECTIVE:
         return NE1;

      case ZH_PP_TOKEN_NE:
         return NE2;

      case ZH_PP_TOKEN_ASSIGN:
         return INASSIGN;

      case ZH_PP_TOKEN_EQUAL:
         return EQ;

      case ZH_PP_TOKEN_INC:
         return INC;

      case ZH_PP_TOKEN_DEC:
         return DEC;

      case ZH_PP_TOKEN_ALIAS:
         return ALIASOP;

      case ZH_PP_TOKEN_LE:
         return LE;

      case ZH_PP_TOKEN_GE:
         return GE;

      case ZH_PP_TOKEN_PLUSEQ:
         return PLUSEQ;

      case ZH_PP_TOKEN_MINUSEQ:
         return MINUSEQ;

      case ZH_PP_TOKEN_MULTEQ:
         return MULTEQ;

      case ZH_PP_TOKEN_DIVEQ:
         return DIVEQ;

      case ZH_PP_TOKEN_MODEQ:
         return MODEQ;

      case ZH_PP_TOKEN_EXPEQ:
         return EXPEQ;

      case ZH_PP_TOKEN_POWER:
         return POWER;

      case ZH_PP_TOKEN_AND:
         return AND;

      case ZH_PP_TOKEN_OR:
         return OR;

      case ZH_PP_TOKEN_NOT:
         return NOT;

      default:
         return pToken->value[ 0 ];
   }
}

#endif /* ZH_MACRO_PPLEX */
