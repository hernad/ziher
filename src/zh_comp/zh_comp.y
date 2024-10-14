%define api.pure

%parse-param { PZH_COMP pComp }
%lex-param   { PZH_COMP pComp }

%name-prefix "zh_comp_yy"

%{
/*
 * Compiler YACC rules and actions
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 * (or visit their website at https://www.gnu.org/licenses/).
 *
 */



#include "zh_comp.h"

/* Compile using: bison -d -v ziher.y */

/* to pacify some meaningless warnings */
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

/* NOTE: increase the maximum size of bison stack size */
#define YYMAXDEPTH 100000

static void zh_compLoopStart( ZH_COMP_DECL, ZH_BOOL );
static void zh_compLoopEnd( ZH_COMP_DECL );
static void zh_compLoopLoop( ZH_COMP_DECL );
static void zh_compLoopExit( ZH_COMP_DECL );
static void zh_compLoopHere( ZH_COMP_DECL );
static long zh_compLoopCount( ZH_COMP_DECL );

static void * zh_compElseIfGen( ZH_COMP_DECL, void * pFirstElseIf, ZH_SIZE nOffset ); /* generates a support structure for ELSEIFs pcode fixups */
static void zh_compElseIfFix( ZH_COMP_DECL, void * pIfElseIfs ); /* implements the ELSEIFs pcode fixups */

static void zh_compRTVariableAdd( ZH_COMP_DECL, PZH_EXPR, ZH_BOOL );
static void zh_compRTVariableGen( ZH_COMP_DECL, const char * );

static PZH_EXPR zh_compArrayDimPush( PZH_EXPR pInitValue, ZH_COMP_DECL );
static void zh_compVariableDim( const char *, PZH_EXPR, ZH_COMP_DECL );

static void zh_compForStart( ZH_COMP_DECL, const char *szVarName, int iForEachDir );
static void zh_compForEnd( ZH_COMP_DECL, const char *szVarName );
static void zh_compEnumStart( ZH_COMP_DECL, PZH_EXPR pVars, PZH_EXPR pExprs, int descend );
static void zh_compEnumNext( ZH_COMP_DECL, PZH_EXPR pExpr, int descend );
static void zh_compEnumEnd( ZH_COMP_DECL, PZH_EXPR pExpr );

static void zh_compSwitchStart( ZH_COMP_DECL, PZH_EXPR );
static void zh_compSwitchAdd( ZH_COMP_DECL, PZH_EXPR );
static void zh_compSwitchEnd( ZH_COMP_DECL );

static PZH_EXPR zh_compCheckMethod( ZH_COMP_DECL, PZH_EXPR pExpr );
static PZH_EXPR zh_compCheckPassByRef( ZH_COMP_DECL, PZH_EXPR pExpr );

static void zh_compErrStru( ZH_COMP_DECL, int iError );
static void zh_compErrUnclosed( ZH_COMP_DECL, const char * szStru );

#ifdef ZH_YYDEBUG
   #define YYDEBUG        1 /* Parser debug information support */
#endif

/* Controls if passing by reference '@' is allowed */
#define ZH_PASSBYREF_OFF      0
#define ZH_PASSBYREF_FUNCALL  1
#define ZH_PASSBYREF_ARRAY    2

static void zh_compDebugStart( void ) { }

%}

%union                  /* special structure used by lex and yacc to share info */
{
   const char * string; /* to hold a string returned by lex */
   int     iNumber;     /* to hold a temporary integer number */
   ZH_SIZE sNumber;     /* to hold a temporary ZH_SIZE values */
   ZH_MAXINT lNumber;   /* to hold a temporary long number */
   ZH_BOOL bTrue;
   PZH_EXPR asExpr;
   void * pVoid;        /* to hold any memory structure we may need */
   struct
   {
      ZH_MAXINT lNumber;   /* to hold a long number returned by lex */
      ZH_UCHAR  bWidth;    /* to hold the width of the value */
   } valLong;
   struct
   {
      double   dNumber;    /* to hold a double number returned by lex */
      ZH_UCHAR bWidth;     /* to hold the width of the value */
      ZH_UCHAR bDec;       /* to hold the number of decimal points in the value */
   } valDouble;
   struct
   {
      long     date;
      long     time;
   } valTimeStamp;
   struct
   {
      char *   string;
      ZH_SIZE  length;
      ZH_BOOL  dealloc;
   } valChar;
   struct
   {
      char *   string;
      ZH_SIZE  length;
      int      flags;   /* Flag for early {|| &macro} (1) or late {|| &(macro)} (2) binding */
   } asCodeblock;
   PZH_VARTYPE asVarType;
}

%{
/* This must be placed after the above union - the union is
 * typedef-ined to YYSTYPE
 */
extern int  yylex( YYSTYPE *, ZH_COMP_DECL );    /* main lex token function, called by yyparse() */
extern void yyerror( ZH_COMP_DECL, const char * );     /* parsing error management function */
%}


%token FUNCTION PROCEDURE IDENTIFIER RETURN NIL
%token LOCAL STATIC IIF IF ELSE ELSEIF END ENDIF ENDERR
%token LITERAL TRUEVALUE FALSEVALUE NUM_DOUBLE INASSIGN NUM_LONG
%token ANNOUNCE EXTERN DYNAMIC AND OR NOT PUBLIC EQ NE1 NE2
%token INC DEC ALIASOP DOCASE CASE OTHERWISE ENDCASE ENDDO MEMVAR
%token WHILE LOOP EXIT INIT FOR NEXT TO STEP LE GE FIELD IN PARAMETERS
%token PLUSEQ MINUSEQ MULTEQ DIVEQ POWER EXPEQ MODEQ
%token PRIVATE BEGINSEQ BREAK RECOVER RECOVERUSING ALWAYS ENDSEQ
%token DO WITH SELF LINE
%token MACROVAR MACROTEXT
%token AS_ARRAY AS_BLOCK AS_CHARACTER AS_CLASS AS_DATE AS_LOGICAL AS_NUMERIC AS_OBJECT AS_VARIANT
%token AS_ARRAY_ARRAY AS_BLOCK_ARRAY AS_CHARACTER_ARRAY AS_CLASS_ARRAY AS_DATE_ARRAY AS_LOGICAL_ARRAY AS_NUMERIC_ARRAY AS_OBJECT_ARRAY
%token DECLARE OPTIONAL DECLARE_CLASS DECLARE_MEMBER
%token PROCREQ
%token CBSTART DOIDENT
%token FOREACH DESCEND
%token DOSWITCH ENDSWITCH WITHOBJECT ENDWITH
%token NUM_DATE TIMESTAMP
%token EPSILON
%token HASHOP
%token THREAD_STATIC

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
%right  '\n' ';' ','
/*the highest precedence*/

%type <string>  IdentName IDENTIFIER MACROVAR MACROTEXT CompTimeStr InAlias
%type <string>  DOIDENT WHILE
%type <valChar> LITERAL
%type <valDouble> NUM_DOUBLE
%type <valLong>   NUM_LONG
%type <valLong>   NUM_DATE
%type <valTimeStamp> TIMESTAMP
%type <iNumber> FUNCTION
%type <iNumber> PROCEDURE
%type <iNumber> Descend
%type <iNumber> Params ParamList
%type <iNumber> VarList ExtVarList
%type <iNumber> FieldList
%type <sNumber> IfBegin
%type <sNumber> WhileBegin
%type <sNumber> BlockSeq AlwaysSeq Always RecoverSeq RecoverEmpty RecoverUsing
%type <pVoid>   IfElseIf Cases
%type <asExpr>  Argument ExtArgument RefArgument ArgList ElemList
%type <asExpr>  BlockHead BlockExpList BlockVars BlockVarList
%type <asExpr>  DoProc DoArgs DoArgument DoArgList
%type <asExpr>  NumValue NumAlias
%type <asExpr>  NilValue NilAlias
%type <asExpr>  LiteralValue LiteralAlias
%type <asExpr>  CodeBlock CodeBlockAlias
%type <asExpr>  Logical LogicalAlias
%type <asExpr>  DateValue TimeStampValue
%type <asExpr>  SelfValue SelfAlias
%type <asExpr>  Array ArrayAlias
%type <asExpr>  ArrayAt ArrayAtAlias
%type <asExpr>  Hash HashList HashAlias
%type <asExpr>  Variable VarAlias
%type <asExpr>  MacroVar MacroVarAlias
%type <asExpr>  MacroExpr MacroExprAlias
%type <asExpr>  MacroAny
%type <asExpr>  AliasId AliasVar AliasExpr
%type <asExpr>  VariableAt VariableAtAlias
%type <asExpr>  FunIdentCall FunCall FunCallAlias FunRef
%type <asExpr>  ObjectData ObjectDataAlias ObjectRef
%type <asExpr>  ObjectMethod ObjectMethodAlias
%type <asExpr>  IfInline IfInlineAlias
%type <asExpr>  PareExpList PareExpListAlias ExpList
%type <asExpr>  Expression ExtExpression SimpleExpression LValue LeftExpression
%type <asExpr>  EmptyExpression
%type <asExpr>  ExprAssign ExprOperEq ExprPreOp ExprPostOp
%type <asExpr>  ExprEqual ExprMath ExprBool ExprRelation ExprUnary
%type <asExpr>  ExprPlusEq ExprMinusEq ExprMultEq ExprDivEq ExprModEq ExprExpEq
%type <asExpr>  ArrayIndex IndexList
%type <asExpr>  DimIndex DimList
%type <asExpr>  FieldAlias FieldVarAlias
%type <asExpr>  PostOp
%type <asExpr>  ForVar ForList ForExpr ForArgs
%type <asExpr>  SwitchStart SwitchBegin
%type <asCodeblock> CBSTART
%type <asExpr>  SendId
%type <asVarType> AsType StrongType AsArrayType AsArray

/*
   We cannot use destructors for expressions. The internal bison logic cannot
   detect properly if the expression was used or not in our grammar definition
   so it's possible that destructors will never be executed or executed for
   expressions which we freed ourselves.

%destructor {
               ZH_COMP_EXPR_FREE( $$ );
            }
            Argument ExtArgument ArgList ...
 */
%destructor { if( $$.string )  zh_xfree( $$.string ); } CBSTART
%destructor { if( $$.dealloc ) zh_xfree( $$.string ); } LITERAL

%%

Main       : Source
           | /* empty file */
           ;

Source     : Crlf
           | Declaration
           | Function
           | Statement
           | Line
           | error  Crlf  { yyclearin; yyerrok; }
           | Source Crlf
           | Source Declaration
           | Source Function
           | Source Statement
           | Source Line
           | Source error Crlf  { yyclearin; yyerrok; }
           ;

Line       : LINE NUM_LONG Crlf
                  { ZH_COMP_PARAM->currLine = ( int ) $2.lNumber;
                    ZH_COMP_PARAM->pLex->fEol = ZH_FALSE; }
           | LINE NUM_LONG LITERAL Crlf
                  { ZH_COMP_PARAM->currModule = zh_compIdentifierNew( ZH_COMP_PARAM, $3.string, $3.dealloc ? ZH_IDENT_FREE : ZH_IDENT_STATIC );
                    ZH_COMP_PARAM->currLine = ( int ) $2.lNumber;
                    ZH_COMP_PARAM->pLex->fEol = ZH_FALSE;
                    $3.dealloc = ZH_FALSE; }
           | LINE NUM_LONG LITERAL '@' LITERAL Crlf   /* Xbase++ style */
                  { ZH_COMP_PARAM->currModule = zh_compIdentifierNew( ZH_COMP_PARAM, $5.string, $5.dealloc ? ZH_IDENT_FREE : ZH_IDENT_STATIC );
                    ZH_COMP_PARAM->currLine = ( int ) $2.lNumber;
                    ZH_COMP_PARAM->pLex->fEol = ZH_FALSE;
                    if( $3.dealloc ) { zh_xfree( $3.string ); $3.dealloc = ZH_FALSE; }
                    $5.dealloc = ZH_FALSE; }
           ;

Function   : FUNCTION  IdentName { zh_compFunctionAdd( ZH_COMP_PARAM, $2, ( ZH_SYMBOLSCOPE ) $1, 0 ); } Crlf
           | PROCEDURE IdentName { zh_compFunctionAdd( ZH_COMP_PARAM, $2, ( ZH_SYMBOLSCOPE ) $1, ZH_FUNF_PROCEDURE ); } Crlf
           | FUNCTION  IdentName { zh_compFunctionAdd( ZH_COMP_PARAM, $2, ( ZH_SYMBOLSCOPE ) $1, 0 ); ZH_COMP_PARAM->iVarScope = ZH_VSCOMP_PARAMETER; } '(' Params ')' Crlf
           | PROCEDURE IdentName { zh_compFunctionAdd( ZH_COMP_PARAM, $2, ( ZH_SYMBOLSCOPE ) $1, ZH_FUNF_PROCEDURE ); ZH_COMP_PARAM->iVarScope = ZH_VSCOMP_PARAMETER;} '(' Params ')' Crlf
           ;

Params     : /*no parameters */ { $$ = 0; }
           | EPSILON { ZH_COMP_PARAM->functions.pLast->fVParams = ZH_TRUE; $$ = 0; }
           | ParamList
           | ParamList ',' EPSILON { ZH_COMP_PARAM->functions.pLast->fVParams = ZH_TRUE; $$ = $1; }
           ;

AsType     : /* not specified */           { $$ = zh_compVarTypeNew( ZH_COMP_PARAM, ' ', NULL ); }
           | StrongType
           ;

AsArrayType: /* not specified */           { $$ = zh_compVarTypeNew( ZH_COMP_PARAM, ' ', NULL ); }
           | AsArray
           ;

StrongType : AS_NUMERIC                    { $$ = zh_compVarTypeNew( ZH_COMP_PARAM, 'N', NULL ); }
           | AS_CHARACTER                  { $$ = zh_compVarTypeNew( ZH_COMP_PARAM, 'C', NULL ); }
           | AS_DATE                       { $$ = zh_compVarTypeNew( ZH_COMP_PARAM, 'D', NULL ); }
           | AS_LOGICAL                    { $$ = zh_compVarTypeNew( ZH_COMP_PARAM, 'L', NULL ); }
           | AS_BLOCK                      { $$ = zh_compVarTypeNew( ZH_COMP_PARAM, 'B', NULL ); }
           | AS_OBJECT                     { $$ = zh_compVarTypeNew( ZH_COMP_PARAM, 'O', NULL ); }
           | AS_CLASS IdentName            { $$ = zh_compVarTypeNew( ZH_COMP_PARAM, 'S', $2 );   }
           | AS_VARIANT                    { $$ = zh_compVarTypeNew( ZH_COMP_PARAM, ' ', NULL ); }
           | AsArray
           ;

AsArray    : AS_ARRAY                      { $$ = zh_compVarTypeNew( ZH_COMP_PARAM, 'A', NULL ); }
           | AS_NUMERIC_ARRAY              { $$ = zh_compVarTypeNew( ZH_COMP_PARAM, 'n', NULL ); }
           | AS_CHARACTER_ARRAY            { $$ = zh_compVarTypeNew( ZH_COMP_PARAM, 'c', NULL ); }
           | AS_DATE_ARRAY                 { $$ = zh_compVarTypeNew( ZH_COMP_PARAM, 'd', NULL ); }
           | AS_LOGICAL_ARRAY              { $$ = zh_compVarTypeNew( ZH_COMP_PARAM, 'l', NULL ); }
           | AS_ARRAY_ARRAY                { $$ = zh_compVarTypeNew( ZH_COMP_PARAM, 'a', NULL ); }
           | AS_BLOCK_ARRAY                { $$ = zh_compVarTypeNew( ZH_COMP_PARAM, 'b', NULL ); }
           | AS_OBJECT_ARRAY               { $$ = zh_compVarTypeNew( ZH_COMP_PARAM, 'o', NULL ); }
           | AS_CLASS_ARRAY IdentName      { $$ = zh_compVarTypeNew( ZH_COMP_PARAM, 's', $2 );   }
           ;

ParamList  : IdentName AsType                { zh_compVariableAdd( ZH_COMP_PARAM, $1, $2 ); $$ = 1; }
           | ParamList ',' IdentName AsType  { zh_compVariableAdd( ZH_COMP_PARAM, $3, $4 ); $$++; }
           ;

/* NOTE: This allows the use of Expression as a statement.
 *    The Expression is validated later in reduction phase of
 *    zh_compExprGenStatement(). With this solution we don't have to
 *    stop compilation if invalid syntax will be used.
 */
Statement  : ExecFlow CrlfStmnt
           | IfInline CrlfStmnt     { ZH_COMP_EXPR_FREE( zh_compExprGenStatement( $1, ZH_COMP_PARAM ) ); ZH_COMP_PARAM->functions.pLast->funFlags &= ~ ZH_FUNF_WITH_RETURN; }
           | FunCall CrlfStmnt      { ZH_COMP_EXPR_FREE( zh_compExprGenStatement( $1, ZH_COMP_PARAM ) ); ZH_COMP_PARAM->functions.pLast->funFlags &= ~ ZH_FUNF_WITH_RETURN; }
           | AliasExpr CrlfStmnt    { ZH_COMP_EXPR_FREE( zh_compExprGenStatement( $1, ZH_COMP_PARAM ) ); ZH_COMP_PARAM->functions.pLast->funFlags &= ~ ZH_FUNF_WITH_RETURN; }
           | ObjectMethod CrlfStmnt { ZH_COMP_EXPR_FREE( zh_compExprGenStatement( $1, ZH_COMP_PARAM ) ); ZH_COMP_PARAM->functions.pLast->funFlags &= ~ ZH_FUNF_WITH_RETURN; }
           | MacroAny CrlfStmnt     { if( ZH_COMP_ISSUPPORTED( ZH_COMPFLAG_XBASE ) )
                                         ZH_COMP_EXPR_FREE( zh_compExprGenStatement( $1, ZH_COMP_PARAM ) );
                                      else
                                         ZH_COMP_EXPR_FREE( ZH_COMP_ERROR_SYNTAX( $1 ) );
                                      ZH_COMP_PARAM->functions.pLast->funFlags &= ~ ZH_FUNF_WITH_RETURN;
                                    }
           | PareExpList CrlfStmnt  { ZH_COMP_EXPR_FREE( zh_compExprGenStatement( $1, ZH_COMP_PARAM ) ); ZH_COMP_PARAM->functions.pLast->funFlags &= ~ ZH_FUNF_WITH_RETURN; }
           | ExprPreOp CrlfStmnt    { ZH_COMP_EXPR_FREE( zh_compExprGenStatement( $1, ZH_COMP_PARAM ) ); ZH_COMP_PARAM->functions.pLast->funFlags &= ~ ZH_FUNF_WITH_RETURN; }
           | ExprPostOp CrlfStmnt   { ZH_COMP_EXPR_FREE( zh_compExprGenStatement( $1, ZH_COMP_PARAM ) ); ZH_COMP_PARAM->functions.pLast->funFlags &= ~ ZH_FUNF_WITH_RETURN; }
           | ExprOperEq CrlfStmnt   { ZH_COMP_EXPR_FREE( zh_compExprGenStatement( $1, ZH_COMP_PARAM ) ); ZH_COMP_PARAM->functions.pLast->funFlags &= ~ ZH_FUNF_WITH_RETURN; }
           | ExprEqual CrlfStmnt    { ZH_COMP_EXPR_FREE( zh_compExprGenStatement( $1, ZH_COMP_PARAM ) ); ZH_COMP_PARAM->functions.pLast->funFlags &= ~ ZH_FUNF_WITH_RETURN; }
           | ExprAssign CrlfStmnt   { ZH_COMP_EXPR_FREE( zh_compExprGenStatement( $1, ZH_COMP_PARAM ) ); ZH_COMP_PARAM->functions.pLast->funFlags &= ~ ZH_FUNF_WITH_RETURN; }
           | DoProc CrlfStmnt       { ZH_COMP_EXPR_FREE( zh_compExprGenStatement( $1, ZH_COMP_PARAM ) ); ZH_COMP_PARAM->functions.pLast->funFlags &= ~ ZH_FUNF_WITH_RETURN; }
           | BREAK CrlfStmnt        { zh_compGenBreak( ZH_COMP_PARAM ); zh_compGenPCode2( ZH_P_DOSHORT, 0, ZH_COMP_PARAM );
                                      ZH_COMP_PARAM->functions.pLast->funFlags |= ZH_FUNF_BREAK_CODE; }
           | BREAK { zh_compLinePushIfInside( ZH_COMP_PARAM ); } Expression Crlf
                                    {
                                       zh_compGenBreak( ZH_COMP_PARAM ); ZH_COMP_EXPR_FREE( zh_compExprGenPush( $3, ZH_COMP_PARAM ) );
                                       zh_compGenPCode2( ZH_P_DOSHORT, 1, ZH_COMP_PARAM );
                                       ZH_COMP_PARAM->functions.pLast->funFlags |= ZH_FUNF_BREAK_CODE;
                                    }
           | EXIT CrlfStmnt { zh_compLoopExit( ZH_COMP_PARAM ); ZH_COMP_PARAM->functions.pLast->funFlags |= ZH_FUNF_BREAK_CODE; }
           | LOOP CrlfStmnt { zh_compLoopLoop( ZH_COMP_PARAM ); ZH_COMP_PARAM->functions.pLast->funFlags |= ZH_FUNF_BREAK_CODE; }
           | RETURN CrlfStmnt {
                        if( ZH_COMP_PARAM->functions.pLast->wSeqCounter )
                        {
                           zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_EXIT_IN_SEQUENCE, "RETURN", NULL );
                        }
                        zh_compGenPCode1( ZH_P_ENDPROC, ZH_COMP_PARAM );
                        if( ( ZH_COMP_PARAM->functions.pLast->funFlags & ZH_FUNF_PROCEDURE ) == 0 )
                        {
                           /* return from a function without a return value */
                           zh_compGenWarning( ZH_COMP_PARAM, zh_comp_szWarnings, 'W', ZH_COMP_WARN_NO_RETURN_VALUE, NULL, NULL );
                        }
                        ZH_COMP_PARAM->functions.pLast->funFlags |= ZH_FUNF_WITH_RETURN | ZH_FUNF_BREAK_CODE;
                     }
           | RETURN  {  zh_compLinePushIfInside( ZH_COMP_PARAM ); }
             Expression Crlf
                     {
                        if( ZH_COMP_PARAM->functions.pLast->wSeqCounter )
                        {
                           zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_EXIT_IN_SEQUENCE, "RETURN", NULL );
                        }
                        /* TODO: check if return value agree with declared value */
                        ZH_COMP_EXPR_FREE( zh_compExprGenPush( $3, ZH_COMP_PARAM ) );
                        if( ZH_COMP_PARAM->functions.pLast->funFlags & ZH_FUNF_EXTBLOCK )
                           /* extended clodeblock, use ZH_P_ENDBLOCK to return value and stop execution */
                           zh_compGenPCode1( ZH_P_ENDBLOCK, ZH_COMP_PARAM );
                        else
                           zh_compGenPCode2( ZH_P_RETVALUE, ZH_P_ENDPROC, ZH_COMP_PARAM );
                        if( ZH_COMP_PARAM->functions.pLast->funFlags & ZH_FUNF_PROCEDURE )
                        {
                           /* procedure returns a value */
                           zh_compGenWarning( ZH_COMP_PARAM, zh_comp_szWarnings, 'W', ZH_COMP_WARN_PROC_RETURN_VALUE, NULL, NULL );
                        }
                        ZH_COMP_PARAM->functions.pLast->funFlags |= ZH_FUNF_WITH_RETURN | ZH_FUNF_BREAK_CODE;
                     }
           | PUBLIC  {  zh_compLinePushIfInside( ZH_COMP_PARAM ); ZH_COMP_PARAM->iVarScope = ZH_VSCOMP_PUBLIC; }
                     ExtVarList
                     {  zh_compRTVariableGen( ZH_COMP_PARAM, "__MVPUBLIC" );
                        ZH_COMP_PARAM->iVarScope = ZH_VSCOMP_NONE;
                        ZH_COMP_PARAM->functions.pLast->funFlags &= ~ ZH_FUNF_WITH_RETURN;
                     } Crlf
           | PRIVATE {  zh_compLinePushIfInside( ZH_COMP_PARAM ); ZH_COMP_PARAM->iVarScope = ZH_VSCOMP_PRIVATE; }
                     ExtVarList
                     {  zh_compRTVariableGen( ZH_COMP_PARAM, "__MVPRIVATE" );
                        ZH_COMP_PARAM->iVarScope = ZH_VSCOMP_NONE;
                        ZH_COMP_PARAM->functions.pLast->funFlags &= ~ ZH_FUNF_WITH_RETURN;
                     } Crlf
           | VarDefs
           | FieldsDef
           | MemvarDef
           | EXTERN ExtList Crlf
           | DYNAMIC DynList Crlf
           | ANNOUNCE IdentName {
                  if( ZH_COMP_PARAM->szAnnounce == NULL )
                     ZH_COMP_PARAM->szAnnounce = $2;
                  else
                     zh_compGenWarning( ZH_COMP_PARAM, zh_comp_szWarnings, 'W', ZH_COMP_WARN_DUPL_ANNOUNCE, $2, NULL );
               } Crlf
           | PROCREQ CompTimeStr ')' Crlf
           ;

CompTimeStr : LITERAL {
               if( $1.dealloc )
               {
                  $1.string = ( char * ) ZH_UNCONST( zh_compIdentifierNew( ZH_COMP_PARAM, $1.string, ZH_IDENT_FREE ) );
                  $1.dealloc = ZH_FALSE;
               }
               zh_compModuleAdd( ZH_COMP_PARAM, $1.string, ZH_FALSE );
            }
            | LITERAL '+' LITERAL {
               {
                  char szFileName[ ZH_PATH_MAX ];
                  zh_strncat( zh_strncpy( szFileName, $1.string, sizeof( szFileName ) - 1 ), $3.string, sizeof( szFileName ) - 1 );
                  zh_compModuleAdd( ZH_COMP_PARAM, zh_compIdentifierNew( ZH_COMP_PARAM, szFileName, ZH_IDENT_COPY ), ZH_FALSE );
                  if( $1.dealloc )
                  {
                     zh_xfree( $1.string );
                     $1.dealloc = ZH_FALSE;
                  }
                  if( $3.dealloc )
                  {
                     zh_xfree( $3.string );
                     $3.dealloc = ZH_FALSE;
                  }
               }
            }
           ;

CrlfStmnt  : { zh_compLinePushIfInside( ZH_COMP_PARAM ); } Crlf
           ;

LineStat   : Crlf          { $<lNumber>$ = 0; }
           | Statement     { $<lNumber>$ = 1; }
           | Declaration   { $<lNumber>$ = 1; }
           | Line          { $<lNumber>$ = 0; }
           /* | error Function */
           | error         { if( ZH_COMP_PARAM->ilastLineErr && ZH_COMP_PARAM->ilastLineErr == ZH_COMP_PARAM->currLine )
                             {
                                yyclearin;
                             }
                             else
                             {
                                yyerrok;
                                ZH_COMP_PARAM->ilastLineErr = ZH_COMP_PARAM->currLine;
                             }
                             $<lNumber>$ = 0;
                           }
           ;

Statements : LineStat
           | Statements LineStat       { $<lNumber>$ += $<lNumber>2; }
           ;

EmptyStats : /* empty */               { $<lNumber>$ = 0; }
           | Statements
           ;

ExtList    : IdentName                 { zh_compExternAdd( ZH_COMP_PARAM, $1, 0 ); }
           | ExtList ',' IdentName     { zh_compExternAdd( ZH_COMP_PARAM, $3, 0 ); }
           ;

DynList    : IdentName                 { zh_compExternAdd( ZH_COMP_PARAM, $1, ZH_FS_DEFERRED ); }
           | DynList ',' IdentName     { zh_compExternAdd( ZH_COMP_PARAM, $3, ZH_FS_DEFERRED ); }
           ;

IdentName  : IDENTIFIER
           | STEP             { $$ = "STEP"; }
           | TO               { $$ = "TO"; }
           | LOOP             { $$ = "LOOP"; }
           | EXIT             { $$ = "EXIT"; }
           | IN               { $$ = "IN"; }
           | OPTIONAL         { $$ = $<string>1; }
           | EXTERN           { $$ = $<string>1; }
           | DYNAMIC          { $$ = $<string>1; }
           | ANNOUNCE         { $$ = $<string>1; }
           | LOCAL            { $$ = $<string>1; }
           | MEMVAR           { $$ = $<string>1; }
           | STATIC           { $$ = $<string>1; }
           | PRIVATE          { $$ = $<string>1; }
           | PUBLIC           { $$ = $<string>1; }
           | PARAMETERS       { $$ = $<string>1; }
           | PROCREQ          { $$ = $<string>1; }
           | DESCEND          { $$ = $<string>1; }
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

NumAlias   : NUM_LONG    ALIASOP    { $$ = zh_compExprNewLong( $1.lNumber, ZH_COMP_PARAM ); }
           | NUM_DOUBLE  ALIASOP    { $$ = zh_compErrorAlias( ZH_COMP_PARAM, zh_compExprNewDouble( $1.dNumber, $1.bWidth, $1.bDec, ZH_COMP_PARAM ) ); }
           ;

/* NIL value
 */
NilValue   : NIL                    { $$ = zh_compExprNewNil( ZH_COMP_PARAM ); }
           ;

NilAlias   : NilValue ALIASOP
           ;

/* Literal string value
 */
LiteralValue : LITERAL        {
                                 $$ = zh_compExprNewString( $1.string, $1.length, $1.dealloc, ZH_COMP_PARAM );
                                 $1.dealloc = ZH_FALSE;
                              }
             ;

LiteralAlias : LiteralValue ALIASOP
             ;

/* Codeblock value
 */
CodeBlockAlias : CodeBlock ALIASOP
               ;

/* Logical value
 */
Logical  : TRUEVALUE          { $$ = zh_compExprNewLogical( ZH_TRUE, ZH_COMP_PARAM ); }
         | FALSEVALUE         { $$ = zh_compExprNewLogical( ZH_FALSE, ZH_COMP_PARAM ); }
         ;

LogicalAlias : Logical ALIASOP
             ;

/* SELF value and expressions
 */
SelfValue : SELF              { $$ = zh_compExprNewSelf( ZH_COMP_PARAM ); }
          ;

SelfAlias  : SelfValue ALIASOP
           ;

/* Literal array
 */
/*
Array    : '{' { $<bTrue>$=ZH_COMP_PARAM->iPassByRef; ZH_COMP_PARAM->iPassByRef=ZH_PASSBYREF_ARRAY; }
               ElemList
           '}' { $$ = zh_compExprNewArray( $3, ZH_COMP_PARAM ); ZH_COMP_PARAM->iPassByRef=$<bTrue>2; }
         ;
*/
Array    : '{' ElemList '}'         { $$ = zh_compExprNewArray( $2, ZH_COMP_PARAM ); }
         ;

ArrayAlias  : Array ALIASOP
            ;

/* Literal array access
 */
ArrayAt     : Array ArrayIndex      { $$ = $2; }
            ;

ArrayAtAlias : ArrayAt ALIASOP
             ;

Hash     : '{' HASHOP '}'           { $$ = zh_compExprNewHash( NULL, ZH_COMP_PARAM ); }
         | '{' HashList '}'         { $$ = zh_compExprNewHash( $2, ZH_COMP_PARAM ); }
         ;

HashAlias: Hash ALIASOP
         ;

HashList : Expression HASHOP EmptyExpression                { $$ = zh_compExprAddListExpr( zh_compExprNewList( $1, ZH_COMP_PARAM ), $3 ); }
         | HashList ',' Expression HASHOP EmptyExpression   { $$ = zh_compExprAddListExpr( zh_compExprAddListExpr( $1, $3 ), $5 ); }
         ;

/* Variables
 */
Variable : IdentName          { $$ = zh_compExprNewVar( $1, ZH_COMP_PARAM ); }
         ;

VarAlias : IdentName ALIASOP  { $$ = zh_compExprNewAlias( $1, ZH_COMP_PARAM ); }
         ;

/* Macro variables
 */
MacroVar : MACROVAR           { $$ = zh_compExprNewMacro( NULL, '&', $1, ZH_COMP_PARAM ); }
         | MACROTEXT          { $$ = zh_compExprNewMacro( NULL, 0, $1, ZH_COMP_PARAM ); }
         ;

MacroVarAlias  : MacroVar ALIASOP
               ;

/* Macro expressions
 */
MacroExpr : '&' PareExpList   { $$ = zh_compExprNewMacro( $2, 0, NULL, ZH_COMP_PARAM ); }
          ;

MacroExprAlias : MacroExpr ALIASOP
               ;

MacroAny : MacroVar
         | MacroExpr
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
FieldVarAlias  : FieldAlias VarAlias            { ZH_COMP_EXPR_FREE( $1 ); $$ = $2; }
               | FieldAlias NumAlias            { ZH_COMP_EXPR_FREE( $1 ); $$ = $2; }
               | FieldAlias PareExpListAlias    { ZH_COMP_EXPR_FREE( $1 ); $$ = $2; }
               | FieldAlias MacroVarAlias       { ZH_COMP_EXPR_FREE( $1 ); $$ = $2; }
               | FieldAlias MacroExprAlias      { ZH_COMP_EXPR_FREE( $1 ); $$ = $2; }
               | FieldAlias NilAlias            { ZH_COMP_EXPR_FREE( $1 ); $$ = zh_compErrorAlias( ZH_COMP_PARAM, $2 ); }
               | FieldAlias LiteralAlias        { ZH_COMP_EXPR_FREE( $1 ); $$ = zh_compErrorAlias( ZH_COMP_PARAM, $2 ); }
               | FieldAlias LogicalAlias        { ZH_COMP_EXPR_FREE( $1 ); $$ = zh_compErrorAlias( ZH_COMP_PARAM, $2 ); }
               | FieldAlias CodeBlockAlias      { ZH_COMP_EXPR_FREE( $1 ); $$ = zh_compErrorAlias( ZH_COMP_PARAM, $2 ); }
               | FieldAlias SelfAlias           { ZH_COMP_EXPR_FREE( $1 ); $$ = zh_compErrorAlias( ZH_COMP_PARAM, $2 ); }
               | FieldAlias ArrayAlias          { ZH_COMP_EXPR_FREE( $1 ); $$ = zh_compErrorAlias( ZH_COMP_PARAM, $2 ); }
               | FieldAlias ArrayAtAlias        { ZH_COMP_EXPR_FREE( $1 ); $$ = zh_compErrorAlias( ZH_COMP_PARAM, $2 ); }
               | FieldAlias HashAlias           { ZH_COMP_EXPR_FREE( $1 ); $$ = zh_compErrorAlias( ZH_COMP_PARAM, $2 ); }
               | FieldAlias IfInlineAlias       { ZH_COMP_EXPR_FREE( $1 ); $$ = zh_compErrorAlias( ZH_COMP_PARAM, $2 ); }
               ;

AliasId     : IdentName       { $$ = zh_compExprNewVar( $1, ZH_COMP_PARAM ); }
            | MacroAny
            ;

AliasVar   : NumAlias AliasId          { $$ = zh_compExprNewAliasVar( $1, $2, ZH_COMP_PARAM ); }
           | MacroVarAlias AliasId     { $$ = zh_compExprNewAliasVar( $1, $2, ZH_COMP_PARAM ); }
           | MacroExprAlias AliasId    { $$ = zh_compExprNewAliasVar( $1, $2, ZH_COMP_PARAM ); }
           | PareExpListAlias AliasId  { $$ = zh_compExprNewAliasVar( $1, $2, ZH_COMP_PARAM ); }
           | NilAlias AliasId          { ZH_COMP_EXPR_FREE( $2 ); $$ = zh_compErrorAlias( ZH_COMP_PARAM, $1 ); }
           | LiteralAlias AliasId      { ZH_COMP_EXPR_FREE( $2 ); $$ = zh_compErrorAlias( ZH_COMP_PARAM, $1 ); }
           | LogicalAlias AliasId      { ZH_COMP_EXPR_FREE( $2 ); $$ = zh_compErrorAlias( ZH_COMP_PARAM, $1 ); }
           | CodeBlockAlias AliasId    { ZH_COMP_EXPR_FREE( $2 ); $$ = zh_compErrorAlias( ZH_COMP_PARAM, $1 ); }
           | HashAlias AliasId         { ZH_COMP_EXPR_FREE( $2 ); $$ = zh_compErrorAlias( ZH_COMP_PARAM, $1 ); }
           | SelfAlias AliasId         { ZH_COMP_EXPR_FREE( $2 ); $$ = zh_compErrorAlias( ZH_COMP_PARAM, $1 ); }
           | ArrayAlias AliasId        { ZH_COMP_EXPR_FREE( $2 ); $$ = zh_compErrorAlias( ZH_COMP_PARAM, $1 ); }
           | ArrayAtAlias AliasId      { ZH_COMP_EXPR_FREE( $2 ); $$ = zh_compErrorAlias( ZH_COMP_PARAM, $1 ); }
           | VariableAtAlias AliasId   { ZH_COMP_EXPR_FREE( $2 ); $$ = zh_compErrorAlias( ZH_COMP_PARAM, $1 ); }
           | IfInlineAlias AliasId     { ZH_COMP_EXPR_FREE( $2 ); $$ = zh_compErrorAlias( ZH_COMP_PARAM, $1 ); }
           | FunCallAlias AliasId      { ZH_COMP_EXPR_FREE( $2 ); $$ = zh_compErrorAlias( ZH_COMP_PARAM, $1 ); }
           | ObjectDataAlias AliasId   { ZH_COMP_EXPR_FREE( $2 ); $$ = zh_compErrorAlias( ZH_COMP_PARAM, $1 ); }
           | ObjectMethodAlias AliasId { ZH_COMP_EXPR_FREE( $2 ); $$ = zh_compErrorAlias( ZH_COMP_PARAM, $1 ); }
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
           | FieldAlias PareExpList       { ZH_COMP_EXPR_FREE( $1 ); $$ = zh_compErrorAlias( ZH_COMP_PARAM, $2 ); }
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
            | MacroAny        ArrayIndex  { $$ = $2; }
            | ObjectData      ArrayIndex  { $$ = $2; }
            | ObjectMethod    ArrayIndex  { $$ = $2; }
            | FunCall         ArrayIndex  { $$ = $2; }
            | IfInline        ArrayIndex  { $$ = $2; }
            | PareExpList     ArrayIndex  { $$ = $2; }
            ;

VariableAtAlias : VariableAt ALIASOP
                ;
/* function call
 */
FunIdentCall: IdentName '(' ArgList ')'   { $$ = zh_compExprNewFunCall( zh_compExprNewFunName( $1, ZH_COMP_PARAM ), $3, ZH_COMP_PARAM ); }
            ;

FunCall     : FunIdentCall
            | MacroAny  '(' ArgList ')'   { $$ = zh_compExprNewFunCall( $1, $3, ZH_COMP_PARAM ); }
            ;

/* FunRef      : '@' FunCall     { $$ = zh_compCheckPassByRef( ZH_COMP_PARAM, $2 ); } */
FunRef      : '@' FunIdentCall   { $$ = zh_compCheckPassByRef( ZH_COMP_PARAM, $2 ); }
            ;

FunCallAlias : FunCall ALIASOP
             ;

ArgList     : ExtArgument               { $$ = zh_compExprNewArgList( $1, ZH_COMP_PARAM ); }
            | ArgList ',' ExtArgument   { $$ = zh_compExprAddListExpr( $1, $3 ); }
            ;

Argument    : EmptyExpression
            | RefArgument
            ;

RefArgument : '@' IdentName    { $$ = zh_compCheckPassByRef( ZH_COMP_PARAM, zh_compExprNewVarRef( $2, ZH_COMP_PARAM ) ); }
            | '@' MacroVar     { $$ = zh_compCheckPassByRef( ZH_COMP_PARAM, zh_compExprNewRef( $2, ZH_COMP_PARAM ) ); }
            | '@' AliasVar     { $$ = zh_compCheckPassByRef( ZH_COMP_PARAM, zh_compExprNewRef( $2, ZH_COMP_PARAM ) ); }
            | '@' ObjectData   { $$ = zh_compCheckPassByRef( ZH_COMP_PARAM, zh_compExprNewRef( $2, ZH_COMP_PARAM ) ); }
            | '@' VariableAt   { $$ = zh_compCheckPassByRef( ZH_COMP_PARAM, $2 ); $$->value.asList.reference = ZH_TRUE; }
            ;

ExtArgument : EPSILON  { $$ = zh_compExprNewArgRef( ZH_COMP_PARAM ); }
            | Argument
            ;

/* Object's instance variable
 */
ObjectData  : LeftExpression ':' SendId   { $$ = zh_compCheckMethod( ZH_COMP_PARAM, zh_compExprNewMethodObject( $3, $1 ) ); }
            | ObjectRef ':' SendId        { $$ = zh_compExprNewMethodObject( $3, $1 ); }
            | ':' SendId                  {  if( ZH_COMP_PARAM->functions.pLast->wWithObjectCnt == 0 )
                                                zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_WITHOBJECT, NULL, NULL );
                                             $$ = $2;
                                          }
            ;

SendId      : IdentName      { $$ = zh_compExprNewSend( $1, ZH_COMP_PARAM ); }
            | MacroAny       { $$ = zh_compExprNewMacroSend( $1, ZH_COMP_PARAM ); }
            ;

ObjectRef   : '(' '@' IdentName ')'       { $$ = zh_compExprNewVarRef( $3, ZH_COMP_PARAM ); }
            ;

ObjectDataAlias : ObjectData ALIASOP
                ;

/* Object's method
 */
ObjectMethod : ObjectData '(' ArgList ')' { $$ = zh_compExprNewMethodCall( $1, $3 ); }
             ;

ObjectMethodAlias : ObjectMethod ALIASOP
                  ;


/* NOTE: We have to distinguish IdentName here because it is repeated
 * in DoArgument (a part of DO <proc> WITH .. statement)
 * where it generates different action.
 */
SimpleExpression :
             NumValue
           | NilValue
           | DateValue
           | TimeStampValue
           | LiteralValue
           | CodeBlock
           | Logical
           | SelfValue
           | SelfValue        StrongType  { $$ = $1; }
           | Array
           | ArrayAt
           | Hash
           | AliasVar
           | AliasExpr
           | MacroVar
           | MacroExpr
           | VariableAt
           | FunCall
           | FunCall          StrongType  { $$ = $1; }
           | IfInline
           | ObjectData
           | ObjectData       StrongType  { $$ = $1; }
           | ObjectMethod
           | ObjectMethod     StrongType  { $$ = $1; }
           | ExprAssign
           | ExprOperEq
           | ExprPostOp
           | ExprPreOp
           | ExprUnary
           | ExprMath
           | ExprBool
           | ExprRelation
           ;

Expression : SimpleExpression
           | Variable
           | PareExpList
           | Variable     StrongType { $$ = $1; }
           | PareExpList  StrongType { $$ = $1; }
           | FunRef
           ;

ExtExpression : EPSILON  { $$ = zh_compExprNewArgRef( ZH_COMP_PARAM ); }
              | Expression
              ;

EmptyExpression : /* nothing => nil */    { $$ = zh_compExprNewEmpty( ZH_COMP_PARAM ); }
                | Expression
                ;

LValue      : IdentName                   { $$ = zh_compExprNewVar( $1, ZH_COMP_PARAM ); }
            | AliasVar
            | MacroVar
            | MacroExpr
            | ObjectData
            | VariableAt
            | PareExpList                 { $$ = zh_compExprListStrip( $1, ZH_COMP_PARAM ); }
            ;

/* NOTE: The rule: Expression Operator Expression
 * that can be used standalone as a statement have to be written
 * using all possible left values to resolve shift/reduce conflicts
 */
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
               | MacroAny
               | Variable
               | VariableAt
               | PareExpList
               | FunCall
               | IfInline
               | ObjectData
               | ObjectMethod
               ;

/* NOTE: PostOp can be used in one context only - it uses $0 rule
 *    (the rule that stands before PostOp)
 */
PostOp      : INC    { $$ = zh_compExprNewPostInc( $<asExpr>0, ZH_COMP_PARAM ); }
            | DEC    { $$ = zh_compExprNewPostDec( $<asExpr>0, ZH_COMP_PARAM ); }
            ;

ExprPostOp  : LeftExpression  PostOp %prec POST  { $$ = $2; }
            ;

ExprPreOp   : INC Expression  %prec PRE      { $$ = zh_compExprNewPreInc( $2, ZH_COMP_PARAM ); }
            | DEC Expression  %prec PRE      { $$ = zh_compExprNewPreDec( $2, ZH_COMP_PARAM ); }
            ;

ExprUnary   : NOT Expression                 { $$ = zh_compExprNewNot( $2, ZH_COMP_PARAM ); }
            | '-' Expression  %prec UNARY    { $$ = zh_compExprNewNegate( $2, ZH_COMP_PARAM ); }
            | '+' Expression  %prec UNARY    { $$ = $2; }
            ;

ExprEqual   : LeftExpression '=' Expression %prec INASSIGN { $$ = zh_compExprAssign( $1, $3, ZH_COMP_PARAM ); }
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

ExprMath    : Expression '+' Expression   { $$ = zh_compExprSetOperand( zh_compExprNewPlus( $1, ZH_COMP_PARAM ), $3, ZH_COMP_PARAM ); }
            | Expression '-' Expression   { $$ = zh_compExprSetOperand( zh_compExprNewMinus( $1, ZH_COMP_PARAM ), $3, ZH_COMP_PARAM ); }
            | Expression '*' Expression   { $$ = zh_compExprSetOperand( zh_compExprNewMult( $1, ZH_COMP_PARAM ), $3, ZH_COMP_PARAM ); }
            | Expression '/' Expression   { $$ = zh_compExprSetOperand( zh_compExprNewDiv( $1, ZH_COMP_PARAM ), $3, ZH_COMP_PARAM ); }
            | Expression '%' Expression   { $$ = zh_compExprSetOperand( zh_compExprNewMod( $1, ZH_COMP_PARAM ), $3, ZH_COMP_PARAM ); }
            | Expression POWER Expression { $$ = zh_compExprSetOperand( zh_compExprNewPower( $1, ZH_COMP_PARAM ), $3, ZH_COMP_PARAM ); }
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

ArrayIndex : IndexList ']'
           ;

/* NOTE: $0 represents the expression before ArrayIndex
 *    Don't use ArrayIndex in other context than as an array index!
 */
IndexList  : '[' ExtExpression               { $$ = zh_compExprNewArrayAt( $<asExpr>0, $2, ZH_COMP_PARAM ); }
           | IndexList ',' ExtExpression     { $$ = zh_compExprNewArrayAt( $1, $3, ZH_COMP_PARAM ); }
           | IndexList ']' '[' ExtExpression { $$ = zh_compExprNewArrayAt( $1, $4, ZH_COMP_PARAM ); }
           ;

ElemList   : ExtArgument               { $$ = zh_compExprNewList( $1, ZH_COMP_PARAM ); }
           | ElemList ',' ExtArgument  { $$ = zh_compExprAddListExpr( $1, $3 ); }
           ;

BlockHead   : CBSTART         { $<asExpr>$ = zh_compExprNewCodeBlock( $1.string, $1.length, $1.flags, ZH_COMP_PARAM ); $1.string = NULL; }
              BlockVars '|'   { $$ = $<asExpr>2; }
            ;

/* NOTE: This uses $0 then don't use BlockVars, BlockVarList and BlockExpList in other context
 */
BlockVars  : /* empty list */          { $$ = NULL; }
           | EPSILON                   { $$ = NULL; $<asExpr>0->value.asCodeblock.flags |= ZH_BLOCK_VPARAMS; }
           | BlockVarList              { $$ = $1;   }
           | BlockVarList ',' EPSILON  { $$ = $1;   $<asExpr>0->value.asCodeblock.flags |= ZH_BLOCK_VPARAMS; }
           ;

BlockVarList : IdentName AsType                    { ZH_COMP_PARAM->iVarScope = ZH_VSCOMP_LOCAL; $$ = zh_compExprCBVarAdd( $<asExpr>0, $1, $2->cVarType, ZH_COMP_PARAM ); }
             | BlockVarList ',' IdentName AsType   { ZH_COMP_PARAM->iVarScope = ZH_VSCOMP_LOCAL; $$ = zh_compExprCBVarAdd( $<asExpr>0, $3, $4->cVarType, ZH_COMP_PARAM ); }
             ;

BlockExpList : Expression                    { $$ = zh_compExprAddCodeblockExpr( $<asExpr>-1, $1 ); }
             | BlockExpList ',' Expression   { $$ = zh_compExprAddCodeblockExpr( $<asExpr>-1, $3 ); }
             ;

CodeBlock   : BlockHead
              { $<bTrue>$ = ZH_COMP_PARAM->functions.pLast->bBlock;
                ZH_COMP_PARAM->functions.pLast->bBlock = ZH_TRUE; }
              BlockExpList
              { ZH_COMP_PARAM->functions.pLast->bBlock = $<bTrue>2; }
              '}'
            | BlockHead Crlf
            {  /* 3 */
               PZH_CBVAR pVar;
               $<sNumber>$ = ZH_COMP_PARAM->functions.pLast->nPCodePos;
               $<sNumber>2 = ZH_COMP_PARAM->lastLine;
               zh_compCodeBlockStart( ZH_COMP_PARAM, 0 );
               ZH_COMP_PARAM->functions.pLast->funFlags |= ZH_FUNF_EXTBLOCK;
               ZH_COMP_PARAM->functions.pLast->fVParams =
                  ( $1->value.asCodeblock.flags & ZH_BLOCK_VPARAMS ) != 0;

               $1->value.asCodeblock.flags |= ZH_BLOCK_EXT;
               if( $1->value.asCodeblock.string )
               {
                  zh_xfree( $1->value.asCodeblock.string );
                  $1->value.asCodeblock.string = NULL;
                  $1->nLength = 0;
               }

               ZH_COMP_PARAM->iVarScope = ZH_VSCOMP_PARAMETER;
               pVar = $1->value.asCodeblock.pLocals;
               while( pVar )
               {
                  zh_compVariableAdd( ZH_COMP_PARAM, pVar->szName, zh_compVarTypeNew( ZH_COMP_PARAM, pVar->bType, NULL ) );
                  pVar =pVar->pNext;
               }
            }
            EmptyStats BlockEnd
            {  /* 6 */
               /* protection against nested function/procedure inside extended block */
               if( ZH_COMP_PARAM->iErrorCount == 0 ||
                   ZH_COMP_PARAM->functions.pLast->funFlags & ZH_FUNF_EXTBLOCK )
               {
                  zh_compCodeBlockEnd( ZH_COMP_PARAM );
                  $$ = zh_compExprSetCodeblockBody( $1,
                          ZH_COMP_PARAM->functions.pLast->pCode + $<sNumber>3,
                          ZH_COMP_PARAM->functions.pLast->nPCodePos - $<sNumber>3 );
                  ZH_COMP_PARAM->functions.pLast->nPCodePos = $<sNumber>3;
                  ZH_COMP_PARAM->lastLine = $<sNumber>2;
               }
            }
            ;

BlockEnd    : '}'
            | ENDERR ';' { zh_compErrUnclosed( ZH_COMP_PARAM, "{||...}" ); }
            ;


ExpList     : Expression               { $$ = zh_compExprNewList( $1, ZH_COMP_PARAM ); }
            | ExpList ',' Expression   { $$ = zh_compExprAddListExpr( $1, $3 ); }

PareExpList : '(' ExpList ')'          { $$ = $2; }
            ;

PareExpListAlias : PareExpList ALIASOP
                 ;

IfInline : IIF '(' Expression ',' Argument ',' Argument ')'
            { $$ = zh_compExprNewIIF( zh_compExprAddListExpr( zh_compExprAddListExpr( zh_compExprNewList( $3, ZH_COMP_PARAM ), $5 ), $7 ) ); }
         ;

IfInlineAlias : IfInline ALIASOP
              ;

VarDefs  : LOCAL { ZH_COMP_PARAM->iVarScope = ZH_VSCOMP_LOCAL; zh_compLinePush( ZH_COMP_PARAM ); }
           VarList Crlf
         | STATIC { ZH_COMP_PARAM->iVarScope = ZH_VSCOMP_STATIC; zh_compLinePush( ZH_COMP_PARAM ); }
           VarList Crlf
         | THREAD_STATIC { ZH_COMP_PARAM->iVarScope = ZH_VSCOMP_TH_STATIC; zh_compLinePush( ZH_COMP_PARAM ); }
           VarList Crlf
         | PARAMETERS { if( ZH_COMP_PARAM->functions.pLast->funFlags & ZH_FUNF_USES_LOCAL_PARAMS )
                           zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_PARAMETERS_NOT_ALLOWED, NULL, NULL );
                        else
                        {
                           ZH_COMP_PARAM->functions.pLast->wParamNum = 0;
                           ZH_COMP_PARAM->iVarScope = ( ZH_VSCOMP_PRIVATE | ZH_VSCOMP_PARAMETER );
                        }
                      } MemvarList Crlf { ZH_COMP_PARAM->iVarScope = ZH_VSCOMP_NONE; }
         ;

VarList  : VarDef                         { $$ = 1; }
         | VarList ',' VarDef             { $$++; }
         ;

ExtVarList  : ExtVarDef                   { $$ = 1; }
            | ExtVarList ',' ExtVarDef    { $$++; }
            ;

/* NOTE: if STATIC or LOCAL variables are declared and initialized then we can
 * assign a value immediately - however for PRIVATE and PUBLIC variables
 * initialization have to be delayed because we have to create these variables
 * first.
 */
ExtVarDef  : VarDef
           | MacroVar AsType
               { zh_compRTVariableAdd( ZH_COMP_PARAM, zh_compExprNewRTVar( NULL, $1, ZH_COMP_PARAM ), ZH_FALSE ); }
           | MacroVar AsType INASSIGN Expression
               { ZH_COMP_EXPR_FREE( zh_compExprGenPush( $4, ZH_COMP_PARAM ) );
                 zh_compRTVariableAdd( ZH_COMP_PARAM, zh_compExprNewRTVar( NULL, $1, ZH_COMP_PARAM ), ZH_TRUE );
               }
           | MacroVar DimList AsArrayType
               {
                  ZH_COMP_EXPR_FREE( zh_compArrayDimPush( $2, ZH_COMP_PARAM ) );
                  zh_compRTVariableAdd( ZH_COMP_PARAM, zh_compExprNewRTVar( NULL, $1, ZH_COMP_PARAM ), ZH_TRUE );
               }
           ;

VarDef     : IdentName AsType
               {
                  zh_compVariableAdd( ZH_COMP_PARAM, $1, $2 );
                  if( ZH_COMP_PARAM->iVarScope & ZH_VSCOMP_STATIC )
                  {
                     zh_compStaticDefStart( ZH_COMP_PARAM );   /* switch to statics pcode buffer */
                     zh_compStaticDefEnd( ZH_COMP_PARAM, $1 );
                  }
                  else if( ZH_COMP_PARAM->iVarScope == ZH_VSCOMP_PUBLIC || ZH_COMP_PARAM->iVarScope == ZH_VSCOMP_PRIVATE )
                  {
                     zh_compRTVariableAdd( ZH_COMP_PARAM, zh_compExprNewRTVar( $1, NULL, ZH_COMP_PARAM ), ZH_FALSE );
                  }
                  else if( ZH_COMP_PARAM->iVarScope == ZH_VSCOMP_LOCAL &&
                           ( ZH_COMP_PARAM->functions.pLast->funFlags & ZH_FUNF_EXTBLOCK ) )
                  {
                     ZH_COMP_EXPR_FREE( zh_compExprGenPush( zh_compExprNewNil( ZH_COMP_PARAM ), ZH_COMP_PARAM ) );
                  }
               }
           | IdentName AsType { $<iNumber>$ = ZH_COMP_PARAM->iVarScope;
                                zh_compVariableAdd( ZH_COMP_PARAM, $1, $2 );
                              }
             INASSIGN Expression
               {
                  ZH_COMP_PARAM->iVarScope = $<iNumber>3;
                  if( ZH_COMP_PARAM->iVarScope & ZH_VSCOMP_STATIC )
                  {
                     zh_compStaticDefStart( ZH_COMP_PARAM );   /* switch to statics pcode buffer */
                     ZH_COMP_EXPR_FREE( zh_compExprGenStatement( zh_compExprAssignStatic( zh_compExprNewVar( $1, ZH_COMP_PARAM ), $5, ZH_COMP_PARAM ), ZH_COMP_PARAM ) );
                     zh_compStaticDefEnd( ZH_COMP_PARAM, $1 );
                  }
                  else if( ZH_COMP_PARAM->iVarScope == ZH_VSCOMP_PUBLIC || ZH_COMP_PARAM->iVarScope == ZH_VSCOMP_PRIVATE )
                  {
                     ZH_COMP_EXPR_FREE( zh_compExprGenPush( $5, ZH_COMP_PARAM ) );
                     zh_compRTVariableAdd( ZH_COMP_PARAM, zh_compExprNewRTVar( $1, NULL, ZH_COMP_PARAM ), ZH_TRUE );
                  }
                  else if( ZH_COMP_PARAM->iVarScope == ZH_VSCOMP_LOCAL &&
                           ( ZH_COMP_PARAM->functions.pLast->funFlags & ZH_FUNF_EXTBLOCK ) )
                  {
                     ZH_COMP_EXPR_FREE( zh_compExprGenPush( $5, ZH_COMP_PARAM ) );
                  }
                  else
                  {
                     ZH_COMP_EXPR_FREE( zh_compExprGenStatement( zh_compExprAssign( zh_compExprNewVar( $1, ZH_COMP_PARAM ), $5, ZH_COMP_PARAM ), ZH_COMP_PARAM ) );
                  }
                  ZH_COMP_PARAM->iVarScope = $<iNumber>3;
               }

           | IdentName DimList AsArrayType   { zh_compVariableDim( $1, $2, ZH_COMP_PARAM ); }
           ;

/* NOTE: DimList and DimIndex is the same as ArrayIndex and IndexList
 *       however we are using quite different actions here
 */
DimList    : DimIndex ']'
           ;

DimIndex   : '[' Expression               { $$ = zh_compExprNewArgList( $2, ZH_COMP_PARAM ); }
           | DimIndex ',' Expression      { $$ = zh_compExprAddListExpr( $1, $3 ); }
           | DimIndex ']' '[' Expression  { $$ = zh_compExprAddListExpr( $1, $4 ); }
           ;

FieldsDef  : FIELD { ZH_COMP_PARAM->iVarScope = ZH_VSCOMP_FIELD; }
             FieldList InAlias Crlf
             {
               if( $4 ) zh_compFieldSetAlias( ZH_COMP_PARAM, $4, $3 );
             }
           ;

FieldList  : IdentName AsType                { $$ = zh_compFieldsCount( ZH_COMP_PARAM ); zh_compVariableAdd( ZH_COMP_PARAM, $1, $2 ); }
           | FieldList ',' IdentName AsType  { zh_compVariableAdd( ZH_COMP_PARAM, $3, $4 ); }
           ;

InAlias    : /* no alias */   { $$ = NULL; }
           | IN IdentName     { $$ = $2; }
           ;

MemvarDef  : MEMVAR { ZH_COMP_PARAM->iVarScope = ZH_VSCOMP_MEMVAR; } MemvarList Crlf
           ;

MemvarList : IdentName AsType                   { zh_compVariableAdd( ZH_COMP_PARAM, $1, $2 ); }
           | MemvarList ',' IdentName AsType    { zh_compVariableAdd( ZH_COMP_PARAM, $3, $4 ); }
           ;

Declaration: DECLARE IdentName '(' { zh_compDeclaredAdd( ZH_COMP_PARAM, $2 ); ZH_COMP_PARAM->szDeclaredFun = $2; } DecList ')' AsType Crlf
             {
               if( ZH_COMP_PARAM->pLastDeclared )
               {
                 ZH_COMP_PARAM->pLastDeclared->cType = $7->cVarType;

                 if( ZH_TOUPPER( $7->cVarType ) == 'S' )
                 {
                   ZH_COMP_PARAM->pLastDeclared->pClass = zh_compClassFind( ZH_COMP_PARAM, $7->szFromClass );
                   if( ! ZH_COMP_PARAM->pLastDeclared->pClass )
                   {
                     zh_compGenWarning( ZH_COMP_PARAM, zh_comp_szWarnings, 'W', ZH_COMP_WARN_CLASS_NOT_FOUND, $7->szFromClass, ZH_COMP_PARAM->pLastDeclared->szName );
                     ZH_COMP_PARAM->pLastDeclared->cType = ( ZH_ISUPPER( ( ZH_UCHAR ) $7->cVarType ) ? 'O' : 'o' );
                   }
                 }
               }
               ZH_COMP_PARAM->szDeclaredFun = NULL;
               ZH_COMP_PARAM->iVarScope = ZH_VSCOMP_NONE;
             }
           | DECLARE IdentName { ZH_COMP_PARAM->pLastClass = zh_compClassAdd( ZH_COMP_PARAM, $2, NULL ); } ClassInfo Crlf { ZH_COMP_PARAM->iVarScope = ZH_VSCOMP_NONE; }
           | DECLARE_CLASS IdentName Crlf { ZH_COMP_PARAM->pLastClass = zh_compClassAdd( ZH_COMP_PARAM, $2, NULL ); ZH_COMP_PARAM->iVarScope = ZH_VSCOMP_NONE; }
           | DECLARE_CLASS IdentName IdentName Crlf { ZH_COMP_PARAM->pLastClass = zh_compClassAdd( ZH_COMP_PARAM, $2, $3 ); ZH_COMP_PARAM->iVarScope = ZH_VSCOMP_NONE; }
           | DECLARE_MEMBER DecMethod Crlf { ZH_COMP_PARAM->iVarScope = ZH_VSCOMP_NONE; }
           | DECLARE_MEMBER '{' AsType { ZH_COMP_PARAM->cDataListType = $3->cVarType; } DecDataList '}' Crlf { ZH_COMP_PARAM->cDataListType = 0; ZH_COMP_PARAM->iVarScope = ZH_VSCOMP_NONE; }
           ;

DecDataList: DecData
           | DecDataList ',' DecData
           ;

ClassInfo  : DecMethod
           | ClassInfo DecMethod
           | DecData
           | ClassInfo DecData
           ;

DecMethod  : IdentName '(' { ZH_COMP_PARAM->pLastMethod = zh_compMethodAdd( ZH_COMP_PARAM, ZH_COMP_PARAM->pLastClass, $1 ); } DecListExt ')' AsType
             {
               if( ZH_COMP_PARAM->pLastMethod )
               {
                 ZH_COMP_PARAM->pLastMethod->cType = $6->cVarType;
                 if( ZH_TOUPPER( $6->cVarType ) == 'S' )
                 {
                   ZH_COMP_PARAM->pLastMethod->pClass = zh_compClassFind( ZH_COMP_PARAM, $6->szFromClass );
                   if( ! ZH_COMP_PARAM->pLastMethod->pClass )
                   {
                     zh_compGenWarning( ZH_COMP_PARAM, zh_comp_szWarnings, 'W', ZH_COMP_WARN_CLASS_NOT_FOUND, $6->szFromClass, ZH_COMP_PARAM->pLastMethod->szName );
                     ZH_COMP_PARAM->pLastMethod->cType = ( ZH_ISUPPER( ( ZH_UCHAR ) $6->cVarType ) ? 'O' : 'o' );
                   }
                 }
               }
               ZH_COMP_PARAM->pLastMethod = NULL;
             }
           ;

DecData    : IdentName { ZH_COMP_PARAM->pLastMethod = zh_compMethodAdd( ZH_COMP_PARAM, ZH_COMP_PARAM->pLastClass, $1 ); } AsType
             {
               if( ZH_COMP_PARAM->pLastMethod )
               {
                  PZH_HCLASS pClass;
                  char       szSetData[ ZH_SYMBOL_NAME_LEN + 1 ];
                  int        iLen;
                  ZH_BYTE    cVarType = $3->cVarType;

                  /* List Type overrides if exists. */
                  if( ZH_COMP_PARAM->cDataListType )
                     cVarType = ZH_COMP_PARAM->cDataListType;

                  ZH_COMP_PARAM->pLastMethod->cType = cVarType;
                  if( ZH_TOUPPER( cVarType ) == 'S' )
                  {
                     pClass = zh_compClassFind( ZH_COMP_PARAM, $3->szFromClass );
                     ZH_COMP_PARAM->pLastMethod->pClass = pClass;
                     if( ! ZH_COMP_PARAM->pLastMethod->pClass )
                     {
                        zh_compGenWarning( ZH_COMP_PARAM, zh_comp_szWarnings, 'W', ZH_COMP_WARN_CLASS_NOT_FOUND, $3->szFromClass, ZH_COMP_PARAM->pLastMethod->szName );
                        ZH_COMP_PARAM->pLastMethod->cType = ( ZH_ISUPPER( ( ZH_UCHAR ) cVarType ) ? 'O' :'o' );
                     }
                  }
                  else
                     pClass = NULL;

                  iLen = ( int ) strlen( $1 );
                  if( iLen >= ZH_SYMBOL_NAME_LEN )
                     iLen = ZH_SYMBOL_NAME_LEN - 1;
                  szSetData[ 0 ] = '_';
                  memcpy( szSetData + 1, $1, iLen );
                  szSetData[ iLen + 1 ] = '\0';

                  ZH_COMP_PARAM->pLastMethod = zh_compMethodAdd( ZH_COMP_PARAM, ZH_COMP_PARAM->pLastClass,
                     zh_compIdentifierNew( ZH_COMP_PARAM, szSetData, ZH_IDENT_COPY ) );
                  ZH_COMP_PARAM->pLastMethod->cType = cVarType;
                  ZH_COMP_PARAM->pLastMethod->iParamCount = 1;

                  ZH_COMP_PARAM->pLastMethod->cParamTypes = ( ZH_BYTE * ) zh_xgrab( 1 );
                  ZH_COMP_PARAM->pLastMethod->pParamClasses = ( PZH_HCLASS * ) zh_xgrab( sizeof( ZH_HCLASS ) );

                  ZH_COMP_PARAM->pLastMethod->cParamTypes[ 0 ] = cVarType;
                  ZH_COMP_PARAM->pLastMethod->pParamClasses[ 0 ] = pClass;

                  if( ZH_TOUPPER( cVarType ) == 'S' )
                  {
                     ZH_COMP_PARAM->pLastMethod->pClass = pClass;
                  }
               }

               ZH_COMP_PARAM->pLastMethod = NULL;
             }
           ;

DecList    : /* Nothing */
           | FormalList
           | OptList
           | FormalList ',' OptList
           ;

DecListExt : /* Nothing */
           | FormalList
           | OptList
           | EPSILON
           | FormalList ',' EPSILON
           | FormalList ',' OptList
           | FormalList ',' OptList ',' EPSILON
           ;

DummyArgList : DummyArgument
             | DummyArgList ',' DummyArgument
             ;

DummyArgument : EmptyExpression     { ZH_COMP_EXPR_FREE( $1 ); }
              ;

FormalList : IdentName AsType                                  { zh_compDeclaredParameterAdd( ZH_COMP_PARAM, $1, $2 ); }
           | '@' IdentName AsType                              { zh_compDeclaredParameterAdd( ZH_COMP_PARAM, $2, zh_compVarTypeNew( ZH_COMP_PARAM, $3->cVarType + ZH_VT_OFFSET_BYREF, NULL ) ); }
           | '@' IdentName '(' DummyArgList ')'                { zh_compDeclaredParameterAdd( ZH_COMP_PARAM, $2, zh_compVarTypeNew( ZH_COMP_PARAM, 'F', NULL ) ); }
           | FormalList ',' IdentName AsType                   { zh_compDeclaredParameterAdd( ZH_COMP_PARAM, $3, $4 ); }
           | FormalList ',' '@' IdentName AsType               { zh_compDeclaredParameterAdd( ZH_COMP_PARAM, $4, zh_compVarTypeNew( ZH_COMP_PARAM, $5->cVarType + ZH_VT_OFFSET_BYREF, NULL ) ); }
           | FormalList ',' '@' IdentName '(' DummyArgList ')' { zh_compDeclaredParameterAdd( ZH_COMP_PARAM, $4, zh_compVarTypeNew( ZH_COMP_PARAM, 'F', NULL ) ); }
           ;

OptList    : OPTIONAL IdentName AsType                               { zh_compDeclaredParameterAdd( ZH_COMP_PARAM, $2, zh_compVarTypeNew( ZH_COMP_PARAM, $3->cVarType + ZH_VT_OFFSET_OPTIONAL, NULL ) ); }
           | OPTIONAL '@' IdentName AsType                           { zh_compDeclaredParameterAdd( ZH_COMP_PARAM, $3, zh_compVarTypeNew( ZH_COMP_PARAM, $4->cVarType + ZH_VT_OFFSET_OPTIONAL + ZH_VT_OFFSET_BYREF, NULL ) ); }
           | OPTIONAL '@' IdentName '(' DummyArgList ')'             { zh_compDeclaredParameterAdd( ZH_COMP_PARAM, $3, zh_compVarTypeNew( ZH_COMP_PARAM, 'F' + ZH_VT_OFFSET_OPTIONAL + ZH_VT_OFFSET_BYREF, NULL ) ); }
           | OptList ',' OPTIONAL IdentName AsType                   { zh_compDeclaredParameterAdd( ZH_COMP_PARAM, $4, zh_compVarTypeNew( ZH_COMP_PARAM, $5->cVarType + ZH_VT_OFFSET_OPTIONAL, NULL ) ); }
           | OptList ',' OPTIONAL '@' IdentName AsType               { zh_compDeclaredParameterAdd( ZH_COMP_PARAM, $5, zh_compVarTypeNew( ZH_COMP_PARAM, $6->cVarType + ZH_VT_OFFSET_OPTIONAL + ZH_VT_OFFSET_BYREF, NULL ) ); }
           | OptList ',' OPTIONAL '@' IdentName '(' DummyArgList ')' { zh_compDeclaredParameterAdd( ZH_COMP_PARAM, $5, zh_compVarTypeNew( ZH_COMP_PARAM, 'F' + ZH_VT_OFFSET_OPTIONAL + ZH_VT_OFFSET_BYREF, NULL ) ); }
           ;

ExecFlow   : IfEndif
           | DoCase
           | DoWhile
           | ForNext
           | BeginSeq
           | ForEach
           | DoSwitch
           | WithObject
           ;

ErrEndFor    : NEXT      { zh_compErrStru( ZH_COMP_PARAM, ZH_COMP_ERR_NEXTFOR ); }
             ;

ErrEndCase   : ENDCASE   { zh_compErrStru( ZH_COMP_PARAM, ZH_COMP_ERR_ENDCASE ); }
             ;

ErrEndIf     : ENDIF     { zh_compErrStru( ZH_COMP_PARAM, ZH_COMP_ERR_ENDIF ); }
             ;

ErrEndWith   : ENDWITH   { zh_compErrStru( ZH_COMP_PARAM, ZH_COMP_ERR_ENDWITH ); }
             ;

ErrEndSeq    : ENDSEQ    { zh_compErrStru( ZH_COMP_PARAM, ZH_COMP_ERR_ENDSEQ ); }
             ;

ErrEndWhile  : ENDDO     { zh_compErrStru( ZH_COMP_PARAM, ZH_COMP_ERR_ENDDO ); }
             ;

ErrEndSwitch : ENDSWITCH { zh_compErrStru( ZH_COMP_PARAM, ZH_COMP_ERR_ENDSWITCH ); }
             ;

IfEndif    : IfBegin EndIf                    { zh_compGenJumpHere( $1, ZH_COMP_PARAM ); }
           | IfBegin IfElse EndIf             { zh_compGenJumpHere( $1, ZH_COMP_PARAM ); }
           | IfBegin IfElseIf EndIf           { zh_compGenJumpHere( $1, ZH_COMP_PARAM ); zh_compElseIfFix( ZH_COMP_PARAM, $2 ); }
           | IfBegin IfElseIf IfElse EndIf    { zh_compGenJumpHere( $1, ZH_COMP_PARAM ); zh_compElseIfFix( ZH_COMP_PARAM, $2 ); }
           ;

IfBegin    : IF ExpList
               { ++ZH_COMP_PARAM->functions.pLast->wIfCounter; zh_compLinePushIfInside( ZH_COMP_PARAM ); }
             Crlf
               { ZH_COMP_EXPR_FREE( zh_compExprGenPush( $2, ZH_COMP_PARAM ) ); $<sNumber>$ = zh_compGenJumpFalse( 0, ZH_COMP_PARAM ); }
             EmptyStats
               { $$ = zh_compGenJump( 0, ZH_COMP_PARAM ); zh_compGenJumpHere( $<sNumber>5, ZH_COMP_PARAM ); }
           ;

IfElse     : ELSE Crlf { ZH_COMP_PARAM->functions.pLast->funFlags &= ~ ZH_FUNF_BREAK_CODE; }
               EmptyStats
           ;

IfElseIf   : ELSEIF { ZH_COMP_PARAM->functions.pLast->funFlags &= ~ ZH_FUNF_BREAK_CODE; zh_compLinePush( ZH_COMP_PARAM ); }
               ExpList Crlf
               { ZH_COMP_EXPR_FREE( zh_compExprGenPush( $3, ZH_COMP_PARAM ) );
                  $<sNumber>$ = zh_compGenJumpFalse( 0, ZH_COMP_PARAM );
               }
               EmptyStats
               { $$ = zh_compElseIfGen( ZH_COMP_PARAM, NULL, zh_compGenJump( 0, ZH_COMP_PARAM ) );
                  zh_compGenJumpHere( $<sNumber>5, ZH_COMP_PARAM );
               }

           | IfElseIf ELSEIF { ZH_COMP_PARAM->functions.pLast->funFlags &= ~ ZH_FUNF_BREAK_CODE; zh_compLinePush( ZH_COMP_PARAM ); }
               ExpList Crlf
               { ZH_COMP_EXPR_FREE( zh_compExprGenPush( $4, ZH_COMP_PARAM ) );
                  $<sNumber>$ = zh_compGenJumpFalse( 0, ZH_COMP_PARAM );
               }
               EmptyStats
               { $$ = zh_compElseIfGen( ZH_COMP_PARAM, $1, zh_compGenJump( 0, ZH_COMP_PARAM ) );
                  zh_compGenJumpHere( $<sNumber>6, ZH_COMP_PARAM );
               }
           ;

EndIf      : EndIfID
               {
                  if( ZH_COMP_PARAM->functions.pLast->wIfCounter )
                     --ZH_COMP_PARAM->functions.pLast->wIfCounter;
                  ZH_COMP_PARAM->functions.pLast->funFlags &= ~ ( ZH_FUNF_WITH_RETURN | ZH_FUNF_BREAK_CODE );
               }
           ;

EndIfID    : ENDIF
           | END
           | ENDERR { zh_compErrUnclosed( ZH_COMP_PARAM, "IF" ); }
           | ErrEndFor
           | ErrEndCase
           | ErrEndWhile
           | ErrEndWith
           | ErrEndSeq
           | ErrEndSwitch
           ;

DoCase     : DoCaseBegin
                Cases
             EndCase                { zh_compElseIfFix( ZH_COMP_PARAM, $2 ); }

           | DoCaseBegin
                Otherwise
             EndCase

           | DoCaseBegin
             EndCase

           | DoCaseBegin
                Cases
                Otherwise
             EndCase                { zh_compElseIfFix( ZH_COMP_PARAM, $2 ); }
           ;

EndCase    : EndCaseID
               {  if( ZH_COMP_PARAM->functions.pLast->wCaseCounter )
                     --ZH_COMP_PARAM->functions.pLast->wCaseCounter;
                  ZH_COMP_PARAM->functions.pLast->funFlags &= ~ ( ZH_FUNF_WITH_RETURN | ZH_FUNF_BREAK_CODE );
               }
           ;

EndCaseID  : ENDCASE
           | END
           | ENDERR { zh_compErrUnclosed( ZH_COMP_PARAM, "CASE" ); }
           | ErrEndIf
           | ErrEndFor
           | ErrEndWhile
           | ErrEndWith
           | ErrEndSeq
           | ErrEndSwitch
           ;

DoCaseStart : DOCASE { ++ZH_COMP_PARAM->functions.pLast->wCaseCounter; zh_compLinePushIfDebugger( ZH_COMP_PARAM );} Crlf
            ;

DoCaseBegin : DoCaseStart
            | DoCaseStart Statements {
                        if( $<lNumber>2 > 0 )
                        {
                           zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_MAYHEM_IN_CASE, NULL, NULL );
                        }
                     }
           ;

Cases      : CASE { zh_compLinePushIfInside( ZH_COMP_PARAM ); } ExpList Crlf
               {
                  ZH_COMP_EXPR_FREE( zh_compExprGenPush( $3, ZH_COMP_PARAM ) );
                  $<sNumber>$ = zh_compGenJumpFalse( 0, ZH_COMP_PARAM );
               }
             EmptyStats
               {
                  ZH_COMP_PARAM->functions.pLast->funFlags &= ~ ZH_FUNF_BREAK_CODE;
                  $$ = zh_compElseIfGen( ZH_COMP_PARAM, NULL, zh_compGenJump( 0, ZH_COMP_PARAM ) );
                  zh_compGenJumpHere( $<sNumber>5, ZH_COMP_PARAM );
               }

           | Cases CASE { zh_compLinePushIfInside( ZH_COMP_PARAM ); } ExpList Crlf
               {
                  ZH_COMP_EXPR_FREE( zh_compExprGenPush( $4, ZH_COMP_PARAM ) );
                  $<sNumber>$ = zh_compGenJumpFalse( 0, ZH_COMP_PARAM );
               }
             EmptyStats
               {
                  ZH_COMP_PARAM->functions.pLast->funFlags &= ~ ZH_FUNF_BREAK_CODE;
                  $$ = zh_compElseIfGen( ZH_COMP_PARAM, $1, zh_compGenJump( 0, ZH_COMP_PARAM ) );
                  zh_compGenJumpHere( $<sNumber>6, ZH_COMP_PARAM );
               }
           ;

Otherwise  : OTHERWISE {zh_compLinePushIfDebugger( ZH_COMP_PARAM ); } Crlf { ZH_COMP_PARAM->functions.pLast->funFlags &= ~ ZH_FUNF_BREAK_CODE; }
                EmptyStats
           | Otherwise OTHERWISE { zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_MAYHEM_IN_CASE, NULL, NULL ); } Crlf
                EmptyStats
           ;

DoWhile    : WhileBegin ExpList Crlf
               {
                  ZH_COMP_EXPR_FREE( zh_compExprGenPush( $2, ZH_COMP_PARAM ) );
                  $<sNumber>$ = zh_compGenJumpFalse( 0, ZH_COMP_PARAM );
               }
             EmptyStats
               {
                  zh_compLoopHere( ZH_COMP_PARAM );
                  zh_compGenJump( $1 - ZH_COMP_PARAM->functions.pLast->nPCodePos, ZH_COMP_PARAM );
               }
             EndWhile
               {
                  zh_compGenJumpHere( $<sNumber>4, ZH_COMP_PARAM );
                  if( ZH_COMP_PARAM->functions.pLast->wWhileCounter )
                     --ZH_COMP_PARAM->functions.pLast->wWhileCounter;
                  zh_compLoopEnd( ZH_COMP_PARAM );
                  ZH_COMP_PARAM->functions.pLast->funFlags &= ~ ZH_FUNF_WITH_RETURN;
               }
           ;

WhileBegin : WHILE
               {
                  $$ = ZH_COMP_PARAM->functions.pLast->nPCodePos;
                  zh_compLinePushIfInside( ZH_COMP_PARAM );
                  ++ZH_COMP_PARAM->functions.pLast->wWhileCounter;
                  zh_compLoopStart( ZH_COMP_PARAM, ZH_TRUE );
               }
           ;

EndWhile   : EndWhileID
               { ZH_COMP_PARAM->functions.pLast->funFlags &= ~ ZH_FUNF_BREAK_CODE; }
           ;

EndWhileID : ENDDO
           | END
           | ENDERR { zh_compErrUnclosed( ZH_COMP_PARAM, "WHILE" ); }
           | ErrEndIf
           | ErrEndFor
           | ErrEndCase
           | ErrEndWith
           | ErrEndSeq
           | ErrEndSwitch
           ;

ForNext    : FOR LValue ForAssign Expression          /* 1  2  3  4 */
               {                                      /* 5 */
                  zh_compLinePushIfInside( ZH_COMP_PARAM );
                  $<iNumber>1 = ZH_COMP_PARAM->currLine;
                  zh_compDebugStart();
                  ++ZH_COMP_PARAM->functions.pLast->wForCounter;
                  $2 = zh_compExprReduce( $2, ZH_COMP_PARAM );
                  $<asExpr>$ = zh_compExprGenPush( zh_compExprAssign( $2, $4, ZH_COMP_PARAM ), ZH_COMP_PARAM );
                  if( zh_compExprAsSymbol( $2 ) )
                  {
                     zh_compForStart( ZH_COMP_PARAM, zh_compExprAsSymbol( $2 ), 0 );
                  }
               }
             TO ExpList StepExpr                      /* 6  7  8 */
               {                                      /* 9 */
                  zh_compLoopStart( ZH_COMP_PARAM, ZH_TRUE );
                  $<sNumber>$ = zh_compGenJump( 0, ZH_COMP_PARAM );
               }
             Crlf                                     /* 10 */
               {                                      /* 11 */
                  $<sNumber>$ = ZH_COMP_PARAM->functions.pLast->nPCodePos;
               }
             ForStatements                            /* 12 */
               {
                  int iSign, iLine;

                  zh_compLoopHere( ZH_COMP_PARAM );

                  iLine = ZH_COMP_PARAM->currLine;
                  ZH_COMP_PARAM->currLine = $<iNumber>1;
                  zh_compLinePush( ZH_COMP_PARAM );
                  ZH_COMP_PARAM->currLine = iLine;

                  if( $<asExpr>8 )
                  {
                     $<asExpr>8 = zh_compExprReduce( $<asExpr>8, ZH_COMP_PARAM );
                     iSign = zh_compExprAsNumSign( $<asExpr>8 );
                     ZH_COMP_EXPR_CLEAR( zh_compExprGenPush( zh_compExprSetOperand( zh_compExprNewPlusEq( $2, ZH_COMP_PARAM ), $<asExpr>8, ZH_COMP_PARAM ), ZH_COMP_PARAM ) );
                  }
                  else
                  {
                     iSign = 1;
                     ZH_COMP_EXPR_CLEAR( zh_compExprGenPush( zh_compExprNewPreInc( $2, ZH_COMP_PARAM ), ZH_COMP_PARAM ) );
                  }
                  zh_compGenJumpHere( $<sNumber>9, ZH_COMP_PARAM );
                  ZH_COMP_EXPR_FREE( zh_compExprGenPush( $7, ZH_COMP_PARAM ) );   /* end */
                  if( iSign )
                  {
                     zh_compGenPCode1( ( ZH_BYTE ) ( iSign > 0 ? ZH_P_GREATER : ZH_P_LESS ), ZH_COMP_PARAM );
                     if( $<asExpr>8 )
                        ZH_COMP_EXPR_FREE( $<asExpr>8 );
                  }
                  else
                  {
                     ZH_COMP_EXPR_FREE( zh_compExprGenPush( $<asExpr>8, ZH_COMP_PARAM ) );   /* step */
                     zh_compGenPCode1( ZH_P_FORTEST, ZH_COMP_PARAM );
                  }

                  zh_compGenJumpFalse( $<sNumber>11 - ZH_COMP_PARAM->functions.pLast->nPCodePos, ZH_COMP_PARAM );
                  zh_compLoopEnd( ZH_COMP_PARAM );
                  if( zh_compExprAsSymbol( $2 ) )
                     zh_compForEnd( ZH_COMP_PARAM, zh_compExprAsSymbol( $2 ) );
                  ZH_COMP_EXPR_FREE( $<asExpr>5 );  /* deletes $5, $2, $4 */
                  ZH_COMP_PARAM->functions.pLast->funFlags &= ~ ( ZH_FUNF_WITH_RETURN | ZH_FUNF_BREAK_CODE );
               }
           ;

ForAssign  : '='
           | INASSIGN
           ;

StepExpr   : /* default step expression */       { $<asExpr>$ = NULL; }
           | STEP ExpList                        { $<asExpr>$ = zh_compExprReduce( $2, ZH_COMP_PARAM ); }
           ;

ForStatements : EmptyStats EndForID
                  {
                     zh_compLinePush( ZH_COMP_PARAM );
                     if( ZH_COMP_PARAM->functions.pLast->wForCounter )
                        --ZH_COMP_PARAM->functions.pLast->wForCounter;
                  }
              ;

EndForID   : NEXT
           | NEXT IdentName
           | END
           | ENDERR { zh_compErrUnclosed( ZH_COMP_PARAM, "FOR" ); }
           | ErrEndIf
           | ErrEndCase
           | ErrEndWhile
           | ErrEndWith
           | ErrEndSeq
           | ErrEndSwitch
           ;

ForVar     : IdentName     { $$ = zh_compExprNewVarRef( $1, ZH_COMP_PARAM ); }
           | AliasVar      { $$ = zh_compExprNewRef( $1, ZH_COMP_PARAM ); }
           ;

ForList    : ForVar              { $$ = zh_compExprNewArgList( $1, ZH_COMP_PARAM ); }
           | ForList ',' ForVar  { $$ = zh_compExprAddListExpr( $1, $3 ); }
           ;

ForExpr    : '@' IdentName       { $$ = zh_compExprNewVarRef( $2, ZH_COMP_PARAM ); }
           | Expression
           ;

ForArgs    : ForExpr             { $$ = zh_compExprNewArgList( $1, ZH_COMP_PARAM ); }
           | ForArgs ',' ForExpr { $$ = zh_compExprAddListExpr( $1, $3 ); }
           ;


ForEach    : FOREACH ForList IN ForArgs          /* 1  2  3  4 */
             {
                ++ZH_COMP_PARAM->functions.pLast->wForCounter;    /* 5 */
                zh_compLinePushIfInside( ZH_COMP_PARAM );
                zh_compDebugStart();
             }
             Descend    /* 6 */
             {
                /* 7 */
                $2 = zh_compExprReduce( $2, ZH_COMP_PARAM );
                $4 = zh_compExprReduce( $4, ZH_COMP_PARAM );
                zh_compEnumStart( ZH_COMP_PARAM, $2, $4, $6 );

                zh_compLoopStart( ZH_COMP_PARAM, ZH_TRUE );
                $<sNumber>$ = ZH_COMP_PARAM->functions.pLast->nPCodePos;
             }
             Crlf                                     /* 8 */
             {
                /* 9 */
                $<sNumber>$ = zh_compGenJumpFalse( 0, ZH_COMP_PARAM );
             }
             ForStatements                            /* 10 */
             {
                zh_compLoopHere( ZH_COMP_PARAM );
                zh_compEnumNext( ZH_COMP_PARAM, $2, $6 );
                zh_compGenJump( $<sNumber>7 - ZH_COMP_PARAM->functions.pLast->nPCodePos, ZH_COMP_PARAM );

                zh_compGenJumpHere( $<sNumber>9, ZH_COMP_PARAM );
                zh_compLoopEnd( ZH_COMP_PARAM );
                ZH_COMP_PARAM->functions.pLast->funFlags &= ~ ( ZH_FUNF_WITH_RETURN | ZH_FUNF_BREAK_CODE );
                zh_compEnumEnd( ZH_COMP_PARAM, $2 );
                ZH_COMP_EXPR_FREE( $2 );
                ZH_COMP_EXPR_FREE( $4 );
             }
           ;

Descend    : /* default up */     { $$ =  1; }
           | DESCEND              { $$ = -1; }
           ;

DoSwitch    : SwitchBegin
               {
                  zh_compLoopStart( ZH_COMP_PARAM, ZH_FALSE );
                  zh_compSwitchStart( ZH_COMP_PARAM, $1 );
                  zh_compGenJump( 0, ZH_COMP_PARAM );
               }
               SwitchCases
               EndSwitch
               {
                  zh_compSwitchEnd( ZH_COMP_PARAM );
                  zh_compLoopEnd( ZH_COMP_PARAM );
               }

            | SwitchBegin
              EndSwitch
               {
                  ZH_COMP_EXPR_FREE( $1 );
               }
            ;

EndSwitch   : EndSwitchID
               {
                  if( ZH_COMP_PARAM->functions.pLast->wSwitchCounter )
                     --ZH_COMP_PARAM->functions.pLast->wSwitchCounter;
                  ZH_COMP_PARAM->functions.pLast->funFlags &= ~ ( ZH_FUNF_WITH_RETURN | ZH_FUNF_BREAK_CODE );
               }
            ;

EndSwitchID : ENDSWITCH
            | END
            | ENDERR { zh_compErrUnclosed( ZH_COMP_PARAM, "SWITCH" ); }
            | ErrEndIf
            | ErrEndFor
            | ErrEndCase
            | ErrEndWhile
            | ErrEndWith
            | ErrEndSeq
            ;

SwitchStart : DOSWITCH
               {
                  ++ZH_COMP_PARAM->functions.pLast->wSwitchCounter;
                  zh_compLinePushIfInside( ZH_COMP_PARAM );
               }
              Expression Crlf
               {
                  $$ = zh_compExprReduce( $3, ZH_COMP_PARAM );
               }
            ;

SwitchBegin : SwitchStart
            | SwitchStart Statements
               {
                  if( $<lNumber>2 > 0 )
                  {
                     zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_MAYHEM_IN_CASE, NULL, NULL );
                  }
               }
            ;

SwitchCases : CASE Expression { zh_compSwitchAdd( ZH_COMP_PARAM, $2 ); zh_compLinePush( ZH_COMP_PARAM ); } Crlf
              EmptyStats

            | SwitchCases CASE Expression { zh_compSwitchAdd( ZH_COMP_PARAM, $3 ); zh_compLinePush( ZH_COMP_PARAM ); } Crlf
              EmptyStats

            | SwitchDefault

            | SwitchCases SwitchDefault
            ;

SwitchDefault : OTHERWISE { zh_compSwitchAdd( ZH_COMP_PARAM, NULL ); zh_compLinePush( ZH_COMP_PARAM ); } Crlf { ZH_COMP_PARAM->functions.pLast->funFlags &= ~ ZH_FUNF_BREAK_CODE; }
                EmptyStats
              ;

BeginSeq    : BEGINSEQ        /* 1 */
               {              /* 2 */
                  zh_compLinePushIfInside( ZH_COMP_PARAM );
                  ++ZH_COMP_PARAM->functions.pLast->wSeqCounter;
                  ++ZH_COMP_PARAM->functions.pLast->wSeqBegCounter;
                  $<sNumber>$ = zh_compSequenceBegin( ZH_COMP_PARAM );
               }
               BlockSeq       /* 3 */
               Crlf           /* 4 */
               EmptyStats     /* 5 */
               {              /* 6 */
                  /* Set jump address for ZH_P_SEQBEGIN opcode - this address
                   * will be used in BREAK code if there is no RECOVER clause
                   */
                  if( $3 )
                     zh_compGenPCode1( ZH_P_POP, ZH_COMP_PARAM );
                  zh_compGenJumpHere( $<sNumber>2, ZH_COMP_PARAM );
                  $<sNumber>$ = zh_compSequenceEnd( ZH_COMP_PARAM );
                  $<lNumber>4 = zh_compLoopCount( ZH_COMP_PARAM );
               }
               RecoverSeq     /* 7 */
               {              /* 8 */
                  /* Replace END address with RECOVER address in
                   * ZH_P_SEQBEGIN opcode if there is RECOVER clause
                   */
                  if( $7 )
                     zh_compGenJumpThere( $<sNumber>2, $7, ZH_COMP_PARAM );
               }
               AlwaysSeq      /* 9 */
               {              /* 10 */
                  long lLoopCount = zh_compLoopCount( ZH_COMP_PARAM );
                  ZH_COMP_PARAM->functions.pLast->funFlags &= ~ ( ZH_FUNF_WITH_RETURN | ZH_FUNF_BREAK_CODE );
                  if( $9 )
                  {
                     if( $<lNumber>4 != lLoopCount )
                     {
                        /* ALWAYS statement after RECOVER with EXIT/LOOP statements */
                        zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_ALWAYS_AFTER_EXIT, "EXIT/LOOP", NULL );
                     }
                     --ZH_COMP_PARAM->functions.pLast->wAlwaysCounter;
                     /* replace END address with ALWAYS address in
                        ZH_P_SEQEND opcode */
                     zh_compGenJumpThere( $<sNumber>6, $9, ZH_COMP_PARAM );
                     /* Fix ALWAYS address in ZH_P_SEQALWAYS opcode */
                     zh_compGenJumpThere( $<sNumber>2 - 4, $9, ZH_COMP_PARAM );
                     /* Fix ALWAYSEND address in ZH_P_ALWAYSBEGIN opcode */
                     zh_compGenJumpHere( $9 + 1, ZH_COMP_PARAM );
                     zh_compGenPCode1( ZH_P_ALWAYSEND, ZH_COMP_PARAM );
                  }
                  else
                  {
                     /* Fix END address in ZH_P_SEQEND opcode */
                     zh_compGenJumpHere( $<sNumber>6, ZH_COMP_PARAM );
                  }
                  zh_compSequenceFinish( ZH_COMP_PARAM, $<sNumber>2, $<sNumber>6, $9,
                                         $<lNumber>5 != 0, $7 != 0, $<lNumber>4 == lLoopCount );
               }
               EndSeqID       /* 10 */
               {
                  if( ZH_COMP_PARAM->functions.pLast->wSeqBegCounter )
                     --ZH_COMP_PARAM->functions.pLast->wSeqBegCounter;
               }
            ;

EndSeqID    : ENDSEQ
            | END
            | ENDERR { zh_compErrUnclosed( ZH_COMP_PARAM, "BEGIN SEQUENCE" ); }
            | ErrEndIf
            | ErrEndFor
            | ErrEndCase
            | ErrEndWhile
            | ErrEndWith
            | ErrEndSwitch
            ;

BlockSeq    : /* no always */    { $$ = 0; }
            | WITH Expression
               {
                  ZH_COMP_EXPR_FREE( zh_compExprGenPush( $2, ZH_COMP_PARAM ) );
                  zh_compGenPCode1( ZH_P_SEQBLOCK, ZH_COMP_PARAM );
                  $$ = ZH_COMP_PARAM->functions.pLast->nPCodePos;
               }
            ;

AlwaysSeq   : /* no always */    { $$ = 0; }
            | Always Crlf EmptyStats
            ;

Always      : ALWAYS
               {
                  ZH_COMP_PARAM->functions.pLast->funFlags &= ~ ( ZH_FUNF_WITH_RETURN | ZH_FUNF_BREAK_CODE );
                  $$ = ZH_COMP_PARAM->functions.pLast->nPCodePos;
                  ++ZH_COMP_PARAM->functions.pLast->wAlwaysCounter;
                  zh_compSequenceAlways( ZH_COMP_PARAM );
               }
            ;

RecoverSeq  : /* no recover */
               {
                  $$ = 0;
                  if( ZH_COMP_PARAM->functions.pLast->wSeqCounter )
                     --ZH_COMP_PARAM->functions.pLast->wSeqCounter;
                  ZH_COMP_PARAM->functions.pLast->funFlags &= ~ ZH_FUNF_BREAK_CODE;
               }
            | RecoverEmpty Crlf EmptyStats
            | RecoverUsing Crlf EmptyStats
            ;

RecoverEmpty : RECOVER
               {
                  $$ = ZH_COMP_PARAM->functions.pLast->nPCodePos;
                  if( ZH_COMP_PARAM->functions.pLast->wSeqCounter )
                     --ZH_COMP_PARAM->functions.pLast->wSeqCounter;
                  ZH_COMP_PARAM->functions.pLast->funFlags &= ~ ZH_FUNF_BREAK_CODE;
                  zh_compLinePushIfInside( ZH_COMP_PARAM );
                  zh_compGenPCode2( ZH_P_SEQRECOVER, ZH_P_POP, ZH_COMP_PARAM );
               }
             ;

RecoverUsing : RECOVERUSING IdentName
               {
                  $$ = ZH_COMP_PARAM->functions.pLast->nPCodePos;
                  if( ZH_COMP_PARAM->functions.pLast->wSeqCounter )
                     --ZH_COMP_PARAM->functions.pLast->wSeqCounter;
                  ZH_COMP_PARAM->functions.pLast->funFlags &= ~ ZH_FUNF_BREAK_CODE;
                  zh_compLinePushIfInside( ZH_COMP_PARAM );
                  zh_compGenPCode1( ZH_P_SEQRECOVER, ZH_COMP_PARAM );
                  zh_compGenPopVar( $2, ZH_COMP_PARAM );
               }
             ;


DoProc     : DO MacroAny DoArgs
               {
                  $$ = zh_compExprNewFunCall( $2, $3, ZH_COMP_PARAM );
               }
           | DOIDENT DoArgs
               {
                  zh_compModuleAdd( ZH_COMP_PARAM, $1, ZH_FALSE );
                  /* DOIDENT is the only one identifier which can be returned in lower letters */
                  $$ = zh_compExprNewFunCall( zh_compExprNewFunName( zh_compIdentifierNew( ZH_COMP_PARAM, zh_strupr( zh_strdup( $1 ) ), ZH_IDENT_FREE ), ZH_COMP_PARAM ), $2, ZH_COMP_PARAM );
               }
           ;

DoArgs     : /* empty */      { $$ = NULL; }
           | WITH DoArgList   { $$ = $2; }
           ;

DoArgList  : ','                       { $$ = zh_compExprAddListExpr( zh_compExprNewArgList( zh_compExprNewNil( ZH_COMP_PARAM ), ZH_COMP_PARAM ), zh_compExprNewNil( ZH_COMP_PARAM ) ); }
           | ',' DoArgument            { $$ = zh_compExprAddListExpr( zh_compExprNewArgList( zh_compExprNewNil( ZH_COMP_PARAM ), ZH_COMP_PARAM ), $2 ); }
           | DoArgument                { $$ = zh_compExprNewArgList( $1, ZH_COMP_PARAM ); }
           | DoArgList ','             { $$ = zh_compExprAddListExpr( $1, zh_compExprNewNil( ZH_COMP_PARAM ) ); }
           | DoArgList ',' DoArgument  { $$ = zh_compExprAddListExpr( $1, $3 ); }
           ;

DoArgument : IdentName        { $$ = zh_compExprNewVarRef( $1, ZH_COMP_PARAM ); }
           | RefArgument
           | FunRef
           | SimpleExpression
           | PareExpList
           ;

WithObject : WITHOBJECT Expression Crlf
               {
                  zh_compLinePushIfInside( ZH_COMP_PARAM );
                  ZH_COMP_EXPR_FREE( zh_compExprGenPush( $2, ZH_COMP_PARAM ) );
                  $<sNumber>$ = ZH_COMP_PARAM->functions.pLast->nPCodePos;
                  zh_compGenPCode1( ZH_P_WITHOBJECTSTART, ZH_COMP_PARAM );
                  ZH_COMP_PARAM->functions.pLast->wWithObjectCnt++;
               }
             EmptyStats
             EndWithID
               {  if( ZH_COMP_PARAM->functions.pLast->wWithObjectCnt )
                    --ZH_COMP_PARAM->functions.pLast->wWithObjectCnt;
                  if( $<lNumber>5 )
                     zh_compGenPCode1( ZH_P_WITHOBJECTEND, ZH_COMP_PARAM );
                  else
                  {
                     zh_compNOOPfill( ZH_COMP_PARAM->functions.pLast,
                                      $<sNumber>4, 1, ZH_FALSE, ZH_TRUE );
                     zh_compGenPCode1( ZH_P_POP, ZH_COMP_PARAM );
                  }
               }
           ;

EndWithID  : ENDWITH
           | END
           | ENDERR { zh_compErrUnclosed( ZH_COMP_PARAM, "WITH OBJECT" ); }
           | ErrEndIf
           | ErrEndFor
           | ErrEndCase
           | ErrEndWhile
           | ErrEndSeq
           | ErrEndSwitch
           ;

Crlf       : '\n'       { ZH_COMP_PARAM->fError = ZH_FALSE; }
           | ';'
           ;

%%

/*
 ** ------------------------------------------------------------------------ **
 */

/*
 * Avoid tracing in preprocessor/compiler.
 */
#if ! defined( ZH_TRACE_UTILS )
   #if defined( ZH_TRACE_LEVEL )
      #undef ZH_TRACE_LEVEL
   #endif
#endif


/* ************************************************************************* */

/*
 * This function stores the position in pcode buffer where the FOR/WHILE
 * loop starts. It will be used to fix any LOOP/EXIT statements
 */
static void zh_compLoopStart( ZH_COMP_DECL, ZH_BOOL fCanLoop )
{
   PZH_LOOPEXIT pLoop = ( PZH_LOOPEXIT ) zh_xgrab( sizeof( ZH_LOOPEXIT ) );
   PZH_ZFUNC pFunc = ZH_COMP_PARAM->functions.pLast;
   
   if( pFunc->pLoops )
   {
      PZH_LOOPEXIT pLast = pFunc->pLoops;

      while( pLast->pNext )
         pLast = pLast->pNext;
      pLast->pNext = pLoop;
   }
   else
      pFunc->pLoops = pLoop;

   pLoop->nOffset          = pFunc->nPCodePos;  /* store the start position */
   pLoop->fCanLoop         = fCanLoop;    /* can we use LOOP inside */
   pLoop->wSeqCounter      = pFunc->wSeqCounter;      /* store current SEQUENCE counter */
   pLoop->wWithObjectCnt   = pFunc->wWithObjectCnt;   /* store current WITH OBJECT counter */
   pLoop->wAlwaysCounter   = pFunc->wAlwaysCounter;   /* store current ALWAYS counter */
   pLoop->pExitList        = NULL;
   pLoop->pLoopList        = NULL;
   pLoop->pNext            = NULL;
}

/*
 * return number of LOOP of EXIT statement in the top most structure
 */
static long zh_compLoopCount( ZH_COMP_DECL )
{
   PZH_LOOPEXIT pLastLoop, pLastExit, pLoop;
   PZH_ZFUNC pFunc = ZH_COMP_PARAM->functions.pLast;
   long lCount = 0;

   pLastLoop = pLastExit = NULL;
   pLoop = pFunc->pLoops;
   while( pLoop )
   {
      if( pLoop->fCanLoop )
         pLastLoop = pLoop;
      pLastExit = pLoop;
      pLoop = pLoop->pNext;
   }

   if( pLastLoop )
   {
      while( pLastLoop->pLoopList )
      {
         ++lCount;
         pLastLoop = pLastLoop->pLoopList;
      }
   }
   if( pLastExit )
   {
      while( pLastExit->pExitList )
      {
         ++lCount;
         pLastExit = pLastExit->pExitList;
      }
   }

   return lCount;
}

/*
 * Stores the position of LOOP statement to fix it later at the end of loop
 */
static void zh_compLoopLoop( ZH_COMP_DECL )
{
   PZH_LOOPEXIT pLast = NULL, pLoop;
   PZH_ZFUNC pFunc = ZH_COMP_PARAM->functions.pLast;

   pLoop = pFunc->pLoops;
   while( pLoop )
   {
      if( pLoop->fCanLoop )
         pLast = pLoop;
      pLoop = pLoop->pNext;
   }

   if( ! pLast )
   {
      zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_UNMATCHED_EXIT, "LOOP", NULL );
   }
   else
   {
      if( pLast->wSeqCounter != pFunc->wSeqCounter )
      {
         /* Attempt to LOOP from BEGIN/END sequence
          * Current SEQUENCE counter is different then at the beginning of loop
          * Notice that LOOP is allowed in RECOVER code when there is no
          * ALWAYS block
          */
         zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_EXIT_IN_SEQUENCE, "LOOP", NULL );
      }
      else if( pLast->wAlwaysCounter != pFunc->wAlwaysCounter )
      {
         /* Attempt to LOOP from ALWAYS block of BEGIN/END sequence
          */
         zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_EXIT_IN_SEQUENCE, "LOOP", NULL );
      }
      else
      {
         ZH_USHORT wWithObjectCnt = pLast->wWithObjectCnt;

         pLoop = ( PZH_LOOPEXIT ) zh_xgrab( sizeof( ZH_LOOPEXIT ) );
         pLoop->pLoopList = NULL;
         while( pLast->pLoopList )
            pLast = pLast->pLoopList;
         pLast->pLoopList = pLoop;

         while( wWithObjectCnt < pFunc->wWithObjectCnt )
         {
            zh_compGenPCode1( ZH_P_WITHOBJECTEND, ZH_COMP_PARAM );
            wWithObjectCnt++;
         }
         /* store the position to fix */
         pLoop->nOffset = pFunc->nPCodePos;
         zh_compGenJump( 0, ZH_COMP_PARAM );
      }
   }
}

/*
 * Stores the position of EXIT statement to fix it later at the end of loop
 */
static void zh_compLoopExit( ZH_COMP_DECL )
{
   PZH_ZFUNC pFunc = ZH_COMP_PARAM->functions.pLast;

   if( ! pFunc->pLoops )
   {
      zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_UNMATCHED_EXIT, "EXIT", NULL );
   }
   else
   {
      PZH_LOOPEXIT pLast, pLoop;

      pLast = pFunc->pLoops;
      while( pLast->pNext )
         pLast = pLast->pNext;

      if( pLast->wSeqCounter != pFunc->wSeqCounter )
      {
         /* Attempt to EXIT from BEGIN/END sequence
          * Current SEQUENCE counter is different then at the beginning of loop
          * Notice that EXIT is allowed in RECOVER code when there is no
          * ALWAYS block
          */
         zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_EXIT_IN_SEQUENCE, "EXIT", NULL );
      }
      else if( pLast->wAlwaysCounter != pFunc->wAlwaysCounter )
      {
         /* Attempt to EXIT from ALWAYS block of BEGIN/END sequence
          */
         zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_EXIT_IN_SEQUENCE, "EXIT", NULL );
      }
      else
      {
         ZH_USHORT wWithObjectCnt = pLast->wWithObjectCnt;

         pLoop = ( PZH_LOOPEXIT ) zh_xgrab( sizeof( ZH_LOOPEXIT ) );
         pLoop->pExitList = NULL;
         while( pLast->pExitList )
            pLast = pLast->pExitList;
         pLast->pExitList = pLoop;

         while( wWithObjectCnt < pFunc->wWithObjectCnt )
         {
            zh_compGenPCode1( ZH_P_WITHOBJECTEND, ZH_COMP_PARAM );
            wWithObjectCnt++;
         }
         /* store the position to fix */
         pLoop->nOffset = pFunc->nPCodePos;
         zh_compGenJump( 0, ZH_COMP_PARAM );
      }
   }
}

/*
 * Fixes the LOOP statement
 */
static void zh_compLoopHere( ZH_COMP_DECL )
{
   PZH_ZFUNC pFunc = ZH_COMP_PARAM->functions.pLast;
   PZH_LOOPEXIT pLoop = pFunc->pLoops, pFree, pLast;

   if( pLoop )
   {
      while( pLoop->pNext )
         pLoop = pLoop->pNext;

      pLast = pLoop;
      pLoop = pLoop->pLoopList;
      while( pLoop )
      {
         zh_compGenJumpHere( pLoop->nOffset + 1, ZH_COMP_PARAM );
         pFree = pLoop;
         pLoop = pLoop->pLoopList;
         zh_xfree( pFree );
      }
      pLast->pLoopList = NULL;
   }
}

/*
 * Fixes the EXIT statements and releases memory allocated for current loop
 */
static void zh_compLoopEnd( ZH_COMP_DECL )
{
   PZH_ZFUNC pFunc = ZH_COMP_PARAM->functions.pLast;
   PZH_LOOPEXIT pLoop = pFunc->pLoops, pLast = pFunc->pLoops, pExit, pFree;

   if( pLoop )
   {
      while( pLoop->pNext )
      {
         pLast = pLoop;
         pLoop = pLoop->pNext;
      }

      pExit = pLoop->pExitList;
      while( pExit )
      {
         zh_compGenJumpHere( pExit->nOffset + 1, ZH_COMP_PARAM );
         pFree = pExit;
         pExit = pExit->pExitList;
         zh_xfree( pFree );
      }

      pLast->pNext = NULL;
      if( pLoop == pFunc->pLoops )
         pFunc->pLoops = NULL;
      zh_xfree( pLoop );
   }
}

void zh_compLoopKill( PZH_ZFUNC pFunc )
{
   PZH_LOOPEXIT pLoop, pFree;

   while( pFunc->pLoops )
   {
      pLoop = pFunc->pLoops;
      while( pLoop->pExitList )
      {
         pFree = pLoop->pExitList;
         pLoop->pExitList = pFree->pExitList;
         zh_xfree( pFree );
      }
      while( pLoop->pLoopList )
      {
         pFree = pLoop->pLoopList;
         pLoop->pLoopList = pFree->pLoopList;
         zh_xfree( pFree );
      }
      pFunc->pLoops = pLoop->pNext;
      zh_xfree( pLoop );
   }
}

static void * zh_compElseIfGen( ZH_COMP_DECL, void * pFirst, ZH_SIZE nOffset )
{
   PZH_ELSEIF pElseIf = ( PZH_ELSEIF ) zh_xgrab( sizeof( ZH_ELSEIF ) ), pLast;
   PZH_ZFUNC pFunc = ZH_COMP_PARAM->functions.pLast;

   pElseIf->nOffset = nOffset;
   pElseIf->pPrev   = NULL;
   pElseIf->pElseif = NULL;

   if( pFirst )
   {
      pLast = ( PZH_ELSEIF ) pFirst;
      while( pLast->pElseif )
         pLast = pLast->pElseif;
      pLast->pElseif = pElseIf;
   }
   else
   {
      if( pFunc->elseif )
      {
         pElseIf->pPrev = pFunc->elseif;
      }
      pFirst = pElseIf;
      pFunc->elseif = pElseIf;
   }
   return pFirst;
}


static void zh_compElseIfFix( ZH_COMP_DECL, void * pFixElseIfs )
{
   PZH_ELSEIF pFix = ( PZH_ELSEIF ) pFixElseIfs;
   PZH_ELSEIF pDel;

   ZH_COMP_PARAM->functions.pLast->elseif = pFix->pPrev;
   while( pFix )
   {
      zh_compGenJumpHere( pFix->nOffset, ZH_COMP_PARAM );
      pDel = pFix;
      pFix = pFix->pElseif;
      zh_xfree( pDel );
   }
}

void zh_compElseIfKill( PZH_ZFUNC pFunc )
{
   PZH_ELSEIF pFix;
   PZH_ELSEIF pDel;

   while( pFunc->elseif )
   {
      pFix = pFunc->elseif;
      pFunc->elseif = pFix->pPrev;
      while( pFix )
      {
         pDel = pFix;
         pFix = pFix->pElseif;
         zh_xfree( pDel );
      }
   }
}

static void zh_compRTVariableAdd( ZH_COMP_DECL, PZH_EXPR pVar, ZH_BOOL bPopInitValue )
{
   PZH_RTVAR pRTvar = ( PZH_RTVAR ) zh_xgrab( sizeof( ZH_RTVAR ) );
   PZH_ZFUNC pFunc = ZH_COMP_PARAM->functions.pLast;

   pRTvar->pVar = pVar;
   pRTvar->bPopValue = bPopInitValue;
   pRTvar->pNext = NULL;
   pRTvar->pPrev = NULL;

   if( pFunc->rtvars )
   {
      PZH_RTVAR pLast = pFunc->rtvars;
      while( pLast->pNext )
         pLast = pLast->pNext;
      pLast->pNext = pRTvar;
      pRTvar->pPrev = pLast;
   }
   else
      pFunc->rtvars = pRTvar;
}

static void zh_compRTVariableGen( ZH_COMP_DECL, const char * szCreateFun )
{
   ZH_USHORT usCount = 0;
   PZH_ZFUNC pFunc = ZH_COMP_PARAM->functions.pLast;
   PZH_RTVAR pVar = pFunc->rtvars;
   PZH_RTVAR pDel;

   /* generate the function call frame */
   zh_compGenPushFunCall( szCreateFun, ZH_FN_UDF, ZH_COMP_PARAM );

   /* push variable names to create */
   while( pVar->pNext )
   {
      pVar->pVar = zh_compExprGenPush( pVar->pVar, ZH_COMP_PARAM );
      pVar = pVar->pNext;
      ++usCount;
   }
   pVar->pVar = zh_compExprGenPush( pVar->pVar, ZH_COMP_PARAM );
   ++usCount;

   /* call function that will create either PUBLIC or PRIVATE variables */
   if( usCount > 255 )
      zh_compGenPCode3( ZH_P_DO, ZH_LOBYTE( usCount ), ZH_HIBYTE( usCount ), ZH_COMP_PARAM );
   else
      zh_compGenPCode2( ZH_P_DOSHORT, ( ZH_BYTE ) usCount, ZH_COMP_PARAM );

   /* pop initial values */
   while( pVar )
   {
      if( pVar->bPopValue )
         ZH_COMP_EXPR_FREE( zh_compExprGenPop( pVar->pVar, ZH_COMP_PARAM ) );
      else
         ZH_COMP_EXPR_FREE( pVar->pVar );
      pDel = pVar;
      pVar = pVar->pPrev;
      zh_xfree( pDel );
   }
   pFunc->rtvars = NULL;
}

void zh_compRTVariableKill( ZH_COMP_DECL, PZH_ZFUNC pFunc )
{
   PZH_RTVAR pVar;

   while( pFunc->rtvars )
   {
      pVar = pFunc->rtvars;

      ZH_COMP_EXPR_FREE( pVar->pVar );
      pFunc->rtvars = pVar->pPrev;
      zh_xfree( pVar );
   }
   pFunc->rtvars = NULL;
}

static PZH_EXPR zh_compArrayDimPush( PZH_EXPR pInitValue, ZH_COMP_DECL )
{
   ZH_USHORT uCount = ( ZH_USHORT ) zh_compExprListLen( pInitValue );

   if( uCount == 1 && zh_compExprIsInteger( pInitValue->value.asList.pExprList ) &&
       zh_compExprAsInteger( pInitValue->value.asList.pExprList ) == 0 )
   {
      zh_compGenPCode3( ZH_P_ARRAYGEN, 0, 0, ZH_COMP_PARAM );
   }
   else
   {
      pInitValue = zh_compExprGenPush( pInitValue, ZH_COMP_PARAM );
      zh_compGenPCode3( ZH_P_ARRAYDIM, ZH_LOBYTE( uCount ), ZH_HIBYTE( uCount ), ZH_COMP_PARAM );
   }
   return pInitValue;
}

static void zh_compVariableDim( const char * szName, PZH_EXPR pInitValue, ZH_COMP_DECL )
{
   if( ZH_COMP_PARAM->iVarScope == ZH_VSCOMP_PUBLIC || ZH_COMP_PARAM->iVarScope == ZH_VSCOMP_PRIVATE )
   {
      zh_compVariableAdd( ZH_COMP_PARAM, szName, zh_compVarTypeNew( ZH_COMP_PARAM, 'A', NULL ) );
      ZH_COMP_EXPR_FREE( zh_compArrayDimPush( pInitValue, ZH_COMP_PARAM ) );
      zh_compRTVariableAdd( ZH_COMP_PARAM, zh_compExprNewRTVar( szName, NULL, ZH_COMP_PARAM ), ZH_TRUE );
   }
   else if( ZH_COMP_PARAM->iVarScope & ZH_VSCOMP_STATIC )
   {
      PZH_EXPR pVar = zh_compExprNewVar( szName, ZH_COMP_PARAM );
      PZH_EXPR pAssign;

      /* create a static variable */
      zh_compVariableAdd( ZH_COMP_PARAM, szName, zh_compVarTypeNew( ZH_COMP_PARAM, 'A', NULL ) );

      zh_compStaticDefStart( ZH_COMP_PARAM );   /* switch to statics pcode buffer */
      /* create an array */
      pInitValue = zh_compArrayDimPush( pInitValue, ZH_COMP_PARAM );
      /* now pop an array */
      pVar = zh_compExprGenPop( pVar, ZH_COMP_PARAM );
      /* check if valid initializers were used but don't generate any code */
      pAssign = zh_compExprAssignStatic( pVar, pInitValue, ZH_COMP_PARAM );
      /* delete all used expressions */
      ZH_COMP_EXPR_FREE( pAssign );
      zh_compStaticDefEnd( ZH_COMP_PARAM, szName );
   }
   else
   {
      zh_compVariableAdd( ZH_COMP_PARAM, szName, zh_compVarTypeNew( ZH_COMP_PARAM, 'A', NULL ) );
      ZH_COMP_EXPR_FREE( zh_compArrayDimPush( pInitValue, ZH_COMP_PARAM ) );
      if( ZH_COMP_PARAM->iVarScope != ZH_VSCOMP_LOCAL ||
          !( ZH_COMP_PARAM->functions.pLast->funFlags & ZH_FUNF_EXTBLOCK ) )
      {
         ZH_COMP_EXPR_FREE( zh_compExprGenPop( zh_compExprNewVar( szName, ZH_COMP_PARAM ), ZH_COMP_PARAM ) );
      }
   }
}

static void zh_compForStart( ZH_COMP_DECL, const char *szVarName, int iForEachDir )
{
   PZH_ENUMERATOR pEnumVar;

   pEnumVar = ZH_COMP_PARAM->functions.pLast->pEnum;
   if( pEnumVar == NULL )
   {
      ZH_COMP_PARAM->functions.pLast->pEnum = ( PZH_ENUMERATOR ) zh_xgrab( sizeof( ZH_ENUMERATOR ) );
      pEnumVar = ZH_COMP_PARAM->functions.pLast->pEnum;
   }
   else
   {
      ZH_BOOL bWarn = ZH_TRUE;
      PZH_ENUMERATOR pLast = pEnumVar;

      while( pEnumVar )
      {
         if( strcmp( pEnumVar->szName, szVarName ) == 0 )
         {
            /* Enumerator variable exists already - throw warning */
            if( bWarn )
            {
               zh_compGenWarning( ZH_COMP_PARAM, zh_comp_szWarnings, 'W', ZH_COMP_WARN_FORVAR_DUPL, szVarName, NULL );
               bWarn = ZH_FALSE;
            }
         }
         pLast = pEnumVar;
         pEnumVar = pEnumVar->pNext;
      }
      pLast->pNext = ( PZH_ENUMERATOR ) zh_xgrab( sizeof( ZH_ENUMERATOR ) );
      pEnumVar = pLast->pNext;
   }
   pEnumVar->szName      = szVarName;
   pEnumVar->iForEachDir = iForEachDir;
   pEnumVar->pNext       = NULL;
}

static ZH_BOOL zh_compForEachVarError( ZH_COMP_DECL, const char *szVarName, int * piDir )
{
   PZH_ENUMERATOR pEnumVar;

   pEnumVar = ZH_COMP_PARAM->functions.pLast->pEnum;
   if( pEnumVar && ! ZH_COMP_PARAM->functions.pLast->bBlock )
   {
      while( pEnumVar )
      {
         if( strcmp( pEnumVar->szName, szVarName ) == 0 )
         {
            * piDir = pEnumVar->iForEachDir;
            if( * piDir != 0 )
            {
               /* only if it is FOR EACH enumerator
                * generate warning if it is FOR/NEXT loop
               */
               return ZH_FALSE;
            }
         }
         pEnumVar = pEnumVar->pNext;
      }
   }

   zh_compGenWarning( ZH_COMP_PARAM, zh_comp_szWarnings, 'W', ZH_COMP_WARN_ENUM_INVALID, szVarName, NULL );
   return ZH_TRUE;
}

static void zh_compForEnd( ZH_COMP_DECL, const char *szVar )
{
   PZH_ENUMERATOR * pEnumVar;

   ZH_SYMBOL_UNUSED( szVar );

   pEnumVar = &ZH_COMP_PARAM->functions.pLast->pEnum;
   if( *pEnumVar )
   {
      while( ( *pEnumVar )->pNext )
         pEnumVar = &( *pEnumVar )->pNext;

      zh_xfree( *pEnumVar );
      *pEnumVar = NULL;
   }
}

static ZH_COMP_CARGO2_FUNC( zh_compEnumEvalStart )
{
   const char * szName = zh_compExprAsSymbol( ( PZH_EXPR ) cargo );

   if( szName )
      zh_compForStart( ZH_COMP_PARAM, szName, ZH_COMP_PARAM->fDescend ? -1 : 1 );

   zh_compExprGenPush( ( PZH_EXPR ) dummy, ZH_COMP_PARAM );  /* expression */
   zh_compExprGenPush( ( PZH_EXPR ) cargo, ZH_COMP_PARAM );  /* variable */
}

static void zh_compEnumStart( ZH_COMP_DECL, PZH_EXPR pVars, PZH_EXPR pExprs, int descend )
{
   ZH_SIZE nLen;

   if( zh_compExprListLen( pVars ) != zh_compExprListLen( pExprs ) )
   {
      zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_FORVAR_DIFF, NULL, NULL );
   }

   ZH_COMP_PARAM->fDescend = descend < 0;
   nLen = zh_compExprListEval2( ZH_COMP_PARAM, pVars, pExprs, zh_compEnumEvalStart );

   if( nLen > 255 )
   {
      zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_FORVAR_TOOMANY, NULL, NULL );
   }
   else
   {
      zh_compGenPCode3( ZH_P_ENUMSTART, ( ZH_BYTE ) ( nLen & 0xFF ), ( ZH_BYTE ) ( descend > 0 ? 1 : 0 ), ZH_COMP_PARAM );
   }
}

static void zh_compEnumNext( ZH_COMP_DECL, PZH_EXPR pExpr, int descend )
{
   ZH_SYMBOL_UNUSED( pExpr );
   if( descend > 0 )
   {
      zh_compGenPCode1( ZH_P_ENUMNEXT, ZH_COMP_PARAM );
   }
   else
   {
      zh_compGenPCode1( ZH_P_ENUMPREV, ZH_COMP_PARAM );
   }
}

static ZH_COMP_CARGO_FUNC( zh_compEnumEvalEnd )
{
   const char * szName = zh_compExprAsSymbol( ( PZH_EXPR ) cargo );

   if( szName )
      zh_compForEnd( ZH_COMP_PARAM, szName );
}

static void zh_compEnumEnd( ZH_COMP_DECL, PZH_EXPR pExpr )
{
   zh_compExprListEval( ZH_COMP_PARAM, pExpr, zh_compEnumEvalEnd );
   zh_compGenPCode1( ZH_P_ENUMEND, ZH_COMP_PARAM );
}

static void zh_compSwitchStart( ZH_COMP_DECL, PZH_EXPR pExpr )
{
   PZH_SWITCHCMD pSwitch = ( PZH_SWITCHCMD ) zh_xgrab( sizeof( ZH_SWITCHCMD ) );
   PZH_ZFUNC pFunc = ZH_COMP_PARAM->functions.pLast;

   pSwitch->pCases = NULL;
   pSwitch->pLast  = NULL;
   pSwitch->nDefault = 0;
   pSwitch->nOffset = pFunc->nPCodePos;
   pSwitch->pExpr = pExpr;
   pSwitch->pPrev = pFunc->pSwitch;
   pFunc->pSwitch = pSwitch;
}

static void zh_compSwitchAdd( ZH_COMP_DECL, PZH_EXPR pExpr )
{
   PZH_SWITCHCASE pCase;
   PZH_ZFUNC pFunc = ZH_COMP_PARAM->functions.pLast;

   pFunc->funFlags &= ~ZH_FUNF_BREAK_CODE;

   if( pExpr )
   {
      /* normal CASE */
      pCase = ( PZH_SWITCHCASE ) zh_xgrab( sizeof( ZH_SWITCHCASE ) );
      pCase->nOffset = pFunc->nPCodePos;
      pCase->pNext = NULL;
      pCase->pExpr = pExpr = zh_compExprReduce( pExpr, ZH_COMP_PARAM );
      if( !( zh_compExprIsLong( pExpr ) || zh_compExprIsString( pExpr ) ) )
         zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_NOT_LITERAL_CASE, NULL, NULL );
      else if( pFunc->pSwitch->pCases )
      {
         PZH_SWITCHCASE pCases = pFunc->pSwitch->pCases;
         while( pCases )
         {
            ZH_BOOL fEqual = ZH_FALSE;

            if( zh_compExprIsLong( pExpr ) )
            {
               if( zh_compExprIsLong( pCases->pExpr ) )
                  fEqual = zh_compExprAsLongNum( pExpr ) == zh_compExprAsLongNum( pCases->pExpr );
            }
            else
            {
               if( zh_compExprIsString( pCases->pExpr ) )
                  fEqual = zh_compExprAsStringLen( pExpr ) == zh_compExprAsStringLen( pCases->pExpr ) &&
                           memcmp( zh_compExprAsString( pExpr ),
                                   zh_compExprAsString( pCases->pExpr ),
                                   zh_compExprAsStringLen( pExpr ) ) == 0;
            }
            if( fEqual )
               zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_DUPL_CASE, NULL, NULL );
            pCases = pCases->pNext;
         }
      }

      if( pFunc->pSwitch->pLast )
      {
         pFunc->pSwitch->pLast->pNext = pCase;
         pFunc->pSwitch->pLast = pCase;
      }
      else
      {
         pFunc->pSwitch->pCases = pFunc->pSwitch->pLast = pCase;
      }
      if( zh_compExprIsString( pExpr ) && zh_compExprAsStringLen( pExpr ) > 255 )
      {
         zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_INVALID_STR, NULL, NULL );
      }
   }
   else
   {
      /* DEFAULT */
      if( pFunc->pSwitch->nDefault )
      {
         /* more than one default clause */
         zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_MAYHEM_IN_CASE, NULL, NULL );
      }
      else
      {
         pFunc->pSwitch->nDefault = pFunc->nPCodePos;
      }
   }
}

static void zh_compSwitchEnd( ZH_COMP_DECL )
{
   PZH_ZFUNC pFunc = ZH_COMP_PARAM->functions.pLast;
   PZH_SWITCHCMD pSwitch = pFunc->pSwitch;
   PZH_EXPR pExpr = pSwitch->pExpr;
   PZH_SWITCHCASE pCase, pTmp;
   ZH_SIZE nExitPos, nCountPos;
   int iCount = 0;

   /* skip switch pcode if there was no EXIT in the last CASE
    * or in the DEFAULT case
   */
   nExitPos = zh_compGenJump( 0, ZH_COMP_PARAM );
   zh_compGenJumpHere( pSwitch->nOffset + 1, ZH_COMP_PARAM );

   pCase = pSwitch->pCases;
   if( zh_compExprIsLong( pExpr ) || zh_compExprIsString( pExpr ) )
   {
      ZH_BOOL fGen = ZH_FALSE;
      while( pCase )
      {
         if( zh_compExprIsLong( pCase->pExpr ) )
         {
            fGen = zh_compExprIsLong( pExpr ) &&
                   zh_compExprAsLongNum( pExpr ) ==
                   zh_compExprAsLongNum( pCase->pExpr );
         }
         else if( zh_compExprIsString( pCase->pExpr ) )
         {
            fGen = zh_compExprIsString( pExpr ) &&
                   zh_compExprAsStringLen( pExpr ) ==
                   zh_compExprAsStringLen( pCase->pExpr ) &&
                   memcmp( zh_compExprAsString( pExpr ),
                           zh_compExprAsString( pCase->pExpr ),
                           zh_compExprAsStringLen( pExpr ) ) == 0;
         }
         if( fGen )
         {
            zh_compGenJumpThere( zh_compGenJump( 0, ZH_COMP_PARAM ),
                                 pCase->nOffset, ZH_COMP_PARAM );
            break;
         }
         pCase = pCase->pNext;
      }
      if( pSwitch->nDefault && ! fGen )
      {
         zh_compGenJumpThere( zh_compGenJump( 0, ZH_COMP_PARAM ),
                              pSwitch->nDefault, ZH_COMP_PARAM );
      }
   }
   else
   {
      ZH_BOOL fSwitchCase = ZH_COMP_PARAM->fSwitchCase;
      ZH_BOOL fMacroText = ( ZH_COMP_PARAM->supported & ZH_COMPFLAG_MACROTEXT ) != 0;

      pExpr = zh_compExprGenPush( pExpr, ZH_COMP_PARAM );
      nCountPos = pFunc->nPCodePos + 1;
      zh_compGenPCode3( ZH_P_SWITCH, 0, 0, ZH_COMP_PARAM );
      ZH_COMP_PARAM->fSwitchCase = ZH_TRUE;
      ZH_COMP_PARAM->supported &= ~ZH_COMPFLAG_MACROTEXT;
      while( pCase )
      {
         if( zh_compExprIsLong( pCase->pExpr ) || zh_compExprIsString( pCase->pExpr ) )
         {
            iCount++;
            pCase->pExpr = zh_compExprGenPush( pCase->pExpr, ZH_COMP_PARAM );
            zh_compGenJumpThere( zh_compGenJump( 0, ZH_COMP_PARAM ),
                                 pCase->nOffset, ZH_COMP_PARAM );
         }
         pCase = pCase->pNext;
      }
      if( pSwitch->nDefault )
      {
         iCount++;
         zh_compGenPCode1( ZH_P_PUSHNIL, ZH_COMP_PARAM );
         zh_compGenJumpThere( zh_compGenJump( 0, ZH_COMP_PARAM ),
                              pSwitch->nDefault, ZH_COMP_PARAM );
      }
      ZH_PUT_LE_UINT16( pFunc->pCode + nCountPos, iCount );

      ZH_COMP_PARAM->fSwitchCase = fSwitchCase;
      if( fMacroText )
         ZH_COMP_PARAM->supported |= ZH_COMPFLAG_MACROTEXT;
   }

   zh_compGenJumpHere( nExitPos, ZH_COMP_PARAM );

   if( pExpr )
      ZH_COMP_EXPR_FREE( pExpr );

   pCase = pSwitch->pCases;
   while( pCase )
   {
      ZH_COMP_EXPR_FREE( pCase->pExpr );
      pTmp = pCase->pNext;
      zh_xfree( pCase );
      pCase = pTmp;
   }
   pFunc->pSwitch = pSwitch->pPrev;
   zh_xfree( pSwitch );
}

/* Release all switch statements
*/
void zh_compSwitchKill( ZH_COMP_DECL, PZH_ZFUNC pFunc )
{
   PZH_SWITCHCASE pCase;
   PZH_SWITCHCMD pSwitch;

   while( pFunc->pSwitch )
   {
      while( pFunc->pSwitch->pCases )
      {
         pCase = pFunc->pSwitch->pCases;
         ZH_COMP_EXPR_FREE( pCase->pExpr );
         pFunc->pSwitch->pCases = pCase->pNext;
         zh_xfree( pCase );
      }
      pSwitch = pFunc->pSwitch;
      pFunc->pSwitch = pSwitch->pPrev;
      if( pSwitch->pExpr )
         ZH_COMP_EXPR_FREE( pSwitch->pExpr );
      zh_xfree( pSwitch );
   }
}

static PZH_EXPR zh_compCheckPassByRef( ZH_COMP_DECL, PZH_EXPR pExpr )
{
   if( pExpr->ExprType == ZH_ET_FUNCALL )
   {
      if( zh_compExprParamListLen( pExpr->value.asFunCall.pParms ) == 0 )
      {
         PZH_EXPR pDelExpr = pExpr;
         if( pExpr->value.asFunCall.pFunName->ExprType == ZH_ET_MACRO )
         {
            pExpr = pExpr->value.asFunCall.pFunName;
            ZH_COMP_EXPR_CLEAR( pDelExpr );
         }
         else
         {
            pExpr = zh_compExprNewFunRef( zh_compExprAsSymbol( pExpr ), ZH_COMP_PARAM );
            ZH_COMP_EXPR_FREE( pDelExpr );
         }
         return pExpr;
      }
      else
      {
         const char * szDesc;

         szDesc = zh_compExprAsSymbol( pExpr );
         if( ! szDesc )
            szDesc = zh_compExprDescription( pExpr );

         return zh_compErrorRefer( ZH_COMP_PARAM, pExpr, szDesc );
      }
   }

   return pExpr;
}

static PZH_EXPR zh_compCheckMethod( ZH_COMP_DECL, PZH_EXPR pExpr )
{
   if( pExpr->value.asMessage.szMessage &&
       pExpr->value.asMessage.pObject &&
       pExpr->value.asMessage.pObject->ExprType == ZH_ET_VARIABLE &&
       pExpr->value.asMessage.szMessage[ 0 ] == '_' &&
       strncmp( "__ENUM", pExpr->value.asMessage.szMessage, 6 ) == 0 )
   {
      const char * szMessage = pExpr->value.asMessage.szMessage + 6;

      if( strcmp( "INDEX",   szMessage ) == 0 ||
          strcmp( "KEY",     szMessage ) == 0 ||
          strcmp( "BASE",    szMessage ) == 0 ||
          strcmp( "VALUE",   szMessage ) == 0 ||
          strcmp( "ISFIRST", szMessage ) == 0 ||
          strcmp( "ISLAST",  szMessage ) == 0 )
      {
         int iDir = 0;
         if( ! zh_compForEachVarError( ZH_COMP_PARAM, pExpr->value.asMessage.pObject->value.asSymbol.name, &iDir ) )
         {
            pExpr->value.asMessage.pObject->ExprType = ZH_ET_VARREF;
         }
      }
   }

   return pExpr;
}

static void zh_compErrStru( ZH_COMP_DECL, int iError )
{
   zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', iError, NULL, NULL );
}

static void zh_compErrUnclosed( ZH_COMP_DECL, const char * szStru )
{
   ZH_COMP_PARAM->fError = ZH_FALSE;
   zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_UNCLOSED_STRU, szStru, NULL );
}

/* ************************************************************************* */

ZH_BOOL zh_compCheckUnclosedStru( ZH_COMP_DECL, PZH_ZFUNC pFunc )
{
   ZH_BOOL fUnclosed = ZH_TRUE;

   if( pFunc->wIfCounter )
   {
      zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_UNCLOSED_STRU, "IF", NULL );
      pFunc->wIfCounter = 0;
   }
   else if( pFunc->wForCounter )
   {
      zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_UNCLOSED_STRU, "FOR", NULL );
      pFunc->wForCounter = 0;
   }
   else if( pFunc->wWhileCounter )
   {
      zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_UNCLOSED_STRU, "WHILE", NULL );
      pFunc->wWhileCounter = 0;
   }
   else if( pFunc->wCaseCounter )
   {
      zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_UNCLOSED_STRU, "CASE", NULL );
      pFunc->wCaseCounter = 0;
   }
   else if( pFunc->wSwitchCounter )
   {
      zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_UNCLOSED_STRU, "SWITCH", NULL );
      pFunc->wSwitchCounter = 0;
   }
   else if( pFunc->wWithObjectCnt )
   {
      zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_UNCLOSED_STRU, "WITH OBJECT", NULL );
      pFunc->wWithObjectCnt = 0;
   }
   else if( pFunc->wSeqCounter )
   {
      zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_UNCLOSED_STRU, "BEGIN SEQUENCE", NULL );
      pFunc->wSeqCounter = 0;
   }
   else if( pFunc->funFlags & ZH_FUNF_EXTBLOCK )
   {
      zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_UNCLOSED_STRU, "{||...}", NULL );
      pFunc->funFlags &= ~ZH_FUNF_EXTBLOCK;
   }
   else
      fUnclosed = ZH_FALSE;

   return fUnclosed;
}

void yyerror( ZH_COMP_DECL, const char * s )
{
   if( ! ZH_COMP_PARAM->pLex->lasttok || ZH_COMP_PARAM->pLex->lasttok[ 0 ] == '\n' )
   {
      if( ZH_COMP_PARAM->iErrorCount == 0 || ! zh_pp_eof( ZH_COMP_PARAM->pLex->pPP ) )
         zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_INCOMPLETE_STMT, NULL, NULL );
   }
   else if( ZH_COMP_PARAM->pLex->iState == NEXT )
      zh_compErrStru( ZH_COMP_PARAM, ZH_COMP_ERR_NEXTFOR );
   else if( ZH_COMP_PARAM->pLex->iState == ELSE )
      zh_compErrStru( ZH_COMP_PARAM, ZH_COMP_ERR_UNMATCHED_ELSE );
   else if( ZH_COMP_PARAM->pLex->iState == ELSEIF )
      zh_compErrStru( ZH_COMP_PARAM, ZH_COMP_ERR_UNMATCHED_ELSEIF );
   else if( ZH_COMP_PARAM->pLex->iState == ENDIF || ZH_COMP_PARAM->pLex->iState == END )
      zh_compErrStru( ZH_COMP_PARAM, ZH_COMP_ERR_ENDIF );
   else if( ZH_COMP_PARAM->pLex->iState == CASE || ZH_COMP_PARAM->pLex->iState == OTHERWISE )
      zh_compErrStru( ZH_COMP_PARAM, ZH_COMP_ERR_CASE );
   else if( ZH_COMP_PARAM->pLex->iState == ENDCASE )
      zh_compErrStru( ZH_COMP_PARAM, ZH_COMP_ERR_ENDCASE );
   else if( ZH_COMP_PARAM->pLex->iState == ENDDO )
      zh_compErrStru( ZH_COMP_PARAM, ZH_COMP_ERR_ENDDO );
   else if( ZH_COMP_PARAM->pLex->iState == ENDWITH )
      zh_compErrStru( ZH_COMP_PARAM, ZH_COMP_ERR_ENDWITH );
   else if( ZH_COMP_PARAM->pLex->iState == ENDSEQ )
      zh_compErrStru( ZH_COMP_PARAM, ZH_COMP_ERR_ENDSEQ );
   else if( ZH_COMP_PARAM->pLex->iState == ENDSWITCH )
      zh_compErrStru( ZH_COMP_PARAM, ZH_COMP_ERR_ENDSWITCH );
   else
      zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_YACC, s, ZH_COMP_PARAM->pLex->lasttok );
}
