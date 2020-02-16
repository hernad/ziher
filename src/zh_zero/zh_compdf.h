/*
 * Definitions shared by compiler and macro compiler
 *
 * Copyright 2006 Przemyslaw Czerpak
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

#ifndef ZH_COMPDF_H_
#define ZH_COMPDF_H_

#include "zh_pp.h"
#include "zh_hash.h"

ZH_EXTERN_BEGIN

/* compiler related declarations */

/* Output types */
typedef enum
{
   ZH_LANG_C,                      /* C language (by default) <file.c> */
   ZH_LANG_PORT_OBJ,               /* Portable objects <file.zhb> */
   ZH_LANG_PORT_OBJ_BUF            /* Portable objects in memory buffer */
} ZH_LANGUAGES;                    /* supported Ziher output languages */

/* Error message format modes */
typedef enum
{
   ZH_ERRORFMT_CLIPPER,
   ZH_ERRORFMT_IDE
} ZH_ERRORFMT;

struct _ZH_HCLASS;    /* forward declaration */

/* Declared Function/Method support structure */
typedef struct _ZH_HDECLARED
{
   const char *          szName;              /* the name of the symbol */
   ZH_BYTE               cType;
   ZH_USHORT             iParamCount;
   ZH_BYTE *             cParamTypes;
   struct _ZH_HCLASS * pClass;
   struct _ZH_HCLASS * ( * pParamClasses );
   struct _ZH_HDECLARED * pNext;               /* pointer to the next declared function */
} ZH_HDECLARED, * PZH_HDECLARED;

/* Declared Class support structure */
typedef struct _ZH_HCLASS
{
   const char *       szName;
   PZH_HDECLARED      pMethod;
   PZH_HDECLARED      pLastMethod;
   struct _ZH_HCLASS * pNext;
} ZH_HCLASS, * PZH_HCLASS;

/* locals, static, public variables support */
typedef struct _ZH_HVAR
{
   const char *   szName;           /* variable name */
   const char *   szAlias;          /* variable alias namespace */
   int            iUsed;            /* number of times used */
   int            iDeclLine;        /* declaration line number */
   ZH_USHORT      uiFlags;          /* optional flags, e.g. THREAD STATIC */
   ZH_BYTE        cType;            /* optional strong typing */
   PZH_HCLASS     pClass;
   struct _ZH_HVAR * pNext;            /* pointer to next defined variable */
} ZH_HVAR, * PZH_HVAR;

/* local variables declared in a codeblock */
typedef struct ZH_CBVAR_
{
   const char * szName;
   ZH_BYTE bType;
   ZH_BOOL bUsed;
   struct ZH_CBVAR_ * pNext;
} ZH_CBVAR, * PZH_CBVAR;

typedef struct _ZH_VARTYPE
{
   struct _ZH_VARTYPE *   pNext;
   ZH_BYTE                cVarType;
   const char *           szFromClass;
} ZH_VARTYPE, * PZH_VARTYPE;

/* value types seen at language level
 */
#define ZH_EV_UNKNOWN     0x0000
#define ZH_EV_NIL         0x0001
#define ZH_EV_NUMERIC     0x0002
#define ZH_EV_STRING      0x0004
#define ZH_EV_CODEBLOCK   0x0008
#define ZH_EV_LOGICAL     0x0010
#define ZH_EV_OBJECT      0x0020
#define ZH_EV_ARRAY       0x0040
#define ZH_EV_SYMBOL      0x0080
#define ZH_EV_VARREF      0x0100
#define ZH_EV_FUNREF      0x0200
#define ZH_EV_DATE        0x0400
#define ZH_EV_TIMESTAMP   0x0800
#define ZH_EV_HASH        0x1000

/* messages sent to expressions
 */
typedef enum
{
   ZH_EA_REDUCE = 0,    /* reduce the expression into optimized one */
   ZH_EA_ARRAY_AT,      /* check if the expression can be used as array */
   ZH_EA_ARRAY_INDEX,   /* check if the expression can be used as index */
   ZH_EA_LVALUE,        /* check if the expression can be used as lvalue (left side of an assignment) */
   ZH_EA_PUSH_PCODE,    /* generate the pcodes to push the value of expression */
   ZH_EA_POP_PCODE,     /* generate the pcodes to pop the value of expression */
   ZH_EA_PUSH_POP,      /* generate the pcodes to push and pop the expression */
   ZH_EA_STATEMENT,     /* generate the pcodes for a statement */
   ZH_EA_DELETE         /* delete components of the expression */
} ZH_EXPR_MESSAGE;

/* additional definitions used to distinguish numeric expressions
 */
#define ZH_ET_LONG     1
#define ZH_ET_DOUBLE   2

/* additional definitions used to distinguish macro expressions
 */
#define ZH_ET_MACRO_VAR       0x0001   /* &variable */
#define ZH_ET_MACRO_SYMBOL    0x0002   /* &fimcall() */
#define ZH_ET_MACRO_ALIASED   0x0004   /* &alias->&variable */
#define ZH_ET_MACRO_EXPR      0x0008   /* &( expr ) */
#define ZH_ET_MACRO_LIST      0x0010   /* &variable used as in literal arrays or function call argument. */
#define ZH_ET_MACRO_PARE      0x0020   /* &variable used as parenthesized expressions. */
#define ZH_ET_MACRO_REFER     0x0040   /* &macro used in @ (pass by reference) */
#define ZH_ET_MACRO_ASSIGN    0x0080   /* o:&msgname := value */
#define ZH_ET_MACRO_NOLIST    ( ZH_ET_MACRO_SYMBOL | ZH_ET_MACRO_ALIASED | \
                                ZH_ET_MACRO_ASSIGN | ZH_ET_MACRO_PARE | \
                                ZH_ET_MACRO_REFER )
#define ZH_ET_MACRO_NOPARE    ( ZH_ET_MACRO_SYMBOL | ZH_ET_MACRO_ALIASED | \
                                ZH_ET_MACRO_ASSIGN | ZH_ET_MACRO_REFER )

/* types of expressions
 * NOTE: the order of these definition is important - change it carefully
 *    All types <= ZH_ET_FUNREF are constant values
 *    All types <= ZH_ET_VARIABLE are a simple values
 *    All types > ZH_ET_VARIABLE are operators
 */
typedef enum
{
   ZH_ET_NONE = 0,
   ZH_ET_NIL,
   ZH_ET_NUMERIC,
   ZH_ET_DATE,
   ZH_ET_TIMESTAMP,
   ZH_ET_STRING,
   ZH_ET_CODEBLOCK,
   ZH_ET_LOGICAL,
   ZH_ET_SELF,
   ZH_ET_ARRAY,
   ZH_ET_HASH,
   ZH_ET_FUNREF,
   ZH_ET_VARREF,
   ZH_ET_REFERENCE,
   ZH_ET_IIF,
   ZH_ET_LIST,
   ZH_ET_ARGLIST,
   ZH_ET_MACROARGLIST,
   ZH_ET_ARRAYAT,
   ZH_ET_MACRO,
   ZH_ET_FUNCALL,
   ZH_ET_ALIASVAR,
   ZH_ET_ALIASEXPR,
   ZH_ET_SETGET,
   ZH_ET_SEND,
   ZH_ET_FUNNAME,
   ZH_ET_ALIAS,
   ZH_ET_RTVAR,      /* PRIVATE or PUBLIC declaration of variable */
   ZH_ET_VARIABLE,
   ZH_EO_POSTINC,    /* post-operators -> lowest precedence */
   ZH_EO_POSTDEC,
   ZH_EO_ASSIGN,     /* assignments */
   ZH_EO_PLUSEQ,
   ZH_EO_MINUSEQ,
   ZH_EO_MULTEQ,
   ZH_EO_DIVEQ,
   ZH_EO_MODEQ,
   ZH_EO_EXPEQ,
   ZH_EO_OR,         /* logical operators */
   ZH_EO_AND,
   ZH_EO_NOT,
   ZH_EO_EQUAL,      /* relational operators */
   ZH_EO_EQ,
   ZH_EO_NE,
   ZH_EO_IN,
   ZH_EO_LT,
   ZH_EO_GT,
   ZH_EO_LE,
   ZH_EO_GE,
   ZH_EO_PLUS,       /* addition */
   ZH_EO_MINUS,
   ZH_EO_MULT,       /* multiple */
   ZH_EO_DIV,
   ZH_EO_MOD,
   ZH_EO_POWER,
   ZH_EO_NEGATE,     /* sign operator */
   ZH_EO_PREINC,
   ZH_EO_PREDEC      /* pre-operators -> the highest precedence */
} ZH_EXPR_OPERATOR;

#define ZH_EXPR_COUNT   ( ZH_EO_PREDEC + 1 )

typedef enum
{
   ZH_F_UDF = 0,
   ZH_F_AADD,
   ZH_F_ABS,
   ZH_F_ARRAY,
   ZH_F_ASC,
   ZH_F_AT,
   ZH_F_BOF,
   ZH_F_BREAK,
   ZH_F_CDOW,
   ZH_F_CHR,
   ZH_F_CMONTH,
   ZH_F_COL,
   ZH_F_CTOD,
   ZH_F_DATE,
   ZH_F_DAY,
   ZH_F_DELETED,
   ZH_F_DEVPOS,
   ZH_F_DOW,
   ZH_F_DTOC,
   ZH_F_DTOS,
   ZH_F_EMPTY,
   ZH_F_EOF,
   ZH_F_EVAL,
   ZH_F_EXP,
   ZH_F_FCOUNT,
   ZH_F_FIELDNAME,
   ZH_F_FILE,
   ZH_F_FLOCK,
   ZH_F_FOUND,
   ZH_F_INKEY,
   ZH_F_INT,
   ZH_F_LASTREC,
   ZH_F_LEFT,
   ZH_F_LEN,
   ZH_F_LOCK,
   ZH_F_LOG,
   ZH_F_LOWER,
   ZH_F_LTRIM,
   ZH_F_MAX,
   ZH_F_MIN,
   ZH_F_MONTH,
   ZH_F_PCOL,
   ZH_F_PCOUNT,
   ZH_F_PROW,
   ZH_F_QSELF,
   ZH_F_RECCOUNT,
   ZH_F_RECNO,
   ZH_F_REPLICATE,
   ZH_F_RLOCK,
   ZH_F_ROUND,
   ZH_F_ROW,
   ZH_F_RTRIM,
   ZH_F_SECONDS,
   ZH_F_SELECT,
   ZH_F_SETPOS,
   ZH_F_SETPOSBS,
   ZH_F_SPACE,
   ZH_F_SQRT,
   ZH_F_STOD,
   ZH_F_STOT,
   ZH_F_STR,
   ZH_F_SUBSTR,
   ZH_F_TIME,
   ZH_F_TRANSFORM,
   ZH_F_TRIM,
   ZH_F_TYPE,
   ZH_F_UPPER,
   ZH_F_VAL,
   ZH_F_VALTYPE,
   ZH_F_WORD,
   ZH_F_YEAR,

   ZH_F_BCHAR,
   ZH_F_BCODE,
   ZH_F_BITAND,
   ZH_F_BITOR,
   ZH_F_BITXOR,
   ZH_F_BITSET,
   ZH_F_BITRESET,
   ZH_F_BITSHIFT,
   ZH_F_BITTEST,
   ZH_F_BITNOT,
   ZH_F_ARRAYTOPARAMS,
   ZH_F_I18N_GETTEXT,
   ZH_F_I18N_GETTEXT_STRICT,
   ZH_F_I18N_GETTEXT_NOOP,
   ZH_F_I18N_NGETTEXT,
   ZH_F_I18N_NGETTEXT_STRICT,
   ZH_F_I18N_NGETTEXT_NOOP,
   ZH_F__GET_
} ZH_FUNC_ID;

#define ZH_FN_UDF       0
#define ZH_FN_RESERVED  1
#define ZH_FN_MULTIARG  2

typedef ZH_USHORT ZH_EXPRTYPE;

typedef struct ZH_EXPR_
{
   union
   {
      ZH_BOOL asLogical;      /* logical value */
      struct
      {
         const char * name;   /* variable/function name */
         ZH_FUNC_ID funcid;   /* function ID */
         int flags;           /* function flags */
      } asSymbol;
      struct
      {
         char * string;       /* literal strings */
         ZH_BOOL dealloc;     /* automatically deallocate on expression deletion */
      } asString;
      struct
      {
         struct ZH_EXPR_ * pMacro;  /* macro variable */
         const char * szName;       /* variable name */
      } asRTVar;                 /* PUBLIC or PRIVATE variable declaration */
      struct
      {
         struct ZH_EXPR_ * pVar;    /* variable */
         struct ZH_EXPR_ * pExpr;   /* expression */
      } asSetGet;                /* IIF( <var>==NIL, <expr>, <expr>:=<var> ) */
      struct
      {
         union {
            ZH_MAXINT l;            /* long value */
            double    d;            /* double value */
         } val;
         unsigned char bWidth;   /* unsigned char used intentionally */
         unsigned char bDec;     /* unsigned char used intentionally */
         unsigned char NumType;  /* used to distinguish LONG and DOUBLE */
      } asNum;
      struct
      {
         long  lDate;            /* Julian date */
         long  lTime;            /* time in milliseconds */
      } asDate;
      struct
      {
         const char * szMacro;         /* identifier after the macro operator */
         struct ZH_EXPR_ * pExprList;  /* list elements if &(...) was used */
         ZH_USHORT SubType;            /* context in which macro is used */
         unsigned char cMacroOp;       /* macro operator */
      } asMacro;
      struct
      {
         struct ZH_EXPR_ * pExprList;  /* list elements */
         struct ZH_EXPR_ * pIndex;     /* array index, others */
         ZH_BOOL  reference;           /* push array item by reference or pass variable parameters to called function or method */
      } asList;
      struct
      {
         struct ZH_EXPR_ * pExprList;  /* list elements */
         PZH_CBVAR pLocals;            /* list of local variables */
         char * string;                /* source code of a codeblock */
         ZH_USHORT flags;              /* ZH_BLOCK_* */
      } asCodeblock;
      struct
      {
         struct ZH_EXPR_ * pAlias;     /* alias expression */
         struct ZH_EXPR_ * pVar;       /* aliased variable or macro */
         struct ZH_EXPR_ * pExpList;   /* aliased expression list */
      } asAlias;
      struct
      {
         struct ZH_EXPR_ * pFunName;   /* function name */
         struct ZH_EXPR_ * pParms;     /* function call parameters */
      } asFunCall;
      struct
      {
         struct ZH_EXPR_ * pObject;    /* object */
         struct ZH_EXPR_ * pParms;     /* method parameters */
         const char * szMessage;       /* message as string */
         struct ZH_EXPR_ * pMessage;   /* message as macro */
      } asMessage;
      struct
      {
         struct ZH_EXPR_ * pLeft;      /* object */
         struct ZH_EXPR_ * pRight;     /* object */
      } asOperator;
      struct ZH_EXPR_ * asReference;
   } value;
   ZH_SIZE     nLength;
   ZH_EXPRTYPE ExprType;      /* internal expression type */
   ZH_USHORT   ValType;       /* language level value type */
   struct ZH_EXPR_ * pNext;   /* next expression in the list of expressions */
} ZH_EXPR, * PZH_EXPR;

typedef struct ZH_ENUMERATOR_
{
   const char * szName;
   int iForEachDir;     /* 0 - standard FOR/NEXT, 1(-1) FOR EACH(descendant) */
   struct ZH_ENUMERATOR_ *pNext;
} ZH_ENUMERATOR, * PZH_ENUMERATOR; /* support structure for FOR EACH statements */

/* support structure for else if pcode fixups */
typedef struct ZH_ELSEIF_
{
   ZH_SIZE  nOffset;
   struct   ZH_ELSEIF_ * pElseif;   /* next ELSEIF in the current IF statement */
   struct   ZH_ELSEIF_ * pPrev;     /* previous IF statement */
} ZH_ELSEIF, * PZH_ELSEIF;

/* support structure for EXIT and LOOP statements */
typedef struct ZH_LOOPEXIT_
{
   ZH_SIZE   nOffset;
   ZH_BOOL   fCanLoop;
   ZH_USHORT wSeqCounter;
   ZH_USHORT wAlwaysCounter;
   ZH_USHORT wWithObjectCnt;
   struct ZH_LOOPEXIT_ * pLoopList;
   struct ZH_LOOPEXIT_ * pExitList;
   struct ZH_LOOPEXIT_ * pNext;
} ZH_LOOPEXIT, * PZH_LOOPEXIT;

/* support structure for SWITCH statement */
typedef struct ZH_SWITCHCASE_
{
   ZH_SIZE nOffset;
   PZH_EXPR pExpr;
   struct ZH_SWITCHCASE_ * pNext;
} ZH_SWITCHCASE, * PZH_SWITCHCASE;

typedef struct ZH_SWITCHCMD_
{
   ZH_SIZE nOffset;
   PZH_SWITCHCASE pCases;
   PZH_SWITCHCASE pLast;
   PZH_EXPR pExpr;
   ZH_SIZE nDefault;
   struct ZH_SWITCHCMD_ * pPrev;
} ZH_SWITCHCMD, * PZH_SWITCHCMD;

/* support structure for PUBLIC and PRIVATE statements */
typedef struct ZH_RTVAR_
{
   PZH_EXPR pVar;
   ZH_BOOL bPopValue;
   struct ZH_RTVAR_ * pNext;
   struct ZH_RTVAR_ * pPrev;
} ZH_RTVAR, * PZH_RTVAR;

/* structure to hold a Clipper defined function */
typedef struct _ZH_ZFUNC
{
   const char * szName;                   /* name of a defined Ziher function */
   ZH_SYMBOLSCOPE cScope;                 /* scope of a defined Ziher function */
   ZH_USHORT    funFlags;                 /* some flags we may need */
   ZH_USHORT    wParamCount;              /* number of declared parameters */
   ZH_USHORT    wParamNum;                /* current parameter number */
   PZH_HVAR     pLocals;                  /* pointer to local variables list */
   PZH_HVAR     pStatics;                 /* pointer to static variables list */
   PZH_HVAR     pFields;                  /* pointer to fields variables list */
   PZH_HVAR     pMemvars;                 /* pointer to memvar variables list */
   PZH_HVAR     pDetached;                /* pointer to detached local variables list */
   PZH_HVAR     pPrivates;                /* pointer to private variables list */
   ZH_BYTE *    pCode;                    /* pointer to a memory block where pcode is stored */
   ZH_SIZE      nPCodeSize;               /* total memory size for pcode */
   ZH_SIZE      nPCodePos;                /* actual pcode offset */
   ZH_SIZE *    pNOOPs;                   /* pointer to the NOOP array */
   ZH_SIZE *    pJumps;                   /* pointer to the Jumps array */
   ZH_SIZE      nNOOPs;                   /* NOOPs Counter */
   ZH_SIZE      nJumps;                   /* Jumps Counter */
   int          iStaticsBase;             /* base for this function statics */
   int          iFuncSuffix;              /* function suffix for multiple static functions with the same name */
   int          iEarlyEvalPass;           /* !=0 if early evaluated block is compiled - accessing of declared (compile time) variables is limited */
   ZH_BOOL      fVParams;                 /* ZH_TRUE if variable number of parameters is used */
   ZH_BOOL      bError;                   /* error during function compilation */
   ZH_BOOL      bBlock;                   /* ZH_TRUE if simple codeblock body is compiled */
   struct _ZH_ZFUNC * pOwner;             /* pointer to the function/procedure that owns the codeblock */
   struct _ZH_ZFUNC * pNext;              /* pointer to the next defined function */
   PZH_ENUMERATOR    pEnum;               /* pointer to FOR EACH variables */
   PZH_LOOPEXIT      pLoops;
   PZH_SWITCHCMD     pSwitch;
   PZH_ELSEIF        elseif;
   PZH_RTVAR         rtvars;
   ZH_USHORT         wSeqBegCounter;
   ZH_USHORT         wSeqCounter;
   ZH_USHORT         wAlwaysCounter;
   ZH_USHORT         wForCounter;
   ZH_USHORT         wIfCounter;
   ZH_USHORT         wWhileCounter;
   ZH_USHORT         wCaseCounter;
   ZH_USHORT         wSwitchCounter;
   ZH_USHORT         wWithObjectCnt;
} ZH_ZFUNC, * PZH_ZFUNC;

/* structure to hold PP #define variables passed as command-line parameters */
typedef struct _ZH_PPDEFINE
{
   char * szName;                         /* name of PP #define variable */
   const char * szValue;                  /* value of PP #define variable */
   struct _ZH_PPDEFINE * pNext;           /* pointer to the next var */
} ZH_PPDEFINE, * PZH_PPDEFINE;

/* structure to hold an INLINE block of source */
typedef struct _ZH_HINLINE
{
   const char * szName;                   /* name of a inline function */
   ZH_BYTE *    pCode;                    /* pointer to a memory block where pcode is stored */
   ZH_SIZE      nPCodeSize;               /* total memory size for pcode */
   const char * szFileName;               /* Source file name */
   int          iLine;                    /* Source line number */
   struct _ZH_HINLINE * pNext;               /* pointer to the next defined inline */
} ZH_HINLINE, * PZH_HINLINE;

/* structure to hold a called functions */
typedef struct _ZH_ZFUNCALL
{
   const char * szName;                   /* name of a called function */
   ZH_SYMBOLSCOPE cScope;                 /* the scope of the function */
   struct _ZH_ZFUNCALL * pNext;              /* pointer to the next called function */
} ZH_ZFUNCALL, PZH_ZFUNCALL;

/* structure to control all Clipper defined functions */
typedef struct
{
   PZH_ZFUNC pFirst;            /* pointer to the first defined function */
   PZH_ZFUNC pLast;             /* pointer to the last defined function */
   int       iCount;            /* number of defined functions */
} ZH_ZFUNCTION_LIST;

/* structure to control all Clipper defined functions */
typedef struct
{
   PZH_HINLINE pFirst;        /* pointer to the first defined inline */
   PZH_HINLINE pLast;         /* pointer to the last defined inline */
   int         iCount;        /* number of defined inlines */
} ZH_HINLINE_LIST;

/* compiler symbol support structure */
typedef struct _ZH_HSYMBOL
{
   const char *   szName;     /* the name of the symbol */
   ZH_SYMBOLSCOPE cScope;     /* the scope of the symbol */
   int            iFunc;      /* is it a function name (TRUE) or memvar (FALSE) */
   PZH_ZFUNC      pFunc;
   struct _ZH_HSYMBOL * pNext; /* pointer to the next defined symbol */
} ZH_HSYMBOL, * PZH_HSYMBOL;

/* symbol table support structures */
typedef struct
{
   PZH_HSYMBOL pFirst;          /* pointer to the first defined symbol */
   PZH_HSYMBOL pLast;           /* pointer to the last defined symbol */
   int         iCount;          /* number of defined symbols */
} ZH_HSYMBOL_LIST;

typedef struct _ZH_HEXTERN
{
   const char * szName;         /* name of the extern function */
   ZH_SYMBOLSCOPE cScope;       /* the scope of the function */
   struct _ZH_HEXTERN * pNext;
} ZH_HEXTERN, * PZH_HEXTERN;      /* support structure for extern symbols */
/* as they have to be placed on the symbol table later than the first public symbol */

typedef struct _ZH_MODULE
{
   const char *         szName;
   ZH_BOOL              force;  /* force module compilation */
   struct _ZH_MODULE *  pNext;
} ZH_MODULE, * PZH_MODULE;

/* definitions for zh_compPCodeEval() support */
typedef void * PZH_VOID;
#define ZH_PCODE_FUNC( func, type ) ZH_SIZE func( PZH_ZFUNC pFunc, ZH_SIZE nPCodePos, type cargo )
typedef ZH_PCODE_FUNC( ( * PZH_PCODE_FUNC ), PZH_VOID );

typedef struct _ZH_DEBUGINFO
{
   char *    pszModuleName;
   ZH_ULONG  ulFirstLine;
   ZH_ULONG  ulLastLine;
   ZH_ULONG  ulAllocated;
   ZH_BYTE * pLineMap;
   struct _ZH_DEBUGINFO * pNext;
} ZH_DEBUGINFO, * PZH_DEBUGINFO;

typedef struct _ZH_LABEL_INFO
{
   FILE *    yyc;
   ZH_BOOL   fVerbose;
   ZH_BOOL   fSetSeqBegin;
   ZH_BOOL   fCondJump;
   ZH_BOOL   fEndRequest;
   int       iNestedBlock;
   ZH_SIZE * pnLabels;
   const PZH_PCODE_FUNC * pFuncTable;
} ZH_LABEL_INFO, * PZH_LABEL_INFO;

#define ZH_MODE_COMPILER      1
#define ZH_MODE_MACRO         2

struct _ZH_COMP_FUNCS;

#if defined( ZH_COMMON_SUPPORT )

typedef struct _ZH_COMMON
{
   /* common to macro compiler members */
   int    mode;               /* ZH_MODE_* */
   int    supported;          /* various flags for supported capabilities */
   const struct _ZH_COMP_FUNCS * funcs;
} ZH_COMMON, * PZH_COMMON;

#define ZH_COMP_PARAM         pCommon
#define ZH_COMP_DECL          PZH_COMMON ZH_COMP_PARAM

#elif defined( ZH_MACRO_SUPPORT )

#define ZH_COMP_PARAM         pMacro
#define ZH_COMP_DECL          PZH_MACRO ZH_COMP_PARAM

typedef struct ZH_PCODE_INFO_ /* compiled pcode container for macro compiler */
{
   ZH_BYTE * pCode;        /* pointer to a memory block where pcode is stored */
   ZH_SIZE nPCodeSize;     /* total memory size for pcode */
   ZH_SIZE nPCodePos;      /* actual pcode offset */
   ZH_BOOL fVParams;       /* function/codeblock with variable parameters */
   PZH_CBVAR pLocals;
   struct ZH_PCODE_INFO_ * pPrev;
} ZH_PCODE_INFO, * PZH_PCODE_INFO;

typedef struct ZH_MACRO_      /* a macro compiled pcode container */
{
   /* common to compiler members */
   int      mode;             /* ZH_MODE_* */
   int      supported;        /* various flags for supported capabilities */
   const struct _ZH_COMP_FUNCS * funcs;

   /* macro compiler only members */
   const char * string;       /* compiled string */
   ZH_SIZE  length;           /* length of the string */
   int      Flags;            /* some flags we may need */
   int      status;           /* status of compilation */
   PZH_ITEM pError;           /* error object returned from the parser */
   PZH_PCODE_INFO pCodeInfo;  /* pointer to pcode buffer and info */
   void *   pLex;             /* lexer buffer pointer */
   void *   pExprLst;         /* list with allocated expressions */
   void *   pIdentLst;        /* list with allocated identifiers */
   int      exprType;         /* type of successfully compiled expression */
   ZH_USHORT uiListElements;  /* number of elements in macro list expression */
   ZH_USHORT uiNameLen;       /* the maximum symbol name length */
   ZH_PCODE_INFO pCodeInfoBuffer;
} ZH_MACRO;

#else

#define ZH_COMP_PARAM         pComp
#define ZH_COMP_DECL          PZH_COMP ZH_COMP_PARAM

#define ZH_I18N_PLURAL_MAX    8

typedef struct _ZH_I18NPOS
{
   const char *   szFile;
   ZH_UINT        uiLine;
} ZH_I18NPOS, *PZH_I18NPOS;

typedef struct _ZH_I18NSTRING
{
   const char *   szText;
   const char *   szContext;
   const char *   szPlurals[ ZH_I18N_PLURAL_MAX ];
   ZH_UINT        uiPlurals;
   ZH_I18NPOS     pPos;
   ZH_I18NPOS *   pPosLst;
   ZH_UINT        uiPosCount;
} ZH_I18NSTRING, * PZH_I18NSTRING;

typedef struct _ZH_I18NTABLE
{
   PZH_I18NSTRING    pString;
   ZH_ULONG          uiCount;
   ZH_ULONG          uiAllocated;
} ZH_I18NTABLE, * PZH_I18NTABLE;

typedef struct _ZH_COMP_LEX
{
   PZH_PP_STATE   pPP;
   int            iState;
   int            iClose;
   int            iScope;
   ZH_BOOL        fEol;
   const char *   lasttok;
} ZH_COMP_LEX, * PZH_COMP_LEX;

typedef struct _ZH_EXPRLST
{
   ZH_EXPR Expression;
   struct _ZH_EXPRLST * pPrev;
   struct _ZH_EXPRLST * pNext;
} ZH_EXPRLST, * PZH_EXPRLST;

typedef struct _ZH_INCLST
{
   struct _ZH_INCLST * pNext;
   char szFileName[ 1 ];
} ZH_INCLST, * PZH_INCLST;

typedef struct _ZH_COMP
{
   /* common to macro compiler members */
   int    mode;            /* ZH_MODE_* */
   int    supported;       /* various flags for supported capabilities */
   const struct _ZH_COMP_FUNCS * funcs;

   /* compiler only members */
   PZH_COMP_LEX      pLex;
   PZH_EXPRLST       pExprLst;

   PZH_HASH_TABLE    pIdentifiers;
   ZH_ZFUNCTION_LIST functions;
   ZH_HSYMBOL_LIST   symbols;
   ZH_HINLINE_LIST   inlines;
   PZH_HEXTERN       externs;
   PZH_MODULE        modules;
   PZH_VARTYPE       pVarType;
   PZH_INCLST        incfiles;
   PZH_PPDEFINE      ppdefines;

   PZH_HDECLARED     pFirstDeclared;
   PZH_HDECLARED     pLastDeclared;
   PZH_HDECLARED     pLastMethod;
   PZH_HCLASS        pFirstClass;
   PZH_HCLASS        pLastClass;

   PZH_ZFUNC         pInitFunc;
   PZH_ZFUNC         pLineFunc;
   PZH_ZFUNC         pDeclFunc;
   PZH_FNAME         pFileName;
   PZH_FNAME         pOutPath;
   PZH_FNAME         pPpoPath;
   PZH_FNAME         pI18nFileName;
   PZH_I18NTABLE     pI18n;
   ZH_BOOL           fI18n;

   void              ( * outStdFunc ) ( void *, const char * );
   void              ( * outErrFunc ) ( void *, const char * );
   PZH_PP_MSG_FUNC   outMsgFunc;
   void *            cargo;

   ZH_SIZE           nOutBufSize;         /* memory output buffer size */
   ZH_BYTE *         pOutBuf;             /* memory output buffer address */

   int               lastLine;            /* last generated in PCODE line number */
   int               currLine;            /* currently compiled line number */
   const char *      lastModule;          /* last generated in PCODE module name */
   const char *      currModule;          /* currently compiled module name */

   const char *      szAnnounce;
   const char *      szDeclaredFun;
   const char *      szFile;              /* Source file name of compiled module */
   char *            szDepExt;            /* destination file extension used in decencies list */
   char *            szStdCh;             /* standard definitions file name (-u) */
   char **           szStdChExt;          /* extended definitions file names (-u+<file>) */
   int               iStdChExt;           /* number of extended definition files (-u+<file>) */

   ZH_BYTE           cDataListType;       /* current declared variable list type */

   int               iErrorCount;
   int               iModulesCount;       /* number of compiled .zh modules */
   int               iStartProc;          /* holds if we need to create the starting procedure */
   int               iMaxTransCycles;     /* maximum translate cycles in PP (-r=<n>) */
   int               iHidden;             /* hide strings */
   int               iWarnings;           /* enable parse warnings */
   int               iExitLevel;          /* holds if there was any warning during the compilation process */
   int               iStaticCnt;          /* number of defined statics variables on the PRG */
   int               iVarScope;           /* holds the scope for next variables to be defined */
   int               iLanguage;           /* default Ziher generated output language */
   int               iGenCOutput;         /* C code generation should be verbose (use comments) or not */
   int               ilastLineErr;        /* line number with last syntax error */
   int               iTraceInclude;       /* trace included files and generate dependencies list */
   int               iSyntaxCheckOnly;    /* syntax check only */
   int               iErrorFmt;           /* error message formatting mode (default: Clipper) */

   ZH_BOOL           fQuiet;              /* be quiet during compilation (-q) */
   ZH_BOOL           fGauge;              /* hide line counter gauge (-ql) */
   ZH_BOOL           fFullQuiet;          /* be quiet during compilation disable all messages */
   ZH_BOOL           fExit;               /* force breaking compilation process */
   ZH_BOOL           fPPO;                /* flag indicating, is .ppo output needed */
   ZH_BOOL           fPPT;                /* flag indicating, is .ppt output needed */
   ZH_BOOL           fLineNumbers;        /* holds if we need pcodes with line numbers */
   ZH_BOOL           fAnyWarning;         /* holds if there was any warning during the compilation process */
   ZH_BOOL           fAutoMemvarAssume;   /* holds if undeclared variables are automatically assumed MEMVAR (-a)*/
   ZH_BOOL           fForceMemvars;       /* holds if memvars are assumed when accessing undeclared variable (-v)*/
   ZH_BOOL           fDebugInfo;          /* holds if generate debugger required info */
   ZH_BOOL           fHideSource;         /* do not embed original source filename into generated source code */
   ZH_BOOL           fNoStartUp;          /* C code generation embed ZH_FS_FIRST or not */
   ZH_BOOL           fCredits;            /* print credits */
   ZH_BOOL           fBuildInfo;          /* print build info */
   ZH_BOOL           fLogo;               /* print logo */
   ZH_BOOL           fSwitchCase;         /* generate PCODE for CASE value of SWITCH statement */
   ZH_BOOL           fDescend;            /* add descendant FOR EACH iterators */
   ZH_BOOL           fSingleModule;       /* do not automatically compile DO...[WITH...] external modules (-m) */
   ZH_BOOL           fError;              /* error appeared during compilation */
   ZH_BOOL           fNoArchDefs;         /* do not define architecture dependent macros: __PLATFORM__*, __ARCH??BIT__, __*_ENDIAN__ */
   ZH_BOOL           fMeaningful;         /* do not generate warnings about meaningless expression usage */
   ZH_BOOL           fINCLUDE;            /* use INCLUDE envvar as header path (default) */
} ZH_COMP, * PZH_COMP;

typedef struct
{
   ZH_BOOL  fDebugInfo;
   ZH_BOOL  fHideSource;
   ZH_BOOL  fAutoMemvarAssume;
   ZH_BOOL  fI18n;
   ZH_BOOL  fLineNumbers;
   ZH_BOOL  fPPO;
   ZH_BOOL  fPPT;
   ZH_BOOL  fQuiet;
   ZH_BOOL  fForceMemvars;
   int      iStartProc;
   int      iWarnings;
   int      iGenCOutput;
   int      iExitLevel;
   int      iHidden;
   int      supported;
} ZH_COMP_SWITCHES, * PZH_COMP_SWITCHES;

extern PZH_COMP zh_comp_new( void );
extern void zh_comp_free( PZH_COMP );

#endif /* ! ZH_MACRO_SUPPORT */

typedef struct _ZH_COMP_FUNCS
{
   PZH_EXPR ( * ExprNew )        ( ZH_COMP_DECL, ZH_EXPRTYPE iType );
   void     ( * ExprClear )      ( ZH_COMP_DECL, PZH_EXPR pExpr );
   void     ( * ExprFree )       ( ZH_COMP_DECL, PZH_EXPR pExpr );

   PZH_EXPR ( * ErrorType )      ( ZH_COMP_DECL, PZH_EXPR );
   PZH_EXPR ( * ErrorSyntax )    ( ZH_COMP_DECL, PZH_EXPR );
   void     ( * ErrorDuplVar )   ( ZH_COMP_DECL, const char * );
} ZH_COMP_FUNCS, * PZH_COMP_FUNCS;


#define ZH_MACRO_DATA         ZH_COMP_PARAM
#define ZH_PCODE_DATA         ( ZH_MACRO_DATA->pCodeInfo )


/* Support for traversing of linked list */
#define ZH_COMP_CARGO_FUNC( proc )   void proc( ZH_COMP_DECL, void * cargo )
typedef ZH_COMP_CARGO_FUNC( ( * PZH_COMP_CARGO_FUNC ) );

#define ZH_COMP_CARGO2_FUNC( proc )  void proc( ZH_COMP_DECL, void * cargo, void * dummy )
typedef ZH_COMP_CARGO2_FUNC( ( * PZH_COMP_CARGO2_FUNC ) );

/* pcode chunks bytes size */
#define ZH_PCODE_CHUNK   100


ZH_EXTERN_END

#endif /* ZH_COMPDF_H_ */
