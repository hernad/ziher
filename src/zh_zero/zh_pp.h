/*
 * Ziher preprocessor
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

#ifndef ZH_PP_H_
#define ZH_PP_H_

#include "zh_api.h"
#include "zh_apifs.h"

ZH_EXTERN_BEGIN

#define ZH_BLOCK_MACROVAR  1
/* #define ZH_BLOCK_LATEEVAL  2 */
#define ZH_BLOCK_VPARAMS   4
#define ZH_BLOCK_EXT       8
#define ZH_BLOCK_REDUCE   16

/* #pragma {__text,__stream,__cstream}|functionOut|functionEnd|functionStart */
#define ZH_PP_STREAM_OFF      0 /* standard preprocessing */
#define ZH_PP_STREAM_COMMENT  1 /* multiline comment */
#define ZH_PP_STREAM_DUMP_C   2 /* pragma BEGINDUMP */
#define ZH_PP_STREAM_TEXT     3 /* TEXT/ENDTEXT */
#define ZH_PP_STREAM_PRG      4 /* TEXT/ENDTEXT lines joined with LF */
#define ZH_PP_STREAM_C        5 /* TEXT/ENDTEXT lines joined and ESC sequences processed */
#define ZH_PP_STREAM_INLINE_C 6 /* zh_inLIne() {...} data, should not be preprocessed */
#define ZH_PP_STREAM_BINARY   7 /* __binarystreaminclude */

/* zh_inLine() states */
#define ZH_PP_INLINE_OFF      0
#define ZH_PP_INLINE_START    1
#define ZH_PP_INLINE_PARAM    2
#define ZH_PP_INLINE_BODY     3
#define ZH_PP_INLINE_COMMENT  4
#define ZH_PP_INLINE_QUOTE1   5
#define ZH_PP_INLINE_QUOTE2   6

/* actions returned by function to open included files */
#define ZH_PP_OPEN_OK         0
#define ZH_PP_OPEN_FILE       1
#define ZH_PP_OPEN_ERROR      2

/* function to open included files */
#define ZH_PP_OPEN_FUNC_( func ) int func( void *, char *, ZH_BOOL, ZH_BOOL, ZH_BOOL, ZH_PATHNAMES *, ZH_BOOL *, FILE **, const char **, ZH_SIZE *, ZH_BOOL * )
typedef ZH_PP_OPEN_FUNC_( ( * PZH_PP_OPEN_FUNC ) );

/* function to close included files */
#define ZH_PP_CLOSE_FUNC_( func ) void func( void *, FILE * )
typedef ZH_PP_CLOSE_FUNC_( ( * PZH_PP_CLOSE_FUNC ) );

/* function to generate errors */
#define ZH_PP_ERROR_FUNC_( func ) void func( void *, const char * const *, char, int, const char *, const char * )
typedef ZH_PP_ERROR_FUNC_( ( * PZH_PP_ERROR_FUNC ) );

/* function to redirect stdout messages */
#define ZH_PP_DISP_FUNC_( func ) void func( void *, const char * )
typedef ZH_PP_DISP_FUNC_( ( * PZH_PP_DISP_FUNC ) );

/* function for catching #pragma dump data */
#define ZH_PP_DUMP_FUNC_( func ) void func( void *, char *, ZH_SIZE, int )
typedef ZH_PP_DUMP_FUNC_( ( * PZH_PP_DUMP_FUNC ) );

/* function for catching ZH_INLINE(...){...} data */
#define ZH_PP_INLINE_FUNC_( func ) void func( void *, char *, char *, ZH_SIZE, int )
typedef ZH_PP_INLINE_FUNC_( ( * PZH_PP_INLINE_FUNC ) );

/* function for catching #pragma dump data */
#define ZH_PP_SWITCH_FUNC_( func ) ZH_BOOL func( void *, const char *, int *, ZH_BOOL )
typedef ZH_PP_SWITCH_FUNC_( ( * PZH_PP_SWITCH_FUNC ) );

/* function to register included files */
#define ZH_PP_INC_FUNC_( func ) void func( void *, const char * )
typedef ZH_PP_INC_FUNC_( ( * PZH_PP_INC_FUNC ) );

/* function to generate errors */
#define ZH_PP_MSG_FUNC_( func ) void func( void * cargo, int iErrorFmt, int iLine, const char * szModule, char cPrefix, int iValue, const char * szText, const char * szPar1, const char * szPar2 )
typedef ZH_PP_MSG_FUNC_( ( * PZH_PP_MSG_FUNC ) );


/* preprocessor tokens */
#define ZH_PP_TOKEN_NUL          0

#define ZH_PP_MMARKER_REGULAR    1
#define ZH_PP_MMARKER_LIST       2
#define ZH_PP_MMARKER_RESTRICT   3
#define ZH_PP_MMARKER_WILD       4
#define ZH_PP_MMARKER_EXTEXP     5
#define ZH_PP_MMARKER_NAME       6
#define ZH_PP_MMARKER_OPTIONAL   7

#define ZH_PP_RMARKER_REGULAR    11
#define ZH_PP_RMARKER_STRDUMP    12
#define ZH_PP_RMARKER_STRSTD     13
#define ZH_PP_RMARKER_STRSMART   14
#define ZH_PP_RMARKER_BLOCK      15
#define ZH_PP_RMARKER_LOGICAL    16
#define ZH_PP_RMARKER_NUL        17
#define ZH_PP_RMARKER_OPTIONAL   18
#define ZH_PP_RMARKER_DYNVAL     19
#define ZH_PP_RMARKER_REFERENCE  20

/* keywords, pseudo keywords and PP only tokens */
#define ZH_PP_TOKEN_KEYWORD      21
#define ZH_PP_TOKEN_MACROVAR     22
#define ZH_PP_TOKEN_MACROTEXT    23
#define ZH_PP_TOKEN_TEXT         24
#define ZH_PP_TOKEN_OTHER        25   /* non keyword, text, or operator character */
#define ZH_PP_TOKEN_BACKSLASH    26   /* "\\" */
#define ZH_PP_TOKEN_PIPE         27   /* "|" */
#define ZH_PP_TOKEN_DOT          28   /* "." */
#define ZH_PP_TOKEN_COMMA        29   /* "," */
#define ZH_PP_TOKEN_EOC          30   /* ";" */
#define ZH_PP_TOKEN_EOL          31   /* "\n" */
#define ZH_PP_TOKEN_HASH         32   /* "#" */
#define ZH_PP_TOKEN_DIRECTIVE    33   /* direct # directive first token */

/* constant values */
#define ZH_PP_TOKEN_STRING       41
#define ZH_PP_TOKEN_NUMBER       42
#define ZH_PP_TOKEN_DATE         43
#define ZH_PP_TOKEN_TIMESTAMP    44
#define ZH_PP_TOKEN_LOGICAL      45

/* operators */
#define ZH_PP_TOKEN_LEFT_PB      50
#define ZH_PP_TOKEN_RIGHT_PB     51
#define ZH_PP_TOKEN_LEFT_SB      52
#define ZH_PP_TOKEN_RIGHT_SB     53
#define ZH_PP_TOKEN_LEFT_CB      54
#define ZH_PP_TOKEN_RIGHT_CB     55
#define ZH_PP_TOKEN_REFERENCE    56
#define ZH_PP_TOKEN_AMPERSAND    57
#define ZH_PP_TOKEN_SEND         58
#define ZH_PP_TOKEN_ALIAS        59

#define ZH_PP_TOKEN_ASSIGN       60
#define ZH_PP_TOKEN_PLUSEQ       61
#define ZH_PP_TOKEN_MINUSEQ      62
#define ZH_PP_TOKEN_MULTEQ       63
#define ZH_PP_TOKEN_DIVEQ        64
#define ZH_PP_TOKEN_MODEQ        65
#define ZH_PP_TOKEN_EXPEQ        66

#define ZH_PP_TOKEN_INC          67
#define ZH_PP_TOKEN_DEC          68
#define ZH_PP_TOKEN_NOT          69
#define ZH_PP_TOKEN_OR           70
#define ZH_PP_TOKEN_AND          71
#define ZH_PP_TOKEN_EQUAL        72
#define ZH_PP_TOKEN_EQ           73
#define ZH_PP_TOKEN_LT           74
#define ZH_PP_TOKEN_GT           75
#define ZH_PP_TOKEN_LE           76
#define ZH_PP_TOKEN_GE           77
#define ZH_PP_TOKEN_NE           78
#define ZH_PP_TOKEN_IN           79
#define ZH_PP_TOKEN_PLUS         80
#define ZH_PP_TOKEN_MINUS        81
#define ZH_PP_TOKEN_MULT         82
#define ZH_PP_TOKEN_DIV          83
#define ZH_PP_TOKEN_MOD          84
#define ZH_PP_TOKEN_POWER        85
#define ZH_PP_TOKEN_EPSILON      86
#define ZH_PP_TOKEN_SHIFTL       87
#define ZH_PP_TOKEN_SHIFTR       88
#define ZH_PP_TOKEN_BITXOR       89

#define ZH_PP_TOKEN_TYPE(t)      ( (t) & 0xff )
/* bitfields */
/* #define ZH_PP_TOKEN_UNARY        0x0100 */
/* #define ZH_PP_TOKEN_BINARY       0x0200 */
/* #define ZH_PP_TOKEN_JOINABLE     0x0400 */
#define ZH_PP_TOKEN_MATCHMARKER  0x2000
#define ZH_PP_TOKEN_STATIC       0x4000
#define ZH_PP_TOKEN_PREDEFINED   0x8000

#define ZH_PP_TOKEN_SETTYPE(t,n) do{ (t)->type = ( (t)->type & 0xff00 ) | (n); } while(0)

#define ZH_PP_TOKEN_ALLOC(t)     ( ( (t) & ZH_PP_TOKEN_STATIC ) == 0 )
#define ZH_PP_TOKEN_ISPREDEF(t)  ( ( (t)->type & ZH_PP_TOKEN_PREDEFINED ) != 0 )

/* These macros are very important for the PP behavior. They define what
   and how will be translated.
         EOL - end of line    => '\n' or NULL
         EOC - end of command => EOL or ';'
         EOS - end of subst   => EOL or ';' + '#'
         EOP - end of pattern => EOL for direct and EOC for indirect
 */

/* End Of Line */
#define ZH_PP_TOKEN_ISEOL(t)     ( (t) == NULL || \
                                   ZH_PP_TOKEN_TYPE((t)->type) == ZH_PP_TOKEN_EOL )
/* End Of Command */
#define ZH_PP_TOKEN_ISEOC(t)     ( ZH_PP_TOKEN_ISEOL(t) || \
                                   ZH_PP_TOKEN_TYPE((t)->type) == ZH_PP_TOKEN_EOC )


/* End Of Subst - define how many tokens in line should be translated */
#  define ZH_PP_TOKEN_ISEOS(t)   ( ZH_PP_TOKEN_ISEOL(t) || \
                                   ( ZH_PP_TOKEN_TYPE((t)->type) == ZH_PP_TOKEN_EOC && \
                                     (t)->pNext && \
                                     ( ZH_PP_TOKEN_TYPE((t)->pNext->type) == ZH_PP_TOKEN_HASH || \
                                       ZH_PP_TOKEN_TYPE((t)->pNext->type) == ZH_PP_TOKEN_DIRECTIVE ) ) )
/* End Of Pattern - the second parameter define if it's direct or indirect
                    pattern */
#  define ZH_PP_TOKEN_ISEOP(t,l) ( (l) ? ZH_PP_TOKEN_ISEOL(t) : ZH_PP_TOKEN_ISEOC(t) )


#define ZH_PP_TOKEN_ISDIRECTIVE(t)  ( ZH_PP_TOKEN_TYPE((t)->type) == ZH_PP_TOKEN_DIRECTIVE || \
                                      ZH_PP_TOKEN_TYPE((t)->type) == ZH_PP_TOKEN_HASH )

#define ZH_PP_TOKEN_CANJOIN(t)   ( ! ZH_PP_TOKEN_CLOSE_BR(t) && \
                                   ZH_PP_TOKEN_TYPE(t) != ZH_PP_TOKEN_KEYWORD && \
                                   ZH_PP_TOKEN_TYPE(t) != ZH_PP_TOKEN_MACROVAR && \
                                   ZH_PP_TOKEN_TYPE(t) != ZH_PP_TOKEN_MACROTEXT && \
                                   ZH_PP_TOKEN_TYPE(t) != ZH_PP_TOKEN_TEXT && \
                                   ZH_PP_TOKEN_TYPE(t) != ZH_PP_TOKEN_STRING && \
                                   ZH_PP_TOKEN_TYPE(t) != ZH_PP_TOKEN_NUMBER && \
                                   ZH_PP_TOKEN_TYPE(t) != ZH_PP_TOKEN_DATE && \
                                   ZH_PP_TOKEN_TYPE(t) != ZH_PP_TOKEN_TIMESTAMP && \
                                   ZH_PP_TOKEN_TYPE(t) != ZH_PP_TOKEN_LOGICAL )

#define ZH_PP_TOKEN_OPEN_BR(t)   ( ZH_PP_TOKEN_TYPE(t) == ZH_PP_TOKEN_LEFT_PB || \
                                   ZH_PP_TOKEN_TYPE(t) == ZH_PP_TOKEN_LEFT_SB || \
                                   ZH_PP_TOKEN_TYPE(t) == ZH_PP_TOKEN_LEFT_CB )

#define ZH_PP_TOKEN_CLOSE_BR(t)  ( ZH_PP_TOKEN_TYPE(t) == ZH_PP_TOKEN_RIGHT_PB || \
                                   ZH_PP_TOKEN_TYPE(t) == ZH_PP_TOKEN_RIGHT_SB || \
                                   ZH_PP_TOKEN_TYPE(t) == ZH_PP_TOKEN_RIGHT_CB )

#define ZH_PP_TOKEN_ISNEUTRAL(t) ( ZH_PP_TOKEN_TYPE(t) == ZH_PP_TOKEN_DEC || \
                                   ZH_PP_TOKEN_TYPE(t) == ZH_PP_TOKEN_INC )

#define ZH_PP_TOKEN_NEEDLEFT(t)  ( ZH_PP_TOKEN_TYPE((t)->type) == ZH_PP_TOKEN_ASSIGN || \
                                   ZH_PP_TOKEN_TYPE((t)->type) == ZH_PP_TOKEN_PLUSEQ || \
                                   ZH_PP_TOKEN_TYPE((t)->type) == ZH_PP_TOKEN_MINUSEQ || \
                                   ZH_PP_TOKEN_TYPE((t)->type) == ZH_PP_TOKEN_MULTEQ || \
                                   ZH_PP_TOKEN_TYPE((t)->type) == ZH_PP_TOKEN_DIVEQ || \
                                   ZH_PP_TOKEN_TYPE((t)->type) == ZH_PP_TOKEN_MODEQ || \
                                   ZH_PP_TOKEN_TYPE((t)->type) == ZH_PP_TOKEN_EXPEQ || \
                                   ZH_PP_TOKEN_TYPE((t)->type) == ZH_PP_TOKEN_EQUAL || \
                                   ZH_PP_TOKEN_TYPE((t)->type) == ZH_PP_TOKEN_EQ || \
                                   ( ZH_PP_TOKEN_TYPE((t)->type) == ZH_PP_TOKEN_SEND && \
                                     (t)->spaces == 0 && (t)->pNext && \
                                     ( ZH_PP_TOKEN_TYPE((t)->pNext->type) == ZH_PP_TOKEN_KEYWORD || \
                                       ZH_PP_TOKEN_TYPE((t)->pNext->type) == ZH_PP_TOKEN_MACROVAR || \
                                       ZH_PP_TOKEN_TYPE((t)->pNext->type) == ZH_PP_TOKEN_MACROTEXT ) ) )


#define ZH_PP_TOKEN_NEEDRIGHT(t) ( ZH_FALSE )



#  define ZH_PP_TOKEN_ISUNARY(t) ( ZH_PP_TOKEN_TYPE(t) == ZH_PP_TOKEN_MINUS || \
                                   ZH_PP_TOKEN_TYPE(t) == ZH_PP_TOKEN_DEC || \
                                   ZH_PP_TOKEN_TYPE(t) == ZH_PP_TOKEN_INC || \
                                   ZH_PP_TOKEN_TYPE(t) == ZH_PP_TOKEN_AMPERSAND || \
                                   ZH_PP_TOKEN_TYPE(t) == ZH_PP_TOKEN_PLUS || \
                                   ZH_PP_TOKEN_TYPE(t) == ZH_PP_TOKEN_NOT || \
                                   ZH_PP_TOKEN_TYPE(t) == ZH_PP_TOKEN_REFERENCE )


#define ZH_PP_TOKEN_ISMATCH(t)   ( (t) && ( (t)->type & ZH_PP_TOKEN_MATCHMARKER ) != 0 )


#define ZH_PP_TOKEN_ISEXPVAL(t)     ( ZH_PP_TOKEN_TYPE(t) == ZH_PP_TOKEN_KEYWORD || \
                                      ZH_PP_TOKEN_TYPE(t) == ZH_PP_TOKEN_MACROVAR || \
                                      ZH_PP_TOKEN_TYPE(t) == ZH_PP_TOKEN_MACROTEXT || \
                                      ZH_PP_TOKEN_TYPE(t) == ZH_PP_TOKEN_STRING || \
                                      ZH_PP_TOKEN_TYPE(t) == ZH_PP_TOKEN_NUMBER || \
                                      ZH_PP_TOKEN_TYPE(t) == ZH_PP_TOKEN_DATE || \
                                      ZH_PP_TOKEN_TYPE(t) == ZH_PP_TOKEN_TIMESTAMP )
#define ZH_PP_TOKEN_ISEXPTOKEN(t)   ( ZH_PP_TOKEN_ISEXPVAL( (t)->type ) || \
                                      ( (t)->pNext && ZH_PP_TOKEN_ISUNARY( (t)->type ) && \
                                        ZH_PP_TOKEN_ISEXPVAL( (t)->pNext->type ) ) )


#define ZH_PP_TOKEN_CANQUOTE(t)     ( ZH_PP_TOKEN_TYPE(t) != ZH_PP_TOKEN_KEYWORD && \
                                      ZH_PP_TOKEN_TYPE(t) != ZH_PP_TOKEN_MACROVAR && \
                                      ZH_PP_TOKEN_TYPE(t) != ZH_PP_TOKEN_MACROTEXT && \
                                      ZH_PP_TOKEN_TYPE(t) != ZH_PP_TOKEN_RIGHT_PB && \
                                      ZH_PP_TOKEN_TYPE(t) != ZH_PP_TOKEN_RIGHT_SB && \
                                      ZH_PP_TOKEN_TYPE(t) != ZH_PP_TOKEN_RIGHT_CB && \
                                      ZH_PP_TOKEN_TYPE(t) != ZH_PP_TOKEN_STRING && \
                                      ZH_PP_TOKEN_TYPE(t) != ZH_PP_TOKEN_NUMBER && \
                                      ZH_PP_TOKEN_TYPE(t) != ZH_PP_TOKEN_DATE && \
                                      ZH_PP_TOKEN_TYPE(t) != ZH_PP_TOKEN_TIMESTAMP && \
                                      ZH_PP_TOKEN_TYPE(t) != ZH_PP_TOKEN_LOGICAL )

typedef struct _ZH_PP_TOKEN
{
   struct _ZH_PP_TOKEN * pNext;     /* next token pointer */
   struct _ZH_PP_TOKEN * pMTokens;  /* restrict or optional marker token(s) */

   const char * value;              /* token value */
   ZH_SIZE   len;                   /* token value length */
   ZH_SIZE   spaces;                /* leading spaces for stringify */
   ZH_USHORT type;                  /* token type, see ZH_PP_TOKEN_* */
   ZH_USHORT index;                 /* index to match marker or 0 */
}
ZH_PP_TOKEN, * PZH_PP_TOKEN;


#ifdef _ZH_PP_INTERNAL

/* default maximum number of translations */
#define ZH_PP_MAX_CYCLES      4096
#define ZH_PP_MAX_REPEATS     128

#define ZH_PP_MAX_INCLUDED_FILES    64

#define ZH_PP_HASHID(t)       ( ( ZH_UCHAR ) ZH_PP_UPPER( (t)->value[ 0 ] ) )
#define ZH_PP_HASHID_MAX      256
#define ZH_PP_DEFINE          1
#define ZH_PP_TRANSLATE       2
#define ZH_PP_COMMAND         4

/* comparison modes */
#define ZH_PP_CMP_ADDR        0 /* compare token addresses */
#define ZH_PP_CMP_STD         1 /* standard comparison, ignore the case of the characters */
#define ZH_PP_CMP_DBASE       2 /* dBase keyword comparison (accepts at least four character shortcuts) ignore the case of the characters */
#define ZH_PP_CMP_CASE        3 /* case sensitive comparison */

#define ZH_PP_CMP_MODE(t)     ( (t) & 0xff )
#define ZH_PP_STD_RULE        0x8000


/* conditional compilation */
#define ZH_PP_COND_ELSE       1     /* preprocessing and output stopped until corresponding #else */
#define ZH_PP_COND_DISABLE    2     /* preprocessing and output stopped until corresponding #endif(s) */

/* operation precedence for #if calculation */
#define ZH_PP_PREC_NUL  0
#define ZH_PP_PREC_LOG  1
#define ZH_PP_PREC_NOT  2
#define ZH_PP_PREC_REL  3
#define ZH_PP_PREC_BIT  4
#define ZH_PP_PREC_PLUS 5
#define ZH_PP_PREC_MULT 6
#define ZH_PP_PREC_NEG  7

/* For platforms which does not use ASCII based character tables this macros
   have to be changed to use valid C functions, e.g.:
      isalpha(), isdigit(), ... */

#  define ZH_PP_ISILLEGAL(c)     ( (c) < 32 || (c) == 127 )

#define ZH_PP_ISTEXTCHAR(c)      ( (unsigned char) (c) >= 128 )
#define ZH_PP_ISBLANK(c)         ( (c) == ' ' || (c) == '\t' )
#define ZH_PP_ISDIGIT(c)         ZH_ISDIGIT( c )
#define ZH_PP_ISHEX(c)           ZH_ISXDIGIT( c )
#define ZH_PP_ISTRUE(c)          ( (c) == 'T' || (c) == 't' || \
                                   (c) == 'Y' || (c) == 'y' )
#define ZH_PP_ISFALSE(c)         ( (c) == 'F' || (c) == 'f' || \
                                   (c) == 'N' || (c) == 'n' )
#define ZH_PP_ISFIRSTIDCHAR(c)   ZH_ISFIRSTIDCHAR( c )
#define ZH_PP_ISNEXTIDCHAR(c)    ZH_ISNEXTIDCHAR( c )
#define ZH_PP_UPPER(c)           ZH_TOUPPER( c )

typedef struct _ZH_PP_RESULT
{
   struct _ZH_PP_RESULT * pNext;
   PZH_PP_TOKEN   pFirstToken;
   PZH_PP_TOKEN   pNextExpr;
}
ZH_PP_RESULT, * PZH_PP_RESULT;

typedef struct _ZH_PP_MARKERPTR
{
   struct _ZH_PP_MARKERPTR * pNext;
   PZH_PP_TOKEN   pToken;
   PZH_PP_TOKEN   pMTokens;
   ZH_USHORT      type;
}
ZH_PP_MARKERPTR, * PZH_PP_MARKERPTR;

typedef struct _ZH_PP_MARKERLST
{
   struct _ZH_PP_MARKERLST * pNext;
   PZH_PP_MARKERPTR  pMatchMarkers;
   ZH_USHORT         canrepeat;
   ZH_USHORT         index;
}
ZH_PP_MARKERLST, * PZH_PP_MARKERLST;

typedef struct
{
   ZH_USHORT canrepeat;
   /* filled when pattern matches for substitution, cleared after */
   ZH_USHORT matches;
   PZH_PP_RESULT  pResult;
}
ZH_PP_MARKER, * PZH_PP_MARKER;

typedef struct _ZH_PP_RULE
{
   struct _ZH_PP_RULE * pPrev;      /* previous rule */
   PZH_PP_TOKEN   pMatch;           /* match pattern or NULL */
   PZH_PP_TOKEN   pResult;          /* result pattern or NULL */
   ZH_USHORT      mode;             /* comparison mode ZH_PP_CMP_* */
   ZH_USHORT      markers;          /* number of markers in marker table */
   /* filled when pattern matches for substitution, cleared after */
   PZH_PP_MARKER  pMarkers;         /* marker table */
   PZH_PP_TOKEN   pNextExpr;        /* next expression after match pattern */
}
ZH_PP_RULE, * PZH_PP_RULE;

typedef struct _ZH_PP_DEFRULE
{
   PZH_PP_TOKEN   pMatch;
   PZH_PP_TOKEN   pResult;
   ZH_USHORT      mode;
   ZH_USHORT      markers;
   ZH_ULONG       repeatbits;
}
ZH_PP_DEFRULE, * PZH_PP_DEFRULE;

typedef struct
{
   const char *   name;       /* input name */
   ZH_SIZE        len;        /* input name length */
   const char *   value;      /* output name */
   ZH_USHORT      type;       /* ZH_PP_TOKEN_* */
}
ZH_PP_OPERATOR, * PZH_PP_OPERATOR;

typedef struct
{
   char *   pBufPtr;
   ZH_SIZE  nLen;
   ZH_SIZE  nAllocated;
}
ZH_MEM_BUFFER, * PZH_MEM_BUFFER;

typedef struct _ZH_PP_FILE
{
   char *   szFileName;            /* input file name */
   FILE *   file_in;               /* input file handle */
   PZH_PP_TOKEN pTokenList;        /* current line decoded to tokens */
   int      iCurrentLine;          /* current line in file */
   int      iLastLine;             /* last non empty generated line */
   int      iLastDisp;             /* last shown line number */
   int      iTokens;               /* number of decoded tokens */
   ZH_BOOL  fGenLineInfo;          /* #line information should be generated */
   ZH_BOOL  fEof;                  /* the end of file reached */

   ZH_BOOL  fFree;                 /* free external buffer */
   const char * pLineBuf;          /* buffer for parsing external lines */
   ZH_SIZE  nLineBufLen;           /* size of external line buffer */

   struct _ZH_PP_FILE * pPrev;     /* previous file, the one which included this file */
}
ZH_PP_FILE, * PZH_PP_FILE;

typedef struct
{
   /* common for all included files */
   PZH_PP_OPERATOR pOperators;      /* user defined operators */
   PZH_PP_RULE    pDefinitions;     /* #define table */
   PZH_PP_RULE    pTranslations;    /* #[x]translate table */
   PZH_PP_RULE    pCommands;        /* #[x]command table */
   int            iOperators;       /* number of user defined operators */
   int            iDefinitions;     /* number of rules in pDefinitions */
   int            iTranslations;    /* number of rules in pTranslations */
   int            iCommands;        /* number of rules in pCommands */
   ZH_BYTE        pMap[ ZH_PP_HASHID_MAX ]; /* translation map */

   PZH_PP_TOKEN   pTokenOut;        /* preprocessed tokens */
   PZH_PP_TOKEN * pNextTokenPtr;    /* pointer to the last NULL pointer in token list */

   PZH_MEM_BUFFER pDumpBuffer;      /* buffer for dump output */
   PZH_MEM_BUFFER pOutputBuffer;    /* buffer for preprocessed line */

   int      iLineTot;               /* total number of parsed lines */
   int      iCycle;                 /* translation counter */
   int      iMaxCycles;             /* maximum number of translations (current) */
   int      iMaxCyclesSet;          /* maximum number of translations (default) */
   ZH_BOOL  fTracePragmas;          /* display information about set pragmas */
   ZH_BOOL  fWritePreprocesed;      /* write preprocessed data to file (.ppo) */
   ZH_BOOL  fWriteTrace;            /* write translation to file (.ppt) */

   ZH_PATHNAMES * pIncludePath;     /* search path(s) for included files */

   char *   szOutFileName;          /* output file name */
   FILE *   file_out;               /* output file handle */
   char *   szTraceFileName;        /* trace output file name */
   FILE *   file_trace;             /* trace output file handle */

   ZH_BOOL   fQuietSet;             /* do not show standard information (default) */
   ZH_BOOL   fQuiet;                /* do not show standard information (current) */
   ZH_BOOL   fGauge;                /* do not show line counter gauge */
   ZH_BOOL   fEscStr;               /* use \ in strings as escape character */
   ZH_BOOL   fMultiLineStr;         /* allow to define multiline [] and e"" strings using ; as line concatenator */
   ZH_BOOL   fError;                /* indicates error in last operation */
   int       iErrors;               /* number of error during preprocessing */
   int       iCondCompile;          /* current conditional compilation flag, when not 0 disable preprocessing and output */
   int       iCondCount;            /* number of nested #if[n]def directive */
   int       iCondStackSize;        /* size of conditional compilation stack */
   int *     pCondStack;            /* conditional compilation stack */

   /* used to divide line per tokens and tokens manipulations */
   PZH_MEM_BUFFER pBuffer;          /* buffer for input and output line */
   ZH_SIZE   nSpaces;               /* leading spaces for next token */
   ZH_SIZE   nSpacesNL;             /* leading spaces ';' token (fCanNextLine) if it will not be line concatenator */
   ZH_SIZE   nSpacesMin;            /* minimal number of leading spaces for next token */
   ZH_USHORT usLastType;            /* last token type */
   ZH_BOOL   fCanNextLine;          /* ';' token found and we do not know yet if it's command separator or line concatenator */
   ZH_BOOL   fDirective;            /* # directives is parsed */
   ZH_BOOL   fNewStatement;         /* set to ZH_TRUE at line beginning or after each ';' token */
   PZH_PP_TOKEN   pFuncOut;         /* function used for each line in ZH_PP_STREAM_* dumping */
   PZH_PP_TOKEN   pFuncEnd;         /* end function for ZH_PP_STREAM_* dumping */
   PZH_MEM_BUFFER pStreamBuffer;    /* buffer for stream output */
   int       iStreamDump;           /* stream output, see ZH_PP_STREAM_* */
   int       iDumpLine;             /* line where current dump output begins */
   int       iInLineCount;          /* number of zh_inLine() functions */
   int       iInLineState;          /* zh_inLine() state */
   int       iInLineBraces;         /* braces counter for zh_inLine() */
   int       iNestedBlock;          /* nested extended block counter */
   int       iBlockState;           /* state of extended block declaration */

   PZH_PP_FILE pFile;               /* currently preprocessed file structure */
   int       iFiles;                /* number of open files */

   void *   cargo;                  /* parameter passed to user functions */
   PZH_PP_OPEN_FUNC   pOpenFunc;    /* function to open files */
   PZH_PP_CLOSE_FUNC  pCloseFunc;   /* function to close files */
   PZH_PP_ERROR_FUNC  pErrorFunc;   /* function to generate errors */
   PZH_PP_DISP_FUNC   pDispFunc;    /* function to redirect stdout messages */
   PZH_PP_DUMP_FUNC   pDumpFunc;    /* function for catching #pragma dump data */
   PZH_PP_INC_FUNC    pIncFunc;     /* function to register included files */
   PZH_PP_INLINE_FUNC pInLineFunc;  /* function for zh_inLine(...) {...} blocks */
   PZH_PP_SWITCH_FUNC pSwitchFunc;  /* function for compiler switches with #pragma ... */
}
ZH_PP_STATE, * PZH_PP_STATE;

extern void zh_pp_initRules( PZH_PP_RULE * pRulesPtr, int * piRules,
                             const ZH_PP_DEFRULE pDefRules[], int iDefRules );

void zh_pp_ruleFree( PZH_PP_RULE pRule );


#else

typedef void * PZH_PP_STATE;

#endif /* _ZH_PP_INTERNAL */

/* public functions */
extern ZH_EXPORT PZH_PP_STATE zh_pp_new( void );
extern ZH_EXPORT void    zh_pp_free( PZH_PP_STATE pState );
extern ZH_EXPORT void    zh_pp_reset( PZH_PP_STATE pState );
extern ZH_EXPORT void    zh_pp_init( PZH_PP_STATE pState, ZH_BOOL fQuiet,
                   ZH_BOOL fGauge, int iCycles, void * cargo,
                   PZH_PP_OPEN_FUNC  pOpenFunc, PZH_PP_CLOSE_FUNC pCloseFunc,
                   PZH_PP_ERROR_FUNC pErrorFunc, PZH_PP_DISP_FUNC  pDispFunc,
                   PZH_PP_DUMP_FUNC  pDumpFunc, PZH_PP_INLINE_FUNC pInLineFunc,
                   PZH_PP_SWITCH_FUNC pSwitchFunc );
extern ZH_EXPORT void    zh_pp_initDynDefines( PZH_PP_STATE pState, ZH_BOOL fArchDefs );
extern ZH_EXPORT void    zh_pp_setIncFunc( PZH_PP_STATE pState, PZH_PP_INC_FUNC pIncFunc );
extern ZH_EXPORT void    zh_pp_readRules( PZH_PP_STATE pState, const char * szRulesFile );
extern ZH_EXPORT void    zh_pp_setStdRules( PZH_PP_STATE pState );
extern ZH_EXPORT void    zh_pp_setStdBase( PZH_PP_STATE pState );
extern ZH_EXPORT void    zh_pp_setStream( PZH_PP_STATE pState, int iMode );
extern ZH_EXPORT void    zh_pp_addSearchPath( PZH_PP_STATE pState, const char * szPath, ZH_BOOL fReplace );
extern ZH_EXPORT ZH_BOOL zh_pp_inBuffer( PZH_PP_STATE pState, const char * szFileName, const char * pBuffer, ZH_SIZE nLen, int iStartLine );
extern ZH_EXPORT ZH_BOOL zh_pp_inFile( PZH_PP_STATE pState, const char * szFileName, ZH_BOOL fSearchPath, FILE * file_in, ZH_BOOL fError );
extern ZH_EXPORT ZH_BOOL zh_pp_outFile( PZH_PP_STATE pState, const char * szOutFileName, FILE * file_out );
extern ZH_EXPORT ZH_BOOL zh_pp_traceFile( PZH_PP_STATE pState, const char * szTraceFileName, FILE * file_trace );
extern ZH_EXPORT char *  zh_pp_fileName( PZH_PP_STATE pState );
extern ZH_EXPORT int     zh_pp_line( PZH_PP_STATE pState );
extern ZH_EXPORT ZH_BOOL zh_pp_eof( PZH_PP_STATE pState );
extern ZH_EXPORT int     zh_pp_lineTot( PZH_PP_STATE pState );
extern ZH_EXPORT char *  zh_pp_outFileName( PZH_PP_STATE pState );
extern ZH_EXPORT char *  zh_pp_traceFileName( PZH_PP_STATE pState );
extern ZH_EXPORT char *  zh_pp_nextLine( PZH_PP_STATE pState, ZH_SIZE * pnLen );
extern ZH_EXPORT char *  zh_pp_parseLine( PZH_PP_STATE pState, const char * pLine, ZH_SIZE * pnLen );
extern ZH_EXPORT void    zh_pp_addDefine( PZH_PP_STATE pState, const char * szDefName, const char * szDefValue );
extern ZH_EXPORT void    zh_pp_delDefine( PZH_PP_STATE pState, const char * szDefName );
extern ZH_EXPORT ZH_BOOL zh_pp_lasterror( PZH_PP_STATE pState );
extern ZH_EXPORT int     zh_pp_errorCount( PZH_PP_STATE pState );

extern ZH_EXPORT void    zh_pp_tokenUpper( PZH_PP_TOKEN pToken );
extern ZH_EXPORT void    zh_pp_tokenToString( PZH_PP_STATE pState, PZH_PP_TOKEN pToken );
extern ZH_EXPORT char *  zh_pp_tokenBlockString( PZH_PP_STATE pState, PZH_PP_TOKEN pToken, int * piType, ZH_SIZE * pnLen );
extern ZH_EXPORT PZH_PP_STATE zh_pp_lexNew( const char * pString, ZH_SIZE nLen );
extern ZH_EXPORT PZH_PP_TOKEN zh_pp_lexGet( PZH_PP_STATE pState );
extern ZH_EXPORT PZH_PP_TOKEN zh_pp_tokenGet( PZH_PP_STATE pState );
extern ZH_EXPORT ZH_BOOL zh_pp_tokenNextExp( PZH_PP_TOKEN * pTokenPtr );

/* PP lib helper functions */
extern PZH_PP_STATE zh_pp_Param( int iParam );


ZH_EXTERN_END

#endif /* ZH_PP_H_ */
