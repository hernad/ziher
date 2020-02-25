/*
 * Ziher compatible preprocessor
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

/* #define ZH_PP_STRICT_LINEINFO_TOKEN */

#define _ZH_PP_INTERNAL

#include "zh_api.h"
#include "zh_pp.h"
#include "zh_date.h"

#define ZH_PP_WARN_EXPLICIT                     1     /* C10?? */
#define ZH_PP_WARN_DEFINE_REDEF                 2     /* C1005 */

#define ZH_PP_ERR_ILLEGAL_CHAR                  1     /* C2004 */
#define ZH_PP_ERR_STRING_TERMINATOR             2     /* C2007 */
#define ZH_PP_ERR_MISSING_ENDTEXT               3     /* C2033 */
#define ZH_PP_ERR_DEFINE_SYNTAX                 4     /* C2055 */
#define ZH_PP_ERR_LABEL_MISSING_IN_DEFINE       5     /* C2057 */
#define ZH_PP_ERR_PARE_MISSING_IN_DEFINE        6     /* C2058 */
#define ZH_PP_ERR_MISSING_PATTERN_SEP           7     /* C2059 */
#define ZH_PP_ERR_UNKNOWN_RESULT_MARKER         8     /* C2060 */
#define ZH_PP_ERR_WRONG_LABEL                   9     /* C2061 */
#define ZH_PP_ERR_BAD_MATCH_MARKER              10    /* C2062 */
#define ZH_PP_ERR_EMPTY_OPTIONAL                11    /* C2065 */
#define ZH_PP_ERR_UNCLOSED_OPTIONAL             12    /* C2066 */
#define ZH_PP_ERR_DIRECTIVE_IFDEF               13    /* C2068 */
#define ZH_PP_ERR_DIRECTIVE_ENDIF               14    /* C2069 */
#define ZH_PP_ERR_DIRECTIVE_ELSE                15    /* C2070 */
#define ZH_PP_ERR_DIRECTIVE_UNDEF               16    /* C2071 */
#define ZH_PP_ERR_AMBIGUOUS_MATCH_PATTERN       17    /* C2072 */
#define ZH_PP_ERR_NESTED_OPTIONAL               18    /* C2073 */
#define ZH_PP_ERR_EXPLICIT                      19    /* C2074 */
#define ZH_PP_ERR_CYCLIC_DEFINE                 20    /* C2078 */
#define ZH_PP_ERR_CYCLIC_TRANSLATE              21    /* C2079 */
#define ZH_PP_ERR_CYCLIC_COMMAND                22    /* C2080 */
#define ZH_PP_ERR_UNTERMINATED_COMMENT          23    /* C2083 */
#define ZH_PP_ERR_PRAGMA                        24    /* C20?? */
#define ZH_PP_ERR_DIRECTIVE_IF                  25    /* C20?? */
#define ZH_PP_ERR_CANNOT_OPEN_INPUT             26    /* C30?? */
#define ZH_PP_ERR_FILE_TOO_LONG                 27    /* C30?? */
#define ZH_PP_ERR_CANNOT_CREATE_FILE            28    /* C3006 */
#define ZH_PP_ERR_CANNOT_OPEN_FILE              29    /* C3007 */
#define ZH_PP_ERR_WRONG_FILE_NAME               30    /* C3008 */
#define ZH_PP_ERR_NESTED_INCLUDES               31    /* C3009 */
#define ZH_PP_ERR_INVALID_DIRECTIVE             32    /* C3010 */
#define ZH_PP_ERR_CANNOT_OPEN_RULES             33    /* C3011 */
#define ZH_PP_ERR_WRITE_FILE                    34    /* C3029 */


/* warning messages */
static const char * const s_pp_szWarnings[] =
{
   "1%s",                                                               /* C10?? */
   "1Redefinition or duplicate definition of #define %s"                /* C1005 */
};

/* error messages */
static const char * const s_pp_szErrors[] =
{
   "Illegal character '\\x%s'",                                         /* C2004 */
   "Unterminated string '%s'",                                          /* C2007 */
   "Missing ENDTEXT",                                                   /* C2033 */
   "Syntax error in #define",                                           /* C2055 */
   "Label missing in #define",                                          /* C2057 */
   "Comma or right parenthesis missing in #define",                     /* C2058 */
   "Missing => in #translate/#command",                                 /* C2059 */
   "Unknown result marker in #translate/#command",                      /* C2060 */
   "Label error in #translate/#command",                                /* C2061 */
   "Bad match marker in #translate/#command",                           /* C2062 */
   "Empty optional clause in #translate/#command",                      /* C2065 */
   "Unclosed optional clause in #translate/#command",                   /* C2066 */
   "Error in #ifdef",                                                   /* C2068 */
   "#endif does not match #ifdef",                                      /* C2069 */
   "#else does not match #ifdef",                                       /* C2070 */
   "Error in #undef",                                                   /* C2071 */
   "Ambiguous match pattern in #translate/#command",                    /* C2072 */
   "Result pattern contains nested clauses in #translate/#command",     /* C2073 */
   "#error '%s'",                                                       /* C2074 */
   "Circularity detected in #define '%s'",                              /* C2078 */
   "Circularity detected in #translate '%s'",                           /* C2079 */
   "Circularity detected in #command '%s'",                             /* C2080 */
   "Unterminated /* */ comment",                                        /* C2083 */

   "Error in #pragma",                                                  /* C20?? */
   "Error in #if expression",                                           /* C20?? */

   "Cannot open input file '%s'",                                       /* C30?? */

   "File %s is too long",                                               /* C30?? */

   "Can't create preprocessed output file",                             /* C3006 */
   "Can't open #include file '%s'",                                     /* C3007 */
   "Bad filename in #include",                                          /* C3008 */
   "Too many nested #includes",                                         /* C3009 */
   "Invalid name follows #",                                            /* C3010 */
   "Can't open standard rule file '%s'",                                /* C3011 */
   "Write error to intermediate file '%s'"                              /* C3029 */
};


static const ZH_PP_OPERATOR s_operators[] =
{
   { ".NOT.", 5, "!"    , ZH_PP_TOKEN_NOT       | ZH_PP_TOKEN_STATIC },
   { ".AND.", 5, ".AND.", ZH_PP_TOKEN_AND       | ZH_PP_TOKEN_STATIC },
   { ".OR." , 4, ".OR." , ZH_PP_TOKEN_OR        | ZH_PP_TOKEN_STATIC },
   { "..."  , 3, "..."  , ZH_PP_TOKEN_EPSILON   | ZH_PP_TOKEN_STATIC },
   { "**="  , 3, "^="   , ZH_PP_TOKEN_EXPEQ     | ZH_PP_TOKEN_STATIC },
   { "**"   , 2, "^"    , ZH_PP_TOKEN_POWER     | ZH_PP_TOKEN_STATIC },
   { "++"   , 2, "++"   , ZH_PP_TOKEN_INC       | ZH_PP_TOKEN_STATIC },
   { "--"   , 2, "--"   , ZH_PP_TOKEN_DEC       | ZH_PP_TOKEN_STATIC },
   { "=="   , 2, "=="   , ZH_PP_TOKEN_EQUAL     | ZH_PP_TOKEN_STATIC },
   { ":="   , 2, ":="   , ZH_PP_TOKEN_ASSIGN    | ZH_PP_TOKEN_STATIC },
   { "+="   , 2, "+="   , ZH_PP_TOKEN_PLUSEQ    | ZH_PP_TOKEN_STATIC },
   { "-="   , 2, "-="   , ZH_PP_TOKEN_MINUSEQ   | ZH_PP_TOKEN_STATIC },
   { "*="   , 2, "*="   , ZH_PP_TOKEN_MULTEQ    | ZH_PP_TOKEN_STATIC },
   { "/="   , 2, "/="   , ZH_PP_TOKEN_DIVEQ     | ZH_PP_TOKEN_STATIC },
   { "%="   , 2, "%="   , ZH_PP_TOKEN_MODEQ     | ZH_PP_TOKEN_STATIC },
   { "^="   , 2, "^="   , ZH_PP_TOKEN_EXPEQ     | ZH_PP_TOKEN_STATIC },
   { "<="   , 2, "<="   , ZH_PP_TOKEN_LE        | ZH_PP_TOKEN_STATIC },
   { ">="   , 2, ">="   , ZH_PP_TOKEN_GE        | ZH_PP_TOKEN_STATIC },
   { "!="   , 2, "<>"   , ZH_PP_TOKEN_NE        | ZH_PP_TOKEN_STATIC },
   { "<>"   , 2, "<>"   , ZH_PP_TOKEN_NE        | ZH_PP_TOKEN_STATIC },
   { "->"   , 2, "->"   , ZH_PP_TOKEN_ALIAS     | ZH_PP_TOKEN_STATIC },
   { "@"    , 1, "@"    , ZH_PP_TOKEN_REFERENCE | ZH_PP_TOKEN_STATIC },
   { "("    , 1, "("    , ZH_PP_TOKEN_LEFT_PB   | ZH_PP_TOKEN_STATIC },
   { ")"    , 1, ")"    , ZH_PP_TOKEN_RIGHT_PB  | ZH_PP_TOKEN_STATIC },
   { "["    , 1, "["    , ZH_PP_TOKEN_LEFT_SB   | ZH_PP_TOKEN_STATIC },
   { "]"    , 1, "]"    , ZH_PP_TOKEN_RIGHT_SB  | ZH_PP_TOKEN_STATIC },
   { "{"    , 1, "{"    , ZH_PP_TOKEN_LEFT_CB   | ZH_PP_TOKEN_STATIC },
   { "}"    , 1, "}"    , ZH_PP_TOKEN_RIGHT_CB  | ZH_PP_TOKEN_STATIC },
   { ","    , 1, ","    , ZH_PP_TOKEN_COMMA     | ZH_PP_TOKEN_STATIC },
   { "\\"   , 1, "\\"   , ZH_PP_TOKEN_BACKSLASH | ZH_PP_TOKEN_STATIC },
   { "|"    , 1, "|"    , ZH_PP_TOKEN_PIPE      | ZH_PP_TOKEN_STATIC },
   { "."    , 1, "."    , ZH_PP_TOKEN_DOT       | ZH_PP_TOKEN_STATIC },
   { "&"    , 1, "&"    , ZH_PP_TOKEN_AMPERSAND | ZH_PP_TOKEN_STATIC },
   { ":"    , 1, ":"    , ZH_PP_TOKEN_SEND      | ZH_PP_TOKEN_STATIC },
   { "!"    , 1, "!"    , ZH_PP_TOKEN_NOT       | ZH_PP_TOKEN_STATIC },
   { "="    , 1, "="    , ZH_PP_TOKEN_EQ        | ZH_PP_TOKEN_STATIC },
   { "<"    , 1, "<"    , ZH_PP_TOKEN_LT        | ZH_PP_TOKEN_STATIC },
   { ">"    , 1, ">"    , ZH_PP_TOKEN_GT        | ZH_PP_TOKEN_STATIC },
   { "#"    , 1, "#"    , ZH_PP_TOKEN_HASH      | ZH_PP_TOKEN_STATIC },
   { "$"    , 1, "$"    , ZH_PP_TOKEN_IN        | ZH_PP_TOKEN_STATIC },
   { "+"    , 1, "+"    , ZH_PP_TOKEN_PLUS      | ZH_PP_TOKEN_STATIC },
   { "-"    , 1, "-"    , ZH_PP_TOKEN_MINUS     | ZH_PP_TOKEN_STATIC },
   { "*"    , 1, "*"    , ZH_PP_TOKEN_MULT      | ZH_PP_TOKEN_STATIC },
   { "/"    , 1, "/"    , ZH_PP_TOKEN_DIV       | ZH_PP_TOKEN_STATIC },
   { "%"    , 1, "%"    , ZH_PP_TOKEN_MOD       | ZH_PP_TOKEN_STATIC },
   { "^"    , 1, "^"    , ZH_PP_TOKEN_POWER     | ZH_PP_TOKEN_STATIC }
/* unused: ? ~ " ' ` */
/* not accessible: " ' `  */
/* illegal: ~ */
};

static const char s_pp_dynamicResult = 0;

static void zh_pp_disp( PZH_PP_STATE pState, const char * szMessage )
{
   if( ! pState->pDispFunc )
   {
      printf( "%s", szMessage );
      fflush( stdout );
   }
   else
      ( pState->pDispFunc )( pState->cargo, szMessage );
}

static void zh_pp_error( PZH_PP_STATE pState, char type, int iError, const char * szParam )
{
   const char * const * szMsgTable = type == 'W' ? s_pp_szWarnings : s_pp_szErrors;

   if( pState->pErrorFunc )
   {
      ( pState->pErrorFunc )( pState->cargo, szMsgTable, type, iError, szParam, NULL );
   }
   else
   {
      char line[ 16 ];
      char msg[ 200 ];
      char buffer[ 256 ];

      if( pState->pFile )
         zh_snprintf( line, sizeof( line ), "(%d) ", pState->pFile->iCurrentLine );
      else
         line[ 0 ] = '\0';
      zh_snprintf( msg, sizeof( msg ), szMsgTable[ iError - 1 ], szParam );
      zh_snprintf( buffer, sizeof( buffer ), "%s%s: %s\n", line,
                type == 'F' ? "Fatal" : type == 'W' ? "Warning" : "Error", msg );
      zh_pp_disp( pState, buffer );
   }
   if( type != 'W' )
   {
      pState->fError = ZH_TRUE;
      pState->iErrors++;
   }
}

static void zh_pp_operatorsFree( PZH_PP_OPERATOR pOperators, int iOperators )
{
   PZH_PP_OPERATOR pOperator = pOperators;

   while( --iOperators >= 0 )
   {
      zh_xfree( ZH_UNCONST( pOperator->name ) );
      zh_xfree( ZH_UNCONST( pOperator->value ) );
      ++pOperator;
   }
   zh_xfree( pOperators );
}

static const ZH_PP_OPERATOR * zh_pp_operatorFind( PZH_PP_STATE pState,
                                                  char * buffer, ZH_SIZE nLen )
{
   const ZH_PP_OPERATOR * pOperator = pState->pOperators;
   int i = pState->iOperators;

   while( --i >= 0 )
   {
      if( pOperator->len <= nLen && pOperator->name[ 0 ] == buffer[ 0 ] &&
          ( pOperator->len == 1 ||
            zh_strnicmp( pOperator->name + 1, buffer + 1, pOperator->len - 1 ) == 0 ) )
         return pOperator;

      ++pOperator;
   }

   pOperator = s_operators;
   i = ZH_SIZEOFARRAY( s_operators );

   do
   {
      if( pOperator->len <= nLen && pOperator->name[ 0 ] == buffer[ 0 ] &&
          ( pOperator->len == 1 ||
            ( pOperator->len >= 4 ?
              zh_strnicmp( pOperator->name + 1, buffer + 1, pOperator->len - 1 ) == 0 :
              ( pOperator->name[ 1 ] == buffer[ 1 ] &&
                ( pOperator->len == 2 || pOperator->name[ 2 ] == buffer[ 2 ] ) ) ) ) )
         return pOperator;

      ++pOperator;
   }
   while( --i > 0 );

   return NULL;
}

#define ZH_MEMBUF_DEFAULT_SIZE      256

static PZH_MEM_BUFFER zh_membufNew( void )
{
   PZH_MEM_BUFFER pBuffer = ( PZH_MEM_BUFFER ) zh_xgrab( sizeof( ZH_MEM_BUFFER ) );

   pBuffer->nLen = 0;
   pBuffer->nAllocated = ZH_MEMBUF_DEFAULT_SIZE;
   pBuffer->pBufPtr = ( char * ) zh_xgrab( pBuffer->nAllocated );

   return pBuffer;
}

static void zh_membufFree( PZH_MEM_BUFFER pBuffer )
{
   zh_xfree( pBuffer->pBufPtr );
   zh_xfree( pBuffer );
}

static void zh_membufFlush( PZH_MEM_BUFFER pBuffer )
{
   pBuffer->nLen = 0;
}

static void zh_membufRemove( PZH_MEM_BUFFER pBuffer, ZH_SIZE nLeft )
{
   if( nLeft < pBuffer->nLen )
      pBuffer->nLen = nLeft;
}

static ZH_SIZE zh_membufLen( const PZH_MEM_BUFFER pBuffer )
{
   return pBuffer->nLen;
}

static char * zh_membufPtr( const PZH_MEM_BUFFER pBuffer )
{
   return pBuffer->pBufPtr;
}

static void zh_membufAddCh( PZH_MEM_BUFFER pBuffer, char ch )
{
   if( pBuffer->nLen == pBuffer->nAllocated )
   {
      pBuffer->nAllocated <<= 1;
      pBuffer->pBufPtr = ( char * ) zh_xrealloc( pBuffer->pBufPtr, pBuffer->nAllocated );
   }
   pBuffer->pBufPtr[ pBuffer->nLen++ ] = ch;
}

static void zh_membufAddData( PZH_MEM_BUFFER pBuffer, const char * data, ZH_SIZE nLen )
{
   if( pBuffer->nLen + nLen > pBuffer->nAllocated )
   {
      do
      {
         pBuffer->nAllocated <<= 1;
      }
      while( pBuffer->nLen + nLen > pBuffer->nAllocated );
      pBuffer->pBufPtr = ( char * ) zh_xrealloc( pBuffer->pBufPtr, pBuffer->nAllocated );
   }

   memcpy( &pBuffer->pBufPtr[ pBuffer->nLen ], data, nLen );
   pBuffer->nLen += nLen;
}

static void zh_membufAddStr( PZH_MEM_BUFFER pBuffer, const char * szText )
{
   zh_membufAddData( pBuffer, szText, strlen( szText ) );
}

static void zh_pp_tokenFree( PZH_PP_TOKEN pToken )
{
   if( ZH_PP_TOKEN_ALLOC( pToken->type ) )
      zh_xfree( ZH_UNCONST( pToken->value ) );
   if( ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_MMARKER_RESTRICT ||
       ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_MMARKER_OPTIONAL ||
       ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_RMARKER_OPTIONAL )
   {
      while( pToken->pMTokens )
      {
         PZH_PP_TOKEN pMTokens = pToken->pMTokens;
         pToken->pMTokens = pMTokens->pNext;
         zh_pp_tokenFree( pMTokens );
      }
   }
   zh_xfree( pToken );
}

static void zh_pp_tokenListFree( PZH_PP_TOKEN * pTokenPtr )
{
   if( *pTokenPtr && ! ZH_PP_TOKEN_ISPREDEF( *pTokenPtr ) )
   {
      do
      {
         PZH_PP_TOKEN pToken = *pTokenPtr;
         *pTokenPtr = pToken->pNext;
         zh_pp_tokenFree( pToken );
      }
      while( *pTokenPtr );
   }
}

static int zh_pp_tokenListFreeCmd( PZH_PP_TOKEN * pTokenPtr )
{
   ZH_BOOL fStop = ZH_FALSE;
   int iLines = 0;

   while( *pTokenPtr && ! fStop )
   {
      PZH_PP_TOKEN pToken = *pTokenPtr;
      *pTokenPtr = pToken->pNext;
      if( ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_EOL )
         ++iLines;
      fStop = ZH_PP_TOKEN_ISEOC( pToken );
      zh_pp_tokenFree( pToken );
   }
   return *pTokenPtr ? iLines : 0;
}

static void zh_pp_tokenMoveCommand( PZH_PP_STATE pState,
                                    PZH_PP_TOKEN * pDestPtr,
                                    PZH_PP_TOKEN * pSrcPtr )
{
   PZH_PP_TOKEN pToken;
   int iLines = 0;

   while( *pSrcPtr )
   {
      pToken = *pSrcPtr;
      *pSrcPtr = pToken->pNext;
      *pDestPtr = pToken;
      pDestPtr = &pToken->pNext;
      if( ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_EOL )
         ++iLines;
      if( ZH_PP_TOKEN_ISEOC( pToken ) )
         break;
   }
   *pDestPtr = NULL;

   if( iLines )
   {
      pState->pFile->iLastLine = pState->pFile->iCurrentLine + iLines;
      if( *pSrcPtr )
         pState->pFile->iCurrentLine += iLines;
   }
}

static PZH_PP_TOKEN zh_pp_tokenResultEnd( PZH_PP_TOKEN * pTokenPtr, ZH_BOOL fDirect )
{
   PZH_PP_TOKEN pNext = NULL;


   while( *pTokenPtr )
   {
      if( ZH_PP_TOKEN_ISEOP( *pTokenPtr, fDirect ) )
      {
         pNext = *pTokenPtr;
         *pTokenPtr = NULL;
         break;
      }
      pTokenPtr = &( *pTokenPtr )->pNext;
   }

   return pNext;
}

static PZH_PP_TOKEN zh_pp_tokenNew( const char * value, ZH_SIZE nLen,
                                    ZH_SIZE nSpaces, ZH_USHORT type )
{
   PZH_PP_TOKEN pToken = ( PZH_PP_TOKEN ) zh_xgrab( sizeof( ZH_PP_TOKEN ) );

   if( ZH_PP_TOKEN_ALLOC( type ) )
   {
      if( nLen <= 1 )
      {
         pToken->value = zh_szAscii[ nLen ? ( ZH_UCHAR ) value[ 0 ] : 0 ];
         type |= ZH_PP_TOKEN_STATIC;
      }
      else
      {
         char * val = ( char * ) memcpy( zh_xgrab( nLen + 1 ), value, nLen );
         val[ nLen ] = '\0';
         pToken->value = val;
      }
   }
   else
      pToken->value = value;

   pToken->len    = nLen;
   pToken->spaces = nSpaces;
   pToken->type   = type;
   pToken->index  = 0;
   pToken->pNext  = NULL;
   pToken->pMTokens = NULL;

   return pToken;
}

static void zh_pp_tokenSetValue( PZH_PP_TOKEN pToken,
                                 const char * value, ZH_SIZE nLen )
{
   if( ZH_PP_TOKEN_ALLOC( pToken->type ) )
      zh_xfree( ZH_UNCONST( pToken->value ) );
   if( nLen <= 1 )
   {
      pToken->value = zh_szAscii[ nLen ? ( ZH_UCHAR ) value[ 0 ] : 0 ];
      pToken->type |= ZH_PP_TOKEN_STATIC;
   }
   else
   {
      char * val = ( char * ) memcpy( zh_xgrab( nLen + 1 ), value, nLen );
      val[ nLen ] = '\0';
      pToken->value = val;
      pToken->type &= ~ZH_PP_TOKEN_STATIC;
   }
   pToken->len = nLen;
}

static PZH_PP_TOKEN zh_pp_tokenClone( PZH_PP_TOKEN pSource )
{
   PZH_PP_TOKEN pDest = ( PZH_PP_TOKEN ) zh_xgrab( sizeof( ZH_PP_TOKEN ) );

   memcpy( pDest, pSource, sizeof( ZH_PP_TOKEN ) );
   if( ZH_PP_TOKEN_ALLOC( pDest->type ) )
   {
      char * val = ( char * ) memcpy( zh_xgrab( pDest->len + 1 ),
                                      pSource->value, pDest->len );
      val[ pDest->len ] = '\0';
      pDest->value = val;
   }
   pDest->pNext  = NULL;

   return pDest;
}

static void zh_pp_tokenAdd( PZH_PP_TOKEN ** pTokenPtr,
                            const char * value, ZH_SIZE nLen,
                            ZH_SIZE nSpaces, ZH_USHORT type )
{
   PZH_PP_TOKEN pToken = zh_pp_tokenNew( value, nLen, nSpaces, type );

   **pTokenPtr = pToken;
   *pTokenPtr  = &pToken->pNext;
}

static void zh_pp_tokenAddCmdSep( PZH_PP_STATE pState )
{
   zh_pp_tokenAdd( &pState->pNextTokenPtr, ";", 1, pState->nSpacesNL, ZH_PP_TOKEN_EOC | ZH_PP_TOKEN_STATIC );
   pState->pFile->iTokens++;
   pState->fNewStatement = ZH_TRUE;
   pState->fCanNextLine = ZH_FALSE;
   if( pState->iBlockState )
   {
      if( pState->iBlockState == 5 )
         pState->iNestedBlock++;
      pState->iBlockState = 0;
   }
}

static void zh_pp_tokenAddNext( PZH_PP_STATE pState, const char * value, ZH_SIZE nLen,
                                ZH_USHORT type )
{
   if( pState->fCanNextLine )
      zh_pp_tokenAddCmdSep( pState );

   if( ! pState->fDirective )
   {
      if( pState->iNestedBlock && pState->fNewStatement &&
          ZH_PP_TOKEN_TYPE( type ) == ZH_PP_TOKEN_RIGHT_CB )
      {
         pState->iBlockState = 0;
         pState->iNestedBlock--;
      }
      else if( pState->usLastType == ZH_PP_TOKEN_LEFT_CB &&
               ZH_PP_TOKEN_TYPE( type ) == ZH_PP_TOKEN_PIPE )
      {
         pState->iBlockState = 1;
      }
      else if( pState->iBlockState )
      {
         if( ( pState->iBlockState == 1 || pState->iBlockState == 2 ||
               pState->iBlockState == 4 ) &&
             ZH_PP_TOKEN_TYPE( type ) == ZH_PP_TOKEN_PIPE )
            pState->iBlockState = 5;
         else if( pState->iBlockState == 1 &&
                  ZH_PP_TOKEN_TYPE( type ) == ZH_PP_TOKEN_KEYWORD )
            pState->iBlockState = 2;
         else if( pState->iBlockState == 1 &&
                  ZH_PP_TOKEN_TYPE( type ) == ZH_PP_TOKEN_EPSILON )
            pState->iBlockState = 4;
         else if( pState->iBlockState == 2 &&
                  ZH_PP_TOKEN_TYPE( type ) == ZH_PP_TOKEN_COMMA )
            pState->iBlockState = 1;
         else
            pState->iBlockState = 0;
      }

      if( pState->fNewStatement && nLen == 1 && *value == '#' )
      {
         pState->fDirective = ZH_TRUE;
         value = "#";
         type = ZH_PP_TOKEN_DIRECTIVE | ZH_PP_TOKEN_STATIC;
      }
   }

   if( pState->nSpacesMin != 0 && pState->nSpaces == 0 &&
       ZH_PP_TOKEN_TYPE( type ) == ZH_PP_TOKEN_KEYWORD )
      pState->nSpaces = pState->nSpacesMin;
   zh_pp_tokenAdd( &pState->pNextTokenPtr, value, nLen, pState->nSpaces, type );
   pState->pFile->iTokens++;
   pState->fNewStatement = ZH_FALSE;

   pState->nSpaces = pState->nSpacesMin = 0;
   pState->usLastType = ZH_PP_TOKEN_TYPE( type );

   if( pState->iInLineState != ZH_PP_INLINE_OFF )
   {
      if( pState->iInLineState == ZH_PP_INLINE_START &&
          pState->usLastType == ZH_PP_TOKEN_LEFT_PB )
      {
         pState->iInLineState = ZH_PP_INLINE_PARAM;
         pState->iInLineBraces = 1;
      }
      else if( pState->iInLineState == ZH_PP_INLINE_PARAM )
      {
         if( pState->usLastType == ZH_PP_TOKEN_LEFT_PB )
            pState->iInLineBraces++;
         else if( pState->usLastType == ZH_PP_TOKEN_RIGHT_PB )
         {
            if( --pState->iInLineBraces == 0 )
               pState->iInLineState = ZH_PP_INLINE_BODY;
         }
      }
      else
         pState->iInLineState = ZH_PP_INLINE_OFF;
   }
}

static void zh_pp_tokenAddStreamFunc( PZH_PP_STATE pState, PZH_PP_TOKEN pToken,
                                      const char * value, ZH_SIZE nLen )
{
   while( pToken )
   {
      if( ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_RMARKER_STRDUMP )
      {
         if( value )
         {
            zh_pp_tokenAdd( &pState->pNextTokenPtr, value, nLen, pToken->spaces, ZH_PP_TOKEN_STRING );
            pState->pFile->iTokens++;
         }
      }
      else
      {
         *pState->pNextTokenPtr = zh_pp_tokenClone( pToken );
         pState->pNextTokenPtr  = &( *pState->pNextTokenPtr )->pNext;
         pState->pFile->iTokens++;
      }
      pToken = pToken->pNext;
   }
   pState->fNewStatement = ZH_TRUE;
}

static void zh_pp_readLine( PZH_PP_STATE pState )
{
   int ch, iLine = 0, iBOM = pState->pFile->iCurrentLine == 0 ? 1 : 0;

   for( ;; )
   {
      if( pState->pFile->pLineBuf )
      {
         if( pState->pFile->nLineBufLen )
         {
            ch = ( ZH_UCHAR ) pState->pFile->pLineBuf[ 0 ];
            pState->pFile->pLineBuf++;
            pState->pFile->nLineBufLen--;
         }
         else
            break;
      }
      else
      {
         ch = fgetc( pState->pFile->file_in );
         if( ch == EOF )
         {
            pState->pFile->fEof = ZH_TRUE;
            break;
         }
      }
      iLine = 1;
      /* ^Z works like \n */
      if( ch == '\n' || ch == '\x1a' )
      {
         break;
      }
      /* strips \r characters even from quoted strings */
      else if( ch != '\r' )
      {
         zh_membufAddCh( pState->pBuffer, ( char ) ch );

         /* strip UTF-8 BOM signature */
         if( iBOM && ch == 0xBF && zh_membufLen( pState->pBuffer ) == 3 )
         {
            iBOM = 0;
            if( zh_membufPtr( pState->pBuffer )[ 0 ] == '\xEF' &&
                zh_membufPtr( pState->pBuffer )[ 1 ] == '\xBB' )
               zh_membufFlush( pState->pBuffer );
         }
      }
   }
   pState->iLineTot += iLine;
   iLine = ++pState->pFile->iCurrentLine / 100;
   if( ! pState->fQuiet && pState->fGauge &&
       iLine != pState->pFile->iLastDisp )
   {
      char szLine[ 12 ];

      pState->pFile->iLastDisp = iLine;
      zh_snprintf( szLine, sizeof( szLine ), "\r%i00\r", iLine );
      zh_pp_disp( pState, szLine );
   }
}

static ZH_BOOL zh_pp_canQuote( ZH_BOOL fQuote, char * pBuffer, ZH_SIZE nLen,
                               ZH_SIZE n, ZH_SIZE * pnAt )
{
   char cQuote = 0;

   while( n < nLen )
   {
      if( pBuffer[ n ] == ']' )
      {
         if( cQuote && ! fQuote )
         {
            ZH_SIZE u = n + 1;
            cQuote = 0;
            while( u < nLen )
            {
               if( cQuote )
               {
                  if( pBuffer[ u ] == cQuote )
                     cQuote = 0;
               }
               else if( pBuffer[ u ] == '`' )
                  cQuote = '\'';
               else if( pBuffer[ u ] == '\'' || pBuffer[ u ] == '"' )
                  cQuote = pBuffer[ u ];
               else if( pBuffer[ u ] == '[' )
                  zh_pp_canQuote( ZH_TRUE, pBuffer, nLen, u + 1, &u );
               ++u;
            }
            fQuote = cQuote == 0;
         }
         if( fQuote )
            *pnAt = n;
         return fQuote;
      }
      else if( ! fQuote )
      {
         if( cQuote )
         {
            if( pBuffer[ n ] == cQuote )
               cQuote = 0;
         }
         else if( pBuffer[ n ] == '`' )
            cQuote = '\'';
         else if( pBuffer[ n ] == '\'' || pBuffer[ n ] == '"' )
            cQuote = pBuffer[ n ];
         else if( ZH_PP_ISILLEGAL( pBuffer[ n ] ) )
            fQuote = ZH_TRUE;
      }
      ++n;
   }
   return ZH_FALSE;
}

static ZH_BOOL zh_pp_hasCommand( char * pBuffer, ZH_SIZE nLen, ZH_SIZE * pnAt, int iCmds, ... )
{
   ZH_SIZE n = 0;
   va_list va;
   int i;

   va_start( va, iCmds );
   for( i = 0; i < iCmds && n < nLen; ++i )
   {
      ZH_SIZE nl;
      char * cmd = va_arg( va, char * );
      nl = strlen( cmd );
      while( n < nLen && ZH_PP_ISBLANK( pBuffer[ n ] ) )
         ++n;
      if( n + nl > nLen || zh_strnicmp( cmd, pBuffer + n, nl ) != 0 )
         break;
      n += nl;
      if( n < nLen && ( ZH_PP_ISNEXTIDCHAR( cmd[ nl - 1 ] ) ||
                        ZH_PP_ISTEXTCHAR( cmd[ nl - 1 ] ) ) &&
                      ( ZH_PP_ISNEXTIDCHAR( pBuffer[ n ] ) ||
                        ZH_PP_ISTEXTCHAR( pBuffer[ n ] ) ) )
         break;
   }
   va_end( va );

   if( i == iCmds )
   {
      while( n < nLen && ZH_PP_ISBLANK( pBuffer[ n ] ) )
         ++n;

      if( n + 1 < nLen &&
          ( pBuffer[ n ] == '/' || pBuffer[ n ] == '&' ) &&
          pBuffer[ n ] == pBuffer[ n + 1 ] )
         /* strip the rest of line with // or && comment */
         n = nLen;

      if( n == nLen || pBuffer[ n ] == ';' ||
          ( n + 1 < nLen && pBuffer[ n ] == '/' && pBuffer[ n + 1 ] == '*' ) )
      {
         *pnAt = n;
         return ZH_TRUE;
      }
   }
   return ZH_FALSE;
}

static void zh_pp_dumpEnd( PZH_PP_STATE pState )
{
   pState->iStreamDump = ZH_PP_STREAM_OFF;
   if( pState->iCondCompile )
   {
      zh_membufFlush( pState->pDumpBuffer );
   }
   else if( pState->pDumpFunc )
   {
      ( pState->pDumpFunc )( pState->cargo,
                             zh_membufPtr( pState->pDumpBuffer ),
                             zh_membufLen( pState->pDumpBuffer ),
                             pState->iDumpLine + 1 );

      /* I do not like it - dump data should be separated from
         preprocessed .zh code. What is inside DUMP area and
         how it will be interpreted depends on backend not on
         PP itself */
      if( pState->fWritePreprocesed )
      {
         int iLines = 0;
         char * pBuffer;
         ZH_SIZE nLen;

         if( pState->pFile->fGenLineInfo )
         {
            fprintf( pState->file_out, "#line %d", pState->iDumpLine );
            if( pState->pFile->szFileName )
            {
               fprintf( pState->file_out, " \"%s\"", pState->pFile->szFileName );
            }
            fputc( '\n', pState->file_out );
            pState->pFile->fGenLineInfo = ZH_FALSE;
         }
         else if( pState->pFile->iLastLine < pState->iDumpLine )
         {
            do
            {
               fputc( '\n', pState->file_out );
            }
            while( ++pState->pFile->iLastLine < pState->iDumpLine );
         }
         pBuffer = zh_membufPtr( pState->pDumpBuffer );
         nLen = zh_membufLen( pState->pDumpBuffer );
         fputs( "#pragma BEGINDUMP\n", pState->file_out );
         if( fwrite( pBuffer, sizeof( char ), nLen, pState->file_out ) != nLen )
            zh_pp_error( pState, 'F', ZH_PP_ERR_WRITE_FILE, pState->szOutFileName );
         fputs( "#pragma ENDDUMP\n", pState->file_out );

         while( nLen-- )
         {
            if( *pBuffer++ == '\n' )
               ++iLines;
         }
         pState->pFile->iLastLine = pState->iDumpLine + iLines + 2;
      }
      zh_membufFlush( pState->pDumpBuffer );
   }
}

static void zh_pp_getLine( PZH_PP_STATE pState )
{
   PZH_PP_TOKEN * pInLinePtr, * pEolTokenPtr;
   char * pBuffer;
   ZH_BOOL fDump = ZH_FALSE;
   int iLines = 0, iStartLine;

   pInLinePtr = pEolTokenPtr = NULL;
   zh_pp_tokenListFree( &pState->pFile->pTokenList );
   pState->pNextTokenPtr = &pState->pFile->pTokenList;
   pState->pFile->iTokens = 0;
   pState->nSpaces = pState->nSpacesMin = 0;
   pState->fCanNextLine = pState->fDirective = ZH_FALSE;
   pState->fNewStatement = ZH_TRUE;
   pState->usLastType = ZH_PP_TOKEN_NUL;
   pState->iInLineState = ZH_PP_INLINE_OFF;
   pState->iInLineBraces = 0;
   pState->iBlockState = pState->iNestedBlock = 0;
   iStartLine = pState->pFile->iCurrentLine + 1;

   do
   {
      ZH_SIZE nLen, n;

      zh_membufFlush( pState->pBuffer );
      zh_pp_readLine( pState );
      pBuffer = zh_membufPtr( pState->pBuffer );
      nLen = zh_membufLen( pState->pBuffer );
      if( pState->fCanNextLine )
      {
         pState->nSpaces = pState->nSpacesNL;

         pState->nSpacesMin = 1;
         pState->fCanNextLine = ZH_FALSE;

         if( nLen > 1 && ZH_PP_ISBLANK( pBuffer[ 0 ] ) )
         {
            while( nLen > 1 && ZH_PP_ISBLANK( pBuffer[ 1 ] ) )
            {
               --nLen;
               ++pBuffer;
            }
         }
      }
      else if( pState->iStreamDump && nLen == 0 )
      {
         pBuffer[ 0 ] = '\0';
         fDump = ZH_TRUE;
      }
      n = 0;
      while( n < nLen || fDump )
      {
         char ch = pBuffer[ 0 ];
         if( pState->iStreamDump )
         {
            fDump = ZH_FALSE;
            if( pState->iStreamDump == ZH_PP_STREAM_COMMENT )
            {
               if( nLen > 0 )
               {
                  ++n;
                  if( nLen > 1 && ch == '*' && pBuffer[ 1 ] == '/' )
                  {
                     pState->iStreamDump = ZH_PP_STREAM_OFF;

                     pState->nSpaces = 0;
                  
                     pState->nSpacesMin = 1;
                     ++n;
                  }
               }
            }
            else if( pState->iStreamDump == ZH_PP_STREAM_INLINE_C )
            {
               if( nLen > 0 )
               {
                  ++n;
                  switch( pState->iInLineState )
                  {
                     case ZH_PP_INLINE_QUOTE1:
                        if( ch == '\'' )
                           pState->iInLineState = ZH_PP_INLINE_OFF;
                        else if( ch == '\\' && nLen > 1 )
                           ++n;
                        break;

                     case ZH_PP_INLINE_QUOTE2:
                        if( ch == '"' )
                           pState->iInLineState = ZH_PP_INLINE_OFF;
                        else if( ch == '\\' && nLen > 1 )
                           ++n;
                        break;

                     case ZH_PP_INLINE_COMMENT:
                        if( nLen > 1 && ch == '*' && pBuffer[ 1 ] == '/' )
                        {
                           pState->iInLineState = ZH_PP_INLINE_OFF;
                           ++n;
                        }
                        break;

                     default:
                        if( ch == '\'' )
                           pState->iInLineState = ZH_PP_INLINE_QUOTE1;
                        else if( ch == '"' )
                           pState->iInLineState = ZH_PP_INLINE_QUOTE2;
                        else if( ch == '{' )
                           ++pState->iInLineBraces;
                        else if( ch == '}' )
                        {
                           if( --pState->iInLineBraces == 0 )
                              pState->iStreamDump = ZH_PP_STREAM_OFF;
                        }
                        else if( nLen > 1 )
                        {
                           if( ch == '/' && pBuffer[ 1 ] == '*' )
                           {
                              pState->iInLineState = ZH_PP_INLINE_COMMENT;
                              ++n;
                           }
                           else if( ch == '/' && pBuffer[ 1 ] == '/' )
                              nLen = n = 0;
                        }
                  }
               }
               if( n )
                  zh_membufAddData( pState->pStreamBuffer, pBuffer, n );

               if( nLen == n || pState->iStreamDump == ZH_PP_STREAM_OFF )
               {
                  zh_membufAddCh( pState->pStreamBuffer, '\n' );
                  if( pState->iStreamDump == ZH_PP_STREAM_OFF )
                  {
                     if( pState->iCondCompile )
                     {
                        ;
                     }
                     else if( pState->pInLineFunc )
                     {
                        char szFunc[ 24 ];
                        zh_snprintf( szFunc, sizeof( szFunc ), "ZH_INLINE_%03d", ++pState->iInLineCount );
                        if( pInLinePtr && *pInLinePtr )
                           zh_pp_tokenSetValue( *pInLinePtr, szFunc, strlen( szFunc ) );
                        pState->pInLineFunc( pState->cargo, szFunc,
                                    zh_membufPtr( pState->pStreamBuffer ),
                                    zh_membufLen( pState->pStreamBuffer ),
                                    pState->iDumpLine );
                     }
                     else
                     {
                        zh_pp_tokenAddNext( pState,
                                   zh_membufPtr( pState->pStreamBuffer ),
                                   zh_membufLen( pState->pStreamBuffer ),
                                   ZH_PP_TOKEN_TEXT );
                     }
                     zh_membufFlush( pState->pStreamBuffer );
                  }
               }
            }
            else if( pState->iStreamDump == ZH_PP_STREAM_DUMP_C )
            {
               if( zh_pp_hasCommand( pBuffer, nLen, &n, 3, "#", "pragma", "enddump" ) )
               {
                  zh_pp_dumpEnd( pState );
               }
               else
               {
                  n = nLen;
                  zh_membufAddData( pState->pDumpBuffer, pBuffer, n );
                  zh_membufAddCh( pState->pDumpBuffer, '\n' );
               }
            }
            else if( zh_pp_hasCommand( pBuffer, nLen, &n, 1, "ENDTEXT" ) ||
                     zh_pp_hasCommand( pBuffer, nLen, &n, 3, "#", "pragma", "__endtext" ) )
            {
               if( pState->iStreamDump == ZH_PP_STREAM_TEXT )
               {
                  if( pState->pFuncEnd )
                     zh_pp_tokenAddStreamFunc( pState, pState->pFuncEnd, NULL, 0 );
               }
               else
               {
                  /* ZH_PP_STREAM_PRG, ZH_PP_STREAM_C */
                  zh_pp_tokenAddStreamFunc( pState, pState->pFuncOut,
                                            zh_membufPtr( pState->pStreamBuffer ),
                                            zh_membufLen( pState->pStreamBuffer ) );
                  if( pState->pFuncEnd )
                  {
                     if( pState->pFuncOut )
                        zh_pp_tokenAddCmdSep( pState );
                     zh_pp_tokenAddStreamFunc( pState, pState->pFuncEnd,
                                               zh_membufPtr( pState->pStreamBuffer ),
                                               zh_membufLen( pState->pStreamBuffer ) );
                  }
                  zh_membufFlush( pState->pStreamBuffer );
               }
               zh_pp_tokenListFree( &pState->pFuncOut );
               zh_pp_tokenListFree( &pState->pFuncEnd );
               pState->iStreamDump = ZH_PP_STREAM_OFF;
            }
            else if( pState->iStreamDump == ZH_PP_STREAM_TEXT )
            {
               n = nLen;
               zh_pp_tokenAddStreamFunc( pState, pState->pFuncOut, pBuffer, n );
            }
            else /* ZH_PP_STREAM_PRG, ZH_PP_STREAM_C */
            {
               n = nLen;
               if( pState->iStreamDump == ZH_PP_STREAM_C )
                  zh_strRemEscSeq( pBuffer, &n );
               zh_membufAddData( pState->pStreamBuffer, pBuffer, n );
               zh_membufAddCh( pState->pStreamBuffer, '\n' );
               n = nLen; /* zh_strRemEscSeq() above could change n */
            }
         }
         else if( ( ( ch == 'e' || ch == 'E' ) && nLen > 1 &&
                    pBuffer[ 1 ] == '"' ) || ( ch == '"' && pState->fEscStr ) )
         {
            ZH_SIZE nStrip, u;

            if( ch != '"' )
               ++n;
            while( ++n < nLen && pBuffer[ n ] != '"' )
            {
               if( pBuffer[ n ] == '\\' )
               {
                  if( ++n == nLen )
                     break;
               }
            }
            if( pState->fMultiLineStr )
            {
               while( n == nLen )
               {
                  u = 1;
                  while( n > u && pBuffer[ n - u ] == ' ' )
                     ++u;
                  if( n >= u && pBuffer[ n - u ] == ';' )
                  {
                     n -= u;
                     nLen -= u;
                     u = zh_membufLen( pState->pBuffer ) - u;
                     zh_membufRemove( pState->pBuffer, u );
                     zh_pp_readLine( pState );
                     nLen += zh_membufLen( pState->pBuffer ) - u;
                     pBuffer = zh_membufPtr( pState->pBuffer ) + u - n;
                     --n;
                     while( ++n < nLen && pBuffer[ n ] != '"' )
                     {
                        if( pBuffer[ n ] == '\\' )
                        {
                           if( ++n == nLen )
                              break;
                        }
                     }
                  }
                  else
                     break;
               }
            }
            u = ch != '"' ? 2 : 1;
            nStrip = n - u;
            zh_strRemEscSeq( pBuffer + u, &nStrip );
            zh_pp_tokenAddNext( pState, pBuffer + u, nStrip,
                                ZH_PP_TOKEN_STRING );
            if( n == nLen )
            {
               ZH_SIZE nSkip = pBuffer - zh_membufPtr( pState->pBuffer );
               zh_membufAddCh( pState->pBuffer, '\0' );
               pBuffer = zh_membufPtr( pState->pBuffer ) + nSkip;
               zh_pp_error( pState, 'E', ZH_PP_ERR_STRING_TERMINATOR, pBuffer + u - 1 );
            }
            else
               ++n;
         }
         else if( ( ch == 't' || ch == 'T' ) && nLen > 1 && pBuffer[ 1 ] == '"' )
         {
            ++n;
            while( ++n < nLen && pBuffer[ n ] != '"' )
               ;
            zh_pp_tokenAddNext( pState, pBuffer + 2, n - 2,
                                ZH_PP_TOKEN_TIMESTAMP );
            if( n == nLen )
            {
               ZH_SIZE nSkip = pBuffer - zh_membufPtr( pState->pBuffer ) + 1;
               zh_membufAddCh( pState->pBuffer, '\0' );
               pBuffer = zh_membufPtr( pState->pBuffer ) + nSkip;
               zh_pp_error( pState, 'E', ZH_PP_ERR_STRING_TERMINATOR, pBuffer );
            }
            else
               ++n;
         }
         else if( ( ch == 'd' || ch == 'D' ) && nLen > 1 && pBuffer[ 1 ] == '"' )
         {
            ++n;
            while( ++n < nLen && pBuffer[ n ] != '"' )
               ;
            zh_pp_tokenAddNext( pState, pBuffer + 2, n - 2,
                                ZH_PP_TOKEN_DATE );
            if( n == nLen )
            {
               ZH_SIZE nSkip = pBuffer - zh_membufPtr( pState->pBuffer ) + 1;
               zh_membufAddCh( pState->pBuffer, '\0' );
               pBuffer = zh_membufPtr( pState->pBuffer ) + nSkip;
               zh_pp_error( pState, 'E', ZH_PP_ERR_STRING_TERMINATOR, pBuffer );
            }
            else
               ++n;
         }
         else if( ch == '"' || ch == '\'' || ch == '`' )
         {
            if( ch == '`' )
               ch = '\'';
            while( ++n < nLen && pBuffer[ n ] != ch )
               ;
            if( pState->fMultiLineStr )
            {
               while( n == nLen )
               {
                  ZH_SIZE u = 1;
                  while( n > u && pBuffer[ n - u ] == ' ' )
                     ++u;
                  if( n >= u && pBuffer[ n - u ] == ';' )
                  {
                     n -= u;
                     nLen -= u;
                     u = zh_membufLen( pState->pBuffer ) - u;
                     zh_membufRemove( pState->pBuffer, u );
                     zh_pp_readLine( pState );
                     nLen += zh_membufLen( pState->pBuffer ) - u;
                     pBuffer = zh_membufPtr( pState->pBuffer ) + u - n;
                     --n;
                     while( ++n < nLen && pBuffer[ n ] != ch )
                        ;
                  }
                  else
                  {
                     n = nLen;
                     break;
                  }
               }
            }
            zh_pp_tokenAddNext( pState, pBuffer + 1, n - 1,
                                ZH_PP_TOKEN_STRING );
            if( n == nLen )
            {
               ZH_SIZE nSkip = pBuffer - zh_membufPtr( pState->pBuffer ) + 1;
               zh_membufAddCh( pState->pBuffer, '\0' );
               pBuffer = zh_membufPtr( pState->pBuffer ) + nSkip;
               zh_pp_error( pState, 'E', ZH_PP_ERR_STRING_TERMINATOR, pBuffer );
            }
            else
               ++n;
         }
         else if( ch == '[' && ! pState->fDirective &&
                  zh_pp_canQuote( pState->fCanNextLine ||
                                  ZH_PP_TOKEN_CANQUOTE( pState->usLastType ),
                                  pBuffer, nLen, 1, &n ) )
         {
            zh_pp_tokenAddNext( pState, pBuffer + 1, n - 1, ZH_PP_TOKEN_STRING );
            ++n;
         }
         else if( ( ch == '/' || ch == '&' ) && nLen > 1 && pBuffer[ 1 ] == ch )
         {
            /* strip the rest of line with // or && comment */
            n = nLen;
         }
         else if( ch == '*' && pState->pFile->iTokens == 0 )
         {
            /* strip the rest of line with * comment */
            n = nLen;
         }
         else if( ch == '/' && nLen > 1 && pBuffer[ 1 ] == '*' )
         {

            pState->iStreamDump = ZH_PP_STREAM_COMMENT;
            n += 2;
         }
         else if( ch == ' ' || ch == '\t' )
         {
            do
            {
               if( pBuffer[ n ] == ' ' )
                  pState->nSpaces++;
               else if( pBuffer[ n ] == '\t' )
                  pState->nSpaces += 4;
               else
                  break;
            }
            while( ++n < nLen );
         }
         else if( ch == ';' )
         {
            if( pState->fCanNextLine )
               zh_pp_tokenAddCmdSep( pState );
            pState->fCanNextLine = ZH_TRUE;
            pState->nSpacesNL = pState->nSpaces;
            pState->nSpaces = 0;
            ++n;
         }
         else if( ZH_PP_ISFIRSTIDCHAR( ch ) )
         {
            while( ++n < nLen && ZH_PP_ISNEXTIDCHAR( pBuffer[ n ] ) )
               ;

  
            if( pState->fNewStatement &&
                n == 4 && zh_strnicmp( "NOTE", pBuffer, 4 ) == 0 )
            {
               /* strip the rest of line */
               n = nLen;
            }
            else
            {
               if( n < nLen && pBuffer[ n ] == '&' )
               {

                  while( nLen - n > 1 && pBuffer[ n ] == '&' &&
                         ZH_PP_ISFIRSTIDCHAR( pBuffer[ n + 1 ] ) )
                  {
                     while( ++n < nLen && ZH_PP_ISNEXTIDCHAR( pBuffer[ n ] ) )
                        ;
                     if( n < nLen && pBuffer[ n ] == '.' )
                        while( ++n < nLen && ZH_PP_ISNEXTIDCHAR( pBuffer[ n ] ) )
                           ;
                  }
                  if( n < nLen && pBuffer[ n ] == '&' )
                     ++n;
                  zh_pp_tokenAddNext( pState, pBuffer, n, ZH_PP_TOKEN_MACROTEXT );
               }
               else if( pState->pInLineFunc &&
                        pState->iInLineState == ZH_PP_INLINE_OFF &&
                        n == 9 && zh_strnicmp( "zh_inline", pBuffer, 9 ) == 0 )
               {
                  if( pState->fCanNextLine )
                     zh_pp_tokenAddCmdSep( pState );
                  pInLinePtr = pState->pNextTokenPtr;
                  zh_pp_tokenAddNext( pState, pBuffer, n, ZH_PP_TOKEN_KEYWORD );
                  pState->iInLineState = ZH_PP_INLINE_START;
                  pState->iInLineBraces = 0;
               }
               else
                  zh_pp_tokenAddNext( pState, pBuffer, n, ZH_PP_TOKEN_KEYWORD );
            }
         }

         else if( ZH_PP_ISTEXTCHAR( ch ) )
         {
            while( ++n < nLen && ZH_PP_ISTEXTCHAR( pBuffer[ n ] ) )
               ;

            zh_pp_tokenAddNext( pState, pBuffer, n, ZH_PP_TOKEN_TEXT );
         }
         else if( ZH_PP_ISILLEGAL( ch ) )
         {
            char szCh[ 3 ];

            zh_pp_tokenAddNext( pState, pBuffer, ++n, ZH_PP_TOKEN_NUL );
            zh_snprintf( szCh, sizeof( szCh ), "%02x", ch & 0xff );
            zh_pp_error( pState, 'E', ZH_PP_ERR_ILLEGAL_CHAR, szCh );
         }
         else if( ZH_PP_ISDIGIT( ch ) )
         {
            if( nLen >= 3 && pBuffer[ 0 ] == '0' &&
                ( pBuffer[ 1 ] == 'x' || pBuffer[ 1 ] == 'X' ) &&
                ZH_PP_ISHEX( pBuffer[ 2 ] ) )
            {
               n = 2;
               while( ++n < nLen && ZH_PP_ISHEX( pBuffer[ n ] ) )
                  ;

               /* (LEX: mark token as hex?) */
               zh_pp_tokenAddNext( pState, pBuffer, n, ZH_PP_TOKEN_NUMBER );
            }
            else if( nLen >= 3 && pBuffer[ 0 ] == '0' &&
                     ( pBuffer[ 1 ] == 'd' || pBuffer[ 1 ] == 'D' ) &&
                     ZH_PP_ISDIGIT( pBuffer[ 2 ] ) )
            {
               n = 2;
               while( ++n < nLen && ZH_PP_ISDIGIT( pBuffer[ n ] ) )
                  ;

               zh_pp_tokenAddNext( pState, pBuffer, n, ZH_PP_TOKEN_DATE );
            }
            else
            {
               while( ++n < nLen && ZH_PP_ISDIGIT( pBuffer[ n ] ) )
                  ;
               if( nLen - n > 1 && pBuffer[ n ] == '.' &&
                                     ZH_PP_ISDIGIT( pBuffer[ n + 1 ] ) )
               {
                  ++n;
                  while( ++n < nLen && ZH_PP_ISDIGIT( pBuffer[ n ] ) )
                     ;
               }
               zh_pp_tokenAddNext( pState, pBuffer, n, ZH_PP_TOKEN_NUMBER );
            }
         }
         else if( ch == '.' && nLen > 1 && ZH_PP_ISDIGIT( pBuffer[ 1 ] ) )
         {
            while( ++n < nLen && ZH_PP_ISDIGIT( pBuffer[ n ] ) )
               ;

            zh_pp_tokenAddNext( pState, pBuffer, n, ZH_PP_TOKEN_NUMBER );
         }
         else if( ch == '.' && nLen >= 3 && pBuffer[ 2 ] == '.' &&
                  ( ZH_PP_ISTRUE( pBuffer[ 1 ] ) || ZH_PP_ISFALSE( pBuffer[ 1 ] ) ) )
         {
            const char * value = ZH_PP_ISTRUE( pBuffer[ 1 ] ) ? ".T." : ".F.";

            n = 3;
            zh_pp_tokenAddNext( pState, value, n, ZH_PP_TOKEN_LOGICAL | ZH_PP_TOKEN_STATIC );
         }
         else if( ch == '&' && nLen > 1 && ZH_PP_ISFIRSTIDCHAR( pBuffer[ 1 ] ) )
         {
            int iParts = 0;

            while( nLen - n > 1 && pBuffer[ n ] == '&' &&
                   ZH_PP_ISFIRSTIDCHAR( pBuffer[ n + 1 ] ) )
            {
               ++iParts;
               while( ++n < nLen && ZH_PP_ISNEXTIDCHAR( pBuffer[ n ] ) )
                  ;
               if( n < nLen && pBuffer[ n ] == '.' )
                  while( ++n < nLen && ZH_PP_ISNEXTIDCHAR( pBuffer[ n ] ) )
                     ++iParts;
            }
            if( n < nLen && pBuffer[ n ] == '&' )
            {
               ++iParts;
               ++n;
            }
            zh_pp_tokenAddNext( pState, pBuffer, n, iParts == 1 ?
                                ZH_PP_TOKEN_MACROVAR : ZH_PP_TOKEN_MACROTEXT );
         }
         else if( ch == '{' && ! pState->fCanNextLine &&
                  ( pState->iInLineState == ZH_PP_INLINE_BODY ||
                    pState->iInLineState == ZH_PP_INLINE_START ) )
         {
            if( pState->iInLineState == ZH_PP_INLINE_START )
            {
               zh_pp_tokenAddNext( pState, "(", 1, ZH_PP_TOKEN_LEFT_PB | ZH_PP_TOKEN_STATIC );
               zh_pp_tokenAddNext( pState, ")", 1, ZH_PP_TOKEN_RIGHT_PB | ZH_PP_TOKEN_STATIC );
            }
            pState->iInLineState = ZH_PP_INLINE_OFF;
            pState->iStreamDump = ZH_PP_STREAM_INLINE_C;
            pState->iDumpLine = pState->pFile->iCurrentLine - 1;
            if( pState->pStreamBuffer )
               zh_membufFlush( pState->pStreamBuffer );
            else
               pState->pStreamBuffer = zh_membufNew();
         }
         else
         {
            const ZH_PP_OPERATOR * pOperator = zh_pp_operatorFind( pState, pBuffer, nLen );

            if( pOperator )
            {
               zh_pp_tokenAddNext( pState, pOperator->value,
                                   strlen( pOperator->value ),
                                   pOperator->type );
               n = pOperator->len;
            }
            else
            {
               zh_pp_tokenAddNext( pState, pBuffer, ++n, ZH_PP_TOKEN_OTHER );
            }
         }
         pBuffer += n;
         nLen -= n;
         n = 0;
      }

      if( pEolTokenPtr &&
          ( pEolTokenPtr != pState->pNextTokenPtr ||
            ( pState->iNestedBlock && pState->pFile->iTokens &&
              ( pState->pFile->pLineBuf ? pState->pFile->nLineBufLen == 0 :
                                          pState->pFile->fEof ) ) ) )
      {
         PZH_PP_TOKEN pToken = *pEolTokenPtr;

         while( iStartLine < pState->pFile->iCurrentLine )
         {
            zh_pp_tokenAdd( &pEolTokenPtr, "\n", 1, 0, ZH_PP_TOKEN_EOL | ZH_PP_TOKEN_STATIC );
            pState->pFile->iTokens++;
            iStartLine++;
            iLines++;
         }
         if( pToken == NULL )
            pState->pNextTokenPtr = pEolTokenPtr;
         *pEolTokenPtr = pToken;
      }

      if( ! pState->fCanNextLine &&
          ! ( pState->iStreamDump && pState->iStreamDump != ZH_PP_STREAM_TEXT ) &&
          ( pState->iNestedBlock || pState->iBlockState == 5 ) )
      {
         pEolTokenPtr = pState->pNextTokenPtr;
         pState->nSpaces = pState->nSpacesMin = 0;
         pState->fNewStatement = ZH_TRUE;
         pState->fDirective = ZH_FALSE;
         if( pState->iBlockState )
         {
            if( pState->iBlockState == 5 )
               pState->iNestedBlock++;
            pState->iBlockState = 0;
         }
      }
   }
   while( ( pState->pFile->pLineBuf ? pState->pFile->nLineBufLen != 0 :
                                      ! pState->pFile->fEof ) &&
          ( pState->fCanNextLine || pState->iNestedBlock ||
            ( pState->iStreamDump && pState->iStreamDump != ZH_PP_STREAM_TEXT ) ) );

   if( pState->iStreamDump )
   {
      if( pState->iStreamDump == ZH_PP_STREAM_COMMENT )
         zh_pp_error( pState, 'E', ZH_PP_ERR_UNTERMINATED_COMMENT, NULL );
      else if( pState->iStreamDump == ZH_PP_STREAM_DUMP_C )
         zh_pp_dumpEnd( pState );
      else if( pState->pFile->pLineBuf ? ! pState->pFile->nLineBufLen :
                                         pState->pFile->fEof )
         zh_pp_error( pState, 'E', ZH_PP_ERR_MISSING_ENDTEXT, NULL );
   }

   if( pState->pFile->iTokens != 0 )
   {
      zh_pp_tokenAdd( &pState->pNextTokenPtr, "\n", 1, 0, ZH_PP_TOKEN_EOL | ZH_PP_TOKEN_STATIC );
      pState->pFile->iTokens++;
   }
   pState->pFile->iCurrentLine -= iLines;
}

static int zh_pp_tokenStr( PZH_PP_TOKEN pToken, PZH_MEM_BUFFER pBuffer,
                           ZH_BOOL fSpaces, ZH_BOOL fQuote, ZH_USHORT ltype )
{
   int iLines = 0;
   ZH_ISIZ nSpace = fSpaces ? pToken->spaces : 0;


   if( nSpace == 0 && fQuote && ltype &&
       ltype >= ZH_PP_TOKEN_ASSIGN && ltype != ZH_PP_TOKEN_EQ &&
       ZH_PP_TOKEN_TYPE( pToken->type ) >= ZH_PP_TOKEN_ASSIGN &&
       ZH_PP_TOKEN_TYPE( pToken->type ) != ZH_PP_TOKEN_EQ )
      nSpace = 1;

   if( nSpace > 0 )
   {
      do
      {
         zh_membufAddCh( pBuffer, ' ' );
      }
      while( --nSpace );
   }

   if( ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_STRING )
   {
      int iq = 7;
      ZH_SIZE n;
      char ch;

      for( n = 0; iq && n < pToken->len; ++n )
      {
         switch( pToken->value[ n ] )
         {
            case '"':
               iq &= ~1;
               break;
            case '\'':
               iq &= ~2;
               break;
            case ']':
               iq &= ~4;
               break;
            case '\n':
            case '\r':
            case '\0':
               iq = 0;
               break;
         }
      }
      if( iq == 0 && fQuote )
      {
         /* generate string with 'e' prefix before opening '"' and quote
            control characters inside, e.g.:
               e"line1\nline2"
          */

         zh_membufAddCh( pBuffer, 'e' );
         zh_membufAddCh( pBuffer, '"' );
         for( n = 0; n < pToken->len; ++n )
         {
            ch = pToken->value[ n ];
            switch( ch )
            {
               case '\r':
                  iq = ch = 'r';
                  break;
               case '\n':
                  iq = ch = 'n';
                  break;
               case '\t':
                  iq = ch = 't';
                  break;
               case '\b':
                  iq = ch = 'b';
                  break;
               case '\f':
                  iq = ch = 'f';
                  break;
               case '\v':
                  iq = ch = 'v';
                  break;
               case '\a':
                  iq = ch = 'a';
                  break;
               case '\0':
                  iq = ch = '0';
                  break;
               case '"':
               case '\\':
                  iq = 1;
                  break;
               default:
                  iq = 0;
                  break;
            }
            if( iq )
               zh_membufAddCh( pBuffer, '\\' );
            zh_membufAddCh( pBuffer, ch );
         }
         zh_membufAddCh( pBuffer, '"' );
      }
      else
      {
         if( iq & 1 )
            ch = '"';
         else if( iq & 2 )
            ch = '\'';
         else
            ch = '[';

         zh_membufAddCh( pBuffer, ch );
         zh_membufAddData( pBuffer, pToken->value, pToken->len );
         zh_membufAddCh( pBuffer, ( char ) ( ch == '[' ? ']' : ch ) );
      }
   }
   else if( ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_TIMESTAMP )
   {
      if( pToken->len >= 2 && pToken->value[ 0 ] == '0' &&
          ( pToken->value[ 1 ] == 'T' || pToken->value[ 1 ] == 't' ) )
      {
         zh_membufAddData( pBuffer, pToken->value, pToken->len );
      }
      else
      {
         zh_membufAddStr( pBuffer, "t\"" );
         zh_membufAddData( pBuffer, pToken->value, pToken->len );
         zh_membufAddCh( pBuffer, '"' );
      }
   }
   else if( ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_DATE )
   {
      if( pToken->len >= 2 && pToken->value[ 0 ] == '0' &&
          ( pToken->value[ 1 ] == 'D' || pToken->value[ 1 ] == 'd' ) )
      {
         zh_membufAddData( pBuffer, pToken->value, pToken->len );
      }
      else
      {
         zh_membufAddStr( pBuffer, "d\"" );
         zh_membufAddData( pBuffer, pToken->value, pToken->len );
         zh_membufAddCh( pBuffer, '"' );
      }
   }
   else
   {
      if( ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_EOL )
         ++iLines;
      zh_membufAddData( pBuffer, pToken->value, pToken->len );
   }

   return iLines;
}

static ZH_BOOL zh_pp_tokenValueCmp( PZH_PP_TOKEN pToken, const char * szValue, ZH_USHORT mode )
{
   if( pToken->len )
   {
      if( mode == ZH_PP_CMP_CASE )
         return memcmp( szValue, pToken->value, pToken->len ) == 0;
      if( mode == ZH_PP_CMP_DBASE && pToken->len >= 4 &&
          ( ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_KEYWORD ||
            ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_STRING ||
            ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_TEXT ) )
         return zh_strnicmp( szValue, pToken->value, pToken->len ) == 0;
      else
         return zh_stricmp( szValue, pToken->value ) == 0;
   }
   return ZH_FALSE;
}

static ZH_BOOL zh_pp_tokenEqual( PZH_PP_TOKEN pToken, PZH_PP_TOKEN pMatch,
                                 ZH_USHORT mode )
{
   return pToken == pMatch ||
         ( mode != ZH_PP_CMP_ADDR &&
           ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_TYPE( pMatch->type ) &&
           ( pToken->len == pMatch->len ||
             ( mode == ZH_PP_CMP_DBASE && pMatch->len > 4 &&
               pToken->len >= 4 && pMatch->len > pToken->len ) ) &&
           zh_pp_tokenValueCmp( pToken, pMatch->value, mode ) );
}

static void zh_pp_patternClearResults( PZH_PP_RULE pRule )
{
   PZH_PP_MARKER pMarker = pRule->pMarkers;
   int i = pRule->markers;

   while( --i >= 0 )
   {
      pMarker->matches = 0;
      while( pMarker->pResult )
      {
         PZH_PP_RESULT pResult = pMarker->pResult;
         pMarker->pResult = pResult->pNext;
         zh_xfree( pResult );
      }
      ++pMarker;
   }
   pRule->pNextExpr = NULL;
}

static ZH_BOOL zh_pp_patternAddResult( PZH_PP_RULE pRule, ZH_USHORT marker,
                                       PZH_PP_TOKEN pFirst, PZH_PP_TOKEN pNext )
{
   PZH_PP_MARKER pMarker = &pRule->pMarkers[ marker - 1 ];

   if( pMarker->matches == 0 || pMarker->canrepeat )
   {
      PZH_PP_RESULT * pResultPtr,
               pResult = ( PZH_PP_RESULT ) zh_xgrab( sizeof( ZH_PP_RESULT ) );
      pMarker->matches++;
      pResult->pFirstToken = pFirst;
      pResult->pNextExpr = pNext;
      pResult->pNext = NULL;
      pResultPtr = &pMarker->pResult;
      while( *pResultPtr )
         pResultPtr = &( *pResultPtr )->pNext;
      *pResultPtr = pResult;
      return ZH_TRUE;
   }

   return ZH_FALSE;
}

static PZH_PP_RULE zh_pp_ruleNew( PZH_PP_TOKEN pMatch, PZH_PP_TOKEN pResult,
                                  ZH_USHORT mode, ZH_USHORT markers,
                                  PZH_PP_MARKER pMarkers )
{
   PZH_PP_RULE pRule = ( PZH_PP_RULE ) zh_xgrab( sizeof( ZH_PP_RULE ) );

   pRule->pPrev = NULL;
   pRule->mode = mode;
   pRule->pMatch = pMatch;
   pRule->pResult = pResult;
   pRule->markers = markers;
   pRule->pMarkers = pMarkers;
   pRule->pNextExpr = NULL;

   return pRule;
}

void zh_pp_ruleFree( PZH_PP_RULE pRule )
{
   zh_pp_tokenListFree( &pRule->pMatch );
   zh_pp_tokenListFree( &pRule->pResult );
   zh_pp_patternClearResults( pRule );
   if( pRule->pMarkers )
      zh_xfree( pRule->pMarkers );
   zh_xfree( pRule );
}

static void zh_pp_ruleListFree( PZH_PP_RULE * pRulePtr )
{
   PZH_PP_RULE pRule;

   while( *pRulePtr )
   {
      pRule = *pRulePtr;
      *pRulePtr = pRule->pPrev;
      zh_pp_ruleFree( pRule );
   }
}

static void zh_pp_ruleListNonStdFree( PZH_PP_RULE * pRulePtr )
{
   PZH_PP_RULE pRule;

   while( *pRulePtr )
   {
      pRule = *pRulePtr;
      if( ( pRule->mode & ZH_PP_STD_RULE ) != 0 )
      {
         pRulePtr = &pRule->pPrev;
      }
      else
      {
         *pRulePtr = pRule->pPrev;
         zh_pp_ruleFree( pRule );
      }
   }
}

static void zh_pp_ruleListSetStd( PZH_PP_RULE pRule )
{
   while( pRule )
   {
      pRule->mode |= ZH_PP_STD_RULE;
      pRule = pRule->pPrev;
   }
}

static void zh_pp_ruleSetId( PZH_PP_STATE pState, PZH_PP_TOKEN pMatch, ZH_BYTE id )
{
   if( ZH_PP_TOKEN_ISMATCH( pMatch ) )
   {
      int i;
      for( i = 0; i < ZH_PP_HASHID_MAX; ++i )
         pState->pMap[ i ] |= id;
   }
   else
      pState->pMap[ ZH_PP_HASHID( pMatch ) ] |= id;
}

static void zh_pp_ruleListSetId( PZH_PP_STATE pState, PZH_PP_RULE pRule, ZH_BYTE id )
{
   while( pRule )
   {
      zh_pp_ruleSetId( pState, pRule->pMatch, id );
      if( ZH_PP_TOKEN_ISMATCH( pRule->pMatch ) )
         break;
      pRule = pRule->pPrev;
   }
}

static PZH_PP_RULE zh_pp_defineFind( PZH_PP_STATE pState, PZH_PP_TOKEN pToken )
{
   PZH_PP_RULE pRule = pState->pDefinitions;

   /* TODO% create binary tree or hash table - the #define keyword token has
            to be unique so it's not necessary to keep the stack list,
            it will increase the speed when there is a lot of #define values */

   while( pRule && ! zh_pp_tokenEqual( pToken, pRule->pMatch, ZH_PP_CMP_CASE ) )
      pRule = pRule->pPrev;

   return pRule;
}

static void zh_pp_defineAdd( PZH_PP_STATE pState, ZH_USHORT mode,
                             ZH_USHORT markers, PZH_PP_MARKER pMarkers,
                             PZH_PP_TOKEN pMatch, PZH_PP_TOKEN pResult )
{
   PZH_PP_RULE pRule = zh_pp_defineFind( pState, pMatch );

   if( pRule )
   {
      zh_pp_tokenListFree( &pRule->pMatch );
      zh_pp_tokenListFree( &pRule->pResult );
      zh_pp_patternClearResults( pRule );
      if( pRule->pMarkers )
         zh_xfree( pRule->pMarkers );
      pRule->pMatch = pMatch;
      pRule->pResult = pResult;
      pRule->pMarkers = pMarkers;
      pRule->markers = markers;
      pRule->mode = mode;
      zh_pp_error( pState, 'W', ZH_PP_WARN_DEFINE_REDEF, pMatch->value );
   }
   else
   {
      pRule = zh_pp_ruleNew( pMatch, pResult, mode, markers, pMarkers );
      pRule->pPrev = pState->pDefinitions;
      pState->pDefinitions = pRule;
      pState->iDefinitions++;
   }
   zh_pp_ruleSetId( pState, pMatch, ZH_PP_DEFINE );
}

static void zh_pp_defineDel( PZH_PP_STATE pState, PZH_PP_TOKEN pToken )
{
   PZH_PP_RULE * pRulePtr = &pState->pDefinitions, pRule;

   while( *pRulePtr )
   {
      pRule = *pRulePtr;
      if( zh_pp_tokenEqual( pToken, pRule->pMatch, ZH_PP_CMP_CASE ) )
      {
         *pRulePtr = pRule->pPrev;
         zh_pp_ruleFree( pRule );
         pState->iDefinitions--;
         return;
      }
      pRulePtr = &pRule->pPrev;
   }
}

static PZH_PP_FILE zh_pp_FileNew( PZH_PP_STATE pState, const char * szFileName,
                                  ZH_BOOL fSysFile, ZH_BOOL * pfNested,
                                  FILE * file_in, ZH_BOOL fSearchPath,
                                  PZH_PP_OPEN_FUNC pOpenFunc, ZH_BOOL fBinary )
{
   char szFileNameBuf[ ZH_PATH_MAX ];
   const char * pLineBuf = NULL;
   ZH_SIZE nLineBufLen = 0;
   ZH_BOOL fFree = ZH_FALSE;
   PZH_PP_FILE pFile;

   if( ! file_in )
   {
      int iAction = ZH_PP_OPEN_FILE;

      if( pOpenFunc )
      {
         zh_strncpy( szFileNameBuf, szFileName, sizeof( szFileNameBuf ) - 1 );
         iAction = ( pOpenFunc )( pState->cargo, szFileNameBuf,
                                  ZH_TRUE, fSysFile, fBinary,
                                  fSearchPath ? pState->pIncludePath : NULL,
                                  pfNested, &file_in,
                                  &pLineBuf, &nLineBufLen, &fFree );
         if( iAction == ZH_PP_OPEN_OK )
            szFileName = szFileNameBuf;
      }

      if( iAction == ZH_PP_OPEN_FILE )
      {
         PZH_FNAME pFileName = zh_fsFNameSplit( szFileName );
         ZH_BOOL fNested = ZH_FALSE;

         pFileName->szName = szFileName;
         pFileName->szExtension = NULL;
         if( ! fSysFile )
         {
            if( pFileName->szPath )
               file_in = zh_fopen( szFileName, fBinary ? "rb" : "r" );
            if( ! file_in && ( ! pFileName->szPath || ( ! pFileName->szDrive &&
                ! strchr( ZH_OS_PATH_DELIM_CHR_LIST, ( ZH_UCHAR ) pFileName->szPath[ 0 ] ) ) ) )
            {
               char * szFirstFName = NULL;
               pFile = pState->pFile;
               while( pFile )
               {
                  if( pFile->szFileName )
                     szFirstFName = pFile->szFileName;
                  pFile = pFile->pPrev;
               }
               if( szFirstFName )
               {
                  PZH_FNAME pFirstFName = zh_fsFNameSplit( szFirstFName );
                  pFileName->szPath = pFirstFName->szPath;
                  zh_fsFNameMerge( szFileNameBuf, pFileName );
                  zh_xfree( pFirstFName );
                  szFileName = szFileNameBuf;
               }
               if( ! pFileName->szPath || szFirstFName )
                  file_in = zh_fopen( szFileName, fBinary ? "rb" : "r" );
            }
            if( file_in )
               iAction = ZH_PP_OPEN_OK;
            else
               fNested = zh_fsMaxFilesError();
         }

         if( iAction != ZH_PP_OPEN_OK )
         {
            if( fNested )
            {
               if( pfNested )
                  *pfNested = ZH_TRUE;
            }
            else if( pState->pIncludePath && fSearchPath )
            {
               ZH_PATHNAMES * pPath = pState->pIncludePath;
               do
               {
                  pFileName->szPath = pPath->szPath;
                  zh_fsFNameMerge( szFileNameBuf, pFileName );
                  file_in = zh_fopen( szFileNameBuf, fBinary ? "rb" : "r" );
                  if( file_in != NULL )
                  {
                     iAction = ZH_PP_OPEN_OK;
                     szFileName = szFileNameBuf;
                     break;
                  }
                  pPath = pPath->pNext;
               }
               while( pPath );
            }

            if( iAction != ZH_PP_OPEN_OK && pOpenFunc && ! fNested )
            {
               zh_strncpy( szFileNameBuf, pFileName->szName, sizeof( szFileNameBuf ) - 1 );
               iAction = ( pOpenFunc )( pState->cargo, szFileNameBuf,
                                        ZH_FALSE, fSysFile, fBinary,
                                        fSearchPath ? pState->pIncludePath : NULL,
                                        pfNested, &file_in,
                                        &pLineBuf, &nLineBufLen, &fFree );
               if( iAction == ZH_PP_OPEN_OK )
                  szFileName = szFileNameBuf;
            }
         }
         zh_xfree( pFileName );
      }

      if( iAction != ZH_PP_OPEN_OK )
         return NULL;

      if( pState->pIncFunc )
         ( pState->pIncFunc )( pState->cargo, szFileName );
   }

   pFile = ( PZH_PP_FILE ) zh_xgrabz( sizeof( ZH_PP_FILE ) );

   pFile->szFileName = zh_strdup( szFileName );
   pFile->file_in = file_in;
   pFile->fFree = fFree;
   pFile->pLineBuf = pLineBuf;
   pFile->nLineBufLen = nLineBufLen;
   pFile->iLastLine = 1;

   return pFile;
}

static PZH_PP_FILE zh_pp_FileBufNew( const char * pLineBuf, ZH_SIZE nLineBufLen )
{
   PZH_PP_FILE pFile = ( PZH_PP_FILE ) zh_xgrabz( sizeof( ZH_PP_FILE ) );

   pFile->fFree = ZH_FALSE;
   pFile->pLineBuf = pLineBuf;
   pFile->nLineBufLen = nLineBufLen;
   pFile->iLastLine = 1;

   return pFile;
}

static void zh_pp_FileFree( PZH_PP_STATE pState, PZH_PP_FILE pFile,
                            PZH_PP_CLOSE_FUNC pCloseFunc )
{
   if( pFile->file_in )
   {
      if( pCloseFunc )
         ( pCloseFunc )( pState->cargo, pFile->file_in );
      else
         fclose( pFile->file_in );
   }

   if( pFile->szFileName )
      zh_xfree( pFile->szFileName );

   if( pFile->fFree && pFile->pLineBuf )
      zh_xfree( ZH_UNCONST( pFile->pLineBuf ) );

   zh_pp_tokenListFree( &pFile->pTokenList );
   zh_xfree( pFile );
}

static void zh_pp_InFileFree( PZH_PP_STATE pState )
{
   while( pState->pFile )
   {
      PZH_PP_FILE pFile = pState->pFile;
      pState->pFile = pFile->pPrev;
      zh_pp_FileFree( pState, pFile, pState->pCloseFunc );
   }
   pState->iFiles = 0;
}

static void zh_pp_OutFileFree( PZH_PP_STATE pState )
{
   if( pState->file_out )
   {
      fclose( pState->file_out );
      pState->file_out = NULL;
   }
   if( pState->szOutFileName )
   {
      zh_xfree( pState->szOutFileName );
      pState->szOutFileName = NULL;
   }
   pState->fWritePreprocesed = ZH_FALSE;
}

static void zh_pp_TraceFileFree( PZH_PP_STATE pState )
{
   if( pState->file_trace )
   {
      fclose( pState->file_trace );
      pState->file_trace = NULL;
   }
   if( pState->szTraceFileName )
   {
      zh_xfree( pState->szTraceFileName );
      pState->szTraceFileName = NULL;
   }
   pState->fWriteTrace = ZH_FALSE;
}

static PZH_PP_STATE zh_pp_stateNew( void )
{
   PZH_PP_STATE pState = ( PZH_PP_STATE ) zh_xgrabz( sizeof( ZH_PP_STATE ) );

   /* create new line buffer */
   pState->pBuffer = zh_membufNew();

   /* set default maximum number of translations */
   pState->iMaxCycles = ZH_PP_MAX_CYCLES;

   return pState;
}

static void zh_pp_stateFree( PZH_PP_STATE pState )
{
   zh_pp_InFileFree( pState );
   zh_pp_OutFileFree( pState );
   zh_pp_TraceFileFree( pState );

   if( pState->pIncludePath )
      zh_fsFreeSearchPath( pState->pIncludePath );

   if( pState->iOperators > 0 )
      zh_pp_operatorsFree( pState->pOperators, pState->iOperators );

   zh_pp_ruleListFree( &pState->pDefinitions );
   zh_pp_ruleListFree( &pState->pTranslations );
   zh_pp_ruleListFree( &pState->pCommands );

   zh_pp_tokenListFree( &pState->pTokenOut );

   zh_membufFree( pState->pBuffer );
   if( pState->pDumpBuffer )
      zh_membufFree( pState->pDumpBuffer );
   if( pState->pOutputBuffer )
      zh_membufFree( pState->pOutputBuffer );
   if( pState->pStreamBuffer )
      zh_membufFree( pState->pStreamBuffer );

   if( pState->pCondStack )
      zh_xfree( pState->pCondStack );

   zh_pp_tokenListFree( &pState->pFuncOut );
   zh_pp_tokenListFree( &pState->pFuncEnd );

   zh_xfree( pState );
}

static PZH_PP_TOKEN zh_pp_streamFuncGet( PZH_PP_TOKEN pToken, PZH_PP_TOKEN * pFuncPtr )
{
   zh_pp_tokenListFree( pFuncPtr );

   if( pToken && ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_PIPE &&
       ! ZH_PP_TOKEN_ISEOC( pToken->pNext ) )
   {
      PZH_PP_TOKEN * pStartPtr, * pEndPtr, pStart, pNext;
      pStartPtr = pEndPtr = &pToken->pNext;
      while( ! ZH_PP_TOKEN_ISEOC( *pEndPtr ) &&
             ZH_PP_TOKEN_TYPE( ( *pEndPtr )->type ) != ZH_PP_TOKEN_PIPE )
         pEndPtr = &( *pEndPtr )->pNext;

      pToken = *pEndPtr;
      *pEndPtr = NULL;
      *pFuncPtr = pStart = *pStartPtr;
      *pStartPtr = pToken;
      /* replace %s with ZH_PP_RMARKER_STRDUMP marker */
      while( pStart && pStart->pNext )
      {
         pNext = pStart->pNext;
         if( ZH_PP_TOKEN_TYPE( pStart->type ) == ZH_PP_TOKEN_MOD &&
             ZH_PP_TOKEN_TYPE( pNext->type ) == ZH_PP_TOKEN_KEYWORD &&
             pNext->len == 1 && pNext->value[ 0 ] == 's' )
         {
            ZH_PP_TOKEN_SETTYPE( pStart, ZH_PP_RMARKER_STRDUMP );
            pStart->pNext = pNext->pNext;
            zh_pp_tokenFree( pNext );
            pNext = pStart->pNext;
         }
         pStart = pNext;
      }
   }
   return pToken;
}

/* #pragma {__text,__stream,__cstream}|functionOut|functionEnd|functionStart */
static ZH_BOOL zh_pp_pragmaStream( PZH_PP_STATE pState, PZH_PP_TOKEN pToken )
{
   ZH_BOOL fError = ZH_FALSE;

   pToken = zh_pp_streamFuncGet( pToken, &pState->pFuncOut );
   pToken = zh_pp_streamFuncGet( pToken, &pState->pFuncEnd );
   if( pToken && ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_PIPE )
   {
      zh_pp_tokenSetValue( pToken, ";", 1 );
      ZH_PP_TOKEN_SETTYPE( pToken, ZH_PP_TOKEN_EOC );
   }

   return fError;
}

#define MAX_STREAM_SIZE       0x1000000

static void zh_pp_pragmaStreamFile( PZH_PP_STATE pState, const char * szFileName )
{
   PZH_PP_FILE pFile = zh_pp_FileNew( pState, szFileName, ZH_FALSE, NULL, NULL,
                                      ZH_TRUE, pState->pOpenFunc,
                                      pState->iStreamDump == ZH_PP_STREAM_BINARY );

   if( pFile )
   {
      ZH_SIZE nSize;

      if( pFile->file_in )
      {
         ( void ) fseek( pFile->file_in, 0L, SEEK_END );
         nSize = ftell( pFile->file_in );
         ( void ) fseek( pFile->file_in, 0L, SEEK_SET );
      }
      else
         nSize = pFile->nLineBufLen;

      if( nSize > MAX_STREAM_SIZE )
         zh_pp_error( pState, 'F', ZH_PP_ERR_FILE_TOO_LONG, szFileName );
      else if( pState->pFuncOut || pState->pFuncEnd )
      {
         PZH_PP_TOKEN pToken;
         ZH_BOOL fEOL = ZH_FALSE;

         if( ! pState->pStreamBuffer )
            pState->pStreamBuffer = zh_membufNew();

         if( nSize )
         {
            if( pFile->file_in == NULL && pState->iStreamDump != ZH_PP_STREAM_C )
               zh_membufAddData( pState->pStreamBuffer, pFile->pLineBuf, nSize );
            else
            {
               char * pBuffer = ( char * ) zh_xgrab( nSize * sizeof( char ) );

               if( pFile->file_in )
                  nSize = ( ZH_SIZE ) fread( pBuffer, sizeof( char ), nSize, pFile->file_in );
               else
                  memcpy( pBuffer, pFile->pLineBuf, nSize );

               if( pState->iStreamDump == ZH_PP_STREAM_C )
                  zh_strRemEscSeq( pBuffer, &nSize );

               zh_membufAddData( pState->pStreamBuffer, pBuffer, nSize );
               zh_xfree( pBuffer );
            }
         }

         /* insert new tokens into incoming buffer
          * so they can be preprocessed
          */
         pState->pNextTokenPtr = &pState->pFile->pTokenList;
         while( ! ZH_PP_TOKEN_ISEOS( *pState->pNextTokenPtr ) )
            pState->pNextTokenPtr = &( *pState->pNextTokenPtr )->pNext;
         if( *pState->pNextTokenPtr == NULL )
         {
            zh_pp_tokenAdd( &pState->pNextTokenPtr, "\n", 1, 0, ZH_PP_TOKEN_EOL | ZH_PP_TOKEN_STATIC );
            pState->pFile->iTokens++;
         }
         else if( ZH_PP_TOKEN_TYPE( ( *pState->pNextTokenPtr )->type ) == ZH_PP_TOKEN_EOL )
         {
            zh_pp_tokenSetValue( *pState->pNextTokenPtr, ";", 1 );
            ZH_PP_TOKEN_SETTYPE( *pState->pNextTokenPtr, ZH_PP_TOKEN_EOC );
            fEOL = ZH_TRUE;
         }
         pState->pNextTokenPtr = &( *pState->pNextTokenPtr )->pNext;
         pToken = *pState->pNextTokenPtr;

         if( pState->pFuncOut )
            zh_pp_tokenAddStreamFunc( pState, pState->pFuncOut,
                                      zh_membufPtr( pState->pStreamBuffer ),
                                      zh_membufLen( pState->pStreamBuffer ) );
         if( pState->pFuncEnd )
         {
            if( pState->pFuncOut )
               zh_pp_tokenAddCmdSep( pState );
            zh_pp_tokenAddStreamFunc( pState, pState->pFuncEnd,
                                      zh_membufPtr( pState->pStreamBuffer ),
                                      zh_membufLen( pState->pStreamBuffer ) );
         }
         if( fEOL )
            zh_pp_tokenAdd( &pState->pNextTokenPtr, "\n", 1, 0, ZH_PP_TOKEN_EOL | ZH_PP_TOKEN_STATIC );
         else
            zh_pp_tokenAdd( &pState->pNextTokenPtr, ";", 1, 0, ZH_PP_TOKEN_EOC | ZH_PP_TOKEN_STATIC );
         pState->pFile->iTokens++;
         pState->fNewStatement = ZH_TRUE;
         *pState->pNextTokenPtr = pToken;
         zh_membufFlush( pState->pStreamBuffer );
      }
      zh_pp_FileFree( pState, pFile, pState->pCloseFunc );
   }
   else
      zh_pp_error( pState, 'F', ZH_PP_ERR_CANNOT_OPEN_FILE, szFileName );

   zh_pp_tokenListFree( &pState->pFuncOut );
   zh_pp_tokenListFree( &pState->pFuncEnd );
}

static ZH_BOOL zh_pp_pragmaOperatorNew( PZH_PP_STATE pState, PZH_PP_TOKEN pToken )
{
   ZH_BOOL fError = ZH_TRUE;

   if( ! ZH_PP_TOKEN_ISEOC( pToken ) && ZH_PP_TOKEN_CANJOIN( pToken->type ) )
   {
      ZH_SIZE nLen;

      zh_membufFlush( pState->pBuffer );
      do
      {
         zh_membufAddData( pState->pBuffer, pToken->value, pToken->len );
         pToken = pToken->pNext;
      }
      while( ! ZH_PP_TOKEN_ISEOC( pToken ) && pToken->spaces == 0 );
      nLen = zh_membufLen( pState->pBuffer );
      if( ! ZH_PP_TOKEN_ISEOC( pToken ) )
      {
         do
         {
            zh_membufAddData( pState->pBuffer, pToken->value, pToken->len );
            pToken = pToken->pNext;
         }
         while( ! ZH_PP_TOKEN_ISEOC( pToken ) && pToken->spaces == 0 );
      }
      if( ZH_PP_TOKEN_ISEOC( pToken ) && nLen > 0 )
      {
         PZH_PP_OPERATOR pOperator;
         char * pBuffer = zh_membufPtr( pState->pBuffer ), * pDstBuffer;
         ZH_SIZE nDstLen = zh_membufLen( pState->pBuffer ) - nLen;

         if( nDstLen )
            pDstBuffer = pBuffer + nLen;
         else
         {
            pDstBuffer = pBuffer;
            nDstLen = nLen;
         }
         if( pState->iOperators )
            pState->pOperators = ( PZH_PP_OPERATOR ) zh_xrealloc(
                     pState->pOperators,
                     sizeof( ZH_PP_OPERATOR ) * ( pState->iOperators + 1 ) );
         else
            pState->pOperators = ( PZH_PP_OPERATOR ) zh_xgrab(
                     sizeof( ZH_PP_OPERATOR ) * ( pState->iOperators + 1 ) );
         pOperator = &pState->pOperators[ pState->iOperators++ ];
         pOperator->name  = zh_strndup( pBuffer, nLen );
         pOperator->len   = nLen;
         pOperator->value = zh_strndup( pDstBuffer, nDstLen );
         pOperator->type  = ZH_PP_TOKEN_OTHER;
         fError = ZH_FALSE;
      }
   }
   return fError;
}

static ZH_BOOL zh_pp_setCompilerSwitch( PZH_PP_STATE pState, const char * szSwitch,
                                        int iValue )
{
   ZH_BOOL fError = ZH_TRUE;

   switch( szSwitch[ 0 ] )
   {
      case 'p':
      case 'P':
         if( szSwitch[ 1 ] == '\0' )
         {
            pState->fWritePreprocesed = pState->file_out != NULL && iValue != 0;
            fError = ZH_FALSE;
         }
         else if( szSwitch[ 1 ] == '+' && szSwitch[ 2 ] == '\0' )
         {
            pState->fWriteTrace = pState->file_trace != NULL && iValue != 0;
            fError = ZH_FALSE;
         }
         break;

      case 'q':
      case 'Q':
         if( szSwitch[ 1 ] == '\0' )
         {
            pState->fQuiet = iValue != 0;
            fError = ZH_FALSE;
         }
         break;
   }

   if( pState->pSwitchFunc )
      fError = ( pState->pSwitchFunc )( pState->cargo, szSwitch, &iValue, ZH_TRUE );

   return fError;
}

static ZH_BOOL zh_pp_getCompilerSwitch( PZH_PP_STATE pState, const char * szSwitch,
                                        int * piValue )
{
   ZH_BOOL fError = ZH_TRUE;

   if( pState->pSwitchFunc )
      fError = ( pState->pSwitchFunc )( pState->cargo, szSwitch, piValue, ZH_FALSE );

   if( fError )
   {
      switch( szSwitch[ 0 ] )
      {
         case 'p':
         case 'P':
            if( szSwitch[ 1 ] == '\0' )
            {
               *piValue = pState->fWritePreprocesed ? 1 : 0;
               fError = ZH_FALSE;
            }
            else if( szSwitch[ 1 ] == '+' && szSwitch[ 2 ] == '\0' )
            {
               *piValue = pState->fWriteTrace ? 1 : 0;
               fError = ZH_FALSE;
            }
            break;

         case 'q':
         case 'Q':
            if( szSwitch[ 1 ] == '\0' )
            {
               *piValue = pState->fQuiet ? 1 : 0;
               fError = ZH_FALSE;
            }
            break;
      }
   }

   return fError;
}

static PZH_PP_TOKEN zh_pp_pragmaGetLogical( PZH_PP_TOKEN pToken, ZH_BOOL * pfValue )
{
   PZH_PP_TOKEN pValue = NULL;

   if( pToken && pToken->pNext &&
       ZH_PP_TOKEN_TYPE( pToken->pNext->type ) == ZH_PP_TOKEN_KEYWORD )
   {
      if( ( ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_EQ &&
            ZH_PP_TOKEN_ISEOC( pToken->pNext->pNext ) ) ||
          ( pToken->pNext->pNext &&
            ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_LEFT_PB &&
            ZH_PP_TOKEN_TYPE( pToken->pNext->pNext->type ) == ZH_PP_TOKEN_RIGHT_PB &&
            ZH_PP_TOKEN_ISEOC( pToken->pNext->pNext->pNext ) ) )
      {
         pValue = pToken->pNext;
         if( zh_stricmp( pValue->value, "ON" ) == 0 )
            *pfValue = ZH_TRUE;
         else if( zh_stricmp( pValue->value, "OFF" ) == 0 )
            *pfValue = ZH_FALSE;
         else
            pValue = NULL;
      }
   }
   return pValue;
}

static PZH_PP_TOKEN zh_pp_pragmaGetInt( PZH_PP_TOKEN pToken, int * piValue )
{
   PZH_PP_TOKEN pValue = NULL;

   if( pToken && pToken->pNext &&
       ZH_PP_TOKEN_TYPE( pToken->pNext->type ) == ZH_PP_TOKEN_NUMBER )
   {
      if( ( ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_EQ &&
            ZH_PP_TOKEN_ISEOC( pToken->pNext->pNext ) ) ||
          ( pToken->pNext->pNext &&
            ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_LEFT_PB &&
            ZH_PP_TOKEN_TYPE( pToken->pNext->pNext->type ) == ZH_PP_TOKEN_RIGHT_PB &&
            ZH_PP_TOKEN_ISEOC( pToken->pNext->pNext->pNext ) ) )
      {
         pValue = pToken->pNext;
         *piValue = atoi( pValue->value );
      }
   }
   return pValue;
}

static PZH_PP_TOKEN zh_pp_pragmaGetSwitch( PZH_PP_TOKEN pToken, int * piValue )
{
   PZH_PP_TOKEN pValue = NULL;

   if( pToken && ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_KEYWORD )
   {
      ZH_BOOL fNum = pToken->len > 1 && ZH_PP_ISDIGIT( pToken->value[ pToken->len - 1 ] );

      if( ZH_PP_TOKEN_ISEOC( pToken->pNext ) )
      {
         if( fNum )
         {
            pValue = pToken;
            *piValue = pValue->value[ pToken->len - 1 ] - '0';
         }
      }
      else if( ZH_PP_TOKEN_ISEOC( pToken->pNext->pNext ) && ! fNum )
      {
         if( ZH_PP_TOKEN_TYPE( pToken->pNext->type ) == ZH_PP_TOKEN_MINUS )
         {
            pValue = pToken;
            *piValue = 0;
         }
         else if( ZH_PP_TOKEN_TYPE( pToken->pNext->type ) == ZH_PP_TOKEN_PLUS )
         {
            pValue = pToken;
            *piValue = 1;
         }
         else if( ZH_PP_TOKEN_TYPE( pToken->pNext->type ) == ZH_PP_TOKEN_NUMBER )
         {
            pValue = pToken;
            *piValue = atoi( pValue->pNext->value );
         }
      }
   }
   return pValue;
}

static void zh_pp_pragmaNew( PZH_PP_STATE pState, PZH_PP_TOKEN pToken )
{
   PZH_PP_TOKEN pValue = NULL;
   ZH_BOOL fError = ZH_FALSE, fValue = ZH_FALSE;
   int iValue = 0;

   if( ! pToken )
      fError = ZH_TRUE;
   else if( pToken->len == 1 && ZH_ISOPTSEP( pToken->value[ 0 ] ) )
   {
      if( ! pState->iCondCompile )
      {
         pToken = pToken->pNext;
         pValue = zh_pp_pragmaGetSwitch( pToken, &iValue );
         if( pValue )
            fError = zh_pp_setCompilerSwitch( pState, pValue->value, iValue );
         else
            fError = ZH_TRUE;
      }
   }
   else if( ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_KEYWORD )
   {
      if( zh_pp_tokenValueCmp( pToken, "begindump", ZH_PP_CMP_DBASE ) )
      {
         pState->iStreamDump = ZH_PP_STREAM_DUMP_C;
         pState->iDumpLine = pState->pFile->iCurrentLine;
         if( ! pState->pDumpBuffer )
            pState->pDumpBuffer = zh_membufNew();
      }
      else if( zh_pp_tokenValueCmp( pToken, "enddump", ZH_PP_CMP_DBASE ) )
      {
         pState->iStreamDump = ZH_PP_STREAM_OFF;
      }
      else if( zh_pp_tokenValueCmp( pToken, "__text", ZH_PP_CMP_DBASE ) )
      {
         fError = zh_pp_pragmaStream( pState, pToken->pNext );
         if( ! fError )
            pState->iStreamDump = ZH_PP_STREAM_TEXT;
      }
      else if( zh_pp_tokenValueCmp( pToken, "__stream", ZH_PP_CMP_DBASE ) )
      {
         fError = zh_pp_pragmaStream( pState, pToken->pNext );
         if( ! fError )
         {
            pState->iStreamDump = ZH_PP_STREAM_PRG;
            if( ! pState->pStreamBuffer )
               pState->pStreamBuffer = zh_membufNew();
         }
      }
      else if( zh_pp_tokenValueCmp( pToken, "__cstream", ZH_PP_CMP_DBASE ) )
      {
         fError = zh_pp_pragmaStream( pState, pToken->pNext );
         if( ! fError )
         {
            pState->iStreamDump = ZH_PP_STREAM_C;
            if( ! pState->pStreamBuffer )
               pState->pStreamBuffer = zh_membufNew();
         }
      }
      else if( zh_pp_tokenValueCmp( pToken, "__streaminclude", ZH_PP_CMP_DBASE ) )
      {
         if( pToken->pNext && ZH_PP_TOKEN_TYPE( pToken->pNext->type ) == ZH_PP_TOKEN_STRING )
         {
            fError = zh_pp_pragmaStream( pState, pToken->pNext->pNext );
            if( ! fError && ! pState->iCondCompile )
            {
               pState->iStreamDump = ZH_PP_STREAM_PRG;
               zh_pp_pragmaStreamFile( pState, pToken->pNext->value );
               pState->iStreamDump = ZH_PP_STREAM_OFF;
            }
         }
         else
            fError = ZH_TRUE;
      }
      else if( zh_pp_tokenValueCmp( pToken, "__cstreaminclude", ZH_PP_CMP_DBASE ) )
      {
         if( pToken->pNext && ZH_PP_TOKEN_TYPE( pToken->pNext->type ) == ZH_PP_TOKEN_STRING )
         {
            fError = zh_pp_pragmaStream( pState, pToken->pNext->pNext );
            if( ! fError && ! pState->iCondCompile )
            {
               pState->iStreamDump = ZH_PP_STREAM_C;
               zh_pp_pragmaStreamFile( pState, pToken->pNext->value );
               pState->iStreamDump = ZH_PP_STREAM_OFF;
            }
         }
         else
            fError = ZH_TRUE;
      }
      else if( zh_pp_tokenValueCmp( pToken, "__binarystreaminclude", ZH_PP_CMP_DBASE ) )
      {
         if( pToken->pNext && ZH_PP_TOKEN_TYPE( pToken->pNext->type ) == ZH_PP_TOKEN_STRING )
         {
            fError = zh_pp_pragmaStream( pState, pToken->pNext->pNext );
            if( ! fError && ! pState->iCondCompile )
            {
               pState->iStreamDump = ZH_PP_STREAM_BINARY;
               zh_pp_pragmaStreamFile( pState, pToken->pNext->value );
               pState->iStreamDump = ZH_PP_STREAM_OFF;
            }
         }
         else
            fError = ZH_TRUE;
      }
      else if( zh_pp_tokenValueCmp( pToken, "__endtext", ZH_PP_CMP_DBASE ) )
      {
         pState->iStreamDump = ZH_PP_STREAM_OFF;
      }
      else if( pState->iCondCompile )
      {
         /* conditional compilation - other preprocessing and output disabled */
      }
      else if( zh_pp_tokenValueCmp( pToken, "AUTOMEMVAR", ZH_PP_CMP_DBASE ) )
      {
         pValue = zh_pp_pragmaGetLogical( pToken->pNext, &fValue );
         if( pValue )
            fError = zh_pp_setCompilerSwitch( pState, "a", ( int ) fValue );
         else
            fError = ZH_TRUE;
      }
      else if( zh_pp_tokenValueCmp( pToken, "DEBUGINFO", ZH_PP_CMP_DBASE ) )
      {
         pValue = zh_pp_pragmaGetLogical( pToken->pNext, &fValue );
         if( pValue )
            fError = zh_pp_setCompilerSwitch( pState, "b", ( int ) fValue );
         else
            fError = ZH_TRUE;
      }
      else if( zh_pp_tokenValueCmp( pToken, "DYNAMICMEMVAR", ZH_PP_CMP_DBASE ) )
      {
         pValue = zh_pp_pragmaGetLogical( pToken->pNext, &fValue );
         if( pValue )
            fError = zh_pp_setCompilerSwitch( pState, "v", ( int ) fValue );
         else
            fError = ZH_TRUE;
      }
      else if( zh_pp_tokenValueCmp( pToken, "ENABLEWARNINGS", ZH_PP_CMP_DBASE ) )
      {
         pValue = zh_pp_pragmaGetLogical( pToken->pNext, &fValue );
         if( pValue )
            fError = zh_pp_setCompilerSwitch( pState, "w", fValue ? 1 : 0 );
         else
            fError = ZH_TRUE;
      }
      else if( zh_pp_tokenValueCmp( pToken, "ESCAPEDSTRINGS", ZH_PP_CMP_DBASE ) )
      {
         pValue = zh_pp_pragmaGetLogical( pToken->pNext, &pState->fEscStr );
         fError = pValue == NULL;
      }
      else if( zh_pp_tokenValueCmp( pToken, "MULTILINESTRINGS", ZH_PP_CMP_DBASE ) )
      {
         pValue = zh_pp_pragmaGetLogical( pToken->pNext, &pState->fMultiLineStr );
         fError = pValue == NULL;
      }
      else if( zh_pp_tokenValueCmp( pToken, "EXITSEVERITY", ZH_PP_CMP_DBASE ) )
      {
         pValue = zh_pp_pragmaGetInt( pToken->pNext, &iValue );
         if( pValue )
            fError = zh_pp_setCompilerSwitch( pState, "es", iValue );
         else
            fError = ZH_TRUE;
      }
      else if( zh_pp_tokenValueCmp( pToken, "LINENUMBER", ZH_PP_CMP_DBASE ) )
      {
         pValue = zh_pp_pragmaGetLogical( pToken->pNext, &fValue );
         if( pValue )
            fError = zh_pp_setCompilerSwitch( pState, "l", fValue );
         else
            fError = ZH_TRUE;
      }
      else if( zh_pp_tokenValueCmp( pToken, "NOSTARTPROC", ZH_PP_CMP_DBASE ) )
      {
         pValue = zh_pp_pragmaGetInt( pToken->pNext, &iValue );
         if( pValue )
            fError = zh_pp_setCompilerSwitch( pState, "n", iValue );
         else
            fError = ZH_TRUE;
      }
      else if( zh_pp_tokenValueCmp( pToken, "OPERATOR", ZH_PP_CMP_DBASE ) )
      {
         fError = zh_pp_pragmaOperatorNew( pState, pToken->pNext );
      }
      else if( zh_pp_tokenValueCmp( pToken, "PREPROCESSING", ZH_PP_CMP_DBASE ) )
      {
         pValue = zh_pp_pragmaGetLogical( pToken->pNext, &fValue );
         if( pValue )
            fError = zh_pp_setCompilerSwitch( pState, "p", fValue );
         else
            fError = ZH_TRUE;
      }
      else if( zh_pp_tokenValueCmp( pToken, "SHORTCUT", ZH_PP_CMP_DBASE ) )
      {
         pValue = zh_pp_pragmaGetLogical( pToken->pNext, &fValue );
         if( pValue )
            fError = zh_pp_setCompilerSwitch( pState, "z", fValue );
         else
            fError = ZH_TRUE;
      }
      else if( zh_pp_tokenValueCmp( pToken, "RECURSELEVEL", ZH_PP_CMP_DBASE ) )
      {
         pValue = zh_pp_pragmaGetInt( pToken->pNext, &pState->iMaxCycles );
         fError = pValue == NULL;
      }
      /* xZiher extension */
      else if( zh_pp_tokenValueCmp( pToken, "TEXTHIDDEN", ZH_PP_CMP_DBASE ) )
      {
         pValue = zh_pp_pragmaGetInt( pToken->pNext, &iValue );
         if( pValue )
            fError = zh_pp_setCompilerSwitch( pState, pToken->value, iValue );
         else
            fError = ZH_TRUE;
      }
      else if( zh_pp_tokenValueCmp( pToken, "TRACE", ZH_PP_CMP_DBASE ) )
      {
         pValue = zh_pp_pragmaGetLogical( pToken->pNext, &fValue );
         if( pValue )
            fError = zh_pp_setCompilerSwitch( pState, "p+", fValue );
         else
            fError = ZH_TRUE;
      }
      else if( zh_pp_tokenValueCmp( pToken, "TRACEPRAGMAS", ZH_PP_CMP_DBASE ) )
      {
         pValue = zh_pp_pragmaGetLogical( pToken->pNext, &pState->fTracePragmas );
         fError = pValue == NULL;
      }
      else if( zh_pp_tokenValueCmp( pToken, "WARNINGLEVEL", ZH_PP_CMP_DBASE ) )
      {
         pValue = zh_pp_pragmaGetInt( pToken->pNext, &iValue );
         if( pValue )
            fError = zh_pp_setCompilerSwitch( pState, "w", iValue );
         else
            fError = ZH_TRUE;
      }
      else
         fError = ZH_TRUE;
   }
   else
      fError = ZH_TRUE;

   if( pState->iCondCompile )
   {
      ;
   }
   else if( fError )
   {
      zh_pp_error( pState, 'E', ZH_PP_ERR_PRAGMA, NULL );
   }
   else if( pState->fTracePragmas || pState->fWriteTrace )
   {
      char szLine[ 12 ];

      zh_snprintf( szLine, sizeof( szLine ), "%d", pState->pFile->iCurrentLine );
      zh_membufFlush( pState->pBuffer );
      zh_membufAddCh( pState->pBuffer, '(' );
      zh_membufAddStr( pState->pBuffer, szLine );
      zh_membufAddStr( pState->pBuffer, ") #pragma " );
      zh_membufAddStr( pState->pBuffer, pToken->value );
      if( pValue && pValue != pToken )
      {
         zh_membufAddStr( pState->pBuffer, " set to '" );
         zh_membufAddStr( pState->pBuffer, pValue->value );
         zh_membufAddCh( pState->pBuffer, '\'' );
      }
      zh_membufAddCh( pState->pBuffer, '\n' );
      if( pState->fWriteTrace )
      {
         if( fwrite( zh_membufPtr( pState->pBuffer ), sizeof( char ),
                     zh_membufLen( pState->pBuffer ), pState->file_trace ) !=
             zh_membufLen( pState->pBuffer ) )
         {
            zh_pp_error( pState, 'F', ZH_PP_ERR_WRITE_FILE, pState->szTraceFileName );
         }
      }
      if( pState->fTracePragmas )
      {
         zh_membufAddCh( pState->pBuffer, '\0' );
         zh_pp_disp( pState, zh_membufPtr( pState->pBuffer ) );
      }
   }
}

static void zh_pp_defineNew( PZH_PP_STATE pState, PZH_PP_TOKEN pToken, ZH_BOOL fDirect )
{
   PZH_PP_TOKEN pMatch = pToken ? pToken->pNext : NULL;

   if( ! pMatch || ZH_PP_TOKEN_TYPE( pMatch->type ) != ZH_PP_TOKEN_KEYWORD )
   {
      zh_pp_error( pState, 'E', ZH_PP_ERR_DEFINE_SYNTAX, NULL );
   }
   else
   {
      PZH_PP_TOKEN pResult, pLast = pMatch->pNext, pParam;
      PZH_PP_MARKER pMarkers = NULL;
      ZH_USHORT usPCount = 0, usParam;

      /* pseudo function? */
      if( pLast && ZH_PP_TOKEN_TYPE( pLast->type ) == ZH_PP_TOKEN_LEFT_PB &&
          pLast->spaces == 0 )
      {
         ZH_USHORT type = ZH_PP_TOKEN_KEYWORD;
         for( ;; )
         {
            pLast = pLast->pNext;
            if( pLast && ( usPCount == 0 || type == ZH_PP_TOKEN_COMMA ) &&
                ZH_PP_TOKEN_TYPE( pLast->type ) == ZH_PP_TOKEN_RIGHT_PB )
               break;
            if( ! pLast || type != ZH_PP_TOKEN_TYPE( pLast->type ) )
            {
               if( type == ZH_PP_TOKEN_KEYWORD )
                  zh_pp_error( pState, 'E', ZH_PP_ERR_LABEL_MISSING_IN_DEFINE, NULL );
               else
                  zh_pp_error( pState, 'E', ZH_PP_ERR_PARE_MISSING_IN_DEFINE, NULL );
               return;
            }
            else if( type == ZH_PP_TOKEN_KEYWORD )
            {
               ++usPCount;
               type = ZH_PP_TOKEN_COMMA;
            }
            else
               type = ZH_PP_TOKEN_KEYWORD;
         }
      }
      else  /* simple keyword define */
         pLast = pMatch;
      pResult = pLast->pNext;
      pLast->pNext = NULL;
      pToken->pNext = zh_pp_tokenResultEnd( &pResult, fDirect );
      if( usPCount )
      {
         usPCount = 0;
         pParam = pMatch->pNext->pNext;
         while( ZH_PP_TOKEN_TYPE( pParam->type ) == ZH_PP_TOKEN_KEYWORD )
         {
            usParam = 0;
            /* Check if it's not repeated ID */
            pLast = pMatch->pNext->pNext;
            while( pLast != pParam && ! zh_pp_tokenEqual( pParam, pLast, ZH_PP_CMP_CASE ) )
            {
               pLast = pLast->pNext;
            }
            if( pLast == pParam )
            {
               pLast = pResult;
               /* replace parameter tokens in result pattern with regular
                  result markers */
               while( pLast )
               {
                  if( zh_pp_tokenEqual( pParam, pLast, ZH_PP_CMP_CASE ) )
                  {
                     ZH_PP_TOKEN_SETTYPE( pLast, ZH_PP_RMARKER_REGULAR );
                     if( usParam == 0 )
                        usParam = ++usPCount;
                     pLast->index = usParam;
                  }
                  pLast = pLast->pNext;
               }
            }
            ZH_PP_TOKEN_SETTYPE( pParam, ZH_PP_MMARKER_REGULAR );
            pParam->index = usParam;
            pParam = pParam->pNext;
            if( ZH_PP_TOKEN_TYPE( pParam->type ) == ZH_PP_TOKEN_COMMA )
               pParam = pParam->pNext;
         }
         if( usPCount )
         {
            /* create regular match and result markers from parameters */
            pMarkers = ( PZH_PP_MARKER ) zh_xgrabz( usPCount * sizeof( ZH_PP_MARKER ) );
         }
      }
      zh_pp_defineAdd( pState, ZH_PP_CMP_CASE, usPCount, pMarkers, pMatch, pResult );
   }
}

static ZH_BOOL zh_pp_tokenUnQuotedGet( PZH_PP_TOKEN ** pTokenPtr, ZH_BOOL * pfQuoted,
                                       ZH_BOOL fFree )
{
   PZH_PP_TOKEN pToken = **pTokenPtr;

   *pfQuoted = ZH_FALSE;
   if( pToken )
   {
      if( fFree )
      {
         **pTokenPtr = pToken->pNext;
         zh_pp_tokenFree( pToken );
      }
      else
      {
         *pTokenPtr = &pToken->pNext;
      }
      pToken = **pTokenPtr;
      if( pToken )
      {
         if( ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_BACKSLASH )
         {
            *pfQuoted = ZH_TRUE;
            if( pToken->pNext )
               pToken->pNext->spaces = pToken->spaces;
            **pTokenPtr = pToken->pNext;
            zh_pp_tokenFree( pToken );
            pToken = **pTokenPtr;
         }
      }
   }

   return pToken != NULL;
}

static ZH_BOOL zh_pp_matchMarkerNew( PZH_PP_TOKEN * pTokenPtr,
                                     PZH_PP_MARKERLST * pMarkerListPtr )
{
   ZH_USHORT type = ZH_PP_TOKEN_NUL;
   PZH_PP_TOKEN pMarkerId = NULL, pMTokens = NULL;
   ZH_BOOL fQuoted;

   /* At start pTokenPtr points to '<' token */

   if( zh_pp_tokenUnQuotedGet( &pTokenPtr, &fQuoted, ZH_TRUE ) && ! fQuoted )
   {
      if( ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_KEYWORD )
      {
         pMarkerId = *pTokenPtr;
         if( zh_pp_tokenUnQuotedGet( &pTokenPtr, &fQuoted, ZH_FALSE ) && ! fQuoted )
         {
            if( ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_GT )
               type = ZH_PP_MMARKER_REGULAR;
            else if( ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_COMMA )
            {
               int i = 3;
               do
               {
                  if( ! zh_pp_tokenUnQuotedGet( &pTokenPtr, &fQuoted, ZH_TRUE ) || fQuoted )
                     break;
                  if( i == 3 && ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_EPSILON )
                  {
                     i = 0;
                     break;
                  }
                  if( ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) != ZH_PP_TOKEN_DOT )
                     break;
               }
               while( --i > 0 );
               if( i == 0 && zh_pp_tokenUnQuotedGet( &pTokenPtr, &fQuoted, ZH_TRUE ) &&
                   ! fQuoted && ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_GT )
                  type = ZH_PP_MMARKER_LIST;
            }
            else if( ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_SEND )
            {
               if( zh_pp_tokenUnQuotedGet( &pTokenPtr, &fQuoted, ZH_TRUE ) )
               {
                  PZH_PP_TOKEN pLast = NULL;
                  do
                  {
                     if( ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_GT && ! fQuoted )
                     {
                        if( pLast )
                        {
                           pMTokens = pMarkerId->pNext;
                           pMarkerId->pNext = *pTokenPtr;
                           pTokenPtr = &pMarkerId->pNext;
                           pLast->pNext = NULL;
                        }
                        type = ZH_PP_MMARKER_RESTRICT;
                        break;
                     }
                     pLast = *pTokenPtr;
                  }
                  while( zh_pp_tokenUnQuotedGet( &pTokenPtr, &fQuoted, ZH_FALSE ) );
               }
            }
         }
      }
      else if( ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_MULT )
      {
         if( zh_pp_tokenUnQuotedGet( &pTokenPtr, &fQuoted, ZH_TRUE ) && ! fQuoted &&
             ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_KEYWORD )
         {
            pMarkerId = *pTokenPtr;
            if( zh_pp_tokenUnQuotedGet( &pTokenPtr, &fQuoted, ZH_FALSE ) && ! fQuoted &&
                ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_MULT &&
                zh_pp_tokenUnQuotedGet( &pTokenPtr, &fQuoted, ZH_TRUE ) && ! fQuoted &&
                ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_GT )
               type = ZH_PP_MMARKER_WILD;
         }
      }
      else if( ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_LEFT_PB )
      {
         if( zh_pp_tokenUnQuotedGet( &pTokenPtr, &fQuoted, ZH_TRUE ) && ! fQuoted &&
             ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_KEYWORD )
         {
            pMarkerId = *pTokenPtr;
            if( zh_pp_tokenUnQuotedGet( &pTokenPtr, &fQuoted, ZH_FALSE ) && ! fQuoted &&
                ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_RIGHT_PB &&
                zh_pp_tokenUnQuotedGet( &pTokenPtr, &fQuoted, ZH_TRUE ) && ! fQuoted &&
                ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_GT )
               type = ZH_PP_MMARKER_EXTEXP;
         }
      }
      else if( ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_NOT )
      {
         if( zh_pp_tokenUnQuotedGet( &pTokenPtr, &fQuoted, ZH_TRUE ) && ! fQuoted &&
             ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_KEYWORD )
         {
            pMarkerId = *pTokenPtr;
            if( zh_pp_tokenUnQuotedGet( &pTokenPtr, &fQuoted, ZH_FALSE ) && ! fQuoted &&
                ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_NOT &&
                zh_pp_tokenUnQuotedGet( &pTokenPtr, &fQuoted, ZH_TRUE ) && ! fQuoted &&
                ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_GT )
               type = ZH_PP_MMARKER_NAME;
         }
      }
   }

   if( type != ZH_PP_TOKEN_NUL )
   {
      PZH_PP_MARKERLST pMrkLst = *pMarkerListPtr, pMrkPrev = NULL;
      PZH_PP_MARKERPTR pMrkPtr;

      while( pMrkLst && ! zh_pp_tokenEqual( pMrkLst->pMatchMarkers->pToken,
                                            pMarkerId, ZH_PP_CMP_CASE ) )
      {
         pMrkPrev = pMrkLst;
         pMrkLst = pMrkLst->pNext;
      }
      if( ! pMrkLst )
      {
         pMrkLst = ( PZH_PP_MARKERLST ) zh_xgrab( sizeof( ZH_PP_MARKERLST ) );
         if( pMrkPrev )
            pMrkPrev->pNext = pMrkLst;
         else
            *pMarkerListPtr = pMrkLst;
         pMrkLst->pNext = NULL;
         pMrkLst->pMatchMarkers = NULL;
         pMrkLst->canrepeat = ZH_TRUE;
         pMrkLst->index = 0;
      }
      pMrkPtr = ( PZH_PP_MARKERPTR ) zh_xgrab( sizeof( ZH_PP_MARKERPTR ) );
      pMrkPtr->pNext = pMrkLst->pMatchMarkers;
      pMrkLst->pMatchMarkers = pMrkPtr;
      pMrkPtr->pToken = pMarkerId;
      pMrkPtr->pMTokens = pMTokens;
      pMrkPtr->type = type;
      /* mark non restricted markers for later detection two consecutive
         optional match markers */
      if( type != ZH_PP_MMARKER_RESTRICT )
         pMarkerId->type |= ZH_PP_TOKEN_MATCHMARKER;
      /* free the trailing '>' marker token */
      pMTokens = *pTokenPtr;
      *pTokenPtr = pMTokens->pNext;
      zh_pp_tokenFree( pMTokens );
      return ZH_TRUE;
   }
   return ZH_FALSE;
}

static ZH_BOOL zh_pp_matchHasKeywords( PZH_PP_TOKEN pToken )
{

   while( ZH_PP_TOKEN_ISMATCH( pToken ) )
      pToken = pToken->pNext;
   return pToken != NULL;
}

static ZH_BOOL zh_pp_matchPatternNew( PZH_PP_STATE pState, PZH_PP_TOKEN * pTokenPtr,
                                      PZH_PP_MARKERLST * pMarkerListPtr,
                                      PZH_PP_TOKEN ** pOptional )
{
   PZH_PP_TOKEN * pLastPtr = NULL;
   ZH_BOOL fQuoted = ZH_FALSE;

   if( ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_BACKSLASH )
   {
      PZH_PP_TOKEN pToken = *pTokenPtr;
      *pTokenPtr = pToken->pNext;
      zh_pp_tokenFree( pToken );
      fQuoted = ZH_TRUE;
   }

   do
   {
      if( ! fQuoted )
      {
         if( ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_LT )
         {
            if( ! zh_pp_matchMarkerNew( pTokenPtr, pMarkerListPtr ) )
            {
               zh_pp_error( pState, 'E', ZH_PP_ERR_BAD_MATCH_MARKER, NULL );
               return ZH_FALSE;
            }
            /* now pTokenPtr points to marker keyword, all other tokens
               have been stripped */
         }
         else if( ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_RIGHT_SB )
         {
            if( pOptional )
            {
               *pOptional = pTokenPtr;
               return ZH_TRUE;
            }
         }
         else if( ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_LEFT_SB )
         {
            PZH_PP_TOKEN * pStopOptPtr = NULL;
            if( ! ( *pTokenPtr )->pNext )
            {
               /* assign pOptional only to force error below */
               pOptional = &pTokenPtr;
               break;
            }
            else if( ! zh_pp_matchPatternNew( pState, &( *pTokenPtr )->pNext,
                                              pMarkerListPtr, &pStopOptPtr ) )
               return ZH_FALSE;
            else if( *pStopOptPtr == ( *pTokenPtr )->pNext )
            {
               zh_pp_error( pState, 'E', ZH_PP_ERR_EMPTY_OPTIONAL, NULL );
               return ZH_FALSE;
            }
            else
            {
               PZH_PP_TOKEN pToken, pOptTok = ( *pTokenPtr )->pNext;
               pToken = *pStopOptPtr;
               *pStopOptPtr = NULL;
               ( *pTokenPtr )->pNext = pToken->pNext;
               zh_pp_tokenFree( pToken );
               /* create new optional match marker */
               ZH_PP_TOKEN_SETTYPE( *pTokenPtr, ZH_PP_MMARKER_OPTIONAL );
               if( ( *pTokenPtr )->spaces > 1 )
                  ( *pTokenPtr )->spaces = 1;
               ( *pTokenPtr )->type |= ZH_PP_TOKEN_MATCHMARKER;
               ( *pTokenPtr )->pMTokens = pOptTok;
               if( pLastPtr && ! zh_pp_matchHasKeywords( *pLastPtr ) )
               {
                  if( ! zh_pp_matchHasKeywords( pOptTok ) )
                  {
                     zh_pp_error( pState, 'E', ZH_PP_ERR_AMBIGUOUS_MATCH_PATTERN, NULL );
                     return ZH_FALSE;
                  }
                  /* replace the order for these optional tokens to keep
                     the ones with keywords 1st */
                  ( *pTokenPtr )->pMTokens = *pLastPtr;
                  *pLastPtr = pOptTok;
               }
               pLastPtr = &( *pTokenPtr )->pMTokens;
               /* to skip resetting pLastPtr below */
               continue;
            }
         }
      }
      pLastPtr = NULL;
   }
   while( zh_pp_tokenUnQuotedGet( &pTokenPtr, &fQuoted, ZH_FALSE ) );

   if( pOptional )
   {
      zh_pp_error( pState, 'E', ZH_PP_ERR_UNCLOSED_OPTIONAL, NULL );
      return ZH_FALSE;
   }

   return ZH_TRUE;
}

static ZH_BOOL zh_pp_resultMarkerNew( PZH_PP_STATE pState,
                                      PZH_PP_TOKEN * pTokenPtr,
                                      PZH_PP_MARKERLST * pMarkerListPtr,
                                      ZH_BOOL fDump, ZH_BOOL fOptional,
                                      ZH_USHORT * pusPCount, ZH_SIZE spaces )
{
   ZH_USHORT type = ZH_PP_TOKEN_NUL, rtype;
   PZH_PP_TOKEN pMarkerId = NULL, pToken;
   ZH_BOOL fQuoted;

   /* At start pTokenPtr points to '<' token */
   if( zh_pp_tokenUnQuotedGet( &pTokenPtr, &fQuoted, ZH_TRUE ) && ! fQuoted )
   {
      rtype = ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type );
      if( rtype == ZH_PP_TOKEN_KEYWORD || rtype == ZH_PP_TOKEN_STRING )
      {
         pMarkerId = *pTokenPtr;
         if( zh_pp_tokenUnQuotedGet( &pTokenPtr, &fQuoted, ZH_FALSE ) && ! fQuoted &&
             ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_GT )
         {
            if( rtype == ZH_PP_TOKEN_STRING )
            {
               type = ZH_PP_RMARKER_STRSTD;
               ZH_PP_TOKEN_SETTYPE( pMarkerId, ZH_PP_TOKEN_KEYWORD );
            }
            else
               type = fDump ? ZH_PP_RMARKER_STRDUMP : ZH_PP_RMARKER_REGULAR;
         }
      }
      else if( rtype == ZH_PP_TOKEN_LEFT_PB )
      {
         if( zh_pp_tokenUnQuotedGet( &pTokenPtr, &fQuoted, ZH_TRUE ) && ! fQuoted &&
             ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_KEYWORD )
         {
            pMarkerId = *pTokenPtr;
            if( zh_pp_tokenUnQuotedGet( &pTokenPtr, &fQuoted, ZH_FALSE ) && ! fQuoted &&
                ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_RIGHT_PB &&
                zh_pp_tokenUnQuotedGet( &pTokenPtr, &fQuoted, ZH_TRUE ) && ! fQuoted &&
                ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_GT )
               type = ZH_PP_RMARKER_STRSMART;
         }
      }
      else if( rtype == ZH_PP_TOKEN_LEFT_CB )
      {
         if( zh_pp_tokenUnQuotedGet( &pTokenPtr, &fQuoted, ZH_TRUE ) && ! fQuoted &&
             ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_KEYWORD )
         {
            pMarkerId = *pTokenPtr;
            if( zh_pp_tokenUnQuotedGet( &pTokenPtr, &fQuoted, ZH_FALSE ) && ! fQuoted &&
                ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_RIGHT_CB &&
                zh_pp_tokenUnQuotedGet( &pTokenPtr, &fQuoted, ZH_TRUE ) && ! fQuoted &&
                ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_GT )
               type = ZH_PP_RMARKER_BLOCK;
         }
      }
      else if( rtype == ZH_PP_TOKEN_DOT )
      {
         if( zh_pp_tokenUnQuotedGet( &pTokenPtr, &fQuoted, ZH_TRUE ) && ! fQuoted &&
             ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_KEYWORD )
         {
            pMarkerId = *pTokenPtr;
            if( zh_pp_tokenUnQuotedGet( &pTokenPtr, &fQuoted, ZH_FALSE ) && ! fQuoted &&
                ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_DOT &&
                zh_pp_tokenUnQuotedGet( &pTokenPtr, &fQuoted, ZH_TRUE ) && ! fQuoted &&
                ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_GT )
               type = ZH_PP_RMARKER_LOGICAL;
         }
      }
      else if( rtype == ZH_PP_TOKEN_MINUS )
      {
         if( zh_pp_tokenUnQuotedGet( &pTokenPtr, &fQuoted, ZH_TRUE ) && ! fQuoted &&
             ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_KEYWORD )
         {
            pMarkerId = *pTokenPtr;
            if( zh_pp_tokenUnQuotedGet( &pTokenPtr, &fQuoted, ZH_FALSE ) && ! fQuoted )
            {
               /* <-id-> was bad choice for marker type because -> is single
                  ALIAS token so we have to add workaround for it now */
               if( ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_ALIAS ||
                   ( ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_MINUS &&
                     zh_pp_tokenUnQuotedGet( &pTokenPtr, &fQuoted, ZH_TRUE ) && ! fQuoted &&
                     ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_GT ) )
                  type = ZH_PP_RMARKER_NUL;
            }
         }
      }
      else if( rtype == ZH_PP_TOKEN_REFERENCE )
      {
         /* <@> */
         if( zh_pp_tokenUnQuotedGet( &pTokenPtr, &fQuoted, ZH_TRUE ) && ! fQuoted &&
             ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_GT )
            type = ZH_PP_RMARKER_REFERENCE;
      }
   }

   if( type == ZH_PP_TOKEN_NUL )
   {
      zh_pp_error( pState, 'E', ZH_PP_ERR_WRONG_LABEL, NULL );
   }
   else if( type == ZH_PP_RMARKER_REFERENCE )
   {
      zh_pp_tokenSetValue( *pTokenPtr, "~", 1 );
      ZH_PP_TOKEN_SETTYPE( *pTokenPtr, type );
      return ZH_TRUE;
   }
   else
   {
      PZH_PP_MARKERLST pMrkLst = *pMarkerListPtr;

      while( pMrkLst && ! zh_pp_tokenEqual( pMrkLst->pMatchMarkers->pToken,
                                            pMarkerId, ZH_PP_CMP_CASE ) )
      {
         pMrkLst = pMrkLst->pNext;
      }

      if( ! pMrkLst )
      {
         zh_pp_error( pState, 'E', ZH_PP_ERR_UNKNOWN_RESULT_MARKER, NULL );
      }
      else
      {
         if( ! pMrkLst->index )
            pMrkLst->index = ++( *pusPCount );
         if( ! fOptional )
            pMrkLst->canrepeat = ZH_FALSE;
         ZH_PP_TOKEN_SETTYPE( pMarkerId, type );
         pMarkerId->index = pMrkLst->index;
         pMarkerId->spaces = spaces;
         /* free the trailing '>' marker token */
         pToken = *pTokenPtr;
         *pTokenPtr = pToken->pNext;
         zh_pp_tokenFree( pToken );
         return ZH_TRUE;
      }
   }
   return ZH_FALSE;
}

static ZH_BOOL zh_pp_patternCompare( PZH_PP_TOKEN pToken1, PZH_PP_TOKEN pToken2 )
{
   while( pToken1 && pToken2 )
   {
      if( ! zh_pp_tokenEqual( pToken1, pToken2, ZH_PP_CMP_STD ) )
         break;
      if( ZH_PP_TOKEN_TYPE( pToken1->type ) == ZH_PP_MMARKER_RESTRICT ||
          ZH_PP_TOKEN_TYPE( pToken1->type ) == ZH_PP_MMARKER_OPTIONAL ||
          ZH_PP_TOKEN_TYPE( pToken1->type ) == ZH_PP_RMARKER_OPTIONAL )
      {
         if( ! zh_pp_patternCompare( pToken1->pMTokens, pToken2->pMTokens ) )
            break;
      }
      pToken1 = pToken1->pNext;
      pToken2 = pToken2->pNext;
   }
   return ! pToken1 && ! pToken2;
}

static void zh_pp_directiveDel( PZH_PP_STATE pState, PZH_PP_TOKEN pMatch,
                                ZH_USHORT markers, PZH_PP_MARKER pMarkers,
                                ZH_USHORT mode, ZH_BOOL fCommand )
{
   PZH_PP_RULE pRule, * pRulePtr = fCommand ? &pState->pCommands :
                                              &pState->pTranslations;

   while( *pRulePtr )
   {
      pRule = *pRulePtr;
      if( ZH_PP_CMP_MODE( pRule->mode ) == mode && pRule->markers == markers )
      {
         ZH_USHORT u;
         for( u = 0; u < markers; ++u )
         {
            if( pRule->pMarkers[ u ].canrepeat != pMarkers[ u ].canrepeat )
               break;
         }
         if( u == markers && zh_pp_patternCompare( pRule->pMatch, pMatch ) )
         {
            *pRulePtr = pRule->pPrev;
            zh_pp_ruleFree( pRule );
            if( fCommand )
               pState->iCommands--;
            else
               pState->iTranslations--;
            return;
         }
      }
      pRulePtr = &pRule->pPrev;
   }
}

static void zh_pp_directiveNew( PZH_PP_STATE pState, PZH_PP_TOKEN pToken,
                                ZH_USHORT mode, ZH_BOOL fCommand, ZH_BOOL fDirect,
                                ZH_BOOL fDelete )
{
   PZH_PP_TOKEN pResult, pMatch, pStart, pLast;
   ZH_BOOL fValid = ZH_FALSE;


   pMatch = pResult = pLast = NULL;
   if( pToken->pNext )
   {
      pStart = pToken->pNext;
      while( ! ZH_PP_TOKEN_ISEOP( pStart, fDirect ) )
      {
         if( pMatch )
         {
            if( pStart->spaces > 1 )
               pStart->spaces = 1;
         }
         else if( pStart->pNext &&
                  ZH_PP_TOKEN_TYPE( pStart->type ) == ZH_PP_TOKEN_EQ &&
                  ZH_PP_TOKEN_TYPE( pStart->pNext->type ) == ZH_PP_TOKEN_GT )
         {
            fValid = ZH_TRUE;
            if( ! pLast )
               break;

            pLast->pNext = NULL;
            pMatch = pToken->pNext;
            pToken->pNext = pStart;
            pToken = pStart = pStart->pNext;
         }
         pLast = pStart;
         pStart = pStart->pNext;
      }
      if( pMatch && pLast != pToken )
      {
         pLast->pNext = NULL;
         pResult = pToken->pNext;
         pToken->pNext = pStart;
      }
   }

   if( ! fValid )
   {
      zh_pp_error( pState, 'E', ZH_PP_ERR_MISSING_PATTERN_SEP, NULL );
   }
   else if( pMatch ) /* isn't dummy directive? */
   {
      PZH_PP_MARKERLST pMarkerList = NULL, pMrkLst;
      PZH_PP_MARKERPTR pMrkPtr;
      PZH_PP_MARKER pMarkers = NULL;
      ZH_USHORT usPCount = 0;

      fValid = zh_pp_matchPatternNew( pState, &pMatch, &pMarkerList, NULL );
      if( fValid )
      {
         if( pResult )
         {
            PZH_PP_TOKEN * pTokenPtr, * pDumpPtr = NULL, * pOptStart = NULL;
            ZH_BOOL fQuoted = ZH_FALSE;

            if( ZH_PP_TOKEN_TYPE( pResult->type ) == ZH_PP_TOKEN_BACKSLASH )
            {
               fQuoted = ZH_TRUE;
               pLast = pResult;
               pResult = pResult->pNext;
               zh_pp_tokenFree( pLast );
            }
            pTokenPtr = &pResult;
            do
            {
               if( ! fQuoted )
               {
                  if( ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_HASH )
                  {
                     pDumpPtr = pTokenPtr;
                     /* to skip pDumpPtr reseting below */
                     continue;
                  }
                  else if( ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_LT )
                  {
                     ZH_SIZE spaces = ( *pTokenPtr )->spaces;
                     if( pDumpPtr )
                     {
                        pLast = *pDumpPtr;
                        spaces = pLast->spaces;
                        *pDumpPtr = pLast->pNext;
                        zh_pp_tokenFree( pLast );
                        pTokenPtr = pDumpPtr;
                     }

                     if( ! zh_pp_resultMarkerNew( pState, pTokenPtr, &pMarkerList,
                                                  pDumpPtr != NULL, pOptStart != NULL,
                                                  &usPCount, spaces ) )
                     {
                        fValid = ZH_FALSE;
                        break;
                     }
                     /* now pTokenPtr points to marker keyword, all other tokens
                        have been stripped */
                  }
                  else if( ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_LEFT_SB )
                  {
                     if( pOptStart )
                     {
                        fValid = ZH_FALSE;
                        zh_pp_error( pState, 'E', ZH_PP_ERR_NESTED_OPTIONAL, NULL );
                        break;
                     }
                     pOptStart = pTokenPtr;
                  }
                  else if( ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_RIGHT_SB && pOptStart )
                  {
                     pLast      = *pTokenPtr;
                     *pTokenPtr = NULL;
                     ( *pOptStart )->pMTokens = ( *pOptStart )->pNext;
                     ( *pOptStart )->pNext    = pLast->pNext;
                     ZH_PP_TOKEN_SETTYPE( *pOptStart, ZH_PP_RMARKER_OPTIONAL );
                     if( ( *pOptStart )->pMTokens &&
                         ( *pOptStart )->pMTokens->spaces == 0 &&
                         ( *pOptStart )->spaces > 0 &&
                         ZH_PP_TOKEN_TYPE( ( *pOptStart )->pMTokens->type ) !=
                                                            ZH_PP_TOKEN_COMMA )
                        ( *pOptStart )->pMTokens->spaces = 1;
                     pTokenPtr = pOptStart;
                     pOptStart = NULL;
                     zh_pp_tokenFree( pLast );
                  }
               }
               /* reset pDumpPtr */
               pDumpPtr = NULL;
            }
            while( zh_pp_tokenUnQuotedGet( &pTokenPtr, &fQuoted, ZH_FALSE ) );

            if( fValid && pOptStart )
            {
               fValid = ZH_FALSE;
               zh_pp_error( pState, 'E', ZH_PP_ERR_UNKNOWN_RESULT_MARKER, NULL );
            }
         }
      }

      if( fValid && usPCount )
      {
         /* create regular match and result markers from parameters */
         pMarkers = ( PZH_PP_MARKER ) zh_xgrabz( usPCount * sizeof( ZH_PP_MARKER ) );
      }

      /* free marker index list */
      while( pMarkerList )
      {
         pMrkLst = pMarkerList;
         while( pMrkLst->pMatchMarkers )
         {
            pMrkPtr = pMrkLst->pMatchMarkers;
            pMrkLst->pMatchMarkers = pMrkPtr->pNext;
            /* set match token type and parameters */
            if( pMarkers && pMrkLst->index )
            {
               pMarkers[ pMrkLst->index - 1 ].canrepeat = pMrkLst->canrepeat;
               pMrkPtr->pToken->index = pMrkLst->index;
            }
            pMrkPtr->pToken->pMTokens = pMrkPtr->pMTokens;
            ZH_PP_TOKEN_SETTYPE( pMrkPtr->pToken, pMrkPtr->type );
            zh_xfree( pMrkPtr );
         }
         pMarkerList = pMarkerList->pNext;
         zh_xfree( pMrkLst );
      }

      if( fValid )
      {
         if( fDelete )
         {
            zh_pp_directiveDel( pState, pMatch, usPCount, pMarkers, mode, fCommand );
            if( pMarkers )
               zh_xfree( pMarkers );
         }
         else
         {
            PZH_PP_RULE pRule;
            pRule = zh_pp_ruleNew( pMatch, pResult, mode, usPCount, pMarkers );
            if( fCommand )
            {
               pRule->pPrev = pState->pCommands;
               pState->pCommands = pRule;
               pState->iCommands++;
               zh_pp_ruleSetId( pState, pMatch, ZH_PP_COMMAND );
            }
            else
            {
               pRule->pPrev = pState->pTranslations;
               pState->pTranslations = pRule;
               pState->iTranslations++;
               zh_pp_ruleSetId( pState, pMatch, ZH_PP_TRANSLATE );
            }
            pMatch = pResult = NULL;
         }
      }
   }
   zh_pp_tokenListFree( &pMatch );
   zh_pp_tokenListFree( &pResult );
}

static ZH_BOOL zh_pp_tokenStartExtBlock( PZH_PP_TOKEN * pTokenPtr )
{
   PZH_PP_TOKEN pToken = *pTokenPtr;

   if( pToken && ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_LEFT_CB &&
       pToken->pNext && ZH_PP_TOKEN_TYPE( pToken->pNext->type ) == ZH_PP_TOKEN_PIPE )
   {
      ZH_USHORT prevtype = ZH_PP_TOKEN_COMMA;
      pToken = pToken->pNext->pNext;
      while( pToken )
      {
         ZH_USHORT type = ZH_PP_TOKEN_TYPE( pToken->type );
         if( ( ( type == ZH_PP_TOKEN_KEYWORD || type == ZH_PP_TOKEN_EPSILON ) &&
               prevtype == ZH_PP_TOKEN_COMMA ) ||
             ( type == ZH_PP_TOKEN_COMMA && prevtype == ZH_PP_TOKEN_KEYWORD ) )
         {
            prevtype = type;
            pToken = pToken->pNext;
         }
         else
            break;
      }
      if( pToken && pToken->pNext && ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_PIPE &&
          ZH_PP_TOKEN_ISEOC( pToken->pNext ) )
      {
         *pTokenPtr = pToken->pNext;
         return ZH_TRUE;
      }
   }
   return ZH_FALSE;
}

static ZH_BOOL zh_pp_tokenStopExtBlock( PZH_PP_TOKEN * pTokenPtr )
{
   PZH_PP_TOKEN pToken = *pTokenPtr;

   if( ZH_PP_TOKEN_ISEOC( pToken ) && pToken->pNext )
   {
      pToken = pToken->pNext;
      if( ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_RIGHT_CB )
      {
         *pTokenPtr = pToken->pNext;
         return ZH_TRUE;
      }
      if( pToken->pNext &&
          ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_KEYWORD &&
          ZH_PP_TOKEN_TYPE( pToken->pNext->type ) == ZH_PP_TOKEN_KEYWORD )
      {
         PZH_PP_TOKEN pFirst = pToken;

         if( zh_pp_tokenValueCmp( pToken, "INIT", ZH_PP_CMP_DBASE ) ||
             zh_pp_tokenValueCmp( pToken, "EXIT", ZH_PP_CMP_DBASE ) ||
             zh_pp_tokenValueCmp( pToken, "STATIC", ZH_PP_CMP_DBASE ) )
            pToken = pToken->pNext;

         if( zh_pp_tokenValueCmp( pToken, "FUNCTION", ZH_PP_CMP_DBASE ) ||
             zh_pp_tokenValueCmp( pToken, "PROCEDURE", ZH_PP_CMP_DBASE ) )
         {
            if( pToken != pFirst ||
                ZH_PP_TOKEN_TYPE( pToken->pNext->type ) == ZH_PP_TOKEN_KEYWORD )

            *pTokenPtr = pFirst;
            return ZH_TRUE;
         }
      }
   }
   return ZH_FALSE;
}

static ZH_BOOL zh_pp_tokenSkipExp( PZH_PP_TOKEN * pTokenPtr, PZH_PP_TOKEN pStop,
                                   ZH_USHORT mode, ZH_BOOL * pfStop )
{
   ZH_USHORT curtype, prevtype = 0, lbrtype = 0, rbrtype = 0;
   PZH_PP_TOKEN pToken = *pTokenPtr, pPrev;
   int iBraces = 0;
   ZH_BOOL fMatch;

   if( pfStop )
      *pfStop = ZH_FALSE;

   for( ;; )
   {
      pPrev = pToken;
      if( zh_pp_tokenStartExtBlock( &pToken ) )
      {
         int iExtBlock = 1;
         while( pToken )
         {
            if( zh_pp_tokenStartExtBlock( &pToken ) )
               iExtBlock++;
            else if( zh_pp_tokenStopExtBlock( &pToken ) )
            {
               if( --iExtBlock == 0 )
                  break;
            }
            else
               pToken = pToken->pNext;
         }
         if( iExtBlock )
            pToken = pPrev;
      }

      if( mode == ZH_PP_CMP_ADDR ? pToken == pStop :
                                   ZH_PP_TOKEN_ISEOC( pToken ) )
      {
         if( pfStop )
            *pfStop = ZH_TRUE;
         break;
      }
      curtype = ZH_PP_TOKEN_TYPE( pToken->type );
      if( iBraces )
      {
         if( curtype == lbrtype )
            ++iBraces;
         else if( curtype == rbrtype )
            --iBraces;
      }
      else if( curtype == ZH_PP_TOKEN_COMMA )
      {
         if( pfStop )
         {
            if( mode != ZH_PP_CMP_ADDR && ZH_PP_TOKEN_NEEDRIGHT( prevtype ) )
               *pfStop = ZH_TRUE;
            else
               pToken = pToken->pNext;
         }
         break;
      }
      else if( mode != ZH_PP_CMP_ADDR &&
               ( ZH_PP_TOKEN_CLOSE_BR( curtype ) ||
                 ( ! ZH_PP_TOKEN_CANJOIN( curtype ) &&
                   ! ZH_PP_TOKEN_CANJOIN( prevtype ) ) ||
                 ( ZH_PP_TOKEN_NEEDRIGHT( prevtype ) &&
                   ! ZH_PP_TOKEN_ISEXPTOKEN( pToken ) ) ||
                 ( pStop && zh_pp_tokenEqual( pToken, pStop, mode ) ) ) )
      {
         if( pfStop )
            *pfStop = ZH_TRUE;
         break;
      }
      else if( ZH_PP_TOKEN_OPEN_BR( curtype ) )
      {
         lbrtype = curtype;
         rbrtype = ( curtype == ZH_PP_TOKEN_LEFT_PB ? ZH_PP_TOKEN_RIGHT_PB :
                   ( curtype == ZH_PP_TOKEN_LEFT_SB ? ZH_PP_TOKEN_RIGHT_SB :
                                                      ZH_PP_TOKEN_RIGHT_CB ) );
         ++iBraces;
      }
      if( ! ZH_PP_TOKEN_ISNEUTRAL( curtype ) )
         prevtype = curtype;
      pToken = pToken->pNext;
   }

   fMatch = pToken != *pTokenPtr;
   *pTokenPtr = pToken;

   return fMatch;
}

static ZH_BOOL zh_pp_tokenCanStartExp( PZH_PP_TOKEN pToken )
{
   if( ! ZH_PP_TOKEN_NEEDLEFT( pToken ) && ! ZH_PP_TOKEN_ISEOC( pToken ) )
   {
      if( ZH_PP_TOKEN_TYPE( pToken->type ) != ZH_PP_TOKEN_LEFT_SB )
         return ZH_TRUE;
      else
      {
         PZH_PP_TOKEN pEoc = NULL;

         pToken = pToken->pNext;
         while( ! ZH_PP_TOKEN_ISEOL( pToken ) )
         {
            if( ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_RIGHT_SB )
            {
               if( pEoc )
               {
                  do
                  {
                     if( ZH_PP_TOKEN_TYPE( pEoc->type ) == ZH_PP_TOKEN_EOC )
                        ZH_PP_TOKEN_SETTYPE( pEoc, ZH_PP_TOKEN_TEXT );
                     pEoc = pEoc->pNext;
                  }
                  while( pEoc != pToken );
               }
               return ZH_TRUE;
            }
            if( ! pEoc && ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_EOC )
               pEoc = pToken;
            pToken = pToken->pNext;
         }
      }
   }
   return ZH_FALSE;
}

static ZH_BOOL zh_pp_tokenMatch( PZH_PP_TOKEN pMatch, PZH_PP_TOKEN * pTokenPtr,
                                 PZH_PP_TOKEN pStop, ZH_USHORT mode )
{
   ZH_BOOL fMatch = ZH_FALSE;
   ZH_USHORT type;

   type = ZH_PP_TOKEN_TYPE( pMatch->type );
   if( type == ZH_PP_MMARKER_REGULAR )
   {
      if( zh_pp_tokenCanStartExp( *pTokenPtr ) )
      {
         if( ! pStop )
            pStop = pMatch->pNext;
         fMatch = zh_pp_tokenSkipExp( pTokenPtr, pStop, mode, NULL );
      }
   }
   else if( type == ZH_PP_MMARKER_LIST )
   {
      if( zh_pp_tokenCanStartExp( *pTokenPtr ) )
      {
         ZH_BOOL fStop = ZH_FALSE;
         if( ! pStop )
            pStop = pMatch->pNext;
         do
         {
            if( ! zh_pp_tokenSkipExp( pTokenPtr, pStop, mode, &fStop ) )
               break;
            fMatch = ZH_TRUE;
         }
         while( ! fStop );
      }
   }
   else if( type == ZH_PP_MMARKER_RESTRICT )
   {
      PZH_PP_TOKEN pRestrict = pMatch->pMTokens, pToken = *pTokenPtr;

      while( pRestrict )
      {
         if( ZH_PP_TOKEN_TYPE( pRestrict->type ) == ZH_PP_TOKEN_COMMA )
         {
            *pTokenPtr = pToken;
            fMatch = ZH_TRUE;
            break;
         }
         else if( ZH_PP_TOKEN_TYPE( pRestrict->type ) == ZH_PP_TOKEN_AMPERSAND &&
                  ( ! pRestrict->pNext ||
                    ZH_PP_TOKEN_TYPE( pRestrict->pNext->type ) == ZH_PP_TOKEN_COMMA ) &&
                  ( ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_MACROVAR ||
                    ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_MACROTEXT ||
                    ( ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_AMPERSAND &&
                      pToken->pNext &&
                      ZH_PP_TOKEN_TYPE( pToken->pNext->type ) == ZH_PP_TOKEN_LEFT_PB ) ) )
         {
            if( ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_MACROVAR ||
                ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_MACROTEXT )
            {
               *pTokenPtr = pToken->pNext;
            }
            else
            {
               int iBraces = 1;
               pToken = pToken->pNext->pNext;
               while( iBraces > 0 && ! ZH_PP_TOKEN_ISEOC( pToken ) )
               {
                  if( ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_LEFT_PB )
                     ++iBraces;
                  else if( ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_RIGHT_PB )
                     --iBraces;
                  pToken = pToken->pNext;
               }
               *pTokenPtr = pToken;
            }
            fMatch = ZH_TRUE;
            break;
         }
         else if( ! ZH_PP_TOKEN_ISEOC( pToken ) &&
                  zh_pp_tokenEqual( pToken, pRestrict, mode ) )
         {
            pToken = pToken->pNext;
            pRestrict = pRestrict->pNext;
            if( ! pRestrict )
            {
               *pTokenPtr = pToken;
               fMatch = ZH_TRUE;
               break;
            }
         }
         else
         {
            pToken = *pTokenPtr;
            do
            {
               type = ZH_PP_TOKEN_TYPE( pRestrict->type );
               pRestrict = pRestrict->pNext;
            }
            while( pRestrict && type != ZH_PP_TOKEN_COMMA );
         }
      }
   }
   else if( type == ZH_PP_MMARKER_WILD )
   {
      if( ! ZH_PP_TOKEN_ISEOS( *pTokenPtr ) )
      {
         fMatch = ZH_TRUE;
         do
         {
            *pTokenPtr = ( *pTokenPtr )->pNext;
         }
         while( ! ZH_PP_TOKEN_ISEOS( *pTokenPtr ) );
      }
   }
   else if( type == ZH_PP_MMARKER_EXTEXP )
   {
      if( ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) != ZH_PP_TOKEN_RIGHT_PB &&
          ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) != ZH_PP_TOKEN_RIGHT_SB &&
          ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) != ZH_PP_TOKEN_COMMA &&
          zh_pp_tokenCanStartExp( *pTokenPtr ) )
      {
         if( ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_LEFT_PB )
         {
            if( ! pStop )
               pStop = pMatch->pNext;
            fMatch = zh_pp_tokenSkipExp( pTokenPtr, pStop, mode, NULL );
         }
         else
         {
            do
            {
               *pTokenPtr = ( *pTokenPtr )->pNext;
            }
            while( ! ZH_PP_TOKEN_ISEOC( *pTokenPtr ) &&
                   ( *pTokenPtr )->spaces == 0 &&
                   ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) != ZH_PP_TOKEN_COMMA );

            fMatch = ZH_TRUE;
         }
      }
   }
   else if( type == ZH_PP_MMARKER_NAME )
   {
      if( ZH_PP_TOKEN_TYPE( ( *pTokenPtr )->type ) == ZH_PP_TOKEN_KEYWORD )
      {
         *pTokenPtr = ( *pTokenPtr )->pNext;
         fMatch = ZH_TRUE;
      }
   }
   else if( zh_pp_tokenEqual( *pTokenPtr, pMatch, mode ) )
   {
      *pTokenPtr = ( *pTokenPtr )->pNext;
      fMatch = ZH_TRUE;
   }

   return fMatch;
}

static ZH_BOOL zh_pp_patternMatch( PZH_PP_TOKEN pMatch, PZH_PP_TOKEN * pTokenPtr,
                                   PZH_PP_TOKEN pStop,
                                   ZH_USHORT mode, PZH_PP_RULE pRule )
{
   PZH_PP_TOKEN pToken = *pTokenPtr;
   PZH_PP_TOKEN pFirst;
   ZH_BOOL fOverflow = ZH_FALSE;

   while( pMatch && ! ZH_PP_TOKEN_ISEOS( pToken ) )
   {
      if( ZH_PP_TOKEN_TYPE( pMatch->type ) == ZH_PP_MMARKER_OPTIONAL )
      {
         PZH_PP_TOKEN pOptional = pMatch, pLast, pNewStop = pMatch->pNext;

         while( pNewStop && ZH_PP_TOKEN_TYPE( pNewStop->type ) == ZH_PP_MMARKER_OPTIONAL )
            pNewStop = pNewStop->pNext;

         do
         {
            pLast = pOptional;
            pFirst = pToken;
            if( zh_pp_patternMatch( pOptional->pMTokens, &pToken, pNewStop, mode, NULL ) &&
                pFirst != pToken )
            {
               if( pRule && ! zh_pp_patternMatch( pOptional->pMTokens, &pFirst, pNewStop, mode, pRule ) )
               {
                  fOverflow = ZH_TRUE;
                  break;
               }
               pOptional = pMatch;
            }
            else
               pOptional = pOptional->pNext;
         }
         while( pOptional && ZH_PP_TOKEN_TYPE( pOptional->type ) == ZH_PP_MMARKER_OPTIONAL &&
                ! ZH_PP_TOKEN_ISEOS( pToken ) );
         pMatch = pLast;
      }
      else
      {
         pFirst = pToken;
         if( zh_pp_tokenMatch( pMatch, &pToken, pStop, mode ) )
         {
            if( pRule && pMatch->index && pFirst != pToken )
            {
               if( ! zh_pp_patternAddResult( pRule, pMatch->index, pFirst, pToken ) )
               {
                  fOverflow = ZH_TRUE;
                  break;
               }
            }
         }
         else
            break;
      }

      pMatch = pMatch->pNext;
   }

   if( ! fOverflow )
   {
      while( pMatch && ZH_PP_TOKEN_TYPE( pMatch->type ) == ZH_PP_MMARKER_OPTIONAL )
         pMatch = pMatch->pNext;
      if( pMatch == NULL )
      {
         *pTokenPtr = pToken;
         if( pRule )
            pRule->pNextExpr = pToken;
         return ZH_TRUE;
      }
   }
   return ZH_FALSE;
}

static ZH_BOOL zh_pp_patternCmp( PZH_PP_RULE pRule, PZH_PP_TOKEN pToken,
                                 ZH_BOOL fCommand )
{
   PZH_PP_TOKEN pFirst = pToken;

   if( zh_pp_patternMatch( pRule->pMatch, &pToken, NULL,
                           ZH_PP_CMP_MODE( pRule->mode ), NULL ) )
   {
      if( ! fCommand || ZH_PP_TOKEN_ISEOC( pToken ) )
      {
         if( zh_pp_patternMatch( pRule->pMatch, &pFirst, NULL,
                                 ZH_PP_CMP_MODE( pRule->mode ), pRule ) )
            return ZH_TRUE;
         else
            zh_pp_patternClearResults( pRule );
      }
   }
   return ZH_FALSE;
}

static PZH_PP_RESULT zh_pp_matchResultGet( PZH_PP_RULE pRule, ZH_USHORT usMatch,
                                           ZH_USHORT usIndex )
{
   PZH_PP_MARKER pMarker = &pRule->pMarkers[ usIndex - 1];
   PZH_PP_RESULT pMarkerResult;

   if( pMarker->matches == 1 )
      pMarkerResult = pMarker->pResult;
   else if( usMatch < pMarker->matches )
   {
      pMarkerResult = pMarker->pResult;
      while( usMatch-- )
         pMarkerResult = pMarkerResult->pNext;
   }
   else
      pMarkerResult = NULL;

   return pMarkerResult;
}

static PZH_PP_TOKEN * zh_pp_matchResultLstAdd( PZH_PP_STATE pState,
                                               ZH_SIZE spaces, ZH_USHORT type,
                                               PZH_PP_TOKEN * pResultPtr,
                                               PZH_PP_TOKEN pToken,
                                               PZH_PP_TOKEN pStop )
{
   PZH_PP_TOKEN pNext;
   ZH_BOOL fFirst = ZH_TRUE, fStop = ZH_FALSE;

   for( ;; )
   {
      pNext = pToken;
      if( zh_pp_tokenSkipExp( &pNext, pStop, ZH_PP_CMP_ADDR, &fStop ) &&
          ( fStop ? pToken : pToken->pNext ) != pNext )
      {
         /* Check for '&' token followed by single keyword or '('
            token and do not stringify such expressions but
            clone them */
         if( type == ZH_PP_RMARKER_BLOCK )
         {
            ZH_BOOL fBlock = ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_LEFT_CB &&
                             pToken->pNext &&
                             ( fStop ? pToken->pNext : pToken->pNext->pNext ) != pNext &&
                             ZH_PP_TOKEN_TYPE( pToken->pNext->type ) == ZH_PP_TOKEN_PIPE;

            if( ! fBlock )
            {
               zh_pp_tokenAdd( &pResultPtr, "{", 1, fFirst ? spaces : 1,
                               ZH_PP_TOKEN_LEFT_CB | ZH_PP_TOKEN_STATIC );
               zh_pp_tokenAdd( &pResultPtr, "|", 1, 0, ZH_PP_TOKEN_PIPE | ZH_PP_TOKEN_STATIC );
               zh_pp_tokenAdd( &pResultPtr, "|", 1, 0, ZH_PP_TOKEN_PIPE | ZH_PP_TOKEN_STATIC );
               fFirst = ZH_FALSE;
            }
            do
            {
               *pResultPtr = zh_pp_tokenClone( pToken );
               if( fFirst )
               {
                  ( *pResultPtr )->spaces = spaces;
                  fFirst = ZH_FALSE;
               }
               pResultPtr = &( *pResultPtr )->pNext;
               pToken = pToken->pNext;
            }
            while( ( fStop ? pToken : pToken->pNext ) != pNext );
            if( ! fBlock )
               zh_pp_tokenAdd( &pResultPtr, "}", 1, 0, ZH_PP_TOKEN_RIGHT_CB | ZH_PP_TOKEN_STATIC );
         }
         else if( ( ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_MACROVAR ||
                    ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_MACROTEXT ) &&
                  ( fStop ? pToken->pNext : pToken->pNext->pNext ) == pNext )
         {
            if( ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_MACROVAR )
            {
               zh_pp_tokenAdd( &pResultPtr, pToken->value + 1, pToken->len -
                               ( pToken->value[ pToken->len - 1 ] == '.' ? 2 : 1 ),
                               fFirst ? spaces : pToken->spaces,
                               ZH_PP_TOKEN_KEYWORD );
            }
            else
            {
               zh_membufFlush( pState->pBuffer );
               zh_pp_tokenStr( pToken, pState->pBuffer, ZH_FALSE, ZH_FALSE, 0 );
               zh_pp_tokenAdd( &pResultPtr,
                               zh_membufPtr( pState->pBuffer ),
                               zh_membufLen( pState->pBuffer ),
                               fFirst ? spaces : pToken->spaces,
                               ZH_PP_TOKEN_STRING );
            }
            pToken = pToken->pNext;
            fFirst = ZH_FALSE;
         }
         else if( ( type == ZH_PP_RMARKER_STRSMART &&
                    ( ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_STRING ||
                      ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_LEFT_PB ) ) ||
                  ( ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_AMPERSAND &&
                    pToken->pNext &&
                    ( fStop ? pToken->pNext : pToken->pNext->pNext ) != pNext &&
                    ZH_PP_TOKEN_TYPE( pToken->pNext->type ) == ZH_PP_TOKEN_LEFT_PB ) )
         {
            if( ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_AMPERSAND )
               pToken = pToken->pNext;
            do
            {
               *pResultPtr = zh_pp_tokenClone( pToken );
               if( fFirst )
               {
                  ( *pResultPtr )->spaces = spaces;
                  fFirst = ZH_FALSE;
               }
               pResultPtr = &( *pResultPtr )->pNext;
               pToken = pToken->pNext;
            }
            while( ( fStop ? pToken : pToken->pNext ) != pNext );
         }
         else
         {
            ZH_BOOL fSpaces = ZH_FALSE;
            if( ! fFirst )
               spaces = pToken->spaces;
            zh_membufFlush( pState->pBuffer );
            do
            {
               zh_pp_tokenStr( pToken, pState->pBuffer, fSpaces, ZH_FALSE, 0 );
               fSpaces = ZH_TRUE;
               pToken = pToken->pNext;
            }
            while( ( fStop ? pToken : pToken->pNext ) != pNext );
            zh_pp_tokenAdd( &pResultPtr,
                            zh_membufPtr( pState->pBuffer ),
                            zh_membufLen( pState->pBuffer ),
                            spaces, ZH_PP_TOKEN_STRING );
            fFirst = ZH_FALSE;
         }
      }
      if( fStop )
         break;
      /* clone comma token */
      *pResultPtr = zh_pp_tokenClone( pToken );
      if( fFirst )
      {
         ( *pResultPtr )->spaces = spaces;
         fFirst = ZH_FALSE;
      }
      pResultPtr = &( *pResultPtr )->pNext;
      pToken = pNext;
   }

   return pResultPtr;
}

static PZH_PP_TOKEN * zh_pp_matchResultAdd( PZH_PP_STATE pState,
                                            PZH_PP_RULE pRule, PZH_PP_TOKEN * pResultPtr,
                                            PZH_PP_TOKEN pMatch, ZH_USHORT usMatch )
{
   PZH_PP_RESULT pMarkerResult = zh_pp_matchResultGet( pRule, usMatch, pMatch->index );
   PZH_PP_TOKEN pToken, pStop;

   if( ZH_PP_TOKEN_TYPE( pMatch->type ) == ZH_PP_RMARKER_REGULAR )
   {
      if( pMarkerResult )
      {
         ZH_BOOL fFirst = ZH_TRUE;
         pToken = pMarkerResult->pFirstToken;
         pStop = pMarkerResult->pNextExpr;
         if( pToken != pStop )
         {
            do
            {
               *pResultPtr = zh_pp_tokenClone( pToken );
               if( fFirst )
               {
                  ( *pResultPtr )->spaces = pMatch->spaces;
                  fFirst = ZH_FALSE;
               }
               pResultPtr = &( *pResultPtr )->pNext;
               pToken = pToken->pNext;
            }
            while( pToken != pStop );
         }
      }
   }
   else if( ZH_PP_TOKEN_TYPE( pMatch->type ) == ZH_PP_RMARKER_STRDUMP )
   {
      zh_membufFlush( pState->pBuffer );
      if( pMarkerResult )
      {
         pToken = pMarkerResult->pFirstToken;
         pStop = pMarkerResult->pNextExpr;
         if( pToken != pStop )
         {
            ZH_BOOL fSpaces = ZH_FALSE;
            do
            {
               zh_pp_tokenStr( pToken, pState->pBuffer, fSpaces, ZH_FALSE, 0 );
               fSpaces = ZH_TRUE;
               pToken = pToken->pNext;
            }
            while( pToken != pStop );
         }
      }
      zh_pp_tokenAdd( &pResultPtr, zh_membufPtr( pState->pBuffer ),
                      zh_membufLen( pState->pBuffer ),
                      pMatch->spaces, ZH_PP_TOKEN_STRING );
   }
   else if( ZH_PP_TOKEN_TYPE( pMatch->type ) == ZH_PP_RMARKER_STRSTD ||
            ZH_PP_TOKEN_TYPE( pMatch->type ) == ZH_PP_RMARKER_STRSMART ||
            ZH_PP_TOKEN_TYPE( pMatch->type ) == ZH_PP_RMARKER_BLOCK )
   {
      if( pMarkerResult )
      {
         pToken = pMarkerResult->pFirstToken;
         pStop = pMarkerResult->pNextExpr;
         /* We have to divide the expression to comma separated ones */
         if( pToken != pStop )
         {
            pResultPtr = zh_pp_matchResultLstAdd( pState, pMatch->spaces,
                  ZH_PP_TOKEN_TYPE( pMatch->type ), pResultPtr, pToken, pStop );
         }
      }
   }
   else if( ZH_PP_TOKEN_TYPE( pMatch->type ) == ZH_PP_RMARKER_LOGICAL )
   {
      zh_pp_tokenAdd( &pResultPtr, pMarkerResult ? ".T." : ".F.", 3,
               pMatch->spaces, ZH_PP_TOKEN_LOGICAL | ZH_PP_TOKEN_STATIC );
   }
   else if( ZH_PP_TOKEN_TYPE( pMatch->type ) == ZH_PP_RMARKER_NUL )
   {
      /* nothing to stuff */
   }
   else
   {
      /* TODO? internal error? */
   }

   return pResultPtr;
}

static PZH_PP_TOKEN *  zh_pp_patternStuff( PZH_PP_STATE pState,
                                           PZH_PP_RULE pRule, ZH_USHORT usMatch,
                                           PZH_PP_TOKEN pResultPattern,
                                           PZH_PP_TOKEN * pResultPtr )
{
   while( pResultPattern )
   {
      if( pResultPattern->index )
      {
         pResultPtr = zh_pp_matchResultAdd( pState, pRule, pResultPtr, pResultPattern, usMatch );
      }
      else if( ZH_PP_TOKEN_TYPE( pResultPattern->type ) == ZH_PP_RMARKER_OPTIONAL )
      {
         ZH_USHORT usMaxMatch = 0, matches;
         PZH_PP_TOKEN pToken = pResultPattern->pMTokens;
         while( pToken )
         {
            if( pToken->index )
            {
               matches = pRule->pMarkers[ pToken->index - 1 ].matches;
               if( matches > usMaxMatch )
                  usMaxMatch = matches;
            }
            pToken = pToken->pNext;
         }
         for( matches = 0; matches < usMaxMatch; ++matches )
         {
            pResultPtr = zh_pp_patternStuff( pState, pRule, matches,
                                             pResultPattern->pMTokens,
                                             pResultPtr );
         }
      }
      else if( ZH_PP_TOKEN_TYPE( pResultPattern->type ) == ZH_PP_RMARKER_DYNVAL )
      {
         if( zh_pp_tokenValueCmp( pResultPattern, "__FILE__", ZH_PP_CMP_CASE ) )
         {
            const char * szFileName = pState->pFile ?
                                      pState->pFile->szFileName : NULL;
            if( ! szFileName )
               szFileName = "";
            *pResultPtr = zh_pp_tokenNew( szFileName, strlen( szFileName ), 0,
                                          ZH_PP_TOKEN_STRING );
            pResultPtr = &( *pResultPtr )->pNext;
         }
         else if( zh_pp_tokenValueCmp( pResultPattern, "__LINE__", ZH_PP_CMP_CASE ) )
         {
            char line[ 16 ];
            zh_snprintf( line, sizeof( line ), "%d",
                         pState->pFile ? pState->pFile->iCurrentLine : 0 );
            *pResultPtr = zh_pp_tokenNew( line, strlen( line ), 0,
                                          ZH_PP_TOKEN_NUMBER );
            pResultPtr = &( *pResultPtr )->pNext;
         }
      }
      else if( ZH_PP_TOKEN_TYPE( pResultPattern->type ) == ZH_PP_RMARKER_REFERENCE )
      {
         PZH_PP_TOKEN * pTokenPtr = pResultPtr;
         zh_pp_tokenAdd( &pResultPtr, "<@>", 3, pResultPattern->spaces,
                         ZH_PP_RMARKER_REFERENCE | ZH_PP_TOKEN_STATIC );
         ( *pTokenPtr )->pMTokens = pRule->pMatch;
      }
      else
      {
         *pResultPtr = zh_pp_tokenClone( pResultPattern );
         pResultPtr = &( *pResultPtr )->pNext;
      }
      pResultPattern = pResultPattern->pNext;
   }

   return pResultPtr;
}

static char * zh_pp_tokenListStr( PZH_PP_TOKEN pToken, PZH_PP_TOKEN pStop,
                                  ZH_BOOL fStop, PZH_MEM_BUFFER pBuffer,
                                  ZH_BOOL fQuote, ZH_BOOL fEol )
{
   ZH_USHORT ltype = ZH_PP_TOKEN_NUL;
   ZH_BOOL fSpaces = ZH_FALSE;

   zh_membufFlush( pBuffer );
   while( pToken && ( fStop ? pToken != pStop : ! ZH_PP_TOKEN_ISEOC( pToken ) ) )
   {
      zh_pp_tokenStr( pToken, pBuffer, fSpaces, fQuote, ltype );
      ltype = ZH_PP_TOKEN_TYPE( pToken->type );
      fSpaces = ZH_TRUE;
      pToken = pToken->pNext;
   }
   if( fEol )
      zh_membufAddCh( pBuffer, '\n' );
   zh_membufAddCh( pBuffer, '\0' );

   return zh_membufPtr( pBuffer );
}

static void zh_pp_patternReplace( PZH_PP_STATE pState, PZH_PP_RULE pRule,
                                  PZH_PP_TOKEN * pTokenPtr, const char * szType )
{
   PZH_PP_TOKEN pFinalResult = NULL, * pResultPtr, pSource;

   pResultPtr = zh_pp_patternStuff( pState, pRule, 0, pRule->pResult, &pFinalResult );

   /* store original matched token pointer */
   pSource = *pTokenPtr;

   /* Copy number of leading spaces from the first matched token
      to the first result token */
   if( pFinalResult && pSource )
      pFinalResult->spaces = pSource->spaces;

   /* Write trace information */
   if( pState->fWriteTrace )
   {
      fprintf( pState->file_trace, "%s(%d) >%s<\n",
               pState->pFile && pState->pFile->szFileName ? pState->pFile->szFileName : "",
               pState->pFile ? pState->pFile->iCurrentLine : 0,
               /* the source string */
               zh_pp_tokenListStr( pSource, pRule->pNextExpr, ZH_TRUE,
                                   pState->pBuffer, ZH_TRUE, ZH_FALSE ) );
      fprintf( pState->file_trace, "#%s%s >%s<\n",
               pRule->mode == ZH_PP_CMP_STD ? "x" : "", szType,
               /* the result string */
               zh_pp_tokenListStr( pFinalResult, *pResultPtr, ZH_TRUE,
                                   pState->pBuffer, ZH_TRUE, ZH_FALSE ) );
   }

   /* Replace matched tokens with result pattern */
   *pResultPtr = pRule->pNextExpr;
   *pTokenPtr = pFinalResult;

   /* Free the matched tokens */
   while( pSource != pRule->pNextExpr )
   {
      PZH_PP_TOKEN pToken = pSource;
      pSource = pSource->pNext;
      zh_pp_tokenFree( pToken );
   }

   zh_pp_patternClearResults( pRule );
}

static void zh_pp_processCondDefined( PZH_PP_STATE pState, PZH_PP_TOKEN pToken )
{
   PZH_PP_TOKEN pNext;

   while( ! ZH_PP_TOKEN_ISEOS( pToken ) )
   {
      pNext = pToken->pNext;
      if( ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_KEYWORD &&
          ( zh_pp_tokenValueCmp( pToken, "defined", ZH_PP_CMP_CASE ) ||
            zh_pp_tokenValueCmp( pToken, "__pragma", ZH_PP_CMP_CASE ) ) &&
          pNext && ZH_PP_TOKEN_TYPE( pNext->type ) == ZH_PP_TOKEN_LEFT_PB &&
          pNext->pNext && ZH_PP_TOKEN_TYPE( pNext->pNext->type ) == ZH_PP_TOKEN_KEYWORD &&
          pNext->pNext->pNext && ZH_PP_TOKEN_TYPE( pNext->pNext->pNext->type ) == ZH_PP_TOKEN_RIGHT_PB )
      {
         const char * szValue = NULL;
         char buffer[ 32 ];

         if( pToken->value[ 0 ] == '_' )
         {
            const char * szSwitch;

            if( zh_pp_tokenValueCmp( pNext->pNext, "AUTOMEMVAR", ZH_PP_CMP_DBASE ) )
               szSwitch = "a";
            else if( zh_pp_tokenValueCmp( pNext->pNext, "DEBUGINFO", ZH_PP_CMP_DBASE ) )
               szSwitch = "b";
            else if( zh_pp_tokenValueCmp( pNext->pNext, "DYNAMICMEMVAR", ZH_PP_CMP_DBASE ) )
               szSwitch = "v";
            else if( zh_pp_tokenValueCmp( pNext->pNext, "EXITSEVERITY", ZH_PP_CMP_DBASE ) )
               szSwitch = "es";
            else if( zh_pp_tokenValueCmp( pNext->pNext, "LINENUMBER", ZH_PP_CMP_DBASE ) )
               szSwitch = "l";
            else if( zh_pp_tokenValueCmp( pNext->pNext, "NOSTARTPROC", ZH_PP_CMP_DBASE ) )
               szSwitch = "n";
            else if( zh_pp_tokenValueCmp( pNext->pNext, "PREPROCESSING", ZH_PP_CMP_DBASE ) )
               szSwitch = "p";
            else if( zh_pp_tokenValueCmp( pNext->pNext, "SHORTCUT", ZH_PP_CMP_DBASE ) )
               szSwitch = "z";
            else if( zh_pp_tokenValueCmp( pNext->pNext, "TEXTHIDDEN", ZH_PP_CMP_DBASE ) )
               szSwitch = "TEXTHIDDEN";
            else if( zh_pp_tokenValueCmp( pNext->pNext, "TRACE", ZH_PP_CMP_DBASE ) )
               szSwitch = "p+";
            else if( zh_pp_tokenValueCmp( pNext->pNext, "WARNINGLEVEL", ZH_PP_CMP_DBASE ) )
               szSwitch = "w";
            else
               szSwitch = pNext->pNext->value;

            if( szSwitch )
            {
               int iValue = 0;
               if( ! zh_pp_getCompilerSwitch( pState, szSwitch, &iValue ) )
                  szValue = zh_numToStr( buffer, sizeof( buffer ), iValue );
            }
         }
         else
            szValue = zh_pp_defineFind( pState, pNext->pNext ) != NULL ?
                      "1" : "0";

         if( szValue )
         {
            zh_pp_tokenSetValue( pToken, szValue, strlen( szValue ) );
            ZH_PP_TOKEN_SETTYPE( pToken, ZH_PP_TOKEN_NUMBER );
            pToken->pNext = pNext->pNext->pNext->pNext;
            pNext->pNext->pNext->pNext = NULL;
            zh_pp_tokenListFree( &pNext );
         }
      }
      pToken = pToken->pNext;
   }
}

static ZH_BOOL zh_pp_processDefine( PZH_PP_STATE pState, PZH_PP_TOKEN * pFirstPtr )
{
   PZH_PP_TOKEN * pPrevPtr;
   ZH_BOOL fSubst = ZH_FALSE, fRepeat;
   int iCycle = 0;

   do
   {
      pPrevPtr = NULL;
      fRepeat = ZH_FALSE;
      while( ! ZH_PP_TOKEN_ISEOS( *pFirstPtr ) )
      {
         if( ZH_PP_TOKEN_TYPE( ( *pFirstPtr )->type ) == ZH_PP_TOKEN_KEYWORD &&
             ( pState->pMap[ ZH_PP_HASHID( *pFirstPtr ) ] & ZH_PP_DEFINE ) )
         {
            PZH_PP_RULE pRule = zh_pp_defineFind( pState, *pFirstPtr );
            if( pRule )
            {
               if( zh_pp_patternCmp( pRule, *pFirstPtr, ZH_FALSE ) )
               {
                  zh_pp_patternReplace( pState, pRule, pFirstPtr, "define" );
                  fSubst = fRepeat = ZH_TRUE;
                  if( ++pState->iCycle > pState->iMaxCycles ||
                      ++iCycle > ZH_PP_MAX_REPEATS + pState->iDefinitions )
                  {
                     pState->iCycle = pState->iMaxCycles + 1;
                     zh_pp_error( pState, 'E', ZH_PP_ERR_CYCLIC_DEFINE, pRule->pMatch->value );
                     return ZH_TRUE;
                  }
                  continue;
               }
               if( ! pPrevPtr )
                  pPrevPtr = pFirstPtr;
            }
         }
         iCycle = 0;
         pFirstPtr = &( *pFirstPtr )->pNext;
      }
      pFirstPtr = pPrevPtr;
   }
   while( pFirstPtr && fRepeat );

   return fSubst;
}

static ZH_BOOL zh_pp_processTranslate( PZH_PP_STATE pState, PZH_PP_TOKEN * pFirstPtr )
{
   ZH_BOOL fSubst = ZH_FALSE, fRepeat;
   int iCycle = 0;

   do
   {
      PZH_PP_TOKEN * pTokenPtr = pFirstPtr;
      fRepeat = ZH_FALSE;
      while( ! ZH_PP_TOKEN_ISEOS( *pTokenPtr ) )
      {
         if( pState->pMap[ ZH_PP_HASHID( *pTokenPtr ) ] & ZH_PP_TRANSLATE )
         {
            PZH_PP_RULE pRule = pState->pTranslations;
            while( pRule )
            {
               if( zh_pp_patternCmp( pRule, *pTokenPtr, ZH_FALSE ) )
               {
                  zh_pp_patternReplace( pState, pRule, pTokenPtr, "translate" );
                  fSubst = fRepeat = ZH_TRUE;
                  if( ++pState->iCycle > pState->iMaxCycles ||
                      ++iCycle > ZH_PP_MAX_REPEATS + pState->iTranslations )
                  {
                     pState->iCycle = pState->iMaxCycles + 1;
                     zh_pp_error( pState, 'E', ZH_PP_ERR_CYCLIC_TRANSLATE, pRule->pMatch->value );
                     return ZH_TRUE;
                  }
                  pRule = pState->pTranslations;
                  continue;
               }
               pRule = pRule->pPrev;
            }
         }
         iCycle = 0;
         pTokenPtr = &( *pTokenPtr )->pNext;
      }
   }
   while( fRepeat );

   return fSubst;
}

static ZH_BOOL zh_pp_processCommand( PZH_PP_STATE pState, PZH_PP_TOKEN * pFirstPtr )
{
   PZH_PP_RULE pRule;
   ZH_BOOL fSubst = ZH_FALSE, fRepeat = ZH_TRUE;
   int iCycle = 0;

   while( fRepeat && ! ZH_PP_TOKEN_ISEOC( *pFirstPtr ) &&
          ( pState->pMap[ ZH_PP_HASHID( *pFirstPtr ) ] & ZH_PP_COMMAND ) )
   {
      fRepeat = ZH_FALSE;
      pRule = pState->pCommands;
      while( pRule )
      {
         if( zh_pp_patternCmp( pRule, *pFirstPtr, ZH_TRUE ) )
         {
            zh_pp_patternReplace( pState, pRule, pFirstPtr, "command" );
            fSubst = fRepeat = ZH_TRUE;
            if( ++pState->iCycle > pState->iMaxCycles ||
                ++iCycle > ZH_PP_MAX_REPEATS + pState->iCommands )
            {
               pState->iCycle = pState->iMaxCycles + 1;
               zh_pp_error( pState, 'E', ZH_PP_ERR_CYCLIC_COMMAND, pRule->pMatch->value );
               return ZH_TRUE;
            }
            break;
         }
         pRule = pRule->pPrev;
      }
   }

   if( ! ZH_PP_TOKEN_ISEOC( *pFirstPtr ) &&
       zh_pp_tokenValueCmp( *pFirstPtr, "TEXT", ZH_PP_CMP_DBASE ) )
   {
      PZH_PP_TOKEN pToken = ( *pFirstPtr )->pNext, * pFuncPtr;

      if( pToken &&
          ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_KEYWORD &&
          pToken->pNext &&
          ZH_PP_TOKEN_TYPE( pToken->pNext->type ) == ZH_PP_TOKEN_COMMA &&
          pToken->pNext->pNext &&
          ZH_PP_TOKEN_TYPE( pToken->pNext->pNext->type ) == ZH_PP_TOKEN_KEYWORD &&
          ZH_PP_TOKEN_ISEOC( pToken->pNext->pNext->pNext ) )
      {
         zh_pp_tokenListFree( &pState->pFuncOut );
         zh_pp_tokenListFree( &pState->pFuncEnd );

         pFuncPtr = &pState->pFuncOut;
         zh_pp_tokenAdd( &pFuncPtr, pToken->value, pToken->len, 0, ZH_PP_TOKEN_KEYWORD );
         zh_pp_tokenAdd( &pFuncPtr, "(", 1, 0, ZH_PP_TOKEN_LEFT_PB | ZH_PP_TOKEN_STATIC );
         zh_pp_tokenAdd( &pFuncPtr, "%", 1, 1, ZH_PP_RMARKER_STRDUMP | ZH_PP_TOKEN_STATIC );
         zh_pp_tokenAdd( &pFuncPtr, ")", 1, 1, ZH_PP_TOKEN_RIGHT_PB | ZH_PP_TOKEN_STATIC );

         pToken = pToken->pNext->pNext;
         pFuncPtr = &pState->pFuncEnd;
         zh_pp_tokenAdd( &pFuncPtr, pToken->value, pToken->len, 0, ZH_PP_TOKEN_KEYWORD );
         zh_pp_tokenAdd( &pFuncPtr, "(", 1, 0, ZH_PP_TOKEN_LEFT_PB | ZH_PP_TOKEN_STATIC );
         zh_pp_tokenAdd( &pFuncPtr, ")", 1, 1, ZH_PP_TOKEN_RIGHT_PB | ZH_PP_TOKEN_STATIC );
         pState->iStreamDump = ZH_PP_STREAM_TEXT;
         zh_pp_tokenListFreeCmd( pFirstPtr );
         fSubst = ZH_TRUE;
      }
   }

   return fSubst;
}

static ZH_BOOL zh_pp_concatenateKeywords( PZH_PP_STATE pState, PZH_PP_TOKEN * pFirstPtr )
{
   PZH_PP_TOKEN pToken = *pFirstPtr, pNext;
   ZH_BOOL fChanged = ZH_FALSE;

   while( pToken && pToken->pNext )
   {
      pNext = pToken->pNext;
      if( ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_KEYWORD &&
          pNext->spaces == 0 &&
          ZH_PP_TOKEN_TYPE( pNext->type ) == ZH_PP_TOKEN_KEYWORD )
      {
         zh_membufFlush( pState->pBuffer );
         zh_membufAddData( pState->pBuffer, pToken->value, pToken->len );
         zh_membufAddData( pState->pBuffer, pNext->value, pNext->len );

         /* Write trace information */
         if( pState->fWriteTrace )
         {
            fprintf( pState->file_trace, "%s(%d) >%s %s<\n(concatenate) >%s%s<\n",
                     pState->pFile && pState->pFile->szFileName ? pState->pFile->szFileName : "",
                     pState->pFile ? pState->pFile->iCurrentLine : 0,
                     pToken->value, pNext->value,
                     pToken->value, pNext->value );
         }

         zh_pp_tokenSetValue( pToken, zh_membufPtr( pState->pBuffer ),
                                      zh_membufLen( pState->pBuffer ) );
         pToken->pNext = pNext->pNext;
         zh_pp_tokenFree( pNext );
         fChanged = ZH_TRUE;
      }
      else
         pToken = pNext;
   }

   return fChanged;
}

static PZH_PP_TOKEN zh_pp_calcPrecedence( PZH_PP_TOKEN pToken,
                                          int * piNextOper, int * piNextPrec )
{
   PZH_PP_TOKEN pNext = pToken->pNext;

   *piNextOper = ZH_PP_TOKEN_TYPE( pToken->type );
   switch( *piNextOper )
   {
      /* not */
      case ZH_PP_TOKEN_NOT:
         *piNextPrec = ZH_PP_PREC_NOT;
         break;

      case ZH_PP_TOKEN_LT:
      case ZH_PP_TOKEN_GT:
         if( pNext && ZH_PP_TOKEN_TYPE( pNext->type ) == *piNextOper &&
             pNext->spaces == 0 )
         {
            *piNextPrec = ZH_PP_PREC_BIT;
            *piNextOper = *piNextOper == ZH_PP_TOKEN_LT ? ZH_PP_TOKEN_SHIFTL :
                                                          ZH_PP_TOKEN_SHIFTR;
            pNext = pNext->pNext;
            break;
         }
         /* fallthrough */
      /* relational */
      case ZH_PP_TOKEN_EQUAL:
      case ZH_PP_TOKEN_HASH:
      case ZH_PP_TOKEN_NE:
      case ZH_PP_TOKEN_LE:
      case ZH_PP_TOKEN_GE:
         *piNextPrec = ZH_PP_PREC_REL;
         break;

      /* logical */
      case ZH_PP_TOKEN_AND:
      case ZH_PP_TOKEN_OR:
         *piNextPrec = ZH_PP_PREC_LOG;
         break;

      /* bit */
      case ZH_PP_TOKEN_PIPE:
         if( pNext && ZH_PP_TOKEN_TYPE( pNext->type ) == ZH_PP_TOKEN_PIPE &&
             pNext->spaces == 0 )
         {
            *piNextPrec = ZH_PP_PREC_LOG;
            *piNextOper = ZH_PP_TOKEN_OR;
            pNext = pNext->pNext;
         }
         else
            *piNextPrec = ZH_PP_PREC_BIT;
         break;
      case ZH_PP_TOKEN_AMPERSAND:
         /* It will not work because && will be stripped as comment */
         if( pNext && ZH_PP_TOKEN_TYPE( pNext->type ) == ZH_PP_TOKEN_AMPERSAND &&
             pNext->spaces == 0 )
         {
            *piNextPrec = ZH_PP_PREC_LOG;
            *piNextOper = ZH_PP_TOKEN_AND;
            pNext = pNext->pNext;
         }
         else
            *piNextPrec = ZH_PP_PREC_BIT;
         break;
      case ZH_PP_TOKEN_POWER:
         *piNextPrec = ZH_PP_PREC_BIT;
         break;

      case ZH_PP_TOKEN_BITXOR:
      case ZH_PP_TOKEN_SHIFTL:
      case ZH_PP_TOKEN_SHIFTR:
         *piNextPrec = ZH_PP_PREC_BIT;
         break;

      /* math plus/minus */
      case ZH_PP_TOKEN_PLUS:
      case ZH_PP_TOKEN_MINUS:
         *piNextPrec = ZH_PP_PREC_PLUS;
         break;

      /* math mult/div/mode */
      case ZH_PP_TOKEN_MULT:
      case ZH_PP_TOKEN_DIV:
      case ZH_PP_TOKEN_MOD:
         *piNextPrec = ZH_PP_PREC_MULT;
         break;

      default:
         *piNextPrec = ZH_PP_PREC_NUL;
         break;
   }

   return pNext;
}

static ZH_BOOL zh_pp_calcReduce( ZH_MAXINT * plValue, int iOperation )
{
   switch( iOperation )
   {
      case ZH_PP_TOKEN_AND:
         if( *plValue == 0 )
            return ZH_TRUE;
         break;
      case ZH_PP_TOKEN_OR:
         if( *plValue )
         {
            *plValue = 1;
            return ZH_TRUE;
         }
         break;
   }

   return ZH_FALSE;
}

static ZH_MAXINT zh_pp_calcOperation( ZH_MAXINT lValueLeft, ZH_MAXINT lValueRight,
                                      int iOperation, ZH_BOOL * pfError )
{
   switch( iOperation )
   {
      case ZH_PP_TOKEN_EQUAL:
         lValueLeft = ( lValueLeft == lValueRight ) ? 1 : 0;
         break;
      case ZH_PP_TOKEN_HASH:
      case ZH_PP_TOKEN_NE:
         lValueLeft = ( lValueLeft != lValueRight ) ? 1 : 0;
         break;
      case ZH_PP_TOKEN_LE:
         lValueLeft = ( lValueLeft <= lValueRight ) ? 1 : 0;
         break;
      case ZH_PP_TOKEN_GE:
         lValueLeft = ( lValueLeft >= lValueRight ) ? 1 : 0;
         break;
      case ZH_PP_TOKEN_LT:
         lValueLeft = ( lValueLeft < lValueRight ) ? 1 : 0;
         break;
      case ZH_PP_TOKEN_GT:
         lValueLeft = ( lValueLeft > lValueRight ) ? 1 : 0;
         break;

      case ZH_PP_TOKEN_AND:
         lValueLeft = ( lValueLeft && lValueRight ) ? 1 : 0;
         break;
      case ZH_PP_TOKEN_OR:
         lValueLeft = ( lValueLeft || lValueRight ) ? 1 : 0;
         break;

      case ZH_PP_TOKEN_PIPE:
         lValueLeft |= lValueRight;
         break;
      case ZH_PP_TOKEN_AMPERSAND:
         lValueLeft &= lValueRight;
         break;
      case ZH_PP_TOKEN_POWER:
      case ZH_PP_TOKEN_BITXOR:
         lValueLeft ^= lValueRight;
         break;
      case ZH_PP_TOKEN_SHIFTL:
         lValueLeft <<= lValueRight;
         break;
      case ZH_PP_TOKEN_SHIFTR:
         lValueLeft >>= lValueRight;
         break;

      case ZH_PP_TOKEN_PLUS:
         lValueLeft += lValueRight;
         break;
      case ZH_PP_TOKEN_MINUS:
         lValueLeft -= lValueRight;
         break;
      case ZH_PP_TOKEN_MULT:
         lValueLeft *= lValueRight;
         break;
      case ZH_PP_TOKEN_DIV:
         if( lValueRight == 0 )
            *pfError = ZH_TRUE;
         else
            lValueLeft /= lValueRight;
         break;
      case ZH_PP_TOKEN_MOD:
         if( lValueRight == 0 )
            *pfError = ZH_TRUE;
         else
            lValueLeft %= lValueRight;
         break;
   }

   return lValueLeft;
}

static PZH_PP_TOKEN zh_pp_calcValue( PZH_PP_TOKEN pToken, int iPrecedense,
                                     ZH_MAXINT * plValue, ZH_BOOL * pfError,
                                     ZH_BOOL * pfUndef )
{
   if( ZH_PP_TOKEN_ISEOC( pToken ) )
      *pfError = ZH_TRUE;
   else if( ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_MINUS )
   {
      pToken = zh_pp_calcValue( pToken->pNext, ZH_PP_PREC_NEG, plValue, pfError, pfUndef );
      *plValue = - *plValue;
   }
   else if( ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_PLUS )
   {
      pToken = zh_pp_calcValue( pToken->pNext, iPrecedense, plValue, pfError, pfUndef );
   }
   else if( ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_NOT )
   {
      pToken = zh_pp_calcValue( pToken->pNext, ZH_PP_PREC_NOT, plValue, pfError, pfUndef );
      *plValue = *plValue ? 0 : 1;
   }
   else if( ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_LEFT_PB )
   {
      *pfError = ZH_TRUE;
      pToken = zh_pp_calcValue( pToken->pNext, ZH_PP_PREC_NUL, plValue, pfError, pfUndef );
      if( ! *pfError && ! ZH_PP_TOKEN_ISEOC( pToken ) &&
          ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_RIGHT_PB )
         pToken = pToken->pNext;
      else
         *pfError = ZH_TRUE;
   }
   else if( ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_RIGHT_PB )
   {
      return pToken;
   }
   else if( ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_NUMBER )
   {
      int iOverflow;
      *plValue = zh_strValInt( pToken->value, &iOverflow );
      if( iOverflow )
         *pfError = ZH_TRUE;
      else
      {
         *pfError = ZH_FALSE;
         pToken = pToken->pNext;
      }
   }
   else if( ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_LOGICAL )
   {
      *plValue = ZH_PP_ISTRUE( pToken->value[ 1 ] ) ? 1 : 0;
      *pfError = ZH_FALSE;
      pToken = pToken->pNext;
   }
   else if( ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_KEYWORD )
   {
      *plValue = 0;
      pToken = pToken->pNext;
      *pfUndef = ZH_TRUE;
      *pfError = ZH_FALSE;
   }
   else
      *pfError = ZH_TRUE;

   while( ! ( *pfError || ZH_PP_TOKEN_ISEOC( pToken ) ||
              ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_RIGHT_PB ) )
   {
      int iNextOper, iNextPrec;
      PZH_PP_TOKEN pNext;
      pNext = zh_pp_calcPrecedence( pToken, &iNextOper, &iNextPrec );
      if( iNextPrec < ZH_PP_PREC_LOG )
         *pfError = ZH_TRUE;
      else if( iNextPrec > iPrecedense )
      {
         ZH_BOOL fDefined = ( ! *pfUndef ) && zh_pp_calcReduce( plValue, iNextOper );
         ZH_MAXINT lValue = 0;
         *pfError = ZH_TRUE;
         pToken = zh_pp_calcValue( pNext, iNextPrec, &lValue, pfError, pfUndef );
         if( ! *pfError )
            *plValue = zh_pp_calcOperation( *plValue, lValue, iNextOper, pfError );
         if( fDefined )
            *pfUndef = ZH_FALSE;
      }
      else
         break;
   }

   return pToken;
}

static ZH_MAXINT zh_pp_calculateValue( PZH_PP_STATE pState, PZH_PP_TOKEN pToken,
                                       ZH_BOOL fNoError )
{
   ZH_BOOL fError = ZH_TRUE, fUndef = ZH_FALSE;
   ZH_MAXINT lValue = 0;

   pToken = zh_pp_calcValue( pToken, ZH_PP_PREC_NUL, &lValue, &fError, &fUndef );
   if( ! ZH_PP_TOKEN_ISEOC( pToken ) || fUndef )
      fError = ZH_TRUE;

   if( fError )
   {
      if( ! fNoError )
         zh_pp_error( pState, 'E', ZH_PP_ERR_DIRECTIVE_IF, NULL );
      lValue = 0;
   }

   return lValue;
}

static void zh_pp_conditionPush( PZH_PP_STATE pState, ZH_BOOL fCond )
{
   if( pState->iCondCount == pState->iCondStackSize )
   {
      pState->iCondStackSize += 5;
      if( pState->pCondStack )
         pState->pCondStack = ( int * ) zh_xrealloc( pState->pCondStack,
                                 pState->iCondStackSize * sizeof( ZH_BOOL ) );
      else
         pState->pCondStack = ( int * ) zh_xgrab( pState->iCondStackSize *
                                                          sizeof( ZH_BOOL ) );
   }
   pState->pCondStack[ pState->iCondCount++ ] = pState->iCondCompile;
   pState->iCondCompile = pState->iCondCompile ? ZH_PP_COND_DISABLE :
                          ( fCond ? 0 : ZH_PP_COND_ELSE );
}

static void zh_pp_condCompile( PZH_PP_STATE pState, PZH_PP_TOKEN pToken,
                               ZH_BOOL fNot )
{
   if( ! pToken || ZH_PP_TOKEN_TYPE( pToken->type ) != ZH_PP_TOKEN_KEYWORD ||
       ! ZH_PP_TOKEN_ISEOC( pToken->pNext ) )
   {
      zh_pp_error( pState, 'E', ZH_PP_ERR_DIRECTIVE_IFDEF, NULL );
   }
   else
   {
      ZH_BOOL fCond = ZH_FALSE;

      if( pState->iCondCompile == 0 )
      {
         fCond = zh_pp_defineFind( pState, pToken ) != NULL;
         if( ! fNot )
            fCond = ! fCond;
      }
      zh_pp_conditionPush( pState, fCond );
   }
}

static void zh_pp_condCompileIf( PZH_PP_STATE pState, PZH_PP_TOKEN pToken )
{
   /* preprocess all define(s) */
   zh_pp_processCondDefined( pState, pToken->pNext );
   zh_pp_processDefine( pState, &pToken->pNext );
   zh_pp_conditionPush( pState, zh_pp_calculateValue( pState, pToken->pNext,
                                             pState->iCondCompile != 0 ) != 0 );
}

static void zh_pp_condCompileElif( PZH_PP_STATE pState, PZH_PP_TOKEN pToken )
{
   if( ( pState->iCondCompile & ZH_PP_COND_DISABLE ) == 0 )
   {
      if( pState->iCondCompile )
      {
         /* preprocess all define(s) */
         zh_pp_processCondDefined( pState, pToken->pNext );
         zh_pp_processDefine( pState, &pToken->pNext );
         if( zh_pp_calculateValue( pState, pToken->pNext, ZH_FALSE ) != 0 )
            pState->iCondCompile ^= ZH_PP_COND_ELSE;
      }
      else
         pState->iCondCompile = ZH_PP_COND_DISABLE;
   }
}

static void zh_pp_lineTokens( PZH_PP_TOKEN ** pTokenPtr, const char * szFileName, int iLine )
{
   char szLine[ 12 ];

   zh_snprintf( szLine, sizeof( szLine ), "%d", iLine );
   zh_pp_tokenAdd( pTokenPtr, "#", 1, 0, ZH_PP_TOKEN_DIRECTIVE | ZH_PP_TOKEN_STATIC );
   zh_pp_tokenAdd( pTokenPtr, "line", 4, 0, ZH_PP_TOKEN_KEYWORD | ZH_PP_TOKEN_STATIC );
   zh_pp_tokenAdd( pTokenPtr, szLine, strlen( szLine ), 1, ZH_PP_TOKEN_NUMBER );
   if( szFileName )
      zh_pp_tokenAdd( pTokenPtr, szFileName, strlen( szFileName ), 1, ZH_PP_TOKEN_STRING );
   zh_pp_tokenAdd( pTokenPtr, "\n", 1, 0, ZH_PP_TOKEN_EOL | ZH_PP_TOKEN_STATIC );
}

static void zh_pp_genLineTokens( PZH_PP_STATE pState )
{
   pState->pNextTokenPtr = &pState->pTokenOut;

   if( pState->pFile->fGenLineInfo )
   {
      zh_pp_lineTokens( &pState->pNextTokenPtr, pState->pFile->szFileName,
                                                pState->pFile->iCurrentLine );
      pState->pFile->iLastLine = pState->pFile->iCurrentLine;
      pState->pFile->fGenLineInfo = ZH_FALSE;
   }
   else if( pState->pFile->iLastLine < pState->pFile->iCurrentLine )
   {
      do
      {
         zh_pp_tokenAdd( &pState->pNextTokenPtr, "\n", 1, 0, ZH_PP_TOKEN_EOL | ZH_PP_TOKEN_STATIC );
      }
      while( ++pState->pFile->iLastLine < pState->pFile->iCurrentLine );
   }
   zh_pp_tokenMoveCommand( pState, pState->pNextTokenPtr,
                           &pState->pFile->pTokenList );
}

static void zh_pp_includeFile( PZH_PP_STATE pState, const char * szFileName, ZH_BOOL fSysFile )
{
   if( pState->iFiles >= ZH_PP_MAX_INCLUDED_FILES )
   {
      zh_pp_error( pState, 'F', ZH_PP_ERR_NESTED_INCLUDES, NULL );
   }
   else
   {
      ZH_BOOL fNested = ZH_FALSE;
      PZH_PP_FILE pFile = zh_pp_FileNew( pState, szFileName, fSysFile, &fNested,
                                         NULL, ZH_TRUE, pState->pOpenFunc, ZH_FALSE );
      if( pFile )
      {
#if defined( ZH_PP_STRICT_LINEINFO_TOKEN )
         pState->pNextTokenPtr = &pState->pTokenOut;
         if( pState->pFile->fGenLineInfo )
         {
            zh_pp_lineTokens( &pState->pNextTokenPtr, pState->pFile->szFileName,
                                                      pState->pFile->iCurrentLine );
            pState->pFile->iLastLine = pState->pFile->iCurrentLine;
            pState->pFile->fGenLineInfo = ZH_FALSE;
         }
         zh_pp_lineTokens( &pState->pNextTokenPtr, szFileName, 1 );
#else
         pFile->fGenLineInfo = ZH_TRUE;
#endif
         pFile->pPrev = pState->pFile;
         pState->pFile = pFile;
         pState->iFiles++;
      }
      else if( fNested )
         zh_pp_error( pState, 'F', ZH_PP_ERR_NESTED_INCLUDES, NULL );
      else
         zh_pp_error( pState, 'F', ZH_PP_ERR_CANNOT_OPEN_FILE, szFileName );
   }
}

static void zh_pp_includeClose( PZH_PP_STATE pState )
{
   PZH_PP_FILE pFile = pState->pFile;

   pState->pFile = pFile->pPrev;
   pState->iFiles--;

#if defined( ZH_PP_STRICT_LINEINFO_TOKEN )
   if( pFile->fGenLineInfo )
   {
      pState->pNextTokenPtr = &pState->pTokenOut;
      zh_pp_lineTokens( &pState->pNextTokenPtr, pFile->szFileName, pFile->iCurrentLine + 1 );
   }
#endif
   if( pState->pFile )
      pState->pFile->fGenLineInfo = ZH_TRUE;

   zh_pp_FileFree( pState, pFile, pState->pCloseFunc );
}

static void zh_pp_preprocessToken( PZH_PP_STATE pState )
{
   while( ! pState->pTokenOut && pState->pFile )
   {
      if( ! pState->pFile->pTokenList )
      {
         while( pState->pFile->pLineBuf ? pState->pFile->nLineBufLen != 0 :
                                          ! pState->pFile->fEof )
         {
            zh_pp_getLine( pState );
            if( pState->pFile->pTokenList /* || pState->fError */ )
               break;
         }

         if( ! pState->pFile->pTokenList )
         {
#if 0       /* disabled for files included from buffer */
            if( pState->pFile->pLineBuf )
               break;
#endif
            /* this condition is only for compiler core code compatibility */
            if( ! pState->pFile->pPrev )
               break;
            zh_pp_includeClose( pState );
            continue;
         }
      }

      if( ZH_PP_TOKEN_ISDIRECTIVE( pState->pFile->pTokenList ) )
      {
         ZH_BOOL fError = ZH_FALSE, fDirect;
         /* Store it here to avoid possible problems after #INCLUDE */
         PZH_PP_TOKEN * pFreePtr = &pState->pFile->pTokenList;
         PZH_PP_TOKEN pToken = *pFreePtr;

         fDirect = ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_DIRECTIVE;

         pToken = pToken->pNext;
         if( ! pToken )
         {
            fError = ZH_TRUE;
         }
         else if( fDirect && pState->pFile->iCurrentLine == 1 &&
                  ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_NOT &&
                  pToken->spaces == 0 && pState->pFile->pTokenList->spaces == 0 )
         {
            /* ignore first line if it begins with "#!"
               minor extension which allow to use the same source code
               as scripts in *nix system and compile it, this feature
               will be necessary also when we integrate compiler with ZHVM and
               add support for direct execution compiled .zh files */
         }
         else if( ZH_PP_TOKEN_TYPE( pToken->type ) != ZH_PP_TOKEN_KEYWORD )
         {
            fError = ZH_TRUE;
         }
         else if( zh_pp_tokenValueCmp( pToken, "IFDEF", ZH_PP_CMP_DBASE ) )
         {
            zh_pp_condCompile( pState, pToken->pNext, ZH_TRUE );
         }
         else if( zh_pp_tokenValueCmp( pToken, "IFNDEF", ZH_PP_CMP_DBASE ) )
         {
            zh_pp_condCompile( pState, pToken->pNext, ZH_FALSE );
         }
         else if( zh_pp_tokenValueCmp( pToken, "IF", ZH_PP_CMP_DBASE ) )
         {
            zh_pp_condCompileIf( pState, pToken );
         }
         else if( zh_pp_tokenValueCmp( pToken, "ELIF", ZH_PP_CMP_DBASE ) )
         {
            if( pState->iCondCount )
               zh_pp_condCompileElif( pState, pToken );
            else
               zh_pp_error( pState, 'E', ZH_PP_ERR_DIRECTIVE_ELSE, NULL );
         }
         else if( zh_pp_tokenValueCmp( pToken, "ENDIF", ZH_PP_CMP_DBASE ) )
         {
            if( pState->iCondCount )
               pState->iCondCompile = pState->pCondStack[ --pState->iCondCount ];
            else
               zh_pp_error( pState, 'E', ZH_PP_ERR_DIRECTIVE_ENDIF, NULL );
         }
         else if( zh_pp_tokenValueCmp( pToken, "ELSE", ZH_PP_CMP_DBASE ) )
         {
            if( pState->iCondCount )
               pState->iCondCompile ^= ZH_PP_COND_ELSE;
            else
               zh_pp_error( pState, 'E', ZH_PP_ERR_DIRECTIVE_ELSE, NULL );
         }
         /* #pragma support is always enabled even in strict compatibility
            mode to allow control by programmer some PP issues */
         else if( zh_pp_tokenValueCmp( pToken, "PRAGMA", ZH_PP_CMP_DBASE ) )
         {
            zh_pp_pragmaNew( pState, pToken->pNext );
         }
         else if( pState->iCondCompile )
         {
            /* conditional compilation - other preprocessing and output disabled */
         }
         else if( zh_pp_tokenValueCmp( pToken, "INCLUDE", ZH_PP_CMP_DBASE ) )
         {
            pToken = pToken->pNext;
            if( pToken && ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_STRING )
               zh_pp_includeFile( pState, pToken->value, ZH_FALSE );
            else if( pToken && ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_LT )
            {
               pToken = pToken->pNext;
               zh_membufFlush( pState->pBuffer );
               while( ! ZH_PP_TOKEN_ISEOC( pToken ) &&
                      ZH_PP_TOKEN_TYPE( pToken->type ) != ZH_PP_TOKEN_GT )
               {
                  zh_membufAddData( pState->pBuffer, pToken->value, pToken->len );
                  pToken = pToken->pNext;
               }
               if( zh_membufLen( pState->pBuffer ) > 0 &&
                   ! ZH_PP_TOKEN_ISEOC( pToken ) &&
                   ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_GT )
               {
                  zh_membufAddCh( pState->pBuffer, '\0' );
                  zh_pp_includeFile( pState, zh_membufPtr( pState->pBuffer ), ZH_TRUE );
               }
               else
                  zh_pp_error( pState, 'F', ZH_PP_ERR_WRONG_FILE_NAME, NULL );
            }
            else
               zh_pp_error( pState, 'F', ZH_PP_ERR_WRONG_FILE_NAME, NULL );
         }
         else if( zh_pp_tokenValueCmp( pToken, "REQUIRE", ZH_PP_CMP_STD ) )
         {
            /* do nothing. this directive is processed by hbmk2 to
               pull in external modules. */
         }
         else if( zh_pp_tokenValueCmp( pToken, "STDOUT", ZH_PP_CMP_DBASE ) )
         {
            zh_pp_disp( pState, zh_pp_tokenListStr( pToken->pNext, NULL, ZH_FALSE,
                                                    pState->pBuffer, ZH_FALSE, ZH_TRUE ) );
         }
         else if( zh_pp_tokenValueCmp( pToken, "ERROR", ZH_PP_CMP_DBASE ) )
         {
            zh_pp_error( pState, 'E', ZH_PP_ERR_EXPLICIT,
                         zh_pp_tokenListStr( pToken->pNext, NULL, ZH_FALSE,
                                             pState->pBuffer, ZH_FALSE, ZH_FALSE ) );
         }
         else if( zh_pp_tokenValueCmp( pToken, "WARNING", ZH_PP_CMP_DBASE ) )
         {
            zh_pp_error( pState, 'W', ZH_PP_WARN_EXPLICIT,
                         zh_pp_tokenListStr( pToken->pNext, NULL, ZH_FALSE,
                                             pState->pBuffer, ZH_FALSE, ZH_FALSE ) );
         }
         else if( zh_pp_tokenValueCmp( pToken, "DEFINE", ZH_PP_CMP_DBASE ) )
         {
            zh_pp_defineNew( pState, pToken, fDirect );
         }
         else if( zh_pp_tokenValueCmp( pToken, "UNDEF", ZH_PP_CMP_DBASE ) )
         {
            pToken = pToken->pNext;
            if( ! pToken || ZH_PP_TOKEN_TYPE( pToken->type ) != ZH_PP_TOKEN_KEYWORD ||
                ! ZH_PP_TOKEN_ISEOC( pToken->pNext ) )
               zh_pp_error( pState, 'E', ZH_PP_ERR_DIRECTIVE_UNDEF, NULL );
            else
               zh_pp_defineDel( pState, pToken );
         }
         else if( zh_pp_tokenValueCmp( pToken, "TRANSLATE", ZH_PP_CMP_DBASE ) )
         {
            zh_pp_directiveNew( pState, pToken, ZH_PP_CMP_DBASE, ZH_FALSE, fDirect, ZH_FALSE );
         }
         else if( zh_pp_tokenValueCmp( pToken, "XTRANSLATE", ZH_PP_CMP_DBASE ) )
         {
            zh_pp_directiveNew( pState, pToken, ZH_PP_CMP_STD, ZH_FALSE, fDirect, ZH_FALSE );
         }
         else if( zh_pp_tokenValueCmp( pToken, "YTRANSLATE", ZH_PP_CMP_DBASE ) )
         {
            zh_pp_directiveNew( pState, pToken, ZH_PP_CMP_CASE, ZH_FALSE, fDirect, ZH_FALSE );
         }
         else if( zh_pp_tokenValueCmp( pToken, "COMMAND", ZH_PP_CMP_DBASE ) )
         {
            zh_pp_directiveNew( pState, pToken, ZH_PP_CMP_DBASE, ZH_TRUE, fDirect, ZH_FALSE );
         }
         else if( zh_pp_tokenValueCmp( pToken, "XCOMMAND", ZH_PP_CMP_DBASE ) )
         {
            zh_pp_directiveNew( pState, pToken, ZH_PP_CMP_STD, ZH_TRUE, fDirect, ZH_FALSE );
         }
         else if( zh_pp_tokenValueCmp( pToken, "YCOMMAND", ZH_PP_CMP_DBASE ) )
         {
            zh_pp_directiveNew( pState, pToken, ZH_PP_CMP_CASE, ZH_TRUE, fDirect, ZH_FALSE );
         }
         /* Ziher PP extensions */
         else if( zh_pp_tokenValueCmp( pToken, "UNTRANSLATE", ZH_PP_CMP_DBASE ) )
         {
            zh_pp_directiveNew( pState, pToken, ZH_PP_CMP_DBASE, ZH_FALSE, fDirect, ZH_TRUE );
         }
         else if( zh_pp_tokenValueCmp( pToken, "XUNTRANSLATE", ZH_PP_CMP_DBASE ) )
         {
            zh_pp_directiveNew( pState, pToken, ZH_PP_CMP_STD, ZH_FALSE, fDirect, ZH_TRUE );
         }
         else if( zh_pp_tokenValueCmp( pToken, "YUNTRANSLATE", ZH_PP_CMP_DBASE ) )
         {
            zh_pp_directiveNew( pState, pToken, ZH_PP_CMP_CASE, ZH_FALSE, fDirect, ZH_TRUE );
         }
         else if( zh_pp_tokenValueCmp( pToken, "UNCOMMAND", ZH_PP_CMP_DBASE ) )
         {
            zh_pp_directiveNew( pState, pToken, ZH_PP_CMP_DBASE, ZH_TRUE, fDirect, ZH_TRUE );
         }
         else if( zh_pp_tokenValueCmp( pToken, "XUNCOMMAND", ZH_PP_CMP_DBASE ) )
         {
            zh_pp_directiveNew( pState, pToken, ZH_PP_CMP_STD, ZH_TRUE, fDirect, ZH_TRUE );
         }
         else if( zh_pp_tokenValueCmp( pToken, "YUNCOMMAND", ZH_PP_CMP_DBASE ) )
         {
            zh_pp_directiveNew( pState, pToken, ZH_PP_CMP_CASE, ZH_TRUE, fDirect, ZH_TRUE );
         }
         else if( zh_pp_tokenValueCmp( pToken, "LINE", ZH_PP_CMP_DBASE ) )
         {
            /* ignore #line directives */
         }
         else
            fError = ZH_TRUE;

         if( fError )
            zh_pp_error( pState, 'F', ZH_PP_ERR_INVALID_DIRECTIVE, NULL );
         pState->pFile->iCurrentLine += zh_pp_tokenListFreeCmd( pFreePtr );
         continue;
      }
      else if( pState->iCondCompile )
      {
         pState->pFile->iCurrentLine += zh_pp_tokenListFreeCmd( &pState->pFile->pTokenList );
      }
      else
      {
         ZH_BOOL fDirective = ZH_FALSE;

         pState->iCycle = 0;
         while( ! ZH_PP_TOKEN_ISEOC( pState->pFile->pTokenList ) &&
                pState->iCycle <= pState->iMaxCycles )
         {
            if( ZH_PP_TOKEN_ISDIRECTIVE( pState->pFile->pTokenList ) )
            {
               fDirective = ZH_TRUE;
               break;
            }
            /* Ziher extension: concatenate keywords without spaces between
               them */
            zh_pp_concatenateKeywords( pState, &pState->pFile->pTokenList );
            if( zh_pp_processDefine( pState, &pState->pFile->pTokenList ) )
               continue;
            if( zh_pp_processTranslate( pState, &pState->pFile->pTokenList ) )
               continue;
            if( zh_pp_processCommand( pState, &pState->pFile->pTokenList ) )
               continue;
            break;
         }
         if( ! fDirective && pState->pFile->pTokenList )
            zh_pp_genLineTokens( pState );
      }
   }
}

/*
 * exported functions
 */

/*
 * internal function to initialize predefined PP rules
 */
void zh_pp_initRules( PZH_PP_RULE * pRulesPtr, int * piRules,
                      const ZH_PP_DEFRULE pDefRules[], int iDefRules )
{
   PZH_PP_MARKER pMarkers;
   PZH_PP_RULE pRule;

   zh_pp_ruleListFree( pRulesPtr );
   *piRules = iDefRules;

   while( --iDefRules >= 0 )
   {
      const ZH_PP_DEFRULE * pDefRule = pDefRules + iDefRules;
      if( pDefRule->markers > 0 )
      {
         ZH_USHORT marker;
         ZH_ULONG ulBit;

         pMarkers = ( PZH_PP_MARKER ) zh_xgrabz( pDefRule->markers * sizeof( ZH_PP_MARKER ) );
         for( marker = 0, ulBit = 1; marker < pDefRule->markers; ++marker, ulBit <<= 1 )
         {
            if( pDefRule->repeatbits & ulBit )
               pMarkers[ marker ].canrepeat = ZH_TRUE;
         }
      }
      else
         pMarkers = NULL;
      pRule = zh_pp_ruleNew( pDefRule->pMatch, pDefRule->pResult,
                             pDefRule->mode, pDefRule->markers, pMarkers );
      pRule->pPrev = *pRulesPtr;
      *pRulesPtr = pRule;
   }
}


/*
 * get preprocessed token
 */
PZH_PP_TOKEN zh_pp_tokenGet( PZH_PP_STATE pState )
{
   pState->fError = ZH_FALSE;

   if( pState->pTokenOut )
   {
      PZH_PP_TOKEN pToken = pState->pTokenOut;
      pState->pTokenOut = pToken->pNext;
      zh_pp_tokenFree( pToken );
   }

   for( ;; )
   {
      if( ! pState->pTokenOut )
      {
         zh_pp_preprocessToken( pState );
         if( ! pState->pTokenOut )
            break;
      }
      while( pState->pTokenOut &&
             ZH_PP_TOKEN_TYPE( pState->pTokenOut->type ) ==
                                                   ZH_PP_RMARKER_REFERENCE )
      {
         PZH_PP_TOKEN pToken = pState->pTokenOut;
         pState->pTokenOut = pToken->pNext;
         zh_pp_tokenFree( pToken );
      }
      if( pState->pTokenOut )
         break;
   }

   if( pState->fWritePreprocesed && pState->pTokenOut )
   {
      zh_membufFlush( pState->pBuffer );
      zh_pp_tokenStr( pState->pTokenOut, pState->pBuffer, ZH_TRUE, ZH_TRUE,
                      pState->usLastType );
      pState->usLastType = ZH_PP_TOKEN_TYPE( pState->pTokenOut->type );
      if( fwrite( zh_membufPtr( pState->pBuffer ), sizeof( char ),
                  zh_membufLen( pState->pBuffer ), pState->file_out ) !=
          zh_membufLen( pState->pBuffer ) )
      {
         zh_pp_error( pState, 'F', ZH_PP_ERR_WRITE_FILE, pState->szOutFileName );
      }
   }

   return pState->pTokenOut;
}


/*
 * create new PP context
 */
PZH_PP_STATE zh_pp_new( void )
{
   return zh_pp_stateNew();
}

/*
 * free PP context
 */
void zh_pp_free( PZH_PP_STATE pState )
{
   zh_pp_stateFree( pState );
}

/*
 * initialize PP context
 */
void zh_pp_init( PZH_PP_STATE pState,
                 ZH_BOOL fQuiet, ZH_BOOL fGauge, int iCycles, void * cargo,
                 PZH_PP_OPEN_FUNC  pOpenFunc, PZH_PP_CLOSE_FUNC pCloseFunc,
                 PZH_PP_ERROR_FUNC pErrorFunc, PZH_PP_DISP_FUNC pDispFunc,
                 PZH_PP_DUMP_FUNC pDumpFunc, PZH_PP_INLINE_FUNC pInLineFunc,
                 PZH_PP_SWITCH_FUNC pSwitchFunc )
{
   pState->fQuiet      = pState->fQuietSet = fQuiet;
   pState->fGauge      = fGauge;
   pState->iMaxCycles  = pState->iMaxCyclesSet = ( iCycles > 0 ) ? iCycles : ZH_PP_MAX_CYCLES;
   pState->cargo       = cargo;
   pState->pOpenFunc   = pOpenFunc;
   pState->pCloseFunc  = pCloseFunc;
   pState->pErrorFunc  = pErrorFunc;
   pState->pDispFunc   = pDispFunc;
   pState->pDumpFunc   = pDumpFunc;
   pState->pInLineFunc = pInLineFunc;
   pState->pSwitchFunc = pSwitchFunc;
}

void zh_pp_setIncFunc( PZH_PP_STATE pState, PZH_PP_INC_FUNC pIncFunc )
{
   pState->pIncFunc = pIncFunc;
}

/*
 * reset PP context, used for multiple .zh file compilation
 * with DO ... or *.clp files
 */
void zh_pp_reset( PZH_PP_STATE pState )
{
   pState->fError        = ZH_FALSE;
   pState->iErrors       = 0;
   pState->iLineTot      = 0;
   pState->fEscStr       = ZH_FALSE;
   pState->fMultiLineStr = ZH_FALSE;
   pState->fTracePragmas = ZH_FALSE;
   pState->fQuiet        = pState->fQuietSet;
   pState->iMaxCycles    = pState->iMaxCyclesSet;
   pState->iCondCompile  = 0;
   pState->iCondCount    = 0;
   pState->iStreamDump   = ZH_PP_STREAM_OFF;

   zh_pp_tokenListFree( &pState->pFuncOut );
   zh_pp_tokenListFree( &pState->pFuncEnd );

   zh_pp_InFileFree( pState );
   zh_pp_OutFileFree( pState );
   zh_pp_TraceFileFree( pState );

   if( pState->iOperators > 0 )
   {
      zh_pp_operatorsFree( pState->pOperators, pState->iOperators );
      pState->pOperators = NULL;
      pState->iOperators = 0;
   }

   zh_pp_ruleListNonStdFree( &pState->pDefinitions );
   zh_pp_ruleListNonStdFree( &pState->pTranslations );
   zh_pp_ruleListNonStdFree( &pState->pCommands );
}

/*
 * add search path for included files
 */
void zh_pp_addSearchPath( PZH_PP_STATE pState, const char * szPath, ZH_BOOL fReplace )
{
   if( fReplace && pState->pIncludePath )
   {
      zh_fsFreeSearchPath( pState->pIncludePath );
      pState->pIncludePath = NULL;
   }

   if( szPath && *szPath )
   {
      zh_fsAddSearchPath( szPath, &pState->pIncludePath );
   }
}

/*
 * mark current rules as standard ones
 */
void zh_pp_setStdBase( PZH_PP_STATE pState )
{
   pState->fError = ZH_FALSE;
   zh_pp_ruleListSetStd( pState->pDefinitions );
   zh_pp_ruleListSetStd( pState->pTranslations );
   zh_pp_ruleListSetStd( pState->pCommands );
   memset( pState->pMap, 0, sizeof( pState->pMap ) );
   zh_pp_ruleListSetId( pState, pState->pDefinitions, ZH_PP_DEFINE );
   zh_pp_ruleListSetId( pState, pState->pTranslations, ZH_PP_TRANSLATE );
   zh_pp_ruleListSetId( pState, pState->pCommands, ZH_PP_COMMAND );

   /* clear total number of preprocessed lines so we will report only
    * lines in compiled .zh files
    */
   pState->iLineTot = 0;
}

/*
 * initialize dynamic definitions
 */
void zh_pp_initDynDefines( PZH_PP_STATE pState, ZH_BOOL fArchDefs )
{
   char szResult[ 65 ];
   int iYear, iMonth, iDay, i;
   long lDate, lTime;

   if( fArchDefs )
   {
      static const char * s_szPlatform = "__PLATFORM__%s";

      char szDefine[ 65 ];

      if( zh_verPlatformMacro() )
      {
         zh_snprintf( szDefine, sizeof( szDefine ), s_szPlatform, zh_verPlatformMacro() );
         zh_pp_addDefine( pState, szDefine, NULL );
      }
#if defined( ZH_OS_UNIX )
      zh_snprintf( szDefine, sizeof( szDefine ), s_szPlatform, "UNIX" );
      zh_pp_addDefine( pState, szDefine, NULL );
#endif

      zh_snprintf( szResult, sizeof( szResult ), "%d", ( int ) sizeof( void * ) );
#if defined( ZH_ARCH_16BIT )
      zh_pp_addDefine( pState, "__ARCH16BIT__", szResult );
#elif defined( ZH_ARCH_32BIT )
      zh_pp_addDefine( pState, "__ARCH32BIT__", szResult );
#elif defined( ZH_ARCH_64BIT )
      zh_pp_addDefine( pState, "__ARCH64BIT__", szResult );
#endif

#if defined( ZH_LITTLE_ENDIAN )
      zh_pp_addDefine( pState, "__LITTLE_ENDIAN__", szResult );
#elif defined( ZH_BIG_ENDIAN )
      zh_pp_addDefine( pState, "__BIG_ENDIAN__", szResult );
#elif defined( ZH_PDP_ENDIAN )
      zh_pp_addDefine( pState, "__PDP_ENDIAN__", szResult );
#endif
   }

#if defined( __ZIHER__ )
   zh_snprintf( szResult, sizeof( szResult ), "0x%02X%02X%02X", ZH_VER_MAJOR & 0xFF, ZH_VER_MINOR & 0xFF, ZH_VER_RELEASE & 0xFF );
   zh_pp_addDefine( pState, "__ZIHER__", szResult );
#endif

   /* __DATE__ */
   zh_dateToday( &iYear, &iMonth, &iDay );
   zh_dateStrPut( szResult + 1, iYear, iMonth, iDay );
   szResult[ 0 ] = '"';
   szResult[ 9 ] = '"';
   szResult[ 10 ] = '\0';
   zh_pp_addDefine( pState, "__DATE__", szResult );

   /* __TIME__ */
   zh_dateTimeStr( szResult + 1 );
   szResult[ 0 ] = '"';
   szResult[ 9 ] = '"';
   szResult[ 10 ] = '\0';
   zh_pp_addDefine( pState, "__TIME__", szResult );

   /* __TIMESTAMP__ */
   szResult[ 0 ] = 't';
   szResult[ 1 ] = '"';
   zh_timeStampGet( &lDate, &lTime );
   zh_timeStampStr( szResult + 2, lDate, lTime );
   i = ( int ) strlen( szResult );
   szResult[ i++ ] = '"';
   szResult[ i ] = '\0';
   zh_pp_addDefine( pState, "__TIMESTAMP__", szResult );

   zh_pp_addDefine( pState, "__FILE__", &s_pp_dynamicResult );
   zh_pp_addDefine( pState, "__LINE__", &s_pp_dynamicResult );

#ifdef ZH_START_PROCEDURE
   zh_pp_addDefine( pState, "__ZH_MAIN__", ZH_START_PROCEDURE );
#endif
}

/*
 * read preprocess rules from file
 */
void zh_pp_readRules( PZH_PP_STATE pState, const char * szRulesFile )
{
   char szFileName[ ZH_PATH_MAX ];
   PZH_PP_FILE pFile = pState->pFile;
   PZH_FNAME pFileName;

   pFileName = zh_fsFNameSplit( szRulesFile );
   if( ! pFileName->szExtension )
      pFileName->szExtension = ".zhh";
   zh_fsFNameMerge( szFileName, pFileName );
   zh_xfree( pFileName );

   pState->pFile = zh_pp_FileNew( pState, szFileName, ZH_FALSE, NULL, NULL,
                                  ZH_TRUE, pState->pOpenFunc, ZH_FALSE );
   if( ! pState->pFile )
   {
      pState->pFile = pFile;
      zh_pp_error( pState, 'F', ZH_PP_ERR_CANNOT_OPEN_RULES, szFileName );
   }
   else
   {
      ZH_BOOL fError = ZH_FALSE;

      pState->iFiles++;
      pState->usLastType = ZH_PP_TOKEN_NUL;
      while( zh_pp_tokenGet( pState ) )
      {
         if( pState->fError )
            fError = ZH_TRUE;
      }
      if( pState->pFile )
      {
         zh_pp_FileFree( pState, pState->pFile, pState->pCloseFunc );
         pState->iFiles--;
      }
      pState->pFile = pFile;
      if( fError )
         pState->fError = ZH_TRUE;
   }
}

/*
 * close all open input files and set the given buffer as input stream
 */
ZH_BOOL zh_pp_inBuffer( PZH_PP_STATE pState, const char * szFileName,
                        const char * pBuffer, ZH_SIZE nLen, int iStartLine )
{
   zh_pp_InFileFree( pState );

   pState->fError = ZH_FALSE;

   pState->pFile = zh_pp_FileBufNew( pBuffer, nLen );
   if( szFileName )
      pState->pFile->szFileName = zh_strdup( szFileName );
   pState->pFile->iCurrentLine = iStartLine;
   pState->pFile->iLastLine = iStartLine + 1;
   pState->iFiles++;
   return ZH_TRUE;
}

/*
 * close all open input files and set the given one as new
 */
ZH_BOOL zh_pp_inFile( PZH_PP_STATE pState, const char * szFileName,
                      ZH_BOOL fSearchPath, FILE * file_in, ZH_BOOL fError )
{
   zh_pp_InFileFree( pState );

   pState->fError = ZH_FALSE;

   pState->pFile = zh_pp_FileNew( pState, szFileName, ZH_FALSE, NULL,
                                  file_in, fSearchPath, NULL, ZH_FALSE );
   if( pState->pFile )
   {
      pState->iFiles++;
      return ZH_TRUE;
   }
   if( fError )
      zh_pp_error( pState, 'F', ZH_PP_ERR_CANNOT_OPEN_INPUT, szFileName );
   return ZH_FALSE;
}

/*
 * set output (.ppo) file
 */
ZH_BOOL zh_pp_outFile( PZH_PP_STATE pState, const char * szOutFileName,
                       FILE * file_out )
{
   pState->fError = ZH_FALSE;
   zh_pp_OutFileFree( pState );

   if( szOutFileName )
   {

      if( file_out )
         pState->file_out = file_out;
      else
         pState->file_out = zh_fopen( szOutFileName, "w" );

      if( pState->file_out )
      {
         pState->szOutFileName = zh_strdup( szOutFileName );
         pState->fWritePreprocesed = ZH_TRUE;
      }
      else
      {
         zh_pp_error( pState, 'F', ZH_PP_ERR_CANNOT_CREATE_FILE, szOutFileName );
      }
   }
   return ! pState->fError;
}

/*
 * set trace (.ppt) file
 */
ZH_BOOL zh_pp_traceFile( PZH_PP_STATE pState, const char * szTraceFileName, FILE * file_trace )
{
   pState->fError = ZH_FALSE;
   zh_pp_TraceFileFree( pState );

   if( szTraceFileName )
   {

      if( file_trace )
         pState->file_trace = file_trace;
      else
         pState->file_trace = zh_fopen( szTraceFileName, "w" );

      if( pState->file_trace )
      {
         pState->szTraceFileName = zh_strdup( szTraceFileName );
         pState->fWriteTrace = ZH_TRUE;
      }
      else
      {
         zh_pp_error( pState, 'F', ZH_PP_ERR_CANNOT_CREATE_FILE, szTraceFileName );
      }
   }
   return ! pState->fError;
}

/*
 * check error status of last PP operation
 */
ZH_BOOL zh_pp_lasterror( PZH_PP_STATE pState )
{
   return pState->fError;
}

/*
 * retrieve number of errors which appeared during preprocessing
 */
int zh_pp_errorCount( PZH_PP_STATE pState )
{
   return pState->iErrors;
}

/*
 * return currently preprocessed file name
 */
char * zh_pp_fileName( PZH_PP_STATE pState )
{
   if( pState->pFile )
      return pState->pFile->szFileName;
   else
      return NULL;
}

/*
 * return currently preprocessed line number
 */
int zh_pp_line( PZH_PP_STATE pState )
{
   if( pState->pFile )
      return pState->pFile->iCurrentLine;
   else
      return 0;
}

int zh_pp_lineTot( PZH_PP_STATE pState )
{
   return pState->iLineTot;
}

/*
 * return output file name (.ppo)
 */
char * zh_pp_outFileName( PZH_PP_STATE pState )
{
   return pState->szOutFileName;
}

/*
 * return trace output file name (.ppt)
 */
char * zh_pp_traceFileName( PZH_PP_STATE pState )
{
   return pState->szTraceFileName;
}

/*
 * return if EOF was reached
 */
ZH_BOOL zh_pp_eof( PZH_PP_STATE pState )
{
   return pState->pFile->fEof;
}

/*
 * add new define value
 */
void zh_pp_addDefine( PZH_PP_STATE pState, const char * szDefName,
                      const char * szDefValue )
{
   PZH_PP_TOKEN pMatch, pResult, pToken;
   PZH_PP_FILE pFile;

   pState->fError = ZH_FALSE;

   pFile = zh_pp_FileBufNew( szDefName, strlen( szDefName ) );
   pFile->pPrev = pState->pFile;
   pState->pFile = pFile;
   pState->iFiles++;
   zh_pp_getLine( pState );
   pMatch = pState->pFile->pTokenList;
   pState->pFile->pTokenList = NULL;
   pToken = zh_pp_tokenResultEnd( &pMatch, ZH_TRUE );
   zh_pp_tokenListFree( &pToken );

   if( szDefValue && ! pState->fError )
   {
      if( szDefValue == &s_pp_dynamicResult )
      {
         pResult = zh_pp_tokenNew( szDefName, strlen( szDefName ), 0,
                                   ZH_PP_RMARKER_DYNVAL | ZH_PP_TOKEN_STATIC );
      }
      else
      {
         pFile->pLineBuf = szDefValue;
         pFile->nLineBufLen = strlen( szDefValue );
         zh_pp_getLine( pState );
         pResult = pState->pFile->pTokenList;
         pState->pFile->pTokenList = NULL;
         pToken = zh_pp_tokenResultEnd( &pResult, ZH_TRUE );
         zh_pp_tokenListFree( &pToken );
      }
   }
   else
      pResult = NULL;

   if( pState->fError || ! pMatch )
   {
      zh_pp_tokenListFree( &pMatch );
      zh_pp_tokenListFree( &pResult );
   }
   else
   {
      zh_pp_defineAdd( pState, ZH_PP_CMP_CASE, 0, NULL, pMatch, pResult );
   }
   pState->pFile = pFile->pPrev;
   zh_pp_FileFree( pState, pFile, NULL );
   pState->iFiles--;
}

/*
 * delete define value
 */
void zh_pp_delDefine( PZH_PP_STATE pState, const char * szDefName )
{
   PZH_PP_TOKEN pToken;

   pToken = zh_pp_tokenNew( szDefName, strlen( szDefName ),
                            0, ZH_PP_TOKEN_KEYWORD );
   zh_pp_defineDel( pState, pToken );
   zh_pp_tokenFree( pToken );
}

/*
 * set stream mode
 */
void zh_pp_setStream( PZH_PP_STATE pState, int iMode )
{
   pState->fError = ZH_FALSE;
   switch( iMode )
   {
      case ZH_PP_STREAM_DUMP_C:
         pState->iDumpLine = pState->pFile ? pState->pFile->iCurrentLine : 0;
         if( ! pState->pDumpBuffer )
            pState->pDumpBuffer = zh_membufNew();
         pState->iStreamDump = iMode;
         break;

      case ZH_PP_STREAM_INLINE_C:
         pState->iDumpLine = pState->pFile ? pState->pFile->iCurrentLine : 0;
         /* fallthrough */
      case ZH_PP_STREAM_TEXT:
      case ZH_PP_STREAM_PRG:
      case ZH_PP_STREAM_C:
         if( ! pState->pStreamBuffer )
            pState->pStreamBuffer = zh_membufNew();
         /* fallthrough */
      case ZH_PP_STREAM_OFF:
      case ZH_PP_STREAM_COMMENT:
         pState->iStreamDump = iMode;
         break;

      default:
         pState->fError = ZH_TRUE;
   }
}

/*
 * return next preprocessed line
 */
char * zh_pp_nextLine( PZH_PP_STATE pState, ZH_SIZE * pnLen )
{
   if( pState->pFile )
   {
      PZH_PP_TOKEN pToken;
      ZH_BOOL fError = ZH_FALSE;
      ZH_USHORT ltype;

      if( ! pState->pOutputBuffer )
         pState->pOutputBuffer = zh_membufNew();
      else
         zh_membufFlush( pState->pOutputBuffer );

      pState->usLastType = ltype = ZH_PP_TOKEN_NUL;
      while( ( pToken = zh_pp_tokenGet( pState ) ) != NULL )
      {
         if( pState->fError )
            fError = ZH_TRUE;
         if( zh_pp_tokenStr( pToken, pState->pOutputBuffer, ZH_TRUE, ZH_TRUE, ltype ) )
            break;
         /* only single command in one call */
         if( ! pState->pTokenOut->pNext )
            break;
         ltype = ZH_PP_TOKEN_TYPE( pToken->type );
      }
      if( fError )
         pState->fError = ZH_TRUE;

      if( pnLen )
         *pnLen = zh_membufLen( pState->pOutputBuffer );
      zh_membufAddCh( pState->pOutputBuffer, '\0' );

      return zh_membufPtr( pState->pOutputBuffer );
   }

   if( pnLen )
      *pnLen = 0;
   return NULL;
}

/*
 * preprocess given buffer
 */
char * zh_pp_parseLine( PZH_PP_STATE pState, const char * pLine, ZH_SIZE * pnLen )
{
   PZH_PP_TOKEN pToken;
   PZH_PP_FILE pFile;
   ZH_BOOL fError = ZH_FALSE;
   ZH_USHORT ltype;
   ZH_SIZE nLen;

   if( ! pState->pOutputBuffer )
      pState->pOutputBuffer = zh_membufNew();
   else
      zh_membufFlush( pState->pOutputBuffer );

   nLen = pnLen ? *pnLen : strlen( pLine );

   pFile = zh_pp_FileBufNew( pLine, nLen );
   pFile->pPrev = pState->pFile;
   pState->pFile = pFile;
   pState->iFiles++;

   pState->usLastType = ltype = ZH_PP_TOKEN_NUL;
   while( ( pToken = zh_pp_tokenGet( pState ) ) != NULL )
   {
      if( pState->fError )
         fError = ZH_TRUE;
      zh_pp_tokenStr( pToken, pState->pOutputBuffer, ZH_TRUE, ZH_TRUE, ltype );
      ltype = ZH_PP_TOKEN_TYPE( pToken->type );
   }
   if( fError )
      pState->fError = ZH_TRUE;

   if( ( nLen && pLine[ nLen - 1 ] == '\n' ) ||
       zh_membufLen( pState->pOutputBuffer ) == 0 ||
       zh_membufPtr( pState->pOutputBuffer )
                        [ zh_membufLen( pState->pOutputBuffer ) - 1 ] != '\n' )
      zh_membufAddCh( pState->pOutputBuffer, '\0' );
   else
      zh_membufPtr( pState->pOutputBuffer )
                        [ zh_membufLen( pState->pOutputBuffer ) - 1 ] = '\0';

   if( pnLen )
      *pnLen = zh_membufLen( pState->pOutputBuffer ) - 1;

   if( pState->pFile == pFile )
   {
      pState->pFile = pFile->pPrev;
      zh_pp_FileFree( pState, pFile, NULL );
      pState->iFiles--;
   }

   return zh_membufPtr( pState->pOutputBuffer );
}

/*
 * create new PP context for macro compiler
 */
PZH_PP_STATE zh_pp_lexNew( const char * pMacroString, ZH_SIZE nLen )
{
   PZH_PP_STATE pState = zh_pp_new();

   pState->fQuiet = ZH_TRUE;
   pState->fGauge = ZH_FALSE;
   pState->pFile = zh_pp_FileBufNew( pMacroString, nLen );
   zh_pp_getLine( pState );
   pState->pTokenOut = pState->pFile->pTokenList;
   pState->pFile->pTokenList = NULL;
   zh_pp_FileFree( pState, pState->pFile, NULL );
   pState->pFile = NULL;
   if( pState->fError )
   {
      zh_pp_free( pState );
      pState = NULL;
   }
   else
      pState->pNextTokenPtr = &pState->pTokenOut;

   return pState;
}

PZH_PP_TOKEN zh_pp_lexGet( PZH_PP_STATE pState )
{
   PZH_PP_TOKEN pToken = *pState->pNextTokenPtr;

   if( pToken )
      pState->pNextTokenPtr = &pToken->pNext;

   return pToken;
}

ZH_BOOL zh_pp_tokenNextExp( PZH_PP_TOKEN * pTokenPtr )
{
   if( zh_pp_tokenCanStartExp( *pTokenPtr ) )
   {
      ZH_BOOL fStop = ZH_FALSE;
      if( zh_pp_tokenSkipExp( pTokenPtr, NULL, ZH_PP_CMP_STD, &fStop ) && ! fStop )
         return ZH_TRUE;
   }

   return ZH_FALSE;
}

/*
 * convert token letters to upper cases
 * strip leading '&' and trailing '.' (if any) from macrovar token
 */
void zh_pp_tokenUpper( PZH_PP_TOKEN pToken )
{
   if( ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_MACROVAR )
   {
      if( pToken->len > ZH_SYMBOL_NAME_LEN + 1 )
         pToken->len = ZH_SYMBOL_NAME_LEN + 1;
      if( pToken->value[ pToken->len - 1 ] == '.' )
         pToken->len -= 2;
      else
         pToken->len--;

      if( pToken->len <= 1 )
      {
         ZH_UCHAR ucVal = pToken->len ? ( ZH_UCHAR ) pToken->value[ 1 ] : 0;
         if( ZH_PP_TOKEN_ALLOC( pToken->type ) )
         {
            zh_xfree( ZH_UNCONST( pToken->value ) );
            pToken->type |= ZH_PP_TOKEN_STATIC;
         }
         pToken->value = zh_szAscii[ ucVal ];
      }
      else
      {
         if( ! ZH_PP_TOKEN_ALLOC( pToken->type ) )
         {
            pToken->value = ( char * ) memcpy( zh_xgrab( pToken->len + 1 ),
                                               pToken->value + 1, pToken->len );
            pToken->type &= ~ZH_PP_TOKEN_STATIC;
         }
         else
            memmove( ZH_UNCONST( pToken->value ), pToken->value + 1, pToken->len );
         ( ( char * ) ZH_UNCONST( pToken->value ) )[ pToken->len ] = '\0';
      }
   }
   else if( pToken->len > 1 )
   {
      if( ! ZH_PP_TOKEN_ALLOC( pToken->type ) )
      {
         char * value = ( char * ) zh_xgrab( pToken->len + 1 );
         memcpy( value, pToken->value, pToken->len + 1 );
         pToken->value = value;
         pToken->type &= ~ZH_PP_TOKEN_STATIC;
      }
      if( ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_KEYWORD &&
          pToken->len > ZH_SYMBOL_NAME_LEN )
      {
         pToken->len = ZH_SYMBOL_NAME_LEN;
         ( ( char * ) ZH_UNCONST( pToken->value ) )[ ZH_SYMBOL_NAME_LEN ] = '\0';
      }
   }

   if( pToken->len <= 1 )
   {
      ZH_UCHAR ucVal = ( ZH_UCHAR ) ZH_PP_UPPER( pToken->value[ 0 ] );
      if( ZH_PP_TOKEN_ALLOC( pToken->type ) )
      {
         zh_xfree( ZH_UNCONST( pToken->value ) );
         pToken->type |= ZH_PP_TOKEN_STATIC;
      }
      pToken->value = zh_szAscii[ ucVal ];
   }
   else
      zh_strupr( ( char * ) ZH_UNCONST( pToken->value ) );
}

/*
 * convert tokens between '[' and ']' tokens into single string token
 * and replace the converted tokens with the new string
 */
void zh_pp_tokenToString( PZH_PP_STATE pState, PZH_PP_TOKEN pToken )
{
   ZH_BOOL fError = ZH_TRUE;

   pState->fError = ZH_FALSE;
   zh_membufFlush( pState->pBuffer );
   if( ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_LEFT_SB )
   {
      PZH_PP_TOKEN pTok, pFirst, pLast = NULL;
      pFirst = pTok = pToken->pNext;
      while( ! ZH_PP_TOKEN_ISEOL( pTok ) )
      {
         pLast = pTok;
         if( ZH_PP_TOKEN_TYPE( pTok->type ) == ZH_PP_TOKEN_RIGHT_SB )
         {
            while( pTok->spaces > 0 )
            {
               zh_membufAddCh( pState->pBuffer, ' ' );
               pTok->spaces--;
            }
            fError = ZH_FALSE;
            pTok = pTok->pNext;
            break;
         }
         else if( ZH_PP_TOKEN_TYPE( pTok->type ) == ZH_PP_TOKEN_EOC &&
                  ! pTok->pNext && pState->pFile->pTokenList )
         {
            zh_pp_tokenMoveCommand( pState, &pTok->pNext,
                                    &pState->pFile->pTokenList );
         }
         zh_pp_tokenStr( pTok, pState->pBuffer, ZH_TRUE, ZH_FALSE, 0 );
         pTok = pTok->pNext;
      }
      if( pLast )
      {
         pLast->pNext = NULL;
         pToken->pNext = pTok;
         zh_pp_tokenListFree( &pFirst );
      }
      zh_pp_tokenSetValue( pToken, zh_membufPtr( pState->pBuffer ),
                                   zh_membufLen( pState->pBuffer ) );
      ZH_PP_TOKEN_SETTYPE( pToken, ZH_PP_TOKEN_STRING );
      if( pState->fWritePreprocesed )
      {
         if( ! fError )
            zh_membufAddCh( pState->pBuffer, ']' );
         if( fwrite( zh_membufPtr( pState->pBuffer ), sizeof( char ),
                     zh_membufLen( pState->pBuffer ), pState->file_out ) !=
             zh_membufLen( pState->pBuffer ) )
         {
            zh_pp_error( pState, 'F', ZH_PP_ERR_WRITE_FILE, pState->szOutFileName );
         }
      }
   }

   if( fError )
   {
      zh_membufAddCh( pState->pBuffer, '\0' );
      zh_pp_error( pState, 'E', ZH_PP_ERR_STRING_TERMINATOR,
                   zh_membufPtr( pState->pBuffer ) );
   }
}

char * zh_pp_tokenBlockString( PZH_PP_STATE pState, PZH_PP_TOKEN pToken,
                               int * piType, ZH_SIZE * pnLen )
{
   *piType = 0;
   zh_membufFlush( pState->pBuffer );
   if( ZH_PP_TOKEN_TYPE( pToken->type ) == ZH_PP_TOKEN_LEFT_CB )
   {
      ZH_USHORT ltype = ZH_PP_TOKEN_NUL;
      int iBraces = 0;
      do
      {
         zh_pp_tokenStr( pToken, pState->pBuffer, ltype != ZH_PP_TOKEN_NUL,
                         ZH_TRUE, ltype );
         ltype = ZH_PP_TOKEN_TYPE( pToken->type );
         switch( ltype )
         {
            case ZH_PP_TOKEN_MACROVAR:
            case ZH_PP_TOKEN_MACROTEXT:
               *piType |= ZH_BLOCK_MACROVAR;
               break;
            case ZH_PP_TOKEN_RIGHT_CB:
               --iBraces;
               break;
            case ZH_PP_TOKEN_LEFT_CB:
               ++iBraces;
               break;
         }
         pToken = pToken->pNext;
      }
      while( iBraces && ! ZH_PP_TOKEN_ISEOC( pToken ) );
   }
   *pnLen = zh_membufLen( pState->pBuffer );
   zh_membufAddCh( pState->pBuffer, '\0' );
   return zh_membufPtr( pState->pBuffer );
}
