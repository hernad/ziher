/*
 * Small and MT safe lexer for macro compiler
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

#define ZH_MACRO_SUPPORT

#include "zh_macro.h"
#include "zh_comp.h"
#include "zh_date.h"
#include "zh_macro/zh_macro_y.h"

typedef struct _ZH_MACRO_LEX
{
   const char * pString;
   char *       pDst;
   ZH_SIZE      nLen;
   ZH_SIZE      nSrc;
   ZH_BOOL      quote;
   char         pBuffer[ 2 ];
}
ZH_MACRO_LEX, * PZH_MACRO_LEX;

ZH_BOOL zh_macroLexNew( PZH_MACRO pMacro )
{
   if( pMacro->length )
   {
      /*
       * the total maximum size for parsed tokens delimited with ASCII NUL
       * cannot be bigger then the size of macro string because only
       * identifiers, strings, macrovars and macrotexts have to be returned
       * as string and all these tokens have to be separated by some non
       * value tokens or strings which will have not used delimiters
       */
      pMacro->pLex = zh_xgrab( sizeof( ZH_MACRO_LEX ) + pMacro->length );
      ( ( PZH_MACRO_LEX ) pMacro->pLex )->pString = pMacro->string;
      ( ( PZH_MACRO_LEX ) pMacro->pLex )->nLen    = pMacro->length;
      ( ( PZH_MACRO_LEX ) pMacro->pLex )->nSrc  = 0;
      ( ( PZH_MACRO_LEX ) pMacro->pLex )->quote = ZH_TRUE;
      ( ( PZH_MACRO_LEX ) pMacro->pLex )->pDst  =
                                 ( ( PZH_MACRO_LEX ) pMacro->pLex )->pBuffer;
      return ZH_TRUE;
   }

   return ZH_FALSE;
}

void zh_macroLexDelete( PZH_MACRO pMacro )
{
   if( pMacro->pLex )
   {
      zh_xfree( pMacro->pLex );
      pMacro->pLex = NULL;
   }
}

static void zh_lexSkipBlank( PZH_MACRO_LEX pLex )
{
   while( pLex->nSrc < pLex->nLen &&
          ( pLex->pString[ pLex->nSrc ] == ' ' ||
            pLex->pString[ pLex->nSrc ] == '\t' ) )
      pLex->nSrc++;
}

static void zh_lexIdentCopy( PZH_MACRO_LEX pLex )
{
   while( pLex->nSrc < pLex->nLen )
   {
      char ch = pLex->pString[ pLex->nSrc ];
      if( ch >= 'a' && ch <= 'z' )
         *pLex->pDst++ = ch - ( 'a' - 'A' );
      else if( ( ch >= 'A' && ch <= 'Z' ) ||
               ( ch >= '0' && ch <= '9' ) || ch == '_' )
         *pLex->pDst++ = ch;
      else
         break;
      pLex->nSrc++;
   }
}

static int zh_lexTimestampGet( YYSTYPE * yylval_ptr, PZH_MACRO pMacro,
                               PZH_MACRO_LEX pLex )
{
   ZH_BOOL fOK = ZH_FALSE;
   char * dst = pLex->pDst;

   pLex->quote = ZH_FALSE;
   while( pLex->nSrc < pLex->nLen )
   {
      char ch = pLex->pString[ pLex->nSrc++ ];
      if( ch == '"' )
      {
         fOK = ZH_TRUE;
         break;
      }
      *dst++ = ch;
   }
   *dst = '\0';
   if( ! zh_timeStampStrGetDT( pLex->pDst,
                               &yylval_ptr->valTimeStamp.date,
                               &yylval_ptr->valTimeStamp.time ) )
      fOK = ZH_FALSE;
   if( ! fOK )
      zh_macroError( EG_SYNTAX, pMacro );
   return TIMESTAMP;
}

static int zh_lexDateGet( YYSTYPE * yylval_ptr, PZH_MACRO pMacro,
                          PZH_MACRO_LEX pLex )
{
   ZH_BOOL fOK = ZH_FALSE;
   char * dst = pLex->pDst;
   int iYear, iMonth, iDay;

   pLex->quote = ZH_FALSE;
   while( pLex->nSrc < pLex->nLen )
   {
      char ch = pLex->pString[ pLex->nSrc++ ];
      if( ch == '"' )
      {
         fOK = ZH_TRUE;
         break;
      }
      *dst++ = ch;
   }
   *dst = '\0';
   if( fOK && zh_timeStampStrGet( pLex->pDst, &iYear, &iMonth, &iDay, NULL, NULL, NULL, NULL ) )
   {
      yylval_ptr->valLong.lNumber = zh_dateEncode( iYear, iMonth, iDay );
   }
   else
   {
      yylval_ptr->valLong.lNumber = 0;
      zh_macroError( EG_SYNTAX, pMacro );
   }

   return NUM_DATE;
}

static int zh_lexStringCopy( YYSTYPE * yylval_ptr, PZH_MACRO pMacro,
                             PZH_MACRO_LEX pLex, char cDelim )
{
   pLex->quote = ZH_FALSE;
   yylval_ptr->valChar.string = pLex->pDst;
   while( pLex->nSrc < pLex->nLen )
   {
      char ch = pLex->pString[ pLex->nSrc++ ];
      if( ch == cDelim )
      {
         yylval_ptr->valChar.length = pLex->pDst - yylval_ptr->valChar.string;
         *pLex->pDst++ = '\0';
         return LITERAL;
      }
      *pLex->pDst++ = ch;
   }
   yylval_ptr->valChar.length = pLex->pDst - yylval_ptr->valChar.string;
   *pLex->pDst++ = '\0';
   zh_macroError( EG_SYNTAX, pMacro );
   return LITERAL;
}

static int zh_lexStringExtCopy( YYSTYPE * yylval_ptr, PZH_MACRO pMacro,
                                PZH_MACRO_LEX pLex )
{
   ZH_SIZE nLen;
   char * string;

   pLex->quote = ZH_FALSE;
   string = pLex->pDst;
   while( pLex->nSrc < pLex->nLen )
   {
      char ch = pLex->pString[ pLex->nSrc++ ];
      if( ch == '\\' )
      {
         if( pLex->nSrc < pLex->nLen )
         {
            *pLex->pDst++ = ch;
            ch = pLex->pString[ pLex->nSrc++ ];
         }
      }
      else if( ch == '"' )
      {
         nLen = pLex->pDst - string;
         *pLex->pDst++ = '\0';
         zh_strRemEscSeq( string, &nLen );
         yylval_ptr->valChar.length = nLen;
         yylval_ptr->valChar.string = string;
         return LITERAL;
      }
      *pLex->pDst++ = ch;
   }
   nLen = pLex->pDst - string;
   *pLex->pDst++ = '\0';
   zh_strRemEscSeq( string, &nLen );
   yylval_ptr->valChar.length = nLen;
   yylval_ptr->valChar.string = string;
   zh_macroError( EG_SYNTAX, pMacro );
   return LITERAL;
}

static int zh_lexNumConv( YYSTYPE * yylval_ptr, PZH_MACRO_LEX pLex, ZH_SIZE nLen )
{
   ZH_MAXINT lNumber;
   double dNumber;
   int iDec, iWidth;

   if( zh_compStrToNum( pLex->pString + pLex->nSrc, nLen,
                        &lNumber, &dNumber, &iDec, &iWidth ) )
   {
      yylval_ptr->valDouble.dNumber = dNumber;
      yylval_ptr->valDouble.bDec = ( ZH_UCHAR ) iDec;
      yylval_ptr->valDouble.bWidth = ( ZH_UCHAR ) iWidth;
      pLex->nSrc += nLen;
      return NUM_DOUBLE;
   }
   else
   {
      yylval_ptr->valLong.lNumber = lNumber;
      yylval_ptr->valLong.bWidth = ( ZH_UCHAR ) iWidth;
      pLex->nSrc += nLen;
      return NUM_LONG;
   }
}

extern int zh_macro_yylex( YYSTYPE * yylval_ptr, PZH_MACRO pMacro );

int zh_macro_yylex( YYSTYPE * yylval_ptr, PZH_MACRO pMacro )
{
   PZH_MACRO_LEX pLex = ( PZH_MACRO_LEX ) pMacro->pLex;

   while( pLex->nSrc < pLex->nLen )
   {
      unsigned char ch = ( unsigned char ) pLex->pString[ pLex->nSrc++ ];
      switch( ch )
      {
         case ' ':
         case '\t':
            break;

         case '$':
         case ',':
         case '|':
         case '@':
         case '(':
         case '{':
            pLex->quote = ZH_TRUE;
            return ch;

         case ')':
         case '}':
         case ']':
            pLex->quote = ZH_FALSE;
            return ch;

         case '#':
            pLex->quote = ZH_TRUE;
            return NE1;

         case '!':
            pLex->quote = ZH_TRUE;
            if( pLex->pString[ pLex->nSrc ] == '=' )
            {
               pLex->nSrc++;
               return NE2;
            }
            return NOT;

         case '<':
            pLex->quote = ZH_TRUE;
            if( pLex->pString[ pLex->nSrc ] == '>' )
            {
               pLex->nSrc++;
               return NE2;
            }
            else if( pLex->pString[ pLex->nSrc ] == '=' )
            {
               pLex->nSrc++;
               return LE;
            }
            return '<';

         case '>':
            pLex->quote = ZH_TRUE;
            if( pLex->pString[ pLex->nSrc ] == '=' )
            {
               pLex->nSrc++;
               return GE;
            }
            return '>';

         case '=':
            pLex->quote = ZH_TRUE;
            if( pLex->pString[ pLex->nSrc ] == '=' )
            {
               pLex->nSrc++;
               return EQ;
            }
            else if( pLex->pString[ pLex->nSrc ] == '>' )
            {
               pLex->nSrc++;
               return HASHOP;
            }
            return '=';

         case '+':
            pLex->quote = ZH_TRUE;
            if( pLex->pString[ pLex->nSrc ] == '+' )
            {
               pLex->nSrc++;
               return INC;
            }
            else if( pLex->pString[ pLex->nSrc ] == '=' )
            {
               pLex->nSrc++;
               return PLUSEQ;
            }
            return '+';

         case '-':
            pLex->quote = ZH_TRUE;
            if( pLex->pString[ pLex->nSrc ] == '-' )
            {
               pLex->nSrc++;
               return DEC;
            }
            else if( pLex->pString[ pLex->nSrc ] == '=' )
            {
               pLex->nSrc++;
               return MINUSEQ;
            }
            else if( pLex->pString[ pLex->nSrc ] == '>' )
            {
               pLex->nSrc++;
               return ALIASOP;
            }
            return '-';

         case '*':
            pLex->quote = ZH_TRUE;
            if( pLex->pString[ pLex->nSrc ] == '*' )
            {
               pLex->nSrc++;
               if( pLex->pString[ pLex->nSrc ] == '=' )
               {
                  pLex->nSrc++;
                  return EXPEQ;
               }
               return POWER;
            }
            else if( pLex->pString[ pLex->nSrc ] == '=' )
            {
               pLex->nSrc++;
               return MULTEQ;
            }
            return '*';

         case '/':
            pLex->quote = ZH_TRUE;
            if( pLex->pString[ pLex->nSrc ] == '=' )
            {
               pLex->nSrc++;
               return DIVEQ;
            }
            return '/';

         case '%':
            pLex->quote = ZH_TRUE;
            if( pLex->pString[ pLex->nSrc ] == '=' )
            {
               pLex->nSrc++;
               return MODEQ;
            }
            return '%';

         case '^':
            pLex->quote = ZH_TRUE;
            if( pLex->pString[ pLex->nSrc ] == '=' )
            {
               pLex->nSrc++;
               return EXPEQ;
            }
            return POWER;

         case ':':
            pLex->quote = ZH_TRUE;
            if( pLex->pString[ pLex->nSrc ] == '=' )
            {
               pLex->nSrc++;
               return INASSIGN;
            }
            else if( pLex->pString[ pLex->nSrc ] == ':' )
            {
               yylval_ptr->string = "SELF";
               return IDENTIFIER;
            }
            return ':';

         case '.':
            pLex->quote = ZH_TRUE;
            if( pLex->nSrc < pLex->nLen &&
                ZH_ISDIGIT( pLex->pString[ pLex->nSrc ] ) )
            {
               ZH_SIZE nPos = pLex->nSrc;
               while( ++nPos < pLex->nLen &&
                      ZH_ISDIGIT( pLex->pString[ nPos ] ) ) {};
               nPos -= --pLex->nSrc;
               return zh_lexNumConv( yylval_ptr, pLex, nPos );
            }
            if( pLex->nLen - pLex->nSrc >= 4 &&
                pLex->pString[ pLex->nSrc + 3 ] == '.' )
            {
               if( ( pLex->pString[ pLex->nSrc + 0 ] | ( 'a' - 'A' ) ) == 'a' &&
                   ( pLex->pString[ pLex->nSrc + 1 ] | ( 'a' - 'A' ) ) == 'n' &&
                   ( pLex->pString[ pLex->nSrc + 2 ] | ( 'a' - 'A' ) ) == 'd' )
               {
                  pLex->nSrc += 4;
                  return AND;
               }
               if( ( pLex->pString[ pLex->nSrc + 0 ] | ( 'a' - 'A' ) ) == 'n' &&
                   ( pLex->pString[ pLex->nSrc + 1 ] | ( 'a' - 'A' ) ) == 'o' &&
                   ( pLex->pString[ pLex->nSrc + 2 ] | ( 'a' - 'A' ) ) == 't' )
               {
                  pLex->nSrc += 4;
                  return NOT;
               }
            }
            if( pLex->nLen - pLex->nSrc >= 3 &&
                pLex->pString[ pLex->nSrc + 2 ] == '.' )
            {
               if( ( pLex->pString[ pLex->nSrc + 0 ] | ( 'a' - 'A' ) ) == 'o' &&
                   ( pLex->pString[ pLex->nSrc + 1 ] | ( 'a' - 'A' ) ) == 'r' )
               {
                  pLex->nSrc += 3;
                  return OR;
               }
            }
            if( pLex->nLen - pLex->nSrc >= 2 &&
                pLex->pString[ pLex->nSrc + 1 ] == '.' )
            {
               if( ( pLex->pString[ pLex->nSrc ] | ( 'a' - 'A' ) ) == 't' ||
                   ( pLex->pString[ pLex->nSrc ] | ( 'a' - 'A' ) ) == 'y' )
               {
                  pLex->quote = ZH_FALSE;
                  pLex->nSrc += 2;
                  return TRUEVALUE;
               }
               if( ( pLex->pString[ pLex->nSrc ] | ( 'a' - 'A' ) ) == 'f' ||
                   ( pLex->pString[ pLex->nSrc ] | ( 'a' - 'A' ) ) == 'n' )
               {
                  pLex->quote = ZH_FALSE;
                  pLex->nSrc += 2;
                  return FALSEVALUE;
               }
               if( pLex->pString[ pLex->nSrc ] == '.' )
               {
                  pLex->nSrc += 2;
                  return EPSILON;
               }
            }
            return '.';

         case '[':
            if( pLex->quote )
               return zh_lexStringCopy( yylval_ptr, pMacro, pLex, ']' );
            pLex->quote = ZH_TRUE;
            return '[';

         case '`':
         case '\'':
            return zh_lexStringCopy( yylval_ptr, pMacro, pLex, '\'' );

         case '"':
            return zh_lexStringCopy( yylval_ptr, pMacro, pLex, '"' );

         case '&':
            if( pLex->nSrc < pLex->nLen )
            {
               if( ZH_ISFIRSTIDCHAR( pLex->pString[ pLex->nSrc ] ) )
               {
                  /* [&<keyword>[.[<nextidchars>]]]+ */
                  int iParts = 0;
                  pLex->quote = ZH_FALSE;
                  yylval_ptr->string = pLex->pDst;
                  pLex->nSrc--;
                  do
                  {
                     ++iParts;
                     *pLex->pDst++ = '&';
                     pLex->nSrc++;
                     zh_lexIdentCopy( pLex );
                     if( pLex->pString[ pLex->nSrc ] == '.' )
                     {
                        ++iParts;
                        *pLex->pDst++ = '.';
                        pLex->nSrc++;
                        zh_lexIdentCopy( pLex );
                     }
                  }
                  while( pLex->nLen - pLex->nSrc > 1 &&
                         pLex->pString[ pLex->nSrc ] == '&' &&
                         ZH_ISFIRSTIDCHAR( pLex->pString[ pLex->nSrc + 1 ] ) );
                  if( iParts == 2 && *( pLex->pDst - 1 ) == '.' )
                  {
                     pLex->pDst--;
                     iParts = 1;
                  }
                  *pLex->pDst++ = '\0';
                  if( iParts == 1 )
                  {
                     yylval_ptr->string++;
                     if( pLex->pDst - yylval_ptr->string > ZH_SYMBOL_NAME_LEN + 1 )
                        ( ( char * ) ZH_UNCONST( yylval_ptr->string ) )[ ZH_SYMBOL_NAME_LEN ] = '\0';
                     return MACROVAR;
                  }
                  return MACROTEXT;
               }
               else if( pLex->pString[ pLex->nSrc ] == '\'' ||
                        pLex->pString[ pLex->nSrc ] == '"' ||
                        pLex->pString[ pLex->nSrc ] == '[' )
                  zh_macroError( EG_SYNTAX, pMacro );
            }
            pLex->quote = ZH_TRUE;
            return '&';

         default:
            if( ZH_ISDIGIT( ch ) )
            {
               ZH_SIZE n = pLex->nSrc;

               pLex->quote = ZH_FALSE;
               if( ch == '0' && n < pLex->nLen )
               {
                  if( pLex->pString[ n ] == 'd' || pLex->pString[ n ] == 'D' )
                  {
                     while( ++n < pLex->nLen &&
                            ZH_ISDIGIT( pLex->pString[ n ] ) ) {};
                     if( n - pLex->nSrc == 9 )
                     {
                        int year, month, day;
                        zh_dateStrGet( pLex->pString + pLex->nSrc + 1,
                                       &year, &month, &day );
                        yylval_ptr->valLong.lNumber =
                                       zh_dateEncode( year, month, day );
                        pLex->nSrc = n;
                        if( yylval_ptr->valLong.lNumber == 0 &&
                            ( year != 0 || month != 0 || day != 0 ) )
                        {
                           zh_macroError( EG_SYNTAX, pMacro );
                        }
                        return NUM_DATE;
                     }
                     else if( n - pLex->nSrc == 2 &&
                              pLex->pString[ pLex->nSrc + 1 ] == '0' )
                     {
                        yylval_ptr->valLong.lNumber = 0;
                        return NUM_DATE;
                     }
                     n = pLex->nSrc;
                  }
                  else if( pLex->pString[ n ] == 'x' ||
                           pLex->pString[ n ] == 'X' )
                  {
                     while( ++n < pLex->nLen &&
                            ZH_ISXDIGIT( pLex->pString[ n ] ) ) {};
                     if( n == pLex->nSrc + 1 )
                        --n;
                  }
                  else
                  {
                     while( n < pLex->nLen &&
                            ZH_ISDIGIT( pLex->pString[ n ] ) )
                        ++n;
                     if( pLex->nLen - n > 1 && pLex->pString[ n ] == '.' &&
                         ZH_ISDIGIT( pLex->pString[ n + 1 ] ) )
                     {
                        while( ++n < pLex->nLen &&
                               ZH_ISDIGIT( pLex->pString[ n ] ) ) {};
                     }
                  }
               }
               else
               {
                  while( n < pLex->nLen &&
                         ZH_ISDIGIT( pLex->pString[ n ] ) )
                     ++n;
                  if( pLex->nLen - n > 1 && pLex->pString[ n ] == '.' &&
                      ZH_ISDIGIT( pLex->pString[ n + 1 ] ) )
                  {
                     while( ++n < pLex->nLen &&
                            ZH_ISDIGIT( pLex->pString[ n ] ) ) {};
                  }
               }
               n -= --pLex->nSrc;
               return zh_lexNumConv( yylval_ptr, pLex, n );
            }
            else if( ZH_ISFIRSTIDCHAR( ch ) )
            {
               ZH_SIZE nLen;
               pLex->quote = ZH_FALSE;
               yylval_ptr->string = pLex->pDst;
               *pLex->pDst++ = ch - ( ( ch >= 'a' && ch <= 'z' ) ? 'a' - 'A' : 0 );
               zh_lexIdentCopy( pLex );
               if( pLex->nLen - pLex->nSrc > 1 &&
                   pLex->pString[ pLex->nSrc ] == '&' &&
                   ZH_ISFIRSTIDCHAR( pLex->pString[ pLex->nSrc + 1 ] ) )
               {
                  /* [<keyword>][&<keyword>[.[<nextidchars>]]]+ */
                  do
                  {
                     *pLex->pDst++ = '&';
                     pLex->nSrc++;
                     zh_lexIdentCopy( pLex );
                     if( pLex->pString[ pLex->nSrc ] == '.' )
                     {
                        *pLex->pDst++ = '.';
                        pLex->nSrc++;
                        zh_lexIdentCopy( pLex );
                     }
                  }
                  while( pLex->nLen - pLex->nSrc > 1 &&
                         pLex->pString[ pLex->nSrc ] == '&' &&
                         ZH_ISFIRSTIDCHAR( pLex->pString[ pLex->nSrc + 1 ] ) );
                  *pLex->pDst++ = '\0';
                  return MACROTEXT;
               }
               nLen = pLex->pDst - yylval_ptr->string;
               *pLex->pDst++ = '\0';
               if( nLen == 1 )
               {
                  if( pLex->nLen > pLex->nSrc &&
                      pLex->pString[ pLex->nSrc ] == '"' )
                  {
                     switch( yylval_ptr->string[ 0 ] )
                     {
                        case 'E':
                           pLex->nSrc++;
                           return zh_lexStringExtCopy( yylval_ptr, pMacro, pLex );
                        case 'T':
                           pLex->nSrc++;
                           return zh_lexTimestampGet( yylval_ptr, pMacro, pLex );
                        case 'D':
                           pLex->nSrc++;
                           return zh_lexDateGet( yylval_ptr, pMacro, pLex );
                     }
                  }
               }
               else if( nLen == 2 )
               {
                  if( yylval_ptr->string[ 0 ] == 'I' &&
                      yylval_ptr->string[ 1 ] == 'F' )
                  {
                     zh_lexSkipBlank( pLex );
                     if( pLex->nSrc < pLex->nLen &&
                         pLex->pString[ pLex->nSrc ] == '(' )
                        return IIF;
                  }
               }
               else if( nLen == 3 )
               {
                  if( yylval_ptr->string[ 0 ] == 'I' &&
                      yylval_ptr->string[ 1 ] == 'I' &&
                      yylval_ptr->string[ 2 ] == 'F' )
                  {
                     zh_lexSkipBlank( pLex );
                     if( pLex->nSrc < pLex->nLen &&
                         pLex->pString[ pLex->nSrc ] == '(' )
                        return IIF;
                  }
                  else if( yylval_ptr->string[ 0 ] == 'N' &&
                           yylval_ptr->string[ 1 ] == 'I' &&
                           yylval_ptr->string[ 2 ] == 'L' )
                     return NIL;
               }
               else /* nLen >= 4 */
               {
                  switch( yylval_ptr->string[ 0 ] )
                  {
                     case '_':
                        if( nLen <= 6 && memcmp( "FIELD", yylval_ptr->string + 1,
                                                 nLen - 1 ) == 0 )
                        {
                           zh_lexSkipBlank( pLex );
                           if( pLex->nSrc + 1 < pLex->nLen &&
                               pLex->pString[ pLex->nSrc ] == '-' &&
                               pLex->pString[ pLex->nSrc + 1 ] == '>' )
                              return FIELD;
                        }
                        break;
                     case 'F':
                        if( nLen <= 5 && memcmp( "IELD", yylval_ptr->string + 1,
                                                 nLen - 1 ) == 0 )
                        {
                           zh_lexSkipBlank( pLex );
                           if( pLex->nSrc + 1 < pLex->nLen &&
                               pLex->pString[ pLex->nSrc ] == '-' &&
                               pLex->pString[ pLex->nSrc + 1 ] == '>' )
                              return FIELD;
                        }
                        break;
                     case 'Q':
                        if( nLen == 5 && memcmp( "SELF", yylval_ptr->string + 1,
                                                 4 ) == 0 )
                        {
                           zh_lexSkipBlank( pLex );
                           if( pLex->nSrc < pLex->nLen &&
                               pLex->pString[ pLex->nSrc ] == '(' )
                           {
                              ZH_SIZE n = pLex->nSrc;
                              while( ++n < pLex->nLen )
                              {
                                 if( pLex->pString[ n ] == ')' )
                                 {
                                    pLex->nSrc = n + 1;
                                    return SELF;
                                 }
                                 else if( pLex->pString[ n ] != ' ' &&
                                          pLex->pString[ n ] != '\t' )
                                    break;
                              }
                           }
                        }
                        break;
                  }
                  if( pLex->pDst - yylval_ptr->string > ZH_SYMBOL_NAME_LEN + 1 )
                     ( ( char * ) ZH_UNCONST( yylval_ptr->string ) )[ ZH_SYMBOL_NAME_LEN ] = '\0';
               }
               return IDENTIFIER;
            }
            return ch;
      }
   }

   return 0;
}
