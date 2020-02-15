/*
 * Preprocessor static rules generator.
 * It creates .c file with tables for defines/[x]translates/[x]commands
 * found in given .zhh or .zh file
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

#include "zh_api.h"

#define _DEFAULT_ORIGIN_URL  "https://github.com/hernad/ziher/"

int zh_verCommitRev( void )
{
   return 0;
}

const char * zh_verCommitID( void )
{
   return "";
}

const char * zh_verCommitIDShort( void )
{
   return "";
}

#include "zh_pp_core.h"

/*
 * functions to create .c files with rules defined in given PP context
 */
static int zh_pp_writeTokenCount( PZH_PP_TOKEN pToken )
{
   int iToken = 0;

   while( pToken )
   {
      iToken += zh_pp_writeTokenCount( pToken->pMTokens ) + 1;
      pToken  = pToken->pNext;
   }
   return iToken;
}

static void zh_pp_writeToken( FILE * fout, PZH_PP_TOKEN pToken,
                              const char * szName, int iToken, ZH_BOOL fLast )
{
   while( pToken )
   {
      int iOptional = zh_pp_writeTokenCount( pToken->pMTokens ), i;

      i = ( int ) strlen( szName );
      if( pToken->pNext )
         fprintf( fout, "   { %s +%2d", szName, iToken + iOptional + 1 );
      else
         fprintf( fout, "   { NULL%*s", i, "" );
      if( iOptional )
         fprintf( fout, ", %s +%2d", szName, iToken + 1 );
      else
         fprintf( fout, ", NULL%*s", i, "" );

      i = 16 - ( int ) strlen( pToken->value );
      fprintf( fout, ", \"%s\", %*s %2d,%2d, 0x%04x, %u }%s\n",
               pToken->value,
               i < 0 ? 0 : i, "",
               ( int ) pToken->len, ( int ) pToken->spaces,
               pToken->type | ZH_PP_TOKEN_STATIC | ZH_PP_TOKEN_PREDEFINED,
               pToken->index,
               fLast && ! pToken->pNext && iOptional == 0 ? "" : "," );

      if( iOptional )
         zh_pp_writeToken( fout, pToken->pMTokens, szName, iToken + 1,
                           pToken->pNext == NULL && fLast );

      iToken += iOptional + 1;
      pToken  = pToken->pNext;
   }
}

static void zh_pp_writeTokenList( FILE * fout, PZH_PP_TOKEN pTokenLst, const char * szName )
{
   int iTokens;

   iTokens = zh_pp_writeTokenCount( pTokenLst );
   if( iTokens )
   {
      fprintf( fout, "static ZH_PP_TOKEN %s[ %d ] = {\n",
               szName, iTokens );
      zh_pp_writeToken( fout, pTokenLst, szName, 0, ZH_TRUE );
      fprintf( fout, "};\n" );
   }
}

static int zh_pp_writeRules( FILE * fout, PZH_PP_RULE pFirst, const char * szName )
{
   char szMatch[ 16 ], szResult[ 16 ];
   ZH_ULONG ulRepeatBits, ulBit;
   PZH_PP_RULE pRule;
   int iRule;
   ZH_USHORT u;

   iRule = 0;
   pRule = pFirst;
   while( pRule )
   {
      ++iRule;
      if( pRule->pMatch )
      {
         zh_snprintf( szMatch, sizeof( szMatch ), "s_%cm%03d", szName[ 0 ], iRule );
         zh_pp_writeTokenList( fout, pRule->pMatch, szMatch );
      }

      if( pRule->pResult )
      {
         zh_snprintf( szResult, sizeof( szResult ), "s_%cr%03d", szName[ 0 ], iRule );
         zh_pp_writeTokenList( fout, pRule->pResult, szResult );
      }
      pRule = pRule->pPrev;
   }

   fprintf( fout, "static const ZH_PP_DEFRULE s_%s[ %d ] = {\n",
            szName, iRule );

   iRule = 0;
   pRule = pFirst;
   while( pRule )
   {
      ++iRule;
      if( pRule->pMatch )
         zh_snprintf( szMatch, sizeof( szMatch ), "s_%cm%03d", szName[ 0 ], iRule );
      else
         zh_strncpy( szMatch, "NULL   ", sizeof( szMatch ) - 1 );
      if( pRule->pResult )
         zh_snprintf( szResult, sizeof( szResult ), "s_%cr%03d", szName[ 0 ], iRule );
      else
         zh_strncpy( szResult, "NULL   ", sizeof( szResult ) - 1 );

      ulRepeatBits = 0;
      for( u = 0, ulBit = 1; u < pRule->markers; ++u, ulBit <<= 1 )
      {
         if( pRule->pMarkers[ u ].canrepeat )
            ulRepeatBits |= ulBit;
      }
      fprintf( fout, "   { %s, %s, %d,%2u, 0x%04lx }%s\n",
               szMatch, szResult, ZH_PP_CMP_MODE( pRule->mode ),
               pRule->markers, ulRepeatBits, pRule->pPrev ? "," : "" );
      pRule = pRule->pPrev;
   }
   fprintf( fout, "};\n\n" );
   return iRule;
}

static void zh_pp_generateInitFunc( FILE * fout, int iRules,
                                    const char * szVar, const char * szRule )
{
   fprintf( fout, "   zh_pp_initRules( &pState->p%s, &pState->i%s, ",
            szVar, szVar );
   if( iRules )
      fprintf( fout, "s_%s, %d );\n", szRule, iRules );
   else
      fprintf( fout, "NULL, 0 );\n" );
}

static void zh_pp_generateRules( FILE * fout, PZH_PP_STATE pState, const char * szPPRuleFuncName )
{
   int iDefs = 0, iTrans = 0, iCmds = 0;

   fprintf( fout, "/*\n"
            " * Built-in preprocessor rules.\n"
            " *\n"
            " * Copyright 2006-present Przemyslaw Czerpak\n"
            " *\n"
            " * This file is generated automatically by Ziher preprocessor\n"
            " * and is covered by the same license as Ziher PP\n"
            " */\n\n#define _ZH_PP_INTERNAL\n#include \"zh_api.h\"\n#include \"zh_pp.h\"\n\n" );

   if( pState->pDefinitions )
      iDefs = zh_pp_writeRules( fout, pState->pDefinitions, "def" );
   if( pState->pTranslations )
      iTrans = zh_pp_writeRules( fout, pState->pTranslations, "trs" );
   if( pState->pCommands )
      iCmds = zh_pp_writeRules( fout, pState->pCommands, "cmd" );

   fprintf( fout, "\nvoid %s( PZH_PP_STATE pState )\n{\n", szPPRuleFuncName ? szPPRuleFuncName : "zh_pp_setStdRules" );
   zh_pp_generateInitFunc( fout, iDefs,  "Definitions",  "def" );
   zh_pp_generateInitFunc( fout, iTrans, "Translations", "trs" );
   zh_pp_generateInitFunc( fout, iCmds,  "Commands",     "cmd" );
   fprintf( fout, "}\n" );
}

static void zh_pp_undefCompilerRules( PZH_PP_STATE pState )
{
   int i;
   PZH_PP_RULE * pRulePtr, pRule;
   const char * szRules[] = { "__HARBOUR__",
                              "__DATE__",
                              "__TIME__",
                              "__FILE__",
                              "__LINE__",
                              "__ZH_MAIN__",
                              "__ARCH16BIT__",
                              "__ARCH32BIT__",
                              "__ARCH64BIT__",
                              "__LITTLE_ENDIAN__",
                              "__BIG_ENDIAN__",
                              "__PDP_ENDIAN__",
                              NULL };

   for( i = 0; szRules[ i ]; ++i )
      zh_pp_delDefine( pState, szRules[ i ] );

   pRulePtr = &pState->pDefinitions;
   while( *pRulePtr )
   {
      pRule = *pRulePtr;
      if( ! pRule->pMatch->pNext &&
          strncmp( pRule->pMatch->value, "__PLATFORM__", 12 ) == 0 )
      {
         *pRulePtr = pRule->pPrev;
         zh_pp_ruleFree( pRule );
         pState->iDefinitions--;
      }
      else
         pRulePtr = &pRule->pPrev;
   }
}

static int zh_pp_preprocesfile( PZH_PP_STATE pState, const char * szRuleFile, const char * szPPRuleFuncName )
{
   int iResult = 0;
   ZH_SIZE nLen;

   while( zh_pp_nextLine( pState, &nLen ) != NULL && nLen )
      ;

   if( szRuleFile )
   {
      FILE * foutr;

      foutr = zh_fopen( szRuleFile, "w" );
      if( ! foutr )
      {
         perror( szRuleFile );
         iResult = 1;
      }
      else
      {
         zh_pp_undefCompilerRules( pState );
         zh_pp_generateRules( foutr, pState, szPPRuleFuncName );
         fclose( foutr );
      }
   }

   return iResult;
}

/* NOTE: Caller should free the pointer. */
static char * zh_pp_escapeString( char * szString )
{
   char * szResult, ch;
   int iLen;

   szResult = szString;
   iLen = 0;
   do
   {
      ch = *szResult++;
      /* NOTE: ? is escaped to avoid conflicts with trigraph sequences which
       *       are part of ANSI C standard
       */
      if( ch == '"' || ch == '\\' || ch == '?' )
         ++iLen;
      ++iLen;
   }
   while( ch );

   szResult = ( char * ) zh_xgrab( iLen );
   iLen = 0;
   do
   {
      ch = *szString++;
      if( ch == '"' || ch == '\\' || ch == '?' )
         szResult[ iLen++ ] = '\\';
      szResult[ iLen++ ] = ch;
   }
   while( ch );

   return szResult;
}

static int zh_pp_generateVerInfo( char * szVerFile,
                                  char * szCommitYear,
                                  int iCommitRev,
                                  char * szCommitInfo,
                                  char * szCommitID,
                                  char * szCommitIDShort,
                                  char * szURL )
{
   int iResult = 0;
   FILE * fout;

   fout = zh_fopen( szVerFile, "w" );
   if( ! fout )
   {
      perror( szVerFile );
      iResult = 1;
   }
   else
   {
      char * pszEnv;
      char * pszEscaped;

      fprintf( fout, "/*\n"
               " * Version information and build time switches.\n"
               " *\n"
               " * Copyright 2008-present Przemyslaw Czerpak\n"
               " *\n"
               " * This file is generated automatically by Ziher preprocessor\n"
               " * and is covered by the same license as Ziher PP\n"
               " */\n\n" );

      if( szURL )
      {
         pszEscaped = zh_pp_escapeString( szURL );
         fprintf( fout, "#define ZH_VER_ORIGIN_URL        \"%s\"\n", pszEscaped );
         zh_xfree( pszEscaped );
      }

      if( szCommitID )
      {
         pszEscaped = zh_pp_escapeString( szCommitID );
         fprintf( fout, "#define ZH_VER_COMMIT_ID         \"%s\"\n", pszEscaped );
         zh_xfree( pszEscaped );
      }

      if( szCommitIDShort )
      {
         pszEscaped = zh_pp_escapeString( szCommitIDShort );
         fprintf( fout, "#define ZH_VER_COMMIT_ID_SHORT   \"%s\"\n", pszEscaped );
         zh_xfree( pszEscaped );
      }

      if( szCommitYear )
      {
         pszEscaped = zh_pp_escapeString( szCommitYear );
         fprintf( fout, "#define ZH_VER_COMMIT_YEAR       \"%s\"\n", pszEscaped );
         zh_xfree( pszEscaped );
      }

      fprintf( fout, "#define ZH_VER_COMMIT_REV        %d\n", iCommitRev );

      if( szCommitInfo )
      {
         pszEscaped = zh_pp_escapeString( szCommitInfo );
         fprintf( fout, "#define ZH_VER_COMMIT_INFO       \"%s\"\n", pszEscaped );
         zh_xfree( pszEscaped );
      }

      pszEnv = zh_getenv( "ZH_USER_CFLAGS" );
      if( pszEnv )
      {
         pszEscaped = zh_pp_escapeString( pszEnv );
         fprintf( fout, "#define ZH_VER_ZH_USER_CFLAGS    \"%s\"\n", pszEscaped );
         zh_xfree( pszEscaped );
         zh_xfree( pszEnv );
      }

      pszEnv = zh_getenv( "ZH_USER_LDFLAGS" );
      if( pszEnv )
      {
         pszEscaped = zh_pp_escapeString( pszEnv );
         fprintf( fout, "#define ZH_VER_ZH_USER_LDFLAGS   \"%s\"\n", pszEscaped );
         zh_xfree( pszEscaped );
         zh_xfree( pszEnv );
      }

      pszEnv = zh_getenv( "ZH_USER_PRGFLAGS" );
      if( pszEnv )
      {
         pszEscaped = zh_pp_escapeString( pszEnv );
         fprintf( fout, "#define ZH_VER_ZH_USER_PRGFLAGS  \"%s\"\n", pszEscaped );
         zh_xfree( pszEscaped );
         zh_xfree( pszEnv );
      }

      pszEnv = zh_getenv( "ZH_PLATFORM" );
      if( pszEnv )
      {
         pszEscaped = zh_pp_escapeString( pszEnv );
         fprintf( fout, "#define ZH_PLATFORM              \"%s\"\n", pszEscaped );
         zh_xfree( pszEscaped );
         zh_xfree( pszEnv );
      }

      pszEnv = zh_getenv( "ZH_COMPILER" );
      if( pszEnv )
      {
         pszEscaped = zh_pp_escapeString( pszEnv );
         fprintf( fout, "#define ZH_COMPILER              \"%s\"\n", pszEscaped );
         zh_xfree( pszEscaped );
         zh_xfree( pszEnv );
      }

      fclose( fout );
   }

   return iResult;
}

static char * zh_fsFileFind( const char * pszFileMask )
{
   PZH_FFIND ffind;

   if( ( ffind = zh_fsFindFirst( pszFileMask, ZH_FA_ALL ) ) != NULL )
   {
      char pszFileName[ ZH_PATH_MAX ];
      PZH_FNAME pFileName = zh_fsFNameSplit( pszFileMask );
      pFileName->szName = ffind->szName;
      pFileName->szExtension = NULL;
      zh_fsFNameMerge( pszFileName, pFileName );
      zh_fsFindClose( ffind );
      zh_xfree( pFileName );
      return zh_strdup( pszFileName );
   }
   return NULL;
}

static int zh_pp_TimeStampToNum( PZH_PP_STATE pState, char * pszLog, char * pszYear )
{
   char szRevID[ 18 ];
   int iLen;

   if( strlen( pszLog ) >= 16 )
   {
      long lJulian = 0, lMilliSec = 0;
      int iUTC = 0;

      if( strlen( pszLog ) >= 25 &&
          ( pszLog[ 20 ] == '+' || pszLog[ 20 ] == '-' ) &&
          ZH_ISDIGIT( pszLog[ 21 ] ) && ZH_ISDIGIT( pszLog[ 22 ] ) &&
          ZH_ISDIGIT( pszLog[ 23 ] ) && ZH_ISDIGIT( pszLog[ 24 ] ) )
      {
         iUTC = ( ( int ) ( pszLog[ 21 ] - '0' ) * 10 +
                  ( int ) ( pszLog[ 22 ] - '0' ) ) * 60 +
                  ( int ) ( pszLog[ 23 ] - '0' ) * 10 +
                  ( int ) ( pszLog[ 24 ] - '0' );
         if( pszLog[ 20 ] == '-' )
            iUTC *= -1;
      }
      pszLog[ 16 ] = '\0';
      if( iUTC != 0 && zh_timeStampStrGetDT( pszLog, &lJulian, &lMilliSec ) )
      {
         zh_timeStampUnpackDT( zh_timeStampPackDT( lJulian, lMilliSec ) -
                               ( double ) iUTC / ( 24 * 60 ),
                               &lJulian, &lMilliSec );
      }
      if( lJulian && lMilliSec )
      {
         zh_timeStampStrRawPut( szRevID, lJulian, lMilliSec );
         memcpy( pszYear, szRevID, 4 );
         memmove( szRevID, szRevID + 2, 10 );
      }
      else
      {
         memcpy( pszYear, pszLog, 4 );

         szRevID[ 0 ] = pszLog[ 2 ];
         szRevID[ 1 ] = pszLog[ 3 ];
         szRevID[ 2 ] = pszLog[ 5 ];
         szRevID[ 3 ] = pszLog[ 6 ];
         szRevID[ 4 ] = pszLog[ 8 ];
         szRevID[ 5 ] = pszLog[ 9 ];
         szRevID[ 6 ] = pszLog[ 11 ];
         szRevID[ 7 ] = pszLog[ 12 ];
         szRevID[ 8 ] = pszLog[ 14 ];
         szRevID[ 9 ] = pszLog[ 15 ];
      }
      pszYear[ 4 ] = szRevID[ 10 ] = '\0';
   }
   else
      pszYear[ 0 ] = szRevID[ 0 ] = '\0';

   zh_pp_delDefine( pState, "ZH_VER_COMMIT_YEAR" );
   zh_pp_addDefine( pState, "ZH_VER_COMMIT_YEAR", pszYear );
   zh_pp_delDefine( pState, "ZH_VER_COMMIT_REV" );
   zh_pp_addDefine( pState, "ZH_VER_COMMIT_REV", szRevID );

   return ( int ) zh_strValInt( szRevID, &iLen );
}

static int zh_pp_parseChangelog( PZH_PP_STATE pState, const char * pszFileName,
                                 int iQuiet, char ** pszCommitYear,
                                 int * piCommitRev, char ** pszCommitInfo )
{
   char * pszFree = NULL;
   int iResult = 0;
   FILE * file_in;
   char szCommitYear[ 5 ];

   char szToCheck[ ZH_PATH_MAX ];

   PZH_FNAME pFileName = zh_fsFNameSplit( pszFileName );

   *szCommitYear = '\0';

   if( ! pFileName->szName )
   {
      static const char * s_szNames[] = {
         "ChangeLog.txt",
         NULL
      };
      int i = 0;

      if( ! pFileName->szPath )
         pFileName->szPath = "../../../../..";

      pszFileName = s_szNames[ i++ ];
      while( pszFileName )
      {
         pFileName->szName = pszFileName;
         zh_fsFNameMerge( szToCheck, pFileName );

         if( zh_fsFileExists( szToCheck ) )
         {
            pszFileName = szToCheck;
            break;
         }

         if( strchr( szToCheck, '?' ) != NULL )
         {
            pszFree = zh_fsFileFind( szToCheck );
            if( pszFree )
            {
               pszFileName = pszFree;
               break;
            }
         }

         pszFileName = s_szNames[ i++ ];
      }

      if( ! pszFileName )
         pszFileName = s_szNames[ 0 ];
   }

   zh_xfree( pFileName );

   file_in = zh_fopen( pszFileName, "r" );
   if( ! file_in )
   {
      if( iQuiet < 2 )
      {
         perror( pszFileName );
      }
      iResult = 1;
   }
   else
   {
      char szLine[ 256 ];
      char szLog[ 128 ];
      int iLen;

      if( iQuiet == 0 )
         fprintf( stdout, "Reading ChangeLog file: %s\n", pszFileName );

      *szLog = '\0';

      do
      {
         if( ! fgets( szLine, sizeof( szLine ), file_in ) )
            break;

         if( ! *szLog )
         {
            if( szLine[ 4 ] == '-' && szLine[ 7 ] == '-' &&
                szLine[ 10 ] == ' ' && szLine[ 13 ] == ':' )
            {
               zh_strncpy( szLog, szLine, sizeof( szLog ) - 1 );
               iLen = ( int ) strlen( szLog );
               while( iLen-- && ZH_ISSPACE( szLog[ iLen ] ) )
                  szLog[ iLen ] = '\0';
            }
         }
      }
      while( ! *szLog );

      fclose( file_in );

      if( ! *szLog )
      {
         if( iQuiet < 2 )
            fprintf( stderr, "Cannot find valid $" "Id entry in the %s file.\n", pszFileName );
         iResult = 1;
      }
      else
      {
         *szLine = '"';
         zh_strncpy( szLine + 1, szLog, sizeof( szLine ) - 3 );
         iLen = ( int ) strlen( szLine );
         szLine[ iLen ] = '"';
         szLine[ ++iLen ] = '\0';
         zh_pp_addDefine( pState, "ZH_VER_COMMIT_INFO", szLine );
         *pszCommitInfo = zh_strdup( szLog );

         *piCommitRev = zh_pp_TimeStampToNum( pState, szLog, szCommitYear );

         *pszCommitYear = zh_strdup( szCommitYear );
      }
   }

   if( pszFree )
      zh_xfree( pszFree );

   return iResult;
}

#define _VALUE_SIZE  128

static int zh_pp_parseRepoVer( PZH_PP_STATE pState, const char * pszFileName,
                               int iQuiet,
                               char ** pszCommitID, char ** pszCommitIDShort,
                               char ** pszCommitYear,
                               int * piCommitRev, char ** pszCommitInfo,
                               char ** pszURL )
{
   FILE * file_in;

   char szId[ _VALUE_SIZE ];
   char szIdShort[ _VALUE_SIZE ];
   char szDate[ _VALUE_SIZE ];
   char szName[ _VALUE_SIZE ];
   char szMail[ _VALUE_SIZE ];
   char szCommitYear[ 5 ];
   char szCommitInfo[ _VALUE_SIZE ];
   char szURL[ _VALUE_SIZE ];

   int iLen;

   *szId = *szIdShort = *szDate = *szName = *szMail =
      *szCommitYear = *szCommitInfo = *szURL = '\0';

   file_in = zh_fopen( pszFileName, "r" );
   if( ! file_in )
   {
      if( iQuiet < 2 )
         fprintf( stderr, "'%s' not found. Skipping.\n", pszFileName );
   }
   else
   {
      char szLine[ 256 ];
      char * pszValue;

      if( iQuiet == 0 )
         fprintf( stdout, "Reading repository revision file: %s\n", pszFileName );

      for( ;; )
      {
         if( ! fgets( szLine, sizeof( szLine ), file_in ) )
            break;

         if( ! *szId )
            pszValue = szId;
         else if( ! *szIdShort )
            pszValue = szIdShort;
         else if( ! *szDate )
            pszValue = szDate;
         else if( ! *szName )
            pszValue = szName;
         else if( ! *szMail )
            pszValue = szMail;
         else if( ! *szURL )
            pszValue = szURL;
         else
            break;

         {
            zh_strncpy( pszValue, szLine, _VALUE_SIZE - 1 );
            iLen = ( int ) strlen( pszValue );
            while( iLen-- && ZH_ISSPACE( pszValue[ iLen ] ) )
               pszValue[ iLen ] = '\0';
         }
      }

      fclose( file_in );
   }

   /* Default value if not building from Git source */
   if( ! *szURL )
      zh_strncpy( szURL, _DEFAULT_ORIGIN_URL, sizeof( szURL ) - 1 );
   /* Strip .git suffix from URL, if any */
   if( strlen( szURL ) >= 4 && strncmp( szURL + strlen( szURL ) - 4, ".git", 4 ) == 0 )
      szURL[ strlen( szURL ) - 4 ] = '\0';
   /* Make sure to end with a slash */
   if( strlen( szURL ) >= 1 && szURL[ strlen( szURL ) - 1 ] != '/' )
      zh_strncat( szURL, "/", sizeof( szURL ) - 1 );

   zh_pp_addDefine( pState, "ZH_VER_COMMIT_ID", szId );
   zh_pp_addDefine( pState, "ZH_VER_COMMIT_ID_SHORT", szIdShort );
   zh_pp_addDefine( pState, "ZH_VER_ORIGIN_URL", szURL );

   *pszCommitID = zh_strdup( szId );
   *pszCommitIDShort = zh_strdup( szIdShort );
   *pszURL = zh_strdup( szURL );

   if( szDate[ 0 ] && szName[ 0 ] && szMail[ 0 ] )
   {
      iLen = ( int ) strlen( szMail );
      while( iLen-- )
      {
         if( szMail[ iLen ] == '@' )
            szMail[ iLen ] = ' ';
      }
#if defined( __ZH_INCLUDE_MORE_COMMIT_INFO )
      zh_snprintf( szCommitInfo, sizeof( szCommitInfo ), "%s %s (%s)", szDate, szName, szMail );
#else
      zh_snprintf( szCommitInfo, sizeof( szCommitInfo ), "%s", szDate );
      ZH_SYMBOL_UNUSED( szName );
      ZH_SYMBOL_UNUSED( szMail );
#endif
      if( *pszCommitInfo )
         zh_xfree( *pszCommitInfo );

      zh_pp_delDefine( pState, "ZH_VER_COMMIT_INFO" );
      zh_pp_addDefine( pState, "ZH_VER_COMMIT_INFO", szCommitInfo );

      *pszCommitInfo = zh_strdup( szCommitInfo );

      *piCommitRev = zh_pp_TimeStampToNum( pState, szCommitInfo, szCommitYear );

      *pszCommitYear = zh_strdup( szCommitYear );
   }

   return 0;
}

/*
 * ppgen only functions
 */
static void zh_pp_usage( char * szName )
{
   printf( "\n" );
   printf( "Syntax:  %s <file[.prg]> [options]\n\n", szName );
   printf( "Options:  -d<id>[=<val>]\t#define <id>\n"
           "          -e[<func>]    \tuse <func> as entry function in generated .c\n"
           "          -i<path>      \tadd #include file search path\n"
           "          -u[<file>]    \tuse command def set in <file> (or none)\n"
           "          -c[<file>]    \tlook for ChangeLog file\n"
           "          -r[<file>]    \tlook for repo revision file\n"
           "          -o<file>      \tcreates .c file with PP rules\n"
           "          -v<file>      \tcreates .h file with version information\n"
           "          -w            \twrite preprocessed (.ppo) file\n"
           "          -q[012]       \tdisable information messages\n" );
   printf( "\n"
           "Note:  if neither -o nor -v is specified then -w is default action\n\n" );
}

int main( int argc, char * argv[] )
{
   char * szFile = NULL, * szRuleFile = NULL, * szVerFile = NULL;
   char * szStdCh = NULL, * szLogFile = NULL, * szRepoVerFile = NULL;
   ZH_BOOL fWrite = ZH_FALSE, fChgLog = ZH_FALSE, fRepoVer = ZH_FALSE;
   char * szCommitID = NULL, * szCommitIDShort = NULL;
   char * szCommitYear = NULL, * szCommitInfo = NULL, * szURL = NULL;
   int iCommitRev = 0, iResult = 0, iQuiet = 0;
   char * szPPRuleFuncName = NULL;
   PZH_PP_STATE pState;

   pState = zh_pp_new();

   if( argc >= 2 )
   {
      int i;

      szFile = argv[ 1 ];
      for( i = 2; szFile && i < argc; i++ )
      {
         if( ! ZH_ISOPTSEP( argv[ i ][ 0 ] ) )
            szFile = NULL;
         else
         {
            switch( argv[ i ][ 1 ] )
            {
               case 'q':
               case 'Q':
                  if( ! argv[ i ][ 2 ] )
                     iQuiet = 1;
                  else if( argv[ i ][ 2 ] == '-' && ! argv[ i ][ 3 ] )
                     iQuiet = 0;
                  else if( argv[ i ][ 2 ] >= '0' && argv[ i ][ 2 ] <= '2' && ! argv[ i ][ 3 ] )
                     iQuiet = argv[ i ][ 2 ] - '0';
                  else
                     szFile = NULL;
                  break;

               case 'd':
               case 'D':
                  if( ! argv[ i ][ 2 ] )
                     szFile = NULL;
                  else
                  {
                     char * szDefText = zh_strdup( argv[ i ] + 2 ), * szAssign;

                     szAssign = strchr( szDefText, '=' );
                     if( szAssign )
                        *szAssign++ = '\0';
                     zh_pp_addDefine( pState, szDefText, szAssign );
                     zh_xfree( szDefText );
                  }
                  break;

               case 'e':
               case 'E':
                  if( argv[ i ][ 2 ] )
                     szPPRuleFuncName = argv[ i ] + 2;
                  else
                     szPPRuleFuncName = NULL;
                  break;

               case 'w':
               case 'W':
                  if( argv[ i ][ 2 ] )
                     szFile = NULL;
                  else
                     fWrite = ZH_TRUE;
                  break;

               case 'c':
               case 'C':
                  fChgLog = ZH_TRUE;
                  if( argv[ i ][ 2 ] )
                     szLogFile = argv[ i ] + 2;
                  break;

               case 'r':
               case 'R':
                  fRepoVer = ZH_TRUE;
                  if( argv[ i ][ 2 ] )
                     szRepoVerFile = argv[ i ] + 2;
                  break;

               case 'i':
               case 'I':
                  if( argv[ i ][ 2 ] )
                     zh_pp_addSearchPath( pState, argv[ i ] + 2, ZH_FALSE );
                  else
                     szFile = NULL;
                  break;

               case 'o':
               case 'O':
                  if( argv[ i ][ 2 ] )
                     szRuleFile = argv[ i ] + 2;
                  else
                     szFile = NULL;
                  break;

               case 'v':
               case 'V':
                  if( argv[ i ][ 2 ] )
                     szVerFile = argv[ i ] + 2;
                  else
                     szFile = NULL;
                  break;

               case 'u':
               case 'U':
                  if( argv[ i ][ 2 ] )
                     szStdCh = argv[ i ] + 2;
                  else
                     szStdCh = NULL;
                  break;

               default:
                  szFile = NULL;
                  break;
            }
         }
      }
   }

   if( iQuiet < 2 )
   {
      printf( "Ziher Preprocessor %d.%d.%d%s\n",
              ZH_VER_MAJOR, ZH_VER_MINOR, ZH_VER_RELEASE, ZH_VER_STATUS );
      printf( "Copyright (c) 1999-present, %s\n", _DEFAULT_ORIGIN_URL );
   }

   if( szFile )
   {
      char * szInclude;

      if( ! szRuleFile && ! szVerFile )
         fWrite = ZH_TRUE;

      zh_pp_init( pState, iQuiet != 0, ZH_TRUE, 0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL );

      szInclude = zh_getenv( "INCLUDE" );
      if( szInclude )
      {
         if( szInclude[ 0 ] )
            zh_pp_addSearchPath( pState, szInclude, ZH_FALSE );
         zh_xfree( szInclude );
      }

      if( szStdCh )
         zh_pp_readRules( pState, szStdCh );

      if( zh_pp_inFile( pState, szFile, ZH_TRUE, NULL, ZH_TRUE ) )
      {
         if( fWrite )
         {
            char szFileName[ ZH_PATH_MAX ];
            PZH_FNAME pFileName;

            pFileName = zh_fsFNameSplit( szFile );
            pFileName->szExtension = ".ppo";
            zh_fsFNameMerge( szFileName, pFileName );
            zh_xfree( pFileName );

            zh_pp_outFile( pState, szFileName, NULL );
         }

         if( fChgLog )
            iResult = zh_pp_parseChangelog( pState, szLogFile, iQuiet,
                                            &szCommitYear, &iCommitRev, &szCommitInfo );
         if( fRepoVer )
            iResult = zh_pp_parseRepoVer( pState, szRepoVerFile, iQuiet,
                                          &szCommitID, &szCommitIDShort, &szCommitYear, &iCommitRev, &szCommitInfo, &szURL );

         if( iResult == 0 )
            iResult = zh_pp_preprocesfile( pState, szRuleFile, szPPRuleFuncName );

         if( iResult == 0 && szVerFile && szRepoVerFile )
            iResult = zh_pp_generateVerInfo( szVerFile, szCommitYear, iCommitRev, szCommitInfo, szCommitID, szCommitIDShort, szURL );

         if( iResult == 0 && zh_pp_errorCount( pState ) > 0 )
            iResult = 1;
      }
      else
         iResult = 1;
   }
   else
   {
      zh_pp_usage( argv[ 0 ] );
      iResult = 1;
   }

   if( szCommitID )
      zh_xfree( szCommitID );
   if( szCommitIDShort )
      zh_xfree( szCommitIDShort );
   if( szCommitYear )
      zh_xfree( szCommitYear );
   if( szCommitInfo )
      zh_xfree( szCommitInfo );
   if( szURL )
      zh_xfree( szURL );

   zh_pp_free( pState );

   return iResult;
}

