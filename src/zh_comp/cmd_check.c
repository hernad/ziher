/*
 * Compiler command-line and environment parameters checking
 *
 * Copyright 2015 Przemyslaw Czerpak
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
#include "zh_set.h"

static char s_szUndefineMarker[ 1 ] = "";  /* TODO: add const */

static ZH_SIZE zh_compChkOptionLen( const char * szSwitch, ZH_BOOL fEnv )
{
   ZH_SIZE nLen;

   if( fEnv )
   {
      nLen = 0;
      while( szSwitch[ nLen ] != '\0' &&
             szSwitch[ nLen ] != ' ' && szSwitch[ nLen ] != '-' )
         ++nLen;
   }
   else
      nLen = strlen( szSwitch );

   return nLen;
}

static const char * zh_compChkAddDefine( ZH_COMP_DECL, const char * szSwitch,
                                         ZH_BOOL fAdd, ZH_BOOL fEnv )
{
   const char * szSwPtr = szSwitch;
   ZH_SIZE nValue = 0;

   while( *szSwPtr && *szSwPtr != ' ' && ! ZH_ISOPTSEP( *szSwPtr ) )
   {
      if( *szSwPtr == '=' )
      {
         nValue = szSwPtr - szSwitch;
         szSwPtr += zh_compChkOptionLen( szSwPtr, fEnv );
         break;
      }
      ++szSwPtr;
   }
   if( szSwPtr > szSwitch && *szSwitch != '=' )
   {
      char * szDefine = zh_strndup( szSwitch, szSwPtr - szSwitch );
      char * szValue = NULL;
      PZH_PPDEFINE * pDefinePtr;

      if( nValue )
      {
         szValue = szDefine + nValue;
         *szValue++ = '\0';
      }
      if( ! fAdd )
         szValue = s_szUndefineMarker;

      pDefinePtr = &ZH_COMP_PARAM->ppdefines;
      while( *pDefinePtr != NULL &&
             strcmp( ( *pDefinePtr )->szName, szDefine ) != 0 )
         pDefinePtr = &( *pDefinePtr )->pNext;

      if( *pDefinePtr == NULL )
      {
         *pDefinePtr = ( PZH_PPDEFINE ) zh_xgrab( sizeof( ZH_PPDEFINE ) );
         ( *pDefinePtr )->pNext = NULL;
      }
      else
         zh_xfree( ( *pDefinePtr )->szName );
      ( *pDefinePtr )->szName = szDefine;
      ( *pDefinePtr )->szValue = szValue;
   }
   return szSwPtr;
}

static void zh_compChkIgnoredInfo( ZH_COMP_DECL, const char * szSwitch )
{
   char buffer[ 64 ];

   zh_snprintf( buffer, sizeof( buffer ),
                "Ignored unsupported command-line option: %s\n", szSwitch );
   zh_compOutStd( ZH_COMP_PARAM, buffer );
}

static char * zh_compChkOptionDup( const char * szSwitch )
{
   return zh_strupr( zh_strndup( szSwitch,
                                 zh_compChkOptionLen( szSwitch, ZH_TRUE ) ) );
}

static const char * zh_compChkOptionGet( const char * szSwitch,
                                         char ** pszResult, ZH_BOOL fEnv )
{
   ZH_SIZE nLen = zh_compChkOptionLen( szSwitch, fEnv );

   if( pszResult )
      *pszResult = zh_strndup( szSwitch, nLen );

   return szSwitch + nLen;
}

static const char * zh_compChkOptionFName( const char * szSwitch,
                                           PZH_FNAME * pResult, ZH_BOOL fEnv )
{
   ZH_SIZE nLen = zh_compChkOptionLen( szSwitch, fEnv );

   if( nLen > 0 )
   {
      if( *pResult )
         zh_xfree( *pResult );
      if( szSwitch[ nLen ] != '\0' )
      {
         char * szVal = zh_strndup( szSwitch, nLen );
         *pResult = zh_fsFNameSplit( szVal );
         zh_xfree( szVal );
      }
      else
         *pResult = zh_fsFNameSplit( szSwitch );
   }
   return szSwitch + nLen;
}

static const char * zh_compChkOptionAddPath( ZH_COMP_DECL, const char * szSwitch,
                                             ZH_BOOL fEnv )
{
   ZH_SIZE nLen = zh_compChkOptionLen( szSwitch, fEnv );

   if( nLen > 0 )
   {
      if( szSwitch[ nLen ] != '\0' )
      {
         char * szVal = zh_strndup( szSwitch, nLen );
         zh_pp_addSearchPath( ZH_COMP_PARAM->pLex->pPP, szSwitch, ZH_FALSE );
         zh_xfree( szVal );
      }
      else
         zh_pp_addSearchPath( ZH_COMP_PARAM->pLex->pPP, szSwitch, ZH_FALSE );
   }
   return szSwitch + nLen;
}

static const char * zh_compChkParseSwitch( ZH_COMP_DECL, const char * szSwitch,
                                           ZH_BOOL fEnv )
{
   const char * szSwPtr = szSwitch;

   if( szSwPtr[ 0 ] == '-' && szSwPtr[ 1 ] == '-' )
   {
      if( strncmp( szSwPtr + 2, "version", 7 ) == 0 )
      {
         szSwPtr += 9;
         ZH_COMP_PARAM->fLogo = ZH_TRUE;
         ZH_COMP_PARAM->fQuiet = ZH_TRUE;
      }
      else if( strncmp( szSwPtr + 2, "help", 4 ) == 0 )
      {
         szSwPtr += 6;
         ZH_COMP_PARAM->fLogo = ZH_TRUE;
         ZH_COMP_PARAM->fQuiet = ZH_FALSE;
         ZH_COMP_PARAM->fExit = ZH_FALSE;
      }
   }
   else if( ZH_ISOPTSEP( *szSwPtr ) )
   {
      ++szSwPtr;
      switch( ZH_TOUPPER( *szSwPtr ) )
      {
         case 'A':
            ++szSwPtr;
            if( *szSwPtr == '-' )
            {
               ++szSwPtr;
               ZH_COMP_PARAM->fAutoMemvarAssume = ZH_FALSE;
            }
            else
               ZH_COMP_PARAM->fAutoMemvarAssume = ZH_TRUE;
            break;

         case 'B':
         {
            char *szOption = zh_compChkOptionDup( szSwPtr );

            if( strcmp( szOption, "BUILD" ) == 0 )
            {
               ZH_COMP_PARAM->fBuildInfo = ZH_TRUE;
               szSwPtr += 5;
            }
            else if( szSwPtr[ 1 ] == '-' )
            {
               ZH_COMP_PARAM->fDebugInfo = ZH_FALSE;
               szSwPtr += 2;
            }
            else
            {
               ZH_COMP_PARAM->fDebugInfo = ZH_TRUE;
               ZH_COMP_PARAM->fLineNumbers = ZH_TRUE;
               ++szSwPtr;
            }
            ZH_COMP_PARAM->fHideSource = ! ZH_COMP_PARAM->fDebugInfo;
            zh_xfree( szOption );
            break;
         }

         case 'C':
         {
            char *szOption = zh_compChkOptionDup( szSwPtr );

            if( strlen( szOption ) >= 4 &&
                strncmp( "CREDITS", szOption, strlen( szOption ) ) == 0 )
            {
               ZH_COMP_PARAM->fCredits = ZH_TRUE;
               szSwPtr += strlen( szOption );
            }
            zh_xfree( szOption );
            break;
         }

         case 'D':
            szSwPtr = zh_compChkAddDefine( ZH_COMP_PARAM, szSwPtr + 1, ZH_TRUE, fEnv );
            break;

         case 'E':
            if( ZH_TOUPPER( szSwPtr[ 1 ] ) == 'S' )
            {
               switch( szSwPtr[ 2 ] )
               {
                  case '1':
                     szSwPtr += 3;
                     ZH_COMP_PARAM->iExitLevel = ZH_EXITLEVEL_SETEXIT;
                     break;
                  case '2':
                     szSwPtr += 3;
                     ZH_COMP_PARAM->iExitLevel = ZH_EXITLEVEL_DELTARGET;
                     break;
                  case '0':
                     ++szSwPtr;
                     /* fallthrough */
                  default:
                     szSwPtr += 2;
                     ZH_COMP_PARAM->iExitLevel = ZH_EXITLEVEL_DEFAULT;
                     break;
               }
            }
            break;

         case 'F':
            switch( ZH_TOUPPER( szSwPtr[ 1 ] ) )
            {
               case 'N':
                  if( szSwPtr[ 2 ] == ':' )
                  {
                     if( ZH_TOUPPER( szSwPtr[ 3 ] ) == 'U' )
                     {
                        szSwPtr += 4;
                        zh_setSetFileCase( ZH_SET_CASE_UPPER );
                     }
                     else if( ZH_TOUPPER( szSwPtr[ 3 ] ) == 'L' )
                     {
                        szSwPtr += 4;
                        zh_setSetFileCase( ZH_SET_CASE_LOWER );
                     }
                  }
                  else
                  {
                     szSwPtr += 2;
                     if( *szSwPtr == '-' )
                        ++szSwPtr;
                     zh_setSetFileCase( ZH_SET_CASE_MIXED );
                  }
                  break;
               case 'D':
                  if( szSwPtr[ 2 ] == ':' )
                  {
                     if( ZH_TOUPPER( szSwPtr[ 3 ] ) == 'U' )
                     {
                        szSwPtr += 4;
                        zh_setSetDirCase( ZH_SET_CASE_UPPER );
                     }
                     else if( ZH_TOUPPER( szSwPtr[ 3 ] ) == 'L' )
                     {
                        szSwPtr += 4;
                        zh_setSetDirCase( ZH_SET_CASE_LOWER );
                     }
                  }
                  else
                  {
                     szSwPtr += 2;
                     if( *szSwPtr == '-' )
                        ++szSwPtr;
                     zh_setSetDirCase( ZH_SET_CASE_MIXED );
                  }
                  break;
               case 'P':
                  szSwPtr += 2;
                  if( *szSwPtr == ':' )
                  {
                     if( szSwPtr[ 1 ] && szSwPtr[ 1 ] != ' ' )
                     {
                        zh_setSetDirSeparator( szSwPtr[ 1 ] );
                        szSwPtr += 2;
                     }
                  }
                  else
                  {
                     if( *szSwPtr == '-' )
                        ++szSwPtr;
                     zh_setSetDirSeparator( ZH_OS_PATH_DELIM_CHR );
                  }
                  break;
               case 'S':
                  szSwPtr += 2;
                  if( *szSwPtr == '-' )
                  {
                     ++szSwPtr;
                     zh_setSetTrimFileName( ZH_FALSE );
                  }
                  else
                     zh_setSetTrimFileName( ZH_TRUE );
            }
            break;

         case 'G':
            switch( ZH_TOUPPER( szSwPtr[ 1 ] ) )
            {
               case 'C':
                  ZH_COMP_PARAM->iLanguage = ZH_LANG_C;
                  szSwPtr += 2;
                  switch( *szSwPtr )
                  {
                     case '1':
                        ++szSwPtr;
                        ZH_COMP_PARAM->iGenCOutput = ZH_COMPGENC_NORMAL;
                        break;
                     case '2':
                        ++szSwPtr;
                        ZH_COMP_PARAM->iGenCOutput = ZH_COMPGENC_VERBOSE;
                        break;
                     case '3':
                        ++szSwPtr;
                        ZH_COMP_PARAM->iGenCOutput = ZH_COMPGENC_REALCODE;
                        break;
                     case '0':
                        ++szSwPtr;
                        /* fallthrough */
                     default:
                        ZH_COMP_PARAM->iGenCOutput = ZH_COMPGENC_COMPACT;
                        break;
                  }
                  break;

               case 'H':
                  ZH_COMP_PARAM->iLanguage = ZH_LANG_PORT_OBJ;
                  szSwPtr += 2;
                  break;

               case 'D':
                  if( ZH_COMP_PARAM->szDepExt )
                  {
                     zh_xfree( ZH_COMP_PARAM->szDepExt );
                     ZH_COMP_PARAM->szDepExt = NULL;
                  }
                  szSwPtr += 2;
                  if( *szSwPtr == '-' )
                  {
                     ZH_COMP_PARAM->iTraceInclude = 0;
                     ++szSwPtr;
                  }
                  else
                  {
                     ZH_COMP_PARAM->iTraceInclude = 2;
                     if( *szSwPtr == '.' )
                        szSwPtr = zh_compChkOptionGet( szSwPtr, &ZH_COMP_PARAM->szDepExt, fEnv );
                  }
                  break;

               case 'E':
                  szSwPtr += 2;
                  switch( *szSwPtr )
                  {
                     case '1':
                        ++szSwPtr;
                        ZH_COMP_PARAM->iErrorFmt = ZH_ERRORFMT_IDE;
                        break;
                     case '0':
                        ++szSwPtr;
                        /* fallthrough */
                     default:
                        ZH_COMP_PARAM->iErrorFmt = ZH_ERRORFMT_DEFAULT;
                        break;
                  }
                  break;

               default:
                  zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'F', ZH_COMP_ERR_UNSUPPORTED_LANG, NULL, NULL );
                  break;
            }
            break;

         case 'H':
         case '?':
            /* HELP message */
            break;

         case 'I':
            ++szSwPtr;
            switch( *szSwPtr )
            {
               case '-':
                  ZH_COMP_PARAM->fINCLUDE = ZH_FALSE;
                  ++szSwPtr;
                  break;
               case '+':
                  ZH_COMP_PARAM->fINCLUDE = ZH_TRUE;
                  ++szSwPtr;
                  break;
               default:
                  szSwPtr = zh_compChkOptionAddPath( ZH_COMP_PARAM, szSwPtr, fEnv );
                  break;
            }
            break;

         case 'J':
            ++szSwPtr;
            ZH_COMP_PARAM->fI18n = ZH_TRUE;
            if( *szSwPtr )
               szSwPtr = zh_compChkOptionFName( szSwPtr, &ZH_COMP_PARAM->pI18nFileName, fEnv );
            break;

         case 'K':
            ++szSwPtr;
            while( *szSwPtr && ! ZH_COMP_PARAM->fExit )
            {
               int ch = ZH_TOUPPER( *szSwPtr );

               ++szSwPtr;
               switch( ch )
               {
                  case '?':
                     zh_compPrintLogo( ZH_COMP_PARAM );
                     zh_compPrintModes( ZH_COMP_PARAM );
                     ZH_COMP_PARAM->fLogo = ZH_FALSE;
                     ZH_COMP_PARAM->fQuiet = ZH_TRUE;
                     break;

                  case 'H':
                     /* default Ziher mode */
                     if( *szSwPtr == '-' )
                     {
                        ZH_COMP_PARAM->supported &= ~ZH_COMPFLAG_ZIHER;
                        ++szSwPtr;
                     }
                     else
                        ZH_COMP_PARAM->supported |= ZH_COMPFLAG_ZIHER;
                     break;

                  case 'C':
                     /* clear all flags - minimal set of features */
                     ZH_COMP_PARAM->supported &= ZH_COMPFLAG_SHORTCUTS;
                     ZH_COMP_PARAM->supported |= ZH_COMPFLAG_OPTJUMP |
                                                 ZH_COMPFLAG_MACROTEXT;
                     break;

                  case 'X':
                     if( *szSwPtr == '-' )
                     {
                        ZH_COMP_PARAM->supported &= ~ZH_COMPFLAG_XBASE;
                        ++szSwPtr;
                     }
                     else
                        ZH_COMP_PARAM->supported |= ZH_COMPFLAG_XBASE;
                     break;

                  case 'I':
                     if( *szSwPtr == '-' )
                     {
                        ZH_COMP_PARAM->supported &= ~ZH_COMPFLAG_ZH_INLINE;
                        ++szSwPtr;
                     }
                     else
                        ZH_COMP_PARAM->supported |= ZH_COMPFLAG_ZH_INLINE;
                     break;

                  case 'J':
                     if( *szSwPtr == '+' )
                     {
                        ZH_COMP_PARAM->supported |= ZH_COMPFLAG_OPTJUMP;
                        ++szSwPtr;
                     }
                     else
                        ZH_COMP_PARAM->supported &= ~ZH_COMPFLAG_OPTJUMP;
                     break;

                  case 'M':
                     if( *szSwPtr == '+' )
                     {
                        ZH_COMP_PARAM->supported |= ZH_COMPFLAG_MACROTEXT;
                        ++szSwPtr;
                     }
                     else
                        ZH_COMP_PARAM->supported &= ~ZH_COMPFLAG_MACROTEXT;
                     break;

                  case 'D':
                     if( *szSwPtr == '-' )
                     {
                        ZH_COMP_PARAM->supported &= ~ZH_COMPFLAG_MACRODECL;
                        ++szSwPtr;
                     }
                     else
                        ZH_COMP_PARAM->supported |= ZH_COMPFLAG_MACRODECL;
                     break;

                  case 'R':
                     if( *szSwPtr == '-' )
                     {
                        ZH_COMP_PARAM->supported &= ~ZH_COMPFLAG_RT_MACRO;
                        ++szSwPtr;
                     }
                     else
                        ZH_COMP_PARAM->supported |= ZH_COMPFLAG_RT_MACRO;
                     break;

                  case 'S':
                     if( *szSwPtr == '-' )
                     {
                        ZH_COMP_PARAM->supported &= ~ZH_COMPFLAG_ARRSTR;
                        ++szSwPtr;
                     }
                     else
                        ZH_COMP_PARAM->supported |= ZH_COMPFLAG_ARRSTR;
                     break;

                  case 'O':
                     if( *szSwPtr == '-' )
                     {
                        ZH_COMP_PARAM->supported &= ~ZH_COMPFLAG_EXTOPT;
                        ++szSwPtr;
                     }
                     else
                        ZH_COMP_PARAM->supported |= ZH_COMPFLAG_EXTOPT;
                     break;

                  case 'U':
                     if( *szSwPtr == '-' )
                     {
                        ZH_COMP_PARAM->supported &= ~ZH_COMPFLAG_USERCP;
                        ++szSwPtr;
                     }
                     else
                        ZH_COMP_PARAM->supported |= ZH_COMPFLAG_USERCP;
                     break;

                  default:
                     ch = -1;
                     --szSwPtr;
                     break;
               }
               if( ch == -1 )
                  break;
            }
            break;

         case 'L':
            ++szSwPtr;
            if( *szSwPtr == '-' )
            {
               ZH_COMP_PARAM->fLineNumbers = ZH_TRUE;
               ++szSwPtr;
            }
            else
               ZH_COMP_PARAM->fLineNumbers = ZH_FALSE;
            break;

         case 'M':
            ++szSwPtr;
            if( *szSwPtr == '-' )
            {
               ZH_COMP_PARAM->fSingleModule = ZH_FALSE;
               ++szSwPtr;
            }
            else
               ZH_COMP_PARAM->fSingleModule = ZH_TRUE;
            break;

         case 'N':
            ++szSwPtr;
            ZH_COMP_PARAM->fNoStartUp = *szSwPtr == '1';
            switch( *szSwPtr )
            {
               case '-':
                  ZH_COMP_PARAM->iStartProc = 0;
                  ++szSwPtr;
                  break;
               case '2':
                  ZH_COMP_PARAM->iStartProc = 2;
                  ++szSwPtr;
                  break;
               case '0':
               case '1':
                  ++szSwPtr;
                  /* fallthrough */
               default:
                  ZH_COMP_PARAM->iStartProc = 1;
                  break;
            }
            break;

         case 'O':
            szSwPtr = zh_compChkOptionFName( szSwPtr + 1, &ZH_COMP_PARAM->pOutPath, fEnv );
            break;

         case 'P':
            ++szSwPtr;
            if( *szSwPtr == '+' )
            {
               ZH_COMP_PARAM->fPPT = ZH_TRUE;
               ++szSwPtr;
            }
            else
            {
               if( ZH_COMP_PARAM->pPpoPath )
               {
                  zh_xfree( ZH_COMP_PARAM->pPpoPath );
                  ZH_COMP_PARAM->pPpoPath = NULL;
               }
               if( *szSwPtr == '-' )
               {
                  ZH_COMP_PARAM->fPPT = ZH_COMP_PARAM->fPPO = ZH_FALSE;
                  ++szSwPtr;
               }
               else
               {
                  if( *szSwPtr )
                     szSwPtr = zh_compChkOptionFName( szSwPtr, &ZH_COMP_PARAM->pPpoPath, fEnv );
                  ZH_COMP_PARAM->fPPO = ZH_TRUE;
               }
            }
            break;

         case 'Q':
            ++szSwPtr;
            switch( *szSwPtr )
            {
               case 'l':
               case 'L':
                  ZH_COMP_PARAM->fGauge = ZH_FALSE;
                  ++szSwPtr;
                  break;
               case '2':
                  ZH_COMP_PARAM->fFullQuiet = ZH_TRUE;
                  /* fallthrough */
               case '0':
                  ZH_COMP_PARAM->fLogo = ZH_FALSE;
                  ++szSwPtr;
                  /* fallthrough */
               default:
                  ZH_COMP_PARAM->fQuiet = ZH_TRUE;
                  break;
            }
            break;

         case 'R':
            ++szSwPtr;
            if( szSwPtr[ 0 ] == ':' )
            {
               if( ZH_ISDIGIT( szSwPtr[ 1 ] ) )
               {
                  int iCycles = 0;
                  ++szSwPtr;
                  while( ZH_ISDIGIT( *szSwPtr ) )
                     iCycles = iCycles * 10 + *szSwPtr++ - '0';
                  if( iCycles > 0 )
                     ZH_COMP_PARAM->iMaxTransCycles = iCycles;
               }
            }
            else
            {
               /* NOTE: ignored for Cl*pper compatibility:
                        /r[<lib>] request linker to search <lib> (or none) */
               zh_compChkIgnoredInfo( ZH_COMP_PARAM, "-r[<lib>]" );
               szSwPtr = zh_compChkOptionGet( szSwPtr, NULL, fEnv );
            }
            break;

         case 'S':
            ++szSwPtr;
            switch( *szSwPtr )
            {
               case '-':
                  ZH_COMP_PARAM->iSyntaxCheckOnly = 0;
                  ++szSwPtr;
                  break;
               case 'm':
               case 'M':
                  ZH_COMP_PARAM->iSyntaxCheckOnly = 2;
                  ++szSwPtr;
                  break;
               default:
                  ZH_COMP_PARAM->iSyntaxCheckOnly = 1;
                  break;
            }
            break;

         case 'T':
            /* NOTE: ignored for Cl*pper compatibility:
                     /t<path> path for temp file creation */
            zh_compChkIgnoredInfo( ZH_COMP_PARAM, "-t<path>" );
            szSwPtr = zh_compChkOptionGet( szSwPtr + 1, NULL, fEnv );
            break;

         case 'U':
            if( zh_strnicmp( szSwPtr, "UNDEF:", 6 ) == 0 )
            {
               if( zh_strnicmp( szSwPtr + 6, ".ARCH.", 6 ) == 0 )
               {
                  ZH_COMP_PARAM->fNoArchDefs = ZH_TRUE;
                  szSwPtr += 12;
               }
               else
                  szSwPtr = zh_compChkAddDefine( ZH_COMP_PARAM, szSwPtr + 6, ZH_FALSE, fEnv );
               break;
            }
            ++szSwPtr;
            /* extended definitions file: -u+<file> */
            if( *szSwPtr == '+' )
            {
               if( szSwPtr[ 1 ] && zh_compChkOptionLen( szSwPtr + 1, fEnv ) > 0 )
               {
                  ZH_COMP_PARAM->szStdChExt = ( char ** )
                     ( ZH_COMP_PARAM->iStdChExt == 0 ?
                        zh_xgrab( sizeof( char * ) ) :
                        zh_xrealloc( ZH_COMP_PARAM->szStdChExt,
                                     ( ZH_COMP_PARAM->iStdChExt + 1 ) *
                                     sizeof( char * ) ) );
                  szSwPtr = zh_compChkOptionGet( szSwPtr + 1,
                                                 &ZH_COMP_PARAM->szStdChExt[ ZH_COMP_PARAM->iStdChExt++ ],
                                                 fEnv );
               }
            }
            else
            {
               if( ZH_COMP_PARAM->szStdCh )
                  zh_xfree( ZH_COMP_PARAM->szStdCh );
               szSwPtr = zh_compChkOptionGet( szSwPtr, &ZH_COMP_PARAM->szStdCh, fEnv );
            }
            break;

         case 'V':
            ++szSwPtr;
            if( *szSwPtr == '-' )
            {
               ZH_COMP_PARAM->fForceMemvars = ZH_FALSE;
               ++szSwPtr;
            }
            else
               ZH_COMP_PARAM->fForceMemvars = ZH_TRUE;
            break;

         case 'W':
            ++szSwPtr;
            ZH_COMP_PARAM->iWarnings = 1;
            if( *szSwPtr >= '0' && *szSwPtr <= '3' )
            {
               ZH_COMP_PARAM->iWarnings = *szSwPtr - '0';
               ++szSwPtr;
            }
            break;

#ifdef YYDEBUG
         case 'Y':
            ++szSwPtr;
            extern int zh_comp_yydebug;
            zh_comp_yydebug = ZH_TRUE;
            break;
#endif

         case 'Z':
            ++szSwPtr;
            if( *szSwPtr == '-' )
            {
               ZH_COMP_PARAM->supported |= ZH_COMPFLAG_SHORTCUTS;
               ++szSwPtr;
            }
            else
               ZH_COMP_PARAM->supported &= ~ZH_COMPFLAG_SHORTCUTS;
            break;
      }
   }

   if( ! ZH_COMP_PARAM->fExit )
   {
      if( szSwPtr - szSwitch <= 1 ||
          ( *szSwPtr != '\0' && *szSwPtr != ' ' && ! ZH_ISOPTSEP( *szSwPtr ) ) )
         zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'F',
                          fEnv ? ZH_COMP_ERR_BADOPTION : ZH_COMP_ERR_BADPARAM,
                          szSwitch, NULL );
      else
         return szSwPtr;
   }

   return "";
}

/* check command-line parameters */
void zh_compChkCommandLine( ZH_COMP_DECL, int argc, const char * const argv[] )
{
   int i;

   for( i = 1; i < argc && ! ZH_COMP_PARAM->fExit; ++i )
   {
      const char * szSwitch = argv[ i ];

      if( ZH_ISOPTSEP( szSwitch[ 0 ] ) )
      {
         do
            szSwitch = zh_compChkParseSwitch( ZH_COMP_PARAM, szSwitch, ZH_FALSE );
         while( *szSwitch != '\0' );
      }
   }
}

/* check environment parameters */
void zh_compChkEnvironment( ZH_COMP_DECL )
{
 
   char * szEnvCMD = zh_getenv( "ZIHERCMD" );


   if( szEnvCMD )
   {
      const char * szSwitch = szEnvCMD;

      while( *szSwitch )
      {
         while( *szSwitch == ' ' )
            ++szSwitch;
         if( *szSwitch )
            szSwitch = zh_compChkParseSwitch( ZH_COMP_PARAM, szSwitch, ZH_TRUE );
      }
      zh_xfree( szEnvCMD );
   }
}

void zh_compChkAddIncPaths( ZH_COMP_DECL )
{
   char * szInclude = zh_getenv( "INCLUDE" );

   if( szInclude )
   {
      if( szInclude[ 0 ] != '\0' )
         zh_pp_addSearchPath( ZH_COMP_PARAM->pLex->pPP, szInclude, ZH_FALSE );
      zh_xfree( szInclude );
   }
}

void zh_compChkSetDefines( ZH_COMP_DECL )
{
   PZH_PPDEFINE pDefine = ZH_COMP_PARAM->ppdefines;

   while( pDefine )
   {
      if( pDefine->szValue == s_szUndefineMarker )
         zh_pp_delDefine( ZH_COMP_PARAM->pLex->pPP, pDefine->szName );
      else
         zh_pp_addDefine( ZH_COMP_PARAM->pLex->pPP, pDefine->szName, pDefine->szValue );
      pDefine = pDefine->pNext;
   }
}
