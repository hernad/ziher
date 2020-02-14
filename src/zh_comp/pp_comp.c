/*
 * Compiler C source with real code generation
 *
 * Copyright 2006 Przemyslaw Czerpak < druzus /at/ priv.onet.pl >
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

static void zh_pp_ErrorGen( void * cargo,
                            const char * const szMsgTable[],
                            char cPrefix, int iErrorCode,
                            const char * szParam1, const char * szParam2 )
{
   ZH_COMP_DECL = ( PZH_COMP ) cargo;
   int iCurrLine = ZH_COMP_PARAM->currLine;
   const char * currModule = ZH_COMP_PARAM->currModule;

   ZH_COMP_PARAM->currLine = zh_pp_line( ZH_COMP_PARAM->pLex->pPP );
   ZH_COMP_PARAM->currModule = zh_pp_fileName( ZH_COMP_PARAM->pLex->pPP );
   if( cPrefix == 'W' )
      zh_compGenWarning( ZH_COMP_PARAM, szMsgTable, cPrefix, iErrorCode, szParam1, szParam2 );
   else
      zh_compGenError( ZH_COMP_PARAM, szMsgTable, cPrefix, iErrorCode, szParam1, szParam2 );
   ZH_COMP_PARAM->fError = ZH_FALSE;
   ZH_COMP_PARAM->currLine = iCurrLine;
   ZH_COMP_PARAM->currModule = currModule;
}

static void zh_pp_Disp( void * cargo, const char * szMessage )
{
   ZH_COMP_DECL = ( PZH_COMP ) cargo;

   zh_compOutStd( ZH_COMP_PARAM, szMessage );
}

static void zh_pp_PragmaDump( void * cargo, char * pBuffer, ZH_SIZE nSize,
                              int iLine )
{
   PZH_HINLINE pInline;

   pInline = zh_compInlineAdd( ( PZH_COMP ) cargo, NULL, iLine );
   pInline->pCode = ( ZH_BYTE * ) zh_xgrab( nSize + 1 );
   memcpy( pInline->pCode, pBuffer, nSize );
   pInline->pCode[ nSize ] = '\0';
   pInline->nPCodeSize = nSize;
}

static void zh_pp_zh_inLine( void * cargo, char * szFunc,
                             char * pBuffer, ZH_SIZE nSize, int iLine )
{
   ZH_COMP_DECL = ( PZH_COMP ) cargo;

   if( ZH_COMP_PARAM->iLanguage != ZH_LANG_C )
   {
      int iCurrLine = ZH_COMP_PARAM->currLine;
      ZH_COMP_PARAM->currLine = iLine;
      zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'F', ZH_COMP_ERR_REQUIRES_C, NULL, NULL );
      ZH_COMP_PARAM->fError = ZH_FALSE;
      ZH_COMP_PARAM->currLine = iCurrLine;
   }
   else
   {
      PZH_HINLINE pInline = zh_compInlineAdd( ZH_COMP_PARAM,
         zh_compIdentifierNew( ZH_COMP_PARAM, szFunc, ZH_IDENT_COPY ), iLine );
      pInline->pCode = ( ZH_BYTE * ) zh_xgrab( nSize + 1 );
      memcpy( pInline->pCode, pBuffer, nSize );
      pInline->pCode[ nSize ] = '\0';
      pInline->nPCodeSize = nSize;
   }
}

static ZH_BOOL zh_pp_CompilerSwitch( void * cargo, const char * szSwitch,
                                     int * piValue, ZH_BOOL fSet )
{
   ZH_COMP_DECL = ( PZH_COMP ) cargo;
   ZH_BOOL fError = ZH_FALSE;
   int iValue, i;

   iValue = *piValue;

   i = ( int ) strlen( szSwitch );
   if( i > 1 && ( ( int ) ( szSwitch[ i - 1 ] - '0' ) ) == iValue )
      --i;

   if( i == 1 )
   {
      switch( szSwitch[ 0 ] )
      {
         case 'a':
         case 'A':
            if( fSet )
               ZH_COMP_PARAM->fAutoMemvarAssume = iValue != 0;
            else
               iValue = ZH_COMP_PARAM->fAutoMemvarAssume ? 1 : 0;
            break;

         case 'b':
         case 'B':
            if( fSet )
            {
               ZH_COMP_PARAM->fDebugInfo = iValue != 0;
               ZH_COMP_PARAM->fHideSource = ! ZH_COMP_PARAM->fDebugInfo;
            }
            else
               iValue = ZH_COMP_PARAM->fDebugInfo ? 1 : 0;
            break;

         case 'j':
         case 'J':
            if( fSet )
               ZH_COMP_PARAM->fI18n = iValue != 0;
            else
               iValue = ZH_COMP_PARAM->fI18n ? 1 : 0;
            break;

         case 'l':
         case 'L':
            if( fSet )
               ZH_COMP_PARAM->fLineNumbers = iValue != 0;
            else
               iValue = ZH_COMP_PARAM->fLineNumbers ? 1 : 0;
            break;

         case 'n':
         case 'N':
            if( fSet )
               fError = ZH_TRUE;
            else
               iValue = ZH_COMP_PARAM->iStartProc;
            break;

         case 'p':
         case 'P':
            if( fSet )
               ZH_COMP_PARAM->fPPO = iValue != 0;
            else
               iValue = ZH_COMP_PARAM->fPPO ? 1 : 0;
            break;

         case 'q':
         case 'Q':
            if( fSet )
               ZH_COMP_PARAM->fQuiet = iValue != 0;
            else
               iValue = ZH_COMP_PARAM->fQuiet ? 1 : 0;
            break;

         case 'v':
         case 'V':
            if( fSet )
               ZH_COMP_PARAM->fForceMemvars = iValue != 0;
            else
               iValue = ZH_COMP_PARAM->fForceMemvars ? 1 : 0;
            break;

         case 'w':
         case 'W':
            if( fSet )
            {
               if( iValue >= 0 && iValue <= 3 )
                  ZH_COMP_PARAM->iWarnings = iValue;
               else
                  fError = ZH_TRUE;
            }
            else
               iValue = ZH_COMP_PARAM->iWarnings;
            break;

         case 'z':
         case 'Z':
            if( fSet )
            {
               if( iValue )
                  ZH_COMP_PARAM->supported &= ~ZH_COMPFLAG_SHORTCUTS;
               else
                  ZH_COMP_PARAM->supported |= ZH_COMPFLAG_SHORTCUTS;
            }
            else
               iValue = ( ZH_COMP_PARAM->supported & ZH_COMPFLAG_SHORTCUTS ) ? 0 : 1;
            break;

         default:
            fError = ZH_TRUE;
      }
   }
   else if( i == 2 )
   {
      if( szSwitch[ 0 ] == 'k' || szSwitch[ 0 ] == 'K' )
      {
         int iFlag = 0;
         /* -k? parameters are case sensitive */
         switch( szSwitch[ 1 ] )
         {
            case '?':
               if( fSet )
                  ZH_COMP_PARAM->supported = iValue;
               else
                  iValue = ZH_COMP_PARAM->supported;
               break;
            case 'c':
            case 'C':
               if( fSet )
               {
                  /* clear all flags - minimal set of features */
                  ZH_COMP_PARAM->supported &= ZH_COMPFLAG_SHORTCUTS;
                  ZH_COMP_PARAM->supported |= ZH_COMPFLAG_OPTJUMP |
                                              ZH_COMPFLAG_MACROTEXT;
               }
               else
               {
                  iValue = ( ZH_COMP_PARAM->supported & ~ZH_COMPFLAG_SHORTCUTS ) ==
                           ( ZH_COMPFLAG_OPTJUMP | ZH_COMPFLAG_MACROTEXT ) ? 1 : 0;
               }
               break;
            case 'h':
            case 'H':
               iFlag = ZH_COMPFLAG_ZIHER;
               break;
            case 'o':
            case 'O':
               iFlag = ZH_COMPFLAG_EXTOPT;
               break;
            case 'i':
            case 'I':
               iFlag = ZH_COMPFLAG_ZH_INLINE;
               break;
            case 'r':
            case 'R':
               iFlag = ZH_COMPFLAG_RT_MACRO;
               break;
            case 'x':
            case 'X':
               iFlag = ZH_COMPFLAG_XBASE;
               break;
            case 'j':
            case 'J':
               iFlag = ZH_COMPFLAG_OPTJUMP;
               iValue = ! iValue;
               break;
            case 'm':
            case 'M':
               iFlag = ZH_COMPFLAG_MACROTEXT;
               iValue = ! iValue;
               break;
            case 'd':
            case 'D':
               iFlag = ZH_COMPFLAG_MACRODECL;
               break;
            case 's':
            case 'S':
               iFlag = ZH_COMPFLAG_ARRSTR;
               break;
            default:
               fError = ZH_TRUE;
         }
         if( ! fError && iFlag )
         {
            if( fSet )
            {
               if( iValue )
                  ZH_COMP_PARAM->supported |= iFlag;
               else
                  ZH_COMP_PARAM->supported &= ~iFlag;
            }
            else
            {
               if( iValue )
                  iValue = ( ZH_COMP_PARAM->supported & iFlag ) ? 0 : 1;
               else
                  iValue = ( ZH_COMP_PARAM->supported & iFlag ) ? 1 : 0;
            }
         }
      }
      else if( zh_strnicmp( szSwitch, "gc", 2 ) == 0 )
      {
         if( fSet )
         {
            if( iValue == ZH_COMPGENC_REALCODE ||
                iValue == ZH_COMPGENC_VERBOSE ||
                iValue == ZH_COMPGENC_NORMAL ||
                iValue == ZH_COMPGENC_COMPACT )
               ZH_COMP_PARAM->iGenCOutput = iValue;
         }
         else
            iValue = ZH_COMP_PARAM->iGenCOutput;
      }
      else if( zh_strnicmp( szSwitch, "es", 2 ) == 0 )
      {
         if( fSet )
         {
            if( iValue == ZH_EXITLEVEL_DEFAULT ||
                iValue == ZH_EXITLEVEL_SETEXIT ||
                iValue == ZH_EXITLEVEL_DELTARGET )
               ZH_COMP_PARAM->iExitLevel = iValue;
         }
         else
            iValue = ZH_COMP_PARAM->iExitLevel;
      }
      else if( zh_stricmp( szSwitch, "p+" ) == 0 )
      {
         if( fSet )
            ZH_COMP_PARAM->fPPT = iValue != 0;
         else
            iValue = ZH_COMP_PARAM->fPPT ? 1 : 0;
      }
      else
         fError = ZH_TRUE;
   }
   /* xZiher extension */
   else if( i >= 4 && zh_strnicmp( szSwitch, "TEXTHIDDEN", i ) == 0 )
   {
      if( fSet )
      {
         if( iValue >= 0 && iValue <= 1 )
            ZH_COMP_PARAM->iHidden = iValue;
      }
      else
         iValue = ZH_COMP_PARAM->iHidden;
   }
   else
      fError = ZH_TRUE;

   *piValue = iValue;

   return fError;
}

static void zh_pp_fileIncluded( void * cargo, const char * szFileName )
{
   ZH_COMP_DECL = ( PZH_COMP ) cargo;
   PZH_INCLST pIncFile, * pIncFilePtr;
   int iLen;

   pIncFilePtr = &ZH_COMP_PARAM->incfiles;
   while( *pIncFilePtr )
   {
#if defined( ZH_OS_UNIX )
      if( strcmp( ( *pIncFilePtr )->szFileName, szFileName ) == 0 )
         return;
#else
      if( zh_stricmp( ( *pIncFilePtr )->szFileName, szFileName ) == 0 )
         return;
#endif
      pIncFilePtr = &( *pIncFilePtr )->pNext;
   }

   iLen = ( int ) strlen( szFileName );
   pIncFile = ( PZH_INCLST ) zh_xgrab( sizeof( ZH_INCLST ) + iLen );
   pIncFile->pNext = NULL;
   memcpy( pIncFile->szFileName, szFileName, iLen + 1 );
   *pIncFilePtr = pIncFile;
}

void zh_compInitPP( ZH_COMP_DECL, PZH_PP_OPEN_FUNC pOpenFunc )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compInitPP()" ) );

   if( ZH_COMP_PARAM->pLex->pPP )
   {
      zh_pp_init( ZH_COMP_PARAM->pLex->pPP,
                  ZH_COMP_PARAM->fQuiet, ZH_COMP_PARAM->fGauge,
                  ZH_COMP_PARAM->iMaxTransCycles,
                  ZH_COMP_PARAM, pOpenFunc, NULL,
                  zh_pp_ErrorGen, zh_pp_Disp, zh_pp_PragmaDump,
                  ZH_COMP_ISSUPPORTED( ZH_COMPFLAG_ZH_INLINE ) ?
                  zh_pp_zh_inLine : NULL, zh_pp_CompilerSwitch );

      if( ZH_COMP_PARAM->iTraceInclude )
         zh_pp_setIncFunc( ZH_COMP_PARAM->pLex->pPP, zh_pp_fileIncluded );

      if( ! ZH_COMP_PARAM->szStdCh )
         zh_pp_setStdRules( ZH_COMP_PARAM->pLex->pPP );
      else if( ZH_COMP_PARAM->szStdCh[ 0 ] > ' ' )
         zh_pp_readRules( ZH_COMP_PARAM->pLex->pPP, ZH_COMP_PARAM->szStdCh );
      else if( ! ZH_COMP_PARAM->fQuiet )
         zh_compOutStd( ZH_COMP_PARAM, "Standard command definitions excluded.\n" );

      zh_pp_initDynDefines( ZH_COMP_PARAM->pLex->pPP, ! ZH_COMP_PARAM->fNoArchDefs );

      /* Add /D and /undef: command-line or envvar defines */
      zh_compChkSetDefines( ZH_COMP_PARAM );

      /* add extended definitions files (-u+<file>) */
      if( ZH_COMP_PARAM->iStdChExt > 0 )
      {
         int i = 0;

         while( i < ZH_COMP_PARAM->iStdChExt )
            zh_pp_readRules( ZH_COMP_PARAM->pLex->pPP,
                             ZH_COMP_PARAM->szStdChExt[ i++ ] );
      }

      /* mark current rules as standard ones */
      zh_pp_setStdBase( ZH_COMP_PARAM->pLex->pPP );
   }
}
