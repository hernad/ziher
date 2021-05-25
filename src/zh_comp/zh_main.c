/*
 * Compiler main file
 *
 * Copyright 1999 Antonio Linares <alinares@fivetechsoft.com>
 * Copyright 2000 Ron Pinkas <Ron@Profit-Master.com>
 *   (zh_compPrepareJumps(), zh_compOptimizeJumps(), zh_compOptimizeFrames(),
 *   zh_compDeclaredParameterAdd(), zh_compClassAdd(), zh_compClassFind(),
 *   zh_compMethodAdd(), zh_compMethodFind(), zh_compDeclaredAdd())
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

/* Avoid tracing in preprocessor/compiler. */
#if ! defined( ZH_TRACE_UTILS )
   #if defined( ZH_TRACE_LEVEL )
      #undef ZH_TRACE_LEVEL
   #endif
#endif

#include "zh_comp.h"
#include "zh_set.h"

static int zh_compCompile( ZH_COMP_DECL, const char * szPrg, const char * szBuffer, int iStartLine );
static ZH_BOOL zh_compRegisterFunc( ZH_COMP_DECL, PZH_ZFUNC pFunc, ZH_BOOL fError );

/* ************************************************************************* */

int zh_compMainExt( int argc, const char * const argv[],
                    ZH_BYTE ** pBufPtr, ZH_SIZE * pnSize,
                    const char * szSource, int iStartLine,
                    void * cargo, PZH_PP_OPEN_FUNC pOpenFunc,
                                  PZH_PP_MSG_FUNC pMsgFunc )
{
   ZH_COMP_DECL;
   int iStatus = EXIT_SUCCESS, iFileCount = 0;
   int iFileCase, iDirCase, iDirSep;
   ZH_BOOL fTrimFN;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compMain()" ) );

   iFileCase = zh_setGetFileCase();
   iDirCase = zh_setGetDirCase();
   iDirSep = zh_setGetDirSeparator();
   fTrimFN = zh_setGetTrimFileName();

   if( pBufPtr && pnSize )
   {
      *pBufPtr = NULL;
      *pnSize  = 0;
   }

   ZH_COMP_PARAM = zh_comp_new();
   ZH_COMP_PARAM->cargo = cargo;
   if( pMsgFunc )
      ZH_COMP_PARAM->outMsgFunc = pMsgFunc;

   ZH_COMP_PARAM->pOutPath = NULL;

   /* First check the environment variables */
   zh_compChkEnvironment( ZH_COMP_PARAM );

   /* Then check command-line arguments
      This will override duplicated environment settings */
   zh_compChkCommandLine( ZH_COMP_PARAM, argc, argv );

   if( ! ZH_COMP_PARAM->fExit )
   {
      if( ZH_COMP_PARAM->iTraceInclude == 0 &&
          ZH_COMP_PARAM->iSyntaxCheckOnly == 2 )
         ZH_COMP_PARAM->iTraceInclude = 1;

      if( pBufPtr && pnSize )
      {
         if( ZH_COMP_PARAM->iTraceInclude > 0 &&
             ZH_COMP_PARAM->iSyntaxCheckOnly > 0 )
            ZH_COMP_PARAM->iTraceInclude |= 0x100;
         else
            ZH_COMP_PARAM->iLanguage = ZH_LANG_PORT_OBJ_BUF;
      }

      if( ZH_COMP_PARAM->fLogo )
         zh_compPrintLogo( ZH_COMP_PARAM );

      if( ZH_COMP_PARAM->fBuildInfo )
      {
         zh_compOutStd( ZH_COMP_PARAM, "\n" );
         zh_verBuildInfoCB( zh_conOutStd );
      }

      if( ZH_COMP_PARAM->fCredits )
         zh_compPrintCredits( ZH_COMP_PARAM );

      /* Set Search Path */
      if( ZH_COMP_PARAM->fINCLUDE )
         zh_compChkAddIncPaths( ZH_COMP_PARAM );

      /* Set standard rules */
      zh_compInitPP( ZH_COMP_PARAM, pOpenFunc );

      /* Prepare the table of identifiers */
      zh_compIdentifierOpen( ZH_COMP_PARAM );
   }

   if( szSource )
   {
      iFileCount++;
      iStatus = zh_compCompile( ZH_COMP_PARAM, "{SOURCE}", szSource, iStartLine );
   }
   else
   {
      int i;
      /* Process all files passed via the command-line. */
      for( i = 1; i < argc && ! ZH_COMP_PARAM->fExit; i++ )
      {
         ZH_TRACE( ZH_TR_DEBUG, ( "main LOOP(%i,%s)", i, argv[ i ] ) );
         if( ! ZH_ISOPTSEP( argv[ i ][ 0 ] ) )
         {
            iFileCount++;
            iStatus = zh_compCompile( ZH_COMP_PARAM, argv[ i ], NULL, 0 );
            if( iStatus != EXIT_SUCCESS )
               break;
         }
      }
   }

   if( iFileCount == 0 && ! ZH_COMP_PARAM->fQuiet && ! ZH_COMP_PARAM->fExit &&
       ! ZH_COMP_PARAM->fBuildInfo && ! ZH_COMP_PARAM->fCredits )
   {
      zh_compPrintUsage( ZH_COMP_PARAM, argv[ 0 ] );
      iStatus = EXIT_FAILURE;
   }
   else if( ZH_COMP_PARAM->iErrorCount > 0 )
      iStatus = EXIT_FAILURE;

   if( iFileCount > 0 && iStatus == EXIT_SUCCESS )
   {
      zh_compI18nSave( ZH_COMP_PARAM, ZH_TRUE );

      if( pBufPtr && pnSize && iStatus == EXIT_SUCCESS )
      {
         *pBufPtr = ZH_COMP_PARAM->pOutBuf;
         *pnSize  = ZH_COMP_PARAM->nOutBufSize;
         ZH_COMP_PARAM->pOutBuf = NULL;
         ZH_COMP_PARAM->nOutBufSize = 0;
      }
   }

   zh_comp_free( ZH_COMP_PARAM );

   zh_setSetFileCase( iFileCase );
   zh_setSetDirCase( iDirCase );
   zh_setSetDirSeparator( iDirSep );
   zh_setSetTrimFileName( fTrimFN );

   return iStatus;
}

int zh_compMain( int argc, const char * const argv[] )
{
   return zh_compMainExt( argc, argv, NULL, NULL, NULL, 0, NULL, NULL, NULL );
}

static int zh_compReadClpFile( ZH_COMP_DECL, const char * szClpFile )
{
   char buffer[ ZH_PATH_MAX + 80 ];
   char szFile[ ZH_PATH_MAX ];
   int iStatus = EXIT_SUCCESS;
   PZH_FNAME pFileName;
   FILE *inFile;

   pFileName = zh_fsFNameSplit( szClpFile );
   if( ! pFileName->szExtension )
   {
      pFileName->szExtension = ".clp";
      zh_fsFNameMerge( szFile, pFileName );
      szClpFile = szFile;
   }

   ZH_COMP_PARAM->szFile = zh_compIdentifierNew( ZH_COMP_PARAM, szClpFile, ZH_IDENT_COPY );
   if( ! ZH_COMP_PARAM->pFileName )
      ZH_COMP_PARAM->pFileName = pFileName;
   else
      zh_xfree( pFileName );

   inFile = zh_fopen( szClpFile, "r" );
   if( ! inFile )
   {
      zh_snprintf( buffer, sizeof( buffer ),
                   "Cannot open input file: %s\n", szClpFile );
      zh_compOutErr( ZH_COMP_PARAM, buffer );
      iStatus = EXIT_FAILURE;
   }
   else
   {
      int i = 0, ch;

      zh_snprintf( buffer, sizeof( buffer ), "Reading '%s'...\n", szClpFile );
      zh_compOutStd( ZH_COMP_PARAM, buffer );

      do
      {
         ch = fgetc( inFile );

         if( ch == '"' )
         {
            while( ( ch = fgetc( inFile ) ) != EOF && ch != '"' && ch != '\n' )
            {
               if( i < ( ZH_PATH_MAX - 1 ) )
                  szFile[ i++ ] = ( char ) ch;
            }
            if( ch == '"' )
               continue;
         }

         while( i == 0 && ZH_ISSPACE( ch ) )
            ch = fgetc( inFile );

         if( ch == EOF || ZH_ISSPACE( ch ) || ch == '#' )
         {
            szFile[ i ] = '\0';
            if( i > 0 )
               zh_compModuleAdd( ZH_COMP_PARAM,
                                 zh_compIdentifierNew( ZH_COMP_PARAM, szFile, ZH_IDENT_COPY ),
                                 ZH_TRUE );
            i = 0;
            while( ch != EOF && ch != '\n' )
               ch = fgetc( inFile );
         }
         else if( i < ( ZH_PATH_MAX - 1 ) )
            szFile[ i++ ] = ( char ) ch;
      }
      while( ch != EOF );

      fclose( inFile );
   }

   return iStatus;
}


/* --- ACTIONS --- */


static PZH_HSYMBOL zh_compSymbolAdd( ZH_COMP_DECL, const char * szSymbolName, ZH_USHORT * pwPos, ZH_BOOL bFunction )
{
   PZH_HSYMBOL pSym;

   pSym = ( PZH_HSYMBOL ) zh_xgrab( sizeof( ZH_HSYMBOL ) );

   pSym->szName = szSymbolName;
   pSym->cScope = 0;
   pSym->iFunc = bFunction ? ZH_COMP_PARAM->iModulesCount : 0;
   pSym->pFunc = NULL;
   pSym->pNext = NULL;

   if( ! ZH_COMP_PARAM->symbols.iCount )
   {
      ZH_COMP_PARAM->symbols.pFirst =
      ZH_COMP_PARAM->symbols.pLast  = pSym;
   }
   else
   {
      ZH_COMP_PARAM->symbols.pLast->pNext = pSym;
      ZH_COMP_PARAM->symbols.pLast = pSym;
   }
   ZH_COMP_PARAM->symbols.iCount++;

   if( pwPos )
      *pwPos = ( ZH_USHORT ) ( ZH_COMP_PARAM->symbols.iCount - 1 );  /* position number starts form 0 */

   return pSym;
}

static PZH_HSYMBOL zh_compSymbolFind( ZH_COMP_DECL, const char * szSymbolName, ZH_USHORT * pwPos, ZH_BOOL bFunction )
{
   PZH_HSYMBOL pSym = ZH_COMP_PARAM->symbols.pFirst;
   ZH_USHORT wCnt = 0;
   int iFunc = bFunction ? ZH_COMP_PARAM->iModulesCount : 0;

   while( pSym )
   {
      if( ! strcmp( pSym->szName, szSymbolName ) )
      {
         if( iFunc == pSym->iFunc )
         {
            if( pwPos )
               *pwPos = wCnt;
            return pSym;
         }
      }
      pSym = pSym->pNext;
      ++wCnt;
   }

   if( pwPos )
      *pwPos = 0;

   return NULL;
}

/* returns a symbol name based on its index on the symbol table
 * index starts from 0
 */
const char * zh_compSymbolName( ZH_COMP_DECL, ZH_USHORT uiSymbol )
{
   PZH_HSYMBOL pSym = ZH_COMP_PARAM->symbols.pFirst;

   while( pSym )
   {
      if( uiSymbol-- == 0 )
         return pSym->szName;
      pSym = pSym->pNext;
   }
   return NULL;
}

static void zh_compCheckDuplVars( ZH_COMP_DECL, PZH_HVAR pVar, const char * szVarName )
{
   while( pVar )
   {
      if( ! strcmp( pVar->szName, szVarName ) )
      {
         ZH_COMP_ERROR_DUPLVAR( szVarName );
         break;
      }
      else
         pVar = pVar->pNext;
   }
}

static ZH_USHORT zh_compVarListAdd( PZH_HVAR * pVarLst, PZH_HVAR pVar )
{
   ZH_USHORT uiVar = 1;

   while( *pVarLst )
   {
      pVarLst = &( *pVarLst )->pNext;
      ++uiVar;
   }
   *pVarLst = pVar;

   return uiVar;
}

void zh_compVariableAdd( ZH_COMP_DECL, const char * szVarName, PZH_VARTYPE pVarType )
{
   PZH_ZFUNC pFunc = ZH_COMP_PARAM->functions.pLast;
   PZH_HVAR pVar;
   ZH_BOOL bFreeVar = ZH_TRUE;

   if( ( pFunc->funFlags & ZH_FUNF_FILE_DECL ) != 0 &&
       ( ZH_COMP_PARAM->iVarScope == ZH_VSCOMP_LOCAL ||
         ZH_COMP_PARAM->iVarScope == ( ZH_VSCOMP_PRIVATE | ZH_VSCOMP_PARAMETER ) ) )
   {
      if( ZH_COMP_PARAM->iStartProc == 2 && pFunc->szName[ 0 ] &&
          zh_compRegisterFunc( ZH_COMP_PARAM, pFunc, ZH_FALSE ) )
         pFunc->funFlags &= ~ZH_FUNF_FILE_DECL;
      else
      {
         /* Variable declaration is outside of function/procedure body.
            In this case only STATICs, MEMVARs and FIELDs declarations are allowed. */
         zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_OUTSIDE, NULL, NULL );
         return;
      }
   }

   /* check if we are declaring local/static variable after some
    * executable statements
    */
   if( pFunc->funFlags & ZH_FUNF_STATEMENTS )
   {
      const char * szVarScope;
      switch( ZH_COMP_PARAM->iVarScope )
      {
         case ZH_VSCOMP_LOCAL:
            szVarScope = "LOCAL";
            break;
         case ZH_VSCOMP_STATIC:
         case ZH_VSCOMP_TH_STATIC:
            szVarScope = "STATIC";
            break;
         case ZH_VSCOMP_FIELD:
            szVarScope = "FIELD";
            break;
         case ZH_VSCOMP_MEMVAR:
            szVarScope = "MEMVAR";
            break;
         default:
            szVarScope = NULL;
      }
      if( szVarScope )
      {
         zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_FOLLOWS_EXEC, szVarScope, NULL );
         return;
      }
   }

   /* Check if a declaration of duplicated variable name is requested */
   if( pFunc->szName )
   {
      /* variable defined in a function/procedure */
      zh_compCheckDuplVars( ZH_COMP_PARAM, pFunc->pFields, szVarName );
      zh_compCheckDuplVars( ZH_COMP_PARAM, pFunc->pStatics, szVarName );

      if( ! ( ZH_COMP_PARAM->iVarScope == ZH_VSCOMP_PRIVATE ||
              ZH_COMP_PARAM->iVarScope == ZH_VSCOMP_PUBLIC ) )
         zh_compCheckDuplVars( ZH_COMP_PARAM, pFunc->pMemvars, szVarName );
   }
   else if( pFunc->funFlags & ZH_FUNF_EXTBLOCK )
   {
      /* variable defined in an extended codeblock */
      zh_compCheckDuplVars( ZH_COMP_PARAM, pFunc->pFields, szVarName );
      zh_compCheckDuplVars( ZH_COMP_PARAM, pFunc->pStatics, szVarName );
   }
   else if( ZH_COMP_PARAM->iVarScope != ZH_VSCOMP_PARAMETER )
   {
      char buffer[ 80 ];
      zh_snprintf( buffer, sizeof( buffer ),
                   "Wrong type of codeblock parameter, is: %d, should be: %d\n",
                   ZH_COMP_PARAM->iVarScope, ZH_VSCOMP_PARAMETER );
      zh_compOutErr( ZH_COMP_PARAM, buffer );
      /* variable defined in a codeblock */
      ZH_COMP_PARAM->iVarScope = ZH_VSCOMP_PARAMETER;
   }

   zh_compCheckDuplVars( ZH_COMP_PARAM, pFunc->pLocals, szVarName );

   pVar = ( PZH_HVAR ) zh_xgrab( sizeof( ZH_HVAR ) );
   pVar->szName = szVarName;
   pVar->szAlias = NULL;
   pVar->uiFlags = 0;
   pVar->cType = pVarType->cVarType;
   pVar->iUsed = ZH_VU_NOT_USED;
   pVar->pNext = NULL;
   pVar->iDeclLine = ZH_COMP_PARAM->currLine;

   if( ZH_TOUPPER( pVarType->cVarType ) == 'S' )
   {
      #if 0
      printf( "\nVariable %s is of Class: %s\n", szVarName, pVarType->szFromClass );
      #endif
      pVar->pClass = zh_compClassFind( ZH_COMP_PARAM, pVarType->szFromClass );
      if( ! pVar->pClass )
      {
         zh_compGenWarning( ZH_COMP_PARAM, zh_comp_szWarnings, 'W', ZH_COMP_WARN_CLASS_NOT_FOUND, pVarType->szFromClass, szVarName );
         pVar->cType = 'O';
      }
   }

   if( ZH_COMP_PARAM->iVarScope & ZH_VSCOMP_PARAMETER )
      pVar->iUsed = ZH_VU_INITIALIZED;

   if( ZH_COMP_PARAM->iVarScope & ZH_VSCOMP_MEMVAR )
   {
      PZH_HSYMBOL pSym;
      ZH_USHORT wPos;

      if( ZH_COMP_PARAM->fAutoMemvarAssume || ZH_COMP_PARAM->iVarScope == ZH_VSCOMP_MEMVAR )
      {
         /* add this variable to the list of MEMVAR variables
          */
         if( pFunc->pMemvars )
            zh_compCheckDuplVars( ZH_COMP_PARAM, pFunc->pMemvars, szVarName );

         zh_compVarListAdd( &pFunc->pMemvars, pVar );
         bFreeVar = ZH_FALSE;
      }

      switch( ZH_COMP_PARAM->iVarScope )
      {
         case ZH_VSCOMP_MEMVAR:
            /* variable declared in MEMVAR statement */
            break;

         case ( ZH_VSCOMP_PARAMETER | ZH_VSCOMP_PRIVATE ):
            if( ++pFunc->wParamNum > pFunc->wParamCount )
               pFunc->wParamCount = pFunc->wParamNum;

            pSym = zh_compSymbolFind( ZH_COMP_PARAM, szVarName, &wPos, ZH_SYM_MEMVAR ); /* check if symbol exists already */
            if( ! pSym )
               pSym = zh_compSymbolAdd( ZH_COMP_PARAM, szVarName, &wPos, ZH_SYM_MEMVAR );
            pSym->cScope |= ZH_FS_MEMVAR;

            zh_compGenPCode4( ZH_P_PARAMETER, ZH_LOBYTE( wPos ), ZH_HIBYTE( wPos ), ZH_LOBYTE( pFunc->wParamNum ), ZH_COMP_PARAM );

            if( ZH_COMP_PARAM->iWarnings >= 3 && bFreeVar )
            {
               PZH_HVAR pMemVar = pFunc->pMemvars;
               while( pMemVar && strcmp( pMemVar->szName, pVar->szName ) != 0 )
                  pMemVar = pMemVar->pNext;
               /* Not declared as memvar. */
               if( pMemVar == NULL )
               {
                  /* add this variable to the list of PRIVATE variables. */
                  zh_compVarListAdd( &pFunc->pPrivates, pVar );
                  bFreeVar = ZH_FALSE;
               }
            }
            if( bFreeVar )
               zh_xfree( pVar );
            break;

         case ZH_VSCOMP_PRIVATE:
            pSym = zh_compSymbolFind( ZH_COMP_PARAM, szVarName, &wPos, ZH_SYM_MEMVAR ); /* check if symbol exists already */
            if( ! pSym )
               pSym = zh_compSymbolAdd( ZH_COMP_PARAM, szVarName, &wPos, ZH_SYM_MEMVAR );
            pSym->cScope |= ZH_FS_MEMVAR;

            if( ZH_COMP_PARAM->iWarnings >= 3 && bFreeVar )
            {
               PZH_HVAR pMemVar = pFunc->pMemvars;
               while( pMemVar && strcmp( pMemVar->szName, pVar->szName ) != 0 )
                  pMemVar = pMemVar->pNext;
               /* Not declared as memvar. */
               if( pMemVar == NULL )
               {
                  /* add this variable to the list of PRIVATE variables. */
                  zh_compVarListAdd( &pFunc->pPrivates, pVar );
                  bFreeVar = ZH_FALSE;
               }
            }
            if( bFreeVar )
               zh_xfree( pVar );
            break;

         case ZH_VSCOMP_PUBLIC:
            pSym = zh_compSymbolFind( ZH_COMP_PARAM, szVarName, &wPos, ZH_SYM_MEMVAR ); /* check if symbol exists already */
            if( ! pSym )
               pSym = zh_compSymbolAdd( ZH_COMP_PARAM, szVarName, &wPos, ZH_SYM_MEMVAR );
            pSym->cScope |= ZH_FS_MEMVAR;
            if( bFreeVar )
               zh_xfree( pVar );
            break;
      }
   }
   else
   {
      switch( ZH_COMP_PARAM->iVarScope )
      {
         case ZH_VSCOMP_LOCAL:
         case ZH_VSCOMP_PARAMETER:
         {
            ZH_USHORT wLocal = zh_compVarListAdd( &pFunc->pLocals, pVar );

            if( ZH_COMP_PARAM->iVarScope == ZH_VSCOMP_PARAMETER )
            {
               ++pFunc->wParamCount;
               pFunc->funFlags |= ZH_FUNF_USES_LOCAL_PARAMS;
            }
            if( ZH_COMP_PARAM->fDebugInfo )
            {
               zh_compGenPCode3( ZH_P_LOCALNAME, ZH_LOBYTE( wLocal ), ZH_HIBYTE( wLocal ), ZH_COMP_PARAM );
               zh_compGenPCodeN( ( const ZH_BYTE * ) szVarName, strlen( szVarName ) + 1, ZH_COMP_PARAM );
            }
            break;
         }

         case ZH_VSCOMP_TH_STATIC:
            pVar->uiFlags = ZH_VSCOMP_THREAD;
            /* fallthrough */
         case ZH_VSCOMP_STATIC:
            ++ZH_COMP_PARAM->iStaticCnt;
            zh_compVarListAdd( &pFunc->pStatics, pVar );
            break;

         case ZH_VSCOMP_FIELD:
            zh_compVarListAdd( &pFunc->pFields, pVar );
            break;
      }
   }
}

/* Set the name of an alias for the list of previously declared FIELDs
 *
 * szAlias -> name of the alias
 * iField  -> position of the first FIELD name to change
 */
void zh_compFieldSetAlias( ZH_COMP_DECL, const char * szAlias, int iField )
{
   PZH_HVAR pVar;

   pVar = ZH_COMP_PARAM->functions.pLast->pFields;
   while( iField-- && pVar )
      pVar = pVar->pNext;

   while( pVar )
   {
      pVar->szAlias = szAlias;
      pVar = pVar->pNext;
   }
}

/* This functions counts the number of FIELD declaration in a function
 * We will required this information in zh_compFieldSetAlias function
 */
int zh_compFieldsCount( ZH_COMP_DECL )
{
   int iFields = 0;
   PZH_HVAR pVar = ZH_COMP_PARAM->functions.pLast->pFields;

   while( pVar )
   {
      ++iFields;
      pVar = pVar->pNext;
   }

   return iFields;
}

static PZH_HVAR zh_compVariableGet( PZH_HVAR pVars, const char * szVarName, int * piPos )
{
   int iVar = 1;

   while( pVars )
   {
      if( pVars->szName && ! strcmp( pVars->szName, szVarName ) )
      {
         pVars->iUsed |= ZH_VU_USED;
         *piPos = iVar;
         return pVars;
      }
      pVars = pVars->pNext;
      ++iVar;
   }
   return NULL;
}

/* returns variable pointer if defined or NULL */
static PZH_HVAR zh_compVariableGetVar( PZH_HVAR pVars, ZH_USHORT wOrder )
{
   while( pVars && --wOrder )
      pVars = pVars->pNext;
   return pVars;
}

/* returns the order + 1 of a variable if defined or zero */
static ZH_USHORT zh_compVariableGetPos( PZH_HVAR pVars, const char * szVarName )
{
   ZH_USHORT wVar = 1;

   while( pVars )
   {
      if( pVars->szName && ! strcmp( pVars->szName, szVarName ) )
      {
         pVars->iUsed |= ZH_VU_USED;
         return wVar;
      }
      pVars = pVars->pNext;
      ++wVar;
   }
   return 0;
}

PZH_HVAR zh_compVariableFind( ZH_COMP_DECL, const char * szVarName, int * piPos, int * piScope )
{
   PZH_ZFUNC pFunc, pGlobal, pOutBlock = NULL;
   ZH_BOOL fStatic = ZH_FALSE, fBlock = ZH_FALSE, fGlobal = ZH_FALSE;
   PZH_HVAR pVar = NULL;
   int iPos = 0, iScope = 0, iLevel = 0;

   if( piPos )
      *piPos = 0;
   else
      piPos = &iPos;
   if( piScope )
      *piScope = ZH_VS_UNDECLARED;
   else
      piScope = &iScope;

   /* check current function/codeblock variables */
   pFunc = ZH_COMP_PARAM->functions.pLast;
   pGlobal = ( ZH_COMP_PARAM->pDeclFunc &&
               ZH_COMP_PARAM->pDeclFunc != pFunc &&
               ( ZH_COMP_PARAM->pDeclFunc->funFlags & ZH_FUNF_FILE_DECL ) )
             ? ZH_COMP_PARAM->pDeclFunc : NULL;

   while( pFunc )
   {
      if( ( pFunc->cScope & ZH_FS_INITEXIT ) == ZH_FS_INITEXIT )
      {
         /* static initialization function */
         fStatic = ZH_TRUE;
      }
      else if( pFunc->szName )
      {
         /* normal function/procedure */
         /* check local parameters */
         pVar = zh_compVariableGet( pFunc->pLocals, szVarName, piPos );
         if( pVar )
         {
            *piScope = ZH_VS_LOCAL_VAR;
            if( fStatic )
            {
               zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_ILLEGAL_INIT, "(b)", szVarName );
            }
            else if( fBlock && ZH_COMP_PARAM->functions.pLast->iEarlyEvalPass < 2 )
            {
               /* We want to access a local variable defined in a function
                * that owns this codeblock. We cannot access this variable in
                * a normal way because at runtime the stack base will point
                * to local variables of EVAL function.
                */
               /* NOTE: The list of local variables defined in a function
                * and referenced in a codeblock will be stored in a outer
                * codeblock only. This makes sure that all variables will be
                * detached properly - the inner codeblock can be created
                * outside of a function where it was defined when the local
                * variables are not accessible.
                */
               *piPos = - zh_compVariableGetPos( pOutBlock->pDetached, szVarName );
               if( *piPos == 0 )
               {
                  /* szVarName may point to dynamic buffer,
                   * make sure it's static one.
                   */
                  szVarName = pVar->szName;

                  /* this variable was not referenced yet - add it to the list */
                  pVar = ( PZH_HVAR ) zh_xgrab( sizeof( ZH_HVAR ) );

                  pVar->szName = szVarName;
                  pVar->szAlias = NULL;
                  pVar->cType = ' ';
                  pVar->iUsed = ZH_VU_NOT_USED;
                  pVar->pNext  = NULL;
                  pVar->iDeclLine = ZH_COMP_PARAM->currLine;
                  /* Use negative order to signal that we are accessing a local
                   * variable from a codeblock
                   */
                  *piPos = - zh_compVarListAdd( &pOutBlock->pDetached, pVar );
               }
               *piScope = ZH_VS_CBLOCAL_VAR;
            }
         }
         else
         {
            /* check static variables */
            pVar = zh_compVariableGet( pFunc->pStatics, szVarName, piPos );
            if( pVar )
            {
               *piScope = ZH_VS_STATIC_VAR;
               *piPos += pFunc->iStaticsBase;
            }
            else
            {
               /* check FIELDs */
               pVar = zh_compVariableGet( pFunc->pFields, szVarName, piPos );
               if( pVar )
                  *piScope = ZH_VS_LOCAL_FIELD;
               else
               {
                  /* check MEMVARs */
                  pVar = zh_compVariableGet( pFunc->pMemvars, szVarName, piPos );
                  if( pVar )
                     *piScope = ZH_VS_LOCAL_MEMVAR;
               }
            }
         }
      }
      else
      {
         /* codeblock */
         fBlock = ZH_TRUE;
         /* check local parameters */
         pVar = zh_compVariableGet( pFunc->pLocals, szVarName, piPos );
         if( pVar )
         {
            *piScope = ZH_VS_LOCAL_VAR;
            if( iLevel )
            {
               /* this variable is defined in a parent codeblock
                * It is not possible to access a parameter of a codeblock
                * in which the current codeblock is defined
                */
               zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_OUTER_VAR, szVarName, NULL );
            }
         }
         else if( pFunc->funFlags & ZH_FUNF_EXTBLOCK )
         {
            /* extended codeblock */
            /* check static variables */
            pVar = zh_compVariableGet( pFunc->pStatics, szVarName, piPos );
            if( pVar )
            {
               *piScope = ZH_VS_STATIC_VAR;
               *piPos += pFunc->iStaticsBase;
            }
            else
            {
               /* check FIELDs */
               pVar = zh_compVariableGet( pFunc->pFields, szVarName, piPos );
               if( pVar )
                  *piScope = ZH_VS_LOCAL_FIELD;
               else
               {
                  /* check MEMVARs */
                  pVar = zh_compVariableGet( pFunc->pMemvars, szVarName, piPos );
                  if( pVar )
                     *piScope = ZH_VS_LOCAL_MEMVAR;
               }
            }
         }
      }

      if( pVar )
         break;

      pOutBlock = pFunc;
      pFunc = pFunc->pOwner;
      if( ! pFunc && ! fGlobal )
      {
         /* instead of making this trick with pGlobal switching it will be
          * much cleaner to set pOwner in each compiled function to first
          * global pseudo function created when -n compiler switch is used
          * [druzus]
          */
         pFunc = pGlobal;
         fGlobal = ZH_TRUE;
      }
      ++iLevel;
   }

   if( pVar && fGlobal )
      *piScope |= ZH_VS_FILEWIDE;

   return pVar;
}

/* return local variable name using its order after final fixing */
const char * zh_compLocalVariableName( PZH_ZFUNC pFunc, ZH_USHORT wVar )
{
   PZH_HVAR pVar;

   if( pFunc->wParamCount && ! ( pFunc->funFlags & ZH_FUNF_USES_LOCAL_PARAMS ) )
      wVar -= pFunc->wParamCount;
   pVar = zh_compVariableGetVar( pFunc->pLocals, wVar );

   return pVar ? pVar->szName : NULL;
}

const char * zh_compStaticVariableName( ZH_COMP_DECL, ZH_USHORT wVar )
{
   PZH_HVAR pVar;
   PZH_ZFUNC pTmp = ZH_COMP_PARAM->functions.pFirst;

   while( pTmp->pNext && pTmp->pNext->iStaticsBase < wVar )
      pTmp = pTmp->pNext;
   pVar = zh_compVariableGetVar( pTmp->pStatics, ( ZH_USHORT ) ( wVar - pTmp->iStaticsBase ) );

   return pVar ? pVar->szName : NULL;
}

static int zh_compVariableScope( ZH_COMP_DECL, const char * szVarName )
{
   int iScope;

   zh_compVariableFind( ZH_COMP_PARAM, szVarName, NULL, &iScope );

   return iScope;
}

void zh_compPushMacroVar( ZH_COMP_DECL, const char * szVarName )
{
   /* save and restore iEarlyEvalPass to not disable early
      evaluation when only macrovar and/or macrotext is used */
   int iEarlyEvalPass = ZH_COMP_PARAM->functions.pLast->iEarlyEvalPass;

   zh_compGenPushVar( szVarName, ZH_COMP_PARAM );

   ZH_COMP_PARAM->functions.pLast->iEarlyEvalPass = iEarlyEvalPass;
}

void zh_compPushMacroText( ZH_COMP_DECL, const char * szText, ZH_SIZE nLen, ZH_BOOL fMacro )
{
   int iEarlyEvalPass = ZH_COMP_PARAM->functions.pLast->iEarlyEvalPass;
   ZH_BOOL fFound = ZH_FALSE;
   ZH_SIZE n = 0;
   int iParts = 0;

   while( n < nLen )
   {
      if( szText[ n++ ] == '&' )
      {
         char szSymName[ ZH_SYMBOL_NAME_LEN + 1 ];
         int iSize = 0;

         while( n < nLen && iSize < ZH_SYMBOL_NAME_LEN )
         {
            char ch = szText[ n ];
            if( ch >= 'a' && ch <= 'z' )
               szSymName[ iSize++ ] = ch - ( 'a' - 'A' );
            else if( ch == '_' || ( ch >= 'A' && ch <= 'Z' ) ||
                     ( iSize > 0 && ch >= '0' && ch <= '9' ) )
               szSymName[ iSize++ ] = ch;
            else
               break;
            ++n;
         }

         if( iSize )
         {
            int iScope;

            szSymName[ iSize ] = '\0';

            iScope = zh_compVariableScope( ZH_COMP_PARAM, szSymName );
            if( iScope == ZH_VS_UNDECLARED || ( iScope & ZH_VS_LOCAL_MEMVAR ) )
            {
               fFound = ZH_TRUE;
               if( fMacro && iScope == ZH_VS_UNDECLARED /* && ZH_SUPPORT_MACRODECL */ )
                  zh_compGenWarning( ZH_COMP_PARAM, zh_comp_szWarnings, 'W', ZH_COMP_WARN_AMBIGUOUS_VAR, szSymName, NULL );
            }
            else if( ZH_SUPPORT_MACRODECL )
            {
               ZH_SIZE nPrefix = n - iSize - 1;
               if( nPrefix > 0 )
               {
                  char * pszPrefix = ( char * ) zh_xgrab( nPrefix + 1 );
                  memcpy( pszPrefix, szText, nPrefix );
                  pszPrefix[ nPrefix ] = '\0';
                  zh_compGenPushString( pszPrefix, nPrefix + 1, ZH_COMP_PARAM );
                  zh_xfree( pszPrefix );
                  if( iParts++ > 0 )
                     zh_compGenPCode1( ZH_P_PLUS, ZH_COMP_PARAM );
               }
               zh_compGenPushVar( szSymName, ZH_COMP_PARAM );
               if( iParts++ > 0 )
                  zh_compGenPCode1( ZH_P_PLUS, ZH_COMP_PARAM );
               if( n < nLen && szText[ n ] == '.' )
                  ++n;
               szText += n;
               nLen -= n;
               n = 0;
            }
            else
            {
               zh_compErrorMacro( ZH_COMP_PARAM, szText );
               break;
            }
         }
         
      }
   }

   if( nLen > 0 || iParts == 0 )
   {
      zh_compGenPushString( szText, nLen + 1, ZH_COMP_PARAM );
      if( iParts > 0 )
         zh_compGenPCode1( ZH_P_PLUS, ZH_COMP_PARAM );
   }

   if( fFound )
      zh_compGenPCode1( ZH_P_MACROTEXT, ZH_COMP_PARAM );

   ZH_COMP_PARAM->functions.pLast->iEarlyEvalPass = iEarlyEvalPass;
}



/*
 * DECLARATIONS
 */

static void zh_compDeclaredReset( ZH_COMP_DECL )
{
   while( ZH_COMP_PARAM->pFirstDeclared )
   {
      PZH_HDECLARED pDeclared = ZH_COMP_PARAM->pFirstDeclared;
      ZH_COMP_PARAM->pFirstDeclared = pDeclared->pNext;
      if( pDeclared->cParamTypes )
         zh_xfree( pDeclared->cParamTypes );
      if( pDeclared->pParamClasses )
         zh_xfree( pDeclared->pParamClasses );
      zh_xfree( pDeclared );
   }
   ZH_COMP_PARAM->pLastDeclared = NULL;

   while( ZH_COMP_PARAM->pFirstClass )
   {
      PZH_HCLASS pClass = ZH_COMP_PARAM->pFirstClass;
      ZH_COMP_PARAM->pFirstClass = pClass->pNext;
      while( pClass->pMethod )
      {
         PZH_HDECLARED pDeclared = pClass->pMethod;
         pClass->pMethod = pDeclared->pNext;
         if( pDeclared->cParamTypes )
            zh_xfree( pDeclared->cParamTypes );
         if( pDeclared->pParamClasses )
            zh_xfree( pDeclared->pParamClasses );
         zh_xfree( pDeclared );
      }
      zh_xfree( pClass );
   }
   ZH_COMP_PARAM->pLastClass = NULL;
   ZH_COMP_PARAM->pLastMethod = NULL;
}

PZH_HCLASS zh_compClassFind( ZH_COMP_DECL, const char * szClassName )
{
   PZH_HCLASS pClass = ZH_COMP_PARAM->pFirstClass;

   if( ZH_COMP_PARAM->iWarnings < 3 )
      return NULL;

   while( pClass )
   {
      if( ! strcmp( pClass->szName, szClassName ) )
         return pClass;
      pClass = pClass->pNext;
   }
   return NULL;
}

PZH_HCLASS zh_compClassAdd( ZH_COMP_DECL, const char * szClassName, const char * szClassFunc )
{
   PZH_HCLASS pClass;
   PZH_HDECLARED pDeclared;

   #if 0
   printf( "Declaring Class: %s\n", szClassName );
   #endif

   if( ZH_COMP_PARAM->iWarnings < 3 )
      return NULL;

   if( ( pClass = zh_compClassFind( ZH_COMP_PARAM, szClassName ) ) != NULL )
   {
      zh_compGenWarning( ZH_COMP_PARAM, zh_comp_szWarnings, 'W', ZH_COMP_WARN_DUP_DECLARATION, "class", szClassName );
      return pClass;
   }

   pClass = ( PZH_HCLASS ) zh_xgrab( sizeof( ZH_HCLASS ) );

   pClass->szName = szClassName;
   pClass->pMethod = NULL;
   pClass->pNext = NULL;

   if( ZH_COMP_PARAM->pFirstClass == NULL )
      ZH_COMP_PARAM->pFirstClass = pClass;
   else
      ZH_COMP_PARAM->pLastClass->pNext = pClass;

   ZH_COMP_PARAM->pLastClass = pClass;

   /* Auto declaration for the Class Function. */
   pDeclared = zh_compDeclaredAdd( ZH_COMP_PARAM, szClassFunc ? szClassFunc : szClassName );
   pDeclared->cType = 'S';
   pDeclared->pClass = pClass;

   return pClass;
}

PZH_HDECLARED zh_compMethodFind( PZH_HCLASS pClass, const char * szMethodName )
{
   if( pClass )
   {
      PZH_HDECLARED pMethod = pClass->pMethod;

      while( pMethod )
      {
         if( ! strcmp( pMethod->szName, szMethodName ) )
            return pMethod;
         pMethod = pMethod->pNext;
      }
   }

   return NULL;
}

PZH_HDECLARED zh_compMethodAdd( ZH_COMP_DECL, PZH_HCLASS pClass, const char * szMethodName )
{
   PZH_HDECLARED pMethod;

   #if 0
   printf( "\nDeclaring Method: %s of Class: %s Pointer: %li\n", szMethodName, pClass->szName, pClass );
   #endif

   if( ZH_COMP_PARAM->iWarnings < 3 )
      return NULL;

   if( ( pMethod = zh_compMethodFind( pClass, szMethodName ) ) != NULL )
   {
      zh_compGenWarning( ZH_COMP_PARAM, zh_comp_szWarnings, 'W', ZH_COMP_WARN_DUP_DECLARATION, "method", szMethodName );

      /* Last Declaration override previous declarations */
      pMethod->iParamCount = 0;
      if( pMethod->cParamTypes )
         zh_xfree( pMethod->cParamTypes );
      pMethod->cParamTypes = NULL;
      if( pMethod->pParamClasses )
         zh_xfree( pMethod->pParamClasses );
      pMethod->pParamClasses = NULL;

      return pMethod;
   }

   pMethod = ( PZH_HDECLARED ) zh_xgrab( sizeof( ZH_HDECLARED ) );

   pMethod->szName = szMethodName;
   pMethod->cType = ' '; /* Not known yet */
   pMethod->cParamTypes = NULL;
   pMethod->iParamCount = 0;
   pMethod->pParamClasses = NULL;
   pMethod->pNext = NULL;

   if( pClass->pMethod == NULL )
      pClass->pMethod = pMethod;
   else
      pClass->pLastMethod->pNext = pMethod;

   pClass->pLastMethod = pMethod;

   ZH_COMP_PARAM->pLastMethod = pMethod;

   return pMethod;
}

/* returns a symbol pointer from the symbol table
 * and sets its position in the symbol table.
 * NOTE: symbol's position number starts from 0
 */
static PZH_HDECLARED zh_compDeclaredFind( ZH_COMP_DECL, const char * szDeclaredName )
{
   PZH_HDECLARED pSym = ZH_COMP_PARAM->pFirstDeclared;

   while( pSym )
   {
      if( ! strcmp( pSym->szName, szDeclaredName ) )
         return pSym;
      pSym = pSym->pNext;
   }
   return NULL;
}

PZH_HDECLARED zh_compDeclaredAdd( ZH_COMP_DECL, const char * szDeclaredName )
{
   PZH_HDECLARED pDeclared;

   if( ZH_COMP_PARAM->iWarnings < 3 )
      return NULL;

   #if 0
   printf( "\nDeclaring Function: %s\n", szDeclaredName, NULL );
   #endif

   if( ( pDeclared = zh_compDeclaredFind( ZH_COMP_PARAM, szDeclaredName ) ) != NULL )
   {
      zh_compGenWarning( ZH_COMP_PARAM, zh_comp_szWarnings, 'W', ZH_COMP_WARN_DUP_DECLARATION, "function", szDeclaredName );

      /* Last declaration will take effect. */
      pDeclared->cType = ' '; /* Not known yet */
      pDeclared->iParamCount = 0;
      if( pDeclared->cParamTypes )
         zh_xfree( pDeclared->cParamTypes );
      pDeclared->cParamTypes = NULL;
      if( pDeclared->pParamClasses )
         zh_xfree( pDeclared->pParamClasses );
      pDeclared->pParamClasses = NULL;

      return pDeclared;
   }

   pDeclared = ( PZH_HDECLARED ) zh_xgrab( sizeof( ZH_HDECLARED ) );

   pDeclared->szName = szDeclaredName;
   pDeclared->cType = ' '; /* Not known yet */
   pDeclared->cParamTypes = NULL;
   pDeclared->iParamCount = 0;
   pDeclared->pParamClasses = NULL;
   pDeclared->pNext = NULL;

   if( ZH_COMP_PARAM->pFirstDeclared == NULL )
      ZH_COMP_PARAM->pFirstDeclared = pDeclared;
   else
      ZH_COMP_PARAM->pLastDeclared->pNext = pDeclared;

   ZH_COMP_PARAM->pLastDeclared = pDeclared;

   return pDeclared;
}

void zh_compDeclaredParameterAdd( ZH_COMP_DECL, const char * szVarName, PZH_VARTYPE pVarType )
{
   /* Nothing to do since no warnings requested.*/
   if( ZH_COMP_PARAM->iWarnings < 3 )
   {
      ZH_SYMBOL_UNUSED( szVarName );
      return;
   }

   /* Either a Declared Function Parameter or a Declared Method Parameter. */
   if( ZH_COMP_PARAM->szDeclaredFun )
   {
      /* Find the Declared Function owner of this parameter. */
      PZH_HDECLARED pDeclared = zh_compDeclaredFind( ZH_COMP_PARAM, ZH_COMP_PARAM->szDeclaredFun );

      if( pDeclared )
      {
         pDeclared->iParamCount++;

         if( pDeclared->cParamTypes )
         {
            pDeclared->cParamTypes = ( ZH_BYTE * ) zh_xrealloc( pDeclared->cParamTypes, pDeclared->iParamCount );
            pDeclared->pParamClasses = ( PZH_HCLASS * ) zh_xrealloc( pDeclared->pParamClasses, pDeclared->iParamCount * sizeof( PZH_HCLASS ) );
         }
         else
         {
            pDeclared->cParamTypes = ( ZH_BYTE * ) zh_xgrab( 1 );
            pDeclared->pParamClasses = ( PZH_HCLASS * ) zh_xgrab( sizeof( PZH_HCLASS ) );
         }

         pDeclared->cParamTypes[ pDeclared->iParamCount - 1 ] = pVarType->cVarType;

         if( ZH_TOUPPER( pVarType->cVarType ) == 'S' )
         {
            pDeclared->pParamClasses[ pDeclared->iParamCount - 1 ] = zh_compClassFind( ZH_COMP_PARAM, pVarType->szFromClass );
         }
      }
   }
   else /* Declared Method Parameter */
   {
      #if 0
      printf( "\nAdding parameter: %s Type: %c In Method: %s Class: %s FROM CLASS: %s\n", szVarName, pVarType->cVarType, ZH_COMP_PARAM->pLastMethod->szName, ZH_COMP_PARAM->pLastClass->szName, pVarType->szFromClass );
      #endif

      ZH_COMP_PARAM->pLastMethod->iParamCount++;

      if( ZH_COMP_PARAM->pLastMethod->cParamTypes )
      {
         ZH_COMP_PARAM->pLastMethod->cParamTypes = ( ZH_BYTE * ) zh_xrealloc( ZH_COMP_PARAM->pLastMethod->cParamTypes, ZH_COMP_PARAM->pLastMethod->iParamCount );
         ZH_COMP_PARAM->pLastMethod->pParamClasses = ( PZH_HCLASS * ) zh_xrealloc( ZH_COMP_PARAM->pLastMethod->pParamClasses, ZH_COMP_PARAM->pLastMethod->iParamCount * sizeof( ZH_HCLASS ) );
      }
      else
      {
         ZH_COMP_PARAM->pLastMethod->cParamTypes = ( ZH_BYTE * ) zh_xgrab( 1 );
         ZH_COMP_PARAM->pLastMethod->pParamClasses = ( PZH_HCLASS * ) zh_xgrab( sizeof( ZH_HCLASS ) );
      }

      ZH_COMP_PARAM->pLastMethod->cParamTypes[ ZH_COMP_PARAM->pLastMethod->iParamCount - 1 ] = pVarType->cVarType;

      if( ZH_TOUPPER( pVarType->cVarType ) == 'S' )
      {
         ZH_COMP_PARAM->pLastMethod->pParamClasses[ ZH_COMP_PARAM->pLastMethod->iParamCount - 1 ] = zh_compClassFind( ZH_COMP_PARAM, pVarType->szFromClass );

         #if 0
         printf( "\nParameter: %s FROM CLASS: %s\n", szVarName, ZH_COMP_PARAM->pLastMethod->pParamClasses[ ZH_COMP_PARAM->pLastMethod->iParamCount - 1 ]->szName );
         #endif
      }
   }
}

PZH_VARTYPE zh_compVarTypeNew( ZH_COMP_DECL, ZH_BYTE cVarType, const char* szFromClass )
{
   PZH_VARTYPE   pVT = ZH_COMP_PARAM->pVarType;
   PZH_VARTYPE*  ppVT = &( ZH_COMP_PARAM->pVarType );

   while( pVT )
   {
      if( pVT->cVarType == cVarType &&
          ( ( ! pVT->szFromClass && ! szFromClass ) ||
            ( pVT->szFromClass && szFromClass && ! strcmp( pVT->szFromClass, szFromClass ) ) ) )
         return pVT;

      ppVT = &pVT->pNext;
      pVT = pVT->pNext;
   }

   /* Add to the end of list. I hope it will help the most usual type (' ', NULL)
      to be in the beginning of the list, and it will be found faster. [Mindaugas] */
   pVT = ( PZH_VARTYPE ) zh_xgrab( sizeof( ZH_VARTYPE ) );
   pVT->pNext = NULL;
   pVT->cVarType = cVarType;
   pVT->szFromClass = szFromClass;
   *ppVT = pVT;
   return pVT;
}


/*
 * Functions
 */

static int zh_compSort_ZH_SIZE( const void * pLeft, const void * pRight )
{
   ZH_SIZE nLeft  = *( ( const ZH_SIZE * ) ( pLeft ) );
   ZH_SIZE nRight = *( ( const ZH_SIZE * ) ( pRight ) );

   if( nLeft == nRight )
      return 0;
   else if( nLeft < nRight )
      return -1;
   else
      return 1;
}

/* Jump Optimizer and dummy code eliminator */
static void zh_compOptimizeJumps( ZH_COMP_DECL )
{
   ZH_BYTE * pCode = ZH_COMP_PARAM->functions.pLast->pCode;
   ZH_SIZE * pNOOPs, * pJumps;
   ZH_SIZE nOptimized, nNextByte, nBytes2Copy, nJumpAddr, nNOOP, nJump;
   ZH_BOOL fLineStrip = ZH_COMP_PARAM->fLineNumbers;
   int iPass;

   if( ! ZH_COMP_ISSUPPORTED( ZH_COMPFLAG_OPTJUMP ) )
      return;

   zh_compOptimizePCode( ZH_COMP_PARAM, ZH_COMP_PARAM->functions.pLast );
   zh_compCodeTraceMarkDead( ZH_COMP_PARAM, ZH_COMP_PARAM->functions.pLast );

   for( iPass = 0; iPass < 4 && ! ZH_COMP_PARAM->fExit; ++iPass )
   {
      ZH_ISIZ nOffset;

      if( iPass == 3 && fLineStrip )
      {
         zh_compStripFuncLines( ZH_COMP_PARAM, ZH_COMP_PARAM->functions.pLast );
         fLineStrip = ZH_FALSE;
      }

      if( ZH_COMP_PARAM->functions.pLast->nJumps > 0 )
      {
         pJumps = ZH_COMP_PARAM->functions.pLast->pJumps;
         nJump = ZH_COMP_PARAM->functions.pLast->nJumps - 1;

         do
         {
            nJumpAddr = pJumps[ nJump ];

            /*
             * optimize existing jumps, it will be good to also join
             * unconditional jump chain calculating total jump offset but
             * it will be necessary to add some code to protect against
             * infinite loop which will appear when we add optimization
             * for the PCODE sequences like:
             *
             *    ZH_P_{FALSE|TRUE},
             * [ no jump targets or stack modification here ]
             *    ZH_P_JUMP{FALSE|TRUE}*,
             *
             * I'll think about something like that later, [druzus]
             */
            switch( pCode[ nJumpAddr ] )
            {
               case ZH_P_JUMPNEAR:
                  if( ( signed char ) pCode[ nJumpAddr + 1 ] == 2 )
                     zh_compNOOPfill( ZH_COMP_PARAM->functions.pLast, nJumpAddr, 2, ZH_FALSE, ZH_FALSE );
                  break;

               case ZH_P_JUMPFALSENEAR:
               case ZH_P_JUMPTRUENEAR:
                  if( ( signed char ) pCode[ nJumpAddr + 1 ] == 2 )
                     zh_compNOOPfill( ZH_COMP_PARAM->functions.pLast, nJumpAddr, 2, ZH_TRUE, ZH_FALSE );
                  break;

               case ZH_P_JUMP:
                  nOffset = ZH_PCODE_MKSHORT( &pCode[ nJumpAddr + 1 ] );
                  if( nOffset == 3 )
                     zh_compNOOPfill( ZH_COMP_PARAM->functions.pLast, nJumpAddr, 3, ZH_FALSE, ZH_FALSE );
                  else if( ZH_LIM_INT8( nOffset ) )
                  {
                     pCode[ nJumpAddr ] = ZH_P_JUMPNEAR;
                     zh_compNOOPfill( ZH_COMP_PARAM->functions.pLast, nJumpAddr + 2, 1, ZH_FALSE, ZH_FALSE );
                  }
                  break;

               case ZH_P_JUMPFALSE:
                  nOffset = ZH_PCODE_MKSHORT( &pCode[ nJumpAddr + 1 ] );
                  if( nOffset == 3 )
                     zh_compNOOPfill( ZH_COMP_PARAM->functions.pLast, nJumpAddr, 3, ZH_TRUE, ZH_FALSE );
                  else if( ZH_LIM_INT8( nOffset ) )
                  {
                     pCode[ nJumpAddr ] = ZH_P_JUMPFALSENEAR;
                     zh_compNOOPfill( ZH_COMP_PARAM->functions.pLast, nJumpAddr + 2, 1, ZH_FALSE, ZH_FALSE );
                  }
                  break;

               case ZH_P_JUMPTRUE:
                  nOffset = ZH_PCODE_MKSHORT( &pCode[ nJumpAddr + 1 ] );
                  if( nOffset == 3 )
                     zh_compNOOPfill( ZH_COMP_PARAM->functions.pLast, nJumpAddr, 3, ZH_TRUE, ZH_FALSE );
                  else if( ZH_LIM_INT8( nOffset ) )
                  {
                     pCode[ nJumpAddr ] = ZH_P_JUMPTRUENEAR;
                     zh_compNOOPfill( ZH_COMP_PARAM->functions.pLast, nJumpAddr + 2, 1, ZH_FALSE, ZH_FALSE );
                  }
                  break;

               case ZH_P_JUMPFAR:
                  nOffset = ZH_PCODE_MKINT24( &pCode[ nJumpAddr + 1 ] );
                  if( nOffset == 4 )
                     zh_compNOOPfill( ZH_COMP_PARAM->functions.pLast, nJumpAddr, 4, ZH_FALSE, ZH_FALSE );
                  else if( iPass > 0 && ZH_LIM_INT16( nOffset ) )
                  {
                     if( ZH_LIM_INT8( nOffset ) )
                     {
                        pCode[ nJumpAddr ] = ZH_P_JUMPNEAR;
                        zh_compNOOPfill( ZH_COMP_PARAM->functions.pLast, nJumpAddr + 2, 2, ZH_FALSE, ZH_FALSE );
                     }
                     else
                     {
                        pCode[ nJumpAddr ] = ZH_P_JUMP;
                        zh_compNOOPfill( ZH_COMP_PARAM->functions.pLast, nJumpAddr + 3, 1, ZH_FALSE, ZH_FALSE );
                     }
                  }
                  break;

               case ZH_P_JUMPFALSEFAR:
                  nOffset = ZH_PCODE_MKINT24( &pCode[ nJumpAddr + 1 ] );
                  if( nOffset == 4 )
                     zh_compNOOPfill( ZH_COMP_PARAM->functions.pLast, nJumpAddr, 4, ZH_TRUE, ZH_FALSE );
                  else if( iPass > 0 && ZH_LIM_INT16( nOffset ) )
                  {
                     if( ZH_LIM_INT8( nOffset ) )
                     {
                        pCode[ nJumpAddr ] = ZH_P_JUMPFALSENEAR;
                        zh_compNOOPfill( ZH_COMP_PARAM->functions.pLast, nJumpAddr + 2, 2, ZH_FALSE, ZH_FALSE );
                     }
                     else
                     {
                        pCode[ nJumpAddr ] = ZH_P_JUMPFALSE;
                        zh_compNOOPfill( ZH_COMP_PARAM->functions.pLast, nJumpAddr + 3, 1, ZH_FALSE, ZH_FALSE );
                     }
                  }
                  break;

               case ZH_P_JUMPTRUEFAR:
                  nOffset = ZH_PCODE_MKINT24( &pCode[ nJumpAddr + 1 ] );
                  if( nOffset == 4 )
                     zh_compNOOPfill( ZH_COMP_PARAM->functions.pLast, nJumpAddr, 4, ZH_TRUE, ZH_FALSE );
                  else if( iPass > 0 && ZH_LIM_INT16( nOffset ) )
                  {
                     if( ZH_LIM_INT8( nOffset ) )
                     {
                        pCode[ nJumpAddr ] = ZH_P_JUMPTRUENEAR;
                        zh_compNOOPfill( ZH_COMP_PARAM->functions.pLast, nJumpAddr + 2, 2, ZH_FALSE, ZH_FALSE );
                     }
                     else
                     {
                        pCode[ nJumpAddr ] = ZH_P_JUMPTRUE;
                        zh_compNOOPfill( ZH_COMP_PARAM->functions.pLast, nJumpAddr + 3, 1, ZH_FALSE, ZH_FALSE );
                     }
                  }
                  break;
            }

            /* remove dummy jumps (over dead code) */
            if( pCode[ nJumpAddr ] == ZH_P_NOOP ||
                pCode[ nJumpAddr ] == ZH_P_POP )
            {
               if( ZH_COMP_PARAM->functions.pLast->nJumps > nJump + 1 )
                  memmove( &pJumps[ nJump ], &pJumps[ nJump + 1 ],
                           ( ZH_COMP_PARAM->functions.pLast->nJumps - nJump - 1 ) *
                           sizeof( ZH_SIZE ) );
               ZH_COMP_PARAM->functions.pLast->nJumps--;
            }
         }
         while( nJump-- );

         if( ZH_COMP_PARAM->functions.pLast->nJumps == 0 )
         {
            zh_xfree( ZH_COMP_PARAM->functions.pLast->pJumps );
            ZH_COMP_PARAM->functions.pLast->pJumps = NULL;
         }
      }

      if( ZH_COMP_PARAM->functions.pLast->nNOOPs == 0 )
      {
         if( iPass == 0 )
            continue;
         if( fLineStrip )
            zh_compStripFuncLines( ZH_COMP_PARAM, ZH_COMP_PARAM->functions.pLast );
         if( ZH_COMP_PARAM->functions.pLast->nNOOPs == 0 )
            return;
      }

      pNOOPs = ZH_COMP_PARAM->functions.pLast->pNOOPs;

      /* Needed so the pasting of PCODE pieces below will work correctly */
      qsort( ( void * ) pNOOPs, ZH_COMP_PARAM->functions.pLast->nNOOPs, sizeof( ZH_SIZE ), zh_compSort_ZH_SIZE );

      if( ZH_COMP_PARAM->functions.pLast->nJumps )
      {
         ZH_ISIZ * plSizes, * plShifts;
         ZH_SIZE nSize;

         pJumps = ZH_COMP_PARAM->functions.pLast->pJumps;
         nSize = sizeof( ZH_ISIZ ) * ZH_COMP_PARAM->functions.pLast->nJumps;
         plSizes = ( ZH_ISIZ * ) zh_xgrab( nSize );
         plShifts = ( ZH_ISIZ * ) zh_xgrab( nSize );

         for( nJump = 0; nJump < ZH_COMP_PARAM->functions.pLast->nJumps; nJump++ )
            plSizes[ nJump ] = plShifts[ nJump ] = 0;

         /* First Scan NOOPS - Adjust Jump addresses. */
         for( nNOOP = 0; nNOOP < ZH_COMP_PARAM->functions.pLast->nNOOPs; nNOOP++ )
         {
            /* Adjusting preceding jumps that point to code beyond the current NOOP
               or trailing backward jumps pointing to lower address. */
            for( nJump = 0; nJump < ZH_COMP_PARAM->functions.pLast->nJumps; nJump++ )
            {
               nJumpAddr = pJumps[ nJump ];
               switch( pCode[ nJumpAddr ] )
               {
                  case ZH_P_JUMPNEAR:
                  case ZH_P_JUMPFALSENEAR:
                  case ZH_P_JUMPTRUENEAR:
                     nOffset = ( signed char ) pCode[ nJumpAddr + 1 ];
                     break;

                  case ZH_P_JUMP:
                  case ZH_P_JUMPFALSE:
                  case ZH_P_JUMPTRUE:
                     nOffset = ZH_PCODE_MKSHORT( &pCode[ nJumpAddr + 1 ] );
                     break;

                  case ZH_P_JUMPFAR:
                  case ZH_P_JUMPTRUEFAR:
                  case ZH_P_JUMPFALSEFAR:
                  case ZH_P_ALWAYSBEGIN:
                  case ZH_P_SEQALWAYS:
                  case ZH_P_SEQBEGIN:
                  case ZH_P_SEQEND:
                     nOffset = ZH_PCODE_MKINT24( &pCode[ nJumpAddr + 1 ] );
                     break;

                  default:
                     zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'F', ZH_COMP_ERR_JUMP_NOT_FOUND, NULL, NULL );
                     continue;
               }

               /* update jump size */
               if( nOffset > 0 ) /* forward (positive) jump */
               {
                  /* Only if points to code beyond the current fix. */
                  if( pNOOPs[ nNOOP ] > nJumpAddr &&
                      pNOOPs[ nNOOP ] < ( ZH_SIZE ) ( nJumpAddr + nOffset ) )
                     plSizes[ nJump ]--;
               }
               else /* if( nOffset < 0 ) - backward (negative) jump */
               {
                  /* Only if points to code prior the current fix. */
                  if( pNOOPs[ nNOOP ] < nJumpAddr &&
                      pNOOPs[ nNOOP ] >= ( ZH_SIZE ) ( nJumpAddr + nOffset ) )
                     plSizes[ nJump ]++;
               }

               /* update jump address */
               if( pNOOPs[ nNOOP ] < nJumpAddr )
                  plShifts[ nJump ]++;
            }
         }

         for( nJump = 0; nJump < ZH_COMP_PARAM->functions.pLast->nJumps; nJump++ )
         {
            nOffset = plSizes[ nJump ];
            if( nOffset != 0 )
            {
               nJumpAddr = pJumps[ nJump ];
               switch( pCode[ nJumpAddr ] )
               {
                  case ZH_P_JUMPNEAR:
                  case ZH_P_JUMPFALSENEAR:
                  case ZH_P_JUMPTRUENEAR:
                     nOffset += ( signed char ) pCode[ nJumpAddr + 1 ];
                     pCode[ nJumpAddr + 1 ] = ZH_LOBYTE( nOffset );
                     break;

                  case ZH_P_JUMP:
                  case ZH_P_JUMPFALSE:
                  case ZH_P_JUMPTRUE:
                     nOffset += ZH_PCODE_MKSHORT( &pCode[ nJumpAddr + 1 ] );
                     ZH_PUT_LE_UINT16( &pCode[ nJumpAddr + 1 ], nOffset );
                     break;

                  default:
                     nOffset += ZH_PCODE_MKINT24( &pCode[ nJumpAddr + 1 ] );
                     ZH_PUT_LE_UINT24( &pCode[ nJumpAddr + 1 ], nOffset );
                     break;
               }
            }
            pJumps[ nJump ] -= plShifts[ nJump ];
         }
         zh_xfree( plSizes );
         zh_xfree( plShifts );
      }

      nOptimized = nNextByte = 0;
      /* Second Scan, after all adjustments been made, we can copy the optimized code. */
      for( nNOOP = 0; nNOOP < ZH_COMP_PARAM->functions.pLast->nNOOPs; nNOOP++ )
      {
         nBytes2Copy = ( pNOOPs[ nNOOP ] - nNextByte );

         memmove( pCode + nOptimized, pCode + nNextByte, nBytes2Copy );

         nOptimized += nBytes2Copy;
         nNextByte  += nBytes2Copy;

         /* Skip the NOOP and point to next valid byte */
         nNextByte++;
      }

      nBytes2Copy = ( ZH_COMP_PARAM->functions.pLast->nPCodePos - nNextByte );
      memmove( pCode + nOptimized, pCode + nNextByte, nBytes2Copy );
      nOptimized += nBytes2Copy;

      ZH_COMP_PARAM->functions.pLast->nPCodePos  = nOptimized;
      ZH_COMP_PARAM->functions.pLast->nPCodeSize = nOptimized;

      zh_xfree( ZH_COMP_PARAM->functions.pLast->pNOOPs );
      ZH_COMP_PARAM->functions.pLast->pNOOPs = NULL;
      ZH_COMP_PARAM->functions.pLast->nNOOPs = 0;

      if( iPass <= 1 )
      {
         zh_compOptimizePCode( ZH_COMP_PARAM, ZH_COMP_PARAM->functions.pLast );
         zh_compCodeTraceMarkDead( ZH_COMP_PARAM, ZH_COMP_PARAM->functions.pLast );
      }
   }
}

static void zh_compOptimizeFrames( ZH_COMP_DECL, PZH_ZFUNC pFunc )
{
   ZH_USHORT w;

   if( pFunc == NULL )
      return;

   if( pFunc == ZH_COMP_PARAM->pInitFunc )
   {
      if( pFunc->pCode[ 0 ] == ZH_P_STATICS &&
          pFunc->pCode[ 5 ] == ZH_P_SFRAME )
      {
         zh_compSymbolFind( ZH_COMP_PARAM, pFunc->szName, &w, ZH_SYM_FUNCNAME );
         pFunc->pCode[ 1 ] = ZH_LOBYTE( w );
         pFunc->pCode[ 2 ] = ZH_HIBYTE( w );
         pFunc->pCode[ 6 ] = ZH_LOBYTE( w );
         pFunc->pCode[ 7 ] = ZH_HIBYTE( w );

         /* Remove the SFRAME pcode if there's no global static
            initialization: */

         /* NOTE: For some reason this will not work for the static init
            function, so I'm using an ugly hack instead. [vszakats] */
         #if 0
         if( !( pFunc->funFlags & ZH_FUNF_USES_STATICS ) )
         #endif
         if( pFunc->pCode[ 8 ] == ZH_P_ENDPROC )
         {
            pFunc->nPCodePos -= 3;
            memmove( pFunc->pCode + 5, pFunc->pCode + 8, pFunc->nPCodePos - 5 );
         }
         else /* Check Global Statics. */
         {
            #if 0
            PZH_HVAR pVar = pFunc->pStatics;
            #endif
            PZH_HVAR pVar = ZH_COMP_PARAM->functions.pFirst->pStatics;

            while( pVar )
            {
               #if 0
               printf( "\nChecking: %s Used: %i\n", pVar->szName, pVar->iUsed );
               #endif

               if( ! ( pVar->iUsed & ZH_VU_USED ) && ( pVar->iUsed & ZH_VU_INITIALIZED ) )
                  zh_compGenWarning( ZH_COMP_PARAM, zh_comp_szWarnings, 'W', ZH_COMP_WARN_VAL_NOT_USED, pVar->szName, NULL );

               /* May have been initialized in previous execution of the function.
                  else if( ( pVar->iUsed & ZH_VU_USED ) && ! ( pVar->iUsed & ZH_VU_INITIALIZED ) )
                  zh_compGenWarning( ZH_COMP_PARAM, zh_comp_szWarnings, 'W', ZH_COMP_WARN_NOT_INITIALIZED, pVar->szName, NULL );
                */
               pVar = pVar->pNext;
            }
         }
      }
   }
   else if( pFunc->pCode[ 0 ] == ZH_P_FRAME && pFunc->pCode[ 3 ] == ZH_P_SFRAME )
   {
      PZH_HVAR pLocal;
      int iLocals = 0, iOffset = 0;
      ZH_BOOL bSkipFRAME;
      ZH_BOOL bSkipSFRAME;

      pLocal = pFunc->pLocals;

      while( pLocal )
      {
         pLocal = pLocal->pNext;
         iLocals++;
      }

      if( pFunc->funFlags & ZH_FUNF_USES_STATICS )
      {
         zh_compSymbolFind( ZH_COMP_PARAM, ZH_COMP_PARAM->pInitFunc->szName, &w, ZH_SYM_FUNCNAME );
         pFunc->pCode[ 4 ] = ZH_LOBYTE( w );
         pFunc->pCode[ 5 ] = ZH_HIBYTE( w );
         bSkipSFRAME = ZH_FALSE;
      }
      else
         bSkipSFRAME = ZH_TRUE;

      if( iLocals || pFunc->wParamCount )
      {
         /* Parameters declared with PARAMETERS statement are not
          * placed in the local variable list.
          */
         if( pFunc->funFlags & ZH_FUNF_USES_LOCAL_PARAMS )
            iLocals -= pFunc->wParamCount;

         if( iLocals > 255 )
         {
            /* more then 255 local variables,
             * make a room for ZH_P_LARGE[V]FRAME
             */
            zh_compGenPCode1( 0, ZH_COMP_PARAM );
            memmove( pFunc->pCode + 4, pFunc->pCode + 3, pFunc->nPCodePos - 4 );
            pFunc->pCode[ 0 ] = ZH_P_LARGEFRAME;
            pFunc->pCode[ 1 ] = ZH_LOBYTE( iLocals );
            pFunc->pCode[ 2 ] = ZH_HIBYTE( iLocals );
            pFunc->pCode[ 3 ] = ( ZH_BYTE ) pFunc->wParamCount;
            iOffset = 1;
         }
         else
         {
            pFunc->pCode[ 1 ] = ( ZH_BYTE ) iLocals;
            pFunc->pCode[ 2 ] = ( ZH_BYTE ) pFunc->wParamCount;
         }
         bSkipFRAME = ZH_FALSE;
      }
      else
         /* Skip LOCALs frame only when function is not declared with
          * variable number of parameters (ZH_P_[LARGE]VFRAME)
          */
         bSkipFRAME = ! pFunc->fVParams;

      /* Remove the frame pcodes if they are not needed */
      if( bSkipFRAME )
      {
         if( bSkipSFRAME )
         {
            pFunc->nPCodePos -= 6;
            memmove( pFunc->pCode, pFunc->pCode + 6, pFunc->nPCodePos );
         }
         else
         {
            pFunc->nPCodePos -= 3;
            memmove( pFunc->pCode, pFunc->pCode + 3, pFunc->nPCodePos );
         }
      }
      else
      {
         if( pFunc->fVParams )
            pFunc->pCode[ 0 ] = ( ZH_BYTE ) ( iOffset ? ZH_P_LARGEVFRAME : ZH_P_VFRAME );

         if( bSkipSFRAME )
         {
            pFunc->nPCodePos -= 3;
            memmove( pFunc->pCode + 3 + iOffset, pFunc->pCode + 6 + iOffset,
                     pFunc->nPCodePos - 3 - iOffset );
         }
      }
   }
}

static void zh_compWarnUnusedVar( ZH_COMP_DECL, const char * szFuncName,
                                  const char * szVarName, int iDeclLine )
{
   char szFun[ ZH_SYMBOL_NAME_LEN + 17 ];

   if( ZH_COMP_PARAM->iErrorFmt == ZH_ERRORFMT_DEFAULT )
      zh_snprintf( szFun, sizeof( szFun ), "%s(%i)", szFuncName, iDeclLine );
   else
      zh_snprintf( szFun, sizeof( szFun ), "%i:%s", iDeclLine, szFuncName );
   zh_compGenWarning( ZH_COMP_PARAM, zh_comp_szWarnings, 'W',
                      ZH_COMP_WARN_VAR_NOT_USED, szVarName, szFun );
}

static void zh_compFinalizeFunction( ZH_COMP_DECL ) /* fixes all last defined function returns jumps offsets */
{
   PZH_ZFUNC pFunc = ZH_COMP_PARAM->functions.pLast;

   if( pFunc )
   {
      if( ( pFunc->funFlags & ZH_FUNF_WITH_RETURN ) == 0 )
      {
         /* The last statement in a function/procedure was not a RETURN
          * Generate end-of-procedure pcode
          */
         zh_compGenPCode1( ZH_P_ENDPROC, ZH_COMP_PARAM );
      }

      zh_compCheckUnclosedStru( ZH_COMP_PARAM, pFunc );

      zh_compRTVariableKill( ZH_COMP_PARAM, pFunc );
      zh_compSwitchKill( ZH_COMP_PARAM, pFunc );
      zh_compElseIfKill( pFunc );
      zh_compLoopKill( pFunc );

      if( ZH_COMP_PARAM->iWarnings &&
          ( pFunc->funFlags & ZH_FUNF_FILE_DECL ) == 0 )
      {
         PZH_HVAR pVar;

         pVar = pFunc->pLocals;
         while( pVar )
         {
            if( pVar->szName && ( pVar->iUsed & ZH_VU_USED ) == 0 )
               zh_compWarnUnusedVar( ZH_COMP_PARAM, pFunc->szName, pVar->szName, pVar->iDeclLine );
            pVar = pVar->pNext;
         }

         pVar = pFunc->pStatics;
         while( pVar )
         {
            if( pVar->szName && ( pVar->iUsed & ZH_VU_USED ) == 0 )
               zh_compWarnUnusedVar( ZH_COMP_PARAM, pFunc->szName, pVar->szName, pVar->iDeclLine );
            pVar = pVar->pNext;
         }

         /* Check if the function returned some value
          */
         if( ( pFunc->funFlags & ZH_FUNF_WITH_RETURN ) == 0 &&
             ( pFunc->funFlags & ZH_FUNF_PROCEDURE ) == 0 )
            zh_compGenWarning( ZH_COMP_PARAM, zh_comp_szWarnings, 'W', ZH_COMP_WARN_FUN_WITH_NO_RETURN,
                               pFunc->szName, NULL );
      }

      if( ! pFunc->bError )
      {
         if( pFunc->wParamCount && ( pFunc->funFlags & ZH_FUNF_USES_LOCAL_PARAMS ) == 0 )
         {
            /* There was a PARAMETERS statement used.
             * NOTE: This fixes local variables references in a case when
             * there is PARAMETERS statement after a LOCAL variable declarations.
             * All local variables are numbered from 1 - which means use first
             * item from the eval stack. However if PARAMETERS statement is used
             * then there are additional items on the eval stack - the
             * function arguments. Then first local variable is at the position
             * (1 + <number of arguments>). We cannot fix this numbering
             * because the PARAMETERS statement can be used even at the end
             * of function body when all local variables are already created.
             */
            zh_compFixFuncPCode( ZH_COMP_PARAM, pFunc );
         }

         zh_compPCodeTraceOptimizer( ZH_COMP_PARAM );
         zh_compOptimizeJumps( ZH_COMP_PARAM );
      }
   }
}

/*
 * This function creates and initializes the ZH_ZFUNC structure
 */
static PZH_ZFUNC zh_compFunctionNew( ZH_COMP_DECL, const char * szName, ZH_SYMBOLSCOPE cScope )
{
   PZH_ZFUNC pFunc = ( PZH_ZFUNC ) zh_xgrabz( sizeof( ZH_ZFUNC ) );

   pFunc->szName         = szName;
   pFunc->cScope         = cScope;
   pFunc->iStaticsBase   = ZH_COMP_PARAM->iStaticCnt;
   pFunc->iEarlyEvalPass = 0;
   pFunc->fVParams       = ZH_FALSE;
   pFunc->bError         = ZH_FALSE;

   return pFunc;
}

static PZH_HINLINE zh_compInlineNew( ZH_COMP_DECL, const char * szName, int iLine )
{
   PZH_HINLINE pInline = ( PZH_HINLINE ) zh_xgrab( sizeof( ZH_HINLINE ) );

   pInline->szName     = szName;
   pInline->pCode      = NULL;
   pInline->nPCodeSize = 0;
   pInline->pNext      = NULL;
   pInline->szFileName = zh_compIdentifierNew( ZH_COMP_PARAM,
                  zh_pp_fileName( ZH_COMP_PARAM->pLex->pPP ), ZH_IDENT_COPY );
   pInline->iLine      = iLine;

   return pInline;
}

/* NOTE: Names of variables and functions are released in hbident.c on exit */
static PZH_ZFUNC zh_compFunctionKill( ZH_COMP_DECL, PZH_ZFUNC pFunc )
{
   PZH_ZFUNC pNext = pFunc->pNext;
   PZH_ENUMERATOR pEVar;
   PZH_HVAR pVar;

   zh_compRTVariableKill( ZH_COMP_PARAM, pFunc );
   zh_compSwitchKill( ZH_COMP_PARAM, pFunc );
   zh_compElseIfKill( pFunc );
   zh_compLoopKill( pFunc );

   while( pFunc->pLocals )
   {
      pVar = pFunc->pLocals;
      pFunc->pLocals = pVar->pNext;
      zh_xfree( pVar );
   }

   while( pFunc->pStatics )
   {
      pVar = pFunc->pStatics;
      pFunc->pStatics = pVar->pNext;
      zh_xfree( pVar );
   }

   while( pFunc->pFields )
   {
      pVar = pFunc->pFields;
      pFunc->pFields = pVar->pNext;
      zh_xfree( pVar );
   }

   while( pFunc->pMemvars )
   {
      pVar = pFunc->pMemvars;
      pFunc->pMemvars = pVar->pNext;
      zh_xfree( pVar );
   }

   while( pFunc->pDetached )
   {
      pVar = pFunc->pDetached;
      pFunc->pDetached = pVar->pNext;
      zh_xfree( pVar );
   }

   while( pFunc->pPrivates )
   {
      pVar = pFunc->pPrivates;
      pFunc->pPrivates = pVar->pNext;
      zh_xfree( pVar );
   }

   while( pFunc->pEnum )
   {
      pEVar = pFunc->pEnum;
      pFunc->pEnum = pEVar->pNext;
      zh_xfree( pEVar );
   }

   /* Release the NOOP array. */
   if( pFunc->pNOOPs )
      zh_xfree( pFunc->pNOOPs );

   /* Release the Jumps array. */
   if( pFunc->pJumps )
      zh_xfree( pFunc->pJumps );

   if( pFunc->pCode )
      zh_xfree( pFunc->pCode );

   zh_xfree( pFunc );

   return pNext;
}

/*
 * This function adds the name of external symbol into the list of externals
 * as they have to be placed on the symbol table later than the first
 * public symbol
 */
void zh_compExternAdd( ZH_COMP_DECL, const char * szExternName, ZH_SYMBOLSCOPE cScope ) /* defines a new extern name */
{
   PZH_HEXTERN * pExtern;

   if( strcmp( "_GET_", szExternName ) == 0 )
   {
      /* special function to implement @ GET statement */
      zh_compExternAdd( ZH_COMP_PARAM, "__GETA", 0 );
      szExternName = "__GET";
   }

   pExtern = &ZH_COMP_PARAM->externs;
   while( *pExtern )
   {
      if( strcmp( ( *pExtern )->szName, szExternName ) == 0 )
         break;
      pExtern = &( *pExtern )->pNext;
   }
   if( *pExtern )
      ( *pExtern )->cScope |= cScope;
   else
   {
      *pExtern = ( PZH_HEXTERN ) zh_xgrab( sizeof( ZH_HEXTERN ) );
      ( *pExtern )->szName = szExternName;
      ( *pExtern )->cScope = cScope;
      ( *pExtern )->pNext  = NULL;
   }
}

static void zh_compAddFunc( ZH_COMP_DECL, PZH_ZFUNC pFunc )
{
   while( ZH_COMP_PARAM->functions.pLast &&
          ! ZH_COMP_PARAM->functions.pLast->szName )
   {
      PZH_ZFUNC pBlock = ZH_COMP_PARAM->functions.pLast;
      ZH_COMP_PARAM->functions.pLast = pBlock->pOwner;
      zh_compFunctionKill( ZH_COMP_PARAM, pBlock );
   }

   if( ZH_COMP_PARAM->functions.iCount == 0 )
      ZH_COMP_PARAM->functions.pFirst = pFunc;
   else
      ZH_COMP_PARAM->functions.pLast->pNext = pFunc;
   ZH_COMP_PARAM->functions.pLast = pFunc;
   ZH_COMP_PARAM->functions.iCount++;
}

static PZH_ZFUNC zh_compFunctionFind( ZH_COMP_DECL, const char * szName, ZH_BOOL fLocal )
{
   PZH_ZFUNC pFunc;

   if( ZH_COMP_PARAM->iModulesCount <= 1 )
   {
      pFunc = ZH_COMP_PARAM->functions.pFirst;
      fLocal = ZH_TRUE;
   }
   else
      pFunc = fLocal ? ZH_COMP_PARAM->pDeclFunc :
                       ZH_COMP_PARAM->functions.pFirst;
   while( pFunc )
   {
      if( pFunc == ZH_COMP_PARAM->pDeclFunc )
         fLocal = ZH_TRUE;

      if( ( pFunc->funFlags & ZH_FUNF_FILE_DECL ) == 0 &&
          ( fLocal || ( pFunc->cScope & ( ZH_FS_STATIC | ZH_FS_INITEXIT ) ) == 0 ) &&
          strcmp( pFunc->szName, szName ) == 0 )
         break;

      pFunc = pFunc->pNext;
   }
   return pFunc;
}

static ZH_BOOL zh_compIsModuleFunc( ZH_COMP_DECL, const char * szFunctionName )
{
   PZH_ZFUNC pFunc = ZH_COMP_PARAM->functions.pFirst;

   while( pFunc )
   {
      if( ( pFunc->cScope & ZH_FS_STATIC ) == 0 &&
          zh_stricmp( pFunc->szName, szFunctionName ) == 0 )
         break;
      pFunc = pFunc->pNext;
   }
   return pFunc != NULL;
}

static void zh_compUpdateFunctionNames( ZH_COMP_DECL )
{
   if( ZH_COMP_PARAM->iModulesCount > 1 )
   {
      PZH_ZFUNC pFunc = ZH_COMP_PARAM->functions.pFirst;

      while( pFunc )
      {
         if( ( pFunc->cScope & ( ZH_FS_STATIC | ZH_FS_INITEXIT ) ) != 0 )
         {
            PZH_HSYMBOL pSym = ZH_COMP_PARAM->symbols.pFirst, pFuncSym = NULL;
            ZH_BOOL fExists = ZH_FALSE;

            while( pSym )
            {
               if( pSym->iFunc )
               {
                  if( pSym->pFunc == pFunc )
                     pFuncSym = pSym;
                  else if( ( ( pSym->cScope & ZH_FS_LOCAL ) != 0 ||
                        ( pSym->cScope & ZH_FS_DEFERRED ) == 0 ) &&
                      strcmp( pFunc->szName, pSym->szName ) == 0 )
                     fExists = ZH_TRUE;

                  if( pFuncSym && fExists )
                  {
                     pFunc->iFuncSuffix = pFuncSym->iFunc;
                     break;
                  }
               }
               pSym = pSym->pNext;
            }
         }
         pFunc = pFunc->pNext;
      }
   }
}

static ZH_BOOL zh_compCheckReservedNames( ZH_COMP_DECL, const char * szFunName, ZH_BOOL fError )
{
   const char * szFunction;
   ZH_FUNC_ID funcID;
   int iFlags;

   szFunction = zh_compGetFuncID( szFunName, &funcID, &iFlags );
   if( iFlags & ZH_FN_RESERVED )
   {
      if( fError )
         zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_FUNC_RESERVED, szFunction, szFunName );
      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}

static ZH_BOOL zh_compRegisterFunc( ZH_COMP_DECL, PZH_ZFUNC pFunc, ZH_BOOL fError )
{
   if( zh_compFunctionFind( ZH_COMP_PARAM, pFunc->szName,
                            ( pFunc->cScope & ZH_FS_STATIC ) != 0 ) )
   {
      /* The name of a function/procedure is already defined */
      if( fError )
         zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_FUNC_DUPL, pFunc->szName, NULL );
   }
   else if( ! zh_compCheckReservedNames( ZH_COMP_PARAM, pFunc->szName, fError ) )
   {
      PZH_HSYMBOL pSym = zh_compSymbolFind( ZH_COMP_PARAM, pFunc->szName, NULL, ZH_SYM_FUNCNAME );
      if( ! pSym )
         pSym = zh_compSymbolAdd( ZH_COMP_PARAM, pFunc->szName, NULL, ZH_SYM_FUNCNAME );
      pSym->cScope |= pFunc->cScope | ZH_FS_LOCAL;
      pSym->pFunc = pFunc;
      return ZH_TRUE;
   }
   return ZH_FALSE;
}

/*
 * Stores a defined function/procedure
 * szFunName - name of a function
 * cScope    - scope of a function
 * iType     - ZH_FUNF_PROCEDURE if a procedure or 0
 */
void zh_compFunctionAdd( ZH_COMP_DECL, const char * szFunName, ZH_SYMBOLSCOPE cScope, int iType )
{
   PZH_ZFUNC pFunc;

   zh_compFinalizeFunction( ZH_COMP_PARAM );    /* fix all previous function returns offsets */

   if( cScope & ( ZH_FS_INIT | ZH_FS_EXIT ) )
   {
      char szNewName[ ZH_SYMBOL_NAME_LEN + 1 ];
      int iLen;

      iLen = ( int ) strlen( szFunName );
      if( iLen >= ZH_SYMBOL_NAME_LEN )
         iLen = ZH_SYMBOL_NAME_LEN - 1;
      memcpy( szNewName, szFunName, iLen );
      szNewName[ iLen ] ='$';
      szNewName[ iLen + 1 ] = '\0';
      szFunName = zh_compIdentifierNew( ZH_COMP_PARAM, szNewName, ZH_IDENT_COPY );
   }

   pFunc = zh_compFunctionNew( ZH_COMP_PARAM, szFunName, cScope );
   pFunc->funFlags |= iType;

   if( ( iType & ZH_FUNF_FILE_DECL ) == 0 )
      zh_compRegisterFunc( ZH_COMP_PARAM, pFunc, ZH_TRUE );

   if( ( iType & ( ZH_FUNF_FILE_DECL | ZH_FUNF_FILE_FIRST ) ) != 0 )
      ZH_COMP_PARAM->pDeclFunc = pFunc;

   zh_compAddFunc( ZH_COMP_PARAM, pFunc );

   ZH_COMP_PARAM->ilastLineErr = 0; /* position of last syntax error (line number) */

   zh_compGenPCode3( ZH_P_FRAME, 0, 0, ZH_COMP_PARAM );  /* frame for locals and parameters */
   zh_compGenPCode3( ZH_P_SFRAME, 0, 0, ZH_COMP_PARAM ); /* frame for statics variables */

   if( ZH_COMP_PARAM->fDebugInfo )
      zh_compGenModuleName( ZH_COMP_PARAM, szFunName );
   else
      ZH_COMP_PARAM->lastLine = -1;
}

/* create an ANNOUNCEd procedure
 */
static void zh_compAnnounce( ZH_COMP_DECL, const char * szFunName )
{
   PZH_ZFUNC pFunc;

   zh_compCheckReservedNames( ZH_COMP_PARAM, szFunName, ZH_TRUE );

   pFunc = zh_compFunctionFind( ZH_COMP_PARAM, szFunName, ZH_FALSE );
   if( pFunc )
   {
      /* there is a function/procedure defined already - ANNOUNCEd procedure
       * have to be a public symbol - check if existing symbol is public
       */
      if( pFunc->cScope & ZH_FS_STATIC )
         zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'F', ZH_COMP_ERR_FUNC_ANNOUNCE, szFunName, NULL );
   }
   else
   {
      PZH_HSYMBOL pSym;

      /* create a new procedure
       */
      pFunc = zh_compFunctionNew( ZH_COMP_PARAM, szFunName, ZH_FS_LOCAL );
      pFunc->funFlags |= ZH_FUNF_PROCEDURE;

      pSym = zh_compSymbolFind( ZH_COMP_PARAM, szFunName, NULL, ZH_SYM_FUNCNAME );
      if( ! pSym )
         pSym = zh_compSymbolAdd( ZH_COMP_PARAM, szFunName, NULL, ZH_SYM_FUNCNAME );
      pSym->cScope |= pFunc->cScope;
      pSym->pFunc = pFunc;

      zh_compAddFunc( ZH_COMP_PARAM, pFunc );

      /* this function have a very limited functionality
       */
      zh_compGenPCode1( ZH_P_ENDPROC, ZH_COMP_PARAM );
   }
}

void zh_compFunctionMarkStatic( ZH_COMP_DECL, const char * szFunName )
{
   PZH_HSYMBOL pSym = zh_compSymbolFind( ZH_COMP_PARAM, szFunName, NULL, ZH_SYM_FUNCNAME );

   if( pSym )
   {
      if( ( pSym->cScope & ( ZH_FS_DEFERRED | ZH_FS_LOCAL ) ) == 0 )
         pSym->cScope |= ZH_FS_STATIC | ZH_FS_LOCAL;
   }
}

PZH_HINLINE zh_compInlineAdd( ZH_COMP_DECL, const char * szFunName, int iLine )
{
   PZH_HINLINE pInline;
   PZH_HSYMBOL pSym;

   if( szFunName )
   {
      pSym = zh_compSymbolFind( ZH_COMP_PARAM, szFunName, NULL, ZH_SYM_FUNCNAME );
      if( ! pSym )
         pSym = zh_compSymbolAdd( ZH_COMP_PARAM, szFunName, NULL, ZH_SYM_FUNCNAME );
      pSym->cScope |= ZH_FS_STATIC | ZH_FS_LOCAL;
   }
   pInline = zh_compInlineNew( pComp, szFunName, iLine );

   if( ZH_COMP_PARAM->inlines.iCount == 0 )
   {
      ZH_COMP_PARAM->inlines.pFirst = pInline;
      ZH_COMP_PARAM->inlines.pLast  = pInline;
   }
   else
   {
      ZH_COMP_PARAM->inlines.pLast->pNext = pInline;
      ZH_COMP_PARAM->inlines.pLast = pInline;
   }

   ZH_COMP_PARAM->inlines.iCount++;

   return pInline;
}

void zh_compGenBreak( ZH_COMP_DECL )
{
   zh_compGenPushFunCall( "BREAK", ZH_FN_RESERVED, ZH_COMP_PARAM );
}

/* generates the symbols for the EXTERN names */
static void zh_compExternGen( ZH_COMP_DECL )
{
   PZH_HEXTERN pDelete;

   while( ZH_COMP_PARAM->externs )
   {
      ZH_SYMBOLSCOPE cScope = ZH_COMP_PARAM->externs->cScope;
      PZH_HSYMBOL pSym = zh_compSymbolFind( ZH_COMP_PARAM, ZH_COMP_PARAM->externs->szName, NULL, ZH_SYM_FUNCNAME );

      if( pSym )
      {
         pSym->cScope |= cScope;
      }
      else if( ( cScope & ZH_FS_DEFERRED ) == 0 )
      {
         pSym = zh_compSymbolAdd( ZH_COMP_PARAM, ZH_COMP_PARAM->externs->szName, NULL, ZH_SYM_FUNCNAME );
         pSym->cScope |= cScope;
      }
      pDelete = ZH_COMP_PARAM->externs;
      ZH_COMP_PARAM->externs = ZH_COMP_PARAM->externs->pNext;
      zh_xfree( pDelete );
   }
}

static void zh_compNOOPadd( PZH_ZFUNC pFunc, ZH_SIZE nPos )
{
   pFunc->pCode[ nPos ] = ZH_P_NOOP;

   if( pFunc->nNOOPs )
      pFunc->pNOOPs = ( ZH_SIZE * ) zh_xrealloc( pFunc->pNOOPs, sizeof( ZH_SIZE ) * ( pFunc->nNOOPs + 1 ) );
   else
      pFunc->pNOOPs = ( ZH_SIZE * ) zh_xgrab( sizeof( ZH_SIZE ) );
   pFunc->pNOOPs[ pFunc->nNOOPs++ ] = nPos;
}

static void zh_compPrepareJumps( ZH_COMP_DECL )
{
   PZH_ZFUNC pFunc = ZH_COMP_PARAM->functions.pLast;

   if( pFunc->nJumps )
      pFunc->pJumps = ( ZH_SIZE * ) zh_xrealloc( pFunc->pJumps, sizeof( ZH_SIZE ) * ( pFunc->nJumps + 1 ) );
   else
      pFunc->pJumps = ( ZH_SIZE * ) zh_xgrab( sizeof( ZH_SIZE ) );
   pFunc->pJumps[ pFunc->nJumps++ ] = ( ZH_SIZE ) ( pFunc->nPCodePos - 4 );
}

ZH_SIZE zh_compGenJump( ZH_ISIZ nOffset, ZH_COMP_DECL )
{
   if( ! ZH_LIM_INT24( nOffset ) )
      zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'F', ZH_COMP_ERR_JUMP_TOO_LONG, NULL, NULL );

   zh_compGenPCode4( ZH_P_JUMPFAR, ZH_LOBYTE( nOffset ), ZH_HIBYTE( nOffset ), ( ZH_BYTE ) ( ( nOffset >> 16 ) & 0xFF ), ZH_COMP_PARAM );
   zh_compPrepareJumps( ZH_COMP_PARAM );

   return ZH_COMP_PARAM->functions.pLast->nPCodePos - 3;
}

ZH_SIZE zh_compGenJumpFalse( ZH_ISIZ nOffset, ZH_COMP_DECL )
{
   if( ! ZH_LIM_INT24( nOffset ) )
      zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'F', ZH_COMP_ERR_JUMP_TOO_LONG, NULL, NULL );

   zh_compGenPCode4( ZH_P_JUMPFALSEFAR, ZH_LOBYTE( nOffset ), ZH_HIBYTE( nOffset ), ( ZH_BYTE ) ( ( nOffset >> 16 ) & 0xFF ), ZH_COMP_PARAM );
   zh_compPrepareJumps( ZH_COMP_PARAM );

   return ZH_COMP_PARAM->functions.pLast->nPCodePos - 3;
}

ZH_SIZE zh_compGenJumpTrue( ZH_ISIZ nOffset, ZH_COMP_DECL )
{
   if( ! ZH_LIM_INT24( nOffset ) )
      zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'F', ZH_COMP_ERR_JUMP_TOO_LONG, NULL, NULL );

   zh_compGenPCode4( ZH_P_JUMPTRUEFAR, ZH_LOBYTE( nOffset ), ZH_HIBYTE( nOffset ), ( ZH_BYTE ) ( ( nOffset >> 16 ) & 0xFF ), ZH_COMP_PARAM );
   zh_compPrepareJumps( ZH_COMP_PARAM );

   return ZH_COMP_PARAM->functions.pLast->nPCodePos - 3;
}

void zh_compGenJumpThere( ZH_SIZE nFrom, ZH_SIZE nTo, ZH_COMP_DECL )
{
   ZH_BYTE * pCode = ZH_COMP_PARAM->functions.pLast->pCode;
   ZH_ISIZ nOffset = nTo - nFrom + 1;

   if( ZH_LIM_INT24( nOffset ) )
   {
      ZH_PUT_LE_UINT24( &pCode[ nFrom ], nOffset );
   }
   else
      zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'F', ZH_COMP_ERR_JUMP_TOO_LONG, NULL, NULL );
}

void zh_compGenJumpHere( ZH_SIZE nOffset, ZH_COMP_DECL )
{
   zh_compGenJumpThere( nOffset, ZH_COMP_PARAM->functions.pLast->nPCodePos, ZH_COMP_PARAM );
}

void zh_compLinePush( ZH_COMP_DECL ) /* generates the pcode with the currently compiled source code line */
{
   if( ZH_COMP_PARAM->fLineNumbers )
   {
      if( ZH_COMP_PARAM->fDebugInfo && ZH_COMP_PARAM->lastModule != ZH_COMP_PARAM->currModule )
         zh_compGenModuleName( ZH_COMP_PARAM, NULL );

      if( ZH_COMP_PARAM->currLine != ZH_COMP_PARAM->lastLine )
      {
         zh_compGenPCode3( ZH_P_LINE, ZH_LOBYTE( ZH_COMP_PARAM->currLine ),
                                      ZH_HIBYTE( ZH_COMP_PARAM->currLine ), ZH_COMP_PARAM );
         ZH_COMP_PARAM->lastLine = ZH_COMP_PARAM->currLine;
      }
   }

   if( ZH_COMP_PARAM->functions.pLast->funFlags & ZH_FUNF_BREAK_CODE )
   {
      /* previous line contained RETURN/BREAK/LOOP/EXIT statement */
      zh_compGenWarning( ZH_COMP_PARAM, zh_comp_szWarnings, 'W', ZH_COMP_WARN_UNREACHABLE, NULL, NULL );
      /* clear RETURN/BREAK flag */
      ZH_COMP_PARAM->functions.pLast->funFlags &= ~ ( /*ZH_FUNF_WITH_RETURN |*/ ZH_FUNF_BREAK_CODE );
   }
}

/*
 * Test if we can generate statement (without line pushing)
 */
void zh_compStatmentStart( ZH_COMP_DECL )
{
   if( ( ZH_COMP_PARAM->functions.pLast->funFlags & ZH_FUNF_STATEMENTS ) == 0 )
   {
      PZH_ZFUNC pFunc = ZH_COMP_PARAM->functions.pLast;

      if( ( pFunc->funFlags & ZH_FUNF_FILE_DECL ) != 0 )
      {
         if( ZH_COMP_PARAM->iStartProc == 2 && pFunc->szName[ 0 ] &&
             zh_compRegisterFunc( ZH_COMP_PARAM, pFunc, ZH_FALSE ) )
            pFunc->funFlags &= ~ZH_FUNF_FILE_DECL;
         else
            zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_OUTSIDE, NULL, NULL );
      }
      pFunc->funFlags |= ZH_FUNF_STATEMENTS;
   }
}

void zh_compLinePushIfInside( ZH_COMP_DECL ) /* generates the pcode with the currently compiled source code line */
{
   zh_compStatmentStart( ZH_COMP_PARAM );
   zh_compLinePush( ZH_COMP_PARAM );
}

/* Generates the pcode with the currently compiled source code line
 * if debug code was requested only
 */
void zh_compLinePushIfDebugger( ZH_COMP_DECL )
{
   zh_compStatmentStart( ZH_COMP_PARAM );

   if( ZH_COMP_PARAM->fDebugInfo )
      zh_compLinePush( ZH_COMP_PARAM );
   else
   {
      if( ZH_COMP_PARAM->functions.pLast->funFlags & ZH_FUNF_BREAK_CODE )
      {
         /* previous line contained RETURN/BREAK/LOOP/EXIT statement */
         zh_compGenWarning( ZH_COMP_PARAM, zh_comp_szWarnings, 'W', ZH_COMP_WARN_UNREACHABLE, NULL, NULL );
      }
      ZH_COMP_PARAM->functions.pLast->funFlags &= ~( /*ZH_FUNF_WITH_RETURN |*/ ZH_FUNF_BREAK_CODE );   /* clear RETURN flag */
   }
}

/* generates the pcode with the currently compiled module and function name */
void zh_compGenModuleName( ZH_COMP_DECL, const char * szFunName )
{
   zh_compGenPCode1( ZH_P_MODULENAME, ZH_COMP_PARAM );
   zh_compGenPCodeN( ( const ZH_BYTE * ) ZH_COMP_PARAM->currModule,
                     strlen( ZH_COMP_PARAM->currModule ), ZH_COMP_PARAM );
   zh_compGenPCode1( ':', ZH_COMP_PARAM );
   if( szFunName && *szFunName )
      zh_compGenPCodeN( ( const ZH_BYTE * ) szFunName, strlen( szFunName ) + 1, ZH_COMP_PARAM );
   else /* special version "filename:" when the file changes within function */
      zh_compGenPCode1( '\0', ZH_COMP_PARAM );
   ZH_COMP_PARAM->lastModule = ZH_COMP_PARAM->currModule;
   ZH_COMP_PARAM->lastLine = -1;
}


/*
 * Function generates passed pcode for passed runtime variable
 * (field or memvar)
 */
static void zh_compGenVarPCode( ZH_BYTE bPCode, const char * szVarName, ZH_COMP_DECL )
{
   ZH_USHORT wVar;
   PZH_HSYMBOL pSym;

   /* Check if this variable name is placed into the symbol table
    */
   pSym = zh_compSymbolFind( ZH_COMP_PARAM, szVarName, &wVar, ZH_SYM_MEMVAR );
   if( ! pSym )
      pSym = zh_compSymbolAdd( ZH_COMP_PARAM, szVarName, &wVar, ZH_SYM_MEMVAR );
   pSym->cScope |= ZH_FS_MEMVAR;

   if( bPCode == ZH_P_PUSH_ALIASED_FIELD && wVar <= 255 )
      zh_compGenPCode2( ZH_P_PUSH_ALIASED_FIELDNEAR, ( ZH_BYTE ) wVar, ZH_COMP_PARAM );
   else if( bPCode == ZH_P_POPALIASEDFIELD && wVar <= 255 )
      zh_compGenPCode2( ZH_P_POPALIASEDFIELDNEAR, ( ZH_BYTE ) wVar, ZH_COMP_PARAM );
   else
      zh_compGenPCode3( bPCode, ZH_LOBYTE( wVar ), ZH_HIBYTE( wVar ), ZH_COMP_PARAM );
}

/*
 * Function generates pcode for undeclared variable
 */
static void zh_compGenVariablePCode( ZH_COMP_DECL, ZH_BYTE bPCode, const char * szVarName )
{
   ZH_BOOL bGenCode;

   bGenCode = ZH_COMP_PARAM->fForceMemvars;
   
   if( bGenCode )
   {
      /* -v switch was used -> assume it is a memvar variable
       */
      if( ZH_COMP_PARAM->functions.pLast->iEarlyEvalPass == 0 || ZH_SUPPORT_MACRODECL )
         zh_compGenWarning( ZH_COMP_PARAM, zh_comp_szWarnings, 'W', ZH_COMP_WARN_MEMVAR_ASSUMED, szVarName, NULL );

      if( bPCode == ZH_P_POPVARIABLE )
         bPCode = ZH_P_POPMEMVAR;
      else if( bPCode == ZH_P_PUSHVARIABLE )
         bPCode = ZH_P_PUSHMEMVAR;
      else
         bPCode = ZH_P_PUSHMEMVARREF;
   }
   else if( ZH_COMP_PARAM->functions.pLast->iEarlyEvalPass == 0 || ZH_SUPPORT_MACRODECL )
      zh_compGenWarning( ZH_COMP_PARAM, zh_comp_szWarnings, 'W', ZH_COMP_WARN_AMBIGUOUS_VAR, szVarName, NULL );

   zh_compGenVarPCode( bPCode, szVarName, ZH_COMP_PARAM );
}

/* Generate a pcode for a field variable
 */
static void zh_compGenFieldPCode( ZH_COMP_DECL, ZH_BYTE bPCode, PZH_HVAR pField )
{
   if( pField->szAlias )
   {
      /* the alias was specified in FIELD declaration
       * Push alias symbol before the field symbol
       */
      if( bPCode == ZH_P_POPFIELD )
         bPCode = ZH_P_POPALIASEDFIELD;
      else if( bPCode == ZH_P_PUSHFIELD )
         bPCode = ZH_P_PUSH_ALIASED_FIELD;

      zh_compGenPushSymbol( pField->szAlias, ZH_SYM_ALIAS, ZH_COMP_PARAM );
   }
   zh_compGenVarPCode( bPCode, pField->szName, ZH_COMP_PARAM );
}

/* sends a message to an object */
/* bIsObject = ZH_TRUE if we are sending a message to real object
   bIsObject is ZH_FALSE if we are sending a message to an object specified
   with WITH OBJECT statement.
 */
void zh_compGenMessage( const char * szMsgName, ZH_BOOL bIsObject, ZH_COMP_DECL )
{
   ZH_USHORT wSym;
   PZH_HSYMBOL pSym;

   if( szMsgName )
   {
      pSym = zh_compSymbolFind( ZH_COMP_PARAM, szMsgName, &wSym, ZH_SYM_MSGNAME );
      if( ! pSym )  /* the symbol was not found on the symbol table */
         pSym = zh_compSymbolAdd( ZH_COMP_PARAM, szMsgName, &wSym, ZH_SYM_MSGNAME );
      pSym->cScope |= ZH_FS_MESSAGE;
      if( bIsObject )
         zh_compGenPCode3( ZH_P_MESSAGE, ZH_LOBYTE( wSym ), ZH_HIBYTE( wSym ), ZH_COMP_PARAM );
      else
         zh_compGenPCode3( ZH_P_WITHOBJECTMESSAGE, ZH_LOBYTE( wSym ), ZH_HIBYTE( wSym ), ZH_COMP_PARAM );
   }
   else
   {
      wSym = 0xFFFF;
      zh_compGenPCode3( ZH_P_WITHOBJECTMESSAGE, ZH_LOBYTE( wSym ), ZH_HIBYTE( wSym ), ZH_COMP_PARAM );
   }

   if( ! bIsObject && ZH_COMP_PARAM->functions.pLast->iEarlyEvalPass == 1 )
   {
      if( ZH_SUPPORT_MACRODECL )
         ZH_COMP_PARAM->functions.pLast->iEarlyEvalPass = 0;
      else
         zh_compErrorCodeblockWith( ZH_COMP_PARAM, szMsgName ? szMsgName : "&..." );
   }
}

void zh_compGenMessageData( const char * szMsg, ZH_BOOL bIsObject, ZH_COMP_DECL ) /* generates an underscore-symbol name for a data assignment */
{
   char szResult[ ZH_SYMBOL_NAME_LEN + 1 ];
   int iLen = ( int ) strlen( szMsg );

   if( iLen >= ZH_SYMBOL_NAME_LEN )
      iLen = ZH_SYMBOL_NAME_LEN - 1;
   szResult[ 0 ] = '_';
   memcpy( szResult + 1, szMsg, iLen );
   szResult[ iLen + 1 ] = '\0';

   zh_compGenMessage( zh_compIdentifierNew( ZH_COMP_PARAM, szResult, ZH_IDENT_COPY ), bIsObject, ZH_COMP_PARAM );
}

static void zh_compCheckEarlyMacroEval( ZH_COMP_DECL, const char * szVarName, int iScope )
{
   if( iScope == ZH_VS_CBLOCAL_VAR ||
       /* iScope == ZH_VS_LOCAL_VAR || */ /* codeblock parameters */
       iScope == ZH_VS_STATIC_VAR ||
       iScope == ZH_VS_GLOBAL_STATIC ||
       iScope == ZH_VS_LOCAL_FIELD ||
       iScope == ZH_VS_GLOBAL_FIELD )
   {
      /* Disable early evaluation if codeblock contains macros and
       * declared variables, i.e. {| x | cLocal + &cPriv }
       * and support for macros with declared symbols is enabled.
       */
      if( ZH_COMP_PARAM->functions.pLast->iEarlyEvalPass == 1 &&
          ZH_SUPPORT_MACRODECL )
         ZH_COMP_PARAM->functions.pLast->iEarlyEvalPass = 0;
      else
         zh_compErrorCodeblockDecl( ZH_COMP_PARAM, szVarName );
   }
}

/* Check variable in the following order:
 * LOCAL variable
 *    local STATIC variable
 *       local FIELD variable
 *  local MEMVAR variable
 * global STATIC variable
 *    global FIELD variable
 *       global MEMVAR variable
 * (if not found - it is an undeclared variable)
 */
void zh_compGenPopVar( const char * szVarName, ZH_COMP_DECL ) /* generates the pcode to pop a value from the virtual machine stack onto a variable */
{
   int iVar, iScope;
   PZH_HVAR pVar;

   pVar = zh_compVariableFind( ZH_COMP_PARAM, szVarName, &iVar, &iScope );
   if( pVar )
   {
      if( ZH_COMP_PARAM->functions.pLast->iEarlyEvalPass == 1 )
         zh_compCheckEarlyMacroEval( ZH_COMP_PARAM, szVarName, iScope );

      switch( iScope )
      {
         case ZH_VS_LOCAL_VAR:
         case ZH_VS_CBLOCAL_VAR:
            /* local variable */
            /* local variables used in a codeblock will not be adjusted
             * if PARAMETERS statement will be used then it is safe to
             * use 2 bytes for LOCALNEAR
             */
            if( ZH_LIM_INT8( iVar ) && ! ZH_COMP_PARAM->functions.pLast->szName &&
                !( ZH_COMP_PARAM->functions.pLast->funFlags & ZH_FUNF_EXTBLOCK ) )
               zh_compGenPCode2( ZH_P_POPLOCALNEAR, ( ZH_BYTE ) iVar, ZH_COMP_PARAM );
            else
               zh_compGenPCode3( ZH_P_POPLOCAL, ZH_LOBYTE( iVar ), ZH_HIBYTE( iVar ), ZH_COMP_PARAM );
            break;

         case ZH_VS_STATIC_VAR:
         case ZH_VS_GLOBAL_STATIC:
            /* Static variable */
            zh_compGenPCode3( ZH_P_POPSTATIC, ZH_LOBYTE( iVar ), ZH_HIBYTE( iVar ), ZH_COMP_PARAM );
            {
               PZH_ZFUNC pFunc;
               /* Check if we are generating a pop code for static variable
                * initialization function - if YES then we have to switch to a function
                * where the static variable was declared
                */
               pFunc = ZH_COMP_PARAM->functions.pLast;
               if( ( ZH_COMP_PARAM->functions.pLast->cScope & ZH_FS_INITEXIT ) == ZH_FS_INITEXIT )
                  pFunc = pFunc->pOwner;
               pFunc->funFlags |= ZH_FUNF_USES_STATICS;
            }
            break;

         case ZH_VS_LOCAL_FIELD:
         case ZH_VS_GLOBAL_FIELD:
            /* declared field */
            zh_compGenFieldPCode( ZH_COMP_PARAM, ZH_P_POPFIELD, pVar );
            break;

         case ZH_VS_LOCAL_MEMVAR:
         case ZH_VS_GLOBAL_MEMVAR:
            /* declared memvar variable */
            zh_compGenVarPCode( ZH_P_POPMEMVAR, szVarName, ZH_COMP_PARAM );
            break;

         default:
            pVar = NULL;
            break;
      }
   }

   if( ! pVar ) /* undeclared variable */
   {
      zh_compGenVariablePCode( ZH_COMP_PARAM, ZH_P_POPVARIABLE, szVarName );
   }
}

/* generates the pcode to pop a value from the virtual machine stack onto a memvar variable */
void zh_compGenPopMemvar( const char * szVarName, ZH_COMP_DECL )
{
   if( ( zh_compVariableScope( ZH_COMP_PARAM, szVarName ) & ZH_VS_LOCAL_MEMVAR ) == 0 )
      zh_compGenWarning( ZH_COMP_PARAM, zh_comp_szWarnings, 'W', ZH_COMP_WARN_MEMVAR_ASSUMED, szVarName, NULL );
   zh_compGenVarPCode( ZH_P_POPMEMVAR, szVarName, ZH_COMP_PARAM );
}

/* generates the pcode to push a non-aliased variable value to the virtual
 * machine stack
 * bMacroVar is ZH_TRUE if macro &szVarName context
 */
void zh_compGenPushVar( const char * szVarName, ZH_COMP_DECL )
{
   int iVar, iScope;
   PZH_HVAR pVar;

   pVar = zh_compVariableFind( ZH_COMP_PARAM, szVarName, &iVar, &iScope );
   if( pVar )
   {
      if( ZH_COMP_PARAM->functions.pLast->iEarlyEvalPass == 1 )
         zh_compCheckEarlyMacroEval( ZH_COMP_PARAM, szVarName, iScope );

      switch( iScope )
      {
         case ZH_VS_LOCAL_VAR:
         case ZH_VS_CBLOCAL_VAR:
            /* local variable */
            /* local variables used in a codeblock will not be adjusted
             * if PARAMETERS statement will be used then it is safe to
             * use 2 bytes for LOCALNEAR
             */
            if( ZH_LIM_INT8( iVar ) && ! ZH_COMP_PARAM->functions.pLast->szName &&
                !( ZH_COMP_PARAM->functions.pLast->funFlags & ZH_FUNF_EXTBLOCK ) )
               zh_compGenPCode2( ZH_P_PUSHLOCALNEAR, ( ZH_BYTE ) iVar, ZH_COMP_PARAM );
            else
               zh_compGenPCode3( ZH_P_PUSHLOCAL, ZH_LOBYTE( iVar ), ZH_HIBYTE( iVar ), ZH_COMP_PARAM );
            break;

         case ZH_VS_STATIC_VAR:
         case ZH_VS_GLOBAL_STATIC:
            /* Static variable */
            zh_compGenPCode3( ZH_P_PUSHSTATIC, ZH_LOBYTE( iVar ), ZH_HIBYTE( iVar ), ZH_COMP_PARAM );
            ZH_COMP_PARAM->functions.pLast->funFlags |= ZH_FUNF_USES_STATICS;
            break;

         case ZH_VS_LOCAL_FIELD:
         case ZH_VS_GLOBAL_FIELD:
            /* declared field */
            zh_compGenFieldPCode( ZH_COMP_PARAM, ZH_P_PUSHFIELD, pVar );
            break;

         case ZH_VS_LOCAL_MEMVAR:
         case ZH_VS_GLOBAL_MEMVAR:
            /* declared memvar variable */
            zh_compGenVarPCode( ZH_P_PUSHMEMVAR, szVarName, ZH_COMP_PARAM );
            break;

         default:
            pVar = NULL;
            break;
      }
   }

   if( ! pVar ) /* undeclared variable */
   {
      zh_compGenVariablePCode( ZH_COMP_PARAM, ZH_P_PUSHVARIABLE, szVarName );
   }
}

void zh_compGenPushVarRef( const char * szVarName, ZH_COMP_DECL ) /* generates the pcode to push a variable by reference to the virtual machine stack */
{
   int iVar, iScope;
   PZH_HVAR pVar;

   pVar = zh_compVariableFind( ZH_COMP_PARAM, szVarName, &iVar, &iScope );
   if( pVar )
   {
      if( ZH_COMP_PARAM->functions.pLast->iEarlyEvalPass == 1 )
         zh_compCheckEarlyMacroEval( ZH_COMP_PARAM, szVarName, iScope );

      switch( iScope )
      {
         case ZH_VS_LOCAL_VAR:
         case ZH_VS_CBLOCAL_VAR:
            /* local variable */
            zh_compGenPCode3( ZH_P_PUSHLOCALREF, ZH_LOBYTE( iVar ), ZH_HIBYTE( iVar ), ZH_COMP_PARAM );
            break;

         case ZH_VS_STATIC_VAR:
         case ZH_VS_GLOBAL_STATIC:
            /* Static variable */
            zh_compGenPCode3( ZH_P_PUSHSTATICREF, ZH_LOBYTE( iVar ), ZH_HIBYTE( iVar ), ZH_COMP_PARAM );
            ZH_COMP_PARAM->functions.pLast->funFlags |= ZH_FUNF_USES_STATICS;
            break;

         case ZH_VS_LOCAL_FIELD:
         case ZH_VS_GLOBAL_FIELD:
            /* pushing fields by reference is not allowed */
            zh_compErrorRefer( ZH_COMP_PARAM, NULL, szVarName );
            break;

         case ZH_VS_LOCAL_MEMVAR:
         case ZH_VS_GLOBAL_MEMVAR:
            /* declared memvar variable */
            zh_compGenVarPCode( ZH_P_PUSHMEMVARREF, szVarName, ZH_COMP_PARAM );
            break;

         default:
            pVar = NULL;
      }
   }

   if( ! pVar )
   {
      /* undeclared variable */
      /* field cannot be passed by the reference - assume the memvar */
      zh_compGenVariablePCode( ZH_COMP_PARAM, ZH_P_PUSHMEMVARREF, szVarName );
   }
}

void zh_compGenPushMemvarRef( const char * szVarName, ZH_COMP_DECL ) /* generates the pcode to push memvar variable by reference to the virtual machine stack */
{
   zh_compGenVarPCode( ZH_P_PUSHMEMVARREF, szVarName, ZH_COMP_PARAM );
}

/* generates the pcode to pop a value from the virtual machine stack onto
 * an aliased variable
 */
void zh_compGenPopAliasedVar( const char * szVarName,
                              ZH_BOOL bPushAliasValue,
                              const char * szAlias,
                              ZH_MAXINT nWorkarea,
                              ZH_COMP_DECL )
{
   if( bPushAliasValue )
   {
      if( szAlias )
      {
         int iLen = ( int ) strlen( szAlias );
         if( szAlias[ 0 ] == 'M' && ( iLen == 1 ||
             ( iLen >= 4 && iLen <= 6 &&
               memcmp( szAlias, "MEMVAR", iLen ) == 0 ) ) )
         {
            /* M->variable or MEMV[A[R]]->variable */
            zh_compGenVarPCode( ZH_P_POPMEMVAR, szVarName, ZH_COMP_PARAM );
         }
         else if( iLen >= 4 && iLen <= 5 &&
                  memcmp( szAlias, "FIELD", iLen ) == 0 )
         {
            /* FIEL[D]->variable */
            zh_compGenVarPCode( ZH_P_POPFIELD, szVarName, ZH_COMP_PARAM );
         }
         else
         {
            /* database alias */
            zh_compGenPushSymbol( szAlias, ZH_SYM_ALIAS, ZH_COMP_PARAM );
            zh_compGenVarPCode( ZH_P_POPALIASEDFIELD, szVarName, ZH_COMP_PARAM );
         }
      }
      else
      {
         zh_compGenPushLong( nWorkarea, ZH_COMP_PARAM );
         zh_compGenVarPCode( ZH_P_POPALIASEDFIELD, szVarName, ZH_COMP_PARAM );
      }
   }
   else
      /* Alias is already placed on stack
       * NOTE: An alias will be determined at runtime then we cannot decide
       * here if passed name is either a field or a memvar
       */
      zh_compGenVarPCode( ZH_P_POPALIASEDVAR, szVarName, ZH_COMP_PARAM );
}

/* generates the pcode to push an aliased variable value to the virtual
 * machine stack
 */
void zh_compGenPushAliasedVar( const char * szVarName,
                               ZH_BOOL bPushAliasValue,
                               const char * szAlias,
                               ZH_MAXINT nWorkarea,
                               ZH_COMP_DECL )
{
   if( bPushAliasValue )
   {
      if( szAlias )
      {
         int iLen = ( int ) strlen( szAlias );
         /* myalias->var
          * FIELD->var
          * MEMVAR->var
          */
         if( szAlias[ 0 ] == 'M' && ( iLen == 1 ||
             ( iLen >= 4 && iLen <= 6 &&
               memcmp( szAlias, "MEMVAR", iLen ) == 0 ) ) )
         {
            /* M->variable or MEMV[A[R]]->variable */
            zh_compGenVarPCode( ZH_P_PUSHMEMVAR, szVarName, ZH_COMP_PARAM );
         }
         else if( iLen >= 4 && iLen <= 5 &&
                  memcmp( szAlias, "FIELD", iLen ) == 0 )
         {
            /* FIEL[D]->variable */
            zh_compGenVarPCode( ZH_P_PUSHFIELD, szVarName, ZH_COMP_PARAM );
         }
         else
         {
            /* database alias */
            zh_compGenPushSymbol( szAlias, ZH_SYM_ALIAS, ZH_COMP_PARAM );
            zh_compGenVarPCode( ZH_P_PUSH_ALIASED_FIELD, szVarName, ZH_COMP_PARAM );
         }
      }
      else
      {
         zh_compGenPushLong( nWorkarea, ZH_COMP_PARAM );
         zh_compGenVarPCode( ZH_P_PUSH_ALIASED_FIELD, szVarName, ZH_COMP_PARAM );
      }
   }
   else
      /* Alias is already placed on stack
       * NOTE: An alias will be determined at runtime then we cannot decide
       * here if passed name is either a field or a memvar
       */
      zh_compGenVarPCode( ZH_P_PUSHALIASEDVAR, szVarName, ZH_COMP_PARAM );
}


void zh_compGenPushLogical( int iTrueFalse, ZH_COMP_DECL ) /* pushes a logical value on the virtual machine stack */
{
   zh_compGenPCode1( ( ZH_BYTE ) ( iTrueFalse ? ZH_P_TRUE : ZH_P_FALSE ), ZH_COMP_PARAM );
}

void zh_compGenPushNil( ZH_COMP_DECL )
{
   zh_compGenPCode1( ZH_P_PUSHNIL, ZH_COMP_PARAM );
}

/* generates the pcode to push a double number on the virtual machine stack */
void zh_compGenPushDouble( double dNumber, ZH_BYTE bWidth, ZH_BYTE bDec, ZH_COMP_DECL )
{
   ZH_BYTE pBuffer[ sizeof( double ) + sizeof( ZH_BYTE ) + sizeof( ZH_BYTE ) + 1 ];

   pBuffer[ 0 ] = ZH_P_PUSHDOUBLE;
   ZH_PUT_LE_DOUBLE( &( pBuffer[ 1 ] ), dNumber );

   pBuffer[ 1 + sizeof( double ) ] = bWidth;
   pBuffer[ 1 + sizeof( double ) + sizeof( ZH_BYTE ) ] = bDec;

   zh_compGenPCodeN( pBuffer, sizeof( pBuffer ), ZH_COMP_PARAM );
}

void zh_compGenPushFunCall( const char * szFunName, int iFlags, ZH_COMP_DECL )
{
   PZH_HSYMBOL pSym;
   ZH_USHORT wSym;

   ZH_SYMBOL_UNUSED( iFlags );

   pSym = zh_compSymbolFind( ZH_COMP_PARAM, szFunName, &wSym, ZH_SYM_FUNCNAME );
   if( ! pSym )
      pSym = zh_compSymbolAdd( ZH_COMP_PARAM, szFunName, &wSym, ZH_SYM_FUNCNAME );

   pSym->cScope |= ZH_FS_USED;

   zh_compGenPCode3( ZH_P_PUSHFUNCSYM, ZH_LOBYTE( wSym ), ZH_HIBYTE( wSym ), ZH_COMP_PARAM );
}

void zh_compGenPushFunSym( const char * szFunName, int iFlags, ZH_COMP_DECL )
{
   ZH_SYMBOL_UNUSED( iFlags );
   zh_compGenPushSymbol( szFunName, ZH_SYM_FUNCNAME, ZH_COMP_PARAM );
}

void zh_compGenPushFunRef( const char * szFunName, ZH_COMP_DECL )
{
   zh_compGenPushSymbol( szFunName, ZH_SYM_FUNCNAME, ZH_COMP_PARAM );
}

/* generates the pcode to push a symbol on the virtual machine stack */
void zh_compGenPushSymbol( const char * szSymbolName, ZH_BOOL bFunction, ZH_COMP_DECL )
{
   PZH_HSYMBOL pSym;
   ZH_USHORT wSym;

   pSym = zh_compSymbolFind( ZH_COMP_PARAM, szSymbolName, &wSym, bFunction );
   if( ! pSym )
      pSym = zh_compSymbolAdd( ZH_COMP_PARAM, szSymbolName, &wSym, bFunction );

   if( bFunction )
      pSym->cScope |= ZH_FS_USED;

   if( wSym > 255 )
      zh_compGenPCode3( ZH_P_PUSHSYM, ZH_LOBYTE( wSym ), ZH_HIBYTE( wSym ), ZH_COMP_PARAM );
   else
      zh_compGenPCode2( ZH_P_PUSHSYMNEAR, ( ZH_BYTE ) wSym, ZH_COMP_PARAM );
}

/* generates the pcode to push a long number on the virtual machine stack */
void zh_compGenPushLong( ZH_MAXINT nNumber, ZH_COMP_DECL )
{
   if( ! ZH_COMP_PARAM->fSwitchCase )
   {
      if( nNumber == 0 )
         zh_compGenPCode1( ZH_P_ZERO, ZH_COMP_PARAM );
      else if( nNumber == 1 )
         zh_compGenPCode1( ZH_P_ONE, ZH_COMP_PARAM );
      else if( ZH_LIM_INT8( nNumber ) )
         zh_compGenPCode2( ZH_P_PUSHBYTE, ( ZH_BYTE ) nNumber, ZH_COMP_PARAM );
      else if( ZH_LIM_INT16( nNumber ) )
         zh_compGenPCode3( ZH_P_PUSHINT, ZH_LOBYTE( nNumber ), ZH_HIBYTE( nNumber ), ZH_COMP_PARAM );
      else if( ZH_LIM_INT32( nNumber ) )
      {
         ZH_BYTE pBuffer[ 5 ];
         pBuffer[ 0 ] = ZH_P_PUSHLONG;
         ZH_PUT_LE_UINT32( pBuffer + 1, nNumber );
         zh_compGenPCodeN( pBuffer, sizeof( pBuffer ), ZH_COMP_PARAM );
      }
      else
      {
         ZH_BYTE pBuffer[ 9 ];
         pBuffer[ 0 ] = ZH_P_PUSHLONGLONG;
         ZH_PUT_LE_UINT64( pBuffer + 1, nNumber );
         zh_compGenPCodeN( pBuffer, sizeof( pBuffer ), ZH_COMP_PARAM );
      }
   }
   else
   {
      if( ZH_LIM_INT32( nNumber ) )
      {
         ZH_BYTE pBuffer[ 5 ];
         pBuffer[ 0 ] = ZH_P_PUSHLONG;
         ZH_PUT_LE_UINT32( pBuffer + 1, nNumber );
         zh_compGenPCodeN( pBuffer, sizeof( pBuffer ), ZH_COMP_PARAM );
      }
      else
      {
         ZH_BYTE pBuffer[ 9 ];
         pBuffer[ 0 ] = ZH_P_PUSHLONGLONG;
         ZH_PUT_LE_UINT64( pBuffer + 1, nNumber );
         zh_compGenPCodeN( pBuffer, sizeof( pBuffer ), ZH_COMP_PARAM );
      }
   }
}

void zh_compGenPushDate( long lDate, ZH_COMP_DECL )
{
   ZH_BYTE pBuffer[ 5 ];

   pBuffer[ 0 ] = ZH_P_PUSH_DATE;
   ZH_PUT_LE_UINT32( pBuffer + 1, lDate );
   zh_compGenPCodeN( pBuffer, sizeof( pBuffer ), ZH_COMP_PARAM );
}

void zh_compGenPushTimeStamp( long lDate, long lTime, ZH_COMP_DECL )
{
   ZH_BYTE pBuffer[ 9 ];

   pBuffer[ 0 ] = ZH_P_PUSHTIMESTAMP;
   ZH_PUT_LE_UINT32( pBuffer + 1, lDate );
   ZH_PUT_LE_UINT32( pBuffer + 5, lTime );
   zh_compGenPCodeN( pBuffer, sizeof( pBuffer ), ZH_COMP_PARAM );
}

/* generates the pcode to push a string on the virtual machine stack */
void zh_compGenPushString( const char * szText, ZH_SIZE nStrLen, ZH_COMP_DECL )
{
   if( ZH_COMP_PARAM->iHidden && ! ZH_COMP_PARAM->fSwitchCase )
   {
      char * szTemp;
      --nStrLen;
      szTemp = zh_compEncodeString( ZH_COMP_PARAM->iHidden, szText, &nStrLen );
      if( nStrLen > UINT16_MAX )
         zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_STRING_TOO_LONG, NULL, NULL );
      else
      {
         zh_compGenPCode4( ZH_P_PUSH_STR_HIDDEN, ( ZH_BYTE ) ZH_COMP_PARAM->iHidden,
                           ZH_LOBYTE( nStrLen ), ZH_HIBYTE( nStrLen ), ZH_COMP_PARAM );
         zh_compGenPCodeN( ( ZH_BYTE * ) szTemp, nStrLen, ZH_COMP_PARAM );
      }
      zh_xfree( szTemp );
   }
   else
   {
      if( nStrLen > UINT24_MAX )
         zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_STRING_TOO_LONG, NULL, NULL );
      else
      {
         if( nStrLen > USHRT_MAX )
            zh_compGenPCode4( ZH_P_PUSHSTRLARGE, ZH_LOBYTE( nStrLen ), ZH_HIBYTE( nStrLen ), ZH_ULBYTE( nStrLen ), ZH_COMP_PARAM );
         else if( nStrLen > UCHAR_MAX )
            zh_compGenPCode3( ZH_P_PUSHSTR, ZH_LOBYTE( nStrLen ), ZH_HIBYTE( nStrLen ), ZH_COMP_PARAM );
         else
            zh_compGenPCode2( ZH_P_PUSHSTRSHORT, ( ZH_BYTE ) nStrLen, ZH_COMP_PARAM );
         zh_compGenPCodeN( ( const ZH_BYTE * ) szText, nStrLen, ZH_COMP_PARAM );
      }
   }
}

void zh_compNOOPfill( PZH_ZFUNC pFunc, ZH_SIZE nFrom, ZH_ISIZ nCount, ZH_BOOL fPop, ZH_BOOL fCheck )
{
   while( nCount-- )
   {
      if( fPop )
      {
         pFunc->pCode[ nFrom ] = ZH_P_POP;
         fPop = ZH_FALSE;
      }
      else if( fCheck && pFunc->pCode[ nFrom ] == ZH_P_NOOP && pFunc->nNOOPs )
      {
         ZH_SIZE n;

         for( n = 0; n < pFunc->nNOOPs; ++n )
         {
            if( pFunc->pNOOPs[ n ] == nFrom )
               break;
         }
         if( n == pFunc->nNOOPs )
            zh_compNOOPadd( pFunc, nFrom );
      }
      else
         zh_compNOOPadd( pFunc, nFrom );
      ++nFrom;
   }
}

/*
 * Warning - when jump optimization is disabled this function can be used
 * _ONLY_ in very limited situations when there is no jumps over the
 * removed block
 */
static void zh_compRemovePCODE( ZH_COMP_DECL, ZH_SIZE nPos, ZH_SIZE nCount,
                                ZH_BOOL fCanMove )
{
   PZH_ZFUNC pFunc = ZH_COMP_PARAM->functions.pLast;

   if( ZH_COMP_ISSUPPORTED( ZH_COMPFLAG_OPTJUMP ) || ! fCanMove )
   {
      /*
       * We can safely remove the dead code when Jump Optimization
       * is enabled by replacing it with ZH_P_NOOP opcodes - which
       * will be later eliminated and jump data updated.
       */
      zh_compNOOPfill( pFunc, nPos, nCount, ZH_FALSE, ZH_TRUE );
   }
   else
   {
      ZH_SIZE n;

      memmove( pFunc->pCode + nPos, pFunc->pCode + nPos + nCount,
               pFunc->nPCodePos - nPos - nCount );
      pFunc->nPCodePos -= nCount;

      for( n = pFunc->nNOOPs; n; --n )
      {
         if( pFunc->pNOOPs[ n ] >= nPos )
         {
            if( pFunc->pNOOPs[ n ] < nPos + nCount )
            {
               memmove( &pFunc->pNOOPs[ n ], &pFunc->pNOOPs[ n + 1 ],
                        pFunc->nNOOPs - n );
               pFunc->nNOOPs--;
            }
            else
            {
               pFunc->pNOOPs[ n ] -= nCount;
            }
         }
      }
   }
}

ZH_BOOL zh_compHasJump( PZH_ZFUNC pFunc, ZH_SIZE nPos )
{
   ZH_SIZE nJump;

   for( nJump = 0; nJump < pFunc->nJumps; nJump++ )
   {
      ZH_SIZE nJumpAddr = pFunc->pJumps[ nJump ];
      switch( pFunc->pCode[ nJumpAddr ] )
      {
         case ZH_P_JUMPNEAR:
         case ZH_P_JUMPFALSENEAR:
         case ZH_P_JUMPTRUENEAR:
            nJumpAddr += ( signed char ) pFunc->pCode[ nJumpAddr + 1 ];
            break;

         case ZH_P_JUMP:
         case ZH_P_JUMPFALSE:
         case ZH_P_JUMPTRUE:
            nJumpAddr += ZH_PCODE_MKSHORT( &pFunc->pCode[ nJumpAddr + 1 ] );
            break;

         /* Jump can be replaced by series of NOOPs or POP and NOOPs
          * and not stripped yet
          */
         case ZH_P_NOOP:
         case ZH_P_POP:
            nJumpAddr = nPos + 1;
            break;

         default:
            nJumpAddr += ZH_PCODE_MKINT24( &pFunc->pCode[ nJumpAddr + 1 ] );
            break;
      }
      if( nJumpAddr == nPos )
         return ZH_TRUE;
   }

   return ZH_FALSE;
}

/* Generate the opcode to open BEGIN/END sequence
 * This code is similar to JUMP opcode - the offset will be filled with
 * - either the address of ZH_P_SEQEND opcode if there is no RECOVER clause
 * - or the address of RECOVER code
 */
ZH_SIZE zh_compSequenceBegin( ZH_COMP_DECL )
{
   zh_compGenPCode4( ZH_P_SEQALWAYS, 0, 0, 0, ZH_COMP_PARAM );
   zh_compPrepareJumps( ZH_COMP_PARAM );

   zh_compGenPCode4( ZH_P_SEQBEGIN, 0, 0, 0, ZH_COMP_PARAM );
   zh_compPrepareJumps( ZH_COMP_PARAM );

   return ZH_COMP_PARAM->functions.pLast->nPCodePos - 3;
}

/* Generate the opcode to close BEGIN/END sequence
 * This code is similar to JUMP opcode - the offset will be filled with
 * the address of first line after END SEQUENCE
 * This opcode will be executed if recover code was not requested (as the
 * last statement in code between BEGIN ... RECOVER) or if BREAK was requested
 * and there was no matching RECOVER clause.
 */
ZH_SIZE zh_compSequenceEnd( ZH_COMP_DECL )
{
   zh_compGenPCode4( ZH_P_SEQEND, 0, 0, 0, ZH_COMP_PARAM );

   zh_compPrepareJumps( ZH_COMP_PARAM );

   return ZH_COMP_PARAM->functions.pLast->nPCodePos - 3;
}

ZH_SIZE zh_compSequenceAlways( ZH_COMP_DECL )
{
   zh_compGenPCode4( ZH_P_ALWAYSBEGIN, 0, 0, 0, ZH_COMP_PARAM );

   zh_compPrepareJumps( ZH_COMP_PARAM );

   return ZH_COMP_PARAM->functions.pLast->nPCodePos - 3;
}

/* Remove unnecessary opcodes in case there were no executable statements
 * between BEGIN and RECOVER sequence
 */
void zh_compSequenceFinish( ZH_COMP_DECL, ZH_SIZE nStartPos, ZH_SIZE nEndPos,
                            ZH_SIZE nAlways, ZH_BOOL fUsualStmts, ZH_BOOL fRecover,
                            ZH_BOOL fCanMove )
{
   --nStartPos;  /* ZH_P_SEQBEGIN address */

   if( ! fUsualStmts && fCanMove && ! ZH_COMP_PARAM->fDebugInfo )
   {
      nStartPos -= 4;
      if( nAlways )
      {
         /* remove ZH_P_ALWAYSEND opcode */
         ZH_COMP_PARAM->functions.pLast->nPCodePos--;
         /* remove ZH_P_SEQALWAYS ... ZH_P_ALWAYSBEGIN opcodes */
         zh_compRemovePCODE( ZH_COMP_PARAM, nStartPos,
                             nAlways - nStartPos + 4, fCanMove );
      }
      else
      {
         zh_compRemovePCODE( ZH_COMP_PARAM, nStartPos,
                             ZH_COMP_PARAM->functions.pLast->nPCodePos -
                             nStartPos, fCanMove );
      }
   }
   else if( ! nAlways )
   {
      /* remove ZH_P_SEQALWAYS opcode */
      zh_compRemovePCODE( ZH_COMP_PARAM, nStartPos - 4, 4, fCanMove );
   }
   else
   {
      if( ! fRecover )
      {
         /* remove ZH_P_SEQBEGIN and ZH_P_SEQEND */
         zh_compRemovePCODE( ZH_COMP_PARAM, nEndPos - 1, 4, fCanMove );
         zh_compRemovePCODE( ZH_COMP_PARAM, nStartPos, 4, fCanMove );
         if( ! ZH_COMP_ISSUPPORTED( ZH_COMPFLAG_OPTJUMP ) )
         {
            /* Fix ALWAYS address in ZH_P_SEQALWAYS opcode */
            nAlways -= 8;
            zh_compGenJumpThere( nStartPos - 3, nAlways, ZH_COMP_PARAM );
         }
      }
      /* empty always block? */
      if( ZH_COMP_PARAM->functions.pLast->nPCodePos - nAlways == 5 &&
          ! ZH_COMP_PARAM->fDebugInfo )
      {
         /* remove ZH_P_ALWAYSBEGIN and ZH_P_ALWAYSEND opcodes */
         zh_compRemovePCODE( ZH_COMP_PARAM, nAlways, 5, ZH_TRUE );
         /* remove ZH_P_SEQALWAYS opcode */
         zh_compRemovePCODE( ZH_COMP_PARAM, nStartPos - 4, 4, fCanMove );
      }
   }
}

/*
 * Start of definition of static variable
 * We are using here the special function ZH_COMP_PARAM->pInitFunc which will store
 * pcode needed to initialize all static variables declared in PRG module.
 * pOwner member will point to a function where the static variable is
 * declared:
 */
void zh_compStaticDefStart( ZH_COMP_DECL )
{
   ZH_COMP_PARAM->functions.pLast->funFlags |= ZH_FUNF_USES_STATICS;
   if( ! ZH_COMP_PARAM->pInitFunc )
   {
      ZH_BYTE pBuffer[ 5 ];

      ZH_COMP_PARAM->pInitFunc = zh_compFunctionNew( ZH_COMP_PARAM, "(_INITSTATICS)", ZH_FS_INITEXIT | ZH_FS_LOCAL );
      ZH_COMP_PARAM->pInitFunc->pOwner = ZH_COMP_PARAM->functions.pLast;
      ZH_COMP_PARAM->pInitFunc->funFlags = ZH_FUNF_USES_STATICS | ZH_FUNF_PROCEDURE;
      ZH_COMP_PARAM->functions.pLast = ZH_COMP_PARAM->pInitFunc;

      pBuffer[ 0 ] = ZH_P_STATICS;
      pBuffer[ 1 ] = 0;
      pBuffer[ 2 ] = 0;
      pBuffer[ 3 ] = 1; /* the number of static variables is unknown now */
      pBuffer[ 4 ] = 0;

      zh_compGenPCodeN( pBuffer, 5, ZH_COMP_PARAM );

      zh_compGenPCode3( ZH_P_SFRAME, 0, 0, ZH_COMP_PARAM );     /* frame for statics variables */

      if( ZH_COMP_PARAM->fDebugInfo )
      {
         /* uncomment this if you want to always set main module name
            not the one where first static variable was declared */
         #if 0
         ZH_COMP_PARAM->currModule = ZH_COMP_PARAM->szFile;
         #endif
         zh_compGenModuleName( ZH_COMP_PARAM, ZH_COMP_PARAM->pInitFunc->szName );
      }
   }
   else
   {
      ZH_COMP_PARAM->pInitFunc->pOwner = ZH_COMP_PARAM->functions.pLast;
      ZH_COMP_PARAM->functions.pLast = ZH_COMP_PARAM->pInitFunc;
   }
}

/*
 * End of definition of static variable
 * Return to previously pcoded function.
 */
void zh_compStaticDefEnd( ZH_COMP_DECL, const char * szVarName )
{
   ZH_COMP_PARAM->functions.pLast = ZH_COMP_PARAM->pInitFunc->pOwner;
   ZH_COMP_PARAM->pInitFunc->pOwner = NULL;
   if( ZH_COMP_PARAM->fDebugInfo )
   {
      ZH_BYTE bGlobal = 0;
      int iVar;

      if( ( ZH_COMP_PARAM->functions.pLast->funFlags & ZH_FUNF_FILE_DECL ) != 0 )
      {
         /* Variable declaration is outside of function/procedure body.
          * File-wide static variable
          */
         ZH_COMP_PARAM->pInitFunc->pOwner = ZH_COMP_PARAM->functions.pLast;
         ZH_COMP_PARAM->functions.pLast = ZH_COMP_PARAM->pInitFunc;
         bGlobal = 1;
      }

      iVar = ZH_COMP_PARAM->iStaticCnt;
      zh_compGenPCode4( ZH_P_STATICNAME, bGlobal, ZH_LOBYTE( iVar ), ZH_HIBYTE( iVar ), ZH_COMP_PARAM );
      zh_compGenPCodeN( ( const ZH_BYTE * ) szVarName, strlen( szVarName ) + 1, ZH_COMP_PARAM );
      if( bGlobal )
      {
         ZH_COMP_PARAM->functions.pLast = ZH_COMP_PARAM->pInitFunc->pOwner;
         ZH_COMP_PARAM->pInitFunc->pOwner = NULL;
      }
   }
}

/*
 * Mark thread static variables
 */
static void zh_compStaticDefThreadSet( ZH_COMP_DECL )
{
   if( ZH_COMP_PARAM->pInitFunc )
   {
      ZH_USHORT uiCount = 0;
      PZH_ZFUNC pFunc;
      PZH_HVAR pVar;

      pFunc = ZH_COMP_PARAM->functions.pFirst;
      while( pFunc )
      {
         pVar = pFunc->pStatics;
         while( pVar )
         {
            if( pVar->uiFlags & ZH_VSCOMP_THREAD )
               ++uiCount;
            pVar = pVar->pNext;
         }
         pFunc = pFunc->pNext;
      }

      if( uiCount )
      {
         ZH_SIZE nSize = ( ( ZH_SIZE ) uiCount << 1 ) + 3;
         ZH_BYTE * pBuffer = ( ZH_BYTE * ) zh_xgrab( nSize ), *ptr;
         ZH_USHORT uiVar = 0;
         pBuffer[ 0 ] = ZH_P_THREADSTATICS;
         pBuffer[ 1 ] = ZH_LOBYTE( uiCount );
         pBuffer[ 2 ] = ZH_HIBYTE( uiCount );
         ptr = pBuffer + 3;
         pFunc = ZH_COMP_PARAM->functions.pFirst;
         while( pFunc && uiCount )
         {
            pVar = pFunc->pStatics;
            while( pVar && uiCount )
            {
               ++uiVar;
               if( pVar->uiFlags & ZH_VSCOMP_THREAD )
               {
                  ZH_PUT_LE_UINT16( ptr, uiVar );
                  ptr += 2;
                  --uiCount;
               }
               pVar = pVar->pNext;
            }
            pFunc = pFunc->pNext;
         }

         ZH_COMP_PARAM->pInitFunc->pOwner = ZH_COMP_PARAM->functions.pLast;
         ZH_COMP_PARAM->functions.pLast = ZH_COMP_PARAM->pInitFunc;

         zh_compGenPCodeN( pBuffer, nSize, ZH_COMP_PARAM );

         ZH_COMP_PARAM->functions.pLast = ZH_COMP_PARAM->pInitFunc->pOwner;
         ZH_COMP_PARAM->pInitFunc->pOwner = NULL;

         zh_xfree( pBuffer );
      }
   }
}

/*
 * Start of stop line number info generation
 */
static void zh_compLineNumberDefStart( ZH_COMP_DECL )
{
   if( ! ZH_COMP_PARAM->pLineFunc )
   {
      ZH_COMP_PARAM->pLineFunc = zh_compFunctionNew( ZH_COMP_PARAM, "(_INITLINES)", ZH_FS_INITEXIT | ZH_FS_LOCAL );
      ZH_COMP_PARAM->pLineFunc->pOwner = ZH_COMP_PARAM->functions.pLast;
      ZH_COMP_PARAM->pLineFunc->funFlags = 0;
      ZH_COMP_PARAM->functions.pLast = ZH_COMP_PARAM->pLineFunc;

      if( ZH_COMP_PARAM->fDebugInfo )
      {
         /* set main module name */
         ZH_COMP_PARAM->currModule = ZH_COMP_PARAM->szFile;
         zh_compGenModuleName( ZH_COMP_PARAM, ZH_COMP_PARAM->pLineFunc->szName );
      }
   }
   else
   {
      ZH_COMP_PARAM->pLineFunc->pOwner = ZH_COMP_PARAM->functions.pLast;
      ZH_COMP_PARAM->functions.pLast = ZH_COMP_PARAM->pLineFunc;
   }
}

/*
 * End of stop line number info generation
 * Return to previously pcoded function.
 */
static void zh_compLineNumberDefEnd( ZH_COMP_DECL )
{
   ZH_COMP_PARAM->functions.pLast = ZH_COMP_PARAM->pLineFunc->pOwner;
   ZH_COMP_PARAM->pLineFunc->pOwner = NULL;
}

/*
 * Start a new fake-function that will hold pcodes for a codeblock
 */
void zh_compCodeBlockStart( ZH_COMP_DECL, int iEarlyEvalPass )
{
   PZH_ZFUNC pBlock;

   pBlock                  = zh_compFunctionNew( ZH_COMP_PARAM, NULL, ZH_FS_STATIC | ZH_FS_LOCAL );
   pBlock->pOwner          = ZH_COMP_PARAM->functions.pLast;
   pBlock->iEarlyEvalPass  = iEarlyEvalPass;

   ZH_COMP_PARAM->functions.pLast = pBlock;
}

void zh_compCodeBlockEnd( ZH_COMP_DECL )
{
   PZH_ZFUNC pCodeblock;   /* pointer to the current codeblock */
   PZH_ZFUNC pFunc;        /* pointer to a function that owns a codeblock */
   const char * pFuncName;
   ZH_SIZE nSize;
   ZH_USHORT wLocals = 0;  /* number of referenced local variables */
   ZH_USHORT wLocalsCnt, wLocalsLen;
   PZH_HVAR pVar;

   pCodeblock = ZH_COMP_PARAM->functions.pLast;

   /* Check if the extended codeblock has return statement */
   if( ( pCodeblock->funFlags & ZH_FUNF_EXTBLOCK ) )
   {
      if( !( pCodeblock->funFlags & ZH_FUNF_WITH_RETURN ) )
      {
         if( ZH_COMP_PARAM->iWarnings >= 1 )
            zh_compGenWarning( ZH_COMP_PARAM, zh_comp_szWarnings, 'W', ZH_COMP_WARN_FUN_WITH_NO_RETURN,
                               "{||...}", NULL );
         /* finish the codeblock without popping the return value from ZHVM stack */
         zh_compGenPCode1( ZH_P_ENDPROC, ZH_COMP_PARAM );
      }
   }

   zh_compGenPCode1( ZH_P_ENDBLOCK, ZH_COMP_PARAM ); /* finish the codeblock */

   if( ! pCodeblock->bError )
   {
      if( pCodeblock->wParamCount && !( pCodeblock->funFlags & ZH_FUNF_USES_LOCAL_PARAMS ) )
         /* PARAMETERs were used after LOCALs in extended codeblock
          * fix generated local indexes
          */
         zh_compFixFuncPCode( ZH_COMP_PARAM, pCodeblock );
      zh_compOptimizeJumps( ZH_COMP_PARAM );
   }

   /* return to pcode buffer of function/codeblock in which the current
    * codeblock was defined
    */
   ZH_COMP_PARAM->functions.pLast = pCodeblock->pOwner;

   /* find the function that owns the codeblock */
   pFunc = pCodeblock->pOwner;
   pFuncName = pFunc->szName;
   while( pFunc->pOwner )
   {
      pFunc = pFunc->pOwner;
      if( pFunc->szName && *pFunc->szName )
         pFuncName = pFunc->szName;
   }
   if( *pFuncName == 0 )
      pFuncName = "(_INITSTATICS)";
   pFunc->funFlags |= ( pCodeblock->funFlags & ZH_FUNF_USES_STATICS );

   /* generate a proper codeblock frame with a codeblock size and with
    * a number of expected parameters
    */

   /* Count the number of referenced local variables */
   wLocalsLen = 0;
   pVar = pCodeblock->pDetached;
   while( pVar )
   {
      if( ZH_COMP_PARAM->fDebugInfo )
         wLocalsLen += 4 + ( ZH_USHORT ) strlen( pVar->szName );
      pVar = pVar->pNext;
      ++wLocals;
   }
   wLocalsCnt = wLocals;

   nSize = pCodeblock->nPCodePos + 2;
   if( ZH_COMP_PARAM->fDebugInfo )
   {
      nSize += 3 + strlen( ZH_COMP_PARAM->currModule ) + strlen( pFuncName );
      nSize += wLocalsLen;
   }

   if( nSize <= 255 && pCodeblock->wParamCount == 0 && wLocals == 0 )
   {
      /* NOTE: 2 = ZH_P_PUSH_BLOCK + ZH_BYTE( size ) */
      zh_compGenPCode2( ZH_P_PUSH_BLOCKSHORT, ( ZH_BYTE ) nSize, ZH_COMP_PARAM );
   }
   else
   {
      /* NOTE: 8 = ZH_P_PUSH_BLOCK + ZH_USHORT( size ) + ZH_USHORT( wParams ) + ZH_USHORT( wLocals ) + _ENDBLOCK */
      nSize += 5 + wLocals * 2;
      if( nSize <= USHRT_MAX )
         zh_compGenPCode3( ZH_P_PUSH_BLOCK, ZH_LOBYTE( nSize ), ZH_HIBYTE( nSize ), ZH_COMP_PARAM );
      else if( nSize < UINT24_MAX )
      {
         ++nSize;
         zh_compGenPCode4( ZH_P_PUSH_BLOCKLARGE, ZH_LOBYTE( nSize ), ZH_HIBYTE( nSize ), ZH_ULBYTE( nSize ), ZH_COMP_PARAM );
      }
      else
         zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_BLOCK_TOO_BIG, NULL, NULL );

      /* generate the number of local parameters */
      zh_compGenPCode2( ZH_LOBYTE( pCodeblock->wParamCount ), ZH_HIBYTE( pCodeblock->wParamCount ), ZH_COMP_PARAM );
      /* generate the number of referenced local variables */
      zh_compGenPCode2( ZH_LOBYTE( wLocals ), ZH_HIBYTE( wLocals ), ZH_COMP_PARAM );

      /* generate the table of referenced local variables */
      pVar = pCodeblock->pDetached;
      while( wLocals-- )
      {
         ZH_USHORT wPos = zh_compVariableGetPos( pFunc->pLocals, pVar->szName );
         zh_compGenPCode2( ZH_LOBYTE( wPos ), ZH_HIBYTE( wPos ), ZH_COMP_PARAM );
         pVar = pVar->pNext;
      }
   }

   if( ZH_COMP_PARAM->fDebugInfo )
   {
      int iLocalPos;

      zh_compGenModuleName( ZH_COMP_PARAM, pFuncName );

      /* generate the name of referenced local variables */
      pVar = pCodeblock->pDetached;
      iLocalPos = -1;
      while( wLocalsCnt-- )
      {
         zh_compGenPCode3( ZH_P_LOCALNAME, ZH_LOBYTE( iLocalPos ), ZH_HIBYTE( iLocalPos ), ZH_COMP_PARAM );
         zh_compGenPCodeN( ( const ZH_BYTE * ) pVar->szName, strlen( pVar->szName ) + 1, ZH_COMP_PARAM );
         iLocalPos--;
         pVar = pVar->pNext;
      }

   }

   zh_compGenPCodeN( pCodeblock->pCode, pCodeblock->nPCodePos, ZH_COMP_PARAM );

   if( ZH_COMP_PARAM->iWarnings )
   {
      pVar = pCodeblock->pLocals;
      while( pVar )
      {
         if( ZH_COMP_PARAM->iWarnings && pVar->szName && ! ( pVar->iUsed & ZH_VU_USED ) )
            zh_compGenWarning( ZH_COMP_PARAM, zh_comp_szWarnings, 'W', ZH_COMP_WARN_BLOCKVAR_NOT_USED, pVar->szName, pFuncName );
         pVar = pVar->pNext;
      }
      pVar = pCodeblock->pStatics;
      while( pVar )
      {
         if( ZH_COMP_PARAM->iWarnings && pVar->szName && ! ( pVar->iUsed & ZH_VU_USED ) )
            zh_compWarnUnusedVar( ZH_COMP_PARAM, "{||...}", pVar->szName, pVar->iDeclLine );
         pVar = pVar->pNext;
      }
   }

   /* move static variables to owner */
   pVar = pFunc->pStatics;
   if( pVar )
   {
      while( pVar->pNext )
         pVar = pVar->pNext;
      pVar->pNext = pCodeblock->pStatics;
   }
   else
      pFunc->pStatics = pCodeblock->pStatics;
   pVar = pCodeblock->pStatics;
   pCodeblock->pStatics = NULL;
   /* change static variables names to avoid conflicts */
   while( pVar )
   {
      char szName[ ZH_SYMBOL_NAME_LEN + 4 ];
      zh_snprintf( szName, sizeof( szName ), "%s(b)", pVar->szName );
      pVar->szName = zh_compIdentifierNew( ZH_COMP_PARAM, szName, ZH_IDENT_COPY );
      pVar = pVar->pNext;
   }

   zh_compFunctionKill( ZH_COMP_PARAM, pCodeblock );
}

void zh_compCodeBlockStop( ZH_COMP_DECL )
{
   PZH_ZFUNC pCodeblock;   /* pointer to the current codeblock */

   pCodeblock = ZH_COMP_PARAM->functions.pLast;

   /* return to pcode buffer of function/codeblock in which the current
    * codeblock was defined
    */
   ZH_COMP_PARAM->functions.pLast = pCodeblock->pOwner;
   zh_compGenPCodeN( pCodeblock->pCode, pCodeblock->nPCodePos, ZH_COMP_PARAM );

   if( ZH_COMP_PARAM->iWarnings )
   {
      PZH_HVAR pVar = pCodeblock->pLocals;
      /* find the function that owns the codeblock */
      PZH_ZFUNC pFunc = pCodeblock->pOwner;
      while( pFunc->pOwner )
         pFunc = pFunc->pOwner;
      while( pVar )
      {
         if( pFunc->szName && pVar->szName && ! ( pVar->iUsed & ZH_VU_USED ) )
            zh_compGenWarning( ZH_COMP_PARAM, zh_comp_szWarnings, 'W', ZH_COMP_WARN_BLOCKVAR_NOT_USED, pVar->szName, pFunc->szName );
         pVar = pVar->pNext;
      }
   }

   zh_compFunctionKill( ZH_COMP_PARAM, pCodeblock );
}

void zh_compCodeBlockRewind( ZH_COMP_DECL )
{
   PZH_ZFUNC pCodeblock;   /* pointer to the current codeblock */

   pCodeblock = ZH_COMP_PARAM->functions.pLast;
   pCodeblock->nPCodePos = 0;

   /* Release the NOOP array. */
   if( pCodeblock->pNOOPs )
   {
      zh_xfree( pCodeblock->pNOOPs );
      pCodeblock->pNOOPs = NULL;
      pCodeblock->nNOOPs = 0;
   }
   /* Release the Jumps array. */
   if( pCodeblock->pJumps )
   {
      zh_xfree( pCodeblock->pJumps );
      pCodeblock->pJumps = NULL;
      pCodeblock->nJumps = 0;
   }
   while( pCodeblock->pDetached )
   {
      PZH_HVAR pVar = pCodeblock->pDetached;
      pCodeblock->pDetached = pVar->pNext;
      zh_xfree( pVar );
   }
   while( pCodeblock->pLocals )
   {
      PZH_HVAR pVar = pCodeblock->pLocals;
      pCodeblock->pLocals = pVar->pNext;
      zh_xfree( pVar );
   }
}

/* ************************************************************************* */

/* initialize support variables */
static void zh_compInitVars( ZH_COMP_DECL )
{
   ZH_COMP_PARAM->functions.iCount = 0;
   ZH_COMP_PARAM->functions.pFirst = NULL;
   ZH_COMP_PARAM->functions.pLast  = NULL;
   ZH_COMP_PARAM->szAnnounce       = NULL;
   ZH_COMP_PARAM->fSwitchCase      = ZH_FALSE;

   ZH_COMP_PARAM->symbols.iCount   = 0;
   ZH_COMP_PARAM->symbols.pFirst   = NULL;
   ZH_COMP_PARAM->symbols.pLast    = NULL;
   ZH_COMP_PARAM->pInitFunc        = NULL;
   ZH_COMP_PARAM->pLineFunc        = NULL;
   ZH_COMP_PARAM->pDeclFunc        = NULL;

   ZH_COMP_PARAM->iStaticCnt       = 0;
   ZH_COMP_PARAM->iVarScope        = ZH_VSCOMP_LOCAL;

   ZH_COMP_PARAM->inlines.iCount   = 0;
   ZH_COMP_PARAM->inlines.pFirst   = NULL;
   ZH_COMP_PARAM->inlines.pLast    = NULL;

   ZH_COMP_PARAM->szFile           = NULL;

   ZH_COMP_PARAM->iModulesCount    = 0;
}

static void zh_compGenOutput( ZH_COMP_DECL, int iLanguage )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compGenOutput()" ) );

   switch( iLanguage )
   {
      case ZH_LANG_C:
         zh_compGenCCode( ZH_COMP_PARAM, ZH_COMP_PARAM->pFileName );
         break;

      case ZH_LANG_PORT_OBJ:
         zh_compGenPortObj( ZH_COMP_PARAM, ZH_COMP_PARAM->pFileName );
         break;

      case ZH_LANG_PORT_OBJ_BUF:
         if( ZH_COMP_PARAM->pOutBuf )
            zh_xfree( ZH_COMP_PARAM->pOutBuf );
         zh_compGenBufPortObj( ZH_COMP_PARAM, &ZH_COMP_PARAM->pOutBuf, &ZH_COMP_PARAM->nOutBufSize );
         break;
   }
}

static void zh_compPpoFile( ZH_COMP_DECL, const char * szPrg, const char * szExt,
                            char * szPpoName )
{
   PZH_FNAME pFilePpo = zh_fsFNameSplit( szPrg );

   pFilePpo->szExtension = szExt;
   if( ZH_COMP_PARAM->pPpoPath )
   {
      pFilePpo->szPath = ZH_COMP_PARAM->pPpoPath->szPath;
      if( ZH_COMP_PARAM->pPpoPath->szName )
      {
         pFilePpo->szName = ZH_COMP_PARAM->pPpoPath->szName;
         pFilePpo->szExtension = ZH_COMP_PARAM->pPpoPath->szExtension;
      }
   }
   zh_fsFNameMerge( szPpoName, pFilePpo );
   zh_xfree( pFilePpo );
}

static void zh_compOutputFile( ZH_COMP_DECL )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compOutputFile()" ) );

   ZH_COMP_PARAM->pFileName->szPath = NULL;
   ZH_COMP_PARAM->pFileName->szExtension = NULL;

   /* we create the output file name */
   if( ZH_COMP_PARAM->pOutPath )
   {
      if( ZH_COMP_PARAM->pOutPath->szPath )
         ZH_COMP_PARAM->pFileName->szPath = ZH_COMP_PARAM->pOutPath->szPath;

      if( ZH_COMP_PARAM->pOutPath->szName )
      {
         ZH_COMP_PARAM->pFileName->szName = ZH_COMP_PARAM->pOutPath->szName;
         if( ZH_COMP_PARAM->pOutPath->szExtension )
            ZH_COMP_PARAM->pFileName->szExtension = ZH_COMP_PARAM->pOutPath->szExtension;
      }
   }
}

static void zh_compAddInitFunc( ZH_COMP_DECL, PZH_ZFUNC pFunc )
{
   PZH_HSYMBOL pSym = zh_compSymbolAdd( ZH_COMP_PARAM, pFunc->szName, NULL, ZH_SYM_FUNCNAME );

   pSym->cScope |= pFunc->cScope;
   pSym->pFunc = pFunc;
   pFunc->funFlags |= ZH_FUNF_ATTACHED;
   zh_compAddFunc( ZH_COMP_PARAM, pFunc );
   zh_compGenPCode1( ZH_P_ENDPROC, ZH_COMP_PARAM );
}

void zh_compModuleAdd( ZH_COMP_DECL, const char * szModuleName, ZH_BOOL fForce )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compModuleAdd(%s,%d)", szModuleName, fForce ) );

   if( ! ZH_COMP_PARAM->fSingleModule || fForce )
   {
      PZH_MODULE * pModule = &ZH_COMP_PARAM->modules;

      while( *pModule )
      {
         if( zh_stricmp( ( *pModule )->szName, szModuleName ) == 0 )
            return;
         pModule = &( *pModule )->pNext;
      }

      *pModule = ( PZH_MODULE ) zh_xgrab( sizeof( ZH_MODULE ) );
      ( *pModule )->szName = szModuleName;
      ( *pModule )->force = fForce;
      ( *pModule )->pNext = NULL;
   }
}

void zh_compCompileEnd( ZH_COMP_DECL )
{
   if( ZH_COMP_PARAM->pFileName )
   {
      zh_xfree( ZH_COMP_PARAM->pFileName );
      ZH_COMP_PARAM->pFileName = NULL;
   }

   while( ZH_COMP_PARAM->functions.pLast &&
          ! ZH_COMP_PARAM->functions.pLast->szName )
   {
      PZH_ZFUNC pFunc = ZH_COMP_PARAM->functions.pLast;
      ZH_COMP_PARAM->functions.pLast = pFunc->pOwner;
      zh_compFunctionKill( ZH_COMP_PARAM, pFunc );
   }
   ZH_COMP_PARAM->functions.pLast = NULL;

   if( ZH_COMP_PARAM->pInitFunc &&
       ( ZH_COMP_PARAM->pInitFunc->funFlags & ZH_FUNF_ATTACHED ) == 0 )
   {
      zh_compFunctionKill( ZH_COMP_PARAM, ZH_COMP_PARAM->pInitFunc );
   }
   ZH_COMP_PARAM->pInitFunc = NULL;

   if( ZH_COMP_PARAM->functions.pFirst )
   {
      PZH_ZFUNC pFunc = ZH_COMP_PARAM->functions.pFirst;

      while( pFunc )
         pFunc = zh_compFunctionKill( ZH_COMP_PARAM, pFunc );
      ZH_COMP_PARAM->functions.pFirst = NULL;
   }

   while( ZH_COMP_PARAM->externs )
   {
      PZH_HEXTERN pExtern = ZH_COMP_PARAM->externs;

      ZH_COMP_PARAM->externs = pExtern->pNext;
      zh_xfree( pExtern );
   }

   while( ZH_COMP_PARAM->modules )
   {
      PZH_MODULE pModule = ZH_COMP_PARAM->modules;

      ZH_COMP_PARAM->modules = pModule->pNext;
      zh_xfree( pModule );
   }

   while( ZH_COMP_PARAM->incfiles )
   {
      PZH_INCLST pIncFile = ZH_COMP_PARAM->incfiles;

      ZH_COMP_PARAM->incfiles = pIncFile->pNext;
      zh_xfree( pIncFile );
   }

   while( ZH_COMP_PARAM->ppdefines )
   {
      PZH_PPDEFINE pDefine = ZH_COMP_PARAM->ppdefines;

      ZH_COMP_PARAM->ppdefines = pDefine->pNext;
      zh_xfree( pDefine->szName );
      zh_xfree( pDefine );
   }

   while( ZH_COMP_PARAM->inlines.pFirst )
   {
      PZH_HINLINE pInline = ZH_COMP_PARAM->inlines.pFirst;

      ZH_COMP_PARAM->inlines.pFirst = pInline->pNext;
      if( pInline->pCode )
         zh_xfree( pInline->pCode );
      zh_xfree( pInline );
   }

   while( ZH_COMP_PARAM->symbols.pFirst )
   {
      PZH_HSYMBOL pSym = ZH_COMP_PARAM->symbols.pFirst;
      ZH_COMP_PARAM->symbols.pFirst = pSym->pNext;
      zh_xfree( pSym );
   }

   zh_compDeclaredReset( ZH_COMP_PARAM );
}

static void zh_compGenIncluded( ZH_COMP_DECL )
{
   if( ZH_COMP_PARAM->iTraceInclude > 0 && ZH_COMP_PARAM->incfiles )
   {
      PZH_INCLST pIncFile = ZH_COMP_PARAM->incfiles;
      char szDestFile[ ZH_PATH_MAX ];
      ZH_FNAME FileName;

      memcpy( &FileName, ZH_COMP_PARAM->pFileName, sizeof( ZH_FNAME ) );
      szDestFile[ 0 ] = '\0';

      if( ( ZH_COMP_PARAM->iTraceInclude & 0xff ) == 2 )
      {
         FileName.szExtension = ZH_COMP_PARAM->szDepExt;
         if( ! FileName.szExtension )
         {
            switch( ZH_COMP_PARAM->iLanguage )
            {
               case ZH_LANG_C:
                  FileName.szExtension = ".c";
                  break;
               case ZH_LANG_PORT_OBJ:
               case ZH_LANG_PORT_OBJ_BUF:
                  FileName.szExtension = ".zhb";
                  break;

               default:
                  FileName.szExtension = ".c";
            }
         }
         zh_fsFNameMerge( szDestFile, &FileName );
      }

      if( ( ZH_COMP_PARAM->iTraceInclude & 0x100 ) != 0 )
      {
         ZH_SIZE nLen = 0, u;
         ZH_BYTE * buffer;

         while( pIncFile )
         {
            nLen += strlen( pIncFile->szFileName ) + 1;
            pIncFile = pIncFile->pNext;
         }
         if( ZH_COMP_PARAM->nOutBufSize != 0 )
            ++nLen;
         u = strlen( szDestFile );
         if( u )
            nLen += u + 2;
         ZH_COMP_PARAM->pOutBuf = ( ZH_BYTE * ) zh_xrealloc(
                                       ZH_COMP_PARAM->pOutBuf,
                                       ZH_COMP_PARAM->nOutBufSize + nLen );
         buffer = ZH_COMP_PARAM->pOutBuf + ZH_COMP_PARAM->nOutBufSize;
         if( ZH_COMP_PARAM->nOutBufSize != 0 )
            *buffer++ = '\n';
         if( u )
         {
            memcpy( buffer, szDestFile, u );
            buffer += u;
            *buffer++ = ':';
            *buffer++ = ' ';
         }
         ZH_COMP_PARAM->nOutBufSize += nLen - 1;
         pIncFile = ZH_COMP_PARAM->incfiles;
         while( pIncFile )
         {
            u = strlen( pIncFile->szFileName );
            memcpy( buffer, pIncFile->szFileName, u );
            buffer +=u;
            pIncFile = pIncFile->pNext;
            *buffer++ = pIncFile ? ' ' : '\0';
         }
      }
      else if( ( ZH_COMP_PARAM->iTraceInclude & 0xff ) == 2 )
      {
         char szFileName[ ZH_PATH_MAX ];
         FILE * file;

         FileName.szExtension = ".d";
         zh_fsFNameMerge( szFileName, &FileName );
         file = zh_fopen( szFileName, "w" );
         if( file )
         {
            if( szDestFile[ 0 ] )
               fprintf( file, "%s:", szDestFile );
            while( pIncFile )
            {
               fprintf( file, " %s", pIncFile->szFileName );
               pIncFile = pIncFile->pNext;
            }
            fprintf( file, "\n" );
            fclose( file );
         }
         else
            zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_CREATE_OUTPUT, szFileName, NULL );
      }
      else
      {
         ZH_BOOL fFullQuiet = ZH_COMP_PARAM->fFullQuiet;
         ZH_COMP_PARAM->fFullQuiet = ZH_FALSE;
         if( szDestFile[ 0 ] )
         {
            zh_compOutStd( ZH_COMP_PARAM, szDestFile );
            zh_compOutStd( ZH_COMP_PARAM, ": " );
         }
         while( pIncFile )
         {
            if( pIncFile != ZH_COMP_PARAM->incfiles )
               zh_compOutStd( ZH_COMP_PARAM, " " );
            zh_compOutStd( ZH_COMP_PARAM, pIncFile->szFileName );
            pIncFile = pIncFile->pNext;
         }
         zh_compOutStd( ZH_COMP_PARAM, "\n" );
         ZH_COMP_PARAM->fFullQuiet = fFullQuiet;
      }
   }
}

static void zh_compSaveSwitches( ZH_COMP_DECL, PZH_COMP_SWITCHES pSwitches )
{
   pSwitches->fDebugInfo        = ZH_COMP_PARAM->fDebugInfo;
   pSwitches->fHideSource       = ZH_COMP_PARAM->fHideSource;
   pSwitches->fAutoMemvarAssume = ZH_COMP_PARAM->fAutoMemvarAssume;
   pSwitches->fI18n             = ZH_COMP_PARAM->fI18n;
   pSwitches->fLineNumbers      = ZH_COMP_PARAM->fLineNumbers;
   pSwitches->fPPO              = ZH_COMP_PARAM->fPPO;
   pSwitches->fPPT              = ZH_COMP_PARAM->fPPT;
   pSwitches->fQuiet            = ZH_COMP_PARAM->fQuiet;
   pSwitches->fForceMemvars     = ZH_COMP_PARAM->fForceMemvars;
   pSwitches->iStartProc        = ZH_COMP_PARAM->iStartProc;
   pSwitches->iWarnings         = ZH_COMP_PARAM->iWarnings;
   pSwitches->iGenCOutput       = ZH_COMP_PARAM->iGenCOutput;
   pSwitches->iExitLevel        = ZH_COMP_PARAM->iExitLevel;
   pSwitches->iHidden           = ZH_COMP_PARAM->iHidden;
   pSwitches->supported         = ZH_COMP_PARAM->supported;
}

static void zh_compRestoreSwitches( ZH_COMP_DECL, PZH_COMP_SWITCHES pSwitches )
{
   ZH_COMP_PARAM->fDebugInfo        = pSwitches->fDebugInfo;
   ZH_COMP_PARAM->fHideSource       = pSwitches->fHideSource;
   ZH_COMP_PARAM->fAutoMemvarAssume = pSwitches->fAutoMemvarAssume;
   ZH_COMP_PARAM->fI18n             = pSwitches->fI18n;
   ZH_COMP_PARAM->fLineNumbers      = pSwitches->fLineNumbers;
   ZH_COMP_PARAM->fPPO              = pSwitches->fPPO;
   ZH_COMP_PARAM->fPPT              = pSwitches->fPPT;
   ZH_COMP_PARAM->fQuiet            = pSwitches->fQuiet;
   ZH_COMP_PARAM->fForceMemvars     = pSwitches->fForceMemvars;
   ZH_COMP_PARAM->iStartProc        = pSwitches->iStartProc;
   ZH_COMP_PARAM->iWarnings         = pSwitches->iWarnings;
   ZH_COMP_PARAM->iGenCOutput       = pSwitches->iGenCOutput;
   ZH_COMP_PARAM->iExitLevel        = pSwitches->iExitLevel;
   ZH_COMP_PARAM->iHidden           = pSwitches->iHidden;
   ZH_COMP_PARAM->supported         = pSwitches->supported;
}

static int zh_compCompile( ZH_COMP_DECL, const char * szPrg, const char * szBuffer, int iStartLine )
{
   char buffer[ ZH_PATH_MAX * 2 + 80 ];
   ZH_COMP_SWITCHES switches;
   int iStatus = EXIT_SUCCESS;
   PZH_FNAME pFileName = NULL;
   PZH_MODULE pModule;
   ZH_BOOL fGenCode = ZH_TRUE;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compCompile(%s,%p,%d)", szPrg, ( const void * ) szBuffer, iStartLine ) );

   zh_compSaveSwitches( ZH_COMP_PARAM, &switches );
   /* Initialize support variables */
   zh_compInitVars( ZH_COMP_PARAM );

   if( ! szBuffer )
   {
      if( szPrg[ 0 ] == '@' )
         iStatus = zh_compReadClpFile( ZH_COMP_PARAM, szPrg + 1 );
      else
         zh_compModuleAdd( ZH_COMP_PARAM,
                           zh_compIdentifierNew( ZH_COMP_PARAM, szPrg, ZH_IDENT_COPY ),
                           ZH_TRUE );
   }

   pModule = ZH_COMP_PARAM->modules;
   while( iStatus == EXIT_SUCCESS && ! ZH_COMP_PARAM->fExit &&
          ( pModule || szBuffer ) )
   {
      char szFileName[ ZH_PATH_MAX ];     /* filename to parse */
      ZH_BOOL fSkip = ZH_FALSE;

      /* Clear and reinitialize preprocessor state */
      zh_pp_reset( ZH_COMP_PARAM->pLex->pPP );
      ZH_COMP_PARAM->pLex->iState = ZH_COMP_PARAM->pLex->iClose =
      ZH_COMP_PARAM->pLex->iScope = 0;
      ZH_COMP_PARAM->pLex->fEol = ZH_FALSE;
      zh_compDeclaredReset( ZH_COMP_PARAM );

      if( ! szBuffer )
         szPrg = pModule->szName;

      if( pFileName && ZH_COMP_PARAM->pFileName != pFileName )
         zh_xfree( pFileName );
      pFileName = zh_fsFNameSplit( szPrg );
      if( ! ZH_COMP_PARAM->pFileName )
         ZH_COMP_PARAM->pFileName = pFileName;

      if( ! pFileName->szName )
      {
         zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'F', ZH_COMP_ERR_BADFILENAME, szPrg, NULL );
         iStatus = EXIT_FAILURE;
         break;
      }

      if( ! pFileName->szExtension )
         pFileName->szExtension = ".zh";
      zh_fsFNameMerge( szFileName, pFileName );

      if( szBuffer )
      {
         if( ! zh_pp_inBuffer( ZH_COMP_PARAM->pLex->pPP, szFileName, szBuffer, strlen( szBuffer ), iStartLine ) )
         {
            zh_compOutErr( ZH_COMP_PARAM, "Cannot create preprocessor buffer." );
            iStatus = EXIT_FAILURE;
            break;
         }
      }
      else if( ! zh_pp_inFile( ZH_COMP_PARAM->pLex->pPP, szFileName, ZH_FALSE, NULL, ZH_FALSE ) )
      {
         zh_snprintf( buffer, sizeof( buffer ),
                      "Cannot open %s, assumed external\n", szFileName );
         zh_compOutErr( ZH_COMP_PARAM, buffer );
         if( pModule->force )
         {
            iStatus = EXIT_FAILURE;
            break;
         }
         fSkip = ZH_TRUE;
      }

      if( ! fSkip )
      {
         char szPpoName[ ZH_PATH_MAX ];

         if( ZH_COMP_PARAM->fPPT )
         {
            zh_compPpoFile( ZH_COMP_PARAM, szFileName, ".ppt", szPpoName );
            if( ! zh_pp_traceFile( ZH_COMP_PARAM->pLex->pPP, szPpoName, NULL ) )
            {
               iStatus = EXIT_FAILURE;
               break;
            }
         }

         if( ZH_COMP_PARAM->fPPO )
         {
            zh_compPpoFile( ZH_COMP_PARAM, szFileName, ".ppo", szPpoName );
            if( ! zh_pp_outFile( ZH_COMP_PARAM->pLex->pPP, szPpoName, NULL ) )
            {
               iStatus = EXIT_FAILURE;
               break;
            }
         }

#if ! defined( ZH_MODULES_MERGE )
         /* TODO: ZHB format does not support yet multiple static functions
          *       with the same name. Such functionality needs extended .ZHB
          *       file format.
          */
         if( ZH_COMP_PARAM->iLanguage != ZH_LANG_PORT_OBJ &&
             ZH_COMP_PARAM->iLanguage != ZH_LANG_PORT_OBJ_BUF )
         {
            if( ZH_COMP_PARAM->iModulesCount++ )
               zh_compExternGen( ZH_COMP_PARAM );
         }
         else
#endif
            ZH_COMP_PARAM->iModulesCount = 1;

         ZH_COMP_PARAM->currLine = zh_pp_line( ZH_COMP_PARAM->pLex->pPP ) + 1;
         ZH_COMP_PARAM->currModule = zh_compIdentifierNew(
                                    ZH_COMP_PARAM, szFileName, ZH_IDENT_COPY );
         if( ZH_COMP_PARAM->szFile == NULL )
            ZH_COMP_PARAM->szFile = ZH_COMP_PARAM->currModule;

         if( szBuffer )
            /* Generate the starting procedure frame, lower letters used
             * intentionally to not create name conflicts when -n2 switch
             * is used and we compile code not encapsulated in function.
             */
            zh_compFunctionAdd( ZH_COMP_PARAM, "__zhInit", ZH_FS_STATIC,
                                ZH_FUNF_PROCEDURE | ZH_FUNF_FILE_FIRST | ZH_FUNF_FILE_DECL );
         else
         {
            if( ! ZH_COMP_PARAM->fQuiet )
            {
               if( ZH_COMP_PARAM->fPPO )
                  zh_snprintf( buffer, sizeof( buffer ),
                               "Compiling '%s' and generating preprocessed output to '%s'...\n",
                               szFileName, szPpoName );
               else
                  zh_snprintf( buffer, sizeof( buffer ),
                               "Compiling '%s'...\n", szFileName );
               zh_compOutStd( ZH_COMP_PARAM, buffer );
            }

            /* Generate the starting procedure frame */
            zh_compFunctionAdd( ZH_COMP_PARAM,
                                zh_compIdentifierNew( ZH_COMP_PARAM, zh_strupr( zh_strdup( pFileName->szName ) ), ZH_IDENT_FREE ),
                                ZH_FS_PUBLIC, ZH_FUNF_PROCEDURE | ZH_FUNF_FILE_FIRST | ( ZH_COMP_PARAM->iStartProc == 0 ? 0 : ZH_FUNF_FILE_DECL ) );
         }

         if( ! ZH_COMP_PARAM->fExit )
         {
            int iExitLevel = ZH_COMP_PARAM->iExitLevel;
            if( ZH_COMP_PARAM->iSyntaxCheckOnly >= 2 )
               zh_compParserRun( ZH_COMP_PARAM );
            else
               zh_comp_yyparse( ZH_COMP_PARAM );
            ZH_COMP_PARAM->iExitLevel = ZH_MAX( iExitLevel, ZH_COMP_PARAM->iExitLevel );
         }
      }

      if( szBuffer )
      {
         szBuffer = NULL;
         pModule = ZH_COMP_PARAM->modules;
      }
      else
         pModule = pModule->pNext;

      while( pModule && ! pModule->force &&
             zh_compIsModuleFunc( ZH_COMP_PARAM, pModule->szName ) )
         pModule = pModule->pNext;
   }

   if( pFileName && ZH_COMP_PARAM->pFileName != pFileName )
      zh_xfree( pFileName );

   if( ! ZH_COMP_PARAM->fExit && iStatus == EXIT_SUCCESS )
   {
      /* Begin of finalization phase. */

      /* fix all previous function returns offsets */
      zh_compFinalizeFunction( ZH_COMP_PARAM );

      if( ZH_COMP_PARAM->fDebugInfo )
         zh_compExternAdd( ZH_COMP_PARAM, "__DBGENTRY", 0 );

      zh_compExternGen( ZH_COMP_PARAM );       /* generates EXTERN symbols names */

      if( ZH_COMP_PARAM->pInitFunc )
      {
         char szNewName[ 25 ];

         /* Mark thread static variables */
         zh_compStaticDefThreadSet( ZH_COMP_PARAM );
         /* Fix the number of static variables */
         ZH_COMP_PARAM->pInitFunc->pCode[ 3 ] = ZH_LOBYTE( ZH_COMP_PARAM->iStaticCnt );
         ZH_COMP_PARAM->pInitFunc->pCode[ 4 ] = ZH_HIBYTE( ZH_COMP_PARAM->iStaticCnt );
         ZH_COMP_PARAM->pInitFunc->iStaticsBase = ZH_COMP_PARAM->iStaticCnt;
         /* Update pseudo function name */
         zh_snprintf( szNewName, sizeof( szNewName ), "(_INITSTATICS%05d)", ZH_COMP_PARAM->iStaticCnt );
         ZH_COMP_PARAM->pInitFunc->szName = zh_compIdentifierNew( ZH_COMP_PARAM, szNewName, ZH_IDENT_COPY );

         zh_compAddInitFunc( ZH_COMP_PARAM, ZH_COMP_PARAM->pInitFunc );
      }

      if( ZH_COMP_PARAM->fLineNumbers && ZH_COMP_PARAM->fDebugInfo )
      {
         PZH_DEBUGINFO pInfo = zh_compGetDebugInfo( ZH_COMP_PARAM ), pNext;
         if( pInfo )
         {
            int iModules = 0;
            zh_compLineNumberDefStart( ZH_COMP_PARAM );
            do
            {
               ZH_SIZE nSkip = pInfo->ulFirstLine >> 3;
               ZH_SIZE nLen = ( ( pInfo->ulLastLine + 7 ) >> 3 ) - nSkip;

               zh_compGenPushString( pInfo->pszModuleName, strlen( pInfo->pszModuleName ) + 1, ZH_COMP_PARAM );
               zh_compGenPushLong( nSkip << 3, ZH_COMP_PARAM );
               zh_compGenPushString( ( char * ) pInfo->pLineMap + nSkip, nLen + 1, ZH_COMP_PARAM );
               zh_compGenPCode3( ZH_P_ARRAYGEN, 3, 0, ZH_COMP_PARAM );
               iModules++;

               pNext = pInfo->pNext;
               zh_xfree( pInfo->pszModuleName );
               zh_xfree( pInfo->pLineMap );
               zh_xfree( pInfo );
               pInfo = pNext;
            }
            while( pInfo );

            zh_compGenPCode3( ZH_P_ARRAYGEN, ZH_LOBYTE( iModules ), ZH_HIBYTE( iModules ), ZH_COMP_PARAM );
            zh_compGenPCode1( ZH_P_RETVALUE, ZH_COMP_PARAM );
            zh_compLineNumberDefEnd( ZH_COMP_PARAM );
            zh_compAddInitFunc( ZH_COMP_PARAM, ZH_COMP_PARAM->pLineFunc );
         }
      }

      if( ZH_COMP_PARAM->szAnnounce )
         zh_compAnnounce( ZH_COMP_PARAM, ZH_COMP_PARAM->szAnnounce );

      zh_compUpdateFunctionNames( ZH_COMP_PARAM );

      /* End of finalization phase. */

      if( ZH_COMP_PARAM->iErrorCount || ZH_COMP_PARAM->fAnyWarning )
      {
         if( ZH_COMP_PARAM->iErrorCount )
         {
            iStatus = EXIT_FAILURE;
            fGenCode = ZH_FALSE;
            zh_snprintf( buffer, sizeof( buffer ),
                         "\r%i error%s\n",
                         ZH_COMP_PARAM->iErrorCount,
                         ZH_COMP_PARAM->iErrorCount > 1 ? "s" : "" );
            zh_compOutStd( ZH_COMP_PARAM, buffer );
         }
         else if( ZH_COMP_PARAM->iExitLevel == ZH_EXITLEVEL_SETEXIT )
         {
            iStatus = EXIT_FAILURE;
         }
         else if( ZH_COMP_PARAM->iExitLevel == ZH_EXITLEVEL_DELTARGET )
         {
            iStatus = EXIT_FAILURE;
            fGenCode = ZH_FALSE;
         }
      }

      /* we create the output file name */
      zh_compOutputFile( ZH_COMP_PARAM );

      if( fGenCode && ZH_COMP_PARAM->iErrorCount == 0 &&
          ZH_COMP_PARAM->iTraceInclude > 0 )
         zh_compGenIncluded( ZH_COMP_PARAM );

      if( ZH_COMP_PARAM->iSyntaxCheckOnly == 0 &&
          fGenCode && ZH_COMP_PARAM->iErrorCount == 0 )
      {
         const char * szFirstFunction = NULL;
         int iFunctionCount = 0;
         PZH_ZFUNC pFunc;


         pFunc = ZH_COMP_PARAM->functions.pFirst;

         while( pFunc && ! ZH_COMP_PARAM->fExit )
         {
            /* skip pseudo function frames used in automatically included
               files for file wide declarations */
            if( ( pFunc->funFlags & ZH_FUNF_FILE_DECL ) == 0 )
            {
               zh_compOptimizeFrames( ZH_COMP_PARAM, pFunc );

               if( szFirstFunction == NULL &&
                  ! ( pFunc->cScope & ( ZH_FS_INIT | ZH_FS_EXIT ) ) )
               {
                  szFirstFunction = pFunc->szName;
               }
               if( pFunc != ZH_COMP_PARAM->pInitFunc &&
                   pFunc != ZH_COMP_PARAM->pLineFunc )
                  ++iFunctionCount;
            }
            pFunc = pFunc->pNext;
         }

         if( szFirstFunction )
         {
            PZH_HSYMBOL pSym = zh_compSymbolFind( ZH_COMP_PARAM, szFirstFunction,
                                                  NULL, ZH_SYM_FUNCNAME );
            if( pSym )
               pSym->cScope |= ZH_FS_FIRST;
         }

         if( ! ZH_COMP_PARAM->fQuiet )
         {
            zh_snprintf( buffer, sizeof( buffer ),
                         "\rLines %i, Functions/Procedures %i\n",
                         zh_pp_lineTot( ZH_COMP_PARAM->pLex->pPP ),
                         iFunctionCount );
            zh_compOutStd( ZH_COMP_PARAM, buffer );
         }

         zh_compGenOutput( ZH_COMP_PARAM, ZH_COMP_PARAM->iLanguage );
      }
      else
         fGenCode = ZH_FALSE;

      if( fGenCode && ZH_COMP_PARAM->iErrorCount == 0 )
      {
         if( zh_compI18nSave( ZH_COMP_PARAM, ZH_FALSE ) )
            zh_compI18nFree( ZH_COMP_PARAM );
      }
   }
   else
      fGenCode = ZH_FALSE;

   if( ! fGenCode && ! ZH_COMP_PARAM->fExit &&
       ZH_COMP_PARAM->iSyntaxCheckOnly == 0 )
      zh_compOutStd( ZH_COMP_PARAM, "\nNo code generated.\n" );

   zh_compCompileEnd( ZH_COMP_PARAM );
   zh_compRestoreSwitches( ZH_COMP_PARAM, &switches );

   return ZH_COMP_PARAM->fExit ? EXIT_FAILURE : iStatus;
}
