/*
 * Allocate/free new compiler context
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

#include "zh_comp.h"

static PZH_EXPR zh_compExprAlloc( ZH_COMP_DECL )
{
   PZH_EXPRLST pExpItm = ( PZH_EXPRLST ) zh_xgrab( sizeof( ZH_EXPRLST ) );

   pExpItm->pNext = ZH_COMP_PARAM->pExprLst;
   ZH_COMP_PARAM->pExprLst = pExpItm;
   if( pExpItm->pNext )
   {
      pExpItm->pPrev = pExpItm->pNext->pPrev;
      pExpItm->pNext->pPrev = pExpItm;
      pExpItm->pPrev->pNext = pExpItm;
   }
   else
      pExpItm->pPrev = pExpItm->pNext = pExpItm;

   return &pExpItm->Expression;
}

static void zh_compExprDealloc( ZH_COMP_DECL, PZH_EXPR pExpr )
{
   if( ZH_COMP_PARAM->pExprLst )
   {
      PZH_EXPRLST pExpItm = ( PZH_EXPRLST ) pExpr;

      pExpItm->pNext->pPrev = pExpItm->pPrev;
      pExpItm->pPrev->pNext = pExpItm->pNext;
      if( pExpItm == ZH_COMP_PARAM->pExprLst )
      {
         if( pExpItm->pNext == pExpItm )
            ZH_COMP_PARAM->pExprLst = NULL;
         else
            ZH_COMP_PARAM->pExprLst = pExpItm->pNext;
      }
      zh_xfree( pExpItm );
   }
   else
      pExpr->ExprType = ZH_ET_NONE;
}

static PZH_EXPR zh_compExprNew( ZH_COMP_DECL, ZH_EXPRTYPE iType )
{
   PZH_EXPR pExpr;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprNew(%p,%i)", ( void * ) ZH_COMP_PARAM, iType ) );

   pExpr = zh_compExprAlloc( ZH_COMP_PARAM );
   pExpr->ExprType = iType;
   pExpr->pNext    = NULL;
   pExpr->ValType  = ZH_EV_UNKNOWN;

   return pExpr;
}

/* Delete self - all components will be deleted somewhere else
 */
static void zh_compExprClear( ZH_COMP_DECL, PZH_EXPR pExpr )
{
   zh_compExprDealloc( ZH_COMP_PARAM, pExpr );
}

/* Delete all components and delete self
 */
static void zh_compExprFree( ZH_COMP_DECL, PZH_EXPR pExpr )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compExprFree()" ) );

   ZH_EXPR_USE( pExpr, ZH_EA_DELETE );
   zh_compExprDealloc( ZH_COMP_PARAM, pExpr );
}

static void zh_compExprLstDealloc( ZH_COMP_DECL )
{
   if( ZH_COMP_PARAM->pExprLst )
   {
      PZH_EXPRLST pExpItm, pExp;
      pExpItm = pExp = ZH_COMP_PARAM->pExprLst;
      ZH_COMP_PARAM->pExprLst = NULL;
      do
      {
         zh_compExprFree( ZH_COMP_PARAM, &pExp->Expression );
         pExp = pExp->pNext;
      }
      while( pExp != pExpItm );
      do
      {
         PZH_EXPRLST pFree = pExp;
         pExp = pExp->pNext;
         zh_xfree( pFree );
      }
      while( pExp != pExpItm );
   }
}

static PZH_EXPR zh_compErrorType( ZH_COMP_DECL, PZH_EXPR pExpr )
{
   const char * szDesc = zh_compExprDescription( pExpr );

   zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_INVALID_TYPE, szDesc, NULL );
   return pExpr;
}

static PZH_EXPR zh_compErrorSyntax( ZH_COMP_DECL, PZH_EXPR pExpr )
{
   const char * szDesc = zh_compExprDescription( pExpr );

   zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_SYNTAX, szDesc, NULL );
   return pExpr;
}

static void zh_compErrorDuplVar( ZH_COMP_DECL, const char * szVarName )
{
   zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_VAR_DUPL, szVarName, NULL );
}

static void zh_compOutMsg( void * cargo, int iErrorFmt, int iLine,
                           const char * szModule, char cPrefix, int iValue,
                           const char * szText,
                           const char * szPar1, const char * szPar2 )
{
   char buffer[ 512 ];

   if( szModule )
   {
      if( iErrorFmt == ZH_ERRORFMT_CLIPPER )
         zh_snprintf( buffer, sizeof( buffer ), "\r%s(%i) ", szModule, iLine );
      else if( iLine )
         zh_snprintf( buffer, sizeof( buffer ), "\n%s:%i: ", szModule, iLine );
      else
         zh_snprintf( buffer, sizeof( buffer ), "\n%s:%s ", szModule, szPar2 );

      zh_compOutErr( ( PZH_COMP ) cargo, buffer );
   }

   if( iErrorFmt == ZH_ERRORFMT_CLIPPER )
      zh_snprintf( buffer, sizeof( buffer ), "%s %c%04i  ",
                   cPrefix == 'W' ? "Warning" : "Error", cPrefix, iValue );
   else
      zh_snprintf( buffer, sizeof( buffer ), "%s %c%04i  ",
                   cPrefix == 'W' ? "warning" : "error", cPrefix, iValue );

   zh_compOutErr( ( PZH_COMP ) cargo, buffer );
   zh_snprintf( buffer, sizeof( buffer ), szText, szPar1, szPar2 );
   zh_compOutErr( ( PZH_COMP ) cargo, buffer );
   zh_compOutErr( ( PZH_COMP ) cargo, "\n" );
}

void zh_compOutStd( ZH_COMP_DECL, const char * szMessage )
{
   if( ! ZH_COMP_PARAM->fFullQuiet )
   {
      if( ZH_COMP_PARAM->outStdFunc )
         ZH_COMP_PARAM->outStdFunc( ZH_COMP_PARAM, szMessage );
      else
      {
#if defined( ZH_OS_DOS )
         fprintf( stderr, "%s", szMessage ); fflush( stderr );
#else
         fprintf( stdout, "%s", szMessage ); fflush( stdout );
#endif
      }
   }
}

void zh_compOutErr( ZH_COMP_DECL, const char * szMessage )
{
   if( ! ZH_COMP_PARAM->fFullQuiet )
   {
      if( ZH_COMP_PARAM->outErrFunc )
         ZH_COMP_PARAM->outErrFunc( ZH_COMP_PARAM, szMessage );
      else
      {
#if defined( ZH_OS_DOS )
         fprintf( stdout, "%s", szMessage ); fflush( stdout );
#else
         fprintf( stderr, "%s", szMessage ); fflush( stderr );
#endif
      }
   }
}
static const ZH_COMP_FUNCS s_comp_funcs =
{
   zh_compExprNew,
   zh_compExprClear,
   zh_compExprFree,

   zh_compErrorType,
   zh_compErrorSyntax,
   zh_compErrorDuplVar,
};

PZH_COMP zh_comp_new( void )
{
   PZH_COMP pComp = NULL;
   PZH_PP_STATE pPP = zh_pp_new();

   if( pPP )
   {
      pComp = ( PZH_COMP ) zh_xgrabz( sizeof( ZH_COMP ) );
      pComp->pLex = ( PZH_COMP_LEX ) zh_xgrabz( sizeof( ZH_COMP_LEX ) );

      /* initialize default settings */
      pComp->mode = ZH_MODE_COMPILER;
      pComp->funcs = &s_comp_funcs;

      pComp->pLex->pPP = pPP;

      /* various compatibility flags (-k switch)
         activate Ziher extensions by default. */
      pComp->supported = ZH_COMPFLAG_ZIHER   |
                         ZH_COMPFLAG_XBASE     |
                         ZH_COMPFLAG_ZH_INLINE |
                         ZH_COMPFLAG_OPTJUMP   |
                         ZH_COMPFLAG_MACROTEXT |
                         ZH_COMPFLAG_SHORTCUTS;

      pComp->fSwitchCase       = ZH_FALSE;
      pComp->fPPO              = ZH_FALSE;   /* flag indicating, is .ppo output needed */
      pComp->fLineNumbers      = ZH_TRUE;    /* holds if we need pcodes with line numbers */
      pComp->fAnyWarning       = ZH_FALSE;   /* holds if there was any warning during the compilation process */
      pComp->fAutoMemvarAssume = ZH_FALSE;   /* holds if undeclared variables are automatically assumed MEMVAR (-a)*/
      pComp->fForceMemvars     = ZH_FALSE;   /* holds if memvars are assumed when accessing undeclared variable (-v)*/
      pComp->fDebugInfo        = ZH_FALSE;   /* holds if generate debugger required info */
      pComp->fHideSource       = ! pComp->fDebugInfo;  /* do not store .prg file names in PCODE */
      pComp->fNoStartUp        = ZH_FALSE;   /* C code generation embed ZH_FS_FIRST or not */
      pComp->fCredits          = ZH_FALSE;   /* print credits */
      pComp->fBuildInfo        = ZH_FALSE;   /* print build info */
      pComp->fGauge            = ZH_TRUE;    /* line counter gauge */
      pComp->fLogo             = ZH_TRUE;    /* print logo */
      pComp->fSingleModule     = ZH_FALSE;
      pComp->fError            = ZH_FALSE;
      pComp->fINCLUDE          = ZH_TRUE;

      pComp->iSyntaxCheckOnly = 0;               /* syntax check only */
      pComp->iStartProc       = 0;               /* no implicit starting procedure */
      pComp->iWarnings        = 0;               /* enable parse warnings */
      pComp->iErrorCount      = 0;               /* number of compile errors */

      pComp->iGenCOutput = ZH_COMPGENC_COMPACT;  /* C code generation default mode */
      pComp->iExitLevel  = ZH_EXITLEVEL_DEFAULT; /* holds if there was any warning during the compilation process */
      pComp->iLanguage   = ZH_LANG_C;            /* default Ziher generated output language */
      pComp->iErrorFmt   = ZH_ERRORFMT_CLIPPER;  /* default Ziher generated output language */

      pComp->outMsgFunc  = zh_compOutMsg;
   }

   return pComp;
}

void zh_comp_free( PZH_COMP pComp )
{
   zh_compI18nFree( pComp );
   zh_compCompileEnd( pComp );
   zh_compParserStop( pComp );

   /* free allocated expressions only when errors appear - in all
    * other cases expressions should be always cleanly freed so
    * executing zh_compExprLstDealloc() may only hides some real
    * memory leaks
    */
   if( pComp->iErrorCount != 0 )
      zh_compExprLstDealloc( pComp );

   zh_compIdentifierClose( pComp );

   if( pComp->pOutPath )
      zh_xfree( pComp->pOutPath );

   if( pComp->pPpoPath )
      zh_xfree( pComp->pPpoPath );

   while( pComp->modules )
   {
      PZH_MODULE pModule = pComp->modules;

      pComp->modules = pComp->modules->pNext;
      zh_xfree( pModule );
   }

   while( pComp->pVarType )
   {
      PZH_VARTYPE pVarType = pComp->pVarType;

      pComp->pVarType = pComp->pVarType->pNext;
      zh_xfree( pVarType );
   }

   if( pComp->pOutBuf )
      zh_xfree( pComp->pOutBuf );

   if( pComp->pLex )
   {
      if( pComp->pLex->pPP )
         zh_pp_free( pComp->pLex->pPP );
      zh_xfree( pComp->pLex );
   }

   if( pComp->szDepExt )
      zh_xfree( pComp->szDepExt );

   if( pComp->szStdCh )
      zh_xfree( pComp->szStdCh );

   if( pComp->iStdChExt > 0 )
   {
      do
      {
         zh_xfree( pComp->szStdChExt[ --pComp->iStdChExt ] );
      }
      while( pComp->iStdChExt != 0 );
      zh_xfree( pComp->szStdChExt );
   }

   if( pComp->pI18nFileName )
      zh_xfree( pComp->pI18nFileName );

   zh_xfree( pComp );
}
