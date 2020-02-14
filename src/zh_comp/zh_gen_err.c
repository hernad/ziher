/*
 * Compiler parse errors & warnings messages
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
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

/* Table with parse errors */
const char * const zh_comp_szErrors[] =
{
   "Statement not allowed outside of procedure or function",
   "Redefinition of procedure or function '%s'",
   "Duplicate variable declaration '%s'",
   "%s declaration follows executable statement",
   "Outer codeblock variable '%s' is out of reach",
   "Invalid numeric format '.'",
   "Unterminated string '%s'",
   "Redefinition of predefined function %s as '%s'",
   "Illegal variable '%s' initializer '%s'",
   "ENDIF does not match IF",
   "ENDDO does not match WHILE",
   "ENDCASE does not match DO CASE",
   "NEXT does not match FOR",
   "ELSE does not match IF",
   "ELSEIF does not match IF",
   "Syntax error '%s'",
   "Unclosed control structure '%s'",
   "%s statement with no loop in sight",
   "Syntax error '%s' in '%s'",
   "Incomplete statement or unbalanced delimiters",
   "Incorrect number of arguments in %s %s",   // E0021
   "Invalid lvalue '%s'",
   "Invalid use of '@' (pass by reference) '%s'",
   "Formal parameters already declared",
   "Invalid %s from within of SEQUENCE code",
   "Unterminated array index",
   "Could not allocate %s byte(s)",
   "Could not reallocate %s byte(s)",
   "Freeing a NULL memory pointer",
   "Syntax error \"%s at '%s'\"",
   "Jump offset too long",
   "Can't create output file '%s'",
   "Can't create preprocessed output file '%s'",
   "Bad command-line option '%s'",
   "Bad command-line parameter '%s'",
   "Invalid filename '%s'",
   "Mayhem in CASE handler",
   "Operation not supported for data type '%s'",
   "Invalid alias expression '%s'",
   "Invalid array index expression '%s'",
   "Bound error '%s'",
   "Macro of declared symbol '%s'",
   "Invalid selector '%s' in send",
   "ANNOUNCEd procedure '%s' must be a public symbol",
   "Jump PCode not found",
   "CASE or OTHERWISE does not match DO CASE or SWITCH",
   "Code block contains both macro and declared symbol references '%s'",
   "GET contains complex macro",
   "Unterminated inline block in function '%s'",
   "Too many inline blocks %s",
   "Inline C requires C output generation, use -gc[n]",
   "Too many local variables [%s] or parameters [%s]",
   "Too many enumerate variables in FOR EACH loop",
   "Incorrect number of enumerate variables",
   "CASE requires either numeric or string constant",
   "String too long for SWITCH",
   "Invalid date constant '%s'",
   "Invalid timestamp constant '%s'",
   "Memory buffer overflow",
   "Memory corruption detected",
   "Implicit send operator with no WITH OBJECT in sight",
   "Input buffer overflow",
   "Unsupported output language option",
   "String too long",
   "Code block size too big",
   "%s not declared with variable number of parameters",
   "Can't find %s file",
   "Invalid ALWAYS after %s in RECOVER code",
   "File write error",
   "Duplicate case value",
   "ENDWITH does not match WITH OBJECT",
   "ENDSWITCH does not match SWITCH",
   "END SEQUENCE does not match BEGIN SEQUENCE",
   "Code block contains both macro and WITH OBJECT messages ':%s'",
   /* Some historical, funny sounding error messages from original CA-Cl*pper.
      They serve no purpose whatsoever. [vszakats] */
   "END wreaks terrible vengeance on control stack",
   "Control level closure leaves gaping wound in control stack",
   "Ford Maverick error number",
   "Something terrible has happened"
};

/* Table with parse warnings */
/* NOTE: The first character stores the warning's level that triggers this
 * warning. The warning's level is set by -w<n> command-line option.
 */
const char * const zh_comp_szWarnings[] =
{
   "1Ambiguous reference '%s'",
   "1Ambiguous reference, assuming memvar '%s'",
   "2Variable '%s' declared but not used in function '%s'",
   "2Codeblock parameter '%s' declared but not used in function '%s'",
   "1RETURN statement with no return value in function",
   "1Procedure returns value",
   "1Function '%s' does not end with RETURN statement",
   "3Incompatible type in assignment to '%s', expected '%s'",
   "3Incompatible operand type '%s', expected '%s'",
   "3Incompatible operand types '%s' and '%s'",
   "4Suspicious type in assignment to '%s', expected '%s'",
   "4Suspicious operand type 'unknown', expected '%s'",
   "3Can't use array index with non-array",
   "3Incompatible return type '%s', expected '%s'",
   "4Suspicious return type '%s', expected '%s'",
   "3Invalid number of parameters %s, expected %s",
   "3Incompatible parameter '%s', expected '%s'",
   "4Suspicious parameter '%s', expected '%s'",
   "3Duplicate declaration of %s '%s'",
   "3Function '%s' conflicting with its declaration",
   "3Variable '%s' used but never initialized",
   "3Value of Variable '%s' never used",
   "3Incompatible type in assignment to declared array element expected '%s'",
   "4Suspicious type in assignment to declared array element expected '%s'",
   "3Class '%s' not known in declaration of '%s'",
   "3Message '%s' not known in class '%s'",
   "1Meaningless use of expression '%s'",
   "2Unreachable code",
   "1Redundant 'ANNOUNCE %s' statement ignored",
   "1Duplicate variable '%s' in nested FOR loop",
   "1Invalid variable '%s' for enumerator message",
   "3Variable '%s' is assigned but not used in function '%s'",
   "3Variable '%s' is never assigned in function '%s'",
   "2STATIC Function '%s' defined but never used"
};

static void zh_compDispMessage( ZH_COMP_DECL, char cPrefix, int iValue,
                                const char * szText,
                                const char * szPar1, const char * szPar2 )
{
   ZH_COMP_PARAM->outMsgFunc( ZH_COMP_PARAM, ZH_COMP_PARAM->iErrorFmt,
                              ZH_COMP_PARAM->currLine, ZH_COMP_PARAM->currModule,
                              cPrefix, iValue, szText, szPar1, szPar2 );
}

void zh_compGenError( ZH_COMP_DECL, const char * const szErrors[],
                      char cPrefix, int iError,
                      const char * szError1, const char * szError2 )
{
   if( ! ZH_COMP_PARAM->fExit && ( cPrefix == 'F' || ! ZH_COMP_PARAM->fError ) )
   {
      PZH_ZFUNC pFunc = ZH_COMP_PARAM->functions.pLast;

      zh_compDispMessage( ZH_COMP_PARAM, cPrefix, iError,
                          szErrors[ iError - 1 ], szError1, szError2 );

      ZH_COMP_PARAM->iErrorCount++;
      ZH_COMP_PARAM->fError = ZH_TRUE;
      while( pFunc )
      {
         pFunc->bError = ZH_TRUE;
         pFunc = pFunc->pOwner;
      }
      /* fatal error - exit immediately */
      if( cPrefix == 'F' )
         ZH_COMP_PARAM->fExit = ZH_TRUE;
   }
}

void zh_compGenWarning( ZH_COMP_DECL, const char * const szWarnings[],
                        char cPrefix, int iWarning,
                        const char * szWarning1, const char * szWarning2 )
{
   const char * szText = szWarnings[ iWarning - 1 ];

   if( ! ZH_COMP_PARAM->fExit && ( ( int ) ( szText[ 0 ] - '0' ) <= ZH_COMP_PARAM->iWarnings ) )
   {
      zh_compDispMessage( ZH_COMP_PARAM, cPrefix, iWarning,
                          szText + 1, szWarning1, szWarning2 );

      ZH_COMP_PARAM->fAnyWarning = ZH_TRUE;    /* report warnings at exit */
   }
}

PZH_EXPR zh_compErrorLValue( ZH_COMP_DECL, PZH_EXPR pExpr )
{
   const char * szDesc = zh_compExprDescription( pExpr );

   zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_INVALID_LVALUE, szDesc, NULL );
   return pExpr;
}

PZH_EXPR zh_compErrorIndex( ZH_COMP_DECL, PZH_EXPR pExpr )
{
   const char * szDesc = zh_compExprDescription( pExpr );

   zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_INVALID_INDEX, szDesc, NULL );
   return pExpr;
}

PZH_EXPR zh_compErrorBound( ZH_COMP_DECL, PZH_EXPR pExpr )
{
   const char * szDesc = zh_compExprDescription( pExpr );

   zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_INVALID_BOUND, szDesc, NULL );
   return pExpr;
}

PZH_EXPR zh_compErrorAlias( ZH_COMP_DECL, PZH_EXPR pExpr )
{
   const char * szDesc = zh_compExprDescription( pExpr );

   zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_INVALID_ALIAS, szDesc, NULL );
   return pExpr;
}

PZH_EXPR zh_compErrorStatic( ZH_COMP_DECL, const char * szVarName, PZH_EXPR pExpr )
{
   const char * szDesc = zh_compExprDescription( pExpr );

   zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_ILLEGAL_INIT, szVarName, szDesc );
   return pExpr;
}

PZH_EXPR zh_compWarnMeaningless( ZH_COMP_DECL, PZH_EXPR pExpr )
{
   if( ! ZH_COMP_PARAM->fMeaningful )
   {
      const char * szDesc = zh_compExprDescription( pExpr );
      zh_compGenWarning( ZH_COMP_PARAM, zh_comp_szWarnings, 'W', ZH_COMP_WARN_MEANINGLESS, szDesc, NULL );
   }
   return pExpr;
}

void zh_compErrorCodeblockDecl( ZH_COMP_DECL, const char * szVarName )
{
   ZH_BOOL fError = ZH_COMP_PARAM->fError;

   zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_BLOCK, szVarName, NULL );
   ZH_COMP_PARAM->fError = fError; /* restore error flag for this line */
}

void zh_compErrorCodeblockWith( ZH_COMP_DECL, const char * szMessage )
{
   ZH_BOOL fError = ZH_COMP_PARAM->fError;

   zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_WITHOBJECT_MACROBLOCK, szMessage, NULL );
   ZH_COMP_PARAM->fError = fError; /* restore error flag for this line */
}

void zh_compErrorMacro( ZH_COMP_DECL, const char * szText )
{
   zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_BAD_MACRO, szText, NULL );
}

PZH_EXPR zh_compErrorRefer( ZH_COMP_DECL, PZH_EXPR pExpr, const char * szDesc )
{
   zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_INVALID_REFER, szDesc, NULL );
   return pExpr;
}

void zh_compErrorVParams( ZH_COMP_DECL, const char * szFuncOrBlock )
{
   zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_NOT_VPARAMS, szFuncOrBlock, NULL );
}
