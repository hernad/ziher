/*
 * Compiler C source generation
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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
#include "zh_date.h"
#include "zh_assert.h"

static void zh_compGenCReadable( ZH_COMP_DECL, PZH_ZFUNC pFunc, FILE * yyc );
static void zh_compGenCCompact( PZH_ZFUNC pFunc, FILE * yyc );
static void zh_compGenCFunc( FILE * yyc, const char * cDecor, const char * szName, ZH_BOOL fStrip, int iFuncSuffix );
static void zh_writeEndInit( ZH_COMP_DECL, FILE * yyc, const char * szModulname, const char * szSourceFile );

/* helper structure to pass information */
typedef struct ZH_stru_genc_info
{
   ZH_COMP_DECL;
   FILE *  yyc;
   ZH_BOOL bVerbose;
   ZH_SIZE nEndBlockPos;
} ZH_GENC_INFO, * PZH_GENC_INFO;

#define ZH_GENC_FUNC( func )  ZH_PCODE_FUNC( func, PZH_GENC_INFO )
typedef ZH_GENC_FUNC( ZH_GENC_FUNC_ );
typedef ZH_GENC_FUNC_ * PZH_GENC_FUNC;

static void zh_compDumpFindCFunc( ZH_COMP_DECL )
{
   PZH_HINLINE pInline;

   pInline = ZH_COMP_PARAM->inlines.pFirst;
   while( pInline )
   {
      if( pInline->pCode && ! pInline->szName )
      {
         const char * pszCCode = ( const char * ) pInline->pCode;
         char         ch;
         int          len;
         while( ( ch = *pszCCode++ ) != 0 )
         {
            if( ZH_ISFIRSTIDCHAR( ch ) )
            {
               if( ch == 'H' && strncmp( pszCCode, "B_FUNC_STATIC", 13 ) == 0 )
               {
                  pszCCode += 13;
                  while( ZH_ISSPACE( *pszCCode ) )
                     ++pszCCode;
                  if( *pszCCode == '(' )
                  {
                     ++pszCCode;
                     while( ZH_ISSPACE( *pszCCode ) )
                        ++pszCCode;
                     if( ZH_ISFIRSTIDCHAR( *pszCCode ) )
                     {
                        const char * pszName = pszCCode++;

                        while( ZH_ISNEXTIDCHAR( *pszCCode ) )
                           ++pszCCode;
                        len = ( int ) ( pszCCode - pszName );
                        while( ZH_ISSPACE( *pszCCode ) )
                           ++pszCCode;
                        if( *pszCCode == ')' )
                        {
                           char * name = zh_strndup( pszName, len );
                           zh_compFunctionMarkStatic( ZH_COMP_PARAM, name );
                           zh_xfree( name );
                        }
                     }
                  }
               }
               while( ZH_ISNEXTIDCHAR( *pszCCode ) )
                  ++pszCCode;
            }
            else if( ch == '/' && *pszCCode == '*' )
            {
               pszCCode++;
               while( *pszCCode )
               {
                  if( *pszCCode++ == '*' )
                  {
                     if( *pszCCode++ == '/' )
                        break;
                  }
               }
            }
            else if( ch == '/' && *pszCCode == '/' )
            {
               do
               {
                  ++pszCCode;
               }
               while( *pszCCode && *pszCCode != '\n' );
            }
            else if( ch == '"' || ch == '\'' )
            {
               while( *pszCCode )
               {
                  if( *pszCCode == '\\' )
                  {
                     pszCCode++;
                     if( *pszCCode )
                        pszCCode++;
                  }
                  else if( *pszCCode++ == ch )
                     break;
               }
            }
         }
      }
      pInline = pInline->pNext;
   }
}

static void zh_compGenCStdHeaders( ZH_COMP_DECL, FILE * yyc, ZH_BOOL fHbInLine )
{
   fprintf( yyc, "#include \"zh_vm_pub.h\"\n" );

   if( ZH_COMP_PARAM->iGenCOutput != ZH_COMPGENC_COMPACT )
      fprintf( yyc, "#include \"zh_pcode.h\"\n" );

   fprintf( yyc, "#include \"zh_init.h\"\n" );

   if( ZH_COMP_PARAM->iGenCOutput == ZH_COMPGENC_REALCODE )
      fprintf( yyc, "#include \"zh_xvm.h\"\n" );

   if( fHbInLine )
   {
      fprintf( yyc, "#include \"zh_api.h\"\n" );
      fprintf( yyc, "#include \"zh_stack.h\"\n" );
      fprintf( yyc, "#include \"zh_error_api.h\"\n" );
      fprintf( yyc, "#include \"zh_item_api.h\"\n" );
      fprintf( yyc, "#include \"zh_vm.h\"\n" );
      fprintf( yyc, "#include \"zh_class_api.h\"\n" );
      fprintf( yyc, "#include \"oo.zhh\"\n" );
   }
}

static void zh_compFuncUsed( ZH_COMP_DECL, PZH_HSYMBOL pSym )
{
   if( ( pSym->cScope & ZH_FS_USED ) == 0 )
      zh_compGenWarning( ZH_COMP_PARAM, zh_comp_szWarnings, 'W', ZH_COMP_WARN_STATIC_FUNC_UNUSED, pSym->szName, NULL );
}

void zh_compGenCCode( ZH_COMP_DECL, PZH_FNAME pFileName )       /* generates the C language output */
{
   char        szFileName[ ZH_PATH_MAX ];
   PZH_HSYMBOL pSym;
   PZH_ZFUNC   pFunc;
   PZH_HINLINE pInline;
   FILE *      yyc; /* file handle for C output */
   ZH_BOOL     fHasHbInline = ZH_FALSE;

   zh_fsFNameMerge( szFileName, pFileName );
   if( ! pFileName->szExtension )
      pFileName->szExtension = ".c";
   zh_fsFNameMerge( szFileName, pFileName );

   yyc = zh_fopen( szFileName, "w" );
   if( ! yyc )
   {
      zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_CREATE_OUTPUT, szFileName, NULL );
      return;
   }

   if( ! ZH_COMP_PARAM->fQuiet )
   {
      char buffer[ 80 + ZH_PATH_MAX - 1 ];
      zh_snprintf( buffer, sizeof( buffer ),
                   "Generating C source output to \'%s\'... ", szFileName );
      zh_compOutStd( ZH_COMP_PARAM, buffer );
   }

   if( ZH_COMP_PARAM->fDebugInfo )
   {
      char * szCmp = zh_verCompiler();
      char * szHrb = zh_verZiher();
      char * szPcd = zh_verPCode();

      fprintf( yyc, "/*\n * %s\n", szHrb );
      fprintf( yyc, " * %s\n", szCmp );
      fprintf( yyc, " * %s\n", szPcd );
      fprintf( yyc, " * Generated C source from \"%s\"\n */\n\n", ZH_COMP_PARAM->szFile );

      zh_xfree( szPcd );
      zh_xfree( szHrb );
      zh_xfree( szCmp );
   }
   else {
      fprintf( yyc, "/* C source generated by Ziher */\n\n" );
      //fprintf( yyc, "#define ZH_DYNLIB\n\n" );
   }

   pFunc = ZH_COMP_PARAM->functions.pFirst;
   while( pFunc &&
          ( ( pFunc->funFlags & ZH_FUNF_FILE_DECL ) != 0 ||
            pFunc == ZH_COMP_PARAM->pInitFunc ||
            pFunc == ZH_COMP_PARAM->pLineFunc ) )
      pFunc = pFunc->pNext;

   if( pFunc )
   {
      int iFuncSuffix;

      zh_compDumpFindCFunc( ZH_COMP_PARAM );

      pInline = ZH_COMP_PARAM->inlines.pFirst;
      while( pInline )
      {
         if( pInline->szName )
         {
            fHasHbInline = ZH_TRUE;
            break;
         }
         pInline = pInline->pNext;
      }

      zh_compGenCStdHeaders( ZH_COMP_PARAM, yyc, fHasHbInline );

      /* write functions prototypes */
      pSym = ZH_COMP_PARAM->symbols.pFirst;

      if( pSym )
         fprintf( yyc, "\n" );

      while( pSym )
      {
         if( pSym->iFunc )
         {
            if( pSym->szName[ 0 ] == '(' )
            {
               fprintf( yyc, "ZH_FUNC_INIT%s();\n",
                        ! memcmp( pSym->szName + 1, "_INITLINES", 10 ) ?
                        "LINES" : "STATICS" );
            }
            else if( pSym->cScope & ZH_FS_LOCAL ) /* is it a function defined in this module */
            {
               iFuncSuffix = pSym->pFunc ? pSym->pFunc->iFuncSuffix : 0;
               if( pSym->cScope & ZH_FS_INIT )
                  zh_compGenCFunc( yyc, "ZH_FUNC_INIT( %s );\n", pSym->szName, ZH_TRUE, iFuncSuffix );
               else if( pSym->cScope & ZH_FS_EXIT )
                  zh_compGenCFunc( yyc, "ZH_FUNC_EXIT( %s );\n", pSym->szName, ZH_TRUE, iFuncSuffix );
               else if( pSym->cScope & ZH_FS_STATIC )
               {
                  zh_compGenCFunc( yyc, "ZH_FUNC_STATIC( %s );\n", pSym->szName, ZH_FALSE, iFuncSuffix );
                  zh_compFuncUsed( ZH_COMP_PARAM, pSym );
               }
               else
                  zh_compGenCFunc( yyc, "ZH_FUNC( %s );\n", pSym->szName, ZH_FALSE, iFuncSuffix );
            }
            else if( ( pSym->cScope & ZH_FS_DEFERRED ) == 0 ) /* it's not a function declared as dynamic */
               zh_compGenCFunc( yyc, "ZH_FUNC_EXTERN( %s );\n", pSym->szName, ZH_FALSE, 0 );
         }
         pSym = pSym->pNext;
      }

      /* writes the symbol table */
      /* Generate the wrapper that will initialize local symbol table
       */
      zh_strncpyUpper( szFileName, pFileName->szName, sizeof( szFileName ) - 1 );
      /* replace non ID characters in name of local symbol table by '_' */
      {
         int iLen = ( int ) strlen( szFileName ), i;

         for( i = 0; i < iLen; i++ )
         {
            char c = szFileName[ i ];
            if( ! ZH_ISNEXTIDCHAR( c ) )
               szFileName[ i ] = '_';
         }
      }
      fprintf( yyc, "\nZH_INIT_SYMBOLS_BEGIN( zh_vm_SymbolInit_%s )\n", szFileName );

      pSym = ZH_COMP_PARAM->symbols.pFirst;
      while( pSym )
      {
         if( pSym->szName[ 0 ] == '(' )
         {
            /* Since the normal function cannot be INIT and EXIT at the same time
             * we are using these two bits to mark the special function used to
             * initialize static variables or debugging info about valid stop lines
             */
            fprintf( yyc, "{ \"%s\", { ZH_FS_INITEXIT | ZH_FS_LOCAL }, { zh_INIT%s }, NULL }",
                     pSym->szName, ! memcmp( pSym->szName + 1, "_INITLINES", 10 ) ?
                     "LINES" : "STATICS" ); /* NOTE: "zh_" intentionally in lower case */
         }
         else
         {
            fprintf( yyc, "{ \"%s\", { ", pSym->szName );

            if( pSym->cScope & ZH_FS_STATIC )
               fprintf( yyc, "ZH_FS_STATIC" );
            else if( pSym->cScope & ZH_FS_INIT )
               fprintf( yyc, "ZH_FS_INIT" );
            else if( pSym->cScope & ZH_FS_EXIT )
               fprintf( yyc, "ZH_FS_EXIT" );
            else
               fprintf( yyc, "ZH_FS_PUBLIC" );

            if( pSym->cScope & ZH_VSCOMP_MEMVAR )
               fprintf( yyc, " | ZH_FS_MEMVAR" );

            if( pSym->cScope & ZH_FS_MESSAGE )
               fprintf( yyc, " | ZH_FS_MESSAGE" );

            if( ( pSym->cScope & ZH_FS_FIRST ) && ( ! ZH_COMP_PARAM->fNoStartUp ) )
               fprintf( yyc, " | ZH_FS_FIRST" );

            /* specify the function address if it is a defined function or an
               external called function */
            if( pSym->cScope & ZH_FS_LOCAL ) /* is it a function defined in this module */
            {
               fprintf( yyc, " | ZH_FS_LOCAL" );

               iFuncSuffix = pSym->pFunc ? pSym->pFunc->iFuncSuffix : 0;
               if( pSym->cScope & ZH_FS_INIT )
                  zh_compGenCFunc( yyc, " }, { ZH_INIT_FUNCNAME( %s ) }, NULL }", pSym->szName, ZH_TRUE, iFuncSuffix );
               else if( pSym->cScope & ZH_FS_EXIT )
                  zh_compGenCFunc( yyc, " }, { ZH_EXIT_FUNCNAME( %s ) }, NULL }", pSym->szName, ZH_TRUE, iFuncSuffix );
               else
                  zh_compGenCFunc( yyc, " }, { ZH_FUNCNAME( %s ) }, NULL }", pSym->szName, ZH_FALSE, iFuncSuffix );
            }
            else if( pSym->cScope & ZH_FS_DEFERRED ) /* is it a function declared as dynamic */
               fprintf( yyc, " | ZH_FS_DEFERRED }, { NULL }, NULL }" );
            else if( pSym->iFunc )                   /* is it a function called from this module */
               zh_compGenCFunc( yyc, " }, { ZH_FUNCNAME( %s ) }, NULL }", pSym->szName, ZH_FALSE, 0 );
            else
               fprintf( yyc, " }, { NULL }, NULL }" );   /* memvar | alias | message */
         }

         if( pSym != ZH_COMP_PARAM->symbols.pLast )
            fprintf( yyc, ",\n" );

         pSym = pSym->pNext;
      }

      zh_writeEndInit( ZH_COMP_PARAM, yyc, szFileName, ZH_COMP_PARAM->szFile );

      /* Generate functions data
       */
      pFunc = ZH_COMP_PARAM->functions.pFirst;
      while( pFunc )
      {
         if( ( pFunc->funFlags & ZH_FUNF_FILE_DECL ) == 0 )
         {
            fprintf( yyc, "\n" );

            /* Is it _STATICS$ - static initialization function */
            if( pFunc == ZH_COMP_PARAM->pInitFunc )
               fprintf( yyc, "ZH_FUNC_INITSTATICS()\n" );
            /* Is it an (_INITLINES) function */
            else if( pFunc == ZH_COMP_PARAM->pLineFunc )
               fprintf( yyc, "ZH_FUNC_INITLINES()\n" );
            /* Is it an INIT FUNCTION/PROCEDURE */
            else if( pFunc->cScope & ZH_FS_INIT )
               zh_compGenCFunc( yyc, "ZH_FUNC_INIT( %s )\n", pFunc->szName, ZH_TRUE, pFunc->iFuncSuffix );
            /* Is it an EXIT FUNCTION/PROCEDURE */
            else if( pFunc->cScope & ZH_FS_EXIT )
               zh_compGenCFunc( yyc, "ZH_FUNC_EXIT( %s )\n", pFunc->szName, ZH_TRUE, pFunc->iFuncSuffix );
            /* Is it a STATIC FUNCTION/PROCEDURE */
            else if( pFunc->cScope & ZH_FS_STATIC )
               zh_compGenCFunc( yyc, "ZH_FUNC_STATIC( %s )\n", pFunc->szName, ZH_FALSE, pFunc->iFuncSuffix );
            else /* Then it must be PUBLIC FUNCTION/PROCEDURE */
               zh_compGenCFunc( yyc, "ZH_FUNC( %s )\n", pFunc->szName, ZH_FALSE, pFunc->iFuncSuffix );

            if( ZH_COMP_PARAM->iGenCOutput == ZH_COMPGENC_REALCODE )
               zh_compGenCRealCode( ZH_COMP_PARAM, pFunc, yyc );
            else
            {
               if( ZH_COMP_PARAM->iGenCOutput == ZH_COMPGENC_COMPACT )
                  zh_compGenCCompact( pFunc, yyc );
               else
                  zh_compGenCReadable( ZH_COMP_PARAM, pFunc, yyc );
            }
         }
         pFunc = pFunc->pNext;
      }

      /* Generate C inline functions
       */
      pInline = ZH_COMP_PARAM->inlines.pFirst;

      if( pInline )
         fprintf( yyc, "\n" );

      while( pInline )
      {
         if( pInline->pCode )
         {
            fprintf( yyc, "#line %i ", pInline->iLine );
            zh_compGenCString( yyc, ( const ZH_BYTE * ) pInline->szFileName,
                               strlen( pInline->szFileName ) );
            fprintf( yyc, "\n" );

            if( pInline->szName )
               zh_compGenCFunc( yyc, "ZH_FUNC_STATIC( %s )\n", pInline->szName, ZH_FALSE, 0 );

            fprintf( yyc, "%s", pInline->pCode );
         }
         pInline = pInline->pNext;
      }
   }
   else
   {
      pInline = ZH_COMP_PARAM->inlines.pFirst;
      while( pInline )
      {
         if( pInline->pCode )
         {
            if( ! fHasHbInline )
            {
               zh_compGenCStdHeaders( ZH_COMP_PARAM, yyc, ZH_FALSE );
               fprintf( yyc, "\n" );
               fHasHbInline = ZH_TRUE;
            }
            fprintf( yyc, "#line %i ", pInline->iLine );
            zh_compGenCString( yyc, ( const ZH_BYTE * ) pInline->szFileName,
                               strlen( pInline->szFileName ) );
            fprintf( yyc, "\n" );

            if( pInline->szName )
               zh_compGenCFunc( yyc, "ZH_FUNC_STATIC( %s )\n", pInline->szName, ZH_FALSE, 0 );

            fprintf( yyc, "%s", pInline->pCode );
         }
         pInline = pInline->pNext;
      }
      if( ! fHasHbInline )
         fprintf( yyc, "static const void * dummy = &dummy;  /* Empty source file */\n" );
   }

   fclose( yyc );

   if( ! ZH_COMP_PARAM->fQuiet )
      zh_compOutStd( ZH_COMP_PARAM, "Done.\n" );
}

static void zh_writeEndInit( ZH_COMP_DECL, FILE * yyc, const char * szModulname, const char * szSourceFile )
{
   fprintf( yyc, "\nZH_INIT_SYMBOLS_EX_END( zh_vm_SymbolInit_%s, ", szModulname );
   //hernad
   //if( ZH_COMP_PARAM->fHideSource )
   //   szSourceFile = "";
   zh_compGenCString( yyc, ( const ZH_BYTE * ) szSourceFile, strlen( szSourceFile ) );
   fprintf( yyc, ", 0x%lx, 0x%04x )\n\n", 0L, ZH_PCODE_VER );

   fprintf( yyc,
            "#if defined( ZH_DATASEG_STARTUP )\n"
            //"#pragma GCC diagnostic warning \"ZHDATASEG_STARTUP\"\n"
            "   #define ZH_DATASEG_BODY    ZH_DATASEG_FUNC( zh_vm_SymbolInit_%s )\n"
            "   #include \"zh_ini_seg.h\"\n"
            //"#else\n"
            //"#pragma GCC diagnostic warning \"NODEFINED_STARTUP\"\n"
            "#endif\n",
            szModulname, szModulname );
}

static void zh_compGenCFunc( FILE * yyc, const char * cDecor, const char * szName,
                             ZH_BOOL fStrip, int iFuncSuffix )
{
   int i = 0;

   while( cDecor[ i ] )
   {
      if( cDecor[ i ] == '%' && cDecor[ i + 1 ] == 's' )
      {
         const char * tmp = szName;
         char         c;

         while( ( c = *tmp++ ) != 0 )
         {
            if( ZH_ISNEXTIDCHAR( c ) )
               fputc( ( ZH_UCHAR ) c, yyc );
            else if( ! fStrip || c != '$' || *tmp != 0 )
            {
               /* 'x' is used to force unique name and eliminate possible
                * collisions with other function names.
                */
               fprintf( yyc, "x%02x", ( ZH_UCHAR ) c );
            }
         }
         if( iFuncSuffix )
            fprintf( yyc, "v%d", iFuncSuffix );
         i += 2;
      }
      else
      {
         fputc( ( ZH_UCHAR ) cDecor[ i ], yyc );
         i++;
      }
   }
}

static void zh_compGenCByteStr( FILE * yyc, const ZH_BYTE * pText, ZH_SIZE nLen )
{
   ZH_SIZE nPos;

   for( nPos = 0; nPos < nLen; nPos++ )
   {
      ZH_BYTE uchr = ( ZH_BYTE ) pText[ nPos ];
      /*
       * NOTE: After optimization some Chr( n ) can be converted
       *    into a string containing non-printable characters.
       *
       * TODO: add switch to use hexadecimal format "%#04x"
       */
      fprintf( yyc, ( uchr < ( ZH_BYTE ) ' ' || uchr >= 127 || uchr == '\\' ||
                      uchr == '\'' ) ? "%i, " : "\'%c\', ", uchr );
   }
}

static void zh_compGenCLocalName( PZH_ZFUNC pFunc, int iLocal, ZH_SIZE nPCodePos, PZH_GENC_INFO cargo )
{
   /* Variable with negative order are local variables
    * referenced in a codeblock -handle it with care
    */

   if( cargo->nEndBlockPos > nPCodePos )
   {
      /* we are accessing variables within a codeblock */
      /* the names of codeblock variable are lost     */
      if( iLocal < 0 )
         fprintf( cargo->yyc, "\t/* localvar%i */", -iLocal );
      else
         fprintf( cargo->yyc, "\t/* codeblockvar%i */", iLocal );
   }
   else
   {
      const char * szName = zh_compLocalVariableName( pFunc, ( ZH_USHORT ) iLocal );

      if( szName )
         fprintf( cargo->yyc, "\t/* %s */", szName );
      else
         fprintf( cargo->yyc, "\t/* localvar%i */", iLocal );
   }
}

static void zh_compGenCStaticName( ZH_USHORT uiStatic, PZH_GENC_INFO cargo )
{
   const char * szName = zh_compStaticVariableName( cargo->ZH_COMP_PARAM, uiStatic );

   if( szName )
      fprintf( cargo->yyc, "\t/* %s */", szName );
   else
      fprintf( cargo->yyc, "\t/* staticvar%hu */", uiStatic );
}

static ZH_GENC_FUNC( zh_p_and )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_AND,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_arraypush )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_ARRAYPUSH,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_arraypushref )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_ARRAYPUSHREF,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_arraypop )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_ARRAYPOP,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_dec )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_DEC,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_arraydim )
{
   fprintf( cargo->yyc, "\tZH_P_ARRAYDIM, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %u */", ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static ZH_GENC_FUNC( zh_p_divide )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_DIVIDE,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_do )
{
   fprintf( cargo->yyc, "\tZH_P_DO, %u, %u,\n",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   return 3;
}

static ZH_GENC_FUNC( zh_p_doshort )
{
   fprintf( cargo->yyc, "\tZH_P_DOSHORT, %u,\n", pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static ZH_GENC_FUNC( zh_p_duplicate )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_DUPLICATE,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_duplunref )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_DUPLUNREF,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_pushunref )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_PUSHUNREF,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_swap )
{
   fprintf( cargo->yyc, "\tZH_P_SWAP, %u,\n", pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static ZH_GENC_FUNC( zh_p_equal )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_EQUAL,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_exactlyequal )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_EXACTLYEQUAL,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_endblock )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_ENDBLOCK,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_endproc )
{
   if( nPCodePos + 1 == pFunc->nPCodePos )
      fprintf( cargo->yyc, "\tZH_P_ENDPROC\n" );
   else
      fprintf( cargo->yyc, "\tZH_P_ENDPROC,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_false )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_FALSE,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_fortest )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_FORTEST,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_frame )
{
   fprintf( cargo->yyc, "\tZH_P_FRAME, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* locals, params */" );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static ZH_GENC_FUNC( zh_p_funcptr )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_FUNCPTR,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_function )
{
   fprintf( cargo->yyc, "\tZH_P_FUNCTION, %u, %u,\n",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   return 3;
}

static ZH_GENC_FUNC( zh_p_functionshort )
{
   fprintf( cargo->yyc, "\tZH_P_FUNCTIONSHORT, %u,\n", pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static ZH_GENC_FUNC( zh_p_arraygen )
{
   fprintf( cargo->yyc, "\tZH_P_ARRAYGEN, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %u */", ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static ZH_GENC_FUNC( zh_p_hashgen )
{
   fprintf( cargo->yyc, "\tZH_P_HASHGEN, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %u */", ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static ZH_GENC_FUNC( zh_p_greater )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_GREATER,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_greaterequal )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_GREATEREQUAL,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_inc )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_INC,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_instring )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_INSTRING,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_jumpnear )
{
   fprintf( cargo->yyc, "\tZH_P_JUMPNEAR, %u,",
            pFunc->pCode[ nPCodePos + 1 ] );
   if( cargo->bVerbose )
   {
      ZH_I_SIZE nOffset = ( signed char ) ( pFunc->pCode[ nPCodePos + 1 ] );

      fprintf( cargo->yyc, "\t/* %" ZH_PFS "i (abs: %05" ZH_PFS "i) */", nOffset, ( ZH_I_SIZE ) ( nPCodePos + nOffset ) );
   }
   fprintf( cargo->yyc, "\n" );
   return 2;
}

static ZH_GENC_FUNC( zh_p_jump )
{
   fprintf( cargo->yyc, "\tZH_P_JUMP, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
   {
      ZH_I_SIZE nOffset = ZH_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] );

      fprintf( cargo->yyc, "\t/* %" ZH_PFS "i (abs: %05" ZH_PFS "i) */", nOffset, ( ZH_I_SIZE ) ( nPCodePos + nOffset ) );
   }
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static ZH_GENC_FUNC( zh_p_jumpfar )
{
   fprintf( cargo->yyc, "\tZH_P_JUMPFAR, %u, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ] );
   if( cargo->bVerbose )
   {
      ZH_I_SIZE nOffset = ZH_PCODE_MKINT24( &pFunc->pCode[ nPCodePos + 1 ] );
      fprintf( cargo->yyc, "\t/* %" ZH_PFS "i (abs: %08" ZH_PFS "i) */", nOffset, ( ZH_I_SIZE ) ( nPCodePos + nOffset ) );
   }
   fprintf( cargo->yyc, "\n" );
   return 4;
}

static ZH_GENC_FUNC( zh_p_jumpfalsenear )
{
   fprintf( cargo->yyc, "\tZH_P_JUMPFALSENEAR, %u,",
            pFunc->pCode[ nPCodePos + 1 ] );
   if( cargo->bVerbose )
   {
      ZH_I_SIZE nOffset = ( signed char ) ( pFunc->pCode[ nPCodePos + 1 ] );
      fprintf( cargo->yyc, "\t/* %" ZH_PFS "i (abs: %05" ZH_PFS "i) */", nOffset, ( ZH_I_SIZE ) ( nPCodePos + nOffset ) );
   }
   fprintf( cargo->yyc, "\n" );
   return 2;
}

static ZH_GENC_FUNC( zh_p_jumpfalse )
{
   fprintf( cargo->yyc, "\tZH_P_JUMPFALSE, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
   {
      ZH_I_SIZE nOffset = ZH_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] );
      fprintf( cargo->yyc, "\t/* %" ZH_PFS "i (abs: %05" ZH_PFS "i) */", nOffset, ( ZH_I_SIZE ) ( nPCodePos + nOffset ) );
   }
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static ZH_GENC_FUNC( zh_p_jumpfalsefar )
{
   fprintf( cargo->yyc, "\tZH_P_JUMPFALSEFAR, %u, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ] );
   if( cargo->bVerbose )
   {
      ZH_I_SIZE nOffset = ZH_PCODE_MKINT24( &pFunc->pCode[ nPCodePos + 1 ] );
      fprintf( cargo->yyc, "\t/* %" ZH_PFS "i (abs: %08" ZH_PFS "i) */", nOffset, ( ZH_I_SIZE ) ( nPCodePos + nOffset ) );
   }
   fprintf( cargo->yyc, "\n" );
   return 4;
}

static ZH_GENC_FUNC( zh_p_jumptruenear )
{
   fprintf( cargo->yyc, "\tZH_P_JUMPTRUENEAR, %u,",
            pFunc->pCode[ nPCodePos + 1 ] );
   if( cargo->bVerbose )
   {
      ZH_I_SIZE nOffset = ( signed char ) ( pFunc->pCode[ nPCodePos + 1 ] );
      fprintf( cargo->yyc, "\t/* %" ZH_PFS "i (abs: %05" ZH_PFS "i) */", nOffset, ( ZH_I_SIZE ) ( nPCodePos + nOffset ) );
   }
   fprintf( cargo->yyc, "\n" );
   return 2;
}

static ZH_GENC_FUNC( zh_p_jumptrue )
{
   fprintf( cargo->yyc, "\tZH_P_JUMPTRUE, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
   {
      ZH_I_SIZE nOffset = ZH_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] );
      fprintf( cargo->yyc, "\t/* %" ZH_PFS "i (abs: %05" ZH_PFS "i) */", nOffset, ( ZH_I_SIZE ) ( nPCodePos + nOffset ) );
   }
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static ZH_GENC_FUNC( zh_p_jumptruefar )
{
   fprintf( cargo->yyc, "\tZH_P_JUMPTRUEFAR, %u, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ] );
   if( cargo->bVerbose )
   {
      ZH_I_SIZE nOffset = ZH_PCODE_MKINT24( &pFunc->pCode[ nPCodePos + 1 ] );
      fprintf( cargo->yyc, "\t/* %" ZH_PFS "i (abs: %08" ZH_PFS "i) */", nOffset, ( ZH_I_SIZE ) ( nPCodePos + nOffset ) );
   }
   fprintf( cargo->yyc, "\n" );
   return 4;
}

static ZH_GENC_FUNC( zh_p_less )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_LESS,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_lessequal )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_LESSEQUAL,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_line )
{
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "/* %05" ZH_PFS "u */ ", nPCodePos );
   else
      fprintf( cargo->yyc, "\t" );
   fprintf( cargo->yyc, "ZH_P_LINE, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %u */", ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static ZH_GENC_FUNC( zh_p_localname )
{
   ZH_SIZE nStart = nPCodePos;

   fprintf( cargo->yyc, "\tZH_P_LOCALNAME, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", ( char * ) pFunc->pCode + nPCodePos + 3 );
   fprintf( cargo->yyc, "\n" );
   nPCodePos += 3;
   while( pFunc->pCode[ nPCodePos ] )
   {
      char chr = pFunc->pCode[ nPCodePos++ ];
      if( chr == '\'' || chr == '\\' )
         fprintf( cargo->yyc, " \'\\%c\',", chr );
      else
         fprintf( cargo->yyc, " \'%c\',", chr );
   }
   fprintf( cargo->yyc, " 0,\n" );

   return nPCodePos - nStart + 1;
}

static ZH_GENC_FUNC( zh_p_macropop )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_MACROPOP, %u,\n", pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static ZH_GENC_FUNC( zh_p_macropopaliased )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_MACROPOPALIASED, %u,\n", pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static ZH_GENC_FUNC( zh_p_macropush )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_MACRO_PUSH, %u,\n", pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static ZH_GENC_FUNC( zh_p_macropushref )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_MACRO_PUSHREF,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_macrodo )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_MACRODO, %u, %u,\n",
            pFunc->pCode[ nPCodePos + 1 ], pFunc->pCode[ nPCodePos + 2 ] );
   return 3;
}

static ZH_GENC_FUNC( zh_p_macro_func )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_MACRO_FUNC, %u, %u,\n",
            pFunc->pCode[ nPCodePos + 1 ], pFunc->pCode[ nPCodePos + 2 ] );
   return 3;
}

static ZH_GENC_FUNC( zh_p_macrosend )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_MACRO_SEND, %u, %u,\n",
            pFunc->pCode[ nPCodePos + 1 ], pFunc->pCode[ nPCodePos + 2 ] );
   return 3;
}

static ZH_GENC_FUNC( zh_p_macro_array_gen )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_MACRO_ARRAY_GEN, %u, %u,\n",
            pFunc->pCode[ nPCodePos + 1 ], pFunc->pCode[ nPCodePos + 2 ] );
   return 3;
}

static ZH_GENC_FUNC( zh_p_macropushlist )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_MACRO_PUSHLIST, %u,\n", pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static ZH_GENC_FUNC( zh_p_macropushindex )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_MACRO_PUSHINDEX,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_macropushpare )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_MACRO_PUSHPARE, %u,\n", pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static ZH_GENC_FUNC( zh_p_macropushaliased )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_MACRO_PUSHALIASED, %u,\n", pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static ZH_GENC_FUNC( zh_p_macrosymbol )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_MACROSYMBOL,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_macrotext )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_MACROTEXT,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_message )
{
   fprintf( cargo->yyc, "\tZH_P_MESSAGE, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", zh_compSymbolName( cargo->ZH_COMP_PARAM, ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static ZH_GENC_FUNC( zh_p_minus )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_MINUS,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_modulename )
{
   ZH_SIZE nStart = nPCodePos;

   fprintf( cargo->yyc, "\tZH_P_MODULENAME," );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", ( char * ) pFunc->pCode + nPCodePos + 1 );
   fprintf( cargo->yyc, "\n" );
   nPCodePos++;
   while( pFunc->pCode[ nPCodePos ] )
   {
      char chr = pFunc->pCode[ nPCodePos++ ];
      if( chr == '\'' || chr == '\\' )
         fprintf( cargo->yyc, " \'\\%c\',", chr );
      else
         fprintf( cargo->yyc, " \'%c\',", chr );
   }
   fprintf( cargo->yyc, " 0,\n" );

   return nPCodePos - nStart + 1;
}

static ZH_GENC_FUNC( zh_p_modulus )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_MODULUS,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_mult )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_MULT,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_negate )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_NEGATE,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_not )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_NOT,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_notequal )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_NOTEQUAL,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_or )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_OR,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_parameter )
{
   fprintf( cargo->yyc, "\tZH_P_PARAMETER, %u, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", zh_compSymbolName( cargo->ZH_COMP_PARAM, ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) ) );
   fprintf( cargo->yyc, "\n" );
   return 4;
}

static ZH_GENC_FUNC( zh_p_plus )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_PLUS,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_pop )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_POP,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_popalias )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_POPALIAS,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_popaliasedfield )
{
   fprintf( cargo->yyc, "\tZH_P_POPALIASEDFIELD, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", zh_compSymbolName( cargo->ZH_COMP_PARAM, ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static ZH_GENC_FUNC( zh_p_popaliasedfieldnear )
{
   fprintf( cargo->yyc, "\tZH_P_POPALIASEDFIELDNEAR, %u,",
            pFunc->pCode[ nPCodePos + 1 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", zh_compSymbolName( cargo->ZH_COMP_PARAM, pFunc->pCode[ nPCodePos + 1 ] ) );
   fprintf( cargo->yyc, "\n" );
   return 2;
}

static ZH_GENC_FUNC( zh_p_popaliasedvar )
{
   fprintf( cargo->yyc, "\tZH_P_POPALIASEDVAR, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", zh_compSymbolName( cargo->ZH_COMP_PARAM, ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static ZH_GENC_FUNC( zh_p_popfield )
{
   fprintf( cargo->yyc, "\tZH_P_POPFIELD, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", zh_compSymbolName( cargo->ZH_COMP_PARAM, ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static ZH_GENC_FUNC( zh_p_poplocal )
{
   fprintf( cargo->yyc, "\tZH_P_POPLOCAL, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
   {
      int iVar = ZH_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] );
      zh_compGenCLocalName( pFunc, iVar, nPCodePos, cargo );
   }
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static ZH_GENC_FUNC( zh_p_poplocalnear )
{
   fprintf( cargo->yyc, "\tZH_P_POPLOCALNEAR, %u,",
            pFunc->pCode[ nPCodePos + 1 ] );
   if( cargo->bVerbose )
   {
      int iVar = ( signed char ) pFunc->pCode[ nPCodePos + 1 ];
      zh_compGenCLocalName( pFunc, iVar, nPCodePos, cargo );
   }
   fprintf( cargo->yyc, "\n" );
   return 2;
}

static ZH_GENC_FUNC( zh_p_popmemvar )
{
   fprintf( cargo->yyc, "\tZH_P_POPMEMVAR, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", zh_compSymbolName( cargo->ZH_COMP_PARAM, ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static ZH_GENC_FUNC( zh_p_popstatic )
{
   fprintf( cargo->yyc, "\tZH_P_POPSTATIC, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      zh_compGenCStaticName( ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ), cargo );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static ZH_GENC_FUNC( zh_p_popvar )
{
   fprintf( cargo->yyc, "\tZH_P_POPVARIABLE, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", zh_compSymbolName( cargo->ZH_COMP_PARAM, ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static ZH_GENC_FUNC( zh_p_power )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_POWER,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_pushalias )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_PUSHALIAS,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_pushaliasedfield )
{
   fprintf( cargo->yyc, "\tZH_P_PUSH_ALIASED_FIELD, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", zh_compSymbolName( cargo->ZH_COMP_PARAM, ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static ZH_GENC_FUNC( zh_p_pushaliasedfieldnear )
{
   fprintf( cargo->yyc, "\tZH_P_PUSH_ALIASED_FIELDNEAR, %u,",
            pFunc->pCode[ nPCodePos + 1 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", zh_compSymbolName( cargo->ZH_COMP_PARAM, pFunc->pCode[ nPCodePos + 1 ] ) );
   fprintf( cargo->yyc, "\n" );
   return 2;
}

static ZH_GENC_FUNC( zh_p_pushaliasedvar )
{
   fprintf( cargo->yyc, "\tZH_P_PUSHALIASEDVAR, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", zh_compSymbolName( cargo->ZH_COMP_PARAM, ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static ZH_GENC_FUNC( zh_p_push_blockshort )
{
   fprintf( cargo->yyc, "\tZH_P_PUSH_BLOCKSHORT, %u,",
            pFunc->pCode[ nPCodePos + 1 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %u */",
               pFunc->pCode[ nPCodePos + 1 ] );
   fprintf( cargo->yyc, "\n" );

   if( cargo->nEndBlockPos < nPCodePos )
      cargo->nEndBlockPos = nPCodePos + pFunc->pCode[ nPCodePos + 1 ] - 1;

   return 2;
}

static ZH_GENC_FUNC( zh_p_push_block )
{
   ZH_USHORT wVar, w;
   ZH_SIZE   nStart = nPCodePos;

   fprintf( cargo->yyc, "\tZH_P_PUSH_BLOCK, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %u */",
               ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   fprintf( cargo->yyc, "\n" );

   w = ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 3 ] );
   fprintf( cargo->yyc, "\t%u, %u,",
            pFunc->pCode[ nPCodePos + 3 ],
            pFunc->pCode[ nPCodePos + 4 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* number of local parameters (%u) */", w );
   fprintf( cargo->yyc, "\n" );

   wVar = ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 5 ] );
   fprintf( cargo->yyc, "\t%u, %u,",
            pFunc->pCode[ nPCodePos + 5 ],
            pFunc->pCode[ nPCodePos + 6 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* number of local variables (%u) */", wVar );
   fprintf( cargo->yyc, "\n" );

   nPCodePos += 7;  /* codeblock size + number of parameters + number of local variables */
   /* create the table of referenced local variables */
   while( wVar-- )
   {
      fprintf( cargo->yyc, "\t%u, %u,",
               pFunc->pCode[ nPCodePos ],
               pFunc->pCode[ nPCodePos + 1 ] );
      /* NOTE:
       * When a codeblock is used to initialize a static variable
       * the names of local variables cannot be determined
       * because at the time of C code generation we don't know
       * in which function was defined this local variable
       */
      if( cargo->bVerbose && ( pFunc->cScope & ZH_FS_INITEXIT ) != ZH_FS_INITEXIT )
      {
         w = ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos ] );
         zh_compGenCLocalName( pFunc, w, nPCodePos, cargo );
      }
      fprintf( cargo->yyc, "\n" );
      nPCodePos += 2;
   }

   if( cargo->nEndBlockPos < nStart )
      cargo->nEndBlockPos = nStart + ZH_PCODE_MKUSHORT( &pFunc->pCode[ nStart + 1 ] ) - 1;

   return nPCodePos - nStart;
}

static ZH_GENC_FUNC( zh_p_push_blocklarge )
{
   ZH_USHORT wVar, w;
   ZH_SIZE   nStart = nPCodePos;

   fprintf( cargo->yyc, "\tZH_P_PUSH_BLOCKLARGE, %u, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %lu */",
               ZH_PCODE_MKUINT24( &pFunc->pCode[ nPCodePos + 1 ] ) );
   fprintf( cargo->yyc, "\n" );

   w = ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 4 ] );
   fprintf( cargo->yyc, "\t%u, %u,",
            pFunc->pCode[ nPCodePos + 4 ],
            pFunc->pCode[ nPCodePos + 5 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* number of local parameters (%u) */", w );
   fprintf( cargo->yyc, "\n" );

   wVar = ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 6 ] );
   fprintf( cargo->yyc, "\t%u, %u,",
            pFunc->pCode[ nPCodePos + 6 ],
            pFunc->pCode[ nPCodePos + 7 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* number of local variables (%u) */", wVar );
   fprintf( cargo->yyc, "\n" );

   nPCodePos += 8;  /* codeblock size + number of parameters + number of local variables */
   /* create the table of referenced local variables */
   while( wVar-- )
   {
      fprintf( cargo->yyc, "\t%u, %u,",
               pFunc->pCode[ nPCodePos ],
               pFunc->pCode[ nPCodePos + 1 ] );
      /* NOTE:
       * When a codeblock is used to initialize a static variable
       * the names of local variables cannot be determined
       * because at the time of C code generation we don't know
       * in which function was defined this local variable
       */
      if( cargo->bVerbose && ( pFunc->cScope & ZH_FS_INITEXIT ) != ZH_FS_INITEXIT )
      {
         w = ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos ] );
         zh_compGenCLocalName( pFunc, w, nPCodePos, cargo );
      }
      fprintf( cargo->yyc, "\n" );
      nPCodePos += 2;
   }

   if( cargo->nEndBlockPos < nStart )
      cargo->nEndBlockPos = nStart + ZH_PCODE_MKUINT24( &pFunc->pCode[ nStart + 1 ] ) - 1;

   return nPCodePos - nStart;
}

static ZH_GENC_FUNC( zh_p_pushdouble )
{
   int i;

   fprintf( cargo->yyc, "\tZH_P_PUSHDOUBLE," );
   ++nPCodePos;
   for( i = 0; i < ( int ) ( sizeof( double ) + sizeof( ZH_BYTE ) + sizeof( ZH_BYTE ) ); ++i )
   {
      fprintf( cargo->yyc, " %u,", ( ZH_UCHAR ) pFunc->pCode[ nPCodePos + i ] );
   }
   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %.*f, %u, %u */",
               ( ZH_UCHAR ) pFunc->pCode[ nPCodePos + sizeof( double ) + sizeof( ZH_BYTE ) ],
               ZH_PCODE_MKDOUBLE( &pFunc->pCode[ nPCodePos ] ),
               ( ZH_UCHAR ) pFunc->pCode[ nPCodePos + sizeof( double ) ],
               ( ZH_UCHAR ) pFunc->pCode[ nPCodePos + sizeof( double ) + sizeof( ZH_BYTE ) ] );
   }
   fprintf( cargo->yyc, "\n" );

   return sizeof( double ) + sizeof( ZH_BYTE ) + sizeof( ZH_BYTE ) + 1;
}

static ZH_GENC_FUNC( zh_p_pushfield )
{
   fprintf( cargo->yyc, "\tZH_P_PUSHFIELD, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", zh_compSymbolName( cargo->ZH_COMP_PARAM, ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static ZH_GENC_FUNC( zh_p_pushbyte )
{
   fprintf( cargo->yyc, "\tZH_P_PUSHBYTE, %u,",
            pFunc->pCode[ nPCodePos + 1 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %i */", ( signed char ) pFunc->pCode[ nPCodePos + 1 ] );
   fprintf( cargo->yyc, "\n" );
   return 2;
}

static ZH_GENC_FUNC( zh_p_pushint )
{
   fprintf( cargo->yyc, "\tZH_P_PUSHINT, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %i */", ZH_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static ZH_GENC_FUNC( zh_p_pushlocal )
{
   fprintf( cargo->yyc, "\tZH_P_PUSHLOCAL, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
   {
      int iVar = ( int ) ZH_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] );
      zh_compGenCLocalName( pFunc, iVar, nPCodePos, cargo );
   }
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static ZH_GENC_FUNC( zh_p_pushlocalnear )
{
   fprintf( cargo->yyc, "\tZH_P_PUSHLOCALNEAR, %u,",
            pFunc->pCode[ nPCodePos + 1 ] );
   if( cargo->bVerbose )
   {
      int iVar = ( signed char ) pFunc->pCode[ nPCodePos + 1 ];
      zh_compGenCLocalName( pFunc, iVar, nPCodePos, cargo );
   }
   fprintf( cargo->yyc, "\n" );
   return 2;
}

static ZH_GENC_FUNC( zh_p_pushlocalref )
{
   fprintf( cargo->yyc, "\tZH_P_PUSHLOCALREF, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
   {
      int iVar = ( int ) ZH_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] );
      zh_compGenCLocalName( pFunc, iVar, nPCodePos, cargo );
   }
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static ZH_GENC_FUNC( zh_p_pushlong )
{
   fprintf( cargo->yyc, "\tZH_P_PUSHLONG, %u, %u, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ],
            pFunc->pCode[ nPCodePos + 4 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %li */", ZH_PCODE_MKLONG( &pFunc->pCode[ nPCodePos + 1 ] ) );
   fprintf( cargo->yyc, "\n" );

   return 5;
}

static ZH_GENC_FUNC( zh_p_pushlonglong )
{
   fprintf( cargo->yyc, "\tZH_P_PUSHLONGLONG, %u, %u, %u, %u, %u, %u, %u, %u, ",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ],
            pFunc->pCode[ nPCodePos + 4 ],
            pFunc->pCode[ nPCodePos + 5 ],
            pFunc->pCode[ nPCodePos + 6 ],
            pFunc->pCode[ nPCodePos + 7 ],
            pFunc->pCode[ nPCodePos + 8 ] );
   if( cargo->bVerbose )
   {
#ifdef ZH_LONG_LONG_OFF
      fprintf( cargo->yyc, "\t/* %lf */", ZH_PCODE_MKLONGLONG( &pFunc->pCode[ nPCodePos + 1 ] ) );
#else
      char szBuf[ 24 ];
      fprintf( cargo->yyc, "\t/* %s */", zh_numToStr( szBuf, sizeof( szBuf ),
                                                      ZH_PCODE_MKLONGLONG( &pFunc->pCode[ nPCodePos + 1 ] ) ) );
#endif
   }
   fprintf( cargo->yyc, "\n" );

   return 9;
}

static ZH_GENC_FUNC( zh_p_pushmemvar )
{
   fprintf( cargo->yyc, "\tZH_P_PUSHMEMVAR, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", zh_compSymbolName( cargo->ZH_COMP_PARAM, ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static ZH_GENC_FUNC( zh_p_pushmemvarref )
{
   fprintf( cargo->yyc, "\tZH_P_PUSHMEMVARREF, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", zh_compSymbolName( cargo->ZH_COMP_PARAM, ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static ZH_GENC_FUNC( zh_p_pushnil )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_PUSHNIL,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_pushself )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_PUSHSELF,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_pushstatic )
{
   fprintf( cargo->yyc, "\tZH_P_PUSHSTATIC, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      zh_compGenCStaticName( ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ), cargo );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static ZH_GENC_FUNC( zh_p_pushstaticref )
{
   fprintf( cargo->yyc, "\tZH_P_PUSHSTATICREF, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      zh_compGenCStaticName( ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ), cargo );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static ZH_GENC_FUNC( zh_p_pushstrshort )
{
   ZH_USHORT wLen = pFunc->pCode[ nPCodePos + 1 ];

   fprintf( cargo->yyc, "\tZH_P_PUSHSTRSHORT, %u,", pFunc->pCode[ nPCodePos + 1 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %u */", wLen );

   if( wLen > 0 )
   {
      fprintf( cargo->yyc, "\n\t" );
      zh_compGenCByteStr( cargo->yyc, &pFunc->pCode[ nPCodePos + 2 ], wLen );
   }
   fprintf( cargo->yyc, "\n" );
   return wLen + 2;
}

static ZH_GENC_FUNC( zh_p_pushstr )
{
   ZH_USHORT wLen = ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] );

   fprintf( cargo->yyc, "\tZH_P_PUSHSTR, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %u */", wLen );

   if( wLen > 0 )
   {
      fprintf( cargo->yyc, "\n\t" );
      zh_compGenCByteStr( cargo->yyc, &pFunc->pCode[ nPCodePos + 3 ], wLen );
   }
   fprintf( cargo->yyc, "\n" );
   return wLen + 3;
}

static ZH_GENC_FUNC( zh_p_pushstrlarge )
{
   ZH_SIZE nLen = ZH_PCODE_MKUINT24( &pFunc->pCode[ nPCodePos + 1 ] );

   fprintf( cargo->yyc, "\tZH_P_PUSHSTRLARGE, %u, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %" ZH_PFS "u */", nLen );

   if( nLen > 0 )
   {
      fprintf( cargo->yyc, "\n\t" );
      zh_compGenCByteStr( cargo->yyc, &pFunc->pCode[ nPCodePos + 4 ], nLen );
   }
   fprintf( cargo->yyc, "\n" );
   return nLen + 4;
}

static ZH_GENC_FUNC( zh_p_push_str_hidden )
{
   ZH_USHORT wLen = ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 2 ] );

   fprintf( cargo->yyc, "\tZH_P_PUSH_STR_HIDDEN, %u, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %u */", wLen );

   if( wLen > 0 )
   {
      fprintf( cargo->yyc, "\n\t" );
      zh_compGenCByteStr( cargo->yyc, &pFunc->pCode[ nPCodePos + 4 ], wLen );
   }
   fprintf( cargo->yyc, "\n" );
   return wLen + 4;
}

static ZH_GENC_FUNC( zh_p_pushsym )
{
   fprintf( cargo->yyc, "\tZH_P_PUSHSYM, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", zh_compSymbolName( cargo->ZH_COMP_PARAM, ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static ZH_GENC_FUNC( zh_p_pushsymnear )
{
   fprintf( cargo->yyc, "\tZH_P_PUSHSYMNEAR, %u,",
            pFunc->pCode[ nPCodePos + 1 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", zh_compSymbolName( cargo->ZH_COMP_PARAM, pFunc->pCode[ nPCodePos + 1 ] ) );
   fprintf( cargo->yyc, "\n" );
   return 2;
}

static ZH_GENC_FUNC( zh_p_pushfuncsym )
{
   fprintf( cargo->yyc, "\tZH_P_PUSHFUNCSYM, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", zh_compSymbolName( cargo->ZH_COMP_PARAM, ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static ZH_GENC_FUNC( zh_p_pushvar )
{
   fprintf( cargo->yyc, "\tZH_P_PUSHVARIABLE, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", zh_compSymbolName( cargo->ZH_COMP_PARAM, ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static ZH_GENC_FUNC( zh_p_retvalue )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_RETVALUE,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_send )
{
   fprintf( cargo->yyc, "\tZH_P_SEND, %u, %u,\n",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   return 3;
}

static ZH_GENC_FUNC( zh_p_sendshort )
{
   fprintf( cargo->yyc, "\tZH_P_SENDSHORT, %u,\n", pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static ZH_GENC_FUNC( zh_p_pushovarref )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_PUSHOVARREF,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_seqblock )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_SEQBLOCK,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_seqbegin )
{
   fprintf( cargo->yyc, "\tZH_P_SEQBEGIN, %u, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ] );
   if( cargo->bVerbose )
   {
      ZH_I_SIZE nOffset = ZH_PCODE_MKINT24( &pFunc->pCode[ nPCodePos + 1 ] );
      fprintf( cargo->yyc, "\t/* %" ZH_PFS "i (abs: %08" ZH_PFS "u) */", nOffset, nPCodePos + nOffset );
   }
   fprintf( cargo->yyc, "\n" );
   return 4;
}

static ZH_GENC_FUNC( zh_p_seqend )
{
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "/* %05" ZH_PFS "u */ ", nPCodePos );
   else
      fprintf( cargo->yyc, "\t" );
   fprintf( cargo->yyc, "ZH_P_SEQEND, %u, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ] );
   if( cargo->bVerbose )
   {
      ZH_I_SIZE nOffset = ZH_PCODE_MKINT24( &pFunc->pCode[ nPCodePos + 1 ] );
      fprintf( cargo->yyc, "\t/* %" ZH_PFS "i (abs: %08" ZH_PFS "u) */", nOffset, nPCodePos + nOffset );
   }
   fprintf( cargo->yyc, "\n" );
   return 4;
}

static ZH_GENC_FUNC( zh_p_seqrecover )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_SEQRECOVER,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_seqalways )
{
   fprintf( cargo->yyc, "\tZH_P_SEQALWAYS, %u, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ] );
   if( cargo->bVerbose )
   {
      ZH_I_SIZE nOffset = ZH_PCODE_MKINT24( &pFunc->pCode[ nPCodePos + 1 ] );
      fprintf( cargo->yyc, "\t/* %" ZH_PFS "i (abs: %08" ZH_PFS "u) */", nOffset, nPCodePos + nOffset );
   }
   fprintf( cargo->yyc, "\n" );
   return 4;
}

static ZH_GENC_FUNC( zh_p_alwaysbegin )
{
   ZH_SYMBOL_UNUSED( pFunc );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "/* %05" ZH_PFS "u */ ", nPCodePos );
   else
      fprintf( cargo->yyc, "\t" );

   fprintf( cargo->yyc, "ZH_P_ALWAYSBEGIN, %u, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ] );
   if( cargo->bVerbose )
   {
      ZH_I_SIZE nOffset = ZH_PCODE_MKINT24( &pFunc->pCode[ nPCodePos + 1 ] );
      fprintf( cargo->yyc, "\t/* %" ZH_PFS "i (abs: %08" ZH_PFS "u) */", nOffset, nPCodePos + nOffset );
   }
   fprintf( cargo->yyc, "\n" );
   return 4;
}

static ZH_GENC_FUNC( zh_p_alwaysend )
{
   ZH_SYMBOL_UNUSED( pFunc );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "/* %05" ZH_PFS "u */ ", nPCodePos );
   else
      fprintf( cargo->yyc, "\t" );

   fprintf( cargo->yyc, "ZH_P_ALWAYSEND,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_sframe )
{
   fprintf( cargo->yyc, "\tZH_P_SFRAME, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* symbol (_INITSTATICS) */" );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static ZH_GENC_FUNC( zh_p_statics )
{
   fprintf( cargo->yyc, "\tZH_P_STATICS, %u, %u, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ],
            pFunc->pCode[ nPCodePos + 4 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* symbol (_INITSTATICS), %u statics */", ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 3 ] ) );
   fprintf( cargo->yyc, "\n" );

   return 5;
}

static ZH_GENC_FUNC( zh_p_staticname )
{
   ZH_SIZE nStart = nPCodePos;

   fprintf( cargo->yyc, "\tZH_P_STATICNAME, %u, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", ( char * ) pFunc->pCode + nPCodePos + 4 );
   fprintf( cargo->yyc, "\n" );

   nPCodePos += 4;
   while( pFunc->pCode[ nPCodePos ] )
   {
      char chr = pFunc->pCode[ nPCodePos++ ];
      if( chr == '\'' || chr == '\\' )
         fprintf( cargo->yyc, " \'\\%c\',", chr );
      else
         fprintf( cargo->yyc, " \'%c\',", chr );
   }
   fprintf( cargo->yyc, " 0,\n" );

   return nPCodePos - nStart + 1;
}

static ZH_GENC_FUNC( zh_p_threadstatics )
{
   ZH_USHORT w = ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ), u;

   fprintf( cargo->yyc, "\tZH_P_THREADSTATICS, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* number of thread static variables: %u */", w );
   fprintf( cargo->yyc, "\n" );

   nPCodePos += 3;
   for( u = 0; u < w; ++u )
   {
      fprintf( cargo->yyc, "\t%u, %u,",
               pFunc->pCode[ nPCodePos ],
               pFunc->pCode[ nPCodePos + 1 ] );
      if( cargo->bVerbose )
         zh_compGenCStaticName( ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos ] ), cargo );
      fprintf( cargo->yyc, "\n" );
      nPCodePos += 2;
   }

   return ( ( ZH_SIZE ) w << 1 ) + 3;
}

static ZH_GENC_FUNC( zh_p_swapalias )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_SWAPALIAS,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_true )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_TRUE,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_one )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_ONE,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_zero )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_ZERO,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_noop )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_NOOP,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_dummy )
{
   ZH_SYMBOL_UNUSED( cargo );
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );
   return 1;
}

static ZH_GENC_FUNC( zh_p_enumstart )
{
   fprintf( cargo->yyc, "\tZH_P_ENUMSTART, %u, %u,\n",
            pFunc->pCode[ nPCodePos + 1 ], pFunc->pCode[ nPCodePos + 2 ] );
   return 3;
}

static ZH_GENC_FUNC( zh_p_enumnext )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );
   fprintf( cargo->yyc, "\tZH_P_ENUMNEXT,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_enumprev )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );
   fprintf( cargo->yyc, "\tZH_P_ENUMPREV,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_enumend )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );
   fprintf( cargo->yyc, "\tZH_P_ENUMEND,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_switch )
{
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "/* %05" ZH_PFS "u */ ", nPCodePos );
   else
      fprintf( cargo->yyc, "\t" );

   fprintf( cargo->yyc, "ZH_P_SWITCH, %u, %u,", pFunc->pCode[ nPCodePos + 1 ], pFunc->pCode[ nPCodePos + 2 ] );

   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %i*/", ZH_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   }

   fprintf( cargo->yyc, "\n" );
   return 3;
}

static ZH_GENC_FUNC( zh_p_pushdate )
{
   fprintf( cargo->yyc, "\tZH_P_PUSH_DATE, %u, %u, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ],
            pFunc->pCode[ nPCodePos + 4 ] );
   if( cargo->bVerbose )
   {
      int  year, month, day;
      char date[ 9 ];

      zh_dateDecode( ZH_PCODE_MKLONG( &pFunc->pCode[ nPCodePos + 1 ] ), &year, &month, &day );
      zh_dateStrPut( date, year, month, day );
      date[ 8 ] = '\0';
      fprintf( cargo->yyc, "\t/* %s */", date );
   }
   fprintf( cargo->yyc, "\n" );

   return 5;
}

static ZH_GENC_FUNC( zh_p_pushtimestamp )
{
   fprintf( cargo->yyc, "\tZH_P_PUSHTIMESTAMP, %u, %u, %u, %u, %u, %u, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ],
            pFunc->pCode[ nPCodePos + 4 ],
            pFunc->pCode[ nPCodePos + 5 ],
            pFunc->pCode[ nPCodePos + 6 ],
            pFunc->pCode[ nPCodePos + 7 ],
            pFunc->pCode[ nPCodePos + 8 ] );
   if( cargo->bVerbose )
   {
      char timestamp[ 24 ];

      zh_timeStampStr( timestamp,
                       ZH_PCODE_MKLONG( &pFunc->pCode[ nPCodePos + 1 ] ),
                       ZH_PCODE_MKLONG( &pFunc->pCode[ nPCodePos + 5 ] ) );
      fprintf( cargo->yyc, "\t/* %s */", timestamp );
   }
   fprintf( cargo->yyc, "\n" );

   return 9;
}

static ZH_GENC_FUNC( zh_p_localnearaddint )
{
   fprintf( cargo->yyc, "\tZH_P_LOCALNEARADDINT, %u, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ] );

   if( cargo->bVerbose )
   {
      int iVar = ( signed char ) pFunc->pCode[ nPCodePos + 1 ];
      zh_compGenCLocalName( pFunc, iVar, nPCodePos, cargo );
      fprintf( cargo->yyc, "/* %i */", ZH_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 2 ] ) );
   }
   fprintf( cargo->yyc, "\n" );

   return 4;
}

static ZH_GENC_FUNC( zh_p_localaddint )
{
   fprintf( cargo->yyc, "\tZH_P_LOCALADDINT, %u, %u, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ],
            pFunc->pCode[ nPCodePos + 4 ] );

   if( cargo->bVerbose )
   {
      int iVar = ZH_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] );
      zh_compGenCLocalName( pFunc, iVar, nPCodePos, cargo );
      fprintf( cargo->yyc, "/* %i */", ZH_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 3 ] ) );
   }
   fprintf( cargo->yyc, "\n" );

   return 5;
}

static ZH_GENC_FUNC( zh_p_localinc )
{
   fprintf( cargo->yyc, "\tZH_P_LOCALINC, %u, %u,", pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );

   if( cargo->bVerbose )
   {
      int iVar = ZH_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] );
      zh_compGenCLocalName( pFunc, iVar, nPCodePos, cargo );
   }
   fprintf( cargo->yyc, "\n" );

   return 3;
}

static ZH_GENC_FUNC( zh_p_localdec )
{
   fprintf( cargo->yyc, "\tZH_P_LOCALDEC, %u, %u,", pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );

   if( cargo->bVerbose )
   {
      int iVar = ZH_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] );
      zh_compGenCLocalName( pFunc, iVar, nPCodePos, cargo );
   }
   fprintf( cargo->yyc, "\n" );

   return 3;
}

static ZH_GENC_FUNC( zh_p_localincpush )
{
   fprintf( cargo->yyc, "\tZH_P_LOCALINCPUSH, %u, %u,", pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );

   if( cargo->bVerbose )
   {
      int iVar = ZH_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] );
      zh_compGenCLocalName( pFunc, iVar, nPCodePos, cargo );
   }
   fprintf( cargo->yyc, "\n" );

   return 3;
}

static ZH_GENC_FUNC( zh_p_pluseqpop )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_PLUSEQPOP,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_minuseqpop )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_MINUSEQPOP,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_multeqpop )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_MULTEQPOP,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_diveqpop )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_DIVEQPOP,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_modeqpop )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_MODEQPOP,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_expeqpop )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_EXPEQPOP,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_inceqpop )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_INCEQPOP,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_deceqpop )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_DECEQPOP,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_pluseq )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_PLUSEQ,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_minuseq )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_MINUSEQ,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_multeq )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_MULTEQ,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_diveq )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_DIVEQ,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_modeq )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_MODEQ,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_expeq )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_EXPEQ,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_inceq )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_INCEQ,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_deceq )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_DECEQ,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_withobjectstart )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_WITHOBJECTSTART,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_withobjectmessage )
{
   fprintf( cargo->yyc, "\tZH_P_WITHOBJECTMESSAGE, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", zh_compSymbolName( cargo->ZH_COMP_PARAM, ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static ZH_GENC_FUNC( zh_p_withobjectend )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_WITHOBJECTEND,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_vframe )
{
   fprintf( cargo->yyc, "\tZH_P_VFRAME, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* locals, params */" );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static ZH_GENC_FUNC( zh_p_largeframe )
{
   fprintf( cargo->yyc, "\tZH_P_LARGEFRAME, %u, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* locals, params */" );
   fprintf( cargo->yyc, "\n" );
   return 4;
}

static ZH_GENC_FUNC( zh_p_largevframe )
{
   fprintf( cargo->yyc, "\tZH_P_LARGEVFRAME, %u, %u, %u,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* locals, params */" );
   fprintf( cargo->yyc, "\n" );
   return 4;
}

static ZH_GENC_FUNC( zh_p_pushvparams )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_PUSHVPARAMS,\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_pushaparams )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tZH_P_PUSHAPARAMS,\n" );
   return 1;
}

/* NOTE: The order of functions have to match the order of opcodes
 *       mnemonics
 */
static const PZH_GENC_FUNC s_verbose_table[] = {
   zh_p_and,
   zh_p_arraypush,
   zh_p_arraypop,
   zh_p_arraydim,
   zh_p_arraygen,
   zh_p_equal,
   zh_p_endblock,
   zh_p_endproc,
   zh_p_exactlyequal,
   zh_p_false,
   zh_p_fortest,
   zh_p_function,
   zh_p_functionshort,
   zh_p_frame,
   zh_p_funcptr,
   zh_p_greater,
   zh_p_greaterequal,
   zh_p_dec,
   zh_p_divide,
   zh_p_do,
   zh_p_doshort,
   zh_p_duplicate,
   zh_p_pushtimestamp,
   zh_p_inc,
   zh_p_instring,
   zh_p_jumpnear,
   zh_p_jump,
   zh_p_jumpfar,
   zh_p_jumpfalsenear,
   zh_p_jumpfalse,
   zh_p_jumpfalsefar,
   zh_p_jumptruenear,
   zh_p_jumptrue,
   zh_p_jumptruefar,
   zh_p_lessequal,
   zh_p_less,
   zh_p_line,
   zh_p_localname,
   zh_p_macropop,
   zh_p_macropopaliased,
   zh_p_macropush,
   zh_p_macro_array_gen,
   zh_p_macropushlist,
   zh_p_macropushindex,
   zh_p_macropushpare,
   zh_p_macropushaliased,
   zh_p_macrosymbol,
   zh_p_macrotext,
   zh_p_message,
   zh_p_minus,
   zh_p_modulus,
   zh_p_modulename,
   /* start: pcodes generated by macro compiler */
   zh_p_dummy,
   zh_p_dummy,
   zh_p_dummy,
   zh_p_dummy,
   zh_p_dummy,
   zh_p_dummy,
   zh_p_dummy,
   zh_p_dummy,
   zh_p_dummy,
   zh_p_dummy,
   zh_p_dummy,
   zh_p_dummy,
   zh_p_dummy,
   /* end: */
   zh_p_mult,
   zh_p_negate,
   zh_p_noop,
   zh_p_not,
   zh_p_notequal,
   zh_p_or,
   zh_p_parameter,
   zh_p_plus,
   zh_p_pop,
   zh_p_popalias,
   zh_p_popaliasedfield,
   zh_p_popaliasedfieldnear,
   zh_p_popaliasedvar,
   zh_p_popfield,
   zh_p_poplocal,
   zh_p_poplocalnear,
   zh_p_popmemvar,
   zh_p_popstatic,
   zh_p_popvar,
   zh_p_power,
   zh_p_pushalias,
   zh_p_pushaliasedfield,
   zh_p_pushaliasedfieldnear,
   zh_p_pushaliasedvar,
   zh_p_push_block,
   zh_p_push_blockshort,
   zh_p_pushfield,
   zh_p_pushbyte,
   zh_p_pushint,
   zh_p_pushlocal,
   zh_p_pushlocalnear,
   zh_p_pushlocalref,
   zh_p_pushlong,
   zh_p_pushmemvar,
   zh_p_pushmemvarref,
   zh_p_pushnil,
   zh_p_pushdouble,
   zh_p_pushself,
   zh_p_pushstatic,
   zh_p_pushstaticref,
   zh_p_pushstr,
   zh_p_pushstrshort,
   zh_p_pushsym,
   zh_p_pushsymnear,
   zh_p_pushvar,
   zh_p_retvalue,
   zh_p_send,
   zh_p_sendshort,
   zh_p_seqbegin,
   zh_p_seqend,
   zh_p_seqrecover,
   zh_p_sframe,
   zh_p_statics,
   zh_p_staticname,
   zh_p_swapalias,
   zh_p_true,
   zh_p_zero,
   zh_p_one,
   zh_p_macro_func,
   zh_p_macrodo,
   /* start: more pcodes generated by macro compiler */
   zh_p_dummy,
   /* end: */
   zh_p_localnearaddint,
   zh_p_macropushref,
   zh_p_pushlonglong,
   zh_p_enumstart,
   zh_p_enumnext,
   zh_p_enumprev,
   zh_p_enumend,
   zh_p_switch,
   zh_p_pushdate,
   /* optimization of inlined math operations (+=, -= */
   zh_p_pluseqpop,
   zh_p_minuseqpop,
   zh_p_multeqpop,
   zh_p_diveqpop,
   zh_p_pluseq,
   zh_p_minuseq,
   zh_p_multeq,
   zh_p_diveq,
   zh_p_withobjectstart,
   zh_p_withobjectmessage,
   zh_p_withobjectend,
   zh_p_macrosend,
   zh_p_pushovarref,
   zh_p_arraypushref,
   zh_p_vframe,
   zh_p_largeframe,
   zh_p_largevframe,
   zh_p_push_str_hidden,
   zh_p_localaddint,
   zh_p_modeqpop,
   zh_p_expeqpop,
   zh_p_modeq,
   zh_p_expeq,
   zh_p_duplunref,
   zh_p_dummy,
   zh_p_dummy,
   zh_p_push_blocklarge,
   zh_p_pushstrlarge,
   zh_p_swap,
   zh_p_pushvparams,
   zh_p_pushunref,
   zh_p_seqalways,
   zh_p_alwaysbegin,
   zh_p_alwaysend,
   zh_p_deceqpop,
   zh_p_inceqpop,
   zh_p_deceq,
   zh_p_inceq,
   zh_p_localdec,
   zh_p_localinc,
   zh_p_localincpush,
   zh_p_pushfuncsym,
   zh_p_hashgen,
   zh_p_seqblock,
   zh_p_threadstatics,
   zh_p_pushaparams
};

static void zh_compGenCReadable( ZH_COMP_DECL, PZH_ZFUNC pFunc, FILE * yyc )
{
   const PZH_GENC_FUNC * pFuncTable = s_verbose_table;
   ZH_GENC_INFO genc_info;

   /* Make sure that table is correct */
   assert( ZH_P_LAST_PCODE == sizeof( s_verbose_table ) / sizeof( PZH_GENC_FUNC ) );

   genc_info.ZH_COMP_PARAM = ZH_COMP_PARAM;
   genc_info.nEndBlockPos  = 0;
   genc_info.bVerbose      = ( ZH_COMP_PARAM->iGenCOutput == ZH_COMPGENC_VERBOSE );
   genc_info.yyc = yyc;

   fprintf( yyc, "{\n   static const ZH_BYTE pcode[] =\n   {\n" );
   zh_compPCodeEval( pFunc, ( const PZH_PCODE_FUNC * ) pFuncTable, ( void * ) &genc_info );

   if( genc_info.bVerbose )
      fprintf( yyc, "/* %05" ZH_PFS "u */\n", pFunc->nPCodePos );
   fprintf( yyc, "   };\n\n" );
   fprintf( yyc, "   zh_vmExecute( pcode, symbols );\n}\n" );
}

static void zh_compGenCCompact( PZH_ZFUNC pFunc, FILE * yyc )
{
   ZH_SIZE nPCodePos = 0;
   int     nChar;

   fprintf( yyc, "{\n\tstatic const ZH_BYTE pcode[] =\n\t{\n\t\t" );

   nChar = 0;
   while( nPCodePos < pFunc->nPCodePos )
   {
      ++nChar;

      if( nChar > 1 )
         fprintf( yyc, "," );

      if( nChar == 20 )
      {
         fprintf( yyc, "\n\t\t" );
         nChar = 1;
      }

      /* Displaying as decimal is more compact than hex */
      fprintf( yyc, "%d", ( int ) pFunc->pCode[ nPCodePos++ ] );
   }

   if( nChar != 0 )
      fprintf( yyc, "\n" );

   fprintf( yyc, "\t};\n\n" );
   fprintf( yyc, "\tzh_vmExecute( pcode, symbols );\n}\n" );
}
