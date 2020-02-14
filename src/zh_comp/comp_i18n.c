/*
 * i18n support in Ziher compiler
 *
 * Copyright 2008 Mindaugas Kavaliauskas <dbtopas.at.dbtopas.lt>
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

static PZH_I18NTABLE zh_compI18nCreate( void )
{
   PZH_I18NTABLE pI18n;

   pI18n = ( PZH_I18NTABLE ) zh_xgrab( sizeof( ZH_I18NTABLE ) );
   pI18n->pString     = NULL;
   pI18n->uiCount     = 0;
   pI18n->uiAllocated = 0;

   return pI18n;
}

void zh_compI18nFree( ZH_COMP_DECL )
{
   PZH_I18NTABLE pI18n = ZH_COMP_PARAM->pI18n;

   if( pI18n )
   {
      if( pI18n->pString )
      {
         ZH_UINT ui;

         for( ui = 0; ui < pI18n->uiCount; ui++ )
         {
            if( pI18n->pString[ ui ].uiPosCount )
               zh_xfree( pI18n->pString[ ui ].pPosLst );
         }
         zh_xfree( pI18n->pString );
      }
      zh_xfree( pI18n );
      ZH_COMP_PARAM->pI18n = NULL;
   }
}

static int zh_compI18nCompare( PZH_I18NSTRING pString, const char * pText, const char * pContext )
{
   int i;

   i = pString->szText == pText ? 0 : pString->szText > pText ? 1 : -1;
   if( i == 0 && pString->szContext != pContext )
      i = pString->szContext > pContext ? 1 : -1;

   return i;
}

static PZH_I18NSTRING zh_compI18nAddSingle( ZH_COMP_DECL, const char * szText, const char * szContext,
                                            const char * szModule, ZH_UINT uiLine )
{
   PZH_I18NTABLE  pI18n;
   PZH_I18NSTRING pString;
   ZH_UINT        uiLeft, uiRight;

   if( ! ZH_COMP_PARAM->pI18n )
      ZH_COMP_PARAM->pI18n = zh_compI18nCreate();
   pI18n = ZH_COMP_PARAM->pI18n;

   szText = zh_compIdentifierNew( ZH_COMP_PARAM, szText, ZH_IDENT_COPY );
   if( szContext )
      szContext = zh_compIdentifierNew( ZH_COMP_PARAM, szContext, ZH_IDENT_COPY );

   if( pI18n->uiCount >= pI18n->uiAllocated )
   {
      if( pI18n->pString )
      {
         pI18n->uiAllocated += 32;
         pI18n->pString      = ( PZH_I18NSTRING ) zh_xrealloc( pI18n->pString, sizeof( ZH_I18NSTRING )
                                                               * pI18n->uiAllocated );
      }
      else
      {
         pI18n->pString     = ( PZH_I18NSTRING ) zh_xgrab( sizeof( ZH_I18NSTRING ) * 32 );
         pI18n->uiAllocated = 32;
      }
   }

   uiLeft  = 0;
   uiRight = pI18n->uiCount;

   while( uiLeft < uiRight )
   {
      ZH_UINT uiMiddle = ( uiLeft + uiRight ) >> 1;
      int iCompare = zh_compI18nCompare( &pI18n->pString[ uiMiddle ], szText, szContext );

      if( iCompare == 0 )
      {
         pString = &pI18n->pString[ uiMiddle ];

         if( pString->uiPosCount )
         {
            pString->pPosLst = ( PZH_I18NPOS ) zh_xrealloc( pString->pPosLst, ( pString->uiPosCount + 1 ) * sizeof( ZH_I18NPOS ) );
            pString->pPosLst[ pString->uiPosCount ].uiLine = uiLine;
            pString->pPosLst[ pString->uiPosCount ].szFile = szModule;
            pString->uiPosCount++;
         }
         else
         {
            pString->pPosLst = ( PZH_I18NPOS ) zh_xgrab( sizeof( ZH_I18NPOS ) );
            pString->pPosLst[ 0 ].uiLine = uiLine;
            pString->pPosLst[ 0 ].szFile = szModule;
            pString->uiPosCount = 1;
         }
         return pString;
      }
      else if( iCompare < 0 )
         uiLeft = uiMiddle + 1;
      else
         uiRight = uiMiddle;
   }

   memmove( &pI18n->pString[ uiLeft + 1 ], &pI18n->pString[ uiLeft ],
            ( pI18n->uiCount - uiLeft ) * sizeof( ZH_I18NSTRING ) );

   pString              = &pI18n->pString[ uiLeft ];
   pString->szText      = szText;
   pString->szContext   = szContext;
   pString->pPos.uiLine = uiLine;
   pString->pPos.szFile = szModule;
   pString->uiPosCount  = 0;
   pString->uiPlurals   = 0;

   pI18n->uiCount++;

   return pString;
}

void zh_compI18nAdd( ZH_COMP_DECL, const char * szText, const char * szContext,
                     const char * szModule, ZH_UINT uiLine )
{
   zh_compI18nAddSingle( ZH_COMP_PARAM, szText, szContext, szModule, uiLine );
}

void zh_compI18nAddPlural( ZH_COMP_DECL, const char ** szTexts, ZH_ULONG ulCount,
                           const char * szContext, const char * szModule, ZH_UINT uiLine )
{
   PZH_I18NSTRING pString = zh_compI18nAddSingle( ZH_COMP_PARAM, szTexts[ 0 ], szContext, szModule, uiLine );

   if( ulCount == 1 )
   {
      /* set the same string as plural form to mark it as plural text */
      if( ! pString->uiPlurals )
      {
         pString->szPlurals[ 0 ] = pString->szText;
         pString->uiPlurals      = 1;
      }
   }
   else
   {
      ZH_ULONG ul;

      for( ul = 1; ul < ulCount && pString->uiPlurals < ZH_I18N_PLURAL_MAX; ++ul )
      {
         const char * szText = zh_compIdentifierNew( ZH_COMP_PARAM, szTexts[ ul ], ZH_IDENT_COPY );
         ZH_UINT uiPlural = pString->uiPlurals;

         while( uiPlural-- )
         {
            if( pString->szPlurals[ uiPlural ] == szText )
            {
               szText = NULL;
               break;
            }
         }
         if( szText )
            pString->szPlurals[ pString->uiPlurals++ ] = szText;
      }
   }
}

static void zh_compI18nEscapeString( FILE * file, const char * szText )
{
   while( *szText )
   {
      if( ( ZH_UCHAR ) *szText < ' ' )
      {
         if( *szText == '\t' )
            fprintf( file, "\\t" );
         else if( *szText == '\n' )
            fprintf( file, "\\n" );
         else if( *szText == '\r' )
            fprintf( file, "\\r" );
         else if( ( ( ZH_UCHAR ) szText[ 1 ] >= '0' && ( ZH_UCHAR ) szText[ 1 ] <= '9' ) ||
                  ( ( ZH_UCHAR ) szText[ 1 ] >= 'A' && ( ZH_UCHAR ) szText[ 1 ] <= 'F' ) ||
                  ( ( ZH_UCHAR ) szText[ 1 ] >= 'a' && ( ZH_UCHAR ) szText[ 1 ] <= 'f' ) )
            fprintf( file, "\\%03o", *szText );
         else
            fprintf( file, "\\x%02X", *szText );
      }
      else if( *szText == '"' )
         fprintf( file, "\\\"" );
      else if( *szText == '\\' )
         fprintf( file, "\\\\" );
      else
         fprintf( file, "%c", *szText );

      szText++;
   }
}

static char * zh_compI18nFileName( char * szBuffer, const char * szFileName )
{
   ZH_UINT ui = 0;
   char    ch;

   do
   {
      if( ui == ZH_PATH_MAX - 1 )
         ch = '\0';
      else
      {
         ch = szFileName[ ui ];
         if( ch == '\\' )
            ch = '/';
      }
      szBuffer[ ui++ ] = ch;
   }
   while( ch );

   return szBuffer;
}

ZH_BOOL zh_compI18nSave( ZH_COMP_DECL, ZH_BOOL fFinal )
{
   PZH_I18NTABLE pI18n;
   ZH_FNAME      FileName;
   char          szFileName[ ZH_PATH_MAX ];
   char *        szText;
   ZH_UINT       uiIndex;
   FILE *        file;

   pI18n = ZH_COMP_PARAM->pI18n;
   if( ! pI18n )
      return ZH_FALSE;

   FileName.szPath            =
      FileName.szName         =
         FileName.szExtension =
            FileName.szDrive  = NULL;

   if( ZH_COMP_PARAM->pOutPath )
   {
      FileName.szDrive = ZH_COMP_PARAM->pOutPath->szDrive;
      FileName.szPath  = ZH_COMP_PARAM->pOutPath->szPath;
   }

   if( ZH_COMP_PARAM->pI18nFileName )
   {
      if( ZH_COMP_PARAM->pI18nFileName->szName )
         FileName.szName = ZH_COMP_PARAM->pI18nFileName->szName;

      if( ZH_COMP_PARAM->pI18nFileName->szExtension )
         FileName.szExtension = ZH_COMP_PARAM->pI18nFileName->szExtension;

      if( ZH_COMP_PARAM->pI18nFileName->szPath )
      {
         FileName.szDrive = ZH_COMP_PARAM->pI18nFileName->szDrive;
         FileName.szPath  = ZH_COMP_PARAM->pI18nFileName->szPath;
      }
   }

   if( ! FileName.szName )
      FileName.szName = ZH_COMP_PARAM->pFileName->szName;
   else if( ! fFinal )
      /* The exact file name was given generate single .pot file for
       * all compiled .prg files in final phase.
       */
      return ZH_FALSE;

   if( ! FileName.szExtension )
      FileName.szExtension = ".pot";

   zh_fsFNameMerge( szFileName, &FileName );

   file = zh_fopen( szFileName, "w" );

   if( ! file )
   {
      zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_CREATE_OUTPUT, szFileName, NULL );
      return ZH_FALSE;
   }

   szText = zh_verZiher();
   fprintf( file, "#\n# This file is generated by %s\n#\n\n", szText );
   zh_xfree( szText );

   for( uiIndex = 0; uiIndex < pI18n->uiCount; uiIndex++ )
   {
      PZH_I18NSTRING pString = &pI18n->pString[ uiIndex ];
      ZH_UINT        uiLine;

      fprintf( file, "#: %s:%u",
               zh_compI18nFileName( szFileName, pString->pPos.szFile ),
               pString->pPos.uiLine );

      for( uiLine = 0; uiLine < pString->uiPosCount; ++uiLine )
         fprintf( file, " %s:%u",
                  zh_compI18nFileName( szFileName, pString->pPosLst[ uiLine ].szFile ),
                  pString->pPosLst[ uiLine ].uiLine );

      fprintf( file, "\n#, c-format\n" );

      if( pString->szContext )
      {
         fprintf( file, "msgctxt \"" );
         zh_compI18nEscapeString( file, pString->szContext );
         fprintf( file, "\"\n" );
      }

      fprintf( file, "msgid \"" );
      zh_compI18nEscapeString( file, pString->szText );
      for( uiLine = 0; uiLine < pString->uiPlurals; ++uiLine )
      {
         if( uiLine == 0 )
            fprintf( file, "\"\nmsgid_plural \"" );
         else
            fprintf( file, "\"\nmsgid_plural%u \"", uiLine + 1 );
         zh_compI18nEscapeString( file, pString->szPlurals[ uiLine ] );
      }
      fprintf( file, "\"\nmsgstr%s \"\"\n\n", pString->uiPlurals ? "[0]" : "" );
   }

   fclose( file );
   return ZH_TRUE;
}
