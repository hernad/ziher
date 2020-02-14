/*
 * Compiler Ziher Portable Object (.hrb) generation
 *
 * Copyright 1999 Eddie Runia <eddie@runia.com>
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *    rewritten to work on memory buffers and with new compiler code
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

#define SYM_NOLINK    0             /* symbol does not have to be linked */
#define SYM_FUNC      1             /* function defined in this module   */
#define SYM_EXTERN    2             /* function defined in other module  */
#define SYM_DEFERRED  3             /* lately bound function             */

static ZH_SIZE zh_compHrbSize( ZH_COMP_DECL, ZH_ULONG * pulSymbols, ZH_ULONG * pulFunctions )
{
   PZH_ZFUNC pFunc;
   PZH_HSYMBOL pSym;
   ZH_SIZE nSize;

   *pulSymbols = *pulFunctions = 0;

   /* count total size */
   nSize = 10;  /* signature[4] + version[2] + symbols_number[4] */
   pSym = ZH_COMP_PARAM->symbols.pFirst;
   while( pSym )
   {
      ( *pulSymbols )++;
      nSize += strlen( pSym->szName ) + 3; /* \0 + symscope[1] + symtype[1] */
      pSym = pSym->pNext;
   }
   nSize += 4; /* functions_number[4] */
   /* Generate functions data */
   pFunc = ZH_COMP_PARAM->functions.pFirst;
   while( pFunc )
   {
      if( ( pFunc->funFlags & ZH_FUNF_FILE_DECL ) == 0 )
      {
         ( *pulFunctions )++;
         nSize += strlen( pFunc->szName ) + 5 + pFunc->nPCodePos; /* \0 + func_size[4] + function_body */
      }
      pFunc = pFunc->pNext;
   }

   return nSize;
}

void zh_compGenBufPortObj( ZH_COMP_DECL, ZH_BYTE ** pBufPtr, ZH_SIZE * pnSize )
{
   PZH_ZFUNC pFunc;
   PZH_HSYMBOL pSym;
   ZH_ULONG ulSymbols, ulFunctions;
   ZH_SIZE nLen;
   ZH_BYTE * ptr;

   *pnSize = zh_compHrbSize( ZH_COMP_PARAM, &ulSymbols, &ulFunctions );
   /* additional 0 byte is for passing buffer directly as string item */
   ptr = *pBufPtr = ( ZH_BYTE * ) zh_xgrab( *pnSize + 1 );

   /* signature */
   *ptr++ = 0xC0;
   *ptr++ = 'H';
   *ptr++ = 'R';
   *ptr++ = 'B';
   ZH_PUT_LE_UINT16( ptr, 2 );   /* version number */
   ptr += 2;

   ZH_PUT_LE_UINT32( ptr, ulSymbols ); /* number of symbols */
   ptr += 4;
   /* generate the symbol table */
   pSym = ZH_COMP_PARAM->symbols.pFirst;
   while( pSym )
   {
      nLen = strlen( pSym->szName ) + 1;
      memcpy( ptr, pSym->szName, nLen );
      ptr += nLen;
      /* FIXME: this conversion strips upper byte from symbol scope
       *        Now we added workaround for it by using some strict
       *        bit order and restoring some others at runtime when
       *        .hrb file is loaded but we should create new format
       *        for .hrb files in which this field will have at least
       *        16-bit [druzus]
       */
      *ptr++ = ( ZH_BYTE ) pSym->cScope;
      /* symbol type */
      if( pSym->cScope & ZH_FS_LOCAL )
         *ptr++ = SYM_FUNC;      /* function defined in this module */
      else if( pSym->cScope & ZH_FS_DEFERRED )
         *ptr++ = SYM_DEFERRED;  /* lately bound function */
      else if( pSym->iFunc )
         *ptr++ = SYM_EXTERN;    /* external function */
      else
         *ptr++ = SYM_NOLINK;    /* other symbol */
      pSym = pSym->pNext;
   }

   ZH_PUT_LE_UINT32( ptr, ulFunctions );  /* number of functions */
   ptr += 4;
   /* generate functions data */
   pFunc = ZH_COMP_PARAM->functions.pFirst;
   while( pFunc )
   {
      if( ( pFunc->funFlags & ZH_FUNF_FILE_DECL ) == 0 )
      {
         nLen = strlen( pFunc->szName ) + 1;
         memcpy( ptr, pFunc->szName, nLen );
         ptr += nLen;
         ZH_PUT_LE_UINT32( ptr, pFunc->nPCodePos );      /* function size */
         ptr += 4;
         memcpy( ptr, pFunc->pCode, pFunc->nPCodePos );  /* function body */
         ptr += pFunc->nPCodePos;
      }
      pFunc = pFunc->pNext;
   }
}

void zh_compGenPortObj( ZH_COMP_DECL, PZH_FNAME pFileName )
{
   char szFileName[ ZH_PATH_MAX ];
   ZH_SIZE nSize;
   ZH_BYTE * pHrbBody;
   FILE * yyc;

   if( ! pFileName->szExtension )
      pFileName->szExtension = ".hrb";
   zh_fsFNameMerge( szFileName, pFileName );

   yyc = zh_fopen( szFileName, "wb" );
   if( ! yyc )
   {
      zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_CREATE_OUTPUT, szFileName, NULL );
      return;
   }

   if( ! ZH_COMP_PARAM->fQuiet )
   {
      char buffer[ 80 + ZH_PATH_MAX - 1 ];
      zh_snprintf( buffer, sizeof( buffer ),
                   "Generating Ziher Portable Object output to \'%s\'... ", szFileName );
      zh_compOutStd( ZH_COMP_PARAM, buffer );
   }

   zh_compGenBufPortObj( ZH_COMP_PARAM, &pHrbBody, &nSize );

   if( fwrite( pHrbBody, nSize, 1, yyc ) != 1 )
      zh_compGenError( ZH_COMP_PARAM, zh_comp_szErrors, 'E', ZH_COMP_ERR_FILE_WRITE, szFileName, NULL );

   zh_xfree( pHrbBody );

   fclose( yyc );

   if( ! ZH_COMP_PARAM->fQuiet )
      zh_compOutStd( ZH_COMP_PARAM, "Done.\n" );
}
