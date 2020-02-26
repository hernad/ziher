/*
 * Copyright 2008 Pritpal Bedi <pritpal@vouchcac.com>
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

#include "zh_harupdf.h"

/* HPDF_LoadTypeIFontFromFile( hDoc, cAFMFileName, cPFA_PFBFileName ) --> cFontName */
ZH_FUNC( HPDF_LOADTYPE1FONTFROMFILE )
{
   char *       pszFree1;
   const char * pszFileName1 = zh_fsNameConv( zh_parcx( 2 ), &pszFree1 );
   char *       pszFree2;
   const char * pszFileName2 = zh_fsNameConv( zh_parcx( 3 ), &pszFree2 );

   zh_retc( HPDF_LoadType1FontFromFile( zh_HPDF_Doc_par( 1 ), pszFileName1, pszFileName2 ) );

   if( pszFree1 )
      zh_xfree( pszFree1 );

   if( pszFree2 )
      zh_xfree( pszFree2 );
}

/* HPDF_LoadTTFontFromFile( hDoc, cTTFontFileName, lEmbed ) --> cFontName */
ZH_FUNC( HPDF_LOADTTFONTFROMFILE )
{
   char *       pszFree;
   const char * pszFileName = zh_fsNameConv( zh_parcx( 2 ), &pszFree );

   zh_retc( HPDF_LoadTTFontFromFile( zh_HPDF_Doc_par( 1 ), pszFileName, zh_parl( 3 ) ? HPDF_TRUE : HPDF_FALSE ) );

   if( pszFree )
      zh_xfree( pszFree );
}

/* HPDF_LoadTTFontFromFile2( hDoc, cTTFontFileName, nIndexInFile, lEmbed ) --> cFontName */
ZH_FUNC( HPDF_LOADTTFONTFROMFILE2 )
{
   char *       pszFree;
   const char * pszFileName = zh_fsNameConv( zh_parcx( 2 ), &pszFree );

   zh_retc( HPDF_LoadTTFontFromFile2( zh_HPDF_Doc_par( 1 ), pszFileName, zh_parni( 3 ), zh_parl( 4 ) ? HPDF_TRUE : HPDF_FALSE ) );

   if( pszFree )
      zh_xfree( pszFree );
}
