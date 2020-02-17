/*
 * SIX compatible function:
 *       sx_FNameParser()
 *
 * Copyright 2007 Przemyslaw Czerpak
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
#include "zh_apifs.h"
#include "zh_set.h"

ZH_FUNC( SX_FNAMEPARSER )
{
   const char * szFileName = zh_parc( 1 );

   if( szFileName )
   {
      char szPathBuf[ ZH_PATH_MAX ];
      PZH_FNAME pFileName;
      ZH_SIZE nLen;
      char * pszFree;

      szFileName = zh_fsNameConv( szFileName, &pszFree );
      pFileName = zh_fsFNameSplit( szFileName );
      if( pszFree )
         zh_xfree( pszFree );

      if( ! zh_parl( 2 ) )
         pFileName->szPath = NULL;
      if( ! zh_parl( 3 ) )
         pFileName->szExtension = NULL;

      if( ! zh_setGetTrimFileName() )
      {
         if( pFileName->szName )
         {
            nLen = strlen( pFileName->szName );
            nLen = zh_strRTrimLen( pFileName->szName, nLen, ZH_FALSE );
            pFileName->szName = zh_strLTrim( pFileName->szName, &nLen );
            ( ( char * ) ZH_UNCONST( pFileName->szName ) )[ nLen ] = '\0';
         }
         if( pFileName->szExtension )
         {
            nLen = strlen( pFileName->szExtension );
            nLen = zh_strRTrimLen( pFileName->szExtension, nLen, ZH_FALSE );
            pFileName->szExtension = zh_strLTrim( pFileName->szExtension, &nLen );
            ( ( char * ) ZH_UNCONST( pFileName->szExtension ) )[ nLen ] = '\0';
         }
      }

      zh_retc( zh_fsFNameMerge( szPathBuf, pFileName ) );
      zh_xfree( pFileName );
   }
   else
      zh_retc_null();
}
