/*
 * zh_fsFile() function
 *
 * Copyright 1999-2002 Viktor Szakats
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

ZH_BOOL zh_fsFile( const char * pszFileName )
{
   PZH_FFIND ffind;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsFile(%s)", pszFileName ) );

   if( ( ffind = zh_fsFindFirst( pszFileName, ZH_FA_ALL ) ) != NULL )
   {
      zh_fsFindClose( ffind );
      return ZH_TRUE;
   }

   return ZH_FALSE;
}

ZH_BOOL zh_fsIsDirectory( const char * pszFileName )
{
   ZH_BOOL bResult = ZH_FALSE;
   char * pszFree = NULL;
   int iLen;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsIsDirectory(%s)", pszFileName ) );

   iLen = ( int ) strlen( pszFileName );
   while( iLen && strchr( ZH_OS_PATH_DELIM_CHR_LIST, pszFileName[ iLen - 1 ] ) )
      --iLen;

   if( pszFileName[ iLen ] )
      pszFileName = pszFree = zh_strndup( pszFileName, iLen );

   if( iLen && iLen <= ( ZH_PATH_MAX - 1 ) )
   {
      PZH_FFIND ffind;
      if( ( ffind = zh_fsFindFirst( pszFileName, ZH_FA_DIRECTORY ) ) != NULL )
      {
         do
         {
            if( ( ffind->attr & ZH_FA_DIRECTORY ) == ZH_FA_DIRECTORY )
            {
               bResult = ZH_TRUE;
               break;
            }
         }
         while( zh_fsFindNext( ffind ) );
         zh_fsFindClose( ffind );
      }
   }

   if( pszFree )
      zh_xfree( pszFree );

   return bResult;
}
