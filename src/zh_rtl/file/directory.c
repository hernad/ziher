/*
 * Directory() function
 *
 * Copyright 1999 Leslee Griffith <les.griffith@vantagesystems.ca>
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
#include "zh_fs_api.h"
#include "zh_item_api.h"

#include "../directory.zhh"

PZH_ITEM zh_fsDirectory( const char * pszDirSpec, const char * pszAttributes, ZH_BOOL fDateTime )
{
   PZH_ITEM  pDir = zh_itemArrayNew( 0 );
   char *    pszFree = NULL;
   PZH_FFIND ffind;
   ZH_FATTR  ulMask;

   /* Get the passed attributes and convert them to Ziher Flags */

   ulMask = ZH_FA_ARCHIVE | ZH_FA_READONLY;

   if( pszAttributes && *pszAttributes )
      ulMask |= zh_fsAttrEncode( pszAttributes );

   if( pszDirSpec && *pszDirSpec )
   {
      if( ulMask != ZH_FA_LABEL )
      {
         /* CA-Cl*pper compatible behavior - add all file mask when
          * last character is directory or drive separator
          */
         ZH_SIZE nLen = strlen( pszDirSpec ) - 1;
#ifdef ZH_OS_HAS_DRIVE_LETTER
         if( pszDirSpec[ nLen ] == ZH_OS_PATH_DELIM_CHR ||
             pszDirSpec[ nLen ] == ZH_OS_DRIVE_DELIM_CHR )
#else
         if( pszDirSpec[ nLen ] == ZH_OS_PATH_DELIM_CHR )
#endif
            pszDirSpec = pszFree =
                           zh_xstrcpy( NULL, pszDirSpec, ZH_OS_ALLFILE_MASK, NULL );
      }
   }
   else
      pszDirSpec = ZH_OS_ALLFILE_MASK;

   /* Get the file list */

   if( ( ffind = zh_fsFindFirst( pszDirSpec, ulMask ) ) != NULL )
   {
      PZH_ITEM pSubarray = zh_itemNew( NULL );

      do
      {
         char buffer[ 32 ];

         zh_arrayNew    ( pSubarray, F_LEN );
         zh_arraySetC   ( pSubarray, F_NAME, ffind->szName );
         zh_arraySetNInt( pSubarray, F_SIZE, ffind->size );
         zh_arraySetC   ( pSubarray, F_TIME, ffind->szTime );
         zh_arraySetC   ( pSubarray, F_ATTR, zh_fsAttrDecode( ffind->attr, buffer ) );

         if( fDateTime )
            zh_arraySetTDT( pSubarray, F_DATE, ffind->lDate, ffind->lTime );
         else
            zh_arraySetDL ( pSubarray, F_DATE, ffind->lDate );

         /* Don't exit when array limit is reached */
         zh_arrayAddForward( pDir, pSubarray );
      }
      while( zh_fsFindNext( ffind ) );

      zh_itemRelease( pSubarray );

      zh_fsFindClose( ffind );
   }

   if( pszFree )
      zh_xfree( pszFree );

   return pDir;
}

ZH_FUNC( DIRECTORY )
{
   zh_itemReturnRelease( zh_fsDirectory( zh_parc( 1 ), zh_parc( 2 ), ZH_FALSE ) );
}

ZH_FUNC( ZH_DIRECTORY )
{
   zh_itemReturnRelease( zh_fsDirectory( zh_parc( 1 ), zh_parc( 2 ), ZH_TRUE ) );
}
