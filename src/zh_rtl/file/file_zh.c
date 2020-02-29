/*
 * File() function
 *
 * Copyright 1999-2002 Viktor Szakats (vszakats.net/harbour)
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
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "zh_api.h"
#include "zh_fs_api.h"

ZH_FUNC( FILE )
{
   const char * szFile = zh_parc( 1 );

   zh_retl( szFile ? zh_spFile( szFile, NULL ) : ZH_FALSE );
}


#include "zh_api.h"
#include "zh_fs_api.h"

ZH_FUNC( FILEBASE )
{
   const char * szPath = zh_parc( 1 );
   if( szPath )
   {
      PZH_FNAME pFileName = zh_fsFNameSplit( szPath );
      zh_retc( pFileName->szName );
      zh_xfree( pFileName );
   }
   else
      zh_retc_null();
}

/* FileExt( <cFile> ) --> cFileExt
*/
ZH_FUNC( FILEEXT )
{
   const char * szPath = zh_parc( 1 );
   if( szPath )
   {
      PZH_FNAME pFileName = zh_fsFNameSplit( szPath );
      if( pFileName->szExtension != NULL )
         zh_retc( pFileName->szExtension + 1 ); /* Skip the dot */
      else
         zh_retc_null();
      zh_xfree( pFileName );
   }
   else
      zh_retc_null();
}

