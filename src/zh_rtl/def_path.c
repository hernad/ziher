/*
 * __DefPath() function
 *
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
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
#include "zh_set.h"

ZH_FUNC( __DEFPATH )
{
   char buffer[ ZH_PATH_MAX - 1 + 2 ];
   const char * szDefault = zh_setGetDefault();
   int size = 0;

   if( szDefault )
   {
      /* Leave enough space to append a path delimiter */
      zh_strncpy( buffer, szDefault, sizeof( buffer ) - 1 );
      size = ( int ) strlen( buffer );
   }

   ZH_TRACE( ZH_TR_INFO, ( "ZH_DEFPATH: buffer is |%s|, size is %d, last char is |%c|", buffer, size, buffer[ size - 1 ] ) );
   ZH_TRACE( ZH_TR_INFO, ( "ZH_DEFPATH: ZH_OS_PATH_DELIM_CHR is |%c| and ZH_OS_PATH_LIST_SEP_CHR is |%c|", ZH_OS_PATH_DELIM_CHR, ZH_OS_PATH_LIST_SEP_CHR ) );

   /* If the path is not empty and it doesn't end with a drive or path
      delimiter, then add the appropriate separator. Use ':' if the size
      of the path is 1 and the list separator is not ':', otherwise use
      the path delimiter. This allows the use of a drive letter delimiter
      for DOS compatible operating systems while preventing it from being
      with a Unix compatible OS. */
#ifdef ZH_OS_HAS_DRIVE_LETTER
   if( size && buffer[ size - 1 ] != ZH_OS_PATH_DELIM_CHR &&
               buffer[ size - 1 ] != ZH_OS_DRIVE_DELIM_CHR )
   {
      if( size == 1 )
         buffer[ size++ ] = ZH_OS_DRIVE_DELIM_CHR;
      else
         buffer[ size++ ] = ZH_OS_PATH_DELIM_CHR;
   }
#else
   if( size && buffer[ size - 1 ] != ZH_OS_PATH_DELIM_CHR )
      buffer[ size++ ] = ZH_OS_PATH_DELIM_CHR;
#endif

   zh_retclen( buffer, size );
}
