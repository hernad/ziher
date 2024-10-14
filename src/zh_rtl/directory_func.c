/*
 * DirChange(), MakeDir(), DirRemove(), IsDisk(), DiskChange(), DiskName() functions
 *
 * Copyright 1999-2001 Viktor Szakats
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

ZH_FUNC( ZH_DIRCREATE )
{
   if( ZH_ISCHAR( 1 ) )
      zh_retni( zh_fsMkDir( zh_parc( 1 ) ) ? 0 : zh_fsError() );
   else
      zh_retni( F_ERROR );
}

ZH_FUNC( ZH_DIRDELETE )
{
   if( ZH_ISCHAR( 1 ) )
      zh_retni( zh_fsRmDir( zh_parc( 1 ) ) ? 0 : zh_fsError() );
   else
      zh_retni( F_ERROR );
}


ZH_FUNC( DIRCHANGE )
{
   if( ZH_ISCHAR( 1 ) )
      zh_retni( zh_fsChDir( zh_parc( 1 ) ) ? 0 : zh_fsError() );
   else
      zh_retni( F_ERROR );
}

ZH_FUNC_TRANSLATE( MAKEDIR, ZH_DIRCREATE )
ZH_FUNC_TRANSLATE( DIRREMOVE, ZH_DIRDELETE )


ZH_FUNC( ISDISK )
{
   ZH_BOOL fResult = ZH_FALSE;
   const char * szDrive = zh_parc( 1 );

   if( szDrive )
   {
      if( *szDrive >= 'A' && *szDrive <= 'Z' )
         fResult = zh_fsIsDrv( *szDrive - 'A' ) == 0;
      else if( *szDrive >= 'a' && *szDrive <= 'z' )
         fResult = zh_fsIsDrv( *szDrive - 'a' ) == 0;
   }
   zh_retl( fResult );
}

ZH_FUNC( DISKCHANGE )
{
   ZH_BOOL fResult = ZH_FALSE;
   const char * szDrive = zh_parc( 1 );

   if( szDrive )
   {
      if( *szDrive >= 'A' && *szDrive <= 'Z' )
         fResult = zh_fsChDrv( *szDrive - 'A' ) == 0;
      else if( *szDrive >= 'a' && *szDrive <= 'z' )
         fResult = zh_fsChDrv( *szDrive - 'a' ) == 0;
   }
   zh_retl( fResult );
}

ZH_FUNC( DISKNAME )
{
#if defined( ZH_OS_HAS_DRIVE_LETTER )
   char szDrive[ 1 ];

   szDrive[ 0 ] = ( ( char ) zh_fsCurDrv() ) + 'A';
   zh_retclen( szDrive, 1 );
#else
   zh_retc_null();
#endif
}

