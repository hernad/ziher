/*
 * Display build information
 *
 * Copyright 1999-2017 Viktor Szakats
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
#include "memory.zhh"

void zh_verBuildInfoCB( PZH_OUT_FUNC pOutFunc )
{
   ( pOutFunc )( "Ziher Build Info", 0 );
   ( pOutFunc )( zh_conNewLine(), 0 );
   ( pOutFunc )( "---------------------------", 0 );
   ( pOutFunc )( zh_conNewLine(), 0 );

   {
      char * pszVersion = zh_verZiher();
      ( pOutFunc )( "Version: ", 0 );
      ( pOutFunc )( pszVersion, 0 );
      ( pOutFunc )( zh_conNewLine(), 0 );
      zh_xfree( pszVersion );
   }

   {
      char * pszVersion = zh_verCompiler();
      ( pOutFunc )( "Compiler: ", 0 );
      ( pOutFunc )( pszVersion, 0 );
      ( pOutFunc )( zh_conNewLine(), 0 );
      zh_xfree( pszVersion );
   }

   {
      char * pszVersion = zh_verPlatform();
      ( pOutFunc )( "Platform: ", 0 );
      ( pOutFunc )( pszVersion, 0 );
      ( pOutFunc )( zh_conNewLine(), 0 );
      zh_xfree( pszVersion );
   }

   {
      char * pszPCode = zh_verPCode();
      ( pOutFunc )( pszPCode, 0 );
      ( pOutFunc )( zh_conNewLine(), 0 );
      zh_xfree( pszPCode );
   }

   ( pOutFunc )( "Commit info: ", 0 );
   ( pOutFunc )( zh_verCommitInfo(), 0 );
   ( pOutFunc )( zh_conNewLine(), 0 );

   ( pOutFunc )( "Commit ID: ", 0 );
   ( pOutFunc )( zh_verCommitID(), 0 );
   ( pOutFunc )( zh_conNewLine(), 0 );

   {
      const char * pszFlags = zh_verFlagsPRG();
      if( pszFlags && *pszFlags )
      {
         ( pOutFunc )( "Extra Ziher compiler options: ", 0 );
         ( pOutFunc )( pszFlags, 0 );
         ( pOutFunc )( zh_conNewLine(), 0 );
      }
   }

   {
      const char * pszFlags = zh_verFlagsC();
      if( pszFlags && *pszFlags )
      {
         ( pOutFunc )( "Extra C compiler options: ", 0 );
         ( pOutFunc )( pszFlags, 0 );
         ( pOutFunc )( zh_conNewLine(), 0 );
      }
   }

   {
      const char * pszFlags = zh_verFlagsL();
      if( pszFlags && *pszFlags )
      {
         ( pOutFunc )( "Extra linker options: ", 0 );
         ( pOutFunc )( pszFlags, 0 );
         ( pOutFunc )( zh_conNewLine(), 0 );
      }
   }

   ( pOutFunc )( "Build options:", 0 );
   if( zh_xquery( ZH_MEM_BLOCKS ) != 0 )
      ( pOutFunc )( " (memory tracking)", 0 );
#if defined( ZH_TR_INFO ) && ( ZH_TR_LEVEL == ZH_TR_INFO || ZH_TR_LEVEL == ZH_TR_DEBUG )
   ( pOutFunc )( " (tracing)", 0 );
#endif
#if ! defined( ZH_NO_PROFILER )
   ( pOutFunc )( " (profiler)", 0 );
#endif
#if defined( __cplusplus )
   ( pOutFunc )( " (C++ mode)", 0 );
#endif


   ( pOutFunc )( zh_conNewLine(), 0 );

   ( pOutFunc )( "---------------------------", 0 );
   ( pOutFunc )( zh_conNewLine(), 0 );
}

/* deprecated */
void zh_verBuildInfo( void )
{
   zh_verBuildInfoCB( zh_conOutErr );
}
