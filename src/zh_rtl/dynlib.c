/*
 * Dynamic link libraries management functions
 *
 * Copyright 2001 Antonio Linares <alinares@fivetech.com>
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

#define INCL_DOSMODULEMGR
#define INCL_ERRORS

#include "zh_vm_int.h"
#include "zh_api.h"
#include "zh_item_api.h"
#include "zh_stack.h"
#include "zh_vm.h"

#if defined( ZH_OS_WIN )
# include "zh_win_unicode.h"
# include <windows.h>
#  endif


#if ! defined( ZH_HAS_DLFCN ) && defined( ZH_OS_LINUX )  || defined( ZH_OS_DARWIN )
#  define ZH_HAS_DLFCN
#endif

#if defined( ZH_HAS_DLFCN )
#  include <dlfcn.h>
#endif

static ZH_GARBAGE_FUNC( zh_libRelease )
{
   /* do nothing */
   ZH_SYMBOL_UNUSED( Cargo );
}

static const ZH_GC_FUNCS s_gcDynlibFuncs =
{
   zh_libRelease,
   zh_gcDummyMark
};

PZH_ITEM zh_libLoad( PZH_ITEM pLibName, PZH_ITEM pArgs )
{
   void * hDynLib = NULL;

   if( zh_itemGetCLen( pLibName ) > 0 )
   {
      int argc = pArgs ? ( int ) zh_arrayLen( pArgs ) : 0, i;
      const char ** argv = NULL;

      if( argc > 0 )
      {
         argv = ( const char ** ) zh_xgrab( sizeof( char * ) * argc );
         for( i = 0; i < argc; ++i )
            argv[ i ] = zh_arrayGetCPtr( pArgs, i + 1 );
      }

      if( zh_vmLockModuleSymbols() )
      {
         /* use stack address as first level marker */
         zh_vmBeginSymbolGroup( ( void * ) zh_stackId(), ZH_TRUE );
#if defined( ZH_OS_WIN )
         {
            void * hFileName;

            hDynLib = ( void * ) LoadLibraryEx( ZH_ITEMGETSTR( pLibName, &hFileName, NULL ), NULL, LOAD_WITH_ALTERED_SEARCH_PATH );

            zh_strfree( hFileName );
         }
#elif defined( ZH_HAS_DLFCN )
         hDynLib = ( void * ) dlopen( zh_itemGetCPtr( pLibName ), RTLD_LAZY | RTLD_GLOBAL );

         if( ! hDynLib )
         {
            ZH_TRACE( ZH_TR_DEBUG, ( "zh_libLoad(): dlopen(): %s", dlerror() ) );
         }
#elif defined( ZH_CAUSEWAY_DLL )
         hDynLib = LoadLibrary( zh_itemGetCPtr( pLibName ) );
#else
         {
            int iTODO;
         }
#endif
         /* set real marker */
         zh_vmInitSymbolGroup( hDynLib, argc, argv );

         zh_vmUnlockModuleSymbols();
      }

      if( argv )
         zh_xfree( ( void * ) argv );
   }

   if( hDynLib )
   {
      void ** pLibPtr = ( void ** ) zh_gcAllocate( sizeof( void * ), &s_gcDynlibFuncs );
      *pLibPtr = hDynLib;
      return zh_itemPutPtrGC( NULL, pLibPtr );
   }

   return NULL;
}

ZH_BOOL zh_libFree( PZH_ITEM pDynLib )
{
   ZH_BOOL fResult = ZH_FALSE;
   void ** pDynLibPtr = ( void ** ) zh_itemGetPtrGC( pDynLib, &s_gcDynlibFuncs );

   if( pDynLibPtr && *pDynLibPtr &&
       zh_vmLockModuleSymbols() )
   {
      void * hDynLib = *pDynLibPtr;
      if( hDynLib )
      {
         *pDynLibPtr = NULL;
         zh_vmExitSymbolGroup( hDynLib );
#if defined( ZH_OS_WIN )
         fResult = FreeLibrary( ( HMODULE ) hDynLib );
#elif defined( ZH_HAS_DLFCN )
         fResult = dlclose( hDynLib ) == 0;
#elif defined( ZH_CAUSEWAY_DLL )
         FreeLibrary( hDynLib );
         fResult = ZH_TRUE;
#endif
      }
      zh_vmUnlockModuleSymbols();
   }

   return fResult;
}

void * zh_libHandle( PZH_ITEM pDynLib )
{
   void ** pDynLibPtr = ( void ** ) zh_itemGetPtrGC( pDynLib, &s_gcDynlibFuncs );

   return pDynLibPtr ? *pDynLibPtr : NULL;
}

void * zh_libSymAddr( PZH_ITEM pDynLib, const char * pszSymbol )
{
   void * hDynLib = zh_libHandle( pDynLib );

   if( hDynLib )
   {
#if defined( ZH_OS_WIN )
      return ( void * ) GetProcAddress( ( HMODULE ) hDynLib, pszSymbol );
#elif defined( ZH_HAS_DLFCN )
      return dlsym( hDynLib, pszSymbol );
#elif defined( ZH_CAUSEWAY_DLL )
      return GetProcAddress( hDynLib, pszSymbol );
#else
      ZH_SYMBOL_UNUSED( pszSymbol );
#endif
   }
   return NULL;
}

ZH_FUNC( ZH_LIBLOAD )
{
   int iPCount = zh_pcount(), i;
   PZH_ITEM pArgs = NULL;

   if( iPCount > 1 )
   {
      pArgs = zh_itemArrayNew( iPCount - 1 );
      for( i = 2; i <= iPCount; ++i )
         zh_arraySet( pArgs, i, zh_param( i, ZH_IT_ANY ) );
   }

   zh_itemReturnRelease( zh_libLoad( zh_param( 1, ZH_IT_ANY ), pArgs ) );

   if( pArgs )
      zh_itemRelease( pArgs );
}

ZH_FUNC( ZH_LIBFREE )
{
   zh_retl( zh_libFree( zh_param( 1, ZH_IT_ANY ) ) );
}

ZH_FUNC( ZH_LIBERROR )
{
#if defined( ZH_HAS_DLFCN )
   zh_retc( dlerror() );
#else
   zh_retc_null();
#endif
}

/* Get FUNCTION or PROCEDURE symbol from given library.
 *    zh_libGetFunSym( <pLibHandle>, <cFuncName> ) --> <sFuncSym> | NIL
 */
ZH_FUNC( ZH_LIBGETFUNSYM )
{
   const char * szFuncName = zh_parc( 2 );

   if( szFuncName )
   {
      void * hDynLib = zh_libHandle( zh_param( 1, ZH_IT_ANY ) );

      if( hDynLib )
      {
         PZH_SYMB pSym = zh_vmFindFuncSym( szFuncName, hDynLib );

         if( pSym )
            zh_itemPutSymbol( zh_stackReturnItem(), pSym );
      }
   }
}
