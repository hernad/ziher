/*
 * Import library for PCODE DLLs
 *
 * Copyright 2010 Przemyslaw Czerpak
 * This code uses ZH_DLL_NAME* macros defined by
 *    Viktor Szakats
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

#include "hbtypes.h"
#include "zh_vm.h"

#if defined( ZH_OS_WIN )
   #include <windows.h>
#endif

#define ZH_DLL_PREF      TEXT( "ziher" )
#define ZH_DLL_VER       TEXT( "-" ) TEXT( ZH_MACRO2STRING( ZH_VER_MAJOR ) ) TEXT( ZH_MACRO2STRING( ZH_VER_MINOR ) )
#define ZH_DLL_EXT       TEXT( ".dll" )

#define ZH_DLL_NAME      ZH_DLL_PREF ZH_DLL_EXT

#if defined( ZH_OS_WIN_64 ) && defined( ZH_CPU_X86_64 )
   #define ZH_DLL_NAME2  ZH_DLL_PREF ZH_DLL_VER TEXT( "-x64" ) ZH_DLL_EXT
#elif defined( ZH_OS_WIN_64 ) && defined( ZH_CPU_IA_64 )
   #define ZH_DLL_NAME2  ZH_DLL_PREF ZH_DLL_VER TEXT( "-ia64" ) ZH_DLL_EXT
#else
   #define ZH_DLL_NAME2  ZH_DLL_PREF ZH_DLL_VER ZH_DLL_EXT
#endif

#if defined( ZH_OS_WIN )

ZH_EXTERN_BEGIN

#define ZH_DLL_MSG_NO_FUNC( func )  \
   do \
   { \
      MessageBox( NULL, \
                  TEXT( "Function '" ) TEXT( func ) TEXT( "' not found!" ), \
                  TEXT( func ), \
                  MB_OK | MB_ICONERROR ); \
   } while( 0 )

typedef PZH_FUNC ( *ZH_PROC_GET )( const char * szFuncName );

/* zh_vmProcessSymbols() */
typedef PZH_SYMBOL ( *ZH_VM_PROCESS_SYMBOLS )
   ( PZH_SYMBOL pModuleSymbols, ZH_USHORT uiModuleSymbols,
   const char * szModuleName, ZH_ULONG ulID,
   ZH_USHORT uiPcodeVer );
static PZH_SYMBOL s_vmProcessSymbols( PZH_SYMBOL pSymbols, ZH_USHORT uiSymbols,
                                    const char * szModuleName, ZH_ULONG ulID,
                                    ZH_USHORT uiPcodeVer );
static ZH_VM_PROCESS_SYMBOLS s_pProcessSymbols = s_vmProcessSymbols;


/* zh_vmExecute() */
typedef void ( *ZH_VM_EXECUTE )( const ZH_BYTE * pCode, PZH_SYMBOL pSymbols );
static void s_vmExecute( const ZH_BYTE * pCode, PZH_SYMBOL pSymbols );
static ZH_VM_EXECUTE s_pExecute = s_vmExecute;


PZH_FUNC zh_dllGetProcAddress( const char * szProcName )
{
   static ZH_PROC_GET s_pProcGet = NULL;
   static HMODULE     s_hModule  = NULL;

   if( s_hModule == NULL )
   {
      s_hModule = GetModuleHandle( ZH_DLL_NAME );
      if( s_hModule == NULL )
         s_hModule = GetModuleHandle( ZH_DLL_NAME2 );
      if( s_hModule == NULL )
         s_hModule = GetModuleHandle( NULL );

      if( s_hModule != NULL )
      {
         int i = 5;

         do
         {
            static const char * s_szGetProcAddr = "_dll_zh_vmProcAddress";
            s_pProcGet = ( ZH_PROC_GET ) GetProcAddress( s_hModule, s_szGetProcAddr + i );
         }
         while( s_pProcGet == NULL && ( i -= i == 4 ? 3 : 1 ) >= 0 );
         if( s_pProcGet == NULL )
            ZH_DLL_MSG_NO_FUNC( "zh_vmProcAddress" );
      }
   }

   return s_pProcGet ? s_pProcGet( szProcName ) : NULL;
}


BOOL WINAPI ZH_DLL_ENTRY_POINT( HINSTANCE hInstance, DWORD dwReason, PVOID pvReserved )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "DllEntryPoint(%p, %lu, %p)", hInstance, dwReason, pvReserved ) );

   ZH_SYMBOL_UNUSED( hInstance );
   ZH_SYMBOL_UNUSED( dwReason );
   ZH_SYMBOL_UNUSED( pvReserved );

   return TRUE;
}


static PZH_SYMBOL s_dummy_vmProcessSymbols( PZH_SYMBOL pSymbols, ZH_USHORT uiSymbols,
                                          const char * szModuleName, ZH_ULONG ulID,
                                          ZH_USHORT uiPcodeVer )
{
   ZH_SYMBOL_UNUSED( uiSymbols );
   ZH_SYMBOL_UNUSED( szModuleName );
   ZH_SYMBOL_UNUSED( ulID );
   ZH_SYMBOL_UNUSED( uiPcodeVer );

   return pSymbols;
}

static PZH_SYMBOL s_vmProcessSymbols( PZH_SYMBOL pSymbols, ZH_USHORT uiSymbols,
                                    const char * szModuleName, ZH_ULONG ulID,
                                    ZH_USHORT uiPcodeVer )
{
   ZH_VM_PROCESS_SYMBOLS pProcessSymbols = ( ZH_VM_PROCESS_SYMBOLS )
                                           zh_dllGetProcAddress( "zh_vmProcessSymbols" );

   if( pProcessSymbols )
   {
      s_pProcessSymbols = pProcessSymbols;
      return s_pProcessSymbols( pSymbols, uiSymbols, szModuleName, ulID, uiPcodeVer );
   }
   else
   {
      s_pProcessSymbols = s_dummy_vmProcessSymbols;
      ZH_DLL_MSG_NO_FUNC( "zh_vmProcessSymbols" );
      return pSymbols;
   }
}

/*
PZH_SYMBOL zh_vmProcessSymbols( PZH_SYMBOL pSymbols, ZH_USHORT uiSymbols,
                              const char * szModuleName, ZH_ULONG ulID,
                              ZH_USHORT uiPcodeVer )
{
   return s_pProcessSymbols( pSymbols, uiSymbols, szModuleName, ulID, uiPcodeVer );
}
*/

static void s_dummy_vmExecute( const ZH_BYTE * pCode, PZH_SYMBOL pSymbols )
{
   ZH_SYMBOL_UNUSED( pCode );
   ZH_SYMBOL_UNUSED( pSymbols );
}

static void s_vmExecute( const ZH_BYTE * pCode, PZH_SYMBOL pSymbols )
{
   ZH_VM_EXECUTE pExecute = ( ZH_VM_EXECUTE )
                            zh_dllGetProcAddress( "zh_vmExecute" );

   if( pExecute )
   {
      s_pExecute = pExecute;
      s_pExecute( pCode, pSymbols );
   }
   else
   {
      s_pExecute = s_dummy_vmExecute;
      ZH_DLL_MSG_NO_FUNC( "zh_vmExecute" );
   }
}

void zh_vmExecute( const ZH_BYTE * pCode, PZH_SYMBOL pSymbols )
{
   s_pExecute( pCode, pSymbols );
}

ZH_EXTERN_END

#endif /* ZH_OS_WIN */
