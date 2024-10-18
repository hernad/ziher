/*
 * Header file for automatic static initialization
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

#ifndef ZH_INIT_H_
#define ZH_INIT_H_

#include "zh_setup.h"

ZH_EXTERN_BEGIN

extern ZH_EXPORT PZH_SYMBOL zh_vmProcessSymbols( PZH_SYMBOL pSymbols, ZH_USHORT uiSymbols, const char * szModuleName, ZH_ULONG ulID, ZH_USHORT uiPcodeVer ); /* module symbols initialization with extended information */

#define ZH_INIT_SYMBOLS_END( func ) ZH_INIT_SYMBOLS_EX_END( func, "", 0L, 0x0000 )

/* By default in all C++ builds use static variable initialization as startup
   code with the exception for GCC which new versions show warning about
   defined but not used static variable initialized with this method. */
#if defined( __cplusplus ) && ! defined( ZH_STATIC_STARTUP ) && \
    ! defined( ZH_GNUC_STARTUP ) && \
    ! defined( ZH_INITSEG_STARTUP ) && ! defined( ZH_DATASEG_STARTUP ) && \
    ! defined( __GNUC__ )
   #define ZH_STATIC_STARTUP
#endif

#define ZH_INIT_SYMBOLS_COUNT ( sizeof( symbols_table ) / sizeof( ZH_SYMBOL ) )


#if defined( ZH_GNUC_STARTUP ) || \
      defined( __GNUC__ ) || \
      defined( __clang__ )

   // https://stackoverflow.com/questions/2053029/how-exactly-does-attribute-constructor-work
   // It runs when a shared library is loaded, typically during program startup.
   // That's how all GCC attributes are; presumably to distinguish them from function calls.
   // GCC-specific syntax.
   // Yes, this works in C and C++.
   // No, the function does not need to be static.
   // The destructor runs when the shared library is unloaded, typically at program exit.

   // https://stackoverflow.com/questions/64897476/why-does-the-gcc-compiler-not-support-pragma-startup-and-pragma-exit-directive?rq=1

   #if defined( ZH_DATASEG_STARTUP )
      #error Wrong macros set for startup code - clean your make/env settings.
   #endif

   #define ZH_INIT_SYMBOLS_BEGIN( func ) \
      static ZH_SYMBOL symbols_table[] = {

   #define ZH_INIT_SYMBOLS_EX_END( func, module, id, vpcode ) \
      }; \
      static PZH_SYMBOL symbols = symbols_table; \
      static void __attribute__ ((constructor)) func( void ) \
      { \
         symbols = zh_vmProcessSymbols( symbols_table, ( ZH_USHORT ) ZH_INIT_SYMBOLS_COUNT, (module), (id), (vpcode) ); \
      } \
      ZH_EXTERN_BEGIN \
      void ext_##func( void ) \
      { \
         func(); \
      } \
      ZH_EXTERN_END

   #define ZH_CALL_ON_STARTUP_BEGIN( func ) \
      static void __attribute__ ((constructor)) func( void ) \
      {

   #define ZH_CALL_ON_STARTUP_END( func ) \
      }

   #define ZH_CALL_ON_STARTUP_EXT_END( func ) \
      } \
      ZH_EXTERN_BEGIN \
      void ext_##func( void ) {\
         func();\
      } \
      ZH_EXTERN_END

#elif defined( _MSC_VER )

   #define ZH_DATASEG_STARTUP

   #if _MSC_VER >= 1010
      #define ZH_STARTUP_SEGMENT    ".CRT$XIY"
   #else
      #define ZH_STARTUP_SEGMENT    "XIY"
   #endif

   #define ZH_INIT_SYMBOLS_BEGIN( func ) \
      static ZH_SYMBOL symbols_table[] = {

   #define ZH_INIT_SYMBOLS_EX_END( func, module, id, vpcode ) \
      }; \
      static PZH_SYMBOL symbols = symbols_table; \
      static int func( void ) \
      { \
         symbols = zh_vmProcessSymbols( symbols_table, ( ZH_USHORT ) ZH_INIT_SYMBOLS_COUNT, (module), (id), (vpcode) ); \
         return 0; \
      }; \
      ZH_EXTERN_BEGIN \
      void ext_##func( void ) \
      { \
         symbols = zh_vmProcessSymbols( symbols_table, ( ZH_USHORT ) ZH_INIT_SYMBOLS_COUNT, (module), (id), (vpcode) ); \
         return 0; \
      } \
      ZH_EXTERN_END


   #define ZH_CALL_ON_STARTUP_BEGIN( func ) \
      static int func( void ) \
      {

   #define ZH_CALL_ON_STARTUP_END( func ) \
         return 0; \
      }

   typedef int (* ZH_$INITSYM)( void );

   #define ZH_DATASEG_FUNC( func )     ZH_DATASEG_FUNC_( func )
   #define ZH_DATASEG_FUNC_( func ) \
      static ZH_$INITSYM _s_init_func_##func = func;

   /*  After each '*_END' symbol, additional 'hooks' are required
    *  See the C output of a generated .zh for example
    */

#else
   #error Unknown initialization method.
#endif

ZH_EXTERN_END

#endif /* ZH_INIT_H_ */
