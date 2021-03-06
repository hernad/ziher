/*
 * Header file for compiler and runtime configuration
 *
 * Copyright 2000-2009 Viktor Szakats
 * Copyright 1999 Ryszard Glab <rglab@imid.med.pl>
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

#ifndef ZH_SETUP_H_
#define ZH_SETUP_H_

/*
 * Include settings common for .zh and .c files
 */

/*
 * Define PCODE version number
 * ZH_PCODE_VER_MIN define minimum supported PCODE by ZHVM
 */

#define ZH_PCODE_VER          0x0003
#define ZH_PCODE_VER_MIN      0x0002

/*
 * NOTE: You can select the default language module used by Ziher, by
 *       defining this to a valid language module identifier.
 */

#ifndef ZH_LANG_DEFAULT
   #define ZH_LANG_DEFAULT       EN
#endif

/*
 * NOTE: You can select the default codepage used by Ziher, by
 *       defining this to a valid codepage module identifier.
 */

#ifndef ZH_CODEPAGE_DEFAULT
   #define ZH_CODEPAGE_DEFAULT   EN
#endif

/*
 * Enable profiler support in ZHVM
 * By default this is turned off. Define ZH_USE_PROFILER to turn it on.
 */

#ifndef ZH_USE_PROFILER
   #define ZH_NO_PROFILER
#endif

/*
 * This symbol defines if Ziher is compiled using C compiler
 * that support strict ANSI C only
 *
 * The only non ANSI C feature that we are using is an ability
 * to call functions before the 'main' module is called.
 * This trick is used to automatically join all symbol tables defined
 * in run-time support modules and in user defined modules.
 *   If strict ANSI C compatibility is required then all symbol tables
 * have to be joined manually by calling special function named
 * zh_vm_SymbolInit_<module_name>
 * (for example for myfirst.zh it will be: 'zh_vm_SymbolInit_MYFIRST'
 * The generation of this function is performed by the macro called
 * ZH_CALL_ON_STARTUP that is defined in 'zhinit.h'
 *
 * By default we are using extensions to ANSI C (symbol is not defined)
 */
/* #define ZH_STRICT_ANSI_C */

/*
 * Define this option if you want the /y YACC trace option to be available
 * in the Ziher compiler.
 *
 * Note that if you turn this on, the compiler will slightly grow in size.
 *
 * By default this is turned off.
 */
/* #define ZH_YYDEBUG */

/*
 * Use native Windows memory allocation functions (ZH_OS_WIN)
 * This option can disable compiler memory allocation optimization
 * so you should really have a good reason to enable it
 */

/* #define ZH_FM_WIN_ALLOC */

/*
 * CPU detection
 */

/* Partially based on:
      https://sourceforge.net/p/predef/wiki/
      http://guest:guest123@poshlib.hookatooka.com/poshlib/trac.cgi/browser/posh.h
      [vszakats]
 */

#if defined( __amd64__ ) || \
      defined( __amd64 ) || \
      defined( __AMD64__ ) || \
      defined( __x86_64__ ) || \
      defined( __x86_64 ) || \
      defined( _M_AMD64 ) || \
      defined( _M_X64 ) || \
      defined( __MINGW64__ )
   #define ZH_CPU_X64  /* FUTURE */
   #define ZH_CPU_X86_64

#elif defined( __arm64__ )
   #define ZH_CPU_ARM_64

#elif defined( __arm__ ) || \
      defined( __arm ) || \
      defined( ARM ) || \
      defined( _ARM ) || \
      defined( _M_ARM )
   #define ZH_CPU_ARM

#elif defined( i386 ) || \
      defined( __i386__ ) || \
      defined( __i386 ) || \
      defined( __386__ ) || \
      defined( _M_IX86 ) || \
      defined( _M_I86 ) || \
      defined( M_I86 ) || \
      defined( __X86__ ) || \
      defined( _X86_ ) || \
      defined( __I86__ ) || \
      defined( __THW_INTEL__ ) || \
      defined( __INTEL__ )
   #define ZH_CPU_X86

#elif defined( __ia64__ ) || \
      defined( __ia64 ) || \
      defined( _IA64 ) || \
      defined( __IA64__ ) || \
      defined( _M_IA64 )
   #define ZH_CPU_IA_64

#elif defined( __m68k__ ) || \
      defined( M68000 )
   #define ZH_CPU_M68K

#elif defined( __mips__ ) || \
      defined( __mips ) || \
      defined( __MIPS__ ) || \
      defined( mips ) || \
      defined( _MIPS ) || \
      defined( __MIPS__ ) || \
      defined( _M_MRX000 ) || \
      defined( _M_MIPS )
   #define ZH_CPU_MIPS

#elif defined( __SYSC_ZARCH__ )
   #define ZH_CPU_ZARCH

#endif

/*
 * You can select here, what type of main entry will be used in the
 * application (main() or WinMain()).
 *
 * By default the standard C main() function will be used.
 */
/* #define ZH_MAIN_STD */
/* #define ZH_MAIN_WIN */

/* NOTE:
   Compiler                                _MSC_VER value
   --------                                --------------
   C Compiler version 6.0                  600
   C/C++ compiler version 7.0              700
   Visual C++, Windows, version 1.0        800
   Visual C++, 32-bit, version 1.0         800
   Visual C++, Windows, version 2.0        900
   Visual C++, 32-bit, version 2.x         900
   Visual C++, 32-bit, version 4.0         1000
   Visual C++, 32-bit, version 5.0         1100
   Visual C++, 32-bit, version 6.0         1200
   Visual Studio .NET (2002), version 7.0  1300
   Visual Studio .NET 2003, version 7.1    1310
   Visual Studio 2005, version 8.0         1400
   Visual Studio 2008, version 9.0         1500
   Visual Studio 2010, version 10.0        1600
   Visual Studio 2012, version 11.0        1700
   Visual Studio 2013, version 12.0        1800
   Visual Studio 2015, version 14.0        1900
   Visual Studio 2017, version 14.1        1910

   For newer versions, refer to this page:
      https://en.wikipedia.org/wiki/Microsoft_Visual_C%2B%2B#Internal_version_numbering
*/

/*
 * Platform detection
 *
 * Ref: http://nadeausoftware.com/articles/2012/01/c_c_tip_how_use_compiler_predefined_macros_detect_operating_system
 */


#ifndef ZH_OS_WIN
   #if defined( WINNT ) || defined( _Windows ) || defined( __NT__ ) || defined( _WIN32 ) || defined( _WINDOWS_ ) || defined( __WINDOWS_386__ ) || defined( __WIN32__ )
      #define ZH_OS_WIN
   #endif
#endif

/* Sub-option inside ZH_OS_WIN */
#ifndef ZH_OS_WIN_64
   #if defined( _WIN64 )
      #define ZH_OS_WIN_64
   #endif
#endif

#ifndef ZH_OS_LINUX
   #if defined( linux ) || defined( __linux ) || defined( __linux__ ) || defined( __gnu_linux__ ) || defined( __EMSCRIPTEN__ )
      #define ZH_OS_LINUX
   #endif
#endif


#ifndef ZH_OS_DARWIN
   #if defined( __APPLE__ ) || defined( __DARWIN__ )
      #define ZH_OS_DARWIN
   #endif
#endif


#ifndef ZH_OS_UNIX
   #if defined( ZH_OS_LINUX ) || \
       defined( ZH_OS_DARWIN ) || \
       defined( ZH_OS_ANDROID )
      #define ZH_OS_UNIX
   #endif
#endif


/*
 * Operating system specific definitions
 */
#if defined( ZH_OS_UNIX )
   #define ZH_OS_PATH_LIST_SEP_CHR      ':'
   #define ZH_OS_PATH_DELIM_CHR         '/'
   #define ZH_OS_PATH_DELIM_CHR_STRING  "/"
   #define ZH_OS_PATH_DELIM_CHR_LIST    "/"
   #define ZH_OS_ALLFILE_MASK           "*"
   #undef  ZH_OS_DRIVE_DELIM_CHR
   #undef  ZH_OS_HAS_DRIVE_LETTER
   #define ZH_OS_EOL_LEN                1
   #define ZH_OS_OPT_DELIM_LIST         "-"
   #define ZH_ISOPTSEP( c )             ( ( c ) == '-' )
#else
   /* we are assuming here an MS-DOS/Windows compatible OS */
   #define ZH_OS_PATH_LIST_SEP_CHR      ';'
   #define ZH_OS_PATH_DELIM_CHR         '\\'
   #define ZH_OS_PATH_DELIM_CHR_STRING  "\\"
   #define ZH_OS_PATH_DELIM_CHR_LIST    "\\/:"
   #define ZH_OS_ALLFILE_MASK           "*.*"
   #define ZH_OS_DRIVE_DELIM_CHR        ':'
   #define ZH_OS_HAS_DRIVE_LETTER
   #define ZH_OS_EOL_LEN                2  /* # of bytes in End of Line marker */
   #define ZH_OS_OPT_DELIM_LIST         "/-"
   #define ZH_ISOPTSEP( c )             ( ( c ) == '-' || ( c ) == '/' )
#endif

#define ZH_PATH_MAX     264 /* with trailing 0 byte */

/*
 * Here you can force the EOL string to be CRLF
 *
 * By default, the EOL string depends upon the detected platform.
 */
/* #define ZH_EOL_CRLF */
#ifdef ZH_EOL_CRLF
   #undef ZH_OS_EOL_LEN
   #define ZH_OS_EOL_LEN 2
#endif

/*
 * See also the following files for task specific definitions/settings
 *
 * zhmather.h    - math errors handling
 */

/*
 * Extern "C" detection
 */

#if defined( __cplusplus )
   #define ZH_EXTERN_C        extern "C"
   #define ZH_EXTERN_BEGIN    extern "C" {
   #define ZH_EXTERN_END      }
#else
   #define ZH_EXTERN_C
   #define ZH_EXTERN_BEGIN
   #define ZH_EXTERN_END
#endif

#define ZH_PP_VALTOSTR( x )    #x
#define ZH_PP_VAL( x )         ZH_PP_VALTOSTR( x )
#define ZH_PP_VALDEBUG( var )  #var "=" ZH_PP_VAL( var )

#if defined( __clang__ )
   #define ZH_GCC_HAS_DIAG
   #define ZH_GCC_VER  0
#elif defined( __GNUC__ )
   #define ZH_GCC_VER  ( ( ( __GNUC__ - 0 ) * 100 ) + ( __GNUC_MINOR__ - 0 ) )
#  if ZH_GCC_VER >= 406
      #define ZH_GCC_HAS_DIAG
#  else
      #undef ZH_GCC_HAS_DIAG
#  endif
#  if ZH_GCC_VER >= 404
      #define ZH_GCC_HAS_OPTIMIZE
#  else
      #undef ZH_GCC_HAS_OPTIMIZE
#  endif
#else
   #define ZH_GCC_VER  0
#endif

#if defined( __GNUC__ ) && ( __GNUC__ - 0 >= 3 )

   #define ZH_DEPRECATED __attribute__ (( __deprecated__ ))

   #define ZH_PRINTF_FORMAT( _nStr, _nParam ) \
                     __attribute__ (( format (printf, _nStr, _nParam)))
   #define ZH_MALLOC_ATTR \
                     __attribute__ (( malloc ))
   #define ZH_PURE_ATTR \
                     __attribute__ (( pure ))
   #define ZH_CONST_ATTR \
                     __attribute__ (( const ))

   #define ZH_NORETURN_ATTR

#  if ( ( __GNUC__ > 4 ) || ( __GNUC__ == 4 && __GNUC_MINOR__ >= 1 ) ) && \
      ! defined( ZH_NO_FLATTEN )
   #define ZH_FLATTEN_ATTR \
                     __attribute__ (( flatten ))
#  else
   #define ZH_FLATTEN_ATTR
#  endif
#  if ( ( __GNUC__ > 4 ) || ( __GNUC__ == 4 && __GNUC_MINOR__ >= 3 ) ) && \
      ! defined( __ICC ) && ! defined( __OPENCC__ )
   #define ZH_ALLOC_SIZE_ATTR( _nParam ) \
                     __attribute__ (( alloc_size (_nParam)))
   #define ZH_HOT_ATTR \
                     __attribute__ (( hot ))
   #define ZH_COLD_ATTR \
                     __attribute__ (( cold ))
#  else
   #define ZH_ALLOC_SIZE_ATTR( _nParam )
   #define ZH_HOT_ATTR
   #define ZH_COLD_ATTR
#  endif
   #define ZH_RESTRICT  __restrict

#else
   #define ZH_DEPRECATED
   #define ZH_PRINTF_FORMAT( _nStr, _nParam )
   #define ZH_MALLOC_ATTR
   #define ZH_NORETURN_ATTR
   #define ZH_HOT_ATTR
   #define ZH_COLD_ATTR
   #define ZH_PURE_ATTR
   #define ZH_CONST_ATTR
   #define ZH_FLATTEN_ATTR
   #define ZH_ALLOC_SIZE_ATTR( _nParam )
   #define ZH_RESTRICT
#endif

#if defined( __GNUC__ )
   #define _ZH_INLINE_  __inline__
#elif defined( _MSC_VER )
   #define _ZH_INLINE_  __inline
#else /* __cplusplus */
   #define _ZH_INLINE_  inline
#endif

#if defined( __GNUC__ ) && \
    ( ( __GNUC__ > 3 ) || ( ( __GNUC__ == 3 ) && ( __GNUC_MINOR__ >= 2 ) ) )
   #define ZH_FORCEINLINE     __inline__ __attribute__((always_inline))
#elif ( defined( _MSC_VER ) && ( _MSC_VER >= 1200 ) )
   #define ZH_FORCEINLINE     __forceinline
#elif defined( FORCEINLINE )
   #define ZH_FORCEINLINE     FORCEINLINE
#else
   #define ZH_FORCEINLINE     _ZH_INLINE_
#endif

#endif /* ZH_SETUP_H_ */
