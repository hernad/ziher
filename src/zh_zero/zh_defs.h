/*
 * Header file for compiler and runtime basic type declarations
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
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

#ifndef ZH_DEFS_H_
#define ZH_DEFS_H_

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#include "zh_setup.h"
#include "zh_ver.h"

#if defined( __POCC__ ) || defined( __MINGW32__ ) || \
    ( defined( _MSC_VER ) && _MSC_VER >= 1600 ) || \
    ( defined( __BORLANDC__ ) && __BORLANDC__ >= 0x0582 ) || \
    ( defined( __WATCOMC__ ) && __WATCOMC__ >= 1270 ) || \
    ( ( defined( __GNUC__ ) || defined( __SUNPRO_C ) || defined( __SUNPRO_CC ) ) && \
      ( defined( _ISOC99_SOURCE ) || defined( _STDC_C99 ) || \
        ( defined( __STDC_VERSION__ ) && __STDC_VERSION__ >= 199901L ) || \
        ( defined( __DJGPP__ ) && \
          ( __DJGPP__ > 2 || ( __DJGPP__ == 2 && __DJGPP_MINOR__ >= 4 ) ) ) || \
        defined( ZH_OS_LINUX ) || defined( ZH_OS_DARWIN ) || \
        defined( ZH_OS_BSD ) || defined( ZH_OS_SUNOS ) || \
        defined( ZH_OS_BEOS ) || defined( ZH_OS_QNX ) || \
        defined( ZH_OS_VXWORKS ) || defined( ZH_OS_MINIX ) ) )
#  include <stdint.h>
#  if defined( _MSC_VER ) && _MSC_VER >= 1400
#  include <intrin.h>
#  endif
   /* NOTE: Hack to avoid collision between stdint.h and unistd.h. [vszakats] */
#  if defined( ZH_OS_VXWORKS ) && defined( _INTPTR ) && ! defined( _INTPTR_T )
#     define _INTPTR_T
#  endif
   #if ( defined( __BORLANDC__ ) && __BORLANDC__ >= 0x0582 )  /* workaround for compiler bug */
      #undef INT32_MIN
      #define INT32_MIN ((int32_t) (-INT32_MAX-1))
      #undef INT64_MIN
      #define INT64_MIN (9223372036854775807i64-1)
      #undef INT64_MAX
      #define INT64_MAX 9223372036854775807i64
   #endif
#endif

#if ( defined( __GNUC__ ) || defined( __SUNPRO_C ) || defined( __SUNPRO_CC ) ) && \
    ( defined( _ISOC99_SOURCE ) || defined( _STDC_C99 ) || \
      ( defined( __STDC_VERSION__ ) && __STDC_VERSION__ >= 199901L ) )
   #define ZH_C99_STATIC    static
   #define ZH_C99_RESTRICT  restrict
#else
   #define ZH_C99_STATIC
   #define ZH_C99_RESTRICT
#endif

#if 0
#define ZH_CLIPPER_INT_ITEMS
#define ZH_LONG_LONG_OFF
#endif

#if defined( ZH_OS_WIN )
   #if defined( ZH_OS_WIN_64 )
      #undef ZH_LONG_LONG_OFF
   #endif
#endif

#if defined( ZH_OS_DOS )

   #if defined( __WATCOMC__ ) && defined( __386__ ) && ! defined( __WINDOWS_386__ )
      #define ZH_DOS_INT86 int386
      #define ZH_DOS_INT86X int386x
      #define ZH_XREGS w
   #elif defined( __DJGPP__ )
      #define ZH_DOS_INT86 int86
      #define ZH_DOS_INT86X int86x
      #define ZH_XREGS w
   #else
      #define ZH_DOS_INT86 int86
      #define ZH_DOS_INT86X int86x
      #define ZH_XREGS x
   #endif

#elif defined( ZH_OS_DARWIN )

   /* Detect if it is Darwin < 6.x */
   #include <pthread.h>
   #ifndef PTHREAD_MUTEX_RECURSIVE
      #define ZH_OS_DARWIN_5
   #endif

#endif

/*
 * below are some hacks which don't have to be true on some machines
 * please update it if necessary
 */
#if defined( ZH_OS_WIN_64 )
#  define ZH_ARCH_64BIT
#elif ULONG_MAX > UINT_MAX && UINT_MAX > USHRT_MAX
#  define ZH_ARCH_64BIT
#elif ULONG_MAX == UINT_MAX && UINT_MAX > USHRT_MAX
#  define ZH_ARCH_32BIT
#elif ULONG_MAX > UINT_MAX && UINT_MAX == USHRT_MAX
#  define ZH_ARCH_16BIT
#endif

/* Native Ziher types */

#ifndef ZH_LONG_LONG_OFF

   #if defined( ZH_OS_WIN ) && ! defined( __GNUC__ )
      typedef __int64            ZH_LONGLONG;
      typedef unsigned __int64   ZH_ULONGLONG;
   #else
      typedef signed long long   ZH_LONGLONG;
      typedef unsigned long long ZH_ULONGLONG;
   #endif

   #if ! defined( ULONGLONG_MAX )
      #if defined( _UI64_MAX )
         #define ULONGLONG_MAX      _UI64_MAX
      #elif defined( ULLONG_MAX )
         #define ULONGLONG_MAX      ULLONG_MAX
      #elif defined( ULONG_LONG_MAX )
         #define ULONGLONG_MAX      ULONG_LONG_MAX
      #else
         #define ULONGLONG_MAX      18446744073709551615ULL
      #endif
   #endif
   #if ! defined( LONGLONG_MAX )
      #if defined( _I64_MAX )
         #define LONGLONG_MAX       _I64_MAX
      #elif defined( LLONG_MAX )
         #define LONGLONG_MAX       LLONG_MAX
      #elif defined( LONG_LONG_MAX )
         #define LONGLONG_MAX       LONG_LONG_MAX
      #else
         #define LONGLONG_MAX       9223372036854775807LL
      #endif
   #endif
   #if ! defined( LONGLONG_MIN )
      #if defined( _I64_MIN )
         #define LONGLONG_MIN       _I64_MIN
      #elif defined( LLONG_MIN )
         #define LONGLONG_MIN       LLONG_MIN
      #elif defined( LONG_LONG_MIN )
         #define LONGLONG_MIN       LONG_LONG_MIN
      #else
         #define LONGLONG_MIN       (-LONGLONG_MAX - 1LL)
      #endif
   #endif

#endif /* ZH_LONG_LONG_OFF */

/* Basic types */
#define ZH_FALSE 0
#define ZH_TRUE  ( ! 0 )

typedef int                 ZH_BOOL;
typedef signed char         ZH_SCHAR;
typedef unsigned char       ZH_UCHAR;
typedef short               ZH_SHORT;
typedef unsigned short      ZH_USHORT;
typedef long                ZH_LONG;           /* WARNING: These types have a new size in Ziher 2.1.x and upper. */
typedef unsigned long       ZH_ULONG;          /* WARNING: These types have a new size in Ziher 2.1.x and upper. */
typedef int                 ZH_INT;
typedef unsigned int        ZH_UINT;

/* Ziher size type */
#if defined( ZH_OS_WIN_64 )
#  if defined( ZH_SIZE_SIGNED )
      typedef ZH_LONGLONG         ZH_SIZE;
#  else
      typedef ZH_ULONGLONG        ZH_SIZE;        /* TODO: Currently 'unsigned', to be changed 'signed' */
#  endif
   typedef ZH_LONGLONG         ZH_ISIZ;           /* TODO: Change to ZH_SIZE, after ZH_SIZE has been converted to signed type. TEMPORARY type. */
   typedef ZH_ULONGLONG        ZH_USIZ;           /* TEMPORARY type. Do not use it. */
#else
#  if defined( ZH_SIZE_SIGNED )
      typedef ZH_LONG             ZH_SIZE;
#  else
      typedef ZH_ULONG            ZH_SIZE;        /* TODO: Currently 'unsigned', to be changed 'signed' */
#  endif
   typedef ZH_LONG             ZH_ISIZ;           /* TODO: Change to ZH_SIZE, after ZH_SIZE has been converted to signed type. TEMPORARY type. */
   typedef ZH_ULONG            ZH_USIZ;           /* TEMPORARY type. Do not use it. */
#endif

/* Ziher abstract types */
#define ZH_AREANO           ZH_USHORT
#define ZH_FIELDNO          ZH_USHORT
#define ZH_PARAMNO          ZH_USHORT

/* Convenience */
typedef ZH_UCHAR            ZH_BYTE;

/* Guaranteed 8-bit types */
typedef ZH_SCHAR            ZH_I8;
typedef ZH_UCHAR            ZH_U8;

/* Guaranteed 16-bit types */
#if USHRT_MAX == 0xFFFF
   typedef signed short int    ZH_I16;
   typedef unsigned short int  ZH_U16;
   #define ZH_I16_MIN          SHRT_MIN
   #define ZH_I16_MAX          SHRT_MAX
   #define ZH_U16_MAX          USHRT_MAX
#  if ! defined( UINT16_MAX )
#     define UINT16_MAX    USHRT_MAX
#  endif
#  if ! defined( INT16_MAX )
#     define INT16_MAX     SHRT_MAX
#  endif
#  if ! defined( INT16_MIN )
#     define INT16_MIN     SHRT_MIN
#  endif
#else
   typedef short int           ZH_I16;
   typedef unsigned short int  ZH_U16;
   #define ZH_I16_MIN          SHRT_MIN
   #define ZH_I16_MAX          SHRT_MAX
   #define ZH_U16_MAX          USHRT_MAX
#endif

/* Guaranteed 32-bit types */
#if UINT_MAX == 0xFFFFFFFF
   typedef signed int          ZH_I32;
   typedef unsigned int        ZH_U32;
   #define ZH_I32_MIN          INT_MIN
   #define ZH_I32_MAX          INT_MAX
   #define ZH_U32_MAX          UINT_MAX
#  if ! defined( UINT32_MAX )
#     define UINT32_MAX    UINT_MAX
#  endif
#  if ! defined( INT32_MAX )
#     define INT32_MAX     INT_MAX
#  endif
#  if ! defined( INT32_MIN )
#     define INT32_MIN     INT_MIN
#  endif
#elif ULONG_MAX == 0xFFFFFFFF
   typedef signed long         ZH_I32;
   typedef unsigned long       ZH_U32;
   #define ZH_I32_MIN          LONG_MIN
   #define ZH_I32_MAX          LONG_MAX
   #define ZH_U32_MAX          ULONG_MAX
#  if ! defined( UINT32_MAX )
#     define UINT32_MAX    ULONG_MAX
#  endif
#  if ! defined( INT32_MAX )
#     define INT32_MAX     LONG_MAX
#  endif
#  if ! defined( INT32_MIN )
#     define INT32_MIN     LONG_MIN
#  endif
#endif

#if ! defined( UCHAR_MAX )
#  define UCHAR_MAX     0x0FF
#endif
#if ! defined( UINT24_MAX )
#  define UINT24_MAX    0x0FFFFFFL
#endif
#if ! defined( INT24_MAX )
#  define INT24_MAX     8388607L
#endif
#if ! defined( INT24_MIN )
#  define INT24_MIN     -8388608L
#endif

/* Guaranteed 64-bit types */
#if defined( ZH_ARCH_64BIT ) && ! defined( ZH_OS_WIN_64 )
   typedef signed long         ZH_I64;
   typedef unsigned long       ZH_U64;
   #define ZH_I64_MIN          LONG_MIN
   #define ZH_I64_MAX          LONG_MAX
   #define ZH_U64_MAX          ULONG_MAX
   #define ZH_PF64             "l"
#  if ! defined( UINT64_MAX )
#     define UINT64_MAX    ULONG_MAX
#  endif
#  if ! defined( INT64_MAX )
#     define INT64_MAX     LONG_MAX
#  endif
#  if ! defined( INT64_MIN )
#     define INT64_MIN     LONG_MIN
#  endif
#elif ! defined( ZH_LONG_LONG_OFF )
   typedef ZH_LONGLONG         ZH_I64;
   typedef ZH_ULONGLONG        ZH_U64;
   #define ZH_I64_MIN          LONGLONG_MIN
   #define ZH_I64_MAX          LONGLONG_MAX
   #define ZH_U64_MAX          ULONGLONG_MAX
#  if ! defined( UINT64_MAX )
#     define UINT64_MAX     ULONGLONG_MAX
#  endif
#  if ! defined( INT64_MAX )
#     define INT64_MAX      LONGLONG_MAX
#  endif
#  if ! defined( INT64_MIN )
#     define INT64_MIN      LONGLONG_MIN
#  endif
#endif


#ifndef ZH_LONG_DOUBLE_OFF
   typedef long double  ZH_MAXDBL;
#else
   typedef double       ZH_MAXDBL;
#endif

#if defined( ZH_CLIPPER_INT_ITEMS )
#  define ZH_VMINT_MAX           SHRT_MAX
#  define ZH_VMINT_MIN           SHRT_MIN
#  define ZH_VMUINT_MAX          USHRT_MAX
#  define ZH_VMLONG_MAX          LONG_MAX
#  define ZH_VMLONG_MIN          LONG_MIN
#  define ZH_VMULONG_MAX         ULONG_MAX
   typedef long                  ZH_MAXINT;
   typedef unsigned long         ZH_MAXUINT;
#  define PFHL                   "l"
#elif ! defined( ZH_LONG_LONG_OFF ) && ULONG_MAX == UINT_MAX
#  define ZH_VMINT_MAX           INT_MAX
#  define ZH_VMINT_MIN           INT_MIN
#  define ZH_VMUINT_MAX          UINT_MAX
#  define ZH_VMLONG_MAX          LONGLONG_MAX
#  define ZH_VMLONG_MIN          LONGLONG_MIN
#  define ZH_VMULONG_MAX         ULONGLONG_MAX
   typedef ZH_LONGLONG           ZH_MAXINT;
   typedef ZH_ULONGLONG          ZH_MAXUINT;
#else
#  define ZH_VMINT_MAX           INT_MAX
#  define ZH_VMINT_MIN           INT_MIN
#  define ZH_VMUINT_MAX          UINT_MAX
#  define ZH_VMLONG_MAX          LONG_MAX
#  define ZH_VMLONG_MIN          LONG_MIN
#  define ZH_VMULONG_MAX         ULONG_MAX
   typedef long                  ZH_MAXINT;
   typedef unsigned long         ZH_MAXUINT;
#  define PFHL                   "l"
#endif

typedef ZH_MAXINT    ZH_VMMAXINT;
typedef ZH_MAXUINT   ZH_VMMAXUINT;

#define ZH_DBL_LIM_INT(d)     ( ZH_VMINT_MIN <= (d) && (d) <= ZH_VMINT_MAX )
#define ZH_DBL_LIM_LONG(d)    ( (ZH_MAXDBL) ZH_VMLONG_MIN <= (ZH_MAXDBL) (d) && (ZH_MAXDBL) (d) <= (ZH_MAXDBL) ZH_VMLONG_MAX )
#define ZH_LIM_INT(l)         ( ZH_VMINT_MIN <= (l) && (l) <= ZH_VMINT_MAX )
#define ZH_LIM_LONG(l)        ( ZH_VMLONG_MIN <= (l) && (l) <= ZH_VMLONG_MAX )

#define ZH_DBL_LIM_INT8(d)    ( -128 <= (d) && (d) <= 127 )
#define ZH_DBL_LIM_INT16(d)   ( INT16_MIN <= (d) && (d) <= INT16_MAX )
#define ZH_DBL_LIM_INT24(d)   ( INT24_MIN <= (d) && (d) <= INT24_MAX )
#define ZH_DBL_LIM_INT32(d)   ( INT32_MIN <= (d) && (d) <= INT32_MAX )
#define ZH_DBL_LIM_INT64(d)   ( (ZH_MAXDBL) INT64_MIN <= (ZH_MAXDBL) (d) && (ZH_MAXDBL) (d) <= (ZH_MAXDBL) INT64_MAX )
#define ZH_LIM_INT8(l)        ( -128 <= (l) && (l) <= 127 )
#define ZH_LIM_INT16(l)       ( INT16_MIN <= (l) && (l) <= INT16_MAX )
#define ZH_LIM_INT24(l)       ( INT24_MIN <= (l) && (l) <= INT24_MAX )
#define ZH_LIM_INT32(l)       ( INT32_MIN <= (l) && (l) <= INT32_MAX )
#define ZH_LIM_INT64(l)       ( INT64_MIN <= (l) && (l) <= INT64_MAX )

#define ZH_CAST_INT( d )      ( ( int ) ( ZH_MAXINT ) ( d ) )
#define ZH_CAST_LONG( d )     ( ( long ) ( ZH_MAXINT ) ( d ) )
#define ZH_CAST_LONGLONG( d ) ( ( ZH_LONGLONG ) ( d ) )
#define ZH_CAST_MAXINT( d )   ( ( ZH_MAXINT ) ( d ) )
#define ZH_CAST_ISIZ( d )     ( ( ZH_ISIZ ) ( ZH_MAXINT ) ( d ) )

/*
 * It's a hack for compilers which don't support LL suffix for LONGLONG
 * numeric constant. This suffix is necessary for some compilers -
 * without it they cut the number to ZH_LONG
 */
#if defined( __BORLANDC__ )
#  define ZH_LL( num )           num##i64
#  define ZH_ULL( num )          num##ui64
#elif defined( _MSC_VER )
#  define ZH_LL( num )           num
#  define ZH_ULL( num )          num
#else
#  define ZH_LL( num )           num##LL
#  define ZH_ULL( num )          num##ULL
#endif


/* ZH_*_EXPLENGTH() macros are used by HVM to set the size of
 * math operations, ZH_*_LENGTH() macros are used when new
 * item is created. [druzus]
 */
/* NOTE: the positive number limit 999999999 in ZH_INT_LENGTH()
 *       (ZH_LONG_LENGTH() on 16-bit platforms) below is not
 *       compatible with other limits. Clipper have such limit
 *       but IMHO it's result of some typo or wrong compiler
 *       warnings cleanup when someone removed one digit from
 *       upper limit instead of removing the whole limit.
 *       It's also possible that it comes from DBASE and was
 *       intentionally replicated. I think we should keep it
 *       only in strict compatibility mode. [druzus]
 */
#if ZH_VMINT_MIN < -999999999
#  define ZH_INT_LENGTH( i )        ( ( (i) < -999999999 || (i) > 999999999 ) ? 20 : 10 )
#else
#  define ZH_INT_LENGTH( i )        10
#  define ZH_INT_EXPLENGTH( i )     10
#  if ZH_VMLONG_MIN < -999999999
#     define ZH_LONG_LENGTH( i )    ( ( (i) < -999999999 || (i) > 999999999 ) ? 20 : 10 )
#  endif
#endif

#if ! defined( ZH_LONG_LONG_OFF )
#  if ZH_VMLONG_MAX > ZH_LL( 9999999999 )
#     define ZH_LONG_LENGTH( l )    ( ( (l) < -999999999 || (l) > ZH_LL( 9999999999 ) ) ? 20 : 10 )
#  endif
#  if ZH_VMINT_MAX > ZH_LL( 9999999999 )
#     define ZH_INT_EXPLENGTH( i )  ZH_LONG_LENGTH( i )
#  endif
#endif

#if ! defined( ZH_LONG_LENGTH )
#  define ZH_LONG_LENGTH( l )       ( ( (l) < -999999999 ) ? 20 : 10 )
#endif
#if ! defined( ZH_INT_EXPLENGTH )
#  define ZH_INT_EXPLENGTH( i )     ( ( (i) < -999999999 ) ? 20 : 10 )
#endif
#if ! defined( ZH_LONG_EXPLENGTH )
#  define ZH_LONG_EXPLENGTH( l ) ZH_LONG_LENGTH( l )
#endif

/* ZH_DBL_LENGTH() is used by Val() for strings longer then 10 characters
 * (counted to '.') and to set the size of math operations and new
 * double item - it's CA-Cl*pper compatible range. For doubles we do
 * not have separated limit for result of math operations. [druzus]
 */
#define ZH_DBL_LENGTH( d ) ( ( (d) > 9999999999.0 || (d) < -999999999.0 ) ? 20 : 10 )

/* uncomment this if you need strict Clipper compatibility */
/* #define PCODE_LONG_LIM(l)     ZH_LIM_INT32( l ) */

/* #define PCODE_LONG_LIM(l)     ZH_LIM_LONG( l ) */

/* type of ZH_ITEM */
#if 0
typedef USHORT ZH_TYPE;
#endif
typedef ZH_U32 ZH_TYPE;

/* type of file attributes */
typedef ZH_U32 ZH_FATTR;

/* type of reference counter */
#if defined( ZH_OS_WIN_64 )
   typedef ZH_ULONGLONG    ZH_COUNTER;
#  define ZH_COUNTER_SIZE  8
#else
   typedef unsigned long   ZH_COUNTER;
#  if ULONG_MAX <= UINT32_MAX
#     define ZH_COUNTER_SIZE  4
#  else
#     define ZH_COUNTER_SIZE  8
#  endif
#endif

/* type for memory pointer diff */
#if defined( ZH_OS_WIN_64 )
   typedef ZH_LONGLONG ZH_PTRDIFF;
   typedef ZH_ULONGLONG ZH_PTRUINT;
#else
   typedef long ZH_PTRDIFF;
   typedef unsigned long ZH_PTRUINT;
#endif

/* type for file offsets */
#if defined( ZH_LONG_LONG_OFF ) || ULONG_MAX == ULONGLONG_MAX
   typedef ZH_LONG ZH_FOFFSET;
   /* we can add hack with double as work around what should
      effectively give 52bit file size limit */
#else
   typedef ZH_LONGLONG ZH_FOFFSET;
#endif

#if defined( ZH_OS_WIN )
   typedef ZH_PTRDIFF ZH_FHANDLE;
   typedef ZH_PTRDIFF ZH_NHANDLE;
#  define zh_numToHandle( h )   ( ( ZH_FHANDLE ) ( ZH_NHANDLE ) ( h ) )
#else
   typedef int ZH_FHANDLE;
   typedef int ZH_NHANDLE;
#  define zh_numToHandle( h )   ( ( int ) ( h ) )
#endif

/* maximum index size */
#if defined( ZH_OS_WIN_64 )
#  if defined( ZH_SIZE_SIGNED )
#     define ZH_SIZE_MAX    LONGLONG_MAX
#  else
#     define ZH_SIZE_MAX    ULONGLONG_MAX
#  endif
#else
#  if defined( ZH_SIZE_SIGNED )
#     define ZH_SIZE_MAX    LONG_MAX
#  else
#     define ZH_SIZE_MAX    ULONG_MAX
#  endif
#endif

#if defined( ZH_OS_WIN )
   typedef wchar_t         ZH_WCHAR;
#else
   typedef unsigned short  ZH_WCHAR;
#endif

/* maximum length of double number in decimal representation:
   log10(2^1024) ~ 308.25 */
#define ZH_MAX_DOUBLE_LENGTH 320

/* This value is used to hack the double FL value in round/int
   operation - similar thing is done by CL5.3 - I do not know
   only the exact factor value but it should be close to this one.
*/
#define ZH_DBLFL_PREC_FACTOR 1.0000000000000002;

/* try to detect byte order if not explicitly set */
#if ! defined( ZH_PDP_ENDIAN ) && ! defined( ZH_BIG_ENDIAN ) && \
    ! defined( ZH_LITTLE_ENDIAN )

   /* I intentionaly move the first two #if/#elif to the beginning
      to avoid compiler error when this macro will be defined as
      empty statement in next conditions, e.g. SunOS
    */
#  if ( defined( __LITTLE_ENDIAN__ ) && ! defined( __BIG_ENDIAN__ ) ) || \
      ( defined( __LITTLE_ENDIAN ) && ! defined( __BIG_ENDIAN ) ) || \
      ( defined( _LITTLE_ENDIAN ) && ! defined( _BIG_ENDIAN ) ) || \
      ( defined( LITTLE_ENDIAN ) && ! defined( BIG_ENDIAN ) )

#     define ZH_LITTLE_ENDIAN

#  elif ( ! defined( __LITTLE_ENDIAN__ ) && defined( __BIG_ENDIAN__ ) ) || \
        ( ! defined( __LITTLE_ENDIAN ) && defined( __BIG_ENDIAN ) ) || \
        ( ! defined( _LITTLE_ENDIAN ) && defined( _BIG_ENDIAN ) ) || \
        ( ! defined( LITTLE_ENDIAN ) && defined( BIG_ENDIAN ) )

#     define ZH_BIG_ENDIAN

#  elif ( defined( __BYTE_ORDER ) && defined( __LITTLE_ENDIAN ) && __BYTE_ORDER == __LITTLE_ENDIAN ) || \
        ( defined( _BYTE_ORDER ) && defined( _LITTLE_ENDIAN ) && _BYTE_ORDER == _LITTLE_ENDIAN ) || \
        ( defined( BYTE_ORDER ) && defined( LITTLE_ENDIAN ) && BYTE_ORDER == LITTLE_ENDIAN )

#     define ZH_LITTLE_ENDIAN

#  elif ( defined( __BYTE_ORDER ) && defined( __BIG_ENDIAN ) && __BYTE_ORDER == __BIG_ENDIAN ) || \
        ( defined( _BYTE_ORDER ) && defined( _BIG_ENDIAN ) && _BYTE_ORDER == _BIG_ENDIAN ) || \
        ( defined( BYTE_ORDER ) && defined( BIG_ENDIAN ) && BYTE_ORDER == BIG_ENDIAN )

#     define ZH_BIG_ENDIAN

#  elif ( defined( __BYTE_ORDER ) && defined( __PDP_ENDIAN ) && __BYTE_ORDER == __PDP_ENDIAN ) || \
        ( defined( _BYTE_ORDER ) && defined( _PDP_ENDIAN ) && _BYTE_ORDER == _PDP_ENDIAN ) || \
        ( defined( BYTE_ORDER ) && defined( PDP_ENDIAN ) && BYTE_ORDER == PDP_ENDIAN )

#     define ZH_PDP_ENDIAN

#  else /* We cannot detect byte order, we will have to guess */

#     if defined( ZH_OS_DARWIN ) || defined( ZH_OS_SUNOS ) || defined( ZH_OS_HPUX )
#        define ZH_BIG_ENDIAN
#     else
#        define ZH_LITTLE_ENDIAN
#     endif

#  endif

#endif

#define ZH_MAX( a, b )          ( ( ( a ) > ( b ) ) ? ( a ) : ( b ) )
#define ZH_MIN( a, b )          ( ( ( a ) < ( b ) ) ? ( a ) : ( b ) )

#define ZH_LOBYTE( w )          ( ( ZH_BYTE ) ( w ) )
#define ZH_HIBYTE( w )          ( ( ZH_BYTE ) ( ( ( w ) >>  8 ) & 0xFF ) )
#define ZH_ULBYTE( w )          ( ( ZH_BYTE ) ( ( ( w ) >> 16 ) & 0xFF ) )
#define ZH_UHBYTE( w )          ( ( ZH_BYTE ) ( ( ( w ) >> 24 ) & 0xFF ) )
#define ZH_LOWORD( l )          ( ( ZH_U16 ) ( l ) )
#define ZH_HIWORD( l )          ( ( ZH_U16 ) ( ( ( l ) >> 16 ) & 0xFFFF ) )
#define ZH_MKSHORT( lo, hi )    ( ( ZH_SHORT ) ( ( ( ZH_I16 ) ( hi ) ) << 8 ) | ( lo ) )
#define ZH_MKUSHORT( lo, hi )   ( ( ZH_USHORT ) ( ( ( ZH_U16 ) ( hi ) ) << 8 ) | ( lo ) )
#define ZH_MKLONG( b1, b2, b3, b4 )  ( ( ZH_LONG ) \
                                       ( ( ( ( ZH_I32 ) ( b4 ) ) << 24 ) | \
                                         ( ( ( ZH_I32 ) ( b3 ) ) << 16 ) | \
                                         ( ( ( ZH_I32 ) ( b2 ) ) <<  8 ) | \
                                         ( ( ( ZH_I32 ) ( b1 ) ) ) ) )
#define ZH_MKULONG( b1, b2, b3, b4 ) ( ( ZH_ULONG ) \
                                       ( ( ( ( ZH_U32 ) ( b4 ) ) << 24 ) | \
                                         ( ( ( ZH_U32 ) ( b3 ) ) << 16 ) | \
                                         ( ( ( ZH_U32 ) ( b2 ) ) <<  8 ) | \
                                         ( ( ( ZH_U32 ) ( b1 ) ) ) ) )

#define ZH_SWAP_UINT16( w )     ( ( ZH_U16 ) ( ( ( ( ZH_U16 ) ( w ) & 0xFF00 ) >> 8 ) | \
                                               ( ( ( ZH_U16 ) ( w ) & 0x00FF ) << 8 ) ) )
#define ZH_SWAP_UINT32( w )     ( ( ZH_U32 ) ( ( ( ( ZH_U32 ) ( w ) & 0x000000FF ) << 24 ) | \
                                               ( ( ( ZH_U32 ) ( w ) & 0x0000FF00 ) <<  8 ) | \
                                               ( ( ( ZH_U32 ) ( w ) & 0x00FF0000 ) >>  8 ) | \
                                               ( ( ( ZH_U32 ) ( w ) & 0xFF000000 ) >> 24 ) ) )


#ifndef PFLL
#  if ( defined( __BORLANDC__ ) || defined( _MSC_VER ) || defined( __MINGW32__ ) ) && \
      ! defined( __clang__ )
#     define PFLL    "I64"
#  else
#     define PFLL    "ll"
#  endif
#endif
#ifndef PFHL
#  define PFHL    PFLL
#endif

#ifndef ZH_PF64
#  define ZH_PF64 PFLL
#endif

#if defined( ZH_OS_WIN_64 )
#  define ZH_PFS  PFLL
#else
#  define ZH_PFS  "l"
#endif

#define ZH_SWAP_UINT64( w )      ( ( ZH_U64 ) ( ( ( ( ZH_U64 ) ( w ) & ZH_LL( 0x00000000000000FF ) ) << 56 ) | \
                                                ( ( ( ZH_U64 ) ( w ) & ZH_LL( 0x000000000000FF00 ) ) << 40 ) | \
                                                ( ( ( ZH_U64 ) ( w ) & ZH_LL( 0x0000000000FF0000 ) ) << 24 ) | \
                                                ( ( ( ZH_U64 ) ( w ) & ZH_LL( 0x00000000FF000000 ) ) <<  8 ) | \
                                                ( ( ( ZH_U64 ) ( w ) & ZH_LL( 0x000000FF00000000 ) ) >>  8 ) | \
                                                ( ( ( ZH_U64 ) ( w ) & ZH_LL( 0x0000FF0000000000 ) ) >> 24 ) | \
                                                ( ( ( ZH_U64 ) ( w ) & ZH_LL( 0x00FF000000000000 ) ) >> 40 ) | \
                                                ( ( ( ZH_U64 ) ( w ) & ZH_LL( 0xFF00000000000000 ) ) >> 56 ) ) )

/*
 * on some machines it's not safe to directly access pointers stored
 * at byte buffer they have to be stored at odd (or other alignment)
 * addresses.
 * For example SPARC which needs 4 byte alignment for pointers
 * and 8 byte alignment for doubles and structures (when GCC is used)
 * IMHO need ZH_ARCH_<arch> macro yet - the same OS can be used with
 * different architectures - SPARC + LINUX, ALPHA + LINUX
 */
#if ! defined( ZH_STRICT_ALIGNMENT )
#  if ! defined( ZH_CPU_X86 ) && \
      ! defined( ZH_CPU_X86_64 )
#     define ZH_STRICT_ALIGNMENT
#  endif
#endif

#if defined( ZH_STRICT_ALIGNMENT )
#  if ! defined( ZH_ALLOC_ALIGNMENT ) || ( ZH_ALLOC_ALIGNMENT + 1 == 1 )
#     define ZH_ALLOC_ALIGNMENT     8
#  endif
#endif

#if defined( ZH_ALLOC_ALIGNMENT ) && ZH_COUNTER_SIZE < ZH_ALLOC_ALIGNMENT + 0
#  define ZH_COUNTER_OFFSET   ZH_ALLOC_ALIGNMENT
#else
#  define ZH_COUNTER_OFFSET   ZH_COUNTER_SIZE
#endif

#define ZH_COUNTER_PTR( p )         ((ZH_COUNTER*) ((ZH_BYTE *) (p)-ZH_COUNTER_OFFSET))

#if defined( ZH_PDP_ENDIAN )
   #error PDP-Endian support unimplemented. If you have such machine do it yourself.
#endif

/*
 * These macros are necessary for architectures which need
 * strict alignment for pointers.
 */
#if defined( __GNUC__ )
#  define   ZH_PUT_PTR( p, v )     _zh_put_ptr( ( ZH_BYTE * ) ( p ), v )
#  define   ZH_GET_PTR( p )        _zh_get_ptr( ( const ZH_BYTE * ) ( p ) )
#elif ! defined( ZH_STRICT_ALIGNMENT )
#  define   ZH_PUT_PTR( p, v )      do { *( void ** ) ( p ) = ( void * ) ( v ); } while( 0 )
#  define   ZH_GET_PTR( p )         ( *( void ** ) ( p ) )
#else
#  if defined( ZH_BIG_ENDIAN )
#     if defined( ZH_ARCH_64BIT )
#        define   ZH_PUT_PTR( p, v )   ZH_PUT_BE_UINT64( p, ( ZH_U64 ) ( v ) )
#        define   ZH_GET_PTR( p )      ( ( void * ) ZH_GET_BE_UINT64( p ) )
#     else
#        define   ZH_PUT_PTR( p, v )   ZH_PUT_BE_UINT32( p, ( ZH_U32 ) ( v ) )
#        define   ZH_GET_PTR( p )      ( ( void * ) ZH_GET_BE_UINT32( p ) )
#     endif
#  else
#     if defined( ZH_ARCH_64BIT )
#        define   ZH_PUT_PTR( p, v )   ZH_PUT_LE_UINT64( p, ( ZH_U64 ) ( v ) )
#        define   ZH_GET_PTR( p )      ( ( void * ) ZH_GET_LE_UINT64( p ) )
#     else
#        define   ZH_PUT_PTR( p, v )   ZH_PUT_LE_UINT32( p, ( ZH_U32 ) ( v ) )
#        define   ZH_GET_PTR( p )      ( ( void * ) ZH_GET_LE_UINT32( p ) )
#     endif
#  endif
#endif
#if defined( ZH_BIG_ENDIAN )
#  define   ZH_PUT_UINT32( p, v )   ZH_PUT_BE_UINT32( p, ( ZH_U32 ) ( v ) )
#  define   ZH_GET_UINT32( p )      ZH_GET_BE_UINT32( p )
#  define   ZH_PUT_UINT64( p, v )   ZH_PUT_BE_UINT64( p, ( ZH_U64 ) ( v ) )
#  define   ZH_GET_UINT64( p )      ZH_GET_BE_UINT64( p )
#else
#  define   ZH_PUT_UINT32( p, v )   ZH_PUT_LE_UINT32( p, ( ZH_U32 ) ( v ) )
#  define   ZH_GET_UINT32( p )      ZH_GET_LE_UINT32( p )
#  define   ZH_PUT_UINT64( p, v )   ZH_PUT_LE_UINT64( p, ( ZH_U64 ) ( v ) )
#  define   ZH_GET_UINT64( p )      ZH_GET_LE_UINT64( p )
#endif

/* Macros to store/retrieve integer and double values at/from byte address */
#if defined( __GNUC__ ) || ( defined( _MSC_VER ) && ( _MSC_VER >= 1400 ) )

#  if ( __GNUC__ > 4 || ( __GNUC__ == 4 && __GNUC_MINOR__ >= 3 ) ) && \
      ! defined( __ICC ) && ! defined( __OPENCC__ ) && ! defined( __PCC__ )
#     define ZH_BUILTIN_BSWAP32( n )   __builtin_bswap32( n )
#     define ZH_BUILTIN_BSWAP64( n )   __builtin_bswap64( n )
#  elif defined( _MSC_VER )
#     define ZH_BUILTIN_BSWAP32( n )   _byteswap_ulong( n )
#     define ZH_BUILTIN_BSWAP64( n )   _byteswap_uint64( n )
#  endif

#  if defined( _MSC_VER )
#     define _ZH_CAST16 ( ZH_U16 )
#     define _ZH_CAST32 ( ZH_U32 )
#     define _ZH_CAST64 ( ZH_U64 )
#  else
#     define _ZH_CAST16
#     define _ZH_CAST32
#     define _ZH_CAST64
#  endif

   typedef union
   {
      void *   val;
#  if defined( ZH_ARCH_64BIT )
      ZH_BYTE  buf[ 8 ];
#  else
      ZH_BYTE  buf[ 4 ];
#  endif
   } ZH_PTRCAST, * PZH_PTRCAST;

   typedef union
   {
      ZH_U16   val;
      ZH_BYTE  buf[ 2 ];
   } ZH_U16CAST, * PZH_U16CAST;

   typedef union
   {
      ZH_U32   val;
      ZH_BYTE  buf[ 4 ];
   } ZH_U32CAST, * PZH_U32CAST;

#  if ! defined( ZH_LONG_LONG_OFF ) || defined( ZH_ARCH_64BIT )
   typedef union
   {
      ZH_U64   val;
      ZH_BYTE  buf[ 8 ];
   } ZH_U64CAST, * PZH_U64CAST;
#  endif

   typedef union
   {
      double   val;
      ZH_BYTE  buf[ 8 ];
#  if ( ! defined( ZH_LONG_LONG_OFF ) || defined( ZH_ARCH_64BIT ) ) && \
      defined( ZH_BUILTIN_BSWAP64 )
      ZH_U64   i64;
#  endif
   } ZH_DBLCAST, * PZH_DBLCAST;

   static ZH_FORCEINLINE void * _zh_get_ptr( const ZH_BYTE * buf )
   {
      ZH_PTRCAST u;
      memcpy( u.buf, buf, sizeof( void * ) );
      return u.val;
   }

   static ZH_FORCEINLINE void _zh_put_ptr( ZH_BYTE * buf, void * val )
   {
      ZH_PTRCAST u;
      u.val = val;
      memcpy( buf, u.buf, sizeof( void * ) );
   }

   static ZH_FORCEINLINE ZH_U16 _zh_get_std_uint16( const ZH_BYTE * buf )
   {
      ZH_U16CAST u;
      memcpy( u.buf, buf, sizeof( u.buf ) );
      return u.val;
   }

   static ZH_FORCEINLINE void _zh_put_std_uint16( ZH_BYTE * buf, ZH_U16 val )
   {
      ZH_U16CAST u;
      u.val = val;
      memcpy( buf, u.buf, sizeof( u.buf ) );
   }

   static ZH_FORCEINLINE ZH_U16 _zh_get_rev_uint16( const ZH_BYTE * buf )
   {
      ZH_U16CAST u;
      u.buf[ 0 ] = buf[ 1 ];
      u.buf[ 1 ] = buf[ 0 ];
      return u.val;
   }

   static ZH_FORCEINLINE void _zh_put_rev_uint16( ZH_BYTE * buf, ZH_U16 val )
   {
      ZH_U16CAST u;
      u.val = val;
      buf[ 0 ] = u.buf[ 1 ];
      buf[ 1 ] = u.buf[ 0 ];
   }

   static ZH_FORCEINLINE ZH_U32 _zh_get_std_uint32( const ZH_BYTE * buf )
   {
      ZH_U32CAST u;
      memcpy( u.buf, buf, sizeof( u.buf ) );
      return u.val;
   }

   static ZH_FORCEINLINE void _zh_put_std_uint32( ZH_BYTE * buf, ZH_U32 val )
   {
      ZH_U32CAST u;
      u.val = val;
      memcpy( buf, u.buf, sizeof( u.buf ) );
   }

   static ZH_FORCEINLINE ZH_U32 _zh_get_rev_uint32( const ZH_BYTE * buf )
   {
      ZH_U32CAST u;
#  if defined( ZH_BUILTIN_BSWAP32 )
      memcpy( u.buf, buf, sizeof( u.buf ) );
      return ZH_BUILTIN_BSWAP32( u.val );
#  else
      u.buf[ 0 ] = buf[ 3 ];
      u.buf[ 1 ] = buf[ 2 ];
      u.buf[ 2 ] = buf[ 1 ];
      u.buf[ 3 ] = buf[ 0 ];
      return u.val;
#  endif
   }

   static ZH_FORCEINLINE void _zh_put_rev_uint32( ZH_BYTE * buf, ZH_U32 val )
   {
      ZH_U32CAST u;
#  if defined( ZH_BUILTIN_BSWAP32 )
      u.val = ZH_BUILTIN_BSWAP32( val );
      memcpy( buf, u.buf, sizeof( u.buf ) );
#  else
      u.val = val;
      buf[ 0 ] = u.buf[ 3 ];
      buf[ 1 ] = u.buf[ 2 ];
      buf[ 2 ] = u.buf[ 1 ];
      buf[ 3 ] = u.buf[ 0 ];
#  endif
   }

#  if ! defined( ZH_LONG_LONG_OFF ) || defined( ZH_ARCH_64BIT )
      static ZH_FORCEINLINE ZH_U64 _zh_get_std_uint64( const ZH_BYTE * buf )
      {
         ZH_U64CAST u;
         memcpy( u.buf, buf, sizeof( u.buf ) );
         return u.val;
      }

      static ZH_FORCEINLINE void _zh_put_std_uint64( ZH_BYTE * buf, ZH_U64 val )
      {
         ZH_U64CAST u;
         u.val = val;
         memcpy( buf, u.buf, sizeof( u.buf ) );
      }

      static ZH_FORCEINLINE ZH_U64 _zh_get_rev_uint64( const ZH_BYTE * buf )
      {
         ZH_U64CAST u;
#     if defined( ZH_BUILTIN_BSWAP64 )
         memcpy( u.buf, buf, sizeof( u.buf ) );
         return ZH_BUILTIN_BSWAP64( u.val );
#     else
         u.buf[ 0 ] = buf[ 7 ];
         u.buf[ 1 ] = buf[ 6 ];
         u.buf[ 2 ] = buf[ 5 ];
         u.buf[ 3 ] = buf[ 4 ];
         u.buf[ 4 ] = buf[ 3 ];
         u.buf[ 5 ] = buf[ 2 ];
         u.buf[ 6 ] = buf[ 1 ];
         u.buf[ 7 ] = buf[ 0 ];
         return u.val;
#     endif
      }

      static ZH_FORCEINLINE void _zh_put_rev_uint64( ZH_BYTE * buf, ZH_U64 val )
      {
         ZH_U64CAST u;
#     if defined( ZH_BUILTIN_BSWAP64 )
         u.val = ZH_BUILTIN_BSWAP64( val );
         memcpy( buf, u.buf, sizeof( u.buf ) );
#     else
         u.val = val;
         buf[ 0 ] = u.buf[ 7 ];
         buf[ 1 ] = u.buf[ 6 ];
         buf[ 2 ] = u.buf[ 5 ];
         buf[ 3 ] = u.buf[ 4 ];
         buf[ 4 ] = u.buf[ 3 ];
         buf[ 5 ] = u.buf[ 2 ];
         buf[ 6 ] = u.buf[ 1 ];
         buf[ 7 ] = u.buf[ 0 ];
#     endif
      }
#  endif

   static ZH_FORCEINLINE double _zh_get_std_double( const ZH_BYTE * buf )
   {
      ZH_DBLCAST u;
      memcpy( u.buf, buf, sizeof( u.buf ) );
      return u.val;
   }

   static ZH_FORCEINLINE void _zh_put_std_double( ZH_BYTE * buf, double val )
   {
      ZH_DBLCAST u;
      u.val = val;
      memcpy( buf, u.buf, sizeof( u.buf ) );
   }

   static ZH_FORCEINLINE double _zh_get_rev_double( const ZH_BYTE * buf )
   {
      ZH_DBLCAST u;
#  if ( ! defined( ZH_LONG_LONG_OFF ) || defined( ZH_ARCH_64BIT ) ) && \
      defined( ZH_BUILTIN_BSWAP64 )
      memcpy( u.buf, buf, sizeof( u.buf ) );
      u.i64 = ZH_BUILTIN_BSWAP64( u.i64 );
      return u.val;
#  else
      u.buf[ 0 ] = buf[ 7 ];
      u.buf[ 1 ] = buf[ 6 ];
      u.buf[ 2 ] = buf[ 5 ];
      u.buf[ 3 ] = buf[ 4 ];
      u.buf[ 4 ] = buf[ 3 ];
      u.buf[ 5 ] = buf[ 2 ];
      u.buf[ 6 ] = buf[ 1 ];
      u.buf[ 7 ] = buf[ 0 ];
      return u.val;
#  endif
   }

   static ZH_FORCEINLINE void _zh_put_rev_double( ZH_BYTE * buf, double val )
   {
      ZH_DBLCAST u;
#  if ( ! defined( ZH_LONG_LONG_OFF ) || defined( ZH_ARCH_64BIT ) ) && \
      defined( ZH_BUILTIN_BSWAP64 )
      u.val = val;
      u.i64 = ZH_BUILTIN_BSWAP64( u.i64 );
      memcpy( buf, u.buf, sizeof( u.buf ) );
#  else
      u.val = val;
      buf[ 0 ] = u.buf[ 7 ];
      buf[ 1 ] = u.buf[ 6 ];
      buf[ 2 ] = u.buf[ 5 ];
      buf[ 3 ] = u.buf[ 4 ];
      buf[ 4 ] = u.buf[ 3 ];
      buf[ 5 ] = u.buf[ 2 ];
      buf[ 6 ] = u.buf[ 1 ];
      buf[ 7 ] = u.buf[ 0 ];
#  endif
   }

#  define ZH_GET_STD_DOUBLE( p )       _zh_get_std_double( ( const ZH_BYTE * ) ( p ) )
#  define ZH_GET_REV_DOUBLE( p )       _zh_get_rev_double( ( const ZH_BYTE * ) ( p ) )
#  define ZH_PUT_STD_DOUBLE( p, d )    _zh_put_std_double( ( ZH_BYTE * ) ( p ), d )
#  define ZH_PUT_REV_DOUBLE( p, d )    _zh_put_rev_double( ( ZH_BYTE * ) ( p ), d )

#  if defined( ZH_BIG_ENDIAN )

#     define ZH_GET_BE_UINT16( p )        _zh_get_std_uint16( ( const ZH_BYTE * ) ( p ) )
#     define ZH_PUT_BE_UINT16( p, w )     _zh_put_std_uint16( ( ZH_BYTE * ) ( p ), _ZH_CAST16 ( w ) )
#     define ZH_GET_BE_UINT32( p )        _zh_get_std_uint32( ( const ZH_BYTE * ) ( p ) )
#     define ZH_PUT_BE_UINT32( p, l )     _zh_put_std_uint32( ( ZH_BYTE * ) ( p ), _ZH_CAST32 ( l ) )
#     define ZH_GET_BE_UINT64( p )        _zh_get_std_uint64( ( const ZH_BYTE * ) ( p ) )
#     define ZH_PUT_BE_UINT64( p, q )     _zh_put_std_uint64( ( ZH_BYTE * ) ( p ), _ZH_CAST64 ( q ) )

#     define ZH_GET_LE_UINT16( p )        _zh_get_rev_uint16( ( const ZH_BYTE * ) ( p ) )
#     define ZH_PUT_LE_UINT16( p, w )     _zh_put_rev_uint16( ( ZH_BYTE * ) ( p ), _ZH_CAST16 ( w ) )
#     define ZH_GET_LE_UINT32( p )        _zh_get_rev_uint32( ( const ZH_BYTE * ) ( p ) )
#     define ZH_PUT_LE_UINT32( p, l )     _zh_put_rev_uint32( ( ZH_BYTE * ) ( p ), _ZH_CAST32 ( l ) )
#     define ZH_GET_LE_UINT64( p )        _zh_get_rev_uint64( ( const ZH_BYTE * ) ( p ) )
#     define ZH_PUT_LE_UINT64( p, q )     _zh_put_rev_uint64( ( ZH_BYTE * ) ( p ), _ZH_CAST64 ( q ) )

#  else /* ZH_LITTLE_ENDIAN */

#     define ZH_GET_BE_UINT16( p )        _zh_get_rev_uint16( ( const ZH_BYTE * ) ( p ) )
#     define ZH_PUT_BE_UINT16( p, w )     _zh_put_rev_uint16( ( ZH_BYTE * ) ( p ), _ZH_CAST16 ( w ) )
#     define ZH_GET_BE_UINT32( p )        _zh_get_rev_uint32( ( const ZH_BYTE * ) ( p ) )
#     define ZH_PUT_BE_UINT32( p, l )     _zh_put_rev_uint32( ( ZH_BYTE * ) ( p ), _ZH_CAST32 ( l ) )
#     define ZH_GET_BE_UINT64( p )        _zh_get_rev_uint64( ( const ZH_BYTE * ) ( p ) )
#     define ZH_PUT_BE_UINT64( p, q )     _zh_put_rev_uint64( ( ZH_BYTE * ) ( p ), _ZH_CAST64 ( q ) )

#     define ZH_GET_LE_UINT16( p )        _zh_get_std_uint16( ( const ZH_BYTE * ) ( p ) )
#     define ZH_PUT_LE_UINT16( p, w )     _zh_put_std_uint16( ( ZH_BYTE * ) ( p ), _ZH_CAST16 ( w ) )
#     define ZH_GET_LE_UINT32( p )        _zh_get_std_uint32( ( const ZH_BYTE * ) ( p ) )
#     define ZH_PUT_LE_UINT32( p, l )     _zh_put_std_uint32( ( ZH_BYTE * ) ( p ), _ZH_CAST32 ( l ) )
#     define ZH_GET_LE_UINT64( p )        _zh_get_std_uint64( ( const ZH_BYTE * ) ( p ) )
#     define ZH_PUT_LE_UINT64( p, q )     _zh_put_std_uint64( ( ZH_BYTE * ) ( p ), _ZH_CAST64 ( q ) )

#  endif

#else /* ! __GNUC__ || _MSC_VER < 1400 */

#  define ZH_GET_STD_DOUBLE( p )    zh_get_std_double( ( const ZH_BYTE * ) ( p ) )
#  define ZH_GET_REV_DOUBLE( p )    zh_get_rev_double( ( const ZH_BYTE * ) ( p ) )
#  define ZH_PUT_REV_DOUBLE( p, d ) \
         do { \
            union { \
               double dbl; \
               ZH_BYTE buffer[ 8 ]; \
            } u; \
            u.dbl = ( double ) ( d ); \
            (( ZH_BYTE * )( p ))[ 7 ] = u.buffer[ 0 ]; \
            (( ZH_BYTE * )( p ))[ 6 ] = u.buffer[ 1 ]; \
            (( ZH_BYTE * )( p ))[ 5 ] = u.buffer[ 2 ]; \
            (( ZH_BYTE * )( p ))[ 4 ] = u.buffer[ 3 ]; \
            (( ZH_BYTE * )( p ))[ 3 ] = u.buffer[ 4 ]; \
            (( ZH_BYTE * )( p ))[ 2 ] = u.buffer[ 5 ]; \
            (( ZH_BYTE * )( p ))[ 1 ] = u.buffer[ 6 ]; \
            (( ZH_BYTE * )( p ))[ 0 ] = u.buffer[ 7 ]; \
         } while( 0 )
#  define ZH_PUT_STD_DOUBLE( p, d ) \
         do { \
            union { \
               double dbl; \
               ZH_BYTE buffer[ 8 ]; \
            } u; \
            u.dbl = ( double ) ( d ); \
            (( ZH_BYTE * )( p ))[ 0 ] = u.buffer[ 0 ]; \
            (( ZH_BYTE * )( p ))[ 1 ] = u.buffer[ 1 ]; \
            (( ZH_BYTE * )( p ))[ 2 ] = u.buffer[ 2 ]; \
            (( ZH_BYTE * )( p ))[ 3 ] = u.buffer[ 3 ]; \
            (( ZH_BYTE * )( p ))[ 4 ] = u.buffer[ 4 ]; \
            (( ZH_BYTE * )( p ))[ 5 ] = u.buffer[ 5 ]; \
            (( ZH_BYTE * )( p ))[ 6 ] = u.buffer[ 6 ]; \
            (( ZH_BYTE * )( p ))[ 7 ] = u.buffer[ 7 ]; \
         } while( 0 )

#  if ! defined( ZH_STRICT_ALIGNMENT ) && defined( ZH_LITTLE_ENDIAN )

   #define ZH_GET_LE_UINT16( p )    ( *( const ZH_U16 * )( p ) )
   #define ZH_PUT_LE_UINT16( p, w ) ( *( ZH_U16 * )( p ) = ( ZH_U16 ) ( w ) )
   #define ZH_GET_LE_UINT32( p )    ( *( const ZH_U32 * )( p ) )
   #define ZH_PUT_LE_UINT32( p, l ) ( *( ZH_U32 * )( p ) = ( ZH_U32 ) ( l ) )
   #define ZH_GET_LE_UINT64( p )    ( *( const ZH_U64 * )( p ) )
   #define ZH_PUT_LE_UINT64( p, q ) ( *( ZH_U64 * )( p ) = ( ZH_U64 ) ( q ) )

#  else

   #define ZH_GET_LE_UINT16( p )    ( ( ZH_U16 ) \
                                      ( ( ( ZH_U16 ) (( const ZH_BYTE * )( p ))[ 0 ] ) | \
                                        ( ( ZH_U16 ) (( const ZH_BYTE * )( p ))[ 1 ] <<  8 ) ) )
   #define ZH_GET_LE_UINT32( p )    ( ( ZH_U32 ) \
                                      ( ( ( ZH_U32 ) (( const ZH_BYTE * )( p ))[ 0 ] ) | \
                                        ( ( ZH_U32 ) (( const ZH_BYTE * )( p ))[ 1 ] <<  8 ) | \
                                        ( ( ZH_U32 ) (( const ZH_BYTE * )( p ))[ 2 ] << 16 ) | \
                                        ( ( ZH_U32 ) (( const ZH_BYTE * )( p ))[ 3 ] << 24 ) ) )
   #define ZH_GET_LE_UINT64( p )    ( ( ZH_U64 ) \
                                      ( ( ( ZH_U64 ) (( const ZH_BYTE * )( p ))[ 0 ] ) | \
                                        ( ( ZH_U64 ) (( const ZH_BYTE * )( p ))[ 1 ] <<  8 ) | \
                                        ( ( ZH_U64 ) (( const ZH_BYTE * )( p ))[ 2 ] << 16 ) | \
                                        ( ( ZH_U64 ) (( const ZH_BYTE * )( p ))[ 3 ] << 24 ) | \
                                        ( ( ZH_U64 ) (( const ZH_BYTE * )( p ))[ 4 ] << 32 ) | \
                                        ( ( ZH_U64 ) (( const ZH_BYTE * )( p ))[ 5 ] << 40 ) | \
                                        ( ( ZH_U64 ) (( const ZH_BYTE * )( p ))[ 6 ] << 48 ) | \
                                        ( ( ZH_U64 ) (( const ZH_BYTE * )( p ))[ 7 ] << 56 ) ) )

   #define ZH_PUT_LE_UINT16( p, w )    do { \
                                         (( ZH_BYTE * )( p ))[ 0 ] = ( ZH_BYTE )( w ); \
                                         (( ZH_BYTE * )( p ))[ 1 ] = ( ZH_BYTE )( (w) >>  8 ); \
                                       } while( 0 )
   #define ZH_PUT_LE_UINT32( p, l )    do { \
                                         (( ZH_BYTE * )( p ))[ 0 ] = ( ZH_BYTE )( l ); \
                                         (( ZH_BYTE * )( p ))[ 1 ] = ( ZH_BYTE )( (l) >>  8 ); \
                                         (( ZH_BYTE * )( p ))[ 2 ] = ( ZH_BYTE )( (l) >> 16 ); \
                                         (( ZH_BYTE * )( p ))[ 3 ] = ( ZH_BYTE )( (l) >> 24 ); \
                                       } while( 0 )
   #define ZH_PUT_LE_UINT64( p, q )    do { \
                                         (( ZH_BYTE * )( p ))[ 0 ] = ( ZH_BYTE )( q ); \
                                         (( ZH_BYTE * )( p ))[ 1 ] = ( ZH_BYTE )( (q) >>  8 ); \
                                         (( ZH_BYTE * )( p ))[ 2 ] = ( ZH_BYTE )( (q) >> 16 ); \
                                         (( ZH_BYTE * )( p ))[ 3 ] = ( ZH_BYTE )( (q) >> 24 ); \
                                         (( ZH_BYTE * )( p ))[ 4 ] = ( ZH_BYTE )( (q) >> 32 ); \
                                         (( ZH_BYTE * )( p ))[ 5 ] = ( ZH_BYTE )( (q) >> 40 ); \
                                         (( ZH_BYTE * )( p ))[ 6 ] = ( ZH_BYTE )( (q) >> 48 ); \
                                         (( ZH_BYTE * )( p ))[ 7 ] = ( ZH_BYTE )( (q) >> 56 ); \
                                       } while( 0 )
#  endif

#  if ! defined( ZH_STRICT_ALIGNMENT ) && defined( ZH_BIG_ENDIAN )

   #define ZH_GET_BE_UINT16( p )    ( *( const ZH_U16 * )( p ) )
   #define ZH_PUT_BE_UINT16( p, w ) ( *( ZH_U16 * )( p ) = ( ZH_U16 ) ( w ) )
   #define ZH_GET_BE_UINT32( p )    ( *( const ZH_U32 * )( p ) )
   #define ZH_PUT_BE_UINT32( p, l ) ( *( ZH_U32 * )( p ) = ( ZH_U32 ) ( l ) )
   #define ZH_GET_BE_UINT64( p )    ( *( const ZH_U64 * )( p ) )
   #define ZH_PUT_BE_UINT64( p, q ) ( *( ZH_U64 * )( p ) = ( ZH_U64 ) ( q ) )

#  else

   #define ZH_GET_BE_UINT16( p )    ( ( ZH_U16 ) \
                                      ( ( ( ZH_U16 ) (( const ZH_BYTE * )( p ))[ 0 ] << 8 ) | \
                                        ( ( ZH_U16 ) (( const ZH_BYTE * )( p ))[ 1 ] ) ) )
   #define ZH_GET_BE_UINT32( p )    ( ( ZH_U32 ) \
                                      ( ( ( ZH_U32 ) (( const ZH_BYTE * )( p ))[ 0 ] << 24 ) | \
                                        ( ( ZH_U32 ) (( const ZH_BYTE * )( p ))[ 1 ] << 16 ) | \
                                        ( ( ZH_U32 ) (( const ZH_BYTE * )( p ))[ 2 ] <<  8 ) | \
                                        ( ( ZH_U32 ) (( const ZH_BYTE * )( p ))[ 3 ] ) ) )
   #define ZH_GET_BE_UINT64( p )    ( ( ZH_U64 ) \
                                      ( ( ( ZH_U64 ) (( const ZH_BYTE * )( p ))[ 0 ] << 56 ) | \
                                        ( ( ZH_U64 ) (( const ZH_BYTE * )( p ))[ 1 ] << 48 ) | \
                                        ( ( ZH_U64 ) (( const ZH_BYTE * )( p ))[ 2 ] << 40 ) | \
                                        ( ( ZH_U64 ) (( const ZH_BYTE * )( p ))[ 3 ] << 32 ) | \
                                        ( ( ZH_U64 ) (( const ZH_BYTE * )( p ))[ 4 ] << 24 ) | \
                                        ( ( ZH_U64 ) (( const ZH_BYTE * )( p ))[ 5 ] << 16 ) | \
                                        ( ( ZH_U64 ) (( const ZH_BYTE * )( p ))[ 6 ] <<  8 ) | \
                                        ( ( ZH_U64 ) (( const ZH_BYTE * )( p ))[ 7 ] ) ) )

   #define ZH_PUT_BE_UINT16( p, w )    do { \
                                         (( ZH_BYTE * )( p ))[ 0 ] = ( ZH_BYTE )( (w) >>  8 ); \
                                         (( ZH_BYTE * )( p ))[ 1 ] = ( ZH_BYTE )( w ); \
                                       } while( 0 )
   #define ZH_PUT_BE_UINT32( p, l )    do { \
                                         (( ZH_BYTE * )( p ))[ 0 ] = ( ZH_BYTE )( (l) >> 24 ); \
                                         (( ZH_BYTE * )( p ))[ 1 ] = ( ZH_BYTE )( (l) >> 16 ); \
                                         (( ZH_BYTE * )( p ))[ 2 ] = ( ZH_BYTE )( (l) >>  8 ); \
                                         (( ZH_BYTE * )( p ))[ 3 ] = ( ZH_BYTE )( l ); \
                                       } while( 0 )
   #define ZH_PUT_BE_UINT64( p, q )    do { \
                                         (( ZH_BYTE * )( p ))[ 0 ] = ( ZH_BYTE )( (q) >> 56 ); \
                                         (( ZH_BYTE * )( p ))[ 1 ] = ( ZH_BYTE )( (q) >> 48 ); \
                                         (( ZH_BYTE * )( p ))[ 2 ] = ( ZH_BYTE )( (q) >> 40 ); \
                                         (( ZH_BYTE * )( p ))[ 3 ] = ( ZH_BYTE )( (q) >> 32 ); \
                                         (( ZH_BYTE * )( p ))[ 4 ] = ( ZH_BYTE )( (q) >> 24 ); \
                                         (( ZH_BYTE * )( p ))[ 5 ] = ( ZH_BYTE )( (q) >> 16 ); \
                                         (( ZH_BYTE * )( p ))[ 6 ] = ( ZH_BYTE )( (q) >>  8 ); \
                                         (( ZH_BYTE * )( p ))[ 7 ] = ( ZH_BYTE )( q ); \
                                       } while( 0 )
#  endif

#endif /* ! __GNUC__ */

/*
 * ZH_FORCE_IEEE754_DOUBLE will can be used on platforms which use different
 * double format and we want to force storing double number as IEEE754
 * double value for sharing binary data (e.g. PCODE in .hrb files or CDX
 * indexes or DBFs with "B" fields.
 */
#if defined( ZH_FORCE_IEEE754_DOUBLE )

#  define ZH_GET_LE_DOUBLE( p )     zh_get_ieee754( ( const ZH_BYTE * ) ( p ) )
#  define ZH_PUT_LE_DOUBLE( p, d )  zh_put_ieee754( ( ZH_BYTE * ) ( p ), ( d ) )
#  define ZH_DBL2ORD( d, o )        zh_put_ord_ieee754( ( o ), *( d ) )
#  define ZH_ORD2DBL( o, d )  do { \
                                 *( d ) = zh_get_ord_ieee754( ( const ZH_BYTE * ) ( o ) ); \
                              } while( 0 )

#elif defined( ZH_BIG_ENDIAN )

#  define ZH_GET_LE_DOUBLE( p )     ZH_GET_REV_DOUBLE( ( p ) )
#  define ZH_PUT_LE_DOUBLE( p, d )  ZH_PUT_REV_DOUBLE( ( p ), ( d ) )

#elif defined( ZH_STRICT_ALIGNMENT ) || defined( __GNUC__ )

#  define ZH_GET_LE_DOUBLE( p )     ZH_GET_STD_DOUBLE( ( p ) )
#  define ZH_PUT_LE_DOUBLE( p, d )  ZH_PUT_STD_DOUBLE( ( p ), ( d ) )

#else

#  define ZH_GET_LE_DOUBLE( p )     ( *( const double * )( p ) )
#  define ZH_PUT_LE_DOUBLE( p, d )  ( *( double * )( p ) = ( double ) ( d ) )

#endif

#if ! defined( ZH_FORCE_IEEE754_DOUBLE )
#  if defined( ZH_BIG_ENDIAN )

   #define ZH_ORD2DBL( o, d )       do { \
      if ( ( ( const ZH_BYTE * ) ( o ) )[ 0 ] & 0x80 ) { \
         ( ( ZH_BYTE * ) ( d ) )[ 0 ] = ( ( const ZH_BYTE * ) ( o ) )[ 0 ]; \
         ( ( ZH_BYTE * ) ( d ) )[ 1 ] = ( ( const ZH_BYTE * ) ( o ) )[ 1 ]; \
         ( ( ZH_BYTE * ) ( d ) )[ 2 ] = ( ( const ZH_BYTE * ) ( o ) )[ 2 ]; \
         ( ( ZH_BYTE * ) ( d ) )[ 3 ] = ( ( const ZH_BYTE * ) ( o ) )[ 3 ]; \
         ( ( ZH_BYTE * ) ( d ) )[ 4 ] = ( ( const ZH_BYTE * ) ( o ) )[ 4 ]; \
         ( ( ZH_BYTE * ) ( d ) )[ 5 ] = ( ( const ZH_BYTE * ) ( o ) )[ 5 ]; \
         ( ( ZH_BYTE * ) ( d ) )[ 6 ] = ( ( const ZH_BYTE * ) ( o ) )[ 6 ]; \
         ( ( ZH_BYTE * ) ( d ) )[ 7 ] = ( ( const ZH_BYTE * ) ( o ) )[ 7 ] ^ ( ZH_BYTE ) 0x80; \
      } else { \
         ( ( ZH_BYTE * ) ( d ) )[ 0 ] = ( ( const ZH_BYTE * ) ( o ) )[ 0 ] ^ ( ZH_BYTE ) 0xFF; \
         ( ( ZH_BYTE * ) ( d ) )[ 1 ] = ( ( const ZH_BYTE * ) ( o ) )[ 1 ] ^ ( ZH_BYTE ) 0xFF; \
         ( ( ZH_BYTE * ) ( d ) )[ 2 ] = ( ( const ZH_BYTE * ) ( o ) )[ 2 ] ^ ( ZH_BYTE ) 0xFF; \
         ( ( ZH_BYTE * ) ( d ) )[ 3 ] = ( ( const ZH_BYTE * ) ( o ) )[ 3 ] ^ ( ZH_BYTE ) 0xFF; \
         ( ( ZH_BYTE * ) ( d ) )[ 4 ] = ( ( const ZH_BYTE * ) ( o ) )[ 4 ] ^ ( ZH_BYTE ) 0xFF; \
         ( ( ZH_BYTE * ) ( d ) )[ 5 ] = ( ( const ZH_BYTE * ) ( o ) )[ 5 ] ^ ( ZH_BYTE ) 0xFF; \
         ( ( ZH_BYTE * ) ( d ) )[ 6 ] = ( ( const ZH_BYTE * ) ( o ) )[ 6 ] ^ ( ZH_BYTE ) 0xFF; \
         ( ( ZH_BYTE * ) ( d ) )[ 7 ] = ( ( const ZH_BYTE * ) ( o ) )[ 7 ] ^ ( ZH_BYTE ) 0xFF; \
      } } while( 0 )

   #define ZH_DBL2ORD( d, o )       do { \
      if ( *( d ) >= 0.0 ) { \
         if( *( d ) == -0.0 ) *( d ) = 0.0; \
         ( ( ZH_BYTE * ) ( o ) )[ 0 ] = ( ( const ZH_BYTE * ) ( d ) )[ 0 ] ^ ( ZH_BYTE ) 0x80; \
         ( ( ZH_BYTE * ) ( o ) )[ 1 ] = ( ( const ZH_BYTE * ) ( d ) )[ 1 ]; \
         ( ( ZH_BYTE * ) ( o ) )[ 2 ] = ( ( const ZH_BYTE * ) ( d ) )[ 2 ]; \
         ( ( ZH_BYTE * ) ( o ) )[ 3 ] = ( ( const ZH_BYTE * ) ( d ) )[ 3 ]; \
         ( ( ZH_BYTE * ) ( o ) )[ 4 ] = ( ( const ZH_BYTE * ) ( d ) )[ 4 ]; \
         ( ( ZH_BYTE * ) ( o ) )[ 5 ] = ( ( const ZH_BYTE * ) ( d ) )[ 5 ]; \
         ( ( ZH_BYTE * ) ( o ) )[ 6 ] = ( ( const ZH_BYTE * ) ( d ) )[ 6 ]; \
         ( ( ZH_BYTE * ) ( o ) )[ 7 ] = ( ( const ZH_BYTE * ) ( d ) )[ 7 ]; \
      } else { \
         ( ( ZH_BYTE * ) ( o ) )[ 0 ] = ( ( const ZH_BYTE * ) ( d ) )[ 0 ] ^ ( ZH_BYTE ) 0xFF; \
         ( ( ZH_BYTE * ) ( o ) )[ 1 ] = ( ( const ZH_BYTE * ) ( d ) )[ 1 ] ^ ( ZH_BYTE ) 0xFF; \
         ( ( ZH_BYTE * ) ( o ) )[ 2 ] = ( ( const ZH_BYTE * ) ( d ) )[ 2 ] ^ ( ZH_BYTE ) 0xFF; \
         ( ( ZH_BYTE * ) ( o ) )[ 3 ] = ( ( const ZH_BYTE * ) ( d ) )[ 3 ] ^ ( ZH_BYTE ) 0xFF; \
         ( ( ZH_BYTE * ) ( o ) )[ 4 ] = ( ( const ZH_BYTE * ) ( d ) )[ 4 ] ^ ( ZH_BYTE ) 0xFF; \
         ( ( ZH_BYTE * ) ( o ) )[ 5 ] = ( ( const ZH_BYTE * ) ( d ) )[ 5 ] ^ ( ZH_BYTE ) 0xFF; \
         ( ( ZH_BYTE * ) ( o ) )[ 6 ] = ( ( const ZH_BYTE * ) ( d ) )[ 6 ] ^ ( ZH_BYTE ) 0xFF; \
         ( ( ZH_BYTE * ) ( o ) )[ 7 ] = ( ( const ZH_BYTE * ) ( d ) )[ 7 ] ^ ( ZH_BYTE ) 0xFF; \
      } } while( 0 )

#  else /* ZH_LITTLE_ENDIAN */

   #define ZH_ORD2DBL( o, d )       do { \
      if ( ( ( const ZH_BYTE * ) ( o ) )[ 0 ] & 0x80 ) { \
         ( ( ZH_BYTE * ) ( d ) )[ 0 ] = ( ( const ZH_BYTE * ) ( o ) )[ 7 ]; \
         ( ( ZH_BYTE * ) ( d ) )[ 1 ] = ( ( const ZH_BYTE * ) ( o ) )[ 6 ]; \
         ( ( ZH_BYTE * ) ( d ) )[ 2 ] = ( ( const ZH_BYTE * ) ( o ) )[ 5 ]; \
         ( ( ZH_BYTE * ) ( d ) )[ 3 ] = ( ( const ZH_BYTE * ) ( o ) )[ 4 ]; \
         ( ( ZH_BYTE * ) ( d ) )[ 4 ] = ( ( const ZH_BYTE * ) ( o ) )[ 3 ]; \
         ( ( ZH_BYTE * ) ( d ) )[ 5 ] = ( ( const ZH_BYTE * ) ( o ) )[ 2 ]; \
         ( ( ZH_BYTE * ) ( d ) )[ 6 ] = ( ( const ZH_BYTE * ) ( o ) )[ 1 ]; \
         ( ( ZH_BYTE * ) ( d ) )[ 7 ] = ( ( const ZH_BYTE * ) ( o ) )[ 0 ] ^ ( ZH_BYTE ) 0x80; \
      } else { \
         ( ( ZH_BYTE * ) ( d ) )[ 0 ] = ( ( const ZH_BYTE * ) ( o ) )[ 7 ] ^ ( ZH_BYTE ) 0xFF; \
         ( ( ZH_BYTE * ) ( d ) )[ 1 ] = ( ( const ZH_BYTE * ) ( o ) )[ 6 ] ^ ( ZH_BYTE ) 0xFF; \
         ( ( ZH_BYTE * ) ( d ) )[ 2 ] = ( ( const ZH_BYTE * ) ( o ) )[ 5 ] ^ ( ZH_BYTE ) 0xFF; \
         ( ( ZH_BYTE * ) ( d ) )[ 3 ] = ( ( const ZH_BYTE * ) ( o ) )[ 4 ] ^ ( ZH_BYTE ) 0xFF; \
         ( ( ZH_BYTE * ) ( d ) )[ 4 ] = ( ( const ZH_BYTE * ) ( o ) )[ 3 ] ^ ( ZH_BYTE ) 0xFF; \
         ( ( ZH_BYTE * ) ( d ) )[ 5 ] = ( ( const ZH_BYTE * ) ( o ) )[ 2 ] ^ ( ZH_BYTE ) 0xFF; \
         ( ( ZH_BYTE * ) ( d ) )[ 6 ] = ( ( const ZH_BYTE * ) ( o ) )[ 1 ] ^ ( ZH_BYTE ) 0xFF; \
         ( ( ZH_BYTE * ) ( d ) )[ 7 ] = ( ( const ZH_BYTE * ) ( o ) )[ 0 ] ^ ( ZH_BYTE ) 0xFF; \
      } } while( 0 )

   #define ZH_DBL2ORD( d, o )       do { \
      if ( *( d ) >= 0.0 ) { \
         if( *( d ) == -0.0 ) *( d ) = 0.0; \
         ( ( ZH_BYTE * ) ( o ) )[ 0 ] = ( ( const ZH_BYTE * ) ( d ) )[ 7 ] ^ ( ZH_BYTE ) 0x80; \
         ( ( ZH_BYTE * ) ( o ) )[ 1 ] = ( ( const ZH_BYTE * ) ( d ) )[ 6 ]; \
         ( ( ZH_BYTE * ) ( o ) )[ 2 ] = ( ( const ZH_BYTE * ) ( d ) )[ 5 ]; \
         ( ( ZH_BYTE * ) ( o ) )[ 3 ] = ( ( const ZH_BYTE * ) ( d ) )[ 4 ]; \
         ( ( ZH_BYTE * ) ( o ) )[ 4 ] = ( ( const ZH_BYTE * ) ( d ) )[ 3 ]; \
         ( ( ZH_BYTE * ) ( o ) )[ 5 ] = ( ( const ZH_BYTE * ) ( d ) )[ 2 ]; \
         ( ( ZH_BYTE * ) ( o ) )[ 6 ] = ( ( const ZH_BYTE * ) ( d ) )[ 1 ]; \
         ( ( ZH_BYTE * ) ( o ) )[ 7 ] = ( ( const ZH_BYTE * ) ( d ) )[ 0 ]; \
      } else { \
         ( ( ZH_BYTE * ) ( o ) )[ 0 ] = ( ( const ZH_BYTE * ) ( d ) )[ 7 ] ^ ( ZH_BYTE ) 0xFF; \
         ( ( ZH_BYTE * ) ( o ) )[ 1 ] = ( ( const ZH_BYTE * ) ( d ) )[ 6 ] ^ ( ZH_BYTE ) 0xFF; \
         ( ( ZH_BYTE * ) ( o ) )[ 2 ] = ( ( const ZH_BYTE * ) ( d ) )[ 5 ] ^ ( ZH_BYTE ) 0xFF; \
         ( ( ZH_BYTE * ) ( o ) )[ 3 ] = ( ( const ZH_BYTE * ) ( d ) )[ 4 ] ^ ( ZH_BYTE ) 0xFF; \
         ( ( ZH_BYTE * ) ( o ) )[ 4 ] = ( ( const ZH_BYTE * ) ( d ) )[ 3 ] ^ ( ZH_BYTE ) 0xFF; \
         ( ( ZH_BYTE * ) ( o ) )[ 5 ] = ( ( const ZH_BYTE * ) ( d ) )[ 2 ] ^ ( ZH_BYTE ) 0xFF; \
         ( ( ZH_BYTE * ) ( o ) )[ 6 ] = ( ( const ZH_BYTE * ) ( d ) )[ 1 ] ^ ( ZH_BYTE ) 0xFF; \
         ( ( ZH_BYTE * ) ( o ) )[ 7 ] = ( ( const ZH_BYTE * ) ( d ) )[ 0 ] ^ ( ZH_BYTE ) 0xFF; \
      } } while( 0 )
#  endif

#endif /* ! defined( ZH_FORCE_IEEE754_DOUBLE ) */


/* Now the rest of endian macros */

/*
 * 24 bit integers are not directly supported by any processor we used so far
 * so we always have to build them from ZH_BYTEs and cannot use C casting
 */
#define ZH_GET_LE_INT24( p )        ( ( ZH_I32 ) \
                                      ( ( ( ZH_I32 ) (( const ZH_BYTE * )( p ))[ 0 ] ) | \
                                        ( ( ZH_I32 ) (( const ZH_BYTE * )( p ))[ 1 ] <<  8 ) | \
                                        ( ( ZH_I32 ) (( const ZH_BYTE * )( p ))[ 2 ] << 16 ) | \
                                        ( ( ZH_I32 ) (((( const ZH_BYTE * )( p ))[ 2 ] & 0x80 ) ? 0xFF : 0x00 ) << 24 ) ) )
#define ZH_GET_LE_UINT24( p )       ( ( ZH_U32 ) \
                                      ( ( ( ZH_U32 ) (( const ZH_BYTE * )( p ))[ 0 ] ) | \
                                        ( ( ZH_U32 ) (( const ZH_BYTE * )( p ))[ 1 ] <<  8 ) | \
                                        ( ( ZH_U32 ) (( const ZH_BYTE * )( p ))[ 2 ] << 16 ) ) )
#define ZH_PUT_LE_UINT24( p, u )    do { \
                                       (( ZH_BYTE * )( p ))[ 0 ] = ( ZH_BYTE )( u ); \
                                       (( ZH_BYTE * )( p ))[ 1 ] = ( ZH_BYTE )( ( u ) >>  8 ); \
                                       (( ZH_BYTE * )( p ))[ 2 ] = ( ZH_BYTE )( ( u ) >> 16 ); \
                                    } while( 0 )
#define ZH_GET_BE_INT24( p )        ( ( ZH_I32 ) \
                                      ( ( ( ZH_I32 ) (( const ZH_BYTE * )( p ))[ 2 ] ) | \
                                        ( ( ZH_I32 ) (( const ZH_BYTE * )( p ))[ 1 ] <<  8 ) | \
                                        ( ( ZH_I32 ) (( const ZH_BYTE * )( p ))[ 0 ] << 16 ) | \
                                        ( ( ZH_I32 ) (((( const ZH_BYTE * )( p ))[ 0 ] & 0x80 ) ? 0xFF : 0x00 ) << 24 ) ) )
#define ZH_GET_BE_UINT24( p )       ( ( ZH_U32 ) \
                                      ( ( ( ZH_U32 ) (( const ZH_BYTE * )( p ))[ 2 ] ) | \
                                        ( ( ZH_U32 ) (( const ZH_BYTE * )( p ))[ 1 ] <<  8 ) | \
                                        ( ( ZH_U32 ) (( const ZH_BYTE * )( p ))[ 0 ] << 16 ) ) )
#define ZH_PUT_BE_UINT24( p, u )    do { \
                                       (( ZH_BYTE * )( p ))[ 2 ] = ( ZH_BYTE )( u ); \
                                       (( ZH_BYTE * )( p ))[ 1 ] = ( ZH_BYTE )( ( u ) >>  8 ); \
                                       (( ZH_BYTE * )( p ))[ 0 ] = ( ZH_BYTE )( ( u ) >> 16 ); \
                                    } while( 0 )


#define ZH_GET_LE_INT16( p )        (( ZH_I16 ) ZH_GET_LE_UINT16( p ))
#define ZH_GET_LE_INT32( p )        (( ZH_I32 ) ZH_GET_LE_UINT32( p ))
#define ZH_GET_LE_INT64( p )        (( ZH_I64 ) ZH_GET_LE_UINT64( p ))

#define ZH_PCODE_MKSHORT( p )       (( ZH_SHORT )     ZH_GET_LE_INT16( p ))
#define ZH_PCODE_MKUSHORT( p )      (( ZH_USHORT )    ZH_GET_LE_UINT16( p ))
#define ZH_PCODE_MKLONG( p )        (( ZH_LONG )      ZH_GET_LE_INT32( p ))
#define ZH_PCODE_MKULONG( p )       (( ZH_ULONG )     ZH_GET_LE_UINT32( p ))
#define ZH_PCODE_MKLONGLONG( p )    (( ZH_LONGLONG )  ZH_GET_LE_INT64( p ))
#define ZH_PCODE_MKULONGLONG( p )   (( ZH_ULONGLONG ) ZH_GET_LE_UINT64( p ))
#define ZH_PCODE_MKDOUBLE( p )      (( double )       ZH_GET_LE_DOUBLE( p ))
#define ZH_PCODE_MKINT24( p )       (( ZH_LONG )      ZH_GET_LE_INT24( p ))
#define ZH_PCODE_MKUINT24( p )      (( ZH_ULONG )     ZH_GET_LE_UINT24( p ))

/*
 * Below are hacked version of INT64 macros which operates on double
 * when INT64 is not supported - they are necessary for PCODE and
 * database access
 */
#if defined( ZH_LONG_LONG_OFF ) && ! defined( ZH_ARCH_64BIT )
   #undef ZH_GET_LE_INT64
   #undef ZH_GET_LE_UINT64
   #undef ZH_PUT_LE_UINT64
   #undef ZH_PCODE_MKLONGLONG
   #undef ZH_PCODE_MKULONGLONG
   #undef ZH_DBL_LIM_INT64
   #define UINT64_MAXDBL               ( ( ( double ) UINT32_MAX + 1.0 ) * \
                                         ( ( double ) UINT32_MAX + 1.0 ) - 1.0 )
   #define ZH_GET_LE_INT64( p )        zh_get_le_int64( ( const ZH_BYTE * ) ( p ) )
   #define ZH_GET_LE_UINT64( p )       zh_get_le_uint64( ( const ZH_BYTE * ) ( p ) )
   #define ZH_PUT_LE_UINT64( p, d )    zh_put_le_uint64( ( ZH_BYTE * ) ( p ), \
                                                         ( double ) ( d ) )
   #define ZH_PCODE_MKLONGLONG( p )    ( ( double ) ZH_GET_LE_INT64( p ) )
   #define ZH_PCODE_MKULONGLONG( p )   ( ( double ) ZH_GET_LE_UINT64( p ) )
   #define ZH_DBL_LIM_INT64( d )       ( ( ZH_MAXDBL ) -UINT64_MAXDBL / 2 - 1 <= \
                                         ( ZH_MAXDBL ) ( d ) && ( ZH_MAXDBL ) ( d ) <= \
                                         ( ZH_MAXDBL ) UINT64_MAXDBL / 2 )
#endif

#define ZH_MACRO2STRING( macro )    ZH_MACRO2STRING_( macro )
#define ZH_MACRO2STRING_( macro )   #macro

#define ZH_MACRONAME_JOIN( m1, m2 )       ZH_MACRONAME_JOIN_( m1, m2 )
#define ZH_MACRONAME_JOIN_( m1, m2 )      m1 ## m2

#define ZH_SIZEOFARRAY( var )       ( sizeof( var ) / sizeof( *var ) )

#define ZH_UNCONST( p )       ( ( void * ) ( ZH_PTRUINT ) ( const void * ) ( p ) )
#define ZH_DECONST( c, p )    ( ( c ) ZH_UNCONST( p ) )



#define ZH_SYMBOL_UNUSED( symbol )  ( void ) symbol
#define ZH_SOURCE_FILE_UNUSED()  static void * dummy = &dummy

/*
 * The name of starting procedure
 * Note: You have to define it in case when Ziher cannot find the proper
 * starting procedure (due to unknown order of static data initialization)
 */
#define ZH_START_PROCEDURE "MAIN"
#if defined( __WATCOMC__ ) || \
    ( defined( __GNUC__ ) && ! defined( __DJGPP__ ) && ! defined( ZH_OS_OS2_GCC ) )
   #define ZH_START_PROC_STRICT
#endif

#if defined( __WATCOMC__ ) || defined( _MSC_VER ) || defined( __POCC__ )
   #define ZH_DLL_ENTRY_POINT    DllMain
#else
   #define ZH_DLL_ENTRY_POINT    DllEntryPoint
#endif

#define ZH_EXTERN extern

#if defined( __GNUC__ ) && defined( ZH_OS_WIN )
   #define ZH_EXPORT_ATTR     __attribute__ (( dllexport ))
#elif defined( __GNUC__ ) && defined( ZH_OS_LINUX ) && __GNUC__ >= 3
   #define ZH_EXPORT_ATTR     __attribute__ ((visibility ("default")))
#elif defined( __BORLANDC__ )
   #define ZH_EXPORT_ATTR     __declspec( dllexport )
#elif defined( __WATCOMC__ )
   #define ZH_EXPORT_ATTR     __declspec( dllexport )
#elif defined( ASANLM ) || defined( ASANT )
   #define ZH_EXPORT_ATTR
#elif defined( ZH_OS_WIN )
   #define ZH_EXPORT_ATTR     _declspec( dllexport )
#else
   #define ZH_EXPORT_ATTR
#endif

#if defined( ZH_DYNLIB )
   #define ZH_EXPORT    ZH_EXPORT_ATTR
#else
   #define ZH_EXPORT
#endif

#define ZH_EXPORT_INT ZH_EXPORT

#if defined( __GNUC__ ) && defined( ZH_OS_WIN )
   #define ZH_IMPORT_ATTR     __attribute__ (( dllimport ))
#elif defined( __BORLANDC__ )
   #define ZH_IMPORT_ATTR     __declspec( dllimport )
#elif defined( __WATCOMC__ )
   #define ZH_IMPORT_ATTR     __declspec( dllimport )
#elif defined( ASANLM ) || defined( ASANT )
   #define ZH_IMPORT_ATTR
#elif defined( ZH_OS_WIN )
   #define ZH_IMPORT_ATTR     _declspec( dllimport )
#else
   #define ZH_IMPORT_ATTR
#endif

#define ZH_IMPORT    ZH_IMPORT_ATTR

#if defined( ZH_OS_WIN )

   /* Features provided for Windows builds only */

   ZH_EXTERN_BEGIN
      extern ZH_EXPORT int       zh_wctomblen( const wchar_t * szText );
      extern ZH_EXPORT wchar_t * zh_mbtowc( const char * srcA );
      extern ZH_EXPORT char *    zh_wctomb( const wchar_t * srcW );
      extern ZH_EXPORT wchar_t * zh_mbntowc( const char * srcA, ZH_SIZE nLen );
      extern ZH_EXPORT char *    zh_wcntomb( const wchar_t * srcW, ZH_SIZE nLen );
      extern ZH_EXPORT void      zh_wcntombcpy( char * dstA, const wchar_t * srcW, ZH_SIZE nLen );
      extern ZH_EXPORT void      zh_mbntowccpy( wchar_t * dstW, const char * srcA, ZH_SIZE nLen );
   ZH_EXTERN_END

#endif

#if defined( ZH_OS_WIN )
   #if defined( UNICODE )
      #define ZH_TCHAR_COPYTO( d, s, l )     zh_mbntowccpy( d, s, l )
      #define ZH_TCHAR_COPYFROM( d, s, l )   zh_wcntombcpy( d, s, l )
      #define ZH_TCHAR_CONVTO( s )           zh_mbtowc( s )
      #define ZH_TCHAR_CONVFROM( s )         zh_wctomb( s )
      #define ZH_TCHAR_FREE( s )             zh_xfree( s )
   #else
      #define ZH_TCHAR_COPYTO( d, s, l )     zh_strncpy( d, s, l )
      #define ZH_TCHAR_COPYFROM( d, s, l )   zh_strncpy( d, s, l )
      #define ZH_TCHAR_CONVTO( s )           ( ( char * )( s ) )
      #define ZH_TCHAR_CONVFROM( s )         ( ( char * )( s ) )
      #define ZH_TCHAR_FREE( s )             ZH_SYMBOL_UNUSED( s )
   #endif
#endif

/* Function declaration macros */

/* NOTE: The prefix is "ZH_FUN_" currently, this is needed to
         avoid collision with any other declared symbol.
         Note that "ZH_" is not enough, since the Ziher internals
         are also prefixed with ZH_. [vszakats] */

#define ZH_FUNCNAME( funcname )        ZH_FUN_##funcname
#define ZH_INIT_FUNCNAME( funcname )   ZH_FUN_init_##funcname
#define ZH_EXIT_FUNCNAME( funcname )   ZH_FUN_exit_##funcname
#define ZH_INITSTATICS_FUNCNAME()      zh_INITSTATICS

#if defined( __cplusplus ) && ! defined( ZH_FUNC_USE_DECORATION )
   #define ZH_EXTERN_C_ ZH_EXTERN_C
   #define ZH_EXTERN_
#else
   #define ZH_EXTERN_C_
   #define ZH_EXTERN_   extern
#endif

#define ZH_FUNC_EXEC( funcname )   ZH_FUN_##funcname()
#define ZH_FUNC( funcname )        ZH_EXTERN_C_ ZH_EXPORT ZIHER ZH_FUN_##funcname ( void )
#define ZH_FUNC_EXTERN( funcname ) ZH_EXTERN_C_ ZH_EXTERN_ ZIHER ZH_EXPORT ZH_FUN_##funcname ( void )
#define ZH_FUNC_STATIC( funcname ) static ZIHER ZH_FUN_##funcname ( void )
#define ZH_FUNC_INIT( funcname )   static ZIHER ZH_FUN_init_##funcname ( void )
#define ZH_FUNC_EXIT( funcname )   static ZIHER ZH_FUN_exit_##funcname ( void )
#define ZH_FUNC_INITSTATICS()      static ZIHER zh_INITSTATICS( void )
#define ZH_FUNC_INITLINES()        static ZIHER zh_INITLINES( void )
#define ZH_FUNC_TRANSLATE( w, o )  ZH_FUNC_EXTERN( o ); ZH_FUNC( w ) { ZH_FUNC_EXEC( o ); }


#if defined( ZH_FUNC_CALLCONV )
   #define ZIHER void ZH_FUNC_CALLCONV
#else
   #define ZIHER void
#endif

ZH_EXTERN_BEGIN
   typedef ZIHER ( * PZH_FUNC )( void );
ZH_EXTERN_END

typedef ZH_SHORT ZH_SYMBOLSCOPE;   /* stores symbol's scope */

typedef unsigned char ZH_ATTR;
typedef int           ZH_COLOR;

/* Some common character constants */

#define ZH_CHAR_NUL             '\0'    /*   0 - NUL */
#define ZH_CHAR_EOS             ZH_CHAR_NUL
#define ZH_CHAR_BEL             '\a'    /*   7 - Bell */
#define ZH_CHAR_BS              '\b'    /*   8 - Backspace */
#define ZH_CHAR_HT              '\t'    /*   9 - Tab horizontal */
#define ZH_CHAR_LF              '\n'    /*  10 - Linefeed */
#define ZH_CHAR_VT              '\v'    /*  11 - Tab vertical */
#define ZH_CHAR_FF              '\f'    /*  12 - Formfeed */
#define ZH_CHAR_CR              '\r'    /*  13 - Carriage return */
#define ZH_CHAR_EOF             '\x1A'  /*  26 - End of file marker */

/* Ziher specific character constants */

#define ZH_CHAR_HARD1           ZH_CHAR_CR
#define ZH_CHAR_HARD2           ZH_CHAR_LF

#define ZH_CHAR_SOFT1           '\x8D'  /* 141 */
#define ZH_CHAR_SOFT2           ZH_CHAR_LF

#define ZH_ISUPPER( c )         ( ( c ) >= 'A' && ( c ) <= 'Z' )
#define ZH_ISLOWER( c )         ( ( c ) >= 'a' && ( c ) <= 'z' )
#define ZH_TOUPPER( c )         ( ( c ) >= 'a' && ( c ) <= 'z' ? ( c ) - ( 'a' - 'A' ) : ( c ) )
#define ZH_TOLOWER( c )         ( ( c ) >= 'A' && ( c ) <= 'Z' ? ( c ) + ( 'a' - 'A' ) : ( c ) )
#define ZH_ISDIGIT( c )         ( ( c ) >= '0' && ( c ) <= '9' )
#define ZH_ISALPHA( c )         ( ZH_ISUPPER( c ) || ZH_ISLOWER( c ) )
#define ZH_ISALNUM( c )         ( ZH_ISALPHA( c ) || ZH_ISDIGIT( c ) )
#define ZH_ISXDIGIT( c )        ( ZH_ISDIGIT(c) || \
                                  ( (c) >= 'A' && (c) <= 'F' ) || \
                                  ( (c) >= 'a' && (c) <= 'f' ) )
#define ZH_ISSPACE( c )         ( ( c ) == ' ' || \
                                  ( c ) == ZH_CHAR_HT || \
                                  ( c ) == ZH_CHAR_LF || \
                                  ( c ) == ZH_CHAR_CR )
#define ZH_ISFIRSTIDCHAR( c )   ( ZH_ISALPHA( c ) || ( c ) == '_' )
#define ZH_ISNEXTIDCHAR( c )    ( ZH_ISFIRSTIDCHAR(c) || ZH_ISDIGIT( c ) )

#include "zh_trace.h"

#endif /* ZH_DEFS_H_ */
