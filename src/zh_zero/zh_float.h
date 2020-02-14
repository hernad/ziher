/*
 * Ziher floating point math macros
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#ifndef ZH_FLOAT_H_
#define ZH_FLOAT_H_

#ifndef _GNU_SOURCE
#  define _GNU_SOURCE
#endif

#ifndef __EXTENSIONS__
#  define __EXTENSIONS__
#endif

/* workaround for some missing C99 math macros in SunOS GCC
 * used in C++ mode
 */
#if ! defined( __C99FEATURES__ ) && defined( __GNUC__ ) && defined( __sun__ )
#  define __C99FEATURES__
#endif

#include "zh_api.h"


#  include <math.h>
#  if defined( __BORLANDC__ ) || defined( __WATCOMC__ ) || defined( _MSC_VER ) || defined( ZH_OS_MINIX )
#     include <float.h>
#  elif defined( ZH_OS_SUNOS )
#     include <ieeefp.h>    /* for finite() */
#  endif



#if defined( ZH_LONG_DOUBLE_OFF ) && ! defined( __NO_LONGDOUBLE__ )
#  define __NO_LONGDOUBLE__
#endif


#define _ZH_NUM_NAN     1
#define _ZH_NUM_PINF    2
#define _ZH_NUM_NINF    4



/* on some platforms signbit() needs _GNU_SOURCE defined.
 * If it's not available on given platform then it can be replaced by
 * 'value < 0' but in such case -0.0 will be shown as 0.0
 */
#if defined( _ISOC99_SOURCE ) || defined( _STDC_C99 ) || defined( signbit )

#  define zh_signbit( d )     signbit( d )

#elif defined( __BORLANDC__ ) && defined( zh_fpclassify )

#  define zh_signbit( d )     ( ( zh_fpclassify( d ) & ( _FPCLASS_NINF | _FPCLASS_NZ ) ) != 0 )

#elif 0 /* TODO: add other C compilers here (check their version number) */
#else

#  define zh_signbit( d )     ( d < 0 )

#endif



#if defined( _ISOC99_SOURCE ) || defined( _STDC_C99 ) || \
    ( defined( isfinite ) && defined( isnan ) )

   /* use C99 macros */
#  define zh_isfinite( d )    isfinite( d )
#  define ZH_NUMTYPE( v, d )  do { \
                                 v = ( isfinite( d ) ? 0 : \
                                       ( isnan( d ) ? _ZH_NUM_NAN : \
                                         ( zh_signbit( d ) ? _ZH_NUM_NINF : \
                                           _ZH_NUM_PINF ) ) ); \
                              } while( 0 )

#elif ( defined( __GNUC__ ) || \
        defined( __SUNPRO_C ) || defined( __SUNPRO_CC ) ) && \
      ( defined( _BSD_SOURCE ) || defined( _SVID_SOURCE ) || \
        defined( _XOPEN_SOURCE ) )

   /* use BSD floating point functions */

#  define zh_isfinite( d )    finite( d )
#  define ZH_NUMTYPE( v, d )  do { \
                                 v = ( finite( d ) ? 0 : \
                                       ( isnan( d ) ? _ZH_NUM_NAN : \
                                         ( isinf( d ) < 0 ? _ZH_NUM_NINF : \
                                           _ZH_NUM_PINF ) ) ); \
                              } while( 0 )
#  if ! defined( __NO_LONGDOUBLE__ ) && ! defined( ZH_OS_SUNOS )
#     define ZH_NUMTYPEL( v, d ) do { \
                                    v = ( finitel( d ) ? 0 : \
                                          ( isnanl( d ) ? _ZH_NUM_NAN : \
                                            ( isinfl( d ) < 0 ? _ZH_NUM_NINF : \
                                              _ZH_NUM_PINF ) ) ); \
                                 } while( 0 )
#  endif

#else

#  if defined( __EMX__ ) || defined( __POCC__ ) || \
      defined( __MINGW32__ ) || defined( ZH_OS_HPUX ) || defined( ZH_OS_MINIX )
#     define zh_isfinite( d )       isfinite( d )
#  elif defined( _MSC_VER )
#     define zh_isfinite( d )       _finite( ( double ) d )
#  elif defined( __BORLANDC__ ) || defined( __WATCOMC__ )
#     define zh_isfinite( d )       _finite( d )
#  elif defined( __GNUC__ ) || defined( __DJGPP__ ) || \
      defined( ZH_OS_SUNOS )
#     define zh_isfinite( d )       finite( d )
#  endif

#  if defined( zh_isfinite )
#     define ZH_NUMTYPE( v, d )  do { \
                                    v = zh_isfinite( d ) ? 0 : _ZH_NUM_NAN ; \
                                 } while( 0 )
#  else
#     define zh_isfinite( d )       ZH_TRUE
#     define ZH_NUMTYPE( v, d )  do { \
                                    int iTODO; \
                                    v = zh_isfinite( d ) ? 0 : _ZH_NUM_NAN ; \
                                 } while( 0 )
#  endif

#endif

#if ! defined( ZH_NUMTYPEL )
#  define ZH_NUMTYPEL( v, d ) ZH_NUMTYPE( v, d )
#endif


/* NOTE: Workaround for Pelles C 5.00 not having an 'inf' (HUGE_VAL)
         in '-Tarm-coff' mode. [vszakats] */
#if defined( __POCC__ ) && defined( ZH_OS_WIN_CE )
   #undef HUGE_VAL
   #define HUGE_VAL   ( 1.0 / ( 1.0, 0.0 ) )
#endif

#endif /* ZH_FLOAT_H_ */
