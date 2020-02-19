/*
 * Ziher math functions and API
 *
 * Copyright 2001 IntTec GmbH, Neunlindenstr 32, 79106 Freiburg, Germany
 *        Author: Martin Vogel <vogel@inttec.de>
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

#ifndef ZH_MATHER_H_
#define ZH_MATHER_H_

#include "zh_math.h"

ZH_EXTERN_BEGIN


#if ! defined( ZH_MATH_HANDLER ) && \
    ( defined( __GNUC__ ) || defined( ZH_OS_UNIX ) )
   #define ZH_MATH_ERRNO
#endif

typedef struct _ZH_MATH_EXCEPTION
{
   int          type;
   const char * funcname;
   const char * error;
   double       arg1;
   double       arg2;
   double       retval;
   int          retvalwidth;
   int          retvaldec;
   int          handled;
} ZH_MATH_EXCEPTION;

typedef int ( * ZH_MATH_HANDLERPROC )( ZH_MATH_EXCEPTION * err );

extern ZH_EXPORT void zh_mathResetError( ZH_MATH_EXCEPTION * pzh_exc );
extern ZH_EXPORT ZH_BOOL zh_mathGetError( ZH_MATH_EXCEPTION * pzh_exc, const char * szFunc, double arg1, double arg2, double dResult );

extern ZH_EXPORT int zh_mathSetErrMode( int imode );
extern ZH_EXPORT int zh_mathGetErrMode( void );

extern ZH_EXPORT ZH_MATH_HANDLERPROC zh_mathSetHandler( ZH_MATH_HANDLERPROC handlerproc );
extern ZH_EXPORT ZH_MATH_HANDLERPROC zh_mathGetHandler( void );

/* include defines from math.zhh */
#include "math.zhh"

ZH_EXTERN_END

#endif /* ZH_MATHER_H_ */
