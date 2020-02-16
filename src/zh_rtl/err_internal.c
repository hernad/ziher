/*
 * The Error API (internal error)
 *
 * Copyright 1999-2004 Viktor Szakats
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
#include "zh_api_error.h"

/* NOTE: Use as minimal calls from here, as possible.
         Don't allocate memory from this function. [vszakats] */

void zh_errInternal( ZH_ERRCODE errCode, const char * szText, const char * szPar1, const char * szPar2 )
{
   zh_errInternalRaw( errCode, szText, szPar1, szPar2 );

   /* release console settings */
   zh_conRelease();

   if( zh_cmdargCheck( "ERRGPF" ) )
   {
#if defined( _MSC_VER ) && _MSC_VER >= 1800
#  pragma warning(push)
#  pragma warning(disable:6011)
#elif defined( ZH_GCC_HAS_DIAG ) && ( ZH_GCC_VER >= 601 )
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wnull-dereference"
#endif
      int * pGPF = NULL;
      /* cppcheck-suppress nullPointer */
      *pGPF = 0;
      *( --pGPF ) = 0;
#if defined( _MSC_VER ) && _MSC_VER >= 1800
#  pragma warning(pop)
#elif defined( ZH_GCC_HAS_DIAG ) && ( ZH_GCC_VER >= 601 )
#  pragma GCC diagnostic pop
#endif
   }

   exit( EXIT_FAILURE );
}