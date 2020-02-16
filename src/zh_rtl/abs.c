/*
 * Abs() function
 *
 * Copyright 1999 Matthew Hamilton <mhamilton@bunge.com.au>
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
#include "zh_item_api.h"
#include "zh_api_error.h"

ZH_FUNC( ABS )
{
   PZH_ITEM pNumber = zh_param( 1, ZH_IT_NUMERIC );

   if( pNumber )
   {
      int iWidth;
      int iDec;

      zh_itemGetNLen( pNumber, &iWidth, &iDec );

      if( ZH_IS_INTEGER( pNumber ) )
      {
         int iNumber = zh_itemGetNI( pNumber );

         if( iNumber >= 0 )
            zh_retnilen( iNumber, iWidth );
#if -ZH_VMINT_MAX > ZH_VMINT_MIN
         else if( iNumber < -INT_MAX )
#if ZH_VMLONG_MAX > ZH_VMINT_MAX
            zh_retnint( -( ZH_MAXINT ) iNumber );
#else
            zh_retndlen( -( double ) iNumber, 0, iDec );
#endif
#endif
         else
            zh_retni( -iNumber );
      }
      else if( ZH_IS_LONG( pNumber ) )
      {
         ZH_MAXINT lNumber = zh_itemGetNInt( pNumber );

         if( lNumber >= 0 )
            zh_retnintlen( lNumber, iWidth );
#if -ZH_VMLONG_MAX > ZH_VMLONG_MIN
         else if( lNumber < -ZH_VMLONG_MAX )
            zh_retndlen( -( double ) lNumber, 0, iDec );
#endif
         else
            zh_retnint( -lNumber );
      }
      else
      {
         double dNumber = zh_itemGetND( pNumber );

         zh_retndlen( dNumber >= 0.0 ? dNumber : -dNumber, 0, iDec );
      }
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1089, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}
