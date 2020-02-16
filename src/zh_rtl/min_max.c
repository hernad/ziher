/*
 * Min(), Max() functions
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


ZH_FUNC( MAX )
{
   PZH_ITEM p1 = zh_param( 1, ZH_IT_ANY );
   PZH_ITEM p2 = zh_param( 2, ZH_IT_ANY );

   if( p1 && p2 )
   {
      if( ZH_IS_NUMINT( p1 ) && ZH_IS_NUMINT( p2 ) )
      {
         ZH_MAXINT l1 = zh_itemGetNInt( p1 );
         ZH_MAXINT l2 = zh_itemGetNInt( p2 );

         if( l1 >= l2 )
            zh_itemReturn( p1 );
         else
            zh_itemReturn( p2 );
         return;
      }
      else if( ZH_IS_NUMERIC( p1 ) && ZH_IS_NUMERIC( p2 ) )
      {
         double d1 = zh_itemGetND( p1 );
         double d2 = zh_itemGetND( p2 );

         if( d1 >= d2 )
            zh_itemReturn( p1 );
         else
            zh_itemReturn( p2 );
         return;
      }
      else if( ZH_IS_LOGICAL( p1 ) && ZH_IS_LOGICAL( p2 ) )
      {
         ZH_BOOL b1 = zh_itemGetL( p1 );
         ZH_BOOL b2 = zh_itemGetL( p2 );

         zh_retl( b1 >= b2 ? b1 : b2 );
         return;
      }
      else if( ZH_IS_DATE( p1 ) && ZH_IS_DATE( p2 ) )
      {
         long l1 = zh_itemGetDL( p1 );
         long l2 = zh_itemGetDL( p2 );

         zh_retdl( l1 >= l2 ? l1 : l2 );
         return;
      }
      else if( ZH_IS_DATETIME( p1 ) && ZH_IS_DATETIME( p2 ) )
      {
         if( ZH_IS_DATE( p1 ) && zh_itemGetDL( p1 ) == zh_itemGetDL( p2 ) )
            zh_itemReturn( p1 );
         else if( ZH_IS_DATE( p2 ) && zh_itemGetDL( p1 ) == zh_itemGetDL( p2 ) )
            zh_itemReturn( p2 );
         else
            zh_itemReturn( zh_itemGetTD( p1 ) >= zh_itemGetTD( p2 ) ? p1 : p2 );
         return;
      }
   }
   zh_errRT_BASE_SubstR( EG_ARG, 1093, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/* returns the minimum of two date or numerics */
/* NOTE: CA-Cl*pper returns 1st item when they are equal [druzus] */
ZH_FUNC( MIN )
{
   PZH_ITEM p1 = zh_param( 1, ZH_IT_ANY );
   PZH_ITEM p2 = zh_param( 2, ZH_IT_ANY );

   if( p1 && p2 )
   {
      if( ZH_IS_NUMINT( p1 ) && ZH_IS_NUMINT( p2 ) )
      {
         ZH_MAXINT l1 = zh_itemGetNInt( p1 );
         ZH_MAXINT l2 = zh_itemGetNInt( p2 );

         if( l1 <= l2 )
            zh_itemReturn( p1 );
         else
            zh_itemReturn( p2 );
         return;
      }
      else if( ZH_IS_NUMERIC( p1 ) && ZH_IS_NUMERIC( p2 ) )
      {
         double d1 = zh_itemGetND( p1 );
         double d2 = zh_itemGetND( p2 );

         if( d1 <= d2 )
            zh_itemReturn( p1 );
         else
            zh_itemReturn( p2 );
         return;
      }
      else if( ZH_IS_LOGICAL( p1 ) && ZH_IS_LOGICAL( p2 ) )
      {
         ZH_BOOL b1 = zh_itemGetL( p1 );
         ZH_BOOL b2 = zh_itemGetL( p2 );

         zh_retl( b1 <= b2 ? b1 : b2 );
         return;
      }
      else if( ZH_IS_DATE( p1 ) && ZH_IS_DATE( p2 ) )
      {
         long l1 = zh_itemGetDL( p1 );
         long l2 = zh_itemGetDL( p2 );

         zh_retdl( l1 <= l2 ? l1 : l2 );
         return;
      }
      else if( ZH_IS_DATETIME( p1 ) && ZH_IS_DATETIME( p2 ) )
      {
         if( ZH_IS_DATE( p1 ) && zh_itemGetDL( p1 ) == zh_itemGetDL( p2 ) )
            zh_itemReturn( p1 );
         else if( ZH_IS_DATE( p2 ) && zh_itemGetDL( p1 ) == zh_itemGetDL( p2 ) )
            zh_itemReturn( p2 );
         else
            zh_itemReturn( zh_itemGetTD( p1 ) <= zh_itemGetTD( p2 ) ? p1 : p2 );
         return;
      }
   }

   zh_errRT_BASE_SubstR( EG_ARG, 1092, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}
