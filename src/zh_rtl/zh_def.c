/*
 * zh_default() and __defaultNIL() functions
 *
 * Copyright 2012 Viktor Szakats
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

typedef enum
{
   ZH_IT_U,
   ZH_IT_N,
   ZH_IT_C,
   ZH_IT_L,
   ZH_IT_T,
   ZH_IT_E,
   ZH_IT_H,
   ZH_IT_A,
   ZH_IT_O,
   ZH_IT_P,
} ZH_IT_BASIC;

static ZH_IT_BASIC s_zh_itemTypeBasic( PZH_ITEM pItem )
{
   switch( ZH_ITEM_TYPE( pItem ) )
   {
      case ZH_IT_ARRAY:
         return zh_arrayIsObject( pItem ) ? ZH_IT_O : ZH_IT_A;

      case ZH_IT_BLOCK:
      case ZH_IT_SYMBOL:
         return ZH_IT_E;

      case ZH_IT_DATE:
      case ZH_IT_TIMESTAMP:
         return ZH_IT_T;

      case ZH_IT_LOGICAL:
         return ZH_IT_L;

      case ZH_IT_INTEGER:
      case ZH_IT_LONG:
      case ZH_IT_DOUBLE:
         return ZH_IT_N;

      case ZH_IT_STRING:
      case ZH_IT_MEMO:
         return ZH_IT_C;

      case ZH_IT_HASH:
         return ZH_IT_H;

      case ZH_IT_POINTER:
         return ZH_IT_P;
   }

   return ZH_IT_U;
}

ZH_FUNC( ZH_DEFAULT )
{
   PZH_ITEM pDefault = zh_param( 2, ZH_IT_ANY );

   if( pDefault &&
       s_zh_itemTypeBasic( zh_param( 1, ZH_IT_ANY ) ) !=
       s_zh_itemTypeBasic( pDefault ) )
      zh_itemParamStore( 1, pDefault );
}

ZH_FUNC( ZH_DEFAULTVALUE )
{
   PZH_ITEM pParam = zh_param( 1, ZH_IT_ANY );
   PZH_ITEM pDefault = zh_param( 2, ZH_IT_ANY );

   if( pDefault &&
       s_zh_itemTypeBasic( pParam ) != s_zh_itemTypeBasic( pDefault ) )
      pParam = pDefault;

   zh_itemReturn( pParam );
}

/* For compatibility with legacy DEFAULT ... TO ... command.
   Not recommended for new code. */
ZH_FUNC( __DEFAULTNIL )
{
   if( zh_pcount() >= 2 && ZH_IS_NIL( zh_param( 1, ZH_IT_ANY ) ) )
      zh_itemParamStore( 1, zh_param( 2, ZH_IT_ANY ) );
}
