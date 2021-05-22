/*
 * Empty() function
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

#include "zh_api.h"
#include "zh_item_api.h"

ZH_FUNC( EMPTY )
{
   PZH_ITEM pItem = zh_param( 1, ZH_IT_ANY );
   long lDate, lTime;
   PZH_SYMBOL pSym;

   switch( zh_itemType( pItem ) )
   {
      case ZH_IT_ARRAY:
         zh_retl( zh_arrayLen( pItem ) == 0 );
         break;

      case ZH_IT_HASH:
         zh_retl( zh_hashLen( pItem ) == 0 );
         break;

      case ZH_IT_STRING:
      case ZH_IT_MEMO:
         zh_retl( zh_strEmpty( zh_itemGetCPtr( pItem ), zh_itemGetCLen( pItem ) ) );
         break;

      case ZH_IT_INTEGER:
         zh_retl( zh_itemGetNI( pItem ) == 0 );
         break;

      case ZH_IT_LONG:
         zh_retl( zh_itemGetNInt( pItem ) == 0 );
         break;

      case ZH_IT_DOUBLE:
         zh_retl( zh_itemGetND( pItem ) == 0.0 );
         break;

      case ZH_IT_DATE:
         zh_retl( zh_itemGetDL( pItem ) == 0 );
         break;

      case ZH_IT_TIMESTAMP:
         zh_itemGetTDT( pItem, &lDate, &lTime );
         zh_retl( lDate == 0 && lTime == 0 );
         break;

      case ZH_IT_LOGICAL:
         zh_retl( ! zh_itemGetL( pItem ) );
         break;

      case ZH_IT_BLOCK:
         zh_retl( ZH_FALSE );
         break;

      case ZH_IT_POINTER:
         zh_retl( zh_itemGetPtr( pItem ) == NULL );
         break;

      case ZH_IT_SYMBOL:
         pSym = zh_itemGetSymbol( pItem );
         if( pSym && ( pSym->scope.value & ZH_FS_DEFERRED ) && \
             pSym->pDynSym )
            pSym = zh_dynsymSymbol( pSym->pDynSym );
         zh_retl( pSym == NULL || pSym->value.pFunPtr == NULL );
         break;

      default:
         zh_retl( ZH_TRUE );
         break;
   }
}
