/*
 * ValType() function
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
#include "zh_api_error.h"

ZH_FUNC( VALTYPE )
{
   zh_retc( zh_itemTypeStr( zh_param( 1, ZH_IT_ANY ) ) );
}

ZH_FUNC( ZH_ISNUMERIC )
{
   zh_retl( ZH_IS_PARAM_NUM( 1 ) );
}

ZH_FUNC( ZH_ISLOGICAL )
{
   zh_retl( ZH_ISLOG( 1 ) );
}

ZH_FUNC( ZH_ISDATE )
{
   zh_retl( ZH_ISDATE( 1 ) );
}

ZH_FUNC( ZH_ISDATETIME )
{
   zh_retl( ZH_ISDATETIME( 1 ) );
}

ZH_FUNC( ZH_ISTIMESTAMP )
{
   zh_retl( ZH_ISTIMESTAMP( 1 ) );
}

ZH_FUNC( ZH_ISBLOCK )
{
   zh_retl( ZH_ISBLOCK( 1 ) );
}

ZH_FUNC( ZH_ISPOINTER )
{
   zh_retl( ZH_ISPOINTER( 1 ) );
}

ZH_FUNC( ZH_ISSYMBOL )
{
   zh_retl( ZH_ISSYMBOL( 1 ) );
}

ZH_FUNC( ZH_ISSTRING )
{
   zh_retl( ZH_ISCHAR( 1 ) );
}

ZH_FUNC( ZH_ISCHAR )
{
   zh_retl( ( zh_parinfo( 1 ) & ( ZH_IT_MEMO | ZH_IT_STRING ) ) == ZH_IT_STRING );
}

ZH_FUNC( ZH_ISMEMO )
{
   zh_retl( ZH_ISMEMO( 1 ) );
}

ZH_FUNC( ZH_ISARRAY )
{
   zh_retl( zh_extIsArray( 1 ) );
}

ZH_FUNC( ZH_ISOBJECT )
{
   zh_retl( ZH_ISOBJECT( 1 ) );
}

ZH_FUNC( ZH_ISHASH )
{
   zh_retl( ZH_ISHASH( 1 ) );
}

ZH_FUNC( ZH_ISHASHKEY )
{
   zh_retl( ( zh_parinfo( 1 ) & ZH_IT_HASHKEY ) != 0 );
}

ZH_FUNC( ZH_ISEVALITEM )
{
   PZH_ITEM pItem = zh_param( 1, ZH_IT_ANY );

   zh_retl( pItem && ZH_IS_EVALITEM( pItem ) );
}

ZH_FUNC( ZH_ISNULL )
{
   PZH_ITEM pItem = zh_param( 1, ZH_IT_ANY );

   if( pItem )
   {
      if( ZH_IS_STRING( pItem ) )
      {
         zh_retl( zh_itemGetCLen( pItem ) == 0 );
         return;
      }
      else if( ZH_IS_ARRAY( pItem ) )
      {
         zh_retl( zh_arrayLen( pItem ) == 0 );
         return;
      }
      else if( ZH_IS_HASH( pItem ) )
      {
         zh_retl( zh_hashLen( pItem ) == 0 );
         return;
      }
   }
   zh_errRT_BASE_SubstR( EG_ARG, 1111, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}
