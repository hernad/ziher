/*
 * Bin2W(), Bin2I(), Bin2L(), I2Bin(), L2Bin() functions
 *
 * Copyright 2009 Przemyslaw Czerpak
 * Copyright 1999 Manuel Ruiz <mrt@joca.es>
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

ZH_FUNC( BIN2W )
{
   PZH_ITEM pItem = zh_param( 1, ZH_IT_STRING );
   ZH_U16 uiResult = 0;

   if( pItem )
   {
      ZH_SIZE nLen = zh_itemGetCLen( pItem );
      if( nLen )
      {
         const char * pszString = zh_itemGetCPtr( pItem );
         uiResult = ZH_GET_LE_INT16( pszString );
      }
   }
   zh_retnint( uiResult );
}

ZH_FUNC( BIN2I )
{
   PZH_ITEM pItem = zh_param( 1, ZH_IT_STRING );
   ZH_I16 iResult = 0;

   if( pItem )
   {
      ZH_SIZE nLen = zh_itemGetCLen( pItem );
      if( nLen )
      {
         const char * pszString = zh_itemGetCPtr( pItem );
         iResult = ZH_GET_LE_UINT16( pszString );
      }
   }
   zh_retnint( iResult );
}

ZH_FUNC( BIN2L )
{
   PZH_ITEM pItem = zh_param( 1, ZH_IT_STRING );
   ZH_I32 iResult = 0;

   if( pItem )
   {
      ZH_SIZE nLen = zh_itemGetCLen( pItem );
      if( nLen )
      {
         const char * pszString = zh_itemGetCPtr( pItem );
         if( nLen >= 3 )
            iResult = ZH_GET_LE_INT32( pszString );
         else
            iResult = ZH_GET_LE_UINT16( pszString );
      }
   }
   zh_retnint( iResult );
}

ZH_FUNC( I2BIN )
{
   char szResult[ 2 ];
   ZH_I16 iValue = ( ZH_I16 ) zh_parni( 1 );

   ZH_PUT_LE_UINT16( szResult, iValue );
   zh_retclen( szResult, 2 );
}

ZH_FUNC( L2BIN )
{
   char szResult[ 4 ];
   ZH_I32 iValue = ( ZH_I32 ) zh_parnl( 1 );

   ZH_PUT_LE_UINT32( szResult, iValue );
   zh_retclen( szResult, 4 );
}
