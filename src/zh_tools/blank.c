/*
 * CT3 Blank() function
 *
 * Copyright 2009 Pavel Tsarenko <tpe2@mail.ru>
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

#include "ct.h"

ZH_FUNC( BLANK )
{
   PZH_ITEM pItem = zh_param( 1, ZH_IT_ANY );
   ZH_BOOL bRef = ZH_ISBYREF( 1 );
   ZH_BOOL bRet = ! ct_getref();

   if( ! pItem )
   {
      if( bRet )
         zh_retl( ZH_FALSE );
   }
   else if( ZH_IS_TIMESTAMP( pItem ) )
   {
      if( bRef )
         zh_stortdt( 0, 0, 1 );
      if( bRet )
         zh_rettdt( 0, 0 );
   }
   else if( ZH_IS_DATE( pItem ) )
   {
      if( bRef )
         zh_stordl( 0, 1 );
      if( bRet )
         zh_retdl( 0 );
   }
   else if( ZH_IS_NUMBER( pItem ) )
   {
      if( bRef )
         zh_stornl( 0, 1 );
      if( bRet )
         zh_retnl( 0 );
   }
   else if( ZH_IS_STRING( pItem ) )
   {
      PZH_ITEM pMode = zh_param( 2, ZH_IT_LOGICAL );

      if( pMode && zh_itemGetL( pMode ) )
      {
         ZH_SIZE nLen = zh_itemGetCLen( pItem );
         char * szResult = ( char * ) zh_xgrab( nLen + 1 );

         if( nLen > 0 )
            zh_xmemset( szResult, ' ', nLen );
         if( bRef )
            zh_storclen( szResult, nLen, 1 );
         if( bRet )
            zh_retclen_buffer( szResult, nLen );
         else
            zh_xfree( szResult );
      }
      else
      {
         if( bRef )
            zh_storc( NULL, 1 );
         if( bRet )
            zh_retc_null();
      }
   }
   else if( ZH_IS_ARRAY( pItem ) )
   {
      if( bRef )
         zh_arraySize( pItem, 0 );
      if( bRet )
         zh_reta( 0 );
   }
   else if( ZH_IS_LOGICAL( pItem ) )
   {
      if( bRef )
         zh_storl( ZH_FALSE, 1 );
      if( bRet )
         zh_retl( ZH_FALSE );
   }
   else
   {
      if( bRet )
         zh_retl( ZH_FALSE );
   }
   if( ! bRet )
      zh_ret();
}
