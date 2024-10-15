/*
 * DO command/function
 *
 * Copyright 1999 Ryszard Glab <rglab@imid.med.pl>
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
#include "zh_error_api.h"
#include "zh_vm.h"
#include "zh_stack.h"

/* NOTE: use zh_stackItemFromBase( uiParam ) instead of
 *       zh_param( uiParam, ZH_IT_ANY ) to keep references to
 *       parameters passed by refeence. [druzus]
 */

ZH_FUNC( DO )
{
   ZH_USHORT uiPCount = ( ZH_USHORT ) zh_pcount();
   PZH_ITEM pSelf = NULL;

   if( uiPCount > 0 )
   {
      PZH_ITEM pItem = zh_param( 1, ZH_IT_ANY );

      if( ZH_IS_STRING( pItem ) )
      {
         PZH_DYNSYMBOL pDynSym = zh_dynsymFindName( zh_itemGetCPtr( pItem ) );

         if( ! pDynSym )
         {
            zh_errRT_BASE( EG_NOFUNC, 1001, NULL, zh_itemGetCPtr( pItem ), ZH_ERR_ARGS_BASEPARAMS );
            return;
         }
         zh_vmPushDynSym( pDynSym );
      }
      else if( ZH_IS_BLOCK( pItem ) )
      {
         zh_vmPushEvalSym();
         pSelf = pItem;
      }
      else if( ZH_IS_SYMBOL( pItem ) )
         zh_vmPush( pItem );
      else
         uiPCount = 0;
   }

   if( uiPCount > 0 )
   {
      ZH_USHORT uiParam;

      if( pSelf )
         zh_vmPush( pSelf );
      else
         zh_vmPushNil();

      for( uiParam = 2; uiParam <= uiPCount; ++uiParam )
         zh_vmPush( zh_stackItemFromBase( uiParam ) );

      if( pSelf )
         zh_vmSend( ( ZH_USHORT ) ( uiPCount - 1 ) );
      else
         zh_vmProc( ( ZH_USHORT ) ( uiPCount - 1 ) );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}