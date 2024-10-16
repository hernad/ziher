/*
 * ProcName(), ProcLine() and ProcFile() functions
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * Copyright 1999-2001 Viktor Szakats (ProcFile())
 * Copyright 2001 JFL (Mafact) <jfl@mafact.com>
 *    Adding the MethodName() just calling ProcName()
 *    Special treatment in case of Object and Eval (only for method mame)
 *    skipping block and adding (b) before the method name
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

#include "zh_vm_int.h"
#include "zh_api.h"
#include "zh_class_api.h"
#include "zh_item_api.h"
#include "zh_stack.h"
#include "zh_vm.h"

ZH_FUNC( ZH_METHODNAME )
{
   char szName[ ZH_SYMBOL_NAME_LEN + ZH_SYMBOL_NAME_LEN + 5 ];

   zh_retc( zh_procname( zh_parni( 1 ) + 1, szName, ZH_TRUE ) );
}

ZH_FUNC( PROCNAME )
{
   char szName[ ZH_SYMBOL_NAME_LEN + ZH_SYMBOL_NAME_LEN + 5 ];

   zh_retc( zh_procname( zh_parni( 1 ) + 1, szName, ZH_FALSE ) );
}

ZH_FUNC( PROCLINE )
{
   ZH_I_SIZE nOffset = zh_stackBaseProcOffset( zh_parni( 1 ) + 1 );

   if( nOffset > 0 )
      zh_retni( zh_stackItem( nOffset )->item.asSymbol.stackstate->uiLineNo );
   else
      zh_retni( 0 );
}



ZH_FUNC( PROCFILE )
{
   PZH_SYMBOL pSym = NULL;

   if( ZH_ISSYMBOL( 1 ) )
   {
      pSym = zh_itemGetSymbol( zh_param( 1, ZH_IT_SYMBOL ) );
   }
   else if( ZH_ISCHAR( 1 ) )
   {
      PZH_DYNSYMBOL pDynSym = zh_dynsymFindName( zh_parc( 1 ) );

      if( pDynSym )
         pSym = pDynSym->pSymbol;
   }
   else
   {
      ZH_I_SIZE nOffset = zh_stackBaseProcOffset( zh_parni( 1 ) + 1 );

      if( nOffset > 0 )
      {
         PZH_ITEM pBase = zh_stackItem( nOffset );

         pSym = pBase->item.asSymbol.value;
         if( pSym == pZhSymEval || pSym->pDynSym == pZhSymEval->pDynSym )
         {
            PZH_ITEM pSelf = zh_stackItem( nOffset + 1 );

            if( ZH_IS_BLOCK( pSelf ) )
               pSym = pSelf->item.asBlock.value->pDefSymb;
            else if( pBase->item.asSymbol.stackstate->uiClass )
               pSym = zh_clsMethodSym( pBase );
         }
         else if( pBase->item.asSymbol.stackstate->uiClass )
            pSym = zh_clsMethodSym( pBase );
      }
   }
   zh_retc( zh_vmFindModuleSymbolName( zh_vmGetRealFuncSym( pSym ) ) );

}


/* NOTE: szName size must be an at least:
         ZH_SYMBOL_NAME_LEN + ZH_SYMBOL_NAME_LEN + 5 [vszakats] */
#define ZH_PROCBUF_LEN  ( ZH_SYMBOL_NAME_LEN + ZH_SYMBOL_NAME_LEN + 4 )
char * zh_procname( int iLevel, char * szName, ZH_BOOL fMethodName )
{
   ZH_I_SIZE nOffset = zh_stackBaseProcOffset( iLevel );

   szName[ 0 ] = '\0';
   if( nOffset > 0 )
   {
      PZH_ITEM pBase, pSelf;

      pBase = zh_stackItem( nOffset );
      pSelf = zh_stackItem( nOffset + 1 );

      if( fMethodName && nOffset > 0 &&
          pBase->item.asSymbol.value == pZhSymEval &&
          pBase->item.asSymbol.stackstate->uiClass )
      {
         ZH_I_SIZE nPrevOffset = zh_stackItem( nOffset )->item.asSymbol.stackstate->nBaseItem;

         if( zh_stackItem( nPrevOffset )->item.asSymbol.stackstate->uiClass ==
             pBase->item.asSymbol.stackstate->uiClass &&
             zh_stackItem( nPrevOffset )->item.asSymbol.stackstate->uiMethod ==
             pBase->item.asSymbol.stackstate->uiMethod )
         {
            pBase = zh_stackItem( nPrevOffset );
            pSelf = zh_stackItem( nPrevOffset + 1 );
         }
      }

      if( pBase->item.asSymbol.value == pZhSymEval ||
          pBase->item.asSymbol.value->pDynSym == pZhSymEval->pDynSym )
      {
         zh_strncat( szName, "(b)", ZH_PROCBUF_LEN );
         /* it is a method name? */
         if( fMethodName && pBase->item.asSymbol.stackstate->uiClass )
         {
            zh_strncat( szName, zh_clsName( pBase->item.asSymbol.stackstate->uiClass ),
                        ZH_PROCBUF_LEN );
            zh_strncat( szName, ":", ZH_PROCBUF_LEN );
            zh_strncat( szName, zh_clsMethodName( pBase->item.asSymbol.stackstate->uiClass,
                                                  pBase->item.asSymbol.stackstate->uiMethod ), ZH_PROCBUF_LEN );
         }
         else if( ZH_IS_BLOCK( pSelf ) )
            zh_strncat( szName, pSelf->item.asBlock.value->pDefSymb->szName,
                        ZH_PROCBUF_LEN );
         else if( ZH_IS_SYMBOL( pSelf ) )
            zh_strncpy( szName, pSelf->item.asSymbol.value->szName, ZH_PROCBUF_LEN );
         else
            zh_strncat( szName, pBase->item.asSymbol.value->szName, ZH_PROCBUF_LEN );
      }
      else
      {
         /* it is a method name? */
         if( pBase->item.asSymbol.stackstate->uiClass )
         {
            zh_strncat( szName, zh_clsName( pBase->item.asSymbol.stackstate->uiClass ),
                        ZH_PROCBUF_LEN );
            zh_strncat( szName, ":", ZH_PROCBUF_LEN );
         }
         zh_strncat( szName, pBase->item.asSymbol.value->szName, ZH_PROCBUF_LEN );
      }
   }

   return szName;
}

/* NOTE: szName size must be an at least:
 *          ZH_SYMBOL_NAME_LEN + ZH_SYMBOL_NAME_LEN + 5
 *       szFile size must be an at least:
 *          ZH_PATH_MAX
 */
ZH_BOOL zh_procinfo( int iLevel, char * szName, ZH_USHORT * puiLine, char * szFile )
{
   ZH_I_SIZE nOffset = zh_stackBaseProcOffset( iLevel );

   if( nOffset > 0 )
   {
      PZH_ITEM pBase, pSelf;
      PZH_SYMBOL pSym;

      pBase = zh_stackItem( nOffset );
      pSelf = zh_stackItem( nOffset + 1 );

      pSym = pBase->item.asSymbol.value;

      if( szName )
      {
         szName[ 0 ] = '\0';
         if( pSym == pZhSymEval || pSym->pDynSym == pZhSymEval->pDynSym )
         {
            zh_strncat( szName, "(b)", ZH_PROCBUF_LEN );

            if( ZH_IS_BLOCK( pSelf ) )
               zh_strncat( szName, pSelf->item.asBlock.value->pDefSymb->szName,
                           ZH_PROCBUF_LEN );
            else
               zh_strncat( szName, pSym->szName, ZH_PROCBUF_LEN );
         }
         else
         {
            if( pBase->item.asSymbol.stackstate->uiClass ) /* it is a method name */
            {
               zh_strncat( szName, zh_clsName( pBase->item.asSymbol.stackstate->uiClass ),
                           ZH_PROCBUF_LEN );
               zh_strncat( szName, ":", ZH_PROCBUF_LEN );
            }
            zh_strncat( szName, pSym->szName, ZH_PROCBUF_LEN );
         }
      }

      if( puiLine )
         *puiLine = pBase->item.asSymbol.stackstate->uiLineNo;

      if( szFile )
      {
         const char * szModule;

         if( ZH_IS_BLOCK( pSelf ) &&
             ( pSym == pZhSymEval || pSym->pDynSym == pZhSymEval->pDynSym ) )
            pSym = pSelf->item.asBlock.value->pDefSymb;
         else if( pBase->item.asSymbol.stackstate->uiClass )
            pSym = zh_clsMethodSym( pBase );

         szModule = zh_vmFindModuleSymbolName( zh_vmGetRealFuncSym( pSym ) );

         if( szModule )
            zh_strncpy( szFile, szModule, ZH_PATH_MAX - 1 );
         else
            szFile[ 0 ] = '\0';
      }

      return ZH_TRUE;
   }

   if( szName )
      szName[ 0 ] = '\0';
   if( puiLine )
      *puiLine = 0;
   if( szFile )
      szFile[ 0 ] = '\0';

   return ZH_FALSE;
}
