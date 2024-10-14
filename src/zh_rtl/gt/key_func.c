/*
 * SetKey() and related functions
 *
 * Copyright 1999 April White <bright.tigra gmail.com>
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

/*
   Either way you have to clean up the memory on exit. The best way to
   do this is to add a zh_setkeyInit() and zh_setkeyExit() function
   and call them from console.c Init/Exit functions.
 */

#include "zh_api.h"
#include "zh_vm.h"
#include "zh_item_api.h"
#include "zh_gt_api.h"
#include "zh_stack.h"

typedef struct ZH_SETKEY_
{
   int iKeyCode;
   PZH_ITEM pAction;
   PZH_ITEM pIsActive;
   struct ZH_SETKEY_ * next;
} ZH_SETKEY, * PZH_SETKEY;

typedef struct
{
   PZH_SETKEY sk_list;
} ZH_SK_DATA, * PZH_SK_DATA;

static void zh_setkeyRelease( void * cargo )
{
   PZH_SETKEY sk_list = ( ( PZH_SK_DATA ) cargo )->sk_list;

   while( sk_list )
   {
      PZH_SETKEY sk_list_tmp;

      zh_itemRelease( sk_list->pAction );
      if( sk_list->pIsActive )
         zh_itemRelease( sk_list->pIsActive );
      sk_list_tmp = sk_list;
      sk_list = sk_list->next;
      zh_xfree( sk_list_tmp );
   }

   ( ( PZH_SK_DATA ) cargo )->sk_list = NULL;
}

static ZH_TSD_NEW( s_skData, sizeof( ZH_SK_DATA ), NULL, zh_setkeyRelease );

static ZH_BOOL sk_testActive( PZH_ITEM pIsActive, int iKeyCode )
{
   if( pIsActive )
   {
      zh_vmPushEvalSym();
      zh_vmPush( pIsActive );
      zh_vmPushInteger( iKeyCode );
      zh_vmSend( 1 );
      return zh_parldef( -1, ZH_TRUE );
   }
   return ZH_TRUE;
}

static PZH_SETKEY sk_findkey( int iKeyCode, PZH_SETKEY sk_list,
                              PZH_SETKEY * sk_list_end )
{
   PZH_SETKEY sk_list_tmp;

   *sk_list_end = NULL;
   for( sk_list_tmp = sk_list;
        sk_list_tmp && sk_list_tmp->iKeyCode != iKeyCode;
        sk_list_tmp = sk_list_tmp->next )
      *sk_list_end = sk_list_tmp;

   return sk_list_tmp;
}

static void sk_add( PZH_SETKEY * sk_list_ptr, ZH_BOOL bReturn,
                    int iKeyCode, PZH_ITEM pAction, PZH_ITEM pIsActive )
{
   if( iKeyCode )
   {
      PZH_SETKEY sk_list_tmp, sk_list_end;

      if( pIsActive && ! ZH_IS_EVALITEM( pIsActive ) )
         pIsActive = NULL;
      if( pAction && ! ZH_IS_EVALITEM( pAction ) )
         pAction = NULL;

      sk_list_tmp = sk_findkey( iKeyCode, *sk_list_ptr, &sk_list_end );
      if( sk_list_tmp == NULL )
      {
         if( pAction )
         {
            sk_list_tmp = ( PZH_SETKEY ) zh_xgrab( sizeof( ZH_SETKEY ) );
            sk_list_tmp->next = NULL;
            sk_list_tmp->iKeyCode = iKeyCode;
            sk_list_tmp->pAction = zh_itemNew( pAction );
            sk_list_tmp->pIsActive = pIsActive ? zh_itemNew( pIsActive ) : NULL;

            if( sk_list_end == NULL )
               *sk_list_ptr = sk_list_tmp;
            else
               sk_list_end->next = sk_list_tmp;
         }
      }
      else
      {
         /* Return the previous value */

         if( bReturn )
            zh_itemReturn( sk_list_tmp->pAction );

         /* Free the previous values */

         zh_itemRelease( sk_list_tmp->pAction );
         if( sk_list_tmp->pIsActive )
            zh_itemRelease( sk_list_tmp->pIsActive );

         /* Set the new values or free the entry */

         if( pAction )
         {
            sk_list_tmp->pAction = zh_itemNew( pAction );
            sk_list_tmp->pIsActive = pIsActive ? zh_itemNew( pIsActive ) : NULL;
         }
         else
         {
            /* if this is true, then the key found is the first key in the list */
            if( sk_list_end == NULL )
            {
               sk_list_tmp = *sk_list_ptr;
               *sk_list_ptr = sk_list_tmp->next;
               zh_xfree( sk_list_tmp );
            }
            else
            {
               sk_list_end->next = sk_list_tmp->next;
               zh_xfree( sk_list_tmp );
            }
         }
      }
   }
}

ZH_FUNC( SETKEY )
{
   int iKeyCode = zh_parni( 1 );

   if( iKeyCode != 0 )
   {
      PZH_SK_DATA sk_data = ( PZH_SK_DATA ) zh_stackGetTSD( &s_skData );

      if( zh_pcount() == 1 )
      {
         /* Get a SETKEY value */
         PZH_SETKEY sk_list_tmp, sk_list_end;

         /* sk_list_end is not used in this context */
         sk_list_tmp = sk_findkey( iKeyCode, sk_data->sk_list, &sk_list_end );

         if( sk_list_tmp )
            zh_itemReturn( sk_list_tmp->pAction );
      }
      else
      {
         /* Set a SETKEY value */
         sk_add( &sk_data->sk_list, ZH_TRUE, iKeyCode,
                 zh_param( 2, ZH_IT_EVALITEM ), NULL );
      }
   }
}

ZH_FUNC( ZH_SETKEY )
{
   int iKeyCode = zh_parni( 1 );

   if( iKeyCode != 0 )
   {
      PZH_SK_DATA sk_data = ( PZH_SK_DATA ) zh_stackGetTSD( &s_skData );

      if( zh_pcount() == 1 )
      {
         /* Get a SETKEY value */
         PZH_SETKEY sk_list_tmp, sk_list_end;

         /* sk_list_end is not used in this context */
         sk_list_tmp = sk_findkey( iKeyCode, sk_data->sk_list, &sk_list_end );
         if( sk_list_tmp == NULL )
         {
            int iKeyStd = zh_inkeyKeyStd( iKeyCode );

            if( iKeyStd != iKeyCode )
            {
               sk_list_tmp = sk_findkey( iKeyStd, sk_data->sk_list, &sk_list_end );
               iKeyCode = iKeyStd;
            }
         }

         if( sk_list_tmp )
         {
            if( sk_testActive( sk_list_tmp->pIsActive, iKeyCode ) )
               zh_itemReturn( sk_list_tmp->pAction );
         }
      }
      else
      {
         /* Set a SETKEY value */
         sk_add( &sk_data->sk_list, ZH_TRUE, iKeyCode,
                 zh_param( 2, ZH_IT_EVALITEM ), zh_param( 3, ZH_IT_EVALITEM ) );
      }
   }
}

/* Sets the same block for an array of keycodes */

ZH_FUNC( ZH_SETKEYARRAY )
{
   PZH_ITEM pKeyCodeArray = zh_param( 1, ZH_IT_ARRAY );
   PZH_ITEM pAction = zh_param( 2, ZH_IT_EVALITEM );

   if( pKeyCodeArray && pAction )
   {
      PZH_SK_DATA sk_data = ( PZH_SK_DATA ) zh_stackGetTSD( &s_skData );
      PZH_ITEM pIsActive = zh_param( 3, ZH_IT_EVALITEM );
      ZH_SIZE nLen = zh_arrayLen( pKeyCodeArray );
      ZH_SIZE nPos;

      for( nPos = 1; nPos <= nLen; nPos++ )
         sk_add( &sk_data->sk_list, ZH_FALSE, zh_arrayGetNI( pKeyCodeArray, nPos ), pAction, pIsActive );
   }
}

ZH_FUNC( ZH_SETKEYGET )
{
   PZH_ITEM pKeyCode = zh_param( 1, ZH_IT_NUMERIC );

   if( pKeyCode )
   {
      PZH_SK_DATA sk_data = ( PZH_SK_DATA ) zh_stackGetTSD( &s_skData );
      PZH_SETKEY sk_list_tmp, sk_list_end;

      /* sk_list_end is not used in this context */
      sk_list_tmp = sk_findkey( zh_itemGetNI( pKeyCode ), sk_data->sk_list, &sk_list_end );

      if( sk_list_tmp )
      {
         zh_itemReturn( sk_list_tmp->pAction );

         if( sk_list_tmp->pIsActive )
            zh_itemParamStore( 2, sk_list_tmp->pIsActive );
      }
   }
}

ZH_FUNC( ZH_SETKEYSAVE )
{
   PZH_SK_DATA sk_data = ( PZH_SK_DATA ) zh_stackGetTSD( &s_skData );
   PZH_ITEM pKeys, pKeyElements, pParam;
   PZH_SETKEY sk_list_tmp;
   ZH_SIZE nItemCount, nItem;

   /* build an multi-dimensional array from existing hot-keys, and return it */

   /* count the number of items in the list */
   for( nItemCount = 0, sk_list_tmp = sk_data->sk_list;
        sk_list_tmp;
        nItemCount++, sk_list_tmp = sk_list_tmp->next )
      ;

   pKeys = zh_itemArrayNew( nItemCount );
   pKeyElements = zh_itemNew( NULL );

   for( nItem = 1, sk_list_tmp = sk_data->sk_list;
        nItem <= nItemCount;
        nItem++, sk_list_tmp = sk_list_tmp->next )
   {
      zh_arrayNew( pKeyElements, 3 );
      zh_arraySetNI( pKeyElements, 1, sk_list_tmp->iKeyCode );
      zh_arraySet( pKeyElements, 2, sk_list_tmp->pAction );
      if( sk_list_tmp->pIsActive )
         zh_arraySet( pKeyElements, 3, sk_list_tmp->pIsActive );
      zh_arraySetForward( pKeys, nItem, pKeyElements );
   }
   zh_itemRelease( pKeyElements );
   zh_itemReturnRelease( pKeys );

   pParam = zh_param( 1, ZH_IT_ANY );
   if( pParam )
   {
      zh_setkeyRelease( sk_data ); /* destroy the internal list */

      if( ZH_IS_ARRAY( pParam ) )
      {
         nItemCount = zh_arrayLen( pParam );

         for( nItem = 1; nItem <= nItemCount; nItem++ )
         {
            PZH_ITEM itmKeyElements = zh_arrayGetItemPtr( pParam, nItem );

            sk_add( &sk_data->sk_list, ZH_FALSE,
                    zh_arrayGetNI( itmKeyElements, 1 ),
                    zh_arrayGetItemPtr( itmKeyElements, 2 ),
                    zh_arrayGetItemPtr( itmKeyElements, 3 ) );
         }
      }
   }
}

ZH_FUNC( ZH_SETKEYCHECK )
{
   ZH_BOOL bIsKeySet = ZH_FALSE;
   int iKeyCode = zh_parni( 1 );

   if( iKeyCode != 0 )
   {
      PZH_SK_DATA sk_data = ( PZH_SK_DATA ) zh_stackGetTSD( &s_skData );
      PZH_SETKEY sk_list_tmp, sk_list_end;

      /* sk_list_end is not used in this context */
      sk_list_tmp = sk_findkey( iKeyCode, sk_data->sk_list, &sk_list_end );
      if( sk_list_tmp == NULL )
      {
         int iKeyStd = zh_inkeyKeyStd( iKeyCode );

         if( iKeyStd != iKeyCode )
         {
            sk_list_tmp = sk_findkey( iKeyStd, sk_data->sk_list, &sk_list_end );
            iKeyCode = iKeyStd;
         }
      }

      if( sk_list_tmp )
      {
         if( sk_testActive( sk_list_tmp->pIsActive, iKeyCode ) )
         {
            ZH_USHORT uiPCount = ( ZH_USHORT ) zh_pcount(), uiParam;

            zh_vmPushEvalSym();
            zh_vmPush( sk_list_tmp->pAction );
            for( uiParam = 2; uiParam <= uiPCount; ++uiParam )
               zh_vmPush( zh_stackItemFromBase( uiParam ) );
            zh_vmPushInteger( iKeyCode );
            zh_vmSend( uiPCount );

            bIsKeySet = ZH_TRUE;
         }
      }
   }

   zh_retl( bIsKeySet );
}
