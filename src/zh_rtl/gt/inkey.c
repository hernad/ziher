/*
 * The Keyboard API
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 * Copyright 1999-2012 Viktor Szakats (zh_keyPut(), zh_keyNew())
 * Copyright 2003-2012 Przemyslaw Czerpak <druzus@acn.waw.pl> (zh_keySetLast(), zh_keyChar(), zh_keyStd())
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
#include "zh_gt_api.h"
#include "zh_gt_core.h"
#include "zh_item_api.h"
#include "zh_codepage_api.h"
#include "zh_set.h"
#include "zh_stack.h"
#include "zh_vm.h"

static void zh_inkeySetTextKeys( const char * pszText, ZH_SIZE nSize, ZH_BOOL fInsert )
{
   PZH_CODEPAGE cdp = zh_vmCDP();
   ZH_SIZE nIndex = 0;
   ZH_WCHAR wc;

   if( fInsert )
   {
      ZH_WCHAR buffer[ 32 ], * keys;
      ZH_SIZE n = 0;

      keys = nSize <= ZH_SIZEOFARRAY( buffer ) ? buffer :
                        ( ZH_WCHAR * ) zh_xgrab( nSize * sizeof( ZH_WCHAR ) );
      while( ZH_CODEPAGE_CHAR_GET( cdp, pszText, nSize, &nIndex, &wc ) )
         keys[ n++ ] = wc;

      while( n-- )
      {
         int iKey = keys[ n ] >= 128 ? ZH_INKEY_NEW_UNICODE( keys[ n ] ) : keys[ n ];
         zh_inkeyIns( iKey );
      }
      if( nSize > ZH_SIZEOFARRAY( buffer ) )
         zh_xfree( keys );
   }
   else
   {
      while( ZH_CODEPAGE_CHAR_GET( cdp, pszText, nSize, &nIndex, &wc ) )
      {
         int iKey = wc >= 128 ? ZH_INKEY_NEW_UNICODE( wc ) : wc;
         zh_inkeyPut( iKey );
      }
   }
}

ZH_FUNC( INKEY )
{
   int iPCount = zh_pcount();

   zh_retni( zh_inkey( iPCount == 1 || ( iPCount > 1 && ZH_IS_PARAM_NUM( 1 ) ),
                       zh_parnd( 1 ), zh_parnidef( 2, zh_setGetEventMask() ) ) );
}

ZH_FUNC( __KEYBOARD )
{
   /* Clear the typeahead buffer without reallocating the keyboard buffer */
   zh_inkeyReset();

   if( ZH_ISCHAR( 1 ) )
      zh_inkeySetText( zh_parc( 1 ), zh_parclen( 1 ) );
}

ZH_FUNC( ZH_KEYCLEAR )
{
   zh_inkeyReset();
}

ZH_FUNC( ZH_KEYPUT )
{
   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      zh_inkeyPut( zh_parni( 1 ) );
   }
   else if( ZH_ISCHAR( 1 ) )
   {
      zh_inkeySetTextKeys( zh_parc( 1 ), zh_parclen( 1 ), ZH_FALSE );
   }
   else if( ZH_ISARRAY( 1 ) )
   {
      PZH_ITEM pArray = zh_param( 1, ZH_IT_ARRAY );
      ZH_SIZE nIndex;
      ZH_SIZE nElements = zh_arrayLen( pArray );

      for( nIndex = 1; nIndex <= nElements; ++nIndex )
      {
         ZH_TYPE type = zh_arrayGetType( pArray, nIndex );

         if( type & ZH_IT_NUMERIC )
         {
            zh_inkeyPut( zh_arrayGetNI( pArray, nIndex ) );
         }
         else if( type & ZH_IT_STRING )
         {
            zh_inkeySetTextKeys( zh_arrayGetCPtr( pArray, nIndex ),
                                 zh_arrayGetCLen( pArray, nIndex ), ZH_FALSE );
         }
      }
   }
}

ZH_FUNC( ZH_KEYINS )
{
   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      zh_inkeyIns( zh_parni( 1 ) );
   }
   else if( ZH_ISCHAR( 1 ) )
   {
      zh_inkeySetTextKeys( zh_parc( 1 ), zh_parclen( 1 ), ZH_TRUE );
   }
   else if( ZH_ISARRAY( 1 ) )
   {
      PZH_ITEM pArray = zh_param( 1, ZH_IT_ARRAY );
      ZH_SIZE nIndex;
      ZH_SIZE nElements = zh_arrayLen( pArray );

      for( nIndex = 1; nIndex <= nElements; ++nIndex )
      {
         ZH_TYPE type = zh_arrayGetType( pArray, nIndex );

         if( type & ZH_IT_NUMERIC )
         {
            zh_inkeyIns( zh_arrayGetNI( pArray, nIndex ) );
         }
         else if( type & ZH_IT_STRING )
         {
            zh_inkeySetTextKeys( zh_arrayGetCPtr( pArray, nIndex ),
                                 zh_arrayGetCLen( pArray, nIndex ), ZH_TRUE );
         }
      }
   }
}

ZH_FUNC( ZH_KEYNEXT )
{
   zh_retni( zh_inkeyNext( ZH_IS_PARAM_NUM( 1 ) ? zh_parni( 1 ) : zh_setGetEventMask() ) );
}

ZH_FUNC( NEXTKEY )
{
   zh_retni( zh_inkeyNext( zh_setGetEventMask() ) );
}

ZH_FUNC( ZH_KEYLAST )
{
   zh_retni( zh_inkeyLast( ZH_IS_PARAM_NUM( 1 ) ? zh_parni( 1 ) : zh_setGetEventMask() ) );
}

ZH_FUNC( LASTKEY )
{
   zh_retni( zh_inkeyLast( ZH_INKEY_ALL ) );
}

ZH_FUNC( ZH_KEYSETLAST )
{
   if( ZH_IS_PARAM_NUM( 1 ) )
      zh_retni( zh_inkeySetLast( zh_parni( 1 ) ) );
}


ZH_FUNC( ZH_KEYCODE )
{
   const char * szValue = zh_parc( 1 );
   int iKey;

   if( szValue )
   {
      PZH_CODEPAGE cdp = zh_vmCDP();
      ZH_SIZE nIndex = 0;
      ZH_WCHAR wc;

      if( ZH_CODEPAGE_CHAR_GET( cdp, szValue, zh_parclen( 1 ), &nIndex, &wc ) )
         iKey = wc >= 128 ? ZH_INKEY_NEW_UNICODE( wc ) : wc;
      else
         iKey = 0;
   }
   else
      iKey = 0;

   zh_retni( iKey );
}

ZH_FUNC( ZH_KEYCHAR )
{
   char szKeyChr[ ZH_MAX_CHAR_LEN ];
   ZH_SIZE nLen;

   nLen = zh_inkeyKeyString( zh_parni( 1 ), szKeyChr, sizeof( szKeyChr ) );
   zh_retclen( szKeyChr, nLen );
}

ZH_FUNC( ZH_KEYSTD )
{
   zh_retni( zh_inkeyKeyStd( zh_parni( 1 ) ) );
}

ZH_FUNC( ZH_KEYEXT )
{
   zh_retni( zh_inkeyKeyExt( zh_parni( 1 ) ) );
}

ZH_FUNC( ZH_KEYMOD )
{
   zh_retni( zh_inkeyKeyMod( zh_parni( 1 ) ) );
}

ZH_FUNC( ZH_KEYVAL )
{
   zh_retni( zh_inkeyKeyVal( zh_parni( 1 ) ) );
}

ZH_FUNC( ZH_KEYNEW )
{
   PZH_ITEM pText = zh_param( 1, ZH_IT_STRING );
   int iMod = zh_parni( 2 );
   int iKey = pText ? zh_cdpTextGetU16( zh_vmCDP(), zh_itemGetCPtr( pText ),
                                                    zh_itemGetCLen( pText ) ) : zh_parni( 1 );

   if( iKey >= 127 )
      iKey = ZH_INKEY_NEW_UNICODEF( iKey, iMod );
   else if( ! pText || ( iMod & ( ZH_KF_CTRL | ZH_KF_ALT ) ) != 0 )
      iKey = ZH_INKEY_NEW_KEY( iKey, iMod );
   else
      iKey = ZH_INKEY_NEW_CHARF( iKey, iMod );

   zh_retni( iKey );
}
