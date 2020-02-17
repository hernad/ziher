/*
 * The Array API (C level)
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * Copyright 1999-2001 Viktor Szakats
 *   (zh_arrayIsObject(), zh_arrayCopyC(), zh_arrayGetC())
 * Copyright 2001 Ron Pinkas <ron@profit-master.com>
 *   (zh_arrayClone(), zh_arrayFromStack(), zh_arrayFromParams())
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

#include "zh_vm_opt.h"
#include "zh_api.h"
#include "zh_item_api.h"
#include "zh_class_api.h"
#include "zh_api_error.h"
#include "zh_lang_api.h"
#include "zh_vm.h"
#include "zh_stack.h"

static void zh_arrayReleaseItems( PZH_BASEARRAY pBaseArray )
{
   if( pBaseArray->nLen )
   {
      do
      {
         pBaseArray->nLen--;
         if( ZH_IS_COMPLEX( pBaseArray->pItems + pBaseArray->nLen ) )
            zh_itemClear( pBaseArray->pItems + pBaseArray->nLen );
      }
      while( pBaseArray->nLen );

      /* protection against possible base array resizing in user destructors */
      if( pBaseArray->pItems )
      {
         zh_xfree( pBaseArray->pItems );
         pBaseArray->pItems = NULL;
      }
   }
}

void zh_arrayPushBase( PZH_BASEARRAY pBaseArray )
{
   ZH_STACK_TLS_PRELOAD
   PZH_ITEM pItem = zh_stackAllocItem();

   pItem->type = ZH_IT_ARRAY;
   pItem->item.asArray.value = pBaseArray;
   zh_gcRefInc( pBaseArray );
}

/* This releases array when called from the garbage collector */
static ZH_GARBAGE_FUNC( zh_arrayGarbageRelease )
{
   PZH_BASEARRAY pBaseArray = ( PZH_BASEARRAY ) Cargo;

   if( pBaseArray->uiClass )
   {
      /*
       * do not execute destructor for supercasted objects [druzus]
       */
      if( pBaseArray->uiPrevCls == 0 &&
          zh_clsHasDestructor( pBaseArray->uiClass ) )
      {
         ZH_STACK_TLS_PRELOAD
         zh_arrayPushBase( pBaseArray );
         zh_objDestructorCall( zh_stackItemFromTop( -1 ) );
         zh_stackPop();
      }

      /*
       * This is only some additional protection for buggy code
       * which can store reference to this object in other class
       * destructor when executed from GC and it will only cause
       * RT error when user will try to send any message to this
       * object [druzus]
       */
      pBaseArray->uiClass = 0;
   }

   zh_arrayReleaseItems( pBaseArray );
}

static ZH_GARBAGE_FUNC( zh_arrayGarbageMark )
{
   PZH_BASEARRAY pBaseArray = ( PZH_BASEARRAY ) Cargo;

   if( pBaseArray->nLen )
   {
      ZH_SIZE nLen = pBaseArray->nLen;
      PZH_ITEM pItems = pBaseArray->pItems;

      while( nLen-- )
      {
         if( ZH_IS_GCITEM( pItems + nLen ) )
            zh_gcItemRef( pItems + nLen );
      }
   }
}

static const ZH_GC_FUNCS s_gcArrayFuncs =
{
   zh_arrayGarbageRelease,
   zh_arrayGarbageMark
};


ZH_BOOL zh_arrayNew( PZH_ITEM pItem, ZH_SIZE nLen ) /* creates a new array */
{
   PZH_BASEARRAY pBaseArray;
   PZH_ITEM pItems;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayNew(%p, %" ZH_PFS "u)", ( void * ) pItem, nLen ) );

   if( ZH_IS_COMPLEX( pItem ) )
      zh_itemClear( pItem );

   /*
    * allocate memory for items before zh_gcAllocRaw() to be
    * safe for automatic GC activation in zh_xgrab() without
    * calling zh_gcLock()/zh_gcUnlock(). [druzus]
    */
   if( nLen > 0 )
   {
      ZH_SIZE nPos;
      pItems = ( PZH_ITEM ) zh_xgrab( sizeof( ZH_ITEM ) * nLen );
      for( nPos = 0; nPos < nLen; ++nPos )
         ( pItems + nPos )->type = ZH_IT_NIL;
   }
   else
      pItems = NULL;

   pBaseArray = ( PZH_BASEARRAY ) zh_gcAllocRaw( sizeof( ZH_BASEARRAY ), &s_gcArrayFuncs );
   pBaseArray->pItems     = pItems;
   pBaseArray->nLen       = nLen;
   pBaseArray->uiClass    = 0;
   pBaseArray->uiPrevCls  = 0;
   pBaseArray->nAllocated = nLen;
   pItem->type = ZH_IT_ARRAY;
   pItem->item.asArray.value = pBaseArray;

   return ZH_TRUE;
}

void zh_arraySwap( PZH_ITEM pArray1, PZH_ITEM pArray2 )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arraySwap(%p, %p)", ( void * ) pArray1, ( void * ) pArray2 ) );

   if( ZH_IS_ARRAY( pArray1 ) && ZH_IS_ARRAY( pArray2 ) )
   {
      ZH_BASEARRAY tmpBaseArray;

      tmpBaseArray = * pArray1->item.asArray.value;
      * pArray1->item.asArray.value = * pArray2->item.asArray.value;
      * pArray2->item.asArray.value = tmpBaseArray;
   }
}

ZH_BOOL zh_arraySize( PZH_ITEM pArray, ZH_SIZE nLen )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arraySize(%p, %" ZH_PFS "u)", ( void * ) pArray, nLen ) );

   if( ZH_IS_ARRAY( pArray ) )
   {
      PZH_BASEARRAY pBaseArray = pArray->item.asArray.value;

      if( nLen != pBaseArray->nLen )
      {
         ZH_SIZE nPos;

         if( pBaseArray->nLen == 0 )
         {
            pBaseArray->pItems = ( PZH_ITEM ) zh_xgrab( nLen * sizeof( ZH_ITEM ) );
            pBaseArray->nAllocated = nLen;

            for( nPos = 0; nPos < nLen; nPos++ )
               ( pBaseArray->pItems + nPos )->type = ZH_IT_NIL;
         }
         else
         {
            if( pBaseArray->nLen < nLen )
            {
               if( pBaseArray->nAllocated < nLen )
               {
                  /*
                     A common practice is to double allocation buffer size. Thus, making
                     reallocation count logarithmic to total number of added numbers.
                     I've used here a little different formula. ulAllocated is divided by
                     factor 2 ( >> 1 ) and 1 is added to requested size. This algorithm
                     has properties:
                       - reallocation count remains asymptotically logarithmic;
                       - saves memory for large arrays, because reallocation buffer
                         size is not doubled, but multiplied by 1.5;
                       - adding of 1, allows reduce reallocation count for small arrays.
                   */
                  pBaseArray->nAllocated = ( pBaseArray->nAllocated >> 1 ) + 1 + nLen;
                  pBaseArray->pItems = ( PZH_ITEM ) zh_xrealloc( pBaseArray->pItems, sizeof( ZH_ITEM ) * pBaseArray->nAllocated );
               }

               /* set value for new items */
               for( nPos = pBaseArray->nLen; nPos < nLen; nPos++ )
                  ( pBaseArray->pItems + nPos )->type = ZH_IT_NIL;
            }
            else if( pBaseArray->nLen > nLen )
            {
               /* release old items */
               for( nPos = nLen; nPos < pBaseArray->nLen; nPos++ )
               {
                  if( ZH_IS_COMPLEX( pBaseArray->pItems + nPos ) )
                     zh_itemClear( pBaseArray->pItems + nPos );
               }

               if( nLen == 0 )
               {
                  zh_xfree( pBaseArray->pItems );
                  pBaseArray->pItems = NULL;
               }
               else if( nLen < ( pBaseArray->nAllocated >> 1 ) )
               {
                  pBaseArray->pItems = ( PZH_ITEM ) zh_xrealloc( pBaseArray->pItems, sizeof( ZH_ITEM ) * nLen );
                  pBaseArray->nAllocated = nLen;
               }
            }
         }

         pBaseArray->nLen = nLen;
      }

      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}

ZH_SIZE zh_arrayLen( PZH_ITEM pArray )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayLen(%p)", ( void * ) pArray ) );

   if( ZH_IS_ARRAY( pArray ) )
      return pArray->item.asArray.value->nLen;
   else
      return 0;
}

ZH_BOOL zh_arrayIsObject( PZH_ITEM pArray )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayIsObject(%p)", ( void * ) pArray ) );

   if( ZH_IS_ARRAY( pArray ) )
      return pArray->item.asArray.value->uiClass != 0;
   else
      return ZH_FALSE;
}

/* retrieves the array unique ID */
void * zh_arrayId( PZH_ITEM pArray )
{
   if( pArray && ZH_IS_ARRAY( pArray ) )
      return ( void * ) pArray->item.asArray.value;
   else
      return NULL;
}

/* retrieves numer of references to the array */
ZH_COUNTER zh_arrayRefs( PZH_ITEM pArray )
{
   if( pArray && ZH_IS_ARRAY( pArray ) )
      return zh_gcRefCount( pArray->item.asArray.value );
   else
      return 0;
}

PZH_ITEM zh_arrayFromId( PZH_ITEM pItem, void * pArrayId )
{
   ZH_STACK_TLS_PRELOAD

   zh_arrayPushBase( ( PZH_BASEARRAY ) pArrayId );
   if( pItem == NULL )
      pItem = zh_itemNew( NULL );
   zh_itemMove( pItem, zh_stackItemFromTop( -1 ) );
   zh_stackPop();

   return pItem;
}

ZH_BOOL zh_arrayAdd( PZH_ITEM pArray, PZH_ITEM pValue )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayAdd(%p, %p)", ( void * ) pArray, ( void * ) pValue ) );

   if( ZH_IS_ARRAY( pArray ) )
   {
      PZH_BASEARRAY pBaseArray = ( PZH_BASEARRAY ) pArray->item.asArray.value;

      if( pBaseArray->nLen < ZH_SIZE_MAX )
      {
         zh_arraySize( pArray, pBaseArray->nLen + 1 );
         pBaseArray = ( PZH_BASEARRAY ) pArray->item.asArray.value;
         zh_itemCopy( pBaseArray->pItems + ( pBaseArray->nLen - 1 ), pValue );

         return ZH_TRUE;
      }
   }

   return ZH_FALSE;
}

ZH_BOOL zh_arrayAddForward( PZH_ITEM pArray, PZH_ITEM pValue )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayAddForward(%p, %p)", ( void * ) pArray, ( void * ) pValue ) );

   if( ZH_IS_ARRAY( pArray ) )
   {
      PZH_BASEARRAY pBaseArray = ( PZH_BASEARRAY ) pArray->item.asArray.value;

      if( pBaseArray->nLen < ZH_SIZE_MAX )
      {
         zh_arraySize( pArray, pBaseArray->nLen + 1 );
         pBaseArray = ( PZH_BASEARRAY ) pArray->item.asArray.value;
         zh_itemMove( pBaseArray->pItems + ( pBaseArray->nLen - 1 ), pValue );

         return ZH_TRUE;
      }
   }

   return ZH_FALSE;
}

ZH_BOOL zh_arrayDel( PZH_ITEM pArray, ZH_SIZE nIndex )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayDel(%p, %" ZH_PFS "u)", ( void * ) pArray, nIndex ) );

   if( ZH_IS_ARRAY( pArray ) )
   {
      ZH_SIZE nLen = pArray->item.asArray.value->nLen;

      if( nIndex > 0 && nIndex <= nLen )
      {
         PZH_BASEARRAY pBaseArray = pArray->item.asArray.value;

         if( nIndex == nLen )
         {
            zh_itemSetNil( pBaseArray->pItems + nIndex - 1 );
         }
         else
         {
            for(; nIndex < nLen; ++nIndex )        /* move items */
               zh_itemMoveRef( pBaseArray->pItems + nIndex - 1,
                               pBaseArray->pItems + nIndex );
         }

         return ZH_TRUE;
      }
   }

   return ZH_FALSE;
}

ZH_BOOL zh_arrayIns( PZH_ITEM pArray, ZH_SIZE nIndex )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayIns(%p, %" ZH_PFS "u)", ( void * ) pArray, nIndex ) );

   if( ZH_IS_ARRAY( pArray ) )
   {
      ZH_SIZE nLen = pArray->item.asArray.value->nLen;

      if( nIndex > 0 && nIndex <= nLen )
      {
         PZH_BASEARRAY pBaseArray = pArray->item.asArray.value;

         if( nIndex == nLen )
         {
            zh_itemSetNil( pBaseArray->pItems + nIndex - 1 );
         }
         else
         {
            while( --nLen >= nIndex )                     /* move items */
               zh_itemMoveRef( pBaseArray->pItems + nLen,
                               pBaseArray->pItems + nLen - 1 );
         }

         return ZH_TRUE;
      }
   }

   return ZH_FALSE;
}

ZH_BOOL zh_arraySet( PZH_ITEM pArray, ZH_SIZE nIndex, PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arraySet(%p, %" ZH_PFS "u, %p)", ( void * ) pArray, nIndex, ( void * ) pItem ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      zh_itemCopy( pArray->item.asArray.value->pItems + ( nIndex - 1 ), pItem );
      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}

ZH_BOOL zh_arraySetForward( PZH_ITEM pArray, ZH_SIZE nIndex, PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arraySetForward(%p, %" ZH_PFS "u, %p)", ( void * ) pArray, nIndex, ( void * ) pItem ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      zh_itemMove( pArray->item.asArray.value->pItems + ( nIndex - 1 ), pItem );
      return ZH_TRUE;
   }
   else
   {
      zh_itemClear( pItem );
      return ZH_FALSE;
   }
}

ZH_BOOL zh_arrayGet( PZH_ITEM pArray, ZH_SIZE nIndex, PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayGet(%p, %" ZH_PFS "u, %p)", ( void * ) pArray, nIndex, ( void * ) pItem ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      zh_itemCopy( pItem, pArray->item.asArray.value->pItems + ( nIndex - 1 ) );
      return ZH_TRUE;
   }
   else
   {
      zh_itemSetNil( pItem );
      return ZH_FALSE;
   }
}

ZH_BOOL zh_arrayGetItemRef( PZH_ITEM pArray, ZH_SIZE nIndex, PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayGetItemRef(%p, %" ZH_PFS "u, %p)", ( void * ) pArray, nIndex, ( void * ) pItem ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      if( pArray != pItem )
      {
         if( ZH_IS_COMPLEX( pItem ) )
            zh_itemClear( pItem );
         zh_gcRefInc( pArray->item.asArray.value );
      }
      pItem->type = ZH_IT_BYREF;
      pItem->item.asRefer.BasePtr.array = pArray->item.asArray.value;
      pItem->item.asRefer.value = nIndex - 1;
      pItem->item.asRefer.offset = 0;
      return ZH_TRUE;
   }
   else
   {
      zh_itemSetNil( pItem );
      return ZH_FALSE;
   }
}

/*
 * This function returns a pointer to an item occupied by the specified
 * array element - it doesn't return an item's value
 */
PZH_ITEM zh_arrayGetItemPtr( PZH_ITEM pArray, ZH_SIZE nIndex )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayGetItemPtr(%p, %" ZH_PFS "u)", ( void * ) pArray, nIndex ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return pArray->item.asArray.value->pItems + nIndex - 1;
   else
      return NULL;
}

char * zh_arrayGetDS( PZH_ITEM pArray, ZH_SIZE nIndex, char * szDate )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayGetDS(%p, %" ZH_PFS "u, %s)", ( void * ) pArray, nIndex, szDate ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return zh_itemGetDS( pArray->item.asArray.value->pItems + nIndex - 1, szDate );
   else
      /* NOTE: Intentionally calling it with a bad parameter in order to get
               the default value from zh_itemGetDS(). [vszakats] */
      return zh_itemGetDS( NULL, szDate );
}

long zh_arrayGetDL( PZH_ITEM pArray, ZH_SIZE nIndex )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayGetDL(%p, %" ZH_PFS "u)", ( void * ) pArray, nIndex ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return zh_itemGetDL( pArray->item.asArray.value->pItems + nIndex - 1 );
   else
      /* NOTE: Intentionally calling it with a bad parameter in order to get
               the default value from zh_itemGetDL(). [vszakats] */
      return zh_itemGetDL( NULL );
}

double zh_arrayGetTD( PZH_ITEM pArray, ZH_SIZE nIndex )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayGetTD(%p, %" ZH_PFS "u)", ( void * ) pArray, nIndex ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return zh_itemGetTD( pArray->item.asArray.value->pItems + nIndex - 1 );
   else
      return 0;
}

ZH_BOOL zh_arrayGetTDT( PZH_ITEM pArray, ZH_SIZE nIndex, long * plJulian, long * plMilliSec )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayGetTDT(%p, %" ZH_PFS "u, %p, %p)", ( void * ) pArray, nIndex, ( void * ) plJulian, ( void * ) plMilliSec ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return zh_itemGetTDT( pArray->item.asArray.value->pItems + nIndex - 1, plJulian, plMilliSec );
   else
   {
      *plJulian = *plMilliSec = 0;
      return ZH_FALSE;
   }
}

ZH_BOOL zh_arrayGetL( PZH_ITEM pArray, ZH_SIZE nIndex )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayGetL(%p, %" ZH_PFS "u)", ( void * ) pArray, nIndex ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return zh_itemGetL( pArray->item.asArray.value->pItems + nIndex - 1 );
   else
      return ZH_FALSE;
}

int zh_arrayGetNI( PZH_ITEM pArray, ZH_SIZE nIndex )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayGetNI(%p, %" ZH_PFS "u)", ( void * ) pArray, nIndex ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return zh_itemGetNI( pArray->item.asArray.value->pItems + nIndex - 1 );
   else
      return 0;
}

long zh_arrayGetNL( PZH_ITEM pArray, ZH_SIZE nIndex )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayGetNL(%p, %" ZH_PFS "u)", ( void * ) pArray, nIndex ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return zh_itemGetNL( pArray->item.asArray.value->pItems + nIndex - 1 );
   else
      return 0;
}

ZH_ISIZ zh_arrayGetNS( PZH_ITEM pArray, ZH_SIZE nIndex )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayGetNS(%p, %" ZH_PFS "u)", ( void * ) pArray, nIndex ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return zh_itemGetNS( pArray->item.asArray.value->pItems + nIndex - 1 );
   else
      return 0;
}

#ifndef ZH_LONG_LONG_OFF
ZH_LONGLONG zh_arrayGetNLL( PZH_ITEM pArray, ZH_SIZE nIndex )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayGetNLL(%p, %" ZH_PFS "u)", ( void * ) pArray, nIndex ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return zh_itemGetNLL( pArray->item.asArray.value->pItems + nIndex - 1 );
   else
      return 0;
}
#endif

ZH_MAXINT zh_arrayGetNInt( PZH_ITEM pArray, ZH_SIZE nIndex )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayGetNInt(%p, %" ZH_PFS "u)", ( void * ) pArray, nIndex ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return zh_itemGetNInt( pArray->item.asArray.value->pItems + nIndex - 1 );
   else
      return 0;
}

double zh_arrayGetND( PZH_ITEM pArray, ZH_SIZE nIndex )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayGetND(%p, %" ZH_PFS "u)", ( void * ) pArray, nIndex ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return zh_itemGetND( pArray->item.asArray.value->pItems + nIndex - 1 );
   else
      return 0;
}

ZH_SIZE zh_arrayCopyC( PZH_ITEM pArray, ZH_SIZE nIndex, char * szBuffer, ZH_SIZE nLen )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayCopyC(%p, %" ZH_PFS "u, %s, %" ZH_PFS "u)", ( void * ) pArray, nIndex, szBuffer, nLen ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return zh_itemCopyC( pArray->item.asArray.value->pItems + nIndex - 1, szBuffer, nLen );
   else
      return 0;
}

char * zh_arrayGetC( PZH_ITEM pArray, ZH_SIZE nIndex )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayGetC(%p, %" ZH_PFS "u)", ( void * ) pArray, nIndex ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return zh_itemGetC( pArray->item.asArray.value->pItems + nIndex - 1 );
   else
      return NULL;
}

const char * zh_arrayGetCPtr( PZH_ITEM pArray, ZH_SIZE nIndex )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayGetCPtr(%p, %" ZH_PFS "u)", ( void * ) pArray, nIndex ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return zh_itemGetCPtr( pArray->item.asArray.value->pItems + nIndex - 1 );
   else
      return "";
}

ZH_SIZE zh_arrayGetCLen( PZH_ITEM pArray, ZH_SIZE nIndex )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayGetCLen(%p, %" ZH_PFS "u)", ( void * ) pArray, nIndex ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return zh_itemGetCLen( pArray->item.asArray.value->pItems + nIndex - 1 );
   else
      return 0;
}

void * zh_arrayGetPtr( PZH_ITEM pArray, ZH_SIZE nIndex )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayGetPtr(%p, %" ZH_PFS "u)", ( void * ) pArray, nIndex ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return zh_itemGetPtr( pArray->item.asArray.value->pItems + nIndex - 1 );
   else
      return NULL;
}

void * zh_arrayGetPtrGC( PZH_ITEM pArray, ZH_SIZE nIndex, const ZH_GC_FUNCS * pFuncs )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayGetPtrGC(%p, %" ZH_PFS "u, %p)", ( void * ) pArray, nIndex, ( const void * ) pFuncs ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return zh_itemGetPtrGC( pArray->item.asArray.value->pItems + nIndex - 1, pFuncs );
   else
      return NULL;
}

PZH_SYMB zh_arrayGetSymbol( PZH_ITEM pArray, ZH_SIZE nIndex )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayGetSymbol(%p, %" ZH_PFS "u)", ( void * ) pArray, nIndex ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return zh_itemGetSymbol( pArray->item.asArray.value->pItems + nIndex - 1 );
   else
      return NULL;
}


ZH_TYPE zh_arrayGetType( PZH_ITEM pArray, ZH_SIZE nIndex )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayGetType(%p, %" ZH_PFS "u)", ( void * ) pArray, nIndex ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return zh_itemType( pArray->item.asArray.value->pItems + nIndex - 1 );
   else
      return 0;
}

ZH_BOOL zh_arraySetDS( PZH_ITEM pArray, ZH_SIZE nIndex, const char * szDate )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arraySetDS(%p, %" ZH_PFS "u, %s)", ( void * ) pArray, nIndex, szDate ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      zh_itemPutDS( pArray->item.asArray.value->pItems + nIndex - 1, szDate );
      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}

ZH_BOOL zh_arraySetDL( PZH_ITEM pArray, ZH_SIZE nIndex, long lDate )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arraySetDL(%p, %" ZH_PFS "u, %ld)", ( void * ) pArray, nIndex, lDate ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      zh_itemPutDL( pArray->item.asArray.value->pItems + nIndex - 1, lDate );
      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}

ZH_BOOL zh_arraySetTD( PZH_ITEM pArray, ZH_SIZE nIndex, double dTimeStamp )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arraySetTD(%p, %" ZH_PFS "u, %lf)", ( void * ) pArray, nIndex, dTimeStamp ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      zh_itemPutTD( pArray->item.asArray.value->pItems + nIndex - 1, dTimeStamp );
      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}

ZH_BOOL zh_arraySetTDT( PZH_ITEM pArray, ZH_SIZE nIndex, long lJulian, long lMilliSec )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arraySetTDT(%p, %" ZH_PFS "u, %lu, %lu)", ( void * ) pArray, nIndex, lJulian, lMilliSec ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      zh_itemPutTDT( pArray->item.asArray.value->pItems + nIndex - 1, lJulian, lMilliSec );
      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}

ZH_BOOL zh_arraySetL( PZH_ITEM pArray, ZH_SIZE nIndex, ZH_BOOL fValue )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arraySetL(%p, %" ZH_PFS "u, %d)", ( void * ) pArray, nIndex, fValue ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      zh_itemPutL( pArray->item.asArray.value->pItems + nIndex - 1, fValue );
      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}

ZH_BOOL zh_arraySetNI( PZH_ITEM pArray, ZH_SIZE nIndex, int iNumber )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arraySetNI(%p, %" ZH_PFS "u, %d)", ( void * ) pArray, nIndex, iNumber ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      zh_itemPutNI( pArray->item.asArray.value->pItems + nIndex - 1, iNumber );
      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}

ZH_BOOL zh_arraySetNL( PZH_ITEM pArray, ZH_SIZE nIndex, long lNumber )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arraySetNL(%p, %" ZH_PFS "u, %lu)", ( void * ) pArray, nIndex, lNumber ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      zh_itemPutNL( pArray->item.asArray.value->pItems + nIndex - 1, lNumber );
      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}

ZH_BOOL zh_arraySetNS( PZH_ITEM pArray, ZH_SIZE nIndex, ZH_ISIZ nNumber )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arraySetNS(%p, %" ZH_PFS "u, %" ZH_PFS "d)", ( void * ) pArray, nIndex, nNumber ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      zh_itemPutNS( pArray->item.asArray.value->pItems + nIndex - 1, nNumber );
      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}

#ifndef ZH_LONG_LONG_OFF
ZH_BOOL zh_arraySetNLL( PZH_ITEM pArray, ZH_SIZE nIndex, ZH_LONGLONG llNumber )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arraySetNLL(%p, %" ZH_PFS "u, %" PFLL "d)", ( void * ) pArray, nIndex, llNumber ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      zh_itemPutNLL( pArray->item.asArray.value->pItems + nIndex - 1, llNumber );
      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}
#endif

ZH_BOOL zh_arraySetNInt( PZH_ITEM pArray, ZH_SIZE nIndex, ZH_MAXINT nNumber )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arraySetNInt(%p, %" ZH_PFS "u, %" PFHL "d)", ( void * ) pArray, nIndex, nNumber ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      zh_itemPutNInt( pArray->item.asArray.value->pItems + nIndex - 1, nNumber );
      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}

ZH_BOOL zh_arraySetND( PZH_ITEM pArray, ZH_SIZE nIndex, double dNumber )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arraySetND(%p, %" ZH_PFS "u, %lf)", ( void * ) pArray, nIndex, dNumber ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      zh_itemPutND( pArray->item.asArray.value->pItems + nIndex - 1, dNumber );
      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}

ZH_BOOL zh_arraySetC( PZH_ITEM pArray, ZH_SIZE nIndex, const char * szText )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arraySetC(%p, %" ZH_PFS "u, %p)", ( void * ) pArray, nIndex, ( const void * ) szText ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      zh_itemPutC( pArray->item.asArray.value->pItems + nIndex - 1, szText );
      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}

ZH_BOOL zh_arraySetCL( PZH_ITEM pArray, ZH_SIZE nIndex, const char * szText, ZH_SIZE nLen )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arraySetC(%p, %" ZH_PFS "u, %p, %" ZH_PFS "u)", ( void * ) pArray, nIndex, ( const void * ) szText, nLen ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      zh_itemPutCL( pArray->item.asArray.value->pItems + nIndex - 1, szText, nLen );
      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}

ZH_BOOL zh_arraySetCPtr( PZH_ITEM pArray, ZH_SIZE nIndex, char * szText )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arraySetCPtr(%p, %" ZH_PFS "u, %p)", ( void * ) pArray, nIndex, ( void * ) szText ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      zh_itemPutCPtr( pArray->item.asArray.value->pItems + nIndex - 1, szText );
      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}

ZH_BOOL zh_arraySetCLPtr( PZH_ITEM pArray, ZH_SIZE nIndex, char * szText, ZH_SIZE nLen )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arraySetCLPtr(%p, %" ZH_PFS "u, %p, %" ZH_PFS "u)", ( void * ) pArray, nIndex, ( void * ) szText, nLen ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      zh_itemPutCLPtr( pArray->item.asArray.value->pItems + nIndex - 1, szText, nLen );
      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}

ZH_BOOL zh_arraySetCConst( PZH_ITEM pArray, ZH_SIZE nIndex, const char * szText )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arraySetCConst(%p, %" ZH_PFS "u, %p)", ( void * ) pArray, nIndex, ( const void * ) szText ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      zh_itemPutCConst( pArray->item.asArray.value->pItems + nIndex - 1, szText );
      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}

ZH_BOOL zh_arraySetPtr( PZH_ITEM pArray, ZH_SIZE nIndex, void * pValue )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arraySetPtr(%p, %" ZH_PFS "u, %p)", ( void * ) pArray, nIndex, pValue ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      zh_itemPutPtr( pArray->item.asArray.value->pItems + nIndex - 1, pValue );
      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}

ZH_BOOL zh_arraySetPtrGC( PZH_ITEM pArray, ZH_SIZE nIndex, void * pValue )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arraySetPtrGC(%p, %" ZH_PFS "u, %p)", ( void * ) pArray, nIndex, pValue ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      zh_itemPutPtrGC( pArray->item.asArray.value->pItems + nIndex - 1, pValue );
      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}

ZH_BOOL zh_arraySetSymbol( PZH_ITEM pArray, ZH_SIZE nIndex, PZH_SYMB pSymbol )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arraySetSymbol(%p, %" ZH_PFS "u, %p)", ( void * ) pArray, nIndex, ( void * ) pSymbol ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      zh_itemPutSymbol( pArray->item.asArray.value->pItems + nIndex - 1, pSymbol );
      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}

ZH_BOOL zh_arrayLast( PZH_ITEM pArray, PZH_ITEM pResult )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayLast(%p, %p)", ( void * ) pArray, ( void * ) pResult ) );

   if( ZH_IS_ARRAY( pArray ) )
   {
      if( pArray->item.asArray.value->nLen > 0 )
         zh_itemCopy( pResult, pArray->item.asArray.value->pItems +
                             ( pArray->item.asArray.value->nLen - 1 ) );
      else
         zh_itemSetNil( pResult );

      return ZH_TRUE;
   }

   zh_itemSetNil( pResult );

   return ZH_FALSE;
}

ZH_BOOL zh_arrayFill( PZH_ITEM pArray, PZH_ITEM pValue, ZH_SIZE * pnStart, ZH_SIZE * pnCount )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayFill(%p, %p, %p, %p)", ( void * ) pArray, ( void * ) pValue, ( void * ) pnStart, ( void * ) pnCount ) );

   if( ZH_IS_ARRAY( pArray ) )
   {
      PZH_BASEARRAY pBaseArray = pArray->item.asArray.value;
      ZH_SIZE nLen = pBaseArray->nLen;
      ZH_SIZE nStart;

      if( pnStart && *pnStart )
         nStart = *pnStart - 1;
      else
         nStart = 0;

      if( nStart < nLen )
      {
         ZH_SIZE nCount = nLen - nStart;
         if( pnCount && *pnCount < nCount )
            nCount = *pnCount;

         if( nCount > 0 )
         {
            do
            {
               zh_itemCopy( pBaseArray->pItems + nStart++, pValue );
            }
            while( --nCount > 0 );
         }
      }

      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}

ZH_SIZE zh_arrayScanCase( PZH_ITEM pArray, PZH_ITEM pValue, ZH_SIZE * pnStart, ZH_SIZE * pnCount, ZH_BOOL fExact, ZH_BOOL fMatchCase )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayScanCase(%p, %p, %p, %p, %d, %d)", ( void * ) pArray, ( void * ) pValue, ( void * ) pnStart, ( void * ) pnCount, ( int ) fExact, ( int ) fMatchCase ) );

   if( ZH_IS_ARRAY( pArray ) )
   {
      PZH_BASEARRAY pBaseArray = pArray->item.asArray.value;
      ZH_SIZE nLen = pBaseArray->nLen;
      ZH_SIZE nStart;

      if( pnStart && *pnStart )
         nStart = *pnStart - 1;
      else
         nStart = 0;

      if( nStart < nLen )
      {
         ZH_SIZE nCount = nLen - nStart;
         if( pnCount && *pnCount < nCount )
            nCount = *pnCount;

         if( nCount > 0 )
         {
            /* Make separate search loops for different types to find, so that
               the loop can be faster. */

            if( ZH_IS_BLOCK( pValue ) )
            {
               ZH_STACK_TLS_PRELOAD
               do
               {
                  zh_vmPushEvalSym();
                  zh_vmPush( pValue );
                  zh_vmPush( pBaseArray->pItems + nStart );
                  zh_vmPushSize( ++nStart );
                  zh_vmEval( 2 );

                  if( ZH_IS_LOGICAL( zh_stackReturnItem() ) && zh_stackReturnItem()->item.asLogical.value )
                     return nStart;
               }
               while( --nCount > 0 && nStart < pBaseArray->nLen );
            }
            else if( ZH_IS_STRING( pValue ) )
            {
               int ( * pzh_itemStrCmp )( PZH_ITEM, PZH_ITEM, ZH_BOOL ) =
                  fMatchCase ? zh_itemStrCmp : zh_itemStrICmp;

               do
               {
                  PZH_ITEM pItem = pBaseArray->pItems + nStart++;

                  /* NOTE: The order of the pItem and pValue parameters passed to
                           zh_itemStrCmp()/zh_itemStrICmp() is significant,
                           please don't change it. [vszakats] */
                  if( ZH_IS_STRING( pItem ) && pzh_itemStrCmp( pItem, pValue, fExact ) == 0 )
                     return nStart;
               }
               while( --nCount > 0 );
            }
            else if( ZH_IS_NUMINT( pValue ) )
            {
               ZH_MAXINT nValue = zh_itemGetNInt( pValue );

               do
               {
                  PZH_ITEM pItem = pBaseArray->pItems + nStart++;

                  if( ZH_IS_NUMERIC( pItem ) && zh_itemGetNInt( pItem ) == nValue &&
                      zh_itemGetND( pItem ) == ( double ) nValue )
                     return nStart;
               }
               while( --nCount > 0 );
            }
            else if( ZH_IS_NUMERIC( pValue ) )
            {
               double dValue = zh_itemGetND( pValue );

               do
               {
                  PZH_ITEM pItem = pBaseArray->pItems + nStart++;

                  if( ZH_IS_NUMERIC( pItem ) && zh_itemGetND( pItem ) == dValue )
                     return nStart;
               }
               while( --nCount > 0 );
            }
            else if( ZH_IS_DATETIME( pValue ) )
            {
               if( fExact )
               {
                  do
                  {
                     PZH_ITEM pItem = pBaseArray->pItems + nStart++;

                     if( ZH_IS_DATETIME( pItem ) &&
                         pItem->item.asDateTime.julian == pValue->item.asDateTime.julian &&
                         pItem->item.asDateTime.time == pValue->item.asDateTime.time )
                        return nStart;
                  }
                  while( --nCount > 0 );
               }
               else
               {
                  do
                  {
                     PZH_ITEM pItem = pBaseArray->pItems + nStart++;

                     if( ZH_IS_DATETIME( pItem ) &&
                         pItem->item.asDateTime.julian == pValue->item.asDateTime.julian )
                        return nStart;
                  }
                  while( --nCount > 0 );
               }
            }
            else if( ZH_IS_LOGICAL( pValue ) )
            {
               ZH_BOOL bValue = zh_itemGetL( pValue );

               do
               {
                  PZH_ITEM pItem = pBaseArray->pItems + nStart++;

                  if( ZH_IS_LOGICAL( pItem ) && zh_itemGetL( pItem ) == bValue )
                     return nStart;
               }
               while( --nCount > 0 );
            }
            else if( ZH_IS_NIL( pValue ) )
            {
               do
               {
                  PZH_ITEM pItem = pBaseArray->pItems + nStart++;

                  if( ZH_IS_NIL( pItem ) )
                     return nStart;
               }
               while( --nCount > 0 );
            }
            else if( ZH_IS_POINTER( pValue ) )
            {
               do
               {
                  PZH_ITEM pItem = pBaseArray->pItems + nStart++;

                  if( ZH_IS_POINTER( pItem ) &&
                      pItem->item.asPointer.value == pValue->item.asPointer.value )
                     return nStart;
               }
               while( --nCount > 0 );
            }
            else if( fExact && ZH_IS_ARRAY( pValue ) )
            {
               do
               {
                  PZH_ITEM pItem = pBaseArray->pItems + nStart++;

                  if( ZH_IS_ARRAY( pItem ) &&
                      pItem->item.asArray.value == pValue->item.asArray.value )
                     return nStart;
               }
               while( --nCount > 0 );
            }
            else if( fExact && ZH_IS_HASH( pValue ) )
            {
               do
               {
                  PZH_ITEM pItem = pBaseArray->pItems + nStart++;

                  if( ZH_IS_HASH( pItem ) &&
                      pItem->item.asHash.value == pValue->item.asHash.value )
                     return nStart;
               }
               while( --nCount > 0 );
            }
         }
      }
   }

   return 0;
}

ZH_SIZE zh_arrayRevScan( PZH_ITEM pArray, PZH_ITEM pValue, ZH_SIZE * pnStart, ZH_SIZE * pnCount, ZH_BOOL fExact )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayRevScan(%p, %p, %p, %p, %d)", ( void * ) pArray, ( void * ) pValue, ( void * ) pnStart, ( void * ) pnCount, ( int ) fExact ) );

   if( ZH_IS_ARRAY( pArray ) )
   {
      PZH_BASEARRAY pBaseArray = pArray->item.asArray.value;
      ZH_SIZE nLen = pBaseArray->nLen;
      ZH_SIZE nStart;

      if( pnStart && *pnStart )
         nStart = *pnStart - 1;
      else
         nStart = nLen - 1;

      if( nStart < nLen )
      {
         ZH_SIZE nCount = nStart + 1;
         if( pnCount && *pnCount < nCount )
            nCount = *pnCount;

         if( nCount > 0 )
         {
            /* Make separate search loops for different types to find, so that
               the loop can be faster. */

            if( ZH_IS_BLOCK( pValue ) )
            {
               ZH_STACK_TLS_PRELOAD
               do
               {
                  zh_vmPushEvalSym();
                  zh_vmPush( pValue );
                  if( nStart < pBaseArray->nLen )
                     zh_vmPush( pBaseArray->pItems + nStart );
                  else
                     zh_vmPushNil();
                  zh_vmPushSize( nStart + 1 );
                  zh_vmEval( 2 );

                  if( ZH_IS_LOGICAL( zh_stackReturnItem() ) && zh_stackReturnItem()->item.asLogical.value )
                     return nStart + 1;
               }
               while( --nCount && nStart-- );
            }
            else if( ZH_IS_STRING( pValue ) )
            {
               do
               {
                  PZH_ITEM pItem = pBaseArray->pItems + nStart;

                  /* NOTE: The order of the pItem and pValue parameters passed to
                           zh_itemStrCmp() is significant, please don't change it. [vszakats] */
                  if( ZH_IS_STRING( pItem ) && zh_itemStrCmp( pItem, pValue, fExact ) == 0 )
                     return nStart + 1;
               }
               while( --nCount && nStart-- );
            }
            else if( ZH_IS_NUMINT( pValue ) )
            {
               ZH_MAXINT nValue = zh_itemGetNInt( pValue );

               do
               {
                  PZH_ITEM pItem = pBaseArray->pItems + nStart;

                  if( ZH_IS_NUMERIC( pItem ) && zh_itemGetNInt( pItem ) == nValue &&
                      zh_itemGetND( pItem ) == ( double ) nValue )
                     return nStart + 1;
               }
               while( --nCount && nStart-- );
            }
            else if( ZH_IS_NUMERIC( pValue ) )
            {
               double dValue = zh_itemGetND( pValue );

               do
               {
                  PZH_ITEM pItem = pBaseArray->pItems + nStart;

                  if( ZH_IS_NUMERIC( pItem ) && zh_itemGetND( pItem ) == dValue )
                     return nStart + 1;
               }
               while( --nCount && nStart-- );
            }
            else if( ZH_IS_DATETIME( pValue ) )
            {
               if( fExact )
               {
                  do
                  {
                     PZH_ITEM pItem = pBaseArray->pItems + nStart;

                     if( ZH_IS_DATETIME( pItem ) &&
                         pItem->item.asDateTime.julian == pValue->item.asDateTime.julian &&
                         pItem->item.asDateTime.time == pValue->item.asDateTime.time )
                        return nStart + 1;
                  }
                  while( --nCount && nStart-- );
               }
               else
               {
                  do
                  {
                     PZH_ITEM pItem = pBaseArray->pItems + nStart;

                     if( ZH_IS_DATETIME( pItem ) &&
                         pItem->item.asDateTime.julian == pValue->item.asDateTime.julian )
                        return nStart + 1;
                  }
                  while( --nCount && nStart-- );
               }
            }
            else if( ZH_IS_LOGICAL( pValue ) )
            {
               ZH_BOOL bValue = zh_itemGetL( pValue );

               do
               {
                  PZH_ITEM pItem = pBaseArray->pItems + nStart;

                  if( ZH_IS_LOGICAL( pItem ) && zh_itemGetL( pItem ) == bValue )
                     return nStart + 1;
               }
               while( --nCount && nStart-- );
            }
            else if( ZH_IS_NIL( pValue ) )
            {
               do
               {
                  PZH_ITEM pItem = pBaseArray->pItems + nStart;

                  if( ZH_IS_NIL( pItem ) )
                     return nStart + 1;
               }
               while( --nCount && nStart-- );
            }
            else if( ZH_IS_POINTER( pValue ) )
            {
               do
               {
                  PZH_ITEM pItem = pBaseArray->pItems + nStart;

                  if( ZH_IS_POINTER( pItem ) &&
                      pItem->item.asPointer.value == pValue->item.asPointer.value )
                     return nStart + 1;
               }
               while( --nCount && nStart-- );
            }
            else if( fExact && ZH_IS_ARRAY( pValue ) )
            {
               do
               {
                  PZH_ITEM pItem = pBaseArray->pItems + nStart;

                  if( ZH_IS_ARRAY( pItem ) &&
                      pItem->item.asArray.value == pValue->item.asArray.value )
                     return nStart + 1;
               }
               while( --nCount && nStart-- );
            }
            else if( fExact && ZH_IS_HASH( pValue ) )
            {
               do
               {
                  PZH_ITEM pItem = pBaseArray->pItems + nStart;

                  if( ZH_IS_HASH( pItem ) &&
                      pItem->item.asHash.value == pValue->item.asHash.value )
                     return nStart + 1;
               }
               while( --nCount && nStart-- );
            }
         }
      }
   }

   return 0;
}

ZH_BOOL zh_arrayEval( PZH_ITEM pArray, PZH_ITEM bBlock, ZH_SIZE * pnStart, ZH_SIZE * pnCount )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayEval(%p, %p, %p, %p)", ( void * ) pArray, ( void * ) bBlock, ( void * ) pnStart, ( void * ) pnCount ) );

   if( ZH_IS_ARRAY( pArray ) && ZH_IS_BLOCK( bBlock ) )
   {
      PZH_BASEARRAY pBaseArray = pArray->item.asArray.value;
      ZH_SIZE nLen = pBaseArray->nLen;
      ZH_SIZE nStart;

      if( pnStart && *pnStart )
         nStart = *pnStart - 1;
      else
         nStart = 0;

      if( nStart < nLen )
      {
         ZH_SIZE nCount = nLen - nStart;
         if( pnCount && *pnCount < nCount )
            nCount = *pnCount;

         if( nCount > 0 )
         {
            do
            {
               zh_vmPushEvalSym();
               zh_vmPush( bBlock );
               zh_vmPush( pBaseArray->pItems + nStart );
               zh_vmPushSize( nStart + 1 );
               zh_vmEval( 2 );
            }
            while( --nCount > 0 && ++nStart < pBaseArray->nLen );
            /*
             * checking for nStart < pBaseArray->nLen is fix for
             * possible GPF when codeblock decrease array size
             */
         }
      }

      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}

/* NOTE: CA-Cl*pper 5.3a has a fix for the case when the starting position
         is greater than the length of the array. [vszakats] */

ZH_BOOL zh_arrayCopy( PZH_ITEM pSrcArray, PZH_ITEM pDstArray, ZH_SIZE * pnStart,
                      ZH_SIZE * pnCount, ZH_SIZE * pnTarget )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayCopy(%p, %p, %p, %p, %p)", ( void * ) pSrcArray, ( void * ) pDstArray, ( void * ) pnStart, ( void * ) pnCount, ( void * ) pnTarget ) );

   if( ZH_IS_ARRAY( pSrcArray ) && ZH_IS_ARRAY( pDstArray ) )
   {
      PZH_BASEARRAY pSrcBaseArray = pSrcArray->item.asArray.value;
      PZH_BASEARRAY pDstBaseArray = pDstArray->item.asArray.value;
      ZH_SIZE nSrcLen = pSrcBaseArray->nLen;
      ZH_SIZE nDstLen = pDstBaseArray->nLen;
      ZH_SIZE nStart;
      ZH_SIZE nTarget;

      if( pnStart && ( *pnStart >= 1 ) )
         nStart = *pnStart;
      else
         nStart = 1;

      if( pnTarget && ( *pnTarget >= 1 ) )
         nTarget = *pnTarget;
      else
         nTarget = 1;


      if( nStart <= nSrcLen )
      {
         ZH_SIZE nCount;

         if( pnCount && ( *pnCount <= nSrcLen - nStart ) )
            nCount = *pnCount;
         else
            nCount = nSrcLen - nStart + 1;

         if( nDstLen > 0 )
         {
            if( nTarget > nDstLen )
               nTarget = nDstLen;
            if( pDstBaseArray->pItems + nTarget != pSrcBaseArray->pItems + nStart )
            {
               if( nCount > nDstLen - nTarget )
                  nCount = nDstLen - nTarget + 1;

               for( nTarget--, nStart--; nCount > 0; nCount--, nStart++, nTarget++ )
                  zh_itemCopy( pDstBaseArray->pItems + nTarget, pSrcBaseArray->pItems + nStart );
            }
         }
      }

      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}

static void zh_arrayCloneBody( PZH_ITEM pDest, PZH_ITEM pArray, PZH_NESTED_CLONED pClonedList )
{
   PZH_ITEM pSrcItem, pDstItem;
   ZH_SIZE nLen;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayCloneBody(%p, %p, %p)", ( void * ) pDest, ( void * ) pArray, ( void * ) pClonedList ) );

   nLen = pArray->item.asArray.value->nLen;
   zh_arrayNew( pDest, nLen );
   pDest->item.asArray.value->uiClass = pArray->item.asArray.value->uiClass;
   pSrcItem = pArray->item.asArray.value->pItems;
   pDstItem = pDest->item.asArray.value->pItems;

   while( nLen-- )
      zh_nestedCloneDo( pDstItem++, pSrcItem++, pClonedList );
}

void zh_nestedCloneInit( PZH_NESTED_CLONED pClonedList, void * pValue, PZH_ITEM pDest )
{
   pClonedList->nSize  = 16;
   pClonedList->nCount = 1;
   pClonedList->pRefs  = ( PZH_NESTED_REF )
                     zh_xgrab( pClonedList->nSize * sizeof( ZH_NESTED_REF ) );
   pClonedList->pRefs[ 0 ].value = pValue;
   pClonedList->pRefs[ 0 ].pDest = pDest;
}

void zh_nestedCloneFree( PZH_NESTED_CLONED pClonedList )
{
   zh_xfree( pClonedList->pRefs );
}

static ZH_BOOL zh_nestedCloneFind( PZH_NESTED_CLONED pClonedList, void * pValue, PZH_ITEM pDest )
{
   ZH_SIZE nFirst, nLast, nMiddle;
   PZH_NESTED_REF pRef;

   nFirst = 0;
   nLast = pClonedList->nCount;
   nMiddle = ( nFirst + nLast ) >> 1;
   pRef = pClonedList->pRefs;

   while( nFirst < nLast )
   {
      if( ( ZH_PTRUINT ) pRef[ nMiddle ].value < ( ZH_PTRUINT ) pValue )
         nFirst = nMiddle + 1;
      else if( ( ZH_PTRUINT ) pRef[ nMiddle ].value > ( ZH_PTRUINT ) pValue )
         nLast = nMiddle;
      else
      {
         zh_itemCopy( pDest, pRef[ nMiddle ].pDest );
         return ZH_TRUE;
      }
      nMiddle = ( nFirst + nLast ) >> 1;
   }

   if( pClonedList->nCount >= pClonedList->nSize )
   {
      pClonedList->nSize += pClonedList->nSize >> 1;
      pClonedList->pRefs = ( PZH_NESTED_REF )
                  zh_xrealloc( pClonedList->pRefs,
                               pClonedList->nSize * sizeof( ZH_NESTED_REF ) );
   }

   pRef = &pClonedList->pRefs[ nMiddle ];
   if( nMiddle < pClonedList->nCount )
      memmove( pRef + 1, pRef,
               ( pClonedList->nCount - nMiddle ) * sizeof( ZH_NESTED_REF ) );
   pClonedList->nCount++;

   pRef->value = pValue;
   pRef->pDest = pDest;

   return ZH_FALSE;
}

void zh_nestedCloneDo( PZH_ITEM pDstItem, PZH_ITEM pSrcItem, PZH_NESTED_CLONED pClonedList )
{
   if( ZH_IS_ARRAY( pSrcItem ) )
   {
      if( ! zh_nestedCloneFind( pClonedList, ( void * ) pSrcItem->item.asArray.value, pDstItem ) )
      {
         if( pSrcItem->item.asArray.value->uiClass != 0 )
            zh_objCloneBody( pDstItem, pSrcItem, pClonedList );
         else
            zh_arrayCloneBody( pDstItem, pSrcItem, pClonedList );
      }
   }
   else if( ZH_IS_HASH( pSrcItem ) )
   {
      if( ! zh_nestedCloneFind( pClonedList, ( void * ) pSrcItem->item.asHash.value, pDstItem ) )
         zh_hashCloneBody( pDstItem, pSrcItem, pClonedList );
   }
   else
      zh_itemCopy( pDstItem, pSrcItem );
}

PZH_ITEM zh_arrayCloneTo( PZH_ITEM pDest, PZH_ITEM pArray )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayCloneTo(%p,%p)", ( void * ) pDest, ( void * ) pArray ) );

   if( ZH_IS_ARRAY( pArray ) )
   {
      ZH_NESTED_CLONED clonedList;

      zh_nestedCloneInit( &clonedList, ( void * ) pArray->item.asArray.value, pDest );
      zh_arrayCloneBody( pDest, pArray, &clonedList );
      zh_nestedCloneFree( &clonedList );
   }
   return pDest;
}

PZH_ITEM zh_arrayClone( PZH_ITEM pArray )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayClone(%p)", ( void * ) pArray ) );

   return zh_arrayCloneTo( zh_itemNew( NULL ), pArray );
}

PZH_ITEM zh_arrayFromStack( ZH_USHORT uiLen )
{
   ZH_STACK_TLS_PRELOAD
   PZH_ITEM pArray = zh_itemNew( NULL );
   ZH_USHORT uiPos;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayFromStack(%hu)", uiLen ) );

   zh_arrayNew( pArray, uiLen );

   for( uiPos = 1; uiPos <= uiLen; uiPos++ )
      zh_arraySet( pArray, uiPos, zh_stackItemFromTop( uiPos - uiLen - 1 ) );

   return pArray;
}

PZH_ITEM zh_arrayFromParams( int iLevel )
{
   ZH_STACK_TLS_PRELOAD
   PZH_ITEM pArray;
   ZH_USHORT uiPos, uiPCount;
   ZH_ISIZ nBaseOffset;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayFromParams(%d)", iLevel ) );

   nBaseOffset = zh_stackBaseProcOffset( iLevel );
   if( nBaseOffset > 0 )
      uiPCount = zh_stackItem( nBaseOffset )->item.asSymbol.paramcnt;
   else
      uiPCount = 0;

   pArray = zh_itemArrayNew( uiPCount );
   for( uiPos = 1; uiPos <= uiPCount; uiPos++ )
      zh_arraySet( pArray, uiPos, zh_stackItem( nBaseOffset + uiPos + 1 ) );

   return pArray;
}

PZH_ITEM zh_arrayBaseParams( void )
{
   ZH_STACK_TLS_PRELOAD
   PZH_ITEM pArray;
   ZH_USHORT uiPos, uiPCount;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayBaseParams()" ) );

   pArray = zh_itemNew( NULL );
   uiPCount = zh_stackBaseItem()->item.asSymbol.paramcnt;

   zh_arrayNew( pArray, uiPCount );

   for( uiPos = 1; uiPos <= uiPCount; uiPos++ )
      zh_arraySet( pArray, uiPos, zh_stackItemFromBase( uiPos ) );

   return pArray;
}

PZH_ITEM zh_arraySelfParams( void )
{
   ZH_STACK_TLS_PRELOAD
   PZH_ITEM pArray;
   ZH_USHORT uiPos, uiPCount;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arraySelfParams()" ) );

   pArray = zh_itemNew( NULL );
   uiPCount = zh_stackBaseItem()->item.asSymbol.paramcnt;

   zh_arrayNew( pArray, uiPCount + 1 );

   for( uiPos = 0; uiPos <= uiPCount; uiPos++ )
      zh_arraySet( pArray, uiPos + 1, zh_stackItemFromBase( uiPos ) );

   return pArray;
}
