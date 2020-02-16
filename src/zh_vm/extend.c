/*
 * The Extend API
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * Copyright 1999-2009 Viktor Szakats (vszakats.net/ziher) (zh_stor(), zh_retn*len(), zh_retdl(), zh_parn*def())
 * Copyright 2000 Jose Lalin <dezac@corevia.com> (zh_retd())
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
#include "zh_set.h"
#include "zh_date.h"
#include "zh_stack.h"

/* NOTE: iParam = -1 can be used to access the return value. */
/* NOTE: iParam = 0 can be used to access the SELF object. */

PZH_ITEM zh_param( int iParam, long lMask )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_param(%d, %ld)", iParam, lMask ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam  );

      if( pItem->type & ZH_IT_BYREF )
      {
         pItem = zh_itemUnRef( pItem );
         if( ( ZH_TYPE ) lMask == ZH_IT_BYREF )
            return pItem;
      }

      if( ( pItem->type & ( ZH_TYPE ) lMask ) || ( ZH_TYPE ) lMask == ZH_IT_ANY )
         return pItem;
   }

   return NULL;
}

PZH_ITEM zh_paramError( int iParam )
{
   static ZH_ITEM s_NIL;

   PZH_ITEM pParam = zh_param( iParam, ZH_IT_ANY );

   if( pParam == NULL )
   {
      zh_itemClear( &s_NIL );
      pParam = &s_NIL;
   }

   return pParam;
}

ZH_ULONG zh_parinfo( int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parinfo(%d)", iParam ) );

   if( iParam == 0 )
      return ( ZH_ULONG ) zh_pcount();
   else
   {
      if( iParam >= -1 && iParam <= zh_pcount() )
      {
         PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );
         ZH_TYPE uiType = ZH_ITEM_TYPE( pItem );

         if( uiType & ZH_IT_BYREF )
            uiType |= ZH_ITEM_TYPE( zh_itemUnRef( pItem ) );

         return ( ZH_ULONG ) uiType;
      }
      else
         return 0;
   }
}

ZH_SIZE zh_parinfa( int iParamNum, ZH_SIZE nArrayIndex )
{
   PZH_ITEM pArray;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parinfa(%d, %" ZH_PFS "u)", iParamNum, nArrayIndex ) );

   pArray = zh_param( iParamNum, ZH_IT_ARRAY );

   if( pArray )
   {
      if( nArrayIndex == 0 )
         return zh_arrayLen( pArray );
      else
         return ( ZH_ISIZ ) zh_arrayGetType( pArray, nArrayIndex );
   }
   else
      return 0;
}

ZH_BOOL zh_extIsNil( int iParam )
{
   ZH_STACK_TLS_PRELOAD
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_extIsNil(%d)", iParam ) );

   if( iParam == -1 )
      pItem = zh_stackReturnItem();
   else if( iParam >= 0 && iParam <= zh_pcount() )
      pItem = zh_stackItemFromBase( iParam );
   else
      return ZH_TRUE;

   if( ZH_IS_BYREF( pItem ) )
      pItem = zh_itemUnRef( pItem );

   return ZH_IS_NIL( pItem );
}


/* function to be called from pcode DLLs to detect if the extend system
 * is going to use an array item */

ZH_BOOL zh_extIsArray( int iParam )
{
   ZH_STACK_TLS_PRELOAD
   PZH_ITEM pItem;

   if( iParam == -1 )
      pItem = zh_stackReturnItem();
   else if( iParam >= 0 && iParam <= zh_pcount() )
      pItem = zh_stackItemFromBase( iParam );
   else
      return ZH_FALSE;

   if( ZH_IS_BYREF( pItem ) )
      pItem = zh_itemUnRef( pItem );

   return ZH_IS_ARRAY( pItem ) && ! ZH_ARRAY_OBJ( pItem );
}

/* function to be called from pcode DLLs to detect if the extend system
 * is going to use an object item */

ZH_BOOL zh_extIsObject( int iParam )
{
   ZH_STACK_TLS_PRELOAD
   PZH_ITEM pItem;

   if( iParam == -1 )
      pItem = zh_stackReturnItem();
   else if( iParam >= 0 && iParam <= zh_pcount() )
      pItem = zh_stackItemFromBase( iParam );
   else
      return ZH_FALSE;

   if( ZH_IS_BYREF( pItem ) )
      pItem = zh_itemUnRef( pItem );

   return ZH_IS_OBJECT( pItem );
}

/* NOTE: Caller should not modify the buffer returned by this function.
         [vszakats] */

const char * zh_parc( int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parc(%d)", iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_STRING( pItem ) )
         return pItem->item.asString.value;
   }

   return NULL;
}

const char * zh_parcx( int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parcx(%d)", iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_STRING( pItem ) )
         return pItem->item.asString.value;
   }

   return "";
}

ZH_SIZE zh_parclen( int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parclen(%d)", iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_STRING( pItem ) )
         return pItem->item.asString.length;
   }

   return 0;
}

/* NOTE: Similar to _parclen() but returns the length including the
         terminating zero byte, and it only works for parameters passed by
         reference. [vszakats] */

ZH_SIZE zh_parcsiz( int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parcsiz(%d)", iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      /* NOTE: zh_parcsiz() will only work for strings passed by reference.
               CA-Cl*pper works like this. [vszakats] */

      if( ZH_IS_BYREF( pItem ) )
      {
         pItem = zh_itemUnRef( pItem );

         if( ZH_IS_STRING( pItem ) )
            return pItem->item.asString.length + 1;
      }
   }

   return 0;
}

/* NOTE: Using zh_stackDateBuffer() a temporary date buffer guaranties
         good behavior when multithreading. */

const char * zh_pards( int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_pards(%d)", iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_DATETIME( pItem ) )
         return zh_dateDecStr( zh_stackDateBuffer(), pItem->item.asDateTime.julian );
   }

   return zh_dateDecStr( zh_stackDateBuffer(), 0 );
}

/* NOTE: szDate must be a 9 chars wide buffer. [vszakats] */

char * zh_pardsbuff( char * szDate, int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_pardsbuff(%p, %d)", ( void * ) szDate, iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_DATETIME( pItem ) )
         return zh_dateDecStr( szDate, pItem->item.asDateTime.julian );
   }

   return zh_dateDecStr( szDate, 0 );
}

/* retrieve a date as long integer - number of days from Julian's day */

long zh_pardl( int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_pardl(%d)", iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_DATETIME( pItem ) )
         return pItem->item.asDateTime.julian;
   }

   return zh_itemGetDL( NULL );
}

double zh_partd( int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_partd(%d)", iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_DATETIME( pItem ) )
         return zh_timeStampPackDT( pItem->item.asDateTime.julian,
                                    pItem->item.asDateTime.time );
   }

   return 0;
}

ZH_BOOL zh_partdt( long * plJulian, long * plMilliSec, int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_partdt(%p,%p,%d)", ( void * ) plJulian, ( void * ) plMilliSec, iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_DATETIME( pItem ) )
      {
         *plJulian = pItem->item.asDateTime.julian;
         *plMilliSec = pItem->item.asDateTime.time;
         return ZH_TRUE;
      }
   }

   return ZH_FALSE;
}


int  zh_parl( int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parl(%d)", iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_LOGICAL( pItem ) )
         return pItem->item.asLogical.value ? 1 : 0;
   }

   return 0;
}

int  zh_parldef( int iParam, int iDefValue )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parldef(%d,%d)", iParam, iDefValue ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_LOGICAL( pItem ) )
         return pItem->item.asLogical.value ? 1 : 0;
   }

   return iDefValue;
}

double  zh_parnd( int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parnd(%d)", iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_DOUBLE( pItem ) )
         return pItem->item.asDouble.value;
      else if( ZH_IS_INTEGER( pItem ) )
         return ( double ) pItem->item.asInteger.value;
      else if( ZH_IS_LONG( pItem ) )
         return ( double ) pItem->item.asLong.value;
   }

   return 0;
}

int  zh_parni( int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parni(%d)", iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_INTEGER( pItem ) )
         return pItem->item.asInteger.value;
      else if( ZH_IS_LONG( pItem ) )
         return ( int ) pItem->item.asLong.value;
      else if( ZH_IS_DOUBLE( pItem ) )
         return ZH_CAST_INT( pItem->item.asDouble.value );
   }

   return 0;
}

int  zh_parnidef( int iParam, int iDefValue )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parni(%d, %d)", iParam, iDefValue ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_INTEGER( pItem ) )
         return pItem->item.asInteger.value;
      else if( ZH_IS_LONG( pItem ) )
         return ( int ) pItem->item.asLong.value;
      else if( ZH_IS_DOUBLE( pItem ) )
         return ZH_CAST_INT( pItem->item.asDouble.value );
   }

   return iDefValue;
}

long  zh_parnl( int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parnl(%d)", iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_LONG( pItem ) )
         return ( long ) pItem->item.asLong.value;
      else if( ZH_IS_INTEGER( pItem ) )
         return ( long ) pItem->item.asInteger.value;
      else if( ZH_IS_DOUBLE( pItem ) )
         return ZH_CAST_LONG( pItem->item.asDouble.value );
   }

   return 0;
}

long  zh_parnldef( int iParam, long lDefValue )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parnldef(%d, %ld)", iParam, lDefValue ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_LONG( pItem ) )
         return ( long ) pItem->item.asLong.value;
      else if( ZH_IS_INTEGER( pItem ) )
         return ( long ) pItem->item.asInteger.value;
      else if( ZH_IS_DOUBLE( pItem ) )
         return ZH_CAST_LONG( pItem->item.asDouble.value );
   }

   return lDefValue;
}

ZH_ISIZ zh_parns( int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parns(%d)", iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_LONG( pItem ) )
         return ( ZH_ISIZ ) pItem->item.asLong.value;
      else if( ZH_IS_INTEGER( pItem ) )
         return ( ZH_ISIZ ) pItem->item.asInteger.value;
      else if( ZH_IS_DOUBLE( pItem ) )
         return ZH_CAST_ISIZ( pItem->item.asDouble.value );
   }

   return 0;
}

ZH_ISIZ zh_parnsdef( int iParam, ZH_ISIZ nDefValue )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parnsdef(%d, %" ZH_PFS "d)", iParam, nDefValue ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_LONG( pItem ) )
         return ( ZH_ISIZ ) pItem->item.asLong.value;
      else if( ZH_IS_INTEGER( pItem ) )
         return ( ZH_ISIZ ) pItem->item.asInteger.value;
      else if( ZH_IS_DOUBLE( pItem ) )
         return ZH_CAST_ISIZ( pItem->item.asDouble.value );
   }

   return nDefValue;
}

#ifndef ZH_LONG_LONG_OFF
ZH_LONGLONG  zh_parnll( int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parnll(%d)", iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_LONG( pItem ) )
         return ( ZH_LONGLONG ) pItem->item.asLong.value;
      else if( ZH_IS_INTEGER( pItem ) )
         return ( ZH_LONGLONG ) pItem->item.asInteger.value;
      else if( ZH_IS_DOUBLE( pItem ) )
         return ZH_CAST_LONGLONG( pItem->item.asDouble.value );
   }

   return 0;
}
#endif

ZH_MAXINT zh_parnint( int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parnint(%d)", iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_LONG( pItem ) )
         return ( ZH_MAXINT ) pItem->item.asLong.value;
      else if( ZH_IS_INTEGER( pItem ) )
         return ( ZH_MAXINT ) pItem->item.asInteger.value;
      else if( ZH_IS_DOUBLE( pItem ) )
         return ZH_CAST_MAXINT( pItem->item.asDouble.value );
   }

   return 0;
}

ZH_MAXINT zh_parnintdef( int iParam, ZH_MAXINT nDefValue )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parnintdef(%d, %" PFHL "d)", iParam, nDefValue ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_LONG( pItem ) )
         return ( ZH_MAXINT ) pItem->item.asLong.value;
      else if( ZH_IS_INTEGER( pItem ) )
         return ( ZH_MAXINT ) pItem->item.asInteger.value;
      else if( ZH_IS_DOUBLE( pItem ) )
         return ZH_CAST_MAXINT( pItem->item.asDouble.value );
   }

   return nDefValue;
}

void * zh_parptr( int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parptr(%d)", iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_POINTER( pItem ) )
         return pItem->item.asPointer.value;
   }

   return NULL;
}

void * zh_parptrGC( const ZH_GC_FUNCS * pFuncs, int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parptrGC(%p,%d)", ( const void * ) pFuncs, iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_POINTER( pItem ) && pItem->item.asPointer.collect &&
          zh_gcFuncs( pItem->item.asPointer.value ) == pFuncs )
         return pItem->item.asPointer.value;
   }

   return NULL;
}

/* NOTE: Caller should not modify the buffer returned by this function.
         [vszakats] */

const char * zh_parvc( int iParam, ... )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parvc(%d, ...)", iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_STRING( pItem ) )
         return pItem->item.asString.value;
      else if( ZH_IS_ARRAY( pItem ) )
      {
         va_list va;
         ZH_SIZE nArrayIndex;

         va_start( va, iParam );
         nArrayIndex = va_arg( va, ZH_SIZE );
         va_end( va );

         pItem = zh_arrayGetItemPtr( pItem, nArrayIndex );
         return pItem && ZH_IS_STRING( pItem ) ? zh_itemGetCPtr( pItem ) : NULL;
      }
   }

   return NULL;
}

const char * zh_parvcx( int iParam, ... )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parvcx(%d, ...)", iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_STRING( pItem ) )
         return pItem->item.asString.value;
      else if( ZH_IS_ARRAY( pItem ) )
      {
         va_list va;
         ZH_SIZE nArrayIndex;

         va_start( va, iParam );
         nArrayIndex = va_arg( va, ZH_SIZE );
         va_end( va );

         return zh_arrayGetCPtr( pItem, nArrayIndex );
      }
   }

   return "";
}

ZH_SIZE zh_parvclen( int iParam, ... )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parvclen(%d, ...)", iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_STRING( pItem ) )
         return pItem->item.asString.length;
      else if( ZH_IS_ARRAY( pItem ) )
      {
         va_list va;
         ZH_SIZE nArrayIndex;

         va_start( va, iParam );
         nArrayIndex = va_arg( va, ZH_SIZE );
         va_end( va );

         return zh_arrayGetCLen( pItem, nArrayIndex );
      }
   }

   return 0;
}

/* NOTE: Similar to _parclen() but returns the length including the
         terminating zero byte, and it only works for parameters passed by
         reference. [vszakats] */

ZH_SIZE zh_parvcsiz( int iParam, ... )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parvcsiz(%d, ...)", iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      /* NOTE: zh_parvcsiz() will only work for strings passed by reference.
               CA-Cl*pper works like this. [vszakats] */

      if( ZH_IS_BYREF( pItem ) )
      {
         pItem = zh_itemUnRef( pItem );

         if( ZH_IS_STRING( pItem ) )
            return pItem->item.asString.length + 1;
         else if( ZH_IS_ARRAY( pItem ) )
         {
            va_list va;
            ZH_SIZE nArrayIndex;

            va_start( va, iParam );
            nArrayIndex = va_arg( va, ZH_SIZE );
            va_end( va );

            return zh_arrayGetCLen( pItem, nArrayIndex ) + 1;
         }
      }
   }

   return 0;
}

/* NOTE: Using zh_stackDateBuffer() a temporary date buffer guaranties
         good behavior when multithreading. */

const char * zh_parvds( int iParam, ... )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parvds(%d, ...)", iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_DATETIME( pItem ) )
         return zh_dateDecStr( zh_stackDateBuffer(), pItem->item.asDateTime.julian );
      else if( ZH_IS_ARRAY( pItem ) )
      {
         va_list va;
         ZH_SIZE nArrayIndex;

         va_start( va, iParam );
         nArrayIndex = va_arg( va, ZH_SIZE );
         va_end( va );

         return zh_arrayGetDS( pItem, nArrayIndex, zh_stackDateBuffer() );
      }
   }

   return zh_dateDecStr( zh_stackDateBuffer(), 0 );
}

/* NOTE: szDate must be a 9 chars wide buffer. [vszakats] */

char  * zh_parvdsbuff( char * szDate, int iParam, ... )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parvdsbuff(%p, %d, ...)", ( void * ) szDate, iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_DATETIME( pItem ) )
         return zh_dateDecStr( szDate, pItem->item.asDateTime.julian );
      else if( ZH_IS_ARRAY( pItem ) )
      {
         va_list va;
         ZH_SIZE nArrayIndex;

         va_start( va, iParam );
         nArrayIndex = va_arg( va, ZH_SIZE );
         va_end( va );

         return zh_arrayGetDS( pItem, nArrayIndex, szDate );
      }
   }

   return zh_dateDecStr( szDate, 0 );
}

/* retrieve a date as long integer - number of days from Julian's day */

long zh_parvdl( int iParam, ... )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parvdl(%d, ...)", iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_DATETIME( pItem ) )
         return pItem->item.asDateTime.julian;
      else if( ZH_IS_ARRAY( pItem ) )
      {
         va_list va;
         ZH_SIZE nArrayIndex;

         va_start( va, iParam );
         nArrayIndex = va_arg( va, ZH_SIZE );
         va_end( va );

         return zh_arrayGetDL( pItem, nArrayIndex );
      }
   }

   return zh_itemGetDL( NULL );
}

double zh_parvtd( int iParam, ... )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parvtd(%d, ...)", iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_DATETIME( pItem ) )
         return zh_timeStampPackDT( pItem->item.asDateTime.julian,
                                    pItem->item.asDateTime.time );
      else if( ZH_IS_ARRAY( pItem ) )
      {
         va_list va;
         ZH_SIZE nArrayIndex;

         va_start( va, iParam );
         nArrayIndex = va_arg( va, ZH_SIZE );
         va_end( va );

         return zh_arrayGetTD( pItem, nArrayIndex );
      }
   }

   return 0;
}

ZH_BOOL zh_parvtdt( long * plJulian, long * plMilliSec, int iParam, ... )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parvtdt(%p,%p,%d, ...)", ( void * ) plJulian, ( void * ) plMilliSec, iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_DATETIME( pItem ) )
      {
         *plJulian = pItem->item.asDateTime.julian;
         *plMilliSec = pItem->item.asDateTime.time;
         return ZH_TRUE;
      }
      else if( ZH_IS_ARRAY( pItem ) )
      {
         va_list va;
         ZH_SIZE nArrayIndex;

         va_start( va, iParam );
         nArrayIndex = va_arg( va, ZH_SIZE );
         va_end( va );

         return zh_arrayGetTDT( pItem, nArrayIndex, plJulian, plMilliSec );
      }
   }

   return ZH_FALSE;
}


int  zh_parvl( int iParam, ... )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parvl(%d, ...)", iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_LOGICAL( pItem ) )
         return pItem->item.asLogical.value ? 1 : 0;
      else if( ZH_IS_INTEGER( pItem ) )
         return pItem->item.asInteger.value != 0 ? 1 : 0;
      else if( ZH_IS_LONG( pItem ) )
         return pItem->item.asLong.value != 0 ? 1 : 0;
      else if( ZH_IS_DOUBLE( pItem ) )
         return pItem->item.asDouble.value != 0.0 ? 1 : 0;
      else if( ZH_IS_ARRAY( pItem ) )
      {
         va_list va;
         ZH_SIZE nArrayIndex;

         va_start( va, iParam );
         nArrayIndex = va_arg( va, ZH_SIZE );
         va_end( va );

         return zh_arrayGetL( pItem, nArrayIndex ) ? 1 : 0;
      }
   }

   return 0;
}

double  zh_parvnd( int iParam, ... )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parvnd(%d, ...)", iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_DOUBLE( pItem ) )
         return pItem->item.asDouble.value;
      else if( ZH_IS_INTEGER( pItem ) )
         return ( double ) pItem->item.asInteger.value;
      else if( ZH_IS_LONG( pItem ) )
         return ( double ) pItem->item.asLong.value;
      else if( ZH_IS_ARRAY( pItem ) )
      {
         va_list va;
         ZH_SIZE nArrayIndex;

         va_start( va, iParam );
         nArrayIndex = va_arg( va, ZH_SIZE );
         va_end( va );

         return zh_arrayGetND( pItem, nArrayIndex );
      }
   }

   return 0;
}

int  zh_parvni( int iParam, ... )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parvni(%d, ...)", iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_INTEGER( pItem ) )
         return pItem->item.asInteger.value;
      else if( ZH_IS_LONG( pItem ) )
         return ( int ) pItem->item.asLong.value;
      else if( ZH_IS_DOUBLE( pItem ) )
         return ZH_CAST_INT( pItem->item.asDouble.value );
      else if( ZH_IS_ARRAY( pItem ) )
      {
         va_list va;
         ZH_SIZE nArrayIndex;

         va_start( va, iParam );
         nArrayIndex = va_arg( va, ZH_SIZE );
         va_end( va );

         return zh_arrayGetNI( pItem, nArrayIndex );
      }
   }

   return 0;
}

long  zh_parvnl( int iParam, ... )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parvnl(%d, ...)", iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_LONG( pItem ) )
         return ( long ) pItem->item.asLong.value;
      else if( ZH_IS_INTEGER( pItem ) )
         return ( long ) pItem->item.asInteger.value;
      else if( ZH_IS_DOUBLE( pItem ) )
         return ZH_CAST_LONG( pItem->item.asDouble.value );
      /* CA-Cl*pper does it */
      else if( ZH_IS_DATETIME( pItem ) )
         return ( long ) pItem->item.asDateTime.julian;
      else if( ZH_IS_ARRAY( pItem ) )
      {
         va_list va;
         ZH_SIZE nArrayIndex;

         va_start( va, iParam );
         nArrayIndex = va_arg( va, ZH_SIZE );
         va_end( va );

         return zh_arrayGetNL( pItem, nArrayIndex );
      }
   }

   return 0;
}

ZH_ISIZ zh_parvns( int iParam, ... )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parvns(%d, ...)", iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_LONG( pItem ) )
         return ( ZH_ISIZ ) pItem->item.asLong.value;
      else if( ZH_IS_INTEGER( pItem ) )
         return ( ZH_ISIZ ) pItem->item.asInteger.value;
      else if( ZH_IS_DOUBLE( pItem ) )
         return ZH_CAST_ISIZ( pItem->item.asDouble.value );
      else if( ZH_IS_ARRAY( pItem ) )
      {
         va_list va;
         ZH_SIZE nArrayIndex;

         va_start( va, iParam );
         nArrayIndex = va_arg( va, ZH_SIZE );
         va_end( va );

         return zh_arrayGetNS( pItem, nArrayIndex );
      }
   }

   return 0;
}

#ifndef ZH_LONG_LONG_OFF
ZH_LONGLONG zh_parvnll( int iParam, ... )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parvnll(%d, ...)", iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_LONG( pItem ) )
         return ( ZH_LONGLONG ) pItem->item.asLong.value;
      else if( ZH_IS_INTEGER( pItem ) )
         return ( ZH_LONGLONG ) pItem->item.asInteger.value;
      else if( ZH_IS_DOUBLE( pItem ) )
         return ZH_CAST_LONGLONG( pItem->item.asDouble.value );
      else if( ZH_IS_ARRAY( pItem ) )
      {
         va_list va;
         ZH_SIZE nArrayIndex;

         va_start( va, iParam );
         nArrayIndex = va_arg( va, ZH_SIZE );
         va_end( va );

         return zh_arrayGetNLL( pItem, nArrayIndex );
      }
   }

   return 0;
}
#endif

ZH_MAXINT zh_parvnint( int iParam, ... )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parvnint(%d, ...)", iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_LONG( pItem ) )
         return ( ZH_MAXINT ) pItem->item.asLong.value;
      else if( ZH_IS_INTEGER( pItem ) )
         return ( ZH_MAXINT ) pItem->item.asInteger.value;
      else if( ZH_IS_DOUBLE( pItem ) )
         return ZH_CAST_MAXINT( pItem->item.asDouble.value );
      else if( ZH_IS_ARRAY( pItem ) )
      {
         va_list va;
         ZH_SIZE nArrayIndex;

         va_start( va, iParam );
         nArrayIndex = va_arg( va, ZH_SIZE );
         va_end( va );

         return zh_arrayGetNInt( pItem, nArrayIndex );
      }
   }

   return 0;
}

void * zh_parvptr( int iParam, ... )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parvptr(%d, ...)", iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_POINTER( pItem ) )
         return pItem->item.asPointer.value;
      else if( ZH_IS_ARRAY( pItem ) )
      {
         va_list va;
         ZH_SIZE nArrayIndex;

         va_start( va, iParam );
         nArrayIndex = va_arg( va, ZH_SIZE );
         va_end( va );

         return zh_arrayGetPtr( pItem, nArrayIndex );
      }
   }

   return NULL;
}

void * zh_parvptrGC( const ZH_GC_FUNCS * pFuncs, int iParam, ... )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parvptrGC(%p,%d, ...)", ( const void * ) pFuncs, iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_POINTER( pItem ) )
      {
         if( pItem->item.asPointer.collect &&
             zh_gcFuncs( pItem->item.asPointer.value ) == pFuncs )
            return pItem->item.asPointer.value;
      }
      else if( ZH_IS_ARRAY( pItem ) )
      {
         va_list va;
         ZH_SIZE nArrayIndex;

         va_start( va, iParam );
         nArrayIndex = va_arg( va, ZH_SIZE );
         va_end( va );

         pItem = zh_arrayGetItemPtr( pItem, nArrayIndex );
         if( pItem && ZH_IS_POINTER( pItem ) &&
             pItem->item.asPointer.collect &&
             zh_gcFuncs( pItem->item.asPointer.value ) == pFuncs )
            return pItem->item.asPointer.value;
      }
   }

   return NULL;
}

#undef zh_ret
void zh_ret( void )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_ret()" ) );

   zh_itemClear( zh_stackReturnItem() );
}

#undef zh_reta
void zh_reta( ZH_SIZE nLen )  /* undocumented zh_reta() */
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_reta(%" ZH_PFS "u)", nLen ) );

   zh_arrayNew( zh_stackReturnItem(), nLen );
}

#undef zh_retc
void zh_retc( const char * szText )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_retc(%s)", szText ) );

   zh_itemPutC( zh_stackReturnItem(), szText );
}

#undef zh_retc_null
void zh_retc_null( void )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_retc_null()" ) );

   zh_itemPutC( zh_stackReturnItem(), NULL );
}

#undef zh_retc_buffer
void zh_retc_buffer( char * szText )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_retc_buffer(%s)", szText ) );

   zh_itemPutCPtr( zh_stackReturnItem(), szText );
}

#undef zh_retc_const
void zh_retc_const( const char * szText )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_retc_const(%s)", szText ) );

   zh_itemPutCConst( zh_stackReturnItem(), szText );
}

#undef zh_retclen
void zh_retclen( const char * szText, ZH_SIZE nLen )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_retclen(%.*s, %" ZH_PFS "u)", ( int ) nLen, szText, nLen ) );

   zh_itemPutCL( zh_stackReturnItem(), szText, nLen );
}

#undef zh_retclen_buffer
void zh_retclen_buffer( char * szText, ZH_SIZE nLen )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_retclen_buffer(%.*s, %" ZH_PFS "u)", ( int ) nLen, szText, nLen ) );

   zh_itemPutCLPtr( zh_stackReturnItem(), szText, nLen );
}

#undef zh_retclen_const
void zh_retclen_const( const char * szText, ZH_SIZE nLen )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_retclen_const(%.*s, %" ZH_PFS "u)", ( int ) nLen, szText, nLen ) );

   zh_itemPutCLConst( zh_stackReturnItem(), szText, nLen );
}

/* szDate must have YYYYMMDD format */

#undef zh_retds
void zh_retds( const char * szDate )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_retds(%s)", szDate ) );

   zh_itemPutDS( zh_stackReturnItem(), szDate );
}

#undef zh_retd
void zh_retd( int iYear, int iMonth, int iDay )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_retd(%04i, %02i, %02i)", iYear, iMonth, iDay ) );

   zh_itemPutD( zh_stackReturnItem(), iYear, iMonth, iDay );
}

#undef zh_retdl
void zh_retdl( long lJulian )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_retdl(%ld)", lJulian ) );

   zh_itemPutDL( zh_stackReturnItem(), lJulian );
}

#undef zh_rettd
void zh_rettd( double dTimeStamp )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_rettd(%lf)", dTimeStamp ) );

   zh_itemPutTD( zh_stackReturnItem(), dTimeStamp );
}

#undef zh_rettdt
void zh_rettdt( long lJulian, long lMilliSec )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_rettdt(%ld, %ld)", lJulian, lMilliSec ) );

   zh_itemPutTDT( zh_stackReturnItem(), lJulian, lMilliSec );
}

#undef zh_retl
void zh_retl( int iLogical )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_retl(%d)", iLogical ) );

   zh_itemPutL( zh_stackReturnItem(), iLogical ? ZH_TRUE : ZH_FALSE );
}

#undef zh_retnd
void zh_retnd( double dNumber )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_retnd(%lf)", dNumber ) );

   zh_itemPutND( zh_stackReturnItem(), dNumber );
}

#undef zh_retni
void zh_retni( int iNumber )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_retni(%d)", iNumber ) );

   zh_itemPutNI( zh_stackReturnItem(), iNumber );
}

#undef zh_retnl
void zh_retnl( long lNumber )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_retnl(%ld)", lNumber ) );

   zh_itemPutNL( zh_stackReturnItem(), lNumber );
}

#undef zh_retns
void zh_retns( ZH_ISIZ nNumber )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_retns(%" ZH_PFS "d )", nNumber ) );

   zh_itemPutNS( zh_stackReturnItem(), nNumber );
}

#ifndef ZH_LONG_LONG_OFF
#undef zh_retnll
void zh_retnll( ZH_LONGLONG llNumber )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_retnll(%" PFLL "d)", llNumber ) );

   zh_itemPutNLL( zh_stackReturnItem(), llNumber );
}
#endif

#undef zh_retnint
void zh_retnint( ZH_MAXINT nNumber )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_retnl(%" PFHL "d )", nNumber ) );

   zh_itemPutNInt( zh_stackReturnItem(), nNumber );
}

#undef zh_retnlen
void zh_retnlen( double dNumber, int iWidth, int iDec )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_retnlen(%lf, %d, %d)", dNumber, iWidth, iDec ) );

   zh_itemPutNLen( zh_stackReturnItem(), dNumber, iWidth, iDec );
}

#undef zh_retndlen
void zh_retndlen( double dNumber, int iWidth, int iDec )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_retndlen(%lf, %d, %d)", dNumber, iWidth, iDec ) );

   zh_itemPutNDLen( zh_stackReturnItem(), dNumber, iWidth, iDec );
}

#undef zh_retnilen
void zh_retnilen( int iNumber, int iWidth )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_retnilen(%d, %d)", iNumber, iWidth ) );

   zh_itemPutNILen( zh_stackReturnItem(), iNumber, iWidth );
}

#undef zh_retnllen
void zh_retnllen( long lNumber, int iWidth )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_retnllen(%ld, %d)", lNumber, iWidth ) );

   zh_itemPutNLLen( zh_stackReturnItem(), lNumber, iWidth );
}

#ifndef ZH_LONG_LONG_OFF
#undef zh_retnlllen
void zh_retnlllen( ZH_LONGLONG llNumber, int iWidth )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_retnlllen(%" PFLL "d, %d)", llNumber, iWidth ) );

   zh_itemPutNLLLen( zh_stackReturnItem(), llNumber, iWidth );
}
#endif

#undef zh_retnintlen
void zh_retnintlen( ZH_MAXINT nNumber, int iWidth )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_retnintlen(%" PFHL "d, %d)", nNumber, iWidth ) );

   zh_itemPutNIntLen( zh_stackReturnItem(), nNumber, iWidth );
}

#undef zh_retptr
void zh_retptr( void * pointer )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_retptr(%p)", pointer ) );

   zh_itemPutPtr( zh_stackReturnItem(), pointer );
}

#undef zh_retptrGC
void zh_retptrGC( void * pointer )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_retptrGC(%p)", pointer ) );

   zh_itemPutPtrGC( zh_stackReturnItem(), pointer );
}

int zh_stor( int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stor(%d)", iParam ) );

   if( iParam == -1 )
   {
      zh_itemClear( zh_stackReturnItem() );
      return 1;
   }
   else if( iParam >= 0 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
      {
         zh_itemClear( zh_itemUnRef( pItem ) );
         return 1;
      }
   }

   return 0;
}

int zh_storc( const char * szText, int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_storc(%s, %d)", szText, iParam ) );

   if( iParam == -1 )
   {
      zh_itemPutC( zh_stackReturnItem(), szText );
      return 1;
   }
   else if( iParam >= 0 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
      {
         zh_itemPutC( zh_itemUnRef( pItem ), szText );
         return 1;
      }
   }

   return 0;
}

int zh_storclen( const char * szText, ZH_SIZE nLen, int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_storclen(%.*s, %" ZH_PFS "u, %d)", ( int ) nLen, szText, nLen, iParam ) );

   if( iParam == -1 )
   {
      zh_itemPutCL( zh_stackReturnItem(), szText, nLen );
      return 1;
   }
   else if( iParam >= 0 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
      {
         zh_itemPutCL( zh_itemUnRef( pItem ), szText, nLen );
         return 1;
      }
   }

   return 0;
}

int zh_storclen_buffer( char * szText, ZH_SIZE nLen, int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_storclen_buffer(%.*s, %" ZH_PFS "u, %d)", ( int ) nLen, szText, nLen, iParam ) );

   if( iParam == -1 )
   {
      zh_itemPutCLPtr( zh_stackReturnItem(), szText, nLen );
      return 1;
   }
   else if( iParam >= 0 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
      {
         zh_itemPutCLPtr( zh_itemUnRef( pItem ), szText, nLen );
         return 1;
      }
   }

   return 0;
}

/* szDate must have YYYYMMDD format */

int zh_stords( const char * szDate, int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stords(%s, %d)", szDate, iParam ) );

   if( iParam == -1 )
   {
      zh_itemPutDS( zh_stackReturnItem(), szDate );
      return 1;
   }
   else if( iParam >= 0 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
      {
         zh_itemPutDS( zh_itemUnRef( pItem ), szDate );
         return 1;
      }
   }

   return 0;
}

int zh_stordl( long lJulian, int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stordl(%ld, %d)", lJulian, iParam ) );

   if( iParam == -1 )
   {
      zh_itemPutDL( zh_stackReturnItem(), lJulian );
      return 1;
   }
   else if( iParam >= 0 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
      {
         zh_itemPutDL( zh_itemUnRef( pItem ), lJulian );
         return 1;
      }
   }

   return 0;
}

int zh_stortd( double dTimeStamp, int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stortd(%lf, %d)", dTimeStamp, iParam ) );

   if( iParam == -1 )
   {
      zh_itemPutTD( zh_stackReturnItem(), dTimeStamp );
      return 1;
   }
   else if( iParam >= 0 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
      {
         zh_itemPutTD( zh_itemUnRef( pItem ), dTimeStamp );
         return 1;
      }
   }

   return 0;
}

int zh_stortdt( long lJulian, long lMilliSec, int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stortd(%ld, %ld, %d)", lJulian, lMilliSec, iParam ) );

   if( iParam == -1 )
   {
      zh_itemPutTDT( zh_stackReturnItem(), lJulian, lMilliSec );
      return 1;
   }
   else if( iParam >= 0 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
      {
         zh_itemPutTDT( zh_itemUnRef( pItem ), lJulian, lMilliSec );
         return 1;
      }
   }

   return 0;
}

int zh_storl( int iLogical, int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_storl(%d, %d)", iLogical, iParam ) );

   if( iParam == -1 )
   {
      zh_itemPutL( zh_stackReturnItem(), iLogical ? ZH_TRUE : ZH_FALSE );
      return 1;
   }
   else if( iParam >= 0 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
      {
         zh_itemPutL( zh_itemUnRef( pItem ), iLogical ? ZH_TRUE : ZH_FALSE );
         return 1;
      }
   }

   return 0;
}

int zh_storni( int iValue, int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_storni(%d, %d)", iValue, iParam ) );

   if( iParam == -1 )
   {
      zh_itemPutNI( zh_stackReturnItem(), iValue );
      return 1;
   }
   else if( iParam >= 0 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
      {
         zh_itemPutNI( zh_itemUnRef( pItem ), iValue );
         return 1;
      }
   }

   return 0;
}

int zh_stornl( long lValue, int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stornl(%ld, %d)", lValue, iParam ) );

   if( iParam == -1 )
   {
      zh_itemPutNL( zh_stackReturnItem(), lValue );
      return 1;
   }
   else if( iParam >= 0 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
      {
         zh_itemPutNL( zh_itemUnRef( pItem ), lValue );
         return 1;
      }
   }

   return 0;
}

int zh_storns( ZH_ISIZ nValue, int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_storns(%" ZH_PFS "d, %d)", nValue, iParam ) );

   if( iParam == -1 )
   {
      zh_itemPutNS( zh_stackReturnItem(), nValue );
      return 1;
   }
   else if( iParam >= 0 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
      {
         zh_itemPutNS( zh_itemUnRef( pItem ), nValue );
         return 1;
      }
   }

   return 0;
}

#ifndef ZH_LONG_LONG_OFF
int zh_stornll( ZH_LONGLONG llValue, int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stornll(%" PFLL "d, %d)", llValue, iParam ) );

   if( iParam == -1 )
   {
      zh_itemPutNLL( zh_stackReturnItem(), llValue );
      return 1;
   }
   else if( iParam >= 0 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
      {
         zh_itemPutNLL( zh_itemUnRef( pItem ), llValue );
         return 1;
      }
   }

   return 0;
}
#endif

int zh_stornint( ZH_MAXINT nValue, int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stornint(%" PFHL "d, %d)", nValue, iParam ) );

   if( iParam == -1 )
   {
      zh_itemPutNInt( zh_stackReturnItem(), nValue );
      return 1;
   }
   else if( iParam >= 0 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
      {
         zh_itemPutNInt( zh_itemUnRef( pItem ), nValue );
         return 1;
      }
   }

   return 0;
}

int zh_stornd( double dNumber, int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_stornd(%lf, %d)", dNumber, iParam ) );

   if( iParam == -1 )
   {
      zh_itemPutND( zh_stackReturnItem(), dNumber );
      return 1;
   }
   else if( iParam >= 0 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
      {
         zh_itemPutND( zh_itemUnRef( pItem ), dNumber );
         return 1;
      }
   }

   return 0;
}

int zh_storptr( void * pointer, int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_storptr(%p, %d)", pointer, iParam ) );

   if( iParam == -1 )
   {
      zh_itemPutPtr( zh_stackReturnItem(), pointer );
      return 1;
   }
   else if( iParam >= 0 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
      {
         zh_itemPutPtr( zh_itemUnRef( pItem ), pointer );
         return 1;
      }
   }

   return 0;
}

int zh_storptrGC( void * pointer, int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_storptrGC(%p, %d)", pointer, iParam ) );

   if( iParam == -1 )
   {
      zh_itemPutPtrGC( zh_stackReturnItem(), pointer );
      return 1;
   }
   else if( iParam >= 0 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
      {
         zh_itemPutPtrGC( zh_itemUnRef( pItem ), pointer );
         return 1;
      }
   }

   return 0;
}

/* zh_storv*() similar to zh_stor*() but they accepts optional array index
 * just like Cl*pper's _stor*() functions
 */

int zh_storvc( const char * szText, int iParam, ... )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_storvc(%s, %d, ...)", szText, iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );
      ZH_BOOL bByRef = ZH_IS_BYREF( pItem );

      if( bByRef )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = zh_arraySetC( pItem, va_arg( va, ZH_SIZE ), szText ) ? 1 : 0;
         va_end( va );
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         zh_itemPutC( pItem, szText );
         return 1;
      }
   }

   return 0;
}

int zh_storvclen( const char * szText, ZH_SIZE nLen, int iParam, ... )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_storvclen(%.*s, %" ZH_PFS "u, %d, ...)", ( int ) nLen, szText, nLen, iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );
      ZH_BOOL bByRef = ZH_IS_BYREF( pItem );

      if( bByRef )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = zh_arraySetCL( pItem, va_arg( va, ZH_SIZE ), szText, nLen ) ? 1 : 0;
         va_end( va );
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         zh_itemPutCL( pItem, szText, nLen );
         return 1;
      }
   }

   return 0;
}

int zh_storvclen_buffer( char * szText, ZH_SIZE nLen, int iParam, ... )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_storvclen_buffer(%.*s, %" ZH_PFS "u, %d, ...)", ( int ) nLen, szText, nLen, iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );
      ZH_BOOL bByRef = ZH_IS_BYREF( pItem );

      if( bByRef )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = zh_arraySetCLPtr( pItem, va_arg( va, ZH_SIZE ), szText, nLen ) ? 1 : 0;
         va_end( va );
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         zh_itemPutCLPtr( pItem, szText, nLen );
         return 1;
      }
   }

   return 0;
}

/* szDate must have YYYYMMDD format */

int zh_storvds( const char * szDate, int iParam, ... )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_storvds(%s, %d, ...)", szDate, iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );
      ZH_BOOL bByRef = ZH_IS_BYREF( pItem );

      if( bByRef )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = zh_arraySetDS( pItem, va_arg( va, ZH_SIZE ), szDate ) ? 1 : 0;
         va_end( va );
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         zh_itemPutDS( pItem, szDate );
         return 1;
      }
   }

   return 0;
}

int zh_storvdl( long lJulian, int iParam, ... )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_storvdl(%ld, %d, ...)", lJulian, iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );
      ZH_BOOL bByRef = ZH_IS_BYREF( pItem );

      if( bByRef )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = zh_arraySetDL( pItem, va_arg( va, ZH_SIZE ), lJulian ) ? 1 : 0;
         va_end( va );
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         zh_itemPutDL( pItem, lJulian );
         return 1;
      }
   }

   return 0;
}

int zh_storvtd( double dTimeStamp, int iParam, ... )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_storvtd(%lf, %d, ...)", dTimeStamp, iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );
      ZH_BOOL bByRef = ZH_IS_BYREF( pItem );

      if( bByRef )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = zh_arraySetTD( pItem, va_arg( va, ZH_SIZE ), dTimeStamp ) ? 1 : 0;
         va_end( va );
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         zh_itemPutTD( pItem, dTimeStamp );
         return 1;
      }
   }

   return 0;
}

int zh_storvtdt( long lJulian, long lMilliSec, int iParam, ... )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_storvtd(%ld, %ld, %d, ...)", lJulian, lMilliSec, iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );
      ZH_BOOL bByRef = ZH_IS_BYREF( pItem );

      if( bByRef )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = zh_arraySetTDT( pItem, va_arg( va, ZH_SIZE ), lJulian, lMilliSec ) ? 1 : 0;
         va_end( va );
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         zh_itemPutTDT( pItem, lJulian, lMilliSec );
         return 1;
      }
   }

   return 0;
}

int zh_storvl( int iLogical, int iParam, ... )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_storvl(%d, %d, ...)", iLogical, iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );
      ZH_BOOL bByRef = ZH_IS_BYREF( pItem );

      if( bByRef )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = zh_arraySetL( pItem, va_arg( va, ZH_SIZE ), iLogical ? ZH_TRUE : ZH_FALSE ) ? 1 : 0;
         va_end( va );
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         zh_itemPutL( pItem, iLogical ? ZH_TRUE : ZH_FALSE );
         return 1;
      }
   }

   return 0;
}

int zh_storvni( int iValue, int iParam, ... )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_storvni(%d, %d, ...)", iValue, iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );
      ZH_BOOL bByRef = ZH_IS_BYREF( pItem );

      if( bByRef )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = zh_arraySetNI( pItem, va_arg( va, ZH_SIZE ), iValue ) ? 1 : 0;
         va_end( va );
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         zh_itemPutNI( pItem, iValue );
         return 1;
      }
   }

   return 0;
}

int zh_storvnl( long lValue, int iParam, ... )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_storvnl(%ld, %d, ...)", lValue, iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );
      ZH_BOOL bByRef = ZH_IS_BYREF( pItem );

      if( bByRef )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = zh_arraySetNL( pItem, va_arg( va, ZH_SIZE ), lValue ) ? 1 : 0;
         va_end( va );
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         zh_itemPutNL( pItem, lValue );
         return 1;
      }
   }

   return 0;
}

int zh_storvns( ZH_ISIZ nValue, int iParam, ... )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_storvns(%" ZH_PFS "d, %d, ...)", nValue, iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );
      ZH_BOOL bByRef = ZH_IS_BYREF( pItem );

      if( bByRef )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = zh_arraySetNS( pItem, va_arg( va, ZH_SIZE ), nValue ) ? 1 : 0;
         va_end( va );
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         zh_itemPutNS( pItem, nValue );
         return 1;
      }
   }

   return 0;
}

#ifndef ZH_LONG_LONG_OFF
int zh_storvnll( ZH_LONGLONG llValue, int iParam, ... )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_storvnll(%" PFLL "d, %d, ...)", llValue, iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );
      ZH_BOOL bByRef = ZH_IS_BYREF( pItem );

      if( bByRef )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = zh_arraySetNLL( pItem, va_arg( va, ZH_SIZE ), llValue ) ? 1 : 0;
         va_end( va );
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         zh_itemPutNLL( pItem, llValue );
         return 1;
      }
   }

   return 0;
}
#endif

int zh_storvnint( ZH_MAXINT nValue, int iParam, ... )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_storvnint(%" PFHL "d, %d, ...)", nValue, iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );
      ZH_BOOL bByRef = ZH_IS_BYREF( pItem );

      if( bByRef )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = zh_arraySetNInt( pItem, va_arg( va, ZH_SIZE ), nValue ) ? 1 : 0;
         va_end( va );
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         zh_itemPutNInt( pItem, nValue );
         return 1;
      }
   }

   return 0;
}

int zh_storvnd( double dNumber, int iParam, ... )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_storvnd(%lf, %d, ...)", dNumber, iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );
      ZH_BOOL bByRef = ZH_IS_BYREF( pItem );

      if( bByRef )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = zh_arraySetND( pItem, va_arg( va, ZH_SIZE ), dNumber ) ? 1 : 0;
         va_end( va );
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         zh_itemPutND( pItem, dNumber );
         return 1;
      }
   }

   return 0;
}

int zh_storvptr( void * pointer, int iParam, ... )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_storvptr(%p, %d, ...)", pointer, iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );
      ZH_BOOL bByRef = ZH_IS_BYREF( pItem );

      if( bByRef )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = zh_arraySetPtr( pItem, va_arg( va, ZH_SIZE ), pointer ) ? 1 : 0;
         va_end( va );
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         zh_itemPutPtr( pItem, pointer );
         return 1;
      }
   }

   return 0;
}

int zh_storvptrGC( void * pointer, int iParam, ... )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_storvptrGC(%p, %d, ...)", pointer, iParam ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );
      ZH_BOOL bByRef = ZH_IS_BYREF( pItem );

      if( bByRef )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = zh_arraySetPtrGC( pItem, va_arg( va, ZH_SIZE ), pointer ) ? 1 : 0;
         va_end( va );
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         zh_itemPutPtrGC( pItem, pointer );
         return 1;
      }
   }

   return 0;
}

#undef zh_pcount
int  zh_pcount( void )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_pcount()" ) );

   return ( int ) ( zh_stackBaseItem() )->item.asSymbol.paramcnt;
}
