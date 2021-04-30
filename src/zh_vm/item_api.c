/*
 * The Item API
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * Copyright 1999-2007 Viktor Szakats
 *    zh_itemPCount(), zh_itemParamPtr(), zh_itemReturnPtr()
 *    zh_itemPutDL(), zh_itemPutNI(), zh_itemGetDL(), zh_itemGetNI(),
 *    zh_itemGetCPtr(), zh_itemPutCLPtr(), zh_itemGetCLen(), zh_itemGetNLen()
 *    zh_itemPutCConst(), zh_itemPutCLConst()
 *    zh_itemPutNLen(), zh_itemPutNDLen(), zh_itemPutNILen(), zh_itemPutNLLen()
 *    zh_itemPutD(), zh_itemSetCMemo()
 * Copyright 1999 Eddie Runia <eddie@runia.com> (zh_itemStrCmp())
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com> (zh_itemStr(), zh_itemString(), zh_itemValToStr())
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
/* zh_float.h have to be included before other header files */
#include "zh_float.h"

#include "zh_vm.h"
#include "zh_stack.h"
#include "zh_class_api.h"
#include "zh_item_api.h"
#include "zh_lang_api.h"
#include "zh_error_api.h"
#include "zh_date.h"
#include "zh_set.h"
#include "zh_codepage_api.h"

PZH_ITEM zh_itemNew( PZH_ITEM pNull )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemNew(%p)", ( void * ) pNull ) );

   return zh_gcGripGet( pNull );
}

PZH_ITEM zh_itemParam( ZH_USHORT uiParam )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemParam(%hu)", uiParam ) );

   return zh_itemNew( zh_param( uiParam, ZH_IT_ANY ) );
}

/* Internal Item API. Use this with care. */

PZH_ITEM zh_itemParamPtr( ZH_USHORT uiParam, long lMask )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemParamPtr(%hu, %ld)", uiParam, lMask ) );

   return zh_param( ( int ) uiParam, lMask );
}

ZH_BOOL zh_itemParamStore( ZH_USHORT uiParam, PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemParamStore(%hu, %p)", uiParam, ( void * ) pItem ) );

   if( zh_param( uiParam, ZH_IT_BYREF ) )
   {
      ZH_STACK_TLS_PRELOAD
      PZH_ITEM pDest = zh_stackItemFromBase( uiParam );

      if( pItem )
         zh_itemCopyToRef( pDest, pItem );
      else
         zh_itemSetNil( zh_itemUnRef( pDest ) );
      return ZH_TRUE;
   }

   return ZH_FALSE;
}

ZH_BOOL zh_itemParamStoreForward( ZH_USHORT uiParam, PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemParamStoreForward(%hu, %p)", uiParam, ( void * ) pItem ) );

   if( zh_param( uiParam, ZH_IT_BYREF ) )
   {
      ZH_STACK_TLS_PRELOAD
      PZH_ITEM pDest = zh_stackItemFromBase( uiParam );

      if( pItem )
         zh_itemMoveToRef( pDest, pItem );
      else
         zh_itemSetNil( zh_itemUnRef( pDest ) );
      return ZH_TRUE;
   }

   return ZH_FALSE;
}

ZH_BOOL zh_itemParamStoreRelease( ZH_USHORT uiParam, PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemParamStoreRelease(%hu, %p)", uiParam, ( void * ) pItem ) );

   if( zh_param( uiParam, ZH_IT_BYREF ) )
   {
      ZH_STACK_TLS_PRELOAD
      PZH_ITEM pDest = zh_stackItemFromBase( uiParam );

      if( pItem )
      {
         zh_itemMoveToRef( pDest, pItem );
         zh_itemRelease( pItem );
      }
      else
         zh_itemSetNil( zh_itemUnRef( pDest ) );
      return ZH_TRUE;
   }

   return ZH_FALSE;
}

ZH_USHORT zh_itemPCount( void )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemPCount()" ) );

   return ( ZH_USHORT ) zh_pcount();
}

ZH_BOOL zh_itemRelease( PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemRelease(%p)", ( void * ) pItem ) );

   if( pItem )
   {
      zh_gcGripDrop( pItem );
      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}

PZH_ITEM zh_itemArrayNew( ZH_SIZE nLen )
{
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemArrayNew(%" ZH_PFS "u)", nLen ) );

   pItem = zh_itemNew( NULL );

   zh_arrayNew( pItem, nLen );

   return pItem;
}

PZH_ITEM zh_itemArrayGet( PZH_ITEM pArray, ZH_SIZE nIndex )
{
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemArrayGet(%p, %" ZH_PFS "u)", ( void * ) pArray, nIndex ) );

   pItem = zh_itemNew( NULL );

   if( pArray )
      zh_arrayGet( pArray, nIndex, pItem );

   return pItem;
}

PZH_ITEM zh_itemArrayPut( PZH_ITEM pArray, ZH_SIZE nIndex, PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemArrayPut(%p, %" ZH_PFS "u, %p)", ( void * ) pArray, nIndex, ( void * ) pItem ) );

   if( pArray )
      zh_arraySet( pArray, nIndex, pItem );

   return pArray;
}

PZH_ITEM zh_itemPutNil( PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemPutNil(%p)", ( void * ) pItem ) );

   if( pItem )
      zh_itemSetNil( pItem );
   else
      pItem = zh_itemNew( NULL );

   return pItem;
}

PZH_ITEM zh_itemPutC( PZH_ITEM pItem, const char * szText )
{
   ZH_SIZE nLen, nAlloc;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemPutC(%p, %s)", ( void * ) pItem, szText ) );

   nLen = szText ? strlen( szText ) : 0;
   if( nLen <= 1 )
   {
      nAlloc = 0;
      szText = zh_szAscii[ nLen ? ( unsigned char ) szText[ 0 ] : 0 ];
   }
   else
   {
      nAlloc = nLen + 1;
      szText = ( char * ) zh_xmemcpy( zh_xgrab( nAlloc ), szText, nAlloc );
   }

   if( pItem )
   {
      if( ZH_IS_COMPLEX( pItem ) )
         zh_itemClear( pItem );
   }
   else
      pItem = zh_itemNew( NULL );

   pItem->type = ZH_IT_STRING;
   pItem->item.asString.value     = ( char * ) ZH_UNCONST( szText );
   pItem->item.asString.length    = nLen;
   pItem->item.asString.allocated = nAlloc;

   return pItem;
}

PZH_ITEM zh_itemPutCL( PZH_ITEM pItem, const char * szText, ZH_SIZE nLen )
{
   ZH_SIZE nAlloc;
   char * szValue;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemPutCL(%p, %.*s, %" ZH_PFS "u)", ( void * ) pItem, ( int ) nLen, szText, nLen ) );

   if( nLen <= 1 )
   {
      nAlloc = 0;
      szValue = ( char * ) ZH_UNCONST( zh_szAscii[ nLen ? ( unsigned char ) szText[ 0 ] : 0 ] );
   }
   else
   {
      nAlloc = nLen + 1;
      szValue = ( char * ) zh_xmemcpy( zh_xgrab( nAlloc ), szText, nLen );
      szValue[ nLen ] = '\0';
   }

   if( pItem )
   {
      if( ZH_IS_COMPLEX( pItem ) )
         zh_itemClear( pItem );
   }
   else
      pItem = zh_itemNew( NULL );

   /* NOTE: CA-Cl*pper seems to be buggy here, it will return nLen bytes of
            trash if the szText buffer is NULL, at least with zh_retclen().
            [vszakats] */

   pItem->type = ZH_IT_STRING;
   pItem->item.asString.value     = szValue;
   pItem->item.asString.length    = nLen;
   pItem->item.asString.allocated = nAlloc;

   return pItem;
}

PZH_ITEM zh_itemPutCConst( PZH_ITEM pItem, const char * szText )
{
   ZH_SIZE nLen;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemPutCConst(%p, %s)", ( void * ) pItem, szText ) );

   if( pItem )
   {
      if( ZH_IS_COMPLEX( pItem ) )
         zh_itemClear( pItem );
   }
   else
      pItem = zh_itemNew( NULL );

   nLen = szText ? strlen( szText ) : 0;

   pItem->type = ZH_IT_STRING;
   pItem->item.asString.length = nLen;
   pItem->item.asString.allocated = 0;
   pItem->item.asString.value = ( char * ) ZH_UNCONST( ( nLen > 1 ? szText :
                     zh_szAscii[ nLen ? ( unsigned char ) szText[ 0 ] : 0 ] ) );

   return pItem;
}

PZH_ITEM zh_itemPutCLConst( PZH_ITEM pItem, const char * szText, ZH_SIZE nLen )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemPutCLConst(%p, %.*s, %" ZH_PFS "u)", ( void * ) pItem, ( int ) nLen, szText, nLen ) );

   if( pItem )
   {
      if( ZH_IS_COMPLEX( pItem ) )
         zh_itemClear( pItem );
   }
   else
      pItem = zh_itemNew( NULL );

   pItem->type = ZH_IT_STRING;
   pItem->item.asString.length = nLen;
   pItem->item.asString.allocated = 0;

   if( nLen <= 1 )
      pItem->item.asString.value = ( char * ) ZH_UNCONST( zh_szAscii[ nLen ? ( unsigned char ) szText[ 0 ] : 0 ] );
   else if( szText[ nLen ] == '\0' )
      pItem->item.asString.value = ( char * ) ZH_UNCONST( szText );
   else
      zh_errInternal( 6003, "Internal error: zh_itemPutCLConst() missing termination character", NULL, NULL );

   return pItem;
}

PZH_ITEM zh_itemPutCPtr( PZH_ITEM pItem, char * szText )
{
   ZH_SIZE nLen;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemPutCPtr(%p, %s)", ( void * ) pItem, szText ) );

   if( pItem )
   {
      if( ZH_IS_COMPLEX( pItem ) )
         zh_itemClear( pItem );
   }
   else
      pItem = zh_itemNew( NULL );

   nLen = szText ? strlen( szText ) : 0;

   pItem->type = ZH_IT_STRING;
   pItem->item.asString.length = nLen;
   if( nLen <= 1 )
   {
      pItem->item.asString.allocated = 0;
      pItem->item.asString.value = ( char * ) ZH_UNCONST( zh_szAscii[ nLen ? ( unsigned char ) szText[ 0 ] : 0 ] );
      if( szText )
         zh_xfree( szText );
   }
   else
   {
      pItem->item.asString.allocated = nLen + 1;
      pItem->item.asString.value = szText;
   }

   return pItem;
}

PZH_ITEM zh_itemPutCLPtr( PZH_ITEM pItem, char * szText, ZH_SIZE nLen )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemPutCLPtr(%p, %.*s, %" ZH_PFS "u)", ( void * ) pItem, ( int ) nLen, szText, nLen ) );

   if( pItem )
   {
      if( ZH_IS_COMPLEX( pItem ) )
         zh_itemClear( pItem );
   }
   else
      pItem = zh_itemNew( NULL );

   pItem->type = ZH_IT_STRING;
   pItem->item.asString.length = nLen;
   if( nLen <= 1 )
   {
      pItem->item.asString.allocated = 0;
      pItem->item.asString.value = ( char * ) ZH_UNCONST( zh_szAscii[ nLen ? ( unsigned char ) szText[ 0 ] : 0 ] );
      zh_xfree( szText );
   }
   else
   {
      szText[ nLen ] = '\0';
      pItem->item.asString.allocated = nLen + 1;
      pItem->item.asString.value = szText;
   }

   return pItem;
}

void zh_itemSetCMemo( PZH_ITEM pItem )
{
   if( pItem && ZH_IS_STRING( pItem ) )
      pItem->type |= ZH_IT_MEMOFLAG;
}

/* NOTE: The caller should free the pointer if it's not NULL. [vszakats] */

char * zh_itemGetC( PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemGetC(%p)", ( void * ) pItem ) );

   if( pItem && ZH_IS_STRING( pItem ) )
   {
      char * szResult = ( char * ) zh_xgrab( pItem->item.asString.length + 1 );
      zh_xmemcpy( szResult, pItem->item.asString.value, pItem->item.asString.length );
      szResult[ pItem->item.asString.length ] = '\0';

      return szResult;
   }
   else
      return NULL;
}

/* NOTE: Caller should not modify the buffer returned by this function.
         [vszakats] */

const char * zh_itemGetCPtr( PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemGetCPtr(%p)", ( void * ) pItem ) );

   if( pItem && ZH_IS_STRING( pItem ) )
      return pItem->item.asString.value;
   else
      return "";
}

ZH_SIZE zh_itemGetCLen( PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemGetCLen(%p)", ( void * ) pItem ) );

   if( pItem && ZH_IS_STRING( pItem ) )
      return pItem->item.asString.length;
   else
      return 0;
}

ZH_SIZE zh_itemCopyC( PZH_ITEM pItem, char * szBuffer, ZH_SIZE nLen )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemCopyC(%p, %s, %" ZH_PFS "u)", ( void * ) pItem, szBuffer, nLen ) );

   if( pItem && ZH_IS_STRING( pItem ) )
   {
      if( nLen == 0 || nLen > pItem->item.asString.length )
         nLen = pItem->item.asString.length;

      zh_xmemcpy( szBuffer, pItem->item.asString.value, nLen );

      return nLen;
   }
   else
      return 0;
}

ZH_BOOL zh_itemFreeC( char * szText )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemFreeC(%s)", szText ) );

   if( szText )
   {
      zh_xfree( szText );

      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}


char * zh_itemGetDS( PZH_ITEM pItem, char * szDate )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemGetDS(%p, %p)", ( void * ) pItem, ( void * ) szDate ) );

   if( pItem && ZH_IS_DATETIME( pItem ) )
      return zh_dateDecStr( szDate, pItem->item.asDateTime.julian );
   else
      return zh_dateDecStr( szDate, 0 );
}

long zh_itemGetDL( PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemGetDL(%p)", ( void * ) pItem ) );

   if( pItem && ZH_IS_DATETIME( pItem ) )
      return pItem->item.asDateTime.julian;
   else
      return 0;
}

/* This function always closes the time with a zero byte, so it needs a
 * 18 character long buffer to store time in format "YYYYMMDDhhmmssfff"
 * with trailing 0 byte.
 */
char * zh_itemGetTS( PZH_ITEM pItem, char * szDateTime )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemGetTS(%p, %s)", ( void * ) pItem, szDateTime ) );

   if( pItem && ZH_IS_DATETIME( pItem ) )
      return zh_timeStampStrRawPut( szDateTime, pItem->item.asDateTime.julian,
                                                pItem->item.asDateTime.time );
   else
      return zh_timeStampStrRawPut( szDateTime, 0, 0 );
}

double zh_itemGetTD( PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemGetTD(%p)", ( void * ) pItem ) );

   if( pItem && ZH_IS_DATETIME( pItem ) )
      return zh_timeStampPackDT( pItem->item.asDateTime.julian,
                                 pItem->item.asDateTime.time );
   else
      return 0;
}

ZH_BOOL zh_itemGetTDT( PZH_ITEM pItem, long * plJulian, long * plMilliSec )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemGetTDT(%p,%p,%p)", ( void * ) pItem, ( void * ) plJulian, ( void * ) plMilliSec ) );

   if( pItem && ZH_IS_DATETIME( pItem ) )
   {
      *plJulian = pItem->item.asDateTime.julian;
      *plMilliSec = pItem->item.asDateTime.time;
      return ZH_TRUE;
   }
   else
   {
      *plJulian = *plMilliSec = 0;
      return ZH_FALSE;
   }
}

ZH_BOOL zh_itemGetL( PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemGetL(%p)", ( void * ) pItem ) );

   if( pItem )
   {
      if( ZH_IS_LOGICAL( pItem ) )
         return pItem->item.asLogical.value;

      else if( ZH_IS_INTEGER( pItem ) )
         return pItem->item.asInteger.value != 0;

      else if( ZH_IS_LONG( pItem ) )
         return pItem->item.asLong.value != 0;

      else if( ZH_IS_DOUBLE( pItem ) )
         return pItem->item.asDouble.value != 0.0;
   }

   return ZH_FALSE;
}

ZH_BOOL zh_itemGetLX( PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemGetLX(%p)", ( void * ) pItem ) );

   if( pItem )
   {
      if( ZH_IS_LOGICAL( pItem ) )
         return pItem->item.asLogical.value;

      else if( ZH_IS_INTEGER( pItem ) )
         return pItem->item.asInteger.value != 0;

      else if( ZH_IS_LONG( pItem ) )
         return pItem->item.asLong.value != 0;

      else if( ZH_IS_DOUBLE( pItem ) )
         return pItem->item.asDouble.value != 0.0;

      else if( ZH_IS_DATETIME( pItem ) )
         return pItem->item.asDateTime.julian != 0 ||
                pItem->item.asDateTime.time != 0;

      else
         return ZH_TRUE;
   }

   return ZH_FALSE;
}

double zh_itemGetND( PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemGetND(%p)", ( void * ) pItem ) );

   if( pItem )
   {
      if( ZH_IS_DOUBLE( pItem ) )
         return pItem->item.asDouble.value;

      else if( ZH_IS_INTEGER( pItem ) )
         return ( double ) pItem->item.asInteger.value;

      else if( ZH_IS_LONG( pItem ) )
         return ( double ) pItem->item.asLong.value;
   }

   return 0;
}

int zh_itemGetNI( PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemGetNI(%p)", ( void * ) pItem ) );

   if( pItem )
   {
      if( ZH_IS_INTEGER( pItem ) )
         return pItem->item.asInteger.value;

      else if( ZH_IS_LONG( pItem ) )
         return ( int ) pItem->item.asLong.value;

      else if( ZH_IS_DOUBLE( pItem ) )
         return ZH_CAST_INT( pItem->item.asDouble.value );
   }

   return 0;
}

long zh_itemGetNL( PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemGetNL(%p)", ( void * ) pItem ) );

   if( pItem )
   {
      if( ZH_IS_LONG( pItem ) )
         return ( long ) pItem->item.asLong.value;

      else if( ZH_IS_INTEGER( pItem ) )
         return ( long ) pItem->item.asInteger.value;

      else if( ZH_IS_DOUBLE( pItem ) )
         return ZH_CAST_LONG( pItem->item.asDouble.value );
   }

   return 0;
}

ZH_ISIZ zh_itemGetNS( PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemGetNS(%p)", ( void * ) pItem ) );

   if( pItem )
   {
      if( ZH_IS_LONG( pItem ) )
         return ( ZH_ISIZ ) pItem->item.asLong.value;

      else if( ZH_IS_INTEGER( pItem ) )
         return ( ZH_ISIZ ) pItem->item.asInteger.value;

      else if( ZH_IS_DOUBLE( pItem ) )
         return ZH_CAST_ISIZ( pItem->item.asDouble.value );
   }

   return 0;
}

ZH_MAXINT zh_itemGetNInt( PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemGetNL(%p)", ( void * ) pItem ) );

   if( pItem )
   {
      if( ZH_IS_LONG( pItem ) )
         return ( ZH_MAXINT ) pItem->item.asLong.value;

      else if( ZH_IS_INTEGER( pItem ) )
         return ( ZH_MAXINT ) pItem->item.asInteger.value;

      else if( ZH_IS_DOUBLE( pItem ) )
         return ZH_CAST_MAXINT( pItem->item.asDouble.value );
   }

   return 0;
}

#ifndef ZH_LONG_LONG_OFF
ZH_LONGLONG zh_itemGetNLL( PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemGetNL(%p)", ( void * ) pItem ) );

   if( pItem )
   {
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

void * zh_itemGetPtr( PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemGetPtr(%p)", ( void * ) pItem ) );

   if( pItem && ZH_IS_POINTER( pItem ) )
      return pItem->item.asPointer.value;
   else
      return NULL;
}

void * zh_itemGetPtrGC( PZH_ITEM pItem, const ZH_GC_FUNCS * pFuncs )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemGetPtrGC(%p,%p)", ( void * ) pItem, ( const void * ) pFuncs ) );

   if( pItem && ZH_IS_POINTER( pItem ) &&
       pItem->item.asPointer.collect &&
       zh_gcFuncs( pItem->item.asPointer.value ) == pFuncs )
      return pItem->item.asPointer.value;
   else
      return NULL;
}

PZH_SYMB zh_itemGetSymbol( PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemGetSymbol(%p)", ( void * ) pItem ) );

   if( pItem && ZH_IS_SYMBOL( pItem ) )
      return pItem->item.asSymbol.value;
   else
      return NULL;
}

PZH_ITEM zh_itemReturn( PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemReturn(%p)", ( void * ) pItem ) );

   if( pItem )
   {
      ZH_STACK_TLS_PRELOAD
      zh_itemCopy( zh_stackReturnItem(), pItem );
   }

   return pItem;
}

PZH_ITEM zh_itemReturnForward( PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemReturnForward(%p)", ( void * ) pItem ) );

   if( pItem )
   {
      ZH_STACK_TLS_PRELOAD
      zh_itemMove( zh_stackReturnItem(), pItem );
   }

   return pItem;
}

void zh_itemReturnRelease( PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemReturnRelease(%p)", ( void * ) pItem ) );

   if( pItem )
   {
      ZH_STACK_TLS_PRELOAD
      zh_itemMove( zh_stackReturnItem(), pItem );
      zh_itemRelease( pItem );
   }
}


PZH_ITEM zh_itemPutDS( PZH_ITEM pItem, const char * szDate )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemPutDS(%p, %.8s)", ( void * ) pItem, szDate ) );

   if( pItem )
   {
      if( ZH_IS_COMPLEX( pItem ) )
         zh_itemClear( pItem );
   }
   else
      pItem = zh_itemNew( NULL );

   pItem->type = ZH_IT_DATE;
   pItem->item.asDateTime.julian = zh_dateEncStr( szDate );
   pItem->item.asDateTime.time = 0;

   return pItem;
}

PZH_ITEM zh_itemPutD( PZH_ITEM pItem, int iYear, int iMonth, int iDay )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemPutD(%p, %04i, %02i, %02i)", ( void * ) pItem, iYear, iMonth, iDay ) );

   if( pItem )
   {
      if( ZH_IS_COMPLEX( pItem ) )
         zh_itemClear( pItem );
   }
   else
      pItem = zh_itemNew( NULL );

   pItem->type = ZH_IT_DATE;
   pItem->item.asDateTime.julian = zh_dateEncode( iYear, iMonth, iDay );
   pItem->item.asDateTime.time = 0;

   return pItem;
}

PZH_ITEM zh_itemPutDL( PZH_ITEM pItem, long lJulian )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemPutDL(%p, %ld)", ( void * ) pItem, lJulian ) );

   if( pItem )
   {
      if( ZH_IS_COMPLEX( pItem ) )
         zh_itemClear( pItem );
   }
   else
      pItem = zh_itemNew( NULL );

   pItem->type = ZH_IT_DATE;
   pItem->item.asDateTime.julian = lJulian;
   pItem->item.asDateTime.time = 0;

   return pItem;
}

PZH_ITEM zh_itemPutTS( PZH_ITEM pItem, const char * szDateTime )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemPutTS(%p, %s)", ( void * ) pItem, szDateTime ) );

   if( pItem )
   {
      if( ZH_IS_COMPLEX( pItem ) )
         zh_itemClear( pItem );
   }
   else
      pItem = zh_itemNew( NULL );

   pItem->type = ZH_IT_TIMESTAMP;
   zh_timeStampStrRawGet( szDateTime, &pItem->item.asDateTime.julian,
                                      &pItem->item.asDateTime.time );

   return pItem;
}

PZH_ITEM zh_itemPutTD( PZH_ITEM pItem, double dTimeStamp )
{
   long lJulian, lMilliSec;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemPutTD(%p, %lf)", ( void * ) pItem, dTimeStamp ) );

   if( pItem )
   {
      if( ZH_IS_COMPLEX( pItem ) )
         zh_itemClear( pItem );
   }
   else
      pItem = zh_itemNew( NULL );

   zh_timeStampUnpackDT( dTimeStamp, &lJulian, &lMilliSec );
   pItem->type = ZH_IT_TIMESTAMP;
   pItem->item.asDateTime.julian = lJulian;
   pItem->item.asDateTime.time = lMilliSec;

   return pItem;
}

PZH_ITEM zh_itemPutTDT( PZH_ITEM pItem, long lJulian, long lMilliSec )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemPutTDT(%p, %ld, %ld)", ( void * ) pItem, lJulian, lMilliSec ) );

   if( pItem )
   {
      if( ZH_IS_COMPLEX( pItem ) )
         zh_itemClear( pItem );
   }
   else
      pItem = zh_itemNew( NULL );

   pItem->type = ZH_IT_TIMESTAMP;
   pItem->item.asDateTime.julian = lJulian;
   pItem->item.asDateTime.time = lMilliSec;

   return pItem;
}

PZH_ITEM zh_itemPutL( PZH_ITEM pItem, ZH_BOOL bValue )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemPutL(%p, %d)", ( void * ) pItem, ( int ) bValue ) );

   if( pItem )
   {
      if( ZH_IS_COMPLEX( pItem ) )
         zh_itemClear( pItem );
   }
   else
      pItem = zh_itemNew( NULL );

   pItem->type = ZH_IT_LOGICAL;
   pItem->item.asLogical.value = bValue;

   return pItem;
}

PZH_ITEM zh_itemPutND( PZH_ITEM pItem, double dNumber )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemPutND(%p, %lf)", ( void * ) pItem, dNumber ) );

   if( pItem )
   {
      if( ZH_IS_COMPLEX( pItem ) )
         zh_itemClear( pItem );
   }
   else
      pItem = zh_itemNew( NULL );

   pItem->type = ZH_IT_DOUBLE;
   pItem->item.asDouble.length = ZH_DBL_LENGTH( dNumber );
   pItem->item.asDouble.decimal = ( ZH_USHORT ) zh_stackSetStruct()->ZH_SET_DECIMALS;
   pItem->item.asDouble.value = dNumber;

   return pItem;
}

PZH_ITEM zh_itemPutNI( PZH_ITEM pItem, int iNumber )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemPutNI(%p, %d)", ( void * ) pItem, iNumber ) );

   if( pItem )
   {
      if( ZH_IS_COMPLEX( pItem ) )
         zh_itemClear( pItem );
   }
   else
      pItem = zh_itemNew( NULL );

   pItem->type = ZH_IT_INTEGER;
   pItem->item.asInteger.value = iNumber;
   pItem->item.asInteger.length = ZH_INT_LENGTH( iNumber );

   return pItem;
}

PZH_ITEM zh_itemPutNL( PZH_ITEM pItem, long lNumber )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemPutNL(%p, %ld)", ( void * ) pItem, lNumber ) );

   if( pItem )
   {
      if( ZH_IS_COMPLEX( pItem ) )
         zh_itemClear( pItem );
   }
   else
      pItem = zh_itemNew( NULL );

   ZH_ITEM_PUT_LONGRAW( pItem, lNumber );

   return pItem;
}

PZH_ITEM zh_itemPutNS( PZH_ITEM pItem, ZH_ISIZ nNumber )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemPutNS(%p, %" ZH_PFS "d)", ( void * ) pItem, nNumber ) );

   if( pItem )
   {
      if( ZH_IS_COMPLEX( pItem ) )
         zh_itemClear( pItem );
   }
   else
      pItem = zh_itemNew( NULL );

#if ZH_SIZE_MAX <= ZH_VMUINT_MAX
   pItem->type = ZH_IT_INTEGER;
   pItem->item.asInteger.value = nNumber;
   /* EXP limit used intentionally */
   pItem->item.asInteger.length = ZH_INT_EXPLENGTH( nNumber );
#else
   if( ZH_LIM_INT( nNumber ) )
   {
      pItem->type = ZH_IT_INTEGER;
      pItem->item.asInteger.value = ( int ) nNumber;
      /* EXP limit used intentionally */
      pItem->item.asInteger.length = ZH_INT_EXPLENGTH( nNumber );
   }
   else
   {
      pItem->type = ZH_IT_LONG;
      pItem->item.asLong.value = nNumber;
      pItem->item.asLong.length = ZH_LONG_LENGTH( nNumber );
   }
#endif

   return pItem;
}

#ifndef ZH_LONG_LONG_OFF
PZH_ITEM zh_itemPutNLL( PZH_ITEM pItem, ZH_LONGLONG llNumber )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemPutNL(%p, %" PFLL "d)", ( void * ) pItem, llNumber ) );

   if( pItem )
   {
      if( ZH_IS_COMPLEX( pItem ) )
         zh_itemClear( pItem );
   }
   else
      pItem = zh_itemNew( NULL );

#if ZH_VMLONG_MAX >= LONGLONG_MAX
   pItem->type = ZH_IT_LONG;
   pItem->item.asLong.value = ( ZH_MAXINT ) llNumber;
   pItem->item.asLong.length = ZH_LONG_LENGTH( llNumber );
#else
   pItem->type = ZH_IT_DOUBLE;
   pItem->item.asDouble.value = ( double ) llNumber;
   pItem->item.asDouble.length = ZH_DBL_LENGTH( pItem->item.asDouble.value );
   pItem->item.asDouble.decimal = 0;
#endif
   return pItem;
}
#endif

PZH_ITEM zh_itemPutNInt( PZH_ITEM pItem, ZH_MAXINT nNumber )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemPutNInt(%p, %" PFHL "d)", ( void * ) pItem, nNumber ) );

   if( pItem )
   {
      if( ZH_IS_COMPLEX( pItem ) )
         zh_itemClear( pItem );
   }
   else
      pItem = zh_itemNew( NULL );

   if( ZH_LIM_INT( nNumber ) )
   {
      pItem->type = ZH_IT_INTEGER;
      pItem->item.asInteger.value = ( int ) nNumber;
      /* EXP limit used intentionally */
      pItem->item.asInteger.length = ZH_INT_EXPLENGTH( nNumber );
   }
   else
   {
      pItem->type = ZH_IT_LONG;
      pItem->item.asLong.value = nNumber;
      pItem->item.asLong.length = ZH_LONG_LENGTH( nNumber );
   }

   return pItem;
}

PZH_ITEM zh_itemPutNIntLen( PZH_ITEM pItem, ZH_MAXINT nNumber, int iWidth )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemPutNIntLen(%p, %" PFHL "d, %d)", ( void * ) pItem, nNumber, iWidth ) );

   if( ZH_LIM_INT( nNumber ) )
   {
      return zh_itemPutNILen( pItem, ( int ) nNumber, iWidth );
   }
   else
   {
#ifdef ZH_LONG_LONG_OFF
      return zh_itemPutNLLen( pItem, ( long ) nNumber, iWidth );
#else
      return zh_itemPutNLLLen( pItem, ( ZH_LONGLONG ) nNumber, iWidth );
#endif
   }
}

PZH_ITEM zh_itemPutNLen( PZH_ITEM pItem, double dNumber, int iWidth, int iDec )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemPutNLen(%p, %lf, %d, %d)", ( void * ) pItem, dNumber, iWidth, iDec ) );

   if( iDec < 0 )
   {
      ZH_STACK_TLS_PRELOAD
      iDec = zh_stackSetStruct()->ZH_SET_DECIMALS;
   }

   if( iDec == 0 )
   {
      ZH_MAXINT nNumber = ( ZH_MAXINT ) dNumber;

      if( ( double ) nNumber == dNumber )
      {
         if( iWidth <= 0 || iWidth >= ZH_DEFAULT_WIDTH )
            iWidth = ZH_DBL_LENGTH( dNumber );

         return zh_itemPutNIntLen( pItem, nNumber, iWidth );
      }
   }

   return zh_itemPutNDLen( pItem, dNumber, iWidth, iDec );
}

PZH_ITEM zh_itemPutNDLen( PZH_ITEM pItem, double dNumber, int iWidth, int iDec )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemPutNDLen(%p, %lf, %d, %d)", ( void * ) pItem, dNumber, iWidth, iDec ) );

   if( pItem )
   {
      if( ZH_IS_COMPLEX( pItem ) )
         zh_itemClear( pItem );
   }
   else
      pItem = zh_itemNew( NULL );

   if( iWidth <= 0 || iWidth >= ZH_DEFAULT_WIDTH )
      iWidth = ZH_DBL_LENGTH( dNumber );

   if( iDec < 0 )
   {
      ZH_STACK_TLS_PRELOAD
      iDec = zh_stackSetStruct()->ZH_SET_DECIMALS;
   }

   pItem->type = ZH_IT_DOUBLE;
   pItem->item.asDouble.length = ( ZH_USHORT ) iWidth;
   pItem->item.asDouble.decimal = ( ZH_USHORT ) iDec;
   pItem->item.asDouble.value = dNumber;

   return pItem;
}

PZH_ITEM zh_itemPutNDDec( PZH_ITEM pItem, double dNumber, int iDec )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemPutNDDec(%p, %lf, %i)", ( void * ) pItem, dNumber, iDec ) );

   if( pItem )
   {
      if( ZH_IS_COMPLEX( pItem ) )
         zh_itemClear( pItem );
   }
   else
      pItem = zh_itemNew( NULL );

   pItem->type = ZH_IT_DOUBLE;
   pItem->item.asDouble.length = ZH_DBL_LENGTH( dNumber );

   if( iDec == ZH_DEFAULT_DECIMALS )
   {
      ZH_STACK_TLS_PRELOAD
      pItem->item.asDouble.decimal = ( ZH_USHORT ) zh_stackSetStruct()->ZH_SET_DECIMALS;
   }
   else
   {
      pItem->item.asDouble.decimal = ( ZH_USHORT ) iDec;
   }

   pItem->item.asDouble.value = dNumber;

   return pItem;
}

double zh_itemGetNDDec( PZH_ITEM pItem, int * piDec )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemGetNDDec(%p,%p)", ( void * ) pItem, ( void * ) piDec ) );

   if( ZH_IS_INTEGER( pItem ) )
   {
      *piDec = 0;
      return ( double ) pItem->item.asInteger.value;
   }
   else if( ZH_IS_LONG( pItem ) )
   {
      *piDec = 0;
      return ( double ) pItem->item.asLong.value;
   }
   else if( ZH_IS_DOUBLE( pItem ) )
   {
      *piDec = pItem->item.asDouble.decimal;
      return pItem->item.asDouble.value;
   }

   *piDec = 0;
   return 0.0;
}


PZH_ITEM zh_itemPutNILen( PZH_ITEM pItem, int iNumber, int iWidth )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemPutNILen(%p, %d, %d)", ( void * ) pItem, iNumber, iWidth ) );

   if( pItem )
   {
      if( ZH_IS_COMPLEX( pItem ) )
         zh_itemClear( pItem );
   }
   else
      pItem = zh_itemNew( NULL );

   if( iWidth <= 0 || iWidth >= ZH_DEFAULT_WIDTH )
      iWidth = ZH_INT_LENGTH( iNumber );

   pItem->type = ZH_IT_INTEGER;
   pItem->item.asInteger.length = ( ZH_USHORT ) iWidth;
   pItem->item.asInteger.value = iNumber;

   return pItem;
}

PZH_ITEM zh_itemPutNLLen( PZH_ITEM pItem, long lNumber, int iWidth )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemPutNLLen(%p, %ld, %d)", ( void * ) pItem, lNumber, iWidth ) );

   if( pItem )
   {
      if( ZH_IS_COMPLEX( pItem ) )
         zh_itemClear( pItem );
   }
   else
      pItem = zh_itemNew( NULL );

#if ZH_VMINT_MAX == LONG_MAX
   if( iWidth <= 0 || iWidth >= ZH_DEFAULT_WIDTH )
      iWidth = ZH_INT_LENGTH( lNumber );

   pItem->type = ZH_IT_INTEGER;
   pItem->item.asInteger.value = ( int ) lNumber;
   pItem->item.asInteger.length = ( ZH_USHORT ) iWidth;
#else
   if( iWidth <= 0 || iWidth >= ZH_DEFAULT_WIDTH )
      iWidth = ZH_LONG_LENGTH( lNumber );

   pItem->type = ZH_IT_LONG;
   pItem->item.asLong.value = ( ZH_MAXINT ) lNumber;
   pItem->item.asLong.length = iWidth;
#endif

   return pItem;
}

#ifndef ZH_LONG_LONG_OFF
PZH_ITEM zh_itemPutNLLLen( PZH_ITEM pItem, ZH_LONGLONG llNumber, int iWidth )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemPutNLLLen(%p, %" PFLL "d, %d)", ( void * ) pItem, llNumber, iWidth ) );

   if( pItem )
   {
      if( ZH_IS_COMPLEX( pItem ) )
         zh_itemClear( pItem );
   }
   else
      pItem = zh_itemNew( NULL );

#if ZH_VMLONG_MAX >= LONGLONG_MAX
   if( iWidth <= 0 || iWidth >= ZH_DEFAULT_WIDTH )
      iWidth = ZH_LONG_LENGTH( llNumber );

   pItem->type = ZH_IT_LONG;
   pItem->item.asLong.value = ( ZH_MAXINT ) llNumber;
   pItem->item.asLong.length = ( ZH_USHORT ) iWidth;
#else
   pItem->type = ZH_IT_DOUBLE;
   pItem->item.asDouble.value = ( double ) llNumber;
   if( iWidth <= 0 || iWidth >= ZH_DEFAULT_WIDTH )
      iWidth = ZH_LONG_LENGTH( pItem->item.asDouble.value );
   pItem->item.asDouble.length = iWidth;
   pItem->item.asDouble.decimal = 0;
#endif

   return pItem;
}
#endif

PZH_ITEM zh_itemPutNumType( PZH_ITEM pItem, double dNumber, int iDec, int iType1, int iType2 )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemPutNumType( %p, %lf, %d, %i, %i)", ( void * ) pItem, dNumber, iDec, iType1, iType2 ) );

   if( iDec || iType1 & ZH_IT_DOUBLE || iType2 & ZH_IT_DOUBLE )
   {
      return zh_itemPutNDDec( pItem, dNumber, iDec );
   }
   else if( ZH_DBL_LIM_INT( dNumber ) )
   {
      return zh_itemPutNI( pItem, ( int ) dNumber );
   }
   else if( ZH_DBL_LIM_LONG( dNumber ) )
   {
#ifdef ZH_LONG_LONG_OFF
      return zh_itemPutNL( pItem, ( long ) ( unsigned long ) dNumber );
#else
      return zh_itemPutNLL( pItem, ( ZH_LONGLONG ) dNumber );
#endif
   }
   else
   {
      return zh_itemPutND( pItem, dNumber );
   }
}

PZH_ITEM zh_itemPutPtr( PZH_ITEM pItem, void * pValue )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemPutPtr(%p, %p)", ( void * ) pItem, pValue ) );

   if( pItem )
   {
      if( ZH_IS_COMPLEX( pItem ) )
         zh_itemClear( pItem );
   }
   else
      pItem = zh_itemNew( NULL );

   pItem->type = ZH_IT_POINTER;
   pItem->item.asPointer.value = pValue;
   pItem->item.asPointer.collect =
   pItem->item.asPointer.single = ZH_FALSE;

   return pItem;
}

PZH_ITEM zh_itemPutPtrGC( PZH_ITEM pItem, void * pValue )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemPutPtrGC(%p, %p)", ( void * ) pItem, pValue ) );

   if( pItem )
   {
      if( ZH_IS_COMPLEX( pItem ) )
         zh_itemClear( pItem );
   }
   else
      pItem = zh_itemNew( NULL );

   pItem->type = ZH_IT_POINTER;
   pItem->item.asPointer.value = pValue;
   pItem->item.asPointer.collect = ZH_TRUE;
   pItem->item.asPointer.single = ZH_FALSE;

   zh_gcAttach( pValue );

   return pItem;
}

PZH_ITEM zh_itemPutPtrRawGC( PZH_ITEM pItem, void * pValue )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemPutPtrRawGC(%p, %p)", ( void * ) pItem, pValue ) );

   if( pItem )
   {
      if( ZH_IS_COMPLEX( pItem ) )
         zh_itemClear( pItem );
   }
   else
      pItem = zh_itemNew( NULL );

   pItem->type = ZH_IT_POINTER;
   pItem->item.asPointer.value = pValue;
   pItem->item.asPointer.collect = ZH_TRUE;
   pItem->item.asPointer.single = ZH_FALSE;

   return pItem;
}

PZH_ITEM zh_itemPutSymbol( PZH_ITEM pItem, PZH_SYMB pSym )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemPutSymbol(%p,%p)", ( void * ) pItem, ( void * ) pSym ) );

   if( pItem )
   {
      if( ZH_IS_COMPLEX( pItem ) )
         zh_itemClear( pItem );
   }
   else
      pItem = zh_itemNew( NULL );

   pItem->type = ZH_IT_SYMBOL;
   pItem->item.asSymbol.value        = pSym;
   pItem->item.asSymbol.stackstate   = NULL;
   pItem->item.asSymbol.paramcnt     =
   pItem->item.asSymbol.paramdeclcnt = 0;

   return pItem;
}

void zh_itemGetNLen( PZH_ITEM pItem, int * piWidth, int * piDecimal )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemGetNLen(%p, %p, %p)", ( void * ) pItem, ( void * ) piWidth, ( void * ) piDecimal ) );

   if( pItem )
   {
      if( ZH_IS_DOUBLE( pItem ) )
      {
         if( piWidth ) *piWidth = ( int ) pItem->item.asDouble.length;
         if( piDecimal ) *piDecimal = ( int ) pItem->item.asDouble.decimal;
      }
      else if( ZH_IS_INTEGER( pItem ) )
      {
         if( piWidth ) *piWidth = ( int ) pItem->item.asInteger.length;
         if( piDecimal ) *piDecimal = 0;
      }
      else if( ZH_IS_LONG( pItem ) )
      {
         if( piWidth ) *piWidth = ( int ) pItem->item.asLong.length;
         if( piDecimal ) *piDecimal = 0;
      }
      else
      {
         if( piWidth ) *piWidth = 0;
         if( piDecimal ) *piDecimal = 0;
      }
   }
}

ZH_SIZE zh_itemSize( PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemSize(%p)", ( void * ) pItem ) );

   if( pItem )
   {
      if( ZH_IS_STRING( pItem ) )
         return pItem->item.asString.length;
      else if( ZH_IS_ARRAY( pItem ) )
         return zh_arrayLen( pItem );
      else if( ZH_IS_HASH( pItem ) )
         return zh_hashLen( pItem );
   }

   return 0;
}

ZH_TYPE zh_itemType( PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemType(%p)", ( void * ) pItem ) );

   if( pItem )
      return ( ZH_TYPE ) ZH_ITEM_TYPE( pItem );
   else
      return ZH_IT_NIL;
}

const char * zh_itemTypeStr( PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemTypeStr(%p)", ( void * ) pItem ) );

   if( pItem )
   {
      switch( ZH_ITEM_TYPE( pItem ) )
      {
         case ZH_IT_ARRAY:
            return zh_arrayIsObject( pItem ) ? "O" : "A";

         case ZH_IT_BLOCK:
            return "B";

         case ZH_IT_DATE:
            return "D";

         case ZH_IT_TIMESTAMP:
            return "T";

         case ZH_IT_LOGICAL:
            return "L";

         case ZH_IT_INTEGER:
         case ZH_IT_LONG:
         case ZH_IT_DOUBLE:
            return "N";

         case ZH_IT_STRING:
            return "C";

         case ZH_IT_MEMO:
            return "M";

         case ZH_IT_HASH:
            return "H";

         case ZH_IT_POINTER:
            return "P";

         case ZH_IT_SYMBOL:
            return "S";
      }
   }

   return "U";
}

void zh_itemInit( PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemInit(%p)", ( void * ) pItem ) );

   if( pItem )
      pItem->type = ZH_IT_NIL;
}

void zh_itemClear( PZH_ITEM pItem )
{
   ZH_TYPE type;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemClear(%p)", ( void * ) pItem ) );

   type = ZH_ITEM_TYPERAW( pItem );
   pItem->type = ZH_IT_NIL;

   /* GCLOCK enter */
   if( type & ZH_IT_STRING )
   {
      if( pItem->item.asString.allocated )
         zh_xRefFree( pItem->item.asString.value );
   }
   else if( type & ZH_IT_ARRAY )
      zh_gcRefFree( pItem->item.asArray.value );

   else if( type & ZH_IT_BLOCK )
      zh_gcRefFree( pItem->item.asBlock.value );

   else if( type & ZH_IT_HASH )
      zh_gcRefFree( pItem->item.asHash.value );

   else if( type & ZH_IT_BYREF )
   {
      if( type & ZH_IT_MEMVAR )
         zh_memvarValueDecRef( pItem->item.asMemvar.value );

      else if( type & ZH_IT_ENUM )     /* FOR EACH control variable */
         zh_vmEnumRelease( pItem->item.asEnum.basePtr,
                           pItem->item.asEnum.valuePtr );

      else if( type & ZH_IT_EXTREF )
         pItem->item.asExtRef.func->clear( pItem->item.asExtRef.value );

      else if( pItem->item.asRefer.offset == 0 && pItem->item.asRefer.value >= 0 )
         zh_gcRefFree( pItem->item.asRefer.BasePtr.array );
   }
   else if( type & ZH_IT_POINTER )
   {
      if( pItem->item.asPointer.collect )
         zh_gcRefFree( pItem->item.asPointer.value );
   }
   /* GCLOCK leave */
}

void zh_itemCopy( PZH_ITEM pDest, PZH_ITEM pSource )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemCopy(%p, %p)", ( void * ) pDest, ( void * ) pSource ) );

   if( pDest == pSource )
      zh_errInternal( ZH_EI_ITEMBADCOPY, NULL, "zh_itemCopy()", NULL );

   if( ZH_IS_COMPLEX( pDest ) )
      zh_itemClear( pDest );

   zh_itemRawCpy( pDest, pSource );
   pDest->type &= ~ZH_IT_DEFAULT;

   if( ZH_IS_COMPLEX( pSource ) )
   {
      /* GCLOCK enter */
      if( ZH_IS_STRING( pSource ) )
      {
         if( pSource->item.asString.allocated )
            zh_xRefInc( pSource->item.asString.value );
      }
      else if( ZH_IS_ARRAY( pSource ) )
         zh_gcRefInc( pSource->item.asArray.value );

      else if( ZH_IS_BLOCK( pSource ) )
         zh_gcRefInc( pSource->item.asBlock.value );

      else if( ZH_IS_HASH( pSource ) )
         zh_gcRefInc( pSource->item.asHash.value );

      else if( ZH_IS_BYREF( pSource ) )
      {
         if( ZH_IS_MEMVAR( pSource ) )
            zh_memvarValueIncRef( pSource->item.asMemvar.value );

         else if( ZH_IS_ENUM( pSource ) )    /* enumerators cannot be copied */
            pDest->type = ZH_IT_NIL;

         else if( ZH_IS_EXTREF( pSource ) )
            pSource->item.asExtRef.func->copy( pDest );

         else if( pSource->item.asRefer.offset == 0 && pSource->item.asRefer.value >= 0 )
            zh_gcRefInc( pSource->item.asRefer.BasePtr.array );
      }
      else if( ZH_IS_POINTER( pSource ) )
      {
         if( pSource->item.asPointer.collect )
         {
            if( pSource->item.asPointer.single )
               pDest->item.asPointer.collect = ZH_FALSE;
            else
               zh_gcRefInc( pSource->item.asPointer.value );
         }
      }
      /* GCLOCK leave */
   }
}

void zh_itemCopyToRef( PZH_ITEM pDest, PZH_ITEM pSource )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemCopyToRef(%p, %p)", ( void * ) pDest, ( void * ) pSource ) );

   if( ZH_IS_BYREF( pDest ) )
   {
      pDest = zh_itemUnRefWrite( pDest, pSource );
      if( ! pDest || pDest == pSource )
         /* extended reference or pDest is a reference to pSource
            - do not copy */
         return;
   }

   if( ZH_IS_BYREF( pSource ) )
   {
      if( zh_itemUnRef( pSource ) == pDest )
         /*
          * assign will create cyclic reference
          * pSource and pDest reference to the same item
          * we can simply drop coping
          */
         return;
   }

   if( ZH_IS_OBJECT( pDest ) &&
       zh_objOperatorCall( ZH_OO_OP_ASSIGN, pDest, pDest, pSource, NULL ) )
      return;

   zh_itemCopy( pDest, pSource );
}

void zh_itemCopyFromRef( PZH_ITEM pDest, PZH_ITEM pSource )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemCopyFromRef(%p, %p)", ( void * ) pDest, ( void * ) pSource ) );

   if( ZH_IS_BYREF( pSource ) )
   {
      pSource = zh_itemUnRef( pSource );
      if( pDest == pSource )
         /* pSource is a reference to pDest - do not copy */
         return;
   }

   zh_itemCopy( pDest, pSource );
}

/*
 * copy (transfer) the value of item without increasing
 * a reference counters, the pSource item is cleared
 */
void zh_itemMove( PZH_ITEM pDest, PZH_ITEM pSource )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemMove(%p, %p)", ( void * ) pDest, ( void * ) pSource ) );

   if( pDest == pSource )
      zh_errInternal( ZH_EI_ITEMBADCOPY, NULL, "zh_itemMove()", NULL );

   if( ZH_IS_COMPLEX( pDest ) )
      zh_itemClear( pDest );

   /* GCLOCK enter */
   zh_itemRawCpy( pDest, pSource );
   pDest->type &= ~ZH_IT_DEFAULT;
   pSource->type = ZH_IT_NIL;
   /* GCLOCK leave */
}

void zh_itemMoveRef( PZH_ITEM pDest, PZH_ITEM pSource )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemMoveRef(%p, %p)", ( void * ) pDest, ( void * ) pSource ) );

   if( ZH_IS_BYREF( pSource ) )
   {
      if( zh_itemUnRef( pSource ) == ( ZH_IS_BYREF( pDest ) ?
                                             zh_itemUnRef( pDest ) : pDest ) )
      {
         /*
          * assign will create cyclic reference
          * pSource is a reference to pDest
          * we can simply drop coping
          */
         zh_itemSetNil( pSource );
         return;
      }
   }

   if( ZH_IS_COMPLEX( pDest ) )
      zh_itemClear( pDest );

   /* GCLOCK enter */
   zh_itemRawCpy( pDest, pSource );
   pDest->type &= ~ZH_IT_DEFAULT;
   pSource->type = ZH_IT_NIL;
   /* GCLOCK leave */
}


void zh_itemMoveToRef( PZH_ITEM pDest, PZH_ITEM pSource )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemMoveToRef(%p, %p)", ( void * ) pDest, ( void * ) pSource ) );

   if( ZH_IS_BYREF( pDest ) )
   {
      pDest = zh_itemUnRefWrite( pDest, pSource );
      if( ! pDest || pDest == pSource )
      {
         /* extended reference or pDest is a reference to pSource
            - do not copy */
         zh_itemSetNil( pSource );
         return;
      }
   }

   if( ZH_IS_BYREF( pSource ) )
   {
      if( zh_itemUnRef( pSource ) == pDest )
      {
         /*
          * assign will create cyclic reference
          * pSource and pDest reference to the same item
          * we can simply drop coping
          */
         zh_itemSetNil( pSource );
         return;
      }
   }

   if( ZH_IS_OBJECT( pDest ) &&
       zh_objOperatorCall( ZH_OO_OP_ASSIGN, pDest, pDest, pSource, NULL ) )
   {
      zh_itemSetNil( pSource );
      return;
   }

   if( ZH_IS_COMPLEX( pDest ) )
      zh_itemClear( pDest );

   /* GCLOCK enter */
   zh_itemRawCpy( pDest, pSource );
   pDest->type &= ~ZH_IT_DEFAULT;
   pSource->type = ZH_IT_NIL;
   /* GCLOCK leave */
}

void zh_itemMoveFromRef( PZH_ITEM pDest, PZH_ITEM pSource )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemMoveFromRef(%p, %p)", ( void * ) pDest, ( void * ) pSource ) );

   if( ZH_IS_BYREF( pSource ) )
   {
      PZH_ITEM pUnRef = zh_itemUnRef( pSource );
      if( pDest != pUnRef )
         /* pSource is not a reference to pDest - make copy */
         zh_itemCopy( pDest, pUnRef );
      zh_itemClear( pSource );
   }
   else
      zh_itemMove( pDest, pSource );
}


void zh_itemSwap( PZH_ITEM pItem1, PZH_ITEM pItem2 )
{
   ZH_ITEM temp;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemSwap(%p, %p)", ( void * ) pItem1, ( void * ) pItem2 ) );

   /*
    * It's safe to use this version because our GC cannot be
    * activated inside memcpy()
    */
   /* GCLOCK enter */
   zh_itemRawCpy( &temp, pItem2 );
   zh_itemRawCpy( pItem2, pItem1 );
   zh_itemRawCpy( pItem1, &temp );
   pItem1->type &= ~ZH_IT_DEFAULT;
   pItem2->type &= ~ZH_IT_DEFAULT;
   /* GCLOCK leave */
}


/* De-references item passed by the reference */

PZH_ITEM zh_itemUnRefOnce( PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemUnRefOnce(%p)", ( void * ) pItem ) );

   if( ZH_IS_BYREF( pItem ) )
   {
      if( ZH_IS_MEMVAR( pItem ) )
      {
         pItem = pItem->item.asMemvar.value;
      }
      else if( ZH_IS_ENUM( pItem ) ) /* FOR EACH control variable */
      {
         /* enumerator variable */
         if( pItem->item.asEnum.valuePtr )
            return pItem->item.asEnum.valuePtr;
         else
         {
            PZH_ITEM pBase = ZH_IS_BYREF( pItem->item.asEnum.basePtr ) ?
                            zh_itemUnRef( pItem->item.asEnum.basePtr ) :
                                          pItem->item.asEnum.basePtr;
            if( ZH_IS_ARRAY( pBase ) )
            {
               pBase = zh_arrayGetItemPtr( pBase, pItem->item.asEnum.offset );
               if( pBase )
                  return pBase;
            }
            else if( ZH_IS_HASH( pBase ) )
            {
               pBase = zh_hashGetValueAt( pBase, pItem->item.asEnum.offset );
               if( pBase )
                  return pBase;
            }
            else if( ZH_IS_STRING( pBase ) )
            {
               if( pItem->item.asEnum.offset > 0 &&
                   ( ZH_SIZE ) pItem->item.asEnum.offset <= pBase->item.asString.length )
               {
                  pItem->item.asEnum.valuePtr = zh_itemPutCL( NULL,
                     pBase->item.asString.value + pItem->item.asEnum.offset - 1, 1 );
                  return pItem->item.asEnum.valuePtr;
               }
            }

            /* put it here to avoid recursive RT error generation */
            pItem->item.asEnum.valuePtr = zh_itemNew( NULL );

            if( zh_vmRequestQuery() == 0 )
            {
               ZH_STACK_TLS_PRELOAD
               zh_itemPutNS( zh_stackAllocItem(), pItem->item.asEnum.offset );
               zh_errRT_BASE( EG_BOUND, 1132, NULL, zh_langDGetErrorDesc( EG_ARRACCESS ),
                              2, pItem->item.asEnum.basePtr, zh_stackItemFromTop( -1 ) );
               zh_stackPop();
            }
            return pItem->item.asEnum.valuePtr;
         }
      }
      else if( ZH_IS_EXTREF( pItem ) )
      {
         pItem = pItem->item.asExtRef.func->read( pItem );
      }
      else
      {
         if( pItem->item.asRefer.value >= 0 )
         {
            if( pItem->item.asRefer.offset == 0 )
            {
               /* a reference to a static variable or array item */
               if( ( ZH_SIZE ) pItem->item.asRefer.value <
                   pItem->item.asRefer.BasePtr.array->nLen )
               {
                  pItem = pItem->item.asRefer.BasePtr.array->pItems +
                          pItem->item.asRefer.value;
               }
               else if( zh_vmRequestQuery() == 0 )
               {
                  ZH_STACK_TLS_PRELOAD
                  zh_arrayPushBase( pItem->item.asRefer.BasePtr.array );
                  zh_itemPutNS( zh_stackAllocItem(), pItem->item.asRefer.value + 1 );
                  zh_errRT_BASE( EG_BOUND, 1132, NULL, zh_langDGetErrorDesc( EG_ARRACCESS ),
                                 2, zh_stackItemFromTop( -2 ), zh_stackItemFromTop( -1 ) );
                  zh_stackPop();
                  zh_stackPop();

                  /* check it again - user error handler can resize the array */
                  if( ( ZH_SIZE ) pItem->item.asRefer.value <
                      pItem->item.asRefer.BasePtr.array->nLen )
                  {
                     pItem = pItem->item.asRefer.BasePtr.array->pItems +
                             pItem->item.asRefer.value;
                  }
                  else
                     /* It's safe to clear the item - if we are here then
                        the reference chain to this item does not start in
                        one of the pItem->item.asRefer.BasePtr.array items
                        or more then one reference to this array exists
                        so it will not be freed [druzus] */
                     zh_itemClear( pItem );
               }
            }
            else
            {
               /* a reference to a local variable */
               PZH_ITEM * pLocal;

               pLocal = *( pItem->item.asRefer.BasePtr.itemsbasePtr ) +
                        pItem->item.asRefer.offset + pItem->item.asRefer.value;
               pItem = *pLocal;
            }
         }
         else
         {
            /* local variable referenced in a codeblock */
            pItem = zh_codeblockGetRef( pItem->item.asRefer.BasePtr.block,
                                        ( int ) pItem->item.asRefer.value );
         }
      }
   }

   return pItem;
}

/* De-references item passed by the reference */

PZH_ITEM zh_itemUnRef( PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemUnRef(%p)", ( void * ) pItem ) );

   do
   {
      pItem = zh_itemUnRefOnce( pItem );
   }
   while( ZH_IS_BYREF( pItem ) );

   return pItem;
}

/* Unreference passed variable for writing
 * Do not unreference string enumerators
 */
PZH_ITEM zh_itemUnRefWrite( PZH_ITEM pItem, PZH_ITEM pSource )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemUnRefWrite(%p,%p)", ( void * ) pItem, ( void * ) pSource ) );

   if( ZH_IS_EXTREF( pItem ) )
   {
      pItem = pItem->item.asExtRef.func->write( pItem, pSource );
   }
   else if( ZH_IS_STRING( pSource ) &&
       pSource->item.asString.length == 1 )
   {
      do
      {
         if( ZH_IS_ENUM( pItem ) && ZH_IS_BYREF( pItem->item.asEnum.basePtr ) &&
             pItem->item.asEnum.offset >= 1 )
         {
            PZH_ITEM pBase = zh_itemUnRef( pItem->item.asEnum.basePtr );
            if( ZH_IS_STRING( pBase ) &&
                ( ZH_SIZE ) pItem->item.asEnum.offset <= pBase->item.asString.length )
            {
               zh_itemUnShareString( pBase );
               pBase->item.asString.value[ pItem->item.asEnum.offset - 1 ] =
                                             pSource->item.asString.value[ 0 ];
               return pItem->item.asEnum.valuePtr;
            }
         }
         pItem = zh_itemUnRefOnce( pItem );
      }
      while( ZH_IS_BYREF( pItem ) );
   }
   else
      pItem = zh_itemUnRef( pItem );

   return pItem;
}

/* Unreference passed variable
 * Do not unreference the last reference stored
 */
PZH_ITEM zh_itemUnRefRefer( PZH_ITEM pItem )
{
   PZH_ITEM pLast;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemUnRefRefer(%p)", ( void * ) pItem ) );

   do
   {
      pLast = pItem;
      pItem = zh_itemUnRefOnce( pItem );
   }
   while( ZH_IS_BYREF( pItem ) );

   return pLast;
}

/* Resize string buffer of given string item */

PZH_ITEM zh_itemReSizeString( PZH_ITEM pItem, ZH_SIZE nSize )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemReSizeString(%p,%" ZH_PFS "u)", ( void * ) pItem, nSize ) );

   if( pItem->item.asString.allocated == 0 )
   {
      char * szText = ( char * ) zh_xgrab( nSize + 1 );
      zh_xmemcpy( szText, pItem->item.asString.value,
                  pItem->item.asString.length );
      szText[ nSize ] = '\0';
      pItem->item.asString.value     = szText;
      pItem->item.asString.length    = nSize;
      pItem->item.asString.allocated = nSize + 1;
   }
   else
   {
      ZH_SIZE nAlloc = nSize + 1 +
                ( pItem->item.asString.allocated <= nSize ? nSize : 0 );
      pItem->item.asString.value = ( char * )
                     zh_xRefResize( pItem->item.asString.value,
                                    pItem->item.asString.length,
                                    nAlloc, &pItem->item.asString.allocated );
      pItem->item.asString.length = nSize;
      pItem->item.asString.value[ nSize ] = '\0';
   }
   pItem->type &= ~ZH_IT_DEFAULT;

   return pItem;
}

/* UnShare string buffer of given string item */

PZH_ITEM zh_itemUnShareString( PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemUnShareString(%p)", ( void * ) pItem ) );

   if( pItem->item.asString.allocated == 0 ||
       zh_xRefCount( pItem->item.asString.value ) > 1 )
   {
      ZH_SIZE nLen = pItem->item.asString.length + 1;
      char * szText = ( char * ) zh_xmemcpy( zh_xgrab( nLen ),
                                             pItem->item.asString.value, nLen );
      if( pItem->item.asString.allocated )
      {
         /* GCLOCK enter */
         zh_xRefFree( pItem->item.asString.value );
         /* GCLOCK leave */
      }
      pItem->item.asString.value = szText;
      pItem->item.asString.allocated = nLen;
   }
   pItem->type &= ~ZH_IT_DEFAULT;

   return pItem;
}

PZH_ITEM zh_itemUnShare( PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemUnShare(%p)", ( void * ) pItem ) );

   if( ZH_IS_BYREF( pItem ) )
      pItem = zh_itemUnRef( pItem );

   if( ZH_IS_STRING( pItem ) )
      return zh_itemUnShareString( pItem );
   else
      return pItem;
}

ZH_BOOL zh_itemGetWriteCL( PZH_ITEM pItem, char ** pszValue, ZH_SIZE * pnLen )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemGetWriteCL(%p,%p,%p)", ( void * ) pItem, ( void * ) pszValue, ( void * ) pnLen ) );

   if( pItem )
   {
      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_STRING( pItem ) )
      {
         zh_itemUnShareString( pItem );
         *pnLen = pItem->item.asString.length;
         *pszValue = pItem->item.asString.value;
         return ZH_TRUE;
      }
   }
   return ZH_FALSE;
}

/* clone the given item */
PZH_ITEM zh_itemClone( PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemClone(%p)", ( void * ) pItem ) );

   if( ZH_IS_ARRAY( pItem ) )
   {
      if( ZH_IS_OBJECT( pItem ) )
         return zh_objCloneTo( zh_itemNew( NULL ), pItem );
      else
         return zh_arrayClone( pItem );
   }
   else if( ZH_IS_HASH( pItem ) )
      return zh_hashClone( pItem );
   else
      return zh_itemNew( pItem );
}

void zh_itemCloneTo( PZH_ITEM pDest, PZH_ITEM pSource )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemCloneTo(%p,%p)", ( void * ) pDest, ( void * ) pSource ) );

   if( ZH_IS_ARRAY( pSource ) )
   {
      if( ZH_IS_OBJECT( pSource ) )
         zh_objCloneTo( pDest, pSource );
      else
         zh_arrayCloneTo( pDest, pSource );
   }
   else if( ZH_IS_HASH( pSource ) )
      zh_hashCloneTo( pDest, pSource );
   else
      zh_itemCopy( pDest, pSource );
}


/* Check whether two items are exactly equal */
ZH_BOOL zh_itemEqual( PZH_ITEM pItem1, PZH_ITEM pItem2 )
{
   ZH_BOOL fResult = ZH_FALSE;

   if( ZH_IS_NUMERIC( pItem1 ) )
   {
      if( ZH_IS_NUMINT( pItem1 ) && ZH_IS_NUMINT( pItem2 ) )
         fResult = ZH_ITEM_GET_NUMINTRAW( pItem1 ) == ZH_ITEM_GET_NUMINTRAW( pItem2 );
      else
         fResult = ZH_IS_NUMERIC( pItem2 ) &&
                   zh_itemGetND( pItem1 ) == zh_itemGetND( pItem2 );
   }
   else if( ZH_IS_STRING( pItem1 ) )
      fResult = ZH_IS_STRING( pItem2 ) &&
                pItem1->item.asString.length == pItem2->item.asString.length &&
                memcmp( pItem1->item.asString.value,
                        pItem2->item.asString.value,
                        pItem1->item.asString.length ) == 0;

   else if( ZH_IS_NIL( pItem1 ) )
      fResult = ZH_IS_NIL( pItem2 );

   else if( ZH_IS_DATETIME( pItem1 ) )
      fResult = ZH_IS_DATETIME( pItem2 ) &&
                pItem1->item.asDateTime.julian == pItem2->item.asDateTime.julian &&
                pItem1->item.asDateTime.time == pItem2->item.asDateTime.time;

   else if( ZH_IS_LOGICAL( pItem1 ) )
      fResult = ZH_IS_LOGICAL( pItem2 ) && ( pItem1->item.asLogical.value ?
                pItem2->item.asLogical.value : ! pItem2->item.asLogical.value );

   else if( ZH_IS_ARRAY( pItem1 ) )
      fResult = ZH_IS_ARRAY( pItem2 ) &&
                pItem1->item.asArray.value == pItem2->item.asArray.value;

   else if( ZH_IS_HASH( pItem1 ) )
      fResult = ZH_IS_HASH( pItem2 ) &&
                pItem1->item.asHash.value == pItem2->item.asHash.value;

   else if( ZH_IS_POINTER( pItem1 ) )
      fResult = ZH_IS_POINTER( pItem2 ) &&
                pItem1->item.asPointer.value == pItem2->item.asPointer.value;

   else if( ZH_IS_BLOCK( pItem1 ) )
      fResult = ZH_IS_BLOCK( pItem2 ) &&
                pItem1->item.asBlock.value == pItem2->item.asBlock.value;

   else if( ZH_IS_SYMBOL( pItem1 ) )
      fResult = ZH_IS_SYMBOL( pItem2 ) &&
                ( pItem1->item.asSymbol.value == pItem2->item.asSymbol.value ||
                  ( pItem1->item.asSymbol.value->pDynSym != NULL &&
                    pItem1->item.asSymbol.value->pDynSym ==
                    pItem2->item.asSymbol.value->pDynSym ) );

   return fResult;
}

/* For compatible types compare pItem1 with pItem2 setting piResult
   to -1, 0 or 1 if pItem1 is <, == or > then pItem2 and return true
   otherwise return false.
 */
ZH_BOOL zh_itemCompare( PZH_ITEM pItem1, PZH_ITEM pItem2, ZH_BOOL bForceExact, int * piResult )
{
   ZH_BOOL fResult = ZH_FALSE;

   if( ZH_IS_NUMERIC( pItem1 ) )
   {
      if( ZH_IS_NUMINT( pItem1 ) && ZH_IS_NUMINT( pItem2 ) )
      {
         ZH_MAXINT n1 = ZH_ITEM_GET_NUMINTRAW( pItem1 ),
                   n2 = ZH_ITEM_GET_NUMINTRAW( pItem2 );
         *piResult = n1 < n2 ? -1 : ( n1 > n2 ? 1 : 0 );
         fResult = ZH_TRUE;
      }
      else if( ZH_IS_NUMERIC( pItem2 ) )
      {
         double d1 = zh_itemGetND( pItem1 ),
                d2 = zh_itemGetND( pItem2 );
         *piResult = d1 < d2 ? -1 : ( d1 > d2 ? 1 : 0 );
         fResult = ZH_TRUE;
      }
   }
   else if( ZH_IS_STRING( pItem1 ) )
   {
      if( ZH_IS_STRING( pItem2 ) )
      {
         *piResult = zh_itemStrCmp( pItem1, pItem2, bForceExact );
         fResult = ZH_TRUE;
      }
   }
   else if( ZH_IS_NIL( pItem1 ) )
   {
      if( ZH_IS_NIL( pItem2 ) )
      {
         *piResult = 0;
         fResult = ZH_TRUE;
      }
   }
   else if( ZH_IS_DATETIME( pItem1 ) )
   {
      if( ZH_IS_DATETIME( pItem2 ) )
      {
         *piResult = pItem1->item.asDateTime.julian < pItem2->item.asDateTime.julian ? -1 :
                   ( pItem1->item.asDateTime.julian > pItem2->item.asDateTime.julian ? 1 :
                   ( pItem1->item.asDateTime.time < pItem2->item.asDateTime.time ? -1 :
                   ( pItem1->item.asDateTime.time > pItem2->item.asDateTime.time ? 1 : 0 ) ) );
         fResult = ZH_TRUE;
      }
   }
   else if( ZH_IS_LOGICAL( pItem1 ) )
   {
      if( ZH_IS_LOGICAL( pItem2 ) )
      {
         *piResult = pItem1->item.asLogical.value ?
                     ( pItem2->item.asLogical.value ? 0 : 1 ) :
                     ( pItem2->item.asLogical.value ? -1 : 0 );
         fResult = ZH_TRUE;
      }
   }
   else if( ZH_IS_ARRAY( pItem1 ) )
   {
      if( ZH_IS_ARRAY( pItem2 ) )
      {
         *piResult = pItem1->item.asArray.value < pItem2->item.asArray.value ? -1 :
                   ( pItem1->item.asArray.value > pItem2->item.asArray.value ? 1 : 0 );
         fResult = ZH_TRUE;
      }
   }
   else if( ZH_IS_HASH( pItem1 ) )
   {
      if( ZH_IS_HASH( pItem2 ) )
      {
         *piResult = pItem1->item.asHash.value < pItem2->item.asHash.value ? -1 :
                   ( pItem1->item.asHash.value > pItem2->item.asHash.value ? 1 : 0 );
         fResult = ZH_TRUE;
      }
   }
   else if( ZH_IS_POINTER( pItem1 ) )
   {
      if( ZH_IS_POINTER( pItem2 ) )
      {
         *piResult = pItem1->item.asPointer.value < pItem2->item.asPointer.value ? -1 :
                   ( pItem1->item.asPointer.value > pItem2->item.asPointer.value ? 1 : 0 );
         fResult = ZH_TRUE;
      }
   }
   else if( ZH_IS_BLOCK( pItem1 ) )
   {
      if( ZH_IS_BLOCK( pItem2 ) )
      {
         *piResult = pItem1->item.asBlock.value < pItem2->item.asBlock.value ? -1 :
                   ( pItem1->item.asBlock.value > pItem2->item.asBlock.value ? 1 : 0 );
         fResult = ZH_TRUE;
      }
   }
   else if( ZH_IS_SYMBOL( pItem1 ) )
   {
      if( ZH_IS_SYMBOL( pItem2 ) )
      {
         *piResult = ( pItem1->item.asSymbol.value == pItem2->item.asSymbol.value ||
                       ( pItem1->item.asSymbol.value->pDynSym != NULL &&
                         pItem1->item.asSymbol.value->pDynSym ==
                         pItem2->item.asSymbol.value->pDynSym ) ) ? 0 :
                     ( pItem1->item.asSymbol.value < pItem2->item.asSymbol.value ? -1 : 1 );
         fResult = ZH_TRUE;
      }
   }
   return fResult;
}


/* Check whether two strings are equal (0), smaller (-1), or greater (1) */
int zh_itemStrCmp( PZH_ITEM pFirst, PZH_ITEM pSecond, ZH_BOOL bForceExact )
{
   ZH_STACK_TLS_PRELOAD
   const char * szFirst;
   const char * szSecond;
   ZH_SIZE nLenFirst;
   ZH_SIZE nLenSecond;
   ZH_SIZE nMinLen;
   int iRet = 0; /* Current status */

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemStrCmp(%p, %p, %d)", ( void * ) pFirst, ( void * ) pSecond, ( int ) bForceExact ) );

   szFirst = pFirst->item.asString.value;
   szSecond = pSecond->item.asString.value;
   nLenFirst = pFirst->item.asString.length;
   nLenSecond = pSecond->item.asString.length;

   if( szFirst == szSecond && nLenFirst == nLenSecond )
      return 0;


   if( ! bForceExact && zh_stackSetStruct()->ZH_SET_EXACT )
   {
      /* SET EXACT ON and not using == */
      /* Don't include trailing spaces */
      while( nLenFirst > nLenSecond && szFirst[ nLenFirst - 1 ] == ' ' )
         nLenFirst--;
      while( nLenSecond > nLenFirst && szSecond[ nLenSecond - 1 ] == ' ' )
         nLenSecond--;
      bForceExact = ZH_TRUE;
   }

   nMinLen = nLenFirst < nLenSecond ? nLenFirst : nLenSecond;

   /* Both strings not empty */
   if( nMinLen )
   {
      PZH_CODEPAGE cdp = zh_vmCodepage();
      if( cdp && ! ZH_CODEPAGE_ISBINSORT( cdp ) )
         iRet = zh_cdpcmp( szFirst, nLenFirst, szSecond, nLenSecond,
                           cdp, bForceExact );
      else
      {
         do
         {
            if( *szFirst != *szSecond )
            {
               iRet = ( ( ZH_UCHAR ) *szFirst < ( ZH_UCHAR ) *szSecond ) ? -1 : 1;
               break;
            }
            szFirst++;
            szSecond++;
         }
         while( --nMinLen );

         /* If equal and length is different ! */
         if( ! iRet && nLenFirst != nLenSecond )
         {
            /* Force an exact comparison? */
            if( bForceExact || nLenSecond > nLenFirst )
               iRet = ( nLenFirst < nLenSecond ) ? -1 : 1;
         }
      }
   }
   else
   {
      /* Both empty ? */
      if( nLenFirst != nLenSecond )
      {
         if( bForceExact )
            iRet = ( nLenFirst < nLenSecond ) ? -1 : 1;
         else
            iRet = ( nLenSecond == 0 ) ? 0 : -1;
      }
      else
         /* Both empty => Equal ! */
         iRet = 0;
   }

   return iRet;
}

/* Check whether two strings are equal (0), smaller (-1), or greater (1), ignore case */
int zh_itemStrICmp( PZH_ITEM pFirst, PZH_ITEM pSecond, ZH_BOOL bForceExact )
{
   ZH_STACK_TLS_PRELOAD
   const char * szFirst;
   const char * szSecond;
   ZH_SIZE nLenFirst;
   ZH_SIZE nLenSecond;
   ZH_SIZE nMinLen;
   int iRet = 0; /* Current status */

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemStrICmp(%p, %p, %d)", ( void * ) pFirst, ( void * ) pSecond, ( int ) bForceExact ) );

   szFirst = pFirst->item.asString.value;
   szSecond = pSecond->item.asString.value;
   nLenFirst = pFirst->item.asString.length;
   nLenSecond = pSecond->item.asString.length;

   if( ! bForceExact && zh_stackSetStruct()->ZH_SET_EXACT )
   {
      /* SET EXACT ON and not using == */
      /* Don't include trailing spaces */
      while( nLenFirst > nLenSecond && szFirst[ nLenFirst - 1 ] == ' ' )
         nLenFirst--;
      while( nLenSecond > nLenFirst && szSecond[ nLenSecond - 1 ] == ' ' )
         nLenSecond--;
      bForceExact = ZH_TRUE;
   }

   nMinLen = nLenFirst < nLenSecond ? nLenFirst : nLenSecond;

   /* Both strings not empty */
   if( nMinLen )
   {
      PZH_CODEPAGE cdp = zh_vmCodepage();
      if( cdp && ! ZH_CODEPAGE_ISBINSORT( cdp ) )
         iRet = zh_cdpicmp( szFirst, nLenFirst, szSecond, nLenSecond,
                            cdp, bForceExact );
      else
      {
         do
         {
            int i1 = ZH_TOUPPER( ( ZH_UCHAR ) *szFirst );
            int i2 = ZH_TOUPPER( ( ZH_UCHAR ) *szSecond );
            if( i1 != i2 )
            {
               iRet = ( i1 < i2 ) ? -1 : 1;
               break;
            }
            szFirst++;
            szSecond++;
         }
         while( --nMinLen );

         /* If equal and length is different ! */
         if( ! iRet && nLenFirst != nLenSecond )
         {
            /* Force an exact comparison? */
            if( bForceExact || nLenSecond > nLenFirst )
               iRet = ( nLenFirst < nLenSecond ) ? -1 : 1;
         }
      }
   }
   else
   {
      /* Both empty ? */
      if( nLenFirst != nLenSecond )
      {
         if( bForceExact )
            iRet = ( nLenFirst < nLenSecond ) ? -1 : 1;
         else
            iRet = ( nLenSecond == 0 ) ? 0 : -1;
      }
      else
         /* Both empty => Equal ! */
         iRet = 0;
   }

   return iRet;
}

/* converts a numeric to a string with optional width & precision. */

ZH_BOOL zh_itemStrBuf( char * szResult, PZH_ITEM pNumber, int iSize, int iDec )
{
   int iPos, iDot;
   ZH_BOOL fNeg;

   if( iDec < 0 )
      iDec = 0;

   if( iDec > 0 )
      iPos = iDot = iSize - iDec - 1;
   else
   {
      iPos = iSize;
      iDot = 0;
   }

   if( ZH_IS_DOUBLE( pNumber ) )
   {
      double dNumber = zh_itemGetND( pNumber );

      if( ! zh_isfinite( dNumber ) )
      {
         /* Numeric overflow */
         iPos = -1;
      }
      else
      {
         double dInt, dFract, dDig, doBase = 10.0;
         int iPrec, iFirst = -1;

         #if 0
         dNumber = zh_numRound( dNumber, iDec );
         #endif

#ifdef ZH_NUM_PRECISION
         iPrec = ZH_NUM_PRECISION;
#else
         iPrec = 16;
#endif

         if( dNumber < 0 )
         {
            fNeg = ZH_TRUE;
            dFract = modf( -dNumber, &dInt );
         }
         else
         {
            fNeg = ZH_FALSE;
            dFract = modf( dNumber, &dInt );
         }

         while( iPos-- > 0 )
         {
            dDig = modf( dInt / doBase + 0.01, &dInt ) * doBase;
            szResult[ iPos ] = '0' + ( char ) ( dDig + 0.01 );
            if( szResult[ iPos ] != '0' )
               iFirst = iPos;
            if( dInt < 1 )
               break;
         }

         if( iPos > 0 )
            memset( szResult, ' ', iPos );

         if( iDec > 0 && iPos >= 0 )
         {
            for( iPos = iDot + 1; iPos < iSize; iPos++ )
            {
               dFract = modf( dFract * doBase, &dDig );
               szResult[ iPos ] = '0' + ( char ) ( dDig + 0.01 );
               if( iFirst < 0 )
               {
                  if( szResult[ iPos ] != '0' )
                     iFirst = iPos - 1;
               }
               else if( iPos - iFirst >= iPrec )
                  break;
            }
         }


         if( iPos >= 0 )
         {
            int iZer, iLast;

            if( iFirst < 0 )
               iZer = 0;
            else
               iZer = iSize - iFirst - iPrec - ( iDec > 0 ? 1 : 0 );

            dFract = modf( dFract * doBase, &dDig );
            iLast = ( int ) ( dDig + 0.01 );

            /* hack for x.xxxx4999999999, e.g. 8.995 ~FL 8.994999999999999218.. */
            if( iLast == 4 && iZer < 0 )
            {
               for( iPos = -iZer; iPos > 0; --iPos )
               {
                  dFract = modf( dFract * doBase, &dDig );
                  if( dDig + 0.01 < 9 && ( iPos != 1 || dDig < 2 ) )
                     break;
               }
               if( iPos == 0 )
                  iLast = 5;
            }
            iLast = iLast >= 5 ? 1 : 0;

            iPos = iSize;
            while( iPos-- > 0 )
            {
               if( iDec == 0 || iPos != iDot )
               {
                  if( iZer > 0 )
                  {
                     if( iDec == 0 || iPos <= iDot + 1 )
                        iLast = szResult[ iPos ] >= '5' ? 1 : 0;

                     szResult[ iPos ] = '0';
                     --iZer;
                  }
                  else if( iLast > 0 )
                  {
                     if( szResult[ iPos ] == '9' )
                        szResult[ iPos ] = '0';
                     else
                     {
                        if( szResult[ iPos ] < '0' ) /* '-' or ' ' */
                        {
                           szResult[ iPos ] = '1';
                           iFirst = iPos;
                        }
                        else
                        {
                           szResult[ iPos ]++;
                           if( iFirst < 0 )
                              iFirst = iPos;
                        }
                        break;
                     }
                  }
                  else
                     break;
               }
            }

            if( fNeg && iFirst >= 0 && iPos >= 0 )
            {
               iPos = ( iDot > 0 && iFirst >= iDot ) ? iDot - 2 : iFirst - 1;
               if( iPos >= 0 )
                  szResult[ iPos ] = '-';
            }
         }
      }
   }
   else
   {
      ZH_MAXINT nNumber;

      if( ZH_IS_INTEGER( pNumber ) )
         nNumber = pNumber->item.asInteger.value;

      else if( ZH_IS_LONG( pNumber ) )
         nNumber = pNumber->item.asLong.value;

      else
      {
         nNumber = 0;
         iPos = -1;
      }

      fNeg = ( nNumber < 0 );
      while( iPos-- > 0 )
      {
         szResult[ iPos ] = '0' + ( char ) ( fNeg ? -( nNumber % 10 ) : ( nNumber % 10 ) );
         nNumber /= 10;
         if( nNumber == 0 )
            break;
      }
      if( fNeg && iPos-- > 0 )
         szResult[ iPos ] = '-';

      if( iPos > 0 )
         memset( szResult, ' ', iPos );

      if( iDec > 0 && iPos >= 0 )
         memset( &szResult[ iSize - iDec ], '0', iDec );
   }

   szResult[ iSize ] = '\0';
   /* Set to asterisks in case of overflow */
   if( iPos < 0 )
   {
      memset( szResult, '*', iSize );
      return ZH_FALSE;
   }
   else if( iDot > 0 )
      szResult[ iDot ] = '.';

   return ZH_TRUE;
}

/* converts a numeric to a string with optional width & precision.
   This function should be used by any function that wants to format numeric
   data for displaying, printing, or putting in a database.

   Note: The caller is responsible for calling zh_xfree() to free the results
         buffer, but ONLY if the return value is not a NULL pointer! (If a NULL
         pointer is returned, then there was a conversion error.)
 */
char * zh_itemStr( PZH_ITEM pNumber, PZH_ITEM pWidth, PZH_ITEM pDec )
{
   char * szResult = NULL;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemStr(%p, %p, %p)", ( void * ) pNumber, ( void * ) pWidth, ( void * ) pDec ) );

   if( pNumber )
   {
      /* Default to the width and number of decimals specified by the item,
         with a limit of 90 integer places, plus one space for the sign. */
      int iWidth, iDec, iSize;

      zh_itemGetNLen( pNumber, &iWidth, &iDec );

      if( iWidth > 90 )
         iWidth = 90;

      if( pWidth && ZH_IS_NUMERIC( pWidth ) )
      {
         /* If the width parameter is specified, override the default value
            and set the number of decimals to zero */
         iWidth = zh_itemGetNI( pWidth );

         if( iWidth < 1 )
            iWidth = 10;                  /* If 0 or negative, use default */
         iDec = 0;
      }

      if( iWidth > 1 && pDec && ZH_IS_NUMERIC( pDec ) )
      {
         /* This function does not include the decimal places in the width,
            so the width must be adjusted downwards, if the decimal places
            parameter is greater than 0  */
         iDec = zh_itemGetNI( pDec );

         if( iDec <= 0 )
            iDec = 0;
         else if( pWidth )
            iWidth -= ( iDec + 1 );
      }

      iSize = ( iDec > 0 ? iWidth + 1 + iDec : iWidth );

      if( iSize > 0 )
      {
         szResult = ( char * ) zh_xgrab( iSize + 1 );
         zh_itemStrBuf( szResult, pNumber, iSize, iDec );
      }
   }

   return szResult;
}

/* NOTE: The caller must free the pointer if the bFreeReq param gets set to
         ZH_TRUE, this trick is required to stay thread safe, while minimize
         memory allocation and buffer copying.
         As a side effect the caller should never modify the returned buffer
         since it may point to a constant value. [vszakats] */

char * zh_itemString( PZH_ITEM pItem, ZH_SIZE * nLen, ZH_BOOL * bFreeReq )
{
   char * buffer;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemString(%p, %p, %p)", ( void * ) pItem, ( void * ) nLen, ( void * ) bFreeReq ) );

   switch( ZH_ITEM_TYPE( pItem ) )
   {
      case ZH_IT_STRING:
      case ZH_IT_MEMO:
         buffer = ( char * ) ZH_UNCONST( zh_itemGetCPtr( pItem ) );
         * nLen = zh_itemGetCLen( pItem );
         * bFreeReq = ZH_FALSE;
         break;

      case ZH_IT_DATE:
      {
         ZH_STACK_TLS_PRELOAD
         char szDate[ 9 ];

         zh_dateDecStr( szDate, pItem->item.asDateTime.julian );

         buffer = ( char * ) zh_xgrab( 11 );
         zh_dateFormat( szDate, buffer, zh_stackSetStruct()->ZH_SET_DATEFORMAT );
         * nLen = strlen( buffer );
         * bFreeReq = ZH_TRUE;
         break;
      }

      case ZH_IT_TIMESTAMP:
      {
         ZH_STACK_TLS_PRELOAD
         char szDateTime[ 27 ];

         zh_timeStampFormat( szDateTime,
                             zh_stackSetStruct()->ZH_SET_DATEFORMAT,
                             zh_stackSetStruct()->ZH_SET_TIMEFORMAT,
                             pItem->item.asDateTime.julian,
                             pItem->item.asDateTime.time );

         buffer = zh_strdup( szDateTime );
         * nLen = strlen( buffer );
         * bFreeReq = ZH_TRUE;
         break;
      }

      case ZH_IT_DOUBLE:
      case ZH_IT_INTEGER:
      case ZH_IT_LONG:
      {
         ZH_STACK_TLS_PRELOAD
         if( zh_stackSetStruct()->ZH_SET_FIXED )
         {
            /* If fixed mode is enabled, use the default number of decimal places. */
            zh_itemPutNI( zh_stackAllocItem(), zh_stackSetStruct()->ZH_SET_DECIMALS );
            buffer = zh_itemStr( pItem, NULL, zh_stackItemFromTop( -1 ) );
            zh_stackPop();
         }
         else
            buffer = zh_itemStr( pItem, NULL, NULL );
         if( buffer )
         {
            *nLen     = strlen( buffer );
            *bFreeReq = ZH_TRUE;
         }
         else
         {
            buffer    = ( char * ) "";
            *nLen     = 0;
            *bFreeReq = ZH_FALSE;
         }
         break;
      }
      case ZH_IT_NIL:
         buffer = ( char * ) "NIL";
         *nLen = 3;
         *bFreeReq = ZH_FALSE;
         break;

      case ZH_IT_LOGICAL:
         buffer = ( char * ) ( zh_itemGetL( pItem ) ? ".T." : ".F." );
         *nLen = 3;
         *bFreeReq = ZH_FALSE;
         break;

      case ZH_IT_SYMBOL:
      {
         PZH_SYMB pSymbol = zh_itemGetSymbol( pItem );
         const char * szName = pSymbol ? pSymbol->szName : "?";

         *bFreeReq = ZH_TRUE;
         *nLen = strlen( szName ) + 3;
         buffer = ( char * ) zh_xgrab( *nLen + 1 );
         buffer[ 0 ] = '@';
         memcpy( buffer + 1, szName, *nLen - 3 );
         buffer[ *nLen - 2 ] = '(';
         buffer[ *nLen - 1 ] = ')';
         buffer[ *nLen ] = '\0';
         break;
      }
      case ZH_IT_POINTER:
      {
         int size = ( sizeof( void * ) << 1 ) + 3; /* n bytes for address + 0x + \0 */

         ZH_PTRUINT addr = zh_vmInternalsEnabled() ?
            ( ZH_PTRUINT ) zh_itemGetPtr( pItem ) :
            ( ZH_PTRUINT ) ( zh_itemGetPtr( pItem ) ? -1 : 0 );

         *nLen = size - 1;
         *bFreeReq = ZH_TRUE;
         buffer = ( char * ) zh_xgrab( size );
         buffer[ 0 ] = '0';
         buffer[ 1 ] = 'x';
         buffer[ --size ] = '\0';
         do
         {
            ZH_UCHAR uc = ( ZH_UCHAR ) ( addr & 0xf );
            buffer[ --size ] = ( char ) ( uc + ( uc < 10 ? '0' : 'A' - 10 ) );
            addr >>= 4;
         }
         while( size > 2 );
         break;
      }
      default:
         buffer = ( char * ) "";
         *nLen = 0;
         *bFreeReq = ZH_FALSE;
   }

   return buffer;
}

/* This function is used by all of the PAD functions to prepare the argument
   being padded. If date, convert to string using zh_dateFormat(). If numeric,
   convert to unpadded string. Return pointer to string and set string length */

char * zh_itemPadConv( PZH_ITEM pItem, ZH_SIZE * pnSize, ZH_BOOL * bFreeReq )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemPadConv(%p, %p, %p)", ( void * ) pItem, ( void * ) pnSize, ( void * ) bFreeReq ) );

   if( pItem )
   {
      switch( ZH_ITEM_TYPE( pItem ) )
      {
         case ZH_IT_STRING:
         case ZH_IT_MEMO:
         case ZH_IT_DATE:
         case ZH_IT_TIMESTAMP:
            return zh_itemString( pItem, pnSize, bFreeReq );

         case ZH_IT_DOUBLE:
         case ZH_IT_INTEGER:
         case ZH_IT_LONG:
         {
            int i;
            char * buffer = zh_itemString( pItem, pnSize, bFreeReq );

            /* remove leading spaces if any, a little bit redundant but
             * I don't want to complicate the API interface more. Druzus
             */
            for( i = 0; buffer[ i ] == ' '; i++ )
               ;

            if( i > 0 )
            {
               int j = 0;
               *pnSize -= i;
               do
               {
                  buffer[ j++ ] = buffer[ i ];
               }
               while( buffer[ i++ ] );
            }
            return buffer;
         }
         default:
            break;
      }
   }
   return NULL;
}

PZH_ITEM zh_itemValToStr( PZH_ITEM pItem )
{
   PZH_ITEM pResult;
   char * buffer;
   ZH_SIZE nLen;
   ZH_BOOL bFreeReq;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemValToStr(%p)", ( void * ) pItem ) );

   buffer = zh_itemString( pItem, &nLen, &bFreeReq );
   if( bFreeReq )
      pResult = zh_itemPutCLPtr( NULL, buffer, nLen );
   else
      pResult = zh_itemPutCL( NULL, buffer, nLen );

   return pResult;
}
