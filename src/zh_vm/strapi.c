/*
 * String API functions
 *
 * Copyright 2009 Przemyslaw Czerpak
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
#include "zh_string_api.h"
#include "zh_item_api.h"
#include "zh_stack.h"


static const ZH_WCHAR s_szConstStr[ 1 ] = { 0 };

ZH_SIZE zh_wstrlen( const ZH_WCHAR * szText )
{
   ZH_SIZE nLen = 0;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_wstrlen(%p)", ( const void * ) szText ) );

   if( szText )
   {
      while( szText[ nLen ] )
         ++nLen;
   }

   return nLen;
}

ZH_SIZE zh_wstrnlen( const ZH_WCHAR * szText, ZH_SIZE nCount )
{
   ZH_SIZE nLen = 0;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_wstrnlen(%p,%" ZH_PFS "u)", ( const void * ) szText, nCount ) );

   if( szText )
   {
      while( nCount-- && szText[ nLen ] )
         ++nLen;
   }

   return nLen;
}

int zh_wstrcmp( const ZH_WCHAR * s1, const ZH_WCHAR * s2 )
{
   int rc = 0;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_wstrcmp(%p, %p)", ( const void * ) s1, ( const void * ) s2 ) );

   for( ;; )
   {
      if( *s1 != *s2 )
      {
         rc = ( *s1 < *s2 ? -1 : 1 );
         break;
      }
      else if( *s1 == 0 )
         break;

      s1++;
      s2++;
   }

   return rc;
}

int zh_wstrncmp( const ZH_WCHAR * s1, const ZH_WCHAR * s2, ZH_SIZE nCount )
{
   int rc = 0;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_wstrncmp(%p, %p, %" ZH_PFS "u)", ( const void * ) s1, ( const void * ) s2, nCount ) );

   while( nCount-- )
   {
      if( *s1 != *s2 )
      {
         rc = ( *s1 < *s2 ? -1 : 1 );
         break;
      }
      else if( *s1 == 0 )
         break;

      s1++;
      s2++;
   }

   return rc;
}

ZH_WCHAR * zh_wstrncpy( ZH_WCHAR * pDest, const ZH_WCHAR * pSource, ZH_SIZE nLen )
{
   ZH_WCHAR * pBuf = pDest;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_wstrncpy(%p, %p, %" ZH_PFS "u)", ( void * ) pDest, ( const void * ) pSource, nLen ) );

   pDest[ nLen ] = '\0';

   while( nLen && ( *pDest++ = *pSource++ ) != '\0' )
      nLen--;

   return pBuf;
}

ZH_WCHAR * zh_wstrncat( ZH_WCHAR * pDest, const ZH_WCHAR * pSource, ZH_SIZE nLen )
{
   ZH_WCHAR * pBuf = pDest;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_strncat(%p, %p, %" ZH_PFS "u)", ( void * ) pDest, ( const void * ) pSource, nLen ) );

   pDest[ nLen ] = '\0';

   while( nLen && *pDest )
   {
      pDest++;
      nLen--;
   }

   while( nLen && ( *pDest++ = *pSource++ ) != '\0' )
      nLen--;

   return pBuf;
}

ZH_WCHAR * zh_wstrdup( const ZH_WCHAR * szText )
{
   ZH_WCHAR * pszDest;
   ZH_SIZE nSize;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_wstrdup(%p)", ( const void * ) szText ) );

   nSize = ( zh_wstrlen( szText ) + 1 ) * sizeof( ZH_WCHAR );
   pszDest = ( ZH_WCHAR * ) zh_xgrab( nSize );

   memcpy( pszDest, szText, nSize );

   return pszDest;
}

ZH_WCHAR * zh_wstrndup( const ZH_WCHAR * szText, ZH_SIZE nLen )
{
   ZH_WCHAR * pszDest;
   ZH_SIZE nSize;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_wstrndup(%p,%" ZH_PFS "u)", ( const void * ) szText, nLen ) );

   nSize = zh_wstrlen( szText );
   if( nSize < nLen )
      nLen = nSize;
   nSize = nLen * sizeof( ZH_WCHAR );
   pszDest = ( ZH_WCHAR * ) zh_xgrab( nSize + sizeof( ZH_WCHAR ) );
   memcpy( pszDest, szText, nSize );
   pszDest[ nLen ] = 0;

   return pszDest;
}

ZH_WCHAR * zh_wstrunshare( void ** phStr, const ZH_WCHAR * pStr, ZH_SIZE nLen )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_wstrunshare(%p,%p,%" ZH_PFS "u)", ( void * ) phStr, ( const void * ) pStr, nLen ) );

   if( pStr == NULL || phStr == NULL || *phStr == NULL )
      return NULL;

   if( nLen > 0 &&
       ( *phStr == ( const void * ) s_szConstStr || zh_xRefCount( *phStr ) > 1 ) )
   {
      ZH_WCHAR * pszDest = ( ZH_WCHAR * ) zh_xgrab( ( nLen + 1 ) *
                                                    sizeof( ZH_WCHAR ) );
      memcpy( pszDest, pStr, nLen * sizeof( ZH_WCHAR ) );
      pszDest[ nLen ] = 0;
      if( *phStr != ( const void * ) s_szConstStr )
         zh_xRefDec( *phStr );
      *phStr = ( void * ) pszDest;

      return pszDest;
   }

   return ( ZH_WCHAR * ) ZH_UNCONST( pStr );
}

char * zh_strunshare( void ** phStr, const char * pStr, ZH_SIZE nLen )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_strunshare(%p,%p,%" ZH_PFS "u)", ( void * ) phStr, ( const void * ) pStr, nLen ) );

   if( pStr == NULL || phStr == NULL || *phStr == NULL )
      return NULL;

   if( nLen > 0 &&
       ( *phStr == ( const void * ) s_szConstStr || zh_xRefCount( *phStr ) > 1 ) )
   {
      char * pszDest = ( char * ) zh_xgrab( ( nLen + 1 ) * sizeof( char ) );
      memcpy( pszDest, pStr, nLen * sizeof( char ) );
      pszDest[ nLen ] = 0;
      if( *phStr != ( const void * ) s_szConstStr )
         zh_xRefDec( *phStr );
      *phStr = ( void * ) pszDest;

      return pszDest;
   }

   return ( char * ) ZH_UNCONST( pStr );
}

const char * zh_strnull( const char * str )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_strnull(%p)", ( const void * ) str ) );

   return str ? str : "";
}

const ZH_WCHAR * zh_wstrnull( const ZH_WCHAR * str )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_wstrnull(%p)", ( const void * ) str ) );

   return str ? str : s_szConstStr;
}

void zh_strfree( void * hString )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_strfree(%p)", hString ) );

   if( hString && hString != ( const void * ) s_szConstStr )
      zh_xRefFree( hString );
}




const char * zh_itemGetStr( PZH_ITEM pItem, void * cdp, void ** phString, ZH_SIZE * pnLen )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemGetStr(%p,%p,%p,%p)", ( void * ) pItem, cdp, ( void * ) phString, ( void * ) pnLen ) );

   if( pItem && ZH_IS_STRING( pItem ) )
   {
      const char * pString;
      char * pFree = NULL;
      ZH_SIZE nSize = 0;

      pString = zh_cdpnDup3( pItem->item.asString.value,
                             pItem->item.asString.length,
                             NULL, pnLen, &pFree, &nSize,
                             zh_vmCDP(), ( PZH_CODEPAGE ) cdp );
      if( pFree != NULL )
         *phString = ( void * ) pFree;
      else if( pItem->item.asString.allocated == 0 )
         *phString = ZH_UNCONST( s_szConstStr );
      else
      {
         *phString = ( void * ) pItem->item.asString.value;
         zh_xRefInc( pItem->item.asString.value );
      }
      return pString;
   }

   if( pnLen )
      *pnLen = 0;
   *phString = NULL;

   return NULL;
}

const char * zh_itemGetStrUTF8( PZH_ITEM pItem, void ** phString, ZH_SIZE * pnLen )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemGetStrUTF8(%p,%p,%p)", ( void * ) pItem, ( void * ) phString, ( void * ) pnLen ) );

   if( pItem && ZH_IS_STRING( pItem ) )
   {
      PZH_CODEPAGE cdp = zh_vmCDP();
      ZH_SIZE nLen = zh_cdpStrAsUTF8Len( cdp,
                                         pItem->item.asString.value,
                                         pItem->item.asString.length, 0 );
      if( pnLen )
         *pnLen = nLen;

      if( nLen != pItem->item.asString.length )
      {
         char * pszUtf8 = ( char * ) zh_xgrab( nLen + 1 );
         zh_cdpStrToUTF8( cdp,
                          pItem->item.asString.value, pItem->item.asString.length,
                          pszUtf8, nLen + 1 );
         *phString = ( void * ) pszUtf8;
         return pszUtf8;
      }

      if( pItem->item.asString.allocated != 0 )
      {
         *phString = ( void * ) pItem->item.asString.value;
         zh_xRefInc( pItem->item.asString.value );
      }
      else
         *phString = ZH_UNCONST( s_szConstStr );
      return pItem->item.asString.value;
   }

   if( pnLen )
      *pnLen = 0;
   *phString = NULL;

   return NULL;
}

const ZH_WCHAR * zh_itemGetStrU16( PZH_ITEM pItem, int iEndian,
                                   void ** phString, ZH_SIZE * pnLen )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemGetStrU16(%p,%d,%p,%p)", ( void * ) pItem, iEndian, ( void * ) phString, ( void * ) pnLen ) );

   if( pItem && ZH_IS_STRING( pItem ) )
   {
      ZH_WCHAR * pszU16;
      PZH_CODEPAGE cdp = zh_vmCDP();
      ZH_SIZE nLen = zh_cdpStrAsU16Len( cdp, pItem->item.asString.value,
                                             pItem->item.asString.length, 0 );
      if( pnLen )
         *pnLen = nLen;

      if( nLen == 0 )
      {
         *phString = ZH_UNCONST( s_szConstStr );
         return s_szConstStr;
      }

      pszU16 = ( ZH_WCHAR * ) zh_xgrab( ( nLen + 1 ) * sizeof( ZH_WCHAR ) );
      zh_cdpStrToU16( cdp, iEndian,
                      pItem->item.asString.value, pItem->item.asString.length,
                      pszU16, nLen + 1 );

      *phString = ( void * ) pszU16;
      return pszU16;
   }

   if( pnLen )
      *pnLen = 0;
   *phString = NULL;

   return NULL;
}


ZH_SIZE zh_itemCopyStr( PZH_ITEM pItem, void * cdp, char * pStrBuffer, ZH_SIZE nSize )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemCopyStr(%p,%p,%p,%" ZH_PFS "u)", ( void * ) pItem, cdp, ( void * ) pStrBuffer, nSize ) );

   if( pItem && ZH_IS_STRING( pItem ) )
   {
      if( pStrBuffer )
         return zh_cdpTransTo( pItem->item.asString.value,
                               pItem->item.asString.length,
                               pStrBuffer, nSize,
                               zh_vmCDP(), ( PZH_CODEPAGE ) cdp );
      else
         return zh_cdpnDup2Len( pItem->item.asString.value,
                                pItem->item.asString.length,
                                nSize, zh_vmCDP(), ( PZH_CODEPAGE ) cdp );
   }
   else if( pStrBuffer && nSize )
      pStrBuffer[ 0 ] = '\0';

   return 0;
}

ZH_SIZE zh_itemCopyStrUTF8( PZH_ITEM pItem, char * pStrBuffer, ZH_SIZE nSize )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemCopyStrUTF8(%p,%p,%" ZH_PFS "u)", ( void * ) pItem, ( void * ) pStrBuffer, nSize ) );

   if( pItem && ZH_IS_STRING( pItem ) )
   {
      if( pStrBuffer )
         nSize = zh_cdpStrToUTF8( zh_vmCDP(),
                                  pItem->item.asString.value,
                                  pItem->item.asString.length,
                                  pStrBuffer, nSize );
      else
         nSize = zh_cdpStrAsUTF8Len( zh_vmCDP(),
                                     pItem->item.asString.value,
                                     pItem->item.asString.length, nSize );
      return nSize;
   }
   else if( pStrBuffer && nSize )
      pStrBuffer[ 0 ] = '\0';

   return 0;
}


ZH_SIZE zh_itemCopyStrU16( PZH_ITEM pItem, int iEndian, ZH_WCHAR * pStrBuffer, ZH_SIZE nSize )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemCopyStrU16(%p,%d,%p,%" ZH_PFS "u)", ( void * ) pItem, iEndian, ( void * ) pStrBuffer, nSize ) );

   if( pItem && ZH_IS_STRING( pItem ) )
   {
      if( pStrBuffer )
         nSize = zh_cdpStrToU16( zh_vmCDP(), iEndian,
                                 pItem->item.asString.value,
                                 pItem->item.asString.length,
                                 pStrBuffer, nSize );
      else
         nSize = zh_cdpStrAsU16Len( zh_vmCDP(),
                                    pItem->item.asString.value,
                                    pItem->item.asString.length, nSize );
      return nSize;
   }
   else if( pStrBuffer && nSize )
      pStrBuffer[ 0 ] = '\0';

   return 0;
}


PZH_ITEM zh_itemPutStrLen( PZH_ITEM pItem, void * cdp, const char * pStr, ZH_SIZE nLen )
{
   char * pszText;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemPutStrLen(%p,%p,%p,%" ZH_PFS "u)", ( void * ) pItem, cdp, ( const void * ) pStr, nLen ) );

   if( nLen == 0 )
      return zh_itemPutC( pItem, NULL );

   pszText = zh_cdpnDup( pStr, &nLen, ( PZH_CODEPAGE ) cdp, zh_vmCDP() );

   return zh_itemPutCLPtr( pItem, pszText, nLen );
}

PZH_ITEM zh_itemPutStrLenUTF8( PZH_ITEM pItem, const char * pStr, ZH_SIZE nLen )
{
   PZH_CODEPAGE cdp;
   char * pszDest;
   ZH_SIZE nDest;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemPutStrLenUTF8(%p,%p,%" ZH_PFS "u)", ( void * ) pItem, ( const void * ) pStr, nLen ) );

   if( nLen == 0 )
      return zh_itemPutC( pItem, NULL );

   cdp = zh_vmCDP();
   nDest = zh_cdpUTF8AsStrLen( cdp, pStr, nLen, 0 );
   pszDest = ( char * ) zh_xgrab( nDest + 1 );
   zh_cdpUTF8ToStr( cdp, pStr, nLen, pszDest, nDest + 1 );

   return zh_itemPutCLPtr( pItem, pszDest, nDest );
}

PZH_ITEM zh_itemPutStrLenU16( PZH_ITEM pItem, int iEndian, const ZH_WCHAR * pStr, ZH_SIZE nLen )
{
   PZH_CODEPAGE cdp;
   char * pszDest;
   ZH_SIZE nDest;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemPutStrLenU16(%p,%d,%p,%" ZH_PFS "u)", ( void * ) pItem, iEndian, ( const void * ) pStr, nLen ) );

   if( nLen == 0 )
      return zh_itemPutC( pItem, NULL );

   cdp = zh_vmCDP();
   nDest = zh_cdpU16AsStrLen( cdp, pStr, nLen, 0 );
   pszDest = ( char * ) zh_xgrab( nDest + 1 );
   zh_cdpU16ToStr( cdp, iEndian, pStr, nLen, pszDest, nDest + 1 );

   return zh_itemPutCLPtr( pItem, pszDest, nDest );
}


PZH_ITEM zh_itemPutStr( PZH_ITEM pItem, void * cdp, const char * pStr )
{
   char * pszText;
   ZH_SIZE nLen;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemPutStr(%p,%p,%p)", ( void * ) pItem, cdp, ( const void * ) pStr ) );

   if( pStr == NULL )
      return zh_itemPutC( pItem, NULL );

   nLen = strlen( pStr );
   pszText = zh_cdpnDup( pStr, &nLen, ( PZH_CODEPAGE ) cdp, zh_vmCDP() );

   return zh_itemPutCLPtr( pItem, pszText, nLen );
}

PZH_ITEM zh_itemPutStrUTF8( PZH_ITEM pItem, const char * pStr )
{
   PZH_CODEPAGE cdp;
   char * pszDest;
   ZH_SIZE nDest, nLen;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemPutStrUTF8(%p,%p)", ( void * ) pItem, ( const void * ) pStr ) );

   if( pStr == NULL )
      return zh_itemPutC( pItem, NULL );

   cdp = zh_vmCDP();
   nLen = strlen( pStr );
   nDest = zh_cdpUTF8AsStrLen( cdp, pStr, nLen, 0 );
   pszDest = ( char * ) zh_xgrab( nDest + 1 );
   zh_cdpUTF8ToStr( cdp, pStr, nLen, pszDest, nDest + 1 );

   return zh_itemPutCLPtr( pItem, pszDest, nDest );
}

PZH_ITEM zh_itemPutStrU16( PZH_ITEM pItem, int iEndian, const ZH_WCHAR * pStr )
{
   PZH_CODEPAGE cdp;
   char * pszDest;
   ZH_SIZE nDest, nLen;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_itemPutStrU16(%p,%d,%p)", ( void * ) pItem, iEndian, ( const void * ) pStr ) );

   if( pStr == NULL )
      return zh_itemPutC( pItem, NULL );

   cdp = zh_vmCDP();
   nLen = zh_wstrlen( pStr );
   nDest = zh_cdpU16AsStrLen( cdp, pStr, nLen, 0 );
   pszDest = ( char * ) zh_xgrab( nDest + 1 );
   zh_cdpU16ToStr( cdp, iEndian, pStr, nLen, pszDest, nDest + 1 );

   return zh_itemPutCLPtr( pItem, pszDest, nDest );
}




const char * zh_arrayGetStr( PZH_ITEM pArray, ZH_SIZE nIndex, void * cdp,
                             void ** phString, ZH_SIZE * pnLen )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayGetStr(%p, %" ZH_PFS "u, %p, %p, %p)", ( void * ) pArray, nIndex, cdp, ( void * ) phString, ( void * ) pnLen ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return zh_itemGetStr( pArray->item.asArray.value->pItems + nIndex - 1,
                            cdp, phString, pnLen );
   if( pnLen )
      *pnLen = 0;
   *phString = NULL;

   return NULL;
}

const char * zh_arrayGetStrUTF8( PZH_ITEM pArray, ZH_SIZE nIndex,
                                 void ** phString, ZH_SIZE * pnLen )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayGetStrUTF8(%p, %" ZH_PFS "u, %p, %p)", ( void * ) pArray, nIndex, ( void * ) phString, ( void * ) pnLen ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return zh_itemGetStrUTF8( pArray->item.asArray.value->pItems + nIndex - 1,
                                phString, pnLen );
   if( pnLen )
      *pnLen = 0;
   *phString = NULL;

   return NULL;
}

const ZH_WCHAR * zh_arrayGetStrU16( PZH_ITEM pArray, ZH_SIZE nIndex, int iEndian,
                                    void ** phString, ZH_SIZE * pnLen )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arrayGetStrU16(%p, %" ZH_PFS "u, %d, %p, %p)", ( void * ) pArray, nIndex, iEndian, ( void * ) phString, ( void * ) pnLen ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return zh_itemGetStrU16( pArray->item.asArray.value->pItems + nIndex - 1,
                               iEndian, phString, pnLen );
   if( pnLen )
      *pnLen = 0;
   *phString = NULL;

   return NULL;
}


ZH_BOOL zh_arraySetStrLen( PZH_ITEM pArray, ZH_SIZE nIndex, void * cdp,
                           const char * pStr, ZH_SIZE nLen )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arraySetStrLen(%p, %" ZH_PFS "u, %p, %p, %" ZH_PFS "u)", ( void * ) pArray, nIndex, cdp, ( const void * ) pStr, nLen ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      zh_itemPutStrLen( pArray->item.asArray.value->pItems + nIndex - 1, cdp,
                        pStr, nLen );
      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}

ZH_BOOL zh_arraySetStrLenUTF8( PZH_ITEM pArray, ZH_SIZE nIndex,
                               const char * pStr, ZH_SIZE nLen )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arraySetStrLenUTF8(%p, %" ZH_PFS "u, %p, %" ZH_PFS "u)", ( void * ) pArray, nIndex, ( const void * ) pStr, nLen ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      zh_itemPutStrLenUTF8( pArray->item.asArray.value->pItems + nIndex - 1,
                            pStr, nLen );
      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}

ZH_BOOL zh_arraySetStrLenU16( PZH_ITEM pArray, ZH_SIZE nIndex, int iEndian,
                              const ZH_WCHAR * pStr, ZH_SIZE nLen )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arraySetStrLenU16(%p, %" ZH_PFS "u, %d, %p, %" ZH_PFS "u)", ( void * ) pArray, nIndex, iEndian, ( const void * ) pStr, nLen ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      zh_itemPutStrLenU16( pArray->item.asArray.value->pItems + nIndex - 1,
                           iEndian, pStr, nLen );
      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}


ZH_BOOL zh_arraySetStr( PZH_ITEM pArray, ZH_SIZE nIndex, void * cdp, const char * pStr )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arraySetStr(%p, %" ZH_PFS "u, %p, %p)", ( void * ) pArray, nIndex, cdp, ( const void * ) pStr ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      zh_itemPutStr( pArray->item.asArray.value->pItems + nIndex - 1, cdp, pStr );
      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}

ZH_BOOL zh_arraySetStrUTF8( PZH_ITEM pArray, ZH_SIZE nIndex, const char * pStr )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arraySetStrUTF8(%p, %" ZH_PFS "u, %p)", ( void * ) pArray, nIndex, ( const void * ) pStr ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      zh_itemPutStrUTF8( pArray->item.asArray.value->pItems + nIndex - 1, pStr );
      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}

ZH_BOOL zh_arraySetStrU16( PZH_ITEM pArray, ZH_SIZE nIndex, int iEndian, const ZH_WCHAR * pStr )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_arraySetStrU16(%p, %" ZH_PFS "u, %d, %p)", ( void * ) pArray, nIndex, iEndian, ( const void * ) pStr ) );

   if( ZH_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      zh_itemPutStrU16( pArray->item.asArray.value->pItems + nIndex - 1, iEndian, pStr );
      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}




const char * zh_parstr( int iParam, void * cdp, void ** phString, ZH_SIZE * pnLen )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parstr(%d,%p,%p,%p)", iParam, cdp, ( void * ) phString, ( void * ) pnLen ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      return zh_itemGetStr( pItem, cdp, phString, pnLen );
   }

   if( pnLen )
      *pnLen = 0;
   *phString = NULL;

   return NULL;
}

const char * zh_parstr_utf8( int iParam, void ** phString, ZH_SIZE * pnLen )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parstr_utf8(%d,%p,%p)", iParam, ( void * ) phString, ( void * ) pnLen ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      return zh_itemGetStrUTF8( pItem, phString, pnLen );
   }

   if( pnLen )
      *pnLen = 0;
   *phString = NULL;

   return NULL;
}

const ZH_WCHAR * zh_parstr_u16( int iParam, int iEndian,
                                void ** phString, ZH_SIZE * pnLen )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parstr_u16(%d,%d,%p,%p)", iParam, iEndian, ( void * ) phString, ( void * ) pnLen ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      return zh_itemGetStrU16( pItem, iEndian, phString, pnLen );
   }

   if( pnLen )
      *pnLen = 0;
   *phString = NULL;

   return NULL;
}


const char * zh_parastr( int iParam, ZH_SIZE nIndex,
                         void * cdp, void ** phString, ZH_SIZE * pnLen )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parastr(%d,%" ZH_PFS "u,%p,%p,%p)", iParam, nIndex, cdp, ( void * ) phString, ( void * ) pnLen ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_ARRAY( pItem ) )
         return zh_arrayGetStr( pItem, nIndex, cdp, phString, pnLen );
      else
         return zh_itemGetStr( pItem, cdp, phString, pnLen );
   }

   if( pnLen )
      *pnLen = 0;
   *phString = NULL;

   return NULL;
}

const char * zh_parastr_utf8( int iParam, ZH_SIZE nIndex,
                              void ** phString, ZH_SIZE * pnLen )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parastr_utf8(%d,%" ZH_PFS "u,%p,%p)", iParam, nIndex, ( void * ) phString, ( void * ) pnLen ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_ARRAY( pItem ) )
         return zh_arrayGetStrUTF8( pItem, nIndex, phString, pnLen );
      else
         return zh_itemGetStrUTF8( pItem, phString, pnLen );
   }

   if( pnLen )
      *pnLen = 0;
   *phString = NULL;

   return NULL;
}

const ZH_WCHAR * zh_parastr_u16( int iParam, ZH_SIZE nIndex, int iEndian,
                                 void ** phString, ZH_SIZE * pnLen )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_parastr_u16(%d,%" ZH_PFS "u,%d,%p,%p)", iParam, nIndex, iEndian, ( void * ) phString, ( void * ) pnLen ) );

   if( iParam >= -1 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = ( iParam == -1 ) ? zh_stackReturnItem() : zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
         pItem = zh_itemUnRef( pItem );

      if( ZH_IS_ARRAY( pItem ) )
         return zh_arrayGetStrU16( pItem, nIndex, iEndian, phString, pnLen );
      else
         return zh_itemGetStrU16( pItem, iEndian, phString, pnLen );
   }

   if( pnLen )
      *pnLen = 0;
   *phString = NULL;

   return NULL;
}


void zh_retstr( void * cdp, const char * szText )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_retstr(%p,%s)", cdp, szText ) );

   zh_itemPutStrLen( zh_stackReturnItem(), cdp, szText,
                     szText ? strlen( szText ) : 0 );
}

void zh_retstr_utf8( const char * szText )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_retstr_utf8(%s)", szText ) );

   zh_itemPutStrLenUTF8( zh_stackReturnItem(), szText,
                         szText ? strlen( szText ) : 0 );
}

void zh_retstr_u16( int iEndian, const ZH_WCHAR * szText )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_retstr_u16(%d,%p)", iEndian, ( const void * ) szText ) );

   zh_itemPutStrLenU16( zh_stackReturnItem(), iEndian, szText,
                        zh_wstrlen( szText ) );
}


void zh_retstrlen( void * cdp, const char * szText, ZH_SIZE nLen )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_retstrlen(%p,%s,%" ZH_PFS "u)", cdp, ( const void * ) szText, nLen ) );

   zh_itemPutStrLen( zh_stackReturnItem(), cdp, szText, nLen );
}

void zh_retstrlen_utf8( const char * szText, ZH_SIZE nLen )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_retstrlen_utf8(%s,%" ZH_PFS "u)", szText, nLen ) );

   zh_itemPutStrLenUTF8( zh_stackReturnItem(), szText, nLen );
}

void zh_retstrlen_u16( int iEndian, const ZH_WCHAR * szText, ZH_SIZE nLen )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_retstrlen_u16(%d,%p,%" ZH_PFS "u)", iEndian, ( const void * ) szText, nLen ) );

   zh_itemPutStrLenU16( zh_stackReturnItem(), iEndian, szText, nLen );
}


int zh_storstr( void * cdp, const char * szText, int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_storstr(%p,%s,%d)", cdp, ( const void * ) szText, iParam ) );

   if( iParam == -1 )
   {
      zh_itemPutStrLen( zh_stackReturnItem(), cdp, szText,
                        szText ? strlen( szText ) : 0 );
      return 1;
   }
   else if( iParam >= 0 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
      {
         zh_itemPutStrLen( zh_itemUnRef( pItem ), cdp, szText,
                           szText ? strlen( szText ) : 0 );
         return 1;
      }
   }

   return 0;
}

int zh_storstr_utf8( const char * szText, int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_storstr_utf8(%s,%d)", szText, iParam ) );

   if( iParam == -1 )
   {
      zh_itemPutStrLenUTF8( zh_stackReturnItem(), szText,
                            szText ? strlen( szText ) : 0 );
      return 1;
   }
   else if( iParam >= 0 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
      {
         zh_itemPutStrLenUTF8( zh_itemUnRef( pItem ), szText,
                               szText ? strlen( szText ) : 0 );
         return 1;
      }
   }

   return 0;
}

int zh_storstr_u16( int iEndian, const ZH_WCHAR * szText, int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_storstr_u16(%d,%p,%d)", iEndian, ( const void * ) szText, iParam ) );

   if( iParam == -1 )
   {
      zh_itemPutStrLenU16( zh_stackReturnItem(), iEndian,
                           szText, zh_wstrlen( szText ) );
      return 1;
   }
   else if( iParam >= 0 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
      {
         zh_itemPutStrLenU16( zh_itemUnRef( pItem ), iEndian,
                              szText, zh_wstrlen( szText ) );
         return 1;
      }
   }

   return 0;
}


int zh_storstrlen( void * cdp, const char * szText, ZH_SIZE nLen, int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_storstrlen(%p,%s,%" ZH_PFS "u,%d)", cdp, ( const void * ) szText, nLen, iParam ) );

   if( iParam == -1 )
   {
      zh_itemPutStrLen( zh_stackReturnItem(), cdp, szText, nLen );
      return 1;
   }
   else if( iParam >= 0 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
      {
         zh_itemPutStrLen( zh_itemUnRef( pItem ), cdp, szText, nLen );
         return 1;
      }
   }

   return 0;
}

int zh_storstrlen_utf8( const char * szText, ZH_SIZE nLen, int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_storstrlen_utf8(%s,%" ZH_PFS "u,%d)", szText, nLen, iParam ) );

   if( iParam == -1 )
   {
      zh_itemPutStrLenUTF8( zh_stackReturnItem(), szText, nLen );
      return 1;
   }
   else if( iParam >= 0 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
      {
         zh_itemPutStrLenUTF8( zh_itemUnRef( pItem ), szText, nLen );
         return 1;
      }
   }

   return 0;
}

int zh_storstrlen_u16( int iEndian, const ZH_WCHAR * szText, ZH_SIZE nLen, int iParam )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_storstrlen_u16(%d,%p,%" ZH_PFS "u,%d)", iEndian, ( const void * ) szText, nLen, iParam ) );

   if( iParam == -1 )
   {
      zh_itemPutStrLenU16( zh_stackReturnItem(), iEndian, szText, nLen );
      return 1;
   }
   else if( iParam >= 0 && iParam <= zh_pcount() )
   {
      PZH_ITEM pItem = zh_stackItemFromBase( iParam );

      if( ZH_IS_BYREF( pItem ) )
      {
         zh_itemPutStrLenU16( zh_itemUnRef( pItem ), iEndian, szText, nLen );
         return 1;
      }
   }

   return 0;
}
