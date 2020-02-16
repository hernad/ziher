/*
 * Low-level ClipBoard code common to some GT drivers
 *
 * Copyright 2006 Przemyslaw Czerpak
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

/* NOTE: User programs should never call this layer directly! */

#include "zh_api.h"
#include "zh_gt_core.h"
#include "zh_string_api.h"
#include "zh_item_api.h"

#if defined( ZH_OS_WIN )
   #include <windows.h>
   /* For Global*() */
   #if defined( ZH_OS_WIN_CE )
      #include "hbwince.h"
   #endif

   #if defined( __CYGWIN__ )
      #include <wchar.h>
   #elif defined( __POCC__ ) && defined( ZH_OS_WIN_CE )
      #ifndef GMEM_MOVEABLE
         #define GMEM_MOVEABLE  2
      #endif
   #endif
#endif

#include "zh_thread.h"

static ZH_CRITICAL_NEW( s_clipMtx );

static char *  s_szClipboardData;
static ZH_SIZE s_nClipboardLen;

ZH_BOOL zh_gt_setClipboard( const char * szClipData, ZH_SIZE nLen )
{
   zh_threadEnterCriticalSection( &s_clipMtx );

   if( s_nClipboardLen )
      zh_xfree( s_szClipboardData );
   s_nClipboardLen = nLen;
   if( nLen )
   {
      s_szClipboardData = ( char * ) zh_xgrab( s_nClipboardLen + 1 );
      memcpy( s_szClipboardData, szClipData, s_nClipboardLen );
      s_szClipboardData[ s_nClipboardLen ] = '\0';
   }

   zh_threadLeaveCriticalSection( &s_clipMtx );

   return ZH_TRUE;
}

ZH_BOOL zh_gt_getClipboard( char ** pszClipData, ZH_SIZE * pnLen )
{
   zh_threadEnterCriticalSection( &s_clipMtx );

   *pszClipData = NULL;
   *pnLen = s_nClipboardLen;
   if( s_nClipboardLen )
   {
      *pszClipData = ( char * ) zh_xgrab( s_nClipboardLen + 1 );
      memcpy( *pszClipData, s_szClipboardData, s_nClipboardLen );
      ( *pszClipData )[ s_nClipboardLen ] = '\0';
   }

   zh_threadLeaveCriticalSection( &s_clipMtx );

   return s_nClipboardLen != 0;
}

#if defined( ZH_OS_WIN )

ZH_BOOL zh_gt_winapi_setClipboardRaw( ZH_UINT uFormat, void * pData, ZH_SIZE nSize )
{
   ZH_BOOL fResult = ZH_FALSE;

   if( OpenClipboard( NULL ) )
   {
      EmptyClipboard();

      if( nSize )
      {
         /* Allocate a global memory object for the text. */
         HGLOBAL hglb = GlobalAlloc( GMEM_MOVEABLE, nSize );
         if( hglb )
         {
            /* Lock the handle and copy the text to the buffer. */
            LPVOID lpMem = GlobalLock( hglb );

            if( lpMem )
            {
               memcpy( lpMem, pData, nSize );
               ( void ) GlobalUnlock( hglb );
               /* Place the handle on the clipboard. */
               fResult = SetClipboardData( ( UINT ) uFormat, hglb ) != 0;
            }
            if( ! fResult )
               GlobalFree( hglb );
         }
      }
      else
         fResult = ZH_TRUE;

      CloseClipboard();
   }
   return fResult;
}

ZH_BOOL zh_gt_winapi_setClipboard( ZH_UINT uFormat, PZH_ITEM pItem )
{
   ZH_BOOL fResult = ZH_FALSE;

   if( OpenClipboard( NULL ) )
   {
      ZH_SIZE nSize;

      EmptyClipboard();

      if( uFormat == CF_UNICODETEXT )
         nSize = zh_itemCopyStrU16( pItem, ZH_CODEPAGE_ENDIAN_NATIVE, NULL, 0 );
      else
         nSize = zh_itemCopyStr( pItem, zh_setGetOSCP(), NULL, 0 );

      if( nSize )
      {
         /* Allocate a global memory object for the text. */
         HGLOBAL hglb = GlobalAlloc( GMEM_MOVEABLE, ( nSize + 1 ) *
                                     ( uFormat == CF_UNICODETEXT ?
                                       sizeof( wchar_t ) : sizeof( char ) ) );
         if( hglb )
         {
            /* Lock the handle and copy the text to the buffer. */
            LPVOID lpMem = GlobalLock( hglb );

            if( lpMem )
            {
               if( uFormat == CF_UNICODETEXT )
                  zh_itemCopyStrU16( pItem, ZH_CODEPAGE_ENDIAN_NATIVE,
                                     ( wchar_t * ) lpMem, nSize + 1 );
               else
                  zh_itemCopyStr( pItem, zh_setGetOSCP(),
                                  ( char * ) lpMem, nSize + 1 );
               ( void ) GlobalUnlock( hglb );
               /* Place the handle on the clipboard. */
               fResult = SetClipboardData( ( UINT ) uFormat, hglb ) != 0;
            }
            if( ! fResult )
               GlobalFree( hglb );
         }
      }
      else
         fResult = ZH_TRUE;

      CloseClipboard();
   }
   return fResult;
}

ZH_BOOL zh_gt_winapi_getClipboard( ZH_UINT uFormat, PZH_ITEM pItem )
{
   ZH_SIZE nSize = 0;

   if( IsClipboardFormatAvailable( uFormat ) && OpenClipboard( NULL ) )
   {
      HGLOBAL hglb = GetClipboardData( ( UINT ) uFormat );
      if( hglb )
      {
         LPVOID lpMem = GlobalLock( hglb );
         if( lpMem )
         {
            nSize = ( ZH_SIZE ) GlobalSize( hglb );

            switch( uFormat )
            {
               case CF_UNICODETEXT:
                  nSize = zh_wstrnlen( ( const wchar_t * ) lpMem, nSize >> 1 );
                  if( nSize )
                     zh_itemPutStrLenU16( pItem, ZH_CODEPAGE_ENDIAN_NATIVE,
                                          ( const wchar_t * ) lpMem, nSize );
                  break;
               case CF_OEMTEXT:
               case CF_TEXT:
                  nSize = zh_strnlen( ( const char * ) lpMem, nSize );
                  /* fallthrough */
               default:
                  if( nSize )
                     zh_itemPutStrLen( pItem, uFormat == CF_TEXT ?
                                              zh_setGetOSCP() : NULL,
                                       ( const char * ) lpMem, nSize );
                  break;
            }
            ( void ) GlobalUnlock( hglb );
         }
      }
      CloseClipboard();
   }

   if( nSize == 0 )
      zh_itemPutC( pItem, NULL );

   return nSize != 0;
}

#endif /* ZH_OS_WIN */
