/*
 * Calling function from dynamic library (zh_DynCall())
 *
 * Copyright 2009-2010 Viktor Szakats
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

#include "dyn.zhh"

ZH_FUNC( ZH_DYNCALL )
{
   PZH_ITEM pParam = zh_param( 1, ZH_IT_ARRAY );
   int * piArgFlags = NULL;
   int iFuncFlags = ZH_DYN_CALLCONV_CDECL;

   PZH_ITEM pLibraryHandle = NULL;
   ZH_BOOL bFreeLibrary = ZH_FALSE;
   void * pFunctionPtr = NULL;

   if( pParam )
   {
      if( ZH_IS_ARRAY( pParam ) )
      {
         ZH_SIZE nLen = zh_arrayLen( pParam );

         if( nLen >= 1 )
         {
            PZH_ITEM pFunction = zh_arrayGetItemPtr( pParam, 1 );
            ZH_SIZE nBasePos = 2;

            if( ZH_IS_STRING( pFunction ) && nLen >= nBasePos )
            {
               PZH_ITEM pLibrary = zh_arrayGetItemPtr( pParam, nBasePos );

               if( ZH_IS_STRING( pLibrary ) )
               {
                  pLibraryHandle = zh_libLoad( pLibrary, NULL );
                  if( pLibraryHandle )
                     bFreeLibrary = ZH_TRUE;
               }
               else if( zh_libHandle( pLibrary ) )
                  pLibraryHandle = pLibrary;

               if( pLibraryHandle )
                  pFunctionPtr = zh_libSymAddr( pLibraryHandle, zh_itemGetCPtr( pFunction ) );

               ++nBasePos;
            }

            /* Function flags */
            if( nBasePos <= nLen )
               iFuncFlags = zh_arrayGetNI( pParam, nBasePos );

            ++nBasePos;

            /* Argument flags */
            if( nBasePos <= nLen )
            {
               ZH_SIZE nPos;
               ZH_SIZE nArgCount = zh_pcount() - 1;

               piArgFlags = ( int * ) zh_xgrab( sizeof( int ) * nArgCount );

               for( nPos = 0; nPos < nArgCount; ++nPos )
                  piArgFlags[ nPos ] = ( ( nPos + nBasePos ) <= nLen && ZH_IS_NUMERIC( zh_arrayGetItemPtr( pParam, nPos + nBasePos ) ) ) ? zh_arrayGetNI( pParam, nPos + nBasePos ) : ZH_DYN_CTYPE_DEFAULT;
            }
         }
      }
   }

   zh_dynCall( iFuncFlags, pFunctionPtr, zh_pcount(), 2, piArgFlags );

   if( piArgFlags )
      zh_xfree( piArgFlags );

   if( bFreeLibrary )
      zh_libFree( pLibraryHandle );
}
