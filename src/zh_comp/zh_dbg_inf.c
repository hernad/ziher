/*
 * Generate line information for debugger
 *
 * Copyright 2006 Przemyslaw Czerpak < druzus /at/ priv.onet.pl >
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

#include "zh_comp.h"

PZH_DEBUGINFO zh_compGetDebugInfo( ZH_COMP_DECL )
{
   PZH_DEBUGINFO pLineInfo = NULL, pInfo = NULL;
   ZH_SIZE nPos, nSkip, nOffset;
   ZH_ULONG ulLine;
   const char * pszModuleName = "", * ptr;
   PZH_ZFUNC pFunc;

   pFunc = ZH_COMP_PARAM->functions.pFirst;

   while( pFunc )
   {
      if( ( pFunc->funFlags & ZH_FUNF_FILE_DECL ) == 0 )
      {
         nPos = ulLine = 0;
         while( nPos < pFunc->nPCodePos )
         {
            nSkip = 0;
            switch( pFunc->pCode[ nPos ] )
            {
               case ZH_P_LINE:
                  ulLine = ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPos + 1 ] );
                  break;

               case ZH_P_MODULENAME:
                  pszModuleName = ( const char * ) &pFunc->pCode[ nPos + 1 ];
                  pInfo = NULL;
                  break;

               /*
                * This enables checking also code block bodies,
                * if it's not necessary then simply remove the
                * code below. [druzus]
                */
               case ZH_P_PUSH_BLOCKLARGE:
                  nSkip = 8 + ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPos + 6 ] ) * 2;
                  break;

               case ZH_P_PUSH_BLOCK:
                  nSkip = 7 + ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPos + 5 ] ) * 2;
                  break;

               case ZH_P_PUSH_BLOCKSHORT:
                  nSkip = 2;
                  break;
            }

            if( ulLine != 0 )
            {
               if( ! pInfo )
               {
                  int i;

                  ptr = strrchr( pszModuleName, ':' );
                  i = ptr ? ( int ) ( ptr - pszModuleName ) : ( int ) strlen( pszModuleName );

                  pInfo = pLineInfo;
                  while( pInfo != NULL )
                  {
                     if( strncmp( pszModuleName, pInfo->pszModuleName, i ) == 0 &&
                         ( pInfo->pszModuleName[ i ] == '\0' ||
                           pInfo->pszModuleName[ i ] == ':' ) )
                        break;
                     pInfo = pInfo->pNext;
                  }
                  if( ! pInfo )
                  {
                     pInfo = ( PZH_DEBUGINFO ) zh_xgrab( sizeof( ZH_DEBUGINFO ) );
                     pInfo->pszModuleName = zh_strndup( pszModuleName, i );
                     pInfo->ulFirstLine = pInfo->ulLastLine = ulLine;
                     /*
                      * allocate memory in 256 bytes chunks (for 2048 lines)
                      * The last 1 byte is reserved for additional 0 byte if
                      * the caller will want to use the returned buffer as
                      * parameter to zh_compGenPushString(). [druzus]
                      */
                     pInfo->ulAllocated = ( ( ulLine >> 3 ) + 0x100 ) & 0xFFFFFF00L;
                     pInfo->pLineMap = ( ZH_BYTE * ) zh_xgrabz( pInfo->ulAllocated + 1 );
                     pInfo->pNext = pLineInfo;
                     pLineInfo = pInfo;
                  }
               }
               nOffset = ulLine >> 3;
               if( pInfo->ulAllocated <= nOffset )
               {
                  ZH_ULONG ulNewSize = ( ( ulLine >> 3 ) + 0x100 ) & 0xFFFFFF00L;
                  pInfo->pLineMap = ( ZH_BYTE * ) zh_xrealloc( pInfo->pLineMap, ulNewSize + 1 );
                  memset( pInfo->pLineMap + pInfo->ulAllocated, 0, ulNewSize - pInfo->ulAllocated + 1 );
                  pInfo->ulAllocated = ulNewSize;
               }
               pInfo->pLineMap[ nOffset ] |= 1 << ( ulLine & 0x7 );
               /*
                * It's possible the the line number will be ascending
                * if some external file is included more then once. [druzus]
                */
               if( pInfo->ulFirstLine > ulLine )
                  pInfo->ulFirstLine = ulLine;
               if( pInfo->ulLastLine < ulLine )
                  pInfo->ulLastLine = ulLine;
               ulLine = 0;
            }

            if( nSkip == 0 )
            {
               nSkip = zh_compPCodeSize( pFunc, nPos );
               if( nSkip == 0 )
                  break;
            }
            nPos += nSkip;
         }
      }
      pFunc = pFunc->pNext;
   }

   return pLineInfo;
}
