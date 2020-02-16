/*
 * Dynamic reference to ZLIB functions
 *
 * Copyright 2013 Przemyslaw Czerpak
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

#define _ZH_ZLIB_INTERNAL_

#include "zh_api.h"
#include "zh_zlib.h"

static ZH_SIZE s_zlibCompressBound( ZH_SIZE nLen )
{
   ZH_SYMBOL_UNUSED( nLen );
   return 0;
}

static ZH_SIZE s_zlibUncompressedSize( const char * pSrc, ZH_SIZE nLen, int * piResult )
{
   ZH_SYMBOL_UNUSED( pSrc );
   ZH_SYMBOL_UNUSED( nLen );
   ZH_SYMBOL_UNUSED( piResult );
   return 0;
}

static int s_zlibCompress( char * pDst, ZH_SIZE * pnDst, const char * pSrc, ZH_SIZE nLen, int iLevel )
{
   ZH_SYMBOL_UNUSED( pDst );
   ZH_SYMBOL_UNUSED( pnDst );
   ZH_SYMBOL_UNUSED( pSrc );
   ZH_SYMBOL_UNUSED( nLen );
   ZH_SYMBOL_UNUSED( iLevel );

   return ZH_ZLIB_RES_UNSUPPORTED;
}

static int s_zlibUncompress( char * pDst, ZH_SIZE * pnDst, const char * pSrc, ZH_SIZE nLen )
{
   ZH_SYMBOL_UNUSED( pDst );
   ZH_SYMBOL_UNUSED( pnDst );
   ZH_SYMBOL_UNUSED( pSrc );
   ZH_SYMBOL_UNUSED( nLen );

   return ZH_ZLIB_RES_UNSUPPORTED;
}

static ZH_ZLIB_CBOUND s_compressBound    = s_zlibCompressBound;
static ZH_ZLIB_UNSIZE s_uncompressedSize = s_zlibUncompressedSize;
static ZH_ZLIB_COMPRS s_compress         = s_zlibCompress;
static ZH_ZLIB_UNCMPS s_uncompress       = s_zlibUncompress;

void zh_zlibInit( ZH_ZLIB_CBOUND pBound, ZH_ZLIB_UNSIZE pUnSize,
                  ZH_ZLIB_COMPRS pCompress, ZH_ZLIB_UNCMPS pUncompress )
{
   s_compressBound    = pBound;
   s_uncompressedSize = pUnSize;
   s_compress         = pCompress;
   s_uncompress       = pUncompress;
}

ZH_SIZE zh_zlibCompressBound( ZH_SIZE nLen )
{
   return s_compressBound( nLen );
}

ZH_SIZE zh_zlibUncompressedSize( const char * pSrc, ZH_SIZE nLen, int * piResult )
{
   return s_uncompressedSize( pSrc, nLen, piResult );
}

int zh_zlibCompress( char * pDst, ZH_SIZE * pnDst, const char * pSrc, ZH_SIZE nLen, int iLevel )
{
   return s_compress( pDst, pnDst, pSrc, nLen, iLevel );
}

int zh_zlibUncompress( char * pDst, ZH_SIZE * pnDst, const char * pSrc, ZH_SIZE nLen )
{
   return s_uncompress( pDst, pnDst, pSrc, nLen );
}
