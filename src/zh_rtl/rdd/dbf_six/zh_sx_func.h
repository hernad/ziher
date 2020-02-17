/*
 * Header file for SIX compatible functions
 *
 * Copyright 2005 Przemyslaw Czerpak <druzus@acn.waw.pl>
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

#ifndef ZH_SXFUNC_H_
#define ZH_SXFUNC_H_

#include "zh_api.h"
#include "zh_item_api.h"
#include "zh_apifs.h"
#include "zh_rdd_api.h"
#include "zh_api_error.h"
#include "zh_date.h"

ZH_EXTERN_BEGIN

char * zh_sxDtoP( char * pDate, long lJulian );
long zh_sxPtoD( const char * pDate );

void zh_sxEnCrypt( const char * pSrc, char * pDst, const char * pKeyVal, ZH_SIZE nLen );
void zh_sxDeCrypt( const char * pSrc, char * pDst, const char * pKeyVal, ZH_SIZE nLen );

ZH_BOOL zh_LZSSxDecompressMem( const char * pSrcBuf, ZH_SIZE nSrcLen, char * pDstBuf, ZH_SIZE nDstLen );
ZH_BOOL zh_LZSSxCompressMem( const char * pSrcBuf, ZH_SIZE nSrcLen, char * pDstBuf, ZH_SIZE nDstLen, ZH_SIZE * pnSize );
ZH_BOOL zh_LZSSxCompressFile( PZH_FILE pInput, PZH_FILE pOutput, ZH_SIZE * pnSize );
ZH_BOOL zh_LZSSxDecompressFile( PZH_FILE pInput, PZH_FILE pOutput );

ZH_EXTERN_END

#endif /* ZH_SXFUNC_H_ */
