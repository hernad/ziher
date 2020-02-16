/*
 * Header files for functions to calculate different checksums
 *
 * Copyright 2007 Przemyslaw Czerpak
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

ZH_EXTERN_BEGIN

extern ZH_EXPORT ZH_U32 zh_adler32( ZH_U32 adler, const void * buf, ZH_SIZE len );
extern ZH_EXPORT ZH_U16 zh_crc16( ZH_U16 crc, const void * buf, ZH_SIZE len );
extern ZH_EXPORT ZH_U32 zh_crc32( ZH_U32 crc, const void * buf, ZH_SIZE len );
extern ZH_EXPORT ZH_MAXUINT zh_crc( ZH_MAXUINT crc, const void * buf, ZH_SIZE len, ZH_MAXUINT poly );
extern ZH_EXPORT ZH_MAXUINT zh_crcct( ZH_MAXUINT crc, const void * buf, ZH_SIZE len, ZH_MAXUINT poly );
extern ZH_EXPORT void zh_md5( const void * data, ZH_SIZE datalen, char * digest );
extern ZH_EXPORT ZH_BOOL zh_md5file( const char * pszFileName, char * digest );

ZH_EXTERN_END
