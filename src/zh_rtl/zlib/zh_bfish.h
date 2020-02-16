/*
 * This code implements BlowFish algorithm designed by Bruce Schneier.
 * The description of BlowFish algorithm can be found at:
 *    https://www.schneier.com/paper-blowfish-fse.html
 * This code uses for initial s-boxes and p-array values PI hex digits
 * taken from tables public at:
 *    https://www.schneier.com/blowfish.html
 * which can be downloaded from:
 *    https://www.schneier.com/code/constants.txt
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

#ifndef ZH_BLOWFISH_H_
#define ZH_BLOWFISH_H_

#include "zh_defs.h"

ZH_EXTERN_BEGIN

#define ZH_BF_CIPHERBLOCK     8

#define SUBKEYS_COUNT         18
#define SBOX_ENTRIES          256

typedef struct
{
   ZH_U32   P[ SUBKEYS_COUNT ];
   ZH_U32   S1[ SBOX_ENTRIES ];
   ZH_U32   S2[ SBOX_ENTRIES ];
   ZH_U32   S3[ SBOX_ENTRIES ];
   ZH_U32   S4[ SBOX_ENTRIES ];
}
ZH_BLOWFISH;

extern ZH_EXPORT void zh_blowfishInit( ZH_BLOWFISH * bf, const void * keydata, int keylen );
extern ZH_EXPORT void zh_blowfishEncrypt( const ZH_BLOWFISH * bf, ZH_U32 * xl, ZH_U32 * xr );
extern ZH_EXPORT void zh_blowfishDecrypt( const ZH_BLOWFISH * bf, ZH_U32 * xl, ZH_U32 * xr );

ZH_EXTERN_END

#endif /* ZH_BLOWFISH_H_ */
