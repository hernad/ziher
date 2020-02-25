/*
 * Crypt() CA-T*ols compatible function
 *
 * WARNING: Non-standard, insecure crypto. Use core zh_blowfish*()
 *          functions or other _standard_ alternatives instead.
 *
 * Copyright 1999-2001 Viktor Szakats (vszakats.net/ziher)
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

ZH_FUNC( CRYPT )
{
   ZH_SIZE nCryptLen = zh_parclen( 2 );

   if( nCryptLen >= 2 )
   {
      const ZH_BYTE * pbyCrypt = ( const ZH_BYTE * ) zh_parc( 2 );
      ZH_SIZE nCryptPos = 0;

      const ZH_BYTE * pbyString = ( const ZH_BYTE * ) zh_parc( 1 );
      ZH_SIZE nStringLen = zh_parclen( 1 );
      ZH_SIZE nStringPos;

      ZH_BYTE * pbyResult = ( ZH_BYTE * ) zh_xgrab( nStringLen + 1 );

      ZH_USHORT uiCount2 =
         ( ( ( ZH_USHORT ) ( pbyCrypt[ nCryptPos ] + ( ZH_USHORT ) ( pbyCrypt[ nCryptPos + 1 ] * 256 ) ) ) &
           0xFFFF ) ^ ( ( ZH_USHORT ) nCryptLen & 0xFFFF );
      ZH_USHORT uiCount1 = 0xAAAA;

      for( nStringPos = 0; nStringPos < nStringLen; )
      {
         ZH_USHORT uiTmpCount1 = uiCount1;
         ZH_USHORT uiTmpCount2 = uiCount2;
         ZH_BYTE byte = pbyString[ nStringPos ] ^ pbyCrypt[ nCryptPos++ ];
         ZH_USHORT tmp;

         uiTmpCount2 =
            ZH_MKUSHORT( ( ZH_LOBYTE( uiTmpCount2 ) ^ ZH_HIBYTE( uiTmpCount2 ) ),
                         ZH_HIBYTE( uiTmpCount2 ) );

         for( tmp = ZH_LOBYTE( uiTmpCount2 ); tmp; tmp-- )
            uiTmpCount2 = ( uiTmpCount2 >> 1 ) | ( ( uiTmpCount2 & 1 ) << 15 );

         uiTmpCount2 ^= uiTmpCount1;
         uiTmpCount2 += 16;

         uiCount2 = uiTmpCount2;

         uiTmpCount2 &= 0x1E;
         uiTmpCount2 += 2;

         do
         {
            ZH_BYTE byTmp;

            uiTmpCount2--;

            for( tmp = ZH_LOBYTE( uiTmpCount2 ); tmp; tmp-- )
               uiTmpCount1 = ( uiTmpCount1 >> 1 ) | ( ( uiTmpCount1 & 1 ) << 15 );

            uiTmpCount1 = ZH_MKUSHORT( ZH_HIBYTE( uiTmpCount1 ), ZH_LOBYTE( uiTmpCount1 ) );
            uiTmpCount1 =
               ZH_MKUSHORT( ( ZH_LOBYTE( uiTmpCount1 ) ^ 0xFF ), ZH_HIBYTE( uiTmpCount1 ) );
            uiTmpCount1 = ( uiTmpCount1 << 1 ) | ( ( uiTmpCount1 & 0x8000 ) >> 15 );
            uiTmpCount1 ^= 0xAAAA;

            byTmp = ZH_LOBYTE( uiTmpCount1 );
            byTmp = ( byTmp << 1 ) | ( ( byTmp & 0x80 ) >> 7 );

            uiTmpCount1 = ZH_MKUSHORT( byTmp, ZH_HIBYTE( uiTmpCount1 ) );

         }
         while( --uiTmpCount2 );

         uiCount1 = uiTmpCount1;

         pbyResult[ nStringPos++ ] = byte ^ ZH_LOBYTE( uiTmpCount1 );

         if( nCryptPos == nCryptLen )
            nCryptPos = 0;
      }

      zh_retclen_buffer( ( char * ) pbyResult, nStringLen );
   }
   else
      zh_retc_null();
}
