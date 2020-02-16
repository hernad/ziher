/*
 * PRG functions for MD5 encryption/decryption using
 * CFB (cipher feedback) mode
 *
 * Copyright 2012 Przemyslaw Czerpak
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
#include "zh_chksum.h"

static void zh_md5_init_seed( char * vect, const char * pszKey, int iLen )
{
   zh_md5( pszKey, iLen, vect );
}

static void zh_md5_next_seed( char * vect, const char * pszKey, int iLen )
{
   int i;

   for( i = 0; i < 16; ++i )
      vect[ i ] ^= pszKey[ i % iLen ];
   zh_md5( vect, 16, vect );
}

/* zh_MD5Encrypt( <cText>, <cPasswd> ) --> <cCipher>
 */
ZH_FUNC( ZH_MD5ENCRYPT )
{
   PZH_ITEM pData = zh_param( 1, ZH_IT_STRING );

   if( pData && zh_parclen( 2 ) > 0 )
   {
      ZH_SIZE nLen = zh_itemGetCLen( pData );

      if( nLen )
      {
         const char * pszSource = zh_itemGetCPtr( pData );
         char * pszData = ( char * ) zh_xgrab( nLen + 1 );
         const char * pszKey = zh_parc( 2 );
         int iLen = ( int ) zh_parclen( 2 );
         char vect[ 16 ];
         ZH_SIZE n;

         zh_md5_init_seed( vect, pszKey, iLen );

         for( n = 0; n < nLen; ++n )
         {
            int i = ( int ) ( n & 0x0F );
            if( i == 0 )
               zh_md5_next_seed( vect, pszKey, iLen );
            pszData[ n ] = ( vect[ i ] ^= pszSource[ n ] );
         }
         zh_retclen_buffer( pszData, nLen );
      }
      else
         zh_retc_null();
   }
}

/* zh_MD5Decrypt( <cCipher>, <cPasswd> ] ) --> <cText>
 */
ZH_FUNC( ZH_MD5DECRYPT )
{
   PZH_ITEM pData = zh_param( 1, ZH_IT_STRING );

   if( pData && zh_parclen( 2 ) > 0 )
   {
      ZH_SIZE nLen = zh_itemGetCLen( pData );

      if( nLen )
      {
         const char * pszSource = zh_itemGetCPtr( pData );
         char * pszData = ( char * ) zh_xgrab( nLen + 1 );
         const char * pszKey = zh_parc( 2 );
         int iLen = ( int ) zh_parclen( 2 );
         char vect[ 16 ];
         ZH_SIZE n;

         zh_md5_init_seed( vect, pszKey, iLen );

         for( n = 0; n < nLen; ++n )
         {
            int i = ( int ) ( n & 0x0F );
            if( i == 0 )
               zh_md5_next_seed( vect, pszKey, iLen );
            pszData[ n ] = ( vect[ i ] ^ pszSource[ n ] );
            vect[ i ] = pszSource[ n ];
         }
         zh_retclen_buffer( pszData, nLen );
      }
      else
         zh_retc_null();
   }
}
