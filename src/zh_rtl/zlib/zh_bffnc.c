/*
 * ZH functions for BlowFish encryption
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


#include "zh_api.h"
#include "zh_item_api.h"
#include "zh_bfish.h"

static const ZH_BLOWFISH * zh_bf_keyparam( void )
{
   if( zh_parclen( 1 ) == sizeof( ZH_BLOWFISH ) )
      return ( const ZH_BLOWFISH * ) zh_parc( 1 );
   else
      return NULL;
}

/* zh_blowfishKey( <cPasswd> ) --> <cBfKey>
 */
ZH_FUNC( ZH_BLOWFISHKEY )
{
   int iLen = ( int ) zh_parclen( 1 );

   if( iLen )
   {
      ZH_BLOWFISH bf;

      zh_blowfishInit( &bf, zh_parc( 1 ), iLen );
      zh_retclen( ( const char * ) &bf, sizeof( ZH_BLOWFISH ) );
   }
}

/* zh_blowfishEncrypt( <cBfKey>, <cText> [, <lRaw>=.F. ] ) --> <cCipher> | NIL
 * return string encrypted using ECB (electronic codebook) mode or
 * NIL on error (wrong parameters),
 * in raw mode passed string is padded to 8 bytes with '\0'
 * otherwise ANSI X.923 padding is used
 */
ZH_FUNC( ZH_BLOWFISHENCRYPT )
{
   const ZH_BLOWFISH * bf = zh_bf_keyparam();
   PZH_ITEM pData = zh_param( 2, ZH_IT_STRING );

   if( bf && pData )
   {
      ZH_SIZE nLen = zh_itemGetCLen( pData );

      if( nLen )
      {
         char * pszData;
         ZH_BOOL fRaw = zh_parl( 3 );
         ZH_SIZE nSize;

         /* In raw mode passed string is padded to 8 bytes with '\0'
          * otherwise ANSI X.923 padding is used
          */
         nSize = ( fRaw ? ( ( nLen + 7 ) >> 3 ) :
                          ( ( nLen >> 3 ) + 1 ) ) << 3;
         pszData = ( char * ) zh_xgrab( nSize + 1 );
         memcpy( pszData, zh_itemGetCPtr( pData ), nLen );
         memset( pszData + nLen, '\0', nSize - nLen );
         if( ! fRaw )
            pszData[ nSize - 1 ] = ( char ) ( nSize - nLen );
         for( nLen = 0; nLen < nSize; nLen += 8 )
         {
            ZH_U32 xl, xr;
            xl = ZH_GET_BE_UINT32( &pszData[ nLen ] );
            xr = ZH_GET_BE_UINT32( &pszData[ nLen + 4 ] );
            zh_blowfishEncrypt( bf, &xl, &xr );
            ZH_PUT_BE_UINT32( &pszData[ nLen ], xl );
            ZH_PUT_BE_UINT32( &pszData[ nLen + 4 ], xr );
         }
         zh_retclen_buffer( pszData, nSize );
      }
      else
         zh_retc_null();
   }
}

/* zh_blowfishDecrypt( <cBfKey>, <cCipher> [, <lRaw>=.F. ] ) --> <cText> | NIL
 * return string decrypted using ECB (electronic codebook) mode or
 * NIL on error (wrong parameters),
 * in raw mode whole passed string is decoded as is
 * otherwise it's decoded ANSI X.923 padded data
 */
ZH_FUNC( ZH_BLOWFISHDECRYPT )
{
   const ZH_BLOWFISH * bf = zh_bf_keyparam();
   PZH_ITEM pData = zh_param( 2, ZH_IT_STRING );

   if( bf && pData )
   {
      ZH_SIZE nSize = zh_itemGetCLen( pData );

      if( nSize >= 8 && ( nSize & 0x07 ) == 0 )
      {
         const char * pszSource;
         char * pszData;
         ZH_BOOL fRaw = zh_parl( 3 );
         ZH_SIZE nLen;

         pszData = ( char * ) zh_xgrab( nSize + ( fRaw ? 1 : 0 ) );
         pszSource = zh_itemGetCPtr( pData );
         for( nLen = 0; nLen < nSize; nLen += 8 )
         {
            ZH_U32 xl, xr;
            xl = ZH_GET_BE_UINT32( &pszSource[ nLen ] );
            xr = ZH_GET_BE_UINT32( &pszSource[ nLen + 4 ] );
            zh_blowfishDecrypt( bf, &xl, &xr );
            ZH_PUT_BE_UINT32( &pszData[ nLen ], xl );
            ZH_PUT_BE_UINT32( &pszData[ nLen + 4 ], xr );
         }
         if( ! fRaw )
         {
            nSize = ( unsigned char ) pszData[ nSize - 1 ];
            nLen -= ( ( nSize - 1 ) & ~0x07 ) == 0 ? nSize : nLen;
         }
         if( nLen )
            zh_retclen_buffer( pszData, nLen );
         else
            zh_xfree( pszData );
      }
      else if( nSize == 0 )
         zh_retc_null();
   }
}

/* BlowFish encryption using CFB (cipher feedback) mode instead
 * of ECB (electronic codebook) mode with ANSI X.923 padding
 */
static void zh_bf_initvect( ZH_BYTE * vect )
{
   const char * pszVect = zh_parc( 3 );
   int iLen = ( int ) zh_parclen( 3 );
   int i;

   for( i = 0; i < ZH_BF_CIPHERBLOCK; ++i )
   {
      vect[ i ] = ( ZH_BYTE ) i;
      if( iLen > 0 )
         vect[ i ] ^= ( ZH_BYTE ) pszVect[ i % iLen ];
   }
}

static void zh_bf_encode( const ZH_BLOWFISH * bf, ZH_BYTE * vect )
{
   ZH_U32 xl, xr;

   xl = ZH_GET_BE_UINT32( &vect[ 0 ] );
   xr = ZH_GET_BE_UINT32( &vect[ 4 ] );
   zh_blowfishEncrypt( bf, &xl, &xr );
   ZH_PUT_BE_UINT32( &vect[ 0 ], xl );
   ZH_PUT_BE_UINT32( &vect[ 4 ], xr );
}

/* zh_blowfishEncrypt_CFB( <cBfKey>, <cText> [, <cInitSeed> ] )
 *          --> <cCipher> | NIL
 * return string encrypted using CFB (cipher feedback) mode or
 * NIL on error (wrong parameters)
 */
ZH_FUNC( ZH_BLOWFISHENCRYPT_CFB )
{
   const ZH_BLOWFISH * bf = zh_bf_keyparam();
   PZH_ITEM pData = zh_param( 2, ZH_IT_STRING );

   if( bf && pData )
   {
      ZH_SIZE nLen = zh_itemGetCLen( pData );

      if( nLen )
      {
         const char * pszSource = zh_itemGetCPtr( pData );
         char * pszData = ( char * ) zh_xgrab( nLen + 1 );
         ZH_BYTE vect[ ZH_BF_CIPHERBLOCK ];
         ZH_SIZE n;

         zh_bf_initvect( vect );

         for( n = 0; n < nLen; ++n )
         {
            int i = ( int ) ( n & ( ZH_BF_CIPHERBLOCK - 1 ) );
            if( i == 0 )
               zh_bf_encode( bf, vect );
            pszData[ n ] = ( vect[ i ] ^= pszSource[ n ] );
         }
         zh_retclen_buffer( pszData, nLen );
      }
      else
         zh_retc_null();
   }
}

/* zh_blowfishDecrypt_CFB( <cBfKey>, <cCipher> [, <cInitSeed> ] )
 *          --> <cText> | NIL
 * return string decrypted using CFB (cipher feedback) mode or
 * NIL on error (wrong parameters),
 */
ZH_FUNC( ZH_BLOWFISHDECRYPT_CFB )
{
   const ZH_BLOWFISH * bf = zh_bf_keyparam();
   PZH_ITEM pData = zh_param( 2, ZH_IT_STRING );

   if( bf && pData )
   {
      ZH_SIZE nLen = zh_itemGetCLen( pData );

      if( nLen )
      {
         const char * pszSource = zh_itemGetCPtr( pData );
         char * pszData = ( char * ) zh_xgrab( nLen + 1 );
         ZH_BYTE vect[ ZH_BF_CIPHERBLOCK ];
         ZH_SIZE n;

         zh_bf_initvect( vect );

         for( n = 0; n < nLen; ++n )
         {
            int i = ( int ) ( n & ( ZH_BF_CIPHERBLOCK - 1 ) );
            if( i == 0 )
               zh_bf_encode( bf, vect );
            pszData[ n ] = ( vect[ i ] ^ pszSource[ n ] );
            vect[ i ] = pszSource[ n ];
         }
         zh_retclen_buffer( pszData, nLen );
      }
      else
         zh_retc_null();
   }
}
