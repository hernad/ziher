/*
 * SIX compatible functions:
 *       zh_sxEnCrypt()
 *       zh_sxDeCrypt()
 *
 *       sx_Encrypt()
 *       sx_Decrypt()
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
#include "zh_item_api.h"
#include "zh_apifs.h"
#include "zh_rdd_api.h"
#include "zh_sx_func.h"

#define rnd_mul1  0x0de6d
#define rnd_mul2  0x0278d

static ZH_U32 zh_sxInitSeed( const char * pKeyVal, ZH_U16 * puiKey )
{
   ZH_U32 ulSeed = 0;
   int i;

   for( i = 0; i < 7; i++ )
   {
      ulSeed = ( ( ( ulSeed >> 16 ) + ( ulSeed << 16 ) ) * 17 ) +
               ZH_GET_LE_UINT16( &pKeyVal[ i ] );
   }
   ulSeed |= 1;
   *puiKey = ( ZH_U16 ) ulSeed;
   return ( ulSeed << 16 ) + ( ulSeed >> 16 );
}

static ZH_U32 zh_sxNextSeed( ZH_U32 ulSeed, const char * pKeyVal, ZH_U16 * puiKey )
{
   ZH_U32 ulTemp1, ulTemp2;
   ZH_U16 uiSeedLo, uiSeedHi;

   uiSeedLo = ( ZH_U16 ) ulSeed;
   ulTemp1  = ( ZH_U32 ) rnd_mul1 * ( ZH_U32 ) uiSeedLo;
   ulTemp2  = ( ZH_U32 ) rnd_mul2 * ( ZH_U32 ) uiSeedLo + ( ulTemp1 >> 16 );
   uiSeedLo = ( ZH_U16 ) ulTemp1;
   ulTemp1  = ( ZH_U32 ) rnd_mul1 * ( ulSeed >> 16 );
   uiSeedHi = ( ZH_U16 ) ( ulTemp1 + ulTemp2 );
   ulSeed   = ( ( ZH_U32 ) uiSeedHi << 16 ) + ( ZH_U32 ) uiSeedLo;
   uiSeedHi |= 1;
   *puiKey  = uiSeedHi + ZH_GET_LE_UINT16( pKeyVal );
   return ulSeed;
}

void zh_sxEnCrypt( const char * pSrc, char * pDst, const char * pKeyVal, ZH_SIZE nLen )
{
   ZH_U32 ulSeed;
   ZH_U16 uiKey;
   ZH_SIZE nPos;
   int i;

   ulSeed = zh_sxInitSeed( pKeyVal, &uiKey );
   for( nPos = 0, i = 0; nPos < nLen; nPos++ )
   {
      ZH_UCHAR ucChar, ucShft;

      ucChar = ( ZH_UCHAR ) pSrc[ nPos ];
      ucShft = ( ZH_UCHAR ) ( uiKey & 0x07 );
      pDst[ nPos ] = ( ( ucChar >> ucShft ) + ( ucChar << ( 8 - ucShft ) ) +
                       ( uiKey & 0xFF ) );
      ulSeed = zh_sxNextSeed( ulSeed, &pKeyVal[ i ], &uiKey );
      if( ++i == 7 )
         i = 0;
   }
}

void zh_sxDeCrypt( const char * pSrc, char * pDst, const char * pKeyVal, ZH_SIZE nLen )
{
   ZH_U32 ulSeed;
   ZH_U16 uiKey;
   ZH_SIZE nPos;
   int i;

   ulSeed = zh_sxInitSeed( pKeyVal, &uiKey );
   for( nPos = 0, i = 0; nPos < nLen; nPos++ )
   {
      ZH_UCHAR ucChar, ucShft;

      ucChar = ( ZH_UCHAR ) pSrc[ nPos ] - ( uiKey & 0xFF );
      ucShft = ( ZH_UCHAR ) ( uiKey & 0x07 );
      pDst[ nPos ] = ( ( ucChar << ucShft ) + ( ucChar >> ( 8 - ucShft ) ) );
      ulSeed = zh_sxNextSeed( ulSeed, &pKeyVal[ i ], &uiKey );
      if( ++i == 7 )
         i = 0;
   }
}

static ZH_BOOL _zh_sxGetKey( PZH_ITEM pKeyItem, char * pKeyVal )
{
   ZH_BOOL fResult = ZH_FALSE;
   PZH_ITEM pItem = NULL;

   if( ! ( zh_itemType( pKeyItem ) & ZH_IT_STRING ) )
   {
      AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

      if( pArea )
      {
         pItem = zh_itemNew( NULL );
         if( SELF_INFO( pArea, DBI_PASSWORD, pItem ) == ZH_SUCCESS )
            pKeyItem = pItem;
      }
   }
   if( zh_itemType( pKeyItem ) & ZH_IT_STRING )
   {
      ZH_SIZE nKey = zh_itemGetCLen( pKeyItem );
      if( nKey )
         memcpy( pKeyVal, zh_itemGetCPtr( pKeyItem ), ZH_MIN( nKey, 8 ) );
      if( nKey < 8 )
         memset( pKeyVal + nKey, 0, 8 - nKey );
      fResult = ZH_TRUE;
   }
   if( pItem )
      zh_itemRelease( pItem );
   return fResult;
}

ZH_FUNC( SX_ENCRYPT )
{
   if( zh_pcount() > 0 )
   {
      char keyBuf[ 8 ];
      ZH_SIZE nLen = zh_parclen( 1 );

      if( nLen > 0 && _zh_sxGetKey( zh_param( 2, ZH_IT_ANY ), keyBuf ) )
      {
         char * pDst = ( char * ) zh_xgrab( nLen + 1 );
         zh_sxEnCrypt( zh_parc( 1 ), pDst, keyBuf, nLen );
         pDst[ nLen ] = 0;
         zh_retclen_buffer( pDst, nLen );
      }
      else
         zh_itemReturn( zh_param( 1, ZH_IT_ANY ) );
   }
}

ZH_FUNC( SX_DECRYPT )
{
   if( zh_pcount() > 0 )
   {
      char keyBuf[ 8 ];
      ZH_SIZE nLen = zh_parclen( 1 );

      if( nLen > 0 && _zh_sxGetKey( zh_param( 2, ZH_IT_ANY ), keyBuf ) )
      {
         char * pDst = ( char * ) zh_xgrab( nLen + 1 );
         zh_sxDeCrypt( zh_parc( 1 ), pDst, keyBuf, nLen );
         pDst[ nLen ] = 0;
         zh_retclen_buffer( pDst, nLen );
      }
      else
         zh_itemReturn( zh_param( 1, ZH_IT_ANY ) );
   }
}
