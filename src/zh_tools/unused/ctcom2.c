/*
 * CT3 serial communication functions not directly operating on
 *    serial devices
 *
 * Copyright 2010 Przemyslaw Czerpak
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
#include "zh_gt_api.h"
#include "crypto/zh_chksum.h"

/* com_DosCon( <cString>, [<nLine>], [<nColumn>] ) --> <cNull>
 */
ZH_FUNC( COM_DOSCON )
{
   ZH_SIZE nLen = zh_parclen( 1 );

   if( nLen > 0 )
   {
      if( ZH_IS_PARAM_NUM( 2 ) || ZH_IS_PARAM_NUM( 3 ) )
      {
         int iRow, iCol;

         zh_gtGetPos( &iRow, &iCol );
         if( ZH_IS_PARAM_NUM( 2 ) )
            iRow = zh_parni( 1 );
         if( ZH_IS_PARAM_NUM( 3 ) )
            iCol = zh_parni( 2 );
         zh_gtSetPos( iRow, iCol );
      }
      zh_gtWriteCon( zh_parc( 1 ), nLen );
   }
   zh_retc_null();
}

/* com_CRC( <cString>, [<nStart>], [<nPolynomial>] ) --> <nCRC>
 */
ZH_FUNC( COM_CRC )
{
   ZH_MAXUINT crc = zh_parnint( 2 );
   const char * szString = zh_parc( 1 );

   if( szString )
   {
      ZH_MAXUINT nPolynomial = ( ZH_MAXUINT ) zh_parnint( 3 );

      if( nPolynomial == 0 )
         nPolynomial = 0x11021;  /* CRC_16_X25 */

      /* NOTE: warning this function is not bug compatible with CT3.
       *       It fixes few problems in original CT3 implementation
       *       i.e. it works correctly for 8-bit and smaller polynomials
       *       instead of returning 0 or supports much larger polynomials
       *       up to 64-bit.
       *       For 16/17-bit polynomials it gives the same results as CT3
       *       so for most common usage it should be binary compatible
       *       with CT3. [druzus]
       */
      crc = zh_crcct( crc, szString, zh_parclen( 1 ), nPolynomial );
   }
   zh_retnint( crc );
}

static char s_xmoblock_sum( const char * szData, ZH_SIZE nLen )
{
   unsigned char uc = 0;

   while( nLen-- )
      uc += ( unsigned char ) *szData++;
   return ( char ) uc;
}

/* XMoBlock( <cString>, <nBlockNumber>, [<lCRC>], [<nMode>] ) --> <cXModemBlock>
 */
ZH_FUNC( XMOBLOCK )
{
   const char * szData;
   ZH_SIZE nLen, nSize;
   char * pszBlock;
   int iBlock;
   ZH_BOOL fCRC;

   szData = zh_parc( 1 );
   nLen = zh_parclen( 1 );
   iBlock = zh_parni( 2 );
   fCRC = zh_parl( 3 );
   nSize = zh_parni( 4 ) == 2 ? 1024 : 128;

   iBlock &= 0xFF;
   if( nLen > nSize )
      nLen = nSize;
   pszBlock = ( char * ) zh_xgrab( nSize + ( fCRC ? 6 : 5 ) );
   pszBlock[ 0 ] = nSize == 128 ? 1 : 2;
   pszBlock[ 1 ] = ( char ) iBlock;
   pszBlock[ 2 ] = ( char ) ( 0xFF - iBlock );
   if( szData )
      memcpy( pszBlock + 3, szData, nLen );
   if( nLen < nSize )
      memset( pszBlock + nLen + 3, 0, nSize - nLen );
   if( fCRC )
   {
      ZH_U16 crc = ( ZH_U16 ) zh_crcct( 0, pszBlock + 3, nSize, 0x11021 );
      ZH_PUT_BE_UINT16( &pszBlock[ 3 + nSize ], crc );
      nSize += 5;
   }
   else
   {
      pszBlock[ 3 + nSize ] = s_xmoblock_sum( szData, nLen );
      nSize += 4;
   }
   zh_retclen_buffer( pszBlock, nSize );
}

/* XMoCheck( <cString>, [<lCRC>] ) --> <nBlockNumber>|-1
 */
ZH_FUNC( XMOCHECK )
{
   ZH_SIZE nLen = zh_parclen( 1 );
   int iResult = -1;

   if( nLen >= 132 )
   {
      const char * szBlock = zh_parc( 1 );
      ZH_BOOL fCRC = zh_parl( 2 );
      ZH_SIZE nSize;

      if( *szBlock == 0x01 )
         nSize = 128;
      else if( *szBlock == 0x02 )
         nSize = 1024;
      else
         nSize = nLen;
      if( nLen == nSize + ( fCRC ? 5 : 4 ) &&
          ( unsigned char ) szBlock[ 1 ] +
          ( unsigned char ) szBlock[ 2 ] == 0xFF )
      {
         if( fCRC ?
             zh_crcct( 0, szBlock + 3, nSize + 2, 0x11021 ) == 0 :
             s_xmoblock_sum( szBlock + 3, nSize ) == szBlock[ 3 + nSize ] )
            iResult = ( unsigned char ) szBlock[ 1 ];
      }
   }
   zh_retni( iResult );
}

/* ZeroInsert( <cString> ) --> <cDataBlock>
 */
ZH_FUNC( ZEROINSERT )
{
   PZH_ITEM pString = zh_param( 1, ZH_IT_STRING );

   if( pString )
   {
      const char * szText;
      ZH_SIZE nLen, nBits, n;
      unsigned int uiVal;
      int i;

      szText = zh_itemGetCPtr( pString );
      nLen = zh_itemGetCLen( pString );
      uiVal = 0;
      nBits = 0;
      /* NOTE: trailing zero accessed intentionally */
      for( n = 0; n <= nLen; ++n )
      {
         uiVal |= ( unsigned char ) szText[ n ];
         for( i = 0; i < 8; ++i )
         {
            if( ( uiVal & 0xF800 ) == 0xF800 )
            {
               uiVal &= 0xF7FF;
               ++nBits;
            }
            uiVal <<= 1;
         }
      }
      if( nBits )
      {
         ZH_SIZE nDest = nLen + ( ( nBits + 7 ) >> 3 );
         char * pszDest = ( char * ) zh_xgrab( nDest + 1 );
         unsigned char c = 0;
         int j;

         nBits = n = 0;
         i = 1;
         j = 8;
         uiVal = ( unsigned char ) szText[ n ];
         uiVal <<= 8;
         while( nBits < nDest )
         {
            if( --i == 0 )
            {
               if( ++n < nLen )
               {
                  uiVal |= ( unsigned char ) szText[ n ];
                  i = 8;
               }
            }
            if( ( uiVal & 0xF800 ) == 0xF800 )
            {
               c = ( c << 1 ) | 1;
               if( --j == 0 )
               {
                  pszDest[ nBits++ ] = c;
                  j = 8;
               }
               uiVal &= 0xF7FF;
            }
            c <<= 1;
            if( uiVal & 0x8000 )
               c |= 1;
            if( --j == 0 )
            {
               pszDest[ nBits++ ] = c;
               j = 8;
            }
            uiVal <<= 1;
         }

         zh_retclen_buffer( pszDest, nDest );
      }
      else
         zh_itemReturn( pString );
   }
   else
      zh_retc_null();
}

/* ZeroRemove( <cDataBlock> ) --> cString
 */
ZH_FUNC( ZEROREMOVE )
{
   PZH_ITEM pString = zh_param( 1, ZH_IT_STRING );

   if( pString )
   {
      const char * szText;
      ZH_SIZE nLen, nDest, nBits, n;
      int i, j, l;
      unsigned char ucVal;

      szText = zh_itemGetCPtr( pString );
      nLen = zh_itemGetCLen( pString );
      j = 8;
      l = 0;
      ucVal = 0;

      for( n = nDest = nBits = 0; n < nLen; ++n )
      {
         unsigned char c = szText[ n ];

         for( i = 0; i < 8; ++i )
         {
            if( l == 5 )
            {
               if( c & 0x80 )
               {
                  /* wrong string encoding which does not confirm
                   * CCITT specification.
                   */
                  zh_retc_null();
                  return;
               }
               l = 0;
               ++nBits;
            }
            else
            {
               ucVal <<= 1;
               if( c & 0x80 )
               {
                  ++l;
                  ucVal |= 1;
               }
               else
                  l = 0;
               if( --j == 0 )
               {
                  ucVal = 0;
                  ++nDest;
                  j = 8;
               }
            }
            c <<= 1;
         }
      }

      /* NOTE: CT3 decodes some wrong CCITT strings which does not have
       *       trailing 0 instead of returning empty string "", i.e.:
       *          ? Len( ZeroRemove( Chr( 31 ) ) )
       *          ? Len( ZeroRemove( Chr( 31 ) + Chr( 31 ) ) )
       *       this implementation fixed this bug but if you need strict
       *       CT3 behavior for compatibility in some broken code then
       *       you can disable this fix setting ZH_CT3_ZEROREMOVE_BUG
       *       macro. [druzus]
       */
#ifdef ZH_CT3_ZEROREMOVE_BUG
      if( l == 5 )
      {
         ++nLen;
         ++nDest;
         ucVal = 0;
      }
      if( ucVal != 0 )
#else
      if( ucVal != 0 || l == 5 )
#endif

         zh_retc_null();
      else if( nBits )
      {
         char * pszDest = ( char * ) zh_xgrab( nDest + 1 );

         j = 8;
         l = 0;
         ucVal = 0;
         for( n = nDest = 0; n < nLen; ++n )
         {
            unsigned char c = szText[ n ];

            for( i = 0; i < 8; ++i )
            {
               if( l == 5 )
                  l = 0;
               else
               {
                  ucVal <<= 1;
                  if( c & 0x80 )
                  {
                     ++l;
                     ucVal |= 1;
                  }
                  else
                     l = 0;
                  if( --j == 0 )
                  {
                     pszDest[ nDest++ ] = ucVal;
                     j = 8;
                  }
               }
               c <<= 1;
            }
         }

         zh_retclen_buffer( pszDest, nDest );
      }
      else
         zh_itemReturn( pString );
   }
   else
      zh_retc_null();
}
