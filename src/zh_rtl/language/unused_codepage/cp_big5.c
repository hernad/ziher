/*
 * Example of Ziher codepage using BIG5 encoding
 *
 * Copyright 2011 Przemyslaw Czerpak
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
#include "zh_codepage_api.h"

#include "big5.c"

static ZH_CODEPAGE_GET_FUNC( BIG5_get )
{
   *wc = 0;
   if( *pnIndex < nLen )
   {
      ZH_UCHAR uc = pSrc[ ( *pnIndex )++ ];

      if( uc >= ( ZH_BIG5_FIRST >> 8 ) && uc <= ( ZH_BIG5_LAST >> 8 ) &&
          *pnIndex < nLen )
      {
         *wc = s_big5_to_ucs16( ( ( int ) uc << 8 ) | ( ZH_UCHAR ) pSrc[ *pnIndex ] );
         if( *wc )
         {
            ( *pnIndex )++;
            return ZH_TRUE;
         }
      }
      *wc = cdp->uniTable->uniCodes[ uc ];
      if( *wc == 0 )
         *wc = uc;
      return ZH_TRUE;
   }
   return ZH_FALSE;
}

static ZH_CODEPAGE_PUT_FUNC( BIG5_put )
{
   if( *pnIndex < nLen )
   {
      ZH_USHORT b5 = s_ucs16_to_big5( wc );

      if( b5 )
      {
         if( *pnIndex + 1 < nLen )
         {
            ZH_PUT_BE_UINT16( &pDst[ ( *pnIndex ) ], b5 );
            *pnIndex += 2;
            return ZH_TRUE;
         }
      }
      else
      {
         if( cdp->uniTable->uniTrans == NULL )
            zh_cdpBuildTransTable( cdp->uniTable );

         if( wc <= cdp->uniTable->wcMax &&
             cdp->uniTable->uniTrans[ wc ] )
            pDst[ ( *pnIndex )++ ] = cdp->uniTable->uniTrans[ wc ];
         else
            pDst[ ( *pnIndex )++ ] = wc >= 0x100 ? '?' : ( ZH_UCHAR ) wc;
         return ZH_TRUE;
      }
   }
   return ZH_FALSE;
}

static ZH_CODEPAGE_LEN_FUNC( BIG5_len )
{
   ZH_USHORT b5 = s_ucs16_to_big5( wc );

   ZH_SYMBOL_UNUSED( cdp );

   return b5 ? 2 : 1;
}

static void zh_cp_init( PZH_CODEPAGE cdp )
{
   ZH_UCHAR * flags, * upper, * lower;
   int i;

   cdp->buffer = ( ZH_UCHAR * ) zh_xgrab( 0x300 );
   cdp->flags = flags = ( ZH_UCHAR * ) cdp->buffer;
   cdp->upper = upper = ( ZH_UCHAR * ) cdp->buffer + 0x100;
   cdp->lower = lower = ( ZH_UCHAR * ) cdp->buffer + 0x200;

   for( i = 0; i < 0x100; ++i )
   {
      flags[ i ] = 0;
      if( ZH_ISDIGIT( i ) )
         flags[ i ] |= ZH_CODEPAGE_DIGIT;
      if( ZH_ISALPHA( i ) )
         flags[ i ] |= ZH_CODEPAGE_ALPHA;
      if( ZH_ISUPPER( i ) )
         flags[ i ] |= ZH_CODEPAGE_UPPER;
      if( ZH_ISLOWER( i ) )
         flags[ i ] |= ZH_CODEPAGE_LOWER;
      upper[ i ] = ( ZH_UCHAR ) ZH_TOUPPER( i );
      lower[ i ] = ( ZH_UCHAR ) ZH_TOLOWER( i );
   }

#if 0
   for( i = 0; i < 0x10000; ++i )
   {
      ZH_WCHAR wc = s_big5_to_ucs16( i );
      if( wc )
      {
         if( i != s_ucs16_to_big5( wc ) )
         {
            printf( "irreversible translation: (BIG5)%04X -> U+%04X -> (BIG5)%04X\r\n",
                    i, wc, s_ucs16_to_big5( wc ) );
            fflush(stdout);
         }
      }
   }
#endif
}

#define ZH_CP_RAW

#define ZH_CP_ID              BIG5
#define ZH_CP_INFO            "BIG-5"
#define ZH_CP_UNITB           ZH_UNITB_437

#define ZH_CP_GET_FUNC        BIG5_get
#define ZH_CP_PUT_FUNC        BIG5_put
#define ZH_CP_LEN_FUNC        BIG5_len

#define ZH_CP_FLAGS_FUNC      NULL
#define ZH_CP_UPPER_FUNC      NULL
#define ZH_CP_LOWER_FUNC      NULL

#define ZH_CP_CMP_FUNC        NULL

#define s_flags               NULL
#define s_upper               NULL
#define s_lower               NULL
#define s_sort                NULL

#define ZH_CP_INIT zh_cp_init

/* include CP registration code */
#include "zh_codepage_reg.h"
