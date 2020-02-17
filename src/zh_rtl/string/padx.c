/*
 * PadL(), PadR(), PadC() functions
 *
 * Copyright 2012 Przemyslaw Czerpak
 * Copyright 1999 Matthew Hamilton <mhamilton@bunge.com.au>
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
#include "zh_codepage_api.h"
#include "zh_api_error.h"

static ZH_SIZE zh_cdpItemLen( PZH_CODEPAGE cdp, PZH_ITEM pItem )
{
   ZH_SIZE nLen = zh_itemGetCLen( pItem );

   return nLen && cdp ?
          zh_cdpTextLen( cdp, zh_itemGetCPtr( pItem ), nLen ) : nLen;
}

static const char * s_zh_padGet( PZH_CODEPAGE cdp, ZH_SIZE * pnPad )
{
   const char * szPad = zh_parc( 3 );

   *pnPad = 1;
   if( szPad == NULL )
      szPad = " ";
   else if( cdp )
   {
      *pnPad = zh_cdpTextPos( cdp, szPad, zh_parclen( 3 ), 1 );
      if( *pnPad == 0 )
         szPad = "";
   }
   return szPad;
}

#define ZH_PAD_L  0
#define ZH_PAD_R  1
#define ZH_PAD_C  2

static void s_zh_strPad( int iMode, PZH_CODEPAGE cdp )
{
   ZH_ISIZ nLen = zh_parns( 2 );

   if( nLen > 0 )
   {
      PZH_ITEM pItem = zh_param( 1, ZH_IT_ANY );

      if( pItem && ZH_IS_STRING( pItem ) &&
          ( ZH_SIZE ) nLen == zh_cdpItemLen( cdp, pItem ) )
      {
         zh_itemReturn( pItem );
      }
      else
      {
         ZH_SIZE nSize;
         ZH_BOOL bFreeReq;
         char * szText = zh_itemPadConv( pItem, &nSize, &bFreeReq );

         if( szText )
         {
            if( cdp )
            {
               ZH_SIZE nText = nLen;
               nLen = zh_cdpTextPosEx( cdp, szText, nSize, &nText );
               nLen += nText;
            }

            if( ( ZH_SIZE ) nLen > nSize )
            {
               ZH_SIZE nPad = 0;
               const char * szPad = s_zh_padGet( cdp, &nPad );
               char * szResult;

               switch( iMode )
               {
                  case ZH_PAD_L:
                     if( nPad > 1 )
                     {
                        ZH_SIZE nRep = ( ( ZH_SIZE ) nLen - nSize ), nPos = 0;
                        nLen += nRep * ( nPad - 1 );
                        szResult = ( char * ) zh_xgrab( nLen + 1 );
                        while( nRep-- )
                        {
                           zh_xmemcpy( szResult + nPos, szPad, nPad );
                           nPos += nPad;
                        }
                        zh_xmemcpy( szResult + nPos, szText, nSize );
                     }
                     else
                     {
                        szResult = ( char * ) zh_xgrab( nLen + 1 );
                        zh_xmemset( szResult, szPad[ 0 ], ( ZH_SIZE ) nLen - nSize );
                        zh_xmemcpy( szResult + ( ZH_SIZE ) nLen - nSize, szText, nSize );
                     }
                     break;
                  case ZH_PAD_R:
                     if( nPad > 1 )
                     {
                        nLen += ( nLen - nSize ) * ( nPad - 1 );
                        szResult = ( char * ) zh_xgrab( nLen + 1 );
                        zh_xmemcpy( szResult, szText, nSize );
                        while( nSize < ( ZH_SIZE ) nLen )
                        {
                           zh_xmemcpy( szResult + nSize, szPad, nPad );
                           nSize += nPad;
                        }
                     }
                     else
                     {
                        szResult = ( char * ) zh_xgrab( nLen + 1 );
                        zh_xmemcpy( szResult, szText, nSize );
                        zh_xmemset( szResult + nSize, szPad[ 0 ], ( ZH_SIZE ) nLen - nSize );
                     }
                     break;
                  default: /* ZH_PAD_C */
                     if( nPad > 1 )
                     {
                        ZH_SIZE nRep = ( ( ZH_SIZE ) nLen - nSize ) >> 1, nPos = 0;
                        nLen += ( nLen - nSize ) * ( nPad - 1 );
                        szResult = ( char * ) zh_xgrab( nLen + 1 );
                        while( nRep-- )
                        {
                           zh_xmemcpy( szResult + nPos, szPad, nPad );
                           nPos += nPad;
                        }
                        zh_xmemcpy( szResult + nPos, szText, nSize );
                        nSize += nPos;
                        while( nSize < ( ZH_SIZE ) nLen )
                        {
                           zh_xmemcpy( szResult + nSize, szPad, nPad );
                           nSize += nPad;
                        }
                     }
                     else
                     {
                        szResult = ( char * ) zh_xgrab( nLen + 1 );
                        nPad = ( ( ZH_SIZE ) nLen - nSize ) >> 1;
                        zh_xmemset( szResult, szPad[ 0 ], nPad );
                        zh_xmemcpy( szResult + nPad, szText, nSize );
                        zh_xmemset( szResult + nPad + nSize, szPad[ 0 ],
                                    ( ZH_SIZE ) nLen - nSize - nPad );
                     }
                     break;
               }
               zh_retclen_buffer( szResult, ( ZH_SIZE ) nLen );
               if( bFreeReq )
                  zh_xfree( szText );
            }
            else
            {
               if( bFreeReq )
                  zh_retclen_buffer( szText, ( ZH_SIZE ) nLen );
               else
                  zh_retclen( szText, nLen );
            }
         }
         else
            zh_retc_null();
      }
   }
   else
      zh_retc_null();
}

/* left-pads a date, number, or string with spaces or supplied character */
ZH_FUNC( PADL )
{
   PZH_CODEPAGE cdp = zh_vmCDP();

   if( ! ZH_CODEPAGE_ISCHARIDX( cdp ) )
      cdp = NULL;

   s_zh_strPad( ZH_PAD_L, cdp );
}

ZH_FUNC( ZH_BPADL )
{
   s_zh_strPad( ZH_PAD_L, NULL );
}

ZH_FUNC( ZH_UPADL )
{
   PZH_CODEPAGE cdp = zh_vmCDP();

   if( ! ZH_CODEPAGE_ISCUSTOM( cdp ) )
      cdp = NULL;

   s_zh_strPad( ZH_PAD_L, cdp );
}

/* right-pads a date, number, or string with spaces or supplied character */
ZH_FUNC( PADR )
{
   PZH_CODEPAGE cdp = zh_vmCDP();

   if( ! ZH_CODEPAGE_ISCHARIDX( cdp ) )
      cdp = NULL;

   s_zh_strPad( ZH_PAD_R, cdp );
}

ZH_FUNC( ZH_BPADR )
{
   s_zh_strPad( ZH_PAD_R, NULL );
}

ZH_FUNC( ZH_UPADR )
{
   PZH_CODEPAGE cdp = zh_vmCDP();

   if( ! ZH_CODEPAGE_ISCUSTOM( cdp ) )
      cdp = NULL;

   s_zh_strPad( ZH_PAD_R, cdp );
}

/* centre-pads a date, number, or string with spaces or supplied character */
ZH_FUNC( PADC )
{
   PZH_CODEPAGE cdp = zh_vmCDP();

   if( ! ZH_CODEPAGE_ISCHARIDX( cdp ) )
      cdp = NULL;

   s_zh_strPad( ZH_PAD_C, cdp );
}

ZH_FUNC( ZH_BPADC )
{
   s_zh_strPad( ZH_PAD_C, NULL );
}

ZH_FUNC( ZH_UPADC )
{
   PZH_CODEPAGE cdp = zh_vmCDP();

   if( ! ZH_CODEPAGE_ISCUSTOM( cdp ) )
      cdp = NULL;

   s_zh_strPad( ZH_PAD_C, cdp );
}
