/*
 * TIP Class oriented Internet protocol library
 *
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
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
#include "zh_error_api.h"

ZH_FUNC( TIP_URLENCODE )
{
   const char * pszData = zh_parc( 1 );

   if( pszData )
   {
      ZH_ISIZ nLen = zh_parclen( 1 );

      if( nLen )
      {
         ZH_BOOL bComplete = zh_parldef( 2, ZH_TRUE );
         ZH_ISIZ nPos = 0, nPosRet = 0;

         /* Giving maximum final length possible */
         char * pszRet = ( char * ) zh_xgrab( nLen * 3 + 1 );

         while( nPos < nLen )
         {
            char cElem = pszData[ nPos ];

            if( cElem == ' ' )
            {
               pszRet[ nPosRet ] = '+';
            }
            else if( ( cElem >= 'A' && cElem <= 'Z' ) ||
                     ( cElem >= 'a' && cElem <= 'z' ) ||
                     ( cElem >= '0' && cElem <= '9' ) ||
                     cElem == '.' || cElem == ',' || cElem == '&' ||
                     cElem == '/' || cElem == ';' || cElem == '_' )
            {
               pszRet[ nPosRet ] = cElem;
            }
            else if( ! bComplete && ( cElem == ':' || cElem == '?' || cElem == '=' ) )
            {
               pszRet[ nPosRet ] = cElem;
            }
            else /* encode! */
            {
               unsigned int uiVal;
               pszRet[ nPosRet++ ] = '%';
               uiVal = ( ( ZH_UCHAR ) cElem ) >> 4;
               pszRet[ nPosRet++ ] = ( char ) ( ( uiVal < 10 ? '0' : 'A' - 10 ) + uiVal );
               uiVal = ( ( ZH_UCHAR ) cElem ) & 0x0F;
               pszRet[ nPosRet ] = ( char ) ( ( uiVal < 10 ? '0' : 'A' - 10 ) + uiVal );
            }

            nPosRet++;
            nPos++;
         }

         zh_retclen_buffer( pszRet, nPosRet );
      }
      else
         zh_retc_null();
   }
   else
      zh_errRT_BASE( EG_ARG, 3012, NULL,
                     ZH_ERR_FUNCNAME, 1, zh_paramError( 1 ) );
}

ZH_FUNC( TIP_URLDECODE )
{
   const char * pszData = zh_parc( 1 );

   if( pszData )
   {
      ZH_ISIZ nLen = zh_parclen( 1 );

      if( nLen )
      {
         ZH_ISIZ nPos = 0, nPosRet = 0;

         /* maximum possible length */
         char * pszRet = ( char * ) zh_xgrab( nLen );

         while( nPos < nLen )
         {
            char cElem = pszData[ nPos ];

            if( cElem == '%' && ZH_ISXDIGIT( pszData[ nPos + 1 ] ) &&
                                ZH_ISXDIGIT( pszData[ nPos + 2 ] ) )
            {
               cElem = pszData[ ++nPos ];
               pszRet[ nPosRet ]  = cElem - ( cElem >= 'a' ? 'a' - 10 :
                                            ( cElem >= 'A' ? 'A' - 10 : '0' ) );
               pszRet[ nPosRet ] <<= 4;
               cElem = pszData[ ++nPos ];
               pszRet[ nPosRet ] |= cElem - ( cElem >= 'a' ? 'a' - 10 :
                                            ( cElem >= 'A' ? 'A' - 10 : '0' ) );
            }
            else
               pszRet[ nPosRet ] = cElem == '+' ? ' ' : cElem;

            nPos++;
            nPosRet++;
         }

         /* this function also adds a zero */
         /* hopefully reduce the size of pszRet */
         zh_retclen_buffer( ( char * ) zh_xrealloc( pszRet, nPosRet + 1 ), nPosRet );
      }
      else
         zh_retc_null();
   }
   else
      zh_errRT_BASE( EG_ARG, 3012, NULL,
                     ZH_ERR_FUNCNAME, 1, zh_paramError( 1 ) );
}
