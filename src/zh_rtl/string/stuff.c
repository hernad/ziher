/*
 * Stuff() function
 *
 * Copyright 2012 Przemyslaw Czerpak
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

/* replaces characters in a string */
ZH_FUNC( STUFF )
{
   const char * szText = zh_parc( 1 );
   const char * szIns = zh_parc( 4 );

   if( szText && szIns && ZH_IS_PARAM_NUM( 2 ) && ZH_IS_PARAM_NUM( 3 ) )
   {
      PZH_CODEPAGE cdp = zh_vmCodepage();
      ZH_SIZE nLen = zh_parclen( 1 );
      ZH_SIZE nPos = zh_parns( 2 );
      ZH_SIZE nDel = zh_parns( 3 );
      ZH_SIZE nIns = zh_parclen( 4 );
      ZH_SIZE nTot;

      if( ZH_CODEPAGE_ISCHARIDX( cdp ) )
      {
         if( nPos )
            nPos = nPos < 1 ? nLen : zh_cdpTextPos( cdp, szText, nLen, nPos - 1 );
         if( nDel )
         {
            if( nPos < nLen )
            {
               nDel = zh_cdpTextPos( cdp, szText + nPos, nLen - nPos, nDel );
               if( nDel == 0 )
                  nDel = nLen - nPos;
            }
            else
               nDel = 0;
         }
      }
      else
      {
         if( nPos )
         {
            if( nPos < 1 || nPos > nLen )
               nPos = nLen;
            else
               nPos--;
         }
         if( nDel )
         {
            if( nDel < 1 || nDel > nLen - nPos )
               nDel = nLen - nPos;
         }
      }

      if( ( nTot = nLen + nIns - nDel ) > 0 )
      {
         char * szResult = ( char * ) zh_xgrab( nTot + 1 );

         zh_xmemcpy( szResult, szText, nPos );
         zh_xmemcpy( szResult + nPos, szIns, nIns );
         zh_xmemcpy( szResult + nPos + nIns, szText + nPos + nDel,
                     nLen - ( nPos + nDel ) );
         zh_retclen_buffer( szResult, nTot );
      }
      else
         zh_retc_null();
   }
   else
      zh_retc_null();
}
