/*
 * Chr(), Asc() functions
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
#include "zh_error_api.h"

/* converts an ASCII code to a character value */
ZH_FUNC( CHR )
{
   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      PZH_CODEPAGE cdp = zh_vmCodepage();
      if( ZH_CODEPAGE_ISCHARUNI( cdp ) )
      {
         char szChar[ ZH_MAX_CHAR_LEN ];
         ZH_SIZE nLen;

         nLen = zh_cdpTextPutU16( zh_vmCodepage(), szChar, sizeof( szChar ),
                                           ( ZH_WCHAR ) zh_parni( 1 ) );
         zh_retclen( szChar, nLen );
      }
      else
         zh_retclen( zh_szAscii[ zh_parni( 1 ) & 0xFF ], 1 );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1104, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/* converts a character value to an ASCII code */
ZH_FUNC( ASC )
{
   const char * szValue = zh_parc( 1 );

   if( szValue )
   {
      int iChar;
      PZH_CODEPAGE cdp = zh_vmCodepage();
      if( ZH_CODEPAGE_ISCHARUNI( cdp ) )
         iChar = zh_cdpTextGetU16( cdp, szValue, zh_parclen( 1 ) );
      else
         iChar = ( ZH_UCHAR ) szValue[ 0 ];

      zh_retni( iChar );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1107, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}
