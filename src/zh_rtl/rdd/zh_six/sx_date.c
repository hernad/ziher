/*
 * SIX compatible functions:
 *       zh_sxDtoP()
 *       zh_sxPtoD()
 *
 *       sx_DToP()
 *       sx_PToD()
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
#include "zh_apifs.h"
#include "zh_date.h"
#include "zh_sx_func.h"

char * zh_sxDtoP( char * pDate, long lJulian )
{
   int iYear, iMonth, iDay;
   long lPDate;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_sxDtoP(%p, %ld)", ( void * ) pDate, lJulian ) );

   zh_dateDecode( lJulian, &iYear, &iMonth, &iDay );
   lPDate = ( ( ( iYear << 1 ) | ( iMonth >> 3 ) ) << 8 ) |
            ( ( iMonth & 7 ) << 5 ) | iDay;
   ZH_PUT_BE_UINT24( pDate, lPDate );

   return pDate;
}

long zh_sxPtoD( const char * pDate )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_sxPtoD(%p)", ( const void * ) pDate ) );

   if( pDate )
   {
      int iYear, iMonth, iDay;
      long lPDate;

      lPDate = ZH_GET_BE_UINT24( pDate );
      iDay = lPDate & 0x1f;
      iMonth = ( lPDate >> 5 ) & 0x0f;
      iYear = ( lPDate >> 9 );

      return zh_dateEncode( iYear, iMonth, iDay );
   }
   return 0;
}

ZH_FUNC( SX_DTOP )
{
   char pDate[ 3 ];

   zh_retclen( zh_sxDtoP( pDate, zh_pardl( 1 ) ), 3 );
}

ZH_FUNC( SX_PTOD )
{
   zh_retdl( zh_sxPtoD( zh_parclen( 1 ) < 3 ? NULL : zh_parc( 1 ) ) );
}
