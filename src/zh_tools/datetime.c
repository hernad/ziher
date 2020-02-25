/*
 * CT3 Date & Time functions: BoM() / EoM(), BoQ() / EoQ(), BoY() / EoY(), WoM()
 *
 * Copyright 2005 Pavel Tsarenko <tpe2@mail.ru>
 * Copyright 1999 Jose Lalin <dezac@corevia.com> (WoM())
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
#include "zh_date.h"

ZH_FUNC( BOM )
{
   long lDate;
   int iYear, iMonth, iDay;

   if( ZH_ISNIL( 1 ) )
   {
      zh_dateToday( &iYear, &iMonth, &iDay );
      lDate = zh_dateEncode( iYear, iMonth, iDay );
   }
   else
      lDate = zh_pardl( 1 );

   if( lDate != 0 )
   {
      zh_dateDecode( lDate, &iYear, &iMonth, &iDay );
      zh_retd( iYear, iMonth, 1 );
   }
   else
      zh_retdl( 0 );
}

ZH_FUNC( EOM )
{
   long lDate;
   int iYear, iMonth, iDay;

   if( ZH_ISNIL( 1 ) )
   {
      zh_dateToday( &iYear, &iMonth, &iDay );
      lDate = zh_dateEncode( iYear, iMonth, iDay );
   }
   else
      lDate = zh_pardl( 1 );

   if( lDate != 0 )
   {
      zh_dateDecode( lDate, &iYear, &iMonth, &iDay );
      iMonth++;
      if( iMonth > 12 )
      {
         iMonth = 1;
         iYear++;
      }
      zh_retdl( zh_dateEncode( iYear, iMonth, 1 ) - 1 );
   }
   else
      zh_retdl( 0 );
}

ZH_FUNC( BOQ )
{
   long lDate;
   int iYear, iMonth, iDay;

   if( ZH_ISNIL( 1 ) )
   {
      zh_dateToday( &iYear, &iMonth, &iDay );
      lDate = zh_dateEncode( iYear, iMonth, iDay );
   }
   else
      lDate = zh_pardl( 1 );

   if( lDate != 0 )
   {
      zh_dateDecode( lDate, &iYear, &iMonth, &iDay );
      iMonth -= ( iMonth - 1 ) % 3;

      zh_retd( iYear, iMonth, 1 );
   }
   else
      zh_retdl( 0 );
}

ZH_FUNC( EOQ )
{
   long lDate;
   int iYear, iMonth, iDay;

   if( ZH_ISNIL( 1 ) )
   {
      zh_dateToday( &iYear, &iMonth, &iDay );
      lDate = zh_dateEncode( iYear, iMonth, iDay );
   }
   else
      lDate = zh_pardl( 1 );

   if( lDate != 0 )
   {
      zh_dateDecode( lDate, &iYear, &iMonth, &iDay );
      iMonth += 3 - ( ( iMonth - 1 ) % 3 );
      if( iMonth > 12 )
      {
         iMonth = 1;
         iYear++;
      }
      zh_retdl( zh_dateEncode( iYear, iMonth, 1 ) - 1 );
   }
   else
      zh_retdl( 0 );
}

ZH_FUNC( BOY )
{
   long lDate;
   int iYear, iMonth, iDay;

   if( ZH_ISNIL( 1 ) )
   {
      zh_dateToday( &iYear, &iMonth, &iDay );
      lDate = zh_dateEncode( iYear, iMonth, iDay );
   }
   else
      lDate = zh_pardl( 1 );

   if( lDate != 0 )
   {
      zh_dateDecode( lDate, &iYear, &iMonth, &iDay );
      zh_retd( iYear, 1, 1 );
   }
   else
      zh_retdl( 0 );
}

ZH_FUNC( EOY )
{
   long lDate;
   int iYear, iMonth, iDay;

   if( ZH_ISNIL( 1 ) )
   {
      zh_dateToday( &iYear, &iMonth, &iDay );
      lDate = zh_dateEncode( iYear, iMonth, iDay );
   }
   else
      lDate = zh_pardl( 1 );

   if( lDate != 0 )
   {
      zh_dateDecode( lDate, &iYear, &iMonth, &iDay );
      zh_retdl( zh_dateEncode( iYear + 1, 1, 1 ) - 1 );
   }
   else
      zh_retdl( 0 );
}

static int zh_wom( int iYear, int iMonth, int iDay )
{
   int iWom;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_wom(%d, %d, %d)", iYear, iMonth, iDay ) );

   iWom = iDay + zh_dateDOW( iYear, iMonth, 1 ) - 1;
   if( iWom > 0 )
      return ( iWom - zh_dateDOW( iYear, iMonth, iDay ) ) / 7 + 1;
   else
      return 0;
}

ZH_FUNC( WOM )
{
   long lDate;
   int iYear, iMonth, iDay;

   if( ZH_ISNIL( 1 ) )
   {
      zh_dateToday( &iYear, &iMonth, &iDay );
      lDate = zh_dateEncode( iYear, iMonth, iDay );
   }
   else
      lDate = zh_pardl( 1 );

   if( lDate != 0 )
   {
      zh_dateDecode( lDate, &iYear, &iMonth, &iDay );
      zh_retni( zh_wom( iYear, iMonth, iDay ) );
   }
   else
      zh_retni( 0 );
}
