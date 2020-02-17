/*
 * NATION undocumented functions
 *
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
 * Copyright 1999-2001 Viktor Szakats (__natSortVer(), __natMsgVer())
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
#include "zh_lang_api.h"
#include "zh_codepage_api.h"

#define _DIR_HEADER      1              /* "Database Files    # Records    Last Update     Size" */
#define _LF_SAMPLES      2              /* "Do you want more samples?" */
#define _RF_PAGENO       3              /* "Page No." */
#define _RF_SUBTOTAL     4              /* "** Subtotal **" */
#define _RF_SUBSUBTOTAL  5              /* "* Subsubtotal *" */
#define _RF_TOTAL        6              /* "*** Total ***" */
#define _GET_INSERT_ON   7              /* "Ins" */
#define _GET_INSERT_OFF  8              /* "   " */
#define _GET_INVD_DATE   9              /* "Invalid Date" */
#define _GET_RANGE_FROM  10             /* "Range: " */
#define _GET_RANGE_TO    11             /* " - " */
#define _LF_YN           12             /* "Y/N" */ /* NOTE: This must be in uppercase. [vszakats] */
#define _INVALID_EXPR    13             /* "INVALID EXPRESSION" */

static const char * zh_nationGetMsg( int iMsg )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_nationGetMsg(%u)", iMsg ) );

   return ( iMsg >= 1 && iMsg <= 13 ) ? zh_langDGetItem( ZH_LANG_ITEM_BASE_NATMSG + iMsg - 1 ) : "";
}

ZH_FUNC( __NATISAFFIRM )
{
   ZH_SIZE nLen = zh_parclen( 1 );
   ZH_BOOL fIS = ZH_FALSE;

   if( nLen > 0 )
   {
      const char * szYesNo = zh_langDGetItem( ZH_LANG_ITEM_BASE_NATMSG + _LF_YN - 1 );
      ZH_SIZE nStr = 0;

      while( szYesNo[ nStr ] && szYesNo[ nStr ] != '/' )
         ++nStr;

      if( nStr && nLen >= nStr )
      {
         PZH_CODEPAGE cdp = zh_vmCDP();
         if( cdp )
            fIS = zh_cdpicmp( zh_parc( 1 ), nLen, szYesNo, nStr, cdp, ZH_FALSE ) == 0;
         else
            fIS = zh_strnicmp( zh_parc( 1 ), szYesNo, nStr ) == 0;
      }
   }
   zh_retl( fIS );
}

ZH_FUNC( __NATISNEGATIVE )
{
   ZH_SIZE nLen = zh_parclen( 1 );
   ZH_BOOL fIS = ZH_FALSE;

   if( nLen > 0 )
   {
      const char * szYesNo = zh_langDGetItem( ZH_LANG_ITEM_BASE_NATMSG + _LF_YN - 1 );
      ZH_SIZE nStr;

      while( *szYesNo )
         if( *szYesNo++ == '/' )
            break;
      nStr = strlen( szYesNo );

      if( nStr && nLen >= nStr )
      {
         PZH_CODEPAGE cdp = zh_vmCDP();
         if( cdp )
            fIS = zh_cdpicmp( zh_parc( 1 ), nLen, szYesNo, nStr, cdp, ZH_FALSE ) == 0;
         else
            fIS = zh_strnicmp( zh_parc( 1 ), szYesNo, nStr ) == 0;
      }
   }
   zh_retl( fIS );
}

ZH_FUNC( __NATMSG )
{
   if( zh_pcount() == 0 )
      /* TODO: Replace this with Language API call. */
      zh_retc_const( "Invalid argument" );
   else if( ZH_IS_PARAM_NUM( 1 ) )
      zh_retc_const( zh_nationGetMsg( zh_parni( 1 ) ) );
   else
      zh_retc_null();
}

ZH_FUNC( __NATMSGVER )
{
   /* NOTE: CA-Cl*pper 5.2e Intl. will return: "NATMSGS v1.2i x14 19/Mar/93" */
   /* NOTE: CA-Cl*pper 5.3  Intl. will return: "NATMSGS v1.3i x19 06/Mar/95" */

   zh_retc_const( "NATMSGS (Ziher)" );
}
