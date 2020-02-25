/*
 * CT3 Miscellaneous functions: KbdStat()
 *
 * Copyright 2005 Pavel Tsarenko <tpe2@mail.ru>
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
#include "zh_gt_api.h"
#include "zh_item_api.h"

ZH_FUNC( KBDSTAT )
{
   int iRet = 0;
   ZH_GT_INFO gtInfo;

   memset( &gtInfo, 0, sizeof( gtInfo ) );

   zh_gtInfo( ZH_GTI_KBDSHIFTS, &gtInfo );

   if( gtInfo.pResult )
   {
      int iState = zh_itemGetNI( gtInfo.pResult );

      zh_itemRelease( gtInfo.pResult );
      if( iState & ZH_GTI_KBD_SHIFT )
         iRet |= 0x01;
      if( iState & ZH_GTI_KBD_CTRL )
         iRet |= 0x04;
      if( iState & ZH_GTI_KBD_ALT )
         iRet |= 0x08;
      if( iState & ZH_GTI_KBD_SCROLOCK )
         iRet |= 0x10;
      if( iState & ZH_GTI_KBD_NUMLOCK )
         iRet |= 0x20;
      if( iState & ZH_GTI_KBD_CAPSLOCK )
         iRet |= 0x40;
      if( iState & ZH_GTI_KBD_INSERT )
         iRet |= 0x80;
   }

   zh_retni( iRet );
}
