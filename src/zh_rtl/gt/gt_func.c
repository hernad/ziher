/*
 * Ziher extended GT functions
 *
 * Copyright 2006 Przemyslaw Czerpak
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
#include "zh_error_api.h"

ZH_FUNC( ZH_SETDISPCP )
{
   if( ZH_ISCHAR( 1 ) )
   {
      if( zh_pcount() == 2 && ZH_ISLOGICAL( 2 ) )
         zh_gtSetDisplayCodepage( zh_parc( 1 ), NULL, zh_parl( 2 ) );
      else
         zh_gtSetDisplayCodepage( zh_parc( 1 ), zh_parc( 2 ), zh_parl( 3 ) );
   }
   else if( ! ( zh_pcount() >= 1 && ZH_ISNIL( 1 ) ) )
      zh_errRT_BASE_SubstR( EG_ARG, 1089, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_SETKEYCP )
{
   if( ZH_ISCHAR( 1 ) )
      zh_gtSetKeyboardCodepage( zh_parc( 1 ), zh_parc( 2 ) );
   else if( ! ( zh_pcount() >= 1 && ZH_ISNIL( 1 ) ) )
      zh_errRT_BASE_SubstR( EG_ARG, 1089, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_SETTERMCP )
{
   if( ZH_ISCHAR( 1 ) )
   {
      if( zh_pcount() == 2 && ZH_ISLOGICAL( 2 ) )
      {
         zh_gtSetDisplayCodepage( zh_parc( 1 ), NULL, zh_parl( 2 ) );
         zh_gtSetKeyboardCodepage( zh_parc( 1 ), NULL );
      }
      else
      {
         zh_gtSetDisplayCodepage( zh_parc( 1 ), zh_parc( 2 ), zh_parl( 3 ) );
         zh_gtSetKeyboardCodepage( zh_parc( 1 ), zh_parc( 2 ) );
      }
   }
   else if( ! ( zh_pcount() >= 1 && ZH_ISNIL( 1 ) ) )
      zh_errRT_BASE_SubstR( EG_ARG, 1089, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_GTINFO )
{
   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      ZH_GT_INFO gtInfo;

      gtInfo.pNewVal  = zh_param( 2, ZH_IT_ANY );
      gtInfo.pNewVal2 = zh_param( 3, ZH_IT_ANY );
      gtInfo.pResult  = NULL;

      zh_gtInfo( zh_parni( 1 ), &gtInfo );
      if( gtInfo.pResult )
         zh_itemReturnRelease( gtInfo.pResult );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_GTVERSION )
{
   zh_retc_const( zh_gtVersion( zh_parni( 1 ) ) );
}

ZH_FUNC( ZH_GTALERT )
{
   zh_retni( zh_gtAlert( zh_param( 1, ZH_IT_ANY ),
                         zh_param( 2, ZH_IT_ANY ),
                         ZH_ISCHAR( 3 ) ? zh_gtColorToN( zh_parc( 3 ) ) : zh_parni( 3 ) /* iClrNorm */,
                         ZH_ISCHAR( 4 ) ? zh_gtColorToN( zh_parc( 4 ) ) : zh_parni( 4 ) /* iClrHigh */,
                         zh_parnd( 5 ) ) );
}

ZH_FUNC( ZH_GFXPRIMITIVE )
{
   zh_retni( zh_gtGfxPrimitive( zh_parni( 1 ) /* nType   */,
                                zh_parni( 2 ) /* nTop    */,
                                zh_parni( 3 ) /* nLeft   */,
                                zh_parni( 4 ) /* nBottom */,
                                zh_parni( 5 ) /* nRight  */,
                                zh_parni( 6 ) /* nColor  */ ) );
}

ZH_FUNC( ZH_GFXTEXT )
{
   zh_gtGfxText( zh_parni( 1 ) /* nTop   */,
                 zh_parni( 2 ) /* nLeft  */,
                 zh_parc( 3 ) /* cText  */,
                 zh_parni( 4 ) /* nColor */,
                 zh_parni( 5 ) /* nSize  */,
                 zh_parni( 6 ) /* nWidth */ );
}

ZH_FUNC( ZH_GTLOCK )
{
   zh_retl( zh_gtLock() );
}

ZH_FUNC( ZH_GTUNLOCK )
{
   zh_gtUnlock();
}
