/*
 * zh_byte*() functions
 *
 * Copyright 2009 Przemyslaw Czerpak
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
#include "zh_error_api.h"

static ZH_BOOL zh_numParam( int iParam, ZH_MAXINT * plNum )
{
   if( ZH_IS_PARAM_NUM( iParam ) )
   {
      *plNum = zh_parnint( iParam );
      return ZH_TRUE;
   }
   zh_errRT_BASE_SubstR( EG_ARG, 1089, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
   *plNum = 0;
   return ZH_FALSE;
}

ZH_FUNC( ZH_BYTESWAPI )
{
   ZH_MAXINT lValue;

   if( zh_numParam( 1, &lValue ) )
   {
      ZH_I16 iVal = ( ZH_I16 ) ZH_SWAP_UINT16( lValue );
      zh_retnint( iVal );
   }
}

ZH_FUNC( ZH_BYTESWAPW )
{
   ZH_MAXINT lValue;

   if( zh_numParam( 1, &lValue ) )
   {
      ZH_U16 uiVal = ( ZH_U16 ) ZH_SWAP_UINT16( lValue );
      zh_retnint( uiVal );
   }
}

ZH_FUNC( ZH_BYTESWAPL )
{
   ZH_MAXINT lValue;

   if( zh_numParam( 1, &lValue ) )
   {
      ZH_I32 iVal = ( ZH_I32 ) ZH_SWAP_UINT32( lValue );
      zh_retnint( iVal );
   }
}

ZH_FUNC( ZH_BYTESWAPU )
{
   ZH_MAXINT lValue;

   if( zh_numParam( 1, &lValue ) )
   {
      ZH_U32 uiVal = ( ZH_U32 ) ZH_SWAP_UINT32( lValue );
      zh_retnint( uiVal );
   }
}

ZH_FUNC( ZH_BYTESWAPLL )
{
   ZH_MAXINT lValue;

   if( zh_numParam( 1, &lValue ) )
   {
#if defined( ZH_LONG_LONG_OFF )
      ZH_MAXINT iVal = ( ZH_MAXINT ) ZH_SWAP_UINT32( lValue );
#else
      ZH_MAXINT iVal = ( ZH_MAXINT ) ZH_SWAP_UINT64( lValue );
#endif
      zh_retnint( iVal );
   }
}
