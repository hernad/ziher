/*
 * ZLIB compression for Ziher zh_inet*() connections
 *
 * Copyright 2010 Przemyslaw Czerpak
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
#include "../zh_socket.h"
#include "zh_znet.h"
#include "zlib.zhh"

/* this function is intentionally not in hbinet.c to not create binding
 * to ZLIB if user does not use it
 */
ZH_FUNC( ZH_INETCOMPRESS )
{
   PZH_ITEM pItem = zh_param( 1, ZH_IT_POINTER );
   int iLevel = zh_parnidef( 2, ZH_ZLIB_COMPRESSION_DEFAULT ),
       iStrategy = zh_parnidef( 3, ZH_ZLIB_STRATEGY_DEFAULT );

   if( iLevel == ZH_ZLIB_COMPRESSION_DISABLE )
      zh_znetInetInitialize( pItem, NULL, NULL, NULL, NULL, NULL, NULL, NULL );
   else
   {
      PZH_ZNETSTREAM pStream = zh_znetOpen( iLevel, iStrategy );
      if( pStream == NULL )
         pItem = NULL;  /* to force RTE */
      if( zh_znetInetInitialize( pItem, pStream, zh_znetRead, zh_znetWrite,
                                 zh_znetFlush, zh_znetClose, NULL, NULL ) )
      {
         int keylen = ( int ) zh_parclen( 4 );
         if( keylen )
            zh_znetEncryptKey( pStream, zh_parc( 4 ), keylen );
      }
      else if( pStream )
         zh_znetClose( pStream );
   }
}
