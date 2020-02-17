/*
 * SIX compatible functions:
 *       sx_MakeSem()
 *       sx_KillSem()
 *       sx_IsSem()
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
#include "zh_item_api.h"
#include "zh_apifs.h"
#include "zh_rdd_api.h"


static ZH_BOOL zh_sxSemName( char * szFileName )
{
   const char * szName = zh_parc( 1 );
   ZH_BOOL fResult = ZH_FALSE;

   if( szName && szName[ 0 ] )
   {
      zh_cdpnDup2Lower( zh_vmCDP(), szName, strlen( szName ),
                        szFileName, ZH_PATH_MAX );
      szFileName[ ZH_PATH_MAX - 1 ] = '\0';
      fResult = ZH_TRUE;
   }
   else
   {
      AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

      if( pArea )
      {
         DBORDERINFO pOrderInfo;

         memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
         pOrderInfo.itmOrder = zh_param( 1, ZH_IT_NUMERIC );
         if( pOrderInfo.itmOrder && zh_itemGetNI( pOrderInfo.itmOrder ) == 0 )
            pOrderInfo.itmOrder = NULL;
         pOrderInfo.itmResult = zh_itemPutC( NULL, NULL );
         SELF_ORDINFO( pArea, DBOI_NAME, &pOrderInfo );
         szName = zh_itemGetCPtr( pOrderInfo.itmResult );
         if( szName && szName[ 0 ] )
         {
            zh_cdpnDup2Lower( zh_vmCDP(), szName, strlen( szName ),
                              szFileName, ZH_PATH_MAX );
            szFileName[ ZH_PATH_MAX - 1 ] = '\0';
            fResult = ZH_TRUE;
         }
         zh_itemRelease( pOrderInfo.itmResult );
      }
   }

   return fResult;
}

static PZH_FILE zh_sxSemOpen( char * szFileName, ZH_BOOL * pfNewFile )
{
   PZH_FILE pFile;
   int i = 0;

   do
   {
      pFile = zh_fileExtOpen( szFileName, ".sem",
                              FO_READWRITE | FO_EXCLUSIVE | FXO_DEFAULTS |
                              FXO_SHARELOCK | FXO_COPYNAME, NULL, NULL );
      if( pFile != NULL )
         break;

      if( pfNewFile )
      {
         pFile = zh_fileExtOpen( szFileName, ".sem", FXO_UNIQUE |
                                 FO_READWRITE | FO_EXCLUSIVE | FXO_DEFAULTS |
                                 FXO_SHARELOCK | FXO_COPYNAME, NULL, NULL );
         if( pFile != NULL )
         {
            *pfNewFile = ZH_TRUE;
            break;
         }
      }
      else
      {
         ZH_ERRCODE errCode = zh_fsError();
         if( errCode != 5 && errCode != 32 && errCode != 33 )
            break;
      }

      zh_idleSleep( 0.01 );
   }
   while( ++i < 25 );

   return pFile;
}


ZH_FUNC( SX_MAKESEM )
{
   char szFileName[ ZH_PATH_MAX ];
   int iUsers = -1;
   ZH_BOOL fError = ZH_FALSE, fNewFile = ZH_FALSE;

   if( zh_sxSemName( szFileName ) )
   {
      PZH_FILE pFile = zh_sxSemOpen( szFileName, &fNewFile );

      if( pFile != NULL )
      {
         ZH_BYTE buffer[ 2 ];

         if( fNewFile )
            iUsers = 1;
         else
         {
            if( zh_fileReadAt( pFile, buffer, 2, 0 ) != 2 )
               fError = ZH_TRUE;
            else
               iUsers = ZH_GET_LE_INT16( buffer ) + 1;
         }
         if( ! fError )
         {
            ZH_PUT_LE_UINT16( buffer, iUsers );
            if( zh_fileWriteAt( pFile, buffer, 2, 0 ) != 2 )
               fError = ZH_TRUE;
         }
         zh_fileClose( pFile );
      }
   }
   if( fError )
      iUsers = -1;
   zh_retni( iUsers );
}


ZH_FUNC( SX_KILLSEM )
{
   char szFileName[ ZH_PATH_MAX ];
   int iUsers = -1;

   if( zh_sxSemName( szFileName ) )
   {
      PZH_FILE pFile = zh_sxSemOpen( szFileName, NULL );

      if( pFile != NULL )
      {
         ZH_BYTE buffer[ 2 ];
         if( zh_fileReadAt( pFile, buffer, 2, 0 ) == 2 )
         {
            iUsers = ZH_GET_LE_INT16( buffer ) - 1;
            ZH_PUT_LE_UINT16( buffer, iUsers );
            zh_fileWriteAt( pFile, buffer, 2, 0 );
         }
         zh_fileClose( pFile );
         if( iUsers == 0 )
            zh_fileDelete( szFileName );
      }
   }
   zh_retni( iUsers );
}


ZH_FUNC( SX_ISSEM )
{
   char szFileName[ ZH_PATH_MAX ];
   PZH_FILE pFile = NULL;

   if( zh_sxSemName( szFileName ) )
   {
      pFile = zh_sxSemOpen( szFileName, NULL );
      if( pFile != NULL )
         zh_fileClose( pFile );
   }

   zh_retl( pFile != NULL );
}
