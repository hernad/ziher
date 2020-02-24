/*
 * SIX compatible function:
 *       sx_TagOrder() *
 *       sx_TagNo()
 *       sx_Freeze()
 *       sx_Warm()
 *       sx_Chill()
 *       sx_Thermometer()
 *       sx_ClrScope()
 *       sx_SetScope()
 *       sx_IsReindex()
 *       sx_Step()
 *       sx_KeySincluded()
 *       sx_I_IndexName()
 *       sx_I_TagName()
 *       sx_IndexCount()
 *       sx_IndexName()
 *       sx_IndexType()
 *       sx_KeyAdd()
 *       sx_KeyDrop()
 *       sx_KeyData()
 *       sx_KeySkip()
 *       sx_KeyCount()
 *       sx_KeyNo()
 *       sx_KeyGoto()
 *       sx_SkipUnique()
 *       sx_SeekLast()
 *       sx_TagUnique()
 *       sx_WildSeek()
 *       sx_ROXLock()
 *       sx_ROXUnlock()
 *       sx_IsMyROX()
 *       sx_IsROXLock()
 *       sx_SortOption()
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
#include "zh_fs_api.h"
#include "zh_rdd_api.h"

static ZH_BOOL zh_sxOrdParam( LPDBORDERINFO pInfo )
{
   memset( pInfo, 0, sizeof( DBORDERINFO ) );

   if( ZH_ISCHAR( 1 ) )
   {
      pInfo->itmOrder = zh_param( 1, ZH_IT_STRING );
      pInfo->atomBagName = zh_param( 2, ZH_IT_STRING );
   }
   else if( ZH_IS_PARAM_NUM( 1 ) )
   {
      pInfo->itmOrder = zh_param( 1, ZH_IT_NUMERIC );
      if( ! ZH_ISNIL( 2 ) ) /* zh_pcount() > 2 */
      {
         pInfo->atomBagName = zh_param( 2, ZH_IT_NUMERIC );
         if( zh_parni( 2 ) <= 0 )
            return ZH_FALSE;
      }
   }
   return ZH_TRUE;
}

ZH_FUNC( SX_TAGORDER )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
   int iOrder = 0;

   if( pArea )
   {
      DBORDERINFO Info;

      if( zh_sxOrdParam( &Info ) )
      {
         Info.itmResult = zh_itemPutNI( NULL, 0 );
         SELF_ORDINFO( pArea, DBOI_NUMBER, &Info );
         iOrder = zh_itemGetNI( Info.itmResult );
         zh_itemRelease( Info.itmResult );
      }
   }

   zh_retni( iOrder );
}

/*
 * sx_TagNo( tag, bag ) --> nTagPosInBag
 * returns order position in order bag
 */
ZH_FUNC( SX_TAGNO )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
   int iBagOrder = 0;

   if( pArea )
   {
      DBORDERINFO Info;

      if( zh_sxOrdParam( &Info ) )
      {
         Info.itmResult = zh_itemPutNI( NULL, 0 );
         if( SELF_ORDINFO( pArea, DBOI_NUMBER, &Info ) == ZH_SUCCESS )
         {
            int iOrder = zh_itemGetNI( Info.itmResult );
            if( iOrder )
            {
               Info.itmOrder = zh_itemPutNI( NULL, iOrder );
               Info.atomBagName = NULL;
               zh_itemClear( Info.itmResult );
               if( SELF_ORDINFO( pArea, DBOI_FULLPATH, &Info ) == ZH_SUCCESS &&
                   zh_itemGetCLen( Info.itmResult ) > 0 )
               {
                  Info.atomBagName = Info.itmResult;
                  Info.itmResult = Info.itmOrder;
                  Info.itmOrder = NULL;
                  zh_itemClear( Info.itmResult );
                  if( SELF_ORDINFO( pArea, DBOI_BAGORDER, &Info ) == ZH_SUCCESS )
                     iBagOrder = iOrder - zh_itemGetNI( Info.itmResult ) + 1;
                  Info.itmOrder = Info.atomBagName;
               }
               zh_itemRelease( Info.itmOrder );
            }
         }
         zh_itemRelease( Info.itmResult );
      }
   }

   zh_retni( iBagOrder );
}

ZH_FUNC( SX_FREEZE )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO Info;

      if( zh_sxOrdParam( &Info ) )
      {
         ZH_BOOL fResult = ZH_FALSE;
         Info.itmNewVal = zh_itemPutL( NULL, ZH_TRUE );
         Info.itmResult = zh_itemNew( NULL );
         if( SELF_ORDINFO( pArea, DBOI_CUSTOM, &Info ) == ZH_SUCCESS )
            fResult = ZH_IS_LOGICAL( Info.itmResult ) &&
                      zh_itemGetL( Info.itmResult );
         zh_itemRelease( Info.itmNewVal );
         zh_itemRelease( Info.itmResult );
         zh_retl( fResult );
      }
   }
}

ZH_FUNC( SX_WARM )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO Info;

      if( zh_sxOrdParam( &Info ) )
      {
         ZH_BOOL fResult = ZH_FALSE;
         Info.itmNewVal = zh_itemPutL( NULL, ZH_FALSE );
         Info.itmResult = zh_itemNew( NULL );
         if( SELF_ORDINFO( pArea, DBOI_CHGONLY, &Info ) == ZH_SUCCESS )
            fResult = ZH_IS_LOGICAL( Info.itmResult ) &&
                      ! zh_itemGetL( Info.itmResult );
         zh_itemRelease( Info.itmNewVal );
         zh_itemRelease( Info.itmResult );
         zh_retl( fResult );
      }
   }
}

ZH_FUNC( SX_CHILL )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO Info;

      if( zh_sxOrdParam( &Info ) )
      {
         ZH_BOOL fResult = ZH_FALSE;
         Info.itmNewVal = zh_itemPutL( NULL, ZH_TRUE );
         Info.itmResult = zh_itemNew( NULL );
         if( SELF_ORDINFO( pArea, DBOI_CHGONLY, &Info ) == ZH_SUCCESS )
            fResult = ZH_IS_LOGICAL( Info.itmResult ) &&
                      zh_itemGetL( Info.itmResult );
         zh_itemRelease( Info.itmNewVal );
         zh_itemRelease( Info.itmResult );
         zh_retl( fResult );
      }
   }
}

/*
 * 1 - Full Update
 * 2 - Full Update (partial index)
 * 3 - Changes Only
 * 4 - No Update
 * -1 - not table or no order
 */
ZH_FUNC( SX_THERMOMETER )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
   int iTemperature = -1;

   if( pArea )
   {
      DBORDERINFO Info;

      if( zh_sxOrdParam( &Info ) )
      {
         int i;
         Info.itmResult = zh_itemPutNI( NULL, 0 );
         SELF_ORDINFO( pArea, DBOI_NUMBER, &Info );
         i = zh_itemGetNI( Info.itmResult );
         if( i )
         {
            static const ZH_USHORT s_iStates[] =
                     { DBOI_CUSTOM, DBOI_CHGONLY, DBOI_PARTIAL };
            iTemperature = 4;
            for( i = 0; i < 3; ++i, --iTemperature )
            {
               zh_itemClear( Info.itmResult );
               if( SELF_ORDINFO( pArea, s_iStates[ i ], &Info ) == ZH_SUCCESS &&
                   ZH_IS_LOGICAL( Info.itmResult ) &&
                   zh_itemGetL( Info.itmResult ) )
                  break;
            }
         }
         zh_itemRelease( Info.itmResult );
      }
   }

   zh_retni( iTemperature );
}

ZH_FUNC( SX_CLRSCOPE )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO Info;

      if( zh_sxOrdParam( &Info ) )
      {
         int iScope = zh_parnidef( 1, 2 );
         Info.itmResult = zh_itemNew( NULL );
         if( iScope )
            SELF_ORDINFO( pArea, DBOI_SCOPEBOTTOMCLEAR, &Info );
         if( iScope == 0 || iScope == 2 )
            SELF_ORDINFO( pArea, DBOI_SCOPETOPCLEAR, &Info );
         zh_itemRelease( Info.itmResult );
      }
   }
}

ZH_FUNC( SX_SETSCOPE )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO Info;

      if( zh_sxOrdParam( &Info ) )
      {
         int iScope = zh_parni( 1 );
         Info.itmResult = zh_itemNew( NULL );
         if( ! ZH_ISNIL( 2 ) )
            Info.itmNewVal = zh_param( 2, ZH_IT_ANY );
         SELF_ORDINFO( pArea, ( ZH_USHORT ) ( iScope ? DBOI_SCOPEBOTTOM : DBOI_SCOPETOP ), &Info );
         zh_itemReturnRelease( Info.itmResult );
      }
   }
}

ZH_FUNC( SX_ISREINDEX )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
   ZH_BOOL fReindex = ZH_FALSE;

   if( pArea )
   {
      DBORDERINFO Info;
      memset( &Info, 0, sizeof( Info ) );
      Info.itmResult = zh_itemNew( NULL );
      SELF_ORDINFO( pArea, DBOI_ISREINDEX, &Info );
      fReindex = zh_itemGetL( Info.itmResult );
      zh_itemRelease( Info.itmResult );
   }

   zh_retl( fReindex );
}

ZH_FUNC( SX_STEP )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
   ZH_LONG lStep = 0;

   if( pArea )
   {
      DBORDERINFO Info;
      memset( &Info, 0, sizeof( Info ) );
      Info.itmResult = zh_itemNew( NULL );
      SELF_ORDINFO( pArea, DBOI_EVALSTEP, &Info );
      lStep = zh_itemGetNL( Info.itmResult );
      zh_itemRelease( Info.itmResult );
   }

   zh_retnint( lStep );
}

ZH_FUNC( SX_KEYSINCLUDED )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
   ZH_ULONG ulKeys = 0;

   if( pArea )
   {
      DBORDERINFO Info;
      memset( &Info, 0, sizeof( Info ) );
      Info.itmResult = zh_itemNew( NULL );
      SELF_ORDINFO( pArea, DBOI_KEYSINCLUDED, &Info );
      ulKeys = zh_itemGetNL( Info.itmResult );
      zh_itemRelease( Info.itmResult );
   }

   zh_retnint( ulKeys );
}

ZH_FUNC( SX_I_INDEXNAME )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO Info;
      memset( &Info, 0, sizeof( Info ) );
      Info.itmResult = zh_itemNew( NULL );
      SELF_ORDINFO( pArea, DBOI_I_BAGNAME, &Info );
      zh_itemReturnRelease( Info.itmResult );
      return;
   }

   zh_retc_null();
}

ZH_FUNC( SX_I_TAGNAME )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO Info;
      memset( &Info, 0, sizeof( Info ) );
      Info.itmResult = zh_itemNew( NULL );
      SELF_ORDINFO( pArea, DBOI_I_TAGNAME, &Info );
      zh_itemReturnRelease( Info.itmResult );
      return;
   }

   zh_retc_null();
}

ZH_FUNC( SX_INDEXCOUNT )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
   int iCount = 0;

   if( pArea )
   {
      DBORDERINFO Info;
      memset( &Info, 0, sizeof( Info ) );
      Info.itmResult = zh_itemNew( NULL );
      SELF_ORDINFO( pArea, DBOI_BAGCOUNT, &Info );
      iCount = zh_itemGetNI( Info.itmResult );
      zh_itemRelease( Info.itmResult );
   }

   zh_retni( iCount );
}

ZH_FUNC( SX_INDEXNAME )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO Info;
      if( zh_sxOrdParam( &Info ) )
      {
         Info.itmResult = zh_itemNew( NULL );
         SELF_ORDINFO( pArea, DBOI_FULLPATH, &Info );
         zh_itemReturnRelease( Info.itmResult );
      }
      else
         zh_retc_null();
   }
}

ZH_FUNC( SX_INDEXTYPE )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
   int iType = DBOI_TYPE_UNDEF;

   if( pArea )
   {
      DBORDERINFO Info;
      if( zh_sxOrdParam( &Info ) )
      {
         if( zh_pcount() == 1 && ZH_ISCHAR( 1 ) )
         {
            Info.atomBagName = Info.itmOrder;
            Info.itmOrder = NULL;
         }
         Info.itmResult = zh_itemNew( NULL );
         if( SELF_ORDINFO( pArea, DBOI_INDEXTYPE, &Info ) == ZH_SUCCESS )
            iType = zh_itemGetNI( Info.itmResult );
         zh_itemRelease( Info.itmResult );
      }
   }
   zh_retni( iType );
}

ZH_FUNC( SX_DESCEND )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO Info;
      if( zh_sxOrdParam( &Info ) )
      {
         Info.itmResult = zh_itemNew( NULL );
         if( SELF_ORDINFO( pArea, DBOI_ISDESC, &Info ) == ZH_SUCCESS )
         {
            Info.itmNewVal = zh_itemPutL( NULL, ! zh_itemGetL( Info.itmResult ) );
            SELF_ORDINFO( pArea, DBOI_ISDESC, &Info );
            zh_itemRelease( Info.itmNewVal );
         }
         zh_itemRelease( Info.itmResult );
      }
   }
}

ZH_FUNC( SX_KEYADD )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
   ZH_BOOL fResult = ZH_FALSE;

   if( pArea )
   {
      DBORDERINFO Info;
      if( zh_sxOrdParam( &Info ) )
      {
         Info.itmResult = zh_itemPutL( NULL, ZH_FALSE );
         Info.itmNewVal = zh_param( 3, ZH_IT_ANY );
         SELF_ORDINFO( pArea, DBOI_KEYADD, &Info );
         fResult = zh_itemGetL( Info.itmResult );
         zh_itemRelease( Info.itmResult );
      }
   }
   zh_retl( fResult );
}

ZH_FUNC( SX_KEYDROP )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
   ZH_BOOL fResult = ZH_FALSE;

   if( pArea )
   {
      DBORDERINFO Info;
      if( zh_sxOrdParam( &Info ) )
      {
         Info.itmResult = zh_itemPutL( NULL, ZH_FALSE );
         Info.itmNewVal = zh_param( 3, ZH_IT_ANY );
         SELF_ORDINFO( pArea, DBOI_KEYDELETE, &Info );
         fResult = zh_itemGetL( Info.itmResult );
         zh_itemRelease( Info.itmResult );
      }
   }
   zh_retl( fResult );
}

ZH_FUNC( SX_KEYDATA )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO Info;
      if( zh_sxOrdParam( &Info ) )
      {
         Info.itmResult = zh_itemNew( NULL );
         SELF_ORDINFO( pArea, DBOI_KEYVAL, &Info );
         zh_itemReturnRelease( Info.itmResult );
      }
   }
}

ZH_FUNC( SX_KEYSKIP )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
   ZH_BOOL fResult = ZH_FALSE, fBEof = ZH_FALSE;

   if( pArea )
   {
      if( SELF_SKIPRAW( pArea, zh_parnldef( 1, 1 ) ) == ZH_SUCCESS )
      {
         if( SELF_EOF( pArea, &fBEof ) == ZH_SUCCESS && ! fBEof )
            fResult = SELF_BOF( pArea, &fBEof ) == ZH_SUCCESS && ! fBEof;
      }
   }
   zh_retl( fResult );
}

ZH_FUNC( SX_KEYCOUNT )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
   ZH_ULONG ulKeys = 0;

   if( pArea )
   {
      DBORDERINFO Info;
      if( zh_sxOrdParam( &Info ) )
      {
         Info.itmResult = zh_itemNew( NULL );
         SELF_ORDINFO( pArea, DBOI_KEYCOUNT, &Info );
         ulKeys = zh_itemGetNL( Info.itmResult );
         zh_itemRelease( Info.itmResult );
      }
   }

   zh_retnint( ulKeys );
}

ZH_FUNC( SX_KEYNO )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
   ZH_ULONG ulKeyNo = 0;

   if( pArea )
   {
      DBORDERINFO Info;
      if( zh_sxOrdParam( &Info ) )
      {
         Info.itmResult = zh_itemNew( NULL );
         SELF_ORDINFO( pArea, DBOI_POSITION, &Info );
         ulKeyNo = zh_itemGetNL( Info.itmResult );
         zh_itemRelease( Info.itmResult );
      }
   }

   zh_retnint( ulKeyNo );
}

ZH_FUNC( SX_KEYGOTO )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
   ZH_BOOL fResult = ZH_FALSE;

   if( pArea && zh_parnl( 3 ) != 0 )
   {
      DBORDERINFO Info;
      if( zh_sxOrdParam( &Info ) )
      {
         Info.itmNewVal = zh_param( 3, ZH_IT_NUMERIC );
         Info.itmResult = zh_itemNew( NULL );
         SELF_ORDINFO( pArea, DBOI_POSITION, &Info );
         fResult = zh_itemGetL( Info.itmResult );
         zh_itemRelease( Info.itmResult );
      }
   }

   zh_retl( fResult );
}

ZH_FUNC( SX_SKIPUNIQUE )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO Info;
      memset( &Info, 0, sizeof( Info ) );
      Info.itmNewVal = zh_param( 1, ZH_IT_ANY );
      Info.itmResult = zh_itemNew( NULL );
      SELF_ORDINFO( pArea, DBOI_SKIPUNIQUE, &Info );
      zh_itemRelease( Info.itmResult );
   }
}

ZH_FUNC( SX_SEEKLAST )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
   ZH_BOOL fFound = ZH_FALSE;

   if( pArea && zh_pcount() > 0 )
   {
      PZH_ITEM pKey = zh_param( 1, ZH_IT_ANY );
      ZH_BOOL bSoftSeek = zh_parl( 2 );

      if( SELF_SEEK( pArea, bSoftSeek, pKey, ZH_TRUE ) == ZH_SUCCESS )
      {
         if( SELF_FOUND( pArea, &fFound ) != ZH_SUCCESS )
            fFound = ZH_FALSE;
      }
   }
   zh_retl( fFound );
}

ZH_FUNC( SX_TAGUNIQUE )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO Info;
      if( zh_sxOrdParam( &Info ) )
      {
         Info.itmResult = zh_itemPutL( NULL, ZH_FALSE );
         SELF_ORDINFO( pArea, DBOI_UNIQUE, &Info );
         zh_itemReturnRelease( Info.itmResult );
      }
   }
}

ZH_FUNC( SX_WILDSEEK )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
   const char * szPattern = zh_parc( 1 );
   ZH_BOOL fCont = zh_parl( 2 );
   ZH_BOOL fFound = ZH_FALSE;

   if( pArea )
   {
      int iOrder = 0;

      DBORDERINFO Info;
      memset( &Info, 0, sizeof( Info ) );
      Info.itmResult = zh_itemNew( NULL );

      if( szPattern && szPattern[ 0 ] )
      {
         if( SELF_ORDINFO( pArea, DBOI_NUMBER, &Info ) == ZH_SUCCESS )
            iOrder = zh_itemGetNI( Info.itmResult );
      }
      if( iOrder > 0 )
      {
         ZH_ERRCODE errCode = ZH_SUCCESS;
         if( ! fCont )
         {
            errCode = SELF_GOTOP( pArea );
            if( errCode == ZH_SUCCESS )
            {
               errCode = SELF_ORDINFO( pArea, DBOI_KEYVAL, &Info );
               if( errCode == ZH_SUCCESS )
               {
                  const char * szKey = zh_itemGetCPtr( Info.itmResult );
                  fFound = zh_strMatchWild( szKey, szPattern );
               }
            }
         }
         if( ! fFound && errCode == ZH_SUCCESS )
         {
            Info.itmNewVal = zh_param( 1, ZH_IT_STRING );
            if( SELF_ORDINFO( pArea, DBOI_SKIPWILD, &Info ) == ZH_SUCCESS )
               fFound = ZH_IS_LOGICAL( Info.itmResult ) &&
                        zh_itemGetL( Info.itmResult );
         }
      }
      else
         SELF_GOTO( pArea, 0 );
      zh_itemReturnRelease( Info.itmResult );
   }

   zh_retl( fFound );
}

ZH_FUNC( SX_ROXLOCK )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
   ZH_BOOL fLocked = ZH_FALSE;

   if( pArea )
   {
      DBORDERINFO Info;
      if( zh_sxOrdParam( &Info ) )
      {
         Info.itmNewVal = zh_itemPutL( NULL, ZH_TRUE );
         Info.itmResult = zh_itemPutL( NULL, ZH_FALSE );
         if( SELF_ORDINFO( pArea, DBOI_READLOCK, &Info ) == ZH_SUCCESS )
            fLocked = zh_itemGetL( Info.itmResult );
         zh_itemRelease( Info.itmNewVal );
         zh_itemRelease( Info.itmResult );
      }
   }
   zh_retl( fLocked );
}

ZH_FUNC( SX_ROXUNLOCK )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO Info;
      if( zh_sxOrdParam( &Info ) )
      {
         Info.itmNewVal = zh_itemPutL( NULL, ZH_FALSE );
         Info.itmResult = zh_itemPutL( NULL, ZH_FALSE );
         SELF_ORDINFO( pArea, DBOI_READLOCK, &Info );
         zh_itemRelease( Info.itmNewVal );
         zh_itemRelease( Info.itmResult );
      }
   }
}

ZH_FUNC( SX_ISMYROX )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
   ZH_BOOL fLocked = ZH_FALSE;

   if( pArea )
   {
      DBORDERINFO Info;
      if( zh_sxOrdParam( &Info ) )
      {
         Info.itmResult = zh_itemNew( NULL );
         if( SELF_ORDINFO( pArea, DBOI_READLOCK, &Info ) == ZH_SUCCESS )
            fLocked = zh_itemGetL( Info.itmResult );
         zh_itemRelease( Info.itmResult );
      }
   }
   zh_retl( fLocked );
}

ZH_FUNC( SX_ISROXLOCK )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
   ZH_BOOL fLocked = ZH_FALSE;

   if( pArea )
   {
      DBORDERINFO Info;
      if( zh_sxOrdParam( &Info ) )
      {
         Info.itmResult = zh_itemNew( NULL );
         if( SELF_ORDINFO( pArea, DBOI_READLOCK, &Info ) == ZH_SUCCESS )
            fLocked = zh_itemGetL( Info.itmResult );
         if( ! fLocked )
         {
            Info.itmNewVal = zh_itemPutL( NULL, ZH_TRUE );
            if( SELF_ORDINFO( pArea, DBOI_READLOCK, &Info ) == ZH_SUCCESS )
               fLocked = zh_itemGetL( Info.itmResult );
            if( fLocked )
            {
               zh_itemPutL( Info.itmNewVal, ZH_FALSE );
               SELF_ORDINFO( pArea, DBOI_READLOCK, &Info );
            }
            zh_itemRelease( Info.itmNewVal );
         }
         zh_itemRelease( Info.itmResult );
      }
   }
   zh_retl( fLocked );
}

ZH_FUNC( SX_SORTOPTION )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
   ZH_BOOL fUseCurrent = ZH_TRUE;

   if( pArea )
   {
      DBORDERINFO Info;
      if( zh_sxOrdParam( &Info ) )
      {
         Info.itmResult = zh_itemNew( NULL );
         Info.itmNewVal = zh_param( 1, ZH_IT_LOGICAL );
         if( SELF_ORDINFO( pArea, DBOI_USECURRENT, &Info ) == ZH_SUCCESS )
            fUseCurrent = zh_itemGetL( Info.itmResult );
         zh_itemRelease( Info.itmResult );
      }
   }
   zh_retl( fUseCurrent );
}
