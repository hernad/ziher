/*
 * USRRDD
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
#include "zh_item_api.h"
#include "zh_api_error.h"
#include "zh_lang_api.h"
#include "zh_rdd_api.h"
#include "zh_vm.h"
#include "zh_xvm.h"
#include "zh_stack.h"
#include "zh_init.h"
#include "../rdd_sys.zhh"
#include "rdd_usr.zhh"

#define SELF_USRNODE( w )  ( s_pUsrRddNodes[ ( w )->rddID ] )
#define SELF_USRDATA( w )  ( ( LPUSRRDDDATA ) ( ( ZH_BYTE * ) ( w ) + \
                                                SELF_USRNODE( w )->uiDataOffset ) )

#undef _SUPERTABLE
#define _SUPERTABLE( w )   ( SELF_USRNODE( w )->pSuperTable )
#undef __SUPERTABLE
#define __SUPERTABLE( r )  ( &( ( r )->pSuperTable ) )

typedef struct _USRRDDNODE
{
   ZH_USHORT uiDataOffset;
   PRDDFUNCS pSuperTable;
   PZH_ITEM  pMethods;
   PZH_ITEM  pItem;
} USRRDDNODE;
typedef USRRDDNODE * LPUSRRDDNODE;

typedef struct _USRRDDDATA
{
   PZH_ITEM pItem;
} USRRDDDATA;
typedef USRRDDDATA * LPUSRRDDDATA;

static ZH_USHORT      s_uiUsrNodes   = 0;
static LPUSRRDDNODE * s_pUsrRddNodes = NULL;

static ZH_BOOL zh_usrIsMethod( PZH_ITEM pMethods, ZH_USHORT uiMethod )
{
   PZH_ITEM pItem = zh_arrayGetItemPtr( pMethods, uiMethod );

   return pItem && ZH_IS_EVALITEM( pItem );
}

static ZH_BOOL zh_usrPushMethod( PZH_ITEM pMethods, ZH_USHORT uiMethod )
{
   PZH_ITEM pItem = zh_arrayGetItemPtr( pMethods, uiMethod );

   if( pItem )
   {
      if( ZH_IS_SYMBOL( pItem ) )
      {
         zh_vmPush( pItem );
         zh_vmPushNil();
         return ZH_TRUE;
      }
      else if( ZH_IS_BLOCK( pItem ) )
      {
         zh_vmPushEvalSym();
         zh_vmPush( pItem );
         return ZH_TRUE;
      }
   }
   return ZH_FALSE;
}

static ZH_ERRCODE zh_usrReturn( void )
{
   ZH_ERRCODE errCode = zh_parni( -1 );

   zh_ret();

   return errCode;
}

static ZH_ERRCODE zh_usrEvalRddFunc( PZH_ITEM pMethods, ZH_USHORT uiMethod, ZH_USHORT uiRddID )
{
   if( zh_usrPushMethod( pMethods, uiMethod ) )
   {
      zh_vmPushInteger( uiRddID );
      zh_vmDo( 1 );
      return zh_usrReturn();
   }

   return ZH_SUCCESS;
}

static ZH_ERRCODE zh_usrEvalAreaFunc( PZH_ITEM pMethods, ZH_USHORT uiMethod, AREAP pArea )
{
   if( zh_usrPushMethod( pMethods, uiMethod ) )
   {
      zh_vmPushPointer( pArea );
      zh_vmDo( 1 );
      return zh_usrReturn();
   }

   return ZH_SUCCESS;
}

static AREAP zh_usrGetAreaPointer( int iArea )
{
   if( iArea != 0 )
      return ( AREAP ) zh_rddGetWorkAreaPointer( iArea );
   else
      return NULL;
}



/*
 * RDD structures conversions
 */

static PZH_ITEM zh_usrArrayGet( PZH_ITEM pArray, ZH_SIZE nPos, ZH_TYPE uiType )
{
   PZH_ITEM pItem = zh_arrayGetItemPtr( pArray, nPos );

   if( pItem && ( zh_itemType( pItem ) & uiType ) != 0 )
      return pItem;
   else
      return NULL;
}

static const char * zh_usrArrayGetCPtr( PZH_ITEM pArray, ZH_SIZE nPos )
{
   PZH_ITEM pItem = zh_arrayGetItemPtr( pArray, nPos );

   if( pItem && ZH_IS_STRING( pItem ) )
      return zh_itemGetCPtr( pItem );
   else
      return NULL;
}

static PZH_ITEM zh_usrFieldInfoToItem( LPDBFIELDINFO pFieldInfo )
{
   PZH_ITEM pItem;

   pItem = zh_itemArrayNew( UR_FI_SIZE );
   if( pFieldInfo->atomName )
      zh_itemPutC( zh_arrayGetItemPtr( pItem, UR_FI_NAME ), pFieldInfo->atomName );
   zh_itemPutNI( zh_arrayGetItemPtr( pItem, UR_FI_TYPE ), pFieldInfo->uiType );
   zh_itemPutNI( zh_arrayGetItemPtr( pItem, UR_FI_TYPEEXT ), pFieldInfo->uiTypeExtended );
   zh_itemPutNI( zh_arrayGetItemPtr( pItem, UR_FI_LEN ), pFieldInfo->uiLen );
   zh_itemPutNI( zh_arrayGetItemPtr( pItem, UR_FI_DEC ), pFieldInfo->uiDec );

   return pItem;
}

static ZH_BOOL zh_usrItemToFieldInfo( PZH_ITEM pItem, LPDBFIELDINFO pFieldInfo )
{
   if( pItem && zh_arrayLen( pItem ) == UR_FI_SIZE )
   {
      pFieldInfo->atomName       = zh_usrArrayGetCPtr( pItem, UR_FI_NAME );
      pFieldInfo->uiType         = ( ZH_USHORT ) zh_arrayGetNI( pItem, UR_FI_TYPE );
      pFieldInfo->uiTypeExtended = ( ZH_USHORT ) zh_arrayGetNI( pItem, UR_FI_TYPEEXT );
      pFieldInfo->uiLen          = ( ZH_USHORT ) zh_arrayGetNI( pItem, UR_FI_LEN );
      pFieldInfo->uiDec          = ( ZH_USHORT ) zh_arrayGetNI( pItem, UR_FI_DEC );
      return ZH_TRUE;
   }
   return ZH_FALSE;
}

static PZH_ITEM zh_usrOpenInfoToItem( LPDBOPENINFO pOpenInfo )
{
   PZH_ITEM pItem;

   pItem = zh_itemArrayNew( UR_OI_SIZE );
   zh_itemPutNI( zh_arrayGetItemPtr( pItem, UR_OI_AREA ), pOpenInfo->uiArea );
   if( pOpenInfo->abName )
      zh_itemPutC( zh_arrayGetItemPtr( pItem, UR_OI_NAME ), pOpenInfo->abName );
   if( pOpenInfo->atomAlias )
      zh_itemPutC( zh_arrayGetItemPtr( pItem, UR_OI_ALIAS ), pOpenInfo->atomAlias );
   zh_itemPutL( zh_arrayGetItemPtr( pItem, UR_OI_SHARED ), pOpenInfo->fShared );
   zh_itemPutL( zh_arrayGetItemPtr( pItem, UR_OI_READONLY ), pOpenInfo->fReadonly );
   if( pOpenInfo->cdpId )
      zh_itemPutC( zh_arrayGetItemPtr( pItem, UR_OI_CDPID ), pOpenInfo->cdpId );
   zh_itemPutNL( zh_arrayGetItemPtr( pItem, UR_OI_CONNECT ), pOpenInfo->ulConnection );
   if( pOpenInfo->lpdbHeader )
      zh_itemPutPtr( zh_arrayGetItemPtr( pItem, UR_OI_HEADER ), pOpenInfo->lpdbHeader );

   return pItem;
}

static ZH_BOOL zh_usrItemToOpenInfo( PZH_ITEM pItem, LPDBOPENINFO pOpenInfo )
{
   if( pItem && zh_arrayLen( pItem ) == UR_OI_SIZE )
   {
      pOpenInfo->uiArea       = ( ZH_USHORT ) zh_arrayGetNI( pItem, UR_OI_AREA );
      pOpenInfo->abName       = zh_usrArrayGetCPtr( pItem, UR_OI_NAME );
      pOpenInfo->atomAlias    = zh_usrArrayGetCPtr( pItem, UR_OI_ALIAS );
      pOpenInfo->fShared      = zh_arrayGetL( pItem, UR_OI_SHARED );
      pOpenInfo->fReadonly    = zh_arrayGetL( pItem, UR_OI_READONLY );
      pOpenInfo->cdpId        = zh_usrArrayGetCPtr( pItem, UR_OI_CDPID );
      pOpenInfo->ulConnection = zh_arrayGetNL( pItem, UR_OI_CONNECT );
      pOpenInfo->lpdbHeader   = zh_arrayGetPtr( pItem, UR_OI_HEADER );
      return ZH_TRUE;
   }
   return ZH_FALSE;
}

static PZH_ITEM zh_usrFilterInfoToItem( LPDBFILTERINFO pFilterInfo )
{
   PZH_ITEM pItem;

   pItem = zh_itemArrayNew( UR_FRI_SIZE );
   if( pFilterInfo->itmCobExpr )
      zh_itemCopy( zh_arrayGetItemPtr( pItem, UR_FRI_BEXPR ), pFilterInfo->itmCobExpr );
   if( pFilterInfo->abFilterText )
      zh_itemCopy( zh_arrayGetItemPtr( pItem, UR_FRI_CEXPR ), pFilterInfo->abFilterText );
   zh_itemPutL( zh_arrayGetItemPtr( pItem, UR_FRI_ACTIVE ), pFilterInfo->fFilter );
   zh_itemPutL( zh_arrayGetItemPtr( pItem, UR_FRI_OPTIMIZED ), pFilterInfo->fOptimized );
   zh_itemPutPtr( zh_arrayGetItemPtr( pItem, UR_FRI_CARGO ), pFilterInfo->lpvCargo );

   return pItem;
}

static ZH_BOOL zh_usrItemToFilterInfo( PZH_ITEM pItem, LPDBFILTERINFO pFilterInfo )
{
   if( pItem && zh_arrayLen( pItem ) == UR_FRI_SIZE )
   {
      pFilterInfo->itmCobExpr   = zh_usrArrayGet( pItem, UR_FRI_BEXPR, ZH_IT_ANY );
      pFilterInfo->abFilterText = zh_usrArrayGet( pItem, UR_FRI_CEXPR, ZH_IT_ANY );
      pFilterInfo->fFilter      = zh_arrayGetL( pItem, UR_FRI_ACTIVE );
      pFilterInfo->fOptimized   = zh_arrayGetL( pItem, UR_FRI_OPTIMIZED );
      pFilterInfo->lpvCargo     = zh_arrayGetPtr( pItem, UR_FRI_CARGO );
      return ZH_TRUE;
   }
   return ZH_FALSE;
}

static PZH_ITEM zh_usrRelInfoToItem( LPDBRELINFO pRelInfo )
{
   PZH_ITEM pItem;

   pItem = zh_itemArrayNew( UR_RI_SIZE );
   if( pRelInfo->itmCobExpr )
      zh_itemCopy( zh_arrayGetItemPtr( pItem, UR_RI_BEXPR ), pRelInfo->itmCobExpr );
   if( pRelInfo->abKey )
      zh_itemCopy( zh_arrayGetItemPtr( pItem, UR_RI_CEXPR ), pRelInfo->abKey );
   zh_itemPutL( zh_arrayGetItemPtr( pItem, UR_RI_SCOPED ), pRelInfo->isScoped );
   zh_itemPutL( zh_arrayGetItemPtr( pItem, UR_RI_OPTIMIZED ), pRelInfo->isOptimized );
   zh_itemPutNI( zh_arrayGetItemPtr( pItem, UR_RI_PARENT ), pRelInfo->lpaParent ? pRelInfo->lpaParent->uiArea : 0 );
   zh_itemPutNI( zh_arrayGetItemPtr( pItem, UR_RI_CHILD ), pRelInfo->lpaChild ? pRelInfo->lpaChild->uiArea : 0 );
   zh_itemPutPtr( zh_arrayGetItemPtr( pItem, UR_RI_NEXT ), pRelInfo->lpdbriNext );

   return pItem;
}

static ZH_BOOL zh_usrItemToRelInfo( PZH_ITEM pItem, LPDBRELINFO pRelInfo )
{
   if( pItem && zh_arrayLen( pItem ) == UR_RI_SIZE )
   {
      pRelInfo->itmCobExpr  = zh_usrArrayGet( pItem, UR_RI_BEXPR, ZH_IT_ANY );
      pRelInfo->abKey       = zh_usrArrayGet( pItem, UR_RI_CEXPR, ZH_IT_ANY );
      pRelInfo->isScoped    = zh_arrayGetL( pItem, UR_RI_SCOPED );
      pRelInfo->isOptimized = zh_arrayGetL( pItem, UR_RI_OPTIMIZED );
      pRelInfo->lpaParent   = zh_usrGetAreaPointer( zh_arrayGetNI( pItem, UR_RI_PARENT ) );
      pRelInfo->lpaChild    = zh_usrGetAreaPointer( zh_arrayGetNI( pItem, UR_RI_CHILD ) );
      pRelInfo->lpdbriNext  = ( LPDBRELINFO ) zh_arrayGetPtr( pItem, UR_RI_NEXT );
      return ZH_TRUE;
   }
   return ZH_FALSE;
}

static PZH_ITEM zh_usrLockInfoToItem( LPDBLOCKINFO pLockInfo )
{
   PZH_ITEM pItem;

   pItem = zh_itemArrayNew( UR_LI_SIZE );
   if( pLockInfo->itmRecID )
      zh_itemCopy( zh_arrayGetItemPtr( pItem, UR_LI_RECORD ), pLockInfo->itmRecID );
   zh_itemPutNI( zh_arrayGetItemPtr( pItem, UR_LI_METHOD ), pLockInfo->uiMethod );
   zh_itemPutL( zh_arrayGetItemPtr( pItem, UR_LI_RESULT ), pLockInfo->fResult );

   return pItem;
}

static ZH_BOOL zh_usrItemToLockInfo( PZH_ITEM pItem, LPDBLOCKINFO pLockInfo )
{
   if( pItem && zh_arrayLen( pItem ) == UR_LI_SIZE )
   {
      pLockInfo->itmRecID = zh_usrArrayGet( pItem, UR_LI_RECORD, ZH_IT_ANY );
      pLockInfo->uiMethod = ( ZH_USHORT ) zh_arrayGetNI( pItem, UR_LI_METHOD );
      pLockInfo->fResult  = ( ZH_USHORT ) zh_arrayGetL( pItem, UR_LI_RESULT );
      return ZH_TRUE;
   }
   return ZH_FALSE;
}

static PZH_ITEM zh_usrScopeInfoToItem( LPDBSCOPEINFO pScopeInfo )
{
   PZH_ITEM pItem;

   pItem = zh_itemArrayNew( UR_SI_SIZE );
   if( pScopeInfo->itmCobFor )
      zh_itemCopy( zh_arrayGetItemPtr( pItem, UR_SI_BFOR ), pScopeInfo->itmCobFor );
   if( pScopeInfo->lpstrFor )
      zh_itemCopy( zh_arrayGetItemPtr( pItem, UR_SI_CFOR ), pScopeInfo->lpstrFor );
   if( pScopeInfo->itmCobWhile )
      zh_itemCopy( zh_arrayGetItemPtr( pItem, UR_SI_BWHILE ), pScopeInfo->itmCobWhile );
   if( pScopeInfo->lpstrWhile )
      zh_itemCopy( zh_arrayGetItemPtr( pItem, UR_SI_CWHILE ), pScopeInfo->lpstrWhile );
   if( pScopeInfo->lNext )
      zh_itemCopy( zh_arrayGetItemPtr( pItem, UR_SI_NEXT ), pScopeInfo->lNext );
   if( pScopeInfo->itmRecID )
      zh_itemCopy( zh_arrayGetItemPtr( pItem, UR_SI_RECORD ), pScopeInfo->itmRecID );
   if( pScopeInfo->fRest )
      zh_itemCopy( zh_arrayGetItemPtr( pItem, UR_SI_REST ), pScopeInfo->fRest );
   zh_itemPutL( zh_arrayGetItemPtr( pItem, UR_SI_IGNOREFILTER ), pScopeInfo->fIgnoreFilter );
   zh_itemPutL( zh_arrayGetItemPtr( pItem, UR_SI_INCLUDEDELETED ), pScopeInfo->fIncludeDeleted );
   zh_itemPutL( zh_arrayGetItemPtr( pItem, UR_SI_LAST ), pScopeInfo->fLast );
   zh_itemPutL( zh_arrayGetItemPtr( pItem, UR_SI_IGNOREDUPS ), pScopeInfo->fIgnoreDuplicates );
   zh_itemPutL( zh_arrayGetItemPtr( pItem, UR_SI_BACKWARD ), pScopeInfo->fBackward );
   zh_itemPutL( zh_arrayGetItemPtr( pItem, UR_SI_OPTIMIZED ), pScopeInfo->fOptimized );

   return pItem;
}

static ZH_BOOL zh_usrItemToScopeInfo( PZH_ITEM pItem, LPDBSCOPEINFO pScopeInfo )
{
   if( pItem && zh_arrayLen( pItem ) == UR_SI_SIZE )
   {
      pScopeInfo->itmCobFor         = zh_usrArrayGet( pItem, UR_SI_BFOR, ZH_IT_ANY );
      pScopeInfo->lpstrFor          = zh_usrArrayGet( pItem, UR_SI_CFOR, ZH_IT_ANY );
      pScopeInfo->itmCobWhile       = zh_usrArrayGet( pItem, UR_SI_BWHILE, ZH_IT_ANY );
      pScopeInfo->lpstrWhile        = zh_usrArrayGet( pItem, UR_SI_CWHILE, ZH_IT_ANY );
      pScopeInfo->lNext             = zh_usrArrayGet( pItem, UR_SI_NEXT, ZH_IT_ANY );
      pScopeInfo->itmRecID          = zh_usrArrayGet( pItem, UR_SI_RECORD, ZH_IT_ANY );
      pScopeInfo->fRest             = zh_usrArrayGet( pItem, UR_SI_REST, ZH_IT_ANY );
      pScopeInfo->fIgnoreFilter     = zh_arrayGetL( pItem, UR_SI_IGNOREFILTER );
      pScopeInfo->fIncludeDeleted   = zh_arrayGetL( pItem, UR_SI_INCLUDEDELETED );
      pScopeInfo->fLast             = zh_arrayGetL( pItem, UR_SI_LAST );
      pScopeInfo->fIgnoreDuplicates = zh_arrayGetL( pItem, UR_SI_IGNOREDUPS );
      pScopeInfo->fBackward         = zh_arrayGetL( pItem, UR_SI_BACKWARD );
      pScopeInfo->fOptimized        = zh_arrayGetL( pItem, UR_SI_OPTIMIZED );
      return ZH_TRUE;
   }
   return ZH_FALSE;
}

static PZH_ITEM zh_usrEvalInfoToItem( LPDBEVALINFO pEvalInfo )
{
   PZH_ITEM pItem, pScope;

   pScope = zh_usrScopeInfoToItem( &pEvalInfo->dbsci );
   pItem = zh_itemArrayNew( UR_EI_SIZE );
   if( pEvalInfo->itmBlock )
      zh_itemCopy( zh_arrayGetItemPtr( pItem, UR_EI_BLOCK ), pEvalInfo->itmBlock );
   if( pEvalInfo->abBlock )
      zh_itemCopy( zh_arrayGetItemPtr( pItem, UR_EI_CEXPR ), pEvalInfo->abBlock );
   zh_itemMove( zh_arrayGetItemPtr( pItem, UR_EI_SCOPE ), pScope );
   zh_itemRelease( pScope );

   return pItem;
}

static ZH_BOOL zh_usrItemToEvalInfo( PZH_ITEM pItem, LPDBEVALINFO pEvalInfo )
{
   if( pItem && zh_arrayLen( pItem ) == UR_EI_SIZE )
   {
      pEvalInfo->itmBlock = zh_usrArrayGet( pItem, UR_EI_BLOCK, ZH_IT_ANY );
      pEvalInfo->abBlock  = zh_usrArrayGet( pItem, UR_EI_CEXPR, ZH_IT_ANY );
      return zh_usrItemToScopeInfo( zh_arrayGetItemPtr( pItem, UR_EI_SCOPE ),
                                    &pEvalInfo->dbsci );
   }
   return ZH_FALSE;
}

static PZH_ITEM zh_usrTransInfoToItem( LPDBTRANSINFO pTransInfo )
{
   PZH_ITEM pItem, pScope;

   pScope = zh_usrScopeInfoToItem( &pTransInfo->dbsci );
   pItem = zh_itemArrayNew( UR_TI_SIZE );
   zh_itemPutNI( zh_arrayGetItemPtr( pItem, UR_TI_SRCAREA ), pTransInfo->lpaSource->uiArea );
   zh_itemPutNI( zh_arrayGetItemPtr( pItem, UR_TI_DSTAREA ), pTransInfo->lpaDest->uiArea );
   zh_itemMove( zh_arrayGetItemPtr( pItem, UR_TI_SCOPE ), pScope );
   zh_itemPutNI( zh_arrayGetItemPtr( pItem, UR_TI_FLAGS ), pTransInfo->uiFlags );
   zh_itemPutNI( zh_arrayGetItemPtr( pItem, UR_TI_ITEMCOUNT ), pTransInfo->uiItemCount );
   if( pTransInfo->uiItemCount )
   {
      PZH_ITEM pItems = zh_arrayGetItemPtr( pItem, UR_TI_ITEMS );
      LPDBTRANSITEM pTransItem = pTransInfo->lpTransItems;
      ZH_USHORT uiCount;

      zh_arrayNew( pItems, pTransInfo->uiItemCount );
      for( uiCount = 1; uiCount <= pTransInfo->uiItemCount; ++uiCount, ++pTransItem )
      {
         PZH_ITEM pItm = zh_arrayGetItemPtr( pItems, uiCount );
         zh_arrayNew( pItm, UR_TITEM_SIZE );
         zh_itemPutNI( zh_arrayGetItemPtr( pItm, UR_TITEM_SOURCE ), pTransItem->uiSource );
         zh_itemPutNI( zh_arrayGetItemPtr( pItm, UR_TITEM_DESTIN ), pTransItem->uiDest );
      }
   }
   zh_itemRelease( pScope );

   return pItem;
}

static ZH_BOOL zh_usrItemToTransInfo( PZH_ITEM pItem, LPDBTRANSINFO pTransInfo )
{
   if( pItem && zh_arrayLen( pItem ) == UR_TI_SIZE )
   {
      ZH_USHORT uiItemCount = ( ZH_USHORT ) zh_arrayGetNI( pItem, UR_TI_ITEMCOUNT ), uiCount;
      PZH_ITEM pItems = zh_arrayGetItemPtr( pItem, UR_TI_ITEMS );

      if( zh_arrayLen( pItems ) == ( ZH_SIZE ) uiItemCount &&
          zh_usrItemToScopeInfo( zh_arrayGetItemPtr( pItem, UR_TI_SCOPE ),
                                 &pTransInfo->dbsci ) )
      {
         pTransInfo->lpaSource   = zh_usrGetAreaPointer( zh_arrayGetNI( pItem, UR_TI_SRCAREA ) );
         pTransInfo->lpaDest     = zh_usrGetAreaPointer( zh_arrayGetNI( pItem, UR_TI_DSTAREA ) );
         pTransInfo->uiFlags     = ( ZH_USHORT ) zh_arrayGetNI( pItem, UR_TI_FLAGS );
         pTransInfo->uiItemCount = uiItemCount;
         if( uiItemCount )
         {
            LPDBTRANSITEM pTransItem;

            pTransInfo->lpTransItems = pTransItem =
               ( LPDBTRANSITEM ) zh_xgrab( uiItemCount * sizeof( DBTRANSITEM ) );

            for( uiCount = 1; uiCount <= uiItemCount; ++uiCount, ++pTransItem )
            {
               PZH_ITEM pItm = zh_arrayGetItemPtr( pItems, uiCount );
               pTransItem->uiSource = ( ZH_USHORT ) zh_arrayGetNI( pItm, UR_TITEM_SOURCE );
               pTransItem->uiDest   = ( ZH_USHORT ) zh_arrayGetNI( pItm, UR_TITEM_DESTIN );
            }
         }
         else
         {
            pTransInfo->lpTransItems = NULL;
         }
         return ZH_TRUE;
      }
   }
   return ZH_FALSE;
}

static void zh_usrTransInfoFree( LPDBTRANSINFO pTransInfo )
{
   if( pTransInfo->uiItemCount )
      zh_xfree( pTransInfo->lpTransItems );
}

static PZH_ITEM zh_usrSortInfoToItem( LPDBSORTINFO pSortInfo )
{
   PZH_ITEM pItem, pTrans;

   pTrans = zh_usrTransInfoToItem( &pSortInfo->dbtri );
   pItem = zh_itemArrayNew( UR_SRI_SIZE );
   zh_itemMove( zh_arrayGetItemPtr( pItem, UR_SRI_TRANSINFO ), pTrans );
   zh_itemPutNI( zh_arrayGetItemPtr( pItem, UR_SRI_ITEMCOUNT ), pSortInfo->uiItemCount );
   if( pSortInfo->uiItemCount )
   {
      PZH_ITEM pItems = zh_arrayGetItemPtr( pItem, UR_SRI_ITEMS );
      LPDBSORTITEM pSortItem = pSortInfo->lpdbsItem;
      ZH_USHORT uiCount;

      zh_arrayNew( pItems, pSortInfo->uiItemCount );
      for( uiCount = 1; uiCount <= pSortInfo->uiItemCount; ++uiCount, ++pSortItem )
      {
         PZH_ITEM pItm = zh_arrayGetItemPtr( pItems, uiCount );
         zh_arrayNew( pItm, UR_SITEM_SIZE );
         zh_itemPutNI( zh_arrayGetItemPtr( pItm, UR_SITEM_FIELD ), pSortItem->uiField );
         zh_itemPutNI( zh_arrayGetItemPtr( pItm, UR_SITEM_FLAGS ), pSortItem->uiFlags );
      }
   }
   zh_itemRelease( pTrans );

   return pItem;
}

static ZH_BOOL zh_usrItemToSortInfo( PZH_ITEM pItem, LPDBSORTINFO pSortInfo )
{
   if( pItem && zh_arrayLen( pItem ) == UR_SRI_SIZE )
   {
      ZH_USHORT uiItemCount = ( ZH_USHORT ) zh_arrayGetNI( pItem, UR_SRI_ITEMCOUNT ), uiCount;
      PZH_ITEM pItems = zh_arrayGetItemPtr( pItem, UR_SRI_ITEMS );

      if( zh_arrayLen( pItems ) == ( ZH_SIZE ) uiItemCount &&
          zh_usrItemToTransInfo( zh_arrayGetItemPtr( pItem, UR_SRI_TRANSINFO ),
                                 &pSortInfo->dbtri ) )
      {
         pSortInfo->uiItemCount = uiItemCount;
         if( uiItemCount )
         {
            LPDBSORTITEM pSortItem;

            pSortInfo->lpdbsItem = pSortItem =
               ( LPDBSORTITEM ) zh_xgrab( uiItemCount * sizeof( DBSORTITEM ) );

            for( uiCount = 1; uiCount <= uiItemCount; ++uiCount, ++pSortItem )
            {
               PZH_ITEM pItm = zh_arrayGetItemPtr( pItems, uiCount );
               pSortItem->uiField = ( ZH_USHORT ) zh_arrayGetNI( pItm, UR_SITEM_FIELD );
               pSortItem->uiFlags = ( ZH_USHORT ) zh_arrayGetNI( pItm, UR_SITEM_FLAGS );
            }
         }
         else
         {
            pSortInfo->lpdbsItem = NULL;
         }
         return ZH_TRUE;
      }
   }
   return ZH_FALSE;
}

static void zh_usrSortInfoFree( LPDBSORTINFO pSortInfo )
{
   zh_usrTransInfoFree( &pSortInfo->dbtri );
   if( pSortInfo->uiItemCount )
      zh_xfree( pSortInfo->lpdbsItem );
}

static PZH_ITEM zh_usrOrderInfoToItem( LPDBORDERINFO pOrderInfo )
{
   PZH_ITEM pItem;

   pItem = zh_itemArrayNew( UR_ORI_SIZE );
   if( pOrderInfo->atomBagName )
      zh_itemCopy( zh_arrayGetItemPtr( pItem, UR_ORI_BAG ), pOrderInfo->atomBagName );
   if( pOrderInfo->itmOrder )
      zh_itemCopy( zh_arrayGetItemPtr( pItem, UR_ORI_TAG ), pOrderInfo->itmOrder );
   if( pOrderInfo->itmCobExpr )
      zh_itemCopy( zh_arrayGetItemPtr( pItem, UR_ORI_BLOCK ), pOrderInfo->itmCobExpr );
   if( pOrderInfo->itmResult )
      zh_itemCopy( zh_arrayGetItemPtr( pItem, UR_ORI_RESULT ), pOrderInfo->itmResult );
   if( pOrderInfo->itmNewVal )
      zh_itemCopy( zh_arrayGetItemPtr( pItem, UR_ORI_NEWVAL ), pOrderInfo->itmNewVal );
   zh_itemPutL( zh_arrayGetItemPtr( pItem, UR_ORI_ALLTAGS ), pOrderInfo->fAllTags );

   return pItem;
}

static ZH_BOOL zh_usrItemToOrderInfo( PZH_ITEM pItem, LPDBORDERINFO pOrderInfo )
{
   if( pItem && zh_arrayLen( pItem ) == UR_ORI_SIZE )
   {
      pOrderInfo->atomBagName = zh_usrArrayGet( pItem, UR_ORI_BAG, ZH_IT_ANY );
      pOrderInfo->itmOrder    = zh_usrArrayGet( pItem, UR_ORI_TAG, ZH_IT_ANY );
      pOrderInfo->itmCobExpr  = zh_usrArrayGet( pItem, UR_ORI_BLOCK, ZH_IT_ANY );
      pOrderInfo->itmResult   = zh_usrArrayGet( pItem, UR_ORI_RESULT, ZH_IT_ANY );
      pOrderInfo->itmNewVal   = zh_usrArrayGet( pItem, UR_ORI_NEWVAL, ZH_IT_ANY );
      pOrderInfo->fAllTags    = zh_arrayGetL( pItem, UR_ORI_ALLTAGS );
      return ZH_TRUE;
   }
   return ZH_FALSE;
}

static PZH_ITEM zh_usrOrderCondInfoToItem( LPDBORDERCONDINFO pOrderCondInfo )
{
   PZH_ITEM pItem;

   pItem = zh_itemArrayNew( UR_ORC_SIZE );
   zh_itemPutL( zh_arrayGetItemPtr( pItem, UR_ORC_ACTIVE ), pOrderCondInfo->fActive );
   if( pOrderCondInfo->abFor )
      zh_itemPutC( zh_arrayGetItemPtr( pItem, UR_ORC_CFOR ), pOrderCondInfo->abFor );
   if( pOrderCondInfo->abWhile )
      zh_itemPutC( zh_arrayGetItemPtr( pItem, UR_ORC_CWHILE ), pOrderCondInfo->abWhile );
   if( pOrderCondInfo->itmCobFor )
      zh_itemCopy( zh_arrayGetItemPtr( pItem, UR_ORC_BFOR ), pOrderCondInfo->itmCobFor );
   if( pOrderCondInfo->itmCobWhile )
      zh_itemCopy( zh_arrayGetItemPtr( pItem, UR_ORC_BWHILE ), pOrderCondInfo->itmCobWhile );
   if( pOrderCondInfo->itmCobEval )
      zh_itemCopy( zh_arrayGetItemPtr( pItem, UR_ORC_BEVAL ), pOrderCondInfo->itmCobEval );
   zh_itemPutNL( zh_arrayGetItemPtr( pItem, UR_ORC_STEP ), pOrderCondInfo->lStep );
   if( pOrderCondInfo->itmStartRecID )
      zh_itemCopy( zh_arrayGetItemPtr( pItem, UR_ORC_STARTREC ), pOrderCondInfo->itmStartRecID );
   zh_itemPutNL( zh_arrayGetItemPtr( pItem, UR_ORC_NEXT ), pOrderCondInfo->lNextCount );
   if( pOrderCondInfo->itmRecID )
      zh_itemCopy( zh_arrayGetItemPtr( pItem, UR_ORC_RECORD ), pOrderCondInfo->itmRecID );
   zh_itemPutL( zh_arrayGetItemPtr( pItem, UR_ORC_REST ), pOrderCondInfo->fRest );
   zh_itemPutL( zh_arrayGetItemPtr( pItem, UR_ORC_DESCEND ), pOrderCondInfo->fDescending );
   zh_itemPutL( zh_arrayGetItemPtr( pItem, UR_ORC_SCOPED ), pOrderCondInfo->fScoped );
   zh_itemPutL( zh_arrayGetItemPtr( pItem, UR_ORC_ALL ), pOrderCondInfo->fAll );
   zh_itemPutL( zh_arrayGetItemPtr( pItem, UR_ORC_ADDITIVE ), pOrderCondInfo->fAdditive );
   zh_itemPutL( zh_arrayGetItemPtr( pItem, UR_ORC_USECURRENT ), pOrderCondInfo->fUseCurrent );
   zh_itemPutL( zh_arrayGetItemPtr( pItem, UR_ORC_CUSTOM ), pOrderCondInfo->fCustom );
   zh_itemPutL( zh_arrayGetItemPtr( pItem, UR_ORC_NOOPTIMIZE ), pOrderCondInfo->fNoOptimize );
   zh_itemPutL( zh_arrayGetItemPtr( pItem, UR_ORC_COMPOUND ), pOrderCondInfo->fCompound );
   zh_itemPutL( zh_arrayGetItemPtr( pItem, UR_ORC_USEFILTER ), pOrderCondInfo->fUseFilter );
   zh_itemPutL( zh_arrayGetItemPtr( pItem, UR_ORC_TEMPORARY ), pOrderCondInfo->fTemporary );
   zh_itemPutL( zh_arrayGetItemPtr( pItem, UR_ORC_EXCLUSIVE ), pOrderCondInfo->fExclusive );
   zh_itemPutPtr( zh_arrayGetItemPtr( pItem, UR_ORC_CARGO ), pOrderCondInfo->lpvCargo );

   return pItem;
}

static ZH_BOOL zh_usrItemToOrderCondInfo( PZH_ITEM pItem,
                                          LPDBORDERCONDINFO pOrderCondInfo )
{
   if( pItem && zh_arrayLen( pItem ) == UR_ORC_SIZE )
   {
      pOrderCondInfo->fActive       = zh_arrayGetL( pItem, UR_ORC_ACTIVE );
      pOrderCondInfo->abFor         = zh_arrayGetC( pItem, UR_ORC_CFOR );
      pOrderCondInfo->abWhile       = zh_arrayGetC( pItem, UR_ORC_CWHILE );
      pOrderCondInfo->itmCobFor     = zh_usrArrayGet( pItem, UR_ORC_BFOR, ZH_IT_ANY );
      pOrderCondInfo->itmCobWhile   = zh_usrArrayGet( pItem, UR_ORC_BWHILE, ZH_IT_ANY );
      pOrderCondInfo->itmCobEval    = zh_usrArrayGet( pItem, UR_ORC_BEVAL, ZH_IT_ANY );
      pOrderCondInfo->lStep         = zh_arrayGetNL( pItem, UR_ORC_STEP );
      pOrderCondInfo->itmStartRecID = zh_usrArrayGet( pItem, UR_ORC_STARTREC, ZH_IT_ANY );
      pOrderCondInfo->lNextCount    = zh_arrayGetNL( pItem, UR_ORC_NEXT );
      pOrderCondInfo->itmRecID      = zh_usrArrayGet( pItem, UR_ORC_RECORD, ZH_IT_ANY );
      pOrderCondInfo->fRest         = zh_arrayGetL( pItem, UR_ORC_REST );
      pOrderCondInfo->fDescending   = zh_arrayGetL( pItem, UR_ORC_DESCEND );
      pOrderCondInfo->fScoped       = zh_arrayGetL( pItem, UR_ORC_SCOPED );
      pOrderCondInfo->fAll          = zh_arrayGetL( pItem, UR_ORC_ALL );
      pOrderCondInfo->fAdditive     = zh_arrayGetL( pItem, UR_ORC_ADDITIVE );
      pOrderCondInfo->fUseCurrent   = zh_arrayGetL( pItem, UR_ORC_USECURRENT );
      pOrderCondInfo->fCustom       = zh_arrayGetL( pItem, UR_ORC_CUSTOM );
      pOrderCondInfo->fNoOptimize   = zh_arrayGetL( pItem, UR_ORC_NOOPTIMIZE );
      pOrderCondInfo->fCompound     = zh_arrayGetL( pItem, UR_ORC_COMPOUND );
      pOrderCondInfo->fUseFilter    = zh_arrayGetL( pItem, UR_ORC_USEFILTER );
      pOrderCondInfo->fTemporary    = zh_arrayGetL( pItem, UR_ORC_TEMPORARY );
      pOrderCondInfo->fExclusive    = zh_arrayGetL( pItem, UR_ORC_EXCLUSIVE );
      pOrderCondInfo->lpvCargo      = zh_arrayGetPtr( pItem, UR_ORC_CARGO );
      return ZH_TRUE;
   }
   return ZH_FALSE;
}

static void zh_usrOrderCondFree( LPDBORDERCONDINFO pOrderCondInfo )
{
   if( pOrderCondInfo->abFor )
      zh_xfree( pOrderCondInfo->abFor );
   if( pOrderCondInfo->abWhile )
      zh_xfree( pOrderCondInfo->abWhile );
   if( pOrderCondInfo->itmCobFor )
      zh_itemRelease( pOrderCondInfo->itmCobFor );
   if( pOrderCondInfo->itmCobWhile )
      zh_itemRelease( pOrderCondInfo->itmCobWhile );
   if( pOrderCondInfo->itmCobEval )
      zh_itemRelease( pOrderCondInfo->itmCobEval );
   if( pOrderCondInfo->itmStartRecID )
      zh_itemRelease( pOrderCondInfo->itmStartRecID );
   if( pOrderCondInfo->itmRecID )
      zh_itemRelease( pOrderCondInfo->itmRecID );
   zh_xfree( pOrderCondInfo );
}

static void zh_usrOrderCondClone( LPDBORDERCONDINFO pOrderCondInfo )
{
   if( pOrderCondInfo->abFor )
      pOrderCondInfo->abFor = zh_strdup( pOrderCondInfo->abFor );
   if( pOrderCondInfo->abWhile )
      pOrderCondInfo->abWhile = zh_strdup( pOrderCondInfo->abWhile );
   if( pOrderCondInfo->itmCobFor )
      pOrderCondInfo->itmCobFor = zh_itemNew( pOrderCondInfo->itmCobFor );
   if( pOrderCondInfo->itmCobWhile )
      pOrderCondInfo->itmCobWhile = zh_itemNew( pOrderCondInfo->itmCobWhile );
   if( pOrderCondInfo->itmCobEval )
      pOrderCondInfo->itmCobEval = zh_itemNew( pOrderCondInfo->itmCobEval );
   if( pOrderCondInfo->itmStartRecID )
      pOrderCondInfo->itmStartRecID = zh_itemNew( pOrderCondInfo->itmStartRecID );
   if( pOrderCondInfo->itmRecID )
      pOrderCondInfo->itmRecID = zh_itemNew( pOrderCondInfo->itmRecID );
}

static PZH_ITEM zh_usrOrderCreateInfoToItem( LPDBORDERCREATEINFO pOrderCreateInfo )
{
   PZH_ITEM pItem = zh_itemArrayNew( UR_ORCR_SIZE );

   if( pOrderCreateInfo->lpdbOrdCondInfo )
   {
      PZH_ITEM pCond = zh_usrOrderCondInfoToItem( pOrderCreateInfo->lpdbOrdCondInfo );
      zh_arraySet( pItem, UR_ORCR_CONDINFO, pCond );
      zh_itemRelease( pCond );
   }
   if( pOrderCreateInfo->abBagName )
      zh_itemPutC( zh_arrayGetItemPtr( pItem, UR_ORCR_BAGNAME ), pOrderCreateInfo->abBagName );
   if( pOrderCreateInfo->atomBagName )
      zh_itemPutC( zh_arrayGetItemPtr( pItem, UR_ORCR_TAGNAME ), pOrderCreateInfo->atomBagName );
   if( pOrderCreateInfo->itmOrder )
      zh_itemCopy( zh_arrayGetItemPtr( pItem, UR_ORCR_ORDER ), pOrderCreateInfo->itmOrder );
   zh_itemPutL( zh_arrayGetItemPtr( pItem, UR_ORCR_UNIQUE ), pOrderCreateInfo->fUnique );
   if( pOrderCreateInfo->itmCobExpr )
      zh_itemCopy( zh_arrayGetItemPtr( pItem, UR_ORCR_BKEY ), pOrderCreateInfo->itmCobExpr );
   if( pOrderCreateInfo->abExpr )
      zh_itemCopy( zh_arrayGetItemPtr( pItem, UR_ORCR_CKEY ), pOrderCreateInfo->abExpr );

   return pItem;
}

static ZH_BOOL zh_usrItemToOrderCreateInfo( PZH_ITEM pItem,
                                            LPDBORDERCREATEINFO pOrderCreateInfo )
{
   if( pItem && zh_arrayLen( pItem ) == UR_ORCR_SIZE )
   {
      PZH_ITEM pCond = zh_arrayGetItemPtr( pItem, UR_ORCR_CONDINFO );

      if( zh_arrayLen( pCond ) > 0 )
      {
         LPDBORDERCONDINFO pOrderCondInfo;
         pOrderCondInfo = ( LPDBORDERCONDINFO ) zh_xgrab( sizeof( DBORDERCONDINFO ) );
         if( ! zh_usrItemToOrderCondInfo( pCond, pOrderCondInfo ) )
         {
            zh_xfree( pOrderCondInfo );
            return ZH_FALSE;
         }
         pOrderCreateInfo->lpdbOrdCondInfo = pOrderCondInfo;
      }
      else
      {
         pOrderCreateInfo->lpdbOrdCondInfo = NULL;
      }

      pOrderCreateInfo->abBagName   = zh_usrArrayGetCPtr( pItem, UR_ORCR_BAGNAME );
      pOrderCreateInfo->atomBagName = zh_usrArrayGetCPtr( pItem, UR_ORCR_TAGNAME );
      pOrderCreateInfo->itmOrder    = zh_usrArrayGet( pItem, UR_ORCR_ORDER, ZH_IT_ANY );
      pOrderCreateInfo->fUnique     = zh_arrayGetL( pItem, UR_ORCR_UNIQUE );
      pOrderCreateInfo->itmCobExpr  = zh_usrArrayGet( pItem, UR_ORCR_BKEY, ZH_IT_ANY );
      pOrderCreateInfo->abExpr      = zh_usrArrayGet( pItem, UR_ORCR_CKEY, ZH_IT_ANY );

      return ZH_TRUE;
   }
   return ZH_FALSE;
}

static void zh_usrOrderCreateFree( LPDBORDERCREATEINFO pOrderCreateInfo )
{
   if( pOrderCreateInfo->lpdbOrdCondInfo )
      zh_xfree( pOrderCreateInfo->lpdbOrdCondInfo );
}


/*
 * -- USRRDD METHODS --
 */

static ZH_ERRCODE zh_usrInit( LPRDDNODE pRDD )
{
   ZH_ERRCODE errCode;
   LPUSRRDDNODE pNode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrInit(%p)", ( void * ) pRDD ) );

   if( pRDD->rddID >= s_uiUsrNodes )
   {
      ZH_SIZE nSize = ( pRDD->rddID + 1 ) * sizeof( LPUSRRDDNODE );
      if( s_uiUsrNodes )
         s_pUsrRddNodes = ( LPUSRRDDNODE * ) zh_xrealloc( s_pUsrRddNodes, nSize );
      else
         s_pUsrRddNodes = ( LPUSRRDDNODE * ) zh_xgrab( nSize );
      do
      {
         s_pUsrRddNodes[ s_uiUsrNodes ] = NULL;
      }
      while( ++s_uiUsrNodes <= pRDD->rddID );
   }

   s_pUsrRddNodes[ pRDD->rddID ] = pNode = ( LPUSRRDDNODE ) zh_xgrabz( sizeof( USRRDDNODE ) );
   pNode->pSuperTable = &pRDD->pSuperTable;
   pNode->pMethods = ( PZH_ITEM ) pRDD->pTable.whoCares;
   pRDD->pTable.whoCares = pRDD->pSuperTable.whoCares;
   pNode->pItem = zh_itemNew( NULL );

   if( ISSUPER_INIT( pRDD ) )
      errCode = SUPER_INIT( pRDD );
   else
      errCode = ZH_SUCCESS;

   zh_usrEvalRddFunc( pNode->pMethods, UR_INIT, pRDD->rddID );

   return errCode;
}

static ZH_ERRCODE zh_usrExit( LPRDDNODE pRDD )
{
   ZH_ERRCODE errCode;
   LPUSRRDDNODE pNode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrExit(%p)", ( void * ) pRDD ) );

   pNode = s_pUsrRddNodes[ pRDD->rddID ];
   zh_usrEvalRddFunc( pNode->pMethods, UR_EXIT, pRDD->rddID );
   if( pNode->pItem )
      zh_itemRelease( pNode->pItem );
   if( pNode->pMethods )
      zh_itemRelease( pNode->pMethods );
   zh_xfree( pNode );
   s_pUsrRddNodes[ pRDD->rddID ] = NULL;

   if( pRDD->rddID == s_uiUsrNodes - 1 )
   {
      while( --s_uiUsrNodes > 0 )
      {
         if( s_pUsrRddNodes[ s_uiUsrNodes - 1 ] != NULL )
            break;
      }

      if( s_uiUsrNodes )
      {
         s_pUsrRddNodes = ( LPUSRRDDNODE * ) zh_xrealloc( s_pUsrRddNodes,
                                       s_uiUsrNodes * sizeof( LPUSRRDDNODE ) );
      }
      else
      {
         zh_xfree( s_pUsrRddNodes );
         s_pUsrRddNodes = NULL;
      }
   }

   if( ISSUPER_EXIT( pRDD ) )
      errCode = SUPER_EXIT( pRDD );
   else
      errCode = ZH_SUCCESS;

   return errCode;
}

static ZH_ERRCODE zh_usrStructSize( AREAP pArea, ZH_USHORT * puiSize )
{
   ZH_ERRCODE errCode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrStrucSize(%p, %p)", ( void * ) pArea, ( void * ) puiSize ) );

   errCode = SUPER_STRUCTSIZE( pArea, puiSize );
   s_pUsrRddNodes[ pArea->rddID ]->uiDataOffset = *puiSize;
   *puiSize += sizeof( USRRDDDATA );

   return errCode;
}

static ZH_ERRCODE zh_usrSysName( AREAP pArea, char * szSysName )
{
   int nOffset;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrSysName(%p,%p)", ( void * ) pArea, ( void * ) szSysName ) );

   nOffset = ( int ) ( zh_stackTopOffset() - zh_stackBaseOffset() );
   zh_vmPushNil();
   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_SYSNAME ) )
   {
      zh_stackPop();
      zh_strncpy( szSysName, SELF_RDDNODE( pArea )->szName,
                  ZH_RDD_MAX_DRIVERNAME_LEN );
      return ZH_SUCCESS;
   }

   zh_vmPushInteger( pArea->uiArea );
   zh_xvmPushLocalByRef( ( ZH_SHORT ) nOffset );
   zh_vmDo( 2 );

   zh_strncpy( szSysName, zh_itemGetCPtr( zh_stackItemFromBase( nOffset ) ),
               ZH_RDD_MAX_DRIVERNAME_LEN );
   zh_stackPop();

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrNewArea( AREAP pArea )
{
   ZH_ERRCODE errCode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrNewArea(%p)", ( void * ) pArea ) );

   errCode = SUPER_NEW( pArea );

   if( errCode == ZH_SUCCESS )
   {
      SELF_USRDATA( pArea )->pItem = zh_itemNew( NULL );
      zh_usrEvalAreaFunc( SELF_USRNODE( pArea )->pMethods, UR_NEW, pArea );
   }

   return errCode;
}

static ZH_ERRCODE zh_usrRelease( AREAP pArea )
{
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrRelease(%p)", ( void * ) pArea ) );

   zh_usrEvalAreaFunc( SELF_USRNODE( pArea )->pMethods, UR_RELEASE, pArea );

   pItem = SELF_USRDATA( pArea )->pItem;
   if( pItem )
      zh_itemRelease( pItem );

   return SUPER_RELEASE( pArea );
}

/*
 * methods which user can overload
 */


/*
 * Movement and positioning methods
 */

static ZH_ERRCODE zh_usrBof( AREAP pArea, ZH_BOOL * pBof )
{
   int nOffset;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrBof(%p, %p)", ( void * ) pArea, ( void * ) pBof ) );

   nOffset = ( int ) ( zh_stackTopOffset() - zh_stackBaseOffset() );
   zh_vmPushLogical( pArea->fBof );
   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_BOF ) )
   {
      zh_stackPop();
      return SUPER_BOF( pArea, pBof );
   }
   zh_vmPushInteger( pArea->uiArea );
   zh_xvmPushLocalByRef( ( ZH_SHORT ) nOffset );
   zh_vmDo( 2 );

   if( zh_xvmPopLogical( pBof ) )
   {
      zh_ret();
      return ZH_FAILURE;
   }

   pArea->fBof = *pBof;
   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrEof( AREAP pArea, ZH_BOOL * pEof )
{
   int nOffset;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrEof(%p, %p)", ( void * ) pArea, ( void * ) pEof ) );

   nOffset = ( int ) ( zh_stackTopOffset() - zh_stackBaseOffset() );
   zh_vmPushLogical( pArea->fEof );
   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_EOF ) )
   {
      zh_stackPop();
      return SUPER_EOF( pArea, pEof );
   }
   zh_vmPushInteger( pArea->uiArea );
   zh_xvmPushLocalByRef( ( ZH_SHORT ) nOffset );
   zh_vmDo( 2 );

   if( zh_xvmPopLogical( pEof ) )
   {
      zh_ret();
      return ZH_FAILURE;
   }

   pArea->fEof = *pEof;
   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrFound( AREAP pArea, ZH_BOOL * pFound )
{
   int nOffset;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrFound(%p, %p)", ( void * ) pArea, ( void * ) pFound ) );

   nOffset = ( int ) ( zh_stackTopOffset() - zh_stackBaseOffset() );
   zh_vmPushLogical( pArea->fFound );
   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_FOUND ) )
   {
      zh_stackPop();
      return SUPER_FOUND( pArea, pFound );
   }
   zh_vmPushInteger( pArea->uiArea );
   zh_xvmPushLocalByRef( ( ZH_SHORT ) nOffset );
   zh_vmDo( 2 );

   if( zh_xvmPopLogical( pFound ) )
   {
      zh_ret();
      return ZH_FAILURE;
   }

   pArea->fFound = *pFound;
   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrGoBottom( AREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrGoBottom(%p)", ( void * ) pArea ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_GOBOTTOM ) )
      return SUPER_GOBOTTOM( pArea );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmDo( 1 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrGoTop( AREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrGoTop(%p)", ( void * ) pArea ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_GOTOP ) )
      return SUPER_GOTOP( pArea );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmDo( 1 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrGoTo( AREAP pArea, ZH_ULONG ulRecNo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrGoTo(%p,%lu)", ( void * ) pArea, ulRecNo ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_GOTO ) )
      return SUPER_GOTO( pArea, ulRecNo );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPushLong( ulRecNo );
   zh_vmDo( 2 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrGoToId( AREAP pArea, PZH_ITEM pRecNo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrGoToId(%p,%p)", ( void * ) pArea, ( void * ) pRecNo ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_GOTOID ) )
      return SUPER_GOTOID( pArea, pRecNo );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPush( pRecNo );
   zh_vmDo( 2 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrSeek( AREAP pArea, ZH_BOOL fSoftSeek, PZH_ITEM pItem, ZH_BOOL fFindLast )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrSeek(%p,%d,%p,%d)", ( void * ) pArea, fSoftSeek, ( void * ) pItem, fFindLast ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_SEEK ) )
      return SUPER_SEEK( pArea, fSoftSeek, pItem, fFindLast );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPushLogical( fSoftSeek );
   zh_vmPush( pItem );
   zh_vmPushLogical( fFindLast );
   zh_vmDo( 4 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrSkip( AREAP pArea, ZH_LONG lRecords )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrSkip(%p,%ld)", ( void * ) pArea, lRecords ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_SKIP ) )
      return SUPER_SKIP( pArea, lRecords );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPushLong( lRecords );
   zh_vmDo( 2 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrSkipFilter( AREAP pArea, ZH_LONG lDirect )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrSkipFilter(%p,%ld)", ( void * ) pArea, lDirect ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_SKIPFILTER ) )
      return SUPER_SKIPFILTER( pArea, lDirect );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPushLong( lDirect );
   zh_vmDo( 2 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrSkipRaw( AREAP pArea, ZH_LONG lRecords )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrSkipRaw(%p,%ld)", ( void * ) pArea, lRecords ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_SKIPRAW ) )
      return SUPER_SKIPRAW( pArea, lRecords );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPushLong( lRecords );
   zh_vmDo( 2 );

   return zh_usrReturn();
}


/*
 * Data management
 */

static ZH_ERRCODE zh_usrDeleted( AREAP pArea, ZH_BOOL * pDeleted )
{
   int nOffset;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrDeleted(%p, %p)", ( void * ) pArea, ( void * ) pDeleted ) );

   nOffset = ( int ) ( zh_stackTopOffset() - zh_stackBaseOffset() );
   zh_vmPushLogical( ZH_FALSE );
   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_DELETED ) )
   {
      zh_stackPop();
      return SUPER_DELETED( pArea, pDeleted );
   }
   zh_vmPushInteger( pArea->uiArea );
   zh_xvmPushLocalByRef( ( ZH_SHORT ) nOffset );
   zh_vmDo( 2 );

   if( zh_xvmPopLogical( pDeleted ) )
   {
      zh_ret();
      return ZH_FAILURE;
   }

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrAddField( AREAP pArea, LPDBFIELDINFO pFieldInfo )
{
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrAddField(%p, %p)", ( void * ) pArea, ( void * ) pFieldInfo ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_ADDFIELD ) )
      return SUPER_ADDFIELD( pArea, pFieldInfo );

   pItem = zh_usrFieldInfoToItem( pFieldInfo );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPush( pItem );
   zh_itemRelease( pItem );
   zh_vmDo( 2 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrFieldDisplay( AREAP pArea, LPDBFIELDINFO pFieldInfo )
{
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrFieldDisplay(%p, %p)", ( void * ) pArea, ( void * ) pFieldInfo ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_FIELDDISPLAY ) )
      return SUPER_FIELDDISPLAY( pArea, pFieldInfo );

   pItem = zh_usrFieldInfoToItem( pFieldInfo );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPush( pItem );
   zh_itemRelease( pItem );
   zh_vmDo( 2 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrFieldName( AREAP pArea, ZH_USHORT uiIndex, char * szName )
{
   int nOffset;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrFieldName(%p,%hu,%p)", ( void * ) pArea, uiIndex, ( void * ) szName ) );

   nOffset = ( int ) ( zh_stackTopOffset() - zh_stackBaseOffset() );
   zh_vmPushNil();
   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_FIELDNAME ) )
   {
      zh_stackPop();
      return SUPER_FIELDNAME( pArea, uiIndex, szName );
   }

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPushInteger( uiIndex );
   zh_xvmPushLocalByRef( ( ZH_SHORT ) nOffset );
   zh_vmDo( 3 );

   zh_strncpy( szName, zh_itemGetCPtr( zh_stackItemFromBase( nOffset ) ),
               pArea->uiMaxFieldNameLength );
   zh_stackPop();

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrAppend( AREAP pArea, ZH_BOOL fUnLockAll )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrAppend(%p, %d)", ( void * ) pArea, fUnLockAll ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_APPEND ) )
      return SUPER_APPEND( pArea, fUnLockAll );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPushLogical( fUnLockAll );
   zh_vmDo( 2 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrDelete( AREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrDelete(%p)", ( void * ) pArea ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_DELETE ) )
      return SUPER_DELETE( pArea );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmDo( 1 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrRecall( AREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrRecall(%p)", ( void * ) pArea ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_RECALL ) )
      return SUPER_RECALL( pArea );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmDo( 1 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrFieldCount( AREAP pArea, ZH_USHORT * puiFields )
{
   int nOffset;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrFieldCount(%p,%p)", ( void * ) pArea, ( void * ) puiFields ) );

   nOffset = ( int ) ( zh_stackTopOffset() - zh_stackBaseOffset() );
   zh_vmPushInteger( 0 );
   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_FIELDCOUNT ) )
   {
      zh_stackPop();
      return SUPER_FIELDCOUNT( pArea, puiFields );
   }

   zh_vmPushInteger( pArea->uiArea );
   zh_xvmPushLocalByRef( ( ZH_SHORT ) nOffset );
   zh_vmDo( 2 );

   *puiFields = ( ZH_USHORT ) zh_itemGetNI( zh_stackItemFromBase( nOffset ) );
   zh_stackPop();

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrFlush( AREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrFlush(%p)", ( void * ) pArea ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_FLUSH ) )
      return SUPER_FLUSH( pArea );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmDo( 1 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrGoCold( AREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrGoCold(%p)", ( void * ) pArea ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_GOCOLD ) )
      return SUPER_GOCOLD( pArea );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmDo( 1 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrGoHot( AREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrGoHot(%p)", ( void * ) pArea ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_GOHOT ) )
      return SUPER_GOHOT( pArea );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmDo( 1 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrPutRec( AREAP pArea, const ZH_BYTE * pBuffer )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrPutRec(%p,%p)", ( void * ) pArea, ( const void * ) pBuffer ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_PUTREC ) )
      return SUPER_PUTREC( pArea, pBuffer );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPushPointer( ZH_UNCONST( pBuffer ) );
   zh_vmDo( 2 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrGetRec( AREAP pArea, ZH_BYTE ** pBuffer )
{
   PZH_ITEM pItem;
   int      nOffset;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrGetRec(%p,%p)", ( void * ) pArea, ( void * ) pBuffer ) );

   nOffset = ( int ) ( zh_stackTopOffset() - zh_stackBaseOffset() );
   zh_vmPushNil();
   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_GETREC ) )
   {
      zh_stackPop();
      return SUPER_GETREC( pArea, pBuffer );
   }

   zh_vmPushInteger( pArea->uiArea );
   zh_xvmPushLocalByRef( ( ZH_SHORT ) nOffset );
   zh_vmDo( 2 );

   pItem = zh_stackItemFromBase( nOffset );
   if( ZH_IS_STRING( pItem ) )
      *pBuffer = ( ZH_BYTE * ) ZH_UNCONST( zh_itemGetCPtr( pItem ) );
   else
      *pBuffer = ( ZH_BYTE * ) zh_itemGetPtr( pItem );
   zh_stackPop();

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrGetValue( AREAP pArea, ZH_USHORT uiIndex, PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrGetValue(%p,%hu,%p)", ( void * ) pArea, uiIndex, ( void * ) pItem ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_GETVALUE ) )
      return SUPER_GETVALUE( pArea, uiIndex, pItem );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPushInteger( uiIndex );
   zh_vmPushItemRef( pItem );
   zh_vmDo( 3 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrPutValue( AREAP pArea, ZH_USHORT uiIndex, PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrPutValue(%p,%hu,%p)", ( void * ) pArea, uiIndex, ( void * ) pItem ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_PUTVALUE ) )
      return SUPER_PUTVALUE( pArea, uiIndex, pItem );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPushInteger( uiIndex );
   zh_vmPush( pItem );
   zh_vmDo( 3 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrGetVarLen( AREAP pArea, ZH_USHORT uiIndex, ZH_ULONG * pulLength )
{
   int nOffset;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrGetVarLen(%p,%hu,%p)", ( void * ) pArea, uiIndex, ( void * ) pulLength ) );

   nOffset = ( int ) ( zh_stackTopOffset() - zh_stackBaseOffset() );
   zh_vmPushInteger( 0 );
   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_GETVARLEN ) )
   {
      zh_stackPop();
      return SUPER_GETVARLEN( pArea, uiIndex, pulLength );
   }

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPushInteger( uiIndex );
   zh_xvmPushLocalByRef( ( ZH_SHORT ) nOffset );
   zh_vmDo( 3 );

   *pulLength = zh_itemGetNL( zh_stackItemFromBase( nOffset ) );
   zh_stackPop();

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrRecCount( AREAP pArea, ZH_ULONG * pulRecCount )
{
   int nOffset;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrRecCount(%p,%p)", ( void * ) pArea, ( void * ) pulRecCount ) );

   nOffset = ( int ) ( zh_stackTopOffset() - zh_stackBaseOffset() );
   zh_vmPushInteger( 0 );
   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_RECCOUNT ) )
   {
      zh_stackPop();
      return SUPER_RECCOUNT( pArea, pulRecCount );
   }

   zh_vmPushInteger( pArea->uiArea );
   zh_xvmPushLocalByRef( ( ZH_SHORT ) nOffset );
   zh_vmDo( 2 );

   *pulRecCount = zh_itemGetNL( zh_stackItemFromBase( nOffset ) );
   zh_stackPop();

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrRecInfo( AREAP pArea, PZH_ITEM pRecID, ZH_USHORT uiInfoType, PZH_ITEM pInfo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrRecInfo(%p,%p,%hu,%p)", ( void * ) pArea, ( void * ) pRecID, uiInfoType, ( void * ) pInfo ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_RECINFO ) )
      return SUPER_RECINFO( pArea, pRecID, uiInfoType, pInfo );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPush( pRecID );
   zh_vmPushInteger( uiInfoType );
   zh_vmPushItemRef( pInfo );
   zh_vmDo( 4 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrRecNo( AREAP pArea, ZH_ULONG * pulRecNo )
{
   int nOffset;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrRecNo(%p,%p)", ( void * ) pArea, ( void * ) pulRecNo ) );

   nOffset = ( int ) ( zh_stackTopOffset() - zh_stackBaseOffset() );
   zh_vmPushInteger( 0 );
   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_RECNO ) )
   {
      zh_stackPop();
      return SUPER_RECNO( pArea, pulRecNo );
   }

   zh_vmPushInteger( pArea->uiArea );
   zh_xvmPushLocalByRef( ( ZH_SHORT ) nOffset );
   zh_vmDo( 2 );

   *pulRecNo = zh_itemGetNL( zh_stackItemFromBase( nOffset ) );
   zh_stackPop();

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrRecId( AREAP pArea, PZH_ITEM pRecId )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrRecId(%p,%p)", ( void * ) pArea, ( void * ) pRecId ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_RECID ) )
      return SUPER_RECID( pArea, pRecId );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPushItemRef( pRecId );
   zh_vmDo( 2 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrFieldInfo( AREAP pArea, ZH_USHORT uiIndex, ZH_USHORT uiInfoType, PZH_ITEM pInfo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrFieldInfo(%p,%hu,%hu,%p)", ( void * ) pArea, uiIndex, uiInfoType, ( void * ) pInfo ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_FIELDINFO ) )
      return SUPER_FIELDINFO( pArea, uiIndex, uiInfoType, pInfo );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPushInteger( uiIndex );
   zh_vmPushInteger( uiInfoType );
   zh_vmPushItemRef( pInfo );
   zh_vmDo( 4 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrCreateFields( AREAP pArea, PZH_ITEM pStruct )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrCreateFields(%p,%p)", ( void * ) pArea, ( void * ) pStruct ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_CREATEFIELDS ) )
      return SUPER_CREATEFIELDS( pArea, pStruct );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPush( pStruct );
   zh_vmDo( 2 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrSetFieldExtent( AREAP pArea, ZH_USHORT uiFieldExtent )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrSetFieldExtent(%p,%hu)", ( void * ) pArea, uiFieldExtent ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_SETFIELDEXTENT ) )
      return SUPER_SETFIELDEXTENT( pArea, uiFieldExtent );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPushInteger( uiFieldExtent );
   zh_vmDo( 2 );

   return zh_usrReturn();
}


/*
 * WorkArea/Database management
 */

static ZH_ERRCODE zh_usrAlias( AREAP pArea, char * szAlias )
{
   int nOffset;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrAlias(%p,%p)", ( void * ) pArea, ( void * ) szAlias ) );

   nOffset = ( int ) ( zh_stackTopOffset() - zh_stackBaseOffset() );
   zh_vmPushNil();
   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_ALIAS ) )
   {
      zh_stackPop();
      return SUPER_ALIAS( pArea, szAlias );
   }

   zh_vmPushInteger( pArea->uiArea );
   zh_xvmPushLocalByRef( ( ZH_SHORT ) nOffset );
   zh_vmDo( 2 );

   zh_strncpy( szAlias, zh_itemGetCPtr( zh_stackItemFromBase( nOffset ) ),
               ZH_RDD_MAX_ALIAS_LEN );
   zh_stackPop();

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrClose( AREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrClose(%p)", ( void * ) pArea ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_CLOSE ) )
      return SUPER_CLOSE( pArea );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmDo( 1 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrCreate( AREAP pArea, LPDBOPENINFO pOpenInfo )
{
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrCreate(%p,%p)", ( void * ) pArea, ( void * ) pOpenInfo ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_CREATE ) )
      return SUPER_CREATE( pArea, pOpenInfo );

   pItem = zh_usrOpenInfoToItem( pOpenInfo );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPush( pItem );
   zh_itemRelease( pItem );
   zh_vmDo( 2 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrOpen( AREAP pArea, LPDBOPENINFO pOpenInfo )
{
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrOpen(%p,%p)", ( void * ) pArea, ( void * ) pOpenInfo ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_OPEN ) )
      return SUPER_OPEN( pArea, pOpenInfo );

   pItem = zh_usrOpenInfoToItem( pOpenInfo );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPush( pItem );
   zh_itemRelease( pItem );
   zh_vmDo( 2 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrInfo( AREAP pArea, ZH_USHORT uiInfoType, PZH_ITEM pInfo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrInfo(%p,%hu,%p)", ( void * ) pArea, uiInfoType, ( void * ) pInfo ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_INFO ) )
      return SUPER_INFO( pArea, uiInfoType, pInfo );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPushInteger( uiInfoType );
   zh_vmPushItemRef( pInfo );
   zh_vmDo( 3 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrEval( AREAP pArea, LPDBEVALINFO pEvalInfo )
{
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrEval(%p,%p)", ( void * ) pArea, ( void * ) pEvalInfo ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_DBEVAL ) )
      return SUPER_DBEVAL( pArea, pEvalInfo );

   pItem = zh_usrEvalInfoToItem( pEvalInfo );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPush( pItem );
   zh_itemRelease( pItem );
   zh_vmDo( 2 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrPack( AREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrPack(%p)", ( void * ) pArea ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_PACK ) )
      return SUPER_PACK( pArea );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmDo( 1 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrPackRec( AREAP pArea, ZH_ULONG ulRecNo, ZH_BOOL * pWritten )
{
   int nOffset;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrPackRec(%p,%lu,%p)", ( void * ) pArea, ulRecNo, ( void * ) pWritten ) );

   nOffset = ( int ) ( zh_stackTopOffset() - zh_stackBaseOffset() );
   zh_vmPushLogical( ZH_TRUE );
   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_PACKREC ) )
   {
      zh_stackPop();
      return SUPER_PACKREC( pArea, ulRecNo, pWritten );
   }

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPushLong( ulRecNo );
   zh_xvmPushLocalByRef( ( ZH_SHORT ) nOffset );
   zh_vmDo( 3 );

   if( zh_xvmPopLogical( pWritten ) )
   {
      zh_ret();
      return ZH_FAILURE;
   }

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrSort( AREAP pArea, LPDBSORTINFO pSortInfo )
{
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrSort(%p,%p)", ( void * ) pArea, ( void * ) pSortInfo ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_SORT ) )
      return SUPER_SORT( pArea, pSortInfo );

   pItem = zh_usrSortInfoToItem( pSortInfo );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPush( pItem );
   zh_itemRelease( pItem );
   zh_vmDo( 2 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrTrans( AREAP pArea, LPDBTRANSINFO pTransInfo )
{
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrTrans(%p,%p)", ( void * ) pArea, ( void * ) pTransInfo ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_TRANS ) )
      return SUPER_TRANS( pArea, pTransInfo );

   pItem = zh_usrTransInfoToItem( pTransInfo );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPush( pItem );
   zh_itemRelease( pItem );
   zh_vmDo( 2 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrTransRec( AREAP pArea, LPDBTRANSINFO pTransInfo )
{
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrTransRec(%p,%p)", ( void * ) pArea, ( void * ) pTransInfo ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_TRANSREC ) )
      return SUPER_TRANSREC( pArea, pTransInfo );

   pItem = zh_usrTransInfoToItem( pTransInfo );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPush( pItem );
   zh_itemRelease( pItem );
   zh_vmDo( 2 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrZap( AREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrZap(%p)", ( void * ) pArea ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_ZAP ) )
      return SUPER_ZAP( pArea );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmDo( 1 );

   return zh_usrReturn();
}

/*
 * Relational Methods
 */

static ZH_ERRCODE zh_usrChildEnd( AREAP pArea, LPDBRELINFO pRelInfo )
{
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrChildEnd(%p,%p)", ( void * ) pArea, ( void * ) pRelInfo ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_CHILDEND ) )
      return SUPER_CHILDEND( pArea, pRelInfo );

   pItem = zh_usrRelInfoToItem( pRelInfo );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPush( pItem );
   zh_itemRelease( pItem );
   zh_vmDo( 2 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrChildStart( AREAP pArea, LPDBRELINFO pRelInfo )
{
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrChildStart(%p,%p)", ( void * ) pArea, ( void * ) pRelInfo ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_CHILDSTART ) )
      return SUPER_CHILDSTART( pArea, pRelInfo );

   pItem = zh_usrRelInfoToItem( pRelInfo );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPush( pItem );
   zh_itemRelease( pItem );
   zh_vmDo( 2 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrChildSync( AREAP pArea, LPDBRELINFO pRelInfo )
{
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrChildSync(%p,%p)", ( void * ) pArea, ( void * ) pRelInfo ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_CHILDSYNC ) )
      return SUPER_CHILDSYNC( pArea, pRelInfo );

   pItem = zh_usrRelInfoToItem( pRelInfo );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPush( pItem );
   zh_itemRelease( pItem );
   zh_vmDo( 2 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrSyncChildren( AREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrSyncChildren(%p)", ( void * ) pArea ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_SYNCCHILDREN ) )
      return SUPER_SYNCCHILDREN( pArea );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmDo( 1 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrClearRel( AREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrClearRel(%p)", ( void * ) pArea ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_CLEARREL ) )
      return SUPER_CLEARREL( pArea );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmDo( 1 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrForceRel( AREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrForceRel(%p)", ( void * ) pArea ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_FORCEREL ) )
      return SUPER_FORCEREL( pArea );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmDo( 1 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrRelArea( AREAP pArea, ZH_USHORT uiRelNo, ZH_USHORT * puiRelArea )
{
   int nOffset;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrRelArea(%p,%hu,%p)", ( void * ) pArea, uiRelNo, ( void * ) puiRelArea ) );

   nOffset = ( int ) ( zh_stackTopOffset() - zh_stackBaseOffset() );
   zh_vmPushInteger( 0 );
   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_RELAREA ) )
   {
      zh_stackPop();
      return SUPER_RELAREA( pArea, uiRelNo, puiRelArea );
   }

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPushInteger( uiRelNo );
   zh_xvmPushLocalByRef( ( ZH_SHORT ) nOffset );
   zh_vmDo( 3 );

   *puiRelArea = ( ZH_USHORT ) zh_itemGetNI( zh_stackItemFromBase( nOffset ) );
   zh_stackPop();

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrRelEval( AREAP pArea, LPDBRELINFO pRelInfo )
{
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrRelEval(%p,%p)", ( void * ) pArea, ( void * ) pRelInfo ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_RELEVAL ) )
      return SUPER_RELEVAL( pArea, pRelInfo );

   pItem = zh_usrRelInfoToItem( pRelInfo );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPush( pItem );
   zh_itemRelease( pItem );
   zh_vmDo( 2 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrRelText( AREAP pArea, ZH_USHORT uiRelNo, PZH_ITEM pExpr )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrRelText(%p,%hu,%p)", ( void * ) pArea, uiRelNo, ( void * ) pExpr ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_RELTEXT ) )
      return SUPER_RELTEXT( pArea, uiRelNo, pExpr );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPushInteger( uiRelNo );
   zh_vmPushItemRef( pExpr );
   zh_vmDo( 3 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrSetRel( AREAP pArea, LPDBRELINFO pRelInfo )
{
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrSetRel(%p,%p)", ( void * ) pArea, ( void * ) pRelInfo ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_SETREL ) )
      return SUPER_SETREL( pArea, pRelInfo );

   pItem = zh_usrRelInfoToItem( pRelInfo );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPush( pItem );
   zh_itemRelease( pItem );
   zh_vmDo( 2 );

   return zh_usrReturn();
}


/*
 * Order Management
 */

static ZH_ERRCODE zh_usrOrderListAdd( AREAP pArea, LPDBORDERINFO pOrderInfo )
{
   PZH_ITEM pItem, pResult;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrOrderListAdd(%p,%p)", ( void * ) pArea, ( void * ) pOrderInfo ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_ORDLSTADD ) )
      return SUPER_ORDLSTADD( pArea, pOrderInfo );

   pItem = zh_usrOrderInfoToItem( pOrderInfo );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPush( pItem );
   zh_vmDo( 2 );

   pResult = zh_arrayGetItemPtr( pItem, UR_ORI_RESULT );
   if( pResult && ! ZH_IS_NIL( pResult ) )
   {
      if( pOrderInfo->itmResult )
         zh_itemCopy( pOrderInfo->itmResult, pResult );
      else
         pOrderInfo->itmResult = zh_itemNew( pResult );
   }
   zh_itemRelease( pItem );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrOrderListClear( AREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrOrderListClear(%p)", ( void * ) pArea ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_ORDLSTCLEAR ) )
      return SUPER_ORDLSTCLEAR( pArea );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmDo( 1 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrOrderListDelete( AREAP pArea, LPDBORDERINFO pOrderInfo )
{
   PZH_ITEM pItem, pResult;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrOrderListDelete(%p,%p)", ( void * ) pArea, ( void * ) pOrderInfo ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_ORDLSTDELETE ) )
      return SUPER_ORDLSTDELETE( pArea, pOrderInfo );

   pItem = zh_usrOrderInfoToItem( pOrderInfo );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPush( pItem );
   zh_vmDo( 2 );

   pResult = zh_arrayGetItemPtr( pItem, UR_ORI_RESULT );
   if( pResult && ! ZH_IS_NIL( pResult ) )
   {
      if( pOrderInfo->itmResult )
         zh_itemCopy( pOrderInfo->itmResult, pResult );
      else
         pOrderInfo->itmResult = zh_itemNew( pResult );
   }
   zh_itemRelease( pItem );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrOrderListFocus( AREAP pArea, LPDBORDERINFO pOrderInfo )
{
   PZH_ITEM pItem, pResult;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrOrderListFocus(%p,%p)", ( void * ) pArea, ( void * ) pOrderInfo ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_ORDLSTFOCUS ) )
      return SUPER_ORDLSTFOCUS( pArea, pOrderInfo );

   pItem = zh_usrOrderInfoToItem( pOrderInfo );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPush( pItem );
   zh_vmDo( 2 );

   pResult = zh_arrayGetItemPtr( pItem, UR_ORI_RESULT );
   if( pResult && ! ZH_IS_NIL( pResult ) )
   {
      if( pOrderInfo->itmResult )
         zh_itemCopy( pOrderInfo->itmResult, pResult );
      else
         pOrderInfo->itmResult = zh_itemNew( pResult );
   }
   zh_itemRelease( pItem );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrOrderListRebuild( AREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrOrderListRebuild(%p)", ( void * ) pArea ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_ORDLSTREBUILD ) )
      return SUPER_ORDLSTREBUILD( pArea );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmDo( 1 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrOrderCondition( AREAP pArea, LPDBORDERCONDINFO pOrderCondInfo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrOrderCondition(%p,%p)", ( void * ) pArea, ( void * ) pOrderCondInfo ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_ORDSETCOND ) )
      return SUPER_ORDSETCOND( pArea, pOrderCondInfo );


   zh_vmPushInteger( pArea->uiArea );
   if( pOrderCondInfo )
   {
      PZH_ITEM pItem = zh_usrOrderCondInfoToItem( pOrderCondInfo );
      zh_vmPush( pItem );
      zh_itemRelease( pItem );
      zh_usrOrderCondFree( pOrderCondInfo );
   }
   else
   {
      zh_vmPushNil();
   }
   zh_vmDo( 2 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrOrderCreate( AREAP pArea, LPDBORDERCREATEINFO pOrderCreateInfo )
{
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrOrderCreate(%p,%p)", ( void * ) pArea, ( void * ) pOrderCreateInfo ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_ORDCREATE ) )
      return SUPER_ORDCREATE( pArea, pOrderCreateInfo );

   pItem = zh_usrOrderCreateInfoToItem( pOrderCreateInfo );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPush( pItem );
   zh_itemRelease( pItem );
   zh_vmDo( 2 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrOrderDestroy( AREAP pArea, LPDBORDERINFO pOrderInfo )
{
   PZH_ITEM pItem, pResult;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrOrderDestroy(%p,%p)", ( void * ) pArea, ( void * ) pOrderInfo ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_ORDDESTROY ) )
      return SUPER_ORDDESTROY( pArea, pOrderInfo );

   pItem = zh_usrOrderInfoToItem( pOrderInfo );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPush( pItem );
   zh_vmDo( 2 );

   pResult = zh_arrayGetItemPtr( pItem, UR_ORI_RESULT );
   if( pResult && ! ZH_IS_NIL( pResult ) )
   {
      if( pOrderInfo->itmResult )
         zh_itemCopy( pOrderInfo->itmResult, pResult );
      else
         pOrderInfo->itmResult = zh_itemNew( pResult );
   }
   zh_itemRelease( pItem );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrOrderInfo( AREAP pArea, ZH_USHORT uiIndex, LPDBORDERINFO pOrderInfo )
{
   PZH_ITEM pItem, pResult;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrOrderInfo(%p,%hu,%p)", ( void * ) pArea, uiIndex, ( void * ) pOrderInfo ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_ORDINFO ) )
      return SUPER_ORDINFO( pArea, uiIndex, pOrderInfo );

   pItem = zh_usrOrderInfoToItem( pOrderInfo );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPushInteger( uiIndex );
   zh_vmPush( pItem );
   zh_vmDo( 3 );

   pResult = zh_arrayGetItemPtr( pItem, UR_ORI_RESULT );
   if( pResult && ! ZH_IS_NIL( pResult ) )
   {
      if( pOrderInfo->itmResult )
         zh_itemCopy( pOrderInfo->itmResult, pResult );
      else
         pOrderInfo->itmResult = zh_itemNew( pResult );
   }
   zh_itemRelease( pItem );

   return zh_usrReturn();
}


/*
 * Filters and Scope Settings
 */

static ZH_ERRCODE zh_usrClearFilter( AREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrClearFilter(%p)", ( void * ) pArea ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_CLEARFILTER ) )
      return SUPER_CLEARFILTER( pArea );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmDo( 1 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrClearLocate( AREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrClearLocate(%p)", ( void * ) pArea ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_CLEARLOCATE ) )
      return SUPER_CLEARLOCATE( pArea );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmDo( 1 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrClearScope( AREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrClearScope(%p)", ( void * ) pArea ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_CLEARSCOPE ) )
      return SUPER_CLEARSCOPE( pArea );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmDo( 1 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrFilterText( AREAP pArea, PZH_ITEM pFilter )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrFilterText(%p,%p)", ( void * ) pArea, ( void * ) pFilter ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_FILTERTEXT ) )
      return SUPER_FILTERTEXT( pArea, pFilter );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPushItemRef( pFilter );
   zh_vmDo( 2 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrSetFilter( AREAP pArea, LPDBFILTERINFO pFilterInfo )
{
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrSetFilter(%p,%p)", ( void * ) pArea, ( void * ) pFilterInfo ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_SETFILTER ) )
      return SUPER_SETFILTER( pArea, pFilterInfo );

   pItem = zh_usrFilterInfoToItem( pFilterInfo );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPush( pItem );
   zh_itemRelease( pItem );
   zh_vmDo( 2 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrSetLocate( AREAP pArea, LPDBSCOPEINFO pScopeInfo )
{
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrSetLocate(%p,%p)", ( void * ) pArea, ( void * ) pScopeInfo ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_SETLOCATE ) )
      return SUPER_SETLOCATE( pArea, pScopeInfo );

   pItem = zh_usrScopeInfoToItem( pScopeInfo );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPush( pItem );
   zh_itemRelease( pItem );
   zh_vmDo( 2 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrLocate( AREAP pArea, ZH_BOOL fContinue )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrLocate(%p,%d)", ( void * ) pArea, fContinue ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_LOCATE ) )
      return SUPER_LOCATE( pArea, fContinue );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPushLogical( fContinue );
   zh_vmDo( 2 );

   return zh_usrReturn();
}


/*
 * Miscellaneous
 */

static ZH_ERRCODE zh_usrCompile( AREAP pArea, const char * szExpr )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrCompile(%p,%p)", ( void * ) pArea, ( const void * ) szExpr ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_COMPILE ) )
      return SUPER_COMPILE( pArea, szExpr );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPushString( szExpr, strlen( szExpr ) );
   zh_vmDo( 2 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrError( AREAP pArea, PZH_ITEM pError )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrError(%p,%p)", ( void * ) pArea, ( void * ) pError ) );

   if( ! pArea )
   {
      zh_errPutSeverity( pError, ES_ERROR );
      zh_errPutSubSystem( pError, "???DRIVER" );
      return zh_errLaunch( pError );
   }

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_ERROR ) )
      return SUPER_ERROR( pArea, pError );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPush( pError );
   zh_vmDo( 2 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrEvalBlock( AREAP pArea, PZH_ITEM pBlock )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrEvalBlock(%p,%p)", ( void * ) pArea, ( void * ) pBlock ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_EVALBLOCK ) )
      return SUPER_EVALBLOCK( pArea, pBlock );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPush( pBlock );
   zh_vmDo( 2 );

   return zh_usrReturn();
}


/*
 * Network operations
 */

static ZH_ERRCODE zh_usrRawLock( AREAP pArea, ZH_USHORT uiAction, ZH_ULONG ulRecNo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrRawLock(%p,%hu,%lu)", ( void * ) pArea, uiAction, ulRecNo ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_RAWLOCK ) )
      return SUPER_RAWLOCK( pArea, uiAction, ulRecNo );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPushInteger( uiAction );
   zh_vmPushLong( ulRecNo );
   zh_vmDo( 3 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrLock( AREAP pArea, LPDBLOCKINFO pLockInfo )
{
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrLock(%p,%p)", ( void * ) pArea, ( void * ) pLockInfo ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_LOCK ) )
      return SUPER_LOCK( pArea, pLockInfo );

   pItem = zh_usrLockInfoToItem( pLockInfo );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPush( pItem );
   zh_vmDo( 2 );

   pLockInfo->fResult = ( ZH_USHORT ) zh_arrayGetL( pItem, UR_LI_RESULT );
   zh_itemRelease( pItem );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrUnLock( AREAP pArea, PZH_ITEM pRecNo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrUnLock(%p,%p)", ( void * ) pArea, ( void * ) pRecNo ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_UNLOCK ) )
      return SUPER_UNLOCK( pArea, pRecNo );

   zh_vmPushInteger( pArea->uiArea );
   if( pRecNo )
      zh_vmPush( pRecNo );
   else
      zh_vmPushNil();
   zh_vmDo( 2 );

   return zh_usrReturn();
}


/*
 * Memofile functions
 */

static ZH_ERRCODE zh_usrCloseMemFile( AREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrCloseMemFile(%p)", ( void * ) pArea ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_CLOSEMEMFILE ) )
      return SUPER_CLOSEMEMFILE( pArea );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmDo( 1 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrCreateMemFile( AREAP pArea, LPDBOPENINFO pOpenInfo )
{
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrCreateMemFile(%p,%p)", ( void * ) pArea, ( void * ) pOpenInfo ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_CREATEMEMFILE ) )
      return SUPER_CREATEMEMFILE( pArea, pOpenInfo );

   pItem = zh_usrOpenInfoToItem( pOpenInfo );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPush( pItem );
   zh_itemRelease( pItem );
   zh_vmDo( 2 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrOpenMemFile( AREAP pArea, LPDBOPENINFO pOpenInfo )
{
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrOpenMemFile(%p,%p)", ( void * ) pArea, ( void * ) pOpenInfo ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_OPENMEMFILE ) )
      return SUPER_OPENMEMFILE( pArea, pOpenInfo );

   pItem = zh_usrOpenInfoToItem( pOpenInfo );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPush( pItem );
   zh_itemRelease( pItem );
   zh_vmDo( 2 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrGetValueFile( AREAP pArea, ZH_USHORT uiIndex, const char * szFile, ZH_USHORT uiMode )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrGetValueFile(%p,%hu,%p,%hu)", ( void * ) pArea, uiIndex, ( const void * ) szFile, uiMode ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_GETVALUEFILE ) )
      return SUPER_GETVALUEFILE( pArea, uiIndex, szFile, uiMode );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPushInteger( uiIndex );
   zh_vmPushString( szFile, strlen( szFile ) );
   zh_vmPushInteger( uiMode );
   zh_vmDo( 4 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrPutValueFile( AREAP pArea, ZH_USHORT uiIndex, const char * szFile, ZH_USHORT uiMode )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrPutValueFile(%p,%hu,%p,%hu)", ( void * ) pArea, uiIndex, ( const void * ) szFile, uiMode ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_PUTVALUEFILE ) )
      return SUPER_PUTVALUEFILE( pArea, uiIndex, szFile, uiMode );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmPushInteger( uiIndex );
   zh_vmPushString( szFile, strlen( szFile ) );
   zh_vmPushInteger( uiMode );
   zh_vmDo( 4 );

   return zh_usrReturn();
}


/*
 * Database file header handling
 */

static ZH_ERRCODE zh_usrReadDBHeader( AREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrReadDBHeader(%p)", ( void * ) pArea ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_READDBHEADER ) )
      return SUPER_READDBHEADER( pArea );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmDo( 1 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrWriteDBHeader( AREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrWriteDBHeader(%p)", ( void * ) pArea ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_WRITEDBHEADER ) )
      return SUPER_WRITEDBHEADER( pArea );

   zh_vmPushInteger( pArea->uiArea );
   zh_vmDo( 1 );

   return zh_usrReturn();
}


/*
 * non WorkArea functions
 */

static ZH_ERRCODE zh_usrDrop( LPRDDNODE pRDD, PZH_ITEM pTable, PZH_ITEM pIndex, ZH_ULONG ulConnection )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrDrop(%p,%p,%p,%lu)", ( void * ) pRDD, ( void * ) pTable, ( void * ) pIndex, ulConnection ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pRDD )->pMethods, UR_DROP ) )
      return SUPER_DROP( pRDD, pTable, pIndex, ulConnection );

   zh_vmPushInteger( pRDD->rddID );
   zh_vmPush( pTable );
   zh_vmPush( pIndex );
   zh_vmPushLong( ulConnection );
   zh_vmDo( 4 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrExists( LPRDDNODE pRDD, PZH_ITEM pTable, PZH_ITEM pIndex, ZH_ULONG ulConnection )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrExists(%p,%p,%p,%lu)", ( void * ) pRDD, ( void * ) pTable, ( void * ) pIndex, ulConnection ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pRDD )->pMethods, UR_EXISTS ) )
      return SUPER_EXISTS( pRDD, pTable, pIndex, ulConnection );

   zh_vmPushInteger( pRDD->rddID );
   zh_vmPush( pTable );
   zh_vmPush( pIndex );
   zh_vmPushLong( ulConnection );
   zh_vmDo( 4 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrRename( LPRDDNODE pRDD, PZH_ITEM pTable, PZH_ITEM pIndex, PZH_ITEM pNewName, ZH_ULONG ulConnection )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrRename(%p,%p,%p,%p,%lu)", ( void * ) pRDD, ( void * ) pTable, ( void * ) pIndex, ( void * ) pNewName, ulConnection ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pRDD )->pMethods, UR_RENAME ) )
      return SUPER_RENAME( pRDD, pTable, pIndex, pNewName, ulConnection );

   zh_vmPushInteger( pRDD->rddID );
   zh_vmPush( pTable );
   zh_vmPush( pIndex );
   zh_vmPush( pNewName );
   zh_vmPushLong( ulConnection );
   zh_vmDo( 5 );

   return zh_usrReturn();
}

static ZH_ERRCODE zh_usrRddInfo( LPRDDNODE pRDD, ZH_USHORT uiInfoType, ZH_ULONG ulConnection, PZH_ITEM pInfo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_usrRddInfo(%p,%hu,%lu,%p)", ( void * ) pRDD, uiInfoType, ulConnection, ( void * ) pInfo ) );

   if( ! zh_usrPushMethod( SELF_USRNODE( pRDD )->pMethods, UR_RDDINFO ) )
      return SUPER_RDDINFO( pRDD, uiInfoType, ulConnection, pInfo );

   zh_vmPushInteger( pRDD->rddID );
   zh_vmPushInteger( uiInfoType );
   zh_vmPushLong( ulConnection );
   zh_vmPushItemRef( pInfo );
   zh_vmDo( 4 );

   return zh_usrReturn();
}

typedef union
{
   RDDFUNCS   funcTable;
   DBENTRYP_V funcentries[ 1 ];
}
ZH_RDD_FUNCTABLE;

static const ZH_RDD_FUNCTABLE usrFuncTable =
{ {
   /* Movement and positioning methods */
   /* ( DBENTRYP_BP )   */ zh_usrBof,         /* Bof        */
   /* ( DBENTRYP_BP )   */ zh_usrEof,         /* Eof        */
   /* ( DBENTRYP_BP )   */ zh_usrFound,       /* Found      */
   /* ( DBENTRYP_V )    */ zh_usrGoBottom,    /* GoBottom   */
   /* ( DBENTRYP_UL )   */ zh_usrGoTo,        /* GoTo       */
   /* ( DBENTRYP_I )    */ zh_usrGoToId,      /* GoToId     */
   /* ( DBENTRYP_V )    */ zh_usrGoTop,       /* GoTop      */
   /* ( DBENTRYP_BIB )  */ zh_usrSeek,        /* Seek       */
   /* ( DBENTRYP_L )    */ zh_usrSkip,        /* Skip       */
   /* ( DBENTRYP_L )    */ zh_usrSkipFilter,  /* SkipFilter */
   /* ( DBENTRYP_L )    */ zh_usrSkipRaw,     /* SkipRaw    */

   /* Data management */
   /* ( DBENTRYP_VF )   */ zh_usrAddField,    /* AddField       */
   /* ( DBENTRYP_B )    */ zh_usrAppend,      /* Append         */
   /* ( DBENTRYP_I )    */ zh_usrCreateFields,/* CreateFields   */
   /* ( DBENTRYP_V )    */ zh_usrDelete,      /* DeleteRec      */
   /* ( DBENTRYP_BP )   */ zh_usrDeleted,     /* Deleted        */
   /* ( DBENTRYP_SP )   */ zh_usrFieldCount,  /* FieldCount     */
   /* ( DBENTRYP_VF )   */ zh_usrFieldDisplay,/* FieldDisplay   */
   /* ( DBENTRYP_SSI )  */ zh_usrFieldInfo,   /* FieldInfo      */
   /* ( DBENTRYP_SCP )  */ zh_usrFieldName,   /* FieldName      */
   /* ( DBENTRYP_V )    */ zh_usrFlush,       /* Flush          */
   /* ( DBENTRYP_PP )   */ zh_usrGetRec,      /* GetRec         */
   /* ( DBENTRYP_SI )   */ zh_usrGetValue,    /* GetValue       */
   /* ( DBENTRYP_SVL )  */ zh_usrGetVarLen,   /* GetVarLen      */
   /* ( DBENTRYP_V )    */ zh_usrGoCold,      /* GoCold         */
   /* ( DBENTRYP_V )    */ zh_usrGoHot,       /* GoHot          */
   /* ( DBENTRYP_P )    */ zh_usrPutRec,      /* PutRec         */
   /* ( DBENTRYP_SI )   */ zh_usrPutValue,    /* PutValue       */
   /* ( DBENTRYP_V )    */ zh_usrRecall,      /* Recall         */
   /* ( DBENTRYP_ULP )  */ zh_usrRecCount,    /* RecCount       */
   /* ( DBENTRYP_ISI )  */ zh_usrRecInfo,     /* RecInfo        */
   /* ( DBENTRYP_ULP )  */ zh_usrRecNo,       /* RecNo          */
   /* ( DBENTRYP_I )    */ zh_usrRecId,       /* RecId          */
   /* ( DBENTRYP_S )    */ zh_usrSetFieldExtent, /* SetFieldExtent */

   /* WorkArea/Database management */
   /* ( DBENTRYP_CP )   */ zh_usrAlias,       /* Alias       */
   /* ( DBENTRYP_V )    */ zh_usrClose,       /* Close       */
   /* ( DBENTRYP_VO )   */ zh_usrCreate,      /* Create      */
   /* ( DBENTRYP_SI )   */ zh_usrInfo,        /* Info        */
   /* ( DBENTRYP_V )    */ NULL, /* RDD */    /* NewArea     */
   /* ( DBENTRYP_VO )   */ zh_usrOpen,        /* Open        */
   /* ( DBENTRYP_V )    */ NULL, /* RDD */    /* Release     */
   /* ( DBENTRYP_SP )   */ NULL, /* RDD */    /* StructSize  */
   /* ( DBENTRYP_CP )   */ NULL, /* RDD */    /* SysName     */
   /* ( DBENTRYP_VEI )  */ zh_usrEval,        /* Eval        */
   /* ( DBENTRYP_V )    */ zh_usrPack,        /* Pack        */
   /* ( DBENTRYP_LSP )  */ zh_usrPackRec,     /* PackRec     */
   /* ( DBENTRYP_VS )   */ zh_usrSort,        /* Sort        */
   /* ( DBENTRYP_VT )   */ zh_usrTrans,       /* Trans       */
   /* ( DBENTRYP_VT )   */ zh_usrTransRec,    /* TransRec    */
   /* ( DBENTRYP_V )    */ zh_usrZap,         /* Zap         */

   /* Relational Methods */
   /* ( DBENTRYP_VR )   */ zh_usrChildEnd,    /* ChildEnd      */
   /* ( DBENTRYP_VR )   */ zh_usrChildStart,  /* ChildStart    */
   /* ( DBENTRYP_VR )   */ zh_usrChildSync,   /* ChildSync     */
   /* ( DBENTRYP_V )    */ zh_usrSyncChildren,/* SyncChildren  */
   /* ( DBENTRYP_V )    */ zh_usrClearRel,    /* ClearRel      */
   /* ( DBENTRYP_V )    */ zh_usrForceRel,    /* ForceRel      */
   /* ( DBENTRYP_SCS )  */ zh_usrRelArea,     /* RelArea       */
   /* ( DBENTRYP_VR )   */ zh_usrRelEval,     /* RelEval       */
   /* ( DBENTRYP_SI )   */ zh_usrRelText,     /* RelText       */
   /* ( DBENTRYP_VR )   */ zh_usrSetRel,      /* SetRel        */

   /* Order Management */
   /* ( DBENTRYP_OI )   */ zh_usrOrderListAdd,     /* OrderListAdd      */
   /* ( DBENTRYP_V )    */ zh_usrOrderListClear,   /* OrderListClear    */
   /* ( DBENTRYP_OI )   */ zh_usrOrderListDelete,  /* OrderListDelete   */
   /* ( DBENTRYP_OI )   */ zh_usrOrderListFocus,   /* OrderListFocus    */
   /* ( DBENTRYP_V )    */ zh_usrOrderListRebuild, /* OrderListRebuild  */
   /* ( DBENTRYP_VOI )  */ zh_usrOrderCondition,   /* OrderCondition    */
   /* ( DBENTRYP_VOC )  */ zh_usrOrderCreate,      /* OrderCreate       */
   /* ( DBENTRYP_OI )   */ zh_usrOrderDestroy,     /* OrderDestroy      */
   /* ( DBENTRYP_OII )  */ zh_usrOrderInfo,        /* OrderInfo         */

   /* Filters and Scope Settings */
   /* ( DBENTRYP_V )    */ zh_usrClearFilter, /* ClearFilter  */
   /* ( DBENTRYP_V )    */ zh_usrClearLocate, /* ClearLocate  */
   /* ( DBENTRYP_V )    */ zh_usrClearScope,  /* ClearScope   */
   /* ( DBENTRYP_VPLP ) */ NULL,              /* CountScope   */
   /* ( DBENTRYP_I )    */ zh_usrFilterText,  /* FilterText   */
   /* ( DBENTRYP_SI )   */ NULL,              /* ScopeInfo    */
   /* ( DBENTRYP_VFI )  */ zh_usrSetFilter,   /* SetFilter    */
   /* ( DBENTRYP_VLO )  */ zh_usrSetLocate,   /* SetLocate    */
   /* ( DBENTRYP_VOS )  */ NULL,              /* SetScope     */
   /* ( DBENTRYP_VPL )  */ NULL,              /* SkipScope    */
   /* ( DBENTRYP_B )    */ zh_usrLocate,      /* Locate       */

   /* Miscellaneous */
   /* ( DBENTRYP_CC )   */ zh_usrCompile,     /* Compile    */
   /* ( DBENTRYP_I )    */ zh_usrError,       /* Error      */
   /* ( DBENTRYP_I )    */ zh_usrEvalBlock,   /* EvalBlock  */

   /* Network operations */
   /* ( DBENTRYP_VSP )  */ zh_usrRawLock,     /* RawLock  */
   /* ( DBENTRYP_VL )   */ zh_usrLock,        /* Lock     */
   /* ( DBENTRYP_I )    */ zh_usrUnLock,      /* UnLock   */

   /* Memofile functions */
   /* ( DBENTRYP_V )    */ zh_usrCloseMemFile,  /* CloseMemFile   */
   /* ( DBENTRYP_VO )   */ zh_usrCreateMemFile, /* CreateMemFile  */
   /* ( DBENTRYP_SCCS ) */ zh_usrGetValueFile,  /* GetValueFile   */
   /* ( DBENTRYP_VO )   */ zh_usrOpenMemFile,   /* OpenMemFile    */
   /* ( DBENTRYP_SCCS ) */ zh_usrPutValueFile,  /* PutValueFile   */

   /* Database file header handling */
   /* ( DBENTRYP_V )    */ zh_usrReadDBHeader,  /* ReadDBHeader   */
   /* ( DBENTRYP_V )    */ zh_usrWriteDBHeader, /* WriteDBHeader  */

   /* non WorkArea functions */
   /* ( DBENTRYP_R )    */ NULL, /* RDD */    /* Init    */
   /* ( DBENTRYP_R )    */ NULL, /* RDD */    /* Exit    */
   /* ( DBENTRYP_RVVL ) */ zh_usrDrop,        /* Drop    */
   /* ( DBENTRYP_RVVL ) */ zh_usrExists,      /* Exists  */
   /* ( DBENTRYP_RVVVL )*/ zh_usrRename,      /* Rename  */
   /* ( DBENTRYP_RSLV ) */ zh_usrRddInfo,     /* RddInfo */

   /* Special and reserved methods */
   /* ( DBENTRYP_SVP )  */ NULL               /* WhoCares */
} };

static const ZH_RDD_FUNCTABLE rddFuncTable =
{ {
   /* Movement and positioning methods */
   /* ( DBENTRYP_BP )   */ NULL,              /* Bof        */
   /* ( DBENTRYP_BP )   */ NULL,              /* Eof        */
   /* ( DBENTRYP_BP )   */ NULL,              /* Found      */
   /* ( DBENTRYP_V )    */ NULL,              /* GoBottom   */
   /* ( DBENTRYP_UL )   */ NULL,              /* GoTo       */
   /* ( DBENTRYP_I )    */ NULL,              /* GoToId     */
   /* ( DBENTRYP_V )    */ NULL,              /* GoTop      */
   /* ( DBENTRYP_BIB )  */ NULL,              /* Seek       */
   /* ( DBENTRYP_L )    */ NULL,              /* Skip       */
   /* ( DBENTRYP_L )    */ NULL,              /* SkipFilter */
   /* ( DBENTRYP_L )    */ NULL,              /* SkipRaw    */

   /* Data management */
   /* ( DBENTRYP_VF )   */ NULL,              /* AddField       */
   /* ( DBENTRYP_B )    */ NULL,              /* Append         */
   /* ( DBENTRYP_I )    */ NULL,              /* CreateFields   */
   /* ( DBENTRYP_V )    */ NULL,              /* DeleteRec      */
   /* ( DBENTRYP_BP )   */ NULL,              /* Deleted        */
   /* ( DBENTRYP_SP )   */ NULL,              /* FieldCount     */
   /* ( DBENTRYP_VF )   */ NULL,              /* FieldDisplay   */
   /* ( DBENTRYP_SSI )  */ NULL,              /* FieldInfo      */
   /* ( DBENTRYP_SCP )  */ NULL,              /* FieldName      */
   /* ( DBENTRYP_V )    */ NULL,              /* Flush          */
   /* ( DBENTRYP_PP )   */ NULL,              /* GetRec         */
   /* ( DBENTRYP_SI )   */ NULL,              /* GetValue       */
   /* ( DBENTRYP_SVL )  */ NULL,              /* GetVarLen      */
   /* ( DBENTRYP_V )    */ NULL,              /* GoCold         */
   /* ( DBENTRYP_V )    */ NULL,              /* GoHot          */
   /* ( DBENTRYP_P )    */ NULL,              /* PutRec         */
   /* ( DBENTRYP_SI )   */ NULL,              /* PutValue       */
   /* ( DBENTRYP_V )    */ NULL,              /* Recall         */
   /* ( DBENTRYP_ULP )  */ NULL,              /* RecCount       */
   /* ( DBENTRYP_ISI )  */ NULL,              /* RecInfo        */
   /* ( DBENTRYP_ULP )  */ NULL,              /* RecNo          */
   /* ( DBENTRYP_I )    */ NULL,              /* RecId          */
   /* ( DBENTRYP_S )    */ NULL,              /* SetFieldExtent */

   /* WorkArea/Database management */
   /* ( DBENTRYP_CP )   */ NULL,              /* Alias       */
   /* ( DBENTRYP_V )    */ NULL,              /* Close       */
   /* ( DBENTRYP_VO )   */ NULL,              /* Create      */
   /* ( DBENTRYP_SI )   */ NULL,              /* Info        */
   /* ( DBENTRYP_V )    */ zh_usrNewArea,     /* NewArea     */
   /* ( DBENTRYP_VO )   */ NULL,              /* Open        */
   /* ( DBENTRYP_V )    */ zh_usrRelease,     /* Release     */
   /* ( DBENTRYP_SP )   */ zh_usrStructSize,  /* StructSize  */
   /* ( DBENTRYP_CP )   */ zh_usrSysName,     /* SysName     */
   /* ( DBENTRYP_VEI )  */ NULL,              /* Eval        */
   /* ( DBENTRYP_V )    */ NULL,              /* Pack        */
   /* ( DBENTRYP_LSP )  */ NULL,              /* PackRec     */
   /* ( DBENTRYP_VS )   */ NULL,              /* Sort        */
   /* ( DBENTRYP_VT )   */ NULL,              /* Trans       */
   /* ( DBENTRYP_VT )   */ NULL,              /* TransRec    */
   /* ( DBENTRYP_V )    */ NULL,              /* Zap         */

   /* Relational Methods */
   /* ( DBENTRYP_VR )   */ NULL,              /* ChildEnd      */
   /* ( DBENTRYP_VR )   */ NULL,              /* ChildStart    */
   /* ( DBENTRYP_VR )   */ NULL,              /* ChildSync     */
   /* ( DBENTRYP_V )    */ NULL,              /* SyncChildren  */
   /* ( DBENTRYP_V )    */ NULL,              /* ClearRel      */
   /* ( DBENTRYP_V )    */ NULL,              /* ForceRel      */
   /* ( DBENTRYP_SCS )  */ NULL,              /* RelArea       */
   /* ( DBENTRYP_VR )   */ NULL,              /* RelEval       */
   /* ( DBENTRYP_SI )   */ NULL,              /* RelText       */
   /* ( DBENTRYP_VR )   */ NULL,              /* SetRel        */

   /* Order Management */
   /* ( DBENTRYP_OI )   */ NULL,              /* OrderListAdd      */
   /* ( DBENTRYP_V )    */ NULL,              /* OrderListClear    */
   /* ( DBENTRYP_OI )   */ NULL,              /* OrderListDelete   */
   /* ( DBENTRYP_OI )   */ NULL,              /* OrderListFocus    */
   /* ( DBENTRYP_V )    */ NULL,              /* OrderListRebuild  */
   /* ( DBENTRYP_VOI )  */ NULL,              /* OrderCondition    */
   /* ( DBENTRYP_VOC )  */ NULL,              /* OrderCreate       */
   /* ( DBENTRYP_OI )   */ NULL,              /* OrderDestroy      */
   /* ( DBENTRYP_OII )  */ NULL,              /* OrderInfo         */

   /* Filters and Scope Settings */
   /* ( DBENTRYP_V )    */ NULL,              /* ClearFilter  */
   /* ( DBENTRYP_V )    */ NULL,              /* ClearLocate  */
   /* ( DBENTRYP_V )    */ NULL,              /* ClearScope   */
   /* ( DBENTRYP_VPLP ) */ NULL,              /* CountScope   */
   /* ( DBENTRYP_I )    */ NULL,              /* FilterText   */
   /* ( DBENTRYP_SI )   */ NULL,              /* ScopeInfo    */
   /* ( DBENTRYP_VFI )  */ NULL,              /* SetFilter    */
   /* ( DBENTRYP_VLO )  */ NULL,              /* SetLocate    */
   /* ( DBENTRYP_VOS )  */ NULL,              /* SetScope     */
   /* ( DBENTRYP_VPL )  */ NULL,              /* SkipScope    */
   /* ( DBENTRYP_B )    */ NULL,              /* Locate       */

   /* Miscellaneous */
   /* ( DBENTRYP_CC )   */ NULL,              /* Compile    */
   /* ( DBENTRYP_I )    */ NULL,              /* Error      */
   /* ( DBENTRYP_I )    */ NULL,              /* EvalBlock  */

   /* Network operations */
   /* ( DBENTRYP_VSP )  */ NULL,              /* RawLock  */
   /* ( DBENTRYP_VL )   */ NULL,              /* Lock     */
   /* ( DBENTRYP_I )    */ NULL,              /* UnLock   */

   /* Memofile functions */
   /* ( DBENTRYP_V )    */ NULL,              /* CloseMemFile   */
   /* ( DBENTRYP_VO )   */ NULL,              /* CreateMemFile  */
   /* ( DBENTRYP_SCCS ) */ NULL,              /* GetValueFile   */
   /* ( DBENTRYP_VO )   */ NULL,              /* OpenMemFile    */
   /* ( DBENTRYP_SCCS ) */ NULL,              /* PutValueFile   */

   /* Database file header handling */
   /* ( DBENTRYP_V )    */ NULL,              /* ReadDBHeader   */
   /* ( DBENTRYP_V )    */ NULL,              /* WriteDBHeader  */

   /* non WorkArea functions */
   /* ( DBENTRYP_R )    */ zh_usrInit,        /* Init    */
   /* ( DBENTRYP_R )    */ zh_usrExit,        /* Exit    */
   /* ( DBENTRYP_RVVL ) */ NULL,              /* Drop    */
   /* ( DBENTRYP_RVVL ) */ NULL,              /* Exists  */
   /* ( DBENTRYP_RVVVL )*/ NULL,              /* Rename  */
   /* ( DBENTRYP_RSLV ) */ NULL,              /* RddInfo */

   /* Special and reserved methods */
   /* ( DBENTRYP_SVP )  */ NULL               /* WhoCares */
} };

ZH_FUNC( USRRDD_GETFUNCTABLE )
{
   RDDFUNCS * pSelfTable, * pSuperTable;
   ZH_USHORT * puiCount, * puiSuperRddId, uiCount, uiSize;
   const char * szSuperRDD;
   PZH_ITEM pMethods;

   ZH_TRACE( ZH_TR_DEBUG, ( "USRRDD_GETFUNCTABLE()" ) );

   puiCount    = ( ZH_USHORT * ) zh_parptr( 1 );
   pSelfTable  = ( RDDFUNCS * ) zh_parptr( 2 );
   pSuperTable = ( RDDFUNCS * ) zh_parptr( 3 );
#if 0
   uiRddID = zh_parni( 4 );
#endif
   szSuperRDD = zh_parc( 5 );
   pMethods = zh_param( 6, ZH_IT_ARRAY );
   puiSuperRddId = ( ZH_USHORT * ) zh_parptr( 7 );

   if( puiCount && pSelfTable && pSuperTable && pMethods )
   {
      ZH_ERRCODE uiResult;
      ZH_RDD_FUNCTABLE funcTable;
      DBENTRYP_V * pFunction;
      const DBENTRYP_V * pUsrFunction, * pRddFunction;

      *puiCount = RDDFUNCSCOUNT;
      uiSize = ( ZH_USHORT ) zh_arrayLen( pMethods );

      pUsrFunction = usrFuncTable.funcentries;
      pRddFunction = rddFuncTable.funcentries;
      pFunction    = funcTable.funcentries;

      for( uiCount = 1; uiCount <= RDDFUNCSCOUNT; ++uiCount )
      {
         *pFunction = *pRddFunction;
         if( *pFunction == NULL && *pUsrFunction && uiCount <= uiSize &&
             zh_usrIsMethod( pMethods, uiCount ) )
         {
            *pFunction = *pUsrFunction;
         }
         ++pUsrFunction;
         ++pRddFunction;
         ++pFunction;
      }
      uiResult = zh_rddInheritEx( pSelfTable, &funcTable.funcTable, pSuperTable, szSuperRDD, puiSuperRddId );
      if( uiResult == ZH_SUCCESS )
         pSelfTable->whoCares = ( DBENTRYP_SVP ) zh_itemNew( pMethods );

      zh_retni( uiResult );
   }
   else
      zh_retni( ZH_FAILURE );
}

ZH_FUNC( USRRDD_RDDDATA )
{
   ZH_USHORT uiRddID = ( ZH_USHORT ) zh_parni( 1 );

   if( uiRddID < s_uiUsrNodes && s_pUsrRddNodes[ uiRddID ] )
   {
      PZH_ITEM pItem = s_pUsrRddNodes[ uiRddID ]->pItem;

      zh_itemReturn( pItem );
      if( zh_pcount() >= 2 )
         zh_itemCopy( pItem, zh_param( 2, ZH_IT_ANY ) );
   }
}

ZH_FUNC( USRRDD_ID )
{
   if( ZH_ISCHAR( 1 ) )
   {
      ZH_USHORT uiRddId;
      LPRDDNODE pRddNode = zh_rddFindNode( zh_parc( 1 ), &uiRddId );

      if( pRddNode && uiRddId < s_uiUsrNodes && s_pUsrRddNodes[ uiRddId ] )
         zh_retni( uiRddId );
   }
   else
   {
      AREAP pArea;

      if( ZH_IS_PARAM_NUM( 1 ) )
         pArea = zh_usrGetAreaPointer( zh_parni( 1 ) );
      else
         pArea = ( AREAP ) zh_parptr( 1 );

      if( pArea && pArea->rddID < s_uiUsrNodes && SELF_USRNODE( pArea ) )
         zh_retni( pArea->rddID );
   }
}

ZH_FUNC( USRRDD_AREADATA )
{
   AREAP pArea;

   if( ZH_IS_PARAM_NUM( 1 ) )
      pArea = zh_usrGetAreaPointer( zh_parni( 1 ) );
   else
      pArea = ( AREAP ) zh_parptr( 1 );

   if( pArea && pArea->rddID < s_uiUsrNodes && SELF_USRNODE( pArea ) )
   {
      PZH_ITEM pItem = SELF_USRDATA( pArea )->pItem;

      zh_itemReturn( pItem );
      if( zh_pcount() >= 2 )
         zh_itemCopy( pItem, zh_param( 2, ZH_IT_ANY ) );
   }
}

ZH_FUNC( USRRDD_AREARESULT )
{
   AREAP pArea;

   if( ZH_IS_PARAM_NUM( 1 ) )
      pArea = zh_usrGetAreaPointer( zh_parni( 1 ) );
   else
      pArea = ( AREAP ) zh_parptr( 1 );

   if( pArea && pArea->rddID < s_uiUsrNodes && SELF_USRNODE( pArea ) )
   {
      if( ! pArea->valResult )
         pArea->valResult = zh_itemNew( NULL );

      zh_itemReturn( pArea->valResult );
      if( zh_pcount() >= 2 )
         zh_itemCopy( pArea->valResult, zh_param( 2, ZH_IT_ANY ) );
   }
}

ZH_FUNC( USRRDD_SETBOF )
{
   if( ZH_ISLOG( 2 ) )
   {
      AREAP pArea;

      if( ZH_IS_PARAM_NUM( 1 ) )
         pArea = zh_usrGetAreaPointer( zh_parni( 1 ) );
      else
         pArea = ( AREAP ) zh_parptr( 1 );

      if( pArea && pArea->rddID < s_uiUsrNodes && SELF_USRNODE( pArea ) )
         pArea->fBof = zh_parl( 2 );
   }
}

ZH_FUNC( USRRDD_SETEOF )
{
   if( ZH_ISLOG( 2 ) )
   {
      AREAP pArea;

      if( ZH_IS_PARAM_NUM( 1 ) )
         pArea = zh_usrGetAreaPointer( zh_parni( 1 ) );
      else
         pArea = ( AREAP ) zh_parptr( 1 );

      if( pArea && pArea->rddID < s_uiUsrNodes && SELF_USRNODE( pArea ) )
         pArea->fEof = zh_parl( 2 );
   }
}

ZH_FUNC( USRRDD_SETFOUND )
{
   if( ZH_ISLOG( 2 ) )
   {
      AREAP pArea;

      if( ZH_IS_PARAM_NUM( 1 ) )
         pArea = zh_usrGetAreaPointer( zh_parni( 1 ) );
      else
         pArea = ( AREAP ) zh_parptr( 1 );

      if( pArea && pArea->rddID < s_uiUsrNodes && SELF_USRNODE( pArea ) )
         pArea->fFound = zh_parl( 2 );
   }
}

ZH_FUNC( USRRDD_SETTOP )
{
   if( ZH_ISLOG( 2 ) )
   {
      AREAP pArea;

      if( ZH_IS_PARAM_NUM( 1 ) )
         pArea = zh_usrGetAreaPointer( zh_parni( 1 ) );
      else
         pArea = ( AREAP ) zh_parptr( 1 );

      if( pArea && pArea->rddID < s_uiUsrNodes && SELF_USRNODE( pArea ) )
         pArea->fTop = zh_parl( 2 );
   }
}

ZH_FUNC( USRRDD_SETBOTTOM )
{
   if( ZH_ISLOG( 2 ) )
   {
      AREAP pArea;

      if( ZH_IS_PARAM_NUM( 1 ) )
         pArea = zh_usrGetAreaPointer( zh_parni( 1 ) );
      else
         pArea = ( AREAP ) zh_parptr( 1 );

      if( pArea && pArea->rddID < s_uiUsrNodes && SELF_USRNODE( pArea ) )
         pArea->fBottom = zh_parl( 2 );
   }
}

static ZH_ERRCODE zh_usrErrorRT( AREAP pArea, ZH_ERRCODE errGenCode, ZH_ERRCODE errSubCode )
{
   ZH_ERRCODE iRet = ZH_FAILURE;

   if( zh_vmRequestQuery() == 0 )
   {
      PZH_ITEM pError = zh_errNew();
      zh_errPutGenCode( pError, errGenCode );
      zh_errPutSubCode( pError, errSubCode );
      zh_errPutDescription( pError, zh_langDGetErrorDesc( errGenCode ) );
      if( pArea )
         iRet = SELF_ERROR( pArea, pError );
      else
      {
         zh_errPutSeverity( pError, ES_ERROR );
         zh_errPutSubSystem( pError, "???DRIVER" );
         zh_errPutOperation( pError, ZH_ERR_FUNCNAME );
         iRet = zh_errLaunch( pError );
      }
      zh_errRelease( pError );
   }
   return iRet;
}


static AREAP zh_usrGetAreaParam( int iParams )
{
   AREAP pArea = NULL;

   if( iParams <= zh_pcount() )
   {
      if( ZH_IS_PARAM_NUM( 1 ) )
         pArea = zh_usrGetAreaPointer( zh_parni( 1 ) );
      else
         pArea = ( AREAP ) zh_parptr( 1 );

      if( pArea && pArea->rddID < s_uiUsrNodes && SELF_USRNODE( pArea ) )
         return pArea;
   }

   if( pArea )
      zh_usrErrorRT( pArea, EG_UNSUPPORTED, 0 );
   else if( zh_pcount() > 0 )
      zh_usrErrorRT( pArea, EG_NOTABLE, EDBCMD_NOTABLE );
   else
      zh_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );

   zh_retni( ZH_FAILURE );

   return NULL;
}

static LPRDDNODE zh_usrGetNodeParam( int iParams )
{
   LPRDDNODE pRDD = NULL;
   ZH_USHORT uiNode = 0;

   if( iParams <= zh_pcount() )
   {
      uiNode = ( ZH_USHORT ) zh_parni( 1 );
      pRDD = zh_rddGetNode( uiNode );
      if( pRDD && uiNode < s_uiUsrNodes && s_pUsrRddNodes[ uiNode ] )
         return pRDD;
   }

   if( pRDD )
      zh_usrErrorRT( NULL, EG_UNSUPPORTED, 0 );
   else if( uiNode )
      zh_usrErrorRT( NULL, EG_NOTABLE, EDBCMD_NOTABLE );
   else
      zh_usrErrorRT( NULL, EG_ARG, EDBCMD_NOVAR );

   zh_retni( ZH_FAILURE );

   return NULL;
}


#define ZH_FUNC_UR_SUPER( x )  ZH_FUNC( UR_SUPER_##x )


ZH_FUNC_UR_SUPER( BOF )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
   {
      ZH_BOOL fBof;

      zh_retni( SUPER_BOF( pArea, &fBof ) );
      zh_storl( fBof, 2 );
   }
}

ZH_FUNC_UR_SUPER( EOF )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
   {
      ZH_BOOL fEof;

      zh_retni( SUPER_EOF( pArea, &fEof ) );
      zh_storl( fEof, 2 );
   }
}

ZH_FUNC_UR_SUPER( FOUND )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
   {
      ZH_BOOL fFound;

      zh_retni( SUPER_FOUND( pArea, &fFound ) );
      zh_storl( fFound, 2 );
   }
}

ZH_FUNC_UR_SUPER( GOBOTTOM )
{
   AREAP pArea = zh_usrGetAreaParam( 1 );

   if( pArea )
      zh_retni( SUPER_GOBOTTOM( pArea ) );
}

ZH_FUNC_UR_SUPER( GOTOP )
{
   AREAP pArea = zh_usrGetAreaParam( 1 );

   if( pArea )
      zh_retni( SUPER_GOTOP( pArea ) );
}

ZH_FUNC_UR_SUPER( GOTO )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
      zh_retni( SUPER_GOTO( pArea, zh_parnl( 2 ) ) );
}

ZH_FUNC_UR_SUPER( GOTOID )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
      zh_retni( SUPER_GOTOID( pArea, zh_param( 2, ZH_IT_ANY ) ) );
}

ZH_FUNC_UR_SUPER( SEEK )
{
   AREAP pArea = zh_usrGetAreaParam( 4 );

   if( pArea )
      zh_retni( SUPER_SEEK( pArea, zh_parl( 2 ), zh_param( 3, ZH_IT_ANY ),
                                   zh_parl( 4 ) ) );
}

ZH_FUNC_UR_SUPER( SKIP )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
      zh_retni( SUPER_SKIP( pArea, zh_parnl( 2 ) ) );
}

ZH_FUNC_UR_SUPER( SKIPFILTER )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
      zh_retni( SUPER_SKIPFILTER( pArea, zh_parnl( 2 ) ) );
}

ZH_FUNC_UR_SUPER( SKIPRAW )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
      zh_retni( SUPER_SKIPRAW( pArea, zh_parnl( 2 ) ) );
}

ZH_FUNC_UR_SUPER( DELETED )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
   {
      ZH_BOOL fDeleted;

      zh_retni( SUPER_DELETED( pArea, &fDeleted ) );
      zh_storl( fDeleted, 2 );
   }
}

ZH_FUNC_UR_SUPER( ADDFIELD )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBFIELDINFO dbFieldInfo;

      if( zh_usrItemToFieldInfo( zh_param( 2, ZH_IT_ARRAY ), &dbFieldInfo ) )
      {
         zh_retni( SUPER_ADDFIELD( pArea, &dbFieldInfo ) );
      }
      else
      {
         zh_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         zh_retni( ZH_FAILURE );
      }
   }
}

ZH_FUNC_UR_SUPER( FIELDDISPLAY )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBFIELDINFO dbFieldInfo;

      if( zh_usrItemToFieldInfo( zh_param( 2, ZH_IT_ARRAY ), &dbFieldInfo ) )
      {
         zh_retni( SUPER_FIELDDISPLAY( pArea, &dbFieldInfo ) );
      }
      else
      {
         zh_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         zh_retni( ZH_FAILURE );
      }
   }
}

ZH_FUNC_UR_SUPER( FIELDNAME )
{
   AREAP pArea = zh_usrGetAreaParam( 3 );

   if( pArea )
   {
      char * szName = ( char * ) zh_xgrab( pArea->uiMaxFieldNameLength + 1 );

      zh_retni( SUPER_FIELDNAME( pArea, ( ZH_USHORT ) zh_parni( 2 ),
                                        szName ) );
      zh_storc( szName, 3 );
      zh_xfree( szName );
   }
}

ZH_FUNC_UR_SUPER( APPEND )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
      zh_retni( SUPER_APPEND( pArea, zh_parl( 2 ) ) );
}

ZH_FUNC_UR_SUPER( DELETE )
{
   AREAP pArea = zh_usrGetAreaParam( 1 );

   if( pArea )
      zh_retni( SUPER_DELETE( pArea ) );
}

ZH_FUNC_UR_SUPER( RECALL )
{
   AREAP pArea = zh_usrGetAreaParam( 1 );

   if( pArea )
      zh_retni( SUPER_RECALL( pArea ) );
}

ZH_FUNC_UR_SUPER( FIELDCOUNT )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
   {
      ZH_USHORT uiCount;

      zh_retni( SUPER_FIELDCOUNT( pArea, &uiCount ) );
      zh_storni( uiCount, 2 );
   }
}

ZH_FUNC_UR_SUPER( FLUSH )
{
   AREAP pArea = zh_usrGetAreaParam( 1 );

   if( pArea )
      zh_retni( SUPER_FLUSH( pArea ) );
}

ZH_FUNC_UR_SUPER( GOCOLD )
{
   AREAP pArea = zh_usrGetAreaParam( 1 );

   if( pArea )
      zh_retni( SUPER_GOCOLD( pArea ) );
}

ZH_FUNC_UR_SUPER( GOHOT )
{
   AREAP pArea = zh_usrGetAreaParam( 1 );

   if( pArea )
      zh_retni( SUPER_GOHOT( pArea ) );
}

ZH_FUNC_UR_SUPER( PUTREC )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
   {
      if( ZH_ISPOINTER( 2 ) )
      {
         zh_retni( SUPER_PUTREC( pArea, ( const ZH_BYTE * ) zh_parptr( 2 ) ) );
      }
      else if( ZH_ISCHAR( 2 ) )
      {
         zh_retni( SUPER_PUTREC( pArea, ( const ZH_BYTE * ) zh_parc( 2 ) ) );
      }
      else
      {
         zh_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         zh_retni( ZH_FAILURE );
      }
   }
}

ZH_FUNC_UR_SUPER( GETREC )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
   {
      ZH_BYTE * pBuffer;

      zh_retni( SUPER_GETREC( pArea, &pBuffer ) );
      zh_storptr( pBuffer, 2 );
   }
}

ZH_FUNC_UR_SUPER( GETVALUE )
{
   AREAP pArea = zh_usrGetAreaParam( 3 );

   if( pArea )
      zh_retni( SUPER_GETVALUE( pArea, ( ZH_USHORT ) zh_parni( 2 ),
                                       zh_param( 3, ZH_IT_ANY ) ) );
}

ZH_FUNC_UR_SUPER( PUTVALUE )
{
   AREAP pArea = zh_usrGetAreaParam( 3 );

   if( pArea )
      zh_retni( SUPER_PUTVALUE( pArea, ( ZH_USHORT ) zh_parni( 2 ),
                                       zh_param( 3, ZH_IT_ANY ) ) );
}

ZH_FUNC_UR_SUPER( GETVARLEN )
{
   AREAP pArea = zh_usrGetAreaParam( 3 );

   if( pArea )
   {
      ZH_ULONG ulLength;

      zh_retni( SUPER_GETVARLEN( pArea, ( ZH_USHORT ) zh_parni( 2 ),
                                        &ulLength ) );
      zh_stornl( ulLength, 3 );
   }
}

ZH_FUNC_UR_SUPER( RECCOUNT )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
   {
      ZH_ULONG ulRecCount;

      zh_retni( SUPER_RECCOUNT( pArea, &ulRecCount ) );
      zh_stornl( ulRecCount, 2 );
   }
}

ZH_FUNC_UR_SUPER( RECINFO )
{
   AREAP pArea = zh_usrGetAreaParam( 4 );

   if( pArea )
      zh_retni( SUPER_RECINFO( pArea, zh_param( 2, ZH_IT_ANY ),
                                      ( ZH_USHORT ) zh_parni( 3 ),
                                      zh_param( 4, ZH_IT_ANY ) ) );
}

ZH_FUNC_UR_SUPER( RECNO )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
   {
      ZH_ULONG ulRecNo;

      zh_retni( SUPER_RECNO( pArea, &ulRecNo ) );
      zh_stornl( ulRecNo, 2 );
   }
}

ZH_FUNC_UR_SUPER( RECID )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
      zh_retni( SUPER_RECID( pArea, zh_param( 2, ZH_IT_ANY ) ) );
}

ZH_FUNC_UR_SUPER( FIELDINFO )
{
   AREAP pArea = zh_usrGetAreaParam( 4 );

   if( pArea )
      zh_retni( SUPER_FIELDINFO( pArea, ( ZH_USHORT ) zh_parni( 2 ),
                                        ( ZH_USHORT ) zh_parni( 3 ),
                                        zh_param( 4, ZH_IT_ANY ) ) );
}

ZH_FUNC_UR_SUPER( CREATEFIELDS )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
      zh_retni( SUPER_CREATEFIELDS( pArea, zh_param( 2, ZH_IT_ANY ) ) );
}

ZH_FUNC_UR_SUPER( SETFIELDEXTENT )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
      zh_retni( SUPER_SETFIELDEXTENT( pArea, ( ZH_USHORT ) zh_parni( 2 ) ) );
}

ZH_FUNC_UR_SUPER( ALIAS )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
   {
      char szAlias[ ZH_RDD_MAX_ALIAS_LEN + 1 ];

      zh_retni( SUPER_ALIAS( pArea, szAlias ) );
      zh_storc( szAlias, 2 );
   }
}

ZH_FUNC_UR_SUPER( CLOSE )
{
   AREAP pArea = zh_usrGetAreaParam( 1 );

   if( pArea )
      zh_retni( SUPER_CLOSE( pArea ) );
}

ZH_FUNC_UR_SUPER( CREATE )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBOPENINFO dbOpenInfo;

      if( zh_usrItemToOpenInfo( zh_param( 2, ZH_IT_ARRAY ), &dbOpenInfo ) )
      {
         zh_retni( SUPER_CREATE( pArea, &dbOpenInfo ) );
      }
      else
      {
         zh_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         zh_retni( ZH_FAILURE );
      }
   }
}

ZH_FUNC_UR_SUPER( OPEN )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBOPENINFO dbOpenInfo;

      if( zh_usrItemToOpenInfo( zh_param( 2, ZH_IT_ARRAY ), &dbOpenInfo ) )
      {
         zh_retni( SUPER_OPEN( pArea, &dbOpenInfo ) );
      }
      else
      {
         zh_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         zh_retni( ZH_FAILURE );
      }
   }
}

ZH_FUNC_UR_SUPER( INFO )
{
   AREAP pArea = zh_usrGetAreaParam( 3 );

   if( pArea )
      zh_retni( SUPER_INFO( pArea, ( ZH_USHORT ) zh_parni( 2 ),
                                   zh_param( 3, ZH_IT_ANY ) ) );
}

ZH_FUNC_UR_SUPER( DBEVAL )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBEVALINFO dbEvalInfo;

      if( zh_usrItemToEvalInfo( zh_param( 2, ZH_IT_ARRAY ), &dbEvalInfo ) )
      {
         zh_retni( SUPER_DBEVAL( pArea, &dbEvalInfo ) );
      }
      else
      {
         zh_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         zh_retni( ZH_FAILURE );
      }
   }
}

ZH_FUNC_UR_SUPER( PACK )
{
   AREAP pArea = zh_usrGetAreaParam( 1 );

   if( pArea )
      zh_retni( SUPER_PACK( pArea ) );
}

ZH_FUNC_UR_SUPER( PACKREC )
{
   AREAP pArea = zh_usrGetAreaParam( 3 );

   if( pArea )
   {
      ZH_BOOL fWritten;

      zh_retni( SUPER_PACKREC( pArea, zh_parnl( 2 ), &fWritten ) );
      zh_storl( fWritten, 3 );
   }
}

ZH_FUNC_UR_SUPER( SORT )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBSORTINFO dbSortInfo;

      if( zh_usrItemToSortInfo( zh_param( 2, ZH_IT_ARRAY ), &dbSortInfo ) )
      {
         zh_retni( SUPER_SORT( pArea, &dbSortInfo ) );
         zh_usrSortInfoFree( &dbSortInfo );
      }
      else
      {
         zh_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         zh_retni( ZH_FAILURE );
      }
   }
}

ZH_FUNC_UR_SUPER( TRANS )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBTRANSINFO dbTransInfo;

      if( zh_usrItemToTransInfo( zh_param( 2, ZH_IT_ARRAY ), &dbTransInfo ) )
      {
         zh_retni( SUPER_TRANS( pArea, &dbTransInfo ) );
         zh_usrTransInfoFree( &dbTransInfo );
      }
      else
      {
         zh_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         zh_retni( ZH_FAILURE );
      }
   }
}

ZH_FUNC_UR_SUPER( TRANSREC )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBTRANSINFO dbTransInfo;

      if( zh_usrItemToTransInfo( zh_param( 2, ZH_IT_ARRAY ), &dbTransInfo ) )
      {
         zh_retni( SUPER_TRANSREC( pArea, &dbTransInfo ) );
         zh_usrTransInfoFree( &dbTransInfo );
      }
      else
      {
         zh_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         zh_retni( ZH_FAILURE );
      }
   }
}

ZH_FUNC_UR_SUPER( ZAP )
{
   AREAP pArea = zh_usrGetAreaParam( 1 );

   if( pArea )
      zh_retni( SUPER_ZAP( pArea ) );
}

ZH_FUNC_UR_SUPER( CHILDEND )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBRELINFO dbRelInfo;

      if( zh_usrItemToRelInfo( zh_param( 2, ZH_IT_ARRAY ), &dbRelInfo ) )
      {
         zh_retni( SUPER_CHILDEND( pArea, &dbRelInfo ) );
      }
      else
      {
         zh_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         zh_retni( ZH_FAILURE );
      }
   }
}

ZH_FUNC_UR_SUPER( CHILDSTART )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBRELINFO dbRelInfo;

      if( zh_usrItemToRelInfo( zh_param( 2, ZH_IT_ARRAY ), &dbRelInfo ) )
      {
         zh_retni( SUPER_CHILDSTART( pArea, &dbRelInfo ) );
      }
      else
      {
         zh_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         zh_retni( ZH_FAILURE );
      }
   }
}

ZH_FUNC_UR_SUPER( CHILDSYNC )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBRELINFO dbRelInfo;

      if( zh_usrItemToRelInfo( zh_param( 2, ZH_IT_ARRAY ), &dbRelInfo ) )
      {
         zh_retni( SUPER_CHILDSYNC( pArea, &dbRelInfo ) );
      }
      else
      {
         zh_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         zh_retni( ZH_FAILURE );
      }
   }
}

ZH_FUNC_UR_SUPER( SYNCCHILDREN )
{
   AREAP pArea = zh_usrGetAreaParam( 1 );

   if( pArea )
      zh_retni( SUPER_SYNCCHILDREN( pArea ) );
}

ZH_FUNC_UR_SUPER( CLEARREL )
{
   AREAP pArea = zh_usrGetAreaParam( 1 );

   if( pArea )
      zh_retni( SUPER_CLEARREL( pArea ) );
}

ZH_FUNC_UR_SUPER( FORCEREL )
{
   AREAP pArea = zh_usrGetAreaParam( 1 );

   if( pArea )
      zh_retni( SUPER_FORCEREL( pArea ) );
}

ZH_FUNC_UR_SUPER( RELAREA )
{
   AREAP pArea = zh_usrGetAreaParam( 3 );

   if( pArea )
   {
      ZH_USHORT uiRelArea;

      zh_retni( SUPER_RELAREA( pArea, ( ZH_USHORT ) zh_parni( 2 ),
                                      &uiRelArea ) );
      zh_storni( uiRelArea, 3 );
   }
}

ZH_FUNC_UR_SUPER( RELEVAL )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBRELINFO dbRelInfo;

      if( zh_usrItemToRelInfo( zh_param( 2, ZH_IT_ARRAY ), &dbRelInfo ) )
      {
         zh_retni( SUPER_RELEVAL( pArea, &dbRelInfo ) );
      }
      else
      {
         zh_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         zh_retni( ZH_FAILURE );
      }
   }
}

ZH_FUNC_UR_SUPER( RELTEXT )
{
   AREAP pArea = zh_usrGetAreaParam( 3 );

   if( pArea )
      zh_retni( SUPER_RELTEXT( pArea, ( ZH_USHORT ) zh_parni( 2 ),
                                      zh_param( 3, ZH_IT_ANY ) ) );
}

ZH_FUNC_UR_SUPER( SETREL )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBRELINFO dbRelInfo;

      if( zh_usrItemToRelInfo( zh_param( 2, ZH_IT_ARRAY ), &dbRelInfo ) )
      {
         zh_retni( SUPER_SETREL( pArea, &dbRelInfo ) );
      }
      else
      {
         zh_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         zh_retni( ZH_FAILURE );
      }
   }
}

ZH_FUNC_UR_SUPER( ORDLSTADD )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBORDERINFO dbOrderInfo;
      PZH_ITEM pItem = zh_param( 2, ZH_IT_ARRAY );

      if( zh_usrItemToOrderInfo( pItem, &dbOrderInfo ) )
      {
         zh_retni( SUPER_ORDLSTADD( pArea, &dbOrderInfo ) );
         zh_arraySet( pItem, UR_ORI_RESULT, dbOrderInfo.itmResult );
      }
      else
      {
         zh_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         zh_retni( ZH_FAILURE );
      }
   }
}

ZH_FUNC_UR_SUPER( ORDLSTCLEAR )
{
   AREAP pArea = zh_usrGetAreaParam( 1 );

   if( pArea )
      zh_retni( SUPER_ORDLSTCLEAR( pArea ) );
}

ZH_FUNC_UR_SUPER( ORDLSTDELETE )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBORDERINFO dbOrderInfo;
      PZH_ITEM pItem = zh_param( 2, ZH_IT_ARRAY );

      if( zh_usrItemToOrderInfo( pItem, &dbOrderInfo ) )
      {
         zh_retni( SUPER_ORDLSTDELETE( pArea, &dbOrderInfo ) );
         zh_arraySet( pItem, UR_ORI_RESULT, dbOrderInfo.itmResult );
      }
      else
      {
         zh_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         zh_retni( ZH_FAILURE );
      }
   }
}

ZH_FUNC_UR_SUPER( ORDLSTFOCUS )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBORDERINFO dbOrderInfo;
      PZH_ITEM pItem = zh_param( 2, ZH_IT_ARRAY );

      if( zh_usrItemToOrderInfo( pItem, &dbOrderInfo ) )
      {
         zh_retni( SUPER_ORDLSTFOCUS( pArea, &dbOrderInfo ) );
         zh_arraySet( pItem, UR_ORI_RESULT, dbOrderInfo.itmResult );
      }
      else
      {
         zh_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         zh_retni( ZH_FAILURE );
      }
   }
}

ZH_FUNC_UR_SUPER( ORDLSTREBUILD )
{
   AREAP pArea = zh_usrGetAreaParam( 1 );

   if( pArea )
      zh_retni( SUPER_ORDLSTREBUILD( pArea ) );
}

ZH_FUNC_UR_SUPER( ORDSETCOND )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
   {
      PZH_ITEM pItem = zh_param( 2, ZH_IT_ANY );

      if( pItem && ZH_IS_NIL( pItem ) )
      {
         zh_retni( SUPER_ORDSETCOND( pArea, NULL ) );
      }
      else
      {
         LPDBORDERCONDINFO lpdbOrderCondInfo = ( LPDBORDERCONDINFO )
                                       zh_xgrab( sizeof( DBORDERCONDINFO ) );
         if( zh_usrItemToOrderCondInfo( pItem, lpdbOrderCondInfo ) )
         {
            zh_usrOrderCondClone( lpdbOrderCondInfo );
            zh_retni( SUPER_ORDSETCOND( pArea, lpdbOrderCondInfo ) );
         }
         else
         {
            zh_xfree( lpdbOrderCondInfo );
            zh_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
            zh_retni( ZH_FAILURE );
         }
      }
   }
}

ZH_FUNC_UR_SUPER( ORDCREATE )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBORDERCREATEINFO dbOrderCreateInfo;
      PZH_ITEM pItem = zh_param( 2, ZH_IT_ARRAY );

      if( zh_usrItemToOrderCreateInfo( pItem, &dbOrderCreateInfo ) )
      {
         zh_retni( SUPER_ORDCREATE( pArea, &dbOrderCreateInfo ) );
         zh_usrOrderCreateFree( &dbOrderCreateInfo );
      }
      else
      {
         zh_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         zh_retni( ZH_FAILURE );
      }
   }
}

ZH_FUNC_UR_SUPER( ORDDESTROY )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBORDERINFO dbOrderInfo;
      PZH_ITEM pItem = zh_param( 2, ZH_IT_ARRAY );

      if( zh_usrItemToOrderInfo( pItem, &dbOrderInfo ) )
      {
         zh_retni( SUPER_ORDDESTROY( pArea, &dbOrderInfo ) );
         zh_arraySet( pItem, UR_ORI_RESULT, dbOrderInfo.itmResult );
      }
      else
      {
         zh_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         zh_retni( ZH_FAILURE );
      }
   }
}

ZH_FUNC_UR_SUPER( ORDINFO )
{
   AREAP pArea = zh_usrGetAreaParam( 3 );

   if( pArea )
   {
      DBORDERINFO dbOrderInfo;
      PZH_ITEM pItem = zh_param( 3, ZH_IT_ARRAY );

      if( zh_usrItemToOrderInfo( pItem, &dbOrderInfo ) )
      {
         zh_retni( SUPER_ORDINFO( pArea, ( ZH_USHORT ) zh_parni( 2 ),
                                         &dbOrderInfo ) );
         zh_arraySet( pItem, UR_ORI_RESULT, dbOrderInfo.itmResult );
      }
      else
      {
         zh_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         zh_retni( ZH_FAILURE );
      }
   }
}

ZH_FUNC_UR_SUPER( CLEARFILTER )
{
   AREAP pArea = zh_usrGetAreaParam( 1 );

   if( pArea )
      zh_retni( SUPER_CLEARFILTER( pArea ) );
}

ZH_FUNC_UR_SUPER( CLEARLOCATE )
{
   AREAP pArea = zh_usrGetAreaParam( 1 );

   if( pArea )
      zh_retni( SUPER_CLEARLOCATE( pArea ) );
}

ZH_FUNC_UR_SUPER( CLEARSCOPE )
{
   AREAP pArea = zh_usrGetAreaParam( 1 );

   if( pArea )
      zh_retni( SUPER_CLEARSCOPE( pArea ) );
}

ZH_FUNC_UR_SUPER( FILTERTEXT )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
      zh_retni( SUPER_FILTERTEXT( pArea, zh_param( 2, ZH_IT_ANY ) ) );
}

ZH_FUNC_UR_SUPER( SETFILTER )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBFILTERINFO dbFilterInfo;

      if( zh_usrItemToFilterInfo( zh_param( 2, ZH_IT_ARRAY ), &dbFilterInfo ) )
      {
         zh_retni( SUPER_SETFILTER( pArea, &dbFilterInfo ) );
      }
      else
      {
         zh_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         zh_retni( ZH_FAILURE );
      }
   }
}

ZH_FUNC_UR_SUPER( SETLOCATE )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBSCOPEINFO dbScopeInfo;

      if( zh_usrItemToScopeInfo( zh_param( 2, ZH_IT_ARRAY ), &dbScopeInfo ) )
      {
         zh_retni( SUPER_SETLOCATE( pArea, &dbScopeInfo ) );
      }
      else
      {
         zh_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         zh_retni( ZH_FAILURE );
      }
   }
}

ZH_FUNC_UR_SUPER( LOCATE )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
      zh_retni( SUPER_LOCATE( pArea, zh_parl( 2 ) ) );
}

ZH_FUNC_UR_SUPER( COMPILE )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
   {
      if( ZH_ISCHAR( 2 ) )
      {
         zh_retni( SUPER_COMPILE( pArea, zh_parc( 2 ) ) );
      }
      else
      {
         zh_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         zh_retni( ZH_FAILURE );
      }
   }
}

ZH_FUNC_UR_SUPER( ERROR )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
   {
      PZH_ITEM pItem = zh_param( 2, ZH_IT_OBJECT );

      if( pItem )
      {
         pItem = zh_itemNew( pItem );
         zh_retni( SUPER_ERROR( pArea, pItem ) );
         zh_itemRelease( pItem );
      }
      else
      {
         zh_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         zh_retni( ZH_FAILURE );
      }
   }
}

ZH_FUNC_UR_SUPER( EVALBLOCK )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
   {
      PZH_ITEM pItem = zh_param( 2, ZH_IT_BLOCK );

      if( pItem )
      {
         zh_retni( SUPER_EVALBLOCK( pArea, pItem ) );
      }
      else
      {
         zh_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         zh_retni( ZH_FAILURE );
      }
   }
}

ZH_FUNC_UR_SUPER( RAWLOCK )
{
   AREAP pArea = zh_usrGetAreaParam( 3 );

   if( pArea )
      zh_retni( SUPER_RAWLOCK( pArea, ( ZH_USHORT ) zh_parni( 2 ),
                                      zh_parnl( 3 ) ) );
}

ZH_FUNC_UR_SUPER( LOCK )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBLOCKINFO dbLockInfo;
      PZH_ITEM pItem = zh_param( 2, ZH_IT_ARRAY );

      if( zh_usrItemToLockInfo( pItem, &dbLockInfo ) )
      {
         zh_retni( SUPER_LOCK( pArea, &dbLockInfo ) );
         zh_itemPutL( zh_arrayGetItemPtr( pItem, UR_LI_RESULT ),
                      dbLockInfo.fResult );
      }
      else
      {
         zh_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         zh_retni( ZH_FAILURE );
      }
   }
}

ZH_FUNC_UR_SUPER( UNLOCK )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
      zh_retni( SUPER_UNLOCK( pArea, zh_param( 2, ZH_IT_ANY ) ) );
}

ZH_FUNC_UR_SUPER( CLOSEMEMFILE )
{
   AREAP pArea = zh_usrGetAreaParam( 1 );

   if( pArea )
      zh_retni( SUPER_CLOSEMEMFILE( pArea ) );
}

ZH_FUNC_UR_SUPER( CREATEMEMFILE )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBOPENINFO dbOpenInfo;

      if( zh_usrItemToOpenInfo( zh_param( 2, ZH_IT_ARRAY ), &dbOpenInfo ) )
      {
         zh_retni( SUPER_CREATEMEMFILE( pArea, &dbOpenInfo ) );
      }
      else
      {
         zh_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         zh_retni( ZH_FAILURE );
      }
   }
}

ZH_FUNC_UR_SUPER( OPENMEMFILE )
{
   AREAP pArea = zh_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBOPENINFO dbOpenInfo;

      if( zh_usrItemToOpenInfo( zh_param( 2, ZH_IT_ARRAY ), &dbOpenInfo ) )
      {
         zh_retni( SUPER_OPENMEMFILE( pArea, &dbOpenInfo ) );
      }
      else
      {
         zh_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         zh_retni( ZH_FAILURE );
      }
   }
}

ZH_FUNC_UR_SUPER( GETVALUEFILE )
{
   AREAP pArea = zh_usrGetAreaParam( 4 );

   if( pArea )
      zh_retni( SUPER_GETVALUEFILE( pArea, ( ZH_USHORT ) zh_parni( 2 ),
                                           zh_parc( 3 ),
                                           ( ZH_USHORT ) zh_parni( 4 ) ) );
}

ZH_FUNC_UR_SUPER( PUTVALUEFILE )
{
   AREAP pArea = zh_usrGetAreaParam( 4 );

   if( pArea )
      zh_retni( SUPER_PUTVALUEFILE( pArea, ( ZH_USHORT ) zh_parni( 2 ),
                                           zh_parc( 3 ),
                                           ( ZH_USHORT ) zh_parni( 4 ) ) );
}

ZH_FUNC_UR_SUPER( READDBHEADER )
{
   AREAP pArea = zh_usrGetAreaParam( 1 );

   if( pArea )
      zh_retni( SUPER_READDBHEADER( pArea ) );
}

ZH_FUNC_UR_SUPER( WRITEDBHEADER )
{
   AREAP pArea = zh_usrGetAreaParam( 1 );

   if( pArea )
      zh_retni( SUPER_WRITEDBHEADER( pArea ) );
}

ZH_FUNC_UR_SUPER( DROP )
{
   LPRDDNODE pRDD = zh_usrGetNodeParam( 2 );

   if( pRDD )
      zh_retni( SUPER_DROP( pRDD, zh_param( 2, ZH_IT_ANY ),
                                  zh_param( 3, ZH_IT_ANY ),
                                  zh_parnl( 4 ) ) );
}

ZH_FUNC_UR_SUPER( EXISTS )
{
   LPRDDNODE pRDD = zh_usrGetNodeParam( 2 );

   if( pRDD )
      zh_retni( SUPER_EXISTS( pRDD, zh_param( 2, ZH_IT_ANY ),
                                    zh_param( 3, ZH_IT_ANY ),
                                    zh_parnl( 4 ) ) );
}

ZH_FUNC_UR_SUPER( RENAME )
{
   LPRDDNODE pRDD = zh_usrGetNodeParam( 2 );

   if( pRDD )
      zh_retni( SUPER_RENAME( pRDD, zh_param( 2, ZH_IT_ANY ),
                                    zh_param( 3, ZH_IT_ANY ),
                                    zh_param( 4, ZH_IT_ANY ),
                                    zh_parnl( 5 ) ) );
}

ZH_FUNC_UR_SUPER( RDDINFO )
{
   LPRDDNODE pRDD = zh_usrGetNodeParam( 4 );

   if( pRDD )
      zh_retni( SUPER_RDDINFO( pRDD, ( ZH_USHORT ) zh_parni( 2 ),
                                     zh_parnl( 3 ),
                                     zh_param( 4, ZH_IT_ANY ) ) );
}
