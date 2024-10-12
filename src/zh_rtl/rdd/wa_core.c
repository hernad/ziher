/*
 * Default RDD module
 *
 * Copyright 1999 Bruno Cantero <bruno@issnet.net>
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
#include "zh_rdd_api.h"
#include "zh_item_api.h"
#include "zh_error_api.h"
#include "zh_vm.h"
#include "zh_stack.h"
#include "zh_thread.h"


#define ZH_SET_WA( n )  \
   do \
   { \
      pRddInfo->uiCurrArea = n; \
      pRddInfo->pCurrArea  = ( ( pRddInfo->uiCurrArea < pRddInfo->uiWaNumMax ) ? \
                               pRddInfo->waList[ pRddInfo->waNums[ pRddInfo->uiCurrArea ] ] : \
                               NULL ); \
   } while( 0 )


/*
 * Insert new WorkArea node at current WA position
 */
static void zh_waNodeInsert( PZH_STACKRDD pRddInfo, AREAP pArea )
{
   ZH_USHORT uiWaPos;

   if( pRddInfo->uiCurrArea >= pRddInfo->uiWaNumMax )
   {
      int iSize = ( ( ( int ) pRddInfo->uiCurrArea + 256 ) >> 8 ) << 8;

      if( iSize > ZH_RDD_MAX_AREA_NUM )
         iSize = ZH_RDD_MAX_AREA_NUM;

      if( pRddInfo->uiWaNumMax == 0 )
         pRddInfo->waNums = ( ZH_USHORT * ) zh_xgrab( iSize * sizeof( ZH_USHORT ) );
      else
         pRddInfo->waNums = ( ZH_USHORT * ) zh_xrealloc( pRddInfo->waNums, iSize * sizeof( ZH_USHORT ) );

      memset( &pRddInfo->waNums[ pRddInfo->uiWaNumMax ], 0, ( iSize - pRddInfo->uiWaNumMax ) * sizeof( ZH_USHORT ) );
      pRddInfo->uiWaNumMax = ( ZH_USHORT ) iSize;
   }

   if( pRddInfo->uiWaSpace == 0 )
   {
      pRddInfo->uiWaSpace = 256;
      pRddInfo->waList = ( void ** ) zh_xgrabz( pRddInfo->uiWaSpace * sizeof( void * ) );
      uiWaPos = 1;
      pRddInfo->uiWaMax = 2;
   }
   else
   {
      uiWaPos = pRddInfo->uiWaMax++;
      if( pRddInfo->uiWaMax > pRddInfo->uiWaSpace )
      {
         int iSize = ( ( ( int ) pRddInfo->uiWaMax + 256 ) >> 8 ) << 8;

         if( iSize > ZH_RDD_MAX_AREA_NUM )
            iSize = ZH_RDD_MAX_AREA_NUM;

         pRddInfo->uiWaSpace = ( ZH_USHORT ) iSize;
         pRddInfo->waList = ( void ** ) zh_xrealloc( pRddInfo->waList, pRddInfo->uiWaSpace * sizeof( void * ) );
         memset( &pRddInfo->waList[ pRddInfo->uiWaMax ], 0, ( pRddInfo->uiWaSpace - pRddInfo->uiWaMax ) * sizeof( void * ) );
      }
      while( uiWaPos > 1 )
      {
         if( ( ( AREAP ) pRddInfo->waList[ uiWaPos - 1 ] )->uiArea < pRddInfo->uiCurrArea )
            break;
         pRddInfo->waList[ uiWaPos ] = pRddInfo->waList[ uiWaPos - 1 ];
         pRddInfo->waNums[ ( ( AREAP ) pRddInfo->waList[ uiWaPos ] )->uiArea ] = uiWaPos;
         uiWaPos--;
      }
   }
   pRddInfo->waNums[ pRddInfo->uiCurrArea ] = uiWaPos;
   pRddInfo->pCurrArea = pRddInfo->waList[ uiWaPos ] = pArea;
   pArea->uiArea = pRddInfo->uiCurrArea;
}

/*
 * Remove current WorkArea node
 */
static void zh_waNodeDelete( PZH_STACKRDD pRddInfo )
{
   ZH_USHORT uiWaPos;

   uiWaPos = pRddInfo->waNums[ pRddInfo->uiCurrArea ];
   pRddInfo->waNums[ pRddInfo->uiCurrArea ] = 0;
   pRddInfo->uiWaMax--;
   if( pRddInfo->uiWaMax <= 1 )
   {
      pRddInfo->uiWaSpace = pRddInfo->uiWaMax = pRddInfo->uiWaNumMax = 0;
      zh_xfree( pRddInfo->waList );
      zh_xfree( pRddInfo->waNums );
      pRddInfo->waList = NULL;
      pRddInfo->waNums = NULL;
   }
   else
   {
      while( uiWaPos < pRddInfo->uiWaMax )
      {
         pRddInfo->waList[ uiWaPos ] = pRddInfo->waList[ uiWaPos + 1 ];
         pRddInfo->waNums[ ( ( AREAP ) pRddInfo->waList[ uiWaPos ] )->uiArea ] = uiWaPos;
         uiWaPos++;
      }
      pRddInfo->waList[ pRddInfo->uiWaMax ] = NULL;
      if( pRddInfo->uiWaSpace - pRddInfo->uiWaMax > 256 )
      {
         int iSize = ( ( ( int ) pRddInfo->uiWaMax + 256 ) >> 8 ) << 8;

         if( iSize > ZH_RDD_MAX_AREA_NUM )
            iSize = ZH_RDD_MAX_AREA_NUM;

         pRddInfo->uiWaSpace = ( ZH_USHORT ) iSize;
         pRddInfo->waList = ( void ** ) zh_xrealloc( pRddInfo->waList, pRddInfo->uiWaSpace * sizeof( void * ) );
      }
   }
   pRddInfo->pCurrArea = NULL;
}

/*
 * Return the next free WorkArea for later use.
 */
ZH_ERRCODE zh_rddSelectFirstAvailable( void )
{
   PZH_STACKRDD pRddInfo;
   ZH_USHORT uiArea;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_rddSelectFirstAvailable()" ) );

   pRddInfo = zh_stackRDD();

   uiArea = 1;
   while( uiArea < pRddInfo->uiWaNumMax )
   {
      if( pRddInfo->waNums[ uiArea ] == 0 )
         break;
      uiArea++;
   }
   if( uiArea >= ZH_RDD_MAX_AREA_NUM )
      return ZH_FAILURE;
   ZH_SET_WA( uiArea );
   return ZH_SUCCESS;
}

/*
 * Create and insert the new WorkArea node
 */
ZH_USHORT zh_rddInsertAreaNode( const char * szDriver )
{
   PZH_STACKRDD pRddInfo;
   LPRDDNODE pRddNode;
   ZH_USHORT uiRddID;
   AREAP pArea;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_rddInsertAreaNode(%s)", szDriver ) );

   pRddInfo = zh_stackRDD();
   if( pRddInfo->uiCurrArea && pRddInfo->pCurrArea )
      return 0;

   pRddNode = zh_rddFindNode( szDriver, &uiRddID );
   if( ! pRddNode )
      return 0;

   if( pRddInfo->uiCurrArea == 0 )
   {
      if( zh_rddSelectFirstAvailable() != ZH_SUCCESS )
         return 0;
   }

   pArea = ( AREAP ) zh_rddNewAreaNode( pRddNode, uiRddID );
   if( ! pArea )
      return 0;

   zh_waNodeInsert( pRddInfo, pArea );

   return pRddInfo->uiCurrArea;
}

/*
 * Closes and releases the current WorkArea preparing it
 * to be used with a new database.
 */
void zh_rddReleaseCurrentArea( void )
{
   PZH_STACKRDD pRddInfo;
   AREAP pArea;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_rddReleaseCurrentArea()" ) );

   pRddInfo = zh_stackRDD();
   pArea = ( AREAP ) pRddInfo->pCurrArea;
   if( ! pArea )
      return;

   if( SELF_CLOSE( pArea ) == ZH_FAILURE )
      return;

   SELF_RELEASE( pArea );

   zh_waNodeDelete( pRddInfo );
}

/*
 * Closes all WorkAreas.
 */
void zh_rddCloseAll( void )
{
   PZH_STACKRDD pRddInfo;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_rddCloseAll()" ) );

   pRddInfo = zh_stackRDD();
   if( pRddInfo->uiWaMax > 0 )
   {
      ZH_BOOL isParents, isFinish = ZH_FALSE;
      AREAP pArea;
      ZH_USHORT uiIndex;

      do
      {
         isParents = ZH_FALSE;
         for( uiIndex = 1; uiIndex < pRddInfo->uiWaMax; uiIndex++ )
         {
            pArea = ( AREAP ) pRddInfo->waList[ uiIndex ];
            ZH_SET_WA( pArea->uiArea );
            if( isFinish )
            {
               SELF_RELEASE( pArea );
               pRddInfo->waNums[ pRddInfo->uiCurrArea ] = 0;
               pRddInfo->pCurrArea = NULL;
            }
            else if( pArea->uiParents )
            {
               isParents = ZH_TRUE;
            }
            else
            {
               SELF_CLOSE( pArea );
            }
         }
         if( ! isParents && ! isFinish )
         {
            isParents = isFinish = ZH_TRUE;
         }
      }
      while( isParents );

      pRddInfo->uiWaSpace = pRddInfo->uiWaMax = pRddInfo->uiWaNumMax = 0;
      zh_xfree( pRddInfo->waList );
      zh_xfree( pRddInfo->waNums );
      pRddInfo->waList = NULL;
      pRddInfo->waNums = NULL;
      ZH_SET_WA( 1 );
   }
}

void zh_rddFlushAll( void )
{
   PZH_STACKRDD pRddInfo = zh_stackRDD();
   ZH_USHORT uiArea = ( ZH_AREANO ) zh_rddGetCurrentWorkAreaNumber(), uiIndex;

   for( uiIndex = 1; uiIndex < pRddInfo->uiWaMax; ++uiIndex )
   {
      zh_rddSelectWorkAreaNumber( ( ( AREAP ) pRddInfo->waList[ uiIndex ] )->uiArea );
      SELF_FLUSH( ( AREAP ) pRddInfo->pCurrArea );
   }
   zh_rddSelectWorkAreaNumber( uiArea );
}

void zh_rddUnLockAll( void )
{
   PZH_STACKRDD pRddInfo = zh_stackRDD();
   ZH_USHORT uiArea = ( ZH_AREANO ) zh_rddGetCurrentWorkAreaNumber(), uiIndex;

   for( uiIndex = 1; uiIndex < pRddInfo->uiWaMax; ++uiIndex )
   {
      zh_rddSelectWorkAreaNumber( ( ( AREAP ) pRddInfo->waList[ uiIndex ] )->uiArea );
      SELF_UNLOCK( ( AREAP ) pRddInfo->pCurrArea, NULL );
   }
   zh_rddSelectWorkAreaNumber( uiArea );
}

/*
 * call a pCallBack function with all open workareas ###
 */
ZH_ERRCODE zh_rddIterateWorkAreas( WACALLBACK pCallBack, void * cargo )
{
   PZH_STACKRDD pRddInfo;
   ZH_ERRCODE errCode = ZH_SUCCESS;
   ZH_USHORT uiIndex;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_rddIterateWorkAreas(%p, %p)", ( void * ) pCallBack, cargo ) );

   pRddInfo = zh_stackRDD();
   for( uiIndex = 1; uiIndex < pRddInfo->uiWaMax; uiIndex++ )
   {
      AREAP pArea = ( AREAP ) pRddInfo->waList[ uiIndex ];
      errCode = pCallBack( pArea, cargo );
      if( errCode != ZH_SUCCESS )
         break;
      if( uiIndex >= pRddInfo->uiWaMax ||
          pArea != ( AREAP ) pRddInfo->waList[ uiIndex ] )
         uiIndex--;
   }
   return errCode;
}

ZH_BOOL zh_rddGetNetErr( void )
{
   return zh_stackRDD()->fNetError;
}

void zh_rddSetNetErr( ZH_BOOL fNetErr )
{
   zh_stackRDD()->fNetError = fNetErr;
}

/*
 * Get (/set) default RDD driver
 */
const char * zh_rddDefaultDrv( const char * szDriver )
{
   PZH_STACKRDD pRddInfo;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_rddDefaultDrv(%s)", szDriver ) );

   pRddInfo = zh_stackRDD();

   if( szDriver && *szDriver )
   {
      char szNewDriver[ ZH_RDD_MAX_DRIVERNAME_LEN + 1 ];
      LPRDDNODE pRddNode;

      zh_strncpyUpper( szNewDriver, szDriver, sizeof( szNewDriver ) - 1 );
      pRddNode = zh_rddFindNode( szNewDriver, NULL );
      if( ! pRddNode )
         return NULL;

      pRddInfo->szDefaultRDD = pRddNode->szName;
   }
   else if( ! pRddInfo->szDefaultRDD && zh_rddGetNode( 0 ) )
   {
      const char * szDrvTable[] = { "DBFCDX", "DBFFPT", "DBF" };
      int i;

      pRddInfo->szDefaultRDD = "";
      for( i = 0; i < ( int ) ZH_SIZEOFARRAY( szDrvTable ); ++i )
      {
         if( zh_rddFindNode( szDrvTable[ i ], NULL ) )
         {
            pRddInfo->szDefaultRDD = szDrvTable[ i ];
            break;
         }
      }
   }

   return pRddInfo->szDefaultRDD;
}

/*
 * Get default RDD driver respecting passed table/file name
 */
const char * zh_rddFindDrv( const char * szDriver, const char * szFileName )
{
   LPRDDNODE pRddNode = NULL;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_rddFindDrv(%s, %s)", szDriver, szFileName ) );

   if( szDriver && *szDriver )
   {
      char szNewDriver[ ZH_RDD_MAX_DRIVERNAME_LEN + 1 ];

      zh_strncpyUpper( szNewDriver, szDriver, sizeof( szNewDriver ) - 1 );
      pRddNode = zh_rddFindNode( szNewDriver, NULL );
   }
   else
   {
      PZH_STACKRDD pRddInfo = zh_stackRDD();

      if( pRddInfo->szDefaultRDD )
      {
         if( pRddInfo->szDefaultRDD[ 0 ] )
            pRddNode = zh_rddFindNode( pRddInfo->szDefaultRDD, NULL );
      }
      else if( zh_rddGetNode( 0 ) )
      {
         const char * szDrvTable[] = { "DBFCDX", "DBFFPT", "DBF" };
         int i;

         pRddInfo->szDefaultRDD = "";
         for( i = 0; i < ( int ) ZH_SIZEOFARRAY( szDrvTable ); ++i )
         {
            pRddNode = zh_rddFindNode( szDrvTable[ i ], NULL );
            if( pRddNode )
            {
               pRddInfo->szDefaultRDD = szDrvTable[ i ];
               break;
            }
         }
      }
   }

   return pRddNode ? zh_rddFindFileNode( pRddNode, szFileName )->szName : NULL;
}

/*
 * Function for getting given workarea pointer
 */
void * zh_rddGetWorkAreaPointer( int iArea )
{
   PZH_STACKRDD pRddInfo;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_rddGetWorkAreaPointer(%d)", iArea ) );

   pRddInfo = zh_stackRDD();

   if( iArea == 0 )
      return pRddInfo->pCurrArea;
   else if( iArea >= 1 && ( ZH_UINT ) iArea < ( ZH_UINT ) pRddInfo->uiWaNumMax )
      return pRddInfo->waList[ pRddInfo->waNums[ iArea ] ];
   else
      return NULL;
}

/*
 * Function for getting current workarea pointer
 */
void * zh_rddGetCurrentWorkAreaPointer( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_rddGetCurrentWorkAreaPointer()" ) );

   return zh_stackRDD()->pCurrArea;
}

/*
 * Return the current WorkArea number.
 */
int zh_rddGetCurrentWorkAreaNumber( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_rddGetCurrentWorkAreaNumber()" ) );

   return zh_stackRDD()->uiCurrArea;
}

/*
 * Select a WorkArea by the number.
 */
ZH_ERRCODE zh_rddSelectWorkAreaNumber( int iArea )
{
   PZH_STACKRDD pRddInfo;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_rddSelectWorkAreaNumber(%d)", iArea ) );

   pRddInfo = zh_stackRDD();
   if( iArea < 1 || iArea > ZH_RDD_MAX_AREA_NUM )
      ZH_SET_WA( 0 );
   else
      ZH_SET_WA( ( ZH_AREANO ) iArea );

   return ( pRddInfo->pCurrArea == NULL ) ? ZH_FAILURE : ZH_SUCCESS;
}


/* Moving workareas between threads */

static ZH_CRITICAL_NEW( s_waMtx );
static ZH_COND_NEW( s_waCond );
static PZH_ITEM s_pDetachedAreas = NULL;

static ZH_GARBAGE_FUNC( zh_waHolderDestructor )
{
   AREAP * pHolder = ( AREAP * ) Cargo;

   if( *pHolder )
   {
      AREAP pArea;
      int iArea;

      pArea = *pHolder;
      *pHolder = NULL;

      iArea = zh_rddGetCurrentWorkAreaNumber();

      if( zh_rddSelectFirstAvailable() != ZH_SUCCESS )
         /* workarea number ZH_RDD_MAX_AREA_NUM is reserved
            for this destructor and used when all other workareas
            are active [druzus] */
         zh_rddSelectWorkAreaNumber( ZH_RDD_MAX_AREA_NUM );
      zh_waNodeInsert( zh_stackRDD(), pArea );
      zh_rddReleaseCurrentArea();

      zh_rddSelectWorkAreaNumber( iArea );
   }
}

static const ZH_GC_FUNCS s_gcWAFuncs =
{
   zh_waHolderDestructor,
   zh_gcDummyMark
};

void zh_rddCloseDetachedAreas( void )
{
   PZH_ITEM pDetachedArea;

   /* protect by critical section access to s_pDetachedAreas array */
   zh_threadEnterCriticalSectionGC( &s_waMtx );
   pDetachedArea = s_pDetachedAreas;
   s_pDetachedAreas = NULL;
   /* leave critical section */
   zh_threadLeaveCriticalSection( &s_waMtx );
   /* release detached areas */
   if( pDetachedArea )
      zh_itemRelease( pDetachedArea );
}

ZH_ERRCODE zh_rddDetachArea( AREAP pArea, PZH_ITEM pCargo )
{
   AREAP * pHolder;
   PZH_ITEM pDetachedArea;
   ZH_SIZE nPos;
   int iArea;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_rddDetachArea(%p, %p)", ( void * ) pArea, ( void * ) pCargo ) );

   /* save current WA number */
   iArea = zh_rddGetCurrentWorkAreaNumber();
   /* select given WA */
   zh_rddSelectWorkAreaNumber( pArea->uiArea );
   /* flush buffers */
   SELF_GOCOLD( pArea );

   /* tests shows that Xbase++ does not remove locks */
   #if 0
   SELF_UNLOCK( pArea, NULL );
   #endif

   /* Xbase++ documentation says that child areas are also detached but
    * but tests shows that it's not true and either child or parent relations
    * are still active and corresponding WA are not detached together.
    * Ziher clears all child and parent relations.
    */
   SELF_CLEARREL( pArea );
   zh_rddCloseAllParentRelations( pArea );

   /* detach WA and alias */
   zh_waNodeDelete( zh_stackRDD() );
   pArea->uiArea = 0;
   if( pArea->atomAlias )
      zh_dynsymSetAreaHandle( ( PZH_DYNSYMBOL ) pArea->atomAlias, 0 );

   /* restore previous WA number */
   zh_rddSelectWorkAreaNumber( iArea );

   /* protect by critical section access to s_pDetachedAreas array */
   zh_threadEnterCriticalSectionGC( &s_waMtx );
   if( ! s_pDetachedAreas )
   {
      s_pDetachedAreas = zh_itemArrayNew( 1 );
      nPos = 1;
   }
   else
   {
      nPos = zh_arrayLen( s_pDetachedAreas ) + 1;
      zh_arraySize( s_pDetachedAreas, nPos );
   }
   pDetachedArea = zh_arrayGetItemPtr( s_pDetachedAreas, nPos );
   zh_arrayNew( pDetachedArea, 2 );
   if( pCargo )
      zh_arraySet( pDetachedArea, 2, pCargo );
   pHolder = ( AREAP * ) zh_gcAllocate( sizeof( AREAP ), &s_gcWAFuncs );
   *pHolder = pArea;
   zh_arraySetPtrGC( pDetachedArea, 1, pHolder );
   /* siagnal waiting processes that new area is available */
   zh_threadCondBroadcast( &s_waCond );
   /* leave critical section */
   zh_threadLeaveCriticalSection( &s_waMtx );

   return ZH_SUCCESS;
}

AREAP zh_rddRequestArea( const char * szAlias, PZH_ITEM pCargo,
                         ZH_BOOL fNewArea, ZH_ULONG ulMilliSec )
{
   PZH_DYNSYMBOL pSymAlias = NULL;
   AREAP pArea = NULL;

   if( pCargo )
      zh_itemClear( pCargo );

   /* close current WA or chose 1st free available */
   if( ! fNewArea )
   {
      zh_rddReleaseCurrentArea();
   }
   else if( zh_rddSelectFirstAvailable() != ZH_SUCCESS )
   {
      zh_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, ZH_ERR_FUNCNAME );
      return NULL;
   }

   if( szAlias )
   {
      pSymAlias = zh_dynsymGet( szAlias );

      /* verify if the alias name is valid symbol */
      if( zh_rddVerifyAliasName( szAlias ) != ZH_SUCCESS )
      {
         zh_errRT_DBCMD_Ext( EG_BADALIAS, EDBCMD_BADALIAS, NULL, szAlias, EF_CANDEFAULT );
         return NULL;
      }
      /* verify if the alias is already in use */
      if( zh_dynsymAreaHandle( pSymAlias ) != 0 )
      {
         zh_errRT_DBCMD_Ext( EG_DUPALIAS, EDBCMD_DUPALIAS, NULL, szAlias, EF_CANDEFAULT );
         return NULL;
      }
   }

   /* protect by critical section access to s_pDetachedAreas array */
   zh_threadEnterCriticalSectionGC( &s_waMtx );
   for( ;; )
   {
      if( s_pDetachedAreas )
      {
         ZH_SIZE nLen = zh_arrayLen( s_pDetachedAreas ), nPos = 1;
         if( pSymAlias )
         {
            for( nPos = 1; nPos <= nLen; ++nPos )
            {
               AREAP * pDetachedArea = ( AREAP * )
                  zh_arrayGetPtrGC( zh_arrayGetItemPtr( s_pDetachedAreas, nPos ),
                                    1, &s_gcWAFuncs );
               if( pSymAlias == ( PZH_DYNSYMBOL ) ( *pDetachedArea )->atomAlias )
                  break;
            }
         }
         if( nPos <= nLen )
         {
            PZH_ITEM pArray = zh_arrayGetItemPtr( s_pDetachedAreas, nPos );
            AREAP * pDetachedArea = ( AREAP * )
                                    zh_arrayGetPtrGC( pArray, 1, &s_gcWAFuncs );

            pArea = *pDetachedArea;
            *pDetachedArea = NULL;
            if( pCargo )
               zh_arrayGet( pArray, 2, pCargo );
            zh_arrayDel( s_pDetachedAreas, nPos );
            zh_arraySize( s_pDetachedAreas, nLen - 1 );
         }
      }

      if( pArea || ulMilliSec == 0 )
         break;

      zh_vmUnlock();
      /* wait for detached workareas */
      if( ulMilliSec == ZH_THREAD_INFINITE_WAIT )
         zh_threadCondWait( &s_waCond, &s_waMtx );
      else if( ! zh_threadCondTimedWait( &s_waCond, &s_waMtx, ulMilliSec ) )
         ulMilliSec = 0;
      zh_vmLock();

      if( ulMilliSec == 0 || zh_vmRequestQuery() != 0 )
         break;
   }
   /* leave critical section */
   zh_threadLeaveCriticalSection( &s_waMtx );

   /* attach WA and set alias */
   if( pArea )
   {
      zh_waNodeInsert( zh_stackRDD(), pArea );
      if( pArea->atomAlias )
      {
         if( zh_dynsymAreaHandle( ( PZH_DYNSYMBOL ) pArea->atomAlias ) == 0 )
            zh_dynsymSetAreaHandle( ( PZH_DYNSYMBOL ) pArea->atomAlias, pArea->uiArea );
      }
   }

   return pArea;
}

PZH_ITEM zh_rddDetachedList( void )
{
   PZH_ITEM pArray;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_rddDetachedList()" ) );

   pArray = zh_itemArrayNew( 0 );
   /* protect by critical section access to s_pDetachedAreas array */
   zh_threadEnterCriticalSectionGC( &s_waMtx );
   if( s_pDetachedAreas )
   {
      ZH_SIZE nLen = zh_arrayLen( s_pDetachedAreas ), nPos;

      zh_arraySize( pArray, nLen );
      for( nPos = 1; nPos <= nLen; ++nPos )
      {
         AREAP * pDetachedArea = ( AREAP * )
               zh_arrayGetPtrGC( zh_arrayGetItemPtr( s_pDetachedAreas, nPos ),
                                 1, &s_gcWAFuncs );
         PZH_DYNSYMBOL pAlias = ( PZH_DYNSYMBOL ) ( *pDetachedArea )->atomAlias;
         zh_arraySetC( pArray, nPos, zh_dynsymName( pAlias ) );
      }
   }
   /* leave critical section */
   zh_threadLeaveCriticalSection( &s_waMtx );

   return pArray;
}
