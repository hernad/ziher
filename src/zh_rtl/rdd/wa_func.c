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
#include "zh_set.h"
#include "zh_rtl/rdd_sys.zhh"

/*
 * check if a given name can be used as alias expression
 */
ZH_ERRCODE zh_rddVerifyAliasName( const char * szAlias )
{
   if( szAlias )
   {
      char c;

      c = *szAlias;
      if( ( c >= 'A' && c <= 'Z' ) || ( c >= 'a' && c <= 'z' ) || c == '_' )
      {
         c = *( ++szAlias );
         while( c != 0 )
         {
            if( c != '_' && ! ( c >= '0' && c <= '9' ) &&
                ! ( c >= 'A' && c <= 'Z' ) && ! ( c >= 'a' && c <= 'z' ) )
            {
               if( c == ' ' )
               {
                  while( *( ++szAlias ) == ' ' )
                     ;
                  if( ! *szAlias )
                     break;
               }
               return ZH_FAILURE;
            }
            c = *( ++szAlias );
         }
         return ZH_SUCCESS;
      }
   }
   return ZH_FAILURE;
}

/*
 * Prepares a new WorkArea node.
 */
void * zh_rddNewAreaNode( LPRDDNODE pRddNode, ZH_USHORT uiRddID )
{
   AREAP pArea;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_rddNewAreaNode(%p,%hu)", ( void * ) pRddNode, uiRddID ) );

   if( pRddNode->uiAreaSize == 0 ) /* Calculate the size of WorkArea */
   {
      ZH_USHORT uiSize;

      pArea = ( AREAP ) zh_xgrabz( sizeof( AREA ) );
      pArea->lprfsHost = &pRddNode->pTable;
      pArea->rddID = uiRddID;

      if( SELF_STRUCTSIZE( pArea, &uiSize ) != ZH_SUCCESS )
         return NULL;

      /* Need more space? */
      if( uiSize > sizeof( AREA ) )   /* Size of Area changed */
      {
         pArea = ( AREAP ) zh_xrealloc( pArea, uiSize );
         memset( pArea, 0, uiSize );
         pArea->lprfsHost = &pRddNode->pTable;
         pArea->rddID = uiRddID;
      }

      pRddNode->uiAreaSize = uiSize;  /* Update the size of WorkArea */
   }
   else
   {
      pArea = ( AREAP ) zh_xgrabz( pRddNode->uiAreaSize );
      pArea->lprfsHost = &pRddNode->pTable;
      pArea->rddID = uiRddID;
   }

   if( SELF_NEW( pArea ) != ZH_SUCCESS )
   {
      SELF_RELEASE( pArea );
      return NULL;
   }

   return ( void * ) pArea;
}

ZH_ERRCODE zh_rddGetTempAlias( char * szAliasTmp )
{
   int i, iArea;

   for( i = 1; i < 1000; i++ )
   {
      zh_snprintf( szAliasTmp, 11, "__ZHTMP%03i", i );
      if( zh_rddGetAliasNumber( szAliasTmp, &iArea ) != ZH_SUCCESS )
         return ZH_SUCCESS;
   }
   szAliasTmp[ 0 ] = '\0';
   return ZH_FAILURE;
}

/*
 * allocate and return atomAlias for new workarea or NULL if alias already exist
 */
void * zh_rddAllocWorkAreaAlias( const char * szAlias, int iArea )
{
   int iDummyArea;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_rddAllocWorkAreaAlias(%s, %d)", szAlias, iArea ) );

   /* Verify if the alias name is valid symbol */
   if( zh_rddVerifyAliasName( szAlias ) != ZH_SUCCESS )
      zh_errRT_DBCMD_Ext( EG_BADALIAS, EDBCMD_BADALIAS, NULL, szAlias, EF_CANDEFAULT );
   /* Verify if the alias is already in use */
   else if( zh_rddGetAliasNumber( szAlias, &iDummyArea ) == ZH_SUCCESS )
      zh_errRT_DBCMD_Ext( EG_DUPALIAS, EDBCMD_DUPALIAS, NULL, szAlias, EF_CANDEFAULT );
   else
   {
      PZH_DYNS pSymAlias = zh_dynsymGet( szAlias );

      if( zh_dynsymAreaHandle( pSymAlias ) == 0 )
      {
         zh_dynsymSetAreaHandle( pSymAlias, iArea );
         return pSymAlias;
      }
      zh_errRT_DBCMD_Ext( EG_DUPALIAS, EDBCMD_DUPALIAS, NULL, szAlias, EF_CANDEFAULT );
   }

   return NULL;
}

/*
 * Find a field index by name
 */
ZH_USHORT zh_rddFieldIndex( AREAP pArea, const char * szName )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_rddFieldIndex(%p, %s)", ( void * ) pArea, szName ) );

   while( ZH_ISSPACE( *szName ) )
      ++szName;

   if( *szName )
   {
      ZH_SIZE nLen = strlen( szName );

      while( ZH_ISSPACE( szName[ nLen - 1 ] ) )
         --nLen;

      if( nLen <= ZH_SYMBOL_NAME_LEN )
      {
         char szFieldName[ ZH_SYMBOL_NAME_LEN + 1 ];
         PZH_DYNS pDynSym;

         szFieldName[ nLen ] = '\0';
         while( nLen-- )
            szFieldName[ nLen ] = ZH_TOUPPER( szName[ nLen ] );

         pDynSym = zh_dynsymFind( szFieldName );
         if( pDynSym )
         {
            LPFIELD pField = pArea->lpFields;
            ZH_USHORT uiCount = 0;

            while( pField )
            {
               ++uiCount;
               if( pDynSym == ( PZH_DYNS ) pField->sym )
                  return uiCount;
               pField = pField->lpfNext;
            }
         }
      }
   }
   return 0;
}

/*
 * find a field expression index, this function strips _FIELD->, FIELD->,
 * alias-> prefixes
 */
ZH_USHORT zh_rddFieldExpIndex( AREAP pArea, const char * szField )
{
   while( ZH_ISSPACE( *szField ) )
      ++szField;

   if( strchr( szField, '>' ) != NULL )
   {
      char szAlias[ ZH_RDD_MAX_ALIAS_LEN + 1 ];
      int j, l, n;

      n = 0;
      if( SELF_ALIAS( pArea, szAlias ) == ZH_SUCCESS )
         l = ( int ) strlen( szAlias );
      else
         l = 0;

      /*
       * strip the _FIELD-> and FIELD-> prefix, it could be nested
       * so repeat this process until all prefixes will be removed
       */
      do
      {
         int i;
         j = n;
         i = 0;
         if( ZH_ISFIRSTIDCHAR( szField[ n ] ) )
         {
            ++i;
            while( ZH_ISNEXTIDCHAR( szField[ n + i ] ) )
               ++i;

            if( ! ( ( i == l &&
                      zh_strnicmp( &szField[ n ], szAlias, l ) == 0 ) ) &&
                ! ( i >= 4 && i <= 5 &&
                    zh_strnicmp( &szField[ n ], "FIELD", i ) == 0 ) &&
                ! ( i >= 4 && i <= 6 &&
                    zh_strnicmp( &szField[ n ], "_FIELD", i ) == 0 ) )
            {
               i = 0;
            }
         }

         if( i > 0 )
         {
            i += n;
            while( ZH_ISSPACE( szField[ i ] ) )
               i++;
            if( szField[ i ] == '-' && szField[ i + 1 ] == '>' )
            {
               n = i + 2;
               while( szField[ n ] == ' ' )
                  n++;
            }
         }
      }
      while( n != j );
      szField = &szField[ n ];
   }
   return zh_rddFieldIndex( pArea, szField );
}

/*
 * Find a WorkArea by the alias, return ZH_FAILURE if not found
 */
ZH_ERRCODE zh_rddGetAliasNumber( const char * szAlias, int * iArea )
{
   ZH_BOOL fOneLetter;
   char c;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_rddGetAliasNumber(%s, %p)", szAlias, ( void * ) iArea ) );

   while( *szAlias == ' ' )
      szAlias++;

   c = szAlias[ 0 ];
   if( c >= 'a' && c <= 'z' )
      c -= 'a' - 'A';

   fOneLetter = c && ( szAlias[ 1 ] == 0 || szAlias[ 1 ] == ' ' );

   if( c >= '0' && c <= '9' )
   {
      *iArea = atoi( szAlias );
   }
   else if( fOneLetter && c >= 'A' && c <= 'K' )
   {
      *iArea = c - 'A' + 1;
   }
   else if( fOneLetter && c == 'M' )
   {
      *iArea = ZH_RDD_MAX_AREA_NUM;
   }
   else
   {
      PZH_DYNS pSymAlias = zh_dynsymFindName( szAlias );

      *iArea = pSymAlias ? ( int ) zh_dynsymAreaHandle( pSymAlias ) : 0;
      if( *iArea == 0 )
         return ZH_FAILURE;
   }

   return ZH_SUCCESS;
}

/*
 * Select a WorkArea by the symbol name.
 */
ZH_ERRCODE zh_rddSelectWorkAreaSymbol( PZH_SYMBOL pSymAlias )
{
   PZH_ITEM pError;
   ZH_ERRCODE errCode;
   const char * szName;
   int iArea;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_rddSelectWorkAreaSymbol(%p)", ( void * ) pSymAlias ) );

   iArea = ( int ) zh_dynsymAreaHandle( pSymAlias->pDynSym );
   if( iArea )
   {
      zh_rddSelectWorkAreaNumber( iArea );
      return ZH_SUCCESS;
   }

   szName = zh_dynsymName( pSymAlias->pDynSym );

   if( szName[ 0 ] && ! szName[ 1 ] )
   {
      if( szName[ 0 ] >= 'A' && szName[ 0 ] <= 'K' )
      {
         zh_rddSelectWorkAreaNumber( szName[ 0 ] - 'A' + 1 );
         return ZH_SUCCESS;
      }
      else if( szName[ 0 ] >= 'a' && szName[ 0 ] <= 'k' )
      {
         zh_rddSelectWorkAreaNumber( szName[ 0 ] - 'a' + 1 );
         return ZH_SUCCESS;
      }
      else if( szName[ 0 ] == 'M' || szName[ 0 ] == 'm' )
      {
         zh_rddSelectWorkAreaNumber( ZH_RDD_MAX_AREA_NUM );
         return ZH_SUCCESS;
      }
   }

   /*
    * generate an error with retry possibility
    * (user created error handler can open a missing database)
    */

   pError = zh_errRT_New( ES_ERROR, NULL, EG_NOALIAS, EDBCMD_NOALIAS, NULL, pSymAlias->szName, 0, EF_CANRETRY );
   errCode = ZH_FAILURE;

   do
   {
      if( zh_errLaunch( pError ) != E_RETRY )
         break;
      iArea = ( int ) zh_dynsymAreaHandle( pSymAlias->pDynSym );
      if( iArea )
      {
         zh_rddSelectWorkAreaNumber( iArea );
         errCode = ZH_SUCCESS;
      }
   }
   while( errCode == ZH_FAILURE );

   zh_itemRelease( pError );

   return errCode;
}

/*
 * Select a WorkArea by the name.
 */
ZH_ERRCODE zh_rddSelectWorkAreaAlias( const char * szAlias )
{
   ZH_ERRCODE errCode;
   int iArea;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_rddSelectWorkAreaAlias(%s)", szAlias ) );

   errCode = zh_rddGetAliasNumber( szAlias, &iArea );

   if( errCode == ZH_FAILURE )
   {
      /*
       * generate an error with retry possibility
       * (user created error handler can open a missing database)
       */
      PZH_ITEM pError = zh_errRT_New( ES_ERROR, NULL, EG_NOALIAS, EDBCMD_NOALIAS, NULL, szAlias, 0, EF_CANRETRY );

      do
      {
         if( zh_errLaunch( pError ) != E_RETRY )
            break;
         errCode = zh_rddGetAliasNumber( szAlias, &iArea );
      }
      while( errCode == ZH_FAILURE );

      zh_itemRelease( pError );
   }

   if( errCode == ZH_SUCCESS )
   {
      if( iArea < 1 || iArea > ZH_RDD_MAX_AREA_NUM )
         errCode = zh_rddSelectFirstAvailable();
      else
         errCode = zh_rddSelectWorkAreaNumber( iArea );
   }

   return errCode;
}

/*
 * Obtain the current value of a field.
 */
ZH_ERRCODE zh_rddFieldGet( PZH_ITEM pItem, PZH_SYMBOL pFieldSymbol )
{
   AREAP pArea;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_rddFieldGet(%p, %p)", ( void * ) pItem, ( void * ) pFieldSymbol ) );

   pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
   if( pArea )
   {
      ZH_USHORT uiField = 1;
      LPFIELD pField = pArea->lpFields;
      PZH_DYNS pDynSym = pFieldSymbol->pDynSym;

      while( pField )
      {
         if( ( PZH_DYNS ) pField->sym == pDynSym )
         {
            return SELF_GETVALUE( pArea, uiField, pItem );
         }
         ++uiField;
         pField = pField->lpfNext;
      }
   }
   return ZH_FAILURE;
}

/*
 * Assign a value to a field.
 */
ZH_ERRCODE zh_rddFieldPut( PZH_ITEM pItem, PZH_SYMBOL pFieldSymbol )
{
   AREAP pArea;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_rddFieldPut(%p, %p)", ( void * ) pItem, ( void * ) pFieldSymbol ) );

   pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
   if( pArea )
   {
      ZH_USHORT uiField = 1;
      LPFIELD pField = pArea->lpFields;
      PZH_DYNS pDynSym = pFieldSymbol->pDynSym;

      while( pField )
      {
         if( ( PZH_DYNS ) pField->sym == pDynSym )
         {
            return SELF_PUTVALUE( pArea, uiField, pItem );
         }
         ++uiField;
         pField = pField->lpfNext;
      }
   }
   return ZH_FAILURE;
}

/*
 * Obtain the current value of a field.
 */
ZH_ERRCODE zh_rddGetFieldValue( PZH_ITEM pItem, PZH_SYMBOL pFieldSymbol )
{
   ZH_ERRCODE errCode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_rddGetFieldValue(%p, %p)", ( void * ) pItem, ( void * ) pFieldSymbol ) );

   errCode = zh_rddFieldGet( pItem, pFieldSymbol );

   if( errCode == ZH_FAILURE && zh_vmRequestQuery() == 0 )
   {
      /*
       * generate an error with retry possibility
       * (user created error handler can make this field accessible)
       */
      PZH_ITEM pError;

      pError = zh_errRT_New( ES_ERROR, NULL, EG_NOVAR, EDBCMD_NOVAR,
                             NULL, pFieldSymbol->szName, 0, EF_CANRETRY );
      zh_itemClear( pItem );

      while( zh_errLaunch( pError ) == E_RETRY )
      {
         errCode = zh_rddFieldGet( pItem, pFieldSymbol );

         if( errCode == ZH_SUCCESS || zh_vmRequestQuery() != 0 )
            break;
      }
      zh_itemRelease( pError );
   }

   return errCode;
}

/*
 * Assign a value to a field.
 */
ZH_ERRCODE zh_rddPutFieldValue( PZH_ITEM pItem, PZH_SYMBOL pFieldSymbol )
{
   ZH_ERRCODE errCode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_rddPutFieldValue(%p, %p)", ( void * ) pItem, ( void * ) pFieldSymbol ) );

   errCode = zh_rddFieldPut( pItem, pFieldSymbol );

   if( errCode == ZH_FAILURE && zh_vmRequestQuery() == 0 )
   {
      /*
       * generate an error with retry possibility
       * (user created error handler can make this field accessible)
       */
      PZH_ITEM pError = zh_errRT_New( ES_ERROR, NULL, EG_NOVAR, EDBCMD_NOVAR,
                                      NULL, pFieldSymbol->szName, 0, EF_CANRETRY );

      while( zh_errLaunch( pError ) == E_RETRY )
      {
         errCode = zh_rddFieldPut( pItem, pFieldSymbol );

         if( errCode == ZH_SUCCESS || zh_vmRequestQuery() != 0 )
            break;
      }
      zh_itemRelease( pError );
   }

   return errCode;
}

ZH_ERRCODE zh_rddOpenTable( const char * szFileName, const char * szDriver,
                            ZH_USHORT uiArea, const char * szAlias,
                            ZH_BOOL fShared, ZH_BOOL fReadonly,
                            const char * szCpId, ZH_ULONG ulConnection,
                            PZH_ITEM pStruct, PZH_ITEM pDelim )
{
   DBOPENINFO pInfo;
   ZH_ERRCODE errCode;
   AREAP pArea;

   /* uiArea = 0 in zh_rddInsertAreaNode() means chose first
    * available free area, otherwise we should close table in
    * current WA and it should be done before parameter validation
    * RT errors below. This breaks xZiher like MT code which
    * shares WA between threads so dbUseArea() should be covered
    * by external mutex to make lNewArea MT safe, [druzus]
    */
   if( uiArea && uiArea < ZH_RDD_MAX_AREA_NUM )
   {
      zh_rddSelectWorkAreaNumber( uiArea );
      zh_rddReleaseCurrentArea();
   }
   else if( zh_rddSelectFirstAvailable() != ZH_SUCCESS )
   {
      zh_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, ZH_ERR_FUNCNAME );
      return ZH_FAILURE;
   }

   zh_rddSetNetErr( ZH_FALSE );

   szDriver = zh_rddFindDrv( szDriver, szFileName );

   /* First try to create new area node and validate RDD name */
   if( ! szDriver || ! zh_rddInsertAreaNode( szDriver ) )
   {
      zh_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, ZH_ERR_FUNCNAME );
      return ZH_FAILURE;
   }

   if( ! szFileName )
   {
      zh_rddReleaseCurrentArea();
      zh_errRT_DBCMD( EG_ARG, EDBCMD_USE_BADPARAMETER, NULL, ZH_ERR_FUNCNAME );
      return ZH_FAILURE;
   }

   pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   /* Fill pInfo structure */
   pInfo.uiArea = pArea->uiArea;
   pInfo.abName = szFileName;
   pInfo.atomAlias = szAlias;
   pInfo.fShared = fShared;
   pInfo.fReadonly = fReadonly;
   pInfo.cdpId = szCpId ? szCpId : zh_setGetDBCODEPAGE();
   pInfo.ulConnection = ulConnection;
   pInfo.lpdbHeader = NULL;

   errCode = pStruct ? SELF_CREATEFIELDS( pArea, pStruct ) : ZH_SUCCESS;
   if( errCode == ZH_SUCCESS )
   {
      if( pDelim && ! ZH_IS_NIL( pDelim ) )
         errCode = SELF_INFO( pArea, DBI_SETDELIMITER, pDelim );
      if( errCode == ZH_SUCCESS )
         /* Open file */
         errCode = SELF_OPEN( pArea, &pInfo );
   }

   if( errCode != ZH_SUCCESS )
      zh_rddReleaseCurrentArea();

   return errCode;
}

ZH_ERRCODE zh_rddCreateTable( const char * szFileName, const char * szDriver,
                              ZH_USHORT uiArea, const char * szAlias,
                              ZH_BOOL fKeepOpen,
                              const char * szCpId, ZH_ULONG ulConnection,
                              PZH_ITEM pStruct, PZH_ITEM pDelim )
{
   DBOPENINFO pInfo;
   ZH_ERRCODE errCode;
   ZH_USHORT uiPrevArea;
   AREAP pArea;

   if( ! szFileName )
   {
      zh_errRT_DBCMD( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, ZH_ERR_FUNCNAME );
      return ZH_FAILURE;
   }

   uiPrevArea = ( ZH_AREANO ) zh_rddGetCurrentWorkAreaNumber();

   /* 0 means chose first available in zh_rddInsertAreaNode() */
   zh_rddSelectWorkAreaNumber( uiArea );
   if( uiArea )
      zh_rddReleaseCurrentArea();

   szDriver = zh_rddFindDrv( szDriver, szFileName );

   /* Create a new WorkArea node */
   if( ! szDriver || ! zh_rddInsertAreaNode( szDriver ) )
   {
      zh_rddSelectWorkAreaNumber( uiPrevArea );
      zh_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, ZH_ERR_FUNCNAME );
      return ZH_FAILURE;
   }
   pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   /* Fill pInfo structure */
   pInfo.uiArea = pArea->uiArea;
   pInfo.abName = szFileName;
   pInfo.atomAlias = szAlias;
   pInfo.fShared = ZH_FALSE;
   pInfo.fReadonly = ZH_FALSE;
   pInfo.cdpId = szCpId ? szCpId : zh_setGetDBCODEPAGE();
   pInfo.ulConnection = ulConnection;
   pInfo.lpdbHeader = NULL;

   if( pDelim && ! ZH_IS_NIL( pDelim ) )
      errCode = SELF_INFO( pArea, DBI_SETDELIMITER, pDelim );
   else
      errCode = ZH_SUCCESS;

   if( errCode == ZH_SUCCESS )
   {
      errCode = SELF_CREATEFIELDS( pArea, pStruct );
      if( errCode == ZH_SUCCESS )
         errCode = SELF_CREATE( pArea, &pInfo );
   }

   if( ! fKeepOpen || errCode != ZH_SUCCESS )
   {
      zh_rddReleaseCurrentArea();
      zh_rddSelectWorkAreaNumber( uiPrevArea );
   }

   return errCode;
}

ZH_ERRCODE zh_rddCreateTableTemp( const char * szDriver,
                                  const char * szAlias,
                                  const char * szCpId, ZH_ULONG ulConnection,
                                  PZH_ITEM pStruct )
{
   char szDriverBuffer[ ZH_RDD_MAX_DRIVERNAME_LEN + 1 ];
   DBOPENINFO pInfo;
   PZH_ITEM pItem;
   ZH_ERRCODE errCode;
   ZH_USHORT uiPrevArea;
   AREAP pArea;

   uiPrevArea = ( ZH_AREANO ) zh_rddGetCurrentWorkAreaNumber();

   /* 0 means chose first available in zh_rddInsertAreaNode() */
   zh_rddSelectWorkAreaNumber( 0 );

   if( szDriver && szDriver[ 0 ] )
   {
      zh_strncpyUpper( szDriverBuffer, szDriver, sizeof( szDriverBuffer ) - 1 );
      szDriver = szDriverBuffer;
   }
   else
      szDriver = zh_rddDefaultDrv( NULL );

   /* Create a new WorkArea node */
   if( ! zh_rddInsertAreaNode( szDriver ) )
   {
      zh_rddSelectWorkAreaNumber( uiPrevArea );
      zh_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, ZH_ERR_FUNCNAME );
      return ZH_FAILURE;
   }
   pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   /* Fill pInfo structure */
   pInfo.uiArea = pArea->uiArea;
   pInfo.abName = NULL;
   pInfo.atomAlias = szAlias;
   pInfo.fShared = ZH_FALSE;
   pInfo.fReadonly = ZH_FALSE;
   pInfo.cdpId = szCpId ? szCpId : zh_setGetDBCODEPAGE();
   pInfo.ulConnection = ulConnection;
   pInfo.lpdbHeader = NULL;

   pItem = zh_itemPutL( NULL, ZH_TRUE );
   errCode = SELF_INFO( pArea, DBI_ISTEMPORARY, pItem );
   zh_itemRelease( pItem );

   if( errCode == ZH_SUCCESS )
   {
      errCode = SELF_CREATEFIELDS( pArea, pStruct );
      if( errCode == ZH_SUCCESS )
         errCode = SELF_CREATE( pArea, &pInfo );
   }

   if( errCode != ZH_SUCCESS )
   {
      zh_rddReleaseCurrentArea();
      zh_rddSelectWorkAreaNumber( uiPrevArea );
   }

   return errCode;
}

static void zh_fldStructure( AREAP pArea, ZH_USHORT uiField, ZH_USHORT uiSize,
                             PZH_ITEM pField )
{

   static const ZH_USHORT s_uiActions[] =
            { DBS_NAME, DBS_TYPE, DBS_LEN, DBS_DEC };

   ZH_USHORT uiCount;

   if( uiSize == 0 || uiSize > ZH_SIZEOFARRAY( s_uiActions ) )
      uiSize = ZH_SIZEOFARRAY( s_uiActions );

   zh_arrayNew( pField, uiSize );
   for( uiCount = 0; uiCount < uiSize; ++uiCount )
   {
      SELF_FIELDINFO( pArea, uiField, s_uiActions[ uiCount ],
                      zh_arrayGetItemPtr( pField, uiCount + 1 ) );
   }
}

void zh_tblStructure( AREAP pArea, PZH_ITEM pStruct, ZH_USHORT uiSize )
{
   ZH_USHORT uiFields, uiCount;

   if( SELF_FIELDCOUNT( pArea, &uiFields ) == ZH_SUCCESS )
   {
      if( zh_arraySize( pStruct, uiFields ) )
      {
         for( uiCount = 1; uiCount <= uiFields; ++uiCount )
            zh_fldStructure( pArea, uiCount, uiSize,
                             zh_arrayGetItemPtr( pStruct, uiCount ) );
      }
   }
}

static const char * zh_dbTransFieldPos( PZH_ITEM pFields, ZH_USHORT uiField )
{
   const char * szField = NULL;
   PZH_ITEM pItem;

   pItem = zh_arrayGetItemPtr( pFields, uiField );
   if( pItem )
   {
      if( ZH_IS_ARRAY( pItem ) )
         szField = zh_arrayGetCPtr( pItem, DBS_NAME );
      else
         szField = zh_itemGetCPtr( pItem );

      if( *szField == '\0' )
         szField = NULL;
   }

   return szField;
}

static const ZH_GC_FUNCS s_gcTransInfo =
{
   zh_gcDummyClear,
   zh_gcDummyMark
};

PZH_ITEM zh_dbTransInfoPut( PZH_ITEM pItem, LPDBTRANSINFO lpdbTransInfo )
{
   LPDBTRANSINFO * pHolder;

   pHolder = ( LPDBTRANSINFO * ) zh_gcAllocate( sizeof( LPDBTRANSINFO ), &s_gcTransInfo );
   *pHolder = lpdbTransInfo;

   return zh_itemPutPtrGC( pItem, pHolder );
}

LPDBTRANSINFO zh_dbTransInfoGet( PZH_ITEM pItem )
{
   LPDBTRANSINFO * pHolder = ( LPDBTRANSINFO * ) zh_itemGetPtrGC( pItem, &s_gcTransInfo );

   return pHolder ? * pHolder : NULL;
}

/* update counters for autoinc and rowver fields */
ZH_ERRCODE zh_dbTransCounters( LPDBTRANSINFO lpdbTransInfo )
{
   PZH_ITEM pItem = zh_itemNew( NULL );
   ZH_USHORT uiCount;

   for( uiCount = 0; uiCount < lpdbTransInfo->uiItemCount; ++uiCount )
   {
      LPDBTRANSITEM lpdbTransItem = &lpdbTransInfo->lpTransItems[ uiCount ];

      if( SELF_FIELDINFO( lpdbTransInfo->lpaSource, lpdbTransItem->uiSource,
                          DBS_COUNTER, pItem ) == ZH_SUCCESS &&
          SELF_FIELDINFO( lpdbTransInfo->lpaDest, lpdbTransItem->uiDest,
                          DBS_COUNTER, pItem ) == ZH_SUCCESS )
      {
         zh_itemClear( pItem );
         if( SELF_FIELDINFO( lpdbTransInfo->lpaSource, lpdbTransItem->uiSource,
                             DBS_STEP, pItem ) == ZH_SUCCESS )
             SELF_FIELDINFO( lpdbTransInfo->lpaDest, lpdbTransItem->uiDest,
                             DBS_STEP, pItem );
      }
      zh_itemClear( pItem );
   }
   zh_itemRelease( pItem );

   return ZH_SUCCESS;
}

ZH_ERRCODE zh_dbTransStruct( AREAP lpaSource, AREAP lpaDest,
                             LPDBTRANSINFO lpdbTransInfo,
                             PZH_ITEM * pStruct, PZH_ITEM pFields )
{
   ZH_USHORT uiFields, uiSize, uiCount, uiPosSrc, uiPosDst, uiSizeSrc, uiSizeDst;
   ZH_ERRCODE errCode;
   const char * szField;
   ZH_BOOL fAll;

   errCode = SELF_FIELDCOUNT( lpaSource, &uiSizeSrc );
   if( errCode != ZH_SUCCESS )
      return errCode;

   if( lpaDest )
   {
      errCode = SELF_FIELDCOUNT( lpaDest, &uiSizeDst );
      if( errCode != ZH_SUCCESS )
         return errCode;
      uiSize = ZH_MIN( uiSizeDst, uiSizeSrc );
   }
   else
      uiSize = uiSizeDst = uiSizeSrc;

   if( ! uiSize )
      return ZH_FAILURE;
   if( zh_itemType( pFields ) & ZH_IT_ARRAY )
   {
      uiFields = ( ZH_USHORT ) zh_arrayLen( pFields );
      if( uiFields )
         uiSize = uiFields;
   }
   else
      uiFields = 0;

   fAll = ( uiSizeDst == uiSizeSrc );

   lpdbTransInfo->lpaSource    = lpaSource;
   lpdbTransInfo->lpaDest      = lpaDest;
   lpdbTransInfo->lpTransItems = ( LPDBTRANSITEM )
                                    zh_xgrab( uiSize * sizeof( DBTRANSITEM ) );

   if( ! lpaDest )
   {
      *pStruct = zh_itemNew( NULL );
      zh_arrayNew( *pStruct, 0 );
   }

   if( uiFields == 0 )
   {
      if( lpaDest )
      {
         PZH_ITEM pItem = zh_itemNew( NULL );
         uiSize = 0;
         for( uiCount = 1; uiCount <= uiSizeSrc; ++uiCount )
         {
            if( SELF_FIELDINFO( lpaSource, uiCount, DBS_NAME, pItem ) != ZH_SUCCESS )
            {
               uiSize = 0;
               break;
            }
            szField = zh_itemGetCPtr( pItem );
            uiPosDst = zh_rddFieldExpIndex( lpaDest, szField );
            if( uiPosDst != uiCount )
               fAll = ZH_FALSE;
            if( uiPosDst )
            {
               ZH_USHORT ui;

               /* check for replicated field names in source area */
               for( ui = 0; ui < uiSize; ++ui )
               {
                  if( lpdbTransInfo->lpTransItems[ ui ].uiDest == uiPosDst )
                     break;
               }
               if( ui == uiSize )
               {
                  lpdbTransInfo->lpTransItems[ uiSize ].uiSource = uiCount;
                  lpdbTransInfo->lpTransItems[ uiSize++ ].uiDest = uiPosDst;
               }
            }
         }
         zh_itemRelease( pItem );
      }
      else
      {
         zh_tblStructure( lpaSource, *pStruct, 0 );
         uiSize = ( ZH_USHORT ) zh_arrayLen( *pStruct );
         for( uiCount = 0; uiCount < uiSize; ++uiCount )
         {
            lpdbTransInfo->lpTransItems[ uiCount ].uiSource =
            lpdbTransInfo->lpTransItems[ uiCount ].uiDest = uiCount + 1;
         }
      }
   }
   else
   {
      uiSize = 0;
      for( uiCount = 1; uiCount <= uiFields; ++uiCount )
      {
         szField = zh_dbTransFieldPos( pFields, uiCount );
         if( szField )
         {
            uiPosSrc = zh_rddFieldExpIndex( lpaSource, szField );
            if( ! uiPosSrc )
               continue;
            if( lpaDest )
               uiPosDst = zh_rddFieldExpIndex( lpaDest, szField );
            else
               uiPosDst = uiSize + 1;
            if( uiPosDst )
            {
               if( uiPosSrc != uiPosDst )
                  fAll = ZH_FALSE;
               lpdbTransInfo->lpTransItems[ uiSize ].uiSource = uiPosSrc;
               lpdbTransInfo->lpTransItems[ uiSize++ ].uiDest = uiPosDst;
               if( ! lpaDest )
               {
                  zh_arraySize( *pStruct, uiSize );
                  zh_fldStructure( lpaSource, uiPosSrc, 0,
                                   zh_arrayGetItemPtr( *pStruct, uiSize ) );
               }
            }
         }
      }
   }

   if( uiSize != uiSizeSrc )
      fAll = ZH_FALSE;

   if( fAll && lpaDest )
   {
      PZH_ITEM pSrcItm = zh_itemNew( NULL ),
               pDstItm = zh_itemNew( NULL );
      /*
       * if fAll is ZH_TRUE here then it means that all fields are included
       * and they are on the same positions in both tables, so now check
       * if their types and sizes are also equal
       */
      for( uiCount = 1; uiCount <= uiSize; ++uiCount )
      {
         if( SELF_FIELDINFO( lpaSource, uiCount, DBS_TYPE, pSrcItm ) != ZH_SUCCESS ||
             SELF_FIELDINFO( lpaDest,   uiCount, DBS_TYPE, pDstItm ) != ZH_SUCCESS )
         {
            uiSize = 0;
            break;
         }
         if( zh_stricmp( zh_itemGetCPtr( pSrcItm ),
                         zh_itemGetCPtr( pDstItm ) ) != 0 )
         {
            fAll = ZH_FALSE;
            break;
         }
         if( SELF_FIELDINFO( lpaSource, uiCount, DBS_LEN, pSrcItm ) != ZH_SUCCESS ||
             SELF_FIELDINFO( lpaDest,   uiCount, DBS_LEN, pDstItm ) != ZH_SUCCESS )
         {
            uiSize = 0;
            break;
         }
         if( zh_itemGetNL( pSrcItm ) != zh_itemGetNL( pDstItm ) )
         {
            fAll = ZH_FALSE;
            break;
         }
         if( SELF_FIELDINFO( lpaSource, uiCount, DBS_DEC, pSrcItm ) != ZH_SUCCESS ||
             SELF_FIELDINFO( lpaDest,   uiCount, DBS_DEC, pDstItm ) != ZH_SUCCESS )
         {
            uiSize = 0;
            break;
         }
         if( zh_itemGetNL( pSrcItm ) != zh_itemGetNL( pDstItm ) )
         {
            fAll = ZH_FALSE;
            break;
         }

      }
      zh_itemRelease( pSrcItm );
      zh_itemRelease( pDstItm );
   }

   lpdbTransInfo->uiFlags = fAll ? DBTF_MATCH : 0;
   lpdbTransInfo->uiItemCount = uiSize;

   return uiSize ? ZH_SUCCESS : ZH_FAILURE;
}

ZH_ERRCODE zh_rddTransRecords( AREAP pArea,
                               const char * szFileName, const char * szDriver,
                               ZH_ULONG ulConnection,
                               PZH_ITEM pFields, ZH_BOOL fExport,
                               PZH_ITEM pCobFor, PZH_ITEM pStrFor,
                               PZH_ITEM pCobWhile, PZH_ITEM pStrWhile,
                               PZH_ITEM pNext, PZH_ITEM pRecID,
                               PZH_ITEM pRest,
                               const char * szCpId,
                               PZH_ITEM pDelim )
{
   AREAP lpaClose = NULL;
   PZH_ITEM pStruct = NULL;
   DBTRANSINFO dbTransInfo;
   ZH_USHORT uiPrevArea;
   ZH_ERRCODE errCode;

   memset( &dbTransInfo, 0, sizeof( dbTransInfo ) );
   uiPrevArea = ( ZH_AREANO ) zh_rddGetCurrentWorkAreaNumber();

   szDriver = zh_rddFindDrv( szDriver, szFileName );

   if( fExport )
   {
      errCode = zh_dbTransStruct( pArea, NULL, &dbTransInfo,
                                  &pStruct, pFields );
      if( errCode == ZH_SUCCESS )
      {
         errCode = zh_rddCreateTable( szFileName, szDriver, 0, "",
                                      ZH_TRUE,
                                      szCpId, ulConnection, pStruct, pDelim );
         if( errCode == ZH_SUCCESS )
            dbTransInfo.lpaDest = lpaClose =
                                 ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
      }
   }
   else
   {
      LPRDDNODE pRddNode = zh_rddFindNode( szDriver, NULL );

      if( ! pRddNode )
      {
         zh_errRT_DBCMD( EG_ARG, EDBCMD_USE_BADPARAMETER, NULL, ZH_ERR_FUNCNAME );
         return ZH_FAILURE;
      }

      if( pRddNode->uiType == RDD_REGISTER_TYPE_TRANSFER )
      {
         ZH_USHORT uiCount;

         errCode = zh_dbTransStruct( pArea, NULL, &dbTransInfo,
                                     &pStruct, pFields );

         /* revert area and items */
         dbTransInfo.lpaDest = dbTransInfo.lpaSource;
         for( uiCount = 0; uiCount < dbTransInfo.uiItemCount; ++uiCount )
         {
            ZH_USHORT uiSwap = dbTransInfo.lpTransItems[ uiCount ].uiSource;
            dbTransInfo.lpTransItems[ uiCount ].uiSource =
                                    dbTransInfo.lpTransItems[ uiCount ].uiDest;
            dbTransInfo.lpTransItems[ uiCount ].uiDest = uiSwap;
         }

         if( errCode == ZH_SUCCESS )
         {
            errCode = zh_rddOpenTable( szFileName, szDriver, 0, "", ZH_TRUE, ZH_TRUE,
                                       szCpId, ulConnection, pStruct, pDelim );
            if( errCode == ZH_SUCCESS )
            {
               lpaClose = dbTransInfo.lpaSource =
                                 ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
            }
         }
      }
      else
      {
         errCode = zh_rddOpenTable( szFileName, szDriver, 0, "", ZH_TRUE, ZH_TRUE,
                                    szCpId, ulConnection, NULL, pDelim );
         if( errCode == ZH_SUCCESS )
         {
            lpaClose = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
            errCode = zh_dbTransStruct( lpaClose, pArea, &dbTransInfo,
                                        NULL, pFields );
         }
      }
   }

   if( pStruct )
      zh_itemRelease( pStruct );

   if( errCode == ZH_SUCCESS )
   {
      PZH_ITEM pTransItm;

      zh_rddSelectWorkAreaNumber( dbTransInfo.lpaSource->uiArea );

      dbTransInfo.dbsci.itmCobFor   = pCobFor;
      dbTransInfo.dbsci.lpstrFor    = pStrFor;
      dbTransInfo.dbsci.itmCobWhile = pCobWhile;
      dbTransInfo.dbsci.lpstrWhile  = pStrWhile;
      dbTransInfo.dbsci.lNext       = pNext;
      dbTransInfo.dbsci.itmRecID    = pRecID;
      dbTransInfo.dbsci.fRest       = pRest;

      dbTransInfo.dbsci.fIgnoreFilter     = ZH_TRUE;
      dbTransInfo.dbsci.fIncludeDeleted   = ZH_TRUE;
      dbTransInfo.dbsci.fLast             = ZH_FALSE;
      dbTransInfo.dbsci.fIgnoreDuplicates = ZH_FALSE;
      dbTransInfo.dbsci.fBackward         = ZH_FALSE;

      pTransItm = zh_dbTransInfoPut( NULL, &dbTransInfo );
      errCode = SELF_INFO( dbTransInfo.lpaDest, DBI_TRANSREC, pTransItm );
      if( errCode == ZH_SUCCESS )
      {
         errCode = dbTransInfo.uiItemCount == 0 ? ZH_FAILURE :
                   SELF_TRANS( dbTransInfo.lpaSource, &dbTransInfo );
         /* we always call DBI_TRANSREC second time after TRANS() method
          * even if TRANS() failed - it's for RDDs which may need to store
          * pointer to dbTransInfo in first call and then release it and/or
          * clean some structures allocated for transfer operation [druzus]
          */
         SELF_INFO( dbTransInfo.lpaDest, DBI_TRANSREC, pTransItm );
         if( errCode == ZH_SUCCESS && ( dbTransInfo.uiFlags & DBTF_CPYCTR ) )
            errCode = zh_dbTransCounters( &dbTransInfo );
      }
      zh_itemRelease( pTransItm );
   }

   if( dbTransInfo.lpTransItems )
      zh_xfree( dbTransInfo.lpTransItems );
   if( lpaClose )
   {
      zh_rddSelectWorkAreaNumber( lpaClose->uiArea );
      zh_rddReleaseCurrentArea();
   }
   zh_rddSelectWorkAreaNumber( uiPrevArea );

   return errCode;
}

static ZH_ERRCODE zh_rddCloseParentRel( AREAP pArea, void * pChildArea )
{
   if( pArea->lpdbRelations )
   {
      LPDBRELINFO * lpdbRelationPtr = &pArea->lpdbRelations;
      ZH_USHORT uiArea = ( ( AREAP ) pChildArea )->uiArea;

      do
      {
         LPDBRELINFO lpdbRelation = *lpdbRelationPtr;

         if( lpdbRelation->lpaChild->uiArea == uiArea )
         {
            /* Clear this relation */
            zh_rddSelectWorkAreaNumber( lpdbRelation->lpaChild->uiArea );
            SELF_CHILDEND( lpdbRelation->lpaChild, lpdbRelation );
            if( lpdbRelation->itmCobExpr )
               zh_itemRelease( lpdbRelation->itmCobExpr );
            if( lpdbRelation->abKey )
               zh_itemRelease( lpdbRelation->abKey );

            *lpdbRelationPtr = lpdbRelation->lpdbriNext;
            zh_xfree( lpdbRelation );
         }
         else
            lpdbRelationPtr = &lpdbRelation->lpdbriNext;
      }
      while( *lpdbRelationPtr );
   }
   return ZH_SUCCESS;
}

/* close all parent relations */
ZH_ERRCODE zh_rddCloseAllParentRelations( AREAP pArea )
{
   ZH_ERRCODE errCode = ZH_SUCCESS;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_rddCloseAllParentRelations(%p)", ( void * ) pArea ) );

   if( pArea->uiParents > 0 )
   {
      ZH_USHORT uiArea = ( ZH_AREANO ) zh_rddGetCurrentWorkAreaNumber();
      errCode = zh_rddIterateWorkAreas( zh_rddCloseParentRel, pArea );
      zh_rddSelectWorkAreaNumber( uiArea );
   }

   return errCode;
}

static ZH_ERRCODE zh_rddEvalWABlock( AREAP pArea, void * pBlock )
{
   PZH_ITEM pItem;

   zh_rddSelectWorkAreaNumber( pArea->uiArea );
   pItem = zh_vmEvalBlockOrMacro( ( PZH_ITEM ) pBlock );

   if( zh_vmRequestQuery() != 0 ||
       ( ZH_IS_LOGICAL( pItem ) && ! zh_itemGetL( pItem ) ) )
      return ZH_FAILURE;
   else
      return ZH_SUCCESS;
}

ZH_ERRCODE zh_rddEvalWA( PZH_ITEM pBlock )
{
   ZH_ERRCODE errCode;
   ZH_USHORT uiArea;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_rddEvalWA(%p)", ( void * ) pBlock ) );

   uiArea = ( ZH_AREANO ) zh_rddGetCurrentWorkAreaNumber();
   errCode = zh_rddIterateWorkAreas( zh_rddEvalWABlock, pBlock );
   zh_rddSelectWorkAreaNumber( uiArea );

   return errCode;
}
