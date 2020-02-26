/*
 * PostgreSQL RDBMS low level (client api) interface code.
 *
 * Copyright 2003 Rodrigo Moreno rodrigo_moreno@yahoo.com
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#include "zh_pgsql.h"

#include "zh_error_api.h"
#include "zh_item_api.h"
#include "zh_rdd_api.h"
#include "zh_vm.h"
#include "zh_date.h"

typedef struct
{
   char *   buffer;
   int      position;
   int      length;
   ZH_BOOL  str_trim;
   PGconn * connection;
} pgCopyContext;

#define ZH_VM_UNLOCK()  do { zh_vmUnlock()
#define ZH_VM_LOCK()    zh_vmLock(); } while( 0 )

#if PG_VERSION_NUM >= 80000
static ZH_BOOL addToContext( pgCopyContext * context, const char c )
{
   if( context->position == context->length )
   {
      ZH_BOOL fOK;

      ZH_VM_UNLOCK();
      fOK = PQputCopyData( context->connection, context->buffer, context->position ) != -1;
      ZH_VM_LOCK();
      if( ! fOK )
         return ZH_FALSE;

      context->position = 0;
   }
   context->buffer[ context->position++ ] = ( ZH_BYTE ) c;

   return ZH_TRUE;
}

static ZH_BOOL addStrToContext( pgCopyContext * context, const char * str )
{
   while( *str )
   {
      if( context->position == context->length )
      {
         ZH_BOOL fOK;

         ZH_VM_UNLOCK();
         fOK = PQputCopyData( context->connection, context->buffer, context->position ) != -1;
         ZH_VM_LOCK();
         if( ! fOK )
            return ZH_FALSE;

         context->position = 0;
      }
      context->buffer[ context->position++ ] = ( ZH_BYTE ) *str++;
   }

   return ZH_TRUE;
}
static ZH_BOOL addStrnToContext( pgCopyContext * context, const char * str, ZH_SIZE size )
{
   ZH_SIZE nSize = 0;

   while( nSize < size )
   {
      if( context->position == context->length )
      {
         ZH_BOOL fOK;

         ZH_VM_UNLOCK();
         fOK = PQputCopyData( context->connection, context->buffer, context->position ) != -1;
         ZH_VM_LOCK();
         if( ! fOK )
            return ZH_FALSE;

         context->position = 0;
      }
      context->buffer[ context->position++ ] = ( ZH_BYTE ) str[ nSize++ ];
   }

   return ZH_TRUE;
}

/* Export field value into the buffer in PG accepted CSV format */
static ZH_BOOL exportBufSqlVar( pgCopyContext * context, PZH_ITEM pValue, const char * szQuote, const char * szEsc )
{
   switch( zh_itemType( pValue ) )
   {
      case ZH_IT_STRING:
      case ZH_IT_MEMO:
      {
         ZH_SIZE      nLen  = zh_itemGetCLen( pValue );
         ZH_SIZE      nCnt  = 0;
         const char * szVal = zh_itemGetCPtr( pValue );

         if( ! addStrToContext( context, szQuote ) )
            return ZH_FALSE;

         if( context->str_trim )
         {
            while( nLen && ZH_ISSPACE( szVal[ nLen - 1 ] ) )
               nLen--;
         }

         while( *szVal && nCnt++ < nLen )
         {
            if( ( ZH_UCHAR ) *szVal >= 32 )
            {
               /* if( *szVal == *szDelim || *szVal == *szEsc || *szVal == *szQuote )
                  we don't need to escape delim in CSV mode,
                  only the quote and the escape itself */

               if( *szVal == *szQuote || *szVal == *szEsc )
               {
                  if( ! addToContext( context, *szEsc ) )
                     return ZH_FALSE;
               }
               if( ! addToContext( context, *szVal ) )
                  return ZH_FALSE;
            }
            szVal++;
         }
         if( ! addStrToContext( context, szQuote ) )
            return ZH_FALSE;
         break;
      }

      case ZH_IT_DATE:
      {
         char szDate[ 9 ];

         if( ! addStrToContext( context, szQuote ) )
            return ZH_FALSE;
         zh_itemGetDS( pValue, szDate );
         if( szDate[ 0 ] == ' ' )
         {
            if( ! addStrToContext( context, "0100-01-01" ) )
               return ZH_FALSE;
         }
         else
         {
            if( ! addStrnToContext( context, &szDate[ 0 ], 4 ) ||
                ! addToContext( context, '-' ) ||
                ! addStrnToContext( context, &szDate[ 4 ], 2 ) ||
                ! addToContext( context, '-' ) ||
                ! addStrnToContext( context, &szDate[ 6 ], 2 ) )
               return ZH_FALSE;
         }
         if( ! addStrToContext( context, szQuote ) )
            return ZH_FALSE;
         break;
      }

      case ZH_IT_TIMESTAMP:
      {
         long lDate, lTime;
         char szDateTime[ 24 ];

         zh_itemGetTDT( pValue, &lDate, &lTime );
         zh_timeStampStr( szDateTime, lDate, lTime );
         if( ! addStrToContext( context, szQuote ) ||
             ! addStrToContext( context, szDateTime ) ||
             ! addStrToContext( context, szQuote ) )
            return ZH_FALSE;
         break;
      }

      case ZH_IT_LOGICAL:
         if( ! addToContext( context, zh_itemGetL( pValue ) ? 'Y' : 'N' ) )
            return ZH_FALSE;
         break;

      case ZH_IT_INTEGER:
      case ZH_IT_LONG:
      case ZH_IT_DOUBLE:
      {
         char szResult[ ZH_MAX_DOUBLE_LENGTH ];
         int  iSize, iWidth, iDec;

         zh_itemGetNLen( pValue, &iWidth, &iDec );
         iSize = ( iDec > 0 ? iWidth + 1 + iDec : iWidth );
         if( zh_itemStrBuf( szResult, pValue, iSize, iDec ) )
         {
            int iPos = 0;
            while( iSize && ZH_ISSPACE( szResult[ iPos ] ) )
            {
               iPos++;
               iSize--;
            }
            if( ! addStrnToContext( context, &szResult[ iPos ], iSize ) )
               return ZH_FALSE;
         }
         else if( ! addToContext( context, '0' ) )
            return ZH_FALSE;
         break;
      }
      /* an "M" field or the other, might be a "V" in SixDriver */
      default:
         return ZH_FALSE;
   }

   return ZH_TRUE;
}
#endif

ZH_FUNC( ZH_PQCOPYFROMWA )
{
#if PG_VERSION_NUM >= 80000
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
   PGconn * pConn = zh_PGconn_par( 1 );

   if( pConn == NULL )
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
   else if( pArea == NULL )
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, ZH_ERR_FUNCNAME );
   else
   {
      static const char * sc_szQuote = "\"";
      static const char * sc_szEsc   = "\"";
      static const char * sc_szDelim = ",";

      const char *    szTable   = zh_parcx( 2 );
      PZH_ITEM        pWhile    = zh_param( 3, ZH_IT_EVALITEM );
      PZH_ITEM        pFor      = zh_param( 4, ZH_IT_EVALITEM );
      PZH_ITEM        pFields   = zh_param( 5, ZH_IT_ARRAY );
      ZH_ULONG        nCount    = zh_parnldef( 6, 0 );
      ZH_BOOL         str_rtrim = zh_parldef( 7, ZH_TRUE );
      ZH_ULONG        nBufLen   = zh_parnldef( 8, 1 );
      ZH_USHORT       uiFields;
      ZH_ULONG        uiRecCount = 0;
      ZH_BOOL         bNoFieldPassed = ( pFields == NULL || zh_arrayLen( pFields ) == 0 );
      ZH_BOOL         bEof = ZH_FALSE;
      PZH_ITEM        pItem;
      ZH_USHORT       uiFieldCopy = 0;
      ZH_USHORT       uiIter;
      pgCopyContext * context;
      char *          szInit;
      char *          szFields = NULL;
      char *          szTmp;
      PGresult *      pgResult;
      ZH_BOOL         bFail    = ZH_FALSE;

      pItem = zh_itemNew( NULL );

      context = ( pgCopyContext * ) zh_xgrabz( sizeof( pgCopyContext ) );

      context->buffer     = ( char * ) zh_xgrab( sizeof( char ) * nBufLen * 1400 );
      context->position   = 0;
      context->length     = sizeof( char ) * nBufLen * 1400;
      context->str_trim   = str_rtrim;
      context->connection = pConn;

      SELF_FIELDCOUNT( pArea, &uiFields );

      if( ! bNoFieldPassed )
      {
         szFields      = ( char * ) zh_xgrab( sizeof( char ) * 2 );
         szFields[ 0 ] = '(';
         szFields[ 1 ] = '\0';
         uiFieldCopy   = ( ZH_USHORT ) zh_arrayLen( pFields );

         for( uiIter = 1; uiIter <= uiFieldCopy; uiIter++ )
         {
            const char * szFieldName = zh_arrayGetCPtr( pFields, uiIter );
            if( szFieldName )
            {
               int iPos = zh_rddFieldIndex( pArea, szFieldName );

               szTmp = zh_xstrcpy( NULL, szFields, szFieldName, NULL );
               zh_xfree( szFields );
               szFields = szTmp;
               if( uiIter != uiFieldCopy )
               {
                  szTmp = zh_xstrcpy( NULL, szFields, sc_szDelim, NULL );
                  zh_xfree( szFields );
                  szFields = szTmp;
               }

               if( iPos )
               {
                  zh_arraySetNI( pFields, uiIter, iPos );
                  continue;
               }
            }

            if( zh_arrayDel( pFields, uiIter ) )
            {
               zh_arraySize( pFields, zh_arrayLen( pFields ) - 1 );
               uiIter--;
               uiFieldCopy--;
            }
         }
         szTmp = zh_xstrcpy( NULL, szFields, ")", NULL );
         zh_xfree( szFields );
         szFields = szTmp;
      }

      if( szFields )
      {
         szInit = zh_xstrcpy( NULL, "COPY ", szTable, " ", szFields, " FROM STDIN WITH DELIMITER '", sc_szDelim, "' CSV  QUOTE AS '", sc_szQuote, "' ESCAPE AS '", sc_szEsc, "'", NULL );
         zh_xfree( szFields );
      }
      else
         szInit = zh_xstrcpy( NULL, "COPY ", szTable, " FROM STDIN WITH DELIMITER '", sc_szDelim, "' CSV  QUOTE AS '", sc_szQuote, "' ESCAPE AS '", sc_szEsc, "'", NULL );

      ZH_VM_UNLOCK();
      pgResult = PQexec( context->connection, szInit );
      if( PQresultStatus( pgResult ) != PGRES_COPY_IN )
         bFail = ZH_TRUE;
      PQclear( pgResult );
      zh_xfree( szInit );
      ZH_VM_LOCK();

      while( ! bFail && ( nCount == 0 || uiRecCount < nCount ) &&
             ( ! pWhile || zh_itemGetL( zh_vmEvalBlock( pWhile ) ) ) )
      {

         if( SELF_EOF( pArea, &bEof ) != ZH_SUCCESS )
            break;

         if( bEof )
            break;

         if( ! pFor || zh_itemGetL( zh_vmEvalBlock( pFor ) ) )
         {
            if( bNoFieldPassed )
            {
               for( uiIter = 1; uiIter <= uiFields; uiIter++ )
               {
                  if( SELF_GETVALUE( pArea, uiIter, pItem ) != ZH_SUCCESS ||
                      ! exportBufSqlVar( context, pItem, sc_szQuote, sc_szEsc ) ||
                      ! addStrToContext( context, uiIter == uiFields ? "\n" : sc_szDelim ) )
                  {
                     bFail = ZH_TRUE;
                     break;
                  }
               }
            }
            else
            {
               for( uiIter = 1; uiIter <= uiFieldCopy; uiIter++ )
               {
                  if( SELF_GETVALUE( pArea, ( ZH_USHORT ) zh_arrayGetNI( pFields, uiIter ), pItem ) != ZH_SUCCESS ||
                      ! exportBufSqlVar( context, pItem, sc_szQuote, sc_szEsc ) ||
                      ! addStrToContext( context, uiIter == uiFields ? "\n" : sc_szDelim ) )
                  {
                     bFail = ZH_TRUE;
                     break;
                  }
               }
            }

            if( bFail )
               break;

            uiRecCount++;
         }

         if( SELF_SKIP( pArea, 1 ) != ZH_SUCCESS )
            break;
      }

      if( ! bFail && ! addStrnToContext( context, "\\.\n", 3 ) ) /* end CSV transfer */
         bFail = ZH_TRUE;

      ZH_VM_UNLOCK();
      if( bFail )
         PQputCopyEnd( context->connection, "export buffer problems" );
      else if( PQputCopyData( context->connection, context->buffer, context->position ) == -1 ||
               PQputCopyEnd( context->connection, NULL ) == -1 )
         bFail = ZH_TRUE;
      else
      {
         while( ( pgResult = PQgetResult( context->connection ) ) )
         {
            if( PQresultStatus( pgResult ) != PGRES_COMMAND_OK )
               bFail = ZH_TRUE;
            PQclear( pgResult );
         }
      }
      ZH_VM_LOCK();

      zh_itemRelease( pItem );
      zh_xfree( context->buffer );
      zh_xfree( context );

      zh_retl( ! bFail );
   }
#else
   zh_retl( ZH_FALSE );
#endif
}
