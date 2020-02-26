/*
 * PostgreSQL RDBMS low level (client api) interface code.
 *
 * Copyright 2010 Viktor Szakats (vszakats.net/ziher) (GC support)
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
#include "zh_api.h"
#include <errno.h>

static ZH_GARBAGE_FUNC( PGconn_release )
{
   void ** ph = ( void ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ph && *ph )
   {
      /* Destroy the object */
      PQfinish( ( PGconn * ) *ph );

      /* set pointer to NULL to avoid multiple freeing */
      *ph = NULL;
   }
}

static const ZH_GC_FUNCS s_gcPGconnFuncs =
{
   PGconn_release,
   zh_gcDummyMark
};

void zh_PGconn_ret( PGconn * p )
{
   if( p )
   {
      void ** ph = ( void ** ) zh_gcAllocate( sizeof( PGconn * ), &s_gcPGconnFuncs );

      *ph = p;

      zh_retptrGC( ph );
   }
   else
      zh_retptr( NULL );
}

PGconn * zh_PGconn_par( int iParam )
{
   void ** ph = ( void ** ) zh_parptrGC( &s_gcPGconnFuncs, iParam );

   return ph ? ( PGconn * ) *ph : NULL;
}

static ZH_GARBAGE_FUNC( PGresult_release )
{
   void ** ph = ( void ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ph && *ph )
   {
      /* Destroy the object */
      PQclear( ( PGresult * ) *ph );

      /* set pointer to NULL to avoid multiple freeing */
      *ph = NULL;
   }
}

static const ZH_GC_FUNCS s_gcPGresultFuncs =
{
   PGresult_release,
   zh_gcDummyMark
};

void zh_PGresult_ret( PGresult * p )
{
   if( p )
   {
      void ** ph = ( void ** ) zh_gcAllocate( sizeof( PGresult * ), &s_gcPGresultFuncs );

      *ph = p;

      zh_retptrGC( ph );
   }
   else
      zh_retptr( NULL );
}

PGresult * zh_PGresult_par( int iParam )
{
   void ** ph = ( void ** ) zh_parptrGC( &s_gcPGresultFuncs, iParam );

   return ph ? ( PGresult * ) *ph : NULL;
}

#if PG_VERSION_NUM >= 80000

static ZH_GARBAGE_FUNC( PGcancel_release )
{
   void ** ph = ( void ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ph && *ph )
   {
      /* Destroy the object */
      PQfreeCancel( ( PGcancel * ) *ph );

      /* set pointer to NULL to avoid multiple freeing */
      *ph = NULL;
   }
}

static const ZH_GC_FUNCS s_gcPGcancelFuncs =
{
   PGcancel_release,
   zh_gcDummyMark
};

static void zh_PGcancel_ret( PGcancel * p )
{
   if( p )
   {
      void ** ph = ( void ** ) zh_gcAllocate( sizeof( PGcancel * ), &s_gcPGcancelFuncs );

      *ph = p;

      zh_retptrGC( ph );
   }
   else
      zh_retptr( NULL );
}

static PGcancel * zh_PGcancel_par( int iParam )
{
   void ** ph = ( void ** ) zh_parptrGC( &s_gcPGcancelFuncs, iParam );

   return ph ? ( PGcancel * ) *ph : NULL;
}

#endif

#ifdef NODLL

static ZH_GARBAGE_FUNC( FILE_release )
{
   void ** ph = ( void ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ph && *ph )
   {
      /* Destroy the object */
      fclose( ( FILE * ) *ph );

      /* set pointer to NULL to avoid multiple freeing */
      *ph = NULL;
   }
}

static const ZH_GC_FUNCS s_gcFILEFuncs =
{
   FILE_release,
   zh_gcDummyMark
};

static void zh_FILE_ret( FILE * p )
{
   if( p )
   {
      void ** ph = ( void ** ) zh_gcAllocate( sizeof( FILE * ), &s_gcFILEFuncs );

      *ph = p;

      zh_retptrGC( ph );
   }
   else
      zh_retptr( NULL );
}

static FILE * zh_FILE_par( int iParam )
{
   void ** ph = ( void ** ) zh_parptrGC( &s_gcFILEFuncs, iParam );

   return ph ? ( FILE * ) *ph : NULL;
}

#endif

/*
 * Connection handling functions
 */

ZH_FUNC( PQCONNECTDB )
{
   if( ZH_ISCHAR( 1 ) )
      zh_PGconn_ret( PQconnectdb( zh_parc( 1 ) ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/*
static void
processNotice(void *arg, const char *message)
{
   // UNUSED(arg);
    fprintf( stderr, "NOTIFY_PORUKA: %s %s\n", message);
}

// https://github.com/afiskon/c-libpq-example/blob/master/libpq_example.c

ZH_FUNC( PQNOTICEPROCESSOR )
{

   PGconn * conn = zh_PGconn_par( 1 ); 

   if( conn ) {
      PQsetNoticeProcessor(conn, processNotice, NULL);
      fprintf( stderr, "NOTIFY PROCESOR OK\n");
   } else
      fprintf( stderr, "NOTIFY PROCESOR CONN ERROR\n");
   //   zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}
*/

ZH_FUNC ( PQRECEIVE )
{
   PGnotify  *notify;
   PGconn * conn = zh_PGconn_par( 1 );

/*
   int         sock;
   fd_set      input_mask;
   struct timeval timeout;

   sock = PQsocket(conn);

   if (sock < 0)
      return;

   FD_ZERO(&input_mask);
   FD_SET(sock, &input_mask);

   timeout.tv_sec = 10;
   timeout.tv_usec = 0;

   if (select(sock + 1, &input_mask, NULL, NULL, &timeout) < 0)
   {
      fprintf(stderr, "select() failed: %s\n", strerror(errno));
      return;
   }
*/

   if (PQisBusy(conn)) {
      zh_ret(); // RETURN NIL
      return;
   }

   /* Now check for input */
   PQconsumeInput(conn);
   // while ((notify = PQnotifies(conn)) != NULL)
   if ((notify = PQnotifies(conn)) != NULL)
   {
      fprintf(stderr,
         "ASYNC NOTIFY of '%s' : payload: '%s' received from backend PID %d\n",
                    notify->relname, notify->extra, notify->be_pid);
   
      // zh_arraySetNI( aInfo, 1, cf.iPointSize );
      // zh_arraySetNInt( aInfo, 2, cf.rgbColors  );

      PZH_ITEM pRet = zh_itemArrayNew( 3 );

      zh_arraySetC(  pRet, 1, notify->relname );
      zh_arraySetC(  pRet, 2, notify->extra );
      zh_arraySetNInt( pRet, 3, notify->be_pid  );
      
      PQfreemem(notify);

      zh_itemReturnRelease( pRet );
      // break;
   } else
      zh_ret();

   // fprintf(stderr, "kraj saslusanja\n");
   
   
}



/* NOTE: Deprecated */
ZH_FUNC( PQSETDBLOGIN )
{
   zh_PGconn_ret( PQsetdbLogin( zh_parcx( 1 ) /* pghost */,
                                zh_parcx( 2 ) /* pgport */,
                                zh_parcx( 3 ) /* pgoptions */,
                                zh_parcx( 4 ) /* pgtty */,
                                zh_parcx( 5 ) /* dbName */,
                                zh_parcx( 6 ) /* login */,
                                zh_parcx( 7 ) /* pwd */ ) );
}

ZH_FUNC( PQRESET )
{
   PGconn * conn = zh_PGconn_par( 1 );

   if( conn )
      PQreset( conn );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQPROTOCOLVERSION )
{
   PGconn * conn = zh_PGconn_par( 1 );

   if( conn )
      zh_retni( PQprotocolVersion( conn ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQCLIENTENCODING )
{
   PGconn * conn = zh_PGconn_par( 1 );

   if( conn )
      zh_retni( PQclientEncoding( conn ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQSETCLIENTENCODING )
{
   PGconn * conn = zh_PGconn_par( 1 );

   if( conn )
      zh_retni( PQsetClientEncoding( conn, zh_parcx( 2 ) ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQDB )
{
   PGconn * conn = zh_PGconn_par( 1 );

   if( conn )
      zh_retc( PQdb( conn ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQUSER )
{
   PGconn * conn = zh_PGconn_par( 1 );

   if( conn )
      zh_retc( PQuser( conn ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQPASS )
{
   PGconn * conn = zh_PGconn_par( 1 );

   if( conn )
      zh_retc( PQpass( conn ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQHOST )
{
   PGconn * conn = zh_PGconn_par( 1 );

   if( conn )
      zh_retc( PQhost( conn ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQPORT )
{
   PGconn * conn = zh_PGconn_par( 1 );

   if( conn )
      zh_retc( PQport( conn ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQTTY )
{
   PGconn * conn = zh_PGconn_par( 1 );

   if( conn )
      zh_retc( PQtty( conn ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQOPTIONS )
{
   PGconn * conn = zh_PGconn_par( 1 );

   if( conn )
      zh_retc( PQoptions( conn ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQTRANSACTIONSTATUS )
{
   PGconn * conn = zh_PGconn_par( 1 );

   if( conn )
      zh_retni( PQtransactionStatus( conn ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQERRORMESSAGE )
{
   PGconn * conn = zh_PGconn_par( 1 );

   if( conn )
      zh_retc( PQerrorMessage( conn ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQRESULTERRORFIELD )
{
#if PG_VERSION_NUM >= 70400
   PGresult * res = zh_PGresult_par( 1 );

   if( res )
      zh_retc( PQresultErrorField( res, zh_parni( 2 ) ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
#else
   zh_retc_null();
#endif
}

ZH_FUNC( PQRESSTATUS )
{
   zh_retc( PQresStatus( ( ExecStatusType ) zh_parnl( 1 ) ) );
}

ZH_FUNC( PQSTATUS )
{
   PGconn * conn = zh_PGconn_par( 1 );

   if( conn )
      zh_retni( PQstatus( conn ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/*
 * Query handling functions
 */

ZH_FUNC( PQEXEC )
{
   PGconn * conn = zh_PGconn_par( 1 );

   if( conn )
      zh_PGresult_ret( PQexec( conn, zh_parcx( 2 ) ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQEXECPARAMS )
{
   PGconn * conn   = zh_PGconn_par( 1 );
   PZH_ITEM aParam = zh_param( 3, ZH_IT_ARRAY );

   if( conn && aParam )
   {
      int n = ( int ) zh_arrayLen( aParam );
      int i;

      const char ** paramvalues = ( const char ** ) zh_xgrab( sizeof( char * ) * n );

      for( i = 0; i < n; ++i )
         paramvalues[ i ] = zh_arrayGetCPtr( aParam, i + 1 );

      zh_PGresult_ret( PQexecParams( conn, zh_parcx( 2 ), n, NULL, paramvalues, NULL, NULL, 1 ) );

      zh_xfree( ( void * ) paramvalues );
   }
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQFCOUNT )  /* not a direct wrapper */
{
   PGresult * res = zh_PGresult_par( 1 );

   if( res )
   {
      int nFields = 0;

      if( PQresultStatus( res ) == PGRES_TUPLES_OK )
         nFields = PQnfields( res );

      zh_retni( nFields );
   }
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQLASTREC )  /* not a direct wrapper */
{
   PGresult * res = zh_PGresult_par( 1 );

   if( res )
   {
      int nRows = 0;

      if( PQresultStatus( res ) == PGRES_TUPLES_OK )
         nRows = PQntuples( res );

      zh_retni( nRows );
   }
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQGETVALUE )
{
   PGresult * res = zh_PGresult_par( 1 );

   if( res )
   {
      if( PQresultStatus( res ) == PGRES_TUPLES_OK )
      {
         int nRow = zh_parni( 2 ) - 1;
         int nCol = zh_parni( 3 ) - 1;

         if( ! PQgetisnull( res, nRow, nCol ) )
            zh_retc( PQgetvalue( res, nRow, nCol ) );
         else
            zh_ret();
      }
      else
         zh_ret();
   }
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQGETLENGTH )
{
   PGresult * res = zh_PGresult_par( 1 );

   if( res )
   {
      int result = 0;

      if( PQresultStatus( res ) == PGRES_TUPLES_OK )
      {
         int nRow = zh_parni( 2 ) - 1;
         int nCol = zh_parni( 3 ) - 1;

         result = PQgetlength( res, nRow, nCol );
      }

      zh_retni( result );
   }
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/* PQmetadata() positions for array returned */
#define ZHPG_META_FIELDNAME  1
#define ZHPG_META_FIELDTYPE  2
#define ZHPG_META_FIELDLEN   3
#define ZHPG_META_FIELDDEC   4
#define ZHPG_META_TABLE      5
#define ZHPG_META_TABLECOL   6
#define ZHPG_META_LEN_       6

ZH_FUNC( PQMETADATA )  /* not a direct wrapper */
{
   PGresult * res = zh_PGresult_par( 1 );

   if( res )
   {
      if( PQresultStatus( res ) == PGRES_TUPLES_OK )
      {
         int      nFields = PQnfields( res ), i;
         PZH_ITEM pResult = zh_itemArrayNew( nFields );

         for( i = 0; i < nFields; i++ )
         {
            char buf[ 256 ];
            int  typemod = PQfmod( res, i );
            int  length  = 0;
            int  decimal = 0;

            PZH_ITEM pField;

            switch( PQftype( res, i ) )
            {
               case BITOID:
                  if( typemod >= 0 )
                     length = ( int ) typemod;
                  zh_strncpy( buf, "bit", sizeof( buf ) - 1 );
                  break;

               case BOOLOID:
                  length = 1;
                  zh_strncpy( buf, "boolean", sizeof( buf ) - 1 );
                  break;

               case BPCHAROID:
                  if( typemod >= 0 )
                     length = ( int ) ( typemod - VARHDRSZ );
                  zh_strncpy( buf, "character", sizeof( buf ) - 1 );
                  break;

               case FLOAT4OID:
                  zh_strncpy( buf, "real", sizeof( buf ) - 1 );
                  break;

               case FLOAT8OID:
               case UUIDOID:
                  zh_strncpy( buf, "double precision", sizeof( buf ) - 1 );
                  break;

               case INT2OID:
                  zh_strncpy( buf, "smallint", sizeof( buf ) - 1 );
                  break;

               case INT4OID:
                  zh_strncpy( buf, "integer", sizeof( buf ) - 1 );
                  break;

               case INT8OID:
               case OIDOID:
                  zh_strncpy( buf, "bigint", sizeof( buf ) - 1 );
                  break;

               case NUMERICOID:
                  length  = ( ( typemod - VARHDRSZ ) >> 16 ) & 0xffff;
                  decimal = ( typemod - VARHDRSZ ) & 0xffff;
                  zh_strncpy( buf, "numeric", sizeof( buf ) - 1 );
                  break;

               case DATEOID:
                  zh_strncpy( buf, "date", sizeof( buf ) - 1 );
                  break;

               case TIMEOID:
               case TIMETZOID:
                  zh_strncpy( buf, "timezone", sizeof( buf ) - 1 );
                  break;

               case TIMESTAMPOID:
               case TIMESTAMPTZOID:
                  zh_strncpy( buf, "timestamp", sizeof( buf ) - 1 );
                  break;

               case VARBITOID:
                  if( typemod >= 0 )
                     length = ( int ) typemod;
                  zh_strncpy( buf, "bit varying", sizeof( buf ) - 1 );
                  break;

               case VARCHAROID:
                  if( typemod >= 0 )
                     length = ( int ) ( typemod - VARHDRSZ );
                  zh_strncpy( buf, "character varying", sizeof( buf ) - 1 );
                  break;

               case TEXTOID:
                  zh_strncpy( buf, "text", sizeof( buf ) - 1 );
                  break;

               case CASHOID:
                  zh_strncpy( buf, "money", sizeof( buf ) - 1 );
                  break;

               case NAMEOID:
                  zh_strncpy( buf, "name", sizeof( buf ) - 1 );
                  break;

               default:
                  zh_strncpy( buf, "not supported", sizeof( buf ) - 1 );
                  break;
            }

            pField = zh_arrayGetItemPtr( pResult, i + 1 );
            zh_arrayNew( pField, ZHPG_META_LEN_ );
            zh_arraySetC(  pField, ZHPG_META_FIELDNAME, PQfname( res, i ) );
            zh_arraySetC(  pField, ZHPG_META_FIELDTYPE, buf );
            zh_arraySetNI( pField, ZHPG_META_FIELDLEN, length );
            zh_arraySetNI( pField, ZHPG_META_FIELDDEC, decimal );
            zh_arraySetNL( pField, ZHPG_META_TABLE, PQftable( res, i ) );
            zh_arraySetNI( pField, ZHPG_META_TABLECOL, PQftablecol( res, i ) );
         }

         zh_itemReturnRelease( pResult );
      }
      else
         zh_reta( 0 );
   }
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQRESULT2ARRAY )  /* not a direct wrapper */
{
   PGresult * res = zh_PGresult_par( 1 );

   if( res )
   {
      if( PQresultStatus( res ) == PGRES_TUPLES_OK )
      {
         int nRows = PQntuples( res ), nRow;
         int nCols = PQnfields( res ), nCol;

         PZH_ITEM pResult = zh_itemArrayNew( nRows );

         for( nRow = 0; nRow < nRows; nRow++ )
         {
            PZH_ITEM pRow = zh_arrayGetItemPtr( pResult, nRow + 1 );
            zh_arrayNew( pRow, nCols );
            for( nCol = 0; nCol < nCols; nCol++ )
               zh_arraySetC( pRow, nCol + 1, PQgetvalue( res, nRow, nCol ) );
         }

         zh_itemReturnRelease( pResult );
      }
      else
         zh_reta( 0 );
   }
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQRESULTERRORMESSAGE )
{
   PGresult * res = zh_PGresult_par( 1 );

   if( res )
      zh_retc( PQresultErrorMessage( res ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQRESULTSTATUS )
{
   PGresult * res = zh_PGresult_par( 1 );

   if( res )
      zh_retni( PQresultStatus( res ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQCMDSTATUS )
{
   PGresult * res = zh_PGresult_par( 1 );

   if( res )
      zh_retc( PQcmdStatus( res ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQCMDTUPLES )
{
   PGresult * res = zh_PGresult_par( 1 );

   if( res )
      zh_retc( PQcmdTuples( res ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQESCAPESTRING )
{
   const char * source = zh_parcx( 1 );
   ZH_SIZE      size   = strlen( source );
   char *       dest   = ( char * ) zh_xgrab( size * 2 + 1 );

   PQescapeString( dest, source, ( size_t ) size );

   zh_retc_buffer( dest );
}

ZH_FUNC( PQESCAPEBYTEA ) /* deprecated */
{
   if( ZH_ISCHAR( 1 ) )
   {
      size_t from_length = ( size_t ) zh_parclen( 1 );
      size_t to_length   = from_length * 5 + 1;
      unsigned char * to = PQescapeBytea( ( const unsigned char * ) zh_parc( 1 ), from_length, &to_length );
      zh_retclen( ( char * ) to, ( ZH_SIZE ) to_length );
      PQfreemem( to );
   }
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQUNESCAPEBYTEA )
{
   if( ZH_ISCHAR( 1 ) )
   {
      size_t to_length;
      unsigned char * from = PQunescapeBytea( ( const unsigned char * ) zh_parc( 1 ), &to_length );
      zh_retclen( ( char * ) from, ( ZH_SIZE ) to_length );
      PQfreemem( from );
   }
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQOIDVALUE )
{
   PGresult * res = zh_PGresult_par( 1 );

   if( res )
      zh_retnl( ( Oid ) PQoidValue( res ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQOIDSTATUS )
{
   PGresult * res = zh_PGresult_par( 1 );

   if( res )
      zh_retc( PQoidStatus( res ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQBINARYTUPLES )
{
   PGresult * res = zh_PGresult_par( 1 );

   if( res )
      zh_retl( PQbinaryTuples( res ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQFTABLE )
{
   PGresult * res = zh_PGresult_par( 1 );

   if( res )
      zh_retnl( ( Oid ) PQftable( res, zh_parni( 2 ) - 1 ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQFTYPE )
{
   PGresult * res = zh_PGresult_par( 1 );

   if( res )
      zh_retnl( ( Oid ) PQftype( res, zh_parni( 2 ) - 1 ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQFNAME )
{
   PGresult * res = zh_PGresult_par( 1 );

   if( res )
      zh_retc( PQfname( res, zh_parni( 2 ) - 1 ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQFMOD )
{
   PGresult * res = zh_PGresult_par( 1 );

   if( res )
      zh_retni( PQfmod( res, zh_parni( 2 ) - 1 ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQFSIZE )
{
   PGresult * res = zh_PGresult_par( 1 );

   if( res )
      zh_retni( PQfsize( res, zh_parni( 2 ) - 1 ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQGETISNULL )
{
   PGresult * res = zh_PGresult_par( 1 );

   if( res )
      zh_retl( PQgetisnull( res, zh_parni( 2 ) - 1, zh_parni( 3 ) - 1 ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQFNUMBER )
{
   PGresult * res = zh_PGresult_par( 1 );

   if( res )
      zh_retni( PQfnumber( res, zh_parcx( 2 ) ) + 1 );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQNTUPLES )
{
   PGresult * res = zh_PGresult_par( 1 );

   if( res )
      zh_retnl( PQntuples( res ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQNFIELDS )
{
   PGresult * res = zh_PGresult_par( 1 );

   if( res )
      zh_retnl( PQnfields( res ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/*
 * Asynchronous functions
 */

ZH_FUNC( PQSENDQUERY )
{
   PGconn * conn = zh_PGconn_par( 1 );

   if( conn )
      zh_retl( PQsendQuery( conn, zh_parcx( 2 ) ) ? ZH_TRUE : ZH_FALSE );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQGETRESULT )
{
   PGconn * conn = zh_PGconn_par( 1 );

   if( conn )
      zh_PGresult_ret( PQgetResult( conn ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQCONSUMEINPUT )
{
   PGconn * conn = zh_PGconn_par( 1 );

   if( conn )
      zh_retl( PQconsumeInput( conn ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQISBUSY )
{
   PGconn * conn = zh_PGconn_par( 1 );

   if( conn )
      zh_retl( PQisBusy( conn ) ? ZH_TRUE : ZH_FALSE );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQREQUESTCANCEL ) /* deprecated */
{
   PGconn * conn = zh_PGconn_par( 1 );

   if( conn )
      zh_retl( PQrequestCancel( conn ) ? ZH_TRUE : ZH_FALSE );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQFLUSH )
{
   PGconn * conn = zh_PGconn_par( 1 );

   if( conn )
      zh_retni( PQflush( conn ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQSETNONBLOCKING )
{
   PGconn * conn = zh_PGconn_par( 1 );

   if( conn )
      zh_retl( PQsetnonblocking( conn, zh_parl( 2 ) ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQISNONBLOCKING )
{
   PGconn * conn = zh_PGconn_par( 1 );

   if( conn )
      zh_retl( PQisnonblocking( conn ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/*
 * Trace Connection handling functions
 */

ZH_FUNC( PQTRACECREATE )  /* not a direct wrapper */
{
#ifdef NODLL
   zh_FILE_ret( zh_fopen( zh_parcx( 1 ), "w+b" ) );
#else
   zh_retptr( NULL );
#endif
}

ZH_FUNC( PQTRACE )
{
#ifdef NODLL
   PGconn * conn   = zh_PGconn_par( 1 );
   FILE *   trfile = zh_FILE_par( 2 );

   if( conn && trfile )
      PQtrace( conn, trfile );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
#endif
}

ZH_FUNC( PQUNTRACE )
{
   PGconn * conn = zh_PGconn_par( 1 );

   if( conn )
      PQuntrace( conn );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/* PQERRORS_TERSE   0
   PQERRORS_DEFAULT 1
   PQERRORS_VERBOSE 2
 */
ZH_FUNC( PQSETERRORVERBOSITY )
{
   PGconn * conn = zh_PGconn_par( 1 );

   if( conn )
      zh_retni( ( PGVerbosity ) PQsetErrorVerbosity( conn, ( PGVerbosity ) zh_parni( 2 ) ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/*
 * Large Object functions
 */

ZH_FUNC( LO_IMPORT )
{
   PGconn * conn = zh_PGconn_par( 1 );

   if( conn )
      zh_retni( lo_import( conn, zh_parcx( 2 ) ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( LO_EXPORT )
{
   PGconn * conn = zh_PGconn_par( 1 );

   if( conn )
      zh_retl( lo_export( conn, ( Oid ) zh_parnl( 2 ), zh_parcx( 3 ) ) == 1 );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( LO_UNLINK )
{
   PGconn * conn = zh_PGconn_par( 1 );

   if( conn )
      zh_retl( lo_unlink( conn, ( Oid ) zh_parnl( 2 ) ) == 1 );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQSERVERVERSION )
{
#if PG_VERSION_NUM >= 80000
   PGconn * conn = zh_PGconn_par( 1 );

   if( conn )
      zh_retni( PQserverVersion( conn ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
#else
   zh_retni( 0 );
#endif
}

ZH_FUNC( PQGETCANCEL )
{
#if PG_VERSION_NUM >= 80000
   PGconn * conn = zh_PGconn_par( 1 );

   if( conn )
      zh_PGcancel_ret( PQgetCancel( conn ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
#else
   zh_retptr( NULL );
#endif
}

ZH_FUNC( PQCANCEL )
{
#if PG_VERSION_NUM >= 80000
   PGcancel * cancel = zh_PGcancel_par( 1 );

   if( cancel )
   {
      char errbuf[ 256 ];

      errbuf[ 0 ] = '\0';

      zh_retl( PQcancel( cancel, errbuf, sizeof( errbuf ) - 1 ) == 1 );

      zh_storc( errbuf, 2 );
   }
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
#else
   zh_retl( ZH_FALSE );
   zh_storc( NULL, 2 );
#endif
}

ZH_FUNC( PQESCAPEBYTEACONN )
{
#if PG_VERSION_NUM >= 80000
   PGconn * conn = zh_PGconn_par( 1 );

   if( conn && ZH_ISCHAR( 2 ) )
   {
      size_t from_length = zh_parclen( 2 );
      size_t to_length   = from_length * 5 + 1;

      unsigned char * to = PQescapeByteaConn( conn, ( unsigned const char * ) zh_parc( 2 ), from_length, &to_length );
      zh_retclen( ( char * ) to, ( ZH_SIZE ) to_length );
      PQfreemem( to );
   }
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
#else
   zh_retc_null();
#endif
}

ZH_FUNC( PQPREPARE )
{
   PGconn * conn = zh_PGconn_par( 1 );

   if( conn )
      zh_PGresult_ret( PQprepare( conn, zh_parcx( 2 ), zh_parcx( 3 ), zh_parni( 4 ), NULL ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQEXECPREPARED )
{
   PGconn * conn = zh_PGconn_par( 1 );

   if( conn )
   {
      PZH_ITEM aParam = zh_param( 3, ZH_IT_ARRAY );
      ZH_SIZE  n      = zh_arrayLen( aParam );
      ZH_SIZE  i;

      const char ** paramvalues = ( const char ** ) zh_xgrab( sizeof( char * ) * n );

      for( i = 0; i < n; ++i )
         paramvalues[ i ] = zh_arrayGetCPtr( aParam, i + 1 );

      zh_PGresult_ret( PQexecPrepared( conn, zh_parcx( 2 ), ( int ) n, ( const char * const * ) paramvalues, NULL, NULL, 1 ) );

      zh_xfree( ( void * ) paramvalues );
   }
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( PQPUTCOPYDATA )
{
#if PG_VERSION_NUM >= 80000
   PGconn * conn = zh_PGconn_par( 1 );

   if( conn )
      zh_retni( PQputCopyData( conn, zh_parcx( 2 ), ( int ) zh_parclen( 2 ) ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
#else
   zh_retc_null();
#endif
}

ZH_FUNC( PQPUTCOPYEND )
{
#if PG_VERSION_NUM >= 80000
   PGconn * conn = zh_PGconn_par( 1 );

   if( conn )
      zh_retni( PQputCopyEnd( conn, NULL ) );
   else
      zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
#else
   zh_retc_null();
#endif
}

ZH_FUNC( PG_ENCODING_TO_CHAR )
{
   zh_retc( pg_encoding_to_char( zh_parni( 1 ) ) );
}

#if 0

 TODO: Implement Full Large Objects Support
 TODO: Implement Prepared Query handling

extern int lo_open( PGconn * conn, Oid lobjId, int mode );
extern int lo_close( PGconn * conn, int fd );
extern int lo_read( PGconn * conn, int fd, char * buf, size_t len );
extern int lo_write( PGconn * conn, int fd, char * buf, size_t len );
extern int lo_lseek( PGconn * conn, int fd, int offset, int whence );
extern Oid lo_creat( PGconn * conn, int mode );
extern int lo_tell( PGconn * conn, int fd );

int PQsendQueryParams( PGconn * conn,
                       const char * command,
                       int nParams,
                       const Oid * paramTypes,
                       const char * const * paramValues,
                       const int * paramLengths,
                       const int * paramFormats,
                       int resultFormat );

int PQsendPrepare( PGconn * conn,
                   const char * stmtName,
                   const char * query,
                   int nParams,
                   const Oid * paramTypes );

int PQsendQueryPrepared( PGconn * conn,
                         const char * stmtName,
                         int nParams,
                         const char * const * paramValues,
                         const int * paramLengths,
                         const int * paramFormats,
                         int resultFormat );

#endif
