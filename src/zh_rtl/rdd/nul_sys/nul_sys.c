/*
 * NULL RDD
 *
 * Copyright 2005 Przemyslaw Czerpak
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

ZH_ERRCODE zh_rddSelectWorkAreaAlias( const char * szName )
{
   ZH_SYMBOL_UNUSED( szName );

   return ZH_FAILURE;
}

ZH_ERRCODE zh_rddSelectWorkAreaNumber( int iArea )
{
   ZH_SYMBOL_UNUSED( iArea );

   return ZH_FAILURE;
}

ZH_ERRCODE zh_rddSelectWorkAreaSymbol( PZH_SYMBOL pSymAlias )
{
   ZH_SYMBOL_UNUSED( pSymAlias );

   return ZH_FAILURE;
}

int zh_rddGetCurrentWorkAreaNumber( void )
{
   return 0;
}

ZH_ERRCODE zh_rddFieldGet( PZH_ITEM pItem, PZH_SYMBOL pFieldSymbol )
{
   ZH_SYMBOL_UNUSED( pItem );
   ZH_SYMBOL_UNUSED( pFieldSymbol );

   return ZH_FAILURE;
}

ZH_ERRCODE zh_rddFieldPut( PZH_ITEM pItem, PZH_SYMBOL pFieldSymbol )
{
   ZH_SYMBOL_UNUSED( pItem );
   ZH_SYMBOL_UNUSED( pFieldSymbol );

   return ZH_FAILURE;
}

ZH_ERRCODE zh_rddGetFieldValue( PZH_ITEM pItem, PZH_SYMBOL pFieldSymbol )
{
   ZH_SYMBOL_UNUSED( pItem );
   ZH_SYMBOL_UNUSED( pFieldSymbol );

   return ZH_FAILURE;
}

ZH_ERRCODE zh_rddPutFieldValue( PZH_ITEM pItem, PZH_SYMBOL pFieldSymbol )
{
   ZH_SYMBOL_UNUSED( pItem );
   ZH_SYMBOL_UNUSED( pFieldSymbol );

   return ZH_FAILURE;
}

ZH_ERRCODE zh_rddGetAliasNumber( const char * szAlias, int * iArea )
{
   ZH_SYMBOL_UNUSED( szAlias );
   ZH_SYMBOL_UNUSED( iArea );

   return ZH_FAILURE;
}

void zh_rddCloseAll( void ) {}

void zh_rddShutDown( void ) {}


ZH_FUNC( RDDSYS ) {}

ZH_FUNC( RDDNAME ) { zh_retc_null(); }

ZH_FUNC( RDDLIST ) { zh_reta( 0 ); }

ZH_FUNC( FIELDGET ) { zh_retc_null(); }

ZH_FUNC( FIELDPUT ) { zh_retc_null(); }

ZH_FUNC( FIELDPOS ) { zh_retni( 0 ); }

ZH_FUNC( FIELDNAME ) { zh_retc_null(); }

ZH_FUNC( DBCREATE ) {}

ZH_FUNC( DBUSEAREA ) {}

ZH_FUNC( DBCLOSEAREA ) {}

ZH_FUNC( DBSELECTAREA ) {}

ZH_FUNC( DBSTRUCT ) {}

ZH_FUNC( DBGOTO ) { zh_retni( 0 ); }

ZH_FUNC( DBGOTOP ) {}

ZH_FUNC( DBGOBOTTOM ) {}

ZH_FUNC( DBSEEK ) { zh_retl( ZH_FALSE ); }

ZH_FUNC( DBSKIP ) { zh_retni( 0 ); }

ZH_FUNC( DBAPPEND ) {}

ZH_FUNC( DBRECALL ) {}

ZH_FUNC( DBDELETE ) {}

ZH_FUNC( DBRLOCK ) { zh_retl( ZH_FALSE ); }

ZH_FUNC( DBUNLOCK ) { zh_retl( ZH_FALSE ); }

ZH_FUNC( DBRELATION ) { zh_retc_null(); }

ZH_FUNC( DBRSELECT ) { zh_retni( 0 ); }

ZH_FUNC( DBFILTER ) { zh_retc_null(); }

ZH_FUNC( DBEVAL ) {}

ZH_FUNC( SELECT ) { zh_retni( 0 ); }

ZH_FUNC( ALIAS ) { zh_retc_null(); }

ZH_FUNC( USED ) { zh_retl( ZH_FALSE ); }

ZH_FUNC( NETERR ) { zh_retl( ZH_FALSE ); }

ZH_FUNC( LOCK ) { zh_retl( ZH_FALSE ); }

ZH_FUNC( FLOCK ) { zh_retl( ZH_FALSE ); }

ZH_FUNC( RLOCK ) { zh_retl( ZH_FALSE ); }

ZH_FUNC( BOF ) { zh_retl( ZH_FALSE ); }

ZH_FUNC( EOF ) { zh_retl( ZH_FALSE ); }

ZH_FUNC( FOUND ) { zh_retl( ZH_FALSE ); }

ZH_FUNC( DELETED ) { zh_retl( ZH_FALSE ); }

ZH_FUNC( RECNO ) { zh_retni( 0 ); }

ZH_FUNC( RECCOUNT ) { zh_parni( 0 ); }

ZH_FUNC( LASTREC ) { zh_retni( 0 ); }

ZH_FUNC( FCOUNT ) { zh_parni( 0 ); }

ZH_FUNC( RECSIZE ) { zh_retni( 0 ); }

ZH_FUNC( HEADER ) { zh_retni( 0 ); }

ZH_FUNC( LUPDATE ) { zh_retds( NULL ); }


ZH_FUNC( INDEXORD ) { zh_parni( 1 ); }

ZH_FUNC( INDEXKEY ) { zh_retc_null(); }

ZH_FUNC( ORDNAME ) { zh_retc_null(); }

ZH_FUNC( ORDKEY ) { zh_retc_null(); }

ZH_FUNC( ORDFOR ) { zh_retc_null(); }


ZH_FUNC( ZH_DBDROP ) { zh_retl( ZH_FALSE ); }

ZH_FUNC( ZH_DBEXISTS ) { zh_retl( ZH_FALSE ); }

ZH_FUNC( ZH_DBRENAME ) { zh_retl( ZH_FALSE ); }

ZH_FUNC( ZH_FIELDLEN ) { zh_retni( 0 ); }

ZH_FUNC( ZH_FIELDDEC ) { zh_retni( 0 ); }

ZH_FUNC( ZH_FIELDTYPE ) { zh_retc_null(); }

ZH_FUNC( ZH_WAEVAL ) { zh_retc_null(); }
