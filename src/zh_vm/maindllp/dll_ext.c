/*
 * Windows pcode DLL entry point and VM/RTL routing functions
 *
 * Copyright 2001 Antonio Linares <alinares@fivetech.com>
 * Copyright 2010 Przemyslaw Czerpak (rewritten)
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

//#include "hbtypes.h"

#if defined( ZH_OS_WIN )

#include <windows.h>

#define ZH_DLL_MSG_NO_FUNC( func )  \
   do \
   { \
      MessageBox( NULL, \
                  TEXT( "Function '" ) TEXT( func ) TEXT( "' not found!" ), \
                  TEXT( func ), \
                  MB_OK | MB_ICONERROR ); \
   } while( 0 )

int zh_pcount( void )
{
   static ZH_PCOUNT s_pcount = NULL;

   if( ! s_pcount )
   {
      s_pcount = ( ZH_PCOUNT ) zh_dllGetProcAddress( "zh_pcount" );
      if( ! s_pcount )
         ZH_DLL_MSG_NO_FUNC( "zh_pcount" );
   }

   return s_pcount ? s_pcount() : 0;
}

ZH_ULONG zh_parinfo( int iParam )
{
   static ZH_PARINFO s_parinfo = NULL;

   if( ! s_parinfo )
   {
      s_parinfo = ( ZH_PARINFO ) zh_dllGetProcAddress( "zh_parinfo" );
      if( ! s_parinfo )
         ZH_DLL_MSG_NO_FUNC( "zh_parinfo" );
   }
   return s_parinfo ? s_parinfo( iParam ) : 0;
}

ZH_SIZE zh_parinfa( int iParam, ZH_SIZE nArrayIndex )
{
   static ZH_PARINFA s_parinfa = NULL;

   if( ! s_parinfa )
   {
      s_parinfa = ( ZH_PARINFA ) zh_dllGetProcAddress( "zh_parinfa" );
      if( ! s_parinfa )
         ZH_DLL_MSG_NO_FUNC( "zh_parinfa" );
   }
   return s_parinfa ? s_parinfa( iParam, nArrayIndex ) : 0;
}

PZH_ITEM zh_param( int iParam, long lMask )
{
   static ZH_PARAM s_param = NULL;

   if( ! s_param )
   {
      s_param = ( ZH_PARAM ) zh_dllGetProcAddress( "zh_param" );
      if( ! s_param )
         ZH_DLL_MSG_NO_FUNC( "zh_param" );
   }

   return s_param ? s_param( iParam, lMask ) : NULL;
}

PZH_ITEM zh_paramError( int iParam )
{
   static ZH_PARAMERROR s_paramError = NULL;

   if( ! s_paramError )
   {
      s_paramError = ( ZH_PARAMERROR ) zh_dllGetProcAddress( "zh_paramError" );
      if( ! s_paramError )
         ZH_DLL_MSG_NO_FUNC( "zh_paramError" );
   }

   return s_paramError ? s_paramError( iParam ) : NULL;
}

ZH_BOOL zh_extIsNil( int iParam )
{
   static ZH_EXTISPARAM s_extIsNil = NULL;

   if( ! s_extIsNil )
   {
      s_extIsNil = ( ZH_EXTISPARAM ) zh_dllGetProcAddress( "zh_extIsNil" );
      if( ! s_extIsNil )
         ZH_DLL_MSG_NO_FUNC( "zh_extIsNil" );
   }

   return s_extIsNil ? s_extIsNil( iParam ) : FALSE;
}

ZH_BOOL zh_extIsArray( int iParam )
{
   static ZH_EXTISPARAM s_extIsArray = NULL;

   if( ! s_extIsArray )
   {
      s_extIsArray = ( ZH_EXTISPARAM ) zh_dllGetProcAddress( "zh_extIsArray" );
      if( ! s_extIsArray )
         ZH_DLL_MSG_NO_FUNC( "zh_extIsArray" );
   }

   return s_extIsArray ? s_extIsArray( iParam ) : FALSE;
}

ZH_BOOL zh_extIsObject( int iParam )
{
   static ZH_EXTISPARAM s_extIsObject = NULL;

   if( ! s_extIsObject )
   {
      s_extIsObject = ( ZH_EXTISPARAM ) zh_dllGetProcAddress( "zh_extIsObject" );
      if( ! s_extIsObject )
         ZH_DLL_MSG_NO_FUNC( "zh_extIsObject" );
   }

   return s_extIsObject ? s_extIsObject( iParam ) : FALSE;
}


void zh_ret( void )
{
   static ZH_RET s_ret = NULL;

   if( ! s_ret )
   {
      s_ret = ( ZH_RET ) zh_dllGetProcAddress( "zh_ret" );
      if( ! s_ret )
         ZH_DLL_MSG_NO_FUNC( "zh_ret" );
   }

   if( s_ret )
      s_ret();
}

void zh_retc( const char * szText )
{
   static ZH_RETC s_retc = NULL;

   if( ! s_retc )
   {
      s_retc = ( ZH_RETC ) zh_dllGetProcAddress( "zh_retc" );
      if( ! s_retc )
         ZH_DLL_MSG_NO_FUNC( "zh_retc" );
   }

   if( s_retc )
      s_retc( szText );
}

void zh_retclen( const char * szText, ZH_SIZE nLen )
{
   static ZH_RETCLEN s_retclen = NULL;

   if( ! s_retclen )
   {
      s_retclen = ( ZH_RETCLEN ) zh_dllGetProcAddress( "zh_retclen" );
      if( ! s_retclen )
         ZH_DLL_MSG_NO_FUNC( "zh_retclen" );
   }

   if( s_retclen )
      s_retclen( szText, nLen );
}

void zh_retds( const char * szDate )
{
   static ZH_RETDS s_retds = NULL;

   if( ! s_retds )
   {
      s_retds = ( ZH_RETDS ) zh_dllGetProcAddress( "zh_retds" );
      if( ! s_retds )
         ZH_DLL_MSG_NO_FUNC( "zh_retds" );
   }

   if( s_retds )
      s_retds( szDate );
}

void zh_retd( int iYear, int iMonth, int iDay )
{
   static ZH_RETD s_retd = NULL;

   if( ! s_retd )
   {
      s_retd = ( ZH_RETD ) zh_dllGetProcAddress( "zh_retd" );
      if( ! s_retd )
         ZH_DLL_MSG_NO_FUNC( "zh_retd" );
   }

   if( s_retd )
      s_retd( iYear, iMonth, iDay );
}

void zh_retdl( long lJulian )
{
   static ZH_RETDL s_retdl = NULL;

   if( ! s_retdl )
   {
      s_retdl = ( ZH_RETDL ) zh_dllGetProcAddress( "zh_retdl" );
      if( ! s_retdl )
         ZH_DLL_MSG_NO_FUNC( "zh_retdl" );
   }

   if( s_retdl )
      s_retdl( lJulian );
}

void zh_retl( int iValue )
{
   static ZH_RETL s_retl = NULL;

   if( ! s_retl )
   {
      s_retl = ( ZH_RETL ) zh_dllGetProcAddress( "zh_retl" );
      if( ! s_retl )
         ZH_DLL_MSG_NO_FUNC( "zh_retl" );
   }

   if( s_retl )
      s_retl( iValue );
}

void zh_retnd( double dNumber )
{
   static ZH_RETND s_retnd = NULL;

   if( ! s_retnd )
   {
      s_retnd = ( ZH_RETND ) zh_dllGetProcAddress( "zh_retnd" );
      if( ! s_retnd )
         ZH_DLL_MSG_NO_FUNC( "zh_retnd" );
   }

   if( s_retnd )
      s_retnd( dNumber );
}

void zh_retni( int iNumber )
{
   static ZH_RETNI s_retni = NULL;

   if( ! s_retni )
   {
      s_retni = ( ZH_RETNI ) zh_dllGetProcAddress( "zh_retni" );
      if( ! s_retni )
         ZH_DLL_MSG_NO_FUNC( "zh_retni" );
   }

   if( s_retni )
      s_retni( iNumber );
}

void zh_retnl( long lNumber )
{
   static ZH_RETNL s_retnl = NULL;

   if( ! s_retnl )
   {
      s_retnl = ( ZH_RETNL ) zh_dllGetProcAddress( "zh_retnl" );
      if( ! s_retnl )
         ZH_DLL_MSG_NO_FUNC( "zh_retnl" );
   }

   if( s_retnl )
      s_retnl( lNumber );
}

void zh_retnlen( double dNumber, int iWidth, int iDec )
{
   static ZH_RETNLEN s_retnlen = NULL;

   if( ! s_retnlen )
   {
      s_retnlen = ( ZH_RETNLEN ) zh_dllGetProcAddress( "zh_retnlen" );
      if( ! s_retnlen )
         ZH_DLL_MSG_NO_FUNC( "zh_retnlen" );
   }

   if( s_retnlen )
      s_retnlen( dNumber, iWidth, iDec );
}

void zh_retndlen( double dNumber, int iWidth, int iDec )
{
   static ZH_RETNDLEN s_retndlen = NULL;

   if( ! s_retndlen )
   {
      s_retndlen = ( ZH_RETNDLEN ) zh_dllGetProcAddress( "zh_retndlen" );
      if( ! s_retndlen )
         ZH_DLL_MSG_NO_FUNC( "zh_retndlen" );
   }

   if( s_retndlen )
      s_retndlen( dNumber, iWidth, iDec );
}

void zh_retnilen( int iNumber, int iWidth )
{
   static ZH_RETNILEN s_retnilen = NULL;

   if( ! s_retnilen )
   {
      s_retnilen = ( ZH_RETNILEN ) zh_dllGetProcAddress( "zh_retnilen" );
      if( ! s_retnilen )
         ZH_DLL_MSG_NO_FUNC( "zh_retnilen" );
   }

   if( s_retnilen )
      s_retnilen( iNumber, iWidth );
}

void zh_retnllen( long lNumber, int iWidth )
{
   static ZH_RETNLLEN s_retnllen = NULL;

   if( ! s_retnllen )
   {
      s_retnllen = ( ZH_RETNLLEN ) zh_dllGetProcAddress( "zh_retnllen" );
      if( ! s_retnllen )
         ZH_DLL_MSG_NO_FUNC( "zh_retnllen" );
   }

   if( s_retnllen )
      s_retnllen( lNumber, iWidth );
}

void zh_reta( ZH_SIZE nLen )
{
   static ZH_RETA s_reta = NULL;

   if( ! s_reta )
   {
      s_reta = ( ZH_RETA ) zh_dllGetProcAddress( "zh_reta" );
      if( ! s_reta )
         ZH_DLL_MSG_NO_FUNC( "zh_reta" );
   }

   if( s_reta )
      s_reta( nLen );
}

const char * zh_parvc( int iParam, ... )
{
   static ZH_PARVC s_parvc = NULL;

   if( ! s_parvc )
   {
      s_parvc = ( ZH_PARVC ) zh_dllGetProcAddress( "zh_parvc" );
      if( ! s_parvc )
         ZH_DLL_MSG_NO_FUNC( "zh_parvc" );
   }

   if( s_parvc )
   {
      ZH_SIZE nArrayIndex = 0;

      if( zh_extIsArray( iParam ) )
      {
         va_list va;
         va_start( va, iParam );
         nArrayIndex = va_arg( va, ZH_SIZE );
         va_end( va );
      }

      return s_parvc( iParam, nArrayIndex );
   }

   return NULL;
}

ZH_SIZE zh_parvclen( int iParam, ... )
{
   static ZH_PARVCLEN s_parvclen = NULL;

   if( ! s_parvclen )
   {
      s_parvclen = ( ZH_PARVCLEN ) zh_dllGetProcAddress( "zh_parvclen" );
      if( ! s_parvclen )
         ZH_DLL_MSG_NO_FUNC( "zh_parvclen" );
   }

   if( s_parvclen )
   {
      ZH_SIZE nArrayIndex = 0;

      if( zh_extIsArray( iParam ) )
      {
         va_list va;
         va_start( va, iParam );
         nArrayIndex = va_arg( va, ZH_SIZE );
         va_end( va );
      }

      return s_parvclen( iParam, nArrayIndex );
   }

   return 0;
}

ZH_SIZE zh_parvcsiz( int iParam, ... )
{
   static ZH_PARVCSIZ s_parvcsiz = NULL;

   if( ! s_parvcsiz )
   {
      s_parvcsiz = ( ZH_PARVCSIZ ) zh_dllGetProcAddress( "zh_parvcsiz" );
      if( ! s_parvcsiz )
         ZH_DLL_MSG_NO_FUNC( "zh_parvcsiz" );
   }

   if( s_parvcsiz )
   {
      ZH_SIZE nArrayIndex = 0;

      if( zh_extIsArray( iParam ) )
      {
         va_list va;
         va_start( va, iParam );
         nArrayIndex = va_arg( va, ZH_SIZE );
         va_end( va );
      }

      return s_parvcsiz( iParam, nArrayIndex );
   }

   return 0;
}

const char * zh_parvds( int iParam, ... )
{
   static ZH_PARVDS s_parvds = NULL;

   if( ! s_parvds )
   {
      s_parvds = ( ZH_PARVDS ) zh_dllGetProcAddress( "zh_parvds" );
      if( ! s_parvds )
         ZH_DLL_MSG_NO_FUNC( "zh_parvds" );
   }

   if( s_parvds )
   {
      ZH_SIZE nArrayIndex = 0;

      if( zh_extIsArray( iParam ) )
      {
         va_list va;
         va_start( va, iParam );
         nArrayIndex = va_arg( va, ZH_SIZE );
         va_end( va );
      }

      return s_parvds( iParam, nArrayIndex );
   }

   return "        ";
}

char * zh_parvdsbuff( char * szDate, int iParam, ... )
{
   static ZH_PARVDSBUFF s_parvdsbuff = NULL;

   if( ! s_parvdsbuff )
   {
      s_parvdsbuff = ( ZH_PARVDSBUFF ) zh_dllGetProcAddress( "zh_parvdsbuff" );
      if( ! s_parvdsbuff )
         ZH_DLL_MSG_NO_FUNC( "zh_parvdsbuff" );
   }

   if( s_parvdsbuff )
   {
      ZH_SIZE nArrayIndex = 0;

      if( zh_extIsArray( iParam ) )
      {
         va_list va;
         va_start( va, iParam );
         nArrayIndex = va_arg( va, ZH_SIZE );
         va_end( va );
      }

      return s_parvdsbuff( szDate, iParam, nArrayIndex );
   }

   return szDate;
}

int zh_parvl( int iParam, ... )
{
   static ZH_PARVL s_parvl = NULL;

   if( ! s_parvl )
   {
      s_parvl = ( ZH_PARVL ) zh_dllGetProcAddress( "zh_parvl" );
      if( ! s_parvl )
         ZH_DLL_MSG_NO_FUNC( "zh_parvl" );
   }

   if( s_parvl )
   {
      ZH_SIZE nArrayIndex = 0;

      if( zh_extIsArray( iParam ) )
      {
         va_list va;
         va_start( va, iParam );
         nArrayIndex = va_arg( va, ZH_SIZE );
         va_end( va );
      }

      return s_parvl( iParam, nArrayIndex );
   }

   return 0;
}

double zh_parvnd( int iParam, ... )
{
   static ZH_PARVND s_parvnd = NULL;

   if( ! s_parvnd )
   {
      s_parvnd = ( ZH_PARVND ) zh_dllGetProcAddress( "zh_parvnd" );
      if( ! s_parvnd )
         ZH_DLL_MSG_NO_FUNC( "zh_parvnd" );
   }

   if( s_parvnd )
   {
      ZH_SIZE nArrayIndex = 0;

      if( zh_extIsArray( iParam ) )
      {
         va_list va;
         va_start( va, iParam );
         nArrayIndex = va_arg( va, ZH_SIZE );
         va_end( va );
      }

      return s_parvnd( iParam, nArrayIndex );
   }

   return 0;
}

int zh_parvni( int iParam, ... )
{
   static ZH_PARVNI s_parvni = NULL;

   if( ! s_parvni )
   {
      s_parvni = ( ZH_PARVNI ) zh_dllGetProcAddress( "zh_parvni" );
      if( ! s_parvni )
         ZH_DLL_MSG_NO_FUNC( "zh_parvni" );
   }

   if( s_parvni )
   {
      ZH_SIZE nArrayIndex = 0;

      if( zh_extIsArray( iParam ) )
      {
         va_list va;
         va_start( va, iParam );
         nArrayIndex = va_arg( va, ZH_SIZE );
         va_end( va );
      }

      return s_parvni( iParam, nArrayIndex );
   }

   return 0;
}

long zh_parvnl( int iParam, ... )
{
   static ZH_PARVNL s_parvnl = NULL;

   if( ! s_parvnl )
   {
      s_parvnl = ( ZH_PARVNL ) zh_dllGetProcAddress( "zh_parvnl" );
      if( ! s_parvnl )
         ZH_DLL_MSG_NO_FUNC( "zh_parvnl" );
   }

   if( s_parvnl )
   {
      ZH_SIZE nArrayIndex = 0;

      if( zh_extIsArray( iParam ) )
      {
         va_list va;
         va_start( va, iParam );
         nArrayIndex = va_arg( va, ZH_SIZE );
         va_end( va );
      }

      return s_parvnl( iParam, nArrayIndex );
   }

   return 0;
}

int zh_storvc( const char * szText, int iParam, ... )
{
   static ZH_STORVC s_storvc = NULL;

   if( ! s_storvc )
   {
      s_storvc = ( ZH_STORVC ) zh_dllGetProcAddress( "zh_storvc" );
      if( ! s_storvc )
         ZH_DLL_MSG_NO_FUNC( "zh_storvc" );
   }

   if( s_storvc )
   {
      ZH_SIZE nArrayIndex = 0;

      if( zh_extIsArray( iParam ) )
      {
         va_list va;
         va_start( va, iParam );
         nArrayIndex = va_arg( va, ZH_SIZE );
         va_end( va );
      }

      return s_storvc( szText, iParam, nArrayIndex );
   }

   return 0;
}

int zh_storvclen( const char * szText, ZH_SIZE nLen, int iParam, ... )
{
   static ZH_STORVCLEN s_storvclen = NULL;

   if( ! s_storvclen )
   {
      s_storvclen = ( ZH_STORVCLEN ) zh_dllGetProcAddress( "zh_storvclen" );
      if( ! s_storvclen )
         ZH_DLL_MSG_NO_FUNC( "zh_storvclen" );
   }

   if( s_storvclen )
   {
      ZH_SIZE nArrayIndex = 0;

      if( zh_extIsArray( iParam ) )
      {
         va_list va;
         va_start( va, iParam );
         nArrayIndex = va_arg( va, ZH_SIZE );
         va_end( va );
      }

      return s_storvclen( szText, nLen, iParam, nArrayIndex );
   }

   return 0;
}

int zh_storvds( const char * szDate, int iParam, ... )
{
   static ZH_STORVDS s_storvds = NULL;

   if( ! s_storvds )
   {
      s_storvds = ( ZH_STORVDS ) zh_dllGetProcAddress( "zh_storvds" );
      if( ! s_storvds )
         ZH_DLL_MSG_NO_FUNC( "zh_storvds" );
   }

   if( s_storvds )
   {
      ZH_SIZE nArrayIndex = 0;

      if( zh_extIsArray( iParam ) )
      {
         va_list va;
         va_start( va, iParam );
         nArrayIndex = va_arg( va, ZH_SIZE );
         va_end( va );
      }

      return s_storvds( szDate, iParam, nArrayIndex );
   }

   return 0;
}

int zh_storvl( int iLogical, int iParam, ... )
{
   static ZH_STORVL s_storvl = NULL;

   if( ! s_storvl )
   {
      s_storvl = ( ZH_STORVL ) zh_dllGetProcAddress( "zh_storvl" );
      if( ! s_storvl )
         ZH_DLL_MSG_NO_FUNC( "zh_storvl" );
   }

   if( s_storvl )
   {
      ZH_SIZE nArrayIndex = 0;

      if( zh_extIsArray( iParam ) )
      {
         va_list va;
         va_start( va, iParam );
         nArrayIndex = va_arg( va, ZH_SIZE );
         va_end( va );
      }

      return s_storvl( iLogical, iParam, nArrayIndex );
   }

   return 0;
}

int zh_storvni( int iValue, int iParam, ... )
{
   static ZH_STORVNI s_storvni = NULL;

   if( ! s_storvni )
   {
      s_storvni = ( ZH_STORVNI ) zh_dllGetProcAddress( "zh_storvni" );
      if( ! s_storvni )
         ZH_DLL_MSG_NO_FUNC( "zh_storvni" );
   }

   if( s_storvni )
   {
      ZH_SIZE nArrayIndex = 0;

      if( zh_extIsArray( iParam ) )
      {
         va_list va;
         va_start( va, iParam );
         nArrayIndex = va_arg( va, ZH_SIZE );
         va_end( va );
      }

      return s_storvni( iValue, iParam, nArrayIndex );
   }

   return 0;
}

int zh_storvnl( long lValue, int iParam, ... )
{
   static ZH_STORVNL s_storvnl = NULL;

   if( ! s_storvnl )
   {
      s_storvnl = ( ZH_STORVNL ) zh_dllGetProcAddress( "zh_storvnl" );
      if( ! s_storvnl )
         ZH_DLL_MSG_NO_FUNC( "zh_storvnl" );
   }

   if( s_storvnl )
   {
      ZH_SIZE nArrayIndex = 0;

      if( zh_extIsArray( iParam ) )
      {
         va_list va;
         va_start( va, iParam );
         nArrayIndex = va_arg( va, ZH_SIZE );
         va_end( va );
      }

      return s_storvnl( lValue, iParam, nArrayIndex );
   }

   return 0;
}

int zh_storvnd( double dNumber, int iParam, ... )
{
   static ZH_STORVND s_storvnd = NULL;

   if( ! s_storvnd )
   {
      s_storvnd = ( ZH_STORVND ) zh_dllGetProcAddress( "zh_storvnd" );
      if( ! s_storvnd )
         ZH_DLL_MSG_NO_FUNC( "zh_storvnd" );
   }

   if( s_storvnd )
   {
      ZH_SIZE nArrayIndex = 0;

      if( zh_extIsArray( iParam ) )
      {
         va_list va;
         va_start( va, iParam );
         nArrayIndex = va_arg( va, ZH_SIZE );
         va_end( va );
      }

      return s_storvnd( dNumber, iParam, nArrayIndex );
   }

   return 0;
}

ZH_BOOL zh_arrayNew( PZH_ITEM pItem, ZH_SIZE nLen )
{
   static ZH_ARRAYNEW s_arrayNew = NULL;

   if( ! s_arrayNew )
   {
      s_arrayNew = ( ZH_ARRAYNEW ) zh_dllGetProcAddress( "zh_arrayNew" );
      if( ! s_arrayNew )
         ZH_DLL_MSG_NO_FUNC( "zh_arrayNew" );
   }
   return s_arrayNew ? s_arrayNew( pItem, nLen ) : ZH_FALSE;
}

ZH_SIZE zh_arrayLen( PZH_ITEM pArray )
{
   static ZH_ARRAYLEN s_arrayLen = NULL;

   if( ! s_arrayLen )
   {
      s_arrayLen = ( ZH_ARRAYLEN ) zh_dllGetProcAddress( "zh_arrayLen" );
      if( ! s_arrayLen )
         ZH_DLL_MSG_NO_FUNC( "zh_arrayLen" );
   }
   return s_arrayLen ? s_arrayLen( pArray ) : 0;
}

ZH_BOOL zh_arrayIsObject( PZH_ITEM pArray )
{
   static ZH_ARRAYISOBJECT s_arrayIsObject = NULL;

   if( ! s_arrayIsObject )
   {
      s_arrayIsObject = ( ZH_ARRAYISOBJECT ) zh_dllGetProcAddress( "zh_arrayIsObject" );
      if( ! s_arrayIsObject )
         ZH_DLL_MSG_NO_FUNC( "zh_arrayIsObject" );
   }
   return s_arrayIsObject ? s_arrayIsObject( pArray ) : ZH_FALSE;
}

ZH_BOOL zh_arrayAdd( PZH_ITEM pArray, PZH_ITEM pItem )
{
   static ZH_ARRAYADD s_arrayAdd = NULL;

   if( ! s_arrayAdd )
   {
      s_arrayAdd = ( ZH_ARRAYADD ) zh_dllGetProcAddress( "zh_arrayAdd" );
      if( ! s_arrayAdd )
         ZH_DLL_MSG_NO_FUNC( "zh_arrayAdd" );
   }
   return s_arrayAdd ? s_arrayAdd( pArray, pItem ) : ZH_FALSE;
}

ZH_BOOL zh_arrayIns( PZH_ITEM pArray, ZH_SIZE nIndex )
{
   static ZH_ARRAYINS s_arrayIns = NULL;

   if( ! s_arrayIns )
   {
      s_arrayIns = ( ZH_ARRAYINS ) zh_dllGetProcAddress( "zh_arrayIns" );
      if( ! s_arrayIns )
         ZH_DLL_MSG_NO_FUNC( "zh_arrayIns" );
   }
   return s_arrayIns ? s_arrayIns( pArray, nIndex ) : ZH_FALSE;
}

ZH_BOOL zh_arrayDel( PZH_ITEM pArray, ZH_SIZE nIndex )
{
   static ZH_ARRAYDEL s_arrayDel = NULL;

   if( ! s_arrayDel )
   {
      s_arrayDel = ( ZH_ARRAYDEL ) zh_dllGetProcAddress( "zh_arrayDel" );
      if( ! s_arrayDel )
         ZH_DLL_MSG_NO_FUNC( "zh_arrayDel" );
   }
   return s_arrayDel ? s_arrayDel( pArray, nIndex ) : ZH_FALSE;
}

ZH_BOOL zh_arraySize( PZH_ITEM pArray, ZH_SIZE nLen )
{
   static ZH_ARRAYSIZE s_arraySize = NULL;

   if( ! s_arraySize )
   {
      s_arraySize = ( ZH_ARRAYSIZE ) zh_dllGetProcAddress( "zh_arraySize" );
      if( ! s_arraySize )
         ZH_DLL_MSG_NO_FUNC( "zh_arraySize" );
   }
   return s_arraySize ? s_arraySize( pArray, nLen ) : ZH_FALSE;
}

ZH_BOOL zh_arrayLast( PZH_ITEM pArray, PZH_ITEM pResult )
{
   static ZH_ARRAYLAST s_arrayLast = NULL;

   if( ! s_arrayLast )
   {
      s_arrayLast = ( ZH_ARRAYLAST ) zh_dllGetProcAddress( "zh_arrayLast" );
      if( ! s_arrayLast )
         ZH_DLL_MSG_NO_FUNC( "zh_arrayLast" );
   }
   return s_arrayLast ? s_arrayLast( pArray, pResult ) : ZH_FALSE;
}

ZH_BOOL zh_arraySet( PZH_ITEM pArray, ZH_SIZE nIndex, PZH_ITEM pItem )
{
   static ZH_ARRAYSET s_arraySet = NULL;

   if( ! s_arraySet )
   {
      s_arraySet = ( ZH_ARRAYSET ) zh_dllGetProcAddress( "zh_arraySet" );
      if( ! s_arraySet )
         ZH_DLL_MSG_NO_FUNC( "zh_arraySet" );
   }
   return s_arraySet ? s_arraySet( pArray, nIndex, pItem ) : ZH_FALSE;
}

ZH_BOOL zh_arrayGet( PZH_ITEM pArray, ZH_SIZE nIndex, PZH_ITEM pItem )
{
   static ZH_ARRAYGET s_arrayGet = NULL;

   if( ! s_arrayGet )
   {
      s_arrayGet = ( ZH_ARRAYGET ) zh_dllGetProcAddress( "zh_arrayGet" );
      if( ! s_arrayGet )
         ZH_DLL_MSG_NO_FUNC( "zh_arrayGet" );
   }
   return s_arrayGet ? s_arrayGet( pArray, nIndex, pItem ) : ZH_FALSE;
}

void * zh_xalloc( ZH_SIZE nSize )
{
   static ZH_XALLOC s_xalloc = NULL;

   if( ! s_xalloc )
   {
      s_xalloc = ( ZH_XALLOC ) zh_dllGetProcAddress( "zh_xalloc" );
      if( ! s_xalloc )
         ZH_DLL_MSG_NO_FUNC( "zh_xalloc" );
   }

   return s_xalloc ? s_xalloc( nSize ) : NULL;
}

void * zh_xgrab( ZH_SIZE nSize )
{
   static ZH_XGRAB s_xgrab = NULL;

   if( ! s_xgrab )
   {
      s_xgrab = ( ZH_XGRAB ) zh_dllGetProcAddress( "zh_xgrab" );
      if( ! s_xgrab )
         ZH_DLL_MSG_NO_FUNC( "zh_xgrab" );
   }

   return s_xgrab ? s_xgrab( nSize ) : NULL;
}

void zh_xfree( void * pMem )
{
   static ZH_XFREE s_xfree = NULL;

   if( ! s_xfree )
   {
      s_xfree = ( ZH_XFREE ) zh_dllGetProcAddress( "zh_xfree" );
      if( ! s_xfree )
         ZH_DLL_MSG_NO_FUNC( "zh_xfree" );
   }

   if( s_xfree )
      s_xfree( pMem );
}

void * zh_xrealloc( void * pMem, ZH_SIZE nSize )
{
   static ZH_XREALLOC s_xrealloc = NULL;

   if( ! s_xrealloc )
   {
      s_xrealloc = ( ZH_XREALLOC ) zh_dllGetProcAddress( "zh_xrealloc" );
      if( ! s_xrealloc )
         ZH_DLL_MSG_NO_FUNC( "zh_xrealloc" );
   }

   return s_xrealloc ? s_xrealloc( pMem, nSize ) : NULL;
}

void zh_macroTextValue( PZH_ITEM pItem )
{
   static ZH_MACROTEXTVALUE s_macroTextValue = NULL;

   if( ! s_macroTextValue )
   {
      s_macroTextValue = ( ZH_MACROTEXTVALUE ) zh_dllGetProcAddress( "macroTextValue" );
      if( ! s_macroTextValue )
         ZH_DLL_MSG_NO_FUNC( "macroTextValue" );
   }

   if( s_macroTextValue )
      s_macroTextValue( pItem );
}

#endif /* ZH_OS_WIN */
