/*
 * GZIP functions wrapper
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
#include "zh_error_api.h"
#include "zh_string_api.h"
#include "zh_vm.h"

#include <zlib.h>

#ifndef ZH_NO_GZLIB
/* GZIP stream destructor */
static ZH_GARBAGE_FUNC( zh_gz_Destructor )
{
   gzFile * gzHolder = ( gzFile * ) Cargo;

   if( *gzHolder )
   {
      zh_vmUnlock();
      gzclose( *gzHolder );
      zh_vmLock();
      *gzHolder = NULL;
   }
}

static const ZH_GC_FUNCS s_gcGZFuncs =
{
   zh_gz_Destructor,
   zh_gcDummyMark
};

static gzFile zh_gzParam( int iParam )
{
   gzFile * gzHolder = ( gzFile * ) zh_parptrGC( &s_gcGZFuncs, iParam );

   if( gzHolder && *gzHolder )
      return *gzHolder;

   zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
   return NULL;
}
#endif

/*
 * zh_gzOpen( <cFile>, <cMode> ) => <pGZipStream> or NIL on Error
 */
ZH_FUNC( ZH_GZOPEN )
{
#ifndef ZH_NO_GZLIB
   const char * cFile = zh_parc( 1 ), * cMode = zh_parc( 2 );

   if( cFile && cMode )
   {
      gzFile gz;

      zh_vmUnlock();
      #if defined( ZH_OS_WIN ) && ZLIB_VERNUM >= 0x1270
      {
         void * hFile;
         gz = gzopen_w( zh_parstr_u16( 1, ZH_CODEPAGE_ENDIAN_NATIVE, &hFile, NULL ), cMode );
         zh_strfree( hFile );
      }
      #else
         gz = gzopen( cFile, cMode );
      #endif
      zh_vmLock();

      if( gz )
      {
         gzFile * gzHolder = ( gzFile * ) zh_gcAllocate( sizeof( gzFile ),
                                                         &s_gcGZFuncs );
         *gzHolder = gz;
         zh_retptrGC( gzHolder );
      }
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
#endif
}

/*
 * zh_gzDOpen( <hFile>, <cMode> ) => <pGZipStream> or NIL on Error
 */
ZH_FUNC( ZH_GZDOPEN )
{
#ifndef ZH_NO_GZLIB
   const char * cMode = zh_parc( 2 );

   if( ZH_IS_PARAM_NUM( 1 ) && cMode )
   {
      gzFile gz;

      zh_vmUnlock();
      gz = gzdopen( zh_parni( 1 ), cMode );
      zh_vmLock();

      if( gz )
      {
         gzFile * gzHolder = ( gzFile * ) zh_gcAllocate( sizeof( gzFile ),
                                                         &s_gcGZFuncs );
         *gzHolder = gz;
         zh_retptrGC( gzHolder );
      }
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
#endif
}

/*
 * zh_gzClose( <pGZipStream> ) => <nResult>
 */
ZH_FUNC( ZH_GZCLOSE )
{
#ifndef ZH_NO_GZLIB
   gzFile * gzHolder = ( gzFile * ) zh_parptrGC( &s_gcGZFuncs, 1 );

   if( gzHolder )
   {
      gzFile gz = *gzHolder;
      int iResult;

      *gzHolder = NULL;

      zh_vmUnlock();
      iResult = gzclose( gz );
      zh_vmLock();

      zh_retni( iResult );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
#endif
}

/*
 * zh_gzSetParams( <pGZipStream>, <nLevel>, <nStrategy> ) => <nResult>
 */
ZH_FUNC( ZH_GZSETPARAMS )
{
#ifndef ZH_NO_GZLIB
   if( ZH_IS_PARAM_NUM( 2 ) && ZH_IS_PARAM_NUM( 3 ) )
   {
      gzFile gz = zh_gzParam( 1 );
      if( gz )
         zh_retni( gzsetparams( gz, zh_parni( 2 ), zh_parni( 3 ) ) );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
#endif
}

/*
 * zh_gzRead( <pGZipStream>, <@cData>, [ <nLen> ] ) => <nResult>
 */
ZH_FUNC( ZH_GZREAD )
{
#ifndef ZH_NO_GZLIB
   PZH_ITEM pBuffer = ZH_ISBYREF( 2 ) ? zh_param( 2, ZH_IT_STRING ) : NULL;
   char * szBuffer;
   ZH_SIZE nLen;

   if( pBuffer && zh_itemGetWriteCL( pBuffer, &szBuffer, &nLen ) )
   {
      gzFile gz = zh_gzParam( 1 );
      if( gz )
      {
         int iResult;

         if( ZH_IS_PARAM_NUM( 3 ) )
         {
            ZH_SIZE nLim = zh_parns( 3 );
            if( nLim < nLen )
               nLen = nLim;
         }

         zh_vmUnlock();
         iResult = gzread( gz, szBuffer, ( unsigned ) nLen );
         zh_vmLock();

         zh_retni( iResult );
      }
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
#endif
}

/*
 * zh_gzWrite( <pGZipStream>, <cData>, [ <nLen> ] ) => <nResult>
 */
ZH_FUNC( ZH_GZWRITE )
{
#ifndef ZH_NO_GZLIB
   const char * szData = zh_parc( 2 );

   if( szData )
   {
      gzFile gz = zh_gzParam( 1 );
      if( gz )
      {
         int iResult;

         zh_vmUnlock();
         iResult = gzwrite( gz, szData,
                            ZH_IS_PARAM_NUM( 3 ) ? ( unsigned ) zh_parns( 3 ) :
                                            ( unsigned ) zh_parclen( 2 ) );
         zh_vmLock();

         zh_retni( iResult );
      }
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
#endif
}

/*
 * zh_gzGetS( <pGZipStream>, <nMaxBytes> ) => <cLine> or NIL on error
 */
ZH_FUNC( ZH_GZGETS )
{
#ifndef ZH_NO_GZLIB
   int iLen = zh_parni( 2 );

   if( iLen > 0 )
   {
      gzFile gz = zh_gzParam( 1 );
      if( gz )
      {
         char * szBuffer = ( char * ) zh_xalloc( iLen + 1 );

         if( szBuffer )
         {
            char * szBuff;

            zh_vmUnlock();
            szBuff = gzgets( gz, szBuffer, iLen );
            zh_vmLock();

            if( szBuff != Z_NULL )
               zh_retc_buffer( szBuffer );
            else
               zh_xfree( szBuffer );
         }
      }
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
#endif
}

/*
 * zh_gzPutS( <pGZipStream>, <cData> ) => <nResult>
 */
ZH_FUNC( ZH_GZPUTS )
{
#ifndef ZH_NO_GZLIB
   const char * szData = zh_parc( 2 );

   if( szData )
   {
      gzFile gz = zh_gzParam( 1 );
      if( gz )
      {
         int iResult;

         zh_vmUnlock();
         iResult = gzputs( gz, szData );
         zh_vmLock();

         zh_retni( iResult );
      }
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
#endif
}

/*
 * zh_gzPutC( <pGZipStream>, <nByte> ) => <nResult>
 */
ZH_FUNC( ZH_GZPUTC )
{
#ifndef ZH_NO_GZLIB
   if( ZH_IS_PARAM_NUM( 2 ) )
   {
      gzFile gz = zh_gzParam( 1 );
      if( gz )
      {
         int iResult;

         zh_vmUnlock();
         iResult = gzputc( gz, zh_parni( 2 ) );
         zh_vmLock();

         zh_retni( iResult );
      }
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
#endif
}

/*
 * zh_gzGetC( <pGZipStream> ) => <nByte>
 */
ZH_FUNC( ZH_GZGETC )
{
#ifndef ZH_NO_GZLIB
   gzFile gz = zh_gzParam( 1 );

   if( gz )
   {
      int iResult;

      zh_vmUnlock();
      iResult = gzgetc( gz );
      zh_vmLock();

      zh_retni( iResult );
   }
#endif
}

/*
 * zh_gzUnGetC( <nByte>, <pGZipStream> ) => <nByte>
 */
ZH_FUNC( ZH_GZUNGETC )
{
#ifndef ZH_NO_GZLIB
   if( ZH_IS_PARAM_NUM( 1 ) )
   {
#if ZLIB_VERNUM >= 0x1202
      gzFile gz = zh_gzParam( 2 );
      if( gz )
      {
         int iResult;

         zh_vmUnlock();
         iResult = gzungetc( zh_parni( 1 ), gz );
         zh_vmLock();

         zh_retni( iResult );
      }
#endif
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
#endif
}

/*
 * zh_gzFlush( <pGZipStream>, [ <nFlush> ] ) => <nResult>
 */
ZH_FUNC( ZH_GZFLUSH )
{
#ifndef ZH_NO_GZLIB
   gzFile gz = zh_gzParam( 1 );

   if( gz )
   {
      int iResult;

      zh_vmUnlock();
      iResult = gzflush( gz, zh_parnidef( 2, Z_SYNC_FLUSH ) );
      zh_vmLock();

      zh_retni( iResult );
   }
#endif
}

/*
 * zh_gzSeek( <pGZipStream>, <nOffset>, [ <nWhence> ] ) => <nOffset>
 */
ZH_FUNC( ZH_GZSEEK )
{
#ifndef ZH_NO_GZLIB
   if( ZH_IS_PARAM_NUM( 2 ) )
   {
      gzFile gz = zh_gzParam( 1 );
      if( gz )
      {
         ZH_MAXINT nResult;

         zh_vmUnlock();
         nResult = gzseek( gz, ( z_off_t ) zh_parnint( 2 ),
                           zh_parnidef( 3, SEEK_SET ) );
         zh_vmLock();

         zh_retnint( nResult );
      }
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
#endif
}

/*
 * zh_gzRewind( <pGZipStream> ) => <nResult>
 */
ZH_FUNC( ZH_GZREWIND )
{
#ifndef ZH_NO_GZLIB
   gzFile gz = zh_gzParam( 1 );

   if( gz )
   {
      int iResult;

      zh_vmUnlock();
      iResult = gzrewind( gz );
      zh_vmLock();

      zh_retni( iResult );
   }
#endif
}

/*
 * zh_gzTell( <pGZipStream> ) => <nResult>
 */
ZH_FUNC( ZH_GZTELL )
{
#ifndef ZH_NO_GZLIB
   gzFile gz = zh_gzParam( 1 );

   if( gz )
   {
      ZH_MAXINT nResult;

      zh_vmUnlock();
      nResult = gztell( gz );
      zh_vmLock();

      zh_retnint( nResult );
   }
#endif
}

/*
 * zh_gzEof( <pGZipStream> ) => <lResult>
 */
ZH_FUNC( ZH_GZEOF )
{
#ifndef ZH_NO_GZLIB
   gzFile gz = zh_gzParam( 1 );

   if( gz )
   {
      int iResult;

      zh_vmUnlock();
      iResult = gzeof( gz );
      zh_vmLock();

      zh_retl( iResult != 0 );
   }
#endif
}

/*
 * zh_gzDirect( <pGZipStream> ) => <lResult>
 */
ZH_FUNC( ZH_GZDIRECT )
{
#ifndef ZH_NO_GZLIB
#if ZLIB_VERNUM >= 0x1230
   gzFile gz = zh_gzParam( 1 );
   if( gz )
   {
      int iResult;

      zh_vmUnlock();
      iResult = gzdirect( gz );
      zh_vmLock();

      zh_retl( iResult != 0 );
   }
#endif
#endif
}

/*
 * zh_gzError( <pGZipStream>, [ <@nError> ] ) => <cError>
 */
ZH_FUNC( ZH_GZERROR )
{
#ifndef ZH_NO_GZLIB
   gzFile gz = zh_gzParam( 1 );

   if( gz )
   {
      int iErrNum = 0;

      zh_retc( gzerror( gz, &iErrNum ) );
      zh_storni( iErrNum, 2 );
   }
#endif
}

/*
 * zh_gzClearErr( <pGZipStream> ) => NIL
 */
ZH_FUNC( ZH_GZCLEARERR )
{
#ifndef ZH_NO_GZLIB
#if ZLIB_VERNUM >= 0x1202
   gzFile gz = zh_gzParam( 1 );
   if( gz )
      gzclearerr( gz );
#endif
#endif
}
