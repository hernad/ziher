/*
 * ZLIB functions wrapper
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

#define _ZH_ZLIB_INTERNAL_

#include "zh_api.h"
#include "zh_item_api.h"
#include "zh_error_api.h"
#include "zh_init.h"
#include "zh_zlib.h"

#include <zlib.h>

/* Try to figure if we have this function. Z_RLE was introduced in 1.2.0.1,
   while compressBound() and deflateBound() were added in 1.2.0. This means
   we have to miss compressBound() when using zlib 1.2.0. [vszakats] */
/* ZLIB_VERNUM were added in version 1.2.0.2 so it cannot be used for older
   zlib libraries */
#if defined( Z_RLE ) && ! defined( Z_SOLO )
#define _ZH_Z_COMPRESSBOUND
#endif

#if ! defined( _ZH_Z_COMPRESSBOUND )
/* additional 12 bytes is for GZIP compression which uses bigger header */
#define deflateBound( s, n )  ( zh_zlibCompressBound( n ) + ( fGZip ? 12 : 0 ) )
#endif

static ZH_SIZE s_zlibCompressBound( ZH_SIZE nLen )
{
#if ! defined( _ZH_Z_COMPRESSBOUND )
   return nLen + ( nLen >> 12 ) + ( nLen >> 14 ) + ( nLen >> 25 ) + 13;
#else
   return compressBound( ( uLong ) nLen );
#endif
}

static void * s_zlib_alloc( void * cargo, uInt items, uInt size )
{
   ZH_SYMBOL_UNUSED( cargo );

   return ( items > 0 && size > 0 ) ? zh_xalloc( ( ZH_SIZE ) items * size ) : NULL;
}

static void s_zlib_free( void * cargo, void * address )
{
   ZH_SYMBOL_UNUSED( cargo );

   if( address )
      zh_xfree( address );
}

static int s_zlibCompress2( char ** pDstPtr, ZH_SIZE * pnDst,
                            const char * pSrc, ZH_SIZE nSrc,
                            ZH_BOOL fGZip, int level )
{
   z_stream stream;
   int iResult;

   memset( &stream, 0, sizeof( stream ) );
   stream.zalloc    = s_zlib_alloc;
   stream.zfree     = s_zlib_free;
   stream.opaque    = NULL;
   stream.next_in   = ( Bytef* ) ZH_UNCONST( pSrc );
   stream.avail_in  = ( uInt ) nSrc;
   iResult = deflateInit2( &stream, level, Z_DEFLATED,
                           15 + ( fGZip ? 16 : 0 ), 8,
                           Z_DEFAULT_STRATEGY );
   if( iResult == Z_OK )
   {
      if( *pDstPtr == NULL )
      {
         if( *pnDst == 0 )
            *pnDst = deflateBound( &stream, ( uLong ) nSrc );
         *pDstPtr = ( char * ) zh_xalloc( *pnDst + 1 );
         if( *pDstPtr == NULL )
            iResult = Z_MEM_ERROR;
      }
   }

   if( iResult == Z_OK )
   {
      stream.next_out  = ( Bytef* ) *pDstPtr;
      stream.avail_out = ( uInt ) *pnDst;

      do
      {
         iResult = deflate( &stream, Z_FINISH );
      }
      while( iResult == Z_OK );

      if( iResult == Z_STREAM_END )
      {
         *pnDst = stream.total_out;
         iResult = Z_OK;
      }
      deflateEnd( &stream );
   }

   return iResult;
}

static int s_zlibCompress( char * pDst, ZH_SIZE * pnDst,
                           const char * pSrc, ZH_SIZE nSrc, int level )
{
   return s_zlibCompress2( &pDst, pnDst, pSrc, nSrc, ZH_FALSE, level );
}

static ZH_SIZE s_zlibUncompressedSize( const char * szSrc, ZH_SIZE nLen,
                                       int * piResult )
{
   Byte buffer[ 1024 ];
   z_stream stream;
   ZH_SIZE nDest = 0;

   memset( &stream, 0, sizeof( stream ) );
   stream.zalloc    = s_zlib_alloc;
   stream.zfree     = s_zlib_free;
   stream.opaque    = NULL;
   stream.next_in   = ( Bytef * ) ZH_UNCONST( szSrc );
   stream.avail_in  = ( uInt ) nLen;

   *piResult = inflateInit2( &stream, 15 + 32 );
   if( *piResult == Z_OK )
   {
      do
      {
         stream.next_out  = buffer;
         stream.avail_out = sizeof( buffer );
         *piResult = inflate( &stream, Z_NO_FLUSH );
      }
      while( *piResult == Z_OK );

      if( *piResult == Z_STREAM_END )
      {
         nDest = stream.total_out;
         *piResult = Z_OK;
      }
      inflateEnd( &stream );
   }

   return nDest;
}

static int s_zlibUncompress( char * pDst, ZH_SIZE * pnDst,
                             const char * pSrc, ZH_SIZE nSrc )
{
   z_stream stream;
   int iResult;

   memset( &stream, 0, sizeof( stream ) );
   stream.zalloc    = s_zlib_alloc;
   stream.zfree     = s_zlib_free;
   stream.opaque    = NULL;
   stream.next_in   = ( Bytef* ) ZH_UNCONST( pSrc );
   stream.avail_in  = ( uInt ) nSrc;
   iResult = inflateInit2( &stream, 15 + 32 );

   if( iResult == Z_OK )
   {
      stream.next_out  = ( Bytef* ) pDst;
      stream.avail_out = ( uInt ) *pnDst;

      do
      {
         iResult = inflate( &stream, Z_FINISH );
      }
      while( iResult == Z_OK );

      if( iResult == Z_STREAM_END )
      {
         *pnDst = stream.total_out;
         iResult = Z_OK;
      }
      inflateEnd( &stream );
   }

   return iResult;
}

/*
 * zh_ZLibVersion( [<nType>] ) --> <cZlibVersion>
 */
ZH_FUNC( ZH_ZLIBVERSION )
{
   if( zh_parni( 1 ) == 1 )
      zh_retc_const( ZLIB_VERSION );
   else
#if defined( ZH_OS_QNX )
      /* NOTE: Hack to avoid "undefined reference to 'zlibVersion' when linking hbrun on QNX 6.2.1. */
      zh_retc_null();
#else
      zh_retc( zlibVersion() );
#endif
}

/*
 * zh_ZCompressBound( <cData> | <nDataLen> ) --> <nMaxCompressLen>
 */
ZH_FUNC( ZH_ZCOMPRESSBOUND )
{
   if( ZH_ISCHAR( 1 ) )
      zh_retnint( s_zlibCompressBound( zh_parclen( 1 ) ) );
   else if( ZH_IS_PARAM_NUM( 1 ) )
      zh_retnint( s_zlibCompressBound( zh_parns( 1 ) ) );
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/*
 * zh_ZUncompressLen( <cCompressedData>, [<@nResult>] )
 *          --> <nUnCompressedDataLen> or -1 on error
 */
ZH_FUNC( ZH_ZUNCOMPRESSLEN )
{
   const char * szData = zh_parc( 1 );

   if( szData )
   {
      ZH_SIZE nLen = zh_parclen( 1 );
      int iResult = Z_OK;

      if( nLen )
         nLen = s_zlibUncompressedSize( szData, nLen, &iResult );

      if( iResult == Z_OK )
         zh_retnint( nLen );
      else
         zh_retni( -1 );

      zh_storni( iResult, 2 );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/*
 * zh_ZCompress( <cData>, [<nDstBufLen>|<@cBuffer>], [<@nResult>], [<nLevel>] )
 *    => <cCompressedData> or NIL on Error
 */
ZH_FUNC( ZH_ZCOMPRESS )
{
   const char * szData = zh_parc( 1 );

   if( szData )
   {
      ZH_SIZE nLen = zh_parclen( 1 );

      if( nLen )
      {
         PZH_ITEM pBuffer = ZH_ISBYREF( 2 ) ? zh_param( 2, ZH_IT_STRING ) : NULL;
         ZH_BOOL fAlloc = ZH_FALSE;
         ZH_SIZE nDstLen;
         char * pDest;
         int iResult;

         if( pBuffer )
         {
            if( ! zh_itemGetWriteCL( pBuffer, &pDest, &nDstLen ) )
               pDest = NULL;
         }
         else
         {
            if( ZH_IS_PARAM_NUM( 2 ) )
            {
               nDstLen = zh_parns( 2 );
               pDest = ( char * ) zh_xalloc( nDstLen + 1 );
            }
            else
            {
               pDest = NULL;
               nDstLen = 0;
               fAlloc = ZH_TRUE;
            }
         }

         if( pDest || fAlloc )
         {
            iResult = s_zlibCompress2( &pDest, &nDstLen, szData, nLen, ZH_FALSE,
                                       zh_parnidef( 4, Z_DEFAULT_COMPRESSION ) );
            if( ! pBuffer )
            {
               if( iResult == Z_OK )
                  zh_retclen_buffer( pDest, nDstLen );
               else if( pDest )
                  zh_xfree( pDest );
            }
            else if( iResult == Z_OK )
               zh_retclen( pDest, nDstLen );
         }
         else
            iResult = Z_MEM_ERROR;

         zh_storni( iResult, 3 );
      }
      else
      {
         zh_retc_null();
         zh_storni( Z_OK, 3 );
      }
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/*
 * zh_ZUncompress( <cCompressedData>, [<nDstBufLen>|<@cBuffer>], [<@nResult>] )
 *    => <cUnCompressedData> or NIL on Error
 */
ZH_FUNC( ZH_ZUNCOMPRESS )
{
   PZH_ITEM pBuffer = ZH_ISBYREF( 2 ) ? zh_param( 2, ZH_IT_STRING ) : NULL;
   const char * szData = zh_parc( 1 );

   if( szData )
   {
      ZH_SIZE nLen = zh_parclen( 1 );

      if( nLen )
      {
         ZH_SIZE nDstLen;
         char * pDest = NULL;
         int iResult = Z_OK;

         if( pBuffer )
         {
            if( ! zh_itemGetWriteCL( pBuffer, &pDest, &nDstLen ) )
               iResult = Z_MEM_ERROR;
         }
         else
         {
            nDstLen = ZH_IS_PARAM_NUM( 2 ) ? ( ZH_SIZE ) zh_parns( 2 ) :
                           s_zlibUncompressedSize( szData, nLen, &iResult );
            if( iResult == Z_OK )
            {
               pDest = ( char * ) zh_xalloc( nDstLen + 1 );
               if( ! pDest )
                  iResult = Z_MEM_ERROR;
            }
         }

         if( iResult == Z_OK )
         {
            iResult = s_zlibUncompress( pDest, &nDstLen, szData, nLen );

            if( ! pBuffer )
            {
               if( iResult == Z_OK )
                  zh_retclen_buffer( pDest, nDstLen );
               else
                  zh_xfree( pDest );
            }
            else if( iResult == Z_OK )
               zh_retclen( pDest, nDstLen );
         }
         zh_storni( iResult, 3 );
      }
      else
      {
         zh_retc_null();
         zh_storni( Z_OK, 3 );
      }
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/*
 * zh_gzCompressBound( <cData> | <nDataLen> ) --> <nMaxCompressLen>
 */
ZH_FUNC( ZH_GZCOMPRESSBOUND )
{
   if( ZH_ISCHAR( 1 ) )
      zh_retnint( s_zlibCompressBound( ( uLong ) zh_parclen( 1 ) ) + 12 );
   else if( ZH_IS_PARAM_NUM( 1 ) )
      zh_retnint( s_zlibCompressBound( ( uLong ) zh_parns( 1 ) ) + 12 );
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/*
 * zh_gzCompress( <cData>, [<nDstBufLen>|<@cBuffer>], [<@nResult>], [<nLevel>] )
 *    => <cCompressedData> or NIL on Error
 *
 * Note: this function does not create any references to gz* ZLIB functions
 *       so it's intentionally here not in hbzlibgz.c file.
 */
ZH_FUNC( ZH_GZCOMPRESS )
{
   const char * szData = zh_parc( 1 );

   if( szData )
   {
      ZH_SIZE nLen = zh_parclen( 1 );

      if( nLen )
      {
         PZH_ITEM pBuffer = ZH_ISBYREF( 2 ) ? zh_param( 2, ZH_IT_STRING ) : NULL;
         ZH_BOOL fAlloc = ZH_FALSE;
         ZH_SIZE nDstLen;
         char * pDest;
         int iResult;

         if( pBuffer )
         {
            if( ! zh_itemGetWriteCL( pBuffer, &pDest, &nDstLen ) )
               pDest = NULL;
         }
         else
         {
            if( ZH_IS_PARAM_NUM( 2 ) )
            {
               nDstLen = zh_parns( 2 );
               pDest = ( char * ) zh_xalloc( nDstLen + 1 );
            }
            else
            {
               pDest = NULL;
               nDstLen = 0;
               fAlloc = ZH_TRUE;
            }
         }

         if( pDest || fAlloc )
         {
            iResult = s_zlibCompress2( &pDest, &nDstLen, szData, nLen, ZH_TRUE,
                                       zh_parnidef( 4, Z_DEFAULT_COMPRESSION ) );
            if( ! pBuffer )
            {
               if( iResult == Z_OK )
                  zh_retclen_buffer( pDest, nDstLen );
               else if( pDest )
                  zh_xfree( pDest );
            }
            else if( iResult == Z_OK )
               zh_retclen( pDest, nDstLen );
         }
         else
            iResult = Z_MEM_ERROR;

         zh_storni( iResult, 3 );
      }
      else
      {
         zh_retc_null();
         zh_storni( Z_OK, 3 );
      }
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/*
 * zh_ZError( <nError> ) => <cErrorDescription>
 */
ZH_FUNC( ZH_ZERROR )
{
#if defined( ZH_OS_QNX )
   /* NOTE: Hack to avoid "undefined reference to 'zlibVersion' when linking hbrun on QNX 6.2.1. */
   zh_retc_null();
#else
   zh_retc( zError( zh_parni( 1 ) ) );
#endif
}

ZH_CALL_ON_STARTUP_BEGIN( _zh_zlib_init_ )
   zh_zlibInit( s_zlibCompressBound, s_zlibUncompressedSize,
                s_zlibCompress, s_zlibUncompress );
ZH_CALL_ON_STARTUP_END( _zh_zlib_init_ )

#if defined( ZH_PRAGMA_STARTUP )
   #pragma startup _zh_zlib_init_
#elif defined( ZH_DATASEG_STARTUP )
   #define ZH_DATASEG_BODY    ZH_DATASEG_FUNC( _zh_zlib_init_ )
   #include "zh_ini_seg.h"
#endif
