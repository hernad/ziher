/*
 * Ziher extended socket filter with ZLIB compression
 *
 * Copyright 2015 Przemyslaw Czerpak
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

/* this has to be declared before zh_socket.h is included */
#define _ZH_SOCKEX_IMPLEMENTATION_

#include "zh_api.h"
#include "zh_item_api.h"
#include "zh_api_error.h"
#include "../zh_socket.h"
#include "zh_init.h"
#include "zlib.zhh"

#include <zlib.h>

#define ZH_ZSOCK_ERROR_BASE   100

#define ZH_ZSOCK_READAHEAD    0x40
#define ZH_ZSOCK_RDBUFSIZE    4096
#define ZH_ZSOCK_WRBUFSIZE    4096

#if MAX_MEM_LEVEL >= 8
#  define ZH_ZSOCK_MEM_LEVEL  8
#else
#  define ZH_ZSOCK_MEM_LEVEL  MAX_MEM_LEVEL
#endif

#define ZH_ZSOCK_GET( p )     ( ( PZH_SOCKEX_Z ) p->cargo )

typedef struct
{
   PZH_SOCKEX     sock;

   ZH_BOOL        fDecompressIn;
   ZH_BOOL        fCompressOut;

   z_stream       z_read;
   z_stream       z_write;

   ZH_BYTE *      rdbuf;
   ZH_BYTE *      wrbuf;
}
ZH_SOCKEX_Z, * PZH_SOCKEX_Z;

static voidpf s_zsock_zalloc( voidpf opaque, uInt items, uInt size )
{
   ZH_SYMBOL_UNUSED( opaque );
   return zh_xalloc( ( ZH_SIZE ) items * size );
}

static void s_zsock_zfree( voidpf opaque, voidpf address )
{
   ZH_SYMBOL_UNUSED( opaque );
   zh_xfree( address );
}

static long s_zsock_write( PZH_SOCKEX_Z pZ, ZH_MAXINT timeout )
{
   long lSent = 0, len = ZH_ZSOCK_WRBUFSIZE - pZ->z_write.avail_out;

   while( lSent < len )
   {
      long l = zh_sockexWrite( pZ->sock, pZ->wrbuf + lSent, len - lSent, timeout );
      if( l <= 0 )
      {
         switch( zh_socketGetError() )
         {
            case ZH_SOCKET_ERR_TIMEOUT:
            case ZH_SOCKET_ERR_AGAIN:
            case ZH_SOCKET_ERR_TRYAGAIN:
               break;
            default:
               lSent = -1;
               break;
         }
         break;
      }

      lSent += l;
      if( timeout > 0 )
         timeout = 0;
   }

   if( lSent > 0 )
   {
      if( lSent < len )
         memmove( pZ->wrbuf, pZ->wrbuf + lSent, len - lSent );
      pZ->z_write.avail_out += lSent;
      pZ->z_write.next_out -= lSent;
   }

   return lSent;
}

static int s_zsock_inbuffer( PZH_SOCKEX pSock )
{
   PZH_SOCKEX_Z pZ = ZH_ZSOCK_GET( pSock );

   if( pSock->inbuffer == 0 && pZ->fDecompressIn )
   {
      int err;

      if( pSock->buffer == NULL )
      {
         if( pSock->readahead <= 0 )
            pSock->readahead = ZH_ZSOCK_READAHEAD;
         pSock->buffer = ( ZH_BYTE * ) zh_xgrab( pSock->readahead );
      }

      pZ->z_read.next_out  = ( Bytef * ) pSock->buffer;
      pZ->z_read.avail_out = ( uInt ) pSock->readahead;

      err = inflate( &pZ->z_read, Z_SYNC_FLUSH );
      if( err != Z_OK && err != Z_BUF_ERROR )
         zh_socketSetError( ZH_ZSOCK_ERROR_BASE - err );
      pSock->inbuffer = pSock->readahead - pZ->z_read.avail_out;
   }
   return pSock->inbuffer > 0 ? 1 : 0;
}


/* socket filter */

static long s_sockexRead( PZH_SOCKEX pSock, void * data, long len, ZH_MAXINT timeout )
{
   PZH_SOCKEX_Z pZ = ZH_ZSOCK_GET( pSock );
   long lRecv = 0;

   if( pSock->inbuffer > 0 && len > 0 )
   {
      lRecv = ZH_MIN( pSock->inbuffer, len );
      memcpy( data, pSock->buffer + pSock->posbuffer, lRecv );
      if( ( pSock->inbuffer -= lRecv ) > 0 )
         pSock->posbuffer += lRecv;
      else
         pSock->posbuffer = 0;
      return lRecv;
   }
   else if( pZ->fDecompressIn )
   {
      int err = Z_OK;

      pZ->z_read.next_out  = ( Bytef * ) data;
      pZ->z_read.avail_out = ( uInt ) len;
      pZ->z_read.total_out = 0;

      while( pZ->z_read.avail_out )
      {
         if( err == Z_BUF_ERROR && pZ->z_read.avail_in == 0 )
         {
            lRecv = zh_sockexRead( pZ->sock, pZ->rdbuf, ZH_ZSOCK_RDBUFSIZE,
                                   pZ->z_read.total_out == 0 ? timeout : 0 );
            if( lRecv <= 0 )
               break;
            pZ->z_read.next_in = ( Bytef * ) pZ->rdbuf;
            pZ->z_read.avail_in = ( uInt ) lRecv;
         }
         else if( err != Z_OK )
         {
            zh_socketSetError( ZH_ZSOCK_ERROR_BASE - err );
            lRecv = -1;
            break;
         }
         err = inflate( &pZ->z_read, Z_SYNC_FLUSH );
      }

      if( pZ->z_read.total_out != 0 )
         lRecv = ( long ) pZ->z_read.total_out;

      return lRecv;
   }
   else
      return zh_sockexRead( pZ->sock, data, len, timeout );
}

static long s_sockexWrite( PZH_SOCKEX pSock, const void * data, long len, ZH_MAXINT timeout )
{
   PZH_SOCKEX_Z pZ = ZH_ZSOCK_GET( pSock );

   if( pZ->fCompressOut )
   {
      long lWritten = 0;

      pZ->z_write.next_in  = ( Bytef * ) ZH_UNCONST( data );
      pZ->z_write.avail_in = ( uInt ) len;

      while( pZ->z_write.avail_in )
      {
         int err;

         if( pZ->z_write.avail_out == 0 )
         {
            lWritten = s_zsock_write( pZ, timeout );
            if( lWritten <= 0 )
               break;
            timeout = 0;
         }
         err = deflate( &pZ->z_write, Z_NO_FLUSH );
         if( err != Z_OK )
         {
            if( err != Z_BUF_ERROR )
            {
               zh_socketSetError( ZH_ZSOCK_ERROR_BASE - err );
               lWritten = -1;
            }
            break;
         }
      }

      return lWritten >= 0 ? ( long ) ( len - pZ->z_write.avail_in ) : lWritten;
   }
   else
      return zh_sockexWrite( pZ->sock, data, len, timeout );
}

static long s_sockexFlush( PZH_SOCKEX pSock, ZH_MAXINT timeout, ZH_BOOL fSync )
{
   PZH_SOCKEX_Z pZ = ZH_ZSOCK_GET( pSock );
   long lResult = 0;

   if( pZ->fCompressOut &&
       ( ! fSync || pZ->z_write.avail_out != ZH_ZSOCK_WRBUFSIZE ||
         pZ->z_write.total_in != 0 || pZ->z_write.total_out != 0 ) )
   {
      int err;

      if( pZ->z_write.avail_out > 0 )
         err = deflate( &pZ->z_write, fSync ? Z_FULL_FLUSH : Z_PARTIAL_FLUSH );
      else
         err = Z_OK;

      while( pZ->z_write.avail_out < ZH_ZSOCK_WRBUFSIZE )
      {
         if( s_zsock_write( pZ, timeout ) <= 0 )
            break;
         if( err == Z_OK || err == Z_BUF_ERROR )
            err = deflate( &pZ->z_write, fSync ? Z_FULL_FLUSH : Z_PARTIAL_FLUSH );
      }
      if( err != Z_OK && err != Z_BUF_ERROR )
         zh_socketSetError( ZH_ZSOCK_ERROR_BASE - err );
      lResult = ZH_ZSOCK_WRBUFSIZE - pZ->z_write.avail_out;
   }
   return lResult + zh_sockexFlush( pZ->sock, timeout, fSync );
}

static int s_sockexCanRead( PZH_SOCKEX pSock, ZH_BOOL fBuffer, ZH_MAXINT timeout )
{
   return s_zsock_inbuffer( pSock ) ? 1 :
          zh_sockexCanRead( ZH_ZSOCK_GET( pSock )->sock, fBuffer, timeout );
}

static int s_sockexCanWrite( PZH_SOCKEX pSock, ZH_BOOL fBuffer, ZH_MAXINT timeout )
{
   return zh_sockexCanWrite( ZH_ZSOCK_GET( pSock )->sock, fBuffer, timeout );
}

static char * s_sockexName( PZH_SOCKEX pSock )
{
   char * pszName = zh_sockexIsRaw( ZH_ZSOCK_GET( pSock )->sock ) ? NULL :
                    zh_sockexName( ZH_ZSOCK_GET( pSock )->sock );
   if( pszName )
   {
      char * pszFree = pszName;
      pszName = zh_xstrcpy( NULL, pSock->pFilter->pszName, "|", pszName, NULL );
      zh_xfree( pszFree );
   }
   else
      pszName = zh_strdup( pSock->pFilter->pszName );

   return pszName;
}

static const char * s_sockexErrorStr( PZH_SOCKEX pSock, int iError )
{
   switch( ZH_ZSOCK_ERROR_BASE - iError )
   {
      case Z_STREAM_END:
         return "Z_STREAM_END";
      case Z_NEED_DICT:
         return "Z_NEED_DICT";
      case Z_ERRNO:
         return "Z_ERRNO";
      case Z_STREAM_ERROR:
         return "Z_STREAM_ERROR";
      case Z_DATA_ERROR:
         return "Z_DATA_ERROR";
      case Z_MEM_ERROR:
         return "Z_MEM_ERROR";
      case Z_BUF_ERROR:
         return "Z_BUF_ERROR";
      case Z_VERSION_ERROR:
         return "Z_VERSION_ERROR";
   }

   return zh_sockexErrorStr( ZH_ZSOCK_GET( pSock )->sock, iError );
}

static int s_sockexClose( PZH_SOCKEX pSock, ZH_BOOL fClose )
{
   PZH_SOCKEX_Z pZ = ZH_ZSOCK_GET( pSock );
   int iResult = 0;

   if( pZ )
   {
      if( pZ->sock )
         s_sockexFlush( pSock, ZH_MAX( 15000, pSock->iAutoFlush ), ZH_TRUE );

      if( pZ->fDecompressIn )
         inflateEnd( &pZ->z_read );
      if( pZ->rdbuf )
         zh_xfree( pZ->rdbuf );
      if( pZ->fCompressOut )
         deflateEnd( &pZ->z_write );
      if( pZ->wrbuf )
         zh_xfree( pZ->wrbuf );

      if( pZ->sock )
      {
         if( pSock->fShutDown )
            pZ->sock->fShutDown = ZH_TRUE;
         if( pSock->iAutoFlush != 0 && pZ->sock->iAutoFlush == 0 )
            pZ->sock->iAutoFlush = pSock->iAutoFlush;
         iResult = zh_sockexClose( pZ->sock, fClose );
      }

      zh_xfree( pZ );
   }
   /* call zh_sockexRawClear() with fClose = ZH_FALSE because
      zh_sockexClose() already closed real socket */
   zh_sockexRawClear( pSock, ZH_FALSE );
   zh_xfree( pSock );

   return iResult;
}

static PZH_SOCKEX s_sockexNext( PZH_SOCKEX pSock, PZH_ITEM pParams );

static PZH_SOCKEX s_sockexNew( ZH_SOCKET sd, PZH_ITEM pParams )
{
   PZH_SOCKEX pSock, pSockNew = NULL;

   pSock = zh_sockexNew( sd, NULL, pParams );
   if( pSock )
   {
      pSockNew = s_sockexNext( pSock, pParams );
      if( pSockNew == NULL )
         zh_sockexClose( pSock, ZH_FALSE );
   }

   return pSockNew;
}

static const ZH_SOCKET_FILTER s_sockFilter =
{
   "ZSOCK",
   s_sockexNew,
   s_sockexNext,
   s_sockexClose,
   s_sockexRead,
   s_sockexWrite,
   s_sockexFlush,
   s_sockexCanRead,
   s_sockexCanWrite,
   s_sockexName,
   s_sockexErrorStr
};

static PZH_SOCKEX s_sockexNext( PZH_SOCKEX pSock, PZH_ITEM pParams )
{
   PZH_SOCKEX pSockNew = NULL;

   if( pSock )
   {
      ZH_BOOL fDecompressIn = ZH_TRUE, fCompressOut = ZH_TRUE;
      int level = ZH_ZLIB_COMPRESSION_DEFAULT,
          strategy = ZH_ZLIB_STRATEGY_DEFAULT,
          windowBitsIn = MAX_WBITS, windowBitsOut = MAX_WBITS;

      if( pParams && ZH_IS_HASH( pParams ) )
      {
         PZH_ITEM pItem;

         if( ( pItem = zh_hashGetCItemPtr( pParams, "zlib" ) ) != NULL &&
             ZH_IS_NUMERIC( pItem ) )
            level = zh_itemGetNI( pItem );
         if( ( pItem = zh_hashGetCItemPtr( pParams, "zs" ) ) != NULL &&
             ZH_IS_NUMERIC( pItem ) )
            strategy = zh_itemGetNI( pItem );

         if( ( pItem = zh_hashGetCItemPtr( pParams, "gzin" ) ) != NULL &&
             ZH_IS_LOGICAL( pItem ) )
         {
            fDecompressIn = zh_itemGetL( pItem );
            if( fDecompressIn )
               windowBitsIn += 16;
         }
         if( ( pItem = zh_hashGetCItemPtr( pParams, "zin" ) ) != NULL &&
             ZH_IS_LOGICAL( pItem ) )
         {
            if( windowBitsIn == MAX_WBITS )
               fDecompressIn = zh_itemGetL( pItem );
            else if( zh_itemGetL( pItem ) )
               windowBitsIn += 16;
         }

         if( ( pItem = zh_hashGetCItemPtr( pParams, "gzout" ) ) != NULL &&
             ZH_IS_LOGICAL( pItem ) )
         {
            fCompressOut = zh_itemGetL( pItem );
            if( fCompressOut )
               windowBitsOut += 16;
         }
         if( ( pItem = zh_hashGetCItemPtr( pParams, "zout" ) ) != NULL &&
             ZH_IS_LOGICAL( pItem ) && windowBitsOut == MAX_WBITS )
            fCompressOut = zh_itemGetL( pItem );
      }

      if( level != ZH_ZLIB_COMPRESSION_DISABLE &&
          ( fDecompressIn || fCompressOut ) )
      {
         PZH_SOCKEX_Z pZ = ( PZH_SOCKEX_Z ) zh_xgrabz( sizeof( ZH_SOCKEX_Z ) );

         pSockNew = ( PZH_SOCKEX ) zh_xgrabz( sizeof( ZH_SOCKEX ) );
         pSockNew->sd = ZH_NO_SOCKET;
         pSockNew->fRedirAll = ZH_TRUE;
         pSockNew->pFilter = &s_sockFilter;

         pSockNew->cargo = ( void * ) pZ;
         pZ->z_read.zalloc = s_zsock_zalloc;
         pZ->z_read.zfree  = s_zsock_zfree;
         pZ->z_read.opaque = Z_NULL;

         pZ->z_write.zalloc = s_zsock_zalloc;
         pZ->z_write.zfree  = s_zsock_zfree;
         pZ->z_write.opaque = Z_NULL;

         pZ->z_read.next_in  = NULL;
         pZ->z_read.avail_in = 0;

         if( level != Z_DEFAULT_COMPRESSION &&
             !( level >= Z_NO_COMPRESSION && level <= Z_BEST_COMPRESSION ) )
            level = Z_DEFAULT_COMPRESSION;

         if( strategy != Z_FILTERED    &&
#if defined( Z_RLE )
             strategy != Z_RLE         &&
#endif
#if defined( Z_FIXED )
             strategy != Z_FIXED       &&
#endif
             strategy != Z_HUFFMAN_ONLY )
            strategy = Z_DEFAULT_STRATEGY;

         if( fDecompressIn && level != ZH_ZLIB_COMPRESSION_DISABLE )
         {
            /* MAX_WBITS=15, decompression - support for formats:
             * -15: raw, 15: ZLIB, 31: GZIP, 47: ZLIB+GZIP
             */
            if( inflateInit2( &pZ->z_read, windowBitsIn ) == Z_OK )
            {
               pZ->fDecompressIn = ZH_TRUE;
               pZ->rdbuf = ( ZH_BYTE * ) zh_xgrab( ZH_ZSOCK_RDBUFSIZE );
            }
            else
               level = ZH_ZLIB_COMPRESSION_DISABLE;
         }

         if( fCompressOut && level != ZH_ZLIB_COMPRESSION_DISABLE )
         {
            /* MAX_WBITS=15, compression format:
             * -15: raw, 15: ZLIB (+6 bytes), 31: GZIP(+18 bytes)
             */
            if( deflateInit2( &pZ->z_write, level,
                              Z_DEFLATED, windowBitsOut, ZH_ZSOCK_MEM_LEVEL,
                              strategy ) == Z_OK )
            {
               pZ->fCompressOut = ZH_TRUE;
               pZ->wrbuf = ( ZH_BYTE * ) zh_xgrab( ZH_ZSOCK_WRBUFSIZE );
               pZ->z_write.next_out  = ( Bytef * ) pZ->wrbuf;
               pZ->z_write.avail_out = ZH_ZSOCK_WRBUFSIZE;
            }
            else
               level = ZH_ZLIB_COMPRESSION_DISABLE;
         }

         if( level != ZH_ZLIB_COMPRESSION_DISABLE )
         {
            pSockNew->sd = pSock->sd;
            pSockNew->fShutDown = pSock->fShutDown;
            pSockNew->iAutoFlush = pSock->iAutoFlush;
            pZ->sock = pSock;
            zh_socekxParamsInit( pSockNew, pParams );
         }
         else
         {
            s_sockexClose( pSockNew, ZH_FALSE );
            pSockNew = NULL;
         }
      }
   }

   return pSockNew;
}

/* zh_socketNewZSock( <pSocket>, [<hParams>] ) --> <pSocket> */
ZH_FUNC( ZH_SOCKETNEWZSOCK )
{
   PZH_SOCKEX pSock = zh_sockexParam( 1 );

   if( pSock )
   {
      pSock = s_sockexNext( pSock, zh_param( 2, ZH_IT_HASH ) );
      if( pSock )
      {
         zh_sockexItemClear( zh_param( 1, ZH_IT_POINTER ) );
         zh_sockexItemPut( zh_param( -1, ZH_IT_ANY ), pSock );
      }
   }
}


ZH_CALL_ON_STARTUP_BEGIN( _zh_zsock_init_ )
   zh_sockexRegister( &s_sockFilter );
ZH_CALL_ON_STARTUP_END( _zh_zsock_init_ )

#if defined( ZH_PRAGMA_STARTUP )
   #pragma startup _zh_zsock_init_
#elif defined( ZH_DATASEG_STARTUP )
   #define ZH_DATASEG_BODY  ZH_DATASEG_FUNC( _zh_zsock_init_ )
   #include "zh_ini_seg.h"
#endif
