/*
 * ZLIB compression for Ziher stream sockets
 *
 * Copyright 2010 Przemyslaw Czerpak
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
/* this has to be declared before zh_znet.h is included */
#define _ZH_ZNET_INTERNAL_

#include "zh_api.h"
#include "zh_znet.h"
#include "zh_bfish.h"
#include "zlib.zhh"
#include "zh_init.h"

#include <zlib.h>

typedef struct _ZH_ZNETSTREAM
{
   z_stream rd;         /* input stream */
   z_stream wr;         /* output stream */
   int      err;        /* error code for last stream operation */
   int      crypt;      /* encryption */
   uInt     crypt_in;   /* number of encrypted characters in buffer  */
   uInt     crypt_size; /* size of encrypted block */
   uInt     skip_in;    /* encryption block padding bytes */
   uInt     skip_out;   /* room for block size */
   Bytef *  crypt_out;  /* block size offset for encrypted blocks */
   Bytef *  inbuf;      /* input buffer */
   Bytef *  outbuf;     /* output buffer */
   ZH_BLOWFISH * bf;
}
ZH_ZNETSTREAM;

#define ZH_ZNET_BUFSIZE       0x4000
#define ZH_ZNET_READAHEAD     0x40

#if MAX_MEM_LEVEL >= 8
#  define ZH_ZNET_MEM_LEVEL   8
#else
#  define ZH_ZNET_MEM_LEVEL   MAX_MEM_LEVEL
#endif

/* return status of last compression/decompression operation */
int zh_znetError( PZH_ZNETSTREAM pStream )
{
   return pStream->err;
}

/* release stream structure
 */
void zh_znetClose( PZH_ZNETSTREAM pStream )
{
   if( pStream->inbuf )
      zh_xfree( pStream->inbuf );

   if( pStream->outbuf )
      zh_xfree( pStream->outbuf );

   if( pStream->bf )
      zh_xfree( pStream->bf );

   deflateEnd( &pStream->wr );
   inflateEnd( &pStream->rd );

   zh_xfree( pStream );
}

/* create new stream structure
 */
PZH_ZNETSTREAM zh_znetOpen( int level, int strategy )
{
   PZH_ZNETSTREAM pStream = ( PZH_ZNETSTREAM ) zh_xgrabz( sizeof( ZH_ZNETSTREAM ) );

   if( level != Z_DEFAULT_COMPRESSION &&
       !( level >= Z_NO_COMPRESSION && level <= Z_BEST_COMPRESSION ) )
      level = Z_DEFAULT_COMPRESSION;

   if( strategy != Z_FILTERED     &&
#if defined( Z_RLE )
       strategy != Z_RLE          &&
#endif
#if defined( Z_FIXED )
       strategy != Z_FIXED        &&
#endif
       strategy != Z_HUFFMAN_ONLY )
      strategy = Z_DEFAULT_STRATEGY;

   if( deflateInit2( &pStream->wr, level,
                     Z_DEFLATED, -MAX_WBITS, ZH_ZNET_MEM_LEVEL, strategy ) == Z_OK )
   {
      pStream->wr.next_out = pStream->outbuf = ( Bytef * ) zh_xgrab( ZH_ZNET_BUFSIZE );
      pStream->wr.avail_out = ZH_ZNET_BUFSIZE;

      pStream->rd.next_in = pStream->inbuf = ( Bytef * ) zh_xgrab( ZH_ZNET_BUFSIZE );
      if( inflateInit2( &pStream->rd, -MAX_WBITS ) == Z_OK )
         return pStream;
   }

   zh_znetClose( pStream );
   return NULL;
}

/* set encryption key
 */
void zh_znetEncryptKey( PZH_ZNETSTREAM pStream, const void * keydata, int keylen )
{
   if( pStream->crypt == 0 )
   {
      pStream->crypt = 1;

      /* initialize encryption key */
      pStream->bf = ( ZH_BLOWFISH * ) zh_xgrab( sizeof( ZH_BLOWFISH ) );
      zh_blowfishInit( pStream->bf, keydata, keylen );

      /* initialize input buffer */
      pStream->skip_in = 0;
      pStream->crypt_size = 0;
      pStream->crypt_in = pStream->rd.avail_in;
      pStream->rd.avail_in = 0;

      /* initialize output buffer */
      pStream->crypt_out = pStream->wr.next_out;
      pStream->wr.next_out += 2;
      if( pStream->wr.avail_out < 2 )
         pStream->skip_out = 2 - pStream->wr.avail_out;
      else
         pStream->skip_out = 0;
      pStream->wr.avail_out -= 2 - pStream->skip_out;
   }
}

static void zh_znetDecrypt( PZH_ZNETSTREAM pStream, Bytef * data )
{
   ZH_U32 xl, xr;

   xl = ZH_GET_BE_UINT32( data );
   xr = ZH_GET_BE_UINT32( data + 4 );
   zh_blowfishDecrypt( pStream->bf, &xl, &xr );
   ZH_PUT_BE_UINT32( data, xl );
   ZH_PUT_BE_UINT32( data + 4, xr );
}

static void zh_znetEncrypt( PZH_ZNETSTREAM pStream, Bytef * data )
{
   ZH_U32 xl, xr;

   xl = ZH_GET_BE_UINT32( data );
   xr = ZH_GET_BE_UINT32( data + 4 );
   zh_blowfishEncrypt( pStream->bf, &xl, &xr );
   ZH_PUT_BE_UINT32( data, xl );
   ZH_PUT_BE_UINT32( data + 4, xr );
}

/* read data using stream structure
 */
long zh_znetRead( PZH_ZNETSTREAM pStream, ZH_SOCKET sd, void * buffer, long len, ZH_MAXINT timeout )
{
   long rec = 0;

   pStream->rd.next_out = ( Bytef * ) buffer;
   pStream->rd.avail_out = ( uInt ) len;
   pStream->err = Z_OK;

   while( pStream->rd.avail_out )
   {
      if( pStream->rd.avail_in == 0 && pStream->err == Z_BUF_ERROR )
      {
         if( pStream->skip_in )
         {
            pStream->rd.next_in += pStream->skip_in;
            pStream->skip_in = 0;
         }
         if( pStream->crypt_in && pStream->rd.next_in > pStream->inbuf )
            memmove( pStream->inbuf, pStream->rd.next_in, pStream->crypt_in );
         pStream->rd.next_in = pStream->inbuf;

         if( ! pStream->crypt || pStream->crypt_in < 8 )
         {
            if( pStream->rd.avail_out != ( uInt ) len )
               timeout = 0;
            rec = zh_socketRecv( sd, pStream->inbuf + pStream->crypt_in, ZH_ZNET_BUFSIZE - pStream->crypt_in, 0, timeout );
            if( rec <= 0 )
               break;
         }

         if( pStream->crypt )
         {
            pStream->crypt_in += rec;
            if( pStream->crypt_size == 0 )
            {
               if( pStream->crypt_in >= 8 )
               {
                  zh_znetDecrypt( pStream, pStream->rd.next_in );
                  pStream->crypt_size = ZH_GET_BE_UINT16( pStream->rd.next_in );
                  pStream->rd.next_in += 2;
                  pStream->crypt_in -= 8;
                  rec = ZH_MIN( pStream->crypt_size, 6 );
                  pStream->crypt_size -= ( uInt ) rec;
                  pStream->rd.avail_in += ( uInt ) rec;
                  pStream->skip_in = ( uInt ) ( 6 - rec );
                  rec = 0;
               }
            }
            if( pStream->skip_in == 0 )
            {
               long l = ( pStream->crypt_size + 0x07 ) & ~0x07;
               rec = pStream->crypt_in & ~0x07;
               if( rec > l )
                  rec = l;
               /* decrypt the buffer */
               for( l = 0; l < rec; l += 8 )
                  zh_znetDecrypt( pStream, pStream->rd.next_in + pStream->rd.avail_in + l );
               pStream->crypt_in -= rec;
               if( ( uInt ) rec > pStream->crypt_size )
               {
                  pStream->skip_in = rec - pStream->crypt_size;
                  rec = pStream->crypt_size;
               }
               pStream->crypt_size -= rec;
            }
         }

         pStream->rd.avail_in += ( uInt ) rec;
         rec = 0;
         if( pStream->rd.avail_in == 0 )
         {
            if( pStream->rd.avail_out == ( uInt ) len )
               continue;
            else
               break;
         }
      }
      pStream->err = inflate( &pStream->rd, Z_SYNC_FLUSH );
/*
      if( pStream->err == Z_STREAM_END && pStream->rd.avail_in == 0 )
         pStream->err = Z_BUF_ERROR;
 */
      if( pStream->err != Z_OK &&
          ! ( pStream->err == Z_BUF_ERROR && pStream->rd.avail_in == 0 ) )
      {
         zh_socketSetError( ZH_ZNET_SOCK_ERROR_BASE - pStream->err );
         rec = -1;
         break;
      }
   }

   len -= pStream->rd.avail_out;

   return len == 0 ? rec : len;
}

static long zh_znetStreamWrite( PZH_ZNETSTREAM pStream, ZH_SOCKET sd, ZH_MAXINT timeout )
{
   long snd = 0, rest =  0, tosnd;

   if( pStream->crypt )
   {
      rest = ( long ) ( pStream->wr.next_out - pStream->crypt_out );
      if( rest > 2 )
      {
         ZH_U16 uiLen = ( ZH_U16 ) ( rest - 2 );
         ZH_PUT_BE_UINT16( pStream->crypt_out, uiLen );
         uiLen = ( ZH_U16 ) ( ( ( rest + 0x07 ) ^ 0x07 ) & 0x07 );
         if( ( uInt ) uiLen <= pStream->wr.avail_out )
         {
            while( uiLen-- )
            {
               *pStream->wr.next_out++ = ( Byte ) 0; /* TODO: use better hashing data */
               pStream->wr.avail_out--;
               rest++;
            }
            /* encrypt the buffer */
            for( tosnd = 0; tosnd < rest; tosnd += 8 )
               zh_znetEncrypt( pStream, pStream->crypt_out + tosnd );
            rest = 0;
            pStream->crypt_out = pStream->wr.next_out;
            pStream->wr.next_out += 2;
            if( pStream->wr.avail_out < 2 )
               pStream->skip_out = 2 - pStream->wr.avail_out;
            pStream->wr.avail_out -= 2 - pStream->skip_out;
         }
      }
      else
         rest = 0;
      tosnd = ( long ) ( pStream->crypt_out - pStream->outbuf );
   }
   else
      tosnd = ZH_ZNET_BUFSIZE - pStream->wr.avail_out;

   if( tosnd > 0 )
   {
      snd = zh_socketSend( sd, pStream->outbuf, tosnd, 0, timeout );
      if( snd > 0 )
      {
         tosnd += rest - snd;
         if( tosnd > 0 )
            memmove( pStream->outbuf, pStream->outbuf + snd, tosnd );
         pStream->wr.avail_out += ( uInt ) snd;
         pStream->wr.next_out -= snd;
         pStream->crypt_out -= snd;
         if( pStream->skip_out )
         {
            if( pStream->skip_out <= pStream->wr.avail_out )
            {
               pStream->wr.avail_out -= pStream->skip_out;
               pStream->skip_out = 0;
            }
            else
            {
               pStream->skip_out -= pStream->wr.avail_out;
               pStream->wr.avail_out = 0;
            }
         }
      }
   }
   return snd;
}

/* flush data in stream structure - return number of bytes left in the
 * buffer which were not sent
 */
long zh_znetFlush( PZH_ZNETSTREAM pStream, ZH_SOCKET sd, ZH_MAXINT timeout,
                   ZH_BOOL fSync )
{
   uInt uiSize = ZH_ZNET_BUFSIZE - ( pStream->crypt ? 2 : 0 );

   if( fSync && pStream->wr.avail_out == uiSize &&
       pStream->wr.total_in == 0 && pStream->wr.total_out == 0 )
      return 0;

   if( pStream->wr.avail_out > 0 )
      pStream->err = deflate( &pStream->wr,
                              fSync ? Z_FULL_FLUSH : Z_PARTIAL_FLUSH );
   else
      pStream->err = Z_OK;

   while( pStream->wr.avail_out < uiSize )
   {
      if( zh_znetStreamWrite( pStream, sd, timeout ) <= 0 )
         break;

      if( pStream->err == Z_OK || pStream->err == Z_BUF_ERROR )
         pStream->err = deflate( &pStream->wr,
                                 fSync ? Z_FULL_FLUSH : Z_PARTIAL_FLUSH );
   }

   if( pStream->err == Z_BUF_ERROR )
      pStream->err = Z_OK;

   if( pStream->err != Z_OK )
      zh_socketSetError( ZH_ZNET_SOCK_ERROR_BASE - pStream->err );

   return uiSize - pStream->wr.avail_out;
}

/* write data using stream structure
 */
long zh_znetWrite( PZH_ZNETSTREAM pStream, ZH_SOCKET sd, const void * buffer, long len, ZH_MAXINT timeout, long * plast )
{
   long snd = 0;

   pStream->wr.next_in = ( Bytef * ) ZH_UNCONST( buffer );
   pStream->wr.avail_in = ( uInt ) len;
   pStream->err = Z_OK;

   while( pStream->wr.avail_in )
   {
      if( pStream->wr.avail_out == 0 )
      {
         snd = zh_znetStreamWrite( pStream, sd, timeout );
         if( plast )
            *plast = snd;
         if( snd <= 0 )
            break;
         snd = 0;
      }
      pStream->err = deflate( &pStream->wr, Z_NO_FLUSH );
      if( pStream->err != Z_OK )
      {
         if( pStream->err == Z_BUF_ERROR )
            pStream->err = Z_OK;
         else
         {
            zh_socketSetError( ZH_ZNET_SOCK_ERROR_BASE - pStream->err );
            snd = -1;
         }
         break;
      }
   }

   len -= pStream->wr.avail_in;

   return len == 0 ? snd : len;
}

/* socket filter */

#define ZH_ZNET_GET( p )      ( ( PZH_ZNETSTREAM ) p->cargo )

static PZH_SOCKEX s_sockexNew( ZH_SOCKET sd, PZH_ITEM pParams )
{
   PZH_SOCKEX pSock;
   const void * keydata = NULL;
   int keylen = 0,
       level = ZH_ZLIB_COMPRESSION_DEFAULT,
       strategy = ZH_ZLIB_STRATEGY_DEFAULT;

   zh_socekxParamsGetStd( pParams, &keydata, &keylen, NULL, NULL,
                          &level, &strategy );

   pSock = zh_sockexNewZNet( sd, keydata, keylen, level, strategy );
   if( pSock )
      zh_socekxParamsInit( pSock, pParams );

   return pSock;
}

/* this wrapper does not support multilevel filtering so
   it destroys previous wrappers if any and create new one.
 */
static PZH_SOCKEX s_sockexNext( PZH_SOCKEX pSock, PZH_ITEM pParams )
{
   PZH_SOCKEX pSockNew = NULL;

   if( pSock && pSock->sd != ZH_NO_SOCKET )
   {
      pSockNew = s_sockexNew( pSock->sd, pParams );
      if( pSockNew )
         zh_sockexClose( pSock, ZH_FALSE );
   }

   return pSockNew;
}

static int s_sockexClose( PZH_SOCKEX pSock, ZH_BOOL fClose )
{
   int iResult;

   if( pSock->cargo )
   {
      if( pSock->sd != ZH_NO_SOCKET )
         zh_znetFlush( ZH_ZNET_GET( pSock ), pSock->sd,
                       ZH_MAX( 15000, pSock->iAutoFlush ), ZH_TRUE );
      zh_znetClose( ZH_ZNET_GET( pSock ) );
   }

   iResult = zh_sockexRawClear( pSock, fClose );
   zh_xfree( pSock );

   return iResult;
}

static long s_sockexRead( PZH_SOCKEX pSock, void * data, long len, ZH_MAXINT timeout )
{
   long lRead = ZH_MIN( pSock->inbuffer, len );

   if( lRead > 0 )
   {
      memcpy( data, pSock->buffer + pSock->posbuffer, lRead );
      pSock->inbuffer -= lRead;
      if( pSock->inbuffer )
         pSock->posbuffer += lRead;
      else
         pSock->posbuffer = 0;
      len -= lRead;
      if( len == 0 || pSock->sd == ZH_NO_SOCKET )
         return lRead;
      data = ( ZH_BYTE * ) data + lRead;
      timeout = 0;
   }
   else if( pSock->sd == ZH_NO_SOCKET )
   {
      zh_socketSetError( ZH_SOCKET_ERR_INVALIDHANDLE );
      return -1;
   }

   len = pSock->cargo ? zh_znetRead( ZH_ZNET_GET( pSock ), pSock->sd, data, len, timeout ) :
                        zh_socketRecv( pSock->sd, data, len, 0, timeout );

   return lRead > 0 ? ZH_MAX( len, 0 ) + lRead : len;
}

static long s_sockexWrite( PZH_SOCKEX pSock, const void * data, long len, ZH_MAXINT timeout )
{
   if( pSock->sd == ZH_NO_SOCKET )
   {
      zh_socketSetError( ZH_SOCKET_ERR_INVALIDHANDLE );
      return -1;
   }
   return pSock->cargo ? zh_znetWrite( ZH_ZNET_GET( pSock ), pSock->sd, data, len, timeout, NULL ) :
                         zh_socketSend( pSock->sd, data, len, 0, timeout );
}

static long s_sockexFlush( PZH_SOCKEX pSock, ZH_MAXINT timeout, ZH_BOOL fSync )
{
   if( pSock->sd == ZH_NO_SOCKET )
   {
      zh_socketSetError( ZH_SOCKET_ERR_INVALIDHANDLE );
      return -1;
   }
   return pSock->cargo ? zh_znetFlush( ZH_ZNET_GET( pSock ), pSock->sd, timeout, fSync ) : 0;
}

static int s_sockexCanRead( PZH_SOCKEX pSock, ZH_BOOL fBuffer, ZH_MAXINT timeout )
{
   if( pSock->inbuffer )
      return 1;
   else if( pSock->sd == ZH_NO_SOCKET )
   {
      zh_socketSetError( ZH_SOCKET_ERR_INVALIDHANDLE );
      return -1;
   }
   else if( pSock->cargo )
   {
      long len;

      if( pSock->buffer == NULL )
      {
         if( pSock->readahead <= 0 )
            pSock->readahead = ZH_ZNET_READAHEAD;
         pSock->buffer = ( ZH_BYTE * ) zh_xgrab( pSock->readahead );
      }
      len = zh_znetRead( ZH_ZNET_GET( pSock ), pSock->sd, pSock->buffer,
                         pSock->readahead, 0 );
      if( len > 0 )
      {
         pSock->inbuffer = len;
         len = 1;
      }
      return ( int ) len;
   }
   return fBuffer ? 0 : zh_socketSelectRead( pSock->sd, timeout );
}

static int s_sockexCanWrite( PZH_SOCKEX pSock, ZH_BOOL fBuffer, ZH_MAXINT timeout )
{
   if( pSock->sd == ZH_NO_SOCKET )
   {
      zh_socketSetError( ZH_SOCKET_ERR_INVALIDHANDLE );
      return -1;
   }
   else if( fBuffer && pSock->cargo && /* ZH_ZNET_GET( pSock )->wr.avail_out > 0 && */
            ( uInt ) ( ZH_ZNET_BUFSIZE - ( ZH_ZNET_GET( pSock )->crypt ? 2 : 0 ) ) <=
            ZH_ZNET_GET( pSock )->wr.avail_out )
      return 1;
   else
      return fBuffer ? 0 : zh_socketSelectWrite( pSock->sd, timeout );
}

static char * s_sockexName( PZH_SOCKEX pSock )
{
   return zh_strdup( pSock->pFilter->pszName );
}

static const char * s_sockexErrorStr( PZH_SOCKEX pSock, int iError )
{
   ZH_SYMBOL_UNUSED( pSock );

   switch( ZH_ZNET_SOCK_ERROR_BASE - iError )
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

   return zh_socketErrorStr( iError );
}

static const ZH_SOCKET_FILTER s_sockFilter =
{
   "znet",
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

PZH_SOCKEX zh_sockexNewZNet( ZH_SOCKET sd, const void * keydata, int keylen,
                             int level, int strategy )
{
   PZH_SOCKEX pSock = NULL;

   if( sd != ZH_NO_SOCKET )
   {
      PZH_ZNETSTREAM pStream = NULL;

      if( level != ZH_ZLIB_COMPRESSION_DISABLE )
      {
         pStream = zh_znetOpen( level, strategy );
         if( pStream )
         {
            if( keydata && keylen > 0 )
               zh_znetEncryptKey( pStream, keydata, keylen );
         }
         else
            sd = ZH_NO_SOCKET;
      }
      if( sd != ZH_NO_SOCKET )
      {
         pSock = ( PZH_SOCKEX ) zh_xgrabz( sizeof( ZH_SOCKEX ) );
         pSock->sd = sd;
         pSock->fRedirAll = ZH_TRUE;
         pSock->fShutDown = ZH_TRUE;
         pSock->pFilter = &s_sockFilter;
         pSock->cargo = ( void * ) pStream;
      }
   }

   return pSock;
}

/* zh_socketNewZNet( <pSocket>, [<cPass>], ;
                     [<nCompressionLevel>], [<nStrategy>] ) --> <pSocket>
 */
ZH_FUNC( ZH_SOCKETNEWZNET )
{
   ZH_SOCKET sd = zh_socketParam( 1 );

   if( sd != ZH_NO_SOCKET )
   {
      PZH_SOCKEX pSock;

      if( ZH_ISHASH( 2 ) )
      {
         pSock = zh_sockexNew( sd, s_sockFilter.pszName, zh_param( 2, ZH_IT_ANY ) );
      }
      else
      {
         const char * keydata = zh_parc( 2 );
         int keylen = ( int ) zh_parclen( 2 ),
             level = zh_parnidef( 3, ZH_ZLIB_COMPRESSION_DEFAULT ),
             strategy = zh_parnidef( 4, ZH_ZLIB_STRATEGY_DEFAULT );

         pSock = zh_sockexNewZNet( sd, keydata, keylen, level, strategy );
      }

      if( pSock )
      {
         zh_socketItemClear( zh_param( 1, ZH_IT_POINTER ) );
         zh_sockexItemPut( zh_param( -1, ZH_IT_ANY ), pSock );
      }
   }
}

ZH_CALL_ON_STARTUP_BEGIN( _zh_znet_init_ )
   zh_sockexRegister( &s_sockFilter );
ZH_CALL_ON_STARTUP_END( _zh_znet_init_ )

#if defined( ZH_PRAGMA_STARTUP )
   #pragma startup _zh_znet_init_
#elif defined( ZH_DATASEG_STARTUP )
   #define ZH_DATASEG_BODY  ZH_DATASEG_FUNC( _zh_znet_init_ )
   #include "../zh_ini_seg.h"
#endif
