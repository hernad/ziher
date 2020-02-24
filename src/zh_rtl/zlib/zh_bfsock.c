/*
 * Ziher extended socket filter with BlowFish encryption
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
#include "zh_error_api.h"
#include "../zh_socket.h"
#include "zh_bfish.h"
#include "zh_init.h"

#define ZH_BFSOCK_READAHEAD   0x40
#define ZH_BFSOCK_WRBUFSIZE   4096

#define ZH_BFSOCK_GET( p )    ( ( PZH_SOCKEX_BF ) p->cargo )

typedef struct
{
   PZH_SOCKEX     sock;

   ZH_BLOWFISH    bf;
   ZH_BYTE        encryptkey[ ZH_BF_CIPHERBLOCK ];
   ZH_BYTE        decryptkey[ ZH_BF_CIPHERBLOCK ];
   ZH_BYTE        encounter[ ZH_BF_CIPHERBLOCK ];
   ZH_BYTE        decounter[ ZH_BF_CIPHERBLOCK ];
   ZH_BYTE        buffer[ ZH_BFSOCK_WRBUFSIZE ];
   long           inbuffer;
   int            encoded;
   int            decoded;
}
ZH_SOCKEX_BF, * PZH_SOCKEX_BF;

static void s_bf_hash( const ZH_BLOWFISH * bf,
                       ZH_BYTE * vect, ZH_BYTE * counter )
{
   ZH_U32 xl, xr, cl, cr;

   cl = xl = ZH_GET_BE_UINT32( &counter[ 0 ] );
   cr = xr = ZH_GET_BE_UINT32( &counter[ 4 ] );
   ++cr;
   ZH_PUT_BE_UINT32( &counter[ 4 ], cr );
   if( cr == 0 )
   {
      ++cl;
      ZH_PUT_BE_UINT32( &counter[ 0 ], cl );
   }
   zh_blowfishEncrypt( bf, &xl, &xr );
   ZH_PUT_BE_UINT32( &vect[ 0 ], xl );
   ZH_PUT_BE_UINT32( &vect[ 4 ], xr );
}

static long s_bf_send( PZH_SOCKEX_BF pBF, ZH_MAXINT timeout )
{
   long lSent = 0, len = pBF->inbuffer;

   while( lSent < len )
   {
      long l = zh_sockexWrite( pBF->sock, pBF->buffer + lSent, len - lSent, timeout );
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
         memmove( pBF->buffer, pBF->buffer + lSent, len - lSent );
      pBF->inbuffer -= lSent;
   }

   return lSent;
}

/* socket filter */

static long s_sockexRead( PZH_SOCKEX pSock, void * data, long len, ZH_MAXINT timeout )
{
   PZH_SOCKEX_BF pBF = ZH_BFSOCK_GET( pSock );
   long lRecv;

   if( pSock->inbuffer > 0 && len > 0 )
   {
      lRecv = ZH_MIN( pSock->inbuffer, len );
      memcpy( data, pSock->buffer + pSock->posbuffer, lRecv );
      if( ( pSock->inbuffer -= lRecv ) > 0 )
         pSock->posbuffer += lRecv;
      else
         pSock->posbuffer = 0;
   }
   else
   {
      lRecv = zh_sockexRead( pBF->sock, data, len, timeout );
      if( lRecv > 0 )
      {
         ZH_BYTE * pData = ( ZH_BYTE * ) data;
         long l;

         for( l = 0; l < lRecv; ++l )
         {
            if( ( pBF->decoded & ( ZH_BF_CIPHERBLOCK - 1 ) ) == 0 )
            {
               s_bf_hash( &pBF->bf, pBF->decryptkey, pBF->decounter );
               pBF->decoded = 0;
            }
            pData[ l ] ^= pBF->decryptkey[ pBF->decoded++ ];
         }
      }
   }
   return lRecv;
}

static long s_sockexWrite( PZH_SOCKEX pSock, const void * data, long len, ZH_MAXINT timeout )
{
   PZH_SOCKEX_BF pBF = ZH_BFSOCK_GET( pSock );
   const ZH_BYTE * pData = ( const ZH_BYTE * ) data;
   long lWritten = 0, lDone;

   for( lDone = 0; lDone < len; ++lDone )
   {
      if( pBF->inbuffer == ZH_BFSOCK_WRBUFSIZE )
      {
         lWritten = s_bf_send( pBF, timeout );
         if( lWritten <= 0 )
            break;
         timeout = 0;
      }
      if( ( pBF->encoded & ( ZH_BF_CIPHERBLOCK - 1 ) ) == 0 )
      {
         s_bf_hash( &pBF->bf, pBF->encryptkey, pBF->encounter );
         pBF->encoded = 0;
      }
      pBF->buffer[ pBF->inbuffer++ ] = pData[ lDone ] ^ pBF->encryptkey[ pBF->encoded++ ];
   }

   return lWritten >= 0 ? lDone : lWritten;
}

static long s_sockexFlush( PZH_SOCKEX pSock, ZH_MAXINT timeout, ZH_BOOL fSync )
{
   PZH_SOCKEX_BF pBF = ZH_BFSOCK_GET( pSock );

   while( pBF->inbuffer > 0 )
   {
      if( s_bf_send( pBF, timeout ) <= 0 )
         break;
   }
   return pBF->inbuffer + zh_sockexFlush( pBF->sock, timeout, fSync );
}

static int s_sockexCanRead( PZH_SOCKEX pSock, ZH_BOOL fBuffer, ZH_MAXINT timeout )
{
   return pSock->inbuffer > 0 ? 1 :
          zh_sockexCanRead( ZH_BFSOCK_GET( pSock )->sock, fBuffer, timeout );
}

static int s_sockexCanWrite( PZH_SOCKEX pSock, ZH_BOOL fBuffer, ZH_MAXINT timeout )
{
   return zh_sockexCanWrite( ZH_BFSOCK_GET( pSock )->sock, fBuffer, timeout );
}

static char * s_sockexName( PZH_SOCKEX pSock )
{
   char * pszName = zh_sockexIsRaw( ZH_BFSOCK_GET( pSock )->sock ) ? NULL :
                    zh_sockexName( ZH_BFSOCK_GET( pSock )->sock );
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
   return zh_sockexErrorStr( ZH_BFSOCK_GET( pSock )->sock, iError );
}

static int s_sockexClose( PZH_SOCKEX pSock, ZH_BOOL fClose )
{
   PZH_SOCKEX_BF pBF = ZH_BFSOCK_GET( pSock );
   int iResult = 0;

   if( pBF )
   {
      if( pBF->sock )
         s_sockexFlush( pSock, ZH_MAX( 15000, pSock->iAutoFlush ), ZH_TRUE );

      if( pBF->sock )
      {
         if( pSock->fShutDown )
            pBF->sock->fShutDown = ZH_TRUE;
         if( pSock->iAutoFlush != 0 && pBF->sock->iAutoFlush == 0 )
            pBF->sock->iAutoFlush = pSock->iAutoFlush;
         iResult = zh_sockexClose( pBF->sock, fClose );
      }
      memset( pBF, 0, sizeof( *pBF ) );
      zh_xfree( pBF );
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
   "BFSOCK",
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
      const void * keydata = NULL, * iv = NULL;
      int keylen = 0, ivlen = 0;

      zh_socekxParamsGetStd( pParams, &keydata, &keylen, &iv, &ivlen, NULL, NULL );
      if( keylen > 0 )
      {
         PZH_SOCKEX_BF pBF = ( PZH_SOCKEX_BF ) zh_xgrabz( sizeof( ZH_SOCKEX_BF ) );
         const ZH_BYTE * pVect = ( const ZH_BYTE * ) ( ivlen > 0 ? iv : NULL );
         int i;

         zh_blowfishInit( &pBF->bf, keydata, keylen );
         for( i = 0; i < ZH_BF_CIPHERBLOCK; ++i )
         {
            if( pVect && ivlen > 0 )
               pBF->encounter[ i ] =
               pBF->decounter[ i ] = pVect[ i % ivlen ];
            else
               pBF->encounter[ i ] =
               pBF->decounter[ i ] = ( ZH_BYTE ) i;
         }

         pSockNew = ( PZH_SOCKEX ) zh_xgrabz( sizeof( ZH_SOCKEX ) );
         pSockNew->sd = pSock->sd;
         pSockNew->fRedirAll = ZH_TRUE;
         pSockNew->fShutDown = pSock->fShutDown;
         pSockNew->iAutoFlush = pSock->iAutoFlush;
         pSockNew->pFilter = &s_sockFilter;
         pSockNew->cargo = ( void * ) pBF;
         pBF->sock = pSock;
         zh_socekxParamsInit( pSockNew, pParams );
      }
   }

   return pSockNew;
}

/* zh_socketNewBFSock( <pSocket>, [<hParams>] ) --> <pSocket> */
ZH_FUNC( ZH_SOCKETNEWBFSOCK )
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

ZH_CALL_ON_STARTUP_BEGIN( _zh_bfsock_init_ )
   zh_sockexRegister( &s_sockFilter );
ZH_CALL_ON_STARTUP_END( _zh_bfsock_init_ )

#if defined( ZH_PRAGMA_STARTUP )
   #pragma startup _zh_bfsock_init_
#elif defined( ZH_DATASEG_STARTUP )
   #define ZH_DATASEG_BODY  ZH_DATASEG_FUNC( _zh_bfsock_init_ )
   #include "zh_ini_seg.h"
#endif
