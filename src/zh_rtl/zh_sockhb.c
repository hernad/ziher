/*
 * Socket API wrapper functions
 *
 * Copyright 2010 Mindaugas Kavaliauskas <dbtopas / at / dbtopas.lt>
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

/*
 * zh_socketGetError() --> nSocketError
 * zh_socketGetOSError() --> nOSError
 * zh_socketErrorString( [ nSocketErrror = zh_socketGetError() ], [ hSocket ] ) --> cError
 * zh_socketGetSockName( hSocket ) --> aAddr | NIL
 * zh_socketGetPeerName( hSocket ) --> aAddr | NIL
 * zh_socketOpen( [ nDomain = ZH_SOCKET_AF_INET ], [ nType = ZH_SOCKET_PT_STREAM ], [ nProtocol = 0 ] ) --> hSocket
 * zh_socketClose( hSocket ) --> lSuccess
 * zh_socketShutdown( hSocket, [ nMode = ZH_SOCKET_SHUT_RDWR ] ) --> lSuccess
 * zh_socketBind( hSocket, aAddr ) --> lSuccess
 * zh_socketListen( hSocket, [ iQueueLen = 10 ] ) --> lSuccess
 * zh_socketAccept( hSocket, [ @aAddr ], [ nTimeout = FOREVER ] ) --> hConnectionSocket
 * zh_socketConnect( hSocket, aAddr, [ nTimeout = FOREVER ] ) --> lSuccess
 * zh_socketSend( hSocket, cBuffer, [ nLen = Len( cBuffer ) ], [ nFlags = 0 ], [ nTimeout = FOREVER ] ) --> nBytesSent
 * zh_socketSendTo( hSocket, cBuffer, [ nLen = Len( cBuffer ) ], [ nFlags = 0 ], aAddr, [ nTimeout = FOREVER ] ) --> nBytesSent
 * zh_socketRecv( hSocket, @cBuffer, [ nLen = Len( cBuffer ) ], [ nFlags = 0 ], [ nTimeout = FOREVER ] ) --> nBytesRecv
 * zh_socketRecvFrom( hSocket, @cBuffer, [ nLen = Len( cBuffer ) ], [ nFlags = 0 ], @aAddr, [ nTimeout = FOREVER ] ) --> nBytesRecv
 * zh_socketSetBlockingIO( hSocket, lValue ) --> nSuccess
 * zh_socketSetNoDelay( hSocket, lValue ) --> lSuccess
 * zh_socketSetExclusiveAddr( hSocket, lValue ) --> lSuccess
 * zh_socketSetReuseAddr( hSocket, lValue ) --> lSuccess
 * zh_socketSetKeepAlive( hSocket, lValue ) --> lSuccess
 * zh_socketSetBroadcast( hSocket, lValue ) --> lSuccess
 * zh_socketSetSndBufSize( hSocket, nValue ) --> lSuccess
 * zh_socketSetRcvBufSize( hSocket, nValue ) --> lSuccess
 * zh_socketGetSndBufSize( hSocket, @nValue ) --> lSuccess
 * zh_socketGetRcvBufSize( hSocket, @nValue ) --> lSuccess
 * zh_socketSetMulticast( hSocket, [ nFamily = ZH_SOCKET_AF_INET ], cAddr ) --> lSuccess
 * zh_socketSelectRead( hSocket, [ nTimeout = FOREVER ] ) --> nRet
 * zh_socketSelectWrite( hSocket, [ nTimeout = FOREVER ] ) --> nRet
 * zh_socketSelectWriteEx( hSocket, [ nTimeout = FOREVER ] ) --> nRet
 * zh_socketSelect( aRead, lSetRead, aWrite, lSetWrite, aExcep, lSetExcep, [ nTimeout = FOREVER ] ) --> nRet
 * zh_socketResolveINetAddr( cAddr, nPort ) --> aAddr | NIL
 * zh_socketResolveAddr( cAddr, [ nFamily = ZH_SOCKET_AF_INET ] ) --> cResolved
 * zh_socketGetHostName( aAddr ) --> cHostName
 * zh_socketGetHosts( cAddr, [ nFamily = ZH_SOCKET_AF_INET ] ) --> aHosts
 * zh_socketGetIFaces( [ nFamily ], [ lNoAliases ] ) --> aIfaces
 * zh_socketGetFD( hSocket ) --> nFD
 * zh_socketSetFilter( hSocket, cFilterName, [ hSockParams ] ) --> hSocket
 * zh_socketGetFilter( hSocket ) --> cFilterName
 * zh_socketRead( hSocket, @cBuffer, [ nLen = Len( cBuffer ) ], [ nTimeout = FOREVER ] ) --> nBytesRead
 * zh_socketWrite( hSocket, cBuffer, [ nLen = Len( cBuffer ) ], [ nTimeout = FOREVER ] ) --> nBytesWritten
 * zh_socketFlush( hSocket, [ nTimeout = FOREVER ], [ lSync ] ) --> nBytesLeft
 * zh_socketAutoFlush( hSocket, [ nNewSetting ] ) --> nPrevSetting
 * zh_socketAutoShutdown( hSocket, [ lNewSetting ] ) --> lPrevSetting
 */

/* this has to be declared before zh_socket.h is included */
#define _ZH_SOCKEX_IMPLEMENTATION_

#include "zh_api.h"
#include "zh_item_api.h"
#include "zh_api_error.h"
#include "zh_vm.h"
#include "zh_stack.h"
#include "zh_socket.h"

static ZH_BOOL s_fInit = ZH_FALSE;

/* create new extended socket structure */
static PZH_SOCKEX s_sockexNew( ZH_SOCKET sd, PZH_ITEM pParams );

/* destroy extended socket structure */
static int s_sockexClose( PZH_SOCKEX pSock, ZH_BOOL fClose )
{
   int iResult = zh_sockexRawClear( pSock, fClose );

   zh_xfree( pSock );

   return iResult;
}

/* read data from extended socket, check internal read-ahead
   buffer first */
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
      return lRead;
   }
   if( pSock->sd == ZH_NO_SOCKET )
   {
      zh_socketSetError( ZH_SOCKET_ERR_INVALIDHANDLE );
      return -1;
   }
   return zh_socketRecv( pSock->sd, data, len, 0, timeout );
}

/* write data to extended socket */
static long s_sockexWrite( PZH_SOCKEX pSock, const void * data, long len, ZH_MAXINT timeout )
{
   if( pSock->sd == ZH_NO_SOCKET )
   {
      zh_socketSetError( ZH_SOCKET_ERR_INVALIDHANDLE );
      return -1;
   }
   return zh_socketSend( pSock->sd, data, len, 0, timeout );
}

/* flush data written to extended socket which may not be sent so far,
   return number of bytes not flushed (still in sent buffer), parameter
   fSync is set to TRUE if this function should use special flush method
   which allows to synchronize with new filter initialized from scratch,
   i.e. it's necessary to inform peer that we replace ZLIB compressor,
   core code set it to true only just before closing the filter. */
static long s_sockexFlush( PZH_SOCKEX pSock, ZH_MAXINT timeout, ZH_BOOL fSync )
{
   ZH_SYMBOL_UNUSED( pSock );
   ZH_SYMBOL_UNUSED( timeout );
   ZH_SYMBOL_UNUSED( fSync );

   return 0;
}

/* check if data can be read from extended socket,
   return 1, 0 or -1 to indicate error.
   If fBuffer parameter is set to true then only data already read
   from socket and stored in memory buffer should be checked without
   any timeout. Such call is executed just before inside
   zh_sockexSelect() just before call to low-level socket select()
   function. */
static int s_sockexCanRead( PZH_SOCKEX pSock, ZH_BOOL fBuffer, ZH_MAXINT timeout )
{
   if( pSock->inbuffer > 0 )
      return 1;
   else if( pSock->sd == ZH_NO_SOCKET )
   {
      zh_socketSetError( ZH_SOCKET_ERR_INVALIDHANDLE );
      return -1;
   }
   return fBuffer ? 0 : zh_socketSelectRead( pSock->sd, timeout );
}

/* check if data can be written to extended socket without any delay,
   return 1, 0 or -1 to indicate error.
   If fBuffer parameter is set to true then only this function should
   check only free place in internal sent buffer if such buffer exists.
   Real socket device should be checked only when fBuffer is false.
   If extended socket does not use any sent buffers and fBuffer is true
   then this functions should return 0. In most of implementations 0
   can be returned in all cases if fBuffer is true. Such behavior will
   reduce number of data buffered and not flushed. */
static int s_sockexCanWrite( PZH_SOCKEX pSock, ZH_BOOL fBuffer, ZH_MAXINT timeout )
{
   if( pSock->sd == ZH_NO_SOCKET )
   {
      zh_socketSetError( ZH_SOCKET_ERR_INVALIDHANDLE );
      return -1;
   }
   return fBuffer ? 0 : zh_socketSelectWrite( pSock->sd, timeout );
}

/* return socket name, caller is responsible to free the buffer
   it's intentionally non static buffer to allow nested filters
   create complex filter name dynamically, i.e. "zlib|blowfish"
 */
static char * s_sockexName( PZH_SOCKEX pSock )
{
   return zh_strdup( pSock->pFilter->pszName );
}

/* convert error code into short string description */
static const char * s_sockexErrorStr( PZH_SOCKEX pSock, int iError )
{
   ZH_SYMBOL_UNUSED( pSock );

   return zh_socketErrorStr( iError );
}

/* this is basic wrapper which does not support multilevel filtering
   so it destroys previous wrappers if any and restore new raw socket,
   some other wrappers may allow to join filters encapsulating existing
   ones, i.e.: "ZLIB" | "BlowFish" */
static PZH_SOCKEX s_sockexNext( PZH_SOCKEX pSock, PZH_ITEM pParams )
{
   PZH_SOCKEX pSockNew = NULL;

   if( pSock )
   {
      pSockNew = s_sockexNew( pSock->sd, pParams );
      if( pSockNew )
         zh_sockexClose( pSock, ZH_FALSE );
   }

   return pSockNew;
}

static const ZH_SOCKET_FILTER s_sockFilter =
{
   "socket",
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

/* create new extended socket structure */
static PZH_SOCKEX s_sockexNew( ZH_SOCKET sd, PZH_ITEM pParams )
{
   PZH_SOCKEX pSock;

   pSock = ( PZH_SOCKEX ) zh_xgrabz( sizeof( ZH_SOCKEX ) );
   pSock->sd = sd;
   pSock->pFilter = &s_sockFilter;

   zh_socekxParamsInit( pSock, pParams );

   return pSock;
}

static const ZH_SOCKET_FILTER * s_socketFilters[ ZH_SOCKET_FILTER_MAX ];
static int s_iFilterCount = 0;

int zh_sockexRegister( const ZH_SOCKET_FILTER * pFilter )
{
   if( s_iFilterCount == 0 )
      s_socketFilters[ s_iFilterCount++ ] = &s_sockFilter;

   if( pFilter )
   {
      int i;

      for( i = 0; i < s_iFilterCount; ++i )
      {
         if( s_socketFilters[ i ] == pFilter )
            return 1;
         if( zh_stricmp( s_socketFilters[ i ]->pszName, pFilter->pszName ) == 0 )
            return 2;
      }

      if( s_iFilterCount >= ZH_SOCKET_FILTER_MAX )
         return 3;

      s_socketFilters[ s_iFilterCount++ ] = pFilter;
   }

   return 0;
}

/* helper functions */

static ZH_BOOL s_socketaddrParam( int iParam, void ** pAddr, unsigned int * puiLen )
{
   PZH_ITEM pItem = zh_param( iParam, ZH_IT_ARRAY );

   if( pItem && zh_socketAddrFromItem( pAddr, puiLen, pItem ) )
      return ZH_TRUE;

   zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
   return ZH_FALSE;
}

static ZH_SOCKET s_socketSelectCallback( PZH_ITEM pItem )
{
   ZH_SOCKET socket = zh_socketItemGet( pItem );

   if( socket != ZH_NO_SOCKET )
      return socket;

   zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
   return ZH_NO_SOCKET;
}


static void s_socket_exit( void * cargo )
{
   ZH_SYMBOL_UNUSED( cargo );

   if( s_fInit )
   {
      zh_socketCleanup();
      s_iFilterCount = 0;
      s_fInit = ZH_FALSE;
   }
}

static void s_socket_init( void )
{
   if( ! s_fInit )
   {
      zh_sockexRegister( NULL );
      zh_socketInit();
      zh_vmAtQuit( s_socket_exit, NULL );
      s_fInit = ZH_TRUE;
   }
}

void zh_socketAutoInit( void )
{
   s_socket_init();
}


/* Collectable pointer support */

static ZH_GARBAGE_FUNC( zh_socket_destructor )
{
   PZH_SOCKEX * pSockPtr = ( PZH_SOCKEX * ) Cargo;

   if( *pSockPtr )
   {
      zh_sockexClose( *pSockPtr, ZH_TRUE );
      *pSockPtr = NULL;
   }
}

static const ZH_GC_FUNCS s_gcSocketFuncs =
{
   zh_socket_destructor,
   zh_gcDummyMark
};

ZH_SOCKET zh_socketParam( int iParam )
{
   PZH_SOCKEX * pSockPtr = ( PZH_SOCKEX * ) zh_parptrGC( &s_gcSocketFuncs, iParam );

   if( pSockPtr && *pSockPtr )
      return ( *pSockPtr )->sd;

   zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
   return ZH_NO_SOCKET;
}

ZH_SOCKET zh_socketItemGet( PZH_ITEM pItem )
{
   PZH_SOCKEX * pSockPtr = ( PZH_SOCKEX * ) zh_itemGetPtrGC( pItem, &s_gcSocketFuncs );

   return pSockPtr && *pSockPtr ? ( *pSockPtr )->sd : ZH_NO_SOCKET;
}

PZH_ITEM zh_socketItemPut( PZH_ITEM pItem, ZH_SOCKET sd )
{
   PZH_SOCKEX * pSockPtr = ( PZH_SOCKEX * ) zh_gcAllocate( sizeof( PZH_SOCKEX ), &s_gcSocketFuncs );

   *pSockPtr = zh_sockexNew( sd, NULL, NULL );

   return zh_itemPutPtrGC( pItem, pSockPtr );
}

void zh_socketItemClear( PZH_ITEM pItem )
{
   PZH_SOCKEX * pSockPtr = ( PZH_SOCKEX * ) zh_itemGetPtrGC( pItem, &s_gcSocketFuncs );

   if( pSockPtr && *pSockPtr )
   {
      zh_sockexClose( *pSockPtr, ZH_FALSE );
      *pSockPtr = NULL;
   }
}

/* extended socket functions zh_sockex* */

PZH_SOCKEX zh_sockexParam( int iParam )
{
   PZH_SOCKEX * pSockPtr = ( PZH_SOCKEX * ) zh_parptrGC( &s_gcSocketFuncs, iParam );

   if( pSockPtr && *pSockPtr )
      return *pSockPtr;

   zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
   return NULL;
}

PZH_SOCKEX zh_sockexItemGet( PZH_ITEM pItem )
{
   PZH_SOCKEX * pSockPtr = ( PZH_SOCKEX * ) zh_itemGetPtrGC( pItem, &s_gcSocketFuncs );

   return pSockPtr ? *pSockPtr : NULL;
}

PZH_ITEM zh_sockexItemPut( PZH_ITEM pItem, PZH_SOCKEX pSock )
{
   PZH_SOCKEX * pSockPtr = ( PZH_SOCKEX * ) zh_gcAllocate( sizeof( PZH_SOCKEX ), &s_gcSocketFuncs );

   *pSockPtr = pSock;

   return zh_itemPutPtrGC( pItem, pSockPtr );
}

void zh_sockexItemClear( PZH_ITEM pItem )
{
   PZH_SOCKEX * pSockPtr = ( PZH_SOCKEX * ) zh_itemGetPtrGC( pItem, &s_gcSocketFuncs );

   if( pSockPtr && *pSockPtr )
      *pSockPtr = NULL;
}

ZH_BOOL zh_sockexItemReplace( PZH_ITEM pItem, PZH_SOCKEX pSock )
{
   PZH_SOCKEX * pSockPtr = ( PZH_SOCKEX * ) zh_itemGetPtrGC( pItem, &s_gcSocketFuncs );

   if( pSockPtr )
   {
      if( *pSockPtr )
         zh_sockexClose( *pSockPtr, ZH_FALSE );
      *pSockPtr = pSock;
      return ZH_TRUE;
   }
   return ZH_FALSE;
}

ZH_BOOL zh_sockexItemSetFilter( PZH_ITEM pItem, const char * pszFilter, PZH_ITEM pParams )
{
   PZH_SOCKEX * pSockPtr = ( PZH_SOCKEX * ) zh_itemGetPtrGC( pItem, &s_gcSocketFuncs );

   if( pSockPtr && *pSockPtr )
   {
      PZH_SOCKEX pSock = *pSockPtr;

      if( pszFilter == NULL ? pSock->pFilter == &s_sockFilter :
          ! zh_stricmp( pSock->pFilter->pszName, pszFilter ) )
         return ZH_TRUE;
      else
      {
         PZH_SOCKEX pSockNew = zh_sockexNext( pSock, pszFilter, pParams );

         if( pSockNew )
         {
            *pSockPtr = pSockNew;
            return ZH_TRUE;
         }
      }
   }
   return ZH_FALSE;
}

static int s_socket_filter_find( const char * pszFilter )
{
   int i;

   for( i = 0; i < s_iFilterCount; ++i )
   {
      if( zh_stricmp( s_socketFilters[ i ]->pszName, pszFilter ) == 0 )
         return i;
   }
   return -1;
}

static const ZH_SOCKET_FILTER ** s_socket_getfilters( const char * pszFilter,
                                                      const ZH_SOCKET_FILTER ** pFilters,
                                                      int * piCount )
{
   int iCount = 0, iMax = *piCount;

   if( pszFilter == NULL )
      pFilters[ iCount++ ] = &s_sockFilter;
   else
   {
      int i = s_socket_filter_find( pszFilter );

      if( i >= 0 )
         pFilters[ iCount++ ] = s_socketFilters[ i ];
      else if( strchr( pszFilter, '|' ) != NULL )
      {
         char * pszFilterList, * ptr;

         pszFilterList = zh_strdup( pszFilter );
         ptr = pszFilterList + strlen( pszFilterList );

         while( ptr-- > pszFilterList )
         {
            i = -2;
            if( *ptr == '|' )
            {
               *ptr = '\0';
               if( ptr[ 1 ] )
                  i = s_socket_filter_find( ptr + 1 );
            }
            else if( ptr == pszFilterList && ptr[ 0 ] )
               i = s_socket_filter_find( ptr );

            if( i >= 0 )
            {
               if( iCount == iMax )
               {
                  if( iMax == *piCount )
                     pFilters = ( const ZH_SOCKET_FILTER ** )
                                zh_xmemdup( pFilters, sizeof( *pFilters ) * iMax );
                  iMax += 16;
                  pFilters = ( const ZH_SOCKET_FILTER ** )
                             zh_xrealloc( ZH_UNCONST( pFilters ), sizeof( *pFilters ) * iMax );
               }
               pFilters[ iCount++ ] = s_socketFilters[ i ];
            }
            else if( i == -1 )
            {
               iCount = 0;
               break;
            }
         }
         zh_xfree( pszFilterList );
      }
   }
   if( iCount == 0 )
   {
      if( iMax > *piCount )
         zh_xfree( ZH_UNCONST( pFilters ) );
      pFilters = NULL;
   }
   *piCount = iCount;

   return pFilters;
}

PZH_SOCKEX zh_sockexNew( ZH_SOCKET sd, const char * pszFilter, PZH_ITEM pParams )
{
   const ZH_SOCKET_FILTER * pBuffer[ 16 ];
   const ZH_SOCKET_FILTER ** pFilters;
   int iCount = ZH_SIZEOFARRAY( pBuffer );
   PZH_SOCKEX pSock = NULL;

   pFilters = s_socket_getfilters( pszFilter, pBuffer, &iCount );
   if( pFilters )
   {
      int i;

      for( i = 0; i < iCount; ++i )
      {
         PZH_SOCKEX pSockNew = pSock == NULL ?
                               pFilters[ i ]->New( sd, pParams ) :
                               pFilters[ i ]->Next( pSock, pParams );
         if( pSockNew == NULL )
         {
            if( pSock )
            {
               zh_sockexClose( pSock, ZH_FALSE );
               pSock = NULL;
            }
            break;
         }
         pSock = pSockNew;
      }
      if( pFilters != pBuffer )
         zh_xfree( ZH_UNCONST( pFilters ) );
   }
   return pSock;
}

PZH_SOCKEX zh_sockexNext( PZH_SOCKEX pSock, const char * pszFilter, PZH_ITEM pParams )
{
   const ZH_SOCKET_FILTER * pBuffer[ 16 ];
   const ZH_SOCKET_FILTER ** pFilters;
   int iCount = ZH_SIZEOFARRAY( pBuffer );

   pFilters = s_socket_getfilters( pszFilter, pBuffer, &iCount );
   if( pFilters )
   {
      int i;

      for( i = 0; pSock && i < iCount; ++i )
      {
         PZH_SOCKEX pSockNew = pFilters[ i ]->Next( pSock, pParams );
         if( pSockNew != NULL )
            pSock = pSockNew;
         else if( i == 0 )
            pSock = NULL;
      }
      if( pFilters != pBuffer )
         zh_xfree( ZH_UNCONST( pFilters ) );
   }
   return pSock;
}

int zh_sockexClose( PZH_SOCKEX pSock, ZH_BOOL fClose )
{
   return pSock->pFilter->Close( pSock, fClose );
}

long zh_sockexRead( PZH_SOCKEX pSock, void * data, long len, ZH_MAXINT timeout )
{
   return pSock->pFilter->Read( pSock, data, len, timeout );
}

long zh_sockexWrite( PZH_SOCKEX pSock, const void * data, long len, ZH_MAXINT timeout )
{
   len = pSock->pFilter->Write( pSock, data, len, timeout );
   if( len >= 0 && pSock->iAutoFlush > 0 )
   {
      if( timeout >= 0 )
         timeout = ZH_MAX( timeout, pSock->iAutoFlush );
      zh_sockexFlush( pSock, timeout, ZH_FALSE );
   }
   return len;
}

long zh_sockexFlush( PZH_SOCKEX pSock, ZH_MAXINT timeout, ZH_BOOL fSync )
{
   return pSock->pFilter->Flush( pSock, timeout, fSync );
}

int zh_sockexCanRead( PZH_SOCKEX pSock, ZH_BOOL fBuffer, ZH_MAXINT timeout )
{
   return pSock->pFilter->CanRead( pSock, fBuffer, timeout );
}

int zh_sockexCanWrite( PZH_SOCKEX pSock, ZH_BOOL fBuffer, ZH_MAXINT timeout )
{
   return pSock->pFilter->CanWrite( pSock, fBuffer, timeout );
}

char * zh_sockexName( PZH_SOCKEX pSock )
{
   return pSock->pFilter->Name( pSock );
}

const char * zh_sockexErrorStr( PZH_SOCKEX pSock, int iError )
{
   return pSock->pFilter->ErrorStr( pSock, iError );
}

int zh_sockexSelect( PZH_ITEM pArrayRD, ZH_BOOL fSetRD,
                     PZH_ITEM pArrayWR, ZH_BOOL fSetWR,
                     PZH_ITEM pArrayEX, ZH_BOOL fSetEX,
                     ZH_MAXINT timeout, ZH_SOCKET_FUNC pFunc )
{
   int iResult;
   ZH_SIZE nRead = 0, nWrite = 0, nLen, nPos;
   PZH_SOCKEX pSock;

   if( pArrayRD )
   {
      nLen = zh_arrayLen( pArrayRD );
      for( nPos = 1; nPos <= nLen; ++nPos )
      {
         pSock = zh_sockexItemGet( zh_arrayGetItemPtr( pArrayRD, nPos ) );
         if( pSock && pSock->pFilter->CanRead( pSock, ZH_TRUE, 0 ) > 0 )
         {
            ++nRead;
            if( fSetRD && nRead != nPos )
               zh_itemMove( zh_arrayGetItemPtr( pArrayRD, nRead ),
                            zh_arrayGetItemPtr( pArrayRD, nPos ) );
         }
      }
   }
   if( pArrayWR )
   {
      nLen = zh_arrayLen( pArrayWR );
      for( nPos = 1; nPos <= nLen; ++nPos )
      {
         pSock = zh_sockexItemGet( zh_arrayGetItemPtr( pArrayWR, nPos ) );
         if( pSock && pSock->pFilter->CanWrite( pSock, ZH_TRUE, 0 ) > 0 )
         {
            ++nWrite;
            if( fSetWR && nWrite != nPos )
               zh_itemMove( zh_arrayGetItemPtr( pArrayWR, nWrite ),
                            zh_arrayGetItemPtr( pArrayWR, nPos ) );
         }
      }
   }

   if( nRead > 0 || nWrite > 0 )
   {
      if( fSetRD && pArrayRD )
         zh_arraySize( pArrayRD, nRead );
      if( fSetWR && pArrayWR )
         zh_arraySize( pArrayWR, nWrite );
      if( fSetEX && pArrayEX )
         zh_arraySize( pArrayEX, 0 );
      iResult = ( int ) ( nRead + nWrite );
   }
   else
   {
      if( pFunc == NULL )
         pFunc = s_socketSelectCallback;
      iResult = zh_socketSelect( pArrayRD, fSetRD, pArrayWR, fSetWR, pArrayEX, fSetEX,
                                 timeout, pFunc );
   }

   return iResult;
}

int zh_sockexRawClear( PZH_SOCKEX pSock, ZH_BOOL fClose )
{
   int iResult = 0;

   if( fClose && pSock->sd != ZH_NO_SOCKET )
   {
      if( pSock->fShutDown )
         zh_socketShutdown( pSock->sd, ZH_SOCKET_SHUT_RDWR );
      iResult = zh_socketClose( pSock->sd );
   }
   if( pSock->buffer )
      zh_xfree( pSock->buffer );

   memset( pSock, 0, sizeof( *pSock ) );
   pSock->sd = ZH_NO_SOCKET;

   return iResult;
}

ZH_BOOL zh_sockexIsRaw( PZH_SOCKEX pSock )
{
   return pSock && pSock->pFilter == &s_sockFilter;
}

ZH_SOCKET zh_sockexGetHandle( PZH_SOCKEX pSock )
{
   return pSock ? pSock->sd : ZH_NO_SOCKET;
}

void zh_sockexClearHandle( PZH_SOCKEX pSock )
{
   if( pSock )
      pSock->sd = ZH_NO_SOCKET;
}

ZH_BOOL zh_sockexGetShutDown( PZH_SOCKEX pSock )
{
   return pSock && pSock->fShutDown;
}

void zh_sockexSetShutDown( PZH_SOCKEX pSock, ZH_BOOL fShutDown )
{
   if( pSock )
      pSock->fShutDown = fShutDown;
}

int zh_sockexGetAutoFlush( PZH_SOCKEX pSock )
{
   return pSock ? pSock->iAutoFlush : 0;
}

void zh_sockexSetAutoFlush( PZH_SOCKEX pSock, int iAutoFlush )
{
   if( pSock )
      pSock->iAutoFlush = iAutoFlush;
}

void zh_socekxParamsGetStd( PZH_ITEM pParams,
                            const void ** pKeydata, int * pKeylen,
                            const void ** pIV, int * pIVlen,
                            int * pLevel, int * pStrategy )
{
   if( pParams && ZH_IS_HASH( pParams ) )
   {
      PZH_ITEM pItem;

      if( pKeydata && pKeylen &&
          ( pItem = zh_hashGetCItemPtr( pParams, "key" ) ) != NULL &&
          ZH_IS_STRING( pItem ) )
      {
         *pKeydata = zh_itemGetCPtr( pItem );
         *pKeylen  = ( int ) zh_itemGetCLen( pItem );
      }
      else if( pKeydata && pKeylen &&
               ( pItem = zh_hashGetCItemPtr( pParams, "pass" ) ) != NULL &&
               ZH_IS_STRING( pItem ) )
      {
         *pKeydata = zh_itemGetCPtr( pItem );
         *pKeylen  = ( int ) zh_itemGetCLen( pItem );
      }
      if( pIV && pIVlen &&
          ( pItem = zh_hashGetCItemPtr( pParams, "iv" ) ) != NULL &&
          ZH_IS_STRING( pItem ) )
      {
         *pIV    = zh_itemGetCPtr( pItem );
         *pIVlen = ( int ) zh_itemGetCLen( pItem );
      }
      if( pLevel &&
          ( pItem = zh_hashGetCItemPtr( pParams, "zlib" ) ) != NULL &&
          ZH_IS_NUMERIC( pItem ) )
         *pLevel = zh_itemGetNI( pItem );
      if( pStrategy &&
          ( pItem = zh_hashGetCItemPtr( pParams, "zs" ) ) != NULL &&
          ZH_IS_NUMERIC( pItem ) )
         *pStrategy = zh_itemGetNI( pItem );
   }
}

void zh_socekxParamsInit( PZH_SOCKEX pSock, PZH_ITEM pParams )
{
   if( pParams && ZH_IS_HASH( pParams ) )
   {
      PZH_ITEM pItem;

      if( ( pItem = zh_hashGetCItemPtr( pParams, "readahead" ) ) != NULL &&
          ZH_IS_NUMERIC( pItem ) )
      {
         if( pSock->buffer == NULL )
            pSock->readahead = zh_itemGetNL( pItem );
      }
      if( ( pItem = zh_hashGetCItemPtr( pParams, "flush" ) ) != NULL &&
          ZH_IS_NUMERIC( pItem ) )
         pSock->iAutoFlush = zh_itemGetNI( pItem );
      if( ( pItem = zh_hashGetCItemPtr( pParams, "redir" ) ) != NULL &&
          ZH_IS_LOGICAL( pItem ) )
         pSock->fRedirAll = zh_itemGetL( pItem );
   }
}


/* PRG functions */

ZH_FUNC( ZH_SOCKETGETERROR )
{
   zh_retni( zh_socketGetError() );
}

ZH_FUNC( ZH_SOCKETGETOSERROR )
{
   zh_retni( zh_socketGetOsError() );
}

ZH_FUNC( ZH_SOCKETERRORSTRING )
{
   PZH_SOCKEX pSock;
   int iError = 1;

   if( ZH_ISPOINTER( 1 ) )
      pSock = zh_sockexParam( 1 );
   else if( ZH_ISPOINTER( 2 ) )
      pSock = zh_sockexParam( 2 );
   else
   {
      pSock = NULL;
      iError = 0;
   }

   if( pSock || iError == 0 )
   {
      if( ZH_IS_PARAM_NUM( 1 ) )
         iError = zh_parni( 1 );
      else if( ZH_IS_PARAM_NUM( 2 ) )
         iError = zh_parni( 2 );
      else
         iError = zh_socketGetError();

      zh_retc( pSock ? zh_sockexErrorStr( pSock, iError ) :
                       zh_socketErrorStr( iError ) );
   }
}

ZH_FUNC( ZH_SOCKETGETSOCKNAME )
{
   ZH_SOCKET socket = zh_socketParam( 1 );

   if( socket != ZH_NO_SOCKET )
   {
      void * addr;
      unsigned int len;

      if( zh_socketGetSockName( socket, &addr, &len ) == 0 )
      {
         PZH_ITEM pItem = zh_socketAddrToItem( addr, len );

         if( addr )
            zh_xfree( addr );

         if( pItem )
         {
            zh_itemReturnRelease( pItem );
            return;
         }
      }
      zh_ret();
   }
}

ZH_FUNC( ZH_SOCKETGETPEERNAME )
{
   ZH_SOCKET socket = zh_socketParam( 1 );

   if( socket != ZH_NO_SOCKET )
   {
      void * addr;
      unsigned int len;

      if( zh_socketGetPeerName( socket, &addr, &len ) == 0 )
      {
         PZH_ITEM pItem = zh_socketAddrToItem( addr, len );

         if( addr )
            zh_xfree( addr );

         if( pItem )
         {
            zh_itemReturnRelease( pItem );
            return;
         }
      }
      zh_ret();
   }
}

ZH_FUNC( ZH_SOCKETOPEN )
{
   ZH_SOCKET socket;
   int iDomain = zh_parnidef( 1, ZH_SOCKET_AF_INET );
   int iType = zh_parnidef( 2, ZH_SOCKET_PT_STREAM );
   int iProtocol = zh_parni( 3 );

   s_socket_init();
   if( ( socket = zh_socketOpen( iDomain, iType, iProtocol ) ) != ZH_NO_SOCKET )
      zh_socketItemPut( zh_stackReturnItem(), socket );
   else
      zh_retptr( NULL );
}

ZH_FUNC( ZH_SOCKETCLOSE )
{
   PZH_SOCKEX pSock = zh_sockexParam( 1 );

   if( pSock )
   {
      zh_sockexItemClear( zh_param( 1, ZH_IT_POINTER ) );
      zh_retl( zh_sockexClose( pSock, ZH_TRUE ) == 0 );
   }
}

ZH_FUNC( ZH_SOCKETSHUTDOWN )
{
   ZH_SOCKET socket = zh_socketParam( 1 );

   if( socket != ZH_NO_SOCKET )
      zh_retl( zh_socketShutdown( socket, zh_parnidef( 2, ZH_SOCKET_SHUT_RDWR ) ) == 0 );
}

ZH_FUNC( ZH_SOCKETBIND )
{
   ZH_SOCKET socket = zh_socketParam( 1 );
   void * addr;
   unsigned int len;

   if( socket != ZH_NO_SOCKET && s_socketaddrParam( 2, &addr, &len ) )
   {
      zh_retl( zh_socketBind( socket, addr, len ) == 0 );
      zh_xfree( addr );
   }
}

ZH_FUNC( ZH_SOCKETLISTEN )
{
   ZH_SOCKET socket = zh_socketParam( 1 );

   if( socket != ZH_NO_SOCKET )
      zh_retl( zh_socketListen( socket, zh_parnidef( 2, 10 ) ) == 0 );
}

ZH_FUNC( ZH_SOCKETACCEPT )
{
   ZH_SOCKET socket = zh_socketParam( 1 );

   if( socket != ZH_NO_SOCKET )
   {
      ZH_SOCKET socketaccept;
      void * addr = NULL;
      unsigned int len;

      socketaccept = zh_socketAccept( socket, &addr, &len, zh_parnintdef( 3, -1 ) );

      if( socketaccept != ZH_NO_SOCKET )
         zh_sockexSetShutDown( zh_sockexItemGet( zh_socketItemPut( zh_stackReturnItem(),
                                                            socketaccept ) ), ZH_TRUE );
      else
         zh_retptr( NULL );


      if( ZH_ISBYREF( 2 ) )
      {
         PZH_ITEM pItem;
         if( socketaccept != ZH_NO_SOCKET && ( pItem = zh_socketAddrToItem( addr, len ) ) != NULL )
         {
            zh_itemParamStoreForward( 2, pItem );
            zh_itemRelease( pItem );
         }
         else
            zh_stor( 2 );
      }

      if( addr )
         zh_xfree( addr );
   }
}

ZH_FUNC( ZH_SOCKETCONNECT )
{
   ZH_SOCKET socket = zh_socketParam( 1 );
   void * addr;
   unsigned int len;

   if( socket != ZH_NO_SOCKET && s_socketaddrParam( 2, &addr, &len ) )
   {
      ZH_BOOL fResult = zh_socketConnect( socket, addr, len, zh_parnintdef( 3, -1 ) ) == 0;

      if( fResult )
         zh_sockexSetShutDown( zh_sockexItemGet( zh_param( 1, ZH_IT_POINTER ) ), ZH_TRUE );
      zh_retl( fResult );
      zh_xfree( addr );
   }
}

ZH_FUNC( ZH_SOCKETSEND )
{
   PZH_SOCKEX pSock = zh_sockexParam( 1 );

   if( pSock )
   {
      long lLen = ( long ) zh_parclen( 2 );
      const char * data = zh_parc( 2 );
      ZH_MAXINT timeout = zh_parnintdef( 5, -1 );

      if( ZH_IS_PARAM_NUM( 3 ) )
      {
         long lParam = zh_parnl( 3 );

         if( lParam >= 0 && lParam < lLen )
            lLen = lParam;
      }
      if( pSock->fRedirAll )
      {
         int iAutoFlush = pSock->iAutoFlush;
         if( iAutoFlush <= 0 )
            pSock->iAutoFlush = 15000;
         lLen = zh_sockexWrite( pSock, zh_parc( 2 ), lLen, timeout );
         pSock->iAutoFlush = iAutoFlush;
      }
      else
         lLen = zh_socketSend( pSock->sd, data, lLen, zh_parni( 4 ), timeout );
      zh_retnl( lLen );
   }
}

ZH_FUNC( ZH_SOCKETSENDTO )
{
   ZH_SOCKET socket = zh_socketParam( 1 );
   void * addr;
   unsigned int len;

   if( socket != ZH_NO_SOCKET && s_socketaddrParam( 5, &addr, &len ) )
   {
      long lLen = ( long ) zh_parclen( 2 );

      if( ZH_IS_PARAM_NUM( 3 ) )
      {
         long lParam = zh_parnl( 3 );

         if( lParam >= 0 && lParam < lLen )
            lLen = lParam;
      }
      zh_retnl( zh_socketSendTo( socket, zh_parc( 2 ), lLen, zh_parni( 4 ),
                                 addr, len, zh_parnintdef( 6, -1 ) ) );
      zh_xfree( addr );
   }
}

ZH_FUNC( ZH_SOCKETRECV )
{
   PZH_SOCKEX pSock = zh_sockexParam( 1 );

   if( pSock )
   {
      PZH_ITEM pItem = zh_param( 2, ZH_IT_STRING );
      char * pBuffer;
      ZH_SIZE nLen;

      if( pItem && ZH_ISBYREF( 2 ) && zh_itemGetWriteCL( pItem, &pBuffer, &nLen ) )
      {
         if( ZH_IS_PARAM_NUM( 3 ) )
         {
            long lRead = zh_parnl( 3 );
            if( lRead >= 0 && lRead < ( long ) nLen )
               nLen = lRead;
         }
         zh_retnl( pSock->fRedirAll ?
                   zh_sockexRead( pSock, pBuffer, ( long ) nLen,
                                  zh_parnintdef( 5, -1 ) ) :
                   zh_socketRecv( pSock->sd, pBuffer, ( long ) nLen,
                                  zh_parni( 4 ), zh_parnintdef( 5, -1 ) ) );
      }
      else
         zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
   }
}

ZH_FUNC( ZH_SOCKETRECVFROM )
{
   ZH_SOCKET socket = zh_socketParam( 1 );

   if( socket != ZH_NO_SOCKET )
   {
      PZH_ITEM pItem = zh_param( 2, ZH_IT_STRING );
      char * pBuffer;
      ZH_SIZE nLen;

      if( pItem && ZH_ISBYREF( 2 ) && zh_itemGetWriteCL( pItem, &pBuffer, &nLen ) )
      {
         void * addr = NULL;
         unsigned int len;
         long lRet;

         if( ZH_IS_PARAM_NUM( 3 ) )
         {
            long lRead = zh_parnl( 3 );
            if( lRead >= 0 && lRead < ( long ) nLen )
               nLen = lRead;
         }
         zh_retnl( lRet = zh_socketRecvFrom( socket, pBuffer, ( long ) nLen,
                                             zh_parni( 4 ), &addr, &len,
                                             zh_parnintdef( 6, -1 ) ) );
         if( ZH_ISBYREF( 5 ) )
         {
            PZH_ITEM pAddr;

            if( lRet != -1 && ( pAddr = zh_socketAddrToItem( addr, len ) ) != NULL )
            {
               zh_itemParamStoreForward( 5, pAddr );
               zh_itemRelease( pAddr );
            }
            else
               zh_stor( 5 );
         }

         if( addr )
            zh_xfree( addr );
         return;
      }
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
   }
}

ZH_FUNC( ZH_SOCKETSETBLOCKINGIO )
{
   ZH_SOCKET socket = zh_socketParam( 1 );

   if( socket != ZH_NO_SOCKET )
      zh_retni( zh_socketSetBlockingIO( socket, zh_parl( 2 ) ) );
}

ZH_FUNC( ZH_SOCKETSETNODELAY )
{
   ZH_SOCKET socket = zh_socketParam( 1 );

   if( socket != ZH_NO_SOCKET )
      zh_retl( zh_socketSetNoDelay( socket, zh_parl( 2 ) ) == 0 );
}

ZH_FUNC( ZH_SOCKETSETEXCLUSIVEADDR )
{
   ZH_SOCKET socket = zh_socketParam( 1 );

   if( socket != ZH_NO_SOCKET )
      zh_retl( zh_socketSetExclusiveAddr( socket, zh_parl( 2 ) ) == 0 );
}

ZH_FUNC( ZH_SOCKETSETREUSEADDR )
{
   ZH_SOCKET socket = zh_socketParam( 1 );

   if( socket != ZH_NO_SOCKET )
      zh_retl( zh_socketSetReuseAddr( socket, zh_parl( 2 ) ) == 0 );
}

ZH_FUNC( ZH_SOCKETSETKEEPALIVE )
{
   ZH_SOCKET socket = zh_socketParam( 1 );

   if( socket != ZH_NO_SOCKET )
      zh_retl( zh_socketSetKeepAlive( socket, zh_parl( 2 ) ) == 0 );
}

ZH_FUNC( ZH_SOCKETSETBROADCAST )
{
   ZH_SOCKET socket = zh_socketParam( 1 );

   if( socket != ZH_NO_SOCKET )
      zh_retl( zh_socketSetBroadcast( socket, zh_parl( 2 ) ) == 0 );
}

ZH_FUNC( ZH_SOCKETSETSNDBUFSIZE )
{
   ZH_SOCKET socket = zh_socketParam( 1 );

   if( socket != ZH_NO_SOCKET )
      zh_retl( zh_socketSetSndBufSize( socket, zh_parni( 2 ) ) == 0 );
}

ZH_FUNC( ZH_SOCKETSETRCVBUFSIZE )
{
   ZH_SOCKET socket = zh_socketParam( 1 );

   if( socket != ZH_NO_SOCKET )
      zh_retl( zh_socketSetRcvBufSize( socket, zh_parni( 2 ) ) == 0 );
}

ZH_FUNC( ZH_SOCKETGETSNDBUFSIZE )
{
   ZH_SOCKET socket = zh_socketParam( 1 );

   if( socket != ZH_NO_SOCKET )
   {
      int size;
      zh_retl( zh_socketGetSndBufSize( socket, &size ) == 0 );
      zh_storni( size, 2 );
   }
}

ZH_FUNC( ZH_SOCKETGETRCVBUFSIZE )
{
   ZH_SOCKET socket = zh_socketParam( 1 );

   if( socket != ZH_NO_SOCKET )
   {
      int size;
      zh_retl( zh_socketGetRcvBufSize( socket, &size ) == 0 );
      zh_storni( size, 2 );
   }
}

ZH_FUNC( ZH_SOCKETSETMULTICAST )
{
   ZH_SOCKET socket = zh_socketParam( 1 );

   if( socket != ZH_NO_SOCKET )
      zh_retl( zh_socketSetMulticast( socket, zh_parnidef( 2, ZH_SOCKET_AF_INET ), zh_parc( 3 ) ) == 0 );
}

ZH_FUNC( ZH_SOCKETSELECTREAD )
{
   PZH_SOCKEX pSock = zh_sockexParam( 1 );

   if( pSock )
      zh_retni( zh_sockexCanRead( pSock, ZH_FALSE, zh_parnintdef( 2, -1 ) ) );
}

ZH_FUNC( ZH_SOCKETSELECTWRITE )
{
   PZH_SOCKEX pSock = zh_sockexParam( 1 );

   if( pSock )
      zh_retni( zh_sockexCanWrite( pSock, ZH_FALSE, zh_parnintdef( 2, -1 ) ) );
}

ZH_FUNC( ZH_SOCKETSELECTWRITEEX )
{
   ZH_SOCKET socket = zh_socketParam( 1 );

   if( socket != ZH_NO_SOCKET )
      zh_retni( zh_socketSelectWriteEx( socket, zh_parnintdef( 2, -1 ) ) );
}

ZH_FUNC( ZH_SOCKETSELECT )
{
   s_socket_init();
   zh_retni( zh_sockexSelect( zh_param( 1, ZH_IT_ARRAY ), zh_parl( 2 ),
                              zh_param( 3, ZH_IT_ARRAY ), zh_parl( 4 ),
                              zh_param( 5, ZH_IT_ARRAY ), zh_parl( 6 ),
                              zh_parnintdef( 7, -1 ), s_socketSelectCallback ) );
}

ZH_FUNC( ZH_SOCKETRESOLVEINETADDR )
{
   void * addr;
   unsigned int len;

   s_socket_init();
   if( zh_socketResolveInetAddr( &addr, &len, zh_parc( 1 ), zh_parni( 2 ) ) )
   {
      PZH_ITEM pItem = zh_socketAddrToItem( addr, len );

      if( addr )
         zh_xfree( addr );

      if( pItem )
      {
         zh_itemReturnRelease( pItem );
         return;
      }
   }
   zh_ret();
}

ZH_FUNC( ZH_SOCKETRESOLVEADDR )
{
   char * szAddr;

   s_socket_init();
   szAddr = zh_socketResolveAddr( zh_parc( 1 ), zh_parnidef( 2, ZH_SOCKET_AF_INET ) );
   if( szAddr )
      zh_retc_buffer( szAddr );
   else
      zh_retc_null();
}

ZH_FUNC( ZH_SOCKETGETHOSTNAME )
{
   void * addr;
   unsigned int len;

   if( s_socketaddrParam( 1, &addr, &len ) )
   {
      char * szHostName = zh_socketGetHostName( addr, len );

      if( addr )
         zh_xfree( addr );
      if( szHostName )
         zh_retc_buffer( szHostName );
      else
         zh_retc_null();
   }
}

ZH_FUNC( ZH_SOCKETGETHOSTS )
{
   const char * szAddr = zh_parc( 1 );

   if( szAddr )
   {
      PZH_ITEM pItem;

      s_socket_init();
      pItem = zh_socketGetHosts( szAddr, zh_parnidef( 2, ZH_SOCKET_AF_INET ) );
      if( pItem )
         zh_itemReturnRelease( pItem );
      else
         zh_reta( 0 );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

#if 0
/* This function is not implemented at C level, yet [Mindaugas] */
ZH_FUNC( ZH_SOCKETGETALIASES )
{
   const char * szAddr = zh_parc( 1 );

   if( szAddr )
   {
      PZH_ITEM pItem;

      s_socket_init();
      pItem = zh_socketGetAliases( szAddr, zh_parnidef( 2, ZH_SOCKET_AF_INET ) );
      if( pItem )
         zh_itemReturnRelease( pItem );
      else
         zh_reta( 0 );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}
#endif

ZH_FUNC( ZH_SOCKETGETIFACES )
{
   PZH_ITEM pItem;

   s_socket_init();
   pItem = zh_socketGetIFaces( zh_parni( 1 ), zh_parl( 2 ) );
   if( pItem )
      zh_itemReturnRelease( pItem );
   else
      zh_reta( 0 );
}

ZH_FUNC( ZH_SOCKETGETFD )
{
   zh_retnint( zh_socketParam( 1 ) );
}

ZH_FUNC( ZH_SOCKETSETFILTER )
{
   PZH_ITEM pItem = zh_param( 1, ZH_IT_POINTER );

   if( zh_sockexItemSetFilter( pItem, zh_parc( 2 ), zh_param( 3, ZH_IT_ANY ) ) )
      zh_itemReturn( pItem );
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_SOCKETGETFILTER )
{
   PZH_SOCKEX pSock = zh_sockexParam( 1 );

   if( pSock )
      zh_retc_buffer( zh_sockexName( pSock ) );
}

ZH_FUNC( ZH_SOCKETREAD )
{
   PZH_SOCKEX pSock = zh_sockexParam( 1 );

   if( pSock )
   {
      PZH_ITEM pItem = zh_param( 2, ZH_IT_STRING );
      char * pBuffer;
      ZH_SIZE nLen;

      if( pItem && ZH_ISBYREF( 2 ) && zh_itemGetWriteCL( pItem, &pBuffer, &nLen ) )
      {
         if( ZH_IS_PARAM_NUM( 3 ) )
         {
            long lRead = zh_parnl( 3 );
            if( lRead >= 0 && lRead < ( long ) nLen )
               nLen = lRead;
         }
         zh_retnl( zh_sockexRead( pSock, pBuffer, ( long ) nLen,
                                  zh_parnintdef( 4, -1 ) ) );
      }
      else
         zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
   }
}

ZH_FUNC( ZH_SOCKETWRITE )
{
   PZH_SOCKEX pSock = zh_sockexParam( 1 );

   if( pSock )
   {
      ZH_MAXINT timeout = zh_parnintdef( 4, -1 );
      long lLen = ( long ) zh_parclen( 2 );

      if( ZH_IS_PARAM_NUM( 3 ) )
      {
         long lWrite = zh_parnl( 3 );

         if( lWrite >= 0 && lWrite < lLen )
            lLen = lWrite;
      }
      zh_retnl( zh_sockexWrite( pSock, zh_parc( 2 ), lLen, timeout ) );
   }
}

ZH_FUNC( ZH_SOCKETFLUSH )
{
   PZH_SOCKEX pSock = zh_sockexParam( 1 );

   if( pSock )
      zh_retnl( zh_sockexFlush( pSock, zh_parnintdef( 2, -1 ), zh_parl( 3 ) ) );
}

ZH_FUNC( ZH_SOCKETAUTOFLUSH )
{
   PZH_SOCKEX pSock = zh_sockexParam( 1 );

   if( pSock )
   {
      zh_retni( zh_sockexGetAutoFlush( pSock ) );
      if( ZH_IS_PARAM_NUM( 2 ) )
         zh_sockexSetAutoFlush( pSock, zh_parni( 2 ) );
   }
}

ZH_FUNC( ZH_SOCKETAUTOSHUTDOWN )
{
   PZH_SOCKEX pSock = zh_sockexParam( 1 );

   if( pSock )
   {
      zh_retl( zh_sockexGetShutDown( pSock ) );
      if( ZH_ISLOG( 2 ) )
         zh_sockexSetShutDown( pSock, zh_parl( 2 ) );
   }
}
