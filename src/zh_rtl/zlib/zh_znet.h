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

#ifndef ZH_ZNET_H_
#define ZH_ZNET_H_

#include "zh_api.h"
#include "../zh_socket.h"

ZH_EXTERN_BEGIN

#define ZH_INET_ERR_OK            0
#define ZH_INET_ERR_TIMEOUT       ( -1 )
#define ZH_INET_ERR_CLOSEDCONN    ( -2 )
#define ZH_INET_ERR_BUFFOVERRUN   ( -3 )
#define ZH_INET_ERR_CLOSEDSOCKET  ( -4 )

#define ZH_ZNET_SOCK_ERROR_BASE   100

#if defined( _ZH_ZNET_INTERNAL_ )
   struct _ZH_ZNETSTREAM;
   typedef struct _ZH_ZNETSTREAM * PZH_ZNETSTREAM;
#else
   typedef void * PZH_ZNETSTREAM;
#endif

typedef long ( * ZH_INET_RDFUNC ) ( PZH_ZNETSTREAM, ZH_SOCKET, void *, long, ZH_MAXINT );
typedef long ( * ZH_INET_WRFUNC ) ( PZH_ZNETSTREAM, ZH_SOCKET, const void *, long, ZH_MAXINT, long * );
typedef long ( * ZH_INET_FLFUNC ) ( PZH_ZNETSTREAM, ZH_SOCKET, ZH_MAXINT, ZH_BOOL );
typedef void ( * ZH_INET_CLFUNC ) ( PZH_ZNETSTREAM );
typedef int  ( * ZH_INET_ERFUNC ) ( PZH_ZNETSTREAM );
typedef const char * ( * ZH_INET_ESFUNC ) ( PZH_ZNETSTREAM, int );

extern ZH_EXPORT PZH_ZNETSTREAM zh_znetOpen( int level, int strategy );
extern ZH_EXPORT void    zh_znetEncryptKey( PZH_ZNETSTREAM pStream, const void * keydata, int keylen );
extern ZH_EXPORT void    zh_znetClose( PZH_ZNETSTREAM pStream );
extern ZH_EXPORT int     zh_znetError( PZH_ZNETSTREAM pStream );
extern ZH_EXPORT long    zh_znetRead( PZH_ZNETSTREAM pStream, ZH_SOCKET sd, void * buffer, long len, ZH_MAXINT timeout );
extern ZH_EXPORT long    zh_znetFlush( PZH_ZNETSTREAM pStream, ZH_SOCKET sd, ZH_MAXINT timeout, ZH_BOOL fSync );
extern ZH_EXPORT long    zh_znetWrite( PZH_ZNETSTREAM pStream, ZH_SOCKET sd, const void * buffer, long len, ZH_MAXINT timeout, long * plast );

extern ZH_EXPORT ZH_SOCKET zh_znetInetFD( PZH_ITEM pItem, ZH_BOOL fError );
extern ZH_EXPORT ZH_MAXINT zh_znetInetTimeout( PZH_ITEM pItem, ZH_BOOL fError );
extern ZH_EXPORT ZH_BOOL   zh_znetInetInitialize( PZH_ITEM, PZH_ZNETSTREAM,
                                                  ZH_INET_RDFUNC,
                                                  ZH_INET_WRFUNC,
                                                  ZH_INET_FLFUNC,
                                                  ZH_INET_CLFUNC,
                                                  ZH_INET_ERFUNC,
                                                  ZH_INET_ESFUNC );

extern ZH_EXPORT PZH_SOCKEX zh_sockexNewZNet( ZH_SOCKET sd, const void * keydata, int keylen,
                                              int level, int strategy );

ZH_EXTERN_END

#endif /* ZH_ZNET_H_ */
