/*
 * Windows UNICODE helper macros
 *
 * Copyright 2009 Viktor Szakats (vszakats.net/harbour)
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

#ifndef ZH_WINUNI_H_
#define ZH_WINUNI_H_

#include "zh_string_api.h"

#if defined( ZH_OS_WIN )

#include "zh_set.h"

#if defined( UNICODE )
   #define ZH_PARSTR( n, h, len )                zh_parstr_u16( n, ZH_CDP_ENDIAN_NATIVE, h, len )
   #define ZH_PARSTRDEF( n, h, len )             zh_wstrnull( zh_parstr_u16( n, ZH_CDP_ENDIAN_NATIVE, h, len ) )
   #define ZH_PARASTR( n, i, h, len )            zh_parastr_u16( n, i, ZH_CDP_ENDIAN_NATIVE, h, len )
   #define ZH_PARASTRDEF( n, i, h, len )         zh_wstrnull( zh_parastr_u16( n, i, ZH_CDP_ENDIAN_NATIVE, h, len ) )
   #define ZH_RETSTR( str )                      zh_retstr_u16( ZH_CDP_ENDIAN_NATIVE, str )
   #define ZH_RETSTRLEN( str, len )              zh_retstrlen_u16( ZH_CDP_ENDIAN_NATIVE, str, len )
   #define ZH_STORSTR( str, n )                  zh_storstr_u16( ZH_CDP_ENDIAN_NATIVE, str, n )
   #define ZH_STORSTRLEN( str, len, n )          zh_storstrlen_u16( ZH_CDP_ENDIAN_NATIVE, str, len, n )
   #define ZH_ARRAYGETSTR( arr, n, phstr, plen ) zh_arrayGetStrU16( arr, n, ZH_CDP_ENDIAN_NATIVE, phstr, plen )
   #define ZH_ARRAYSETSTR( arr, n, str )         zh_arraySetStrU16( arr, n, ZH_CDP_ENDIAN_NATIVE, str )
   #define ZH_ARRAYSETSTRLEN( arr, n, str, len ) zh_arraySetStrLenU16( arr, n, ZH_CDP_ENDIAN_NATIVE, str, len )
   #define ZH_ITEMCOPYSTR( itm, str, len )       zh_itemCopyStrU16( itm, ZH_CDP_ENDIAN_NATIVE, str, len )
   #define ZH_ITEMGETSTR( itm, phstr, plen )     zh_itemGetStrU16( itm, ZH_CDP_ENDIAN_NATIVE, phstr, plen )
   #define ZH_ITEMPUTSTR( itm, str )             zh_itemPutStrU16( itm, ZH_CDP_ENDIAN_NATIVE, str )
   #define ZH_ITEMPUTSTRLEN( itm, str, len )     zh_itemPutStrLenU16( itm, ZH_CDP_ENDIAN_NATIVE, str, len )
   #define ZH_STRUNSHARE( h, str, len )          zh_wstrunshare( h, str, len )
   #define ZH_STRLEN( str )                      zh_wstrlen( str )
   #define ZH_STRNLEN( str, len )                zh_wstrnlen( str, len )
   #define ZH_STRDUP( str )                      zh_wstrdup( str )
   #define ZH_STRNDUP( str, len )                zh_wstrndup( str, len )
   #define ZH_STRNCPY( dst, src, len )           zh_wstrncpy( dst, src, len )
   #define ZH_STRNCAT( dst, src, len )           zh_wstrncat( dst, src, len )
   #define ZH_STRCMP( s1, s2 )                   zh_wstrcmp( s1, s2 )
   #define ZH_STRNCMP( s1, s2, len )             zh_wstrncmp( s1, s2, len )
   #define ZH_FSNAMECONV( fname, pfree )         ( ( LPCTSTR ) ( *( pfree ) = zh_fsNameConvU16( fname ) ) )
   #define ZH_CHARDUP( str )                     zh_osStrU16Encode( str )
   #define ZH_CHARDUPN( str, len )               zh_osStrU16EncodeN( str, len )
   #define ZH_OSSTRDUP( str )                    zh_osStrU16Decode( str )
   #define ZH_OSSTRDUP2( str, buf, len )         zh_osStrU16Decode2( str, buf, len )
   #define ZH_WINAPI_SYSTEM( cmd )               _wsystem( cmd )
   #define ZH_WINAPI_KERNEL32_DLL()              ( zh_iswin9x() ? TEXT( "unicows.dll" ) : TEXT( "kernel32.dll" ) )

   #define ZH_WINAPI_GETPROCADDRESS( h, n )   GetProcAddress( h, n )
   #define ZH_WINAPI_GETPROCADDRESST( h, n )  GetProcAddress( h, n "W" )
#else
   #define ZH_PARSTR( n, h, len )                zh_parstr( n, zh_setGetOSCP(), h, len )
   #define ZH_PARSTRDEF( n, h, len )             zh_strnull( zh_parstr( n, zh_setGetOSCP(), h, len ) )
   #define ZH_PARASTR( n, i, h, len )            zh_parastr( n, i, zh_setGetOSCP(), h, len )
   #define ZH_PARASTRDEF( n, i, h, len )         zh_strnull( zh_parastr( n, i, zh_setGetOSCP(), h, len ) )
   #define ZH_RETSTR( str )                      zh_retstr( zh_setGetOSCP(), str )
   #define ZH_RETSTRLEN( str, len )              zh_retstrlen( zh_setGetOSCP(), str, len )
   #define ZH_STORSTR( str, n )                  zh_storstr( zh_setGetOSCP(), str, n )
   #define ZH_STORSTRLEN( str, len, n )          zh_storstrlen( zh_setGetOSCP(), str, len, n )
   #define ZH_ARRAYGETSTR( arr, n, phstr, plen ) zh_arrayGetStr( arr, n, zh_setGetOSCP(), phstr, plen )
   #define ZH_ARRAYSETSTR( arr, n, str )         zh_arraySetStr( arr, n, zh_setGetOSCP(), str )
   #define ZH_ARRAYSETSTRLEN( arr, n, str, len ) zh_arraySetStrLen( arr, n, zh_setGetOSCP(), str, len )
   #define ZH_ITEMCOPYSTR( itm, str, len )       zh_itemCopyStr( itm, zh_setGetOSCP(), str, len )
   #define ZH_ITEMGETSTR( itm, phstr, plen )     zh_itemGetStr( itm, zh_setGetOSCP(), phstr, plen )
   #define ZH_ITEMPUTSTR( itm, str )             zh_itemPutStr( itm, zh_setGetOSCP(), str )
   #define ZH_ITEMPUTSTRLEN( itm, str, len )     zh_itemPutStrLen( itm, zh_setGetOSCP(), str, len )
   #define ZH_STRUNSHARE( h, str, len )          zh_strunshare( h, str, len )
   #define ZH_STRLEN( str )                      strlen( str )
   #define ZH_STRNLEN( str, len )                zh_strnlen( str, len )
   #define ZH_STRDUP( str )                      zh_strdup( str )
   #define ZH_STRNDUP( str, len )                zh_strndup( str, len )
   #define ZH_STRNCPY( dst, src, len )           zh_strncpy( dst, src, len )
   #define ZH_STRNCAT( dst, src, len )           zh_strncat( dst, src, len )
   #define ZH_STRCMP( s1, s2 )                   strcmp( s1, s2 )
   #define ZH_STRNCMP( s1, s2, len )             strncmp( s1, s2, len )
   #define ZH_FSNAMECONV( fname, pfree )         zh_fsNameConv( fname, pfree )
   #define ZH_CHARDUP( str )                     zh_osStrEncode( str )
   #define ZH_CHARDUPN( str, len )               zh_osStrEncodeN( str, len )
   #define ZH_OSSTRDUP( str )                    zh_osStrDecode( str )
   #define ZH_OSSTRDUP2( str, buf, len )         zh_osStrDecode2( str, buf, len )
   #define ZH_WINAPI_SYSTEM( cmd )               system( cmd )
   #define ZH_WINAPI_KERNEL32_DLL()              ( TEXT( "kernel32.dll" ) )
   #define ZH_WINAPI_GETPROCADDRESS( h, n )      GetProcAddress( h, n )
   #define ZH_WINAPI_GETPROCADDRESST( h, n )     GetProcAddress( h, n "A" )
#endif

#endif /* ZH_OS_WIN */

#endif /* ZH_WINUNI_H_ */
