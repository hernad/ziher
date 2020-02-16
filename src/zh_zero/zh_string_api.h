/*
 * String API functions
 *
 * Copyright 2009 Przemyslaw Czerpak
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

#ifndef ZH_APISTR_H_
#define ZH_APISTR_H_

#include "zh_api.h"
#include "zh_codepage_api.h"

ZH_EXTERN_BEGIN

extern ZH_EXPORT ZH_SIZE zh_wstrlen( const ZH_WCHAR * szText );
extern ZH_EXPORT ZH_SIZE zh_wstrnlen( const ZH_WCHAR * szText, ZH_SIZE nCount );
extern ZH_EXPORT int zh_wstrcmp( const ZH_WCHAR * s1, const ZH_WCHAR * s2 );
extern ZH_EXPORT int zh_wstrncmp( const ZH_WCHAR * s1, const ZH_WCHAR * s2, ZH_SIZE nCount );
extern ZH_EXPORT ZH_WCHAR * zh_wstrncpy( ZH_WCHAR * pDest, const ZH_WCHAR * pSource, ZH_SIZE nLen );
extern ZH_EXPORT ZH_WCHAR * zh_wstrncat( ZH_WCHAR * pDest, const ZH_WCHAR * pSource, ZH_SIZE nLen );
extern ZH_EXPORT ZH_WCHAR * zh_wstrdup( const ZH_WCHAR * szText );
extern ZH_EXPORT ZH_WCHAR * zh_wstrndup( const ZH_WCHAR * szText, ZH_SIZE nLen );

extern ZH_EXPORT char * zh_strunshare( void ** phStr, const char * pStr, ZH_SIZE nLen );
extern ZH_EXPORT ZH_WCHAR * zh_wstrunshare( void ** phStr, const ZH_WCHAR * pStr, ZH_SIZE nLen );
extern ZH_EXPORT const char * zh_strnull( const char * str );
extern ZH_EXPORT const ZH_WCHAR * zh_wstrnull( const ZH_WCHAR * str );


extern ZH_EXPORT void zh_strfree( void * hString );

extern ZH_EXPORT const char * zh_itemGetStr( PZH_ITEM pItem, void * cdp, void ** phString, ZH_SIZE * pnLen );
extern ZH_EXPORT const char * zh_itemGetStrUTF8( PZH_ITEM pItem, void ** phString, ZH_SIZE * pnLen );
extern ZH_EXPORT const ZH_WCHAR * zh_itemGetStrU16( PZH_ITEM pItem, int iEndian, void ** phString, ZH_SIZE * pnLen );

extern ZH_EXPORT ZH_SIZE zh_itemCopyStr( PZH_ITEM pItem, void * cdp, char * pStrBuffer, ZH_SIZE nSize );
extern ZH_EXPORT ZH_SIZE zh_itemCopyStrUTF8( PZH_ITEM pItem, char * pStrBuffer, ZH_SIZE nSize );
extern ZH_EXPORT ZH_SIZE zh_itemCopyStrU16( PZH_ITEM pItem, int iEndian, ZH_WCHAR * pStrBuffer, ZH_SIZE nSize );

extern ZH_EXPORT PZH_ITEM zh_itemPutStrLen( PZH_ITEM pItem, void * cdp, const char * pStr, ZH_SIZE nLen );
extern ZH_EXPORT PZH_ITEM zh_itemPutStrLenUTF8( PZH_ITEM pItem, const char * pStr, ZH_SIZE nLen );
extern ZH_EXPORT PZH_ITEM zh_itemPutStrLenU16( PZH_ITEM pItem, int iEndian, const ZH_WCHAR * pStr, ZH_SIZE nLen );

extern ZH_EXPORT PZH_ITEM zh_itemPutStr( PZH_ITEM pItem, void * cdp, const char * pStr );
extern ZH_EXPORT PZH_ITEM zh_itemPutStrUTF8( PZH_ITEM pItem, const char * pStr );
extern ZH_EXPORT PZH_ITEM zh_itemPutStrU16( PZH_ITEM pItem, int iEndian, const ZH_WCHAR * pStr );


extern ZH_EXPORT const char * zh_arrayGetStr( PZH_ITEM pArray, ZH_SIZE nIndex, void * cdp, void ** phString, ZH_SIZE * pnLen );
extern ZH_EXPORT const char * zh_arrayGetStrUTF8( PZH_ITEM pArray, ZH_SIZE nIndex, void ** phString, ZH_SIZE * pnLen );
extern ZH_EXPORT const ZH_WCHAR * zh_arrayGetStrU16( PZH_ITEM pArray, ZH_SIZE nIndex, int iEndian, void ** phString, ZH_SIZE * pnLen );

extern ZH_EXPORT ZH_BOOL zh_arraySetStrLen( PZH_ITEM pArray, ZH_SIZE nIndex, void * cdp, const char * pStr, ZH_SIZE nLen );
extern ZH_EXPORT ZH_BOOL zh_arraySetStrLenUTF8( PZH_ITEM pArray, ZH_SIZE nIndex, const char * pStr, ZH_SIZE nLen );
extern ZH_EXPORT ZH_BOOL zh_arraySetStrLenU16( PZH_ITEM pArray, ZH_SIZE nIndex, int iEndian, const ZH_WCHAR * pStr, ZH_SIZE nLen );

extern ZH_EXPORT ZH_BOOL zh_arraySetStr( PZH_ITEM pArray, ZH_SIZE nIndex, void * cdp, const char * pStr );
extern ZH_EXPORT ZH_BOOL zh_arraySetStrUTF8( PZH_ITEM pArray, ZH_SIZE nIndex, const char * pStr );
extern ZH_EXPORT ZH_BOOL zh_arraySetStrU16( PZH_ITEM pArray, ZH_SIZE nIndex, int iEndian, const ZH_WCHAR * pStr );


extern ZH_EXPORT const char * zh_parstr( int iParam, void * cdp, void ** phString, ZH_SIZE * pnLen );
extern ZH_EXPORT const char * zh_parstr_utf8( int iParam, void ** phString, ZH_SIZE * pnLen );
extern ZH_EXPORT const ZH_WCHAR * zh_parstr_u16( int iParam, int iEndian, void ** phString, ZH_SIZE * pnLen );

extern ZH_EXPORT const char * zh_parastr( int iParam, ZH_SIZE nIndex, void * cdp, void ** phString, ZH_SIZE * pnLen );
extern ZH_EXPORT const char * zh_parastr_utf8( int iParam, ZH_SIZE nIndex, void ** phString, ZH_SIZE * pnLen );
extern ZH_EXPORT const ZH_WCHAR * zh_parastr_u16( int iParam, ZH_SIZE nIndex, int iEndian, void ** phString, ZH_SIZE * pnLen );

extern ZH_EXPORT void zh_retstr( void * cdp, const char * szText );
extern ZH_EXPORT void zh_retstr_utf8( const char * szText );
extern ZH_EXPORT void zh_retstr_u16( int iEndian, const ZH_WCHAR * szText );

extern ZH_EXPORT void zh_retstrlen( void * cdp, const char * szText, ZH_SIZE nLen );
extern ZH_EXPORT void zh_retstrlen_utf8( const char * szText, ZH_SIZE nLen );
extern ZH_EXPORT void zh_retstrlen_u16( int iEndian, const ZH_WCHAR * szText, ZH_SIZE nLen );

extern ZH_EXPORT int zh_storstr( void * cdp, const char * szText, int iParam );
extern ZH_EXPORT int zh_storstr_utf8( const char * szText, int iParam );
extern ZH_EXPORT int zh_storstr_u16( int iEndian, const ZH_WCHAR * szText, int iParam );

extern ZH_EXPORT int zh_storstrlen( void * cdp, const char * szText, ZH_SIZE nLen, int iParam );
extern ZH_EXPORT int zh_storstrlen_utf8( const char * szText, ZH_SIZE nLen, int iParam );
extern ZH_EXPORT int zh_storstrlen_u16( int iEndian, const ZH_WCHAR * szText, ZH_SIZE nLen, int iParam );

ZH_EXTERN_END

#endif /* ZH_APISTR_H_ */
