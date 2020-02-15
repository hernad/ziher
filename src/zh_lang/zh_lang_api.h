/*
 * Header file for the Language API
 *
 * Copyright 1999-2001 Viktor Szakats
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

#ifndef ZH_APILNG_H_
#define ZH_APILNG_H_

#include "zh_defs.h"
#include "zh_vmpub.h"
#include "zh_init.h"

#include "lang.zhh" /* Base values for the unified language item table */

ZH_EXTERN_BEGIN

/* This hack is needed to force preprocessing if id is also a macro */
#define ZH_LANG_REQUEST( id )          ZH_LANG_REQUEST_( id )
#define ZH_LANG_REQUEST_( id )         ZH_FUNC_EXTERN( ZH_LANG_##id ); \
                                       extern void zh_lang_ForceLink_##id( void ); \
                                       void zh_lang_ForceLink_##id( void ) \
                                       { \
                                          ZH_FUNC_EXEC( ZH_LANG_##id ); \
                                       }

/* Macro to publish a specific language module, for both C and Ziher level */
#define ZH_LANG_ANNOUNCE( id )          ZH_LANG_ANNOUNCE_( id )
#define ZH_LANG_ANNOUNCE_( id )         ZH_FUNC( ZH_LANG_##id ) {}

typedef const struct _ZH_LANG
{
   const char * pItemList[ ZH_LANG_ITEM_MAX_ ];
} ZH_LANG, * PZH_LANG;

extern ZH_EXPORT PZH_LANG  zh_vmLang( void );
extern ZH_EXPORT void      zh_vmSetLang( PZH_LANG pLang );

/* Supported language list management */

extern ZH_EXPORT void      zh_langReleaseAll    ( void );
extern ZH_EXPORT ZH_BOOL   zh_langRegister      ( PZH_LANG lang );
extern ZH_EXPORT PZH_LANG  zh_langFind          ( const char * pszID );

/* Default language selection and data query */

extern ZH_EXPORT PZH_LANG     zh_langSelect     ( PZH_LANG lang );
extern ZH_EXPORT const char * zh_langSelectID   ( const char * pszID );
extern ZH_EXPORT const char * zh_langGetItem    ( const char * pszID, int iIndex );
extern ZH_EXPORT const char * zh_langID         ( void );
extern ZH_EXPORT char *       zh_langName       ( const char * pszID );

/* Compatibility interfaces */

extern ZH_EXPORT const char * zh_langDGetItem         ( int iIndex );
extern ZH_EXPORT const char * zh_langDGetErrorDesc    ( int iIndex );

ZH_EXTERN_END

#endif /* ZH_APILNG_H_ */
