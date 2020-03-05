/*
 * Code used to register new CP definition
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

#include "zh_codepage_api.h"

ZH_CODEPAGE_ANNOUNCE( ZH_CP_ID )

#if defined( ZH_PRAGMA_STARTUP )
ZH_CALL_ON_STARTUP_BEGIN( _zh_codepage_Init_ )
#else
ZH_CALL_ON_STARTUP_BEGIN( ZH_MACRONAME_JOIN( _zh_codepage_Init_, ZH_CP_ID ) )
#endif

#if defined( ZH_CP_RAW )
   #if defined( ZH_CP_CUSTOM )
      #define ZH_CP_TP_CUSTOM    ZH_CODEPAGE_TYPE_CUSTOM
   #else
      #if defined( ZH_CP_GET_FUNC ) && \
          defined( ZH_CP_PUT_FUNC ) && \
          defined( ZH_CP_LEN_FUNC )
         #define ZH_CP_TP_CUSTOM    ZH_CODEPAGE_TYPE_CUSTOM
      #else
         #define ZH_CP_TP_CUSTOM    0
         #define ZH_CP_GET_FUNC     NULL
         #define ZH_CP_PUT_FUNC     NULL
         #define ZH_CP_LEN_FUNC     NULL
      #endif
      #if ! defined( ZH_CP_UPPER_FUNC ) && \
          ! defined( ZH_CP_LOWER_FUNC ) && \
          ! defined( ZH_CP_FLAG_FUNC )
         #define ZH_CP_UPPER_FUNC   NULL
         #define ZH_CP_LOWER_FUNC   NULL
         #define ZH_CP_FLAGS_FUNC   NULL
      #endif
      #ifndef ZH_CP_CMP_FUNC
         #define ZH_CP_CMP_FUNC     NULL
      #endif
      #ifndef ZH_CP_CMPI_FUNC
         #define ZH_CP_CMPI_FUNC    NULL
      #endif
   #endif

   #if defined( ZH_CP_CHARIDX )
      #define ZH_CP_TP_CHARIDX   ZH_CODEPAGE_TYPE_CHARIDX
   #else
      #define ZH_CP_TP_CHARIDX   0
   #endif

   #if defined( ZH_CP_CHARUNI )
      #define ZH_CP_TP_CHARUNI   ZH_CODEPAGE_TYPE_CHARUNI
   #else
      #define ZH_CP_TP_CHARUNI   0
   #endif

   #if defined( ZH_CP_UTF8 )
      #define ZH_CP_TP_UTF8      ZH_CODEPAGE_TYPE_UTF8
   #else
      #define ZH_CP_TP_UTF8      0
   #endif

   static ZH_CODEPAGE s_codePage =
   {
      ZH_MACRO2STRING( ZH_CP_ID ),
      ZH_CP_INFO,
      ZH_CP_UNITB,
      s_flags,
      s_upper,
      s_lower,
      s_sort,
      NULL,
      ZH_CODEPAGE_ACSORT_NONE,
      ( ZH_CP_TP_CUSTOM | ZH_CP_TP_CHARIDX | ZH_CP_TP_CHARUNI | ZH_CP_TP_UTF8 ),
      ZH_CP_GET_FUNC,
      ZH_CP_PUT_FUNC,
      ZH_CP_LEN_FUNC,
      ZH_CP_UPPER_FUNC,
      ZH_CP_LOWER_FUNC,
      ZH_CP_FLAGS_FUNC,
      ZH_CP_CMP_FUNC,
      ZH_CP_CMPI_FUNC,
      0,
      0,
      NULL,
      NULL,
      NULL,
   };
   #if defined( ZH_CP_INIT )
      ZH_CP_INIT( &s_codePage );
   #endif
   zh_cdpRegisterRaw( &s_codePage );
#else
   #ifndef ZH_CP_CSSORT
      #define ZH_CP_CSSORT    ZH_CODEPAGE_CSSORT_UPLO
   #endif
   #ifdef ZH_CP_UTF8
      #define ZH_CP_UTF8_STR  ZH_TRUE
   #else
      #define ZH_CP_UTF8_STR  ZH_FALSE
   #endif
   zh_cdpRegisterNew( ZH_MACRO2STRING( ZH_CP_ID ), ZH_CP_INFO, ZH_CP_UNITB,
                      ZH_CP_UPPER, ZH_CP_LOWER, ZH_CP_ACSORT, ZH_CP_CSSORT,
                      ZH_CP_UTF8_STR );
#endif /* ZH_CP_RAW */

#if defined( ZH_PRAGMA_STARTUP )
ZH_CALL_ON_STARTUP_END( _zh_codepage_Init_ )
#else
ZH_CALL_ON_STARTUP_END( ZH_MACRONAME_JOIN( _zh_codepage_Init_, ZH_CP_ID ) )
#endif

// help
// #pragma startup [priority]
// #pragma exit [priority]
// pragma startup always execute the function before the main function 
// pragma exit always execute the function after the main function.

#if defined( ZH_PRAGMA_STARTUP )
   #pragma startup _zh_codepage_Init_
#elif defined( ZH_DATASEG_STARTUP )
   #define ZH_DATASEG_BODY    \
         ZH_DATASEG_FUNC( ZH_MACRONAME_JOIN( _zh_codepage_Init_, ZH_CP_ID ) )
   #include "../zh_ini_seg.h"
#endif
