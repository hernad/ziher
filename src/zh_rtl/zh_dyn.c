/*
 * Dynamic call support
 *
 * Copyright 2009-2010 Viktor Szakats
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

#include "zh_api.h"
#include "zh_api_error.h"
#include "zh_item_api.h"
#include "zh_string_api.h"
#include "zh_set.h"
#include "zh_stack.h"

#include "dyn.zhh"

#define _MASK_CTYPE         0x000FFFF
#define _MASK_ENCODING      0x00F0000
#define _MASK_CALLCONV      0x0F00000
#define _MASK_OPTIONS       0xF000000

/* C raw return types */
#define _RETTYPERAW_INT32   1
#define _RETTYPERAW_INT64   2
#define _RETTYPERAW_DOUBLE  3
#define _RETTYPERAW_FLOAT   4

#define _DYNEXEC_MAXPARAM   15

typedef void ( *PZH_DYNADDR )( void );

static int zh_zhToCtype( int iZiherType )
{
   switch( iZiherType )
   {
      case ZH_IT_NIL:
         return ZH_DYN_CTYPE_VOID;

      case ZH_IT_LOGICAL:
         return ZH_DYN_CTYPE_BOOL;

      case ZH_IT_INTEGER:
      case ZH_IT_LONG:
      case ZH_IT_DATE:
         return ZH_DYN_CTYPE_LONG;

      case ZH_IT_DOUBLE:
         return ZH_DYN_CTYPE_DOUBLE;

      case ZH_IT_STRING:
      case ZH_IT_MEMO:
         return ZH_DYN_CTYPE_CHAR_PTR;

      case ZH_IT_POINTER:
         return ZH_DYN_CTYPE_VOID_PTR;
   }

   return ZH_DYN_CTYPE_DEFAULT;
}

#if defined( ZH_ARCH_64BIT )

typedef struct
{
   union
   {
      ZH_U32 n32;
      ZH_U64 n64;
      double nDB;
      float  nFL;
   } t;
} ZH_DYNVAL;

typedef struct
{
   void *    hString;
   int       iType;
   int       iEncoding;
   int       iOptions;
   ZH_BOOL   bRawBuffer;
   ZH_BOOL   bByRef;
   ZH_DYNVAL value;
} ZH_DYNARG, * PZH_DYNARG;

static ZH_U64 zh_u64par( PZH_ITEM pParam, PZH_DYNARG pArg )
{
   ZH_U64 r;

   switch( pArg->iType )
   {
      case ZH_DYN_CTYPE_BOOL:
         pArg->value.t.n64 = zh_itemGetL( pParam );
         r = pArg->bByRef ? ( ZH_PTRUINT ) &pArg->value.t.n64 : pArg->value.t.n64;
         break;

      case ZH_DYN_CTYPE_CHAR:
         pArg->value.t.n64 = ( char ) zh_itemGetNI( pParam );
         r = pArg->bByRef ? ( ZH_PTRUINT ) &pArg->value.t.n64 : pArg->value.t.n64;
         break;

      case ZH_DYN_CTYPE_CHAR_UNSIGNED:
         pArg->value.t.n64 = ( unsigned char ) zh_itemGetNI( pParam );
         r = pArg->bByRef ? ( ZH_PTRUINT ) &pArg->value.t.n64 : pArg->value.t.n64;
         break;

      case ZH_DYN_CTYPE_SHORT:
         pArg->value.t.n64 = ( short ) zh_itemGetNI( pParam );
         r = pArg->bByRef ? ( ZH_PTRUINT ) &pArg->value.t.n64 : pArg->value.t.n64;
         break;

      case ZH_DYN_CTYPE_SHORT_UNSIGNED:
         pArg->value.t.n64 = ( unsigned short ) zh_itemGetNI( pParam );
         r = pArg->bByRef ? ( ZH_PTRUINT ) &pArg->value.t.n64 : pArg->value.t.n64;
         break;

      case ZH_DYN_CTYPE_INT:
         pArg->value.t.n64 = zh_itemGetNI( pParam );
         r = pArg->bByRef ? ( ZH_PTRUINT ) &pArg->value.t.n64 : pArg->value.t.n64;
         break;

      case ZH_DYN_CTYPE_INT_UNSIGNED:
         pArg->value.t.n64 = ( unsigned int ) zh_itemGetNInt( pParam );
         r = pArg->bByRef ? ( ZH_PTRUINT ) &pArg->value.t.n64 : pArg->value.t.n64;
         break;

      case ZH_DYN_CTYPE_LONG:
         pArg->value.t.n64 = zh_itemGetNL( pParam );
         r = pArg->bByRef ? ( ZH_PTRUINT ) &pArg->value.t.n64 : pArg->value.t.n64;
         break;

      case ZH_DYN_CTYPE_LONG_UNSIGNED:
         pArg->value.t.n64 = ( unsigned long ) zh_itemGetNInt( pParam );
         r = pArg->bByRef ? ( ZH_PTRUINT ) &pArg->value.t.n64 : pArg->value.t.n64;
         break;

      case ZH_DYN_CTYPE_LLONG:
         pArg->value.t.n64 = zh_itemGetNInt( pParam );
         r = pArg->bByRef ? ( ZH_PTRUINT ) &pArg->value.t.n64 : pArg->value.t.n64;
         break;

      case ZH_DYN_CTYPE_LLONG_UNSIGNED:
         /* FIXME: Digits are lost. */
#if ZH_VMLONG_MAX == INT32_MAX || defined( ZH_LONG_LONG_OFF )
         pArg->value.t.n64 = ( ZH_MAXUINT ) zh_itemGetNInt( pParam );
#else
         pArg->value.t.n64 = ( ZH_ULONGLONG ) zh_itemGetNInt( pParam );
#endif
         r = pArg->bByRef ? ( ZH_PTRUINT ) &pArg->value.t.n64 : pArg->value.t.n64;
         break;

      case ZH_DYN_CTYPE_FLOAT:
         /* FIXME */

      case ZH_DYN_CTYPE_DOUBLE:
         ZH_PUT_LE_DOUBLE( ( ZH_BYTE * ) &pArg->value.t.n64, zh_itemGetND( pParam ) );
         r = pArg->bByRef ? ( ZH_PTRUINT ) &pArg->value.t.n64 : pArg->value.t.n64;
         break;

      case ZH_DYN_CTYPE_CHAR_UNSIGNED_PTR:
      case ZH_DYN_CTYPE_STRUCTURE:
      {
         ZH_SIZE nLen = zh_itemGetCLen( pParam );
         pArg->hString = zh_xgrab( nLen + sizeof( char ) );
         pArg->bRawBuffer = ZH_TRUE;
         memcpy( ( char * ) pArg->hString, zh_itemGetCPtr( pParam ), nLen );
         ( ( char * ) pArg->hString )[ nLen ] = '\0';
         r = ( ZH_PTRUINT ) pArg->hString;
         pArg->value.t.n64 = r;
         break;
      }
      case ZH_DYN_CTYPE_CHAR_PTR:

         switch( pArg->iEncoding )
         {
            case ZH_DYN_ENC_ASCII:
            {
               ZH_SIZE nLen;
               const char * s = zh_itemGetStr( pParam, zh_setGetOSCP(), &pArg->hString, &nLen );
               r = ( ZH_PTRUINT ) zh_strunshare( &pArg->hString, s, nLen );
               break;
            }
            case ZH_DYN_ENC_UTF8:
            {
               ZH_SIZE nLen;
               const char * s = zh_itemGetStrUTF8( pParam, &pArg->hString, &nLen );
               r = ( ZH_PTRUINT ) zh_strunshare( &pArg->hString, s, nLen );
               break;
            }
            case ZH_DYN_ENC_UTF16:
            {
               ZH_SIZE nLen;
               const ZH_WCHAR * s = zh_itemGetStrU16( pParam, ZH_CODEPAGE_ENDIAN_NATIVE, &pArg->hString, &nLen );
               r = ( ZH_PTRUINT ) zh_wstrunshare( &pArg->hString, s, nLen );
               break;
            }
            default:
            {
               ZH_SIZE nLen = zh_itemGetCLen( pParam );
               pArg->hString = zh_xgrab( nLen + sizeof( char ) );
               pArg->bRawBuffer = ZH_TRUE;
               memcpy( ( char * ) pArg->hString, zh_itemGetCPtr( pParam ), nLen );
               ( ( char * ) pArg->hString )[ nLen ] = '\0';
               r = ( ZH_PTRUINT ) pArg->hString;
               break;
            }
         }
         pArg->value.t.n64 = r;
         break;

      case ZH_DYN_CTYPE_VOID_PTR:
      case ZH_DYN_CTYPE_BOOL_PTR:
      case ZH_DYN_CTYPE_SHORT_PTR:
      case ZH_DYN_CTYPE_SHORT_UNSIGNED_PTR:
      case ZH_DYN_CTYPE_INT_PTR:
      case ZH_DYN_CTYPE_INT_UNSIGNED_PTR:
      case ZH_DYN_CTYPE_LONG_PTR:
      case ZH_DYN_CTYPE_LONG_UNSIGNED_PTR:
      case ZH_DYN_CTYPE_LLONG_PTR:
      case ZH_DYN_CTYPE_LLONG_UNSIGNED_PTR:
      case ZH_DYN_CTYPE_FLOAT_PTR:
      case ZH_DYN_CTYPE_DOUBLE_PTR:
      case ZH_DYN_CTYPE_STRUCTURE_PTR:
         pArg->value.t.n64 = ( ZH_PTRUINT ) zh_itemGetPtr( pParam );
         r = pArg->bByRef ? ( ZH_PTRUINT ) &pArg->value.t.n64 : pArg->value.t.n64;
         break;

      case ZH_DYN_CTYPE_VOID:
      default:
         r = pArg->value.t.n64 = 0;
   }

   return r;
}

static PZH_ITEM zh_u64ret( PZH_ITEM pItem, int iRetType, int iEncoding, ZH_DYNVAL value, ZH_ISIZ nLen )
{
   switch( iRetType )
   {
      case ZH_DYN_CTYPE_VOID:
         zh_itemClear( pItem );
         break;

      case ZH_DYN_CTYPE_BOOL:
         zh_itemPutL( pItem, value.t.n64 != 0 );
         break;

      case ZH_DYN_CTYPE_CHAR:
         zh_itemPutNI( pItem, ( char ) value.t.n64 );
         break;

      case ZH_DYN_CTYPE_CHAR_UNSIGNED:
         zh_itemPutNI( pItem, ( unsigned char ) value.t.n64 );
         break;

      case ZH_DYN_CTYPE_SHORT:
         zh_itemPutNI( pItem, ( short ) value.t.n64 );
         break;

      case ZH_DYN_CTYPE_SHORT_UNSIGNED:
         zh_itemPutNI( pItem, ( unsigned short ) value.t.n64 );
         break;

      case ZH_DYN_CTYPE_INT:
         zh_itemPutNI( pItem, ( int ) value.t.n64 );
         break;

      case ZH_DYN_CTYPE_INT_UNSIGNED:
         zh_itemPutNInt( pItem, ( unsigned int ) value.t.n64 );
         break;

      case ZH_DYN_CTYPE_LONG:
         zh_itemPutNL( pItem, ( long ) value.t.n64 );
         break;

      case ZH_DYN_CTYPE_LONG_UNSIGNED:
         zh_itemPutNInt( pItem, ( unsigned long ) value.t.n64 );
         break;

      case ZH_DYN_CTYPE_LLONG:
#if ZH_VMLONG_MAX == INT32_MAX || defined( ZH_LONG_LONG_OFF )
         zh_itemPutNInt( pItem, ( ZH_MAXINT ) value.t.n64 );
#else
         zh_itemPutNInt( pItem, ( ZH_LONGLONG ) value.t.n64 );
#endif
         break;

      case ZH_DYN_CTYPE_LLONG_UNSIGNED:
#if ZH_VMLONG_MAX == INT32_MAX || defined( ZH_LONG_LONG_OFF )
         zh_itemPutNInt( pItem, ( ZH_MAXUINT ) value.t.n64 );
#else
         zh_itemPutNInt( pItem, ( ZH_ULONGLONG ) value.t.n64 );
#endif
         break;

      case ZH_DYN_CTYPE_CHAR_UNSIGNED_PTR:
         if( nLen == -1 )
            zh_itemPutC( pItem, ( const char * ) value.t.n64 );
         else
            zh_itemPutCL( pItem, ( const char * ) value.t.n64, nLen );
         break;

      case ZH_DYN_CTYPE_CHAR_PTR:
         switch( iEncoding )
         {
            case ZH_DYN_ENC_ASCII:
               if( nLen == -1 )
                  zh_itemPutStr( pItem, zh_setGetOSCP(), ( const char * ) value.t.n64 );
               else
                  zh_itemPutStrLen( pItem, zh_setGetOSCP(), ( const char * ) value.t.n64, nLen );
               break;
            case ZH_DYN_ENC_UTF8:
               if( nLen == -1 )
                  zh_itemPutStrUTF8( pItem, ( const char * ) value.t.n64 );
               else
                  zh_itemPutStrLenUTF8( pItem, ( const char * ) value.t.n64, nLen );
               break;
            case ZH_DYN_ENC_UTF16:
               if( nLen == -1 )
                  zh_itemPutStrU16( pItem, ZH_CODEPAGE_ENDIAN_NATIVE, ( const ZH_WCHAR * ) value.t.n64 );
               else
                  zh_itemPutStrLenU16( pItem, ZH_CODEPAGE_ENDIAN_NATIVE, ( const ZH_WCHAR * ) value.t.n64, nLen );
               break;
            default:
               if( nLen == -1 )
                  zh_itemPutC( pItem, ( const char * ) value.t.n64 );
               else
                  zh_itemPutCL( pItem, ( const char * ) value.t.n64, nLen );
         }
         break;

      case ZH_DYN_CTYPE_VOID_PTR:
      case ZH_DYN_CTYPE_BOOL_PTR:
      case ZH_DYN_CTYPE_SHORT_PTR:
      case ZH_DYN_CTYPE_SHORT_UNSIGNED_PTR:
      case ZH_DYN_CTYPE_INT_PTR:
      case ZH_DYN_CTYPE_INT_UNSIGNED_PTR:
      case ZH_DYN_CTYPE_LONG_PTR:
      case ZH_DYN_CTYPE_LONG_UNSIGNED_PTR:
      case ZH_DYN_CTYPE_LLONG_PTR:
      case ZH_DYN_CTYPE_LLONG_UNSIGNED_PTR:
      case ZH_DYN_CTYPE_FLOAT_PTR:
      case ZH_DYN_CTYPE_DOUBLE_PTR:
      case ZH_DYN_CTYPE_STRUCTURE_PTR:
         zh_itemPutPtr( pItem, ( void * ) value.t.n64 );
         break;

      case ZH_DYN_CTYPE_FLOAT:
         zh_itemPutND( pItem, ( double ) value.t.nFL );
         break;

      case ZH_DYN_CTYPE_DOUBLE:
         zh_itemPutND( pItem, value.t.nDB );
         break;

      default:
         zh_itemPutNInt( pItem, value.t.n64 );
   }

   return pItem;
}

#define ZH_DYN_CTYPE_DECL( _ret_, _type_ ) \
   typedef _ret_ ( *_type_##P00 )( void ); \
   typedef _ret_ ( *_type_##P01 )( ZH_U64 ); \
   typedef _ret_ ( *_type_##P02 )( ZH_U64, ZH_U64 ); \
   typedef _ret_ ( *_type_##P03 )( ZH_U64, ZH_U64, ZH_U64 ); \
   typedef _ret_ ( *_type_##P04 )( ZH_U64, ZH_U64, ZH_U64, ZH_U64 ); \
   typedef _ret_ ( *_type_##P05 )( ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64 ); \
   typedef _ret_ ( *_type_##P06 )( ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64 ); \
   typedef _ret_ ( *_type_##P07 )( ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64 ); \
   typedef _ret_ ( *_type_##P08 )( ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64 ); \
   typedef _ret_ ( *_type_##P09 )( ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64 ); \
   typedef _ret_ ( *_type_##P10 )( ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64 ); \
   typedef _ret_ ( *_type_##P11 )( ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64 ); \
   typedef _ret_ ( *_type_##P12 )( ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64 ); \
   typedef _ret_ ( *_type_##P13 )( ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64 ); \
   typedef _ret_ ( *_type_##P14 )( ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64 ); \
   typedef _ret_ ( *_type_##P15 )( ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64, ZH_U64 )

#define ZH_DYN_FUN_CALL( pcount, _ret_, _type_ ) \
   do \
   { \
      switch( pcount ) \
      { \
         case  0: ret.t._ret_ = ( ( _type_##P00 ) * pFunction )( ); break; \
         case  1: ret.t._ret_ = ( ( _type_##P01 ) * pFunction )( rawpar[ 0 ] ); break; \
         case  2: ret.t._ret_ = ( ( _type_##P02 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ] ); break; \
         case  3: ret.t._ret_ = ( ( _type_##P03 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ] ); break; \
         case  4: ret.t._ret_ = ( ( _type_##P04 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ] ); break; \
         case  5: ret.t._ret_ = ( ( _type_##P05 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ] ); break; \
         case  6: ret.t._ret_ = ( ( _type_##P06 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ] ); break; \
         case  7: ret.t._ret_ = ( ( _type_##P07 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ] ); break; \
         case  8: ret.t._ret_ = ( ( _type_##P08 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ] ); break; \
         case  9: ret.t._ret_ = ( ( _type_##P09 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ] ); break; \
         case 10: ret.t._ret_ = ( ( _type_##P10 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ] ); break; \
         case 11: ret.t._ret_ = ( ( _type_##P11 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ] ); break; \
         case 12: ret.t._ret_ = ( ( _type_##P12 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ] ); break; \
         case 13: ret.t._ret_ = ( ( _type_##P13 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ] ); break; \
         case 14: ret.t._ret_ = ( ( _type_##P14 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ] ); break; \
         case 15: ret.t._ret_ = ( ( _type_##P15 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ] ); break; \
      } \
   } \
   while( 0 )

ZH_DYN_CTYPE_DECL( ZH_U64, FX64_64 );
ZH_DYN_CTYPE_DECL( double, FX64_DB );
ZH_DYN_CTYPE_DECL( float,  FX64_FL );

#elif defined( ZH_ARCH_32BIT )

typedef struct
{
   union
   {
      ZH_U32 n32;
#if ! defined( ZH_LONG_LONG_OFF )
      ZH_U64 n64;
#endif
      double nDB;
      float  nFL;
   } t;
} ZH_DYNVAL;

typedef struct
{
   void *    hString;
   int       iType;
   int       iEncoding;
   int       iOptions;
   ZH_BOOL   bRawBuffer;
   ZH_BOOL   bByRef;
   ZH_DYNVAL value;
} ZH_DYNARG, * PZH_DYNARG;

static void zh_u32par( PZH_ITEM pParam, PZH_DYNARG pArg, ZH_U32 * r1, ZH_U32 * r2, ZH_BOOL * b64 )
{
   *b64 = ZH_FALSE;
   *r2 = 0;

   switch( pArg->iType )
   {
      case ZH_DYN_CTYPE_BOOL:
         pArg->value.t.n32 = zh_itemGetL( pParam );
         *r1 = pArg->bByRef ? ( ZH_U32 ) &pArg->value.t.n32 : pArg->value.t.n32;
         break;

      case ZH_DYN_CTYPE_CHAR:
         pArg->value.t.n32 = ( char ) zh_itemGetNI( pParam );
         *r1 = pArg->bByRef ? ( ZH_U32 ) &pArg->value.t.n32 : pArg->value.t.n32;
         break;

      case ZH_DYN_CTYPE_CHAR_UNSIGNED:
         pArg->value.t.n32 = ( unsigned char ) zh_itemGetNI( pParam );
         *r1 = pArg->bByRef ? ( ZH_U32 ) &pArg->value.t.n32 : pArg->value.t.n32;
         break;

      case ZH_DYN_CTYPE_SHORT:
         pArg->value.t.n32 = ( short ) zh_itemGetNI( pParam );
         *r1 = pArg->bByRef ? ( ZH_U32 ) &pArg->value.t.n32 : pArg->value.t.n32;
         break;

      case ZH_DYN_CTYPE_SHORT_UNSIGNED:
         pArg->value.t.n32 = ( unsigned short ) zh_itemGetNI( pParam );
         *r1 = pArg->bByRef ? ( ZH_U32 ) &pArg->value.t.n32 : pArg->value.t.n32;
         break;

      case ZH_DYN_CTYPE_INT:
         pArg->value.t.n32 = zh_itemGetNI( pParam );
         *r1 = pArg->bByRef ? ( ZH_U32 ) &pArg->value.t.n32 : pArg->value.t.n32;
         break;

      case ZH_DYN_CTYPE_INT_UNSIGNED:
         pArg->value.t.n32 = ( unsigned int ) zh_itemGetNInt( pParam );
         *r1 = pArg->bByRef ? ( ZH_U32 ) &pArg->value.t.n32 : pArg->value.t.n32;
         break;

      case ZH_DYN_CTYPE_LONG:
         pArg->value.t.n32 = zh_itemGetNL( pParam );
         *r1 = pArg->bByRef ? ( ZH_U32 ) &pArg->value.t.n32 : pArg->value.t.n32;
         break;

      case ZH_DYN_CTYPE_LONG_UNSIGNED:
         pArg->value.t.n32 = ( unsigned long ) zh_itemGetNInt( pParam );
         *r1 = pArg->bByRef ? ( ZH_U32 ) &pArg->value.t.n32 : pArg->value.t.n32;
         break;

      case ZH_DYN_CTYPE_LLONG:
#if ! defined( ZH_LONG_LONG_OFF )
         pArg->value.t.n64 = zh_itemGetNInt( pParam );
         if( pArg->bByRef )
            *r1 = ( ZH_U32 ) &pArg->value.t.n64;
         else
         {
            *r1 = ( ZH_U32 ) ( pArg->value.t.n64 & 0xFFFFFFFF );
            *r2 = ( ZH_U32 ) ( pArg->value.t.n64 >> 32 );
            *b64 = ZH_TRUE;
         }
#endif
         break;

      case ZH_DYN_CTYPE_LLONG_UNSIGNED:
#if ! defined( ZH_LONG_LONG_OFF )
         /* FIXME: Digits are lost. */
#if ZH_VMLONG_MAX == INT32_MAX || defined( ZH_LONG_LONG_OFF )
         pArg->value.t.n64 = ( ZH_MAXUINT ) zh_itemGetNInt( pParam );
#else
         pArg->value.t.n64 = ( ZH_ULONGLONG ) zh_itemGetNInt( pParam );
#endif
         if( pArg->bByRef )
            *r1 = ( ZH_U32 ) &pArg->value.t.n64;
         else
         {
            *r1 = ( ZH_U32 ) ( pArg->value.t.n64 & 0xFFFFFFFF );
            *r2 = ( ZH_U32 ) ( pArg->value.t.n64 >> 32 );
            *b64 = ZH_TRUE;
         }
#endif
         break;

      case ZH_DYN_CTYPE_FLOAT:
         pArg->value.t.nFL = ( float ) zh_itemGetND( pParam );
         *r1 = pArg->bByRef ? ( ZH_U32 ) &pArg->value.t.nFL : pArg->value.t.n32;
         break;

      case ZH_DYN_CTYPE_DOUBLE:
         pArg->value.t.nDB = zh_itemGetND( pParam );
         if( pArg->bByRef )
            *r1 = ( ZH_U32 ) &pArg->value.t.nDB;
         else
         {
#if ! defined( ZH_LONG_LONG_OFF )
            *r1 = ( ZH_U32 ) ( pArg->value.t.n64 & 0xFFFFFFFF );
            *r2 = ( ZH_U32 ) ( pArg->value.t.n64 >> 32 );
            *b64 = ZH_TRUE;
#endif
         }
         break;

      case ZH_DYN_CTYPE_CHAR_UNSIGNED_PTR:
      case ZH_DYN_CTYPE_STRUCTURE:
      {
         ZH_SIZE nLen = zh_itemGetCLen( pParam );
         pArg->hString = zh_xgrab( nLen + sizeof( char ) );
         pArg->bRawBuffer = ZH_TRUE;
         memcpy( ( char * ) pArg->hString, zh_itemGetCPtr( pParam ), nLen );
         ( ( char * ) pArg->hString )[ nLen ] = '\0';
         *r1 = ( ZH_PTRUINT ) pArg->hString;
         pArg->value.t.n32 = *r1;
         break;
      }
      case ZH_DYN_CTYPE_CHAR_PTR:

         switch( pArg->iEncoding )
         {
            case ZH_DYN_ENC_ASCII:
            {
               ZH_SIZE nLen;
               const char * s = zh_itemGetStr( pParam, zh_setGetOSCP(), &pArg->hString, &nLen );
               *r1 = ( ZH_U32 ) zh_strunshare( &pArg->hString, s, nLen );
               break;
            }
            case ZH_DYN_ENC_UTF8:
            {
               ZH_SIZE nLen;
               const char * s = zh_itemGetStrUTF8( pParam, &pArg->hString, &nLen );
               *r1 = ( ZH_U32 ) zh_strunshare( &pArg->hString, s, nLen );
               break;
            }
            case ZH_DYN_ENC_UTF16:
            {
               ZH_SIZE nLen;
               const ZH_WCHAR * s = zh_itemGetStrU16( pParam, ZH_CODEPAGE_ENDIAN_NATIVE, &pArg->hString, &nLen );
               *r1 = ( ZH_U32 ) zh_wstrunshare( &pArg->hString, s, nLen );
               break;
            }
            default:
            {
               ZH_SIZE nLen = zh_itemGetCLen( pParam );
               pArg->hString = zh_xgrab( nLen + sizeof( char ) );
               pArg->bRawBuffer = ZH_TRUE;
               memcpy( ( char * ) pArg->hString, zh_itemGetCPtr( pParam ), nLen );
               ( ( char * ) pArg->hString )[ nLen ] = '\0';
               *r1 = ( ZH_PTRUINT ) pArg->hString;
               break;
            }
         }
         pArg->value.t.n32 = *r1;
         break;

      case ZH_DYN_CTYPE_VOID_PTR:
      case ZH_DYN_CTYPE_BOOL_PTR:
      case ZH_DYN_CTYPE_SHORT_PTR:
      case ZH_DYN_CTYPE_SHORT_UNSIGNED_PTR:
      case ZH_DYN_CTYPE_INT_PTR:
      case ZH_DYN_CTYPE_INT_UNSIGNED_PTR:
      case ZH_DYN_CTYPE_LONG_PTR:
      case ZH_DYN_CTYPE_LONG_UNSIGNED_PTR:
      case ZH_DYN_CTYPE_LLONG_PTR:
      case ZH_DYN_CTYPE_LLONG_UNSIGNED_PTR:
      case ZH_DYN_CTYPE_FLOAT_PTR:
      case ZH_DYN_CTYPE_DOUBLE_PTR:
      case ZH_DYN_CTYPE_STRUCTURE_PTR:
         pArg->value.t.n32 = ( ZH_U32 ) zh_itemGetPtr( pParam );
         *r1 = pArg->bByRef ? ( ZH_U32 ) &pArg->value.t.n32 : pArg->value.t.n32;
         break;

      case ZH_DYN_CTYPE_VOID:
      default:
         *r1 = pArg->value.t.n32 = 0;
   }
}

static PZH_ITEM zh_u32ret( PZH_ITEM pItem, int iRetType, int iEncoding, ZH_DYNVAL value, ZH_ISIZ nLen )
{
   switch( iRetType )
   {
      case ZH_DYN_CTYPE_VOID:
         zh_itemClear( pItem );
         break;

      case ZH_DYN_CTYPE_BOOL:
         zh_itemPutL( pItem, value.t.n32 != 0 );
         break;

      case ZH_DYN_CTYPE_CHAR:
         zh_itemPutNI( pItem, ( char ) value.t.n32 );
         break;

      case ZH_DYN_CTYPE_CHAR_UNSIGNED:
         zh_itemPutNI( pItem, ( unsigned char ) value.t.n32 );
         break;

      case ZH_DYN_CTYPE_SHORT:
         zh_itemPutNI( pItem, ( short ) value.t.n32 );
         break;

      case ZH_DYN_CTYPE_SHORT_UNSIGNED:
         zh_itemPutNI( pItem, ( unsigned short ) value.t.n32 );
         break;

      case ZH_DYN_CTYPE_INT:
         zh_itemPutNI( pItem, ( int ) value.t.n32 );
         break;

      case ZH_DYN_CTYPE_INT_UNSIGNED:
         zh_itemPutNInt( pItem, ( unsigned int ) value.t.n32 );
         break;

      case ZH_DYN_CTYPE_LONG:
         zh_itemPutNL( pItem, ( long ) value.t.n32 );
         break;

      case ZH_DYN_CTYPE_LONG_UNSIGNED:
         zh_itemPutNInt( pItem, value.t.n32 );
         break;

      case ZH_DYN_CTYPE_LLONG:
#if ! defined( ZH_LONG_LONG_OFF )
#if ZH_VMLONG_MAX == INT32_MAX || defined( ZH_LONG_LONG_OFF )
         zh_itemPutNInt( pItem, ( ZH_MAXINT ) value.t.n64 );
#else
         zh_itemPutNInt( pItem, ( ZH_LONGLONG ) value.t.n64 );
#endif
#endif
         break;

      case ZH_DYN_CTYPE_LLONG_UNSIGNED:
#if ! defined( ZH_LONG_LONG_OFF )
#if ZH_VMLONG_MAX == INT32_MAX || defined( ZH_LONG_LONG_OFF )
         zh_itemPutNInt( pItem, ( ZH_MAXUINT ) value.t.n64 );
#else
         zh_itemPutNInt( pItem, ( ZH_ULONGLONG ) value.t.n64 );
#endif
#endif
         break;

      case ZH_DYN_CTYPE_CHAR_UNSIGNED_PTR:
         if( nLen == -1 )
            zh_itemPutC( pItem, ( const char * ) value.t.n32 );
         else
            zh_itemPutCL( pItem, ( const char * ) value.t.n32, nLen );
         break;

      case ZH_DYN_CTYPE_CHAR_PTR:

         switch( iEncoding )
         {
            case ZH_DYN_ENC_ASCII:
               if( nLen == -1 )
                  zh_itemPutStr( pItem, zh_setGetOSCP(), ( const char * ) value.t.n32 );
               else
                  zh_itemPutStrLen( pItem, zh_setGetOSCP(), ( const char * ) value.t.n32, nLen );
               break;
            case ZH_DYN_ENC_UTF8:
               if( nLen == -1 )
                  zh_itemPutStrUTF8( pItem, ( const char * ) value.t.n32 );
               else
                  zh_itemPutStrLenUTF8( pItem, ( const char * ) value.t.n32, nLen );
               break;
            case ZH_DYN_ENC_UTF16:
               if( nLen == -1 )
                  zh_itemPutStrU16( pItem, ZH_CODEPAGE_ENDIAN_NATIVE, ( const ZH_WCHAR * ) value.t.n32 );
               else
                  zh_itemPutStrLenU16( pItem, ZH_CODEPAGE_ENDIAN_NATIVE, ( const ZH_WCHAR * ) value.t.n32, nLen );
               break;
            default:
               if( nLen == -1 )
                  zh_itemPutC( pItem, ( const char * ) value.t.n32 );
               else
                  zh_itemPutCL( pItem, ( const char * ) value.t.n32, nLen );
         }
         break;

      case ZH_DYN_CTYPE_VOID_PTR:
      case ZH_DYN_CTYPE_BOOL_PTR:
      case ZH_DYN_CTYPE_SHORT_PTR:
      case ZH_DYN_CTYPE_SHORT_UNSIGNED_PTR:
      case ZH_DYN_CTYPE_INT_PTR:
      case ZH_DYN_CTYPE_INT_UNSIGNED_PTR:
      case ZH_DYN_CTYPE_LONG_PTR:
      case ZH_DYN_CTYPE_LONG_UNSIGNED_PTR:
      case ZH_DYN_CTYPE_LLONG_PTR:
      case ZH_DYN_CTYPE_LLONG_UNSIGNED_PTR:
      case ZH_DYN_CTYPE_FLOAT_PTR:
      case ZH_DYN_CTYPE_DOUBLE_PTR:
      case ZH_DYN_CTYPE_STRUCTURE_PTR:
         zh_itemPutPtr( pItem, ( void * ) value.t.n32 );
         break;

      case ZH_DYN_CTYPE_FLOAT:
         zh_itemPutND( pItem, ( double ) value.t.nFL );
         break;

      case ZH_DYN_CTYPE_DOUBLE:
         zh_itemPutND( pItem, value.t.nDB );
         break;

      default:
         zh_itemPutNL( pItem, ( long ) value.t.n32 );
   }

   return pItem;
}

#define ZH_DYN_CTYPE_DECL( ret, abi, _type_ ) \
   typedef ret ( abi * _type_##P00 )( void ); \
   typedef ret ( abi * _type_##P01 )( ZH_U32 ); \
   typedef ret ( abi * _type_##P02 )( ZH_U32, ZH_U32 ); \
   typedef ret ( abi * _type_##P03 )( ZH_U32, ZH_U32, ZH_U32 ); \
   typedef ret ( abi * _type_##P04 )( ZH_U32, ZH_U32, ZH_U32, ZH_U32 ); \
   typedef ret ( abi * _type_##P05 )( ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32 ); \
   typedef ret ( abi * _type_##P06 )( ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32 ); \
   typedef ret ( abi * _type_##P07 )( ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32 ); \
   typedef ret ( abi * _type_##P08 )( ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32 ); \
   typedef ret ( abi * _type_##P09 )( ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32 ); \
   typedef ret ( abi * _type_##P10 )( ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32 ); \
   typedef ret ( abi * _type_##P11 )( ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32 ); \
   typedef ret ( abi * _type_##P12 )( ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32 ); \
   typedef ret ( abi * _type_##P13 )( ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32 ); \
   typedef ret ( abi * _type_##P14 )( ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32 ); \
   typedef ret ( abi * _type_##P15 )( ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32 ); \
   typedef ret ( abi * _type_##P16 )( ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32 ); \
   typedef ret ( abi * _type_##P17 )( ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32 ); \
   typedef ret ( abi * _type_##P18 )( ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32 ); \
   typedef ret ( abi * _type_##P19 )( ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32 ); \
   typedef ret ( abi * _type_##P20 )( ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32 ); \
   typedef ret ( abi * _type_##P21 )( ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32 ); \
   typedef ret ( abi * _type_##P22 )( ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32 ); \
   typedef ret ( abi * _type_##P23 )( ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32 ); \
   typedef ret ( abi * _type_##P24 )( ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32 ); \
   typedef ret ( abi * _type_##P25 )( ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32 ); \
   typedef ret ( abi * _type_##P26 )( ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32 ); \
   typedef ret ( abi * _type_##P27 )( ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32 ); \
   typedef ret ( abi * _type_##P28 )( ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32 ); \
   typedef ret ( abi * _type_##P29 )( ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32 ); \
   typedef ret ( abi * _type_##P30 )( ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32, ZH_U32 )

#define ZH_DYN_FUN_CALL( pcount, _ret_, _type_ ) \
   do \
   { \
      switch( pcount ) \
      { \
         case  0: ret.t._ret_ = ( ( _type_##P00 ) * pFunction )( ); break; \
         case  1: ret.t._ret_ = ( ( _type_##P01 ) * pFunction )( rawpar[ 0 ] ); break; \
         case  2: ret.t._ret_ = ( ( _type_##P02 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ] ); break; \
         case  3: ret.t._ret_ = ( ( _type_##P03 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ] ); break; \
         case  4: ret.t._ret_ = ( ( _type_##P04 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ] ); break; \
         case  5: ret.t._ret_ = ( ( _type_##P05 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ] ); break; \
         case  6: ret.t._ret_ = ( ( _type_##P06 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ] ); break; \
         case  7: ret.t._ret_ = ( ( _type_##P07 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ] ); break; \
         case  8: ret.t._ret_ = ( ( _type_##P08 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ] ); break; \
         case  9: ret.t._ret_ = ( ( _type_##P09 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ] ); break; \
         case 10: ret.t._ret_ = ( ( _type_##P10 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ] ); break; \
         case 11: ret.t._ret_ = ( ( _type_##P11 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ] ); break; \
         case 12: ret.t._ret_ = ( ( _type_##P12 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ] ); break; \
         case 13: ret.t._ret_ = ( ( _type_##P13 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ] ); break; \
         case 14: ret.t._ret_ = ( ( _type_##P14 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ] ); break; \
         case 15: ret.t._ret_ = ( ( _type_##P15 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ] ); break; \
         case 16: ret.t._ret_ = ( ( _type_##P16 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ] ); break; \
         case 17: ret.t._ret_ = ( ( _type_##P17 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ] ); break; \
         case 18: ret.t._ret_ = ( ( _type_##P18 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ] ); break; \
         case 19: ret.t._ret_ = ( ( _type_##P19 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ] ); break; \
         case 20: ret.t._ret_ = ( ( _type_##P20 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ] ); break; \
         case 21: ret.t._ret_ = ( ( _type_##P21 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ] ); break; \
         case 22: ret.t._ret_ = ( ( _type_##P22 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ] ); break; \
         case 23: ret.t._ret_ = ( ( _type_##P23 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ] ); break; \
         case 24: ret.t._ret_ = ( ( _type_##P24 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ] ); break; \
         case 25: ret.t._ret_ = ( ( _type_##P25 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ] ); break; \
         case 26: ret.t._ret_ = ( ( _type_##P26 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ] ); break; \
         case 27: ret.t._ret_ = ( ( _type_##P27 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ], rawpar[ 26 ] ); break; \
         case 28: ret.t._ret_ = ( ( _type_##P28 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ], rawpar[ 26 ], rawpar[ 27 ] ); break; \
         case 29: ret.t._ret_ = ( ( _type_##P29 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ], rawpar[ 26 ], rawpar[ 27 ], rawpar[ 28 ] ); break; \
         case 30: ret.t._ret_ = ( ( _type_##P30 ) * pFunction )( rawpar[ 0 ], rawpar[ 1 ], rawpar[ 2 ], rawpar[ 3 ], rawpar[ 4 ], rawpar[ 5 ], rawpar[ 6 ], rawpar[ 7 ], rawpar[ 8 ], rawpar[ 9 ], rawpar[ 10 ], rawpar[ 11 ], rawpar[ 12 ], rawpar[ 13 ], rawpar[ 14 ], rawpar[ 15 ], rawpar[ 16 ], rawpar[ 17 ], rawpar[ 18 ], rawpar[ 19 ], rawpar[ 20 ], rawpar[ 21 ], rawpar[ 22 ], rawpar[ 23 ], rawpar[ 24 ], rawpar[ 25 ], rawpar[ 26 ], rawpar[ 27 ], rawpar[ 28 ], rawpar[ 29 ] ); break; \
      } \
   } while( 0 )


#if defined( ZH_OS_WIN ) || defined( ZH_OS_OS2 )
   #define ZH_CDECL  _cdecl
#else
   #define ZH_CDECL
#endif

ZH_DYN_CTYPE_DECL( ZH_U32, ZH_CDECL, FX86_C32 );
#if ! defined( ZH_LONG_LONG_OFF )
ZH_DYN_CTYPE_DECL( ZH_U64, ZH_CDECL, FX86_C64 );
#endif
ZH_DYN_CTYPE_DECL( double, ZH_CDECL, FX86_CDB );
ZH_DYN_CTYPE_DECL( float,  ZH_CDECL, FX86_CFL );

#if defined( ZH_OS_WIN )

ZH_DYN_CTYPE_DECL( ZH_U32, _stdcall, FX86_S32 );
ZH_DYN_CTYPE_DECL( ZH_U64, _stdcall, FX86_S64 );
ZH_DYN_CTYPE_DECL( double, _stdcall, FX86_SDB );
ZH_DYN_CTYPE_DECL( float,  _stdcall, FX86_SFL );

#endif

#if defined( ZH_OS_OS2 )

ZH_DYN_CTYPE_DECL( ZH_U32, _System, FX86_O32 );
ZH_DYN_CTYPE_DECL( ZH_U64, _System, FX86_O64 );
ZH_DYN_CTYPE_DECL( double, _System, FX86_ODB );
ZH_DYN_CTYPE_DECL( float,  _System, FX86_OFL );

#endif

#endif

void zh_dynCall( int iFuncFlags, void * pFunctionRaw, int iParams, int iFirst, int * piArgFlags )
{
   PZH_DYNADDR pFunction = ( PZH_DYNADDR ) pFunctionRaw;

   if( ! pFunction )
      return;

#if defined( ZH_ARCH_64BIT )
   {
      int iRetType  = iFuncFlags & _MASK_CTYPE;
      int iEncoding = iFuncFlags & _MASK_ENCODING;
      int iOptions  = iFuncFlags & _MASK_OPTIONS;

      iParams -= iFirst - 1;

      if( iParams <= _DYNEXEC_MAXPARAM )
      {
         int iRetTypeRaw;
         ZH_DYNVAL ret;
         ZH_DYNARG * pArg;
         int tmp;

         ZH_U64 rawpar[ _DYNEXEC_MAXPARAM ];

         ret.t.n64 = 0;

         if( iRetType == ZH_DYN_CTYPE_DOUBLE )
            iRetTypeRaw = _RETTYPERAW_DOUBLE;
         else if( iRetType == ZH_DYN_CTYPE_FLOAT )
            iRetTypeRaw = _RETTYPERAW_FLOAT;
         else
            iRetTypeRaw = _RETTYPERAW_INT64;

         if( iParams )
            pArg = ( ZH_DYNARG * ) zh_xgrabz( iParams * sizeof( ZH_DYNARG ) );
         else
            pArg = NULL;

         for( tmp = 0; tmp < iParams; ++tmp )
         {
            PZH_ITEM pParam = zh_param( iFirst + tmp, ZH_IT_ANY );

            if( piArgFlags )
            {
               pArg[ tmp ].iType     = piArgFlags[ tmp ] & _MASK_CTYPE;
               pArg[ tmp ].iEncoding = piArgFlags[ tmp ] & _MASK_ENCODING;
               pArg[ tmp ].iOptions  = piArgFlags[ tmp ] & _MASK_OPTIONS;
            }
            else
            {
               pArg[ tmp ].iType     = ZH_DYN_CTYPE_DEFAULT;
               pArg[ tmp ].iEncoding = iEncoding;
               pArg[ tmp ].iOptions  = iOptions;
            }

            if( pArg[ tmp ].iType == ZH_DYN_CTYPE_DEFAULT )
               pArg[ tmp ].iType = zh_zhToCtype( ZH_ITEM_TYPE( pParam ) );

            pArg[ tmp ].bByRef = ZH_ISBYREF( iFirst + tmp );

            rawpar[ tmp ] = zh_u64par( pParam, &pArg[ tmp ] );
         }

         switch( iRetTypeRaw )
         {
            case _RETTYPERAW_INT64:
               ZH_DYN_FUN_CALL( iParams, n64, FX64_64 );
               break;
            case _RETTYPERAW_DOUBLE:
               ZH_DYN_FUN_CALL( iParams, nDB, FX64_DB );
               break;
            case _RETTYPERAW_FLOAT:
               ZH_DYN_FUN_CALL( iParams, nFL, FX64_FL );
               break;
         }

         zh_u64ret( zh_stackReturnItem(), iRetType, iEncoding, ret, -1 );

         for( tmp = 0; tmp < iParams; ++tmp )
         {
            if( pArg[ tmp ].bByRef )
            {
               PZH_ITEM pItem = zh_itemNew( NULL );

               zh_itemParamStoreForward( ( ZH_USHORT ) ( iFirst + tmp ),
                  zh_u64ret( pItem, pArg[ tmp ].iType, pArg[ tmp ].iEncoding, pArg[ tmp ].value,
                     ( pArg[ tmp ].iOptions & ZH_DYC_OPT_NULLTERM ) != 0 ? -1 : ( ZH_ISIZ ) zh_parclen( iFirst + tmp ) ) );

               zh_itemRelease( pItem );
            }

            if( pArg[ tmp ].bRawBuffer )
               zh_xfree( pArg[ tmp ].hString );
            else
               zh_strfree( pArg[ tmp ].hString );
         }

         if( pArg )
            zh_xfree( pArg );
      }
      else
         zh_errRT_BASE( EG_LIMIT, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
   }
#elif defined( ZH_ARCH_32BIT )
   {
      int iCallConv = iFuncFlags & _MASK_CALLCONV;
      int iRetType  = iFuncFlags & _MASK_CTYPE;
      int iEncoding = iFuncFlags & _MASK_ENCODING;
      int iOptions  = iFuncFlags & _MASK_OPTIONS;

      iParams -= iFirst - 1;

      if( iParams <= _DYNEXEC_MAXPARAM )
      {
         int iRetTypeRaw;
         ZH_DYNVAL ret;
         ZH_DYNARG * pArg;
         int tmp;

         int iParamsRaw = 0;
         ZH_U32 rawpar[ _DYNEXEC_MAXPARAM * 2 ];

#if ! defined( ZH_LONG_LONG_OFF )
         ret.t.n64 = 0;
#else
         memset( &ret, 0, sizeof( ret ) );
#endif

         if( iRetType == ZH_DYN_CTYPE_DOUBLE )
            iRetTypeRaw = _RETTYPERAW_DOUBLE;
         else if( iRetType == ZH_DYN_CTYPE_FLOAT )
            iRetTypeRaw = _RETTYPERAW_FLOAT;
         else if( iRetType == ZH_DYN_CTYPE_LLONG ||
                  iRetType == ZH_DYN_CTYPE_LLONG_UNSIGNED )
            iRetTypeRaw = _RETTYPERAW_INT64;
         else
            iRetTypeRaw = _RETTYPERAW_INT32;

         if( iParams )
            pArg = ( ZH_DYNARG * ) zh_xgrabz( iParams * sizeof( ZH_DYNARG ) );
         else
            pArg = NULL;

         for( tmp = 0; tmp < iParams; ++tmp )
         {
            PZH_ITEM pParam = zh_param( iFirst + tmp, ZH_IT_ANY );

            ZH_U32 r1;
            ZH_U32 r2;
            ZH_BOOL b64;

            if( piArgFlags )
            {
               pArg[ tmp ].iType     = piArgFlags[ tmp ] & _MASK_CTYPE;
               pArg[ tmp ].iEncoding = piArgFlags[ tmp ] & _MASK_ENCODING;
               pArg[ tmp ].iOptions  = piArgFlags[ tmp ] & _MASK_OPTIONS;
            }
            else
            {
               pArg[ tmp ].iType     = ZH_DYN_CTYPE_DEFAULT;
               pArg[ tmp ].iEncoding = iEncoding;
               pArg[ tmp ].iOptions  = iOptions;
            }

            if( pArg[ tmp ].iType == ZH_DYN_CTYPE_DEFAULT )
               pArg[ tmp ].iType = zh_zhToCtype( ZH_ITEM_TYPE( pParam ) );

            pArg[ tmp ].bByRef = ZH_ISBYREF( iFirst + tmp );

            zh_u32par( pParam, &pArg[ tmp ], &r1, &r2, &b64 );

            rawpar[ iParamsRaw++ ] = r1;
            if( b64 )
               rawpar[ iParamsRaw++ ] = r2;
         }

         switch( iCallConv )
         {
#if defined( ZH_OS_WIN )
            case ZH_DYN_CALLCONV_STDCALL:
               switch( iRetTypeRaw )
               {
                  case _RETTYPERAW_INT32:
                     ZH_DYN_FUN_CALL( iParamsRaw, n32, FX86_S32 );
                     break;
                  case _RETTYPERAW_INT64:
                     ZH_DYN_FUN_CALL( iParamsRaw, n64, FX86_S64 );
                     break;
                  case _RETTYPERAW_DOUBLE:
                     ZH_DYN_FUN_CALL( iParamsRaw, nDB, FX86_SDB );
                     break;
                  case _RETTYPERAW_FLOAT:
                     ZH_DYN_FUN_CALL( iParamsRaw, nFL, FX86_SFL );
                     break;
               }
               break;
#endif
#if defined( ZH_OS_OS2 )
            case ZH_DYN_CALLCONV_SYSCALL:
               switch( iRetTypeRaw )
               {
                  case _RETTYPERAW_INT32:
                     ZH_DYN_FUN_CALL( iParamsRaw, n32, FX86_O32 );
                     break;
                  case _RETTYPERAW_INT64:
                     ZH_DYN_FUN_CALL( iParamsRaw, n64, FX86_O64 );
                     break;
                  case _RETTYPERAW_DOUBLE:
                     ZH_DYN_FUN_CALL( iParamsRaw, nDB, FX86_ODB );
                     break;
                  case _RETTYPERAW_FLOAT:
                     ZH_DYN_FUN_CALL( iParamsRaw, nFL, FX86_OFL );
                     break;
               }
               break;
#endif
            default: /* ZH_DYN_CALLCONV_CDECL */
               switch( iRetTypeRaw )
               {
                  case _RETTYPERAW_INT32:
                     ZH_DYN_FUN_CALL( iParamsRaw, n32, FX86_C32 );
                     break;
                  case _RETTYPERAW_INT64:
#if ! defined( ZH_LONG_LONG_OFF )
                     ZH_DYN_FUN_CALL( iParamsRaw, n64, FX86_C64 );
#endif
                     break;
                  case _RETTYPERAW_DOUBLE:
                     ZH_DYN_FUN_CALL( iParamsRaw, nDB, FX86_CDB );
                     break;
                  case _RETTYPERAW_FLOAT:
                     ZH_DYN_FUN_CALL( iParamsRaw, nFL, FX86_CFL );
                     break;
               }
               break;
         }

         zh_u32ret( zh_stackReturnItem(), iRetType, iEncoding, ret, -1 );

         for( tmp = 0; tmp < iParams; ++tmp )
         {
            if( pArg[ tmp ].bByRef )
            {
               PZH_ITEM pItem = zh_itemNew( NULL );

               zh_itemParamStoreForward( ( ZH_USHORT ) ( iFirst + tmp ),
                  zh_u32ret( pItem, pArg[ tmp ].iType, pArg[ tmp ].iEncoding, pArg[ tmp ].value,
                     ( pArg[ tmp ].iOptions & ZH_DYC_OPT_NULLTERM ) != 0 ? -1 : ( ZH_ISIZ ) zh_parclen( iFirst + tmp ) ) );

               zh_itemRelease( pItem );
            }

            if( pArg[ tmp ].bRawBuffer )
               zh_xfree( pArg[ tmp ].hString );
            else
               zh_strfree( pArg[ tmp ].hString );
         }

         if( pArg )
            zh_xfree( pArg );
      }
      else
         zh_errRT_BASE( EG_LIMIT, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
   }
#else
   ZH_SYMBOL_UNUSED( iFuncFlags );
   ZH_SYMBOL_UNUSED( iParams );
   ZH_SYMBOL_UNUSED( iFirst );
   ZH_SYMBOL_UNUSED( piArgFlags );
#endif
}
