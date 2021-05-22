/*
 * Header file for the generated C language source code
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

#ifndef ZH_VMPUB_H_
#define ZH_VMPUB_H_

#include "zh_defs.h"

/* Maximum symbol name length handled by Ziher compiler and runtime. */
#define ZH_SYMBOL_NAME_LEN   63

ZH_EXTERN_BEGIN

#ifdef _ZH_API_INTERNAL_

struct _ZH_SYMB;

#  define ZH_ITEM_TYPERAW( p )   ( ( p )->type )
#  define ZH_ITEM_TYPE( p )      ( ZH_ITEM_TYPERAW( p ) & ~ ZH_IT_DEFAULT )
#  define ZH_OBJ_CLASS( p )      ( ( p )->item.asArray.value->uiClass )
#  define ZH_ARRAY_OBJ( p )      ( ( p )->item.asArray.value->uiClass != 0 )
#  define ZH_VM_PUSHNIL()        do { zh_stackAllocItem()->type = ZH_IT_NIL; } while( 0 )

#  define ZH_ITEM_GET_NUMINTRAW( p )  ( ZH_IS_INTEGER( p ) ? \
                                        ( ZH_MAXINT ) (p)->item.asInteger.value : \
                                        ( ZH_MAXINT ) (p)->item.asLong.value )

#  define ZH_ITEM_PUT_NUMINTRAW( p, v )  \
               do { \
                  if( ZH_LIM_INT( v ) ) \
                  { \
                     (p)->type = ZH_IT_INTEGER; \
                     (p)->item.asInteger.length = ZH_INT_EXPLENGTH( v ); \
                     (p)->item.asInteger.value = ( int ) (v); \
                  } \
                  else \
                  { \
                     (p)->type = ZH_IT_LONG; \
                     (p)->item.asLong.value = (v); \
                     (p)->item.asLong.length = ZH_LONG_EXPLENGTH( v ); \
                  } \
               } while( 0 )

#  if ZH_VMINT_MAX >= LONG_MAX
#     define ZH_ITEM_PUT_LONGRAW( p, v )  \
               do { \
                  (p)->type = ZH_IT_INTEGER; \
                  (p)->item.asInteger.value = ( int ) (v); \
                  (p)->item.asInteger.length = ZH_INT_LENGTH( v ); \
               } while( 0 )
#  else
#     define ZH_ITEM_PUT_LONGRAW( p, v )  \
               do { \
                  (p)->type = ZH_IT_LONG; \
                  (p)->item.asLong.value = (v); \
                  (p)->item.asLong.length = ZH_LONG_LENGTH( v ); \
               } while( 0 )
#  endif

#  define ZH_ITEM_GET_NUMDBLRAW( p )   ( ZH_IS_INTEGER( p ) ? \
                                          ( double ) (p)->item.asInteger.value : \
                                          ( ZH_IS_LONG( p ) ? \
                                             ( double ) (p)->item.asLong.value : \
                                                        (p)->item.asDouble.value ) )

#  define ZH_VM_ISFUNC( pSym ) ( ( pSym )->value.pFunPtr )

#  define ZH_VM_FUNCUNREF( pSym ) \
               do { \
                  if( ( ( pSym )->scope.value & ZH_FS_DEFERRED ) && \
                      ( pSym )->pDynSym ) \
                     pSym = ( pSym )->pDynSym->pSymbol; \
               } while( 0 )

#  define ZH_VM_EXECUTE( pSym ) \
               do { \
                  /* Running pCode dynamic function from .zhb? */ \
                  if( ( pSym )->scope.value & ZH_FS_PCODEFUNC ) \
                     zh_vmExecute( ( pSym )->value.pCodeFunc->pCode, \
                                   ( pSym )->value.pCodeFunc->pSymbols ); \
                  else \
                     ( pSym )->value.pFunPtr(); \
               } while( 0 )

   /* dynamic symbol structure */
   typedef struct _ZH_DYNS
   {
      struct _ZH_SYMB * pSymbol; /* pointer to its relative local symbol */
      ZH_USHORT uiSymNum;         /* dynamic symbol number */
#  if ! defined( ZH_NO_PROFILER )
      ZH_ULONG  ulCalls;          /* profiler support */
      ZH_ULONG  ulTime;           /* profiler support */
      ZH_ULONG  ulRecurse;        /* profiler support */
#  endif /* ! ZH_NO_PROFILER */
   } ZH_DYNS, * PZH_DYNS;

   /* pCode dynamic function - ZHB */
   typedef struct _ZH_PCODEFUNC
   {
      ZH_BYTE *   pCode;         /* function body - PCODE */
      struct _ZH_SYMB * pSymbols;/* module symbol table */
   } ZH_PCODEFUNC, * PZH_PCODEFUNC;

#else /* ! _ZH_API_INTERNAL_ */

#  undef _ZH_API_MACROS_
#  undef _ZH_STACK_MACROS_

/* This is ugly trick but works without speed overhead */
#if defined( __cplusplus )
#     define ZH_ITEM_TYPERAW( p )   ( * static_cast< ZH_TYPE * >( p ) )
#else
#     define ZH_ITEM_TYPERAW( p )   ( * ( ZH_TYPE * ) ( p ) )
#endif
/* if you do not like it then use this definition */
/* #  define ZH_ITEM_TYPERAW( p )   ( zh_itemType( p ) ) */

#  define ZH_ITEM_TYPE( p )      ( ZH_ITEM_TYPERAW( p ) & ~ZH_IT_DEFAULT )

#  define ZH_OBJ_CLASS( p )   ( zh_objGetClass( p ) )
#  define ZH_ARRAY_OBJ( p )   ( zh_arrayIsObject( p ) )

   /* basic types */
   typedef void *  PZH_ITEM;
   typedef void *  PZH_CODEBLOCK;
   typedef void *  PZH_PCODEFUNC;

   typedef void    ZH_STACK;

   typedef void *  PZH_DYNS;

#endif /* ! _ZH_API_INTERNAL_ */


/* symbol support structure */
typedef struct _ZH_SYMB
{
   const char *   szName;           /* the name of the symbol */
   union
   {
      ZH_SYMBOLSCOPE value;         /* the scope of the symbol */
      void *         pointer;       /* filler to force alignment */
   } scope;
   union
   {
      PZH_FUNC       pFunPtr;       /* machine code function address for function symbol table entries */
      PZH_PCODEFUNC  pCodeFunc;     /* PCODE function address */
      void *         pStaticsBase;  /* base offset to array of statics */
   } value;
   PZH_DYNS       pDynSym;          /* pointer to its dynamic symbol if defined */
} ZH_SYMBOL, * PZH_SYMBOL;

#define ZH_DYNS_FUNC( zhfunc )   ZH_BOOL zhfunc( PZH_DYNS pDynSymbol, void * Cargo )
typedef ZH_DYNS_FUNC( ( * PZH_DYNS_FUNC ) );

#define ZH_CARGO_FUNC( func )    void func( void *cargo )
typedef ZH_CARGO_FUNC( ( * PZH_CARGO_FUNC ) );

typedef void * ( * PZH_ALLOCUPDT_FUNC )( void *, int );

typedef void ( * ZH_INIT_FUNC )( void * );
/* List of functions used by zh_vmAtInit()/zh_vmAtExit() */
typedef struct _ZH_FUNC_LIST
{
   ZH_INIT_FUNC   pFunc;
   void *         cargo;
   void *         hDynLib;
   struct _ZH_FUNC_LIST * pNext;
} ZH_FUNC_LIST, * PZH_FUNC_LIST;

/* Ziher Functions scope ( ZH_SYMBOLSCOPE ) */
#define ZH_FS_PUBLIC    0x0001
#define ZH_FS_STATIC    0x0002
#define ZH_FS_FIRST     0x0004
#define ZH_FS_INIT      0x0008
#define ZH_FS_EXIT      0x0010
#define ZH_FS_MESSAGE   0x0020
#define ZH_FS_MEMVAR    0x0080
#define ZH_FS_PCODEFUNC 0x0100
#define ZH_FS_LOCAL     0x0200
#define ZH_FS_DYNCODE   0x0400
#define ZH_FS_DEFERRED  0x0800
#define ZH_FS_FRAME     0x1000
#define ZH_FS_USED      0x2000

#define ZH_FS_INITEXIT ( ZH_FS_INIT | ZH_FS_EXIT )

extern ZH_EXPORT void zh_vmExecute( const ZH_BYTE * pCode, PZH_SYMBOL pSymbols ) ZH_FLATTEN_ATTR;  /* invokes the virtual machine */

ZH_EXTERN_END

#endif /* ZH_VMPUB_H_ */
