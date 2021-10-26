/*
 * Header file for the Extend API, Array API, misc API and base declarations
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

/* FIXME: There are several things in this file which are not part of the
          standard Ziher API, in other words these things are not
          guaranteed to remain unchanged. To avoid confusion these should be
          moved to somewhere else (like zhrtl.h). [vszakats] */

#ifndef ZH_APIEXT_H_
#define ZH_APIEXT_H_

#include "zh_vm_pub.h"

ZH_EXTERN_BEGIN


/* this definition signals that number of decimal places for double value
 * was not specified at compile time (the value is a result of optimization
 * performed by the compiler)
 */
#define ZH_DEFAULT_WIDTH     255
#define ZH_DEFAULT_DECIMALS  255


/* items types and type checking macros */
#define ZH_IT_NIL       0x00000
#define ZH_IT_POINTER   0x00001
#define ZH_IT_INTEGER   0x00002
#define ZH_IT_HASH      0x00004
#define ZH_IT_LONG      0x00008
#define ZH_IT_DOUBLE    0x00010
#define ZH_IT_DATE      0x00020
#define ZH_IT_TIMESTAMP 0x00040
#define ZH_IT_LOGICAL   0x00080
#define ZH_IT_SYMBOL    0x00100
#define ZH_IT_ALIAS     0x00200
#define ZH_IT_STRING    0x00400
#define ZH_IT_MEMOFLAG  0x00800
#define ZH_IT_MEMO      ( ZH_IT_MEMOFLAG | ZH_IT_STRING )
#define ZH_IT_BLOCK     0x01000
#define ZH_IT_BYREF     0x02000
#define ZH_IT_MEMVAR    0x04000
#define ZH_IT_ARRAY     0x08000
#define ZH_IT_ENUM      0x10000
#define ZH_IT_EXTREF    0x20000
#define ZH_IT_DEFAULT   0x40000
#define ZH_IT_RECOVER   0x80000
#define ZH_IT_OBJECT    ZH_IT_ARRAY
#define ZH_IT_NUMERIC   ( ZH_IT_INTEGER | ZH_IT_LONG | ZH_IT_DOUBLE )
#define ZH_IT_NUMINT    ( ZH_IT_INTEGER | ZH_IT_LONG )
#define ZH_IT_DATETIME  ( ZH_IT_DATE | ZH_IT_TIMESTAMP )
#define ZH_IT_ANY       0xFFFFFFFF
#define ZH_IT_COMPLEX   ( ZH_IT_BLOCK | ZH_IT_ARRAY | ZH_IT_HASH | ZH_IT_POINTER | /* ZH_IT_MEMVAR | ZH_IT_ENUM | ZH_IT_EXTREF |*/ ZH_IT_BYREF | ZH_IT_STRING )
#define ZH_IT_GCITEM    ( ZH_IT_BLOCK | ZH_IT_ARRAY | ZH_IT_HASH | ZH_IT_POINTER | ZH_IT_BYREF )
#define ZH_IT_EVALITEM  ( ZH_IT_BLOCK | ZH_IT_SYMBOL )
#define ZH_IT_HASHKEY   ( ZH_IT_INTEGER | ZH_IT_LONG | ZH_IT_DOUBLE | ZH_IT_DATE | ZH_IT_TIMESTAMP | ZH_IT_STRING | ZH_IT_POINTER )


/*
 * these ones are can be the most efficiently optimized on some CPUs
 */
#define ZH_IS_NIL( p )        ( ZH_ITEM_TYPE( p ) == ZH_IT_NIL )
#define ZH_IS_ARRAY( p )      ( ( ZH_ITEM_TYPERAW( p ) & ZH_IT_ARRAY ) != 0 )
#define ZH_IS_BLOCK( p )      ( ( ZH_ITEM_TYPERAW( p ) & ZH_IT_BLOCK ) != 0 )
#define ZH_IS_DATE( p )       ( ( ZH_ITEM_TYPERAW( p ) & ZH_IT_DATE ) != 0 )
#define ZH_IS_TIMESTAMP( p )  ( ( ZH_ITEM_TYPERAW( p ) & ZH_IT_TIMESTAMP ) != 0 )
#define ZH_IS_DOUBLE( p )     ( ( ZH_ITEM_TYPERAW( p ) & ZH_IT_DOUBLE ) != 0 )
#define ZH_IS_INTEGER( p )    ( ( ZH_ITEM_TYPERAW( p ) & ZH_IT_INTEGER ) != 0 )
#define ZH_IS_LOGICAL( p )    ( ( ZH_ITEM_TYPERAW( p ) & ZH_IT_LOGICAL ) != 0 )
#define ZH_IS_LONG( p )       ( ( ZH_ITEM_TYPERAW( p ) & ZH_IT_LONG ) != 0 )
#define ZH_IS_SYMBOL( p )     ( ( ZH_ITEM_TYPERAW( p ) & ZH_IT_SYMBOL ) != 0 )
#define ZH_IS_POINTER( p )    ( ( ZH_ITEM_TYPERAW( p ) & ZH_IT_POINTER ) != 0 )
#define ZH_IS_HASH( p )       ( ( ZH_ITEM_TYPERAW( p ) & ZH_IT_HASH ) != 0 )
#define ZH_IS_MEMO( p )       ( ( ZH_ITEM_TYPERAW( p ) & ZH_IT_MEMOFLAG ) != 0 )
#define ZH_IS_STRING( p )     ( ( ZH_ITEM_TYPERAW( p ) & ZH_IT_STRING ) != 0 )
#define ZH_IS_MEMVAR( p )     ( ( ZH_ITEM_TYPERAW( p ) & ZH_IT_MEMVAR ) != 0 )
#define ZH_IS_ENUM( p )       ( ( ZH_ITEM_TYPERAW( p ) & ZH_IT_ENUM ) != 0 )
#define ZH_IS_EXTREF( p )     ( ( ZH_ITEM_TYPERAW( p ) & ZH_IT_EXTREF ) != 0 )
#define ZH_IS_BYREF( p )      ( ( ZH_ITEM_TYPERAW( p ) & ZH_IT_BYREF ) != 0 )
#define ZH_IS_NUMERIC( p )    ( ( ZH_ITEM_TYPERAW( p ) & ZH_IT_NUMERIC ) != 0 )
#define ZH_IS_NUMINT( p )     ( ( ZH_ITEM_TYPERAW( p ) & ZH_IT_NUMINT ) != 0 )
#define ZH_IS_DATETIME( p )   ( ( ZH_ITEM_TYPERAW( p ) & ZH_IT_DATETIME ) != 0 )
#define ZH_IS_COMPLEX( p )    ( ( ZH_ITEM_TYPERAW( p ) & ZH_IT_COMPLEX ) != 0 )
#define ZH_IS_GCITEM( p )     ( ( ZH_ITEM_TYPERAW( p ) & ZH_IT_GCITEM ) != 0 )
#define ZH_IS_EVALITEM( p )   ( ( ZH_ITEM_TYPERAW( p ) & ZH_IT_EVALITEM ) != 0 )
#define ZH_IS_HASHKEY( p )    ( ( ZH_ITEM_TYPERAW( p ) & ZH_IT_HASHKEY ) != 0 )
#define ZH_IS_BADITEM( p )    ( ( ZH_ITEM_TYPERAW( p ) & ZH_IT_COMPLEX ) != 0 && ( ZH_ITEM_TYPERAW( p ) & ~( ZH_IT_COMPLEX | ZH_IT_MEMOFLAG | ZH_IT_DEFAULT ) ) != 0 )
#define ZH_IS_OBJECT( p )     ( ZH_IS_ARRAY( p ) && ZH_ARRAY_OBJ( p ) )
#define ZH_IS_NUMBER( p )     ZH_IS_NUMERIC( p )


#define ZH_ISNIL( n )         ( zh_extIsNil( n ) )                         /* NOTE: Intentionally using a different method */
#define ZH_ISCHAR( n )        ( zh_param( n, ZH_IT_STRING ) != NULL )
#define ZH_IS_PARAM_NUM( n )         ( zh_param( n, ZH_IT_NUMERIC ) != NULL )
#define ZH_ISLOGICAL( n )         ( zh_param( n, ZH_IT_LOGICAL ) != NULL )
#define ZH_ISDATE( n )        ( zh_param( n, ZH_IT_DATE ) != NULL )
#define ZH_ISTIMESTAMP( n )   ( zh_param( n, ZH_IT_TIMESTAMP ) != NULL )
#define ZH_ISMEMO( n )        ( zh_param( n, ZH_IT_MEMOFLAG ) != NULL )
#define ZH_ISBYREF( n )       ( ( zh_parinfo( n ) & ZH_IT_BYREF ) != 0 )   /* NOTE: Intentionally using a different method */
#define ZH_ISARRAY( n )       ( zh_param( n, ZH_IT_ARRAY ) != NULL )
#define ZH_ISOBJECT( n )      ( zh_extIsObject( n ) )
#define ZH_ISBLOCK( n )       ( zh_param( n, ZH_IT_BLOCK ) != NULL )
#define ZH_ISPOINTER( n )     ( zh_param( n, ZH_IT_POINTER ) != NULL )
#define ZH_ISHASH( n )        ( zh_param( n, ZH_IT_HASH ) != NULL )
#define ZH_ISSYMBOL( n )      ( zh_param( n, ZH_IT_SYMBOL ) != NULL )
#define ZH_ISEVALITEM( n )    ( zh_param( n, ZH_IT_EVALITEM ) != NULL )
#define ZH_ISDATETIME( n )    ( zh_param( n, ZH_IT_DATETIME ) != NULL )



#ifdef _ZH_API_INTERNAL_

/* forward declarations */
struct _ZH_CODEBLOCK;
struct _ZH_BASEARRAY;
struct _ZH_BASEHASH;
struct _ZH_ITEM;
struct _ZH_EXTREF;

typedef struct _ZH_STACK_STATE
{
   ZH_ISIZ   nBaseItem;        /* stack base offset of previous func/proc */
   ZH_SIZE   nPrivateBase;     /* memvars base offset of previous func/proc */
   void *    pStatics;         /* statics frame of previous func/proc */
   ZH_USHORT uiClass;          /* class when message is sent */
   ZH_USHORT uiMethod;         /* number of class method */
   ZH_USHORT uiLineNo;         /* current line number */
   ZH_USHORT fDebugging;       /* debugger active */
} ZH_STACK_STATE, * PZH_STACK_STATE; /* used to save/restore stack state in zh_vmDo)_ */


/* Internal structures that holds data */
struct zh_struArray
{
   struct _ZH_BASEARRAY * value;
};

struct zh_struHash
{
   struct _ZH_BASEHASH * value;
};

struct zh_struBlock
{
   struct _ZH_CODEBLOCK * value;
   ZH_USHORT paramcnt;
   ZH_USHORT lineno;
   ZH_USHORT hclass;
   ZH_USHORT method;
};

struct zh_struPointer
{
   void * value;
   ZH_BOOL collect;
   ZH_BOOL single;
};

struct zh_struDateTime
{
   long julian;
   long time;
};

struct zh_struDouble
{
   double value;
   ZH_USHORT length;
   ZH_USHORT decimal;
};

struct zh_struInteger
{
   int value;
   ZH_USHORT length;
};

struct zh_struLong
{
   ZH_MAXINT value;
   ZH_USHORT length;
};

struct zh_struLogical
{
   ZH_BOOL value;
};

struct zh_struMemvar
{
   struct _ZH_ITEM * value;
};

struct zh_struRefer
{
   union {
      struct _ZH_BASEARRAY * array;       /* array (statics and array item references) */
      struct _ZH_CODEBLOCK * block;       /* codeblock */
      struct _ZH_ITEM * itemPtr;          /* item pointer */
      struct _ZH_ITEM ** *itemsbasePtr;   /* local variables */
   } BasePtr;
   ZH_ISIZ offset;                        /* 0 for static variables */
   ZH_ISIZ value;
};

struct zh_struEnum
{
   struct _ZH_ITEM * basePtr;             /* base item pointer */
   struct _ZH_ITEM * valuePtr;            /* value item pointer */
   ZH_ISIZ offset;
};

struct zh_struExtRef
{
   void * value;                          /* value item pointer */
   const struct _ZH_EXTREF * func;        /* extended reference functions */
};

struct zh_struString
{
   ZH_SIZE length;
   ZH_SIZE allocated;     /* size of memory block allocated for string value, 0 for static strings */
   char * value;
};

struct zh_struSymbol
{
   PZH_SYMBOL        value;
   PZH_STACK_STATE stackstate;      /* function stack state */
   ZH_USHORT       paramcnt;        /* number of passed parameters in function call */
   ZH_USHORT       paramdeclcnt;    /* number of declared parameters in function definition */
};

struct zh_struRecover
{
   const ZH_BYTE * recover;    /* address of recover code */
   ZH_SIZE         base;       /* previous recover base */
   ZH_USHORT       flags;      /* previous recovery state and recover type */
   ZH_USHORT       request;    /* requested action */
};

/* items hold at the virtual machine stack */
typedef struct _ZH_ITEM
{
   ZH_TYPE type;
   union
   {
      struct zh_struArray     asArray;
      struct zh_struBlock     asBlock;
      struct zh_struDateTime  asDateTime;
      struct zh_struDouble    asDouble;
      struct zh_struInteger   asInteger;
      struct zh_struLogical   asLogical;
      struct zh_struLong      asLong;
      struct zh_struPointer   asPointer;
      struct zh_struHash      asHash;
      struct zh_struMemvar    asMemvar;
      struct zh_struRefer     asRefer;
      struct zh_struEnum      asEnum;
      struct zh_struExtRef    asExtRef;
      struct zh_struString    asString;
      struct zh_struSymbol    asSymbol;
      struct zh_struRecover   asRecover;
   } item;
} ZH_ITEM, * PZH_ITEM;

/* internal structure for arrays */
typedef struct _ZH_BASEARRAY
{
   PZH_ITEM    pItems;       /* pointer to the array items */
   ZH_SIZE     nLen;         /* number of items in the array */
   ZH_SIZE     nAllocated;   /* number of allocated items */
   ZH_USHORT   uiClass;      /* offset to the classes base if it is an object */
   ZH_USHORT   uiPrevCls;    /* for fixing after access super */
} ZH_BASEARRAY, * PZH_BASEARRAY;

#ifndef _ZH_HASH_INTERNAL_
/* internal structure for hashes */
typedef struct _ZH_BASEHASH
{
   void *      value;
} ZH_BASEHASH, * PZH_BASEHASH;
#endif

/* internal structure for codeblocks */
typedef struct _ZH_CODEBLOCK
{
   const ZH_BYTE * pCode;    /* codeblock pcode */
   PZH_SYMBOL    pSymbols;     /* codeblocks symbols */
   PZH_SYMBOL    pDefSymb;     /* symbol where the codeblock was created */
   PZH_ITEM    pLocals;      /* table with referenced local variables */
   void *      pStatics;     /* STATICs base frame */
   ZH_USHORT   uiLocals;     /* number of referenced local variables */
   ZH_SHORT    dynBuffer;    /* is pcode buffer allocated dynamically, SHORT used instead of ZH_BOOL intentionally to force optimal alignment */
} ZH_CODEBLOCK, * PZH_CODEBLOCK;

typedef void     ( * ZH_EXTREF_FUNC0 )( void * );
typedef PZH_ITEM ( * ZH_EXTREF_FUNC1 )( PZH_ITEM );
typedef PZH_ITEM ( * ZH_EXTREF_FUNC2 )( PZH_ITEM, PZH_ITEM );
typedef void     ( * ZH_EXTREF_FUNC3 )( PZH_ITEM );

typedef struct _ZH_EXTREF
{
   ZH_EXTREF_FUNC1 read;
   ZH_EXTREF_FUNC2 write;
   ZH_EXTREF_FUNC3 copy;
   ZH_EXTREF_FUNC0 clear;
   ZH_EXTREF_FUNC0 mark;
} ZH_EXTREF, * PZH_EXTREF;

typedef struct
{
   void *   value;
   PZH_ITEM pDest;
} ZH_NESTED_REF, * PZH_NESTED_REF;

typedef struct
{
   ZH_SIZE        nSize;
   ZH_SIZE        nCount;
   PZH_NESTED_REF pRefs;
} ZH_NESTED_CLONED, * PZH_NESTED_CLONED;

#endif /* _ZH_API_INTERNAL_ */


/* RDD method return codes */
typedef unsigned int ZH_ERRCODE;

#define ZH_SUCCESS         0
#define ZH_FAILURE         1

#if defined( _ZH_API_INTERNAL_ )
/* NOTE: Deprecated. Use 'zh_vmPushEvalSym()' instead of 'zh_vmPushSymbol( &zh_symEval )' */
extern ZH_SYMBOL zh_symEval;
#endif

extern ZH_EXPORT void     zh_xinit( void );                           /* Initialize fixed memory subsystem */
extern ZH_EXPORT void     zh_xexit( void );                           /* Deinitialize fixed memory subsystem */
extern ZH_EXPORT void *   zh_xalloc( ZH_SIZE nSize );                 /* allocates memory, returns NULL on failure */
extern ZH_EXPORT void *   zh_xgrab( ZH_SIZE nSize ) ZH_MALLOC_ATTR ZH_ALLOC_SIZE_ATTR( 1 ); /* allocates memory, exits on failure */
extern ZH_EXPORT void     zh_xfree( void * pMem );                    /* frees memory */
extern ZH_EXPORT void *   zh_xrealloc( void * pMem, ZH_SIZE nSize ) ZH_ALLOC_SIZE_ATTR( 2 ); /* reallocates memory */
extern ZH_EXPORT ZH_SIZE  zh_xsize( void * pMem );                    /* returns the size of an allocated memory block */
extern ZH_EXPORT const char * zh_xinfo( void * pMem, int * piLine );  /* return allocation place (function name and line number) */
extern ZH_EXPORT ZH_SIZE  zh_xquery( int iMode );                     /* Query different types of memory information */
extern ZH_EXPORT ZH_BOOL  zh_xtraced( void );
extern ZH_EXPORT void     zh_xsetfilename( const char * szValue );
extern ZH_EXPORT void     zh_xsetinfo( const char * szValue );
#ifdef _ZH_API_INTERNAL_
extern void zh_xinit_thread( void );
extern void zh_xexit_thread( void );
extern void zh_xclean( void );
#endif

#ifdef _ZH_API_INTERNAL_
extern void       zh_xRefInc( void * pMem );    /* increment reference counter */
extern ZH_BOOL    zh_xRefDec( void * pMem );    /* decrement reference counter, return ZH_TRUE when 0 reached */
extern void       zh_xRefFree( void * pMem );   /* decrement reference counter and free the block when 0 reached */
extern ZH_COUNTER zh_xRefCount( void * pMem );  /* return number of references */
extern void *     zh_xRefResize( void * pMem, ZH_SIZE nSave, ZH_SIZE nSize, ZH_SIZE * pnAllocated );   /* reallocates memory, create copy if reference counter greater then 1 */


#endif /* _ZH_API_INTERNAL_ */


#define zh_xgrabz( n )        memset( zh_xgrab( ( n ) ), 0, ( n ) )
#define zh_xmemdup( p, n )    memcpy( zh_xgrab( ( n ) ), ( p ), ( n ) )
#define zh_xreallocz( p, n )  memset( zh_xrealloc( ( p ), ( n ) ), 0, ( n ) )

/* #if UINT_MAX == ULONG_MAX */
/* it fails on 64-bit platforms where int has 32 bits and long has 64 bits.
   we need these functions only when max(size_t) < max(long)
   and only on 16-bit platforms, so the below condition seems to be
   more reasonable. */
#if UINT_MAX > USHRT_MAX
   /* NOTE: memcpy()/memset() can work with ZH_SIZE data blocks */
   #define  zh_xmemcpy  memcpy
   #define  zh_xmemset  memset
#else
   /* NOTE: otherwise, the zh_xmemcpy() and zh_xmemset() functions
            will be used to copy and/or set ZH_SIZE data blocks */
extern ZH_EXPORT void * zh_xmemcpy( void * pDestArg, const void * pSourceArg, ZH_SIZE nLen ); /* copy more than memcpy() can */
extern ZH_EXPORT void * zh_xmemset( void * pDestArg, int iFill, ZH_SIZE nLen ); /* set more than memset() can */
#endif

/* virtual memory */
typedef unsigned long ZH_VMHANDLE;

extern ZH_EXPORT ZH_VMHANDLE zh_xvalloc( ZH_SIZE nSize, ZH_USHORT nFlags );
extern ZH_EXPORT void        zh_xvfree( ZH_VMHANDLE h );
extern ZH_EXPORT ZH_VMHANDLE zh_xvrealloc( ZH_VMHANDLE h, ZH_SIZE nSize, ZH_USHORT nFlags );
extern ZH_EXPORT void *      zh_xvlock( ZH_VMHANDLE h );
extern ZH_EXPORT void        zh_xvunlock( ZH_VMHANDLE h );
extern ZH_EXPORT void *      zh_xvwire( ZH_VMHANDLE h );
extern ZH_EXPORT void        zh_xvunwire( ZH_VMHANDLE h );
extern ZH_EXPORT ZH_SIZE     zh_xvlockcount( ZH_VMHANDLE h );
extern ZH_EXPORT ZH_SIZE     zh_xvsize( ZH_VMHANDLE h );
extern ZH_EXPORT ZH_VMHANDLE zh_xvheapnew( ZH_SIZE nSize );
extern ZH_EXPORT void        zh_xvheapdestroy( ZH_VMHANDLE h );
extern ZH_EXPORT ZH_VMHANDLE zh_xvheapresize( ZH_VMHANDLE h, ZH_SIZE nSize );
extern ZH_EXPORT ZH_SIZE     zh_xvheapalloc( ZH_VMHANDLE h, ZH_SIZE nSize );
extern ZH_EXPORT void        zh_xvheapfree( ZH_VMHANDLE h, ZH_SIZE nOffset );
extern ZH_EXPORT void *      zh_xvheaplock( ZH_VMHANDLE h, ZH_SIZE nOffset );
extern ZH_EXPORT void        zh_xvheapunlock( ZH_VMHANDLE h, ZH_SIZE nOffset );

/* garbage collector */
#define ZH_GARBAGE_FUNC( zhfunc )   void zhfunc( void * Cargo ) /* callback function for cleaning garbage memory pointer */
typedef ZH_GARBAGE_FUNC( ( * PZH_GARBAGE_FUNC ) );

typedef struct
{
   PZH_GARBAGE_FUNC  clear;
   PZH_GARBAGE_FUNC  mark;
} ZH_GC_FUNCS;

extern ZH_EXPORT  void *   zh_gcAllocate( ZH_SIZE nSize, const ZH_GC_FUNCS * pFuncs ); /* allocates a memory controlled by the garbage collector */
extern ZH_EXPORT  void     zh_gcFree( void * pAlloc ); /* deallocates a memory allocated by the garbage collector */
extern ZH_EXPORT  void *   zh_gcLock( void * pAlloc ); /* do not release passed memory block */
extern ZH_EXPORT  void *   zh_gcUnlock( void * pAlloc ); /* passed block is allowed to be released */
extern ZH_EXPORT  void     zh_gcMark( void * pAlloc ); /* mark given block as used */
extern ZH_EXPORT  void     zh_gcRefInc( void * pAlloc );  /* increment reference counter */
extern ZH_EXPORT  void     zh_gcRefFree( void * pAlloc ); /* decrement reference counter and free the block when 0 reached */

extern ZH_EXPORT  void     zh_gcDummyClear( void * Cargo ); /* dummy GC clear function */
extern ZH_EXPORT  void     zh_gcDummyMark( void * Cargo ); /* dummy GC mark function */

extern PZH_ITEM   zh_gcGripGet( PZH_ITEM pItem );
extern void       zh_gcGripDrop( PZH_ITEM pItem );

#ifdef _ZH_API_INTERNAL_
extern const ZH_GC_FUNCS * zh_gcFuncs( void *pBlock );  /* return cleanup function pointer */
extern void       zh_gcAttach( void * pBlock );
extern void *     zh_gcAllocRaw( ZH_SIZE nSize, const ZH_GC_FUNCS * pFuncs ); /* allocates a memory controlled by the garbage collector */
extern void       zh_gcGripMark( void * Cargo ); /* mark complex variables inside given item as used */
extern void       zh_gcItemRef( PZH_ITEM pItem ); /* mark complex variables inside given item as used */
extern void       zh_vmIsStackRef( void ); /* hvm.c - mark all local variables as used */
extern void       zh_vmIsStaticRef( void ); /* hvm.c - mark all static variables as used */
extern void       zh_gcReleaseAll( void ); /* release all memory blocks unconditionally */

extern ZH_COUNTER zh_gcRefCount( void * pAlloc );  /* return number of references */

#if 0
#define zh_gcRefInc( p )      zh_xRefInc( ZH_GC_PTR( p ) )
#define zh_gcRefCount( p )    zh_xRefCount( ZH_GC_PTR( p ) )
#define zh_gcFunc( p )        ( ZH_GC_PTR( p )->pFunc )
#endif

#endif /* _ZH_API_INTERNAL_ */
extern ZH_EXPORT void         zh_gcCollect( void ); /* checks if a single memory block can be released */
extern ZH_EXPORT void         zh_gcCollectAll( ZH_BOOL fForce ); /* checks if all memory blocks can be released */

/* Extend API */
extern ZH_EXPORT ZH_ULONG     zh_parinfo( int iParam ); /* Determine the param count or data type */
extern ZH_EXPORT ZH_SIZE      zh_parinfa( int iParamNum, ZH_SIZE nArrayIndex ); /* retrieve length or element type of an array parameter */
extern ZH_EXPORT PZH_ITEM     zh_param( int iParam, long lMask ); /* retrieve a generic parameter */
extern ZH_EXPORT PZH_ITEM     zh_paramError( int iParam ); /* Returns either the generic parameter or a NIL item if param not provided */
extern ZH_EXPORT ZH_BOOL      zh_extIsNil( int iParam );
extern ZH_EXPORT ZH_BOOL      zh_extIsArray( int iParam );
extern ZH_EXPORT ZH_BOOL      zh_extIsObject( int iParam );

extern ZH_EXPORT const char * zh_parc( int iParam ); /* retrieve a string parameter */
extern ZH_EXPORT const char * zh_parcx( int iParam ); /* retrieve a string parameter */
extern ZH_EXPORT ZH_SIZE      zh_parclen( int iParam ); /* retrieve a string parameter length */
extern ZH_EXPORT ZH_SIZE      zh_parcsiz( int iParam ); /* retrieve a by-reference string parameter length, including terminator */
extern ZH_EXPORT const char * zh_pards( int iParam ); /* retrieve a date as a string YYYYMMDD */
extern ZH_EXPORT char *       zh_pardsbuff( char * szDate, int iParam ); /* retrieve a date as a string YYYYMMDD */
extern ZH_EXPORT long         zh_pardl( int iParam ); /* retrieve a date as a long integer */
extern ZH_EXPORT double       zh_partd( int iParam ); /* retrieve a timestamp as a double number */
extern ZH_EXPORT ZH_BOOL      zh_partdt( long * plJulian, long * plMilliSec , int iParam ); /* retrieve a timestamp as two long numbers */
extern ZH_EXPORT int          zh_parl( int iParam ); /* retrieve a logical parameter as an int */
extern ZH_EXPORT int          zh_parldef( int iParam, int iDefValue ); /* retrieve a logical parameter as an int, return default value if parameter isn't logical */
extern ZH_EXPORT double       zh_parnd( int iParam ); /* retrieve a numeric parameter as a double */
extern ZH_EXPORT int          zh_parni( int iParam ); /* retrieve a numeric parameter as a integer */
extern ZH_EXPORT int          zh_parnidef( int iParam, int iDefValue ); /* retrieve a numeric parameter as a integer, return default value if parameter isn't numeric */
extern ZH_EXPORT long         zh_parnl( int iParam ); /* retrieve a numeric parameter as a long */
extern ZH_EXPORT long         zh_parnldef( int iParam, long lDefValue ); /* retrieve a numeric parameter as a long, return default value if parameter isn't numeric */
extern ZH_EXPORT ZH_ISIZ      zh_parns( int iParam ); /* retrieve a numeric parameter as a ZH_SIZE */
extern ZH_EXPORT ZH_ISIZ      zh_parnsdef( int iParam, ZH_ISIZ nDefValue ); /* retrieve a numeric parameter as a ZH_SIZE, return default value if parameter isn't numeric */
extern ZH_EXPORT ZH_MAXINT    zh_parnint( int iParam ); /* retrieve a numeric parameter as a ZH_MAXINT */
extern ZH_EXPORT ZH_MAXINT    zh_parnintdef( int iParam, ZH_MAXINT nDefValue ); /* retrieve a numeric parameter as a ZH_MAXINT, return default value if parameter isn't numeric */
extern ZH_EXPORT void *       zh_parptr( int iParam ); /* retrieve a parameter as a pointer */
extern ZH_EXPORT void *       zh_parptrGC( const ZH_GC_FUNCS * pFuncs, int iParam ); /* retrieve a parameter as a pointer if it's a pointer to GC allocated block */
#ifndef ZH_LONG_LONG_OFF
extern ZH_EXPORT ZH_LONGLONG  zh_parnll( int iParam ); /* retrieve a numeric parameter as a long long */
#endif

extern ZH_EXPORT const char * zh_parvc( int iParam, ... ); /* retrieve a string parameter */
extern ZH_EXPORT const char * zh_parvcx( int iParam, ... ); /* retrieve a string parameter */
extern ZH_EXPORT ZH_SIZE      zh_parvclen( int iParam, ... ); /* retrieve a string parameter length */
extern ZH_EXPORT ZH_SIZE      zh_parvcsiz( int iParam, ... ); /* retrieve a by-reference string parameter length, including terminator */
extern ZH_EXPORT const char * zh_parvds( int iParam, ... ); /* retrieve a date as a string YYYYMMDD */
extern ZH_EXPORT char *       zh_parvdsbuff( char * szDate, int iParam, ... ); /* retrieve a date as a string YYYYMMDD */
extern ZH_EXPORT long         zh_parvdl( int iParam, ... ); /* retrieve a date as a long integer */
extern ZH_EXPORT double       zh_parvtd( int iParam, ... ); /* retrieve a timestamp as a double number */
extern ZH_EXPORT ZH_BOOL      zh_parvtdt( long * plJulian, long * plMilliSec , int iParam, ... ); /* retrieve a timestamp as two long numbers */
extern ZH_EXPORT int          zh_parvl( int iParam, ... ); /* retrieve a logical parameter as an int */
extern ZH_EXPORT double       zh_parvnd( int iParam, ... ); /* retrieve a numeric parameter as a double */
extern ZH_EXPORT int          zh_parvni( int iParam, ... ); /* retrieve a numeric parameter as a integer */
extern ZH_EXPORT long         zh_parvnl( int iParam, ... ); /* retrieve a numeric parameter as a long */
extern ZH_EXPORT ZH_ISIZ      zh_parvns( int iParam, ... ); /* retrieve a numeric parameter as a ZH_SIZE */
extern ZH_EXPORT ZH_MAXINT    zh_parvnint( int iParam, ... ); /* retrieve a numeric parameter as a ZH_MAXINT */
extern ZH_EXPORT void *       zh_parvptr( int iParam, ... ); /* retrieve a parameter as a pointer */
extern ZH_EXPORT void *       zh_parvptrGC( const ZH_GC_FUNCS * pFuncs, int iParam, ... ); /* retrieve a parameter as a pointer if it's a pointer to GC allocated block */
#ifndef ZH_LONG_LONG_OFF
extern ZH_EXPORT ZH_LONGLONG  zh_parvnll( int iParam, ... ); /* retrieve a numeric parameter as a long long */
#endif

extern ZH_EXPORT int    zh_pcount( void ); /* returns the number of supplied parameters */
extern ZH_EXPORT void   zh_ret( void );  /* post a NIL return value */
extern ZH_EXPORT void   zh_retc( const char * szText ); /* returns a string */
extern ZH_EXPORT void   zh_retc_null( void ); /* returns an empty string */
extern ZH_EXPORT void   zh_retc_buffer( char * szText ); /* same as above, but accepts an allocated buffer */
extern ZH_EXPORT void   zh_retc_const( const char * szText ); /* returns a string as a pcode based string */
extern ZH_EXPORT void   zh_retclen( const char * szText, ZH_SIZE nLen ); /* returns a string with a specific length */
extern ZH_EXPORT void   zh_retclen_buffer( char * szText, ZH_SIZE nLen ); /* same as above, but accepts an allocated buffer */
extern ZH_EXPORT void   zh_retclen_const( const char * szText, ZH_SIZE nLen ); /* returns a string with a specific length formed from a constant buffer */
extern ZH_EXPORT void   zh_retds( const char * szDate );  /* returns a date, must use YYYYMMDD format */
extern ZH_EXPORT void   zh_retd( int iYear, int iMonth, int iDay ); /* returns a date */
extern ZH_EXPORT void   zh_retdl( long lJulian ); /* returns a long value as a Julian date */
extern ZH_EXPORT void   zh_rettd( double dTimeStamp ); /* returns a double value as a timestamp */
extern ZH_EXPORT void   zh_rettdt( long lJulian, long lMilliSec ); /* returns two long values as a timestamp */
extern ZH_EXPORT void   zh_retl( int iTrueFalse ); /* returns a logical integer */
extern ZH_EXPORT void   zh_retnd( double dNumber ); /* returns a double */
extern ZH_EXPORT void   zh_retni( int iNumber ); /* returns a integer number */
extern ZH_EXPORT void   zh_retnl( long lNumber ); /* returns a long number */
extern ZH_EXPORT void   zh_retns( ZH_ISIZ nNumber ); /* returns a size */
extern ZH_EXPORT void   zh_retnint( ZH_MAXINT nNumber ); /* returns a long number */
extern ZH_EXPORT void   zh_retnlen( double dNumber, int iWidth, int iDec ); /* returns a double, with specific width and decimals */
extern ZH_EXPORT void   zh_retndlen( double dNumber, int iWidth, int iDec ); /* returns a double, with specific width and decimals */
extern ZH_EXPORT void   zh_retnilen( int iNumber, int iWidth ); /* returns a integer number, with specific width */
extern ZH_EXPORT void   zh_retnllen( long lNumber, int iWidth ); /* returns a long number, with specific width */
extern ZH_EXPORT void   zh_retnintlen( ZH_MAXINT nNumber, int iWidth ); /* returns a long long number, with specific width */
extern ZH_EXPORT void   zh_reta( ZH_SIZE nLen ); /* returns an array with a specific length */
extern ZH_EXPORT void   zh_retptr( void * ptr ); /* returns a pointer */
extern ZH_EXPORT void   zh_retptrGC( void * ptr ); /* returns a pointer to an allocated memory, collected by GC */
#ifndef ZH_LONG_LONG_OFF
extern ZH_EXPORT void   zh_retnll( ZH_LONGLONG lNumber ); /* returns a long long number */
extern ZH_EXPORT void   zh_retnlllen( ZH_LONGLONG lNumber, int iWidth ); /* returns a long long number, with specific width */
#endif

#define ZH_IS_VALID_INDEX( idx, max )  ( (idx) > 0 && ( ZH_SIZE ) (idx) <= (max) )

#ifdef _ZH_API_MACROS_

#define zh_pcount()                          ( ( int ) ( zh_stackBaseItem() )->item.asSymbol.paramcnt )

#define zh_ret()                             zh_itemClear( zh_stackReturnItem() )
#define zh_reta( nLen )                      zh_arrayNew( zh_stackReturnItem(), nLen )
#define zh_retc( szText )                    zh_itemPutC( zh_stackReturnItem(), szText )
#define zh_retc_null()                       zh_itemPutC( zh_stackReturnItem(), NULL )
#define zh_retc_buffer( szText )             zh_itemPutCPtr( zh_stackReturnItem(), szText )
#define zh_retc_const( szText )              zh_itemPutCConst( zh_stackReturnItem(), szText )
#define zh_retclen( szText, nLen )           zh_itemPutCL( zh_stackReturnItem(), szText, nLen )
#define zh_retclen_buffer( szText, nLen )    zh_itemPutCLPtr( zh_stackReturnItem(), szText, nLen )
#define zh_retclen_const( szText, nLen )     zh_itemPutCLConst( zh_stackReturnItem(), szText, nLen )
#define zh_retds( szDate )                   zh_itemPutDS( zh_stackReturnItem(), szDate )
#define zh_retd( iYear, iMonth, iDay )       zh_itemPutD( zh_stackReturnItem(), iYear, iMonth, iDay )
#define zh_retdl( lJulian )                  zh_itemPutDL( zh_stackReturnItem(), lJulian )
#define zh_rettd( dTimeStamp )               zh_itemPutTD( zh_stackReturnItem(), dTimeStamp )
#define zh_rettdt( lJulian, lMilliSec )      zh_itemPutTDT( zh_stackReturnItem(), lJulian, lMilliSec )
#define zh_retl( iLogical )                  zh_itemPutL( zh_stackReturnItem(), ( iLogical ) ? ZH_TRUE : ZH_FALSE )
#define zh_retnd( dNumber )                  zh_itemPutND( zh_stackReturnItem(), dNumber )
#define zh_retni( iNumber )                  zh_itemPutNI( zh_stackReturnItem(), iNumber )
#define zh_retnl( lNumber )                  zh_itemPutNL( zh_stackReturnItem(), lNumber )
#define zh_retns( nNumber )                  zh_itemPutNS( zh_stackReturnItem(), nNumber )
#define zh_retnll( lNumber )                 zh_itemPutNLL( zh_stackReturnItem(), lNumber )
#define zh_retnlen( dNumber, iWidth, iDec )  zh_itemPutNLen( zh_stackReturnItem(), dNumber, iWidth, iDec )
#define zh_retndlen( dNumber, iWidth, iDec ) zh_itemPutNDLen( zh_stackReturnItem(), dNumber, iWidth, iDec )
#define zh_retnilen( iNumber, iWidth )       zh_itemPutNILen( zh_stackReturnItem(), iNumber, iWidth )
#define zh_retnllen( lNumber, iWidth )       zh_itemPutNLLen( zh_stackReturnItem(), lNumber, iWidth )
#define zh_retnlllen( lNumber, iWidth )      zh_itemPutNLLLen( zh_stackReturnItem(), lNumber, iWidth )
#define zh_retnint( iNumber )                zh_itemPutNInt( zh_stackReturnItem(), iNumber )
#define zh_retnintlen( lNumber, iWidth )     zh_itemPutNIntLen( zh_stackReturnItem(), lNumber, iWidth )
#define zh_retptr( pointer )                 zh_itemPutPtr( zh_stackReturnItem(), pointer )
#define zh_retptrGC( pointer )               zh_itemPutPtrGC( zh_stackReturnItem(), pointer )

#endif /* _ZH_API_MACROS_ */


extern ZH_EXPORT int    zh_stor( int iParam ); /* stores a NIL on a variable by reference */
extern ZH_EXPORT int    zh_storc( const char * szText, int iParam ); /* stores a szString on a variable by reference */
extern ZH_EXPORT int    zh_storclen( const char * szText, ZH_SIZE nLength, int iParam ); /* stores a fixed length string on a variable by reference */
extern ZH_EXPORT int    zh_storclen_buffer( char * szText, ZH_SIZE nLength, int iParam ); /* stores a fixed length string buffer on a variable by reference */
extern ZH_EXPORT int    zh_stords( const char * szDate, int iParam );   /* szDate must have YYYYMMDD format */
extern ZH_EXPORT int    zh_stordl( long lJulian, int iParam ); /* lJulian must be a date in Julian format */
extern ZH_EXPORT int    zh_stortd( double dTimeStamp, int iParam ); /* stores a double value as timestamp on a variable by reference */
extern ZH_EXPORT int    zh_stortdt( long lJulian, long lMilliSec, int iParam ); /* stores two long values as timestamp on a variable by reference */
extern ZH_EXPORT int    zh_storl( int iLogical, int iParam ); /* stores a logical integer on a variable by reference */
extern ZH_EXPORT int    zh_storni( int iValue, int iParam ); /* stores an integer on a variable by reference */
extern ZH_EXPORT int    zh_stornl( long lValue, int iParam ); /* stores a long on a variable by reference */
extern ZH_EXPORT int    zh_storns( ZH_ISIZ nValue, int iParam ); /* stores a ZH_SIZE on a variable by reference */
extern ZH_EXPORT int    zh_stornd( double dValue, int iParam ); /* stores a double on a variable by reference */
extern ZH_EXPORT int    zh_stornint( ZH_MAXINT nValue, int iParam ); /* stores a ZH_MAXINT on a variable by reference */
extern ZH_EXPORT int    zh_storptr( void * pointer, int iParam ); /* stores a pointer on a variable by reference */
extern ZH_EXPORT int    zh_storptrGC( void * pointer, int iParam ); /* stores a pointer to GC block on a variable by reference */
#ifndef ZH_LONG_LONG_OFF
extern ZH_EXPORT int    zh_stornll( ZH_LONGLONG llValue, int iParam ); /* stores a long long on a variable by reference */
#endif

extern ZH_EXPORT int    zh_storvc( const char * szText, int iParam, ... ); /* stores a szString on a variable by reference */
extern ZH_EXPORT int    zh_storvclen( const char * szText, ZH_SIZE nLength, int iParam, ... ); /* stores a fixed length string on a variable by reference */
extern ZH_EXPORT int    zh_storvclen_buffer( char * szText, ZH_SIZE nLength, int iParam, ... ); /* stores a fixed length string buffer on a variable by reference */
extern ZH_EXPORT int    zh_storvds( const char * szDate, int iParam, ... );   /* szDate must have YYYYMMDD format */
extern ZH_EXPORT int    zh_storvdl( long lJulian, int iParam, ... ); /* lJulian must be a date in Julian format */
extern ZH_EXPORT int    zh_storvtd( double dTimeStamp, int iParam, ... ); /* stores a double value as timestamp on a variable by reference */
extern ZH_EXPORT int    zh_storvtdt( long lJulian, long lMilliSec, int iParam, ... ); /* stores two long values as timestamp on a variable by reference */
extern ZH_EXPORT int    zh_storvl( int iLogical, int iParam, ... ); /* stores a logical integer on a variable by reference */
extern ZH_EXPORT int    zh_storvni( int iValue, int iParam, ... ); /* stores an integer on a variable by reference */
extern ZH_EXPORT int    zh_storvnl( long lValue, int iParam, ... ); /* stores a long on a variable by reference */
extern ZH_EXPORT int    zh_storvns( ZH_ISIZ nValue, int iParam, ... ); /* stores a ZH_SIZE on a variable by reference */
extern ZH_EXPORT int    zh_storvnd( double dValue, int iParam, ... ); /* stores a double on a variable by reference */
extern ZH_EXPORT int    zh_storvnint( ZH_MAXINT nValue, int iParam, ... ); /* stores a ZH_MAXINT on a variable by reference */
extern ZH_EXPORT int    zh_storvptr( void * pointer, int iParam, ... ); /* stores a pointer on a variable by reference */
extern ZH_EXPORT int    zh_storvptrGC( void * pointer, int iParam, ... ); /* stores a pointer to GC block on a variable by reference */
#ifndef ZH_LONG_LONG_OFF
extern ZH_EXPORT int    zh_storvnll( ZH_LONGLONG llValue, int iParam, ... ); /* stores a long long on a variable by reference */
#endif

/* array management */
extern ZH_EXPORT ZH_BOOL      zh_arrayNew( PZH_ITEM pItem, ZH_SIZE nLen ); /* creates a new array */
extern ZH_EXPORT ZH_SIZE      zh_arrayLen( PZH_ITEM pArray ); /* retrieves the array length */
extern ZH_EXPORT ZH_BOOL      zh_arrayIsObject( PZH_ITEM pArray ); /* retrieves if the array is an object */
extern ZH_EXPORT void *       zh_arrayId( PZH_ITEM pArray ); /* retrieves the array unique ID */
extern ZH_EXPORT ZH_COUNTER   zh_arrayRefs( PZH_ITEM pArray ); /* retrieves number of references to the array */
extern ZH_EXPORT PZH_ITEM     zh_arrayFromId( PZH_ITEM pItem, void * pArrayId );
extern ZH_EXPORT ZH_BOOL      zh_arrayAdd( PZH_ITEM pArray, PZH_ITEM pItemValue ); /* add a new item to the end of an array item */
extern ZH_EXPORT ZH_BOOL      zh_arrayAddForward( PZH_ITEM pArray, PZH_ITEM pValue ); /* add a new item to the end of an array item with no incrementing of reference counters */
extern ZH_EXPORT ZH_BOOL      zh_arrayIns( PZH_ITEM pArray, ZH_SIZE nIndex ); /* insert a nil item into an array, without changing the length */
extern ZH_EXPORT ZH_BOOL      zh_arrayDel( PZH_ITEM pArray, ZH_SIZE nIndex ); /* delete an array item, without changing length */
extern ZH_EXPORT ZH_BOOL      zh_arraySize( PZH_ITEM pArray, ZH_SIZE nLen ); /* sets the array total length */
extern ZH_EXPORT ZH_BOOL      zh_arrayLast( PZH_ITEM pArray, PZH_ITEM pResult ); /* retrieve last item in an array */
extern ZH_EXPORT ZH_BOOL      zh_arrayGet( PZH_ITEM pArray, ZH_SIZE nIndex, PZH_ITEM pItem ); /* retrieves an item */
extern ZH_EXPORT ZH_BOOL      zh_arrayGetItemRef( PZH_ITEM pArray, ZH_SIZE nIndex, PZH_ITEM pItem ); /* create a reference to an array element */
/* zh_arrayGetItemPtr() is dangerous, be sure that base ARRAY value will not be changed (e.g. resized) */
extern ZH_EXPORT PZH_ITEM     zh_arrayGetItemPtr( PZH_ITEM pArray, ZH_SIZE nIndex ); /* returns pointer to specified element of the array */
extern ZH_EXPORT ZH_SIZE      zh_arrayCopyC( PZH_ITEM pArray, ZH_SIZE nIndex, char * szBuffer, ZH_SIZE nLen ); /* copy a string from an array item */
extern ZH_EXPORT char *       zh_arrayGetC( PZH_ITEM pArray, ZH_SIZE nIndex ); /* retrieves the string contained on an array element */
extern ZH_EXPORT const char * zh_arrayGetCPtr( PZH_ITEM pArray, ZH_SIZE nIndex ); /* retrieves the string pointer on an array element */
extern ZH_EXPORT ZH_SIZE      zh_arrayGetCLen( PZH_ITEM pArray, ZH_SIZE nIndex ); /* retrieves the string length contained on an array element */
extern ZH_EXPORT void *       zh_arrayGetPtr( PZH_ITEM pArray, ZH_SIZE nIndex ); /* retrieves the pointer contained on an array element */
extern ZH_EXPORT void *       zh_arrayGetPtrGC( PZH_ITEM pArray, ZH_SIZE nIndex, const ZH_GC_FUNCS * pFuncs ); /* retrieves the GC pointer contained on an array element */
extern ZH_EXPORT PZH_SYMBOL     zh_arrayGetSymbol( PZH_ITEM pArray, ZH_SIZE nIndex ); /* retrieves symbol contained on an array element */
extern ZH_EXPORT ZH_BOOL      zh_arrayGetL( PZH_ITEM pArray, ZH_SIZE nIndex ); /* retrieves the logical value contained on an array element */
extern ZH_EXPORT int          zh_arrayGetNI( PZH_ITEM pArray, ZH_SIZE nIndex ); /* retrieves the int value contained on an array element */
extern ZH_EXPORT long         zh_arrayGetNL( PZH_ITEM pArray, ZH_SIZE nIndex ); /* retrieves the long numeric value contained on an array element */
extern ZH_EXPORT ZH_ISIZ      zh_arrayGetNS( PZH_ITEM pArray, ZH_SIZE nIndex ); /* retrieves the ZH_SIZE value contained on an array element */
extern ZH_EXPORT ZH_MAXINT    zh_arrayGetNInt( PZH_ITEM pArray, ZH_SIZE nIndex ); /* retrieves the ZH_MAXINT value contained on an array element */
extern ZH_EXPORT double       zh_arrayGetND( PZH_ITEM pArray, ZH_SIZE nIndex ); /* retrieves the double value contained on an array element */
extern ZH_EXPORT char *       zh_arrayGetDS( PZH_ITEM pArray, ZH_SIZE nIndex, char * szDate ); /* retrieves the date value contained in an array element */
extern ZH_EXPORT long         zh_arrayGetDL( PZH_ITEM pArray, ZH_SIZE nIndex ); /* retrieves the date value contained in an array element, as a long integer */
extern ZH_EXPORT double       zh_arrayGetTD( PZH_ITEM pArray, ZH_SIZE nIndex ); /* retrieves the timestamp value contained in an array element, as a double value */
extern ZH_EXPORT ZH_BOOL      zh_arrayGetTDT( PZH_ITEM pArray, ZH_SIZE nIndex, long * plJulian, long * plMilliSec ); /* retrieves the timestamp value contained in an array element, as two long values */
extern ZH_EXPORT ZH_TYPE      zh_arrayGetType( PZH_ITEM pArray, ZH_SIZE nIndex ); /* retrieves the type of an array item */
extern ZH_EXPORT ZH_BOOL      zh_arraySet( PZH_ITEM pArray, ZH_SIZE nIndex, PZH_ITEM pItem ); /* sets an array element */
extern ZH_EXPORT ZH_BOOL      zh_arraySetForward( PZH_ITEM pArray, ZH_SIZE nIndex, PZH_ITEM pItem ); /* sets an array element by forwarding it's value */
extern ZH_EXPORT ZH_BOOL      zh_arraySetDS( PZH_ITEM pArray, ZH_SIZE nIndex, const char * szDate );
extern ZH_EXPORT ZH_BOOL      zh_arraySetDL( PZH_ITEM pArray, ZH_SIZE nIndex, long lDate );
extern ZH_EXPORT ZH_BOOL      zh_arraySetTD( PZH_ITEM pArray, ZH_SIZE nIndex, double dTimeStamp );
extern ZH_EXPORT ZH_BOOL      zh_arraySetTDT( PZH_ITEM pArray, ZH_SIZE nIndex, long lJulian, long lMilliSec );
extern ZH_EXPORT ZH_BOOL      zh_arraySetL( PZH_ITEM pArray, ZH_SIZE nIndex, ZH_BOOL fValue );
extern ZH_EXPORT ZH_BOOL      zh_arraySetNI( PZH_ITEM pArray, ZH_SIZE nIndex, int iNumber );
extern ZH_EXPORT ZH_BOOL      zh_arraySetNL( PZH_ITEM pArray, ZH_SIZE nIndex, long lNumber );
extern ZH_EXPORT ZH_BOOL      zh_arraySetNS( PZH_ITEM pArray, ZH_SIZE nIndex, ZH_ISIZ nNumber );
#ifndef ZH_LONG_LONG_OFF
extern ZH_EXPORT ZH_BOOL      zh_arraySetNLL( PZH_ITEM pArray, ZH_SIZE nIndex, ZH_LONGLONG llNumber );
#endif
extern ZH_EXPORT ZH_BOOL      zh_arraySetNInt( PZH_ITEM pArray, ZH_SIZE nIndex, ZH_MAXINT nNumber );
extern ZH_EXPORT ZH_BOOL      zh_arraySetND( PZH_ITEM pArray, ZH_SIZE nIndex, double dNumber );
extern ZH_EXPORT ZH_BOOL      zh_arraySetC( PZH_ITEM pArray, ZH_SIZE nIndex, const char * szText );
extern ZH_EXPORT ZH_BOOL      zh_arraySetCL( PZH_ITEM pArray, ZH_SIZE nIndex, const char * szText, ZH_SIZE nLen );
extern ZH_EXPORT ZH_BOOL      zh_arraySetCPtr( PZH_ITEM pArray, ZH_SIZE nIndex, char * szText );
extern ZH_EXPORT ZH_BOOL      zh_arraySetCLPtr( PZH_ITEM pArray, ZH_SIZE nIndex, char * szText, ZH_SIZE nLen );
extern ZH_EXPORT ZH_BOOL      zh_arraySetCConst( PZH_ITEM pArray, ZH_SIZE nIndex, const char * szText );
extern ZH_EXPORT ZH_BOOL      zh_arraySetPtr( PZH_ITEM pArray, ZH_SIZE nIndex, void * pValue );
extern ZH_EXPORT ZH_BOOL      zh_arraySetPtrGC( PZH_ITEM pArray, ZH_SIZE nIndex, void * pValue );
extern ZH_EXPORT ZH_BOOL      zh_arraySetSymbol( PZH_ITEM pArray, ZH_SIZE nIndex, PZH_SYMBOL pSymbol );
extern ZH_EXPORT ZH_BOOL      zh_arrayFill( PZH_ITEM pArray, PZH_ITEM pValue, ZH_SIZE * pnStart, ZH_SIZE * pnCount ); /* fill an array with a given item */
extern ZH_EXPORT ZH_SIZE      zh_arrayScanCase( PZH_ITEM pArray, PZH_ITEM pValue, ZH_SIZE * pnStart, ZH_SIZE * pnCount, ZH_BOOL fExact, ZH_BOOL fMatchCase ); /* scan an array for a given item, or until code-block item returns ZH_TRUE */
extern ZH_EXPORT ZH_SIZE      zh_arrayRevScan( PZH_ITEM pArray, PZH_ITEM pValue, ZH_SIZE * pnStart, ZH_SIZE * pnCount, ZH_BOOL fExact ); /* scan an array for a given item, or until code-block item returns ZH_TRUE in reverted order */
extern ZH_EXPORT ZH_BOOL      zh_arrayEval( PZH_ITEM pArray, PZH_ITEM bBlock, ZH_SIZE * pnStart, ZH_SIZE * pnCount ); /* execute a code-block for every element of an array item */
extern ZH_EXPORT ZH_BOOL      zh_arrayCopy( PZH_ITEM pSrcArray, PZH_ITEM pDstArray, ZH_SIZE * pnStart, ZH_SIZE * pnCount, ZH_SIZE * pnTarget ); /* copy items from one array to another */
extern ZH_EXPORT PZH_ITEM     zh_arrayClone( PZH_ITEM pArray ); /* returns a duplicate of an existing array, including all nested items */
extern ZH_EXPORT PZH_ITEM     zh_arrayCloneTo( PZH_ITEM pDest, PZH_ITEM pArray ); /* returns a duplicate of an existing array, including all nested items */
extern ZH_EXPORT ZH_BOOL      zh_arraySort( PZH_ITEM pArray, ZH_SIZE * pnStart, ZH_SIZE * pnCount, PZH_ITEM pBlock ); /* sorts an array item */
extern ZH_EXPORT PZH_ITEM     zh_arrayFromStack( ZH_USHORT uiLen ); /* Creates and returns an Array of n Elements from the Eval Stack - Does NOT pop the items. */
extern ZH_EXPORT PZH_ITEM     zh_arrayFromParams( int iLevel ); /* Creates and returns an Array of Generic Parameters for a given call level */
extern ZH_EXPORT PZH_ITEM     zh_arrayBaseParams( void ); /* Creates and returns an Array of Generic Parameters for current base symbol. */
extern ZH_EXPORT PZH_ITEM     zh_arraySelfParams( void ); /* Creates and returns an Array of Generic Parameters for current base symbol with self item */
#ifndef ZH_LONG_LONG_OFF
extern ZH_EXPORT ZH_LONGLONG  zh_arrayGetNLL( PZH_ITEM pArray, ZH_SIZE nIndex ); /* retrieves the long long numeric value contained on an array element */
#endif
#ifdef _ZH_API_INTERNAL_
/* internal array API not exported */
extern void zh_arrayPushBase( PZH_BASEARRAY pBaseArray );
extern void zh_arraySwap( PZH_ITEM pArray1, PZH_ITEM pArray2 );
extern void zh_nestedCloneInit( PZH_NESTED_CLONED pClonedList, void * pValue, PZH_ITEM pDest );
extern void zh_nestedCloneFree( PZH_NESTED_CLONED pClonedList );
extern void zh_nestedCloneDo( PZH_ITEM pDstItem, PZH_ITEM pSrcItem, PZH_NESTED_CLONED pClonedList );
extern void zh_hashCloneBody( PZH_ITEM pDest, PZH_ITEM pHash, PZH_NESTED_CLONED pClonedList );
#endif

/* COMPATIBILITY */
#define zh_arrayScan( pArray, pValue, pnStart, pnCount, fExact )  zh_arrayScanCase( pArray, pValue, pnStart, pnCount, fExact, ZH_TRUE )

/* hash management */
extern ZH_EXPORT PZH_ITEM     zh_hashNew( PZH_ITEM pItem );
extern ZH_EXPORT ZH_SIZE      zh_hashLen( PZH_ITEM pHash );
extern ZH_EXPORT ZH_BOOL      zh_hashDel( PZH_ITEM pHash, PZH_ITEM pKey );
extern ZH_EXPORT ZH_BOOL      zh_hashAdd( PZH_ITEM pHash, PZH_ITEM pKey, PZH_ITEM pValue );
extern ZH_EXPORT ZH_BOOL      zh_hashAddNew( PZH_ITEM pHash, PZH_ITEM pKey, PZH_ITEM pValue );
extern ZH_EXPORT ZH_BOOL      zh_hashRemove( PZH_ITEM pHash, PZH_ITEM pItem );
extern ZH_EXPORT ZH_BOOL      zh_hashClear( PZH_ITEM pHash );
extern ZH_EXPORT ZH_BOOL      zh_hashAllocNewPair( PZH_ITEM pHash, PZH_ITEM * pKeyPtr, PZH_ITEM * pValPtr );
extern ZH_EXPORT void         zh_hashSort( PZH_ITEM pHash );
extern ZH_EXPORT PZH_ITEM     zh_hashClone( PZH_ITEM pHash );
extern ZH_EXPORT PZH_ITEM     zh_hashCloneTo( PZH_ITEM pDest, PZH_ITEM pHash );
extern ZH_EXPORT void         zh_hashJoin( PZH_ITEM pDest, PZH_ITEM pSource, int iType );
extern ZH_EXPORT ZH_BOOL      zh_hashScan( PZH_ITEM pHash, PZH_ITEM pKey, ZH_SIZE * pnPos );
extern ZH_EXPORT ZH_BOOL      zh_hashScanSoft( PZH_ITEM pHash, PZH_ITEM pKey, ZH_SIZE * pnPos );
extern ZH_EXPORT void         zh_hashPreallocate( PZH_ITEM pHash, ZH_SIZE nNewSize );
extern ZH_EXPORT PZH_ITEM     zh_hashGetKeys( PZH_ITEM pHash );
extern ZH_EXPORT PZH_ITEM     zh_hashGetValues( PZH_ITEM pHash );
extern ZH_EXPORT void         zh_hashSetDefault( PZH_ITEM pHash, PZH_ITEM pValue );
extern ZH_EXPORT PZH_ITEM     zh_hashGetDefault( PZH_ITEM pHash );
extern ZH_EXPORT void         zh_hashSetFlags( PZH_ITEM pHash, int iFlags );
extern ZH_EXPORT void         zh_hashClearFlags( PZH_ITEM pHash, int iFlags );
extern ZH_EXPORT int          zh_hashGetFlags( PZH_ITEM pHash );
extern ZH_EXPORT void *       zh_hashId( PZH_ITEM pHash ); /* retrieves the hash unique ID */
extern ZH_EXPORT ZH_COUNTER   zh_hashRefs( PZH_ITEM pHash ); /* retrieves number of references to the hash */

/* these zh_hashGet*() functions are dangerous, be sure that base HASH value will not be changed */
extern ZH_EXPORT PZH_ITEM    zh_hashGetItemPtr( PZH_ITEM pHash, PZH_ITEM pKey, int iFlags );
extern ZH_EXPORT PZH_ITEM    zh_hashGetItemRefPtr( PZH_ITEM pHash, PZH_ITEM pKey );
extern ZH_EXPORT PZH_ITEM    zh_hashGetCItemPtr( PZH_ITEM pHash, const char * pszKey );
extern ZH_EXPORT ZH_SIZE     zh_hashGetCItemPos( PZH_ITEM pHash, const char * pszKey );
extern ZH_EXPORT PZH_ITEM    zh_hashGetKeyAt( PZH_ITEM pHash, ZH_SIZE nPos );
extern ZH_EXPORT PZH_ITEM    zh_hashGetValueAt( PZH_ITEM pHash, ZH_SIZE nPos );

extern ZH_EXPORT ZH_BOOL     zh_hashDelAt( PZH_ITEM pHash, ZH_SIZE nPos );

/* hash item flags */
#define ZH_HASH_AUTOADD_NEVER       0x00
#define ZH_HASH_AUTOADD_ACCESS      0x01
#define ZH_HASH_AUTOADD_ASSIGN      0x02
#define ZH_HASH_AUTOADD_ALWAYS      ( ZH_HASH_AUTOADD_ACCESS | ZH_HASH_AUTOADD_ASSIGN )
#define ZH_HASH_AUTOADD_REFERENCE   ZH_HASH_AUTOADD_ALWAYS
#define ZH_HASH_AUTOADD_MASK        0x03

#define ZH_HASH_RESORT              0x08

#define ZH_HASH_IGNORECASE          0x10
#define ZH_HASH_BINARY              0x20
#define ZH_HASH_KEEPORDER           0x40

#define ZH_HASH_FLAG_MASK           0xFFFF
#define ZH_HASH_FLAG_DEFAULT        ( ZH_HASH_AUTOADD_ASSIGN | ZH_HASH_BINARY | ZH_HASH_KEEPORDER )

#define ZH_HASH_UNION      0  /* logical OR  on items in two hash tables */
#define ZH_HASH_INTERSECT  1  /* logical AND on items in two hash tables */
#define ZH_HASH_DIFFERENCE 2  /* logical XOR on items in two hash tables */
#define ZH_HASH_REMOVE     3  /* h1 & ( h1 ^ h2 ) */


/* string management */

extern const char * const zh_szAscii[ 256 ];    /* array of 1 character length strings */

extern ZH_EXPORT int       zh_stricmp( const char * s1, const char * s2 ); /* compare two strings without regards to case */
extern ZH_EXPORT int       zh_strnicmp( const char * s1, const char * s2, ZH_SIZE nLen ); /* compare two string without regards to case, limited by length */
extern ZH_EXPORT char *    zh_strupr( char * pszText ); /* convert a string in-place to upper-case */
extern ZH_EXPORT char *    zh_strlow( char * pszText ); /* convert a string in-place to lower-case */
extern ZH_EXPORT char *    zh_strdup( const char * pszText ); /* returns a pointer to a newly allocated copy of the source string */
extern ZH_EXPORT char *    zh_strndup( const char * pszText, ZH_SIZE nLen ); /* returns a pointer to a newly allocated copy of the source string not longer then nLen */
extern ZH_EXPORT char *    zh_strduptrim( const char * pszText ); /* returns a pointer to a newly allocated copy of the trimmed source string */
extern ZH_EXPORT ZH_SIZE   zh_strlentrim( const char * pszText ); /* like strlen() but result is the length of trimmed text */
extern ZH_EXPORT ZH_SIZE   zh_strnlen( const char * pszText, ZH_SIZE nLen ); /* like strlen() but result is limited to nLen */
extern ZH_EXPORT char *    zh_xstrcat( char * dest, const char * src, ... ); /* Concatenates multiple strings into a single result */
extern ZH_EXPORT char *    zh_xstrcpy( char * szDest, const char * szSrc, ... ); /* Concatenates multiple strings into a single result */
extern ZH_EXPORT ZH_BOOL   zh_compStrToNum( const char * szNum, ZH_SIZE nLen, ZH_MAXINT * pnVal, double * pdVal, int * piDec, int * piWidth );  /* converts string to number, sets iDec, iWidth and returns ZH_TRUE if results is double, used by compiler */
extern ZH_EXPORT ZH_BOOL   zh_valStrnToNum( const char * szNum, ZH_SIZE nLen, ZH_MAXINT * pnVal, double * pdVal, int * piDec, int * piWidth );  /* converts string to number, sets iDec, iWidth and returns ZH_TRUE if results is double, used by Val() */
extern ZH_EXPORT ZH_BOOL   zh_strToNum( const char * szNum, ZH_MAXINT * pnVal, double * pdVal ); /* converts string to number, returns ZH_TRUE if results is double */
extern ZH_EXPORT ZH_BOOL   zh_strnToNum( const char * szNum, ZH_SIZE nLen, ZH_MAXINT * pnVal, double * pdVal ); /* converts string to number, returns ZH_TRUE if results is double */
extern ZH_EXPORT int       zh_snprintf( char * buffer, size_t bufsize, const char * format, ... ) ZH_PRINTF_FORMAT( 3, 4 ); /* snprintf() equivalent */
extern ZH_EXPORT int       zh_vsnprintf( char * buffer, size_t bufsize, const char * format, va_list ap ); /* vsnprintf() equivalent */
extern ZH_EXPORT int       zh_printf_params( const char * format );

extern ZH_EXPORT ZH_BOOL   zh_strMatchFile( const char * pszString, const char * szPattern ); /* compare two strings using platform dependent rules for file matching */
extern ZH_EXPORT ZH_BOOL   zh_strMatchRegExp( const char * szString, const char * szPattern ); /* compare two strings using a regular expression pattern */
extern ZH_EXPORT ZH_BOOL   zh_strMatchWild( const char * szString, const char * szPattern ); /* compare two strings using pattern with wildcard (?*) - pattern have to be prefix of given string */
extern ZH_EXPORT ZH_BOOL   zh_strMatchWildExact( const char * szString, const char * szPattern ); /* compare two strings using pattern with wildcard (?*) - pattern have to cover whole string */
extern ZH_EXPORT ZH_BOOL   zh_strMatchCaseWildExact( const char * szString, const char * szPattern ); /* compare two strings using pattern with wildcard (?*) ignoring the case of the characters - pattern have to cover whole string */
extern ZH_EXPORT ZH_BOOL   zh_strEmpty( const char * szText, ZH_SIZE nLen ); /* returns whether a string contains only white space */
extern ZH_EXPORT void      zh_strDescend( char * szStringTo, const char * szStringFrom, ZH_SIZE nLen ); /* copy a string to a buffer, inverting each character */
extern ZH_EXPORT ZH_SIZE   zh_strAt( const char * szSub, ZH_SIZE nSubLen, const char * szText, ZH_SIZE nLen ); /* returns an index to a sub-string within another string */
extern ZH_EXPORT ZH_SIZE   zh_strAtI( const char * szSub, ZH_SIZE nSubLen, const char * szText, ZH_SIZE nLen ); /* returns an index to a sub-string within another, ignore the case of the characters */
extern ZH_EXPORT ZH_ISIZ   zh_strAtTBM( const char * needle, ZH_ISIZ m, const char * haystack, ZH_ISIZ n );

/* Warning: this functions works only with byte oriented CPs */
extern ZH_EXPORT char *    zh_strUpper( char * szText, ZH_SIZE nLen ); /* convert an existing string buffer to upper case */
extern ZH_EXPORT char *    zh_strLower( char * szText, ZH_SIZE nLen ); /* convert an existing string buffer to lower case */
extern ZH_EXPORT ZH_BOOL   zh_charIsDigit( int iChar );
extern ZH_EXPORT ZH_BOOL   zh_charIsAlpha( int iChar );
extern ZH_EXPORT ZH_BOOL   zh_charIsLower( int iChar );
extern ZH_EXPORT ZH_BOOL   zh_charIsUpper( int iChar );
extern ZH_EXPORT int       zh_charUpper( int iChar );  /* converts iChar to upper case */
extern ZH_EXPORT int       zh_charLower( int iChar );  /* converts iChar to lower case */

extern ZH_EXPORT ZH_BOOL   zh_strIsDigit( const char * szChar );
extern ZH_EXPORT ZH_BOOL   zh_strIsAlpha( const char * szChar );
extern ZH_EXPORT ZH_BOOL   zh_strIsLower( const char * szChar );
extern ZH_EXPORT ZH_BOOL   zh_strIsUpper( const char * szChar );
extern ZH_EXPORT char *    zh_strncpy( char * pDest, const char * pSource, ZH_SIZE nLen ); /* copy at most nLen bytes from string buffer to another buffer and _always_ set 0 in destination buffer */
extern ZH_EXPORT char *    zh_strncat( char * pDest, const char * pSource, ZH_SIZE nLen ); /* copy at most nLen-strlen(pDest) bytes from string buffer to another buffer and _always_ set 0 in destination buffer */
extern ZH_EXPORT char *    zh_strncpyTrim( char * pDest, const char * pSource, ZH_SIZE nLen );
extern ZH_EXPORT char *    zh_strncpyLower( char * pDest, const char * pSource, ZH_SIZE nLen ); /* copy an existing string buffer to another buffer, as lower case */
extern ZH_EXPORT char *    zh_strncpyUpper( char * pDest, const char * pSource, ZH_SIZE nLen ); /* copy an existing string buffer to another buffer, as upper case */
extern ZH_EXPORT char *    zh_strncpyUpperTrim( char * pDest, const char * pSource, ZH_SIZE nLen );
extern ZH_EXPORT const char * zh_strLTrim( const char * szText, ZH_SIZE * nLen ); /* return a pointer to the first non-white space character */
extern ZH_EXPORT ZH_SIZE   zh_strRTrimLen( const char * szText, ZH_SIZE nLen, ZH_BOOL bAnySpace ); /* return length of a string, ignoring trailing white space (or true spaces) */
extern ZH_EXPORT double    zh_strVal( const char * szText, ZH_SIZE nLen ); /* return the numeric value of a character string representation of a number */
extern ZH_EXPORT ZH_MAXINT zh_strValInt( const char * szText, int * iOverflow );
extern ZH_EXPORT char *    zh_strRemEscSeq( char * szText, ZH_SIZE * nLen );
extern ZH_EXPORT char *    zh_numToStr( char * szBuf, ZH_SIZE nSize, ZH_MAXINT nNumber );
extern ZH_EXPORT char *    zh_dblToStr( char * szBuf, ZH_SIZE nSize, double dNumber, int iMaxDec );
extern ZH_EXPORT double    zh_numRound( double dResult, int iDec ); /* round a number to a specific number of digits */
extern ZH_EXPORT double    zh_numInt( double dNum ); /* take the integer part of the number */
extern ZH_EXPORT void      zh_random_seed( ZH_I32 seed );
extern ZH_EXPORT double    zh_random_num( void );
extern ZH_EXPORT double    zh_random_num_secure( void );
extern ZH_EXPORT void      zh_random_block( void * data, ZH_SIZE len );
extern ZH_EXPORT double    zh_numDecConv( double dNum, int iDec );
extern ZH_EXPORT double    zh_numExpConv( double dNum, int iDec );
extern ZH_EXPORT void      zh_strtohex( const char * pSource, ZH_SIZE size, char * pDest );

extern ZH_EXPORT PZH_ITEM  zh_strFormat( PZH_ITEM pItemReturn, PZH_ITEM pItemFormat, int iCount, PZH_ITEM * pItemArray );

/* architecture dependent number conversions */
extern ZH_EXPORT void      zh_put_ieee754( ZH_BYTE * ptr, double d );
extern ZH_EXPORT double    zh_get_ieee754( const ZH_BYTE * ptr );
extern ZH_EXPORT void      zh_put_ord_ieee754( ZH_BYTE * ptr, double d );
extern ZH_EXPORT double    zh_get_ord_ieee754( const ZH_BYTE * ptr );
extern ZH_EXPORT double    zh_get_rev_double( const ZH_BYTE * ptr );
extern ZH_EXPORT double    zh_get_std_double( const ZH_BYTE * ptr );

#if defined( ZH_LONG_LONG_OFF )
extern ZH_EXPORT double    zh_get_le_int64( const ZH_BYTE * ptr );
extern ZH_EXPORT double    zh_get_le_uint64( const ZH_BYTE * ptr );
extern ZH_EXPORT void      zh_put_le_uint64( const ZH_BYTE * ptr, double d );
#endif

/* dynamic symbol table management */
extern ZH_EXPORT PZH_DYNSYMBOL  zh_dynsymGet( const char * szName );    /* finds and creates a dynamic symbol if not found */
extern ZH_EXPORT PZH_DYNSYMBOL  zh_dynsymGetCase( const char * szName );    /* finds and creates a dynamic symbol if not found - case sensitive */
extern ZH_EXPORT PZH_DYNSYMBOL  zh_dynsymNew( PZH_SYMBOL pSymbol ); /* creates a new dynamic symbol based on a local one */
extern ZH_EXPORT PZH_DYNSYMBOL  zh_dynsymFind( const char * szName );   /* finds a dynamic symbol */
extern ZH_EXPORT PZH_DYNSYMBOL  zh_dynsymFindName( const char * szName ); /* converts to uppercase and finds a dynamic symbol */
extern ZH_EXPORT void      zh_dynsymRelease( void );         /* releases the memory of the dynamic symbol table */
extern ZH_EXPORT void      zh_dynsymEval( PZH_DYNS_FUNC pFunction, void * Cargo ); /* enumerates all dynamic symbols */
extern ZH_EXPORT void      zh_dynsymProtectEval( PZH_DYNS_FUNC pFunction, void * Cargo ); /* enumerates all dynamic symbols with global symbol table locked - can be used ONLY when user function does not try to access dynamic symbol table */
extern ZH_EXPORT PZH_SYMBOL  zh_dynsymGetSymbol( const char * szName ); /* finds and creates a dynamic symbol if not found and return pointer to its ZH_SYMBOL structure */
extern ZH_EXPORT PZH_SYMBOL  zh_dynsymFindSymbol( const char * szName ); /* finds a dynamic symbol and return pointer to its ZH_SYMBOL structure */
extern ZH_EXPORT PZH_SYMBOL  zh_dynsymSymbol( PZH_DYNSYMBOL pDynSym );
extern ZH_EXPORT const char * zh_dynsymName( PZH_DYNSYMBOL pDynSym ); /* return dynamic symbol name */
extern ZH_EXPORT ZH_BOOL   zh_dynsymIsFunction( PZH_DYNSYMBOL pDynSym );
extern ZH_EXPORT ZH_BOOL   zh_dynsymIsMemvar( PZH_DYNSYMBOL pDynSym );
extern ZH_EXPORT int       zh_dynsymAreaHandle( PZH_DYNSYMBOL pDynSym ); /* return work area number bound with given dynamic symbol */
extern ZH_EXPORT void      zh_dynsymSetAreaHandle( PZH_DYNSYMBOL pDynSym, int iArea ); /* set work area number for a given dynamic symbol */
extern ZH_EXPORT int       zh_dynsymToNum( PZH_DYNSYMBOL pDynSym );
extern ZH_EXPORT PZH_DYNSYMBOL  zh_dynsymFromNum( int iSymNum );
#ifdef _ZH_API_INTERNAL_
extern           PZH_ITEM  zh_dynsymGetMemvar( PZH_DYNSYMBOL pDynSym ); /* return memvar handle number bound with given dynamic symbol */
extern           void      zh_dynsymSetMemvar( PZH_DYNSYMBOL pDynSym, PZH_ITEM pMemvar ); /* set memvar handle for a given dynamic symbol */
extern           ZH_LONG   zh_dynsymCount( void ); /* number of dynamic symbols */
#endif

/* Symbol management */
extern ZH_EXPORT PZH_SYMBOL  zh_symbolNew( const char * szName ); /* create a new symbol */

/* Command-line and environment argument management */
extern ZH_EXPORT void          zh_cmdargInit( int argc, char * argv[] ); /* initialize command-line argument API's */
extern ZH_EXPORT int           zh_cmdargARGC( void ); /* retrieve command-line argument count */
extern ZH_EXPORT char **       zh_cmdargARGV( void ); /* retrieve command-line argument buffer pointer */
extern ZH_EXPORT const char *  zh_cmdargARGVN( int argc ); /* retrieve given command-line argument */
extern ZH_EXPORT ZH_BOOL       zh_cmdargIsInternal( const char * szArg, int * piLen ); /* determine if a string is an internal setting */
extern ZH_EXPORT char *        zh_cmdargProgName( void ); /* return application name with path or NULL if not set, caller must free returned value with zh_xfree() if not NULL */
extern ZH_EXPORT char *        zh_cmdargBaseProgName( void ); /* return application name without path or NULL if not set, caller must free returned value with zh_xfree() if not NULL */
extern           int           zh_cmdargPushArgs( void ); /* places application parameters on the ZHVM stack */
extern           void          zh_cmdargUpdate( void ); /* update arguments after ZHVM initialization */
extern           ZH_BOOL       zh_cmdargCheck( const char * pszName ); /* Check if a given internal switch (like //INFO) was set */
extern           char *        zh_cmdargString( const char * pszName ); /* Returns the string value of an internal switch (like //GT:cgi) */
extern           int           zh_cmdargNum( const char * pszName ); /* Returns the numeric value of an internal switch (like //F:90) */
extern           void          zh_cmdargProcess( void ); /* Check for command-line internal arguments */
#if defined( ZH_OS_WIN )
extern ZH_EXPORT void          zh_winmainArgInit( void * hInstance, void * hPrevInstance, int iCmdShow ); /* Set WinMain() parameters */
extern ZH_EXPORT ZH_BOOL       zh_winmainArgGet( void * phInstance, void * phPrevInstance, int * piCmdShow ); /* Retrieve WinMain() parameters */
extern ZH_EXPORT void          zh_winmainArgVBuild( void );
extern ZH_EXPORT void          zh_winmainArgVFree( void );
#endif

/* Codeblock management */
extern ZH_EXPORT void *       zh_codeblockId( PZH_ITEM pItem ); /* retrieves the codeblock unique ID */
extern ZH_EXPORT ZH_COUNTER   zh_codeblockRefs( PZH_ITEM pItem ); /* retrieves number of references to the codeblock */
extern PZH_CODEBLOCK    zh_codeblockNew( const ZH_BYTE * pBuffer, ZH_USHORT uiLocals, const ZH_BYTE * pLocalPosTable, PZH_SYMBOL pSymbols, ZH_SIZE nLen ); /* create a code-block */
extern PZH_CODEBLOCK    zh_codeblockMacroNew( const ZH_BYTE * pBuffer, ZH_SIZE nLen );
extern PZH_ITEM         zh_codeblockGetVar( PZH_ITEM pItem, int iItemPos ); /* get local variable referenced in a codeblock */
extern PZH_ITEM         zh_codeblockGetRef( PZH_CODEBLOCK pCBlock, int iItemPos ); /* get local variable passed by reference */

/* memvars subsystem */
extern           void       zh_memvarsClear( ZH_BOOL fAll ); /* clear all PUBLIC and PRIVATE variables optionally without GetList PUBLIC variable */
extern ZH_EXPORT void       zh_memvarSetValue( PZH_SYMBOL pMemvarSymb, PZH_ITEM pItem ); /* copy an item into a symbol */
extern ZH_EXPORT ZH_ERRCODE zh_memvarGet( PZH_ITEM pItem, PZH_SYMBOL pMemvarSymb ); /* copy an symbol value into an item */
extern           void       zh_memvarGetValue( PZH_ITEM pItem, PZH_SYMBOL pMemvarSymb ); /* copy an symbol value into an item, with error trapping */
extern           void       zh_memvarGetRefer( PZH_ITEM pItem, PZH_SYMBOL pMemvarSymb ); /* copy a reference to a symbol value into an item, with error trapping */
extern           ZH_SIZE    zh_memvarGetPrivatesBase( void ); /* retrieve current PRIVATE variables stack base */
extern           void       zh_memvarSetPrivatesBase( ZH_SIZE nBase ); /* release PRIVATE variables created after specified base */
extern           void       zh_memvarUpdatePrivatesBase( void ); /* Update PRIVATE base offset so they will not be removed when function return */
extern           void       zh_memvarNewParameter( PZH_SYMBOL pSymbol, PZH_ITEM pValue );
extern           char *     zh_memvarGetStrValuePtr( char * szVarName, ZH_SIZE * pnLen );
extern           void       zh_memvarCreateFromItem( PZH_ITEM pMemvar, int iScope, PZH_ITEM pValue );
extern           int        zh_memvarScope( const char * szVarName, ZH_SIZE nLength ); /* retrieve scope of a dynamic variable symbol */
extern           PZH_ITEM   zh_memvarDetachLocal( PZH_ITEM pLocal ); /* Detach a local variable from the eval stack */
extern ZH_EXPORT PZH_ITEM   zh_memvarGetValueBySym( PZH_DYNSYMBOL pDynSym );
extern ZH_EXPORT PZH_ITEM   zh_memvarSaveInArray( int iScope, ZH_BOOL fCopy ); /* create array with visible memvar references or copies respecting given memvars scope */
extern           void       zh_memvarRestoreFromArray( PZH_ITEM pArray );

#ifdef _ZH_API_INTERNAL_
extern void       zh_memvarValueIncRef( PZH_ITEM pValue ); /* increase the reference count of a global value */
extern void       zh_memvarValueDecRef( PZH_ITEM pValue ); /* decrease the reference count of a global value */
extern PZH_ITEM   zh_memvarGetItem( PZH_SYMBOL pMemvarSymb );
#if defined( _ZH_API_MACROS_ )
#  define zh_memvarValueIncRef( p )       zh_xRefInc( p )
#endif /* _ZH_API_MACROS_ */
#endif /* _ZH_API_INTERNAL_ */

/* console I/O subsystem */
extern void zh_conInit( void );     /* initialize the console API system */
extern ZH_EXPORT void zh_conRelease( void );  /* release the console API system */
extern ZH_EXPORT const char * zh_conNewLine( void ); /* retrieve a pointer to a static buffer containing new-line characters */
extern ZH_EXPORT void         zh_conOutStd( const char * pStr, ZH_SIZE nLen ); /* output an string to STDOUT */
extern ZH_EXPORT void         zh_conOutErr( const char * pStr, ZH_SIZE nLen ); /* output an string to STDERR */
extern ZH_EXPORT void         zh_conOutAlt( const char * pStr, ZH_SIZE nLen ); /* output an string to the screen and/or printer and/or alternate */
extern ZH_EXPORT int          zh_conSetCursor( ZH_BOOL bSetCursor, int iNewCursor ); /* retrieve and optionally set cursor shape */
extern ZH_EXPORT const char * zh_conSetColor( const char * szColor ); /* retrieve and optionally set console color */

/* compiler and macro compiler */
extern ZH_EXPORT_INT char *       zh_compEncodeString( int iMethod, const char * szText, ZH_SIZE * pnLen );
extern               char *       zh_compDecodeString( int iMethod, const char * szText, ZH_SIZE * pnLen );

/* misc */
extern ZH_EXPORT char *   zh_procname( int iLevel, char * szName, ZH_BOOL bskipBlock ); /* retrieve a procedure name into a buffer */
extern ZH_EXPORT ZH_BOOL  zh_procinfo( int iLevel, char * szName, ZH_USHORT * puiLine, char * szFile );

/* macro compiler */
#if defined( ZH_MACRO_SUPPORT )
struct ZH_MACRO_;
typedef struct ZH_MACRO_ * PZH_MACRO;
#else
typedef void * PZH_MACRO;
#endif
extern ZH_EXPORT void         zh_macroGetValue( PZH_ITEM pItem, int iContext, int flags ); /* retrieve results of a macro expansion */
extern           void         zh_macroSetValue( PZH_ITEM pItem, int flags ); /* assign a value to a macro-expression item */
extern           void         zh_macroPushReference( PZH_ITEM pItem ); /* push reference to given expression */
extern ZH_EXPORT void         zh_macroTextValue( PZH_ITEM pItem ); /* macro text substitution */
extern           void         zh_macroPushSymbol( PZH_ITEM pItem ); /* handle a macro function calls, e.g. var := &macro() */
extern           void         zh_macroRun( PZH_MACRO pMacro ); /* executes pcode compiled by macro compiler */
extern           PZH_MACRO    zh_macroCompile( const char * szString ); /* compile a string and return a pcode buffer */
extern           void         zh_macroDelete( PZH_MACRO pMacro ); /* release all memory allocated for macro evaluation */
extern           char *       zh_macroTextSymbol( const char * szString, ZH_SIZE nLength, ZH_BOOL * pfNewString ); /* substitute macro variables occurrences within a given string and check if result is a valid function or variable name */
extern           char *       zh_macroExpandString( const char * szString, ZH_SIZE nLength, ZH_BOOL * pfNewString ); /* expands valid '&' operator */
extern           void         zh_macroPopAliasedValue( PZH_ITEM pAlias, PZH_ITEM pVar, int flags ); /* compiles and evaluates an aliased macro expression */
extern           void         zh_macroPushAliasedValue( PZH_ITEM pAlias, PZH_ITEM pVar, int flags ); /* compiles and evaluates an aliased macro expression */
extern ZH_EXPORT const char * zh_macroGetType( PZH_ITEM pItem ); /* determine the type of an expression */

/* idle states */
extern ZH_EXPORT void zh_releaseCPU( void );
extern ZH_EXPORT void zh_idleState( void ); /* services a single idle state */
extern ZH_EXPORT void zh_idleReset( void ); /* services a single idle state */
extern ZH_EXPORT void zh_idleSleep( double dSeconds ); /* sleep for a given time serving idle task */

/* I18N public API */
extern PZH_ITEM zh_i18n_ngettext( PZH_ITEM pNum, PZH_ITEM pMsgID, PZH_ITEM pContext );
extern PZH_ITEM zh_i18n_gettext( PZH_ITEM pMsgID, PZH_ITEM pContext );
/* I18N internal ZHVM API */
#if defined( _ZH_API_INTERNAL_ ) || defined( _ZH_I18N_INTERNAL_ )
extern void *   zh_vmI18N( void );
extern void     zh_vmSetI18N( void * );
extern void     zh_i18n_init( void );
extern void     zh_i18n_exit( void );
extern void     zh_i18n_release( void * cargo );
extern void *   zh_i18n_alloc( void * cargo );
#endif /* _ZH_API_INTERNAL_ || _ZH_I18N_INTERNAL_ */

extern ZH_EXPORT void         zh_vmSetLinkedMain( const char * szMain );
extern ZH_EXPORT void         zh_vmSetDefaultGT( const char * szGtName );
extern ZH_EXPORT ZH_BOOL      zh_vmInternalsEnabled( void );

extern ZH_EXPORT PZH_FUNC     zh_vmProcAddress( const char * szFuncName );

extern ZH_EXPORT PZH_ITEM     zh_libLoad( PZH_ITEM pLibName, PZH_ITEM pArgs );
extern ZH_EXPORT ZH_BOOL      zh_libFree( PZH_ITEM pDynLib );
extern ZH_EXPORT void *       zh_libHandle( PZH_ITEM pDynLib );
extern ZH_EXPORT void *       zh_libSymAddr( PZH_ITEM pDynLib, const char * pszSymbol );

extern ZH_EXPORT void         zh_dynCall( int iFuncFlags, void * pFunction, int iParams, int iFirst, int * piArgFlags );

/* misc */
typedef void ( * PZH_OUT_FUNC )( const char *, ZH_SIZE );

extern ZH_EXPORT const char * zh_verHostCPU( void );         /* retrieves a constant string with host OS CPU architecture */
extern ZH_EXPORT int          zh_verHostBitWidth( void );    /* retrieves bit width of host OS */
extern ZH_EXPORT const char * zh_verCPU( void );             /* retrieves a constant string with CPU architecture */
extern ZH_EXPORT const char * zh_verPlatformMacro( void );   /* retrieves a constant string with OS platform (as it appears in __PLATFORM__* macro) */
extern ZH_EXPORT char *       zh_verPlatform( void );        /* retrieves a newly allocated buffer containing platform version */
extern ZH_EXPORT char *       zh_verCompiler( void );        /* retrieves a newly allocated buffer containing compiler version */
extern ZH_EXPORT char *       zh_verZiher( void );         /* retrieves a newly allocated buffer containing Ziher version */
extern ZH_EXPORT char *       zh_verPCode( void );           /* retrieves a newly allocated buffer containing PCode version */
extern ZH_EXPORT void         zh_verBuildInfo( void );       /* display Ziher, compiler and platform versions to stderr console (deprecated) */
extern ZH_EXPORT void         zh_verBuildInfoCB( PZH_OUT_FUNC );  /* pass Ziher, compiler and platform versions to callback function */
extern ZH_EXPORT const char * zh_verCommitID( void );        /* retrieves a static buffer containing source repository hash/id */
extern ZH_EXPORT const char * zh_verCommitIDShort( void );   /* retrieves a static buffer containing source repository hash/id (short version) */
extern ZH_EXPORT int          zh_verCommitRev( void );       /* retrieves source repository revision number */
extern ZH_EXPORT const char * zh_verCommitInfo( void );      /* retrieves a static buffer containing source repository last commit header */
extern ZH_EXPORT const char * zh_verFlagsC( void );          /* retrieves a static buffer containing build time C compiler flags in ZH_USER_CFLAGS envvar */
extern ZH_EXPORT const char * zh_verFlagsL( void );          /* retrieves a static buffer containing build time linker flags in ZH_USER_LDFLAGS envvar */
extern ZH_EXPORT const char * zh_verFlagsPRG( void );        /* retrieves a static buffer containing build time Ziher compiler flags in ZH_USER_PRGFLAGS envvar */

extern ZH_EXPORT int     zh_iswine( void );     /* return non-zero if OS == Wine */
extern ZH_EXPORT int     zh_iswin9x( void );    /* return non-zero if OS == Windows 9x, ME */
extern ZH_EXPORT int     zh_iswinnt( void );    /* return non-zero if OS == Windows NT or newer */
extern ZH_EXPORT ZH_BOOL zh_iswin2k( void );    /* return ZH_TRUE if OS == Windows 2000 or newer */
extern ZH_EXPORT ZH_BOOL zh_iswin2k3( void );   /* return ZH_TRUE if OS == Windows 2003 Server or newer */
extern ZH_EXPORT ZH_BOOL zh_iswinvista( void ); /* return ZH_TRUE if OS == Windows Vista or newer */
extern ZH_EXPORT ZH_BOOL zh_iswin7( void );     /* return ZH_TRUE if OS == Windows 7 or newer */
extern ZH_EXPORT ZH_BOOL zh_iswin8( void );     /* return ZH_TRUE if OS == Windows 8 or newer */
extern ZH_EXPORT ZH_BOOL zh_iswin81( void );    /* return ZH_TRUE if OS == Windows 8.1 or newer */
extern ZH_EXPORT ZH_BOOL zh_iswin10( void );    /* return ZH_TRUE if OS == Windows 10 or newer */
extern ZH_EXPORT ZH_BOOL zh_iswinver( int iMajor, int iMinor, int iType, ZH_BOOL fOrUpper );
extern ZH_EXPORT ZH_BOOL zh_iswinsp( int iServicePackMajor, ZH_BOOL fOrUpper );

extern ZH_EXPORT ZH_BOOL zh_printerIsReady( const char * pszPrinterName );

/* OS/Ziher codepage conversion */
extern ZH_EXPORT ZH_BOOL      zh_osUseCP( void ); /* Is OS<->Ziher codepage conversion enabled? */
extern ZH_EXPORT const char * zh_osEncodeCP( const char * szName, char ** pszFree, ZH_SIZE * pnSize ); /* Convert a string sent to a system call, from Ziher codepage. */
extern ZH_EXPORT const char * zh_osDecodeCP( const char * szName, char ** pszFree, ZH_SIZE * pnSize ); /* Convert a string received from a system call, to Ziher codepage. */

extern ZH_EXPORT char *       zh_osStrEncode( const char * pszName );
extern ZH_EXPORT char *       zh_osStrEncodeN( const char * pszName, ZH_SIZE nLen );
extern ZH_EXPORT char *       zh_osStrDecode( const char * pszName );
extern ZH_EXPORT char *       zh_osStrDecode2( const char * pszName, char * pszBuffer, ZH_SIZE nSize );
#if defined( ZH_OS_WIN )
extern ZH_EXPORT ZH_WCHAR *   zh_osStrU16Encode( const char * pszName );
extern ZH_EXPORT ZH_WCHAR *   zh_osStrU16EncodeN( const char * pszName, ZH_SIZE nLen );
extern ZH_EXPORT char *       zh_osStrU16Decode( const ZH_WCHAR * pszNameW );
extern ZH_EXPORT char *       zh_osStrU16Decode2( const ZH_WCHAR * pszNameW, char * pszBuffer, ZH_SIZE nSize );
#endif

/* environment variables access */
extern ZH_EXPORT ZH_BOOL zh_getenv_buffer( const char * szName, char * szBuffer, int nSize );
/* WARNING: This returned pointer must be freed if not NULL using zh_xfree( ptr ); */
extern ZH_EXPORT char *  zh_getenv( const char * name );
extern ZH_EXPORT ZH_BOOL zh_setenv( const char * szName, const char * szValue ); /* set or delete (szValue==NULL) environment variable */
extern ZH_EXPORT char *  zh_netname( void );
extern ZH_EXPORT char *  zh_username( void );

/* Translation related things */

/* Dummy define for start */
#define ZH_I_( x ) x

ZH_EXTERN_END

#if defined( ZH_MACRO_SUPPORT )
#include "zh_compdf.h"
#endif

#endif /* ZH_APIEXT_H_ */
