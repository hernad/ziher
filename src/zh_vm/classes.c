/*
 * Base-routines for OOPS system
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * Copyright 1999 Eddie Runia <eddie@runia.com>
 *    :CLASSSEL()
 *    __clsDelMsg()
 *    __clsModMsg()
 *    __clsInstSuper()
 *    __cls_CntClsData()
 *    __cls_CntData()
 *    __cls_DecData()
 *    __cls_IncData()
 *    __objClone()
 *    __objHasMsg()
 *    __objSendMsg()
 *
 * Copyright 1999-2001 Viktor Szakats
 *    __classNew()
 *    __classInstance()
 *    __classAdd()
 *    __className()
 *    __classSel() (based on zh___msgClsSel())
 *
 * Copyright 1999 Janica Lubos <janica@fornax.elf.stuba.sk>
 *    zh_clsDictRealloc()
 *
 * Copyright 2000-2007 JF. Lefebvre <jfl@mafact.com> & RA. Cuylen <cakiral@altern.org
 *    Multiple inheritance fully implemented
 *    Forwarding, delegating
 *    Data initialization & Autoinit for Bool and Numeric
 *    Scoping: PROTECTED / EXPORTED
 *
 * Copyright 2008- JF. Lefebvre <jfl@mafact.com>
 *    zh_clsDictRealloc()   New version
 *    Now support of shared and not shared class data
 *    Multiple data declarations fully supported
 *
 * Copyright 2000 Ryszard Glab <rglab@imid.med.pl>
 *    Garbage collector fixes
 *
 * Copyright 2001 JF. Lefebvre <jfl@mafact.com>
 *    Super msg corrected
 *    Scoping: working for PROTECTED, HIDDEN and READONLY
 *    To Many enhancement and correction to give a full list :-)
 *    Improved Class(y) compatibility
 *    Improved TopClass compatibility
 *    __CLS_PAR00() (Allow the creation of class which not autoinherit of the default ZHObject())
 *    Adding ZH_CLS_ENFORCERO FLAG to disable Write access to RO VAR
 *    outside of Constructors /!\ Could be related to some incompatibility
 *    Added zh_objGetRealClsName() to keep a full class tree (for 99% cases)
 *    Fixed zh_clsIsParent()
 *    zh_objGetMthd() & __clsAddMsg() modified to translate operators
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

#include "zh_vm_opt.h"
#include "zh_api.h"
#include "zh_class_api.h"
#include "zh_stack.h"
#include "zh_error_api.h"
#include "zh_item_api.h"
#include "zh_vm.h"
#include "zh_thread.h"
#include "oo.zhh"

typedef struct
{
   ZH_USHORT   uiClass;
   ZH_USHORT   uiOffset;
} ZH_CLSCAST, * PZH_CLSCAST;

typedef struct
{
   PZH_ITEM    pInitValue;       /* Init Value for data */
   ZH_USHORT   uiType;           /* ZH_OO_MSG_DATA, ZH_OO_MSG_CLASSDATA or ZH_OO_MSG_INITIALIZED */
   ZH_USHORT   uiData;           /* Item position in instance area or class data */
   ZH_USHORT   uiOffset;         /* Super cast instance area offset for ZH_OO_MSG_DATA or real class item position */
   ZH_USHORT   uiSprClass;       /* The real class where method were defined */
} INITDATA, * PINITDATA;

typedef struct
{
   PZH_DYNS    pMessage;         /* Method symbolic name */
   PZH_DYNS    pAccMsg;          /* Corresponding access method symbolic name */
   PZH_SYMB    pFuncSym;         /* Function symbol */
   PZH_SYMB    pRealSym;         /* Real function symbol when wrapper is used */
   ZH_TYPE     itemType;         /* Type of item in restricted assignment */
   ZH_USHORT   uiSprClass;       /* Original class handle (super or current class handle if not inherited). [RAC&JF] */
   ZH_USHORT   uiScope;          /* Scoping value */
   ZH_USHORT   uiData;           /* Item position for instance data, class data and shared data (Ziher like, begin from 1), supercast class or delegated message index object */
   ZH_USHORT   uiOffset;         /* position in pInitData for class datas (from 1) or offset to instance area in inherited instance data and supercast messages (from 0) */
   ZH_USHORT   uiPrevCls;
   ZH_USHORT   uiPrevMth;
#ifndef ZH_NO_PROFILER
   ZH_ULONG    ulCalls;          /* profiler support */
   ZH_ULONG    ulTime;           /* profiler support */
   ZH_ULONG    ulRecurse;        /* profiler support */
#endif
} METHOD, * PMETHOD;


#define ZH_MSG_POOL
typedef struct
{
   char *      szName;           /* Class name */
   PZH_DYNS    pClassSym;        /* Class symbolic name */
   PMETHOD     pMethods;         /* Class methods */
   PZH_SYMB    pClassFuncSym;    /* Class function symbol */
   PZH_SYMB    pFriendModule;    /* Class friend symbols */
   PINITDATA   pInitData;        /* Class/instance Initialization data */
   PZH_ITEM    pClassDatas;      /* Ziher Array for Class Datas */
   PZH_ITEM    pSharedDatas;     /* Ziher Array for Class Shared Datas */
   PZH_ITEM    pInlines;         /* Array for inline codeblocks */
   PZH_ITEM    pMutex;           /* Class sync method mutex */
   PZH_SYMB *  pFriendSyms;      /* Friend functions' symbols */
   PZH_CLSCAST pSuperClasses;    /* Super classes */
   ZH_U32      nOpFlags;         /* Flags for overloaded operators */
   ZH_USHORT   uiClass;          /* This class handle */
   ZH_USHORT   fHasDestructor;   /* has the class destructor message? */
   ZH_USHORT   fHasOnError;      /* has the class OnError message? */
   ZH_USHORT   fLocked;          /* Class is locked against modifications */
   ZH_USHORT   uiMethods;        /* Total Method initialized Counter */
   ZH_USHORT   uiInitDatas;      /* Total Method initialized Counter */
   ZH_USHORT   uiDatas;          /* Total Data Counter */
   ZH_USHORT   uiDataFirst;      /* First instance item from this class */
   ZH_USHORT   uiSuperClasses;   /* Number of super classes */
   ZH_USHORT   uiFriendSyms;     /* Number of friend function's symbols */
   ZH_USHORT   uiFriendModule;   /* Number of friend symbols in pFriendModule */
   ZH_USHORT   uiMutexOffset;    /* Offset in instance area to SYNC method mutex */
   ZH_USHORT   uiHashKey;
#ifdef ZH_MSG_POOL
   ZH_USHORT * puiMsgIdx;
   ZH_USHORT   uiMethodCount;
#endif
} CLASS, * PCLASS;

#define BUCKETBITS      2
#define BUCKETSIZE      ( 1 << BUCKETBITS )
#define BUCKETMASK      ( BUCKETSIZE - 1 )
#define HASHBITS        3
#define HASH_KEY        ( ( 1 << HASHBITS ) - 1 )
#define HASH_KEYMAX     ( 1 << ( 16 - BUCKETBITS ) )
#define zh_clsInited(p) ( (p)->pMethods != NULL )
#define zh_clsBucketPos( p, m )     ( ( (p)->uiSymNum & (m) ) << BUCKETBITS )

#ifdef ZH_MSG_POOL
#  define zh_clsMthNum( p )      ( ( ZH_SIZE ) ( p )->uiMethodCount )
#else
#  define zh_clsMthNum( p )      ( ( ( ZH_SIZE ) ( p )->uiHashKey + 1 ) << BUCKETBITS )
#endif

#if defined( ZH_REAL_BLOCK_SCOPE )
#  undef ZH_CLASSY_BLOCK_SCOPE
#elif ! defined( ZH_CLASSY_BLOCK_SCOPE )
#  define ZH_REAL_BLOCK_SCOPE
#endif

#if ! defined( ZH_CLASSY_BLOCK_SCOPE )
#  define zh_clsSenderOffset()  zh_stackBaseProcOffset( 1 )
#endif


ZH_FUNC_STATIC( msgGetData );
ZH_FUNC_STATIC( msgSetData );
ZH_FUNC_STATIC( msgGetClsData );
ZH_FUNC_STATIC( msgSetClsData );
ZH_FUNC_STATIC( msgGetShrData );
ZH_FUNC_STATIC( msgSetShrData );
ZH_FUNC_STATIC( msgEvalInline );
ZH_FUNC_STATIC( msgVirtual );
ZH_FUNC_STATIC( msgSuper );
ZH_FUNC_STATIC( msgRealClass );
ZH_FUNC_STATIC( msgPerform );
ZH_FUNC_STATIC( msgDelegate );
ZH_FUNC_STATIC( msgSync );
ZH_FUNC_STATIC( msgSyncClass );
ZH_FUNC_STATIC( msgNoMethod );
ZH_FUNC_STATIC( msgScopeErr );
ZH_FUNC_STATIC( msgTypeErr );
ZH_FUNC_STATIC( msgNull );

ZH_FUNC_STATIC( msgClassH );
ZH_FUNC_STATIC( msgClassName );
ZH_FUNC_STATIC( msgClassSel );
#if 0
ZH_FUNC_STATIC( msgClass );
ZH_FUNC_STATIC( msgClassParent );
#endif

/* --- */

/* static variables and structures initialized at ZHVM startup which
 * do not need any synchronization mechanism in MT mode, [druzus]
 */

/* The positions of items in symbol table below have to correspond
 * to ZH_OO_OP_* constants in zh_class_api.h, [druzus]
 */
static ZH_SYMB s_opSymbols[ ZH_OO_MAX_OPERATOR + 1 ] = {
   { "__OPPLUS",              {ZH_FS_MESSAGE}, {NULL}, NULL },  /* 00 */
   { "__OPMINUS",             {ZH_FS_MESSAGE}, {NULL}, NULL },  /* 01 */
   { "__OPMULT",              {ZH_FS_MESSAGE}, {NULL}, NULL },  /* 02 */
   { "__OPDIVIDE",            {ZH_FS_MESSAGE}, {NULL}, NULL },  /* 03 */
   { "__OPMOD",               {ZH_FS_MESSAGE}, {NULL}, NULL },  /* 04 */
   { "__OPPOWER",             {ZH_FS_MESSAGE}, {NULL}, NULL },  /* 05 */
   { "__OPINC",               {ZH_FS_MESSAGE}, {NULL}, NULL },  /* 06 */
   { "__OPDEC",               {ZH_FS_MESSAGE}, {NULL}, NULL },  /* 07 */
   { "__OPEQUAL",             {ZH_FS_MESSAGE}, {NULL}, NULL },  /* 08 */
   { "__OPEXACTEQUAL",        {ZH_FS_MESSAGE}, {NULL}, NULL },  /* 09 */
   { "__OPNOTEQUAL",          {ZH_FS_MESSAGE}, {NULL}, NULL },  /* 10 */
   { "__OPLESS",              {ZH_FS_MESSAGE}, {NULL}, NULL },  /* 11 */
   { "__OPLESSEQUAL",         {ZH_FS_MESSAGE}, {NULL}, NULL },  /* 12 */
   { "__OPGREATER",           {ZH_FS_MESSAGE}, {NULL}, NULL },  /* 13 */
   { "__OPGREATEREQUAL",      {ZH_FS_MESSAGE}, {NULL}, NULL },  /* 14 */
   { "__OPASSIGN",            {ZH_FS_MESSAGE}, {NULL}, NULL },  /* 15 */
   { "__OPINSTRING",          {ZH_FS_MESSAGE}, {NULL}, NULL },  /* 16 */
   { "__OPINCLUDE",           {ZH_FS_MESSAGE}, {NULL}, NULL },  /* 17 */
   { "__OPNOT",               {ZH_FS_MESSAGE}, {NULL}, NULL },  /* 18 */
   { "__OPAND",               {ZH_FS_MESSAGE}, {NULL}, NULL },  /* 19 */
   { "__OPOR",                {ZH_FS_MESSAGE}, {NULL}, NULL },  /* 20 */
   { "__OPARRAYINDEX",        {ZH_FS_MESSAGE}, {NULL}, NULL },  /* 21 */
   { "__ENUMINDEX",           {ZH_FS_MESSAGE}, {NULL}, NULL },  /* 22 */
   { "__ENUMBASE",            {ZH_FS_MESSAGE}, {NULL}, NULL },  /* 23 */
   { "__ENUMVALUE",           {ZH_FS_MESSAGE}, {NULL}, NULL },  /* 24 */
   { "__ENUMSTART",           {ZH_FS_MESSAGE}, {NULL}, NULL },  /* 25 */
   { "__ENUMSKIP",            {ZH_FS_MESSAGE}, {NULL}, NULL },  /* 26 */
   { "__ENUMSTOP",            {ZH_FS_MESSAGE}, {NULL}, NULL },  /* 27 */
   { "__ENUMISFIRST",         {ZH_FS_MESSAGE}, {NULL}, NULL },  /* 28 */
   { "__ENUMISLAST",          {ZH_FS_MESSAGE}, {NULL}, NULL },  /* 29 */
};

static ZH_SYMB s___msgDestructor  = { "__msgDestructor", {ZH_FS_MESSAGE}, {NULL},               NULL };
static ZH_SYMB s___msgOnError     = { "__msgOnError",    {ZH_FS_MESSAGE}, {NULL},               NULL };

static ZH_SYMB s___msgSetData     = { "__msgSetData",    {ZH_FS_MESSAGE}, {ZH_FUNCNAME( msgSetData )},    NULL };
static ZH_SYMB s___msgGetData     = { "__msgGetData",    {ZH_FS_MESSAGE}, {ZH_FUNCNAME( msgGetData )},    NULL };
static ZH_SYMB s___msgSetClsData  = { "__msgSetClsData", {ZH_FS_MESSAGE}, {ZH_FUNCNAME( msgSetClsData )}, NULL };
static ZH_SYMB s___msgGetClsData  = { "__msgGetClsData", {ZH_FS_MESSAGE}, {ZH_FUNCNAME( msgGetClsData )}, NULL };
static ZH_SYMB s___msgSetShrData  = { "__msgSetShrData", {ZH_FS_MESSAGE}, {ZH_FUNCNAME( msgSetShrData )}, NULL };
static ZH_SYMB s___msgGetShrData  = { "__msgGetShrData", {ZH_FS_MESSAGE}, {ZH_FUNCNAME( msgGetShrData )}, NULL };
static ZH_SYMB s___msgEvalInline  = { "__msgEvalInline", {ZH_FS_MESSAGE}, {ZH_FUNCNAME( msgEvalInline )}, NULL };
static ZH_SYMB s___msgVirtual     = { "__msgVirtual",    {ZH_FS_MESSAGE}, {ZH_FUNCNAME( msgVirtual )},    NULL };
static ZH_SYMB s___msgSuper       = { "__msgSuper",      {ZH_FS_MESSAGE}, {ZH_FUNCNAME( msgSuper )},      NULL };
static ZH_SYMB s___msgRealClass   = { "__msgRealClass",  {ZH_FS_MESSAGE}, {ZH_FUNCNAME( msgRealClass )},  NULL };
static ZH_SYMB s___msgPerform     = { "__msgPerform",    {ZH_FS_MESSAGE}, {ZH_FUNCNAME( msgPerform )},    NULL };
static ZH_SYMB s___msgDelegate    = { "__msgDelegate",   {ZH_FS_MESSAGE}, {ZH_FUNCNAME( msgDelegate )},   NULL };
static ZH_SYMB s___msgSync        = { "__msgSync",       {ZH_FS_MESSAGE}, {ZH_FUNCNAME( msgSync )},       NULL };
static ZH_SYMB s___msgSyncClass   = { "__msgSyncClass",  {ZH_FS_MESSAGE}, {ZH_FUNCNAME( msgSyncClass )},  NULL };
static ZH_SYMB s___msgNoMethod    = { "__msgNoMethod",   {ZH_FS_MESSAGE}, {ZH_FUNCNAME( msgNoMethod )},   NULL };
static ZH_SYMB s___msgScopeErr    = { "__msgScopeErr",   {ZH_FS_MESSAGE}, {ZH_FUNCNAME( msgScopeErr )},   NULL };
static ZH_SYMB s___msgTypeErr     = { "__msgTypeErr",    {ZH_FS_MESSAGE}, {ZH_FUNCNAME( msgTypeErr )},    NULL };

static ZH_SYMB s___msgNew         = { "NEW",             {ZH_FS_MESSAGE}, {NULL},               NULL };
static ZH_SYMB s___msgSymbol      = { "SYMBOL",          {ZH_FS_MESSAGE}, {NULL},               NULL };

static ZH_SYMB s___msgClassName   = { "CLASSNAME",       {ZH_FS_MESSAGE}, {ZH_FUNCNAME( msgClassName )},  NULL };
static ZH_SYMB s___msgClassH      = { "CLASSH",          {ZH_FS_MESSAGE}, {ZH_FUNCNAME( msgClassH )},     NULL };
static ZH_SYMB s___msgClassSel    = { "CLASSSEL",        {ZH_FS_MESSAGE}, {ZH_FUNCNAME( msgClassSel )},   NULL };
static ZH_SYMB s___msgExec        = { "EXEC",            {ZH_FS_MESSAGE}, {ZH_FUNCNAME( msgNull )},       NULL };
static ZH_SYMB s___msgName        = { "NAME",            {ZH_FS_MESSAGE}, {ZH_FUNCNAME( msgNull )},       NULL };
#if 0
static ZH_SYMB s___msgClsParent   = { "ISDERIVEDFROM",   {ZH_FS_MESSAGE}, {ZH_FUNCNAME( msgClassParent )},NULL };
static ZH_SYMB s___msgClass       = { "CLASS",           {ZH_FS_MESSAGE}, {ZH_FUNCNAME( msgClass )},      NULL };
#endif
static ZH_SYMB s___msgKeys        = { "KEYS",            {ZH_FS_MESSAGE}, {ZH_FUNCNAME( msgNull )},       NULL };
static ZH_SYMB s___msgValues      = { "VALUES",          {ZH_FS_MESSAGE}, {ZH_FUNCNAME( msgNull )},       NULL };

/* Default enumerator methods (FOR EACH) */
static ZH_SYMB s___msgEnumIndex   = { "__ENUMINDEX",     {ZH_FS_MESSAGE}, {ZH_FUNCNAME( msgNull )},       NULL };
static ZH_SYMB s___msgEnumBase    = { "__ENUMBASE",      {ZH_FS_MESSAGE}, {ZH_FUNCNAME( msgNull )},       NULL };
static ZH_SYMB s___msgEnumKey     = { "__ENUMKEY",       {ZH_FS_MESSAGE}, {ZH_FUNCNAME( msgNull )},       NULL };
static ZH_SYMB s___msgEnumValue   = { "__ENUMVALUE",     {ZH_FS_MESSAGE}, {ZH_FUNCNAME( msgNull )},       NULL };
static ZH_SYMB s___msgEnumIsFirst = { "__ENUMISFIRST",   {ZH_FS_MESSAGE}, {ZH_FUNCNAME( msgNull )},       NULL };
static ZH_SYMB s___msgEnumIsLast  = { "__ENUMISLAST",    {ZH_FS_MESSAGE}, {ZH_FUNCNAME( msgNull )},       NULL };

/* WITH OBJECT base value access/assign methods (:__withobject) */
static ZH_SYMB s___msgWithObjectPush = { "__WITHOBJECT",  {ZH_FS_MESSAGE}, {ZH_FUNCNAME( msgNull )},       NULL };
static ZH_SYMB s___msgWithObjectPop  = { "___WITHOBJECT", {ZH_FS_MESSAGE}, {ZH_FUNCNAME( msgNull )},       NULL };

/* --- */

/* Scalar classes' handles */

/* If user wants to change scalar classes at runtime in MT mode then
 * he must resolve thread synchronization problem himself, [druzus]
 */
static ZH_USHORT s_uiArrayClass     = 0;
static ZH_USHORT s_uiBlockClass     = 0;
static ZH_USHORT s_uiCharacterClass = 0;
static ZH_USHORT s_uiDateClass      = 0;
static ZH_USHORT s_uiTimeStampClass = 0;
static ZH_USHORT s_uiHashClass      = 0;
static ZH_USHORT s_uiLogicalClass   = 0;
static ZH_USHORT s_uiNilClass       = 0;
static ZH_USHORT s_uiNumericClass   = 0;
static ZH_USHORT s_uiSymbolClass    = 0;
static ZH_USHORT s_uiPointerClass   = 0;

static ZH_USHORT s_uiObjectClass    = 0;

/* --- */

/* Class definition holder */

/* In MT mode we are allocating array big enough to hold all
 * class definitions so we do not have to worry about runtime
 * s_pClasses reallocation, [druzus]
 */

#  include "zh_thread.h"

#  define ZH_CLASS_POOL_SIZE  16382
#  define ZH_CLASS_LOCK()     zh_threadEnterCriticalSection( &s_clsMtx )
#  define ZH_CLASS_UNLOCK()   zh_threadLeaveCriticalSection( &s_clsMtx )
   static ZH_CRITICAL_NEW( s_clsMtx );


#define ZH_CLASS_POOL_RESIZE  64

static PCLASS *  s_pClasses  = NULL;
static ZH_USHORT s_uiClsSize = 0;
static ZH_USHORT s_uiClasses = 0;

static PZH_ITEM s_pClassMtx = NULL;

/* --- */

#if 0
static ZH_USHORT zh_clsBucketPos( PZH_DYNS pMsg, ZH_USHORT uiMask )
{
   /* we can use PZH_DYNS address as base for hash key.
    * This value is perfectly unique and we do not need anything more
    * but it's not continuous so we will have to add dynamic BUCKETSIZE
    * modification to be 100% sure that we can resolve all symbol name
    * conflicts (though even without it it's rather theoretical problem).
    * [druzus]
    */

   /* Safely divide it by 16 - it's minimum memory allocated for single
    * ZH_DYNS structure
    */
   #if 0
   return ( ( ZH_USHORT ) ( ( ZH_PTRUINT ) pMsg >> 4 ) & uiMask ) << BUCKETBITS;
   #endif

   /* Using continuous symbol numbers we are 100% sure that we will cover
    * the whole 16-bit area and we will never have any problems until number
    * of symbols is limited to 2^16. [druzus]
    */
   return ( pMsg->uiSymNum & uiMask ) << BUCKETBITS;
}
#endif

/* zh_clsDictRealloc( PCLASS )
 *
 * Realloc (widen) class
 */
static ZH_BOOL zh_clsDictRealloc( PCLASS pClass )
{
   ZH_SIZE nNewHashKey, nLimit, n;

#ifdef ZH_MSG_POOL
   ZH_USHORT * puiMsgIdx;
#else
   PMETHOD pNewMethods;
#endif

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_clsDictRealloc(%p)", ( void * ) pClass ) );

   nNewHashKey = ( ZH_SIZE ) pClass->uiHashKey + 1;
   nLimit = nNewHashKey << BUCKETBITS;

   do
   {
      nNewHashKey <<= 1;
      if( nNewHashKey > HASH_KEYMAX )
         zh_errInternal( 6002, "Could not realloc class message in __clsDictRealloc()", NULL, NULL );

#ifdef ZH_MSG_POOL
      puiMsgIdx = ( ZH_USHORT * ) zh_xgrabz( ( nNewHashKey << BUCKETBITS ) * sizeof( ZH_USHORT ) );

      for( n = 0; n < nLimit; n++ )
      {
         ZH_USHORT uiMsg = pClass->puiMsgIdx[ n ];
         if( pClass->puiMsgIdx[ n ] )
         {
            ZH_USHORT uiBucket = BUCKETSIZE;
            ZH_USHORT * puiIdx = puiMsgIdx + zh_clsBucketPos(
                        pClass->pMethods[ uiMsg ].pMessage, nNewHashKey - 1 );
            do
            {
               if( *puiIdx == 0 )  /* this message position is empty */
               {
                  *puiIdx = uiMsg;
                  break;
               }
               ++puiIdx;
            }
            while( --uiBucket );

            /* Not enough go back to the beginning */
            if( ! uiBucket )
            {
               zh_xfree( puiMsgIdx );
               break;
            }
         }
      }
   }
   while( n < nLimit );

   pClass->uiHashKey = ( ZH_USHORT ) ( nNewHashKey - 1 );
   zh_xfree( pClass->puiMsgIdx );
   pClass->puiMsgIdx = puiMsgIdx;

#else

      pNewMethods = ( PMETHOD ) zh_xgrabz( ( nNewHashKey << BUCKETBITS ) * sizeof( METHOD ) );

      for( n = 0; n < nLimit; n++ )
      {
         PZH_DYNS pMessage = ( PZH_DYNS ) pClass->pMethods[ n ].pMessage;

         if( pMessage )
         {
            PMETHOD pMethod = pNewMethods + zh_clsBucketPos( pMessage, nNewHashKey - 1 );
            ZH_USHORT uiBucket = BUCKETSIZE;

            do
            {
               if( ! pMethod->pMessage ) /* this message position is empty */
               {
                  memcpy( pMethod, pClass->pMethods + n, sizeof( METHOD ) );
                  break;
               }
               ++pMethod;
            }
            while( --uiBucket );

            /* Not enough go back to the beginning */
            if( ! uiBucket )
            {
               zh_xfree( pNewMethods );
               break;
            }
         }
      }
   }
   while( n < nLimit );

   pClass->uiHashKey = ( ZH_USHORT ) ( nNewHashKey - 1 );
   zh_xfree( pClass->pMethods );
   pClass->pMethods = pNewMethods;
#endif

   return ZH_TRUE;
}

static void zh_clsDictInit( PCLASS pClass, ZH_USHORT uiHashKey )
{
   ZH_SIZE nSize;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_clsDictInit(%p,%hu)", ( void * ) pClass, uiHashKey ) );

   pClass->uiHashKey = uiHashKey;
#ifdef ZH_MSG_POOL
   nSize = ( ( ( ZH_SIZE ) uiHashKey + 1 ) << BUCKETBITS ) * sizeof( ZH_USHORT );
   pClass->puiMsgIdx = ( ZH_USHORT * ) zh_xgrabz( nSize );

   pClass->uiMethodCount = 1;
   pClass->pMethods = ( PMETHOD ) zh_xgrabz( sizeof( METHOD ) );
#else
   nSize = ( ( ( ZH_SIZE ) uiHashKey + 1 ) << BUCKETBITS ) * sizeof( METHOD );
   pClass->pMethods = ( PMETHOD ) zh_xgrabz( nSize );
#endif
}

static PMETHOD zh_clsFindMsg( PCLASS pClass, PZH_DYNS pMsg )
{
#ifdef ZH_MSG_POOL

   ZH_USHORT uiBucket, * puiMsgIdx;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_clsFindMsg(%p,%p)", ( void * ) pClass, ( void * ) pMsg ) );

   puiMsgIdx = pClass->puiMsgIdx + zh_clsBucketPos( pMsg, pClass->uiHashKey );
   uiBucket = BUCKETSIZE;

   do
   {
      PMETHOD pMethod = &pClass->pMethods[ *puiMsgIdx ];

      if( pMethod->pMessage == pMsg )
         return pMethod;
      ++puiMsgIdx;
   }
   while( --uiBucket );

#else

   PMETHOD pMethod;
   ZH_USHORT uiBucket;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_clsFindMsg(%p,%p)", ( void * ) pClass, ( void * ) pMsg ) );

   pMethod = pClass->pMethods + zh_clsBucketPos( pMsg, pClass->uiHashKey );
   uiBucket = BUCKETSIZE;

   do
   {
      if( pMethod->pMessage == pMsg )
         return pMethod;
      ++pMethod;
   }
   while( --uiBucket );

#endif

   return NULL;
}

static PMETHOD zh_clsAllocMsg( PCLASS pClass, PZH_DYNS pMsg )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_clsAllocMsg(%p,%p)", ( void * ) pClass, ( void * ) pMsg ) );

   do
   {

#ifdef ZH_MSG_POOL

      ZH_USHORT uiBucket = BUCKETSIZE, * puiMsgIdx = pClass->puiMsgIdx +
                                    zh_clsBucketPos( pMsg, pClass->uiHashKey );

      do
      {
         if( *puiMsgIdx == 0 )
         {
            pClass->pMethods = ( PMETHOD ) zh_xrealloc( pClass->pMethods,
                           sizeof( METHOD ) * ( pClass->uiMethodCount + 1 ) );
            memset( &pClass->pMethods[ pClass->uiMethodCount ], 0, sizeof( METHOD ) );
            *puiMsgIdx = pClass->uiMethodCount++;
            return &pClass->pMethods[ *puiMsgIdx ];
         }
         else if( pClass->pMethods[ *puiMsgIdx ].pMessage == pMsg )
            return &pClass->pMethods[ *puiMsgIdx ];
         ++puiMsgIdx;
      }
      while( --uiBucket );

#else

      PMETHOD pMethod = pClass->pMethods + zh_clsBucketPos( pMsg, pClass->uiHashKey );
      ZH_USHORT uiBucket = BUCKETSIZE;

      do
      {
         if( ! pMethod->pMessage || pMethod->pMessage == pMsg )
            return pMethod;
         ++pMethod;
      }
      while( --uiBucket );

#endif

   }
   while( zh_clsDictRealloc( pClass ) );

   zh_errInternal( 6001, "Could not allocate new message", NULL, NULL );

   return NULL;
}

static ZH_BOOL zh_clsCanClearMethod( PMETHOD pMethod, ZH_BOOL fError )
{
   ZH_SYMBOL_UNUSED( pMethod );
   ZH_SYMBOL_UNUSED( fError );
#if 0
   if( pMethod->pFuncSym == &s___msgSuper )
   {
      if( fError )
         zh_errRT_BASE( EG_ARG, 3000, "Cannot delete supercast messages", ZH_ERR_FUNCNAME, 0 );
      return ZH_FALSE;
   }
#endif
   return ZH_TRUE;
}

static void zh_clsFreeMsg( PCLASS pClass, PZH_DYNS pMsg )
{
#ifdef ZH_MSG_POOL

   ZH_USHORT uiBucket, * puiMsgIdx;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_clsFreeMsg(%p,%p)", ( void * ) pClass, ( void * ) pMsg ) );


   puiMsgIdx = pClass->puiMsgIdx + zh_clsBucketPos( pMsg, pClass->uiHashKey );
   uiBucket = BUCKETSIZE;

   do
   {
      if( *puiMsgIdx && pClass->pMethods[ *puiMsgIdx ].pMessage == pMsg )
      {
         if( zh_clsCanClearMethod( &pClass->pMethods[ *puiMsgIdx ], ZH_TRUE ) )
         {
            memset( &pClass->pMethods[ *puiMsgIdx ], 0, sizeof( METHOD ) );
            *puiMsgIdx = 0;
            pClass->uiMethods--;       /* Decrease number of messages */
         }
         return;
      }
      ++puiMsgIdx;
   }
   while( --uiBucket );

#else

   PMETHOD pMethod;
   ZH_USHORT uiBucket;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_clsFreeMsg(%p,%p)", ( void * ) pClass, ( void * ) pMsg ) );

   pMethod = pClass->pMethods + zh_clsBucketPos( pMsg, pClass->uiHashKey );
   uiBucket = BUCKETSIZE;

   do
   {
      if( pMethod->pMessage == pMsg )
      {
         if( zh_clsCanClearMethod( pMethod, ZH_TRUE ) )
         {
            /* Move messages */
            while( --uiBucket )
            {
               memcpy( pMethod, pMethod + 1, sizeof( METHOD ) );
               pMethod++;
            }
            memset( pMethod, 0, sizeof( METHOD ) );
            pClass->uiMethods--;       /* Decrease number of messages */
         }
         return;
      }
      ++pMethod;
   }
   while( --uiBucket );

#endif
}

static ZH_BOOL zh_clsHasParentClass( PCLASS pClass, ZH_USHORT uiParentCls )
{
   ZH_USHORT uiCount = pClass->uiSuperClasses;

   while( uiCount )
   {
      if( pClass->pSuperClasses[ --uiCount ].uiClass == uiParentCls )
         return ZH_TRUE;
   }
   return ZH_FALSE;

   /* alternative method but can give wrong results
    * if user overloads super casting method, [druzus].
    */
   #if 0
   PMETHOD pMethod = zh_clsFindMsg( pClass, s_pClasses[ uiParentCls ]->pClassSym );
   return pMethod && pMethod->pFuncSym == &s___msgSuper;
   #endif
}

static ZH_USHORT zh_clsGetParent( PCLASS pClass, PZH_DYNS pParentSym )
{
   ZH_USHORT uiCount = pClass->uiSuperClasses;

   while( uiCount )
   {
      ZH_USHORT uiClass = pClass->pSuperClasses[ --uiCount ].uiClass;
      if( s_pClasses[ uiClass ]->pClassSym == pParentSym )
         return uiClass;
   }
   return 0;

   /* alternative method but can give wrong results
    * if user overloads super casting method, [druzus].
    */
   #if 0
   PMETHOD pMethod = zh_clsFindMsg( pClass, pParentSym );
   return pMethod && pMethod->pFuncSym == &s___msgSuper;
   #endif
}

static ZH_USHORT zh_clsParentInstanceOffset( PCLASS pClass, ZH_USHORT uiParentCls )
{
   ZH_USHORT uiCount = pClass->uiSuperClasses;

   while( uiCount )
   {
      if( pClass->pSuperClasses[ --uiCount ].uiClass == uiParentCls )
         return pClass->pSuperClasses[ uiCount ].uiOffset;
   }
   return 0;
}

#if 0
static ZH_USHORT zh_clsParentInstanceOffset( PCLASS pClass, ZH_USHORT uiParentCls )
{
   PMETHOD pMethod = zh_clsFindMsg( pClass, s_pClasses[ uiParentCls ]->pClassSym );

   return ( pMethod && pMethod->pFuncSym == &s___msgSuper ) ? pMethod->uiOffset : 0;
}
#endif

static ZH_USHORT zh_clsAddInitValue( PCLASS pClass, PZH_ITEM pItem,
                                     ZH_USHORT uiType, ZH_USHORT uiData,
                                     ZH_USHORT uiOffset, ZH_USHORT uiSprClass )
{
   PINITDATA pInitData;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_clsAddInitValue(%p,%p,%hu,%hu,%hu,%hu)", ( void * ) pClass, ( void * ) pItem, uiType, uiData, uiOffset, uiSprClass ) );

   if( ! pItem || ZH_IS_NIL( pItem ) )
      return 0;

   if( ! pClass->uiInitDatas )
   {
      pClass->pInitData = ( PINITDATA ) zh_xgrab( sizeof( INITDATA ) );
      pInitData = pClass->pInitData + pClass->uiInitDatas++;
   }
   else
   {
      ZH_USHORT ui = pClass->uiInitDatas;
      pInitData = pClass->pInitData;
      do
      {
         if( pInitData->uiType == uiType &&
             pInitData->uiData + pInitData->uiOffset == uiData + uiOffset )
         {
            zh_itemRelease( pInitData->pInitValue );
            break;
         }
         ++pInitData;
      }
      while( --ui );

      if( ui == 0 )
      {
         pClass->pInitData = ( PINITDATA ) zh_xrealloc( pClass->pInitData,
                  ( ZH_SIZE ) ( pClass->uiInitDatas + 1 ) * sizeof( INITDATA ) );
         pInitData = pClass->pInitData + pClass->uiInitDatas++;
      }
   }

   pInitData->pInitValue = zh_itemClone( pItem );
   pInitData->uiType = uiType;
   pInitData->uiData = uiData;
   pInitData->uiOffset = uiOffset;
   pInitData->uiSprClass = uiSprClass;

   return pClass->uiInitDatas;
}

static ZH_USHORT zh_clsFindRealClassDataOffset( PMETHOD pMethod )
{
   PMETHOD pRealMth;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_clsFindRealClassDataOffset(%p)", ( void * ) pMethod ) );

   pRealMth = zh_clsFindMsg( s_pClasses[ pMethod->uiSprClass ],
                             pMethod->pMessage );
   if( pRealMth && pRealMth->uiSprClass == pMethod->uiSprClass &&
       ( pRealMth->pFuncSym == &s___msgSetClsData ||
         pRealMth->pFuncSym == &s___msgGetClsData ) )
   {
      return pRealMth->uiData;
   }
   return 0;
}

static ZH_USHORT zh_clsFindClassDataOffset( PCLASS pClass, PMETHOD pNewMethod )
{
   ZH_USHORT uiData;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_clsFindClassDataOffset(%p,%p)", ( void * ) pClass, ( void * ) pNewMethod ) );

   uiData = zh_clsFindRealClassDataOffset( pNewMethod );
   if( uiData )
   {
      ZH_SIZE nLimit = zh_clsMthNum( pClass );
      PMETHOD pMethod = pClass->pMethods;
      do
      {
         if( pMethod->pMessage && pMethod != pNewMethod &&
             pMethod->uiSprClass == pNewMethod->uiSprClass &&
             ( pMethod->pFuncSym == &s___msgSetClsData ||
               pMethod->pFuncSym == &s___msgGetClsData ) &&
             uiData == zh_clsFindRealClassDataOffset( pMethod ) )
         {
            return pMethod->uiData;
         }
         ++pMethod;
      }
      while( --nLimit );
   }

   return 0;
}

static ZH_BOOL zh_clsUpdateHiddenMessages( PMETHOD pSrcMethod, PMETHOD pDstMethod,
                                           PCLASS pDstClass )
{
   PMETHOD pNewMethod = pSrcMethod;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_clsUpdateHiddenMessages(%p,%p,%p)", ( void * ) pSrcMethod, ( void * ) pDstMethod, ( void * ) pDstClass ) );

   if( ! pDstMethod->pMessage ||
       ( zh_clsCanClearMethod( pDstMethod, ZH_FALSE ) &&
         pDstMethod->uiPrevCls != pDstMethod->uiSprClass &&
         ( pDstMethod->uiScope & ZH_OO_CLSTP_HIDDEN ) &&
         ( pDstMethod->uiScope & ZH_OO_CLSTP_NONVIRTUAL ) ) )
   {
      while( pNewMethod &&
             pNewMethod->uiPrevCls != pNewMethod->uiSprClass &&
             ( pNewMethod->uiScope & ZH_OO_CLSTP_HIDDEN ) &&
             ( pNewMethod->uiScope & ZH_OO_CLSTP_NONVIRTUAL ) )
      {
         pNewMethod = zh_clsFindMsg( s_pClasses[ pNewMethod->uiPrevCls ],
                                     pNewMethod->pMessage );
      }
      if( pNewMethod && pNewMethod != pSrcMethod &&
          ! ( pNewMethod->uiScope & ZH_OO_CLSTP_HIDDEN ) &&
          zh_clsCanClearMethod( pDstMethod, ZH_FALSE ) )
      {
         ZH_USHORT uiPrevCls = pDstMethod->uiPrevCls,
                   uiPrevMth = pDstMethod->uiPrevMth;
         PZH_SYMB pFuncSym;

         memcpy( pDstMethod, pNewMethod, sizeof( METHOD ) );
         pDstMethod->uiPrevCls = uiPrevCls;
         pDstMethod->uiPrevMth = uiPrevMth;
         pDstMethod->uiScope |= ZH_OO_CLSTP_OVERLOADED | ZH_OO_CLSTP_SUPER;
         pFuncSym = pDstMethod->pFuncSym;
         if( pFuncSym == &s___msgSync || pFuncSym == &s___msgSyncClass )
            pFuncSym = pDstMethod->pRealSym;
         if( pFuncSym == &s___msgSetData ||
             pFuncSym == &s___msgGetData )
         {
            pDstMethod->uiOffset = zh_clsParentInstanceOffset( pDstClass,
                                                   pDstMethod->uiSprClass );
         }
         else if( pFuncSym == &s___msgSetClsData ||
                  pFuncSym == &s___msgGetClsData )
         {
            PCLASS pSrcClass = s_pClasses[ pDstMethod->uiSprClass ];
            ZH_USHORT uiData;

            /* check if we already have corresponding access or assign
             * message for this class var to reuse its index
             */
            uiData = zh_clsFindClassDataOffset( pDstClass, pDstMethod );

            if( uiData == 0 )
            {
               uiData = ( ZH_USHORT ) zh_arrayLen( pDstClass->pClassDatas ) + 1;
               zh_arraySize( pDstClass->pClassDatas, uiData );
            }
            if( pDstMethod->uiOffset )
            {
               pDstMethod->uiOffset = zh_clsAddInitValue( pDstClass,
                  pSrcClass->pInitData[ pDstMethod->uiOffset - 1 ].pInitValue,
                  ZH_OO_MSG_CLASSDATA, uiData, 0, pDstMethod->uiSprClass );
            }
            pDstMethod->uiData = uiData;
         }
         return ZH_TRUE;
      }
   }

   return ZH_FALSE;
}

static void zh_clsAddSuperClass( PCLASS pClass, ZH_USHORT uiSuperCls, ZH_USHORT uiOffset )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_clsAddSuperClass(%p,%hu,%hu)", ( void * ) pClass, uiSuperCls, uiOffset ) );

   pClass->pSuperClasses = ( PZH_CLSCAST ) zh_xrealloc( pClass->pSuperClasses,
                     ( pClass->uiSuperClasses + 1 ) * sizeof( ZH_CLSCAST ) );
   pClass->pSuperClasses[ pClass->uiSuperClasses ].uiClass = uiSuperCls;
   pClass->pSuperClasses[ pClass->uiSuperClasses++ ].uiOffset = uiOffset;
}

static void zh_clsDefineSuperClass( PCLASS pClass, ZH_USHORT uiSuperCls, ZH_BOOL fNew )
{
   PMETHOD pMethod;
   PCLASS pSprCls;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_clsDefineSuperClass(%p,%hu,%d)", ( void * ) pClass, uiSuperCls, fNew ) );

   pSprCls = s_pClasses[ uiSuperCls ];

   if( ! zh_clsHasParentClass( pClass, uiSuperCls ) )
   {
      if( fNew )
      {
         zh_clsAddSuperClass( pClass, uiSuperCls, pClass->uiDatas );
         pClass->uiDatas += pSprCls->uiDatas - pSprCls->uiDataFirst;
      }
      else
         zh_clsAddSuperClass( pClass, uiSuperCls, pSprCls->uiDataFirst );
   }

   pMethod = zh_clsAllocMsg( pClass, pSprCls->pClassSym );
   if( pMethod->pMessage == NULL )
   {
      pClass->uiMethods++;
      pMethod->pMessage = pSprCls->pClassSym;
      pMethod->uiSprClass = pClass->uiClass;
      pMethod->uiData = uiSuperCls;
      pMethod->uiScope = ZH_OO_CLSTP_EXPORTED;
      pMethod->pFuncSym = &s___msgSuper;
      pMethod->uiOffset = zh_clsParentInstanceOffset( pClass, uiSuperCls );
   }
   else
   {
      PZH_SYMB pFuncSym = pMethod->pFuncSym;

      if( pFuncSym == &s___msgSync || pFuncSym == &s___msgSyncClass )
         pFuncSym = pMethod->pRealSym;
      if( pFuncSym == &s___msgSuper && pMethod->uiData == uiSuperCls )
         pMethod->uiOffset = zh_clsParentInstanceOffset( pClass, uiSuperCls );
   }
}

static void zh_clsCopyClass( PCLASS pClsDst, PCLASS pClsSrc )
{
   PMETHOD pMethod;
   ZH_SIZE nLimit;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_clsCopyClass(%p,%p)", ( void * ) pClsDst, ( void * ) pClsSrc ) );

   zh_clsDictInit( pClsDst, pClsSrc->uiHashKey );
   pClsDst->fHasOnError = pClsSrc->fHasOnError;
   pClsDst->fHasDestructor = pClsSrc->fHasDestructor;

   /* CLASS DATA Not Shared ( new array, new value ) */
#if 0
   /* enable this code if you want to inherit class variables values
    * from ancestor classes when new class is dynamically created.
    * (compatibility with older [x]Ziher versions)
    */
   pClsDst->pClassDatas = zh_arrayClone( pClsSrc->pClassDatas );
#else
   pClsDst->pClassDatas = zh_itemArrayNew( zh_arrayLen( pClsSrc->pClassDatas ) );
#endif
   /* do not copy shared data array - just simply create new one */
   pClsDst->pSharedDatas = zh_itemArrayNew( 0 );
   pClsDst->pInlines = zh_arrayClone( pClsSrc->pInlines );
   pClsDst->uiDatas = pClsSrc->uiDatas;
   pClsDst->uiMutexOffset = pClsSrc->uiMutexOffset;
   pClsDst->nOpFlags = pClsSrc->nOpFlags;
   if( pClsSrc->pMutex )
      pClsDst->pMutex = zh_threadMutexCreate();

   if( pClsSrc->uiInitDatas )
   {
      ZH_SIZE nSize = ( ZH_SIZE ) pClsSrc->uiInitDatas * sizeof( INITDATA );
      ZH_USHORT uiData;

      pClsDst->uiInitDatas = pClsSrc->uiInitDatas;
      pClsDst->pInitData = ( PINITDATA ) zh_xgrab( nSize );
      memcpy( pClsDst->pInitData, pClsSrc->pInitData, nSize );
      for( uiData = 0; uiData < pClsDst->uiInitDatas; ++uiData )
      {
         if( pClsDst->pInitData[ uiData ].uiType == ZH_OO_MSG_INITIALIZED )
            pClsDst->pInitData[ uiData ].uiType = ZH_OO_MSG_CLASSDATA;
         pClsDst->pInitData[ uiData ].pInitValue =
                        zh_itemNew( pClsDst->pInitData[ uiData ].pInitValue );
      }
   }

   nLimit = zh_clsMthNum( pClsSrc );
#ifdef ZH_MSG_POOL
   memcpy( pClsDst->puiMsgIdx, pClsSrc->puiMsgIdx,
      ( ( ( ZH_SIZE ) pClsSrc->uiHashKey + 1 ) << BUCKETBITS ) * sizeof( ZH_USHORT ) );
   pClsDst->uiMethodCount = pClsSrc->uiMethodCount;
   pClsDst->pMethods = ( PMETHOD ) zh_xrealloc( pClsDst->pMethods,
                                                nLimit * sizeof( METHOD ) );
#endif
   memcpy( pClsDst->pMethods, pClsSrc->pMethods, nLimit * sizeof( METHOD ) );
   pClsDst->uiMethods = pClsSrc->uiMethods;

   if( pClsSrc->uiSuperClasses )
   {
      pClsDst->uiSuperClasses = pClsSrc->uiSuperClasses;
      pClsDst->pSuperClasses = ( PZH_CLSCAST )
                     zh_xgrab( pClsSrc->uiSuperClasses * sizeof( ZH_CLSCAST ) );
      memcpy( pClsDst->pSuperClasses, pClsSrc->pSuperClasses,
              pClsSrc->uiSuperClasses * sizeof( ZH_CLSCAST ) );
   }
   zh_clsDefineSuperClass( pClsDst, pClsSrc->uiClass, ZH_FALSE );

   pMethod = pClsDst->pMethods;
   do
   {
      if( pMethod->pMessage )
      {
         zh_clsUpdateHiddenMessages( pMethod, pMethod, pClsDst );
         pMethod->uiScope |= ZH_OO_CLSTP_SUPER;
      }
      ++pMethod;
   }
   while( --nLimit );
}

static ZH_BOOL zh_clsIsFriendSymbol( PCLASS pClass, PZH_SYMB pSym )
{
   ZH_USHORT uiCount;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_clsIsFriendSymbol(%p,%p)", ( void * ) pClass, ( void * ) pSym ) );

   if( pSym >= pClass->pFriendModule &&
       pSym < pClass->pFriendModule + pClass->uiFriendModule )
      return ZH_TRUE;

   for( uiCount = 0; uiCount < pClass->uiFriendSyms; ++uiCount )
   {
      if( pClass->pFriendSyms[ uiCount ] == pSym )
         return ZH_TRUE;
   }

   return ZH_FALSE;
}

static void zh_clsAddFriendSymbol( PCLASS pClass, PZH_SYMB pSym )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_clsAddFriendSymbol(%p,%p)", ( void * ) pClass, ( void * ) pSym ) );

   if( ! zh_clsIsFriendSymbol( pClass, pSym ) )
   {
      if( pClass->uiFriendSyms == 0 )
      {
         pClass->pFriendSyms = ( PZH_SYMB * ) zh_xgrab( sizeof( PZH_SYMB ) );
         pClass->pFriendSyms[ 0 ] = pSym;
         pClass->uiFriendSyms++;
      }
      else
      {
         pClass->pFriendSyms = ( PZH_SYMB * ) zh_xrealloc( pClass->pFriendSyms,
                           ( pClass->uiFriendSyms + 1 ) * sizeof( PZH_SYMB ) );
         pClass->pFriendSyms[ pClass->uiFriendSyms++ ] = pSym;
      }
   }
}

/* initialize Classy/OO system at ZHVM startup */
void zh_clsInit( void )
{
   PZH_SYMB pOpSym;
   ZH_USHORT uiOperator;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_clsInit()" ) );

   for( uiOperator = 0, pOpSym = s_opSymbols; uiOperator <= ZH_OO_MAX_OPERATOR;
        ++uiOperator, ++pOpSym )
   {
      pOpSym->pDynSym = zh_dynsymGetCase( pOpSym->szName );
   }

   s___msgDestructor.pDynSym  = zh_dynsymGetCase( s___msgDestructor.szName );
   s___msgOnError.pDynSym     = zh_dynsymGetCase( s___msgOnError.szName );

   s___msgClassName.pDynSym   = zh_dynsymGetCase( s___msgClassName.szName );  /* Standard messages */
   s___msgClassH.pDynSym      = zh_dynsymGetCase( s___msgClassH.szName );     /* Not present in classdef. */
   s___msgClassSel.pDynSym    = zh_dynsymGetCase( s___msgClassSel.szName );
   s___msgExec.pDynSym        = zh_dynsymGetCase( s___msgExec.szName );
   s___msgName.pDynSym        = zh_dynsymGetCase( s___msgName.szName );
   s___msgNew.pDynSym         = zh_dynsymGetCase( s___msgNew.szName );
   s___msgSymbol.pDynSym      = zh_dynsymGetCase( s___msgSymbol.szName );
   s___msgKeys.pDynSym        = zh_dynsymGetCase( s___msgKeys.szName );
   s___msgValues.pDynSym      = zh_dynsymGetCase( s___msgValues.szName );
/*
   s___msgClsParent.pDynSym   = zh_dynsymGetCase( s___msgClsParent.szName );
   s___msgClass.pDynSym       = zh_dynsymGetCase( s___msgClass.szName );
*/
   s___msgEnumIndex.pDynSym   = zh_dynsymGetCase( s___msgEnumIndex.szName );
   s___msgEnumBase.pDynSym    = zh_dynsymGetCase( s___msgEnumBase.szName );
   s___msgEnumKey.pDynSym     = zh_dynsymGetCase( s___msgEnumKey.szName );
   s___msgEnumValue.pDynSym   = zh_dynsymGetCase( s___msgEnumValue.szName );
   s___msgEnumIsFirst.pDynSym = zh_dynsymGetCase( s___msgEnumIsFirst.szName );
   s___msgEnumIsLast.pDynSym  = zh_dynsymGetCase( s___msgEnumIsLast.szName );

   s___msgWithObjectPush.pDynSym = zh_dynsymGetCase( s___msgWithObjectPush.szName );
   s___msgWithObjectPop.pDynSym  = zh_dynsymGetCase( s___msgWithObjectPop.szName );


   s_uiClsSize = ZH_CLASS_POOL_SIZE;
   s_uiClasses = 0;
   s_pClasses = ( PCLASS * ) zh_xgrab( ( ( ZH_SIZE ) s_uiClsSize + 1 ) * sizeof( PCLASS ) );
   s_pClasses[ 0 ] = NULL;

   s_pClassMtx = zh_threadMutexCreate();
}

/* initialize Classy/OO system .zh functions */
void zh_clsDoInit( void )
{
   static const char * s_pszFuncNames[] =
      { "HBARRAY", "HBBLOCK", "HBCHARACTER",
        "ZHDATE", "HBTIMESTAMP",
        "HBHASH", "HBLOGICAL", "HBNIL", "HBNUMERIC",
        "HBSYMBOL", "HBPOINTER",
        "ZHObject" };
   static ZH_USHORT * s_puiHandles[] =
      { &s_uiArrayClass, &s_uiBlockClass, &s_uiCharacterClass,
        &s_uiDateClass, &s_uiTimeStampClass,
        &s_uiHashClass, &s_uiLogicalClass, &s_uiNilClass, &s_uiNumericClass,
        &s_uiSymbolClass, &s_uiPointerClass,
        &s_uiObjectClass };

   ZH_STACK_TLS_PRELOAD
   int i;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_clsDoInit()" ) );

   for( i = 0; i < ( int ) ZH_SIZEOFARRAY( s_puiHandles ); ++i )
   {
      PZH_DYNS pFuncSym = zh_dynsymFindName( s_pszFuncNames[i] );
      if( pFuncSym && zh_dynsymIsFunction( pFuncSym ) )
      {
         PZH_ITEM pReturn = zh_stackReturnItem();
         zh_itemSetNil( pReturn );
         zh_vmPushDynSym( pFuncSym );
         zh_vmPushNil();
         zh_vmProc( 0 );
         if( ZH_IS_OBJECT( pReturn ) )
            *( s_puiHandles[ i ] ) = pReturn->item.asArray.value->uiClass;
      }
   }
}

/* zh_clsRelease( <pClass> )
 *
 * Release a class from memory
 */
static void zh_clsRelease( PCLASS pClass )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_clsRelease(%p)", ( void * ) pClass ) );

   if( pClass->uiInitDatas )
   {
      ZH_USHORT ui = pClass->uiInitDatas;
      PINITDATA pInitData = pClass->pInitData;

      do
      {
         zh_itemRelease( pInitData->pInitValue );
         ++pInitData;
      }
      while( --ui );
      zh_xfree( pClass->pInitData );
   }

   if( pClass->szName )
      zh_xfree( pClass->szName );
   if( pClass->pMethods )
      zh_xfree( pClass->pMethods );
   if( pClass->uiFriendSyms )
      zh_xfree( pClass->pFriendSyms );
   if( pClass->pSuperClasses )
      zh_xfree( pClass->pSuperClasses );
#ifdef ZH_MSG_POOL
   if( pClass->puiMsgIdx )
      zh_xfree( pClass->puiMsgIdx );
#endif
   if( pClass->pClassDatas )
      zh_itemRelease( pClass->pClassDatas );
   if( pClass->pSharedDatas )
      zh_itemRelease( pClass->pSharedDatas );
   if( pClass->pInlines )
      zh_itemRelease( pClass->pInlines );

   zh_xfree( pClass );
}


/* zh_clsReleaseAll()
 *
 * Release all classes
 */
void zh_clsReleaseAll( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_clsReleaseAll()" ) );

   if( s_uiClasses )
   {
      ZH_USHORT uiClass = s_uiClasses;

      /* It blocks destructor execution - don't move. [druzus] */
      s_uiClasses = 0;

      do
      {
         zh_clsRelease( s_pClasses[ uiClass ] );
      }
      while( --uiClass );

   }

   if( s_pClasses )
   {
      zh_xfree( s_pClasses );
      s_pClasses = NULL;
      s_uiClsSize = 0;
   }

   if( s_pClassMtx )
   {
      zh_itemRelease( s_pClassMtx );
      s_pClassMtx = NULL;
   }
}

/* Mark all internal data as used so it will not be released by the
 * garbage collector
 */
void zh_clsIsClassRef( void )
{
   /* All internal items are allocated with zh_itemNew()
    * GC knows them and scan itself so it's not necessary
    * to repeat scanning here [druzus].
    */
#if 0
   ZH_USHORT uiClass = s_uiClasses;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_clsIsClassRef()" ) );

   while( uiClass )
   {
      PCLASS pClass = s_pClasses[ uiClass-- ];

      if( pClass->pInlines )
         zh_gcItemRef( pClass->pInlines );

      if( pClass->pClassDatas )
         zh_gcItemRef( pClass->pClassDatas );

      if( pClass->pSharedDatas )
         zh_gcItemRef( pClass->pSharedDatas );

      if( pClass->uiInitDatas )
      {
         ZH_USHORT ui = pClass->uiInitDatas;
         PINITDATA pInitData = pClass->pInitData;

         do
         {
            if( ZH_IS_GCITEM( pInitData->pInitValue ) )
               zh_gcItemRef( pInitData->pInitValue );
            ++pInitData;
         }
         while( --ui );
      }
   }
#endif
}

ZH_BOOL zh_clsIsParent( ZH_USHORT uiClass, const char * szParentName )
{
   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS pClass = s_pClasses[ uiClass ];

      if( strcmp( pClass->szName, szParentName ) == 0 )
         return ZH_TRUE;
      else
      {
         PZH_DYNS pMsg = zh_dynsymFindName( szParentName );

         if( pMsg )
            return zh_clsGetParent( pClass, pMsg ) != 0;
      }
   }

   return ZH_FALSE;
}

ZH_USHORT zh_objGetClass( PZH_ITEM pItem )
{
   if( pItem && ZH_IS_ARRAY( pItem ) )
      return pItem->item.asArray.value->uiClass;
   else
      return 0;
}

/* get object class handle using class name and class function name */
ZH_USHORT zh_objSetClass( PZH_ITEM pItem, const char * szClass, const char * szFunc )
{
   ZH_USHORT uiClass = 0;

   if( pItem && ZH_IS_ARRAY( pItem ) &&
       pItem->item.asArray.value->uiClass == 0 )
   {
      uiClass = pItem->item.asArray.value->uiClass =
                                          zh_clsFindClass( szClass, szFunc );
   }
   return uiClass;
}

/* --- */

/* Get the class handle */
static ZH_USHORT zh_objGetClassH( PZH_ITEM pObject )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_objGetClassH(%p)", ( void * ) pObject ) );

   if( ZH_IS_ARRAY( pObject ) )
   {
      if( pObject->item.asArray.value->uiClass != 0 )
         return pObject->item.asArray.value->uiClass;
      else
         return s_uiArrayClass;
   }
   /* built in types */
   else if( ZH_IS_NIL( pObject ) )
      return s_uiNilClass;

   else if( ZH_IS_STRING( pObject ) )
      return s_uiCharacterClass;

   else if( ZH_IS_NUMERIC( pObject ) )
      return s_uiNumericClass;

   else if( ZH_IS_DATE( pObject ) )
      return s_uiDateClass;

   else if( ZH_IS_TIMESTAMP( pObject ) )
      return s_uiTimeStampClass;

   else if( ZH_IS_LOGICAL( pObject ) )
      return s_uiLogicalClass;

   else if( ZH_IS_BLOCK( pObject ) )
      return s_uiBlockClass;

   else if( ZH_IS_HASH( pObject ) )
      return s_uiHashClass;

   else if( ZH_IS_POINTER( pObject ) )
      return s_uiPointerClass;

   else if( ZH_IS_SYMBOL( pObject ) )
      return s_uiSymbolClass;

   return 0;
}

/* Get the class name of an object */
const char * zh_objGetClsName( PZH_ITEM pObject )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_objGetClsName(%p)", ( void * ) pObject ) );

   if( ZH_IS_ARRAY( pObject ) )
   {
      if( pObject->item.asArray.value->uiClass != 0 )
         return s_pClasses[ pObject->item.asArray.value->uiClass ]->szName;
      else
         return "ARRAY";
   }
   /* built in types */
   else if( ZH_IS_NIL( pObject ) )
      return "NIL";

   else if( ZH_IS_STRING( pObject ) )
      return "CHARACTER";

   else if( ZH_IS_NUMERIC( pObject ) )
      return "NUMERIC";

   else if( ZH_IS_DATE( pObject ) )
      return "DATE";

   else if( ZH_IS_TIMESTAMP( pObject ) )
      return "TIMESTAMP";

   else if( ZH_IS_LOGICAL( pObject ) )
      return "LOGICAL";

   else if( ZH_IS_BLOCK( pObject ) )
      return "BLOCK";

   else if( ZH_IS_HASH( pObject ) )
      return "HASH";

   else if( ZH_IS_POINTER( pObject ) )
      return "POINTER";

   else if( ZH_IS_SYMBOL( pObject ) )
      return "SYMBOL";

   else
      return "UNKNOWN";
}

const char * zh_clsName( ZH_USHORT uiClass )
{
   if( uiClass && uiClass <= s_uiClasses )
      return s_pClasses[ uiClass ]->szName;
   else
      return NULL;
}

const char * zh_clsFuncName( ZH_USHORT uiClass )
{
   if( uiClass && uiClass <= s_uiClasses )
      return s_pClasses[ uiClass ]->pClassFuncSym ?
             s_pClasses[ uiClass ]->pClassFuncSym->szName :
             "";
   else
      return NULL;
}

PZH_SYMB zh_clsFuncSym( ZH_USHORT uiClass )
{
   if( uiClass && uiClass <= s_uiClasses )
      return s_pClasses[ uiClass ]->pClassFuncSym;
   else
      return NULL;
}

const char * zh_clsMethodName( ZH_USHORT uiClass, ZH_USHORT uiMethod )
{
   if( uiClass && uiClass <= s_uiClasses &&
       ( ZH_UINT ) uiMethod < ( ZH_UINT ) zh_clsMthNum( s_pClasses[ uiClass ] ) )
   {
      PMETHOD pMethod = s_pClasses[ uiClass ]->pMethods + uiMethod;
      if( pMethod->pMessage )
         return pMethod->pMessage->pSymbol->szName;
   }
   return NULL;
}

static ZH_SIZE zh_clsGetVarIndexEx( ZH_USHORT uiClass, PZH_DYNS pVarSym,
                                    ZH_USHORT uiSuper )
{
   PMETHOD pMethod = zh_clsFindMsg( s_pClasses[ uiSuper ], pVarSym );
   if( pMethod )
   {
      PZH_SYMB pFuncSym = pMethod->pFuncSym;

      if( pFuncSym == &s___msgSync || pFuncSym == &s___msgSyncClass )
         pFuncSym = pMethod->pRealSym;

      if( pFuncSym == &s___msgSetData || pFuncSym == &s___msgGetData )
      {
         return ( ZH_SIZE ) pMethod->uiData + ( uiClass != uiSuper ?
                zh_clsParentInstanceOffset( s_pClasses[ uiClass ], uiSuper ) :
                pMethod->uiOffset );
      }
   }
   return 0;
}

ZH_SIZE zh_clsGetVarIndex( ZH_USHORT uiClass, PZH_DYNS pVarSym )
{
   if( uiClass && uiClass <= s_uiClasses )
      return zh_clsGetVarIndexEx( uiClass, pVarSym, uiClass );
   else
      return 0;
}

ZH_USHORT zh_clsFindClass( const char * szClass, const char * szClassFunc )
{
   ZH_USHORT uiClass;

   for( uiClass = 1; uiClass <= s_uiClasses; uiClass++ )
   {
      if( strcmp( szClass, s_pClasses[ uiClass ]->szName ) == 0 &&
          ( ! szClassFunc || ( ! s_pClasses[ uiClass ]->pClassFuncSym ? ! *szClassFunc :
            strcmp( szClassFunc, s_pClasses[ uiClass ]->pClassFuncSym->szName ) == 0 ) ) )
      {
         return uiClass;
      }
   }
   return 0;
}

static ZH_USHORT zh_clsFindClassByFunc( PZH_SYMB pClassFuncSym )
{
   ZH_USHORT uiClass;

   for( uiClass = 1; uiClass <= s_uiClasses; uiClass++ )
   {
      if( s_pClasses[ uiClass ]->pClassFuncSym == pClassFuncSym )
      {
         return uiClass;
      }
   }
   return 0;
}

/* Get the real method symbol for given stack symbol */
PZH_SYMB zh_clsMethodSym( PZH_ITEM pBaseSymbol )
{
   PZH_STACK_STATE pStack = pBaseSymbol->item.asSymbol.stackstate;

   if( pStack->uiClass )
   {
      PMETHOD pMethod = s_pClasses[ pStack->uiClass ]->pMethods + pStack->uiMethod;
      PZH_SYMB pFuncSym = pMethod->pFuncSym;

      if( pFuncSym == &s___msgSync || pFuncSym == &s___msgSyncClass )
         pFuncSym = pMethod->pRealSym;

      if( pFuncSym == &s___msgEvalInline )
      {
         PZH_ITEM pItem = zh_arrayGetItemPtr( s_pClasses[ pMethod->uiSprClass ]->pInlines,
                                              pMethod->uiData );
         return pItem ? pItem->item.asBlock.value->pDefSymb : NULL;
      }
/*
      else if( pFuncSym == &s___msgPerform )
 */
      else if( pFuncSym == &s___msgDelegate )
         return s_pClasses[ pStack->uiClass ]->pMethods[ pMethod->uiData ].pFuncSym;
      else
         return pFuncSym;
   }

   return pBaseSymbol->item.asSymbol.value;
}

/* Get the real class name of an object message
 * Will return the class name from wich the message is inherited in case
 * of inheritance.
 */
const char * zh_objGetRealClsName( PZH_ITEM pObject, const char * szName )
{
   ZH_USHORT uiClass;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_objGetrealClsName(%p,%s)", ( void * ) pObject, szName ) );

   uiClass = zh_objGetClassH( pObject );
   if( uiClass && uiClass <= s_uiClasses )
   {
      PZH_DYNS pMsg = zh_dynsymFindName( szName );

      if( pMsg )
      {
         PMETHOD pMethod = zh_clsFindMsg( s_pClasses[ uiClass ], pMsg );
         if( pMethod )
            uiClass = pMethod->uiSprClass;
      }
      if( uiClass && uiClass <= s_uiClasses )
         return s_pClasses[ uiClass ]->szName;
   }

   return zh_objGetClsName( pObject );
}

#if defined( ZH_CLASSY_BLOCK_SCOPE )
static ZH_ISIZ zh_clsSenderOffset( void )
{
   ZH_ISIZ nOffset = zh_stackBaseProcOffset( 1 );

   if( nOffset > 0 )
   {
      /* Is it inline method? */
      if( nOffset > 0 && ZH_IS_BLOCK( zh_stackItem( nOffset + 1 ) ) &&
          ( zh_stackItem( nOffset )->item.asSymbol.value == &zh_symEval ||
            zh_stackItem( nOffset )->item.asSymbol.value->pDynSym ==
            zh_symEval.pDynSym ) )
      {
         nOffset = zh_stackItem( nOffset )->item.asSymbol.stackstate->nBaseItem;

         /* I do not like it but Class(y) makes something like that. [druzus] */
         while( nOffset > 0 &&
                zh_stackItem( nOffset )->item.asSymbol.stackstate->uiClass == 0 )
            nOffset = zh_stackItem( nOffset )->item.asSymbol.stackstate->nBaseItem;
      }
      return nOffset;
   }
   return -1;
}
#endif

#if 0
static ZH_USHORT zh_clsSenderClass( void )
{
   ZH_ISIZ nOffset = zh_clsSenderOffset();

   if( nOffset > 0 )
      return zh_stackItem( nOffset )->item.asSymbol.stackstate->uiClass;
   else
      return 0;
}
#endif

static ZH_USHORT zh_clsSenderMethodClass( void )
{
   ZH_ISIZ nOffset = zh_clsSenderOffset();

   if( nOffset > 0 )
   {
      ZH_STACK_TLS_PRELOAD
      PZH_STACK_STATE pStack = zh_stackItem( nOffset )->item.asSymbol.stackstate;

      if( pStack->uiClass )
         return ( s_pClasses[ pStack->uiClass ]->pMethods +
                  pStack->uiMethod )->uiSprClass;
   }
   return 0;
}

static PZH_SYMB zh_clsSenderSymbol( void )
{
   PZH_SYMB pSym = NULL;
   ZH_ISIZ nOffset = zh_clsSenderOffset();

   if( nOffset > 0 )
   {
      ZH_STACK_TLS_PRELOAD
      pSym = zh_stackItem( nOffset )->item.asSymbol.value;

      if( pSym == &zh_symEval || pSym->pDynSym == zh_symEval.pDynSym )
      {
         PZH_ITEM pBlock = zh_stackItem( nOffset + 1 );

         if( ZH_IS_BLOCK( pBlock ) )
            pSym = pBlock->item.asBlock.value->pDefSymb;
      }
   }

   return zh_vmGetRealFuncSym( pSym );
}

static ZH_USHORT zh_clsSenderObjectClass( void )
{
   ZH_ISIZ nOffset = zh_clsSenderOffset();

   if( nOffset > 0 )
   {
      ZH_STACK_TLS_PRELOAD
      PZH_ITEM pSender = zh_stackItem( nOffset + 1 );

      if( ZH_IS_ARRAY( pSender ) )
         return pSender->item.asArray.value->uiClass;
   }
   return 0;
}

static PZH_SYMB zh_clsValidScope( PMETHOD pMethod, PZH_STACK_STATE pStack )
{
   if( pMethod->uiScope & ( ZH_OO_CLSTP_HIDDEN | ZH_OO_CLSTP_PROTECTED |
                            ZH_OO_CLSTP_OVERLOADED ) )
   {
      ZH_USHORT uiSenderClass = zh_clsSenderMethodClass();

      if( uiSenderClass == pMethod->uiSprClass )
         return pMethod->pFuncSym;
      else if( uiSenderClass )
      {
         /* Warning!!! Friends cannot access overloaded non virtual methods.
          * This feature is available _ONLY_ for real class members, [druzus]
          */
         if( pMethod->uiScope & ZH_OO_CLSTP_OVERLOADED &&
             zh_clsHasParentClass( s_pClasses[ pStack->uiClass ], uiSenderClass ) )
         {
            PCLASS pClass = s_pClasses[ uiSenderClass ];
            PMETHOD pHiddenMthd = zh_clsFindMsg( pClass, pMethod->pMessage );

            if( pHiddenMthd && ( pHiddenMthd->uiScope & ZH_OO_CLSTP_NONVIRTUAL ) &&
                pHiddenMthd->uiSprClass == uiSenderClass )
            {
               pStack->uiClass = uiSenderClass;
               pStack->uiMethod = ( ZH_USHORT ) ( pHiddenMthd - pClass->pMethods );
               return pHiddenMthd->pFuncSym;
            }
         }

         if( pMethod->uiScope & ZH_OO_CLSTP_HIDDEN )
         {
            if( ! zh_clsIsFriendSymbol( s_pClasses[ pMethod->uiSprClass ],
                              s_pClasses[ uiSenderClass ]->pClassFuncSym ) )
               return &s___msgScopeErr;
         }
         else if( pMethod->uiScope & ZH_OO_CLSTP_PROTECTED &&
             ! zh_clsHasParentClass( s_pClasses[ pStack->uiClass ],
                                     uiSenderClass ) &&
             ! zh_clsHasParentClass( s_pClasses[ uiSenderClass ],
                                     pStack->uiClass ) &&
             ! zh_clsIsFriendSymbol( s_pClasses[ pMethod->uiSprClass ],
                                     s_pClasses[ uiSenderClass ]->pClassFuncSym ) &&
             ( pStack->uiClass == pMethod->uiSprClass ||
               ! zh_clsIsFriendSymbol( s_pClasses[ pStack->uiClass ],
                              s_pClasses[ uiSenderClass ]->pClassFuncSym ) ) )
            return &s___msgScopeErr;
      }
      else if( pMethod->uiScope & ( ZH_OO_CLSTP_HIDDEN | ZH_OO_CLSTP_PROTECTED ) )
      {
         PZH_SYMB pSym = zh_clsSenderSymbol();

         if( ! zh_clsIsFriendSymbol( s_pClasses[ pMethod->uiSprClass ], pSym ) )
         {
            if( ( pMethod->uiScope & ZH_OO_CLSTP_HIDDEN ) ||
                ! zh_clsIsFriendSymbol( s_pClasses[ pStack->uiClass ], pSym ) )
               return &s___msgScopeErr;
         }
      }
   }

   return pMethod->pFuncSym;
}

static PZH_SYMB zh_clsScalarMethod( PCLASS pClass, PZH_DYNS pMsg,
                                    PZH_STACK_STATE pStack )
{
   PMETHOD pMethod = zh_clsFindMsg( pClass, pMsg );

   if( pStack )
   {
      pStack->uiClass = pClass->uiClass;
      if( pMethod )
      {
         pStack->uiMethod = ( ZH_USHORT ) ( pMethod - pClass->pMethods );
         return zh_clsValidScope( pMethod, pStack );
      }
   }
   else if( pMethod )
      return pMethod->pFuncSym;

   return NULL;
}

static void zh_clsMakeSuperObject( PZH_ITEM pDest, PZH_ITEM pObject,
                                   ZH_USHORT uiSuperClass )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_clsMakeSuperObject(%p, %p, %hu)", ( void * ) pDest, ( void * ) pObject, uiSuperClass ) );

   /* create a fake object array */
   zh_arrayNew( pDest, 1 );
   /* Now save the Self object as the 1st elem. */
   zh_arraySet( pDest, 1, pObject );
   /* And transform it into a fake object */
   /* backup of actual handle */
   pDest->item.asArray.value->uiPrevCls = zh_objGetClassH( pObject );
   /* superclass handle casting */
   pDest->item.asArray.value->uiClass = uiSuperClass;
}

/* <pFuncSym> = zh_objGetMethod( <pObject>, <pMessage>, <pStackState> )
 *
 * Internal function to the function pointer of a message of an object
 */
PZH_SYMB zh_objGetMethod( PZH_ITEM pObject, PZH_SYMB pMessage,
                          PZH_STACK_STATE pStack )
{
   ZH_STACK_TLS_PRELOAD
   PCLASS pClass = NULL;
   PZH_DYNS pMsg;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_objGetMethod(%p, %p, %p)", ( void * ) pObject, ( void * ) pMessage, ( void * ) pStack ) );

   pMsg = pMessage->pDynSym;

   if( ZH_IS_ARRAY( pObject ) )
   {
      if( pObject->item.asArray.value->uiClass )
      {
         pClass = s_pClasses[ pObject->item.asArray.value->uiClass ];
         if( pStack )
         {
            pStack->uiClass = pObject->item.asArray.value->uiClass;
            if( pObject->item.asArray.value->uiPrevCls )
            {
               if( pObject->item.asArray.value->nLen )
               {
                  /* Copy real object - do not move! the same super casted
                   * object can be used more then once and we mustn't
                   * destroy it. We can safely use zh_stackReturnItem() here.
                   */
                  zh_itemCopy( zh_stackReturnItem(), pObject->item.asArray.value->pItems );
                  /* move real object back to the stack */
                  zh_itemMove( pObject, zh_stackReturnItem() );
               }
               else
                  /* Someone tried to manipulate with supercast array */
                  zh_itemClear( pObject );
            }
#ifdef ZH_MSG_POOL
            {
               ZH_USHORT uiBucket = BUCKETSIZE, * puiMsgIdx =
                  pClass->puiMsgIdx + zh_clsBucketPos( pMsg, pClass->uiHashKey );

               do
               {
                  PMETHOD pMethod = &pClass->pMethods[ *puiMsgIdx ];
                  if( pMethod->pMessage == pMsg )
                  {
                     pStack->uiMethod = *puiMsgIdx;
                     return zh_clsValidScope( pMethod, pStack );
                  }
                  ++puiMsgIdx;
               }
               while( --uiBucket );
            }
#else
            {
               PMETHOD pMethod = zh_clsFindMsg( pClass, pMsg );
               if( pMethod )
               {
                  pStack->uiMethod = ( ZH_USHORT ) ( pMethod - pClass->pMethods );
                  return zh_clsValidScope( pMethod, pStack );
               }
            }
#endif
         }
         else
         {
            PMETHOD pMethod = zh_clsFindMsg( pClass, pMsg );
            if( pMethod )
               return pMethod->pFuncSym;
         }
      }
      else if( s_uiArrayClass )
      {
         pClass = s_pClasses[ s_uiArrayClass ];
         {
            PZH_SYMB pExecSym = zh_clsScalarMethod( pClass, pMsg, pStack );
            if( pExecSym )
               return pExecSym;
         }
      }
   }
   else if( ZH_IS_BLOCK( pObject ) )
   {
      if( pMsg == zh_symEval.pDynSym )
         return &zh_symEval;
      else if( s_uiBlockClass )
      {
         pClass = s_pClasses[ s_uiBlockClass ];
         {
            PZH_SYMB pExecSym = zh_clsScalarMethod( pClass, pMsg, pStack );
            if( pExecSym )
               return pExecSym;
         }
      }
   }
   else if( ZH_IS_BYREF( pObject ) )
   {
      if( pStack )
      {
         /* method of enumerator variable from FOR EACH statement
          */
         PZH_ITEM pEnum = zh_itemUnRefOnce( pObject );

         if( ZH_IS_ENUM( pEnum ) )
         {
            /* Do actions here - we already have unreferenced pEnum so
             * it will be a little bit faster but in the future it's
             * possible that I'll move it to separate function when
             * I'll add enumerators overloading. [druzus]
             */
            if( pMsg == s___msgEnumIndex.pDynSym )
            {
               zh_itemPutNS( zh_stackReturnItem(), pEnum->item.asEnum.offset );
               if( zh_pcount() > 0 && ZH_IS_PARAM_NUM( 1 ) )
                  pEnum->item.asEnum.offset = zh_itemGetNS( zh_param( 1, ZH_IT_ANY ) );
               return &s___msgEnumIndex;
            }
            else if( pMsg == s___msgEnumKey.pDynSym )
            {
               PZH_ITEM pBase = ZH_IS_BYREF( pEnum->item.asEnum.basePtr ) ?
                                zh_itemUnRef( pEnum->item.asEnum.basePtr ) :
                                pEnum->item.asEnum.basePtr;
               if( ZH_IS_HASH( pBase ) )
               {
                  pBase = zh_hashGetKeyAt( pBase, pEnum->item.asEnum.offset );
                  if( pBase )
                     zh_itemCopy( zh_stackReturnItem(), pBase );
               }
               return &s___msgEnumKey;
            }
            else if( pMsg == s___msgEnumBase.pDynSym )
            {
               if( ZH_IS_BYREF( pEnum->item.asEnum.basePtr ) )
                  zh_itemCopy( zh_stackReturnItem(),
                               zh_itemUnRef( pEnum->item.asEnum.basePtr ) );
               else
                  zh_itemCopy( zh_stackReturnItem(),
                               pEnum->item.asEnum.basePtr );
               if( zh_pcount() > 0 )
                  zh_itemCopy( pEnum->item.asEnum.basePtr,
                               zh_itemUnRef( zh_stackItemFromBase( 1 ) ) );
               return &s___msgEnumBase;
            }
            else if( pMsg == s___msgEnumValue.pDynSym )
            {
               pEnum = zh_itemUnRef( pEnum );
               zh_itemCopy( zh_stackReturnItem(), pEnum );
               if( zh_pcount() > 0 )
                  zh_itemCopy( pEnum, zh_itemUnRef( zh_stackItemFromBase( 1 ) ) );
               return &s___msgEnumValue;
            }
            else if( pMsg == s___msgEnumIsFirst.pDynSym )
            {
               PZH_ITEM pBase = ZH_IS_BYREF( pEnum->item.asEnum.basePtr ) ?
                                zh_itemUnRef( pEnum->item.asEnum.basePtr ) :
                                pEnum->item.asEnum.basePtr;
               if( ZH_IS_OBJECT( pBase ) &&
                   zh_objHasOperator( pBase, ZH_OO_OP_ENUMISFIRST ) )
                  return zh_objGetMethod( pBase, pMessage, pStack );
               zh_itemPutL( zh_stackReturnItem(), ( ZH_SIZE ) pEnum->item.asEnum.offset <= 1 );
               return &s___msgEnumIsFirst;
            }
            else if( pMsg == s___msgEnumIsLast.pDynSym )
            {
               PZH_ITEM pBase = ZH_IS_BYREF( pEnum->item.asEnum.basePtr ) ?
                                zh_itemUnRef( pEnum->item.asEnum.basePtr ) :
                                pEnum->item.asEnum.basePtr;
               if( ZH_IS_ARRAY( pBase ) )
               {
                  if( ZH_IS_OBJECT( pBase ) &&
                      zh_objHasOperator( pBase, ZH_OO_OP_ENUMISLAST ) )
                     return zh_objGetMethod( pBase, pMessage, pStack );
                  else
                     zh_itemPutL( zh_stackReturnItem(), ( ZH_SIZE ) pEnum->item.asEnum.offset >= zh_arrayLen( pBase ) );
               }
               else if( ZH_IS_HASH( pBase ) )
                  zh_itemPutL( zh_stackReturnItem(), ( ZH_SIZE ) pEnum->item.asEnum.offset >= zh_hashLen( pBase ) );
               else if( ZH_IS_STRING( pBase ) )
                  zh_itemPutL( zh_stackReturnItem(), ( ZH_SIZE ) pEnum->item.asEnum.offset >= zh_itemGetCLen( pBase ) );

               return &s___msgEnumIsLast;
            }
         }
      }
   }
   else if( ZH_IS_SYMBOL( pObject ) )
   {
      if( s_uiSymbolClass )
      {
         pClass = s_pClasses[ s_uiSymbolClass ];
         {
            PZH_SYMB pExecSym = zh_clsScalarMethod( pClass, pMsg, pStack );
            if( pExecSym )
               return pExecSym;
         }
      }
      if( pMsg == s___msgExec.pDynSym || pMsg == zh_symEval.pDynSym )
      {
         if( ! pObject->item.asSymbol.value->value.pFunPtr &&
               pObject->item.asSymbol.value->pDynSym )
            return pObject->item.asSymbol.value->pDynSym->pSymbol;
         else
            return pObject->item.asSymbol.value;
      }
      else if( pMsg == s___msgName.pDynSym )
      {
         zh_itemPutC( zh_stackReturnItem(),
                      pObject->item.asSymbol.value->szName );
         return &s___msgName;
      }
   }
   else if( ZH_IS_HASH( pObject ) )
   {
      if( s_uiHashClass )
      {
         pClass = s_pClasses[ s_uiHashClass ];
         {
            PZH_SYMB pExecSym = zh_clsScalarMethod( pClass, pMsg, pStack );
            if( pExecSym )
               return pExecSym;
         }
      }

      if( pMsg == s___msgKeys.pDynSym )
      {
         zh_itemReturnRelease( zh_hashGetKeys( pObject ) );
         return &s___msgKeys;
      }
      else if( pMsg == s___msgValues.pDynSym )
      {
         zh_itemReturnRelease( zh_hashGetValues( pObject ) );
         return &s___msgValues;
      }
#if defined( ZH_HASH_MSG_ITEMS )
      else
      {
         if( zh_pcount() == 1 && pMessage->szName[ 0 ] == '_' ) /* ASSIGN */
         {
            PZH_ITEM pIndex = zh_itemPutCConst( zh_stackAllocItem(), pMessage->szName + 1 );
            PZH_ITEM pDest = zh_hashGetItemPtr( pObject, pIndex, ZH_HASH_AUTOADD_ASSIGN );
            zh_stackPop();
            if( pDest )
            {
               PZH_ITEM pValue = zh_param( 1, ZH_IT_ANY );
               zh_itemCopyFromRef( pDest, pValue );
               zh_itemReturn( pValue );
               return &s___msgVirtual;
            }
         }
         else if( zh_pcount() == 0 ) /* ACCESS */
         {
            PZH_ITEM pIndex = zh_itemPutCConst( zh_stackAllocItem(), pMessage->szName );
            PZH_ITEM pValue = zh_hashGetItemPtr( pObject, pIndex, ZH_HASH_AUTOADD_ACCESS );
            zh_stackPop();
            if( pValue )
            {
               zh_itemReturn( pValue );
               return &s___msgVirtual;
            }
         }
      }
#endif
   }
   else if( ZH_IS_STRING( pObject ) )
   {
      if( s_uiCharacterClass )
      {
         pClass = s_pClasses[ s_uiCharacterClass ];
         {
            PZH_SYMB pExecSym = zh_clsScalarMethod( pClass, pMsg, pStack );
            if( pExecSym )
               return pExecSym;
         }
      }
   }
   else if( ZH_IS_DATE( pObject ) )
   {
      if( s_uiDateClass )
      {
         pClass = s_pClasses[ s_uiDateClass ];
         {
            PZH_SYMB pExecSym = zh_clsScalarMethod( pClass, pMsg, pStack );
            if( pExecSym )
               return pExecSym;
         }
      }
   }
   else if( ZH_IS_TIMESTAMP( pObject ) )
   {
      if( s_uiTimeStampClass )
      {
         pClass = s_pClasses[ s_uiTimeStampClass ];
         {
            PZH_SYMB pExecSym = zh_clsScalarMethod( pClass, pMsg, pStack );
            if( pExecSym )
               return pExecSym;
         }
      }
   }
   else if( ZH_IS_NUMERIC( pObject ) )
   {
      if( s_uiNumericClass )
      {
         pClass = s_pClasses[ s_uiNumericClass ];
         {
            PZH_SYMB pExecSym = zh_clsScalarMethod( pClass, pMsg, pStack );
            if( pExecSym )
               return pExecSym;
         }
      }
   }
   else if( ZH_IS_LOGICAL( pObject ) )
   {
      if( s_uiLogicalClass )
      {
         pClass = s_pClasses[ s_uiLogicalClass ];
         {
            PZH_SYMB pExecSym = zh_clsScalarMethod( pClass, pMsg, pStack );
            if( pExecSym )
               return pExecSym;
         }
      }
   }
   else if( ZH_IS_POINTER( pObject ) )
   {
      if( s_uiPointerClass )
      {
         pClass = s_pClasses[ s_uiPointerClass ];
         {
            PZH_SYMB pExecSym = zh_clsScalarMethod( pClass, pMsg, pStack );
            if( pExecSym )
               return pExecSym;
         }
      }
   }
   else if( ZH_IS_NIL( pObject ) )
   {
      if( s_uiNilClass )
      {
         pClass = s_pClasses[ s_uiNilClass ];
         {
            PZH_SYMB pExecSym = zh_clsScalarMethod( pClass, pMsg, pStack );
            if( pExecSym )
               return pExecSym;
         }
      }
   }

   /* Default messages here */
   if( pMsg == s___msgWithObjectPush.pDynSym )
   {
      if( pStack )
      {
         PZH_ITEM pItem = zh_stackWithObjectItem();
         if( pItem )
         {
            /* push current WITH OBJECT object */
            zh_itemCopy( zh_stackReturnItem(), pItem );
            return &s___msgWithObjectPush;
         }
      }
   }
   else if( pMsg == s___msgWithObjectPop.pDynSym )
   {
      if( pStack )
      {
         PZH_ITEM pItem = zh_stackWithObjectItem();
         if( pItem )
         {
            /* replace current WITH OBJECT object */
            zh_itemCopy( pItem, zh_stackItemFromBase( 1 ) );
            zh_itemCopy( zh_stackReturnItem(), pItem );
            return &s___msgWithObjectPop;
         }
      }
   }
   else if( pMsg == s___msgClassName.pDynSym )
      return &s___msgClassName;

   else if( pMsg == s___msgClassH.pDynSym )
      return &s___msgClassH;

   else if( pMsg == s___msgClassSel.pDynSym )
      return &s___msgClassSel;

/*
   else if( pMsg == s___msgClsParent.pDynSym )
      return &s___msgClsParent;

   else if( pMsg == s___msgClass.pDynSym )
      return &s___msgClass;
 */
   if( pStack )
   {
      if( pClass && pClass->fHasOnError )
      {
         PMETHOD pMethod = zh_clsFindMsg( pClass, s___msgOnError.pDynSym );
         if( pMethod )
         {
            pStack->uiMethod = ( ZH_USHORT ) ( pMethod - pClass->pMethods );
            return pMethod->pFuncSym;
         }
      }

      /* remove this line if you want default ZHVM error message */
      return &s___msgNoMethod;
   }
   return NULL;
}

ZH_BOOL zh_objGetVarRef( PZH_ITEM pObject, PZH_SYMB pMessage,
                         PZH_STACK_STATE pStack )
{
   PZH_SYMB pExecSym;

#if defined( ZH_HASH_MSG_ITEMS )
   if( ZH_IS_HASH( pObject ) )
   {
      ZH_STACK_TLS_PRELOAD
      PZH_ITEM pIndex = zh_itemPutCConst( zh_stackAllocItem(), pMessage->szName + 1 );
      PZH_ITEM pValue = zh_hashGetItemRefPtr( pObject, pIndex );
      zh_stackPop();
      if( pValue )
         zh_itemReturn( pValue );
      return pValue != NULL;
   }
#endif

   pExecSym = zh_objGetMethod( pObject, pMessage, pStack );
   if( pExecSym )
   {
      ZH_STACK_TLS_PRELOAD
      if( pExecSym == &s___msgSetData )
      {
         ZH_USHORT uiObjClass = pObject->item.asArray.value->uiClass;
         PCLASS pClass        = s_pClasses[ pStack->uiClass ];
         PMETHOD pMethod      = pClass->pMethods + pStack->uiMethod;
         ZH_SIZE nIndex       = pMethod->uiData;

         if( pStack->uiClass != uiObjClass )
            nIndex += zh_clsParentInstanceOffset( s_pClasses[ uiObjClass ],
                                                  pMethod->uiSprClass );
         else
            nIndex += pMethod->uiOffset;

         /* will arise only if the class has been modified after first instance */
         if( nIndex > zh_arrayLen( pObject ) ) /* Resize needed */
            zh_arraySize( pObject, nIndex );   /* Make large enough */

         return zh_arrayGetItemRef( pObject, nIndex, zh_stackReturnItem() );
      }
      else if( pExecSym == &s___msgSetClsData )
      {
         PCLASS pClass   = s_pClasses[ pStack->uiClass ];
         PMETHOD pMethod = pClass->pMethods + pStack->uiMethod;

         return zh_arrayGetItemRef( pClass->pClassDatas, pMethod->uiData,
                                    zh_stackReturnItem() );
      }
      else if( pExecSym == &s___msgSetShrData )
      {
         PCLASS pClass   = s_pClasses[ pStack->uiClass ];
         PMETHOD pMethod = pClass->pMethods + pStack->uiMethod;

         return zh_arrayGetItemRef( s_pClasses[ pMethod->uiSprClass ]->pSharedDatas,
                                    pMethod->uiData, zh_stackReturnItem() );
      }
      else if( pExecSym == &s___msgScopeErr )
      {
         pExecSym->value.pFunPtr();
      }
      else
      {
         PCLASS pClass   = s_pClasses[ pStack->uiClass ];
         PMETHOD pMethod = pClass->pMethods + pStack->uiMethod;

         if( pMethod->pMessage == s___msgOnError.pDynSym )
            return zh_vmMsgReference( pObject, pMessage->pDynSym, NULL );

         if( ! pMethod->pAccMsg )
            pMethod->pAccMsg = zh_dynsymGetCase( pMessage->szName + 1 );

         return zh_vmMsgReference( pObject, pMessage->pDynSym, pMethod->pAccMsg );
      }
   }

   return ZH_FALSE;
}

/* Check if class has object destructors */
ZH_BOOL zh_clsHasDestructor( ZH_USHORT uiClass )
{
   if( uiClass && uiClass <= s_uiClasses )
      return s_pClasses[ uiClass ]->fHasDestructor;
   else
      return ZH_FALSE;
}

/* Call all known super destructors */
static void zh_objSuperDestructorCall( PZH_ITEM pObject, PCLASS pClass )
{
#if 0
   ZH_STACK_TLS_PRELOAD
   PMETHOD pMethod = pClass->pMethods;
   ZH_SIZE nLimit = zh_clsMthNum( pClass );
   char * pcClasses;
   ZH_USHORT uiClass;

   pcClasses = ( char * ) zh_xgrabz( ( ZH_SIZE ) s_uiClasses + 1 );

   do
   {
      if( pMethod->pMessage )
      {
         if( pMethod->pFuncSym == &s___msgSuper )
         {
            PCLASS pSuperClass = s_pClasses[ pMethod->uiData ];
            if( pSuperClass->fHasDestructor && pSuperClass != pClass )
               pcClasses[ pMethod->uiData ] |= 1;
         }
         else if( pMethod->pMessage == s___msgDestructor.pDynSym )
            pcClasses[ pMethod->uiSprClass ] |= 2;
      }
      ++pMethod;
   }
   while( --nLimit );

   for( uiClass = s_uiClasses; uiClass; --uiClass )
   {
      if( pcClasses[ uiClass ] == 1 )
      {
         PMETHOD pDestructor = zh_clsFindMsg( s_pClasses[ uiClass ],
                                              s___msgDestructor.pDynSym );
         if( pDestructor )
         {
            if( pcClasses[ pDestructor->uiSprClass ] == 1 )
            {
               zh_vmPushSymbol( &s___msgDestructor );
               zh_clsMakeSuperObject( zh_stackAllocItem(), pObject, uiClass );
               zh_vmSend( 0 );
               if( zh_vmRequestQuery() != 0 )
                  break;
               pcClasses[ pDestructor->uiSprClass ] |= 2;
            }
         }
      }
   }

   zh_xfree( pcClasses );
#else
   ZH_STACK_TLS_PRELOAD
   PMETHOD pDtorMethod = zh_clsFindMsg( pClass, s___msgDestructor.pDynSym );

   if( pDtorMethod )
   {
      ZH_USHORT uiDtorClass = pDtorMethod->uiSprClass;
      ZH_USHORT uiCount = pClass->uiSuperClasses;

      while( uiCount-- )
      {
         ZH_USHORT uiParentCls = pClass->pSuperClasses[ uiCount ].uiClass;

         if( uiParentCls != uiDtorClass && uiParentCls != pClass->uiClass )
         {
            PCLASS pSuperClass = s_pClasses[ uiParentCls ];

            if( pSuperClass->fHasDestructor )
            {
               PMETHOD pDestructor = zh_clsFindMsg( s_pClasses[ uiParentCls ],
                                                    s___msgDestructor.pDynSym );
               if( pDestructor && pDestructor->uiSprClass == uiParentCls )
               {
                  zh_vmPushSymbol( &s___msgDestructor );
                  zh_clsMakeSuperObject( zh_stackAllocItem(), pObject, uiParentCls );
                  zh_vmSend( 0 );
                  if( zh_vmRequestQuery() != 0 )
                     break;
               }
            }
         }
      }
   }
#endif
}

/* Call object destructor */
void zh_objDestructorCall( PZH_ITEM pObject )
{
   if( ZH_IS_OBJECT( pObject ) &&
       pObject->item.asArray.value->uiClass <= s_uiClasses )
   {
      PCLASS pClass = s_pClasses[ pObject->item.asArray.value->uiClass ];

      if( pClass->fHasDestructor )
      {
         if( zh_vmRequestReenter() )
         {
            zh_vmPushSymbol( &s___msgDestructor );
            zh_vmPush( pObject );
            zh_vmSend( 0 );
            if( zh_vmRequestQuery() == 0 )
               zh_objSuperDestructorCall( pObject, pClass );
            zh_vmRequestRestore();
         }
      }
   }
}

/* Check if object has a given operator */
ZH_BOOL zh_objHasOperator( PZH_ITEM pObject, ZH_USHORT uiOperator )
{
   ZH_USHORT uiClass;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_objHasOperator(%p,%hu)", ( void * ) pObject, uiOperator ) );

   uiClass = zh_objGetClassH( pObject );
   if( uiClass && uiClass <= s_uiClasses )
   {
      return ( s_pClasses[ uiClass ]->nOpFlags & ( 1 << uiOperator ) ) != 0;
   }

   return ZH_FALSE;
}

/* Call object operator. If pMsgArg is NULL then operator is unary.
 * Function return ZH_TRUE when object class overloads given operator
 * and ZH_FALSE otherwise. [druzus]
 */
ZH_BOOL zh_objOperatorCall( ZH_USHORT uiOperator, PZH_ITEM pResult, PZH_ITEM pObject,
                            PZH_ITEM pMsgArg1, PZH_ITEM pMsgArg2 )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_objOperatorCall(%hu,%p,%p,%p,%p)", uiOperator, ( void * ) pResult, ( void * ) pObject, ( void * ) pMsgArg1, ( void * ) pMsgArg2 ) );

   if( zh_objHasOperator( pObject, uiOperator ) )
   {
      ZH_STACK_TLS_PRELOAD
      zh_vmPushSymbol( s_opSymbols + uiOperator );
      zh_vmPush( pObject );
      zh_itemSetNil( zh_stackReturnItem() );
      if( pMsgArg1 )
      {
         zh_vmPush( pMsgArg1 );
         if( pMsgArg2 )
         {
            zh_vmPush( pMsgArg2 );
            zh_vmSend( 2 );
         }
         else
            zh_vmSend( 1 );
      }
      else
         zh_vmSend( 0 );

      /* store the return value */
      zh_itemMove( pResult, zh_stackReturnItem() );
      return ZH_TRUE;
   }
   return ZH_FALSE;
}

/* return ZH_TRUE if object has a given message */
ZH_BOOL zh_objHasMessage( PZH_ITEM pObject, PZH_DYNS pMessage )
{
   return zh_objGetMethod( pObject, pMessage->pSymbol, NULL ) != NULL;
}

/* <bool> = zh_objHasMsg( <pObject>, <szString> )
 *
 * Check whether <szString> is an existing message for object.
 *
 * <uPtr> should be read as a boolean
 */
ZH_BOOL zh_objHasMsg( PZH_ITEM pObject, const char * szString )
{
   PZH_DYNS pDynSym;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_objHasMsg(%p, %s)", ( void * ) pObject, szString ) );

   pDynSym = zh_dynsymFindName( szString );
   if( pDynSym )
      return zh_objGetMethod( pObject, pDynSym->pSymbol, NULL ) != NULL;
   else
      return ZH_FALSE;
}

PZH_ITEM zh_objSendMessage( PZH_ITEM pObject, PZH_DYNS pMsgSym, ZH_ULONG ulArg, ... )
{
   if( pObject && pMsgSym )
   {
      zh_vmPushSymbol( pMsgSym->pSymbol );
      zh_vmPush( pObject );

      if( ulArg )
      {
         ZH_ULONG i;
         va_list ap;

         va_start( ap, ulArg );
         for( i = 0; i < ulArg; i++ )
         {
            zh_vmPush( va_arg( ap, PZH_ITEM ) );
         }
         va_end( ap );
      }
      zh_vmSend( ( ZH_USHORT ) ulArg );
   }
   else
      zh_errRT_BASE( EG_ARG, 3000, NULL, "__objSendMessage()", 0 );

   {
      ZH_STACK_TLS_PRELOAD
      return zh_stackReturnItem();
   }
}

PZH_ITEM zh_objSendMsg( PZH_ITEM pObject, const char * szMsg, ZH_ULONG ulArg, ... )
{
   zh_vmPushSymbol( zh_dynsymGet( szMsg )->pSymbol );
   zh_vmPush( pObject );
   if( ulArg )
   {
      ZH_ULONG i;
      va_list ap;

      va_start( ap, ulArg );
      for( i = 0; i < ulArg; i++ )
      {
         zh_vmPush( va_arg( ap, PZH_ITEM ) );
      }
      va_end( ap );
   }
   zh_vmSend( ( ZH_USHORT ) ulArg );

   {
      ZH_STACK_TLS_PRELOAD
      return zh_stackReturnItem();
   }
}

PZH_ITEM zh_objGetVarPtr( PZH_ITEM pObject, PZH_DYNS pVarMsg )
{
   if( pObject && ZH_IS_OBJECT( pObject ) && pVarMsg )
   {
      ZH_USHORT uiClass = pObject->item.asArray.value->uiClass;
      PCLASS pClass = s_pClasses[ uiClass ];
      PMETHOD pMethod = zh_clsFindMsg( pClass, pVarMsg );

      if( pMethod )
      {
         PZH_SYMB pFuncSym = pMethod->pFuncSym;

         if( pFuncSym == &s___msgSync || pFuncSym == &s___msgSyncClass )
            pFuncSym = pMethod->pRealSym;

         if( pFuncSym == &s___msgSetData || pFuncSym == &s___msgGetData )
         {
            ZH_SIZE nIndex = pMethod->uiData + pMethod->uiOffset;
            if( pObject->item.asArray.value->uiPrevCls )
            {
               pObject = zh_arrayGetItemPtr( pObject, 1 );
               if( ! pObject )
                  return NULL;
               if( uiClass != pObject->item.asArray.value->uiClass )
                  nIndex = pMethod->uiData +
                           zh_clsParentInstanceOffset( s_pClasses[ pObject->item.asArray.value->uiClass ],
                                                       pMethod->uiSprClass );
            }
            return zh_arrayGetItemPtr( pObject, nIndex );
         }
      }
   }
   return NULL;
}

static PZH_DYNS zh_objGetMsgSym( PZH_ITEM pMessage )
{
   PZH_DYNS pDynSym = NULL;

   if( pMessage )
   {
      const char * szMsg = NULL;

      if( ZH_IS_STRING( pMessage ) )
         szMsg = pMessage->item.asString.value;
      else if( ZH_IS_SYMBOL( pMessage ) )
      {
         pDynSym = pMessage->item.asSymbol.value->pDynSym;
         if( ! pDynSym )
            szMsg = pMessage->item.asSymbol.value->szName;
      }

      if( szMsg && *szMsg )
         pDynSym = zh_dynsymGet( szMsg );
   }

   return pDynSym;
}

static PZH_SYMB zh_objGetFuncSym( PZH_ITEM pItem )
{
   if( pItem )
   {
      if( ZH_IS_SYMBOL( pItem ) )
         return pItem->item.asSymbol.value;
      else if( ZH_IS_STRING( pItem ) )
      {
         PZH_DYNS pDynSym = zh_dynsymFindName( zh_itemGetCPtr( pItem ) );

         if( pDynSym && pDynSym->pSymbol->value.pFunPtr )
            return pDynSym->pSymbol;
      }
   }

   return NULL;
}

/* clone object if user defined clone method or copy it */
void zh_objCloneBody( PZH_ITEM pDest, PZH_ITEM pObject, PZH_NESTED_CLONED pClonedList )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_objCloneBody(%p,%p,%p)", ( void * ) pDest, ( void * ) pObject, ( void * ) pClonedList ) );

   ZH_SYMBOL_UNUSED( pClonedList );

   /* TODO: add support for user defined clone operation */

   zh_itemCopy( pDest, pObject );
}

/* clone object if user defined clone method or copy it */
PZH_ITEM zh_objCloneTo( PZH_ITEM pDest, PZH_ITEM pObject )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_objCloneTo(%p,%p)", ( void * ) pDest, ( void * ) pObject ) );

   zh_objCloneBody( pDest, pObject, NULL );

   return pDest;
}

/* send message which allows to set execution context for debugger */
void zh_dbg_objSendMessage( int iProcLevel, PZH_ITEM pObject, PZH_ITEM pMessage, int iParamOffset )
{
   ZH_STACK_TLS_PRELOAD
   PZH_DYNS pMsgSym;

   pMsgSym = zh_objGetMsgSym( pMessage );
   if( pObject && pMsgSym )
   {
      ZH_USHORT uiParams = 0;

      /* set requested sender class and method id for scope verification */
      if( iProcLevel > 0 )
      {
         int iLevel = zh_stackCallDepth();
         if( iProcLevel < iLevel )
         {
            ZH_ISIZ nOffset = zh_stackBaseProcOffset( iLevel - iProcLevel );
            if( nOffset > 0 )
            {
               PZH_ITEM pItem = zh_stackItem( nOffset );
               PZH_ITEM pBase = zh_stackBaseItem();
               pBase->item.asSymbol.stackstate->uiClass =
                                    pItem->item.asSymbol.stackstate->uiClass;
               pBase->item.asSymbol.stackstate->uiMethod =
                                    pItem->item.asSymbol.stackstate->uiMethod;
            }
         }
      }
      else if( iProcLevel == 0 )
      {
         /* set scope like for internal object messages to any visible
            method without respecting overloaded methods */
         ZH_USHORT uiClass = zh_objGetClassH( pObject );

         if( uiClass && uiClass <= s_uiClasses )
         {
            PMETHOD pMethod = zh_clsFindMsg( s_pClasses[ uiClass ], pMsgSym );
            if( pMethod )
            {
               PZH_ITEM pBase = zh_stackBaseItem();

               pBase->item.asSymbol.stackstate->uiClass = uiClass;
               pBase->item.asSymbol.stackstate->uiMethod =
                     ( ZH_USHORT ) ( pMethod - s_pClasses[ uiClass ]->pMethods );
            }
         }
      }

      zh_vmPushSymbol( pMsgSym->pSymbol );
      zh_vmPush( pObject );

      if( iParamOffset > 0 )
      {
         int iPCount = zh_pcount();

         while( iParamOffset <= iPCount )
         {
            zh_vmPush( zh_stackItemFromBase( iParamOffset ) );
            ++uiParams;
            ++iParamOffset;
         }
      }
      zh_vmSend( uiParams );
   }
   else
      zh_errRT_BASE( EG_ARG, 3000, NULL, "zh_dbg_objSendMessage()", 2, pObject, pMsgSym );
}

static ZH_USHORT zh_clsUpdateScope( ZH_USHORT uiScope, ZH_BOOL fAssign )
{
   if( ! fAssign )
      uiScope &= ~ZH_OO_CLSTP_READONLY;
   else
   {
      uiScope &= ~ZH_OO_CLSTP_PERSIST;

      if( ( uiScope & ZH_OO_CLSTP_READONLY ) &&
         !( uiScope & ZH_OO_CLSTP_HIDDEN ) )
      {
         /* Class(y) does not allow to write to HIDDEN+READONLY
            instance variables, [druzus] */

         uiScope &= ~ZH_OO_CLSTP_READONLY;
         uiScope |= ( uiScope & ZH_OO_CLSTP_PROTECTED ) ?
                       ZH_OO_CLSTP_HIDDEN : ZH_OO_CLSTP_PROTECTED;
      }
   }
   return uiScope;
}

static ZH_TYPE zh_clsGetItemType( PZH_ITEM pItem, ZH_TYPE nDefault )
{
   if( pItem )
   {
      if( ZH_IS_STRING( pItem ) )
      {
         switch( zh_itemGetCPtr( pItem )[ 0 ] )
         {
            case 'C':
            case 'c':
               if( zh_strnicmp( zh_itemGetCPtr( pItem ), "code", 4 ) == 0 )
                  return ZH_IT_BLOCK;
               /* fallthrough */
            case '\0':
               return ZH_IT_STRING;

            case 'S':
            case 's':
               if( zh_strnicmp( zh_itemGetCPtr( pItem ), "str", 3 ) == 0 )
                  return ZH_IT_STRING;
               else
                  return ZH_IT_SYMBOL;

            case 'B':
            case 'b':
               return ZH_IT_BLOCK;

            case 'D':
            case 'd':
               if( zh_strnicmp( zh_itemGetCPtr( pItem ), "datet", 5 ) == 0 )
                  return ZH_IT_TIMESTAMP;
               else
                  return ZH_IT_DATE;

            case 'T':
            case 't':
               return ZH_IT_TIMESTAMP;

            case 'L':
            case 'l':
               return ZH_IT_LOGICAL;

            case 'I':
            case 'i':
               return ZH_IT_NUMINT;

            case 'N':
            case 'n':
               if( zh_stricmp( zh_itemGetCPtr( pItem ), "nil" ) == 0 )
                  return ZH_IT_NIL;
               else
                  return ZH_IT_NUMERIC;

            case 'A':
            case 'a':
               return ZH_IT_ARRAY;

            case 'P':
            case 'p':
               return ZH_IT_POINTER;

            case 'H':
            case 'h':
               return ZH_IT_HASH;
         }
      }
      else if( ZH_IS_ARRAY( pItem ) )
      {
         if( pItem->item.asArray.value->uiClass == 0 )
            return ZH_IT_ARRAY;
      }
      else if( ZH_IS_NUMINT( pItem ) )
         return ZH_IT_NUMINT;

      else if( ZH_IS_NUMERIC( pItem ) )
         return ZH_IT_NUMERIC;

      else if( ZH_IS_DATE( pItem ) )
         return ZH_IT_DATE;

      else if( ZH_IS_TIMESTAMP( pItem ) )
         return ZH_IT_TIMESTAMP;

      else if( ZH_IS_LOGICAL( pItem ) )
         return ZH_IT_LOGICAL;

      else if( ZH_IS_BLOCK( pItem ) )
         return ZH_IT_BLOCK;

      else if( ZH_IS_POINTER( pItem ) )
         return ZH_IT_POINTER;

      else if( ZH_IS_SYMBOL( pItem ) )
         return ZH_IT_SYMBOL;

      else if( ZH_IS_NIL( pItem ) )
         return ZH_IT_NIL;
   }

   return nDefault;
}

/* --- */

/*
 * <uiType>    ZH_OO_MSG_METHOD     : standard method
 *             ZH_OO_MSG_ONERROR    : error handler method
 *             ZH_OO_MSG_DESTRUCTOR : destructor method
 *             ZH_OO_MSG_INLINE     : inline (codeblock) method
 *             ZH_OO_MSG_ASSIGN     : assign instance data
 *             ZH_OO_MSG_ACCESS     : access instance data
 *             ZH_OO_MSG_CLSASSIGN  : assign class data
 *             ZH_OO_MSG_CLSACCESS  : access class data
 *             ZH_OO_MSG_SUPER      : supercasting
 *             ZH_OO_MSG_REALCLASS  : caller method real class casting
 *             ZH_OO_MSG_PERFORM    : perform method
 *             ZH_OO_MSG_VIRTUAL    : virtual method
 *             ZH_OO_MSG_DELEGATE   : delegate method
 *
 * <uiScope>   ZH_OO_CLSTP_EXPORTED        1 : default for data and method
 *             ZH_OO_CLSTP_PROTECTED       2 : method or data protected
 *             ZH_OO_CLSTP_HIDDEN          4 : method or data hidden
 *           * ZH_OO_CLSTP_CTOR            8 : method constructor
 *             ZH_OO_CLSTP_READONLY       16 : data read only
 *             ZH_OO_CLSTP_SHARED         32 : (method or) data shared
 *             ZH_OO_CLSTP_CLASS          64 : message is class message not object
 *           * ZH_OO_CLSTP_SUPER         128 : message is inherited
 *             ZH_OO_CLSTP_PERSIST       256 : message is persistent (PROPERTY)
 *             ZH_OO_CLSTP_NONVIRTUAL    512 : Non Virtual message - should not be covered by subclass(es) messages with the same name when executed from a given class message
 *             ZH_OO_CLSTP_OVERLOADED   1024 : message overload NONVIRTUAL one
 *             ZH_OO_CLSTP_SYNC         2048 : message synchronized by object or class mutex
 *
 * <pFunction> ZH_OO_MSG_METHOD     : \
 *             ZH_OO_MSG_ONERROR    :  > Pointer to function
 *             ZH_OO_MSG_DESTRUCTOR : /
 *             ZH_OO_MSG_INLINE     : Code block
 *             ZH_OO_MSG_ASSIGN     :  Index to instance area array 1 based (without offset)
 *             ZH_OO_MSG_ACCESS     : /
 *             ZH_OO_MSG_CLSASSIGN  :  Index class data array
 *             ZH_OO_MSG_CLSACCESS  : /
 *             ZH_OO_MSG_SUPER      : Instance area offset for class casting
 *             ZH_OO_MSG_DELEGATE   : Delegated message symbol
 *
 * <pInit>     ZH_OO_MSG_ACCESS     :  Optional initializer for (Class)DATA
 *             ZH_OO_MSG_CLSACCESS  : /
 *             ZH_OO_MSG_ASSIGN     :  Item type restriction in assignment
 *             ZH_OO_MSG_CLSASSIGN  : /
 *             ZH_OO_MSG_SUPER      : Superclass handle
 *             ZH_OO_MSG_DELEGATE   : Object symbol for delegated message
 */
static ZH_BOOL zh_clsAddMsg( ZH_USHORT uiClass, const char * szMessage,
                             ZH_USHORT uiType, ZH_USHORT uiScope,
                             PZH_ITEM pFunction, PZH_ITEM pInit )
{
   if( szMessage && uiClass && uiClass <= s_uiClasses )
   {
      PCLASS    pClass   = s_pClasses[ uiClass ];

      PZH_DYNS  pMessage;
      PMETHOD   pNewMeth;
      ZH_USHORT uiOperator, uiSprClass = 0, uiIndex = 0, uiPrevCls, uiPrevMth;
      PZH_SYMB  pOpSym, pFuncSym = NULL;
      ZH_BOOL   fOK;
      ZH_U32    nOpFlags = 0;

      if( pClass->fLocked )
         return ZH_FALSE;

      if( ! ( uiScope & ( ZH_OO_CLSTP_EXPORTED | ZH_OO_CLSTP_PROTECTED | ZH_OO_CLSTP_HIDDEN ) ) )
         uiScope |= ZH_OO_CLSTP_EXPORTED;

      /* translate names of operator overloading messages */
      if( uiType == ZH_OO_MSG_DESTRUCTOR )
         pMessage = s___msgDestructor.pDynSym;
      else if( uiType == ZH_OO_MSG_ONERROR )
         pMessage = s___msgOnError.pDynSym;
      else if( strcmp( "+", szMessage ) == 0 )
         pMessage = ( s_opSymbols + ZH_OO_OP_PLUS )->pDynSym;
      else if( strcmp( "-", szMessage ) == 0 )
         pMessage = ( s_opSymbols + ZH_OO_OP_MINUS )->pDynSym;
      else if( strcmp( "*", szMessage ) == 0 )
         pMessage = ( s_opSymbols + ZH_OO_OP_MULT )->pDynSym;
      else if( strcmp( "/", szMessage ) == 0 )
         pMessage = ( s_opSymbols + ZH_OO_OP_DIVIDE )->pDynSym;
      else if( strcmp( "%", szMessage ) == 0 )
         pMessage = ( s_opSymbols + ZH_OO_OP_MOD )->pDynSym;
      else if( strcmp( "^", szMessage ) == 0 )
         pMessage = ( s_opSymbols + ZH_OO_OP_POWER )->pDynSym;
      else if( strcmp( "**", szMessage ) == 0 )
         pMessage = ( s_opSymbols + ZH_OO_OP_POWER )->pDynSym;
      else if( strcmp( "++", szMessage ) == 0 )
         pMessage = ( s_opSymbols + ZH_OO_OP_INC )->pDynSym;
      else if( strcmp( "--", szMessage ) == 0 )
         pMessage = ( s_opSymbols + ZH_OO_OP_DEC )->pDynSym;
      else if( strcmp( "==", szMessage ) == 0 )
         pMessage = ( s_opSymbols + ZH_OO_OP_EXACTEQUAL )->pDynSym;
      else if( strcmp( "=", szMessage ) == 0 )
         pMessage = ( s_opSymbols + ZH_OO_OP_EQUAL )->pDynSym;
      else if( strcmp( "!=", szMessage ) == 0 )
         pMessage = ( s_opSymbols + ZH_OO_OP_NOTEQUAL )->pDynSym;
      else if( strcmp( "<>", szMessage ) == 0 )
         pMessage = ( s_opSymbols + ZH_OO_OP_NOTEQUAL )->pDynSym;
      else if( strcmp( "#", szMessage ) == 0 )
         pMessage = ( s_opSymbols + ZH_OO_OP_NOTEQUAL )->pDynSym;
      else if( strcmp( "<", szMessage ) == 0 )
         pMessage = ( s_opSymbols + ZH_OO_OP_LESS )->pDynSym;
      else if( strcmp( "<=", szMessage ) == 0 )
         pMessage = ( s_opSymbols + ZH_OO_OP_LESSEQUAL )->pDynSym;
      else if( strcmp( ">", szMessage ) == 0 )
         pMessage = ( s_opSymbols + ZH_OO_OP_GREATER )->pDynSym;
      else if( strcmp( ">=", szMessage ) == 0 )
         pMessage = ( s_opSymbols + ZH_OO_OP_GREATEREQUAL )->pDynSym;
      else if( strcmp( ":=", szMessage ) == 0 )
         pMessage = ( s_opSymbols + ZH_OO_OP_ASSIGN )->pDynSym;
      else if( strcmp( "$", szMessage ) == 0 )
         pMessage = ( s_opSymbols + ZH_OO_OP_INSTRING )->pDynSym;
      else if( strcmp( "$$", szMessage ) == 0 )
         pMessage = ( s_opSymbols + ZH_OO_OP_INCLUDE )->pDynSym;
      else if( strcmp( "!", szMessage ) == 0 )
         pMessage = ( s_opSymbols + ZH_OO_OP_NOT )->pDynSym;
      else if( zh_stricmp( ".NOT.", szMessage ) == 0 )
         pMessage = ( s_opSymbols + ZH_OO_OP_NOT )->pDynSym;
      else if( zh_stricmp( ".AND.", szMessage ) == 0 )
         pMessage = ( s_opSymbols + ZH_OO_OP_AND )->pDynSym;
      else if( zh_stricmp( ".OR.", szMessage ) == 0 )
         pMessage = ( s_opSymbols + ZH_OO_OP_OR )->pDynSym;
      else if( strcmp( "[]", szMessage ) == 0 )
         pMessage = ( s_opSymbols + ZH_OO_OP_ARRAYINDEX )->pDynSym;
      else
         pMessage = zh_dynsymGet( szMessage );

      for( uiOperator = 0, pOpSym = s_opSymbols;
           uiOperator <= ZH_OO_MAX_OPERATOR; ++uiOperator, ++pOpSym )
      {
         if( pOpSym->pDynSym == pMessage )
         {
            nOpFlags |= 1 << uiOperator;
            break;
         }
      }

      /* basic parameter validation */
      switch( uiType )
      {
         case ZH_OO_MSG_METHOD:
         case ZH_OO_MSG_ONERROR:
         case ZH_OO_MSG_DESTRUCTOR:
            pFuncSym = zh_objGetFuncSym( pFunction );
            fOK = pFuncSym != NULL;
            break;

         case ZH_OO_MSG_INLINE:
            fOK = pFunction && ZH_IS_BLOCK( pFunction );
            break;

         case ZH_OO_MSG_SUPER:
            uiIndex = ( ZH_USHORT ) zh_itemGetNI( pFunction );
            uiSprClass = ( ZH_USHORT ) zh_itemGetNI( pInit );
            fOK = uiSprClass && uiSprClass <= s_uiClasses &&
                  uiIndex <= pClass->uiDatas;
            break;

         case ZH_OO_MSG_ASSIGN:
         case ZH_OO_MSG_ACCESS:
            uiIndex = ( ZH_USHORT ) zh_itemGetNI( pFunction );
            /* This validation can break buggy .zh code which wrongly
             * sets data offsets but IMHO it will help to clean the code.
             * [druzus]
             */
            fOK = uiIndex && uiIndex <= pClass->uiDatas - pClass->uiDataFirst;
            break;

         case ZH_OO_MSG_CLSASSIGN:
         case ZH_OO_MSG_CLSACCESS:
            uiIndex = ( ZH_USHORT ) zh_itemGetNI( pFunction );
            fOK = uiIndex != 0;
            break;

         case ZH_OO_MSG_DELEGATE:
         {
            PZH_DYNS pDelegMsg = zh_objGetMsgSym( pFunction );
            if( pDelegMsg )
            {
               pNewMeth = zh_clsFindMsg( pClass, pDelegMsg );
               if( pNewMeth )
                  uiIndex = ( ZH_USHORT ) ( pNewMeth - pClass->pMethods );
            }
            fOK = pFunction == NULL || ZH_IS_NIL( pFunction ) || uiIndex != 0;
            if( fOK )
            {
               pDelegMsg = zh_objGetMsgSym( pInit );
               if( pDelegMsg )
               {
                  pNewMeth = zh_clsFindMsg( pClass, pDelegMsg );
                  if( pNewMeth )
                     uiSprClass = ( ZH_USHORT ) ( pNewMeth - pClass->pMethods );
               }
               fOK = ( pInit == NULL || ZH_IS_NIL( pInit ) || uiSprClass != 0 ) &&
                     ( uiIndex != 0 || uiSprClass != 0 );
            }
            break;
         }
         case ZH_OO_MSG_REALCLASS:
         case ZH_OO_MSG_VIRTUAL:
         case ZH_OO_MSG_PERFORM:
            fOK = ZH_TRUE;
            break;

         default:
            fOK = ZH_FALSE;
      }

      if( ! fOK )
      {
         zh_errRT_BASE( EG_ARG, 3000, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
         return ZH_FALSE;
      }

      pNewMeth = zh_clsAllocMsg( pClass, pMessage );

      uiPrevCls = uiClass;
      uiPrevMth = ( ZH_USHORT ) ( pClass->pMethods - pNewMeth );

#ifndef ZH_VIRTUAL_HIDDEN
      if( uiScope & ZH_OO_CLSTP_HIDDEN )
         uiScope |= ZH_OO_CLSTP_NONVIRTUAL;
#endif

      if( ! pNewMeth->pMessage )
         pClass->uiMethods++;           /* One more message */
      else
      {
         ZH_BOOL fOverLoad = ( pNewMeth->uiScope & ZH_OO_CLSTP_OVERLOADED ) ||
                             ( ( pNewMeth->uiScope & ZH_OO_CLSTP_NONVIRTUAL ) &&
                               pNewMeth->uiSprClass != uiClass );

         uiPrevCls = pNewMeth->uiPrevCls;
         uiPrevMth = pNewMeth->uiPrevMth;
         if( ! zh_clsCanClearMethod( pNewMeth, ZH_TRUE ) )
            return ZH_FALSE;

         memset( pNewMeth, 0, sizeof( METHOD ) );
         if( fOverLoad )
            uiScope |= ZH_OO_CLSTP_OVERLOADED;
      }
      pNewMeth->pMessage = pMessage;
      pNewMeth->uiSprClass = uiClass;
      pNewMeth->uiPrevCls = uiPrevCls;
      pNewMeth->uiPrevMth = uiPrevMth;

      switch( uiType )
      {
         case ZH_OO_MSG_METHOD:

            pNewMeth->pFuncSym = pFuncSym;
            pNewMeth->uiScope = uiScope;
            break;

         case ZH_OO_MSG_ASSIGN:

            pNewMeth->uiScope = zh_clsUpdateScope( uiScope, ZH_TRUE );
            /* Class(y) does not allow to write to HIDDEN+READONLY
               instance variables, [druzus] */
            if( pNewMeth->uiScope & ZH_OO_CLSTP_READONLY &&
                pNewMeth->uiScope & ZH_OO_CLSTP_HIDDEN )
               pNewMeth->pFuncSym = &s___msgScopeErr;
            else
            {
               pNewMeth->pFuncSym = &s___msgSetData;
               pNewMeth->uiData = uiIndex;
               pNewMeth->uiOffset = pClass->uiDataFirst;
               pNewMeth->itemType = zh_clsGetItemType( pInit, 0 );
            }
            break;

         case ZH_OO_MSG_ACCESS:

            pNewMeth->uiScope = zh_clsUpdateScope( uiScope, ZH_FALSE );
            pNewMeth->uiData = uiIndex;
            pNewMeth->uiOffset = pClass->uiDataFirst;
            zh_clsAddInitValue( pClass, pInit, ZH_OO_MSG_DATA,
                                pNewMeth->uiData, pNewMeth->uiOffset, uiClass );
            pNewMeth->pFuncSym = &s___msgGetData;
            break;

         case ZH_OO_MSG_CLSASSIGN:

            pNewMeth->uiData = uiIndex;
            pNewMeth->itemType = zh_clsGetItemType( pInit, 0 );
            pNewMeth->uiScope = zh_clsUpdateScope( uiScope, ZH_TRUE );
            /* Class(y) does not allow to write to HIDDEN+READONLY
               instance variables, [druzus] */
            if( pNewMeth->uiScope & ZH_OO_CLSTP_READONLY &&
                pNewMeth->uiScope & ZH_OO_CLSTP_HIDDEN )
               pNewMeth->pFuncSym = &s___msgScopeErr;
            else if( pNewMeth->uiScope & ZH_OO_CLSTP_SHARED )
            {
               if( zh_arrayLen( pClass->pSharedDatas ) < ( ZH_SIZE ) pNewMeth->uiData )
                  zh_arraySize( pClass->pSharedDatas, pNewMeth->uiData );
               pNewMeth->pFuncSym = &s___msgSetShrData;
            }
            else
            {
               if( zh_arrayLen( pClass->pClassDatas ) < ( ZH_SIZE ) pNewMeth->uiData )
                  zh_arraySize( pClass->pClassDatas, pNewMeth->uiData );
               pNewMeth->pFuncSym = &s___msgSetClsData;
            }
            break;

         case ZH_OO_MSG_CLSACCESS:

            pNewMeth->uiScope = zh_clsUpdateScope( uiScope, ZH_FALSE );
            pNewMeth->uiData = uiIndex;
            if( pNewMeth->uiScope & ZH_OO_CLSTP_SHARED )
            {
               if( zh_arrayLen( pClass->pSharedDatas ) < ( ZH_SIZE ) pNewMeth->uiData )
                  zh_arraySize( pClass->pSharedDatas, pNewMeth->uiData );

               if( pInit && ! ZH_IS_NIL( pInit ) ) /* Initializer found */
               {
                  /* Shared Classdata need to be initialized only once
                   * ACCESS/ASSIGN methods will be inherited by subclasses
                   * and will operate on this value so it's not necessary
                   * to keep the init value. [druzus]
                   */
                  zh_itemCloneTo( zh_arrayGetItemPtr( pClass->pSharedDatas,
                                                      pNewMeth->uiData ), pInit );
               }
               pNewMeth->pFuncSym = &s___msgGetShrData;
            }
            else
            {
               if( zh_arrayLen( pClass->pClassDatas ) < ( ZH_SIZE ) pNewMeth->uiData )
                  zh_arraySize( pClass->pClassDatas, pNewMeth->uiData );
               /* uiOffset is used to copy ancestor class data initializers when
                * new class is created
                */
               pNewMeth->uiOffset = zh_clsAddInitValue( pClass, pInit,
                           ZH_OO_MSG_CLASSDATA, pNewMeth->uiData, 0, uiClass );
               pNewMeth->pFuncSym = &s___msgGetClsData;
            }
            break;

         case ZH_OO_MSG_INLINE:

            pNewMeth->pFuncSym = &s___msgEvalInline;
            pNewMeth->uiScope = uiScope;
            zh_arrayAdd( pClass->pInlines, pFunction );
            pNewMeth->uiData = ( ZH_USHORT ) zh_arrayLen( pClass->pInlines );
            break;

         case ZH_OO_MSG_VIRTUAL:

            pNewMeth->pFuncSym = &s___msgVirtual;
            pNewMeth->uiScope = uiScope;
            break;

         case ZH_OO_MSG_SUPER:

            pNewMeth->uiData = uiSprClass; /* store the super handle */
            pNewMeth->uiOffset = uiIndex; /* offset to instance area */
            pNewMeth->uiScope = uiScope;
            pNewMeth->pFuncSym = &s___msgSuper;
            break;

         case ZH_OO_MSG_REALCLASS:

            pNewMeth->pFuncSym = &s___msgRealClass;
            pNewMeth->uiScope = uiScope;
            break;

         case ZH_OO_MSG_PERFORM:
            pNewMeth->pFuncSym = &s___msgPerform;
            pNewMeth->uiScope = uiScope;
            break;

         case ZH_OO_MSG_DELEGATE:

            pNewMeth->pFuncSym = &s___msgDelegate;
            pNewMeth->uiScope = uiScope;
            pNewMeth->uiData = uiIndex;
            break;

         case ZH_OO_MSG_ONERROR:

            pNewMeth->pFuncSym = pFuncSym;
            pClass->fHasOnError = ZH_TRUE;
            break;

         case ZH_OO_MSG_DESTRUCTOR:

            pNewMeth->pFuncSym = pFuncSym;
            pClass->fHasDestructor = ZH_TRUE;
            break;

         default:

            zh_errInternal( ZH_EI_CLSINVMETHOD, NULL, "__clsAddMsg()", NULL );
      }

      pClass->nOpFlags |= nOpFlags;

      if( uiScope & ZH_OO_CLSTP_SYNC )
      {
         pNewMeth->pRealSym = pNewMeth->pFuncSym;
         if( uiScope & ZH_OO_CLSTP_CLASS )
         {
            if( ! pClass->pMutex )
               pClass->pMutex = zh_threadMutexCreate();
            pNewMeth->pFuncSym = &s___msgSyncClass;
         }
         else
         {
            if( ! pClass->uiMutexOffset )
               pClass->uiMutexOffset = pClass->uiDatas + 1;
            pNewMeth->pFuncSym = &s___msgSync;
         }
      }
   }

   return ZH_TRUE;
}

/* __clsAddMsg( <hClass>, <cMessage>, <pFunction>, <nType>, [xInit], <uiScope>, [xType] )
 *
 * Add a message to the class.
 *
 * <hClass>    Class handle
 * <cMessage>  Message
 * <pFunction> ZH_OO_MSG_METHOD     : \
 *             ZH_OO_MSG_ONERROR    :  > Pointer to function
 *             ZH_OO_MSG_DESTRUCTOR : /
 *             ZH_OO_MSG_INLINE     : Code block
 *             ZH_OO_MSG_DATA       : \
 *             ZH_OO_MSG_ASSIGN     :  > Index to instance area array
 *             ZH_OO_MSG_ACCESS     : /
 *             ZH_OO_MSG_CLASSDATA  : \
 *             ZH_OO_MSG_CLSASSIGN  :  > Index class data array
 *             ZH_OO_MSG_CLSACCESS  : /
 *             ZH_OO_MSG_SUPER      : Handle of super class
 *             ZH_OO_MSG_DELEGATE   : delegated message symbol
 *
 * <nType>     see ZH_OO_MSG_* above and:
 *             ZH_OO_MSG_REALCLASS  : caller method real class casting
 *             ZH_OO_MSG_PERFORM    : perform message
 *             ZH_OO_MSG_VIRTUAL    : virtual message
 *             ZH_OO_MSG_DELEGATE   : delegate method
 *
 * <xInit>     ZH_OO_MSG_ACCESS     : \
 *             ZH_OO_MSG_CLSACCESS  :   > Optional initializer for DATA
 *             ZH_OO_MSG_DATA       :  /
 *             ZH_OO_MSG_CLASSDATA  : /
 *             ZH_OO_MSG_SUPER      : Superclass handle
 *             ZH_OO_MSG_ASSIGN     : \ item type restriction in assignment not
 *             ZH_OO_MSG_CLSASSIGN: :   empty character value where first letter
 *                                      is item type or item of a given value
 *
 * <uiScope>   ZH_OO_CLSTP_EXPORTED        1 : default for data and method
 *             ZH_OO_CLSTP_PROTECTED       2 : method or data protected
 *             ZH_OO_CLSTP_HIDDEN          4 : method or data hidden
 *           * ZH_OO_CLSTP_CTOR            8 : method constructor
 *             ZH_OO_CLSTP_READONLY       16 : data read only
 *             ZH_OO_CLSTP_SHARED         32 : (method or) data shared
 *           * ZH_OO_CLSTP_CLASS          64 : message is class message not object
 *           * ZH_OO_CLSTP_SUPER         128 : message is inherited
 *             ZH_OO_CLSTP_PERSIST       256 : message is persistent (PROPERTY)
 *             ZH_OO_CLSTP_NONVIRTUAL    512 : Class method constructor
 *             ZH_OO_CLSTP_OVERLOADED   1024 : Class method constructor
 *             ZH_OO_CLSTP_SYNC         2048 : message synchronized by object or class mutex
 *
 * <xType>     ZH_OO_MSG_PROPERTY      :  > optional item type restriction in assignment
 *             ZH_OO_MSG_CLASSPROPERTY : /
 */

ZH_FUNC( __CLSADDMSG )
{
   ZH_USHORT uiClass = ( ZH_USHORT ) zh_parni( 1 );
   const char * szMessage = zh_parc( 2 );

   if( szMessage && uiClass && uiClass <= s_uiClasses )
   {
      ZH_USHORT nType     = ( ZH_USHORT ) zh_parni( 4 );
      ZH_USHORT uiScope   = ( ZH_USHORT ) zh_parni( 6 );
      PZH_ITEM  pFunction = zh_param( 3, ZH_IT_ANY );
      PZH_ITEM  pInit     = zh_param( 5, ZH_IT_ANY );

      if( nType == ZH_OO_MSG_DATA )
      {
         nType = szMessage[ 0 ] == '_' ? ZH_OO_MSG_ASSIGN : ZH_OO_MSG_ACCESS;
      }
      else if( nType == ZH_OO_MSG_CLASSDATA )
      {
         nType = szMessage[ 0 ] == '_' ? ZH_OO_MSG_CLSASSIGN :
                                         ZH_OO_MSG_CLSACCESS;
      }
      /* to make xZiher users happy ;-) */
      else if( nType == ZH_OO_MSG_PROPERTY ||
               nType == ZH_OO_MSG_CLASSPROPERTY )
      {
         PZH_ITEM pType = zh_param( 7, ZH_IT_ANY );
         char szAssign[ ZH_SYMBOL_NAME_LEN + 1 ];
         int iLen = ( int ) zh_parclen( 2 );
         if( iLen >= ZH_SYMBOL_NAME_LEN )
            iLen = ZH_SYMBOL_NAME_LEN - 1;
         szAssign[ 0 ] = '_';
         memcpy( szAssign + 1, szMessage, iLen );
         szAssign[ iLen + 1 ] = '\0';

         uiScope = ( uiScope | ZH_OO_CLSTP_EXPORTED ) &
                  ~( ZH_OO_CLSTP_PROTECTED | ZH_OO_CLSTP_HIDDEN );
         if( nType == ZH_OO_MSG_PROPERTY )
         {
            zh_clsAddMsg( uiClass, szAssign, ZH_OO_MSG_ASSIGN,
                          ( ZH_USHORT ) ( uiScope & ~ZH_OO_CLSTP_PERSIST ),
                          pFunction, pType );
            nType = ZH_OO_MSG_ACCESS;
         }
         else
         {
            zh_clsAddMsg( uiClass, szAssign, ZH_OO_MSG_CLSASSIGN,
                          ( ZH_USHORT ) ( uiScope & ~ZH_OO_CLSTP_PERSIST ),
                          pFunction, pType );
            nType = ZH_OO_MSG_CLSACCESS;
         }
      }

      zh_clsAddMsg( uiClass, szMessage, nType, uiScope, pFunction, pInit );
   }
}

/* __clsNew( <szClassName>, <uiDatas>,
 *           [<pSuperArray>], [<pClassFunc>],
 *           [<fModuleFriendly>] ) --> <hClass>
 *
 * Create a new class
 *
 * <szClassName>     Name of the class
 * <uiDatas>         Number of DATAs in the class
 * <pSuperArray>     Optional array with handle(s) of superclass(es)
 * <pClassFunc>      Class function symbol, when NULL public function
 *                   with the same name as szClassName is used
 * <fModuleFriendly> when true all functions and classes from the same
 *                   module as pClassFunc are defined as friends
 */
static ZH_USHORT zh_clsNew( const char * szClassName, ZH_USHORT uiDatas,
                            PZH_ITEM pSuperArray, PZH_SYMB pClassFunc,
                            ZH_BOOL fModuleFriendly )
{
   PCLASS pNewCls;
   PMETHOD pMethod;
   ZH_USHORT ui, uiSuper, uiSuperCls;
   ZH_USHORT * puiClassData = NULL, uiClassDataSize = 0;
   ZH_BOOL fClsMutex = ZH_FALSE;

   uiSuper  = ( ZH_USHORT ) ( pSuperArray ? zh_arrayLen( pSuperArray ) : 0 );
   pClassFunc = zh_vmGetRealFuncSym( pClassFunc );

   pNewCls = ( PCLASS ) zh_xgrabz( sizeof( CLASS ) );

   ZH_CLASS_LOCK();

   if( s_uiClasses == s_uiClsSize )
   {
      s_uiClsSize += ZH_CLASS_POOL_RESIZE;
      s_pClasses = ( PCLASS * ) zh_xrealloc( s_pClasses, sizeof( PCLASS ) *
                                             ( ( ZH_SIZE ) s_uiClsSize + 1 ) );
   }
   pNewCls->uiClass = s_uiClasses + 1;
   s_pClasses[ pNewCls->uiClass ] = pNewCls;
   ++s_uiClasses;

   ZH_CLASS_UNLOCK();

   pNewCls->szName = zh_strdup( szClassName );
   pNewCls->pClassSym = zh_dynsymGet( pNewCls->szName );
   if( ! pClassFunc )
      pClassFunc = zh_vmGetRealFuncSym( pNewCls->pClassSym->pSymbol );
   pNewCls->pClassFuncSym = pClassFunc;
   if( fModuleFriendly )
      zh_vmFindModuleSymbols( pClassFunc, &pNewCls->pFriendModule,
                                          &pNewCls->uiFriendModule );

   for( ui = 1; ui <= uiSuper; ++ui )
   {
      uiSuperCls = ( ZH_USHORT ) zh_arrayGetNI( pSuperArray, ui );
      if( uiSuperCls && uiSuperCls < pNewCls->uiClass )
      {
         PCLASS pSprCls;

         pSprCls = s_pClasses[ uiSuperCls ];
         if( ! zh_clsInited( pNewCls ) ) /* This is the first superclass */
         {
            zh_clsCopyClass( pNewCls, pSprCls );
         }
         else if( ! zh_clsHasParentClass( pNewCls, uiSuperCls ) )
         {
            ZH_SIZE n, nLimit;
            ZH_USHORT nLenClsDatas;
            ZH_USHORT uiCount;

            /* create class data translation tables */
            nLenClsDatas = ( ZH_USHORT ) zh_itemSize( pSprCls->pClassDatas );
            if( nLenClsDatas )
            {
               if( nLenClsDatas > uiClassDataSize )
               {
                  puiClassData = ( ZH_USHORT * ) zh_xrealloc( puiClassData,
                                             sizeof( ZH_USHORT ) * nLenClsDatas );
                  uiClassDataSize = nLenClsDatas;
               }
               memset( puiClassData, 0, sizeof( ZH_USHORT ) * nLenClsDatas );
            }

            /* Copy super class handles */
            for( uiCount = 0; uiCount < pSprCls->uiSuperClasses; ++uiCount )
               zh_clsDefineSuperClass( pNewCls, pSprCls->pSuperClasses[ uiCount ].uiClass, ZH_TRUE );
            zh_clsDefineSuperClass( pNewCls, uiSuperCls, ZH_TRUE );

            /* Copy instance area init data */
            if( pSprCls->uiInitDatas )
            {
               ZH_USHORT u;
               for( u = 0; u < pSprCls->uiInitDatas; ++u )
               {
                  if( pSprCls->pInitData[ u ].uiType == ZH_OO_MSG_DATA )
                  {
                     ZH_USHORT uiCls = pSprCls->pInitData[ u ].uiSprClass;
                     zh_clsAddInitValue( pNewCls,
                                 pSprCls->pInitData[ u ].pInitValue,
                                 ZH_OO_MSG_DATA,
                                 pSprCls->pInitData[ u ].uiData,
                                 zh_clsParentInstanceOffset( pNewCls, uiCls ),
                                 uiCls );
                  }
               }
            }

            /* Now working on other methods */
            nLimit = zh_clsMthNum( pSprCls );
            for( n = 0; n < nLimit; ++n )
            {
               if( pSprCls->pMethods[ n ].pMessage )
               {
                  pMethod = zh_clsAllocMsg( pNewCls, pSprCls->pMethods[ n ].pMessage );

                  /* update instance area offset */
                  if( pMethod->pMessage && pMethod->pFuncSym == &s___msgSuper )
                     pMethod->uiOffset = zh_clsParentInstanceOffset( pNewCls, pMethod->uiData );

                  /* Ok, this bucket is empty */
                  if( pMethod->pMessage == NULL ||
                      ( zh_clsCanClearMethod( pMethod, ZH_FALSE ) &&
                        ( pMethod->pFuncSym == &s___msgVirtual ||
                          ( s_uiObjectClass != 0 &&
                            pMethod->uiSprClass == s_uiObjectClass ) ) ) )
                  {
                     if( pMethod->pMessage == NULL )
                        /* Now, we can increment the msg count */
                        pNewCls->uiMethods++;

                     memcpy( pMethod, pSprCls->pMethods + n, sizeof( METHOD ) );
                     if( ! zh_clsUpdateHiddenMessages( pMethod, pMethod, pNewCls ) )
                     {
                        PZH_SYMB pFuncSym = pMethod->pFuncSym;

                        if( pFuncSym == &s___msgSync || pFuncSym == &s___msgSyncClass )
                           pFuncSym = pMethod->pRealSym;

                        if( pFuncSym == &s___msgSetClsData ||
                            pFuncSym == &s___msgGetClsData )
                        {
                           if( pMethod->uiData > nLenClsDatas )
                              zh_errInternal( ZH_EI_CLSINVMETHOD, NULL, "__clsNew()", NULL );

                           if( puiClassData[ pMethod->uiData - 1 ] == 0 )
                           {
                              puiClassData[ pMethod->uiData - 1 ] = ( ZH_USHORT )
                                          zh_arrayLen( pNewCls->pClassDatas ) + 1;
                              zh_arraySize( pNewCls->pClassDatas,
                                            puiClassData[ pMethod->uiData - 1 ] );
                           }
                           if( pMethod->uiOffset )
                           {
                              pMethod->uiOffset = zh_clsAddInitValue( pNewCls,
                                 pSprCls->pInitData[ pMethod->uiOffset - 1 ].pInitValue,
                                 ZH_OO_MSG_CLASSDATA, puiClassData[ pMethod->uiData - 1 ],
                                 0, uiSuperCls );
                           }
                           pMethod->uiData = puiClassData[ pMethod->uiData - 1 ];
                        }
                        else if( pFuncSym == &s___msgSetData ||
                                 pFuncSym == &s___msgGetData )
                        {
                           pMethod->uiOffset = zh_clsParentInstanceOffset( pNewCls,
                                                            pMethod->uiSprClass );
                        }
                        pMethod->uiScope |= ZH_OO_CLSTP_SUPER;
                     }
                  }
                  else
                  {
                     if( pSprCls->pMethods[ n ].uiScope &
                         ( ZH_OO_CLSTP_OVERLOADED | ZH_OO_CLSTP_NONVIRTUAL ) )
                        pMethod->uiScope |= ZH_OO_CLSTP_OVERLOADED;

                     zh_clsUpdateHiddenMessages( pSprCls->pMethods + n, pMethod, pNewCls );
                  }
               }
            }
            if( pSprCls->fHasOnError )
               pNewCls->fHasOnError = ZH_TRUE;
            if( pSprCls->fHasDestructor )
               pNewCls->fHasDestructor = ZH_TRUE;
            pNewCls->nOpFlags |= pSprCls->nOpFlags;
            if( pSprCls->uiMutexOffset )
               pNewCls->uiMutexOffset = 1;
            if( pSprCls->pMutex )
               fClsMutex = ZH_TRUE;
         }
      }
   }
   if( puiClassData )
      zh_xfree( puiClassData );

   if( ! zh_clsInited( pNewCls ) )
   {
      zh_clsDictInit( pNewCls, HASH_KEY );
      pNewCls->pClassDatas  = zh_itemArrayNew( 0 );
      pNewCls->pSharedDatas = zh_itemArrayNew( 0 );
      pNewCls->pInlines     = zh_itemArrayNew( 0 );
   }

   /* add self class casting */
   pNewCls->uiDataFirst = pNewCls->uiDatas;
   zh_clsDefineSuperClass( pNewCls, pNewCls->uiClass, ZH_FALSE );
   pNewCls->uiDatas += uiDatas;
   if( pNewCls->uiMutexOffset )
      pNewCls->uiMutexOffset = pNewCls->uiDatas + 1;
   if( fClsMutex && ! pNewCls->pMutex )
      pNewCls->pMutex = zh_threadMutexCreate();

   return pNewCls->uiClass;
}

/* __clsNew( <cClassName>, <nDatas>, [<ahSuper>], [<pClassFunc>], [<lModuleFriendly>] ) --> <hClass>
 *
 * Create a new class
 *
 * <cClassName> Name of the class
 * <nDatas>     Number of DATAs in the class
 * <ahSuper>    Optional array with handle(s) of superclass(es)
 * <pClassFunc> Class function symbol
 * <lModuleFriendly> when true all functions and classes from the same
 *                   module as pClassFunc are defined as friends
 */
ZH_FUNC( __CLSNEW )
{
   const char * szClassName;
   PZH_ITEM pDatas, pSuperArray, pClassFunc, pModFriend;

   szClassName = zh_parc( 1 );

   pDatas = zh_param( 2, ZH_IT_ANY );

   pSuperArray = zh_param( 3, ZH_IT_ANY );
   if( pSuperArray && ZH_IS_NIL( pSuperArray ) )
      pSuperArray = NULL;

   pClassFunc = zh_param( 4, ZH_IT_ANY );
   if( pClassFunc && ZH_IS_NIL( pClassFunc ) )
      pClassFunc = NULL;

   pModFriend = zh_param( 5, ZH_IT_ANY );
   if( pModFriend && ZH_IS_NIL( pModFriend ) )
      pModFriend = NULL;

   if( szClassName &&
       ( ! pDatas || ZH_IS_NUMERIC( pDatas ) ) &&
       ( ! pSuperArray || ZH_IS_ARRAY( pSuperArray ) ) &&
       ( ! pClassFunc || ZH_IS_SYMBOL( pClassFunc ) ) &&
       ( ! pModFriend || ZH_IS_LOGICAL( pModFriend ) ) )
   {
      ZH_STACK_TLS_PRELOAD
      ZH_USHORT uiClass;
      uiClass = zh_clsNew( szClassName, ( ZH_USHORT ) zh_itemGetNI( pDatas ),
                           pSuperArray, zh_itemGetSymbol( pClassFunc ),
                           zh_itemGetL( pModFriend ) );
      zh_retni( uiClass );
   }
   else
      zh_errRT_BASE( EG_ARG, 3000, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/* __clsAddFriend( <hClass>, <sFuncSym> )
 *
 * Add friend function
 */
ZH_FUNC( __CLSADDFRIEND )
{
   ZH_USHORT uiClass = ( ZH_USHORT ) zh_parni( 1 );

   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS pClass = s_pClasses[ uiClass ];

      if( ! pClass->fLocked )
      {
         PZH_SYMB pSym = zh_vmGetRealFuncSym( zh_itemGetSymbol( zh_param( 2,
                                                         ZH_IT_SYMBOL ) ) );
         if( pSym )
            zh_clsAddFriendSymbol( pClass, pSym );
      }
   }
}

/* __clsDelMsg( <hClass>, <cMessage> )
 *
 * Delete message (only for INLINE and METHOD)
 *
 * <hClass>   class handle
 * <cMessage> message
 */
ZH_FUNC( __CLSDELMSG )
{
   ZH_USHORT uiClass = ( ZH_USHORT ) zh_parni( 1 );
   PZH_ITEM pString = zh_param( 2, ZH_IT_STRING );

   if( uiClass && uiClass <= s_uiClasses && pString &&
       ! s_pClasses[ uiClass ]->fLocked )
   {
      PZH_DYNS pMsg = zh_dynsymFindName( pString->item.asString.value );

      if( pMsg )
         zh_clsFreeMsg( s_pClasses[ uiClass ], pMsg );
   }
}


/* zh_clsInst( <hClass> ) --> <pObjectItm>
 *
 * Create a new object from class definition <hClass>
 */
static PZH_ITEM zh_clsInst( ZH_USHORT uiClass )
{
   PZH_ITEM pSelf = NULL;

   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS    pClass = s_pClasses[ uiClass ];
      ZH_USHORT uiDatas = pClass->uiDatas;

      if( pClass->uiMutexOffset )
         ++uiDatas;
      pSelf = zh_itemNew( NULL );
      zh_arrayNew( pSelf, uiDatas );
      pSelf->item.asArray.value->uiClass = uiClass;

      if( pClass->uiMutexOffset )
      {
         PZH_ITEM pMutex = zh_threadMutexCreate();
         zh_arraySet( pSelf, pClass->uiMutexOffset, pMutex );
         zh_itemRelease( pMutex );
      }
      /* Initialise value if initialisation was requested */
      if( pClass->uiInitDatas )
      {
         PINITDATA pInitData = pClass->pInitData;
         ZH_USHORT ui = pClass->uiInitDatas;
         PZH_ITEM pDestItm;

         do
         {
            if( pInitData->uiType == ZH_OO_MSG_DATA )
               pDestItm = zh_arrayGetItemPtr( pSelf,
                                    pInitData->uiData + pInitData->uiOffset );
            else if( pInitData->uiType == ZH_OO_MSG_CLASSDATA )
            {
               pDestItm = zh_arrayGetItemPtr( pClass->pClassDatas,
                                              pInitData->uiData );
               /* do not initialize it again */
               pInitData->uiType = ZH_OO_MSG_INITIALIZED;
            }
            else
               pDestItm = NULL;

            if( pDestItm )
               zh_itemCloneTo( pDestItm, pInitData->pInitValue );

            ++pInitData;
         }
         while( --ui );
      }
   }

   return pSelf;
}

/* __clsInst( <hClass> ) --> <oNewObject>
 *
 * Create a new object from class definition <hClass>
 */
ZH_FUNC( __CLSINST )
{
   PZH_ITEM pSelf = zh_clsInst( ( ZH_USHORT ) zh_parni( 1 ) );

   if( pSelf )
      zh_itemReturnRelease( pSelf );
}

/* __clsLock( <hClass> )
 *
 * Block farther class modifications
 */
ZH_FUNC( __CLSLOCK )
{
   ZH_USHORT uiClass = ( ZH_USHORT ) zh_parni( 1 );

   if( uiClass && uiClass <= s_uiClasses )
      s_pClasses[ uiClass ]->fLocked = ZH_TRUE;
}

/* __clsModMsg( <hClass>, <cMessage>, <pFunc> )
 *
 * Modify message (only for INLINE and METHOD)
 */
ZH_FUNC( __CLSMODMSG )
{
   ZH_USHORT uiClass = ( ZH_USHORT ) zh_parni( 1 );
   PZH_ITEM pString = zh_param( 2, ZH_IT_STRING );

   if( uiClass && uiClass <= s_uiClasses && pString &&
       ! s_pClasses[ uiClass ]->fLocked )
   {
      PZH_DYNS pMsg = zh_dynsymFindName( pString->item.asString.value );

      if( pMsg )
      {
         PCLASS  pClass  = s_pClasses[ uiClass ];
         PMETHOD pMethod = zh_clsFindMsg( pClass, pMsg );

         if( pMethod )
         {
            PZH_SYMB pFuncSym = pMethod->pFuncSym;

            if( pFuncSym == &s___msgSetData || pFuncSym == &s___msgGetData )
            {
               zh_errRT_BASE( EG_ARG, 3004, "Cannot modify a DATA item", ZH_ERR_FUNCNAME, 0 );
            }
            else if( pFuncSym == &s___msgSetClsData || pFuncSym == &s___msgGetClsData )
            {
               zh_errRT_BASE( EG_ARG, 3004, "Cannot modify a CLASSDATA item", ZH_ERR_FUNCNAME, 0 );
            }
            else if( pFuncSym == &s___msgSetShrData || pFuncSym == &s___msgGetShrData )
            {
               zh_errRT_BASE( EG_ARG, 3004, "Cannot modify a SHARED DATA item", ZH_ERR_FUNCNAME, 0 );
            }
            else if( pFuncSym == &s___msgSuper || pFuncSym == &s___msgRealClass )
            {
               zh_errRT_BASE( EG_ARG, 3004, "Cannot modify a SUPER class casting", ZH_ERR_FUNCNAME, 0 );
            }
            else if( pFuncSym == &s___msgDestructor )
            {
               zh_errRT_BASE( EG_ARG, 3004, "Cannot modify a DESTRUCTOR method", ZH_ERR_FUNCNAME, 0 );
            }
            else if( pFuncSym == &s___msgOnError )
            {
               zh_errRT_BASE( EG_ARG, 3004, "Cannot modify a ONERROR method", ZH_ERR_FUNCNAME, 0 );
            }
            else if( pFuncSym == &s___msgScopeErr )
            {
               zh_errRT_BASE( EG_ARG, 3004, "Cannot modify a SCOPE ERROR method", ZH_ERR_FUNCNAME, 0 );
            }
            else if( pFuncSym == &s___msgPerform )
            {
               zh_errRT_BASE( EG_ARG, 3004, "Cannot modify a PERFORM method", ZH_ERR_FUNCNAME, 0 );
            }
            else if( pFuncSym == &s___msgDelegate )
            {
               zh_errRT_BASE( EG_ARG, 3004, "Cannot modify a DELEGATE method", ZH_ERR_FUNCNAME, 0 );
            }
            else if( pFuncSym == &s___msgSync )
            {
               zh_errRT_BASE( EG_ARG, 3004, "Cannot modify a SYNC method", ZH_ERR_FUNCNAME, 0 );
            }
            else if( pFuncSym == &s___msgSyncClass )
            {
               zh_errRT_BASE( EG_ARG, 3004, "Cannot modify a CLASS SYNC method", ZH_ERR_FUNCNAME, 0 );
            }
            else
            {
               PZH_ITEM pBlock = zh_param( 3, ZH_IT_BLOCK );
               if( pBlock )
               {
                  if( pFuncSym == &s___msgEvalInline &&
                      pMethod->uiSprClass == uiClass )
                  {
                     zh_arraySet( s_pClasses[ pMethod->uiSprClass ]->pInlines,
                                  pMethod->uiData, pBlock );
                  }
                  else
                  {
                     zh_arrayAdd( pClass->pInlines, pBlock );
                     pMethod->uiData = ( ZH_USHORT ) zh_arrayLen( pClass->pInlines );
                  }
               }
               else
               {
                  pFuncSym = zh_objGetFuncSym( zh_param( 3, ZH_IT_ANY ) );
                  if( pFuncSym )
                  {
                     pMethod->pFuncSym = pFuncSym;
                     pMethod->uiData = 0;
                  }
                  else
                     zh_errRT_BASE( EG_ARG, 3000, NULL, ZH_ERR_FUNCNAME, 0 );
               }
            }
         }
      }
   }
}


/* __objGetClsName( <hClass> | <oObj> ) --> <cClassName>
 *
 * Returns class name of <oObj> or <hClass>
 */
ZH_FUNC( __OBJGETCLSNAME )
{
   ZH_STACK_TLS_PRELOAD
   PZH_ITEM pObject = zh_param( 1, ZH_IT_OBJECT );
   ZH_USHORT uiClass;

   if( pObject )
      uiClass = pObject->item.asArray.value->uiClass;
   else
      uiClass = ( ZH_USHORT ) zh_parni( 1 );

   zh_retc( zh_clsName( uiClass ) );
}


/* __objHasMsg( <oObj>, <cMsgName> | <sMsgName> ) --> <lRet>
 *
 * Is <cSymbol> a valid message for the <oObj>
 */
ZH_FUNC( __OBJHASMSG )
{
   PZH_DYNS pMessage = zh_objGetMsgSym( zh_param( 2, ZH_IT_ANY ) );

   if( pMessage )
   {
      ZH_STACK_TLS_PRELOAD
      zh_retl( zh_objHasMessage( zh_param( 1, ZH_IT_ANY ), pMessage ) );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1099, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/* __objHasMsgAssigned( <oObj>, <cMsgName> | <sMsgName> ) --> <lExists>
 *
 * checks if function exists and is not virtual
 */
ZH_FUNC( __OBJHASMSGASSIGNED )
{
   PZH_DYNS pMessage = zh_objGetMsgSym( zh_param( 2, ZH_IT_ANY ) );

   if( pMessage )
   {
      ZH_STACK_TLS_PRELOAD
      PZH_SYMB pExecSym = zh_objGetMethod( zh_param( 1, ZH_IT_ANY ),
                                           pMessage->pSymbol, NULL );
      zh_retl( pExecSym && pExecSym != &s___msgVirtual );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1099, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/* __objSendMsg( <oObj>, <cMsgName> | <sMsgName>, <xArg,..> ) --> <xRet>
 *
 * Send a message to an object
 */
ZH_FUNC( __OBJSENDMSG )
{
   PZH_DYNS pMessage = zh_objGetMsgSym( zh_param( 2, ZH_IT_ANY ) );

   if( pMessage )
   {
      ZH_STACK_TLS_PRELOAD
      ZH_USHORT uiPCount = zh_pcount();
      ZH_USHORT uiParam;

      zh_vmPushSymbol( pMessage->pSymbol );     /* Push message symbol */
      zh_vmPush( zh_param( 1, ZH_IT_ANY ) );    /* Push object */

      for( uiParam = 3; uiParam <= uiPCount; ++uiParam )    /* Push arguments on stack */
         zh_vmPush( zh_stackItemFromBase( uiParam ) );

      zh_vmSend( ( ZH_USHORT ) ( uiPCount - 2 ) );             /* Execute message */
   }
   else
      zh_errRT_BASE( EG_ARG, 3000, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/* __objClone( <oSource> ) --> <oNew>
 *
 * Clone an object. Note the similarity with aClone ;-)
 */
ZH_FUNC( __OBJCLONE )
{
   ZH_STACK_TLS_PRELOAD
   PZH_ITEM pObject = zh_param( 1, ZH_IT_OBJECT );

   if( pObject )
      zh_arrayCloneTo( zh_stackReturnItem(), pObject );
   else
      zh_errRT_BASE( EG_ARG, 3001, NULL, ZH_ERR_FUNCNAME, 0 );
}

/* __clsInstSuper( <cClassName> | <sClassFunc> ) --> <hClass>
 *
 * Instance super class and return class handle
 */
ZH_FUNC( __CLSINSTSUPER )
{
   ZH_STACK_TLS_PRELOAD
   PZH_ITEM pItem = zh_param( 1, ZH_IT_STRING | ZH_IT_SYMBOL );
   ZH_USHORT uiClassH = 0, uiClass;
   PZH_SYMB pClassFuncSym = NULL;
   char szDesc[ 128 ];

   if( pItem )
   {
      if( ZH_IS_SYMBOL( pItem ) )
         pClassFuncSym = zh_itemGetSymbol( pItem );
      else if( ZH_IS_STRING( pItem ) )
      {
         PZH_DYNS pDynSym = zh_dynsymFindName( zh_itemGetCPtr( pItem ) );
         if( pDynSym )
            pClassFuncSym = pDynSym->pSymbol;
      }
      pClassFuncSym = zh_vmGetRealFuncSym( pClassFuncSym );
   }

   if( pClassFuncSym )
   {
      uiClassH = zh_clsFindClassByFunc( pClassFuncSym );
      if( uiClassH == 0 )
      {
         zh_vmPushSymbol( pClassFuncSym );
         zh_vmPushNil();
         zh_vmProc( 0 ); /* Execute super class */

         if( zh_vmRequestQuery() == 0 )
         {
            PZH_ITEM pObject = zh_stackReturnItem();

            if( ZH_IS_OBJECT( pObject ) )
            {
               uiClass = pObject->item.asArray.value->uiClass;

               if( s_pClasses[ uiClass ]->pClassFuncSym == pClassFuncSym )
                  uiClassH = uiClass;
               else
               {
                  uiClassH = zh_clsFindClassByFunc( pClassFuncSym );
                  /* still not found, try to send NEW() message */
                  if( uiClassH == 0 )
                  {
                     zh_vmPushSymbol( &s___msgNew );
                     zh_vmPush( pObject );
                     zh_vmSend( 0 );

                     pObject = zh_stackReturnItem();
                     if( ZH_IS_OBJECT( pObject ) )
                     {
                        uiClass = pObject->item.asArray.value->uiClass;
                        if( s_pClasses[ uiClass ]->pClassFuncSym == pClassFuncSym )
                           uiClassH = uiClass;
                     }
                  }
               }
            }

            /* This disables destructor execution for this object */
            if( uiClassH && ZH_IS_OBJECT( pObject ) )
               pObject->item.asArray.value->uiClass = 0;
            else if( zh_vmRequestQuery() == 0 )
            {
               zh_snprintf( szDesc, sizeof( szDesc ),
                            "Super class '%s' does not return an object",
                            pClassFuncSym->szName );
               zh_errRT_BASE( EG_ARG, 3002, szDesc, ZH_ERR_FUNCNAME, 0 );
            }
         }
      }
   }
   else
   {
      const char * pszName;

      pClassFuncSym = zh_itemGetSymbol( pItem );
      if( pClassFuncSym )
         pszName = pClassFuncSym->szName;
      else
         pszName = zh_itemGetCPtr( pItem );
      zh_snprintf( szDesc, sizeof( szDesc ),
                   "Cannot find super class '%s'", pszName );
      zh_errRT_BASE( EG_ARG, 3003, szDesc, ZH_ERR_FUNCNAME, 0 );
   }

   zh_retni( uiClassH );
}

/* __clsAssocType( <hClass>, <cType> ) --> <lOK>
 *
 * Associate class with given basic type
 */
ZH_FUNC( __CLSASSOCTYPE )
{
   ZH_STACK_TLS_PRELOAD
   ZH_USHORT uiClass = ( ZH_USHORT ) zh_parni( 1 );
   PZH_ITEM pType = zh_param( 2, ZH_IT_ANY );
   ZH_BOOL fResult = ZH_FALSE;

   if( uiClass && uiClass <= s_uiClasses && pType )
   {
      ZH_TYPE nType = zh_clsGetItemType( pType, ZH_IT_ANY );

      if( s_pClasses[ uiClass ]->uiDatas )
         zh_errRT_BASE( EG_ARG, 3005, "Scalar class cannot contain instance variables", ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
      else if( nType != ZH_IT_ANY )
      {
         switch( nType )
         {
            case ZH_IT_ARRAY:
               s_uiArrayClass = uiClass;
               break;
            case ZH_IT_BLOCK:
               s_uiBlockClass = uiClass;
               break;
            case ZH_IT_STRING:
               s_uiCharacterClass = uiClass;
               break;
            case ZH_IT_DATE:
               s_uiDateClass = uiClass;
               break;
            case ZH_IT_TIMESTAMP:
               s_uiTimeStampClass = uiClass;
               break;
            case ZH_IT_HASH:
               s_uiHashClass = uiClass;
               break;
            case ZH_IT_LOGICAL:
               s_uiLogicalClass = uiClass;
               break;
            case ZH_IT_NIL:
               s_uiNilClass = uiClass;
               break;
            case ZH_IT_NUMERIC:
               s_uiNumericClass = uiClass;
               break;
            case ZH_IT_SYMBOL:
               s_uiSymbolClass = uiClass;
               break;
            case ZH_IT_POINTER:
               s_uiPointerClass = uiClass;
               break;
            default:
               uiClass = 0;
         }
         fResult = uiClass != 0;
      }
   }

   zh_retl( fResult );
}

/* __clsCntClasses() --> <nCount>
 *
 * Return number of classes
 */
ZH_FUNC( __CLSCNTCLASSES )
{
   ZH_STACK_TLS_PRELOAD
   zh_retni( ( int ) s_uiClasses );
}

/* __cls_CntClsData( <hClass> ) --> <nCount>
 *
 * Return number of class datas
 */
ZH_FUNC( __CLS_CNTCLSDATA )
{
   ZH_STACK_TLS_PRELOAD
   ZH_USHORT uiClass = ( ZH_USHORT ) zh_parni( 1 );

   zh_retni( uiClass && uiClass <= s_uiClasses ?
                  ( ZH_USHORT ) zh_arrayLen( s_pClasses[ uiClass ]->pClassDatas ) : 0 );
}

/* __cls_CntShrData( <hClass> ) --> <nCount>
 *
 * Return number of class datas
 */
ZH_FUNC( __CLS_CNTSHRDATA )
{
   ZH_STACK_TLS_PRELOAD
   ZH_USHORT uiClass = ( ZH_USHORT ) zh_parni( 1 );

   zh_retni( uiClass && uiClass <= s_uiClasses ?
                  ( ZH_USHORT ) zh_arrayLen( s_pClasses[ uiClass ]->pSharedDatas ) : 0 );
}

/* __cls_CntData( <hClass> ) --> <nCount>
 *
 * Return number of datas
 */
ZH_FUNC( __CLS_CNTDATA )
{
   ZH_STACK_TLS_PRELOAD
   ZH_USHORT uiClass = ( ZH_USHORT ) zh_parni( 1 );

   zh_retni( uiClass && uiClass <= s_uiClasses ?
             s_pClasses[ uiClass ]->uiDatas : 0 );
}

/* __cls_DecData( <hClass> ) --> <nCount>
 *
 * Decrease number of datas and return new value
 */
ZH_FUNC( __CLS_DECDATA )
{
   ZH_STACK_TLS_PRELOAD
   ZH_USHORT uiClass = ( ZH_USHORT ) zh_parni( 1 );

   if( uiClass && uiClass <= s_uiClasses &&
       s_pClasses[ uiClass ]->uiDatas > s_pClasses[ uiClass ]->uiDataFirst )
   {
      if( ! s_pClasses[ uiClass ]->fLocked )
         s_pClasses[ uiClass ]->uiDatas--;
      zh_retni( s_pClasses[ uiClass ]->uiDatas - s_pClasses[ uiClass ]->uiDataFirst );
   }
   else
      zh_retni( 0 );
}

/* __cls_IncData( <hClass> ) --> <nCount>
 *
 * Increase number of datas and return offset to new value
 */
ZH_FUNC( __CLS_INCDATA )
{
   ZH_STACK_TLS_PRELOAD
   ZH_USHORT uiClass = ( ZH_USHORT ) zh_parni( 1 );

   if( uiClass && uiClass <= s_uiClasses )
   {
      if( ! s_pClasses[ uiClass ]->fLocked )
         s_pClasses[ uiClass ]->uiDatas++;
      zh_retni( s_pClasses[ uiClass ]->uiDatas - s_pClasses[ uiClass ]->uiDataFirst );
   }
   else
      zh_retni( 0 );
}


ZH_FUNC_TRANSLATE( __CLASSNEW, __CLSNEW )
ZH_FUNC_TRANSLATE( __CLASSINSTANCE, __CLSINST )
ZH_FUNC_TRANSLATE( __CLASSADD, __CLSADDMSG )

ZH_FUNC( __CLASSNAME )
{
   ZH_STACK_TLS_PRELOAD
   zh_retc( zh_clsName( ( ZH_USHORT ) zh_parni( 1 ) ) );
}

ZH_FUNC( __CLASSSEL )
{
   ZH_USHORT uiClass = ( ZH_USHORT ) zh_parni( 1 );
   PZH_ITEM pReturn = zh_itemNew( NULL );

   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS pClass = s_pClasses[ uiClass ];
      PMETHOD pMethod = pClass->pMethods;
      ZH_SIZE nLimit = zh_clsMthNum( pClass ), nPos = 0;

      zh_arrayNew( pReturn, pClass->uiMethods ); /* Create a transfer array */

      do
      {
         if( pMethod->pMessage )    /* Hash Entry used ? */
            zh_arraySetC( pReturn, ++nPos, pMethod->pMessage->pSymbol->szName );
         ++pMethod;
      }
      while( --nLimit );

      if( nPos < ( ZH_SIZE ) pClass->uiMethods )
         zh_arraySize( pReturn, nPos );
   }

   zh_itemReturnRelease( pReturn );
}

/* to be used from Classes ERROR HANDLER method */
ZH_FUNC( __GETMESSAGE )
{
   ZH_STACK_TLS_PRELOAD
   zh_retc( zh_stackItem ( zh_stackBaseItem()->item.asSymbol.stackstate->nBaseItem )->item.asSymbol.value->szName );
}

/* __clsParent( <hClass>, <cParentClass> ) --> <lIsParent>
 * Checks if <cParentClass> is parent of <hClass>
 */
ZH_FUNC( __CLSPARENT )
{
   ZH_STACK_TLS_PRELOAD
   const char * szParentName = zh_parc( 2 );

   zh_retl( szParentName &&
            zh_clsIsParent( ( ZH_USHORT ) zh_parni( 1 ), szParentName ) );
}

/* __Sender() --> <obj> | NIL
 * returns sender object
 */
ZH_FUNC( __SENDER )
{
   ZH_STACK_TLS_PRELOAD
   ZH_ISIZ nOffset = zh_stackBaseProcOffset( 2 );

   if( nOffset > 0 )
   {
      PZH_ITEM pSelf = zh_stackItem( nOffset + 1 );

      /* Is it inline method? */
      if( nOffset > 0 && ZH_IS_BLOCK( pSelf ) &&
          zh_stackItem( nOffset )->item.asSymbol.value == &zh_symEval )
      {
         pSelf = zh_stackItem( zh_stackItem( nOffset )->
                               item.asSymbol.stackstate->nBaseItem + 1 );
      }

      if( ZH_IS_OBJECT( pSelf ) )
      {
         zh_itemReturn( pSelf );
      }
   }
}

ZH_FUNC( __CLSSYNCSIGNAL )
{
   zh_threadMutexSyncSignal( zh_param( 1, ZH_IT_ANY ) );
}

ZH_FUNC( __CLSSYNCWAIT )
{
   ZH_STACK_TLS_PRELOAD
   PZH_ITEM pMutex = NULL;
   ZH_ULONG ulMilliSec = ZH_THREAD_INFINITE_WAIT;
   ZH_ISIZ nOffset = zh_stackBaseProcOffset( 2 );

   if( nOffset > 0 )
   {
      PZH_ITEM pBase = zh_stackItem( nOffset );
      PZH_STACK_STATE pStack = pBase->item.asSymbol.stackstate;
      ZH_USHORT uiClass = pStack->uiClass;

      if( uiClass && uiClass <= s_uiClasses )
      {
         PCLASS pClass = s_pClasses[ uiClass ];
         PMETHOD pMethod = pClass->pMethods + pStack->uiMethod;

         if( pMethod->pFuncSym == &s___msgSync )
         {
            PZH_ITEM pSelf = zh_stackItem( nOffset + 1 );

            /* Is it inline method? */
            if( ZH_IS_BLOCK( pSelf ) && pBase->item.asSymbol.value == &zh_symEval )
               pSelf = zh_stackItem( pBase->item.asSymbol.stackstate->nBaseItem + 1 );

            uiClass = zh_objGetClass( pSelf );
            if( uiClass && uiClass <= s_uiClasses )
               pMutex = zh_arrayGetItemPtr( pSelf, s_pClasses[ uiClass ]->uiMutexOffset );
         }
         else if( pMethod->pFuncSym == &s___msgSyncClass )
            pMutex = pClass->pMutex;
      }
   }

   if( ZH_IS_PARAM_NUM( 2 ) )
   {
      double dTimeOut = zh_parnd( 2 );
      if( dTimeOut > 0 )
         ulMilliSec = ( ZH_ULONG ) ( dTimeOut * 10 );
   }

   zh_retl( zh_threadMutexSyncWait( zh_param( 1, ZH_IT_ANY ), ulMilliSec, pMutex ) );
}

/* __classH( <obj> ) --> <hClass>
 *
 * Returns class handle of <obj>
 */
ZH_FUNC( __CLASSH )
{
   ZH_STACK_TLS_PRELOAD
   PZH_ITEM pObject = zh_param( 1, ZH_IT_ANY );

   zh_retni( pObject ? zh_objGetClassH( pObject ) : 0 );
}

/* --- */

/* <hClass> := <obj>:ClassH()
 *
 * Returns class handle of <obj>
 */
ZH_FUNC_STATIC( msgClassH )
{
   ZH_STACK_TLS_PRELOAD
   zh_retni( zh_stackBaseItem()->item.asSymbol.stackstate->uiClass );
}


/* <cClassName> := <obj>:ClassName()
 *
 * Return class name of <obj>. Can also be used for all types.
 */
ZH_FUNC_STATIC( msgClassName )
{
   ZH_STACK_TLS_PRELOAD
   ZH_USHORT uiClass = zh_stackBaseItem()->item.asSymbol.stackstate->uiClass;

   if( uiClass )
      zh_retc( s_pClasses[ uiClass ]->szName );
   else
      zh_retc( zh_objGetClsName( zh_stackSelfItem() ) );
}


static int zh_methodType( PMETHOD pMethod )
{
   PZH_SYMB pFuncSym = pMethod->pFuncSym;

   if( pFuncSym == &s___msgSync || pFuncSym == &s___msgSyncClass )
      pFuncSym = pMethod->pRealSym;

   if     ( pFuncSym == &s___msgSetClsData ||
            pFuncSym == &s___msgGetClsData ||
            pFuncSym == &s___msgSetShrData ||
            pFuncSym == &s___msgGetShrData )
      return ZH_OO_MSG_CLASSDATA;

   else if( pFuncSym == &s___msgSetData ||
            pFuncSym == &s___msgGetData )
      return ZH_OO_MSG_DATA;

   else if( pFuncSym == &s___msgEvalInline )
      return ZH_OO_MSG_INLINE;

   else if( pFuncSym == &s___msgVirtual )
      return ZH_OO_MSG_VIRTUAL;

   else if( pFuncSym == &s___msgSuper )
      return ZH_OO_MSG_SUPER;

   else if( pFuncSym == &s___msgRealClass )
      return ZH_OO_MSG_REALCLASS;

   else if( pFuncSym == &s___msgDelegate )
      return ZH_OO_MSG_DELEGATE;

   else if( pFuncSym == &s___msgPerform )
      return ZH_OO_MSG_PERFORM;

   else if( pMethod->pMessage == s___msgOnError.pDynSym )
      return ZH_OO_MSG_ONERROR;

   else if( pMethod->pMessage == s___msgDestructor.pDynSym )
      return ZH_OO_MSG_DESTRUCTOR;

   else
      return ZH_OO_MSG_METHOD;
}

/* <aMessages> := <obj>:ClassSel()
 *
 * Returns all the messages in <obj>
 */
ZH_FUNC_STATIC( msgClassSel )
{
   ZH_STACK_TLS_PRELOAD
   ZH_USHORT uiClass = zh_stackBaseItem()->item.asSymbol.stackstate->uiClass;

   if( uiClass && uiClass <= s_uiClasses )
   {
      PZH_ITEM pReturn;
      PCLASS pClass = s_pClasses[ uiClass ];
      PMETHOD pMethod = pClass->pMethods;
      ZH_SIZE nLimit = zh_clsMthNum( pClass ), nPos = 0;
      ZH_USHORT nParam, nScope;
      ZH_BOOL lFull;

      nParam = ( ZH_USHORT ) zh_parnidef( 1, ZH_MSGLISTALL );
      nScope = ( ZH_USHORT ) zh_parni( 2 );
      lFull = zh_parl( 3 );
      pReturn = zh_itemArrayNew( pClass->uiMethods );

      do
      {
         if( pMethod->pMessage )  /* Hash Entry used ? */
         {
            if( ( nParam == ZH_MSGLISTALL ) ||
                ( nParam == ZH_MSGLISTCLASS &&
                  zh_methodType( pMethod ) == ZH_OO_MSG_CLASSDATA ) ||
                ( nParam == ZH_MSGLISTPURE &&
                  zh_methodType( pMethod ) != ZH_OO_MSG_CLASSDATA ) )
            {
               if( nScope == 0 || ( pMethod->uiScope & nScope ) != 0 )
               {
                  if( lFull )
                  {
                     PZH_ITEM pItem = zh_arrayGetItemPtr( pReturn, ++nPos );
                     if( pItem )
                     {
                        zh_arrayNew( pItem, 4 );
                        zh_arraySetC( pItem, ZH_OO_DATA_SYMBOL,
                                      pMethod->pMessage->pSymbol->szName );
                        zh_arraySetNI( pItem, ZH_OO_DATA_TYPE, zh_methodType( pMethod ) );
                        zh_arraySetNI( pItem, ZH_OO_DATA_SCOPE, pMethod->uiScope );
                     }
                  }
                  else
                     zh_arraySetC( pReturn, ++nPos,
                                   pMethod->pMessage->pSymbol->szName );
               }
            }
         }
         ++pMethod;
      }
      while( --nLimit && nPos < ( ZH_SIZE ) pClass->uiMethods );

      if( nPos < ( ZH_SIZE ) pClass->uiMethods )
         zh_arraySize( pReturn, nPos );
      zh_itemReturnRelease( pReturn );
   }
}

#if 0

/* __msgClass()
 *
 * Internal function to return Self at Self:Class call (classy compatibility)
 */
ZH_FUNC_STATIC( msgClass )
{
   zh_itemReturnForward( zh_stackSelfItem() );
}

/* <obj>:IsDerivedFrom( xParam ) --> <logical>
 *
 * Return true if <obj> is derived from xParam.
 * xParam can be either an obj or a classname
 */
ZH_FUNC_STATIC( msgClassParent )
{
   ZH_BOOL fHasParent = ZH_FALSE;
   PZH_ITEM pItem;
   ZH_USHORT uiClass;

   uiClass = zh_stackBaseItem()->item.asSymbol.stackstate->uiClass;
   pItemParam = zh_param( 1, ZH_IT_ANY );

   if( pItemParam && uiClass && uiClass <= s_uiClasses )
   {
      if( ZH_IS_OBJECT( pItemParam ) )
         fHasParent = zh_clsHasParentClass( s_pClasses[ uiClass ],
                                   pItemParam->item.asArray.value->uiClass );
      else if( ZH_IS_STRING( pItemParam ) )
         fHasParent = zh_clsIsParent( uiClass, zh_parc( pItemParam ) )
   }

   zh_retl( fHasParent );
}

#endif


/* __msgEvalInline()
 *
 * Internal function executed for inline methods
 */
ZH_FUNC_STATIC( msgEvalInline )
{
   ZH_STACK_TLS_PRELOAD
   PZH_STACK_STATE pStack = zh_stackBaseItem()->item.asSymbol.stackstate;
   PCLASS pClass   = s_pClasses[ pStack->uiClass ];
   PMETHOD pMethod = pClass->pMethods + pStack->uiMethod;
   ZH_USHORT uiPCount = zh_pcount(), uiParam;
   PZH_ITEM pBlock;

   zh_vmPushEvalSym();

   zh_vmPush( zh_arrayGetItemPtr( s_pClasses[ pMethod->uiSprClass ]->pInlines,
                                  pMethod->uiData ) );
   pBlock = zh_stackItemFromTop( -1 );    /* Push block */
   pBlock->item.asBlock.hclass = pStack->uiClass;
   pBlock->item.asBlock.method = pStack->uiMethod;

   zh_vmPush( zh_stackSelfItem() );       /* Push self as first argument */

   for( uiParam = 1; uiParam <= uiPCount; uiParam++ )
   {
      zh_vmPush( zh_stackItemFromBase( uiParam ) );
   }

   zh_vmEval( ( ZH_USHORT ) ( uiPCount + 1 ) );
}

ZH_FUNC_STATIC( msgPerform )
{
   ZH_STACK_TLS_PRELOAD
   PZH_ITEM pItem = zh_param( 1, ZH_IT_ANY );

   if( pItem )
   {
      ZH_USHORT uiPCount = zh_pcount(), uiParam;
      PZH_SYMB pSym = NULL;

      if( ZH_IS_SYMBOL( pItem ) )
         pSym = pItem->item.asSymbol.value;

      else if( ZH_IS_OBJECT( pItem ) &&
               s_pClasses[ pItem->item.asArray.value->uiClass ]->pClassSym ==
               s___msgSymbol.pDynSym )
      {
         /* Dirty hack */
         pItem = zh_arrayGetItemPtr( pItem, 1 );
         if( pItem && ZH_IS_SYMBOL( pItem ) )
            pSym = pItem->item.asSymbol.value;
      }

      if( pSym )
      {
         zh_vmPushSymbol( pSym );
         zh_vmPush( zh_stackSelfItem() );

         for( uiParam = 2; uiParam <= uiPCount; uiParam++ )
         {
            zh_vmPush( zh_stackItemFromBase( uiParam ) );
         }
         zh_vmSend( ( ZH_USHORT ) ( uiPCount - 1 ) );
      }
   }
}

ZH_FUNC_STATIC( msgDelegate )
{
   ZH_STACK_TLS_PRELOAD
   PZH_STACK_STATE pStack = zh_stackBaseItem()->item.asSymbol.stackstate;
   PCLASS pClass   = s_pClasses[ pStack->uiClass ];
   PMETHOD pMethod = pClass->pMethods + pStack->uiMethod;
   PZH_SYMB pExecSym = pClass->pMethods[ pMethod->uiData ].pFuncSym;

   if( pExecSym )
      ZH_VM_FUNCUNREF( pExecSym );
   if( pExecSym && ZH_VM_ISFUNC( pExecSym ) )
   {
      ZH_VM_EXECUTE( pExecSym );
   }
   else
   {
      ZH_FUNC_EXEC( msgNoMethod );
   }
}

ZH_FUNC_STATIC( msgSync )
{
   ZH_STACK_TLS_PRELOAD
   PZH_STACK_STATE pStack = zh_stackBaseItem()->item.asSymbol.stackstate;
   PCLASS pClass = s_pClasses[ pStack->uiClass ];
   PMETHOD pMethod = pClass->pMethods + pStack->uiMethod;
   PZH_SYMB pExecSym = pMethod->pRealSym;

   if( pExecSym )
      ZH_VM_FUNCUNREF( pExecSym );
   if( pExecSym && ZH_VM_ISFUNC( pExecSym ) )
   {
      PZH_ITEM pObject = zh_stackSelfItem();
      ZH_USHORT uiClass = zh_objGetClass( pObject );
      PZH_ITEM pMutex = NULL;

      if( uiClass && uiClass <= s_uiClasses )
         pMutex = zh_arrayGetItemPtr( pObject, s_pClasses[ uiClass ]->uiMutexOffset );

      if( ! pMutex || zh_threadMutexLock( pMutex ) )
      {
         ZH_VM_EXECUTE( pExecSym );
         if( pMutex )
            zh_threadMutexUnlock( pMutex );
      }
   }
   else
   {
      ZH_FUNC_EXEC( msgNoMethod );
   }
}

ZH_FUNC_STATIC( msgSyncClass )
{
   ZH_STACK_TLS_PRELOAD
   PZH_STACK_STATE pStack = zh_stackBaseItem()->item.asSymbol.stackstate;
   PCLASS pClass = s_pClasses[ pStack->uiClass ];
   PMETHOD pMethod = pClass->pMethods + pStack->uiMethod;
   PZH_SYMB pExecSym = pMethod->pRealSym;

   if( pExecSym )
      ZH_VM_FUNCUNREF( pExecSym );
   if( pExecSym && ZH_VM_ISFUNC( pExecSym ) )
   {
      if( ! pClass->pMutex || zh_threadMutexLock( pClass->pMutex ) )
      {
         ZH_VM_EXECUTE( pExecSym );
         if( pClass->pMutex )
            zh_threadMutexUnlock( pClass->pMutex );
      }
   }
   else
   {
      ZH_FUNC_EXEC( msgNoMethod );
   }
}

/* __msgNoMethod()
 *
 * Internal function for generating error when not existing message is sent
 */
ZH_FUNC_STATIC( msgNoMethod )
{
   ZH_STACK_TLS_PRELOAD
   PZH_SYMB pSym = zh_itemGetSymbol( zh_stackBaseItem() );
   const char * szName = pSym ? pSym->szName : "";


   if( szName[ 0 ] == '_' )
      zh_errRT_BASE_SubstR( EG_NOVARMETHOD, 1005, NULL, szName + 1, ZH_ERR_ARGS_SELFPARAMS );
   else
      zh_errRT_BASE_SubstR( EG_NOMETHOD, 1004, NULL, szName, ZH_ERR_ARGS_SELFPARAMS );
   char szDesc[ 40 + ZH_SYMBOL_NAME_LEN ];

   if( szName[ 0 ] == '_' )
   {
      zh_snprintf( szDesc, sizeof( szDesc ), "Class: '%s' has no property", zh_objGetClsName( zh_stackSelfItem() ) );
      zh_errRT_BASE_SubstR( EG_NOVARMETHOD, 1005, szDesc, szName + 1, ZH_ERR_ARGS_BASEPARAMS );
   }
   else
   {
      zh_snprintf( szDesc, sizeof( szDesc ), "Class: '%s' has no exported method", zh_objGetClsName( zh_stackSelfItem() ) );
      zh_errRT_BASE_SubstR( EG_NOMETHOD, 1004, szDesc, szName, ZH_ERR_ARGS_BASEPARAMS );
   }
}

/* __msgScopeErr()
 *
 * Internal function for generating error when not existing message is sent
 */
ZH_FUNC_STATIC( msgScopeErr )
{
   ZH_STACK_TLS_PRELOAD
   char * pszProcName;
   PZH_ITEM pObject = zh_stackSelfItem();
   PMETHOD pMethod = s_pClasses[
      zh_stackBaseItem()->item.asSymbol.stackstate->uiClass ]->pMethods +
      zh_stackBaseItem()->item.asSymbol.stackstate->uiMethod;

   pszProcName = zh_xstrcpy( NULL, zh_objGetClsName( pObject ), ":",
                             pMethod->pMessage->pSymbol->szName, NULL );
   if( pMethod->uiScope & ZH_OO_CLSTP_HIDDEN )
      zh_errRT_BASE( EG_NOMETHOD, 41, "Scope violation (hidden)", pszProcName, 0 );
   else
      zh_errRT_BASE( EG_NOMETHOD, 42, "Scope violation (protected)", pszProcName, 0 );
   zh_xfree( pszProcName );
}

ZH_FUNC_STATIC( msgTypeErr )
{
   ZH_STACK_TLS_PRELOAD
   char * pszProcName;
   PZH_ITEM pObject = zh_stackSelfItem();
   PMETHOD pMethod = s_pClasses[
      zh_stackBaseItem()->item.asSymbol.stackstate->uiClass ]->pMethods +
      zh_stackBaseItem()->item.asSymbol.stackstate->uiMethod;

   pszProcName = zh_xstrcpy( NULL, zh_objGetClsName( pObject ), ":",
                             pMethod->pMessage->pSymbol->szName + 1, NULL );
   zh_errRT_BASE( EG_NOMETHOD, 44, "Assigned value is wrong class", pszProcName, ZH_ERR_ARGS_BASEPARAMS );
   zh_xfree( pszProcName );
}

/* __msgSuper()
 *
 * Internal function to return a superobject
 */
ZH_FUNC_STATIC( msgSuper )
{
   ZH_STACK_TLS_PRELOAD
   PZH_STACK_STATE pStack = zh_stackBaseItem()->item.asSymbol.stackstate;

   zh_clsMakeSuperObject( zh_stackReturnItem(), zh_stackSelfItem(),
      s_pClasses[ pStack->uiClass ]->pMethods[ pStack->uiMethod ].uiData );
}

/* __msgRealClass()
 *
 * Internal function to return a superobject of class where the method was
 * defined
 */
ZH_FUNC_STATIC( msgRealClass )
{
   ZH_STACK_TLS_PRELOAD
   PZH_ITEM pObject = zh_stackSelfItem();
   ZH_USHORT uiClass = zh_clsSenderMethodClass();
   ZH_USHORT uiCurClass = zh_objGetClassH( pObject );

   if( uiClass && uiClass != uiCurClass &&
       zh_clsSenderObjectClass() == uiCurClass )
   {
      zh_clsMakeSuperObject( zh_stackReturnItem(), pObject, uiClass );
   }
   else
   {
      zh_itemReturnForward( pObject );
   }
}

/* __msgGetClsData()
 *
 * Internal function to return a CLASSDATA
 */
ZH_FUNC_STATIC( msgGetClsData )
{
   ZH_STACK_TLS_PRELOAD
   PCLASS pClass   = s_pClasses[
                  zh_stackBaseItem()->item.asSymbol.stackstate->uiClass ];
   PMETHOD pMethod = pClass->pMethods +
                  zh_stackBaseItem()->item.asSymbol.stackstate->uiMethod;

   zh_arrayGet( pClass->pClassDatas, pMethod->uiData, zh_stackReturnItem() );
}


/* __msgSetClsData()
 *
 * Internal function to set a CLASSDATA
 */
ZH_FUNC_STATIC( msgSetClsData )
{
   ZH_STACK_TLS_PRELOAD
   PCLASS pClass   = s_pClasses[
                  zh_stackBaseItem()->item.asSymbol.stackstate->uiClass ];
   PMETHOD pMethod = pClass->pMethods +
                  zh_stackBaseItem()->item.asSymbol.stackstate->uiMethod;
   PZH_ITEM pReturn = zh_param( 1, ZH_IT_ANY );

   if( ! pReturn )
      zh_arrayGet( pClass->pClassDatas, pMethod->uiData, zh_stackReturnItem() );

   else
   {
      if( pMethod->itemType &&
          ! ( pMethod->itemType & ZH_ITEM_TYPERAW( pReturn ) ) )
      {
         if( pMethod->itemType == ZH_IT_NUMINT && ZH_IS_NUMERIC( pReturn ) )
            zh_itemPutNInt( pReturn, zh_itemGetNInt( pReturn ) );
         else
         {
            ( s___msgTypeErr.value.pFunPtr )();
            return;
         }
      }

      zh_arraySet( pClass->pClassDatas, pMethod->uiData, pReturn );
      zh_itemReturn( pReturn );
   }
}

/* __msgGetShrData()
 *
 * Internal function to return a SHAREDDATA
 */
ZH_FUNC_STATIC( msgGetShrData )
{
   ZH_STACK_TLS_PRELOAD
   PCLASS pClass   = s_pClasses[
                  zh_stackBaseItem()->item.asSymbol.stackstate->uiClass ];
   PMETHOD pMethod = pClass->pMethods +
                  zh_stackBaseItem()->item.asSymbol.stackstate->uiMethod;

   zh_arrayGet( s_pClasses[ pMethod->uiSprClass ]->pSharedDatas,
                pMethod->uiData, zh_stackReturnItem() );
}

/* __msgSetShrData()
 *
 * Internal function to set a SHAREDDATA
 */
ZH_FUNC_STATIC( msgSetShrData )
{
   ZH_STACK_TLS_PRELOAD
   PCLASS pClass   = s_pClasses[
                  zh_stackBaseItem()->item.asSymbol.stackstate->uiClass ];
   PMETHOD pMethod = pClass->pMethods +
                  zh_stackBaseItem()->item.asSymbol.stackstate->uiMethod;
   PZH_ITEM pReturn = zh_param( 1, ZH_IT_ANY );

   if( ! pReturn )
      zh_arrayGet( s_pClasses[ pMethod->uiSprClass ]->pSharedDatas,
                   pMethod->uiData, zh_stackReturnItem() );
   else
   {
      if( pMethod->itemType &&
          ! ( pMethod->itemType & ZH_ITEM_TYPERAW( pReturn ) ) )
      {
         if( pMethod->itemType == ZH_IT_NUMINT && ZH_IS_NUMERIC( pReturn ) )
            zh_itemPutNInt( pReturn, zh_itemGetNInt( pReturn ) );
         else
         {
            ( s___msgTypeErr.value.pFunPtr )();
            return;
         }
      }

      zh_arraySet( s_pClasses[ pMethod->uiSprClass ]->pSharedDatas,
                   pMethod->uiData, pReturn );
      zh_itemReturn( pReturn );
   }
}

/* __msgGetData()
 *
 * Internal function to return a DATA
 */
ZH_FUNC_STATIC( msgGetData )
{
   ZH_STACK_TLS_PRELOAD
   PZH_ITEM pObject  = zh_stackSelfItem();

   if( ZH_IS_ARRAY( pObject ) )
   {
      ZH_USHORT uiObjClass = pObject->item.asArray.value->uiClass;
      ZH_USHORT uiClass    = zh_stackBaseItem()->item.asSymbol.stackstate->uiClass;
      PCLASS pClass        = s_pClasses[ uiClass ];
      PMETHOD pMethod      = pClass->pMethods +
                             zh_stackBaseItem()->item.asSymbol.stackstate->uiMethod;
      ZH_SIZE nIndex       = pMethod->uiData;

      if( uiClass != uiObjClass )
      {
         nIndex += zh_clsParentInstanceOffset( s_pClasses[ uiObjClass ],
                                               pMethod->uiSprClass );
      }
      else
      {
         nIndex += pMethod->uiOffset;
      }

      zh_arrayGet( pObject, nIndex, zh_stackReturnItem() );
   }
}

/* __msgSetData()
 *
 * Internal function to set a DATA
 */
ZH_FUNC_STATIC( msgSetData )
{
   ZH_STACK_TLS_PRELOAD
   PZH_ITEM pObject  = zh_stackSelfItem();

   if( ZH_IS_ARRAY( pObject ) )
   {
      PZH_ITEM pReturn     = zh_param( 1, ZH_IT_ANY );
      ZH_USHORT uiObjClass = pObject->item.asArray.value->uiClass;
      ZH_USHORT uiClass    = zh_stackBaseItem()->item.asSymbol.stackstate->uiClass;
      PCLASS pClass        = s_pClasses[ uiClass ];
      PMETHOD pMethod      = pClass->pMethods +
                             zh_stackBaseItem()->item.asSymbol.stackstate->uiMethod;
      ZH_SIZE nIndex       = pMethod->uiData;

      if( uiClass != uiObjClass )
      {
         nIndex += zh_clsParentInstanceOffset( s_pClasses[ uiObjClass ],
                                               pMethod->uiSprClass );
      }
      else
      {
         nIndex += pMethod->uiOffset;
      }

      if( ! pReturn )
         zh_arrayGet( pObject, nIndex, zh_stackReturnItem() );

      else
      {
         if( pMethod->itemType &&
             ! ( pMethod->itemType & ZH_ITEM_TYPERAW( pReturn ) ) )
         {
            if( pMethod->itemType == ZH_IT_NUMINT && ZH_IS_NUMERIC( pReturn ) )
               zh_itemPutNInt( pReturn, zh_itemGetNInt( pReturn ) );
            else
            {
               ( s___msgTypeErr.value.pFunPtr )();
               return;
            }
         }

         /* will arise only if the class has been modified after first instance */
         if( nIndex > zh_arrayLen( pObject ) ) /* Resize needed ? */
            zh_arraySize( pObject, nIndex );   /* Make large enough */
         zh_arraySet( pObject, nIndex, pReturn );
         zh_itemReturn( pReturn );
      }
   }
}

/* No comment :-) */
ZH_FUNC_STATIC( msgVirtual )
{
   #if 0
   zh_ret(); /* NOTE: It's safe to have this commented out. */
   #endif
}

ZH_FUNC_STATIC( msgNull )
{
}

#ifndef ZH_NO_PROFILER
void zh_mthAddTime( ZH_ULONG ulClockTicks )
{
   ZH_STACK_TLS_PRELOAD
   PZH_ITEM pObject = zh_stackSelfItem();
   PCLASS pClass = s_pClasses[ zh_objGetClassH( pObject ) ];
   ZH_USHORT uiMethod = zh_stackBaseItem()->item.asSymbol.stackstate->uiMethod;

   if( pClass && uiMethod < zh_clsMthNum( pClass ) )
   {
      PMETHOD pMethod = pClass->pMethods + uiMethod;
      pMethod->ulCalls++;
      pMethod->ulTime += ulClockTicks;
      return;
   }

   if( ZH_IS_BLOCK( pObject ) )
   {
      PZH_SYMB pSym = zh_stackBaseItem()->item.asSymbol.value;

      if( pSym == &zh_symEval || pSym->pDynSym == zh_symEval.pDynSym )
      {
         pSym->pDynSym->ulCalls++;
         if( --pSym->pDynSym->ulRecurse == 0 )
            pSym->pDynSym->ulTime += ulClockTicks;
      }
   }
}
#endif

/* __GetMsgPrf( <hClass>, <cMsg> ) --> <aMethodInfo> { { <nTimes>, <nTime> }, ... } */
ZH_FUNC( __GETMSGPRF ) /* profiler: returns a method called and consumed times */
{
   ZH_STACK_TLS_PRELOAD
#ifndef ZH_NO_PROFILER
   ZH_USHORT uiClass = ( ZH_USHORT ) zh_parni( 1 );
   const char * cMsg = zh_parc( 2 );

   zh_reta( 2 );
   if( uiClass && uiClass <= s_uiClasses && cMsg && *cMsg )
   {
      PZH_DYNS pMsg = zh_dynsymFindName( cMsg );

      if( pMsg )
      {
         PMETHOD pMethod = zh_clsFindMsg( s_pClasses[ uiClass ], pMsg );

         if( pMethod )
         {
            zh_storvnl( pMethod->ulCalls, -1, 1 );
            zh_storvnl( pMethod->ulTime, -1, 2 );
            return;
         }
      }
   }
#else
   zh_reta( 2 );
#endif
   zh_storvnl( 0, -1, 1 );
   zh_storvnl( 0, -1, 2 );
}

typedef struct
{
   PMETHOD   pMethod;
   ZH_USHORT uiClass;
   ZH_USHORT uiStatus;
}
ZH_IVARINFO, * PZH_IVARINFO;

static PZH_ITEM zh_objGetIVars( PZH_ITEM pObject,
                                ZH_USHORT uiScope, ZH_BOOL fChanged )
{
   PZH_IVARINFO pIndex, pInfo;
   PCLASS pClass;
   PMETHOD pMethod;
   PZH_ITEM pReturn, pItem;
   ZH_SIZE nLimit, nLen, nCount, nSize, nIndex, nOffset;
   ZH_USHORT uiClass, uiSuperClasses;

   if( ! pObject || ! ZH_IS_OBJECT( pObject ) )
      return NULL;

   uiClass = pObject->item.asArray.value->uiClass;
   pClass = s_pClasses[ uiClass ];
   nLen = nCount = zh_arrayLen( pObject );
   nSize = 0;
   pIndex = nLen ? ( PZH_IVARINFO ) zh_xgrabz( nLen * sizeof( ZH_IVARINFO ) ) : NULL;

   if( fChanged && pClass->uiInitDatas )
   {
      PINITDATA pInitData = pClass->pInitData;

      nLimit = pClass->uiInitDatas;
      do
      {
         if( pInitData->uiType == ZH_OO_MSG_DATA )
         {
            nIndex = pInitData->uiData + pInitData->uiOffset;
            pItem = zh_arrayGetItemPtr( pObject, nIndex );
            if( pItem )
            {
               if( zh_itemEqual( pItem, pInitData->pInitValue ) )
               {
                  pIndex[ nIndex - 1 ].uiStatus = 3;
                  --nCount;
               }
               else
                  pIndex[ nIndex - 1 ].uiStatus = 1;
            }
         }
         ++pInitData;
      }
      while( --nLimit );
   }

   uiSuperClasses = pClass->uiSuperClasses;
   nOffset = 0;
   nLimit = zh_clsMthNum( pClass );
   pMethod = pClass->pMethods;
   while( nCount && nLimit )
   {
      if( pMethod->pMessage &&
          ( uiScope == 0 || ( pMethod->uiScope & uiScope ) != 0 ) &&
          ( uiClass == pClass->uiClass || uiClass == pMethod->uiSprClass ) )
      {
         PZH_SYMB pFuncSym = pMethod->pFuncSym;

         if( pFuncSym == &s___msgSync || pFuncSym == &s___msgSyncClass )
            pFuncSym = pMethod->pRealSym;

         if( pFuncSym == &s___msgGetData || pFuncSym == &s___msgSetData )
         {
            ZH_USHORT uiStatus = pFuncSym == &s___msgGetData ? 3 : 2;

            nIndex = ( uiClass == pClass->uiClass ?
                     ( ZH_SIZE ) pMethod->uiOffset : nOffset ) + pMethod->uiData;
            if( nIndex == 0 || nIndex > nLen )
               zh_errInternal( ZH_EI_CLSINVMETHOD, NULL, "__objGetIVars()", NULL );

            pInfo = &pIndex[ nIndex - 1 ];
            if( pInfo->uiStatus < uiStatus )
            {
               pItem = zh_arrayGetItemPtr( pObject, nIndex );
               if( ! pItem || ( pInfo->uiStatus == 0 && ZH_IS_NIL( pItem ) ) )
                  uiStatus = 3;
               else
               {
                  if( ! pInfo->pMethod )
                     ++nSize;
                  pInfo->pMethod = pMethod;
                  pInfo->uiClass = uiClass;
               }
               pInfo->uiStatus = uiStatus;
               if( uiStatus == 3 )
                  --nCount;
            }
         }
      }
      ++pMethod;
      if( --nLimit == 0 )
      {
         if( uiSuperClasses-- )
         {
            if( uiClass == pClass->pSuperClasses[ uiSuperClasses ].uiClass )
               if( uiSuperClasses-- == 0 )
                  break;
            uiClass = pClass->pSuperClasses[ uiSuperClasses ].uiClass;
            nOffset = pClass->pSuperClasses[ uiSuperClasses ].uiOffset;
            nLimit = zh_clsMthNum( s_pClasses[ uiClass ] );
            pMethod = s_pClasses[ uiClass ]->pMethods;
         }
      }
   }

   nCount = 0;
   pReturn = zh_itemArrayNew( nSize );
   for( nIndex = 1; nIndex <= nLen && nCount < nSize; ++nIndex )
   {
      pInfo = &pIndex[ nIndex - 1 ];
      if( pInfo->pMethod )
      {
         PZH_ITEM pValue = zh_arrayGetItemPtr( pReturn, ++nCount );
         if( pValue )
         {
            const char * pszVar = pInfo->pMethod->pMessage->pSymbol->szName;
            zh_arrayNew( pValue, 2 );
            if( pInfo->uiClass != pClass->uiClass )
            {
               char * pszCast = zh_xstrcpy( NULL, s_pClasses[ pInfo->uiClass ]->szName,
                                            ":", pszVar, NULL );
               zh_arraySetCPtr( pValue, 1, pszCast );
            }
            else
               zh_arraySetCConst( pValue, 1, pszVar );
            zh_arraySet( pValue, 2, zh_arrayGetItemPtr( pObject, nIndex ) );
         }
      }
   }

   if( pIndex )
      zh_xfree( pIndex );

   return pReturn;
}

static void zh_objSetIVars( PZH_ITEM pObject, PZH_ITEM pArray )
{
   if( pObject && ZH_IS_OBJECT( pObject ) &&
       pArray && ZH_IS_ARRAY( pArray ) &&
       pArray->item.asArray.value->uiClass == 0 )
   {
      ZH_USHORT uiClass = pObject->item.asArray.value->uiClass;
      ZH_SIZE nPos, nIndex, nLen;
      PZH_ITEM pValue;

      nPos = 0;
      while( ( pValue = zh_arrayGetItemPtr( pArray, ++nPos ) ) != NULL )
      {
         const char * pszMethod = zh_arrayGetCPtr( pValue, 1 );
         PZH_DYNS pVarSym = zh_dynsymFind( pszMethod );
         PZH_ITEM pNewVal = zh_arrayGetItemPtr( pValue, 2 );
         ZH_USHORT uiSuper = uiClass;

         if( ! pVarSym )
         {
            const char * pszClass = strchr( pszMethod, ':' );
            if( pszClass )
            {
               nLen = pszClass - pszMethod;
               if( nLen )
               {
                  PZH_DYNS pParentSym;
                  char szClassName[ ZH_SYMBOL_NAME_LEN + 1 ];

                  memcpy( szClassName, pszMethod, nLen );
                  szClassName[ nLen ] = '\0';
                  pParentSym = zh_dynsymFindName( szClassName );
                  uiSuper = pParentSym == NULL ? 0 :
                            zh_clsGetParent( s_pClasses[ uiClass ], pParentSym );
               }
               pVarSym = zh_dynsymFindName( pszClass + 1 );
            }
            else
               pVarSym = zh_dynsymFindName( pszMethod );
         }
         if( uiSuper && pNewVal && pVarSym &&
             ( nIndex = zh_clsGetVarIndexEx( uiClass, pVarSym, uiSuper ) ) != 0 )
         {
            zh_arraySet( pObject, nIndex, pNewVal );
         }
      }
   }
}

/* __objGetIVars( <oObject>, [<nScope>], [<lChanged>] )
 *          --> <aIVars> { { <cName>, <xVal> }, ... }
 */
ZH_FUNC( __OBJGETIVARS )
{
   PZH_ITEM pObject = zh_param( 1, ZH_IT_OBJECT );
   ZH_USHORT uiScope = ( ZH_USHORT ) zh_parni( 2 );
   ZH_BOOL fChanged = zh_parldef( 3, ZH_TRUE );

   zh_itemReturnRelease( zh_objGetIVars( pObject, uiScope, fChanged ) );
}

/* __objSetIVars( <oObject> | <hClass> | <cClassName> | <sClassFunc>,
 *                <aIVars> ) --> <oObject>
 */
ZH_FUNC( __OBJSETIVARS )
{
   PZH_ITEM pObject = zh_param( 1, ZH_IT_ANY );
   PZH_ITEM pArray = zh_param( 2, ZH_IT_ARRAY );

   if( pObject && pArray )
   {
      PZH_ITEM pNewObj = NULL;

      if( ZH_IS_NUMERIC( pObject ) )
         pObject = pNewObj = zh_clsInst( ( ZH_USHORT ) zh_itemGetNI( pObject ) );
      else if( ZH_IS_STRING( pObject ) )
         pObject = pNewObj = zh_clsInst( zh_clsFindClass( zh_itemGetCPtr( pObject ), NULL ) );
      else if( ZH_IS_SYMBOL( pObject ) )
         pObject = pNewObj = zh_clsInst( zh_clsFindClassByFunc( zh_itemGetSymbol( pObject ) ) );
      else if( ! ZH_IS_OBJECT( pObject ) )
         pObject = NULL;

      zh_objSetIVars( pObject, pArray );

      if( pObject )
         zh_itemReturn( pObject );
      if( pNewObj )
         zh_itemRelease( pNewObj );
   }
}

/* __objRestoreIVars( <aIVars>, <hClass> | <sClassFunc> |
                                <cClassName>[, <cClassFuncName>] ) --> <oObject>
 */
ZH_FUNC( __OBJRESTOREIVARS )
{
   PZH_ITEM pArray = zh_param( 1, ZH_IT_ARRAY );
   PZH_ITEM pClass = zh_param( 2, ZH_IT_NUMERIC | ZH_IT_STRING | ZH_IT_SYMBOL );

   if( pClass && pArray && pArray->item.asArray.value->uiClass == 0 )
   {
      PZH_ITEM pObject = NULL;


      if( ZH_IS_NUMERIC( pClass ) )
         pObject = zh_clsInst( ( ZH_USHORT ) zh_itemGetNI( pClass ) );
      else if( ZH_IS_STRING( pClass ) )
         pObject = zh_clsInst( zh_clsFindClass( zh_itemGetCPtr( pClass ), zh_parc( 3 ) ) );
      else if( ZH_IS_SYMBOL( pClass ) )
         pObject = zh_clsInst( zh_clsFindClassByFunc( zh_itemGetSymbol( pClass ) ) );

      if( pObject )
      {
         zh_objSetIVars( pObject, pArray );
         zh_arraySwap( pObject, pArray );
         zh_itemRelease( pObject );
      }
   }

   zh_itemReturn( pArray );
}

/* __clsGetProperties( <nClassHandle>, [<lAllExported>] ) --> <acProperties>
 * Notice that this function works quite similar to __classSel()
 * except that just returns the name of the datas and methods
 * that have been declared as PROPERTY (PERSISTENT) or also EXPORTED
 * if second parameter <lAllExported> is true and message has corresponding
 * assign message (with "_" prefix)
 */
ZH_FUNC( __CLSGETPROPERTIES )
{
   ZH_USHORT uiClass = ( ZH_USHORT ) zh_parni( 1 );
   PZH_ITEM pReturn = zh_itemNew( NULL );

   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS pClass = s_pClasses[ uiClass ];
      PMETHOD pMethod, pAccMth;
      ZH_SIZE nLimit, nCount;
      ZH_USHORT uiScope = ZH_OO_CLSTP_PERSIST;

      if( zh_parl( 2 ) )
         uiScope |= ZH_OO_CLSTP_EXPORTED;

      nCount = 0;
      nLimit = zh_clsMthNum( pClass );
      pMethod = pClass->pMethods;
      do
      {
         if( pMethod->pMessage && ( pMethod->uiScope & uiScope ) != 0 )
         {
            if( ( pMethod->uiScope & ZH_OO_CLSTP_PERSIST ) != 0 )
               ++nCount;
            else if( pMethod->pMessage->pSymbol->szName[ 0 ] == '_' )
            {
               if( ! pMethod->pAccMsg )
                  pMethod->pAccMsg = zh_dynsymGetCase( pMethod->pMessage->pSymbol->szName + 1 );
               pAccMth = zh_clsFindMsg( pClass, pMethod->pAccMsg );
               if( pAccMth && ( pAccMth->uiScope & ZH_OO_CLSTP_PERSIST ) == 0 )
                  ++nCount;
            }
         }
         ++pMethod;
      }
      while( --nLimit );

      zh_arrayNew( pReturn, nCount );

      nCount = 0;
      nLimit = zh_clsMthNum( pClass );
      pMethod = pClass->pMethods;
      do
      {
         if( pMethod->pMessage && ( pMethod->uiScope & uiScope ) != 0 )
         {
            if( ( pMethod->uiScope & ZH_OO_CLSTP_PERSIST ) != 0 )
               zh_arraySetC( pReturn, ++nCount, pMethod->pMessage->pSymbol->szName );
            else if( pMethod->pMessage->pSymbol->szName[ 0 ] == '_' &&
                     pMethod->pAccMsg )
            {
               pAccMth = zh_clsFindMsg( pClass, pMethod->pAccMsg );
               if( pAccMth && ( pAccMth->uiScope & ZH_OO_CLSTP_PERSIST ) == 0 )
                  zh_arraySetC( pReturn, ++nCount, pMethod->pMessage->pSymbol->szName + 1 );
            }
         }
         ++pMethod;
      }
      while( --nLimit );
   }

   zh_itemReturnRelease( pReturn );
}

/* __clsGetAncestors( <nClass> ) --> { <nSuper1>, <nSuper2>, ... } */
ZH_FUNC( __CLSGETANCESTORS )
{
   ZH_USHORT uiClass = ( ZH_USHORT ) zh_parni( 1 ), uiCount;

   if( uiClass && uiClass <= s_uiClasses )
   {
      ZH_STACK_TLS_PRELOAD
      PZH_ITEM pReturn = zh_stackReturnItem();
      PCLASS pClass = s_pClasses[ uiClass ];
      ZH_SIZE nPos = 0;

      uiCount = pClass->uiSuperClasses;
      zh_arrayNew( pReturn, uiCount );
      while( uiCount-- )
      {
         ZH_USHORT uiSuperCls = pClass->pSuperClasses[ uiCount ].uiClass;
         if( uiSuperCls != uiClass )
            zh_arraySetNI( pReturn, ++nPos, uiSuperCls );
      }
      zh_arraySize( pReturn, nPos );
   }
}

/* __clsMsgType( <hClass>, <cMsgName> | <sMsgName> ) --> <nType>
 *
 * return type of method attached to given message,
 * <nType> is one of ZH_OO_MSG_* values defined in oo.zhh or
 * -1 if message is not supported.
 */
ZH_FUNC( __CLSMSGTYPE )
{
   PZH_DYNS pMessage = zh_objGetMsgSym( zh_param( 2, ZH_IT_ANY ) );

   if( pMessage )
   {
      ZH_STACK_TLS_PRELOAD
      ZH_USHORT uiClass = ( ZH_USHORT ) zh_parni( 1 );
      PMETHOD pMethod = NULL;

      if( uiClass && uiClass <= s_uiClasses )
         pMethod = zh_clsFindMsg( s_pClasses[ uiClass ], pMessage );

      zh_retni( pMethod ? zh_methodType( pMethod ) : -1 );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1099, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/* extend the size of classes buffer to given value to avoid later
 * RT reallocations. It may be useful in some very seldom cases
 * for MT programs which will allocate dynamically at runtime
 * more then 16386 classes. In practice rather impossible though
 * who knows ;-)
 * __clsPreallocate( [<nMaxClasses>] ) --> <nMaxClasses>
 */
ZH_FUNC( __CLSPREALLOCATE )
{
   ZH_STACK_TLS_PRELOAD
   ZH_LONG lNewSize = zh_parnl( 1 );

   if( lNewSize > ( ZH_LONG ) USHRT_MAX )
      lNewSize = USHRT_MAX;

   ZH_CLASS_LOCK();

   if( lNewSize > ( ZH_LONG ) s_uiClsSize )
   {
      s_uiClsSize = ( ZH_USHORT ) lNewSize;
      s_pClasses = ( PCLASS * ) zh_xrealloc( s_pClasses, sizeof( PCLASS ) *
                                             ( ( ZH_SIZE ) s_uiClsSize + 1 ) );
   }

   ZH_CLASS_UNLOCK();

   zh_retnl( s_uiClsSize );
}

/* __clsLockDef( <clsItem> ) --> <lLocked> */
ZH_FUNC( __CLSLOCKDEF )
{
   ZH_STACK_TLS_PRELOAD
   PZH_ITEM pClsItm = zh_param( 1, ZH_IT_BYREF );
   ZH_BOOL fLocked = ZH_FALSE;

   if( pClsItm && ZH_IS_NIL( pClsItm ) )
   {
      if( ! s_pClassMtx || zh_threadMutexLock( s_pClassMtx ) )
      {
         if( ZH_IS_NIL( pClsItm ) )
            fLocked = ZH_TRUE;
         else if( s_pClassMtx )
            zh_threadMutexUnlock( s_pClassMtx );
      }
   }
   zh_retl( fLocked );
}

/* __clsUnlockDef( @<clsItem>, <clsDef> ) */
ZH_FUNC( __CLSUNLOCKDEF )
{
   PZH_ITEM pClsDst = zh_param( 1, ZH_IT_BYREF ),
            pClsSrc = zh_param( 2, ZH_IT_ANY );

   if( pClsDst && pClsSrc && ZH_IS_NIL( pClsDst ) && ! ZH_ISBYREF( 2 ) )
   {
      /* special core code only macro used to eliminate race condition
       * in unprotected readonly access to pClsDst variable.
       */
      zh_itemSafeMove( pClsDst, pClsSrc );
   }

   if( s_pClassMtx )
      zh_threadMutexUnlock( s_pClassMtx );
}

/* Dirty functions which converts array to object of given class
 * __objSetClass( <oObject>, <cClassName> [, <cClassFuncName> ] ) --> <oObject>
 */
ZH_FUNC( __OBJSETCLASS )
{
   PZH_ITEM pObject = zh_param( 1, ZH_IT_ARRAY );

   if( pObject && pObject->item.asArray.value->uiClass == 0 )
   {
      const char * szClass = zh_parc( 2 );

      if( szClass )
         zh_objSetClass( pObject, szClass, zh_parc( 3 ) );
   }

   zh_itemReturn( pObject );
}

/* Real dirty function, though very useful under certain circumstances:
 * It allows to change the class handle of an object into another class handle,
 * so the object behaves like a different Class of object.
 * Based on objects.lib SetClsHandle()
 * __objSetClassHandle( <oObject>, <nClassHandle> ) --> <nPrevClassHandle>
 */
ZH_FUNC( __OBJSETCLASSHANDLE )
{
   ZH_STACK_TLS_PRELOAD
   PZH_ITEM pObject = zh_param( 1, ZH_IT_OBJECT );
   ZH_USHORT uiPrevClassHandle = 0;

   if( pObject )
   {
      ZH_USHORT uiClass = ( ZH_USHORT ) zh_parni( 2 );

      uiPrevClassHandle = pObject->item.asArray.value->uiClass;
      if( uiClass <= s_uiClasses )
         pObject->item.asArray.value->uiClass = uiClass;
   }

   zh_retnl( uiPrevClassHandle );
}


ZH_USHORT zh_clsCreate( ZH_USHORT usSize, const char * szClassName )
{
   return zh_clsNew( szClassName, usSize, NULL, NULL, ZH_FALSE );
}

void zh_clsAdd( ZH_USHORT usClassH, const char * szMethodName, PZH_FUNC pFuncPtr )
{
   PZH_SYMB pExecSym;
   PZH_ITEM pFuncItem;

   /* We can use empty name "" for this symbol in zh_symbolNew()
    * It's only envelop for function with additional execution
    * information for ZHVM not registered symbol. [druzus]
    */
   pExecSym = zh_symbolNew( "" );
   pExecSym->value.pFunPtr = pFuncPtr;
   pFuncItem = zh_itemPutSymbol( NULL, pExecSym );

   zh_clsAddMsg( usClassH, szMethodName, ZH_OO_MSG_METHOD, 0, pFuncItem, NULL );

   zh_itemRelease( pFuncItem );
}


void zh_clsAssociate( ZH_USHORT usClassH )
{
   PZH_ITEM pSelf = zh_clsInst( usClassH );

   if( pSelf )
      zh_itemReturnRelease( pSelf );
   else
   {
      ZH_STACK_TLS_PRELOAD
      zh_ret();
   }
}

ZH_FUNC( __CLSVERIFY )
{
   ZH_USHORT uiClass = ( ZH_USHORT ) zh_parni( 1 );
   PZH_ITEM pReturn = zh_itemNew( NULL );

   if( uiClass && uiClass <= s_uiClasses )
   {
      PCLASS pClass = s_pClasses[ uiClass ];
      PMETHOD pMethod = pClass->pMethods;
      ZH_SIZE nLimit = zh_clsMthNum( pClass ), nPos = 0;

      zh_arrayNew( pReturn, pClass->uiMethods );
      do
      {
         if( pMethod->pMessage )
         {
            PZH_DYNS pDynSym = zh_dynsymFind( pMethod->pMessage->pSymbol->szName );

            if( pMethod->pMessage != pDynSym ||
                zh_clsFindMsg( pClass, pDynSym ) != pMethod )
               zh_arraySetC( pReturn, ++nPos, pMethod->pMessage->pSymbol->szName );
         }
         ++pMethod;
      }
      while( --nLimit );

      if( nPos < ( ZH_SIZE ) pClass->uiMethods )
         zh_arraySize( pReturn, nPos );
   }

   zh_itemReturnRelease( pReturn );
}


