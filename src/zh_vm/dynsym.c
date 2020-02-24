/*
 * Dynamic symbol table management
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

#include "zh_vm_opt.h"
#include "zh_api.h"
#include "zh_item_api.h"
#include "zh_error_api.h"
#include "zh_stack.h"

typedef struct
{
   PZH_DYNS pDynSym;             /* Pointer to dynamic symbol */
} DYNZH_ITEM, * PDYNZH_ITEM;

typedef struct _ZH_SYM_HOLDER
{
   ZH_SYMB  symbol;
   struct _ZH_SYM_HOLDER * pNext;
   char     szName[ 1 ];
}
ZH_SYM_HOLDER, * PZH_SYM_HOLDER;


#  include "zh_thread.h"

   static ZH_CRITICAL_NEW( s_dynsMtx );
#  define ZH_DYNSYM_LOCK()      zh_threadEnterCriticalSection( &s_dynsMtx )
#  define ZH_DYNSYM_UNLOCK()    zh_threadLeaveCriticalSection( &s_dynsMtx )

#  define zh_dynsymHandles( p )     zh_stackGetDynHandle( p )


static PDYNZH_ITEM s_pDynItems = NULL;    /* Pointer to dynamic items */
static ZH_USHORT   s_uiDynSymbols = 0;    /* Number of symbols present */

static PZH_SYM_HOLDER s_pAllocSyms = NULL;/* symbols allocated dynamically */

/* table index for dynamic symbol to number conversions */
static PDYNZH_ITEM s_pDynIndex = NULL;
static int         s_iDynIdxSize = 0;

/* Insert new symbol into dynamic symbol table.
 * In MT mode caller should protected it by ZH_DYNSYM_LOCK()
 */
static PZH_DYNS zh_dynsymInsert( PZH_SYMB pSymbol, ZH_UINT uiPos )
{
   PZH_DYNS pDynSym;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dynsymInsert(%p, %u)", ( void * ) pSymbol, uiPos ) );

   if( ++s_uiDynSymbols == 0 )
   {
      --s_uiDynSymbols;
      zh_errInternal( 6004, "Internal error: size of dynamic symbol table exceed", NULL, NULL );
   }
   else if( s_uiDynSymbols == 1 )
   {
      s_pDynItems = ( PDYNZH_ITEM ) zh_xgrab( sizeof( DYNZH_ITEM ) );
   }
   else
   {
      s_pDynItems = ( PDYNZH_ITEM ) zh_xrealloc( s_pDynItems, s_uiDynSymbols * sizeof( DYNZH_ITEM ) );
      memmove( &s_pDynItems[ uiPos + 1 ], &s_pDynItems[ uiPos ],
               sizeof( DYNZH_ITEM ) * ( s_uiDynSymbols - uiPos - 1 ) );
   }

   pDynSym = ( PZH_DYNS ) zh_xgrabz( sizeof( ZH_DYNS ) );
   pDynSym->pSymbol  = pSymbol;
   pDynSym->uiSymNum = s_uiDynSymbols;

   pSymbol->pDynSym = s_pDynItems[ uiPos ].pDynSym = pDynSym;

   return pDynSym;
}

/* Find symbol in dynamic symbol table and set it's position.
 * If not found set position for insert operation.
 * In MT mode caller should protected it by ZH_DYNSYM_LOCK()
 */
static PZH_DYNS zh_dynsymPos( const char * szName, ZH_UINT * puiPos )
{
   ZH_UINT uiFirst, uiLast, uiMiddle;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dynsymPos(%s, %p)", szName, ( void * ) puiPos ) );

   uiFirst = 0;
   uiLast = s_uiDynSymbols;
   uiMiddle = uiLast >> 1;

   while( uiFirst < uiLast )
   {
      int iCmp = strcmp( s_pDynItems[ uiMiddle ].pDynSym->pSymbol->szName, szName );

      if( iCmp == 0 )
      {
         *puiPos = uiMiddle;
         return s_pDynItems[ uiMiddle ].pDynSym;
      }
      else if( iCmp < 0 )
         uiLast = uiMiddle;
      else /* if( iCmp > 0 ) */
         uiFirst = uiMiddle + 1;
      uiMiddle = ( uiFirst + uiLast ) >> 1;
   }

   *puiPos = uiMiddle;

   return NULL;
}

/* Create new symbol.
 * In MT mode caller should protected it by ZH_DYNSYM_LOCK()
 */
static PZH_SYMB zh_symbolAlloc( const char * szName )
{
   PZH_SYM_HOLDER pHolder;
   int iLen;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_symbolAlloc(%s)", szName ) );

   iLen = ( int ) strlen( szName );
   pHolder = ( PZH_SYM_HOLDER ) zh_xgrab( sizeof( ZH_SYM_HOLDER ) + iLen );
   memcpy( pHolder->szName, szName, iLen + 1 );
   pHolder->pNext = s_pAllocSyms;
   s_pAllocSyms = pHolder;

   pHolder->symbol.szName        = pHolder->szName;
   pHolder->symbol.scope.value   = 0;
   pHolder->symbol.value.pFunPtr = NULL;
   pHolder->symbol.pDynSym       = NULL;

   return &pHolder->symbol;
}

/* Find symbol in dynamic symbol table */
PZH_DYNS zh_dynsymFind( const char * szName )
{
   ZH_UINT uiFirst, uiLast;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dynsymFind(%s)", szName ) );

   ZH_DYNSYM_LOCK();

   uiFirst = 0;
   uiLast = s_uiDynSymbols;

   while( uiFirst < uiLast )
   {
      ZH_UINT uiMiddle = ( uiFirst + uiLast ) >> 1;
      int iCmp = strcmp( s_pDynItems[ uiMiddle ].pDynSym->pSymbol->szName, szName );

      if( iCmp == 0 )
      {
         ZH_DYNSYM_UNLOCK();
         return s_pDynItems[ uiMiddle ].pDynSym;
      }
      else if( iCmp < 0 )
         uiLast = uiMiddle;
      else /* if( iCmp > 0 ) */
         uiFirst = uiMiddle + 1;
   }

   ZH_DYNSYM_UNLOCK();

   return NULL;
}

/* Create new symbol */
PZH_SYMB zh_symbolNew( const char * szName )
{
   PZH_SYMB pSymbol;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_symbolNew(%s)", szName ) );

   ZH_DYNSYM_LOCK();

   pSymbol = zh_symbolAlloc( szName );

   ZH_DYNSYM_UNLOCK();

   return pSymbol;
}

/* creates a new dynamic symbol */
PZH_DYNS zh_dynsymNew( PZH_SYMB pSymbol )
{
   PZH_DYNS pDynSym;
   ZH_UINT uiPos;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dynsymNew(%p)", ( void * ) pSymbol ) );

   ZH_DYNSYM_LOCK();

   pDynSym = zh_dynsymPos( pSymbol->szName, &uiPos ); /* Find position */
   if( ! pDynSym )
      pDynSym = zh_dynsymInsert( pSymbol, uiPos );
   else
   {
      pSymbol->pDynSym = pDynSym;

      if( ( pDynSym->pSymbol->scope.value &
            pSymbol->scope.value & ZH_FS_LOCAL ) != 0 &&
            pDynSym->pSymbol != pSymbol )
      {
         /* Someone is using linker which allows to create binaries
          * with multiple function definitions. It's a big chance that
          * wrong binaries are created in such case, f.e both functions
          * linked and not all references updated. Anyhow now we will
          * have to guess which symbol is the real local one [druzus]
          */
         /* Let's check if linker updated function address so both symbols
          * refer to the same function
          */
         if( pDynSym->pSymbol->value.pFunPtr == pSymbol->value.pFunPtr )
         {
            /* The addresses have been updated, e.g. in such way works GCC
             * in Linux (but not MinGW and DJGPP) if user will allow to create
             * binaries with multiple symbols by
             *    -Wl,--allow-multiple-definition
             * when whole module cannot be cleanly replaced.
             * OpenWatcom for Linux, MS-DOS and Windows (I haven't tested OS2
             * version), POCC (with /FORCE:MULTIPLE) also update
             * addresses in such case.
             *
             * We are guessing that symbols are registered in reverted order
             * so we remove the ZH_FS_LOCAL flag from previously registered
             * symbol but some linkers may use different order so it does
             * not have to be true in all cases
             */
            pDynSym->pSymbol->scope.value &= ~ZH_FS_LOCAL;
         }
         else
         {
            /* We have multiple symbol with the same name which refer
             * to different public functions inside this single binary
             * Let's check if this symbol is loaded from dynamic library
             * (.so, .dll, .dyn, ...) or .zhb file
             */
            if( pSymbol->scope.value & ZH_FS_PCODEFUNC )
            {
               /* It's dynamic module so we are guessing that ZHVM
                * intentionally not updated function address allowing
                * multiple functions, e.g. programmer asked about keeping
                * local references using zh_libLoad()/zh_zhbLoad() parameter.
                * In such case update pDynSym address in the new symbol but
                * do not register it as the main one
                */
               ZH_DYNSYM_UNLOCK();
               return pDynSym;                /* Return pointer to DynSym */
            }
            /* The multiple symbols comes from single binaries - we have to
             * decide what to do with them. We can leave it as is or we can
             * try to overload one symbol so both will point to the same
             * function. For .zh code such overloading will work but not
             * for C code which makes something like: ZH_FUNC_EXEC( funcname );
             * In such case we cannot do anything - we cannot even detect
             * such situation. In some cases even linker cannot detect it
             * because C compiler can make auto-inlining or some bindings
             * which are not visible for linker
             */
            /* Let's try to overload one of the functions. Simple:
             *    pDynSym->pSymbol->value.pFunPtr = pSymbol->value.pFunPtr;
             * is not good idea because it's possible that this symbol will
             * be overloaded yet another time after preprocessing rest of
             * symbols so we will use ZH_FS_DEFERRED flag which is updated
             * dynamically in zh_vmSend()/zh_vmDo() functions
             */
#define ZH_OVERLOAD_MULTIPLE_FUNC

#if defined( ZH_OVERLOAD_MULTIPLE_FUNC )
            /* In such way works MinGW, DJGPP, BCC */
#if defined( __GNUC__ ) && ! defined( __DJGPP__ )
            /* MinGW (like most of other GCC ports) uses reverted order for
             * initialization functions
             */
            pDynSym->pSymbol->scope.value &= ~ZH_FS_LOCAL;
            pDynSym->pSymbol->scope.value |= ZH_FS_DEFERRED;
#else
            /* BCC, DJGPP, ... */
            pSymbol->scope.value &= ~ZH_FS_LOCAL;
            pSymbol->scope.value |= ZH_FS_DEFERRED;
#endif
#endif
         }
      }

      if( ( ! pDynSym->pSymbol->value.pFunPtr && pSymbol->value.pFunPtr ) ||
          ( pSymbol->scope.value & ZH_FS_LOCAL ) != 0 )
      {
         pDynSym->pSymbol = pSymbol;
#ifndef ZH_NO_PROFILER
         pDynSym->ulCalls = 0;
         pDynSym->ulTime  = 0;
         pDynSym->ulRecurse = 0;
#endif
      }
   }

   ZH_DYNSYM_UNLOCK();

   return pDynSym;
}

/* finds and creates a symbol if not found */
PZH_DYNS zh_dynsymGetCase( const char * szName )
{
   PZH_DYNS pDynSym;
   ZH_UINT uiPos;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dynsymGetCase(%s)", szName ) );

   ZH_DYNSYM_LOCK();

   pDynSym = zh_dynsymPos( szName, &uiPos );
   if( ! pDynSym )
      pDynSym = zh_dynsymInsert( zh_symbolAlloc( szName ), uiPos );

   ZH_DYNSYM_UNLOCK();

   return pDynSym;
}

PZH_DYNS zh_dynsymGet( const char * szName )  /* finds and creates a symbol if not found */
{
   char szUprName[ ZH_SYMBOL_NAME_LEN + 1 ];

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dynsymGet(%s)", szName ) );

   /* make a copy as we may get a const string, then turn it to uppercase */
   /* NOTE: This block is optimized for speed [vszakats] */
   {
      int iLen = ZH_SYMBOL_NAME_LEN;
      char * pDest = szUprName;

      do
      {
         char cChar = *szName++;
         if( cChar == 0 || cChar == ' ' || cChar == '\t' )
            break;
         else if( cChar >= 'a' && cChar <= 'z' )
            *pDest++ = cChar - ( 'a' - 'A' );
         else
            *pDest++ = cChar;
      }
      while( --iLen );
      *pDest = '\0';
   }

   return zh_dynsymGetCase( szUprName );
}

PZH_DYNS zh_dynsymFindName( const char * szName )  /* finds a symbol */
{
   char szUprName[ ZH_SYMBOL_NAME_LEN + 1 ];

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dynsymFindName(%s)", szName ) );

   /* make a copy as we may get a const string, then turn it to uppercase */
   /* NOTE: This block is optimized for speed [vszakats] */
   {
      int iLen = ZH_SYMBOL_NAME_LEN;
      char * pDest = szUprName;

      do
      {
         char cChar = *szName++;
         if( cChar == 0 || cChar == ' ' || cChar == '\t' )
            break;
         else if( cChar >= 'a' && cChar <= 'z' )
            *pDest++ = cChar - ( 'a' - 'A' );
         else
            *pDest++ = cChar;
      }
      while( --iLen );
      *pDest = '\0';
   }

   return zh_dynsymFind( szUprName );
}

PZH_SYMB zh_dynsymGetSymbol( const char * szName )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dynsymGetSymbol(%s)", szName ) );

   return zh_dynsymGet( szName )->pSymbol;
}

PZH_SYMB zh_dynsymFindSymbol( const char * szName )
{
   PZH_DYNS pDynSym;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dynsymFindSymbol(%s)", szName ) );

   pDynSym = zh_dynsymFind( szName );
   return pDynSym ? pDynSym->pSymbol : NULL;
}

PZH_SYMB zh_dynsymSymbol( PZH_DYNS pDynSym )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dynsymSymbol(%p)", ( void * ) pDynSym ) );

   return pDynSym->pSymbol;
}

const char * zh_dynsymName( PZH_DYNS pDynSym )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dynsymName(%p)", ( void * ) pDynSym ) );

   return pDynSym->pSymbol->szName;
}

ZH_BOOL zh_dynsymIsFunction( PZH_DYNS pDynSym )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dynsymIsFunction(%p)", ( void * ) pDynSym ) );

   return pDynSym->pSymbol->value.pFunPtr != NULL;
}

ZH_BOOL zh_dynsymIsMemvar( PZH_DYNS pDynSym )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dynsymIsMemvar(%p)", ( void * ) pDynSym ) );

   return zh_dynsymHandles( pDynSym )->pMemvar != NULL;
}

PZH_ITEM zh_dynsymGetMemvar( PZH_DYNS pDynSym )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dynsymGetMemvar(%p)", ( void * ) pDynSym ) );

   return ( PZH_ITEM ) zh_dynsymHandles( pDynSym )->pMemvar;
}

void zh_dynsymSetMemvar( PZH_DYNS pDynSym, PZH_ITEM pMemvar )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dynsymSetMemvar(%p, %p)", ( void * ) pDynSym, ( void * ) pMemvar ) );

   zh_dynsymHandles( pDynSym )->pMemvar = ( void * ) pMemvar;
}

int zh_dynsymAreaHandle( PZH_DYNS pDynSym )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dynsymAreaHandle(%p)", ( void * ) pDynSym ) );

   return zh_dynsymHandles( pDynSym )->uiArea;
}

void zh_dynsymSetAreaHandle( PZH_DYNS pDynSym, int iArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dynsymSetAreaHandle(%p, %d)", ( void * ) pDynSym, iArea ) );

   zh_dynsymHandles( pDynSym )->uiArea = ( ZH_USHORT ) iArea;
}

static PZH_DYNS zh_dynsymGetByIndex( ZH_LONG lIndex )
{
   PZH_DYNS pDynSym = NULL;

   ZH_DYNSYM_LOCK();

   if( lIndex >= 1 && lIndex <= s_uiDynSymbols )
      pDynSym = s_pDynItems[ lIndex - 1 ].pDynSym;

   ZH_DYNSYM_UNLOCK();

   return pDynSym;
}

ZH_LONG zh_dynsymCount( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dynsymCount()" ) );

   return s_uiDynSymbols;
}

int zh_dynsymToNum( PZH_DYNS pDynSym )
{
   int iSymNum;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dynsymToNum(%p)", ( void * ) pDynSym ) );

   ZH_DYNSYM_LOCK();

   iSymNum = pDynSym->uiSymNum;

   if( iSymNum > s_iDynIdxSize )
   {
      s_pDynIndex = ( PDYNZH_ITEM )
                    zh_xrealloc( s_pDynIndex, iSymNum * sizeof( DYNZH_ITEM ) );
      memset( &s_pDynIndex[ s_iDynIdxSize ], 0, ( iSymNum - s_iDynIdxSize ) *
                                                sizeof( DYNZH_ITEM ) );
      s_iDynIdxSize = iSymNum;
   }

   if( s_pDynIndex[ iSymNum - 1 ].pDynSym == NULL )
      s_pDynIndex[ iSymNum - 1 ].pDynSym = pDynSym;

   ZH_DYNSYM_UNLOCK();

   return iSymNum;
}

PZH_DYNS zh_dynsymFromNum( int iSymNum )
{
   PZH_DYNS pDynSym;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dynsymFromNum(%d)", iSymNum ) );

   ZH_DYNSYM_LOCK();

   pDynSym = iSymNum > 0 && iSymNum <= s_iDynIdxSize ?
             s_pDynIndex[ iSymNum - 1 ].pDynSym : NULL;

   ZH_DYNSYM_UNLOCK();

   return pDynSym;
}

void zh_dynsymEval( PZH_DYNS_FUNC pFunction, void * Cargo )
{
   PZH_DYNS pDynSym = NULL;
   ZH_USHORT uiPos = 0;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dynsymEval(%p, %p)", ( void * ) pFunction, Cargo ) );

   for( ;; )
   {

      ZH_DYNSYM_LOCK();

      if( pDynSym )
      {
         /* protection against resizing dynamic symbol by
          * user function or other thread in MT mode
          */
         while( s_pDynItems[ uiPos ].pDynSym != pDynSym )
         {
            if( ++uiPos >= s_uiDynSymbols )
               break;
         }
      }
      if( ++uiPos < s_uiDynSymbols )
         pDynSym = s_pDynItems[ uiPos ].pDynSym;
      else
         pDynSym = NULL;

      ZH_DYNSYM_UNLOCK();

      if( ! pDynSym || ! ( pFunction ) ( pDynSym, Cargo ) )
         break;
   }
}

void zh_dynsymProtectEval( PZH_DYNS_FUNC pFunction, void * Cargo )
{
   ZH_USHORT uiPos = 0;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dynsymProtectEval(%p, %p)", ( void * ) pFunction, Cargo ) );

   ZH_DYNSYM_LOCK();

   while( uiPos < s_uiDynSymbols )
   {
      if( ! ( pFunction ) ( s_pDynItems[ uiPos++ ].pDynSym, Cargo ) )
         break;
   }

   ZH_DYNSYM_UNLOCK();
}

void zh_dynsymRelease( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dynsymRelease()" ) );

   ZH_DYNSYM_LOCK();

   if( s_iDynIdxSize )
   {
      zh_xfree( s_pDynIndex );
      s_pDynIndex = NULL;
      s_iDynIdxSize = 0;
   }

   if( s_uiDynSymbols )
   {
      do
      {
         zh_xfree( ( s_pDynItems + --s_uiDynSymbols )->pDynSym );
      }
      while( s_uiDynSymbols );
      zh_xfree( s_pDynItems );
      s_pDynItems = NULL;
   }

   while( s_pAllocSyms )
   {
      PZH_SYM_HOLDER pHolder = s_pAllocSyms;
      s_pAllocSyms = s_pAllocSyms->pNext;
      zh_xfree( pHolder );
   }

   ZH_DYNSYM_UNLOCK();
}

ZH_FUNC( __DYNSCOUNT ) /* How much symbols do we have: dsCount = __dynsymCount() */
{
   ZH_STACK_TLS_PRELOAD
   zh_retnint( s_uiDynSymbols );
}

ZH_FUNC( __DYNSGETNAME ) /* Get name of symbol: cSymbol = __dynsymGetName( dsIndex ) */
{
   ZH_STACK_TLS_PRELOAD
   PZH_DYNS pDynSym = zh_dynsymGetByIndex( zh_parnl( 1 ) );

   zh_retc( pDynSym ? pDynSym->pSymbol->szName : NULL );
}

ZH_FUNC( __DYNSGETINDEX ) /* Gimme index number of symbol: dsIndex = __dynsymGetIndex( cSymbol ) */
{
   ZH_STACK_TLS_PRELOAD
   ZH_UINT uiPos = 0;
   const char * szName = zh_parc( 1 );

   if( szName )
   {
      PZH_DYNS pDynSym = zh_dynsymFindName( szName );
      if( pDynSym )
      {
         ZH_DYNSYM_LOCK();
         if( zh_dynsymPos( pDynSym->pSymbol->szName, &uiPos ) )
            ++uiPos;
         else
            uiPos = 0;
         ZH_DYNSYM_UNLOCK();
      }
   }

   zh_retnint( uiPos );
}

ZH_FUNC( ZH_ISFUNCTION ) /* returns .T. if a symbol has a function/procedure pointer,
                            given its name */
{
   ZH_STACK_TLS_PRELOAD
   const char * szProc = zh_parc( 1 );
   ZH_BOOL fResult = ZH_FALSE;

   if( szProc )
   {
      PZH_DYNS pDynSym = zh_dynsymFindName( szProc );
      if( pDynSym )
         fResult = zh_dynsymIsFunction( pDynSym );
   }

   zh_retl( fResult );
}

ZH_FUNC( __DYNSISFUN ) /* returns .T. if a symbol has a function/procedure pointer,
                          given its symbol index or name */
{
   ZH_STACK_TLS_PRELOAD
   const char * szName = zh_parc( 1 );
   PZH_DYNS pDynSym = szName ? zh_dynsymFindName( szName ) :
                               zh_dynsymGetByIndex( zh_parnl( 1 ) );

   zh_retl( pDynSym && zh_dynsymIsFunction( pDynSym ) );
}

ZH_FUNC( __DYNSGETPRF ) /* profiler: It returns an array with a function or procedure
                                     called and consumed times { nTimes, nTime },
                                     given the dynamic symbol index */
{
   ZH_STACK_TLS_PRELOAD
#ifndef ZH_NO_PROFILER
   PZH_DYNS pDynSym = zh_dynsymGetByIndex( zh_parnl( 1 ) );
#endif

   zh_reta( 2 );
   zh_storvnl( 0, -1, 1 );
   zh_storvnl( 0, -1, 2 );

#ifndef ZH_NO_PROFILER
   if( pDynSym )
   {
      if( zh_dynsymIsFunction( pDynSym ) ) /* it is a function or procedure */
      {
         zh_storvnl( pDynSym->ulCalls, -1, 1 );
         zh_storvnl( pDynSym->ulTime,  -1, 2 );
      }
   }
#endif
}

ZH_FUNC( __DYNSN2PTR )
{
   ZH_STACK_TLS_PRELOAD
   const char * szName = zh_parc( 1 );

   zh_retptr( szName ? zh_dynsymGet( szName ) : NULL );
}

ZH_FUNC( __DYNSN2SYM )
{
   ZH_STACK_TLS_PRELOAD
   const char * szName = zh_parc( 1 );

   if( szName )
      zh_itemPutSymbol( zh_stackReturnItem(), zh_dynsymGet( szName )->pSymbol );
}

ZH_FUNC( __DYNSP2NAME )
{
   ZH_STACK_TLS_PRELOAD
   PZH_DYNS pDynSym = ( PZH_DYNS ) zh_parptr( 1 );

   zh_retc( pDynSym != NULL ? pDynSym->pSymbol->szName : NULL );
}

/* internal function used to debug dynamic symbol integrity */
static int zh_dynsymVerify( void )
{
   ZH_USHORT uiPos = 0;
   int iResult = 0;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dynsymVerify()" ) );

   ZH_DYNSYM_LOCK();

   while( iResult == 0 && uiPos < s_uiDynSymbols )
   {
      PZH_DYNS pDynSym = s_pDynItems[ uiPos ].pDynSym;
      ZH_UINT uiAt;
      int iCmp;

      if( uiPos > 0 &&
          ( iCmp = strcmp( s_pDynItems[ uiPos - 1 ].pDynSym->pSymbol->szName,
                           pDynSym->pSymbol->szName ) ) <= 0 )
         iResult = iCmp == 0 ? -1 : -2;
      else if( zh_dynsymPos( pDynSym->pSymbol->szName, &uiAt ) != pDynSym )
         iResult = -3;
      else if( uiAt != ( ZH_UINT ) uiPos )
         iResult = -4;
      else
         ++uiPos;
   }

   ZH_DYNSYM_UNLOCK();

   return iResult;
}

ZH_FUNC( __DYNSVERIFY )
{
   ZH_STACK_TLS_PRELOAD

   zh_retni( zh_dynsymVerify() );
}
