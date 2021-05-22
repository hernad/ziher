/*
 * Memvar (PRIVATE/PUBLIC) runtime support
 *
 * Copyright 1999 Ryszard Glab <rglab@imid.med.pl>
 * Copyright 1999-2001 Viktor Szakats
 *   __mvSave(), __mvRestore() (Thanks to Dave Pearson and Jo French for
 *   the original function FReadMem() to read .mem files)
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
#include "zh_fs_api.h" /* for __mvSave()/__mvRestore() */
#include "zh_date.h"  /* for __mvSave()/__mvRestore() */
#include "zh_comp.h"  /* for ZH_VSCOMP_* macros */
#include "error.zhh"
#include "memvar.zhh"
#include "zh_set.h"
#include "zh_stack.h"


#define TABLE_INITZH_VALUE    100
#define TABLE_EXPANDZH_VALUE  50

struct mv_PUBLIC_var_info
{
   int      iPos;
   ZH_BOOL  bFound;
   PZH_DYNSYMBOL pDynSym;
};

struct mv_memvarArray_info
{
   PZH_ITEM   pArray;
   PZH_DYNSYMBOL * pDyns;
   ZH_SIZE    nCount;
   int        iScope;
};

static void zh_memvarCreateFromDynSymbol( PZH_DYNSYMBOL pDynVar, int iScope, PZH_ITEM pValue );

static PZH_ITEM zh_memvarValueNew( void )
{
   PZH_ITEM pMemvar;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_memvarValueNew()" ) );

   pMemvar = ( PZH_ITEM ) zh_xgrab( sizeof( ZH_ITEM ) );
   pMemvar->type = ZH_IT_NIL;

   return pMemvar;
}

/*
 * This function increases the number of references to passed global value
 */
#undef zh_memvarValueIncRef
void zh_memvarValueIncRef( PZH_ITEM pMemvar )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_memvarValueIncRef(%p)", ( void * ) pMemvar ) );

   zh_xRefInc( pMemvar );
}

/*
 * This function decreases the number of references to passed global value.
 * If it is the last reference then this value is deleted.
 */
void zh_memvarValueDecRef( PZH_ITEM pMemvar )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_memvarValueDecRef(%p)", ( void * ) pMemvar ) );

   if( zh_xRefDec( pMemvar ) )
   {
      if( ZH_IS_COMPLEX( pMemvar ) )
         zh_itemClear( pMemvar );
      zh_xfree( pMemvar );
   }
}

/*
 * Detach public or private variable (swap current value with a memvar handle)
 */
static void zh_memvarDetachDynSym( PZH_DYNSYMBOL pDynSym, PZH_ITEM pPrevMemvar )
{
   PZH_ITEM pMemvar;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_memvarDetachDynSym(%p,%p)", ( void * ) pDynSym, ( void * ) pPrevMemvar ) );

   pMemvar = zh_dynsymGetMemvar( pDynSym );
   zh_dynsymSetMemvar( pDynSym, pPrevMemvar );

   zh_memvarValueDecRef( pMemvar );
}

/*
 * Detach local variable (swap current value with a memvar handle)
 */
PZH_ITEM zh_memvarDetachLocal( PZH_ITEM pLocal )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_memvarDetachLocal(%p)", ( void * ) pLocal ) );

   if( ZH_IS_BYREF( pLocal ) )
   {
      do
      {
         if( ZH_IS_MEMVAR( pLocal ) || ZH_IS_EXTREF( pLocal ) )
            break;
         else if( ZH_IS_ENUM( pLocal ) )
         {
            if( ! pLocal->item.asEnum.valuePtr )
            {
               PZH_ITEM pBase = ZH_IS_BYREF( pLocal->item.asEnum.basePtr ) ?
                               zh_itemUnRef( pLocal->item.asEnum.basePtr ) :
                                             pLocal->item.asEnum.basePtr;
               if( ZH_IS_ARRAY( pBase ) )
               {
                  PZH_ITEM pItem = zh_itemNew( NULL );
                  zh_arrayGetItemRef( pBase, pLocal->item.asEnum.offset, pItem );
                  pLocal->item.asEnum.valuePtr = pItem;
                  pLocal = pItem;
                  break;
               }
            }
         }
         else if( pLocal->item.asRefer.value >= 0 &&
                  pLocal->item.asRefer.offset == 0 )
            break;
         pLocal = zh_itemUnRefOnce( pLocal );
      }
      while( ZH_IS_BYREF( pLocal ) );
   }

   /* Change the value only if this variable is not referenced
    * by another codeblock yet.
    * In this case we have to copy the current value to a global memory
    * pool so it can be shared by codeblocks
    */
   if( ! ZH_IS_MEMVAR( pLocal ) )
   {
      PZH_ITEM pMemvar = zh_memvarValueNew();

      zh_itemRawCpy( pMemvar, pLocal );
      pMemvar->type &= ~ZH_IT_DEFAULT;

      pLocal->type = ZH_IT_BYREF | ZH_IT_MEMVAR;
      pLocal->item.asMemvar.value = pMemvar;
   }

   return pLocal;
}


/*
 * This function pushes passed dynamic symbol that belongs to PRIVATE variable
 * into the stack. The value will be popped from it if the variable falls
 * outside the scope (either by using RELEASE, CLEAR ALL, CLEAR MEMORY or by
 * an exit from the function/procedure)
 *
 */
static void zh_memvarAddPrivate( PZH_DYNSYMBOL pDynSym, PZH_ITEM pValue )
{
   ZH_STACK_TLS_PRELOAD
   PZH_PRIVATE_STACK pPrivateStack;
   PZH_ITEM pMemvar;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_memvarAddPrivate(%p,%p)", ( void * ) pDynSym, ( void * ) pValue ) );

   pPrivateStack = zh_stackGetPrivateStack();

   pMemvar = zh_dynsymGetMemvar( pDynSym );
   /* If the variable with the same name exists already
    * and it's PRIVATE variable declared in this function then
    * do not push new memvar on PRIVATEs stack
    */
   if( pMemvar )
   {
      ZH_SIZE nCount = pPrivateStack->count;
      while( nCount > pPrivateStack->base )
      {
         if( pDynSym == pPrivateStack->stack[ nCount - 1 ].pDynSym )
            break;
         --nCount;
      }
      if( nCount <= pPrivateStack->base )
         pMemvar = NULL;
   }

   if( ! pMemvar )
   {
      /* Allocate the value from the end of table
       */
      if( pPrivateStack->count == pPrivateStack->size )
      {
         /* No more free values in the table - expand the table
          */
         if( pPrivateStack->size == 0 )
         {
            pPrivateStack->stack = ( PZH_PRIVATE_ITEM )
                  zh_xgrab( sizeof( ZH_PRIVATE_ITEM ) * TABLE_INITZH_VALUE );
            pPrivateStack->size  = TABLE_INITZH_VALUE;
            pPrivateStack->count = pPrivateStack->base = 0;
         }
         else
         {
            pPrivateStack->size += TABLE_EXPANDZH_VALUE;
            pPrivateStack->stack = ( PZH_PRIVATE_ITEM )
                  zh_xrealloc( pPrivateStack->stack,
                               sizeof( ZH_PRIVATE_ITEM ) * pPrivateStack->size );
         }
      }

      pPrivateStack->stack[ pPrivateStack->count ].pDynSym = pDynSym;
      pPrivateStack->stack[ pPrivateStack->count++ ].pPrevMemvar = zh_dynsymGetMemvar( pDynSym );

      if( pValue && ZH_IS_MEMVAR( pValue ) )
      {
         pMemvar = pValue->item.asMemvar.value;
         zh_xRefInc( pMemvar );
         pValue = NULL;
      }
      else
         pMemvar = zh_memvarValueNew();
      zh_dynsymSetMemvar( pDynSym, pMemvar );
   }

   if( pValue )
   {
      zh_itemCopy( pMemvar, pValue );
      /* Remove MEMOFLAG if exists (assignment from field). */
      pMemvar->type &= ~ZH_IT_MEMOFLAG;
   }
}

/*
 * This function returns current PRIVATE variables stack base
 */
ZH_SIZE zh_memvarGetPrivatesBase( void )
{
   ZH_STACK_TLS_PRELOAD
   ZH_SIZE nBase;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_memvarGetPrivatesBase()" ) );

   nBase = zh_stackGetPrivateStack()->base;
   zh_stackGetPrivateStack()->base = zh_stackGetPrivateStack()->count;
   return nBase;
}

/*
 * This function releases PRIVATE variables created after passed base
 */
void zh_memvarSetPrivatesBase( ZH_SIZE nBase )
{
   ZH_STACK_TLS_PRELOAD
   PZH_PRIVATE_STACK pPrivateStack;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_memvarSetPrivatesBase(%" ZH_PFS "u)", nBase ) );

   pPrivateStack = zh_stackGetPrivateStack();

   while( pPrivateStack->count > pPrivateStack->base )
   {
      PZH_DYNSYMBOL pDynSym = pPrivateStack->stack[ --pPrivateStack->count ].pDynSym;

      if( zh_dynsymGetMemvar( pDynSym ) )
      {
         /* Restore previous value for variables that were overridden
          */
         zh_memvarDetachDynSym( pDynSym, pPrivateStack->stack[ pPrivateStack->count ].pPrevMemvar );
      }
   }
   pPrivateStack->base = nBase;
}

/*
 * Update PRIVATE base offset so they will not be removed
 * when function return
 */
void zh_memvarUpdatePrivatesBase( void )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_memvarUpdatePrivatesBase()" ) );

   zh_stackGetPrivateStack()->base = zh_stackGetPrivateStack()->count;
}

/*
 * Reset PRIVATE base offset to the level of previous function
 */
static void zh_memvarResetPrivatesBase( void )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_memvarResetPrivatesBase()" ) );

   zh_stackGetPrivateStack()->base = zh_stackBaseItem()->item.asSymbol.stackstate->nPrivateBase;
}

/*
 * This functions copies passed item value into the memvar pointed
 * by symbol
 *
 * pMemvar - symbol associated with a variable
 * pItem   - value to store in memvar
 *
 */
void zh_memvarSetValue( PZH_SYMBOL pMemvarSymb, PZH_ITEM pItem )
{
   PZH_DYNSYMBOL pDyn;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_memvarSetValue(%p, %p)", ( void * ) pMemvarSymb, ( void * ) pItem ) );

   pDyn = pMemvarSymb->pDynSym;
   if( pDyn )
   {
      PZH_ITEM pMemvar;

      pMemvar = zh_dynsymGetMemvar( pDyn );

      ZH_TRACE( ZH_TR_INFO, ( "Memvar item (%p)(%s) assigned", ( void * ) pMemvar, pMemvarSymb->szName ) );

      if( pMemvar )
      {
         /* value is already created */
         zh_itemCopyToRef( pMemvar, pItem );
         /* Remove MEMOFLAG if exists (assignment from field). */
         pMemvar->type &= ~ZH_IT_MEMOFLAG;
      }
      else
      {
         /* assignment to undeclared memvar - PRIVATE is assumed */
         zh_memvarCreateFromDynSymbol( pDyn, ZH_VSCOMP_PRIVATE, pItem );
      }
   }
   else
      zh_errInternal( ZH_EI_MVBADSYMBOL, NULL, pMemvarSymb->szName, NULL );
}

ZH_ERRCODE zh_memvarGet( PZH_ITEM pItem, PZH_SYMBOL pMemvarSymb )
{
   PZH_DYNSYMBOL pDyn;
   ZH_ERRCODE errCode = ZH_FAILURE;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_memvarGet(%p, %p)", ( void * ) pItem, ( void * ) pMemvarSymb ) );

   pDyn = pMemvarSymb->pDynSym;
   if( pDyn )
   {
      PZH_ITEM pMemvar;

      pMemvar = zh_dynsymGetMemvar( pDyn );

      ZH_TRACE( ZH_TR_INFO, ( "Memvar item (%p)(%s) queried", ( void * ) pMemvar, pMemvarSymb->szName ) );

      if( pMemvar )
      {
         /* value is already created
          */
         if( ZH_IS_BYREF( pMemvar ) )
            zh_itemCopy( pItem, zh_itemUnRef( pMemvar ) );
         else
            zh_itemCopy( pItem, pMemvar );
         errCode = ZH_SUCCESS;
      }
   }
   else
      zh_errInternal( ZH_EI_MVBADSYMBOL, NULL, pMemvarSymb->szName, NULL );

   return errCode;
}

void zh_memvarGetValue( PZH_ITEM pItem, PZH_SYMBOL pMemvarSymb )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_memvarGetValue(%p, %p)", ( void * ) pItem, ( void * ) pMemvarSymb ) );

   if( zh_memvarGet( pItem, pMemvarSymb ) == ZH_FAILURE )
   {
      /* Generate an error with retry possibility
       * (user created error handler can create this variable)
       */
      PZH_ITEM pError;

      pError = zh_errRT_New( ES_ERROR, NULL, EG_NOVAR, 1003,
                             NULL, pMemvarSymb->szName, 0, EF_CANRETRY );
      zh_itemClear( pItem );

      while( zh_errLaunch( pError ) == E_RETRY )
      {
         if( zh_memvarGet( pItem, pMemvarSymb ) == ZH_SUCCESS )
            break;
      }

      zh_errRelease( pError );
   }
}

void zh_memvarGetRefer( PZH_ITEM pItem, PZH_SYMBOL pMemvarSymb )
{
   PZH_DYNSYMBOL pDyn;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_memvarGetRefer(%p, %p)", ( void * ) pItem, ( void * ) pMemvarSymb ) );

   pDyn = ( PZH_DYNSYMBOL ) pMemvarSymb->pDynSym;
   if( pDyn )
   {
      PZH_ITEM pMemvar;

      pMemvar = zh_dynsymGetMemvar( pDyn );

      ZH_TRACE( ZH_TR_INFO, ( "Memvar item (%p)(%s) referenced", ( void * ) pMemvar, pMemvarSymb->szName ) );

      if( pMemvar )
      {
         if( ZH_IS_BYREF( pMemvar ) && ! ZH_IS_ENUM( pMemvar ) )
            zh_itemCopy( pItem, pMemvar );
         else
         {
            /* value is already created */
            pItem->type = ZH_IT_BYREF | ZH_IT_MEMVAR;
            pItem->item.asMemvar.value = pMemvar;
            zh_xRefInc( pMemvar );
         }
      }
      else
      {
         /* Generate an error with retry possibility
          * (user created error handler can make this variable accessible)
          */
         PZH_ITEM pError;

         pError = zh_errRT_New( ES_ERROR, NULL, EG_NOVAR, 1003,
                                NULL, pMemvarSymb->szName, 0, EF_CANRETRY );
         zh_itemClear( pItem );

         while( zh_errLaunch( pError ) == E_RETRY )
         {
            pMemvar = zh_dynsymGetMemvar( pDyn );
            if( pMemvar )
            {
               if( ZH_IS_BYREF( pMemvar ) && ! ZH_IS_ENUM( pMemvar ) )
                  zh_itemCopy( pItem, pMemvar );
               else
               {
                  /* value is already created */
                  pItem->type = ZH_IT_BYREF | ZH_IT_MEMVAR;
                  pItem->item.asMemvar.value = pMemvar;
                  zh_xRefInc( pMemvar );
               }
               break;
            }
         }
         zh_errRelease( pError );
      }
   }
   else
      zh_errInternal( ZH_EI_MVBADSYMBOL, NULL, pMemvarSymb->szName, NULL );
}

PZH_ITEM zh_memvarGetItem( PZH_SYMBOL pMemvarSymb )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_memvarGetItem(%p)", ( void * ) pMemvarSymb ) );

   if( pMemvarSymb->pDynSym )
   {
      PZH_ITEM pMemvar = zh_dynsymGetMemvar( pMemvarSymb->pDynSym );

      if( pMemvar )
      {
         if( ZH_IS_BYREF( pMemvar ) )
            return zh_itemUnRef( pMemvar );
         else
            return pMemvar;
      }
   }
   return NULL;
}

/*
 */
void zh_memvarNewParameter( PZH_SYMBOL pSymbol, PZH_ITEM pValue )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_memvarNewParameter(%p, %p)", ( void * ) pSymbol, ( void * ) pValue ) );

   zh_memvarCreateFromDynSymbol( pSymbol->pDynSym, ZH_VSCOMP_PRIVATE, pValue );
}

static PZH_DYNSYMBOL zh_memvarFindSymbol( const char * szArg, ZH_SIZE nLen )
{
   PZH_DYNSYMBOL pDynSym = NULL;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_memvarFindSymbol(%p,%" ZH_PFS "u)", ( const void * ) szArg, nLen ) );

   if( nLen && szArg && *szArg )
   {
      char szUprName[ ZH_SYMBOL_NAME_LEN + 1 ];
      int iSize = 0;

      do
      {
         char cChar = *szArg++;

         if( cChar >= 'a' && cChar <= 'z' )
            szUprName[ iSize++ ] = cChar - ( 'a' - 'A' );
         else if( cChar == ' ' || cChar == '\t' || cChar == '\n' )
         {
            if( iSize )
               break;
         }
         else if( ! cChar )
            break;
         else
            szUprName[ iSize++ ] = cChar;
      }
      while( --nLen && iSize < ZH_SYMBOL_NAME_LEN );

      if( iSize )
      {
         szUprName[ iSize ] = '\0';
         pDynSym = zh_dynsymFind( szUprName );
      }
   }
   return pDynSym;
}

char * zh_memvarGetStrValuePtr( char * szVarName, ZH_SIZE * pnLen )
{
   PZH_DYNSYMBOL pDynVar;
   char * szValue = NULL;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_memvarGetStrValuePtr(%s, %p)", ( void * ) szVarName, ( void * ) pnLen ) );

   pDynVar = zh_memvarFindSymbol( szVarName, *pnLen );

   if( pDynVar )
   {
      /* there is dynamic symbol with the requested name - check if it is
       * a memvar variable
       */
      PZH_ITEM pMemvar = zh_dynsymGetMemvar( pDynVar );

      if( pMemvar )
      {
         /* variable contains some data
          */
         if( ZH_IS_BYREF( pMemvar ) )
            pMemvar = zh_itemUnRef( pMemvar );

         if( ZH_IS_STRING( pMemvar ) )
         {
            szValue = pMemvar->item.asString.value;
            *pnLen = pMemvar->item.asString.length;
         }
      }
   }

   return szValue;
}

/*
 * This function creates a value for memvar variable
 *
 * pMemvar - an item that stores the name of variable - it can be either
 *          the ZH_IT_SYMBOL (if created by PUBLIC statement) or ZH_IT_STRING
 *          (if created by direct call to __mvPublic() function)
 * iScope - the scope of created variable - if a variable with the same name
 *          exists already then it's value is hidden by new variable with
 *          passed scope
 * pValue - optional item used to initialize the value of created variable
 *          or NULL
 *
 */
void zh_memvarCreateFromItem( PZH_ITEM pMemvar, int iScope, PZH_ITEM pValue )
{
   PZH_DYNSYMBOL pDynVar = NULL;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_memvarCreateFromItem(%p, %d, %p)", ( void * ) pMemvar, iScope, ( void * ) pValue ) );

   /* find dynamic symbol or create one */
   if( ZH_IS_SYMBOL( pMemvar ) )
#if 0
      pDynVar = zh_dynsymGet( pMemvar->item.asSymbol.value->szName );
#else
      pDynVar = pMemvar->item.asSymbol.value->pDynSym;
#endif
   else if( ZH_IS_STRING( pMemvar ) )
      pDynVar = zh_dynsymGet( pMemvar->item.asString.value );

   if( pDynVar )
      zh_memvarCreateFromDynSymbol( pDynVar, iScope, pValue );
   else
      zh_errRT_BASE( EG_ARG, 3008, NULL, "&", ZH_ERR_ARGS_BASEPARAMS );
}

static void zh_memvarCreateFromDynSymbol( PZH_DYNSYMBOL pDynVar, int iScope, PZH_ITEM pValue )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_memvarCreateFromDynSymbol(%p, %d, %p)", ( void * ) pDynVar, iScope, ( void * ) pValue ) );

   if( iScope & ZH_VSCOMP_PUBLIC )
   {
      /* If the variable with the same name exists already
       * then the current value have to be unchanged
       */
      if( ! zh_dynsymGetMemvar( pDynVar ) )
      {
         PZH_ITEM pMemvar = zh_memvarValueNew();

         zh_dynsymSetMemvar( pDynVar, pMemvar );

         if( pValue )
         {
            zh_itemCopy( pMemvar, pValue );
            /* Remove MEMOFLAG if exists (assignment from field). */
            pMemvar->type &= ~ZH_IT_MEMOFLAG;
         }
         else
         {
            /* new PUBLIC variable - initialize it to .F.
             */
            pMemvar->type = ZH_IT_LOGICAL;

            pMemvar->item.asLogical.value =
                        ( strcmp( pDynVar->pSymbol->szName, "ZIHER" ) == 0 );
         }
      }
   }
   else
   {
      /* Create new PRIVATE var and add it to the PRIVATE variables stack
       */
      zh_memvarAddPrivate( pDynVar, pValue );
   }
}

/* This function releases all memory occupied by a memvar variable
 * It also restores the value that was hidden if there is another
 * PRIVATE variable with the same name.
 */
static void zh_memvarRelease( PZH_ITEM pMemvar )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_memvarRelease(%p)", ( void * ) pMemvar ) );

   if( ZH_IS_STRING( pMemvar ) )
   {
      PZH_DYNSYMBOL pDynSymbol = zh_memvarFindSymbol( pMemvar->item.asString.value,
                                                 pMemvar->item.asString.length );

      if( pDynSymbol && zh_dynsymGetMemvar( pDynSymbol ) )
      {
         ZH_STACK_TLS_PRELOAD
         ZH_SIZE nBase = zh_stackGetPrivateStack()->count;

         /* Find the variable with a requested name that is currently visible
          * Start from the top of the stack.
          */
         while( nBase > 0 )
         {
            if( pDynSymbol == zh_stackGetPrivateStack()->stack[ --nBase ].pDynSym )
            {
               /* reset current value to NIL - the overridden variables will be
                * visible after exit from current procedure
                */
               pMemvar = zh_dynsymGetMemvar( pDynSymbol );
               if( pMemvar )
                  zh_itemClear( pMemvar );
               return;
            }
         }

         /* No match found for PRIVATEs - it's PUBLIC so let's remove it.
          */
         zh_memvarDetachDynSym( pDynSymbol, NULL );
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 3008, NULL, "RELEASE", ZH_ERR_ARGS_BASEPARAMS );
}


/* This function releases all memory occupied by a memvar variable and
 * assigns NIL value - it releases variables created in current
 * procedure only.
 * The scope of released variables are specified using passed name's mask
 */
static void zh_memvarReleaseWithMask( const char * szMask, ZH_BOOL bInclude )
{
   ZH_STACK_TLS_PRELOAD
   ZH_SIZE nBase, nCount;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_memvarReleaseWithMask(%s, %d)", szMask, ( int ) bInclude ) );

   nCount = zh_stackGetPrivateStack()->count;
   nBase = zh_stackBaseItem()->item.asSymbol.stackstate->nPrivateBase;
   while( nCount-- > nBase )
   {
      PZH_DYNSYMBOL pDynVar;
      PZH_ITEM pMemvar;

      pDynVar = zh_stackGetPrivateStack()->stack[ nCount ].pDynSym;
      /* reset current value to NIL - the overridden variables will be
       * visible after exit from current procedure
       */
      pMemvar = zh_dynsymGetMemvar( pDynVar );
      if( pMemvar )
      {
         ZH_BOOL fMatch = zh_strMatchCaseWildExact( pDynVar->pSymbol->szName, szMask );
         if( bInclude ? fMatch : ! fMatch )
            zh_itemClear( pMemvar );
      }
   }
}

/* Checks if passed dynamic symbol is a variable and returns its scope
 */
static int zh_memvarScopeGet( PZH_DYNSYMBOL pDynVar )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_memvarScopeGet(%p)", ( void * ) pDynVar ) );

   if( zh_dynsymGetMemvar( pDynVar ) == 0 )
      return ZH_MV_UNKNOWN;
   else
   {
      ZH_STACK_TLS_PRELOAD
      ZH_SIZE nBase = zh_stackGetPrivateStack()->count;    /* start from the top of the stack */

      while( nBase )
      {
         if( pDynVar == zh_stackGetPrivateStack()->stack[ --nBase ].pDynSym )
         {
            if( nBase >= zh_stackGetPrivateStack()->base )
               return ZH_MV_PRIVATE_LOCAL;
            else
               return ZH_MV_PRIVATE_GLOBAL;
         }
      }
      return ZH_MV_PUBLIC;
   }
}

/* This function checks the scope of passed variable name
 */
int zh_memvarScope( const char * szVarName, ZH_SIZE nLength )
{
   PZH_DYNSYMBOL pDynVar;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_memvarScope(%s, %" ZH_PFS "u)", szVarName, nLength ) );

   pDynVar = zh_memvarFindSymbol( szVarName, nLength );

   if( pDynVar )
      return zh_memvarScopeGet( pDynVar );
   else
      return ZH_MV_NOT_FOUND;
}


/* Clear all memvar variables optionally without GetList PUBLIC variable */
void zh_memvarsClear( ZH_BOOL fAll )
{
   ZH_STACK_TLS_PRELOAD
   PZH_DYNSYMBOL pGetList;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_memvarsClear(%d)", ( int ) fAll ) );

   pGetList = fAll ? NULL : zh_dynsymFind( "GETLIST" );

   zh_stackClearMemvarsBase();
   zh_stackGetPrivateStack()->base = 0;
   zh_memvarSetPrivatesBase( 0 );
   /* this is a little bit hacked but many times faster version
    * of memvars clearing because it scans only given thread stack
    * not global dynamic symbol table. It noticeable reduce the cost
    * of ZHVM thread releasing.
    */
   zh_stackClearMemvars( pGetList ? ( int ) pGetList->uiSymNum : -1 );
}

/* Checks passed dynamic symbol if it is a PUBLIC variable and
 * increments the counter eventually
 */
static ZH_DYNS_FUNC( zh_memvarCountPublics )
{
   if( zh_memvarScopeGet( pDynSymbol ) == ZH_MV_PUBLIC )
      ( *( ( int * ) Cargo ) )++;

   return ZH_TRUE;
}

static ZH_SIZE zh_memvarGetBaseOffset( int iProcLevel )
{
   ZH_STACK_TLS_PRELOAD

   if( iProcLevel > 0 )
   {
      int iLevel = zh_stackCallDepth();
      if( iProcLevel < iLevel )
      {
         ZH_ISIZ nOffset = zh_stackBaseProcOffset( iLevel - iProcLevel - 1 );
         if( nOffset > 0 )
            return zh_stackItem( nOffset )->item.asSymbol.stackstate->nPrivateBase;
      }
   }

   return zh_stackBaseItem()->item.asSymbol.stackstate->nPrivateBase;
}

/* Count the number of variables with given scope
 */
static ZH_ISIZ zh_memvarCount( int iScope, int iLevel )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_memvarCount(%d,%d)", iScope, iLevel ) );

   if( iScope == ZH_MV_PUBLIC )
   {
      int iPublicCnt = 0;

      zh_dynsymProtectEval( zh_memvarCountPublics, ( void * ) &iPublicCnt );
      return iPublicCnt;
   }
   else  /* number of PRIVATE variables */
   {
      ZH_STACK_TLS_PRELOAD

      if( iScope == ZH_MV_PRIVATE_LOCAL )
         return zh_stackGetPrivateStack()->count - zh_memvarGetBaseOffset( iLevel );
      else if( iScope == ZH_MV_PRIVATE_GLOBAL )
         return zh_memvarGetBaseOffset( iLevel );
      else
         return zh_stackGetPrivateStack()->count;
   }
}

/* Checks passed dynamic symbol if it is a PUBLIC variable and returns
 * a pointer to its dynamic symbol
 */
static ZH_DYNS_FUNC( zh_memvarFindPublicByPos )
{
   ZH_BOOL bCont = ZH_TRUE;

   if( zh_memvarScopeGet( pDynSymbol ) == ZH_MV_PUBLIC )
   {
      struct mv_PUBLIC_var_info * pStruPub = ( struct mv_PUBLIC_var_info * ) Cargo;
      if( pStruPub->iPos-- == 0 )
      {
         pStruPub->bFound  = ZH_TRUE;
         pStruPub->pDynSym = pDynSymbol;
         bCont = ZH_FALSE;
      }
   }

   return bCont;
}

/* Returns the pointer to item that holds a value of variable (or NULL if
 * not found). It fills also the pointer to the variable name
 * Both pointers points to existing and used data - they shouldn't be
 * deallocated.
 */
static PZH_ITEM zh_memvarDebugVariable( int iScope, int iPos, const char ** pszName )
{
   PZH_ITEM pValue = NULL;

   *pszName = NULL;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_memvarDebugVariable(%d, %d, %p)", iScope, iPos, ( const void * ) pszName ) );

   if( iPos > 0 )
   {
      --iPos;
      if( iScope == ZH_MV_PUBLIC )
      {
         struct mv_PUBLIC_var_info struPub;

         struPub.iPos   = iPos;
         struPub.bFound = ZH_FALSE;
         /* enumerate existing dynamic symbols and fill this structure
          * with info for requested PUBLIC variable
          */
         zh_dynsymProtectEval( zh_memvarFindPublicByPos, ( void * ) &struPub );
         if( struPub.bFound )
         {
            pValue = zh_dynsymGetMemvar( struPub.pDynSym );
            *pszName = struPub.pDynSym->pSymbol->szName;
         }
      }
      else
      {
         ZH_STACK_TLS_PRELOAD
         if( ( ZH_SIZE ) iPos < zh_stackGetPrivateStack()->count )
         {
            PZH_DYNSYMBOL pDynSym = zh_stackGetPrivateStack()->stack[ iPos ].pDynSym;

            pValue = zh_dynsymGetMemvar( pDynSym );
            *pszName = pDynSym->pSymbol->szName;
         }
      }
   }

   return pValue;
}

static ZH_DYNS_FUNC( zh_memvarCountVisible )
{
   PZH_ITEM pMemvar = zh_dynsymGetMemvar( pDynSymbol );

   if( pMemvar )
   {
      struct mv_memvarArray_info * pMVInfo = ( struct mv_memvarArray_info * ) Cargo;
      if( ! pMVInfo->iScope ||
          ( zh_memvarScopeGet( pDynSymbol ) & pMVInfo->iScope ) != 0 )
      {
         pMVInfo->pDyns[ pMVInfo->nCount++ ] = pDynSymbol;
      }
   }
   return ZH_TRUE;
}

PZH_ITEM zh_memvarSaveInArray( int iScope, ZH_BOOL fCopy )
{
   ZH_STACK_TLS_PRELOAD
   struct mv_memvarArray_info MVInfo;
   PZH_ITEM pArray;

   pArray = NULL;

   iScope &= ZH_MV_PUBLIC | ZH_MV_PRIVATE;
   if( iScope == ( ZH_MV_PUBLIC | ZH_MV_PRIVATE ) )
      iScope = 0;


   MVInfo.pDyns = ( PZH_DYNSYMBOL * ) zh_xgrab( zh_stackDynHandlesCount() *
                                           sizeof( PZH_DYNSYMBOL ) );
   MVInfo.nCount = 0;
   MVInfo.iScope = iScope;

   zh_dynsymProtectEval( zh_memvarCountVisible, ( void * ) &MVInfo );
   if( MVInfo.nCount > 0 )
   {
      pArray = zh_itemArrayNew( MVInfo.nCount );
      do
      {
         PZH_ITEM pItem = zh_arrayGetItemPtr( pArray, MVInfo.nCount );
         if( pItem )
         {
            PZH_DYNSYMBOL pDynSymbol = MVInfo.pDyns[ --MVInfo.nCount ];
            PZH_ITEM pMemvar = zh_dynsymGetMemvar( pDynSymbol );

            zh_arrayNew( pItem, 2 );
            zh_arraySetSymbol( pItem, 1, pDynSymbol->pSymbol );
            pItem = zh_arrayGetItemPtr( pItem, 2 );
            if( fCopy )
            {
               zh_itemCopy( pItem, pMemvar );
               zh_memvarDetachLocal( pItem );
            }
            else
            {
               pItem->type = ZH_IT_BYREF | ZH_IT_MEMVAR;
               pItem->item.asMemvar.value = pMemvar;
               zh_xRefInc( pMemvar );
            }
         }
      }
      while( MVInfo.nCount );
   }
   zh_xfree( MVInfo.pDyns );

   return pArray;
}

void zh_memvarRestoreFromArray( PZH_ITEM pArray )
{
   ZH_SIZE nCount, nPos;

   nCount = zh_arrayLen( pArray );
   for( nPos = 1; nPos <= nCount; ++nPos )
   {
      PZH_ITEM pItem = zh_arrayGetItemPtr( pArray, nPos );
      if( pItem )
      {
         PZH_SYMBOL pSymbol = zh_arrayGetSymbol( pItem, 1 );
         PZH_ITEM pMemRef = zh_arrayGetItemPtr( pItem, 2 );
         if( pSymbol && pMemRef )
         {
            PZH_DYNSYMBOL pDynSym = pSymbol->pDynSym;
            PZH_ITEM pMemvar = pMemRef->item.asMemvar.value;
            zh_memvarValueIncRef( pMemvar );
            if( zh_dynsymGetMemvar( pDynSym ) )
               zh_memvarDetachDynSym( pDynSym, pMemvar );
            else
               zh_dynsymSetMemvar( pDynSym, pMemvar );
         }
      }
   }
}

/* - */

static const char * zh_memvarGetMask( int iParam )
{
   const char * pszMask = zh_parc( iParam );

   if( ! pszMask || pszMask[ 0 ] == '*' )
      pszMask = "*";
   return pszMask;
}

ZH_FUNC( __MVPUBLIC )
{
   ZH_STACK_TLS_PRELOAD
   int iCount = zh_pcount();

   if( iCount )
   {
      int i;

      for( i = 1; i <= iCount; i++ )
      {
         PZH_ITEM pMemvar = zh_param( i, ZH_IT_ANY );

         if( pMemvar )
         {
            if( ZH_IS_ARRAY( pMemvar ) )
            {
               /* we are accepting an one-dimensional array of strings only
                */
               ZH_SIZE n, nLen = zh_arrayLen( pMemvar );

               for( n = 1; n <= nLen; n++ )
               {
                  PZH_ITEM pItem = zh_arrayGetItemPtr( pMemvar, n );
                  if( pItem )
                     zh_memvarCreateFromItem( pItem, ZH_VSCOMP_PUBLIC, NULL );
               }
            }
            else
               zh_memvarCreateFromItem( pMemvar, ZH_VSCOMP_PUBLIC, NULL );
         }
      }
   }
}

ZH_FUNC( __MVPRIVATE )
{
   ZH_STACK_TLS_PRELOAD
   int iCount = zh_pcount();

   if( iCount )
   {
      int i;

      zh_memvarResetPrivatesBase();
      for( i = 1; i <= iCount; i++ )
      {
         PZH_ITEM pMemvar = zh_param( i, ZH_IT_ANY );

         if( pMemvar )
         {
            if( ZH_IS_ARRAY( pMemvar ) )
            {
               /* we are accepting an one-dimensional array of strings only
                */
               ZH_SIZE n, nLen = zh_arrayLen( pMemvar );

               for( n = 1; n <= nLen; n++ )
               {
                  PZH_ITEM pItem = zh_arrayGetItemPtr( pMemvar, n );
                  if( pItem )
                     zh_memvarCreateFromItem( pItem, ZH_VSCOMP_PRIVATE, NULL );
               }
            }
            else
               zh_memvarCreateFromItem( pMemvar, ZH_VSCOMP_PRIVATE, NULL );
         }
      }
      zh_memvarUpdatePrivatesBase();
   }
}

ZH_FUNC( __MVXRELEASE )
{
   ZH_STACK_TLS_PRELOAD
   int iCount = zh_pcount();

   if( iCount )
   {
      int i;

      for( i = 1; i <= iCount; i++ )
      {
         PZH_ITEM pMemvar = zh_param( i, ZH_IT_ANY );

         if( pMemvar )
         {
            if( ZH_IS_ARRAY( pMemvar ) )
            {
               /* we are accepting an one-dimensional array of strings only
                */
               ZH_SIZE n, nLen = zh_arrayLen( pMemvar );

               for( n = 1; n <= nLen; n++ )
               {
                  PZH_ITEM pItem = zh_arrayGetItemPtr( pMemvar, n );
                  if( pItem )
                     zh_memvarRelease( pItem );
               }
            }
            else
               zh_memvarRelease( pMemvar );
         }
      }
   }
}

ZH_FUNC( __MVRELEASE )
{
   ZH_STACK_TLS_PRELOAD
   int iCount = zh_pcount();

   if( iCount && ZH_ISCHAR( 1 ) )
   {
      ZH_BOOL bIncludeVar;
      const char * pszMask;

      pszMask = zh_memvarGetMask( 1 );
      bIncludeVar = ( pszMask[ 0 ] == '*' && ! pszMask[ 1 ] ) ||
                    iCount < 2 || zh_parl( 2 );
      zh_memvarReleaseWithMask( pszMask, bIncludeVar );
   }
}

ZH_FUNC( __MVSCOPE )
{
   ZH_STACK_TLS_PRELOAD
   int iMemvar = ZH_MV_ERROR;

   if( zh_pcount() )
   {
      PZH_ITEM pVarName = zh_param( 1, ZH_IT_STRING );

      if( pVarName )
         iMemvar = zh_memvarScope( pVarName->item.asString.value,
                                   pVarName->item.asString.length );
   }

   zh_retni( iMemvar );
}

ZH_FUNC( __MVCLEAR )
{
   zh_memvarsClear( ZH_FALSE );
}

ZH_FUNC( __MVDBGINFO )
{
   ZH_STACK_TLS_PRELOAD
   int iCount = zh_pcount();

   if( iCount == 1 || iCount == 2 )          /* request for a number of variables */
      zh_retns( zh_memvarCount( zh_parni( 1 ), zh_parni( 2 ) ) );

   else if( iCount > 2 )     /* request for a value of variable */
   {
      PZH_ITEM pValue;
      const char * szName;

      pValue = zh_memvarDebugVariable( zh_parni( 1 ), zh_parni( 2 ), &szName );

      if( pValue )   /* the requested variable was found */
      {
         zh_storc( szName, 3 );
         zh_itemCopyFromRef( zh_stackReturnItem(), pValue );
      }
      else
      {
         zh_ret(); /* return NIL value */
         zh_storc( "?", 3 );
      }
   }
}

ZH_FUNC( __MVEXIST )
{
   ZH_STACK_TLS_PRELOAD
   PZH_DYNSYMBOL pDyn;

   pDyn = zh_memvarFindSymbol( zh_parc( 1 ), zh_parclen( 1 ) );
   zh_retl( pDyn && zh_dynsymGetMemvar( pDyn ) );
}

ZH_FUNC( __MVGET )
{
   PZH_ITEM pName = zh_param( 1, ZH_IT_STRING );

   if( pName )
   {
      ZH_STACK_TLS_PRELOAD
      PZH_DYNSYMBOL pDynVar = zh_memvarFindSymbol( pName->item.asString.value,
                                              pName->item.asString.length );

      if( pDynVar )
      {
         PZH_ITEM pValue = zh_stackAllocItem();

         zh_memvarGetValue( pValue, pDynVar->pSymbol );
         zh_itemReturnForward( pValue );
         zh_stackDec();
      }
      else
      {
         /* Generate an error with retry possibility
          * (user created error handler can create this variable)
          */
         PZH_ITEM pError;

         pError = zh_errRT_New( ES_ERROR, NULL, EG_NOVAR, 1003,
                                NULL, pName->item.asString.value, 0, EF_CANRETRY );

         while( zh_errLaunch( pError ) == E_RETRY )
         {
            pDynVar = zh_memvarFindSymbol( zh_itemGetCPtr( pName ),
                                           zh_itemGetCLen( pName ) );
            if( pDynVar )
            {
               PZH_ITEM pValue = zh_stackAllocItem();

               zh_memvarGetValue( pValue, pDynVar->pSymbol );
               zh_itemReturnForward( pValue );
               zh_stackDec();
               break;
            }
         }
         zh_errRelease( pError );
      }
   }
   else
   {
      /* either the first parameter is not specified or it has a wrong type
       * (it must be a string)
       * This is not a critical error - we can continue normal processing
       */
      zh_errRT_BASE_SubstR( EG_ARG, 3009, NULL, NULL, ZH_ERR_ARGS_BASEPARAMS );
   }
}

ZH_FUNC( __MVPUT )
{
   PZH_ITEM pName = zh_param( 1, ZH_IT_STRING );
   PZH_ITEM pValue = zh_paramError( 2 );

   if( pName )
   {
      /* the first parameter is a string with not empty variable name
       */
      PZH_DYNSYMBOL pDynVar = zh_memvarFindSymbol( pName->item.asString.value,
                                              pName->item.asString.length );
      if( pDynVar )
      {
         /* variable was declared somewhere - assign a new value
          */
         zh_memvarSetValue( pDynVar->pSymbol, pValue );
      }
      else
      {
         /* attempt to assign a value to undeclared variable
          * create the PRIVATE one
          */
         zh_memvarCreateFromDynSymbol( zh_dynsymGet( pName->item.asString.value ), ZH_VSCOMP_PRIVATE, pValue );
      }
      zh_memvarUpdatePrivatesBase();
      zh_itemReturn( pValue );
   }
   else
   {
      /* either the first parameter is not specified or it has a wrong type
       * (it must be a string)
       * This is not a critical error - we can continue normal processing
       */
      PZH_ITEM pRetValue = zh_errRT_BASE_Subst( EG_ARG, 3010, NULL, NULL, ZH_ERR_ARGS_BASEPARAMS );

      if( pRetValue )
         zh_itemRelease( pRetValue );
      zh_itemReturn( pValue );
   }
}

#define ZH_MEM_REC_LEN  32
#define ZH_MEM_NUM_LEN  8

typedef struct
{
   const char * pszMask;
   ZH_BOOL      bIncludeMask;
   ZH_BYTE *    buffer;
   PZH_FILE     fhnd;
} MEMVARSAVE_CARGO;

/* saves a variable to a mem file already open */

static ZH_DYNS_FUNC( zh_memvarSave )
{
   const char * pszMask = ( ( MEMVARSAVE_CARGO * ) Cargo )->pszMask;
   ZH_BOOL bIncludeMask = ( ( MEMVARSAVE_CARGO * ) Cargo )->bIncludeMask;
   ZH_BYTE * buffer = ( ( MEMVARSAVE_CARGO * ) Cargo )->buffer;
   PZH_FILE fhnd    = ( ( MEMVARSAVE_CARGO * ) Cargo )->fhnd;
   PZH_ITEM pMemvar;

   /* NOTE: Ziher name lengths are not limited, but the .mem file
            structure is not flexible enough to allow for it.
            [vszakats] */

   pMemvar = zh_dynsymGetMemvar( pDynSymbol );
   if( pMemvar )
   {
      ZH_BOOL bMatch = zh_strMatchCaseWildExact( pDynSymbol->pSymbol->szName, pszMask );

      /* Process it if it matches the passed mask */
      if( bIncludeMask ? bMatch : ! bMatch )
      {
         memset( buffer, 0, ZH_MEM_REC_LEN );

         /* NOTE: Save only the first 10 characters of the name */
         zh_strncpy( ( char * ) buffer, pDynSymbol->pSymbol->szName, 10 );

         if( ZH_IS_STRING( pMemvar ) )
         {
            /* Store the closing zero byte, too */
            ZH_SIZE nLen = zh_itemGetCLen( pMemvar ) + 1;
            int iOverFlow = 0;

            if( nLen > USHRT_MAX )
            {
               nLen = USHRT_MAX;
               iOverFlow = 1;
            }
            buffer[ 11 ] = 'C' + 128;
            ZH_PUT_LE_UINT16( &buffer[ 16 ], nLen );
            zh_fileWrite( fhnd, buffer, ZH_MEM_REC_LEN, -1 );
            zh_fileWrite( fhnd, zh_itemGetCPtr( pMemvar ), nLen - iOverFlow, -1 );
            if( iOverFlow )
               zh_fileWrite( fhnd, "\0", 1, -1 );
         }
         else if( ZH_IS_NUMERIC( pMemvar ) )
         {
            double dNumber;
            int iWidth;
            int iDec;

            dNumber = zh_itemGetND( pMemvar );
            zh_itemGetNLen( pMemvar, &iWidth, &iDec );
            buffer[ 11 ] = 'N' + 128;

            buffer[ 16 ] = ( ZH_BYTE ) iWidth + ( iDec == 0 ? 0 : ( ZH_BYTE ) ( iDec + 1 ) );
            buffer[ 17 ] = ( ZH_BYTE ) iDec;
            ZH_PUT_LE_DOUBLE( &buffer[ ZH_MEM_REC_LEN ], dNumber );
            zh_fileWrite( fhnd, buffer, ZH_MEM_REC_LEN + ZH_MEM_NUM_LEN, -1 );
         }
         else if( ZH_IS_DATE( pMemvar ) )
         {
            double dNumber = ( double ) zh_itemGetDL( pMemvar );

            buffer[ 11 ] = 'D' + 128;
            buffer[ 16 ] = 1;
            buffer[ 17 ] = 0;
            ZH_PUT_LE_DOUBLE( &buffer[ ZH_MEM_REC_LEN ], dNumber );
            zh_fileWrite( fhnd, buffer, ZH_MEM_REC_LEN + ZH_MEM_NUM_LEN, -1 );
         }
         else if( ZH_IS_TIMESTAMP( pMemvar ) )
         {
            double dNumber = zh_itemGetTD( pMemvar );

            buffer[ 11 ] = 'T' + 128;
            buffer[ 16 ] = 1;
            buffer[ 17 ] = 0;
            ZH_PUT_LE_DOUBLE( &buffer[ ZH_MEM_REC_LEN ], dNumber );
            zh_fileWrite( fhnd, buffer, ZH_MEM_REC_LEN + ZH_MEM_NUM_LEN, -1 );
         }
         else if( ZH_IS_LOGICAL( pMemvar ) )
         {
            buffer[ 11 ] = 'L' + 128;
            buffer[ 16 ] = 1;
            buffer[ 17 ] = 0;
            buffer[ ZH_MEM_REC_LEN ] = zh_itemGetL( pMemvar ) ? 1 : 0;
            zh_fileWrite( fhnd, buffer, ZH_MEM_REC_LEN + 1, -1 );
         }
      }
   }
   return ZH_TRUE;
}

ZH_FUNC( __MVSAVE )
{
   ZH_STACK_TLS_PRELOAD

   if( zh_pcount() == 3 && ZH_ISCHAR( 1 ) && ZH_ISCHAR( 2 ) && ZH_ISLOG( 3 ) )
   {
      const char * pszFileName = zh_parc( 1 );
      PZH_ITEM pError = NULL;
      PZH_FILE fhnd;

      /* Create .mem file */
      do
      {
         fhnd = zh_fileExtOpen( pszFileName,
                                zh_stackSetStruct()->ZH_SET_DEFEXTENSIONS ? ".mem" : NULL,
                                FXO_TRUNCATE | FO_WRITE | FO_EXCLUSIVE |
                                FXO_DEFAULTS | FXO_SHARELOCK,
                                NULL, pError );
         if( fhnd == NULL )
         {
            pError = zh_errRT_FileError( pError, NULL, EG_CREATE, 2006, pszFileName );
            if( zh_errLaunch( pError ) != E_RETRY )
               break;
         }
      }
      while( fhnd == NULL );

      if( fhnd != NULL )
      {
         ZH_BYTE buffer[ ZH_MEM_REC_LEN + ZH_MEM_NUM_LEN ];
         MEMVARSAVE_CARGO msc;

         msc.pszMask      = zh_memvarGetMask( 2 );
         msc.bIncludeMask = zh_parl( 3 );
         msc.buffer       = buffer;
         msc.fhnd         = fhnd;

         /* Walk through all visible memory variables and save each one */

         zh_dynsymEval( zh_memvarSave, ( void * ) &msc );

         buffer[ 0 ] = '\x1A';
         zh_fileWrite( fhnd, buffer, 1, -1 );


         if( zh_setGetHardCommit() )
            zh_fileCommit( fhnd );

         zh_fileClose( fhnd );
      }

      if( pError )
         zh_itemRelease( pError );
   }
   else
      /* NOTE: Undocumented error message in CA-Cl*pper 5.2e and 5.3b. [ckedem] */
      zh_errRT_BASE( EG_ARG, 2008, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}



ZH_FUNC( __MVRESTORE )
{

   if( ZH_ISCHAR( 1 ) && ZH_ISLOG( 2 ) )
   {
      ZH_STACK_TLS_PRELOAD
      const char * pszFileName = zh_parc( 1 );
      PZH_ITEM pError = NULL;
      PZH_FILE fhnd;

      ZH_BOOL bAdditive = zh_parl( 2 );

      /* Clear all memory variables if not ADDITIVE */

      if( ! bAdditive )
         zh_memvarsClear( ZH_FALSE );

      /* Open .mem file */
      do
      {
         fhnd = zh_fileExtOpen( pszFileName,
                                zh_stackSetStruct()->ZH_SET_DEFEXTENSIONS ? ".mem" : NULL,
                                FO_READ | FXO_DEFAULTS | FXO_SHARELOCK,
                                NULL, pError );
         if( fhnd == NULL )
         {
            pError = zh_errRT_FileError( pError, NULL, EG_OPEN, 2005, pszFileName );
            if( zh_errLaunch( pError ) != E_RETRY )
               break;
         }
      }
      while( fhnd == NULL );

      if( fhnd != NULL )
      {
         ZH_BOOL bIncludeMask;
         ZH_BYTE buffer[ ZH_MEM_REC_LEN ];
         const char * pszMask;
         PZH_ITEM pItem = NULL;

         pszMask = zh_memvarGetMask( 3 );
         bIncludeMask = zh_parldef( 4, ZH_TRUE );

         while( zh_fileRead( fhnd, buffer, ZH_MEM_REC_LEN, -1 ) == ZH_MEM_REC_LEN )
         {
            char * pszName;

            ZH_USHORT uiType = ( ZH_USHORT ) ( buffer[ 11 ] & 0x7f );
            ZH_USHORT uiWidth = ( ZH_USHORT ) buffer[ 16 ];
            ZH_USHORT uiDec = ( ZH_USHORT ) buffer[ 17 ];

            /* protect against corrupted files */
            buffer[ 10 ] = '\0';
            pszName = ( char * ) buffer;

            switch( uiType )
            {
               case 'C':
               {
                  ZH_BYTE * pbyString;

                  uiWidth += uiDec * 256;
                  pbyString = ( ZH_BYTE * ) zh_xgrab( uiWidth );

                  if( zh_fileRead( fhnd, pbyString, uiWidth, -1 ) == ( ZH_SIZE ) uiWidth )
                     pItem = zh_itemPutCLPtr( pItem, ( char * ) pbyString, uiWidth - 1 );
                  else
                  {
                     zh_xfree( pbyString );
                     pszName = NULL;
                  }

                  break;
               }

               case 'N':
               {
                  ZH_BYTE pbyNumber[ ZH_MEM_NUM_LEN ];

                  if( zh_fileRead( fhnd, pbyNumber, ZH_MEM_NUM_LEN, -1 ) == ZH_MEM_NUM_LEN )
                     pItem = zh_itemPutNLen( pItem, ZH_GET_LE_DOUBLE( pbyNumber ), uiWidth - ( uiDec ? ( uiDec + 1 ) : 0 ), uiDec );
                  else
                     pszName = NULL;

                  break;
               }

               case 'D':
               {
                  ZH_BYTE pbyNumber[ ZH_MEM_NUM_LEN ];

                  if( zh_fileRead( fhnd, pbyNumber, ZH_MEM_NUM_LEN, -1 ) == ZH_MEM_NUM_LEN )
                     pItem = zh_itemPutDL( pItem, ( long ) ZH_GET_LE_DOUBLE( pbyNumber ) );
                  else
                     pszName = NULL;

                  break;
               }

               case 'T':
               {
                  ZH_BYTE pbyNumber[ ZH_MEM_NUM_LEN ];

                  if( zh_fileRead( fhnd, pbyNumber, ZH_MEM_NUM_LEN, -1 ) == ZH_MEM_NUM_LEN )
                     pItem = zh_itemPutTD( pItem, ZH_GET_LE_DOUBLE( pbyNumber ) );
                  else
                     pszName = NULL;

                  break;
               }

               case 'L':
               {
                  ZH_BYTE pbyLogical[ 1 ];

                  if( zh_fileRead( fhnd, pbyLogical, 1, -1 ) == 1 )
                     pItem = zh_itemPutL( pItem, pbyLogical[ 0 ] != 0 );
                  else
                     pszName = NULL;

                  break;
               }

               default:
                  pszName = NULL;
            }

            if( pszName )
            {
               ZH_BOOL bMatch = zh_strMatchCaseWildExact( pszName, pszMask );

               /* Process it if it matches the passed mask */
               if( bIncludeMask ? bMatch : ! bMatch )
               {
                  /* the first parameter is a string with not empty variable name */
                  PZH_DYNSYMBOL pDynVar = zh_memvarFindSymbol( pszName, strlen( pszName ) );

                  if( pDynVar )
                     /* variable was declared somewhere - assign a new value */
                     zh_memvarSetValue( pDynVar->pSymbol, pItem );
                  else
                     /* attempt to assign a value to undeclared variable create the PRIVATE one */
                     zh_memvarCreateFromDynSymbol( zh_dynsymGet( pszName ), ZH_VSCOMP_PRIVATE, pItem );
               }
            }
         }

         zh_fileClose( fhnd );
         zh_memvarUpdatePrivatesBase();
         zh_itemReturnRelease( pItem );
      }
      else
         zh_retl( ZH_FALSE );

      if( pError )
         zh_itemRelease( pError );
   }
   else
      /* NOTE: Undocumented error message in CA-Cl*pper 5.2e and 5.3b. [ckedem] */
      zh_errRT_BASE( EG_ARG, 2007, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/*
 * This is a hacking function which changes base private offset so
 * PRIVATE variables created in function which calls __mvSetBase()
 * will not be released when the function exit but will be inherited
 * by its caller. [druzus]
 */
ZH_FUNC( __MVSETBASE )
{
   ZH_STACK_TLS_PRELOAD
   ZH_ISIZ nOffset = zh_stackBaseProcOffset( 0 );

   if( nOffset > 0 )
      zh_stackItem( nOffset )->item.asSymbol.stackstate->nPrivateBase =
                                                zh_memvarGetPrivatesBase();
}

/* debugger function */
PZH_ITEM zh_memvarGetValueBySym( PZH_DYNSYMBOL pDynSym )
{
   return zh_dynsymGetMemvar( pDynSym );
}
