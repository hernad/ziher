/*
 * The garbage collector for Ziher
 *
 * Copyright 1999 Ryszard Glab <rglab@imid.med.pl>
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
#include "zh_stack.h"
#include "zh_class_api.h"
#include "zh_item_api.h"
#include "zh_error_api.h"
#include "zh_gt_api.h"
#include "zh_vm.h"
#include "error.zhh"

#if ! defined( ZH_GC_PTR )


#  include "zh_thread.h"
#  include "zh_atomic.h"

/* Use spinlock instead of mutex */

#  if defined( ZH_SPINLOCK_INIT ) && ! defined( ZH_HELGRIND_FRIENDLY )

      static ZH_SPINLOCK_T s_gcSpinLock = ZH_SPINLOCK_INIT;
#     define ZH_GC_LOCK()       ZH_SPINLOCK_ACQUIRE( &s_gcSpinLock )
#     define ZH_GC_UNLOCK()     ZH_SPINLOCK_RELEASE( &s_gcSpinLock )

#  else

      static ZH_CRITICAL_NEW( s_gcMtx );
#     define ZH_GC_LOCK()       zh_threadEnterCriticalSection( &s_gcMtx )
#     define ZH_GC_UNLOCK()     zh_threadLeaveCriticalSection( &s_gcMtx )

#endif


/* holder of memory block information */
/* NOTE: ZH_USHORT is used intentionally to fill up the structure to
 * full 16 bytes (on 16/32-bit environment)
 */
typedef struct ZH_GARBAGE_
{
   struct ZH_GARBAGE_ * pNext;   /* next memory block */
   struct ZH_GARBAGE_ * pPrev;   /* previous memory block */
   const ZH_GC_FUNCS *  pFuncs;  /* cleanup function called before memory releasing */
   ZH_USHORT locked;             /* locking counter */
   ZH_USHORT used;               /* used/unused block */
} ZH_GARBAGE, * PZH_GARBAGE;

#ifdef ZH_ALLOC_ALIGNMENT
#  define ZH_GARBAGE_SIZE     ( ( sizeof( ZH_GARBAGE ) + ZH_ALLOC_ALIGNMENT - 1 ) - \
                                ( sizeof( ZH_GARBAGE ) + ZH_ALLOC_ALIGNMENT - 1 ) % ZH_ALLOC_ALIGNMENT )
#else
#  define ZH_GARBAGE_SIZE     sizeof( ZH_GARBAGE )
#endif

#define ZH_GC_PTR( p )        ( ( PZH_GARBAGE ) ( ( ZH_BYTE * ) ( p ) - ZH_GARBAGE_SIZE ) )

#endif /* ! defined( ZH_GC_PTR ) */

#define ZH_BLOCK_PTR( p )       ( ( void * ) ( ( ZH_BYTE * ) ( p ) + ZH_GARBAGE_SIZE ) )

/* we may use a cache later */
#define ZH_GARBAGE_NEW( nSize )    ( ( PZH_GARBAGE ) zh_xgrab( ZH_GARBAGE_SIZE + ( nSize ) ) )
#define ZH_GARBAGE_FREE( pAlloc )   zh_xfree( ( void * ) ( pAlloc ) )

/* status of memory block */
/* flags stored in 'used' slot */
#define ZH_GC_USED_FLAG    1  /* the bit for used/unused flag */
#define ZH_GC_DELETE       2  /* item marked to delete */
#define ZH_GC_DELETELST    4  /* item will be deleted during finalization */

#ifdef ZH_GC_AUTO
#define ZH_GC_AUTO_MAX        ( ( ZH_PTRUINT ) ( -1 ) )
/* number of allocated memory blocks */
static ZH_PTRUINT s_ulBlocks = 0;
/* number of allocated memory blocks after last GC activation */
static ZH_PTRUINT s_ulBlocksMarked = 0;
/* number of memory blocks between automatic GC activation */
static ZH_PTRUINT s_ulBlocksAuto = 0;
/* number of allocated memory blocks which should force next GC activation */
static ZH_PTRUINT s_ulBlocksCheck = 0;

#  define ZH_GC_AUTO_INC()    ++s_ulBlocks
#  define ZH_GC_AUTO_DEC()    --s_ulBlocks
#else
#  define ZH_GC_AUTO_INC()    do {} while( 0 )
#  define ZH_GC_AUTO_DEC()    do {} while( 0 )
#endif

/* pointer to memory block that will be checked in next step */
static PZH_GARBAGE s_pCurrBlock = NULL;
/* memory blocks are stored in linked list with a loop */

/* pointer to locked memory blocks */
static PZH_GARBAGE s_pLockedBlock = NULL;

/* pointer to memory blocks that will be deleted */
static PZH_GARBAGE s_pDeletedBlock = NULL;

/* marks if block releasing is requested during garbage collecting */
static ZH_BOOL volatile s_bCollecting = ZH_FALSE;

/* flag for used/unused blocks - the meaning of the ZH_GC_USED_FLAG bit
 * is reversed on every collecting attempt
 */
static ZH_USHORT s_uUsedFlag = ZH_GC_USED_FLAG;


static void zh_gcLink( PZH_GARBAGE * pList, PZH_GARBAGE pAlloc )
{
   if( *pList )
   {
      /* add new block at the logical end of list */
      pAlloc->pNext = *pList;
      pAlloc->pPrev = ( *pList )->pPrev;
      pAlloc->pPrev->pNext = pAlloc;
      ( *pList )->pPrev = pAlloc;
   }
   else
   {
      *pList = pAlloc->pNext = pAlloc->pPrev = pAlloc;
   }
}

static void zh_gcUnlink( PZH_GARBAGE * pList, PZH_GARBAGE pAlloc )
{
   pAlloc->pPrev->pNext = pAlloc->pNext;
   pAlloc->pNext->pPrev = pAlloc->pPrev;
   if( *pList == pAlloc )
   {
      *pList = pAlloc->pNext;
      if( *pList == pAlloc )
         *pList = NULL;    /* this was the last block */
   }
}

/* allocates a memory block */
void * zh_gcAllocate( ZH_SIZE nSize, const ZH_GC_FUNCS * pFuncs )
{
   PZH_GARBAGE pAlloc;

   pAlloc = ZH_GARBAGE_NEW( nSize );
   pAlloc->pFuncs = pFuncs;
   pAlloc->locked = 1;
   pAlloc->used   = s_uUsedFlag;
   ZH_GC_LOCK();
   zh_gcLink( &s_pLockedBlock, pAlloc );
   ZH_GC_UNLOCK();

   return ZH_BLOCK_PTR( pAlloc );        /* hide the internal data */
}

/* allocates a memory block */
void * zh_gcAllocRaw( ZH_SIZE nSize, const ZH_GC_FUNCS * pFuncs )
{
   PZH_GARBAGE pAlloc;

   pAlloc = ZH_GARBAGE_NEW( nSize );
   pAlloc->pFuncs = pFuncs;
   pAlloc->locked = 0;
   pAlloc->used   = s_uUsedFlag;

   ZH_GC_LOCK();
#ifdef ZH_GC_AUTO
   if( s_ulBlocks > s_ulBlocksCheck )
   {
      ZH_GC_UNLOCK();
      zh_gcCollectAll( ZH_TRUE );
      ZH_GC_LOCK();
      pAlloc->used = s_uUsedFlag;
   }
   ZH_GC_AUTO_INC();
#endif
   zh_gcLink( &s_pCurrBlock, pAlloc );
   ZH_GC_UNLOCK();

   return ZH_BLOCK_PTR( pAlloc );        /* hide the internal data */
}

/* release a memory block allocated with zh_gcAlloc*() */
void zh_gcFree( void * pBlock )
{
   if( pBlock )
   {
      PZH_GARBAGE pAlloc = ZH_GC_PTR( pBlock );

      /* Don't release the block that will be deleted during finalization */
      if( ! ( pAlloc->used & ZH_GC_DELETE ) )
      {
         ZH_GC_LOCK();
         if( pAlloc->locked )
            zh_gcUnlink( &s_pLockedBlock, pAlloc );
         else
         {
            zh_gcUnlink( &s_pCurrBlock, pAlloc );
            ZH_GC_AUTO_DEC();
         }
         ZH_GC_UNLOCK();

         ZH_GARBAGE_FREE( pAlloc );
      }
   }
   else
   {
      zh_errInternal( ZH_EI_XFREENULL, NULL, NULL, NULL );
   }
}

/* return cleanup function pointer */
const ZH_GC_FUNCS * zh_gcFuncs( void * pBlock )
{
   return ZH_GC_PTR( pBlock )->pFuncs;
}

/* increment reference counter */
#undef zh_gcRefInc
void zh_gcRefInc( void * pBlock )
{
   zh_xRefInc( ZH_GC_PTR( pBlock ) );
}

/* decrement reference counter and free the block when 0 reached */
#undef zh_gcRefFree
void zh_gcRefFree( void * pBlock )
{
   if( pBlock )
   {
      PZH_GARBAGE pAlloc = ZH_GC_PTR( pBlock );

      if( zh_xRefDec( pAlloc ) )
      {
         /* Don't release the block that will be deleted during finalization */
         if( ! ( pAlloc->used & ZH_GC_DELETE ) )
         {
            pAlloc->used |= ZH_GC_DELETE;

            /* execute clean-up function */
            pAlloc->pFuncs->clear( pBlock );

            if( zh_xRefCount( pAlloc ) != 0 )
            {
               if( pAlloc->used & ZH_GC_DELETE )
               {
                  pAlloc->used = s_uUsedFlag;
                  if( zh_vmRequestQuery() == 0 )
                     zh_errRT_BASE( EG_DESTRUCTOR, 1301, NULL, "Reference to freed block", 0 );
               }
            }
            else
            {
               ZH_GC_LOCK();
               if( pAlloc->locked )
                  zh_gcUnlink( &s_pLockedBlock, pAlloc );
               else
               {
                  zh_gcUnlink( &s_pCurrBlock, pAlloc );
                  ZH_GC_AUTO_DEC();
               }
               ZH_GC_UNLOCK();
               ZH_GARBAGE_FREE( pAlloc );
            }
         }
      }
   }
   else
   {
      zh_errInternal( ZH_EI_XFREENULL, NULL, NULL, NULL );
   }
}


/* return number of references */
#undef zh_gcRefCount
ZH_COUNTER zh_gcRefCount( void * pBlock )
{
   return zh_xRefCount( ZH_GC_PTR( pBlock ) );
}


ZH_GARBAGE_FUNC( zh_gcDummyClear )
{
   ZH_SYMBOL_UNUSED( Cargo );
}

ZH_GARBAGE_FUNC( zh_gcDummyMark )
{
   ZH_SYMBOL_UNUSED( Cargo );
}

ZH_GARBAGE_FUNC( zh_gcGripMark )
{
   zh_gcItemRef( ( PZH_ITEM ) Cargo );
}

static ZH_GARBAGE_FUNC( zh_gcGripRelease )
{
   if( ZH_IS_COMPLEX( ( PZH_ITEM ) Cargo ) )
      zh_itemClear( ( PZH_ITEM ) Cargo );
}

static const ZH_GC_FUNCS s_gcGripFuncs =
{
   zh_gcGripRelease,
   zh_gcGripMark
};

PZH_ITEM zh_gcGripGet( PZH_ITEM pOrigin )
{
   PZH_GARBAGE pAlloc = ZH_GARBAGE_NEW( sizeof( ZH_ITEM ) );
   PZH_ITEM pItem = ( PZH_ITEM ) ZH_BLOCK_PTR( pAlloc );

   pAlloc->pFuncs = &s_gcGripFuncs;
   pAlloc->locked = 1;
   pAlloc->used   = s_uUsedFlag;

   pItem->type = ZH_IT_NIL;

   ZH_GC_LOCK();
   zh_gcLink( &s_pLockedBlock, pAlloc );
   ZH_GC_UNLOCK();

   if( pOrigin )
      zh_itemCopy( pItem, pOrigin );

   return pItem;
}

void zh_gcGripDrop( PZH_ITEM pItem )
{
   zh_gcRefFree( pItem );
}

/* Lock a memory pointer so it will not be released if stored
   outside of Ziher variables
 */
void * zh_gcLock( void * pBlock )
{
   if( pBlock )
   {
      PZH_GARBAGE pAlloc = ZH_GC_PTR( pBlock );

      ZH_GC_LOCK();
      if( ! pAlloc->locked )
      {
         zh_gcUnlink( &s_pCurrBlock, pAlloc );
         zh_gcLink( &s_pLockedBlock, pAlloc );
         ZH_GC_AUTO_DEC();
      }
      ++pAlloc->locked;
      ZH_GC_UNLOCK();
   }

   return pBlock;
}

/* Unlock a memory pointer so it can be released if there is no
   references inside of Ziher variables
 */
void * zh_gcUnlock( void * pBlock )
{
   if( pBlock )
   {
      PZH_GARBAGE pAlloc = ZH_GC_PTR( pBlock );

      if( pAlloc->locked )
      {
         ZH_GC_LOCK();
         if( pAlloc->locked )
         {
            if( --pAlloc->locked == 0 )
            {
               pAlloc->used = s_uUsedFlag;

               zh_gcUnlink( &s_pLockedBlock, pAlloc );
               zh_gcLink( &s_pCurrBlock, pAlloc );
               ZH_GC_AUTO_INC();
            }
         }
         ZH_GC_UNLOCK();
      }
   }
   return pBlock;
}

void zh_gcAttach( void * pBlock )
{
   PZH_GARBAGE pAlloc = ZH_GC_PTR( pBlock );

   if( pAlloc->locked )
   {
      ZH_GC_LOCK();
      if( pAlloc->locked )
      {
         if( --pAlloc->locked == 0 )
         {
            pAlloc->used = s_uUsedFlag;

            zh_gcUnlink( &s_pLockedBlock, pAlloc );
            zh_gcLink( &s_pCurrBlock, pAlloc );
            ZH_GC_AUTO_INC();
            pAlloc = NULL;
         }
      }
      ZH_GC_UNLOCK();
   }
   if( pAlloc )
      zh_xRefInc( pAlloc );
}

/* mark passed memory block as used so it will be not released by the GC */
void zh_gcMark( void * pBlock )
{
   PZH_GARBAGE pAlloc = ZH_GC_PTR( pBlock );

   if( ( pAlloc->used & ~ZH_GC_DELETE ) == s_uUsedFlag )
   {
      pAlloc->used ^= ZH_GC_USED_FLAG;  /* mark this codeblock as used */
      pAlloc->pFuncs->mark( pBlock );
   }
}

/* Mark a passed item as used so it will be not released by the GC
 */
void zh_gcItemRef( PZH_ITEM pItem )
{
   while( ZH_IS_BYREF( pItem ) )
   {
      if( ZH_IS_ENUM( pItem ) )
         return;
      else if( ZH_IS_EXTREF( pItem ) )
      {
         pItem->item.asExtRef.func->mark( pItem->item.asExtRef.value );
         return;
      }
      else if( ! ZH_IS_MEMVAR( pItem ) &&
               pItem->item.asRefer.offset == 0 &&
               pItem->item.asRefer.value >= 0 )
      {
         /* array item reference */
         PZH_GARBAGE pAlloc = ZH_GC_PTR( pItem->item.asRefer.BasePtr.array );
         if( ( pAlloc->used & ~ZH_GC_DELETE ) == s_uUsedFlag )
         {
            /* mark this array as used */
            pAlloc->used ^= ZH_GC_USED_FLAG;
            /* mark also all array elements */
            pAlloc->pFuncs->mark( ZH_BLOCK_PTR( pAlloc ) );
         }
         return;
      }
      pItem = zh_itemUnRefOnce( pItem );
   }

   if( ZH_IS_ARRAY( pItem ) )
   {
      PZH_GARBAGE pAlloc = ZH_GC_PTR( pItem->item.asArray.value );

      /* Check this array only if it was not checked yet */
      if( ( pAlloc->used & ~ZH_GC_DELETE ) == s_uUsedFlag )
      {
         /* mark this array as used so it will be no re-checked from
          * other references
          */
         pAlloc->used ^= ZH_GC_USED_FLAG;
         /* mark also all array elements */
         pAlloc->pFuncs->mark( ZH_BLOCK_PTR( pAlloc ) );
      }
   }
   else if( ZH_IS_HASH( pItem ) )
   {
      PZH_GARBAGE pAlloc = ZH_GC_PTR( pItem->item.asHash.value );

      /* Check this hash table only if it was not checked yet */
      if( ( pAlloc->used & ~ZH_GC_DELETE ) == s_uUsedFlag )
      {
         /* mark this hash table as used */
         pAlloc->used ^= ZH_GC_USED_FLAG;
         /* mark also all hash elements */
         pAlloc->pFuncs->mark( ZH_BLOCK_PTR( pAlloc ) );
      }
   }
   else if( ZH_IS_BLOCK( pItem ) )
   {
      PZH_GARBAGE pAlloc = ZH_GC_PTR( pItem->item.asBlock.value );

      if( ( pAlloc->used & ~ZH_GC_DELETE ) == s_uUsedFlag )
      {
         /* mark this codeblock as used */
         pAlloc->used ^= ZH_GC_USED_FLAG;
         /* mark as used all detached variables in a codeblock */
         pAlloc->pFuncs->mark( ZH_BLOCK_PTR( pAlloc ) );
      }
   }
   else if( ZH_IS_POINTER( pItem ) )
   {
      if( pItem->item.asPointer.collect )
      {
         PZH_GARBAGE pAlloc = ZH_GC_PTR( pItem->item.asPointer.value );

         if( ( pAlloc->used & ~ZH_GC_DELETE ) == s_uUsedFlag )
         {
            /* mark this memory block as used */
            pAlloc->used ^= ZH_GC_USED_FLAG;
            /* mark also all internal user blocks attached to this block */
            pAlloc->pFuncs->mark( ZH_BLOCK_PTR( pAlloc ) );
         }
      }
   }
   /* all other data types don't need the GC */
}

void zh_gcCollect( void )
{
   /* TODO: decrease the amount of time spend collecting */
   zh_gcCollectAll( ZH_FALSE );
}

/* Check all memory block if they can be released
 */
void zh_gcCollectAll( ZH_BOOL fForce )
{
   /* MTNOTE: it's not necessary to protect s_bCollecting with mutex
    *         because it can be changed at RT only inside this procedure
    *         when all other threads are stoped by zh_vmSuspendThreads(),
    *         [druzus]
    */
   if( ! s_bCollecting && zh_vmSuspendThreads( fForce ) )
   {
      PZH_GARBAGE pAlloc, pDelete;

      if( ! s_pCurrBlock || s_bCollecting )
      {
         zh_vmResumeThreads();
         return;
      }

      s_bCollecting = ZH_TRUE;

      /* Step 1 - mark */
      /* All blocks are already marked because we are flipping
       * the used/unused flag
       */

      /* Step 2 - sweep */
      /* check all known places for blocks they are referring */
      zh_vmIsStackRef();
      zh_vmIsStaticRef();
      zh_clsIsClassRef();

      /* check list of locked block for blocks referenced from
       * locked block
       */
      if( s_pLockedBlock )
      {
         pAlloc = s_pLockedBlock;
         do
         {
            pAlloc->pFuncs->mark( ZH_BLOCK_PTR( pAlloc ) );
            pAlloc = pAlloc->pNext;
         }
         while( s_pLockedBlock != pAlloc );
      }

      /* Step 3 - finalize */
      /* Release all blocks that are still marked as unused */

      /*
       * infinite loop can appear when we are executing clean-up functions
       * scanning s_pCurrBlock. It's possible that one of them will free
       * the GC block which we are using as stop condition. Only blocks
       * for which we set ZH_GC_DELETE flag are guarded against releasing.
       * To avoid such situation first we are moving blocks which will be
       * deleted to separate list. It's additional operation but it can
       * even increase the speed when we are deleting only few percent
       * of all allocated blocks because in next passes we will scan only
       * deleted block list. [druzus]
       */

      pAlloc = NULL; /* for stop condition */
      do
      {
         if( s_pCurrBlock->used == s_uUsedFlag )
         {
            pDelete = s_pCurrBlock;
            pDelete->used |= ZH_GC_DELETE | ZH_GC_DELETELST;
            zh_gcUnlink( &s_pCurrBlock, pDelete );
            zh_gcLink( &s_pDeletedBlock, pDelete );
            ZH_GC_AUTO_DEC();
         }
         else
         {
            /* at least one block will not be deleted, set new stop condition */
            if( ! pAlloc )
               pAlloc = s_pCurrBlock;
            s_pCurrBlock = s_pCurrBlock->pNext;
         }
      }
      while( pAlloc != s_pCurrBlock );

      /* Step 4 - flip flag */
      /* Reverse used/unused flag so we don't have to mark all blocks
       * during next collecting
       */
      s_uUsedFlag ^= ZH_GC_USED_FLAG;

#ifdef ZH_GC_AUTO
      /* store number of marked blocks for automatic GC activation */
      s_ulBlocksMarked = s_ulBlocks;
      if( s_ulBlocksAuto == 0 )
         s_ulBlocksCheck = ZH_GC_AUTO_MAX;
      else
      {
         s_ulBlocksCheck = s_ulBlocksMarked + s_ulBlocksAuto;
         if( s_ulBlocksCheck <= s_ulBlocksMarked )
            s_ulBlocksCheck = ZH_GC_AUTO_MAX;
      }
#endif

      /* call memory manager cleanup function */
      zh_xclean();

      /* resume suspended threads */
      zh_vmResumeThreads();

      /* do we have any deleted blocks? */
      if( s_pDeletedBlock )
      {
         /* call a cleanup function */
         pAlloc = s_pDeletedBlock;
         do
         {
            s_pDeletedBlock->pFuncs->clear( ZH_BLOCK_PTR( s_pDeletedBlock ) );

            s_pDeletedBlock = s_pDeletedBlock->pNext;
         }
         while( pAlloc != s_pDeletedBlock );

         /* release all deleted blocks */
         do
         {
            pDelete = s_pDeletedBlock;
            zh_gcUnlink( &s_pDeletedBlock, pDelete );
            if( zh_xRefCount( pDelete ) != 0 )
            {
               pDelete->used = s_uUsedFlag;
               pDelete->locked = 0;
               ZH_GC_LOCK();
               zh_gcLink( &s_pCurrBlock, pDelete );
               ZH_GC_AUTO_INC();
               ZH_GC_UNLOCK();
               if( zh_vmRequestQuery() == 0 )
                  zh_errRT_BASE( EG_DESTRUCTOR, 1302, NULL, "Reference to freed block", 0 );
            }
            else
               ZH_GARBAGE_FREE( pDelete );
         }
         while( s_pDeletedBlock );
      }

      s_bCollecting = ZH_FALSE;
   }
}


/* MTNOTE: It's executed at the end of ZHVM cleanup code just before
 *         application exit when other threads are destroyed, so it
 *         does not need additional protection code for MT mode, [druzus]
 */
void zh_gcReleaseAll( void )
{
   if( s_pCurrBlock )
   {
      PZH_GARBAGE pAlloc;

      s_bCollecting = ZH_TRUE;

      pAlloc = s_pCurrBlock;
      do
      {
         /* call a cleanup function */
         s_pCurrBlock->used |= ZH_GC_DELETE | ZH_GC_DELETELST;
         s_pCurrBlock->pFuncs->clear( ZH_BLOCK_PTR( s_pCurrBlock ) );

         s_pCurrBlock = s_pCurrBlock->pNext;

      }
      while( s_pCurrBlock && pAlloc != s_pCurrBlock );

      do
      {
         PZH_GARBAGE pDelete;
         ZH_TRACE( ZH_TR_INFO, ( "Release %p", ( void * ) s_pCurrBlock ) );
         pDelete = s_pCurrBlock;
         zh_gcUnlink( &s_pCurrBlock, pDelete );
         ZH_GC_AUTO_DEC();
         ZH_GARBAGE_FREE( pDelete );

      }
      while( s_pCurrBlock );
   }

   s_bCollecting = ZH_FALSE;
}

/* service a single garbage collector step
 * Check a single memory block if it can be released
 */
ZH_FUNC( ZH_GCSTEP )
{
   zh_gcCollect();
}

/* Check all memory blocks if they can be released
 */
ZH_FUNC( ZH_GCALL )
{
   ZH_STACK_TLS_PRELOAD

   /* call zh_ret() to clear stack return item, ZHVM does not clean
    * it before calling functions/procedures if caller does not
    * try to retrieve returned value. It's safe and cost nearly
    * nothing in whole GC scan process. It may help when previously
    * called function returned complex item with cross references.
    * It's quite common situation that people executes zh_gcAll()
    * immediately after such function. [druzus]
    */
   zh_ret();

   zh_gcCollectAll( zh_parldef( 1, ZH_TRUE ) );
}

#ifdef ZH_GC_AUTO
ZH_FUNC( ZH_GCSETAUTO )
{
   ZH_STACK_TLS_PRELOAD

   ZH_PTRUINT nBlocks, nPrevBlocks;
   ZH_BOOL fSet = ZH_IS_PARAM_NUM( 1 );

   nBlocks = fSet ? zh_parnint( 1 ) * 1000 : 0;

   ZH_GC_LOCK();
   nPrevBlocks = s_ulBlocksAuto;
   if( fSet )
   {
      s_ulBlocksAuto = nBlocks;
      if( s_ulBlocksAuto == 0 )
         s_ulBlocksCheck = ZH_GC_AUTO_MAX;
      else
      {
         s_ulBlocksCheck = s_ulBlocksMarked + s_ulBlocksAuto;
         if( s_ulBlocksCheck <= s_ulBlocksMarked )
            s_ulBlocksCheck = ZH_GC_AUTO_MAX;
      }
   }
   ZH_GC_UNLOCK();

   zh_retnint( nPrevBlocks / 1000 );
}
#endif
