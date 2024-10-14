/*
 * The idle state collector
 *
 * Copyright 1999 Ryszard Glab <rglab@imid.med.pl>
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com> (zh_ReleaseCPU())
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
#include "zh_item_api.h"
#include "zh_set.h"
#include "zh_vm.h"
#include "zh_stack.h"
#include "zh_thread.h"
#include "zh_date.h"
#include "error.zhh"

typedef struct
{
   ZH_BOOL    fCollectGarbage;      /* flag to force GC activation in idle state */
   ZH_BOOL    fIamIdle;             /* flag to prevent recursive calls of zh_idleState() */
   int        iIdleTask;            /* current task to be executed */
   int        iIdleMaxTask;         /* number of tasks in the list */
   PZH_ITEM * pIdleTasks;           /* list of background tasks */
} ZH_IDLEDATA, * PZH_IDLEDATA;

static void zh_idleDataRelease( void * Cargo )
{
   PZH_IDLEDATA pIdleData = ( PZH_IDLEDATA ) Cargo;

   if( pIdleData->pIdleTasks )
   {
      do
      {
         zh_itemRelease( pIdleData->pIdleTasks[ --pIdleData->iIdleMaxTask ] );
      }
      while( pIdleData->iIdleMaxTask );
      zh_xfree( pIdleData->pIdleTasks );
   }
}

static ZH_TSD_NEW( s_idleData, sizeof( ZH_IDLEDATA ), NULL, zh_idleDataRelease );

void zh_releaseCPU( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_releaseCPU()" ) );

   zh_threadReleaseCPU();
}

/* performs all tasks defined for idle state */
void zh_idleState( void )
{
   PZH_IDLEDATA pIdleData = ( PZH_IDLEDATA ) zh_stackGetTSD( &s_idleData );

   if( ! pIdleData->fIamIdle )
   {
      pIdleData->fIamIdle = ZH_TRUE;

      zh_releaseCPU();
      if( zh_vmRequestQuery() == 0 )
      {
         if( pIdleData->fCollectGarbage )
         {
            zh_gcCollectAll( ZH_FALSE );
            pIdleData->fCollectGarbage = ZH_FALSE;
         }

         if( pIdleData->pIdleTasks && pIdleData->iIdleTask < pIdleData->iIdleMaxTask )
         {
            zh_itemRelease( zh_itemDo( pIdleData->pIdleTasks[ pIdleData->iIdleTask ], 0 ) );
            ++pIdleData->iIdleTask;
            if( pIdleData->iIdleTask == pIdleData->iIdleMaxTask && zh_setGetIdleRepeat() )
            {
               pIdleData->iIdleTask = 0;    /* restart processing of idle tasks */
               pIdleData->fCollectGarbage = ZH_TRUE;
            }
         }
      }
      pIdleData->fIamIdle = ZH_FALSE;
   }
}

void zh_idleReset( void )
{
   PZH_IDLEDATA pIdleData = ( PZH_IDLEDATA ) zh_stackGetTSD( &s_idleData );

   if( pIdleData->iIdleTask == pIdleData->iIdleMaxTask && ! zh_setGetIdleRepeat() )
      pIdleData->iIdleTask = 0;

   pIdleData->fCollectGarbage = ZH_TRUE;
}

void zh_idleSleep( double dSeconds )
{
   if( dSeconds >= 0 )
   {
      ZH_MAXINT timeout = dSeconds > 0 ? ( ZH_MAXINT ) ( dSeconds * 1000 ) : 0;
      ZH_MAXUINT timer = zh_timerInit( timeout );

      do
      {
         zh_idleState();
      }
      while( ( timeout = zh_timerTest( timeout, &timer ) ) != 0 &&
             zh_vmRequestQuery() == 0 );

      zh_idleReset();
   }
}

/* signal that the user code is in idle state */
ZH_FUNC( ZH_IDLESTATE )
{
   PZH_IDLEDATA pIdleData = ( PZH_IDLEDATA ) zh_stackGetTSD( &s_idleData );

   pIdleData->fCollectGarbage = ZH_TRUE;
   zh_idleState();
}

/* call from user code to reset idle state */
ZH_FUNC( ZH_IDLERESET )
{
   zh_idleReset();
}

/* call from user code to stay in idle state for given period */
ZH_FUNC( ZH_IDLESLEEP )
{
   zh_idleSleep( zh_parnd( 1 ) );
}

/* add a new background task and return its handle */
ZH_FUNC( ZH_IDLEADD )
{
   PZH_ITEM pBlock = zh_param( 1, ZH_IT_EVALITEM );

   if( pBlock )
   {
      PZH_IDLEDATA pIdleData = ( PZH_IDLEDATA ) zh_stackGetTSD( &s_idleData );

      ++pIdleData->iIdleMaxTask;

      if( ! pIdleData->pIdleTasks )
         pIdleData->pIdleTasks = ( PZH_ITEM * ) zh_xgrab( sizeof( PZH_ITEM ) );
      else
         pIdleData->pIdleTasks = ( PZH_ITEM * ) zh_xrealloc( pIdleData->pIdleTasks, sizeof( PZH_ITEM ) * pIdleData->iIdleMaxTask );

      /* store a copy of passed codeblock
       */
      pIdleData->pIdleTasks[ pIdleData->iIdleMaxTask - 1 ] = zh_itemNew( pBlock );

      /* return a pointer as a handle to this idle task
       */
      zh_retptr( ( void * ) zh_codeblockId( pBlock ) );    /* TODO: access to pointers from Ziher code */
   }
}

/* Delete a task with given handle and return a codeblock with this task */
ZH_FUNC( ZH_IDLEDEL )
{
   PZH_IDLEDATA pIdleData = ( PZH_IDLEDATA ) zh_stackTestTSD( &s_idleData );
   void * pID = zh_parptr( 1 );

   if( pID && pIdleData && pIdleData->pIdleTasks )
   {
      int iTask = 0;

      while( iTask < pIdleData->iIdleMaxTask )
      {
         PZH_ITEM pItem = pIdleData->pIdleTasks[ iTask ];

         if( pID == zh_codeblockId( pItem ) )
         {
            zh_itemClear( zh_itemReturn( pItem ) );  /* return a codeblock */
            zh_itemRelease( pItem );

            --pIdleData->iIdleMaxTask;
            if( pIdleData->iIdleMaxTask )
            {
               if( iTask != pIdleData->iIdleMaxTask )
               {
                  memmove( &pIdleData->pIdleTasks[ iTask ], &pIdleData->pIdleTasks[ iTask + 1 ],
                           sizeof( PZH_ITEM ) * ( pIdleData->iIdleMaxTask - iTask ) );
               }
               pIdleData->pIdleTasks = ( PZH_ITEM * ) zh_xrealloc( pIdleData->pIdleTasks, sizeof( PZH_ITEM ) * pIdleData->iIdleMaxTask );
               if( pIdleData->iIdleTask >= pIdleData->iIdleMaxTask )
                  pIdleData->iIdleTask = 0;
            }
            else
            {
               zh_xfree( pIdleData->pIdleTasks );
               pIdleData->pIdleTasks = NULL;
               pIdleData->iIdleTask  = 0;
            }
            break;
         }
         ++iTask;
      }
   }
}

/* Release a CPU time slice */
ZH_FUNC( ZH_RELEASECPU )
{
   zh_releaseCPU();
}
