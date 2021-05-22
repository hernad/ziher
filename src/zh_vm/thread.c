/*
 * MT mode functions
 *
 * Copyright 2008 Przemyslaw Czerpak
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

/*
  Ziher level API:

  zh_threadStart( [ <nThreadAttrs>, ] <@sStart()> | <bStart> | <cStart> [, <params,...> ] ) --> <pThID>
  zh_threadSelf() --> <pThID> | NIL
  zh_threadID( [ <pThID> ] ) --> <nThNo>
  zh_threadJoin( <pThID> [, @<xRetCode> ] ) --> <lOK>
  zh_threadDetach( <pThID> ) --> <lOK>
* zh_threadQuitRequest( <pThID> ) --> <lOK>
  zh_threadTerminateAll() --> NIL
  zh_threadIsMain( [ <pThID> ] ) --> <lMainHvmThread>
  zh_threadWaitForAll() --> NIL
  zh_threadWait( <pThID> | <apThID>, [ <nTimeOut> ] [, <lAll> ] ) => <nThInd> | <nThCount> | 0
  zh_threadOnce( @<onceControl> [, <bAction> | <@sAction()> ] ) --> <lFirstCall>
  zh_threadOnceInit( @<item>, <value> ) --> <lInitialized>
  zh_mutexCreate() --> <pMtx>
  zh_mutexExists( <pMtx> ) --> <lExists>
  zh_mutexLock( <pMtx> [, <nTimeOut> ] ) --> <lLocked>
  zh_mutexUnlock( <pMtx> ) --> <lOK>
  zh_mutexNotify( <pMtx> [, <xVal>] ) --> NIL
  zh_mutexNotifyAll( <pMtx> [, <xVal>] ) --> NIL
  zh_mutexSubscribe( <pMtx>, [ <nTimeOut> ] [, @<xSubscribed> ] ) --> <lSubscribed>
  zh_mutexSubscribeNow( <pMtx>, [ <nTimeOut> ] [, @<xSubscribed> ] ) --> <lSubscribed>
  zh_mutexEval( <pMtx>, <bCode> | <@sFunc()> [, <params,...> ] ) --> <xCodeResult>
** zh_mutexQueueInfo( <pMtx>, [ @<nWaitersCount> ], [ @<nQueueLength> ] ) --> .T.
  zh_mtvm() --> <lMultiThreadVM>

  * - this function call can be ignored by the destination thread in some
      cases. ZHVM does not guaranties that the QUIT signal will be always
      delivered.
  ** - this is only information function and nWaitersCount or nQueueLength
       can be changed simultaneously by other threads so they cannot be
       used for MT synchronization
*/

#define _ZH_THREAD_INTERNAL_

#include "zh_vm_opt.h"
#include "zh_thread.h"
#include "zh_atomic.h"
#include "zh_item_api.h"
#include "zh_error_api.h"
#include "zh_codepage_api.h"
#include "zh_lang_api.h"
#include "zh_vm.h"
#include "zh_stack.h"
#include "zh_date.h"
#include "memvar.zhh"
#include "thread.zhh"

#if defined( ZH_OS_WIN )
#  include <windows.h>
#endif

#if defined( ZH_PTHREAD_API )
#  include <time.h>
#  include <sys/time.h>
#endif

#if defined( ZH_OS_UNIX )
#  include <sys/time.h>
#  include <sys/times.h>
#  include <unistd.h>
#endif


   static volatile ZH_BOOL s_fThreadInit = ZH_FALSE;

   static PZH_ITEM s_pOnceMutex = NULL;

   static int s_waiting_for_threads = 0;

#  if defined( ZH_PTHREAD_API )
   static void zh_threadTimeInit( struct timespec * ts, ZH_ULONG ulMilliSec )
   {
#     if _POSIX_C_SOURCE >= 199309L
      clock_gettime( CLOCK_REALTIME, ts );
#     else
      struct timeval tv;
      gettimeofday( &tv, NULL );
      ts->tv_sec  = tv.tv_sec;
      ts->tv_nsec = tv.tv_usec * 1000l;
#     endif
      ts->tv_nsec += ( ulMilliSec % 1000 ) * 1000000l;
      ts->tv_sec  += ulMilliSec / 1000 + ts->tv_nsec / 1000000000l;
      ts->tv_nsec %= 1000000000l;
   }
#  endif

#  if defined( ZH_CRITICAL_NEED_INIT )
   static ZH_RAWCRITICAL_T s_init_mtx;
   static ZH_RAWCRITICAL_T s_once_mtx;
   static ZH_RAWCRITICAL_T s_thread_mtx;
   static ZH_RAWCRITICAL_T s_mutexlst_mtx;
   static void zh_threadCriticalInit( ZH_CRITICAL_T * critical )
   {
      if( ! s_fThreadInit )
         zh_threadInit();

      ZH_CRITICAL_LOCK( s_init_mtx );
      if( ! critical->fInit )
      {
         ZH_CRITICAL_INIT( critical->critical.value );
         critical->fInit = ZH_TRUE;
      }
      ZH_CRITICAL_UNLOCK( s_init_mtx );
   }
#  else
#     if defined( ZH_COND_NEED_INIT )
         static ZH_CRITICAL_NEW( s_init_mtx );
#     endif
      static ZH_CRITICAL_NEW( s_once_mtx );
      static ZH_CRITICAL_NEW( s_thread_mtx );
      static ZH_CRITICAL_NEW( s_mutexlst_mtx );
#  endif

#  if defined( ZH_COND_NEED_INIT )
      static ZH_RAWCOND_T s_thread_cond;
      static void zh_threadCondInit( ZH_COND_T * cond )
      {
         if( ! s_fThreadInit )
            zh_threadInit();

         ZH_CRITICAL_LOCK( s_init_mtx );
         if( ! cond->fInit )
         {
            ZH_COND_INIT( cond->cond.value );
#     if ! defined( ZH_COND_OS_SUPPORT )
            ZH_CRITICAL_INIT( cond->critical.value );
            cond->waiters = 0;
#     endif
            cond->fInit = ZH_TRUE;
         }
         ZH_CRITICAL_UNLOCK( s_init_mtx );
      }
#  else
      static ZH_COND_NEW( s_thread_cond );
#  endif


void zh_threadInit( void )
{
   if( ! s_fThreadInit )
   {
#  if defined( ZH_CRITICAL_NEED_INIT )
      ZH_CRITICAL_INIT( s_init_mtx );
      ZH_CRITICAL_INIT( s_once_mtx );
      ZH_CRITICAL_INIT( s_thread_mtx );
      ZH_CRITICAL_INIT( s_mutexlst_mtx );
#  endif
#  if defined( ZH_COND_NEED_INIT )
      ZH_COND_INIT( s_thread_cond );
#  endif
      s_fThreadInit = ZH_TRUE;
   }
}

void zh_threadExit( void )
{
   if( s_pOnceMutex )
   {
      zh_itemRelease( s_pOnceMutex );
      s_pOnceMutex = NULL;
   }
}


void zh_threadReleaseCPU( void )
{
   /*
    * The following code is modified:
    *       zh_ReleaseCPU()
    * originally created by:
    *       Copyright 1999 David G. Holm <dholm@jsd-llc.com>
    * and then updated by few Ziher developers
    */

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_threadReleaseCPU()" ) );

   zh_vmUnlock();

   /* TODO: Add code to release time slices on all platforms */

#if defined( ZH_OS_WIN )

   /* Forfeit the remainder of the current time slice. */
   Sleep( 20 );

#elif defined( ZH_OS_UNIX )
   {
      struct timeval tv;
      tv.tv_sec = 0;
      tv.tv_usec = 20000;
      select( 0, NULL, NULL, NULL, &tv );
   }

   /* the code below is simpler but seems that some Linux kernels
    * (e.g. from CentOS 5.1) have problems with nanosleep()
    * so it was replaced by above code
    */

   /*
   {
      static const struct timespec nanosecs = { 0, 1000000 };
      nanosleep( &nanosecs, NULL );
   }
   */
#else

   /* Do nothing */

#endif

   zh_vmLock();
}

#if defined( ZH_COND_ZIHER_SUPPORT )
static PZH_WAIT_LIST _zh_thread_wait_list( void )
{
   PZH_THREADSTATE pThread = ( PZH_THREADSTATE ) zh_vmThreadState();

   if( pThread )
      return &pThread->pWaitList;
   else
      return NULL;
}

static void _zh_thread_wait_add( ZH_COND_T * cond, PZH_WAIT_LIST pWaiting )
{

   pWaiting->signaled = ZH_FALSE;

   if( cond->waiters == NULL )
   {
      cond->waiters = pWaiting->next = pWaiting->prev = pWaiting;
   }
   else
   {
      pWaiting->next = cond->waiters;
      pWaiting->prev = cond->waiters->prev;
      cond->waiters->prev = pWaiting->prev->next = pWaiting;
   }
}

static void _zh_thread_wait_del( ZH_COND_T * cond, PZH_WAIT_LIST pWaiting )
{
   pWaiting->next->prev = pWaiting->prev;
   pWaiting->prev->next = pWaiting->next;
   if( pWaiting == cond->waiters )
   {
      cond->waiters = pWaiting->next;
      if( pWaiting == cond->waiters )
         cond->waiters = NULL;
   }
}

static ZH_BOOL _zh_thread_cond_signal( ZH_COND_T * cond )
{
   if( cond->waiters )
   {
      PZH_WAIT_LIST pWaiting = cond->waiters;
      do
      {
         if( ! pWaiting->signaled )
         {
#if defined( ZH_OS_WIN )
            ReleaseSemaphore( pWaiting->cond, 1, NULL );
#endif
            pWaiting->signaled = ZH_TRUE;
            /* signal only single thread */
            break;
         }
         pWaiting = pWaiting->next;
      }
      while( pWaiting != cond->waiters );
   }

   return ZH_TRUE;
}

static ZH_BOOL _zh_thread_cond_broadcast( ZH_COND_T * cond )
{
   if( cond->waiters )
   {
      PZH_WAIT_LIST pWaiting = cond->waiters;
      do
      {
         if( ! pWaiting->signaled )
         {
#if defined( ZH_OS_WIN )
            ReleaseSemaphore( pWaiting->cond, 1, NULL );
#endif
            pWaiting->signaled = ZH_TRUE;
         }
         pWaiting = pWaiting->next;
      }
      while( pWaiting != cond->waiters );
   }

   return ZH_TRUE;
}

static ZH_BOOL _zh_thread_cond_wait( ZH_COND_T * cond, ZH_RAWCRITICAL_T * critical, ZH_ULONG ulMillisec )
{
   PZH_WAIT_LIST pWaiting = _zh_thread_wait_list();
   ZH_BOOL fResult = ZH_FALSE;

   if( pWaiting )
   {
      _zh_thread_wait_add( cond, pWaiting );

#if defined( ZH_OS_WIN )
      LeaveCriticalSection( critical );
      fResult = WaitForSingleObject( pWaiting->cond, ulMillisec ) == WAIT_OBJECT_0;
      EnterCriticalSection( critical );
      /* workaround for race condition */
      if( ! fResult && pWaiting->signaled )
         fResult = WaitForSingleObject( pWaiting->cond, 0 ) == WAIT_OBJECT_0;
#endif

      _zh_thread_wait_del( cond, pWaiting );
   }

   return fResult;
}
#endif

/*
 * atomic increment/decrement operations
 */
#if defined( ZH_ATOM_INC ) && defined( ZH_ATOM_DEC ) && \
      defined( ZH_ATOM_GET ) && defined( ZH_ATOM_SET )
void zh_atomic_set( volatile ZH_COUNTER * pCounter, ZH_COUNTER value )
{
   ZH_ATOM_SET( pCounter, value );
}

ZH_COUNTER zh_atomic_get( volatile ZH_COUNTER * pCounter )
{
   return ZH_ATOM_GET( pCounter );
}

void zh_atomic_inc( volatile ZH_COUNTER * pCounter )
{
   ZH_ATOM_INC( pCounter );
}

ZH_BOOL zh_atomic_dec( volatile ZH_COUNTER * pCounter )
{
   return ZH_ATOM_DEC( pCounter ) == 0;
}
#else
static ZH_CRITICAL_NEW( s_atomicMtx );
void zh_atomic_set( volatile ZH_COUNTER * pCounter, ZH_COUNTER value )
{
   /* NOTE: on some platforms it may be necessary to protect this
    * by critical section, e.g. when ZH_COUNTER cannot be accessed
    * using single memory access by CPU.
    */
   *pCounter = value;
}

ZH_COUNTER zh_atomic_get( volatile ZH_COUNTER * pCounter )
{
   /* NOTE: on some platforms it may be necessary to protect this
    * by critical section, e.g. when ZH_COUNTER cannot be accessed
    * using single memory access by CPU.
    */
   return *pCounter;
}

void zh_atomic_inc( volatile ZH_COUNTER * pCounter )
{
   zh_threadEnterCriticalSection( &s_atomicMtx );
   ++( *pCounter );
   zh_threadLeaveCriticalSection( &s_atomicMtx );
}

ZH_BOOL zh_atomic_dec( volatile ZH_COUNTER * pCounter )
{
   ZH_BOOL fResult;

   zh_threadEnterCriticalSection( &s_atomicMtx );
   fResult = --( *pCounter ) == 0;
   zh_threadLeaveCriticalSection( &s_atomicMtx );
   return fResult;
}
#endif

void zh_threadEnterCriticalSection( ZH_CRITICAL_T * critical )
{
#if defined( ZH_CRITICAL_NEED_INIT )
   if( ! critical->fInit )
      zh_threadCriticalInit( critical );
   ZH_CRITICAL_LOCK( critical->critical.value );
#else
   ZH_CRITICAL_LOCK( *critical );
#endif
}

void zh_threadEnterCriticalSectionGC( ZH_CRITICAL_T * critical )
{
#if defined( ZH_CRITICAL_NEED_INIT )
   if( ! critical->fInit )
      zh_threadCriticalInit( critical );
   zh_vmUnlock();
   ZH_CRITICAL_LOCK( critical->critical.value );
   zh_vmLockForce();
#else
   zh_vmUnlock();
   ZH_CRITICAL_LOCK( *critical );
   zh_vmLockForce();
#endif
}

void zh_threadLeaveCriticalSection( ZH_CRITICAL_T * critical )
{
#if defined( ZH_CRITICAL_NEED_INIT )
   ZH_CRITICAL_UNLOCK( critical->critical.value );
#else
   ZH_CRITICAL_UNLOCK( *critical );
#endif
}

ZH_BOOL zh_threadCondSignal( ZH_COND_T * cond )
{
#if defined( ZH_PTHREAD_API )

#  if defined( ZH_COND_NEED_INIT )
      if( ! cond->fInit )
         zh_threadCondInit( cond );
#  endif
   return pthread_cond_signal( ZH_COND_GET( cond ) ) == 0;

#elif defined( ZH_COND_ZIHER_SUPPORT )

   return _zh_thread_cond_signal( cond );

#else

   if( ! cond->fInit )
      zh_threadCondInit( cond );

   ZH_CRITICAL_LOCK( cond->critical.value );
   if( cond->waiters )
   {
      ZH_COND_SIGNAL( cond->cond.value );
      cond->waiters--;
   }
   ZH_CRITICAL_UNLOCK( cond->critical.value );

   return ZH_TRUE;

#endif
}

ZH_BOOL zh_threadCondBroadcast( ZH_COND_T * cond )
{
#if defined( ZH_PTHREAD_API )

#  if defined( ZH_COND_NEED_INIT )
      if( ! cond->fInit )
         zh_threadCondInit( cond );
#  endif
   return pthread_cond_broadcast( ZH_COND_GET( cond ) ) == 0;

#elif defined( ZH_COND_ZIHER_SUPPORT )

   return _zh_thread_cond_broadcast( cond );

#else

   if( ! cond->fInit )
      zh_threadCondInit( cond );

   ZH_CRITICAL_LOCK( cond->critical.value );
   if( cond->waiters )
   {
      ZH_COND_SIGNALN( cond->cond.value, cond->waiters );
      cond->waiters = 0;
   }
   ZH_CRITICAL_UNLOCK( cond->critical.value );

   return ZH_TRUE;

#endif
}

ZH_BOOL zh_threadCondWait( ZH_COND_T * cond, ZH_CRITICAL_T * mutex )
{
#if defined( ZH_PTHREAD_API )

#  if defined( ZH_COND_NEED_INIT )
      if( ! cond->fInit )
         zh_threadCondInit( cond );
#  endif
   return pthread_cond_wait( ZH_COND_GET( cond ), ZH_CRITICAL_GET( mutex ) ) == 0;

#elif defined( ZH_COND_ZIHER_SUPPORT )

   return _zh_thread_cond_wait( cond, &mutex->critical.value, ZH_THREAD_INFINITE_WAIT );

#else

   ZH_BOOL fResult;

   if( ! cond->fInit )
      zh_threadCondInit( cond );

   /* mutex should be already locked so it's not necessary
    * to make initialization test here
    */

   ZH_CRITICAL_LOCK( cond->critical.value );
   cond->waiters++;
   ZH_CRITICAL_UNLOCK( cond->critical.value );

   ZH_CRITICAL_UNLOCK( mutex->critical.value );
   fResult = ZH_COND_WAIT( cond->cond.value );
   ZH_CRITICAL_LOCK( mutex->critical.value );
   /* There is race condition here and user code should always check if
    * the wait condition is valid after leaving zh_threadCondWait()
    * even if it returns ZH_TRUE
    */
   if( ! fResult )
   {
      ZH_CRITICAL_LOCK( cond->critical.value );
      cond->waiters--;
      ZH_CRITICAL_UNLOCK( cond->critical.value );
   }

   return fResult;

#endif
}

ZH_BOOL zh_threadCondTimedWait( ZH_COND_T * cond, ZH_CRITICAL_T * mutex, ZH_ULONG ulMilliSec )
{
#if defined( ZH_PTHREAD_API )
   struct timespec ts;

#  if defined( ZH_COND_NEED_INIT )
      if( ! cond->fInit )
         zh_threadCondInit( cond );
#  endif
   zh_threadTimeInit( &ts, ulMilliSec );
   return pthread_cond_timedwait( ZH_COND_GET( cond ), ZH_CRITICAL_GET( mutex ), &ts ) == 0;

#elif defined( ZH_COND_ZIHER_SUPPORT )

   return _zh_thread_cond_wait( cond, &mutex->critical.value, ulMilliSec );

#else

   ZH_BOOL fResult;

   if( ! cond->fInit )
      zh_threadCondInit( cond );

   /* mutex should be already locked so it's not necessary
    * to make initialization test here
    */

   ZH_CRITICAL_LOCK( cond->critical.value );
   cond->waiters++;
   ZH_CRITICAL_UNLOCK( cond->critical.value );

   ZH_CRITICAL_UNLOCK( mutex->critical.value );
   fResult = ZH_COND_TIMEDWAIT( cond->cond.value, ulMilliSec );
   ZH_CRITICAL_LOCK( mutex->critical.value );
   /* There is race condition here and user code should always check if
    * the wait condition is valid after leaving zh_threadCondTimedWait()
    * even if it returns ZH_TRUE
    */
   if( ! fResult )
   {
      ZH_CRITICAL_LOCK( cond->critical.value );
      cond->waiters--;
      ZH_CRITICAL_UNLOCK( cond->critical.value );
   }

   return fResult;

#endif
}

ZH_THREAD_HANDLE zh_threadCreate( ZH_THREAD_ID * th_id, PZH_THREAD_STARTFUNC start_func, void * Cargo )
{
   ZH_THREAD_HANDLE th_h;

#if defined( ZH_PTHREAD_API )
   if( pthread_create( th_id, NULL, start_func, Cargo ) != 0 )
      *th_id = ( ZH_THREAD_ID ) 0;
   th_h = *th_id;
#elif defined( ZH_OS_WIN )
#  if defined( ZH_THREAD_RAWWINAPI )
      th_h = CreateThread( NULL, 0, start_func, Cargo, 0, th_id );
#  else
      th_h = ( HANDLE ) _beginthreadex( NULL, 0, start_func, Cargo, 0, th_id );
#  endif
   if( ! th_h )
      *th_id = ( ZH_THREAD_ID ) 0;
#else
   { int iTODO_MT; }
   *th_id = ( ZH_THREAD_ID ) 0;
   th_h = ( ZH_THREAD_HANDLE ) 0;
#endif

   return th_h;
}

ZH_BOOL zh_threadJoin( ZH_THREAD_HANDLE th_h )
{
#if defined( ZH_PTHREAD_API )
   return pthread_join( th_h, NULL ) == 0;
#elif defined( ZH_OS_WIN )
   if( WaitForSingleObject( th_h, INFINITE ) != WAIT_FAILED )
   {
      CloseHandle( th_h );
      return ZH_TRUE;
   }
   return ZH_FALSE;
#else
   { int iTODO_MT; }
   return ZH_FALSE;
#endif
}

ZH_BOOL zh_threadDetach( ZH_THREAD_HANDLE th_h )
{
#if defined( ZH_PTHREAD_API )
   return pthread_detach( th_h ) == 0;
#elif defined( ZH_OS_WIN )
   return CloseHandle( th_h ) != 0;
#else
   { int iTODO_MT; }
   return ZH_FALSE;
#endif
}

ZH_THREAD_NO zh_threadNO( void )
{
   PZH_THREADSTATE pThread = ( PZH_THREADSTATE ) zh_vmThreadState();
   if( pThread )
      return pThread->th_no;
   return 0;
}

/*
 * .zh level functions
 */

/* I. THREADS */

static ZH_GARBAGE_FUNC( zh_threadDestructor )
{
   PZH_THREADSTATE pThread = ( PZH_THREADSTATE ) Cargo;

   if( pThread->pParams )
   {
      zh_itemRelease( pThread->pParams );
      pThread->pParams = NULL;
   }
   if( pThread->pMemvars )
   {
      zh_itemRelease( pThread->pMemvars );
      pThread->pMemvars = NULL;
   }
   if( pThread->pResult )
   {
      zh_itemRelease( pThread->pResult );
      pThread->pResult = NULL;
   }
   if( pThread->pI18N )
   {
      zh_i18n_release( pThread->pI18N );
      pThread->pI18N = NULL;
   }
   if( pThread->pSet )
   {
      zh_setRelease( pThread->pSet );
      zh_xfree( pThread->pSet );
      pThread->pSet = NULL;
   }
   if( pThread->th_h != 0 )
   {
      zh_threadDetach( pThread->th_h );
      pThread->th_h = 0;
   }
   if( pThread->hGT )
   {
      zh_gtRelease( pThread->hGT );
      pThread->hGT = NULL;
   }
#if defined( ZH_COND_ZIHER_SUPPORT )
   if( pThread->pWaitList.cond )
   {
#if defined( ZH_OS_WIN )
      CloseHandle( pThread->pWaitList.cond );
      pThread->pWaitList.cond = ( HANDLE ) 0;
#endif
   }
#endif
}

static ZH_GARBAGE_FUNC( zh_threadMark )
{
   PZH_THREADSTATE pThread = ( PZH_THREADSTATE ) Cargo;

   if( pThread->pResult )
      zh_gcMark( pThread->pResult );
   if( pThread->hGT )
      zh_gtIsGtRef( pThread->hGT );
}

static const ZH_GC_FUNCS s_gcThreadFuncs =
{
   zh_threadDestructor,
   zh_threadMark
};

ZH_CARGO_FUNC( zh_threadStartVM )
{
   PZH_THREADSTATE pThread = ( PZH_THREADSTATE ) cargo;
   ZH_ULONG ulPCount, ulParam;
   ZH_BOOL fSend = ZH_FALSE;

   ulPCount = ( ZH_ULONG ) zh_arrayLen( pThread->pParams );
   if( ulPCount > 0 )
   {
      PZH_ITEM pStart = zh_arrayGetItemPtr( pThread->pParams, 1 );

      if( pStart )
      {
         if( ZH_IS_BLOCK( pStart ) )
         {
            zh_vmPushEvalSym();
            zh_vmPush( pStart );
            fSend = ZH_TRUE;
         }
         else if( ZH_IS_SYMBOL( pStart ) )
         {
            zh_vmPush( pStart );
            zh_vmPushNil();
         }
         else if( ZH_IS_STRING( pStart ) )
         {
            zh_vmPushDynSym( zh_dynsymGet( zh_itemGetCPtr( pStart ) ) );
            zh_vmPushNil();
         }
         else
            ulPCount = 0;
      }
      else
         ulPCount = 0;
   }

   if( ulPCount > 0 )
   {
      for( ulParam = 2; ulParam <= ulPCount; ++ulParam )
         zh_vmPush( zh_arrayGetItemPtr( pThread->pParams, ulParam ) );

      zh_itemRelease( pThread->pParams );
      pThread->pParams = NULL;

      if( fSend )
         zh_vmSend( ( ZH_USHORT ) ( ulPCount - 1 ) );
      else
         zh_vmProc( ( ZH_USHORT ) ( ulPCount - 1 ) );
   }
   else
   {
      zh_itemRelease( pThread->pParams );
      pThread->pParams = NULL;
      if( pThread->pMemvars )
      {
         zh_itemRelease( pThread->pMemvars );
         pThread->pMemvars = NULL;
      }

      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, 0 );
   }
}

static ZH_THREAD_STARTFUNC( zh_threadStartFunc )
{
   PZH_THREADSTATE pThread = ( PZH_THREADSTATE ) Cargo;

   zh_vmThreadInit( ( void * ) pThread );

   /* execute user thread function */
   pThread->pFunc( pThread->cargo );

   /* zh_vmThreadQuit() unlocks and release ZHVM stack and may release
    * also pThItm item so we should not access any ZHVM items or
    * pThread structure after this function.
    */
   zh_vmThreadQuit();

   ZH_CRITICAL_LOCK( s_thread_mtx );
   if( s_waiting_for_threads )
   {
      ZH_COND_SIGNALN( s_thread_cond, s_waiting_for_threads );
      s_waiting_for_threads = 0;
   }
   ZH_CRITICAL_UNLOCK( s_thread_mtx );

   ZH_THREAD_END
}

PZH_THREADSTATE zh_threadStateNew( void )
{
   PZH_ITEM pThItm;
   PZH_THREADSTATE pThread;

   pThItm = zh_itemNew( NULL );
   pThread = ( PZH_THREADSTATE )
                  zh_gcAllocRaw( sizeof( ZH_THREADSTATE ), &s_gcThreadFuncs );
   memset( pThread, 0, sizeof( ZH_THREADSTATE ) );
   zh_itemPutPtrRawGC( pThItm, pThread );

   pThread->pszCDP  = ZH_MACRO2STRING( ZH_CODEPAGE_DEFAULT );
   pThread->pszLang = ZH_MACRO2STRING( ZH_LANG_DEFAULT );
   pThread->pThItm  = pThItm;
   pThread->hGT     = zh_gtAlloc( NULL );

#if defined( ZH_COND_ZIHER_SUPPORT )
#if defined( ZH_OS_WIN )
      pThread->pWaitList.cond = CreateSemaphore( NULL, 0, 1, NULL );
#endif
#endif

   return pThread;
}

PZH_THREADSTATE zh_threadStateClone( ZH_ULONG ulAttr, PZH_ITEM pParams )
{
   ZH_STACK_TLS_PRELOAD
   PZH_THREADSTATE pThread;

   pThread = zh_threadStateNew();
   if( zh_stackId() != NULL )
   {
      pThread->pszCDP    = zh_cdpID();
      pThread->pszLang   = zh_langID();
      pThread->pI18N     = zh_i18n_alloc( zh_vmI18N() );
      pThread->pszDefRDD = zh_stackRDD()->szDefaultRDD;
      pThread->pSet      = zh_setClone( zh_stackSetStruct() );

      if( ( ulAttr & ZH_THREAD_INHERIT_MEMVARS ) != 0 )
      {
         int iScope = 0;
         if( ( ulAttr & ZH_THREAD_INHERIT_PUBLIC ) != 0 )
            iScope |= ZH_MV_PUBLIC;
         if( ( ulAttr & ZH_THREAD_INHERIT_PRIVATE ) != 0 )
            iScope |= ZH_MV_PRIVATE;
         pThread->pMemvars = zh_memvarSaveInArray( iScope,
                                    ( ulAttr & ZH_THREAD_MEMVARS_COPY ) != 0 );
      }
      if( pParams && zh_arrayLen( pParams ) > 0 )
      {
         /* detach LOCAL variables passed by reference */
         ZH_SIZE nPCount, nParam;

         nPCount = zh_arrayLen( pParams );
         for( nParam = 1; nParam <= nPCount; ++nParam )
         {
            PZH_ITEM pParam = zh_arrayGetItemPtr( pParams, nParam );
            if( pParam && ZH_IS_BYREF( pParam ) )
               zh_memvarDetachLocal( pParam );
         }
      }
      pThread->pParams = pParams;
   }
   return pThread;
}

static PZH_THREADSTATE zh_thParam( int iParam, int iPos )
{
   PZH_THREADSTATE pThread = ( PZH_THREADSTATE )
                             zh_parvptrGC( &s_gcThreadFuncs, iParam, iPos );

   if( pThread )
      return pThread;

   zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
   return NULL;
}

PZH_ITEM zh_threadStart( ZH_ULONG ulAttr, PZH_CARGO_FUNC pFunc, void * cargo )
{
   PZH_THREADSTATE pThread;
   PZH_ITEM pReturn;

   pThread = zh_threadStateClone( ulAttr, NULL );
   pThread->pFunc = pFunc;
   pThread->cargo = cargo;
   pReturn = zh_itemNew( pThread->pThItm );

   if( zh_vmThreadRegister( ( void * ) pThread ) )
      pThread->th_h = zh_threadCreate( &pThread->th_id, zh_threadStartFunc, ( void * ) pThread );

   if( ! pThread->th_h )
   {
      zh_vmThreadRelease( pThread );
      zh_itemRelease( pReturn );
      pReturn = NULL;
   }
   return pReturn;
}

ZH_FUNC( ZH_THREADSTART )
{
   ZH_ULONG ulAttr = 0, ulStart = 1;
   const char * szFuncName = NULL;
   PZH_SYMBOL pSymbol = NULL;
   PZH_ITEM pStart;

   pStart = zh_param( ulStart, ZH_IT_ANY );
   while( pStart && ZH_IS_NUMERIC( pStart ) )
   {
      ulAttr |= ( ZH_ULONG ) zh_itemGetNL( pStart );
      pStart = zh_param( ++ulStart, ZH_IT_ANY );
   }

   if( pStart )
   {
      if( ZH_IS_STRING( pStart ) )
      {
         PZH_DYNS pDynSym;
         szFuncName = zh_itemGetCPtr( pStart );
         pDynSym = zh_dynsymFindName( szFuncName );
         if( pDynSym )
            pSymbol = pDynSym->pSymbol;
         if( ! pSymbol || ! pSymbol->value.pFunPtr )
            pStart = NULL;
      }
      else if( ZH_IS_SYMBOL( pStart ) )
      {
         pSymbol = zh_itemGetSymbol( pStart );
         if( ! pSymbol->value.pFunPtr )
         {
            szFuncName = pSymbol->szName;
            pStart = NULL;
         }
      }
      else if( ! ZH_IS_BLOCK( pStart ) )
         pStart = NULL;
   }

   if( pStart )
   {
      ZH_STACK_TLS_PRELOAD
      PZH_THREADSTATE pThread;
      PZH_ITEM pReturn, pParams;
      ZH_SIZE nPCount;

      pParams = zh_arrayBaseParams();
      nPCount = zh_arrayLen( pParams );

      /* remove thread attributes */
      if( ulStart > 1 )
      {
         do
         {
            zh_arrayDel( pParams, 1 );
            nPCount--;
         }
         while( --ulStart > 1 );
         zh_arraySize( pParams, nPCount );
      }

      /* update startup item if necessary */
      if( ZH_IS_STRING( pStart ) && pSymbol )
         zh_itemPutSymbol( zh_arrayGetItemPtr( pParams, 1 ), pSymbol );
      else
      {
         PZH_ITEM pParam = zh_arrayGetItemPtr( pParams, 1 );
         if( pParam && ZH_IS_BYREF( pParam ) )
            zh_itemCopy( pParam, zh_itemUnRef( pParam ) );
      }

      pThread = zh_threadStateClone( ulAttr, pParams );
      pThread->pFunc = zh_threadStartVM;
      pThread->cargo = pThread;
      pReturn = pThread->pThItm;

      /* make copy of thread pointer item before we pass it to new thread
       * to avoid race condition
       */
      zh_itemReturn( pReturn );

      if( zh_vmThreadRegister( ( void * ) pThread ) )
         pThread->th_h = zh_threadCreate( &pThread->th_id, zh_threadStartFunc, ( void * ) pThread );

      if( ! pThread->th_h )
      {
         zh_vmThreadRelease( pThread );
         zh_ret();
      }
   }
   else
   {
      if( szFuncName )
         zh_errRT_BASE_SubstR( EG_NOFUNC, 1001, NULL, szFuncName, 0 );
      else
         zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
   }
}

ZH_FUNC( ZH_THREADSELF )
{
   PZH_THREADSTATE pThread = ( PZH_THREADSTATE ) zh_vmThreadState();
   /* It's possible that pThread will be NULL and this function will
    * return NIL. It may happen only in one case when this function is
    * executed by one of destructors of items stored in thread pointer
    * item (in practice it can be only thread return value) and parent
    * thread destroyed this thread pointer item. [druzus]
    */
   if( pThread )
      zh_itemReturn( pThread->pThItm );
}

ZH_FUNC( ZH_THREADID )
{
   ZH_STACK_TLS_PRELOAD
   PZH_THREADSTATE pThread;

   if( zh_pcount() > 0 )
   {
      pThread = zh_thParam( 1, 0 );
      if( pThread )
         zh_retnint( pThread->th_no );
   }
   else
   {
      pThread = ( PZH_THREADSTATE ) zh_vmThreadState();
      if( pThread )
         zh_retnint( pThread->th_no );
      else
         zh_retnint( 0 );
   }
}

ZH_FUNC( ZH_THREADISMAIN )
{
   ZH_STACK_TLS_PRELOAD

   if( zh_pcount() > 0 )
   {
      PZH_THREADSTATE pThread = zh_thParam( 1, 0 );
      if( pThread )
         zh_retl( zh_vmThreadIsMain( pThread ) );
   }
   else
      zh_retl( zh_vmThreadIsMain( NULL ) );
}

static int zh_threadWait( PZH_THREADSTATE * pThreads, int iThreads,
                          ZH_BOOL fAll, ZH_ULONG ulMilliSec )
{
   int i, iFinished, iResult = 0;
   ZH_BOOL fExit = ulMilliSec == 0;

#if defined( ZH_PTHREAD_API )
   struct timespec ts;

   if( ulMilliSec != ZH_THREAD_INFINITE_WAIT )
      zh_threadTimeInit( &ts, ulMilliSec );
   else
      ts.tv_sec = ts.tv_nsec = 0;
#else
   ZH_MAXUINT timer;

   if( ulMilliSec != ZH_THREAD_INFINITE_WAIT )
      timer = zh_timerGet() + ulMilliSec;
   else
      timer = 0;
#endif

   ZH_CRITICAL_LOCK( s_thread_mtx );
   for( ;; )
   {
      for( i = iFinished = 0; i < iThreads; ++i )
      {
         if( pThreads[ i ]->fFinished )
         {
            iFinished++;
            if( ! fAll )
            {
               iResult = i + 1;
               break;
            }
         }
      }
      if( iFinished >= ( fAll ? iThreads : 1 ) )
         break;

      if( fExit )
         break;

      s_waiting_for_threads++;
#if defined( ZH_PTHREAD_API )
      zh_vmUnlock();
      if( ulMilliSec != ZH_THREAD_INFINITE_WAIT )
         fExit = pthread_cond_timedwait( &s_thread_cond, &s_thread_mtx, &ts ) != 0;
      else
         fExit = pthread_cond_wait( &s_thread_cond, &s_thread_mtx ) != 0;
      zh_vmLock();
#else
#  if defined( ZH_COND_ZIHER_SUPPORT )
      zh_vmUnlock();
      fExit = ! _zh_thread_cond_wait( &s_thread_cond, &s_thread_mtx, ulMilliSec );
      zh_vmLock();
#  else

      ZH_CRITICAL_UNLOCK( s_thread_mtx );
      zh_vmUnlock();
      fExit = ! ZH_COND_TIMEDWAIT( s_thread_cond, ulMilliSec );
      zh_vmLock();
      ZH_CRITICAL_LOCK( s_thread_mtx );
      if( fExit )
         s_waiting_for_threads--;
#  endif
      if( ! fExit && timer )
      {
         ZH_MAXUINT curr = zh_timerGet();
         if( timer <= curr )
            fExit = ZH_TRUE;
         else
            ulMilliSec = ( ZH_ULONG ) ( timer - curr );
      }
#endif

      if( ! fExit && zh_vmRequestQuery() != 0 )
         break;
   }
   ZH_CRITICAL_UNLOCK( s_thread_mtx );

   return fAll ? iFinished : iResult;
}

ZH_FUNC( ZH_THREADJOIN )
{
   PZH_THREADSTATE pThread = zh_thParam( 1, 0 );

   if( pThread )
   {
      ZH_STACK_TLS_PRELOAD
      ZH_BOOL fResult = ZH_FALSE;

      if( pThread->th_h )
      {
         zh_vmUnlock();
         fResult = zh_threadJoin( pThread->th_h );
         if( fResult )
            pThread->th_h = 0;
         zh_vmLock();
      }
      if( fResult )
      {
         if( pThread->pResult )
         {
            zh_itemParamStoreForward( 2, pThread->pResult );
            zh_itemRelease( pThread->pResult );
            pThread->pResult = NULL;
         }
      }
      zh_retl( fResult );
   }
}

ZH_FUNC( ZH_THREADDETACH )
{
   PZH_THREADSTATE pThread = zh_thParam( 1, 0 );

   if( pThread )
   {
      ZH_STACK_TLS_PRELOAD
      ZH_BOOL fResult = ZH_FALSE;

      if( pThread->th_h && zh_threadDetach( pThread->th_h ) )
      {
         pThread->th_h = 0;
         fResult = ZH_TRUE;
      }
      zh_retl( fResult );
   }
}

ZH_FUNC( ZH_THREADQUITREQUEST )
{
   PZH_THREADSTATE pThread = zh_thParam( 1, 0 );

   if( pThread )
   {
      ZH_STACK_TLS_PRELOAD
      ZH_BOOL fResult = ZH_FALSE;

      if( pThread->fActive )
      {
         zh_vmThreadQuitRequest( ( void * ) pThread );
         fResult = ZH_TRUE;
      }
      zh_retl( fResult );
   }
}

ZH_FUNC( ZH_THREADWAIT )
{
#  define ZH_THREAD_WAIT_ALLOC  16
   ZH_STACK_TLS_PRELOAD
   ZH_ULONG ulMilliSec = ZH_THREAD_INFINITE_WAIT;
   PZH_THREADSTATE * pThreads, pAlloc[ ZH_THREAD_WAIT_ALLOC ];
   int iThreads = -1;

   pThreads = pAlloc;
   if( ZH_ISARRAY( 1 ) )
   {
      PZH_ITEM pArray = zh_param( 1, ZH_IT_ARRAY );
      int iLen = ( int ) zh_arrayLen( pArray ), i;

      for( i = iThreads = 0; i < iLen; ++i )
      {
         PZH_THREADSTATE pThread = zh_thParam( 1, i + 1 );
         if( ! pThread )
         {
            iThreads = -1;
            break;
         }
         if( pThreads == pAlloc && iThreads >= ZH_THREAD_WAIT_ALLOC )
         {
            pThreads = ( PZH_THREADSTATE * )
                       zh_xgrab( sizeof( PZH_THREADSTATE ) * iLen );
            memcpy( pThreads, pAlloc, sizeof( pAlloc ) );
         }
         pThreads[ iThreads++ ] = pThread;
      }
   }
   else
   {
      pThreads[ 0 ] = zh_thParam( 1, 0 );
      if( pThreads[ 0 ] )
         iThreads = 1;
   }

   if( iThreads > 0 )
   {
      ZH_BOOL fAll;

      if( ZH_IS_PARAM_NUM( 2 ) )
      {
         double dTimeOut = zh_parnd( 2 );
         ulMilliSec = dTimeOut > 0 ? ( ZH_ULONG ) ( dTimeOut * 1000 ) : 0;
      }

      fAll = zh_parl( 3 );

      zh_retni( zh_threadWait( pThreads, iThreads, fAll, ulMilliSec ) );
   }
   else if( iThreads == 0 )
      zh_retni( 0 );

   if( pThreads != pAlloc )
      zh_xfree( pThreads );
}


ZH_FUNC( ZH_THREADWAITFORALL )
{
   zh_vmWaitForThreads();
}

ZH_FUNC( ZH_THREADTERMINATEALL )
{
   zh_vmTerminateThreads();
}

/* zh_threadOnce( @<onceControl> [, <bAction> ] ) --> <lFirstCall>
 * Execute <bAction> only once. <onceControl> is variable which holds
 * the execution status and have to be initialized to NIL. In most of
 * cases it will be simple static variable in user code.
 * When <bAction> is executed by a thread all other threads which call
 * zh_threadOnce() are stopped even if they use different <onceControl>.
 * Because zh_threadOnce() uses single recursive mutex then deadlock caused
 * by cross call to zh_threadOnce() from different threads is not possible.
 * If thread calls zh_threadOnce() with the same <onceControl> variable
 * recursively from <bAction> then zh_threadOnce() returns immediately
 * returning ZH_FALSE without executing <bAction>.
 * This function returns logical value indicating if it was 1st call to
 * zh_threadOnce() for given <onceControl> variable
 */
ZH_FUNC( ZH_THREADONCE )
{
   PZH_ITEM pItem = zh_param( 1, ZH_IT_ANY );

   if( pItem && ZH_ISBYREF( 1 ) && ( ZH_IS_NIL( pItem ) || ZH_IS_LOGICAL( pItem ) ) )
   {
      ZH_STACK_TLS_PRELOAD
      ZH_BOOL fFirstCall = ZH_FALSE;
      if( ZH_IS_NIL( pItem ) || ! zh_itemGetL( pItem ) )
      {
         PZH_ITEM pAction = zh_param( 2, ZH_IT_EVALITEM );

         if( ! s_pOnceMutex )
         {
            if( ! s_fThreadInit )
               zh_threadInit();
            ZH_CRITICAL_LOCK( s_once_mtx );
            if( ! s_pOnceMutex )
               s_pOnceMutex = zh_threadMutexCreate();
            ZH_CRITICAL_UNLOCK( s_once_mtx );
         }
         if( zh_threadMutexLock( s_pOnceMutex ) )
         {
            if( ZH_IS_NIL( pItem ) )
            {
               if( pAction )
               {
                  zh_storl( ZH_FALSE, 1 );
                  zh_vmEvalBlock( pAction );
               }
               zh_storl( ZH_TRUE, 1 );
               fFirstCall = ZH_TRUE;
            }
            zh_threadMutexUnlock( s_pOnceMutex );
         }
      }
      zh_retl( fFirstCall );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/* zh_threadOnceInit( @<item>, <value> ) --> <lInitialized>
 * assign <value> to @<item> only if <item> is NIL
 */
ZH_FUNC( ZH_THREADONCEINIT )
{
   PZH_ITEM pItem = zh_param( 1, ZH_IT_ANY );
   PZH_ITEM pValue = zh_param( 2, ZH_IT_ANY );

   if( pItem && pValue && ZH_ISBYREF( 1 ) && ! ZH_ISBYREF( 2 ) )
   {
      ZH_STACK_TLS_PRELOAD
      ZH_BOOL fInitialized = ZH_FALSE;

      if( ZH_IS_NIL( pItem ) && ! ZH_IS_NIL( pValue ) )
      {
         if( ! s_fThreadInit )
            zh_threadInit();
         ZH_CRITICAL_LOCK( s_once_mtx );
         if( ZH_IS_NIL( pItem ) )
         {
            /* special core code only macro used to eliminate race condition
             * in unprotected readonly access to pItem variable.
             */
            zh_itemSafeMove( pItem, pValue );
            fInitialized = ZH_TRUE;
         }
         ZH_CRITICAL_UNLOCK( s_once_mtx );
      }
      zh_retl( fInitialized );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/* II. MUTEXES */

typedef struct _ZH_MUTEX
{
   int                lock_count;
   int                lockers;
   int                waiters;
   int                syncsignals;
   PZH_ITEM           events;
   ZH_THREAD_ID       owner;
   ZH_RAWCRITICAL_T   mutex;
   ZH_RAWCOND_T       cond_l;
   ZH_RAWCOND_T       cond_w;
   struct _ZH_MUTEX * pNext;
   struct _ZH_MUTEX * pPrev;
}
ZH_MUTEX, * PZH_MUTEX;

static PZH_MUTEX s_pMutexList = NULL;

static void zh_mutexLink( PZH_MUTEX * pList, PZH_MUTEX pItem )
{
   if( *pList )
   {
      pItem->pNext = *pList;
      pItem->pPrev = ( *pList )->pPrev;
      pItem->pPrev->pNext = pItem;
      ( *pList )->pPrev = pItem;
   }
   else
   {
      *pList = pItem->pNext = pItem->pPrev = pItem;
   }
}

static void zh_mutexUnlink( PZH_MUTEX * pList, PZH_MUTEX pItem )
{
   pItem->pPrev->pNext = pItem->pNext;
   pItem->pNext->pPrev = pItem->pPrev;
   if( *pList == pItem )
   {
      *pList = pItem->pNext;
      if( *pList == pItem )
         *pList = NULL;    /* this was the last block */
   }
}

void zh_threadMutexUnlockAll( void )
{
   ZH_CRITICAL_LOCK( s_mutexlst_mtx );
   if( s_pMutexList )
   {
      PZH_MUTEX pMutex = s_pMutexList;
      do
      {
         if( ZH_THREAD_EQUAL( pMutex->owner, ZH_THREAD_SELF() ) )
         {
            ZH_CRITICAL_LOCK( pMutex->mutex );
            if( ZH_THREAD_EQUAL( pMutex->owner, ZH_THREAD_SELF() ) )
            {
               pMutex->lock_count = 0;
               pMutex->owner = ( ZH_THREAD_ID ) 0;
               if( pMutex->lockers )
                  ZH_COND_SIGNAL( pMutex->cond_l );
            }
            ZH_CRITICAL_UNLOCK( pMutex->mutex );
         }
         pMutex = pMutex->pNext;
      }
      while( pMutex != s_pMutexList );
   }
   ZH_CRITICAL_UNLOCK( s_mutexlst_mtx );
}

void zh_threadMutexUnsubscribeAll( void )
{
   ZH_CRITICAL_LOCK( s_mutexlst_mtx );
   if( s_pMutexList )
   {
      PZH_MUTEX pMutex = s_pMutexList;
      do
      {
         if( pMutex->waiters )
         {
            ZH_CRITICAL_LOCK( pMutex->mutex );
            if( pMutex->waiters )
               ZH_COND_SIGNALN( pMutex->cond_w, pMutex->waiters );
            ZH_CRITICAL_UNLOCK( pMutex->mutex );
         }
         pMutex = pMutex->pNext;
      }
      while( pMutex != s_pMutexList );
   }
   ZH_CRITICAL_UNLOCK( s_mutexlst_mtx );
}

static ZH_GARBAGE_FUNC( zh_mutexDestructor )
{
   PZH_MUTEX pMutex = ( PZH_MUTEX ) Cargo;

   ZH_CRITICAL_LOCK( s_mutexlst_mtx );
   zh_mutexUnlink( &s_pMutexList, pMutex );
   ZH_CRITICAL_UNLOCK( s_mutexlst_mtx );

   if( pMutex->events )
   {
      zh_itemRelease( pMutex->events );
      pMutex->events = NULL;
   }

   ZH_CRITICAL_DESTROY( pMutex->mutex );
#  if ! defined( ZH_COND_ZIHER_SUPPORT )
   ZH_COND_DESTROY( pMutex->cond_l );
   ZH_COND_DESTROY( pMutex->cond_w );
#  endif
}

static ZH_GARBAGE_FUNC( zh_mutexMark )
{
   PZH_MUTEX pMutex = ( PZH_MUTEX ) Cargo;

   if( pMutex->events )
      zh_gcMark( pMutex->events );
}

static const ZH_GC_FUNCS s_gcMutexFuncs =
{
   zh_mutexDestructor,
   zh_mutexMark
};

static PZH_MUTEX zh_mutexPtr( PZH_ITEM pItem )
{
   return ( PZH_MUTEX ) zh_itemGetPtrGC( pItem, &s_gcMutexFuncs );
}

static PZH_ITEM zh_mutexParam( int iParam )
{
   PZH_ITEM pItem = zh_param( iParam, ZH_IT_POINTER );

   if( zh_itemGetPtrGC( pItem, &s_gcMutexFuncs ) )
      return pItem;

   zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
   return NULL;
}

PZH_ITEM zh_threadMutexCreate( void )
{
   PZH_MUTEX pMutex;
   PZH_ITEM pItem;

   pItem = zh_itemNew( NULL );
   pMutex = ( PZH_MUTEX ) zh_gcAllocRaw( sizeof( ZH_MUTEX ), &s_gcMutexFuncs );
   memset( pMutex, 0, sizeof( ZH_MUTEX ) );
   pItem = zh_itemPutPtrRawGC( pItem, pMutex );

   ZH_CRITICAL_INIT( pMutex->mutex );
#  if ! defined( ZH_COND_ZIHER_SUPPORT )
   ZH_COND_INIT( pMutex->cond_l );
   ZH_COND_INIT( pMutex->cond_w );
#  endif

   ZH_CRITICAL_LOCK( s_mutexlst_mtx );
   zh_mutexLink( &s_pMutexList, pMutex );
   ZH_CRITICAL_UNLOCK( s_mutexlst_mtx );

   return pItem;
}

void zh_threadMutexSyncSignal( PZH_ITEM pItemMtx )
{
   PZH_MUTEX pMutex = zh_mutexPtr( pItemMtx );

   if( pMutex )
   {
      zh_vmUnlock();
      ZH_CRITICAL_LOCK( pMutex->mutex );

      if( pMutex->waiters )
      {
         int iCount = pMutex->waiters - pMutex->syncsignals;

         if( iCount == 1 )
            ZH_COND_SIGNAL( pMutex->cond_w );
         else if( iCount > 0 )
            ZH_COND_SIGNALN( pMutex->cond_w, iCount );
      }

      ZH_CRITICAL_UNLOCK( pMutex->mutex );
      zh_vmLock();
   }
}

ZH_BOOL zh_threadMutexSyncWait( PZH_ITEM pItemMtx, ZH_ULONG ulMilliSec,
                                PZH_ITEM pItemSync )
{
   PZH_MUTEX pMutex = zh_mutexPtr( pItemMtx ), pSyncMutex = NULL;
   ZH_BOOL fResult = ZH_FALSE;

   if( pMutex )
   {
      if( pItemSync )
      {
         pSyncMutex = zh_mutexPtr( pItemSync );
         if( ! pSyncMutex )
            pMutex = NULL;
      }
   }

   if( pMutex )
   {
      int lock_count = 0;

      zh_vmUnlock();

      ZH_CRITICAL_LOCK( pMutex->mutex );

      if( ulMilliSec && pMutex->syncsignals == 0 )
      {
         /* release own lock from sync mutex */
         if( pSyncMutex && ZH_THREAD_EQUAL( pSyncMutex->owner, ZH_THREAD_SELF() ) )
         {
            ZH_CRITICAL_LOCK( pSyncMutex->mutex );
            lock_count = pSyncMutex->lock_count;
            pSyncMutex->lock_count = 0;
            pSyncMutex->owner = ( ZH_THREAD_ID ) 0;
            if( pSyncMutex->lockers )
               ZH_COND_SIGNAL( pSyncMutex->cond_l );
            ZH_CRITICAL_UNLOCK( pSyncMutex->mutex );
         }

         if( ulMilliSec == ZH_THREAD_INFINITE_WAIT )
         {
            while( pMutex->syncsignals == 0 )
            {
               pMutex->waiters++;
#  if defined( ZH_PTHREAD_API )
               pthread_cond_wait( &pMutex->cond_w, &pMutex->mutex );
#  elif defined( ZH_COND_ZIHER_SUPPORT )
               _zh_thread_cond_wait( &pMutex->cond_w, &pMutex->mutex, ZH_THREAD_INFINITE_WAIT );
#  else
               ZH_CRITICAL_UNLOCK( pMutex->mutex );
               ( void ) ZH_COND_WAIT( pMutex->cond_w );
               ZH_CRITICAL_LOCK( pMutex->mutex );
#  endif
               pMutex->waiters--;
            }
         }
         else
         {
            pMutex->waiters++;
#  if defined( ZH_PTHREAD_API )
            {
               struct timespec ts;

               zh_threadTimeInit( &ts, ulMilliSec );
               while( pMutex->syncsignals == 0 )
               {
                  if( pthread_cond_timedwait( &pMutex->cond_w, &pMutex->mutex, &ts ) != 0 )
                     break;
               }
            }
#  else
            {
#     if defined( ZH_COND_ZIHER_SUPPORT )
               _zh_thread_cond_wait( &pMutex->cond_w, &pMutex->mutex, ulMilliSec );
#     else
               ZH_CRITICAL_UNLOCK( pMutex->mutex );
               ( void ) ZH_COND_TIMEDWAIT( pMutex->cond_w, ulMilliSec );
               ZH_CRITICAL_LOCK( pMutex->mutex );
#     endif
            }
#  endif
            pMutex->waiters--;
         }
      }

      if( pMutex->syncsignals > 0 )
      {
         pMutex->syncsignals--;
         fResult = ZH_TRUE;
      }

      ZH_CRITICAL_UNLOCK( pMutex->mutex );

      /* restore the own lock on sync mutex if necessary */
      if( lock_count )
      {
         ZH_CRITICAL_LOCK( pSyncMutex->mutex );
         if( pSyncMutex->owner )
         {
            pSyncMutex->lockers++;
            while( pSyncMutex->lock_count != 0 )
            {
#  if defined( ZH_PTHREAD_API )
               pthread_cond_wait( &pSyncMutex->cond_l, &pSyncMutex->mutex );
#  elif defined( ZH_COND_ZIHER_SUPPORT )
               _zh_thread_cond_wait( &pSyncMutex->cond_l, &pSyncMutex->mutex, ZH_THREAD_INFINITE_WAIT );
#  else
               ZH_CRITICAL_UNLOCK( pSyncMutex->mutex );
               ( void ) ZH_COND_WAIT( pSyncMutex->cond_l );
               ZH_CRITICAL_LOCK( pSyncMutex->mutex );
#  endif
            }
            pSyncMutex->lockers--;
         }
         pSyncMutex->lock_count = lock_count;
         pSyncMutex->owner = ZH_THREAD_SELF();
         ZH_CRITICAL_UNLOCK( pSyncMutex->mutex );
      }

      zh_vmLock();
   }

   return fResult;
}

ZH_BOOL zh_threadMutexUnlock( PZH_ITEM pItem )
{
   PZH_MUTEX pMutex = zh_mutexPtr( pItem );
   ZH_BOOL fResult = ZH_FALSE;

   if( pMutex )
   {
      zh_vmUnlock();
      ZH_CRITICAL_LOCK( pMutex->mutex );
      if( ZH_THREAD_EQUAL( pMutex->owner, ZH_THREAD_SELF() ) )
      {
         if( --pMutex->lock_count == 0 )
         {
            pMutex->owner = ( ZH_THREAD_ID ) 0;
            if( pMutex->lockers )
               ZH_COND_SIGNAL( pMutex->cond_l );
         }
         fResult = ZH_TRUE;
      }
      ZH_CRITICAL_UNLOCK( pMutex->mutex );
      zh_vmLock();
   }
   return fResult;
}

ZH_BOOL zh_threadMutexLock( PZH_ITEM pItem )
{
   PZH_MUTEX pMutex = zh_mutexPtr( pItem );
   ZH_BOOL fResult = ZH_FALSE;

   if( pMutex )
   {
#  if ! defined( ZH_HELGRIND_FRIENDLY )
      if( ZH_THREAD_EQUAL( pMutex->owner, ZH_THREAD_SELF() ) )
      {
         pMutex->lock_count++;
         fResult = ZH_TRUE;
      }
      else
#endif
      {
         zh_vmUnlock();

         ZH_CRITICAL_LOCK( pMutex->mutex );
#  if defined( ZH_HELGRIND_FRIENDLY )
         if( ZH_THREAD_EQUAL( pMutex->owner, ZH_THREAD_SELF() ) )
            pMutex->lock_count++;
         else
#  endif
         {
            while( pMutex->lock_count != 0 )
            {
               pMutex->lockers++;
#  if defined( ZH_PTHREAD_API )
               pthread_cond_wait( &pMutex->cond_l, &pMutex->mutex );
#  elif defined( ZH_COND_ZIHER_SUPPORT )
               _zh_thread_cond_wait( &pMutex->cond_l, &pMutex->mutex, ZH_THREAD_INFINITE_WAIT );
#  else
               ZH_CRITICAL_UNLOCK( pMutex->mutex );
               ( void ) ZH_COND_WAIT( pMutex->cond_l );
               ZH_CRITICAL_LOCK( pMutex->mutex );
#  endif
               pMutex->lockers--;
            }
            pMutex->lock_count = 1;
            pMutex->owner = ZH_THREAD_SELF();
         }
         ZH_CRITICAL_UNLOCK( pMutex->mutex );
         fResult = ZH_TRUE;

         zh_vmLock();
      }
   }
   return fResult;
}

ZH_BOOL zh_threadMutexTimedLock( PZH_ITEM pItem, ZH_ULONG ulMilliSec )
{
   PZH_MUTEX pMutex = zh_mutexPtr( pItem );
   ZH_BOOL fResult = ZH_FALSE;

   if( pMutex )
   {
      if( ZH_THREAD_EQUAL( pMutex->owner, ZH_THREAD_SELF() ) )
      {
         pMutex->lock_count++;
         fResult = ZH_TRUE;
      }
      else
      {
         zh_vmUnlock();

         ZH_CRITICAL_LOCK( pMutex->mutex );
         if( ulMilliSec && pMutex->lock_count != 0 )
         {
#  if defined( ZH_PTHREAD_API )
            struct timespec ts;

            zh_threadTimeInit( &ts, ulMilliSec );

            /* pthread_cond_signal() wakes up at least one thread
             * but it's not guaranteed it's exactly one thread so
             * we should use while loop here.
             */
            pMutex->lockers++;
            do
            {
               if( pthread_cond_timedwait( &pMutex->cond_l, &pMutex->mutex, &ts ) != 0 )
                  break;
            }
            while( pMutex->lock_count != 0 );
            pMutex->lockers--;
#  else

#     if defined( ZH_COND_ZIHER_SUPPORT )
            pMutex->lockers++;
            _zh_thread_cond_wait( &pMutex->cond_l, &pMutex->mutex, ulMilliSec );
            pMutex->lockers--;
#     else
            pMutex->lockers++;
            ZH_CRITICAL_UNLOCK( pMutex->mutex );
            ( void ) ZH_COND_TIMEDWAIT( pMutex->cond_l, ulMilliSec );
            ZH_CRITICAL_LOCK( pMutex->mutex );
            pMutex->lockers--;
#     endif
#  endif
         }
         if( pMutex->lock_count == 0 )
         {
            pMutex->lock_count = 1;
            pMutex->owner = ZH_THREAD_SELF();
            fResult = ZH_TRUE;
         }
         ZH_CRITICAL_UNLOCK( pMutex->mutex );

         zh_vmLock();
      }
   }
   return fResult;
}

static void zh_thredMutexEventInit( PZH_MUTEX pMutex )
{
   PZH_ITEM pEvents;

   ZH_CRITICAL_UNLOCK( pMutex->mutex );
   zh_vmLock();
   pEvents = zh_itemArrayNew( 0 );
   zh_vmUnlock();
   ZH_CRITICAL_LOCK( pMutex->mutex );
   if( pMutex->events == NULL )
   {
      zh_vmLockForce();
      pMutex->events = pEvents;
      zh_gcUnlock( pMutex->events );
      zh_vmUnlock();
   }
   else
   {
      ZH_CRITICAL_UNLOCK( pMutex->mutex );
      zh_vmLock();
      zh_itemRelease( pEvents );
      zh_vmUnlock();
      ZH_CRITICAL_LOCK( pMutex->mutex );
   }
}

void zh_threadMutexNotify( PZH_ITEM pItem, PZH_ITEM pNotifier, ZH_BOOL fWaiting )
{
   PZH_MUTEX pMutex = zh_mutexPtr( pItem );

   if( pMutex )
   {
      zh_vmUnlock();
      ZH_CRITICAL_LOCK( pMutex->mutex );

      if( ! fWaiting || pMutex->waiters )
      {
         if( ! pMutex->events )
            zh_thredMutexEventInit( pMutex );

         if( ! fWaiting )
         {
            zh_vmLockForce();
            if( pNotifier )
               zh_arrayAdd( pMutex->events, pNotifier );
            else
               zh_arraySize( pMutex->events, zh_arrayLen( pMutex->events ) + 1 );
            zh_vmUnlock();
            if( pMutex->waiters )
               ZH_COND_SIGNAL( pMutex->cond_w );
         }
         else if( pMutex->waiters )
         {
            int iLen = ( int ) zh_arrayLen( pMutex->events );
            int iCount = pMutex->waiters - iLen;

            if( iCount > 0 )
            {
               zh_vmLockForce();
               zh_arraySize( pMutex->events, iLen + iCount );
               if( pNotifier && ! ZH_IS_NIL( pNotifier ) )
               {
                  int iSet = iCount;
                  do
                  {
                     zh_arraySet( pMutex->events, ++iLen, pNotifier );
                  }
                  while( --iSet );
               }
               zh_vmUnlock();
               if( iCount == 1 )
                  ZH_COND_SIGNAL( pMutex->cond_w );
               else
                  ZH_COND_SIGNALN( pMutex->cond_w, iCount );
            }
         }
      }
      ZH_CRITICAL_UNLOCK( pMutex->mutex );
      zh_vmLock();
   }
}

PZH_ITEM zh_threadMutexSubscribe( PZH_ITEM pItem, ZH_BOOL fClear )
{
   PZH_MUTEX pMutex = zh_mutexPtr( pItem );
   PZH_ITEM pResult = NULL;

   if( pMutex )
   {
      ZH_STACK_TLS_PRELOAD
      int lock_count = 0;

      zh_vmUnlock();
      ZH_CRITICAL_LOCK( pMutex->mutex );

      if( fClear && pMutex->events )
      {
         zh_vmLockForce();
         zh_itemMove( zh_stackAllocItem(), pMutex->events );
         pMutex->events = NULL;
         ZH_CRITICAL_UNLOCK( pMutex->mutex );
         zh_stackPop();
         zh_vmUnlock();
         ZH_CRITICAL_LOCK( pMutex->mutex );
      }

      /* release own lock from this mutex */
      if( ZH_THREAD_EQUAL( pMutex->owner, ZH_THREAD_SELF() ) )
      {
         lock_count = pMutex->lock_count;
         pMutex->lock_count = 0;
         pMutex->owner = ( ZH_THREAD_ID ) 0;
         if( pMutex->lockers )
            ZH_COND_SIGNAL( pMutex->cond_l );
      }

      while( ( ! pMutex->events || zh_arrayLen( pMutex->events ) == 0 ) &&
             zh_vmRequestQuery() == 0 )
      {
         pMutex->waiters++;
#  if defined( ZH_PTHREAD_API )
         pthread_cond_wait( &pMutex->cond_w, &pMutex->mutex );
#  elif defined( ZH_COND_ZIHER_SUPPORT )
         _zh_thread_cond_wait( &pMutex->cond_w, &pMutex->mutex, ZH_THREAD_INFINITE_WAIT );
#  else
         ZH_CRITICAL_UNLOCK( pMutex->mutex );
         ( void ) ZH_COND_WAIT( pMutex->cond_w );
         ZH_CRITICAL_LOCK( pMutex->mutex );
#  endif
         pMutex->waiters--;
      }

      if( pMutex->events && zh_arrayLen( pMutex->events ) > 0 )
      {
         zh_vmLockForce();
         pResult = zh_stackAllocItem();
         zh_arrayGet( pMutex->events, 1, pResult );
         zh_arrayDel( pMutex->events, 1 );
         zh_arraySize( pMutex->events, zh_arrayLen( pMutex->events ) - 1 );
         zh_vmUnlock();
      }

      /* restore the own lock on this mutex if necessary */
      if( lock_count )
      {
         if( pMutex->owner )
         {
            pMutex->lockers++;
            while( pMutex->lock_count != 0 )
            {
#  if defined( ZH_PTHREAD_API )
               pthread_cond_wait( &pMutex->cond_l, &pMutex->mutex );
#  elif defined( ZH_COND_ZIHER_SUPPORT )
               _zh_thread_cond_wait( &pMutex->cond_l, &pMutex->mutex, ZH_THREAD_INFINITE_WAIT );
#  else
               ZH_CRITICAL_UNLOCK( pMutex->mutex );
               ( void ) ZH_COND_WAIT( pMutex->cond_l );
               ZH_CRITICAL_LOCK( pMutex->mutex );
#  endif
            }
            pMutex->lockers--;
         }
         pMutex->lock_count = lock_count;
         pMutex->owner = ZH_THREAD_SELF();
      }

      ZH_CRITICAL_UNLOCK( pMutex->mutex );
      zh_vmLock();

      if( pResult )
      {
         pResult = zh_itemNew( pResult );
         zh_stackPop();
      }
   }
   return pResult;
}

PZH_ITEM zh_threadMutexTimedSubscribe( PZH_ITEM pItem, ZH_ULONG ulMilliSec, ZH_BOOL fClear )
{
   PZH_MUTEX pMutex = zh_mutexPtr( pItem );
   PZH_ITEM pResult = NULL;

   if( pMutex )
   {
      ZH_STACK_TLS_PRELOAD
      int lock_count = 0;

      zh_vmUnlock();
      ZH_CRITICAL_LOCK( pMutex->mutex );

      if( fClear && pMutex->events )
      {
         zh_vmLockForce();
         zh_itemMove( zh_stackAllocItem(), pMutex->events );
         pMutex->events = NULL;
         ZH_CRITICAL_UNLOCK( pMutex->mutex );
         zh_stackPop();
         zh_vmUnlock();
         ZH_CRITICAL_LOCK( pMutex->mutex );
      }

      if( ulMilliSec && ! ( pMutex->events && zh_arrayLen( pMutex->events ) > 0 ) )
      {
         /* release own lock from this mutex */
         if( ZH_THREAD_EQUAL( pMutex->owner, ZH_THREAD_SELF() ) )
         {
            lock_count = pMutex->lock_count;
            pMutex->lock_count = 0;
            pMutex->owner = ( ZH_THREAD_ID ) 0;
            if( pMutex->lockers )
               ZH_COND_SIGNAL( pMutex->cond_l );
         }

         pMutex->waiters++;
#  if defined( ZH_PTHREAD_API )
         {
            struct timespec ts;

            zh_threadTimeInit( &ts, ulMilliSec );
            while( ( ! pMutex->events || zh_arrayLen( pMutex->events ) == 0 ) &&
                   zh_vmRequestQuery() == 0 )
            {
               if( pthread_cond_timedwait( &pMutex->cond_w, &pMutex->mutex, &ts ) != 0 )
                  break;
            }
         }
#  else
         {
#     if defined( ZH_COND_ZIHER_SUPPORT )
            _zh_thread_cond_wait( &pMutex->cond_w, &pMutex->mutex, ulMilliSec );
#     else
            ZH_CRITICAL_UNLOCK( pMutex->mutex );
            ( void ) ZH_COND_TIMEDWAIT( pMutex->cond_w, ulMilliSec );
            ZH_CRITICAL_LOCK( pMutex->mutex );
#     endif
         }
#  endif
         pMutex->waiters--;
      }

      if( pMutex->events && zh_arrayLen( pMutex->events ) > 0 )
      {
         zh_vmLockForce();
         pResult = zh_stackAllocItem();
         zh_arrayGet( pMutex->events, 1, pResult );
         zh_arrayDel( pMutex->events, 1 );
         zh_arraySize( pMutex->events, zh_arrayLen( pMutex->events ) - 1 );
         zh_vmUnlock();
      }

      /* restore the own lock on this mutex if necessary */
      if( lock_count )
      {
         if( pMutex->owner )
         {
            pMutex->lockers++;
            while( pMutex->lock_count != 0 )
            {
#  if defined( ZH_PTHREAD_API )
               pthread_cond_wait( &pMutex->cond_l, &pMutex->mutex );
#  elif defined( ZH_COND_ZIHER_SUPPORT )
               _zh_thread_cond_wait( &pMutex->cond_l, &pMutex->mutex, ZH_THREAD_INFINITE_WAIT );
#  else
               ZH_CRITICAL_UNLOCK( pMutex->mutex );
               ( void ) ZH_COND_WAIT( pMutex->cond_l );
               ZH_CRITICAL_LOCK( pMutex->mutex );
#  endif
            }
            pMutex->lockers--;
         }
         pMutex->lock_count = lock_count;
         pMutex->owner = ZH_THREAD_SELF();
      }

      ZH_CRITICAL_UNLOCK( pMutex->mutex );
      zh_vmLock();

      if( pResult )
      {
         pResult = zh_itemNew( pResult );
         zh_stackPop();
      }
   }
   return pResult;
}

ZH_FUNC( ZH_MUTEXEXISTS )
{
   ZH_STACK_TLS_PRELOAD
   zh_retl( zh_itemGetPtrGC( zh_param( 1, ZH_IT_POINTER ), &s_gcMutexFuncs ) != NULL );
}

ZH_FUNC( ZH_MUTEXCREATE )
{
   zh_itemReturnRelease( zh_threadMutexCreate() );
}

ZH_FUNC( ZH_MUTEXLOCK )
{
   PZH_ITEM pItem = zh_mutexParam( 1 );

   if( pItem )
   {
      ZH_STACK_TLS_PRELOAD
      if( ZH_IS_PARAM_NUM( 2 ) )
      {
         ZH_ULONG ulMilliSec = 0;
         double dTimeOut = zh_parnd( 2 );
         if( dTimeOut > 0 )
            ulMilliSec = ( ZH_ULONG ) ( dTimeOut * 1000 );
         zh_retl( zh_threadMutexTimedLock( pItem, ulMilliSec ) );
      }
      else
         zh_retl( zh_threadMutexLock( pItem ) );
   }
}

ZH_FUNC( ZH_MUTEXUNLOCK )
{
   PZH_ITEM pItem = zh_mutexParam( 1 );

   if( pItem )
   {
      ZH_STACK_TLS_PRELOAD
      zh_retl( zh_threadMutexUnlock( pItem ) );
   }
}

ZH_FUNC( ZH_MUTEXNOTIFY )
{
   PZH_ITEM pItem = zh_mutexParam( 1 );

   if( pItem )
      zh_threadMutexNotify( pItem, zh_param( 2, ZH_IT_ANY ), ZH_FALSE );
}

ZH_FUNC( ZH_MUTEXNOTIFYALL )
{
   PZH_ITEM pItem = zh_mutexParam( 1 );

   if( pItem )
      zh_threadMutexNotify( pItem, zh_param( 2, ZH_IT_ANY ), ZH_TRUE );
}

ZH_FUNC( ZH_MUTEXSUBSCRIBE )
{
   PZH_ITEM pItem = zh_mutexParam( 1 );

   if( pItem )
   {
      ZH_STACK_TLS_PRELOAD
      PZH_ITEM pResult;

      if( ZH_IS_PARAM_NUM( 2 ) )
      {
         ZH_ULONG ulMilliSec = 0;
         double dTimeOut = zh_parnd( 2 );
         if( dTimeOut > 0 )
            ulMilliSec = ( ZH_ULONG ) ( dTimeOut * 1000 );
         pResult = zh_threadMutexTimedSubscribe( pItem, ulMilliSec, ZH_FALSE );
      }
      else
         pResult = zh_threadMutexSubscribe( pItem, ZH_FALSE );

      if( pResult )
      {
         zh_itemParamStoreForward( 3, pResult );
         zh_itemRelease( pResult );
         zh_retl( ZH_TRUE );
      }
      else
         zh_retl( ZH_FALSE );
   }
}

ZH_FUNC( ZH_MUTEXSUBSCRIBENOW )
{
   PZH_ITEM pItem = zh_mutexParam( 1 );

   if( pItem )
   {
      ZH_STACK_TLS_PRELOAD
      PZH_ITEM pResult;

      if( ZH_IS_PARAM_NUM( 2 ) )
      {
         ZH_ULONG ulMilliSec = 0;
         double dTimeOut = zh_parnd( 2 );
         if( dTimeOut > 0 )
            ulMilliSec = ( ZH_ULONG ) ( dTimeOut * 1000 );
         pResult = zh_threadMutexTimedSubscribe( pItem, ulMilliSec, ZH_TRUE );
      }
      else
         pResult = zh_threadMutexSubscribe( pItem, ZH_TRUE );

      if( pResult )
      {
         zh_itemParamStoreForward( 3, pResult );
         zh_itemRelease( pResult );
         zh_retl( ZH_TRUE );
      }
      else
         zh_retl( ZH_FALSE );
   }
}

ZH_FUNC( ZH_MUTEXEVAL )
{
   PZH_ITEM pItem = zh_mutexParam( 1 );

   if( pItem )
   {
      PZH_ITEM pEval = zh_param( 2, ZH_IT_EVALITEM );

      if( pEval )
      {
         ZH_STACK_TLS_PRELOAD
         int iPCount = zh_pcount(), iParam;

         if( zh_threadMutexLock( pItem ) )
         {
            zh_vmPushEvalSym();
            zh_vmPush( pEval );
            for( iParam = 3; iParam <= iPCount; iParam++ )
               zh_vmPush( zh_stackItemFromBase( iParam ) );
            zh_vmSend( ( ZH_USHORT ) ( iPCount - 2 ) );
            zh_threadMutexUnlock( pItem );
         }
      }
      else
         zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
   }
}

ZH_FUNC( ZH_MUTEXQUEUEINFO )
{
   PZH_ITEM pItem = zh_mutexParam( 1 );

   if( pItem )
   {
      PZH_MUTEX pMutex = zh_mutexPtr( pItem );

      if( pMutex )
      {
         ZH_STACK_TLS_PRELOAD
         zh_storni( pMutex->waiters, 2 );
         zh_storns( pMutex->events ? zh_arrayLen( pMutex->events ) : 0, 3 );
         zh_retl( ZH_TRUE );
      }
   }
}

ZH_FUNC( ZH_MTVM )
{
   ZH_STACK_TLS_PRELOAD
   zh_retl( ZH_TRUE );
}

/* function to use in REQUEST statement in .zh code to force MT ZHVM */
ZH_FUNC( ZH_MT ) { ; }

