/*
 * Header file with MT mode functions
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

#ifndef ZH_THREAD_H_
#define ZH_THREAD_H_

#include "zh_api.h"
#include "zh_set.h"


#if defined( ZH_OS_LINUX ) || defined( ZH_OS_DARWIN )
#  include <pthread.h>
#  define ZH_PTHREAD_API
#elif defined( ZH_OS_WIN )
#  include <windows.h>
#  include <process.h>
#endif

ZH_EXTERN_BEGIN

#if defined( ZH_PTHREAD_API )

   typedef ZH_MAXINT       ZH_THREAD_NO;
   typedef pthread_t       ZH_THREAD_ID;
   typedef pthread_t       ZH_THREAD_HANDLE;
   typedef pthread_mutex_t ZH_RAWCRITICAL_T;
   typedef pthread_cond_t  ZH_RAWCOND_T;

#  define ZH_THREAD_STARTFUNC( func )     void * func( void * Cargo )
#  define ZH_THREAD_END                   return NULL;
#  define ZH_THREAD_RAWEND                return NULL;

#  define ZH_THREAD_SELF()          pthread_self()
#  define ZH_THREAD_EQUAL( x, y )   pthread_equal( x, y )

#  define ZH_CRITICAL_INIT(v)       pthread_mutex_init( &(v), NULL )
#  define ZH_CRITICAL_DESTROY(v)    pthread_mutex_destroy( &(v) )
#  define ZH_CRITICAL_LOCK(v)       pthread_mutex_lock( &(v) )
#  define ZH_CRITICAL_UNLOCK(v)     pthread_mutex_unlock( &(v) )
#  define ZH_COND_INIT(v)           pthread_cond_init( &(v), NULL )
#  define ZH_COND_DESTROY(v)        pthread_cond_destroy( &(v) )
#  define ZH_COND_SIGNAL(v)         pthread_cond_signal( &(v) )
#  define ZH_COND_SIGNALN(v,n)      pthread_cond_broadcast( &(v) )

#  define ZH_COND_OS_SUPPORT        /* OS support for conditional variables */
#  undef  ZH_COND_ZIHER_SUPPORT

#  if defined( PTHREAD_MUTEX_INITIALIZER ) && ! defined( ZH_CRITICAL_NEED_INIT )
      typedef pthread_mutex_t          ZH_CRITICAL_T;
#     define ZH_CRITICAL_NEW( name )   ZH_CRITICAL_T name = PTHREAD_MUTEX_INITIALIZER
#     define ZH_CRITICAL_GET(v)        ( v )
#  else
      /* platform does not support static mutex initialization */
#     if ! defined( ZH_CRITICAL_NEED_INIT )
#        define ZH_CRITICAL_NEED_INIT
#     endif
#     define ZH_CRITICAL_GET(v)        ( &( (v)->critical.value ) )
#  endif

#  if defined( PTHREAD_COND_INITIALIZER ) && ! defined( ZH_COND_NEED_INIT )
      typedef pthread_cond_t           ZH_COND_T;
#     define ZH_COND_NEW( name )       ZH_COND_T name = PTHREAD_COND_INITIALIZER
#     define ZH_COND_GET(v)            ( v )
#  else
      /* platform does not support static condition var initialization */
#     if ! defined( ZH_COND_NEED_INIT )
#        define ZH_COND_NEED_INIT
#     endif
#     define ZH_COND_GET(v)            ( &( (v)->cond.value ) )
#  endif

#elif defined( ZH_OS_WIN )

   typedef ZH_MAXINT          ZH_THREAD_NO;
   typedef HANDLE             ZH_THREAD_HANDLE;
   typedef CRITICAL_SECTION   ZH_RAWCRITICAL_T;
   typedef HANDLE             ZH_OSCOND_T;

#  if defined( ZH_THREAD_RAWWINAPI )
      typedef DWORD                       ZH_THREAD_ID;
#     define ZH_THREAD_STARTFUNC( func )  DWORD WINAPI func( void * Cargo )
#     define ZH_THREAD_END                ExitThread( 0 ); return 0;
#  else
      typedef unsigned                    ZH_THREAD_ID;
#     define ZH_THREAD_STARTFUNC( func )  unsigned __stdcall func( void * Cargo )
#     define ZH_THREAD_END                _endthreadex( 0 ); return 0;
#  endif
#  define ZH_THREAD_RAWEND             return 0;

#  define ZH_THREAD_SELF()          GetCurrentThreadId()

#  define ZH_CRITICAL_INIT(v)       InitializeCriticalSection( &(v) )
#  define ZH_CRITICAL_DESTROY(v)    DeleteCriticalSection( &(v) )
#  define ZH_CRITICAL_LOCK(v)       EnterCriticalSection( &(v) )
#  define ZH_CRITICAL_UNLOCK(v)     LeaveCriticalSection( &(v) )

#  undef  ZH_COND_OS_SUPPORT
#  undef  ZH_COND_NEED_INIT
#  define ZH_COND_ZIHER_SUPPORT
#  define ZH_CRITICAL_NEED_INIT

#  define ZH_THREAD_INFINITE_WAIT   INFINITE


#else

   typedef int ZH_THREAD_NO;
   typedef int ZH_THREAD_ID;
   typedef int ZH_THREAD_HANDLE;
   typedef int ZH_CRITICAL_T;
   typedef int ZH_RAWCRITICAL_T;
   typedef int ZH_COND_T;
   typedef int ZH_RAWCOND_T;

#  define ZH_THREAD_STARTFUNC( func )     void func( void * Cargo )
#  define ZH_THREAD_END                   return;
#  define ZH_THREAD_RAWEND                return;

#  define ZH_CRITICAL_NEW( name )      ZH_CRITICAL_T name = 0
#  define ZH_COND_NEW( name )          ZH_COND_T name = 0
#  define ZH_THREAD_SELF()             (-1)

#  define ZH_CRITICAL_LOCK(v)
#  define ZH_CRITICAL_UNLOCK(v)
#  define ZH_COND_SIGNAL(v)
#  define ZH_COND_SIGNALN(v,n)
#  define ZH_COND_WAIT(v)           ( ZH_FALSE )
#  define ZH_COND_TIMEDWAIT(v,n)    ( ZH_FALSE )

#endif

#if defined( ZH_COND_ZIHER_SUPPORT )

   typedef struct _ZH_WAIT_LIST
   {
      struct _ZH_WAIT_LIST *  prev;
      struct _ZH_WAIT_LIST *  next;
      ZH_OSCOND_T             cond;
      ZH_BOOL                 signaled;
   } ZH_WAIT_LIST, * PZH_WAIT_LIST;

   typedef struct
   {
      PZH_WAIT_LIST     waiters;
   } ZH_COND_T, ZH_RAWCOND_T;

#  define ZH_COND_NEW( name )       ZH_COND_T name = { NULL }

#  define ZH_COND_SIGNAL(v)         _zh_thread_cond_signal( &(v) )
#  define ZH_COND_SIGNALN(v,n)      _zh_thread_cond_broadcast( &(v) )
#  define ZH_COND_WAIT(v)           _zh_thread_cond_wait( (v), ZH_THREAD_INFINITE_WAIT )
#  define ZH_COND_TIMEDWAIT(v,n)    _zh_thread_cond_wait( (v), (n) )

#endif

#ifdef ZH_CRITICAL_NEED_INIT
   typedef struct
   {
      ZH_BOOL fInit;
      union
      {
         int               dummy;
         ZH_RAWCRITICAL_T  value;
      } critical;
   } ZH_CRITICAL_T;
#  define ZH_CRITICAL_NEW( name )   ZH_CRITICAL_T name = { ZH_FALSE, { 0 } }
#endif /* ZH_CRITICAL_NEED_INIT */

#ifdef ZH_COND_NEED_INIT
#  if defined( ZH_COND_OS_SUPPORT )
      typedef struct
      {
         ZH_BOOL        fInit;
         union
         {
            int            dummy;
            ZH_RAWCOND_T   value;
         } cond;
      } ZH_COND_T;
#     define ZH_COND_NEW( name )       ZH_COND_T name = { ZH_FALSE, { 0 } }
#  else
      typedef struct
      {
         ZH_BOOL           fInit;
         int               waiters;
         union
         {
            int               dummy;
            ZH_RAWCOND_T      value;
         } cond;
         union
         {
            int               dummy;
            ZH_RAWCRITICAL_T  value;
         } critical;
      } ZH_COND_T;
#     define ZH_COND_NEW( name )       ZH_COND_T name = { ZH_FALSE, 0, { 0 }, { 0 } }
#  endif
#endif /* ZH_COND_NEED_INIT */

#ifndef ZH_THREAD_EQUAL
#  define ZH_THREAD_EQUAL( x, y )   ( (x) == (y) )
#endif

#ifndef ZH_THREAD_SHEDULER
#  define ZH_THREAD_SHEDULER()
#endif

#ifndef ZH_THREAD_INFINITE_WAIT
#  define ZH_THREAD_INFINITE_WAIT   ( ( ZH_ULONG ) -1 )
#endif

typedef ZH_THREAD_STARTFUNC( PZH_THREAD_STARTFUNC );

extern ZH_EXPORT void zh_threadReleaseCPU( void );

/* atomic operations */
extern ZH_EXPORT void        zh_atomic_set( volatile ZH_COUNTER * pCounter, ZH_COUNTER value );
extern ZH_EXPORT ZH_COUNTER  zh_atomic_get( volatile ZH_COUNTER * pCounter );
extern ZH_EXPORT void        zh_atomic_inc( volatile ZH_COUNTER * pCounter );
extern ZH_EXPORT ZH_BOOL     zh_atomic_dec( volatile ZH_COUNTER * pCounter ); /* returns ZH_TRUE when counter reaches 0 after decrementation */

/* Critical sections or fast non recursive MUTEXes */
extern ZH_EXPORT void     zh_threadEnterCriticalSection( ZH_CRITICAL_T * critical );
extern ZH_EXPORT void     zh_threadEnterCriticalSectionGC( ZH_CRITICAL_T * critical );
extern ZH_EXPORT void     zh_threadLeaveCriticalSection( ZH_CRITICAL_T * critical );

/* conditional variables */
extern ZH_EXPORT ZH_BOOL  zh_threadCondSignal( ZH_COND_T * cond );
extern ZH_EXPORT ZH_BOOL  zh_threadCondBroadcast( ZH_COND_T * cond );
extern ZH_EXPORT ZH_BOOL  zh_threadCondWait( ZH_COND_T * cond, ZH_CRITICAL_T * mutex );
extern ZH_EXPORT ZH_BOOL  zh_threadCondTimedWait( ZH_COND_T * cond, ZH_CRITICAL_T * mutex, ZH_ULONG ulMilliSec );

extern ZH_EXPORT ZH_THREAD_HANDLE zh_threadCreate( ZH_THREAD_ID * th_id, PZH_THREAD_STARTFUNC start_func, void * Cargo );
extern ZH_EXPORT ZH_BOOL  zh_threadJoin( ZH_THREAD_HANDLE th_h );
extern ZH_EXPORT ZH_BOOL  zh_threadDetach( ZH_THREAD_HANDLE th_h );
extern ZH_EXPORT ZH_THREAD_NO zh_threadNO( void );

/* used by .zh code */
extern ZH_EXPORT PZH_ITEM zh_threadMutexCreate( void );
extern ZH_EXPORT ZH_BOOL  zh_threadMutexLock( PZH_ITEM pItem );
extern ZH_EXPORT ZH_BOOL  zh_threadMutexTimedLock( PZH_ITEM pItem, ZH_ULONG ulMilliSec );
extern ZH_EXPORT ZH_BOOL  zh_threadMutexUnlock( PZH_ITEM pItem );
extern ZH_EXPORT void     zh_threadMutexNotify( PZH_ITEM pItem, PZH_ITEM pNotifier, ZH_BOOL fWaiting );
extern ZH_EXPORT PZH_ITEM zh_threadMutexSubscribe( PZH_ITEM pItem, ZH_BOOL fClear );
extern ZH_EXPORT PZH_ITEM zh_threadMutexTimedSubscribe( PZH_ITEM pItem, ZH_ULONG ulMilliSec, ZH_BOOL fClear );

#if defined( _ZH_API_INTERNAL_ )

typedef struct _ZH_THREADSTATE
{
   const char *   pszCDP;
   const char *   pszLang;
   const char *   pszDefRDD;
   PZH_SET_STRUCT pSet;
   void *         pI18N;
   void *         hGT;
   void *         pStackId;
   void *         cargo;
   PZH_CARGO_FUNC pFunc;
   ZH_BOOL        fActive;
   ZH_BOOL        fFinished;
   PZH_ITEM       pParams;
   PZH_ITEM       pMemvars;
   PZH_ITEM       pResult;
   PZH_ITEM       pThItm;
   ZH_THREAD_NO      th_no;
   ZH_THREAD_ID      th_id;
   ZH_THREAD_HANDLE  th_h;
   struct _ZH_THREADSTATE * pPrev;
   struct _ZH_THREADSTATE * pNext;
#if defined( ZH_COND_ZIHER_SUPPORT )
   ZH_WAIT_LIST   pWaitList;
#endif
} ZH_THREADSTATE, * PZH_THREADSTATE;

extern void zh_threadInit( void );
extern void zh_threadExit( void );

extern PZH_THREADSTATE zh_threadStateNew( void );
extern PZH_THREADSTATE zh_threadStateClone( ZH_ULONG ulAttr, PZH_ITEM pParams );
extern PZH_ITEM        zh_threadStart( ZH_ULONG ulAttr, PZH_CARGO_FUNC pFunc, void * cargo );

extern void    zh_threadMutexUnlockAll( void );
extern void    zh_threadMutexUnsubscribeAll( void );
extern void    zh_threadMutexSyncSignal( PZH_ITEM pItemMtx );
extern ZH_BOOL zh_threadMutexSyncWait( PZH_ITEM pItemMtx, ZH_ULONG ulMilliSec, PZH_ITEM pItemSync );

   
   
/* enable native compiler TLS support by default for this compilers
    * which are known that it will work correctly
*/
// https://gcc.gnu.org/onlinedocs/gcc-3.3.1/gcc/Thread-Local.html

#if ( defined( _MSC_VER ) && ( _MSC_VER > 1500 ) )
#     define ZH_USE_TLS
#endif
#if defined( __GNUC__ ) && __GNUC__ >= 3
#     define ZH_USE_TLS
#endif

#ifdef ZH_USE_TLS
#  if ( defined( __GNUC__ ) && __GNUC__ >= 3 )
#     define ZH_TLS_ATTR      __thread
#  elif defined( _MSC_VER )
#     define ZH_TLS_ATTR      __declspec( thread )
#  else
#     undef ZH_USE_TLS
#     error "TLS support undefined for this compiler" /* */
#  endif
#endif /* ZH_USE_TLS */


#endif /* _ZH_API_INTERNAL_ */

ZH_EXTERN_END

#endif /* ZH_THREAD_H_ */
