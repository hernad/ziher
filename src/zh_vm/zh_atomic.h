/*
 * Header file with functions for atomic operations
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

#ifndef ZH_ATOMIC_H_
#define ZH_ATOMIC_H_

#include "zh_defs.h"

#if defined( ZH_OS_WIN )
#  include <windows.h>
#elif defined( ZH_OS_DARWIN )
#  include <libkern/OSAtomic.h>
#endif
#if defined( __SVR4 )
#  include <thread.h>
#endif
#if defined( ZH_OS_UNIX )
#  include <sched.h>
#endif


ZH_EXTERN_BEGIN

/* yield the processor */
#if defined( ZH_OS_WIN )
#  define ZH_SCHED_YIELD()    Sleep( 0 )
#elif defined( ZH_OS_UNIX )
#  define ZH_SCHED_YIELD()    sched_yield()
#else
#  define ZH_SCHED_YIELD()    sleep( 0 );
#endif


/* Inline assembler version of atomic operations on memory reference counters */
#if defined( __GNUC__ ) || ( defined( ZH_OS_WIN ) && defined( __clang__ ) )

#  if defined( ZH_USE_GCCATOMIC_OFF )
#     undef ZH_USE_GCCATOMIC
#  elif ( __GNUC__ > 4 || ( __GNUC__ == 4 && __GNUC_MINOR__ >= 1 ) ) && \
        ! defined( __MINGW32CE__ ) && ! defined( ZH_USE_GCCATOMIC )
#     define ZH_USE_GCCATOMIC
#  elif defined( ZH_OS_WIN ) && defined( __clang__ )
#     define ZH_USE_GCCATOMIC
#  endif

#  if defined( ZH_USE_GCCATOMIC )

#     define ZH_ATOM_INC( p )       __sync_add_and_fetch( (p), 1 )
#     define ZH_ATOM_DEC( p )       __sync_sub_and_fetch( (p), 1 )
#     define ZH_ATOM_GET( p )       ( *(p) )
#     define ZH_ATOM_SET( p, n )    do { *(p) = (n); } while(0)

      static __inline__ void zh_spinlock_acquire( int * l )
      {
         for( ;; )
         {
            if( ! __sync_lock_test_and_set( l, 1 ) )
               return;

            #ifdef ZH_SPINLOCK_REPEAT
               if( ! __sync_lock_test_and_set( l, 1 ) )
                  return;
            #endif
            ZH_SCHED_YIELD();
         }
      }

#     define ZH_SPINLOCK_T          int
#     define ZH_SPINLOCK_INIT       0
#     define ZH_SPINLOCK_TRY(l)     (__sync_lock_test_and_set(l, 1)==0)
#     define ZH_SPINLOCK_RELEASE(l) __sync_lock_release(l)
#     define ZH_SPINLOCK_ACQUIRE(l) zh_spinlock_acquire(l)

#  elif defined( ZH_CPU_X86 ) || defined( ZH_CPU_X86_64 )

#     if ZH_COUNTER_SIZE == 4

         static __inline__ void zh_atomic_inc32( volatile int * p )
         {
            __asm__ __volatile__(
               "lock; incl %0\n"
               :"=m" (*p) :"m" (*p)
            );
         }

         static __inline__ int zh_atomic_dec32( volatile int * p )
         {
            unsigned char c;
            __asm__ __volatile__(
               "lock; decl %0\n"
               "sete %1\n"
               :"=m" (*p), "=qm" (c) :"m" (*p) : "memory"
            );
            return c == 0;
         }

#        define ZH_ATOM_INC( p )    ( zh_atomic_inc32( ( volatile int * ) (p) ) )
#        define ZH_ATOM_DEC( p )    ( zh_atomic_dec32( ( volatile int * ) (p) ) )
#        define ZH_ATOM_GET( p )    (*(int volatile *)(p))
#        define ZH_ATOM_SET( p, n ) do { *((int volatile *)(p)) = (n); } while(0)

#     elif ZH_COUNTER_SIZE == 8

         static __inline__ void zh_atomic_inc64( volatile long long int * p )
         {
            __asm__ __volatile__(
               "lock; incq %0\n"
               :"=m" (*p) :"m" (*p)
            );
         }

         static __inline__ int zh_atomic_dec64( volatile long long int * p )
         {
            unsigned char c;
            __asm__ __volatile__(
               "lock; decq %0\n"
               "sete %1\n"
               :"=m" (*p), "=qm" (c) :"m" (*p) : "memory"
            );
            return c == 0;
         }

#        define ZH_ATOM_INC( p )    ( zh_atomic_inc64( ( volatile long long int * ) (p) ) )
#        define ZH_ATOM_DEC( p )    ( zh_atomic_dec64( ( volatile long long int * ) (p) ) )
#        define ZH_ATOM_GET( p )    (*(long long int volatile *)(p))
#        define ZH_ATOM_SET( p, n ) do { *((long long int volatile *)(p)) = (n); } while(0)

#     endif

      static __inline__ int zh_spinlock_trylock( volatile int * p )
      {
         int i = 1;
         __asm__ __volatile__(
            "xchgl %0, %1\n\t"
            : "=r" (i)
            : "m" (*p), "0" (i)
            : "memory"
         );
         return i;
      }

      static __inline__ void zh_spinlock_acquire( volatile int * l )
      {
         for( ;; )
         {
            if( ! zh_spinlock_trylock( l ) )
               return;
            #ifdef ZH_SPINLOCK_REPEAT
               if( ! zh_spinlock_trylock( l ) )
                  return;
            #endif
            ZH_SCHED_YIELD();
         }
      }

      static __inline__ void zh_spinlock_release( volatile int * l )
      {
         *l = 0;
      }

#     define ZH_SPINLOCK_T          volatile int
#     define ZH_SPINLOCK_INIT       0
#     define ZH_SPINLOCK_TRY(l)     (zh_spinlock_trylock(l)==0)
#     define ZH_SPINLOCK_RELEASE(l) zh_spinlock_release(l)
#     define ZH_SPINLOCK_ACQUIRE(l) zh_spinlock_acquire(l)

#  elif defined( ZH_CPU_PPC )

#     if ZH_COUNTER_SIZE == 4

         static __inline__ void zh_atomic_inc32( volatile int * p )
         {
            int i;

            __asm__ __volatile__(
               "1:   lwarx    %0,0,%2\n\t"
               "     addic    %0,%0,1\n\t"
               "     stwcx.   %0,0,%2\n\t"
               "     bne-     1b\n\t"
               : "=&r" (i), "=m" (*p) : "r" (p), "m" (*p) : "cc"
            );
         }

         static __inline__ int zh_atomic_dec32( volatile int * p )
         {
            int i;

            __asm__ __volatile__(
               "1:   lwarx    %0,0,%1\n\t"
               "     addic    %0,%0,-1\n\t"
               "     stwcx.   %0,0,%1\n\t"
               "     bne-     1b\n\t"
               "     isync\n\t"
               : "=&r" (i) : "r" (p) : "cc", "memory"
            );
            return i;
         }

#        define ZH_ATOM_INC( p )    ( zh_atomic_inc32( ( volatile int * ) (p) ) )
#        define ZH_ATOM_DEC( p )    ( zh_atomic_dec32( ( volatile int * ) (p) ) )
#        define ZH_ATOM_GET( p )    (*(int volatile *)(p))
#        define ZH_ATOM_SET( p, n ) do { *((int volatile *)(p)) = (n); } while(0)

#     elif ZH_COUNTER_SIZE == 8

         /* TODO: */

#     endif

#  endif  /* ???CPU?? */

#elif defined( _MSC_VER )

#  if defined( ZH_CPU_X86 )

#     if ZH_COUNTER_SIZE == 4

         static __inline void zh_atomic_inc32( volatile int * p )
         {
            __asm mov eax, p
            __asm lock inc dword ptr [eax]
         }

         static __inline int zh_atomic_dec32( volatile int * p )
         {
            unsigned char c;

            __asm mov eax, p
            __asm lock dec dword ptr [eax]
            __asm setne c

            return c;
         }

#        define ZH_ATOM_INC( p )    ( zh_atomic_inc32( ( volatile int * ) (p) ) )
#        define ZH_ATOM_DEC( p )    ( zh_atomic_dec32( ( volatile int * ) (p) ) )
#        define ZH_ATOM_GET( p )    (*(int volatile *)(p))
#        define ZH_ATOM_SET( p, n ) do { *((int volatile *)(p)) = (n); } while(0)

#     elif ZH_COUNTER_SIZE == 8

         /* TODO: */

#     endif

#  endif    /* x86 */


#endif  /* C compiler */


#if defined( ZH_OS_WIN )

   /* Atomic operations on memory reference counters */
#  if ! defined( ZH_ATOM_INC ) || ! defined( ZH_ATOM_DEC )
#     undef ZH_ATOM_DEC
#     undef ZH_ATOM_INC
#     undef ZH_ATOM_GET
#     undef ZH_ATOM_SET
#     if ZH_COUNTER_SIZE == 8
#        define ZH_ATOM_INC( p )    (InterlockedIncrement64((LONGLONG *)(p)))
#        define ZH_ATOM_DEC( p )    (InterlockedDecrement64((LONGLONG *)(p)))
#        define ZH_ATOM_GET( p )    (*(LONGLONG volatile *)(p))
#        define ZH_ATOM_SET( p, n ) do { (*(LONGLONG volatile *)(p)) = (n); } while(0)
#     else
#        define ZH_ATOM_INC( p )    (InterlockedIncrement((LONG *)(p)))
#        define ZH_ATOM_DEC( p )    (InterlockedDecrement((LONG *)(p)))
#        define ZH_ATOM_GET( p )    (*(LONG volatile *)(p))
#        define ZH_ATOM_SET( p, n ) do { (*(LONG volatile *)(p)) = (n); } while(0)
#     endif
#  endif

   /* Spin locks */
#  if ! defined( ZH_SPINLOCK_T )
#     define ZH_SPINLOCK_T          volatile LONG
#     define ZH_SPINLOCK_INIT       0
#     define ZH_SPINLOCK_TRY(l)     (! InterlockedExchange( (LONG*)(l), 1 ))
#     define ZH_SPINLOCK_RELEASE(l) ( *(l) = 0 )
#  endif

#elif defined( ZH_OS_DARWIN )

   /* Atomic operations on memory reference counters */
#  if ! defined( ZH_ATOM_INC ) || ! defined( ZH_ATOM_DEC )
#     undef ZH_ATOM_DEC
#     undef ZH_ATOM_INC
#     undef ZH_ATOM_GET
#     undef ZH_ATOM_SET
#     if ZH_COUNTER_SIZE == 8
#        define ZH_ATOM_INC( p )    (OSAtomicIncrement64((int64_t *)(p)))
#        define ZH_ATOM_DEC( p )    (OSAtomicDecrement64((int64_t *)(p)))
#        define ZH_ATOM_GET( p )    (*(int64_t volatile *)(p))
#        define ZH_ATOM_SET( p, n ) do { *((int64_t volatile *)(p)) = (n); } while(0)
#     else
#        define ZH_ATOM_INC( p )    (OSAtomicIncrement32((int32_t *)(p)))
#        define ZH_ATOM_DEC( p )    (OSAtomicDecrement32((int32_t *)(p)))
#        define ZH_ATOM_GET( p )    (*(volatile int32_t *)(p))
#        define ZH_ATOM_SET( p, n ) do { *((volatile int32_t *)(p)) = (n); } while(0)
#     endif
#  endif

   /* Spin locks */
#  if ! defined( ZH_SPINLOCK_T )
#     undef ZH_SPINLOCK_T
#     undef ZH_SPINLOCK_INIT
#     undef ZH_SPINLOCK_TRY
#     undef ZH_SPINLOCK_RELEASE
#     undef ZH_SPINLOCK_ACQUIRE
#     define ZH_SPINLOCK_T          OSSpinLock
#     define ZH_SPINLOCK_INIT       OS_SPINLOCK_INIT
#     define ZH_SPINLOCK_TRY(l)     OSSpinLockTry(l)
#     define ZH_SPINLOCK_RELEASE(l) OSSpinLockUnlock(l)
#     define ZH_SPINLOCK_ACQUIRE(l) OSSpinLockLock(l)
#  endif

#elif defined( ZH_OS_SUNOS )

   /* Atomic operations on memory reference counters */
#  if ! defined( ZH_ATOM_INC ) || ! defined( ZH_ATOM_DEC )
#     undef ZH_ATOM_DEC
#     undef ZH_ATOM_INC
#     undef ZH_ATOM_GET
#     undef ZH_ATOM_SET
#     define ZH_ATOM_INC( p )    atomic_inc_ulong((ulong_t *)(p))
#     define ZH_ATOM_DEC( p )    atomic_dec_ulong_nv((ulong_t *)(p))
#     define ZH_ATOM_GET( p )    (*(ulong_t volatile *)(p))
#     define ZH_ATOM_SET( p, n ) do { *((ulong_t volatile *)(p)) = (n); } while(0)
#  endif

   /* Spin locks */
#  if ! defined( ZH_SPINLOCK_T )
#     define ZH_SPINLOCK_T          volatile uint_t
#     define ZH_SPINLOCK_INIT       0
#     define ZH_SPINLOCK_TRY(l)     ( ! atomic_swap_uint( (l), 1 ) )
#     define ZH_SPINLOCK_RELEASE(l) ( *(l) = 0 )
#  endif

#endif  /* ZH_OS_??? */

#if defined( ZH_SPINLOCK_T )
#  if ! defined( ZH_SPINLOCK_ACQUIRE )
#     ifdef ZH_SPINLOCK_REPEAT
#        define ZH_SPINLOCK_ACQUIRE(l) do { \
                                          if( ZH_SPINLOCK_TRY( l ) ) \
                                             break; \
                                          if( ZH_SPINLOCK_TRY( l ) ) \
                                             break; \
                                          ZH_SCHED_YIELD(); \
                                       } while(1)
#     else
#        define ZH_SPINLOCK_ACQUIRE(l) do { \
                                          if( ZH_SPINLOCK_TRY( l ) ) \
                                             break; \
                                          ZH_SCHED_YIELD(); \
                                       } while(1)
#     endif
#  endif
#  if ! defined( ZH_SPINLOCK_R )
      struct zh_spinlock_r
      {
         ZH_SPINLOCK_T  lock;
         unsigned int   count;
         ZH_THREAD_ID   thid;
      };

      static ZH_FORCEINLINE void zh_spinlock_release_r( struct zh_spinlock_r * sl )
      {
         if( --sl->count == 0 )
         {
            sl->thid = 0;
            ZH_SPINLOCK_RELEASE( &sl->lock );
         }
      }

      static ZH_FORCEINLINE int zh_spinlock_try_r( struct zh_spinlock_r * sl )
      {
         ZH_SPINLOCK_T * l = &sl->lock;
         int r = 0;
         if( *l != ZH_SPINLOCK_INIT )
         {
            if( sl->thid == ZH_THREAD_SELF() )
            {
               sl->count++;
               r = 1;
            }
         }
         else if( ZH_SPINLOCK_TRY( l ) )
         {
            sl->thid = ZH_THREAD_SELF();
            sl->count = 1;
            r = 1;
         }
         return r;
      }

#     ifndef ZH_SPINLOCK_REPEAT
#        define ZH_SPINLOCK_REPEAT     63
#     endif

#if defined( __BORLANDC__ )  /* workaround for compiler limitation */
#     define zh_spinlock_acquire_r( sl ) \
      do { \
         ZH_SPINLOCK_T * l = &(sl)->lock; \
         int count = ZH_SPINLOCK_REPEAT; \
         for( ;; ) \
         { \
            if( *l != ZH_SPINLOCK_INIT ) \
            { \
               if( (sl)->thid == ZH_THREAD_SELF() ) \
               { \
                  (sl)->count++; \
                  break; \
               } \
            } \
            else if( ZH_SPINLOCK_TRY( l ) ) \
            { \
               (sl)->thid = ZH_THREAD_SELF(); \
               (sl)->count = 1; \
               break; \
            } \
            if( --count == 0 ) \
            { \
               ZH_SCHED_YIELD(); \
               count = ZH_SPINLOCK_REPEAT; \
            } \
         } \
      } while( 0 )
#else
      static ZH_FORCEINLINE void zh_spinlock_acquire_r( struct zh_spinlock_r * sl )
      {
         ZH_SPINLOCK_T * l = &sl->lock;
         int count = ZH_SPINLOCK_REPEAT;
         for( ;; )
         {
            if( *l != ZH_SPINLOCK_INIT )
            {
               if( sl->thid == ZH_THREAD_SELF() )
               {
                  sl->count++;
                  break;
               }
            }
            else if( ZH_SPINLOCK_TRY( l ) )
            {
               sl->thid = ZH_THREAD_SELF();
               sl->count = 1;
               break;
            }
            if( --count == 0 )
            {
               ZH_SCHED_YIELD();
               count = ZH_SPINLOCK_REPEAT;
            }
         }
      }
#endif

#     define ZH_SPINLOCK_R             struct zh_spinlock_r
#     define ZH_SPINLOCK_INITVAL_R     { 0, 0, 0 }
#     define ZH_SPINLOCK_INIT_R(l)     do { (l)->lock = 0; (l)->count = 0; (l)->thid = 0; } while( 0 )
#     define ZH_SPINLOCK_TRY_R(l)      zh_spinlock_try_r(l)
#     define ZH_SPINLOCK_RELEASE_R(l)  zh_spinlock_release_r(l)
#     define ZH_SPINLOCK_ACQUIRE_R(l)  zh_spinlock_acquire_r(l)
#  endif /* ! ZH_SPINLOCK_R */
#endif /* ZH_SPINLOCK_T */

ZH_EXTERN_END

#endif /* ZH_ATOMIC_H_ */
