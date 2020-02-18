/*
 * zh_SecondsCPU()
 *
 * Copyright 2003 Przemyslaw Czerpak <druzus@acn.waw.pl>
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
#include "zh_date.h"

#if defined( ZH_OS_UNIX )
      #include <sys/times.h>
   #include <unistd.h>
#endif
#if defined( ZH_OS_WIN )
   #include <windows.h>
#endif

/*
   SecondsCPU( n ) --> nTime
   FlagShip/CLIP compatible function, which reports how many CPU and/or
   system seconds have elapsed since the beginning of the program execution.
    n == 1  utime  -> user CPU time of the current process
    n == 2  stime  -> system CPU time behalf of the current process
    n == 3  u + s  -> sum of utime + stime (default)
    n == 11 cutime -> sum of the user CPU time of the current + child process
    n == 12 cstime -> sum of the system CPU time of the current + child process
    n == 13 cu+cs  -> sum of cutime + cstime
 */

double zh_secondsCPU( int n )
{
   double d = 0.0;

#if defined( ZH_OS_WIN ) && ! defined( ZH_OS_WIN_CE ) && ! defined( ZH_OS_UNIX )
   FILETIME Create, Exit, Kernel, User;
#endif


if( ( n < 1 || n > 3 ) && ( n < 11 || n > 13 ) )
      n = 3;

#if defined( ZH_OS_UNIX )
   {
      struct tms tm;

      times( &tm );

      if( n > 10 )
      {
         n -= 10;
         if( n & 1 )
            d += tm.tms_cutime;
         if( n & 2 )
            d += tm.tms_cstime;
      }
      if( n & 1 )
         d += tm.tms_utime;
      if( n & 2 )
         d += tm.tms_stime;

      /* In POSIX-1996 the CLK_TCK symbol is mentioned as obsolescent */
      #if 0
      d /= CLK_TCK;
      #endif
      d /= ( double ) sysconf( _SC_CLK_TCK );
   }
#else
   if( n > 10 )
      n -= 10;
#if defined( ZH_OS_WIN )
   if( zh_iswinnt() &&
       GetProcessTimes( GetCurrentProcess(), &Create, &Exit, &Kernel, &User ) )
   {
      if( n & 1 )
      {
         d += ( double ) ( ( ( ZH_MAXINT ) User.dwHighDateTime << 32 ) +
                             ( ZH_MAXINT ) User.dwLowDateTime );
      }
      if( n & 2 )
      {
         d += ( double ) ( ( ( ZH_MAXINT ) Kernel.dwHighDateTime << 32 ) +
                             ( ZH_MAXINT ) Kernel.dwLowDateTime );
      }
      d /= 10000000.0;
   }
   else
#endif
   {
      /* TODO: this code is only for DOS and other platforms which cannot
               calculate process time */

      if( n & 1 )
         d = zh_dateSeconds();
   }
#endif
   return d;
}
