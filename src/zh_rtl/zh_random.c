/*
 * Random number generator routine
 *
 * Copyright 2011 Przemyslaw Czerpak
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
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
#include "zh_stack.h"

/* NOTE: core random generator algorithm is the work of Steve Park
         https://web.archive.org/web/www.cs.wm.edu/~va/software/park/ */

#define MODULUS     2147483647 /* DON'T CHANGE THIS VALUE */
#define MULTIPLIER  48271      /* DON'T CHANGE THIS VALUE */

static ZH_TSD_NEW( s_seed, sizeof( ZH_I32 ), NULL, NULL );
#define SEED_PTR    ( ( ZH_I32 * ) zh_stackGetTSD( &s_seed ) )

/* Returns a double value between 0 and 1 */
double zh_random_num( void )
{
   ZH_I32 * seed = SEED_PTR, t;

   t = *seed;
   if( t == 0 )
      t = ( ZH_I32 )
          ( ( zh_dateMilliSeconds() ^ ( ZH_PTRUINT ) zh_stackId() ) % MODULUS );

#if ! defined( ZH_LONG_LONG_OFF )
   t = ( ZH_I32 ) ( ( ZH_LONGLONG ) t * MULTIPLIER % MODULUS );
#else
   {
      const ZH_I32 Q = MODULUS / MULTIPLIER;
      const ZH_I32 R = MODULUS % MULTIPLIER;

      t = MULTIPLIER * ( t % Q ) - R * ( t / Q );
      if( t < 0 )
         t += MODULUS;
   }
#endif

   *seed = t;

   return ( double ) ( t - 1 ) / ( MODULUS - 1 );
}

void zh_random_seed( ZH_I32 seed )
{
   seed %= MODULUS;
   * SEED_PTR = ( seed < 0 ) ? seed + MODULUS : seed;
}

static void zh_random( double dRnd )
{
   if( ! ZH_IS_PARAM_NUM( 1 ) )
      zh_retnd( dRnd );
   else if( ! ZH_IS_PARAM_NUM( 2 ) )
      zh_retnd( dRnd * zh_parnd( 1 ) );
   else
   {
      double dX = zh_parnd( 2 );
      double dY = zh_parnd( 1 );
      if( dX > dY )
      {
         double dZ = dY;
         dY = dX;
         dX = dZ;
      }
      zh_retnd( dRnd * ( dY - dX ) + dX );
   }
}

/*
 * zh_Random() --> returns a real value n so that 0 <= n < 1
 * zh_Random( x ) --> returns a real number n so that 0 <= n < x
 * zh_Random( x, y ) --> Returns a real number n so that x <= n < y
 */
ZH_FUNC( ZH_RANDOM )
{
   zh_random( zh_random_num() );
}

ZH_FUNC( ZH_RANDNUM )
{
   zh_random( zh_random_num_secure() );
}

static void zh_randomint( double dRnd )
{
   if( ! ZH_IS_PARAM_NUM( 1 ) )
      zh_retni( dRnd >= 0.5 ? 0 : 1 );
   else if( ! ZH_IS_PARAM_NUM( 2 ) )
      zh_retnint( ( ZH_MAXINT ) ( 1 + ( dRnd * zh_parnint( 1 ) ) ) );
   else
   {
      ZH_MAXINT lX = zh_parnint( 1 );
      ZH_MAXINT lY = zh_parnint( 2 );
      if( lX > lY )
      {
         ZH_MAXINT lZ = lY;
         lY = lX;
         lX = lZ;
      }
      zh_retnint( ( ZH_MAXINT ) ( lX + ( dRnd * ( lY - lX + 1 ) ) ) );
   }
}

/*
 * zh_RandomInt() --> returns 0 or 1, evenly distributed
 * zh_RandomInt( N ) --> returns an integer between 1 and N (inclusive)
 * zh_RandomInt( x, y ) --> Returns an integer number between x and y (inclusive)
 * The integer returned is of the longest type available
 */
ZH_FUNC( ZH_RANDOMINT )
{
   zh_randomint( zh_random_num() );
}

ZH_FUNC( ZH_RANDINT )
{
   zh_randomint( zh_random_num_secure() );
}

ZH_FUNC( ZH_RANDOMSEED )
{
   zh_random_seed( zh_parni( 1 ) );
}

ZH_FUNC( ZH_RANDOMINTMAX )
{
   zh_retnint( MODULUS - 2 );
}
