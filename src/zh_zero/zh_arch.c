/*
 * Architecture dependent conversions
 *
 * Copyright 2005 Przemyslaw Czerpak <druzus@acn.waw.pl>
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
#include "zh_math.h"

/*
 * functions zh_put_ieee754() and zh_get_ieee754() stores / retrieve
 * IEEE754 double value making conversion from/to native C double type.
 * They should be used on platforms which does not use IEEE754 double
 * and user needs binary compatibility, e.g. he wants to share CDXs or
 * or DBFs with "B" fields with other station or use common .hrb files
 * functions zh_put_ord_ieee754() and zh_get_ord_ieee754() converts
 * to/from special modified IEEE754 double form used by some index formats
 * like CDX or NSX to create index keys. In this form double numbers can
 * be sorted as 8-bytes character values (e.g. with memcmp())
 */

#define ZH_MANTISSA_BITS  52
#define ZH_MANTISSA_MASK  ( ( ( ZH_U64 ) 1 << ZH_MANTISSA_BITS ) - 1 )
#define ZH_EXPONENT_BITS  11
#define ZH_EXPONENT_MASK  ( ( 1 << ZH_EXPONENT_BITS ) - 1 )
#define ZH_EXPONENT_ADD   0x3ff

void zh_put_ieee754( ZH_BYTE * ptr, double d )
{
   int iExp, iSig;

#if defined( ZH_LONG_LONG_OFF )
   ZH_U32 l1, l2;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_put_ieee754(%p, %f)", ( void * ) ptr, d ) );

   iSig = d < 0 ? 1 : 0;
   if( d == 0.0 )
   {
      l1 = l2 = 0;
   }
   else
   {
      double df = frexp( iSig ? -d : d, &iExp );
      l1 = ( ZH_U32 ) ldexp( df, ZH_MANTISSA_BITS + 1 );
      l2 = ( ZH_U32 ) ldexp( df, ZH_MANTISSA_BITS + 1 - 32 ) &
                         ( ( ( ZH_U32 ) 1 << ( ZH_MANTISSA_BITS - 32 ) ) - 1 );
      l2 |= ( ZH_U32 ) ( ( iExp + ZH_EXPONENT_ADD - 1 ) & ZH_EXPONENT_MASK ) <<
                       ( ZH_MANTISSA_BITS - 32 );
   }
   l2 |= ( ZH_U32 ) iSig << ( ZH_MANTISSA_BITS + ZH_EXPONENT_BITS - 32 );
   ZH_PUT_LE_UINT32( ptr, l1 );
   ZH_PUT_LE_UINT32( ptr + 4, l2 );
#else
   ZH_U64 ll;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_put_ieee754(%p, %f)", ( void * ) ptr, d ) );

   iSig = d < 0 ? 1 : 0;
   if( d == 0.0 )
   {
      ll = 0;
   }
   else
   {
      double df = frexp( iSig ? -d : d, &iExp );
      ll = ( ZH_U64 ) ldexp( df, ZH_MANTISSA_BITS + 1 ) & ZH_MANTISSA_MASK;
      ll |= ( ZH_U64 ) ( ( iExp + ZH_EXPONENT_ADD - 1 ) & ZH_EXPONENT_MASK ) <<
                       ZH_MANTISSA_BITS;
   }
   ll |= ( ZH_U64 ) iSig << ( ZH_MANTISSA_BITS + ZH_EXPONENT_BITS );
   ZH_PUT_LE_UINT64( ptr, ll );
#endif
}

double zh_get_ieee754( const ZH_BYTE * ptr )
{
   int iExp, iSig;

#if defined( ZH_LONG_LONG_OFF )
   ZH_U32 l1, l2;
   double d;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_get_ieee754(%p)", ( const void * ) ptr ) );

   l1 = ZH_GET_LE_UINT32( ptr );
   l2 = ZH_GET_LE_UINT32( ptr + 4 );
   iSig = ( int ) ( l2 >> ( ZH_MANTISSA_BITS + ZH_EXPONENT_BITS - 32 ) ) & 1;
   iExp = ( int ) ( ( l2 >> ( ZH_MANTISSA_BITS - 32 ) ) & ZH_EXPONENT_MASK );
   l2 &= ( ( ZH_U32 ) 1 << ( ZH_MANTISSA_BITS - 32 ) ) - 1;

   if( ( l1 | l2 | iExp ) != 0 )
      l2 |= ( ZH_U32 ) 1 << ( ZH_MANTISSA_BITS - 32 );

   d = ldexp( ( double ) l2, 32 ) + ( double ) l1;
   return ldexp( iSig ? -d : d, iExp - ZH_MANTISSA_BITS - ZH_EXPONENT_ADD );
#else
   ZH_U64 ll;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_get_ieee754(%p)", ( const void * ) ptr ) );

   ll = ZH_GET_LE_UINT64( ptr );
   iSig = ( int ) ( ll >> ( ZH_MANTISSA_BITS + ZH_EXPONENT_BITS ) ) & 1;
   iExp = ( int ) ( ( ll >> ZH_MANTISSA_BITS ) & ZH_EXPONENT_MASK );
   ll &= ZH_MANTISSA_MASK;
   if( ( ll | iExp ) != 0 )
      ll |= ( ZH_U64 ) 1 << ZH_MANTISSA_BITS;
   /* the casting form ZH_U64 to ZH_I64 is necessary for some
      compilers which does not support ZH_U64 -> double conversion
      It will not change results because there is only up to 53bits
      set in mantissa */
   return ldexp( iSig ? -( double ) ( ZH_I64 ) ll : ( double ) ( ZH_I64 ) ll,
                 iExp - ZH_MANTISSA_BITS - ZH_EXPONENT_ADD );
#endif
}

void zh_put_ord_ieee754( ZH_BYTE * ptr, double d )
{
   int iExp, iSig;
   ZH_U32 l1, l2;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_put_ord_ieee754(%p, %f)", ( void * ) ptr, d ) );

   iSig = d < 0 ? 1 : 0;
   if( d == 0.0 )
   {
      l1 = l2 = 0;
   }
   else
   {
      double df = frexp( iSig ? -d : d, &iExp );
      l1 = ( ZH_U32 ) ldexp( df, ZH_MANTISSA_BITS + 1 );
      l2 = ( ZH_U32 ) ldexp( df, ZH_MANTISSA_BITS + 1 - 32 ) &
                         ( ( ( ZH_U32 ) 1 << ( ZH_MANTISSA_BITS - 32 ) ) - 1 );
      l2 |= ( ZH_U32 ) ( ( iExp + ZH_EXPONENT_ADD - 1 ) & ZH_EXPONENT_MASK ) <<
                       ( ZH_MANTISSA_BITS - 32 );
   }
   if( iSig )
   {
      l2 ^= 0x7FFFFFFFL;
      l1 ^= 0xFFFFFFFFL;
   }
   else
   {
      l2 ^= 0x80000000L;
   }
   ZH_PUT_BE_UINT32( ptr, l2 );
   ZH_PUT_BE_UINT32( ptr + 4, l1 );
}

double zh_get_ord_ieee754( const ZH_BYTE * ptr )
{
   int iExp, iSig;
   ZH_U32 l1, l2;
   double d;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_get_ord_ieee754(%p)", ( const void * ) ptr ) );

   l1 = ZH_GET_BE_UINT32( ptr + 4 );
   l2 = ZH_GET_BE_UINT32( ptr );
   iSig = ( l2 & 0x80000000L ) ? 0 : 1;
   if( iSig )
   {
      l2 ^= 0x7FFFFFFFL;
      l1 ^= 0xFFFFFFFFL;
   }
   iExp = ( ( l2 >> ( ZH_MANTISSA_BITS - 32 ) ) & ZH_EXPONENT_MASK );
   l2 &= ( ( ZH_U32 ) 1 << ( ZH_MANTISSA_BITS - 32 ) ) - 1;

   if( ( l1 | l2 | iExp ) != 0 )
      l2 |= ( ZH_U32 ) 1 << ( ZH_MANTISSA_BITS - 32 );

   d = ldexp( ( double ) l2, 32 ) + ( double ) l1;
   return ldexp( iSig ? -d : d, iExp - ZH_MANTISSA_BITS - ZH_EXPONENT_ADD );
}

/*
 * I added function zh_get_rev_double() and zh_get_std_double() because
 * some compilers does not like construction used by in ZH_GET_LE_DOUBLE()
 * macro => d = { ... }
 */
double zh_get_rev_double( const ZH_BYTE * ptr )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_get_rev_double(%p)", ( const void * ) ptr ) );

   {
#if defined( __GNUC__ )
      return _zh_get_rev_double( ptr );
#else
      union
      {
         double  dbl;
         ZH_BYTE buffer[ 8 ];
      } u;

      u.buffer[ 0 ] = ptr[ 7 ];
      u.buffer[ 1 ] = ptr[ 6 ];
      u.buffer[ 2 ] = ptr[ 5 ];
      u.buffer[ 3 ] = ptr[ 4 ];
      u.buffer[ 4 ] = ptr[ 3 ];
      u.buffer[ 5 ] = ptr[ 2 ];
      u.buffer[ 6 ] = ptr[ 1 ];
      u.buffer[ 7 ] = ptr[ 0 ];

      return u.dbl;
#endif
   }
}

double zh_get_std_double( const ZH_BYTE * ptr )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_get_std_double(%p)", ( const void * ) ptr ) );

   {
#if defined( __GNUC__ )
      return _zh_get_std_double( ptr );
#else
      union
      {
         double  dbl;
         ZH_BYTE buffer[ 8 ];
      } u;

      u.buffer[ 0 ] = ptr[ 0 ];
      u.buffer[ 1 ] = ptr[ 1 ];
      u.buffer[ 2 ] = ptr[ 2 ];
      u.buffer[ 3 ] = ptr[ 3 ];
      u.buffer[ 4 ] = ptr[ 4 ];
      u.buffer[ 5 ] = ptr[ 5 ];
      u.buffer[ 6 ] = ptr[ 6 ];
      u.buffer[ 7 ] = ptr[ 7 ];

      return u.dbl;
#endif
   }
}

#if defined( ZH_LONG_LONG_OFF )

/*
 * The function below are only for platforms which do not support
 * 64 but integer values. So the convert them to/from 'double'
 * values. They are necessary for extracting such number from PCODE,
 * databases or serialization streams in RPC
 */
double zh_get_le_uint64( const ZH_BYTE * ptr )
{
   ZH_U32 l1, l2;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_get_le_uint64(%p)", ( const void * ) ptr ) );

   l1 = ZH_GET_LE_UINT32( ptr );
   l2 = ZH_GET_LE_UINT32( ptr + 4 );
   return ldexp( ( double ) l2, 32 ) + ( double ) l1;
}

double zh_get_le_int64( const ZH_BYTE * ptr )
{
   ZH_U32 l1;
   ZH_I32 l2;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_get_le_int64(%p)", ( const void * ) ptr ) );

   l1 = ZH_GET_LE_UINT32( ptr );
   l2 = ZH_GET_LE_INT32( ptr + 4 );
   return ldexp( ( double ) l2, 32 ) + ( double ) l1;
}

void zh_put_le_uint64( const ZH_BYTE * ptr, double d )
{
   ZH_U32 l1, l2;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_put_le_uint64(%p)", ( const void * ) ptr ) );

   l1 = ( ZH_U32 ) ( d );
   l2 = ( ZH_U32 ) ( d / 4294967296.0 );
   ZH_PUT_LE_UINT32( ptr, l1 );
   ZH_PUT_LE_UINT32( ptr + 4, l2 );
}

#endif
