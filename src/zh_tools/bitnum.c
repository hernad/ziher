/*
 * CT3 Number and bit manipulation functions:
 *       NumAnd(), NumOr(), NumXor(), NumNot(), NumHigh(), NumLow()
 *       NumRol(), NumMirr(), ClearBit(), SetBit(), IsBit(),
 *       IntNeg(), IntPos()
 *
 * Copyright 2007 Przemyslaw Czerpak
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
#include "ct.h"

ZH_BOOL ct_numParam( int iParam, ZH_MAXINT * plNum )
{
   const char * szHex = zh_parc( iParam );

   if( szHex )
   {
      *plNum = 0;
      while( *szHex == ' ' )
         szHex++;
      while( *szHex )
      {
         char c = *szHex++;

         if( c >= '0' && c <= '9' )
            c -= '0';
         else if( c >= 'A' && c <= 'F' )
            c -= 'A' - 10;
         else if( c >= 'a' && c <= 'f' )
            c -= 'a' - 10;
         else
            break;
         *plNum = ( *plNum << 4 ) | c;
         iParam = 0;
      }
      if( ! iParam )
         return ZH_TRUE;
   }
   else if( ZH_IS_PARAM_NUM( iParam ) )
   {
      *plNum = zh_parnint( iParam );
      return ZH_TRUE;
   }

   *plNum = -1;
   return ZH_FALSE;
}

ZH_FUNC( NUMAND )
{
   int iPCount = zh_pcount();
   ZH_MAXINT lValue = -1, lNext = 0;

   if( iPCount && ct_numParam( 1, &lValue ) )
   {
      int i = 1;

      while( --iPCount && ct_numParam( ++i, &lNext ) )
         lValue &= lNext;

      if( iPCount )
         lValue = -1;
   }
   zh_retnint( lValue );
}

ZH_FUNC( NUMOR )
{
   int iPCount = zh_pcount();
   ZH_MAXINT lValue = -1, lNext = 0;

   if( iPCount && ct_numParam( 1, &lValue ) )
   {
      int i = 1;

      while( --iPCount && ct_numParam( ++i, &lNext ) )
         lValue |= lNext;

      if( iPCount )
         lValue = -1;
   }
   zh_retnint( lValue );
}

ZH_FUNC( NUMXOR )
{
   int iPCount = zh_pcount();
   ZH_MAXINT lValue = -1, lNext = 0;

   if( iPCount && ct_numParam( 1, &lValue ) )
   {
      int i = 1;

      while( --iPCount && ct_numParam( ++i, &lNext ) )
         lValue ^= lNext;

      if( iPCount )
         lValue = -1;
   }
   zh_retnint( lValue );
}

ZH_FUNC( NUMNOT )
{
   ZH_MAXINT lValue;

   if( ct_numParam( 1, &lValue ) )
      lValue = ( ~lValue ) & 0xffff;

   zh_retnint( lValue );
}

ZH_FUNC( NUMLOW )
{
   ZH_MAXINT lValue;

   if( ct_numParam( 1, &lValue ) )
      lValue &= 0xff;

   zh_retnint( lValue );
}

ZH_FUNC( NUMHIGH )
{
   ZH_MAXINT lValue;

   if( ct_numParam( 1, &lValue ) /* && lValue == lValue & 0xffff */  )
      lValue = ( lValue >> 8 ) & 0xff;

   zh_retnint( lValue );
}

ZH_FUNC( NUMROL )
{
   ZH_MAXINT lValue, lShift;

   if( ct_numParam( 1, &lValue ) && lValue == ( lValue & 0xffff ) && ct_numParam( 2, &lShift )
       && lShift == ( lShift & 0xffff ) )
   {
      if( zh_parl( 3 ) )
      {
         ZH_USHORT us = ( ZH_USHORT ) ( ( lValue & 0xff ) << ( lShift & 0x07 ) );

         lValue = ( lValue & 0xff00 ) | ( us & 0xff ) | ( us >> 8 );
      }
      else
      {
         lValue <<= ( lShift & 0x0f );
         lValue = ( lValue & 0xffff ) | ( lValue >> 16 );
      }
   }
   else
      lValue = -1;

   zh_retnint( lValue );
}

ZH_FUNC( NUMMIRR )
{
   ZH_MAXINT lValue;

   if( ct_numParam( 1, &lValue ) && lValue == ( lValue & 0xffff ) )
   {
      ZH_USHORT usBits = zh_parl( 2 ) ? 8 : 16;
      ZH_USHORT usResult = ( ZH_USHORT ) ( lValue >> usBits );

      do
      {
         usResult <<= 1;
         if( lValue & 1 )
            usResult |= 1;
         lValue >>= 1;
      }
      while( --usBits );

      lValue = usResult;
   }
   else
      lValue = -1;

   zh_retnint( lValue );
}

ZH_FUNC( CLEARBIT )
{
   int iPCount = zh_pcount();
   ZH_MAXINT lValue;

   if( ct_numParam( 1, &lValue ) )
   {
      int i = 1;

      while( --iPCount )
      {
         int iBit = zh_parni( ++i );
         if( iBit < 1 || iBit > 64 )
            break;
         lValue &= ~( ( ( ZH_MAXINT ) 1 ) << ( iBit - 1 ) );
      }

      if( iPCount )
         lValue = -1;
   }

   zh_retnint( lValue );
}

ZH_FUNC( SETBIT )
{
   int iPCount = zh_pcount();
   ZH_MAXINT lValue;

   if( ct_numParam( 1, &lValue ) )
   {
      int i = 1;

      while( --iPCount )
      {
         int iBit = zh_parni( ++i );
         if( iBit < 1 || iBit > 64 )
            break;
         lValue |= ( ( ZH_MAXINT ) 1 ) << ( iBit - 1 );
      }

      if( iPCount )
         lValue = -1;
   }

   zh_retnint( lValue );
}

ZH_FUNC( ISBIT )
{
   ZH_MAXINT lValue;

   if( ct_numParam( 1, &lValue ) )
   {
      int iBit = zh_parni( 2 );

      if( iBit )
         --iBit;
      lValue &= ( ( ZH_MAXINT ) 1 ) << iBit;
   }
   else
      lValue = 0;

   zh_retl( lValue != 0 );
}

ZH_FUNC( INTNEG )
{
   ZH_MAXINT lValue;

   if( ct_numParam( 1, &lValue ) )
   {
      ZH_BOOL f32Bit = zh_parl( 2 );

      if( f32Bit )
         zh_retnint( ( ZH_I32 ) lValue );
      else
         zh_retnint( ( ZH_I16 ) lValue );
   }
   else
      zh_retni( 0 );
}

ZH_FUNC( INTPOS )
{
   ZH_MAXINT lValue;

   if( ct_numParam( 1, &lValue ) )
   {
      ZH_BOOL f32Bit = zh_parl( 2 );

      if( f32Bit )
#ifndef ZH_LONG_LONG_OFF
         zh_retnint( ( ZH_U32 ) lValue );
#else
         zh_retnlen( ( ZH_U32 ) lValue, 0, 0 );
#endif
      else
         zh_retnint( ( ZH_U16 ) lValue );
   }
   else
      zh_retni( 0 );
}
