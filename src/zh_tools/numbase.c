/*
 * CT3 Number and bit manipulation functions:
 *       CToN(), NToC()
 *
 * Copyright 2011 Przemyslaw Czerpak
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

#if ZH_VMLONG_MAX == INT32_MAX
#  define ZH_CT3_STRICT32
#endif

ZH_FUNC( CTON )
{
   const char * szNumber = zh_parc( 1 );
   int iBase = zh_parnidef( 2, 10 );

   if( szNumber && iBase >= 2 && iBase <= 36 )
   {
      ZH_MAXUINT nValue = 0, nMax;
#ifdef ZH_CT3_STRICT32
      nMax = UINT32_MAX;
#else
      ZH_BOOL fStrict = ZH_ISLOGICAL( 3 );
      if( fStrict )
         nMax = UINT32_MAX;
      else
         nMax = UINT64_MAX;
#endif

      for( ;; )
      {
         int iDigit = ( ZH_UCHAR ) *szNumber++;
         if( iDigit >= '0' && iDigit <= '9' )
            iDigit -= '0';
         else if( iDigit >= 'A' && iDigit <= 'Z' )
            iDigit -= 'A' - 10;
         else if( iDigit >= 'a' && iDigit <= 'z' )
            iDigit -= 'a' - 10;
         else
            break;
         if( iDigit >= iBase )
            break;
         if( nValue > ( nMax - iDigit ) / iBase )
         {
            nValue = 0;
            break;
         }
         nValue = nValue * iBase + iDigit;
      }

#ifdef ZH_CT3_STRICT32
      /* test shows that this is exact CT3 behavior */
      if( ( ZH_I32 ) nValue >= 0 || zh_parl( 3 ) )
         zh_retnl( ( ZH_I32 ) nValue );
      else
         zh_retnd( ( ZH_U32 ) nValue );
#else
      if( fStrict )
      {
         if( zh_parl( 3 ) )
            zh_retnint( ( ZH_I32 ) nValue );
         else
            zh_retnint( ( ZH_U32 ) nValue );
      }
      else if( ( ZH_MAXINT ) nValue < 0 )
         zh_retnd( ( double ) nValue );
      else
         zh_retnint( nValue );
#endif
   }
   else
      zh_retni( 0 );
}

ZH_FUNC( NTOC )
{
   char szBuffer[ 256 ], * pszResult = NULL;
   ZH_MAXINT nValue = 0;
   int iBase = zh_parnidef( 2, 10 ), iLen = zh_parni( 3 );

   if( iLen < 0 || iLen > ( int ) sizeof( szBuffer ) )
      iLen = sizeof( szBuffer );

   if( iBase >= 2 && iBase <= 36 && ct_numParam( 1, &nValue ) )
   {
      ZH_MAXUINT uValue = ( ZH_MAXUINT ) nValue;
      int i;

      i = iLen == 0 ? ( int ) sizeof( szBuffer ) : iLen;
      do
      {
         if( --i < 0 )
            break;
         else
         {
            int iDigit = uValue % iBase;
            uValue /= iBase;
            iDigit += iDigit < 10 ? '0' : ( 'A' - 10 );
            szBuffer[ i ] = ( char ) iDigit;
         }
      }
      while( uValue != 0 );

      if( i >= 0 )
      {
         if( iLen == 0 )
            iLen = sizeof( szBuffer ) - i;
         else
         {
            const char * szPad = zh_parc( 4 );
            char cPad = szPad ? szPad[ 0 ] : ( char ) zh_parnidef( 4, ' ' );

            while( i > 0 )
               szBuffer[ --i ] = cPad;
         }
         pszResult = &szBuffer[ i ];
      }
   }
   if( pszResult == NULL )
   {
      if( iLen == 0 )
         iLen = 1;
      memset( szBuffer, '*', iLen );
      pszResult = szBuffer;
   }
   zh_retclen( pszResult, iLen );
}
