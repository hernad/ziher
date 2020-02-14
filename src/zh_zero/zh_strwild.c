/*
 * Wildcards / file match functions
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
#include "zh_apicdp.h"

#if defined( ZH_OS_UNIX ) && ! defined( ZH_NO_FNMATCH )
#  include <fnmatch.h>
#endif

#define ZH_MAX_WILDPATTERN  256

static ZH_BOOL zh_strMatchWildRaw( const char * szString, const char * szPattern,
                                   ZH_BOOL fExact, ZH_BOOL fCase, ZH_BOOL fFile )
{
   ZH_BOOL fMatch = ZH_TRUE, fAny = ZH_FALSE;
   ZH_SIZE pnBufPosP[ ZH_MAX_WILDPATTERN ], pnBufPosV[ ZH_MAX_WILDPATTERN ],
           nBufSize = ZH_MAX_WILDPATTERN;
   ZH_SIZE * nAnyPosP = pnBufPosP, * nAnyPosV = pnBufPosV,
           nSize, nLen, nAny, nPosP, nPosV;

   nPosP = nPosV = nAny = 0;
   nLen = strlen( szString );
   nSize = strlen( szPattern );
   while( nPosP < nSize || ( fExact && ! fAny && nPosV < nLen ) )
   {
      if( nPosP < nSize && szPattern[ nPosP ] == '*' )
      {
         fAny = ZH_TRUE;
         nPosP++;
      }
      else if( nPosV < nLen && nPosP < nSize &&
               ( szPattern[ nPosP ] == '?' ||
                 ( ! fCase ? szPattern[ nPosP ] == szString[ nPosV ] :
                   ( zh_charUpper( szPattern[ nPosP ] ) ==
                     zh_charUpper( szString[ nPosV ] ) ) ) ) )
      {
         if( fAny )
         {
            if( nAny >= nBufSize )
            {
               if( ( nBufSize <<= 1 ) == ( ZH_MAX_WILDPATTERN << 1 ) )
               {
                  nAnyPosP = ( ZH_SIZE * ) zh_xgrab( nBufSize * sizeof( ZH_SIZE ) );
                  nAnyPosV = ( ZH_SIZE * ) zh_xgrab( nBufSize * sizeof( ZH_SIZE ) );
                  memcpy( nAnyPosP, pnBufPosP, ZH_MAX_WILDPATTERN * sizeof( ZH_SIZE ) );
                  memcpy( nAnyPosV, pnBufPosV, ZH_MAX_WILDPATTERN * sizeof( ZH_SIZE ) );
               }
               else
               {
                  nAnyPosP = ( ZH_SIZE * ) zh_xrealloc( nAnyPosP, nBufSize * sizeof( ZH_SIZE ) );
                  nAnyPosV = ( ZH_SIZE * ) zh_xrealloc( nAnyPosV, nBufSize * sizeof( ZH_SIZE ) );
               }
            }
            nAnyPosP[ nAny ] = nPosP;
            nAnyPosV[ nAny ] = nPosV;
            nAny++;
            fAny = ZH_FALSE;
         }
         nPosV++;
         nPosP++;
      }
      else if( fFile && nPosV == nLen && nPosP < nSize &&
               szPattern[ nPosP ] == '.' &&
               ( nPosP + 1 == nSize ||
                 ( nPosP + 2 == nSize && szPattern[ nPosP + 1 ] == '*' ) ) )
      {
         break;
      }
      else if( fAny && nPosV < nLen )
      {
         nPosV++;
      }
      else if( nAny > 0 )
      {
         nAny--;
         nPosP = nAnyPosP[ nAny ];
         nPosV = nAnyPosV[ nAny ] + 1;
         fAny = ZH_TRUE;
      }
      else
      {
         fMatch = ZH_FALSE;
         break;
      }
   }
   if( nBufSize > ZH_MAX_WILDPATTERN )
   {
      zh_xfree( nAnyPosP );
      zh_xfree( nAnyPosV );
   }
   return fMatch;
}

static ZH_BOOL zh_strMatchWildCDP( const char * szString, const char * szPattern,
                                   ZH_BOOL fExact, ZH_BOOL fCase, ZH_BOOL fFile,
                                   PZH_CODEPAGE cdp )
{
   ZH_BOOL fMatch = ZH_TRUE, fAny = ZH_FALSE;
   ZH_SIZE pnBufPosP[ ZH_MAX_WILDPATTERN ], pnBufPosV[ ZH_MAX_WILDPATTERN ],
           nBufSize = ZH_MAX_WILDPATTERN;
   ZH_SIZE * nAnyPosP = pnBufPosP, * nAnyPosV = pnBufPosV,
           nSize, nLen, nAny, nPosP, nPosV;

   nPosP = nPosV = nAny = 0;
   nLen = strlen( szString );
   nSize = strlen( szPattern );
   while( nPosP < nSize || ( fExact && ! fAny && nPosV < nLen ) )
   {
      if( nPosP < nSize && szPattern[ nPosP ] == '*' )
      {
         fAny = ZH_TRUE;
         nPosP++;
         continue;
      }

      if( nPosV < nLen && nPosP < nSize )
      {
         ZH_SIZE nPP = nPosP, nPV = nPosV;

         if( szPattern[ nPosP ] == '?' )
         {
            nPosP++;
            nPosV += zh_cdpTextPos( cdp, szString + nPosV, nLen - nPosV, 1 );
         }
         else if( fCase )
         {
            if( ! zh_cdpCharCaseEq( cdp, szString, nLen, &nPosV,
                                    szPattern, nSize, &nPosP ) )
            {
               nPosV = nPV;
               nPosP = nPP;
            }
         }
         else
         {
            if( ! zh_cdpCharEq( cdp, szString, nLen, &nPosV,
                                szPattern, nSize, &nPosP ) )
            {
               nPosV = nPV;
               nPosP = nPP;
            }
         }
         if( nPP != nPosP )
         {
            if( fAny )
            {
               if( nAny >= nBufSize )
               {
                  if( ( nBufSize <<= 1 ) == ( ZH_MAX_WILDPATTERN << 1 ) )
                  {
                     nAnyPosP = ( ZH_SIZE * ) zh_xgrab( nBufSize * sizeof( ZH_SIZE ) );
                     nAnyPosV = ( ZH_SIZE * ) zh_xgrab( nBufSize * sizeof( ZH_SIZE ) );
                     memcpy( nAnyPosP, pnBufPosP, ZH_MAX_WILDPATTERN * sizeof( ZH_SIZE ) );
                     memcpy( nAnyPosV, pnBufPosV, ZH_MAX_WILDPATTERN * sizeof( ZH_SIZE ) );
                  }
                  else
                  {
                     nAnyPosP = ( ZH_SIZE * ) zh_xrealloc( nAnyPosP, nBufSize * sizeof( ZH_SIZE ) );
                     nAnyPosV = ( ZH_SIZE * ) zh_xrealloc( nAnyPosV, nBufSize * sizeof( ZH_SIZE ) );
                  }
               }
               nAnyPosP[ nAny ] = nPP;
               nAnyPosV[ nAny ] = nPosV;
               nAny++;
               fAny = ZH_FALSE;
            }
            continue;
         }
      }

      if( fFile && nPosV == nLen && nPosP < nSize &&
          szPattern[ nPosP ] == '.' &&
          ( nPosP + 1 == nSize ||
            ( nPosP + 2 == nSize && szPattern[ nPosP + 1 ] == '*' ) ) )
      {
         break;
      }
      else if( fAny && nPosV < nLen )
      {
         nPosV += zh_cdpTextPos( cdp, szString + nPosV, nLen - nPosV, 1 );
      }
      else if( nAny > 0 )
      {
         nAny--;
         nPosP = nAnyPosP[ nAny ];
         nPosV = nAnyPosV[ nAny ];
         fAny = ZH_TRUE;
      }
      else
      {
         fMatch = ZH_FALSE;
         break;
      }
   }
   if( nBufSize > ZH_MAX_WILDPATTERN )
   {
      zh_xfree( nAnyPosP );
      zh_xfree( nAnyPosV );
   }
   return fMatch;
}

ZH_BOOL zh_strMatchWild( const char * szString, const char * szPattern )
{
   PZH_CODEPAGE cdp = zh_vmCDP();

   if( cdp && ZH_CDP_ISCHARIDX( cdp ) )
      return zh_strMatchWildCDP( szString, szPattern, ZH_FALSE, ZH_FALSE, ZH_FALSE, cdp );
   else
      return zh_strMatchWildRaw( szString, szPattern, ZH_FALSE, ZH_FALSE, ZH_FALSE );
}

ZH_BOOL zh_strMatchWildExact( const char * szString, const char * szPattern )
{
   PZH_CODEPAGE cdp = zh_vmCDP();

   if( cdp && ZH_CDP_ISCHARIDX( cdp ) )
      return zh_strMatchWildCDP( szString, szPattern, ZH_TRUE, ZH_FALSE, ZH_FALSE, cdp );
   else
      return zh_strMatchWildRaw( szString, szPattern, ZH_TRUE, ZH_FALSE, ZH_FALSE );
}

ZH_BOOL zh_strMatchCaseWildExact( const char * szString, const char * szPattern )
{
   PZH_CODEPAGE cdp = zh_vmCDP();

   if( cdp && ZH_CDP_ISCHARIDX( cdp ) )
      return zh_strMatchWildCDP( szString, szPattern, ZH_TRUE, ZH_TRUE, ZH_FALSE, cdp );
   else
      return zh_strMatchWildRaw( szString, szPattern, ZH_TRUE, ZH_TRUE, ZH_FALSE );
}

ZH_BOOL zh_strMatchFile( const char * szString, const char * szPattern )
{
#if defined( ZH_OS_UNIX )
#  if defined( ZH_NO_FNMATCH )
   return zh_strMatchWildExact( szString, szPattern );
#  else
   return fnmatch( szPattern, szString, FNM_PATHNAME ) == 0;
#  endif
#elif defined( ZH_OS_WIN )
   PZH_CODEPAGE cdp = zh_vmCDP();

   if( cdp && ZH_CDP_ISCHARIDX( cdp ) )
      return zh_strMatchWildCDP( szString, szPattern, ZH_TRUE, ZH_TRUE, ZH_TRUE, cdp );
   else
      return zh_strMatchWildRaw( szString, szPattern, ZH_TRUE, ZH_TRUE, ZH_TRUE );
#else
   return zh_strMatchCaseWildExact( szString, szPattern );
#endif
}
