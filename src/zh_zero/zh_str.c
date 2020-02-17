/*
 * Ziher common string functions (accessed from standalone utilities and the RTL)
 *
 * Copyright 1999-2001 Viktor Szakats
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com> (zh_stricmp())
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

const char * const zh_szAscii[ 256 ] = {
   "\x00", "\x01", "\x02", "\x03", "\x04", "\x05", "\x06", "\x07", "\x08", "\x09", "\x0A", "\x0B", "\x0C", "\x0D", "\x0E", "\x0F",
   "\x10", "\x11", "\x12", "\x13", "\x14", "\x15", "\x16", "\x17", "\x18", "\x19", "\x1A", "\x1B", "\x1C", "\x1D", "\x1E", "\x1F",
   "\x20", "\x21", "\x22", "\x23", "\x24", "\x25", "\x26", "\x27", "\x28", "\x29", "\x2A", "\x2B", "\x2C", "\x2D", "\x2E", "\x2F",
   "\x30", "\x31", "\x32", "\x33", "\x34", "\x35", "\x36", "\x37", "\x38", "\x39", "\x3A", "\x3B", "\x3C", "\x3D", "\x3E", "\x3F",
   "\x40", "\x41", "\x42", "\x43", "\x44", "\x45", "\x46", "\x47", "\x48", "\x49", "\x4A", "\x4B", "\x4C", "\x4D", "\x4E", "\x4F",
   "\x50", "\x51", "\x52", "\x53", "\x54", "\x55", "\x56", "\x57", "\x58", "\x59", "\x5A", "\x5B", "\x5C", "\x5D", "\x5E", "\x5F",
   "\x60", "\x61", "\x62", "\x63", "\x64", "\x65", "\x66", "\x67", "\x68", "\x69", "\x6A", "\x6B", "\x6C", "\x6D", "\x6E", "\x6F",
   "\x70", "\x71", "\x72", "\x73", "\x74", "\x75", "\x76", "\x77", "\x78", "\x79", "\x7A", "\x7B", "\x7C", "\x7D", "\x7E", "\x7F",
   "\x80", "\x81", "\x82", "\x83", "\x84", "\x85", "\x86", "\x87", "\x88", "\x89", "\x8A", "\x8B", "\x8C", "\x8D", "\x8E", "\x8F",
   "\x90", "\x91", "\x92", "\x93", "\x94", "\x95", "\x96", "\x97", "\x98", "\x99", "\x9A", "\x9B", "\x9C", "\x9D", "\x9E", "\x9F",
   "\xA0", "\xA1", "\xA2", "\xA3", "\xA4", "\xA5", "\xA6", "\xA7", "\xA8", "\xA9", "\xAA", "\xAB", "\xAC", "\xAD", "\xAE", "\xAF",
   "\xB0", "\xB1", "\xB2", "\xB3", "\xB4", "\xB5", "\xB6", "\xB7", "\xB8", "\xB9", "\xBA", "\xBB", "\xBC", "\xBD", "\xBE", "\xBF",
   "\xC0", "\xC1", "\xC2", "\xC3", "\xC4", "\xC5", "\xC6", "\xC7", "\xC8", "\xC9", "\xCA", "\xCB", "\xCC", "\xCD", "\xCE", "\xCF",
   "\xD0", "\xD1", "\xD2", "\xD3", "\xD4", "\xD5", "\xD6", "\xD7", "\xD8", "\xD9", "\xDA", "\xDB", "\xDC", "\xDD", "\xDE", "\xDF",
   "\xE0", "\xE1", "\xE2", "\xE3", "\xE4", "\xE5", "\xE6", "\xE7", "\xE8", "\xE9", "\xEA", "\xEB", "\xEC", "\xED", "\xEE", "\xEF",
   "\xF0", "\xF1", "\xF2", "\xF3", "\xF4", "\xF5", "\xF6", "\xF7", "\xF8", "\xF9", "\xFA", "\xFB", "\xFC", "\xFD", "\xFE", "\xFF"
};

ZH_SIZE zh_strAt( const char * szSub, ZH_SIZE nSubLen, const char * szText, ZH_SIZE nLen )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_strAt(%s, %" ZH_PFS "u, %s, %" ZH_PFS "u)", szSub, nSubLen, szText, nLen ) );

   if( nSubLen > 0 && nLen >= nSubLen )
   {
      ZH_SIZE nPos = 0;
      nLen -= nSubLen;
      do
      {
         if( szText[ nPos ] == *szSub )
         {
            ZH_SIZE nSubPos = nSubLen;
            do
            {
               if( --nSubPos == 0 )
                  return nPos + 1;
            }
            while( szText[ nPos + nSubPos ] == szSub[ nSubPos ] );
         }
      }
      while( nPos++ < nLen );
   }

   return 0;
}

ZH_SIZE zh_strAtI( const char * szSub, ZH_SIZE nSubLen, const char * szText, ZH_SIZE nLen )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_strAt(%s, %" ZH_PFS "u, %s, %" ZH_PFS "u)", szSub, nSubLen, szText, nLen ) );

   if( nSubLen > 0 && nLen >= nSubLen )
   {
      ZH_SIZE nPos = 0;
      nLen -= nSubLen;
      do
      {
         if( ZH_TOUPPER( szText[ nPos ] ) == ZH_TOUPPER( *szSub ) )
         {
            ZH_SIZE nSubPos = nSubLen;
            do
            {
               if( --nSubPos == 0 )
                  return nPos + 1;
            }
            while( ZH_TOUPPER( szText[ nPos + nSubPos ] ) == ZH_TOUPPER( szSub[ nSubPos ] ) );
         }
      }
      while( nPos++ < nLen );
   }

   return 0;
}

ZH_BOOL zh_strEmpty( const char * szText, ZH_SIZE nLen )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_strEmpty(%s, %" ZH_PFS "u)", szText, nLen ) );

   while( nLen-- )
   {
      char c = szText[ nLen ];

      if( ! ZH_ISSPACE( c ) )
         return ZH_FALSE;
   }

   return ZH_TRUE;
}

char * zh_strupr( char * pszText )
{
   char * pszPos;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_strupr(%s)", pszText ) );

   for( pszPos = pszText; *pszPos; pszPos++ )
      *pszPos = ( char ) ZH_TOUPPER( ( ZH_UCHAR ) *pszPos );

   return pszText;
}

char * zh_strlow( char * pszText )
{
   char * pszPos;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_strlow(%s)", pszText ) );

   for( pszPos = pszText; *pszPos; pszPos++ )
      *pszPos = ( char ) ZH_TOLOWER( ( ZH_UCHAR ) *pszPos );

   return pszText;
}

char * zh_strdup( const char * pszText )
{
   char * pszDup;
   ZH_SIZE nLen;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_strdup(%s)", pszText ) );

   nLen = strlen( pszText ) + 1;

   pszDup = ( char * ) zh_xgrab( nLen );
   memcpy( pszDup, pszText, nLen );

   return pszDup;
}

char * zh_strndup( const char * pszText, ZH_SIZE nLen )
{
   char * pszDup;
   ZH_SIZE nPos;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_strndup(%.*s, %" ZH_PFS "d)", ( int ) nLen, pszText, nLen ) );

   nPos = 0;
   while( nLen-- && pszText[ nPos ] )
      ++nPos;

   pszDup = ( char * ) zh_xgrab( nPos + 1 );
   memcpy( pszDup, pszText, nPos );
   pszDup[ nPos ] = '\0';

   return pszDup;
}

ZH_SIZE zh_strnlen( const char * pszText, ZH_SIZE nLen )
{
   ZH_SIZE nPos = 0;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_strnlen(%.*s, %" ZH_PFS "d)", ( int ) nLen, pszText, nLen ) );

   while( nLen-- && *pszText++ )
      ++nPos;

   return nPos;
}

char * zh_strduptrim( const char * pszText )
{
   char * pszDup;
   ZH_SIZE nLen;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_strduptrim(%s)", pszText ) );

   while( pszText[ 0 ] == ' ' )
      ++pszText;

   nLen = strlen( pszText );
   while( nLen && pszText[ nLen - 1 ] == ' ' )
      --nLen;

   pszDup = ( char * ) zh_xgrab( nLen + 1 );
   memcpy( pszDup, pszText, nLen );
   pszDup[ nLen ] = '\0';

   return pszDup;
}

ZH_SIZE zh_strlentrim( const char * pszText )
{
   ZH_SIZE nPos = 0;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_strlentrim(%s)", pszText ) );

   while( pszText[ 0 ] == ' ' )
      ++pszText;

   while( pszText[ nPos ] )
      ++nPos;

   while( nPos && pszText[ nPos - 1 ] == ' ' )
      --nPos;

   return nPos;
}

int zh_stricmp( const char * s1, const char * s2 )
{
   int rc = 0, c1;

   //ZH_TRACE( ZH_TR_DEBUG, ( "zh_stricmp(%s, %s)", s1, s2 ) );

   do
   {
      int c2;

      c1 = ZH_TOUPPER( ( unsigned char ) *s1 );
      c2 = ZH_TOUPPER( ( unsigned char ) *s2 );

      if( c1 != c2 )
      {
         rc = ( c1 < c2 ? -1 : 1 );
         break;
      }

      s1++;
      s2++;
   }
   while( c1 );

   return rc;
}

/* Warning: It is not case sensitive */
int zh_strnicmp( const char * s1, const char * s2, ZH_SIZE count )
{
   ZH_SIZE nCount;
   int rc = 0;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_strnicmp(%.*s, %.*s, %" ZH_PFS "u)", ( int ) count, s1, ( int ) count, s2, count ) );

   for( nCount = 0; nCount < count; nCount++ )
   {
      unsigned char c1 = ( char ) ZH_TOUPPER( ( unsigned char ) s1[ nCount ] );
      unsigned char c2 = ( char ) ZH_TOUPPER( ( unsigned char ) s2[ nCount ] );

      if( c1 != c2 )
      {
         rc = ( c1 < c2 ? -1 : 1 );
         break;
      }
      else if( ! c1 )
         break;
   }

   return rc;
}

/*
   AJ: 2004-02-23
   Concatenates multiple strings into a single result.
   Eg. zh_xstrcat( buffer, "A", "B", NULL ) stores "AB" in buffer.
 */
char * zh_xstrcat( char * szDest, const char * szSrc, ... )
{
   char * szResult = szDest;
   va_list va;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xstrcat(%p, %p, ...)", ( void * ) szDest, ( const void * ) szSrc ) );

   while( *szDest )
      szDest++;

   va_start( va, szSrc );
   while( szSrc )
   {
      while( *szSrc )
         *szDest++ = *szSrc++;
      szSrc = va_arg( va, char * );
   }
   *szDest = '\0';
   va_end( va );

   return szResult;
}

/*
   AJ: 2004-02-23
   Concatenates multiple strings into a single result.
   Eg. zh_xstrcpy( buffer, "A", "B", NULL ) stores "AB" in buffer.
   Returns szDest.
   Any existing contents of szDest are cleared. If the szDest buffer is NULL,
   allocates a new buffer with the required length and returns that. The
   buffer is allocated using zh_xgrab(), and should eventually be freed
   using zh_xfree().
 */
char * zh_xstrcpy( char * szDest, const char * szSrc, ... )
{
   char * szResult;
   va_list va;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xstrcpy(%p, %p, ...)", ( void * ) szDest, ( const void * ) szSrc ) );

   if( szDest == NULL )
   {
      const char * szSrcPtr = szSrc;
      ZH_SIZE nSize = 1;
      va_start( va, szSrc );
      while( szSrcPtr )
      {
         nSize += strlen( szSrcPtr );
         szSrcPtr = va_arg( va, char * );
      }
      va_end( va );
      szDest = ( char * ) zh_xgrab( nSize );
   }
   szResult = szDest;

   va_start( va, szSrc );
   while( szSrc )
   {
      while( *szSrc )
         *szDest++ = *szSrc++;
      szSrc = va_arg( va, char * );
   }
   *szDest = '\0';
   va_end( va );

   return szResult;
}

static double zh_numPow10( int nPrecision )
{
   static const double s_dPow10[ 16 ] = { 1.0,                  /* 0 */
                                          10.0,                 /* 1 */
                                          100.0,                /* 2 */
                                          1000.0,               /* 3 */
                                          10000.0,              /* 4 */
                                          100000.0,             /* 5 */
                                          1000000.0,            /* 6 */
                                          10000000.0,           /* 7 */
                                          100000000.0,          /* 8 */
                                          1000000000.0,         /* 9 */
                                          10000000000.0,        /* 10 */
                                          100000000000.0,       /* 11 */
                                          1000000000000.0,      /* 12 */
                                          10000000000000.0,     /* 13 */
                                          100000000000000.0,    /* 14 */
                                          1000000000000000.0 }; /* 15 */

   if( nPrecision < 16 )
   {
      if( nPrecision >= 0 )
         return s_dPow10[ nPrecision ];
      else if( nPrecision > -16 )
         return 1.0 / s_dPow10[ ( unsigned int ) -nPrecision ];
   }

   return pow( 10.0, ( double ) nPrecision );
}

double zh_numRound( double dNum, int iDec )
{
   static const double doBase = 10.0;
   double doComplete5, doComplete5i, dPow;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_numRound(%lf, %d)", dNum, iDec ) );

   if( dNum == 0.0 )
      return 0.0;

   if( iDec < 0 )
   {
      dPow = zh_numPow10( -iDec );
      doComplete5 = dNum / dPow * doBase;
   }
   else
   {
      dPow = zh_numPow10( iDec );
      doComplete5 = dNum * dPow * doBase;
   }

/* #define ZH_NUM_PRECISION  16 */

#ifdef ZH_NUM_PRECISION
   /*
    * this is a hack for people who cannot live without hacked FL values
    * in rounding
    */
   {
      int iDecR, iPrec;
      ZH_BOOL fNeg;

      if( dNum < 0 )
      {
         fNeg = ZH_TRUE;
         dNum = -dNum;
      }
      else
         fNeg = ZH_FALSE;

      iDecR = ( int ) log10( dNum );
      iPrec = iDecR + iDec;

      if( iPrec < -1 )
      {
         return 0.0;
      }
      else
      {
         if( iPrec > ZH_NUM_PRECISION )
         {
            iDec = ZH_NUM_PRECISION - ( dNum < 1.0 ? 0 : 1 ) - iDecR;
            iPrec = -1;
         }
         else
            iPrec -= ZH_NUM_PRECISION;
      }
      if( iDec < 0 )
      {
         dPow = zh_numPow10( -iDec );
         doComplete5 = dNum / dPow * doBase + 5.0 + zh_numPow10( iPrec );
      }
      else
      {
         dPow = zh_numPow10( iDec );
         doComplete5 = dNum * dPow * doBase + 5.0 + zh_numPow10( iPrec );
      }

      if( fNeg )
         doComplete5 = -doComplete5;
   }
#else
   if( dNum < 0.0 )
      doComplete5 -= 5.0;
   else
      doComplete5 += 5.0;
#endif

   doComplete5 /= doBase;

#if defined( ZH_DBLFL_PREC_FACTOR )
   /* similar operation is done by Cl5.3
      it's a hack to force rounding FL values UP */
   doComplete5 *= ZH_DBLFL_PREC_FACTOR;
#endif

   ( void ) modf( doComplete5, &doComplete5i );

   if( iDec < 0 )
      return doComplete5i * dPow;
   else
      return doComplete5i / dPow;
}

double zh_numInt( double dNum )
{
   double dInt;

#if defined( ZH_DBLFL_PREC_FACTOR )
   /* Similar hack as in round to make this functions compatible */
   dNum *= ZH_DBLFL_PREC_FACTOR;
#endif
   ( void ) modf( dNum, &dInt );

   return dInt;
}

double zh_numDecConv( double dNum, int iDec )
{
   if( iDec > 0 )
      return zh_numRound( dNum / zh_numPow10( iDec ), iDec );
   else if( iDec < 0 )
      return zh_numRound( dNum * zh_numPow10( -iDec ), 0 );
   else
      return zh_numRound( dNum, 0 );
}

double zh_numExpConv( double dNum, int iExp )
{
   if( iExp > 0 )
      return dNum / zh_numPow10( iExp );
   else if( iExp < 0 )
      return dNum * zh_numPow10( -iExp );
   else
      return dNum;
}

static ZH_BOOL zh_str2number( ZH_BOOL fPCode, const char * szNum, ZH_SIZE nLen, ZH_MAXINT * lVal, double * dVal, int * piDec, int * piWidth )
{
   ZH_BOOL fDbl = ZH_FALSE, fDec = ZH_FALSE, fNeg, fHex = ZH_FALSE;
   int iLen, iPos = 0;
   int c, iWidth, iDec = 0, iDecR = 0;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_str2number(%d, %p, %" ZH_PFS "u, %p, %p, %p, %p)", ( int ) fPCode, ( const void * ) szNum, nLen, ( void * ) lVal, ( void * ) dVal, ( void * ) piDec, ( void * ) piWidth ) );

   iLen = ( int ) nLen;

   while( iPos < iLen && ZH_ISSPACE( szNum[ iPos ] ) )
      iPos++;

   if( iPos >= iLen )
   {
      fNeg = ZH_FALSE;
   }
   else if( szNum[ iPos ] == '-' )
   {
      fNeg = ZH_TRUE;
      iPos++;
   }
   else
   {
      fNeg = ZH_FALSE;
      if( szNum[ iPos ] == '+' )
         iPos++;
   }

   *dVal = 0;
   *lVal = 0;

   /* Hex Number */
   if( fPCode && iPos + 1 < iLen && szNum[ iPos ] == '0' &&
       ( szNum[ iPos + 1 ] == 'X' || szNum[ iPos + 1 ] == 'x' ) )
   {
      iPos += 2;
      iWidth = ZH_DEFAULT_WIDTH;
      fHex = ZH_TRUE;
      for( ; iPos < iLen; iPos++ )
      {
         c = szNum[ iPos ];
         if( c >= '0' && c <= '9' )
            c -= '0';
         else if( c >= 'A' && c <= 'F' )
            c -= 'A' - 10;
         else if( c >= 'a' && c <= 'f' )
            c -= 'a' - 10;
         else
            break;
         *lVal = ( *lVal << 4 ) + c;
      }
   }
   else
   {
      ZH_MAXINT lLimV;
      int iLimC;

      lLimV = ZH_VMLONG_MAX / 10;
      iLimC = ( int ) ( ZH_VMLONG_MAX % 10 );

      iWidth = iPos;

      for( ; iPos < iLen; iPos++ )
      {
         c = szNum[ iPos ];
         if( c >= '0' && c <= '9' )
         {
            if( fDbl )
            {
               *dVal = *dVal * 10.0 + ( c - '0' );
            }
            else if( *lVal < lLimV || ( *lVal <= lLimV && ( ( int ) ( c - '0' ) ) <= iLimC ) )
            {
               *lVal = *lVal * 10 + ( c - '0' );
            }
            else
            {
               *dVal = ( double ) *lVal * 10.0 + ( c - '0' );
               fDbl = ZH_TRUE;
            }
            if( fDec )
               iDec++;
            else
               iWidth++;
         }
         else if( c == '.' && ! fDec )
         {
            fDec = ZH_TRUE;
         }
         else
         {
            while( ! fDec && iPos < iLen )
            {
               if( szNum[ iPos++ ] == '.' )
                  fDec = ZH_TRUE;
               else
                  iWidth++;
            }
            if( fDec )
               iDecR = iLen - iPos;
            break;
         }
      }
   }

   if( fNeg )
   {
      if( fDbl )
         *dVal = -*dVal;
      else
         *lVal = -*lVal;
   }
   if( ! fDbl && (
#if defined( PCODE_LONG_LIM )
        ( fPCode && ! fHex && ! PCODE_LONG_LIM( *lVal ) ) ||
#endif
        fDec ) )
   {
      *dVal = ( double ) *lVal;
      fDbl = ZH_TRUE;
   }
   if( iDec )
   {
#if defined( __POCC__ )
      if( iDec < 16 )
         *dVal /= ( ZH_LONGLONG ) zh_numPow10( iDec );
      else
#endif
         *dVal /= zh_numPow10( iDec );
   }

   if( piDec )
      *piDec = iDec + iDecR;
   if( piWidth )
   {
      if( fHex )
         *piWidth = iWidth;
      else
      {
         if( fPCode )
         {
            if( iWidth < 10 || fNeg )
               *piWidth = fDbl ? ZH_DBL_LENGTH( *dVal ) : ZH_LONG_LENGTH( *lVal );
            else
               *piWidth = iWidth + ( iDec == 0 ? 1 : 0 );
         }
         else if( iWidth > 10 )
         {
            *piWidth = fDbl ? ZH_DBL_LENGTH( *dVal ) : ZH_LONG_LENGTH( *lVal );
         }
         else
         {
            if( iDec + iDecR == 0 )
               *piWidth = iLen;
            else if( iWidth == 0 )
               *piWidth = 1;
            else if( fNeg && iWidth == 1 && *dVal != 0 )
               *piWidth = 2;
            else
               *piWidth = iWidth;
         }
      }
   }

   return fDbl;
}

ZH_BOOL zh_compStrToNum( const char * szNum, ZH_SIZE nLen, ZH_MAXINT * plVal, double * pdVal, int * piDec, int * piWidth )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_compStrToNum( %s, %" ZH_PFS "u, %p, %p, %p, %p)", szNum, nLen, ( void * ) plVal, ( void * ) pdVal, ( void * ) piDec, ( void * ) piWidth ) );
   return zh_str2number( ZH_TRUE, szNum, nLen, plVal, pdVal, piDec, piWidth );
}

ZH_BOOL zh_valStrnToNum( const char * szNum, ZH_SIZE nLen, ZH_MAXINT * plVal, double * pdVal, int * piDec, int * piWidth )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_valStrnToNum( %s, %" ZH_PFS "u, %p, %p, %p, %p)", szNum, nLen, ( void * ) plVal, ( void * ) pdVal, ( void * ) piDec, ( void * ) piWidth ) );
   return zh_str2number( ZH_FALSE, szNum, nLen, plVal, pdVal, piDec, piWidth );
}

ZH_BOOL zh_strToNum( const char * szNum, ZH_MAXINT * plVal, double * pdVal )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_strToNum(%s, %p, %p)", szNum, ( void * ) plVal, ( void * ) pdVal ) );
   return zh_str2number( ZH_FALSE, szNum, strlen( szNum ), plVal, pdVal, NULL, NULL );
}

ZH_BOOL zh_strnToNum( const char * szNum, ZH_SIZE nLen, ZH_MAXINT * plVal, double * pdVal )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_strnToNum(%.*s, %" ZH_PFS "u, %p, %p)", ( int ) nLen, szNum, nLen, ( void * ) plVal, ( void * ) pdVal ) );
   return zh_str2number( ZH_FALSE, szNum, nLen, plVal, pdVal, NULL, NULL );
}

/* returns the numeric value of a character string representation of a number */
double zh_strVal( const char * szText, ZH_SIZE nLen )
{
   ZH_MAXINT lVal;
   double    dVal;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_strVal(%.*s, %" ZH_PFS "u)", ( int ) nLen, szText, nLen ) );

   if( ! zh_str2number( ZH_FALSE, szText, nLen, &lVal, &dVal, NULL, NULL ) )
      dVal = ( double ) lVal;
   return dVal;
}

ZH_MAXINT zh_strValInt( const char * szText, int * iOverflow )
{
   ZH_MAXINT lVal;
   double    dVal;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_strValInt(%s)", szText ) );

   if( zh_str2number( ZH_TRUE, szText, strlen( szText ), &lVal, &dVal, NULL, NULL ) )
   {
      *iOverflow = 1;
      return 0;
   }
   *iOverflow = 0;
   return lVal;
}

char * zh_numToStr( char * szBuf, ZH_SIZE nSize, ZH_MAXINT lNumber )
{
   int iPos = ( int ) nSize;
   ZH_BOOL fNeg = ZH_FALSE;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_numToStr(%p, %" ZH_PFS "u, %" PFHL "i)", ( void * ) szBuf, nSize, lNumber ) );

   szBuf[ --iPos ] = '\0';
   if( lNumber < 0 )
   {
      fNeg = ZH_TRUE;
      lNumber = -lNumber;
   }

   while( --iPos >= 0 )
   {
      szBuf[ iPos ] = '0' + ( char ) ( lNumber % 10 );
      lNumber /= 10;
      if( lNumber == 0 )
         break;
   }
   if( fNeg && --iPos >= 0 )
      szBuf[ iPos ] = '-';

   if( iPos > 0 )
      memset( szBuf, ' ', iPos );
   else if( iPos < 0 )
   {
      memset( szBuf, '*', nSize - 1 );
      iPos = 0;
   }

   return &szBuf[ iPos ];
}

/* if you want to be sure that size of buffer is enough to hold each
   double number with '\0' terminating character then it should have
   at least ZH_MAX_DOUBLE_LENGTH bytes. If buffer is not large enough
   then NULL is returned */
char * zh_dblToStr( char * szBuf, ZH_SIZE nSize, double dNumber, int iMaxDec )
{
   double dInt, dFract, dDig, doBase = 10.0;
   int iLen, iPos, iPrec;
   char * szResult;
   ZH_BOOL fFirst;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dblToStr(%p, %" ZH_PFS "u, %f, %d)", ( void * ) szBuf, nSize, dNumber, iMaxDec ) );

   iLen = ( int ) ( nSize - 1 );
   if( iLen <= 0 )
      return NULL;
#ifdef ZH_NUM_PRECISION
   iPrec = ZH_NUM_PRECISION;
#else
   iPrec = 16;
#endif
   szResult = szBuf;
   if( dNumber < 0 )
   {
      dFract = modf( -dNumber, &dInt );
      if( --iLen == 0 )
      {
         if( dInt < 1 && dFract < 0.5 )
         {
            szBuf[ 0 ] = '0';
            szBuf[ 1 ] = '\0';
            return szBuf;
         }
         return NULL;
      }
      *szBuf++ = '-';
   }
   else
      dFract = modf( dNumber, &dInt );

   iPos = iLen;
   do
   {
      if( iPos == 0 )
         return NULL;
      dDig = modf( dInt / doBase + 0.01, &dInt ) * doBase;
      szBuf[ --iPos ] = '0' + ( char ) ( dDig + 0.01 );
   }
   while( dInt >= 1 );
   if( iPos > 0 )
      memmove( szBuf, szBuf + iPos, ZH_MIN( iLen - iPos, iPrec + 1 ) );
   iPos = iLen - iPos;

   fFirst = iPos > 1 || szBuf[ 0 ] != '0';
   if( fFirst )
   {
      if( iPos >= iPrec )
      {
         fFirst = iPos == iPrec ? dFract >= 0.5 : szBuf[ iPrec ] >= '5';
         if( iPrec < iPos )
            memset( szBuf + iPrec , '0', iPos - iPrec );
         if( fFirst )
         {
            for( ;; )
            {
               if( --iPrec < 0 )
               {
                  if( iPos == iLen )
                     return NULL;
                  memmove( szBuf + 1, szBuf, iPos );
                  *szBuf = '1';
                  ++iPos;
                  break;
               }
               if( szBuf[ iPrec ] != '9' )
               {
                  ++szBuf[ iPrec ];
                  break;
               }
               szBuf[ iPrec ] = '0';
            }
         }
         iPrec = 0;
      }
      else
         iPrec -= iPos;
   }

   if( iPrec > 0 && iLen - iPos > 1 && iMaxDec != 0 && dFract > 0 )
   {
      int iDec = iPos;

      szBuf[ iPos ] = '.';
      while( ++iPos < iLen && iPrec > 0 && iMaxDec-- != 0 )
      {
         dFract = modf( dFract * doBase, &dDig );
         szBuf[ iPos ] = '0' + ( char ) ( dDig + 0.01 );
         if( szBuf[ iPos ] != '0' )
            fFirst = ZH_TRUE;
         if( fFirst )
            --iPrec;
      }
      if( dFract > ( iPrec > 0 ? 0.5 - zh_numPow10( -iPrec ) : 0.2 ) )
      {
         iPrec = iPos;
         for( ;; )
         {
            if( --iPrec < 0 )
            {
               memmove( szBuf + 1, szBuf, iPos );
               *szBuf = '1';
               if( iPos < iLen )
                  ++iPos;
               ++iDec;
               break;
            }
            if( iPrec == iDec )
               --iPrec;
            if( szBuf[ iPrec ] != '9' )
            {
               ++szBuf[ iPrec ];
               break;
            }
            szBuf[ iPrec ] = '0';
         }
      }
      while( iPos > iDec && szBuf[ iPos - 1 ] == '0' )
         --iPos;
      if( szBuf[ iPos - 1 ] == '.' )
         --iPos;
   }

   szBuf[ iPos ] = '\0';
   return iPos == 1 && *szResult == '-' && *szBuf == '0' ? szBuf : szResult;
}

/* This function copies szText to destination buffer.
 * NOTE: Unlike the documentation for strncpy, this routine will always append
 *       a null and the nLen param is pDest size not pSource limit
 */
char * zh_strncpy( char * pDest, const char * pSource, ZH_SIZE nLen )
{
   char * pBuf = pDest;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_strncpy(%p, %.*s, %" ZH_PFS "u)", ( void * ) pDest, ( int ) nLen, pSource, nLen ) );

   pDest[ nLen ] = '\0';

   while( nLen && ( *pDest++ = *pSource++ ) != '\0' )
      nLen--;

   return pBuf;
}

/* This function copies szText to destination buffer.
 * NOTE: Unlike the documentation for strncat, this routine will always append
 *       a null and the nLen param is pDest size not pSource limit
 */
char * zh_strncat( char * pDest, const char * pSource, ZH_SIZE nLen )
{
   char * pBuf = pDest;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_strncat(%p, %.*s, %" ZH_PFS "u)", ( void * ) pDest, ( int ) nLen, pSource, nLen ) );

   pDest[ nLen ] = '\0';

   while( nLen && *pDest )
   {
      pDest++;
      nLen--;
   }

   while( nLen && ( *pDest++ = *pSource++ ) != '\0' )
      nLen--;

   return pBuf;
}

/* This function copies and converts szText to lower case. */

/* NOTE: Unlike the documentation for strncpy, this routine will always append
 *       a null [pt]
 */
char * zh_strncpyLower( char * pDest, const char * pSource, ZH_SIZE nLen )
{
   char * pBuf = pDest;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_strncpyLower(%p, %.*s, %" ZH_PFS "u)", ( void * ) pDest, ( int ) nLen, pSource, nLen ) );

   pDest[ nLen ] = '\0';

   while( nLen && ( *pDest++ = ( char ) ZH_TOLOWER( ( ZH_UCHAR ) *pSource ) ) != '\0' )
   {
      nLen--;
      pSource++;
   }

   return pBuf;
}

/* This function copies and converts szText to upper case. */

/* NOTE: Unlike the documentation for strncpy, this routine will always append
 *       a null [pt]
 */
char * zh_strncpyUpper( char * pDest, const char * pSource, ZH_SIZE nLen )
{
   char * pBuf = pDest;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_strncpyUpper(%p, %.*s, %" ZH_PFS "u)", ( void * ) pDest, ( int ) nLen, pSource, nLen ) );

   pDest[ nLen ] = '\0';

   while( nLen && ( *pDest++ = ( char ) ZH_TOUPPER( ( ZH_UCHAR ) *pSource ) ) != '\0' )
   {
      nLen--;
      pSource++;
   }

   return pBuf;
}

/* This function copies and converts szText to upper case AND Trims it */

/* NOTE: Unlike the documentation for strncpy, this routine will always append
 *       a null [pt]
 */
char * zh_strncpyUpperTrim( char * pDest, const char * pSource, ZH_SIZE nLen )
{
   char * pBuf = pDest;
   ZH_SIZE nSLen;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_strncpyUpperTrim(%p, %.*s, %" ZH_PFS "u)", ( void * ) pDest, ( int ) nLen, pSource, nLen ) );

   nSLen = 0;
   while( nSLen < nLen && pSource[ nSLen ] )
      nSLen++;

   while( nSLen && pSource[ nSLen - 1 ] == ' ' )
      nSLen--;

   while( nLen && nSLen &&
          ( *pDest++ = ( char ) ZH_TOUPPER( ( ZH_UCHAR ) *pSource ) ) != '\0' )
   {
      nSLen--;
      nLen--;
      pSource++;
   }

   *pDest = '\0';

   return pBuf;
}

/* This function copies trimed szText to destination buffer. */

/* NOTE: Unlike the documentation for strncpy, this routine will always append
 *       a null
 */
char * zh_strncpyTrim( char * pDest, const char * pSource, ZH_SIZE nLen )
{
   char * pBuf = pDest;
   ZH_SIZE nSLen;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_strncpyTrim(%p, %.*s, %" ZH_PFS "u)", ( void * ) pDest, ( int ) nLen, pSource, nLen ) );

   nSLen = 0;
   while( nSLen < nLen && pSource[ nSLen ] )
      nSLen++;

   while( nSLen && pSource[ nSLen - 1 ] == ' ' )
      nSLen--;

   while( nLen && nSLen && ( *pDest++ = *pSource++ ) != '\0' )
   {
      nSLen--;
      nLen--;
   }

   *pDest = '\0';

   return pBuf;
}

char * zh_strRemEscSeq( char * str, ZH_SIZE * pnLen )
{
   ZH_SIZE nPos = *pnLen, nStripped = 0;
   char * ptr, * dst;

   ptr = dst = str;
   while( nPos )
   {
      if( *ptr == '\\' )
         break;
      ++ptr; ++dst;
      --nPos;
   }

   while( nPos-- )
   {
      char ch = *ptr++;
      if( ch == '\\' )
      {
         ++nStripped;
         if( nPos )
         {
            nPos--;
            ch = *ptr++;
            switch( ch )
            {
               case 'r':
                  ch = '\r';
                  break;
               case 'n':
                  ch = '\n';
                  break;
               case 't':
                  ch = '\t';
                  break;
               case 'b':
                  ch = '\b';
                  break;
               case 'f':
                  ch = '\f';
                  break;
               case 'v':
                  ch = '\v';
                  break;
               case 'a':
                  ch = '\a';
                  break;
               case '0':
               case '1':
               case '2':
               case '3':
               case '4':
               case '5':
               case '6':
               case '7':
                  ch -= '0';
                  if( nPos && *ptr >= '0' && *ptr <= '7' )
                  {
                     ch = ( ch << 3 ) | ( *ptr++ - '0' );
                     ++nStripped;
                     if( --nPos && *ptr >= '0' && *ptr <= '7' )
                     {
                        ch = ( ch << 3 ) | ( *ptr++ - '0' );
                        ++nStripped;
                        --nPos;
                     }
                  }
                  break;
               case 'x':
                  ch = 0;
                  while( nPos )
                  {
                     if( *ptr >= '0' && *ptr <= '9' )
                        ch = ( ch << 4 ) | ( *ptr++ - '0' );
                     else if( *ptr >= 'A' && *ptr <= 'F' )
                        ch = ( ch << 4 ) | ( *ptr++ - 'A' + 10 );
                     else if( *ptr >= 'a' && *ptr <= 'f' )
                        ch = ( ch << 4 ) | ( *ptr++ - 'a' + 10 );
                     else
                        break;
                     ++nStripped;
                     --nPos;
                  }
                  break;
               case '\\':
               default:
                  break;
            }
         }
         else
            break;
      }
      *dst++ = ch;
   }

   if( nStripped )
   {
      *dst = '\0';
      *pnLen -= nStripped;
   }

   return str;
}

char * zh_compEncodeString( int iMethod, const char * szText, ZH_SIZE * pnLen )
{
   char * pBuffer = ( char * ) zh_xgrab( *pnLen + 1 );

   memcpy( pBuffer, szText, *pnLen );
   pBuffer[ *pnLen ] = '\0';
   if( iMethod == 1 )
   {
      ZH_SIZE nPos;
      for( nPos = 0; nPos < *pnLen; nPos++ )
         pBuffer[ nPos ] ^= 0xF3;
   }
   return pBuffer;
}

char * zh_compDecodeString( int iMethod, const char * szText, ZH_SIZE * pnLen )
{
   char * pBuffer = ( char * ) zh_xgrab( *pnLen + 1 );

   memcpy( pBuffer, szText, *pnLen );
   pBuffer[ *pnLen ] = '\0';
   if( iMethod == 1 )
   {
      ZH_SIZE nPos;
      for( nPos = 0; nPos < *pnLen; nPos++ )
         pBuffer[ nPos ] ^= 0xF3;
   }
   return pBuffer;
}

/* 'pDest' must be double the size of 'size'. [vszakats] */
void zh_strtohex( const char * pSource, ZH_SIZE size, char * pDest )
{
   ZH_SIZE i;

   for( i = 0; i < size; i++ )
   {
      int b;
      b = ( ( ZH_UCHAR ) pSource[ i ] >> 4 ) & 0x0F;
      *pDest++ = ( char ) ( b + ( b > 9 ? 'a' - 10 : '0' ) );
      b = ( ZH_UCHAR ) pSource[ i ] & 0x0F;
      *pDest++ = ( char ) ( b + ( b > 9 ? 'a' - 10 : '0' ) );
   }
}
