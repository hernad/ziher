/*
 * The CodePages API
 *
 * Copyright 2002 Alexander S.Kresin <alex@belacy.belgorod.su>
 * Copyright 2009-2012 Przemyslaw Czerpak
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
#include "zh_error_api.h"
#include "zh_codepage_api.h"
#include "zh_thread.h"


/* MT macros */
#define ZH_CODEPAGE_LOCK()    zh_threadEnterCriticalSection( &s_cdpMtx )
#define ZH_CODEPAGE_UNLOCK()  zh_threadLeaveCriticalSection( &s_cdpMtx )
static ZH_CRITICAL_NEW( s_cdpMtx );


#define NUMBER_OF_CHARS  256

#define ZH_MAX_CTRL_CODE      0x266B

static const ZH_WCHAR s_uniCtrls[ 32 ] =
{
   0x2007, 0x263A, 0x263B, 0x2665, 0x2666, 0x2663, 0x2660, 0x2022,
   0x25D8, 0x25CB, 0x25D9, 0x2642, 0x2640, 0x266A, 0x266B, 0x263C,
   0x25BA, 0x25C4, 0x2195, 0x203C, 0x00B6, 0x00A7, 0x25AC, 0x21A8,
   0x2191, 0x2193, 0x2192, 0x2190, 0x221F, 0x2194, 0x25B2, 0x25BC
};

static ZH_UCHAR * s_rev_ctrl = NULL;

static const ZH_WCHAR s_uniCodes[ NUMBER_OF_CHARS ] =
{
   0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
   0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
   0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
   0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
   0x0020, 0x0021, 0x0022, 0x0023, 0x0024, 0x0025, 0x0026, 0x0027,
   0x0028, 0x0029, 0x002A, 0x002B, 0x002C, 0x002D, 0x002E, 0x002F,
   0x0030, 0x0031, 0x0032, 0x0033, 0x0034, 0x0035, 0x0036, 0x0037,
   0x0038, 0x0039, 0x003A, 0x003B, 0x003C, 0x003D, 0x003E, 0x003F,
   0x0040, 0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0x0047,
   0x0048, 0x0049, 0x004A, 0x004B, 0x004C, 0x004D, 0x004E, 0x004F,
   0x0050, 0x0051, 0x0052, 0x0053, 0x0054, 0x0055, 0x0056, 0x0057,
   0x0058, 0x0059, 0x005A, 0x005B, 0x005C, 0x005D, 0x005E, 0x005F,
   0x0060, 0x0061, 0x0062, 0x0063, 0x0064, 0x0065, 0x0066, 0x0067,
   0x0068, 0x0069, 0x006A, 0x006B, 0x006C, 0x006D, 0x006E, 0x006F,
   0x0070, 0x0071, 0x0072, 0x0073, 0x0074, 0x0075, 0x0076, 0x0077,
   0x0078, 0x0079, 0x007A, 0x007B, 0x007C, 0x007D, 0x007E, 0x007F,
   0x00C7, 0x00FC, 0x00E9, 0x00E2, 0x00E4, 0x00E0, 0x00E5, 0x00E7,
   0x00EA, 0x00EB, 0x00E8, 0x00EF, 0x00EE, 0x00EC, 0x00C4, 0x00C5,
   0x00C9, 0x00E6, 0x00C6, 0x00F4, 0x00F6, 0x00F2, 0x00FB, 0x00F9,
   0x00FF, 0x00D6, 0x00DC, 0x00A2, 0x00A3, 0x00A5, 0x20A7, 0x0192,
   0x00E1, 0x00ED, 0x00F3, 0x00FA, 0x00F1, 0x00D1, 0x00AA, 0x00BA,
   0x00BF, 0x2310, 0x00AC, 0x00BD, 0x00BC, 0x00A1, 0x00AB, 0x00BB,
   0x2591, 0x2592, 0x2593, 0x2502, 0x2524, 0x2561, 0x2562, 0x2556,
   0x2555, 0x2563, 0x2551, 0x2557, 0x255D, 0x255C, 0x255B, 0x2510,
   0x2514, 0x2534, 0x252C, 0x251C, 0x2500, 0x253C, 0x255E, 0x255F,
   0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C, 0x2567,
   0x2568, 0x2564, 0x2565, 0x2559, 0x2558, 0x2552, 0x2553, 0x256B,
   0x256A, 0x2518, 0x250C, 0x2588, 0x2584, 0x258C, 0x2590, 0x2580,
   0x03B1, 0x00DF, 0x0393, 0x03C0, 0x03A3, 0x03C3, 0x00B5, 0x03C4,
   0x03A6, 0x0398, 0x03A9, 0x03B4, 0x221E, 0x03C6, 0x03B5, 0x2229,
   0x2261, 0x00B1, 0x2265, 0x2264, 0x2320, 0x2321, 0x00F7, 0x2248,
   0x00B0, 0x2219, 0x00B7, 0x221A, 0x207F, 0x00B2, 0x25A0, 0x00A0
};

ZH_UNITABLE zh_uniTbl_437 = { ZH_CPID_437, s_uniCodes, NULL, 0 };

static ZH_CODEPAGE_GET_FUNC( zh_cdpStd_get );
static ZH_CODEPAGE_PUT_FUNC( zh_cdpStd_put );
static ZH_CODEPAGE_LEN_FUNC( zh_cdpStd_len );

static ZH_CODEPAGE_CMP_FUNC( zh_cdpBin_cmp );
static ZH_CODEPAGE_CMP_FUNC( zh_cdpBin_cmpi );

static ZH_CODEPAGE_GET_FUNC( zh_cdpUTF8_get );
static ZH_CODEPAGE_PUT_FUNC( zh_cdpUTF8_put );
static ZH_CODEPAGE_LEN_FUNC( zh_cdpUTF8_len );

static ZH_UNITABLE zh_uniTbl_UTF8 = { ZH_CPID_437, s_uniCodes, NULL, 0 };

static ZH_UCHAR s_en_buffer[ 0x300 ];

/* pseudo codepage for translations only */
static ZH_CODEPAGE s_utf8_codepage =
   { "UTF8", "UTF-8", &zh_uniTbl_UTF8,
     NULL, NULL, NULL, NULL, NULL, 0,
     ZH_CODEPAGE_TYPE_CUSTOM | ZH_CODEPAGE_TYPE_UTF8 | ZH_CODEPAGE_TYPE_BINSORT,
     zh_cdpUTF8_get, zh_cdpUTF8_put, zh_cdpUTF8_len,
     NULL, NULL, NULL, zh_cdpBin_cmp, zh_cdpBin_cmpi,
     0, 0, NULL, NULL, NULL };

ZH_CODEPAGE_ANNOUNCE( UTF8 )

static ZH_CODEPAGE s_en_codepage =
   { "EN", "English CP-437", ZH_UNITB_437,
     NULL, NULL, NULL, NULL, NULL, 0,
     ZH_CODEPAGE_TYPE_BINSORT,
     zh_cdpStd_get, zh_cdpStd_put, zh_cdpStd_len,
     NULL, NULL, NULL, zh_cdpBin_cmp, zh_cdpBin_cmpi,
     0, 0, NULL, NULL, &s_utf8_codepage };

ZH_CODEPAGE_ANNOUNCE( EN )

static PZH_CODEPAGE s_cdpList = NULL;


/*
 * conversions
 */
void zh_cdpBuildTransTable( PZH_UNITABLE uniTable )
{
   ZH_CODEPAGE_LOCK();
   if( uniTable->uniTrans == NULL )
   {
      ZH_UCHAR * uniTrans;
      ZH_WCHAR wcMax = 0;
      int i;

      for( i = 0; i < 256; ++i )
      {
         ZH_WCHAR wc = uniTable->uniCodes[ i ];
         if( wc > wcMax )
            wcMax = wc;
      }
      uniTrans = ( ZH_UCHAR * ) zh_xgrabz( ( wcMax + 1 ) * sizeof( ZH_UCHAR ) );
      for( i = 0; i < 256; ++i )
      {
         if( uniTable->uniCodes[ i ] )
            uniTrans[ uniTable->uniCodes[ i ] ] = ( ZH_UCHAR ) i;
      }

      uniTable->wcMax = wcMax;
      uniTable->uniTrans = uniTrans;

      if( s_rev_ctrl == NULL )
      {
         wcMax = ZH_MAX_CTRL_CODE;
         s_rev_ctrl = ( ZH_UCHAR * ) zh_xgrabz( ( wcMax + 1 ) * sizeof( ZH_UCHAR ) );
         for( i = 0; i < 32; ++i )
            s_rev_ctrl[ s_uniCtrls[ i ] ] = ( ZH_UCHAR ) i;
      }
   }
   ZH_CODEPAGE_UNLOCK();
}

/*
 * standard conversion functions
 */
static ZH_BOOL zh_cdpStd_get( PZH_CODEPAGE cdp,
                              const char * pSrc, ZH_SIZE nLen,
                              ZH_SIZE * pnIndex, ZH_WCHAR * wc )
{
   if( *pnIndex < nLen )
   {
      ZH_UCHAR uc = ( ZH_UCHAR ) pSrc[ ( *pnIndex )++ ];

      *wc = cdp->uniTable->uniCodes[ uc ];
      if( *wc == 0 )
         *wc = uc;

      return ZH_TRUE;
   }
   return ZH_FALSE;
}

static ZH_BOOL zh_cdpStd_put( PZH_CODEPAGE cdp,
                              char * pDst, ZH_SIZE nLen,
                              ZH_SIZE * pnIndex, ZH_WCHAR wc )
{
   if( *pnIndex < nLen )
   {
      if( cdp->uniTable->uniTrans == NULL )
         zh_cdpBuildTransTable( cdp->uniTable );

      if( wc <= cdp->uniTable->wcMax &&
          cdp->uniTable->uniTrans[ wc ] )
         pDst[ ( *pnIndex )++ ] = cdp->uniTable->uniTrans[ wc ];
      else
         pDst[ ( *pnIndex )++ ] = wc >= 0x100 ? '?' : ( ZH_UCHAR ) wc;

      return ZH_TRUE;
   }
   return ZH_FALSE;
}

static int zh_cdpStd_len( PZH_CODEPAGE cdp, ZH_WCHAR wc )
{
   ZH_SYMBOL_UNUSED( cdp );
   ZH_SYMBOL_UNUSED( wc );
   return 1;
}

static int zh_cdpBin_cmp( PZH_CODEPAGE cdp,
                          const char * szFirst, ZH_SIZE nLenFirst,
                          const char * szSecond, ZH_SIZE nLenSecond,
                          ZH_BOOL fExact )
{
   ZH_SIZE nLen = nLenFirst < nLenSecond ? nLenFirst : nLenSecond;
   int iRet = memcmp( szFirst, szSecond, nLen );

   ZH_SYMBOL_UNUSED( cdp );

   if( iRet == 0 )
   {
      if( nLenSecond > nLenFirst )
         iRet = -1;
      else if( fExact && nLenSecond < nLenFirst )
         iRet = 1;
   }
   else if( iRet > 0 )
      iRet = 1;
   else
      iRet = -1;

   return iRet;
}

static int zh_cdpBin_cmpi( PZH_CODEPAGE cdp,
                           const char * szFirst, ZH_SIZE nLenFirst,
                           const char * szSecond, ZH_SIZE nLenSecond,
                           ZH_BOOL fExact )
{
   ZH_SIZE nLen = nLenFirst < nLenSecond ? nLenFirst : nLenSecond;
   int iRet = 0;

   while( nLen-- )
   {
      ZH_UCHAR u1 = cdp->upper[ ( ZH_UCHAR ) *szFirst++ ],
               u2 = cdp->upper[ ( ZH_UCHAR ) *szSecond++ ];
      if( u1 != u2 )
      {
         iRet = ( u1 < u2 ) ? -1 : 1;
         break;
      }
   }

   if( iRet == 0 )
   {
      if( nLenSecond > nLenFirst )
         iRet = -1;
      else if( fExact && nLenSecond < nLenFirst )
         iRet = 1;
   }

   return iRet;
}

static int zh_cdpStd_cmp( PZH_CODEPAGE cdp,
                          const char * szFirst, ZH_SIZE nLenFirst,
                          const char * szSecond, ZH_SIZE nLenSecond,
                          ZH_BOOL fExact )
{
   int iRet = 0, iAcc = 0, n1, n2;
   ZH_SIZE nPos, nLen;

   nLen = nLenFirst < nLenSecond ? nLenFirst : nLenSecond;
   for( nPos = 0; nPos < nLen; ++szFirst, ++szSecond, ++nPos )
   {
      if( *szFirst != *szSecond )
      {
         n1 = ( ZH_UCHAR ) cdp->sort[ ( ZH_UCHAR ) *szFirst ];
         n2 = ( ZH_UCHAR ) cdp->sort[ ( ZH_UCHAR ) *szSecond ];
         if( n1 != n2 )
         {
            iRet = ( n1 < n2 ) ? -1 : 1;
            break;
         }
         if( iAcc == 0 && ( fExact || ( nLenFirst == nLenSecond && cdp->acc ) ) )
         {
            if( cdp->acc )
               iAcc = ( cdp->acc[ ( ZH_UCHAR ) *szFirst ] <
                        cdp->acc[ ( ZH_UCHAR ) *szSecond ] ) ? -1 : 1;
            else
               iAcc = ( ( ZH_UCHAR ) *szFirst < ( ZH_UCHAR ) *szSecond ) ? -1 : 1;
         }
      }
   }

   if( ! iRet )
   {
      if( iAcc )
         iRet = iAcc;
      else if( nLenSecond > nLenFirst )
         iRet = -1;
      else if( fExact && nLenSecond < nLenFirst )
         iRet = 1;
   }

   return iRet;
}

static int zh_cdpStd_cmpi( PZH_CODEPAGE cdp,
                           const char * szFirst, ZH_SIZE nLenFirst,
                           const char * szSecond, ZH_SIZE nLenSecond,
                           ZH_BOOL fExact )
{
   int iRet = 0, iAcc = 0;
   ZH_SIZE nPos, nLen;

   nLen = nLenFirst < nLenSecond ? nLenFirst : nLenSecond;
   for( nPos = 0; nPos < nLen; ++szFirst, ++szSecond, ++nPos )
   {
      int u1 = cdp->upper[ ( ZH_UCHAR ) *szFirst ];
      int u2 = cdp->upper[ ( ZH_UCHAR ) *szSecond ];

      if( u1 != u2 )
      {
         int n1 = ( ZH_UCHAR ) cdp->sort[ u1 ];
         int n2 = ( ZH_UCHAR ) cdp->sort[ u2 ];
         if( n1 != n2 )
         {
            iRet = ( n1 < n2 ) ? -1 : 1;
            break;
         }
         if( iAcc == 0 && ( fExact || ( nLenFirst == nLenSecond && cdp->acc ) ) )
         {
            if( cdp->acc )
               iAcc = ( cdp->acc[ u1 ] < cdp->acc[ u2 ] ) ? -1 : 1;
            else
               iAcc = ( u1 < u2 ) ? -1 : 1;
         }
      }
   }

   if( ! iRet )
   {
      if( iAcc )
         iRet = iAcc;
      else if( nLenSecond > nLenFirst )
         iRet = -1;
      else if( fExact && nLenSecond < nLenFirst )
         iRet = 1;
   }

   return iRet;
}


static ZH_BOOL zh_cdpUTF8_get( PZH_CODEPAGE cdp,
                               const char * pSrc, ZH_SIZE nLen,
                               ZH_SIZE * pnIndex, ZH_WCHAR * wc )
{
   ZH_SIZE nIndex = *pnIndex;
   int n = 0;

   ZH_SYMBOL_UNUSED( cdp );

   *wc = 0;
   while( nIndex < nLen )
   {
      if( zh_cdpUTF8ToU16NextChar( ( ZH_UCHAR ) pSrc[ nIndex ], &n, wc ) )
         ++nIndex;
      if( n == 0 )
      {
         *pnIndex = nIndex;
         return ZH_TRUE;
      }
   }
   if( n > 0 )
   {
      *pnIndex = nIndex;
      return ZH_TRUE;
   }
   return ZH_FALSE;
}

static ZH_BOOL zh_cdpUTF8_put( PZH_CODEPAGE cdp,
                               char * pDst, ZH_SIZE nLen,
                               ZH_SIZE * pnIndex, ZH_WCHAR wc )
{
   int i = zh_cdpUTF8CharSize( wc );

   ZH_SYMBOL_UNUSED( cdp );

   if( *pnIndex + i <= nLen )
   {
      zh_cdpU16CharToUTF8( &pDst[ *pnIndex ], wc );
      *pnIndex += i;
      return ZH_TRUE;
   }
   return ZH_FALSE;
}

static int zh_cdpUTF8_len( PZH_CODEPAGE cdp, ZH_WCHAR wc )
{
   ZH_SYMBOL_UNUSED( cdp );

   return zh_cdpUTF8CharSize( wc );
}

static ZH_BOOL zh_cdpMulti_get( PZH_CODEPAGE cdp,
                                const char * pSrc, ZH_SIZE nLen,
                                ZH_SIZE * pnIndex, ZH_WCHAR * wc )
{
   if( *pnIndex < nLen )
   {
      ZH_UCHAR uc = ( ZH_UCHAR ) pSrc[ ( *pnIndex )++ ];

      *wc = cdp->uniTable->uniCodes[ uc ];
      if( *wc == 0 )
         *wc = uc;
      else if( ( cdp->flags[ uc ] & ZH_CODEPAGE_MULTI1 ) != 0 &&
          *pnIndex < nLen )
      {
         ZH_UCHAR uc2 = ( ZH_UCHAR ) pSrc[ *pnIndex + 1 ];
         if( ( cdp->flags[ uc2 ] & ZH_CODEPAGE_MULTI2 ) != 0 )
         {
            int i;
            for( i = 0; i < cdp->nMulti; ++i )
            {
               if( uc2 == cdp->multi[ i ].cLast[ 0 ] ||
                   uc2 == cdp->multi[ i ].cLast[ 1 ] )
               {
                  if( uc == cdp->multi[ i ].cFirst[ 0 ] )
                  {
                     if( cdp->multi[ i ].wcUp )
                     {
                        *wc = cdp->multi[ i ].wcUp;
                        ++( *pnIndex );
                     }
                     break;
                  }
                  else if( uc == cdp->multi[ i ].cFirst[ 1 ] )
                  {
                     if( cdp->multi[ i ].wcLo )
                     {
                        *wc = cdp->multi[ i ].wcLo;
                        ++( *pnIndex );
                     }
                     break;
                  }
               }
            }
         }
      }
      return ZH_TRUE;
   }
   return ZH_FALSE;
}

static ZH_BOOL zh_cdpMulti_put( PZH_CODEPAGE cdp,
                                char * pDst, ZH_SIZE nLen,
                                ZH_SIZE * pnIndex, ZH_WCHAR wc )
{
   if( *pnIndex < nLen )
   {
      if( cdp->uniTable->uniTrans == NULL )
         zh_cdpBuildTransTable( cdp->uniTable );

      if( wc <= cdp->uniTable->wcMax && cdp->uniTable->uniTrans[ wc ] )
         pDst[ ( *pnIndex )++ ] = cdp->uniTable->uniTrans[ wc ];
      else if( wc == 0 )
         pDst[ ( *pnIndex )++ ] = 0;
      else
      {
         int i;
         for( i = 0; i < cdp->nMulti; ++i )
         {
            if( wc == cdp->multi[ i ].wcUp )
            {
               pDst[ ( *pnIndex )++ ] = cdp->multi[ i ].cFirst[ 0 ];
               if( *pnIndex < nLen )
                  pDst[ ( *pnIndex )++ ] = cdp->multi[ i ].cLast[ 0 ];
               return ZH_TRUE;
            }
            if( wc == cdp->multi[ i ].wcLo )
            {
               pDst[ ( *pnIndex )++ ] = cdp->multi[ i ].cFirst[ 1 ];
               if( *pnIndex < nLen )
                  pDst[ ( *pnIndex )++ ] = cdp->multi[ i ].cLast[ 1 ];
               return ZH_TRUE;
            }
         }
         pDst[ ( *pnIndex )++ ] = wc >= 0x100 ? '?' : ( ZH_UCHAR ) wc;
      }
      return ZH_TRUE;
   }
   return ZH_FALSE;
}

static int zh_cdpMulti_len( PZH_CODEPAGE cdp, ZH_WCHAR wc )
{
   int n = 1;

   if( wc )
   {
      int i;
      for( i = 0; i < cdp->nMulti; ++i )
      {
         if( wc == cdp->multi[ i ].wcUp ||
             wc == cdp->multi[ i ].wcLo )
         {
            ++n;
            break;
         }
      }
   }
   return n;
}

static int zh_cdpMulti_weight( PZH_CODEPAGE cdp, const char * szChar )
{
   PZH_MULTICHAR pmulti = cdp->multi;
   int i;

   for( i = cdp->nMulti; i; --i, ++pmulti )
   {
      if( ( szChar[ 0 ] == pmulti->cFirst[ 0 ] ||
            szChar[ 0 ] == pmulti->cFirst[ 1 ] ) &&
          ( szChar[ 1 ] == pmulti->cLast[ 0 ] ||
            szChar[ 1 ] == pmulti->cLast[ 1 ] ) )
      {
         return ( szChar[ 0 ] == pmulti->cFirst[ 0 ] ) ?
                pmulti->sortUp : pmulti->sortLo;
      }
   }

   return 0;
}

static int zh_cdpMulti_cmp( PZH_CODEPAGE cdp,
                            const char * szFirst, ZH_SIZE nLenFirst,
                            const char * szSecond, ZH_SIZE nLenSecond,
                            ZH_BOOL fExact )
{
   int iRet = 0, iAcc = 0, n;
   ZH_SIZE nPos, nLen;

   nLen = nLenFirst < nLenSecond ? nLenFirst : nLenSecond;
   for( nPos = 0; nPos < nLen; ++szFirst, ++szSecond, ++nPos )
   {
      int n1, n2;

      ZH_UCHAR u1 = ( ZH_UCHAR ) *szFirst;
      ZH_UCHAR u2 = ( ZH_UCHAR ) *szSecond;

      n1 = cdp->sort[ u1 ];
      if( ( cdp->flags[ u1 ] & ZH_CODEPAGE_MULTI1 ) != 0 &&
          ( nPos < nLenFirst - 1 ) &&
          ( cdp->flags[ ( ZH_UCHAR ) szFirst[ 1 ] ] & ZH_CODEPAGE_MULTI2 ) != 0 )
      {
         n = zh_cdpMulti_weight( cdp, szFirst );
         if( n != 0 )
         {
            n1 = n;
            ++szFirst;
            if( --nLenFirst < nLen )
               nLen = nLenFirst;
         }
      }
      n2 = cdp->sort[ u2 ];
      if( ( cdp->flags[ u2 ] & ZH_CODEPAGE_MULTI1 ) != 0 &&
          ( nPos < nLenSecond - 1 ) &&
          ( cdp->flags[ ( ZH_UCHAR ) szSecond[ 1 ] ] & ZH_CODEPAGE_MULTI2 ) != 0 )
      {
         n = zh_cdpMulti_weight( cdp, szSecond );
         if( n != 0 )
         {
            n2 = n;
            ++szSecond;
            if( --nLenSecond < nLen )
               nLen = nLenSecond;
         }
      }
      if( n1 != n2 )
      {
         if( n1 == 0 || n2 == 0 )
            /* One of characters doesn't belong to the national characters */
            iRet = ( u1 < u2 ) ? -1 : 1;
         else
            iRet = ( n1 < n2 ) ? -1 : 1;
         break;
      }
      else if( u1 != u2 )
      {
         if( n1 == 0 )
         {
            iRet = ( u1 < u2 ) ? -1 : 1;
            break;
         }
         if( iAcc == 0 && ( fExact || ( nLenFirst == nLenSecond && cdp->acc ) ) )
         {
            if( cdp->acc )
               iAcc = ( cdp->acc[ u1 ] < cdp->acc[ u2 ] ) ? -1 : 1;
            else
               iAcc = ( u1 < u2 ) ? -1 : 1;
         }
      }
   }

   if( ! iRet )
   {
      if( iAcc )
         iRet = iAcc;
      else if( nLenSecond > nLenFirst )
         iRet = -1;
      else if( fExact && nLenSecond < nLenFirst )
         iRet = 1;
   }

   return iRet;
}

static int zh_cdpMulti_weightI( PZH_CODEPAGE cdp, const char * szChar )
{
   PZH_MULTICHAR pmulti = cdp->multi;
   int i;

   for( i = cdp->nMulti; i; --i, ++pmulti )
   {
      if( ( szChar[ 0 ] == pmulti->cFirst[ 0 ] ||
            szChar[ 0 ] == pmulti->cFirst[ 1 ] ) &&
          ( szChar[ 1 ] == pmulti->cLast[ 0 ] ||
            szChar[ 1 ] == pmulti->cLast[ 1 ] ) )
      {
         return pmulti->sortUp;
      }
   }

   return 0;
}

static int zh_cdpMulti_cmpi( PZH_CODEPAGE cdp,
                             const char * szFirst, ZH_SIZE nLenFirst,
                             const char * szSecond, ZH_SIZE nLenSecond,
                             ZH_BOOL fExact )
{
   int iRet = 0, iAcc = 0;
   ZH_SIZE nPos, nLen;

   nLen = nLenFirst < nLenSecond ? nLenFirst : nLenSecond;
   for( nPos = 0; nPos < nLen; ++szFirst, ++szSecond, ++nPos )
   {
      int n, u1, u2, n1, n2;

      u1 = cdp->upper[ ( ZH_UCHAR ) *szFirst ];
      u2 = cdp->upper[ ( ZH_UCHAR ) *szSecond ];

      if( ( cdp->flags[ u1 ] & ZH_CODEPAGE_MULTI1 ) != 0 &&
          ( nPos < nLenFirst - 1 ) &&
          ( cdp->flags[ ( ZH_UCHAR ) szFirst[ 1 ] ] & ZH_CODEPAGE_MULTI2 ) != 0 )
      {
         n = zh_cdpMulti_weightI( cdp, szFirst );
         if( n != 0 )
         {
            n1 = n;
            ++szFirst;
            if( --nLenFirst < nLen )
               nLen = nLenFirst;
         }
         else
            n1 = cdp->sort[ u1 ];
      }
      else
         n1 = cdp->sort[ u1 ];

      if( ( cdp->flags[ u2 ] & ZH_CODEPAGE_MULTI1 ) != 0 &&
          ( nPos < nLenSecond - 1 ) &&
          ( cdp->flags[ ( ZH_UCHAR ) szSecond[ 1 ] ] & ZH_CODEPAGE_MULTI2 ) != 0 )
      {
         n = zh_cdpMulti_weightI( cdp, szSecond );
         if( n != 0 )
         {
            n2 = n;
            ++szSecond;
            if( --nLenSecond < nLen )
               nLen = nLenSecond;
         }
         else
            n2 = cdp->sort[ u2 ];
      }
      else
         n2 = cdp->sort[ u2 ];

      if( n1 != n2 )
      {
         if( n1 == 0 || n2 == 0 )
            /* One of characters doesn't belong to the national characters */
            iRet = ( u1 < u2 ) ? -1 : 1;
         else
            iRet = ( n1 < n2 ) ? -1 : 1;
         break;
      }
      else if( u1 != u2 )
      {
         if( n1 == 0 )
         {
            iRet = ( u1 < u2 ) ? -1 : 1;
            break;
         }
         if( iAcc == 0 && ( fExact || ( nLenFirst == nLenSecond && cdp->acc ) ) )
         {
            if( cdp->acc )
               iAcc = ( cdp->acc[ u1 ] < cdp->acc[ u2 ] ) ? -1 : 1;
            else
               iAcc = ( u1 < u2 ) ? -1 : 1;
         }
      }
   }

   if( ! iRet )
   {
      if( iAcc )
         iRet = iAcc;
      else if( nLenSecond > nLenFirst )
         iRet = -1;
      else if( fExact && nLenSecond < nLenFirst )
         iRet = 1;
   }

   return iRet;
}


/* Warning: this functions works only with byte oriented CPs */
ZH_BOOL zh_cdpIsDigit( PZH_CODEPAGE cdp, int iChar )
{
   if( cdp )
      return ( cdp->flags[ iChar & 0x0ff ] & ZH_CODEPAGE_DIGIT ) != 0;
   else
      return ZH_ISDIGIT( iChar );
}

ZH_BOOL zh_cdpIsAlpha( PZH_CODEPAGE cdp, int iChar )
{
   if( cdp )
      return ( cdp->flags[ iChar & 0x0ff ] & ZH_CODEPAGE_ALPHA ) != 0;
   else
      return ZH_ISALPHA( iChar );
}

ZH_BOOL zh_cdpIsLower( PZH_CODEPAGE cdp, int iChar )
{
   if( cdp )
      return ( cdp->flags[ iChar & 0x0ff ] & ZH_CODEPAGE_LOWER ) != 0;
   else
      return ZH_ISLOWER( iChar );
}

ZH_BOOL zh_cdpIsUpper( PZH_CODEPAGE cdp, int iChar )
{
   if( cdp )
      return ( cdp->flags[ iChar & 0x0ff ] & ZH_CODEPAGE_UPPER ) != 0;
   else
      return ZH_ISUPPER( iChar );
}

ZH_BOOL zh_charIsDigit( int iChar )
{
   PZH_CODEPAGE cdp = zh_vmCodepage();

   if( cdp )
      return ( cdp->flags[ iChar & 0x0ff ] & ZH_CODEPAGE_DIGIT ) != 0;
   else
      return ZH_ISDIGIT( iChar );
}

ZH_BOOL zh_charIsAlpha( int iChar )
{
   PZH_CODEPAGE cdp = zh_vmCodepage();

   if( cdp )
      return ( cdp->flags[ iChar & 0x0ff ] & ZH_CODEPAGE_ALPHA ) != 0;
   else
      return ZH_ISALPHA( iChar );
}

ZH_BOOL zh_charIsLower( int iChar )
{
   PZH_CODEPAGE cdp = zh_vmCodepage();

   if( cdp )
      return ( cdp->flags[ iChar & 0x0ff ] & ZH_CODEPAGE_LOWER ) != 0;
   else
      return ZH_ISLOWER( iChar );
}

ZH_BOOL zh_charIsUpper( int iChar )
{
   PZH_CODEPAGE cdp = zh_vmCodepage();

   if( cdp )
      return ( cdp->flags[ iChar & 0x0ff ] & ZH_CODEPAGE_UPPER ) != 0;
   else
      return ZH_ISUPPER( iChar );
}

int zh_charLower( int iChar )
{
   PZH_CODEPAGE cdp = zh_vmCodepage();

   if( cdp )
      return cdp->lower[ iChar & 0x0ff ];
   else
      return ZH_TOLOWER( iChar );
}

int zh_charUpper( int iChar )
{
   PZH_CODEPAGE cdp = zh_vmCodepage();

   if( cdp )
      return cdp->upper[ iChar & 0x0ff ];
   else
      return ZH_TOUPPER( iChar );
}

char * zh_strLower( char * szText, ZH_SIZE nLen )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_strLower(%s, %" ZH_PFS "u)", szText, nLen ) );

   {
      PZH_CODEPAGE cdp = zh_vmCodepage();
      ZH_SIZE u;

      if( cdp )
         for( u = 0; u < nLen; u++ )
            szText[ u ] = ( char ) cdp->lower[ ( ZH_UCHAR ) szText[ u ] ];
      else
         for( u = 0; u < nLen; u++ )
            szText[ u ] = ZH_TOLOWER( szText[ u ] );
   }

   return szText;
}

char * zh_strUpper( char * szText, ZH_SIZE nLen )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_strUpper(%s, %" ZH_PFS "u)", szText, nLen ) );

   {
      PZH_CODEPAGE cdp = zh_vmCodepage();
      ZH_SIZE u;

      if( cdp )
         for( u = 0; u < nLen; u++ )
            szText[ u ] = ( char ) cdp->upper[ ( ZH_UCHAR ) szText[ u ] ];
      else
         for( u = 0; u < nLen; u++ )
            szText[ u ] = ZH_TOUPPER( szText[ u ] );
   }

   return szText;
}

/* end of byte only functions */


/*
 * basic CP functions
 */

ZH_BOOL zh_strIsDigit( const char * szChar )
{
   PZH_CODEPAGE cdp = zh_vmCodepage();

   if( cdp )
   {
      if( ZH_CODEPAGE_ISCUSTOM( cdp ) && cdp->wcharFlags )
      {
         ZH_SIZE n = 0;
         ZH_WCHAR wc;
         if( ZH_CODEPAGE_CHAR_GET( cdp, szChar, zh_strnlen( szChar, 6 ), &n, &wc ) )
            return ( ZH_CODEPAGE_CHAR_FLAGS( cdp, wc ) & ZH_CODEPAGE_DIGIT ) != 0;
         else
            return ZH_FALSE;
      }
      else
         return ( cdp->flags[ ( ZH_UCHAR ) *szChar ] & ZH_CODEPAGE_DIGIT ) != 0;
   }
   else
   {
      int iChar = ( ZH_UCHAR ) *szChar;
      return ZH_ISDIGIT( iChar );
   }
}

ZH_BOOL zh_strIsAlpha( const char * szChar )
{
   PZH_CODEPAGE cdp = zh_vmCodepage();

   if( cdp )
   {
      if( ZH_CODEPAGE_ISCUSTOM( cdp ) && cdp->wcharFlags )
      {
         ZH_SIZE n = 0;
         ZH_WCHAR wc;
         if( ZH_CODEPAGE_CHAR_GET( cdp, szChar, zh_strnlen( szChar, 6 ), &n, &wc ) )
            return ( ZH_CODEPAGE_CHAR_FLAGS( cdp, wc ) & ZH_CODEPAGE_ALPHA ) != 0;
         else
            return ZH_FALSE;
      }
      else
         return ( cdp->flags[ ( ZH_UCHAR ) *szChar ] & ZH_CODEPAGE_ALPHA ) != 0;
   }
   else
   {
      int iChar = ( ZH_UCHAR ) *szChar;
      return ZH_ISALPHA( iChar );
   }
}

ZH_BOOL zh_strIsLower( const char * szChar )
{
   PZH_CODEPAGE cdp = zh_vmCodepage();

   if( cdp )
   {
      if( ZH_CODEPAGE_ISCUSTOM( cdp ) && cdp->wcharFlags )
      {
         ZH_SIZE n = 0;
         ZH_WCHAR wc;
         if( ZH_CODEPAGE_CHAR_GET( cdp, szChar, zh_strnlen( szChar, 6 ), &n, &wc ) )
            return ( ZH_CODEPAGE_CHAR_FLAGS( cdp, wc ) & ZH_CODEPAGE_LOWER ) != 0;
         else
            return ZH_FALSE;
      }
      else
         return ( cdp->flags[ ( ZH_UCHAR ) *szChar ] & ZH_CODEPAGE_LOWER ) != 0;
   }
   else
   {
      int iChar = ( ZH_UCHAR ) *szChar;
      return ZH_ISLOWER( iChar );
   }
}

ZH_BOOL zh_strIsUpper( const char * szChar )
{
   PZH_CODEPAGE cdp = zh_vmCodepage();

   if( cdp )
   {
      if( ZH_CODEPAGE_ISCUSTOM( cdp ) && cdp->wcharFlags )
      {
         ZH_SIZE n = 0;
         ZH_WCHAR wc;
         if( ZH_CODEPAGE_CHAR_GET( cdp, szChar, zh_strnlen( szChar, 6 ), &n, &wc ) )
            return ( ZH_CODEPAGE_CHAR_FLAGS( cdp, wc ) & ZH_CODEPAGE_UPPER ) != 0;
         else
            return ZH_FALSE;
      }
      else
         return ( cdp->flags[ ( ZH_UCHAR ) *szChar ] & ZH_CODEPAGE_UPPER ) != 0;
   }
   else
   {
      int iChar = ( ZH_UCHAR ) *szChar;
      return ZH_ISUPPER( iChar );
   }
}

/*
 * comparison
 */

const ZH_UCHAR * zh_cdpGetSortTab( PZH_CODEPAGE cdp )
{
   return ( cdp->nMulti == 0 && cdp->nACSort == 0 &&
            cdp->wcharCmp == NULL ) ? cdp->sort : NULL;
}

int zh_cdpcmp( const char * szFirst, ZH_SIZE nLenFirst,
               const char * szSecond, ZH_SIZE nLenSecond,
               PZH_CODEPAGE cdp, ZH_BOOL fExact )
{
   return ZH_CODEPAGE_CHAR_CMP( cdp, szFirst, nLenFirst, szSecond, nLenSecond, fExact );
}

int zh_cdpicmp( const char * szFirst, ZH_SIZE nLenFirst,
                const char * szSecond, ZH_SIZE nLenSecond,
                PZH_CODEPAGE cdp, ZH_BOOL fExact )
{
   return ZH_CODEPAGE_CHAR_CMPI( cdp, szFirst, nLenFirst, szSecond, nLenSecond, fExact );
}

/*
 * UTF-8 conversions
 */
int zh_cdpUTF8CharSize( ZH_WCHAR wc )
{
   if( wc < 0x0080 )
      return 1;
   else if( wc < 0x0800 )
      return 2;
   else                         /* if( wc <= 0xffff ) */
      return 3;
}

int zh_cdpU16CharToUTF8( char * szUTF8, ZH_WCHAR wc )
{
   int n;

   if( wc < 0x0080 )
   {
      szUTF8[ 0 ] = wc & 0xff;
      n = 1;
   }
   else if( wc < 0x0800 )
   {
      szUTF8[ 0 ] = 0xc0 | ( ( wc >> 6 ) & 0x1f );
      szUTF8[ 1 ] = 0x80 | ( wc & 0x3f );
      n = 2;
   }
   else                         /* if( wc <= 0xffff ) */
   {
      szUTF8[ 0 ] = 0xe0 | ( ( wc >> 12 ) & 0x0f );
      szUTF8[ 1 ] = 0x80 | ( ( wc >> 6 ) & 0x3f );
      szUTF8[ 2 ] = 0x80 | ( wc & 0x3f );
      n = 3;
   }
/*
   else
   {
      n = 0;
   }
 */
   return n;
}

ZH_BOOL zh_cdpUTF8ToU16NextChar( ZH_UCHAR ucChar, int * n, ZH_WCHAR * pwc )
{
   if( *n > 0 )
   {
      if( ( ucChar & 0xc0 ) != 0x80 )
      {
         *n = 0;
         return ZH_FALSE;
      }
      *pwc = ( *pwc << 6 ) | ( ucChar & 0x3f );
      ( *n )--;
      return ZH_TRUE;
   }

   *n = 0;
   *pwc = ucChar;
   if( ucChar >= 0xc0 )
   {
      if( ucChar < 0xe0 )
      {
         *pwc &= 0x1f;
         *n = 1;
      }
      else if( ucChar < 0xf0 )
      {
         *pwc &= 0x0f;
         *n = 2;
      }
      else if( ucChar < 0xf8 )
      {
         *pwc &= 0x07;
         *n = 3;
      }
      else if( ucChar < 0xfc )
      {
         *pwc &= 0x03;
         *n = 4;
      }
      else if( ucChar < 0xfe )
      {
         *pwc &= 0x01;
         *n = 5;
      }
   }
   return ZH_TRUE;
}

ZH_SIZE zh_cdpUTF8StringLength( const char * pSrc, ZH_SIZE nLen )
{
   ZH_SIZE nPos, nDst;
   ZH_WCHAR wc;
   int n = 0;

   for( nPos = nDst = 0; nPos < nLen; )
   {
      if( zh_cdpUTF8ToU16NextChar( ( ZH_UCHAR ) pSrc[ nPos ], &n, &wc ) )
         ++nPos;
      if( n == 0 )
         ++nDst;
   }
   if( n > 0 )
      ++nDst;

   return nDst;
}

ZH_SIZE zh_cdpUTF8StringAt( const char * szNeedle, ZH_SIZE nLenN,
                            const char * szHaystack, ZH_SIZE nLenH,
                            ZH_SIZE nStart, ZH_SIZE nEnd, ZH_BOOL fReverse )
{
   ZH_SIZE nPosN = 0;
   ZH_SIZE nPosH = 0;
   ZH_SIZE nPosX = 0;
   ZH_SIZE nPos = 0;
   ZH_SIZE nRAt = 0;
   ZH_SIZE nAt = 0;

   ZH_WCHAR wcN = 0;
   ZH_WCHAR wcH = 0;
   int nN = 0;
   int nH = 0;

   while( nPosH < nLenH && nPosN < nLenN && nPos < nEnd )
   {
      do
      {
         if( ! zh_cdpUTF8ToU16NextChar( ( ZH_UCHAR ) szHaystack[ nPosH ], &nH, &wcH ) )
            break;
         ++nPosH;
      }
      while( nH && nPosH < nLenH );

      if( ++nPos < nStart )
         continue;

      do
      {
         if( ! zh_cdpUTF8ToU16NextChar( ( ZH_UCHAR ) szNeedle[ nPosN ], &nN, &wcN ) )
            break;
         ++nPosN;
      }
      while( nN && nPosN < nLenN );

      if( wcH == wcN )
      {
         if( nAt == 0 )
         {
            nAt = nPos;
            nPosX = nPosH;
         }

         if( nPosN == nLenN )
         {
            if( fReverse )
            {
               nRAt = nAt;
               nPos = nAt;
               nAt = 0;
               nPosH = nPosX;
               nPosX = 0;
               nPosN = 0;
            }
            else
               return nAt;
         }
      }
      else
      {
         if( nAt )
         {
            nPos = nAt;
            nAt = 0;
            nPosH = nPosX;
            nPosX = 0;
         }
         nPosN = 0;
      }
   }

   return nRAt;
}

ZH_WCHAR zh_cdpUTF8StringPeek( const char * pSrc, ZH_SIZE nLen, ZH_SIZE nPos )
{
   if( nLen )
   {
      ZH_SIZE nPos2;
      ZH_WCHAR wc = 0;
      int n = 0;

      for( nPos2 = 0; nPos2 < nLen && nPos; )
      {
         if( zh_cdpUTF8ToU16NextChar( ( ZH_UCHAR ) pSrc[ nPos2 ], &n, &wc ) )
            ++nPos2;
         if( n == 0 )
            --nPos;
      }

      if( nPos2 < nLen )
      {
         n = 0;
         do
         {
            if( zh_cdpUTF8ToU16NextChar( ( ZH_UCHAR ) pSrc[ nPos2 ], &n, &wc ) )
               ++nPos2;
            if( n == 0 )
               return wc;
         }
         while( nPos2 < nLen );
      }
   }

   return 0;
}

/* caller must free the returned buffer if not NULL */
char * zh_cdpUTF8StringSubstr( const char * pSrc, ZH_SIZE nLen,
                               ZH_SIZE nFrom, ZH_SIZE nCount, ZH_SIZE * pulDest )
{
   ZH_SIZE nDst = 0;
   ZH_WCHAR wc;
   int n;
   char * pDst = NULL;

   if( nCount && nLen )
   {
      ZH_SIZE nPos;
      n = 0;
      for( nPos = 0; nPos < nLen && nFrom; )
      {
         if( zh_cdpUTF8ToU16NextChar( pSrc[ nPos ], &n, &wc ) )
            ++nPos;
         if( n == 0 )
            --nFrom;
      }

      if( nPos < nLen )
      {
         ZH_SIZE nCnt;
         nFrom = nPos;
         nCnt = nCount;
         n = 0;
         do
         {
            if( zh_cdpUTF8ToU16NextChar( pSrc[ nPos ], &n, &wc ) )
               ++nPos;
            if( n == 0 )
               --nCnt;
         }
         while( nPos < nLen && nCnt );

         nDst = nPos - nFrom;
         pDst = ( char * ) zh_xgrab( nDst + 1 );
         memcpy( pDst, &pSrc[ nFrom ], nDst );
         pDst[ nDst ] = '\0';
      }
   }

   if( pulDest )
      *pulDest = nDst;

   return pDst;
}

ZH_BOOL zh_cdpGetFromUTF8( PZH_CODEPAGE cdp, ZH_UCHAR ch,
                           int * n, ZH_WCHAR * pwc )
{
   if( zh_cdpUTF8ToU16NextChar( ch, n, pwc ) )
   {
      if( *n == 0 && cdp )
      {
         if( ZH_CODEPAGE_ISCUSTOM( cdp ) )
         {
            if( ZH_CODEPAGE_CHAR_LEN( cdp, *pwc ) == 1 )
            {
               ZH_SIZE nSize = 0;
               char c;

               if( ZH_CODEPAGE_CHAR_PUT( cdp, &c, 1, &nSize, *pwc ) )
                  *pwc = ( ZH_UCHAR ) c;
            }
         }
         else
         {
            if( cdp->uniTable->uniTrans == NULL )
               zh_cdpBuildTransTable( cdp->uniTable );

            if( *pwc <= cdp->uniTable->wcMax )
            {
               ZH_UCHAR uc = cdp->uniTable->uniTrans[ *pwc ];
               if( uc )
                  *pwc = uc;
            }
         }
      }
      return ZH_TRUE;
   }
   return ZH_FALSE;
}

ZH_SIZE zh_cdpStrAsUTF8Len( PZH_CODEPAGE cdp,
                            const char * pSrc, ZH_SIZE nSrc,
                            ZH_SIZE nMax )
{
   ZH_SIZE nPosS, nPosD;
   int i, n;

   if( ZH_CODEPAGE_ISUTF8( cdp ) )
      return ( nMax && nSrc > nMax ) ? nMax : nSrc;
   else if( ZH_CODEPAGE_ISCUSTOM( cdp ) )
   {
      ZH_WCHAR wc;
      nPosS = nPosD = 0;
      while( ZH_CODEPAGE_CHAR_GET( cdp, pSrc, nSrc, &nPosS, &wc ) )
      {
         i = zh_cdpUTF8CharSize( wc );
         if( nMax && nPosD + i > nMax )
            break;
         nPosD += i;
      }
   }
   else
   {
      const ZH_WCHAR * uniCodes = cdp->uniTable->uniCodes;
      for( nPosS = nPosD = 0; nPosS < nSrc; ++nPosS )
      {
         ZH_UCHAR uc = ( ZH_UCHAR ) pSrc[ nPosS ];
         ZH_WCHAR wc = uniCodes[ uc ];

         if( wc == 0 )
            wc = uc;
         n = zh_cdpUTF8CharSize( wc );
         if( nMax && nPosD + n > nMax )
            break;
         nPosD += n;
      }
   }

   return nPosD;
}

ZH_SIZE zh_cdpStrToUTF8( PZH_CODEPAGE cdp,
                         const char * pSrc, ZH_SIZE nSrc,
                         char * pDst, ZH_SIZE nDst )
{
   ZH_SIZE nPosS, nPosD, u;

   if( ZH_CODEPAGE_ISUTF8( cdp ) )
   {
      if( nSrc > nDst )
         nSrc = nDst;
      else if( nSrc < nDst )
         pDst[ nSrc ] = '\0';
      memcpy( pDst, pSrc, nSrc );
      return nSrc;
   }
   else if( ZH_CODEPAGE_ISCUSTOM( cdp ) )
   {
      ZH_WCHAR wc;
      nPosS = nPosD = 0;
      while( nPosD < nDst && ZH_CODEPAGE_CHAR_GET( cdp, pSrc, nSrc, &nPosS, &wc ) )
      {
         u = zh_cdpUTF8CharSize( wc );
         if( nPosD + u <= nDst )
         {
            zh_cdpU16CharToUTF8( &pDst[ nPosD ], wc );
            nPosD += u;
         }
         else
            break;
      }
   }
   else
   {
      const ZH_WCHAR * uniCodes = cdp->uniTable->uniCodes;
      for( nPosS = nPosD = 0; nPosS < nSrc && nPosD < nDst; ++nPosS )
      {
         ZH_UCHAR uc = ( ZH_UCHAR ) pSrc[ nPosS ];
         ZH_WCHAR wc = uniCodes[ uc ];

         if( wc == 0 )
            wc = uc;
         u = zh_cdpUTF8CharSize( wc );
         if( nPosD + u <= nDst )
         {
            zh_cdpU16CharToUTF8( &pDst[ nPosD ], wc );
            nPosD += u;
         }
         else
            break;
      }
   }

   if( nPosD < nDst )
      pDst[ nPosD ] = '\0';

   return nPosD;
}

ZH_SIZE zh_cdpStrToUTF8Disp( PZH_CODEPAGE cdp,
                             const char * pSrc, ZH_SIZE nSrc,
                             char * pDst, ZH_SIZE nDst )
{
   ZH_SIZE nPosS, nPosD, u;

   if( ZH_CODEPAGE_ISUTF8( cdp ) )
   {
      if( nSrc > nDst )
         nSrc = nDst;
      else if( nSrc < nDst )
         pDst[ nSrc ] = '\0';
      memcpy( pDst, pSrc, nSrc );
      return nSrc;
   }
   else if( ZH_CODEPAGE_ISCUSTOM( cdp ) )
   {
      ZH_WCHAR wc;
      nPosS = nPosD = 0;
      while( nPosD < nDst && ZH_CODEPAGE_CHAR_GET( cdp, pSrc, nSrc, &nPosS, &wc ) )
      {
         if( wc < 32 )
            wc = s_uniCtrls[ wc ];
         u = zh_cdpUTF8CharSize( wc );
         if( nPosD + u <= nDst )
         {
            zh_cdpU16CharToUTF8( &pDst[ nPosD ], wc );
            nPosD += u;
         }
         else
            break;
      }
   }
   else
   {
      const ZH_WCHAR * uniCodes = cdp->uniTable->uniCodes;
      for( nPosS = nPosD = 0; nPosS < nSrc && nPosD < nDst; ++nPosS )
      {
         ZH_UCHAR uc = ( ZH_UCHAR ) pSrc[ nPosS ];
         ZH_WCHAR wc = uniCodes[ uc ];

         if( wc == 0 )
            wc = uc < 32 ? s_uniCtrls[ uc ] : s_uniCodes[ uc ];

         u = zh_cdpUTF8CharSize( wc );
         if( nPosD + u <= nDst )
         {
            zh_cdpU16CharToUTF8( &pDst[ nPosD ], wc );
            nPosD += u;
         }
         else
            break;
      }
   }

   if( nPosD < nDst )
      pDst[ nPosD ] = '\0';

   return nPosD;
}

ZH_SIZE zh_cdpUTF8AsStrLen( PZH_CODEPAGE cdp, const char * pSrc, ZH_SIZE nSrc,
                            ZH_SIZE nMax )
{
   ZH_WCHAR wc = 0;
   ZH_SIZE nPosS, nPosD;
   int n = 0, i;

   if( ZH_CODEPAGE_ISUTF8( cdp ) )
      return ( nMax && nSrc > nMax ) ? nMax : nSrc;
   else if( ZH_CODEPAGE_ISCUSTOM( cdp ) )
   {
      for( nPosS = nPosD = 0; nPosS < nSrc; )
      {
         if( zh_cdpUTF8ToU16NextChar( ( ZH_UCHAR ) pSrc[ nPosS ], &n, &wc ) )
            ++nPosS;

         if( n == 0 )
         {
            i = ZH_CODEPAGE_CHAR_LEN( cdp, wc );
            if( nMax && nPosD + i > nMax )
               break;
            nPosD += i;
         }
      }
   }
   else
   {
      for( nPosS = nPosD = 0; nPosS < nSrc; )
      {
         if( zh_cdpUTF8ToU16NextChar( ( ZH_UCHAR ) pSrc[ nPosS ], &n, &wc ) )
            ++nPosS;

         if( n == 0 )
         {
            ++nPosD;
            if( nMax && nPosD >= nMax )
               break;
         }
      }
   }

   return nPosD;
}

ZH_SIZE zh_cdpUTF8ToStr( PZH_CODEPAGE cdp,
                         const char * pSrc, ZH_SIZE nSrc,
                         char * pDst, ZH_SIZE nDst )
{
   ZH_UCHAR * uniTrans;
   ZH_WCHAR wcMax, wc = 0;
   ZH_SIZE nPosS, nPosD;
   int n = 0;

   if( ZH_CODEPAGE_ISUTF8( cdp ) )
   {
      if( nSrc > nDst )
         nSrc = nDst;
      else if( nSrc < nDst )
         pDst[ nSrc ] = '\0';
      memcpy( pDst, pSrc, nSrc );
      return nSrc;
   }
   else if( ZH_CODEPAGE_ISCUSTOM( cdp ) )
   {
      for( nPosS = nPosD = 0; nPosS < nSrc && nPosD < nDst; )
      {
         if( zh_cdpUTF8ToU16NextChar( ( ZH_UCHAR ) pSrc[ nPosS ], &n, &wc ) )
            ++nPosS;

         if( n == 0 )
         {
            if( ! ZH_CODEPAGE_CHAR_PUT( cdp, pDst, nDst, &nPosD, wc ) )
               break;
         }
      }
   }
   else
   {
      if( cdp->uniTable->uniTrans == NULL )
         zh_cdpBuildTransTable( cdp->uniTable );
      uniTrans = cdp->uniTable->uniTrans;
      wcMax = cdp->uniTable->wcMax;

      for( nPosS = nPosD = 0; nPosS < nSrc && nPosD < nDst; )
      {
         if( zh_cdpUTF8ToU16NextChar( ( ZH_UCHAR ) pSrc[ nPosS ], &n, &wc ) )
            ++nPosS;

         if( n == 0 )
         {
            if( wc <= wcMax && uniTrans[ wc ] )
               pDst[ nPosD++ ] = uniTrans[ wc ];
            else
               pDst[ nPosD++ ] = wc >= 0x100 ? '?' : ( ZH_UCHAR ) wc;
         }
      }
   }

   if( nPosD < nDst )
      pDst[ nPosD ] = '\0';

   return nPosD;
}

/*
 * U16 (hb wide char) conversions
 */
ZH_WCHAR zh_cdpGetU16( PZH_CODEPAGE cdp, ZH_UCHAR ch )
{
   if( cdp )
   {
      ZH_WCHAR wc;
      if( ZH_CODEPAGE_ISCUSTOM( cdp ) )
      {
         ZH_SIZE n = 0;
         if( ! ZH_CODEPAGE_CHAR_GET( cdp, ( const char * ) &ch, 1, &n, &wc ) )
            wc = 0;
      }
      else
         wc = cdp->uniTable->uniCodes[ ch ];
      return wc == 0 ? ch : wc;
   }
   else
      return ch;
}

ZH_WCHAR zh_cdpGetWC( PZH_CODEPAGE cdp, ZH_UCHAR ch, ZH_WCHAR wcDef )
{
   if( cdp )
   {
      if( ZH_CODEPAGE_ISCUSTOM( cdp ) )
      {
         ZH_WCHAR wc;
         ZH_SIZE n = 0;
         if( ZH_CODEPAGE_CHAR_GET( cdp, ( const char * ) &ch, 1, &n, &wc ) )
            wcDef = wc;
      }
      else if( cdp->uniTable->uniCodes[ ch ] )
         wcDef = cdp->uniTable->uniCodes[ ch ];
   }
   else if( ch >= 32 && ch <= 126 )
      wcDef = ch;
   return wcDef;
}

ZH_WCHAR zh_cdpGetU16Disp( PZH_CODEPAGE cdp, ZH_UCHAR ch )
{
   if( cdp )
   {
      ZH_WCHAR wc;
      if( ZH_CODEPAGE_ISCUSTOM( cdp ) )
      {
         ZH_SIZE n = 0;
         if( ! ZH_CODEPAGE_CHAR_GET( cdp, ( const char * ) &ch, 1, &n, &wc ) )
            wc = 0;
      }
      else
         wc = cdp->uniTable->uniCodes[ ch ];
      if( wc == 0 )
         wc = ch < 32 ? s_uniCtrls[ ch ] : s_uniCodes[ ch ];
      return wc;
   }
   else
      return ch;
}

ZH_UCHAR zh_cdpGetChar( PZH_CODEPAGE cdp, ZH_WCHAR wc )
{
   if( cdp )
   {
      if( ZH_CODEPAGE_ISCUSTOM( cdp ) )
      {
         if( ZH_CODEPAGE_CHAR_LEN( cdp, wc ) == 1 )
         {
            ZH_SIZE n = 0;
            char c;

            if( ! ZH_CODEPAGE_CHAR_PUT( cdp, &c, 1, &n, wc ) )
               wc = '?';
            else
               wc = ( ZH_UCHAR ) c;
         }
         else
            wc = '?';
      }
      else
      {
         if( cdp->uniTable->uniTrans == NULL )
            zh_cdpBuildTransTable( cdp->uniTable );

         if( wc <= cdp->uniTable->wcMax )
         {
            ZH_UCHAR uc = cdp->uniTable->uniTrans[ wc ];
            if( uc )
               wc = uc;
         }
      }
   }
   return wc >= 0x100 ? '?' : ( ZH_UCHAR ) wc;
}

ZH_UCHAR zh_cdpGetUC( PZH_CODEPAGE cdp, ZH_WCHAR wc, ZH_UCHAR ucDef )
{
   if( cdp )
   {
      if( ZH_CODEPAGE_ISCUSTOM( cdp ) )
      {
         if( ZH_CODEPAGE_CHAR_LEN( cdp, wc ) == 1 )
         {
            ZH_SIZE n = 0;
            char c;
            if( ZH_CODEPAGE_CHAR_PUT( cdp, &c, 1, &n, wc ) )
               ucDef = ( ZH_UCHAR ) c;
         }
      }
      else
      {
         if( cdp->uniTable->uniTrans == NULL )
            zh_cdpBuildTransTable( cdp->uniTable );

         if( wc <= cdp->uniTable->wcMax )
         {
            ZH_UCHAR uc = cdp->uniTable->uniTrans[ wc ];
            if( uc )
               ucDef = uc;
         }
         if( ucDef == 0 && wc <= ZH_MAX_CTRL_CODE )
         {
            ZH_UCHAR uc = s_rev_ctrl[ wc ];
            if( uc )
               ucDef = uc;
         }
      }
   }
   else if( wc <= 0xFF )
      ucDef = ( ZH_UCHAR ) wc;

   return ucDef;
}

ZH_WCHAR zh_cdpGetU16Ctrl( ZH_WCHAR wc )
{
   return wc < 32 ? s_uniCtrls[ wc ] : ( wc == 0x007F ? 0x2302 : wc );
}

ZH_SIZE zh_cdpStrAsU16Len( PZH_CODEPAGE cdp,
                           const char * pSrc, ZH_SIZE nSrc,
                           ZH_SIZE nMax )
{
   if( ZH_CODEPAGE_ISUTF8( cdp ) )
   {
      nSrc = zh_cdpUTF8StringLength( pSrc, nSrc );
   }
   else if( ZH_CODEPAGE_ISCUSTOM( cdp ) )
   {
      ZH_WCHAR wc;
      ZH_SIZE nPosS, nPosD;
      nPosS = nPosD = 0;
      while( ZH_CODEPAGE_CHAR_GET( cdp, pSrc, nSrc, &nPosS, &wc ) )
      {
         ++nPosD;
         if( nMax && nPosD >= nMax )
            break;
      }
      return nPosD;
   }

   return ( nMax && nSrc > nMax ) ? nMax : nSrc;
}

#undef ZH_CODEPAGE_ENDIAN_SWAP
#if defined( ZH_BIG_ENDIAN )
#  define ZH_CODEPAGE_ENDIAN_SWAP  ZH_CODEPAGE_ENDIAN_LITTLE
#elif defined( ZH_LITTLE_ENDIAN )
#  define ZH_CODEPAGE_ENDIAN_SWAP  ZH_CODEPAGE_ENDIAN_BIG
#endif

ZH_SIZE zh_cdpStrToU16( PZH_CODEPAGE cdp, int iEndian,
                        const char * pSrc, ZH_SIZE nSrc,
                        ZH_WCHAR * pDst, ZH_SIZE nDst )
{
   const ZH_WCHAR * uniCodes;
   ZH_SIZE nPosS, nPosD;

   if( ZH_CODEPAGE_ISUTF8( cdp ) )
   {
      ZH_WCHAR wc = 0;
      int n = 0;

      for( nPosS = nPosD = 0; nPosS < nSrc && nPosD < nDst; )
      {
         if( zh_cdpUTF8ToU16NextChar( ( ZH_UCHAR ) pSrc[ nPosS ], &n, &wc ) )
            ++nPosS;

         if( n == 0 )
         {
#if defined( ZH_CODEPAGE_ENDIAN_SWAP )
            if( iEndian == ZH_CODEPAGE_ENDIAN_SWAP )
               wc = ZH_SWAP_UINT16( wc );
            pDst[ nPosD++ ] = wc;
#else
            if( iEndian == ZH_CODEPAGE_ENDIAN_LITTLE )
               ZH_PUT_LE_UINT16( &pDst[ nPosD ], wc );
            else if( iEndian == ZH_CODEPAGE_ENDIAN_BIG )
               ZH_PUT_BE_UINT16( &pDst[ nPosD ], wc );
            else
               pDst[ nPosD ] = wc;
            ++nPosD;
#endif
         }
      }
   }
   else if( ZH_CODEPAGE_ISCUSTOM( cdp ) )
   {
      ZH_WCHAR wc;
      nPosS = nPosD = 0;
      while( nPosD < nDst && ZH_CODEPAGE_CHAR_GET( cdp, pSrc, nSrc, &nPosS, &wc ) )
      {
#if defined( ZH_CODEPAGE_ENDIAN_SWAP )
         if( iEndian == ZH_CODEPAGE_ENDIAN_SWAP )
            wc = ZH_SWAP_UINT16( wc );
         pDst[ nPosD++ ] = wc;
#else
         if( iEndian == ZH_CODEPAGE_ENDIAN_LITTLE )
            ZH_PUT_LE_UINT16( &pDst[ nPosD ], wc );
         else if( iEndian == ZH_CODEPAGE_ENDIAN_BIG )
            ZH_PUT_BE_UINT16( &pDst[ nPosD ], wc );
         else
            pDst[ nPosD ] = wc;
         ++nPosD;
#endif
      }
   }
   else
   {
      uniCodes = cdp->uniTable->uniCodes;
      for( nPosS = nPosD = 0; nPosS < nSrc && nPosD < nDst; ++nPosS )
      {
         ZH_UCHAR uc = ( ZH_UCHAR ) pSrc[ nPosS ];
         ZH_WCHAR wc = uniCodes[ uc ];

         if( wc == 0 )
            wc = uc;
#if defined( ZH_CODEPAGE_ENDIAN_SWAP )
         if( iEndian == ZH_CODEPAGE_ENDIAN_SWAP )
            wc = ZH_SWAP_UINT16( wc );
         pDst[ nPosD++ ] = wc;
#else
         if( iEndian == ZH_CODEPAGE_ENDIAN_LITTLE )
            ZH_PUT_LE_UINT16( &pDst[ nPosD ], wc );
         else if( iEndian == ZH_CODEPAGE_ENDIAN_BIG )
            ZH_PUT_BE_UINT16( &pDst[ nPosD ], wc );
         else
            pDst[ nPosD ] = wc;
         ++nPosD;
#endif
      }
   }

   if( nPosD < nDst )
      pDst[ nPosD ] = '\0';

   return nPosD;
}

ZH_WCHAR * zh_cdpnStrDupU16( PZH_CODEPAGE cdp, int iEndian,
                             const char * pSrc, ZH_SIZE nSrc,
                             ZH_SIZE * pnDst )
{
   ZH_SIZE nLen = zh_cdpStrAsU16Len( cdp, pSrc, nSrc, 0 );
   ZH_WCHAR * pDst = ( ZH_WCHAR * ) zh_xgrab( ( nLen + 1 ) * sizeof( ZH_WCHAR ) );

   zh_cdpStrToU16( cdp, iEndian, pSrc, nSrc, pDst, nLen + 1 );
   if( pnDst )
      *pnDst = nLen;
   return pDst;
}

ZH_WCHAR * zh_cdpStrDupU16( PZH_CODEPAGE cdp, int iEndian, const char * pSrc )
{
   return zh_cdpnStrDupU16( cdp, iEndian, pSrc, strlen( pSrc ), NULL );
}

ZH_WCHAR * zh_cdpStrDupnU16( PZH_CODEPAGE cdp, int iEndian, const char * pSrc, ZH_SIZE nLen )
{
   return zh_cdpnStrDupU16( cdp, iEndian, pSrc, zh_strnlen( pSrc, nLen ), NULL );
}

ZH_SIZE zh_cdpU16AsStrLen( PZH_CODEPAGE cdp,
                           const ZH_WCHAR * pSrc, ZH_SIZE nSrc,
                           ZH_SIZE nMax )
{
   ZH_SIZE nPosS, nPosD;
   int i;

   if( ZH_CODEPAGE_ISUTF8( cdp ) )
   {
      for( nPosS = nPosD = 0; nPosS < nSrc; ++nPosS )
      {
         i = zh_cdpUTF8CharSize( pSrc[ nPosS ] );
         if( nMax && nPosD + i > nMax )
            break;
         nPosD += i;
      }
   }
   else if( ZH_CODEPAGE_ISCUSTOM( cdp ) )
   {
      for( nPosS = nPosD = 0; nPosS < nSrc; ++nPosS )
      {
         i = ZH_CODEPAGE_CHAR_LEN( cdp, pSrc[ nPosS ] );
         if( nMax && nPosD + i > nMax )
            break;
         nPosD += i;
      }
   }
   else
      nPosD = ( nMax && nSrc > nMax ) ? nMax : nSrc;

   return nPosD;
}

ZH_SIZE zh_cdpU16ToStr( PZH_CODEPAGE cdp, int iEndian,
                        const ZH_WCHAR * pSrc, ZH_SIZE nSrc,
                        char * pDst, ZH_SIZE nDst )
{
   ZH_UCHAR * uniTrans;
   ZH_WCHAR wcMax, wc;
   ZH_SIZE nPosS, nPosD;

   if( ZH_CODEPAGE_ISUTF8( cdp ) )
   {
      for( nPosS = nPosD = 0; nPosS < nSrc; ++nPosS )
      {
         int i;
#if defined( ZH_CODEPAGE_ENDIAN_SWAP )
         wc = pSrc[ nPosS ];
         if( iEndian == ZH_CODEPAGE_ENDIAN_SWAP )
            wc = ZH_SWAP_UINT16( wc );
#else
         if( iEndian == ZH_CODEPAGE_ENDIAN_LITTLE )
            wc = ZH_GET_LE_UINT16( &pSrc[ nPosS ] );
         else if( iEndian == ZH_CODEPAGE_ENDIAN_BIG )
            wc = ZH_GET_BE_UINT16( &pSrc[ nPosS ] );
         else
            wc = pSrc[ nPosS ];
#endif
         i = zh_cdpUTF8CharSize( wc );
         if( nPosD + i <= nDst )
         {
            zh_cdpU16CharToUTF8( &pDst[ nPosD ], wc );
            nPosD += i;
         }
         else
            break;
      }
   }
   else if( ZH_CODEPAGE_ISCUSTOM( cdp ) )
   {
      for( nPosS = nPosD = 0; nPosS < nSrc; ++nPosS )
      {
#if defined( ZH_CODEPAGE_ENDIAN_SWAP )
         wc = pSrc[ nPosS ];
         if( iEndian == ZH_CODEPAGE_ENDIAN_SWAP )
            wc = ZH_SWAP_UINT16( wc );
#else
         if( iEndian == ZH_CODEPAGE_ENDIAN_LITTLE )
            wc = ZH_GET_LE_UINT16( &pSrc[ nPosS ] );
         else if( iEndian == ZH_CODEPAGE_ENDIAN_BIG )
            wc = ZH_GET_BE_UINT16( &pSrc[ nPosS ] );
         else
            wc = pSrc[ nPosS ];
#endif
         if( ! ZH_CODEPAGE_CHAR_PUT( cdp, pDst, nDst, &nPosD, wc ) )
            break;
      }
   }
   else
   {
      if( cdp->uniTable->uniTrans == NULL )
         zh_cdpBuildTransTable( cdp->uniTable );
      uniTrans = cdp->uniTable->uniTrans;
      wcMax = cdp->uniTable->wcMax;

      for( nPosS = nPosD = 0; nPosS < nSrc && nPosD < nDst; ++nPosS )
      {
#if defined( ZH_CODEPAGE_ENDIAN_SWAP )
         wc = pSrc[ nPosS ];
         if( iEndian == ZH_CODEPAGE_ENDIAN_SWAP )
            wc = ZH_SWAP_UINT16( wc );
#else
         if( iEndian == ZH_CODEPAGE_ENDIAN_LITTLE )
            wc = ZH_GET_LE_UINT16( &pSrc[ nPosS ] );
         else if( iEndian == ZH_CODEPAGE_ENDIAN_BIG )
            wc = ZH_GET_BE_UINT16( &pSrc[ nPosS ] );
         else
            wc = pSrc[ nPosS ];
#endif
         if( wc <= wcMax && uniTrans[ wc ] )
            pDst[ nPosD++ ] = uniTrans[ wc ];
         else
            pDst[ nPosD++ ] = wc >= 0x100 ? '?' : ( ZH_UCHAR ) wc;
      }
   }

   if( nPosD < nDst )
      pDst[ nPosD ] = '\0';

   return nPosD;
}


/*
 * CP translations
 */
ZH_SIZE zh_cdpTransLen( const char * pSrc, ZH_SIZE nSrc, ZH_SIZE nMax,
                        PZH_CODEPAGE cdpIn, PZH_CODEPAGE cdpOut )
{
   ZH_SIZE nSize;

   if( cdpIn && cdpOut && cdpIn != cdpOut &&
       ( cdpIn->uniTable != cdpOut->uniTable ||
         ZH_CODEPAGE_ISCUSTOM( cdpIn ) || ZH_CODEPAGE_ISCUSTOM( cdpOut ) ) )
   {
      if( ZH_CODEPAGE_ISUTF8( cdpIn ) )
         return zh_cdpUTF8AsStrLen( cdpOut, pSrc, nSrc, nMax );
      else if( ZH_CODEPAGE_ISUTF8( cdpOut ) )
         return zh_cdpStrAsUTF8Len( cdpIn, pSrc, nSrc, nMax );
      else if( ZH_CODEPAGE_ISCUSTOM( cdpIn ) || ZH_CODEPAGE_ISCUSTOM( cdpOut ) )
      {
         ZH_SIZE nPosS;
         ZH_WCHAR wc;

         nPosS = nSize = 0;
         while( ZH_CODEPAGE_CHAR_GET( cdpIn, pSrc, nSrc, &nPosS, &wc ) )
         {
            int i = ZH_CODEPAGE_CHAR_LEN( cdpOut, wc );
            if( nMax && nSize + i > nMax )
               break;
            nSize += i;
         }
      }
      else
         nSize = ( nMax && nSrc > nMax ) ? nMax : nSrc;
   }
   else
      nSize = ( nMax && nSrc > nMax ) ? nMax : nSrc;

   return nSize;
}

ZH_SIZE zh_cdpTransTo( const char * pSrc, ZH_SIZE nSrc,
                       char * pDst, ZH_SIZE nDst,
                       PZH_CODEPAGE cdpIn, PZH_CODEPAGE cdpOut )
{
   ZH_SIZE nSize;

   if( cdpIn && cdpOut && cdpIn != cdpOut &&
       ( cdpIn->uniTable != cdpOut->uniTable ||
         ZH_CODEPAGE_ISCUSTOM( cdpIn ) || ZH_CODEPAGE_ISCUSTOM( cdpOut ) ) )
   {
      if( ZH_CODEPAGE_ISUTF8( cdpIn ) )
         return zh_cdpUTF8ToStr( cdpOut, pSrc, nSrc, pDst, nDst );
      else if( ZH_CODEPAGE_ISUTF8( cdpOut ) )
         return zh_cdpStrToUTF8( cdpIn, pSrc, nSrc, pDst, nDst );
      else if( ZH_CODEPAGE_ISCUSTOM( cdpIn ) || ZH_CODEPAGE_ISCUSTOM( cdpOut ) )
      {
         ZH_SIZE nPosS;
         ZH_WCHAR wc;

         nPosS = nSize = 0;
         while( nSize < nDst && ZH_CODEPAGE_CHAR_GET( cdpIn, pSrc, nSrc, &nPosS, &wc ) )
         {
            if( ! ZH_CODEPAGE_CHAR_PUT( cdpOut, pDst, nDst, &nSize, wc ) )
               break;
         }
      }
      else
      {
         ZH_UCHAR * uniTrans;
         ZH_WCHAR wcMax;

         if( cdpOut->uniTable->uniTrans == NULL )
            zh_cdpBuildTransTable( cdpOut->uniTable );
         uniTrans = cdpOut->uniTable->uniTrans;
         wcMax = cdpOut->uniTable->wcMax;

         if( nSrc > nDst )
            nSrc = nDst;
         for( nSize = 0; nSize < nSrc; ++nSize )
         {
            ZH_UCHAR uc = ( ZH_UCHAR ) pSrc[ nSize ];
            ZH_WCHAR wc = cdpIn->uniTable->uniCodes[ uc ];
            if( wc && wc <= wcMax && uniTrans[ wc ] )
               uc = uniTrans[ wc ];
            pDst[ nSize ] = uc;
         }
      }
   }
   else
   {
      nSize = ( nSrc > nDst ) ? nDst : nSrc;
      memcpy( pDst, pSrc, nSize );
   }

   if( nSize < nDst )
      pDst[ nSize ] = '\0';

   return nSize;
}

int zh_cdpTranslateChar( int iChar, PZH_CODEPAGE cdpIn, PZH_CODEPAGE cdpOut )
{
   if( cdpIn && cdpOut && cdpIn != cdpOut &&
       ( cdpIn->uniTable != cdpOut->uniTable ||
         ZH_CODEPAGE_ISCUSTOM( cdpIn ) || ZH_CODEPAGE_ISCUSTOM( cdpOut ) ) &&
       iChar >= 0 && iChar < 256 )
   {
      ZH_WCHAR wc;

      if( ZH_CODEPAGE_ISCUSTOM( cdpIn ) || ZH_CODEPAGE_ISCUSTOM( cdpOut ) )
      {
         ZH_SIZE n = 0;
         char c = ( char ) iChar;

         if( ZH_CODEPAGE_CHAR_GET( cdpIn, &c, 1, &n, &wc ) )
         {
            if( ZH_CODEPAGE_CHAR_PUT( cdpOut, &c, 1, &n, wc ) )
            {
               if( c != '?' )
                  iChar = ( ZH_UCHAR ) c;
            }
         }
      }
      else
      {
         wc = cdpIn->uniTable->uniCodes[ iChar ];

         if( wc )
         {
            if( cdpOut->uniTable->uniTrans == NULL )
               zh_cdpBuildTransTable( cdpOut->uniTable );

            if( wc <= cdpOut->uniTable->wcMax )
            {
               wc = cdpOut->uniTable->uniTrans[ wc ];
               if( wc )
                  iChar = wc;
            }
         }
      }
   }

   return iChar;
}

int zh_cdpTranslateDispChar( int iChar, PZH_CODEPAGE cdpIn, PZH_CODEPAGE cdpOut )
{
   if( cdpIn && cdpOut && cdpIn != cdpOut &&
       ( cdpIn->uniTable != cdpOut->uniTable ||
         ZH_CODEPAGE_ISCUSTOM( cdpIn ) || ZH_CODEPAGE_ISCUSTOM( cdpOut ) ) &&
       iChar >= 0 && iChar < 256 )
   {
      ZH_WCHAR wc;


      if( ZH_CODEPAGE_ISCUSTOM( cdpIn ) || ZH_CODEPAGE_ISCUSTOM( cdpOut ) )
      {
         ZH_SIZE n = 0;
         char c = ( char ) iChar;

         if( ! ZH_CODEPAGE_CHAR_GET( cdpIn, &c, 1, &n, &wc ) )
            wc = ( ZH_WCHAR ) iChar;
         if( wc < 32 )
            wc = s_uniCtrls[ iChar ];
         if( ZH_CODEPAGE_CHAR_PUT( cdpOut, &c, 1, &n, wc ) )
         {
            if( c != '?' )
               iChar = ( ZH_UCHAR ) c;
         }
      }
      else
      {
         wc = cdpIn->uniTable->uniCodes[ iChar ];

         if( wc == 0 )
            wc = iChar < 32 ? s_uniCtrls[ iChar ] : s_uniCodes[ iChar ];

         if( wc )
         {
            if( cdpOut->uniTable->uniTrans == NULL )
               zh_cdpBuildTransTable( cdpOut->uniTable );

            if( wc <= cdpOut->uniTable->wcMax )
            {
               wc = cdpOut->uniTable->uniTrans[ wc ];
               if( wc )
                  iChar = wc;
            }
         }
      }
   }

   return iChar;
}

ZH_SIZE zh_cdpnDupLen( const char * pSrc, ZH_SIZE nSrc,
                       PZH_CODEPAGE cdpIn, PZH_CODEPAGE cdpOut )
{
   return zh_cdpTransLen( pSrc, nSrc, 0, cdpIn, cdpOut );
}

ZH_SIZE zh_cdpnDup2Len( const char * pSrc, ZH_SIZE nSrc, ZH_SIZE nMax,
                        PZH_CODEPAGE cdpIn, PZH_CODEPAGE cdpOut )
{
   return zh_cdpTransLen( pSrc, nSrc, nMax, cdpIn, cdpOut );
}

char * zh_cdpnDup( const char * pSrc, ZH_SIZE * pnLen,
                   PZH_CODEPAGE cdpIn, PZH_CODEPAGE cdpOut )
{
   char * pDst;
   ZH_SIZE nDst;

   nDst = zh_cdpTransLen( pSrc, *pnLen, 0, cdpIn, cdpOut );
   pDst = ( char * ) zh_xgrab( nDst + 1 );
   zh_cdpTransTo( pSrc, *pnLen, pDst, nDst + 1, cdpIn, cdpOut );
   *pnLen = nDst;

   return pDst;
}

const char * zh_cdpnDup2( const char * pSrc, ZH_SIZE nSrc,
                          char * pDst, ZH_SIZE * pnDst,
                          PZH_CODEPAGE cdpIn, PZH_CODEPAGE cdpOut )
{
   *pnDst = zh_cdpTransTo( pSrc, nSrc, pDst, *pnDst, cdpIn, cdpOut );
   return pDst;
}

const char * zh_cdpnDup3( const char * pSrc, ZH_SIZE nSrc,
                          char * pDst, ZH_SIZE * pnDst,
                          char ** pFree, ZH_SIZE * pnSize,
                          PZH_CODEPAGE cdpIn, PZH_CODEPAGE cdpOut )
{
   if( cdpIn && cdpOut && cdpIn != cdpOut && nSrc &&
       ( ! ZH_CODEPAGE_ISUTF8( cdpIn ) || ! ZH_CODEPAGE_ISUTF8( cdpOut ) ) &&
       ( cdpIn->uniTable != cdpOut->uniTable ||
         ZH_CODEPAGE_ISCUSTOM( cdpIn ) || ZH_CODEPAGE_ISCUSTOM( cdpOut ) ) )
   {
      char * pPrev = NULL;
      ZH_SIZE nDst = zh_cdpTransLen( pSrc, nSrc, 0, cdpIn, cdpOut );

      if( pDst == NULL )
      {
         pDst = *pFree;
         if( pDst == NULL && *pnSize > 0 )
            pDst = ( char * ) ZH_UNCONST( pSrc );
      }

      if( nDst >= *pnSize || ( pDst == pSrc && ZH_CODEPAGE_ISCUSTOM( cdpOut ) ) )
      {
         pPrev = *pFree;
         pDst = *pFree = ( char * ) zh_xgrab( nDst + 1 );
         *pnSize = nDst + 1;
      }

      nDst = zh_cdpTransTo( pSrc, nSrc, pDst, *pnSize, cdpIn, cdpOut );

      if( pPrev )
         zh_xfree( pPrev );
      if( pnDst )
         *pnDst = nDst;
      return pDst;
   }

   if( pnDst )
      *pnDst = nSrc;
   return pSrc;
}

char * zh_cdpDup( const char * pszSrc, PZH_CODEPAGE cdpIn, PZH_CODEPAGE cdpOut )
{
   ZH_SIZE nLen = strlen( pszSrc );

   return zh_cdpnDup( pszSrc, &nLen, cdpIn, cdpOut );
}

char * zh_cdpDupn( const char * pszSrc, ZH_SIZE nLen, PZH_CODEPAGE cdpIn, PZH_CODEPAGE cdpOut )
{
   nLen = zh_strnlen( pszSrc, nLen );
   return zh_cdpnDup( pszSrc, &nLen, cdpIn, cdpOut );
}

char * zh_cdpnDupUpper( PZH_CODEPAGE cdp, const char * pszText, ZH_SIZE * pnSize )
{
   ZH_SIZE nSize = pnSize ? *pnSize : strlen( pszText ), n;
   char * pszDst = ( char * ) zh_xgrab( nSize + 1 );

   if( cdp )
   {
      if( ZH_CODEPAGE_ISCUSTOM( cdp ) && cdp->wcharUpper )
      {
         ZH_SIZE nS = 0, nD = 0, nSrc = nSize;
         ZH_WCHAR wc;

         while( ZH_CODEPAGE_CHAR_GET( cdp, pszText, nSrc, &nS, &wc ) )
         {
            wc = ZH_CODEPAGE_CHAR_UPPER( cdp, wc );
            if( ! ZH_CODEPAGE_CHAR_PUT( cdp, pszDst, nSize, &nD, wc ) )
            {
               nSize += ( nSrc - nS + 2 );
               pszDst = ( char * ) zh_xrealloc( pszDst, nSize + 1 );
               if( ! ZH_CODEPAGE_CHAR_PUT( cdp, pszDst, nSize, &nD, wc ) )
                  break;
            }
         }
         nSize = nD;
         if( pnSize )
            *pnSize = nSize;
      }
      else
         for( n = 0; n < nSize; n++ )
            pszDst[ n ] = ( char ) cdp->upper[ ( ZH_UCHAR ) pszText[ n ] ];
   }
   else
      for( n = 0; n < nSize; n++ )
         pszDst[ n ] = ZH_TOUPPER( pszText[ n ] );
   pszDst[ nSize ] = '\0';

   return pszDst;
}

char * zh_cdpnDupLower( PZH_CODEPAGE cdp, const char * pszText, ZH_SIZE * pnSize )
{
   ZH_SIZE nSize = pnSize ? *pnSize : strlen( pszText ), n;
   char * pszDst = ( char * ) zh_xgrab( nSize + 1 );

   if( cdp )
   {
      if( ZH_CODEPAGE_ISCUSTOM( cdp ) && cdp->wcharLower )
      {
         ZH_SIZE nS = 0, nD = 0, nSrc = nSize;
         ZH_WCHAR wc;

         while( ZH_CODEPAGE_CHAR_GET( cdp, pszText, nSrc, &nS, &wc ) )
         {
            wc = ZH_CODEPAGE_CHAR_LOWER( cdp, wc );
            if( ! ZH_CODEPAGE_CHAR_PUT( cdp, pszDst, nSize, &nD, wc ) )
            {
               nSize += ( nSrc - nS + 2 );
               pszDst = ( char * ) zh_xrealloc( pszDst, nSize + 1 );
               if( ! ZH_CODEPAGE_CHAR_PUT( cdp, pszDst, nSize, &nD, wc ) )
                  break;
            }
         }
         nSize = nD;
         if( pnSize )
            *pnSize = nSize;
      }
      else
         for( n = 0; n < nSize; n++ )
            pszDst[ n ] = ( char ) cdp->lower[ ( ZH_UCHAR ) pszText[ n ] ];
   }
   else
      for( n = 0; n < nSize; n++ )
         pszDst[ n ] = ZH_TOLOWER( pszText[ n ] );
   pszDst[ nSize ] = '\0';

   return pszDst;
}

ZH_SIZE zh_cdpnDup2Upper( PZH_CODEPAGE cdp, const char * pszText, ZH_SIZE nSize, char * pBuffer, ZH_SIZE nBuffLen )
{
   ZH_SIZE nMax = ZH_MIN( nSize, nBuffLen ), n;

   if( ! cdp )
      for( n = 0; n < nMax; n++ )
         pBuffer[ n ] = ZH_TOUPPER( pszText[ n ] );
   else if( ! ZH_CODEPAGE_ISCUSTOM( cdp ) || ! cdp->wcharUpper )
      for( n = 0; n < nMax; n++ )
         pBuffer[ n ] = ( char ) cdp->upper[ ( ZH_UCHAR ) pszText[ n ] ];
   else
   {
      ZH_SIZE nS = 0;
      ZH_WCHAR wc;

      nMax = 0;
      while( ZH_CODEPAGE_CHAR_GET( cdp, pszText, nSize, &nS, &wc ) )
      {
         wc = ZH_CODEPAGE_CHAR_UPPER( cdp, wc );
         if( ! ZH_CODEPAGE_CHAR_PUT( cdp, pBuffer, nBuffLen, &nMax, wc ) )
            break;
      }
   }

   if( nMax < nBuffLen )
      pBuffer[ nMax ] = '\0';

   return nMax;
}

ZH_SIZE zh_cdpnDup2Lower( PZH_CODEPAGE cdp, const char * pszText, ZH_SIZE nSize, char * pBuffer, ZH_SIZE nBuffLen )
{
   ZH_SIZE nMax = ZH_MIN( nSize, nBuffLen ), n;

   if( ! cdp )
      for( n = 0; n < nMax; n++ )
         pBuffer[ n ] = ZH_TOLOWER( pszText[ n ] );
   else if( ! ZH_CODEPAGE_ISCUSTOM( cdp ) || ! cdp->wcharLower )
      for( n = 0; n < nMax; n++ )
         pBuffer[ n ] = ( char ) cdp->lower[ ( ZH_UCHAR ) pszText[ n ] ];
   else
   {
      ZH_SIZE nS = 0;
      ZH_WCHAR wc;

      nMax = 0;
      while( ZH_CODEPAGE_CHAR_GET( cdp, pszText, nSize, &nS, &wc ) )
      {
         wc = ZH_CODEPAGE_CHAR_LOWER( cdp, wc );
         if( ! ZH_CODEPAGE_CHAR_PUT( cdp, pBuffer, nBuffLen, &nMax, wc ) )
            break;
      }
   }

   if( nMax < nBuffLen )
      pBuffer[ nMax ] = '\0';

   return nMax;
}

ZH_WCHAR zh_cdpUpperWC( PZH_CODEPAGE cdp, ZH_WCHAR wc )
{
   if( ! cdp )
      return ZH_TOUPPER( wc );
   else if( ! ZH_CODEPAGE_ISCUSTOM( cdp ) || ! cdp->wcharUpper )
   {
      if( cdp->uniTable->uniTrans == NULL )
         zh_cdpBuildTransTable( cdp->uniTable );

      if( wc <= cdp->uniTable->wcMax &&
          cdp->uniTable->uniTrans[ wc ] )
      {
         wc = cdp->uniTable->uniCodes[ cdp->upper[ cdp->uniTable->uniTrans[ wc ] ] ];
      }
      return wc;
   }
   else
      return ZH_CODEPAGE_CHAR_UPPER( cdp, wc );
}

/* functions operating on character indexes */
ZH_SIZE zh_cdpTextLen( PZH_CODEPAGE cdp, const char * pText, ZH_SIZE nSize )
{
   if( cdp && ZH_CODEPAGE_ISCUSTOM( cdp ) )
   {
      ZH_SIZE nPos = 0, nIndex = 0;
      ZH_WCHAR wc;

      while( ZH_CODEPAGE_CHAR_GET( cdp, pText, nSize, &nPos, &wc ) )
         ++nIndex;

      return nIndex;
   }
   return nSize;
}

ZH_SIZE zh_cdpTextPos( PZH_CODEPAGE cdp, const char * pText, ZH_SIZE nSize, ZH_SIZE nIndex )
{
   if( nIndex > 0 )
   {
      if( cdp && ZH_CODEPAGE_ISCUSTOM( cdp ) )
      {
         ZH_SIZE nPos = 0;
         ZH_WCHAR wc;
         do
         {
            if( ! ZH_CODEPAGE_CHAR_GET( cdp, pText, nSize, &nPos, &wc ) )
               break;
         }
         while( --nIndex );

         return nPos;
      }
      else
         return nIndex >= nSize ? nSize : nIndex;
   }
   return 0;
}

ZH_SIZE zh_cdpTextPosEx( PZH_CODEPAGE cdp, const char * pText, ZH_SIZE nSize, ZH_SIZE * pnIndex )
{
   ZH_SIZE nIndex = *pnIndex;

   if( nIndex > 0 )
   {
      if( cdp && ZH_CODEPAGE_ISCUSTOM( cdp ) )
      {
         ZH_SIZE nPos = 0;
         ZH_WCHAR wc;

         do
         {
            if( ! ZH_CODEPAGE_CHAR_GET( cdp, pText, nSize, &nPos, &wc ) )
               break;
         }
         while( --nIndex );
         *pnIndex = nIndex;
         return nPos;
      }
      else if( nIndex > nSize )
      {
         *pnIndex -= nSize;
         return nSize;
      }
      else
      {
         *pnIndex = 0;
         return nIndex;
      }
   }
   *pnIndex = 0;
   return 0;
}

ZH_WCHAR zh_cdpTextGetU16( PZH_CODEPAGE cdp, const char * szText, ZH_SIZE nLen )
{
   ZH_WCHAR wc = 0;

   if( szText && nLen > 0 )
   {
      if( cdp )
      {
         if( ZH_CODEPAGE_ISCUSTOM( cdp ) )
         {
            ZH_SIZE n = 0;
            if( ! ZH_CODEPAGE_CHAR_GET( cdp, szText, nLen, &n, &wc ) )
               wc = 0;
         }
         else
            wc = cdp->uniTable->uniCodes[ ( ZH_UCHAR ) *szText ];
      }
      else
         wc = ( ZH_UCHAR ) *szText;
   }

   return wc;
}

ZH_SIZE zh_cdpTextPutU16( PZH_CODEPAGE cdp, char * szText, ZH_SIZE nSize, ZH_WCHAR wc )
{
   ZH_SIZE nLen = 0;

   if( szText && nSize > 0 )
   {
      if( cdp )
      {
         if( ZH_CODEPAGE_ISCUSTOM( cdp ) )
            ZH_CODEPAGE_CHAR_PUT( cdp, szText, nSize, &nLen, wc );
         else
         {
            if( cdp->uniTable->uniTrans == NULL )
               zh_cdpBuildTransTable( cdp->uniTable );

            if( wc <= cdp->uniTable->wcMax )
            {
               ZH_UCHAR uc = cdp->uniTable->uniTrans[ wc ];
               if( uc )
                  szText[ nLen++ ] = uc;
            }
         }
      }
      else
         szText[ nLen++ ] = ( char ) wc;

      if( nLen < nSize )
         szText[ nLen ] = '\0';
   }

   return nLen;
}

ZH_BOOL zh_cdpCharEq( PZH_CODEPAGE cdp, const char * szText1, ZH_SIZE nLen1, ZH_SIZE * pnPos1,
                      const char * szText2, ZH_SIZE nLen2, ZH_SIZE * pnPos2 )
{
   if( *pnPos1 < nLen1 && *pnPos2 < nLen2 )
   {
      if( cdp && ZH_CODEPAGE_ISCUSTOM( cdp ) )
      {
         ZH_WCHAR wc1, wc2;
         return ZH_CODEPAGE_CHAR_GET( cdp, szText1, nLen1, pnPos1, &wc1 ) &&
                ZH_CODEPAGE_CHAR_GET( cdp, szText2, nLen2, pnPos2, &wc2 ) &&
                wc1 == wc2;
      }
      else
         return szText1[ ( *pnPos1 )++ ] == szText2[ ( *pnPos2 )++ ];
   }
   return ZH_FALSE;
}

ZH_BOOL zh_cdpCharCaseEq( PZH_CODEPAGE cdp, const char * szText1, ZH_SIZE nLen1, ZH_SIZE * pnPos1,
                          const char * szText2, ZH_SIZE nLen2, ZH_SIZE * pnPos2 )
{
   if( *pnPos1 < nLen1 && *pnPos2 < nLen2 )
   {
      if( ! cdp )
      {
         ZH_UCHAR uc1 = szText1[ ( *pnPos1 )++ ],
                  uc2 = szText2[ ( *pnPos2 )++ ];
         return ZH_TOUPPER( uc1 ) == ZH_TOUPPER( uc2 );
      }
      else if( ! ZH_CODEPAGE_ISCUSTOM( cdp ) || ! cdp->wcharUpper )
      {
         ZH_UCHAR uc1 = szText1[ ( *pnPos1 )++ ],
                  uc2 = szText2[ ( *pnPos2 )++ ];
         return cdp->upper[ uc1 ] == cdp->upper[ uc2 ];
      }
      else
      {
         ZH_WCHAR wc1, wc2;
         if( ZH_CODEPAGE_CHAR_GET( cdp, szText1, nLen1, pnPos1, &wc1 ) &&
             ZH_CODEPAGE_CHAR_GET( cdp, szText2, nLen2, pnPos2, &wc2 ) )
         {
            return wc1 == wc2 ||
                   ZH_CODEPAGE_CHAR_UPPER( cdp, wc1 ) == ZH_CODEPAGE_CHAR_UPPER( cdp, wc2 );
         }
      }
   }
   return ZH_FALSE;
}

/*
 * CP management
 */
static ZH_UCHAR zh_cdpUtf8Char( const char ** pStrPtr, PZH_UNITABLE uniTable )
{
   const char * pszString = *pStrPtr;
   ZH_UCHAR uc = 0;
   ZH_WCHAR wc = 0;
   int n = 0;

   while( *pszString )
   {
      if( ! zh_cdpUTF8ToU16NextChar( ( ZH_UCHAR ) *pszString++, &n, &wc ) )
         break;
      if( n == 0 )
      {
         if( wc < 127 )
            uc = ( ZH_UCHAR ) wc;
         else
         {
            for( n = 0; n < 256; ++n )
            {
               if( wc == uniTable->uniCodes[ n ] )
               {
                  uc = ( ZH_UCHAR ) n;
                  break;
               }
            }
         }
         break;
      }
   }
   if( uc == 0 )
   {
      while( *pszString )
         pszString++;
   }
   *pStrPtr = ( const char * ) pszString;

   return uc;
}

#define _ZH_CODEPAGE_GETUC( p )  ( ! fUtf8 ? ( ZH_UCHAR ) ( *( p ) ? *( p )++ : 0 ) \
                                      : zh_cdpUtf8Char( &( p ), uniTable ) )

static PZH_CODEPAGE zh_buildCodePage( const char * id, const char * info,
                                      PZH_UNITABLE uniTable,
                                      const char * pszUpper,
                                      const char * pszLower,
                                      unsigned int nACSort,
                                      unsigned int nCaseSort,
                                      ZH_BOOL fUtf8 )
{
   ZH_BOOL lSort, fError;
   int iMulti, iAcc, iAccUp, iAccLo, iSortUp, iSortLo, i;
   const char * pup, * plo;
   ZH_UCHAR ucUp, ucLo, ucUp2, ucLo2;
   ZH_SIZE nSize, ul;
   ZH_UCHAR * buffer, * flags, * upper, * lower, * sort, * acc;
   ZH_UCHAR used[ 256 ];
   PZH_CODEPAGE cdp;
   PZH_MULTICHAR multi;

   memset( used, '\0', sizeof( used ) );

   iMulti = iAcc = iSortUp = iSortLo = 0;
   fError = lSort = ZH_FALSE;

   ucUp2 = ucLo2 = 0;
   pup = pszUpper;
   plo = pszLower;
   while( *pup || *plo )
   {
      ucUp = _ZH_CODEPAGE_GETUC( pup );
      ucLo = _ZH_CODEPAGE_GETUC( plo );
      if( ucUp == 0 || ucLo == 0 ||
          ( ucUp == '.' && ucLo != '.' ) ||
          ( ucUp == '~' && ucLo != '~' ) )
      {
         fError = ZH_TRUE;
         break;
      }

      if( ucUp == '.' )
      {
         ZH_UCHAR ucU1, ucU2, ucL1, ucL2;

         ucU1 = _ZH_CODEPAGE_GETUC( pup );
         ucU2 = _ZH_CODEPAGE_GETUC( pup );
         ucL1 = _ZH_CODEPAGE_GETUC( plo );
         ucL2 = _ZH_CODEPAGE_GETUC( plo );

         if( ucU1 && ucU2 && ( *pup == '.' || *pup == '=' ) &&
             ucL1 && ucL2 && ( *plo == '.' || *plo == '=' ) )
         {
            if( ( ucU1 != ' ' || ucL1 != ' ' ) &&
                ( ucU1 == ucU2 || ( ucU1 != ' ' && ucU2 != ' ' ) ) &&
                ( ucL1 == ucL2 || ( ucL1 != ' ' && ucL2 != ' ' ) ) )
            {
               if( ucU1 != ' ' )
                  ++iSortLo;

               if( *pup == '=' )
               {
                  do
                  {
                     ++pup;
                  }
                  while( ZH_ISXDIGIT( *pup ) );
               }
               if( *plo == '=' )
               {
                  do
                  {
                     ++plo;
                  }
                  while( ZH_ISXDIGIT( *plo ) );
               }
               if( *pup == '.' && *plo == '.' )
               {
                  lSort = ZH_TRUE;
                  iMulti++;
                  pup++;
                  plo++;
                  continue;
               }
            }
         }
         fError = ZH_TRUE;
         break;
      }
      if( ucUp == '~' )
      {
         ucUp = _ZH_CODEPAGE_GETUC( pup );
         ucLo = _ZH_CODEPAGE_GETUC( plo );
         if( ucUp == '\0' || ucLo == '\0' )
         {
            fError = ZH_TRUE;
            break;
         }
         ++iAcc;
      }
      if( used[ ucUp ] != 0 )
         ucUp = ' ';
      if( used[ ucLo ] != 0 )
         ucLo = ' ';
      if( ucUp == ' ' && ucLo == ' ' )
      {
         fError = ZH_TRUE;
         break;
      }
      if( ucUp != ' ' )
      {
         used[ ucUp ] = 1;
         if( ucUp < ucUp2 )
            lSort = ZH_TRUE;
         ucUp2 = ucUp;
         ++iSortLo;
      }
      if( ucLo != ' ' )
      {
         used[ ucLo ] = 1;
         if( ucLo < ucLo2 )
            lSort = ZH_TRUE;
         ucLo2 = ucLo;
      }
   }

   if( iMulti > 64 )
      fError = ZH_TRUE;

   if( fError || nACSort > ZH_CODEPAGE_ACSORT_INTERLEAVED ||
       nCaseSort > ZH_CODEPAGE_CSSORT_IGNORE )
   {
#ifdef __ZH_IGNORE_CP_ERRORS
      fprintf( stderr, "Ziher CP (%s) initialization failure (1)\n", id ); fflush( stderr );
      return NULL;
#else
      zh_errInternal( 9994, "Ziher CP (%s) initialization failure", id, NULL );
#endif
   }

   if( iAcc == 0 )
      nACSort = ZH_CODEPAGE_ACSORT_NONE;
   else if( nACSort != ZH_CODEPAGE_ACSORT_NONE )
      lSort = ZH_TRUE;

   if( nCaseSort != ZH_CODEPAGE_CSSORT_UPLO )
      lSort = ZH_TRUE;

   nSize = 0x300;
   if( lSort )
   {
      nSize += 0x100;
      if( nACSort == ZH_CODEPAGE_ACSORT_INTERLEAVED )
         nSize += 0x100;
   }
   ul = nSize;
   nSize += sizeof( ZH_CODEPAGE );
   if( iMulti )
      nSize += iMulti * sizeof( ZH_MULTICHAR );

   buffer = ( ZH_UCHAR * ) zh_xgrabz( nSize );
   cdp = ( PZH_CODEPAGE ) &buffer[ ul ];
   cdp->buffer = buffer;

   cdp->flags = flags = buffer;
   buffer += 0x100;
   cdp->upper = upper = buffer;
   buffer += 0x100;
   cdp->lower = lower = buffer;
   buffer += 0x100;
   sort = acc = NULL;
   if( lSort )
   {
      cdp->sort = sort = buffer;
      buffer += 0x100;
      if( nACSort == ZH_CODEPAGE_ACSORT_INTERLEAVED )
      {
         cdp->acc = acc = buffer;
         buffer += 0x100;
      }
   }
   if( iMulti )
      cdp->multi = ( PZH_MULTICHAR ) &buffer[ sizeof( ZH_CODEPAGE ) ];

   cdp->id = id;
   cdp->info = info;
   cdp->uniTable = uniTable;
   cdp->nACSort = nACSort;
   cdp->nMulti = iMulti;
   for( i = 0; i < 0x100; ++i )
   {
      if( ZH_ISDIGIT( i ) )
         flags[ i ] |= ZH_CODEPAGE_DIGIT;
      if( ZH_ISALPHA( i ) )
         flags[ i ] |= ZH_CODEPAGE_ALPHA;
      if( ZH_ISUPPER( i ) )
         flags[ i ] |= ZH_CODEPAGE_UPPER;
      if( ZH_ISLOWER( i ) )
         flags[ i ] |= ZH_CODEPAGE_LOWER;
      upper[ i ] = ( ZH_UCHAR ) ZH_TOUPPER( i );
      lower[ i ] = ( ZH_UCHAR ) ZH_TOLOWER( i );
   }

   iAccUp = iAccLo = 0;
   multi = cdp->multi;
   pup = pszUpper;
   plo = pszLower;
   ucUp2 = ucLo2 = 255;
   memset( used, '\0', sizeof( used ) );
   while( *pup )
   {
      ucUp = _ZH_CODEPAGE_GETUC( pup );
      ucLo = _ZH_CODEPAGE_GETUC( plo );
      if( ucUp == '.' )
      {
         multi->cFirst[ 0 ] = _ZH_CODEPAGE_GETUC( pup );
         multi->cLast[ 0 ]  = _ZH_CODEPAGE_GETUC( pup );
         multi->cFirst[ 1 ] = _ZH_CODEPAGE_GETUC( plo );
         multi->cLast[ 1 ]  = _ZH_CODEPAGE_GETUC( plo );
         if( multi->cFirst[ 0 ] != ' ' )
         {
            flags[ ( ZH_UCHAR ) multi->cFirst[ 0 ] ] |= ZH_CODEPAGE_MULTI1;
            flags[ ( ZH_UCHAR ) multi->cLast[ 0 ] ]  |= ZH_CODEPAGE_MULTI2;
            multi->sortUp = ++iSortUp - iAccUp;
         }
         if( multi->cFirst[ 1 ] != ' ' )
         {
            flags[ ( ZH_UCHAR ) multi->cFirst[ 1 ] ] |= ZH_CODEPAGE_MULTI1;
            flags[ ( ZH_UCHAR ) multi->cLast[ 1 ] ]  |= ZH_CODEPAGE_MULTI2;

            if( nCaseSort == ZH_CODEPAGE_CSSORT_UPLO )
               ++iSortLo;
            else if( nCaseSort == ZH_CODEPAGE_CSSORT_MIXED )
               iSortLo = ++iSortUp;
            else
               iSortLo = iSortUp;
            multi->sortLo = iSortLo - iAccLo;
         }
         if( *pup == '=' )
         {
            ++pup;
            while( ZH_ISXDIGIT( *pup ) )
            {
               multi->wcUp = ( multi->wcUp << 4 ) |
                             ( *pup >= 'a' ? ( *pup - 'a' + 10 ) :
                               ( *pup >= 'A' ? ( *pup - 'A' + 10 ) :
                                               ( *pup - '0' ) ) );
               ++pup;
            }
         }
         pup++;
         if( *plo == '=' )
         {
            ++plo;
            while( ZH_ISXDIGIT( *plo ) )
            {
               multi->wcLo = ( multi->wcLo << 4 ) |
                             ( *plo >= 'a' ? ( *plo - 'a' + 10 ) :
                               ( *plo >= 'A' ? ( *plo - 'A' + 10 ) :
                                               ( *plo - '0' ) ) );
               ++plo;
            }
         }
         plo++;
         if( multi->wcUp || multi->wcLo )
            cdp->nMultiUC++;
         multi++;
      }
      else
      {
         iAcc = 0;
         if( ucUp == '~' )
         {
            iAcc = 1;
            ucUp = _ZH_CODEPAGE_GETUC( pup );
            ucLo = _ZH_CODEPAGE_GETUC( plo );
         }
         if( ucUp != ' ' )
         {
            flags[ ucUp ] |= ZH_CODEPAGE_ALPHA;
            flags[ ucUp ] |= ZH_CODEPAGE_UPPER;
            if( ucLo != ' ' && ( used[ ucUp ] & ZH_CODEPAGE_UPPER ) == 0 )
            {
               lower[ ucUp ] = ucLo;
               used[ ucUp ] |= ZH_CODEPAGE_UPPER;
            }
            if( sort )
            {
               if( sort[ ucUp ] == 0 )
               {
                  if( iAcc && nACSort != ZH_CODEPAGE_ACSORT_NONE )
                     ++iAccUp;
                  sort[ ucUp ] = ( ZH_UCHAR ) ( ++iSortUp - iAccUp );
                  if( acc )
                     acc[ ucUp ] = ( ZH_UCHAR ) iSortUp;
                  if( ucUp2 > ucUp )
                     ucUp2 = ucUp;
               }
            }
         }
         if( ucLo != ' ' )
         {
            flags[ ucLo ] |= ZH_CODEPAGE_ALPHA;
            flags[ ucLo ] |= ZH_CODEPAGE_LOWER;
            if( ucUp != ' ' && ( used[ ucLo ] & ZH_CODEPAGE_LOWER ) == 0 )
            {
               upper[ ucLo ] = ucUp;
               used[ ucLo ] |= ZH_CODEPAGE_LOWER;
            }
            if( sort )
            {
               if( sort[ ucLo ] == 0 )
               {
                  if( nCaseSort == ZH_CODEPAGE_CSSORT_UPLO )
                  {
                     if( iAcc && nACSort != ZH_CODEPAGE_ACSORT_NONE )
                        ++iAccLo;
                     ++iSortLo;
                  }
                  else
                  {
                     if( nCaseSort == ZH_CODEPAGE_CSSORT_MIXED )
                     {
                        if( iAcc && nACSort != ZH_CODEPAGE_ACSORT_NONE )
                           ++iAccUp;
                        ++iSortUp;
                     }
                     iAccLo = iAccUp;
                     iSortLo = iSortUp;
                  }
                  sort[ ucLo ] = ( ZH_UCHAR ) ( iSortLo - iAccLo );
                  if( acc )
                     acc[ ucLo ] = ( ZH_UCHAR ) iSortLo;
                  if( ucLo2 > ucLo )
                     ucLo2 = ucLo;
               }
            }
         }
      }
   }

   if( sort )
   {
      int iUp, iLo, iSort1, iSort2, iSort3, iAdd;

      if( iMulti > 0 )
      {
         if( iMulti > ucUp2 || iMulti > ucLo2 )
         {
#ifdef __ZH_IGNORE_CP_ERRORS
            fprintf( stderr, "Ziher CP (%s) initialization failure (2)\n", id ); fflush( stderr );
            zh_xfree( buffer );
            return NULL;
#else
            zh_errInternal( 9994, "Ziher CP (%s) initialization failure", id, NULL );
#endif
         }

         if( iMulti <= 32 )
            iMulti = 33;
         else
            iMulti = 65;
      }
      else
         iMulti = 1;

      if( nCaseSort != ZH_CODEPAGE_CSSORT_UPLO )
         ucLo2 = 0;

      for( iUp = iLo = 0, i = iMulti; i < 256; ++i )
      {
         if( sort[ i ] == 0 )
         {
            if( i < ( int ) ucUp2 )
               ++iUp;
            else if( i < ( int ) ucLo2 )
               ++iLo;
         }
      }
      for( iSort1 = iSort2 = iSort3 = 0, i = iMulti; i < 256; ++i )
      {
         if( sort[ i ] == 0 )
         {
            if( i < ( int ) ucUp2 )
               iAdd = ++iSort1;
            else if( i < ( int ) ucLo2 )
               iAdd = ++iSort2 + iSortUp + iUp;
            else
               iAdd = ++iSort3 + iUp + iSortLo + iLo;
         }
         else if( sort[ i ] <= iSortUp )
            iAdd = iUp;
         else
            iAdd = iUp + iLo;

         sort[ i ] += ( ZH_UCHAR ) iAdd;
         if( acc )
            acc[ i ] += ( ZH_UCHAR ) iAdd;
      }
      for( i = 0; i < cdp->nMulti; ++i )
      {
         cdp->multi[ i ].sortUp += iUp;
         cdp->multi[ i ].sortLo += iUp + iLo;
      }
   }

   if( cdp->nMultiUC )
   {
      cdp->wcharGet = zh_cdpMulti_get;
      cdp->wcharPut = zh_cdpMulti_put;
      cdp->wcharLen = zh_cdpMulti_len;
      cdp->type |= ZH_CODEPAGE_TYPE_CUSTOM;
   }
   else
   {
      cdp->wcharGet = zh_cdpStd_get;
      cdp->wcharPut = zh_cdpStd_put;
      cdp->wcharLen = zh_cdpStd_len;
   }

   if( cdp->sort == NULL )
   {
      cdp->wcharCmp = zh_cdpBin_cmp;
      cdp->wcharCmpI = zh_cdpBin_cmpi;
      cdp->type |= ZH_CODEPAGE_TYPE_BINSORT;
   }
   else if( cdp->nMulti )
   {
      cdp->wcharCmp = zh_cdpMulti_cmp;
      cdp->wcharCmpI = zh_cdpMulti_cmpi;
   }
   else
   {
      cdp->wcharCmp = zh_cdpStd_cmp;
      cdp->wcharCmpI = zh_cdpStd_cmpi;
   }

   return cdp;
}

static PZH_CODEPAGE * zh_cdpFindPos( const char * id )
{
   PZH_CODEPAGE * cdp_ptr;

   if( s_cdpList == NULL )
   {
      ZH_UCHAR * flags, * upper, * lower;
      int i;

      memset( s_en_buffer, '\0', 0x300 );
      s_en_codepage.flags = flags = ( ZH_UCHAR * ) s_en_buffer;
      s_en_codepage.upper = upper = ( ZH_UCHAR * ) s_en_buffer + 0x100;
      s_en_codepage.lower = lower = ( ZH_UCHAR * ) s_en_buffer + 0x200;
      for( i = 0; i < 0x100; ++i )
      {
         if( ZH_ISDIGIT( i ) )
            flags[ i ] |= ZH_CODEPAGE_DIGIT;
         if( ZH_ISALPHA( i ) )
            flags[ i ] |= ZH_CODEPAGE_ALPHA;
         if( ZH_ISUPPER( i ) )
            flags[ i ] |= ZH_CODEPAGE_UPPER;
         if( ZH_ISLOWER( i ) )
            flags[ i ] |= ZH_CODEPAGE_LOWER;
         upper[ i ] = ( ZH_UCHAR ) ZH_TOUPPER( i );
         lower[ i ] = ( ZH_UCHAR ) ZH_TOLOWER( i );
      }
      s_utf8_codepage.flags = s_en_codepage.flags;
      s_utf8_codepage.upper = s_en_codepage.upper;
      s_utf8_codepage.lower = s_en_codepage.lower;
      s_utf8_codepage.next  = NULL;
      s_en_codepage.next = &s_utf8_codepage;
      s_cdpList = &s_en_codepage;
   }

   cdp_ptr = &s_cdpList;

   while( *cdp_ptr )
   {
      if( strcmp( ( *cdp_ptr )->id, id ) == 0 )
         break;
      if( zh_stricmp( ( *cdp_ptr )->uniTable->uniID, id ) == 0 )
         break;
      cdp_ptr = &( *cdp_ptr )->next;
   }

   return cdp_ptr;
}

ZH_BOOL zh_cdpRegisterRaw( PZH_CODEPAGE cdp )
{
   PZH_CODEPAGE * cdp_ptr;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_cdpRegisterRaw(%p)", ( void * ) cdp ) );

   cdp_ptr = zh_cdpFindPos( cdp->id );
   if( *cdp_ptr == NULL )
   {
      if( ! ZH_CODEPAGE_ISCUSTOM( cdp ) )
      {
         cdp->wcharGet = zh_cdpStd_get;
         cdp->wcharPut = zh_cdpStd_put;
         cdp->wcharLen = zh_cdpStd_len;
      }
      if( cdp->wcharCmp == NULL )
         cdp->wcharCmp = cdp->sort == NULL ? zh_cdpBin_cmp :
                         ( cdp->nMulti ? zh_cdpMulti_cmp : zh_cdpStd_cmp );
      if( cdp->wcharCmpI == NULL )
         cdp->wcharCmpI = cdp->sort == NULL ? zh_cdpBin_cmpi :
                          ( cdp->nMulti ? zh_cdpMulti_cmpi : zh_cdpStd_cmpi );

      if( cdp->wcharCmp == zh_cdpBin_cmp && cdp->wcharCmpI == zh_cdpBin_cmpi )
         cdp->type |= ZH_CODEPAGE_TYPE_BINSORT;

      *cdp_ptr = cdp;
      return ZH_TRUE;
   }
   return ZH_FALSE;
}

ZH_BOOL zh_cdpRegisterNew( const char * id, const char * info,
                           PZH_UNITABLE uniTable,
                           const char * pszUpper, const char * pszLower,
                           unsigned int nACSort,
                           unsigned int nCaseSort,
                           ZH_BOOL fUtf8 )
{
   PZH_CODEPAGE * cdp_ptr;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_cdpRegisterNew(%s,%s,%s,%s,%u,%u,%d)", id, info, pszUpper, pszLower, nACSort, nCaseSort, fUtf8 ) );

   cdp_ptr = zh_cdpFindPos( id );
   if( *cdp_ptr == NULL )
   {
      *cdp_ptr = zh_buildCodePage( id, info, uniTable, pszUpper, pszLower,
                                   nACSort, nCaseSort, fUtf8 );
      return *cdp_ptr != NULL;
   }
   return ZH_FALSE;
}

void zh_cdpReleaseAll( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_cdpReleaseAll()" ) );

   while( s_cdpList )
   {
      void * buffer = s_cdpList->buffer;
      if( s_cdpList->uniTable->uniTrans )
      {
         zh_xfree( s_cdpList->uniTable->uniTrans );
         s_cdpList->uniTable->uniTrans = NULL;
      }
      s_cdpList = s_cdpList->next;
      if( buffer )
         zh_xfree( buffer );
   }
   if( s_rev_ctrl != NULL )
   {
      zh_xfree( s_rev_ctrl );
      s_rev_ctrl = NULL;
   }
}

PZH_CODEPAGE zh_cdpFind( const char * id )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_cdpFind(%s)", id ) );

   return id ? *zh_cdpFindPos( id ) : NULL;
}

PZH_CODEPAGE zh_cdpFindExt( const char * id )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_cdpFindExt(%s)", id ) );

   if( id )
   {
      PZH_CODEPAGE cdp = *zh_cdpFindPos( id );

      if( cdp )
         return cdp;

      zh_errRT_BASE( EG_ARG, 1302, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
   }
   return NULL;
}

ZH_BOOL zh_cdpIsUTF8( PZH_CODEPAGE cdp )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_cdpIsUTF8(%p)", ( void * ) cdp ) );

   if( cdp == NULL )
      cdp = zh_vmCodepage();

   return ZH_CODEPAGE_ISUTF8( cdp );
}

PZH_CODEPAGE codepageSelect( PZH_CODEPAGE cdp )
{
   PZH_CODEPAGE cdpOld;

   ZH_TRACE( ZH_TR_DEBUG, ( "codepageSelect(%p)", ( void * ) cdp ) );

   cdpOld = zh_vmCodepage();
   if( cdp )
      zh_vmSetCDP( cdp );

   return cdpOld;
}

const char * zh_cdpID( void )
{
   PZH_CODEPAGE cdp;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_cdpID()" ) );

   cdp = zh_vmCodepage();

   return cdp ? cdp->id : NULL;
}

const char * codepageSelectID( const char * id )
{
   PZH_CODEPAGE cdp;

   ZH_TRACE( ZH_TR_DEBUG, ( "codepageSelectID(%s)", id ) );

   cdp = codepageSelect( zh_cdpFindExt( id ) );

   return cdp ? cdp->id : NULL;
}

/* Caller must release the pointer */
const char ** zh_codepageList( void )
{
   PZH_CODEPAGE cdp;
   int iCount, iPos;
   const char ** list;

   cdp = s_cdpList;
   iCount = 0;
   while( cdp )
   {
      ++iCount;
      cdp = cdp->next;
   }

   list = ( const char ** ) zh_xgrab( ( iCount + 1 ) * sizeof( char * ) );

   cdp = s_cdpList;
   iPos = 0;
   while( cdp && iPos < iCount )
   {
      list[ iPos++ ] = cdp->id;
      cdp = cdp->next;
   }
   list[ iPos ] = NULL;

   return list;
}
