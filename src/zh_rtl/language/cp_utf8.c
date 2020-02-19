/*
 * Example of Ziher codepage using UTF-8 encoding
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

#define ZH_UTF8EX_SORT

#include "zh_api.h"
#include "zh_codepage_api.h"

#include "unicode16_def.h"

#ifdef ZH_UTF8EX_SORT
#  include "utf8_sort.h"
#endif

static ZH_CODEPAGE_GET_FUNC( UTF8_get )
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
   if( n != 0 )
   {
      *pnIndex = nIndex;
      return ZH_TRUE;
   }
   return ZH_FALSE;
}

static ZH_CODEPAGE_PUT_FUNC( UTF8_put )
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

static ZH_CODEPAGE_LEN_FUNC( UTF8_len )
{
   ZH_SYMBOL_UNUSED( cdp );

   return zh_cdpUTF8CharSize( wc );
}

static ZH_CODEPAGE_UPPER_FUNC( UTF8_upper )
{
   ZH_WCHAR wcUP;

   ZH_SYMBOL_UNUSED( cdp );

   wcUP = s_uc_upper( wc );
   return wcUP ? wcUP : wc;
}

static ZH_CODEPAGE_LOWER_FUNC( UTF8_lower )
{
   ZH_WCHAR wcLO;

   ZH_SYMBOL_UNUSED( cdp );

   wcLO = s_uc_lower( wc );
   return wcLO ? wcLO : wc;
}

static ZH_CODEPAGE_FLAGS_FUNC( UTF8_flags )
{
   ZH_SYMBOL_UNUSED( cdp );

   return s_uc_flags( wc );
}

static ZH_CODEPAGE_CMP_FUNC( UTF8_cmp )
{
   int iRet;

#ifdef ZH_UTF8EX_SORT

   ZH_SIZE nPos1 = 0, nPos2 = 0;
   ZH_WCHAR wc1, wc2;

   iRet = 0;
   for( ;; )
   {
      if( ! ZH_CODEPAGE_CHAR_GET( cdp, szSecond, nLenSecond, &nPos2, &wc2 ) )
      {
         if( fExact && ZH_CODEPAGE_CHAR_GET( cdp, szFirst, nLenFirst, &nPos1, &wc1 ) )
            iRet = 1;
         break;
      }
      if( ! ZH_CODEPAGE_CHAR_GET( cdp, szFirst, nLenFirst, &nPos1, &wc1 ) )
      {
         iRet = -1;
         break;
      }
      if( wc1 != wc2 )
      {
         ZH_USHORT us1 = s_uniSort[ wc1 ], us2 = s_uniSort[ wc2 ];
         if( us1 != us2 )
         {
            iRet = us1 < us2 ? -1 : 1;
            break;
         }
      }
   }

#else

   ZH_SIZE nLen = nLenFirst < nLenSecond ? nLenFirst : nLenSecond;

   ZH_SYMBOL_UNUSED( cdp );

   iRet = memcmp( szFirst, szSecond, nLen );
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
#endif

   return iRet;
}

static ZH_CODEPAGE_CMP_FUNC( UTF8_cmpi )
{
   int iRet = 0;

#ifdef ZH_UTF8EX_SORT

   ZH_SIZE nPos1 = 0, nPos2 = 0;
   ZH_WCHAR wc1, wc2;

   for( ;; )
   {
      if( ! ZH_CODEPAGE_CHAR_GET( cdp, szSecond, nLenSecond, &nPos2, &wc2 ) )
      {
         if( fExact && ZH_CODEPAGE_CHAR_GET( cdp, szFirst, nLenFirst, &nPos1, &wc1 ) )
            iRet = 1;
         break;
      }
      if( ! ZH_CODEPAGE_CHAR_GET( cdp, szFirst, nLenFirst, &nPos1, &wc1 ) )
      {
         iRet = -1;
         break;
      }
      if( wc1 != wc2 )
      {
         ZH_USHORT us1 = s_uniSort[ ZH_CODEPAGE_CHAR_UPPER( cdp, wc1 ) ],
                   us2 = s_uniSort[ ZH_CODEPAGE_CHAR_UPPER( cdp, wc2 ) ];
         if( us1 != us2 )
         {
            iRet = us1 < us2 ? -1 : 1;
            break;
         }
      }
   }

#else

   ZH_SIZE nLen = nLenFirst < nLenSecond ? nLenFirst : nLenSecond;

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
#endif

   return iRet;
}


static void zh_cp_init( PZH_CODEPAGE cdp )
{
   ZH_UCHAR * flags, * upper, * lower;
   int i;

   cdp->buffer = ( ZH_UCHAR * ) zh_xgrab( 0x300 );
   cdp->flags = flags = ( ZH_UCHAR * ) cdp->buffer;
   cdp->upper = upper = ( ZH_UCHAR * ) cdp->buffer + 0x100;
   cdp->lower = lower = ( ZH_UCHAR * ) cdp->buffer + 0x200;

   for( i = 0; i < 0x100; ++i )
   {
      flags[ i ] = 0;
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
}

#define ZH_CP_RAW

#define ZH_CP_ID              UTF8EX
#define ZH_CP_INFO            "UTF-8 extended"
#define ZH_CP_UNITB           ZH_UNITB_437

/* use character indexes instead of bytes ones */
#define ZH_CP_CHARIDX
/* Chr(), Asc() and similar functions operates on Unicode values instead of bytes */
#define ZH_CP_CHARUNI
/* UTF-8 string encoding */
#define ZH_CP_UTF8

#define ZH_CP_GET_FUNC        UTF8_get
#define ZH_CP_PUT_FUNC        UTF8_put
#define ZH_CP_LEN_FUNC        UTF8_len

#define ZH_CP_FLAGS_FUNC      UTF8_flags
#define ZH_CP_UPPER_FUNC      UTF8_upper
#define ZH_CP_LOWER_FUNC      UTF8_lower

#define ZH_CP_CMP_FUNC        UTF8_cmp
#define ZH_CP_CMPI_FUNC       UTF8_cmpi

#define s_flags               NULL
#define s_upper               NULL
#define s_lower               NULL
#define s_sort                NULL

#define ZH_CP_INIT zh_cp_init

/* include CP registration code */
#include "zh_codepage_reg.h"
