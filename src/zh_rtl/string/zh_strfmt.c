/*
 * zh_StrFormat() function
 *
 * Copyright 2008 Mindaugas Kavaliauskas <dbtopas at dbtopas.lt>
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
#include "zh_item_api.h"
#include "zh_error_api.h"

typedef struct
{
   char *  pData;
   ZH_SIZE nLen;
   ZH_SIZE nMax;
} BUFFERTYPE;

static void bufadd( BUFFERTYPE * pBuf, const char * pAdd, ZH_SIZE nLen )
{
   if( pBuf->nLen + nLen >= pBuf->nMax )
   {
      pBuf->nMax += ( pBuf->nMax >> 1 ) + nLen;
      pBuf->pData = ( char * ) zh_xrealloc( pBuf->pData, pBuf->nMax );
   }
   memcpy( pBuf->pData + pBuf->nLen, pAdd, nLen );
   pBuf->nLen += nLen;
   pBuf->pData[ pBuf->nLen ] = '\0';
}

static void zh_itemHexStr( PZH_ITEM pItem, char * pStr, ZH_BOOL fUpper )
{
   ZH_MAXUINT nValue, nTmp;
   int iLen;

   nValue = nTmp = zh_itemGetNInt( pItem );

   iLen = 0;
   do
   {
      ++iLen;
      nTmp >>= 4;
   }
   while( nTmp );

   pStr[ iLen ] = '\0';
   do
   {
      int iDigit = ( int ) ( nValue & 0x0F );
      pStr[ --iLen ] = ( char ) ( iDigit + ( iDigit < 10 ? '0' :
                                             ( fUpper ? 'A' : 'a' ) - 10 ) );
      nValue >>= 4;
   }
   while( iLen );
}

PZH_ITEM zh_strFormat( PZH_ITEM pItemReturn, PZH_ITEM pItemFormat, int iCount, PZH_ITEM * pItemArray )
{
   BUFFERTYPE  buffer;
   PZH_ITEM    pItem;
   const char  *pFmt, *pFmtEnd, *pFmtSave;
   int         i, iParam, iParamNo, iWidth, iDec;
   ZH_SIZE     nSize;
   ZH_BOOL     fLeftAlign, fForceSign, fPadZero, fSpaceSign, fSign;

   pFmt = zh_itemGetCPtr( pItemFormat );
   nSize = zh_itemGetCLen( pItemFormat );
   pFmtEnd = pFmt + nSize;

   buffer.nMax = nSize + 16;
   buffer.nLen = 0;
   buffer.pData = ( char * ) zh_xgrab( buffer.nMax );
   buffer.pData[ 0 ] = '\0';

   iParam = 0;
   while( pFmt < pFmtEnd )
   {
      if( *pFmt != '%' )
      {
         bufadd( &buffer, pFmt++, 1 );
         continue;
      }

      pFmtSave = pFmt++;

      if( *pFmt == '%' )
      {
         bufadd( &buffer, pFmt++, 1 );
         continue;
      }

      iWidth = iDec = -1;
      fLeftAlign = fForceSign = fPadZero = fSpaceSign = 0;

      /* parse parameter number */
      iParamNo = 0;
      while( ZH_ISDIGIT( *pFmt ) )
         iParamNo = iParamNo * 10 + *pFmt++ - '0';

      if( iParamNo > 0 && *pFmt == '$' )
      {
         pFmt++;
      }
      else
      {
         iParamNo = -1;
         pFmt = pFmtSave + 1;
      }

      /* Parse flags */
      do
      {
         switch( *pFmt )
         {
            case '-':
               fLeftAlign = 1;
               continue;
            case '+':
               fForceSign = 1;
               continue;
            case ' ':
               fSpaceSign = 1;
               continue;
            case '0':
               fPadZero = 1;
               continue;
         }
         break;
      }
      while( *++pFmt );

      /* Parse width */
      if( ZH_ISDIGIT( *pFmt ) )
      {
         iWidth = 0;
         while( ZH_ISDIGIT( *pFmt ) )
            iWidth = iWidth * 10 + *pFmt++ - '0';
      }

      /* Parse decimals */
      if( *pFmt == '.' )
      {
         pFmt++;
         iDec = 0;
         if( ZH_ISDIGIT( *pFmt ) )
         {
            while( ZH_ISDIGIT( *pFmt ) )
               iDec = iDec * 10 + *pFmt++ - '0';
         }
      }

      /* Parse specifier */
      if( *pFmt == 'c' || *pFmt == 'd' || *pFmt == 'x' || *pFmt == 'X' ||
          *pFmt == 'f' || *pFmt == 's' )
      {
         if( iParamNo == -1 )
            iParamNo = ++iParam;

         pItem = ( iParamNo > iCount ) ? NULL : pItemArray[ iParamNo - 1 ];
      }
      else
         pItem = NULL;

      switch( *pFmt )
      {
         case 'c':
         {
            char  buf[ 1 ];

            buf[ 0 ] = ( char ) zh_itemGetNI( pItem );
            if( fLeftAlign )
            {
               bufadd( &buffer, buf, 1 );
            }
            if( iWidth > 1 )
            {
               for( i = 1; i < iWidth; i++ )
                  bufadd( &buffer, " ", 1 );
            }
            if( ! fLeftAlign )
            {
               bufadd( &buffer, buf, 1 );
            }
            break;
         }

         case 'd':
         case 'x':
         case 'X':
         {
            char  * pStr = NULL;
            const char * pStr2;
            int   iSize, iExtra;

            fSign = 0;
            if( pItem && ZH_IS_NUMERIC( pItem ) )
            {
               iSize = sizeof( ZH_MAXINT ) * 3 + 1;
               pStr2 = pStr = ( char * ) zh_xgrab( iSize + 1 );
               if( *pFmt == 'd' )
                  zh_itemStrBuf( pStr, pItem, iSize, 0 );
               else
                  zh_itemHexStr( pItem, pStr, *pFmt == 'X' );

               while( *pStr2 == ' ' )
                  pStr2++;
               iSize = ( int ) strlen( pStr2 );
               if( *pStr2 == '-' )
               {
                  fSign = 1;
                  iSize--;
                  pStr2++;
               }
            }
            else if( pItem && ZH_IS_LOGICAL( pItem ) )
            {
               iSize = 1;
               if( zh_itemGetL( pItem ) )
                  pStr2 = "1";
               else
                  pStr2 = "0";
            }
            else
            {
               iSize = 1;
               pStr2 = "0";
            }

            iExtra = 0;
            if( fForceSign || fSpaceSign || fSign )
               iExtra = 1;

            /* If decimals is set, zero padding flag is ignored */
            if( iDec >= 0 )
               fPadZero = 0;

            if( fLeftAlign )
            {
               /* Zero padding is ignored on left Align */
               /* ForceSign has priority over SpaceSign */
               if( fSign )
                  bufadd( &buffer, "-", 1 );
               else if( fForceSign )
                  bufadd( &buffer, "+", 1 );
               else if( fSpaceSign )
                  bufadd( &buffer, " ", 1 );

               for( i = iSize; i < iDec; i++ )
                  bufadd( &buffer, "0", 1 );

               bufadd( &buffer, pStr2, ( ZH_SIZE ) iSize );
               if( iDec > iSize )
                  iSize = iDec;
               for( i = iSize + iExtra; i < iWidth; i++ )
                  bufadd( &buffer, " ", 1 );
            }
            else
            {
               /* Right align */
               if( fPadZero )
               {
                  /* ForceSign has priority over SpaceSign */
                  if( fSign )
                     bufadd( &buffer, "-", 1 );
                  else if( fForceSign )
                     bufadd( &buffer, "+", 1 );
                  else if( fSpaceSign )
                     bufadd( &buffer, " ", 1 );

                  for( i = iSize + iExtra; i < iWidth; i++ )
                     bufadd( &buffer, "0", 1 );

                  bufadd( &buffer, pStr2, strlen( pStr2 ) );
               }
               else
               {
                  for( i = ( iSize > iDec ? iSize : iDec ) + iExtra; i < iWidth; i++ )
                     bufadd( &buffer, " ", 1 );

                  /* ForceSign has priority over SpaceSign */
                  if( fSign )
                     bufadd( &buffer, "-", 1 );
                  else if( fForceSign )
                     bufadd( &buffer, "+", 1 );
                  else if( fSpaceSign )
                     bufadd( &buffer, " ", 1 );

                  for( i = iSize; i < iDec; i++ )
                     bufadd( &buffer, "0", 1 );

                  bufadd( &buffer, pStr2, ( ZH_SIZE ) iSize );
               }
            }

            if( pStr )
               zh_xfree( pStr );
            break;
         }

         case 'f':
         {
            char  * pStr = NULL;
            const char * pStr2;
            int   iSize, iExtra, iD;

            if( pItem && ZH_IS_NUMERIC( pItem ) )
            {
               zh_itemGetNLen( pItem, &iSize, &iD );

               if( iDec != -1 )
               {
                  iSize += iDec - iD + 1;
                  iD = iDec;
               }

               /* Let 255 be a limit for number length */
               if( iSize > 255 )
                  iSize = 255;
               if( iD > 253 )
                  iD = 253;
               if( iSize < iD + 2 )
                  iSize = iD + 2;

               pStr2 = pStr = ( char * ) zh_xgrab( iSize + 1 );
               zh_itemStrBuf( pStr, pItem, iSize, iD );

               if( pStr[ 0 ] == '*' && iSize < 255 )
               {
                  pStr2 = pStr = ( char * ) zh_xrealloc( pStr, 256 );
                  zh_itemStrBuf( pStr, pItem, 255, iD );
               }
               while( *pStr2 == ' ' )
                  pStr2++;
               iSize = ( int ) strlen( pStr2 );
            }
            else
            {
               iSize = 1;
               pStr2 = "0";
            }

            iExtra = 0;
            if( ( fForceSign || fSpaceSign ) && *pStr2 != '-' )
               iExtra = 1;

            if( fLeftAlign )
            {
               /* Zero padding is ignored on left Align */
               if( *pStr2 != '-' )
               {
                  /* ForceSign has priority over SpaceSign */
                  if( fForceSign )
                     bufadd( &buffer, "+", 1 );
                  else
                  {
                     if( fSpaceSign )
                        bufadd( &buffer, " ", 1 );
                  }
               }
               bufadd( &buffer, pStr2, ( ZH_SIZE ) iSize );
               for( i = iSize + iExtra; i < iWidth; i++ )
                  bufadd( &buffer, " ", 1 );
            }
            else
            {
               /* Right align */
               if( fPadZero )
               {
                  if( *pStr2 == '-' )
                  {
                     bufadd( &buffer, pStr2++, 1 );
                  }
                  else
                  {
                     /* ForceSign has priority over SpaceSign */
                     if( fForceSign )
                        bufadd( &buffer, "+", 1 );
                     else
                     {
                        if( fSpaceSign )
                           bufadd( &buffer, " ", 1 );
                     }
                  }
                  for( i = iSize + iExtra; i < iWidth; i++ )
                     bufadd( &buffer, "0", 1 );

                  bufadd( &buffer, pStr2, strlen( pStr2 ) );
               }
               else
               {
                  for( i = iSize + iExtra; i < iWidth; i++ )
                     bufadd( &buffer, " ", 1 );

                  if( *pStr2 != '-' )
                  {
                     /* ForceSign has priority over SpaceSign */
                     if( fForceSign )
                        bufadd( &buffer, "+", 1 );
                     else
                     {
                        if( fSpaceSign )
                           bufadd( &buffer, " ", 1 );
                     }
                  }
                  bufadd( &buffer, pStr2, ( ZH_SIZE ) iSize );
               }
            }

            if( pStr )
               zh_xfree( pStr );
            break;
         }

         case 's':
         {
            const char * pStr = zh_itemGetCPtr( pItem );

            nSize = zh_itemGetCLen( pItem );
            if( iDec >= 0 )
            {
               if( ( ZH_SIZE ) iDec < nSize )
                  nSize = iDec;
            }
            if( fLeftAlign )
               bufadd( &buffer, pStr, nSize );

            if( iWidth > 1 )
            {
               for( i = ( int ) nSize; i < iWidth; i++ )
                  bufadd( &buffer, " ", 1 );
            }

            if( ! fLeftAlign )
               bufadd( &buffer, pStr, nSize );

            break;
         }

         default:
         {
            bufadd( &buffer, pFmtSave, pFmt - pFmtSave );
            continue;
         }
      }
      pFmt++;
   }

   pItemReturn = zh_itemPutCL( pItemReturn, buffer.pData, buffer.nLen );
   zh_xfree( buffer.pData );
   return pItemReturn;
}

ZH_FUNC( ZH_STRFORMAT )
{
   PZH_ITEM pFormat = zh_param( 1, ZH_IT_STRING );

   if( pFormat )
   {
      int        iParams = zh_pcount();
      PZH_ITEM * pItemArray = NULL;

      if( iParams > 1 )
      {
         int i;
         pItemArray = ( PZH_ITEM * ) zh_xgrab( ( iParams - 1 ) * sizeof( PZH_ITEM ) );
         for( i = 1; i < iParams; i++ )
            pItemArray[ i - 1 ] = zh_param( i + 1, ZH_IT_ANY );
      }

      zh_itemReturnRelease( zh_strFormat( NULL, pFormat, iParams - 1, pItemArray ) );

      if( iParams > 1 )
         zh_xfree( pItemArray );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1099, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}
