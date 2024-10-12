/*
 * memo line functions: MemoLine(), MLCount(), MLPos(), MLCToPos(), MPosToLC()
 *
 * Copyright 2012 Przemyslaw Czerpak
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
#include "zh_codepage_api.h"
#include "zh_set.h"
#include "zh_vm.h"

#define ZH_EOL_BUFFER_SIZE    4

typedef struct
{
   const char *   szEOL;
   ZH_SIZE        nLen;
} ZH_EOL_INFO, * PZH_EOL_INFO;

typedef struct
{
   const char *   pszString;
   ZH_SIZE        nLen;
   ZH_SIZE        nLineLength;
   ZH_SIZE        nTabSize;
   ZH_BOOL        fWordWrap;
   int            iEOLs;
   PZH_EOL_INFO   pEOLs;
   PZH_CODEPAGE   cdp;

   ZH_SIZE        nOffset;
   ZH_SIZE        nMaxCol;
   ZH_SIZE        nMaxPos;
   ZH_SIZE        nCol;

   ZH_EOL_INFO    EOL_buffer[ ZH_EOL_BUFFER_SIZE ];
}
ZH_MLC_INFO, * PZH_MLC_INFO;


static void zh_mlGetEOLs( PZH_MLC_INFO pMLC, int iParam )
{
   int iEOLs = 0;
   ZH_SIZE nLen;

   pMLC->pEOLs = pMLC->EOL_buffer;

   nLen = zh_parclen( iParam );
   if( nLen )
   {
      pMLC->pEOLs[ 0 ].szEOL = zh_parc( iParam );
      pMLC->pEOLs[ 0 ].nLen = nLen;
      iEOLs = 1;
   }
   else if( ZH_ISARRAY( iParam ) )
   {
      PZH_ITEM pArray = zh_param( iParam, ZH_IT_ARRAY );
      ZH_SIZE nSize = zh_arrayLen( pArray ), n;

      for( n = 1; n <= nSize; ++n )
      {
         if( zh_arrayGetCLen( pArray, n ) > 0 )
            ++iEOLs;
      }
      if( iEOLs )
      {
         if( iEOLs > ZH_EOL_BUFFER_SIZE )
            pMLC->pEOLs = ( PZH_EOL_INFO ) zh_xgrab( sizeof( ZH_EOL_INFO ) * iEOLs );
         iEOLs = 0;
         for( n = 1; n <= nSize; ++n )
         {
            nLen = zh_arrayGetCLen( pArray, n );
            if( nLen > 0 )
            {
               pMLC->pEOLs[ iEOLs ].szEOL = zh_arrayGetCPtr( pArray, n );
               pMLC->pEOLs[ iEOLs ].nLen = nLen;
               ++iEOLs;
            }
         }
      }
   }

   if( iEOLs == 0 )
   {
      pMLC->pEOLs[ 0 ].szEOL = zh_setGetEOL();
      if( ! pMLC->pEOLs[ 0 ].szEOL || ! pMLC->pEOLs[ 0 ].szEOL[ 0 ] )
         pMLC->pEOLs[ 0 ].szEOL = zh_conNewLine();
      pMLC->pEOLs[ 0 ].nLen = strlen( pMLC->pEOLs[ 0 ].szEOL );
      iEOLs = pMLC->pEOLs[ 0 ].nLen ? 1 : 0;
   }

   pMLC->iEOLs = iEOLs;
}

static ZH_BOOL zh_mlInit( PZH_MLC_INFO pMLC, int iParAdd )
{
   ZH_I_SIZE nSize = zh_parnsdef( 2, 79 );

   pMLC->pszString = zh_parc( 1 );
   if( pMLC->pszString && nSize > 0 )
   {
      pMLC->nOffset = pMLC->nMaxCol = pMLC->nMaxPos = pMLC->nCol = 0;

      pMLC->nLineLength = nSize;
      pMLC->nLen = zh_parclen( 1 );

      pMLC->nTabSize = zh_parnsdef( 3 + iParAdd, 4 );
      pMLC->fWordWrap = zh_parldef( 4 + iParAdd, ZH_TRUE );

      if( pMLC->nTabSize >= pMLC->nLineLength )
         pMLC->nTabSize = pMLC->nLineLength - 1;
      if( pMLC->nTabSize == 0 )
         pMLC->nTabSize = 1;

      pMLC->cdp = zh_vmCodepage();
      if( ! ZH_CODEPAGE_ISCHARIDX( pMLC->cdp ) )
         pMLC->cdp = NULL;

      zh_mlGetEOLs( pMLC, 5 + iParAdd );
      return ZH_TRUE;
   }

   return ZH_FALSE;
}

static void zh_mlExit( PZH_MLC_INFO pMLC )
{
   if( pMLC->iEOLs > ZH_EOL_BUFFER_SIZE )
      zh_xfree( pMLC->pEOLs );
}

static int zh_mlEol( PZH_MLC_INFO pMLC )
{
   const char * pszString = pMLC->pszString + pMLC->nOffset;
   ZH_SIZE nLen = pMLC->nLen - pMLC->nOffset;
   PZH_EOL_INFO pEOLs = pMLC->pEOLs;
   int i;

   for( i = 0; i < pMLC->iEOLs; ++i )
   {
      if( pszString[ 0 ] == pEOLs[ i ].szEOL[ 0 ] &&
          ( pEOLs[ i ].nLen == 1 ||
            ( nLen >= pEOLs[ i ].nLen &&
              memcmp( pszString, pEOLs[ i ].szEOL, pEOLs[ i ].nLen ) == 0 ) ) )
         return i;
   }
   return -1;
}

static ZH_SIZE zh_mlGetLine( PZH_MLC_INFO pMLC )
{
   ZH_SIZE nBlankCol = 0, nBlankPos = 0, nLastCol = 0, nLastPos;
   int i;

   pMLC->nCol = 0;

   if( pMLC->nOffset >= pMLC->nLen )
      return ZH_FALSE;

   while( pMLC->nOffset < pMLC->nLen )
   {
      ZH_WCHAR ch;

      if( pMLC->pszString[ pMLC->nOffset ] == ZH_CHAR_SOFT1 &&
          pMLC->pszString[ pMLC->nOffset + 1 ] == ZH_CHAR_SOFT2 )
      {
         if( pMLC->nMaxCol && pMLC->nCol )
            break;
         if( pMLC->nOffset < pMLC->nMaxPos )
            nLastCol = pMLC->nCol;
         pMLC->nOffset += 2;
         if( ! pMLC->fWordWrap )
            break;
         else if( nBlankPos + 2 == pMLC->nOffset )
            nBlankPos += 2;
         continue;
      }

      i = zh_mlEol( pMLC );
      if( i >= 0 )
      {
         if( pMLC->nOffset < pMLC->nMaxPos )
            nLastCol = pMLC->nCol;
         if( pMLC->nMaxCol == 0 )
            pMLC->nOffset += pMLC->pEOLs[ i ].nLen;
         break;
      }
      else if( ! pMLC->fWordWrap && pMLC->nCol >= pMLC->nLineLength )
         break;

      nLastPos = pMLC->nOffset;
      if( pMLC->cdp )
      {
         if( ! ZH_CODEPAGE_CHAR_GET( pMLC->cdp, pMLC->pszString, pMLC->nLen, &pMLC->nOffset, &ch ) )
            break;
      }
      else
         ch = pMLC->pszString[ pMLC->nOffset++ ];

      if( pMLC->nOffset <= pMLC->nMaxPos )
         nLastCol = pMLC->nCol;
      pMLC->nCol += ch == ZH_CHAR_HT ?
                    pMLC->nTabSize - ( pMLC->nCol % pMLC->nTabSize ) : 1;

      if( pMLC->nMaxCol && pMLC->nCol >= pMLC->nMaxCol )
      {
         if( pMLC->nCol > pMLC->nMaxCol )
            pMLC->nOffset = nLastPos;
         break;
      }
      else if( pMLC->nCol > pMLC->nLineLength )
      {
         if( pMLC->fWordWrap )
         {
            if( ch == ' ' || ch == ZH_CHAR_HT )
               break;
            else if( nBlankCol != 0 )
            {
               pMLC->nCol = nBlankCol;
               pMLC->nOffset = nBlankPos;
            }
            else
               pMLC->nOffset = nLastPos;
         }
         else
            pMLC->nOffset = nLastPos;
         break;
      }
      if( pMLC->nCol > 1 && ( ch == ' ' || ch == ZH_CHAR_HT ) )
      {
         nBlankCol = pMLC->nCol;
         nBlankPos = pMLC->nOffset;
      }
   }

   if( pMLC->nMaxPos && pMLC->nCol > nLastCol )
      pMLC->nCol = nLastCol;
   else if( pMLC->nCol > pMLC->nLineLength )
      pMLC->nCol = pMLC->nLineLength;

   return ZH_TRUE;
}


/* MemoLine( <cString>, [ <nLineLength>=79 ],
 *           [ <nLineNumber>=1 ],
 *           [ <nTabSize>=4 ], [ <lWrap>=.T. ],
 *           [ <cEOL>|<acEOLs> ],
 *           [ <lPad>=.T. ] ) --> <cLine>
 *
 * NOTE: <lPad> is undocumented parameter and will be removed and
 *       replaced by other solution in the future.
 */
ZH_FUNC( MEMOLINE )
{
   ZH_I_SIZE nLine = zh_parnsdef( 3, 1 );
   char * szLine = NULL;
   ZH_SIZE nLen = 0;

   if( nLine >= 1 )
   {
      ZH_MLC_INFO MLC;

      if( zh_mlInit( &MLC, 1 ) )
      {
         while( --nLine )
         {
            if( ! zh_mlGetLine( &MLC ) )
               break;
         }

         if( nLine == 0 )
         {
            ZH_BOOL fPad = zh_parldef( 7, ZH_TRUE );
            ZH_SIZE nIndex, nSize, nCol;

            nIndex = MLC.nOffset;

            zh_mlGetLine( &MLC );

            if( MLC.cdp )
               nSize = ( MLC.nOffset - nIndex ) + MLC.nLineLength;
            else
               nSize = MLC.nLineLength;
            szLine = ( char * ) zh_xgrab( nSize + 1 );
            nCol = 0;
            while( nIndex < MLC.nLen && nCol < MLC.nCol )
            {
               if( MLC.pszString[ nIndex ] == ZH_CHAR_SOFT1 &&
                   MLC.pszString[ nIndex + 1 ] == ZH_CHAR_SOFT2 )
                  nIndex += 2 ;
               else
               {
                  ZH_WCHAR wc;

                  if( MLC.cdp )
                  {
                     if( ! ZH_CODEPAGE_CHAR_GET( MLC.cdp, MLC.pszString, MLC.nLen, &nIndex, &wc ) )
                        break;
                  }
                  else
                     wc = MLC.pszString[ nIndex++ ];

                  if( wc == ZH_CHAR_HT )
                  {
                     ZH_SIZE n = MLC.nTabSize - ( nLen % MLC.nTabSize );
                     do
                     {
                        szLine[ nLen++ ] = ' ';
                     }
                     while( ++nCol < MLC.nCol && --n );
                  }
                  else
                  {
                     if( MLC.cdp )
                     {
                        if( ! ZH_CODEPAGE_CHAR_PUT( MLC.cdp, szLine, nSize, &nLen, wc ) )
                           break;
                     }
                     else
                        szLine[ nLen++ ] = ( char ) wc;
                     ++nCol;
                  }
               }
            }
            if( nCol < MLC.nLineLength )
            {
               nCol = MLC.nLineLength - nCol;
               if( nCol > nSize - nLen )
                  nCol = nSize - nLen;
               if( ! fPad && nCol > 0 )
                  nCol = nIndex < MLC.nLen &&
                         ( MLC.pszString[ nIndex ] == ' ' ||
                           MLC.pszString[ nIndex ] == ZH_CHAR_HT ) ? 1 : 0;
               if( nCol > 0 )
               {
                  memset( szLine + nLen, ' ', nCol );
                  nLen += nCol;
               }
            }
         }
         zh_mlExit( &MLC );
      }
   }
   if( szLine == NULL )
      zh_retc_null();
   else
      zh_retclen_buffer( szLine, nLen );
}

/* MLCount( <cString>, [ <nLineLength>=79 ],
 *          [ <nTabSize>=4 ], [ <lWrap>=.T. ],
 *          [ <cEOL>|<acEOLs> ] ) --> <nLines>
 */
ZH_FUNC( MLCOUNT )
{
   ZH_MLC_INFO MLC;
   ZH_SIZE nLines  = 0;

   if( zh_mlInit( &MLC, 0 ) )
   {
      while( zh_mlGetLine( &MLC ) )
         ++nLines;
      zh_mlExit( &MLC );
   }
   zh_retns( nLines );
}

/* MLPos( <cString>, [ <nLineLength>=79 ],
 *        [ <nLineNumber>=1 ],
 *        [ <nTabSize>=4 ], [ <lWrap>=.T. ],
 *        [ <cEOL>|<acEOLs> ] ) --> <nLinePos>
 */
ZH_FUNC( MLPOS )
{
   ZH_MLC_INFO MLC;
   ZH_I_SIZE nLine = zh_parnsdef( 3, 1 );
   ZH_SIZE nOffset = 0;

   if( nLine >= 1 )
   {
      if( zh_mlInit( &MLC, 1 ) )
      {
         while( --nLine )
         {
            if( ! zh_mlGetLine( &MLC ) )
               break;
         }
         if( nLine <= 1 )
         {
            nOffset = MLC.nOffset;
            if( MLC.cdp )
               nOffset = zh_cdpTextLen( MLC.cdp, MLC.pszString, nOffset );
            if( MLC.nOffset < MLC.nLen )
               ++nOffset;
         }
         zh_mlExit( &MLC );
      }
   }
   zh_retns( nOffset );
}

/* MLCToPos( <cString>, [ <nLineLength>=79 ],
 *           [ <nLine>=1 ], [ <nCol>=0 ],
 *           [ <nTabSize>=4 ], [ <lWrap>=.T. ],
 *           [ <cEOL>|<acEOLs> ] ) --> <nPos>
 */
ZH_FUNC( MLCTOPOS )
{
   ZH_SIZE nLine   = zh_parns( 3 );
   ZH_SIZE nCol    = zh_parns( 4 );
   ZH_SIZE nOffset = 0;

   if( nLine > 0 && ZH_IS_PARAM_NUM( 4 ) )
   {
      ZH_MLC_INFO MLC;
      if( zh_mlInit( &MLC, 2 ) )
      {
         if( MLC.nLineLength > 4 )
         {
            while( --nLine )
            {
               if( ! zh_mlGetLine( &MLC ) )
                  break;
            }
            if( nCol && nLine == 0 )
            {
               MLC.nMaxCol = nCol;
               MLC.nLineLength = nCol;
               zh_mlGetLine( &MLC );
            }
            nOffset = MLC.nOffset;
            if( MLC.cdp )
               nOffset = zh_cdpTextLen( MLC.cdp, MLC.pszString, nOffset );
         }
         zh_mlExit( &MLC );
      }
   }
   zh_retns( nOffset + 1 );
}

/* MPosToLC( <cString>, [ <nLineLength>=79 ],
 *           [ <nPos>=1 ],
 *           [ <nTabSize>=4 ], [ <lWrap>=.T. ],
 *           [ <cEOL>|<acEOLs> ] ) --> <aLineCol>
 */
ZH_FUNC( MPOSTOLC )
{
   ZH_I_SIZE nPos    = zh_parns( 3 );
   ZH_SIZE nLine   = 0;
   ZH_SIZE nCol    = 0;

   if( nPos > 0 )
   {
      ZH_MLC_INFO MLC;
      if( zh_mlInit( &MLC, 1 ) )
      {
         if( MLC.cdp )
         {
            ZH_SIZE nRest = nPos;
            nPos = zh_cdpTextPosEx( MLC.cdp, MLC.pszString, MLC.nLen, &nRest );
            nPos += nRest;
         }
         MLC.nMaxPos = nPos;
         if( MLC.nMaxPos <= MLC.nLen + 1 )
         {
            for( ;; )
            {
               ZH_SIZE nOffset = MLC.nOffset;
               zh_mlGetLine( &MLC );
               nCol = MLC.nCol;
               ++nLine;
               if( MLC.nOffset == nOffset || MLC.nOffset >= MLC.nMaxPos )
                  break;
            }
         }
         zh_mlExit( &MLC );
      }
   }
   zh_reta( 2 );
   zh_storvns( nLine, -1, 1 );
   zh_storvns( nCol, -1, 2 );
}

/* zh_MLEval( <cString>, <bCode>, [ <nLineLength>=79 ],
 *            [ <nTabSize>=4 ], [ <lWrap>=.T. ],
 *            [ <nPos> ], [ @<nRow> ], [ @<nCol> ] ) --> <nLines>
 */
ZH_FUNC( ZH_MLEVAL )
{
   const char * pszString = zh_parc( 1 );
   PZH_ITEM pBlock = zh_param( 2, ZH_IT_EVALITEM );
   ZH_I_SIZE nSize = zh_parnsdef( 3, 79 );
   ZH_SIZE nRowPos = 0, nColPos = 0, nLines = 0;

   if( pszString && pBlock && nSize > 0 )
   {
      ZH_SIZE nOffset = 0;
      ZH_SIZE nLineLength = nSize;
      ZH_SIZE nLen = zh_parclen( 1 );
      ZH_SIZE nTabSize = zh_parnsdef( 4, 4 );
      ZH_SIZE nPos = zh_parns( 6 ) - 1;
      ZH_BOOL fWordWrap = zh_parldef( 5, ZH_TRUE );
      PZH_CODEPAGE cdp = zh_vmCodepage();
      PZH_ITEM pLineItem = NULL, pSoftItem = NULL;
      ZH_BOOL fSoftCR, fEOL;
      char * pszLine;

      if( ! ZH_CODEPAGE_ISCHARIDX( cdp ) )
         cdp = NULL;

      if( nLineLength > 0xFFFF )
         nLineLength = 0xFFFF;
      if( nTabSize >= nLineLength )
         nTabSize = nLineLength - 1;
      if( nTabSize == 0 )
         nTabSize = 1;

      pszLine = ( char * ) zh_xgrab( nLineLength + 1 );

      do
      {
         ZH_SIZE nBlankCol = 0, nBlankPos = 0, nBlankDst = 0, nCol = 0, nDst = 0;

         fSoftCR = fEOL = ZH_FALSE;
         ++nLines;
         while( ! fSoftCR && nOffset < nLen )
         {
            ZH_SIZE nRepl;
            ZH_WCHAR ch;

            if( pszString[ nOffset ] == ZH_CHAR_SOFT1 &&
                pszString[ nOffset + 1 ] == ZH_CHAR_SOFT2 )
            {
               nOffset += 2;
               if( fWordWrap )
                  continue;
               break;
            }
            else if( pszString[ nOffset ] == ZH_CHAR_CR )
            {
               ++nOffset;
               if( pszString[ nOffset ] == ZH_CHAR_LF )
                  ++nOffset;
               fEOL = ZH_TRUE;
               break;
            }
            else if( pszString[ nOffset ] == ZH_CHAR_LF )
            {
               ++nOffset;
               if( pszString[ nOffset ] == ZH_CHAR_CR )
                  ++nOffset;
               fEOL = ZH_TRUE;
               break;
            }

            if( cdp )
            {
               if( ! ZH_CODEPAGE_CHAR_GET( cdp, pszString, nLen, &nOffset, &ch ) )
                  continue;
               if( ! ZH_CODEPAGE_CHAR_PUT( cdp, pszLine, nLineLength + 1, &nDst, ch ) )
                  break;
            }
            else
               ch = pszLine[ nDst++ ] = pszString[ nOffset++ ];

            if( nRowPos == 0 && nOffset > nPos )
            {
               nRowPos = nLines;
               nColPos = nCol;
            }

            if( ch == ' ' || ch == ZH_CHAR_HT )
            {
               nBlankPos = nOffset;
               nBlankCol = nCol;
               nBlankDst = nDst;
            }

            nRepl = ch == ZH_CHAR_HT ? nTabSize - ( nCol % nTabSize ) -1 : 0;
            for( ;; )
            {
               if( ++nCol >= nLineLength )
               {
                  if( fWordWrap && nBlankCol > 0 )
                  {
                     nOffset = nBlankPos;
                     nCol = nBlankCol;
                     nDst = nBlankDst;
                     if( nOffset <= nPos )
                        nRowPos = nColPos = 0;
                  }
                  fSoftCR = ZH_TRUE;
                  break;
               }
               if( nRepl-- == 0 )
                  break;
               if( !cdp )
                  pszLine[ nDst++ ] = ( char ) ch;
               else if( ! ZH_CODEPAGE_CHAR_PUT( cdp, pszLine, nLineLength + 1, &nDst, ch ) )
                  break;
            }
         }

         if( nRowPos == 0 && nOffset >= nPos && ! fSoftCR )
         {
            nRowPos = nLines;
            nColPos = nCol;
         }

         pLineItem = zh_itemPutCL( pLineItem, pszLine, nDst );
         pSoftItem = zh_itemPutL( pSoftItem, fSoftCR );
         zh_vmEvalBlockV( pBlock, 2, pLineItem, pSoftItem );
      }
      while( nOffset < nLen && zh_vmRequestQuery() == 0 );

      if( fSoftCR || fEOL )
      {
         ++nLines;
         pLineItem = zh_itemPutC( pLineItem, NULL );
         pSoftItem = zh_itemPutL( pSoftItem, ZH_FALSE );
         zh_vmEvalBlockV( pBlock, 2, pLineItem, pSoftItem );
      }

      if( nRowPos == 0 && nOffset >= nPos )
      {
         nRowPos = nLines;
         nColPos = 0;
      }

      zh_itemRelease( pLineItem );
      zh_itemRelease( pSoftItem );
      zh_xfree( pszLine );
   }

   if( zh_vmRequestQuery() == 0 )
   {
      zh_storns( nRowPos, 7 );
      zh_storns( nColPos, 8 );
      zh_retns( nLines );
   }
}
