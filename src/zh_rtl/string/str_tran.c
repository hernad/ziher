/*
 * StrTran() function
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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
#include "zh_item_api.h"
#include "zh_error_api.h"

ZH_FUNC( STRTRAN )
{
   PZH_ITEM pText = zh_param( 1, ZH_IT_STRING );
   PZH_ITEM pSeek = zh_param( 2, ZH_IT_STRING );

   if( pText && pSeek )
   {
      ZH_SIZE nStart, nCount;

      nStart = zh_parnsdef( 4, 1 );
      nCount = zh_parnsdef( 5, -1 );

      if( nStart && nCount )
      {
         ZH_SIZE nText = zh_itemGetCLen( pText );
         ZH_SIZE nSeek = zh_itemGetCLen( pSeek );

         if( nSeek && nSeek <= nText && nStart > 0 )
         {
            PZH_ITEM pReplace = zh_param( 3, ZH_IT_STRING );
            ZH_SIZE nReplace = zh_itemGetCLen( pReplace );
            const char * szReplace = zh_itemGetCPtr( pReplace );
            const char * szText = zh_itemGetCPtr( pText );
            const char * szSeek = zh_itemGetCPtr( pSeek );
            ZH_SIZE nFound = 0;
            ZH_SIZE nReplaced = 0;
            ZH_SIZE nT = 0;
            ZH_SIZE nS = 0;

            while( nT < nText && nText - nT >= nSeek - nS )
            {
               if( szText[ nT ] == szSeek[ nS ] )
               {
                  ++nT;
                  if( ++nS == nSeek )
                  {
                     if( ++nFound >= nStart )
                     {
                        nReplaced++;
                        if( --nCount == 0 )
                           nT = nText;
                     }
                     nS = 0;
                  }
               }
               else if( nS )
               {
                  nT -= nS - 1;
                  nS = 0;
               }
               else
                  ++nT;
            }

            if( nReplaced )
            {
               ZH_SIZE nLength = nText;

               if( nSeek > nReplace )
                  nLength -= ( nSeek - nReplace ) * nReplaced;
               else
                  nLength += ( nReplace - nSeek ) * nReplaced;

               if( nLength )
               {
                  char * szResult = ( char * ) zh_xgrab( nLength + 1 );
                  char * szPtr = szResult;

                  nFound -= nReplaced;
                  nT = nS = 0;
                  do
                  {
                     if( nReplaced && szText[ nT ] == szSeek[ nS ] )
                     {
                        ++nT;
                        if( ++nS == nSeek )
                        {
                           const char * szCopy;

                           if( nFound )
                           {
                              nFound--;
                              szCopy = szSeek;
                           }
                           else
                           {
                              nReplaced--;
                              szCopy = szReplace;
                              nS = nReplace;
                           }
                           while( nS )
                           {
                              *szPtr++ = *szCopy++;
                              --nS;
                           }
                        }
                     }
                     else
                     {
                        if( nS )
                        {
                           nT -= nS;
                           nS = 0;
                        }
                        *szPtr++ = szText[ nT++ ];
                     }
                  }
                  while( nT < nText );

                  zh_retclen_buffer( szResult, nLength );
               }
               else
                  zh_retc_null();
            }
            else
               zh_itemReturn( pText );
         }
         else
            zh_itemReturn( pText );
      }
      else
         zh_retc_null();
   }
   else
   {
      zh_errRT_BASE_SubstR( EG_ARG, 1126, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
   }
}
