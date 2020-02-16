/*
 * zh_NumToHex(), zh_HexToNum()
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
#include "zh_api_error.h"

ZH_FUNC( ZH_HEXTONUM )
{
   const char * szHex = zh_parc( 1 );

   if( szHex )
   {
      ZH_MAXUINT nNum = 0;

      while( *szHex == ' ' )
         szHex++;
      while( *szHex )
      {
         int iDigit;
         char c = *szHex++;
         if( c >= '0' && c <= '9' )
            iDigit = c - '0';
         else if( c >= 'A' && c <= 'F' )
            iDigit = c - ( 'A' - 10 );
         else if( c >= 'a' && c <= 'f' )
            iDigit = c - ( 'a' - 10 );
         else
         {
            nNum = 0;
            break;
         }
         nNum = ( nNum << 4 ) + iDigit;
      }
      zh_retnint( nNum );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_NUMTOHEX )
{
   ZH_MAXUINT nNum;
   int        iLen;
   ZH_BOOL    fDefaultLen;
   char       ret[ 33 ];

   if( ZH_IS_PARAM_NUM( 2 ) )
   {
      iLen = zh_parni( 2 );
      iLen = ( iLen < 1 ) ? 1 : ( ( iLen > 32 ) ? 32 : iLen );
      fDefaultLen = 0;
   }
   else
   {
      iLen = 32;
      fDefaultLen = 1;
   }

   if( ZH_IS_PARAM_NUM( 1 ) )
      nNum = zh_parnint( 1 );
   else if( ZH_ISPOINTER( 1 ) )
      nNum = zh_vmInternalsEnabled() ?
         ( ZH_PTRUINT ) zh_parptr( 1 ) :
         ( ZH_PTRUINT ) ( zh_parptr( 1 ) ? -1 : 0 );
   else
   {
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
      return;
   }

   ret[ iLen ] = '\0';
   do
   {
      int iDigit = ( int ) ( nNum & 0x0F );
      ret[ --iLen ] = ( char ) ( iDigit + ( iDigit < 10 ? '0' : 'A' - 10 ) );
      nNum >>= 4;
   }
   while( fDefaultLen ? nNum != 0 : iLen != 0 );

   zh_retc( &ret[ iLen ] );
}

ZH_FUNC( ZH_STRTOHEX )
{
   const char * szStr = zh_parc( 1 ), * szSep = "";
   ZH_SIZE nStr, nSep = 0;

   if( zh_pcount() > 1 )
   {
      szSep = zh_parc( 2 );
      nSep = zh_parclen( 2 );
   }

   if( ! szStr || ! szSep )
   {
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
      return;
   }

   nStr = zh_parclen( 1 );
   if( nStr )
   {
      ZH_SIZE nDest = ( nStr << 1 ) + ( nStr - 1 ) * nSep;
      char * szDest, * szPtr;

      szPtr = szDest = ( char * ) zh_xgrab( nDest + 1 );
      do
      {
         ZH_UCHAR uc = ( ZH_UCHAR ) *szStr++, ud;
         ud = uc >> 4;
         *szPtr++ = ud + ( ud < 10 ? '0' : 'A' - 10 );
         ud = uc & 0x0F;
         *szPtr++ = ud + ( ud < 10 ? '0' : 'A' - 10 );
         if( --nStr && nSep )
         {
            memcpy( szPtr, szSep, nSep );
            szPtr += nSep;
         }
      }
      while( nStr );
      zh_retclen_buffer( szDest, nDest );
   }
   else
      zh_retc_null();
}

ZH_FUNC( ZH_HEXTOSTR )
{
   const char * szStr = zh_parc( 1 );
   ZH_SIZE nStr;

   if( ! szStr )
   {
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
      return;
   }

   nStr = zh_parclen( 1 );
   if( nStr > 1 )
   {
      ZH_SIZE nDest, ul;
      const char * szPtr;

      szPtr = szStr;
      ul = nStr;
      nDest = 0;
      do
      {
         char c = *szPtr++;
         if( ( c >= '0' && c <= '9' ) ||
             ( c >= 'A' && c <= 'F' ) ||
             ( c >= 'a' && c <= 'f' ) )
            ++nDest;
      }
      while( --ul );

      nDest >>= 1;
      if( nDest )
      {
         int iVal = 0x10;

         char * szDest = ( char * ) zh_xgrab( nDest + 1 );

         /* ul = 0; see above stop condition */
         do
         {
            char c = *szStr++;
            if( c >= '0' && c <= '9' )
               iVal += c - '0';
            else if( c >= 'A' && c <= 'F' )
               iVal += c - ( 'A' - 10 );
            else if( c >= 'a' && c <= 'f' )
               iVal += c - ( 'a' - 10 );
            else
               continue;

            if( iVal & 0x100 )
            {
               szDest[ ul++ ] = ( char ) iVal & 0xff;
               iVal = 0x1;
            }
            iVal <<= 4;
         }
         while( --nStr );

         zh_retclen_buffer( szDest, nDest );
         return;
      }
   }

   zh_retc_null();
}
