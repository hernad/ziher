/*
 * *Trim() functions
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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
#include "zh_api_error.h"

/* trims from the left, and returns a new pointer to szText */
/* also returns the new length in lLen */
const char * zh_strLTrim( const char * szText, ZH_SIZE * nLen )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_strLTrim(%s, %p)", szText, ( void * ) nLen ) );

   while( *nLen && ZH_ISSPACE( *szText ) )
   {
      szText++;
      ( *nLen )--;
   }

   return szText;
}

/* return length of szText ignoring trailing white space (or true spaces) */
ZH_SIZE zh_strRTrimLen( const char * szText, ZH_SIZE nLen, ZH_BOOL bAnySpace )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_strRTrimLen(%s, %lu. %d)", szText, nLen, ( int ) bAnySpace ) );

   if( bAnySpace )
   {
      while( nLen && ZH_ISSPACE( szText[ nLen - 1 ] ) )
         nLen--;
   }
   else
   {
      while( nLen && szText[ nLen - 1 ] == ' ' )
         nLen--;
   }

   return nLen;
}

/* trims leading spaces from a string */

ZH_FUNC( LTRIM )
{
   PZH_ITEM pText = zh_param( 1, ZH_IT_STRING );

   if( pText )
   {
      ZH_SIZE nLen, nSrc;
      const char * szText;

      nLen = nSrc = zh_itemGetCLen( pText );
      szText = zh_strLTrim( zh_itemGetCPtr( pText ), &nLen );

      if( nLen == nSrc )
         zh_itemReturn( pText );
      else
         zh_retclen( szText, nLen );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1101, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/* trims trailing spaces from a string */

/* NOTE: The second parameter is a Ziher extension. */

ZH_FUNC( RTRIM )
{
   PZH_ITEM pText = zh_param( 1, ZH_IT_STRING );

   if( pText )
   {
      ZH_SIZE nLen, nSrc;
      const char * szText = zh_itemGetCPtr( pText );

      nSrc = zh_itemGetCLen( pText );
      nLen = zh_strRTrimLen( szText, nSrc, ZH_FALSE );

      if( nLen == nSrc )
         zh_itemReturn( pText );
      else
         zh_retclen( szText, nLen );
   }
   else
      /* NOTE: "TRIM" is correct here [vszakats] */
      zh_errRT_BASE_SubstR( EG_ARG, 1100, NULL, "TRIM", ZH_ERR_ARGS_BASEPARAMS );
}

/* synonymn for RTRIM */
ZH_FUNC_TRANSLATE( TRIM, RTRIM )

/* trims leading and trailing spaces from a string */

/* NOTE: The second parameter is a Ziher extension. */

ZH_FUNC( ALLTRIM )
{
   PZH_ITEM pText = zh_param( 1, ZH_IT_STRING );

   if( pText )
   {
      ZH_SIZE nLen, nSrc;
      const char * szText = zh_itemGetCPtr( pText );

      nSrc = zh_itemGetCLen( pText );
      nLen = zh_strRTrimLen( szText, nSrc, ZH_FALSE );
      szText = zh_strLTrim( szText, &nLen );

      if( nLen == nSrc )
         zh_itemReturn( pText );
      else
         zh_retclen( szText, nLen );
   }
   else
      zh_retc_null();
}
