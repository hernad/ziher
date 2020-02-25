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
#include "zh_item_api.h"
#include "zh_error_api.h"
#include "zh_codepage_api.h"

ZH_FUNC( CODEPAGESELECT )
{
   const char * id = zh_parc( 1 );

   zh_retc( zh_cdpID() );

   if( id )
      codepageSelectID( id );
}

ZH_FUNC( CODEPAGE_EXISTS )
{
   const char * id = zh_parc( 1 );

   if( id )
      zh_retl( zh_cdpFind( id ) != NULL );
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_CDPUNIID )
{
   const char * id = zh_parc( 1 );
   PZH_CODEPAGE cdp = id ? zh_cdpFindExt( id ) : zh_vmCDP();

   zh_retc( cdp ? cdp->uniTable->uniID : NULL );
}

ZH_FUNC( ZH_CDPINFO )
{
   const char * id = zh_parc( 1 );
   PZH_CODEPAGE cdp = id ? zh_cdpFindExt( id ) : zh_vmCDP();

   zh_retc( cdp ? cdp->info : NULL );
}

ZH_FUNC( ZH_CDPISCHARIDX )
{
   const char * id = zh_parc( 1 );
   PZH_CODEPAGE cdp = id ? zh_cdpFindExt( id ) : zh_vmCDP();
   ZH_BOOL fResult = ZH_FALSE;

   if( cdp )
   {
      fResult = ZH_CODEPAGE_ISCHARIDX( cdp );
      if( ZH_CODEPAGE_ISCUSTOM( cdp ) && ZH_ISLOG( 2 ) )
      {
         if( zh_parl( 2 ) )
            cdp->type |= ZH_CODEPAGE_TYPE_CHARIDX;
         else
            cdp->type &= ~ZH_CODEPAGE_TYPE_CHARIDX;
      }
   }
   zh_retl( fResult );
}

ZH_FUNC( ZH_CODEPAGE_CHARMAX )
{
   zh_retnl( ( 1 << ( ( int ) ( zh_cdpIsUTF8( zh_cdpFindExt( zh_parc( 1 ) ) ) ? sizeof( ZH_WCHAR ) : sizeof( ZH_UCHAR ) ) * 8 ) ) - 1 );
}

ZH_FUNC( ZH_CDPISUTF8 )
{
   zh_retl( zh_cdpIsUTF8( zh_cdpFindExt( zh_parc( 1 ) ) ) );
}

ZH_FUNC( ZH_CDPLIST )
{
   const char ** list = zh_cdpList();
   ZH_ISIZ nPos;

   nPos = 0;
   while( list[ nPos ] )
      ++nPos;

   zh_reta( nPos );

   nPos = 0;
   while( list[ nPos ] )
   {
      zh_storvc( list[ nPos ], -1, nPos + 1 );
      ++nPos;
   }

   zh_xfree( ( void * ) list );
}

/* NOTE: CA-Cl*pper 5.2e Intl. will return: "NATSORT v1.2i x14 19/Mar/93" */
/* NOTE: CA-Cl*pper 5.3  Intl. will return: "NATSORT v1.3i x19 06/Mar/95" */
ZH_FUNC_TRANSLATE( __NATSORTVER, ZH_CDPINFO )

/*
 * extended CP PRG functions
 */
ZH_FUNC( ZH_TRANSLATE )
{
   ZH_SIZE nLen = zh_parclen( 1 );
   const char * szIdIn = zh_parc( 2 );
   const char * szIdOut = zh_parc( 3 );

   if( nLen && ( szIdIn || szIdOut ) )
   {
      PZH_CODEPAGE cdpIn = szIdIn ? zh_cdpFindExt( szIdIn ) : zh_vmCDP();
      PZH_CODEPAGE cdpOut = szIdOut ? zh_cdpFindExt( szIdOut ) : zh_vmCDP();

      if( cdpIn && cdpOut && cdpIn != cdpOut &&
          ( cdpIn->uniTable != cdpOut->uniTable ||
            ZH_CODEPAGE_ISCUSTOM( cdpIn ) ||
            ZH_CODEPAGE_ISCUSTOM( cdpOut ) ) )
      {
         char * szResult = zh_cdpnDup( zh_parc( 1 ), &nLen, cdpIn, cdpOut );
         zh_retclen_buffer( szResult, nLen );
      }
      else
         zh_itemReturn( zh_param( 1, ZH_IT_STRING ) );
   }
   else
      zh_retc_null();
}

ZH_FUNC( ZH_STRTOUTF8 )
{
   ZH_SIZE nLen = zh_parclen( 1 ), nDest = 0;
   char * szDest = NULL;

   if( nLen )
   {
      const char * szCP = zh_parc( 2 );
      PZH_CODEPAGE cdp = szCP ? zh_cdpFindExt( szCP ) : zh_vmCDP();

      if( cdp )
      {
         if( zh_cdpIsUTF8( cdp ) )
         {
            zh_itemReturn( zh_param( 1, ZH_IT_STRING ) );
            return;
         }
         else
         {
            const char * szString = zh_parc( 1 );
            nDest = zh_cdpStrAsUTF8Len( cdp, szString, nLen, 0 );
            szDest = ( char * ) zh_xgrab( nDest + 1 );
            zh_cdpStrToUTF8( cdp, szString, nLen, szDest, nDest + 1 );
         }
      }
   }
   if( szDest )
      zh_retclen_buffer( szDest, nDest );
   else
      zh_retc_null();
}

ZH_FUNC( ZH_UTF8TOSTR )
{
   const char * szString = zh_parc( 1 );

   if( szString )
   {
      ZH_SIZE nLen = zh_parclen( 1 ), nDest = 0;
      char * szDest = NULL;

      if( nLen )
      {
         const char * szCP = zh_parc( 2 );
         PZH_CODEPAGE cdp = szCP ? zh_cdpFindExt( szCP ) : zh_vmCDP();

         if( cdp )
         {
            if( zh_cdpIsUTF8( cdp ) )
            {
               zh_itemReturn( zh_param( 1, ZH_IT_STRING ) );
               return;
            }
            else
            {
               szString = zh_parc( 1 );
               nDest = zh_cdpUTF8AsStrLen( cdp, szString, nLen, 0 );
               szDest = ( char * ) zh_xgrab( nDest + 1 );
               zh_cdpUTF8ToStr( cdp, szString, nLen, szDest, nDest + 1 );
            }
         }
      }

      if( szDest )
         zh_retclen_buffer( szDest, nDest );
      else
         zh_retc_null();
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}
