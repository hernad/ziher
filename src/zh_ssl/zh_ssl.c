/*
 * OpenSSL API (SSL) - Ziher extensions
 *
 * Copyright 2009 Viktor Szakats (vszakats.net/ziher)
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

#include "zh_ssl.h"

#include "zh_item_api.h"
#include "zh_vm.h"

ZH_FUNC( ZH_SSL_READ_ALL )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
      {
         int iMax        = zh_parnidef( 3, INT_MAX );
         int iTimeout    = zh_parnidef( 4, -1 );
         int iBufferSize = zh_parnidef( 5, 80 );

         int    iPos       = 0;
         int    iAllocated = 0;
         char * retval     = NULL;

         for( ;; )
         {
            char buffer[ 1 ];
            int  iLen;
            int  sd = SSL_get_rfd( ssl );

            if( SSL_pending( ssl ) ||
                ( sd >= 0 && zh_socketSelectRead( ( ZH_SOCKET ) sd, iTimeout ) ) )
            {
               iLen = SSL_read( ssl, buffer, 1 );

               if( iLen == SSL_ERROR_WANT_READ )
                  continue;
            }
            else
               break;

            if( iLen <= 0 )
            {
               if( retval )
                  zh_xfree( retval );

               zh_storc( NULL, 2 );
               zh_retni( iLen );
               return;
            }

            if( iPos == iAllocated )
            {
               iAllocated += iBufferSize;
               retval      = ( char * ) zh_xrealloc( retval, iAllocated );
            }

            retval[ iPos++ ] = buffer[ 0 ];

            if( iPos == iMax )
               break;
         }

         if( retval )
         {
            if( ! zh_storclen_buffer( retval, iPos, 2 ) )
               zh_xfree( retval );
         }
         else
            zh_storc( NULL, 2 );

         zh_retni( iPos );
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_SSL_READ_LINE )
{
   if( zh_SSL_is( 1 ) )
   {
      SSL * ssl = zh_SSL_par( 1 );

      if( ssl )
      {
         int iMax        = zh_parnidef( 3, INT_MAX );
         int iTimeout    = zh_parnidef( 4, -1 );
         int iBufferSize = zh_parnidef( 5, 80 );

         int    iPos       = 0;
         int    iAllocated = 0;
         char * retval     = NULL;

         for( ;; )
         {
            char buffer[ 1 ];
            int  iLen;
            int  sd = SSL_get_rfd( ssl );

            if( SSL_pending( ssl ) ||
                ( sd >= 0 && zh_socketSelectRead( ( ZH_SOCKET ) sd, iTimeout ) ) )
            {
               iLen = SSL_read( ssl, buffer, 1 );

               if( iLen == SSL_ERROR_WANT_READ )
                  continue;
            }
            else
               break;

            if( iLen <= 0 )
            {
               if( retval )
                  zh_xfree( retval );

               zh_storc( NULL, 2 );
               zh_retni( iLen );
               return;
            }
            else if( buffer[ 0 ] == '\r' )
               continue;
            else if( buffer[ 0 ] == '\n' )
               break;

            if( iPos == iAllocated )
            {
               iAllocated += iBufferSize;
               retval      = ( char * ) zh_xrealloc( retval, iAllocated );
            }

            retval[ iPos++ ] = buffer[ 0 ];

            if( iPos == iMax )
               break;
         }

         if( retval )
         {
            if( ! zh_storclen_buffer( retval, iPos, 2 ) )
               zh_xfree( retval );
         }
         else
            zh_storc( NULL, 2 );

         zh_retni( iPos );
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}
