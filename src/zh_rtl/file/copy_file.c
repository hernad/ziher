/*
 * __CopyFile() function
 *
 * Copyright 1999 Andi Jahja <andij@aonlippo.co.id>
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
#include "zh_item_api.h"
#include "zh_fs_api.h"

#if defined( ZH_OS_UNIX )
   #include <sys/stat.h>
   #include <unistd.h>
#endif


#define BUFFER_SIZE  65536


static ZH_BOOL zh_copyfile( const char * pszSource, const char * pszDest )
{
   ZH_BOOL bRetVal = ZH_FALSE;
   PZH_FILE pSource;
   PZH_ITEM pError = NULL;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_copyfile(%s, %s)", pszSource, pszDest ) );

   do
   {
      pSource = zh_fileExtOpen( pszSource, NULL,
                                FO_READ | FO_SHARED | FO_PRIVATE |
                                FXO_DEFAULTS | FXO_SHARELOCK,
                                NULL, pError );
      if( pSource == NULL )
      {
         pError = zh_errRT_FileError( pError, NULL, EG_OPEN, 2012, pszSource );
         if( zh_errLaunch( pError ) != E_RETRY )
            break;
      }
   }
   while( pSource == NULL );

   if( pError )
   {
      zh_itemRelease( pError );
      pError = NULL;
   }

   if( pSource != NULL )
   {
      PZH_FILE pDest;

      do
      {
         pDest = zh_fileExtOpen( pszDest, NULL,
                                 FO_WRITE | FO_EXCLUSIVE | FO_PRIVATE |
                                 FXO_TRUNCATE | FXO_DEFAULTS | FXO_SHARELOCK,
                                 NULL, pError );
         if( pDest == NULL )
         {
            pError = zh_errRT_FileError( pError, NULL, EG_CREATE, 2012, pszDest );
            if( zh_errLaunch( pError ) != E_RETRY )
               break;
         }
      }
      while( pDest == NULL );

      if( pError )
      {
         zh_itemRelease( pError );
         pError = NULL;
      }

      if( pDest != NULL )
      {
         ZH_UCHAR * buffer;
         ZH_SIZE nRead;

         buffer = ( ZH_UCHAR * ) zh_xgrab( BUFFER_SIZE );
         bRetVal = ZH_TRUE;

         while( ( nRead = zh_fileRead( pSource, buffer, BUFFER_SIZE, -1 ) ) != 0 &&
                nRead != ( ZH_SIZE ) FS_ERROR )
         {
            ZH_SIZE nWritten = 0;

            while( nWritten < nRead )
            {
               ZH_SIZE nDone = zh_fileWrite( pDest, buffer + nWritten, nRead - nWritten, -1 );
               if( nDone != ( ZH_SIZE ) FS_ERROR )
                  nWritten += nDone;
               if( nWritten < nRead )
               {
                  pError = zh_errRT_FileError( pError, NULL, EG_WRITE, 2016, pszDest );
                  if( zh_errLaunch( pError ) != E_RETRY )
                  {
                     bRetVal = ZH_FALSE;
                     break;
                  }
               }
            }
         }

         if( pError )
            zh_itemRelease( pError );

         zh_xfree( buffer );

         zh_fileClose( pDest );
      }

      zh_fileClose( pSource );

#if defined( ZH_OS_UNIX )
      if( bRetVal )
      {
         ZH_FATTR ulAttr;

         if( zh_fileAttrGet( pszSource, &ulAttr ) )
            zh_fileAttrSet( pszDest, ulAttr );
      }
#endif
   }

   return bRetVal;
}


ZH_FUNC( __COPYFILE )
{
   const char * szSource = zh_parc( 1 );
   const char * szDest = zh_parc( 2 );

   if( szSource && szDest )
   {
      if( ! zh_copyfile( szSource, szDest ) )
         zh_retl( ZH_FALSE );
   }
   else
      zh_errRT_BASE( EG_ARG, 2010, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}
