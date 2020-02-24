/*
 * zh_fsCopy(), zh_FCopy() functions
 *
 * Copyright 1991-2008 Viktor Szakats
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
#include "zh_fs_api.h"

#define ZH_FSCOPY_BUFFERSIZE  65536

ZH_BOOL zh_fsCopy( const char * pszSource, const char * pszDest )
{
   ZH_BOOL fResult = ZH_FALSE;
   PZH_FILE pSrcFile;

   if( ( pSrcFile = zh_fileExtOpen( pszSource, NULL, FO_READ | FO_SHARED | FXO_SHARELOCK, NULL, NULL ) ) != NULL )
   {
      PZH_FILE pDstFile;
      ZH_ERRCODE errCode;

      if( ( pDstFile = zh_fileExtOpen( pszDest, NULL, FXO_TRUNCATE | FO_WRITE | FO_EXCLUSIVE | FXO_SHARELOCK, NULL, NULL ) ) != NULL )
      {
         void * pbyBuffer = zh_xgrab( ZH_FSCOPY_BUFFERSIZE );

         for( ;; )
         {
            ZH_SIZE nBytesRead;
            if( ( nBytesRead = zh_fileRead( pSrcFile, pbyBuffer, ZH_FSCOPY_BUFFERSIZE, -1 ) ) > 0 &&
                nBytesRead != ( ZH_SIZE ) FS_ERROR )
            {
               if( nBytesRead != zh_fileWrite( pDstFile, pbyBuffer, nBytesRead, -1 ) )
               {
                  errCode = zh_fsError();
                  break;
               }
            }
            else
            {
               errCode = zh_fsError();
               fResult = errCode == 0;
               break;
            }
         }

         zh_xfree( pbyBuffer );

         zh_fileClose( pDstFile );
      }
      else
         errCode = zh_fsError();

      zh_fileClose( pSrcFile );

      if( fResult )
      {
         ZH_FATTR ulAttr;

         if( zh_fileAttrGet( pszSource, &ulAttr ) )
            zh_fileAttrSet( pszDest, ulAttr );
      }
      zh_fsSetError( errCode );
   }

   return fResult;
}

ZH_FUNC( ZH_FCOPY )
{
   ZH_ERRCODE errCode = 2; /* file not found */
   ZH_BOOL fResult = ZH_FALSE;
   const char * pszSource = zh_parc( 1 ), * pszDest = zh_parc( 2 );

   if( pszSource && pszDest )
   {
      fResult = zh_fsCopy( pszSource, pszDest );
      errCode = zh_fsError();
   }
   else
   {
      zh_fsSetFError( 2 /* file not found */ );
      zh_retni( F_ERROR );
   }
   zh_fsSetFError( errCode );
   zh_retni( fResult ? 0 : F_ERROR );
}
