/*
 * MemoWrit()/MemoRead() functions
 *
 * Copyright 1999-2001 Viktor Szakats
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
#include "zh_apifs.h"

/* NOTE: CA-Cl*pper has ~64 KiB (65516 bytes exactly) limit on read, in Ziher
         this limit is extended, so we are not *strictly* compatible here.
         [vszakats] */

static void zh_memoread( ZH_BOOL bHandleEOF )
{
   const char * pszFileName = zh_parc( 1 );

   if( pszFileName )
   {
      ZH_SIZE nSize;
      char * pBuffer = ( char * ) zh_fileLoad( pszFileName, 0, &nSize );

      if( pBuffer )
      {
         /* Don't read the file terminating EOF character */
         if( bHandleEOF && nSize > 0 )
         {
            if( pBuffer[ nSize - 1 ] == ZH_CHAR_EOF )
               --nSize;
         }
         zh_retclen_buffer( pBuffer, nSize );
      }
      else
         zh_retc_null();
   }
   else
      zh_retc_null();
}

ZH_FUNC( ZH_MEMOREAD )
{
   zh_memoread( ZH_FALSE );
}

ZH_FUNC( MEMOREAD )
{
   zh_memoread( ZH_TRUE );
}

static ZH_BOOL zh_memowrit( ZH_BOOL bHandleEOF )
{
   const char * pszFileName = zh_parc( 1 );
   PZH_ITEM pString   = zh_param( 2, ZH_IT_STRING );
   ZH_BOOL bRetVal    = ZH_FALSE;

   if( pszFileName && pString )
   {
      PZH_FILE pFile = zh_fileExtOpen( pszFileName, NULL,
                                       FO_WRITE | FO_EXCLUSIVE | FO_PRIVATE |
                                       FXO_TRUNCATE | FXO_SHARELOCK,
                                       NULL, NULL );

      if( pFile != NULL )
      {
         ZH_SIZE nSize = zh_itemGetCLen( pString );
         const char * pData = zh_itemGetCPtr( pString );

         while( nSize > 0 )
         {
            ZH_SIZE nWritten = zh_fileWrite( pFile, pData, nSize, 0 );
            if( nWritten == 0 || nWritten == ( ZH_SIZE ) FS_ERROR )
               break;
            nSize -= nWritten;
            pData += nWritten;
         }
         bRetVal = nSize == 0;

         /* NOTE: CA-Cl*pper will add the EOF even if the write failed. [vszakats] */
         /* NOTE: CA-Cl*pper will not return .F. when the EOF could not be written. [vszakats] */
         if( bHandleEOF && bRetVal )  /* if true, then write EOF */
         {
            char cEOF = ZH_CHAR_EOF;
            zh_fileWrite( pFile, &cEOF, sizeof( char ), -1 );
         }

         zh_fileClose( pFile );
      }
   }

   return bRetVal;
}

ZH_FUNC( ZH_MEMOWRIT )
{
   zh_retl( zh_memowrit( ZH_FALSE ) );
}

ZH_FUNC( MEMOWRIT )
{
   zh_retl( zh_memowrit( ZH_TRUE ) );
}
