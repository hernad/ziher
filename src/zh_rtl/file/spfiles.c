/*
 * A search path shim for the FileSys API (C level)
 *
 * Copyright 2001 David G. Holm <dholm@jsd-llc.com>
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
#include "zh_apifs.h"
#include "zh_set.h"

ZH_BOOL zh_spFile( const char * pszFileName, char * pszRetPath )
{
   char * pszPath;
   ZH_BOOL bIsFile = ZH_FALSE;
   PZH_FNAME pFilepath;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_spFile(%s, %p)", pszFileName, ( void * ) pszRetPath ) );

   if( pszRetPath )
      pszPath = pszRetPath;
   else
      pszPath = ( char * ) zh_xgrab( ZH_PATH_MAX );

   pFilepath = zh_fsFNameSplit( pszFileName );

   if( pFilepath->szPath )
   {
      zh_fsFNameMerge( pszPath, pFilepath );
      bIsFile = zh_fsFile( pszPath );
   }
   else
   {
      const char * szDefault = zh_setGetDefault();
      if( szDefault )
      {
         pFilepath->szPath = szDefault;
         zh_fsFNameMerge( pszPath, pFilepath );
         bIsFile = zh_fsFile( pszPath );
      }

      if( ! bIsFile && zh_setGetPath() )
      {
         ZH_PATHNAMES * pNextPath = zh_setGetFirstSetPath();

         while( bIsFile == ZH_FALSE && pNextPath )
         {
            pFilepath->szPath = pNextPath->szPath;
            zh_fsFNameMerge( pszPath, pFilepath );
            bIsFile = zh_fsFile( pszPath );
            pNextPath = pNextPath->pNext;
         }
      }

      /*
       * This code is intentional. To eliminate race condition,
       * in pending zh_spCreate()/zh_spOpen() call when we have to know
       * real path and file name we have to set its deterministic value
       * here. If it's not necessary the caller may drop this value.
       */
      if( ! bIsFile )
      {
         pFilepath->szPath = szDefault ? szDefault : ".";
         zh_fsFNameMerge( pszPath, pFilepath );
      }
   }

   zh_xfree( pFilepath );

   if( pszRetPath == NULL )
      zh_xfree( pszPath );

   return bIsFile;
}

ZH_BOOL zh_spFileExists( const char * pszFileName, char * pszRetPath )
{
   char * pszPath;
   ZH_BOOL bIsFile = ZH_FALSE;
   PZH_FNAME pFilepath;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_spFileExists(%s, %p)", pszFileName, ( void * ) pszRetPath ) );

   if( pszRetPath )
      pszPath = pszRetPath;
   else
      pszPath = ( char * ) zh_xgrab( ZH_PATH_MAX );

   pFilepath = zh_fsFNameSplit( pszFileName );

   if( pFilepath->szPath )
   {
      zh_fsFNameMerge( pszPath, pFilepath );
      bIsFile = zh_fsFileExists( pszPath );
   }
   else
   {
      const char * szDefault = zh_setGetDefault();
      if( szDefault )
      {
         pFilepath->szPath = szDefault;
         zh_fsFNameMerge( pszPath, pFilepath );
         bIsFile = zh_fsFileExists( pszPath );
      }

      if( ! bIsFile && zh_setGetPath() )
      {
         ZH_PATHNAMES * pNextPath = zh_setGetFirstSetPath();

         while( bIsFile == ZH_FALSE && pNextPath )
         {
            pFilepath->szPath = pNextPath->szPath;
            zh_fsFNameMerge( pszPath, pFilepath );
            bIsFile = zh_fsFileExists( pszPath );
            pNextPath = pNextPath->pNext;
         }
      }

      /*
       * This code is intentional. To eliminate race condition,
       * in pending zh_spCreate()/zh_spOpen() call when we have to know
       * real path and file name we have to set its deterministic value
       * here. If it's not necessary the caller may drop this value.
       */
      if( ! bIsFile )
      {
         pFilepath->szPath = szDefault ? szDefault : ".";
         zh_fsFNameMerge( pszPath, pFilepath );
      }
   }

   zh_xfree( pFilepath );

   if( pszRetPath == NULL )
      zh_xfree( pszPath );

   return bIsFile;
}

ZH_FHANDLE zh_spOpen( const char * pszFileName, ZH_USHORT uiFlags )
{
   char szPath[ ZH_PATH_MAX ];

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_spOpen(%p, %hu)", ( const void * ) pszFileName, uiFlags ) );

   if( zh_spFile( pszFileName, szPath ) )
      return zh_fsOpen( szPath, uiFlags );
   else
      return zh_fsOpen( pszFileName, uiFlags );
}

ZH_FHANDLE zh_spCreate( const char * pszFileName, ZH_FATTR ulAttr )
{
   char szPath[ ZH_PATH_MAX ];
   PZH_FNAME pFilepath;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_spCreate(%p, %u)", ( const void * ) pszFileName, ulAttr ) );

   pFilepath = zh_fsFNameSplit( pszFileName );
   if( ! pFilepath->szPath )
      pFilepath->szPath = zh_setGetDefault();

   zh_fsFNameMerge( szPath, pFilepath );
   zh_xfree( pFilepath );

   return zh_fsCreate( szPath, ulAttr );
}

ZH_FHANDLE zh_spCreateEx( const char * pszFileName, ZH_FATTR ulAttr, ZH_USHORT uiFlags )
{
   char szPath[ ZH_PATH_MAX ];
   PZH_FNAME pFilepath;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_spCreateEx(%p, %u, %hu)", ( const void * ) pszFileName, ulAttr, uiFlags ) );

   pFilepath = zh_fsFNameSplit( pszFileName );
   if( ! pFilepath->szPath )
      pFilepath->szPath = zh_setGetDefault();

   zh_fsFNameMerge( szPath, pFilepath );
   zh_xfree( pFilepath );

   return zh_fsCreateEx( szPath, ulAttr, uiFlags );
}
