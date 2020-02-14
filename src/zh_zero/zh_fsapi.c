/*
 * Ziher common FileSys API (accessed from standalone utilities and the RTL)
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

#if ! defined( _LARGEFILE64_SOURCE )
#  define _LARGEFILE64_SOURCE  1
#endif

#include "zh_api.h"
#include "zh_apifs.h"
#include "zh_io.h"
#include "zh_set.h"
#include "zh_date.h"

#if defined( ZH_OS_WIN )
   #include <windows.h>
   #include "zh_winuni.h"
   #if ! defined( INVALID_FILE_ATTRIBUTES )
      #define INVALID_FILE_ATTRIBUTES  ( ( DWORD ) -1 )
   #endif
   #if ! defined( FILE_ATTRIBUTE_DEVICE )
      #define FILE_ATTRIBUTE_DEVICE    0x00000040
   #endif
#elif defined( ZH_OS_UNIX )
   #include <sys/types.h>
   #include <sys/stat.h>
#endif
#if ! defined( ZH_OS_WIN )
   #include <errno.h>
#endif

#if ! defined( ZH_USE_LARGEFILE64 ) && defined( ZH_OS_UNIX )
   #if defined( __USE_LARGEFILE64 )
      /*
       * The macro: __USE_LARGEFILE64 is set when _LARGEFILE64_SOURCE is
       * defined and effectively enables lseek64/flock64/ftruncate64 functions
       * on 32-bit machines.
       */
      #define ZH_USE_LARGEFILE64
   #elif defined( ZH_OS_UNIX ) && defined( O_LARGEFILE )
      #define ZH_USE_LARGEFILE64
   #endif
#endif

/*
 * Function that adds zero or more paths to a list of pathnames to search
 */
void zh_fsAddSearchPath( const char * szPath, ZH_PATHNAMES ** pSearchList )
{
   char * pPath;
   char * pDelim;
   ZH_BOOL fFree = ZH_TRUE;

   while( *pSearchList )
      pSearchList = &( *pSearchList )->pNext;

   pPath = zh_strdup( szPath );
   while( ( pDelim = strchr( pPath, ZH_OS_PATH_LIST_SEP_CHR ) ) != NULL )
   {
      *pDelim = '\0';
      *pSearchList = ( ZH_PATHNAMES * ) zh_xgrab( sizeof( ZH_PATHNAMES ) );
      ( *pSearchList )->szPath = pPath;
      ( *pSearchList )->fFree  = fFree;
      pSearchList = &( *pSearchList )->pNext;
      pPath = pDelim + 1;
      fFree = ZH_FALSE;
   }
   *pSearchList = ( ZH_PATHNAMES * ) zh_xgrab( sizeof( ZH_PATHNAMES ) );
   ( *pSearchList )->szPath = pPath;
   ( *pSearchList )->pNext  = NULL;
   ( *pSearchList )->fFree  = fFree;
}

/*
 * free list of pathnames to search
 */
void zh_fsFreeSearchPath( ZH_PATHNAMES * pSearchList )
{
   ZH_PATHNAMES * pNext;

   /* Only the first path holds an allocated string.
      All of the other paths in the list are part of
      that first string. */

   while( pSearchList )
   {
      if( pSearchList->fFree )
         zh_xfree( pSearchList->szPath );
      pNext = pSearchList->pNext;
      zh_xfree( pSearchList );
      pSearchList = pNext;
   }
}

/* Split given filename into path, name and extension, plus determine drive */
PZH_FNAME zh_fsFNameSplit( const char * pszFileName )
{
   PZH_FNAME pFileName;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsFNameSplit(%s)", pszFileName ) );

   ZH_TRACE( ZH_TR_INFO, ( "zh_fsFNameSplit: Filename: |%s|", pszFileName ) );

   /* Grab memory, set defaults */
   pFileName = ( PZH_FNAME ) zh_xgrab( sizeof( ZH_FNAME ) );

   pFileName->szPath =
   pFileName->szName =
   pFileName->szExtension =
   pFileName->szDrive = NULL;

   if( pszFileName )
   {
      char * pszPos, cDirSep;
      ZH_ISIZ iSize, iPos;

      iPos = iSize = zh_strnlen( pszFileName, ZH_PATH_MAX - 1 );
      cDirSep = ( char ) zh_setGetDirSeparator();

      pszPos = pFileName->szBuffer;

      /* Find the end of the path part, and find out where the
         name+ext starts */

      while( --iPos >= 0 )
      {
         if( pszFileName[ iPos ] == cDirSep ||
             strchr( ZH_OS_PATH_DELIM_CHR_LIST, pszFileName[ iPos ] ) )
         {
            pFileName->szPath = pszPos;
            zh_strncpy( pszPos, pszFileName, iPos + 1 );
            pszPos += iPos + 2;
            pszFileName += iPos + 1;
            iSize -= iPos + 1;
            break;
         }
      }

      /* From this point pszFileName will point to the name+ext part of the path */
      /* Split the filename part to name and extension */
      iPos = iSize;
      while( --iPos > 0 )
      {
         if( pszFileName[ iPos ] == '.' )
         {
            pFileName->szExtension = pszPos;
            zh_strncpy( pszPos, pszFileName + iPos, iSize - iPos );
            pszPos += iSize - iPos + 1;
            iSize = iPos;
            break;
         }
      }
      if( iSize )
      {
         pFileName->szName = pszPos;
         zh_strncpy( pszPos, pszFileName, iSize );
         pszPos += iSize + 1;
      }

      /* Duplicate the drive letter from the path for easy access on
         platforms where applicable. Note that the drive info is always
         present also in the path itself. */

      if( pFileName->szPath )
      {
         iPos = 0;
         while( iPos < ZH_MAX_DRIVE_LENGTH && pFileName->szPath[ iPos ] != '\0' )
         {
            if( pFileName->szPath[ iPos ] == ':' )
            {
               pFileName->szDrive = pszPos;
               zh_strncpy( pszPos, pFileName->szPath, iPos );
               break;
            }
            ++iPos;
         }
      }
   }

   ZH_TRACE( ZH_TR_INFO, ( "zh_fsFNameSplit:   szPath: |%s|", pFileName->szPath ) );
   ZH_TRACE( ZH_TR_INFO, ( "zh_fsFNameSplit:   szName: |%s|", pFileName->szName ) );
   ZH_TRACE( ZH_TR_INFO, ( "zh_fsFNameSplit:    szExt: |%s|", pFileName->szExtension ) );
   ZH_TRACE( ZH_TR_INFO, ( "zh_fsFNameSplit:  szDrive: |%s|", pFileName->szDrive ) );

   return pFileName;
}

/* NOTE: szFileName buffer must be at least ZH_PATH_MAX long.
 *       Because some foreign code may not be updated yet then
 *       zh_fsFNameMerge() effectively uses only ZH_PATH_MAX buffer
 *       but it will be changed in the future.
 */

/* This function joins path, name and extension into a string with a filename */
char * zh_fsFNameMerge( char * pszFileName, PZH_FNAME pFileName )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsFNameMerge(%p, %p)", ( void * ) pszFileName, ( void * ) pFileName ) );

   if( pszFileName && pFileName )
   {
      const char * pszName;
      char cDirSep;

      /* dir separator set by user */
      cDirSep = ( char ) zh_setGetDirSeparator();

      /* Set the result to an empty string */
      pszFileName[ 0 ] = '\0';

      /* Strip preceding path separators from the filename */
      pszName = pFileName->szName;
      if( pszName && pszName[ 0 ] != '\0' && ( pszName[ 0 ] == cDirSep ||
          strchr( ZH_OS_PATH_DELIM_CHR_LIST, pszName[ 0 ] ) != NULL ) )
         pszName++;

      /* Add path if specified */
      if( pFileName->szPath )
         zh_strncat( pszFileName, pFileName->szPath, ZH_PATH_MAX - 1 - 1 );

      /* If we have a path, append a path separator to the path if there
         was none. */
      if( pszFileName[ 0 ] != '\0' && ( pszName || pFileName->szExtension ) )
      {
         int iLen = ( int ) strlen( pszFileName ) - 1;

         if( iLen < ZH_PATH_MAX - 1 - 2 && pszFileName[ iLen ] != cDirSep &&
             strchr( ZH_OS_PATH_DELIM_CHR_LIST, pszFileName[ iLen ] ) == NULL )
         {
            pszFileName[ iLen + 1 ] = ZH_OS_PATH_DELIM_CHR;
            pszFileName[ iLen + 2 ] = '\0';
         }
      }

      /* Add filename (without extension) if specified */
      if( pszName )
         zh_strncat( pszFileName, pszName, ZH_PATH_MAX - 1 - 1 );

      /* Add extension if specified */
      if( pFileName->szExtension )
      {
         /* Add a dot if the extension doesn't have it */
         if( pFileName->szExtension[ 0 ] != '\0' &&
             pFileName->szExtension[ 0 ] != '.' )
            zh_strncat( pszFileName, ".", ZH_PATH_MAX - 1 - 1 );

         zh_strncat( pszFileName, pFileName->szExtension, ZH_PATH_MAX - 1 - 1 );
      }

      ZH_TRACE( ZH_TR_INFO, ( "zh_fsFNameMerge:   szPath: |%s|", pFileName->szPath ) );
      ZH_TRACE( ZH_TR_INFO, ( "zh_fsFNameMerge:   szName: |%s|", pFileName->szName ) );
      ZH_TRACE( ZH_TR_INFO, ( "zh_fsFNameMerge:    szExt: |%s|", pFileName->szExtension ) );
      ZH_TRACE( ZH_TR_INFO, ( "zh_fsFNameMerge: Filename: |%s|", pszFileName ) );
   }

   return pszFileName;
}


ZH_BOOL zh_fsNameExists( const char * pszFileName )
{
   ZH_BOOL fExist = ZH_FALSE;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsNameExists(%p)", ( const void * ) pszFileName ) );

   if( pszFileName != NULL )
   {
#if defined( ZH_OS_WIN )
      LPTSTR lpFree;
      LPCTSTR lpFileName = ZH_FSNAMECONV( pszFileName, &lpFree );

      fExist = ( GetFileAttributes( lpFileName ) != INVALID_FILE_ATTRIBUTES );

      if( lpFree )
         zh_xfree( lpFree );
#else
      char * pszFree = NULL;

      pszFileName = zh_fsNameConv( pszFileName, &pszFree );

      {
#if defined( ZH_OS_UNIX )
#     if defined( ZH_USE_LARGEFILE64 )
         struct stat64 statbuf;
         fExist = stat64( pszFileName, &statbuf ) == 0;
#     else
         struct stat statbuf;
         fExist = stat( pszFileName, &statbuf ) == 0;
#     endif
#  else
         int iTODO; /* To force warning */
#  endif
      }

      if( pszFree )
         zh_xfree( pszFree );
#endif
   }

   return fExist;
}

ZH_BOOL zh_fsFileExists( const char * pszFileName )
{
   ZH_BOOL fExist = ZH_FALSE;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsFileExists(%p)", ( const void * ) pszFileName ) );

   if( pszFileName != NULL )
   {
#if defined( ZH_OS_WIN )
      LPTSTR lpFree;
      LPCTSTR lpFileName = ZH_FSNAMECONV( pszFileName, &lpFree );
      DWORD dwAttr;

      dwAttr = GetFileAttributes( lpFileName );
      fExist = ( dwAttr != INVALID_FILE_ATTRIBUTES ) &&
               ( dwAttr & ( FILE_ATTRIBUTE_DIRECTORY |
                            FILE_ATTRIBUTE_DEVICE ) ) == 0;

      if( lpFree )
         zh_xfree( lpFree );
#else
      char * pszFree = NULL;

      pszFileName = zh_fsNameConv( pszFileName, &pszFree );

      {

#if defined( ZH_OS_UNIX )
#     if defined( ZH_USE_LARGEFILE64 )
         struct stat64 statbuf;
         fExist = stat64( pszFileName, &statbuf ) == 0 &&
                  S_ISREG( statbuf.st_mode );
#     else
         struct stat statbuf;
         fExist = stat( pszFileName, &statbuf ) == 0 &&
                  S_ISREG( statbuf.st_mode );
#     endif
#  else
         int iTODO; /* To force warning */
#endif
      }

      if( pszFree )
         zh_xfree( pszFree );
#endif
   }

   return fExist;
}

ZH_BOOL zh_fsDirExists( const char * pszDirName )
{
   ZH_BOOL fExist = ZH_FALSE;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsDirExists(%p)", ( const void * ) pszDirName ) );

   if( pszDirName != NULL )
   {
#if defined( ZH_OS_WIN )
      LPTSTR lpFree;
      LPCTSTR lpDirName = ZH_FSNAMECONV( pszDirName, &lpFree );
      DWORD dwAttr;

      dwAttr = GetFileAttributes( lpDirName );
      fExist = ( dwAttr != INVALID_FILE_ATTRIBUTES ) &&
               ( dwAttr & FILE_ATTRIBUTE_DIRECTORY );

      if( lpFree )
         zh_xfree( lpFree );
#else
      char * pszFree = NULL;

      pszDirName = zh_fsNameConv( pszDirName, &pszFree );

      {
#  if defined( ZH_OS_DOS )
#     if defined( __DJGPP__ )
         int iAttr = _chmod( pszDirName, 0, 0 );
         fExist = iAttr != -1 && ( iAttr & 0x10 ) != 0;
#     else
         unsigned int iAttr = 0;
         fExist = _dos_getfileattr( pszDirName, &iAttr ) == 0 &&
                  ( iAttr & 0x10 ) != 0;
#     endif
#  elif defined( ZH_OS_UNIX )
#     if defined( ZH_USE_LARGEFILE64 )
         struct stat64 statbuf;
         fExist = stat64( pszDirName, &statbuf ) == 0 &&
                  S_ISDIR( statbuf.st_mode );
#     else
         struct stat statbuf;
         fExist = stat( pszDirName, &statbuf ) == 0 &&
                  S_ISDIR( statbuf.st_mode );
#     endif
#  else
         int iTODO; /* To force warning */
#  endif
      }

      if( pszFree )
         zh_xfree( pszFree );
#endif
   }

   return fExist;
}

ZH_BOOL zh_fsMaxFilesError( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsMaxFilesError()" ) );

#if defined( ZH_OS_WIN )
   return GetLastError() == ERROR_TOO_MANY_OPEN_FILES;
#else
   return errno == EMFILE;
#endif
}
