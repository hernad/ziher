/*
 * Ziher File Find API (C level)
 *
 * Copyright 2001-2002 Luiz Rafael Culik <culik@sl.conex.net>
 * Copyright 2001-2002 Viktor Szakats
 * Copyright 2001-2002 Paul Tucker <ptucker@sympatico.ca>
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

#define _ZH_FFIND_INTERNAL_

#include "zh_api.h"
#include "zh_fs_api.h"
#include "zh_vm.h"
#include "zh_date.h"
#include "zh_io.h"

/* --- */

#if defined( ZH_OS_WIN )

   #include <windows.h>
   #include "zh_win_unicode.h"

   typedef struct
   {
      HANDLE            hFindFile;
      WIN32_FIND_DATA   pFindFileData;
      DWORD             dwAttr;
      ZH_BOOL           fLabelDone;
   } ZH_FFIND_INFO, * PZH_FFIND_INFO;

   #define _ZH_WIN_MASKATTR                    ( FILE_ATTRIBUTE_DIRECTORY | FILE_ATTRIBUTE_HIDDEN | FILE_ATTRIBUTE_SYSTEM )
   #define _ZH_WIN_MATCH() \
      ( \
         ( ( info->pFindFileData.dwFileAttributes & _ZH_WIN_MASKATTR ) == 0 ) || \
         ( ( info->dwAttr & info->pFindFileData.dwFileAttributes & _ZH_WIN_MASKATTR ) != 0 ) \
      )

#elif defined( ZH_OS_UNIX )

   #include <sys/types.h>
   #include <sys/stat.h>
   #include <fcntl.h>
   #include <dirent.h>
   #include <time.h>

   typedef struct
   {
      DIR *           dir;
      struct dirent * entry;
      char            pattern[ ZH_PATH_MAX ];
      char            path[ ZH_PATH_MAX ];
   } ZH_FFIND_INFO, * PZH_FFIND_INFO;

#else

   typedef struct
   {
      void * unused;
   } ZH_FFIND_INFO, * PZH_FFIND_INFO;

#endif

#if ! defined( ZH_USE_LARGEFILE64 ) && defined( ZH_OS_UNIX )
   #if defined( __USE_LARGEFILE64 )
      /*
       * The macro: __USE_LARGEFILE64 is set when _LARGEFILE64_SOURCE is
       * defined and effectively enables lseek64()/flock64()/ftruncate64()
       * functions on 32-bit machines.
       */
      #define ZH_USE_LARGEFILE64
   #elif defined( ZH_OS_UNIX ) && defined( O_LARGEFILE )
      #define ZH_USE_LARGEFILE64
   #endif
#endif

/* --- */

ZH_FATTR zh_fsAttrFromRaw( ZH_FATTR raw_attr )
{
   ZH_FATTR nAttr;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsAttrFromRaw(%u)", raw_attr ) );


#if defined( ZH_OS_WIN )

   nAttr = 0;
   if( raw_attr & FILE_ATTRIBUTE_ARCHIVE )   nAttr |= ZH_FA_ARCHIVE;
   if( raw_attr & FILE_ATTRIBUTE_DIRECTORY ) nAttr |= ZH_FA_DIRECTORY;
   if( raw_attr & FILE_ATTRIBUTE_HIDDEN )    nAttr |= ZH_FA_HIDDEN;
   if( raw_attr & FILE_ATTRIBUTE_READONLY )  nAttr |= ZH_FA_READONLY;
   if( raw_attr & FILE_ATTRIBUTE_SYSTEM )    nAttr |= ZH_FA_SYSTEM;
   if( raw_attr & FILE_ATTRIBUTE_NORMAL )    nAttr |= ZH_FA_NORMAL;

   /* Note that FILE_ATTRIBUTE_NORMAL is not needed
      ZH_FA_DEVICE not supported
      ZH_FA_VOLCOMP needs to be checked */
   if( raw_attr & FILE_ATTRIBUTE_ENCRYPTED )     nAttr |= ZH_FA_ENCRYPTED;
   if( raw_attr & FILE_ATTRIBUTE_TEMPORARY )     nAttr |= ZH_FA_TEMPORARY;
   if( raw_attr & FILE_ATTRIBUTE_SPARSE_FILE )   nAttr |= ZH_FA_SPARSE;
   if( raw_attr & FILE_ATTRIBUTE_REPARSE_POINT ) nAttr |= ZH_FA_REPARSE;
   if( raw_attr & FILE_ATTRIBUTE_COMPRESSED )    nAttr |= ZH_FA_COMPRESSED;
   if( raw_attr & FILE_ATTRIBUTE_OFFLINE )       nAttr |= ZH_FA_OFFLINE;
   /* FILE_ATTRIBUTE_NOT_CONTENT_INDEXED */
   /* not defined in some older winnt.h  */
   if( raw_attr & 0x00002000 )                   nAttr |= ZH_FA_NOTINDEXED;
   if( raw_attr & 0x00008000 )                   nAttr |= ZH_FA_VOLCOMP;

#elif defined( ZH_OS_UNIX )

   nAttr = ( ( raw_attr & S_IXOTH ) ? ZH_FA_XOTH : 0 ) |
           ( ( raw_attr & S_IWOTH ) ? ZH_FA_WOTH : 0 ) |
           ( ( raw_attr & S_IROTH ) ? ZH_FA_ROTH : 0 ) |
           ( ( raw_attr & S_IXGRP ) ? ZH_FA_XGRP : 0 ) |
           ( ( raw_attr & S_IWGRP ) ? ZH_FA_WGRP : 0 ) |
           ( ( raw_attr & S_IRGRP ) ? ZH_FA_RGRP : 0 ) |
           ( ( raw_attr & S_IXUSR ) ? ZH_FA_XUSR : 0 ) |
           ( ( raw_attr & S_IWUSR ) ? ZH_FA_WUSR : 0 ) |
           ( ( raw_attr & S_IRUSR ) ? ZH_FA_RUSR : 0 ) |
           ( ( raw_attr & S_ISVTX ) ? ZH_FA_SVTX : 0 ) |
           ( ( raw_attr & S_ISGID ) ? ZH_FA_SGID : 0 ) |
           ( ( raw_attr & S_ISUID ) ? ZH_FA_SUID : 0 );

   if( S_ISREG( raw_attr ) )  nAttr |= ZH_FA_FILE;
   if( S_ISDIR( raw_attr ) )  nAttr |= ZH_FA_DIRECTORY;
   if( S_ISLNK( raw_attr ) )  nAttr |= ZH_FA_LINK;
   if( S_ISCHR( raw_attr ) )  nAttr |= ZH_FA_CHRDEVICE;
   if( S_ISBLK( raw_attr ) )  nAttr |= ZH_FA_BLKDEVICE;
   if( S_ISFIFO( raw_attr ) ) nAttr |= ZH_FA_FIFO;
   if( S_ISSOCK( raw_attr ) ) nAttr |= ZH_FA_SOCKET;


#else

   nAttr = 0;
   ZH_SYMBOL_UNUSED( raw_attr );

#endif

   return nAttr;
}

ZH_FATTR zh_fsAttrToRaw( ZH_FATTR nAttr )
{
   ZH_FATTR raw_attr;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsAttrToRaw(%u)", nAttr ) );


#if defined( ZH_OS_WIN )

   raw_attr = 0;

   if( nAttr & ZH_FA_ARCHIVE )   raw_attr |= FILE_ATTRIBUTE_ARCHIVE;
   if( nAttr & ZH_FA_DIRECTORY ) raw_attr |= FILE_ATTRIBUTE_DIRECTORY;
   if( nAttr & ZH_FA_HIDDEN )    raw_attr |= FILE_ATTRIBUTE_HIDDEN;
   if( nAttr & ZH_FA_READONLY )  raw_attr |= FILE_ATTRIBUTE_READONLY;
   if( nAttr & ZH_FA_SYSTEM )    raw_attr |= FILE_ATTRIBUTE_SYSTEM;
   if( nAttr & ZH_FA_NORMAL )    raw_attr |= FILE_ATTRIBUTE_NORMAL;

   /* Note that FILE_ATTRIBUTE_NORMAL is not needed
      ZH_FA_DEVICE not supported
      ZH_FA_VOLCOMP needs to be checked */
   if( nAttr & ZH_FA_ENCRYPTED )  raw_attr |= FILE_ATTRIBUTE_ENCRYPTED;
   if( nAttr & ZH_FA_TEMPORARY )  raw_attr |= FILE_ATTRIBUTE_TEMPORARY;
   if( nAttr & ZH_FA_SPARSE )     raw_attr |= FILE_ATTRIBUTE_SPARSE_FILE;
   if( nAttr & ZH_FA_REPARSE )    raw_attr |= FILE_ATTRIBUTE_REPARSE_POINT;
   if( nAttr & ZH_FA_COMPRESSED ) raw_attr |= FILE_ATTRIBUTE_COMPRESSED;
   if( nAttr & ZH_FA_OFFLINE )    raw_attr |= FILE_ATTRIBUTE_OFFLINE;
   if( nAttr & ZH_FA_NOTINDEXED ) raw_attr |= 0x00002000; /* FILE_ATTRIBUTE_NOT_CONTENT_INDEXED not defined in some older winnt.h */
   if( nAttr & ZH_FA_VOLCOMP )    raw_attr |= 0x00008000;

#elif defined( ZH_OS_UNIX )

   raw_attr = ZH_FA_POSIX_ATTR( nAttr );

   if( nAttr & ZH_FA_FILE )       raw_attr |= S_IFREG;
   if( nAttr & ZH_FA_DIRECTORY )  raw_attr |= S_IFDIR;
   if( nAttr & ZH_FA_LINK )       raw_attr |= S_IFLNK;
   if( nAttr & ZH_FA_CHRDEVICE )  raw_attr |= S_IFCHR;
   if( nAttr & ZH_FA_BLKDEVICE )  raw_attr |= S_IFBLK;
   if( nAttr & ZH_FA_FIFO )       raw_attr |= S_IFIFO;
   if( nAttr & ZH_FA_SOCKET )     raw_attr |= S_IFSOCK;

#else

   ZH_SYMBOL_UNUSED( nAttr );
   raw_attr = 0;

#endif

   return raw_attr;
}

/* Converts a CA-Cl*pper compatible file attribute string
   to the internal representation. */

ZH_FATTR zh_fsAttrEncode( const char * szAttr )
{
   const char * pos = szAttr;
   char ch;
   ZH_FATTR nAttr = 0;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsAttrEncode(%p)", ( const void * ) szAttr ) );

   while( ( ch = ( char ) ZH_TOUPPER( *pos ) ) != '\0' )
   {
      switch( ch )
      {
         case 'R': nAttr |= ZH_FA_READONLY;  break;
         case 'H': nAttr |= ZH_FA_HIDDEN;    break;
         case 'S': nAttr |= ZH_FA_SYSTEM;    break;
         case 'A': nAttr |= ZH_FA_ARCHIVE;   break;
         case 'D': nAttr |= ZH_FA_DIRECTORY; break;
         case 'V': nAttr |= ZH_FA_LABEL;     break;
         case 'L': nAttr |= ZH_FA_LINK;      break;
      }

      pos++;
   }

   return nAttr;
}

/* Converts a file attribute (ffind->attr) to the CA-Cl*pper
   compatible file attribute string format. */

/* NOTE: szAttr buffer must be at least 16 chars long */

char * zh_fsAttrDecode( ZH_FATTR nAttr, char * szAttr )
{
   char * ptr = szAttr;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsAttrDecode(%u, %p)", nAttr, ( void * ) szAttr ) );

   /* Using the same order as CA-Cl*pper did: RHSVDA. */
   if( nAttr & ZH_FA_READONLY   ) *ptr++ = 'R';
   if( nAttr & ZH_FA_HIDDEN     ) *ptr++ = 'H';
   if( nAttr & ZH_FA_SYSTEM     ) *ptr++ = 'S';
   if( nAttr & ZH_FA_ARCHIVE    ) *ptr++ = 'A';
   if( nAttr & ZH_FA_DIRECTORY  ) *ptr++ = 'D';
   if( nAttr & ZH_FA_LABEL      ) *ptr++ = 'V';
   if( nAttr & ZH_FA_LINK       ) *ptr++ = 'L';

   *ptr = '\0';

   return szAttr;
}

/* Finds the first then the next matching file on
   each call. Does low-level (platform dependent
   filtering if needed. */

static ZH_BOOL zh_fsFindNextLow( PZH_FFIND ffind )
{
   ZH_BOOL bFound;

   int iYear  = 0;
   int iMonth = 0;
   int iDay   = 0;

   int iHour = 0;
   int iMin  = 0;
   int iSec  = 0;
   int iMSec = 0;

   ZH_FATTR raw_attr = 0, nAttr = 0;

   /* Set the default values in case some platforms don't
      support some of these, or they may fail on them. */

   ffind->szName[ 0 ] = '\0';
   ffind->size = 0;

   /* Do platform dependent first/next search */

   zh_vmUnlock();

#if defined( ZH_OS_WIN )
   {
      PZH_FFIND_INFO info = ( PZH_FFIND_INFO ) ffind->info;

      bFound = ZH_FALSE;

      if( ( ffind->attrmask & ZH_FA_LABEL ) != 0 && ! info->fLabelDone )
      {
         TCHAR lpVolName[ ZH_PATH_MAX ];
         LPTSTR lpFileMask = NULL;
         char * mask = NULL;

         info->fLabelDone = ZH_TRUE;

         if( ffind->pszFileMask && *ffind->pszFileMask )
         {
            PZH_FNAME pFileName = zh_fsFNameSplit( ffind->pszFileMask );
            if( pFileName->szName && pFileName->szName[ 0 ] )
               mask = zh_strdup( pFileName->szName );
            if( pFileName->szPath && pFileName->szPath[ 0 ] &&
                ( pFileName->szPath[ 1 ] ||
                  pFileName->szPath[ 0 ] != ZH_OS_PATH_DELIM_CHR ) )
               lpFileMask = ZH_CHARDUP( pFileName->szPath );
            zh_xfree( pFileName );
         }
         bFound = GetVolumeInformation( lpFileMask, lpVolName,
                                        ZH_SIZEOFARRAY( lpVolName ),
                                        NULL, NULL, NULL, NULL, 0 ) != 0;
         if( bFound )
         {
            ZH_OSSTRDUP2( lpVolName, ffind->szName, sizeof( ffind->szName ) - 1 );
            if( mask && *mask && ! zh_strMatchFile( ffind->szName, mask ) )
            {
               ffind->szName[ 0 ] = '\0';
               bFound = ZH_FALSE;
            }
         }
         if( lpFileMask )
            zh_xfree( lpFileMask );
         if( mask )
            zh_xfree( mask );
      }

      if( ! bFound &&
          ( ffind->attrmask & ( ZH_FA_LABEL | ZH_FA_HIDDEN | ZH_FA_SYSTEM |
                                ZH_FA_DIRECTORY ) ) != ZH_FA_LABEL )
      {
         if( ffind->bFirst )
         {
            LPTSTR lpFileMask = ZH_CHARDUP( ffind->pszFileMask );
            ffind->bFirst = ZH_FALSE;
            info->dwAttr    = ( DWORD ) zh_fsAttrToRaw( ffind->attrmask );
            info->hFindFile = FindFirstFile( lpFileMask, &info->pFindFileData );
            zh_xfree( lpFileMask );

            if( ( info->hFindFile != INVALID_HANDLE_VALUE ) && _ZH_WIN_MATCH() )
               bFound = ZH_TRUE;
         }

         if( ! bFound && info->hFindFile != INVALID_HANDLE_VALUE )
         {
            while( FindNextFile( info->hFindFile, &info->pFindFileData ) )
            {
               if( _ZH_WIN_MATCH() )
               {
                  bFound = ZH_TRUE;
                  break;
               }
            }
         }

         /* Fill Ziher found file info */

         if( bFound )
         {
            ZH_OSSTRDUP2( info->pFindFileData.cFileName, ffind->szName, sizeof( ffind->szName ) - 1 );

            if( info->pFindFileData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY )
               ffind->size = 0;
            else
            {
#if defined( __POCC__ ) && __POCC__ >= 500
               /* NOTE: Pelles C 5.00.1 will go into an infinite loop if we don't
                        split this into two operations. [vszakats] */
               ffind->size  = ( ZH_FOFFSET ) info->pFindFileData.nFileSizeLow;
               ffind->size += ( ZH_FOFFSET ) info->pFindFileData.nFileSizeHigh << 32;
#else
               ffind->size = ( ZH_FOFFSET ) info->pFindFileData.nFileSizeLow +
                           ( ( ZH_FOFFSET ) info->pFindFileData.nFileSizeHigh << 32 );
#endif
            }

            raw_attr = ( ZH_FATTR ) info->pFindFileData.dwFileAttributes;

            /* NOTE: One of these may fail when searching on an UNC path, I
                     don't know yet what's the reason. [vszakats] */

            {
               FILETIME ft;
               SYSTEMTIME time;

               if( FileTimeToLocalFileTime( &info->pFindFileData.ftLastWriteTime, &ft ) &&
                   FileTimeToSystemTime( &ft, &time ) )
               {
                  iYear  = time.wYear;
                  iMonth = time.wMonth;
                  iDay   = time.wDay;
                  iHour  = time.wHour;
                  iMin   = time.wMinute;
                  iSec   = time.wSecond;
                  iMSec  = time.wMilliseconds;
               }
            }
         }
      }
      zh_fsSetIOError( bFound, 0 );
   }

#elif defined( ZH_OS_UNIX )

   {
      PZH_FFIND_INFO info = ( PZH_FFIND_INFO ) ffind->info;

      char dirname[ ZH_PATH_MAX ];

      bFound = ZH_FALSE;

      /* TODO: ZH_FA_LABEL handling */

      if( ffind->bFirst )
      {
         char * pos;

         ffind->bFirst = ZH_FALSE;

         zh_strncpy( dirname, ffind->pszFileMask, sizeof( dirname ) - 1 );
         pos = strrchr( dirname, ZH_OS_PATH_DELIM_CHR );
         if( pos )
         {
            zh_strncpy( info->pattern, pos + 1, sizeof( info->pattern ) - 1 );
            *( pos + 1 ) = '\0';
         }
         else
         {
            zh_strncpy( info->pattern, dirname, sizeof( info->pattern ) - 1 );
            dirname[ 0 ] = '.';
            dirname[ 1 ] = ZH_OS_PATH_DELIM_CHR;
            dirname[ 2 ] = '\0';
         }
         if( info->pattern[ 0 ] == '.' )
            ffind->attrmask |= ZH_FA_HIDDEN;

         #if 0
         tzset();
         #endif

         info->dir = opendir( dirname );
         zh_strncpy( info->path, dirname, sizeof( info->path ) - 1 );
      }

      if( info->dir && info->pattern[ 0 ] != '\0' )
      {
         while( ( info->entry = readdir( info->dir ) ) != NULL )
         {
            if( zh_strMatchFile( info->entry->d_name, info->pattern ) )
            {
               bFound = ZH_TRUE;
               break;
            }
         }
      }

      /* Fill Ziher found file info */
      if( bFound )
      {
         zh_strncpy( dirname, info->path, sizeof( dirname ) - 1 );
         zh_strncat( dirname, info->entry->d_name, sizeof( dirname ) - 1 );
         {
            time_t ftime;
            struct tm lt;
#if defined( ZH_USE_LARGEFILE64 )
            struct stat64 sStat, sStatL;
            if( lstat64( dirname, &sStat ) == 0 )
            {
               if( S_ISLNK( sStat.st_mode ) && ( ffind->attrmask & ZH_FA_LINK ) == 0 )
               {
                  if( stat64( dirname, &sStatL ) == 0 )
                     memcpy( &sStat, &sStatL, sizeof( sStat ) );
                  nAttr |= ZH_FA_LINK;
               }
#else
            struct stat sStat, sStatL;
            if( lstat( dirname, &sStat ) == 0 )
            {
               if( S_ISLNK( sStat.st_mode ) && ( ffind->attrmask & ZH_FA_LINK ) == 0 )
               {
                  if( stat( dirname, &sStatL ) == 0 )
                     memcpy( &sStat, &sStatL, sizeof( sStat ) );
                  nAttr |= ZH_FA_LINK;
               }
#endif
               if( info->entry->d_name[ 0 ] == '.' )
               {
                  if( info->entry->d_name[ 1 ] &&
                      ( info->entry->d_name[ 1 ] != '.' || info->entry->d_name[ 2 ] ) )
                     nAttr |= ZH_FA_HIDDEN;
               }
               zh_strncpy( ffind->szName, info->entry->d_name, sizeof( ffind->szName ) - 1 );
               ffind->size = sStat.st_size;

               raw_attr = sStat.st_mode;

               ftime = sStat.st_mtime;
#  if defined( ZH_HAS_LOCALTIME_R )
               localtime_r( &ftime, &lt );
#  else
               lt = *localtime( &ftime );
#  endif

               iYear  = lt.tm_year + 1900;
               iMonth = lt.tm_mon + 1;
               iDay   = lt.tm_mday;

               iHour = lt.tm_hour;
               iMin  = lt.tm_min;
               iSec  = lt.tm_sec;

#  if defined( ZH_OS_LINUX ) && \
      defined( __GLIBC__ ) && defined( __GLIBC_MINOR__ ) && \
      ( __GLIBC__ > 2 || ( __GLIBC__ == 2 && __GLIBC_MINOR__ >= 6 ) )
#     if defined( _BSD_SOURCE ) || defined( _SVID_SOURCE ) || \
         ( __GLIBC_MINOR__ >= 12 && \
           ( ( defined( _POSIX_C_SOURCE ) || _POSIX_C_SOURCE >= 200809L ) || \
             ( defined( _XOPEN_SOURCE ) || _XOPEN_SOURCE >= 700 ) ) )
               iMSec = sStat.st_mtim.tv_nsec / 1000000;
#     else
               iMSec = sStat.st_mtimensec / 1000000;
#     endif
#  endif
            }
            else
               bFound = ZH_FALSE;
         }
      }
      zh_fsSetIOError( bFound, 0 );
   }

#else

   {
      int iTODO; /* TODO: for given platform */

      #if 0
      ZH_SYMBOL_UNUSED( ffind );
      #endif

      ZH_SYMBOL_UNUSED( iYear );
      ZH_SYMBOL_UNUSED( iMonth );
      ZH_SYMBOL_UNUSED( iDay );
      ZH_SYMBOL_UNUSED( iHour );
      ZH_SYMBOL_UNUSED( iMin );
      ZH_SYMBOL_UNUSED( iSec );
      ZH_SYMBOL_UNUSED( iMSec );
      ZH_SYMBOL_UNUSED( raw_attr );

      bFound = ZH_FALSE;

      zh_fsSetIOError( bFound, 0 );
   }

#endif

   /* Fill common Ziher found file info */

   if( bFound )
   {
      /* Do the conversions common for all platforms */
      ffind->szName[ sizeof( ffind->szName ) - 1 ] = '\0';

#if ! defined( ZH_OS_WIN )
      /* Convert from OS codepage */
      {
         char * pszFree = NULL;
         ZH_SIZE nSize = sizeof( ffind->szName );
         const char * pszResult = zh_osDecodeCP( ffind->szName, &pszFree, &nSize );

         if( pszFree )
         {
            zh_strncpy( ffind->szName, pszResult, sizeof( ffind->szName ) - 1 );
            zh_xfree( pszFree );
         }
      }
#endif
      ffind->attr = zh_fsAttrFromRaw( raw_attr ) | nAttr;

      ffind->lDate = zh_dateEncode( iYear, iMonth, iDay );
      ffind->lTime = zh_timeEncode( iHour, iMin, iSec, iMSec );
      zh_dateStrPut( ffind->szDate, iYear, iMonth, iDay );
      ffind->szDate[ 8 ] = '\0';

      zh_snprintf( ffind->szTime, sizeof( ffind->szTime ), "%02d:%02d:%02d", iHour, iMin, iSec );
   }
   zh_vmLock();

   return bFound;
}

PZH_FFIND zh_fsFindFirst( const char * pszFileMask, ZH_FATTR attrmask )
{
   PZH_FFIND ffind = ( PZH_FFIND ) zh_xgrabz( sizeof( ZH_FFIND ) );

   /* Allocate platform dependent file find info storage */
   ffind->info = ( void * ) zh_xgrabz( sizeof( ZH_FFIND_INFO ) );

   /* Store search parameters */
#if defined( ZH_OS_WIN )
   ffind->pszFileMask = pszFileMask;
#else
   /* Convert to OS codepage */
   ffind->pszFileMask = zh_fsNameConv( pszFileMask, &ffind->pszFree );
#endif
   ffind->attrmask = attrmask;
   ffind->bFirst = ZH_TRUE;

   /* Find first/next matching file */

   if( zh_fsFindNext( ffind ) )
      return ffind;

   /* If no file found at all, free stuff allocated so far and return NULL. */

   zh_fsFindClose( ffind );

   return NULL;
}

/* Finds next matching file, and applies a filter which makes
   searching CA-Cl*pper/MS-DOS compatible. */

ZH_BOOL zh_fsFindNext( PZH_FFIND ffind )
{
   while( zh_fsFindNextLow( ffind ) )
   {
      /* Filter the result to stay MS-DOS and CA-Cl*pper compatible. */

      if( !( ( ( ffind->attrmask & ZH_FA_HIDDEN    ) == 0 && ( ffind->attr & ZH_FA_HIDDEN    ) != 0 ) ||
             ( ( ffind->attrmask & ZH_FA_SYSTEM    ) == 0 && ( ffind->attr & ZH_FA_SYSTEM    ) != 0 ) ||
             ( ( ffind->attrmask & ZH_FA_LABEL     ) == 0 && ( ffind->attr & ZH_FA_LABEL     ) != 0 ) ||
             ( ( ffind->attrmask & ZH_FA_DIRECTORY ) == 0 && ( ffind->attr & ZH_FA_DIRECTORY ) != 0 ) ) )
      {
         return ZH_TRUE;
      }
   }

   return ZH_FALSE;
}

void zh_fsFindClose( PZH_FFIND ffind )
{
   if( ffind )
   {
      if( ffind->pszFree )
         zh_xfree( ffind->pszFree );

      /* Do platform dependent cleanup */

      if( ffind->info )
      {
         PZH_FFIND_INFO info = ( PZH_FFIND_INFO ) ffind->info;

         if( ! ffind->bFirst )
         {
            zh_vmUnlock();

#if defined( ZH_OS_WIN )

            if( info->hFindFile != INVALID_HANDLE_VALUE )
               FindClose( info->hFindFile );

#elif defined( ZH_OS_UNIX )

            if( info->dir )
               closedir( info->dir );

#else
            {
               /* Intentionally do nothing */
               int iTODO; /* TODO: for given platform */
            }
#endif

            zh_vmLock();
         }

         zh_xfree( info );
      }

      zh_xfree( ffind );
   }
}
