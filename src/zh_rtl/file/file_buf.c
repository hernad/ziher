/*
 * Functions to access files with shared handles and locks
 * (buffers in the future)
 *
 * Copyright 2008 Przemyslaw Czerpak
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

/* this has to be declared before zh_apifs.h is included */
#define _ZH_FILE_INTERNAL_

#if ! defined( _LARGEFILE64_SOURCE )
#  define _LARGEFILE64_SOURCE  1
#endif

#include "zh_api.h"
#include "zh_apifs.h"
#include "zh_api_error.h"
#include "zh_item_api.h"
#include "zh_thread.h"
#include "zh_vm.h"
#include "directory.zhh"

#if defined( ZH_OS_UNIX )
#  include <sys/types.h>
#  include <sys/stat.h>
#  include <unistd.h>
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

#define ZH_FLOCK_RESIZE  16

typedef struct
{
   ZH_FOFFSET start;
   ZH_FOFFSET len;
}
ZH_FLOCK, * PZH_FLOCK;

typedef struct _ZH_FILE
{
   const ZH_FILE_FUNCS * pFuncs;
   ZH_ULONG       device;
   ZH_ULONG       inode;
   int            used;
   int            mode;
   ZH_BOOL        shared;
   ZH_FHANDLE     hFile;
   ZH_FHANDLE     hFileRO;
   PZH_FLOCK      pLocks;
   ZH_UINT        uiLocks;
   ZH_UINT        uiSize;
   struct _ZH_FILE * pNext;
   struct _ZH_FILE * pPrev;
}
ZH_FILE;

static const ZH_FILE_FUNCS * s_fileMethods( void );
#if defined( ZH_OS_UNIX )
   static PZH_FILE zh_fileposNew( PZH_FILE pFile );
#endif

static ZH_CRITICAL_NEW( s_fileMtx );
static ZH_CRITICAL_NEW( s_lockMtx );

static PZH_FILE s_openFiles = NULL;


static PZH_FILE zh_fileFind( ZH_ULONG device, ZH_ULONG inode )
{
   if( s_openFiles && ( device || inode ) )
   {
      PZH_FILE pFile = s_openFiles;
      do
      {
         if( pFile->device == device && pFile->inode == inode )
            return pFile;
         pFile = pFile->pNext;
      }
      while( s_openFiles != pFile );
   }
   return NULL;
}

static PZH_FILE zh_fileNew( ZH_FHANDLE hFile, ZH_BOOL fShared, int iMode,
                            ZH_ULONG device, ZH_ULONG inode, ZH_BOOL fBind )
{
   PZH_FILE pFile = zh_fileFind( device, inode );

   if( ! pFile )
   {
      pFile = ( PZH_FILE ) zh_xgrabz( sizeof( ZH_FILE ) );
      pFile->pFuncs  = s_fileMethods();
      pFile->device  = device;
      pFile->inode   = inode;
      pFile->hFile   = hFile;
      pFile->hFileRO = FS_ERROR;
      pFile->shared  = fShared;
      pFile->mode    = iMode;

      if( fBind )
      {
         if( s_openFiles )
         {
            pFile->pNext = s_openFiles;
            pFile->pPrev = s_openFiles->pPrev;
            pFile->pPrev->pNext = pFile;
            s_openFiles->pPrev = pFile;
         }
         else
            s_openFiles = pFile->pNext = pFile->pPrev = pFile;
      }
   }
   pFile->used++;

   return pFile;
}

static ZH_UINT zh_fileFindOffset( PZH_FILE pFile, ZH_FOFFSET nOffset )
{
   ZH_UINT uiFirst, uiLast, uiMiddle;

   uiFirst = 0;
   uiLast = pFile->uiLocks;
   uiMiddle = uiLast >> 1;

   while( uiFirst < uiLast )
   {
      ZH_FOFFSET nEnd = pFile->pLocks[ uiMiddle ].start +
                        pFile->pLocks[ uiMiddle ].len;
      if( nEnd > 0 && nEnd <= nOffset )
         uiFirst = uiMiddle + 1;
      else
         uiLast = uiMiddle;
      uiMiddle = ( uiFirst + uiLast ) >> 1;
   }

   return uiMiddle;
}

static void zh_fileInsertLock( PZH_FILE pFile, ZH_UINT uiPos,
                               ZH_FOFFSET nStart, ZH_FOFFSET nLen )
{
   if( pFile->uiLocks == pFile->uiSize )
   {
      pFile->uiSize += ZH_FLOCK_RESIZE;
      pFile->pLocks = ( PZH_FLOCK ) zh_xrealloc( pFile->pLocks,
                                          sizeof( ZH_FLOCK ) * pFile->uiSize );
      memset( &pFile->pLocks[ pFile->uiLocks ], 0,
              sizeof( ZH_FLOCK ) * ZH_FLOCK_RESIZE );
   }
   memmove( &pFile->pLocks[ uiPos + 1 ], &pFile->pLocks[ uiPos ],
            ( pFile->uiLocks - uiPos ) * sizeof( ZH_FLOCK ) );
   pFile->pLocks[ uiPos ].start = nStart;
   pFile->pLocks[ uiPos ].len   = nLen;
   pFile->uiLocks++;
}

static void zh_fileDeleteLock( PZH_FILE pFile, ZH_UINT uiPos )
{
   pFile->uiLocks--;
   memmove( &pFile->pLocks[ uiPos ], &pFile->pLocks[ uiPos + 1 ],
            ( pFile->uiLocks - uiPos ) * sizeof( ZH_FLOCK ) );
   if( pFile->uiSize - pFile->uiLocks >= ( ZH_FLOCK_RESIZE << 1 ) )
   {
      pFile->uiSize -= ZH_FLOCK_RESIZE;
      pFile->pLocks = ( PZH_FLOCK ) zh_xrealloc( pFile->pLocks,
                                          sizeof( ZH_FLOCK ) * pFile->uiSize );
   }
}

static ZH_BOOL zh_fileSetLock( PZH_FILE pFile, ZH_BOOL * pfLockFS,
                               ZH_FOFFSET nStart, ZH_FOFFSET nLen )
{
   ZH_BOOL fLJoin, fRJoin;
   ZH_UINT uiPos;

   uiPos = zh_fileFindOffset( pFile, nStart );
   fLJoin = fRJoin = ZH_FALSE;
   if( uiPos < pFile->uiLocks )
   {
      PZH_FLOCK pLock = &pFile->pLocks[ uiPos ];
      ZH_FOFFSET nEnd = nStart + nLen;
      if( nEnd <= 0 || nEnd > pLock->start )
         return ZH_FALSE;
      if( nEnd == pLock->start )
         fRJoin = ZH_TRUE;
   }
   if( uiPos > 0 )
   {
      PZH_FLOCK pLock = &pFile->pLocks[ uiPos - 1 ];
      if( pLock->start + pLock->len == nStart )
         fLJoin = ZH_TRUE;
   }
   if( fLJoin )
   {
      if( fRJoin )
      {
         pFile->pLocks[ uiPos - 1 ].len += nLen + pFile->pLocks[ uiPos ].len;
         zh_fileDeleteLock( pFile, uiPos );
      }
      else
         pFile->pLocks[ uiPos - 1 ].len += nLen;
   }
   else if( fRJoin )
   {
      pFile->pLocks[ uiPos ].start -= nLen;
      pFile->pLocks[ uiPos ].len   += nLen;
   }
   else
      zh_fileInsertLock( pFile, uiPos, nStart, nLen );

   if( pFile->shared )
      *pfLockFS = ZH_TRUE;
   return ZH_TRUE;
}

static ZH_BOOL zh_fileUnlock( PZH_FILE pFile, ZH_BOOL * pfLockFS,
                              ZH_FOFFSET nStart, ZH_FOFFSET nLen )
{
   ZH_BOOL fResult = ZH_FALSE;
   ZH_UINT uiPos;

   uiPos = zh_fileFindOffset( pFile, nStart );
   if( uiPos < pFile->uiLocks )
   {
      PZH_FLOCK pLock = &pFile->pLocks[ uiPos ];
      if( nStart >= pLock->start && pLock->len >= nLen &&
          nStart - pLock->start <= pLock->len - nLen )
      {
         if( pfLockFS && pFile->shared )
            *pfLockFS = ZH_TRUE;
         else if( nStart == pLock->start )
         {
            if( nLen == pLock->len )
               zh_fileDeleteLock( pFile, uiPos );
            else
            {
               pLock->start += nLen;
               pLock->len   -= nLen;
            }
         }
         else if( nStart + nLen == pLock->start + pLock->len )
            pLock->len -= nLen;
         else
         {
            zh_fileInsertLock( pFile, uiPos + 1, nStart + nLen,
                               pLock->start + pLock->len - nStart - nLen );
            pLock = &pFile->pLocks[ uiPos ];
            pLock->len = nStart - pLock->start;
         }
         fResult = ZH_TRUE;
      }
   }
   return fResult;
}

static ZH_BOOL zh_fileTestLock( PZH_FILE pFile,
                                ZH_FOFFSET nStart, ZH_FOFFSET nLen )
{
   ZH_UINT uiPos;

   uiPos = zh_fileFindOffset( pFile, nStart );
   if( uiPos < pFile->uiLocks )
   {
      PZH_FLOCK pLock = &pFile->pLocks[ uiPos ];
      ZH_FOFFSET nEnd = nStart + nLen;
      if( nEnd <= 0 || nEnd > pLock->start )
         return ZH_TRUE;
   }

   return ZH_FALSE;
}


/*
 * file methods
 */

static ZH_BOOL s_fileAccept( PZH_FILE_FUNCS pFuncs, const char * pszFileName )
{
   ZH_SYMBOL_UNUSED( pFuncs );

   return pszFileName && *pszFileName;
}

static ZH_BOOL s_fileExists( PZH_FILE_FUNCS pFuncs, const char * pszFileName, char * pRetPath )
{
   ZH_SYMBOL_UNUSED( pFuncs );

   return pRetPath ? zh_spFileExists( pszFileName, pRetPath ) :
                     zh_fsFileExists( pszFileName );
}

static ZH_BOOL s_fileDelete( PZH_FILE_FUNCS pFuncs, const char * pszFileName )
{
   ZH_SYMBOL_UNUSED( pFuncs );

   return zh_fsDelete( pszFileName );
}

static ZH_BOOL s_fileRename( PZH_FILE_FUNCS pFuncs, const char * pszName, const char * pszNewName )
{
   ZH_SYMBOL_UNUSED( pFuncs );

   return zh_fsRename( pszName, pszNewName );
}

static ZH_BOOL s_fileCopy( PZH_FILE_FUNCS pFuncs, const char * pszSrcFile, const char * pszDstFile )
{
   ZH_SYMBOL_UNUSED( pFuncs );

   return zh_fsCopy( pszSrcFile, pszDstFile );
}

static ZH_BOOL s_fileDirExists( PZH_FILE_FUNCS pFuncs, const char * pszDirName )
{
   ZH_SYMBOL_UNUSED( pFuncs );

   return zh_fsDirExists( pszDirName );
}

static ZH_BOOL s_fileDirMake( PZH_FILE_FUNCS pFuncs, const char * pszDirName )
{
   ZH_SYMBOL_UNUSED( pFuncs );

   return zh_fsMkDir( pszDirName );
}

static ZH_BOOL s_fileDirRemove( PZH_FILE_FUNCS pFuncs, const char * pszDirName )
{
   ZH_SYMBOL_UNUSED( pFuncs );

   return zh_fsRmDir( pszDirName );
}

static double s_fileDirSpace( PZH_FILE_FUNCS pFuncs, const char * pszDirName, ZH_USHORT uiType )
{
   ZH_SYMBOL_UNUSED( pFuncs );

   return zh_fsDiskSpace( pszDirName, uiType );
}

static PZH_ITEM s_fileDirectory( PZH_FILE_FUNCS pFuncs, const char * pszDirSpec, const char * pszAttr )
{
   ZH_SYMBOL_UNUSED( pFuncs );

   return zh_fsDirectory( pszDirSpec, pszAttr, ZH_TRUE );
}

static ZH_BOOL s_fileTimeGet( PZH_FILE_FUNCS pFuncs, const char * pszFileName, long * plJulian, long * plMillisec )
{
   ZH_SYMBOL_UNUSED( pFuncs );

   return zh_fsGetFileTime( pszFileName, plJulian, plMillisec );
}

static ZH_BOOL s_fileTimeSet( PZH_FILE_FUNCS pFuncs, const char * pszFileName, long lJulian, long lMillisec )
{
   ZH_SYMBOL_UNUSED( pFuncs );

   return zh_fsSetFileTime( pszFileName, lJulian, lMillisec );
}

static ZH_BOOL s_fileAttrGet( PZH_FILE_FUNCS pFuncs, const char * pszFileName, ZH_FATTR * pnAttr )
{
   ZH_SYMBOL_UNUSED( pFuncs );

   return zh_fsGetAttr( pszFileName, pnAttr );
}

static ZH_BOOL s_fileAttrSet( PZH_FILE_FUNCS pFuncs, const char * pszFileName, ZH_FATTR nAttr )
{
   ZH_SYMBOL_UNUSED( pFuncs );

   return zh_fsSetAttr( pszFileName, nAttr );
}

static ZH_BOOL s_fileLink( PZH_FILE_FUNCS pFuncs, const char * pszExisting, const char * pszNewName )
{
   ZH_SYMBOL_UNUSED( pFuncs );

   return zh_fsLink( pszExisting, pszNewName );
}

static ZH_BOOL s_fileLinkSym( PZH_FILE_FUNCS pFuncs, const char * pszTarget, const char * pszNewName )
{
   ZH_SYMBOL_UNUSED( pFuncs );

   return zh_fsLinkSym( pszTarget, pszNewName );
}

static char * s_fileLinkRead( PZH_FILE_FUNCS pFuncs, const char * pszFileName )
{
   ZH_SYMBOL_UNUSED( pFuncs );

   return zh_fsLinkRead( pszFileName );
}

static PZH_FILE s_fileExtOpen( PZH_FILE_FUNCS pFuncs, const char * pszFileName, const char * pDefExt,
                               ZH_FATTR nExFlags, const char * pPaths,
                               PZH_ITEM pError )
{
   PZH_FILE pFile = NULL;
#if defined( ZH_OS_UNIX )
   ZH_BOOL fSeek = ZH_FALSE;
#  if defined( ZH_USE_LARGEFILE64 )
   struct stat64 statbuf;
#  else
   struct stat statbuf;
#  endif
#endif
   ZH_BOOL fResult, fShared;
   int iMode;
   char * pszFile;

   ZH_SYMBOL_UNUSED( pFuncs );

   fShared = ( nExFlags & ( FO_DENYREAD | FO_DENYWRITE | FO_EXCLUSIVE ) ) == 0;
   iMode = ( int ) ( nExFlags & ( FO_READ | FO_WRITE | FO_READWRITE ) );
   pszFile = zh_fsExtName( pszFileName, pDefExt, nExFlags, pPaths );

   zh_vmUnlock();
#if ! defined( ZH_OS_UNIX )
   fResult = ZH_TRUE;
#else
#  if defined( ZH_USE_SHARELOCKS ) && ! defined( ZH_USE_BSDLOCKS )
   if( nExFlags & FXO_SHARELOCK )
   {
      if( iMode == FO_WRITE && fShared )
      {
         if( access( pszFile, R_OK ) == 0 ||
             access( pszFile, F_OK ) != 0 )
         {
            nExFlags = ( nExFlags ^ FO_WRITE ) | FO_READWRITE;
            iMode = FO_READWRITE;
         }
         else
            nExFlags ^= FXO_SHARELOCK;
      }
      else if( iMode == FO_READ && ! fShared )
      {
         nExFlags &= ~ ( ZH_FATTR ) ( FO_DENYREAD | FO_DENYWRITE | FO_EXCLUSIVE );
         fShared = ZH_TRUE;
      }
   }
#  endif

   zh_threadEnterCriticalSection( &s_fileMtx );

#  if defined( ZH_USE_LARGEFILE64 )
   fResult = stat64( pszFile, &statbuf ) == 0;
#  else
   fResult = stat( pszFile, &statbuf ) == 0;
#  endif
   zh_fsSetIOError( fResult, 0 );

   if( fResult )
   {
      pFile = zh_fileFind( ( ZH_ULONG ) statbuf.st_dev,
                           ( ZH_ULONG ) statbuf.st_ino );
      if( pFile )
      {
         if( ! fShared || ! pFile->shared || ( nExFlags & FXO_TRUNCATE ) != 0 )
         {
            fResult = ZH_FALSE;
            pFile = NULL;
         }
         else if( pFile->mode != FO_READWRITE && pFile->mode != iMode )
         {
            iMode = FO_READWRITE;
            pFile = NULL;
         }
         else
         {
            pFile->used++;
            if( ( nExFlags & FXO_NOSEEKPOS ) == 0 )
            {
#  if defined( ZH_OS_VXWORKS )
               fSeek  = ! S_ISFIFO( statbuf.st_mode );
#  else
               fSeek  = ! S_ISFIFO( statbuf.st_mode ) && ! S_ISSOCK( statbuf.st_mode );
#  endif
            }
         }
      }
   }
   else
      fResult = ZH_TRUE;

   if( fResult && pFile == NULL )
#endif /* ZH_OS_UNIX */
   {
      ZH_FHANDLE hFile = zh_fsExtOpen( pszFile, NULL,
                            nExFlags & ~ ( ZH_FATTR ) ( FXO_DEFAULTS | FXO_COPYNAME ),
                            NULL, NULL );
      if( hFile != FS_ERROR )
      {
         ZH_ULONG device = 0, inode = 0;
#if ! defined( ZH_OS_UNIX )
         zh_threadEnterCriticalSection( &s_fileMtx );
#else
#  if defined( ZH_USE_LARGEFILE64 )
         if( fstat64( hFile, &statbuf ) == 0 )
#  else
         if( fstat( hFile, &statbuf ) == 0 )
#  endif
         {
            device = ( ZH_ULONG ) statbuf.st_dev;
            inode  = ( ZH_ULONG ) statbuf.st_ino;
            if( ( nExFlags & FXO_NOSEEKPOS ) == 0 )
            {
               fSeek  = ! S_ISFIFO( statbuf.st_mode ) && ! S_ISSOCK( statbuf.st_mode );
            }
         }
#endif /* ZH_OS_UNIX */

         pFile = zh_fileNew( hFile, fShared, iMode, device, inode, ZH_TRUE );
         if( pFile->hFile != hFile )
         {
            if( pFile->mode != FO_READWRITE && iMode == FO_READWRITE )
            {
               ZH_FHANDLE hTemp = pFile->hFileRO;
               pFile->hFileRO = pFile->hFile;
               pFile->hFile = hFile;
               pFile->mode = iMode;
               hFile = hTemp;
            }

            if( ! fShared || ! pFile->shared || pFile->mode != FO_READWRITE )
            {
               fResult = ZH_FALSE;
               if( pFile->hFileRO == FS_ERROR && pFile->uiLocks != 0 )
               {
                  pFile->hFileRO = hFile;
                  hFile = FS_ERROR;
               }
            }

            if( pFile->uiLocks == 0 )
            {
#if ! defined( ZH_USE_SHARELOCKS ) || defined( ZH_USE_BSDLOCKS )
               if( pFile->hFileRO != FS_ERROR )
               {
                  zh_fsClose( pFile->hFileRO );
                  pFile->hFileRO = FS_ERROR;
               }
#endif
               if( hFile != FS_ERROR )
               {
                  zh_fsClose( hFile );
                  hFile = FS_ERROR;
#if defined( ZH_USE_SHARELOCKS ) && ! defined( ZH_USE_BSDLOCKS )
                  /* FIXME: possible race condition */
                  zh_fsLockLarge( pFile->hFile, ZH_SHARELOCK_POS, ZH_SHARELOCK_SIZE,
                                  FL_LOCK | FLX_SHARED );
#endif
               }
            }
            if( !fResult )
            {
               if( pFile )
               {
                  --pFile->used;
                  pFile = NULL;
               }
               if( hFile != FS_ERROR )
               {
                  /* FIXME: possible race condition in MT mode,
                   *        close() is not safe due to existing locks
                   *        which are removed.
                   */
                  zh_fsClose( hFile );
               }
            }
         }
#if ! defined( ZH_OS_UNIX )
         zh_threadLeaveCriticalSection( &s_fileMtx );
#endif
      }
   }

#if defined( ZH_OS_UNIX )
   zh_threadLeaveCriticalSection( &s_fileMtx );
   if( pFile && fSeek )
      pFile = zh_fileposNew( pFile );
#endif

   if( ! fResult )
      zh_fsSetError( ( nExFlags & FXO_TRUNCATE ) ? 5 : 32 );
   if( ( nExFlags & FXO_COPYNAME ) != 0 && pFile )
      zh_strncpy( ( char * ) ZH_UNCONST( pszFileName ), pszFile, ZH_PATH_MAX - 1 );
   if( pError )
   {
      zh_errPutFileName( pError, pszFile );
      if( ! fResult )
      {
         zh_errPutOsCode( pError, zh_fsError() );
         zh_errPutGenCode( pError, ( ZH_ERRCODE ) ( ( nExFlags & FXO_TRUNCATE ) ? EG_CREATE : EG_OPEN ) );
      }
   }

   zh_xfree( pszFile );

   zh_vmLock();

   return pFile;
}

static void s_fileClose( PZH_FILE pFile )
{
   zh_vmUnlock();
   zh_fsSetError( 0 );
   zh_threadEnterCriticalSection( &s_fileMtx );

   if( --pFile->used == 0 )
   {
      if( pFile->pNext )
      {
         pFile->pPrev->pNext = pFile->pNext;
         pFile->pNext->pPrev = pFile->pPrev;
         if( pFile == s_openFiles )
         {
            s_openFiles = pFile->pNext;
            if( pFile == s_openFiles )
               s_openFiles = NULL;
         }
      }
      if( pFile->hFile != FS_ERROR )
         zh_fsClose( pFile->hFile );
      if( pFile->hFileRO != FS_ERROR )
         zh_fsClose( pFile->hFileRO );

      if( pFile->pLocks )
         zh_xfree( pFile->pLocks );

      zh_xfree( pFile );
   }

   zh_threadLeaveCriticalSection( &s_fileMtx );
   zh_vmLock();
}

static ZH_BOOL s_fileLock( PZH_FILE pFile, ZH_FOFFSET nStart, ZH_FOFFSET nLen,
                           int iType )
{
   ZH_BOOL fResult, fLockFS = ZH_FALSE;

   zh_vmUnlock();
   if( ( iType & FL_MASK ) == FL_UNLOCK )
   {
      zh_threadEnterCriticalSection( &s_lockMtx );
      fResult = zh_fileUnlock( pFile, &fLockFS, nStart, nLen );
      zh_threadLeaveCriticalSection( &s_lockMtx );
      if( fLockFS )
      {
         zh_fsLockLarge( pFile->hFile, nStart, nLen, ( ZH_USHORT ) iType );
         zh_threadEnterCriticalSection( &s_lockMtx );
         zh_fileUnlock( pFile, NULL, nStart, nLen );
         zh_threadLeaveCriticalSection( &s_lockMtx );
      }
      else
         zh_fsSetError( fResult ? 0 : 33 );
   }
   else
   {
      zh_threadEnterCriticalSection( &s_lockMtx );
      fResult = zh_fileSetLock( pFile, &fLockFS, nStart, nLen );
      zh_threadLeaveCriticalSection( &s_lockMtx );
      if( fLockFS )
      {
#if defined( ZH_OS_UNIX )
         if( pFile->mode == FO_READ )
            iType |= FLX_SHARED;
         else if( pFile->mode == FO_WRITE )
            iType &= ~FLX_SHARED;
#endif
         fResult = zh_fsLockLarge( pFile->hFile, nStart, nLen, ( ZH_USHORT ) iType );
         if( ! fResult )
         {
            zh_threadEnterCriticalSection( &s_lockMtx );
            zh_fileUnlock( pFile, NULL, nStart, nLen );
            zh_threadLeaveCriticalSection( &s_lockMtx );
         }
      }
      else
         zh_fsSetError( fResult ? 0 : 33 );
   }
   zh_vmLock();

   return fResult;
}

static int s_fileLockTest( PZH_FILE pFile, ZH_FOFFSET nStart, ZH_FOFFSET nLen,
                           int iType )
{
   ZH_BOOL fLocked;
   int iResult;

   zh_vmUnlock();

   zh_threadEnterCriticalSection( &s_lockMtx );
   fLocked = zh_fileTestLock( pFile, nStart, nLen );
   zh_threadLeaveCriticalSection( &s_lockMtx );
   if( fLocked )
   {
#if defined( ZH_OS_UNIX )
      iResult = getpid();
#else
      iResult = 1;
#endif
   }
   else
      iResult = zh_fsLockTest( pFile->hFile, nStart, nLen, ( ZH_USHORT ) iType );

   zh_vmLock();

   return iResult;
}

static ZH_SIZE s_fileRead( PZH_FILE pFile, void * buffer, ZH_SIZE nSize,
                           ZH_MAXINT nTimeout )
{
   ZH_SYMBOL_UNUSED( nTimeout );
   return zh_fsReadLarge( pFile->hFile, buffer, nSize );
}

static ZH_SIZE s_fileWrite( PZH_FILE pFile, const void * buffer, ZH_SIZE nSize,
                            ZH_MAXINT nTimeout )
{
   ZH_SYMBOL_UNUSED( nTimeout );
   return zh_fsWriteLarge( pFile->hFile, buffer, nSize );
}

static ZH_SIZE s_fileReadAt( PZH_FILE pFile, void * buffer, ZH_SIZE nSize,
                             ZH_FOFFSET nOffset )
{
   return zh_fsReadAt( pFile->hFile, buffer, nSize, nOffset );
}

static ZH_SIZE s_fileWriteAt( PZH_FILE pFile, const void * buffer, ZH_SIZE nSize,
                              ZH_FOFFSET nOffset )
{
   return zh_fsWriteAt( pFile->hFile, buffer, nSize, nOffset );
}

static ZH_BOOL s_fileTruncAt( PZH_FILE pFile, ZH_FOFFSET nOffset )
{
   return zh_fsTruncAt( pFile->hFile, nOffset );
}

static ZH_FOFFSET s_fileSeek( PZH_FILE pFile, ZH_FOFFSET nOffset,
                              ZH_USHORT uiFlags )
{
   return zh_fsSeekLarge( pFile->hFile, nOffset, uiFlags );
}

static ZH_FOFFSET s_fileSize( PZH_FILE pFile )
{
   return zh_fsGetSize( pFile->hFile );
}

static ZH_BOOL s_fileEof( PZH_FILE pFile )
{
   return zh_fsEof( pFile->hFile );
}

static void s_fileFlush( PZH_FILE pFile, ZH_BOOL fDirty )
{
   ZH_SYMBOL_UNUSED( pFile );
   ZH_SYMBOL_UNUSED( fDirty );
}

static void s_fileCommit( PZH_FILE pFile )
{
   zh_fsCommit( pFile->hFile );
}

static ZH_BOOL s_fileConfigure( PZH_FILE pFile, int iIndex, PZH_ITEM pValue )
{
   ZH_SYMBOL_UNUSED( pFile );

   switch( iIndex )
   {
      case ZH_VF_IONAME:
         zh_itemPutC( pValue, "FILE:" );
         return ZH_TRUE;
   }

   return ZH_FALSE;
}

static ZH_FHANDLE s_fileHandle( PZH_FILE pFile )
{
   return pFile ? pFile->hFile : FS_ERROR;
}

static const ZH_FILE_FUNCS * s_fileMethods( void )
{
   /* methods table */
   static const ZH_FILE_FUNCS s_fileFuncs =
   {
      s_fileAccept,

      s_fileExists,
      s_fileDelete,
      s_fileRename,
      s_fileCopy,

      s_fileDirExists,
      s_fileDirMake,
      s_fileDirRemove,
      s_fileDirSpace,
      s_fileDirectory,

      s_fileTimeGet,
      s_fileTimeSet,
      s_fileAttrGet,
      s_fileAttrSet,

      s_fileLink,
      s_fileLinkSym,
      s_fileLinkRead,

      s_fileExtOpen,
      s_fileClose,
      s_fileLock,
      s_fileLockTest,
      s_fileRead,
      s_fileWrite,
      s_fileReadAt,
      s_fileWriteAt,
      s_fileTruncAt,
      s_fileSeek,
      s_fileSize,
      s_fileEof,
      s_fileFlush,
      s_fileCommit,
      s_fileConfigure,
      s_fileHandle
   };

   return &s_fileFuncs;
}


#if defined( ZH_OS_UNIX )

typedef struct
{
   const ZH_FILE_FUNCS * pFuncs;
   PZH_FILE       pFile;
   ZH_FOFFSET     seek_pos;
}
ZH_FILEPOS, * PZH_FILEPOS;

#define _PZH_FILEPOS  ( ( PZH_FILEPOS ) pFilePos )
#define _PZH_FILE     _PZH_FILEPOS->pFile

static void s_fileposClose( PZH_FILE pFilePos )
{
   _PZH_FILE->pFuncs->Close( _PZH_FILE );
   zh_xfree( pFilePos );
}

static ZH_BOOL s_fileposLock( PZH_FILE pFilePos, ZH_FOFFSET nStart, ZH_FOFFSET nLen,
                              int iType )
{
   return _PZH_FILE->pFuncs->Lock( _PZH_FILE, nStart, nLen, iType );
}

static int s_fileposLockTest( PZH_FILE pFilePos, ZH_FOFFSET nStart, ZH_FOFFSET nLen,
                              int iType )
{
   return _PZH_FILE->pFuncs->LockTest( _PZH_FILE, nStart, nLen, iType );
}

static ZH_SIZE s_fileposRead( PZH_FILE pFilePos, void * buffer, ZH_SIZE nSize,
                              ZH_MAXINT nTimeout )
{
   ZH_SIZE nDone;

   ZH_SYMBOL_UNUSED( nTimeout );
   nDone = _PZH_FILE->pFuncs->ReadAt( _PZH_FILE, buffer, nSize, _PZH_FILEPOS->seek_pos );
   _PZH_FILEPOS->seek_pos += nDone;

   return nDone;
}

static ZH_SIZE s_fileposWrite( PZH_FILE pFilePos, const void * buffer, ZH_SIZE nSize,
                               ZH_MAXINT nTimeout )
{
   ZH_SIZE nDone;

   ZH_SYMBOL_UNUSED( nTimeout );
   nDone = _PZH_FILE->pFuncs->WriteAt( _PZH_FILE, buffer, nSize, _PZH_FILEPOS->seek_pos );
   _PZH_FILEPOS->seek_pos += nDone;

   return nDone;
}

static ZH_SIZE s_fileposReadAt( PZH_FILE pFilePos, void * buffer, ZH_SIZE nSize,
                                ZH_FOFFSET nOffset )
{
   return _PZH_FILE->pFuncs->ReadAt( _PZH_FILE, buffer, nSize, nOffset );
}

static ZH_SIZE s_fileposWriteAt( PZH_FILE pFilePos, const void * buffer, ZH_SIZE nSize,
                                 ZH_FOFFSET nOffset )
{
   return _PZH_FILE->pFuncs->WriteAt( _PZH_FILE, buffer, nSize, nOffset );
}

static ZH_BOOL s_fileposTruncAt( PZH_FILE pFilePos, ZH_FOFFSET nOffset )
{
   if( _PZH_FILE->pFuncs->TruncAt( _PZH_FILE, nOffset ) )
   {
      _PZH_FILEPOS->seek_pos = nOffset;
      return ZH_TRUE;
   }
   return ZH_FALSE;
}

static ZH_FOFFSET s_fileposSeek( PZH_FILE pFilePos, ZH_FOFFSET nOffset,
                                 ZH_USHORT uiFlags )
{
   if( uiFlags & FS_END )
      nOffset += pFilePos->pFuncs->Size( pFilePos );
   else if( uiFlags & FS_RELATIVE )
      nOffset += _PZH_FILEPOS->seek_pos;
   /* else FS_SET */

   if( nOffset >= 0 )
   {
      _PZH_FILEPOS->seek_pos = nOffset;
      zh_fsSetError( 0 );
   }
   else
      zh_fsSetError( 25 ); /* 'Seek Error' */

   return _PZH_FILEPOS->seek_pos;
}

static ZH_FOFFSET s_fileposSize( PZH_FILE pFilePos )
{
   return _PZH_FILE->pFuncs->Size( _PZH_FILE );
}

static ZH_BOOL s_fileposEof( PZH_FILE pFilePos )
{
   return _PZH_FILEPOS->seek_pos >= pFilePos->pFuncs->Size( pFilePos );
}

static void s_fileposFlush( PZH_FILE pFilePos, ZH_BOOL fDirty )
{
   _PZH_FILE->pFuncs->Flush( _PZH_FILE, fDirty );
}

static void s_fileposCommit( PZH_FILE pFilePos )
{
   _PZH_FILE->pFuncs->Commit( _PZH_FILE );
}

static ZH_BOOL s_fileposConfigure( PZH_FILE pFilePos, int iIndex, PZH_ITEM pValue )
{
   return _PZH_FILE->pFuncs->Configure( _PZH_FILE, iIndex, pValue );
}

static ZH_FHANDLE s_fileposHandle( PZH_FILE pFilePos )
{
   return pFilePos ? _PZH_FILE->pFuncs->Handle( _PZH_FILE ) : FS_ERROR;
}

static const ZH_FILE_FUNCS * s_fileposMethods( void )
{
   /* methods table */
   static const ZH_FILE_FUNCS s_fileFuncs =
   {
      s_fileAccept,

      s_fileExists,
      s_fileDelete,
      s_fileRename,
      s_fileCopy,

      s_fileDirExists,
      s_fileDirMake,
      s_fileDirRemove,
      s_fileDirSpace,
      s_fileDirectory,

      s_fileTimeGet,
      s_fileTimeSet,
      s_fileAttrGet,
      s_fileAttrSet,

      s_fileLink,
      s_fileLinkSym,
      s_fileLinkRead,

      s_fileExtOpen,
      s_fileposClose,
      s_fileposLock,
      s_fileposLockTest,
      s_fileposRead,
      s_fileposWrite,
      s_fileposReadAt,
      s_fileposWriteAt,
      s_fileposTruncAt,
      s_fileposSeek,
      s_fileposSize,
      s_fileposEof,
      s_fileposFlush,
      s_fileposCommit,
      s_fileposConfigure,
      s_fileposHandle
   };

   return &s_fileFuncs;
}

static PZH_FILE zh_fileposNew( PZH_FILE pFile )
{
   PZH_FILEPOS pFilePos = ( PZH_FILEPOS ) zh_xgrabz( sizeof( ZH_FILEPOS ) );

   pFilePos->pFuncs   = s_fileposMethods();
   pFilePos->pFile    = pFile;
   pFilePos->seek_pos = 0;

   return ( PZH_FILE ) pFilePos;
}

#endif /* ZH_OS_UNIX */

static const ZH_FILE_FUNCS * s_pFileTypes[ ZH_FILE_TYPE_MAX ];
static int s_iFileTypes = 0;

static int s_fileFindDrv( const char * pszFileName )
{
   int i = -1;

   if( pszFileName )
   {
      i = s_iFileTypes;

      while( --i >= 0 )
      {
         if( s_pFileTypes[ i ]->Accept( s_pFileTypes[ i ], pszFileName ) )
            break;
      }
   }

   return i;
}


/*
 * public API functions
 */

ZH_BOOL zh_fileRegisterFull( const ZH_FILE_FUNCS * pFuncs )
{
   ZH_BOOL fResult = ZH_FALSE;

   zh_vmUnlock();
   zh_threadEnterCriticalSection( &s_lockMtx );

   if( s_iFileTypes < ZH_FILE_TYPE_MAX )
   {
      s_pFileTypes[ s_iFileTypes ] = pFuncs;
      s_iFileTypes++;
      fResult = ZH_TRUE;
   }

   zh_threadLeaveCriticalSection( &s_lockMtx );
   zh_vmLock();

   return fResult;
}

ZH_BOOL zh_fileExists( const char * pszFileName, char * pRetPath )
{
   int i = s_fileFindDrv( pszFileName );

   if( i >= 0 )
      return s_pFileTypes[ i ]->Exists( s_pFileTypes[ i ], pszFileName, pRetPath );

   return pRetPath ? zh_spFileExists( pszFileName, pRetPath ) :
                     zh_fsFileExists( pszFileName );
}

ZH_BOOL zh_fileDelete( const char * pszFileName )
{
   int i = s_fileFindDrv( pszFileName );

   if( i >= 0 )
      return s_pFileTypes[ i ]->Delete( s_pFileTypes[ i ], pszFileName );

   return zh_fsDelete( pszFileName );
}

ZH_BOOL zh_fileRename( const char * pszFileName, const char * pszNewName )
{
   int i = s_fileFindDrv( pszFileName );

   if( i >= 0 )
      return s_pFileTypes[ i ]->Rename( s_pFileTypes[ i ], pszFileName, pszNewName );

   return zh_fsRename( pszFileName, pszNewName );
}

ZH_BOOL zh_fileCopy( const char * pszSrcFile, const char * pszDstFile )
{
   int i = s_fileFindDrv( pszSrcFile );

   if( i >= 0 )
      return s_pFileTypes[ i ]->Copy( s_pFileTypes[ i ], pszSrcFile, pszDstFile );

   return zh_fsCopy( pszSrcFile, pszDstFile );
}

ZH_BOOL zh_fileMove( const char * pszSrcFile, const char * pszDstFile )
{
   int iS = s_fileFindDrv( pszSrcFile ),
       iD = s_fileFindDrv( pszDstFile );

   if( iS == iD )
   {
      if( iS >= 0 ?
          s_pFileTypes[ iS ]->Rename( s_pFileTypes[ iS ], pszSrcFile, pszDstFile ) :
          zh_fsRename( pszSrcFile, pszDstFile ) )
         return ZH_TRUE;
   }

   return zh_fsCopy( pszSrcFile, pszDstFile ) &&
          zh_fileDelete( pszSrcFile );
}

ZH_BOOL zh_fileDirExists( const char * pszDirName )
{
   int i = s_fileFindDrv( pszDirName );

   if( i >= 0 )
      return s_pFileTypes[ i ]->DirExists( s_pFileTypes[ i ], pszDirName );

   return zh_fsDirExists( pszDirName );
}

ZH_BOOL zh_fileDirMake( const char * pszDirName )
{
   int i = s_fileFindDrv( pszDirName );

   if( i >= 0 )
      return s_pFileTypes[ i ]->DirMake( s_pFileTypes[ i ], pszDirName );

   return zh_fsMkDir( pszDirName );
}

ZH_BOOL zh_fileDirRemove( const char * pszDirName )
{
   int i = s_fileFindDrv( pszDirName );

   if( i >= 0 )
      return s_pFileTypes[ i ]->DirRemove( s_pFileTypes[ i ], pszDirName );

   return zh_fsRmDir( pszDirName );
}

double zh_fileDirSpace( const char * pszDirName, ZH_USHORT uiType )
{
   int i = s_fileFindDrv( pszDirName );

   if( i >= 0 )
      return s_pFileTypes[ i ]->DirSpace( s_pFileTypes[ i ], pszDirName, uiType );

   return zh_fsDiskSpace( pszDirName, uiType );
}

PZH_ITEM zh_fileDirectory( const char * pszDirSpec, const char * pszAttr )
{
   int i = s_fileFindDrv( pszDirSpec );

   if( i >= 0 )
      return s_pFileTypes[ i ]->Directory( s_pFileTypes[ i ], pszDirSpec, pszAttr );

   return zh_fsDirectory( pszDirSpec, pszAttr, ZH_TRUE );
}

ZH_BOOL zh_fileTimeGet( const char * pszFileName, long * plJulian, long * plMillisec )
{
   int i = s_fileFindDrv( pszFileName );

   if( i >= 0 )
      return s_pFileTypes[ i ]->TimeGet( s_pFileTypes[ i ], pszFileName, plJulian, plMillisec );

   return zh_fsGetFileTime( pszFileName, plJulian, plMillisec );
}

ZH_BOOL zh_fileTimeSet( const char * pszFileName, long lJulian, long lMillisec )
{
   int i = s_fileFindDrv( pszFileName );

   if( i >= 0 )
      return s_pFileTypes[ i ]->TimeSet( s_pFileTypes[ i ], pszFileName, lJulian, lMillisec );

   return zh_fsSetFileTime( pszFileName, lJulian, lMillisec );
}

ZH_FOFFSET zh_fileSizeGet( const char * pszFileName, ZH_BOOL bUseDirEntry )
{
   int i = s_fileFindDrv( pszFileName );

   if( i >= 0 )
   {
      ZH_FOFFSET nSize = 0;

      if( bUseDirEntry )
      {
         PZH_ITEM pDir = zh_fileDirectory( pszFileName, "HS" );

         if( pDir )
         {
            PZH_ITEM pEntry = zh_arrayGetItemPtr( pDir, 1 );

            if( pEntry )
               nSize = zh_arrayGetNInt( pEntry, F_SIZE );
            zh_itemRelease( pDir );
         }
      }
      else
      {
         PZH_FILE pFile = zh_fileExtOpen( pszFileName, NULL, FO_READ | FO_COMPAT, NULL, NULL );
         if( pFile )
         {
            ZH_ERRCODE uiError;
            nSize = zh_fileSize( pFile );
            uiError = zh_fsError();
            zh_fileClose( pFile );
            zh_fsSetError( uiError );
         }
      }

      return nSize;
   }

   return zh_fsFSize( pszFileName, bUseDirEntry );
}

ZH_BOOL zh_fileAttrGet( const char * pszFileName, ZH_FATTR * pulAttr )
{
   int i = s_fileFindDrv( pszFileName );

   if( i >= 0 )
      return s_pFileTypes[ i ]->AttrGet( s_pFileTypes[ i ], pszFileName, pulAttr );

   return zh_fsGetAttr( pszFileName, pulAttr );
}

ZH_BOOL zh_fileAttrSet( const char * pszFileName, ZH_FATTR ulAttr )
{
   int i = s_fileFindDrv( pszFileName );

   if( i >= 0 )
      return s_pFileTypes[ i ]->AttrSet( s_pFileTypes[ i ], pszFileName, ulAttr );

   return zh_fsSetAttr( pszFileName, ulAttr );
}

ZH_BOOL zh_fileLink( const char * pszExisting, const char * pszNewName )
{
   int i = s_fileFindDrv( pszExisting );

   if( i >= 0 )
      return s_pFileTypes[ i ]->Link( s_pFileTypes[ i ], pszExisting, pszNewName );

   return zh_fsLink( pszExisting, pszNewName );
}

ZH_BOOL zh_fileLinkSym( const char * pszTarget, const char * pszNewName )
{
   int i = s_fileFindDrv( pszTarget );

   if( i >= 0 )
      return s_pFileTypes[ i ]->LinkSym( s_pFileTypes[ i ], pszTarget, pszNewName );

   return zh_fsLinkSym( pszTarget, pszNewName );
}

char * zh_fileLinkRead( const char * pszFileName )
{
   int i = s_fileFindDrv( pszFileName );

   if( i >= 0 )
      return s_pFileTypes[ i ]->LinkRead( s_pFileTypes[ i ], pszFileName );

   return zh_fsLinkRead( pszFileName );
}

PZH_FILE zh_fileExtOpen( const char * pszFileName, const char * pDefExt,
                         ZH_FATTR nExFlags, const char * pPaths,
                         PZH_ITEM pError )
{
   int i = s_fileFindDrv( pszFileName );

   if( i >= 0 )
      return s_pFileTypes[ i ]->Open( s_pFileTypes[ i ], pszFileName, pDefExt, nExFlags, pPaths, pError );

   return s_fileExtOpen( NULL, pszFileName, pDefExt, nExFlags, pPaths, pError );
}

void zh_fileClose( PZH_FILE pFile )
{
   pFile->pFuncs->Close( pFile );
}

ZH_BOOL zh_fileLock( PZH_FILE pFile, ZH_FOFFSET nStart, ZH_FOFFSET nLen,
                     int iType )
{
   return pFile->pFuncs->Lock( pFile, nStart, nLen, iType );
}

int zh_fileLockTest( PZH_FILE pFile, ZH_FOFFSET nStart, ZH_FOFFSET nLen,
                     int iType )
{
   return pFile->pFuncs->LockTest( pFile, nStart, nLen, iType );
}

ZH_SIZE zh_fileRead( PZH_FILE pFile, void * buffer, ZH_SIZE nSize,
                     ZH_MAXINT nTimeout )
{
   return pFile->pFuncs->Read( pFile, buffer, nSize, nTimeout );
}

ZH_SIZE zh_fileWrite( PZH_FILE pFile, const void * buffer, ZH_SIZE nSize,
                      ZH_MAXINT nTimeout )
{
   return pFile->pFuncs->Write( pFile, buffer, nSize, nTimeout );
}

ZH_SIZE zh_fileReadAt( PZH_FILE pFile, void * buffer, ZH_SIZE nSize,
                       ZH_FOFFSET nOffset )
{
   return pFile->pFuncs->ReadAt( pFile, buffer, nSize, nOffset );
}

ZH_SIZE zh_fileWriteAt( PZH_FILE pFile, const void * buffer, ZH_SIZE nSize,
                        ZH_FOFFSET nOffset )
{
   return pFile->pFuncs->WriteAt( pFile, buffer, nSize, nOffset );
}

ZH_BOOL zh_fileTruncAt( PZH_FILE pFile, ZH_FOFFSET nOffset )
{
   return pFile->pFuncs->TruncAt( pFile, nOffset );
}

ZH_FOFFSET zh_fileSeek( PZH_FILE pFile, ZH_FOFFSET nOffset, ZH_USHORT uiFlags )
{
   return pFile->pFuncs->Seek( pFile, nOffset, uiFlags );
}

ZH_FOFFSET zh_fileSize( PZH_FILE pFile )
{
   return pFile->pFuncs->Size( pFile );
}

ZH_BOOL zh_fileEof( PZH_FILE pFile )
{
   return pFile->pFuncs->Eof( pFile );
}

void zh_fileFlush( PZH_FILE pFile, ZH_BOOL fDirty )
{
   pFile->pFuncs->Flush( pFile, fDirty );
}

void zh_fileCommit( PZH_FILE pFile )
{
   pFile->pFuncs->Commit( pFile );
}

ZH_BOOL zh_fileConfigure( PZH_FILE pFile, int iIndex, PZH_ITEM pValue )
{
   return pFile->pFuncs->Configure( pFile, iIndex, pValue );
}

ZH_FHANDLE zh_fileHandle( PZH_FILE pFile )
{
   return pFile->pFuncs->Handle( pFile );
}

/* internal FILE structures only */

PZH_FILE zh_fileCreateTemp( const char * pszDir,
                            const char * pszPrefix,
                            ZH_FATTR ulAttr,
                            char * pszName )
{
   PZH_FILE pFile = NULL;
   ZH_FHANDLE hFile;

   hFile = zh_fsCreateTemp( pszDir, pszPrefix, ulAttr, pszName );
   if( hFile != FS_ERROR )
      pFile = zh_fileNew( hFile, ZH_FALSE, ZH_FALSE, 0, 0, ZH_FALSE );

   return pFile;
}

PZH_FILE zh_fileCreateTempEx( char * pszName,
                              const char * pszDir,
                              const char * pszPrefix,
                              const char * pszExt,
                              ZH_FATTR ulAttr )
{
   PZH_FILE pFile = NULL;
   ZH_FHANDLE hFile;

   hFile = zh_fsCreateTempEx( pszName, pszDir, pszPrefix, pszExt, ulAttr );
   if( hFile != FS_ERROR )
      pFile = zh_fileNew( hFile, ZH_FALSE, ZH_FALSE, 0, 0, ZH_FALSE );

   return pFile;
}

PZH_FILE zh_fileFromHandle( ZH_FHANDLE hFile )
{
   return zh_fileNew( hFile, ZH_FALSE, ZH_FALSE, 0, 0, ZH_FALSE );
}

ZH_BOOL zh_fileDetach( PZH_FILE pFile )
{
   if( pFile )
   {
      if( pFile->pFuncs == s_fileMethods() )
      {
         pFile->hFile = FS_ERROR;
         s_fileClose( pFile );
         return ZH_TRUE;
      }
#if defined( ZH_OS_UNIX )
      else if( pFile->pFuncs == s_fileposMethods() )
      {
         PZH_FILEPOS pFilePos = ( PZH_FILEPOS ) pFile;

         pFilePos->pFile->hFile = FS_ERROR;
         s_fileposClose( pFile );
         return ZH_TRUE;
      }
#endif
   }

   return ZH_FALSE;
}

ZH_BOOL zh_fileIsLocal( PZH_FILE pFile )
{
   if( pFile )
   {
#if defined( ZH_OS_UNIX )
      if( pFile->pFuncs == s_fileMethods() ||
          pFile->pFuncs == s_fileposMethods() )
#else
      if( pFile->pFuncs == s_fileMethods() )
#endif
         return ZH_TRUE;
   }

   return ZH_FALSE;
}

ZH_BOOL zh_fileIsLocalName( const char * pszFileName )
{
   return s_fileFindDrv( pszFileName ) < 0;
}

PZH_FILE zh_filePOpen( const char * pszFileName, const char * pszMode )
{
   PZH_FILE pFile = NULL;
   ZH_FHANDLE hFile;

   hFile = zh_fsPOpen( pszFileName, pszMode );
   if( hFile != FS_ERROR )
      pFile = zh_fileNew( hFile, ZH_FALSE, ZH_FALSE, 0, 0, ZH_FALSE );

   return pFile;
}

ZH_SIZE zh_fileResult( ZH_SIZE nSize )
{
   return nSize == ( ZH_SIZE ) FS_ERROR ? 0 : nSize;
}

#define ZH_FILELOAD_BUFFERSIZE  65536

ZH_BYTE * zh_fileLoadData( PZH_FILE pFile, ZH_SIZE nMaxSize,
                           ZH_SIZE * pnSize )
{
   ZH_BYTE * pFileBuf = NULL;
   ZH_SIZE nSize = 0, nRead, nBufSize;
   ZH_FOFFSET nFileSize = zh_fileSize( pFile );

   if( nFileSize == FS_ERROR ||
       ( nFileSize == 0 && zh_fsError() != 0 ) )
   {
      for( nBufSize = 0;; )
      {
         if( nBufSize == nSize )
         {
            nBufSize += nBufSize == 0 ? ZH_FILELOAD_BUFFERSIZE : nBufSize >> 1;
            if( nMaxSize > 0 && nBufSize > nMaxSize )
            {
               nBufSize = nMaxSize;
               if( nBufSize == nSize )
                  break;
            }
            pFileBuf = ( ZH_BYTE * ) zh_xrealloc( pFileBuf, nBufSize );
         }
         nRead = zh_fileRead( pFile, pFileBuf + nSize, nBufSize - nSize, -1 );
         if( nRead == 0 || nRead == ( ZH_SIZE ) FS_ERROR )
            break;
         nSize += nRead;
      }
   }
   else if( nFileSize > 0 )
   {
      nBufSize = ( ZH_SIZE ) nFileSize;
      if( nMaxSize > 0 && nBufSize > nMaxSize )
         nBufSize = nMaxSize;

      pFileBuf = ( ZH_BYTE * ) zh_xgrab( nBufSize + 1 );
      do
      {
         nRead = zh_fileReadAt( pFile, pFileBuf + nSize, nBufSize - nSize, nSize );
         if( nRead == 0 || nRead == ( ZH_SIZE ) FS_ERROR )
            break;
         nSize += nRead;
      }
      while( nSize < nBufSize );
   }

   if( nSize > 0 )
   {
      pFileBuf = ( ZH_BYTE * ) zh_xrealloc( pFileBuf, nSize + 1 );
      pFileBuf[ nSize ] = '\0';
   }
   else if( pFileBuf )
   {
      zh_xfree( pFileBuf );
      pFileBuf = NULL;
   }

   if( pnSize )
      *pnSize = nSize;

   return pFileBuf;
}

ZH_BYTE * zh_fileLoad( const char * pszFileName, ZH_SIZE nMaxSize,
                       ZH_SIZE * pnSize )
{
   ZH_BYTE * pFileBuf = NULL;
   PZH_FILE pFile = zh_fileExtOpen( pszFileName, NULL,
                                    FO_READ | FO_SHARED | FO_PRIVATE |
                                    FXO_SHARELOCK | FXO_NOSEEKPOS,
                                    NULL, NULL );

   if( pFile != NULL )
   {
      pFileBuf = zh_fileLoadData( pFile, nMaxSize, pnSize );
      zh_fileClose( pFile );
   }
   else if( pnSize )
      *pnSize = 0;

   return pFileBuf;
}
