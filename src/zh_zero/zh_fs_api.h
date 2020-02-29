/*
 * Header file for the Filesys API
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
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

#ifndef ZH_APIFS_H_
#define ZH_APIFS_H_

#include "zh_api.h"
#include "file_io.zhh"

ZH_EXTERN_BEGIN

#define FS_ERROR ( ZH_FHANDLE ) F_ERROR

/* File locking flags */
#define FL_LOCK       0x0000   /* Lock a region */
#define FL_UNLOCK     0x0001   /* Unlock a region */
#define FL_MASK       0x00FF   /* Mask for lock type */

/* Extended file locking flags */
#define FLX_EXCLUSIVE ZH_FLX_EXCLUSIVE  /* Exclusive lock */
#define FLX_SHARED    ZH_FLX_SHARED     /* Shared lock */
#define FLX_WAIT      ZH_FLX_WAIT       /* Wait for lock until success */

/* File inheritance flags */
#define FO_INHERITED  0x0000   /* Spawned processes can inherit this file handle */
#define FO_PRIVATE    0x0080   /* Spawned processes cannot inherit this file handle */

/* Extended file open mode flags */
#define FXO_TRUNCATE  0x0100   /* Create (truncate if exists) */
#define FXO_APPEND    0x0200   /* Create (append if exists) */
#define FXO_UNIQUE    0x0400   /* Create unique file FO_EXCL ??? */
#define FXO_FORCEEXT  0x0800   /* Force default extension */
#define FXO_DEFAULTS  0x1000   /* Use SET command defaults */
#define FXO_DEVICERAW 0x2000   /* Open devices in raw mode */

#define FXO_NOSEEKPOS FXO_DEVICERAW /* seek pos not needed in regular file */
#define FXO_SHARELOCK 0x4000        /* emulate MS-DOS SH_DENY* mode in POSIX OS */
#define FXO_COPYNAME  0x8000        /* copy final szPath into pszFileName */

/* these definitions should be cleared,
 * now they only help to clean lower-level code
 */
#define ZH_FA_FIFO            ZH_FA_TEMPORARY   /* S_ISFIFO() */
#define ZH_FA_FILE            ZH_FA_ARCHIVE     /* S_ISREG() */
#define ZH_FA_BLKDEVICE       ZH_FA_DEVICE      /* S_ISBLK() */
#define ZH_FA_CHRDEVICE       ZH_FA_COMPRESSED  /* S_ISCHR() */
#define ZH_FA_SOCKET          ZH_FA_SPARSE      /* S_ISSOCK() */
#define ZH_FA_LINK            ZH_FA_REPARSE     /* S_ISLNK() */

#define ZH_FA_UGVS            ( ZH_FA_SUID | ZH_FA_SGID | ZH_FA_SVTX )
#define ZH_FA_RWXU            ( ZH_FA_RUSR | ZH_FA_WUSR | ZH_FA_XUSR )
#define ZH_FA_RWXG            ( ZH_FA_RGRP | ZH_FA_WGRP | ZH_FA_XGRP )
#define ZH_FA_RWXO            ( ZH_FA_ROTH | ZH_FA_WOTH | ZH_FA_XOTH )

#if defined( ZH_OS_VXWORKS ) && ! defined( S_ISVTX )
#  define S_ISVTX 0
#endif

/* macros to convert Ziher attributes to POSIX ones */
#define ZH_FA_POSIX_SID(a)    ( ( ( ( a ) & ZH_FA_SVTX ) ? S_ISVTX : 0 ) | \
                                ( ( ( a ) & ZH_FA_SGID ) ? S_ISGID : 0 ) | \
                                ( ( ( a ) & ZH_FA_SUID ) ? S_ISUID : 0 ) )
#define ZH_FA_POSIX_OTH(a)    ( ( ( ( a ) & ZH_FA_XOTH ) ? S_IXOTH : 0 ) | \
                                ( ( ( a ) & ZH_FA_WOTH ) ? S_IWOTH : 0 ) | \
                                ( ( ( a ) & ZH_FA_ROTH ) ? S_IROTH : 0 ) )
#define ZH_FA_POSIX_GRP(a)    ( ( ( ( a ) & ZH_FA_XGRP ) ? S_IXGRP : 0 ) | \
                                ( ( ( a ) & ZH_FA_WGRP ) ? S_IWGRP : 0 ) | \
                                ( ( ( a ) & ZH_FA_RGRP ) ? S_IRGRP : 0 ) )
#define ZH_FA_POSIX_USR(a)    ( ( ( ( a ) & ZH_FA_XUSR ) ? S_IXUSR : 0 ) | \
                                ( ( ( a ) & ZH_FA_WUSR ) ? S_IWUSR : 0 ) | \
                                ( ( ( a ) & ZH_FA_RUSR ) ? S_IRUSR : 0 ) )

#define ZH_FA_POSIX_ATTR(a)   ( ZH_FA_POSIX_OTH(a) | \
                                ZH_FA_POSIX_GRP(a) | \
                                ZH_FA_POSIX_USR(a) | \
                                ZH_FA_POSIX_SID(a) )

extern ZH_EXPORT ZH_BOOL    zh_fsChDir       ( const char * pszDirName ); /* change working directory */
extern ZH_EXPORT ZH_ERRCODE zh_fsChDrv       ( int iDrive ); /* change working drive */
extern ZH_EXPORT void       zh_fsClose       ( ZH_FHANDLE hFileHandle ); /* close a file */
extern ZH_EXPORT void       zh_fsCloseRaw    ( ZH_FHANDLE hFileHandle ); /* close a file without setting zh_fsError() */
extern ZH_EXPORT void       zh_fsCommit      ( ZH_FHANDLE hFileHandle ); /* commit updates of a file */
extern ZH_EXPORT ZH_FHANDLE zh_fsCreate      ( const char * pszFileName, ZH_FATTR ulAttr ); /* create a file */
extern ZH_EXPORT ZH_FHANDLE zh_fsCreateEx    ( const char * pszFileName, ZH_FATTR ulAttr, ZH_USHORT uiFlags ); /* create a file, with specific open mode */
extern ZH_EXPORT ZH_FHANDLE zh_fsCreateTemp  ( const char * pszDir, const char * pszPrefix, ZH_FATTR ulAttr, char * pszName ); /* create a temporary file from components */
extern ZH_EXPORT ZH_FHANDLE zh_fsCreateTempEx( char * pszName, const char * pszDir, const char * pszPrefix, const char * pszExt, ZH_FATTR ulAttr ); /* create a temporary file from components */
extern ZH_EXPORT ZH_ERRCODE zh_fsTempDir     ( char * pszDir ); /* full buffer with system temp directory (or empty on error) */
extern ZH_EXPORT const char * zh_fsCurDir    ( int iDrive ); /* retrieve a static pointer containing current directory for specified drive */
extern ZH_EXPORT ZH_ERRCODE zh_fsCurDirBuff  ( int iDrive, char * pbyBuffer, ZH_SIZE nLen ); /* copy current directory for given drive into a buffer */
extern ZH_EXPORT void       zh_fsBaseDirBuff ( char * pbyBuffer ); /* retrieve the base dir of the executable */
extern ZH_EXPORT int        zh_fsCurDrv      ( void ); /* retrieve current drive number */
extern ZH_EXPORT ZH_BOOL    zh_fsDelete      ( const char * pszFileName ); /* delete a file */
extern ZH_EXPORT ZH_BOOL    zh_fsEof         ( ZH_FHANDLE hFileHandle ); /* determine if an open file is position at end-of-file */
extern ZH_EXPORT ZH_ERRCODE zh_fsError       ( void ); /* retrieve file system error */
extern ZH_EXPORT ZH_ERRCODE zh_fsOsError     ( void ); /* retrieve system dependent file system error */
extern ZH_EXPORT ZH_BOOL    zh_fsFile        ( const char * pszFileName ); /* determine if a file exists */
extern ZH_EXPORT ZH_BOOL    zh_fsIsDirectory ( const char * pszFileName );
extern ZH_EXPORT ZH_FOFFSET zh_fsFSize       ( const char * pszFileName, ZH_BOOL bUseDirEntry ); /* determine the size of a file */
extern ZH_EXPORT ZH_FHANDLE zh_fsExtOpen     ( const char * pszFileName, const char * pDefExt,
                                               ZH_FATTR nFlags, const char * pPaths, PZH_ITEM pError ); /* open a file using default extension and a list of paths */
extern ZH_EXPORT char *     zh_fsExtName     ( const char * pszFileName, const char * pDefExt,
                                               ZH_FATTR nExFlags, const char * pPaths ); /* convert file name for zh_fsExtOpen(), caller must free the returned buffer */
extern ZH_EXPORT ZH_ERRCODE zh_fsIsDrv       ( int iDrive ); /* determine if a drive number is a valid drive */
extern ZH_EXPORT ZH_BOOL    zh_fsIsDevice    ( ZH_FHANDLE hFileHandle ); /* determine if a file is attached to a device (console?) */
extern ZH_EXPORT ZH_BOOL    zh_fsLock        ( ZH_FHANDLE hFileHandle, ZH_ULONG ulStart, ZH_ULONG ulLength, ZH_USHORT uiMode ); /* request a lock on a portion of a file */
extern ZH_EXPORT ZH_BOOL    zh_fsLockLarge   ( ZH_FHANDLE hFileHandle, ZH_FOFFSET nStart,
                                               ZH_FOFFSET nLength, ZH_USHORT uiMode ); /* request a lock on a portion of a file using 64-bit API */
extern ZH_EXPORT int        zh_fsLockTest    ( ZH_FHANDLE hFileHandle, ZH_FOFFSET nStart,
                                               ZH_FOFFSET nLength, ZH_USHORT uiMode );
extern ZH_EXPORT ZH_BOOL    zh_fsMkDir       ( const char * pszDirName ); /* create a directory */
extern ZH_EXPORT ZH_FHANDLE zh_fsOpen        ( const char * pszFileName, ZH_USHORT uiFlags ); /* open a file */
extern ZH_EXPORT ZH_FHANDLE zh_fsOpenEx      ( const char * pszFileName, ZH_USHORT uiFlags, ZH_FATTR nAttr ); /* open or create a file with given attributes */
extern ZH_EXPORT ZH_USHORT  zh_fsRead        ( ZH_FHANDLE hFileHandle, void * pBuff, ZH_USHORT uiCount ); /* read contents of a file into a buffer (<=64 KiB) */
extern ZH_EXPORT ZH_SIZE    zh_fsReadLarge   ( ZH_FHANDLE hFileHandle, void * pBuff, ZH_SIZE nCount ); /* read contents of a file into a buffer (>64 KiB) */
extern ZH_EXPORT ZH_SIZE    zh_fsReadAt      ( ZH_FHANDLE hFileHandle, void * pBuff, ZH_SIZE nCount, ZH_FOFFSET nOffset ); /* read from given offset contents of a file into a buffer (> 64 KiB) */
extern ZH_EXPORT ZH_BOOL    zh_fsRmDir       ( const char * pszDirName ); /* remove a directory */
extern ZH_EXPORT ZH_BOOL    zh_fsRename      ( const char * pszOldName, const char * pszNewName ); /* rename a file */
extern ZH_EXPORT ZH_ULONG   zh_fsSeek        ( ZH_FHANDLE hFileHandle, ZH_LONG lOffset, ZH_USHORT uiMode ); /* reposition an open file */
extern ZH_EXPORT ZH_FOFFSET zh_fsSeekLarge   ( ZH_FHANDLE hFileHandle, ZH_FOFFSET nOffset, ZH_USHORT uiFlags ); /* reposition an open file using 64-bit API */
extern ZH_EXPORT ZH_FOFFSET zh_fsTell        ( ZH_FHANDLE hFileHandle ); /* retrieve the current position of a file */
extern ZH_EXPORT ZH_FOFFSET zh_fsGetSize     ( ZH_FHANDLE hFileHandle ); /* retrieve the size of a file, it may change current seek position */
extern ZH_EXPORT int        zh_fsSetDevMode  ( ZH_FHANDLE hFileHandle, int iDevMode ); /* change the device mode of a file (text/binary) */
extern ZH_EXPORT ZH_BOOL    zh_fsGetFileTime ( const char * pszFileName, long * plJulian, long * plMillisec );
extern ZH_EXPORT ZH_BOOL    zh_fsSetFileTime ( const char * pszFileName, long lJulian, long lMillisec );
extern ZH_EXPORT ZH_BOOL    zh_fsGetAttr     ( const char * pszFileName, ZH_FATTR * pulAttr );
extern ZH_EXPORT ZH_BOOL    zh_fsSetAttr     ( const char * pszFileName, ZH_FATTR ulAttr );
extern ZH_EXPORT ZH_BOOL    zh_fsGetCWD      ( char * pszBuffer, ZH_SIZE nSize );
extern ZH_EXPORT ZH_BOOL    zh_fsSetCWD      ( const char * pszDirName );
extern ZH_EXPORT void       zh_fsSetError    ( ZH_ERRCODE uiError ); /* set the file system OS error number */
extern ZH_EXPORT void       zh_fsSetIOError  ( ZH_BOOL fResult, ZH_USHORT uiOperation ); /* set the file system error number after IO operation */
extern ZH_EXPORT ZH_BOOL    zh_fsTruncAt     ( ZH_FHANDLE hFileHandle, ZH_FOFFSET nOffset ); /* truncate file to given size */
extern ZH_EXPORT ZH_USHORT  zh_fsWrite       ( ZH_FHANDLE hFileHandle, const void * pBuff, ZH_USHORT uiCount ); /* write to an open file from a buffer (<=64K) */
extern ZH_EXPORT ZH_SIZE    zh_fsWriteLarge  ( ZH_FHANDLE hFileHandle, const void * pBuff, ZH_SIZE nCount ); /* write to an open file from a buffer (>64K) */
extern ZH_EXPORT ZH_SIZE    zh_fsWriteAt     ( ZH_FHANDLE hFileHandle, const void * pBuff, ZH_SIZE nCount, ZH_FOFFSET nOffset ); /* write to an open file at given offset from a buffer (>64K) */
extern ZH_EXPORT ZH_FHANDLE zh_fsPOpen       ( const char * pszFileName, const char * pszMode );
extern ZH_EXPORT ZH_BOOL    zh_fsPipeCreate  ( ZH_FHANDLE hPipe[ 2 ] );
extern ZH_EXPORT ZH_BOOL    zh_fsPipeUnblock ( ZH_FHANDLE hPipeHandle );
extern ZH_EXPORT ZH_SIZE    zh_fsPipeIsData  ( ZH_FHANDLE hPipeHandle, ZH_SIZE nBufferSize, ZH_MAXINT nTimeOut );
extern ZH_EXPORT ZH_SIZE    zh_fsPipeRead    ( ZH_FHANDLE hPipeHandle, void * buffer, ZH_SIZE nSize, ZH_MAXINT nTimeOut );
extern ZH_EXPORT ZH_SIZE    zh_fsPipeWrite   ( ZH_FHANDLE hPipeHandle, const void * buffer, ZH_SIZE nSize, ZH_MAXINT nTimeOut );
extern ZH_EXPORT int        zh_fsIsPipeOrSock( ZH_FHANDLE hPipeHandle );
extern ZH_EXPORT ZH_FHANDLE zh_fsGetOsHandle ( ZH_FHANDLE hFileHandle );
extern ZH_EXPORT ZH_ERRCODE zh_fsGetFError   ( void ); /* get FError() flag */
extern ZH_EXPORT void       zh_fsSetFError   ( ZH_ERRCODE uiError ); /* set FError() flag */
extern ZH_EXPORT ZH_BOOL    zh_fsNameExists  ( const char * pszFileName ); /* check if a name exists in the filesystem (wildcard chars not accepted). */
extern ZH_EXPORT ZH_BOOL    zh_fsFileExists  ( const char * pszFileName ); /* check if a file exists (wildcard chars not accepted). */
extern ZH_EXPORT ZH_BOOL    zh_fsDirExists   ( const char * pszDirName ); /* check if a directory exists (wildcard chars not accepted). */
extern ZH_EXPORT ZH_BOOL    zh_fsCopy        ( const char * pszSource, const char * pszDest ); /* copy file */
extern ZH_EXPORT double     zh_fsDiskSpace   ( const char * pszDirName, ZH_USHORT uiType );
extern ZH_EXPORT PZH_ITEM   zh_fsDirectory   ( const char * pszDirSpec, const char * pszAttributes, ZH_BOOL fDateTime );
extern ZH_EXPORT ZH_BOOL    zh_fsLink        ( const char * pszExisting, const char * pszNewFile ); /* create hard link */
extern ZH_EXPORT ZH_BOOL    zh_fsLinkSym     ( const char * pszTarget, const char * pszNewFile ); /* create symbolic (soft) link */
extern ZH_EXPORT char *     zh_fsLinkRead    ( const char * pszFileName ); /* returns the link pointed to */

#if defined( ZH_OS_UNIX ) || defined( __DJGPP__ )
/* for POSIX systems only, hides low-level select()/poll() access,
   intentionally covered by ZH_OS_UNIX / __DJGPP__ macros to generate
   compile time error in code which tries to use it on other platforms */

typedef struct
{
   ZH_FHANDLE  fd;
   ZH_SHORT    events;
   ZH_SHORT    revents;
} ZH_POLLFD, * PZH_POLLFD;

#define ZH_POLLIN    0x0001 /* There is data to read */
#define ZH_POLLPRI   0x0002 /* There is urgent data to read */
#define ZH_POLLOUT   0x0004 /* Writing now will not block */
#define ZH_POLLERR   0x0008 /* Error condition */
#define ZH_POLLHUP   0x0010 /* Hung up */
#define ZH_POLLNVAL  0x0020 /* Invalid polling request */

extern ZH_EXPORT int        zh_fsPoll        ( PZH_POLLFD pPollSet, int iCount, ZH_MAXINT nTimeOut );
extern ZH_EXPORT int        zh_fsCanRead     ( ZH_FHANDLE hFileHandle, ZH_MAXINT nTimeOut );
extern ZH_EXPORT int        zh_fsCanWrite    ( ZH_FHANDLE hFileHandle, ZH_MAXINT nTimeOut );
#endif /* ZH_OS_UNIX */


#define zh_fsFLock( h, s, l )   zh_fsLock( h, s, l, FL_LOCK )
#define zh_fsFUnlock( h, s, l ) zh_fsLock( h, s, l, FL_UNLOCK )

#if defined( ZH_OS_UNIX ) && ! defined( ZH_USE_SHARELOCKS_OFF )
#  define ZH_USE_SHARELOCKS
#  define ZH_SHARELOCK_POS          0x7fffffffUL
#  define ZH_SHARELOCK_SIZE         0x1UL
#  if defined( ZH_USE_BSDLOCKS_OFF )
#     undef ZH_USE_BSDLOCKS
#  elif defined( ZH_OS_LINUX )
      /* default usage of BSD locks in *BSD systems for emulating
       * MS-DOS/Windows DENY_* flags has been disabled because tests
       * on FreeBSD 6.2 and macOS shows that this implementation
       * can create self deadlock when used simultaneously with
       * POSIX locks - thanks to Phil and Lorenzo for locating the
       * problem and tests [druzus]
       */
#     define ZH_USE_BSDLOCKS
#  endif
#endif

#define ZH_MAX_DRIVE_LENGTH   10
#define ZH_MAX_FILE_EXT       10

/* FileName support */
typedef struct
{
   const char * szPath;
   const char * szName;
   const char * szExtension;
   const char * szDrive;
   char   szBuffer[ ZH_PATH_MAX + ZH_MAX_DRIVE_LENGTH + 6 ];
} ZH_FNAME, * PZH_FNAME;

extern ZH_EXPORT PZH_FNAME  zh_fsFNameSplit( const char * pszFileName ); /* Split given filename into path, name and extension */
extern ZH_EXPORT char *     zh_fsFNameMerge( char * pszFileName, PZH_FNAME pFileName ); /* This function joins path, name and extension into a string with a filename */

/* Searchable path support */
typedef struct _ZH_PATHNAMES
{
   char * szPath;
   struct _ZH_PATHNAMES * pNext;
   ZH_BOOL fFree;
} ZH_PATHNAMES;

extern ZH_EXPORT void       zh_fsAddSearchPath( const char * szPath, ZH_PATHNAMES ** pSearchList );
extern ZH_EXPORT void       zh_fsFreeSearchPath( ZH_PATHNAMES * pSearchList );

extern ZH_EXPORT ZH_BOOL    zh_spFile( const char * pszFileName, char * pRetPath );
extern ZH_EXPORT ZH_BOOL    zh_spFileExists( const char * pszFileName, char * pRetPath );
extern ZH_EXPORT ZH_FHANDLE zh_spOpen( const char * pszFileName, ZH_USHORT uiFlags );
extern ZH_EXPORT ZH_FHANDLE zh_spCreate( const char * pszFileName, ZH_FATTR ulAttr );
extern ZH_EXPORT ZH_FHANDLE zh_spCreateEx( const char * pszFileName, ZH_FATTR ulAttr, ZH_USHORT uiFlags );

/* File Find API structure */
typedef struct
{
   char        szName[ ZH_PATH_MAX ];
   char        szDate[ 9 ]; /* in YYYYMMDD format */
   char        szTime[ 9 ]; /* in HH:MM:SS format */
   long        lDate;
   long        lTime;
   ZH_FATTR    attr;
   ZH_FOFFSET  size;

#if defined( _ZH_FFIND_INTERNAL_ )
   /* Private */
   const char * pszFileMask;
   ZH_FATTR     attrmask;
   ZH_BOOL      bFirst;
   char *       pszFree;

   void * info; /* Pointer to the platform specific find info */
#endif
} ZH_FFIND, * PZH_FFIND;

/* File Find API functions */
extern ZH_EXPORT PZH_FFIND zh_fsFindFirst( const char * pszFileMask, ZH_FATTR attrmask );
extern ZH_EXPORT ZH_BOOL   zh_fsFindNext( PZH_FFIND ffind );
extern ZH_EXPORT void      zh_fsFindClose( PZH_FFIND ffind );

/* functions to create, wait and terminate processes */
extern ZH_EXPORT ZH_FHANDLE zh_fsProcessOpen( const char * pszFileName,
                                              ZH_FHANDLE * phStdin, ZH_FHANDLE * phStdout,
                                              ZH_FHANDLE * phStderr,
                                              ZH_BOOL fDetach, ZH_ULONG * pulPID );
extern ZH_EXPORT int        zh_fsProcessRun( const char * pszFileName,
                                             const char * pStdInBuf, ZH_SIZE nStdInLen,
                                             char ** pStdOutPtr, ZH_SIZE * pnStdOut,
                                             char ** pStdErrPtr, ZH_SIZE * pnStdErr,
                                             ZH_BOOL fDetach );
extern ZH_EXPORT int        zh_fsProcessValue( ZH_FHANDLE hProcess, ZH_BOOL fWait );
extern ZH_EXPORT ZH_BOOL    zh_fsProcessClose( ZH_FHANDLE hProcess, ZH_BOOL fGentle );

/* Misc helper functions */
extern ZH_EXPORT ZH_FATTR   zh_fsAttrFromRaw( ZH_FATTR raw_attr );
extern ZH_EXPORT ZH_FATTR   zh_fsAttrToRaw( ZH_FATTR ulAttr );
extern ZH_EXPORT ZH_FATTR   zh_fsAttrEncode( const char * szAttr );
extern ZH_EXPORT char *     zh_fsAttrDecode( ZH_FATTR ulAttr, char * szAttr );

extern ZH_EXPORT ZH_BOOL      zh_fsMaxFilesError( void );
extern ZH_EXPORT const char * zh_fsNameConv( const char * pszFileName, char ** pszFree );
#if defined( ZH_OS_WIN )
extern ZH_EXPORT ZH_WCHAR *   zh_fsNameConvU16( const char * pszFileName );
#endif


/* Ziher file functions with shared file handles and locks
 * (buffers in the future)
 */

#if defined( _ZH_FILE_IMPLEMENTATION_ ) || defined( _ZH_FILE_INTERNAL_ )

#  define ZH_FILE_TYPE_MAX    128

   struct _ZH_FILE;
   typedef struct _ZH_FILE * PZH_FILE;
   typedef const struct _ZH_FILE_FUNCS * PZH_FILE_FUNCS;

   typedef struct _ZH_FILE_FUNCS
   {
      ZH_BOOL     ( * Accept )      ( PZH_FILE_FUNCS pFuncs, const char * pszFileName );

      ZH_BOOL     ( * Exists )      ( PZH_FILE_FUNCS pFuncs, const char * pszFileName, char * pRetPath );
      ZH_BOOL     ( * Delete )      ( PZH_FILE_FUNCS pFuncs, const char * pszFileName );
      ZH_BOOL     ( * Rename )      ( PZH_FILE_FUNCS pFuncs, const char * pszFileName, const char * pszNewName );
      ZH_BOOL     ( * Copy )        ( PZH_FILE_FUNCS pFuncs, const char * pszSrcFile, const char * pszDstFile );

      ZH_BOOL     ( * DirExists )   ( PZH_FILE_FUNCS pFuncs, const char * pszDirName );
      ZH_BOOL     ( * DirMake )     ( PZH_FILE_FUNCS pFuncs, const char * pszDirName );
      ZH_BOOL     ( * DirRemove )   ( PZH_FILE_FUNCS pFuncs, const char * pszDirName );
      double      ( * DirSpace )    ( PZH_FILE_FUNCS pFuncs, const char * pszDirName, ZH_USHORT uiType );
      PZH_ITEM    ( * Directory )   ( PZH_FILE_FUNCS pFuncs, const char * pszDirSpec, const char * pszAttr );

      ZH_BOOL     ( * TimeGet )     ( PZH_FILE_FUNCS pFuncs, const char * pszFileName, long * plJulian, long * plMillisec );
      ZH_BOOL     ( * TimeSet )     ( PZH_FILE_FUNCS pFuncs, const char * pszFileName, long lJulian, long lMillisec );
      ZH_BOOL     ( * AttrGet )     ( PZH_FILE_FUNCS pFuncs, const char * pszFileName, ZH_FATTR * pulAttr );
      ZH_BOOL     ( * AttrSet )     ( PZH_FILE_FUNCS pFuncs, const char * pszFileName, ZH_FATTR ulAttr );

      ZH_BOOL     ( * Link )        ( PZH_FILE_FUNCS pFuncs, const char * pszExisting, const char * pszNewName );
      ZH_BOOL     ( * LinkSym )     ( PZH_FILE_FUNCS pFuncs, const char * pszTarget, const char * pszNewName );
      char *      ( * LinkRead )    ( PZH_FILE_FUNCS pFuncs, const char * pszFileName );

      PZH_FILE    ( * Open )        ( PZH_FILE_FUNCS pFuncs, const char * pszFileName, const char * pDefExt,
                                      ZH_FATTR nExFlags, const char * pPaths, PZH_ITEM pError );

      void        ( * Close )       ( PZH_FILE pFile );
      ZH_BOOL     ( * Lock )        ( PZH_FILE pFile, ZH_FOFFSET nStart, ZH_FOFFSET nLen, int iType );
      int         ( * LockTest )    ( PZH_FILE pFile, ZH_FOFFSET nStart, ZH_FOFFSET nLen, int iType );
      ZH_SIZE     ( * Read )        ( PZH_FILE pFile, void * buffer, ZH_SIZE nSize, ZH_MAXINT nTimeout );
      ZH_SIZE     ( * Write )       ( PZH_FILE pFile, const void * buffer, ZH_SIZE nSize, ZH_MAXINT nTimeout );
      ZH_SIZE     ( * ReadAt )      ( PZH_FILE pFile, void * buffer, ZH_SIZE nSize, ZH_FOFFSET nOffset );
      ZH_SIZE     ( * WriteAt )     ( PZH_FILE pFile, const void * buffer, ZH_SIZE nSize, ZH_FOFFSET nOffset );
      ZH_BOOL     ( * TruncAt )     ( PZH_FILE pFile, ZH_FOFFSET nOffset );
      ZH_FOFFSET  ( * Seek )        ( PZH_FILE pFile, ZH_FOFFSET nOffset, ZH_USHORT uiFlags );
      ZH_FOFFSET  ( * Size )        ( PZH_FILE pFile );
      ZH_BOOL     ( * Eof )         ( PZH_FILE pFile );
      void        ( * Flush )       ( PZH_FILE pFile, ZH_BOOL fDirty );
      void        ( * Commit )      ( PZH_FILE pFile );
      ZH_BOOL     ( * Configure )   ( PZH_FILE pFile, int iIndex, PZH_ITEM pValue );
      ZH_FHANDLE  ( * Handle )      ( PZH_FILE pFile );
   }
   ZH_FILE_FUNCS;

   extern ZH_EXPORT ZH_BOOL zh_fileRegisterFull( const ZH_FILE_FUNCS * pFuncs );
   extern ZH_EXPORT ZH_BOOL zh_fileRegisterPart( ZH_FILE_FUNCS * pFuncs );
#else
   typedef void * PZH_FILE;
#endif

extern ZH_EXPORT ZH_BOOL      zh_fileExists     ( const char * pszFileName, char * pRetPath );
extern ZH_EXPORT ZH_BOOL      zh_fileDelete     ( const char * pszFileName );
extern ZH_EXPORT ZH_BOOL      zh_fileRename     ( const char * pszFileName, const char * pszNewName );
extern ZH_EXPORT ZH_BOOL      zh_fileCopy       ( const char * pszSrcFile, const char * pszDstFile );
extern ZH_EXPORT ZH_BOOL      zh_fileMove       ( const char * pszSrcFile, const char * pszDstFile );

extern ZH_EXPORT ZH_BOOL      zh_fileDirExists  ( const char * pszDirName );
extern ZH_EXPORT ZH_BOOL      zh_fileDirMake    ( const char * pszDirName );
extern ZH_EXPORT ZH_BOOL      zh_fileDirRemove  ( const char * pszDirName );
extern ZH_EXPORT double       zh_fileDirSpace   ( const char * pszDirName, ZH_USHORT uiType );
extern ZH_EXPORT PZH_ITEM     zh_fileDirectory  ( const char * pszDirSpec, const char * pszAttr );

extern ZH_EXPORT ZH_FOFFSET   zh_fileSizeGet    ( const char * pszFileName, ZH_BOOL bUseDirEntry );
extern ZH_EXPORT ZH_BOOL      zh_fileTimeGet    ( const char * pszFileName, long * plJulian, long * plMillisec );
extern ZH_EXPORT ZH_BOOL      zh_fileTimeSet    ( const char * pszFileName, long lJulian, long lMillisec );
extern ZH_EXPORT ZH_BOOL      zh_fileAttrGet    ( const char * pszFileName, ZH_FATTR * pulAttr );
extern ZH_EXPORT ZH_BOOL      zh_fileAttrSet    ( const char * pszFileName, ZH_FATTR ulAttr );

extern ZH_EXPORT ZH_BOOL      zh_fileLink       ( const char * pszExisting, const char * pszNewName );
extern ZH_EXPORT ZH_BOOL      zh_fileLinkSym    ( const char * pszTarget, const char * pszNewName );
extern ZH_EXPORT char *       zh_fileLinkRead   ( const char * pszFileName );

extern ZH_EXPORT PZH_FILE     zh_fileExtOpen    ( const char * pszFileName, const char * pDefExt,
                                                  ZH_FATTR nExFlags, const char * pPaths,
                                                  PZH_ITEM pError );
extern ZH_EXPORT void         zh_fileClose      ( PZH_FILE pFile );
extern ZH_EXPORT ZH_BOOL      zh_fileLock       ( PZH_FILE pFile, ZH_FOFFSET nStart, ZH_FOFFSET nLen, int iType );
extern ZH_EXPORT int          zh_fileLockTest   ( PZH_FILE pFile, ZH_FOFFSET nStart, ZH_FOFFSET nLen, int iType );
extern ZH_EXPORT ZH_SIZE      zh_fileRead       ( PZH_FILE pFile, void * buffer, ZH_SIZE nSize, ZH_MAXINT nTimeout );
extern ZH_EXPORT ZH_SIZE      zh_fileWrite      ( PZH_FILE pFile, const void * buffer, ZH_SIZE nSize, ZH_MAXINT nTimeout );
extern ZH_EXPORT ZH_SIZE      zh_fileReadAt     ( PZH_FILE pFile, void * buffer, ZH_SIZE nSize, ZH_FOFFSET nOffset );
extern ZH_EXPORT ZH_SIZE      zh_fileWriteAt    ( PZH_FILE pFile, const void * buffer, ZH_SIZE nSize, ZH_FOFFSET nOffset );
extern ZH_EXPORT ZH_BOOL      zh_fileTruncAt    ( PZH_FILE pFile, ZH_FOFFSET nOffset );
extern ZH_EXPORT ZH_FOFFSET   zh_fileSeek       ( PZH_FILE pFile, ZH_FOFFSET nOffset, ZH_USHORT uiFlags );
extern ZH_EXPORT ZH_FOFFSET   zh_fileSize       ( PZH_FILE pFile );
extern ZH_EXPORT ZH_BOOL      zh_fileEof        ( PZH_FILE pFile );
extern ZH_EXPORT void         zh_fileFlush      ( PZH_FILE pFile, ZH_BOOL fDirty );
extern ZH_EXPORT void         zh_fileCommit     ( PZH_FILE pFile );
extern ZH_EXPORT ZH_BOOL      zh_fileConfigure  ( PZH_FILE pFile, int iIndex, PZH_ITEM pValue );
extern ZH_EXPORT ZH_FHANDLE   zh_fileHandle     ( PZH_FILE pFile );

extern ZH_EXPORT PZH_FILE     zh_fileCreateTemp ( const char * pszDir, const char * pszPrefix,
                                                  ZH_FATTR ulAttr, char * pszName );
extern ZH_EXPORT PZH_FILE     zh_fileCreateTempEx( char * pszName,
                                                   const char * pszDir,
                                                   const char * pszPrefix,
                                                   const char * pszExt,
                                                   ZH_FATTR ulAttr );
extern ZH_EXPORT PZH_FILE     zh_filePOpen( const char * pszFileName, const char * pszMode );
extern ZH_EXPORT PZH_FILE     zh_fileFromHandle( ZH_FHANDLE hFile );
extern ZH_EXPORT ZH_BOOL      zh_fileDetach( PZH_FILE pFile );
extern ZH_EXPORT ZH_BOOL      zh_fileIsLocal( PZH_FILE pFile );
extern ZH_EXPORT ZH_BOOL      zh_fileIsLocalName( const char * pszFileName );
extern ZH_EXPORT ZH_SIZE      zh_fileResult( ZH_SIZE nSize );
extern ZH_EXPORT ZH_BYTE *    zh_fileLoad( const char * pszFileName, ZH_SIZE nMaxSize, ZH_SIZE * pnSize );
extern ZH_EXPORT ZH_BYTE *    zh_fileLoadData( PZH_FILE pFile, ZH_SIZE nMaxSize, ZH_SIZE * pnSize );

/* interface to ZH level zh_vf*() file pointer items */
extern ZH_EXPORT PZH_FILE     zh_fileParam( int iParam );
extern ZH_EXPORT PZH_FILE     zh_fileParamGet( int iParam );
extern ZH_EXPORT PZH_FILE     zh_fileItemGet( PZH_ITEM pItem );
extern ZH_EXPORT PZH_ITEM     zh_fileItemPut( PZH_ITEM pItem, PZH_FILE pFile );
extern ZH_EXPORT void         zh_fileItemClear( PZH_ITEM pItem );

#define ZH_FILE_ERR_UNSUPPORTED  ( ( ZH_ERRCODE ) FS_ERROR )

/* wrapper to fopen() which calls zh_fsNameConv() */
extern ZH_EXPORT FILE *       zh_fopen( const char *path, const char *mode );

ZH_EXTERN_END

#endif /* ZH_APIFS_H_ */
