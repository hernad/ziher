/*
 * Dummy I/O driver initialization
 *
 * Copyright 2014 Przemyslaw Czerpak
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

/* this has to be declared before zh_fs_api.h is included */
#define _ZH_FILE_IMPLEMENTATION_

#include "zh_api.h"
#include "zh_fs_api.h"

static ZH_BOOL s_fileAccept( PZH_FILE_FUNCS pFuncs, const char * pszFileName )
{
   ZH_SYMBOL_UNUSED( pFuncs );
   ZH_SYMBOL_UNUSED( pszFileName );

   return ZH_FALSE;
}

static ZH_BOOL s_fileExists( PZH_FILE_FUNCS pFuncs, const char * pszFileName, char * pRetPath )
{
   ZH_SYMBOL_UNUSED( pFuncs );
   ZH_SYMBOL_UNUSED( pszFileName );
   ZH_SYMBOL_UNUSED( pRetPath );

   zh_fsSetError( ZH_FILE_ERR_UNSUPPORTED );

   return ZH_FALSE;
}

static ZH_BOOL s_fileDelete( PZH_FILE_FUNCS pFuncs, const char * pszFileName )
{
   ZH_SYMBOL_UNUSED( pFuncs );
   ZH_SYMBOL_UNUSED( pszFileName );

   zh_fsSetError( ZH_FILE_ERR_UNSUPPORTED );

   return ZH_FALSE;
}

static ZH_BOOL s_fileRename( PZH_FILE_FUNCS pFuncs, const char * pszName, const char * pszNewName )
{
   ZH_SYMBOL_UNUSED( pFuncs );
   ZH_SYMBOL_UNUSED( pszName );
   ZH_SYMBOL_UNUSED( pszNewName );

   zh_fsSetError( ZH_FILE_ERR_UNSUPPORTED );

   return ZH_FALSE;
}

static ZH_BOOL s_fileCopy( PZH_FILE_FUNCS pFuncs, const char * pszSrcFile, const char * pszDstFile )
{
   ZH_SYMBOL_UNUSED( pFuncs );

   return zh_fsCopy( pszSrcFile, pszDstFile );
}

static ZH_BOOL s_fileDirExists( PZH_FILE_FUNCS pFuncs, const char * pszDirName )
{
   ZH_SYMBOL_UNUSED( pFuncs );
   ZH_SYMBOL_UNUSED( pszDirName );

   zh_fsSetError( ZH_FILE_ERR_UNSUPPORTED );

   return ZH_FALSE;
}

static ZH_BOOL s_fileDirMake( PZH_FILE_FUNCS pFuncs, const char * pszDirName )
{
   ZH_SYMBOL_UNUSED( pFuncs );
   ZH_SYMBOL_UNUSED( pszDirName );

   zh_fsSetError( ZH_FILE_ERR_UNSUPPORTED );

   return ZH_FALSE;
}

static ZH_BOOL s_fileDirRemove( PZH_FILE_FUNCS pFuncs, const char * pszDirName )
{
   ZH_SYMBOL_UNUSED( pFuncs );
   ZH_SYMBOL_UNUSED( pszDirName );

   zh_fsSetError( ZH_FILE_ERR_UNSUPPORTED );

   return ZH_FALSE;
}

static double s_fileDirSpace( PZH_FILE_FUNCS pFuncs, const char * pszDirName, ZH_USHORT uiType )
{
   ZH_SYMBOL_UNUSED( pFuncs );
   ZH_SYMBOL_UNUSED( pszDirName );
   ZH_SYMBOL_UNUSED( uiType );

   zh_fsSetError( ZH_FILE_ERR_UNSUPPORTED );

   return 0.0;
}

static PZH_ITEM s_fileDirectory( PZH_FILE_FUNCS pFuncs, const char * pszDirSpec, const char * pszAttr )
{
   ZH_SYMBOL_UNUSED( pFuncs );
   ZH_SYMBOL_UNUSED( pszDirSpec );
   ZH_SYMBOL_UNUSED( pszAttr );

   zh_fsSetError( ZH_FILE_ERR_UNSUPPORTED );

   return NULL;
}

static ZH_BOOL s_fileTimeGet( PZH_FILE_FUNCS pFuncs, const char * pszFileName, long * plJulian, long * plMillisec )
{
   ZH_SYMBOL_UNUSED( pFuncs );
   ZH_SYMBOL_UNUSED( pszFileName );
   ZH_SYMBOL_UNUSED( plJulian );
   ZH_SYMBOL_UNUSED( plMillisec );

   zh_fsSetError( ZH_FILE_ERR_UNSUPPORTED );

   return ZH_FALSE;
}

static ZH_BOOL s_fileTimeSet( PZH_FILE_FUNCS pFuncs, const char * pszFileName, long lJulian, long lMillisec )
{
   ZH_SYMBOL_UNUSED( pFuncs );
   ZH_SYMBOL_UNUSED( pszFileName );
   ZH_SYMBOL_UNUSED( lJulian );
   ZH_SYMBOL_UNUSED( lMillisec );

   zh_fsSetError( ZH_FILE_ERR_UNSUPPORTED );

   return ZH_FALSE;
}

static ZH_BOOL s_fileAttrGet( PZH_FILE_FUNCS pFuncs, const char * pszFileName, ZH_FATTR * pnAttr )
{
   ZH_SYMBOL_UNUSED( pFuncs );
   ZH_SYMBOL_UNUSED( pszFileName );
   ZH_SYMBOL_UNUSED( pnAttr );

   zh_fsSetError( ZH_FILE_ERR_UNSUPPORTED );

   return ZH_FALSE;
}

static ZH_BOOL s_fileAttrSet( PZH_FILE_FUNCS pFuncs, const char * pszFileName, ZH_FATTR nAttr )
{
   ZH_SYMBOL_UNUSED( pFuncs );
   ZH_SYMBOL_UNUSED( pszFileName );
   ZH_SYMBOL_UNUSED( nAttr );

   zh_fsSetError( ZH_FILE_ERR_UNSUPPORTED );

   return ZH_FALSE;
}

static ZH_BOOL s_fileLink( PZH_FILE_FUNCS pFuncs, const char * pszExisting, const char * pszNewName )
{
   ZH_SYMBOL_UNUSED( pFuncs );
   ZH_SYMBOL_UNUSED( pszExisting );
   ZH_SYMBOL_UNUSED( pszNewName );

   zh_fsSetError( ZH_FILE_ERR_UNSUPPORTED );

   return ZH_FALSE;
}

static ZH_BOOL s_fileLinkSym( PZH_FILE_FUNCS pFuncs, const char * pszTarget, const char * pszNewName )
{
   ZH_SYMBOL_UNUSED( pFuncs );
   ZH_SYMBOL_UNUSED( pszTarget );
   ZH_SYMBOL_UNUSED( pszNewName );

   zh_fsSetError( ZH_FILE_ERR_UNSUPPORTED );

   return ZH_FALSE;
}

static char * s_fileLinkRead( PZH_FILE_FUNCS pFuncs, const char * pszFileName )
{
   ZH_SYMBOL_UNUSED( pFuncs );
   ZH_SYMBOL_UNUSED( pszFileName );

   zh_fsSetError( ZH_FILE_ERR_UNSUPPORTED );

   return NULL;
}

static PZH_FILE s_fileOpen( PZH_FILE_FUNCS pFuncs, const char * pszName,
                            const char * pszDefExt, ZH_FATTR nExFlags,
                            const char * pPaths, PZH_ITEM pError )
{
   ZH_SYMBOL_UNUSED( pFuncs );
   ZH_SYMBOL_UNUSED( pszName );
   ZH_SYMBOL_UNUSED( pszDefExt );
   ZH_SYMBOL_UNUSED( nExFlags );
   ZH_SYMBOL_UNUSED( pszDefExt );
   ZH_SYMBOL_UNUSED( pPaths );
   ZH_SYMBOL_UNUSED( pError );

   zh_fsSetError( ZH_FILE_ERR_UNSUPPORTED );

   return NULL;
}

static void s_fileClose( PZH_FILE pFile )
{
   ZH_SYMBOL_UNUSED( pFile );

   zh_fsSetError( ZH_FILE_ERR_UNSUPPORTED );
}

static ZH_BOOL s_fileLock( PZH_FILE pFile, ZH_FOFFSET nStart,
                           ZH_FOFFSET nLen, int iType )
{
   ZH_SYMBOL_UNUSED( pFile );
   ZH_SYMBOL_UNUSED( nStart );
   ZH_SYMBOL_UNUSED( nLen );
   ZH_SYMBOL_UNUSED( iType );

   zh_fsSetError( ZH_FILE_ERR_UNSUPPORTED );

   return ZH_FALSE;
}

static int s_fileLockTest( PZH_FILE pFile, ZH_FOFFSET nStart,
                           ZH_FOFFSET nLen, int iType )
{
   ZH_SYMBOL_UNUSED( pFile );
   ZH_SYMBOL_UNUSED( nStart );
   ZH_SYMBOL_UNUSED( nLen );
   ZH_SYMBOL_UNUSED( iType );

   zh_fsSetError( ZH_FILE_ERR_UNSUPPORTED );

   return -1;
}

static ZH_SIZE s_fileRead( PZH_FILE pFile, void * data,
                           ZH_SIZE nSize, ZH_MAXINT timeout )
{
   ZH_SYMBOL_UNUSED( pFile );
   ZH_SYMBOL_UNUSED( data );
   ZH_SYMBOL_UNUSED( nSize );
   ZH_SYMBOL_UNUSED( timeout );

   zh_fsSetError( ZH_FILE_ERR_UNSUPPORTED );

   return 0;
}

static ZH_SIZE s_fileWrite( PZH_FILE pFile, const void * data,
                            ZH_SIZE nSize, ZH_MAXINT timeout )
{
   ZH_SYMBOL_UNUSED( pFile );
   ZH_SYMBOL_UNUSED( data );
   ZH_SYMBOL_UNUSED( nSize );
   ZH_SYMBOL_UNUSED( timeout );

   zh_fsSetError( ZH_FILE_ERR_UNSUPPORTED );

   return 0;
}

static ZH_SIZE s_fileReadAt( PZH_FILE pFile, void * buffer,
                             ZH_SIZE nSize, ZH_FOFFSET nOffset )
{
   ZH_SYMBOL_UNUSED( pFile );
   ZH_SYMBOL_UNUSED( buffer );
   ZH_SYMBOL_UNUSED( nSize );
   ZH_SYMBOL_UNUSED( nOffset );

   zh_fsSetError( ZH_FILE_ERR_UNSUPPORTED );

   return 0;
}

static ZH_SIZE s_fileWriteAt( PZH_FILE pFile, const void * buffer,
                              ZH_SIZE nSize, ZH_FOFFSET nOffset )
{
   ZH_SYMBOL_UNUSED( pFile );
   ZH_SYMBOL_UNUSED( buffer );
   ZH_SYMBOL_UNUSED( nSize );
   ZH_SYMBOL_UNUSED( nOffset );

   zh_fsSetError( ZH_FILE_ERR_UNSUPPORTED );

   return 0;
}

static ZH_BOOL s_fileTruncAt( PZH_FILE pFile, ZH_FOFFSET nOffset )
{
   ZH_SYMBOL_UNUSED( pFile );
   ZH_SYMBOL_UNUSED( nOffset );

   zh_fsSetError( ZH_FILE_ERR_UNSUPPORTED );

   return ZH_FALSE;
}

static ZH_FOFFSET s_fileSeek( PZH_FILE pFile, ZH_FOFFSET nOffset,
                              ZH_USHORT uiFlags )
{
   ZH_SYMBOL_UNUSED( pFile );
   ZH_SYMBOL_UNUSED( nOffset );
   ZH_SYMBOL_UNUSED( uiFlags );

   zh_fsSetError( ZH_FILE_ERR_UNSUPPORTED );

   return 0;
}

static ZH_FOFFSET s_fileSize( PZH_FILE pFile )
{
   ZH_SYMBOL_UNUSED( pFile );

   zh_fsSetError( ZH_FILE_ERR_UNSUPPORTED );

   return 0;
}

static ZH_BOOL s_fileEof( PZH_FILE pFile )
{
   ZH_SYMBOL_UNUSED( pFile );

   zh_fsSetError( ZH_FILE_ERR_UNSUPPORTED );

   return ZH_TRUE;
}

static void s_fileFlush( PZH_FILE pFile, ZH_BOOL fDirty )
{
   ZH_SYMBOL_UNUSED( pFile );
   ZH_SYMBOL_UNUSED( fDirty );

   zh_fsSetError( ZH_FILE_ERR_UNSUPPORTED );
}

static void s_fileCommit( PZH_FILE pFile )
{
   ZH_SYMBOL_UNUSED( pFile );

   zh_fsSetError( ZH_FILE_ERR_UNSUPPORTED );
}

static ZH_BOOL s_fileConfigure( PZH_FILE pFile, int iIndex, PZH_ITEM pValue )
{
   ZH_SYMBOL_UNUSED( pFile );
   ZH_SYMBOL_UNUSED( iIndex );
   ZH_SYMBOL_UNUSED( pValue );

   zh_fsSetError( ZH_FILE_ERR_UNSUPPORTED );

   return ZH_FALSE;
}

static ZH_FHANDLE s_fileHandle( PZH_FILE pFile )
{
   ZH_SYMBOL_UNUSED( pFile );

   zh_fsSetError( ZH_FILE_ERR_UNSUPPORTED );

   return ( ZH_FHANDLE ) FS_ERROR;
}

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

   s_fileOpen,
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

typedef ZH_BOOL ( * ZH_FILE_FUNC )( PZH_FILE_FUNCS pFuncs, const char * );
#define ZH_FILE_FUNC_COUNT ( sizeof( ZH_FILE_FUNCS ) / sizeof( ZH_FILE_FUNC ) )

ZH_BOOL zh_fileRegisterPart( ZH_FILE_FUNCS * pFuncs )
{
   const ZH_FILE_FUNC * pDummyFunc;
   ZH_FILE_FUNC * pFunction;
   int iCount;

   pDummyFunc = &s_fileFuncs.Accept;
   pFunction = &pFuncs->Accept;

   for( iCount = 0; iCount < ( int ) ZH_FILE_FUNC_COUNT;
        iCount++, pDummyFunc++, pFunction++ )
   {
      if( * pFunction == NULL )
         * pFunction = * pDummyFunc;
   }

   return zh_fileRegisterFull( pFuncs );
}
