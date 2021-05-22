/*
 * IOUSRD - library to create new FILE IO redirectors at .zh level
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
#include "zh_error_api.h"
#include "zh_item_api.h"
#include "zh_xvm.h"
#include "zh_stack.h"
#include "zh_thread.h"

#include "io_us_redirection.zhh"

#define ZH_FILE_ERR_UNSUPPORTED  ( ( ZH_ERRCODE ) FS_ERROR )

typedef struct _ZH_IOUSR
{
   ZH_FILE_FUNCS  funcs;
   char *         prefix;
   int            prefix_len;
   PZH_SYMBOL       prg_funcs[ IOUSR_METHODCOUNT ];
}
ZH_IOUSR, * PZH_IOUSR;

typedef struct _ZH_FILE
{
   const ZH_FILE_FUNCS * pFuncs;
   PZH_ITEM              pFileItm;
}
ZH_FILE;

static ZH_CRITICAL_NEW( s_iousrMtx );
#define ZH_IOUSR_LOCK()       do { zh_threadEnterCriticalSection( &s_iousrMtx )
#define ZH_IOUSR_UNLOCK()     zh_threadLeaveCriticalSection( &s_iousrMtx ); } while( 0 )

static int s_iCount = 0;
static PZH_IOUSR s_ioUsrs[ ZH_FILE_TYPE_MAX ];

static void s_errRT_IOUSR( ZH_ERRCODE errGenCode, ZH_ERRCODE errSubCode,
                           const char * szDescription )
{
   PZH_ITEM pError, pArray;

   pError = zh_errRT_New( ES_ERROR, "IOUSR", errGenCode, errSubCode,
                          szDescription, ZH_ERR_FUNCNAME, 0, EF_NONE );
   pArray = zh_arrayBaseParams();
   if( pArray )
   {
      zh_errPutArgsArray( pError, pArray );
      zh_itemRelease( pArray );
   }
   zh_errLaunch( pError );
   zh_itemRelease( pError );
}

static void s_iousrFreeAll( void * cargo )
{
   ZH_SYMBOL_UNUSED( cargo );

   while( s_iCount > 0 )
   {
      PZH_IOUSR pIO = s_ioUsrs[ --s_iCount ];

      zh_xfree( pIO->prefix );
      zh_xfree( pIO );
   }
}

static PZH_FILE s_fileNew( PZH_IOUSR pIO, PZH_ITEM pFileItm )
{
   PZH_FILE pFile = ( PZH_FILE ) zh_xgrab( sizeof( ZH_FILE ) );

   pFile->pFuncs = &pIO->funcs;
   pFile->pFileItm = pFileItm;

   return pFile;
}

static PZH_IOUSR s_iousrAddNew( const char * pszPrefix )
{
   PZH_IOUSR pIO = NULL;
   int iCount;

   ZH_IOUSR_LOCK();

   iCount = s_iCount;
   while( --iCount >= 0 )
   {
      if( zh_stricmp( pszPrefix, s_ioUsrs[ iCount ]->prefix ) == 0 )
         break;
   }
   if( iCount < 0 )
   {
      if( s_iCount == 0 )
         zh_vmAtQuit( s_iousrFreeAll, NULL );
      pIO = ( PZH_IOUSR ) zh_xgrabz( sizeof( ZH_IOUSR ) );
      pIO->prefix = zh_strdup( pszPrefix );
      pIO->prefix_len = ( int ) strlen( pszPrefix );
      s_ioUsrs[ s_iCount++ ] = pIO;
   }

   ZH_IOUSR_UNLOCK();

   return pIO;
}

#define s_hasMethod( pIO, iMethod ) \
                     ( ( pIO )->prg_funcs[ ( iMethod ) - 1 ] != NULL )

#define s_getUsrIO( p )       ( ( PZH_IOUSR ) ZH_UNCONST( p ) )

static void s_pushMethod( PZH_IOUSR pIO, int iMethod )
{
   zh_vmPushSymbol( pIO->prg_funcs[ iMethod - 1 ] );
   zh_vmPushNil();
}

static ZH_BOOL s_fileAccept( PZH_FILE_FUNCS pFuncs, const char * pszFileName )
{
   PZH_IOUSR pIO = s_getUsrIO( pFuncs );
   ZH_BOOL fResult = ZH_FALSE;

   if( zh_strnicmp( pszFileName, pIO->prefix, pIO->prefix_len ) == 0 )
   {
      if( s_hasMethod( pIO, IOUSR_ACCEPT ) )
      {
         s_pushMethod( pIO, IOUSR_ACCEPT );
         zh_vmPushString( pszFileName, strlen( pszFileName ) );
         zh_vmDo( 1 );
         fResult = zh_parl( -1 );
      }
      else if( pIO->prefix_len > 0 )
         fResult = ZH_TRUE;
   }

   return fResult;
}

static ZH_BOOL s_fileExists( PZH_FILE_FUNCS pFuncs, const char * pszFileName, char * pRetPath )
{
   PZH_IOUSR pIO = s_getUsrIO( pFuncs );

   ZH_SYMBOL_UNUSED( pRetPath );

   s_pushMethod( pIO, IOUSR_EXISTS );
   zh_vmPushString( pszFileName, strlen( pszFileName ) );
   zh_vmDo( 1 );

   return zh_parl( -1 );
}

static ZH_BOOL s_fileDelete( PZH_FILE_FUNCS pFuncs, const char * pszFileName )
{
   PZH_IOUSR pIO = s_getUsrIO( pFuncs );

   s_pushMethod( pIO, IOUSR_DELETE );
   zh_vmPushString( pszFileName, strlen( pszFileName ) );
   zh_vmDo( 1 );

   return zh_parl( -1 );
}

static ZH_BOOL s_fileRename( PZH_FILE_FUNCS pFuncs, const char * pszName, const char * pszNewName )
{
   PZH_IOUSR pIO = s_getUsrIO( pFuncs );

   s_pushMethod( pIO, IOUSR_RENAME );
   zh_vmPushString( pszName, strlen( pszName ) );
   zh_vmPushString( pszNewName, strlen( pszNewName ) );
   zh_vmDo( 2 );

   return zh_parl( -1 );
}

static ZH_BOOL s_fileCopy( PZH_FILE_FUNCS pFuncs, const char * pszSrcFile, const char * pszDstFile )
{
   PZH_IOUSR pIO = s_getUsrIO( pFuncs );

   s_pushMethod( pIO, IOUSR_COPY );
   zh_vmPushString( pszSrcFile, strlen( pszSrcFile ) );
   zh_vmPushString( pszDstFile, strlen( pszDstFile ) );
   zh_vmDo( 2 );

   return zh_parl( -1 );
}

static ZH_BOOL s_fileDirExists( PZH_FILE_FUNCS pFuncs, const char * pszDirName )
{
   PZH_IOUSR pIO = s_getUsrIO( pFuncs );

   s_pushMethod( pIO, IOUSR_DIREXISTS );
   zh_vmPushString( pszDirName, strlen( pszDirName ) );
   zh_vmDo( 1 );

   return zh_parl( -1 );
}

static ZH_BOOL s_fileDirMake( PZH_FILE_FUNCS pFuncs, const char * pszDirName )
{
   PZH_IOUSR pIO = s_getUsrIO( pFuncs );

   s_pushMethod( pIO, IOUSR_DIRMAKE );
   zh_vmPushString( pszDirName, strlen( pszDirName ) );
   zh_vmDo( 1 );

   return zh_parl( -1 );
}

static ZH_BOOL s_fileDirRemove( PZH_FILE_FUNCS pFuncs, const char * pszDirName )
{
   PZH_IOUSR pIO = s_getUsrIO( pFuncs );

   s_pushMethod( pIO, IOUSR_DIRREMOVE );
   zh_vmPushString( pszDirName, strlen( pszDirName ) );
   zh_vmDo( 1 );

   return zh_parl( -1 );
}

static double s_fileDirSpace( PZH_FILE_FUNCS pFuncs, const char * pszDirName, ZH_USHORT uiType )
{
   PZH_IOUSR pIO = s_getUsrIO( pFuncs );

   s_pushMethod( pIO, IOUSR_DIRSPACE );
   zh_vmPushString( pszDirName, strlen( pszDirName ) );
   zh_vmPushInteger( uiType );
   zh_vmDo( 2 );

   return zh_parnd( -1 );
}

static PZH_ITEM s_fileDirectory( PZH_FILE_FUNCS pFuncs, const char * pszDirSpec, const char * pszAttr )
{
   PZH_IOUSR pIO = s_getUsrIO( pFuncs );

   s_pushMethod( pIO, IOUSR_DIRECTORY );
   zh_vmPushString( pszDirSpec, strlen( pszDirSpec ) );
   zh_vmPushString( pszAttr, strlen( pszAttr ) );
   zh_vmDo( 2 );

   return zh_itemNew( zh_stackReturnItem() );
}

static ZH_BOOL s_fileTimeGet( PZH_FILE_FUNCS pFuncs, const char * pszFileName, long * plJulian, long * plMillisec )
{
   PZH_IOUSR pIO = s_getUsrIO( pFuncs );
   ZH_BOOL fResult;
   int iOffset;

   iOffset = ( int ) ( zh_stackTopOffset() - zh_stackBaseOffset() );
   zh_vmPushNil();
   zh_vmPushNil();

   s_pushMethod( pIO, IOUSR_TIMEGET );
   zh_vmPushString( pszFileName, strlen( pszFileName ) );
   zh_xvmPushLocalByRef( ( ZH_SHORT ) iOffset );
   zh_xvmPushLocalByRef( ( ZH_SHORT ) ( iOffset + 1 ) );
   zh_vmDo( 3 );

   fResult = zh_parl( -1 );
   if( fResult )
   {
      *plJulian = zh_itemGetNL( zh_stackItemFromBase( iOffset ) );
      *plMillisec = zh_itemGetNL( zh_stackItemFromBase( iOffset + 1 ) );
   }
   zh_stackPop();
   zh_stackPop();

   return fResult;
}

static ZH_BOOL s_fileTimeSet( PZH_FILE_FUNCS pFuncs, const char * pszFileName, long lJulian, long lMillisec )
{
   PZH_IOUSR pIO = s_getUsrIO( pFuncs );

   s_pushMethod( pIO, IOUSR_TIMESET );
   zh_vmPushString( pszFileName, strlen( pszFileName ) );
   zh_vmPushLong( lJulian );
   zh_vmPushLong( lMillisec );
   zh_vmDo( 3 );

   return zh_parl( -1 );
}

static ZH_BOOL s_fileAttrGet( PZH_FILE_FUNCS pFuncs, const char * pszFileName, ZH_FATTR * pnAttr )
{
   PZH_IOUSR pIO = s_getUsrIO( pFuncs );
   ZH_BOOL fResult;
   int iOffset;

   iOffset = ( int ) ( zh_stackTopOffset() - zh_stackBaseOffset() );
   zh_vmPushNil();

   s_pushMethod( pIO, IOUSR_ATTRGET );
   zh_vmPushString( pszFileName, strlen( pszFileName ) );
   zh_xvmPushLocalByRef( ( ZH_SHORT ) iOffset );
   zh_vmDo( 2 );

   fResult = zh_parl( -1 );
   if( fResult )
      *pnAttr = ( ZH_FATTR ) zh_itemGetNL( zh_stackItemFromBase( iOffset ) );
   zh_stackPop();

   return fResult;
}

static ZH_BOOL s_fileAttrSet( PZH_FILE_FUNCS pFuncs, const char * pszFileName, ZH_FATTR nAttr )
{
   PZH_IOUSR pIO = s_getUsrIO( pFuncs );

   s_pushMethod( pIO, IOUSR_ATTRSET );
   zh_vmPushString( pszFileName, strlen( pszFileName ) );
   zh_vmPushLong( nAttr );
   zh_vmDo( 2 );

   return zh_parl( -1 );
}

static ZH_BOOL s_fileLink( PZH_FILE_FUNCS pFuncs, const char * pszExisting, const char * pszNewName )
{
   PZH_IOUSR pIO = s_getUsrIO( pFuncs );

   s_pushMethod( pIO, IOUSR_LINK );
   zh_vmPushString( pszExisting, strlen( pszExisting ) );
   zh_vmPushString( pszNewName, strlen( pszNewName ) );
   zh_vmDo( 2 );

   return zh_parl( -1 );
}

static ZH_BOOL s_fileLinkSym( PZH_FILE_FUNCS pFuncs, const char * pszTarget, const char * pszNewName )
{
   PZH_IOUSR pIO = s_getUsrIO( pFuncs );

   s_pushMethod( pIO, IOUSR_LINKSYM );
   zh_vmPushString( pszTarget, strlen( pszTarget ) );
   zh_vmPushString( pszNewName, strlen( pszNewName ) );
   zh_vmDo( 2 );

   return zh_parl( -1 );
}

static char * s_fileLinkRead( PZH_FILE_FUNCS pFuncs, const char * pszFileName )
{
   PZH_IOUSR pIO = s_getUsrIO( pFuncs );
   const char * pszLink;

   s_pushMethod( pIO, IOUSR_LINKREAD );
   zh_vmPushString( pszFileName, strlen( pszFileName ) );
   zh_vmDo( 1 );

   pszLink = zh_parc( -1 );
   return pszLink != NULL ? zh_strdup( pszLink ) : NULL;
}

static PZH_FILE s_fileOpen( PZH_FILE_FUNCS pFuncs, const char * pszName,
                            const char * pszDefExt, ZH_FATTR nExFlags,
                            const char * pPaths, PZH_ITEM pError )
{
   PZH_IOUSR pIO = s_getUsrIO( pFuncs );
   PZH_FILE pFile = NULL;
   PZH_ITEM pFileItm;

   s_pushMethod( pIO, IOUSR_OPEN );
   zh_vmPushString( pszName, strlen( pszName ) );
   if( pszDefExt )
      zh_vmPushString( pszDefExt, strlen( pszDefExt ) );
   else
      zh_vmPushNil();
   zh_vmPushInteger( nExFlags );
   if( pPaths )
      zh_vmPushString( pPaths, strlen( pPaths ) );
   else
      zh_vmPushNil();
   if( pError )
      zh_vmPush( pError );
   else
      zh_vmPushNil();

   zh_vmDo( 5 );

   pFileItm = zh_stackReturnItem();
   if( ! ZH_IS_NIL( pFileItm ) )
      pFile = s_fileNew( pIO, zh_itemNew( pFileItm ) );

   return pFile;
}

static void s_fileClose( PZH_FILE pFile )
{
   PZH_IOUSR pIO = s_getUsrIO( pFile->pFuncs );

   s_pushMethod( pIO, IOUSR_CLOSE );
   zh_vmPush( pFile->pFileItm );
   zh_vmDo( 1 );
}

static ZH_BOOL s_fileLock( PZH_FILE pFile, ZH_FOFFSET nStart,
                           ZH_FOFFSET nLen, int iType )
{
   PZH_IOUSR pIO = s_getUsrIO( pFile->pFuncs );

   s_pushMethod( pIO, IOUSR_LOCK );
   zh_vmPush( pFile->pFileItm );
   zh_vmPushNumInt( ( ZH_MAXINT ) nStart );
   zh_vmPushNumInt( ( ZH_MAXINT ) nLen );
   zh_vmPushInteger( iType );
   zh_vmDo( 4 );

   return zh_parl( -1 );
}

static int s_fileLockTest( PZH_FILE pFile, ZH_FOFFSET nStart,
                           ZH_FOFFSET nLen, int iType )
{
   PZH_IOUSR pIO = s_getUsrIO( pFile->pFuncs );

   s_pushMethod( pIO, IOUSR_LOCKTEST );
   zh_vmPush( pFile->pFileItm );
   zh_vmPushNumInt( ( ZH_MAXINT ) nStart );
   zh_vmPushNumInt( ( ZH_MAXINT ) nLen );
   zh_vmPushInteger( iType );
   zh_vmDo( 4 );

   return zh_parni( -1 );
}

static ZH_SIZE s_fileRead( PZH_FILE pFile, void * data,
                           ZH_SIZE nSize, ZH_MAXINT timeout )
{
   PZH_IOUSR pIO = s_getUsrIO( pFile->pFuncs );
   ZH_SIZE nResult;
   int iOffset;

   iOffset = ( int ) ( zh_stackTopOffset() - zh_stackBaseOffset() );
   memset( data, 0, nSize );
   zh_vmPushString( ( const char * ) data, nSize );

   s_pushMethod( pIO, IOUSR_READ );
   zh_vmPush( pFile->pFileItm );
   zh_xvmPushLocalByRef( ( ZH_SHORT ) iOffset );
   zh_vmPushSize( nSize );
   zh_vmPushNumInt( timeout );
   zh_vmDo( 4 );

   nResult = zh_parns( -1 );
   if( nResult > 0 )
   {
      nSize = zh_itemGetCLen( zh_stackItemFromBase( iOffset ) );
      if( nResult > nSize )
         nResult = nSize;
      memcpy( data, zh_itemGetCPtr( zh_stackItemFromBase( iOffset ) ), nSize );
   }
   zh_stackPop();

   return nResult;
}

static ZH_SIZE s_fileWrite( PZH_FILE pFile, const void * data,
                            ZH_SIZE nSize, ZH_MAXINT timeout )
{
   PZH_IOUSR pIO = s_getUsrIO( pFile->pFuncs );

   s_pushMethod( pIO, IOUSR_WRITE );
   zh_vmPush( pFile->pFileItm );
   zh_vmPushString( ( const char * ) data, nSize );
   zh_vmPushSize( nSize );
   zh_vmPushNumInt( timeout );
   zh_vmDo( 4 );

   return zh_parns( -1 );
}

static ZH_SIZE s_fileReadAt( PZH_FILE pFile, void * buffer,
                             ZH_SIZE nSize, ZH_FOFFSET nOffset )
{
   PZH_IOUSR pIO = s_getUsrIO( pFile->pFuncs );
   ZH_SIZE nResult;
   int iOffset;

   iOffset = ( int ) ( zh_stackTopOffset() - zh_stackBaseOffset() );
   memset( buffer, 0, nSize );
   zh_vmPushString( ( const char * ) buffer, nSize );

   s_pushMethod( pIO, IOUSR_READAT );
   zh_vmPush( pFile->pFileItm );
   zh_xvmPushLocalByRef( ( ZH_SHORT ) iOffset );
   zh_vmPushSize( nSize );
   zh_vmPushNumInt( ( ZH_MAXINT ) nOffset );
   zh_vmDo( 4 );

   nResult = zh_parns( -1 );
   if( nResult > 0 )
   {
      nSize = zh_itemGetCLen( zh_stackItemFromBase( iOffset ) );
      if( nResult > nSize )
         nResult = nSize;
      memcpy( buffer, zh_itemGetCPtr( zh_stackItemFromBase( iOffset ) ), nSize );
   }
   zh_stackPop();

   return nResult;
}

static ZH_SIZE s_fileWriteAt( PZH_FILE pFile, const void * buffer,
                              ZH_SIZE nSize, ZH_FOFFSET nOffset )
{
   PZH_IOUSR pIO = s_getUsrIO( pFile->pFuncs );

   s_pushMethod( pIO, IOUSR_WRITEAT );
   zh_vmPush( pFile->pFileItm );
   zh_vmPushString( ( const char * ) buffer, nSize );
   zh_vmPushSize( nSize );
   zh_vmPushNumInt( ( ZH_MAXINT ) nOffset );
   zh_vmDo( 4 );

   return zh_parns( -1 );
}

static ZH_BOOL s_fileTruncAt( PZH_FILE pFile, ZH_FOFFSET nOffset )
{
   PZH_IOUSR pIO = s_getUsrIO( pFile->pFuncs );

   s_pushMethod( pIO, IOUSR_TRUNCAT );
   zh_vmPush( pFile->pFileItm );
   zh_vmPushNumInt( ( ZH_MAXINT ) nOffset );
   zh_vmDo( 2 );

   return zh_parl( -1 );
}

static ZH_FOFFSET s_fileSeek( PZH_FILE pFile, ZH_FOFFSET nOffset,
                              ZH_USHORT uiFlags )
{
   PZH_IOUSR pIO = s_getUsrIO( pFile->pFuncs );

   s_pushMethod( pIO, IOUSR_SEEK );
   zh_vmPush( pFile->pFileItm );
   zh_vmPushNumInt( ( ZH_MAXINT ) nOffset );
   zh_vmPushInteger( uiFlags );
   zh_vmDo( 3 );

   return ( ZH_FOFFSET ) zh_parnint( -1 );
}

static ZH_FOFFSET s_fileSize( PZH_FILE pFile )
{
   PZH_IOUSR pIO = s_getUsrIO( pFile->pFuncs );

   s_pushMethod( pIO, IOUSR_SIZE );
   zh_vmPush( pFile->pFileItm );
   zh_vmDo( 1 );

   return ( ZH_FOFFSET ) zh_parnint( -1 );
}

static ZH_BOOL s_fileEof( PZH_FILE pFile )
{
   PZH_IOUSR pIO = s_getUsrIO( pFile->pFuncs );

   s_pushMethod( pIO, IOUSR_EOF );
   zh_vmPush( pFile->pFileItm );
   zh_vmDo( 1 );

   return zh_parl( -1 );
}

static void s_fileFlush( PZH_FILE pFile, ZH_BOOL fDirty )
{
   PZH_IOUSR pIO = s_getUsrIO( pFile->pFuncs );

   s_pushMethod( pIO, IOUSR_FLUSH );
   zh_vmPush( pFile->pFileItm );
   zh_vmPushLogical( fDirty );
   zh_vmDo( 2 );
}

static void s_fileCommit( PZH_FILE pFile )
{
   PZH_IOUSR pIO = s_getUsrIO( pFile->pFuncs );

   s_pushMethod( pIO, IOUSR_COMMIT );
   zh_vmPush( pFile->pFileItm );
   zh_vmDo( 1 );
}

static ZH_BOOL s_fileConfigure( PZH_FILE pFile, int iIndex, PZH_ITEM pValue )
{
   PZH_IOUSR pIO = s_getUsrIO( pFile->pFuncs );

   s_pushMethod( pIO, IOUSR_CONFIGURE );
   zh_vmPush( pFile->pFileItm );
   zh_vmPushInteger( iIndex );
   if( pValue != NULL )
      zh_vmPush( pValue );
   zh_vmDo( pValue != NULL ? 3 : 2 );

   return zh_parl( -1 );
}

static ZH_FHANDLE s_fileHandle( PZH_FILE pFile )
{
   PZH_IOUSR pIO = s_getUsrIO( pFile->pFuncs );

   s_pushMethod( pIO, IOUSR_HANDLE );
   zh_vmPush( pFile->pFileItm );
   zh_vmDo( 1 );

   return ( ZH_FHANDLE ) zh_parns( -1 );
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

/* IOUSR_Register( <aMethods>, <cPrefix> ) */
ZH_FUNC( IOUSR_REGISTER )
{
   PZH_ITEM pMthItm = zh_param( 1, ZH_IT_ARRAY );
   const char * pszPrefix = zh_parc( 2 );

   if( pMthItm && pszPrefix && *pszPrefix )
   {
      ZH_SIZE nMethods = zh_arrayLen( pMthItm ), nAt;

      if( nMethods > ZH_MIN( IOUSR_METHODCOUNT, ZH_FILE_FUNC_COUNT ) )
         nMethods = ZH_MIN( IOUSR_METHODCOUNT, ZH_FILE_FUNC_COUNT );

      for( nAt = 1; nAt <= nMethods; ++nAt )
      {
         PZH_ITEM pSymItm = zh_arrayGetItemPtr( pMthItm, nAt );

         if( ! ZH_IS_NIL( pSymItm ) && ! ZH_IS_SYMBOL( pSymItm ) )
            break;
      }

      if( nAt > nMethods )
      {
         PZH_IOUSR pIO = s_iousrAddNew( pszPrefix );

         if( pIO != NULL )
         {
            const ZH_FILE_FUNC * pDummyFunc;
            ZH_FILE_FUNC * pFunction;

            pDummyFunc = &s_fileFuncs.Accept;
            pFunction = &pIO->funcs.Accept;
            for( nAt = 1; nAt <= nMethods; ++nAt, pDummyFunc++, pFunction++ )
            {
               pIO->prg_funcs[ nAt - 1 ] = zh_arrayGetSymbol( pMthItm, nAt );
               if( nAt == 1 || pIO->prg_funcs[ nAt - 1 ] != NULL )
                  * pFunction = * pDummyFunc;
            }
            if( ! zh_fileRegisterPart( &pIO->funcs ) )
               pIO = NULL;
         }
         if( pIO == NULL )
            s_errRT_IOUSR( EG_ARG, 1003, pszPrefix );
      }
      else
         s_errRT_IOUSR( EG_ARG, 1002, pszPrefix );
   }
   else
      s_errRT_IOUSR( EG_ARG, 1001, "Argument error" );
}

/* IOUSR_SetError( [<nError> [, <nBase> ]] ) --> <nPrevError> */
ZH_FUNC( IOUSR_SETERROR )
{
   ZH_ERRCODE errCodePrev = zh_fsError();

   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      ZH_ERRCODE errCodeNew = ( ZH_ERRCODE ) zh_parni( 1 );
      if( errCodeNew != 0 )
         errCodeNew += ( ZH_ERRCODE ) zh_parni( 2 );
      zh_fsSetError( errCodeNew );
   }

   zh_retni( errCodePrev );
}
