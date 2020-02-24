/*
 * ZH level interface to Ziher FILE IO API
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

#include "zh_api.h"
#include "zh_fs_api.h"
#include "zh_error_api.h"
#include "zh_item_api.h"
#include "zh_date.h"

/* extended FILE IO handle destructor */
static ZH_GARBAGE_FUNC( zh_file_Destructor )
{
   PZH_FILE * fileHolder = ( PZH_FILE * ) Cargo;

   if( * fileHolder )
   {
      PZH_FILE pFile = * fileHolder;
      * fileHolder = NULL;
      zh_fileClose( pFile );
   }
}

static const ZH_GC_FUNCS s_gcFileFuncs =
{
   zh_file_Destructor,
   zh_gcDummyMark
};

PZH_FILE zh_fileParam( int iParam )
{
   PZH_FILE * fileHolder = ( PZH_FILE * ) zh_parptrGC( &s_gcFileFuncs, iParam );

   if( fileHolder && * fileHolder )
      return * fileHolder;

   zh_errRT_BASE_SubstR( EG_ARG, 2021, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
   return NULL;
}

PZH_FILE zh_fileParamGet( int iParam )
{
   PZH_FILE * fileHolder = ( PZH_FILE * ) zh_parptrGC( &s_gcFileFuncs, iParam );

   return fileHolder ? *fileHolder : NULL;
}

PZH_FILE zh_fileItemGet( PZH_ITEM pItem )
{
   PZH_FILE * fileHolder = ( PZH_FILE * ) zh_itemGetPtrGC( pItem, &s_gcFileFuncs );

   return fileHolder ? *fileHolder : NULL;
}

PZH_ITEM zh_fileItemPut( PZH_ITEM pItem, PZH_FILE pFile )
{
   PZH_FILE * fileHolder = ( PZH_FILE * ) zh_gcAllocate( sizeof( PZH_FILE ),
                                                         &s_gcFileFuncs );
   * fileHolder = pFile;
   return zh_itemPutPtrGC( pItem, fileHolder );
}

void zh_fileItemClear( PZH_ITEM pItem )
{
   PZH_FILE * fileHolder = ( PZH_FILE * ) zh_itemGetPtrGC( pItem, &s_gcFileFuncs );

   if( fileHolder )
      * fileHolder = NULL;
}

static PZH_FILE * zh_fileParamPtr( int iParam )
{
   PZH_FILE * fileHolder = ( PZH_FILE * ) zh_parptrGC( &s_gcFileFuncs, iParam );

   if( fileHolder && * fileHolder )
      return fileHolder;

   zh_errRT_BASE_SubstR( EG_ARG, 2021, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
   return NULL;
}

static void zh_fileReturn( PZH_FILE pFile )
{
   if( pFile )
      zh_fileItemPut( zh_param( -1, ZH_IT_ANY ), pFile );
   else
      zh_ret();
}

/* zh_vfExists( <cFileName>, [ @<cDestFileName> ] ) --> <lOK> */
ZH_FUNC( ZH_VFEXISTS )
{
   const char * pszFileName = zh_parc( 1 );
   ZH_BOOL fResult = ZH_FALSE;
   ZH_ERRCODE uiError = 2;

   if( pszFileName )
   {
      if( ZH_ISBYREF( 2 ) )
      {
         char szName[ ZH_PATH_MAX ];

         szName[ 0 ] = '\0';
         fResult = zh_fileExists( pszFileName, szName );
         zh_storc( szName, 2 );
      }
      else
         fResult = zh_fileExists( pszFileName, NULL );

      uiError = zh_fsError();
   }

   zh_fsSetFError( uiError );
   zh_retl( fResult );
}

/* zh_vfErase( <cFileName> ) --> <nResult> */
ZH_FUNC( ZH_VFERASE )
{
   const char * pszFile = zh_parc( 1 );
   ZH_ERRCODE uiError = 3;
   int iResult = F_ERROR;

   if( pszFile )
   {
      if( zh_fileDelete( pszFile ) )
         iResult = 0;
      uiError = zh_fsError();
   }

   zh_fsSetFError( uiError );
   zh_retni( iResult );
}

/* zh_vfRename( <cFileSrc>, <cFileDst> ) --> <nResult> */
ZH_FUNC( ZH_VFRENAME )
{
   const char * szFileOld = zh_parc( 1 ),
              * szFileNew = zh_parc( 2 );
   ZH_ERRCODE uiError = 2;
   int iResult = F_ERROR;

   if( szFileOld && szFileNew )
   {
      if( zh_fileRename( szFileOld, szFileNew ) )
         iResult = 0;
      uiError = zh_fsError();
   }

   zh_fsSetFError( uiError );
   zh_retni( iResult );
}

/* zh_vfCopyFile( <cFileSrc>, <cFileDst> ) --> <nResult> */
ZH_FUNC( ZH_VFCOPYFILE )
{
   const char * pszSource = zh_parc( 1 ),
              * pszDestin = zh_parc( 2 );
   ZH_ERRCODE uiError = 2;
   int iResult = F_ERROR;

   if( pszSource && pszDestin )
   {
      if( zh_fileCopy( pszSource, pszDestin ) )
         iResult = 0;
      uiError = zh_fsError();
   }

   zh_fsSetFError( uiError );
   zh_retni( iResult );
}

/* zh_vfMoveFile( <cFileSrc>, <cFileDst> ) --> <nResult> */
ZH_FUNC( ZH_VFMOVEFILE )
{
   const char * pszSource = zh_parc( 1 ),
              * pszDestin = zh_parc( 2 );
   ZH_ERRCODE uiError = 2;
   int iResult = F_ERROR;

   if( pszSource && pszDestin )
   {
      if( zh_fileMove( pszSource, pszDestin ) )
         iResult = 0;
      uiError = zh_fsError();
   }

   zh_fsSetFError( uiError );
   zh_retni( iResult );
}

/* zh_vfDirExists( <cDirName> ) --> <lExists> */
ZH_FUNC( ZH_VFDIREXISTS )
{
   const char * pszDirName = zh_parc( 1 );
   ZH_BOOL fResult = ZH_FALSE;
   ZH_ERRCODE uiError = 2;

   if( pszDirName )
   {
      fResult = zh_fileDirExists( pszDirName );
      uiError = zh_fsError();
   }

   zh_fsSetFError( uiError );
   zh_retl( fResult );
}

/* zh_vfDirMake( <cDirName> ) --> <nSuccess> */
ZH_FUNC( ZH_VFDIRMAKE )
{
   const char * pszDirName = zh_parc( 1 );
   ZH_ERRCODE uiError = 2;
   int iResult = F_ERROR;

   if( pszDirName )
   {
      if( zh_fileDirMake( pszDirName ) )
         iResult = 0;
      uiError = zh_fsError();
   }

   zh_fsSetFError( uiError );
   zh_retni( iResult );
}

/* zh_vfDirRemove( <cDirName> ) --> <nSuccess> */
ZH_FUNC( ZH_VFDIRREMOVE )
{
   const char * pszDirName = zh_parc( 1 );
   ZH_ERRCODE uiError = 2;
   int iResult = F_ERROR;

   if( pszDirName )
   {
      if( zh_fileDirRemove( pszDirName ) )
         iResult = 0;
      uiError = zh_fsError();
   }

   zh_fsSetFError( uiError );
   zh_retni( iResult );
}

/* zh_vfDirectory( [ <cDirSpec> ], [ <cAttr> ] ) --> <aDirectory> */
ZH_FUNC( ZH_VFDIRECTORY )
{
   zh_itemReturnRelease( zh_fileDirectory( zh_parc( 1 ), zh_parc( 2 ) ) );
   zh_fsSetFError( zh_fsError() );
}

/* zh_vfDirSpace( <cDirName>, [ <nInfoType> ] ) --> <nFreeSpace> */
ZH_FUNC( ZH_VFDIRSPACE )
{
   ZH_USHORT uiType = ( ZH_USHORT ) zh_parnidef( 2, ZH_DISK_AVAIL );

   zh_retnlen( zh_fileDirSpace( zh_parc( 1 ), uiType ), -1, 0 );
   zh_fsSetFError( zh_fsError() );
}

/* zh_vfAttrGet( <cFileName>, @<nAttr> ) --> <lOK> */
ZH_FUNC( ZH_VFATTRGET )
{
   ZH_FATTR nAttr = 0;

   zh_retl( zh_fileAttrGet( zh_parcx( 1 ), &nAttr ) );
   zh_fsSetFError( zh_fsError() );

   zh_stornl( nAttr, 2 );
}

/* zh_vfAttrSet( <cFileName>, <nAttr> ) --> <lOK> */
ZH_FUNC( ZH_VFATTRSET )
{
   zh_retl( zh_fileAttrSet( zh_parcx( 1 ), ( ZH_FATTR ) zh_parnl( 2 ) ) );
   zh_fsSetFError( zh_fsError() );
}

/* zh_vfTimeGet( <cFileName>, @<tsDateTime> ) --> <lOK> */
ZH_FUNC( ZH_VFTIMEGET )
{
   long lJulian, lMillisec;
   ZH_BOOL fOK;

   fOK = zh_fileTimeGet( zh_parcx( 1 ), &lJulian, &lMillisec );
   zh_fsSetFError( zh_fsError() );

   if( fOK )
   {
      if( ZH_ISBYREF( 3 ) )
      {
         char buf[ 13 ];
         zh_timeStr( buf, lMillisec );
         if( lMillisec % 1000 == 0 )
            buf[ 8 ] = '\0';
         zh_storc( buf, 3 );
         zh_stordl( lJulian, 2 );
      }
      else
         zh_stortdt( lJulian, lMillisec, 2 );

      zh_retl( ZH_TRUE );
   }
   else
   {
      if( ZH_ISBYREF( 3 ) )
      {
         zh_storc( NULL, 3 );
         zh_stordl( 0, 2 );
      }
      else
         zh_stortdt( 0, 0, 2 );

      zh_retl( ZH_FALSE );
   }
}

/* zh_vfTimeSet( <cFileName>, <tsDateTime> ) --> <lOK> */
ZH_FUNC( ZH_VFTIMESET )
{
   long lDate = -1, lTime = -1;

   if( ZH_ISTIMESTAMP( 2 ) )
      zh_partdt( &lDate, &lTime, 2 );
   else
   {
      if( ZH_ISDATE( 2 ) )
         lDate = zh_pardl( 2 );
      if( ZH_ISCHAR( 3 ) )
      {
         int iHour, iMinutes, iSeconds, iMSec;
         if( zh_timeStrGet( zh_parc( 3 ), &iHour, &iMinutes, &iSeconds, &iMSec ) )
            lTime = zh_timeEncode( iHour, iMinutes, iSeconds, iMSec );
      }
   }

   zh_retl( zh_fileTimeSet( zh_parcx( 1 ), lDate, lTime ) );
   zh_fsSetFError( zh_fsError() );
}

/* zh_vfLink( <cExistingFileName>, <cNewFileName> ) --> <nSuccess> */
ZH_FUNC( ZH_VFLINK )
{
   const char * pszExisting = zh_parc( 1 ), * pszNewFile = zh_parc( 2 );
   ZH_ERRCODE uiError = 2;
   int iResult = F_ERROR;

   if( pszExisting && pszNewFile )
   {
      if( zh_fileLink( pszExisting, pszNewFile ) )
         iResult = 0;
      uiError = zh_fsError();
   }

   zh_fsSetFError( uiError );
   zh_retni( iResult );
}

/* zh_vfLinkSym( <cTargetFileName>, <cNewFileName> ) --> <nSuccess> */
ZH_FUNC( ZH_VFLINKSYM )
{
   const char * pszTarget = zh_parc( 1 ), * pszNewFile = zh_parc( 2 );
   ZH_ERRCODE uiError = 2;
   int iResult = F_ERROR;

   if( pszTarget && pszNewFile )
   {
      if( zh_fileLinkSym( pszTarget, pszNewFile ) )
         iResult = 0;
      uiError = zh_fsError();
   }

   zh_fsSetFError( uiError );
   zh_retni( iResult );
}

/* zh_vfLinkRead( <cFileName> ) --> <cDestFileName> | "" */
ZH_FUNC( ZH_VFLINKREAD )
{
   const char * pszFile = zh_parc( 1 );
   char * pszResult = NULL;
   ZH_ERRCODE uiError = 2;

   if( pszFile )
   {
      pszResult = zh_fileLinkRead( pszFile );
      uiError = zh_fsError();
   }

   zh_fsSetFError( uiError );
   zh_retc_buffer( pszResult );
}

/* zh_vfOpen( [@]<cFileName>, [ <nModeAttr> ] ) --> <pHandle> | NIL */
ZH_FUNC( ZH_VFOPEN )
{
   const char * pszFile = zh_parc( 1 );

   if( pszFile )
   {
      char szName[ ZH_PATH_MAX ];
      ZH_FATTR nModeAttr = 0;
      PZH_FILE pFile;
      int iMode;

      iMode = zh_parnidef( 2, FO_READWRITE | FO_DENYNONE | FO_PRIVATE ) &
              ( 0xFF | FO_CREAT | FO_TRUNC | FO_EXCL | FO_DEFAULTS );

      if( iMode & FO_CREAT )
      {
         if( iMode & FO_TRUNC )
            nModeAttr |= FXO_TRUNCATE;
         else
            nModeAttr |= FXO_APPEND;
         if( iMode & FO_EXCL )
            nModeAttr |= FXO_UNIQUE;
      }

      if( iMode & FO_DEFAULTS )
         nModeAttr |= FXO_DEFAULTS;

      if( iMode & ( FO_EXCLUSIVE | FO_DENYWRITE | FO_DENYREAD | FO_DENYNONE ) )
         nModeAttr |= FXO_SHARELOCK;

      nModeAttr |= ( ZH_FATTR ) ( iMode & 0xFF );

      if( ZH_ISBYREF( 1 ) )
      {
         zh_strncpy( szName, pszFile, sizeof( szName ) - 1 );
         nModeAttr |= FXO_COPYNAME;
         pszFile = szName;
      }
      else
         nModeAttr &= ( ZH_FATTR ) ~FXO_COPYNAME;

      pFile = zh_fileExtOpen( pszFile, NULL /* pDefExt */, nModeAttr,
                              NULL /* pPaths */, NULL /* pError */ );

      if( pszFile == szName )
         zh_storc( szName, 1 );

      zh_fsSetFError( zh_fsError() );
      zh_fileReturn( pFile );
   }
   else
   {
      zh_fsSetFError( 0 );
      zh_errRT_BASE( EG_ARG, 2021, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
   }
}

/* zh_vfClose( <pHandle> ) --> <lOK> */
ZH_FUNC( ZH_VFCLOSE )
{
   PZH_FILE * pFilePtr = zh_fileParamPtr( 1 );

   if( pFilePtr )
   {
      PZH_FILE pFile = * pFilePtr;
      * pFilePtr = NULL;
      zh_fileClose( pFile );
      zh_fsSetFError( zh_fsError() );
      zh_retl( ZH_TRUE );
   }
}

/* zh_vfLock( <pHandle>, <nStart>, <nLen>, [ <nType> ] ) --> <lOK> */
ZH_FUNC( ZH_VFLOCK )
{
   PZH_FILE pFile = zh_fileParam( 1 );

   if( pFile )
   {
      ZH_ERRCODE uiError = 0;
      ZH_BOOL fResult = ZH_FALSE;

      if( ZH_IS_PARAM_NUM( 2 ) && ZH_IS_PARAM_NUM( 3 ) )
      {
         fResult = zh_fileLock( pFile,
                                ( ZH_FOFFSET ) zh_parnint( 2 ),
                                ( ZH_FOFFSET ) zh_parnint( 3 ),
                                FL_LOCK | ( zh_parni( 4 ) & ~FL_MASK ) );
         uiError = zh_fsError();
      }
      zh_fsSetFError( uiError );
      zh_retl( fResult );
   }
}

/* zh_vfUnlock( <pHandle>, <nStart>, <nLen> ) --> <lOK> */
ZH_FUNC( ZH_VFUNLOCK )
{
   PZH_FILE pFile = zh_fileParam( 1 );

   if( pFile )
   {
      ZH_ERRCODE uiError = 0;
      ZH_BOOL fResult = ZH_FALSE;

      if( ZH_IS_PARAM_NUM( 2 ) && ZH_IS_PARAM_NUM( 3 ) )
      {
         fResult = zh_fileLock( pFile,
                                ( ZH_FOFFSET ) zh_parnint( 2 ),
                                ( ZH_FOFFSET ) zh_parnint( 3 ),
                                FL_UNLOCK );
         uiError = zh_fsError();
      }
      zh_fsSetFError( uiError );
      zh_retl( fResult );
   }
}

/* zh_vfLockTest( <pHandle>, <nStart>, <nLen>, [ <nType> ] ) --> <nPID> | 0 (nolock) | -1 (err) */
ZH_FUNC( ZH_VFLOCKTEST )
{
   PZH_FILE pFile = zh_fileParam( 1 );

   if( pFile )
   {
      ZH_ERRCODE uiError = 0;
      int iResult = -1;

      if( ZH_IS_PARAM_NUM( 2 ) && ZH_IS_PARAM_NUM( 3 ) )
      {
         iResult = zh_fileLockTest( pFile,
                                    ( ZH_FOFFSET ) zh_parnint( 2 ),
                                    ( ZH_FOFFSET ) zh_parnint( 3 ),
                                    FL_LOCK | ( zh_parni( 4 ) & ~FL_MASK ) );
         uiError = zh_fsError();
      }
      zh_fsSetFError( uiError );
      zh_retni( iResult );
   }
}

/* zh_vfRead( <pHandle>, @<cBuff>, [ <nToRead> ], [ <nTimeOut> ] ) --> <nRead> */
ZH_FUNC( ZH_VFREAD )
{
   PZH_FILE pFile = zh_fileParam( 1 );

   if( pFile )
   {
      PZH_ITEM pBuffer = zh_param( 2, ZH_IT_STRING );
      ZH_ERRCODE uiError = 0;
      ZH_SIZE nRead = 0;
      ZH_SIZE nSize;
      char * buffer;

      if( pBuffer && ZH_ISBYREF( 2 ) &&
          zh_itemGetWriteCL( pBuffer, &buffer, &nSize ) )
      {
         if( ZH_IS_PARAM_NUM( 3 ) )
         {
            nRead = zh_parns( 3 );
            if( nRead < nSize )
               nSize = nRead;
         }
         nRead = zh_fileRead( pFile, buffer, nSize, zh_parnintdef( 4, -1 ) );
         uiError = zh_fsError();
      }

      if( nRead == ( ZH_SIZE ) FS_ERROR )
         zh_retni( FS_ERROR );
      else
         zh_retns( nRead );
      zh_fsSetFError( uiError );
   }
}

/* zh_vfReadLen( <pHandle>, <nToRead>, [ <nTimeOut> ] ) --> <cBuffer> */
ZH_FUNC( ZH_VFREADLEN )
{
   PZH_FILE pFile = zh_fileParam( 1 );

   if( pFile && ZH_IS_PARAM_NUM( 2 ) )
   {
      ZH_ERRCODE uiError = 0;
      ZH_SIZE nToRead = zh_parns( 2 );

      if( nToRead > 0 )
      {
         char * buffer = ( char * ) zh_xgrab( nToRead + 1 );
         ZH_SIZE nRead;

         nRead = zh_fileRead( pFile, buffer, nToRead, zh_parnintdef( 3, -1 ) );
         uiError = zh_fsError();

         if( nRead == ( ZH_SIZE ) FS_ERROR )
            nRead = 0;
         zh_retclen_buffer( buffer, nRead );
      }
      else
         zh_retc_null();

      zh_fsSetFError( uiError );
   }
}

/* zh_vfWrite( <pHandle>, <cBuff>, [ <nToWrite> ], [ <nTimeOut> ] ) --> <nWritten> */
ZH_FUNC( ZH_VFWRITE )
{
   PZH_FILE pFile = zh_fileParam( 1 );

   if( pFile )
   {
      ZH_ERRCODE uiError = 0;

      if( ZH_ISCHAR( 2 ) )
      {
         ZH_SIZE nLen = zh_parclen( 2 );

         if( ZH_IS_PARAM_NUM( 3 ) )
         {
            ZH_SIZE nWrite = zh_parns( 3 );
            if( nWrite < nLen )
               nLen = nWrite;
         }

         nLen = zh_fileWrite( pFile, zh_parc( 2 ), nLen,
                              zh_parnintdef( 4, -1 ) );
         if( nLen == ( ZH_SIZE ) FS_ERROR )
            zh_retni( FS_ERROR );
         else
            zh_retns( nLen );
         uiError = zh_fsError();
      }
      else
         zh_retni( 0 );

      zh_fsSetFError( uiError );
   }
}

/* zh_vfReadAt( <pHandle>, @<cBuff>, [ <nToRead> ], [ <nAtOffset> ] ) --> <nRead> */
ZH_FUNC( ZH_VFREADAT )
{
   PZH_FILE pFile = zh_fileParam( 1 );

   if( pFile )
   {
      PZH_ITEM pBuffer = zh_param( 2, ZH_IT_STRING );
      ZH_ERRCODE uiError = 0;
      ZH_SIZE nRead = 0;
      ZH_SIZE nSize;
      char * buffer;

      if( pBuffer && ZH_ISBYREF( 2 ) &&
          zh_itemGetWriteCL( pBuffer, &buffer, &nSize ) )
      {
         if( ZH_IS_PARAM_NUM( 3 ) )
         {
            nRead = zh_parns( 3 );
            if( nRead < nSize )
               nSize = nRead;
         }
         nRead = zh_fileReadAt( pFile, buffer, nSize,
                                ( ZH_FOFFSET ) zh_parnintdef( 4, -1 ) );
         uiError = zh_fsError();
      }

      if( nRead == ( ZH_SIZE ) FS_ERROR )
         zh_retni( FS_ERROR );
      else
         zh_retns( nRead );
      zh_fsSetFError( uiError );
   }
}

/* zh_vfWriteAt( <pHandle>, <cBuff>, [ <nToWrite> ], [ <nAtOffset> ] ) --> <nWritten> */
ZH_FUNC( ZH_VFWRITEAT )
{
   PZH_FILE pFile = zh_fileParam( 1 );

   if( pFile )
   {
      ZH_ERRCODE uiError = 0;
      const char * pszData = zh_parc( 2 );

      if( pszData )
      {
         ZH_SIZE nLen = zh_parclen( 2 );

         if( ZH_IS_PARAM_NUM( 3 ) )
         {
            ZH_SIZE nWrite = zh_parns( 3 );
            if( nWrite < nLen )
               nLen = nWrite;
         }

         nLen = zh_fileWriteAt( pFile, pszData, nLen,
                                ( ZH_FOFFSET ) zh_parnintdef( 4, -1 ) );
         if( nLen == ( ZH_SIZE ) FS_ERROR )
            zh_retni( FS_ERROR );
         else
            zh_retns( nLen );
         uiError = zh_fsError();
      }
      else
         zh_retni( 0 );

      zh_fsSetFError( uiError );
   }
}

/* zh_vfSeek( <pHandle>, <nOffset>, [ <nWhence> ] ) --> <nOffset> */
ZH_FUNC( ZH_VFSEEK )
{
   PZH_FILE pFile = zh_fileParam( 1 );

   if( pFile )
   {
      ZH_ERRCODE uiError = 0;

      if( ZH_IS_PARAM_NUM( 2 ) )
      {
         zh_retnint( zh_fileSeek( pFile, ( ZH_FOFFSET ) zh_parnint( 2 ),
                                  ( ZH_USHORT ) zh_parnidef( 3, FS_SET ) ) );
         uiError = zh_fsError();
      }
      else
         zh_retni( 0 );

      zh_fsSetFError( uiError );
   }
}

/* zh_vfTrunc( <pHandle>, [ <nAtOffset> ] ) --> <lOK> */
ZH_FUNC( ZH_VFTRUNC )
{
   PZH_FILE pFile = zh_fileParam( 1 );

   if( pFile )
   {
      zh_retl( zh_fileTruncAt( pFile, ( ZH_FOFFSET ) zh_parnint( 2 ) ) );
      zh_fsSetFError( zh_fsError() );
   }
}

/* zh_vfSize( <pHandle> | <cFileName> [, <lUseDirEntry> ] ) --> <nSize> */
ZH_FUNC( ZH_VFSIZE )
{
   const char * pszFile = zh_parc( 1 );

   if( pszFile )
   {
      zh_retnint( zh_fileSizeGet( pszFile, zh_parldef( 2, ZH_TRUE ) ) );
      zh_fsSetFError( zh_fsError() );
   }
   else
   {
      PZH_FILE pFile = zh_fileParam( 1 );
      if( pFile )
      {
         zh_retnint( zh_fileSize( pFile ) );
         zh_fsSetFError( zh_fsError() );
      }
   }
}

/* zh_vfEof( <pHandle> ) --> <lEOF> */
ZH_FUNC( ZH_VFEOF )
{
   PZH_FILE pFile = zh_fileParam( 1 );

   if( pFile )
   {
      zh_retl( zh_fileEof( pFile ) );
      zh_fsSetFError( zh_fsError() );
   }
}

/* zh_vfFlush( <pHandle>, [ <lDirtyOnly> ] ) --> NIL */
ZH_FUNC( ZH_VFFLUSH )
{
   PZH_FILE pFile = zh_fileParam( 1 );

   if( pFile )
   {
      zh_fileFlush( pFile, zh_parl( 2 ) );
      zh_fsSetFError( zh_fsError() );
   }
}

/* zh_vfCommit( <pHandle> ) --> NIL */
ZH_FUNC( ZH_VFCOMMIT )
{
   PZH_FILE pFile = zh_fileParam( 1 );

   if( pFile )
   {
      zh_fileCommit( pFile );
      zh_fsSetFError( zh_fsError() );
   }
}

/* zh_vfConfig( <pHandle>, <nSet>, [ <nParam> ] ) --> <nResult> */
ZH_FUNC( ZH_VFCONFIG )
{
   PZH_FILE pFile = zh_fileParam( 1 );

   if( pFile )
   {
      if( ZH_IS_PARAM_NUM( 2 ) )
      {
         PZH_ITEM pValue = zh_itemNew( zh_param( 3, ZH_IT_ANY ) );

         zh_fileConfigure( pFile, zh_parni( 2 ), pValue );
         zh_fsSetFError( zh_fsError() );
         zh_itemReturnRelease( pValue );
      }
      else
         zh_errRT_BASE_SubstR( EG_ARG, 2021, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
   }
}

/* zh_vfHandle( <pHandle> ) --> <nOsHandle> */
ZH_FUNC( ZH_VFHANDLE )
{
   PZH_FILE pFile = zh_fileParam( 1 );

   zh_retnint( ( ZH_NHANDLE ) ( pFile ? zh_fileHandle( pFile ) : FS_ERROR ) );
}

/* zh_vfTempFile( @<cFileName>, [ <cDir> ], [ <cPrefix> ], [ <cExt> ], [ <nAttr> ] ) --> <pHandle> | NIL */
ZH_FUNC( ZH_VFTEMPFILE )
{
   char szName[ ZH_PATH_MAX ];

   zh_fileReturn( zh_fileCreateTempEx( szName,
                                       zh_parc( 2 ), /* pszDir */
                                       zh_parc( 3 ), /* pszPrefix */
                                       zh_parc( 4 ), /* pszExt */
                                       ( ZH_FATTR ) zh_parnldef( 5, FC_NORMAL ) ) );
   zh_fsSetFError( zh_fsError() );
   zh_storc( szName, 1 );
}

/* zh_vfLoad( <cFileName>, [ <nMaxSize> ] ) --> <cFileBody> | NIL */
ZH_FUNC( ZH_VFLOAD )
{
   const char * pszFileName = zh_parc( 1 );

   if( pszFileName )
   {
      ZH_SIZE nSize;
      char * pBuffer = ( char * ) zh_fileLoad( pszFileName, zh_parns( 2 ), &nSize );
      if( pBuffer )
         zh_retclen_buffer( pBuffer, nSize );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 2021, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}
