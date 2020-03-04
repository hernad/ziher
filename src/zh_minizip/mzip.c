/*
 * Wrapper functions for minizip library
 * Some higher-level ZIP archive functions
 *
 * Copyright 2008 Mindaugas Kavaliauskas <dbtopas.at.dbtopas.lt>
 * Copyright 2011-2017 Viktor Szakats (vszakats.net/ziher)
 *   (codepage, unicode, *Mem())
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
#include "zh_item_api.h"
#include "zh_error_api.h"
#include "zh_string_api.h"
#include "crypto/zh_chksum.h"
#include "zh_date.h"
#include "zh_set.h"

#if ! defined( ZH_OS_UNIX )
#  undef _LARGEFILE64_SOURCE
#endif

#include "zip.h"
#include "unzip.h"

#if defined( ZH_OS_UNIX )


   #include <sys/types.h>
   #include <sys/stat.h>
   #include <unistd.h>
   #include <time.h>
   #include <utime.h>
#elif defined( ZH_OS_WIN )
   #include <windows.h>
   #if ! defined( INVALID_FILE_ATTRIBUTES )
      #define INVALID_FILE_ATTRIBUTES  ( ( DWORD ) -1 )
   #endif
   #include "zh_win_unicode.h"
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

#define _ZIP_FLAG_UNICODE  ( 1 << 11 )  /* Language encoding flag (EFS) */

#if defined( ZH_OS_UNIX )
   #define _VER_PLATFORM   0x03  /* it's necessary for file attributes in unzip */
#else
   #define _VER_PLATFORM   0x00
#endif

static int _version_made_by( ZH_BOOL fUnicode )
{
   return ( fUnicode ? 0x3F /* 6.3.x */ : 0x14 /* 2.0.x */ ) | ( _VER_PLATFORM << 8 );
}

#define ZH_Z_IOBUF_SIZE    ( 1024 * 16 )

static ZH_GARBAGE_FUNC( zh_zipfile_destructor )
{
   zipFile * phZip = ( zipFile * ) Cargo;

   if( *phZip )
   {
      zipClose( *phZip, NULL );
      *phZip = NULL;
   }
}

static const ZH_GC_FUNCS s_gcZipFileFuncs =
{
   zh_zipfile_destructor,
   zh_gcDummyMark
};


static zipFile zh_zipfileParam( int iParam )
{
   zipFile * phZip = ( zipFile * ) zh_parptrGC( &s_gcZipFileFuncs, iParam );

   if( phZip && *phZip )
      return *phZip;

   zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
   return NULL;
}


static ZH_GARBAGE_FUNC( zh_unzipfile_destructor )
{
   unzFile * phUnzip = ( unzFile * ) Cargo;

   if( *phUnzip )
   {
      unzClose( *phUnzip );
      *phUnzip = NULL;
   }
}

static const ZH_GC_FUNCS s_gcUnZipFileFuncs =
{
   zh_unzipfile_destructor,
   zh_gcDummyMark
};


static unzFile zh_unzipfileParam( int iParam )
{
   unzFile * phUnzip = ( unzFile * ) zh_parptrGC( &s_gcUnZipFileFuncs, iParam );

   if( phUnzip && *phUnzip )
      return *phUnzip;

   zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
   return NULL;
}

static PZH_FILE zh_fileHandleParam( int iParam, ZH_BOOL * pfFree )
{
   PZH_FILE pFile = NULL;

   * pfFree = ZH_FALSE;
   if( ZH_IS_PARAM_NUM( iParam ) )
   {
      ZH_FHANDLE hFile = zh_numToHandle( zh_parnint( iParam ) );
      if( hFile != FS_ERROR )
      {
         pFile = zh_fileFromHandle( hFile );
         * pfFree = ZH_TRUE;
      }
   }
   else
      pFile = zh_fileParam( iParam );

   if( pFile != NULL )
      return pFile;

   zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
   return NULL;
}

static ZH_FATTR zh_translateExtAttr( const char * szFileName, ZH_FATTR ulExtAttr )
{
   int iLen;

   iLen = ( int ) strlen( szFileName );
   if( ( iLen > 4 && ( zh_stricmp( szFileName + iLen - 4, ".exe" ) == 0 ||
                       zh_stricmp( szFileName + iLen - 4, ".com" ) == 0 ||
                       zh_stricmp( szFileName + iLen - 4, ".bat" ) == 0 ||
                       zh_stricmp( szFileName + iLen - 4, ".cmd" ) == 0 ) ) ||
       ( iLen > 3 && zh_stricmp( szFileName + iLen - 3, ".sh" ) == 0 ) )
   {
      ulExtAttr |= 0x00490000; /* --x--x--x */
   }

   if( ulExtAttr & ZH_FA_READONLY )
      ulExtAttr |= 0x01240000;  /* r--r--r-- */
   else
      ulExtAttr |= 0x01B60000;  /* rw-rw-rw- */

   if( ulExtAttr & ZH_FA_DIRECTORY )
      ulExtAttr |= 0x40000000;
   else
      ulExtAttr |= 0x80000000;

   return ulExtAttr;
}


/* zh_zipOpen( cFileName, [ iMode = ZH_ZIP_CREATE ], [ @cGlobalComment ] ) --> hZip */
ZH_FUNC( ZH_ZIPOPEN )
{
   const char * szFileName = zh_parc( 1 );

   if( szFileName )
   {
      const char * pszGlobalComment = NULL;
      char *       pszFree;
      zipFile      hZip = zipOpen2( zh_fsNameConv( szFileName, &pszFree ), zh_parnidef( 2, APPEND_STATUS_CREATE ),
                                    &pszGlobalComment, NULL );

      if( pszFree )
         zh_xfree( pszFree );

      if( hZip )
      {
         zipFile * phZip = ( zipFile * ) zh_gcAllocate( sizeof( zipFile ), &s_gcZipFileFuncs );

         *phZip = hZip;
         zh_retptrGC( phZip );

         if( pszGlobalComment )
            zh_storc( pszGlobalComment, 3 );
      }
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}


/* zh_zipClose( hZip, [ cGlobalComment ] ) --> nError */
ZH_FUNC( ZH_ZIPCLOSE )
{
   zipFile * phZip = ( zipFile * ) zh_parptrGC( &s_gcZipFileFuncs, 1 );

   if( phZip && *phZip )
   {
      zipFile hZip = *phZip;

      *phZip = NULL;
      zh_retni( zipClose( hZip, zh_parc( 2 ) ) );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}


/* zh_zipFileCreate( hZip, cZipName, dDate, cTime, nInternalAttr, nExternalAttr,
                     [ nMethod = ZH_ZLIB_METHOD_DEFLATE ],
                     [ nLevel = ZH_ZLIB_COMPRESSION_DEFAULT ],
                     [ cPassword, ulFileCRC32 ], [ cComment ], [ lUnicode ] ) --> nError */
ZH_FUNC( ZH_ZIPFILECREATE )
{
   const char * szZipName = zh_parc( 2 );

   if( szZipName )
   {
      zipFile hZip = zh_zipfileParam( 1 );

      if( hZip )
      {
         int   iMethod = zh_parnidef( 7, Z_DEFLATED );
         int   iLevel  = zh_parnidef( 8, Z_DEFAULT_COMPRESSION );
         long  lJulian, lMillisec;
         int   iYear, iMonth, iDay, iHour, iMinute, iSecond, iMSec;
         uLong flags = 0;

         ZH_BOOL      fUnicode = zh_parl( 12 );
         void *       hZipName = NULL;
         void *       hComment = NULL;
         const char * szComment;

         zip_fileinfo zfi;

         memset( &zfi, 0, sizeof( zfi ) );

         if( ZH_ISTIMESTAMP( 3 ) )
         {
            zh_partdt( &lJulian, &lMillisec, 3 );
            zh_dateDecode( lJulian, &iYear, &iMonth, &iDay );
            zh_timeDecode( lMillisec, &iHour, &iMinute, &iSecond, &iMSec );
         }
         else
         {
            zh_dateDecode( zh_pardl( 3 ), &iYear, &iMonth, &iDay );
            zh_timeStrGet( zh_parc( 4 ), &iHour, &iMinute, &iSecond, &iMSec );
         }

         zfi.tmz_date.tm_hour = iHour;
         zfi.tmz_date.tm_min  = iMinute;
         zfi.tmz_date.tm_sec  = iSecond;

         zfi.tmz_date.tm_year = iYear;
         zfi.tmz_date.tm_mon  = iMonth - 1;
         zfi.tmz_date.tm_mday = iDay;

         zfi.internal_fa = zh_parnl( 5 );
         zfi.external_fa = zh_parnl( 6 );
#if ! defined( ZH_OS_UNIX )
         if( ( zfi.external_fa & 0xFFFF0000 ) == 0 )
            zfi.external_fa = zh_translateExtAttr( szZipName, zfi.external_fa );
#endif

         if( fUnicode )
         {
            szZipName = zh_parstr_utf8( 2, &hZipName, NULL );
            szComment = zh_parstr_utf8( 11, &hComment, NULL );
            flags    |= _ZIP_FLAG_UNICODE;
         }
         else
            szComment = zh_parc( 11 );

         zh_retni( zipOpenNewFileInZip4( hZip, szZipName, &zfi,
                                         NULL, 0, NULL, 0,
                                         szComment, iMethod, iLevel, 0,
                                         -MAX_WBITS, DEF_MEM_LEVEL, Z_DEFAULT_STRATEGY,
                                         zh_parc( 9 ), zh_parnl( 10 ), _version_made_by( fUnicode ), flags ) );

         if( fUnicode )
         {
            zh_strfree( hZipName );
            zh_strfree( hComment );
         }
      }
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}


/* zh_zipFileWrite( hZip, cData [, nLen ] ) --> nError */
ZH_FUNC( ZH_ZIPFILEWRITE )
{
   const char * pData = zh_parc( 2 );

   if( pData )
   {
      zipFile hZip = zh_zipfileParam( 1 );
      if( hZip )
      {
         ZH_SIZE nLen = zh_parclen( 2 );

         if( ZH_IS_PARAM_NUM( 3 ) )
         {
            ZH_SIZE nWrite = zh_parns( 3 );
            if( nWrite < nLen )
               nLen = nWrite;
         }

         zh_retni( zipWriteInFileInZip( hZip, pData, ( unsigned ) nLen ) );
      }
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}


/* zh_zipFileClose( hZip ) --> nError */
ZH_FUNC( ZH_ZIPFILECLOSE )
{
   zipFile hZip = zh_zipfileParam( 1 );

   if( hZip )
      zh_retni( zipCloseFileInZip( hZip ) );
}


/* zh_unzipOpen( cFileName ) --> hUnzip */
ZH_FUNC( ZH_UNZIPOPEN )
{
   const char * szFileName = zh_parc( 1 );

   if( szFileName )
   {
      char *  pszFree;
      unzFile hUnzip = unzOpen( zh_fsNameConv( szFileName, &pszFree ) );

      if( pszFree )
         zh_xfree( pszFree );

      if( hUnzip )
      {
         unzFile * phUnzip = ( unzFile * ) zh_gcAllocate( sizeof( unzFile ), &s_gcUnZipFileFuncs );

         *phUnzip = hUnzip;
         zh_retptrGC( phUnzip );
      }
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}


/* zh_unzipClose( hUnzip ) --> nError */
ZH_FUNC( ZH_UNZIPCLOSE )
{
   unzFile * phUnzip = ( unzFile * ) zh_parptrGC( &s_gcUnZipFileFuncs, 1 );

   if( phUnzip && *phUnzip )
   {
      unzFile hUnzip = *phUnzip;

      *phUnzip = NULL;
      zh_retni( unzClose( hUnzip ) );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}


/* zh_unzipGlobalInfo( hUnzip, @nEntries, @cGlobalComment ) --> nError */
ZH_FUNC( ZH_UNZIPGLOBALINFO )
{
   unzFile hUnzip = zh_unzipfileParam( 1 );

   if( hUnzip )
   {
      unz_global_info ugi;
      int iResult;

      iResult = unzGetGlobalInfo( hUnzip, &ugi );

      zh_retni( iResult );

      if( iResult == UNZ_OK )
      {
         zh_storni( ugi.number_entry, 2 );
         if( ZH_ISBYREF( 3 ) )
         {
            if( ugi.size_comment > 0 )
            {
               char * pszComment = ( char * ) zh_xgrab( ugi.size_comment + 1 );

               iResult = unzGetGlobalComment( hUnzip, pszComment, ugi.size_comment );
               if( iResult < 0 )
               {
                  zh_xfree( pszComment );
                  zh_storc( NULL, 3 );
                  zh_retni( iResult );
               }
               else
               {
                  pszComment[ iResult ] = '\0';
                  if( ! zh_storclen_buffer( pszComment, ugi.size_comment, 3 ) )
                     zh_xfree( pszComment );
               }
            }
         }
      }
      else
      {
         zh_storni( 0, 2 );
         zh_storc( NULL, 3 );
      }
   }
}


/* zh_unzipFileFirst( hUnzip ) --> nError */
ZH_FUNC( ZH_UNZIPFILEFIRST )
{
   unzFile hUnzip = zh_unzipfileParam( 1 );

   if( hUnzip )
      zh_retni( unzGoToFirstFile( hUnzip ) );
}


/* zh_unzipFileNext( hUnzip ) --> nError */
ZH_FUNC( ZH_UNZIPFILENEXT )
{
   unzFile hUnzip = zh_unzipfileParam( 1 );

   if( hUnzip )
      zh_retni( unzGoToNextFile( hUnzip ) );
}


/* zh_unzipFilePos( hUnzip ) --> nPosition */
ZH_FUNC( ZH_UNZIPFILEPOS )
{
   unzFile hUnzip = zh_unzipfileParam( 1 );

   if( hUnzip )
      zh_retnint( unzGetOffset( hUnzip ) );
}


/* zh_unzipFileGoto( hUnzip, nPosition ) --> nError */
ZH_FUNC( ZH_UNZIPFILEGOTO )
{
   unzFile hUnzip = zh_unzipfileParam( 1 );

   if( hUnzip )
      zh_retni( unzSetOffset( hUnzip, ( uLong ) zh_parnint( 2 ) ) );
}


/* zh_unzipFileInfo( hUnzip, @cZipName, @dDate, @cTime,
                     @nInternalAttr, @nExternalAttr,
                     @nMethod, @nSize, @nCompressedSize,
                     @lCrypted, @cComment, @nCRC ) --> nError */
ZH_FUNC( ZH_UNZIPFILEINFO )
{
   unzFile hUnzip = zh_unzipfileParam( 1 );

   if( hUnzip )
   {
      char szFileName[ ZH_PATH_MAX * 3 ];
      unz_file_info ufi;
      int  iResult;

      iResult = unzGetCurrentFileInfo( hUnzip, &ufi, szFileName, sizeof( szFileName ) - 1,
                                       NULL, 0, NULL, 0 );
      zh_retni( iResult );

      if( iResult == UNZ_OK )
      {
         ZH_BOOL fUnicode = ( ufi.flag & _ZIP_FLAG_UNICODE ) != 0;

         long lJulian, lMillisec;

         szFileName[ sizeof( szFileName ) - 1 ] = '\0';

         if( fUnicode )
            zh_storstr_utf8( szFileName, 2 );
         else
            zh_storc( szFileName, 2 );

         lJulian = zh_dateEncode( ufi.tmu_date.tm_year, ufi.tmu_date.tm_mon + 1,
                                  ufi.tmu_date.tm_mday );
         lMillisec = zh_timeEncode( ufi.tmu_date.tm_hour, ufi.tmu_date.tm_min,
                                    ufi.tmu_date.tm_sec, 0 );

         zh_stortdt( lJulian, lMillisec, 3 );
         if( ZH_ISBYREF( 4 ) )
         {
            char buf[ 16 ];
            zh_snprintf( buf, sizeof( buf ), "%02d:%02d:%02d",
                         ufi.tmu_date.tm_hour, ufi.tmu_date.tm_min,
                         ufi.tmu_date.tm_sec );
            zh_storc( buf, 4 );
         }
         zh_stornl( ufi.internal_fa, 5 );
         zh_stornl( ufi.external_fa, 6 );
         zh_stornl( ufi.compression_method, 7 );
         zh_storns( ufi.uncompressed_size, 8 );
         zh_storns( ufi.compressed_size, 9 );
         zh_storl( ( ufi.flag & 1 ) != 0, 10 );
         zh_stornint( ufi.crc, 12 );

         if( ufi.size_file_comment > 0 && ZH_ISBYREF( 11 ) )
         {
            char * pszComment = ( char * ) zh_xgrab( ufi.size_file_comment + 1 );

            iResult = unzGetCurrentFileInfo( hUnzip, &ufi, NULL, 0, NULL, 0,
                                             pszComment, ufi.size_file_comment );
            pszComment[ ufi.size_file_comment ] = '\0';
            if( iResult != UNZ_OK )
            {
               zh_xfree( pszComment );
               zh_storc( NULL, 11 );
            }
            else if( fUnicode )
            {
               zh_storstrlen_utf8( pszComment, ufi.size_file_comment, 11 );
               zh_xfree( pszComment );
            }
            else if( ! zh_storclen_buffer( pszComment, ufi.size_file_comment, 11 ) )
               zh_xfree( pszComment );
         }
      }
      else
      {
         zh_storc( NULL, 2 );
         zh_stortdt( 0, 0, 3 );
         zh_storc( NULL, 4 );
         zh_stornl( 0, 5 );
         zh_stornl( 0, 6 );
         zh_stornl( 0, 7 );
         zh_storns( 0, 8 );
         zh_storns( 0, 9 );
         zh_storl( ZH_FALSE, 10 );
         zh_storc( NULL, 11 );
      }
   }
}


/* zh_unzipFileOpen( hUnzip, [ cPassword ] ) --> nError */
ZH_FUNC( ZH_UNZIPFILEOPEN )
{
   unzFile hUnzip = zh_unzipfileParam( 1 );

   if( hUnzip )
      zh_retni( unzOpenCurrentFilePassword( hUnzip, zh_parc( 2 ) ) );
}


/* zh_unzipFileRead( hUnzip, @cBuf [, nLen ] ) --> nRead */
ZH_FUNC( ZH_UNZIPFILEREAD )
{
   PZH_ITEM pBuffer = zh_param( 2, ZH_IT_STRING );
   char *   buffer;
   ZH_SIZE  nSize;

   if( pBuffer && ZH_ISBYREF( 2 ) &&
       zh_itemGetWriteCL( pBuffer, &buffer, &nSize ) )
   {
      unzFile hUnzip = zh_unzipfileParam( 1 );

      if( hUnzip )
      {
         if( ZH_IS_PARAM_NUM( 3 ) )
         {
            ZH_SIZE nRead = zh_parns( 3 );
            if( nRead < nSize )
               nSize = nRead;
         }

         zh_retns( unzReadCurrentFile( hUnzip, buffer, ( unsigned ) nSize ) );
      }
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}


/* zh_unzipFileClose( hUnzip ) --> nError */
ZH_FUNC( ZH_UNZIPFILECLOSE )
{
   unzFile hUnzip = zh_unzipfileParam( 1 );

   if( hUnzip )
      zh_retni( unzCloseCurrentFile( hUnzip ) );
}


/*
 *
 * Higher-level functions - not wrappers of minizip code
 *
 */

static ZH_BOOL zh_zipGetFileInfoFromHandle( PZH_FILE pFile, ZH_U32 * pulCRC, ZH_BOOL * pfText )
{
   ZH_BOOL fText = pfText != NULL, fResult = ZH_FALSE;
   ZH_U32  ulCRC = 0;

   if( pFile != NULL )
   {
      unsigned char * pString = ( unsigned char * ) zh_xgrab( ZH_Z_IOBUF_SIZE );
      ZH_SIZE         nRead;

      do
      {
         nRead = zh_fileResult( zh_fileRead( pFile, pString, ZH_Z_IOBUF_SIZE, -1 ) );
         if( nRead > 0 )
         {
            ulCRC = crc32( ulCRC, pString, ( uInt ) nRead );
            if( fText )
            {
               ZH_SIZE u;
               for( u = 0; u < nRead; ++u )
               {
                  if( pString[ u ] < 0x20 ?
                      ( pString[ u ] != ZH_CHAR_HT &&
                        pString[ u ] != ZH_CHAR_LF &&
                        pString[ u ] != ZH_CHAR_CR &&
                        pString[ u ] != ZH_CHAR_EOF ) :
                      ( pString[ u ] >= 0x7F && pString[ u ] < 0xA0 &&
                        pString[ u ] != ( unsigned char ) ZH_CHAR_SOFT1 ) )
                  {
                     fText = ZH_FALSE;
                     break;
                  }
               }
            }
         }
      }
      while( nRead == ZH_Z_IOBUF_SIZE );

      fResult = ( zh_fsError() == 0 );

      zh_xfree( pString );
   }

   if( pulCRC )
      *pulCRC = ulCRC;
   if( pfText )
      *pfText = fText;

   return fResult;
}

static ZH_BOOL zh_zipGetFileInfoFromMem( const unsigned char * pString, ZH_SIZE nStringLen, ZH_U32 * pulCRC, ZH_BOOL * pfText )
{
   ZH_BOOL fText = pfText != NULL;
   ZH_U32  ulCRC = 0;

   if( pString && nStringLen )
   {
      ulCRC = zh_crc32( ulCRC, pString, nStringLen );
      if( fText )
      {
         ZH_SIZE u;
         for( u = 0; u < nStringLen; ++u )
         {
            if( pString[ u ] < 0x20 ?
                ( pString[ u ] != ZH_CHAR_HT &&
                  pString[ u ] != ZH_CHAR_LF &&
                  pString[ u ] != ZH_CHAR_CR &&
                  pString[ u ] != ZH_CHAR_EOF ) :
                ( pString[ u ] >= 0x7F && pString[ u ] < 0xA0 &&
                  pString[ u ] != ( unsigned char ) ZH_CHAR_SOFT1 ) )
            {
               fText = ZH_FALSE;
               break;
            }
         }
      }
   }

   if( pulCRC )
      *pulCRC = ulCRC;
   if( pfText )
      *pfText = fText;

   return ZH_TRUE;
}

static ZH_BOOL zh_zipGetFileInfo( const char * pszFileName, ZH_U32 * pulCRC, ZH_BOOL * pfText )
{
   PZH_FILE pFile;
   ZH_BOOL  fResult;

   pFile = zh_fileExtOpen( pszFileName, NULL,
                           FO_READ | FO_SHARED | FO_PRIVATE | FXO_SHARELOCK,
                           NULL, NULL );
   fResult = zh_zipGetFileInfoFromHandle( pFile, pulCRC, pfText );
   if( pFile != NULL )
      zh_fileClose( pFile );

   return fResult;
}


/* zh_zipFileCRC32( cFileName ) --> nCRC */
ZH_FUNC( ZH_ZIPFILECRC32 )
{
   const char * szFileName = zh_parc( 1 );

   if( szFileName )
   {
      ZH_U32 ulCRC = 0;
      if( ! zh_zipGetFileInfo( szFileName, &ulCRC, NULL ) )
         ulCRC = 0;
      zh_retnint( ulCRC );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

static int zh_zipStoreFile( zipFile hZip, int iParamFileName, int iParamZipName, const char * szPassword, int iParamComment, ZH_BOOL fUnicode )
{
   const char * szFileName = zh_parc( iParamFileName );
   PZH_FILE     pFile;
   ZH_SIZE      nLen;
   ZH_FATTR     ulExtAttr;
   zip_fileinfo zfi;
   int          iResult;
   ZH_BOOL      fError;
   ZH_BOOL      fText;
   ZH_U32       ulCRC;
   uLong        flags = 0;
   void *       hZipName = NULL;
   void *       hComment = NULL;
   char *       szZipName;
   const char * szComment;

   memset( &zfi, 0, sizeof( zfi ) );
   fError    = ZH_FALSE;
   ulExtAttr = 0;

#if defined( ZH_OS_WIN )
   if( zh_fileIsLocalName( szFileName ) )
   {
      LPTSTR  lpFileNameFree;
      LPCTSTR lpFileName = ZH_FSNAMECONV( szFileName, &lpFileNameFree );
      DWORD   attr       = GetFileAttributes( lpFileName );

      if( attr != INVALID_FILE_ATTRIBUTES )
      {
         ulExtAttr = attr & ( FILE_ATTRIBUTE_READONLY | FILE_ATTRIBUTE_HIDDEN |
                              FILE_ATTRIBUTE_SYSTEM | FILE_ATTRIBUTE_DIRECTORY |
                              FILE_ATTRIBUTE_ARCHIVE );
      }
      else
         fError = ZH_TRUE;

      if( lpFileNameFree )
         zh_xfree( lpFileNameFree );
   }
   else
#elif defined( ZH_OS_OS2 )
   if( zh_fileIsLocalName( szFileName ) )
   {
      FILESTATUS3 fs3;
      APIRET      ulrc;
      char *      pszFree;

      ulrc = DosQueryPathInfo( ( PCSZ ) zh_fsNameConv( szFileName, &pszFree ), FIL_STANDARD, &fs3, sizeof( fs3 ) );

      if( pszFree )
         zh_xfree( pszFree );

      if( ulrc == NO_ERROR )
      {
         if( fs3.attrFile & FILE_READONLY )
            ulExtAttr |= ZH_FA_READONLY;
         if( fs3.attrFile & FILE_HIDDEN )
            ulExtAttr |= ZH_FA_HIDDEN;
         if( fs3.attrFile & FILE_SYSTEM )
            ulExtAttr |= ZH_FA_SYSTEM;
         if( fs3.attrFile & FILE_DIRECTORY )
            ulExtAttr |= ZH_FA_DIRECTORY;
         if( fs3.attrFile & FILE_ARCHIVED )
            ulExtAttr |= ZH_FA_ARCHIVE;

         zfi.tmz_date.tm_sec  = fs3.ftimeLastWrite.twosecs * 2;
         zfi.tmz_date.tm_min  = fs3.ftimeLastWrite.minutes;
         zfi.tmz_date.tm_hour = fs3.ftimeLastWrite.hours;
         zfi.tmz_date.tm_mday = fs3.fdateLastWrite.day;
         zfi.tmz_date.tm_mon  = fs3.fdateLastWrite.month;
         zfi.tmz_date.tm_year = fs3.fdateLastWrite.year + 1980;
      }
      else
         fError = ZH_TRUE;
   }
   else
#elif defined( ZH_OS_UNIX )
   if( zh_fileIsLocalName( szFileName ) )
   {
      struct tm   st;
      time_t      ftime;
      char *      pszFree;
#  if defined( ZH_USE_LARGEFILE64 )
      struct stat64 statbuf;
      if( stat64( zh_fsNameConv( szFileName, &pszFree ), &statbuf ) == 0 )
#  else
      struct stat statbuf;
      if( stat( zh_fsNameConv( szFileName, &pszFree ), &statbuf ) == 0 )
#  endif
      {
         if( S_ISDIR( statbuf.st_mode ) )
         {
            ulExtAttr |= 0x40000000;
            ulExtAttr |= 0x10; /* FILE_ATTRIBUTE_DIRECTORY */
         }
         else
         {
            ulExtAttr |= 0x80000000;
            ulExtAttr |= 0x20; /* FILE_ATTRIBUTE_ARCHIVE */
         }

         ulExtAttr |= ( ( statbuf.st_mode & S_IXOTH ) ? 0x00010000 : 0 ) |
                      ( ( statbuf.st_mode & S_IWOTH ) ? 0x00020000 : 0 ) |
                      ( ( statbuf.st_mode & S_IROTH ) ? 0x00040000 : 0 ) |
                      ( ( statbuf.st_mode & S_IXGRP ) ? 0x00080000 : 0 ) |
                      ( ( statbuf.st_mode & S_IWGRP ) ? 0x00100000 : 0 ) |
                      ( ( statbuf.st_mode & S_IRGRP ) ? 0x00200000 : 0 ) |
                      ( ( statbuf.st_mode & S_IXUSR ) ? 0x00400000 : 0 ) |
                      ( ( statbuf.st_mode & S_IWUSR ) ? 0x00800000 : 0 ) |
                      ( ( statbuf.st_mode & S_IRUSR ) ? 0x01000000 : 0 );

         ftime = statbuf.st_mtime;
#  if defined( ZH_HAS_LOCALTIME_R )
         localtime_r( &ftime, &st );
#  else
         st = *localtime( &ftime );
#  endif

         zfi.tmz_date.tm_sec  = st.tm_sec;
         zfi.tmz_date.tm_min  = st.tm_min;
         zfi.tmz_date.tm_hour = st.tm_hour;
         zfi.tmz_date.tm_mday = st.tm_mday;
         zfi.tmz_date.tm_mon  = st.tm_mon;
         zfi.tmz_date.tm_year = st.tm_year;
      }
      else
         fError = ZH_TRUE;

      if( pszFree )
         zh_xfree( pszFree );
   }
   else
#endif
   {
      ZH_FATTR attr;
      long lJulian, lMillisec;

      if( ! zh_fileAttrGet( szFileName, &attr ) )
         ulExtAttr = 0x81B60020;  /* ZH_FA_ARCHIVE | rw-rw-rw- */
      else
      {
#if defined( ZH_OS_UNIX )
         if( attr & ZH_FA_DIRECTORY )
            ulExtAttr |= 0x40000000;
         else
         {
            ulExtAttr |= 0x80000000;
            ulExtAttr |= ZH_FA_ARCHIVE;
         }
         /* Ziher uses the same binary values for unix access rights and
          * DOS/WIN/OS2 attributes so we can use them directly
          */
         ulExtAttr |= attr & ( ZH_FA_RWXU | ZH_FA_RWXG | ZH_FA_RWXO );
#endif
         ulExtAttr |= attr & ( ZH_FA_READONLY | ZH_FA_HIDDEN | ZH_FA_SYSTEM |
                               ZH_FA_DIRECTORY | ZH_FA_ARCHIVE );
      }

      if( zh_fileTimeGet( szFileName, &lJulian, &lMillisec ) )
      {
         int iYear, iMonth, iDay;
         int iHour, iMinute, iSecond, iMSec;

         zh_dateDecode( lJulian, &iYear, &iMonth, &iDay );
         zh_timeDecode( lMillisec, &iHour, &iMinute, &iSecond, &iMSec );

         zfi.tmz_date.tm_sec  = iSecond;
         zfi.tmz_date.tm_min  = iMinute;
         zfi.tmz_date.tm_hour = iHour;
         zfi.tmz_date.tm_mday = iDay;
         zfi.tmz_date.tm_mon  = iMonth - 1;
         zfi.tmz_date.tm_year = iYear;
      }
   }

   if( fError )
      return -200;

#if ! defined( ZH_OS_UNIX )
   ulExtAttr = zh_translateExtAttr( szFileName, ulExtAttr );
#endif

   if( ! ZH_ISCHAR( iParamZipName ) )
      iParamZipName = iParamFileName;

   if( fUnicode )
   {
      szZipName = zh_strdup( zh_parstr_utf8( iParamZipName, &hZipName, NULL ) );
      szComment = zh_parstr_utf8( iParamComment, &hComment, NULL );
      flags    |= _ZIP_FLAG_UNICODE;
   }
   else
   {
      szZipName = zh_strdup( zh_parc( iParamZipName ) );
      szComment = zh_parc( iParamComment );
   }

   nLen = strlen( szZipName );
   if( iParamZipName != iParamFileName )
   {
      /* change path separators to '/' */
      while( nLen-- )
      {
         if( szZipName[ nLen ] == '\\' )
            szZipName[ nLen ] = '/';
      }
   }
   else
   {
      while( nLen-- )
      {
         if( szZipName[ nLen ] == '/' || szZipName[ nLen ] == '\\' )
         {
            memmove( szZipName, &szZipName[ nLen + 1 ], strlen( szZipName ) - nLen );
            break;
         }
      }
   }

   fText = ZH_FALSE;
   ulCRC = 0;

   zfi.external_fa = ulExtAttr;
   /* TODO: zip.exe test: 0 for binary file, 1 for text. Does not depend on
      extension. We should analyze content of file to determine this??? */
   zfi.internal_fa = 0;

   if( ulExtAttr & 0x40000000 )
   {
      iResult = zipOpenNewFileInZip4( hZip, szZipName, &zfi, NULL, 0, NULL, 0, szComment,
                                      Z_DEFLATED, Z_DEFAULT_COMPRESSION, 0,
                                      -MAX_WBITS, DEF_MEM_LEVEL, Z_DEFAULT_STRATEGY,
                                      szPassword, ulCRC, _version_made_by( fUnicode ), flags );
      if( iResult == 0 )
         zipCloseFileInZip( hZip );
   }
   else
   {
      pFile = zh_fileExtOpen( szFileName, NULL,
                              FO_READ | FO_SHARED | FO_PRIVATE | FXO_SHARELOCK,
                              NULL, NULL );
      if( pFile != NULL )
      {
#if defined( ZH_OS_WIN )
         if( zh_fileIsLocal( pFile ) )
         {
            FILETIME   ftutc, ft;
            SYSTEMTIME st;

            if( GetFileTime( ( HANDLE ) zh_fileHandle( pFile ), NULL, NULL, &ftutc ) &&
                FileTimeToLocalFileTime( &ftutc, &ft ) &&
                FileTimeToSystemTime( &ft, &st ) )
            {
               zfi.tmz_date.tm_sec  = st.wSecond;
               zfi.tmz_date.tm_min  = st.wMinute;
               zfi.tmz_date.tm_hour = st.wHour;
               zfi.tmz_date.tm_mday = st.wDay;
               zfi.tmz_date.tm_mon  = st.wMonth - 1;
               zfi.tmz_date.tm_year = st.wYear;
            }
         }
#endif
         if( szPassword )
         {
            if( zh_zipGetFileInfo( szFileName, &ulCRC, &fText ) )
               zfi.internal_fa = fText ? 1 : 0;
         }

         iResult = zipOpenNewFileInZip4( hZip, szZipName, &zfi, NULL, 0, NULL, 0, szComment,
                                         Z_DEFLATED, Z_DEFAULT_COMPRESSION, 0,
                                         -MAX_WBITS, DEF_MEM_LEVEL, Z_DEFAULT_STRATEGY,
                                         szPassword, ulCRC, _version_made_by( fUnicode ), flags );
         if( iResult == 0 )
         {
            char * pString = ( char * ) zh_xgrab( ZH_Z_IOBUF_SIZE );

            while( ( nLen = zh_fileResult( zh_fileRead( pFile, pString, ZH_Z_IOBUF_SIZE, -1 ) ) ) > 0 )
            {
               if( ( iResult = zipWriteInFileInZip( hZip, pString, ( unsigned ) nLen ) ) != ZIP_OK )
                  break;
            }

            zh_xfree( pString );

            zipCloseFileInZip( hZip );
         }
         zh_fileClose( pFile );
      }
      else
         iResult = -200 - zh_fsError();
   }

   zh_xfree( szZipName );

   if( fUnicode )
   {
      zh_strfree( hZipName );
      zh_strfree( hComment );
   }

   return iResult;
}


/* zh_zipStoreFile( hZip, cFileName, [ cZipName ], [ cPassword ], [ cComment ], [ lUnicode ] ) --> nError */
ZH_FUNC( ZH_ZIPSTOREFILE )
{
   if( zh_parc( 2 ) )
   {
      zipFile hZip = zh_zipfileParam( 1 );

      if( hZip )
         zh_retni( zh_zipStoreFile( hZip, 2, 3, zh_parc( 4 ), 5, zh_parl( 6 ) ) );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}


static int zh_zipStoreFileHandle( zipFile hZip, PZH_FILE pFile, int iParamZipName, const char * szPassword, int iParamComment, ZH_BOOL fUnicode )
{
   ZH_SIZE      nLen;
   zip_fileinfo zfi;
   int          iResult;
   ZH_BOOL      fText;
   ZH_U32       ulCRC;

   uLong flags = 0;

   void *       hZipName = NULL;
   void *       hComment = NULL;
   char *       szZipName;
   const char * szComment;

   if( pFile == NULL )
      return -200;

   if( fUnicode )
   {
      szZipName = zh_strdup( zh_parstr_utf8( iParamZipName, &hZipName, NULL ) );
      szComment = zh_parstr_utf8( iParamComment, &hComment, NULL );
      flags    |= _ZIP_FLAG_UNICODE;
   }
   else
   {
      szZipName = zh_strdup( zh_parc( iParamZipName ) );
      szComment = zh_parc( iParamComment );
   }

   /* change path separators to '/' */

   nLen = strlen( szZipName );
   while( nLen-- )
   {
      if( szZipName[ nLen ] == '\\' )
         szZipName[ nLen ] = '/';
   }

   memset( &zfi, 0, sizeof( zfi ) );

   zfi.external_fa      = 0x81B60020;
   zfi.tmz_date.tm_sec  = 0;
   zfi.tmz_date.tm_min  = 0;
   zfi.tmz_date.tm_hour = 0;
   zfi.tmz_date.tm_mday = 1;
   zfi.tmz_date.tm_mon  = 0;
   zfi.tmz_date.tm_year = 0;

   ulCRC = 0;
   fText = ZH_FALSE;
   if( szPassword && zh_zipGetFileInfoFromHandle( pFile, &ulCRC, &fText ) )
      zfi.internal_fa = fText ? 1 : 0;
   else
      /* TODO: zip.exe test: 0 for binary file, 1 for text. Does not depend on
         extension. We should analyze content of file to determine this??? */
      zfi.internal_fa = 0;

   iResult = zipOpenNewFileInZip4( hZip, szZipName, &zfi, NULL, 0, NULL, 0, szComment,
                                   Z_DEFLATED, Z_DEFAULT_COMPRESSION, 0,
                                   -MAX_WBITS, DEF_MEM_LEVEL, Z_DEFAULT_STRATEGY,
                                   szPassword, ulCRC, _version_made_by( fUnicode ), flags );
   if( iResult == 0 )
   {
      char * pString = ( char * ) zh_xgrab( ZH_Z_IOBUF_SIZE );
      zh_fileSeek( pFile, 0, FS_SET );
      while( ( nLen = zh_fileResult( zh_fileRead( pFile, pString, ZH_Z_IOBUF_SIZE, -1 ) ) ) > 0 )
      {
         if( ( iResult = zipWriteInFileInZip( hZip, pString, ( unsigned ) nLen ) ) != ZIP_OK )
            break;
      }
      zh_xfree( pString );

      zipCloseFileInZip( hZip );
   }

   zh_xfree( szZipName );

   if( fUnicode )
   {
      zh_strfree( hZipName );
      zh_strfree( hComment );
   }

   return iResult;
}


/* zh_zipStoreFileHandle( hZip, fhnd, cZipName, [ cPassword ], [ cComment ], [ lUnicode ] ) --> nError */
ZH_FUNC( ZH_ZIPSTOREFILEHANDLE )
{
   if( ZH_ISCHAR( 3 ) )
   {
      zipFile hZip = zh_zipfileParam( 1 );

      if( hZip )
      {
         ZH_BOOL fFree;
         PZH_FILE pFile = zh_fileHandleParam( 2, &fFree );

         if( pFile != NULL )
         {
            zh_retni( zh_zipStoreFileHandle( hZip, pFile, 3, zh_parc( 4 ), 5, zh_parl( 6 ) ) );
            if( fFree )
               zh_fileDetach( pFile );
         }
      }
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}


static int zh_zipStoreMem( zipFile hZip, const char * pContent, ZH_SIZE nContentLen, int iParamZipName, const char * szPassword, int iParamComment, ZH_BOOL fUnicode )
{
   ZH_SIZE      nLen;
   zip_fileinfo zfi;
   int          iResult;
   ZH_BOOL      fText;
   ZH_U32       ulCRC;

   uLong flags = 0;

   void *       hZipName = NULL;
   void *       hComment = NULL;
   char *       szZipName;
   const char * szComment;

   if( fUnicode )
   {
      szZipName = zh_strdup( zh_parstr_utf8( iParamZipName, &hZipName, NULL ) );
      szComment = zh_parstr_utf8( iParamComment, &hComment, NULL );
      flags    |= _ZIP_FLAG_UNICODE;
   }
   else
   {
      szZipName = zh_strdup( zh_parc( iParamZipName ) );
      szComment = zh_parc( iParamComment );
   }

   /* change path separators to '/' */

   nLen = strlen( szZipName );
   while( nLen-- )
   {
      if( szZipName[ nLen ] == '\\' )
         szZipName[ nLen ] = '/';
   }

   memset( &zfi, 0, sizeof( zfi ) );

   zfi.external_fa      = 0x81B60020;
   zfi.tmz_date.tm_sec  = 0;
   zfi.tmz_date.tm_min  = 0;
   zfi.tmz_date.tm_hour = 0;
   zfi.tmz_date.tm_mday = 1;
   zfi.tmz_date.tm_mon  = 0;
   zfi.tmz_date.tm_year = 0;

   ulCRC = 0;
   fText = ZH_FALSE;
   if( szPassword && zh_zipGetFileInfoFromMem( ( const unsigned char * ) pContent, nContentLen, &ulCRC, &fText ) )
      zfi.internal_fa = fText ? 1 : 0;
   else
      /* TODO: zip.exe test: 0 for binary file, 1 for text. Does not depend on
         extension. We should analyze content of file to determine this??? */
      zfi.internal_fa = 0;

   iResult = zipOpenNewFileInZip4( hZip, szZipName, &zfi, NULL, 0, NULL, 0, szComment,
                                   Z_DEFLATED, Z_DEFAULT_COMPRESSION, 0,
                                   -MAX_WBITS, DEF_MEM_LEVEL, Z_DEFAULT_STRATEGY,
                                   szPassword, ulCRC, _version_made_by( fUnicode ), flags );
   if( iResult == 0 )
   {
      while( nContentLen > 0 )
      {
         unsigned uiReadLen = ( unsigned ) ( ZH_SIZE ) ZH_MIN( UINT_MAX, nContentLen );
         if( ( iResult = zipWriteInFileInZip( hZip, pContent, ( unsigned ) uiReadLen ) ) != ZIP_OK )
            break;
         pContent += uiReadLen;
         nContentLen -= uiReadLen;
      }

      zipCloseFileInZip( hZip );
   }

   zh_xfree( szZipName );

   if( fUnicode )
   {
      zh_strfree( hZipName );
      zh_strfree( hComment );
   }

   return iResult;
}


/* zh_zipStoreFileMem( hZip, cContent, cZipName, [ cPassword ], [ cComment ], [ lUnicode ] ) --> nError */
ZH_FUNC( ZH_ZIPSTOREFILEMEM )
{
   if( ZH_ISCHAR( 3 ) )
   {
      zipFile hZip = zh_zipfileParam( 1 );

      if( hZip )
         zh_retni( zh_zipStoreMem( hZip, zh_parcx( 2 ), zh_parclen( 2 ), 3, zh_parc( 4 ), 5, zh_parl( 6 ) ) );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}


static int zh_unzipExtractCurrentFile( unzFile hUnzip, const char * szFileName, const char * szPassword )
{
   char          szNameRaw[ ZH_PATH_MAX * 3 ];
   char *        szName;
   ZH_SIZE       nPos, nLen;
   unz_file_info ufi;
   int           iResult;
   PZH_FILE      pFile;

   iResult = unzGetCurrentFileInfo( hUnzip, &ufi, szNameRaw, sizeof( szNameRaw ) - 1,
                                    NULL, 0, NULL, 0 );
   if( iResult != UNZ_OK )
      return iResult;

   iResult = unzOpenCurrentFilePassword( hUnzip, szPassword );

   if( iResult != UNZ_OK )
      return iResult;

   if( szFileName )
      szName = zh_strdup( szFileName );
   else
   {
      ZH_BOOL fUnicode = ( ufi.flag & _ZIP_FLAG_UNICODE ) != 0;

      if( fUnicode )
      {
         PZH_ITEM pTemp = zh_itemPutStrUTF8( NULL, szNameRaw );

         szName = zh_strdup( zh_itemGetCPtr( pTemp ) );

         zh_itemRelease( pTemp );
      }
      else
         szName = zh_strdup( szNameRaw );
   }

   nLen = strlen( szName );

   /* Test shows that files in subdirectories can be stored to zip file without
      explicitly adding directory. So, let's create a required path */

   nPos = 1;
   while( nPos < nLen )
   {
      char cSep = szName[ nPos ];

      /* allow both path separators, ignore terminating path separator */
      if( ( cSep == '\\' || cSep == '/' ) && nPos < nLen - 1 )
      {
         szName[ nPos ] = '\0';
         zh_fileDirMake( szName );
         szName[ nPos ] = cSep;
      }
      nPos++;
   }

   if( ufi.external_fa & 0x40000000 ) /* DIRECTORY */
   {
      if( ! zh_fileDirMake( szName ) )
         iResult = -200 - zh_fsError();
   }
   else
   {
      pFile = zh_fileExtOpen( szName, NULL,
                              FO_WRITE | FO_EXCLUSIVE | FO_PRIVATE |
                              FXO_TRUNCATE | FXO_SHARELOCK, NULL, NULL );
      if( pFile != NULL )
      {
         char * pString = ( char * ) zh_xgrab( ZH_Z_IOBUF_SIZE );

         while( ( iResult = unzReadCurrentFile( hUnzip, pString, ZH_Z_IOBUF_SIZE ) ) > 0 )
            if( zh_fileWrite( pFile, pString, ( ZH_SIZE ) iResult, -1 ) != ( ZH_SIZE ) iResult )
               break;

         zh_xfree( pString );

#if defined( ZH_OS_WIN )
         if( zh_fileIsLocal( pFile ) )
         {
            FILETIME   ftutc, ft;
            SYSTEMTIME st;

            st.wSecond       = ( WORD ) ufi.tmu_date.tm_sec;
            st.wMinute       = ( WORD ) ufi.tmu_date.tm_min;
            st.wHour         = ( WORD ) ufi.tmu_date.tm_hour;
            st.wDay          = ( WORD ) ufi.tmu_date.tm_mday;
            st.wMonth        = ( WORD ) ufi.tmu_date.tm_mon + 1;
            st.wYear         = ( WORD ) ufi.tmu_date.tm_year;
            st.wMilliseconds = 0;

            if( SystemTimeToFileTime( &st, &ft ) &&
                LocalFileTimeToFileTime( &ft, &ftutc ) )
            {
               SetFileTime( ( HANDLE ) zh_fileHandle( pFile ), &ftutc, &ftutc, &ftutc );
            }
         }
#endif

         zh_fileClose( pFile );
      }
      else
         iResult = -200 - zh_fsError();
   }
   unzCloseCurrentFile( hUnzip );

#if defined( ZH_OS_WIN )
   if( zh_fileIsLocalName( szName ) )
   {
      LPTSTR  lpFileNameFree;
      LPCTSTR lpFileName = ZH_FSNAMECONV( szName, &lpFileNameFree );

      SetFileAttributes( ( LPCTSTR ) lpFileName, ufi.external_fa & 0xFF );

      if( lpFileNameFree )
         zh_xfree( lpFileNameFree );
   }
   else
#elif defined( ZH_OS_OS2 )
   if( zh_fileIsLocalName( szName ) )
   {
      FILESTATUS3 fs3;
      APIRET      ulrc;
      ZH_FATTR    ulAttr = FILE_NORMAL;
      int         iAttr  = ufi.external_fa & 0xFF;

      char *       pszFree;
      const char * szNameOS = zh_fsNameConv( szName, &pszFree );

      if( iAttr & ZH_FA_READONLY )
         ulAttr |= FILE_READONLY;
      if( iAttr & ZH_FA_HIDDEN )
         ulAttr |= FILE_HIDDEN;
      if( iAttr & ZH_FA_SYSTEM )
         ulAttr |= FILE_SYSTEM;
      if( iAttr & ZH_FA_ARCHIVE )
         ulAttr |= FILE_ARCHIVED;

      ulrc = DosQueryPathInfo( ( PCSZ ) szNameOS, FIL_STANDARD, &fs3, sizeof( fs3 ) );

      if( ulrc == NO_ERROR )
      {
         FDATE fdate;
         FTIME ftime;

         fdate.year    = ufi.tmu_date.tm_year - 1980;
         fdate.month   = ufi.tmu_date.tm_mon;
         fdate.day     = ufi.tmu_date.tm_mday;
         ftime.hours   = ufi.tmu_date.tm_hour;
         ftime.minutes = ufi.tmu_date.tm_min;
         ftime.twosecs = ufi.tmu_date.tm_sec / 2;

         fs3.attrFile = ulAttr;

         fs3.fdateCreation = fs3.fdateLastAccess = fs3.fdateLastWrite = fdate;
         fs3.ftimeCreation = fs3.ftimeLastAccess = fs3.ftimeLastWrite = ftime;
         ( void ) DosSetPathInfo( ( PCSZ ) szNameOS, FIL_STANDARD,
                                  &fs3, sizeof( fs3 ), DSPI_WRTTHRU );
      }

      if( pszFree )
         zh_xfree( pszFree );
   }
   else
#elif defined( ZH_OS_UNIX )
   if( zh_fileIsLocalName( szName ) )
   {
      struct utimbuf utim;
      struct tm      st;

      char *       pszFree;
      const char * szNameOS = zh_fsNameConv( szName, &pszFree );

      ZH_FATTR ulAttr = ufi.external_fa;

      if( ( ulAttr & 0xFFFF0000 ) == 0 )
         ulAttr = zh_translateExtAttr( szName, ulAttr );

      ( void ) chmod( szNameOS,
                      ( ( ulAttr & 0x00010000 ) ? S_IXOTH : 0 ) |
                      ( ( ulAttr & 0x00020000 ) ? S_IWOTH : 0 ) |
                      ( ( ulAttr & 0x00040000 ) ? S_IROTH : 0 ) |
                      ( ( ulAttr & 0x00080000 ) ? S_IXGRP : 0 ) |
                      ( ( ulAttr & 0x00100000 ) ? S_IWGRP : 0 ) |
                      ( ( ulAttr & 0x00200000 ) ? S_IRGRP : 0 ) |
                      ( ( ulAttr & 0x00400000 ) ? S_IXUSR : 0 ) |
                      ( ( ulAttr & 0x00800000 ) ? S_IWUSR : 0 ) |
                      ( ( ulAttr & 0x01000000 ) ? S_IRUSR : 0 ) );
      memset( &st, 0, sizeof( st ) );

      st.tm_sec  = ufi.tmu_date.tm_sec;
      st.tm_min  = ufi.tmu_date.tm_min;
      st.tm_hour = ufi.tmu_date.tm_hour;
      st.tm_mday = ufi.tmu_date.tm_mday;
      st.tm_mon  = ufi.tmu_date.tm_mon;
      st.tm_year = ufi.tmu_date.tm_year - 1900;
      st.tm_isdst = -1;

      utim.actime = utim.modtime = mktime( &st );
      ( void ) utime( szNameOS, &utim );

      if( pszFree )
         zh_xfree( pszFree );
   }
   else
#endif
   {
      long lJulian, lMillisec;
      ZH_FATTR ulAttr = ufi.external_fa;

      lJulian = zh_dateEncode( ufi.tmu_date.tm_year, ufi.tmu_date.tm_mon + 1,
                               ufi.tmu_date.tm_mday );
      lMillisec = zh_timeEncode( ufi.tmu_date.tm_hour, ufi.tmu_date.tm_min,
                                 ufi.tmu_date.tm_sec, 0 );
      zh_fileTimeSet( szName, lJulian, lMillisec );

#if defined( ZH_OS_UNIX )
      if( ( ulAttr & 0xFFFF0000 ) == 0 )
         ulAttr = zh_translateExtAttr( szName, ulAttr );
      ulAttr &= 0x01FF0000;
#else
      ulAttr &= 0xFF;
#endif
      zh_fileAttrSet( szName, ulAttr );
   }

   zh_xfree( szName );

   return iResult;
}


/* zh_unzipExtractCurrentFile( hUnzip, [ cFileName ], [ cPassword ] ) --> nError */
ZH_FUNC( ZH_UNZIPEXTRACTCURRENTFILE )
{
   unzFile hUnzip = zh_unzipfileParam( 1 );

   if( hUnzip )
      zh_retni( zh_unzipExtractCurrentFile( hUnzip, zh_parc( 2 ), zh_parc( 3 ) ) );
}


static int zh_unzipExtractCurrentFileToHandle( unzFile hUnzip, PZH_FILE pFile, const char * szPassword )
{
   unz_file_info ufi;
   int iResult;

   if( pFile == NULL )
      return -200;

   iResult = unzGetCurrentFileInfo( hUnzip, &ufi, NULL, 0,
                                    NULL, 0, NULL, 0 );
   if( iResult != UNZ_OK )
      return iResult;

   iResult = unzOpenCurrentFilePassword( hUnzip, szPassword );

   if( iResult != UNZ_OK )
      return iResult;

   if( ! ( ufi.external_fa & 0x40000000 ) ) /* DIRECTORY */
   {
      char * pString = ( char * ) zh_xgrab( ZH_Z_IOBUF_SIZE );

      while( ( iResult = unzReadCurrentFile( hUnzip, pString, ZH_Z_IOBUF_SIZE ) ) > 0 )
         if( zh_fileWrite( pFile, pString, ( ZH_SIZE ) iResult, -1 ) != ( ZH_SIZE ) iResult )
            break;

      zh_xfree( pString );

#if defined( ZH_OS_WIN )
      if( zh_fileIsLocal( pFile ) )
      {
         FILETIME   ftutc, ft;
         SYSTEMTIME st;

         st.wSecond       = ( WORD ) ufi.tmu_date.tm_sec;
         st.wMinute       = ( WORD ) ufi.tmu_date.tm_min;
         st.wHour         = ( WORD ) ufi.tmu_date.tm_hour;
         st.wDay          = ( WORD ) ufi.tmu_date.tm_mday;
         st.wMonth        = ( WORD ) ufi.tmu_date.tm_mon + 1;
         st.wYear         = ( WORD ) ufi.tmu_date.tm_year;
         st.wMilliseconds = 0;

         if( SystemTimeToFileTime( &st, &ft ) &&
             LocalFileTimeToFileTime( &ft, &ftutc ) )
         {
            SetFileTime( ( HANDLE ) zh_fileHandle( pFile ), &ftutc, &ftutc, &ftutc );
         }
      }
#endif
   }
   unzCloseCurrentFile( hUnzip );

   return iResult;
}


/* zh_unzipExtractCurrentFileToHandle( hUnzip, fhnd, [ cPassword ] ) --> nError */
ZH_FUNC( ZH_UNZIPEXTRACTCURRENTFILETOHANDLE )
{
   unzFile hUnzip = zh_unzipfileParam( 1 );

   if( hUnzip )
   {
      ZH_BOOL fFree;
      PZH_FILE pFile = zh_fileHandleParam( 2, &fFree );

      if( pFile != NULL )
      {
         zh_retni( zh_unzipExtractCurrentFileToHandle( hUnzip, pFile, zh_parc( 3 ) ) );
         if( fFree )
            zh_fileDetach( pFile );
      }
   }
}


static int zh_unzipExtractCurrentFileToMem( unzFile hUnzip, const char * szPassword, char ** pResult, ZH_SIZE * pLen )
{
   unz_file_info ufi;
   int iResult;

   *pResult = NULL;
   *pLen = 0;

   iResult = unzGetCurrentFileInfo( hUnzip, &ufi, NULL, 0,
                                    NULL, 0, NULL, 0 );
   if( iResult != UNZ_OK )
      return iResult;

   if( ufi.uncompressed_size == 0 )
      return UNZ_OK;

   if( ufi.uncompressed_size + 1 > 256 * 1024 * 1024 )  /* avoiding exhausing memory */
      return -300;

   iResult = unzOpenCurrentFilePassword( hUnzip, szPassword );

   if( iResult != UNZ_OK )
      return iResult;

   if( ! ( ufi.external_fa & 0x40000000 ) ) /* DIRECTORY */
   {
      char * pWindow;
      ZH_SIZE nLeft;
      unsigned uiWriteLen;

      *pLen = ( ZH_SIZE ) ufi.uncompressed_size;
      *pResult = ( char * ) zh_xgrab( *pLen + 1 );

      pWindow = *pResult;
      nLeft = *pLen;

      while( nLeft > 0 &&
         ( uiWriteLen = unzReadCurrentFile( hUnzip, pWindow, ( unsigned ) ( ZH_SIZE ) ZH_MIN( ZH_Z_IOBUF_SIZE, nLeft ) ) ) > 0 )
      {
         pWindow += uiWriteLen;
         nLeft -= uiWriteLen;
      }

      if( nLeft > 0 )
      {
         zh_xfree( *pResult );
         iResult = -400;
         *pResult = NULL;
         *pLen = 0;
      }
   }
   unzCloseCurrentFile( hUnzip );

   return iResult;
}

/* zh_unzipExtractCurrentFileToMem( hUnzip, [ cPassword ], @cContent ) --> nError */
ZH_FUNC( ZH_UNZIPEXTRACTCURRENTFILETOMEM )
{
   unzFile hUnzip = zh_unzipfileParam( 1 );

   if( hUnzip )
   {
      char * pResult;
      ZH_SIZE nLen;

      zh_retni( zh_unzipExtractCurrentFileToMem( hUnzip, zh_parc( 2 ), &pResult, &nLen ) );

      if( pResult )
      {
         if( ! zh_storclen_buffer( pResult, nLen, 3 ) )
            zh_xfree( pResult );
      }
      else
         zh_storc( NULL, 3 );
   }
}


static int zh_zipDeleteFile( const char * szZipFile, const char * szFileMask )
{
   char            szTempFile[ ZH_PATH_MAX ];
   char            szCurrFile[ ZH_PATH_MAX * 3 ];
   PZH_FNAME       pFileName;
   PZH_FILE        pFile;
   unzFile         hUnzip;
   zipFile         hZip;
   unz_global_info ugi;
   unz_file_info   ufi;
   zip_fileinfo    zfi;
   char *          pszGlobalComment = NULL;
   char *          pszFileComment   = NULL;
   void *          pExtraField      = NULL;
   void *          pLocalExtraField = NULL;
   int    iFilesLeft = 0;
   int    iFilesDel  = 0;
   int    iExtraFieldLen;
   int    method;
   int    level;
   int    iResult;
   char * pszFree;

   /* open source file */
   hUnzip = unzOpen( zh_fsNameConv( szZipFile, &pszFree ) );

   if( pszFree )
      zh_xfree( pszFree );

   if( hUnzip == NULL )
      return UNZ_ERRNO;

   pFileName = zh_fsFNameSplit( szZipFile );
   pFile     = zh_fileCreateTemp( pFileName->szPath, NULL, FC_NORMAL, szTempFile );
   hZip      = NULL;
   if( pFile != NULL )
   {
      zh_fileClose( pFile );
      hZip = zipOpen( szTempFile, APPEND_STATUS_CREATE );
   }
   zh_xfree( pFileName );

   if( hZip == NULL )
   {
      unzClose( hUnzip );
      return UNZ_ERRNO;
   }

   iResult = unzGetGlobalInfo( hUnzip, &ugi );
   if( iResult == UNZ_OK )
   {
      if( ugi.size_comment > 0 )
      {
         pszGlobalComment = ( char * ) zh_xgrab( ugi.size_comment + 1 );
         if( ( uLong ) unzGetGlobalComment( hUnzip, pszGlobalComment,
                                            ugi.size_comment ) != ugi.size_comment )
            iResult = UNZ_ERRNO;
         pszGlobalComment[ ugi.size_comment ] = '\0';
      }
      if( iResult == UNZ_OK )
         iResult = unzGoToFirstFile( hUnzip );
   }

   while( iResult == UNZ_OK )
   {
      iResult = unzGetCurrentFileInfo( hUnzip, &ufi, szCurrFile, sizeof( szCurrFile ) - 1, NULL, 0, NULL, 0 );
      if( iResult != UNZ_OK )
         break;

      if( zh_strMatchFile( szCurrFile, szFileMask ) )
         iFilesDel++;
      else
      {
         ZH_BOOL fUnicode;

         if( ufi.size_file_extra )
            pExtraField = ( char * ) zh_xgrab( ufi.size_file_extra );
         if( ufi.size_file_comment )
            pszFileComment = ( char * ) zh_xgrab( ufi.size_file_comment + 1 );

         iResult = unzGetCurrentFileInfo( hUnzip, &ufi, NULL, 0,
                                          pExtraField, ufi.size_file_extra,
                                          pszFileComment, ufi.size_file_comment );
         if( pszFileComment )
            pszFileComment[ ufi.size_file_comment ] = '\0';
         if( iResult != UNZ_OK )
            break;

         iResult = unzOpenCurrentFile2( hUnzip, &method, &level, 1 );
         if( iResult != UNZ_OK )
            break;

         iExtraFieldLen = unzGetLocalExtrafield( hUnzip, NULL, 0 );
         if( iExtraFieldLen < 0 )
         {
            iResult = UNZ_ERRNO;
            break;
         }
         else if( iExtraFieldLen > 0 )
         {
            pLocalExtraField = zh_xgrab( iExtraFieldLen );
            if( unzGetLocalExtrafield( hUnzip, pLocalExtraField, iExtraFieldLen ) != iExtraFieldLen )
            {
               iResult = UNZ_ERRNO;
               break;
            }
         }

         fUnicode = ( ufi.flag & _ZIP_FLAG_UNICODE ) != 0;

         memset( &zfi, 0, sizeof( zfi ) );
         memcpy( &zfi.tmz_date, &ufi.tmu_date, sizeof( tm_unz ) );
         zfi.dosDate     = ufi.dosDate;
         zfi.internal_fa = ufi.internal_fa;
         zfi.external_fa = ufi.external_fa;

         iResult = zipOpenNewFileInZip4( hZip, szCurrFile, &zfi, pLocalExtraField, iExtraFieldLen, pExtraField, ufi.size_file_extra, pszFileComment,
                                         method, level, 1,
                                         -MAX_WBITS, DEF_MEM_LEVEL, Z_DEFAULT_STRATEGY,
                                         NULL, 0, _version_made_by( fUnicode ), ufi.flag );
         if( iResult != UNZ_OK )
            break;

         if( ufi.compressed_size )
         {
            void * buffer = zh_xgrab( ZH_Z_IOBUF_SIZE );
            uLong  ulLeft = ufi.compressed_size;

            while( ulLeft > 0 )
            {
               int iRead = ZH_MIN( ulLeft, ZH_Z_IOBUF_SIZE );
               iResult = unzReadCurrentFile( hUnzip, ( voidp ) buffer, iRead );
               if( iResult < 0 )
                  break;
               if( iResult != iRead )
               {
                  iResult = UNZ_ERRNO;
                  break;
               }
               iResult = zipWriteInFileInZip( hZip, ( voidp ) buffer, iRead );
               if( iResult != ZIP_OK )
                  break;
               ulLeft -= iRead;
            }
            zh_xfree( buffer );
            if( iResult != UNZ_OK )
               break;
         }

         iResult = zipCloseFileInZipRaw( hZip, ufi.uncompressed_size, ufi.crc );
         if( iResult != ZIP_OK )
            break;

         iResult = unzCloseCurrentFile( hUnzip );
         if( iResult != UNZ_OK )
            break;

         if( pExtraField )
         {
            zh_xfree( pExtraField );
            pExtraField = NULL;
         }
         if( pszFileComment )
         {
            zh_xfree( pszFileComment );
            pszFileComment = NULL;
         }
         if( pLocalExtraField )
         {
            zh_xfree( pLocalExtraField );
            pLocalExtraField = NULL;
         }
         iFilesLeft++;
      }
      iResult = unzGoToNextFile( hUnzip );
   }

   if( pExtraField )
      zh_xfree( pExtraField );
   if( pszFileComment )
      zh_xfree( pszFileComment );
   if( pLocalExtraField )
      zh_xfree( pLocalExtraField );

   if( iFilesDel == 0 )
      iResult = UNZ_ERRNO;
   else if( iResult == UNZ_END_OF_LIST_OF_FILE )
      iResult = UNZ_OK;

   if( iResult != UNZ_OK )
      zipClose( hZip, NULL );
   else
      iResult = zipClose( hZip, pszGlobalComment );
   unzClose( hUnzip );
   if( pszGlobalComment )
      zh_xfree( pszGlobalComment );

   if( iResult != UNZ_OK )
      zh_fileDelete( szTempFile );
   else
   {
      zh_fileDelete( szZipFile );

      if( iFilesLeft == 0 )
         zh_fileDelete( szTempFile );
      else if( ! zh_fileRename( szTempFile, szZipFile ) )
         iResult = UNZ_ERRNO;
   }

   return iResult;
}

/* zh_zipDeleteFile( cZipFile, cFileMask ) --> nError */
ZH_FUNC( ZH_ZIPDELETEFILE )
{
   const char * szZipFile  = zh_parc( 1 );
   const char * szFileMask = zh_parc( 2 );

   if( szZipFile && szFileMask )
      zh_retni( zh_zipDeleteFile( szZipFile, szFileMask ) );
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}
