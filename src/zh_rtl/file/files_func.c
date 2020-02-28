/*
 * The FileSys API (Ziher level)
 *
 * Copyright 1999-2009 Viktor Szakats
 * Copyright 2008 Przemyslaw Czerpak
 * Copyright 2000 David G. Holm <dholm@jsd-llc.com>
 * Copyright 1999 Manuel Ruiz <mrt@joca.es>
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

ZH_FUNC( FOPEN )
{
   const char * szFile = zh_parc( 1 );

   if( szFile )
   {
      zh_retnint( ( ZH_NHANDLE ) zh_fsOpen( szFile,
                  ( ZH_USHORT ) zh_parnidef( 2, FO_READ | FO_COMPAT ) ) );
      zh_fsSetFError( zh_fsError() );
   }
   else
   {
      zh_fsSetFError( 0 );
      zh_errRT_BASE( EG_ARG, 2021, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
   }
}

ZH_FUNC( FCREATE )
{
   const char * szFile = zh_parc( 1 );

   if( szFile )
   {
      zh_retnint( ( ZH_NHANDLE ) zh_fsCreate( szFile,
                  zh_parnidef( 2, FC_NORMAL ) ) );
      zh_fsSetFError( zh_fsError() );
   }
   else
   {
      zh_retni( F_ERROR );
      zh_fsSetFError( 0 );
   }
}

ZH_FUNC( ZH_FCREATE )
{
   const char * szFile = zh_parc( 1 );

   if( szFile )
   {
      zh_retnint( ( ZH_NHANDLE ) zh_fsCreateEx( szFile,
                  zh_parnidef( 2, FC_NORMAL ),
                  ( ZH_USHORT ) zh_parnidef( 3, FO_COMPAT ) ) );
      zh_fsSetFError( zh_fsError() );
   }
   else
   {
      zh_retni( F_ERROR );
      zh_fsSetFError( 0 );
   }
}

ZH_FUNC( FREAD )
{
   PZH_ITEM pBuffer = zh_param( 2, ZH_IT_STRING );
   ZH_ERRCODE uiError = 0;
   ZH_SIZE nRead = 0;

   if( ZH_IS_PARAM_NUM( 1 ) && pBuffer && ZH_ISBYREF( 2 ) && ZH_IS_PARAM_NUM( 3 ) )
   {
      char * buffer;
      ZH_SIZE nSize;

      nRead = zh_parns( 3 );

      /* NOTE: CA-Cl*pper determines the maximum size by calling _parcsiz()
               instead of _parclen(), this means that the maximum read length
               will be one more than the length of the passed buffer, because
               the terminating zero could be used if needed. [vszakats] */

      if( nRead <= zh_parcsiz( 2 ) &&
          zh_itemGetWriteCL( pBuffer, &buffer, &nSize ) )
      {
         nRead = zh_fsReadLarge( zh_numToHandle( zh_parnint( 1 ) ), buffer, nRead );
         uiError = zh_fsError();
      }
      else
         nRead = 0;
   }

   zh_retns( nRead );
   zh_fsSetFError( uiError );
}

ZH_FUNC( FWRITE )
{
   ZH_ERRCODE uiError = 0;

   if( ZH_IS_PARAM_NUM( 1 ) && ZH_ISCHAR( 2 ) )
   {
      ZH_SIZE nLen = zh_parclen( 2 );

      if( ZH_IS_PARAM_NUM( 3 ) )
      {
         ZH_SIZE nWrite = zh_parns( 3 );
         if( nWrite < nLen )
            nLen = nWrite;
      }

      zh_retns( zh_fsWriteLarge( zh_numToHandle( zh_parnint( 1 ) ), zh_parc( 2 ), nLen ) );
      uiError = zh_fsError();
   }
   else
      zh_retns( 0 );
   zh_fsSetFError( uiError );
}

ZH_FUNC( FERROR )
{
   zh_retni( zh_fsGetFError() );
}

ZH_FUNC( FCLOSE )
{
   ZH_ERRCODE uiError = 0;

   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      zh_fsClose( zh_numToHandle( zh_parnint( 1 ) ) );
      uiError = zh_fsError();
      zh_retl( uiError == 0 );
   }
   else
      zh_retl( ZH_FALSE );
   zh_fsSetFError( uiError );
}

ZH_FUNC( FERASE )
{
   ZH_ERRCODE uiError = 3;
   const char * szFile = zh_parc( 1 );

   if( szFile )
   {
      zh_retni( zh_fsDelete( szFile ) ? 0 : F_ERROR );
      uiError = zh_fsError();
   }
   else
      zh_retni( F_ERROR );
   zh_fsSetFError( uiError );
}

ZH_FUNC( FRENAME )
{
   ZH_ERRCODE uiError = 2;
   const char * szFileOld = zh_parc( 1 ),
              * szFileNew = zh_parc( 2 );

   if( szFileOld && szFileNew )
   {
      zh_retni( zh_fsRename( szFileOld, szFileNew ) ? 0 : F_ERROR );
      uiError = zh_fsError();
   }
   else
      zh_retni( F_ERROR );
   zh_fsSetFError( uiError );
}

ZH_FUNC( FSEEK )
{
   ZH_ERRCODE uiError = 0;

   if( ZH_IS_PARAM_NUM( 1 ) && ZH_IS_PARAM_NUM( 2 ) )
   {
      zh_retnint( zh_fsSeekLarge( zh_numToHandle( zh_parnint( 1 ) ),
                                  zh_parnint( 2 ),
                                  ( ZH_USHORT ) zh_parnidef( 3, FS_SET ) ) );
      uiError = zh_fsError();
   }
   else
      zh_retni( 0 );

   zh_fsSetFError( uiError );
}

ZH_FUNC( FREADSTR )
{
   ZH_ERRCODE uiError = 0;

   if( ZH_IS_PARAM_NUM( 1 ) && ZH_IS_PARAM_NUM( 2 ) )
   {
      ZH_SIZE nToRead = zh_parns( 2 );

      if( nToRead > 0 )
      {
         ZH_FHANDLE fhnd = zh_numToHandle( zh_parnint( 1 ) );
         char * buffer = ( char * ) zh_xgrab( nToRead + 1 );
         ZH_SIZE nRead;

         nRead = zh_fsReadLarge( fhnd, buffer, nToRead );
         uiError = zh_fsError();
         buffer[ nRead ] = '\0';

         zh_retc_buffer( buffer );
      }
      else
         zh_retc_null();
   }
   else
      zh_retc_null();
   zh_fsSetFError( uiError );
}

ZH_FUNC( ZH_FREADLEN )
{
   ZH_ERRCODE uiError = 0;
   ZH_SIZE nToRead = zh_parns( 2 );

   if( nToRead > 0 && ZH_IS_PARAM_NUM( 1 ) )
   {
      ZH_FHANDLE fhnd = zh_numToHandle( zh_parnint( 1 ) );
      char * buffer = ( char * ) zh_xgrab( nToRead + 1 );
      ZH_SIZE nRead;

      nRead = zh_fsReadLarge( fhnd, buffer, nToRead );
      uiError = zh_fsError();

      zh_retclen_buffer( buffer, nRead );
   }
   else
      zh_retc_null();
   zh_fsSetFError( uiError );
}

/* NOTE: This function should not return the leading and trailing
         (back)slashes. [vszakats] */

/* TODO: Xbase++ is able to change to the specified directory. */

ZH_FUNC( CURDIR )
{
   char szBuffer[ ZH_PATH_MAX ];
   int iDrive = 0;
   const char * szDrive;

   szDrive = zh_parc( 1 );
   if( szDrive )
   {
      if( *szDrive >= 'A' && *szDrive <= 'Z' )
         iDrive = *szDrive - ( 'A' - 1 );
      else if( *szDrive >= 'a' && *szDrive <= 'z' )
         iDrive = *szDrive - ( 'a' - 1 );
   }
   zh_fsCurDirBuff( iDrive, szBuffer, sizeof( szBuffer ) );

   zh_retc( szBuffer );
}

ZH_FUNC( ZH_CURDRIVE )
{
#if defined( ZH_OS_HAS_DRIVE_LETTER )
   char szCurDrive[ 1 ];
   const char * szDrive;

   szCurDrive[ 0 ] = ( ( char ) zh_fsCurDrv() ) + 'A';
   zh_retclen( szCurDrive, 1 );

   szDrive = zh_parc( 1 );
   if( szDrive )
   {
      int iDrive = -1;

      if( *szDrive >= 'A' && *szDrive <= 'Z' )
         iDrive = *szDrive - 'A';
      else if( *szDrive >= 'a' && *szDrive <= 'z' )
         iDrive = *szDrive - 'a';

      if( iDrive >= 0 )
      {
         while( zh_fsChDrv( iDrive ) != 0 )
         {
            ZH_USHORT uiAction = zh_errRT_BASE_Ext1( EG_OPEN, 6001, NULL,
                                                     ZH_ERR_FUNCNAME, 0, EF_CANDEFAULT | EF_CANRETRY,
                                                     ZH_ERR_ARGS_BASEPARAMS );
            if( uiAction != E_RETRY )
               break;
         }
      }
   }
#else
   zh_retc_null();
#endif
}

ZH_FUNC( ZH_CWD )
{
   char szBuffer[ ZH_PATH_MAX ];
   const char * szNewWD;

   if( zh_fsGetCWD( szBuffer, sizeof( szBuffer ) ) )
      zh_retc( szBuffer );
   else
      zh_retc_null();

   szNewWD = zh_parc( 1 );
   if( szNewWD )
      zh_fsSetCWD( szNewWD );

   zh_fsSetFError( zh_fsError() );
}

ZH_FUNC( ZH_PROGNAME )
{
   char * pszBaseName = zh_cmdargProgName();

   if( pszBaseName )
      zh_retc_buffer( pszBaseName );
   else
      zh_retc_null();
}

ZH_FUNC( ZH_DIRBASE )
{
   char szBuffer[ ZH_PATH_MAX ];

   zh_fsBaseDirBuff( szBuffer );

   zh_retc( szBuffer );
}

ZH_FUNC( ZH_FCOMMIT )
{
   ZH_ERRCODE uiError = 6;

   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      zh_fsCommit( zh_numToHandle( zh_parnint( 1 ) ) );
      uiError = zh_fsError();
   }

   zh_fsSetFError( uiError );
}

ZH_FUNC( ZH_FLOCK )
{
   ZH_ERRCODE uiError = 0;
   ZH_BOOL fResult = ZH_FALSE;

   if( ZH_IS_PARAM_NUM( 1 ) && ZH_IS_PARAM_NUM( 2 ) && ZH_IS_PARAM_NUM( 3 ) )
   {
      fResult = zh_fsLockLarge( zh_numToHandle( zh_parnint( 1 ) ),
                                ( ZH_FOFFSET ) zh_parnint( 2 ),
                                ( ZH_FOFFSET ) zh_parnint( 3 ),
                                FL_LOCK | ( ( ZH_USHORT ) zh_parni( 4 ) & ~FL_MASK ) );
      uiError = zh_fsError();
   }
   zh_fsSetFError( uiError );
   zh_retl( fResult );
}

ZH_FUNC( ZH_FUNLOCK )
{
   ZH_ERRCODE uiError = 0;
   ZH_BOOL fResult = ZH_FALSE;

   if( ZH_IS_PARAM_NUM( 1 ) && ZH_IS_PARAM_NUM( 2 ) && ZH_IS_PARAM_NUM( 3 ) )
   {
      fResult = zh_fsLockLarge( zh_numToHandle( zh_parnint( 1 ) ),
                                ( ZH_FOFFSET ) zh_parnint( 2 ),
                                ( ZH_FOFFSET ) zh_parnint( 3 ),
                                FL_UNLOCK );
      uiError = zh_fsError();
   }
   zh_fsSetFError( uiError );
   zh_retl( fResult );
}

ZH_FUNC( ZH_FGETATTR )
{
   ZH_FATTR nAttr;

   zh_retl( zh_fsGetAttr( zh_parcx( 1 ), &nAttr ) );
   zh_fsSetFError( zh_fsError() );

   zh_stornl( nAttr, 2 );
}

ZH_FUNC( ZH_FSETATTR )
{
   zh_retl( zh_fsSetAttr( zh_parcx( 1 ), zh_parnl( 2 ) ) );
   zh_fsSetFError( zh_fsError() );
}

ZH_FUNC( ZH_FSETDATETIME )
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

   zh_retl( zh_fsSetFileTime( zh_parcx( 1 ), lDate, lTime ) );
   zh_fsSetFError( zh_fsError() );
}

ZH_FUNC( ZH_FGETDATETIME )
{
   long lJulian, lMillisec;
   ZH_BOOL fOK;

   fOK = zh_fsGetFileTime( zh_parcx( 1 ), &lJulian, &lMillisec );
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

ZH_FUNC( ZH_FSETDEVMODE )
{
   int iRet = -1;

   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      iRet = zh_fsSetDevMode( zh_numToHandle( zh_parnint( 1 ) ), zh_parni( 2 ) );
      zh_fsSetFError( zh_fsError() );
   }
   else
      zh_fsSetFError( 6 );  /* ERROR_INVALID_HANDLE */

   zh_retni( iRet );
}

ZH_FUNC( ZH_FISDEVICE )
{
   zh_retl( zh_fsIsDevice( zh_numToHandle( zh_parnint( 1 ) ) ) );
   zh_fsSetFError( zh_fsError() );
}

/* zh_PRead( <nPipeHandle>, <@cBuffer>, [<nBytes>], [<nTimeOut>] )
      --> <nBytesRead> */
ZH_FUNC( ZH_PREAD )
{
   ZH_FHANDLE hPipe = zh_numToHandle( zh_parnintdef( 1, FS_ERROR ) );
   PZH_ITEM pBuffer = zh_param( 2, ZH_IT_STRING );
   char * buffer;
   ZH_SIZE nSize;

   if( hPipe != FS_ERROR && pBuffer && ZH_ISBYREF( 2 ) &&
       zh_itemGetWriteCL( pBuffer, &buffer, &nSize ) )
   {
      ZH_ERRCODE uiError = 0;

      if( ZH_IS_PARAM_NUM( 3 ) )
      {
         ZH_ISIZ nToRead = zh_parns( 3 );

         if( nToRead >= 0 && ( ZH_SIZE ) nToRead < nSize )
            nSize = nToRead;
      }

      if( nSize > 0 )
      {
         nSize = zh_fsPipeRead( hPipe, buffer, nSize, zh_parnint( 4 ) );
         uiError = zh_fsError();
      }

      if( nSize == ( ZH_SIZE ) FS_ERROR )
         zh_retni( FS_ERROR );
      else
         zh_retns( nSize );
      zh_fsSetFError( uiError );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 4001, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/* zh_PWrite( <nPipeHandle>, <cBuffer>, [<nBytes>], [<nTimeOut>] )
      --> <nBytesWritten> */
ZH_FUNC( ZH_PWRITE )
{
   ZH_FHANDLE hPipe = zh_numToHandle( zh_parnintdef( 1, FS_ERROR ) );
   const char * data = zh_parc( 2 );

   if( hPipe != FS_ERROR && data != NULL )
   {
      ZH_SIZE nLen = zh_parclen( 2 );

      if( ZH_IS_PARAM_NUM( 3 ) )
      {
         ZH_SIZE nWrite = zh_parns( 3 );
         if( nWrite < nLen )
            nLen = nWrite;
      }
      nLen = zh_fsPipeWrite( hPipe, data, nLen, zh_parnint( 4 ) );
      zh_fsSetFError( zh_fsError() );
      if( nLen == ( ZH_SIZE ) FS_ERROR )
         zh_retni( FS_ERROR );
      else
         zh_retns( nLen );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 4001, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_OSERROR )
{
   zh_retni( zh_fsOsError() );
}

// ZH_FUNC( PathSeparator )
ZH_FUNC( PATHSEPARATOR )
{
   zh_retc_const( ZH_OS_PATH_DELIM_CHR_STRING );
}

#if defined( ZH_LEGACY_LEVEL4 )

/* Deprecated */
ZH_FUNC( ZH_OSPATHSEPARATOR )
{
   zh_retc_const( ZH_OS_PATH_DELIM_CHR_STRING );
}

#endif

ZH_FUNC( ZH_OSPATHLISTSEPARATOR )
{
   static const char s_ret[ 2 ] = { ZH_OS_PATH_LIST_SEP_CHR, '\0' };

   zh_retc_const( s_ret );
}

ZH_FUNC( ZH_OSPATHDELIMITERS )
{
   zh_retc_const( ZH_OS_PATH_DELIM_CHR_LIST );
}

ZH_FUNC( ZH_OSDRIVESEPARATOR )
{
#ifdef ZH_OS_HAS_DRIVE_LETTER
   static const char s_ret[ 2 ] = { ZH_OS_DRIVE_DELIM_CHR, '\0' };
   zh_retc_const( s_ret );
#else
   zh_retc_null();
#endif
}

ZH_FUNC( ZH_OSFILEMASK )
{
   zh_retc_const( ZH_OS_ALLFILE_MASK );
}
