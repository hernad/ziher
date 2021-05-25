/*
 * StrFile(), FileStr(), ScreenFile(), FileScreen()
 * SetFCreate(), CSetSafety()
 *
 * Copyright 2004 Pavel Tsarenko <tpe2@mail.ru>
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
#include "zh_gt_api.h"
#include "zh_stack.h"

#include "ctstrfil.h"

typedef struct
{
   ZH_FATTR nFileAttr;
   ZH_BOOL  bSafety;
} CT_STRFIL;

static void s_strfil_init( void * cargo )
{
   CT_STRFIL * strfil = ( CT_STRFIL * ) cargo;

   strfil->nFileAttr = ZH_FA_NORMAL;
   strfil->bSafety   = ZH_FALSE;
}

static ZH_TSD_NEW( s_strfil, sizeof( CT_STRFIL ), s_strfil_init, NULL );

void ct_setfcreate( ZH_FATTR nFileAttr )
{
   CT_STRFIL * strfil = ( CT_STRFIL * ) zh_stackGetTSD( &s_strfil );

   ZH_TRACE( ZH_TR_DEBUG, ( "ct_setfcreate(%u)", nFileAttr ) );

   strfil->nFileAttr = nFileAttr;
}

ZH_FATTR ct_getfcreate( void )
{
   CT_STRFIL * strfil = ( CT_STRFIL * ) zh_stackGetTSD( &s_strfil );

   ZH_TRACE( ZH_TR_DEBUG, ( "ct_getfcreate()" ) );

   return strfil->nFileAttr;
}

ZH_FUNC( SETFCREATE )
{
   zh_retnl( ct_getfcreate() );

   if( ZH_IS_PARAM_NUM( 1 ) )
      ct_setfcreate( zh_parnl( 1 ) );
}

void ct_setsafety( ZH_BOOL bSafety )
{
   CT_STRFIL * strfil = ( CT_STRFIL * ) zh_stackGetTSD( &s_strfil );

   ZH_TRACE( ZH_TR_DEBUG, ( "ct_setsafety(%i)", bSafety ) );

   strfil->bSafety = bSafety;
}

ZH_BOOL ct_getsafety( void )
{
   CT_STRFIL * strfil = ( CT_STRFIL * ) zh_stackGetTSD( &s_strfil );

   ZH_TRACE( ZH_TR_DEBUG, ( "ct_getsafety()" ) );

   return strfil->bSafety;
}

ZH_FUNC( CSETSAFETY )
{
   zh_retl( ct_getsafety() );

   if( ZH_ISLOGICAL( 1 ) )
      ct_setsafety( zh_parl( 1 ) );
}

static ZH_SIZE ct_StrFile( const char * pFileName, const char * pcStr, ZH_SIZE nLen,
                           ZH_BOOL bAppend, ZH_FOFFSET nOffset, ZH_BOOL bTrunc )
{
   ZH_SIZE nWrite = 0;
   ZH_BOOL bFile = zh_fileExists( pFileName, NULL );

   PZH_FILE hFile = zh_fileExtOpen( pFileName, NULL,
                                    FO_WRITE | FO_PRIVATE |
                                    FXO_SHARELOCK |
                                    ( bAppend ? FXO_APPEND : FXO_TRUNCATE ) |
                                    ( ct_getsafety() ? FXO_UNIQUE : 0 ),
                                    NULL, NULL );

   if( hFile )
   {
      if( ! bFile )
         zh_fileAttrSet( pFileName, ct_getfcreate() );

      if( nOffset )
         zh_fileSeek( hFile, nOffset, FS_SET );
      else
         zh_fileSeek( hFile, 0, FS_END );

      nWrite = zh_fileResult( zh_fileWrite( hFile, pcStr, nLen, -1 ) );
      if( nWrite == nLen && bTrunc )
         zh_fileWrite( hFile, NULL, 0, -1 );

      zh_fileClose( hFile );
   }
   return nWrite;
}

ZH_FUNC( STRFILE )
{
   if( ZH_ISCHAR( 1 ) && ZH_ISCHAR( 2 ) )
   {
      zh_retns( ct_StrFile( zh_parc( 2 ), zh_parc( 1 ),
                            zh_parclen( 1 ), zh_parl( 3 ),
                            ( ZH_FOFFSET ) zh_parnint( 4 ), zh_parl( 5 ) ) );
   }
   else
      zh_retns( 0 );
}

ZH_FUNC( FILESTR )
{
   if( ZH_ISCHAR( 1 ) )
   {
      PZH_FILE hFile;

      if( ( hFile = zh_fileExtOpen( zh_parc( 1 ), NULL,
                                    FO_READ | FO_SHARED | FO_PRIVATE |
                                    FXO_SHARELOCK | FXO_NOSEEKPOS,
                                    NULL, NULL ) ) != NULL )
      {
         ZH_FOFFSET nFileSize = zh_fileSize( hFile );
         ZH_FOFFSET nPos = zh_fileSeek( hFile, ( ZH_FOFFSET ) zh_parnint( 3 ), FS_SET );
         ZH_ISIZ nLength;
         char * pcResult, * pCtrlZ;
         ZH_BOOL bCtrlZ = zh_parl( 4 );

         if( ZH_IS_PARAM_NUM( 2 ) )
         {
            nLength = zh_parns( 2 );
            if( nLength > ( ZH_ISIZ ) ( nFileSize - nPos ) )
               nLength = ( ZH_ISIZ ) ( nFileSize - nPos );
         }
         else
            nLength = ( ZH_ISIZ ) ( nFileSize - nPos );

         pcResult = ( char * ) zh_xgrab( nLength + 1 );
         if( nLength > 0 )
            nLength = zh_fileResult( zh_fileRead( hFile, pcResult, ( ZH_SIZE ) nLength, -1 ) );

         if( bCtrlZ )
         {
            pCtrlZ = ( char * ) memchr( pcResult, 26, nLength );
            if( pCtrlZ )
               nLength = pCtrlZ - pcResult;
         }

         zh_fileClose( hFile );
         zh_retclen_buffer( pcResult, nLength );
      }
      else
         zh_retc_null();
   }
   else
      zh_retc_null();
}

ZH_FUNC( SCREENFILE )
{
   if( ZH_ISCHAR( 1 ) )
   {
      char * pBuffer;
      ZH_SIZE nSize;

      zh_gtRectSize( 0, 0, zh_gtMaxRow(), zh_gtMaxCol(), &nSize );
      pBuffer = ( char * ) zh_xgrab( nSize );

      zh_gtSave( 0, 0, zh_gtMaxRow(), zh_gtMaxCol(), pBuffer );

      zh_retns( ct_StrFile( zh_parc( 1 ), pBuffer,
                            nSize, zh_parl( 2 ),
                            ( ZH_FOFFSET ) zh_parnint( 3 ), zh_parl( 4 ) ) );
      zh_xfree( pBuffer );
   }
   else
      zh_retns( 0 );
}

ZH_FUNC( FILESCREEN )
{
   if( ZH_ISCHAR( 1 ) )
   {
      PZH_FILE hFile;

      if( ( hFile = zh_fileExtOpen( zh_parc( 1 ), NULL,
                                    FO_READ | FO_SHARED | FO_PRIVATE |
                                    FXO_SHARELOCK | FXO_NOSEEKPOS,
                                    NULL, NULL ) ) != NULL )
      {
         char * pBuffer;
         ZH_SIZE nSize;
         ZH_SIZE nLength;

         if( ZH_IS_PARAM_NUM( 2 ) )
            zh_fileSeek( hFile, ( ZH_FOFFSET ) zh_parnint( 2 ), FS_SET );

         zh_gtRectSize( 0, 0, zh_gtMaxRow(), zh_gtMaxCol(), &nSize );
         pBuffer = ( char * ) zh_xgrab( nSize );

         nLength = zh_fileResult( zh_fileRead( hFile, pBuffer, nSize, -1 ) );
         zh_gtRest( 0, 0, zh_gtMaxRow(), zh_gtMaxCol(), pBuffer );

         zh_xfree( pBuffer );

         zh_fileClose( hFile );
         zh_retns( nLength );
      }
      else
         zh_retns( 0 );
   }
   else
      zh_retns( 0 );
}
