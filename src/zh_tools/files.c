/*
 * CT3 files functions: SetFAttr()
 *
 * Copyright 2001 Luiz Rafael Culik <culik@sl.conex.net>
 *    SetFDaTi(), FileSMax(), FileDelete()
 * Copyright 2004 Phil Krylov <phil@newstar.rinet.ru>
 *    FileSeek(), FileSize(), FileAttr(), FileTime(), FileDate()
 *    FileMove(), FileSMax(),
 *    DeleteFile(), RenameFile()
 * Copyright 2007 Przemyslaw Czerpak
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
#include "zh_item_api.h"
#include "zh_vm.h"
#include "zh_stack.h"
#include "zh_date.h"
#include "zh_io.h"
#include "ctdisk.zhh"

#if defined( __DJGPP__ )
#  include <dpmi.h>
#  include <go32.h>
#  include <sys/farptr.h>
#  include <sys/param.h>
#endif

#if defined( ZH_OS_UNIX ) || defined( __DJGPP__ )
#  include <sys/types.h>
#  include <utime.h>
#  include <unistd.h>
#  include <time.h>
#endif

typedef struct
{
   PZH_FFIND ffind;
   ZH_FATTR  ulAttr;
} ZH_FFDATA, * PZH_FFDATA;

static void zh_fileFindRelease( void * cargo )
{
   PZH_FFDATA pFFData = ( PZH_FFDATA ) cargo;

   if( pFFData->ffind )
      zh_fsFindClose( pFFData->ffind );
}

static ZH_TSD_NEW( s_FFData, sizeof( ZH_FFDATA ), NULL, zh_fileFindRelease );

#define ZH_GET_FFDATA()  ( ( PZH_FFDATA ) zh_stackGetTSD( &s_FFData ) )

/* limit attributes to DOS ones for code portability */
#define ZH_FF_ATTR( ff ) ( ( ff )->attr & 0xFF )


static PZH_FFIND _zh_fileStart( ZH_BOOL fNext, ZH_BOOL fAny )
{
   PZH_FFDATA pFFData = ZH_GET_FFDATA();

   if( zh_pcount() > 0 )
   {
      const char * szFile = zh_parc( 1 );

      if( pFFData->ffind )
      {
         zh_fsFindClose( pFFData->ffind );
         pFFData->ffind = NULL;
      }

      if( szFile )
      {
         ZH_FATTR ulAttr;

         ulAttr = ( ZH_FATTR ) zh_parnldef( 2, fAny ? ZH_FA_ANY : ZH_FA_ALL );
         pFFData->ulAttr = zh_parl( 3 ) ? ulAttr : 0;
         pFFData->ffind  = zh_fsFindFirst( szFile, ulAttr );
         while( pFFData->ffind && pFFData->ulAttr &&
                ZH_FF_ATTR( pFFData->ffind ) != pFFData->ulAttr )
         {
            if( ! zh_fsFindNext( pFFData->ffind ) )
            {
               zh_fsFindClose( pFFData->ffind );
               pFFData->ffind = NULL;
            }
         }
      }
   }
   else if( fNext && pFFData->ffind )
   {
      do
      {
         if( ! zh_fsFindNext( pFFData->ffind ) )
         {
            zh_fsFindClose( pFFData->ffind );
            pFFData->ffind = NULL;
            break;
         }
      }
      while( pFFData->ulAttr && ZH_FF_ATTR( pFFData->ffind ) != pFFData->ulAttr );
   }

   return pFFData->ffind;
}

ZH_FUNC( FILESEEK )
{
   PZH_FFIND ffind = _zh_fileStart( ZH_TRUE, ZH_FALSE );

   zh_retc( ffind ? ffind->szName : NULL );
}

ZH_FUNC( FILEATTR )
{
   PZH_FFIND ffind = _zh_fileStart( ZH_FALSE, ZH_TRUE );

   zh_retni( ffind ? ZH_FF_ATTR( ffind ) : 0 );
}

ZH_FUNC( FILESIZE )
{
   PZH_FFIND ffind = _zh_fileStart( ZH_FALSE, ZH_FALSE );

   zh_retnint( ffind ? ffind->size : -1 );
}

ZH_FUNC( FILEDATE )
{
   PZH_FFIND ffind = _zh_fileStart( ZH_FALSE, ZH_FALSE );

   zh_retdl( ffind ? ffind->lDate : 0 );
}

ZH_FUNC( FILETIME )
{
   PZH_FFIND ffind = _zh_fileStart( ZH_FALSE, ZH_FALSE );

   zh_retc( ffind ? ffind->szTime : NULL );
}

ZH_FUNC( SETFATTR )
{
   int iResult;

   if( zh_fileAttrSet( zh_parcx( 1 ), zh_parnldef( 2, ZH_FA_ARCHIVE ) ) )
      iResult = 0;
   else
      iResult = -1;

   zh_retni( iResult );
}

ZH_FUNC( SETFDATI )
{
   const char * szFile = zh_parc( 1 );
   ZH_BOOL fResult = ZH_FALSE;

   if( szFile && *szFile )
   {
      long lJulian, lMillisec;

      if( ZH_ISTIMESTAMP( 1 ) )
      {
         if( ! zh_partdt( &lJulian, &lMillisec, 1 ) )
            lJulian = lMillisec = 0;  /* to silence Coverity analyzer */
      }
      else
      {
         PZH_ITEM pDate, pTime;

         pDate = zh_param( 2, ZH_IT_DATE );
         if( pDate )
            pTime = zh_param( 3, ZH_IT_STRING );
         else
         {
            pTime = zh_param( 2, ZH_IT_STRING );
            pDate = zh_param( 3, ZH_IT_DATE );
         }
         lJulian = pDate ? zh_itemGetDL( pDate ) : -1;
         if( pTime )
         {
            int hour = 0, minute = 0, second = 0, msec = 0;
            zh_timeStrGet( zh_itemGetCPtr( pTime ), &hour, &minute, &second, &msec );
            lMillisec = zh_timeEncode( hour, minute, second, msec );
         }
         else
            lMillisec = -1;
      }
      fResult = zh_fileTimeSet( szFile, lJulian, lMillisec );
   }

   zh_retl( fResult );
}

ZH_FUNC( FILEDELETE )
{
   const char * pszDirSpec = zh_parc( 1 );
   ZH_BOOL fResult = ZH_FALSE;

   if( pszDirSpec )
   {
      ZH_FATTR nAttr = zh_parnldef( 2, ZH_FA_ALL );
      PZH_FFIND ffind;

      /* In CT3 this function does not remove directories */
      nAttr &= ~ZH_FA_DIRECTORY;

      if( ( ffind = zh_fsFindFirst( pszDirSpec, nAttr ) ) != NULL )
      {
         PZH_FNAME pFilepath;

         pFilepath = zh_fsFNameSplit( pszDirSpec );
         pFilepath->szExtension = NULL;

         do
         {
            char szPath[ ZH_PATH_MAX ];

            pFilepath->szName = ffind->szName;
            zh_fsFNameMerge( szPath, pFilepath );

            if( ffind->attr & ZH_FA_READONLY )
            {
               if( nAttr & ZH_FA_READONLY )
                  zh_fsSetAttr( szPath, ffind->attr & ~ ( ZH_FATTR ) ZH_FA_READONLY );
               else
                  continue;
            }
            if( zh_fsDelete( szPath ) )
               fResult = ZH_TRUE;
         }
         while( zh_fsFindNext( ffind ) );

         zh_xfree( pFilepath );
         zh_fsFindClose( ffind );
      }
   }

   zh_retl( fResult );
}

ZH_FUNC( FILEMOVE )
{
   zh_retnint( zh_fileRename( zh_parcx( 1 ),
                              zh_parcx( 2 ) ) ? 0 : -( ZH_MAXINT ) zh_fsOsError() );
}

ZH_FUNC_TRANSLATE( RENAMEFILE, FILEMOVE )

ZH_FUNC( DELETEFILE )
{
   zh_retnint( zh_fileDelete( zh_parcx( 1 ) ) ? 0 : -( ZH_MAXINT ) zh_fsOsError() );
}

ZH_FUNC( FILESMAX )
{
#if defined( __DJGPP__ )
   __dpmi_regs r;
   unsigned handles;
   ZH_ULONG psp;

   r.h.ah = 0x62;               /* Get PSP address */
   __dpmi_int( 0x21, &r );
   psp = ( ( ( ZH_ULONG ) r.x.bx ) << 4 ) & 0xFFFFF;

   handles = _farpeekw( _dos_ds, psp + 0x32 );
   zh_retni( handles );
#elif defined( _SC_OPEN_MAX )
   zh_retnl( sysconf( _SC_OPEN_MAX ) );
#else
   zh_retni( -1 );
#endif
}
