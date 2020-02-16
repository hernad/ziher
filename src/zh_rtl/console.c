/*
 * The Console API
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * Copyright 1999-2015 Viktor Szakats (zh_conNewLine(), DispOutAt(), zh_StrEOL())
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    zh_conOutAlt(), zh_conOutDev(), DevOut(), zh_conDevPos(),
 *    DevPos(), __Eject(),
 *    zh_conOut(), zh_conOutErr(), OutErr(),
 *    zh_conOutStd(), OutStd(), PCol(), PRow(),
 *    SetPRC(), and zh_conInit()
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
#include "zh_codepage_api.h"
#include "zh_item_api.h"
#include "zh_apifs.h"
#include "zh_api_error.h"
#include "zh_gt_api.h"
#include "zh_stack.h"
#include "zh_set.h"
#include "zh_io.h"

#if defined( ZH_OS_WIN )
   #include <windows.h>
   #if defined( ZH_OS_WIN_CE )
      #include "hbwince.h"
   #endif
#endif

/* NOTE: Some C compilers like Borland C optimize the call of small static buffers
 *       into an integer to read it faster. Later, programs like CodeGuard
 *       complain if the given buffer was smaller than an int. [ckedem]
 */

/* length of buffer for CR/LF characters */
#if ! defined( ZH_OS_EOL_LEN ) || ZH_OS_EOL_LEN < 4
#  define CRLF_BUFFER_LEN  4
#else
#  define CRLF_BUFFER_LEN  ZH_OS_EOL_LEN + 1
#endif

static const char s_szCR[] = { ZH_CHAR_CR, 0 };
static const char s_szLF[] = { ZH_CHAR_LF, 0 };
static const char s_szCRLF[] = { ZH_CHAR_CR, ZH_CHAR_LF, 0 };

#if defined( ZH_OS_UNIX ) && ! defined( ZH_EOL_CRLF )
   static const char * s_szEOL = s_szLF;
   static const int s_iEOLLen = 1;
#else
   static const char * s_szEOL = s_szCRLF;
   static const int s_iEOLLen = 2;
#endif

static ZH_FHANDLE s_hFilenoStdin  = ( ZH_FHANDLE ) ZH_STDIN_HANDLE;
static ZH_FHANDLE s_hFilenoStdout = ( ZH_FHANDLE ) ZH_STDOUT_HANDLE;
static ZH_FHANDLE s_hFilenoStderr = ( ZH_FHANDLE ) ZH_STDERR_HANDLE;

typedef struct
{
   int row;
   int col;
} ZH_PRNPOS, * PZH_PRNPOS;

static ZH_TSD_NEW( s_prnPos, sizeof( ZH_PRNPOS ), NULL, NULL );

static PZH_PRNPOS zh_prnPos( void )
{
   return ( PZH_PRNPOS ) zh_stackGetTSD( &s_prnPos );
}

void zh_conInit( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_conInit()" ) );

#if ! defined( ZH_OS_WIN )
   /* On Windows file handles with numbers 0, 1, 2 are
      translated inside filesys to:
      GetStdHandle( STD_INPUT_HANDLE ), GetStdHandle( STD_OUTPUT_HANDLE ),
      GetStdHandle( STD_ERROR_HANDLE ) */

   s_hFilenoStdin  = fileno( stdin );
   s_hFilenoStdout = fileno( stdout );
   s_hFilenoStderr = fileno( stderr );

#endif

   {
      /* Undocumented CA-Cl*pper switch //STDERR:x */
      int iStderr = zh_cmdargNum( "STDERR" );

      if( iStderr == 0 || iStderr == 1 )  /* //STDERR with no parameter or 0 */
         s_hFilenoStderr = s_hFilenoStdout;
      /* disabled in default builds. It's not multi-platform and very
       * dangerous because it can redirect error messages to data files
       * [druzus]
       */
   }

   /*
    * Some compilers open stdout and stderr in text mode, but
    * Ziher needs them to be open in binary mode.
    */
   zh_fsSetDevMode( s_hFilenoStdin,  FD_BINARY );
   zh_fsSetDevMode( s_hFilenoStdout, FD_BINARY );
   zh_fsSetDevMode( s_hFilenoStderr, FD_BINARY );

   if( zh_gtInit( s_hFilenoStdin, s_hFilenoStdout, s_hFilenoStderr ) != ZH_SUCCESS )
      zh_errInternal( 9995, "Ziher terminal (GT) initialization failure", NULL, NULL );

   if( zh_cmdargCheck( "INFO" ) )
   {
      zh_conOutErr( zh_gtVersion( 1 ), 0 );
      zh_conOutErr( zh_conNewLine(), 0 );
   }
}

void zh_conRelease( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_conRelease()" ) );

   /*
    * Clipper does not restore screen size on exit so I removed the code with:
    *    zh_gtSetMode( s_originalMaxRow + 1, s_originalMaxCol + 1 );
    * If the low-level GT drive change some video adapter parameters which
    * have to be restored on exit then it should does it in its Exit()
    * method. Here we cannot force any actions because it may cause bad
    * results in some GTs, e.g. when the screen size is controlled by remote
    * user and not Ziher application (some terminal modes), [Druzus]
    */

   zh_gtExit();

   zh_fsSetDevMode( s_hFilenoStdin,  FD_TEXT );
   zh_fsSetDevMode( s_hFilenoStdout, FD_TEXT );
   zh_fsSetDevMode( s_hFilenoStderr, FD_TEXT );
}

const char * zh_conNewLine( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_conNewLine()" ) );

   return s_szEOL;
}

ZH_FUNC( ZH_EOL )
{
   zh_retc_const( s_szEOL );
}


ZH_FUNC( ZH_STREOL )
{
   ZH_SIZE nLen = zh_parclen( 1 );

   ZH_SIZE nCR = 0;
   ZH_SIZE nLF = 0;

   const char * szEOL = s_szEOL;

   if( nLen > 0 )
   {
      const char * szText = zh_parc( 1 );

      do
      {
         switch( *szText++ )
         {
         case ZH_CHAR_CR:
            ++nCR;
            break;
         case ZH_CHAR_LF:
            ++nLF;
            break;
         }
      }
      while( --nLen );

      if( nLF )
      {
         if( nCR == 0 )
            szEOL = s_szLF;
         else if( nCR == nLF )
            szEOL = s_szCRLF;
      }
      else if( nCR )
         szEOL = s_szCR;
   }

   zh_retc_const( szEOL );
}

/* Output an item to STDOUT */
void zh_conOutStd( const char * szStr, ZH_SIZE nLen )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_conOutStd(%s, %" ZH_PFS "u)", szStr, nLen ) );

   if( nLen == 0 )
      nLen = strlen( szStr );

   if( nLen > 0 )
      zh_gtOutStd( szStr, nLen );
}

/* Output an item to STDERR */
void zh_conOutErr( const char * szStr, ZH_SIZE nLen )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_conOutErr(%s, %" ZH_PFS "u)", szStr, nLen ) );

   if( nLen == 0 )
      nLen = strlen( szStr );

   if( nLen > 0 )
      zh_gtOutErr( szStr, nLen );
}

/* Output an item to the screen and/or printer and/or alternate */
void zh_conOutAlt( const char * szStr, ZH_SIZE nLen )
{
   PZH_FILE pFile;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_conOutAlt(%s, %" ZH_PFS "u)", szStr, nLen ) );

   if( zh_setGetConsole() )
      zh_gtWriteCon( szStr, nLen );

   if( zh_setGetAlternate() && ( pFile = zh_setGetAltHan() ) != NULL )
   {
      /* Print to alternate file if SET ALTERNATE ON and valid alternate file */
      zh_fileWrite( pFile, szStr, nLen, -1 );
   }

   if( ( pFile = zh_setGetExtraHan() ) != NULL )
   {
      /* Print to extra file if valid alternate file */
      zh_fileWrite( pFile, szStr, nLen, -1 );
   }

   if( ( pFile = zh_setGetPrinterHandle( ZH_SET_PRN_CON ) ) != NULL )
   {
      /* Print to printer if SET PRINTER ON and valid printer file */
      zh_fileWrite( pFile, szStr, nLen, -1 );
      zh_prnPos()->col += ( int ) nLen;
   }
}

/* Output an item to the screen and/or printer */
static void zh_conOutDev( const char * szStr, ZH_SIZE nLen )
{
   PZH_FILE pFile;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_conOutDev(%s, %" ZH_PFS "u)", szStr, nLen ) );

   if( ( pFile = zh_setGetPrinterHandle( ZH_SET_PRN_DEV ) ) != NULL )
   {
      /* Display to printer if SET DEVICE TO PRINTER and valid printer file */
      zh_fileWrite( pFile, szStr, nLen, -1 );
      zh_prnPos()->col += ( int ) nLen;
   }
   else
      /* Otherwise, display to console */
      zh_gtWrite( szStr, nLen );
}

static char * zh_itemStringCon( PZH_ITEM pItem, ZH_SIZE * pnLen, ZH_BOOL * pfFreeReq )
{
   /* logical values in device output (not console, stdout or stderr) are
      shown as single letter */
   if( ZH_IS_LOGICAL( pItem ) )
   {
      *pnLen = 1;
      *pfFreeReq = ZH_FALSE;
      return ( char * ) ( zh_itemGetL( pItem ) ? "T" : "F" );
   }
   return zh_itemString( pItem, pnLen, pfFreeReq );
}

ZH_FUNC( OUTSTD ) /* writes a list of values to the standard output device */
{
   int iPCount = zh_pcount(), iParam;

   for( iParam = 1; iParam <= iPCount; iParam++ )
   {
      char * pszString;
      ZH_SIZE nLen;
      ZH_BOOL fFree;

      if( iParam > 1 )
         zh_conOutStd( " ", 1 );
      pszString = zh_itemString( zh_param( iParam, ZH_IT_ANY ), &nLen, &fFree );
      if( nLen )
         zh_conOutStd( pszString, nLen );
      if( fFree )
         zh_xfree( pszString );
   }
}

ZH_FUNC( OUTERR ) /* writes a list of values to the standard error device */
{
   int iPCount = zh_pcount(), iParam;

   for( iParam = 1; iParam <= iPCount; iParam++ )
   {
      char * pszString;
      ZH_SIZE nLen;
      ZH_BOOL fFree;

      if( iParam > 1 )
         zh_conOutErr( " ", 1 );
      pszString = zh_itemString( zh_param( iParam, ZH_IT_ANY ), &nLen, &fFree );
      if( nLen )
         zh_conOutErr( pszString, nLen );
      if( fFree )
         zh_xfree( pszString );
   }
}

ZH_FUNC( QQOUT ) /* writes a list of values to the current device (screen or printer) and is affected by SET ALTERNATE */
{
   int iPCount = zh_pcount(), iParam;

   for( iParam = 1; iParam <= iPCount; iParam++ )
   {
      char * pszString;
      ZH_SIZE nLen;
      ZH_BOOL fFree;

      if( iParam > 1 )
         zh_conOutAlt( " ", 1 );
      pszString = zh_itemString( zh_param( iParam, ZH_IT_ANY ), &nLen, &fFree );
      if( nLen )
         zh_conOutAlt( pszString, nLen );
      if( fFree )
         zh_xfree( pszString );
   }
}

ZH_FUNC( QOUT )
{
   PZH_FILE pFile;

   zh_conOutAlt( s_szEOL, s_iEOLLen );

   if( ( pFile = zh_setGetPrinterHandle( ZH_SET_PRN_CON ) ) != NULL )
   {
      PZH_PRNPOS pPrnPos = zh_prnPos();

      pPrnPos->row++;
      pPrnPos->col = zh_setGetMargin();

      if( pPrnPos->col )
      {
         char buf[ 256 ];

         if( pPrnPos->col > ( int ) sizeof( buf ) )
         {
            char * pBuf = ( char * ) zh_xgrab( pPrnPos->col );
            memset( pBuf, ' ', pPrnPos->col );
            zh_fileWrite( pFile, pBuf, ( ZH_USHORT ) pPrnPos->col, -1 );
            zh_xfree( pBuf );
         }
         else
         {
            memset( buf, ' ', pPrnPos->col );
            zh_fileWrite( pFile, buf, ( ZH_USHORT ) pPrnPos->col, -1 );
         }
      }
   }

   ZH_FUNC_EXEC( QQOUT );
}

ZH_FUNC( __EJECT ) /* Ejects the current page from the printer */
{
   PZH_PRNPOS pPrnPos;
   PZH_FILE pFile;

   if( ( pFile = zh_setGetPrinterHandle( ZH_SET_PRN_ANY ) ) != NULL )
   {
      static const char s_szEop[ 4 ] = { 0x0C, 0x0D, 0x00, 0x00 }; /* Buffer is 4 bytes to make CodeGuard happy */
      zh_fileWrite( pFile, s_szEop, 2, -1 );
   }

   pPrnPos = zh_prnPos();
   pPrnPos->row = pPrnPos->col = 0;
}

ZH_FUNC( PROW ) /* Returns the current printer row position */
{
   zh_retni( ( int ) zh_prnPos()->row );
}

ZH_FUNC( PCOL ) /* Returns the current printer row position */
{
   zh_retni( ( int ) zh_prnPos()->col );
}

static void zh_conDevPos( int iRow, int iCol )
{
   PZH_FILE pFile;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_conDevPos(%d, %d)", iRow, iCol ) );

   /* Position printer if SET DEVICE TO PRINTER and valid printer file
      otherwise position console */

   if( ( pFile = zh_setGetPrinterHandle( ZH_SET_PRN_DEV ) ) != NULL )
   {
      int iPRow = iRow;
      int iPCol = iCol + zh_setGetMargin();
      PZH_PRNPOS pPrnPos = zh_prnPos();

      if( pPrnPos->row != iPRow || pPrnPos->col != iPCol )
      {
         char buf[ 256 ];
         int iPtr = 0;

         if( pPrnPos->row != iPRow )
         {
            if( ++pPrnPos->row > iPRow )
            {
               memcpy( &buf[ iPtr ], "\x0C\x0D\x00\x00", 2 );  /* Source buffer is 4 bytes to make CodeGuard happy */
               iPtr += 2;
               pPrnPos->row = 0;
            }
            else
            {
               memcpy( &buf[ iPtr ], s_szEOL, s_iEOLLen );
               iPtr += s_iEOLLen;
            }

            while( pPrnPos->row < iPRow )
            {
               if( iPtr + s_iEOLLen > ( int ) sizeof( buf ) )
               {
                  zh_fileWrite( pFile, buf, ( ZH_USHORT ) iPtr, -1 );
                  iPtr = 0;
               }
               memcpy( &buf[ iPtr ], s_szEOL, s_iEOLLen );
               iPtr += s_iEOLLen;
               ++pPrnPos->row;
            }
            pPrnPos->col = 0;
         }
         else if( pPrnPos->col > iPCol )
         {
            buf[ iPtr++ ] = '\x0D';
            pPrnPos->col = 0;
         }

         while( pPrnPos->col < iPCol )
         {
            if( iPtr == ( int ) sizeof( buf ) )
            {
               zh_fileWrite( pFile, buf, ( ZH_USHORT ) iPtr, -1 );
               iPtr = 0;
            }
            buf[ iPtr++ ] = ' ';
            ++pPrnPos->col;
         }

         if( iPtr )
            zh_fileWrite( pFile, buf, ( ZH_USHORT ) iPtr, -1 );
      }
   }
   else
      zh_gtSetPos( iRow, iCol );
}

/* NOTE: This should be placed after the zh_conDevPos() definition. */

ZH_FUNC( DEVPOS ) /* Sets the screen and/or printer position */
{
   if( ZH_IS_PARAM_NUM( 1 ) && ZH_IS_PARAM_NUM( 2 ) )
      zh_conDevPos( zh_parni( 1 ), zh_parni( 2 ) );

   zh_itemReturn( zh_param( 1, ZH_IT_ANY ) );
}

ZH_FUNC( SETPRC ) /* Sets the current printer row and column positions */
{
   if( zh_pcount() == 2 && ZH_IS_PARAM_NUM( 1 ) && ZH_IS_PARAM_NUM( 2 ) )
   {
      PZH_PRNPOS pPrnPos = zh_prnPos();
      pPrnPos->row = zh_parni( 1 );
      pPrnPos->col = zh_parni( 2 );
   }
}

ZH_FUNC( DEVOUT ) /* writes a single value to the current device (screen or printer), but is not affected by SET ALTERNATE */
{
   char * pszString;
   ZH_SIZE nLen;
   ZH_BOOL fFree;

   if( ZH_ISCHAR( 2 ) )
   {
      char szOldColor[ ZH_CLRSTR_LEN ];

      zh_gtGetColorStr( szOldColor );
      zh_gtSetColorStr( zh_parc( 2 ) );

      pszString = zh_itemStringCon( zh_param( 1, ZH_IT_ANY ), &nLen, &fFree );
      if( nLen )
         zh_conOutDev( pszString, nLen );
      if( fFree )
         zh_xfree( pszString );

      zh_gtSetColorStr( szOldColor );
   }
   else if( zh_pcount() >= 1 )
   {
      pszString = zh_itemStringCon( zh_param( 1, ZH_IT_ANY ), &nLen, &fFree );
      if( nLen )
         zh_conOutDev( pszString, nLen );
      if( fFree )
         zh_xfree( pszString );
   }
}

ZH_FUNC( DISPOUT ) /* writes a single value to the screen, but is not affected by SET ALTERNATE */
{
   char * pszString;
   ZH_SIZE nLen;
   ZH_BOOL bFreeReq;

   if( ZH_ISCHAR( 2 ) )
   {
      char szOldColor[ ZH_CLRSTR_LEN ];

      zh_gtGetColorStr( szOldColor );
      zh_gtSetColorStr( zh_parc( 2 ) );

      pszString = zh_itemStringCon( zh_param( 1, ZH_IT_ANY ), &nLen, &bFreeReq );

      zh_gtWrite( pszString, nLen );

      if( bFreeReq )
         zh_xfree( pszString );

      zh_gtSetColorStr( szOldColor );
   }
   else if( zh_pcount() >= 1 )
   {
      pszString = zh_itemStringCon( zh_param( 1, ZH_IT_ANY ), &nLen, &bFreeReq );

      zh_gtWrite( pszString, nLen );

      if( bFreeReq )
         zh_xfree( pszString );
   }
}

/* Undocumented Clipper function */

/* NOTE: Clipper does no checks about the screen positions. [vszakats] */

ZH_FUNC( DISPOUTAT )  /* writes a single value to the screen at specific position, but is not affected by SET ALTERNATE */
{
   char * pszString;
   ZH_SIZE nLen;
   ZH_BOOL bFreeReq;

   if( ZH_ISCHAR( 4 ) )
   {
      char szOldColor[ ZH_CLRSTR_LEN ];

      zh_gtGetColorStr( szOldColor );
      zh_gtSetColorStr( zh_parc( 4 ) );

      pszString = zh_itemStringCon( zh_param( 3, ZH_IT_ANY ), &nLen, &bFreeReq );

      zh_gtWriteAt( zh_parni( 1 ), zh_parni( 2 ), pszString, nLen );

      if( bFreeReq )
         zh_xfree( pszString );

      zh_gtSetColorStr( szOldColor );
   }
   else if( zh_pcount() >= 3 )
   {
      pszString = zh_itemStringCon( zh_param( 3, ZH_IT_ANY ), &nLen, &bFreeReq );

      zh_gtWriteAt( zh_parni( 1 ), zh_parni( 2 ), pszString, nLen );

      if( bFreeReq )
         zh_xfree( pszString );
   }
}

/* Ziher extension, works like DispOutAt() but does not change cursor position */

ZH_FUNC( ZH_DISPOUTAT )
{
   if( zh_pcount() >= 3 )
   {
      char * pszString;
      ZH_SIZE nLen;
      ZH_BOOL bFreeReq;
      int iColor;

      pszString = zh_itemStringCon( zh_param( 3, ZH_IT_ANY ), &nLen, &bFreeReq );

      if( ZH_ISCHAR( 4 ) )
         iColor = zh_gtColorToN( zh_parc( 4 ) );
      else if( ZH_IS_PARAM_NUM( 4 ) )
         iColor = zh_parni( 4 );
      else
         iColor = -1;

      zh_gtPutText( zh_parni( 1 ), zh_parni( 2 ), pszString, nLen, iColor );

      if( bFreeReq )
         zh_xfree( pszString );
   }
}

/* Same as zh_DispOutAt(), but draws with the attribute ZH_GT_ATTR_BOX,
   so we can use it to draw graphical elements. */
ZH_FUNC( ZH_DISPOUTATBOX )
{
   ZH_SIZE nLen = zh_parclen( 3 );

   if( nLen > 0 )
   {
      int iRow = zh_parni( 1 );
      int iCol = zh_parni( 2 );
      const char * pszString = zh_parc( 3 );
      int iColor;
      PZH_CODEPAGE cdp;
      ZH_SIZE nIndex = 0;
      ZH_WCHAR wc;

      if( ZH_ISCHAR( 4 ) )
         iColor = zh_gtColorToN( zh_parc( 4 ) );
      else if( ZH_IS_PARAM_NUM( 4 ) )
         iColor = zh_parni( 4 );
      else
         iColor = zh_gtGetCurrColor();

      cdp = zh_gtBoxCP();

      while( ZH_CODEPAGE_CHAR_GET( cdp, pszString, nLen, &nIndex, &wc ) )
         zh_gtPutChar( iRow, iCol++, iColor, ZH_GT_ATTR_BOX, wc );

      zh_gtFlush();
   }
}

ZH_FUNC( ZH_GETSTDIN ) /* Return handle for STDIN */
{
   zh_retnint( ( ZH_NHANDLE ) s_hFilenoStdin );
}

ZH_FUNC( ZH_GETSTDOUT ) /* Return handle for STDOUT */
{
   zh_retnint( ( ZH_NHANDLE ) s_hFilenoStdout );
}

ZH_FUNC( ZH_GETSTDERR ) /* Return handle for STDERR */
{
   zh_retnint( ( ZH_NHANDLE ) s_hFilenoStderr );
}
