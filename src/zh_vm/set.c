/*
 * Set functions
 *
 * Copyright 1999-2003 David G. Holm <dholm@jsd-llc.com>
 * Copyright 2008-2009 Viktor Szakats (zh_osEncode(), zh_osDecode())
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

#define _ZH_SET_INTERNAL_

#include "zh_vm_opt.h"
#include "zh_api.h"
#include "zh_item_api.h"
#include "zh_api_error.h"
#include "zh_apifs.h"
#include "zh_gt_api.h"
#include "zh_lang_api.h"
#include "zh_codepage_api.h"
#include "zh_string_api.h"
#include "zh_set.h"
#include "zh_stack.h"
#include "zh_vm.h"

typedef struct ZH_SET_LISTENER_
{
   int listener;
   ZH_SET_LISTENER_CALLBACK * callback;
   struct ZH_SET_LISTENER_ * next;
} ZH_SET_LISTENER, * PZH_SET_LISTENER;

typedef struct
{
   PZH_SET_LISTENER  first;
   PZH_SET_LISTENER  last;
   int               counter;
} ZH_SET_LISTENER_LST, * PZH_SET_LISTENER_LST;

static char set_char( PZH_ITEM pItem, char oldChar )
{
   char newChar = oldChar;

   ZH_TRACE( ZH_TR_DEBUG, ( "set_char(%p, %c)", ( void * ) pItem, oldChar ) );

   if( ZH_IS_STRING( pItem ) )
   {
      /* Only replace if string has at least one character. */
      ZH_SIZE nLen = zh_itemGetCLen( pItem );
      if( nLen > 0 )
      {
         newChar = *zh_itemGetCPtr( pItem );
      }
   }
   return newChar;
}

/*
 * Change the setting if the parameter is a logical value, or is
 * either "ON" or "OFF" (regardless of case)
 */
static ZH_BOOL set_logical( PZH_ITEM pItem, ZH_BOOL bDefault )
{
   ZH_BOOL bLogical = bDefault;

   ZH_TRACE( ZH_TR_DEBUG, ( "set_logical(%p)", ( void * ) pItem ) );

   if( pItem )
   {
      if( ZH_IS_LOGICAL( pItem ) )
         bLogical = zh_itemGetL( pItem );
      else if( ZH_IS_STRING( pItem ) )
      {
         const char * szString = zh_itemGetCPtr( pItem );
         ZH_SIZE nLen = zh_itemGetCLen( pItem );

         if( nLen >= 2
             && ( ( ZH_UCHAR ) szString[ 0 ] == 'O' || ( ZH_UCHAR ) szString[ 0 ] == 'o' )
             && ( ( ZH_UCHAR ) szString[ 1 ] == 'N' || ( ZH_UCHAR ) szString[ 1 ] == 'n' ) )
            bLogical = ZH_TRUE;
         else if( nLen >= 3
                  && ( ( ZH_UCHAR ) szString[ 0 ] == 'O' || ( ZH_UCHAR ) szString[ 0 ] == 'o' )
                  && ( ( ZH_UCHAR ) szString[ 1 ] == 'F' || ( ZH_UCHAR ) szString[ 1 ] == 'f' )
                  && ( ( ZH_UCHAR ) szString[ 2 ] == 'F' || ( ZH_UCHAR ) szString[ 2 ] == 'f' ) )
            bLogical = ZH_FALSE;
      }
   }

   return bLogical;
}

static int set_number( PZH_ITEM pItem, int iOldValue )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "set_number(%p, %d)", ( void * ) pItem, iOldValue ) );

   return ZH_IS_NUMERIC( pItem ) ? zh_itemGetNI( pItem ) : iOldValue;
}

static char * set_string( PZH_ITEM pItem, char * szOldString )
{
   char * szString;

   ZH_TRACE( ZH_TR_DEBUG, ( "set_string(%p, %s)", ( void * ) pItem, szOldString ) );

   if( ZH_IS_STRING( pItem ) || ZH_IS_NIL( pItem ) )
   {
      if( szOldString )
         zh_xfree( szOldString );
      /* Limit size of SET strings to 64 KiB, truncating if source is longer */
      szString = zh_strndup( zh_itemGetCPtr( pItem ), USHRT_MAX );
   }
   else
      szString = szOldString;

   return szString;
}

static void close_handle( PZH_SET_STRUCT pSet, ZH_set_enum set_specifier )
{
   PZH_FILE * handle_ptr;

   ZH_TRACE( ZH_TR_DEBUG, ( "close_handle(%p, %d)", ( void * ) pSet, ( int ) set_specifier ) );

   switch( set_specifier )
   {
      case ZH_SET_ALTFILE:
         handle_ptr = &pSet->zh_set_althan;
         break;
      case ZH_SET_PRINTFILE:
         handle_ptr = &pSet->zh_set_printhan;
         break;
      case ZH_SET_EXTRAFILE:
         handle_ptr = &pSet->zh_set_extrahan;
         break;
      default:
         return;
   }

   if( *handle_ptr != NULL )
   {
      if( set_specifier != ZH_SET_PRINTFILE && pSet->ZH_SET_EOF )
         zh_fileWrite( *handle_ptr, "\x1A", 1, -1 );
      zh_fileClose( *handle_ptr );
      *handle_ptr = NULL;
   }
}

static const char * is_devicename( const char * szFileName )
{
   if( szFileName && *szFileName )
   {
#if defined( ZH_OS_WIN )
      const char * szDevices[] =
            { "NUL", "PRN", "CON",
              "LPT1", "LPT2", "LPT3",
              "COM1", "COM2", "COM3", "COM4", "COM5",
              "COM6", "COM7", "COM8", "COM9" };
      int iSkip = 0, iLen;

      if( ( szFileName[ 0 ] == '\\' || szFileName[ 0 ] == '/' ) &&
          ( szFileName[ 1 ] == '\\' || szFileName[ 1 ] == '/' ) )
      {
         if( szFileName[ 2 ] == '.' &&
             ( szFileName[ 3 ] == '\\' || szFileName[ 3 ] == '/' ) )
         {
            iSkip = 4;
            if( zh_strnicmp( szFileName + 4, "PIPE", 4 ) == 0 &&
                ( szFileName[ 8 ] == '\\' || szFileName[ 8 ] == '/' ) )
               return szFileName;
         }
         if( szFileName[ 2 ] != '\\' && szFileName[ 2 ] != '/' )
         {
            int iFrom, iTo;
            for( iFrom = 2, iTo = 0; szFileName[ iFrom ]; ++iFrom )
            {
               if( szFileName[ iFrom ] == '\\' || szFileName[ iFrom ] == '/' )
               {
                  if( iTo++ )
                     break;
               }
            }
            if( iTo == 1 )
               return szFileName;
         }
      }
      iLen = ( int ) strlen( szFileName + iSkip );
      if( iLen >= 3 && iLen <= 4 )
      {
         int iFrom, iTo;

         if( iLen == 3 )
         {
            iFrom = 0;
            iTo = 3;
         }
         else
         {
            iFrom = 3;
            iTo = ZH_SIZEOFARRAY( szDevices );
         }
         for( ; iFrom < iTo; ++iFrom )
         {
            if( zh_stricmp( szFileName + iSkip, szDevices[ iFrom ] ) == 0 )
               return iSkip ? szFileName : szDevices[ iFrom ];
         }
      }
#elif defined( ZH_OS_UNIX )
      if( strncmp( szFileName, "/dev/", 5 ) == 0 )
         return szFileName;
      else
      {
         ZH_FATTR ulAttr = 0;
         if( zh_fsGetAttr( szFileName, &ulAttr ) )
         {
            if( ulAttr & ( ZH_FA_CHRDEVICE | ZH_FA_BLKDEVICE | ZH_FA_FIFO | ZH_FA_SOCKET ) )
               return szFileName;
         }
      }
#endif
   }
   return NULL;
}

static void open_handle( PZH_SET_STRUCT pSet, const char * file_name,
                         ZH_BOOL fAppend, ZH_set_enum set_specifier )
{
   ZH_STACK_TLS_PRELOAD
   PZH_ITEM pError = NULL;
   PZH_FILE handle, * handle_ptr;
   ZH_ERRCODE uiError;
   const char * szDevice = NULL, * def_ext;
   char * szFileName = NULL;
   char ** set_value;
   ZH_BOOL fPipe = ZH_FALSE, fStripEof;

   ZH_TRACE( ZH_TR_DEBUG, ( "open_handle(%p, %s, %d, %d)", ( void * ) pSet, file_name, ( int ) fAppend, ( int ) set_specifier ) );

   switch( set_specifier )
   {
      case ZH_SET_ALTFILE:
         uiError = 2013;
         set_value = &pSet->ZH_SET_ALTFILE;
         handle_ptr = &pSet->zh_set_althan;
         def_ext = ".txt";
         break;
      case ZH_SET_PRINTFILE:
         uiError = 2014;
         set_value = &pSet->ZH_SET_PRINTFILE;
         handle_ptr = &pSet->zh_set_printhan;
         def_ext = ".prn";
         break;
      case ZH_SET_EXTRAFILE:
         uiError = 2015;
         set_value = &pSet->ZH_SET_EXTRAFILE;
         handle_ptr = &pSet->zh_set_extrahan;
         def_ext = ".prn";
         break;
      default:
         return;
   }

   if( file_name && file_name[ 0 ] != '\0' )
   {
#if defined( ZH_OS_UNIX )
      fPipe = file_name[ 0 ] == '|';
      if( fPipe )
         szFileName = zh_strdup( file_name );
      else
#endif
      {
         szDevice = is_devicename( file_name );
         if( szDevice )
         {
            szFileName = zh_strdup( szDevice );
            def_ext = NULL;
#if defined( ZH_OS_WIN ) || defined( ZH_OS_DOS )
            fAppend = ZH_TRUE;
#endif
         }
         else
            szFileName = zh_strdup( file_name );
      }
   }

   /* free the old value before setting the new one (CA-Cl*pper does it).
    * This code must be executed after setting szFileName, [druzus]
    */
   close_handle( pSet, set_specifier );
   if( *set_value )
   {
      zh_xfree( *set_value );
      *set_value = NULL;
   }

   if( ! szFileName )
      return;

   fStripEof = fAppend && szDevice == NULL && ! fPipe;

   do
   {
      if( fPipe )
         handle = zh_filePOpen( szFileName + 1, "w" );
      else
         handle = zh_fileExtOpen( szFileName,
                                  zh_stackSetStruct()->ZH_SET_DEFEXTENSIONS ? def_ext : NULL,
                                  ( ! fStripEof || set_specifier == ZH_SET_PRINTFILE ? FO_WRITE : FO_READWRITE ) |
                                  FO_DENYWRITE | FXO_SHARELOCK |
                                  ( fAppend ? FXO_APPEND : FXO_TRUNCATE ) |
                                  ( szDevice ? 0 : FXO_DEFAULTS ),
                                  NULL, pError );

      if( handle == NULL )
      {
         pError = zh_errRT_FileError( pError, ZH_ERR_SS_TERMINAL, EG_CREATE, uiError, szFileName );
         if( zh_errLaunch( pError ) != E_RETRY )
            break;
      }
   }
   while( handle == NULL );

   if( pError )
      zh_itemRelease( pError );

   if( handle != NULL && fStripEof )
   {
      /* Position to EOF */
      if( zh_fileSeek( handle, 0, FS_END ) > 0 )
      {
         /* Special binary vs. text file handling - even for UN*X, now
            that there's an ZH_SET_EOF flag. */

         /* PRINTFILE is always binary and needs no special handling. */
         if( set_specifier != ZH_SET_PRINTFILE )
         {
            /* All other files are text files and may have an EOF
               ('\x1A') character at the end (both UN*X and non-UN*X,
               now that theres an ZH_SET_EOF flag). */
            char cEOF = '\0';
            zh_fileSeek( handle, -1, FS_END );     /* Position to last char. */
            zh_fileRead( handle, &cEOF, 1, -1 );   /* Read the last char. */
            if( cEOF == '\x1A' )                   /* If it's an EOF, */
               zh_fileSeek( handle, -1, FS_END );  /* Then write over it. */
         }
      }
   }

   /* user RT error handler can open it too so we have to
    * close it again if necessary
    */
   if( handle == NULL )
   {
      zh_xfree( szFileName );
      szFileName = NULL;
   }

   close_handle( pSet, set_specifier );
   *handle_ptr = handle;
   if( *set_value )
      zh_xfree( *set_value );
   *set_value = szFileName;
}

int zh_setUpdateEpoch( int iYear )
{
   if( iYear >= 0 && iYear < 100 )
   {
      int iEpoch = zh_setGetEpoch();
      int iCentury = iEpoch / 100;

      if( iYear < iEpoch % 100 )
         ++iCentury;
      iYear += iCentury * 100;
   }
   return iYear;
}

ZH_BOOL zh_setSetCentury( ZH_BOOL new_century_setting )
{
   ZH_STACK_TLS_PRELOAD
   PZH_SET_STRUCT pSet = zh_stackSetStruct();
   ZH_BOOL old_century_setting = pSet->zh_set_century;

   pSet->zh_set_century = new_century_setting;
   /*
    * if the setting changed, adjust the current date format to use
    * the correct number of year digits.
    */
   if( old_century_setting != new_century_setting )
   {
      int count, size, y_size, y_start, y_stop;
      char * szDateFormat, * szNewFormat;

      /* Convert to upper case and determine where year is */
      y_start = y_stop = -1;
      szDateFormat = pSet->ZH_SET_DATEFORMAT;
      size = ( int ) strlen( szDateFormat );
      for( count = 0; count < size; count++ )
      {
         int digit = ZH_TOUPPER( ( ZH_UCHAR ) szDateFormat[ count ] );
         if( digit == 'Y' )
         {
            if( y_start == -1 )
               y_start = count;
         }
         else if( y_start > -1 && y_stop == -1 )
            y_stop = count;
         szDateFormat[ count ] = ( char ) digit;
      }
      /* Determine size of year in current format */
      if( y_start < 0 )
      {
         y_start = 0; /* There is no year in the current format */
         y_stop = 0;
      }
      else if( y_stop < 0 )
         y_stop = size;  /* All digits are year digits */
      y_size = y_stop - y_start;
      /* Calculate size of new format */
      size -= y_size;
      if( new_century_setting )
         size += 4;
      else
         size += 2;

      /* Create the new date format */
      szNewFormat = ( char * ) zh_xgrab( size + 1 );

      {
         int format_len;
         if( y_start > 0 )
            memcpy( szNewFormat, szDateFormat, y_start );
         szNewFormat[ y_start ] = '\0';
         zh_strncat( szNewFormat, "YY", size );
         if( new_century_setting )
            zh_strncat( szNewFormat, "YY", size );
         format_len = ( int ) strlen( szDateFormat );
         if( y_stop < format_len )
            zh_strncat( szNewFormat, szDateFormat + y_stop, size );
         /* DATE FORMAT is under direct control of SET, so notify when it
            it is changed indirectly via __SetCentury() */
         zh_setListenerNotify( ZH_SET_DATEFORMAT, ZH_SET_LISTENER_BEFORE );
         zh_xfree( szDateFormat );
         pSet->ZH_SET_DATEFORMAT = szNewFormat;
         zh_setListenerNotify( ZH_SET_DATEFORMAT, ZH_SET_LISTENER_AFTER );
      }
   }

   /* Return the previous setting */
   return old_century_setting;
}

ZH_FUNC( __SETCENTURY )
{
   ZH_STACK_TLS_PRELOAD
   ZH_BOOL old_century_setting = zh_setGetCentury();
   PZH_ITEM pNewVal = zh_param( 1, ZH_IT_ANY );

   if( pNewVal )
      zh_setSetCentury( set_logical( pNewVal, old_century_setting ) );

   zh_retl( old_century_setting );
}

ZH_FUNC( SETCANCEL )
{
   ZH_STACK_TLS_PRELOAD
   zh_retl( zh_setGetCancel() );
   /* SetCancel() accepts only logical parameters */
   zh_setSetItem( ZH_SET_CANCEL, zh_param( 1, ZH_IT_LOGICAL ) );
}

/* return default printer device */
static char * zh_set_PRINTFILE_default( void )
{
#if defined( ZH_OS_UNIX )
   return zh_strdup( "|lpr" );
#elif defined( ZH_OS_WIN )
   return zh_strdup( "LPT1" );
#else
   return zh_strdup( "PRN" ); /* FIXME */
#endif
}

PZH_ITEM zh_setGetItem( ZH_set_enum set_specifier, PZH_ITEM pResult,
                        PZH_ITEM pArg1, PZH_ITEM pArg2 )
{
   ZH_STACK_TLS_PRELOAD
   PZH_SET_STRUCT pSet = zh_stackSetStruct();

   if( pArg1 != NULL )
      zh_setListenerNotify( set_specifier, ZH_SET_LISTENER_BEFORE );

   switch( set_specifier )
   {
      case ZH_SET_ALTERNATE:
         pResult = zh_itemPutL( pResult, pSet->ZH_SET_ALTERNATE );
         if( pArg1 != NULL )
            pSet->ZH_SET_ALTERNATE = set_logical( pArg1, pSet->ZH_SET_ALTERNATE );
         break;
      case ZH_SET_ALTFILE:
         pResult = zh_itemPutC( pResult, pSet->ZH_SET_ALTFILE );
         if( pArg1 && ZH_IS_STRING( pArg1 ) )
            open_handle( pSet, zh_itemGetCPtr( pArg1 ), set_logical( pArg2, ZH_FALSE ), ZH_SET_ALTFILE );
         break;
      case ZH_SET_AUTOPEN:
         pResult = zh_itemPutL( pResult, pSet->ZH_SET_AUTOPEN );
         if( pArg1 != NULL )
            pSet->ZH_SET_AUTOPEN = set_logical( pArg1, pSet->ZH_SET_AUTOPEN );
         break;
      case ZH_SET_AUTORDER:
         pResult = zh_itemPutNI( pResult, pSet->ZH_SET_AUTORDER );
         if( pArg1 != NULL )
         {
            if( set_number( pArg1, pSet->ZH_SET_AUTORDER ) < 0 )
               zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
            else
               pSet->ZH_SET_AUTORDER = set_number( pArg1, pSet->ZH_SET_AUTORDER );
         }
         break;
      case ZH_SET_AUTOSHARE:
         pResult = zh_itemPutNI( pResult, pSet->ZH_SET_AUTOSHARE );
         if( pArg1 != NULL )
         {
            if( set_number( pArg1, pSet->ZH_SET_AUTOSHARE ) < 0 )
               zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
            else
               pSet->ZH_SET_AUTOSHARE = set_number( pArg1, pSet->ZH_SET_AUTOSHARE );
         }
         break;
      case ZH_SET_BELL:
         pResult = zh_itemPutL( pResult, pSet->ZH_SET_BELL );
         if( pArg1 != NULL )
            pSet->ZH_SET_BELL = set_logical( pArg1, pSet->ZH_SET_BELL );
         break;
      case ZH_SET_CANCEL:
         pResult = zh_itemPutL( pResult, pSet->ZH_SET_CANCEL );
         if( pArg1 != NULL )
            pSet->ZH_SET_CANCEL = set_logical( pArg1, pSet->ZH_SET_CANCEL );
         break;
      case ZH_SET_COLOR:
         pResult = zh_itemPutC( pResult, zh_conSetColor( pArg1 != NULL && ZH_IS_STRING( pArg1 ) ? zh_itemGetCPtr( pArg1 ) : NULL ) );
         break;
      case ZH_SET_CONFIRM:
         pResult = zh_itemPutL( pResult, pSet->ZH_SET_CONFIRM );
         if( pArg1 != NULL )
            pSet->ZH_SET_CONFIRM = set_logical( pArg1, pSet->ZH_SET_CONFIRM );
         break;
      case ZH_SET_CONSOLE:
         pResult = zh_itemPutL( pResult, pSet->ZH_SET_CONSOLE );
         if( pArg1 != NULL )
            pSet->ZH_SET_CONSOLE = set_logical( pArg1, pSet->ZH_SET_CONSOLE );
         break;
      case ZH_SET_CURSOR:
         if( pArg1 != NULL && ZH_IS_NUMERIC( pArg1 ) )
            pResult = zh_itemPutNI( pResult, zh_conSetCursor( ZH_TRUE, zh_itemGetNI( pArg1 ) ) );
         else
            pResult = zh_itemPutNI( pResult, zh_conSetCursor( ZH_FALSE, 0 ) );
         break;
      case ZH_SET_DATEFORMAT:
         pResult = zh_itemPutC( pResult, pSet->ZH_SET_DATEFORMAT );
         if( pArg1 != NULL )
         {
            char * value;
            int year = 0;

            value = pSet->ZH_SET_DATEFORMAT = set_string( pArg1, pSet->ZH_SET_DATEFORMAT );
            while( *value )
            {
               if( *value == 'Y' || *value == 'y' )
                  year++;
               else if( year )   /* Only count the first set of consecutive "Y"s. */
                  break;
               ++value;
            }
            /* CENTURY is not controlled directly by SET, so there is no
               notification for changing it indirectly via DATE FORMAT. */
            pSet->zh_set_century = year >= 4;
         }
         break;
      case ZH_SET_TIMEFORMAT:
         pResult = zh_itemPutC( pResult, pSet->ZH_SET_TIMEFORMAT );
         if( pArg1 != NULL )
            pSet->ZH_SET_TIMEFORMAT = set_string( pArg1, pSet->ZH_SET_TIMEFORMAT );
         break;
      case ZH_SET_DEBUG:
         pResult = zh_itemPutL( pResult, pSet->ZH_SET_DEBUG );
         if( pArg1 != NULL )
            pSet->ZH_SET_DEBUG = set_logical( pArg1, pSet->ZH_SET_DEBUG );
         break;
      case ZH_SET_DECIMALS:
         pResult = zh_itemPutNI( pResult, pSet->ZH_SET_DECIMALS );
         if( pArg1 != NULL )
         {
            if( set_number( pArg1, pSet->ZH_SET_DECIMALS ) < 0 )
               zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
            else
               pSet->ZH_SET_DECIMALS = set_number( pArg1, pSet->ZH_SET_DECIMALS );
         }
         break;
      case ZH_SET_DEFAULT:
         pResult = zh_itemPutC( pResult, pSet->ZH_SET_DEFAULT );
         if( pArg1 != NULL )
            pSet->ZH_SET_DEFAULT = set_string( pArg1, pSet->ZH_SET_DEFAULT );
         break;
      case ZH_SET_DELETED:
         pResult = zh_itemPutL( pResult, pSet->ZH_SET_DELETED );
         if( pArg1 != NULL )
            pSet->ZH_SET_DELETED = set_logical( pArg1, pSet->ZH_SET_DELETED );
         break;
      case ZH_SET_DELIMCHARS:
         pResult = zh_itemPutC( pResult, pSet->ZH_SET_DELIMCHARS );
         if( pArg1 != NULL )
            pSet->ZH_SET_DELIMCHARS = set_string( pArg1, pSet->ZH_SET_DELIMCHARS );
         break;
      case ZH_SET_DELIMITERS:
         pResult = zh_itemPutL( pResult, pSet->ZH_SET_DELIMITERS );
         if( pArg1 != NULL )
            pSet->ZH_SET_DELIMITERS = set_logical( pArg1, pSet->ZH_SET_DELIMITERS );
         break;
      case ZH_SET_DEVICE:
         pResult = zh_itemPutC( pResult, pSet->ZH_SET_DEVICE );
         if( pArg1 && ZH_IS_STRING( pArg1 ) )
         {
            /* If the print file is not already open, open it in overwrite mode. */
            pSet->ZH_SET_DEVICE = set_string( pArg1, pSet->ZH_SET_DEVICE );
            pSet->zh_set_prndevice = strlen( pSet->ZH_SET_DEVICE ) >= 4 &&
                                     zh_strnicmp( pSet->ZH_SET_DEVICE, "PRIN", 4 ) == 0;
         }
         break;
      case ZH_SET_EOF:
         pResult = zh_itemPutL( pResult, pSet->ZH_SET_EOF );
         if( pArg1 != NULL )
            pSet->ZH_SET_EOF = set_logical( pArg1, pSet->ZH_SET_EOF );
         break;
      case ZH_SET_EPOCH:
         pResult = zh_itemPutNI( pResult, pSet->ZH_SET_EPOCH );
         if( pArg1 != NULL )
         {
            if( set_number( pArg1, pSet->ZH_SET_EPOCH ) < 0 )
               zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
            else
               pSet->ZH_SET_EPOCH = set_number( pArg1, pSet->ZH_SET_EPOCH );
         }
         break;
      case ZH_SET_ESCAPE:
         pResult = zh_itemPutL( pResult, pSet->ZH_SET_ESCAPE );
         if( pArg1 != NULL )
            pSet->ZH_SET_ESCAPE = set_logical( pArg1, pSet->ZH_SET_ESCAPE );
         break;
      case ZH_SET_EVENTMASK:
         pResult = zh_itemPutNI( pResult, pSet->ZH_SET_EVENTMASK );
         if( pArg1 != NULL )
         {
            if( set_number( pArg1, pSet->ZH_SET_EVENTMASK ) < 0 )
               zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
            else
               pSet->ZH_SET_EVENTMASK = set_number( pArg1, pSet->ZH_SET_EVENTMASK );
         }
         break;
      case ZH_SET_EXACT:
         pResult = zh_itemPutL( pResult, pSet->ZH_SET_EXACT );
         if( pArg1 != NULL )
            pSet->ZH_SET_EXACT = set_logical( pArg1, pSet->ZH_SET_EXACT );
         break;
      case ZH_SET_EXCLUSIVE:
         pResult = zh_itemPutL( pResult, pSet->ZH_SET_EXCLUSIVE );
         if( pArg1 != NULL )
            pSet->ZH_SET_EXCLUSIVE = set_logical( pArg1, pSet->ZH_SET_EXCLUSIVE );
         break;
      case ZH_SET_EXIT:
         pResult = zh_itemPutL( pResult, pSet->ZH_SET_EXIT );
         /* NOTE: Otherwise ReadExit() will always set the value. [vszakats] */
         if( pArg1 != NULL && ! ZH_IS_NIL( pArg1 ) )
            pSet->ZH_SET_EXIT = set_logical( pArg1, pSet->ZH_SET_EXIT );
         break;
      case ZH_SET_EXTRA:
         pResult = zh_itemPutL( pResult, pSet->ZH_SET_EXTRA );
         if( pArg1 != NULL )
            pSet->ZH_SET_EXTRA = set_logical( pArg1, pSet->ZH_SET_EXTRA );
         break;
      case ZH_SET_EXTRAFILE:
         pResult = zh_itemPutC( pResult, pSet->ZH_SET_EXTRAFILE );
         if( pArg1 && ZH_IS_STRING( pArg1 ) )
            open_handle( pSet, zh_itemGetCPtr( pArg1 ), set_logical( pArg2, ZH_FALSE ), ZH_SET_EXTRAFILE );
         break;
      case ZH_SET_FIXED:
         pResult = zh_itemPutL( pResult, pSet->ZH_SET_FIXED );
         if( pArg1 != NULL )
            pSet->ZH_SET_FIXED = set_logical( pArg1, pSet->ZH_SET_FIXED );
         break;
      case ZH_SET_INSERT:
         pResult = zh_itemPutL( pResult, pSet->ZH_SET_INSERT );
         if( pArg1 != NULL )
            pSet->ZH_SET_INSERT = set_logical( pArg1, pSet->ZH_SET_INSERT );
         break;
      case ZH_SET_INTENSITY:
         pResult = zh_itemPutL( pResult, pSet->ZH_SET_INTENSITY );
         if( pArg1 != NULL )
            pSet->ZH_SET_INTENSITY = set_logical( pArg1, pSet->ZH_SET_INTENSITY );
         break;
      case ZH_SET_MARGIN:
         pResult = zh_itemPutNI( pResult, pSet->ZH_SET_MARGIN );
         if( pArg1 != NULL )
         {
            if( set_number( pArg1, pSet->ZH_SET_MARGIN ) < 0 )
               zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
            else
               pSet->ZH_SET_MARGIN = set_number( pArg1, pSet->ZH_SET_MARGIN );
         }
         break;
      case ZH_SET_MBLOCKSIZE:
         pResult = zh_itemPutNI( pResult, pSet->ZH_SET_MBLOCKSIZE );
         if( pArg1 != NULL )
         {
            if( set_number( pArg1, pSet->ZH_SET_MBLOCKSIZE ) < 0 )
               zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
            else
               pSet->ZH_SET_MBLOCKSIZE = set_number( pArg1, pSet->ZH_SET_MBLOCKSIZE );
         }
         break;
      case ZH_SET_MCENTER:
         pResult = zh_itemPutL( pResult, pSet->ZH_SET_MCENTER );
         if( pArg1 != NULL )
            pSet->ZH_SET_MCENTER = set_logical( pArg1, pSet->ZH_SET_MCENTER );
         break;
      case ZH_SET_MESSAGE:
         pResult = zh_itemPutNI( pResult, pSet->ZH_SET_MESSAGE );
         if( pArg1 != NULL )
         {
            if( set_number( pArg1, pSet->ZH_SET_MESSAGE ) < 0 )
               zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
            else
               pSet->ZH_SET_MESSAGE = set_number( pArg1, pSet->ZH_SET_MESSAGE );
         }
         break;
      case ZH_SET_MFILEEXT:
         pResult = zh_itemPutC( pResult, pSet->ZH_SET_MFILEEXT );
         if( pArg1 != NULL )
            pSet->ZH_SET_MFILEEXT = set_string( pArg1, pSet->ZH_SET_MFILEEXT );
         break;
      case ZH_SET_OPTIMIZE:
         pResult = zh_itemPutL( pResult, pSet->ZH_SET_OPTIMIZE );
         if( pArg1 != NULL )
            pSet->ZH_SET_OPTIMIZE = set_logical( pArg1, pSet->ZH_SET_OPTIMIZE );
         break;
      case ZH_SET_FORCEOPT:
         pResult = zh_itemPutL( pResult, pSet->ZH_SET_FORCEOPT );
         if( pArg1 != NULL )
            pSet->ZH_SET_FORCEOPT = set_logical( pArg1, pSet->ZH_SET_FORCEOPT );
         break;
      case ZH_SET_STRICTREAD:
         pResult = zh_itemPutL( pResult, pSet->ZH_SET_STRICTREAD );
         if( pArg1 != NULL )
            pSet->ZH_SET_STRICTREAD = set_logical( pArg1, pSet->ZH_SET_STRICTREAD );
         break;
      case ZH_SET_HARDCOMMIT:
         pResult = zh_itemPutL( pResult, pSet->ZH_SET_HARDCOMMIT );
         if( pArg1 != NULL )
            pSet->ZH_SET_HARDCOMMIT = set_logical( pArg1, pSet->ZH_SET_HARDCOMMIT );
         break;
      case ZH_SET_PATH:
         pResult = zh_itemPutC( pResult, pSet->ZH_SET_PATH );
         if( pArg1 != NULL )
         {
            pSet->ZH_SET_PATH = set_string( pArg1, pSet->ZH_SET_PATH );
            zh_fsFreeSearchPath( pSet->zh_set_path );
            pSet->zh_set_path = NULL;
            zh_fsAddSearchPath( pSet->ZH_SET_PATH, &pSet->zh_set_path );
         }
         break;
      case ZH_SET_PRINTER:
         pResult = zh_itemPutL( pResult, pSet->ZH_SET_PRINTER );
         if( pArg1 != NULL )
            pSet->ZH_SET_PRINTER = set_logical( pArg1, pSet->ZH_SET_PRINTER );
         break;
      case ZH_SET_PRINTFILE:
         pResult = zh_itemPutC( pResult, pSet->ZH_SET_PRINTFILE );
         if( pArg1 && ZH_IS_STRING( pArg1 ) )
         {
            open_handle( pSet, zh_itemGetCPtr( pArg1 ), set_logical( pArg2, ZH_FALSE ), ZH_SET_PRINTFILE );
            /* With SET PRINTER TO or Set( _SET_PRINTFILE, "" ) are expected to activate the default printer [jarabal] */
            if( pSet->ZH_SET_PRINTFILE == NULL )
               pSet->ZH_SET_PRINTFILE = zh_set_PRINTFILE_default();
         }
         break;
      case ZH_SET_SCOREBOARD:
         pResult = zh_itemPutL( pResult, pSet->ZH_SET_SCOREBOARD );
         if( pArg1 != NULL )
            pSet->ZH_SET_SCOREBOARD = set_logical( pArg1, pSet->ZH_SET_SCOREBOARD );
         break;
      case ZH_SET_SCROLLBREAK:
         pResult = zh_itemPutL( pResult, pSet->ZH_SET_SCROLLBREAK );
         if( pArg1 != NULL )
            pSet->ZH_SET_SCROLLBREAK = set_logical( pArg1, pSet->ZH_SET_SCROLLBREAK );
         break;
      case ZH_SET_SOFTSEEK:
         pResult = zh_itemPutL( pResult, pSet->ZH_SET_SOFTSEEK );
         if( pArg1 != NULL )
            pSet->ZH_SET_SOFTSEEK = set_logical( pArg1, pSet->ZH_SET_SOFTSEEK );
         break;
      case ZH_SET_TYPEAHEAD:
         pResult = zh_itemPutNI( pResult, pSet->ZH_SET_TYPEAHEAD );
         if( pArg1 != NULL )
         {
            /* Set the value and limit the range */
            pSet->ZH_SET_TYPEAHEAD = set_number( pArg1, pSet->ZH_SET_TYPEAHEAD );
            if( pSet->ZH_SET_TYPEAHEAD == 0 )
               /* Do nothing */;
            else if( pSet->ZH_SET_TYPEAHEAD < 16 )
               pSet->ZH_SET_TYPEAHEAD = 16;
            else if( pSet->ZH_SET_TYPEAHEAD > 4096 )
               pSet->ZH_SET_TYPEAHEAD = 4096;
            /* reset keyboard buffer */
            zh_inkeyReset();
         }
         break;
      case ZH_SET_UNIQUE:
         pResult = zh_itemPutL( pResult, pSet->ZH_SET_UNIQUE );
         if( pArg1 != NULL )
            pSet->ZH_SET_UNIQUE = set_logical( pArg1, pSet->ZH_SET_UNIQUE );
         break;
      case ZH_SET_VIDEOMODE:
         pResult = zh_itemPutNI( pResult, pSet->ZH_SET_VIDEOMODE );
         if( pArg1 != NULL )
            pSet->ZH_SET_VIDEOMODE = set_number( pArg1, pSet->ZH_SET_VIDEOMODE );
         break;
      case ZH_SET_WRAP:
         pResult = zh_itemPutL( pResult, pSet->ZH_SET_WRAP );
         if( pArg1 != NULL )
            pSet->ZH_SET_WRAP = set_logical( pArg1, pSet->ZH_SET_WRAP );
         break;
      case ZH_SET_LANGUAGE:
         pResult = zh_itemPutC( pResult, zh_langID() );
         if( pArg1 != NULL )
         {
            if( ZH_IS_STRING( pArg1 ) )
               zh_langSelectID( zh_itemGetCPtr( pArg1 ) );
            else
               zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
         }
         break;
      case ZH_SET_CODEPAGE:
         pResult = zh_itemPutC( pResult, zh_cdpID() );
         if( pArg1 != NULL )
         {
            if( ZH_IS_STRING( pArg1 ) )
               zh_cdpSelectID( zh_itemGetCPtr( pArg1 ) );
            else
               zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
         }
         break;
      case ZH_SET_IDLEREPEAT:
         pResult = zh_itemPutL( pResult, pSet->ZH_SET_IDLEREPEAT );
         if( pArg1 != NULL )
            pSet->ZH_SET_IDLEREPEAT = set_logical( pArg1, pSet->ZH_SET_IDLEREPEAT );
         break;
      case ZH_SET_FILECASE:
         pResult = zh_itemPutNI( pResult, pSet->ZH_SET_FILECASE );
         if( pArg1 != NULL )
         {
            if( ZH_IS_STRING( pArg1 ) )
            {
               if( ! zh_stricmp( zh_itemGetCPtr( pArg1 ), "LOWER" ) )
                  pSet->ZH_SET_FILECASE = ZH_SET_CASE_LOWER;
               else if( ! zh_stricmp( zh_itemGetCPtr( pArg1 ), "UPPER" ) )
                  pSet->ZH_SET_FILECASE = ZH_SET_CASE_UPPER;
               else if( ! zh_stricmp( zh_itemGetCPtr( pArg1 ), "MIXED" ) )
                  pSet->ZH_SET_FILECASE = ZH_SET_CASE_MIXED;
               else
                  zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
            }
            else if( ZH_IS_NUMERIC( pArg1 ) )
            {
               int iValue = set_number( pArg1, pSet->ZH_SET_FILECASE );
               if( iValue == ZH_SET_CASE_LOWER ||
                   iValue == ZH_SET_CASE_UPPER ||
                   iValue == ZH_SET_CASE_MIXED )
                  pSet->ZH_SET_FILECASE = iValue;
               else
                  zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
            }
            else
               zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
         }
         break;
      case ZH_SET_DIRCASE:
         pResult = zh_itemPutNI( pResult, pSet->ZH_SET_DIRCASE );
         if( pArg1 != NULL )
         {
            if( ZH_IS_STRING( pArg1 ) )
            {
               if( ! zh_stricmp( zh_itemGetCPtr( pArg1 ), "LOWER" ) )
                  pSet->ZH_SET_DIRCASE = ZH_SET_CASE_LOWER;
               else if( ! zh_stricmp( zh_itemGetCPtr( pArg1 ), "UPPER" ) )
                  pSet->ZH_SET_DIRCASE = ZH_SET_CASE_UPPER;
               else if( ! zh_stricmp( zh_itemGetCPtr( pArg1 ), "MIXED" ) )
                  pSet->ZH_SET_DIRCASE = ZH_SET_CASE_MIXED;
               else
                  zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
            }
            else if( ZH_IS_NUMERIC( pArg1 ) )
            {
               int iValue = set_number( pArg1, pSet->ZH_SET_DIRCASE );
               if( iValue == ZH_SET_CASE_LOWER ||
                   iValue == ZH_SET_CASE_UPPER ||
                   iValue == ZH_SET_CASE_MIXED )
                  pSet->ZH_SET_DIRCASE = iValue;
               else
                  zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
            }
            else
               zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
         }
         break;
      case ZH_SET_DIRSEPARATOR:
      {
         char szDirSep[ 2 ];
         szDirSep[ 0 ] = ( char ) pSet->ZH_SET_DIRSEPARATOR;
         szDirSep[ 1 ] = '\0';
         pResult = zh_itemPutC( pResult, szDirSep );
         if( pArg1 != NULL )
            pSet->ZH_SET_DIRSEPARATOR = set_char( pArg1, ( char ) pSet->ZH_SET_DIRSEPARATOR );
         break;
      }
      case ZH_SET_DBFLOCKSCHEME:
         pResult = zh_itemPutNI( pResult, pSet->ZH_SET_DBFLOCKSCHEME );
         if( pArg1 != NULL )
         {
            if( set_number( pArg1, pSet->ZH_SET_DBFLOCKSCHEME ) < 0 )
               zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
            else
               pSet->ZH_SET_DBFLOCKSCHEME = set_number( pArg1, pSet->ZH_SET_DBFLOCKSCHEME );
         }
         break;
      case ZH_SET_DEFEXTENSIONS:
         pResult = zh_itemPutL( pResult, pSet->ZH_SET_DEFEXTENSIONS );
         if( pArg1 != NULL )
            pSet->ZH_SET_DEFEXTENSIONS = set_logical( pArg1, pSet->ZH_SET_DEFEXTENSIONS );
         break;
      case ZH_SET_EOL:
         pResult = zh_itemPutC( pResult, pSet->ZH_SET_EOL );
         if( pArg1 != NULL )
            pSet->ZH_SET_EOL = set_string( pArg1, pSet->ZH_SET_EOL );
         break;
      case ZH_SET_TRIMFILENAME:
         pResult = zh_itemPutL( pResult, pSet->ZH_SET_TRIMFILENAME );
         if( pArg1 != NULL )
            pSet->ZH_SET_TRIMFILENAME = set_logical( pArg1, pSet->ZH_SET_TRIMFILENAME );
         break;
      case ZH_SET_HBOUTLOG:
         pResult = zh_itemPutC( pResult, pSet->ZH_SET_HBOUTLOG );
         if( pArg1 != NULL && ( ZH_IS_STRING( pArg1 ) || ZH_IS_NIL( pArg1 ) ) )
         {
            if( pSet->ZH_SET_HBOUTLOG )
               zh_xfree( pSet->ZH_SET_HBOUTLOG );
            if( ZH_IS_NIL( pArg1 ) )
               pSet->ZH_SET_HBOUTLOG = NULL;
            else
               /* Limit size of SET strings to 64 KiB, truncating if source is longer */
               pSet->ZH_SET_HBOUTLOG = zh_strndup( zh_itemGetCPtr( pArg1 ), USHRT_MAX );
            zh_xsetfilename( pSet->ZH_SET_HBOUTLOG );
         }
         break;
      case ZH_SET_HBOUTLOGINFO:
         pResult = zh_itemPutC( pResult, pSet->ZH_SET_HBOUTLOGINFO );
         if( pArg1 != NULL )
         {
            pSet->ZH_SET_HBOUTLOGINFO = set_string( pArg1, pSet->ZH_SET_HBOUTLOGINFO );
            zh_xsetinfo( pSet->ZH_SET_HBOUTLOGINFO );
         }
         break;
      case ZH_SET_OSCODEPAGE:
         if( pSet->zh_set_oscp )
            pResult = zh_itemPutC( pResult, ( ( PZH_CODEPAGE ) pSet->zh_set_oscp )->id );
         else if( pResult )
            zh_itemClear( pResult );
         else
            pResult = zh_itemNew( NULL );
         if( pArg1 != NULL )
         {
            if( ZH_IS_NIL( pArg1 ) )
               pSet->zh_set_oscp = NULL;
            else if( ZH_IS_STRING( pArg1 ) )
            {
               PZH_CODEPAGE cdp = zh_cdpFindExt( zh_itemGetCPtr( pArg1 ) );
               if( cdp )
                  pSet->zh_set_oscp = ( void * ) cdp;
            }
            else
               zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
         }
         break;
      case ZH_SET_DBCODEPAGE:
         if( pSet->zh_set_dbcp )
            pResult = zh_itemPutC( pResult, ( ( PZH_CODEPAGE ) pSet->zh_set_dbcp )->id );
         else if( pResult )
            zh_itemClear( pResult );
         else
            pResult = zh_itemNew( NULL );
         if( pArg1 != NULL )
         {
            if( ZH_IS_NIL( pArg1 ) )
               pSet->zh_set_dbcp = NULL;
            else if( ZH_IS_STRING( pArg1 ) )
            {
               PZH_CODEPAGE cdp = zh_cdpFindExt( zh_itemGetCPtr( pArg1 ) );
               if( cdp )
                  pSet->zh_set_dbcp = ( void * ) cdp;
            }
            else
               zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
         }
         break;

      case ZH_SET_INVALID_:
         /* Return NIL if called with invalid SET specifier */
         break;

#if 0
      /*
       * intentionally removed default: clause to enable C compiler warning
       * when not all ZH_SET_* cases are implemented. [druzus]
       */
      default:
         break;
#endif
   }
   if( pArg1 != NULL )
      zh_setListenerNotify( set_specifier, ZH_SET_LISTENER_AFTER );

   return pResult;
}

ZH_FUNC( SET )
{
   ZH_STACK_TLS_PRELOAD
   zh_setGetItem( ( ZH_set_enum ) zh_parnidef( 1, ZH_SET_INVALID_ ),
                  zh_stackReturnItem(),
                  zh_param( 2, ZH_IT_ANY ), zh_param( 3, ZH_IT_ANY ) );
}

void zh_setInitialize( PZH_SET_STRUCT pSet )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_setInitialize(%p)", ( void * ) pSet ) );

   pSet->ZH_SET_ALTERNATE = ZH_FALSE;
   pSet->ZH_SET_ALTFILE = NULL;
   pSet->zh_set_althan = NULL;
   pSet->ZH_SET_AUTOPEN = ZH_TRUE;
   pSet->ZH_SET_AUTORDER = 0;
   pSet->ZH_SET_AUTOSHARE = 0;
   pSet->ZH_SET_BELL = ZH_FALSE;
   pSet->ZH_SET_CANCEL = ZH_TRUE;
   pSet->zh_set_century = ZH_FALSE;
   pSet->zh_set_prndevice = ZH_FALSE;
   pSet->ZH_SET_COLOR = ( char * ) zh_xgrab( ZH_CLRSTR_LEN + 1 );
   /* NOTE: color must be synced with the one in IsDefColor() function */
   zh_strncpy( pSet->ZH_SET_COLOR, "W/N,N/W,N/N,N/N,N/W", ZH_CLRSTR_LEN );
   pSet->ZH_SET_CONFIRM = ZH_FALSE;
   pSet->ZH_SET_CONSOLE = ZH_TRUE;
   pSet->ZH_SET_DATEFORMAT = zh_strdup( "mm/dd/yy" );
   pSet->ZH_SET_TIMEFORMAT = zh_strdup( "hh:mm:ss.fff" );
   pSet->ZH_SET_DEBUG = zh_dynsymFind( "__DBGENTRY" ) ? ZH_TRUE : ZH_FALSE;
   pSet->ZH_SET_DECIMALS = 2;
   pSet->ZH_SET_DEFAULT = zh_strdup( "" );
   pSet->ZH_SET_DELETED = ZH_FALSE;
   pSet->ZH_SET_DELIMCHARS = zh_strdup( "::" );
   pSet->ZH_SET_DELIMITERS = ZH_FALSE;
   pSet->ZH_SET_DEVICE = zh_strdup( "SCREEN" );
   pSet->ZH_SET_EOF = ZH_TRUE;
   pSet->ZH_SET_EPOCH = 1900;
   pSet->ZH_SET_ESCAPE = ZH_TRUE;
   pSet->ZH_SET_EVENTMASK = INKEY_KEYBOARD;
   pSet->ZH_SET_EXACT = ZH_FALSE;
   pSet->ZH_SET_EXCLUSIVE = ZH_TRUE;
   pSet->ZH_SET_EXIT = ZH_FALSE;
   pSet->ZH_SET_EXTRA = ZH_FALSE;
   pSet->ZH_SET_EXTRAFILE = NULL;
   pSet->zh_set_extrahan = NULL;
   pSet->ZH_SET_FIXED = ZH_FALSE;
   pSet->ZH_SET_FORCEOPT = ZH_FALSE;
   pSet->ZH_SET_HARDCOMMIT = ZH_TRUE;
   pSet->ZH_SET_IDLEREPEAT = ZH_TRUE;
   pSet->ZH_SET_INSERT = ZH_FALSE;
   pSet->ZH_SET_INTENSITY = ZH_TRUE;
   pSet->ZH_SET_MARGIN = 0;
   pSet->ZH_SET_MBLOCKSIZE = 64;
   pSet->ZH_SET_MCENTER = ZH_FALSE;
   pSet->ZH_SET_MESSAGE = 0;
   pSet->ZH_SET_MFILEEXT = zh_strdup( "" );
   pSet->ZH_SET_OPTIMIZE = ZH_TRUE;
   pSet->ZH_SET_PATH = zh_strdup( "" );
   pSet->zh_set_path = NULL;
   pSet->ZH_SET_PRINTER = ZH_FALSE;
   pSet->ZH_SET_PRINTFILE = zh_set_PRINTFILE_default();
   pSet->zh_set_printhan = NULL;
   pSet->ZH_SET_SCOREBOARD = ZH_TRUE;
   pSet->ZH_SET_SCROLLBREAK = ZH_TRUE;
   pSet->ZH_SET_SOFTSEEK = ZH_FALSE;
   pSet->ZH_SET_STRICTREAD = ZH_FALSE;
   pSet->ZH_SET_TYPEAHEAD = ZH_DEFAULT_INKEY_BUFSIZE;
   pSet->ZH_SET_UNIQUE = ZH_FALSE;
   pSet->ZH_SET_FILECASE = ZH_SET_CASE_MIXED;
   pSet->ZH_SET_DIRCASE = ZH_SET_CASE_MIXED;
   pSet->ZH_SET_DIRSEPARATOR = ZH_OS_PATH_DELIM_CHR;
   pSet->ZH_SET_VIDEOMODE = 0;
   pSet->ZH_SET_WRAP = ZH_FALSE;
   pSet->ZH_SET_DBFLOCKSCHEME = 0;
   pSet->ZH_SET_DEFEXTENSIONS = ZH_TRUE;
   pSet->ZH_SET_EOL = zh_strdup( zh_conNewLine() );
   pSet->ZH_SET_TRIMFILENAME = ZH_FALSE;
   pSet->ZH_SET_HBOUTLOG = zh_strdup( "zh_out.log" );
   pSet->ZH_SET_HBOUTLOGINFO = zh_strdup( "" );

   zh_xsetfilename( pSet->ZH_SET_HBOUTLOG );
   zh_xsetinfo( pSet->ZH_SET_HBOUTLOGINFO );

   pSet->zh_set_oscp = NULL;
   pSet->zh_set_dbcp = NULL;

   pSet->zh_set_listener = NULL;
}

void zh_setRelease( PZH_SET_STRUCT pSet )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_setRelease()" ) );

   close_handle( pSet, ZH_SET_ALTFILE );
   close_handle( pSet, ZH_SET_EXTRAFILE );
   close_handle( pSet, ZH_SET_PRINTFILE );

   if( pSet->ZH_SET_ALTFILE )       zh_xfree( pSet->ZH_SET_ALTFILE );
   if( pSet->ZH_SET_DATEFORMAT )    zh_xfree( pSet->ZH_SET_DATEFORMAT );
   if( pSet->ZH_SET_TIMEFORMAT )    zh_xfree( pSet->ZH_SET_TIMEFORMAT );
   if( pSet->ZH_SET_DEFAULT )       zh_xfree( pSet->ZH_SET_DEFAULT );
   if( pSet->ZH_SET_DELIMCHARS )    zh_xfree( pSet->ZH_SET_DELIMCHARS );
   if( pSet->ZH_SET_DEVICE )        zh_xfree( pSet->ZH_SET_DEVICE );
   if( pSet->ZH_SET_EXTRAFILE )     zh_xfree( pSet->ZH_SET_EXTRAFILE );
   if( pSet->ZH_SET_MFILEEXT  )     zh_xfree( pSet->ZH_SET_MFILEEXT );
   if( pSet->ZH_SET_PATH )          zh_xfree( pSet->ZH_SET_PATH );
   if( pSet->ZH_SET_PRINTFILE )     zh_xfree( pSet->ZH_SET_PRINTFILE );
   if( pSet->ZH_SET_COLOR )         zh_xfree( pSet->ZH_SET_COLOR );
   if( pSet->ZH_SET_EOL )           zh_xfree( pSet->ZH_SET_EOL );
   if( pSet->ZH_SET_HBOUTLOG )      zh_xfree( pSet->ZH_SET_HBOUTLOG );
   if( pSet->ZH_SET_HBOUTLOGINFO )  zh_xfree( pSet->ZH_SET_HBOUTLOGINFO );

   zh_fsFreeSearchPath( pSet->zh_set_path );

   /* Free all set listeners */
   if( pSet->zh_set_listener )
   {
      PZH_SET_LISTENER pListener = ( ( PZH_SET_LISTENER_LST )
                                     pSet->zh_set_listener )->first;
      while( pListener )
      {
         PZH_SET_LISTENER pNext = pListener->next;
         zh_xfree( pListener );
         pListener = pNext;
      }
      zh_xfree( pSet->zh_set_listener );
   }
}

PZH_SET_STRUCT zh_setClone( PZH_SET_STRUCT pSrc )
{
   PZH_SET_STRUCT pSet = ( PZH_SET_STRUCT ) zh_xgrab( sizeof( ZH_SET_STRUCT ) );

   memcpy( pSet, pSrc, sizeof( ZH_SET_STRUCT ) );

   pSet->zh_set_althan = pSet->zh_set_extrahan = pSet->zh_set_printhan = NULL;
   pSet->zh_set_path = NULL;
   pSet->zh_set_listener = NULL;

   pSet->ZH_SET_TYPEAHEAD = ZH_DEFAULT_INKEY_BUFSIZE;

   pSet->ZH_SET_COLOR = ( char * ) zh_xgrab( ZH_CLRSTR_LEN + 1 );
   zh_strncpy( pSet->ZH_SET_COLOR, pSrc->ZH_SET_COLOR, ZH_CLRSTR_LEN );

   if( pSet->ZH_SET_ALTFILE )      pSet->ZH_SET_ALTFILE      = zh_strdup( pSet->ZH_SET_ALTFILE );
   if( pSet->ZH_SET_DATEFORMAT )   pSet->ZH_SET_DATEFORMAT   = zh_strdup( pSet->ZH_SET_DATEFORMAT );
   if( pSet->ZH_SET_TIMEFORMAT )   pSet->ZH_SET_TIMEFORMAT   = zh_strdup( pSet->ZH_SET_TIMEFORMAT );
   if( pSet->ZH_SET_DEFAULT )      pSet->ZH_SET_DEFAULT      = zh_strdup( pSet->ZH_SET_DEFAULT );
   if( pSet->ZH_SET_DELIMCHARS )   pSet->ZH_SET_DELIMCHARS   = zh_strdup( pSet->ZH_SET_DELIMCHARS );
   if( pSet->ZH_SET_DEVICE )       pSet->ZH_SET_DEVICE       = zh_strdup( pSet->ZH_SET_DEVICE );
   if( pSet->ZH_SET_EXTRAFILE )    pSet->ZH_SET_EXTRAFILE    = zh_strdup( pSet->ZH_SET_EXTRAFILE );
   if( pSet->ZH_SET_MFILEEXT  )    pSet->ZH_SET_MFILEEXT     = zh_strdup( pSet->ZH_SET_MFILEEXT );
   if( pSet->ZH_SET_PATH )         pSet->ZH_SET_PATH         = zh_strdup( pSet->ZH_SET_PATH );
   if( pSet->ZH_SET_PRINTFILE )    pSet->ZH_SET_PRINTFILE    = zh_strdup( pSet->ZH_SET_PRINTFILE );
   if( pSet->ZH_SET_EOL )          pSet->ZH_SET_EOL          = zh_strdup( pSet->ZH_SET_EOL );
   if( pSet->ZH_SET_HBOUTLOG )     pSet->ZH_SET_HBOUTLOG     = zh_strdup( pSet->ZH_SET_HBOUTLOG );
   if( pSet->ZH_SET_HBOUTLOGINFO ) pSet->ZH_SET_HBOUTLOGINFO = zh_strdup( pSet->ZH_SET_HBOUTLOGINFO );

   if( pSet->ZH_SET_PATH )
      zh_fsAddSearchPath( pSet->ZH_SET_PATH, &pSet->zh_set_path );

   return pSet;
}

int zh_setListenerAdd( ZH_SET_LISTENER_CALLBACK * callback )
{
   ZH_STACK_TLS_PRELOAD
   PZH_SET_STRUCT pSet = zh_stackSetStruct();
   PZH_SET_LISTENER p_sl = ( PZH_SET_LISTENER ) zh_xgrab( sizeof( ZH_SET_LISTENER ) );
   PZH_SET_LISTENER_LST pList;

   if( ! pSet->zh_set_listener )
      pSet->zh_set_listener = zh_xgrabz( sizeof( ZH_SET_LISTENER_LST ) );

   pList = ( PZH_SET_LISTENER_LST ) pSet->zh_set_listener;

   p_sl->callback = callback;
   p_sl->listener = ++pList->counter;
   p_sl->next = NULL;

   if( pList->last )
      pList->last->next = p_sl;
   else if( ! pList->first )
      pList->first = p_sl;
   pList->last = p_sl;

   return p_sl->listener;
}

void zh_setListenerNotify( ZH_set_enum set, ZH_set_listener_enum when )
{
   ZH_STACK_TLS_PRELOAD
   PZH_SET_LISTENER_LST pList = ( PZH_SET_LISTENER_LST ) zh_stackSetStruct()->zh_set_listener;

   if( pList )
   {
      PZH_SET_LISTENER p_sl = pList->first;
      while( p_sl )
      {
         ( *p_sl->callback )( set, when );
         p_sl = p_sl->next;
      }
   }
}

int zh_setListenerRemove( int listener )
{
   ZH_STACK_TLS_PRELOAD
   PZH_SET_LISTENER_LST pList = ( PZH_SET_LISTENER_LST ) zh_stackSetStruct()->zh_set_listener;

   if( pList )
   {
      PZH_SET_LISTENER p_sl = pList->first;
      PZH_SET_LISTENER p_sl_prev = NULL;
      while( p_sl )
      {
         if( listener == p_sl->listener )
         {
            listener = -listener;
            if( p_sl_prev )
               p_sl_prev->next = p_sl->next;
            else
               pList->first = p_sl->next;
            if( p_sl == pList->last )
               pList->last = p_sl_prev;
            zh_xfree( p_sl );
            break;
         }
         p_sl_prev = p_sl;
         p_sl = p_sl->next;
      }
   }
   return listener;
}

ZH_BOOL zh_setSetItem( ZH_set_enum set_specifier, PZH_ITEM pItem )
{
   ZH_STACK_TLS_PRELOAD
   ZH_BOOL fResult = ZH_FALSE;

   if( pItem )
   {
      PZH_SET_STRUCT pSet = zh_stackSetStruct();
      char * szValue;
      int iValue;

      zh_setListenerNotify( set_specifier, ZH_SET_LISTENER_BEFORE );

      switch( set_specifier )
      {
         case ZH_SET_ALTFILE:
         case ZH_SET_EXTRAFILE:
         case ZH_SET_PRINTFILE:
            /* This sets needs 3rd parameter to indicate additive mode
             * so they cannot be fully supported by this function
             */
            if( ZH_IS_STRING( pItem ) || ZH_IS_NIL( pItem ) )
            {
               open_handle( pSet, zh_itemGetCPtr( pItem ), ZH_FALSE, set_specifier );
               fResult = ZH_TRUE;
               if( set_specifier == ZH_SET_PRINTFILE && pSet->ZH_SET_PRINTFILE == NULL )
                  pSet->ZH_SET_PRINTFILE = zh_set_PRINTFILE_default();
            }
            break;

         case ZH_SET_ALTERNATE:
            if( ZH_IS_LOGICAL( pItem ) )
            {
               pSet->ZH_SET_ALTERNATE = zh_itemGetL( pItem );
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_AUTOPEN:
            if( ZH_IS_LOGICAL( pItem ) )
            {
               pSet->ZH_SET_AUTOPEN = zh_itemGetL( pItem );
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_BELL:
            if( ZH_IS_LOGICAL( pItem ) )
            {
               pSet->ZH_SET_BELL = zh_itemGetL( pItem );
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_CANCEL:
            if( ZH_IS_LOGICAL( pItem ) )
            {
               pSet->ZH_SET_CANCEL = zh_itemGetL( pItem );
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_CONFIRM:
            if( ZH_IS_LOGICAL( pItem ) )
            {
               pSet->ZH_SET_CONFIRM = zh_itemGetL( pItem );
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_CONSOLE:
            if( ZH_IS_LOGICAL( pItem ) )
            {
               pSet->ZH_SET_CONSOLE = zh_itemGetL( pItem );
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_DEBUG:
            if( ZH_IS_LOGICAL( pItem ) )
            {
               pSet->ZH_SET_DEBUG = zh_itemGetL( pItem );
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_DELETED:
            if( ZH_IS_LOGICAL( pItem ) )
            {
               pSet->ZH_SET_DELETED = zh_itemGetL( pItem );
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_DELIMITERS:
            if( ZH_IS_LOGICAL( pItem ) )
            {
               pSet->ZH_SET_DELIMITERS = zh_itemGetL( pItem );
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_EOF:
            if( ZH_IS_LOGICAL( pItem ) )
            {
               pSet->ZH_SET_EOF = zh_itemGetL( pItem );
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_ESCAPE:
            if( ZH_IS_LOGICAL( pItem ) )
            {
               pSet->ZH_SET_ESCAPE = zh_itemGetL( pItem );
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_EXACT:
            if( ZH_IS_LOGICAL( pItem ) )
            {
               pSet->ZH_SET_EXACT = zh_itemGetL( pItem );
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_EXCLUSIVE:
            if( ZH_IS_LOGICAL( pItem ) )
            {
               pSet->ZH_SET_EXCLUSIVE = zh_itemGetL( pItem );
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_EXIT:
            if( ZH_IS_LOGICAL( pItem ) )
            {
               pSet->ZH_SET_EXIT = zh_itemGetL( pItem );
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_EXTRA:
            if( ZH_IS_LOGICAL( pItem ) )
            {
               pSet->ZH_SET_EXTRA = zh_itemGetL( pItem );
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_FIXED:
            if( ZH_IS_LOGICAL( pItem ) )
            {
               pSet->ZH_SET_FIXED = zh_itemGetL( pItem );
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_IDLEREPEAT:
            if( ZH_IS_LOGICAL( pItem ) )
            {
               pSet->ZH_SET_IDLEREPEAT = zh_itemGetL( pItem );
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_INSERT:
            if( ZH_IS_LOGICAL( pItem ) )
            {
               pSet->ZH_SET_INSERT = zh_itemGetL( pItem );
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_INTENSITY:
            if( ZH_IS_LOGICAL( pItem ) )
            {
               pSet->ZH_SET_INTENSITY = zh_itemGetL( pItem );
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_MCENTER:
            if( ZH_IS_LOGICAL( pItem ) )
            {
               pSet->ZH_SET_MCENTER = zh_itemGetL( pItem );
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_OPTIMIZE:
            if( ZH_IS_LOGICAL( pItem ) )
            {
               pSet->ZH_SET_OPTIMIZE = zh_itemGetL( pItem );
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_FORCEOPT:
            if( ZH_IS_LOGICAL( pItem ) )
            {
               pSet->ZH_SET_FORCEOPT = zh_itemGetL( pItem );
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_PRINTER:
            if( ZH_IS_LOGICAL( pItem ) )
            {
               pSet->ZH_SET_PRINTER = zh_itemGetL( pItem );
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_SCOREBOARD:
            if( ZH_IS_LOGICAL( pItem ) )
            {
               pSet->ZH_SET_SCOREBOARD = zh_itemGetL( pItem );
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_SCROLLBREAK:
            if( ZH_IS_LOGICAL( pItem ) )
            {
               pSet->ZH_SET_SCROLLBREAK = zh_itemGetL( pItem );
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_SOFTSEEK:
            if( ZH_IS_LOGICAL( pItem ) )
            {
               pSet->ZH_SET_SOFTSEEK = zh_itemGetL( pItem );
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_STRICTREAD:
            if( ZH_IS_LOGICAL( pItem ) )
            {
               pSet->ZH_SET_STRICTREAD = zh_itemGetL( pItem );
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_UNIQUE:
            if( ZH_IS_LOGICAL( pItem ) )
            {
               pSet->ZH_SET_UNIQUE = zh_itemGetL( pItem );
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_WRAP:
            if( ZH_IS_LOGICAL( pItem ) )
            {
               pSet->ZH_SET_WRAP = zh_itemGetL( pItem );
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_HARDCOMMIT:
            if( ZH_IS_LOGICAL( pItem ) )
            {
               pSet->ZH_SET_HARDCOMMIT = zh_itemGetL( pItem );
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_DEFEXTENSIONS:
            if( ZH_IS_LOGICAL( pItem ) )
            {
               pSet->ZH_SET_DEFEXTENSIONS = zh_itemGetL( pItem );
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_TRIMFILENAME:
            if( ZH_IS_LOGICAL( pItem ) )
            {
               pSet->ZH_SET_TRIMFILENAME = zh_itemGetL( pItem );
               fResult = ZH_TRUE;
            }
            break;

         case ZH_SET_DECIMALS:
            if( ZH_IS_NUMERIC( pItem ) )
            {
               iValue = zh_itemGetNI( pItem );
               if( iValue >= 0 )
               {
                  pSet->ZH_SET_DECIMALS = iValue;
                  fResult = ZH_TRUE;
               }
            }
            break;
         case ZH_SET_EPOCH:
            if( ZH_IS_NUMERIC( pItem ) )
            {
               iValue = zh_itemGetNI( pItem );
               if( iValue >= 0 )
               {
                  pSet->ZH_SET_EPOCH = iValue;
                  fResult = ZH_TRUE;
               }
            }
            break;
         case ZH_SET_MBLOCKSIZE:
            if( ZH_IS_NUMERIC( pItem ) )
            {
               iValue = zh_itemGetNI( pItem );
               if( iValue >= 0 )
               {
                  pSet->ZH_SET_MBLOCKSIZE = iValue;
                  fResult = ZH_TRUE;
               }
            }
            break;
         case ZH_SET_DBFLOCKSCHEME:
            if( ZH_IS_NUMERIC( pItem ) )
            {
               iValue = zh_itemGetNI( pItem );
               if( iValue >= 0 )
               {
                  pSet->ZH_SET_DBFLOCKSCHEME = iValue;
                  fResult = ZH_TRUE;
               }
            }
            break;
         case ZH_SET_AUTORDER:
            if( ZH_IS_NUMERIC( pItem ) )
            {
               iValue = zh_itemGetNI( pItem );
               if( iValue >= 0 )
               {
                  pSet->ZH_SET_AUTORDER = iValue;
                  fResult = ZH_TRUE;
               }
            }
            break;
         case ZH_SET_AUTOSHARE:
            if( ZH_IS_NUMERIC( pItem ) )
            {
               iValue = zh_itemGetNI( pItem );
               if( iValue >= 0 )
               {
                  pSet->ZH_SET_AUTOSHARE = iValue;
                  fResult = ZH_TRUE;
               }
            }
            break;
         case ZH_SET_CURSOR:
            if( ZH_IS_NUMERIC( pItem ) )
            {
               zh_conSetCursor( ZH_TRUE, zh_itemGetNI( pItem ) );
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_EVENTMASK:
            if( ZH_IS_NUMERIC( pItem ) )
            {
               iValue = zh_itemGetNI( pItem );
               if( iValue >= 0 )
               {
                  pSet->ZH_SET_EVENTMASK = iValue;
                  fResult = ZH_TRUE;
               }
            }
            break;
         case ZH_SET_MARGIN:
            if( ZH_IS_NUMERIC( pItem ) )
            {
               iValue = zh_itemGetNI( pItem );
               if( iValue >= 0 )
               {
                  pSet->ZH_SET_MARGIN = iValue;
                  fResult = ZH_TRUE;
               }
            }
            break;
         case ZH_SET_MESSAGE:
            if( ZH_IS_NUMERIC( pItem ) )
            {
               iValue = zh_itemGetNI( pItem );
               if( iValue >= 0 )
               {
                  pSet->ZH_SET_MESSAGE = iValue;
                  fResult = ZH_TRUE;
               }
            }
            break;
         case ZH_SET_TYPEAHEAD:
            if( ZH_IS_NUMERIC( pItem ) )
            {
               /* Set the value and limit the range */
               pSet->ZH_SET_TYPEAHEAD = zh_itemGetNI( pItem );
               if( pSet->ZH_SET_TYPEAHEAD == 0 )
                  /* Do nothing */;
               else if( pSet->ZH_SET_TYPEAHEAD < 16 )
                  pSet->ZH_SET_TYPEAHEAD = 16;
               else if( pSet->ZH_SET_TYPEAHEAD > 4096 )
                  pSet->ZH_SET_TYPEAHEAD = 4096;
               /* reset keyboard buffer */
               zh_inkeyReset();
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_VIDEOMODE:
            if( ZH_IS_NUMERIC( pItem ) )
            {
               pSet->ZH_SET_VIDEOMODE = zh_itemGetNI( pItem );
               fResult = ZH_TRUE;
            }
            break;

         case ZH_SET_COLOR:
            if( ZH_IS_STRING( pItem ) )
            {
               zh_conSetColor( zh_itemGetCPtr( pItem ) );
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_LANGUAGE:
            if( ZH_IS_STRING( pItem ) )
            {
               zh_langSelectID( zh_itemGetCPtr( pItem ) );
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_CODEPAGE:
            if( ZH_IS_STRING( pItem ) )
            {
               zh_cdpSelectID( zh_itemGetCPtr( pItem ) );
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_FILECASE:
         case ZH_SET_DIRCASE:
            iValue = -1;
            if( ZH_IS_STRING( pItem ) )
            {
               if( ! zh_stricmp( zh_itemGetCPtr( pItem ), "LOWER" ) )
                  iValue = ZH_SET_CASE_LOWER;
               else if( ! zh_stricmp( zh_itemGetCPtr( pItem ), "UPPER" ) )
                  iValue = ZH_SET_CASE_UPPER;
               else if( ! zh_stricmp( zh_itemGetCPtr( pItem ), "MIXED" ) )
                  iValue = ZH_SET_CASE_MIXED;
            }
            else if( ZH_IS_NUMERIC( pItem ) )
               iValue = zh_itemGetNI( pItem );

            if( iValue == ZH_SET_CASE_LOWER ||
                iValue == ZH_SET_CASE_UPPER ||
                iValue == ZH_SET_CASE_MIXED )
            {
               if( set_specifier == ZH_SET_FILECASE )
                  pSet->ZH_SET_FILECASE = iValue;
               else
                  pSet->ZH_SET_DIRCASE = iValue;
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_DATEFORMAT:
            if( ZH_IS_STRING( pItem ) )
            {
               int iYear = 0;

               szValue = zh_strndup( zh_itemGetCPtr( pItem ), USHRT_MAX );
               if( pSet->ZH_SET_DATEFORMAT )
                  zh_xfree( pSet->ZH_SET_DATEFORMAT );
               pSet->ZH_SET_DATEFORMAT = szValue;
               while( *szValue )
               {
                  if( *szValue == 'Y' || *szValue == 'y' )
                     ++iYear;
                  else if( iYear )   /* Only count the first set of consecutive "Y"s. */
                     break;
                  ++szValue;
               }
               /* CENTURY is not controlled directly by SET, so there is no
                  notification for changing it indirectly via DATE FORMAT. */
               pSet->zh_set_century = iYear >= 4;
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_TIMEFORMAT:
            if( ZH_IS_STRING( pItem ) )
            {
               szValue = zh_strndup( zh_itemGetCPtr( pItem ), USHRT_MAX );
               if( pSet->ZH_SET_TIMEFORMAT )
                  zh_xfree( pSet->ZH_SET_TIMEFORMAT );
               pSet->ZH_SET_TIMEFORMAT = szValue;
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_DIRSEPARATOR:
            if( zh_itemGetCLen( pItem ) > 0 )
            {
               pSet->ZH_SET_DIRSEPARATOR = zh_itemGetCPtr( pItem )[ 0 ];
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_DEVICE:
            if( ZH_IS_STRING( pItem ) )
            {
               szValue = zh_strndup( zh_itemGetCPtr( pItem ), USHRT_MAX );
               if( pSet->ZH_SET_DEVICE )
                  zh_xfree( pSet->ZH_SET_DEVICE );
               pSet->ZH_SET_DEVICE = szValue;
               pSet->zh_set_prndevice = strlen( szValue ) >= 4 &&
                                        zh_strnicmp( szValue, "PRIN", 4 ) == 0;
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_MFILEEXT:
            if( ZH_IS_STRING( pItem ) || ZH_IS_NIL( pItem ) )
            {
               szValue = zh_strndup( zh_itemGetCPtr( pItem ), USHRT_MAX );
               if( pSet->ZH_SET_MFILEEXT )
                  zh_xfree( pSet->ZH_SET_MFILEEXT );
               pSet->ZH_SET_MFILEEXT = szValue;
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_DEFAULT:
            if( ZH_IS_STRING( pItem ) || ZH_IS_NIL( pItem ) )
            {
               szValue = zh_strndup( zh_itemGetCPtr( pItem ), USHRT_MAX );
               if( pSet->ZH_SET_DEFAULT )
                  zh_xfree( pSet->ZH_SET_DEFAULT );
               pSet->ZH_SET_DEFAULT = szValue;
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_PATH:
            if( ZH_IS_STRING( pItem ) || ZH_IS_NIL( pItem ) )
            {
               szValue = zh_strndup( zh_itemGetCPtr( pItem ), USHRT_MAX );
               if( pSet->ZH_SET_PATH )
                  zh_xfree( pSet->ZH_SET_PATH );
               pSet->ZH_SET_PATH = szValue;

               zh_fsFreeSearchPath( pSet->zh_set_path );
               pSet->zh_set_path = NULL;
               zh_fsAddSearchPath( pSet->ZH_SET_PATH, &pSet->zh_set_path );

               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_DELIMCHARS:
            if( ZH_IS_STRING( pItem ) || ZH_IS_NIL( pItem ) )
            {
               szValue = zh_strndup( zh_itemGetCPtr( pItem ), USHRT_MAX );
               if( pSet->ZH_SET_DELIMCHARS )
                  zh_xfree( pSet->ZH_SET_DELIMCHARS );
               pSet->ZH_SET_DELIMCHARS = szValue;
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_EOL:
            if( ZH_IS_STRING( pItem ) || ZH_IS_NIL( pItem ) )
            {
               szValue = zh_strndup( zh_itemGetCPtr( pItem ), USHRT_MAX );
               if( pSet->ZH_SET_EOL )
                  zh_xfree( pSet->ZH_SET_EOL );
               pSet->ZH_SET_EOL = szValue;
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_HBOUTLOG:
            if( ZH_IS_STRING( pItem ) || ZH_IS_NIL( pItem ) )
            {
               if( ZH_IS_NIL( pItem ) )
                  szValue = NULL;
               else
                  szValue = zh_strndup( zh_itemGetCPtr( pItem ), USHRT_MAX );
               if( pSet->ZH_SET_HBOUTLOG )
                  zh_xfree( pSet->ZH_SET_HBOUTLOG );
               pSet->ZH_SET_HBOUTLOG = szValue;
               zh_xsetfilename( pSet->ZH_SET_HBOUTLOG );
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_HBOUTLOGINFO:
            if( ZH_IS_STRING( pItem ) || ZH_IS_NIL( pItem ) )
            {
               szValue = zh_strndup( zh_itemGetCPtr( pItem ), USHRT_MAX );
               if( pSet->ZH_SET_HBOUTLOGINFO )
                  zh_xfree( pSet->ZH_SET_HBOUTLOGINFO );
               pSet->ZH_SET_HBOUTLOGINFO = szValue;
               zh_xsetinfo( pSet->ZH_SET_HBOUTLOGINFO );
               fResult = ZH_TRUE;
            }
            break;
         case ZH_SET_OSCODEPAGE:
            if( ZH_IS_NIL( pItem ) )
            {
               pSet->zh_set_oscp = NULL;
               fResult = ZH_TRUE;
            }
            else if( ZH_IS_STRING( pItem ) )
            {
               PZH_CODEPAGE cdp = zh_cdpFindExt( zh_itemGetCPtr( pItem ) );
               if( cdp )
               {
                  pSet->zh_set_oscp = ( void * ) cdp;
                  fResult = ZH_TRUE;
               }
            }
            break;
         case ZH_SET_DBCODEPAGE:
            if( ZH_IS_NIL( pItem ) )
            {
               pSet->zh_set_dbcp = NULL;
               fResult = ZH_TRUE;
            }
            else if( ZH_IS_STRING( pItem ) )
            {
               PZH_CODEPAGE cdp = zh_cdpFindExt( zh_itemGetCPtr( pItem ) );
               if( cdp )
               {
                  pSet->zh_set_dbcp = ( void * ) cdp;
                  fResult = ZH_TRUE;
               }
            }
            break;

         case ZH_SET_INVALID_:
            break;
#if 0
         /*
          * intentionally removed default: clause to enable C compiler warning
          * when not all ZH_SET_* cases are implemented. [druzus]
          */
         default:
            break;
#endif
      }
      zh_setListenerNotify( set_specifier, ZH_SET_LISTENER_AFTER );
   }

   return fResult;
}

ZH_BOOL zh_setSetItem2( ZH_set_enum set_specifier, PZH_ITEM pItem1, PZH_ITEM pItem2 )
{
   ZH_BOOL fResult = ZH_FALSE;

   if( pItem1 )
   {
      switch( set_specifier )
      {
         case ZH_SET_ALTFILE:
         case ZH_SET_EXTRAFILE:
         case ZH_SET_PRINTFILE:
            if( ZH_IS_STRING( pItem1 ) || ZH_IS_NIL( pItem1 ) )
            {
               ZH_STACK_TLS_PRELOAD
               PZH_SET_STRUCT pSet = zh_stackSetStruct();

               zh_setListenerNotify( set_specifier, ZH_SET_LISTENER_BEFORE );

               open_handle( pSet, zh_itemGetCPtr( pItem1 ),
                            set_logical( pItem2, ZH_FALSE ), set_specifier );
               fResult = ZH_TRUE;
               if( set_specifier == ZH_SET_PRINTFILE && pSet->ZH_SET_PRINTFILE == NULL )
                  pSet->ZH_SET_PRINTFILE = zh_set_PRINTFILE_default();

               zh_setListenerNotify( set_specifier, ZH_SET_LISTENER_AFTER );
            }
            break;
         default:
            fResult = zh_setSetItem( set_specifier, pItem1 );
      }
   }
   return fResult;
}

ZH_BOOL zh_setGetL( ZH_set_enum set_specifier )
{
   ZH_STACK_TLS_PRELOAD
   PZH_SET_STRUCT pSet = zh_stackSetStruct();

   switch( set_specifier )
   {
      case ZH_SET_ALTERNATE:
         return pSet->ZH_SET_ALTERNATE;
      case ZH_SET_AUTOPEN:
         return pSet->ZH_SET_AUTOPEN;
      case ZH_SET_BELL:
         return pSet->ZH_SET_BELL;
      case ZH_SET_CANCEL:
         return pSet->ZH_SET_CANCEL;
      case ZH_SET_CONFIRM:
         return pSet->ZH_SET_CONFIRM;
      case ZH_SET_CONSOLE:
         return pSet->ZH_SET_CONSOLE;
      case ZH_SET_DEBUG:
         return pSet->ZH_SET_DEBUG;
      case ZH_SET_DELETED:
         return pSet->ZH_SET_DELETED;
      case ZH_SET_DELIMITERS:
         return pSet->ZH_SET_DELIMITERS;
      case ZH_SET_EOF:
         return pSet->ZH_SET_EOF;
      case ZH_SET_ESCAPE:
         return pSet->ZH_SET_ESCAPE;
      case ZH_SET_EXACT:
         return pSet->ZH_SET_EXACT;
      case ZH_SET_EXCLUSIVE:
         return pSet->ZH_SET_EXCLUSIVE;
      case ZH_SET_EXIT:
         return pSet->ZH_SET_EXIT;
      case ZH_SET_EXTRA:
         return pSet->ZH_SET_EXTRA;
      case ZH_SET_FIXED:
         return pSet->ZH_SET_FIXED;
      case ZH_SET_IDLEREPEAT:
         return pSet->ZH_SET_IDLEREPEAT;
      case ZH_SET_INSERT:
         return pSet->ZH_SET_INSERT;
      case ZH_SET_INTENSITY:
         return pSet->ZH_SET_INTENSITY;
      case ZH_SET_MCENTER:
         return pSet->ZH_SET_MCENTER;
      case ZH_SET_OPTIMIZE:
         return pSet->ZH_SET_OPTIMIZE;
      case ZH_SET_FORCEOPT:
         return pSet->ZH_SET_FORCEOPT;
      case ZH_SET_PRINTER:
         return pSet->ZH_SET_PRINTER;
      case ZH_SET_SCOREBOARD:
         return pSet->ZH_SET_SCOREBOARD;
      case ZH_SET_SCROLLBREAK:
         return pSet->ZH_SET_SCROLLBREAK;
      case ZH_SET_SOFTSEEK:
         return pSet->ZH_SET_SOFTSEEK;
      case ZH_SET_STRICTREAD:
         return pSet->ZH_SET_STRICTREAD;
      case ZH_SET_UNIQUE:
         return pSet->ZH_SET_UNIQUE;
      case ZH_SET_WRAP:
         return pSet->ZH_SET_WRAP;
      case ZH_SET_HARDCOMMIT:
         return pSet->ZH_SET_HARDCOMMIT;
      case ZH_SET_DEFEXTENSIONS:
         return pSet->ZH_SET_DEFEXTENSIONS;
      case ZH_SET_TRIMFILENAME:
         return pSet->ZH_SET_TRIMFILENAME;

      case ZH_SET_ALTFILE:
      case ZH_SET_AUTORDER:
      case ZH_SET_AUTOSHARE:
      case ZH_SET_COLOR:
      case ZH_SET_CURSOR:
      case ZH_SET_DATEFORMAT:
      case ZH_SET_TIMEFORMAT:
      case ZH_SET_DECIMALS:
      case ZH_SET_DEFAULT:
      case ZH_SET_DELIMCHARS:
      case ZH_SET_DEVICE:
      case ZH_SET_EPOCH:
      case ZH_SET_EVENTMASK:
      case ZH_SET_EXTRAFILE:
      case ZH_SET_MARGIN:
      case ZH_SET_MBLOCKSIZE:
      case ZH_SET_MESSAGE:
      case ZH_SET_MFILEEXT:
      case ZH_SET_PATH:
      case ZH_SET_PRINTFILE:
      case ZH_SET_TYPEAHEAD:
      case ZH_SET_VIDEOMODE:
      case ZH_SET_LANGUAGE:
      case ZH_SET_CODEPAGE:
      case ZH_SET_FILECASE:
      case ZH_SET_DIRCASE:
      case ZH_SET_DIRSEPARATOR:
      case ZH_SET_DBFLOCKSCHEME:
      case ZH_SET_EOL:
      case ZH_SET_HBOUTLOG:
      case ZH_SET_HBOUTLOGINFO:
      case ZH_SET_OSCODEPAGE:
      case ZH_SET_DBCODEPAGE:
      case ZH_SET_INVALID_:
         break;
#if 0
      /*
       * intentionally removed default: clause to enable C compiler warning
       * when not all ZH_SET_* cases are implemented. [druzus]
       */
      default:
         break;
#endif
   }

   zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, 0 );
   return ZH_FALSE;
}

const char * zh_setGetCPtr( ZH_set_enum set_specifier )
{
   ZH_STACK_TLS_PRELOAD
   PZH_SET_STRUCT pSet = zh_stackSetStruct();

   switch( set_specifier )
   {
      case ZH_SET_ALTFILE:
         return pSet->ZH_SET_ALTFILE;
      case ZH_SET_COLOR:
         return pSet->ZH_SET_COLOR;
      case ZH_SET_DATEFORMAT:
         return pSet->ZH_SET_DATEFORMAT;
      case ZH_SET_TIMEFORMAT:
         return pSet->ZH_SET_TIMEFORMAT;
      case ZH_SET_DEFAULT:
         return pSet->ZH_SET_DEFAULT;
      case ZH_SET_DELIMCHARS:
         return pSet->ZH_SET_DELIMCHARS;
      case ZH_SET_DEVICE:
         return pSet->ZH_SET_DEVICE;
      case ZH_SET_EXTRAFILE:
         return pSet->ZH_SET_EXTRAFILE;
      case ZH_SET_PATH:
         return pSet->ZH_SET_PATH;
      case ZH_SET_MFILEEXT:
         return pSet->ZH_SET_MFILEEXT;
      case ZH_SET_PRINTFILE:
         return pSet->ZH_SET_PRINTFILE;
      case ZH_SET_EOL:
         return pSet->ZH_SET_EOL;
      case ZH_SET_HBOUTLOG:
         return pSet->ZH_SET_HBOUTLOG;
      case ZH_SET_HBOUTLOGINFO:
         return pSet->ZH_SET_HBOUTLOGINFO;
      case ZH_SET_OSCODEPAGE:
         return pSet->zh_set_oscp ? ( ( PZH_CODEPAGE ) pSet->zh_set_oscp )->id : NULL;
      case ZH_SET_DBCODEPAGE:
         return pSet->zh_set_dbcp ? ( ( PZH_CODEPAGE ) pSet->zh_set_dbcp )->id : NULL;
      case ZH_SET_LANGUAGE:
         return zh_langID();
      case ZH_SET_CODEPAGE:
         return zh_cdpID();
      case ZH_SET_ALTERNATE:
      case ZH_SET_AUTOPEN:
      case ZH_SET_AUTORDER:
      case ZH_SET_AUTOSHARE:
      case ZH_SET_BELL:
      case ZH_SET_CANCEL:
      case ZH_SET_CONFIRM:
      case ZH_SET_CONSOLE:
      case ZH_SET_CURSOR:
      case ZH_SET_DEBUG:
      case ZH_SET_DECIMALS:
      case ZH_SET_DELETED:
      case ZH_SET_DELIMITERS:
      case ZH_SET_EOF:
      case ZH_SET_EPOCH:
      case ZH_SET_ESCAPE:
      case ZH_SET_EVENTMASK:
      case ZH_SET_EXACT:
      case ZH_SET_EXCLUSIVE:
      case ZH_SET_EXIT:
      case ZH_SET_EXTRA:
      case ZH_SET_FIXED:
      case ZH_SET_INSERT:
      case ZH_SET_INTENSITY:
      case ZH_SET_MARGIN:
      case ZH_SET_MBLOCKSIZE:
      case ZH_SET_MCENTER:
      case ZH_SET_MESSAGE:
      case ZH_SET_OPTIMIZE:
      case ZH_SET_FORCEOPT:
      case ZH_SET_STRICTREAD:
      case ZH_SET_HARDCOMMIT:
      case ZH_SET_PRINTER:
      case ZH_SET_SCOREBOARD:
      case ZH_SET_SCROLLBREAK:
      case ZH_SET_SOFTSEEK:
      case ZH_SET_TYPEAHEAD:
      case ZH_SET_UNIQUE:
      case ZH_SET_VIDEOMODE:
      case ZH_SET_WRAP:
      case ZH_SET_IDLEREPEAT:
      case ZH_SET_FILECASE:
      case ZH_SET_DIRCASE:
      case ZH_SET_DIRSEPARATOR:
      case ZH_SET_DBFLOCKSCHEME:
      case ZH_SET_DEFEXTENSIONS:
      case ZH_SET_TRIMFILENAME:
      case ZH_SET_INVALID_:
         break;
#if 0
      /*
       * intentionally removed default: clause to enable C compiler warning
       * when not all ZH_SET_* cases are implemented. [druzus]
       */
      default:
         break;
#endif
   }

   zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, 0 );
   return NULL;
}

int     zh_setGetNI( ZH_set_enum set_specifier )
{
   ZH_STACK_TLS_PRELOAD
   PZH_SET_STRUCT pSet = zh_stackSetStruct();

   switch( set_specifier )
   {
      case ZH_SET_AUTORDER:
         return pSet->ZH_SET_AUTORDER;
      case ZH_SET_AUTOSHARE:
         return pSet->ZH_SET_AUTOSHARE;
      case ZH_SET_DECIMALS:
         return pSet->ZH_SET_DECIMALS;
      case ZH_SET_EPOCH:
         return pSet->ZH_SET_EPOCH;
      case ZH_SET_EVENTMASK:
         return pSet->ZH_SET_EVENTMASK;
      case ZH_SET_MARGIN:
         return pSet->ZH_SET_MARGIN;
      case ZH_SET_MBLOCKSIZE:
         return pSet->ZH_SET_MBLOCKSIZE;
      case ZH_SET_MESSAGE:
         return pSet->ZH_SET_MESSAGE;
      case ZH_SET_TYPEAHEAD:
         return pSet->ZH_SET_TYPEAHEAD;
      case ZH_SET_FILECASE:
         return pSet->ZH_SET_FILECASE;
      case ZH_SET_DIRCASE:
         return pSet->ZH_SET_DIRCASE;
      case ZH_SET_DIRSEPARATOR:
         return pSet->ZH_SET_DIRSEPARATOR;
      case ZH_SET_VIDEOMODE:
         return pSet->ZH_SET_VIDEOMODE;
      case ZH_SET_DBFLOCKSCHEME:
         return pSet->ZH_SET_DBFLOCKSCHEME;

      case ZH_SET_ALTERNATE:
      case ZH_SET_ALTFILE:
      case ZH_SET_AUTOPEN:
      case ZH_SET_BELL:
      case ZH_SET_CANCEL:
      case ZH_SET_COLOR:
      case ZH_SET_CONFIRM:
      case ZH_SET_CONSOLE:
      case ZH_SET_CURSOR:
      case ZH_SET_DATEFORMAT:
      case ZH_SET_TIMEFORMAT:
      case ZH_SET_DEBUG:
      case ZH_SET_DEFAULT:
      case ZH_SET_DELETED:
      case ZH_SET_DELIMCHARS:
      case ZH_SET_DELIMITERS:
      case ZH_SET_DEVICE:
      case ZH_SET_EOF:
      case ZH_SET_ESCAPE:
      case ZH_SET_EXACT:
      case ZH_SET_EXCLUSIVE:
      case ZH_SET_EXIT:
      case ZH_SET_EXTRA:
      case ZH_SET_EXTRAFILE:
      case ZH_SET_FIXED:
      case ZH_SET_INSERT:
      case ZH_SET_INTENSITY:
      case ZH_SET_MCENTER:
      case ZH_SET_MFILEEXT:
      case ZH_SET_OPTIMIZE:
      case ZH_SET_FORCEOPT:
      case ZH_SET_STRICTREAD:
      case ZH_SET_HARDCOMMIT:
      case ZH_SET_PATH:
      case ZH_SET_PRINTER:
      case ZH_SET_PRINTFILE:
      case ZH_SET_SCOREBOARD:
      case ZH_SET_SCROLLBREAK:
      case ZH_SET_SOFTSEEK:
      case ZH_SET_UNIQUE:
      case ZH_SET_WRAP:
      case ZH_SET_LANGUAGE:
      case ZH_SET_CODEPAGE:
      case ZH_SET_IDLEREPEAT:
      case ZH_SET_EOL:
      case ZH_SET_DEFEXTENSIONS:
      case ZH_SET_TRIMFILENAME:
      case ZH_SET_HBOUTLOG:
      case ZH_SET_HBOUTLOGINFO:
      case ZH_SET_OSCODEPAGE:
      case ZH_SET_DBCODEPAGE:
      case ZH_SET_INVALID_:
         break;
#if 0
      /*
       * intentionally removed default: clause to enable C compiler warning
       * when not all ZH_SET_* cases are implemented. [druzus]
       */
      default:
         break;
#endif
   }

   zh_errRT_BASE( EG_ARG, 2020, NULL, ZH_ERR_FUNCNAME, 0 );
   return 0;
}

long    zh_setGetNL( ZH_set_enum set_specifier )
{
   return zh_setGetNI( set_specifier );
}

ZH_PATHNAMES * zh_setGetFirstSetPath( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->zh_set_path;
}

PZH_FILE zh_setGetAltHan( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->zh_set_althan;
}

ZH_BOOL zh_setGetCentury( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->zh_set_century;
}

PZH_FILE zh_setGetExtraHan( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->zh_set_extrahan;
}

PZH_FILE zh_setGetPrintHan( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->zh_set_printhan;
}

ZH_BOOL zh_setGetAlternate( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_ALTERNATE;
}

const char *  zh_setGetAltFile( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_ALTFILE;
}

ZH_BOOL zh_setGetAutOpen( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_AUTOPEN;
}

int     zh_setGetAutOrder( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_AUTORDER;
}

int     zh_setGetAutoShare( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_AUTOSHARE;
}

ZH_BOOL zh_setGetBell( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_BELL;
}

ZH_BOOL zh_setGetCancel( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_CANCEL;
}

char *  zh_setGetColor( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_COLOR;
}

ZH_BOOL zh_setGetConfirm( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_CONFIRM;
}

ZH_BOOL zh_setGetConsole( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_CONSOLE;
}

const char * zh_setGetDateFormat( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_DATEFORMAT;
}

const char * zh_setGetTimeFormat( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_TIMEFORMAT;
}

ZH_BOOL zh_setGetDebug( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_DEBUG;
}

int     zh_setGetDecimals( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_DECIMALS;
}

const char *  zh_setGetDefault( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_DEFAULT;
}

ZH_BOOL zh_setGetDeleted( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_DELETED;
}

const char *  zh_setGetDelimChars( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_DELIMCHARS;
}

ZH_BOOL zh_setGetDelimiters( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_DELIMITERS;
}

const char *  zh_setGetDevice( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_DEVICE;
}

ZH_BOOL zh_setGetEOF( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_EOF;
}

int     zh_setGetEpoch( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_EPOCH;
}

ZH_BOOL zh_setGetEscape( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_ESCAPE;
}

int     zh_setGetEventMask( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_EVENTMASK;
}

ZH_BOOL zh_setGetExact( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_EXACT;
}

ZH_BOOL zh_setGetExclusive( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_EXCLUSIVE;
}

ZH_BOOL zh_setGetExit( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_EXIT;
}

ZH_BOOL zh_setGetExtra( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_EXTRA;
}

const char *  zh_setGetExtraFile( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_EXTRAFILE;
}

ZH_BOOL zh_setGetFixed( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_FIXED;
}

ZH_BOOL zh_setGetIdleRepeat( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_IDLEREPEAT;
}

ZH_BOOL zh_setGetInsert( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_INSERT;
}

ZH_BOOL zh_setGetIntensity( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_INTENSITY;
}

const char *  zh_setGetPath( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_PATH;
}

int     zh_setGetMargin( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_MARGIN;
}

int     zh_setGetMBlockSize( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_MBLOCKSIZE;
}

ZH_BOOL zh_setGetMCenter( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_MCENTER;
}

int     zh_setGetMessage( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_MESSAGE;
}

const char *  zh_setGetMFileExt( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_MFILEEXT;
}

ZH_BOOL zh_setGetOptimize( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_OPTIMIZE;
}

ZH_BOOL zh_setGetPrinter( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_PRINTER;
}

const char *  zh_setGetPrintFile( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_PRINTFILE;
}

ZH_BOOL zh_setGetScoreBoard( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_SCOREBOARD;
}

ZH_BOOL zh_setGetScrollBreak( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_SCROLLBREAK;
}

ZH_BOOL zh_setGetSoftSeek( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_SOFTSEEK;
}

ZH_BOOL zh_setGetStrictRead( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_STRICTREAD;
}

int     zh_setGetTypeAhead( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_TYPEAHEAD;
}

ZH_BOOL zh_setGetUnique( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_UNIQUE;
}

int     zh_setGetFileCase( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_FILECASE;
}

void zh_setSetFileCase( int iFileCase )
{
   ZH_STACK_TLS_PRELOAD
   zh_stackSetStruct()->ZH_SET_FILECASE = iFileCase;
}

int     zh_setGetDirCase( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_DIRCASE;
}

void zh_setSetDirCase( int iDirCase )
{
   ZH_STACK_TLS_PRELOAD
   zh_stackSetStruct()->ZH_SET_DIRCASE = iDirCase;
}

int zh_setGetDirSeparator( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_DIRSEPARATOR;
}

void zh_setSetDirSeparator( int iSeparator )
{
   ZH_STACK_TLS_PRELOAD
   zh_stackSetStruct()->ZH_SET_DIRSEPARATOR = iSeparator;
}

ZH_BOOL zh_setGetTrimFileName( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_TRIMFILENAME;
}

void zh_setSetTrimFileName( ZH_BOOL fTrim )
{
   ZH_STACK_TLS_PRELOAD
   zh_stackSetStruct()->ZH_SET_TRIMFILENAME = fTrim;
}

int     zh_setGetVideoMode( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_VIDEOMODE;
}

ZH_BOOL zh_setGetWrap( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_WRAP;
}

int     zh_setGetDBFLockScheme( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_DBFLOCKSCHEME;
}

ZH_BOOL zh_setGetHardCommit( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_HARDCOMMIT;
}

ZH_BOOL zh_setGetForceOpt( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_FORCEOPT;
}

ZH_BOOL zh_setGetDefExtension( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_DEFEXTENSIONS;
}

const char * zh_setGetEOL( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_EOL;
}

const char * zh_setGetHBOUTLOG( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_HBOUTLOG;
}

const char * zh_setGetHBOUTLOGINFO( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->ZH_SET_HBOUTLOGINFO;
}

const char * zh_setGetOSCODEPAGE( void )
{
   ZH_STACK_TLS_PRELOAD
   PZH_SET_STRUCT pSet = zh_stackSetStruct();

   return pSet->zh_set_oscp ? ( ( PZH_CODEPAGE ) pSet->zh_set_oscp )->id : NULL;
}

void * zh_setGetOSCP( void )
{
   ZH_STACK_TLS_PRELOAD
   return zh_stackSetStruct()->zh_set_oscp;
}

const char * zh_setGetDBCODEPAGE( void )
{
   ZH_STACK_TLS_PRELOAD
   PZH_SET_STRUCT pSet = zh_stackSetStruct();

   return pSet->zh_set_dbcp ? ( ( PZH_CODEPAGE ) pSet->zh_set_dbcp )->id : NULL;
}

ZH_BOOL zh_osUseCP( void )
{
   ZH_STACK_TLS_PRELOAD

   if( zh_stackId() )
   {
      PZH_CODEPAGE cdpOS = ( PZH_CODEPAGE ) zh_stackSetStruct()->zh_set_oscp;
      if( cdpOS )
      {
         PZH_CODEPAGE cdpHost = zh_vmCDP();
         return cdpHost && cdpHost != cdpOS;
      }
   }

   return ZH_FALSE;
}

const char * zh_osEncodeCP( const char * szName, char ** pszFree, ZH_SIZE * pnSize )
{
   if( zh_vmIsReady() )
   {
      ZH_STACK_TLS_PRELOAD
      PZH_CODEPAGE cdpOS = ( PZH_CODEPAGE ) zh_stackSetStruct()->zh_set_oscp;
      if( cdpOS )
      {
         PZH_CODEPAGE cdpHost = zh_vmCDP();
         if( cdpHost && cdpHost != cdpOS )
         {
            ZH_SIZE nSize = 0;
            char * pszBuf;

            if( pszFree == NULL )
            {
               pszFree = ( char ** ) ZH_UNCONST( &szName );
               nSize = strlen( szName );
            }
            pszBuf = *pszFree;
            if( pnSize == NULL )
               pnSize = &nSize;
            else if( *pnSize > 0 )
               nSize = *pnSize - 1;

            szName = zh_cdpnDup3( szName, strlen( szName ),
                                  pszBuf, &nSize, pszFree, pnSize,
                                  cdpHost, cdpOS );
         }
      }
   }

   return szName;
}

const char * zh_osDecodeCP( const char * szName, char ** pszFree, ZH_SIZE * pnSize )
{
   if( zh_vmIsReady() )
   {
      ZH_STACK_TLS_PRELOAD
      PZH_CODEPAGE cdpOS = ( PZH_CODEPAGE ) zh_stackSetStruct()->zh_set_oscp;
      if( cdpOS )
      {
         PZH_CODEPAGE cdpHost = zh_vmCDP();
         if( cdpHost && cdpHost != cdpOS )
         {
            ZH_SIZE nSize = 0;
            char * pszBuf;

            if( pszFree == NULL )
            {
               pszFree = ( char ** ) ZH_UNCONST( &szName );
               nSize = strlen( szName );
            }
            pszBuf = *pszFree;
            if( pnSize == NULL )
               pnSize = &nSize;
            else if( *pnSize > 0 )
               nSize = *pnSize - 1;

            szName = zh_cdpnDup3( szName, strlen( szName ),
                                  pszBuf, &nSize, pszFree, pnSize,
                                  cdpOS, cdpHost );
         }
      }
   }

   return szName;
}

char * zh_osStrEncode( const char * pszName )
{
   if( zh_vmIsReady() )
   {
      ZH_STACK_TLS_PRELOAD
      PZH_CODEPAGE cdpOS = ( PZH_CODEPAGE ) zh_stackSetStruct()->zh_set_oscp;
      if( cdpOS )
      {
         PZH_CODEPAGE cdpHost = zh_vmCDP();
         if( cdpHost && cdpHost != cdpOS )
            return zh_cdpDup( pszName, cdpHost, cdpOS );
      }
   }

   return zh_strdup( pszName );
}

char * zh_osStrEncodeN( const char * pszName, ZH_SIZE nLen )
{
   if( zh_vmIsReady() )
   {
      ZH_STACK_TLS_PRELOAD
      PZH_CODEPAGE cdpOS = ( PZH_CODEPAGE ) zh_stackSetStruct()->zh_set_oscp;
      if( cdpOS )
      {
         PZH_CODEPAGE cdpHost = zh_vmCDP();
         if( cdpHost && cdpHost != cdpOS )
            return zh_cdpDupn( pszName, nLen, cdpHost, cdpOS );
      }
   }

   return zh_strndup( pszName, nLen );
}

char * zh_osStrDecode( const char * pszName )
{
   if( zh_vmIsReady() )
   {
      ZH_STACK_TLS_PRELOAD
      PZH_CODEPAGE cdpOS = ( PZH_CODEPAGE ) zh_stackSetStruct()->zh_set_oscp;
      if( cdpOS )
      {
         PZH_CODEPAGE cdpHost = zh_vmCDP();
         if( cdpHost && cdpHost != cdpOS )
            return zh_cdpDup( pszName, cdpOS, cdpHost );
      }
   }

   return zh_strdup( pszName );
}

char * zh_osStrDecode2( const char * pszName, char * pszBuffer, ZH_SIZE nSize )
{
   if( zh_vmIsReady() )
   {
      ZH_STACK_TLS_PRELOAD
      PZH_CODEPAGE cdpOS = ( PZH_CODEPAGE ) zh_stackSetStruct()->zh_set_oscp;
      if( cdpOS )
      {
         PZH_CODEPAGE cdpHost = zh_vmCDP();
         if( cdpHost && cdpHost != cdpOS )
         {
            pszBuffer[ nSize ] = 0;
            zh_cdpnDup2( pszName, strlen( pszName ), pszBuffer, &nSize, cdpOS, cdpHost );
            return pszBuffer;
         }
      }
   }

   return zh_strncpy( pszBuffer, pszName, nSize );
}

#if defined( ZH_OS_WIN )
ZH_WCHAR * zh_osStrU16Encode( const char * pszName )
{
   if( zh_vmIsReady() )
   {
      PZH_CODEPAGE cdp = zh_vmCDP();
      if( cdp )
      {
         ZH_SIZE nLen, nSize;
         ZH_WCHAR * pszBufferW;

         nLen = strlen( pszName );
         nSize = zh_cdpStrAsU16Len( cdp, pszName, nLen, 0 );
         pszBufferW = ( ZH_WCHAR * ) zh_xgrab( ( nSize + 1 ) * sizeof( ZH_WCHAR ) );
         zh_cdpStrToU16( cdp, ZH_CODEPAGE_ENDIAN_NATIVE, pszName, nLen, pszBufferW, nSize + 1 );
         return pszBufferW;
      }
   }

   return zh_mbtowc( pszName ); /* No HVM stack */
}

ZH_WCHAR * zh_osStrU16EncodeN( const char * pszName, ZH_SIZE nLen )
{
   if( zh_vmIsReady() )
   {
      PZH_CODEPAGE cdp = zh_vmCDP();
      if( cdp )
      {
         ZH_SIZE nSize;
         ZH_WCHAR * pszBufferW;

         nLen = zh_strnlen( pszName, nLen );
         nSize = zh_cdpStrAsU16Len( cdp, pszName, nLen, 0 );
         pszBufferW = ( ZH_WCHAR * ) zh_xgrab( ( nSize + 1 ) * sizeof( ZH_WCHAR ) );
         zh_cdpStrToU16( cdp, ZH_CODEPAGE_ENDIAN_NATIVE, pszName, nLen, pszBufferW, nSize + 1 );
         return pszBufferW;
      }
   }

   return zh_mbntowc( pszName, nLen ); /* No HVM stack */
}

char * zh_osStrU16Decode( const ZH_WCHAR * pszNameW )
{
   if( zh_vmIsReady() )
   {
      PZH_CODEPAGE cdp = zh_vmCDP();
      if( cdp )
      {
         ZH_SIZE nLen, nSize;
         char * pszBuffer;

         nLen = zh_wstrlen( pszNameW );
         nSize = zh_cdpU16AsStrLen( cdp, pszNameW, nLen, 0 );
         pszBuffer = ( char * ) zh_xgrab( nSize + 1 );
         zh_cdpU16ToStr( cdp, ZH_CODEPAGE_ENDIAN_NATIVE, pszNameW, nLen, pszBuffer, nSize + 1 );
         return pszBuffer;
      }
   }

   return zh_wctomb( pszNameW ); /* No HVM stack */
}

char * zh_osStrU16Decode2( const ZH_WCHAR * pszNameW, char * pszBuffer, ZH_SIZE nSize )
{
   if( zh_vmIsReady() )
   {
      PZH_CODEPAGE cdp = zh_vmCDP();
      if( cdp )
      {
         zh_cdpU16ToStr( cdp, ZH_CODEPAGE_ENDIAN_NATIVE, pszNameW, zh_wstrlen( pszNameW ), pszBuffer, nSize );
         pszBuffer[ nSize ] = 0;
         return pszBuffer;
      }
   }

   zh_wcntombcpy( pszBuffer, pszNameW, nSize ); /* No HVM stack */
   return pszBuffer;
}
#endif

PZH_FILE zh_setGetPrinterHandle( int iType )
{
   ZH_STACK_TLS_PRELOAD
   PZH_SET_STRUCT pSet = zh_stackSetStruct();

   switch( iType )
   {
      case ZH_SET_PRN_DEV:
         if( ! pSet->zh_set_prndevice )
            return NULL;
         break;
      case ZH_SET_PRN_CON:
         if( ! pSet->ZH_SET_PRINTER )
            return NULL;
         break;
      case ZH_SET_PRN_ANY:
         break;
      default:
         return NULL;
   }

   if( pSet->zh_set_printhan == NULL && pSet->ZH_SET_PRINTFILE )
      open_handle( pSet, pSet->ZH_SET_PRINTFILE, ZH_FALSE, ZH_SET_PRINTFILE );

   return pSet->zh_set_printhan;
}
