/*
 * Error code translations
 *
 * Copyright 2003 Przemyslaw Czerpak <druzus@acn.waw.pl>
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
#include "zh_apifs.h"
#include "zh_stack.h"
#include "zh_io.h"

#if defined( ZH_OS_WIN )
#  include <windows.h>
#else
#  include <errno.h>
#endif

/* Try to translate C errno into DOS error code */
#if ! defined( ZH_OS_WIN )
static ZH_ERRCODE zh_errnoToDosError( int ErrCode )
{
   switch( ErrCode )
   {
#if defined( ENMFILE )
      case ENMFILE:
#endif
      case ENOENT:
         return 2;   /* File not found */
#if defined( ENOTDIR )
      case ENOTDIR:
         return 3;   /* Path not found */
#endif
#if defined( ENFILE )
      case ENFILE:
#endif
      case EMFILE:
         return 4;   /* Too many open files */
      case EACCES:
#if defined( ETXTBSY )
      case ETXTBSY:
#endif
#if defined( EPERM )
      case EPERM:
#endif
         return 5;   /* Access denied */
      case EBADF:
         return 6;   /* Invalid handle */
      case ENOMEM:
         return 8;   /* Insufficient memory */
#if defined( EFAULT )
      case EFAULT:
         return 9;   /* Invalid memory block address */
#endif
      case EINVAL:
         return 13;  /* Invalid data */
#if defined( EROFS )
      case EROFS:
         return 19;  /* Attempt to write on write-protected diskette */
#endif
#if defined( ESPIPE )
      case ESPIPE:
         return 25;  /* Seek error */
#endif
#if defined( ENOSPC )
      case ENOSPC:
         return 29;  /* Write fault */
#endif
      case EPIPE:
         return 29;  /* Write fault */
      case EEXIST:
         return 32;  /* Sharing violation */
      case EAGAIN:
         return 33;  /* Lock violation */
   }

   return ( ZH_ERRCODE ) ErrCode;
}

#else

static ZH_ERRCODE zh_WinToDosError( DWORD dwError )
{
   #ifndef ERROR_PRIVILEGE_NOT_HELD
   #define ERROR_PRIVILEGE_NOT_HELD  1314L
   #endif

   switch( dwError )
   {
      case ERROR_PRIVILEGE_NOT_HELD:
      case ERROR_ALREADY_EXISTS:          return 5;
      case ERROR_FILE_NOT_FOUND:          return 2;
      case ERROR_PATH_NOT_FOUND:          return 3;
      case ERROR_TOO_MANY_OPEN_FILES:     return 4;
      case ERROR_INVALID_HANDLE:          return 6;
   }

   return ( ZH_ERRCODE ) dwError;
}

#endif

/* return FError() code */
ZH_ERRCODE zh_fsGetFError( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsGetFError()" ) );

   return zh_stackIOErrors()->uiFError;
}

/* return DOS error code of last operation */
ZH_ERRCODE zh_fsError( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsError()" ) );

   return zh_stackIOErrors()->uiErrorLast;
}

/* return real error code of last operation */
ZH_ERRCODE zh_fsOsError( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsOsError()" ) );

   return zh_stackIOErrors()->uiOsErrorLast;
}

/* set FError() code */
void zh_fsSetFError( ZH_ERRCODE uiError )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsSetFError(%u)", uiError ) );

   zh_stackIOErrors()->uiFError = uiError;
}

/* set DOS error code for last operation */
void  zh_fsSetError( ZH_ERRCODE uiError )
{
   PZH_IOERRORS pIOErrors;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsSetError(%u)", uiError ) );

   pIOErrors = zh_stackIOErrors();
   /* TODO: untranslate uiError into errno */
   pIOErrors->uiOsErrorLast = pIOErrors->uiErrorLast = uiError;
}

/* set error code for last operation */
void  zh_fsSetIOError( ZH_BOOL fResult, ZH_USHORT uiOperation )
{
   ZH_ERRCODE uiOsErrorLast, uiErrorLast;
   PZH_IOERRORS pIOErrors;

   /* TODO: implement it */
   ZH_SYMBOL_UNUSED( uiOperation );

   if( fResult )
      uiOsErrorLast = uiErrorLast = 0;
   else
   {
#if defined( ZH_OS_WIN )
      DWORD dwLastError = GetLastError();
      uiOsErrorLast = ( ZH_ERRCODE ) dwLastError;
      uiErrorLast = zh_WinToDosError( dwLastError );
#else
      int iErrCode = errno;
      uiOsErrorLast = iErrCode;
      uiErrorLast = zh_errnoToDosError( iErrCode );
#endif
   }

   /* ZH_TRACE() message is intentionally here to not overwrite
    * OS error code processed above.
    */
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fsSetIOError(%d,%hu)", fResult, uiOperation ) );

   pIOErrors = zh_stackIOErrors();
   pIOErrors->uiOsErrorLast = uiOsErrorLast;
   pIOErrors->uiErrorLast = uiErrorLast;
}
