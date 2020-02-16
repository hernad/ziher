/*
 * Mouse API
 *
 * Copyright 1999-2001 Viktor Szakats
 * Copyright 1999 Jose Lalin <dezac@corevia.com> (API proposal)
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
#include "zh_gt_core.h"

/* NOTE: Mouse initialization is called directly from low-level GT driver
 * because it possible that mouse subsystem can depend on the terminal
 * (for example, mouse subsystem cannot be initialized before ncurses
 * driver is initialized).
 */
/* C callable interface */

ZH_BOOL zh_mouseIsPresent( void )
{
   ZH_BOOL fPresent = ZH_FALSE;
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_mouseIsPresent()" ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      fPresent = ZH_GTSELF_MOUSEISPRESENT( pGT );
      zh_gt_BaseFree( pGT );
   }
   return fPresent;
}

ZH_BOOL zh_mouseGetCursor( void )
{
   ZH_BOOL fVisible = ZH_FALSE;
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_mouseGetCursor()" ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      fVisible = ZH_GTSELF_MOUSEGETCURSOR( pGT );
      zh_gt_BaseFree( pGT );
   }
   return fVisible;
}

void zh_mouseSetCursor( ZH_BOOL fVisible )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_mouseSetCursor(%d)", ( int ) fVisible ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_MOUSESETCURSOR( pGT, fVisible );
      zh_gt_BaseFree( pGT );
   }
}

int zh_mouseCol( void )
{
   int iCol = 0;
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_mouseCol()" ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      iCol = ZH_GTSELF_MOUSECOL( pGT );
      zh_gt_BaseFree( pGT );
   }
   return iCol;
}

int zh_mouseRow( void )
{
   int iRow = 0;
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_mouseRow()" ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      iRow = ZH_GTSELF_MOUSEROW( pGT );
      zh_gt_BaseFree( pGT );
   }
   return iRow;
}

void zh_mouseGetPos( int * piRow, int * piCol )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_mouseGetPos(%p, %p)", ( void * ) piRow, ( void * ) piCol ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_MOUSEGETPOS( pGT, piRow, piCol );
      zh_gt_BaseFree( pGT );
   }
}

void zh_mouseSetPos( int iRow, int iCol )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_mouseSetPos(%d, %d)", iRow, iCol ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_MOUSESETPOS( pGT, iRow, iCol );
      zh_gt_BaseFree( pGT );
   }
}

void zh_mouseSetBounds( int iTop, int iLeft, int iBottom, int iRight )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_mouseSetBounds(%d, %d, %d, %d)", iTop, iLeft, iBottom, iRight ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_MOUSESETBOUNDS( pGT, iTop, iLeft, iBottom, iRight );
      zh_gt_BaseFree( pGT );
   }
}

void zh_mouseGetBounds( int * piTop, int * piLeft, int * piBottom, int * piRight )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_mouseGetBounds(%p, %p, %p, %p)", ( void * ) piTop, ( void * ) piLeft, ( void * ) piBottom, ( void * ) piRight ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_MOUSEGETBOUNDS( pGT, piTop, piLeft, piBottom, piRight );
      zh_gt_BaseFree( pGT );
   }
}

int zh_mouseStorageSize( void )
{
   int iSize = 0;
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_mouseStorageSize()" ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      iSize = ZH_GTSELF_MOUSESTORAGESIZE( pGT );
      zh_gt_BaseFree( pGT );
   }
   return iSize;
}

void zh_mouseSaveState( void * pBuffer )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_mouseSaveState(%p)", pBuffer ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_MOUSESAVESTATE( pGT, pBuffer );
      zh_gt_BaseFree( pGT );
   }
}

void zh_mouseRestoreState( const void * pBuffer )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_mouseRestoreState(%p)", pBuffer ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_MOUSERESTORESTATE( pGT, pBuffer );
      zh_gt_BaseFree( pGT );
   }
}

int zh_mouseGetDoubleClickSpeed( void )
{
   int iSpeed = 0;
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_mouseGetDoubleClickSpeed()" ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      iSpeed = ZH_GTSELF_MOUSEGETDOUBLECLICKSPEED( pGT );
      zh_gt_BaseFree( pGT );
   }
   return iSpeed;
}

void zh_mouseSetDoubleClickSpeed( int iSpeed )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_mouseSetDoubleClickSpeed(%d)", iSpeed ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_MOUSESETDOUBLECLICKSPEED( pGT, iSpeed );
      zh_gt_BaseFree( pGT );
   }
}

int zh_mouseCountButton( void )
{
   int iButtons = 0;
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_mouseCountButton()" ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      iButtons = ZH_GTSELF_MOUSECOUNTBUTTON( pGT );
      zh_gt_BaseFree( pGT );
   }
   return iButtons;
}

ZH_BOOL zh_mouseButtonState( int iButton )
{
   ZH_BOOL fPressed = ZH_FALSE;
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_mouseButtonState(%d)", iButton ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      fPressed = ZH_GTSELF_MOUSEBUTTONSTATE( pGT, iButton );
      zh_gt_BaseFree( pGT );
   }
   return fPressed;
}

ZH_BOOL zh_mouseButtonPressed( int iButton, int * piRow, int * piCol )
{
   ZH_BOOL fPressed = ZH_FALSE;
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_mouseButtonPressed(%d,%p,%p)", iButton, ( void * ) piRow, ( void * ) piCol ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      fPressed = ZH_GTSELF_MOUSEBUTTONPRESSED( pGT, iButton, piRow, piCol );
      zh_gt_BaseFree( pGT );
   }
   return fPressed;
}

ZH_BOOL zh_mouseButtonReleased( int iButton, int * piRow, int * piCol )
{
   ZH_BOOL fReleased = ZH_FALSE;
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_mouseButtonReleased(%d,%p,%p)", iButton, ( void * ) piRow, ( void * ) piCol ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      fReleased = ZH_GTSELF_MOUSEBUTTONRELEASED( pGT, iButton, piRow, piCol );
      zh_gt_BaseFree( pGT );
   }
   return fReleased;
}

int zh_mouseReadKey( int iEventMask )
{
   int iKey = 0;
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_mouseReadKey(%d)", iEventMask ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      iKey = ZH_GTSELF_MOUSEREADKEY( pGT, iEventMask );
      zh_gt_BaseFree( pGT );
   }
   return iKey;
}
