/*
 * The Error API
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * Copyright 1999-2016 Viktor Szakats (DosError(), __errInHandler(), __errRT*(), zh_errLaunch*(), zh_err*Flags(), zh_errRT*())
 * Copyright 2007 Przemyslaw Czerpak (rewritten in C ERROR class and all zh_errGet*() and zh_errPut*() functions)
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
#include "zh_item_api.h"
#include "zh_error_api.h"
#include "zh_lang_api.h"
#include "zh_class_api.h"
#include "zh_fs_api.h"
#include "zh_vm.h"
#include "zh_stack.h"


#define ZH_ERROR_LAUNCH_MAX    8

/* Error class instance variables offsets */
#define ZH_TERROR_CARGO        1
#define ZH_TERROR_ARGS         2
#define ZH_TERROR_FLAGS        3
#define ZH_TERROR_DESCRIPTION  4
#define ZH_TERROR_FILENAME     5
#define ZH_TERROR_GENCODE      6
#define ZH_TERROR_OPERATION    7
#define ZH_TERROR_OSCODE       8
#define ZH_TERROR_SEVERITY     9
#define ZH_TERROR_SUBCODE      10
#define ZH_TERROR_SUBSYSTEM    11
#define ZH_TERROR_TRIES        12

#define ZH_TERROR_IVARCOUNT    12

ZH_FUNC_EXTERN( ERRORNEW );


static PZH_ITEM s_pError = NULL;

static ZH_SYMBOL s_symErrorNew = { "ERRORNEW", { ZH_FS_PUBLIC | ZH_FS_LOCAL }, { ZH_FUNCNAME( ERRORNEW ) }, NULL };

typedef struct
{
   PZH_ERROR_INFO errorHandler;
   PZH_ITEM       errorBlock;
   int            iLaunchCount;
   int            uiErrorDOS;    /* The value of DosError() */
} ZH_ERRDATA, * PZH_ERRDATA;

static void zh_errorDataRelease( void * Cargo )
{
   PZH_ERRDATA pErrData = ( PZH_ERRDATA ) Cargo;

   zh_itemRelease( pErrData->errorBlock );
}

static ZH_TSD_NEW( s_errData, sizeof( ZH_ERRDATA ), NULL, zh_errorDataRelease );


static ZH_BOOL zh_errGetNumCode( int * piValue, const char * szOperation )
{
   PZH_ITEM pItem = zh_param( 1, ZH_IT_NUMERIC );

   if( pItem )
      *piValue = zh_itemGetNI( pItem );
   else
   {
      pItem = zh_errRT_BASE_Subst( EG_ARG, 0, NULL, szOperation,
                                   ZH_ERR_ARGS_BASEPARAMS );
      if( ! pItem )
      {
         *piValue = 0;
         return ZH_FALSE;
      }

      if( ! ZH_IS_NUMERIC( pItem ) )
         zh_errInternal( ZH_EI_ERRRECFAILURE, NULL, NULL, NULL );

      *piValue = zh_itemGetNI( pItem );
      zh_itemRelease( pItem );
   }

   return ZH_TRUE;
}


ZH_FUNC_STATIC( CARGO )
{
   zh_itemReturn( zh_errGetCargo( zh_stackSelfItem() ) );
}

ZH_FUNC_STATIC( _CARGO )
{
   PZH_ITEM pItem = zh_param( 1, ZH_IT_ANY );

   if( pItem )
      zh_errPutCargo( zh_stackSelfItem(), pItem );

   zh_itemReturn( pItem );
}


ZH_FUNC_STATIC( ARGS )
{
   zh_itemReturn( zh_errGetArgs( zh_stackSelfItem() ) );
}

ZH_FUNC_STATIC( _ARGS )
{
   PZH_ITEM pItem = zh_param( 1, ZH_IT_ARRAY );

   if( pItem )
      zh_errPutArgsArray( zh_stackSelfItem(), pItem );

   zh_itemReturn( pItem );
}


ZH_FUNC_STATIC( CANDEFAULT )
{
   zh_retl( ( zh_errGetFlags( zh_stackSelfItem() ) & EF_CANDEFAULT ) != 0 );
}

ZH_FUNC_STATIC( _CANDEFAULT )
{
   if( ZH_ISLOG( 1 ) )
   {
      PZH_ITEM pError = zh_stackSelfItem();
      ZH_BOOL fCan = zh_parl( 1 );

      if( fCan )
         zh_errPutFlags( pError, ( ZH_USHORT ) ( zh_errGetFlags( pError ) | EF_CANDEFAULT ) );
      else
         zh_errPutFlags( pError, ( ZH_USHORT ) ( zh_errGetFlags( pError ) & ~EF_CANDEFAULT ) );

      zh_retl( fCan );
   }
}


ZH_FUNC_STATIC( CANRETRY )
{
   zh_retl( ( zh_errGetFlags( zh_stackSelfItem() ) & EF_CANRETRY ) != 0 );
}

ZH_FUNC_STATIC( _CANRETRY )
{
   if( ZH_ISLOG( 1 ) )
   {
      PZH_ITEM pError = zh_stackSelfItem();
      ZH_BOOL fCan = zh_parl( 1 );

      if( fCan )
         zh_errPutFlags( pError, ( ZH_USHORT ) ( zh_errGetFlags( pError ) | EF_CANRETRY ) );
      else
         zh_errPutFlags( pError, ( ZH_USHORT ) ( zh_errGetFlags( pError ) & ~EF_CANRETRY ) );

      zh_retl( fCan );
   }
}


ZH_FUNC_STATIC( CANSUBST )
{
   zh_retl( ( zh_errGetFlags( zh_stackSelfItem() ) & EF_CANSUBSTITUTE ) != 0 );
}

ZH_FUNC_STATIC( _CANSUBST )
{
   if( ZH_ISLOG( 1 ) )
   {
      PZH_ITEM pError = zh_stackSelfItem();
      ZH_BOOL fCan = zh_parl( 1 );

      if( fCan )
         zh_errPutFlags( pError, ( ZH_USHORT ) ( zh_errGetFlags( pError ) | EF_CANSUBSTITUTE ) );
      else
         zh_errPutFlags( pError, ( ZH_USHORT ) ( zh_errGetFlags( pError ) & ~EF_CANSUBSTITUTE ) );

      zh_retl( fCan );
   }
}


ZH_FUNC_STATIC( DESCRIPTION )
{
   zh_retc( zh_errGetDescription( zh_stackSelfItem() ) );
}

ZH_FUNC_STATIC( _DESCRIPTION )
{
   PZH_ITEM pItem = zh_param( 1, ZH_IT_ANY );

   if( pItem && ZH_IS_STRING( pItem ) )
      zh_errPutDescription( zh_stackSelfItem(), zh_itemGetCPtr( pItem ) );

   zh_itemReturn( pItem );
}


ZH_FUNC_STATIC( FILENAME )
{
   zh_retc( zh_errGetFileName( zh_stackSelfItem() ) );
}

ZH_FUNC_STATIC( _FILENAME )
{
   PZH_ITEM pItem = zh_param( 1, ZH_IT_ANY );

   if( pItem && ZH_IS_STRING( pItem ) )
      zh_errPutFileName( zh_stackSelfItem(), zh_itemGetCPtr( pItem ) );

   zh_itemReturn( pItem );
}


ZH_FUNC_STATIC( OPERATION )
{
   zh_retc( zh_errGetOperation( zh_stackSelfItem() ) );
}

ZH_FUNC_STATIC( _OPERATION )
{
   PZH_ITEM pItem = zh_param( 1, ZH_IT_ANY );

   if( pItem && ZH_IS_STRING( pItem ) )
      zh_errPutOperation( zh_stackSelfItem(), zh_itemGetCPtr( pItem ) );

   zh_itemReturn( pItem );
}


ZH_FUNC_STATIC( SUBSYSTEM )
{
   zh_retc( zh_errGetSubSystem( zh_stackSelfItem() ) );
}

ZH_FUNC_STATIC( _SUBSYSTEM )
{
   PZH_ITEM pItem = zh_param( 1, ZH_IT_ANY );

   if( pItem && ZH_IS_STRING( pItem ) )
      zh_errPutSubSystem( zh_stackSelfItem(), zh_itemGetCPtr( pItem ) );

   zh_itemReturn( pItem );
}


ZH_FUNC_STATIC( GENCODE )
{
   zh_retni( zh_errGetGenCode( zh_stackSelfItem() ) );
}

ZH_FUNC_STATIC( _GENCODE )
{
   int iValue;

   if( zh_errGetNumCode( &iValue, "GENCODE" ) )
   {
      zh_errPutGenCode( zh_stackSelfItem(), ( ZH_ERRCODE ) iValue );
      zh_errPutDescription( zh_stackSelfItem(),
                            zh_langDGetErrorDesc( iValue ) );
   }

   zh_retni( iValue );
}


ZH_FUNC_STATIC( OSCODE )
{
   zh_retni( zh_errGetOsCode( zh_stackSelfItem() ) );
}

ZH_FUNC_STATIC( _OSCODE )
{
   int iValue;

   if( zh_errGetNumCode( &iValue, "OSCODE" ) )
      zh_errPutOsCode( zh_stackSelfItem(), ( ZH_ERRCODE ) iValue );

   zh_retni( iValue );
}


ZH_FUNC_STATIC( SUBCODE )
{
   zh_retni( zh_errGetSubCode( zh_stackSelfItem() ) );
}

ZH_FUNC_STATIC( _SUBCODE )
{
   int iValue;

   if( zh_errGetNumCode( &iValue, "SUBCODE" ) )
      zh_errPutSubCode( zh_stackSelfItem(), ( ZH_ERRCODE ) iValue );

   zh_retni( iValue );
}


ZH_FUNC_STATIC( SEVERITY )
{
   zh_retni( zh_errGetSeverity( zh_stackSelfItem() ) );
}

ZH_FUNC_STATIC( _SEVERITY )
{
   int iValue;

   if( zh_errGetNumCode( &iValue, "SEVERITY" ) )
      zh_errPutSeverity( zh_stackSelfItem(), ( ZH_USHORT ) iValue );

   zh_retni( iValue );
}


ZH_FUNC_STATIC( TRIES )
{
   zh_retni( zh_errGetTries( zh_stackSelfItem() ) );
}

ZH_FUNC_STATIC( _TRIES )
{
   int iValue;

   if( zh_errGetNumCode( &iValue, "TRIES" ) )
      zh_errPutTries( zh_stackSelfItem(), ( ZH_USHORT ) iValue );

   zh_retni( iValue );
}


static ZH_USHORT zh_errClassCreate( void )
{
   ZH_USHORT usClassH = zh_clsCreate( ZH_TERROR_IVARCOUNT, "ERROR" );

   zh_clsAdd( usClassH, "ARGS"          , ZH_FUNCNAME( ARGS )         );
   zh_clsAdd( usClassH, "_ARGS"         , ZH_FUNCNAME( _ARGS )        );
   zh_clsAdd( usClassH, "CANDEFAULT"    , ZH_FUNCNAME( CANDEFAULT )   );
   zh_clsAdd( usClassH, "_CANDEFAULT"   , ZH_FUNCNAME( _CANDEFAULT )  );
   zh_clsAdd( usClassH, "CANRETRY"      , ZH_FUNCNAME( CANRETRY )     );
   zh_clsAdd( usClassH, "_CANRETRY"     , ZH_FUNCNAME( _CANRETRY )    );
   zh_clsAdd( usClassH, "CANSUBSTITUTE" , ZH_FUNCNAME( CANSUBST )     );
   zh_clsAdd( usClassH, "_CANSUBSTITUTE", ZH_FUNCNAME( _CANSUBST )    );
   zh_clsAdd( usClassH, "CARGO"         , ZH_FUNCNAME( CARGO )        );
   zh_clsAdd( usClassH, "_CARGO"        , ZH_FUNCNAME( _CARGO )       );
   zh_clsAdd( usClassH, "DESCRIPTION"   , ZH_FUNCNAME( DESCRIPTION )  );
   zh_clsAdd( usClassH, "_DESCRIPTION"  , ZH_FUNCNAME( _DESCRIPTION ) );
   zh_clsAdd( usClassH, "FILENAME"      , ZH_FUNCNAME( FILENAME )     );
   zh_clsAdd( usClassH, "_FILENAME"     , ZH_FUNCNAME( _FILENAME )    );
   zh_clsAdd( usClassH, "GENCODE"       , ZH_FUNCNAME( GENCODE )      );
   zh_clsAdd( usClassH, "_GENCODE"      , ZH_FUNCNAME( _GENCODE )     );
   zh_clsAdd( usClassH, "OPERATION"     , ZH_FUNCNAME( OPERATION )    );
   zh_clsAdd( usClassH, "_OPERATION"    , ZH_FUNCNAME( _OPERATION )   );
   zh_clsAdd( usClassH, "OSCODE"        , ZH_FUNCNAME( OSCODE )       );
   zh_clsAdd( usClassH, "_OSCODE"       , ZH_FUNCNAME( _OSCODE )      );
   zh_clsAdd( usClassH, "SEVERITY"      , ZH_FUNCNAME( SEVERITY )     );
   zh_clsAdd( usClassH, "_SEVERITY"     , ZH_FUNCNAME( _SEVERITY )    );
   zh_clsAdd( usClassH, "SUBCODE"       , ZH_FUNCNAME( SUBCODE )      );
   zh_clsAdd( usClassH, "_SUBCODE"      , ZH_FUNCNAME( _SUBCODE )     );
   zh_clsAdd( usClassH, "SUBSYSTEM"     , ZH_FUNCNAME( SUBSYSTEM )    );
   zh_clsAdd( usClassH, "_SUBSYSTEM"    , ZH_FUNCNAME( _SUBSYSTEM )   );
   zh_clsAdd( usClassH, "TRIES"         , ZH_FUNCNAME( TRIES )        );
   zh_clsAdd( usClassH, "_TRIES"        , ZH_FUNCNAME( _TRIES )       );

   return usClassH;
}

ZH_FUNC( ERRORNEW )
{
   zh_itemReturnRelease( zh_errNew() );
}


ZH_FUNC( __ERRINHANDLER )
{
   zh_errInternal( ZH_EI_ERRRECFAILURE, NULL, NULL, NULL );
}

ZH_FUNC( ERRORBLOCK )
{
   PZH_ITEM pNewErrorBlock = zh_param( 1, ZH_IT_EVALITEM );
   PZH_ITEM pErrorBlock = zh_errorBlock();

   zh_itemReturn( pErrorBlock );
   if( pNewErrorBlock )
   {
      zh_itemCopy( pErrorBlock, pNewErrorBlock );
   }
}

PZH_ITEM zh_errorBlock( void )
{
   PZH_ERRDATA pErrData = ( PZH_ERRDATA ) zh_stackGetTSD( &s_errData );

   if( ! pErrData->errorBlock )
      pErrData->errorBlock = zh_itemNew( NULL );

   return pErrData->errorBlock;
}

/* set new low-level error launcher (C function) and return
 * handler currently active
 */
PZH_ERROR_INFO zh_errorHandler( PZH_ERROR_INFO pNewHandler )
{
   PZH_ERRDATA pErrData = ( PZH_ERRDATA ) zh_stackGetTSD( &s_errData );
   PZH_ERROR_INFO pOld = pErrData->errorHandler;

   if( pNewHandler )
      pNewHandler->Previous = pErrData->errorHandler;
   pErrData->errorHandler = pNewHandler;

   return pOld;
}

ZH_FUNC( DOSERROR )
{
   PZH_ERRDATA pErrData = ( PZH_ERRDATA ) zh_stackGetTSD( &s_errData );

   zh_retni( pErrData->uiErrorDOS );

   if( ZH_IS_PARAM_NUM( 1 ) )
      pErrData->uiErrorDOS = zh_parni( 1 );
}

void zh_errInit( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_errInit()" ) );

   /* error function */
   zh_dynsymNew( &s_symErrorNew );

   /* Create error class and base object */
   s_pError = zh_itemNew( NULL );
   zh_clsAssociate( zh_errClassCreate() );
   zh_itemMove( s_pError, zh_stackReturnItem() );
}

void zh_errExit( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_errExit()" ) );

   zh_itemRelease( s_pError );
   s_pError = NULL;
}

PZH_ITEM zh_errNew( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_errNew()" ) );

   if( ! s_pError || ! ZH_IS_OBJECT( s_pError ) )
      zh_errInternal( ZH_EI_ERRRECFAILURE, NULL, NULL, NULL );

   return zh_arrayClone( s_pError );
}

ZH_USHORT zh_errLaunch( PZH_ITEM pError )
{
   ZH_USHORT uiAction = E_DEFAULT; /* Needed to avoid GCC -O2 warning */

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_errLaunch(%p)", pError ) );

   if( pError )
   {
      PZH_ERRDATA pErrData = ( PZH_ERRDATA ) zh_stackGetTSD( &s_errData );
      ZH_USHORT uiFlags = zh_errGetFlags( pError );
      PZH_ITEM pResult;

      /* Check if we have a valid error handler */
      if( ! pErrData->errorBlock || ! ZH_IS_EVALITEM( pErrData->errorBlock ) )
         zh_errInternal( ZH_EI_ERRNOBLOCK, NULL, NULL, NULL );

      /* Check if the error launcher was called too many times recursively */
      if( pErrData->iLaunchCount == ZH_ERROR_LAUNCH_MAX )
         zh_errInternal( ZH_EI_ERRTOOMANY, NULL, NULL, NULL );

      /* Launch the error handler: "lResult := Eval( ErrorBlock(), oError )" */
      pErrData->iLaunchCount++;

      /* set DosError() to last OS error code */
      pErrData->uiErrorDOS = ( int ) zh_errGetOsCode( pError );

      /* Add one try to the counter. */
      if( uiFlags & EF_CANRETRY )
         zh_errPutTries( pError, ( ZH_USHORT ) ( zh_errGetTries( pError ) + 1 ) );

      if( pErrData->errorHandler )
      {
         /* there is a low-level error handler defined - use it instead
          * of normal Ziher level one
          */
         pErrData->errorHandler->Error = pError;
         pErrData->errorHandler->ErrorBlock = pErrData->errorBlock;
         pResult = ( pErrData->errorHandler->Func )( pErrData->errorHandler );
         pErrData->errorHandler->Error = NULL;
      }
      else
         pResult = zh_itemDo( pErrData->errorBlock, 1, pError );

      pErrData->iLaunchCount--;

      /* Check results */
      if( zh_vmRequestQuery() != 0 )
      {
         if( pResult )
            zh_itemRelease( pResult );
         uiAction = E_BREAK;
      }
      else if( pResult )
      {
         ZH_BOOL bFailure = ZH_FALSE;

         /* If the error block didn't return a logical value, */
         /* or the canSubstitute flag has been set, consider it as a failure */
         if( ! ZH_IS_LOGICAL( pResult ) || ( uiFlags & EF_CANSUBSTITUTE ) )
            bFailure = ZH_TRUE;
         else
         {
            uiAction = zh_itemGetL( pResult ) ? E_RETRY : E_DEFAULT;

            if( ( uiAction == E_DEFAULT && !( uiFlags & EF_CANDEFAULT ) ) ||
                ( uiAction == E_RETRY   && !( uiFlags & EF_CANRETRY ) ) )
               bFailure = ZH_TRUE;
         }

         zh_itemRelease( pResult );

         if( bFailure )
            zh_errInternal( ZH_EI_ERRRECFAILURE, NULL, NULL, NULL );

      }
      else
         zh_errInternal( ZH_EI_ERRRECFAILURE, NULL, NULL, NULL );
   }
   else
      uiAction = E_RETRY;

   return uiAction;
}

/* This error launcher should be used in those situations, where the error
   handler is expected to return a value to be substituted as the result of
   a failed operation. [vszakats] */

/* NOTE: This should only be called when the EF_CANSUBSTITUTE flag was set
         Since it this case the error handler will return the value
         to be substituted. [vszakats] */

/* NOTE: The item pointer returned should be zh_itemRelease()-d by the
         caller if it was not NULL. [vszakats] */

PZH_ITEM zh_errLaunchSubst( PZH_ITEM pError )
{
   PZH_ITEM pResult;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_errLaunchSubst(%p)", pError ) );

   if( pError )
   {
      PZH_ERRDATA pErrData = ( PZH_ERRDATA ) zh_stackGetTSD( &s_errData );
      ZH_USHORT uiFlags = zh_errGetFlags( pError );

      /* Check if we have a valid error handler */
      if( ! pErrData->errorBlock || ! ZH_IS_EVALITEM( pErrData->errorBlock ) )
         zh_errInternal( ZH_EI_ERRNOBLOCK, NULL, NULL, NULL );

      /* Check if the error launcher was called too many times recursively */
      if( pErrData->iLaunchCount == ZH_ERROR_LAUNCH_MAX )
         zh_errInternal( ZH_EI_ERRTOOMANY, NULL, NULL, NULL );

      /* Launch the error handler: "xResult := Eval( ErrorBlock(), oError )" */
      pErrData->iLaunchCount++;

      /* set DosError() to last OS error code */
      pErrData->uiErrorDOS = ( int ) zh_errGetOsCode( pError );

      /* Add one try to the counter. */
      if( uiFlags & EF_CANRETRY )
         zh_errPutTries( pError, ( ZH_USHORT ) ( zh_errGetTries( pError ) + 1 ) );

      if( pErrData->errorHandler )
      {
         /* there is a low-level error handler defined - use it instead
          * of normal Ziher level one
          */
         pErrData->errorHandler->Error = pError;
         pErrData->errorHandler->ErrorBlock = pErrData->errorBlock;
         pResult = ( pErrData->errorHandler->Func )( pErrData->errorHandler );
         pErrData->errorHandler->Error = NULL;
      }
      else
         pResult = zh_itemDo( pErrData->errorBlock, 1, pError );

      pErrData->iLaunchCount--;

      /* Check results */
      if( zh_vmRequestQuery() != 0 )
      {
         if( pResult )
            zh_itemRelease( pResult );
         pResult = NULL;
      }
      else
      {
         /* If the canSubstitute flag has not been set,
            consider it as a failure. */
         if( ! ( uiFlags & EF_CANSUBSTITUTE ) )
            zh_errInternal( ZH_EI_ERRRECFAILURE, NULL, NULL, NULL );
      }
   }
   else
      pResult = zh_itemNew( NULL );

   return pResult;
}

void zh_errRelease( PZH_ITEM pError )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_errRelease(%p)", pError ) );

   /* NOTE: NULL pointer is checked by zh_itemRelease() [vszakats] */
   zh_itemRelease( pError );
}

PZH_ITEM zh_errGetCargo( PZH_ITEM pError )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_errGetCargo(%p)", pError ) );

   return zh_arrayGetItemPtr( pError, ZH_TERROR_CARGO );
}

PZH_ITEM zh_errPutCargo( PZH_ITEM pError, PZH_ITEM pCargo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_errPutCargo(%p, %p)", pError, pCargo ) );

   zh_arraySet( pError, ZH_TERROR_CARGO, pCargo );

   return pError;
}

PZH_ITEM zh_errGetArgs( PZH_ITEM pError )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_errGetArgs(%p)", pError ) );

   return zh_arrayGetItemPtr( pError, ZH_TERROR_ARGS );
}

PZH_ITEM zh_errPutArgsArray( PZH_ITEM pError, PZH_ITEM pArgs )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_errPutArgsArray(%p, %p)", pError, pArgs ) );

   zh_arraySet( pError, ZH_TERROR_ARGS, pArgs );

   return pError;
}

const char * zh_errGetDescription( PZH_ITEM pError )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_errGetDescription(%p)", pError ) );

   return zh_arrayGetCPtr( pError, ZH_TERROR_DESCRIPTION );
}

PZH_ITEM zh_errPutDescription( PZH_ITEM pError, const char * szDescription )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_errPutDescription(%p, %s)", pError, szDescription ) );

   zh_arraySetC( pError, ZH_TERROR_DESCRIPTION, szDescription );

   return pError;
}

const char * zh_errGetFileName( PZH_ITEM pError )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_errGetFileName(%p)", pError ) );

   return zh_arrayGetCPtr( pError, ZH_TERROR_FILENAME );
}

PZH_ITEM zh_errPutFileName( PZH_ITEM pError, const char * szFileName )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_errPutFileName(%p, %s)", pError, szFileName ) );

   zh_arraySetC( pError, ZH_TERROR_FILENAME, szFileName );

   return pError;
}

ZH_ERRCODE zh_errGetGenCode( PZH_ITEM pError )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_errGetGenCode(%p)", pError ) );

   return ( ZH_ERRCODE ) zh_arrayGetNI( pError, ZH_TERROR_GENCODE );
}

PZH_ITEM zh_errPutGenCode( PZH_ITEM pError, ZH_ERRCODE errGenCode )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_errPutGenCode(%p, %d)", pError, errGenCode ) );

   zh_arraySetNI( pError, ZH_TERROR_GENCODE, errGenCode );

   return pError;
}

const char * zh_errGetOperation( PZH_ITEM pError )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_errGetOperation(%p)", pError ) );

   return zh_arrayGetCPtr( pError, ZH_TERROR_OPERATION );
}

PZH_ITEM zh_errPutOperation( PZH_ITEM pError, const char * szOperation )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_errPutOperation(%p, %s)", pError, szOperation == ZH_ERR_FUNCNAME ? "ZH_ERR_FUNCNAME" : szOperation ) );

   if( szOperation == ZH_ERR_FUNCNAME )
   {
      PZH_SYMBOL pSym = zh_itemGetSymbol( zh_stackBaseItem() );
      if( pSym )
         szOperation = pSym->szName;
   }

   zh_arraySetC( pError, ZH_TERROR_OPERATION, szOperation );

   return pError;
}

ZH_ERRCODE zh_errGetOsCode( PZH_ITEM pError )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_errGetOsCode(%p)", pError ) );

   return ( ZH_ERRCODE ) zh_arrayGetNI( pError, ZH_TERROR_OSCODE );
}

PZH_ITEM zh_errPutOsCode( PZH_ITEM pError, ZH_ERRCODE errOsCode )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_errPutOsCode(%p, %d)", pError, errOsCode ) );

   zh_arraySetNI( pError, ZH_TERROR_OSCODE, errOsCode );

   return pError;
}

ZH_USHORT zh_errGetSeverity( PZH_ITEM pError )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_errGetSeverity(%p)", pError ) );

   return ( ZH_USHORT ) zh_arrayGetNI( pError, ZH_TERROR_SEVERITY );
}

PZH_ITEM zh_errPutSeverity( PZH_ITEM pError, ZH_USHORT uiSeverity )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_errPutSeverity(%p, %hu)", pError, uiSeverity ) );

   zh_arraySetNI( pError, ZH_TERROR_SEVERITY, uiSeverity );

   return pError;
}

ZH_ERRCODE zh_errGetSubCode( PZH_ITEM pError )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_errGetSubCode(%p)", pError ) );

   return ( ZH_ERRCODE ) zh_arrayGetNI( pError, ZH_TERROR_SUBCODE );
}

PZH_ITEM zh_errPutSubCode( PZH_ITEM pError, ZH_ERRCODE errSubCode )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_errPutSubCode(%p, %d)", pError, errSubCode ) );

   zh_arraySetNI( pError, ZH_TERROR_SUBCODE, errSubCode );

   return pError;
}

const char * zh_errGetSubSystem( PZH_ITEM pError )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_errGetSubSytem(%p)", pError ) );

   return zh_arrayGetCPtr( pError, ZH_TERROR_SUBSYSTEM );
}

PZH_ITEM zh_errPutSubSystem( PZH_ITEM pError, const char * szSubSystem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_errPutSubSytem(%p, %s)", pError, szSubSystem ) );

   zh_arraySetC( pError, ZH_TERROR_SUBSYSTEM, szSubSystem );

   return pError;
}

ZH_USHORT zh_errGetTries( PZH_ITEM pError )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_errGetTries(%p)", pError ) );

   return ( ZH_USHORT ) zh_arrayGetNI( pError, ZH_TERROR_TRIES );
}

PZH_ITEM zh_errPutTries( PZH_ITEM pError, ZH_USHORT uiTries )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_errPutTries(%p, %hu)", pError, uiTries ) );

   zh_arraySetNI( pError, ZH_TERROR_TRIES, uiTries );

   return pError;
}

ZH_USHORT zh_errGetFlags( PZH_ITEM pError )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_errGetFlags(%p)", pError ) );

   return ( ZH_USHORT ) zh_arrayGetNI( pError, ZH_TERROR_FLAGS );
}

PZH_ITEM zh_errPutFlags( PZH_ITEM pError, ZH_USHORT uiFlags )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_errPutFlags(%p, %hu)", pError, uiFlags ) );

   uiFlags &= EF_CANRETRY | EF_CANSUBSTITUTE | EF_CANDEFAULT;
   zh_arraySetNI( pError, ZH_TERROR_FLAGS, uiFlags );

   return pError;
}

PZH_ITEM zh_errPutArgs( PZH_ITEM pError, ZH_ULONG ulArgCount, ... )
{
   PZH_ITEM pArray;
   ZH_ULONG ulArgPos;
   va_list va;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_errPutArgs(%p, %lu, ...)", pError, ulArgCount ) );

   pArray = zh_itemArrayNew( ulArgCount );

   /* Build the array from the passed arguments. */

   va_start( va, ulArgCount );
   for( ulArgPos = 1; ulArgPos <= ulArgCount; ulArgPos++ )
      zh_itemArrayPut( pArray, ulArgPos, va_arg( va, PZH_ITEM ) );
   va_end( va );

   /* Assign the new array to the object data item. */
   zh_errPutArgsArray( pError, pArray );

   /* Release the Array. */
   zh_itemRelease( pArray );

   return pError;
}

/* Wrappers for zh_errLaunch() */

PZH_ITEM zh_errRT_New(
   ZH_USHORT uiSeverity,
   const char * szSubSystem,
   ZH_ERRCODE errGenCode,
   ZH_ERRCODE errSubCode,
   const char * szDescription,
   const char * szOperation,
   ZH_ERRCODE errOsCode,
   ZH_USHORT uiFlags )
{
   PZH_ITEM pError = zh_errNew();

   zh_errPutSeverity( pError, uiSeverity );
   zh_errPutSubSystem( pError, szSubSystem ? szSubSystem : ZH_ERR_SS_BASE );
   zh_errPutGenCode( pError, errGenCode );
   zh_errPutSubCode( pError, errSubCode );
   zh_errPutDescription( pError, szDescription ? szDescription : zh_langDGetItem( ZH_LANG_ITEM_BASE_ERRDESC + errGenCode ) );
   zh_errPutOperation( pError, szOperation );
   zh_errPutOsCode( pError, errOsCode );
   zh_errPutFlags( pError, uiFlags );

   return pError;
}

PZH_ITEM zh_errRT_New_Subst(
   ZH_USHORT uiSeverity,
   const char * szSubSystem,
   ZH_ERRCODE errGenCode,
   ZH_ERRCODE errSubCode,
   const char * szDescription,
   const char * szOperation,
   ZH_ERRCODE errOsCode,
   ZH_USHORT uiFlags )
{
   PZH_ITEM pError = zh_errNew();

   zh_errPutSeverity( pError, uiSeverity );
   zh_errPutSubSystem( pError, szSubSystem ? szSubSystem : ZH_ERR_SS_BASE );
   zh_errPutGenCode( pError, errGenCode );
   zh_errPutSubCode( pError, errSubCode );
   zh_errPutDescription( pError, szDescription ? szDescription : zh_langDGetItem( ZH_LANG_ITEM_BASE_ERRDESC + errGenCode ) );
   zh_errPutOperation( pError, szOperation );
   zh_errPutOsCode( pError, errOsCode );
   zh_errPutFlags( pError, ( ZH_USHORT ) ( uiFlags | EF_CANSUBSTITUTE ) );

   return pError;
}

PZH_ITEM zh_errRT_SubstParams( const char * szSubSystem, ZH_ERRCODE errGenCode, ZH_ERRCODE errSubCode, const char * szDescription, const char * szOperation )
{
   PZH_ITEM pRetVal;
   PZH_ITEM pError;
   PZH_ITEM pArray;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_errRT_SubstParams()" ) );

   pError = zh_errRT_New_Subst( ES_ERROR, szSubSystem ? szSubSystem : ZH_ERR_SS_BASE,
               errGenCode, errSubCode, szDescription, szOperation, 0, EF_NONE );

   pArray = zh_arrayBaseParams();

   /* Assign the new array to the object data item. */
   zh_errPutArgsArray( pError, pArray );

   /* Release the Array. */
   zh_itemRelease( pArray );

   /* Ok, launch... */
   pRetVal = zh_errLaunchSubst( pError );

   zh_itemRelease( pError );

   return pRetVal;
}

PZH_ITEM zh_errRT_FileError( PZH_ITEM pError, const char * szSubSystem,
                             ZH_ERRCODE errGenCode, ZH_ERRCODE errSubCode,
                             const char * szFileName )
{
   if( ! pError )
   {
      pError = zh_errNew();
      zh_errPutSeverity( pError, ES_ERROR );
      zh_errPutSubSystem( pError, szSubSystem ? szSubSystem : ZH_ERR_SS_BASE );
      zh_errPutFlags( pError, EF_CANRETRY | EF_CANDEFAULT );
      zh_errPutFileName( pError, szFileName );
   }
   zh_errPutGenCode( pError, errGenCode );
   zh_errPutDescription( pError, zh_langDGetErrorDesc( errGenCode ) );
   zh_errPutSubCode( pError, errSubCode );
   zh_errPutOsCode( pError, zh_fsError() );

   return pError;
}

ZH_FUNC( __ERRRT_BASE )
{
   zh_errRT_BASE( ( ZH_ERRCODE ) zh_parni( 1 ),
                  ( ZH_ERRCODE ) zh_parni( 2 ),
                  zh_parc( 3 ),
                  zh_parc( 4 ),
                  ( zh_pcount() > 5 && zh_parnl( 5 ) > 0 ? 1 : 0 ),
                  zh_param( 6, ZH_IT_ANY ) );
}

ZH_FUNC( __ERRRT_SBASE )
{
   zh_errRT_BASE_SubstR( ( ZH_ERRCODE ) zh_parni( 1 ),
                         ( ZH_ERRCODE ) zh_parni( 2 ),
                         zh_parc( 3 ),
                         zh_parc( 4 ),
                         ( zh_pcount() > 5 && zh_parnl( 5 ) > 0 ? 1 : 0 ),
                         zh_param( 6, ZH_IT_ANY ) );
}

ZH_USHORT zh_errRT_BASE( ZH_ERRCODE errGenCode, ZH_ERRCODE errSubCode, const char * szDescription, const char * szOperation, ZH_ULONG ulArgCount, ... )
{
   ZH_USHORT uiAction;
   PZH_ITEM pError;

   PZH_ITEM pArray;
   va_list va;
   ZH_ULONG ulArgPos;

   pError = zh_errRT_New( ES_ERROR, ZH_ERR_SS_BASE, errGenCode, errSubCode, szDescription, szOperation, 0, EF_NONE /* EF_CANRETRY */ );

   /* Build the array from the passed arguments. */
   switch( ulArgCount )
   {
      case 0:
         pArray = NULL;
         break;

      case ZH_ERR_ARGS_BASEPARAMS:
         pArray = zh_pcount() ? zh_arrayBaseParams() : NULL;
         break;

      case ZH_ERR_ARGS_SELFPARAMS:
         pArray = zh_arraySelfParams();
         break;

      default:
         pArray = zh_itemArrayNew( ulArgCount );

         va_start( va, ulArgCount );
         for( ulArgPos = 1; ulArgPos <= ulArgCount; ulArgPos++ )
         {
            PZH_ITEM pArg = va_arg( va, PZH_ITEM );
            if( pArg )
               zh_itemArrayPut( pArray, ulArgPos, pArg );
         }
         va_end( va );
   }

   if( pArray )
   {
      /* Assign the new array to the object data item. */
      zh_errPutArgsArray( pError, pArray );

      /* Release the Array. */
      zh_itemRelease( pArray );
   }

   /* Ok, launch... */
   uiAction = zh_errLaunch( pError );

   /* Release. */
   zh_errRelease( pError );

   return uiAction;
}

ZH_USHORT zh_errRT_BASE_Ext1( ZH_ERRCODE errGenCode, ZH_ERRCODE errSubCode, const char * szDescription, const char * szOperation, ZH_ERRCODE errOsCode, ZH_USHORT uiFlags, ZH_ULONG ulArgCount, ... )
{
   ZH_USHORT uiAction;
   PZH_ITEM pError;

   PZH_ITEM pArray;
   va_list va;
   ZH_ULONG ulArgPos;

   pError = zh_errRT_New( ES_ERROR, ZH_ERR_SS_BASE, errGenCode, errSubCode, szDescription, szOperation, errOsCode, uiFlags );

   /* Build the array from the passed arguments. */
   switch( ulArgCount )
   {
      case 0:
         pArray = NULL;
         break;

      case ZH_ERR_ARGS_BASEPARAMS:
         pArray = zh_pcount() ? zh_arrayBaseParams() : NULL;
         break;

      case ZH_ERR_ARGS_SELFPARAMS:
         pArray = zh_arraySelfParams();
         break;

      default:
         pArray = zh_itemArrayNew( ulArgCount );

         va_start( va, ulArgCount );
         for( ulArgPos = 1; ulArgPos <= ulArgCount; ulArgPos++ )
         {
            PZH_ITEM pArg = va_arg( va, PZH_ITEM );
            if( pArg )
               zh_itemArrayPut( pArray, ulArgPos, pArg );
         }
         va_end( va );
   }

   if( pArray )
   {
      /* Assign the new array to the object data item. */
      zh_errPutArgsArray( pError, pArray );

      /* Release the Array. */
      zh_itemRelease( pArray );
   }

   /* Ok, launch... */
   uiAction = zh_errLaunch( pError );

   zh_errRelease( pError );

   return uiAction;
}

PZH_ITEM zh_errRT_BASE_Subst( ZH_ERRCODE errGenCode, ZH_ERRCODE errSubCode, const char * szDescription, const char * szOperation, ZH_ULONG ulArgCount, ... )
{
   PZH_ITEM pRetVal;
   PZH_ITEM pError;

   PZH_ITEM pArray;
   va_list va;
   ZH_ULONG ulArgPos;

   pError = zh_errRT_New_Subst( ES_ERROR, ZH_ERR_SS_BASE, errGenCode, errSubCode, szDescription, szOperation, 0, EF_NONE );

   /* Build the array from the passed arguments. */
   switch( ulArgCount )
   {
      case 0:
         pArray = NULL;
         break;

      case ZH_ERR_ARGS_BASEPARAMS:
         pArray = zh_pcount() ? zh_arrayBaseParams() : NULL;
         break;

      case ZH_ERR_ARGS_SELFPARAMS:
         pArray = zh_arraySelfParams();
         break;

      default:
         pArray = zh_itemArrayNew( ulArgCount );

         va_start( va, ulArgCount );
         for( ulArgPos = 1; ulArgPos <= ulArgCount; ulArgPos++ )
         {
            PZH_ITEM pArg = va_arg( va, PZH_ITEM );
            if( pArg )
               zh_itemArrayPut( pArray, ulArgPos, pArg );
         }
         va_end( va );
   }

   if( pArray )
   {
      /* Assign the new array to the object data item. */
      zh_errPutArgsArray( pError, pArray );

      /* Release the Array. */
      zh_itemRelease( pArray );
   }

   /* Ok, launch... */
   pRetVal = zh_errLaunchSubst( pError );

   zh_errRelease( pError );

   return pRetVal;
}

void zh_errRT_BASE_SubstR( ZH_ERRCODE errGenCode, ZH_ERRCODE errSubCode, const char * szDescription, const char * szOperation, ZH_ULONG ulArgCount, ... )
{
   PZH_ITEM pError;

   PZH_ITEM pArray;
   va_list va;
   ZH_ULONG ulArgPos;

   pError = zh_errRT_New_Subst( ES_ERROR, ZH_ERR_SS_BASE, errGenCode, errSubCode, szDescription, szOperation, 0, EF_NONE );

   /* Build the array from the passed arguments. */
   switch( ulArgCount )
   {
      case 0:
         pArray = NULL;
         break;

      case ZH_ERR_ARGS_BASEPARAMS:
         pArray = zh_pcount() ? zh_arrayBaseParams() : NULL;
         break;

      case ZH_ERR_ARGS_SELFPARAMS:
         pArray = zh_arraySelfParams();
         break;

      default:
         pArray = zh_itemArrayNew( ulArgCount );

         va_start( va, ulArgCount );
         for( ulArgPos = 1; ulArgPos <= ulArgCount; ulArgPos++ )
         {
            PZH_ITEM pArg = va_arg( va, PZH_ITEM );
            if( pArg )
               zh_itemArrayPut( pArray, ulArgPos, pArg );
         }
         va_end( va );
   }

   if( pArray )
   {
      /* Assign the new array to the object data item. */
      zh_errPutArgsArray( pError, pArray );

      /* Release the Array. */
      zh_itemRelease( pArray );
   }

   /* Ok, launch... */
   zh_itemReturnRelease( zh_errLaunchSubst( pError ) );
   zh_errRelease( pError );
}

ZH_USHORT zh_errRT_TERM( ZH_ERRCODE errGenCode, ZH_ERRCODE errSubCode, const char * szDescription, const char * szOperation, ZH_ERRCODE errOsCode, ZH_USHORT uiFlags )
{
   ZH_USHORT uiAction;
   PZH_ITEM pError =
      zh_errRT_New( ES_ERROR, ZH_ERR_SS_TERMINAL, errGenCode, errSubCode, szDescription, szOperation, errOsCode, uiFlags );

   uiAction = zh_errLaunch( pError );

   zh_errRelease( pError );

   return uiAction;
}

ZH_USHORT zh_errRT_DBCMD( ZH_ERRCODE errGenCode, ZH_ERRCODE errSubCode, const char * szDescription, const char * szOperation )
{
   ZH_USHORT uiAction;
   PZH_ITEM pError =
      zh_errRT_New( ES_ERROR, ZH_ERR_SS_DBCMD, errGenCode, errSubCode, szDescription, szOperation, 0, EF_NONE );

   uiAction = zh_errLaunch( pError );

   zh_errRelease( pError );

   return uiAction;
}

ZH_USHORT zh_errRT_DBCMD_Ext( ZH_ERRCODE errGenCode, ZH_ERRCODE errSubCode, const char * szDescription, const char * szOperation, ZH_USHORT uiFlags )
{
   ZH_USHORT uiAction;
   PZH_ITEM pError;

   pError = zh_errRT_New( ES_ERROR, ZH_ERR_SS_DBCMD, errGenCode, errSubCode, szDescription, szOperation, 0, uiFlags );

   uiAction = zh_errLaunch( pError );

   zh_itemRelease( pError );

   return uiAction;
}
