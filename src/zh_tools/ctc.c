/*
 * CT3 general functions (C part)
 *
 * Copyright 2001 IntTec GmbH, Neunlindenstr 32, 79106 Freiburg, Germany
 *        Author: Martin Vogel <vogel@inttec.de>
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

#include "ct.h"
#include "ctmath.h"
#include "zh_vm.h"
#include "zh_stack.h"

/* throwing a CT-subsystem error without value substitution
   - function adapted from errorapi.c */
ZH_USHORT ct_error( ZH_USHORT uiSeverity, ZH_ERRCODE errGenCode, ZH_ERRCODE errSubCode,
                    const char * szDescription, const char * szOperation,
                    ZH_ERRCODE errOsCode, ZH_USHORT uiFlags, ZH_ULONG ulArgCount, ... )
{
   ZH_USHORT uiAction;
   PZH_ITEM pError;

   PZH_ITEM pArray;
   va_list va;
   ZH_ULONG ulArgPos;

   ZH_TRACE( ZH_TR_DEBUG, ( "ct_error(%hu, %d, %d, %s, %s, %d, %hu, %lu)",
                            uiSeverity, errGenCode, errSubCode, szDescription, szOperation, errOsCode, uiFlags, ulArgCount ) );

   pError = zh_errRT_New( uiSeverity, CT_SUBSYSTEM, errGenCode, errSubCode, szDescription, szOperation, errOsCode, uiFlags );

   /* Build the array from the passed arguments. */
   if( ulArgCount == 0 )
   {
      pArray = NULL;
   }
   else if( ulArgCount == ZH_ERR_ARGS_BASEPARAMS )
   {
      if( zh_pcount() == 0 )
         pArray = NULL;
      else
         pArray = zh_arrayBaseParams();
   }
   else if( ulArgCount == ZH_ERR_ARGS_SELFPARAMS )
   {
      pArray = zh_arraySelfParams();
   }
   else
   {
      pArray = zh_itemArrayNew( ulArgCount );

      va_start( va, ulArgCount );
      for( ulArgPos = 1; ulArgPos <= ulArgCount; ulArgPos++ )
      {
         zh_itemArrayPut( pArray, ulArgPos, va_arg( va, PZH_ITEM ) );
      }
      va_end( va );
   }

   if( pArray )
   {
      /* Assign the new array to the object data item. */
      zh_vmPushSymbol( zh_dynsymGetSymbol( "_ARGS" ) );
      zh_vmPush( pError );
      zh_vmPush( pArray );
      zh_vmSend( 1 );

      /* Release the Array. */
      zh_itemRelease( pArray );
   }

   /* launch error codeblock */
   uiAction = zh_errLaunch( pError );

   /* release error codeblock */
   zh_errRelease( pError );

   return uiAction;
}

/* throwing a CT-subsystem error with value substitution
   - function adapted from errorapi.c */
PZH_ITEM ct_error_subst( ZH_USHORT uiSeverity, ZH_ERRCODE errGenCode, ZH_ERRCODE errSubCode,
                         const char * szDescription, const char * szOperation,
                         ZH_ERRCODE errOsCode, ZH_USHORT uiFlags, ZH_ULONG ulArgCount, ... )
{
   PZH_ITEM pRetVal;
   PZH_ITEM pError;

   PZH_ITEM pArray;
   va_list va;
   ZH_ULONG ulArgPos;

   ZH_TRACE( ZH_TR_DEBUG, ( "ct_error_subst(%hu, %d, %d, %s, %s, %d, %hu, %lu)",
                            uiSeverity, errGenCode, errSubCode, szDescription, szOperation, errOsCode, uiFlags, ulArgCount ) );

   pError = zh_errRT_New_Subst( uiSeverity, CT_SUBSYSTEM, errGenCode, errSubCode, szDescription, szOperation, errOsCode, uiFlags );

   /* Build the array from the passed arguments. */
   if( ulArgCount == 0 )
   {
      pArray = NULL;
   }
   else if( ulArgCount == ZH_ERR_ARGS_BASEPARAMS )
   {
      if( zh_pcount() == 0 )
         pArray = NULL;
      else
         pArray = zh_arrayBaseParams();
   }
   else if( ulArgCount == ZH_ERR_ARGS_SELFPARAMS )
   {
      pArray = zh_arraySelfParams();
   }
   else
   {
      pArray = zh_itemArrayNew( ulArgCount );

      va_start( va, ulArgCount );
      for( ulArgPos = 1; ulArgPos <= ulArgCount; ulArgPos++ )
      {
         zh_itemArrayPut( pArray, ulArgPos, va_arg( va, PZH_ITEM ) );
      }
      va_end( va );
   }

   if( pArray )
   {
      /* Assign the new array to the object data item. */
      zh_vmPushSymbol( zh_dynsymGetSymbol( "_ARGS" ) );
      zh_vmPush( pError );
      zh_vmPush( pArray );
      zh_vmSend( 1 );

      /* Release the Array. */
      zh_itemRelease( pArray );
   }

   /* launch error codeblock */
   pRetVal = zh_errLaunchSubst( pError );
   zh_errRelease( pError );

   return pRetVal;
}

/* argument error behaviour */
static void s_iArgErrMode_init( void * cargo )
{
   int * iArgErrMode = ( int * ) cargo;

   *iArgErrMode = CT_ARGERR_IGNORE;
}

static ZH_TSD_NEW( s_iArgErrMode, sizeof( int ), s_iArgErrMode_init, NULL );

void ct_setargerrormode( int iMode )
{
   int * iArgErrMode = ( int * ) zh_stackGetTSD( &s_iArgErrMode );

   ZH_TRACE( ZH_TR_DEBUG, ( "ct_setargerrormode(%i)", iMode ) );

   *iArgErrMode = iMode;
}

int ct_getargerrormode( void )
{
   int * iArgErrMode = ( int * ) zh_stackGetTSD( &s_iArgErrMode );

   ZH_TRACE( ZH_TR_DEBUG, ( "ct_getargerrormode()" ) );

   return *iArgErrMode;
}

ZH_FUNC( CSETARGERR )  /* ZH_EXTENSION */
{
   zh_retni( ct_getargerrormode() );

   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      int iNewMode = zh_parni( 1 );

      if( iNewMode == CT_ARGERR_WHOCARES ||
          iNewMode == CT_ARGERR_WARNING ||
          iNewMode == CT_ARGERR_ERROR ||
          iNewMode == CT_ARGERR_CATASTROPHIC ||
          iNewMode == CT_ARGERR_IGNORE )
      {
         ct_setargerrormode( zh_parni( 1 ) );
      }
      else
      {
         int iArgErrorMode = ct_getargerrormode();

         if( iArgErrorMode != CT_ARGERR_IGNORE )
            ct_error( ( ZH_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_CSETARGERR,
                      NULL, ZH_ERR_FUNCNAME, 0, EF_CANDEFAULT, ZH_ERR_ARGS_BASEPARAMS );
      }
   }
   else if( zh_pcount() > 0 ) /* more than one param but not integer */
   {
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         ct_error( ( ZH_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_CSETARGERR, NULL,
                   ZH_ERR_FUNCNAME, 0, EF_CANDEFAULT, ZH_ERR_ARGS_BASEPARAMS );
   }
}

