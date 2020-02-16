/*
 * .zh interface to preprocessor
 *
 * Copyright 2006 Przemyslaw Czerpak
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
#include "zh_pp.h"
#include "zh_item_api.h"
#include "zh_apifs.h"
#include "zh_api_error.h"
#include "zh_vm.h"

ZH_EXTERN_BEGIN

static void zh_pp_ErrorMessage( void * cargo, const char * const szMsgTable[],
                                char cPrefix, int iCode,
                                const char * szParam1, const char * szParam2 )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_pp_ErrorGen(%p, %p, %c, %d, %s, %s)", cargo, ( const void * ) szMsgTable, cPrefix, iCode, szParam1, szParam2 ) );

   ZH_SYMBOL_UNUSED( cargo );

   /* ignore all warning messages and errors when break or quit request */
   if( cPrefix != 'W' && zh_vmRequestQuery() == 0 )
   {
      char szMsgBuf[ 1024 ];
      PZH_ITEM pError;
      zh_snprintf( szMsgBuf, sizeof( szMsgBuf ), szMsgTable[ iCode - 1 ],
                   szParam1, szParam2 );
      pError = zh_errRT_New( ES_ERROR, "PP", 1001, ( ZH_ERRCODE ) iCode, szMsgBuf,
                             NULL, 0, EF_NONE | EF_CANDEFAULT );
      zh_errLaunch( pError );
      zh_errRelease( pError );
   }
}

static void zh_pp_Disp( void * cargo, const char * szMessage )
{
   /* ignore stdout messages when PP used as library */
   ZH_SYMBOL_UNUSED( cargo );
   ZH_SYMBOL_UNUSED( szMessage );
}

static ZH_BOOL zh_pp_CompilerSwitch( void * cargo, const char * szSwitch,
                                     int * piValue, ZH_BOOL fSet )
{
   /* ignore all compiler switches */
   ZH_SYMBOL_UNUSED( cargo );
   ZH_SYMBOL_UNUSED( szSwitch );
   ZH_SYMBOL_UNUSED( piValue );
   ZH_SYMBOL_UNUSED( fSet );

   return ZH_FALSE;
}

/* PP destructor */
static ZH_GARBAGE_FUNC( zh_pp_Destructor )
{
   PZH_PP_STATE * pStatePtr = ( PZH_PP_STATE * ) Cargo;

   if( *pStatePtr )
   {
      zh_pp_free( *pStatePtr );
      *pStatePtr = NULL;
   }
}

ZH_EXTERN_END


static const ZH_GC_FUNCS s_gcPPFuncs =
{
   zh_pp_Destructor,
   zh_gcDummyMark
};

static void zh_pp_StdRules( PZH_ITEM ppItem )
{
   static ZH_BOOL s_fInit = ZH_TRUE;
   static PZH_DYNS s_pDynSym;

   if( s_fInit )
   {
      s_pDynSym = zh_dynsymFind( "__PP_STDRULES" );
      s_fInit = ZH_FALSE;
   }

   if( s_pDynSym )
   {
      zh_vmPushDynSym( s_pDynSym );
      zh_vmPushNil();
      zh_vmPush( ppItem );
      zh_vmProc( 1 );
   }
}

PZH_PP_STATE zh_pp_Param( int iParam )
{
   PZH_PP_STATE * pStatePtr =
      ( PZH_PP_STATE * ) zh_parptrGC( &s_gcPPFuncs, iParam );

   if( pStatePtr )
      return *pStatePtr;
   else
      return NULL;
}

/*
 * initialize new PP context and return pointer to it.
 * __pp_Init( [<cIncludePath>], [<cStdChFile> ] [, <lArchDefs>] ) --> <pPP>
 * when <cStdChFile> is empty string ("") then no default rules are used
 * only the dynamically created #defines like __ZIHER__, __DATE__, __TIME__
 */
ZH_FUNC( __PP_INIT )
{
   PZH_PP_STATE pState = zh_pp_new();

   if( pState )
   {
      PZH_PP_STATE * pStatePtr;
      const char * szPath = zh_parc( 1 ), * szStdCh = zh_parc( 2 );
      ZH_BOOL fArchDefs = zh_parldef( 3, ZH_TRUE );
      PZH_ITEM ppItem;

      pStatePtr = ( PZH_PP_STATE * ) zh_gcAllocate( sizeof( PZH_PP_STATE ),
                                                    &s_gcPPFuncs );
      *pStatePtr = pState;
      ppItem = zh_itemPutPtrGC( NULL, ( void * ) pStatePtr );

      zh_pp_init( pState, ZH_TRUE, ZH_FALSE, 0, NULL, NULL, NULL,
                  zh_pp_ErrorMessage, zh_pp_Disp, NULL, NULL,
                  zh_pp_CompilerSwitch );

      if( szPath )
         zh_pp_addSearchPath( pState, szPath, ZH_TRUE );

      if( ! szStdCh )
         zh_pp_StdRules( ppItem );
      else if( *szStdCh )
         zh_pp_readRules( pState, szStdCh );

      zh_pp_initDynDefines( pState, fArchDefs );
      zh_pp_setStdBase( pState );

      zh_itemReturnRelease( ppItem );
   }
   else
      zh_ret();
}

/*
 * add new (or replace previous) include paths.
 * __pp_Path( <pPP>, <cPath> [, <lClearPrev>] ) --> NIL
 */
ZH_FUNC( __PP_PATH )
{
   PZH_PP_STATE pState = zh_pp_Param( 1 );

   if( pState )
      zh_pp_addSearchPath( pState, zh_parc( 2 ), zh_parl( 3 ) );
}

/*
 * reset the PP context (remove all rules added by user or preprocessed code)
 * __pp_Reset( <pPP> ) --> NIL
 */
ZH_FUNC( __PP_RESET )
{
   PZH_PP_STATE pState = zh_pp_Param( 1 );

   if( pState )
      zh_pp_reset( pState );
}

/*
 * preprocess and execute new preprocessor directive
 * __pp_AddRule( <pPP>, <cDirective> ) --> <lOK>
 */
ZH_FUNC( __PP_ADDRULE )
{
   PZH_PP_STATE pState = zh_pp_Param( 1 );

   if( pState )
   {
      const char * szText = zh_parc( 2 );
      ZH_SIZE nLen = zh_parclen( 2 );

      if( szText )
      {
         while( nLen && ( szText[ 0 ] == ' ' || szText[ 0 ] == '\t' ) )
         {
            ++szText;
            --nLen;
         }
      }

      if( szText && nLen && szText[ 0 ] == '#' )
      {
         zh_pp_parseLine( pState, szText, &nLen );

         /* probably for parsing #included files the old code was making
            something like that */
         do
         {
            if( zh_vmRequestQuery() != 0 )
               return;
         }
         while( zh_pp_nextLine( pState, NULL ) );

         zh_retl( ZH_TRUE );
         return;
      }
   }
   zh_retl( ZH_FALSE );
}

/*
 * preprocess given code and return result
 * __pp_Process( <pPP>, <cCode> ) --> <cPreprocessedCode>
 */
ZH_FUNC( __PP_PROCESS )
{
   PZH_PP_STATE pState = zh_pp_Param( 1 );

   if( pState )
   {
      ZH_SIZE nLen = zh_parclen( 2 );

      if( nLen )
      {
         char * szText = zh_pp_parseLine( pState, zh_parc( 2 ), &nLen );
         zh_retclen( szText, nLen );
         return;
      }
   }

   zh_retc_null();
}
