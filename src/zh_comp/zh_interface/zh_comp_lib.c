/*
 * zh_compile*() - compiler interface
 *
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

#include "zh_comp.h"

static void s_pp_msg( void * cargo, int iErrorFmt, int iLine,
                      const char * szModule, char cPrefix, int iValue,
                      const char * szText,
                      const char * szPar1, const char * szPar2 )
{
   ZH_SYMBOL_UNUSED( cargo );

   /* ignore all warning messages and errors when break or quit request */
   if( cPrefix != 'W' && zh_vmRequestQuery() == 0 )
   {
      char szMsgBuf[ 512 ], szLine[ 512 ];
      PZH_ITEM pError;

      zh_snprintf( szMsgBuf, sizeof( szMsgBuf ), szText, szPar1, szPar2 );
      if( ! szModule || *szModule == 0 || strcmp( szModule, "{SOURCE}.zh" ) == 0 )
         zh_snprintf( szLine, sizeof( szLine ),
                      "line:%i", iLine );
      else
         zh_snprintf( szLine, sizeof( szLine ),
                      iErrorFmt == ZH_ERRORFMT_DEFAULT ? "%s(%i)" : "%s:%i",
                      szModule, iLine );
      pError = zh_errRT_New( ES_ERROR, "COMPILER", 1001, ( ZH_ERRCODE ) iValue,
                             szMsgBuf, szLine, 0 /*OsCode*/, EF_NONE );
      zh_errLaunch( pError );
      zh_errRelease( pError );
   }
}

static int s_pp_openFile( void * cargo, char * szFileName,
                          ZH_BOOL fBefore, ZH_BOOL fSysFile, ZH_BOOL fBinary,
                          ZH_PATHNAMES * pIncludePaths,
                          ZH_BOOL * pfNested, FILE ** file_ptr,
                          const char ** pBufPtr, ZH_SIZE * pnLen, ZH_BOOL * pfFree )
{
   ZH_SYMBOL_UNUSED( fSysFile );
   ZH_SYMBOL_UNUSED( fBinary );
   ZH_SYMBOL_UNUSED( pIncludePaths );
   ZH_SYMBOL_UNUSED( pfNested );
   ZH_SYMBOL_UNUSED( file_ptr );

   if( ! fBefore )
   {
      ZH_COMP_DECL = ( PZH_COMP ) cargo;
      PZH_ITEM pIncItem = ( PZH_ITEM ) ZH_COMP_PARAM->cargo;

      if( pIncItem )
      {
         if( ZH_IS_HASH( pIncItem ) )
         {
            PZH_ITEM pFileItem = zh_hashGetCItemPtr( pIncItem, szFileName );

            if( pFileItem )
            {
               ZH_SIZE nLen = zh_itemGetCLen( pFileItem );
               if( nLen )
               {
                  *pBufPtr = zh_itemGetCPtr( pFileItem );
                  *pnLen   = nLen;
                  *pfFree  = ZH_FALSE;
                  return ZH_PP_OPEN_OK;
               }
            }
         }
      }
   }

   return ZH_PP_OPEN_FILE;
}

static void zh_compGenArgList( int iFirst, int iLast,
                               int * pArgC, const char *** pArgV,
                               PZH_ITEM * pIncItem,
                               PZH_PP_OPEN_FUNC * pOpenFunc,
                               PZH_PP_MSG_FUNC * pMsgFunc )
{
   PZH_ITEM pParam;
   int argc = 1, i;
   const char ** argv;

   if( pMsgFunc )
   {
      *pMsgFunc = NULL;
      if( ZH_ISLOGICAL( iFirst ) )
      {
         if( zh_parl( iFirst ) )
            *pMsgFunc = s_pp_msg;
         ++iFirst;
      }
   }

   if( pIncItem && pOpenFunc )
   {
      *pOpenFunc = NULL;
      *pIncItem = zh_param( iFirst, ZH_IT_HASH );
      if( *pIncItem )
      {
         ++iFirst;
         *pOpenFunc = s_pp_openFile;
      }
   }

   for( i = iFirst; i <= iLast; ++i )
   {
      pParam = zh_param( i, ZH_IT_ARRAY | ZH_IT_STRING );
      if( pParam )
      {
         if( ZH_IS_ARRAY( pParam ) )
         {
            ZH_SIZE nPos = zh_arrayLen( pParam );
            if( nPos )
            {
               do
               {
                  if( zh_arrayGetType( pParam, nPos ) & ZH_IT_STRING )
                     ++argc;
               }
               while( --nPos );
            }
         }
         else if( ZH_IS_STRING( pParam ) )
            ++argc;
      }
   }

   argv = ( const char ** ) zh_xgrab( sizeof( char * ) * ( argc + 1 ) );
   argc = 0;
   for( i = iFirst; i <= iLast; ++i )
   {
      pParam = zh_param( i, ZH_IT_ARRAY | ZH_IT_STRING );
      if( pParam )
      {
         if( ZH_IS_ARRAY( pParam ) )
         {
            ZH_SIZE nPos, nLen = zh_arrayLen( pParam );
            for( nPos = 1; nPos <= nLen; ++nPos )
            {
               if( zh_arrayGetType( pParam, nPos ) & ZH_IT_STRING )
                  argv[ argc++ ] = zh_arrayGetCPtr( pParam, nPos );
            }
         }
         else if( ZH_IS_STRING( pParam ) )
            argv[ argc++ ] = zh_itemGetCPtr( pParam );
      }
   }
   argv[ argc ] = NULL;

   *pArgC = argc;
   *pArgV = argv;
}

ZH_FUNC( ZH_COMPILE )
{
   int argc;
   const char ** argv;
   PZH_ITEM pIncItem;
   PZH_PP_OPEN_FUNC pOpenFunc;
   PZH_PP_MSG_FUNC pMsgFunc;

   zh_compGenArgList( 1, zh_pcount(), &argc, &argv, &pIncItem, &pOpenFunc, &pMsgFunc );
   zh_retni( zh_compMainExt( argc, argv, NULL, NULL, NULL, 0, pIncItem, pOpenFunc, pMsgFunc ) );
   zh_xfree( ( void * ) argv );
}

ZH_FUNC( ZH_COMPILEBUF )
{
   int iResult, argc;
   const char ** argv;
   PZH_ITEM pIncItem;
   PZH_PP_OPEN_FUNC pOpenFunc;
   PZH_PP_MSG_FUNC pMsgFunc;
   ZH_BYTE * pBuffer;
   ZH_SIZE nLen;

   zh_compGenArgList( 1, zh_pcount(), &argc, &argv, &pIncItem, &pOpenFunc, &pMsgFunc );
   iResult = zh_compMainExt( argc, argv, &pBuffer, &nLen, NULL, 0, pIncItem, pOpenFunc, pMsgFunc );
   zh_xfree( ( void * ) argv );

   if( iResult == EXIT_SUCCESS && pBuffer )
      zh_retclen_buffer( ( char * ) pBuffer, nLen );
}

ZH_FUNC( ZH_COMPILEFROMBUF )
{
   const char * szSource = zh_parc( 1 );

   if( szSource )
   {
      int iResult, argc;
      const char ** argv;
      PZH_ITEM pIncItem;
      PZH_PP_OPEN_FUNC pOpenFunc;
      PZH_PP_MSG_FUNC pMsgFunc;
      ZH_BYTE * pBuffer;
      ZH_SIZE nLen;

      zh_compGenArgList( 2, zh_pcount(), &argc, &argv, &pIncItem, &pOpenFunc, &pMsgFunc );
      iResult = zh_compMainExt( argc, argv, &pBuffer, &nLen, szSource, 0, pIncItem, pOpenFunc, pMsgFunc );
      zh_xfree( ( void * ) argv );

      if( iResult == EXIT_SUCCESS && pBuffer )
         zh_retclen_buffer( ( char * ) pBuffer, nLen );
   }
}
