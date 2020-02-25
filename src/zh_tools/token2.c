/*
 * CT3 string functions
 *     - TokenInit()
 *     - TokenExit()
 *     - TokenNext()
 *     - TokenNum()
 *     - TokenAt()
 *     - SaveToken()
 *     - RestToken()
 *     - TokenEnd()
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
#include "zh_vm.h"

#include "zh_stack.h"

/* static functions for token environment management */

#define TOKEN_ENVIRONMENT_STEP  100

typedef struct
{
   ZH_SIZE sStartPos;            /* relative 0-based index of first char of token */
   ZH_SIZE sEndPos;              /* relative 0-based index of first char BEHIND token,
                                    so that length = sEndPos-sStartPos */
} TOKEN_POSITION, * TOKEN_ENVIRONMENT;

/* alloc new token environment */
static TOKEN_ENVIRONMENT sTokEnvNew( void )
{
   TOKEN_ENVIRONMENT env = ( TOKEN_ENVIRONMENT )
         zh_xalloc( sizeof( TOKEN_POSITION ) * ( 2 + TOKEN_ENVIRONMENT_STEP ) + 1 );

   if( env == NULL )
      return NULL;

   /* use the first element to store current length and use of token env */
   env[ 0 ].sStartPos = 0;                    /* 0-based index to next free, unused element */
   env[ 0 ].sEndPos = TOKEN_ENVIRONMENT_STEP; /* but there are 100 elements ready for use */

   /* use second element to store actual index with TokenNext() */
   env[ 1 ].sStartPos = 0;        /* 0-based index value that is to be used NEXT */

   return env;
}

/* add a tokenizing position to a token environment */
static int sTokEnvAddPos( TOKEN_ENVIRONMENT * pEnv, TOKEN_POSITION * pPos )
{
   ZH_SIZE nIndex;
   TOKEN_ENVIRONMENT env = *pEnv;

   /* new memory needed? */
   if( env[ 0 ].sStartPos == env[ 0 ].sEndPos )
   {
      env = *pEnv = ( TOKEN_ENVIRONMENT )
               zh_xrealloc( env, sizeof( TOKEN_POSITION ) *
                            ( 2 + env[ 0 ].sEndPos + TOKEN_ENVIRONMENT_STEP ) + 1 );

      env[ 0 ].sEndPos += TOKEN_ENVIRONMENT_STEP;
   }

   nIndex = env[ 0 ].sStartPos + 2;        /* +2  because of extra elements */
   env[ nIndex ].sStartPos = pPos->sStartPos;
   env[ nIndex ].sEndPos = pPos->sEndPos;
   env[ 0 ].sStartPos++;

   return 1;
}

/* check to see if token pointer is at end of environment */
static int sTokEnvEnd( TOKEN_ENVIRONMENT env )
{
   return env[ 1 ].sStartPos >= env[ 0 ].sStartPos;
}

/* get size of token environment in memory */
static ZH_SIZE sTokEnvGetSize( TOKEN_ENVIRONMENT env )
{
   return sizeof( TOKEN_POSITION ) * ( 2 + env[ 0 ].sEndPos );
}

/* get position element pointed to by tokenizing pointer */
static TOKEN_POSITION * sTokEnvGetPos( TOKEN_ENVIRONMENT env )
{
   if( env[ 1 ].sStartPos >= env[ 0 ].sStartPos )
      return NULL;

   return env + 2 + ( env[ 1 ].sStartPos ); /* "+2" because of extra elements */
}

/* get position element pointed to by given 0-based index */
static TOKEN_POSITION * sTokEnvGetPosIndex( TOKEN_ENVIRONMENT env, ZH_SIZE nIndex )
{
   if( nIndex >= env[ 0 ].sStartPos )
      return NULL;

   return env + 2 + nIndex; /* "+2" because of extra elements */
}

/* increment tokenizing pointer by one */
static int sTokEnvIncPtr( TOKEN_ENVIRONMENT env )
{
   if( env[ 1 ].sStartPos >= env[ 0 ].sStartPos )
      return 0;
   else
   {
      env[ 1 ].sStartPos++;
      return 1;
   }
}

/* set tokenizing pointer to 0-based value */
static int sTokEnvSetPtr( TOKEN_ENVIRONMENT env, ZH_SIZE sCnt )
{
   if( sCnt >= env[ 0 ].sStartPos )
      return 0;
   else
   {
      env[ 1 ].sStartPos = sCnt;
      return 1;
   }
}

/* decrement tokenizing pointer by one */

/* sTokEnvDecPtr currently not used ! */
#if 0
static int sTokEnvDecPtr( TOKEN_ENVIRONMENT env )
{
   if( env[ 1 ].sStartPos <= 0 )
      return 0;
   else
   {
      env[ 1 ].sStartPos--;
      return 1;
   }
}
#endif

/* get value of tokenizing pointer */
static ZH_SIZE sTokEnvGetPtr( TOKEN_ENVIRONMENT env )
{
   return env[ 1 ].sStartPos;
}

/* get token count */
static ZH_SIZE sTokEnvGetCnt( TOKEN_ENVIRONMENT env )
{
   return env[ 0 ].sStartPos;
}

/* free token environment */
static void sTokEnvDel( TOKEN_ENVIRONMENT env )
{
   zh_xfree( env );
}

/* Ziher functions */

/* static data */
static const char sc_spcSeparatorStr[] =
   "\x00" "\x09" "\x0A" "\x0C" "\x1A" "\x20" "\x8A" "\x8C" ",.;:!\?/\\<>()#&%+-*";

static void s_token_exit( void * cargo )
{
   TOKEN_ENVIRONMENT * penv = ( TOKEN_ENVIRONMENT * ) cargo;

   if( *penv )
   {
      sTokEnvDel( *penv );
      *penv = NULL;
   }
}

static ZH_TSD_NEW( s_token, sizeof( TOKEN_ENVIRONMENT * ), NULL, s_token_exit );

static void sTokSet( TOKEN_ENVIRONMENT env )
{
   TOKEN_ENVIRONMENT * penv = ( TOKEN_ENVIRONMENT * ) zh_stackGetTSD( &s_token );

   if( *penv != env )
   {
      if( *penv )
         sTokEnvDel( *penv );
      *penv = env;
   }
}

static TOKEN_ENVIRONMENT sTokGet( int iParam, ZH_BOOL fReadOnly )
{
   if( iParam < 0 || ( iParam > 0 && ZH_ISCHAR( iParam ) ) )
   {
      if( iParam < 0 || fReadOnly || ZH_ISBYREF( iParam ) )
      {
         ZH_SIZE nLen;

         if( iParam < 0 )
            iParam = -iParam;

         nLen = zh_parclen( iParam );
         if( nLen >= sizeof( TOKEN_POSITION ) * 2 )
         {
            TOKEN_ENVIRONMENT env = ( TOKEN_ENVIRONMENT ) ZH_UNCONST( zh_parc( iParam ) );

            if( sTokEnvGetSize( env ) == nLen )
               return fReadOnly ? env : ( TOKEN_ENVIRONMENT ) zh_xmemdup( env, nLen + 1 );
         }
      }
      return NULL;
   }
   else
      return * ( TOKEN_ENVIRONMENT * ) zh_stackGetTSD( &s_token );
}

static int sTokSave( TOKEN_ENVIRONMENT sTokenEnvironment, int iParam )
{
   if( iParam != 0 && ZH_ISBYREF( iParam ) )
   {
      if( ! zh_storclen_buffer( ( char * ) sTokenEnvironment,
                                sTokEnvGetSize( sTokenEnvironment ), iParam ) )
      {
         sTokEnvDel( sTokenEnvironment );
         return 0;
      }
   }
   else
      sTokSet( sTokenEnvironment );
   return 1;
}

ZH_FUNC( TOKENINIT )
{
   if( ZH_ISCHAR( 1 ) )
   {
      const char * pcString = zh_parc( 1 );
      ZH_SIZE sStrLen = zh_parclen( 1 );
      const char * pcSeparatorStr;
      ZH_SIZE sSeparatorStrLen;
      ZH_SIZE nSkip;
      const char * pcSubStr, * pc;
      ZH_SIZE sSubStrLen;
      TOKEN_ENVIRONMENT sTokenEnvironment;
      TOKEN_POSITION sTokenPosition;

      /* separator string */
      sSeparatorStrLen = zh_parclen( 2 );
      if( sSeparatorStrLen > 0 )
         pcSeparatorStr = zh_parc( 2 );
      else
      {
         pcSeparatorStr = sc_spcSeparatorStr;
         sSeparatorStrLen = sizeof( sc_spcSeparatorStr ) - 1;
      }

      /* skip width */
      if( ZH_IS_PARAM_NUM( 3 ) )
         nSkip = zh_parns( 3 );
      else
         nSkip = ZH_SIZE_MAX;
      if( nSkip == 0 )
         nSkip = ZH_SIZE_MAX;

      /* allocate new token environment */
      if( ( sTokenEnvironment = sTokEnvNew() ) == NULL )
      {
         int iArgErrorMode = ct_getargerrormode();

         if( iArgErrorMode != CT_ARGERR_IGNORE )
            ct_error( ( ZH_USHORT ) iArgErrorMode, EG_MEM, CT_ERROR_TOKENINIT,
                      NULL, ZH_ERR_FUNCNAME, 0, EF_CANDEFAULT,
                      ZH_ERR_ARGS_BASEPARAMS );

         zh_retl( ZH_FALSE );
         return;
      }

      pcSubStr = pcString;
      sSubStrLen = sStrLen;

      /* scan start condition */
      pc = pcSubStr - 1;

      for( ;; )
      {
         ZH_SIZE sMatchedPos = sSeparatorStrLen;
         ZH_SIZE nSkipCnt;

         /* nSkip */
         nSkipCnt = 0;
         do
         {
            sSubStrLen -= ( pc - pcSubStr ) + 1;
            pcSubStr = pc + 1;
            pc = ct_at_charset_forward( pcSubStr, sSubStrLen, pcSeparatorStr,
                                        sSeparatorStrLen, &sMatchedPos );
            nSkipCnt++;
         }
         while( nSkipCnt < nSkip && pc == pcSubStr );

         if( sSubStrLen == 0 )
            break;

         sTokenPosition.sStartPos = pcSubStr - pcString;
         if( pc == NULL )
            sTokenPosition.sEndPos = pcSubStr - pcString + sSubStrLen;
         else
            sTokenPosition.sEndPos = pc - pcString;

         if( ! sTokEnvAddPos( &sTokenEnvironment, &sTokenPosition ) )
         {
            int iArgErrorMode = ct_getargerrormode();

            if( iArgErrorMode != CT_ARGERR_IGNORE )
               ct_error( ( ZH_USHORT ) iArgErrorMode, EG_MEM, CT_ERROR_TOKENINIT,
                         NULL, ZH_ERR_FUNCNAME, 0, EF_CANDEFAULT,
                         ZH_ERR_ARGS_BASEPARAMS );

            sTokEnvDel( sTokenEnvironment );
            zh_retl( ZH_FALSE );
            return;
         }

         if( pc == NULL )
            break;
      }

      /* save token environment to 4th parameter OR to the static */
      zh_retl( sTokSave( sTokenEnvironment, 4 ) );
   }
   else
   {
      /* if there is a token environment stored in either the 4th parameter or
         in the static variable -> rewind to first token */
      TOKEN_ENVIRONMENT sTokenEnvironment = sTokGet( 4, ZH_FALSE );

      if( sTokenEnvironment != NULL )
      {
         /* rewind to first token */
         int iResult = sTokEnvSetPtr( sTokenEnvironment, 0 );

         if( ! sTokSave( sTokenEnvironment, 4 ) )
            iResult = ZH_FALSE;
         zh_retl( iResult );
      }
      else
      {
         /* nothing to rewind -> return .F. */
         PZH_ITEM pSubst = NULL;
         int iArgErrorMode = ct_getargerrormode();

         if( iArgErrorMode != CT_ARGERR_IGNORE )
            pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                     CT_ERROR_TOKENINIT, NULL, ZH_ERR_FUNCNAME, 0,
                                     EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

         if( pSubst != NULL )
            zh_itemReturnRelease( pSubst );
         else
            zh_retl( ZH_FALSE );
      }
   }
}

ZH_FUNC( TOKENNEXT )
{
   if( ZH_ISCHAR( 1 ) )
   {
      const char * pcString = zh_parc( 1 );
      ZH_SIZE sStrLen = zh_parclen( 1 );

      TOKEN_ENVIRONMENT sTokenEnvironment = sTokGet( 3, ZH_FALSE );
      TOKEN_POSITION * psTokenPosition;

      /* token environment by parameter ... */
      if( sTokenEnvironment == NULL )
      {
         int iArgErrorMode = ct_getargerrormode();

         if( iArgErrorMode != CT_ARGERR_IGNORE )
            ct_error( ( ZH_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_TOKENNEXT,
                      NULL, ZH_ERR_FUNCNAME, 0, EF_CANDEFAULT,
                      ZH_ERR_ARGS_BASEPARAMS );
         zh_retc_null();
         return;
      }

      /* nth token or next token ?  */
      if( ZH_IS_PARAM_NUM( 2 ) )
      {
         psTokenPosition = sTokEnvGetPosIndex( sTokenEnvironment, zh_parns( 2 ) - 1 );
         /* no increment here */
      }
      else
      {
         psTokenPosition = sTokEnvGetPos( sTokenEnvironment );
         /* increment counter */
         sTokEnvIncPtr( sTokenEnvironment );
      }

      if( psTokenPosition == NULL || sStrLen <= psTokenPosition->sStartPos )
      {
         int iArgErrorMode = ct_getargerrormode();

         if( iArgErrorMode != CT_ARGERR_IGNORE )
            ct_error( ( ZH_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_TOKENNEXT, NULL,
                      ZH_ERR_FUNCNAME, 0, EF_CANDEFAULT, ZH_ERR_ARGS_BASEPARAMS );

         sTokSave( sTokenEnvironment, 3 );
         zh_retc_null();
         return;
      }

      if( sStrLen < psTokenPosition->sEndPos )
         zh_retclen( pcString + psTokenPosition->sStartPos,
                     sStrLen - ( psTokenPosition->sStartPos ) );
      else
         zh_retclen( pcString + psTokenPosition->sStartPos,
                     ( psTokenPosition->sEndPos ) - ( psTokenPosition->sStartPos ) );

      sTokSave( sTokenEnvironment, 3 );
   }
   else
   {
      /* no string given, no token returns */
      PZH_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_TOKENNEXT, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else
         zh_retc_null();
   }
}

ZH_FUNC( TOKENNUM )
{
   TOKEN_ENVIRONMENT sTokenEnvironment = sTokGet( 1, ZH_TRUE );

   if( sTokenEnvironment != NULL )
      zh_retns( sTokEnvGetCnt( sTokenEnvironment ) );
   else
   {
      PZH_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_TOKENNUM, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else
         zh_retns( 0 );
   }
}

ZH_FUNC( TOKENEND )
{
   TOKEN_ENVIRONMENT sTokenEnvironment = sTokGet( 1, ZH_TRUE );

   if( sTokenEnvironment != NULL )
      zh_retl( sTokEnvEnd( sTokenEnvironment ) );
   else
   {
      PZH_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_TOKENEND, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else
         /* it is CT3 behaviour to return .T. if there's no string TokenInit()'ed */
         zh_retl( ZH_TRUE );
   }
}

ZH_FUNC( TOKENEXIT )
{
   TOKEN_ENVIRONMENT sTokenEnvironment = sTokGet( 0, ZH_TRUE );

   if( sTokenEnvironment != NULL )
   {
      sTokSet( NULL );
      zh_retl( ZH_TRUE );
   }
   else
      zh_retl( ZH_FALSE );
}

ZH_FUNC( TOKENAT )
{
   int iSeparatorPos = 0;
   ZH_SIZE sCurrentIndex;
   TOKEN_ENVIRONMENT sTokenEnvironment;
   TOKEN_POSITION *psTokenPosition;

   if( ZH_ISLOG( 1 ) )
      iSeparatorPos = zh_parl( 1 );

   sTokenEnvironment = sTokGet( 3, ZH_TRUE );
   if( sTokenEnvironment == NULL )
   {
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         ct_error( ( ZH_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_TOKENAT,
                   NULL, ZH_ERR_FUNCNAME, 0, EF_CANDEFAULT, ZH_ERR_ARGS_BASEPARAMS );

      zh_retns( 0 );
      return;
   }

   if( ZH_IS_PARAM_NUM( 2 ) )
      sCurrentIndex = zh_parns( 2 ) - 1;
   else
      sCurrentIndex = sTokEnvGetPtr( sTokenEnvironment );

   psTokenPosition = sTokEnvGetPosIndex( sTokenEnvironment, sCurrentIndex );
   if( psTokenPosition == NULL )
   {
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         ct_error( ( ZH_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_TOKENAT, NULL,
                   ZH_ERR_FUNCNAME, 0, EF_CANDEFAULT, ZH_ERR_ARGS_BASEPARAMS );

      zh_retns( 0 );
      return;
   }

   if( iSeparatorPos )
      zh_retns( psTokenPosition->sEndPos + 1 );
   else
      zh_retns( psTokenPosition->sStartPos + 1 );
}

ZH_FUNC( SAVETOKEN )
{
   TOKEN_ENVIRONMENT sTokenEnvironment = sTokGet( 0, ZH_TRUE );

   if( sTokenEnvironment != NULL )
      zh_retclen( ( char * ) sTokenEnvironment, sTokEnvGetSize( sTokenEnvironment ) );
   else
      zh_retc_null();
}

ZH_FUNC( RESTTOKEN )
{
   TOKEN_ENVIRONMENT sNewTokEnv = sTokGet( 1, ZH_FALSE );

   if( sNewTokEnv != NULL || ( ZH_ISCHAR( 1 ) && zh_parclen( 1 ) == 0 ) )
   {
      TOKEN_ENVIRONMENT sTokenEnvironment = sTokGet( 0, ZH_FALSE );

      if( sTokenEnvironment != NULL )
         zh_retclen( ( char * ) sTokenEnvironment, sTokEnvGetSize( sTokenEnvironment ) );
      else
         zh_retc_null();

      sTokSet( sNewTokEnv );
   }
   else
   {
      PZH_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_RESTTOKEN, NULL, ZH_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         zh_itemReturnRelease( pSubst );
      else
         zh_retc_null();
   }
}
