/*
 * CT3 string functions
 *     - Token()
 *     - NumToken()
 *     - AtToken()
 *     - TokenLower()
 *     - TokenUpper()
 *     - TokenSep()
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

#include "zh_stack.h"

/* static const data */
static const char * sc_pcSeparatorStr =
   "\x00" "\x09" "\x0A" "\x0C" "\x1A" "\x20" "\x8A" "\x8C" ",.;:!\?/\\<>()#&%+-*";
static const ZH_SIZE sc_sSeparatorStrLen = 26;

/* static data */

typedef struct
{
   /* even if these are chars, variable must be int, since we need an extra -1 */
   int iPreSeparator;
   int iPostSeparator;
} CT_TOKEN, * PCT_TOKEN;

static void s_ct_token_init( void * cargo )
{
   PCT_TOKEN ct_token = ( PCT_TOKEN ) cargo;

   ct_token->iPreSeparator  = -1;
   ct_token->iPostSeparator = -1;
}

static ZH_TSD_NEW( s_ct_token, sizeof( CT_TOKEN ), s_ct_token_init, NULL );

/* defines */
#define DO_TOKEN1_TOKEN       0
#define DO_TOKEN1_NUMTOKEN    1
#define DO_TOKEN1_ATTOKEN     2
#define DO_TOKEN1_TOKENLOWER  3
#define DO_TOKEN1_TOKENUPPER  4

/* helper function for the token function group I */
static void do_token1( int iSwitch )
{
   PCT_TOKEN ct_token = ( PCT_TOKEN ) zh_stackGetTSD( &s_ct_token );

   int iParamCheck = 0;
   int iNoRef = ct_getref() && ZH_ISBYREF( 1 );

   switch( iSwitch )
   {
      case DO_TOKEN1_TOKEN:
         ct_token->iPreSeparator = ct_token->iPostSeparator = -1;
         /* fallthrough */
      case DO_TOKEN1_ATTOKEN:
      case DO_TOKEN1_NUMTOKEN:
      case DO_TOKEN1_TOKENLOWER:
      case DO_TOKEN1_TOKENUPPER:
         iParamCheck = ZH_ISCHAR( 1 );
         break;
   }

   if( iParamCheck )
   {
      const char * pcString = zh_parc( 1 );
      ZH_SIZE sStrLen = zh_parclen( 1 );
      const char * pcSeparatorStr;
      ZH_SIZE sSeparatorStrLen;
      ZH_SIZE nTokenCounter;
      ZH_SIZE nSkip;
      const char * pcSubStr;
      char * pcRet = NULL;
      ZH_SIZE sSubStrLen;
      ZH_SIZE sRetStrLen = 0;
      ZH_SIZE nToken = 0;
      const char * pc;

      /* separator string */
      sSeparatorStrLen = zh_parclen( 2 );
      if( sSeparatorStrLen != 0 )
         pcSeparatorStr = zh_parc( 2 );
      else
      {
         pcSeparatorStr = sc_pcSeparatorStr;
         sSeparatorStrLen = sc_sSeparatorStrLen;
      }

      if( iSwitch == DO_TOKEN1_NUMTOKEN )
      {
         /* token counter */
         nTokenCounter = ZH_SIZE_MAX;
         /* skip width */
         nSkip = zh_parns( 3 );
      }
      else
      {
         /* token counter */
         nTokenCounter = zh_parns( 3 );
         /* skip width */
         nSkip = zh_parns( 4 ); /* ZH_EXTENSION for AtToken()/TokenLower()/TokenUpper() */
      }

      if( nTokenCounter == 0 )
         nTokenCounter = ZH_SIZE_MAX;
      if( nSkip == 0 )
         nSkip = ZH_SIZE_MAX;

      /* prepare return value for TokenUpper()/TokenLower() */
      if( iSwitch == DO_TOKEN1_TOKENLOWER || iSwitch == DO_TOKEN1_TOKENUPPER )
      {
         if( sStrLen == 0 )
         {
            if( iNoRef )
               zh_retl( ZH_FALSE );
            else
               zh_retc_null();
            return;
         }
         sRetStrLen = sStrLen;
         pcRet = ( char * ) zh_xgrab( sRetStrLen + 1 );
         zh_xmemcpy( pcRet, pcString, sRetStrLen );
      }

      /* find the <nTokenCounter>th token */
      pcSubStr = pcString;
      sSubStrLen = sStrLen;

      /* scan start condition */
      pc = pcSubStr - 1;

      while( nToken < nTokenCounter )
      {
         ZH_SIZE sMatchedPos = sSeparatorStrLen;
         ZH_SIZE nSkipCnt;

         /* Skip the left nSkip successive separators */
         nSkipCnt = 0;
         do
         {
            sSubStrLen -= ( pc - pcSubStr ) + 1;
            pcSubStr = pc + 1;
            pc = ct_at_charset_forward( pcSubStr, sSubStrLen,
                                        pcSeparatorStr, sSeparatorStrLen, &sMatchedPos );
            if( iSwitch == DO_TOKEN1_TOKEN )
            {
               ct_token->iPreSeparator = ct_token->iPostSeparator;
               if( sMatchedPos < sSeparatorStrLen )
                  ct_token->iPostSeparator = pcSeparatorStr[ sMatchedPos ];
               else
                  ct_token->iPostSeparator = -1;
            }
            nSkipCnt++;
         }
         while( nSkipCnt < nSkip && pc == pcSubStr );

         if( sSubStrLen == 0 )
         {
            /* string ends with tokenizer (null string after tokenizer at
               end of string is not a token) */
            switch( iSwitch )
            {
               case DO_TOKEN1_TOKEN:
               {
                  char cRet;

                  zh_retc_null();
                  if( ZH_ISBYREF( 5 ) ) /* ZH_EXTENSION */
                  {
                     cRet = ( char ) ct_token->iPreSeparator;
                     zh_storclen( &cRet, ( ct_token->iPreSeparator != -1 ? 1 : 0 ), 5 );
                  }
                  if( ZH_ISBYREF( 6 ) ) /* ZH_EXTENSION */
                  {
                     cRet = ( char ) ct_token->iPostSeparator;
                     zh_storclen( &cRet, ( ct_token->iPostSeparator != -1 ? 1 : 0 ), 6 );
                  }
                  break;
               }
               case DO_TOKEN1_NUMTOKEN:
                  zh_retns( nToken );
                  break;

               case DO_TOKEN1_ATTOKEN:
                  zh_retns( 0 );
                  break;

               case DO_TOKEN1_TOKENLOWER:
               case DO_TOKEN1_TOKENUPPER:
                  zh_storclen( pcRet, sRetStrLen, 1 );

                  if( iNoRef )
                  {
                     zh_xfree( pcRet );
                     zh_retl( ZH_FALSE );
                  }
                  else
                     zh_retclen_buffer( pcRet, sRetStrLen );
                  break;
            }
            return;
         }

         switch( iSwitch )
         {
            case DO_TOKEN1_TOKEN:
            case DO_TOKEN1_NUMTOKEN:
            case DO_TOKEN1_ATTOKEN:
               break;

            case DO_TOKEN1_TOKENLOWER:
               if( pcSubStr != pc )     /* letters can be tokenizers, too,
                                           but they should not be lowercase'd */
                  *( pcRet + ( pcSubStr - pcString ) ) = ( char ) zh_charLower( ( ZH_UCHAR ) *pcSubStr );
               break;

            case DO_TOKEN1_TOKENUPPER:
               if( pcSubStr != pc )     /* letters can be tokenizers, too,
                                           but they should not be uppercase'd */
                  *( pcRet + ( pcSubStr - pcString ) ) = ( char ) zh_charUpper( ( ZH_UCHAR ) *pcSubStr );
               break;

            default:
               break;
         }

         nToken++;

         if( pc == NULL )
         {
            /* little trick for return values */
            pc = pcSubStr + sSubStrLen;
            /* we must leave the while loop even if we have not
               yet found the <nTokenCounter>th token */
            break;
         }

         /* should we find the last token, but string ends with tokenizer, i.e.
            pc points to the last character at the moment ?
            -> break here ! */
         if( nTokenCounter == ZH_SIZE_MAX )
         {
            if( nSkip == ZH_SIZE_MAX )
            {
               const char * t;
               ZH_BOOL bLast = ZH_TRUE;

               for( t = pc + 1; t < pcString + sStrLen; t++ )
               {
                  if( ! memchr( pcSeparatorStr, *t, sSeparatorStrLen ) )
                  {
                     bLast = ZH_FALSE;
                     break;
                  }
               }
               if( bLast )
                  break;
            }
            else if( pc + 1 == pcString + sStrLen )
               break;
         }
      }

      switch( iSwitch )
      {
         case DO_TOKEN1_TOKEN:
         {
            char cRet;

            if( nTokenCounter == ZH_SIZE_MAX ||
                nToken == nTokenCounter )
               zh_retclen( pcSubStr, pc - pcSubStr );
            else
               zh_retc_null();

            if( ZH_ISBYREF( 5 ) ) /* ZH_EXTENSION */
            {
               cRet = ( char ) ct_token->iPreSeparator;
               zh_storclen( &cRet, ( ct_token->iPreSeparator != -1 ? 1 : 0 ), 5 );
            }
            if( ZH_ISBYREF( 6 ) ) /* ZH_EXTENSION */
            {
               cRet = ( char ) ct_token->iPostSeparator;
               zh_storclen( &cRet, ( ct_token->iPostSeparator != -1 ? 1 : 0 ), 6 );
            }
            break;
         }
         case DO_TOKEN1_NUMTOKEN:
            zh_retns( nToken );
            break;

         case DO_TOKEN1_ATTOKEN:
            if( nTokenCounter == ZH_SIZE_MAX ||
                nToken == nTokenCounter )
               zh_retns( pcSubStr - pcString + 1 );
            else
               zh_retns( 0 );
            break;

         case DO_TOKEN1_TOKENLOWER:
         case DO_TOKEN1_TOKENUPPER:
            zh_storclen( pcRet, sRetStrLen, 1 );

            if( iNoRef )
            {
               zh_xfree( pcRet );
               zh_retl( ZH_FALSE );
            }
            else
               zh_retclen_buffer( pcRet, sRetStrLen );
            break;
      }
   }
   else
   {
      switch( iSwitch )
      {
         case DO_TOKEN1_TOKEN:
         {
            PZH_ITEM pSubst = NULL;
            int iArgErrorMode = ct_getargerrormode();
            char cRet;

            if( ZH_ISBYREF( 5 ) ) /* ZH_EXTENSION */
            {
               cRet = ( char ) ct_token->iPreSeparator;
               zh_storclen( &cRet, ( ct_token->iPreSeparator != -1 ? 1 : 0 ), 5 );
            }
            if( ZH_ISBYREF( 6 ) ) /* ZH_EXTENSION */
            {
               cRet = ( char ) ct_token->iPostSeparator;
               zh_storclen( &cRet, ( ct_token->iPostSeparator != -1 ? 1 : 0 ), 6 );
            }

            if( iArgErrorMode != CT_ARGERR_IGNORE )
               pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                        CT_ERROR_TOKEN, NULL, ZH_ERR_FUNCNAME, 0,
                                        EF_CANSUBSTITUTE,
                                        ZH_ERR_ARGS_BASEPARAMS );

            if( pSubst != NULL )
               zh_itemReturnRelease( pSubst );
            else if( ! iNoRef )
               zh_retc_null();
            else
               zh_retl( ZH_FALSE );
            break;
         }
         case DO_TOKEN1_TOKENLOWER:
         case DO_TOKEN1_TOKENUPPER:
         {
            PZH_ITEM pSubst = NULL;
            int iArgErrorMode = ct_getargerrormode();

            if( iArgErrorMode != CT_ARGERR_IGNORE )
               pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                        iSwitch == DO_TOKEN1_TOKENLOWER ?
                                        CT_ERROR_TOKENLOWER : CT_ERROR_TOKENUPPER,
                                        NULL, ZH_ERR_FUNCNAME, 0,
                                        EF_CANSUBSTITUTE,
                                        ZH_ERR_ARGS_BASEPARAMS );

            if( pSubst != NULL )
               zh_itemReturnRelease( pSubst );
            else if( ! iNoRef )
               zh_retc_null();
            else
               zh_retl( ZH_FALSE );
            break;
         }
         case DO_TOKEN1_NUMTOKEN:
         case DO_TOKEN1_ATTOKEN:
         {
            PZH_ITEM pSubst = NULL;
            int iArgErrorMode = ct_getargerrormode();

            if( iArgErrorMode != CT_ARGERR_IGNORE )
               pSubst = ct_error_subst( ( ZH_USHORT ) iArgErrorMode, EG_ARG,
                                        iSwitch == DO_TOKEN1_NUMTOKEN ?
                                        CT_ERROR_NUMTOKEN : CT_ERROR_ATTOKEN,
                                        NULL, ZH_ERR_FUNCNAME, 0,
                                        EF_CANSUBSTITUTE, ZH_ERR_ARGS_BASEPARAMS );

            if( pSubst != NULL )
               zh_itemReturnRelease( pSubst );
            else
               zh_retns( 0 );
            break;
         }
      }
   }
}

ZH_FUNC( ATTOKEN )
{
   do_token1( DO_TOKEN1_ATTOKEN );
}

ZH_FUNC( TOKEN )
{
   do_token1( DO_TOKEN1_TOKEN );
}

ZH_FUNC( NUMTOKEN )
{
   do_token1( DO_TOKEN1_NUMTOKEN );
}

ZH_FUNC( TOKENLOWER )
{
   do_token1( DO_TOKEN1_TOKENLOWER );
}

ZH_FUNC( TOKENUPPER )
{
   do_token1( DO_TOKEN1_TOKENUPPER );
}

ZH_FUNC( TOKENSEP )
{
   PCT_TOKEN ct_token = ( PCT_TOKEN ) zh_stackGetTSD( &s_ct_token );

   char cRet;

   if( zh_parl( 1 ) )
   {
      /* return the separator char BEHIND the last token */
      if( ct_token->iPostSeparator != -1 )
      {
         cRet = ( char ) ct_token->iPostSeparator;
         zh_retclen( &cRet, 1 );
      }
      else
         zh_retc_null();
   }
   else
   {
      /* return the separator char BEFORE the last token */
      if( ct_token->iPreSeparator != -1 )
      {
         cRet = ( char ) ct_token->iPreSeparator;
         zh_retclen( &cRet, 1 );
      }
      else
         zh_retc_null();
   }
}
