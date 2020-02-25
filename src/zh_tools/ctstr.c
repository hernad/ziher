/*
 * Internal and switch functions for CT3 string functions
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

typedef struct
{
   int  iRefSwitch;
   int  iAtMupaSwitch;
   int  iAtLikeMode;
   char cAtLikeChar;
} CT_STR, * PCT_STR;

static void s_ct_str_init( void * cargo )
{
   PCT_STR ct_str = ( PCT_STR ) cargo;

   ct_str->iRefSwitch = 0;
   ct_str->iAtMupaSwitch = 0;
   ct_str->iAtLikeMode = 0;
   ct_str->cAtLikeChar = '?';
}

static ZH_TSD_NEW( s_ct_str, sizeof( CT_STR ), s_ct_str_init, NULL );

/* search for exact substring */
const char * ct_at_exact_forward( const char * pcString, ZH_SIZE sStrLen,
                                  const char * pcMatch, ZH_SIZE sMatchLen, ZH_SIZE * psMatchStrLen )
{
   ZH_SIZE sPos;

   ZH_TRACE( ZH_TR_DEBUG, ( "ct_at_exact_forward (\"%s\", %" ZH_PFS "u, \"%s\", %" ZH_PFS "u, %p)",
                            pcString, sStrLen, pcMatch, sMatchLen, ( void * ) psMatchStrLen ) );

   if( sMatchLen == 0 || sStrLen < sMatchLen )
      return NULL;

   sPos = zh_strAt( pcMatch, sMatchLen, pcString, sStrLen );
   if( sPos == 0 )
      return NULL;
   else
   {
      if( psMatchStrLen != NULL )
         *psMatchStrLen = sMatchLen;
      return pcString + sPos - 1;
   }
}

/* search for exact substring in backward direction */
const char * ct_at_exact_backward( const char * pcString, ZH_SIZE sStrLen,
                                   const char * pcMatch, ZH_SIZE sMatchLen, ZH_SIZE * psMatchStrLen )
{
   ZH_SIZE sIndex;
   const char * pcRet;

   ZH_TRACE( ZH_TR_DEBUG, ( "ct_at_exact_backward (\"%s\", %" ZH_PFS "u, \"%s\", %" ZH_PFS "u, %p)",
                            pcString, sStrLen, pcMatch, sMatchLen, ( void * ) psMatchStrLen ) );

   if( sMatchLen == 0 || sStrLen < sMatchLen )
      return NULL;

   for( pcRet = pcString + sStrLen - sMatchLen; pcRet >= pcString; pcRet-- )
   {
      for( sIndex = 0; sIndex < sMatchLen; sIndex++ )
      {
         if( *( pcRet + sIndex ) != *( pcMatch + sIndex ) )
            break;
      }
      if( sIndex == sMatchLen )
      {
         /* last match found */
         if( psMatchStrLen != NULL )
            *psMatchStrLen = sMatchLen;
         return pcRet;
      }
   }

   return NULL;
}

/* search for substring using wildcard */
const char * ct_at_wildcard_forward( const char * pcString, ZH_SIZE sStrLen,
                                     const char * pcMatch, ZH_SIZE sMatchLen,
                                     char cWildCard, ZH_SIZE * psMatchStrLen )
{
   ZH_SIZE sIndex;
   const char * pcRet, * pcStop;

   ZH_TRACE( ZH_TR_DEBUG, ( "ct_at_wildcard_forward (\"%s\", %" ZH_PFS "u, \"%s\", %" ZH_PFS "u, \'%c\', %p)",
                            pcString, sStrLen, pcMatch, sMatchLen, cWildCard, ( void * ) psMatchStrLen ) );

   if( sMatchLen == 0 || sStrLen < sMatchLen )
      return NULL;

   pcStop = pcString + sStrLen - sMatchLen;
   for( pcRet = pcString; pcRet < pcStop; pcRet++ )
   {
      for( sIndex = 0; sIndex < sMatchLen; sIndex++ )
      {
         char c = *( pcMatch + sIndex );

         if( c != cWildCard && c != *( pcRet + sIndex ) )
            break;
      }
      if( sIndex == sMatchLen )
      {
         if( psMatchStrLen != NULL )
            *psMatchStrLen = sMatchLen;
         return pcRet;
      }
   }

   return NULL;
}

/* search for substring using wildcard in backward direction */
const char * ct_at_wildcard_backward( const char * pcString, ZH_SIZE sStrLen,
                                      const char * pcMatch, ZH_SIZE sMatchLen,
                                      char cWildCard, ZH_SIZE * psMatchStrLen )
{
   ZH_SIZE sIndex;
   const char * pcRet;

   ZH_TRACE( ZH_TR_DEBUG, ( "ct_at_wildcard_backward (\"%s\", %" ZH_PFS "u, \"%s\", %" ZH_PFS "u, \'%c\', %p)",
                            pcString, sStrLen, pcMatch, sMatchLen, cWildCard, ( void * ) psMatchStrLen ) );

   if( sMatchLen == 0 || sStrLen < sMatchLen )
      return NULL;

   for( pcRet = pcString + sStrLen - sMatchLen; pcRet >= pcString; pcRet-- )
   {
      for( sIndex = 0; sIndex < sMatchLen; sIndex++ )
      {
         char c = *( pcMatch + sIndex );

         if( c != cWildCard && c != *( pcRet + sIndex ) )
            break;
      }
      if( sIndex == sMatchLen )
      {
         /* last match found */
         if( psMatchStrLen != NULL )
            *psMatchStrLen = sMatchLen;
         return pcRet;
      }
   }

   return NULL;
}

/* search for character from a set */
const char * ct_at_charset_forward( const char * pcString, ZH_SIZE sStrLen,
                                    const char * pcCharSet, ZH_SIZE sCharSetLen, ZH_SIZE * psMatchedCharPos )
{
   const char * pcRet, * pcSet, * pcStop1, * pcStop2;

   ZH_TRACE( ZH_TR_DEBUG, ( "ct_at_charset_forward (\"%s\", %" ZH_PFS "u, \"%s\", %" ZH_PFS "u, %p)",
                            pcString, sStrLen, pcCharSet, sCharSetLen, ( void * ) psMatchedCharPos ) );

   if( psMatchedCharPos != NULL )
      *psMatchedCharPos = sCharSetLen;

   if( sCharSetLen == 0 || sStrLen == 0 )
      return NULL;

   pcStop1 = pcString + sStrLen;
   pcStop2 = pcCharSet + sCharSetLen;
   for( pcRet = pcString; pcRet < pcStop1; pcRet++ )
   {
      for( pcSet = pcCharSet; pcSet < pcStop2; pcSet++ )
      {
         if( *pcSet == *pcRet )
         {
            if( psMatchedCharPos != NULL )
               *psMatchedCharPos = pcSet - pcCharSet;
            return pcRet;
         }
      }
   }

   return NULL;
}

/* search for character from a set in backward direction */
const char * ct_at_charset_backward( const char * pcString, ZH_SIZE sStrLen,
                                     const char * pcCharSet, ZH_SIZE sCharSetLen, ZH_SIZE * psMatchedCharPos )
{
   const char * pcRet, * pcSet, * pcStop;

   ZH_TRACE( ZH_TR_DEBUG, ( "ct_at_charset_backward (\"%s\", %" ZH_PFS "u, \"%s\", %" ZH_PFS "u, %p)",
                            pcString, sStrLen, pcCharSet, sCharSetLen, ( void * ) psMatchedCharPos ) );

   if( psMatchedCharPos != NULL )
      *psMatchedCharPos = sCharSetLen;

   if( sCharSetLen == 0 || sStrLen == 0 )
      return NULL;

   pcStop = pcCharSet + sCharSetLen;
   for( pcRet = pcString + sStrLen - 1; pcRet >= pcString; pcRet-- )
   {
      for( pcSet = pcCharSet; pcSet < pcStop; pcSet++ )
      {
         if( *pcSet == *pcRet )
         {
            if( psMatchedCharPos != NULL )
               *psMatchedCharPos = pcSet - pcCharSet;
            return pcRet;
         }
      }
   }

   return NULL;
}

/* CSetRef() stuff */

void ct_setref( int iNewSwitch )
{
   PCT_STR ct_str = ( PCT_STR ) zh_stackGetTSD( &s_ct_str );

   ZH_TRACE( ZH_TR_DEBUG, ( "ct_setref(%i)", iNewSwitch ) );

   ct_str->iRefSwitch = iNewSwitch;
}

int ct_getref( void )
{
   PCT_STR ct_str = ( PCT_STR ) zh_stackGetTSD( &s_ct_str );

   ZH_TRACE( ZH_TR_DEBUG, ( "ct_getref()" ) );

   return ct_str->iRefSwitch;
}

ZH_FUNC( CSETREF )
{
   zh_retl( ct_getref() );

   if( ZH_ISLOG( 1 ) )
      ct_setref( zh_parl( 1 ) );
   else if( zh_pcount() > 0 ) /* 1 params, but is not logical ! */
   {
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         ct_error( ( ZH_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_CSETREF,
                   NULL, ZH_ERR_FUNCNAME, 0, EF_CANDEFAULT, ZH_ERR_ARGS_BASEPARAMS );
   }
}

/* CSetAtMupa() stuff */

void ct_setatmupa( int iNewSwitch )
{
   PCT_STR ct_str = ( PCT_STR ) zh_stackGetTSD( &s_ct_str );

   ZH_TRACE( ZH_TR_DEBUG, ( "ct_setatmupa(%i)", iNewSwitch ) );

   ct_str->iAtMupaSwitch = iNewSwitch;
}

int ct_getatmupa( void )
{
   PCT_STR ct_str = ( PCT_STR ) zh_stackGetTSD( &s_ct_str );

   ZH_TRACE( ZH_TR_DEBUG, ( "ct_getatmupa()" ) );

   return ct_str->iAtMupaSwitch;
}

ZH_FUNC( CSETATMUPA )
{
   zh_retl( ct_getatmupa() );

   if( ZH_ISLOG( 1 ) )
      ct_setatmupa( zh_parl( 1 ) );
   else if( zh_pcount() > 0 ) /* 1 params, but is not logical ! */
   {
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         ct_error( ( ZH_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_CSETATMUPA, NULL,
                   ZH_ERR_FUNCNAME, 0, EF_CANDEFAULT, ZH_ERR_ARGS_BASEPARAMS );
   }
}

/* SetAtLike() stuff */

void ct_setatlike( int iNewMode )
{
   PCT_STR ct_str = ( PCT_STR ) zh_stackGetTSD( &s_ct_str );

   ZH_TRACE( ZH_TR_DEBUG, ( "ct_setatlike(%i)", iNewMode ) );

   ct_str->iAtLikeMode = iNewMode;
}

int ct_getatlike( void )
{
   PCT_STR ct_str = ( PCT_STR ) zh_stackGetTSD( &s_ct_str );

   ZH_TRACE( ZH_TR_DEBUG, ( "ct_getatlike()" ) );

   return ct_str->iAtLikeMode;
}

void ct_setatlikechar( char cNewChar )
{
   PCT_STR ct_str = ( PCT_STR ) zh_stackGetTSD( &s_ct_str );

   ZH_TRACE( ZH_TR_DEBUG, ( "ct_setatlikechar(\'%c\')", cNewChar ) );

   ct_str->cAtLikeChar = cNewChar;
}

char ct_getatlikechar( void )
{
   PCT_STR ct_str = ( PCT_STR ) zh_stackGetTSD( &s_ct_str );

   ZH_TRACE( ZH_TR_DEBUG, ( "ct_getatlikechar()" ) );

   return ct_str->cAtLikeChar;
}

ZH_FUNC( SETATLIKE )
{
   zh_retni( ct_getatlike() );

   /* set new mode if first parameter is CT_SETATLIKE_EXACT (==0)
      or CT_SETATLIKE_WILDCARD (==1) */
   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      int iNewMode = zh_parni( 1 );

      if( iNewMode == CT_SETATLIKE_EXACT || iNewMode == CT_SETATLIKE_WILDCARD )
         ct_setatlike( iNewMode );
      else
      {
         int iArgErrorMode = ct_getargerrormode();

         if( iArgErrorMode != CT_ARGERR_IGNORE )
            ct_error( ( ZH_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_SETATLIKE,
                      NULL, ZH_ERR_FUNCNAME, 0, EF_CANDEFAULT,
                      ZH_ERR_ARGS_BASEPARAMS );
      }
   }

   /* set new wildcard character, if ZH_ISCHAR( 2 ) but ! ZH_ISBYREF( 2 ) */
   if( ZH_ISCHAR( 2 ) )
   {
      if( ZH_ISBYREF( 2 ) )
      {
         /* new behaviour: store the current wildcard char in second parameter */
         char cResult;

         cResult = ct_getatlikechar();
         zh_storclen( &cResult, 1, 2 );
      }
      else
      {
         const char * pcNewChar = zh_parc( 2 );

         if( zh_parclen( 2 ) > 0 )
            ct_setatlikechar( *pcNewChar );
      }
   }
   else if( zh_pcount() > 1 ) /* more than 2 params, but second is not string ! */
   {
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         ct_error( ( ZH_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_SETATLIKE, NULL,
                   ZH_ERR_FUNCNAME, 0, EF_CANDEFAULT, ZH_ERR_ARGS_BASEPARAMS );
   }
}
