/*
 * The Language API
 *
 * Copyright 1999-2001 Viktor Szakats
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
#include "zh_lang_api.h"
#include "zh_codepage_api.h"
#include "zh_api_error.h"

static ZH_LANG s_lang_en =
{
   {
      /* Identification */

      "en",                        /* ISO ID (2 chars) */
      "English",                   /* Name (in English) */
      "English",                   /* Name (in native language) */
      "EN",                        /* RFC ID */
      "UTF8",                      /* Codepage */
      "",                          /* Version */

      /* Month names */

      "January",
      "February",
      "March",
      "April",
      "May",
      "June",
      "July",
      "August",
      "September",
      "October",
      "November",
      "December",

      /* Day names */

      "Sunday",
      "Monday",
      "Tuesday",
      "Wednesday",
      "Thursday",
      "Friday",
      "Saturday",

      /* CA-Cl*pper compatible natmsg items */

      "Database Files    # Records    Last Update     Size",
      "Do you want more samples?",
      "Page No.",
      "** Subtotal **",
      "* Subsubtotal *",
      "*** Total ***",
      "Ins",
      "   ",
      "Invalid date",
      "Range: ",
      " - ",
      "Y/N",
      "INVALID EXPRESSION",

      /* Error description names */

      "Unknown error",
      "Argument error",
      "Bound error",
      "String overflow",
      "Numeric overflow",
      "Zero divisor",
      "Numeric error",
      "Syntax error",
      "Operation too complex",
      "",
      "",
      "Memory low",
      "Undefined function",
      "No exported method",
      "Variable does not exist",
      "Alias does not exist",
      "No exported variable",
      "Illegal characters in alias",
      "Alias already in use",
      "",
      "Create error",
      "Open error",
      "Close error",
      "Read error",
      "Write error",
      "Print error",
      "",
      "",
      "",
      "",
      "Operation not supported",
      "Limit exceeded",
      "Corruption detected",
      "Data type error",
      "Data width error",
      "Workarea not in use",
      "Workarea not indexed",
      "Exclusive required",
      "Lock required",
      "Write not allowed",
      "Append lock failed",
      "Lock Failure",
      "",
      "",
      "",
      "Object destructor failure",
      "array access",
      "array assign",
      "array dimension",
      "not an array",
      "conditional",

      /* Internal error names */

      "Unrecoverable error %d: ",
      "Error recovery failure",
      "No ERRORBLOCK() for error",
      "Too many recursive error handler calls",
      "RDD invalid or failed to load",
      "Invalid method type from %s",
      "zh_xgrab can't allocate memory",
      "zh_xrealloc called with a NULL pointer",
      "zh_xrealloc called with an invalid pointer",
      "zh_xrealloc can't reallocate memory",
      "zh_xfree called with an invalid pointer",
      "zh_xfree called with a NULL pointer",
      "Can\'t locate the starting procedure: \'%s\'",
      "No starting procedure",
      "Unsupported VM opcode",
      "Symbol item expected from %s",
      "Invalid symbol type for self from %s",
      "Codeblock expected from %s",
      "Incorrect item type on the stack trying to pop from %s",
      "Stack underflow",
      "An item was going to be copied to itself from %s",
      "Invalid symbol item passed as memvar %s",
      "Memory buffer overflow",
      "zh_xgrab requested to allocate zero bytes",
      "zh_xrealloc requested to resize to zero bytes",
      "zh_xalloc requested to allocate zero bytes",

      /* Texts */

      "YYYY/MM/DD", /* NOTE: Use YYYY for year, MM for month and DD for days. */
      "Y",
      "N"
   }
};

ZH_LANG_ANNOUNCE( EN )

/* Always link in the default language */
#if 0
ZH_LANG_REQUEST( ZH_LANG_DEFAULT );
#endif

/* NOTE: This is the maximum number of registered languages, later this can be
         made dynamic. */
#define ZH_LANG_MAX_              128

#define ZH_LANG_ITEM_ID_ID        0
#define ZH_LANG_ITEM_ID_NAME      1
#define ZH_LANG_ITEM_ID_NAMENAT   2
#define ZH_LANG_ITEM_ID_CODEPAGE  4

typedef struct
{
   const char * pItemList[ ZH_LANG_ITEM_MAX_ ];
}
ZH_LANG_TRANS, * PZH_LANG_TRANS;

typedef struct
{
   PZH_LANG lang;
   void *   buffer;
}
ZH_LANG_BASE, * PZH_LANG_BASE;

static ZH_LANG_BASE s_langList[ ZH_LANG_MAX_ ] = { { &s_lang_en, NULL } };

static void zh_langRelease( PZH_LANG_BASE pBase )
{
   if( pBase->lang )
   {
      if( pBase->buffer )
      {
         zh_xfree( pBase->buffer );
         pBase->buffer = NULL;
      }
      pBase->lang = pBase == s_langList ? &s_lang_en : NULL;
   }
}

static PZH_LANG_BASE zh_langFindBase( const char * pszID )
{
   PZH_LANG_BASE pBase = NULL;

   if( pszID )
   {
      int iPos;

      for( iPos = 0; iPos < ZH_LANG_MAX_; iPos++ )
      {
         if( s_langList[ iPos ].lang != NULL )
         {
            if( zh_stricmp( s_langList[ iPos ].lang->pItemList[ ZH_LANG_ITEM_BASE_ID + ZH_LANG_ITEM_ID_ID ], pszID ) == 0 )
               return &s_langList[ iPos ];
         }
         else if( pBase == NULL )
            pBase = &s_langList[ iPos ];
      }
   }

   return pBase;
}


static ZH_BOOL zh_langTranslate( const char * szNewId, PZH_LANG lang, PZH_CODEPAGE cdpIn, PZH_CODEPAGE cdpOut )
{
   PZH_LANG_BASE pBase;
   ZH_LANG_TRANS trans;
   char *        buffer, * ptr;
   ZH_SIZE       nSize;
   int i;

   if( ! szNewId || *szNewId == 0 || ! lang || ! cdpIn || ! cdpOut || cdpIn == cdpOut )
      return ZH_FALSE;

   memset( &trans, 0, sizeof( trans ) );
   nSize = sizeof( trans );

   for( i = 0; i < ZH_LANG_ITEM_MAX_; ++i )
   {
      char * pszTrans;

      if( i == ZH_LANG_ITEM_BASE_ID + ZH_LANG_ITEM_ID_ID )
         pszTrans = zh_strdup( szNewId );
      else if( i == ZH_LANG_ITEM_BASE_ID + ZH_LANG_ITEM_ID_CODEPAGE )
         pszTrans = zh_strdup( cdpOut->id );
      else
         pszTrans = zh_cdpDup( lang->pItemList[ i ], cdpIn, cdpOut );

      if( strcmp( pszTrans, lang->pItemList[ i ] ) != 0 )
      {
         trans.pItemList[ i ] = pszTrans;
         nSize += strlen( pszTrans ) + 1;
      }
      else
         zh_xfree( pszTrans );
   }

   buffer = ( char * ) zh_xgrab( nSize );
   ptr    = buffer + sizeof( trans );
   for( i = 0; i < ZH_LANG_ITEM_MAX_; ++i )
   {
      if( trans.pItemList[ i ] != NULL )
      {
         ZH_SIZE nLen = strlen( trans.pItemList[ i ] ) + 1;
         memcpy( ptr, trans.pItemList[ i ], nLen );
         zh_xfree( ZH_UNCONST( trans.pItemList[ i ] ) );
         trans.pItemList[ i ] = ptr;
         ptr += nLen;
      }
      else
         trans.pItemList[ i ] = lang->pItemList[ i ];
   }
   memcpy( buffer, &trans, sizeof( trans ) );

   pBase = zh_langFindBase( szNewId );
   if( pBase && pBase->lang == NULL )
   {
      pBase->lang   = ( PZH_LANG ) buffer;
      pBase->buffer = ( void * ) buffer;
      return ZH_TRUE;
   }

   zh_xfree( buffer );
   return ZH_FALSE;
}

void zh_langReleaseAll( void )
{
   int iPos;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_langReleaseAll()" ) );

   for( iPos = 0; iPos < ZH_LANG_MAX_; iPos++ )
      zh_langRelease( &s_langList[ iPos ] );
}

ZH_BOOL zh_langRegister( PZH_LANG lang )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_langRegister(%p)", ( const void * ) lang ) );

   if( lang )
   {
      PZH_LANG_BASE pBase = zh_langFindBase( lang->pItemList[ ZH_LANG_ITEM_BASE_ID + ZH_LANG_ITEM_ID_ID ] );

      if( pBase && pBase->lang == NULL )
      {
         pBase->lang = lang;
         return ZH_TRUE;
      }
   }

   return ZH_FALSE;
}

PZH_LANG zh_langFind( const char * pszID )
{
   PZH_LANG_BASE pBase;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_langFind(%s)", pszID ) );

   pBase = zh_langFindBase( pszID );

   return pBase ? pBase->lang : NULL;
}

PZH_LANG zh_langSelect( PZH_LANG lang )
{
   PZH_LANG langOld;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_langSelect(%p)", ( const void * ) lang ) );

   langOld = zh_vmLang();
   if( lang )
      zh_vmSetLang( lang );

   return langOld;
}

const char * zh_langSelectID( const char * pszID )
{
   const char * pszIDOld = zh_langID();
   PZH_LANG     lang;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_langSelectID(%s)", pszID ) );

   lang = zh_langFind( pszID );
   if( lang )
      zh_langSelect( lang );
   else
      zh_errRT_BASE( EG_ARG, 1303, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );

   return pszIDOld;
}

const char * zh_langGetItem( const char * pszID, int iIndex )
{
   PZH_LANG lang;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_langGetItem(%s,%i)", pszID, iIndex ) );

   lang = pszID ? zh_langFind( pszID ) : zh_vmLang();
   if( lang && iIndex >= 0 && iIndex < ZH_LANG_ITEM_MAX_ )
      return lang->pItemList[ iIndex ];
   else
      return NULL;
}

const char * zh_langID( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_langID()" ) );

   return zh_langGetItem( NULL, ZH_LANG_ITEM_BASE_ID + ZH_LANG_ITEM_ID_ID );
}

/* NOTE: Caller must free the pointer. */

char * zh_langName( const char * pszID )
{
   char *   pszName;
   PZH_LANG lang;

   lang = pszID ? zh_langFind( pszID ) : zh_vmLang();
   if( lang )
   {
      pszName = ( char * ) zh_xgrab( 128 );
      zh_snprintf( pszName, 128, "Ziher Language: %s %s (%s)",
                   zh_langGetItem( pszID, ZH_LANG_ITEM_BASE_ID + ZH_LANG_ITEM_ID_ID ),
                   zh_langGetItem( pszID, ZH_LANG_ITEM_BASE_ID + ZH_LANG_ITEM_ID_NAME ),
                   zh_langGetItem( pszID, ZH_LANG_ITEM_BASE_ID + ZH_LANG_ITEM_ID_NAMENAT ) );
   }
   else
      pszName = zh_strdup( "Ziher Language: (not installed)" );

   return pszName;
}

/* Compatibility interfaces */

const char * zh_langDGetErrorDesc( int iIndex )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_langDGetErrorDesc(%i)", iIndex ) );

   return zh_langGetItem( NULL, ZH_LANG_ITEM_BASE_ERRDESC + iIndex );
}

const char * zh_langDGetItem( int iIndex )
{
   PZH_LANG lang;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_langDGetItem(%i)", iIndex ) );

   lang = zh_vmLang();
   if( lang && iIndex >= 0 && iIndex < ZH_LANG_ITEM_MAX_ )
      return lang->pItemList[ iIndex ];
   else
      return NULL;
}

/* Ziher interface */

ZH_FUNC( __ZH_LANGSELECT )
{
   const char * szNewLang;

   zh_retc( zh_langID() );

   szNewLang = zh_parc( 1 );
   if( szNewLang )
      zh_langSelectID( szNewLang );
}

ZH_FUNC( ZH_LANGNAME )
{
   zh_retc_buffer( zh_langName( zh_parc( 1 ) ) );
}

ZH_FUNC( ZH_LANGERRMSG )
{
   zh_retc_const( zh_langDGetErrorDesc( zh_parnl( 1 ) ) );
}

ZH_FUNC( ZH_LANGMESSAGE )
{
   zh_retc_const( zh_langGetItem( zh_parc( 2 ), zh_parnl( 1 ) ) );
}

/* zh_langNew( <cNewLangId>, <cNewLangCpId>,
 *             <cLangId>, <cLangCpId> ) --> <lOK>
 */
ZH_FUNC( ZH_LANGNEW )
{
   zh_retl( zh_langTranslate( zh_parc( 1 ), zh_langFind( zh_parc( 3 ) ),
                              zh_cdpFindExt( zh_parc( 4 ) ),
                              zh_cdpFindExt( zh_parc( 2 ) ) ) );
}
