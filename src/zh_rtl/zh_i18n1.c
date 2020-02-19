/*
 * I18N translation Ziher functions
 *
 * Copyright 2008 Przemyslaw Czerpak
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

#define _ZH_I18N_INTERNAL_

#include "zh_api.h"
#include "zh_item_api.h"
#include "zh_api_error.h"
#include "zh_codepage_api.h"
#include "zh_math.h"
#include "zh_vm.h"
#include "zh_stack.h"
#include "zh_thread.h"
#include "crypto/zh_chksum.h"

/*
   i18n hash table items:

   "LANG"            => <cLangID>
   "BASE_LANG"       => <cLangID>
   "CODEPAGE"        => <cCodePage>
   "BASE_CODEPAGE"   => <cCodePage>
   "DESCRIPTION"     => <cDescription>
   "PLURAL_EXP"      => <cPluralExp>
   "BASE_PLURAL_EXP" => <cPluralExp>
   "CONTEXT"         => ;
               { "" => { <cMsg> => <cResult> | <aResult> } ; // default context
      [, <cContext> => { <cMsg> => <cResult> | <aResult> } ] }

   i18n file format:

      00-03 signature { 193, 'H', 'B', 'L' }
      04-07 size of serialized i18n hash table, 32 bits in little endian order
      08-11 CRC32 of serialized i18n hash table
      12-15 unused
      16-47 description
      48-63 unused
      64-.. serialized i18n hash table
 */

#define ZH_I18N_HEADER_SIZE   64
#define ZH_I18N_SIG_OFFSET    0
#define ZH_I18N_SIG_SIZE      4
#define ZH_I18N_SIZE_OFFSET   4
#define ZH_I18N_CRC_OFFSET    8
#define ZH_I18N_TXT_OFFSET    16
#define ZH_I18N_TXT_SIZE      32

#define ZH_I18N_PLURAL_EN     1
#define ZH_I18N_PLURAL_PL     2
#define ZH_I18N_PLURAL_LT     3
#define ZH_I18N_PLURAL_CS     4
#define ZH_I18N_PLURAL_FR     5
#define ZH_I18N_PLURAL_GA     6
#define ZH_I18N_PLURAL_HR     7
#define ZH_I18N_PLURAL_HU     8
#define ZH_I18N_PLURAL_JA     9
#define ZH_I18N_PLURAL_KO     10
#define ZH_I18N_PLURAL_LV     11
#define ZH_I18N_PLURAL_PT_BR  12
#define ZH_I18N_PLURAL_RO     13
#define ZH_I18N_PLURAL_RU     14
#define ZH_I18N_PLURAL_SK     15
#define ZH_I18N_PLURAL_SL     16
#define ZH_I18N_PLURAL_SR     17
#define ZH_I18N_PLURAL_TR     18
#define ZH_I18N_PLURAL_UK     19
#define ZH_I18N_PLURAL_VI     20

typedef struct _ZH_PLURAL_FORMS
{
   const char * szLangID;
   int          iForm;
}
ZH_PLURAL_FORMS, * PZH_PLURAL_FORMS;

static const ZH_PLURAL_FORMS s_plural_forms[] =
{
   { "EN",    ZH_I18N_PLURAL_EN    },
   { "PL",    ZH_I18N_PLURAL_PL    },
   { "LT",    ZH_I18N_PLURAL_LT    },
   { "CS",    ZH_I18N_PLURAL_CS    },
   { "FR",    ZH_I18N_PLURAL_FR    },
   { "GA",    ZH_I18N_PLURAL_GA    },
   { "HR",    ZH_I18N_PLURAL_HR    },
   { "HU",    ZH_I18N_PLURAL_HU    },
   { "JA",    ZH_I18N_PLURAL_JA    },
   { "KO",    ZH_I18N_PLURAL_KO    },
   { "LV",    ZH_I18N_PLURAL_LV    },
   { "PT-BR", ZH_I18N_PLURAL_PT_BR },
   { "RO",    ZH_I18N_PLURAL_RO    },
   { "RU",    ZH_I18N_PLURAL_RU    },
   { "SK",    ZH_I18N_PLURAL_SK    },
   { "SL",    ZH_I18N_PLURAL_SL    },
   { "SR",    ZH_I18N_PLURAL_SR    },
   { "TR",    ZH_I18N_PLURAL_TR    },
   { "UK",    ZH_I18N_PLURAL_UK    },
   { "VI",    ZH_I18N_PLURAL_VI    }
};

#define ZH_PLURAL_FOMRS_COUNT  ZH_SIZEOFARRAY( s_plural_forms )

static const ZH_UCHAR s_signature[ 4 ] = { 193, 'H', 'B', 'L' };
typedef struct _ZH_I18N_TRANS
{
   ZH_COUNTER   iUsers;
   PZH_CODEPAGE cdpage;
   PZH_CODEPAGE base_cdpage;
   PZH_ITEM     table;
   PZH_ITEM     context_table;
   PZH_ITEM     default_context;
   PZH_ITEM     plural_block;
   PZH_ITEM     base_plural_block;
   int          plural_form;
   int          base_plural_form;
}
ZH_I18N_TRANS, * PZH_I18N_TRANS;

static PZH_I18N_TRANS zh_i18n_table( void )
{
   return ( PZH_I18N_TRANS ) zh_vmI18N();
}

static int zh_i18n_pluralformfind( const char * szLang )
{
   int i;

   for( i = 0; i < ( int ) ZH_PLURAL_FOMRS_COUNT; ++i )
   {
      if( zh_stricmp( szLang, s_plural_forms[ i ].szLangID ) == 0 )
         return s_plural_forms[ i ].iForm;
   }
   if( strlen( szLang ) > 2 )
   {
      for( i = 0; i < ( int ) ZH_PLURAL_FOMRS_COUNT; ++i )
      {
         if( zh_strnicmp( szLang, s_plural_forms[ i ].szLangID, 2 ) == 0 )
            return s_plural_forms[ i ].iForm;
      }
   }
   return 0;
}

static const char * zh_i18n_pluralformid( int iForm )
{
   int i;

   for( i = 0; i < ( int ) ZH_PLURAL_FOMRS_COUNT; ++i )
   {
      if( s_plural_forms[ i ].iForm == iForm )
         return s_plural_forms[ i ].szLangID;
   }
   return NULL;
}

/* NOTE: Source:
         https://www.gnu.org/software/hello/manual/gettext/Plural-forms.html
         [vszakats] */

static long zh_i18n_pluralindex( int iForm, PZH_ITEM pNum )
{
   double n = zh_numRound( zh_itemGetND( pNum ), 10 ), n10, n100;

   switch( iForm )
   {
      case ZH_I18N_PLURAL_PL:
         n10  = fmod( n, 10.0 );
         n100 = fmod( n, 100.0 );
         return n == 1 ? 1 : ( n10 >= 2 && n10 <= 4 &&
                               ( n100 < 10 || n100 >= 20 ) ? 2 : 3 );

      case ZH_I18N_PLURAL_RO:
         n100 = fmod( n, 100.0 );
         return n == 1 ? 1 : ( n == 0 || ( n100 > 0 && n100 < 20 ) ) ? 2 : 3;

      case ZH_I18N_PLURAL_HR:
      case ZH_I18N_PLURAL_SR:
      case ZH_I18N_PLURAL_RU:
      case ZH_I18N_PLURAL_UK:
         n10  = fmod( n, 10.0 );
         n100 = fmod( n, 100.0 );
         return n10 == 1 && n100 != 11 ? 1 : n10 >= 2 && n10 <= 4 && ( n100 < 10 || n100 >= 20 ) ? 2 : 3;

      case ZH_I18N_PLURAL_CS:
      case ZH_I18N_PLURAL_SK:
         return n == 1 ? 1 : ( ( n >= 2 && n <= 4 ) ? 2 : 3 );

      case ZH_I18N_PLURAL_SL:
         n100 = fmod( n, 100.0 );
         return n100 == 1 ? 1 : ( n100 == 2 ? 1 : ( n100 == 3 || n100 == 4 ? 3 : 4 ) );

      case ZH_I18N_PLURAL_LT:
         n10  = fmod( n, 10.0 );
         n100 = fmod( n, 100.0 );
         return n10 == 1 && n100 != 11 ? 1 : ( n10 != 0 && ( n100 < 10 || n100 >= 20 ) ? 2 : 3 );

      case ZH_I18N_PLURAL_LV:
         n10  = fmod( n, 10.0 );
         n100 = fmod( n, 100.0 );
         return ( n10 == 1 && n100 != 11 ) ? 1 : ( n != 0 ? 2 : 3 );

      case ZH_I18N_PLURAL_GA:
         return n == 1 ? 1 : ( n == 2 ? 2 : 3 );

      case ZH_I18N_PLURAL_JA:
      case ZH_I18N_PLURAL_KO:
      case ZH_I18N_PLURAL_VI:
      case ZH_I18N_PLURAL_TR:
         return 1;

      case ZH_I18N_PLURAL_FR:
      case ZH_I18N_PLURAL_PT_BR:
         return n <= 1 ? 1 : 2;

      case ZH_I18N_PLURAL_EN:
      case ZH_I18N_PLURAL_HU:
      default:
         return n == 1 ? 1 : 2;
   }
}

static void zh_i18n_setitem( PZH_ITEM pHash, const char * szKey, const char * szValue )
{
   PZH_ITEM pKey = zh_itemPutC( NULL, szKey );
   PZH_ITEM pValue = zh_itemPutC( NULL, szValue );

   zh_hashAdd( pHash, pKey, pValue );
   zh_itemRelease( pKey );
   zh_itemRelease( pValue );
}

static PZH_ITEM zh_i18n_pluralexp_compile( PZH_ITEM pExp )
{
   ZH_SIZE nLen = zh_itemGetCLen( pExp );
   PZH_ITEM pBlock = NULL;

   if( nLen > 0 )
   {
      char * szMacro = ( char * ) zh_xgrab( nLen + 6 );
      const char * szType;
      PZH_ITEM pMacro;

      szMacro[ 0 ] = '{';
      szMacro[ 1 ] = '|';
      szMacro[ 2 ] = 'n';
      szMacro[ 3 ] = '|';
      memcpy( &szMacro[ 4 ], zh_itemGetCPtr( pExp ), nLen );
      szMacro[ 4 + nLen ] = '}';
      szMacro[ 5 + nLen ] = '\0';
      pMacro = zh_itemPutCLPtr( NULL, szMacro, nLen );
      szType = zh_macroGetType( pMacro );
      if( *szType == 'B' )
      {
         zh_vmPush( pMacro );
         zh_macroGetValue( zh_stackItemFromTop( -1 ), 0, 0 );
         if( zh_vmRequestQuery() == 0 )
         {
            pExp = zh_stackItemFromTop( -1 );
            if( ZH_IS_BLOCK( pExp ) )
               pBlock = zh_itemNew( pExp );
            zh_stackPop();
         }
      }
      zh_itemRelease( pMacro );
   }

   return pBlock;
}

static PZH_I18N_TRANS zh_i18n_new( void )
{
   PZH_I18N_TRANS pI18N;
   PZH_ITEM pKey;

   pI18N = ( PZH_I18N_TRANS ) zh_xgrabz( sizeof( ZH_I18N_TRANS ) );
   zh_atomic_set( &pI18N->iUsers, 1 );
   pI18N->table = zh_hashNew( zh_itemNew( NULL ) );
   pI18N->context_table = zh_hashNew( zh_itemNew( NULL ) );
   pI18N->default_context = zh_hashNew( zh_itemNew( NULL ) );
   pKey = zh_itemPutCConst( NULL, "CONTEXT" );
   zh_hashAdd( pI18N->table, pKey, pI18N->context_table );
   pKey = zh_itemPutC( pKey, NULL );
   zh_hashAdd( pI18N->context_table, pKey, pI18N->default_context );
   zh_itemRelease( pKey );

   return pI18N;
}

/* ZHVM init */
void zh_i18n_init( void )
{
   /* do nothing in this implementation */
}

/* ZHVM exit */
void zh_i18n_exit( void )
{
   /* do nothing in this implementation */
}

/* make copy of i18n set for new thread */
void * zh_i18n_alloc( void * cargo )
{
   if( cargo )
      zh_atomic_inc( &( ( PZH_I18N_TRANS ) cargo )->iUsers );
   return cargo;
}

/* release i18n set when thread is terminated */
void zh_i18n_release( void * cargo )
{
   if( cargo )
   {
      PZH_I18N_TRANS pI18N = ( PZH_I18N_TRANS ) cargo;

      if( zh_atomic_dec( &pI18N->iUsers ) )
      {
         if( pI18N->table )
            zh_itemRelease( pI18N->table );
         if( pI18N->context_table )
            zh_itemRelease( pI18N->context_table );
         if( pI18N->default_context )
            zh_itemRelease( pI18N->default_context );
         if( pI18N->base_plural_block )
            zh_itemRelease( pI18N->base_plural_block );
         if( pI18N->plural_block )
            zh_itemRelease( pI18N->plural_block );
         zh_xfree( pI18N );
      }
   }
}

static PZH_I18N_TRANS zh_i18n_initialize( PZH_ITEM pTable )
{
   PZH_I18N_TRANS pI18N = NULL;

   if( ZH_IS_HASH( pTable ) )
   {
      PZH_ITEM pKey, pContext, pDefContext = NULL;

      pKey = zh_itemPutCConst( NULL, "CONTEXT" );
      pContext = zh_hashGetItemPtr( pTable, pKey, 0 );
      if( pContext )
      {
         pKey = zh_itemPutC( pKey, NULL );
         pDefContext = zh_hashGetItemPtr( pContext, pKey, 0 );
      }

      if( pContext && pDefContext )
      {
         PZH_ITEM pValue;

         pI18N = ( PZH_I18N_TRANS ) zh_xgrabz( sizeof( ZH_I18N_TRANS ) );
         zh_atomic_set( &pI18N->iUsers, 1 );
         pI18N->table = pTable;
         pI18N->context_table = zh_itemNew( pContext );
         pI18N->default_context = zh_itemNew( pDefContext );

         pKey = zh_itemPutCConst( pKey, "BASE_CODEPAGE" );
         pValue = zh_hashGetItemPtr( pTable, pKey, 0 );
         if( pValue )
            pI18N->base_cdpage = zh_cdpFind( zh_itemGetCPtr( pValue ) );

         pKey = zh_itemPutCConst( pKey, "CODEPAGE" );
         pValue = zh_hashGetItemPtr( pTable, pKey, 0 );
         if( pValue )
            pI18N->cdpage = zh_cdpFind( zh_itemGetCPtr( pValue ) );

         pKey = zh_itemPutCConst( pKey, "BASE_LANG" );
         pValue = zh_hashGetItemPtr( pTable, pKey, 0 );
         if( pValue )
            pI18N->base_plural_form = zh_i18n_pluralformfind( zh_itemGetCPtr( pValue ) );

         pKey = zh_itemPutCConst( pKey, "LANG" );
         pValue = zh_hashGetItemPtr( pTable, pKey, 0 );
         if( pValue )
            pI18N->plural_form = zh_i18n_pluralformfind( zh_itemGetCPtr( pValue ) );

         pKey = zh_itemPutCConst( pKey, "BASE_PLURAL_EXP" );
         pValue = zh_hashGetItemPtr( pTable, pKey, 0 );
         if( pValue )
            pI18N->base_plural_block = zh_i18n_pluralexp_compile( pValue );

         pKey = zh_itemPutCConst( pKey, "PLURAL_EXP" );
         pValue = zh_hashGetItemPtr( pTable, pKey, 0 );
         if( pValue )
            pI18N->plural_block = zh_i18n_pluralexp_compile( pValue );
      }
      zh_itemRelease( pKey );
   }

   return pI18N;
}

static PZH_ITEM zh_i18n_serialize( PZH_I18N_TRANS pI18N )
{
   if( pI18N )
   {
      ZH_SIZE nSize;
      ZH_U32 ulCRC;
      char * pBuffer = zh_itemSerialize( pI18N->table, 0, &nSize );
      char * pI18Nbuffer;
      PZH_ITEM pKey, pValue;

      ulCRC = zh_crc32( 0, pBuffer, nSize );
      pI18Nbuffer = ( char * ) memset( zh_xgrab( nSize + ZH_I18N_HEADER_SIZE + 1 ),
                                       0, ZH_I18N_HEADER_SIZE );
      memcpy( pI18Nbuffer + ZH_I18N_HEADER_SIZE, pBuffer, nSize );
      zh_xfree( pBuffer );

      memcpy( pI18Nbuffer, s_signature, ZH_I18N_SIG_SIZE );
      ZH_PUT_LE_UINT32( &pI18Nbuffer[ ZH_I18N_SIZE_OFFSET ], nSize );
      ZH_PUT_LE_UINT32( &pI18Nbuffer[ ZH_I18N_CRC_OFFSET ], ulCRC );

      pKey = zh_itemPutCConst( NULL, "DESCRIPTION" );
      pValue = zh_hashGetItemPtr( pI18N->table, pKey, 0 );
      if( pValue )
         zh_strncpy( &pI18Nbuffer[ ZH_I18N_TXT_OFFSET ],
                     zh_itemGetCPtr( pValue ), ZH_I18N_TXT_SIZE );

      return zh_itemPutCLPtr( pKey, pI18Nbuffer, nSize + ZH_I18N_HEADER_SIZE );
   }

   return NULL;
}

static ZH_BOOL zh_i18n_headercheck( const char * pBuffer, ZH_SIZE nLen )
{
   if( nLen < ZH_I18N_HEADER_SIZE )
      return ZH_FALSE;

   nLen -= ZH_I18N_HEADER_SIZE;
   return memcmp( pBuffer, s_signature, ZH_I18N_SIG_SIZE ) == 0 &&
          ( nLen == 0 ||
            ( ZH_GET_LE_UINT32( &pBuffer[ ZH_I18N_SIZE_OFFSET ] ) == nLen &&
              ZH_GET_LE_UINT32( &pBuffer[ ZH_I18N_CRC_OFFSET ] ) ==
               zh_crc32( 0, pBuffer + ZH_I18N_HEADER_SIZE, nLen ) ) );
}

static PZH_I18N_TRANS zh_i18n_deserialize( PZH_ITEM pItem )
{
   PZH_I18N_TRANS pI18N = NULL;

   if( pItem && ZH_IS_STRING( pItem ) )
   {
      ZH_SIZE nLen = zh_itemGetCLen( pItem );
      const char * pBuffer = zh_itemGetCPtr( pItem );

      if( nLen > ZH_I18N_HEADER_SIZE && zh_i18n_headercheck( pBuffer, nLen ) )
      {
         PZH_ITEM pTable;

         pBuffer += ZH_I18N_HEADER_SIZE;
         nLen -= ZH_I18N_HEADER_SIZE;
         pTable = zh_itemDeserialize( &pBuffer, &nLen );
         if( pTable )
         {
            pI18N = zh_i18n_initialize( pTable );
            if( ! pI18N )
               zh_itemRelease( pTable );
         }
      }
   }

   return pI18N;
}

static ZH_GARBAGE_FUNC( zh_i18n_destructor )
{
   PZH_I18N_TRANS * pI18NHolder = ( PZH_I18N_TRANS * ) Cargo;

   if( *pI18NHolder )
   {
      zh_i18n_release( ( void * ) *pI18NHolder );
      *pI18NHolder = NULL;
   }
}

static const ZH_GC_FUNCS s_gcI18NFuncs =
{
   zh_i18n_destructor,
   zh_gcDummyMark
};

static PZH_I18N_TRANS zh_i18n_param( int * piParam, ZH_BOOL fActive )
{
   PZH_I18N_TRANS * pI18NHolder = ( PZH_I18N_TRANS * ) zh_parptrGC( &s_gcI18NFuncs, *piParam );

   if( pI18NHolder )
   {
      ( *piParam )++;
      return *pI18NHolder;
   }

   return fActive ? zh_i18n_table() : NULL;
}

static PZH_ITEM zh_i18n_newitem( PZH_I18N_TRANS pI18N )
{
   PZH_I18N_TRANS * pI18NHolder;
   PZH_ITEM pItem = zh_itemNew( NULL );

   if( ! pI18N )
      pI18N = zh_i18n_new();
   pI18NHolder = ( PZH_I18N_TRANS * )
                  zh_gcAllocate( sizeof( PZH_I18N_TRANS ), &s_gcI18NFuncs );
   *pI18NHolder = pI18N;

   return zh_itemPutPtrGC( pItem, pI18NHolder );
}

static ZH_BOOL zh_i18n_getpluralform( PZH_I18N_TRANS pI18N, PZH_ITEM pOldForm,
                                      ZH_BOOL fBase )
{
   ZH_BOOL fResult = ZH_FALSE;

   if( pI18N )
   {
      if( pOldForm )
      {
         PZH_ITEM pBlock;
         int iForm;

         if( fBase )
         {
            pBlock = pI18N->base_plural_block;
            iForm = pI18N->base_plural_form;
         }
         else
         {
            pBlock = pI18N->plural_block;
            iForm = pI18N->plural_form;
         }

         if( pBlock )
            zh_itemCopy( pOldForm, pBlock );
         else if( iForm )
            zh_itemPutC( pOldForm, zh_i18n_pluralformid( iForm ) );
         else
            zh_itemPutCConst( pOldForm, "EN" ); /* default is ENGLISH */
      }
      fResult = ZH_TRUE;
   }
   return fResult;
}

static ZH_BOOL zh_i18n_setpluralform( PZH_I18N_TRANS pI18N, PZH_ITEM pForm,
                                      ZH_BOOL fBase )
{
   ZH_BOOL fResult = ZH_FALSE;

   if( pI18N && pForm )
   {
      if( ZH_IS_EVALITEM( pForm ) )
      {
         if( fBase )
         {
            if( pI18N->base_plural_block )
               zh_itemCopy( pI18N->base_plural_block, pForm );
            else
               pI18N->base_plural_block = zh_itemNew( pForm );
         }
         else
         {
            if( pI18N->plural_block )
               zh_itemCopy( pI18N->plural_block, pForm );
            else
               pI18N->plural_block = zh_itemNew( pForm );
         }
         fResult = ZH_TRUE;
      }
      else if( ZH_IS_STRING( pForm ) )
      {
         int iForm = zh_i18n_pluralformfind( zh_itemGetCPtr( pForm ) );
         if( iForm )
         {
            const char * szKey;
            if( fBase )
            {
               if( pI18N->base_plural_block )
               {
                  zh_itemRelease( pI18N->base_plural_block );
                  pI18N->base_plural_block = NULL;
               }
               pI18N->base_plural_form = iForm;
               szKey = "BASE_LANG";
            }
            else
            {
               if( pI18N->plural_block )
               {
                  zh_itemRelease( pI18N->plural_block );
                  pI18N->plural_block = NULL;
               }
               pI18N->plural_form = iForm;
               szKey = "LANG";
            }
            zh_i18n_setitem( pI18N->table, szKey, zh_i18n_pluralformid( iForm ) );
            fResult = ZH_TRUE;
         }
      }
   }
   return fResult;
}

static void zh_i18n_transitm( PZH_ITEM pText, PZH_CODEPAGE cdpIn, PZH_CODEPAGE cdpOut )
{
   ZH_SIZE nLen = zh_itemGetCLen( pText );

   if( nLen > 0 )
   {
      char * szValue = zh_cdpnDup( zh_itemGetCPtr( pText ), &nLen,
                                   cdpIn, cdpOut );
      zh_itemPutCLPtr( pText, szValue, nLen );
   }
}

static const char * zh_i18n_setcodepage( PZH_I18N_TRANS pI18N,
                                         const char * szCdpID,
                                         ZH_BOOL fBase, ZH_BOOL fTranslate )
{
   const char * szOldCdpID = NULL;

   if( pI18N )
   {
      PZH_CODEPAGE cdp = szCdpID ? zh_cdpFind( szCdpID ) : NULL, cdpage;

      cdpage = fBase ? pI18N->base_cdpage : pI18N->cdpage;
      if( cdpage )
         szOldCdpID = cdpage->id;
      if( cdp && cdp != cdpage )
      {
         const char * szKey;

         if( fTranslate && cdpage )
         {
            ZH_SIZE nHashLen = zh_hashLen( pI18N->context_table ), ul;
            for( ul = 1; ul <= nHashLen; ++ul )
            {
               PZH_ITEM pContext = zh_hashGetValueAt( pI18N->context_table, ul );
               ZH_SIZE nCount = zh_hashLen( pContext ), u;

               for( u = 1; u <= nCount; ++u )
               {
                  if( fBase )
                  {
                     zh_i18n_transitm( zh_hashGetKeyAt( pContext, u ),
                                       cdpage, cdp );
                  }
                  else
                  {
                     PZH_ITEM pResult = zh_hashGetValueAt( pContext, u );
                     if( ZH_IS_STRING( pResult ) )
                     {
                        zh_i18n_transitm( pResult, cdpage, cdp );
                     }
                     else if( ZH_IS_ARRAY( pResult ) )
                     {
                        ZH_SIZE nTrans = zh_arrayLen( pResult ), u2;
                        for( u2 = 1; u2 <= nTrans; ++u2 )
                        {
                           zh_i18n_transitm( zh_arrayGetItemPtr( pResult, u2 ),
                                             cdpage, cdp );
                        }
                     }
                  }
               }
               if( fBase )
               {
                  zh_i18n_transitm( zh_hashGetKeyAt( pI18N->context_table, ul ),
                                    cdpage, cdp );
                  zh_hashSetFlags( pContext, ZH_HASH_RESORT );
               }
            }
            if( fBase )
               zh_hashSetFlags( pI18N->context_table, ZH_HASH_RESORT );
         }

         if( fBase )
         {
            pI18N->base_cdpage = cdp;
            szKey = "BASE_CODEPAGE";
         }
         else
         {
            pI18N->cdpage = cdp;
            szKey = "CODEPAGE";
         }
         zh_i18n_setitem( pI18N->table, szKey, szCdpID );
      }
   }

   return szOldCdpID;
}

static const char * zh_i18n_description( PZH_I18N_TRANS pI18N, PZH_ITEM pItem )
{
   if( pI18N )
   {
      PZH_ITEM pKey = zh_itemPutCConst( NULL, "DESCRIPTION" ), pValue;

      pValue = zh_hashGetItemPtr( pI18N->table, pKey, 0 );
      if( pItem )
      {
         if( ZH_IS_STRING( pItem ) )
         {
            if( pValue )
               zh_itemCopy( pValue, pItem );
            else
            {
               zh_hashAdd( pI18N->table, pKey, pItem );
               pValue = zh_hashGetItemPtr( pI18N->table, pKey, 0 );
            }
         }
      }
      zh_itemRelease( pKey );

      return zh_itemGetCPtr( pValue );
   }

   return NULL;
}

static void zh_i18n_addtext( PZH_I18N_TRANS pI18N, PZH_ITEM pMsgID,
                             PZH_ITEM pTrans, PZH_ITEM pContext )
{
   PZH_ITEM pTable = pContext ? zh_hashGetItemPtr( pI18N->context_table,
                                       pContext, 0 ) : pI18N->default_context;

   if( ! pTable )
   {
      pTable = zh_hashNew( zh_itemNew( NULL ) );
      zh_hashAdd( pTable, pMsgID, pTrans );
      zh_hashAdd( pI18N->context_table, pContext, pTable );
      zh_itemRelease( pTable );
   }
   else
      zh_hashAdd( pTable, pMsgID, pTrans );
}

PZH_ITEM zh_i18n_gettext( PZH_ITEM pMsgID, PZH_ITEM pContext )
{
   PZH_I18N_TRANS pI18N = zh_i18n_table();
   PZH_CODEPAGE cdpage = NULL;
   PZH_ITEM pMsgDst = pMsgID;

   if( pI18N )
   {
      PZH_ITEM pTable = pContext && pI18N->context_table ?
                        zh_hashGetItemPtr( pI18N->context_table, pContext, 0 ) :
                        pI18N->default_context;

      cdpage = pI18N->base_cdpage;
      if( pTable )
      {
         pTable = zh_hashGetItemPtr( pTable, pMsgID, 0 );
         if( pTable )
         {
            if( ZH_IS_ARRAY( pTable ) )
               pTable = zh_arrayGetItemPtr( pTable, 1 );
            if( pTable && ZH_IS_STRING( pTable ) )
            {
               pMsgID = pTable;
               cdpage = pI18N->cdpage;
            }
         }
      }
   }

   if( pMsgID )
   {
      if( ZH_IS_STRING( pMsgID ) )
      {
         if( cdpage )
         {
            PZH_CODEPAGE cdp = zh_vmCDP();
            if( cdp && cdp != cdpage )
            {
               if( pMsgDst != pMsgID )
               {
                  zh_itemCopy( pMsgDst, pMsgID );
                  pMsgID = pMsgDst;
               }
               zh_i18n_transitm( pMsgID, cdpage, cdp );
            }
         }
      }
      else
         pMsgID = NULL;
   }

   return pMsgID;
}

PZH_ITEM zh_i18n_ngettext( PZH_ITEM pNum, PZH_ITEM pMsgID, PZH_ITEM pContext )
{
   PZH_I18N_TRANS pI18N = zh_i18n_table();
   PZH_CODEPAGE cdpage = NULL;
   PZH_ITEM pMsgDst = pMsgID;
   PZH_ITEM pBlock = NULL;
   int iPluralForm = 0;

   if( pI18N )
   {
      PZH_ITEM pTable = pContext && pI18N->context_table ?
                        zh_hashGetItemPtr( pI18N->context_table, pContext, 0 ) :
                        pI18N->default_context;

      cdpage = pI18N->base_cdpage;
      pBlock = pI18N->base_plural_block;
      iPluralForm = pI18N->base_plural_form;

      if( pTable )
      {
         PZH_ITEM pMsg = ZH_IS_ARRAY( pMsgID ) ?
                         zh_arrayGetItemPtr( pMsgID, 1 ) : pMsgID;
         pTable = pMsg && ZH_IS_STRING( pMsg ) ?
                  zh_hashGetItemPtr( pTable, pMsg, 0 ) : NULL;
         if( pTable )
         {
            if( ZH_IS_STRING( pTable ) ||
                ( ZH_IS_ARRAY( pTable ) &&
                  ( zh_arrayGetType( pTable, 1 ) & ZH_IT_STRING ) != 0 ) )
            {
               pMsgID = pTable;
               cdpage = pI18N->cdpage;
               pBlock = pI18N->plural_block;
               iPluralForm = pI18N->plural_form;
            }
         }
      }
   }

   if( pMsgID && ZH_IS_ARRAY( pMsgID ) )
   {
      long lIndex;

      if( ! pNum )
         lIndex = 1;
      else if( pBlock )
      {
         zh_evalBlock1( pBlock, pNum );
         lIndex = zh_parnl( -1 );
      }
      else
         lIndex = zh_i18n_pluralindex( iPluralForm, pNum );

      if( lIndex < 1 || ( lIndex != 1 &&
            ( zh_arrayGetType( pMsgID, lIndex ) & ZH_IT_STRING ) == 0 ) )
         lIndex = 1;

      pMsgID = zh_arrayGetItemPtr( pMsgID, lIndex );
   }

   if( pMsgID )
   {
      if( ZH_IS_STRING( pMsgID ) )
      {
         if( cdpage )
         {
            PZH_CODEPAGE cdp = zh_vmCDP();
            if( cdp && cdp != cdpage )
            {
               if( pMsgDst != pMsgID )
               {
                  zh_itemCopy( pMsgDst, pMsgID );
                  pMsgID = pMsgDst;
               }
               zh_i18n_transitm( pMsgID, cdpage, cdp );
            }
         }
      }
      else
         pMsgID = NULL;
   }

   return pMsgID;
}


/*
 * base .prg i18n functions
 */

ZH_FUNC( ZH_I18N_GETTEXT )
{
   PZH_ITEM pMsgID = zh_param( 1, ZH_IT_STRING );
   PZH_ITEM pContext = zh_param( 2, ZH_IT_STRING );

   if( pMsgID )
      pMsgID = zh_i18n_gettext( pMsgID, pContext );

   if( pMsgID && ZH_IS_STRING( pMsgID ) )
      zh_itemReturn( pMsgID );
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_I18N_NGETTEXT )
{
   PZH_ITEM pNum = zh_param( 1, ZH_IT_NUMERIC );
   PZH_ITEM pMsgID = zh_param( 2, ZH_IT_STRING | ZH_IT_ARRAY );
   PZH_ITEM pContext = zh_param( 3, ZH_IT_STRING );

   if( ! pNum )
      pMsgID = NULL;
   else if( pMsgID )
      pMsgID = zh_i18n_ngettext( pNum, pMsgID, pContext );

   if( pMsgID && ZH_IS_STRING( pMsgID ) )
      zh_itemReturn( pMsgID );
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC_TRANSLATE( ZH_I18N_GETTEXT_STRICT, ZH_I18N_GETTEXT )
ZH_FUNC_TRANSLATE( ZH_I18N_NGETTEXT_STRICT, ZH_I18N_NGETTEXT )

/*
 * extended .prg i18n functions to create and manage translation tables
 */

ZH_FUNC( ZH_I18N_CREATE )
{
   zh_itemReturnRelease( zh_i18n_newitem( NULL ) );
}

ZH_FUNC( ZH_I18N_CODEPAGE )
{
   PZH_I18N_TRANS pI18N;
   int iParam = 1;

   pI18N = zh_i18n_param( &iParam, ZH_TRUE );

   if( pI18N )
      zh_retc( zh_i18n_setcodepage( pI18N, zh_parc( iParam ),
                                    zh_parl( iParam + 1 ),
                                    zh_parl( iParam + 2 ) ) );
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );

}

ZH_FUNC( ZH_I18N_PLURALFORM )
{
   PZH_I18N_TRANS pI18N;
   int iParam = 1;

   pI18N = zh_i18n_param( &iParam, ZH_TRUE );
   if( pI18N )
   {
      PZH_ITEM pOldForm = zh_itemNew( NULL );
      PZH_ITEM pForm = zh_param( iParam, ZH_IT_STRING | ZH_IT_EVALITEM );
      ZH_BOOL fBase = zh_parl( iParam + 1 );

      if( zh_i18n_getpluralform( pI18N, pOldForm, fBase ) )
         zh_itemReturn( pOldForm );
      zh_itemRelease( pOldForm );
      if( pForm )
         zh_i18n_setpluralform( pI18N, pForm, fBase );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_I18N_DESCRIPTION )
{
   PZH_I18N_TRANS pI18N;
   int iParam = 1;

   pI18N = zh_i18n_param( &iParam, ZH_TRUE );
   if( pI18N )
   {
      PZH_ITEM pNewDescript = zh_param( iParam, ZH_IT_STRING );

      zh_retc( zh_i18n_description( pI18N, NULL ) );
      if( pNewDescript )
         zh_i18n_description( pI18N, pNewDescript );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_I18N_ADDTEXT )
{
   PZH_I18N_TRANS pI18N;
   int iParam = 1;

   pI18N = zh_i18n_param( &iParam, ZH_FALSE );
   if( pI18N )
   {
      PZH_ITEM pMsgID = zh_param( iParam, ZH_IT_STRING );
      PZH_ITEM pTrans = zh_param( iParam + 1, ZH_IT_STRING | ZH_IT_ARRAY );
      PZH_ITEM pContext = zh_param( iParam + 2, ZH_IT_STRING );

      if( pMsgID && pTrans )
      {
         if( ZH_IS_ARRAY( pTrans ) )
         {
            ZH_SIZE nLen = zh_arrayLen( pTrans );
            if( nLen != 0 )
            {
               ZH_SIZE n;
               for( n = 1; n <= nLen; ++n )
               {
                  if( ! ZH_IS_STRING( zh_arrayGetItemPtr( pTrans, n ) ) )
                  {
                     pTrans = NULL;
                     break;
                  }
               }
            }
            else
               pTrans = NULL;
         }

         if( pTrans )
         {
            zh_i18n_addtext( pI18N, pMsgID, pTrans, pContext );
            return;
         }
      }
   }

   zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_I18N_SET )
{
   if( zh_pcount() > 0 )
   {
      if( ZH_ISNIL( 1 ) )
         zh_vmSetI18N( NULL );
      else
      {
         int iParam = 1;
         PZH_I18N_TRANS pI18N = zh_i18n_param( &iParam, ZH_FALSE );
         if( pI18N )
            zh_vmSetI18N( zh_i18n_alloc( pI18N ) );
         else
         {
            zh_errRT_BASE_SubstR( EG_ARG, 3012, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
            return;
         }
      }
   }
   zh_retl( zh_i18n_table() != NULL );
}

ZH_FUNC( ZH_I18N_SAVETABLE )
{
   PZH_I18N_TRANS pI18N;
   int iParam = 1;

   pI18N = zh_i18n_param( &iParam, ZH_TRUE );
   if( pI18N )
      zh_itemReturnRelease( zh_i18n_serialize( pI18N ) );
}

ZH_FUNC( ZH_I18N_RESTORETABLE )
{
   PZH_ITEM pItem = zh_param( 1, ZH_IT_STRING );

   if( pItem )
   {
      PZH_I18N_TRANS pI18N = zh_i18n_deserialize( pItem );
      if( pI18N )
         zh_itemReturnRelease( zh_i18n_newitem( pI18N ) );
   }
}

ZH_FUNC( ZH_I18N_HEADERSIZE )
{
   zh_retni( ZH_I18N_HEADER_SIZE );
}

ZH_FUNC( ZH_I18N_CHECK )
{
   zh_retl( zh_i18n_headercheck( zh_parc( 1 ), zh_parclen( 1 ) ) );
}

/* unofficial function to access internal hash table used by i18n set */
ZH_FUNC( __I18N_HASHTABLE )
{
   PZH_I18N_TRANS pI18N;
   PZH_ITEM pTable = zh_param( 1, ZH_IT_HASH );

   if( pTable )
   {
      pTable = zh_itemNew( pTable );
      pI18N = zh_i18n_initialize( pTable );
      if( pI18N )
         zh_itemReturnRelease( zh_i18n_newitem( pI18N ) );
      else
         zh_itemRelease( pTable );
   }
   else
   {
      int iParam = 1;

      pI18N = zh_i18n_param( &iParam, ZH_TRUE );
      if( pI18N )
         zh_itemReturn( pI18N->table );
   }
}
