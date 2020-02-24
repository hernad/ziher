/*
 * Standalone RTL stubs
 *
 * Copyright 2009 Przemyslaw Czerpak
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
#include "zh_fs_api.h"
#include "zh_codepage_api.h"
#include "zh_error_api.h"
#include "zh_set.h"
#include "zh_vm.h"
#include "zh_comp.h"
#include "memory.zhh"

/* --- FM statistic module --- */


#ifdef ZH_FM_STATISTICS

#define ZH_MEMINFO_SIGNATURE  0xDEADBEAF
#define ZH_MEMSTR_BLOCK_MAX   256

#ifndef ZH_MEMFILER
#define ZH_MEMFILER         0xff
#endif

typedef struct _ZH_MEMINFO
{
   struct _ZH_MEMINFO * pPrevBlock;
   struct _ZH_MEMINFO * pNextBlock;
   ZH_SIZE nSize;
   ZH_U32  Signature;
} ZH_MEMINFO, * PZH_MEMINFO;

#ifdef ZH_ALLOC_ALIGNMENT
#  define ZH_MEMINFO_SIZE  ( ( sizeof( ZH_MEMINFO ) + ZH_ALLOC_ALIGNMENT - 1 ) - \
                             ( sizeof( ZH_MEMINFO ) + ZH_ALLOC_ALIGNMENT - 1 ) % ZH_ALLOC_ALIGNMENT )
#else
#  define ZH_MEMINFO_SIZE  sizeof( ZH_MEMINFO )
#endif

static PZH_MEMINFO s_pMemBlocks         = NULL;
static ZH_ISIZ     s_nMemoryBlocks      = 0; /* memory blocks used */
static ZH_ISIZ     s_nMemoryMaxBlocks   = 0; /* maximum number of used memory blocks */
static ZH_ISIZ     s_nMemoryMaxConsumed = 0; /* memory size consumed */
static ZH_ISIZ     s_nMemoryConsumed    = 0; /* memory max size consumed */

#endif /* ZH_FM_STATISTICS */

void * zh_xgrab( ZH_SIZE nSize )        /* allocates fixed memory, exits on failure */
{
   void * pMem;

   if( nSize == 0 )
      zh_errInternal( ZH_EI_XGRABNULLSIZE, "zh_xgrab requested to allocate zero bytes", NULL, NULL );

#ifdef ZH_FM_STATISTICS
   pMem = malloc( nSize + ZH_MEMINFO_SIZE + sizeof( ZH_U32 ) );
   if( pMem )
   {
      if( s_pMemBlocks )
         s_pMemBlocks->pPrevBlock = ( PZH_MEMINFO ) pMem;
      ( ( PZH_MEMINFO ) pMem )->pNextBlock = s_pMemBlocks;
      ( ( PZH_MEMINFO ) pMem )->pPrevBlock = NULL;
      s_pMemBlocks = ( PZH_MEMINFO ) pMem;
      ( ( PZH_MEMINFO ) pMem )->nSize = nSize;
      ( ( PZH_MEMINFO ) pMem )->Signature = ZH_MEMINFO_SIGNATURE;
      ZH_PUT_LE_UINT32( ( ( ZH_BYTE * ) pMem ) + ZH_MEMINFO_SIZE + nSize, ZH_MEMINFO_SIGNATURE );

      s_nMemoryConsumed += nSize;
      if( s_nMemoryMaxConsumed < s_nMemoryConsumed )
         s_nMemoryMaxConsumed = s_nMemoryConsumed;
      s_nMemoryBlocks++;
      if( s_nMemoryMaxBlocks < s_nMemoryBlocks )
         s_nMemoryMaxBlocks = s_nMemoryBlocks;
      pMem = ( ZH_BYTE * ) pMem + ZH_MEMINFO_SIZE;
   }
   else
#else
   pMem = malloc( nSize );
   if( ! pMem )
#endif
      zh_errInternal( ZH_EI_XGRABALLOC, "zh_xgrab can't allocate memory", NULL, NULL );

   return pMem;
}

void * zh_xrealloc( void * pMem, ZH_SIZE nSize )       /* reallocates memory */
{
#ifdef ZH_FM_STATISTICS
   PZH_MEMINFO pMemBlock;
   ZH_SIZE nMemSize;
   void * pResult;

   if( nSize == 0 )
   {
      if( pMem )
         zh_xfree( pMem );
      return NULL;
   }
   else if( ! pMem )
      return zh_xgrab( nSize );

   pMemBlock = ( PZH_MEMINFO ) ( ( ZH_BYTE * ) pMem - ZH_MEMINFO_SIZE );
   nMemSize = pMemBlock->nSize;

   if( pMemBlock->Signature != ZH_MEMINFO_SIGNATURE )
      zh_errInternal( ZH_EI_XREALLOCINV, "zh_xrealloc called with an invalid pointer", NULL, NULL );

   if( ZH_GET_LE_UINT32( ( ( ZH_BYTE * ) pMem ) + nMemSize ) != ZH_MEMINFO_SIGNATURE )
      zh_errInternal( ZH_EI_XMEMOVERFLOW, "Memory buffer overflow", NULL, NULL );

   ZH_PUT_LE_UINT32( ( ( ZH_BYTE * ) pMem ) + nMemSize, 0 );

   pResult = realloc( pMemBlock, nSize + ZH_MEMINFO_SIZE + sizeof( ZH_U32 ) );
   if( pResult )
   {
      if( s_pMemBlocks == pMemBlock )
         s_pMemBlocks = ( PZH_MEMINFO ) pResult;
      else
         ( ( PZH_MEMINFO ) pResult )->pPrevBlock->pNextBlock = ( PZH_MEMINFO ) pResult;

      if( ( ( PZH_MEMINFO ) pResult )->pNextBlock )
         ( ( PZH_MEMINFO ) pResult )->pNextBlock->pPrevBlock = ( PZH_MEMINFO ) pResult;
      s_nMemoryConsumed += ( nSize - nMemSize );

      if( s_nMemoryMaxConsumed < s_nMemoryConsumed )
         s_nMemoryMaxConsumed = s_nMemoryConsumed;

      ( ( PZH_MEMINFO ) pResult )->nSize = nSize;  /* size of the memory block */
      ZH_PUT_LE_UINT32( ( ( ZH_BYTE * ) pResult ) + nSize + ZH_MEMINFO_SIZE, ZH_MEMINFO_SIGNATURE );
      pResult = ( ZH_BYTE * ) pResult + ZH_MEMINFO_SIZE;
   }
#else
   void * pResult = realloc( pMem, nSize );
#endif

   if( ! pResult && nSize )
      zh_errInternal( ZH_EI_XREALLOC, "zh_xrealloc can't reallocate memory", NULL, NULL );

   return pResult;
}

/* frees fixed memory */
void zh_xfree( void * pMem )
{
   if( pMem )
   {
#ifdef ZH_FM_STATISTICS
      PZH_MEMINFO pMemBlock = ( PZH_MEMINFO ) ( ( ZH_BYTE * ) pMem - ZH_MEMINFO_SIZE );

      if( pMemBlock->Signature != ZH_MEMINFO_SIGNATURE )
         zh_errInternal( ZH_EI_XFREEINV, "zh_xfree called with an invalid pointer", NULL, NULL );

      if( ZH_GET_LE_UINT32( ( ( ZH_BYTE * ) pMem ) + pMemBlock->nSize ) != ZH_MEMINFO_SIGNATURE )
         zh_errInternal( ZH_EI_XMEMOVERFLOW, "Memory buffer overflow", NULL, NULL );

      s_nMemoryConsumed -= pMemBlock->nSize;
      s_nMemoryBlocks--;
      if( s_pMemBlocks == pMemBlock )
         s_pMemBlocks = pMemBlock->pNextBlock;
      else
         pMemBlock->pPrevBlock->pNextBlock = pMemBlock->pNextBlock;

      if( pMemBlock->pNextBlock )
         pMemBlock->pNextBlock->pPrevBlock = pMemBlock->pPrevBlock;

      pMemBlock->Signature = 0;
      ZH_PUT_LE_UINT32( ( ( ZH_BYTE * ) pMem ) + pMemBlock->nSize, 0 );
      pMem = ( ZH_BYTE * ) pMem - ZH_MEMINFO_SIZE;
#endif
      free( pMem );
   }
   else
      zh_errInternal( ZH_EI_XFREENULL, "zh_xfree called with a NULL pointer!", NULL, NULL );
}

ZH_SIZE zh_xquery( int iMode )
{
   ZH_SIZE nResult = 0;

#ifdef ZH_FM_STATISTICS
   switch( iMode )
   {
      case ZH_MEM_USED:
         nResult = s_nMemoryConsumed;
         break;

      case ZH_MEM_USEDMAX:
         nResult = s_nMemoryMaxConsumed;
         break;
   }
#else
   ZH_SYMBOL_UNUSED( iMode );
#endif
   return nResult;
}

#ifdef ZH_FM_STATISTICS
static char * zh_memToStr( char * szBuffer, void * pMem, ZH_SIZE nSize )
{
   unsigned char * byMem = ( ZH_BYTE * ) pMem;
   char * pDest = szBuffer;
   int iSize, i, iPrintable;

   if( nSize > ZH_MEMSTR_BLOCK_MAX )
      iSize = ZH_MEMSTR_BLOCK_MAX;
   else
      iSize = ( int ) nSize;

   iPrintable = 0;
   for( i = 0; i < iSize; ++i )
      if( ( byMem[ i ] & 0x7f ) >= 0x20 )
         iPrintable++;

   if( ( iPrintable * 100 ) / iSize > 70 ) /* more then 70% printable chars */
   {
      /* format as string of original chars */
      for( i = 0; i < iSize; ++i )
         if( ( byMem[ i ] & 0x7f ) >= 0x20 )
            *pDest++ = byMem[ i ];
         else
            *pDest++ = '.';
   }
   else
   {
      /* format as hex */
      for( i = 0; i < iSize; ++i )
      {
         int iLo = byMem[ i ] & 0x0f, iHi = byMem[ i ] >> 4;
         *pDest++ = '\\';
         *pDest++ = iHi <= 9 ? '0' + iHi : 'A' - 10 + iHi;
         *pDest++ = iLo <= 9 ? '0' + iLo : 'A' - 10 + iLo;
      }
   }
   *pDest = '\0';

   return szBuffer;
}
#endif

void zh_xexit( void )
{
#ifdef ZH_FM_STATISTICS
   if( s_nMemoryBlocks /* || zh_cmdargCheck( "INFO" ) */ )
   {
      char szBuffer[ ZH_MAX( 3 * ZH_MEMSTR_BLOCK_MAX + 1, 100 ) ];
      PZH_MEMINFO pMemBlock;
      int i;

      zh_conOutErr( zh_conNewLine(), 0 );
      zh_conOutErr( "----------------------------------------", 0 );
      zh_conOutErr( zh_conNewLine(), 0 );
      zh_snprintf( szBuffer, sizeof( szBuffer ), "Total memory allocated: %" ZH_PFS "u bytes (%" ZH_PFS "u blocks)", s_nMemoryMaxConsumed, s_nMemoryMaxBlocks );
      zh_conOutErr( szBuffer, 0 );

      if( s_nMemoryBlocks )
      {
         zh_conOutErr( zh_conNewLine(), 0 );
         zh_snprintf( szBuffer, sizeof( szBuffer ), "WARNING! Memory allocated but not released: %" ZH_PFS "u bytes (%" ZH_PFS "u blocks)", s_nMemoryConsumed, s_nMemoryBlocks );
         zh_conOutErr( szBuffer, 0 );
      }

      zh_conOutErr( zh_conNewLine(), 0 );

      for( i = 1, pMemBlock = s_pMemBlocks; pMemBlock; ++i, pMemBlock = pMemBlock->pNextBlock )
         ZH_TRACE( ZH_TR_ERROR, ( "Block %i %p (size %" ZH_PFS "u) \"%s\"", i,
                ( void * ) pMemBlock + ZH_MEMINFO_SIZE, pMemBlock->nSize,
                zh_memToStr( szBuffer, ( char * ) pMemBlock + ZH_MEMINFO_SIZE,
                             pMemBlock->nSize ) ) );
   }
#endif
}

ZH_BOOL zh_xtraced( void )
{
   return ZH_FALSE;
}

/* NOTE: Use as minimal calls from here, as possible.
         Don't allocate memory from this function. [vszakats] */
void zh_errInternal( ZH_ERRCODE errCode, const char * szText, const char * szPar1, const char * szPar2 )
{
   char buffer[ 1024 ];

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_errInternal(%d, %s, %s, %s)", errCode, szText, szPar1, szPar2 ) );

   zh_conOutErr( zh_conNewLine(), 0 );
   zh_snprintf( buffer, sizeof( buffer ), "Unrecoverable error %d: ", errCode );
   zh_conOutErr( buffer, 0 );
   if( szText )
   {
      zh_snprintf( buffer, sizeof( buffer ), szText, szPar1, szPar2 );
      zh_conOutErr( buffer, 0 );
   }
   zh_conOutErr( zh_conNewLine(), 0 );
   exit( EXIT_FAILURE );
}

/* console */
void zh_conOutStd( const char * pStr, ZH_SIZE nLen )
{
   if( nLen == 0 )
      nLen = strlen( pStr );

   fprintf( stdout, "%.*s", ( int ) nLen, pStr );
}

void zh_conOutErr( const char * pStr, ZH_SIZE nLen )
{
   if( nLen == 0 )
      nLen = strlen( pStr );

   fprintf( stderr, "%.*s", ( int ) nLen, pStr );
}

const char * zh_conNewLine( void )
{
   return "\n";
}

int zh_charUpper( int iChar )
{
   return ZH_TOUPPER( iChar );
}

int zh_charLower( int iChar )
{
   return ZH_TOLOWER( iChar );
}

PZH_CODEPAGE zh_vmCDP( void )
{
   return NULL;
}

ZH_SIZE zh_cdpTextPos( PZH_CODEPAGE cdp, const char * pText, ZH_SIZE nSize, ZH_SIZE nIndex )
{
   ZH_SYMBOL_UNUSED( cdp );
   ZH_SYMBOL_UNUSED( pText );

   return nIndex >= nSize ? nSize : nIndex;
}

ZH_BOOL zh_cdpCharEq( PZH_CODEPAGE cdp, const char * szText1, ZH_SIZE nLen1, ZH_SIZE * pnPos1,
                      const char * szText2, ZH_SIZE nLen2, ZH_SIZE * pnPos2 )
{
   ZH_SYMBOL_UNUSED( cdp );

   if( *pnPos1 < nLen1 && *pnPos2 < nLen2 )
      return szText1[ ( *pnPos1 )++ ] == szText2[ ( *pnPos2 )++ ];
   else
      return ZH_FALSE;
}

ZH_BOOL zh_cdpCharCaseEq( PZH_CODEPAGE cdp, const char * szText1, ZH_SIZE nLen1, ZH_SIZE * pnPos1,
                          const char * szText2, ZH_SIZE nLen2, ZH_SIZE * pnPos2 )
{
   ZH_SYMBOL_UNUSED( cdp );

   if( *pnPos1 < nLen1 && *pnPos2 < nLen2 )
   {
      ZH_UCHAR uc1 = szText1[ ( *pnPos1 )++ ],
               uc2 = szText2[ ( *pnPos2 )++ ];
      return ZH_TOUPPER( uc1 ) == ZH_TOUPPER( uc2 );
   }
   else
      return ZH_FALSE;
}

const char * zh_osEncodeCP( const char * szName, char ** pszFree, ZH_SIZE * pnSize )
{
   ZH_SYMBOL_UNUSED( pnSize );
   ZH_SYMBOL_UNUSED( pszFree );
   return szName;
}

const char * zh_osDecodeCP( const char * szName, char ** pszFree, ZH_SIZE * pnSize )
{
   ZH_SYMBOL_UNUSED( pnSize );
   ZH_SYMBOL_UNUSED( pszFree );
   return szName;
}

char * zh_osStrEncode( const char * pszName )
{
   return zh_strdup( pszName );
}

char * zh_osStrEncodeN( const char * pszName, ZH_SIZE nLen )
{
   return zh_strndup( pszName, nLen );
}

char * zh_osStrDecode( const char * pszName )
{
   return zh_strdup( pszName );
}

char * zh_osStrDecode2( const char * pszName, char * pszBuffer, ZH_SIZE nSize )
{
   return zh_strncpy( pszBuffer, pszName, nSize );
}

#if defined( ZH_OS_WIN )
ZH_WCHAR * zh_osStrU16Encode( const char * pszName )
{
   return zh_mbtowc( pszName );
}

ZH_WCHAR * zh_osStrU16EncodeN( const char * pszName, ZH_SIZE nLen )
{
   return zh_mbntowc( pszName, nLen );
}

char * zh_osStrU16Decode( const ZH_WCHAR * pszNameW )
{
   return zh_wctomb( pszNameW );
}

char * zh_osStrU16Decode2( const ZH_WCHAR * pszNameW, char * pszBuffer, ZH_SIZE nSize )
{
   zh_wcntombcpy( pszBuffer, pszNameW, nSize );
   return pszBuffer;
}
#endif


/* ZH_TRACE */
static ZH_TRACEINFO s_traceInfo;

void zh_traceset( int level, const char * file, int line, const char * proc )
{
   s_traceInfo.level = level;
   s_traceInfo.file  = file;
   s_traceInfo.line  = line;
   s_traceInfo.proc  = proc;
}

PZH_TRACEINFO zh_traceinfo( void )
{
   return &s_traceInfo;
}


/* VM */
void zh_vmLock( void ) {}

void zh_vmUnlock( void ) {}

void zh_fsSetIOError( ZH_BOOL fResult, ZH_USHORT uiOperation )
{
   ZH_SYMBOL_UNUSED( fResult );
   ZH_SYMBOL_UNUSED( uiOperation );
}

void  zh_fsSetError( ZH_ERRCODE uiError )
{
   ZH_SYMBOL_UNUSED( uiError );
}

/* file name conversion */

static int     s_iFileCase = ZH_SET_CASE_MIXED;
static int     s_iDirCase  = ZH_SET_CASE_MIXED;
static ZH_BOOL s_fFnTrim   = ZH_FALSE;
static char    s_cDirSep   = ZH_OS_PATH_DELIM_CHR;

const char * zh_fsNameConv( const char * szFileName, char ** pszFree )
{
   if( s_fFnTrim || s_cDirSep != ZH_OS_PATH_DELIM_CHR ||
       s_iFileCase != ZH_SET_CASE_MIXED || s_iDirCase != ZH_SET_CASE_MIXED )
   {
      PZH_FNAME pFileName;

      if( pszFree )
      {
         szFileName = *pszFree = zh_strncpy( ( char * ) zh_xgrab( ZH_PATH_MAX ),
                                             szFileName, ZH_PATH_MAX - 1 );
      }

      if( s_cDirSep != ZH_OS_PATH_DELIM_CHR )
      {
         char * p = ( char * ) ZH_UNCONST( szFileName );
         while( *p )
         {
            if( *p == s_cDirSep )
               *p = ZH_OS_PATH_DELIM_CHR;
            p++;
         }
      }

      pFileName = zh_fsFNameSplit( szFileName );

      /* strip trailing and leading spaces */
      if( s_fFnTrim )
      {
         ZH_SIZE nLen;

         if( pFileName->szName )
         {
            nLen = strlen( pFileName->szName );
            while( nLen && pFileName->szName[ nLen - 1 ] == ' ' )
               --nLen;
            while( nLen && pFileName->szName[ 0 ] == ' ' )
            {
               ++pFileName->szName;
               --nLen;
            }
            ( ( char * ) ZH_UNCONST( pFileName->szName ) )[ nLen ] = '\0';
         }
         if( pFileName->szExtension )
         {
            nLen = strlen( pFileName->szExtension );
            while( nLen && pFileName->szExtension[ nLen - 1 ] == ' ' )
               --nLen;
            while( nLen && pFileName->szExtension[ 0 ] == ' ' )
            {
               ++pFileName->szExtension;
               --nLen;
            }
            ( ( char * ) ZH_UNCONST( pFileName->szExtension ) )[ nLen ] = '\0';
         }
      }

      /* FILECASE */
      if( s_iFileCase == ZH_SET_CASE_LOWER )
      {
         if( pFileName->szName )
            zh_strlow( ( char * ) ZH_UNCONST( pFileName->szName ) );
         if( pFileName->szExtension )
            zh_strlow( ( char * ) ZH_UNCONST( pFileName->szExtension ) );
      }
      else if( s_iFileCase == ZH_SET_CASE_UPPER )
      {
         if( pFileName->szName )
            zh_strupr( ( char * ) ZH_UNCONST( pFileName->szName ) );
         if( pFileName->szExtension )
            zh_strupr( ( char * ) ZH_UNCONST( pFileName->szExtension ) );
      }

      /* DIRCASE */
      if( pFileName->szPath )
      {
         if( s_iDirCase == ZH_SET_CASE_LOWER )
            zh_strlow( ( char * ) ZH_UNCONST( pFileName->szPath ) );
         else if( s_iDirCase == ZH_SET_CASE_UPPER )
            zh_strupr( ( char * ) ZH_UNCONST( pFileName->szPath ) );
      }

      zh_fsFNameMerge( ( char * ) ZH_UNCONST( szFileName ), pFileName );
      zh_xfree( pFileName );
   }
   else if( pszFree )
      *pszFree = NULL;

   return szFileName;
}

#if defined( ZH_OS_WIN )
ZH_WCHAR * zh_fsNameConvU16( const char * szFileName )
{
   char * pszBuffer = NULL;
   ZH_WCHAR * lpwFileName;

   if( s_fFnTrim || s_cDirSep != ZH_OS_PATH_DELIM_CHR ||
       s_iFileCase != ZH_SET_CASE_MIXED || s_iDirCase != ZH_SET_CASE_MIXED )
   {
      PZH_FNAME pFileName;

      szFileName = pszBuffer = zh_strncpy( ( char * ) zh_xgrab( ZH_PATH_MAX ),
                                           szFileName, ZH_PATH_MAX - 1 );

      if( s_cDirSep != ZH_OS_PATH_DELIM_CHR )
      {
         char * p = ( char * ) ZH_UNCONST( szFileName );
         while( *p )
         {
            if( *p == s_cDirSep )
               *p = ZH_OS_PATH_DELIM_CHR;
            p++;
         }
      }

      pFileName = zh_fsFNameSplit( szFileName );

      /* strip trailing and leading spaces */
      if( s_fFnTrim )
      {
         ZH_SIZE nLen;

         if( pFileName->szName )
         {
            nLen = strlen( pFileName->szName );
            while( nLen && pFileName->szName[ nLen - 1 ] == ' ' )
               --nLen;
            while( nLen && pFileName->szName[ 0 ] == ' ' )
            {
               ++pFileName->szName;
               --nLen;
            }
            ( ( char * ) ZH_UNCONST( pFileName->szName ) )[ nLen ] = '\0';
         }
         if( pFileName->szExtension )
         {
            nLen = strlen( pFileName->szExtension );
            while( nLen && pFileName->szExtension[ nLen - 1 ] == ' ' )
               --nLen;
            while( nLen && pFileName->szExtension[ 0 ] == ' ' )
            {
               ++pFileName->szExtension;
               --nLen;
            }
            ( ( char * ) ZH_UNCONST( pFileName->szExtension ) )[ nLen ] = '\0';
         }
      }

      /* FILECASE */
      if( s_iFileCase == ZH_SET_CASE_LOWER )
      {
         if( pFileName->szName )
            zh_strlow( ( char * ) ZH_UNCONST( pFileName->szName ) );
         if( pFileName->szExtension )
            zh_strlow( ( char * ) ZH_UNCONST( pFileName->szExtension ) );
      }
      else if( s_iFileCase == ZH_SET_CASE_UPPER )
      {
         if( pFileName->szName )
            zh_strupr( ( char * ) ZH_UNCONST( pFileName->szName ) );
         if( pFileName->szExtension )
            zh_strupr( ( char * ) ZH_UNCONST( pFileName->szExtension ) );
      }

      /* DIRCASE */
      if( pFileName->szPath )
      {
         if( s_iDirCase == ZH_SET_CASE_LOWER )
            zh_strlow( ( char * ) ZH_UNCONST( pFileName->szPath ) );
         else if( s_iDirCase == ZH_SET_CASE_UPPER )
            zh_strupr( ( char * ) ZH_UNCONST( pFileName->szPath ) );
      }

      zh_fsFNameMerge( ( char * ) ZH_UNCONST( szFileName ), pFileName );
      zh_xfree( pFileName );
   }

   lpwFileName = zh_mbtowc( szFileName );
   if( pszBuffer )
      zh_xfree( pszBuffer );

   return lpwFileName;
}
#endif

int zh_setGetFileCase( void )
{
   return s_iFileCase;
}

int zh_setGetDirCase( void )
{
   return s_iDirCase;
}

int zh_setGetDirSeparator( void )
{
   return s_cDirSep;
}

ZH_BOOL zh_setGetTrimFileName( void )
{
   return s_fFnTrim;
}

void zh_setSetFileCase( int iFileCase )
{
   s_iFileCase = iFileCase;
}

void zh_setSetDirCase( int iDirCase )
{
   s_iDirCase = iDirCase;
}

void zh_setSetDirSeparator( int iSeparator )
{
   s_cDirSep = ( char ) iSeparator;
}

void zh_setSetTrimFileName( ZH_BOOL fTrim )
{
   s_fFnTrim = fTrim;
}
