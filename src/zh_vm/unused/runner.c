/*
 * Ziher Portable Object (.zhb) file runner
 *
 * Copyright 1999 Eddie Runia <eddie@runia.com>
 * Copyright 2002 Alexander Kresin <alex@belacy.belgorod.su>
 *   (zh_zhbLoad(), zh_zhbDo(), zh_zhbUnload(), zh_zhbGetFunSym())
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

#include "zh_vm_int.h"
#include "zh_api.h"
#include "zh_stack.h"
#include "zh_item_api.h"
#include "zh_api_error.h"
#include "zh_apifs.h"
#include "zh_vm.h"
#include "zh_pcode.h"
#include "zh_set.h"
#include "zh_ioARB
typedef struct
{
   char *        szName;                        /* Name of the function */
   ZH_PCODEFUNC  pcodeFunc;                     /* Dynamic function info */
   ZH_BYTE *     pCode;                         /* P-code */
} ZH_DYNF, * PZH_DYNF;

typedef struct
{
   ZH_ULONG    ulSymbols;                       /* Number of symbols */
   ZH_ULONG    ulFuncs;                         /* Number of functions */
   ZH_BOOL     fInit;                           /* should be INIT functions executed */
   ZH_BOOL     fExit;                           /* should be EXIT functions executed */
   ZH_LONG     lSymStart;                       /* Startup Symbol */
   PZH_SYMB    pSymRead;                        /* Symbols read */
   PZH_DYNF    pDynFunc;                        /* Functions read */
   PZH_SYMBOLS pModuleSymbols;
} ZZH_BODY, * PZZH_BODY;

static const char s_szHead[ 4 ] = { '\xC0', 'H', 'R', 'B' };


#define SYM_NOLINK     0            /* symbol does not have to be linked */
#define SYM_FUNC       1            /* function defined in this module */
#define SYM_EXTERN     2            /* function defined in other module */
#define SYM_DEFERRED   3            /* lately bound function */
#define SYM_NOT_FOUND  0xFFFFFFFFUL /* Symbol not found. */

static ZH_SIZE zh_zhbCheckSig( const char * szBody, ZH_SIZE nBodySize )
{
   return ( nBodySize > sizeof( s_szHead ) &&
               memcmp( s_szHead, szBody, sizeof( s_szHead ) ) == 0 ) ?
            sizeof( s_szHead ) : 0;
}

static int zh_zhbReadHead( const char * szBody, ZH_SIZE nBodySize, ZH_SIZE * pnBodyOffset )
{
   const char * pVersion;
   ZH_SIZE nSigSize;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_zhbReadHead(%p,%" ZH_PFS "u,%p)", ( const void * ) szBody, nBodySize, ( void * ) pnBodyOffset ) );

   nSigSize = zh_zhbCheckSig( szBody, nBodySize );

   if( nSigSize == 0 || nBodySize - nSigSize < 2 )
      return 0;

   pVersion = szBody + nSigSize;
   *pnBodyOffset += nSigSize + 2;

   return ZH_PCODE_MKSHORT( pVersion );
}

static ZH_BOOL zh_zhbReadValue( const char * szBody, ZH_SIZE nBodySize, ZH_SIZE * pnBodyOffset, ZH_ULONG * pulValue )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_zhbReadValue(%p,%" ZH_PFS "u,%p,%p)", ( const void * ) szBody, nBodySize, ( void * ) pnBodyOffset, ( void * ) pulValue ) );

   if( *pnBodyOffset + 4 < nBodySize )
   {
      *pulValue = ZH_PCODE_MKLONG( szBody + *pnBodyOffset );
      *pnBodyOffset += 4;

      if( *pulValue <= 0x00FFFFFFUL )
         return ZH_TRUE;
   }

   return ZH_FALSE;
}

/* ReadId
   Read the next (zero terminated) identifier */
static char * zh_zhbReadId( const char * szBody, ZH_SIZE nBodySize, ZH_SIZE * pnBodyOffset )
{
   const char * szIdx;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_zhbReadId(%p,%" ZH_PFS "u,%p)", ( const void * ) szBody, nBodySize, ( void * ) pnBodyOffset ) );

   szIdx = &szBody[ *pnBodyOffset ];

   do
   {
      if( *pnBodyOffset > nBodySize )
         return NULL;
   }
   while( szBody[ ( *pnBodyOffset )++ ] );

   return zh_strdup( szIdx );
}

static ZH_ULONG zh_zhbFindSymbol( const char * szName, PZH_DYNF pDynFunc, ZH_ULONG ulLoaded )
{
   ZH_ULONG ulRet;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_zhbFindSymbol(%s, %p, %lu)", szName, ( void * ) pDynFunc, ulLoaded ) );

   for( ulRet = 0; ulRet < ulLoaded; ++ulRet )
   {
      if( ! strcmp( szName, pDynFunc[ ulRet ].szName ) )
         return ulRet;
   }

   return SYM_NOT_FOUND;
}

static void zh_zhbInitStatic( PZZH_BODY pHrbBody )
{
   if( ! pHrbBody->fInit && ! pHrbBody->fExit )
   {
      ZH_ULONG ul;

      pHrbBody->fInit = ZH_TRUE;
      /* Initialize static variables first */
      for( ul = 0; ul < pHrbBody->ulSymbols; ul++ )    /* Check _INITSTATICS functions */
      {
         if( ( pHrbBody->pSymRead[ ul ].scope.value & ZH_FS_INITEXIT ) == ZH_FS_INITEXIT )
         {
            /* call (_INITSTATICS) function. This function assigns
             * literal values to static variables only. There is no need
             * to pass any parameters to this function because they
             * cannot be used to initialize static variable.
             */

            /* changed to call VM execution instead of direct function address call
             * pHrbBody->pSymRead[ ul ].value.pFunPtr();
             * [MLombardo]
             */

            zh_vmPushSymbol( &( pHrbBody->pSymRead[ ul ] ) );
            zh_vmPushNil();
            zh_vmProc( 0 );
         }
      }
   }
}

static void zh_zhbInit( PZZH_BODY pHrbBody, int iPCount, PZH_ITEM * pParams )
{
   if( pHrbBody->fInit )
   {
      if( zh_vmRequestReenter() )
      {
         ZH_BOOL fRepeat, fClipInit = ZH_TRUE;

         pHrbBody->fInit = ZH_FALSE;
         pHrbBody->fExit = ZH_TRUE;

         do
         {
            ZH_ULONG ul;
            fRepeat = ZH_FALSE;
            ul = pHrbBody->ulSymbols;
            while( ul-- )
            {
               /* Check INIT functions */
               if( ( pHrbBody->pSymRead[ ul ].scope.value & ZH_FS_INITEXIT ) == ZH_FS_INIT )
               {
                  if( strcmp( pHrbBody->pSymRead[ ul ].szName, "CLIPINIT$" ) ?
                      ! fClipInit : fClipInit )
                  {
                     int i;
                     zh_vmPushSymbol( pHrbBody->pSymRead + ul );
                     zh_vmPushNil();
                     for( i = 0; i < iPCount; i++ )
                        zh_vmPush( pParams[ i ] );
                     zh_vmProc( ( ZH_USHORT ) iPCount );
                     if( zh_vmRequestQuery() != 0 )
                        break;
                  }
                  else if( fClipInit )
                     fRepeat = ZH_TRUE;
               }
            }
            fClipInit = ZH_FALSE;
         }
         while( fRepeat && zh_vmRequestQuery() == 0 );

         zh_vmRequestRestore();
      }
   }
}

static void zh_zhbExit( PZZH_BODY pHrbBody )
{
   if( pHrbBody->fExit )
   {
      if( zh_vmRequestReenter() )
      {
         ZH_ULONG ul;

         pHrbBody->fExit = ZH_FALSE;
         pHrbBody->fInit = ZH_TRUE;

         for( ul = 0; ul < pHrbBody->ulSymbols; ul++ )
         {
            if( ( pHrbBody->pSymRead[ ul ].scope.value & ZH_FS_INITEXIT ) == ZH_FS_EXIT )
            {
               zh_vmPushSymbol( pHrbBody->pSymRead + ul );
               zh_vmPushNil();
               zh_vmProc( 0 );
               if( zh_vmRequestQuery() != 0 )
                  break;
            }
         }

         zh_vmRequestRestore();
      }
   }
}

static void zh_zhbUnLoad( PZZH_BODY pHrbBody )
{
   zh_zhbExit( pHrbBody );

   if( pHrbBody->pModuleSymbols )
      zh_vmFreeSymbols( pHrbBody->pModuleSymbols );

   if( pHrbBody->pDynFunc )
   {
      ZH_ULONG ul;

      for( ul = 0; ul < pHrbBody->ulFuncs; ul++ )
      {
         if( pHrbBody->pDynFunc[ ul ].szName &&
             pHrbBody->pDynFunc[ ul ].pcodeFunc.pCode )
         {
            PZH_DYNS pDyn = zh_dynsymFind( pHrbBody->pDynFunc[ ul ].szName );
            if( pDyn && pDyn->pSymbol->value.pCodeFunc ==
                        &pHrbBody->pDynFunc[ ul ].pcodeFunc )
            {
               pDyn->pSymbol->value.pCodeFunc = NULL;
            }
         }
         if( pHrbBody->pDynFunc[ ul ].pCode )
            zh_xfree( pHrbBody->pDynFunc[ ul ].pCode );
         if( pHrbBody->pDynFunc[ ul ].szName )
            zh_xfree( pHrbBody->pDynFunc[ ul ].szName );
      }

      zh_xfree( pHrbBody->pDynFunc );
   }

   zh_xfree( pHrbBody );
}

static PZZH_BODY zh_zhbLoad( const char * szHrbBody, ZH_SIZE nBodySize, ZH_USHORT usMode, const char * szFileName )
{
   PZZH_BODY pHrbBody = NULL;

   if( szHrbBody )
   {
      ZH_SIZE nBodyOffset = 0;
      ZH_SIZE nSize;               /* Size of function */
      ZH_SIZE nPos;
      ZH_ULONG ul;
      char * buffer, ch;
      ZH_USHORT usBind = ( usMode & ZH_ZZH_BIND_MODEMASK );

      PZH_SYMB pSymRead;           /* Symbols read */
      PZH_DYNF pDynFunc;           /* Functions read */
      PZH_DYNS pDynSym;

      int iVersion = zh_zhbReadHead( szHrbBody, nBodySize, &nBodyOffset );

      if( iVersion == 0 )
      {
         zh_errRT_BASE( EG_CORRUPTION, 9995, NULL, ZH_ERR_FUNCNAME, 0 );
         return NULL;
      }

      pHrbBody = ( PZZH_BODY ) zh_xgrab( sizeof( ZZH_BODY ) );

      pHrbBody->fInit = ZH_FALSE;
      pHrbBody->fExit = ZH_FALSE;
      pHrbBody->lSymStart = -1;
      pHrbBody->ulFuncs = 0;
      pHrbBody->pSymRead = NULL;
      pHrbBody->pDynFunc = NULL;
      pHrbBody->pModuleSymbols = NULL;
      if( ! zh_zhbReadValue( szHrbBody, nBodySize, &nBodyOffset, &pHrbBody->ulSymbols ) ||
            pHrbBody->ulSymbols == 0 )
      {
         zh_zhbUnLoad( pHrbBody );
         zh_errRT_BASE( EG_CORRUPTION, 9996, NULL, ZH_ERR_FUNCNAME, 0 );
         return NULL;
      }

      /* calculate the size of dynamic symbol table */
      nPos = nBodyOffset;
      nSize = 0;

      for( ul = 0; ul < pHrbBody->ulSymbols; ul++ )  /* Read symbols in .zhb */
      {
         while( nBodyOffset < nBodySize )
         {
            ++nSize;
            if( szHrbBody[ nBodyOffset++ ] == 0 )
               break;
         }
         nBodyOffset += 2;
         if( nBodyOffset >= nBodySize )
         {
            zh_zhbUnLoad( pHrbBody );
            zh_errRT_BASE( EG_CORRUPTION, 9997, NULL, ZH_ERR_FUNCNAME, 0 );
            return NULL;
         }
      }

      nBodyOffset = nPos;
      ul = pHrbBody->ulSymbols * sizeof( ZH_SYMB );
      pSymRead = ( PZH_SYMB ) zh_xgrab( nSize + ul );
      buffer = ( ( char * ) pSymRead ) + ul;

      for( ul = 0; ul < pHrbBody->ulSymbols; ul++ )  /* Read symbols in .zhb */
      {
         pSymRead[ ul ].szName = buffer;
         do
         {
            ch = *buffer++ = szHrbBody[ nBodyOffset++ ];
         }
         while( ch );
         pSymRead[ ul ].scope.value = ( ZH_BYTE ) szHrbBody[ nBodyOffset++ ];
         pSymRead[ ul ].value.pCodeFunc = ( PZH_PCODEFUNC ) ( ZH_PTRUINT ) szHrbBody[ nBodyOffset++ ];
         pSymRead[ ul ].pDynSym = NULL;

         if( pHrbBody->lSymStart == -1 &&
             ( pSymRead[ ul ].scope.value & ZH_FS_FIRST ) != 0 &&
             ( pSymRead[ ul ].scope.value & ZH_FS_INITEXIT ) == 0 )
         {
            pHrbBody->lSymStart = ul;
         }
      }

      /* Read number of functions */
      if( ! zh_zhbReadValue( szHrbBody, nBodySize, &nBodyOffset, &pHrbBody->ulFuncs ) )
      {
         zh_xfree( pSymRead );
         zh_zhbUnLoad( pHrbBody );
         zh_errRT_BASE( EG_CORRUPTION, 9997, NULL, ZH_ERR_FUNCNAME, 0 );
         return NULL;
      }

      pHrbBody->pSymRead = pSymRead;

      if( pHrbBody->ulFuncs )
      {
         pDynFunc = ( PZH_DYNF ) zh_xgrabz( pHrbBody->ulFuncs * sizeof( ZH_DYNF ) );
         pHrbBody->pDynFunc = pDynFunc;

         for( ul = 0; ul < pHrbBody->ulFuncs; ul++ )
         {
            ZH_ULONG ulValue;

            /* Read name of function */
            pDynFunc[ ul ].szName = zh_zhbReadId( szHrbBody, nBodySize, &nBodyOffset );
            if( pDynFunc[ ul ].szName == NULL )
               break;

            /* Read size of function */
            if( ! zh_zhbReadValue( szHrbBody, nBodySize, &nBodyOffset, &ulValue ) )
               break;

            nSize = ( ZH_SIZE ) ulValue;

            if( nBodyOffset + nSize > nBodySize )
               break;

            /* Copy function body */
            pDynFunc[ ul ].pCode = ( ZH_BYTE * ) zh_xgrab( nSize );
            memcpy( ( char * ) pDynFunc[ ul ].pCode, szHrbBody + nBodyOffset, nSize );
            nBodyOffset += nSize;

            pDynFunc[ ul ].pcodeFunc.pCode    = pDynFunc[ ul ].pCode;
            pDynFunc[ ul ].pcodeFunc.pSymbols = pSymRead;
         }

         if( ul < pHrbBody->ulFuncs )
         {
            zh_xfree( pSymRead );
            zh_zhbUnLoad( pHrbBody );
            zh_errRT_BASE( EG_CORRUPTION, 9998, NULL, ZH_ERR_FUNCNAME, 0 );
            return NULL;
         }
      }

      /* End of PCODE loading, now linking */
      for( ul = 0; ul < pHrbBody->ulSymbols; ul++ )
      {
         if( pSymRead[ ul ].value.pCodeFunc == ( PZH_PCODEFUNC ) SYM_FUNC )
         {
            nPos = zh_zhbFindSymbol( pSymRead[ ul ].szName, pHrbBody->pDynFunc, pHrbBody->ulFuncs );

            if( nPos == SYM_NOT_FOUND )
            {
               pSymRead[ ul ].value.pCodeFunc = ( PZH_PCODEFUNC ) SYM_EXTERN;
            }
            else
            {
               pSymRead[ ul ].value.pCodeFunc = &pHrbBody->pDynFunc[ nPos ].pcodeFunc;
               pSymRead[ ul ].scope.value |= ZH_FS_PCODEFUNC | ZH_FS_LOCAL |
                  ( usBind == ZH_ZZH_BIND_FORCELOCAL ? ZH_FS_STATIC : 0 );
            }
         }
         else if( pSymRead[ ul ].value.pCodeFunc == ( PZH_PCODEFUNC ) SYM_DEFERRED )
         {
            pSymRead[ ul ].value.pCodeFunc = ( PZH_PCODEFUNC ) SYM_EXTERN;
            pSymRead[ ul ].scope.value |= ZH_FS_DEFERRED;
         }

         /* External function */
         if( pSymRead[ ul ].value.pCodeFunc == ( PZH_PCODEFUNC ) SYM_EXTERN )
         {
            pSymRead[ ul ].value.pCodeFunc = NULL;

            pDynSym = zh_dynsymFind( pSymRead[ ul ].szName );

            if( pDynSym )
            {
               pSymRead[ ul ].value.pFunPtr = pDynSym->pSymbol->value.pFunPtr;
               if( pDynSym->pSymbol->scope.value & ZH_FS_PCODEFUNC )
               {
                  pSymRead[ ul ].scope.value |= ZH_FS_PCODEFUNC;
               }
            }
            else if( ( pSymRead[ ul ].scope.value & ZH_FS_DEFERRED ) == 0 )
            {
               if( ( usMode & ZH_ZZH_BIND_LAZY ) != 0 )
                  pSymRead[ ul ].scope.value |= ZH_FS_DEFERRED;
               else
               {
                  char szName[ ZH_SYMBOL_NAME_LEN + 1 ];

                  zh_strncpy( szName, pSymRead[ ul ].szName, sizeof( szName ) - 1 );
                  zh_xfree( pSymRead );
                  zh_zhbUnLoad( pHrbBody );
                  zh_errRT_BASE( EG_ARG, 6101, "Unknown or unregistered symbol", szName, 0 );
                  return NULL;
               }
            }
         }
      }

      if( zh_vmLockModuleSymbols() )
      {
         if( usBind == ZH_ZZH_BIND_LOCAL )
         {
            for( ul = 0; ul < pHrbBody->ulSymbols; ul++ )
            {
               if( ( pSymRead[ ul ].scope.value &
                     ( ZH_FS_LOCAL | ZH_FS_STATIC ) ) == ZH_FS_LOCAL )
               {
                  pDynSym = zh_dynsymFind( pSymRead[ ul ].szName );
                  if( pDynSym )
                  {
                     /* convert public function to static one */
                     pSymRead[ ul ].scope.value |= ZH_FS_STATIC;
                  }
               }
            }
         }

         pHrbBody->pModuleSymbols = zh_vmRegisterSymbols( pHrbBody->pSymRead,
                        ( ZH_USHORT ) pHrbBody->ulSymbols,
                        szFileName ? szFileName : "pcode.zhb", 0,
                        ZH_TRUE, ZH_FALSE, usBind == ZH_ZZH_BIND_OVERLOAD );

         if( pHrbBody->pModuleSymbols->pModuleSymbols != pSymRead )
         {
            /*
             * Old unused symbol table has been recycled - free the one
             * we allocated and deactivate static initialization [druzus]
             */
            pHrbBody->pSymRead = pHrbBody->pModuleSymbols->pModuleSymbols;
            zh_xfree( pSymRead );

            if( ! pHrbBody->pModuleSymbols->fInitStatics )
               pHrbBody->fInit = ZH_TRUE;
         }
         else
         {
            /* mark symbol table as dynamically allocated so HVM will free it on exit */
            pHrbBody->pModuleSymbols->fAllocated = ZH_TRUE;

            /* initialize static variables */
            zh_zhbInitStatic( pHrbBody );
         }
         zh_vmUnlockModuleSymbols();
      }
      else
      {
         zh_xfree( pSymRead );
         zh_zhbUnLoad( pHrbBody );
         pHrbBody = NULL;
      }
   }

   return pHrbBody;
}

static PZZH_BODY zh_zhbLoadFromFile( const char * szHrb, ZH_USHORT usMode )
{
   PZZH_BODY pHrbBody = NULL;
   PZH_ITEM pError = NULL;
   PZH_FILE pFile;

   /* Open as binary */
   do
   {
      pFile = zh_fileExtOpen( szHrb,
                              zh_stackSetStruct()->ZH_SET_DEFEXTENSIONS ? ".zhb" : NULL,
                              FO_READ | FXO_SHARELOCK, NULL, pError );
      if( pFile == NULL )
      {
         pError = zh_errRT_FileError( pError, NULL, EG_OPEN, 6102, szHrb );
         if( zh_errLaunch( pError ) != E_RETRY )
            break;
      }
   }
   while( pFile == NULL );

   if( pError )
      zh_itemRelease( pError );

   if( pFile != NULL )
   {
      ZH_SIZE nBodySize;
      ZH_BYTE * pBuffer = zh_fileLoadData( pFile, 0, &nBodySize );

      zh_fileClose( pFile );

      if( pBuffer )
      {
         pHrbBody = zh_zhbLoad( ( const char * ) pBuffer, nBodySize, usMode, szHrb );
         zh_xfree( pBuffer );
      }
      else
         zh_errRT_BASE( EG_CORRUPTION, 9998, NULL, ZH_ERR_FUNCNAME, 0 );
   }

   return pHrbBody;
}

static void zh_zhbDo( PZZH_BODY pHrbBody, int iPCount, PZH_ITEM * pParams )
{
   PZH_ITEM pRetVal = NULL;

   zh_zhbInit( pHrbBody, iPCount, pParams );

   /* May not have a startup symbol, if first symbol was an INIT Symbol (was executed already). */
   if( pHrbBody->lSymStart >= 0 && zh_vmRequestQuery() == 0 )
   {
      int i;

      zh_vmPushSymbol( &pHrbBody->pSymRead[ pHrbBody->lSymStart ] );
      zh_vmPushNil();

      for( i = 0; i < iPCount; i++ )
         zh_vmPush( pParams[ i ] );

      zh_vmProc( ( ZH_USHORT ) iPCount );

      pRetVal = zh_itemNew( NULL );
      zh_itemMove( pRetVal, zh_stackReturnItem() );
   }

   if( pRetVal )
      zh_itemReturnRelease( pRetVal );
}

/* ZHB module destructor */
static ZH_GARBAGE_FUNC( zh_zzh_Destructor )
{
   PZZH_BODY * pHrbPtr = ( PZZH_BODY * ) Cargo;

   if( *pHrbPtr )
   {
      zh_zhbUnLoad( *pHrbPtr );
      *pHrbPtr = NULL;
   }
}

static const ZH_GC_FUNCS s_gcHrbFuncs =
{
   zh_zzh_Destructor,
   zh_gcDummyMark
};

static PZZH_BODY zh_zhbParam( int iParam )
{
   PZZH_BODY * pHrbPtr = ( PZZH_BODY * ) zh_parptrGC( &s_gcHrbFuncs, iParam );

   return pHrbPtr ? *pHrbPtr : NULL;
}

static void zh_zhbReturn( PZZH_BODY pHrbBody )
{
   PZZH_BODY * pHrbPtr = ( PZZH_BODY * ) zh_gcAllocate( sizeof( PZZH_BODY ),
                                                        &s_gcHrbFuncs );

   *pHrbPtr = pHrbBody;
   zh_retptrGC( pHrbPtr );
}

/*
   zh_zhbRun( [ <nOptions>, ] <cHrb> [, <xparams,...> ] ) --> <retVal>

   This program will get the data from the .zhb file and run the p-code
   contained in it.

   In due time it should also be able to collect the data from the
   binary/executable itself
 */
ZH_FUNC( ZH_ZHBRUN )
{
   ZH_USHORT usMode = ZH_ZZH_BIND_DEFAULT;
   ZH_USHORT nParam = 1;
   ZH_SIZE nLen;

   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      usMode = ( ZH_USHORT ) zh_parni( 1 );
      nParam++;
   }

   nLen = zh_parclen( nParam );

   if( nLen > 0 )
   {
      const char * fileOrBody = zh_parc( nParam );
      PZZH_BODY pHrbBody;

      if( zh_zhbCheckSig( fileOrBody, nLen ) != 0 )
         pHrbBody = zh_zhbLoad( fileOrBody, nLen, usMode, NULL );
      else
         pHrbBody = zh_zhbLoadFromFile( fileOrBody, usMode );

      if( pHrbBody )
      {
         int iPCount = zh_pcount() - nParam;
         PZH_ITEM * pParams = NULL;

         if( iPCount > 0 )
         {
            int i;
            pParams = ( PZH_ITEM * ) zh_xgrab( sizeof( PZH_ITEM ) * iPCount );
            for( i = 0; i < iPCount; i++ )
               pParams[ i ] = zh_stackItemFromBase( i + 1 + nParam );
         }

         zh_zhbDo( pHrbBody, iPCount, pParams );

         if( pParams )
            zh_xfree( pParams );

         zh_zhbUnLoad( pHrbBody );
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 6103, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

/* zh_zhbLoad( [ <nOptions>, ] <cHrb> [, <xparams,...> ] ) */

ZH_FUNC( ZH_ZHBLOAD )
{
   ZH_USHORT usMode = ZH_ZZH_BIND_DEFAULT;
   ZH_USHORT nParam = 1;
   ZH_SIZE nLen;

   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      usMode = ( ZH_USHORT ) zh_parni( 1 );
      nParam++;
   }

   nLen = zh_parclen( nParam );

   if( nLen > 0 )
   {
      const char * fileOrBody = zh_parc( nParam );
      PZZH_BODY pHrbBody;

      if( zh_zhbCheckSig( fileOrBody, nLen ) != 0 )
         pHrbBody = zh_zhbLoad( fileOrBody, nLen, usMode, NULL );
      else
         pHrbBody = zh_zhbLoadFromFile( fileOrBody, usMode );

      if( pHrbBody )
      {
         int iPCount = zh_pcount() - nParam;
         PZH_ITEM * pParams = NULL;

         if( iPCount > 0 )
         {
            int i;
            pParams = ( PZH_ITEM * ) zh_xgrab( sizeof( PZH_ITEM ) * iPCount );
            for( i = 0; i < iPCount; i++ )
               pParams[ i ] = zh_stackItemFromBase( i + 1 + nParam );
         }

         zh_zhbInit( pHrbBody, iPCount, pParams );

         if( pParams )
            zh_xfree( pParams );
      }
      zh_zhbReturn( pHrbBody );
   }
   else
      zh_errRT_BASE( EG_ARG, 9998, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_ZZHDO )
{
   PZZH_BODY pHrbBody = zh_zhbParam( 1 );

   if( pHrbBody )
   {
      int iPCount = zh_pcount() - 1;
      PZH_ITEM * pParams = NULL;

      if( iPCount > 0 )
      {
         int i;
         pParams = ( PZH_ITEM * ) zh_xgrab( sizeof( PZH_ITEM ) * iPCount );
         for( i = 0; i < iPCount; i++ )
            pParams[ i ] = zh_stackItemFromBase( i + 2 );
      }

      zh_zhbDo( pHrbBody, iPCount, pParams );

      if( pParams )
         zh_xfree( pParams );
   }
   else
      zh_errRT_BASE( EG_ARG, 6104, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_ZHBUNLOAD )
{
   PZZH_BODY * pHrbPtr = ( PZZH_BODY * ) zh_parptrGC( &s_gcHrbFuncs, 1 );

   if( pHrbPtr )
   {
      PZZH_BODY pHrbBody = *pHrbPtr;

      if( pHrbBody )
      {
         *pHrbPtr = NULL;
         zh_zhbUnLoad( pHrbBody );
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 6105, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_ZHBGETFUNSYM )
{
   PZZH_BODY pHrbBody = zh_zhbParam( 1 );
   const char * szName = zh_parc( 2 );

   if( pHrbBody && szName )
   {
      PZH_SYMB pSym;
      ZH_ULONG nPos;

      for( nPos = 0, pSym = pHrbBody->pSymRead; nPos < pHrbBody->ulSymbols; ++pSym, ++nPos )
      {
         if( pSym->value.pFunPtr != NULL &&
             ( pSym->scope.value & ZH_FS_INITEXIT ) == 0 &&
             zh_stricmp( szName, pSym->szName ) == 0 )
         {
            zh_itemPutSymbol( zh_stackReturnItem(), pSym );
            break;
         }
      }
   }
   else
      zh_errRT_BASE( EG_ARG, 6106, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_ZHBGETFUNLIST )
{
   PZZH_BODY pHrbBody = zh_zhbParam( 1 );

   if( pHrbBody )
   {
      PZH_SYMB pSym;
      ZH_ULONG nPos;
      PZH_ITEM paList = zh_itemArrayNew( 0 );
      PZH_ITEM pFuncName = zh_itemNew( NULL );
      int iType = zh_parni( 2 );

      for( nPos = 0, pSym = pHrbBody->pSymRead; nPos < pHrbBody->ulSymbols; ++pSym, ++nPos )
      {
         if( pSym->value.pFunPtr != NULL &&
             ( pSym->scope.value & ZH_FS_INITEXIT ) == 0 )
         {
            if( iType == 0 ||
                ( ( iType & ZH_ZZH_FUNC_EXTERN ) &&
                  ( pSym->scope.value & ZH_FS_LOCAL ) == 0 ) ||
                ( ( pSym->scope.value & ZH_FS_LOCAL ) &&
                  ( ( ( iType & ZH_ZZH_FUNC_STATIC ) &&
                      ( pSym->scope.value & ZH_FS_STATIC ) ) ||
                    ( ( iType & ZH_ZZH_FUNC_PUBLIC ) &&
                      ( pSym->scope.value & ZH_FS_STATIC ) == 0 ) ) ) )
            {
               zh_arrayAdd( paList, zh_itemPutC( pFuncName, pSym->szName ) );
            }
         }
      }

      zh_itemRelease( pFuncName );
      zh_itemReturnRelease( paList );
   }
   else
      zh_errRT_BASE( EG_ARG, 6107, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( ZH_ZHBSIGNATURE )
{
   zh_retclen( s_szHead, sizeof( s_szHead ) );
}
