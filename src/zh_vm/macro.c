/*
 * Macro compiler main file
 *
 * Copyright 1999 Ryszard Glab <rglab@imid.med.pl>
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

/* this #define HAS TO be placed before all #include directives
 */
#ifndef ZH_MACRO_SUPPORT
#  define ZH_MACRO_SUPPORT
#endif

#include "zh_vm_opt.h"
#include "zh_macro.h"
#include "zh_comp.h"
#include "zh_stack.h"

/* various flags for macro compiler */
#ifndef ZH_SM_DEFAULT
#     define ZH_SM_DEFAULT  ( ZH_SM_SHORTCUTS | ZH_SM_ZIHER )
#endif


static void zh_macroFlagsInit( void * pFlags )
{
   *( ( int * ) pFlags ) = ZH_SM_DEFAULT;
}

static ZH_TSD_NEW( s_macroFlags, sizeof( int ), zh_macroFlagsInit, NULL );

static int zh_macroFlags( void )
{
   return *( ( int * ) zh_stackGetTSD( &s_macroFlags ) );
}

static void zh_macroFlagsSet( int flag )
{
   *( ( int * ) zh_stackGetTSD( &s_macroFlags ) ) = flag;
}


#define ZH_SM_ISUSERCP()         ( ZH_CODEPAGE_ISCHARUNI( zh_vmCodepage() ) ? ZH_COMPFLAG_USERCP : 0 )

/* - */

/* Compile passed string into a pcode buffer
 *
 * 'pMacro' - pointer to ZH_MACRO structure that will hold all information
 *    needed for macro compilation and evaluation
 * 'szString'  - a string to compile
 * 'iFlag' - specifies if compiled code should generate pcodes either for push
 *    operation (for example: var :=&macro) or for pop operation (&macro :=var)
 */
static int zh_macroParse( PZH_MACRO pMacro )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_macroParse(%p)", ( void * ) pMacro ) );

   /* initialize the output (pcode) buffer - it will be filled by yacc */
   pMacro->pCodeInfo = &pMacro->pCodeInfoBuffer;
   pMacro->pCodeInfo->nPCodeSize = ZH_PCODE_SIZE;
   pMacro->pCodeInfo->nPCodePos  = 0;
   pMacro->pCodeInfo->fVParams   = ZH_FALSE;
   pMacro->pCodeInfo->pLocals    = NULL;
   pMacro->pCodeInfo->pPrev      = NULL;
   pMacro->pCodeInfo->pCode      = ( ZH_BYTE * ) zh_xgrab( ZH_PCODE_SIZE );

   /* reset the type of compiled expression - this should be filled after
    * successfully compilation
    */
   pMacro->pError = NULL;
   pMacro->uiListElements = 0;
   pMacro->exprType = ZH_ET_NONE;

   return zh_macroYYParse( pMacro );
}

/* releases all memory allocated for macro evaluation
 * NOTE:
 *    Only members of ZH_MACRO structure are deallocated
 *    the 'pMacro' pointer is not released - it can be a pointer
 *    to a memory allocated on the stack.
 */
static void zh_macroClear( PZH_MACRO pMacro )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_macroClear(%p)", ( void * ) pMacro ) );

   zh_xfree( pMacro->pCodeInfo->pCode );
   if( pMacro->pError )
      zh_errRelease( pMacro->pError );
}

void zh_macroDelete( PZH_MACRO pMacro )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_macroDelete(%p)", ( void * ) pMacro ) );

   zh_macroClear( pMacro );
   zh_xfree( pMacro );
}

/* checks if a correct ITEM was passed from the virtual machine eval stack
 */
static ZH_BOOL zh_macroCheckParam( PZH_ITEM pItem )
{
   ZH_BOOL bValid = ZH_TRUE;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_macroCheckParam(%p)", ( void * ) pItem ) );

   if( ! ZH_IS_STRING( pItem ) )
   {
      PZH_ITEM pResult = zh_errRT_BASE_Subst( EG_ARG, 1065, NULL, "&", 1, pItem );

      bValid = ZH_FALSE;
      if( pResult )
      {
         ZH_STACK_TLS_PRELOAD
         zh_stackPop();
         zh_vmPush( pResult );
         zh_itemRelease( pResult );
      }
   }
   return bValid;
}

/* It handles an error generated during checking of expression type
 */
static ZH_ERROR_HANDLE( zh_macroErrorType )
{
   PZH_MACRO pMacro = ( PZH_MACRO ) ErrorInfo->Cargo;

   /* copy error object for later diagnostic usage */
   if( ! pMacro->pError )
      pMacro->pError = zh_itemNew( ErrorInfo->Error );

   pMacro->status &= ~ZH_MACRO_CONT;

   /* ignore rest of compiled code */
   zh_vmRequestEndProc();

   return NULL;   /* ignore this error */
}


/* Executes pcode compiled by macro compiler
 *
 * pMacro is a pointer to ZH_MACRO structure created by macro compiler
 *
 */
void zh_macroRun( PZH_MACRO pMacro )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_macroRun(%p)", ( void * ) pMacro ) );

   zh_vmExecute( pMacro->pCodeInfo->pCode, NULL );
}

static void zh_macroSyntaxError( PZH_MACRO pMacro )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_macroSyntaxError(%p)", ( void * ) pMacro ) );

   if( pMacro && pMacro->pError )
   {
      ZH_TRACE( ZH_TR_DEBUG, ( "zh_macroSyntaxError.(%s)", pMacro->string ) );

      zh_stackPop();    /* remove compiled string */

      zh_errLaunch( pMacro->pError );
      zh_errRelease( pMacro->pError );
      pMacro->pError = NULL;
   }
   else
   {
      PZH_ITEM pResult = zh_errRT_BASE_Subst( EG_SYNTAX, 1449, NULL, "&", 1, zh_stackItemFromTop( -1 ) );

      if( pResult )
      {
         zh_stackPop();    /* remove compiled string */
         zh_vmPush( pResult );
         zh_itemRelease( pResult );
      }
   }
}

/* This replaces all '&var' or '&var.' occurrences within a given string
 * with the value of variable 'var' if this variable exists and contains
 * a string value. The value of variable is also searched for
 * occurrences of macro operator and if it is found then it is expanded
 * until there is no more macro operators.
 * NOTE:
 *    this does not evaluate a macro expression - there is a simple text
 *    substitution only
 * NOTE:
 *    zh_macroTextSubst returns either a pointer that points to the passed
 *    string if there was no macro operator in it or a pointer to a new
 *    allocated memory with expanded string if there was a macro operator
 *    in passed string.
 *
 */
static char * zh_macroTextSubst( const char * szString, ZH_SIZE * pnStringLen )
{
   char * szResult;
   ZH_SIZE nResStrLen;
   ZH_SIZE nResBufLen;
   ZH_SIZE nCharsLeft;
   char * pHead;
   char * pTail;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_macroTextSubst(%s, %" ZH_PFS "u)", szString, *pnStringLen ) );

   pHead = ( char * ) memchr( szString, '&', *pnStringLen );
   if( pHead == NULL )
      return ( char * ) ZH_UNCONST( szString );  /* no more processing is required */

   /* initial length of the string and the result buffer (it can contain null bytes) */
   nResBufLen = nResStrLen = *pnStringLen;
   /* initial buffer for return value */
   szResult = ( char * ) zh_xgrab( nResBufLen + 1 );

   /* copy the input string with trailing zero byte
    */
   memcpy( szResult, szString, nResStrLen + 1 );
   /* switch the pointer so it will point into the result buffer
    */
   pHead = szResult + ( pHead - szString );

   do
   {
      /* store the position where '&' was found so we can restart scanning
       * from this point after macro expansion
       */
      pTail = pHead;
      /* check if the next character can start a valid identifier
       * (only _a-zA-Z are allowed)
       */
      ++pHead;    /* skip '&' character */
      if( *pHead == '_' ||
          ( *pHead >= 'A' && *pHead <= 'Z' ) ||
          ( *pHead >= 'a' && *pHead <= 'z' ) )
      {
         /* extract a variable name */
         /* NOTE: the extracted name can be longer then supported maximal
          * length of identifiers (ZH_SYMBOL_NAME_LEN) - only the max allowed
          * are used for name lookup however the whole string is replaced
          */
         ZH_SIZE nNameLen = 1;
         char * pName = pHead;

         while( *++pHead && ( *pHead == '_' ||
                              ( *pHead >= 'A' && *pHead <= 'Z' ) ||
                              ( *pHead >= 'a' && *pHead <= 'z' ) ||
                              ( *pHead >= '0' && *pHead <= '9' ) ) )
         {
            ++nNameLen;
         }
         /* pHead points now at the character that terminated a variable name */

         /* NOTE: '_' is invalid variable name
          */
         if( nNameLen > 1 || *pName != '_' )
         {
            /* this is not the "&_" string */
            char * szValPtr;
            ZH_SIZE nValLen;

            /* Get a pointer to the string value stored in this variable
             * or NULL if variable doesn't exist or doesn't contain a string
             * value.
             * NOTE: This doesn't create a copy of the value then it
             * shouldn't be released here.
             */
            nValLen = nNameLen;   /* the length of name */
            szValPtr = zh_memvarGetStrValuePtr( pName, &nValLen );
            if( szValPtr )
            {
               if( *pHead == '.' )
               {
                  /* we have stopped at the macro terminator '.' - skip it */
                  ++pHead;
                  ++nNameLen;
               }
               ++nNameLen;   /* count also the '&' character */

               /* number of characters left on the right side of a variable name */
               nCharsLeft = nResStrLen - ( pHead - szResult );

               /* NOTE:
                * if a replacement string is shorter then the variable
                * name then we don't have to reallocate the result buffer:
                * 'nResStrLen' stores the current length of a string in the buffer
                * 'nResBufLen' stores the length of the buffer
                */
               if( nValLen > nNameLen )
               {
                  nResStrLen += ( nValLen - nNameLen );
                  if( nResStrLen > nResBufLen )
                  {
                     ZH_SIZE nHead = pHead - szResult;
                     ZH_SIZE nTail = pTail - szResult;
                     nResBufLen = nResStrLen;
                     szResult = ( char * ) zh_xrealloc( szResult, nResBufLen + 1 );
                     pHead = szResult + nHead;
                     pTail = szResult + nTail;
                  }
               }
               else
                  nResStrLen -= ( nNameLen - nValLen );

               /* move bytes located on the right side of a variable name */
               memmove( pTail + nValLen, pHead, nCharsLeft + 1 );
               /* copy substituted value */
               memcpy( pTail, szValPtr, nValLen );
               /* restart scanning from the beginning of replaced string */
               /* NOTE: This causes that the following code:
                *    a := '&a'
                *    var := '&a.b'
                * is the same as:
                *    var := '&ab'
                */
               pHead = pTail;
            }
         }
      }
      nCharsLeft = nResStrLen - ( pHead - szResult );
   }
   while( nCharsLeft && ( pHead = ( char * ) memchr( pHead, '&', nCharsLeft ) ) != NULL );

   if( nResStrLen < nResBufLen )
   {
      /* result string is shorter then allocated buffer -
       * cut it to a required length
       */
      szResult = ( char * ) zh_xrealloc( szResult, nResStrLen + 1 );
   }
   szResult[ nResStrLen ] = 0;  /* place terminating null character */
   /* return a length of result string */
   *pnStringLen = nResStrLen;

   return szResult;   /* a new memory buffer was allocated */
}


/* NOTE:
 *   This will be called when macro variable or macro expression is
 * placed on the right side of the assignment or when it is used as
 * a parameter.
 * PUSH operation
 * iContext contains additional info when ZH_SM_XBASE is enabled
 *  = ZH_P_MACRO_PUSHLIST
 *  = ZH_P_MACRO_PUSHPARE
 *
 * iContext contains ZH_P_MACRO_PUSHPARE if a macro is used inside a codeblock
 * Eval( {|| &macro } )
 *
 */

void zh_macroGetValue( PZH_ITEM pItem, int iContext, int flags )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_macroGetValue(%p)", ( void * ) pItem ) );

   if( zh_macroCheckParam( pItem ) )
   {
      ZH_MACRO struMacro;
      int iStatus;
      char * pszFree;

      struMacro.mode      = ZH_MODE_MACRO;
      struMacro.supported = ( ( flags & ZH_SM_RT_MACRO ) ? zh_macroFlags() : flags ) |
                            ZH_SM_ISUSERCP();
      struMacro.Flags     = ZH_MACRO_GEN_PUSH;
      struMacro.uiNameLen = ZH_SYMBOL_NAME_LEN;
      struMacro.status    = ZH_MACRO_CONT;
      struMacro.length    = pItem->item.asString.length;
   
      pszFree = zh_macroTextSubst( pItem->item.asString.value, &struMacro.length );
      struMacro.string = pszFree;
      if( pszFree == pItem->item.asString.value )
         pszFree = NULL;

      if( iContext != 0 )
      {
         /*
          * If compiled in Xbase++ compatibility mode:
          * macro := "1,2"
          * funCall( &macro )  ==>  funCall( 1, 2 )
          * { &macro }  ==>  { 1, 2 }
          * var[ &macro ]  ==>  var[ 1, 2 ]
          * var := (somevalue, &macro)  ==> var := 2
          *
          * Always:
          * macro := "1,2"
          * Eval( {|| &macro } )
          *
          */
         struMacro.Flags |= ZH_MACRO_GEN_LIST;
         if( iContext == ZH_P_MACRO_PUSHPARE )
         {
            struMacro.Flags |= ZH_MACRO_GEN_PARE;
         }
      }

      iStatus = zh_macroParse( &struMacro );

      if( iStatus == ZH_MACRO_OK && ( struMacro.status & ZH_MACRO_CONT ) )
      {
         zh_stackPop();    /* remove compiled string */
         zh_macroRun( &struMacro );

         if( iContext == ZH_P_MACRO_PUSHLIST )
            zh_vmPushLong( struMacro.uiListElements + 1 );
      }
      else
         zh_macroSyntaxError( &struMacro );

      if( pszFree )
         zh_xfree( pszFree );

      zh_macroClear( &struMacro );
   }
   else if( iContext == ZH_P_MACRO_PUSHLIST && zh_vmRequestQuery() == 0 )
   {
      zh_vmPushInteger( 1 );
   }
}

/* NOTE:
 *   This will be called when macro variable or macro expression is
 * placed on the left side of the assignment
 * POP operation
 */
void zh_macroSetValue( PZH_ITEM pItem, int flags )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_macroSetValue(%p)", ( void * ) pItem ) );

   if( zh_macroCheckParam( pItem ) )
   {
      ZH_MACRO struMacro;
      int iStatus;

      struMacro.mode      = ZH_MODE_MACRO;
      struMacro.supported = ( ( flags & ZH_SM_RT_MACRO ) ? zh_macroFlags() : flags ) |
                            ZH_SM_ISUSERCP();
      struMacro.Flags     = ZH_MACRO_GEN_POP;
      struMacro.uiNameLen = ZH_SYMBOL_NAME_LEN;
      struMacro.status    = ZH_MACRO_CONT;
      struMacro.string    = pItem->item.asString.value;
      struMacro.length    = pItem->item.asString.length;

      iStatus = zh_macroParse( &struMacro );

      if( iStatus == ZH_MACRO_OK && ( struMacro.status & ZH_MACRO_CONT ) )
      {
         zh_stackPop();    /* remove compiled string */
         zh_macroRun( &struMacro );
      }
      else
         zh_macroSyntaxError( &struMacro );

      zh_macroClear( &struMacro );
   }
   else if( zh_vmRequestQuery() == 0 )
   {
      zh_stackPop();
      zh_stackPop();
   }
}

/* NOTE:
 *   This will be called when macro variable or macro expression is
 *   passed by reference or used in optimized left side of the <op>=
 *   expression or as argument of ++ or -- operation
 */
void zh_macroPushReference( PZH_ITEM pItem )
{
   ZH_STACK_TLS_PRELOAD

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_macroPushReference(%p)", ( void * ) pItem ) );

   if( zh_macroCheckParam( pItem ) )
   {
      ZH_MACRO struMacro;
      int iStatus;

      struMacro.mode      = ZH_MODE_MACRO;
      struMacro.supported = ZH_SM_SHORTCUTS | ZH_SM_ZIHER | ZH_SM_ARRSTR;
      struMacro.Flags     = ZH_MACRO_GEN_PUSH | ZH_MACRO_GEN_REFER;
      struMacro.uiNameLen = ZH_SYMBOL_NAME_LEN;
      struMacro.status    = ZH_MACRO_CONT;
      struMacro.string    = pItem->item.asString.value;
      struMacro.length    = pItem->item.asString.length;

      iStatus = zh_macroParse( &struMacro );

      if( iStatus == ZH_MACRO_OK && ( struMacro.status & ZH_MACRO_CONT ) )
      {
         zh_stackPop();    /* remove compiled string */
         zh_macroRun( &struMacro );
      }
      else
         zh_macroSyntaxError( &struMacro );

      zh_macroClear( &struMacro );
   }
}

/*
 * Compile and run:
 *    &alias->var or
 *    alias->&var
 */
static void zh_macroUseAliased( PZH_ITEM pAlias, PZH_ITEM pVar, int iFlag, int iSupported )
{
   ZH_STACK_TLS_PRELOAD

   if( ZH_IS_STRING( pAlias ) && ZH_IS_STRING( pVar ) )
   {
      /* grab memory for "alias->var"
       */
      ZH_SIZE nLen = pAlias->item.asString.length + pVar->item.asString.length + 2;
      char * szString = ( char * ) zh_xgrab( nLen + 1 );
      ZH_MACRO struMacro;
      int iStatus;

      memcpy( szString, pAlias->item.asString.value, pAlias->item.asString.length );
      szString[ pAlias->item.asString.length ]     = '-';
      szString[ pAlias->item.asString.length + 1 ] = '>';
      memcpy( szString + pAlias->item.asString.length + 2, pVar->item.asString.value, pVar->item.asString.length );
      szString[ nLen ] = '\0';

      struMacro.mode      = ZH_MODE_MACRO;
      struMacro.supported = ( ( iSupported & ZH_SM_RT_MACRO ) ? zh_macroFlags() : iSupported ) |
                            ZH_SM_ISUSERCP();
      struMacro.Flags     = iFlag;
      struMacro.uiNameLen = ZH_SYMBOL_NAME_LEN;
      struMacro.status    = ZH_MACRO_CONT;
      struMacro.string    = szString;
      struMacro.length    = nLen;

      iStatus = zh_macroParse( &struMacro );

      zh_stackPop();    /* remove compiled variable name */
      zh_stackPop();    /* remove compiled alias */

      if( iStatus == ZH_MACRO_OK && ( struMacro.status & ZH_MACRO_CONT ) )
      {
         zh_macroRun( &struMacro );
      }
      else
      {
         zh_vmPushString( szString, nLen );
         zh_macroSyntaxError( &struMacro );
      }

      zh_xfree( szString );
      zh_macroClear( &struMacro );
   }
   else if( zh_macroCheckParam( pVar ) )
   {
      /* only right side of alias operator is a string - macro-compile
       * this part only
       */
      ZH_MACRO struMacro;
      int iStatus;

      struMacro.mode      = ZH_MODE_MACRO;
      struMacro.supported = ( ( iSupported & ZH_SM_RT_MACRO ) ? zh_macroFlags() : iSupported ) |
                            ZH_SM_ISUSERCP();
      struMacro.Flags     = iFlag | ZH_MACRO_GEN_ALIASED;
      struMacro.uiNameLen = ZH_SYMBOL_NAME_LEN;
      struMacro.status    = ZH_MACRO_CONT;
      struMacro.string    = pVar->item.asString.value;
      struMacro.length    = pVar->item.asString.length;

      iStatus = zh_macroParse( &struMacro );

      if( iStatus == ZH_MACRO_OK && ( struMacro.status & ZH_MACRO_CONT ) )
      {
         zh_stackPop();    /* remove compiled string */
         zh_macroRun( &struMacro );
      }
      else
         zh_macroSyntaxError( &struMacro );

      zh_macroClear( &struMacro );
   }
}

/* Compiles and run an aliased macro expression - generated pcode
 * pops a value from the stack
 *    &alias->var := any
 *    alias->&var := any
 */
void zh_macroPopAliasedValue( PZH_ITEM pAlias, PZH_ITEM pVar, int flags )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_macroPopAliasedValue(%p, %p)", ( void * ) pAlias, ( void * ) pVar ) );

   zh_macroUseAliased( pAlias, pVar, ZH_MACRO_GEN_POP, flags );
}

/* Compiles and run an aliased macro expression - generated pcode
 * pushes a value onto the stack
 *    any := &alias->var
 *    any := alias->&var
 */
void zh_macroPushAliasedValue( PZH_ITEM pAlias, PZH_ITEM pVar, int flags )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_macroPushAliasedValue(%p, %p)", ( void * ) pAlias, ( void * ) pVar ) );

   zh_macroUseAliased( pAlias, pVar, ZH_MACRO_GEN_PUSH, flags );
}

/* Check for '&' operator and replace it with a macro variable value
 * Returns: the passed string if there is no '&' operator ( pbNewString := FALSE )
 * new string if a valid macro text substitution was found (and sets
 * pbNewString to TRUE)
 */
char * zh_macroExpandString( const char * szString, ZH_SIZE nLength, ZH_BOOL * pfNewString )
{
   char * szResultString;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_macroExpandString(%s,%" ZH_PFS "u,%p)", szString, nLength, ( void * ) pfNewString ) );

   if( szString )
      szResultString = zh_macroTextSubst( szString, &nLength );
   else
      szResultString = ( char * ) ZH_UNCONST( szString );
   *pfNewString = ( szString != szResultString );
   return szResultString;
}

char * zh_macroTextSymbol( const char * szString, ZH_SIZE nLength, ZH_BOOL * pfNewString )
{
   char * szResult = NULL;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_macroTextSymbol(%s,%" ZH_PFS "u,%p)", szString, nLength, ( void * ) pfNewString ) );

   if( szString )
   {
      ZH_SIZE nLen = 0;

      szResult = zh_macroTextSubst( szString, &nLength );

      while( nLength && ( szResult[ 0 ] == ' ' || szResult[ 0 ] == '\t' ) )
      {
         ++szResult;
         ++szString;
         --nLength;
      }

      while( nLength && ( szResult[ nLength - 1 ] == ' ' ||
                          szResult[ nLength - 1 ] == '\t' ) )
         --nLength;

      /* NOTE: This uses _a-zA-Z0-9 pattern to check for a valid name
       * "_" is not valid macro string
       */
      while( nLen < nLength )
      {
         char c = szResult[ nLen ];
         if( c >= 'a' && c <= 'z' )
         {
            if( szResult == szString )
            {
               szResult = ( char * ) zh_xgrab( nLength + 1 );
               memcpy( szResult, szString, nLength );
               szResult[ nLength ] = '\0';
            }
            szResult[ nLen ] = c - ( 'a' - 'A' );
         }
         else if( ! ( c == '_' || ( c >= 'A' && c <= 'Z' ) ||
                      ( nLen && ( c >= '0' && c <= '9' ) ) ) )
         {
            break;
         }
         ++nLen;
      }
      if( nLen == nLength && nLen > ( ZH_SIZE ) ( szResult[ 0 ] == '_' ? 1 : 0 ) )
      {
         if( nLen > ZH_SYMBOL_NAME_LEN )
            nLen = ZH_SYMBOL_NAME_LEN;
         if( szResult[ nLen ] )
         {
            if( szResult == szString )
            {
               szResult = ( char * ) zh_xgrab( nLen + 1 );
               memcpy( szResult, szString, nLen );
            }
            szResult[ nLen ] = '\0';
         }
      }
      else
      {
         if( szResult != szString )
            zh_xfree( szResult );
         szResult = NULL;
      }
   }
   *pfNewString = szResult && szString != szResult;
   return szResult;
}

/* compile a string and return a pcode to push a value of expression
 * NOTE: it can be called to implement an index key evaluation
 * use zh_macroRun() to evaluate a compiled pcode
 */
PZH_MACRO zh_macroCompile( const char * szString )
{
   PZH_MACRO pMacro;
   int iStatus;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_macroCompile(%s)", szString ) );

   pMacro = ( PZH_MACRO ) zh_xgrab( sizeof( ZH_MACRO ) );
   pMacro->mode      = ZH_MODE_MACRO;
   pMacro->supported = zh_macroFlags() | ZH_SM_ISUSERCP();
   pMacro->Flags     = ZH_MACRO_GEN_PUSH | ZH_MACRO_GEN_LIST | ZH_MACRO_GEN_PARE;
   pMacro->uiNameLen = ZH_SYMBOL_NAME_LEN;
   pMacro->status    = ZH_MACRO_CONT;
   pMacro->string    = szString;
   pMacro->length    = strlen( szString );

   iStatus = zh_macroParse( pMacro );
   if( ! ( iStatus == ZH_MACRO_OK && ( pMacro->status & ZH_MACRO_CONT ) ) )
   {
      zh_macroDelete( pMacro );
      pMacro = NULL;
   }

   return pMacro;
}

static void zh_macroBlock( const char * szString, PZH_ITEM pItem )
{
   PZH_MACRO pMacro = zh_macroCompile( szString );

   if( pMacro )
   {
      pMacro->pCodeInfo->pCode[ pMacro->pCodeInfo->nPCodePos - 1 ] = ZH_P_ENDBLOCK;

      if( ZH_IS_COMPLEX( pItem ) )
         zh_itemClear( pItem );

      pItem->item.asBlock.value = zh_codeblockMacroNew( pMacro->pCodeInfo->pCode,
                                                        pMacro->pCodeInfo->nPCodePos );
      pItem->type = ZH_IT_BLOCK;
      pItem->item.asBlock.paramcnt = 0;
      pItem->item.asBlock.lineno = 0;
      pItem->item.asBlock.hclass = 0;
      pItem->item.asBlock.method = 0;

      zh_macroDelete( pMacro );
   }
}

ZH_FUNC( ZH_MACROBLOCK )
{
   const char * szMacro = zh_parc( 1 );

   if( szMacro )
   {
      ZH_STACK_TLS_PRELOAD
      zh_macroBlock( szMacro, zh_stackReturnItem() );
   }
}

static void zh_macroSetGetBlock( PZH_DYNSYMBOL pVarSym, PZH_ITEM pItem,
                                 int iWorkArea, ZH_BOOL fMemVar )
{
   ZH_BYTE byBuf[ 23 + sizeof( PZH_DYNSYMBOL ) + sizeof( PZH_DYNSYMBOL ) ];
   ZH_BYTE bPushPcode, bPopPcode;
   int i = 0, n;

   if( iWorkArea != 0 )
   {
      bPushPcode = ZH_P_MPUSH_ALIASED_FIELD;
      bPopPcode  = ZH_P_MPOPALIASEDFIELD;
   }
   else if( ! fMemVar )
   {
      bPushPcode = ZH_P_MPUSHFIELD;
      bPopPcode  = ZH_P_MPOPFIELD;
   }
   else
   {
      bPushPcode = ZH_P_MPUSHMEMVAR;
      bPopPcode  = ZH_P_MPOPMEMVAR;
   }

   byBuf[ i++ ] = ZH_P_PUSHLOCALNEAR;
   byBuf[ i++ ] = 1;
   byBuf[ i++ ] = ZH_P_PUSHNIL;
   byBuf[ i++ ] = ZH_P_EXACTLYEQUAL;

   byBuf[ i++ ] = ZH_P_JUMPFALSENEAR;
   n = i++;

   if( iWorkArea != 0 )
   {
      byBuf[ i++ ] = ZH_P_PUSHLONG;
      ZH_PUT_LE_UINT32( &byBuf[ i ], iWorkArea );
      i += 4;
   }
   byBuf[ i++ ] = bPushPcode;
   ZH_PUT_PTR( &byBuf[ i ], pVarSym );
   i += sizeof( PZH_DYNSYMBOL );
   byBuf[ i++ ] = ZH_P_ENDBLOCK;

   byBuf[ n ] = ( ZH_BYTE ) ( i - n + 1 );

   byBuf[ i++ ] = ZH_P_PUSHLOCALNEAR;
   byBuf[ i++ ] = 1;
   byBuf[ i++ ] = ZH_P_DUPLICATE;

   if( iWorkArea != 0 )
   {
      byBuf[ i++ ] = ZH_P_PUSHLONG;
      ZH_PUT_LE_UINT32( &byBuf[ i ], iWorkArea );
      i += 4;
   }
   byBuf[ i++ ] = bPopPcode;
   ZH_PUT_PTR( &byBuf[ i ], pVarSym );
   i += sizeof( PZH_DYNSYMBOL );
   byBuf[ i++ ] = ZH_P_ENDBLOCK;

   if( ZH_IS_COMPLEX( pItem ) )
      zh_itemClear( pItem );
   pItem->item.asBlock.value = zh_codeblockMacroNew( byBuf, i );
   pItem->type = ZH_IT_BLOCK;
   pItem->item.asBlock.paramcnt = 1;
   pItem->item.asBlock.lineno = 0;
   pItem->item.asBlock.hclass = 0;
   pItem->item.asBlock.method = 0;
}

ZH_FUNC( MEMVARBLOCK )
{
   const char * szName = zh_parc( 1 );

   if( szName )
   {
      char szVarName[ ZH_SYMBOL_NAME_LEN + 1 ];

      while( ZH_ISSPACE( *szName ) )
         ++szName;
      zh_strncpyUpperTrim( szVarName, szName, sizeof( szVarName ) - 1 );

      if( *szVarName )
      {
         PZH_DYNSYMBOL pVarSym = zh_dynsymFind( szVarName );

         if( pVarSym && zh_dynsymIsMemvar( pVarSym ) )
         {
            ZH_STACK_TLS_PRELOAD
            zh_macroSetGetBlock( pVarSym, zh_stackReturnItem(), 0, ZH_TRUE );
         }
      }
   }
}

ZH_FUNC( FIELDBLOCK )
{
   const char * szName = zh_parc( 1 );

   if( szName )
   {
      char szFieldName[ ZH_SYMBOL_NAME_LEN + 1 ];

      /* Make the same conversion for field name as in default
       * ADDFIELD() workarea method so exactly the same set of
       * symbols is accepted. [druzus]
       */
      while( ZH_ISSPACE( *szName ) )
         ++szName;
      zh_strncpyUpperTrim( szFieldName, szName, sizeof( szFieldName ) - 1 );

      if( *szFieldName )
      {
         /* Cl*pper does not create new symbol in this function
          * so only registered symbols are accepted. When table
          * is open then all field symbols are registered in ZHVM.
          * It means that this function may not create field block
          * if table is not open yet and field name was never used
          * explicitly in compiled application. It's possible to
          * change zh_dynsymFind() to zh_dynsymGetCase() below
          * to automatically register new symbol if we decide it's
          * real limitation and we should drop strict Cl*pper
          * compatibility. Anyhow it may cause that some code
          * will register big number of completely unnecessary
          * symbols. [druzus]
          */
         PZH_DYNSYMBOL pFieldSym = zh_dynsymFind( szFieldName );
         if( pFieldSym )
         {
            ZH_STACK_TLS_PRELOAD
            zh_macroSetGetBlock( pFieldSym, zh_stackReturnItem(), 0, ZH_FALSE );
         }
      }
   }
}

ZH_FUNC( FIELDWBLOCK )
{
   const char * szName = zh_parc( 1 );
   int iWorkArea = zh_parni( 2 );

   if( szName && iWorkArea != 0 )
   {
      char szFieldName[ ZH_SYMBOL_NAME_LEN + 1 ];

      while( ZH_ISSPACE( *szName ) )
         ++szName;
      zh_strncpyUpperTrim( szFieldName, szName, sizeof( szFieldName ) - 1 );

      if( *szFieldName )
      {
         PZH_DYNSYMBOL pFieldSym = zh_dynsymFind( szFieldName );
         if( pFieldSym )
         {
            ZH_STACK_TLS_PRELOAD
            zh_macroSetGetBlock( pFieldSym, zh_stackReturnItem(), iWorkArea, ZH_FALSE );
         }
      }
   }
}

/* This function handles a macro function calls, e.g. var :=&macro()
 * and creating memvar variables using PUBLIC/PRIVATE command
 * PUBLIC &macro
 *
 * 'pItem' points to a ITEM that contains a string value which after
 *    text substitution will return a function name
 */
void zh_macroPushSymbol( PZH_ITEM pItem )
{
   ZH_STACK_TLS_PRELOAD
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_macroPushSymbol(%p)", ( void * ) pItem ) );

   if( zh_macroCheckParam( pItem ) )
   {
      char * szString;
      ZH_BOOL fNewBuffer;

      szString = zh_macroTextSymbol( pItem->item.asString.value,
                                     pItem->item.asString.length,
                                     &fNewBuffer );
      if( szString )
      {
         PZH_DYNSYMBOL pDynSym = zh_dynsymGetCase( szString );

         if( fNewBuffer )
            zh_xfree( szString );   /* free space allocated in zh_macroTextSymbol */

         zh_stackPop();    /* remove compiled string */
         /* NOTE: checking for valid function name (valid pointer) is done
          * in zh_vmDo()
          */
         zh_vmPushSymbol( pDynSym->pSymbol );  /* push compiled symbol instead of a string */
         return;
      }
      else
         zh_macroSyntaxError( NULL );
   }

   if( ! ZH_IS_SYMBOL( zh_stackItemFromTop( -1 ) ) && zh_vmRequestQuery() == 0 )
   {
      zh_stackPop();    /* remove compiled string */
      zh_vmPushDynSym( zh_dynsymGetCase( "" ) );  /* push compiled symbol instead of a string */
   }
}

/* Macro text substitution
 *
 * 'pItem' points to a ITEM that contains a string value which after
 *    text substitution will be returned
 */
void zh_macroTextValue( PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_macroTextValue(%p)", ( void * ) pItem ) );

   if( zh_macroCheckParam( pItem ) )
   {
      char * szString;
      ZH_SIZE nLength = pItem->item.asString.length;

      szString = zh_macroTextSubst( pItem->item.asString.value, &nLength );

      if( szString != pItem->item.asString.value )
      {
         /* replace the old value on the eval stack with the new one
          */
         zh_itemPutCLPtr( pItem, szString, nLength );
      }
      /*
       * else
       *    leave original value on the eval stack - there was no '&' operator
       *    inside a string
       */
   }
}

const char * zh_macroGetType( PZH_ITEM pItem )
{
   ZH_STACK_TLS_PRELOAD
   const char * szType;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_macroGetType(%p)", ( void * ) pItem ) );

   if( zh_macroCheckParam( pItem ) )
   {
      ZH_MACRO struMacro;
      int iStatus;

      struMacro.mode      = ZH_MODE_MACRO;
      struMacro.supported = zh_macroFlags() | ZH_SM_ISUSERCP();
      struMacro.Flags     = ZH_MACRO_GEN_PUSH | ZH_MACRO_GEN_TYPE;
      struMacro.uiNameLen = ZH_SYMBOL_NAME_LEN;
      struMacro.status    = ZH_MACRO_CONT;
      struMacro.string    = pItem->item.asString.value;
      struMacro.length    = pItem->item.asString.length;
      iStatus = zh_macroParse( &struMacro );

      if( iStatus == ZH_MACRO_OK )
      {
         /* passed string was successfully compiled
          */
         if( struMacro.exprType == ZH_ET_CODEBLOCK )
         {
            szType = "B";
         }
         else if( struMacro.status & ZH_MACRO_UNKN_SYM )
         {
            /* request for a symbol that is not in a symbol table or
             * for a variable that is not visible
             */
            szType = "U";
         }
         else if( struMacro.status & ZH_MACRO_UDF )
         {
            szType = "UI";  /* UDF function was used - cannot determine a type */
         }
         else if( struMacro.status & ZH_MACRO_CONT )
         {
            /* OK - the pcode was generated and it can be evaluated
             */
            ZH_ERROR_INFO struErr;
            PZH_ERROR_INFO pOld;

            /* Set our temporary error handler. We do not need any error
             * messages here - we need to know only if evaluation was
             * successful. If evaluation was successful then the data type
             * of expression can be determined.
             */
            struErr.Func  = zh_macroErrorType;
            struErr.Cargo = ( void * ) &struMacro;
            pOld = zh_errorHandler( &struErr );
            zh_macroRun( &struMacro );
            zh_errorHandler( pOld );

            if( struMacro.status & ZH_MACRO_CONT )
            {
               /* Evaluation was successful
                * Now the value of expression is placed on the eval stack -
                * check its type and pop it from the stack
                */
               szType = zh_itemTypeStr( zh_stackItemFromTop( -1 ) );
               zh_stackPop();
            }
            else
            {
               /* something unpleasant happened during macro evaluation */
               if( struMacro.pError )
               {
                  ZH_ERRCODE ulGenCode = zh_errGetGenCode( struMacro.pError );

                  if( ulGenCode == EG_NOVAR || ulGenCode == EG_NOALIAS )
                  {
                     szType = "U";
                  }
                  else
                     szType = "UE";
               }
               else
                  szType = "UE";
            }
         }
         else
            szType = "UE";
      }
      else
         szType = "UE";  /* syntax error during compilation */

      zh_macroClear( &struMacro );
   }
   else
      szType = "U";

   return szType;
}

/*
 * Set macro capabilities if flag > 0 or get current macro capabilities
 * if flag == 0
 */
int zh_macroSetMacro( ZH_BOOL fSet, int flag )
{
   int currentFlags = zh_macroFlags();

   if( flag > 0 )
   {
      if( fSet )
         zh_macroFlagsSet( currentFlags | flag );
      else
         zh_macroFlagsSet( currentFlags & ~flag );
   }

   return currentFlags;
}

ZH_FUNC( ZH_SETMACRO )
{
   ZH_STACK_TLS_PRELOAD
   int iPrmCnt = zh_pcount();

   if( iPrmCnt > 0 )
   {
      int flags = zh_parni( 1 );
      PZH_ITEM pValue;

      switch( flags )
      {
         case ZH_SM_ZIHER:
         /* enable/disable extended Ziher compatibility */
         case ZH_SM_XBASE:
         /* enable/disable extended Xbase++ compatibility */
         case ZH_SM_ARRSTR:
         /* enable/disable processing of strings as an array of bytes */
         case ZH_SM_SHORTCUTS:
            /* enable/disable support for shortcut logical operators */
            zh_retl( zh_macroFlags() & flags );
            pValue = zh_param( 2, ZH_IT_LOGICAL );
            if( pValue )
               zh_macroSetMacro( zh_itemGetL( pValue ), flags );
            break;

         default:
            ;  /* do nothing */
      }
   }
   else
      zh_ret();    /* return NIL */
}

/* - */

/* returns the order + 1 of a variable if defined or zero */
int zh_macroLocalVarGetPos( const char * szVarName, ZH_COMP_DECL )
{
   int iVar = 1;
   PZH_CBVAR pVars = ZH_PCODE_DATA->pLocals;

   while( pVars )
   {
      if( pVars->szName && ! strcmp( pVars->szName, szVarName ) )
         return iVar;
      pVars = pVars->pNext;
      iVar++;
   }
   return 0;
}

ZH_BOOL zh_macroIsValidMacroText( const char * szText, ZH_SIZE nLen )
{
   if( nLen )
   {
      while( --nLen )
      {
         if( *szText++ == '&' )
         {
            char ch = *szText;
            if( ( ch >= 'A' && ch <= 'Z' ) ||
                ( ch >= 'a' && ch <= 'z' ) || ch == '_' )
               return ZH_TRUE;
         }
      }
   }

   return ZH_FALSE;
}

ZH_SIZE zh_macroGenJump( ZH_I_SIZE nOffset, ZH_COMP_DECL )
{
   if( nOffset == 0 )
      zh_macroGenPCode4( ZH_P_JUMPFAR, 0, 0, 0, ZH_COMP_PARAM );
   else if( ZH_LIM_INT8( nOffset ) )
      zh_macroGenPCode2( ZH_P_JUMPNEAR, ZH_LOBYTE( nOffset ), ZH_COMP_PARAM );
   else if( ZH_LIM_INT16( nOffset ) )
      zh_macroGenPCode3( ZH_P_JUMP, ZH_LOBYTE( nOffset ), ZH_HIBYTE( nOffset ), ZH_COMP_PARAM );
   else if( ZH_LIM_INT24( nOffset ) )
      zh_macroGenPCode4( ZH_P_JUMPFAR, ZH_LOBYTE( nOffset ), ZH_HIBYTE( nOffset ), ZH_ULBYTE( nOffset ), ZH_COMP_PARAM );
   else
      zh_macroError( ZH_MACRO_TOO_COMPLEX, ZH_COMP_PARAM );

   return ZH_PCODE_DATA->nPCodePos - 3;
}

ZH_SIZE zh_macroGenJumpFalse( ZH_I_SIZE nOffset, ZH_COMP_DECL )
{
   if( nOffset == 0 )
      zh_macroGenPCode4( ZH_P_JUMPFALSEFAR, 0, 0, 0, ZH_COMP_PARAM );
   else if( ZH_LIM_INT8( nOffset ) )
      zh_macroGenPCode2( ZH_P_JUMPFALSENEAR, ZH_LOBYTE( nOffset ), ZH_COMP_PARAM );
   else if( ZH_LIM_INT16( nOffset ) )
      zh_macroGenPCode3( ZH_P_JUMPFALSE, ZH_LOBYTE( nOffset ), ZH_HIBYTE( nOffset ), ZH_COMP_PARAM );
   else if( ZH_LIM_INT24( nOffset ) )
      zh_macroGenPCode4( ZH_P_JUMPFALSEFAR, ZH_LOBYTE( nOffset ), ZH_HIBYTE( nOffset ), ZH_ULBYTE( nOffset ), ZH_COMP_PARAM );
   else
      zh_macroError( ZH_MACRO_TOO_COMPLEX, ZH_COMP_PARAM );

   return ZH_PCODE_DATA->nPCodePos - 3;
}

ZH_SIZE zh_macroGenJumpTrue( ZH_I_SIZE nOffset, ZH_COMP_DECL )
{
   if( nOffset == 0 )
      zh_macroGenPCode4( ZH_P_JUMPTRUEFAR, 0, 0, 0, ZH_COMP_PARAM );
   else if( ZH_LIM_INT8( nOffset ) )
      zh_macroGenPCode2( ZH_P_JUMPTRUENEAR, ZH_LOBYTE( nOffset ), ZH_COMP_PARAM );
   else if( ZH_LIM_INT16( nOffset ) )
      zh_macroGenPCode3( ZH_P_JUMPTRUE, ZH_LOBYTE( nOffset ), ZH_HIBYTE( nOffset ), ZH_COMP_PARAM );
   else if( ZH_LIM_INT24( nOffset ) )
      zh_macroGenPCode4( ZH_P_JUMPTRUEFAR, ZH_LOBYTE( nOffset ), ZH_HIBYTE( nOffset ), ZH_ULBYTE( nOffset ), ZH_COMP_PARAM );
   else
      zh_macroError( ZH_MACRO_TOO_COMPLEX, ZH_COMP_PARAM );

   return ZH_PCODE_DATA->nPCodePos - 3;
}

void zh_macroGenJumpThere( ZH_SIZE nFrom, ZH_SIZE nTo, ZH_COMP_DECL )
{
   ZH_BYTE * pCode = ZH_PCODE_DATA->pCode;
   ZH_I_SIZE nOffset = nTo - nFrom + 1;

   if( ZH_LIM_INT24( nOffset ) )
      ZH_PUT_LE_UINT24( &pCode[ nFrom ], nOffset );
   else
      zh_macroError( ZH_MACRO_TOO_COMPLEX, ZH_COMP_PARAM );
}

void zh_macroGenJumpHere( ZH_SIZE nOffset, ZH_COMP_DECL )
{
   zh_macroGenJumpThere( nOffset, ZH_PCODE_DATA->nPCodePos, ZH_COMP_PARAM );
}

/*
 * Function generates pcode for passed memvar name
 */
static void zh_macroMemvarGenPCode( ZH_BYTE bPCode, const char * szVarName, ZH_COMP_DECL )
{
   ZH_BYTE byBuf[ sizeof( PZH_DYNSYMBOL ) + 1 ];
   PZH_DYNSYMBOL pSym;

   if( ZH_MACRO_DATA->Flags & ZH_MACRO_GEN_TYPE )
   {
      /* we are determining the type of expression (called from Type() function)
       * then we shouldn't create the requested variable if it doesn't exist
       */
      pSym = zh_dynsymFind( szVarName );
      if( ! pSym )
      {
         ZH_MACRO_DATA->status |= ZH_MACRO_UNKN_VAR;
         pSym = zh_dynsymGetCase( szVarName );
      }
   }
   else
      pSym = zh_dynsymGetCase( szVarName );

   byBuf[ 0 ] = bPCode;
   ZH_PUT_PTR( &byBuf[ 1 ], pSym );
   zh_macroGenPCodeN( byBuf, sizeof( byBuf ), ZH_COMP_PARAM );
}

/* generates the pcode to push a symbol on the virtual machine stack */
void zh_macroGenPushSymbol( const char * szSymbolName, ZH_BOOL bFunction, ZH_COMP_DECL )
{
   ZH_BYTE byBuf[ sizeof( PZH_DYNSYMBOL ) + 1 ];
   PZH_DYNSYMBOL pSym;

   if( ZH_MACRO_DATA->Flags & ZH_MACRO_GEN_TYPE )
   {
      /* we are determining the type of expression (called from Type() function)
       */
      pSym = zh_dynsymFind( szSymbolName );
      if( ! pSym )
      {
         ZH_MACRO_DATA->status |= ZH_MACRO_UNKN_SYM;
         ZH_MACRO_DATA->status &= ~ZH_MACRO_CONT;  /* don't run this pcode */
         /*
          * NOTE: the compiled pcode will be not executed then we can ignore
          * NULL value for pSym
          */
      }
      else if( bFunction )
      {
         if( pSym->pSymbol->value.pFunPtr == NULL )
         {
            /* static functions are not allowed in macro */
            ZH_MACRO_DATA->status |= ZH_MACRO_UNKN_SYM;
            ZH_MACRO_DATA->status &= ~ZH_MACRO_CONT;  /* don't run this pcode */
         }
      }
   }
   else
      pSym = zh_dynsymGetCase( szSymbolName );

   byBuf[ 0 ] = ZH_P_MPUSHSYM;
   ZH_PUT_PTR( &byBuf[ 1 ], pSym );
   zh_macroGenPCodeN( byBuf, sizeof( byBuf ), ZH_COMP_PARAM );
}

/* generates the pcode to push a long number on the virtual machine stack */
void zh_macroGenPushLong( ZH_MAXINT nNumber, ZH_COMP_DECL )
{
   if( nNumber == 0 )
   {
      zh_macroGenPCode1( ZH_P_ZERO, ZH_COMP_PARAM );
   }
   else if( nNumber == 1 )
   {
      zh_macroGenPCode1( ZH_P_ONE, ZH_COMP_PARAM );
   }
   else if( ZH_LIM_INT8( nNumber ) )
   {
      zh_macroGenPCode2( ZH_P_PUSHBYTE, ( ZH_BYTE ) nNumber, ZH_COMP_PARAM );
   }
   else if( ZH_LIM_INT16( nNumber ) )
   {
      zh_macroGenPCode3( ZH_P_PUSHINT, ZH_LOBYTE( nNumber ), ZH_HIBYTE( nNumber ), ZH_COMP_PARAM );
   }
   else if( ZH_LIM_INT32( nNumber ) )
   {
      ZH_BYTE pBuffer[ 5 ];
      pBuffer[ 0 ] = ZH_P_PUSHLONG;
      ZH_PUT_LE_UINT32( pBuffer + 1, nNumber );
      zh_macroGenPCodeN( pBuffer, sizeof( pBuffer ), ZH_COMP_PARAM );
   }
   else
   {
      ZH_BYTE pBuffer[ 9 ];
      pBuffer[ 0 ] = ZH_P_PUSHLONGLONG;
      ZH_PUT_LE_UINT64( pBuffer + 1, nNumber );
      zh_macroGenPCodeN( pBuffer, sizeof( pBuffer ), ZH_COMP_PARAM );
   }
}

/* generates the pcode to push a date on the virtual machine stack */
void zh_macroGenPushDate( long lDate, ZH_COMP_DECL )
{
   ZH_BYTE pBuffer[ 5 ];

   pBuffer[ 0 ] = ZH_P_PUSH_DATE;
   ZH_PUT_LE_UINT32( pBuffer + 1, lDate );
   zh_macroGenPCodeN( pBuffer, sizeof( pBuffer ), ZH_COMP_PARAM );
}

/* generates the pcode to push a timestamp on the virtual machine stack */
void zh_macroGenPushTimeStamp( long lDate, long lTime, ZH_COMP_DECL )
{
   ZH_BYTE pBuffer[ 9 ];

   pBuffer[ 0 ] = ZH_P_PUSHTIMESTAMP;
   ZH_PUT_LE_UINT32( pBuffer + 1, lDate );
   ZH_PUT_LE_UINT32( pBuffer + 5, lTime );
   zh_macroGenPCodeN( pBuffer, sizeof( pBuffer ), ZH_COMP_PARAM );
}

/* sends a message to an object */
void zh_macroGenMessage( const char * szMsgName, ZH_BOOL bIsObject, ZH_COMP_DECL )
{
   if( szMsgName )
   {
      ZH_BYTE byBuf[ sizeof( PZH_DYNSYMBOL ) + 1 ];

      /* Find the address of passed symbol - create the symbol if doesn't exist
       */
      PZH_DYNSYMBOL pSym = zh_dynsymGetCase( szMsgName );

      byBuf[ 0 ] = ZH_P_MMESSAGE;
      ZH_PUT_PTR( &byBuf[ 1 ], pSym );
      zh_macroGenPCodeN( byBuf, sizeof( byBuf ), ZH_COMP_PARAM );
   }
   if( ! bIsObject )    /* used in full compiler only */
      zh_macroGenPCode3( ZH_P_WITHOBJECTMESSAGE, 0xFF, 0xFF, ZH_COMP_PARAM );
}

/* generates an underscore-symbol name for a data assignment */
void zh_macroGenMessageData( const char * szMsg, ZH_BOOL bIsObject, ZH_COMP_DECL )
{
   char szResult[ ZH_SYMBOL_NAME_LEN + 1 ];
   int iLen;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_macroGenMessageData(%s)", szMsg ) );

   iLen = ( int ) strlen( szMsg );
   if( iLen > ZH_SYMBOL_NAME_LEN - 1 )
      iLen = ZH_SYMBOL_NAME_LEN - 1;
   szResult[ 0 ] = '_';
   memcpy( szResult + 1, szMsg, iLen );
   szResult[ iLen + 1 ] = '\0';
   zh_macroGenMessage( szResult, bIsObject, ZH_COMP_PARAM );
}

/* generates the pcode to pop a value from the virtual machine stack onto a variable */
void zh_macroGenPopVar( const char * szVarName, ZH_COMP_DECL )
{
   int iVar;

   iVar = zh_macroLocalVarGetPos( szVarName, ZH_COMP_PARAM );
   if( iVar )
   {
      /* this is a codeblock parameter */
      zh_macroGenPCode3( ZH_P_POPLOCAL, ZH_LOBYTE( iVar ), ZH_HIBYTE( iVar ), ZH_COMP_PARAM );
   }
   else
   {
      /* TODO: memvars created inside Type() function should have PUBLIC scope */
      zh_macroMemvarGenPCode( ZH_P_MPOPMEMVAR, szVarName, ZH_COMP_PARAM );
   }
}

/* generates the pcode to pop a value from the virtual machine stack onto a variable */
void zh_macroGenPopMemvar( const char * szVarName, ZH_COMP_DECL )
{
   zh_macroMemvarGenPCode( ZH_P_MPOPMEMVAR, szVarName, ZH_COMP_PARAM );
}

/* generates the pcode to pop a value from the virtual machine stack onto
 * an aliased variable
 */
void zh_macroGenPopAliasedVar( const char * szVarName,
                               ZH_BOOL bPushAliasValue,
                               const char * szAlias,
                               ZH_MAXINT nWorkarea, ZH_COMP_DECL )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_macroGenPopAliasedVar(%s->%s)", szAlias, szVarName ) );

   if( bPushAliasValue )
   {
      if( szAlias )
      {
         int iLen = ( int ) strlen( szAlias );

         if( szAlias[ 0 ] == 'M' && ( iLen == 1 ||
             ( iLen >= 4 && iLen <= 6 && strncmp( szAlias, "MEMVAR", iLen ) == 0 ) ) )
         {
            /* M-> or MEMV-> or MEMVA-> or MEMVAR-> variable */
            /* TODO: memvars created inside Type() function should have PUBLIC scope */
            zh_macroMemvarGenPCode( ZH_P_MPOPMEMVAR, szVarName, ZH_COMP_PARAM );
         }
         else if( iLen >= 4 && iLen <= 6 &&
                  ( strncmp( szAlias, "FIELD", iLen ) == 0 ||
                    strncmp( szAlias, "_FIELD", iLen ) == 0 ) )
         {
            /* FIELD-> */
            zh_macroMemvarGenPCode( ZH_P_MPOPFIELD, szVarName, ZH_COMP_PARAM );
         }
         else
         {
            /* database alias */
            zh_macroGenPushSymbol( szAlias, ZH_FALSE, ZH_COMP_PARAM );
            zh_macroMemvarGenPCode( ZH_P_MPOPALIASEDFIELD, szVarName, ZH_COMP_PARAM );
         }
      }
      else
      {
         zh_macroGenPushLong( nWorkarea, ZH_COMP_PARAM );
         zh_macroMemvarGenPCode( ZH_P_MPOPALIASEDFIELD, szVarName, ZH_COMP_PARAM );
      }
   }
   else
   {
      /* Alias is already placed on stack
       * NOTE: An alias will be determined at runtime then we cannot decide
       * here if passed name is either a field or a memvar
       */
      /* TODO: memvars created inside Type() function should have PUBLIC scope */
      zh_macroMemvarGenPCode( ZH_P_MPOPALIASEDVAR, szVarName, ZH_COMP_PARAM );
   }
}

/* generates the pcode to push a non-aliased variable value to the virtual
 * machine stack
 */
void zh_macroGenPushVar( const char * szVarName, ZH_COMP_DECL )
{
   int iVar;

   iVar = zh_macroLocalVarGetPos( szVarName, ZH_COMP_PARAM );
   if( iVar )
   {
      /* this is a codeblock parameter */
      zh_macroGenPCode3( ZH_P_PUSHLOCAL, ZH_LOBYTE( iVar ), ZH_HIBYTE( iVar ), ZH_COMP_PARAM );
   }
   else
   {
      zh_macroMemvarGenPCode( ZH_P_MPUSHVARIABLE, szVarName, ZH_COMP_PARAM );
   }
}

/* generates the pcode to push a variable by reference to the virtual machine stack */
void zh_macroGenPushVarRef( const char * szVarName, ZH_COMP_DECL )
{
   int iVar;

   iVar = zh_macroLocalVarGetPos( szVarName, ZH_COMP_PARAM );
   if( iVar )
      zh_macroGenPCode3( ZH_P_PUSHLOCALREF, ZH_LOBYTE( iVar ), ZH_HIBYTE( iVar ), ZH_COMP_PARAM );
   else
   {
      zh_macroMemvarGenPCode( ZH_P_MPUSHMEMVARREF, szVarName, ZH_COMP_PARAM );
   }
}

/* generates the pcode to push a variable by reference to the virtual machine stack */
void zh_macroGenPushMemvarRef( const char * szVarName, ZH_COMP_DECL )
{
   zh_macroMemvarGenPCode( ZH_P_MPUSHMEMVARREF, szVarName, ZH_COMP_PARAM );
}

/* generates the pcode to push an aliased variable value to the virtual
 * machine stack
 */
void zh_macroGenPushAliasedVar( const char * szVarName,
                                ZH_BOOL bPushAliasValue,
                                const char * szAlias,
                                ZH_MAXINT nWorkarea, ZH_COMP_DECL )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_macroGenPushAliasedVar(%s->%s)", szAlias, szVarName ) );

   if( bPushAliasValue )
   {
      if( szAlias )
      {
         /* myalias->var
          * FIELD->var
          * MEMVAR->var
          */
         int iLen = ( int ) strlen( szAlias );

         if( szAlias[ 0 ] == 'M' && ( iLen == 1 ||
             ( iLen >= 4 && iLen <= 6 && strncmp( szAlias, "MEMVAR", iLen ) == 0 ) ) )
         {
            /* M-> or MEMV-> or MEMVA-> or MEMVAR-> variable */
            zh_macroMemvarGenPCode( ZH_P_MPUSHMEMVAR, szVarName, ZH_COMP_PARAM );
         }
         else if( iLen >= 4 && iLen <= 6 &&
                  ( strncmp( szAlias, "FIELD", iLen ) == 0 ||
                    strncmp( szAlias, "_FIELD", iLen ) == 0 ) )
         {
            /* FIELD-> */
            zh_macroMemvarGenPCode( ZH_P_MPUSHFIELD, szVarName, ZH_COMP_PARAM );
         }
         else
         {
            /* database alias */
            zh_macroGenPushSymbol( szAlias, ZH_FALSE, ZH_COMP_PARAM );
            zh_macroMemvarGenPCode( ZH_P_MPUSH_ALIASED_FIELD, szVarName, ZH_COMP_PARAM );
         }
      }
      else
      {
         zh_macroGenPushLong( nWorkarea, ZH_COMP_PARAM );
         zh_macroMemvarGenPCode( ZH_P_MPUSH_ALIASED_FIELD, szVarName, ZH_COMP_PARAM );
      }
   }
   else
   {
      /* Alias is already placed on stack
       * NOTE: An alias will be determined at runtime then we cannot decide
       * here if passed name is either a field or a memvar
       */
      zh_macroMemvarGenPCode( ZH_P_MPUSHALIASEDVAR, szVarName, ZH_COMP_PARAM );
   }
}

/* pushes a logical value on the virtual machine stack , */
void zh_macroGenPushLogical( int iTrueFalse, ZH_COMP_DECL )
{
   if( iTrueFalse )
      zh_macroGenPCode1( ZH_P_TRUE, ZH_COMP_PARAM );
   else
      zh_macroGenPCode1( ZH_P_FALSE, ZH_COMP_PARAM );
}

/* generates the pcode to push a double number on the virtual machine stack */
void zh_macroGenPushDouble( double dNumber, ZH_BYTE bWidth, ZH_BYTE bDec, ZH_COMP_DECL )
{
   ZH_BYTE pBuffer[ sizeof( double ) + sizeof( ZH_BYTE ) + sizeof( ZH_BYTE ) + 1 ];

   pBuffer[ 0 ] = ZH_P_PUSHDOUBLE;
   ZH_PUT_LE_DOUBLE( &( pBuffer[ 1 ] ), dNumber );
   pBuffer[ 1 + sizeof( double ) ] = bWidth;
   pBuffer[ 1 + sizeof( double ) + sizeof( ZH_BYTE ) ] = bDec;

   zh_macroGenPCodeN( pBuffer, 1 + sizeof( double ) + sizeof( ZH_BYTE ) + sizeof( ZH_BYTE ), ZH_COMP_PARAM );
}

void zh_macroGenPushFunSym( const char * szFunName, int iFlags, ZH_COMP_DECL )
{
   if( ( iFlags & ZH_FN_RESERVED ) == 0 )
      ZH_MACRO_DATA->status |= ZH_MACRO_UDF; /* this is used in zh_macroGetType */
   zh_macroGenPushSymbol( szFunName, ZH_TRUE, ZH_COMP_PARAM );
}

void zh_macroGenPushFunCall( const char * szFunName, int iFlags, ZH_COMP_DECL )
{
   zh_macroGenPushFunSym( szFunName, iFlags, ZH_COMP_PARAM );
   zh_macroGenPCode1( ZH_P_PUSHNIL, ZH_COMP_PARAM );
}

void zh_macroGenPushFunRef( const char * szFunName, ZH_COMP_DECL )
{
   zh_macroGenPushSymbol( szFunName, ZH_TRUE, ZH_COMP_PARAM );
}

/* generates the pcode to push a string on the virtual machine stack */
void zh_macroGenPushString( const char * szText, ZH_SIZE nStrLen, ZH_COMP_DECL )
{
   if( nStrLen <= UINT24_MAX )
   {
      if( nStrLen <= USHRT_MAX )
         zh_macroGenPCode3( ZH_P_MPUSHSTR, ZH_LOBYTE( nStrLen ), ZH_HIBYTE( nStrLen ), ZH_COMP_PARAM );
      else
         zh_macroGenPCode4( ZH_P_MPUSHSTRLARGE, ZH_LOBYTE( nStrLen ), ZH_HIBYTE( nStrLen ), ZH_ULBYTE( nStrLen ), ZH_COMP_PARAM );
      zh_macroGenPCodeN( ( const ZH_BYTE * ) szText, nStrLen, ZH_COMP_PARAM );
   }
   else
      zh_macroError( ZH_MACRO_TOO_COMPLEX, ZH_COMP_PARAM );
}

void zh_macroGenPCode1( ZH_BYTE byte, ZH_COMP_DECL )
{
   PZH_PCODE_INFO pFunc = ZH_PCODE_DATA;

   if( ( pFunc->nPCodeSize - pFunc->nPCodePos ) < 1 )
      pFunc->pCode = ( ZH_BYTE * ) zh_xrealloc( pFunc->pCode, pFunc->nPCodeSize += ZH_PCODE_SIZE );

   pFunc->pCode[ pFunc->nPCodePos++ ] = byte;
}

void zh_macroGenPCode2( ZH_BYTE byte1, ZH_BYTE byte2, ZH_COMP_DECL )
{
   PZH_PCODE_INFO pFunc = ZH_PCODE_DATA;

   if( ( pFunc->nPCodeSize - pFunc->nPCodePos ) < 2 )
      pFunc->pCode = ( ZH_BYTE * ) zh_xrealloc( pFunc->pCode, pFunc->nPCodeSize += ZH_PCODE_SIZE );

   pFunc->pCode[ pFunc->nPCodePos++ ] = byte1;
   pFunc->pCode[ pFunc->nPCodePos++ ] = byte2;
}

void zh_macroGenPCode3( ZH_BYTE byte1, ZH_BYTE byte2, ZH_BYTE byte3, ZH_COMP_DECL )
{
   PZH_PCODE_INFO pFunc = ZH_PCODE_DATA;

   if( ( pFunc->nPCodeSize - pFunc->nPCodePos ) < 3 )
      pFunc->pCode = ( ZH_BYTE * ) zh_xrealloc( pFunc->pCode, pFunc->nPCodeSize += ZH_PCODE_SIZE );

   pFunc->pCode[ pFunc->nPCodePos++ ] = byte1;
   pFunc->pCode[ pFunc->nPCodePos++ ] = byte2;
   pFunc->pCode[ pFunc->nPCodePos++ ] = byte3;
}

void zh_macroGenPCode4( ZH_BYTE byte1, ZH_BYTE byte2, ZH_BYTE byte3, ZH_BYTE byte4, ZH_COMP_DECL )
{
   PZH_PCODE_INFO pFunc = ZH_PCODE_DATA;

   if( ( pFunc->nPCodeSize - pFunc->nPCodePos ) < 4 )
      pFunc->pCode = ( ZH_BYTE * ) zh_xrealloc( pFunc->pCode, pFunc->nPCodeSize += ZH_PCODE_SIZE );

   pFunc->pCode[ pFunc->nPCodePos++ ] = byte1;
   pFunc->pCode[ pFunc->nPCodePos++ ] = byte2;
   pFunc->pCode[ pFunc->nPCodePos++ ] = byte3;
   pFunc->pCode[ pFunc->nPCodePos++ ] = byte4;
}

void zh_macroGenPCodeN( const ZH_BYTE * pBuffer, ZH_SIZE nSize, ZH_COMP_DECL )
{
   PZH_PCODE_INFO pFunc = ZH_PCODE_DATA;

   if( pFunc->nPCodePos + nSize > pFunc->nPCodeSize )
   {
      /* not enough free space in pcode buffer - increase it */
      pFunc->nPCodeSize += ( ( ( nSize / ZH_PCODE_SIZE ) + 1 ) * ZH_PCODE_SIZE );
      pFunc->pCode = ( ZH_BYTE * ) zh_xrealloc( pFunc->pCode, pFunc->nPCodeSize );
   }

   memcpy( pFunc->pCode + pFunc->nPCodePos, pBuffer, nSize );
   pFunc->nPCodePos += nSize;
}

/* - */

void zh_macroError( int iError, ZH_COMP_DECL )
{
   ZH_MACRO_DATA->status |= iError;
   ZH_MACRO_DATA->status &= ~ZH_MACRO_CONT;  /* clear CONT bit */
}

/* Start a new pcode buffer for a codeblock */
void zh_macroCodeBlockStart( ZH_COMP_DECL )
{
   PZH_PCODE_INFO pCB;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_macroCodeBlockStart(%p)", ( void * ) ZH_COMP_PARAM ) );

   pCB = ( PZH_PCODE_INFO ) zh_xgrab( sizeof( ZH_PCODE_INFO ) );

   pCB->pCode = ( ZH_BYTE * ) zh_xgrab( ZH_PCODE_SIZE );
   pCB->nPCodeSize = ZH_PCODE_SIZE;
   pCB->nPCodePos  = 0;
   pCB->fVParams   = ZH_FALSE;
   pCB->pLocals    = NULL;

   /* replace current pcode buffer with the new one
    */
   pCB->pPrev = ZH_PCODE_DATA;
   ZH_PCODE_DATA = pCB;
}

void zh_macroCodeBlockEnd( ZH_COMP_DECL )
{
   PZH_PCODE_INFO pCodeblock;   /* pointer to the current codeblock */
   ZH_SIZE nSize;
   ZH_USHORT usParms = 0;   /* number of codeblock parameters */
   PZH_CBVAR pVar;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_macroCodeBlockEnd(%p)", ( void * ) ZH_COMP_PARAM ) );

   /* a currently processed codeblock */
   pCodeblock = ZH_PCODE_DATA;

   /* return to pcode buffer of a codeblock in which the current
    * codeblock was defined
    */
   ZH_PCODE_DATA = pCodeblock->pPrev;

   /* generate a proper codeblock frame with a codeblock size and with
    * a number of expected parameters
    */

   /* Count the number of codeblock parameters */
   pVar = pCodeblock->pLocals;
   while( pVar )
   {
      pVar = pVar->pNext;
      ++usParms;
   }

   /* NOTE: 6 = ZH_P_MPUSH_BLOCK + ZH_USHORT( size ) + ZH_USHORT( wParams ) + _ENDBLOCK
    * runtime compiled codeblock cannot reference local variables defined in a
    * function
    */
   nSize = pCodeblock->nPCodePos + 6;

   /* NOTE: ZH_P_MPUSH_BLOCK differs from ZH_P_PUSH_BLOCK - the pcode
    * is stored in dynamic memory pool instead of static memory
    */
   if( nSize <= USHRT_MAX )
      zh_macroGenPCode3( ZH_P_MPUSH_BLOCK, ZH_LOBYTE( nSize ), ZH_HIBYTE( nSize ), ZH_COMP_PARAM );
   else
   {
      ++nSize;
      zh_macroGenPCode4( ZH_P_MPUSH_BLOCKLARGE, ZH_LOBYTE( nSize ), ZH_HIBYTE( nSize ), ZH_ULBYTE( nSize ), ZH_COMP_PARAM );
   }
   zh_macroGenPCode2( ZH_LOBYTE( usParms ), ZH_HIBYTE( usParms ), ZH_COMP_PARAM );

   /* copy a codeblock pcode buffer */
   zh_macroGenPCodeN( pCodeblock->pCode, pCodeblock->nPCodePos, ZH_COMP_PARAM );
   zh_macroGenPCode1( ZH_P_ENDBLOCK, ZH_COMP_PARAM ); /* finish the codeblock */

   /* free memory allocated for a codeblock */
   zh_xfree( pCodeblock->pCode );
   zh_xfree( pCodeblock );
}
