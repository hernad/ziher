/*
 * The Virtual Machine
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * Copyright 1999-2001 Viktor Szakats
 *   (zh_vmPushLongConst(), zh_vmPushDoubleConst())
 * Copyright 1999 Eddie Runia <eddie@runia.com>
 *   (__dbgVMVarSGet(), __dbgVMVarSList())
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


#include "zh_vm_opt.h"
#include "zh_api.h"
#include "zh_stack.h"
#include "zh_error_api.h"
#include "zh_class_api.h"
#include "zh_debug_api.h"
#include "zh_item_api.h"
#include "zh_lang_api.h"
#include "zh_rdd_api.h"
#include "zh_gt_api.h"
#include "zh_codepage_api.h"
#include "zh_vm.h"
#include "zh_xvm.h"
#include "zh_pcode.h"
#include "zh_set.h"
#include "zh_date.h"
#include "zh_math.h"
#include "zh_thread.h"


#include "debug.zhh"
#include "memory.zhh"

#ifndef ZH_NO_PROFILER
#  include <time.h>
#endif

ZIHERF zh_vmDoBlock( void );

ZH_FUNC_EXTERN( SYSINIT );
ZH_FUNC_EXTERN( BREAK );

/* PCode functions */

/* Operators (mathematical / character / misc) */
static void    zh_vmNegate( void );          /* negates (-) the latest value on the stack */
static void    zh_vmInc( PZH_ITEM pItem );   /* increment the latest numeric value on the stack */
static void    zh_vmDec( PZH_ITEM pItem );   /* decrements the latest numeric value on the stack */
static void    zh_vmFuncPtr( void );         /* pushes a function address pointer. Removes the symbol from the stack */
static void    zh_vmAddInt( PZH_ITEM pResult, ZH_LONG lAdd );      /* add integer to given item */
static void    zh_vmPlus( PZH_ITEM pResult, PZH_ITEM pItem1, PZH_ITEM pItem2 );        /* sums given values */
static void    zh_vmMinus( PZH_ITEM pResult, PZH_ITEM pItem1, PZH_ITEM pItem2 );       /* subtracts given values */
static void    zh_vmMult( PZH_ITEM pResult, PZH_ITEM pItem1, PZH_ITEM pItem2 );        /* multiplies given values */
static void    zh_vmDivide( PZH_ITEM pResult, PZH_ITEM pItem1, PZH_ITEM pItem2 );      /* divides the given values */
static void    zh_vmModulus( PZH_ITEM pResult, PZH_ITEM pItem1, PZH_ITEM pItem2 );     /* calculates modulus given values */
static void    zh_vmPower( PZH_ITEM pResult, PZH_ITEM pItem1, PZH_ITEM pItem2 );       /* power given values */

/* Operators (relational) */
static void    zh_vmEqual( void );           /* checks if the two latest values on the stack are equal, removes both and leaves result */
static void    zh_vmExactlyEqual( void );    /* checks if the two latest values on the stack are exactly equal, removes both and leaves result */
static void    zh_vmNotEqual( void );        /* checks if the two latest values on the stack are not equal, removes both and leaves result */
static void    zh_vmLess( void );            /* checks if the latest - 1 value is less than the latest, removes both and leaves result */
static void    zh_vmLessEqual( void );       /* checks if the latest - 1 value is less than or equal the latest, removes both and leaves result */
static void    zh_vmGreater( void );         /* checks if the latest - 1 value is greater than the latest, removes both and leaves result */
static void    zh_vmGreaterEqual( void );    /* checks if the latest - 1 value is greater than or equal the latest, removes both and leaves result */
static void    zh_vmInstring( void );        /* check whether string 1 is contained in string 2 */
static void    zh_vmForTest( void );         /* test for end condition of for */
static void    zh_vmSeqBlock( void );        /* set begin sequence WITH codeblock */
static void    zh_vmWithObjectStart( void ); /* prepare WITH OBJECT block */
static void    zh_vmEnumStart( int nVars, int nDescend ); /* prepare FOR EACH loop */
static void    zh_vmEnumNext( void );        /* increment FOR EACH loop counter */
static void    zh_vmEnumPrev( void );        /* decrement FOR EACH loop counter */
static void    zh_vmEnumEnd( void );         /* rewind the stack after FOR EACH loop counter */
static const ZH_BYTE * zh_vmSwitch( const ZH_BYTE * pCode, ZH_USHORT );  /* make a SWITCH statement */

/* Operators (logical) */
static void    zh_vmNot( void );             /* changes the latest logical value on the stack */
static void    zh_vmAnd( void );             /* performs the logical AND on the latest two values, removes them and leaves result on the stack */
static void    zh_vmOr( void );              /* performs the logical OR on the latest two values, removes them and leaves result on the stack */

/* Array */
static void    zh_vmArrayPush( void );       /* pushes an array element to the stack, removing the array and the index from the stack */
static void    zh_vmArrayPushRef( void );    /* pushes a reference to an array element to the stack, removing the array and the index from the stack */
static void    zh_vmArrayPop( void );        /* pops a value from the stack */
static void    zh_vmArrayDim( ZH_USHORT uiDimensions ); /* generates an uiDimensions Array and initialize those dimensions from the stack values */
static void    zh_vmArrayGen( ZH_SIZE nElements ); /* generates an nElements Array and fills it from the stack values */
static void    zh_vmHashGen( ZH_SIZE nElements ); /* generates an nElements Hash and fills it from the stack values */

/* macros */
static void    zh_vmMacroDo( ZH_USHORT uiArgSets );         /* execute function passing arguments set on ZHVM stack func( &var ) */
static void    zh_vmMacroFunc( ZH_USHORT uiArgSets );       /* execute procedure passing arguments set on ZHVM stack func( &var ) */
static void    zh_vmMacroSend( ZH_USHORT uiArgSets );       /* execute procedure passing arguments set on ZHVM stack func( &var ) */
static void    zh_vmMacroArrayGen( ZH_USHORT uiArgSets );   /* generate array from arguments set on ZHVM stack { &var } */
static void    zh_vmMacroPushIndex( void );              /* push macro array index {...}[ &var ] */

/* Database */
static ZH_ERRCODE zh_vmSelectWorkarea( PZH_ITEM, PZH_SYMBOL );  /* select the workarea using a given item or a substituted value */
static void       zh_vmSwapAlias( void );           /* swaps items on the eval stack and pops the workarea number */

/* Execution */

static void    zh_vmFrame( ZH_USHORT usLocals, unsigned char ucParams ); /* increases the stack pointer for the amount of locals and params supplied */
static void    zh_vmVFrame( ZH_USHORT usLocals, unsigned char ucParams ); /* increases the stack pointer for the amount of locals and variable number of params supplied */
static void    zh_vmSFrame( PZH_SYMBOL pSym );     /* sets the statics frame for a function */
static void    zh_vmStatics( PZH_SYMBOL pSym, ZH_USHORT uiStatics ); /* increases the global statics array to hold a ZH statics */
static void    zh_vmInitThreadStatics( ZH_USHORT uiCount, const ZH_BYTE * pCode ); /* mark thread static variables */
static void    zh_vmStaticsClear( void );       /* clear complex static variables */
static void    zh_vmStaticsRelease( void );     /* release arrays with static variables */
/* Push */
static void    zh_vmPushAlias( void );            /* pushes the current workarea number */
static void    zh_vmPushAliasedField( PZH_SYMBOL ); /* pushes an aliased field on the eval stack */
static void    zh_vmPushAliasedVar( PZH_SYMBOL );   /* pushes an aliased variable on the eval stack */
static void    zh_vmPushBlock( const ZH_BYTE * pCode, PZH_SYMBOL pSymbols, ZH_SIZE nLen ); /* creates a codeblock */
static void    zh_vmPushBlockShort( const ZH_BYTE * pCode, PZH_SYMBOL pSymbols, ZH_SIZE nLen ); /* creates a codeblock */
static void    zh_vmPushMacroBlock( const ZH_BYTE * pCode, ZH_SIZE nSize, ZH_USHORT usParams ); /* creates a macro-compiled codeblock */
static void    zh_vmPushDoubleConst( double dNumber, int iWidth, int iDec ); /* Pushes a double constant (pcode) */
static void    zh_vmPushLocal( int iLocal );       /* pushes the content of a local onto the stack */
static void    zh_vmPushLocalByRef( int iLocal );  /* pushes a local by reference onto the stack */
static void    zh_vmPushZHLong( ZH_MAXINT nNumber ); /* pushes a ZH_MAXINT number onto the stack */
#if ! defined( ZH_LONG_LONG_OFF )
   static void zh_vmPushLongLongConst( ZH_LONGLONG lNumber );  /* Pushes a long long constant (pcode) */
#endif
#if ZH_VMINT_MAX >= INT32_MAX
static void    zh_vmPushIntegerConst( int iNumber );  /* Pushes a int constant (pcode) */
#else
static void    zh_vmPushLongConst( long lNumber );    /* Pushes a long constant (pcode) */
#endif
static void    zh_vmPushStatic( ZH_USHORT uiStatic );     /* pushes the content of a static onto the stack */
static void    zh_vmPushStaticByRef( ZH_USHORT uiStatic ); /* pushes a static by reference onto the stack */
static void    zh_vmPushVariable( PZH_SYMBOL pVarSymb ); /* pushes undeclared variable */
static void    zh_vmPushObjectVarRef( void );   /* pushes reference to object variable */
static void    zh_vmPushVParams( void );        /* pushes variable parameters */
static void    zh_vmPushAParams( void );        /* pushes array items */
static void    zh_vmPushUnRef( void );          /* push the unreferenced latest value on the stack */
static void    zh_vmDuplicate( void );          /* duplicates the latest value on the stack */
static void    zh_vmDuplUnRef( void );          /* duplicates the latest value on the stack and unref the source one */
static void    zh_vmSwap( int iCount );        /* swap bCount+1 time two items on ZHVM stack starting from the most top one */

/* Pop */
static ZH_BOOL zh_vmPopLogical( void );           /* pops the stack latest value and returns its logical value */
static void    zh_vmPopAlias( void );             /* pops the workarea number form the eval stack */
static void    zh_vmPopAliasedField( PZH_SYMBOL );  /* pops an aliased field from the eval stack*/
static void    zh_vmPopAliasedVar( PZH_SYMBOL );    /* pops an aliased variable from the eval stack*/
static void    zh_vmPopLocal( int iLocal );       /* pops the stack latest value onto a local */
static void    zh_vmPopStatic( ZH_USHORT uiStatic ); /* pops the stack latest value onto a static */

/* misc */
static void    zh_vmDoInitStatics( void );        /* executes all _INITSTATICS functions */
static void    zh_vmDoInitFunctions( void );   /* executes all defined PRGs INIT functions */
static void    zh_vmDoExitFunctions( void );      /* executes all defined PRGs EXIT functions */
static void    zh_vmReleaseLocalSymbols( void );  /* releases the memory of the local symbols linked list */

static void    zh_vmMsgIndexReference( PZH_ITEM pRefer, PZH_ITEM pObject, PZH_ITEM pIndex ); /* create object index reference */

static void    zh_vmLocalName( ZH_USHORT uiLocal, const char * szLocalName ); /* locals and parameters index and name information for the debugger */
static void    zh_vmStaticName( ZH_BYTE bIsGlobal, ZH_USHORT uiStatic, const char * szStaticName ); /* statics vars information for the debugger */
static void    zh_vmModuleName( const char * szModuleName ); /* ZH and function name information for the debugger */

static void    zh_vmDebugEntry( int nMode, int nLine, const char * szName, int nIndex, PZH_ITEM pFrame );
static void    zh_vmDebuggerExit( ZH_BOOL fRemove );      /* shuts down the debugger */
static void    zh_vmDebuggerShowLine( ZH_USHORT uiLine ); /* makes the debugger shows a specific source code line */
static void    zh_vmDebuggerEndProc( void );     /* notifies the debugger for an endproc */

static PZH_DYNSYMBOL s_pDynsDbgEntry = NULL;   /* Cached __DBGENTRY symbol */
static ZH_DBGENTRY_FUNC s_pFunDbgEntry;   /* C level debugger entry */

static ZH_BOOL s_fInternalsEnabled = ZH_TRUE;

static int vmInitCount = 0;

static int volatile zh_vmThreadRequest = 0;
static void zh_vmRequestTest( void );

static PZH_ITEM s_pSymbolsMtx = NULL;

static ZH_CRITICAL_NEW( s_atInitMtx );
#  define ZH_ATINIT_LOCK()    zh_threadEnterCriticalSection( &s_atInitMtx )
#  define ZH_ATINIT_UNLOCK()  zh_threadLeaveCriticalSection( &s_atInitMtx )
#  define ZH_TASK_SHEDULER()  ZH_THREAD_SHEDULER()

#ifndef ZH_NO_PROFILER
static ZH_ULONG zh_ulOpcodesCalls[ ZH_P_LAST_PCODE ]; /* array to profile opcodes calls */
static ZH_ULONG zh_ulOpcodesTime[ ZH_P_LAST_PCODE ];  /* array to profile opcodes consumed time */
static ZH_BOOL zh_bProfiler = ZH_FALSE;                        /* profiler status is off */
#endif

#if defined( ZH_PRG_TRACE )
static ZH_BOOL zh_bTracePrgCalls = ZH_FALSE; /* zh tracing is off */
#  define ZH_TRACE_PRG( _TRMSG_ ) if( zh_bTracePrgCalls ) ZH_TRACE( ZH_TR_ALWAYS, _TRMSG_ )
#else
#  define ZH_TRACE_PRG( _TRMSG_ )
#endif

static const char * s_vm_pszLinkedMain = NULL; /* name of startup function set by linker */

/* virtual machine state */


//ZH_SYMBOL zh_symEval = { "EVAL",  { ZH_FS_PUBLIC }, { zh_vmDoBlock }, NULL }; /* symbol to evaluate codeblocks */
//static ZH_SYMBOL  s_symBreak = { "BREAK", { ZH_FS_PUBLIC }, { ZH_FUNCNAME( BREAK ) }, NULL }; /* symbol to generate break */
static PZH_ITEM s_breakBlock = NULL;

static ZH_BOOL  s_fZHVMActive = ZH_FALSE;  /* is ZHVM ready for PCODE executing */
static ZH_BOOL  s_fDoExitProc = ZH_TRUE;  /* execute EXIT procedures */
static int      s_nErrorLevel = 0;     /* application exit status */
static PZH_SYMBOL s_pSymStart = NULL;    /* start symbol of the application. MAIN() is not required */

static PZH_SYMBOLS s_pSymbols = NULL;  /* to hold a linked list of all different modules symbol tables */
static ZH_ULONG    s_ulFreeSymbols = 0;/* number of free module symbols */
static void *      s_hDynLibID = NULL; /* unique identifier to mark symbol tables loaded from dynamic libraries */
static ZH_BOOL     s_fCloneSym = ZH_FALSE;/* clone registered symbol tables */

/* main VM thread stack ID */
static void * s_main_thread = NULL;

/* SEQUENCE envelope items position from stack top active
 */
#define ZH_RECOVER_STATE   -1
#define ZH_RECOVER_VALUE   -2

#define ZH_SEQ_CANRECOVER  64
#define ZH_SEQ_DOALWAYS    128

static PZH_FUNC_LIST s_InitFunctions = NULL;
static PZH_FUNC_LIST s_ExitFunctions = NULL;
static PZH_FUNC_LIST s_QuitFunctions = NULL;

static PZH_ITEM zh_breakBlock( void )
{
   if( s_breakBlock == NULL )
   {
      static const ZH_BYTE s_pCode[ 8 ] = {
                             ZH_P_PUSHFUNCSYM, 0, 0,  /* BREAK */
                             ZH_P_PUSHLOCALNEAR, 1,   /* oErr */
                             ZH_P_FUNCTIONSHORT, 1,
                             ZH_P_ENDBLOCK };

      s_breakBlock = zh_itemNew( NULL );
      s_breakBlock->item.asBlock.value =
         zh_codeblockNew( s_pCode,  /* pcode buffer         */
                          0,        /* number of referenced local variables */
                          NULL,     /* table with referenced local variables */
                          pZhSymBreak,
                          sizeof( s_pCode ) );
      s_breakBlock->type = ZH_IT_BLOCK;
      s_breakBlock->item.asBlock.paramcnt = 1;
      s_breakBlock->item.asBlock.lineno = 0;
      s_breakBlock->item.asBlock.hclass = 0;
      s_breakBlock->item.asBlock.method = 0;
   }
   return s_breakBlock;
}

static void zh_breakBlockRelease( void )
{
   if( s_breakBlock != NULL )
   {
      zh_itemRelease( s_breakBlock );
      s_breakBlock = NULL;
   }
}

static void zh_vmAddModuleFunction( PZH_FUNC_LIST * pLstPtr, ZH_INIT_FUNC pFunc, void * cargo )
{
   PZH_FUNC_LIST pLst = ( PZH_FUNC_LIST ) zh_xgrab( sizeof( ZH_FUNC_LIST ) );

   pLst->pFunc = pFunc;
   pLst->cargo = cargo;
   pLst->hDynLib = s_hDynLibID;
   ZH_ATINIT_LOCK();
   pLst->pNext = *pLstPtr;
   *pLstPtr = pLst;
   ZH_ATINIT_UNLOCK();
}

static void zh_vmDoModuleFunctions( PZH_FUNC_LIST * pLstPtr )
{
   while( *pLstPtr )
   {
      PZH_FUNC_LIST pLst = *pLstPtr;
      *pLstPtr = pLst->pNext;
      pLst->pFunc( pLst->cargo );
      zh_xfree( pLst );
   }
}

static void zh_vmDoModuleLibFunctions( PZH_FUNC_LIST * pLstPtr, void * hDynLib )
{
   while( *pLstPtr )
   {
      PZH_FUNC_LIST pLst = *pLstPtr;
      if( pLst->hDynLib == hDynLib )
      {
         *pLstPtr = pLst->pNext;
         pLst->pFunc( pLst->cargo );
         zh_xfree( pLst );
      }
      else
         pLstPtr = &pLst->pNext;
   }
}

static void zh_vmDoModuleSetLibID( PZH_FUNC_LIST pLst, void * hDynLib, void * hNewDynLib )
{
   while( pLst )
   {
      if( pLst->hDynLib == hDynLib )
         pLst->hDynLib = hNewDynLib;
      pLst = pLst->pNext;
   }
}

static void zh_vmCleanModuleFunctions( void )
{
   PZH_FUNC_LIST pLst;

   while( s_InitFunctions )
   {
      pLst = s_InitFunctions;
      s_InitFunctions = pLst->pNext;
      zh_xfree( pLst );
   }
   while( s_ExitFunctions )
   {
      pLst = s_ExitFunctions;
      s_ExitFunctions = pLst->pNext;
      zh_xfree( pLst );
   }
   while( s_QuitFunctions )
   {
      pLst = s_QuitFunctions;
      s_QuitFunctions = pLst->pNext;
      zh_xfree( pLst );
   }
}

void zh_vmAtInit( ZH_INIT_FUNC pFunc, void * cargo )
{
   zh_vmAddModuleFunction( &s_InitFunctions, pFunc, cargo );
}

void zh_vmAtExit( ZH_INIT_FUNC pFunc, void * cargo )
{
   zh_vmAddModuleFunction( &s_ExitFunctions, pFunc, cargo );
}

void zh_vmAtQuit( ZH_INIT_FUNC pFunc, void * cargo )
{
   zh_vmAddModuleFunction( &s_QuitFunctions, pFunc, cargo );
}

static void zh_vmDoModuleInitFunctions( void )
{
   zh_vmDoModuleFunctions( &s_InitFunctions );
}

static void zh_vmDoModuleExitFunctions( void )
{
   zh_vmDoModuleFunctions( &s_ExitFunctions );
}

static void zh_vmDoModuleQuitFunctions( void )
{
   zh_vmDoModuleFunctions( &s_QuitFunctions );
}


/* call __ZZHVMINIT() function to initialize GetList public variable
 * and set ErrorBlock() by ErrorSys() function
 */
static void zh_vmDoInitZHVM( void )
{
   char * sFunc = "__ZHVMINIT";
   PZH_DYNSYMBOL pDynSym = zh_dynsymFind( sFunc );
   
   printf(">>>>>>>>>>>>>>> __ZHVMINIT %p  %p\n", pDynSym, pDynSym->pSymbol->value.pFunPtr);

   if( pDynSym && pDynSym->pSymbol->value.pFunPtr )
   {
      ZH_TRACE( ZH_TR_DEBUG, ( "__ZHVMINIT before push" ) );
 
      zh_vmPushSymbol( pDynSym->pSymbol );
      zh_vmPushNil();
      zh_vmProc( 0 );
   }; 


}

static void zh_vmDoInitZHObject( void )
{
   char * sFunc = "ZHOBJECT_VMINIT";
   PZH_DYNSYMBOL pDynSym = zh_dynsymFind( sFunc );
   
   if( pDynSym && pDynSym->pSymbol->value.pFunPtr )
   {
      printf(">>>>>>>>>>>>>>> ZHOBJECT_VMINIT %p  %p\n", pDynSym, pDynSym->pSymbol->value.pFunPtr);
      zh_vmPushSymbol( pDynSym->pSymbol );
      zh_vmPushNil();
      zh_vmProc( 0 );
   }; 

   char * sFunc2 = "ZHCLASS_VMINIT";
   PZH_DYNSYMBOL pDynSym2 = zh_dynsymFind( sFunc2 );
   if( pDynSym2 && pDynSym2->pSymbol->value.pFunPtr )
   {
      printf(">>>>>>>>>>>>>>> ZHCLASS_VMINIT %p  %p\n", pDynSym2, pDynSym2->pSymbol->value.pFunPtr);
      zh_vmPushSymbol( pDynSym2->pSymbol );
      zh_vmPushNil();
      zh_vmProc( 0 );
   }; 

   char * sFunc2b = "ZHCLASS_VMINIT2";
   PZH_DYNSYMBOL pDynSym2b = zh_dynsymFind( sFunc2b );
   if( pDynSym2b && pDynSym2b->pSymbol->value.pFunPtr )
   {
      printf(">>>>>>>>>>>>>>> ZHCLASS_VMINIT2 %p  %p\n", pDynSym2b, pDynSym2b->pSymbol->value.pFunPtr);
      zh_vmPushSymbol( pDynSym2b->pSymbol );
      zh_vmPushNil();
      zh_vmProc( 0 );
   }; 

   char * sFunc3 = "GET_VMINIT";
   PZH_DYNSYMBOL pDynSym3 = zh_dynsymFind( sFunc3 );
   if( pDynSym3 && pDynSym3->pSymbol->value.pFunPtr )
   {
      printf(">>>>>>>>>>>>>>> GET_VMINIT %p  %p\n", pDynSym3, pDynSym3->pSymbol->value.pFunPtr);
      zh_vmPushSymbol( pDynSym3->pSymbol );
      zh_vmPushNil();
      zh_vmProc( 0 );
   }; 

   char * sFunc4 = "__GET_VMINIT";
   PZH_DYNSYMBOL pDynSym4 = zh_dynsymFind( sFunc4 );
   if( pDynSym4 && pDynSym4->pSymbol->value.pFunPtr )
   {
      printf(">>>>>>>>>>>>>>> __GET_VMINIT %p  %p\n", pDynSym4, pDynSym4->pSymbol->value.pFunPtr);
      zh_vmPushSymbol( pDynSym4->pSymbol );
      zh_vmPushNil();
      zh_vmProc( 0 );
   }; 

   //char * sFunc5 = "ERROR_VMINIT";
   //PZH_DYNSYMBOL pDynSym5 = zh_dynsymFind( sFunc5 );
   //if( pDynSym5 && pDynSym5->pSymbol->value.pFunPtr )
   //{
   //   printf(">>>>>>>>>>>>>>> ERROR_VMINIT %p  %p\n", pDynSym5, pDynSym5->pSymbol->value.pFunPtr);
   //   zh_vmPushSymbol( pDynSym5->pSymbol );
   //   zh_vmPushNil();
   //   zh_vmProc( 0 );
   //}; 
}


static ZH_CRITICAL_NEW( s_vmMtx );
static ZH_COND_NEW( s_vmCond );

/* number of allocated ZHVM stacks */
static int volatile s_iStackCount = 0;
/* number of running ZHVM threads */
static int volatile s_iRunningCount = 0;

/* active ZHVM stacks list */
static PZH_THREADSTATE s_vmStackLst = NULL;

/* thread number */
static ZH_THREAD_NO s_threadNo = 0;

#  define ZH_THREQUEST_STOP   1
#  define ZH_THREQUEST_QUIT   2

#  define ZH_VM_LOCK()    zh_threadEnterCriticalSection( &s_vmMtx )
#  define ZH_VM_UNLOCK()  zh_threadLeaveCriticalSection( &s_vmMtx )

ZH_BOOL zh_vmIsMt( void ) { return ZH_TRUE; }

static void zh_vmRequestTest( void )
{
   ZH_VM_LOCK();

   s_iRunningCount--;
   for( ;; )
   {
      if( zh_vmThreadRequest & ZH_THREQUEST_QUIT )
      {
         
         if( ! zh_stackQuitState() )
         {
            zh_stackSetQuitState( ZH_TRUE );
            zh_stackSetActionRequest( ZH_QUIT_REQUESTED );
         }
      }
      if( zh_vmThreadRequest & ZH_THREQUEST_STOP )
      {
         zh_threadCondBroadcast( &s_vmCond );
         zh_threadCondWait( &s_vmCond, &s_vmMtx );
      }
      else
         break;
   }
   s_iRunningCount++;

   ZH_VM_UNLOCK();
}

/* unlock VM, allow GC and other exclusive single task code execution */
void zh_vmUnlock( void )
{
   if( s_fZHVMActive )
   {
      

      if( zh_stackId() )   /* check if thread has associated ZHVM stack */
      {
         if( zh_stackUnlock() == 1 )
         {
            ZH_VM_LOCK();
            s_iRunningCount--;
            if( zh_vmThreadRequest )
            {
               if( zh_vmThreadRequest & ZH_THREQUEST_QUIT )
               {
                  if( ! zh_stackQuitState() )
                  {
                     zh_stackSetQuitState( ZH_TRUE );
                     zh_stackSetActionRequest( ZH_QUIT_REQUESTED );
                  }
               }
               zh_threadCondBroadcast( &s_vmCond );
            }
            ZH_VM_UNLOCK();
         }
      }

      ZH_TASK_SHEDULER();
   }
}

/* lock VM blocking GC and other exclusive single task code execution */
void zh_vmLock( void )
{
   if( s_fZHVMActive )
   {
      

      if( zh_stackId() )   /* check if thread has associated ZHVM stack */
      {
         if( zh_stackLock() == 0 )
         {
            ZH_VM_LOCK();
            for( ;; )
            {
               if( zh_vmThreadRequest & ZH_THREQUEST_QUIT )
               {
                  if( ! zh_stackQuitState() )
                  {
                     zh_stackSetQuitState( ZH_TRUE );
                     zh_stackSetActionRequest( ZH_QUIT_REQUESTED );
                  }
               }
               if( zh_vmThreadRequest & ZH_THREQUEST_STOP )
                  zh_threadCondWait( &s_vmCond, &s_vmMtx );
               else
                  break;
            }
            s_iRunningCount++;
            ZH_VM_UNLOCK();
         }
      }
   }
}

void zh_vmLockForce( void )
{
   if( s_fZHVMActive )
   {
      

      if( zh_stackId() )   /* check if thread has associated ZHVM stack */
      {
         if( zh_stackLock() == 0 )
         {
            ZH_VM_LOCK();
            if( zh_vmThreadRequest & ZH_THREQUEST_QUIT )
            {
               if( ! zh_stackQuitState() )
               {
                  zh_stackSetQuitState( ZH_TRUE );
                  zh_stackSetActionRequest( ZH_QUIT_REQUESTED );
               }
            }
            s_iRunningCount++;
            ZH_VM_UNLOCK();
         }
      }
   }
}

/* (try to) stop all threads except current one */
ZH_BOOL zh_vmSuspendThreads( ZH_BOOL fWait )
{
   ZH_VM_LOCK();

   if( ( zh_vmThreadRequest & ( ZH_THREQUEST_STOP | ZH_THREQUEST_QUIT ) ) == 0 )
   {
      zh_vmThreadRequest |= ZH_THREQUEST_STOP;
      --s_iRunningCount;
      for( ;; )
      {
         if( s_iRunningCount <= 0 )
         {
            ++s_iRunningCount;
            return ZH_TRUE;
         }
         if( ! fWait )
            break;
         zh_threadCondWait( &s_vmCond, &s_vmMtx );
         if( zh_vmThreadRequest & ZH_THREQUEST_QUIT )
            break;
      }
      ++s_iRunningCount;
      zh_vmThreadRequest &= ~ZH_THREQUEST_STOP;
      zh_threadCondBroadcast( &s_vmCond );
   }

   ZH_VM_UNLOCK();

   return ZH_FALSE;
}

/* unblock execution of threads stopped by zh_vmSuspendThreads() */
void zh_vmResumeThreads( void )
{
   zh_vmThreadRequest &= ~ZH_THREQUEST_STOP;
   zh_threadCondBroadcast( &s_vmCond );
   ZH_VM_UNLOCK();
}

/* send QUIT request to all threads except current one
 * and wait for their termination,
 * should be called only by main ZHVM thread
 */
void zh_vmTerminateThreads( void )
{
   

   if( s_main_thread == zh_stackId() )
   {
      ZH_VM_LOCK();

      zh_vmThreadRequest |= ZH_THREQUEST_QUIT;
      --s_iRunningCount;

      zh_threadMutexUnlockAll();
      zh_threadMutexUnsubscribeAll();

      zh_threadCondBroadcast( &s_vmCond );

      while( s_iStackCount > 1 )
         zh_threadCondWait( &s_vmCond, &s_vmMtx );

      ++s_iRunningCount;

      zh_vmThreadRequest = 0;

      ZH_VM_UNLOCK();
   }
}

/* wait for all threads to terminate
 * should be called only by main ZHVM thread
 */
void zh_vmWaitForThreads( void )
{
   

   if( s_main_thread == zh_stackId() )
   {
      ZH_VM_LOCK();

      --s_iRunningCount;
      if( zh_vmThreadRequest )
         zh_threadCondBroadcast( &s_vmCond );

      while( s_iStackCount > 1 )
         zh_threadCondWait( &s_vmCond, &s_vmMtx );

      ++s_iRunningCount;

      ZH_VM_UNLOCK();
   }
}

void * zh_vmThreadState( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmThreadState()" ) );

   return zh_stackId() ? zh_stackList() : NULL;
}

ZH_BOOL zh_vmThreadIsMain( void * Cargo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmThreadIsMain(%p)", Cargo ) );

   if( ! s_fZHVMActive || s_main_thread == NULL )
      return ZH_FALSE;
   else if( Cargo )
      return s_main_thread == ( ( PZH_THREADSTATE ) Cargo )->pStackId;
   else
   {
      
      return s_main_thread == zh_stackId();
   }
}

static void zh_vmStackAdd( PZH_THREADSTATE pState )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmStackAdd(%p)", ( void * ) pState ) );

   if( ! pState->pPrev )
   {
      if( s_vmStackLst )
      {
         pState->pNext = s_vmStackLst;
         pState->pPrev = s_vmStackLst->pPrev;
         pState->pPrev->pNext = pState;
         s_vmStackLst->pPrev = pState;
      }
      else
      {
         s_vmStackLst = pState->pNext = pState->pPrev = pState;
      }
      s_iStackCount++;
   }
   if( pState->th_no == 0 )
      pState->th_no = ++s_threadNo;
}

static PZH_ITEM zh_vmStackDel( PZH_THREADSTATE pState, ZH_BOOL fCounter )
{
   PZH_ITEM pThItm;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmStackDel(%p,%d)", ( void * ) pState, ( int ) fCounter ) );

   pState->fActive = ZH_FALSE;
   pState->pStackId = NULL;
   pState->fFinished = ZH_TRUE;

   if( pState->pPrev )
   {
      pState->pPrev->pNext = pState->pNext;
      pState->pNext->pPrev = pState->pPrev;
      if( s_vmStackLst == pState )
      {
         s_vmStackLst = pState->pNext;
         if( s_vmStackLst == pState )
            s_vmStackLst = NULL;
      }
      pState->pPrev = pState->pNext = NULL;
      if( fCounter )
         s_iStackCount--;
   }

   /* NOTE: releasing pThItm may force pState freeing if parent
    *       thread does not keep thread pointer item. So it's
    *       important to not access it later. [druzus]
    */
   pThItm = pState->pThItm;
   pState->pThItm = NULL;

   return pThItm;
}

static void zh_vmStackInit( PZH_THREADSTATE pState )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmStackInit(%p)", ( void * ) pState ) );

   zh_stackInit();      /* initialize ZHVM thread stack */

   ZH_VM_LOCK();
   {
      

      zh_stackUnlock();
      pState->pStackId = zh_stackId();
      zh_stackListSet( ( void * ) ( pState ) );
      pState->fActive = ZH_TRUE;
      zh_vmStackAdd( pState );
   }
   ZH_VM_UNLOCK();
}

static void zh_vmStackRelease( void )
{
   
   ZH_BOOL fLocked;
   PZH_ITEM pThItm;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmStackRelease()" ) );

   ZH_VM_LOCK();

   fLocked = zh_stackUnlock() == 1;
   pThItm = zh_vmStackDel( ( PZH_THREADSTATE ) zh_stackList(), ZH_FALSE );

   ZH_VM_UNLOCK();

   /* NOTE: releasing pThItm may force pState freeing if parent
    *       thread does not keep thread pointer item. So it's
    *       important to not access it later. [druzus]
    */
   if( pThItm )
      zh_itemRelease( pThItm );

   zh_setRelease( zh_stackSetStruct() );
   zh_stackFree();

   zh_threadMutexUnlockAll();

   ZH_VM_LOCK();

   if( fLocked )
      s_iRunningCount--;

   s_iStackCount--;
   zh_threadCondBroadcast( &s_vmCond );

   ZH_VM_UNLOCK();
}

ZH_BOOL zh_vmThreadRegister( void * Cargo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmThreadRegister(%p)", Cargo ) );

   ZH_VM_LOCK();

   zh_vmStackAdd( ( PZH_THREADSTATE ) Cargo );

   ZH_VM_UNLOCK();

   return ZH_TRUE;
}

void zh_vmThreadRelease( void * Cargo )
{
   PZH_ITEM pThItm;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmThreadRelease(%p)", Cargo ) );

   ZH_VM_LOCK();

   pThItm = zh_vmStackDel( ( PZH_THREADSTATE ) Cargo, ZH_TRUE );
   zh_threadCondBroadcast( &s_vmCond );

   ZH_VM_UNLOCK();

   if( pThItm )
      zh_itemRelease( pThItm );
}

/* thread entry point */
void zh_vmThreadInit( void * Cargo )
{
   PZH_THREADSTATE pState;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmThreadInit(%p)", Cargo ) );

   pState = ( PZH_THREADSTATE ) Cargo;
   if( ! pState )
      pState = zh_threadStateNew();

   zh_vmStackInit( pState );  /* initialize ZHVM thread stack */
   zh_vmLock();
   {
      

      zh_codepageSelectID( pState->pszCDP );
      zh_langSelectID( pState->pszLang );

      zh_vmSetI18N( pState->pI18N );
      pState->pI18N = NULL;

      if( pState->pSet )
      {
         memcpy( zh_stackSetStruct(), pState->pSet, sizeof( ZH_SET_STRUCT ) );
         zh_xfree( pState->pSet );
         pState->pSet = NULL;
      }
      else
         zh_setInitialize( zh_stackSetStruct() );

      zh_gtAttach( pState->hGT );
      pState->hGT = NULL;

      if( pState->pszDefRDD )
         zh_stackRDD()->szDefaultRDD = pState->pszDefRDD;

      if( s_fZHVMActive )
      {
         /* call __ZZHVMINIT() function to initialize GetList public variable
          * and set ErrorBlock() by ErrorSys() function
          */
         zh_vmDoInitZHVM();
         
      }

      if( pState->pMemvars )
      {
         zh_memvarRestoreFromArray( pState->pMemvars );
         zh_itemRelease( pState->pMemvars );
         pState->pMemvars = NULL;
      }
   }
}

/* thread leave point */
void zh_vmThreadQuit( void )
{
   
   PZH_THREADSTATE pState;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmThreadQuit()" ) );

   zh_stackSetQuitState( ZH_TRUE );
   zh_stackSetActionRequest( 0 );

   pState = ( PZH_THREADSTATE ) zh_stackList();
   {
      PZH_ITEM pReturn = zh_stackReturnItem();

      if( ZH_IS_BYREF( pReturn ) )
         pReturn = zh_itemUnRef( pReturn );

      if( ! pState->pResult )
      {
         pState->pResult = zh_itemNew( pReturn );
         zh_gcUnlock( pState->pResult );
      }
      else
         zh_itemCopy( pState->pResult, pReturn );
   }
   zh_itemClear( zh_stackReturnItem() );

   zh_stackSetActionRequest( 0 );
   zh_rddCloseAll();             /* close all workareas */
   zh_stackRemove( 1 );          /* clear stack items, leave only initial symbol item */
   zh_memvarsClear( ZH_TRUE );   /* clear all PUBLIC (and PRIVATE if any) variables */
   zh_vmSetI18N( NULL );         /* remove i18n translation table */
   zh_vmDebuggerExit( ZH_FALSE );   /* deactivate debugger */
   
   zh_gtRelease( NULL );

   zh_vmStackRelease();          /* release ZHVM stack and remove it from linked ZHVM stacks list */
}

/* send QUIT request to given thread */
void zh_vmThreadQuitRequest( void * Cargo )
{
   PZH_THREADSTATE pState;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmThreadQuitRequest(%p)", Cargo ) );

   pState = ( PZH_THREADSTATE ) Cargo;

   ZH_VM_LOCK();

   if( pState->pStackId && pState->fActive )
      zh_stackIdSetActionRequest( pState->pStackId, ZH_QUIT_REQUESTED );

   ZH_VM_UNLOCK();
}

PZH_ITEM zh_vmThreadStart( ZH_ULONG ulAttr, PZH_CARGO_FUNC pFunc, void * cargo )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmThreadStart(%lu,%p,%p)", ulAttr, ( void * ) pFunc, cargo ) );

   return zh_threadStart( ulAttr, pFunc, cargo );
}

void zh_vmSetFunction( PZH_SYMBOL pOldSym, PZH_SYMBOL pNewSym )
{
   PZH_SYMBOLS pLastSymbols = s_pSymbols;
   ZH_SYMBOL SymOldBuf, SymNewBuf;

   /* make copy of symbols to eliminate possible problem with
    * dynamic modification of passed parameters inside the loop
    */
   memcpy( &SymOldBuf, pOldSym, sizeof( SymOldBuf ) );
   pOldSym = &SymOldBuf;
   memcpy( &SymNewBuf, pNewSym, sizeof( SymNewBuf ) );
   pNewSym = &SymNewBuf;

   while( pLastSymbols )
   {
      ZH_USHORT ui, uiSymbols = pLastSymbols->uiModuleSymbols;

      for( ui = 0; ui < uiSymbols; ++ui )
      {
         PZH_SYMBOL pSym = pLastSymbols->pModuleSymbols + ui;

         if( pSym->value.pFunPtr == pOldSym->value.pFunPtr &&
             ( pSym->value.pFunPtr ||
               strcmp( pSym->szName, pOldSym->szName ) == 0 ) )
         {
            pSym->value.pFunPtr = pNewSym->value.pFunPtr;
            pSym->scope.value   = pNewSym->scope.value;
         }
      }
      pLastSymbols = pLastSymbols->pNext;
   }
}

void zh_vmSetDynFunc( PZH_DYNSYMBOL pDynSym )
{
   PZH_SYMBOLS pLastSymbols = s_pSymbols;

   while( pLastSymbols )
   {
      ZH_USHORT ui, uiSymbols = pLastSymbols->uiModuleSymbols;

      for( ui = 0; ui < uiSymbols; ++ui )
      {
         PZH_SYMBOL pSym = pLastSymbols->pModuleSymbols + ui;

         if( pSym->pDynSym == pDynSym && pDynSym->pSymbol != pSym )
            pSym->scope.value |= ZH_FS_DEFERRED;
      }
      pLastSymbols = pLastSymbols->pNext;
   }
}

/* Ziher application entry point */

void zh_vmInit( ZH_BOOL bStartMainProc, ZH_BOOL bInitRT, ZH_BOOL bConInit )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmInit()" ) );

#if defined( ZH_OS_WIN )
   zh_winmainArgVBuild();
#endif

   zh_xinit();
   zh_vmSetExceptionHandler();
   if (bInitRT) {
      printf("============== init symbol RT\n");
      zh_vmSymbolInit_RT();      /* initialize symbol table with runtime support functions */
      
   }

   zh_threadInit();

   zh_vmStackInit( zh_threadStateNew() ); /* initialize ZHVM thread stack */
   s_pSymbolsMtx = zh_threadMutexCreate();


   /* Set the language and codepage to the default */
   /* This trick is needed to stringify the macro value */
   zh_langSelectID( ZH_MACRO2STRING( ZH_LANG_DEFAULT ) );
   zh_codepageSelectID( ZH_MACRO2STRING( ZH_CODEPAGE_DEFAULT ) );
   {
      
      s_main_thread = zh_stackId();
      /* _SET_* initialization */
      zh_setInitialize( zh_stackSetStruct() );
   }

   //printf("init step 4\n");
   if (bInitRT) {
      zh_cmdargUpdate();
      //printf("init step 5\n");
      zh_clsInit(); /* initialize Classy/OO system */
      //printf("init step 6\n");
      zh_errInit();
      //printf("init step 7\n");
      zh_breakBlock();
   }



   printf("init step 9\n");
   if (bConInit)
     zh_conInit();

   /* Check for some internal switches */
   //printf("init step 10\n");

   if (bInitRT)
     zh_cmdargProcess();

   printf("init step 11\n");
   if (bInitRT)
     zh_i18n_init();            /* initialize i18n module */

#ifndef ZH_NO_PROFILER
   /* Initialize opcodes profiler support arrays */
   {
      ZH_ULONG ul;

      for( ul = 0; ul < ZH_P_LAST_PCODE; ul++ )
      {
         zh_ulOpcodesCalls[ ul ] = 0;
         zh_ulOpcodesTime[ ul ] = 0;
      }
   }
#endif

   /* enable executing PCODE (ZHVM reenter request) */
   s_fZHVMActive = ZH_TRUE;

   //printf("init step 12\n");
   /* lock main ZHVM thread */
   zh_vmLock();

   //printf("init step 13\n");
   s_pDynsDbgEntry = zh_dynsymFind( "__DBGENTRY" );
   if( s_pDynsDbgEntry )
   {
      /* Try to get C dbgEntry() function pointer */
      if( ! s_pFunDbgEntry )
         zh_vmDebugEntry( ZH_DBG_GETENTRY, 0, NULL, 0, NULL );
      if( ! s_pFunDbgEntry )
         s_pFunDbgEntry = zh_vmDebugEntry;
   }

   /* Call functions that initializes static variables
    * Static variables have to be initialized before any INIT functions
    * because INIT function can use static variables
    */
   
   //if (vmInitCount == 0) {
     zh_vmDoInitStatics();
   //  vmInitCount = 1;
   //}


   //zh_initDynTable();

   //if (bInitRT) {

      printf("init step 15\n");
      zh_vmDoInitZHVM();

      zh_vmDoInitZHObject();

      printf("init step 16f\n");
      
      
      zh_vmDoModuleInitFunctions();       /* process AtInit registered functions */
      printf("init step 17\n");

if (! zh_dynsymFindName( "FUNC_HELLO_ZIHER_2" )) {
         printf("=============== ext_zh_vm_SymbolInit_F18_UTILS_ZH  =============\n");
         //ext_zh_vm_SymbolInit_F18_UTILS_ZH();
         ext__zh_regex_init_();
         ext__zh_startup_gt_Init_TRM();

         ext_zh_vm_SymbolInit_A_DBFS_ZH();
         ext_zh_vm_SymbolInit_ADD_MCODE_ZH();
         ext_zh_vm_SymbolInit_ADRESAR_ZH();
         ext_zh_vm_SymbolInit_ADRESE_UGOVORI_LABELE_RTM_ZH();
         ext_zh_vm_SymbolInit_ARRAY_BROWSE_ZH();
         ext_zh_vm_SymbolInit_BARKOD_IMPORT_TXT_ZH();
         ext_zh_vm_SymbolInit_BARKOD_LABELE_DELPHIRB_ZH();
         ext_zh_vm_SymbolInit_BARKOD_TERMINAL_MAIN_ZH();
         ext_zh_vm_SymbolInit_BARKOD_TERMINAL_UTILS_ZH();
         ext_zh_vm_SymbolInit_BARKOD_ZH();
         ext_zh_vm_SymbolInit_BLAGAJNA_DNEVNI_IZVJESTAJ_ZH();
         ext_zh_vm_SymbolInit_BROJACI_ZH();
         ext_zh_vm_SymbolInit_BROWSE_FAKT_DOKUMENTI_ZH();
         ext_zh_vm_SymbolInit_BROWSE_P_SIFRA_ZH();
         ext_zh_vm_SymbolInit_BROWSE_STAVKA_ZH();
         ext_zh_vm_SymbolInit_BUG_REPORT_ZH();
         ext_zh_vm_SymbolInit_CALC_ZH();
         ext_zh_vm_SymbolInit_CLASS_CSV_READER_ZH();
         ext_zh_vm_SymbolInit_CLIPBOARD_ZH();
         ext_zh_vm_SymbolInit_CLS_DOK_ATTR_ZH();
         ext_zh_vm_SymbolInit_COALESCE_ZH();
         ext_zh_vm_SymbolInit_COMMON_BH_SLOVA_ZH();
         ext_zh_vm_SymbolInit_COMMON_PRINT_EPL2_ZH();
         ext_zh_vm_SymbolInit_COMMON_PRINT_GVIM_ZH();
         ext_zh_vm_SymbolInit_COMMON_PRINT_PTXT_ZH();
         ext_zh_vm_SymbolInit_COMMON_REPORT_COMMON_2_ZH();
         ext_zh_vm_SymbolInit_COMMON_REPORT_COMMON_ZH();
         ext_zh_vm_SymbolInit_COMMON_RPT_UTILS_ZH();
         ext_zh_vm_SymbolInit_COMMON_SIFRARNICI_ZH();
         ext_zh_vm_SymbolInit_COMMON_TIME_UTIL_ZH();
         ext_zh_vm_SymbolInit_COMMON_UTIL_ZH();
         ext_zh_vm_SymbolInit_COMMON_XML_ZH();
         ext_zh_vm_SymbolInit_CORE_0_ZH();
         ext_zh_vm_SymbolInit_CORE_COLORS_ZH();
         ext_zh_vm_SymbolInit_CORE_HB_UTIL_ZH();
         ext_zh_vm_SymbolInit__CORE_O_SIF_ZH();
         ext_zh_vm_SymbolInit_CORE_OS_ZH();
         ext_zh_vm_SymbolInit__CRE_ALL_DBFS_ZH();
         ext_zh_vm_SymbolInit_CRE_ALL_FAKT_ZH();
         ext_zh_vm_SymbolInit__CRE_ALL_FIN_ZH();
         ext_zh_vm_SymbolInit__CRE_ALL_KALK_ZH();
         ext_zh_vm_SymbolInit__CRE_ALL_LD_ZH();
         ext_zh_vm_SymbolInit_CRE_ALL_OS_ZH();
         ext_zh_vm_SymbolInit__CRE_ALL_POS_ZH();
         ext_zh_vm_SymbolInit__CRE_ALL_ROBA_ZH();
         ext_zh_vm_SymbolInit__CRE_ALL_VIRM_ZH();
         ext_zh_vm_SymbolInit_CRE_DBF_UGOVORI_ZH();
         ext_zh_vm_SymbolInit_CRE_FINMAT_ZH();
         ext_zh_vm_SymbolInit_CRE_POREZNA_FAKTURA_DBF_ZH();
         ext_zh_vm_SymbolInit_CRE_RELACIJE_FAKT_ZH();
         ext_zh_vm_SymbolInit__CRE_R_EXPORT_DBF_ZH();
         ext_zh_vm_SymbolInit_CRE_SIF_KONTO_VALUTE_ZH();
         ext_zh_vm_SymbolInit_CRE_SIF_PARTNERI_ZH();
         ext_zh_vm_SymbolInit_DB_CREATE_2_ZH();
         ext_zh_vm_SymbolInit_DBF_CHECKSUM_ZH();
         ext_zh_vm_SymbolInit_DBF_CREATE_INDEX_ZH();
         ext_zh_vm_SymbolInit_DBF_INIT_ZH();
         ext_zh_vm_SymbolInit_DBF_MODSTRU_ZH();
         ext_zh_vm_SymbolInit_DBF_PARAMS_ZH();
         ext_zh_vm_SymbolInit_DBF_SQL_ZH();
         ext_zh_vm_SymbolInit_DBF_STRUCT_ZH();
         ext_zh_vm_SymbolInit_DBF_UPDATE_SERVER_ZH();
         ext_zh_vm_SymbolInit_DESTINACIJE_2_ZH();
         ext_zh_vm_SymbolInit_DESTINACIJE_ZH();
         ext_zh_vm_SymbolInit_DIAG_INFO_ZH();
         ext_zh_vm_SymbolInit_DOWNLOAD_TEMPLATE_ZH();
         ext_zh_vm_SymbolInit_DUMMY_ZH();
         ext_zh_vm_SymbolInit_EDITOR_ZH();
         ext_zh_vm_SymbolInit_EISPORUKE_ZH();
         ext_zh_vm_SymbolInit_EMAIL_PODRSKA_ZH();
         ext_zh_vm_SymbolInit_EMAIL_SEND_ZH();
         ext_zh_vm_SymbolInit_ENABAVKE_EISPORUKE_DB_ZH();
         ext_zh_vm_SymbolInit_ENABAVKE_EISPORUKE_MENI_ZH();
         ext_zh_vm_SymbolInit_ENABAVKE_EISPORUKE_PDV_ZH();
         ext_zh_vm_SymbolInit_ENABAVKE_UVOZ_ZH();
         ext_zh_vm_SymbolInit_ENABAVKE_ZH();
         ext_zh_vm_SymbolInit_EXPORT_SIFARNIK_ZH();
         ext_zh_vm_SymbolInit_EXPORT_XLSX_ZH();
         ext_zh_vm_SymbolInit_F18_ADMIN_ZH();
         ext_zh_vm_SymbolInit_F18_BACKUP_ZH();
         ext_zh_vm_SymbolInit_F18_DBF_UPGRADE_ZH();
         ext_zh_vm_SymbolInit_F18_DBF_UTIL_ZH();
         ext_zh_vm_SymbolInit_F18_EDITOR_ZH();
         ext_zh_vm_SymbolInit_F18_INI_CONFIG_ZH();
         ext_zh_vm_SymbolInit_F18_INIT_0_ZH();
         ext_zh_vm_SymbolInit_F18_INIT_IDLE_HANDLERS_ZH();
         ext_zh_vm_SymbolInit_F18_INIT_ZH();
         ext_zh_vm_SymbolInit_F18_INI_ZH();
         ext_zh_vm_SymbolInit_F18_LOGIN_ZH();
         ext_zh_vm_SymbolInit_F18_LOG_ZH();
         ext_zh_vm_SymbolInit_F18_PARAMETERS_ZH();
         ext_zh_vm_SymbolInit_F18_REPORT_TEMPLATES_ZH();
         ext_zh_vm_SymbolInit_F18_RTM_ZH();
         ext_zh_vm_SymbolInit_F18_STR_CONVERT_ZH();
         ext_zh_vm_SymbolInit_F18_THREADS_ZH();
         ext_zh_vm_SymbolInit_F18_UPDATE_ZH();
         ext_zh_vm_SymbolInit_F18_UTILS_ZH();
         ext_zh_vm_SymbolInit_F18_VER_ZH();
         ext_zh_vm_SymbolInit_F18_ZH();
         ext_zh_vm_SymbolInit_F18_ZIP_UTIL_ZH();
         ext_zh_vm_SymbolInit_FAKT_ATRIBUTI_ZH();
         ext_zh_vm_SymbolInit_FAKT_AZURIRANJE_ZH();
         ext_zh_vm_SymbolInit_FAKT_BARKOD_TERMINAL_ZH();
         ext_zh_vm_SymbolInit_FAKT_BROJACI_DOKUMENATA_ZH();
         ext_zh_vm_SymbolInit_FAKT_COLUMN_ZH();
         ext_zh_vm_SymbolInit_FAKT_DOKUMENTI_ZH();
         ext_zh_vm_SymbolInit_FAKT_DOKUMENT_ZH();
         ext_zh_vm_SymbolInit_FAKT_EXPORT_LO_ZH();
         ext_zh_vm_SymbolInit_FAKT_EXPORT_ZH();
         ext_zh_vm_SymbolInit_FAKT_FISKALNI_RACUN_ZH();
         ext_zh_vm_SymbolInit_FAKT_FTXT_ZH();
         ext_zh_vm_SymbolInit_FAKT_GENERACIJA_DOKUMENTA_ZH();
         ext_zh_vm_SymbolInit_FAKT_INVENTURA_RPT_PROD_ZH();
         ext_zh_vm_SymbolInit_FAKT_INVENTURA_RPT_ZH();
         ext_zh_vm_SymbolInit_FAKT_INVETURA_INIT_ZH();
         ext_zh_vm_SymbolInit_FAKT_IZVJ_KARTICA_ZH();
         ext_zh_vm_SymbolInit_FAKT_IZVJ_KOLICINE_ZH();
         ext_zh_vm_SymbolInit_FAKT_IZVJ_LAGER_LISTA_ZH();
         ext_zh_vm_SymbolInit_FAKT_IZVJ_MENU_ZH();
         ext_zh_vm_SymbolInit_FAKT_IZVJ_REAL_KOL_ZH();
         ext_zh_vm_SymbolInit_FAKT_IZVJ_REAL_MP_ZH();
         ext_zh_vm_SymbolInit_FAKT_IZVJ_STANJE_ROBE_ZH();
         ext_zh_vm_SymbolInit_FAKT_LAUNCHER_ZH();
         ext_zh_vm_SymbolInit_FAKT_MNU_DOKUMENTI_ZH();
         ext_zh_vm_SymbolInit_FAKT_PARAMETRI_ZH();
         ext_zh_vm_SymbolInit_FAKT_POCETNO_STANJE_ZH();
         ext_zh_vm_SymbolInit_FAKT_POVRAT_ZH();
         ext_zh_vm_SymbolInit_FAKT_PREGLED_DOKUMENATA_TABELA_ZH();
         ext_zh_vm_SymbolInit_FAKT_PREGLED_DOKUMENATA_ZH();
         ext_zh_vm_SymbolInit_FAKT_PRENOS_KALK_FAKT_ZH();
         ext_zh_vm_SymbolInit_FAKT_PRENOS_MNU_ZH();
         ext_zh_vm_SymbolInit_FAKT_PRENOS_TOPS_FAKT_ZH();
         ext_zh_vm_SymbolInit_FAKT_PRINT_NARUDZBENICA_ZH();
         ext_zh_vm_SymbolInit_FAKT_REAL_PARTNERA_ZH();
         ext_zh_vm_SymbolInit_FAKT_SIFRARNICI_MNU_ZH();
         ext_zh_vm_SymbolInit_FAKT_SIFRARNICI_ZH();
         ext_zh_vm_SymbolInit__FAKT_SQL_ZH();
         ext_zh_vm_SymbolInit_FAKT_STAMPA_AZURIRANOG_DOKUMENTA_ZH();
         ext_zh_vm_SymbolInit_FAKT_STAMPA_DOKUMENTA_ODT_ZH();
         ext_zh_vm_SymbolInit_FAKT_STAMPA_DOKUMENTA_PDV_ZH();
         ext_zh_vm_SymbolInit_FAKT_STAMPA_DOKUMENTA_UTIL_ZH();
         ext_zh_vm_SymbolInit_FAKT_STAMPA_LISTE_DOKUMENATA_ZH();
         ext_zh_vm_SymbolInit_FAKT_TRASH_UTILS_ZH();
         ext_zh_vm_SymbolInit_FAKT_UDALJENA_RAZMJENA_ZH();
         ext_zh_vm_SymbolInit_FAKT_UNOS_COMMON_ZH();
         ext_zh_vm_SymbolInit_FAKT_UNOS_DOKUMENTA_UTIL_ZH();
         ext_zh_vm_SymbolInit_FAKT_UNOS_DOKUMENTA_ZH();
         ext_zh_vm_SymbolInit_FAKT_UNOS_FTXT_ZH();
         ext_zh_vm_SymbolInit_FAKT_UPOREDNA_LISTA_KALK_FAKT_ZH();
         ext_zh_vm_SymbolInit_FAKT_UTILS_ZH();
         ext_zh_vm_SymbolInit_FAKT_VRSTE_DOKUMENATA_ZH();
         ext_zh_vm_SymbolInit_FETCH_METRIC_ZH();
         ext_zh_vm_SymbolInit_FIN_ANAL_KARTICA_ZH();
         ext_zh_vm_SymbolInit_FIN_AZURIRANJE_NALOGA_ZH();
         ext_zh_vm_SymbolInit_FIN_BROJACI_DOKUMENATA_ZH();
         ext_zh_vm_SymbolInit_FIN_BRUTO_BILANS_ANALITIKA_B_ZH();
         ext_zh_vm_SymbolInit_FIN_BRUTO_BILANS_A_ZH();
         ext_zh_vm_SymbolInit_FIN_BRUTO_BILANS_GRUPE_B_ZH();
         ext_zh_vm_SymbolInit_FIN_BRUTO_BILANS_SINTETIKA_B_ZH();
         ext_zh_vm_SymbolInit_FIN_BRUTO_BILANS_SUBANALITIKA_B_ZH();
         ext_zh_vm_SymbolInit_FIN_DNEVNIK_NALOGA_ZH();
         ext_zh_vm_SymbolInit_FIN_IMPORT_ELBA_ZH();
         ext_zh_vm_SymbolInit_FIN_IOS_ZH();
         ext_zh_vm_SymbolInit_FIN_IZVJESTAJI_MENU_ZH();
         ext_zh_vm_SymbolInit_FIN_IZVJESTAJI_OSTALI_ZH();
         ext_zh_vm_SymbolInit_FIN_IZVJESTAJI_SPECIFIKACIJE_MENU_ZH();
         ext_zh_vm_SymbolInit_FIN_IZVJESTAJI_UTIL_ZH();
         ext_zh_vm_SymbolInit_FIN_KAMATE_GEN_ZH();
         ext_zh_vm_SymbolInit_FIN_KAMATE_KAMATNI_LIST_ZH();
         ext_zh_vm_SymbolInit_FIN_KAMATE_OBR_ZH();
         ext_zh_vm_SymbolInit_FIN_KAMATE_OPOMENA_PRED_TUZBU_ZH();
         ext_zh_vm_SymbolInit_FIN_KAMATE_PRINT_ZH();
         ext_zh_vm_SymbolInit_FIN_KAMATE_UNOS_ZH();
         ext_zh_vm_SymbolInit_FIN_KAMATE_UTIL_ZH();
         ext_zh_vm_SymbolInit_FIN_KARTICE_ZH();
         ext_zh_vm_SymbolInit_FIN_KNJIZENJE_2_ZH();
         ext_zh_vm_SymbolInit_FIN_KNJIZENJE_UTIL_ZH();
         ext_zh_vm_SymbolInit_FIN_KOMPENZACIJE_ZH();
         ext_zh_vm_SymbolInit_FIN_KONTROLA_ZBIRA_NALOGA_ZH();
         ext_zh_vm_SymbolInit_FIN_KONTROLA_ZBIRA_TABELA_ZH();
         ext_zh_vm_SymbolInit_FIN_KONTROLNI_IZVJESTAJI_ZH();
         ext_zh_vm_SymbolInit_FIN_KUPCI_PREGLED_DUGOVANJA_ZH();
         ext_zh_vm_SymbolInit_FIN_LAUNCHER_ZH();
         ext_zh_vm_SymbolInit_FIN_LEGACY_ZH();
         ext_zh_vm_SymbolInit_FIN_NALOG_CLS_ZH();
         ext_zh_vm_SymbolInit_FIN_NALOG_GEN_ZH();
         ext_zh_vm_SymbolInit_FIN_NALOG_PANAL_PSINT_ZH();
         ext_zh_vm_SymbolInit_FIN_NALOG_PSUBAN_ZH();
         ext_zh_vm_SymbolInit_FIN_NALOG_ZH();
         ext_zh_vm_SymbolInit_FIN_OSTALE_OPERACIJE_MENU_ZH();
         ext_zh_vm_SymbolInit_FIN_OTVORENE_STAVKE_ASISTENT_ZH();
         ext_zh_vm_SymbolInit_FIN_OTVORENE_STAVKE_AUTOMATSKO_ZATVARANJE_ZH();
         ext_zh_vm_SymbolInit_FIN_OTVORENE_STAVKE_GEN_KNJIZENJE_ZH();
         ext_zh_vm_SymbolInit_FIN_OTVORENE_STAVKE_RUCNO_ZATVARANJE_ZH();
         ext_zh_vm_SymbolInit_FIN_OTVORENE_STAVKE_ZH();
         ext_zh_vm_SymbolInit_FIN_PARAM_1_ZH();
         ext_zh_vm_SymbolInit_FIN_PARTNER_SALDO_ZH();
         ext_zh_vm_SymbolInit_FIN_POVRAT_DOKUMENTA_PRIPREMA_ZH();
         ext_zh_vm_SymbolInit_FIN_PREGLED_DOKUMENATA_MENU_ZH();
         ext_zh_vm_SymbolInit_FIN_PREKNJIZENJE_ZH();
         ext_zh_vm_SymbolInit_FIN_PRENOS_DOKUMENATA_MENU_ZH();
         ext_zh_vm_SymbolInit_FIN_PRENOS_FAKT_FIN_ZH();
         ext_zh_vm_SymbolInit_FIN_PRENOS_LD_FIN_ZH();
         ext_zh_vm_SymbolInit_FIN_PRENOS_POCETNO_STANJE_ZH();
         ext_zh_vm_SymbolInit_FIN_PRENOS_POS_FIN_ZH();
         ext_zh_vm_SymbolInit_FIN_PRENOS_RAZMJENA_PODATAKA_MENU_ZH();
         ext_zh_vm_SymbolInit_FIN_PROIZVOLJNI_IZVJESTAJI_ZH();
         ext_zh_vm_SymbolInit_FIN_ROCNI_INTERVALI_SPECIF_DUGOVANJA_ZH();
         ext_zh_vm_SymbolInit_FIN_ROCNI_INTERVALI_VALUTA_VAN_VALUTE_ZH();
         ext_zh_vm_SymbolInit_FIN_ROCNI_INTERVALI_ZH();
         ext_zh_vm_SymbolInit_FIN_RULES_ZH();
         ext_zh_vm_SymbolInit_FIN_SIFARNICI_MENU_ZH();
         ext_zh_vm_SymbolInit_FIN_SIFARNICI_ZH();
         ext_zh_vm_SymbolInit_FIN_SINTETICKI_NALOG_ZH();
         ext_zh_vm_SymbolInit_FIN_SINT_KARTICA_ZH();
         ext_zh_vm_SymbolInit_FIN_SINT_KART_PO_MJESECIMA_ZH();
         ext_zh_vm_SymbolInit_FIN_SPEC_3_ZH();
         ext_zh_vm_SymbolInit_FIN_SPEC_4_ZH();
         ext_zh_vm_SymbolInit_FIN_SPEC_5_ZH();
         ext_zh_vm_SymbolInit_FIN_SPEC_ANALITIKA_ZH();
         ext_zh_vm_SymbolInit_FIN_SPECIFIKACIJA_SQL_ZH();
         ext_zh_vm_SymbolInit_FIN_SPECIFIKACIJA_SUBAN_ZH();
         ext_zh_vm_SymbolInit_FIN_SPECIFIKACIJA_UTIL_ZH();
         ext_zh_vm_SymbolInit_FIN_SPECIF_OTV_STAV_PREKO_BR_DAN_ZH();
         ext_zh_vm_SymbolInit_FIN_SPECIF_OTV_ST_ZH();
         ext_zh_vm_SymbolInit_FIN_SPECIF_PARTN_KONTO_ZH();
         ext_zh_vm_SymbolInit_FIN_SPECIF_PROIZV_SORT_ZH();
         ext_zh_vm_SymbolInit_FIN_SPECIF_RAMAGLAS_REPORTS_MENU_ZH();
         ext_zh_vm_SymbolInit_FIN_SPECIF_RAMAGLAS_REPORTS_ZH();
         ext_zh_vm_SymbolInit_FIN_SPEC_PARTN_VAN_PROMETA_ZH();
         ext_zh_vm_SymbolInit_FIN_SPEC_PREBIJENO_KONTO_KONTO2_ZH();
         ext_zh_vm_SymbolInit_FIN_STAMPA_AZURIRANIH_DOKUMENATA_ZH();
         ext_zh_vm_SymbolInit_FIN_STAMPA_LISTE_NALOGA_ZH();
         ext_zh_vm_SymbolInit_FIN_SUBANALITICKA_KARTICA_DVA_KONTA_ZH();
         ext_zh_vm_SymbolInit_FIN_SUBAN_KARTICA_SQL_ZH();
         ext_zh_vm_SymbolInit_FIN_SUBAN_KARTICA_ZH();
         ext_zh_vm_SymbolInit_FIN_UDALJENA_RAZMJENA_ZH();
         ext_zh_vm_SymbolInit_FIN_UNOS_DOKUMENTA_ZH();
         ext_zh_vm_SymbolInit_FIN_UTIL_ZH();
         ext_zh_vm_SymbolInit_FISCAL_FLINK_ZH();
         ext_zh_vm_SymbolInit_FISCAL_FPRINT_ZH();
         ext_zh_vm_SymbolInit_FISCAL_HCP_ZH();
         ext_zh_vm_SymbolInit_FISCAL_MAIN_ZH();
         ext_zh_vm_SymbolInit_FISCAL_MENU_RPT_ZH();
         ext_zh_vm_SymbolInit_FISCAL_PARAMS_ZH();
         ext_zh_vm_SymbolInit_FISCAL_TREMOL_ZH();
         ext_zh_vm_SymbolInit_FISCAL_TRING_ZH();
         ext_zh_vm_SymbolInit_FISCAL_TXT_FUNCTIONS_ZH();
         ext_zh_vm_SymbolInit_FISCAL_UTILS_ZH();
         ext_zh_vm_SymbolInit_FMK_MIGRATE_ZH();
         ext_zh_vm_SymbolInit_GEN_UGOVORI_2_ZH();
         ext_zh_vm_SymbolInit_GET_LOZINKA_ZH();
         ext_zh_vm_SymbolInit_GLOBAL_VARS_0_1_2_ZH();
         ext_zh_vm_SymbolInit_GLOBAL_VARS_SPECIFIFICNOSTI_VARS_ZH();
         ext_zh_vm_SymbolInit_HASH_UTIL_ZH();
         ext_zh_vm_SymbolInit_HB_GT_UTILS_ZH();
         ext_zh_vm_SymbolInit_HOT_KEYS_ZH();
         ext_zh_vm_SymbolInit_INVALIDITET_ZH();
         ext_zh_vm_SymbolInit_JAVA_DOWNLOAD_ZH();
         ext_zh_vm_SymbolInit_JODREPORTS_DOWNLOAD_ZH();
         ext_zh_vm_SymbolInit_KALK_ATRIBUT_ZH();
         ext_zh_vm_SymbolInit_KALK_AZURIRANJE_PRIPR9_ZH();
         ext_zh_vm_SymbolInit_KALK_AZURIRANJE_ZH();
         ext_zh_vm_SymbolInit_KALK_BOX_STANJE_ZH();
         ext_zh_vm_SymbolInit_KALK_BROJACI_DOKUMENATA_ZH();
         ext_zh_vm_SymbolInit_KALK_BROWSE_DOKUMENTI_ZH();
         ext_zh_vm_SymbolInit_KALK_COPY_AZURIRAN_U_PRIPRT_ZH();
         ext_zh_vm_SymbolInit_KALK_DATVAL_ZH();
         ext_zh_vm_SymbolInit_KALK_DOKUMENT_MENU_ZH();
         ext_zh_vm_SymbolInit_KALK_FAKT_KALK_NORMATIVI_ZH();
         ext_zh_vm_SymbolInit_KALK_FIN_STANJE_MAGACIN_ZH();
         ext_zh_vm_SymbolInit_KALK_FIN_STANJE_PRODAVNICE_ZH();
         ext_zh_vm_SymbolInit_KALK_FRM_DOK_80_ZH();
         ext_zh_vm_SymbolInit_KALK_FRM_DOK_82_ZH();
         ext_zh_vm_SymbolInit_KALK_GENDOK_FAKT_ZH();
         ext_zh_vm_SymbolInit_KALK_GENDOK_MNU_ZH();
         ext_zh_vm_SymbolInit_KALK_GENDOK_SI_ZH();
         ext_zh_vm_SymbolInit_KALK_GEN_FIN_STANJE_MAGACINA_ZH();
         ext_zh_vm_SymbolInit_KALK_GEN_FIN_STANJE_PRODAVNICE_ZH();
         ext_zh_vm_SymbolInit_KALK_GET_1_11_ZH();
         ext_zh_vm_SymbolInit_KALK_GET_1_12_ZH();
         ext_zh_vm_SymbolInit_KALK_GET_1_14_ZH();
         ext_zh_vm_SymbolInit_KALK_GET_1_16_94_ZH();
         ext_zh_vm_SymbolInit_KALK_GET_1_18_ZH();
         ext_zh_vm_SymbolInit_KALK_GET_1_19_ZH();
         ext_zh_vm_SymbolInit_KALK_GET_1_2_10_ZH();
         ext_zh_vm_SymbolInit_KALK_GET_1_41_ZH();
         ext_zh_vm_SymbolInit_KALK_GET_1_95_ZH();
         ext_zh_vm_SymbolInit_KALK_GET_1_IM_ZH();
         ext_zh_vm_SymbolInit_KALK_GET_1_IP_ZH();
         ext_zh_vm_SymbolInit_KALK_GET_UTILS_ZH();
         ext_zh_vm_SymbolInit_KALK_IMPORT_CSV_ZH();
         ext_zh_vm_SymbolInit_KALK_IMP_TXT_COMMON_ZH();
         ext_zh_vm_SymbolInit_KALK_IZVJESTAJI_EXPORT_ZH();
         ext_zh_vm_SymbolInit_KALK_IZVJESTAJI_FIN_OBRT_MAGACINA_ZH();
         ext_zh_vm_SymbolInit_KALK_IZVJESTAJI_MNU_ZH();
         ext_zh_vm_SymbolInit_KALK_IZVJESTAJI_UTILS_ZH();
         ext_zh_vm_SymbolInit_KALK_IZVJESTAJ_KOLICINSKO_STANJE_OBJEKATA_ZH();
         ext_zh_vm_SymbolInit_KALK_KALKULACIJA_CIJENA_ZH();
         ext_zh_vm_SymbolInit_KALK_KARTICA_MAGACIN_2_ZH();
         ext_zh_vm_SymbolInit_KALK_KARTICA_MAGACIN_ZH();
         ext_zh_vm_SymbolInit_KALK_KARTICA_PRODAVNICA_ZH();
         ext_zh_vm_SymbolInit_KALK_KONTIRANJE_DOKUMENTA_ZH();
         ext_zh_vm_SymbolInit_KALK_KONTIRANJE_GEN_FINMAT_ZH();
         ext_zh_vm_SymbolInit_KALK_KONTIRANJE_VISE_DOKUMENATA_PERIOD_ZH();
         ext_zh_vm_SymbolInit_KALK_KOREKCIJA_NC_ZH();
         ext_zh_vm_SymbolInit_KALK_LAGER_LISTA_MAGACIN_ODT_ZH();
         ext_zh_vm_SymbolInit_KALK_LAGER_LISTA_MAGACIN_ZH();
         ext_zh_vm_SymbolInit_KALK_LAGER_LISTA_PRODAVNICA_ZH();
         ext_zh_vm_SymbolInit_KALK_LAUNCHER_ZH();
         ext_zh_vm_SymbolInit_KALK_LISTA_DOKUMENATA_ZH();
         ext_zh_vm_SymbolInit_KALK_MAGACIN_FRM_KARTICA_MAGACINA_ZH();
         ext_zh_vm_SymbolInit_KALK_MAGACIN_GENDOK_INVENTURA_MAGACIN_IM_ZH();
         ext_zh_vm_SymbolInit_KALK_MAGACIN_GENDOK_ZH();
         ext_zh_vm_SymbolInit_KALK_MAGACIN_IZVJESTAJI_MENU_ZH();
         ext_zh_vm_SymbolInit_KALK_MAGACIN_IZVJESTAJ_REAL_VELEPRODAJE_ZH();
         ext_zh_vm_SymbolInit_KALK_MAGACIN_PREGLED_ROBE_ZA_DOBAVLJACA_ZH();
         ext_zh_vm_SymbolInit_KALK_MAGACIN_TRGOVACKA_KNJIGA_NA_VELIKO_TKV_ZH();
         ext_zh_vm_SymbolInit_KALK_MAG_LAGER_LISTA_SQL_ZH();
         ext_zh_vm_SymbolInit_KALK_MNU_RAZMJENA_PODATAKA_ZH();
         ext_zh_vm_SymbolInit_KALK_PARAMS_ZH();
         ext_zh_vm_SymbolInit_KALK_PDV_ZH();
         ext_zh_vm_SymbolInit_KALK_POVRAT_DOKUMENTA_ZH();
         ext_zh_vm_SymbolInit_KALK_PREGLED_PRODAJE_ZH();
         ext_zh_vm_SymbolInit_KALK_PRENOS_FAKT_KALK_ZH();
         ext_zh_vm_SymbolInit_KALK_PRENOS_IZ_FAKT_ZH();
         ext_zh_vm_SymbolInit_KALK_PRENOS_KALK_TOPS_ZH();
         ext_zh_vm_SymbolInit_KALK_PRENOS_TOPS_KALK_ZH();
         ext_zh_vm_SymbolInit_KALK_PRODAVNICA_REALIZOVANI_POREZ_ZH();
         ext_zh_vm_SymbolInit_KALK_PRODAVNICA_UKALKULISANI_POREZ_ZH();
         ext_zh_vm_SymbolInit_KALK_PROD_GENDOK_ZH();
         ext_zh_vm_SymbolInit_KALK_PROD_IZVJESTAJI_FRM_KARTICA_ZH();
         ext_zh_vm_SymbolInit_KALK_PROD_IZVJESTAJI_MENU_ZH();
         ext_zh_vm_SymbolInit_KALK_PROD_IZVJESTAJI_REKAP_FIN_STANJA_ZH();
         ext_zh_vm_SymbolInit_KALK_PROD_IZVJESTAJI_UTILS_ZH();
         ext_zh_vm_SymbolInit_KALK_PROD_LAGER_LISTA_SQL_ZH();
         ext_zh_vm_SymbolInit_KALK_PROD_NIVELACIJA_ZH();
         ext_zh_vm_SymbolInit_KALK_PROD_RAZMJENA_PODATAKA_ZH();
         ext_zh_vm_SymbolInit_KALK_PROD_SINTETICKA_LAGER_LISTA_ZH();
         ext_zh_vm_SymbolInit_KALK_PROD_UTILS_ZH();
         ext_zh_vm_SymbolInit_KALK_PRO_FRM_PROIZVODNJA_ZH();
         ext_zh_vm_SymbolInit_KALK_PRORACUN_NABAVNE_CIJENE_GENERACIJA_USKLADJENJA_95_ZH();
         ext_zh_vm_SymbolInit_KALK_PRORACUN_NABAVNE_CIJENE_MAGACIN_ZH();
         ext_zh_vm_SymbolInit_KALK_PRORACUN_NABAVNE_CIJENE_PRODAVNICA_ZH();
         ext_zh_vm_SymbolInit_KALK_PRORACUN_NABAVNE_CIJENE_ZH();
         ext_zh_vm_SymbolInit_KALK_PRO_RPT_PROIZVODNJA_ZH();
         ext_zh_vm_SymbolInit_KALK_PRO_RPT_RADNI_NALOG_ZH();
         ext_zh_vm_SymbolInit_KALK_RASPORED_TROSKOVA_ZH();
         ext_zh_vm_SymbolInit_KALK_REKAP_FIN_STANJE_MAGACIN_ZH();
         ext_zh_vm_SymbolInit_KALK_ROBA_ZH();
         ext_zh_vm_SymbolInit_KALK_SIFRE_ZH();
         ext_zh_vm_SymbolInit_KALK_SMECE_PRIPR9_ZH();
         ext_zh_vm_SymbolInit_KALK_SPECIF_DB_ZH();
         ext_zh_vm_SymbolInit_KALK_SPECIF_PL_SIF_ZH();
         ext_zh_vm_SymbolInit_KALK_SPECIF_RPT_ALL_ZH();
         ext_zh_vm_SymbolInit_KALK_STAMPA_DOK_11_ZH();
         ext_zh_vm_SymbolInit_KALK_STAMPA_DOK_18_ZH();
         ext_zh_vm_SymbolInit_KALK_STAMPA_DOK_19_ZH();
         ext_zh_vm_SymbolInit_KALK_STAMPA_DOK_41_ZH();
         ext_zh_vm_SymbolInit_KALK_STAMPA_DOK_80_ZH();
         ext_zh_vm_SymbolInit_KALK_STAMPA_DOK_81_ZH();
         ext_zh_vm_SymbolInit_KALK_STAMPA_DOK_82_ZH();
         ext_zh_vm_SymbolInit_KALK_STAMPA_DOK_IM_ZH();
         ext_zh_vm_SymbolInit_KALK_STAMPA_DOK_IP_ZH();
         ext_zh_vm_SymbolInit_KALK_STAMPA_DOK_ZH();
         ext_zh_vm_SymbolInit_KALK_STDOK_10_TXT_ZH();
         ext_zh_vm_SymbolInit_KALK_STDOK_10_ZH();
         ext_zh_vm_SymbolInit_KALK_STDOK_14_TXT_ZH();
         ext_zh_vm_SymbolInit_KALK_STDOK_14_ZH();
         ext_zh_vm_SymbolInit_KALK_STDOK_95_ZH();
         ext_zh_vm_SymbolInit_KALK_STORNO_DOKUMENT_ZH();
         ext_zh_vm_SymbolInit_KALK_TKM_TRGOVACKA_KNJIGA_NA_MALO_ZH();
         ext_zh_vm_SymbolInit_KALK_UDALJENA_RAZMJENA_ZH();
         ext_zh_vm_SymbolInit_KALK_UNOS_DOK_16_ZH();
         ext_zh_vm_SymbolInit_KALK_UNOS_DOK_81_ZH();
         ext_zh_vm_SymbolInit_KALK_UNOS_DOK_PR_ZH();
         ext_zh_vm_SymbolInit_KALK_UNOS_DOK_RN_ZH();
         ext_zh_vm_SymbolInit_KALK_UNOS_DOKUMENTA_CHECK_ZH();
         ext_zh_vm_SymbolInit_KALK_UNOS_DOKUMENTA_MENI_F10_ZH();
         ext_zh_vm_SymbolInit_KALK_UNOS_DOKUMENTA_ZH();
         ext_zh_vm_SymbolInit_LD_AKONTACIJA_POREZA_ZH();
         ext_zh_vm_SymbolInit_LD_BENEFICIRANI_STAZ_ZH();
         ext_zh_vm_SymbolInit_LD_BRISANJE_OBRACUNA_ZH();
         ext_zh_vm_SymbolInit_LD_DATUM_ISPLATE_ZH();
         ext_zh_vm_SymbolInit__LD_DB_SQL_ZH();
         ext_zh_vm_SymbolInit_LD_DOPRINOSI_ZH();
         ext_zh_vm_SymbolInit_LD_EXPORT_BANKE_ZH();
         ext_zh_vm_SymbolInit_LD_FIONS_ZH();
         ext_zh_vm_SymbolInit_LD_ISPRAVKA_KREDITA_ZH();
         ext_zh_vm_SymbolInit_LD_IZVJESTAJI_MNU_ZH();
         ext_zh_vm_SymbolInit_LD_IZVJESTAJI_TOPLI_OBROK_ZH();
         ext_zh_vm_SymbolInit_LD_IZVJESTAJI_UTILS_ZH();
         ext_zh_vm_SymbolInit_LD_IZVJESTAJI_V2_ZH();
         ext_zh_vm_SymbolInit_LD_JS3400_OBRAZAC_ZH();
         ext_zh_vm_SymbolInit_LD_KARTICA_PLATE_AUTORSKI_HONORARI_ZH();
         ext_zh_vm_SymbolInit_LD_KARTICA_PLATE_REDOVAN_RAD_ZH();
         ext_zh_vm_SymbolInit_LD_KARTICA_PLATE_SAMOSTALNI_ZH();
         ext_zh_vm_SymbolInit_LD_KARTICA_PLATE_UGOVORI_ZH();
         ext_zh_vm_SymbolInit_LD_KARTICA_PLATE_UPRAVNI_ODBOR_ZH();
         ext_zh_vm_SymbolInit_LD_KARTICA_PLATE_VISE_MJESECI_ZH();
         ext_zh_vm_SymbolInit_LD_KARTICA_PLATE_ZH();
         ext_zh_vm_SymbolInit_LD_KREDITI_SPECIFIKACIJA_ZH();
         ext_zh_vm_SymbolInit_LD_KREDITI_ZH();
         ext_zh_vm_SymbolInit_LD_LAUNCHER_ZH();
         ext_zh_vm_SymbolInit_LD_MIP_UTILS_ZH();
         ext_zh_vm_SymbolInit_LD_MIP_ZH();
         ext_zh_vm_SymbolInit_LD_OBRACUN_MNU_ZH();
         ext_zh_vm_SymbolInit_LD_OBRACUN_UNOS_ZH();
         ext_zh_vm_SymbolInit_LD_OBRAZAC_GIP_ZH();
         ext_zh_vm_SymbolInit_LD_ODBICI_ELEMENTARNE_NEPOGODE_ZH();
         ext_zh_vm_SymbolInit_LD_PARAMETRI_ZH();
         ext_zh_vm_SymbolInit_LD_PLATNI_SPISAK_TEKUCI_RACUN_ZH();
         ext_zh_vm_SymbolInit_LD_PLATNI_SPISAK_ZH();
         ext_zh_vm_SymbolInit_LD_POREZI_ZH();
         ext_zh_vm_SymbolInit_LD_POREZ_ZH();
         ext_zh_vm_SymbolInit_LD_PREGLED_ISPLATA_TEKUCI_RACUN_ZH();
         ext_zh_vm_SymbolInit_LD_PREGLED_PLATA_ZA_VISE_MJESECI_ZH();
         ext_zh_vm_SymbolInit_LD_PREGLED_PLATA_ZH();
         ext_zh_vm_SymbolInit_LD_PREGLED_PRIMANJA_ZH();
         ext_zh_vm_SymbolInit_LD_PRIJAVA_ZH();
         ext_zh_vm_SymbolInit_LD_RADNI_SATI_ZH();
         ext_zh_vm_SymbolInit_LD_REKAPITULACIJA_UTIL_ZH();
         ext_zh_vm_SymbolInit_LD_REKAPITULACIJA_ZH();
         ext_zh_vm_SymbolInit_LD_SIFRE_MNU_ZH();
         ext_zh_vm_SymbolInit_LD_SIFRE_ZH();
         ext_zh_vm_SymbolInit_LD_SIHTARICE_IZVJESTAJI_ZH();
         ext_zh_vm_SymbolInit_LD_SIHTARICE_SIFRE_ZH();
         ext_zh_vm_SymbolInit_LD_SIHTARICE_UNOS_ZH();
         ext_zh_vm_SymbolInit_LD_SIHTARICE_ZH();
         ext_zh_vm_SymbolInit_LD_SPECIFIKACIJA_NETO_PRIMANJA_PO_OPCINAMA_ZH();
         ext_zh_vm_SymbolInit_LD_SPECIFIKACIJA_PO_RASPONIMA_ZH();
         ext_zh_vm_SymbolInit_LD_SPECIFIKACIJA_PO_RJ_ZH();
         ext_zh_vm_SymbolInit_LD_SPECIFIKACIJA_SAMOSTALNI_O_2002_ZH();
         ext_zh_vm_SymbolInit_LD_SPECIFIKACIJA_UGOVORI_ZH();
         ext_zh_vm_SymbolInit_LD_SPECIFIKACIJA_UTIL_ZH();
         ext_zh_vm_SymbolInit_LD_SPECIFIKACIJA_UZ_ISPLATU_PLATA_2001_STARI_ZH();
         ext_zh_vm_SymbolInit_LD_SPECIFIKACIJA_UZ_ISPLATU_PLATE_OBRAZAC_2001_ZH();
         ext_zh_vm_SymbolInit_LD_UTILS_ZH();
         ext_zh_vm_SymbolInit__LEGACY_OCITAJ_ZH();
         ext_zh_vm_SymbolInit_LO_DOWNLOAD_ZH();
         ext_zh_vm_SymbolInit_LOKAL_DB_ZH();
         ext_zh_vm_SymbolInit_MATCH_CODE_ZH();
         ext_zh_vm_SymbolInit_MY_BROWSE_ZH();
         ext_zh_vm_SymbolInit_MY_SQL_SERVER_ZH();
         ext_zh_vm_SymbolInit_MY_USE_ZH();
         ext_zh_vm_SymbolInit_NOVA_STRANA_ZH();
         ext_zh_vm_SymbolInit_NUMBER_UTIL_ZH();
         ext_zh_vm_SymbolInit_O_DBF_ZH();
         ext_zh_vm_SymbolInit__O_FAKT_ZH();
         ext_zh_vm_SymbolInit__O_FIN_SQL_ZH();
         ext_zh_vm_SymbolInit__O_FIN_ZH();
         ext_zh_vm_SymbolInit__O_KALK_SQL_ZH();
         ext_zh_vm_SymbolInit__O_KALK_ZH();
         ext_zh_vm_SymbolInit__O_LD_SIHTARICE_ZH();
         ext_zh_vm_SymbolInit__O_LD_ZH();
         ext_zh_vm_SymbolInit__O_POS_DBF_ZH();
         ext_zh_vm_SymbolInit_OS_DATABASE_UTILS_ZH();
         ext_zh_vm_SymbolInit__O_SIF_ZH();
         ext_zh_vm_SymbolInit_OS_IZVJ_AMORT_PO_KONTIMA_ZH();
         ext_zh_vm_SymbolInit_OS_IZVJ_AMORT_PO_STOPAMA_ZH();
         ext_zh_vm_SymbolInit_OS_IZVJ_KARTICA_ZH();
         ext_zh_vm_SymbolInit_OS_IZVJ_MENU_ZH();
         ext_zh_vm_SymbolInit_OS_IZVJ_PREGLED_AMORTIZACIJE_ZH();
         ext_zh_vm_SymbolInit_OS_IZVJ_PREGLED_PO_KONTIMA_ZH();
         ext_zh_vm_SymbolInit_OS_IZVJ_PREGLED_PO_RJ_ZH();
         ext_zh_vm_SymbolInit_OS_IZVJ_PREGLED_REVALORIZACIJE_ZH();
         ext_zh_vm_SymbolInit_OS_LAUNCHER_ZH();
         ext_zh_vm_SymbolInit_OS_OBRACUN_ZH();
         ext_zh_vm_SymbolInit_OS_PARAMETRI_ZH();
         ext_zh_vm_SymbolInit_OS_POCETNO_STANJE_ZH();
         ext_zh_vm_SymbolInit_OS_POPISNA_LISTA_ZH();
         ext_zh_vm_SymbolInit_OS_REKAP_PO_K1_ZH();
         ext_zh_vm_SymbolInit_OS_SIFRARNICI_ZH();
         ext_zh_vm_SymbolInit__OS_SQL_ZH();
         ext_zh_vm_SymbolInit_OS_UNOS_SREDSTAVA_ZH();
         ext_zh_vm_SymbolInit_OS_UTIL_ZH();
         ext_zh_vm_SymbolInit_OTPREMNICA_MP_ZH();
         ext_zh_vm_SymbolInit__O_VIRM_DBF_ZH();
         ext_zh_vm_SymbolInit_PARAMS_ORGANIZACIJA_ZH();
         ext_zh_vm_SymbolInit_PARSIRAJ_SQL_ZH();
         ext_zh_vm_SymbolInit_PARTNER_PDV_IDBROJ_ZH();
         ext_zh_vm_SymbolInit_PDF_CLS_ZH();
         ext_zh_vm_SymbolInit_PDF_DOWNLOAD_ZH();
         ext_zh_vm_SymbolInit_PDV_ZH();
         ext_zh_vm_SymbolInit_PIC_ZH();
         ext_zh_vm_SymbolInit_POREZNA_FAKTURA_A4_2_ZH();
         ext_zh_vm_SymbolInit_POREZNA_FAKTURA_A4_ZH();
         ext_zh_vm_SymbolInit_POREZNA_FAKTURA_TRAKA_ZH();
         ext_zh_vm_SymbolInit_POS_AZURIRANJE_OSTALI_DOKUMENTI_ZH();
         ext_zh_vm_SymbolInit_POS_AZURIRANJE_RACUNA_ZH();
         ext_zh_vm_SymbolInit_POS_BROJAC_DOKUMENATA_ZH();
         ext_zh_vm_SymbolInit_POS_DATABASE_TEMP_ZH();
         ext_zh_vm_SymbolInit__POS_DBF_ZH();
         ext_zh_vm_SymbolInit_POS_DISPLAY_BIG_BROJEVE_ZH();
         ext_zh_vm_SymbolInit_POS_FISKALNI_RACUN_ZH();
         ext_zh_vm_SymbolInit_POS_FRM_INVENTURA_NIVELACIJA_ZH();
         ext_zh_vm_SymbolInit_POS_FRM_RACUN_MENU_ZH();
         ext_zh_vm_SymbolInit_POS_IZVJ_INVENTURA_NIVELACIJA_ZH();
         ext_zh_vm_SymbolInit_POS_IZVJ_KARTICA_ZH();
         ext_zh_vm_SymbolInit_POS_IZVJ_MENU_ZH();
         ext_zh_vm_SymbolInit_POS_IZVJ_PDV_POREZI_ZH();
         ext_zh_vm_SymbolInit_POS_IZVJ_POCETNO_STANJE_ZH();
         ext_zh_vm_SymbolInit_POS_IZVJ_POREZ_PO_TARIFAMA_ZH();
         ext_zh_vm_SymbolInit_POS_IZVJ_PREGLED_KUMULATIVA_ZH();
         ext_zh_vm_SymbolInit_POS_IZVJ_RACUN_ZH();
         ext_zh_vm_SymbolInit_POS_IZVJ_REALIZACIJA_KASE_ZH();
         ext_zh_vm_SymbolInit_POS_IZVJ_REALIZACIJA_RADNIK_ZH();
         ext_zh_vm_SymbolInit_POS_IZVJ_STAMPA_DOKUMENATA_ZH();
         ext_zh_vm_SymbolInit_POS_IZVJ_STANJE_PARTNERA_ZH();
         ext_zh_vm_SymbolInit_POS_IZVJ_STANJE_PM_ZH();
         ext_zh_vm_SymbolInit_POS_IZVJ_STANJE_ZH();
         ext_zh_vm_SymbolInit_POS_IZVJ_TARIFE_ZH();
         ext_zh_vm_SymbolInit_POS_IZVJ_TOP_PRODAJA_ZH();
         ext_zh_vm_SymbolInit_POS_IZVJ_UTILS_ZH();
         ext_zh_vm_SymbolInit_POS_IZVJ_VRSTE_PLACANJA_ZH();
         ext_zh_vm_SymbolInit_POS_IZVJ_ZADUZENJE_ZH();
         ext_zh_vm_SymbolInit_POS_LAUNCHER_ZH();
         ext_zh_vm_SymbolInit_POS_LISTA_RACUNA_ZH();
         ext_zh_vm_SymbolInit_POS_MNU_ADMINISTRATOR_ZH();
         ext_zh_vm_SymbolInit_POS_MNU_PRODAVAC_ZH();
         ext_zh_vm_SymbolInit_POS_MNU_UPRAVNIK_ZH();
         ext_zh_vm_SymbolInit_POS_PARAMETRI_ZH();
         ext_zh_vm_SymbolInit_POS_POCETNO_STANJE_ZH();
         ext_zh_vm_SymbolInit_POS_POVRAT_DOKUMENTA_ZH();
         ext_zh_vm_SymbolInit_POS_PREGLED_DOKUMENATA_TABELA_ZH();
         ext_zh_vm_SymbolInit_POS_PREGLED_RACUNA_TABELA_ZH();
         ext_zh_vm_SymbolInit_POS_PRENOS_POS_FAKT_ZH();
         ext_zh_vm_SymbolInit_POS_PRENOS_POS_KALK_ZH();
         ext_zh_vm_SymbolInit_POS_PRIJAVA_ZH();
         ext_zh_vm_SymbolInit_POS_PRIVILEGIJE_ZH();
         ext_zh_vm_SymbolInit_POS_RABATI_ZH();
         ext_zh_vm_SymbolInit_POS_RACUN_UNOS_ZH();
         ext_zh_vm_SymbolInit_POS_REALIZACIJA_MENU_ZH();
         ext_zh_vm_SymbolInit_POS_ROBA_ZH();
         ext_zh_vm_SymbolInit_POS_ROBMAT_MENU_ZH();
         ext_zh_vm_SymbolInit_POS_SIFARNIK_ZH();
         ext_zh_vm_SymbolInit_POS_SIFRARNIK_MENU_ZH();
         ext_zh_vm_SymbolInit_POS_SMJENE_ZH();
         ext_zh_vm_SymbolInit__POS_SQL_ZH();
         ext_zh_vm_SymbolInit_POS_STORNO_RACUNA_ZH();
         ext_zh_vm_SymbolInit_POSTGRESQL_UTIL_ZH();
         ext_zh_vm_SymbolInit_POS_ZADUZENJE_UNOS_ZH();
         ext_zh_vm_SymbolInit_PREGLED_ASORTIMANA_DOBAVLJAC_ZH();
         ext_zh_vm_SymbolInit_PRINT_0_ZH();
         ext_zh_vm_SymbolInit_PRINT_2_ZH();
         ext_zh_vm_SymbolInit_PRINT_LISTA_2_ZH();
         ext_zh_vm_SymbolInit_PRINT_LISTA_ZH();
         ext_zh_vm_SymbolInit_PRINT_ODT_ZH();
         ext_zh_vm_SymbolInit_P_SIFK_ZH();
         ext_zh_vm_SymbolInit_PSQL_DOWNLOAD_ZH();
         ext_zh_vm_SymbolInit_QOUTU_ZH();
         ext_zh_vm_SymbolInit_RACUN_UTILS_ZH();
         ext_zh_vm_SymbolInit_RB_TRAKA_ZH();
         ext_zh_vm_SymbolInit_REPORT_COMMON_ZH();
         ext_zh_vm_SymbolInit_ROBA_MENI_ZH();
         ext_zh_vm_SymbolInit_ROBA_NALJEPNICE_ZH();
         ext_zh_vm_SymbolInit__ROBA_SQL_ZH();
         ext_zh_vm_SymbolInit_ROBA_SVEDI_NA_STANDARNU_JMJ_ZH();
         ext_zh_vm_SymbolInit_RPT_NARUDZBENICA_ZH();
         ext_zh_vm_SymbolInit_RULES_M_RULES_ZH();
         ext_zh_vm_SymbolInit_RULES_RULES_ZH();
         ext_zh_vm_SymbolInit_SASTAVNICE_ADMIN_ZH();
         ext_zh_vm_SymbolInit_SASTAVNICE_IZVJ_ZH();
         ext_zh_vm_SymbolInit_SASTAVNICE_PRINT_ZH();
         ext_zh_vm_SymbolInit_SASTAVNICE_UTIL_ZH();
         ext_zh_vm_SymbolInit_SASTAVNICE_ZH();
         ext_zh_vm_SymbolInit_SAY_UTILS_ZH();
         ext_zh_vm_SymbolInit_SEMAPHORES_FULL_ALGORITAM_ZH();
         ext_zh_vm_SymbolInit_SEMAPHORES_IDS_ALGORITAM_ZH();
         ext_zh_vm_SymbolInit_SEMAPHORES_LOCK_TABLES_ZH();
         ext_zh_vm_SymbolInit_SEMAPHORES_ZH();
         ext_zh_vm_SymbolInit_SERVER_DB_ZH();
         ext_zh_vm_SymbolInit_SERVER_INFO_ZH();
         ext_zh_vm_SymbolInit_SERVER_LOG_ZH();
         ext_zh_vm_SymbolInit__SET_A_DBF_FAKT_ZH();
         ext_zh_vm_SymbolInit__SET_A_DBF_FIN_ZH();
         ext_zh_vm_SymbolInit_SET_A_DBF_KALK_ZH();
         ext_zh_vm_SymbolInit__SET_A_DBF_LD_ZH();
         ext_zh_vm_SymbolInit__SET_A_DBF_OS_ZH();
         ext_zh_vm_SymbolInit_SET_A_DBF_PARAMS_ZH();
         ext_zh_vm_SymbolInit__SET_A_DBF_POS_ZH();
         ext_zh_vm_SymbolInit_SET_A_DBF_SIFK_SIFV_ZH();
         ext_zh_vm_SymbolInit__SET_A_DBF_SIF_ZH();
         ext_zh_vm_SymbolInit_SET_A_DBF_TEMPORARY_ZH();
         ext_zh_vm_SymbolInit_SET_A_DBF_VIRM_ZH();
         ext_zh_vm_SymbolInit_SHA256SUM_ZH();
         ext_zh_vm_SymbolInit_SIF_K1_VRSTEP_ZH();
         ext_zh_vm_SymbolInit_SIF_KONTA_TIPOVI_CIJENA_ZH();
         ext_zh_vm_SymbolInit_SIF_KONTA_ZH();
         ext_zh_vm_SymbolInit_SIFK_SIFV_ZH();
         ext_zh_vm_SymbolInit_SIF_OPS_ZH();
         ext_zh_vm_SymbolInit_SIF_RADNE_JEDINICE_ZH();
         ext_zh_vm_SymbolInit_SIF_REFERENTI_ZH();
         ext_zh_vm_SymbolInit_SIF_ROBA_ZH();
         ext_zh_vm_SymbolInit_SIF_TARIFA_ZH();
         ext_zh_vm_SymbolInit_SIF_TDOK_ZH();
         ext_zh_vm_SymbolInit_SIF_TNAL_ZH();
         ext_zh_vm_SymbolInit_SIF_TRFP_ZH();
         ext_zh_vm_SymbolInit_SIF_UGOV_ZH();
         ext_zh_vm_SymbolInit_SIF_UTIL_ZH();
         ext_zh_vm_SymbolInit_SIF_VALUTE_ZH();
         ext_zh_vm_SymbolInit_SQL_NIL_ZH();
         ext_zh_vm_SymbolInit_SQL_QUERY_ZH();
         ext_zh_vm_SymbolInit_SQL_RDD_ZH();
         ext_zh_vm_SymbolInit_SQL_TABLE_UPDATE_ZH();
         ext_zh_vm_SymbolInit_SQL_UTILS_ZH();
         ext_zh_vm_SymbolInit_STAMPA_VIRMANA_DELPHIRB_ZH();
         ext_zh_vm_SymbolInit_STANJE_NABAVNA_CIJENA_ZH();
         ext_zh_vm_SymbolInit_ST_KALK_DOK_PR_ZH();
         ext_zh_vm_SymbolInit_STRING_UTIL_ZH();
         ext_zh_vm_SymbolInit_TAKSA_GORIVO_ZH();
         ext_zh_vm_SymbolInit_T_APP_MOD_ZH();
         ext_zh_vm_SymbolInit_T_FAKT_MOD_ZH();
         ext_zh_vm_SymbolInit_T_FILE_READ_ZH();
         ext_zh_vm_SymbolInit_T_FIN_MOD_ZH();
         ext_zh_vm_SymbolInit_THREAD_CREATE_DBFS_ZH();
         ext_zh_vm_SymbolInit_T_KALK_MOD_ZH();
         ext_zh_vm_SymbolInit_T_LD_MOD_ZH();
         ext_zh_vm_SymbolInit_T_OS_MOD_ZH();
         ext_zh_vm_SymbolInit_T_POS_MOD_ZH();
         ext_zh_vm_SymbolInit_T_VIRM_MOD_ZH();
         ext_zh_vm_SymbolInit_UDALJENA_RAZMJENA_UTILS_ZH();
         ext_zh_vm_SymbolInit_UGOVORI_PARAMETRI_ZH();
         ext_zh_vm_SymbolInit_UGOVORI_UTILS_ZH();
         ext_zh_vm_SymbolInit_UGOV_ROBA_ZH();
         ext_zh_vm_SymbolInit__UGOV_SQL_ZH();
         ext_zh_vm_SymbolInit_UI2_WINDOWS_ZH();
         ext_zh_vm_SymbolInit_UI_BROWSEKEY_ZH();
         ext_zh_vm_SymbolInit_UI_EVENT_ZH();
         ext_zh_vm_SymbolInit_UI_MAIN_SCREEN_ZH();
         ext_zh_vm_SymbolInit_UI_MAIN_ZH();
         ext_zh_vm_SymbolInit_UI_MENU_ZH();
         ext_zh_vm_SymbolInit_UI_MOUSE_ZH();
         ext_zh_vm_SymbolInit_UI_MSGS_ZH();
         ext_zh_vm_SymbolInit_UI_NASLOVNI_EKRAN_SPLASH_ZH();
         ext_zh_vm_SymbolInit_UI_NASLOVNI_EKRAN_ZH();
         ext_zh_vm_SymbolInit_UI_SCGET_ZH();
         ext_zh_vm_SymbolInit_UI_STANDARD_DIALOGS_ZH();
         ext_zh_vm_SymbolInit_UNICODE_ZH();
         ext_zh_vm_SymbolInit_UPDATE_DBF_FROM_SERVER_ZH();
         ext_zh_vm_SymbolInit_USER_UTIL_ZH();
         ext_zh_vm_SymbolInit_USE_SQL_ZH();
         ext_zh_vm_SymbolInit_VERSION_UTIL_ZH();
         ext_zh_vm_SymbolInit_VIRM_EXPORT_BANKE_ZH();
         ext_zh_vm_SymbolInit_VIRM_LAUNCHER_ZH();
         ext_zh_vm_SymbolInit_VIRM_PARAMETRI_ZH();
         ext_zh_vm_SymbolInit_VIRM_RAZMJENA_LD_ZH();
         ext_zh_vm_SymbolInit_VIRM_RAZMJENA_MENU_ZH();
         ext_zh_vm_SymbolInit_VIRM_SIFRARNIK_MENU_ZH();
         ext_zh_vm_SymbolInit_VIRM_SIFRARNIK_ZH();
         ext_zh_vm_SymbolInit_VIRM_UNOS_PODATAKA_ZH();
         ext_zh_vm_SymbolInit_VRIJEME_SQL_ZH();
         ext_zh_vm_SymbolInit_XBASE_LEGACY_ZH();
         ext_zh_vm_SymbolInit_XBASE_PARSIRAJ_ZH();
         ext_zh_vm_SymbolInit_XBASE_RDD_DBF_ZH();
         ext_zh_vm_SymbolInit_XBASE_RDD_ZH();
         ext_zh_vm_SymbolInit_YARG_DOWNLOAD_ZH();
         ext_zh_vm_SymbolInit_YARG_ZH();

         ext_zh_vm_SymbolInit_ACHOICE_ZH();
         ext_zh_vm_SymbolInit_ADIR_ZH();
         ext_zh_vm_SymbolInit_ALERT_FUNC_ZH();
         ext_zh_vm_SymbolInit_ALTD_ZH();
         ext_zh_vm_SymbolInit_ARRAY_RDD_ZH();
         ext_zh_vm_SymbolInit_BASE64U_ZH();
         ext_zh_vm_SymbolInit_BROWSE_FUNC_ZH();
         ext_zh_vm_SymbolInit_CGI_ZH();
         ext_zh_vm_SymbolInit_CLIENT_ZH();
         ext_zh_vm_SymbolInit_CLS_CHECKBOX_ZH();
         ext_zh_vm_SymbolInit_CLS_GET_HELPER_FUNC_ZH();
         ext_zh_vm_SymbolInit_CLS_GET_ZH();
         ext_zh_vm_SymbolInit_CLS_LISTBOX_ZH();
         ext_zh_vm_SymbolInit_CLS_MENUITEM_ZH();
         ext_zh_vm_SymbolInit_CLS_POPUPMENU_ZH();
         ext_zh_vm_SymbolInit_CLS_PUSH_BUTTON_ZH();
         ext_zh_vm_SymbolInit_CLS_RADIO_BUTTON_ZH();
         ext_zh_vm_SymbolInit_CLS_RADIO_GRP_ZH();
         ext_zh_vm_SymbolInit_CLS_SCROLL_BAR_ZH();
         ext_zh_vm_SymbolInit_CLS_TBCOLUMN_ZH();
         ext_zh_vm_SymbolInit_CLS_TBROWSE_ZH();
         ext_zh_vm_SymbolInit_CLS_TOPBARMENU_ZH();
         ext_zh_vm_SymbolInit_CLS_TPQSERVER_ZH();
         ext_zh_vm_SymbolInit_CLS_ZHEDITOR_ZH();
         ext_zh_vm_SymbolInit_CLS_ZHGETLIST_ZH();
         ext_zh_vm_SymbolInit_CLS_ZH_GET_ZH();
         ext_zh_vm_SymbolInit_CLS_ZHMEMOEDITOR_ZH();
         ext_zh_vm_SymbolInit_CLS_ZHMENUSYS_ZH();
         ext_zh_vm_SymbolInit_CLS_ZH_POPUPMENU_ZH();
         ext_zh_vm_SymbolInit_CLS_ZHTEXTLINE_ZH();
         ext_zh_vm_SymbolInit_CODEPAGE_TERM_ZH();
         ext_zh_vm_SymbolInit_COLORS_FOR_MENU_SYSTEM_ZH();
         ext_zh_vm_SymbolInit_CTDUMMY_ZH();
         ext_zh_vm_SymbolInit_CTMISC_ZH();
         ext_zh_vm_SymbolInit_CTRAND_ZH();
         ext_zh_vm_SymbolInit_CTTIME_ZH();
         ext_zh_vm_SymbolInit___DBCOPYSTRUCT_ZH();
         ext_zh_vm_SymbolInit_DBEDIT_FUNC_ZH();
         ext_zh_vm_SymbolInit_DBGBRWSR_ZH();
         ext_zh_vm_SymbolInit_DBGHELP_ZH();
         ext_zh_vm_SymbolInit_DBGMENU_ZH();
         ext_zh_vm_SymbolInit_DBGTARR_ZH();
         ext_zh_vm_SymbolInit_DBGTHSH_ZH();
         ext_zh_vm_SymbolInit_DBGTINP_ZH();
         ext_zh_vm_SymbolInit_DBGTMENU_ZH();
         ext_zh_vm_SymbolInit_DBGTMITM_ZH();
         ext_zh_vm_SymbolInit_DBGTOBJ_ZH();
         ext_zh_vm_SymbolInit_DBGTWIN_ZH();
         ext_zh_vm_SymbolInit_DBGWA_ZH();
         ext_zh_vm_SymbolInit___DBJOIN_ZH();
         ext_zh_vm_SymbolInit___DBLIST_ZH();
         ext_zh_vm_SymbolInit___DBSORT_ZH();
         ext_zh_vm_SymbolInit___DBTOTAL_ZH();
         ext_zh_vm_SymbolInit___DBUPDATE_ZH();
         ext_zh_vm_SymbolInit_DEBUGGER_ZH();
         ext_zh_vm_SymbolInit_DEVOUTPICT_ZH();
         ext_zh_vm_SymbolInit_DIR_SCAN_ZH();
         ext_zh_vm_SymbolInit_DIR_ZH();
         ext_zh_vm_SymbolInit_EINSTVAR_ZH();
         ext_zh_vm_SymbolInit_ENCB64_ZH();
         ext_zh_vm_SymbolInit_ENCODER_ZH();
         ext_zh_vm_SymbolInit_ENCQP_ZH();
         ext_zh_vm_SymbolInit_ENCURL_ZH();
         ext_zh_vm_SymbolInit_ERROR_HANDLER_ZH();
         ext_zh_vm_SymbolInit_ERRSTR_ZH();
         ext_zh_vm_SymbolInit_F18_UTILS_ZH();
         ext_zh_vm_SymbolInit_FCOPY_ZH();
         ext_zh_vm_SymbolInit_GETINFO_ZH();
         ext_zh_vm_SymbolInit_GETINPUT_ZH();
         ext_zh_vm_SymbolInit_GETLIST_ZH();
         ext_zh_vm_SymbolInit_GETSECRT_ZH();
         ext_zh_vm_SymbolInit_GET_SYSTEM_GUI_READER_ZH();
         ext_zh_vm_SymbolInit_GET_SYSTEM_ZH();
         ext_zh_vm_SymbolInit_GET_SYSTEM_ZH_ZH();
         ext_zh_vm_SymbolInit_GUI_HELPER_ZH();
         ext_zh_vm_SymbolInit_INPUT_ZH();
         ext_zh_vm_SymbolInit_KEYSAVE_ZH();
         ext_zh_vm_SymbolInit_KEYSEC_ZH();
         ext_zh_vm_SymbolInit_KEYTIME_ZH();
         ext_zh_vm_SymbolInit_LANG_COMP_ZH();
         ext_zh_vm_SymbolInit_LIBNAME_FUNC_ZH();
         ext_zh_vm_SymbolInit_LOG_ZH();
         ext_zh_vm_SymbolInit_MAILASSY_ZH();
         ext_zh_vm_SymbolInit_MAILSEND_ZH();
         ext_zh_vm_SymbolInit_MAIL_ZH();
         ext_zh_vm_SymbolInit_MEMVAR_SAVE_RESTORE_ZH();
         ext_zh_vm_SymbolInit_MENU_SYS_ZH();
         ext_zh_vm_SymbolInit_MENU_TO_ZH();
         ext_zh_vm_SymbolInit_MINIZIP_ERROR_ZH();
         ext_zh_vm_SymbolInit_MISC_ZH();
         ext_zh_vm_SymbolInit_NOOP_ZH();
         ext_zh_vm_SymbolInit_OBJECT_LOWLEVEL_FUNC_ZH();
         ext_zh_vm_SymbolInit_PDF_CLASS_ZH();
         ext_zh_vm_SymbolInit_PDF_DOWNLOAD_ZH();
         ext_zh_vm_SymbolInit_PROFILER_ZH();
         ext_zh_vm_SymbolInit_RADIO_BUTTON_ZH_ZH();
         ext_zh_vm_SymbolInit_RDD_INIT_ZH();
         ext_zh_vm_SymbolInit_RDD_ORD_ZH();
         ext_zh_vm_SymbolInit_READKEY_ZH();
         ext_zh_vm_SymbolInit_READVAR_ZH();
         ext_zh_vm_SymbolInit_SAVESCREEN_HELPER_FUNC_ZH();
         ext_zh_vm_SymbolInit_SCREEN3_ZH();
         ext_zh_vm_SymbolInit_SCRMARK_ZH();
         ext_zh_vm_SymbolInit_SESSID_ZH();
         ext_zh_vm_SymbolInit_SET_FUNC_ZH();
         ext_zh_vm_SymbolInit_SET_TYPEAHEAD_ZH();
         ext_zh_vm_SymbolInit_SHOWTIME_ZH();
         ext_zh_vm_SymbolInit_SMTPCLI_ZH();
         ext_zh_vm_SymbolInit_TBROWSEDB_FUNC_ZH();
         ext_zh_vm_SymbolInit_TBRWTEXT_ZH();
         ext_zh_vm_SymbolInit_TEMPFILE_ZH();
         ext_zh_vm_SymbolInit_T_PERSIST_ZH();
         ext_zh_vm_SymbolInit_T_SCALAR_ZH();
         ext_zh_vm_SymbolInit_T_SYMBOL_ZH();
         ext_zh_vm_SymbolInit_URL_ZH();
         ext_zh_vm_SymbolInit_VAL_TO_EXP_ZH();
         ext_zh_vm_SymbolInit_WAIT_ZH();
         ext_zh_vm_SymbolInit_ZHCLASS_FUNC_ZH();
         ext_zh_vm_SymbolInit_ZH_DELIM_ZH();
         ext_zh_vm_SymbolInit_ZH_I18N2_ZH();
         ext_zh_vm_SymbolInit_ZH_INI_FILE_ZH();
         ext_zh_vm_SymbolInit_ZH_INIT_ZH();
         ext_zh_vm_SymbolInit_ZH_INI_ZH();
         ext_zh_vm_SymbolInit_ZHOBJECT_FUNC_ZH();
         ext_zh_vm_SymbolInit_ZH_OTHERS_ZH();
   } 
   
   
      zh_clsDoInit();          


      printf("init step 18\n");
      


                 /* initialize Class(y) .zh functions */
      
   

   //printf("init step 18\n");
   //zh_vmDoInitFunctions();    /* process registered INIT procedures */

   


   /* if there's a function called _APPMAIN() it will be executed first. [vszakats] */
   {
      PZH_DYNSYMBOL pDynSym = zh_dynsymFind( "_APPMAIN" );

      if( pDynSym && pDynSym->pSymbol->value.pFunPtr )
         s_pSymStart = pDynSym->pSymbol;
      else
      {
         /* if first char is '@' then start procedure were set by
          * programmer explicitly and should have the highest priority
          * otherwise it's the name of first public function in
          * first linked module which is used if there is no
          * ZH_START_PROCEDURE in code
          */
         const char * pszMain;

         if( s_vm_pszLinkedMain && *s_vm_pszLinkedMain == '@' )
         {
            pszMain = s_vm_pszLinkedMain + 1;
            pDynSym = zh_dynsymFind( pszMain );
         }
         else
         {
            //puts("ZH_START_PROCEDURE = \"MAIN\"");
            pszMain = ZH_START_PROCEDURE;
            pDynSym = zh_dynsymFind( pszMain );
            //printf("init step 20\n");
            if( ! ( pDynSym && pDynSym->pSymbol->value.pFunPtr ) )
            {
               if( s_vm_pszLinkedMain )
               {
                  //printf("init step 21\n");
                  pszMain = s_vm_pszLinkedMain;
                  pDynSym = zh_dynsymFind( pszMain );
               }
            }
         }

         if( pDynSym && pDynSym->pSymbol->value.pFunPtr )
            s_pSymStart = pDynSym->pSymbol;

         if( bStartMainProc && ! s_pSymStart )
         {
            if( pszMain )
               zh_errInternal( ZH_EI_VMBADSTARTUP, NULL, pszMain, NULL );
            else
               zh_errInternal( ZH_EI_VMNOSTARTUP, NULL, NULL, NULL );
         }
      }
   }

   if( bStartMainProc && s_pSymStart )
   {
      //printf("init step 22\n");
      zh_vmPushSymbol( s_pSymStart ); /* pushes first ZH_FS_PUBLIC defined symbol to the stack */
      //printf("init step 23\n");
      zh_vmPushNil();                 /* places NIL at self */
      //printf("init step 24\n");
      if (bInitRT) {
         //printf("init step 25\n");
          zh_vmProc( ( ZH_USHORT ) zh_cmdargPushArgs() ); /* invoke it with number of supplied parameters */
      } else {
         //printf("init step 26\n");
         zh_vmProc( 0 );
      }

   } else {
      //printf("init step 27\n");
   }

  if (gtcount() < 1)
      printf("=====>========GT_STD REGISTER: %d\n", register_GT_STD());  
   else
     printf("=====>========gtcount: %d===========================\n", gtcount());

   printf("=============zh_vmInit kraj=====================\n");
      
}


/*
void zh_initDynTable( void )
{
   static const char * s_pszFuncNames[] =
      { "ERRORBLOCK", "ERRORSYS", "__ZHVMINIT", "DEFERROR", "EMPTY", "ZH_STRFORMAT", 
        "NETERR", "ERRORMESSAGE", "BREAK" };

   ZH_STACK_TLS_PRELOAD
   int i;

   for( i = 0; i < ( int ) ZH_SIZEOFARRAY( s_pszFuncNames ); ++i )
   {
      PZH_DYNSYMBOL pFuncSym = zh_dynsymFindName( s_pszFuncNames[i] );

      if ( ! pFuncSym ) {
         pFuncSym = zh_dynsymGetCase( s_pszFuncNames[i] ); 
         printf("find %s: %d  %d\n", s_pszFuncNames[i], i, pFuncSym);
      }

   }
}
*/

ZH_EXPORT int zh_vmQuit( ZH_BOOL bInitRT )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmQuit()" ) );

   zh_vmTerminateThreads();

   zh_vmDoExitFunctions();          /* process defined EXIT functions */
   zh_vmDoModuleExitFunctions();    /* process AtExit registered functions */

   /* release all known items stored in subsystems */
   zh_itemClear( zh_stackReturnItem() );
   zh_stackRemove( 1 );          /* clear stack items, leave only initial symbol item */

   /* intentionally here to allow executing object destructors for all
    * cross referenced items before we release classy subsystem
    */
   zh_gcCollectAll( ZH_TRUE );

   /* Clear any pending actions so RDD shutdown process
    * can be cleanly executed
    */
   zh_stackSetActionRequest( 0 );

   zh_rddCloseAll();             /* close all workareas */
   if (bInitRT)
      zh_rddShutDown();             /* remove all registered RDD drivers */

   zh_memvarsClear( ZH_TRUE );   /* clear all PUBLIC (and PRIVATE if any) variables */
   zh_vmSetI18N( NULL );         /* remove i18n translation table */
   zh_i18n_exit();               /* unregister i18n module */

   zh_itemClear( zh_stackReturnItem() );
   zh_gcCollectAll( ZH_TRUE );
   /* deactivate debugger */
   zh_vmDebuggerExit( ZH_TRUE );

   /* stop executing PCODE (ZHVM reenter request) */
   s_fZHVMActive = ZH_FALSE;

   zh_vmStaticsClear();

   /* release thread specific data */
   zh_stackDestroyTSD();

   if (bInitRT) {
     zh_breakBlockRelease();
     zh_errExit();
     zh_clsReleaseAll();
   }

   zh_vmStaticsRelease();

   /* release all remaining items */

   zh_conRelease();                 /* releases Console */
   zh_vmReleaseLocalSymbols();      /* releases the local modules linked list */
   
   if (bInitRT) {
      zh_dynsymRelease();   /* releases the dynamic symbol table */
   }

   zh_itemClear( zh_stackReturnItem() );
   zh_gcCollectAll( ZH_TRUE );

   zh_vmDoModuleQuitFunctions();    /* process AtQuit registered functions */
   zh_vmCleanModuleFunctions();

   zh_vmStackRelease();             /* release ZHVM stack and remove it from linked ZHVM stacks list */
   if( s_pSymbolsMtx )
   {
      zh_itemRelease( s_pSymbolsMtx );
      s_pSymbolsMtx = NULL;
   }
   zh_threadExit();

   if (bInitRT) {
     zh_langReleaseAll();             /* release lang modules */
     zh_cdpReleaseAll();              /* releases codepages */
   }

   /* release all known garbage */
   if( zh_xquery( ZH_MEM_STATISTICS ) == 0 ) /* check if fmstat is ON */
      zh_gcReleaseAll();

   zh_vmUnsetExceptionHandler();
   zh_xexit();

#if defined( ZH_OS_WIN )
   zh_winmainArgVFree();
#endif

   return s_nErrorLevel;
}

void zh_vmExecute( const ZH_BYTE * pCode, PZH_SYMBOL pSymbols )
{
   
   ZH_BOOL bCanRecover = ZH_FALSE;
   ZH_BOOL bDynCode = pSymbols == NULL || ( pSymbols->scope.value & ZH_FS_DYNCODE ) != 0;

#ifndef ZH_NO_PROFILER
   ZH_ULONG ulLastOpcode = 0; /* opcodes profiler support */
   ZH_ULONG ulPastClock = 0;  /* opcodes profiler support */
#endif
#if ! defined( ZH_GUI )
   int * piKeyPolls = zh_stackKeyPolls();
#endif

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmExecute(%p, %p)", ( const void * ) pCode, ( void * ) pSymbols ) );

#ifndef ZH_NO_PROFILER
   if( zh_bProfiler )
      ulPastClock = ( ZH_ULONG ) clock();
#endif

   for( ;; )
   {
#ifndef ZH_NO_PROFILER
      if( zh_bProfiler )
      {
         ZH_ULONG ulActualClock = ( ZH_ULONG ) clock();

         zh_ulOpcodesTime[ ulLastOpcode ] += ( ulActualClock - ulPastClock );
         ulPastClock = ulActualClock;
         ulLastOpcode = pCode[ 0 ];
         zh_ulOpcodesCalls[ ulLastOpcode ]++;
      }
#endif

#if ! defined( ZH_GUI )
      if( ! --( *piKeyPolls ) )
      {
         zh_inkeyPoll();
         *piKeyPolls = 65536;

         /* IMHO we should have a _SET_ controlled by user
          * something like:

         if( zh_stackSetStruct()->ZH_SET_KEYPOLL )
         {
            zh_inkeyPoll();
            *piKeyPolls = zh_stackSetStruct()->ZH_SET_KEYPOLL;
         }

         for some GTs which can work in asynchronous mode user may
         set it to 0 (or if he doesn't need any inkey poll) and
         when ALT+C/ALT+D is pressed (or any other platform dependent
         key combination) they should set proper flags in
         ActionRequest so we can serve it in main VM loop without
         performance decrease or ignore depending on
         zh_stackSetStruct()->ZH_SET_CANCEL,
         zh_stackSetStruct()->ZH_SET_DEBUG flags
         */
      }
#endif
      if( zh_vmThreadRequest )
         zh_vmRequestTest();

      switch( pCode[ 0 ] )
      {
         /* Operators ( mathematical / character / misc ) */

         case ZH_P_NEGATE:
            zh_vmNegate();
            pCode++;
            break;

         case ZH_P_PLUS:
            zh_vmPlus( zh_stackItemFromTop( -2 ), zh_stackItemFromTop( -2 ), zh_stackItemFromTop( -1 ) );
            zh_stackPop();
            pCode++;
            break;

         case ZH_P_PLUSEQ:
            {
               PZH_ITEM pResult, pValue;
               pResult = zh_itemUnRef( zh_stackItemFromTop( -2 ) );
               pValue = zh_stackItemFromTop( -1 );
               zh_vmPlus( pResult, pResult, pValue );
               zh_itemCopy( pValue, pResult );
               zh_itemMove( zh_stackItemFromTop( -2 ), pValue );
               zh_stackDec();
            }
            pCode++;
            break;

         case ZH_P_PLUSEQPOP:
            {
               PZH_ITEM pResult;
               pResult = zh_itemUnRef( zh_stackItemFromTop( -2 ) );
               zh_vmPlus( pResult, pResult, zh_stackItemFromTop( -1 ) );
               zh_stackPop();
               zh_stackPop();
            }
            pCode++;
            break;

         case ZH_P_MINUS:
            zh_vmMinus( zh_stackItemFromTop( -2 ), zh_stackItemFromTop( -2 ), zh_stackItemFromTop( -1 ) );
            zh_stackPop();
            pCode++;
            break;

         case ZH_P_MINUSEQ:
            {
               PZH_ITEM pResult, pValue;
               pResult = zh_itemUnRef( zh_stackItemFromTop( -2 ) );
               pValue = zh_stackItemFromTop( -1 );
               zh_vmMinus( pResult, pResult, pValue );
               zh_itemCopy( pValue, pResult );
               zh_itemMove( zh_stackItemFromTop( -2 ), pValue );
               zh_stackDec();
            }
            pCode++;
            break;

         case ZH_P_MINUSEQPOP:
            {
               PZH_ITEM pResult;
               pResult = zh_itemUnRef( zh_stackItemFromTop( -2 ) );
               zh_vmMinus( pResult, pResult, zh_stackItemFromTop( -1 ) );
               zh_stackPop();
               zh_stackPop();
            }
            pCode++;
            break;

         case ZH_P_MULT:
            zh_vmMult( zh_stackItemFromTop( -2 ), zh_stackItemFromTop( -2 ), zh_stackItemFromTop( -1 ) );
            zh_stackPop();
            pCode++;
            break;

         case ZH_P_MULTEQ:
            {
               PZH_ITEM pResult, pValue;
               pResult = zh_itemUnRef( zh_stackItemFromTop( -2 ) );
               pValue = zh_stackItemFromTop( -1 );
               zh_vmMult( pResult, pResult, pValue );
               zh_itemCopy( pValue, pResult );
               zh_itemMove( zh_stackItemFromTop( -2 ), pValue );
               zh_stackDec();
            }
            pCode++;
            break;

         case ZH_P_MULTEQPOP:
            {
               PZH_ITEM pResult;
               pResult = zh_itemUnRef( zh_stackItemFromTop( -2 ) );
               zh_vmMult( pResult, pResult, zh_stackItemFromTop( -1 ) );
               zh_stackPop();
               zh_stackPop();
            }
            pCode++;
            break;

         case ZH_P_DIVIDE:
            zh_vmDivide( zh_stackItemFromTop( -2 ), zh_stackItemFromTop( -2 ), zh_stackItemFromTop( -1 ) );
            zh_stackPop();
            pCode++;
            break;

         case ZH_P_DIVEQ:
            {
               PZH_ITEM pResult, pValue;
               pResult = zh_itemUnRef( zh_stackItemFromTop( -2 ) );
               pValue = zh_stackItemFromTop( -1 );
               zh_vmDivide( pResult, pResult, pValue );
               zh_itemCopy( pValue, pResult );
               zh_itemMove( zh_stackItemFromTop( -2 ), pValue );
               zh_stackDec();
            }
            pCode++;
            break;

         case ZH_P_DIVEQPOP:
            {
               PZH_ITEM pResult;
               pResult = zh_itemUnRef( zh_stackItemFromTop( -2 ) );
               zh_vmDivide( pResult, pResult, zh_stackItemFromTop( -1 ) );
               zh_stackPop();
               zh_stackPop();
            }
            pCode++;
            break;

         case ZH_P_MODULUS:
            zh_vmModulus( zh_stackItemFromTop( -2 ), zh_stackItemFromTop( -2 ), zh_stackItemFromTop( -1 ) );
            zh_stackPop();
            pCode++;
            break;

         case ZH_P_MODEQ:
            {
               PZH_ITEM pResult, pValue;
               pResult = zh_itemUnRef( zh_stackItemFromTop( -2 ) );
               pValue = zh_stackItemFromTop( -1 );
               zh_vmModulus( pResult, pResult, pValue );
               zh_itemCopy( pValue, pResult );
               zh_itemMove( zh_stackItemFromTop( -2 ), pValue );
               zh_stackDec();
            }
            pCode++;
            break;

         case ZH_P_MODEQPOP:
            {
               PZH_ITEM pResult;
               pResult = zh_itemUnRef( zh_stackItemFromTop( -2 ) );
               zh_vmModulus( pResult, pResult, zh_stackItemFromTop( -1 ) );
               zh_stackPop();
               zh_stackPop();
            }
            pCode++;
            break;

         case ZH_P_POWER:
            zh_vmPower( zh_stackItemFromTop( -2 ), zh_stackItemFromTop( -2 ), zh_stackItemFromTop( -1 ) );
            zh_stackPop();
            pCode++;
            break;

         case ZH_P_EXPEQ:
            {
               PZH_ITEM pResult, pValue;
               pResult = zh_itemUnRef( zh_stackItemFromTop( -2 ) );
               pValue = zh_stackItemFromTop( -1 );
               zh_vmPower( pResult, pResult, pValue );
               zh_itemCopy( pValue, pResult );
               zh_itemMove( zh_stackItemFromTop( -2 ), pValue );
               zh_stackDec();
            }
            pCode++;
            break;

         case ZH_P_EXPEQPOP:
            {
               PZH_ITEM pResult;
               pResult = zh_itemUnRef( zh_stackItemFromTop( -2 ) );
               zh_vmPower( pResult, pResult, zh_stackItemFromTop( -1 ) );
               zh_stackPop();
               zh_stackPop();
            }
            pCode++;
            break;

         case ZH_P_INC:
            zh_vmInc( zh_stackItemFromTop( -1 ) );
            pCode++;
            break;

         case ZH_P_INCEQ:
            {
               PZH_ITEM pResult, pValue, pTemp;
               pResult = zh_stackItemFromTop( -1 );
               pValue = zh_itemUnRef( pResult );
               zh_vmInc( pValue );
               pTemp = zh_stackAllocItem();
               zh_itemCopy( pTemp, pValue );
               zh_itemMove( pResult, pTemp );
               zh_stackDec();
               pCode++;
            }
            break;

         case ZH_P_INCEQPOP:
            zh_vmInc( zh_itemUnRef( zh_stackItemFromTop( -1 ) ) );
            zh_stackPop();
            pCode++;
            break;

         case ZH_P_DEC:
            zh_vmDec( zh_stackItemFromTop( -1 ) );
            pCode++;
            break;

         case ZH_P_DECEQ:
            {
               PZH_ITEM pResult, pValue, pTemp;
               pResult = zh_stackItemFromTop( -1 );
               pValue = zh_itemUnRef( pResult );
               zh_vmDec( pValue );
               pTemp = zh_stackAllocItem();
               zh_itemCopy( pTemp, pValue );
               zh_itemMove( pResult, pTemp );
               zh_stackDec();
               pCode++;
            }
            break;

         case ZH_P_DECEQPOP:
            zh_vmDec( zh_itemUnRef( zh_stackItemFromTop( -1 ) ) );
            zh_stackPop();
            pCode++;
            break;

         case ZH_P_FUNCPTR:
            zh_vmFuncPtr();
            pCode++;
            break;

         /* Operators (relational) */

         case ZH_P_EQUAL:
            zh_vmEqual();
            pCode++;
            break;

         case ZH_P_EXACTLYEQUAL:
            zh_vmExactlyEqual();
            pCode++;
            break;

         case ZH_P_NOTEQUAL:
            zh_vmNotEqual();
            pCode++;
            break;

         case ZH_P_LESS:
            zh_vmLess();
            pCode++;
            break;

         case ZH_P_LESSEQUAL:
            zh_vmLessEqual();
            pCode++;
            break;

         case ZH_P_GREATER:
            zh_vmGreater();
            pCode++;
            break;

         case ZH_P_GREATEREQUAL:
            zh_vmGreaterEqual();
            pCode++;
            break;

         case ZH_P_INSTRING:
            zh_vmInstring();
            pCode++;
            break;

         case ZH_P_FORTEST:
            zh_vmForTest();
            pCode++;
            break;

         case ZH_P_ENUMSTART:
            zh_vmEnumStart( ( unsigned char ) pCode[ 1 ], ( unsigned char ) pCode[ 2 ] );
            pCode += 3;
            break;

         case ZH_P_ENUMNEXT:
            zh_vmEnumNext();
            pCode++;
            break;

         case ZH_P_ENUMPREV:
            zh_vmEnumPrev();
            pCode++;
            break;

         case ZH_P_ENUMEND:
            zh_vmEnumEnd();
            pCode++;
            break;

         case ZH_P_SWITCH:
            pCode = zh_vmSwitch( pCode + 3, ZH_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            break;

         /* Operators (logical) */

         case ZH_P_NOT:
            zh_vmNot();
            pCode++;
            break;

         case ZH_P_AND:
            zh_vmAnd();
            pCode++;
            break;

         case ZH_P_OR:
            zh_vmOr();
            pCode++;
            break;

         /* Array */

         case ZH_P_ARRAYPUSH:
            zh_vmArrayPush();
            pCode++;
            break;

         case ZH_P_ARRAYPUSHREF:
            zh_vmArrayPushRef();
            pCode++;
            break;

         case ZH_P_ARRAYPOP:
            zh_vmArrayPop();
            pCode++;
            break;

         case ZH_P_ARRAYDIM:
            zh_vmArrayDim( ZH_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         case ZH_P_ARRAYGEN:
            zh_vmArrayGen( ZH_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         case ZH_P_HASHGEN:
            zh_vmHashGen( ZH_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         /* Object */

         case ZH_P_MESSAGE:
            zh_vmPushSymbol( pSymbols + ZH_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         /* Database */

         case ZH_P_SWAPALIAS:
            zh_vmSwapAlias();
            pCode++;
            break;

         /* Execution */

         case ZH_P_DO:
            zh_vmProc( ZH_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         case ZH_P_DOSHORT:
            zh_vmProc( pCode[ 1 ] );
            pCode += 2;
            break;

         case ZH_P_FUNCTION:
            zh_itemSetNil( zh_stackReturnItem() );
            zh_vmProc( ZH_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            zh_stackPushReturn();
            pCode += 3;
            break;

         case ZH_P_FUNCTIONSHORT:
            zh_itemSetNil( zh_stackReturnItem() );
            zh_vmProc( pCode[ 1 ] );
            zh_stackPushReturn();
            pCode += 2;
            break;

         case ZH_P_SEND:
            zh_itemSetNil( zh_stackReturnItem() );
            zh_vmSend( ZH_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            pCode += 3;

            /* Small opt */
            if( pCode[ 0 ] == ZH_P_POP )
               pCode++;
            else
               zh_stackPushReturn();
            break;

         case ZH_P_SENDSHORT:
            zh_itemSetNil( zh_stackReturnItem() );
            zh_vmSend( pCode[ 1 ] );
            pCode += 2;

            /* Small opt */
            if( pCode[ 0 ] == ZH_P_POP )
               pCode++;
            else
               zh_stackPushReturn();
            break;

         case ZH_P_PUSHOVARREF:
            zh_vmPushObjectVarRef();
            pCode++;
            break;

         case ZH_P_LINE:
            ZH_TRACE( ZH_TR_INFO, ( "Opcode: ZH_P_LINE: %s (%i)",
                                    zh_stackBaseItem()->item.asSymbol.value->szName,
                                    zh_stackBaseItem()->item.asSymbol.stackstate->uiLineNo ) );

            zh_stackBaseItem()->item.asSymbol.stackstate->uiLineNo = ZH_PCODE_MKUSHORT( &pCode[ 1 ] );
            if( zh_stackBaseItem()->item.asSymbol.stackstate->fDebugging )
               zh_vmDebuggerShowLine( zh_stackBaseItem()->item.asSymbol.stackstate->uiLineNo );
            pCode += 3;
            break;

         case ZH_P_PARAMETER:
            zh_memvarNewParameter( pSymbols + ZH_PCODE_MKUSHORT( &pCode[ 1 ] ), zh_stackItemFromBase( pCode[ 3 ] ) );
            ZH_TRACE( ZH_TR_INFO, ( "(zh_vmPopParameter)" ) );
            pCode += 4;
            break;

         case ZH_P_FRAME:
            zh_vmFrame( ( unsigned char ) pCode[ 1 ], ( unsigned char ) pCode[ 2 ] );
            pCode += 3;
            break;

         case ZH_P_VFRAME:
            zh_vmVFrame( ( unsigned char ) pCode[ 1 ], ( unsigned char ) pCode[ 2 ] );
            pCode += 3;
            break;

         case ZH_P_LARGEFRAME:
            zh_vmFrame( ZH_PCODE_MKUSHORT( &pCode[ 1 ] ), ( unsigned char ) pCode[ 3 ] );
            pCode += 4;
            break;

         case ZH_P_LARGEVFRAME:
            zh_vmVFrame( ZH_PCODE_MKUSHORT( &pCode[ 1 ] ), ( unsigned char ) pCode[ 3 ] );
            pCode += 4;
            break;

         case ZH_P_SFRAME:
            zh_vmSFrame( pSymbols + ZH_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         case ZH_P_STATICS:
            zh_vmStatics( pSymbols + ZH_PCODE_MKUSHORT( &pCode[ 1 ] ), ZH_PCODE_MKUSHORT( &pCode[ 3 ] ) );
            pCode += 5;
            break;

         case ZH_P_THREADSTATICS:
         {
            ZH_USHORT uiCount = ZH_PCODE_MKUSHORT( &pCode[ 1 ] );
            zh_vmInitThreadStatics( uiCount, &pCode[ 3 ] );
            pCode += 3 + ( ( ZH_ULONG ) uiCount << 1 );
            break;
         }

         case ZH_P_LOCALNAME:
            zh_vmLocalName( ZH_PCODE_MKUSHORT( &pCode[ 1 ] ),
                            ( const char * ) pCode + 3 );
            pCode += 3;
            while( *pCode++ )
               ;
            break;

         case ZH_P_STATICNAME:
            zh_vmStaticName( pCode[ 1 ], ZH_PCODE_MKUSHORT( &pCode[ 2 ] ),
                             ( const char * ) pCode + 4 );
            pCode += 4;
            while( *pCode++ )
               ;
            break;

         case ZH_P_MODULENAME:
            zh_vmModuleName( ( const char * ) pCode + 1 );
            pCode++;
            while( *pCode++ )
               ;
            break;

         case ZH_P_RETVALUE:
            zh_stackPopReturn();
            zh_stackReturnItem()->type &= ~ZH_IT_MEMOFLAG;
            pCode++;
            break;

         case ZH_P_ENDBLOCK:
            ZH_TRACE( ZH_TR_INFO, ( "ZH_P_ENDBLOCK" ) );
            zh_stackPopReturn();
            /* manually inlined zh_vmRequestEndProc() for some C compilers
             * which does not make such optimization
             */
            zh_stackSetActionRequest( ZH_ENDPROC_REQUESTED );
            break;

         case ZH_P_ENDPROC:
            ZH_TRACE( ZH_TR_INFO, ( "ZH_P_ENDPROC" ) );
            /* manually inlined zh_vmRequestEndProc() for some C compilers
             * which does not make such optimization
             */
            zh_stackSetActionRequest( ZH_ENDPROC_REQUESTED );
            break;

         /* BEGIN SEQUENCE/RECOVER/ALWAYS/END SEQUENCE */

         case ZH_P_SEQBLOCK:
            zh_vmSeqBlock();
            pCode++;
            break;

         case ZH_P_SEQALWAYS:
         {
            /*
             * Create the SEQUENCE envelope
             * [ break return value ]  -2
             * [ recover envelope   ]  -1
             * [                    ] <- new recover base
             */

            PZH_ITEM pItem;

            /*
             * 1) clear the storage for value returned by BREAK statement
             */
            zh_stackAllocItem()->type = ZH_IT_NIL;

            /*
             * 2) recover data
             */
            pItem = zh_stackAllocItem();
            /* mark type as NIL - it's not real item */
            pItem->type = ZH_IT_RECOVER;
            /* store the address of RECOVER or END opcode */
            pItem->item.asRecover.recover = pCode + ZH_PCODE_MKINT24( &pCode[ 1 ] );
            /* store current RECOVER base */
            pItem->item.asRecover.base = zh_stackGetRecoverBase();
            /* store current bCanRecover flag - in a case of nested sequences */
            pItem->item.asRecover.flags = ZH_SEQ_DOALWAYS | ( bCanRecover ? ZH_SEQ_CANRECOVER : 0 );
            /* clear new recovery state */
            pItem->item.asRecover.request = 0;

            /*
             * set new recover base
             */
            zh_stackSetRecoverBase( zh_stackTopOffset() );
            /*
             * we are now inside a valid SEQUENCE envelope
             */
            bCanRecover = ZH_TRUE;
            pCode += 4;
            break;
         }

         case ZH_P_ALWAYSBEGIN:
#if defined( _ZH_RECOVER_DEBUG )
            if( zh_stackItemFromTop( ZH_RECOVER_STATE )->type != ZH_IT_RECOVER )
               zh_errInternal( ZH_EI_ERRUNRECOV, "ZH_P_ALWAYSBEGIN", NULL, NULL );
#endif
            /* change the recover address to ALWAYSEND opcode */
            zh_stackItemFromTop( ZH_RECOVER_STATE )->item.asRecover.recover =
               pCode + ZH_PCODE_MKINT24( &pCode[ 1 ] );
            /* store and reset action */
            zh_stackItemFromTop( ZH_RECOVER_STATE )->item.asRecover.flags |=
               zh_stackItemFromTop( ZH_RECOVER_STATE )->item.asRecover.request;
            zh_stackItemFromTop( ZH_RECOVER_STATE )->item.asRecover.request = 0;
            /* store RETURN value */
            if( zh_stackItemFromTop( ZH_RECOVER_STATE )->item.asRecover.flags & ZH_ENDPROC_REQUESTED )
               zh_itemMove( zh_stackItemFromTop( ZH_RECOVER_VALUE ), zh_stackReturnItem() );
            pCode += 4;
            break;

         case ZH_P_ALWAYSEND:
         {
            ZH_USHORT uiPrevAction, uiCurrAction;

#if defined( _ZH_RECOVER_DEBUG )
            if( zh_stackItemFromTop( ZH_RECOVER_STATE )->type != ZH_IT_RECOVER )
               zh_errInternal( ZH_EI_ERRUNRECOV, "ZH_P_ALWAYSEND", NULL, NULL );
#endif
            uiPrevAction = zh_stackItemFromTop( ZH_RECOVER_STATE )->item.asRecover.flags;
            uiCurrAction = zh_stackItemFromTop( ZH_RECOVER_STATE )->item.asRecover.request;

            /* restore previous recovery base */
            bCanRecover = ( zh_stackItemFromTop( ZH_RECOVER_STATE )->item.asRecover.flags & ZH_SEQ_CANRECOVER ) != 0;
            zh_stackSetRecoverBase( zh_stackItemFromTop( ZH_RECOVER_STATE )->item.asRecover.base );

            /* restore requested action */
            if( ( uiCurrAction | uiPrevAction ) & ZH_QUIT_REQUESTED )
               zh_stackSetActionRequest( ZH_QUIT_REQUESTED );
            else if( ( uiCurrAction | uiPrevAction ) & ZH_BREAK_REQUESTED )
               zh_stackSetActionRequest( ZH_BREAK_REQUESTED );
            else if( ( uiCurrAction | uiPrevAction ) & ZH_ENDPROC_REQUESTED )
               zh_stackSetActionRequest( ZH_ENDPROC_REQUESTED );
            else
               zh_stackSetActionRequest( 0 );

            /* Remove the ALWAYS envelope */
            zh_stackDec();

            /* restore RETURN value if not overloaded inside ALWAYS code */
            if( ! ( uiCurrAction & ZH_ENDPROC_REQUESTED ) &&
                  ( uiPrevAction & ZH_ENDPROC_REQUESTED ) )
               zh_stackPopReturn();
            else
               zh_stackPop();
            pCode++;
            break;
         }

         case ZH_P_SEQBEGIN:
         {
            /*
             * Create the SEQUENCE envelope
             * [ break return value ]  -2
             * [ recover envelope   ]  -1
             * [                    ] <- new recover base
             */

            PZH_ITEM pItem;

            /*
             * 1) clear the storage for value returned by BREAK statement
             */
            zh_stackAllocItem()->type = ZH_IT_NIL;

            /*
             * 2) recover data
             */
            pItem = zh_stackAllocItem();
            /* mark type as NIL - it's not real item */
            pItem->type = ZH_IT_RECOVER;
            /* store the address of RECOVER or END opcode */
            pItem->item.asRecover.recover = pCode + ZH_PCODE_MKINT24( &pCode[ 1 ] );
            /* store current RECOVER base */
            pItem->item.asRecover.base = zh_stackGetRecoverBase();
            /* store current bCanRecover flag - in a case of nested sequences */
            pItem->item.asRecover.flags = bCanRecover ? ZH_SEQ_CANRECOVER : 0;
            /* clear new recovery state */
            pItem->item.asRecover.request = 0;

            /*
             * set new recover base
             */
            zh_stackSetRecoverBase( zh_stackTopOffset() );
            /*
             * we are now inside a valid SEQUENCE envelope
             */
            bCanRecover = ZH_TRUE;
            pCode += 4;
            break;
         }

         case ZH_P_SEQEND:
            /*
             * Remove the SEQUENCE envelope
             * This is executed either at the end of sequence or as the
             * response to the break statement if there is no RECOVER clause
             */
#if defined( _ZH_RECOVER_DEBUG )
            if( zh_stackItemFromTop( ZH_RECOVER_STATE )->type != ZH_IT_RECOVER )
               zh_errInternal( ZH_EI_ERRUNRECOV, "ZH_P_SEQEND", NULL, NULL );
#endif
            /*
             * 2) Restore previous recovery state
             */
            bCanRecover = ( zh_stackItemFromTop( ZH_RECOVER_STATE )->item.asRecover.flags & ZH_SEQ_CANRECOVER ) != 0;
            zh_stackSetRecoverBase( zh_stackItemFromTop( ZH_RECOVER_STATE )->item.asRecover.base );
            zh_stackDec();

            /*
             * 1) Discard the value returned by BREAK statement - there
             * was no RECOVER clause or there was no BREAK statement
             */
            zh_stackPop();
            /*
             * skip outside of SEQUENCE structure
             */
            pCode += ZH_PCODE_MKINT24( &pCode[ 1 ] );
            break;

         case ZH_P_SEQRECOVER:
            /*
             * Execute the RECOVER code
             */
#if defined( _ZH_RECOVER_DEBUG )
            if( zh_stackItemFromTop( ZH_RECOVER_STATE )->type != ZH_IT_RECOVER )
               zh_errInternal( ZH_EI_ERRUNRECOV, "ZH_P_SEQRECOVER", NULL, NULL );
#endif
            /*
             * 2) Restore previous recovery state
             */
            bCanRecover = ( zh_stackItemFromTop( ZH_RECOVER_STATE )->item.asRecover.flags & ZH_SEQ_CANRECOVER ) != 0;
            zh_stackSetRecoverBase( zh_stackItemFromTop( ZH_RECOVER_STATE )->item.asRecover.base );
            zh_stackDec();
            /*
             * 1) Leave the value returned from BREAK  - it will be popped
             * in next executed opcode
             */
            pCode++;
            break;

         /* Jumps */

         case ZH_P_JUMPNEAR:
            pCode += ( signed char ) pCode[ 1 ];
            break;

         case ZH_P_JUMP:
            pCode += ZH_PCODE_MKSHORT( &pCode[ 1 ] );
            break;

         case ZH_P_JUMPFAR:
            pCode += ZH_PCODE_MKINT24( &pCode[ 1 ] );
            break;

         case ZH_P_JUMPFALSENEAR:
            if( ! zh_vmPopLogical() )
               pCode += ( signed char ) pCode[ 1 ];
            else
               pCode += 2;
            break;

         case ZH_P_JUMPFALSE:
            if( ! zh_vmPopLogical() )
               pCode += ZH_PCODE_MKSHORT( &pCode[ 1 ] );
            else
               pCode += 3;
            break;

         case ZH_P_JUMPFALSEFAR:
            if( ! zh_vmPopLogical() )
               pCode += ZH_PCODE_MKINT24( &pCode[ 1 ] );
            else
               pCode += 4;
            break;

         case ZH_P_JUMPTRUENEAR:
            if( zh_vmPopLogical() )
               pCode += ( signed char ) pCode[ 1 ];
            else
               pCode += 2;
            break;

         case ZH_P_JUMPTRUE:
            if( zh_vmPopLogical() )
               pCode += ZH_PCODE_MKSHORT( &pCode[ 1 ] );
            else
               pCode += 3;
            break;

         case ZH_P_JUMPTRUEFAR:
            if( zh_vmPopLogical() )
               pCode += ZH_PCODE_MKINT24( &pCode[ 1 ] );
            else
               pCode += 4;
            break;

         /* Push */

         case ZH_P_TRUE:
            {
               PZH_ITEM pItem = zh_stackAllocItem();

               pItem->type = ZH_IT_LOGICAL;
               pItem->item.asLogical.value = ZH_TRUE;
               pCode++;
            }
            break;

         case ZH_P_FALSE:
            {
               PZH_ITEM pItem = zh_stackAllocItem();

               pItem->type = ZH_IT_LOGICAL;
               pItem->item.asLogical.value = ZH_FALSE;
               pCode++;
            }
            break;

         case ZH_P_ONE:
            {
               PZH_ITEM pItem = zh_stackAllocItem();

               pItem->type = ZH_IT_INTEGER;
               pItem->item.asInteger.value = 1;
               pItem->item.asInteger.length = 10;
               ZH_TRACE( ZH_TR_INFO, ( "(ZH_P_ONE)" ) );
               pCode++;
            }
            break;

         case ZH_P_ZERO:
            {
               PZH_ITEM pItem = zh_stackAllocItem();

               pItem->type = ZH_IT_INTEGER;
               pItem->item.asInteger.value = 0;
               pItem->item.asInteger.length = 10;
               ZH_TRACE( ZH_TR_INFO, ( "(ZH_P_ZERO)" ) );
               pCode++;
            }
            break;

         case ZH_P_PUSHNIL:
            zh_stackAllocItem()->type = ZH_IT_NIL;
            ZH_TRACE( ZH_TR_INFO, ( "(ZH_P_PUSHNIL)" ) );
            pCode++;
            break;

         case ZH_P_PUSHBYTE:
            {
               PZH_ITEM pItem = zh_stackAllocItem();

               pItem->type = ZH_IT_INTEGER;
               pItem->item.asInteger.value = ( signed char ) pCode[ 1 ];
               pItem->item.asInteger.length = 10;
               ZH_TRACE( ZH_TR_INFO, ( "(ZH_P_PUSHBYTE)" ) );
               pCode += 2;
            }
            break;

         case ZH_P_PUSHINT:
            {
               PZH_ITEM pItem = zh_stackAllocItem();

               pItem->type = ZH_IT_INTEGER;
               pItem->item.asInteger.value = ZH_PCODE_MKSHORT( &pCode[ 1 ] );
               pItem->item.asInteger.length = 10;
               ZH_TRACE( ZH_TR_INFO, ( "(ZH_P_PUSHINT)" ) );
               pCode += 3;
            }
            break;

         case ZH_P_PUSHLONG:
            ZH_TRACE( ZH_TR_DEBUG, ( "(ZH_P_PUSHLONG)" ) );
#if ZH_VMINT_MAX >= INT32_MAX
            zh_vmPushIntegerConst( ( int ) ZH_PCODE_MKLONG( &pCode[ 1 ] ) );
#else
            zh_vmPushLongConst( ( long ) ZH_PCODE_MKLONG( &pCode[ 1 ] ) );
#endif
            pCode += 5;
            break;

         case ZH_P_PUSHLONGLONG:
            ZH_TRACE( ZH_TR_DEBUG, ( "(ZH_P_PUSHLONGLONG)" ) );
#if ! defined( ZH_LONG_LONG_OFF )
            zh_vmPushLongLongConst( ZH_PCODE_MKLONGLONG( &pCode[ 1 ] ) );
#else
            zh_vmPushDoubleConst( ZH_PCODE_MKLONGLONG( &pCode[ 1 ] ),
                                  ZH_DEFAULT_WIDTH, ZH_DEFAULT_DECIMALS );
#endif
            pCode += 9;

            break;

         case ZH_P_PUSHDOUBLE:
            zh_vmPushDoubleConst( ZH_PCODE_MKDOUBLE( &pCode[ 1 ] ),
                                  ( int ) *( const unsigned char * ) &pCode[ 1 + sizeof( double ) ],
                                  ( int ) *( const unsigned char * ) &pCode[ 2 + sizeof( double ) ] );
            pCode += 3 + sizeof( double );
            break;

         case ZH_P_PUSHSTRSHORT:
            if( bDynCode )
               zh_vmPushString( ( const char * ) pCode + 2, ( ZH_SIZE ) pCode[ 1 ] - 1 );
            else
               zh_vmPushStringPcode( ( const char * ) pCode + 2, ( ZH_SIZE ) pCode[ 1 ] - 1 );
            pCode += 2 + pCode[ 1 ];
            break;

         case ZH_P_PUSHSTR:
         {
            ZH_USHORT uiSize = ZH_PCODE_MKUSHORT( &pCode[ 1 ] );
            if( bDynCode )
               zh_vmPushString( ( const char * ) pCode + 3, uiSize - 1 );
            else
               zh_vmPushStringPcode( ( const char * ) pCode + 3, uiSize - 1 );
            pCode += 3 + uiSize;
            break;
         }

         case ZH_P_PUSHSTRLARGE:
         {
            ZH_SIZE nSize = ZH_PCODE_MKUINT24( &pCode[ 1 ] );
            if( bDynCode )
               zh_vmPushString( ( const char * ) pCode + 4, nSize - 1 );
            else
               zh_vmPushStringPcode( ( const char * ) pCode + 4, nSize - 1 );
            pCode += 4 + nSize;
            break;
         }

         case ZH_P_PUSH_STR_HIDDEN:
         {
            ZH_SIZE nSize = ( ZH_SIZE ) ZH_PCODE_MKUSHORT( &pCode[ 2 ] );
            char * szText = zh_compDecodeString( pCode[ 1 ], ( const char * ) pCode + 4, &nSize );
            zh_itemPutCLPtr( zh_stackAllocItem(), szText, nSize );
            pCode += ( 4 + nSize );
            break;
         }

         case ZH_P_PUSH_DATE:
            ZH_TRACE( ZH_TR_DEBUG, ( "(ZH_P_PUSH_DATE)" ) );
            {
               PZH_ITEM pItem = zh_stackAllocItem();

               pItem->type = ZH_IT_DATE;
               pItem->item.asDateTime.julian = ( long ) ZH_PCODE_MKLONG( &pCode[ 1 ] );
               pItem->item.asDateTime.time = 0;
               pCode += 5;
            }
            break;

         case ZH_P_PUSHTIMESTAMP:
            ZH_TRACE( ZH_TR_DEBUG, ( "(ZH_P_PUSHTIMESTAMP)" ) );
            {
               PZH_ITEM pItem = zh_stackAllocItem();

               pItem->type = ZH_IT_TIMESTAMP;
               pItem->item.asDateTime.julian = ( long ) ZH_PCODE_MKLONG( &pCode[ 1 ] );
               pItem->item.asDateTime.time = ( long ) ZH_PCODE_MKLONG( &pCode[ 5 ] );
               pCode += 9;
            }
            break;

         case ZH_P_PUSH_BLOCK:
         {
            /* +0    -> _pushblock
             * +1 +2 -> size of codeblock
             * +3 +4 -> number of expected parameters
             * +5 +6 -> number of referenced local variables
             * +7    -> start of table with referenced local variables
             */
            ZH_SIZE nSize = ZH_PCODE_MKUSHORT( &pCode[ 1 ] );
            zh_vmPushBlock( pCode + 3, pSymbols, bDynCode ? nSize - 7 : 0 );
            pCode += nSize;
            break;
         }
         case ZH_P_PUSH_BLOCKLARGE:
         {
            /* +0       -> _pushblock
             * +1 +2 +3 -> size of codeblock
             * +4 +5    -> number of expected parameters
             * +6 +7    -> number of referenced local variables
             * +8       -> start of table with referenced local variables
             */
            ZH_SIZE nSize = ZH_PCODE_MKUINT24( &pCode[ 1 ] );
            zh_vmPushBlock( pCode + 4, pSymbols, bDynCode ? nSize - 8 : 0 );
            pCode += nSize;
            break;
         }
         case ZH_P_PUSH_BLOCKSHORT:
         {
            /* +0    -> _pushblock
             * +1    -> size of codeblock
             */
            ZH_SIZE nSize = pCode[ 1 ];
            zh_vmPushBlockShort( pCode + 2, pSymbols, bDynCode ? nSize - 2 : 0 );
            pCode += nSize;
            break;
         }

         case ZH_P_PUSHSELF:
            zh_vmPush( zh_stackSelfItem() );
            pCode++;
            break;

         case ZH_P_PUSHSYM:
            zh_vmPushSymbol( pSymbols + ZH_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         case ZH_P_PUSHSYMNEAR:
            zh_vmPushSymbol( pSymbols + pCode[ 1 ] );
            pCode += 2;
            break;

         case ZH_P_PUSHFUNCSYM:
            zh_vmPushSymbol( pSymbols + ZH_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            zh_stackAllocItem()->type = ZH_IT_NIL;
            pCode += 3;
            break;

         case ZH_P_PUSHALIAS:
            zh_vmPushAlias();
            pCode++;
            break;

         case ZH_P_PUSH_ALIASED_FIELD:
            zh_vmPushAliasedField( pSymbols + ZH_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         case ZH_P_PUSH_ALIASED_FIELDNEAR:
            zh_vmPushAliasedField( pSymbols + pCode[ 1 ] );
            pCode += 2;
            break;

         case ZH_P_PUSHALIASEDVAR:
            zh_vmPushAliasedVar( pSymbols + ZH_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         case ZH_P_PUSHFIELD:
            /* It pushes the current value of the given field onto the eval stack
             */
            zh_rddGetFieldValue( zh_stackAllocItem(), pSymbols + ZH_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            ZH_TRACE( ZH_TR_INFO, ( "(zh_vmPushField)" ) );
            pCode += 3;
            break;

         case ZH_P_PUSHLOCAL:
            zh_vmPushLocal( ZH_PCODE_MKSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         case ZH_P_PUSHLOCALNEAR:
            zh_vmPushLocal( ( signed char ) pCode[ 1 ] );
            pCode += 2;  /* only first two bytes are used */
            break;

         case ZH_P_PUSHLOCALREF:
            zh_vmPushLocalByRef( ZH_PCODE_MKSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         case ZH_P_PUSHSTATIC:
            zh_vmPushStatic( ZH_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         case ZH_P_PUSHSTATICREF:
            zh_vmPushStaticByRef( ZH_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         case ZH_P_PUSHMEMVAR:
            zh_memvarGetValue( zh_stackAllocItem(), pSymbols + ZH_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            ZH_TRACE( ZH_TR_INFO, ( "(zh_vmPushMemvar)" ) );
            pCode += 3;
            break;

         case ZH_P_PUSHMEMVARREF:
            zh_memvarGetRefer( zh_stackAllocItem(), pSymbols + ZH_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            ZH_TRACE( ZH_TR_INFO, ( "(zh_vmPushMemvarRef)" ) );
            pCode += 3;
            break;

         case ZH_P_PUSHVAR:
            /* Push a value of variable of unknown type onto the eval stack
             */
            zh_vmPushVariable( pSymbols + ZH_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         case ZH_P_DUPLICATE:
            zh_vmDuplicate();
            pCode++;
            break;

         case ZH_P_DUPLUNREF:
            zh_vmDuplUnRef();
            pCode++;
            break;

         case ZH_P_PUSHUNREF:
            zh_vmPushUnRef();
            pCode++;
            break;

         case ZH_P_PUSHVPARAMS:
            zh_vmPushVParams();
            pCode++;
            break;

         case ZH_P_PUSHAPARAMS:
            zh_vmPushAParams();
            pCode++;
            break;

         case ZH_P_SWAP:
            zh_vmSwap( ( unsigned char ) pCode[ 1 ] );
            pCode += 2;
            break;

         /* Pop */

         case ZH_P_POP:
            zh_stackPop();
            pCode++;
            break;

         case ZH_P_POPALIAS:
            zh_vmPopAlias();
            pCode++;
            break;

         case ZH_P_POPALIASEDFIELD:
            zh_vmPopAliasedField( pSymbols + ZH_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         case ZH_P_POPALIASEDFIELDNEAR:
            zh_vmPopAliasedField( pSymbols + pCode[ 1 ] );
            pCode += 2;
            break;

         case ZH_P_POPALIASEDVAR:
            zh_vmPopAliasedVar( pSymbols + ZH_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         case ZH_P_POPFIELD:
            /* Pops a value from the eval stack and uses it to set
             * a new value of the given field
             */
            zh_rddPutFieldValue( zh_stackItemFromTop( -1 ), pSymbols + ZH_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            zh_stackPop();
            ZH_TRACE( ZH_TR_INFO, ( "(zh_vmPopField)" ) );
            pCode += 3;
            break;

         case ZH_P_POPLOCAL:
            zh_vmPopLocal( ZH_PCODE_MKSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         case ZH_P_POPLOCALNEAR:
            zh_vmPopLocal( ( signed char ) pCode[ 1 ] );
            pCode += 2;  /* only first two bytes are used */
            break;

         case ZH_P_POPSTATIC:
            zh_vmPopStatic( ZH_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         case ZH_P_POPMEMVAR:
            zh_memvarSetValue( pSymbols + ZH_PCODE_MKUSHORT( &pCode[ 1 ] ),
                               zh_stackItemFromTop( -1 ) );
            zh_stackPop();
            ZH_TRACE( ZH_TR_INFO, ( "(zh_vmPopMemvar)" ) );
            pCode += 3;
            break;

         case ZH_P_POPVAR:
         {

            zh_memvarSetValue( pSymbols + ZH_PCODE_MKUSHORT( &pCode[ 1 ] ),
                               zh_stackItemFromTop( -1 ) );
            zh_stackPop();
            ZH_TRACE( ZH_TR_INFO, ( "(zh_vmPopVariable)" ) );
            pCode += 3;
            break;
         }

         /* macro creation */

         case ZH_P_MACROPOP:
            /* compile and run - pop a value from the stack */
            zh_macroSetValue( zh_stackItemFromTop( -1 ), pCode[ 1 ] );
            pCode += 2;
            break;

         case ZH_P_MACROPOPALIASED:
            /* compile and run - pop an aliased variable from the stack */
            zh_macroPopAliasedValue( zh_stackItemFromTop( -2 ), zh_stackItemFromTop( -1 ), pCode[ 1 ] );
            pCode += 2;
            break;

         case ZH_P_MACRO_PUSH:
            /* compile and run - leave the result on the stack */
            /* the topmost element on the stack contains a macro
             * string for compilation
             */
            zh_macroGetValue( zh_stackItemFromTop( -1 ), 0, pCode[ 1 ] );
            pCode += 2;
            break;

         case ZH_P_MACRO_PUSHLIST:
            /* compile and run - leave the result on the stack */
            /* the topmost element on the stack contains a macro
             * string for compilation
             */
            zh_macroGetValue( zh_stackItemFromTop( -1 ), ZH_P_MACRO_PUSHLIST, pCode[ 1 ] );
            pCode += 2;
            break;

         case ZH_P_MACRO_PUSHINDEX:
            zh_vmMacroPushIndex();
            pCode++;
            break;

         case ZH_P_MACRO_ARRAY_GEN:
            zh_vmMacroArrayGen( ZH_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         case ZH_P_MACRODO:
            zh_vmMacroDo( ZH_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         case ZH_P_MACRO_FUNC:
            zh_vmMacroFunc( ZH_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         case ZH_P_MACRO_SEND:
            zh_vmMacroSend( ZH_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         case ZH_P_MACRO_PUSHPARE:
            /* compile and run - leave the result on the stack */
            /* the topmost element on the stack contains a macro
             * string for compilation
             */
            zh_macroGetValue( zh_stackItemFromTop( -1 ), ZH_P_MACRO_PUSHPARE, pCode[ 1 ] );
            pCode += 2;
            break;

         case ZH_P_MACRO_PUSHALIASED:
            /* compile and run - leave an aliased variable on the stack */
            zh_macroPushAliasedValue( zh_stackItemFromTop( -2 ), zh_stackItemFromTop( -1 ), pCode[ 1 ] );
            pCode += 2;
            break;

         case ZH_P_MACRO_PUSHREF:
            {
               PZH_ITEM pMacro = zh_stackItemFromTop( -1 );
               zh_macroPushReference( pMacro );
               pCode++;
            }
            break;

         case ZH_P_MACROSYMBOL:
            /* compile into a symbol name (used in function calls) */
            zh_macroPushSymbol( zh_stackItemFromTop( -1 ) );
            pCode++;
            break;

         case ZH_P_MACROTEXT:
            /* macro text substitution
             * "text &macro.other text"
             */
            zh_macroTextValue( zh_stackItemFromTop( -1 ) );
            pCode++;
            break;

         /* macro compiled opcodes - we are using symbol address here */

         case ZH_P_MMESSAGE:
         {
            PZH_DYNSYMBOL pDynSym = ( PZH_DYNSYMBOL ) ZH_GET_PTR( pCode + 1 );
            zh_vmPushSymbol( pDynSym->pSymbol );
            pCode += sizeof( PZH_DYNSYMBOL ) + 1;
            break;
         }

         case ZH_P_MPOPALIASEDFIELD:
         {
            PZH_DYNSYMBOL pDynSym = ( PZH_DYNSYMBOL ) ZH_GET_PTR( pCode + 1 );
            zh_vmPopAliasedField( pDynSym->pSymbol );
            pCode += sizeof( PZH_DYNSYMBOL ) + 1;
            break;
         }

         case ZH_P_MPOPALIASEDVAR:
         {
            PZH_DYNSYMBOL pDynSym = ( PZH_DYNSYMBOL ) ZH_GET_PTR( pCode + 1 );
            zh_vmPopAliasedVar( pDynSym->pSymbol );
            pCode += sizeof( PZH_DYNSYMBOL ) + 1;
            break;
         }

         case ZH_P_MPOPFIELD:
         {
            PZH_DYNSYMBOL pDynSym = ( PZH_DYNSYMBOL ) ZH_GET_PTR( pCode + 1 );
            /* Pops a value from the eval stack and uses it to set
             * a new value of the given field
             */
            zh_rddPutFieldValue( ( zh_stackItemFromTop( -1 ) ), pDynSym->pSymbol );
            zh_stackPop();
            ZH_TRACE( ZH_TR_INFO, ( "(zh_vmMPopField)" ) );
            pCode += sizeof( PZH_DYNSYMBOL ) + 1;
            break;
         }

         case ZH_P_MPOPMEMVAR:
         {
            PZH_DYNSYMBOL pDynSym = ( PZH_DYNSYMBOL ) ZH_GET_PTR( pCode + 1 );
            zh_memvarSetValue( pDynSym->pSymbol, zh_stackItemFromTop( -1 ) );
            zh_stackPop();
            ZH_TRACE( ZH_TR_INFO, ( "(zh_vmMPopMemvar)" ) );
            pCode += sizeof( PZH_DYNSYMBOL ) + 1;
            break;
         }

         case ZH_P_MPUSH_ALIASED_FIELD:
         {
            PZH_DYNSYMBOL pDynSym = ( PZH_DYNSYMBOL ) ZH_GET_PTR( pCode + 1 );
            zh_vmPushAliasedField( pDynSym->pSymbol );
            pCode += sizeof( PZH_DYNSYMBOL ) + 1;
            break;
         }

         case ZH_P_MPUSHALIASEDVAR:
         {
            PZH_DYNSYMBOL pDynSym = ( PZH_DYNSYMBOL ) ZH_GET_PTR( pCode + 1 );
            zh_vmPushAliasedVar( pDynSym->pSymbol );
            pCode += sizeof( PZH_DYNSYMBOL ) + 1;
            break;
         }

         case ZH_P_MPUSH_BLOCK:
         {
            /*NOTE: the pcode is stored in dynamically allocated memory
             * We need to handle it with more care than compile-time
             * codeblocks
             */
            /* +0    -> _pushblock
             * +1 +2 -> size of codeblock
             * +3 +4 -> number of expected parameters
             * +5    -> pcode bytes
             */
            ZH_SIZE nSize = ZH_PCODE_MKUSHORT( &pCode[ 1 ] );
            zh_vmPushMacroBlock( pCode + 5, nSize - 5,
                                 ZH_PCODE_MKUSHORT( &pCode[ 3 ] ) );
            pCode += nSize;
            break;
         }

         case ZH_P_MPUSH_BLOCKLARGE:
         {
            /*NOTE: the pcode is stored in dynamically allocated memory
             * We need to handle it with more care than compile-time
             * codeblocks
             */
            /* +0       -> _pushblock
             * +1 +2 +3 -> size of codeblock
             * +4 +5    -> number of expected parameters
             * +6       -> pcode bytes
             */
            ZH_SIZE nSize = ZH_PCODE_MKUINT24( &pCode[ 1 ] );
            zh_vmPushMacroBlock( pCode + 6, nSize - 6,
                                 ZH_PCODE_MKUSHORT( &pCode[ 4 ] ) );
            pCode += nSize;
            break;
         }

         case ZH_P_MPUSHFIELD:
         {
            PZH_DYNSYMBOL pDynSym = ( PZH_DYNSYMBOL ) ZH_GET_PTR( pCode + 1 );
            /* It pushes the current value of the given field onto the eval stack
             */
            zh_rddGetFieldValue( zh_stackAllocItem(), pDynSym->pSymbol );
            ZH_TRACE( ZH_TR_INFO, ( "(zh_vmMPushField)" ) );
            pCode += sizeof( PZH_DYNSYMBOL ) + 1;
            break;
         }

         case ZH_P_MPUSHMEMVAR:
         {
            PZH_DYNSYMBOL pDynSym = ( PZH_DYNSYMBOL ) ZH_GET_PTR( pCode + 1 );
            zh_memvarGetValue( zh_stackAllocItem(), pDynSym->pSymbol );
            ZH_TRACE( ZH_TR_INFO, ( "(zh_vmMPushMemvar)" ) );
            pCode += sizeof( PZH_DYNSYMBOL ) + 1;
            break;
         }

         case ZH_P_MPUSHMEMVARREF:
         {
            PZH_DYNSYMBOL pDynSym = ( PZH_DYNSYMBOL ) ZH_GET_PTR( pCode + 1 );
            zh_memvarGetRefer( zh_stackAllocItem(), pDynSym->pSymbol );
            ZH_TRACE( ZH_TR_INFO, ( "(zh_vmMPushMemvarRef)" ) );
            pCode += sizeof( PZH_DYNSYMBOL ) + 1;
            break;
         }

         case ZH_P_MPUSHSYM:
         {
            PZH_DYNSYMBOL pDynSym = ( PZH_DYNSYMBOL ) ZH_GET_PTR( pCode + 1 );
            zh_vmPushSymbol( pDynSym->pSymbol );
            pCode += sizeof( PZH_DYNSYMBOL ) + 1;
            break;
         }

         case ZH_P_MPUSHVARIABLE:
         {
            PZH_DYNSYMBOL pDynSym = ( PZH_DYNSYMBOL ) ZH_GET_PTR( pCode + 1 );
            zh_vmPushVariable( pDynSym->pSymbol );
            pCode += sizeof( PZH_DYNSYMBOL ) + 1;
            break;
         }

         case ZH_P_MPUSHSTR:
         {
            ZH_USHORT uiSize = ZH_PCODE_MKUSHORT( &pCode[ 1 ] );

            zh_vmPushString( ( const char * ) ( pCode + 3 ), uiSize - 1 );
            pCode += 3 + uiSize;
            break;
         }

         case ZH_P_MPUSHSTRLARGE:
         {
            ZH_SIZE nSize = ZH_PCODE_MKUINT24( &pCode[ 1 ] );

            zh_vmPushString( ( const char * ) ( pCode + 3 ), nSize - 1 );
            pCode += 4 + nSize;
            break;
         }

         case ZH_P_LOCALNEARADDINT:
         {
            int iLocal = pCode[ 1 ];
            ZH_TRACE( ZH_TR_DEBUG, ( "ZH_P_LOCALNEARADDINT" ) );

            zh_vmAddInt( zh_stackLocalVariable( iLocal ),
                         ZH_PCODE_MKSHORT( &pCode[ 2 ] ) );
            pCode += 4;
            break;
         }

         case ZH_P_LOCALADDINT:
         {
            int iLocal = ZH_PCODE_MKUSHORT( &pCode[ 1 ] );
            ZH_TRACE( ZH_TR_DEBUG, ( "ZH_P_LOCALADDINT" ) );

            zh_vmAddInt( zh_stackLocalVariable( iLocal ),
                         ZH_PCODE_MKSHORT( &pCode[ 3 ] ) );
            pCode += 5;
            break;
         }

         case ZH_P_LOCALINC:
         {
            int      iLocal = ZH_PCODE_MKUSHORT( &pCode[ 1 ] );
            PZH_ITEM pLocal = zh_stackLocalVariable( iLocal );
            zh_vmInc( ZH_IS_BYREF( pLocal ) ? zh_itemUnRef( pLocal ) : pLocal );
            pCode += 3;
            break;
         }

         case ZH_P_LOCALDEC:
         {
            int iLocal = ZH_PCODE_MKUSHORT( &pCode[ 1 ] );
            PZH_ITEM pLocal = zh_stackLocalVariable( iLocal );
            zh_vmDec( ZH_IS_BYREF( pLocal ) ? zh_itemUnRef( pLocal ) : pLocal );
            pCode += 3;
            break;
         }

         case ZH_P_LOCALINCPUSH:
         {
            int iLocal = ZH_PCODE_MKUSHORT( &pCode[ 1 ] );
            PZH_ITEM pLocal = zh_stackLocalVariable( iLocal );
            if( ZH_IS_BYREF( pLocal ) )
               pLocal = zh_itemUnRef( pLocal );
            zh_vmInc( pLocal );
            zh_itemCopy( zh_stackAllocItem(), pLocal );
            pCode += 3;
            break;
         }

         /* WITH OBJECT */

         case ZH_P_WITHOBJECTMESSAGE:
         {
            PZH_ITEM pWith;
            ZH_USHORT wSymPos = ZH_PCODE_MKUSHORT( &pCode[ 1 ] );
            if( wSymPos != 0xFFFF )
            {
               /* NOTE: 0xFFFF is passed when ':&varmacro' syntax is used.
                * In this case symbol is already pushed on the stack
                * using ZH_P_MACROSYMBOL.
                */
               zh_vmPushSymbol( pSymbols + wSymPos );
            }
            pWith = zh_stackWithObjectItem();
            if( pWith )
               zh_vmPush( pWith );
            else
               zh_stackAllocItem()->type = ZH_IT_NIL;
            pCode += 3;
            break;
         }

         case ZH_P_WITHOBJECTSTART:
            zh_vmWithObjectStart();
            pCode++;
            break;

         case ZH_P_WITHOBJECTEND:
            zh_stackPop();    /* remove with object envelope */
            zh_stackPop();    /* remove implicit object */
            pCode++;
            break;

         /* misc */

         case ZH_P_NOOP:
            /* Intentionally do nothing */
            pCode++;
            break;

         default:
            /* TODO: Include to failing pcode in the error message */
            zh_errInternal( ZH_EI_VMBADOPCODE, NULL, NULL, NULL );
            break;
      }

      if( zh_stackGetActionRequest() )
      {
         if( zh_stackGetActionRequest() & ZH_ENDPROC_REQUESTED )
         {
            /* request to stop current procedure was issued
             * (from macro evaluation)
             */

            /* This code allow to use RETURN inside BEGIN/END sequence
             * or in RECOVER code when ALWAYS clause is used
             */
            if( bCanRecover )
            {
               do
               {
                  zh_stackRemove( zh_stackGetRecoverBase() );
#if defined( _ZH_RECOVER_DEBUG )
                  if( zh_stackItemFromTop( ZH_RECOVER_STATE )->type != ZH_IT_RECOVER )
                     zh_errInternal( ZH_EI_ERRUNRECOV, "ENDPROC", NULL, NULL );
#endif
                  if( zh_stackItemFromTop( ZH_RECOVER_STATE )->item.asRecover.flags & ZH_SEQ_DOALWAYS )
                     break;
                  /* Restore previous recovery state */
                  bCanRecover = ( zh_stackItemFromTop( ZH_RECOVER_STATE )->item.asRecover.flags & ZH_SEQ_CANRECOVER ) != 0;
                  zh_stackSetRecoverBase( zh_stackItemFromTop( ZH_RECOVER_STATE )->item.asRecover.base );
               }
               while( bCanRecover );

               /* ALWAYS found? */
               if( bCanRecover )
               {
#if defined( _ZH_RECOVER_DEBUG )
                  if( zh_stackItemFromTop( ZH_RECOVER_STATE )->type != ZH_IT_RECOVER )
                     zh_errInternal( ZH_EI_ERRUNRECOV, "ENDPROC ALWAYS", NULL, NULL );
#endif
                  /* reload the address of ALWAYS code */
                  pCode = zh_stackItemFromTop( ZH_RECOVER_STATE )->item.asRecover.recover;
                  /* store and reset action */
                  zh_stackItemFromTop( ZH_RECOVER_STATE )->item.asRecover.request = zh_stackGetActionRequest();
                  zh_stackSetActionRequest( 0 );
                  continue;
               }
            }
            zh_stackSetActionRequest( 0 );
            break;
         }
         else if( zh_stackGetActionRequest() & ZH_BREAK_REQUESTED )
         {
            if( bCanRecover )
            {
               /*
                * There is the BEGIN/END sequence defined in current
                * procedure/function - use it to continue opcodes execution
                */
               /*
                * remove all items placed on the stack after BEGIN code
                */
               zh_stackRemove( zh_stackGetRecoverBase() );
               /*
                * reload the address of recovery code
                */
#if defined( _ZH_RECOVER_DEBUG )
               if( zh_stackItemFromTop( ZH_RECOVER_STATE )->type != ZH_IT_RECOVER )
                  zh_errInternal( ZH_EI_ERRUNRECOV, "BREAK", NULL, NULL );
#endif
               pCode = zh_stackItemFromTop( ZH_RECOVER_STATE )->item.asRecover.recover;
               /*
                * leave the SEQUENCE envelope on the stack - it will
                * be popped either in RECOVER or END opcode
                */

               /* store and reset action */
               zh_stackItemFromTop( ZH_RECOVER_STATE )->item.asRecover.request = zh_stackGetActionRequest();
               zh_stackSetActionRequest( 0 );
            }
            else
               break;
         }
         else if( zh_stackGetActionRequest() & ZH_QUIT_REQUESTED )
         {
            if( bCanRecover )
            {
               do
               {
                  zh_stackRemove( zh_stackGetRecoverBase() );
#if defined( _ZH_RECOVER_DEBUG )
                  if( zh_stackItemFromTop( ZH_RECOVER_STATE )->type != ZH_IT_RECOVER )
                     zh_errInternal( ZH_EI_ERRUNRECOV, "QUIT", NULL, NULL );
#endif
                  if( zh_stackItemFromTop( ZH_RECOVER_STATE )->item.asRecover.flags & ZH_SEQ_DOALWAYS )
                     break;
                  /* Restore previous recovery state */
                  bCanRecover = ( zh_stackItemFromTop( ZH_RECOVER_STATE )->item.asRecover.flags & ZH_SEQ_CANRECOVER ) != 0;
                  zh_stackSetRecoverBase( zh_stackItemFromTop( ZH_RECOVER_STATE )->item.asRecover.base );
                  /* skip other steps */
               }
               while( bCanRecover );

               /* ALWAYS found? */
               if( bCanRecover )
               {
#if defined( _ZH_RECOVER_DEBUG )
                  if( zh_stackItemFromTop( ZH_RECOVER_STATE )->type != ZH_IT_RECOVER )
                     zh_errInternal( ZH_EI_ERRUNRECOV, "QUIT ALWAYS", NULL, NULL );
#endif
                  /* reload the address of ALWAYS code */
                  pCode = zh_stackItemFromTop( ZH_RECOVER_STATE )->item.asRecover.recover;
                  /* store and reset action */
                  zh_stackItemFromTop( ZH_RECOVER_STATE )->item.asRecover.request = zh_stackGetActionRequest();
                  zh_stackSetActionRequest( 0 );
                  continue;
               }
            }
            break;
         }
      }
   }
}

/* Operators (mathematical / character / misc) */

static void zh_vmAddInt( PZH_ITEM pResult, ZH_LONG lAdd )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmAddInt(%p,%ld)", ( void * ) pResult, lAdd ) );

   if( ZH_IS_BYREF( pResult ) )
   {
      pResult = zh_itemUnRef( pResult );
   }

   if( ZH_IS_NUMINT( pResult ) )
   {
      ZH_MAXINT nVal = ZH_ITEM_GET_NUMINTRAW( pResult ), nResult;

      nResult = nVal + lAdd;

      if( lAdd >= 0 ? nResult >= nVal : nResult < nVal )
      {
         ZH_ITEM_PUT_NUMINTRAW( pResult, nResult );
      }
      else
      {
         pResult->type = ZH_IT_DOUBLE;
         pResult->item.asDouble.value = ( double ) nVal + lAdd;
         pResult->item.asDouble.length = ZH_DBL_LENGTH( pResult->item.asDouble.value );
         pResult->item.asDouble.decimal = 0;
      }
   }
   else if( ZH_IS_DOUBLE( pResult ) )
   {
      pResult->item.asDouble.value += lAdd;
      pResult->item.asDouble.length = ZH_DBL_LENGTH( pResult->item.asDouble.value );
   }
   else if( ZH_IS_DATETIME( pResult ) )
   {
      pResult->type &= ~ZH_IT_DEFAULT;
      pResult->item.asDateTime.julian += lAdd;
   }
   else if( zh_objHasOperator( pResult, ZH_OO_OP_PLUS ) )
   {
      
      zh_vmPushLong( lAdd );
      zh_objOperatorCall( ZH_OO_OP_PLUS, pResult, pResult, zh_stackItemFromTop( -1 ), NULL );
      zh_stackPop();
   }
   else
   {
      
      PZH_ITEM pSubst;

      zh_vmPushLong( lAdd );
      pSubst = zh_errRT_BASE_Subst( EG_ARG, 1081, NULL, "+", 2, pResult, zh_stackItemFromTop( -1 ) );
      if( pSubst )
      {
         zh_stackPop();
         zh_itemMove( pResult, pSubst );
         zh_itemRelease( pSubst );
      }
   }
}

static void zh_vmNegate( void )
{
   
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmNegate()" ) );

   pItem = zh_stackItemFromTop( -1 );

   if( ZH_IS_INTEGER( pItem ) )
   {
#if -ZH_VMINT_MAX > ZH_VMINT_MIN
      if( pItem->item.asInteger.value < -ZH_VMINT_MAX )
      {
#if ZH_VMLONG_MAX > ZH_VMINT_MAX
         ZH_MAXINT nValue = ( ZH_MAXINT ) pItem->item.asInteger.value;
         pItem->type = ZH_IT_LONG;
         pItem->item.asLong.value = -nValue;
         pItem->item.asLong.length = ZH_LONG_EXPLENGTH( -nValue );
#else
         double dValue = ( double ) pItem->item.asInteger.value;
         pItem->type = ZH_IT_DOUBLE;
         pItem->item.asDouble.value = -dValue;
         pItem->item.asDouble.length = ZH_DBL_LENGTH( -dValue );
         pItem->item.asDouble.decimal = 0;
#endif
      }
      else
#endif
      {
         pItem->type = ZH_IT_INTEGER;
         pItem->item.asInteger.value = -pItem->item.asInteger.value;
         pItem->item.asInteger.length = ZH_INT_EXPLENGTH( pItem->item.asInteger.value );
      }
   }
   else if( ZH_IS_LONG( pItem ) )
   {
#if -ZH_VMLONG_MAX > ZH_VMLONG_MIN
      if( pItem->item.asLong.value < -ZH_VMLONG_MAX )
      {
         double dValue = ( double ) pItem->item.asLong.value;
         pItem->type = ZH_IT_DOUBLE;
         pItem->item.asDouble.value = -dValue;
         pItem->item.asDouble.length = ZH_DBL_LENGTH( -dValue );
         pItem->item.asDouble.decimal = 0;
      }
      else
#endif
      {
         pItem->type = ZH_IT_LONG;
         pItem->item.asLong.value = -pItem->item.asLong.value;
         pItem->item.asLong.length = ZH_LONG_EXPLENGTH( pItem->item.asLong.value );
      }
   }
   else if( ZH_IS_DOUBLE( pItem ) )
   {
      pItem->type = ZH_IT_DOUBLE;
      pItem->item.asDouble.value = -pItem->item.asDouble.value;
      pItem->item.asDouble.length = ZH_DBL_LENGTH( pItem->item.asDouble.value );
   }
   else
   {
      PZH_ITEM pResult = zh_errRT_BASE_Subst( EG_ARG, 1080, NULL, "-", 1, pItem );

      if( pResult )
      {
         zh_itemMove( pItem, pResult );
         zh_itemRelease( pResult );
      }
   }
}

static void zh_vmTimeStampPut( PZH_ITEM pItem, long lJulian, long lMilliSec )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmTimeStampPut(%p,%ld,%ld)", ( void * ) pItem, lJulian, lMilliSec ) );

   /* timestamp normalization */
   if( lJulian < 0 )
   {
      if( lMilliSec <= -ZH_MILLISECS_PER_DAY )
      {
         lMilliSec += ZH_MILLISECS_PER_DAY;
         --lJulian;
      }
      else if( lMilliSec > 0 )
      {
         lMilliSec -= ZH_MILLISECS_PER_DAY;
         ++lJulian;
         if( lMilliSec > 0 )
         {
            lMilliSec -= ZH_MILLISECS_PER_DAY;
            ++lJulian;
         }
      }
   }
   else
   {
      if( lMilliSec >= ZH_MILLISECS_PER_DAY )
      {
         lMilliSec -= ZH_MILLISECS_PER_DAY;
         ++lJulian;
      }
      else if( lMilliSec < 0 )
      {
         lMilliSec += ZH_MILLISECS_PER_DAY;
         --lJulian;
         if( lMilliSec < 0 )
         {
            lMilliSec += ZH_MILLISECS_PER_DAY;
            --lJulian;
         }
      }
   }

   zh_itemPutTDT( pItem, lJulian, lMilliSec );
}

static void zh_vmTimeStampAdd( PZH_ITEM pResult, PZH_ITEM pItem, double dValue )
{
   long lJulian, lMilliSec;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmTimeStampAdd(%p,%p,%lf)", ( void * ) pResult, ( void * ) pItem, dValue ) );

   zh_timeStampUnpackDT( dValue, &lJulian, &lMilliSec );

   lJulian += pItem->item.asDateTime.julian;
   lMilliSec += pItem->item.asDateTime.time;

   zh_vmTimeStampPut( pResult, lJulian, lMilliSec );
}

static void zh_vmPlus( PZH_ITEM pResult, PZH_ITEM pItem1, PZH_ITEM pItem2 )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPlus(%p,%p,%p)", ( void * ) pResult, ( void * ) pItem1, ( void * ) pItem2 ) );

   if( ZH_IS_NUMINT( pItem1 ) && ZH_IS_NUMINT( pItem2 ) )
   {
      ZH_MAXINT nNumber1 = ZH_ITEM_GET_NUMINTRAW( pItem1 );
      ZH_MAXINT nNumber2 = ZH_ITEM_GET_NUMINTRAW( pItem2 );
      ZH_MAXINT nResult = nNumber1 + nNumber2;

      if( ZH_IS_COMPLEX( pResult ) )
         zh_itemClear( pResult );

      if( nNumber2 >= 0 ? nResult >= nNumber1 : nResult < nNumber1 )
      {
         ZH_ITEM_PUT_NUMINTRAW( pResult, nResult );
      }
      else
      {
         double dResult = ( double ) nNumber1 + ( double ) nNumber2;
         pResult->type = ZH_IT_DOUBLE;
         pResult->item.asDouble.value = dResult;
         pResult->item.asDouble.length = ZH_DBL_LENGTH( dResult );
         pResult->item.asDouble.decimal = 0;
      }
   }
   else if( ZH_IS_NUMERIC( pItem1 ) && ZH_IS_NUMERIC( pItem2 ) )
   {
      int iDec1, iDec2;
      double dNumber1 = zh_itemGetNDDec( pItem1, &iDec1 );
      double dNumber2 = zh_itemGetNDDec( pItem2, &iDec2 );

      zh_itemPutNDDec( pResult, dNumber1 + dNumber2, ZH_MAX( iDec1, iDec2 ) );
   }
   else if( ZH_IS_STRING( pItem1 ) && ZH_IS_STRING( pItem2 ) )
   {
      ZH_SIZE nLen1 = pItem1->item.asString.length;
      ZH_SIZE nLen2 = pItem2->item.asString.length;

      if( nLen2 )
      {
         if( nLen1 )
         {
            if( nLen1 < ZH_SIZE_MAX - nLen2 )
            {
               if( pResult != pItem1 )
               {
                  zh_itemMove( pResult, pItem1 );
                  pItem1 = pResult;
               }
               zh_itemReSizeString( pItem1, nLen1 + nLen2 );
               zh_xmemcpy( pItem1->item.asString.value + nLen1,
                           pItem2->item.asString.value, nLen2 );
            }
            else
               zh_errRT_BASE( EG_STROVERFLOW, 1209, NULL, "+", 2, pItem1, pItem2 );
         }
         else
            zh_itemCopy( pResult, pItem2 );
      }
      else if( pResult != pItem1 )
         zh_itemCopy( pResult, pItem1 );
      pResult->type &= ~( ZH_IT_MEMOFLAG | ZH_IT_DEFAULT );
   }
   else if( ZH_IS_DATETIME( pItem1 ) && ZH_IS_DATETIME( pItem2 ) )
   {
      if( ZH_IS_TIMESTAMP( pItem1 ) || ZH_IS_TIMESTAMP( pItem2 ) )
         zh_vmTimeStampPut( pResult, pItem1->item.asDateTime.julian +
                                     pItem2->item.asDateTime.julian,
                                     pItem1->item.asDateTime.time +
                                     pItem2->item.asDateTime.time );
      else
         zh_itemPutDL( pResult, pItem1->item.asDateTime.julian +
                                pItem2->item.asDateTime.julian );
   }
   else if( ZH_IS_DATETIME( pItem1 ) && ZH_IS_NUMERIC( pItem2 ) )
   {
      if( ZH_IS_TIMESTAMP( pItem1 ) )
      {
         if( ZH_IS_NUMINT( pItem2 ) )
            zh_vmTimeStampPut( pResult, pItem1->item.asDateTime.julian +
                                        ( long ) ZH_ITEM_GET_NUMINTRAW( pItem2 ),
                                        pItem1->item.asDateTime.time );
         else
            zh_vmTimeStampAdd( pResult, pItem1, pItem2->item.asDouble.value );
      }
      else
         zh_itemPutDL( pResult, zh_itemGetDL( pItem1 ) + zh_itemGetNL( pItem2 ) );
   }
   else if( ZH_IS_NUMERIC( pItem1 ) && ZH_IS_DATETIME( pItem2 ) )
   {
      if( ZH_IS_TIMESTAMP( pItem2 ) )
      {
         if( ZH_IS_NUMINT( pItem1 ) )
            zh_vmTimeStampPut( pResult, ( long ) ZH_ITEM_GET_NUMINTRAW( pItem1 ) +
                                        pItem2->item.asDateTime.julian,
                                        pItem2->item.asDateTime.time );
         else
            zh_vmTimeStampAdd( pResult, pItem2, pItem1->item.asDouble.value );
      }
      else
         zh_itemPutDL( pResult, zh_itemGetNL( pItem1 ) + zh_itemGetDL( pItem2 ) );
   }
   else if( ! zh_objOperatorCall( ZH_OO_OP_PLUS, pResult, pItem1, pItem2, NULL ) )
   {
      PZH_ITEM pSubst = zh_errRT_BASE_Subst( EG_ARG, 1081, NULL, "+", 2, pItem1, pItem2 );

      if( pSubst )
      {
         zh_itemMove( pResult, pSubst );
         zh_itemRelease( pSubst );
      }
   }
}

static void zh_vmMinus( PZH_ITEM pResult, PZH_ITEM pItem1, PZH_ITEM pItem2 )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmMinus(%p,%p,%p)", ( void * ) pResult, ( void * ) pItem1, ( void * ) pItem2 ) );

   if( ZH_IS_NUMINT( pItem1 ) && ZH_IS_NUMINT( pItem2 ) )
   {
      ZH_MAXINT nNumber1 = ZH_ITEM_GET_NUMINTRAW( pItem1 );
      ZH_MAXINT nNumber2 = ZH_ITEM_GET_NUMINTRAW( pItem2 );
      ZH_MAXINT nResult = nNumber1 - nNumber2;

      if( ZH_IS_COMPLEX( pResult ) )
         zh_itemClear( pResult );

      if( nNumber2 <= 0 ? nResult >= nNumber1 : nResult < nNumber1 )
      {
         ZH_ITEM_PUT_NUMINTRAW( pResult, nResult );
      }
      else
      {
         double dResult = ( double ) nNumber1 - ( double ) nNumber2;
         pResult->type = ZH_IT_DOUBLE;
         pResult->item.asDouble.value = dResult;
         pResult->item.asDouble.length = ZH_DBL_LENGTH( dResult );
         pResult->item.asDouble.decimal = 0;
      }
   }
   else if( ZH_IS_NUMERIC( pItem1 ) && ZH_IS_NUMERIC( pItem2 ) )
   {
      int iDec1, iDec2;
      double dNumber1 = zh_itemGetNDDec( pItem1, &iDec1 );
      double dNumber2 = zh_itemGetNDDec( pItem2, &iDec2 );

      zh_itemPutNDDec( pResult, dNumber1 - dNumber2, ZH_MAX( iDec1, iDec2 ) );
   }
   else if( ZH_IS_DATETIME( pItem1 ) && ZH_IS_DATETIME( pItem2 ) )
   {
      long lTime = pItem1->item.asDateTime.time -
                   pItem2->item.asDateTime.time,
           lJulian = pItem1->item.asDateTime.julian -
                     pItem2->item.asDateTime.julian;
      if( lTime != 0 )
         zh_itemPutNDDec( pResult, zh_timeStampPackDT( lJulian, lTime ), ZH_TIMEDIFF_DEC );
      else
      {
         if( ZH_IS_COMPLEX( pResult ) )
            zh_itemClear( pResult );
         ZH_ITEM_PUT_LONGRAW( pResult, lJulian );
      }
   }
   else if( ZH_IS_DATETIME( pItem1 ) && ZH_IS_NUMERIC( pItem2 ) )
   {
      if( ZH_IS_TIMESTAMP( pItem1 ) )
      {
         if( ZH_IS_NUMINT( pItem2 ) )
            zh_vmTimeStampPut( pResult, pItem1->item.asDateTime.julian -
                                        ( long ) ZH_ITEM_GET_NUMINTRAW( pItem2 ),
                                        pItem1->item.asDateTime.time );
         else
            zh_vmTimeStampAdd( pResult, pItem1, - pItem2->item.asDouble.value );
      }
      else
         zh_itemPutDL( pResult, zh_itemGetDL( pItem1 ) - zh_itemGetNL( pItem2 ) );
   }
   else if( ZH_IS_STRING( pItem1 ) && ZH_IS_STRING( pItem2 ) )
   {
      ZH_SIZE nLen1 = pItem1->item.asString.length;
      ZH_SIZE nLen2 = pItem2->item.asString.length;

      if( nLen1 == 0 )
      {
         zh_itemCopy( pResult, pItem2 );
         pResult->type &= ~( ZH_IT_MEMOFLAG | ZH_IT_DEFAULT );
      }
      else if( nLen2 == 0 )
      {
         if( pResult != pItem1 )
            zh_itemCopy( pResult, pItem1 );
         pResult->type &= ~( ZH_IT_MEMOFLAG | ZH_IT_DEFAULT );
      }
      else if( nLen1 < ZH_SIZE_MAX - nLen2 )
      {
         if( pResult != pItem1 )
         {
            zh_itemMove( pResult, pItem1 );
            pItem1 = pResult;
         }
         zh_itemReSizeString( pItem1, nLen1 + nLen2 );
         while( nLen1 && pItem1->item.asString.value[ nLen1 - 1 ] == ' ' )
            nLen1--;
         zh_xmemcpy( pItem1->item.asString.value + nLen1,
                     pItem2->item.asString.value, nLen2 );
         zh_xmemset( pItem1->item.asString.value + nLen1 + nLen2, ' ',
                     pItem1->item.asString.length - nLen1 - nLen2 );
      }
      else
         zh_errRT_BASE( EG_STROVERFLOW, 1210, NULL, "-", 2, pItem1, pItem2 );
   }
   else if( ! zh_objOperatorCall( ZH_OO_OP_MINUS, pResult, pItem1, pItem2, NULL ) )
   {
      PZH_ITEM pSubst = zh_errRT_BASE_Subst( EG_ARG, 1082, NULL, "-", 2, pItem1, pItem2 );

      if( pSubst )
      {
         zh_itemMove( pResult, pSubst );
         zh_itemRelease( pSubst );
      }
   }
}

static void zh_vmMult( PZH_ITEM pResult, PZH_ITEM pItem1, PZH_ITEM pItem2 )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmMult(%p,%p,%p)", ( void * ) pResult, ( void * ) pItem1, ( void * ) pItem2 ) );

#if -( ZH_VMLONG_MAX / ZH_VMINT_MIN ) >= ZH_VMINT_MAX && 1
   if( ZH_IS_INTEGER( pItem1 ) && ZH_IS_INTEGER( pItem2 ) )
   {
      ZH_MAXINT nResult = ( ZH_MAXINT ) pItem1->item.asInteger.value *
                          ( ZH_MAXINT ) pItem2->item.asInteger.value;
      if( ZH_IS_COMPLEX( pResult ) )
         zh_itemClear( pResult );
      ZH_ITEM_PUT_NUMINTRAW( pResult, nResult );
   }
   else
#endif
   if( ZH_IS_NUMERIC( pItem1 ) && ZH_IS_NUMERIC( pItem2 ) )
   {
      int iDec1, iDec2;
      double dNumber1 = zh_itemGetNDDec( pItem1, &iDec1 );
      double dNumber2 = zh_itemGetNDDec( pItem2, &iDec2 );

      zh_itemPutNumType( pResult, dNumber1 * dNumber2, iDec1 + iDec2,
                         ZH_ITEM_TYPERAW( pItem1 ), ZH_ITEM_TYPERAW( pItem2 ) );
   }
   else if( ! zh_objOperatorCall( ZH_OO_OP_MULT, pResult, pItem1, pItem2, NULL ) )
   {
      PZH_ITEM pSubst = zh_errRT_BASE_Subst( EG_ARG, 1083, NULL, "*", 2, pItem1, pItem2 );

      if( pSubst )
      {
         zh_itemMove( pResult, pSubst );
         zh_itemRelease( pSubst );
      }
   }
}

static void zh_vmDivide( PZH_ITEM pResult, PZH_ITEM pItem1, PZH_ITEM pItem2 )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmDivide(%p,%p,%p)", ( void * ) pResult, ( void * ) pItem1, ( void * ) pItem2 ) );

   if( ZH_IS_NUMINT( pItem1 ) && ZH_IS_NUMINT( pItem2 ) )
   {
      ZH_MAXINT nDivisor = ZH_ITEM_GET_NUMINTRAW( pItem2 );

      if( nDivisor == 0 )
      {
         PZH_ITEM pSubst = zh_errRT_BASE_Subst( EG_ZERODIV, 1340, NULL, "/", 2, pItem1, pItem2 );

         if( pSubst )
         {
            zh_itemMove( pResult, pSubst );
            zh_itemRelease( pSubst );
         }
      }
      else
      {
         ZH_MAXINT nNumber1 = ZH_ITEM_GET_NUMINTRAW( pItem1 );
         zh_itemPutND( pResult, ( double ) nNumber1 / ( double ) nDivisor );
      }
   }
   else if( ZH_IS_NUMERIC( pItem1 ) && ZH_IS_NUMERIC( pItem2 ) )
   {
      double dDivisor = zh_itemGetND( pItem2 );

      if( dDivisor == 0.0 )
      {
         PZH_ITEM pSubst = zh_errRT_BASE_Subst( EG_ZERODIV, 1340, NULL, "/", 2, pItem1, pItem2 );

         if( pSubst )
         {
            zh_itemMove( pResult, pSubst );
            zh_itemRelease( pSubst );
         }
      }
      else
      {
         zh_itemPutND( pResult, zh_itemGetND( pItem1 ) / dDivisor );
      }
   }
   else if( ! zh_objOperatorCall( ZH_OO_OP_DIVIDE, pResult, pItem1, pItem2, NULL ) )
   {
      PZH_ITEM pSubst = zh_errRT_BASE_Subst( EG_ARG, 1084, NULL, "/", 2, pItem1, pItem2 );

      if( pSubst )
      {
         zh_itemMove( pResult, pSubst );
         zh_itemRelease( pSubst );
      }
   }
}

static void zh_vmModulus( PZH_ITEM pResult, PZH_ITEM pItem1, PZH_ITEM pItem2 )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmModulus(%p,%p,%p)", ( void * ) pResult, ( void * ) pItem1, ( void * ) pItem2 ) );

   if( ZH_IS_NUMINT( pItem1 ) && ZH_IS_NUMINT( pItem2 ) )
   {
      ZH_MAXINT nDivisor = ZH_ITEM_GET_NUMINTRAW( pItem2 );

      if( nDivisor == 0 )
      {
         PZH_ITEM pSubst = zh_errRT_BASE_Subst( EG_ZERODIV, 1341, NULL, "%", 2, pItem1, pItem2 );

         if( pSubst )
         {
            zh_itemMove( pResult, pSubst );
            zh_itemRelease( pSubst );
         }
      }
      else
      {
         zh_itemPutND( pResult, ( double ) ( ZH_ITEM_GET_NUMINTRAW( pItem1 ) % nDivisor ) );
      }
   }
   else if( ZH_IS_NUMERIC( pItem1 ) && ZH_IS_NUMERIC( pItem2 ) )
   {
      double dDivisor = zh_itemGetND( pItem2 );

      if( dDivisor == 0.0 )
      {
         PZH_ITEM pSubst = zh_errRT_BASE_Subst( EG_ZERODIV, 1341, NULL, "%", 2, pItem1, pItem2 );

         if( pSubst )
         {
            zh_itemMove( pResult, pSubst );
            zh_itemRelease( pSubst );
         }
      }
      else
      {
         zh_itemPutND( pResult, fmod( zh_itemGetND( pItem1 ), dDivisor ) );
      }
   }
   else if( ! zh_objOperatorCall( ZH_OO_OP_MOD, pResult, pItem1, pItem2, NULL ) )
   {
      PZH_ITEM pSubst = zh_errRT_BASE_Subst( EG_ARG, 1085, NULL, "%", 2, pItem1, pItem2 );

      if( pSubst )
      {
         zh_itemMove( pResult, pSubst );
         zh_itemRelease( pSubst );
      }
   }
}

static void zh_vmPower( PZH_ITEM pResult, PZH_ITEM pItem1, PZH_ITEM pItem2 )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPower(%p,%p,%p)", ( void * ) pResult, ( void * ) pItem1, ( void * ) pItem2 ) );

   if( ZH_IS_NUMERIC( pItem1 ) && ZH_IS_NUMERIC( pItem2 ) )
   {
      zh_itemPutND( pResult, pow( zh_itemGetND( pItem1 ), zh_itemGetND( pItem2 ) ) );
   }
   else if( ! zh_objOperatorCall( ZH_OO_OP_POWER, pResult, pItem1, pItem2, NULL ) )
   {
      PZH_ITEM pSubst = zh_errRT_BASE_Subst( EG_ARG, 1088, NULL, "^", 2, pItem1, pItem2 );

      if( pSubst )
      {
         zh_itemMove( pResult, pSubst );
         zh_itemRelease( pSubst );
      }
   }
}

static void zh_vmInc( PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmInc(%p)", ( void * ) pItem ) );

   if( ZH_IS_NUMINT( pItem ) )
   {
      if( ZH_IS_INTEGER( pItem ) )
      {
         if( pItem->item.asInteger.value < ZH_VMINT_MAX )
         {
            pItem->type = ZH_IT_INTEGER;
            pItem->item.asInteger.value++;
            pItem->item.asInteger.length = ZH_INT_EXPLENGTH( pItem->item.asInteger.value );
         }
         else
         {
#if ZH_VMINT_MAX < ZH_VMLONG_MAX
            pItem->type = ZH_IT_LONG;
            pItem->item.asLong.value = ( ZH_MAXINT ) pItem->item.asInteger.value + 1;
            pItem->item.asLong.length = ZH_LONG_EXPLENGTH( pItem->item.asLong.value );
#else
            pItem->type = ZH_IT_DOUBLE;
            pItem->item.asDouble.value = ( double ) pItem->item.asInteger.value + 1;
            pItem->item.asDouble.length = ZH_DBL_LENGTH( pItem->item.asDouble.value );
            pItem->item.asDouble.decimal = 0;
#endif
         }
      }
      else if( pItem->item.asLong.value < ZH_VMLONG_MAX )
      {
         pItem->type = ZH_IT_LONG;
         pItem->item.asLong.value++;
         pItem->item.asLong.length = ZH_LONG_EXPLENGTH( pItem->item.asLong.value );
      }
      else
      {
         pItem->type = ZH_IT_DOUBLE;
         pItem->item.asDouble.value = ( double ) pItem->item.asLong.value + 1;
         pItem->item.asDouble.length = ZH_DBL_LENGTH( pItem->item.asDouble.value );
         pItem->item.asDouble.decimal = 0;
      }
   }
   else if( ZH_IS_DOUBLE( pItem ) )
   {
      pItem->type = ZH_IT_DOUBLE;
      pItem->item.asDouble.value++;
      pItem->item.asDouble.length = ZH_DBL_LENGTH( pItem->item.asDouble.value );
   }
   else if( ZH_IS_DATETIME( pItem ) )
   {
      pItem->type &= ~ZH_IT_DEFAULT;
      pItem->item.asDateTime.julian++;
   }
   else if( ! zh_objOperatorCall( ZH_OO_OP_INC, pItem, pItem, NULL, NULL ) )
   {
      PZH_ITEM pResult = zh_errRT_BASE_Subst( EG_ARG, 1086, NULL, "++", 1, pItem );

      if( pResult )
      {
         zh_itemMove( pItem, pResult );
         zh_itemRelease( pResult );
      }
   }
}

static void zh_vmDec( PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmDec(%p)", ( void * ) pItem ) );

   if( ZH_IS_NUMINT( pItem ) )
   {
      if( ZH_IS_INTEGER( pItem ) )
      {
         if( pItem->item.asInteger.value > ZH_VMINT_MIN )
         {
            pItem->type = ZH_IT_INTEGER;
            pItem->item.asInteger.value--;
            pItem->item.asInteger.length = ZH_INT_EXPLENGTH( pItem->item.asInteger.value );
         }
         else
         {
#if ZH_VMINT_MIN > ZH_VMLONG_MIN
            pItem->type = ZH_IT_LONG;
            pItem->item.asLong.value = ( ZH_MAXINT ) pItem->item.asInteger.value - 1;
            pItem->item.asLong.length = ZH_LONG_EXPLENGTH( pItem->item.asLong.value );
#else
            pItem->type = ZH_IT_DOUBLE;
            pItem->item.asDouble.value = ( double ) pItem->item.asInteger.value - 1;
            pItem->item.asDouble.length = ZH_DBL_LENGTH( pItem->item.asDouble.value );
            pItem->item.asDouble.decimal = 0;
#endif
         }
      }
      else if( pItem->item.asLong.value > ZH_VMLONG_MIN )
      {
         pItem->type = ZH_IT_LONG;
         pItem->item.asLong.value--;
         pItem->item.asLong.length = ZH_LONG_EXPLENGTH( pItem->item.asLong.value );
      }
      else
      {
         pItem->type = ZH_IT_DOUBLE;
         pItem->item.asDouble.value = ( double ) pItem->item.asLong.value - 1;
         pItem->item.asDouble.length = ZH_DBL_LENGTH( pItem->item.asDouble.value );
         pItem->item.asDouble.decimal = 0;
      }
   }
   else if( ZH_IS_DOUBLE( pItem ) )
   {
      pItem->type = ZH_IT_DOUBLE;
      pItem->item.asDouble.value--;
      pItem->item.asDouble.length = ZH_DBL_LENGTH( pItem->item.asDouble.value );
   }
   else if( ZH_IS_DATETIME( pItem ) )
   {
      pItem->type &= ~ZH_IT_DEFAULT;
      pItem->item.asDateTime.julian--;
   }
   else if( ! zh_objOperatorCall( ZH_OO_OP_DEC, pItem, pItem, NULL, NULL ) )
   {
      PZH_ITEM pResult = zh_errRT_BASE_Subst( EG_ARG, 1087, NULL, "--", 1, pItem );

      if( pResult )
      {
         zh_itemMove( pItem, pResult );
         zh_itemRelease( pResult );
      }
   }
}

static void zh_vmFuncPtr( void )  /* pushes a function address pointer. Removes the symbol from the stack */
{
   
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmFuncPtr()" ) );

   pItem = zh_stackItemFromTop( -1 );

   if( ZH_IS_SYMBOL( pItem ) )
   {
      /* do nothing - now we are using ZH_IT_SYMBOL */
#if 0
      zh_stackPop();
      zh_vmPushPointer( ( void * ) pItem->item.asSymbol.value->value.pFunPtr );
#endif
   }
   else
      zh_errInternal( ZH_EI_VMNOTSYMBOL, NULL, "zh_vmFuncPtr()", NULL );
}

/* ------------------------------- */
/* Operators (relational)          */
/* ------------------------------- */

static void zh_vmExactlyEqual( void )
{
   
   PZH_ITEM pItem2;
   PZH_ITEM pItem1;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmExactlyEqual()" ) );

   pItem2 = zh_stackItemFromTop( -1 );
   pItem1 = zh_stackItemFromTop( -2 );

   if( ZH_IS_NIL( pItem1 ) )
   {
      /* pItem1 is NIL so this is safe */
      pItem1->type = ZH_IT_LOGICAL;
      pItem1->item.asLogical.value = ZH_IS_NIL( pItem2 );
      zh_stackPop();    /* clear the pItem2 */
   }
   else if( ZH_IS_NIL( pItem2 ) )
   {
      zh_stackDec();    /* pItem2 is already NIL */
      if( ZH_IS_COMPLEX( pItem1 ) )
         zh_itemClear( pItem1 );
      pItem1->type = ZH_IT_LOGICAL;
      pItem1->item.asLogical.value = ZH_FALSE;
   }
   else if( ZH_IS_STRING( pItem1 ) && ZH_IS_STRING( pItem2 ) )
   {
      ZH_BOOL fResult = pItem1->item.asString.length == pItem2->item.asString.length &&
                        ( pItem1->item.asString.value == pItem2->item.asString.value ||
                          memcmp( pItem1->item.asString.value,
                                  pItem2->item.asString.value,
                                  pItem1->item.asString.length ) == 0 );
      zh_stackPop();
      zh_itemClear( pItem1 );
      pItem1->type = ZH_IT_LOGICAL;
      pItem1->item.asLogical.value = fResult;
   }
   else if( ZH_IS_NUMINT( pItem1 ) && ZH_IS_NUMINT( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( ZH_ITEM_GET_NUMINTRAW( pItem1 ) ==
                                       ZH_ITEM_GET_NUMINTRAW( pItem2 ) );
      pItem1->type = ZH_IT_LOGICAL;
      zh_stackDec();
   }
   else if( ZH_IS_NUMERIC( pItem1 ) && ZH_IS_NUMERIC( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( ZH_ITEM_GET_NUMDBLRAW( pItem1 ) ==
                                       ZH_ITEM_GET_NUMDBLRAW( pItem2 ) );
      pItem1->type = ZH_IT_LOGICAL;
      zh_stackDec();
   }
   else if( ZH_IS_DATETIME( pItem1 ) && ZH_IS_DATETIME( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( pItem1->item.asDateTime.julian ==
                                       pItem2->item.asDateTime.julian &&
                                       pItem1->item.asDateTime.time ==
                                       pItem2->item.asDateTime.time );
      pItem1->type = ZH_IT_LOGICAL;
      zh_stackDec();
   }
   else if( ZH_IS_LOGICAL( pItem1 ) && ZH_IS_LOGICAL( pItem2 ) )
   {
      pItem1->item.asLogical.value = pItem1->item.asLogical.value ?
                                     pItem2->item.asLogical.value :
                                     ! pItem2->item.asLogical.value;
      zh_stackDec();
   }
   else if( ZH_IS_POINTER( pItem1 ) && ZH_IS_POINTER( pItem2 ) )
   {
      ZH_BOOL fResult = pItem1->item.asPointer.value == pItem2->item.asPointer.value;

      zh_stackPop();
      zh_itemClear( pItem1 );
      pItem1->type = ZH_IT_LOGICAL;
      pItem1->item.asLogical.value = fResult;
   }
   else if( ZH_IS_HASH( pItem1 ) && ZH_IS_HASH( pItem2 ) )
   {
      ZH_BOOL fResult = pItem1->item.asHash.value == pItem2->item.asHash.value;

      zh_stackPop();
      zh_itemClear( pItem1 );
      pItem1->type = ZH_IT_LOGICAL;
      pItem1->item.asLogical.value = fResult;
   }
   else if( ZH_IS_BLOCK( pItem1 ) && ZH_IS_BLOCK( pItem2 ) )
   {
      ZH_BOOL fResult = pItem1->item.asBlock.value == pItem2->item.asBlock.value;

      zh_stackPop();
      zh_itemClear( pItem1 );
      pItem1->type = ZH_IT_LOGICAL;
      pItem1->item.asLogical.value = fResult;
   }
   else if( ZH_IS_SYMBOL( pItem1 ) && ZH_IS_SYMBOL( pItem2 ) )
   {
      pItem1->item.asLogical.value =
               pItem1->item.asSymbol.value == pItem2->item.asSymbol.value ||
               ( pItem1->item.asSymbol.value->pDynSym != NULL &&
                 pItem1->item.asSymbol.value->pDynSym ==
                 pItem2->item.asSymbol.value->pDynSym );
      pItem1->type = ZH_IT_LOGICAL;
      zh_stackDec();
   }
   else if( ZH_IS_ARRAY( pItem1 ) && ZH_IS_ARRAY( pItem2 ) &&
            ! zh_objHasOperator( pItem1, ZH_OO_OP_EXACTEQUAL ) )
   {
      ZH_BOOL fResult = pItem1->item.asArray.value == pItem2->item.asArray.value;

      zh_stackPop();
      zh_itemClear( pItem1 );
      pItem1->type = ZH_IT_LOGICAL;
      pItem1->item.asLogical.value = fResult;
   }
   else if( zh_objOperatorCall( ZH_OO_OP_EXACTEQUAL, pItem1, pItem1, pItem2, NULL ) )
      zh_stackPop();
   else
   {
      PZH_ITEM pResult = zh_errRT_BASE_Subst( EG_ARG, 1070, NULL, "==", 2, pItem1, pItem2 );
      if( pResult )
      {
         zh_stackPop();
         zh_itemMove( pItem1, pResult );
         zh_itemRelease( pResult );
      }
   }
}

static void zh_vmEqual( void )
{
   
   PZH_ITEM pItem2;
   PZH_ITEM pItem1;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmEqual()" ) );

   pItem2 = zh_stackItemFromTop( -1 );
   pItem1 = zh_stackItemFromTop( -2 );

   if( ZH_IS_NIL( pItem1 ) )
   {
      /* pItem1 is NIL so this is safe */
      pItem1->type = ZH_IT_LOGICAL;
      pItem1->item.asLogical.value = ZH_IS_NIL( pItem2 );
      zh_stackPop();    /* clear the pItem2 */
   }
   else if( ZH_IS_NIL( pItem2 ) )
   {
      zh_stackDec();    /* pItem2 is already NIL */
      if( ZH_IS_COMPLEX( pItem1 ) )
         zh_itemClear( pItem1 );
      pItem1->type = ZH_IT_LOGICAL;
      pItem1->item.asLogical.value = ZH_FALSE;
   }
   else if( ZH_IS_STRING( pItem1 ) && ZH_IS_STRING( pItem2 ) )
   {
      ZH_BOOL fResult = zh_itemStrCmp( pItem1, pItem2, ZH_FALSE ) == 0;
      zh_stackPop();
      zh_itemClear( pItem1 );
      pItem1->type = ZH_IT_LOGICAL;
      pItem1->item.asLogical.value = fResult;
   }
   else if( ZH_IS_NUMINT( pItem1 ) && ZH_IS_NUMINT( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( ZH_ITEM_GET_NUMINTRAW( pItem1 ) ==
                                       ZH_ITEM_GET_NUMINTRAW( pItem2 ) );
      pItem1->type = ZH_IT_LOGICAL;
      zh_stackDec();
   }
   else if( ZH_IS_NUMERIC( pItem1 ) && ZH_IS_NUMERIC( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( ZH_ITEM_GET_NUMDBLRAW( pItem1 ) ==
                                       ZH_ITEM_GET_NUMDBLRAW( pItem2 ) );
      pItem1->type = ZH_IT_LOGICAL;
      zh_stackDec();
   }
   else if( ZH_IS_DATETIME( pItem1 ) && ZH_IS_DATETIME( pItem2 ) )
   {
      if( ZH_IS_TIMESTAMP( pItem1 ) && ZH_IS_TIMESTAMP( pItem2 ) )
         pItem1->item.asLogical.value = ( pItem1->item.asDateTime.julian ==
                                          pItem2->item.asDateTime.julian ) &&
                                        ( pItem1->item.asDateTime.time ==
                                          pItem2->item.asDateTime.time );
      else
         pItem1->item.asLogical.value = ( pItem1->item.asDateTime.julian ==
                                          pItem2->item.asDateTime.julian );
      pItem1->type = ZH_IT_LOGICAL;
      zh_stackDec();
   }
   else if( ZH_IS_LOGICAL( pItem1 ) && ZH_IS_LOGICAL( pItem2 ) )
   {
      pItem1->item.asLogical.value = pItem1->item.asLogical.value ?
                                     pItem2->item.asLogical.value :
                                     ! pItem2->item.asLogical.value;
      zh_stackDec();
   }
   else if( ZH_IS_POINTER( pItem1 ) && ZH_IS_POINTER( pItem2 ) )
   {
      ZH_BOOL fResult = pItem1->item.asPointer.value == pItem2->item.asPointer.value;
      zh_stackPop();
      zh_itemClear( pItem1 );
      pItem1->type = ZH_IT_LOGICAL;
      pItem1->item.asLogical.value = fResult;
   }
#if 0
   else if( ZH_IS_HASH( pItem1 ) && ZH_IS_HASH( pItem2 ) )
   {
      ZH_BOOL fResult = pItem1->item.asHash.value == pItem2->item.asHash.value;
      zh_stackPop();
      zh_itemClear( pItem1 );
      pItem1->type = ZH_IT_LOGICAL;
      pItem1->item.asLogical.value = fResult;
   }
#endif
   else if( zh_objOperatorCall( ZH_OO_OP_EQUAL, pItem1, pItem1, pItem2, NULL ) )
      zh_stackPop();
   else
   {
      PZH_ITEM pResult = zh_errRT_BASE_Subst( EG_ARG, 1071, NULL, "=", 2, pItem1, pItem2 );
      if( pResult )
      {
         zh_stackPop();
         zh_itemMove( pItem1, pResult );
         zh_itemRelease( pResult );
      }
   }
}

static void zh_vmNotEqual( void )
{
   
   PZH_ITEM pItem2;
   PZH_ITEM pItem1;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmNotEqual()" ) );

   pItem2 = zh_stackItemFromTop( -1 );
   pItem1 = zh_stackItemFromTop( -2 );

   if( ZH_IS_NIL( pItem1 ) )
   {
      /* pItem1 is NIL so this is safe */
      pItem1->type = ZH_IT_LOGICAL;
      pItem1->item.asLogical.value = ! ZH_IS_NIL( pItem2 );
      zh_stackPop();    /* clear the pItem2 */
   }
   else if( ZH_IS_NIL( pItem2 ) )
   {
      zh_stackDec();    /* pItem2 is already NIL */
      if( ZH_IS_COMPLEX( pItem1 ) )
         zh_itemClear( pItem1 );
      pItem1->type = ZH_IT_LOGICAL;
      pItem1->item.asLogical.value = ZH_TRUE;
   }
   else if( ZH_IS_STRING( pItem1 ) && ZH_IS_STRING( pItem2 ) )
   {
      int i = zh_itemStrCmp( pItem1, pItem2, ZH_FALSE );
      zh_stackPop();
      zh_itemClear( pItem1 );
      pItem1->type = ZH_IT_LOGICAL;
      pItem1->item.asLogical.value = i != 0;
   }
   else if( ZH_IS_NUMINT( pItem1 ) && ZH_IS_NUMINT( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( ZH_ITEM_GET_NUMINTRAW( pItem1 ) !=
                                       ZH_ITEM_GET_NUMINTRAW( pItem2 ) );
      pItem1->type = ZH_IT_LOGICAL;
      zh_stackDec();
   }
   else if( ZH_IS_NUMERIC( pItem1 ) && ZH_IS_NUMERIC( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( ZH_ITEM_GET_NUMDBLRAW( pItem1 ) !=
                                       ZH_ITEM_GET_NUMDBLRAW( pItem2 ) );
      pItem1->type = ZH_IT_LOGICAL;
      zh_stackDec();
   }
   else if( ZH_IS_DATETIME( pItem1 ) && ZH_IS_DATETIME( pItem2 ) )
   {
      if( ZH_IS_TIMESTAMP( pItem1 ) && ZH_IS_TIMESTAMP( pItem2 ) )
         pItem1->item.asLogical.value = ( pItem1->item.asDateTime.julian !=
                                          pItem2->item.asDateTime.julian ) ||
                                        ( pItem1->item.asDateTime.time !=
                                          pItem2->item.asDateTime.time );
      else
         pItem1->item.asLogical.value = ( pItem1->item.asDateTime.julian !=
                                          pItem2->item.asDateTime.julian );
      pItem1->type = ZH_IT_LOGICAL;
      zh_stackDec();
   }
   else if( ZH_IS_LOGICAL( pItem1 ) && ZH_IS_LOGICAL( pItem2 ) )
   {
      pItem1->item.asLogical.value = pItem1->item.asLogical.value ?
                                     ! pItem2->item.asLogical.value :
                                     pItem2->item.asLogical.value;
      zh_stackDec();
   }
   else if( ZH_IS_POINTER( pItem1 ) && ZH_IS_POINTER( pItem2 ) )
   {
      ZH_BOOL fResult = pItem1->item.asPointer.value !=
                        pItem2->item.asPointer.value;
      zh_stackPop();
      zh_itemClear( pItem1 );
      pItem1->type = ZH_IT_LOGICAL;
      pItem1->item.asLogical.value = fResult;
   }
#if 0
   else if( ZH_IS_HASH( pItem1 ) && ZH_IS_HASH( pItem2 ) )
   {
      ZH_BOOL fResult = pItem1->item.asHash.value != pItem2->item.asHash.value;
      zh_stackPop();
      zh_itemClear( pItem1 );
      pItem1->type = ZH_IT_LOGICAL;
      pItem1->item.asLogical.value = fResult;
   }
#endif
   else if( zh_objOperatorCall( ZH_OO_OP_NOTEQUAL, pItem1, pItem1, pItem2, NULL ) )
      zh_stackPop();
   else
   {
      PZH_ITEM pResult = zh_errRT_BASE_Subst( EG_ARG, 1072, NULL, "<>", 2, pItem1, pItem2 );

      if( pResult )
      {
         zh_stackPop();
         zh_itemMove( pItem1, pResult );
         zh_itemRelease( pResult );
      }
   }
}

static void zh_vmLess( void )
{
   
   PZH_ITEM pItem2;
   PZH_ITEM pItem1;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmLess()" ) );

   pItem2 = zh_stackItemFromTop( -1 );
   pItem1 = zh_stackItemFromTop( -2 );

   if( ZH_IS_STRING( pItem1 ) && ZH_IS_STRING( pItem2 ) )
   {
      int i = zh_itemStrCmp( pItem1, pItem2, ZH_FALSE );
      zh_stackPop();
      zh_itemClear( pItem1 );
      pItem1->type = ZH_IT_LOGICAL;
      pItem1->item.asLogical.value = i < 0;
   }
   else if( ZH_IS_NUMINT( pItem1 ) && ZH_IS_NUMINT( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( ZH_ITEM_GET_NUMINTRAW( pItem1 ) <
                                       ZH_ITEM_GET_NUMINTRAW( pItem2 ) );
      pItem1->type = ZH_IT_LOGICAL;
      zh_stackDec();
   }
   else if( ZH_IS_NUMERIC( pItem1 ) && ZH_IS_NUMERIC( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( ZH_ITEM_GET_NUMDBLRAW( pItem1 ) <
                                       ZH_ITEM_GET_NUMDBLRAW( pItem2 ) );
      pItem1->type = ZH_IT_LOGICAL;
      zh_stackDec();
   }
   else if( ZH_IS_DATETIME( pItem1 ) && ZH_IS_DATETIME( pItem2 ) )
   {
      if( ZH_IS_TIMESTAMP( pItem1 ) && ZH_IS_TIMESTAMP( pItem2 ) )
         pItem1->item.asLogical.value = ( pItem1->item.asDateTime.julian <
                                          pItem2->item.asDateTime.julian ) ||
                                        ( pItem1->item.asDateTime.julian ==
                                          pItem2->item.asDateTime.julian &&
                                          pItem1->item.asDateTime.time <
                                          pItem2->item.asDateTime.time );
      else
         pItem1->item.asLogical.value = ( pItem1->item.asDateTime.julian <
                                          pItem2->item.asDateTime.julian );
      pItem1->type = ZH_IT_LOGICAL;
      zh_stackDec();
   }
   else if( ZH_IS_LOGICAL( pItem1 ) && ZH_IS_LOGICAL( pItem2 ) )
   {
      pItem1->item.asLogical.value = ! pItem1->item.asLogical.value &&
                                     pItem2->item.asLogical.value;
      zh_stackDec();
   }
   else if( zh_objOperatorCall( ZH_OO_OP_LESS, pItem1, pItem1, pItem2, NULL ) )
      zh_stackPop();

   else
   {
      PZH_ITEM pResult = zh_errRT_BASE_Subst( EG_ARG, 1073, NULL, "<", 2, pItem1, pItem2 );

      if( pResult )
      {
         zh_stackPop();
         zh_itemMove( pItem1, pResult );
         zh_itemRelease( pResult );
      }
   }
}

static void zh_vmLessEqual( void )
{
   
   PZH_ITEM pItem2;
   PZH_ITEM pItem1;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmLessEqual()" ) );

   pItem2 = zh_stackItemFromTop( -1 );
   pItem1 = zh_stackItemFromTop( -2 );

   if( ZH_IS_STRING( pItem1 ) && ZH_IS_STRING( pItem2 ) )
   {
      int i = zh_itemStrCmp( pItem1, pItem2, ZH_FALSE );
      zh_stackPop();
      zh_itemClear( pItem1 );
      pItem1->type = ZH_IT_LOGICAL;
      pItem1->item.asLogical.value = i <= 0;
   }
   else if( ZH_IS_NUMINT( pItem1 ) && ZH_IS_NUMINT( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( ZH_ITEM_GET_NUMINTRAW( pItem1 ) <=
                                       ZH_ITEM_GET_NUMINTRAW( pItem2 ) );
      pItem1->type = ZH_IT_LOGICAL;
      zh_stackDec();
   }
   else if( ZH_IS_NUMERIC( pItem1 ) && ZH_IS_NUMERIC( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( ZH_ITEM_GET_NUMDBLRAW( pItem1 ) <=
                                       ZH_ITEM_GET_NUMDBLRAW( pItem2 ) );
      pItem1->type = ZH_IT_LOGICAL;
      zh_stackDec();
   }
   else if( ZH_IS_DATETIME( pItem1 ) && ZH_IS_DATETIME( pItem2 ) )
   {
      if( ZH_IS_TIMESTAMP( pItem1 ) && ZH_IS_TIMESTAMP( pItem2 ) )
         pItem1->item.asLogical.value = ( pItem1->item.asDateTime.julian <
                                          pItem2->item.asDateTime.julian ) ||
                                        ( pItem1->item.asDateTime.julian ==
                                          pItem2->item.asDateTime.julian &&
                                          pItem1->item.asDateTime.time <=
                                          pItem2->item.asDateTime.time );
      else
         pItem1->item.asLogical.value = ( pItem1->item.asDateTime.julian <=
                                          pItem2->item.asDateTime.julian );
      pItem1->type = ZH_IT_LOGICAL;
      zh_stackDec();
   }
   else if( ZH_IS_LOGICAL( pItem1 ) && ZH_IS_LOGICAL( pItem2 ) )
   {
      pItem1->item.asLogical.value = ! pItem1->item.asLogical.value ||
                                     pItem2->item.asLogical.value;
      zh_stackDec();
   }
   else if( zh_objOperatorCall( ZH_OO_OP_LESSEQUAL, pItem1, pItem1, pItem2, NULL ) )
      zh_stackPop();

   else
   {
      PZH_ITEM pResult = zh_errRT_BASE_Subst( EG_ARG, 1074, NULL, "<=", 2, pItem1, pItem2 );

      if( pResult )
      {
         zh_stackPop();
         zh_itemMove( pItem1, pResult );
         zh_itemRelease( pResult );
      }
   }
}

static void zh_vmGreater( void )
{
   
   PZH_ITEM pItem2;
   PZH_ITEM pItem1;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmGreater()" ) );

   pItem2 = zh_stackItemFromTop( -1 );
   pItem1 = zh_stackItemFromTop( -2 );

   if( ZH_IS_STRING( pItem1 ) && ZH_IS_STRING( pItem2 ) )
   {
      int i = zh_itemStrCmp( pItem1, pItem2, ZH_FALSE );
      zh_stackPop();
      zh_itemClear( pItem1 );
      pItem1->type = ZH_IT_LOGICAL;
      pItem1->item.asLogical.value = i > 0;
   }
   else if( ZH_IS_NUMINT( pItem1 ) && ZH_IS_NUMINT( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( ZH_ITEM_GET_NUMINTRAW( pItem1 ) >
                                       ZH_ITEM_GET_NUMINTRAW( pItem2 ) );
      pItem1->type = ZH_IT_LOGICAL;
      zh_stackDec();
   }
   else if( ZH_IS_NUMERIC( pItem1 ) && ZH_IS_NUMERIC( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( ZH_ITEM_GET_NUMDBLRAW( pItem1 ) >
                                       ZH_ITEM_GET_NUMDBLRAW( pItem2 ) );
      pItem1->type = ZH_IT_LOGICAL;
      zh_stackDec();
   }
   else if( ZH_IS_DATETIME( pItem1 ) && ZH_IS_DATETIME( pItem2 ) )
   {
      if( ZH_IS_TIMESTAMP( pItem1 ) && ZH_IS_TIMESTAMP( pItem2 ) )
         pItem1->item.asLogical.value = ( pItem1->item.asDateTime.julian >
                                          pItem2->item.asDateTime.julian ) ||
                                        ( pItem1->item.asDateTime.julian ==
                                          pItem2->item.asDateTime.julian &&
                                          pItem1->item.asDateTime.time >
                                          pItem2->item.asDateTime.time );
      else
         pItem1->item.asLogical.value = ( pItem1->item.asDateTime.julian >
                                          pItem2->item.asDateTime.julian );
      pItem1->type = ZH_IT_LOGICAL;
      zh_stackDec();
   }
   else if( ZH_IS_LOGICAL( pItem1 ) && ZH_IS_LOGICAL( pItem2 ) )
   {
      pItem1->item.asLogical.value = pItem1->item.asLogical.value &&
                                     ! pItem2->item.asLogical.value;
      zh_stackDec();
   }
   else if( zh_objOperatorCall( ZH_OO_OP_GREATER, pItem1, pItem1, pItem2, NULL ) )
      zh_stackPop();

   else
   {
      PZH_ITEM pResult = zh_errRT_BASE_Subst( EG_ARG, 1075, NULL, ">", 2, pItem1, pItem2 );

      if( pResult )
      {
         zh_stackPop();
         zh_itemMove( pItem1, pResult );
         zh_itemRelease( pResult );
      }
   }
}

static void zh_vmGreaterEqual( void )
{
   
   PZH_ITEM pItem2;
   PZH_ITEM pItem1;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmGreaterEqual()" ) );

   pItem2 = zh_stackItemFromTop( -1 );
   pItem1 = zh_stackItemFromTop( -2 );

   if( ZH_IS_STRING( pItem1 ) && ZH_IS_STRING( pItem2 ) )
   {
      int i = zh_itemStrCmp( pItem1, pItem2, ZH_FALSE );
      zh_stackPop();
      zh_itemClear( pItem1 );
      pItem1->type = ZH_IT_LOGICAL;
      pItem1->item.asLogical.value = i >= 0;
   }
   else if( ZH_IS_NUMINT( pItem1 ) && ZH_IS_NUMINT( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( ZH_ITEM_GET_NUMINTRAW( pItem1 ) >=
                                       ZH_ITEM_GET_NUMINTRAW( pItem2 ) );
      pItem1->type = ZH_IT_LOGICAL;
      zh_stackDec();
   }
   else if( ZH_IS_NUMERIC( pItem1 ) && ZH_IS_NUMERIC( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( ZH_ITEM_GET_NUMDBLRAW( pItem1 ) >=
                                       ZH_ITEM_GET_NUMDBLRAW( pItem2 ) );
      pItem1->type = ZH_IT_LOGICAL;
      zh_stackDec();
   }
   else if( ZH_IS_DATETIME( pItem1 ) && ZH_IS_DATETIME( pItem2 ) )
   {
      if( ZH_IS_TIMESTAMP( pItem1 ) && ZH_IS_TIMESTAMP( pItem2 ) )
         pItem1->item.asLogical.value = ( pItem1->item.asDateTime.julian >
                                          pItem2->item.asDateTime.julian ) ||
                                        ( pItem1->item.asDateTime.julian ==
                                          pItem2->item.asDateTime.julian &&
                                          pItem1->item.asDateTime.time >=
                                          pItem2->item.asDateTime.time );
      else
         pItem1->item.asLogical.value = ( pItem1->item.asDateTime.julian >=
                                          pItem2->item.asDateTime.julian );
      pItem1->type = ZH_IT_LOGICAL;
      zh_stackDec();
   }
   else if( ZH_IS_LOGICAL( pItem1 ) && ZH_IS_LOGICAL( pItem2 ) )
   {
      pItem1->item.asLogical.value = pItem1->item.asLogical.value ||
                                     ! pItem2->item.asLogical.value;
      zh_stackDec();
   }
   else if( zh_objOperatorCall( ZH_OO_OP_GREATEREQUAL, pItem1, pItem1, pItem2, NULL ) )
      zh_stackPop();

   else
   {
      PZH_ITEM pResult = zh_errRT_BASE_Subst( EG_ARG, 1076, NULL, ">=", 2, pItem1, pItem2 );

      if( pResult )
      {
         zh_stackPop();
         zh_itemMove( pItem1, pResult );
         zh_itemRelease( pResult );
      }
   }
}

static void zh_vmInstring( void )
{
   
   PZH_ITEM pItem1;
   PZH_ITEM pItem2;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmInstring()" ) );

   pItem1 = zh_stackItemFromTop( -2 );
   pItem2 = zh_stackItemFromTop( -1 );

   if( ZH_IS_STRING( pItem1 ) && ZH_IS_STRING( pItem2 ) )
   {
      ZH_BOOL fResult = ( zh_strAt( pItem1->item.asString.value, pItem1->item.asString.length,
                                    pItem2->item.asString.value, pItem2->item.asString.length ) != 0 );
      zh_stackPop();
      zh_itemClear( pItem1 );
      pItem1->type = ZH_IT_LOGICAL;
      pItem1->item.asLogical.value = fResult;
   }
   else if( ZH_IS_HASH( pItem2 ) &&
            ( ZH_IS_HASHKEY( pItem1 ) || zh_hashLen( pItem1 ) == 1 ) )
   {
      ZH_BOOL fResult = zh_hashScan( pItem2, pItem1, NULL );

      zh_stackPop();
      zh_itemClear( pItem1 );
      pItem1->type = ZH_IT_LOGICAL;
      pItem1->item.asLogical.value = fResult;
   }
   else if( zh_objOperatorCall( ZH_OO_OP_INCLUDE, pItem1, pItem2, pItem1, NULL ) )
      zh_stackPop();

   else if( zh_objOperatorCall( ZH_OO_OP_INSTRING, pItem1, pItem1, pItem2, NULL ) )
      zh_stackPop();

   else
   {
      PZH_ITEM pResult = zh_errRT_BASE_Subst( EG_ARG, 1109, NULL, "$", 2, pItem1, pItem2 );

      if( pResult )
      {
         zh_stackPop();
         zh_itemMove( pItem1, pResult );
         zh_itemRelease( pResult );
      }
   }
}

/* At this moment the eval stack should store:
 * -3 -> <current counter value>
 * -2 -> <end value>
 * -1 -> <step value>
 */
static void zh_vmForTest( void )        /* Test to check the end point of the FOR */
{
   
   PZH_ITEM pStep;
   ZH_BOOL fBack;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmForTest()" ) );

   pStep = zh_stackItemFromTop( -1 );
   if( ZH_IS_NUMERIC( pStep ) )
   {
      fBack = ZH_ITEM_GET_NUMDBLRAW( pStep ) < 0.0;
      zh_stackDec();
   }
   else
   {
      PZH_ITEM pResult;

      zh_vmPushInteger( 0 );
      pResult = zh_errRT_BASE_Subst( EG_ARG, 1073, NULL, "<", 2, pStep, zh_stackItemFromTop( -1 ) );

      if( pResult )
      {
         if( ZH_IS_LOGICAL( pResult ) )
         {
            fBack = pResult->item.asLogical.value;
            zh_itemRelease( pResult );
            zh_stackPop();
            zh_stackPop();
         }
         else
         {
            zh_itemMove( zh_stackItemFromTop( -1 ), pResult );
            zh_itemRelease( pResult );
            zh_errRT_BASE( EG_ARG, 1066, NULL, zh_langDGetErrorDesc( EG_CONDITION ), 1, zh_stackItemFromTop( -1 ) );
            return;
         }
      }
      else
         return;
   }

   if( fBack )
      zh_vmLess();
   else
      zh_vmGreater();
}

/* Begin Sequence WITH block auto destructor */
static ZH_GARBAGE_FUNC( zh_SeqBlockDestructor )
{
   zh_itemMove( zh_errorBlock(), ( PZH_ITEM ) Cargo );
}

static const ZH_GC_FUNCS s_gcSeqBlockFuncs =
{
   zh_SeqBlockDestructor,
   zh_gcGripMark
};

static void zh_vmSeqBlock( void )
{
   
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmSeqBlock()" ) );

   pItem = zh_stackItemFromTop( -1 );
   if( ZH_IS_BLOCK( pItem ) )
   {
      PZH_ITEM pBlockCopy, pBlock;

      pBlock = zh_errorBlock();
      pBlockCopy = ( PZH_ITEM ) zh_gcAllocRaw( sizeof( ZH_ITEM ),
                                               &s_gcSeqBlockFuncs );
      zh_itemRawCpy( pBlockCopy, pBlock );
      zh_itemRawCpy( pBlock, pItem );
      pItem->type = ZH_IT_POINTER;
      pItem->item.asPointer.value = pBlockCopy;
      pItem->item.asPointer.collect = pItem->item.asPointer.single = ZH_TRUE;
   }
}

/* With object auto destructor */
static ZH_GARBAGE_FUNC( zh_withObjectDestructor )
{
   
   ZH_I_SIZE * pnWithObjectBase = ( ZH_I_SIZE * ) Cargo;

   zh_stackWithObjectSetOffset( *pnWithObjectBase );
}

static const ZH_GC_FUNCS s_gcWithObjectFuncs =
{
   zh_withObjectDestructor,
   zh_gcDummyMark
};


static void zh_vmWithObjectStart( void )
{
   
   ZH_I_SIZE * pnWithObjectBase;
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmWithObjectStart()" ) );

   pItem = zh_stackAllocItem();
   pnWithObjectBase = ( ZH_I_SIZE * ) zh_gcAllocRaw( sizeof( ZH_I_SIZE ),
                                                &s_gcWithObjectFuncs );
   * pnWithObjectBase = zh_stackWithObjectOffset();
   pItem->type = ZH_IT_POINTER;
   pItem->item.asPointer.value = pnWithObjectBase;
   pItem->item.asPointer.collect = pItem->item.asPointer.single = ZH_TRUE;
   /* The object is pushed directly before this pcode */
   /* store position of current WITH OBJECT frame */
   zh_stackWithObjectSetOffset( zh_stackTopOffset() - 2 );
}

/*
 * Release enumerator items - called from zh_itemClear()
 */
void zh_vmEnumRelease( PZH_ITEM pBase, PZH_ITEM pValue )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmEnumRelease(%p,%p)", ( void * ) pBase, ( void * ) pValue ) );

   if( pValue )
      zh_itemRelease( pValue );

   if( ZH_IS_OBJECT( pBase ) && zh_vmRequestQuery() == 0 &&
       zh_objHasOperator( pBase, ZH_OO_OP_ENUMSTOP ) )
   {
      zh_stackPushReturn();
      ZH_VM_PUSHNIL();
      zh_objOperatorCall( ZH_OO_OP_ENUMSTOP, zh_stackItemFromTop( -1 ),
                          pBase, NULL, NULL );
      zh_stackPop();
      zh_stackPopReturn();
   }
}

/*
 * extended reference used as enumerator destructor
 */
typedef struct
{
   ZH_ITEM basevalue;
   ZH_ITEM oldvalue;
   ZH_ITEM enumref;
} ZH_ENUMREF, * PZH_ENUMREF;

static PZH_ITEM zh_vmEnumRefRead( PZH_ITEM pRefer )
{
   return &( ( PZH_ENUMREF ) pRefer->item.asExtRef.value )->oldvalue;
}

static PZH_ITEM zh_vmEnumRefWrite( PZH_ITEM pRefer, PZH_ITEM pSource )
{
   ZH_SYMBOL_UNUSED( pRefer );
   ZH_SYMBOL_UNUSED( pSource );
   return NULL;
}

static void zh_vmEnumRefCopy( PZH_ITEM pDest )
{
   pDest->type = ZH_IT_NIL;
}

static void zh_vmEnumRefClear( void * value )
{
   zh_itemMove( zh_itemUnRefOnce( &( ( PZH_ENUMREF ) value )->enumref ),
                &( ( PZH_ENUMREF ) value )->oldvalue );
   if( ZH_IS_COMPLEX( &( ( PZH_ENUMREF ) value )->basevalue ) )
      zh_itemClear( &( ( PZH_ENUMREF ) value )->basevalue );
   if( ZH_IS_COMPLEX( &( ( PZH_ENUMREF ) value )->enumref ) )
      zh_itemClear( &( ( PZH_ENUMREF ) value )->enumref );

   zh_xfree( value );
}

static void zh_vmEnumRefMark( void * value )
{
   if( ZH_IS_GCITEM( &( ( PZH_ENUMREF ) value )->basevalue ) )
      zh_gcItemRef( &( ( PZH_ENUMREF ) value )->basevalue );
   if( ZH_IS_GCITEM( &( ( PZH_ENUMREF ) value )->oldvalue ) )
      zh_gcItemRef( &( ( PZH_ENUMREF ) value )->oldvalue );
   if( ZH_IS_GCITEM( &( ( PZH_ENUMREF ) value )->enumref ) )
      zh_gcItemRef( &( ( PZH_ENUMREF ) value )->enumref );
}

/*
 * create extended reference for enumerator destructor
 */
static void zh_vmEnumReference( PZH_ITEM pBase )
{
   static const ZH_EXTREF s_EnumExtRef = {
      zh_vmEnumRefRead,
      zh_vmEnumRefWrite,
      zh_vmEnumRefCopy,
      zh_vmEnumRefClear,
      zh_vmEnumRefMark
   };

   PZH_ENUMREF pEnumExtRef;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmEnumReference(%p)", ( void * ) pBase ) );

   pEnumExtRef = ( PZH_ENUMREF ) zh_xgrab( sizeof( ZH_ENUMREF ) );
   pEnumExtRef->oldvalue.type = ZH_IT_NIL;
   pEnumExtRef->enumref.type = ZH_IT_NIL;
   zh_itemRawCpy( &pEnumExtRef->basevalue, pBase );
   pBase->type = ZH_IT_BYREF | ZH_IT_EXTREF;
   pBase->item.asExtRef.value = ( void * ) pEnumExtRef;
   pBase->item.asExtRef.func = &s_EnumExtRef;
}

/* At this moment the eval stack should store:
 * -2 -> <array for traverse>
 * -1 -> <the reference to enumerate variable>
 */
/* Test to check the start point of the FOR EACH loop */
static void zh_vmEnumStart( int nVars, int nDescend )
{
   
   ZH_BOOL fStart = ZH_TRUE;
   int i;

#if 0
   pItem = zh_itemUnRef( zh_stackItemFromTop( -( ( int ) nVars << 1 ) ) );
   if( ( pItem->type & ( ZH_IT_ARRAY | ZH_IT_HASH | ZH_IT_STRING ) ) == 0 )
   {
      zh_errRT_BASE( EG_ARG, 1068, NULL, zh_langDGetErrorDesc( EG_ARRACCESS ), 1, pItem );
      return;
   }
#endif

   for( i = ( int ) nVars << 1; i > 0 && fStart; i -= 2 )
   {
      PZH_ITEM pBase, pValue, pEnumRef, pEnum;

      pValue = zh_stackItemFromTop( -i );
      /* create extended reference for enumerator destructor */
      zh_vmEnumReference( pValue );
      /* store the reference to control variable */
      pEnumRef = zh_stackItemFromTop( -i + 1 );
      zh_itemCopy( &( ( PZH_ENUMREF ) pValue->item.asExtRef.value )->enumref,
                   pEnumRef );
      /* the control variable */
      pEnum = zh_itemUnRefOnce( pEnumRef );
      /* store the old value of control variable and clear it */
      zh_itemMove( &( ( PZH_ENUMREF ) pValue->item.asExtRef.value )->oldvalue,
                   pEnum );

      /* set the iterator value */
      pEnum->type = ZH_IT_BYREF | ZH_IT_ENUM;
      pEnum->item.asEnum.basePtr =
         pBase = &( ( PZH_ENUMREF ) pValue->item.asExtRef.value )->basevalue;
      pEnum->item.asEnum.valuePtr = NULL;

      if( ZH_IS_BYREF( pBase ) )
         pBase = zh_itemUnRef( pBase );

      if( ZH_IS_OBJECT( pBase ) && zh_objHasOperator( pBase, ZH_OO_OP_ENUMSTART ) )
      {
         pEnum->item.asEnum.offset = 0;
         pEnum->item.asEnum.valuePtr = zh_itemNew( NULL );
         ZH_VM_PUSHNIL();
         zh_vmPushLogical( nDescend == 0 );
         zh_objOperatorCall( ZH_OO_OP_ENUMSTART, zh_stackItemFromTop( -2 ),
                             pBase, pEnumRef, zh_stackItemFromTop( -1 ) );
         zh_stackPop();
         if( zh_vmRequestQuery() != 0 || ! zh_vmPopLogical() )
         {
            fStart = ZH_FALSE;
            break;
         }
         else if( zh_objHasOperator( pBase, ZH_OO_OP_ENUMSKIP ) )
            continue;
         zh_itemRelease( pEnum->item.asEnum.valuePtr );
         pEnum->item.asEnum.valuePtr = NULL;
      }

      if( ZH_IS_ARRAY( pBase ) )
      {
         /* the index into an array */
         pEnum->item.asEnum.offset = ( nDescend > 0 ) ? 1 :
                                       pBase->item.asArray.value->nLen;
         if( pBase->item.asArray.value->nLen == 0 )
            fStart = ZH_FALSE;
      }
      else if( ZH_IS_HASH( pBase ) )
      {
         ZH_SIZE nLen = zh_hashLen( pBase );
         /* the index into a hash */
         pEnum->item.asEnum.offset = ( nDescend > 0 ) ? 1 : nLen;
         if( nLen == 0 )
            fStart = ZH_FALSE;
      }
      else if( ZH_IS_STRING( pBase ) )
      {
         /* storage item for single characters */
         pEnum->item.asEnum.offset = ( nDescend > 0 ) ? 1 :
                                       pBase->item.asString.length;
         if( pBase->item.asString.length )
            pEnum->item.asEnum.valuePtr =
                        zh_itemPutCL( NULL, pBase->item.asString.value +
                                            pEnum->item.asEnum.offset - 1, 1 );
         else
            fStart = ZH_FALSE;
      }
      else if( zh_vmRequestQuery() == 0 )
      {
         zh_errRT_BASE( EG_ARG, 1068, NULL, zh_langDGetErrorDesc( EG_ARRACCESS ), 1, pBase );
         return;
      }
   }

   zh_vmPushInteger( nVars );    /* number of iterators */
   /* empty array/string - do not start enumerations loop */
   zh_vmPushLogical( fStart );
}


/* Enumeration in ascending order
 * At this moment the eval stack should store:
 * -3 -> <old value of enumerator variable>
 * -2 -> <the reference to enumerate variable>
 * -1 -> <number of iterators>
 */
static void zh_vmEnumNext( void )
{
   
   int i;

   for( i = ( int ) zh_stackItemFromTop( -1 )->item.asInteger.value; i > 0; --i )
   {
      PZH_ITEM pEnumRef, pEnum, pBase;

      pEnumRef = zh_stackItemFromTop( -( i << 1 ) );
      pEnum = zh_itemUnRefOnce( pEnumRef );
      pBase = pEnum->item.asEnum.basePtr;
      if( ZH_IS_BYREF( pBase ) )
         pBase = zh_itemUnRef( pBase );
      if( ZH_IS_ARRAY( pBase ) )
      {
         if( ZH_IS_OBJECT( pBase ) &&
             zh_objHasOperator( pBase, ZH_OO_OP_ENUMSKIP ) )
         {
            ++pEnum->item.asEnum.offset;
            ZH_VM_PUSHNIL();
            zh_vmPushLogical( ZH_FALSE );
            zh_objOperatorCall( ZH_OO_OP_ENUMSKIP, zh_stackItemFromTop( -2 ),
                                pBase, pEnumRef, zh_stackItemFromTop( -1 ) );
            zh_stackPop();
            if( zh_vmRequestQuery() != 0 || ! zh_vmPopLogical() )
               break;
         }
         else
         {
            /* Clear the item value which can be set with RT error
               when enumerator was out of array size during unreferencing
             */
            if( pEnum->item.asEnum.valuePtr )
            {
               zh_itemRelease( pEnum->item.asEnum.valuePtr );
               pEnum->item.asEnum.valuePtr = NULL;
            }
            if( ( ZH_SIZE ) ++pEnum->item.asEnum.offset >
                pBase->item.asArray.value->nLen )
               break;
         }
      }
      else if( ZH_IS_HASH( pBase ) )
      {
         /* Clear the item value which can be set with RT error
            when enumerator was out of array size during unreferencing
          */
         if( pEnum->item.asEnum.valuePtr )
         {
            zh_itemRelease( pEnum->item.asEnum.valuePtr );
            pEnum->item.asEnum.valuePtr = NULL;
         }
         if( ( ZH_SIZE ) ++pEnum->item.asEnum.offset > zh_hashLen( pBase ) )
            break;
      }
      else if( ZH_IS_STRING( pBase ) )
      {
         if( ( ZH_SIZE ) ++pEnum->item.asEnum.offset >
             pBase->item.asString.length )
            break;
         pEnum->item.asEnum.valuePtr = zh_itemPutCL(
                                          pEnum->item.asEnum.valuePtr,
                                          pBase->item.asString.value +
                                          pEnum->item.asEnum.offset - 1, 1 );
      }
      else
      {
         zh_errRT_BASE( EG_ARG, 1068, NULL, zh_langDGetErrorDesc( EG_ARRACCESS ), 1, pBase );
         return;
      }
   }
   zh_vmPushLogical( i == 0 );
}

/* Enumeration in descending order
 * At this moment the eval stack should store:
 * -3 -> <old value of enumerator variable>
 * -2 -> <the reference to enumerate variable>
 * -1 -> <number of iterators>
 */
static void zh_vmEnumPrev( void )
{
   
   int i;

   for( i = zh_stackItemFromTop( -1 )->item.asInteger.value; i > 0; --i )
   {
      PZH_ITEM pEnumRef, pEnum, pBase;

      pEnumRef = zh_stackItemFromTop( -( i << 1 ) );
      pEnum = zh_itemUnRefOnce( pEnumRef );
      pBase = pEnum->item.asEnum.basePtr;
      if( ZH_IS_BYREF( pBase ) )
         pBase = zh_itemUnRef( pBase );
      if( ZH_IS_ARRAY( pBase ) )
      {
         if( ZH_IS_OBJECT( pBase ) &&
             zh_objHasOperator( pBase, ZH_OO_OP_ENUMSKIP ) )
         {
            --pEnum->item.asEnum.offset;
            ZH_VM_PUSHNIL();
            zh_vmPushLogical( ZH_TRUE );
            zh_objOperatorCall( ZH_OO_OP_ENUMSKIP, zh_stackItemFromTop( -2 ),
                                pBase, pEnumRef, zh_stackItemFromTop( -1 ) );
            zh_stackPop();
            if( zh_vmRequestQuery() != 0 || ! zh_vmPopLogical() )
               break;
         }
         else
         {
            /* Clear the item value which can be set with RT error
               when enumerator was out of array size during unreferencing
             */
            if( pEnum->item.asEnum.valuePtr )
            {
               zh_itemRelease( pEnum->item.asEnum.valuePtr );
               pEnum->item.asEnum.valuePtr = NULL;
            }
            if( --pEnum->item.asEnum.offset == 0 )
               break;
         }
      }
      else if( ZH_IS_HASH( pBase ) )
      {
         /* Clear the item value which can be set with RT error
            when enumerator was out of array size during unreferencing
          */
         if( pEnum->item.asEnum.valuePtr )
         {
            zh_itemRelease( pEnum->item.asEnum.valuePtr );
            pEnum->item.asEnum.valuePtr = NULL;
         }
         if( --pEnum->item.asEnum.offset == 0 )
            break;
      }
      else if( ZH_IS_STRING( pBase ) )
      {
         if( --pEnum->item.asEnum.offset == 0 )
            break;
         pEnum->item.asEnum.valuePtr = zh_itemPutCL(
                                          pEnum->item.asEnum.valuePtr,
                                          pBase->item.asString.value +
                                          pEnum->item.asEnum.offset - 1, 1 );
      }
      else
      {
         zh_errRT_BASE( EG_ARG, 1068, NULL, zh_langDGetErrorDesc( EG_ARRACCESS ), 1, pBase );
         return;
      }
   }
   zh_vmPushLogical( i == 0 );
}

/* Enumeration in descending order
 * At this moment the eval stack should store:
 * -3 -> <old value of enumerator variable>
 * -2 -> <the reference to enumerate variable>
 * -1 -> <number of iterators>
 */
static void zh_vmEnumEnd( void )
{
   
   int iVars;

   /* remove number of iterators */
   iVars = zh_stackItemFromTop( -1 )->item.asInteger.value;
   zh_stackDec();

   while( --iVars >= 0 )
   {
      zh_stackPop();
      zh_stackPop();
   }
}

static PZH_ITEM zh_vmSwitchGet( void )
{
   
   PZH_ITEM pSwitch = zh_stackItemFromTop( -1 );

   if( ! ( ZH_IS_NUMINT( pSwitch ) || ZH_IS_STRING( pSwitch ) ) )
   {
      PZH_ITEM pResult = zh_errRT_BASE_Subst( EG_ARG, 3104, NULL, "SWITCH", 1, pSwitch );

      if( ! pResult )
         return NULL;

      zh_itemMove( pSwitch, pResult );
      zh_itemRelease( pResult );
   }

   return pSwitch;
}

static const ZH_BYTE * zh_vmSwitch( const ZH_BYTE * pCode, ZH_USHORT casesCnt )
{
   
   PZH_ITEM pSwitch = zh_vmSwitchGet();

   if( pSwitch )
   {
      ZH_BOOL fFound = ZH_FALSE;

      while( ! fFound && casesCnt-- )
      {
         switch( pCode[ 0 ] )
         {
            case ZH_P_PUSHLONG:
               if( ZH_IS_NUMINT( pSwitch ) )
               {
                  fFound = ZH_ITEM_GET_NUMINTRAW( pSwitch ) == ZH_PCODE_MKLONG( &pCode[ 1 ] );
               }
               pCode += 5;
               break;

            case ZH_P_PUSHSTRSHORT:
               if( ZH_IS_STRING( pSwitch ) )
               {
                  #if 0
                  fFound = zh_itemStrCmp( pItem1, pItem2, bExact );
                  #endif
                  fFound = ( ZH_SIZE ) pCode[ 1 ] - 1 == pSwitch->item.asString.length &&
                           memcmp( pSwitch->item.asString.value, &pCode[ 2 ],
                                   pSwitch->item.asString.length ) == 0;
               }
               pCode += 2 + pCode[ 1 ];
               break;

            case ZH_P_PUSHNIL:
               /* default clause */
               fFound = ZH_TRUE;
               pCode++;
               break;
         }

         switch( pCode[ 0 ] )
         {
            case ZH_P_JUMPNEAR:
               if( fFound )
                  pCode += ( signed char ) pCode[ 1 ];
               else
                  pCode += 2;
               break;
            case ZH_P_JUMP:
               if( fFound )
                  pCode += ZH_PCODE_MKSHORT( &pCode[ 1 ] );
               else
                  pCode += 3;
               break;
            case ZH_P_JUMPFAR:
               if( fFound )
                  pCode += ZH_PCODE_MKINT24( &pCode[ 1 ] );
               else
                  pCode += 4;
               break;
         }
      }
   }
   zh_stackPop();
   return pCode;
}

/* ------------------------------- */
/* Operators (logical)             */
/* ------------------------------- */

static void zh_vmNot( void )
{
   
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmNot()" ) );

   pItem = zh_stackItemFromTop( -1 );

   if( ZH_IS_LOGICAL( pItem ) )
   {
      pItem->type = ZH_IT_LOGICAL;
      pItem->item.asLogical.value = ! pItem->item.asLogical.value;
   }
   else if( ! zh_objOperatorCall( ZH_OO_OP_NOT, pItem, pItem, NULL, NULL ) )
   {
      PZH_ITEM pResult = zh_errRT_BASE_Subst( EG_ARG, 1077, NULL, ".NOT.", 1, pItem );

      if( pResult )
      {
         zh_itemMove( pItem, pResult );
         zh_itemRelease( pResult );
      }
   }
}

static void zh_vmAnd( void )
{
   
   PZH_ITEM pItem2;
   PZH_ITEM pItem1;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmAnd()" ) );

   pItem2 = zh_stackItemFromTop( -1 );
   pItem1 = zh_stackItemFromTop( -2 );

   if( ZH_IS_LOGICAL( pItem1 ) && ZH_IS_LOGICAL( pItem2 ) )
   {
      pItem1->type = ZH_IT_LOGICAL;
      pItem1->item.asLogical.value = pItem1->item.asLogical.value && pItem2->item.asLogical.value;
      zh_stackDec();
   }
   else if( zh_objOperatorCall( ZH_OO_OP_AND, pItem1, pItem1, pItem2, NULL ) )
      zh_stackPop();

   else
   {
      PZH_ITEM pResult = zh_errRT_BASE_Subst( EG_ARG, 1078, NULL, ".AND.", 2, pItem1, pItem2 );

      if( pResult )
      {
         zh_stackPop();
         zh_itemMove( pItem1, pResult );
         zh_itemRelease( pResult );
      }
   }
}

static void zh_vmOr( void )
{
   
   PZH_ITEM pItem2;
   PZH_ITEM pItem1;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmOr()" ) );

   pItem2 = zh_stackItemFromTop( -1 );
   pItem1 = zh_stackItemFromTop( -2 );

   if( ZH_IS_LOGICAL( pItem1 ) && ZH_IS_LOGICAL( pItem2 ) )
   {
      pItem1->type = ZH_IT_LOGICAL;
      pItem1->item.asLogical.value = pItem1->item.asLogical.value || pItem2->item.asLogical.value;
      zh_stackDec();
   }
   else if( zh_objOperatorCall( ZH_OO_OP_OR, pItem1, pItem1, pItem2, NULL ) )
      zh_stackPop();

   else
   {
      PZH_ITEM pResult = zh_errRT_BASE_Subst( EG_ARG, 1079, NULL, ".OR.", 2, pItem1, pItem2 );

      if( pResult )
      {
         zh_stackPop();
         zh_itemMove( pItem1, pResult );
         zh_itemRelease( pResult );
      }
   }
}

/* ------------------------------- */
/* Array                           */
/* ------------------------------- */

static void zh_vmArrayPush( void )
{
   
   PZH_ITEM pIndex;
   PZH_ITEM pArray;
   ZH_SIZE nIndex;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmArrayPush()" ) );

   pIndex = zh_stackItemFromTop( -1 );
   pArray = zh_stackItemFromTop( -2 );

   if( ZH_IS_HASH( pArray ) && ZH_IS_HASHKEY( pIndex ) )
   {
      PZH_ITEM pValue = zh_hashGetItemPtr( pArray, pIndex, ZH_HASH_AUTOADD_ACCESS );
      if( pValue )
      {
         zh_itemCopy( pIndex, pValue );
         zh_itemMove( pArray, pIndex );
         zh_stackDec();
      }
      else if( zh_objOperatorCall( ZH_OO_OP_ARRAYINDEX, pArray, pArray, pIndex, NULL ) )
         zh_stackPop();
      else
         zh_errRT_BASE( EG_BOUND, 1132, NULL, zh_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, pIndex );
      return;
   }
   else if( ZH_IS_INTEGER( pIndex ) )
      nIndex = ( ZH_SIZE ) pIndex->item.asInteger.value;
   else if( ZH_IS_LONG( pIndex ) )
      nIndex = ( ZH_SIZE ) pIndex->item.asLong.value;
   else if( ZH_IS_DOUBLE( pIndex ) )
      nIndex = ( ZH_SIZE ) pIndex->item.asDouble.value;
   else
   {
      if( zh_objOperatorCall( ZH_OO_OP_ARRAYINDEX, pArray, pArray, pIndex, NULL ) )
         zh_stackPop();
      else
      {
         PZH_ITEM pResult = zh_errRT_BASE_Subst( EG_ARG, 1068, NULL, zh_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, pIndex );
         if( pResult )
         {
            zh_stackPop();
            zh_itemMove( pArray, pResult );
            zh_itemRelease( pResult );
         }
      }
      return;
   }

   if( ZH_IS_ARRAY( pArray ) )
   {
      if( ZH_IS_OBJECT( pArray ) &&
          zh_objOperatorCall( ZH_OO_OP_ARRAYINDEX, pArray, pArray, pIndex, NULL ) )
      {
         zh_stackPop();
         return;
      }

      if( ZH_IS_VALID_INDEX( nIndex, pArray->item.asArray.value->nLen ) )
      {
         zh_itemCopy( pIndex, pArray->item.asArray.value->pItems + nIndex - 1 );
         zh_itemMove( pArray, pIndex );
         zh_stackDec();
      }
      else if( ! ZH_IS_OBJECT( pArray ) &&
               zh_objOperatorCall( ZH_OO_OP_ARRAYINDEX, pArray, pArray, pIndex, NULL ) )
         zh_stackPop();
      else
         zh_errRT_BASE( EG_BOUND, 1132, NULL, zh_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, pIndex );
   }
   else if( zh_objOperatorCall( ZH_OO_OP_ARRAYINDEX, pArray, pArray, pIndex, NULL ) )
      zh_stackPop();

   else
      zh_errRT_BASE( EG_ARG, 1068, NULL, zh_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, pIndex );
}

static void zh_vmArrayPushRef( void )
{
   
   PZH_ITEM pIndex;
   PZH_ITEM pArray;
   PZH_ITEM pRefer;
   ZH_SIZE  nIndex;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmArrayPushRef()" ) );

   pIndex = zh_stackItemFromTop( -1 );
   pRefer = zh_stackItemFromTop( -2 );
   pArray = ZH_IS_BYREF( pRefer ) ? zh_itemUnRef( pRefer ) : pRefer;

   if( ZH_IS_HASH( pArray ) && ZH_IS_HASHKEY( pIndex ) )
   {
      PZH_ITEM pValue = zh_hashGetItemRefPtr( pArray, pIndex );
      if( pValue )
      {
         zh_itemCopy( pIndex, pValue );
         zh_itemMove( pRefer, pIndex );
         zh_stackDec();
      }
      else if( zh_objHasOperator( pArray, ZH_OO_OP_ARRAYINDEX ) )
      {
         /* create extended object index reference */
         zh_vmMsgIndexReference( pRefer, pArray, pIndex );
         zh_stackPop();
         return;
      }
      else
         zh_errRT_BASE( EG_BOUND, 1132, NULL, zh_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, pIndex );
      return;
   }
   else if( ZH_IS_INTEGER( pIndex ) )
      nIndex = ( ZH_SIZE ) pIndex->item.asInteger.value;
   else if( ZH_IS_LONG( pIndex ) )
      nIndex = ( ZH_SIZE ) pIndex->item.asLong.value;
   else if( ZH_IS_DOUBLE( pIndex ) )
      nIndex = ( ZH_SIZE ) pIndex->item.asDouble.value;
   else if( zh_objHasOperator( pArray, ZH_OO_OP_ARRAYINDEX ) )
   {
      /* create extended object index reference */
      zh_vmMsgIndexReference( pRefer, pArray, pIndex );
      zh_stackPop();
      return;
   }
   else
   {
      PZH_ITEM pResult = zh_errRT_BASE_Subst( EG_ARG, 1068, NULL, zh_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, pIndex );

      if( pResult )
      {
         zh_stackPop();
         zh_itemMove( pRefer, pResult );
         zh_itemRelease( pResult );
      }
      return;
   }

   if( ZH_IS_ARRAY( pArray ) )
   {
      if( ZH_IS_OBJECT( pArray ) && zh_objHasOperator( pArray, ZH_OO_OP_ARRAYINDEX ) )
      {
         /* create extended object index reference */
         zh_vmMsgIndexReference( pRefer, pArray, pIndex );
         zh_stackPop();
         return;
      }
      else if( ZH_IS_VALID_INDEX( nIndex, pArray->item.asArray.value->nLen ) )
      {
         /* This function is safe for overwriting passed array, [druzus] */
         zh_arrayGetItemRef( pArray, nIndex, pRefer );
         zh_stackDec();
      }
      else if( ! ZH_IS_OBJECT( pArray ) && zh_objHasOperator( pArray, ZH_OO_OP_ARRAYINDEX ) )
      {
         /* create extended object index reference */
         zh_vmMsgIndexReference( pRefer, pArray, pIndex );
         zh_stackPop();
         return;
      }
      else
         zh_errRT_BASE( EG_BOUND, 1132, NULL, zh_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, pIndex );
   }
   else if( zh_objHasOperator( pArray, ZH_OO_OP_ARRAYINDEX ) )
   {
      /* create extended object index reference */
      zh_vmMsgIndexReference( pRefer, pArray, pIndex );
      zh_stackPop();
      return;
   }
   else
      zh_errRT_BASE( EG_ARG, 1068, NULL, zh_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, pIndex );
}

static void zh_vmArrayPop( void )
{
   
   PZH_ITEM pValue;
   PZH_ITEM pIndex;
   PZH_ITEM pArray;
   ZH_SIZE  nIndex;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmArrayPop()" ) );

   pValue = zh_stackItemFromTop( -3 );
   pArray = zh_stackItemFromTop( -2 );
   pIndex = zh_stackItemFromTop( -1 );

   if( ZH_IS_BYREF( pArray ) )
      pArray = zh_itemUnRef( pArray );

   if( ZH_IS_HASH( pArray ) && ZH_IS_HASHKEY( pIndex ) )
   {
      PZH_ITEM pDest = zh_hashGetItemPtr( pArray, pIndex, ZH_HASH_AUTOADD_ASSIGN );
      if( pDest )
      {
         pValue->type &= ~( ZH_IT_MEMOFLAG | ZH_IT_DEFAULT );
         zh_itemMoveFromRef( pDest, pValue );
         zh_stackPop();
         zh_stackPop();
         zh_stackDec();    /* value was moved above zh_stackDec() is enough */
      }
      else if( zh_objOperatorCall( ZH_OO_OP_ARRAYINDEX, pArray, pArray, pIndex, pValue ) )
      {
         zh_stackPop();
         zh_stackPop();
         zh_stackPop();
      }
      else
         zh_errRT_BASE( EG_BOUND, 1133, NULL, zh_langDGetErrorDesc( EG_ARRASSIGN ), 3, pArray, pIndex, pValue );
      return;
   }
   else if( ZH_IS_INTEGER( pIndex ) )
      nIndex = ( ZH_SIZE ) pIndex->item.asInteger.value;
   else if( ZH_IS_LONG( pIndex ) )
      nIndex = ( ZH_SIZE ) pIndex->item.asLong.value;
   else if( ZH_IS_DOUBLE( pIndex ) )
      nIndex = ( ZH_SIZE ) pIndex->item.asDouble.value;
   else
   {
      if( zh_objOperatorCall( ZH_OO_OP_ARRAYINDEX, pArray, pArray, pIndex, pValue ) )
      {
         zh_stackPop();
         zh_stackPop();
         zh_stackPop();
      }
      else
         zh_errRT_BASE( EG_ARG, 1069, NULL, zh_langDGetErrorDesc( EG_ARRASSIGN ), 1, pIndex );
      return;
   }

   if( ZH_IS_ARRAY( pArray ) )
   {
      if( ZH_IS_OBJECT( pArray ) &&
          zh_objOperatorCall( ZH_OO_OP_ARRAYINDEX, pArray, pArray, pIndex, pValue ) )
      {
         zh_stackPop();
         zh_stackPop();
         zh_stackPop();
         return;
      }

      if( ZH_IS_VALID_INDEX( nIndex, pArray->item.asArray.value->nLen ) )
      {
         pValue->type &= ~( ZH_IT_MEMOFLAG | ZH_IT_DEFAULT );
         zh_itemMoveRef( pArray->item.asArray.value->pItems + nIndex - 1, pValue );
         zh_stackPop();
         zh_stackPop();
         zh_stackDec();    /* value was moved above zh_stackDec() is enough */
      }
      else if( ! ZH_IS_OBJECT( pArray ) &&
               zh_objOperatorCall( ZH_OO_OP_ARRAYINDEX, pArray, pArray, pIndex, pValue ) )
      {
         zh_stackPop();
         zh_stackPop();
         zh_stackPop();
      }
      else
         zh_errRT_BASE( EG_BOUND, 1133, NULL, zh_langDGetErrorDesc( EG_ARRASSIGN ), 1, pIndex );
   }
   else if( zh_objOperatorCall( ZH_OO_OP_ARRAYINDEX, pArray, pArray, pIndex, pValue ) )
   {
      zh_stackPop();
      zh_stackPop();
      zh_stackPop();
      return;
   }
   else
      zh_errRT_BASE( EG_ARG, 1069, NULL, zh_langDGetErrorDesc( EG_ARRASSIGN ), 1, pIndex );
}

static void zh_vmArrayGen( ZH_SIZE nElements ) /* generates an nElements Array and fills it from the stack values */
{
   
   PZH_ITEM pArray;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmArrayGen(%" ZH_PFS "u)", nElements ) );

   /* create new array on ZHVM stack */
   pArray = zh_stackAllocItem();
   zh_arrayNew( pArray, nElements );

   if( nElements )
   {
      ZH_SIZE nPos;
      /* move items from ZHVM stack to created array */
      for( nPos = 0; nPos < nElements; nPos++ )
      {
         PZH_ITEM pValue = zh_stackItemFromTop( ( int ) ( nPos - nElements - 1 ) );
         pValue->type &= ~( ZH_IT_MEMOFLAG | ZH_IT_DEFAULT );
         zh_itemMove( pArray->item.asArray.value->pItems + nPos, pValue );
      }
      /* move the new array to position of first parameter */
      zh_itemMove( zh_stackItemFromTop( -1 - ( int ) nElements ), pArray );

      /* decrease the stack counter - all items are NIL */
      zh_stackDecrease( nElements );
   }
}

/* This function creates an array item using 'uiDimension' as an index
 * to retrieve the number of elements from the stack
 */
static ZH_BOOL zh_vmArrayNew( PZH_ITEM pArray, ZH_USHORT uiDimension )
{
   
   ZH_I_SIZE  nElements;
   PZH_ITEM pDim;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmArrayNew(%p, %hu)", ( void * ) pArray, uiDimension ) );

   pDim = zh_stackItemFromTop( ( int ) ( -1 - uiDimension ) );

   /* use the proper type of number of elements */
   if( ZH_IS_INTEGER( pDim ) )
      nElements = ( ZH_I_SIZE ) pDim->item.asInteger.value;
   else if( ZH_IS_LONG( pDim ) )
      nElements = ( ZH_I_SIZE ) pDim->item.asLong.value;
   else if( ZH_IS_DOUBLE( pDim ) )
      nElements = ( ZH_I_SIZE ) pDim->item.asDouble.value;
   else
      nElements = 0;

   if( nElements >= 0 )
   {
      /* create an array */
      zh_arrayNew( pArray, nElements );

      if( --uiDimension )
      {
         /* call self recursively to create next dimensions */
         while( nElements-- )
            if( ! zh_vmArrayNew( pArray->item.asArray.value->pItems + nElements, uiDimension ) )
               return ZH_FALSE;
      }
      return ZH_TRUE;
   }

   zh_errRT_BASE( EG_BOUND, 1131, NULL, zh_langDGetErrorDesc( EG_ARRDIMENSION ), 0 );
   return ZH_FALSE;
}

static void zh_vmArrayDim( ZH_USHORT uiDimensions ) /* generates an uiDimensions Array and initialize those dimensions from the stack values */
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmArrayDim(%hu)", uiDimensions ) );

   if( uiDimensions )
   {
      zh_vmArrayNew( zh_stackAllocItem(), uiDimensions );

      zh_itemMove( zh_stackItemFromTop( ( int ) ( -1 - uiDimensions ) ),
                   zh_stackItemFromTop( -1 ) );
      do
         zh_stackPop();
      while( --uiDimensions );
   }
   else
      ZH_VM_PUSHNIL();
}

static void zh_vmHashGen( ZH_SIZE nElements ) /* generates an nElements Hash and fills it from the stack values */
{
   
   PZH_ITEM pHash;
   int iPos;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmHashGen(%" ZH_PFS "u)", nElements ) );

   /* create new hash item */
   pHash = zh_hashNew( NULL );
   zh_hashPreallocate( pHash, nElements );
   nElements <<= 1;
   iPos = - ( int ) nElements;
   while( iPos )
   {
      PZH_ITEM pKey = zh_stackItemFromTop( iPos++ );
      PZH_ITEM pVal = zh_stackItemFromTop( iPos++ );
      if( ZH_IS_HASHKEY( pKey ) )
         zh_hashAdd( pHash, pKey, pVal );
      else
      {
         zh_errRT_BASE( EG_BOUND, 1133, NULL, zh_langDGetErrorDesc( EG_ARRASSIGN ), 3, pHash, pKey, pVal );
         break;
      }
   }
   zh_stackRemove( zh_stackTopOffset() - nElements );
   zh_itemMove( zh_stackAllocItem(), pHash );
   zh_itemRelease( pHash );
}


/* ------------------------------- */
/* Macros                          */
/* ------------------------------- */

static void zh_vmMacroPushIndex( void )
{
   
   ZH_SIZE nIndexes;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmMacroPushIndex()" ) );

   /*
    * Now the top most element on the stack points to number of
    * additional indexes to generated array
    */
   nIndexes = zh_itemGetNS( zh_stackItemFromTop( -1 ) );
   zh_stackDec();

   if( nIndexes > 1 )
   {
      PZH_ITEM pIndexArray;
      ZH_SIZE n = 1;

      zh_vmArrayGen( nIndexes - 1 );
      pIndexArray = zh_itemNew( zh_stackItemFromTop( -1 ) );
      zh_stackPop();

      /* First index is still on stack.*/
      do
      {
         PZH_ITEM pArray = zh_stackItemFromTop( -2 );
         if( ZH_IS_BYREF( pArray ) )
            zh_vmArrayPushRef();
         else
            zh_vmArrayPush();
         /* RT error? */
         if( zh_stackGetActionRequest() != 0 )
            break;
         zh_vmPush( zh_arrayGetItemPtr( pIndexArray, n ) );
      }
      while( ++n < nIndexes );

      zh_itemRelease( pIndexArray );
   }
   else if( nIndexes == 0 )
      ZH_VM_PUSHNIL();  /* It will force RT error later in array push or pop */
}

/*
 * On ZHVM stack we have sets with arguments
 *    offset   value
 *    (-9)     6
 *    (-8)     7
 *    (-7)     2 // number of arguments
 *    (-6)     1
 *    (-5)     2
 *    (-4)     2 // number of arguments
 *    (-3)     1
 *    (-2)     2
 *    (-1)     2 // number of arguments
 * we should join them into one continuous list
 */
static ZH_LONG zh_vmArgsJoin( ZH_LONG lLevel, ZH_USHORT uiArgSets )
{
   
   ZH_LONG lArgs;
   PZH_ITEM pArgs = zh_stackItemFromTop( lLevel ) ;

   lArgs = zh_itemGetNL( pArgs );
   if( ZH_IS_COMPLEX( pArgs ) )
      zh_itemClear( pArgs );

   if( --uiArgSets )
   {
      ZH_LONG lRestArgs, lOffset;

      lRestArgs = lArgs;
      lArgs += zh_vmArgsJoin( lLevel - lArgs - 1, uiArgSets );
      lOffset = lLevel - lRestArgs - uiArgSets;
      while( lRestArgs-- )
      {
         zh_itemMove( zh_stackItemFromTop( lOffset ),
                      zh_stackItemFromTop( lOffset + uiArgSets ) );
         ++lOffset;
      }
   }

   return lArgs;
}

static void zh_vmMacroDo( ZH_USHORT uiArgSets )
{
   
   ZH_LONG lArgs;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmMacroDo(%hu)", uiArgSets ) );

   lArgs = zh_vmArgsJoin( -1, uiArgSets );
   zh_stackDecrease( uiArgSets );
   zh_vmProc( ( ZH_USHORT ) lArgs );
}

static void zh_vmMacroFunc( ZH_USHORT uiArgSets )
{
   
   ZH_LONG lArgs;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmMacroFunc(%hu)", uiArgSets ) );

   lArgs = zh_vmArgsJoin( -1, uiArgSets );
   zh_stackDecrease( uiArgSets );
   zh_itemSetNil( zh_stackReturnItem() );
   zh_vmProc( ( ZH_USHORT ) lArgs );
   zh_stackPushReturn();
}

static void zh_vmMacroSend( ZH_USHORT uiArgSets )
{
   
   ZH_LONG lArgs;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmMacroSend(%hu)", uiArgSets ) );

   lArgs = zh_vmArgsJoin( -1, uiArgSets );
   zh_stackDecrease( uiArgSets );
   zh_itemSetNil( zh_stackReturnItem() );
   zh_vmSend( ( ZH_USHORT ) lArgs );
   zh_stackPushReturn();
}

static void zh_vmMacroArrayGen( ZH_USHORT uiArgSets )
{
   
   ZH_LONG lArgs;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmMacroArrayGen(%hu)", uiArgSets ) );

   lArgs = zh_vmArgsJoin( -1, uiArgSets );
   zh_stackDecrease( uiArgSets );
   zh_vmArrayGen( lArgs );
}

static void zh_vmPushVParams( void )
{
   
   int iPCount, iFirst, i = 0;
   PZH_ITEM pBase;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPushVParams()" ) );

   pBase = zh_stackBaseItem();
   iFirst = pBase->item.asSymbol.paramdeclcnt;
   iPCount = pBase->item.asSymbol.paramcnt;
   while( ++iFirst <= iPCount )
   {
      zh_vmPush( zh_stackItemFromBase( iFirst ) );
      i++;
   }
   zh_vmPushInteger( i );
}

static void zh_vmPushAParams( void )
{
   
   PZH_ITEM pArray;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPushAParams()" ) );

   pArray = zh_stackItemFromTop( -1 );
   if( ZH_IS_ARRAY( pArray ) )
   {
      ZH_SIZE nLen = pArray->item.asArray.value->nLen;

      if( nLen )
      {
         PZH_ITEM pCount;
         ZH_SIZE nPos;
         for( nPos = 1; nPos < nLen; ++nPos )
            zh_vmPush( pArray->item.asArray.value->pItems + nPos );
         pCount = zh_stackAllocItem();
         zh_itemCopy( pCount, pArray->item.asArray.value->pItems );
         zh_itemMove( pArray, pCount );
         zh_itemPutNS( pCount, nLen );
      }
      else
         zh_itemPutNL( pArray, 0 );
   }
   else
      zh_errRT_BASE( EG_ARG, 1068, NULL, zh_langDGetErrorDesc( EG_ARRACCESS ), 1, pArray );
}

/* ------------------------------- */
/* Database                        */
/* ------------------------------- */

static ZH_ERRCODE zh_vmSelectWorkarea( PZH_ITEM pAlias, PZH_SYMBOL pField )
{
   
   ZH_ERRCODE errCode;
   ZH_BOOL fRepeat;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmSelectWorkArea(%p,%p)", ( void * ) pAlias, ( void * ) pField ) );

   do
   {
      fRepeat = ZH_FALSE;
      errCode = ZH_SUCCESS;

      switch( ZH_ITEM_TYPE( pAlias ) )
      {
         case ZH_IT_INTEGER:
            /* Alias was used as integer value, for example: 4->field
             * or it was saved on the stack using zh_vmPushAlias()
             * or was evaluated from an expression, (nWorkArea)->field
             */
            zh_rddSelectWorkAreaNumber( pAlias->item.asInteger.value );
            pAlias->type = ZH_IT_NIL;
            break;

         case ZH_IT_LONG:
            /* Alias was evaluated from an expression, (nWorkArea)->field
             */
            zh_rddSelectWorkAreaNumber( ( int ) pAlias->item.asLong.value );
            pAlias->type = ZH_IT_NIL;
            break;

         case ZH_IT_DOUBLE:
            /* Alias was evaluated from an expression, (nWorkArea)->field
             */
            zh_rddSelectWorkAreaNumber( ( int ) pAlias->item.asDouble.value );
            pAlias->type = ZH_IT_NIL;
            break;

         case ZH_IT_SYMBOL:
            /* Alias was specified using alias identifier, for example: al->field
             */
            errCode = zh_rddSelectWorkAreaSymbol( pAlias->item.asSymbol.value );
            pAlias->type = ZH_IT_NIL;
            break;

         case ZH_IT_STRING:
         {
            /* Alias was evaluated from an expression, for example: (cVar)->field
             */
            /* expand '&' operator if exists */
            char * szAlias;
            ZH_BOOL bNewString;

            szAlias = zh_macroExpandString( pAlias->item.asString.value, pAlias->item.asString.length, &bNewString );
            if( pField )
            {
               errCode = zh_rddSelectWorkAreaAlias( szAlias );
            }
            else
            {
               int iArea;
               zh_rddGetAliasNumber( szAlias, &iArea );
               zh_rddSelectWorkAreaNumber( iArea );
            }

            if( bNewString )
               zh_xfree( szAlias );
            zh_itemClear( pAlias );
            break;
         }

         default:
            if( pField )
            {
               PZH_ITEM pSubstVal;

               zh_vmPushString( pField->szName, strlen( pField->szName ) );
               pSubstVal = zh_errRT_BASE_Subst( EG_ARG, 1065, NULL, "&",
                                       2, pAlias, zh_stackItemFromTop( -1 ) );
               zh_stackPop();
               if( pSubstVal )
               {
                  zh_itemMove( pAlias, pSubstVal );
                  zh_itemRelease( pSubstVal );
                  fRepeat = ZH_TRUE;
               }
               else
               {
                  zh_itemSetNil( pAlias );
                  errCode = ZH_FAILURE;
               }
            }
            else
            {
               zh_rddSelectWorkAreaNumber( -1 );
               zh_itemSetNil( pAlias );
            }
            break;
      }
   }
   while( fRepeat );

   return errCode;
}

/* Swaps two last items on the eval stack - the last item after swapping
 * is popped as current workarea number
 */
static void zh_vmSwapAlias( void )
{
   
   PZH_ITEM pItem;
   PZH_ITEM pWorkArea;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmSwapAlias()" ) );

   pItem = zh_stackItemFromTop( -1 );
   pWorkArea = zh_stackItemFromTop( -2 );

   zh_vmSelectWorkarea( pWorkArea, NULL );

   zh_itemMove( pWorkArea, pItem );
   zh_stackDec();
}

/* ------------------------------- */
/* Execution                       */
/* ------------------------------- */

void zh_vmProc( ZH_USHORT uiParams )
{
   ZH_STACK_STATE sStackState;
   PZH_SYMBOL pSym;

#ifndef ZH_NO_PROFILER
   ZH_ULONG ulClock = 0;
   ZH_BOOL bProfiler = zh_bProfiler; /* because profiler state may change */
#endif

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmProc(%hu)", uiParams ) );

   ZH_TASK_SHEDULER();

#ifndef ZH_NO_PROFILER
   if( bProfiler )
      ulClock = ( ZH_ULONG ) clock();
#endif

   pSym = zh_stackNewFrame( &sStackState, uiParams )->item.asSymbol.value;
   ZH_VM_FUNCUNREF( pSym );
   if( ZH_VM_ISFUNC( pSym ) )
   {
      ZH_TRACE_PRG( ( "Calling: %s", pSym->szName ) );

#ifndef ZH_NO_PROFILER
      if( bProfiler && pSym->pDynSym )
         pSym->pDynSym->ulRecurse++;
#endif

      ZH_VM_EXECUTE( pSym );

#ifndef ZH_NO_PROFILER
      if( bProfiler && pSym->pDynSym )
      {
         pSym->pDynSym->ulCalls++;                   /* profiler support */
         /* Time spent has to be added only inside topmost call of a recursive function */
         if( --pSym->pDynSym->ulRecurse == 0 )
            pSym->pDynSym->ulTime += clock() - ulClock;  /* profiler support */
      }
#endif
   }
   else
      zh_errRT_BASE_SubstR( EG_NOFUNC, 1001, NULL, pSym->szName, ZH_ERR_ARGS_BASEPARAMS );

   if( sStackState.fDebugging )
      zh_vmDebuggerEndProc();

   zh_stackOldFrame( &sStackState );
}

void zh_vmDo( ZH_USHORT uiParams )
{
   
   ZH_STACK_STATE sStackState;
   PZH_SYMBOL pSym;
   PZH_ITEM pSelf;

#ifndef ZH_NO_PROFILER
   ZH_ULONG ulClock = 0;
   ZH_BOOL bProfiler = zh_bProfiler; /* because profiler state may change */
#endif

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmDo(%hu)", uiParams ) );

   ZH_TASK_SHEDULER();

#ifndef ZH_NO_PROFILER
   if( bProfiler )
      ulClock = ( ZH_ULONG ) clock();
#endif


   pSym = zh_stackNewFrame( &sStackState, uiParams )->item.asSymbol.value;
   pSelf = zh_stackSelfItem();   /* NIL, OBJECT or BLOCK */

   if( ! ZH_IS_NIL( pSelf ) )  /* are we sending a message ? */
   {
      PZH_SYMBOL pExecSym;

      pExecSym = zh_objGetMethod( pSelf, pSym, &sStackState );
      if( pExecSym )
         ZH_VM_FUNCUNREF( pExecSym );
      if( pExecSym && ZH_VM_ISFUNC( pExecSym ) )
      {
         ZH_TRACE_PRG( ( "Calling: %s:%s", zh_objGetClsName( pSelf ), pSym->szName ) );

         ZH_VM_EXECUTE( pExecSym );

#ifndef ZH_NO_PROFILER
         if( bProfiler )
            zh_mthAddTime( clock() - ulClock );
#endif
      }
      else if( pSym->szName[ 0 ] == '_' )
         zh_errRT_BASE_SubstR( EG_NOVARMETHOD, 1005, NULL, pSym->szName + 1, ZH_ERR_ARGS_SELFPARAMS );
      else
         zh_errRT_BASE_SubstR( EG_NOMETHOD, 1004, NULL, pSym->szName, ZH_ERR_ARGS_SELFPARAMS );
   }
   else /* it is a function */
   {
      ZH_VM_FUNCUNREF( pSym );
      if( ZH_VM_ISFUNC( pSym ) )
      {
         ZH_TRACE_PRG( ( "Calling: %s", pSym->szName ) );

#ifndef ZH_NO_PROFILER
         if( bProfiler && pSym->pDynSym )
            pSym->pDynSym->ulRecurse++;
#endif

         ZH_VM_EXECUTE( pSym );

#ifndef ZH_NO_PROFILER
         if( bProfiler && pSym->pDynSym )
         {
            pSym->pDynSym->ulCalls++;                   /* profiler support */
            /* Time spent has to be added only inside topmost call of a recursive function */
            if( --pSym->pDynSym->ulRecurse == 0 )
               pSym->pDynSym->ulTime += clock() - ulClock;  /* profiler support */
         }
#endif
      }
      else
         zh_errRT_BASE_SubstR( EG_NOFUNC, 1001, NULL, pSym->szName, ZH_ERR_ARGS_BASEPARAMS );
   }

   if( sStackState.fDebugging )
      zh_vmDebuggerEndProc();

   zh_stackOldFrame( &sStackState );
}

void zh_vmSend( ZH_USHORT uiParams )
{
   
   ZH_STACK_STATE sStackState;
   PZH_SYMBOL pSym;
   PZH_SYMBOL pExecSym;
   PZH_ITEM pSelf;

#ifndef ZH_NO_PROFILER
   ZH_ULONG ulClock = 0;
   ZH_BOOL bProfiler = zh_bProfiler; /* because profiler state may change */
#endif

   ZH_TASK_SHEDULER();

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmSend(%hu)", uiParams ) );

#ifndef ZH_NO_PROFILER
   if( bProfiler )
      ulClock = ( ZH_ULONG ) clock();
#endif

   /* Poll the console keyboard */
#if 0
   #if ! defined( ZH_GUI )
      zh_inkeyPoll();
   #endif
#endif

   pSym = zh_stackNewFrame( &sStackState, uiParams )->item.asSymbol.value;
   pSelf = zh_stackSelfItem();   /* NIL, OBJECT or BLOCK */

   pExecSym = zh_objGetMethod( pSelf, pSym, &sStackState );
   if( pExecSym )
      ZH_VM_FUNCUNREF( pExecSym );
   if( pExecSym && ZH_VM_ISFUNC( pExecSym ) )
   {
      ZH_TRACE_PRG( ( "Calling: %s:%s", zh_objGetClsName( pSelf ), pSym->szName ) );

      ZH_VM_EXECUTE( pExecSym );

#ifndef ZH_NO_PROFILER
      if( bProfiler )
         zh_mthAddTime( clock() - ulClock );
#endif
   }
   else if( pSym->szName[ 0 ] == '_' )
      zh_errRT_BASE_SubstR( EG_NOVARMETHOD, 1005, NULL, pSym->szName + 1, ZH_ERR_ARGS_SELFPARAMS );
   else
      zh_errRT_BASE_SubstR( EG_NOMETHOD, 1004, NULL, pSym->szName, ZH_ERR_ARGS_SELFPARAMS );

   if( sStackState.fDebugging )
      zh_vmDebuggerEndProc();

   zh_stackOldFrame( &sStackState );
}

static void zh_vmPushObjectVarRef( void )
{
   
   ZH_STACK_STATE sStackState;
   PZH_ITEM pItem;
   PZH_SYMBOL pSym;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPushObjectVarRef()" ) );

   pItem = zh_stackNewFrame( &sStackState, 0 );   /* procedure name */
   pSym = pItem->item.asSymbol.value;

   if( ! zh_objGetVarRef( zh_stackSelfItem(), pSym, &sStackState ) &&
       zh_vmRequestQuery() == 0 )
      zh_errRT_BASE_SubstR( EG_NOVARMETHOD, 1005, NULL, pSym->szName + ( pSym->szName[ 0 ] == '_' ? 1 : 0 ), 1, zh_stackSelfItem() );

   zh_stackOldFrame( &sStackState );

   zh_stackPushReturn();
}

void zh_vmEval( ZH_USHORT uiParams )
{
   ZH_STACK_STATE sStackState;

#ifndef ZH_NO_PROFILER
   ZH_ULONG ulClock = 0;
   ZH_BOOL bProfiler = zh_bProfiler; /* because profiler state may change */
#endif

   ZH_TASK_SHEDULER();

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmEval(%hu)", uiParams ) );

#ifndef ZH_NO_PROFILER
   if( bProfiler )
      ulClock = ( ZH_ULONG ) clock();
#endif

   zh_stackNewFrame( &sStackState, uiParams );

   zh_vmDoBlock();

#ifndef ZH_NO_PROFILER
   if( bProfiler )
      zh_mthAddTime( clock() - ulClock );
#endif

   if( sStackState.fDebugging )
      zh_vmDebuggerEndProc();

   zh_stackOldFrame( &sStackState );
}

//static ZIHERF zh_vmDoBlock( void )
ZIHERF zh_vmDoBlock( void )
{
   
   PZH_ITEM pBlock, pBase;
   int iParam;

   //ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmDoBlock()" ) );

   pBlock = zh_stackSelfItem();
   if( ! ZH_IS_BLOCK( pBlock ) )
      zh_errInternal( ZH_EI_VMNOTCBLOCK, NULL, "zh_vmDoBlock()", NULL );

   pBase = zh_stackBaseItem();

   /* set number of declared parameters */
   pBase->item.asSymbol.paramdeclcnt = pBlock->item.asBlock.paramcnt;
   /* set the current line number to a line where the codeblock was defined */
   pBase->item.asSymbol.stackstate->uiLineNo = pBlock->item.asBlock.lineno;
   /* set execution context for OOP scope */
   pBase->item.asSymbol.stackstate->uiClass  = pBlock->item.asBlock.hclass;
   pBase->item.asSymbol.stackstate->uiMethod = pBlock->item.asBlock.method;
   /* add missing parameters */
   iParam = pBlock->item.asBlock.paramcnt - pBase->item.asSymbol.paramcnt;
   while( --iParam >= 0 )
      zh_stackAllocItem()->type = ZH_IT_NIL;
   /* set static base offset */
   zh_stackSetStaticsBase( pBlock->item.asBlock.value->pStatics );

   zh_vmExecute( pBlock->item.asBlock.value->pCode,
                 pBlock->item.asBlock.value->pSymbols );
}

/* Evaluates a passed codeblock item with no arguments passed to a codeblock
 */
PZH_ITEM zh_vmEvalBlock( PZH_ITEM pBlock )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmEvalBlock(%p)", ( void * ) pBlock ) );

   zh_vmPushEvalSym();
   zh_vmPush( pBlock );
   zh_vmSend( 0 );
   return zh_stackReturnItem();
}

/* Evaluates a codeblock item using passed additional arguments
 * pBlock = an item of codeblock type to evaluate
 * ulArgCount = number of arguments passed to a codeblock
 * ... = the list of arguments of type PZH_ITEM
 *
 * for example:
 *  retVal = zh_vmEvalBlockV( pBlock, 2, pParam1, pParam2 );
 */
PZH_ITEM zh_vmEvalBlockV( PZH_ITEM pBlock, ZH_ULONG ulArgCount, ... )
{
   
   va_list va;
   ZH_ULONG i;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmEvalBlockV(%p, %lu, ...)", ( void * ) pBlock, ulArgCount ) );

   zh_vmPushEvalSym();
   zh_vmPush( pBlock );

   va_start( va, ulArgCount );
   for( i = 1; i <= ulArgCount; i++ )
      zh_vmPush( va_arg( va, PZH_ITEM ) );
   va_end( va );

   /* take care here, possible loss of data long to short ... */
   /* added an explicit casting here for VC++ JFL */
   zh_vmSend( ( ZH_USHORT ) ulArgCount );

   return zh_stackReturnItem();
}

/* Evaluates a passed codeblock item or macro pointer item
 */
PZH_ITEM zh_vmEvalBlockOrMacro( PZH_ITEM pItem )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmEvalBlockOrMacro(%p)", ( void * ) pItem ) );

   if( ZH_IS_BLOCK( pItem ) )
   {
      zh_vmPushEvalSym();
      zh_vmPush( pItem );
      zh_vmEval( 0 );
   }
   else
   {
      PZH_MACRO pMacro = ( PZH_MACRO ) zh_itemGetPtr( pItem );
      if( pMacro )
      {
         zh_macroRun( pMacro );
         zh_stackPopReturn();
      }
      else
         zh_itemSetNil( zh_stackReturnItem() );
   }
   return zh_stackReturnItem();
}

/*
 * destroy codeblock or macro in given item
 */
void zh_vmDestroyBlockOrMacro( PZH_ITEM pItem )
{
   if( ZH_IS_POINTER( pItem ) )
   {
      PZH_MACRO pMacro = ( PZH_MACRO ) zh_itemGetPtr( pItem );
      if( pMacro )
         zh_macroDelete( pMacro );
   }
   zh_itemRelease( pItem );
}



void zh_vmFunction( ZH_USHORT uiParams )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmFunction(%hu)", uiParams ) );

   zh_itemSetNil( zh_stackReturnItem() );
   zh_vmDo( uiParams );
}


static void zh_vmDebugEntry( int nMode, int nLine, const char * szName, int nIndex, PZH_ITEM pFrame )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmDebugEntry" ) );

   switch( nMode )
   {
      case ZH_DBG_MODULENAME:
         zh_vmPushDynSym( s_pDynsDbgEntry );
         ZH_VM_PUSHNIL();
         zh_vmPushInteger( ZH_DBG_MODULENAME );
         zh_vmPushString( szName, strlen( szName ) );
         zh_vmProc( 2 );
         break;

      case ZH_DBG_LOCALNAME:
         zh_vmPushDynSym( s_pDynsDbgEntry );
         ZH_VM_PUSHNIL();
         zh_vmPushInteger( ZH_DBG_LOCALNAME );
         zh_vmPushInteger( nIndex );
         zh_vmPushString( szName, strlen( szName ) );
         zh_vmProc( 3 );
         break;

      case ZH_DBG_STATICNAME:
         zh_vmPushDynSym( s_pDynsDbgEntry );
         ZH_VM_PUSHNIL();
         zh_vmPushInteger( ZH_DBG_STATICNAME );
         zh_vmPush( pFrame );                   /* current static frame */
         zh_vmPushInteger( nIndex );            /* variable index */
         zh_vmPushString( szName, strlen( szName ) );
         zh_vmProc( 4 );
         break;

      case ZH_DBG_SHOWLINE:
         zh_vmPushDynSym( s_pDynsDbgEntry );
         ZH_VM_PUSHNIL();
         zh_vmPushInteger( ZH_DBG_SHOWLINE );
         zh_vmPushInteger( nLine );
         zh_vmProc( 2 );
         break;

      case ZH_DBG_ENDPROC:
         zh_stackPushReturn();      /* saves the previous returned value */
         zh_vmPushDynSym( s_pDynsDbgEntry );
         ZH_VM_PUSHNIL();
         zh_vmPushInteger( ZH_DBG_ENDPROC );
         zh_vmProc( 1 );
         zh_stackPopReturn();       /* restores the previous returned value */
         break;

      case ZH_DBG_GETENTRY:
         /* Try to get C dbgEntry() function pointer */
         zh_vmPushDynSym( s_pDynsDbgEntry );
         ZH_VM_PUSHNIL();
         zh_vmPushInteger( ZH_DBG_GETENTRY );
         zh_vmProc( 1 );
         break;

      case ZH_DBG_VMQUIT:
         zh_vmPushDynSym( s_pDynsDbgEntry );
         ZH_VM_PUSHNIL();
         zh_vmPushInteger( ZH_DBG_VMQUIT );
         zh_vmPushInteger( nIndex );
         zh_vmProc( 2 );
         break;
   }
}

static void zh_vmDummyDebugEntry( int nMode, int nLine, const char * szName, int nIndex, PZH_ITEM pFrame )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmDummyDebugEntry" ) );

   ZH_SYMBOL_UNUSED( nMode );
   ZH_SYMBOL_UNUSED( nLine );
   ZH_SYMBOL_UNUSED( szName );
   ZH_SYMBOL_UNUSED( nIndex );
   ZH_SYMBOL_UNUSED( pFrame );
}

static void zh_vmDebuggerExit( ZH_BOOL fRemove )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmDebuggerExit(%d)", fRemove ) );

   /* is debugger linked ? */
   if( s_pFunDbgEntry )
   {
      /* inform debugger that we are quitting now */
      s_pFunDbgEntry( ZH_DBG_VMQUIT, 0, NULL, fRemove ? 1 : 0, NULL );
      /* set dummy debugger function to avoid debugger activation in .zh
       *       destructors if any */
      if( fRemove )
         s_pFunDbgEntry = zh_vmDummyDebugEntry;
   }
}

static void zh_vmDebuggerEndProc( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmDebuggerEndProc()" ) );

   s_pFunDbgEntry( ZH_DBG_ENDPROC, 0, NULL, 0, NULL );
}

static void zh_vmDebuggerShowLine( ZH_USHORT uiLine ) /* makes the debugger shows a specific source code line */
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmDebuggerShowLine(%hu)", uiLine ) );

   s_pFunDbgEntry( ZH_DBG_SHOWLINE, uiLine, NULL, 0, NULL );
}

static void zh_vmLocalName( ZH_USHORT uiLocal, const char * szLocalName ) /* locals and parameters index and name information for the debugger */
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmLocalName(%hu, %s)", uiLocal, szLocalName ) );

   if( zh_stackBaseItem()->item.asSymbol.stackstate->fDebugging )
      s_pFunDbgEntry( ZH_DBG_LOCALNAME, 0, szLocalName, uiLocal, NULL );
}

static void zh_vmStaticName( ZH_BYTE bIsGlobal, ZH_USHORT uiStatic, const char * szStaticName ) /* statics vars information for the debugger */
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmStaticName(%hu, %s)", uiStatic, szStaticName ) );

   ZH_SYMBOL_UNUSED( bIsGlobal );

   if( zh_stackBaseItem()->item.asSymbol.stackstate->fDebugging )
      s_pFunDbgEntry( ZH_DBG_STATICNAME, 0, szStaticName, uiStatic, ( PZH_ITEM ) zh_stackGetStaticsBase() );
}

static void zh_vmModuleName( const char * szModuleName ) /* ZH and function name information for the debugger */
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmModuleName(%s)", szModuleName ) );

   if( s_pFunDbgEntry )
   {
      
      s_pFunDbgEntry( ZH_DBG_MODULENAME, 0, szModuleName, 0, NULL );
      zh_stackBaseItem()->item.asSymbol.stackstate->fDebugging = ZH_TRUE;
   }
}

static void zh_vmFrame( ZH_USHORT usLocals, unsigned char ucParams )
{
   
   PZH_ITEM pBase;
   int iTotal;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmFrame(%d, %d)", ( int ) usLocals, ( int ) ucParams ) );

   pBase = zh_stackBaseItem();

   pBase->item.asSymbol.paramdeclcnt = ucParams;

   iTotal = ucParams - pBase->item.asSymbol.paramcnt;
   if( iTotal < 0 )
      iTotal = 0;
   iTotal += usLocals;

   while( --iTotal >= 0 )
      ZH_VM_PUSHNIL();
}

static void zh_vmVFrame( ZH_USHORT usLocals, unsigned char ucParams )
{
   
   PZH_ITEM pBase;
   int iTotal;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmVFrame(%d, %d)", ( int ) usLocals, ( int ) ucParams ) );

   pBase = zh_stackBaseItem();

   pBase->item.asSymbol.paramdeclcnt = ucParams;

   iTotal = ucParams - pBase->item.asSymbol.paramcnt;
   if( iTotal < 0 )
      iTotal = 0;
   iTotal += usLocals;

   while( --iTotal >= 0 )
      ZH_VM_PUSHNIL();
}

static void zh_vmSFrame( PZH_SYMBOL pSym )      /* sets the statics frame for a function */
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmSFrame(%p)", ( void * ) pSym ) );

   /* _INITSTATICS is now the statics frame. Statics() changed it! */
   zh_stackSetStaticsBase( pSym->value.pStaticsBase );
}

static void zh_vmStatics( PZH_SYMBOL pSym, ZH_USHORT uiStatics ) /* initializes the global aStatics array or redimensions it */
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmStatics(%p, %hu)", ( void * ) pSym, uiStatics ) );

   /* statics frame for this ZH */
   pSym->value.pStaticsBase = ( void * ) zh_itemArrayNew( uiStatics );
   pSym->scope.value |= ZH_FS_FRAME;
}

/*
 * extended thread static variable reference structure
 */
typedef struct
{
   ZH_ITEM source;
   ZH_TSD  threadData;
} ZH_TSVREF, * PZH_TSVREF;

/*
 * extended thread static variable reference functions
 */
static PZH_ITEM zh_vmTSVRefRead( PZH_ITEM pRefer )
{
   PZH_TSVREF pTSVRef = ( PZH_TSVREF ) pRefer->item.asExtRef.value;
   PZH_ITEM pItem = ( PZH_ITEM ) zh_stackTestTSD( &pTSVRef->threadData );

   if( ! pItem )
   {
      pItem = ( PZH_ITEM ) zh_stackGetTSD( &pTSVRef->threadData );
      zh_itemCloneTo( pItem, &pTSVRef->source );
   }
   return pItem;
}

static PZH_ITEM zh_vmTSVRefWrite( PZH_ITEM pRefer, PZH_ITEM pSource )
{
   PZH_TSVREF pTSVRef = ( PZH_TSVREF ) pRefer->item.asExtRef.value;

   ZH_SYMBOL_UNUSED( pSource );
   return ( PZH_ITEM ) zh_stackGetTSD( &pTSVRef->threadData );
}

static void zh_vmTSVRefCopy( PZH_ITEM pDest )
{
   zh_xRefInc( pDest->item.asExtRef.value );
}

static void zh_vmTSVRefClear( void * value )
{
   if( zh_xRefDec( value ) )
   {
      if( ZH_IS_COMPLEX( &( ( PZH_TSVREF ) value )->source ) )
         zh_itemClear( &( ( PZH_TSVREF ) value )->source );

      zh_stackReleaseTSD( &( ( PZH_TSVREF ) value )->threadData );

      zh_xfree( value );
   }
}

static void zh_vmTSVRefMark( void * value )
{
   PZH_ITEM pItem;

   if( ZH_IS_GCITEM( &( ( PZH_TSVREF ) value )->source ) )
      zh_gcItemRef( &( ( PZH_TSVREF ) value )->source );

   pItem = ( PZH_ITEM ) zh_stackTestTSD( &( ( PZH_TSVREF ) value )->threadData );
   if( pItem && ZH_IS_GCITEM( pItem ) )
      zh_gcItemRef( pItem );
}

/* destructor for terminated threads */
static void zh_vmTSVarClean( void * pThreadItem )
{
   if( ZH_IS_COMPLEX( ( PZH_ITEM ) pThreadItem ) )
      zh_itemClear( ( PZH_ITEM ) pThreadItem );
}

/*
 * create extended thread static variable reference
 */
static void zh_vmTSVReference( PZH_ITEM pStatic )
{
   static const ZH_EXTREF s_TSVExtRef = {
      zh_vmTSVRefRead,
      zh_vmTSVRefWrite,
      zh_vmTSVRefCopy,
      zh_vmTSVRefClear,
      zh_vmTSVRefMark
   };

   
   PZH_TSVREF pTSVRef;
   PZH_ITEM pRefer;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmTSVReference(%p)", ( void * ) pStatic ) );

   pTSVRef = ( PZH_TSVREF ) zh_xgrab( sizeof( ZH_TSVREF ) );

   pTSVRef->source.type = ZH_IT_NIL;
   ZH_TSD_INIT( &pTSVRef->threadData, sizeof( ZH_ITEM ), NULL, zh_vmTSVarClean );

   /* Use zh_stackReturnItem() as temporary item holder */
   pRefer = zh_stackReturnItem();
   if( ZH_IS_COMPLEX( pRefer ) )
      zh_itemClear( pRefer );
   pRefer->type = ZH_IT_BYREF | ZH_IT_EXTREF;
   pRefer->item.asExtRef.value = ( void * ) pTSVRef;
   pRefer->item.asExtRef.func = &s_TSVExtRef;

   zh_itemMove( &pTSVRef->source, pStatic );
   zh_itemMove( pStatic, pRefer );
}

static void zh_vmInitThreadStatics( ZH_USHORT uiCount, const ZH_BYTE * pCode )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmInitThreadStatics(%hu,%p)", uiCount, ( const void * ) pCode ) );

   while( uiCount-- )
   {
      ZH_USHORT uiStatic = ZH_PCODE_MKUSHORT( pCode );
      PZH_ITEM pStatic = ( ( PZH_ITEM ) zh_stackGetStaticsBase() )->item.asArray.value->pItems + uiStatic - 1;
      zh_vmTSVReference( pStatic );
      pCode += 2;
   }
}

/* ------------------------------- */
/* Push                            */
/* ------------------------------- */

void zh_vmPush( PZH_ITEM pItem )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPush(%p)", ( void * ) pItem ) );

   zh_itemCopy( zh_stackAllocItem(), pItem );
}

void zh_vmPushNil( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPushNil()" ) );

   zh_stackAllocItem()->type = ZH_IT_NIL;
}

void zh_vmPushLogical( ZH_BOOL bValue )
{
   
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPushLogical(%d)", ( int ) bValue ) );

   pItem = zh_stackAllocItem();
   pItem->type = ZH_IT_LOGICAL;
   pItem->item.asLogical.value = bValue;
}

/* not used by ZHVM code */
void zh_vmPushNumber( double dNumber, int iDec )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPushNumber(%lf, %d)", dNumber, iDec ) );

   if( iDec )
      zh_vmPushDouble( dNumber, iDec );

   else if( ZH_DBL_LIM_INT( dNumber ) )
      zh_vmPushInteger( ( int ) dNumber );

   else if( ZH_DBL_LIM_LONG( dNumber ) )
      zh_vmPushZHLong( ( ZH_MAXINT ) dNumber );

   else
      zh_vmPushDouble( dNumber, zh_stackSetStruct()->ZH_SET_DECIMALS );
}

static int zh_vmCalcIntWidth( ZH_MAXINT nNumber )
{
   int iWidth;

   if( nNumber <= -1000000000L )
   {
      iWidth = 20;
   }
   else
   {
      iWidth = 10;
      while( nNumber >= 1000000000L )
      {
         iWidth++;
         nNumber /= 10;
      }
   }
   return iWidth;
}

void zh_vmPushInteger( int iNumber )
{
   
   PZH_ITEM pItem = zh_stackAllocItem();

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPushInteger(%d)", iNumber ) );

   pItem->type = ZH_IT_INTEGER;
   pItem->item.asInteger.value = iNumber;
   pItem->item.asInteger.length = ZH_INT_LENGTH( iNumber );
}

#if ZH_VMINT_MAX >= INT32_MAX
static void zh_vmPushIntegerConst( int iNumber )
{
   
   PZH_ITEM pItem = zh_stackAllocItem();

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPushIntegerConst(%d)", iNumber ) );

   pItem->type = ZH_IT_INTEGER;
   pItem->item.asInteger.value = iNumber;
   pItem->item.asInteger.length = ( ZH_USHORT ) zh_vmCalcIntWidth( iNumber );
}
#else
static void zh_vmPushLongConst( long lNumber )
{
   
   PZH_ITEM pItem = zh_stackAllocItem();

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPushLongConst(%ld)", lNumber ) );

   pItem->type = ZH_IT_LONG;
   pItem->item.asLong.value = ( ZH_MAXINT ) lNumber;
   pItem->item.asLong.length = ( ZH_USHORT ) zh_vmCalcIntWidth( lNumber );
}
#endif


void zh_vmPushLong( long lNumber )
{
   
   PZH_ITEM pItem = zh_stackAllocItem();

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPushLong(%ld)", lNumber ) );

   ZH_ITEM_PUT_LONGRAW( pItem, lNumber );
}

void zh_vmPushSize( ZH_I_SIZE nNumber )
{
#if ZH_SIZE_MAX <= ZH_VMUINT_MAX
   zh_vmPushInteger( ( int ) nNumber );
#else
   if( ZH_LIM_INT( nNumber ) )
      zh_vmPushInteger( ( int ) nNumber );
   else
      zh_vmPushZHLong( nNumber );
#endif
}

static void zh_vmPushZHLong( ZH_MAXINT nNumber )
{
   
   PZH_ITEM pItem = zh_stackAllocItem();

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPushZHLong(%" PFHL "d)", nNumber ) );

   pItem->type = ZH_IT_LONG;
   pItem->item.asLong.value = nNumber;
   pItem->item.asLong.length = ZH_LONG_LENGTH( nNumber );
}

#if ! defined( ZH_LONG_LONG_OFF )
static void zh_vmPushLongLongConst( ZH_LONGLONG llNumber )
{
   
   PZH_ITEM pItem = zh_stackAllocItem();

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPushLongLongConst(%" PFLL "d)", llNumber ) );

   pItem->type = ZH_IT_LONG;
   pItem->item.asLong.value = ( ZH_MAXINT ) llNumber;
   pItem->item.asLong.length = ( ZH_USHORT ) zh_vmCalcIntWidth( llNumber );
}
#endif

void zh_vmPushNumInt( ZH_MAXINT nNumber )
{
   if( ZH_LIM_INT( nNumber ) )
      zh_vmPushInteger( ( int ) nNumber );
   else
      zh_vmPushZHLong( nNumber );
}

void zh_vmPushDouble( double dNumber, int iDec )
{
   
   PZH_ITEM pItem = zh_stackAllocItem();

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPushDouble(%lf, %d)", dNumber, iDec ) );

   pItem->type = ZH_IT_DOUBLE;
   pItem->item.asDouble.value = dNumber;
   pItem->item.asDouble.length = ZH_DBL_LENGTH( dNumber );
   if( iDec == ZH_DEFAULT_DECIMALS )
      pItem->item.asDouble.decimal = ( ZH_USHORT ) zh_stackSetStruct()->ZH_SET_DECIMALS;
   else
      pItem->item.asDouble.decimal = ( ZH_USHORT ) iDec;
}

static void zh_vmPushDoubleConst( double dNumber, int iWidth, int iDec )
{
   
   PZH_ITEM pItem = zh_stackAllocItem();

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPushDoubleConst(%lf, %d, %d)", dNumber, iWidth, iDec ) );

   pItem->type = ZH_IT_DOUBLE;
   pItem->item.asDouble.value = dNumber;

   if( iDec == ZH_DEFAULT_DECIMALS )
      pItem->item.asDouble.decimal = ( ZH_USHORT ) zh_stackSetStruct()->ZH_SET_DECIMALS;
   else
      pItem->item.asDouble.decimal = ( ZH_USHORT ) iDec;

   if( iWidth == ZH_DEFAULT_WIDTH )
      pItem->item.asDouble.length = ZH_DBL_LENGTH( dNumber );
   else
      pItem->item.asDouble.length = ( ZH_USHORT ) iWidth;
}

void zh_vmPushDate( long lDate )
{
   
   PZH_ITEM pItem = zh_stackAllocItem();

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPushDate(%ld)", lDate ) );

   pItem->type = ZH_IT_DATE;
   pItem->item.asDateTime.julian = lDate;
   pItem->item.asDateTime.time = 0;
}

void zh_vmPushTimeStamp( long lJulian, long lMilliSec )
{
   
   PZH_ITEM pItem = zh_stackAllocItem();

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPushTimeStamp(%ld, %ld)", lJulian, lMilliSec ) );

   pItem->type = ZH_IT_TIMESTAMP;
   pItem->item.asDateTime.julian = lJulian;
   pItem->item.asDateTime.time = lMilliSec;
}

void zh_vmPushPointer( void * pPointer )
{
   
   PZH_ITEM pItem = zh_stackAllocItem();

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPushPointer(%p)", ( void * ) pPointer ) );

   pItem->type = ZH_IT_POINTER;
   pItem->item.asPointer.value = pPointer;
   pItem->item.asPointer.collect =
   pItem->item.asPointer.single = ZH_FALSE;
}

void zh_vmPushPointerGC( void * pPointer )
{
   
   PZH_ITEM pItem = zh_stackAllocItem();

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPushPointerGC(%p)", ( void * ) pPointer ) );

   pItem->type = ZH_IT_POINTER;
   pItem->item.asPointer.value = pPointer;
   pItem->item.asPointer.collect = ZH_TRUE;
   pItem->item.asPointer.single = ZH_FALSE;

   zh_gcAttach( pPointer );
}

void zh_vmPushString( const char * szText, ZH_SIZE nLength )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPushString(%s, %" ZH_PFS "u)", szText, nLength ) );

   zh_itemPutCL( zh_stackAllocItem(), szText, nLength );
}

void zh_vmPushStringPcode( const char * szText, ZH_SIZE nLength )
{
   
   PZH_ITEM pItem = zh_stackAllocItem();

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPushStringPcode(%s, %" ZH_PFS "u)", szText, nLength ) );

   pItem->type = ZH_IT_STRING;
   pItem->item.asString.allocated = 0;
   pItem->item.asString.length = nLength;
   pItem->item.asString.value = ( char * ) ZH_UNCONST( ( nLength <= 1 ?
                        zh_szAscii[ ( unsigned char ) szText[ 0 ] ] : szText ) );
}

void zh_vmPushSymbol( PZH_SYMBOL pSym )
{
   
   PZH_ITEM pItem = zh_stackAllocItem();

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPushSymbol(%p)", ( void * ) pSym ) );

   pItem->type = ZH_IT_SYMBOL;
   pItem->item.asSymbol.value = pSym;
   pItem->item.asSymbol.stackstate = NULL;
}

void zh_vmPushDynSym( PZH_DYNSYMBOL pDynSym )
{
   
   PZH_ITEM pItem = zh_stackAllocItem();

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPushDynSym(%p)", ( void * ) pDynSym ) );

   pItem->type = ZH_IT_SYMBOL;
   pItem->item.asSymbol.value = pDynSym->pSymbol;
   pItem->item.asSymbol.stackstate = NULL;
}

void zh_vmPushEvalSym( void )
{
   
   PZH_ITEM pItem = zh_stackAllocItem();

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPushEvalSym()" ) );

   pItem->type = ZH_IT_SYMBOL;
   pItem->item.asSymbol.value = pZhSymEval;
   pItem->item.asSymbol.stackstate = NULL;
}

/* -3    -> ZH_P_PUSH_BLOCK
 * -2 -1 -> size of codeblock
 *  0 +1 -> number of expected parameters
 * +2 +3 -> number of referenced local variables
 * +4    -> start of table with referenced local variables
 *
 * NOTE: pCode points to static memory
 */
static void zh_vmPushBlock( const ZH_BYTE * pCode, PZH_SYMBOL pSymbols, ZH_SIZE nLen )
{
   
   ZH_USHORT uiLocals;
   PZH_ITEM pItem = zh_stackAllocItem();

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPushBlock(%p,%p,%" ZH_PFS "u)", ( const void * ) pCode, ( void * ) pSymbols, nLen ) );

   uiLocals = ZH_PCODE_MKUSHORT( &pCode[ 2 ] );

   if( nLen )
      nLen -= uiLocals << 1;

   pItem->item.asBlock.value =
      zh_codeblockNew( pCode + 4 + ( uiLocals << 1 ),   /* pcode buffer         */
                       uiLocals,                        /* number of referenced local variables */
                       pCode + 4,                       /* table with referenced local variables */
                       pSymbols,
                       nLen );

   pItem->type = ZH_IT_BLOCK;
   /* store the number of expected parameters
    */
   pItem->item.asBlock.paramcnt = ZH_PCODE_MKUSHORT( pCode );
   /* store the line number where the codeblock was defined
    */
   pItem->item.asBlock.lineno = zh_stackBaseItem()->item.asSymbol.stackstate->uiLineNo;
   pItem->item.asBlock.hclass = zh_stackBaseItem()->item.asSymbol.stackstate->uiClass;
   pItem->item.asBlock.method = zh_stackBaseItem()->item.asSymbol.stackstate->uiMethod;
}

/* -2    -> ZH_P_PUSH_BLOCKSHORT
 * -1    -> size of codeblock
 *  0    -> start of table with referenced local variables
 *
 * NOTE: pCode points to static memory
 */
static void zh_vmPushBlockShort( const ZH_BYTE * pCode, PZH_SYMBOL pSymbols, ZH_SIZE nLen )
{
   
   PZH_ITEM pItem = zh_stackAllocItem();

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPushBlockShort(%p,%p,%" ZH_PFS "u)", ( const void * ) pCode, ( void * ) pSymbols, nLen ) );

   pItem->item.asBlock.value =
      zh_codeblockNew( pCode,                       /* pcode buffer         */
                       0,                           /* number of referenced local variables */
                       NULL,                        /* table with referenced local variables */
                       pSymbols,
                       nLen );

   pItem->type = ZH_IT_BLOCK;

   /* store the number of expected parameters
    */
   pItem->item.asBlock.paramcnt = 0;
   /* store the line number where the codeblock was defined
    */
   pItem->item.asBlock.lineno = zh_stackBaseItem()->item.asSymbol.stackstate->uiLineNo;
   pItem->item.asBlock.hclass = zh_stackBaseItem()->item.asSymbol.stackstate->uiClass;
   pItem->item.asBlock.method = zh_stackBaseItem()->item.asSymbol.stackstate->uiMethod;
}

/* -(5|6)     -> ZH_P_MPUSH_BLOCK[LARGE]
 * [-5] -4 -3 -> size of codeblock
 * -2 -1      -> number of expected parameters
 * +0         -> start of pcode
 *
 * NOTE: pCode points to dynamically allocated memory
 */
static void zh_vmPushMacroBlock( const ZH_BYTE * pCode, ZH_SIZE nSize, ZH_USHORT usParams )
{
   
   PZH_ITEM pItem = zh_stackAllocItem();

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPushMacroBlock(%p,%" ZH_PFS "u,%hu)", ( const void * ) pCode, nSize, usParams ) );

   pItem->item.asBlock.value = zh_codeblockMacroNew( pCode, nSize );
   pItem->type = ZH_IT_BLOCK;

   /* store the number of expected parameters
    */
   pItem->item.asBlock.paramcnt = usParams;
   /* store the line number where the codeblock was defined
    */
   pItem->item.asBlock.lineno = zh_stackBaseItem()->item.asSymbol.stackstate->uiLineNo;
   pItem->item.asBlock.hclass = zh_stackBaseItem()->item.asSymbol.stackstate->uiClass;
   pItem->item.asBlock.method = zh_stackBaseItem()->item.asSymbol.stackstate->uiMethod;
}

/* pushes current workarea number on the eval stack
 */
static void zh_vmPushAlias( void )
{
   
   PZH_ITEM pItem = zh_stackAllocItem();

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPushAlias()" ) );

   pItem->type = ZH_IT_INTEGER;
   pItem->item.asInteger.value = zh_rddGetCurrentWorkAreaNumber();
   pItem->item.asInteger.length = 10;
}

/* It pops the last item from the stack to use it to select a workarea
 * and next pushes the value of a given field
 * (for performance reason it replaces alias value with field value)
 */
static void zh_vmPushAliasedField( PZH_SYMBOL pSym )
{
   
   PZH_ITEM pAlias;
   int iCurrArea;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPushAliasedField(%p)", ( void * ) pSym ) );

   iCurrArea = zh_rddGetCurrentWorkAreaNumber();
   pAlias = zh_stackItemFromTop( -1 );

   /*
    * NOTE: zh_vmSelectWorkarea() clears passed item
    */
   if( zh_vmSelectWorkarea( pAlias, pSym ) == ZH_SUCCESS )
      zh_rddGetFieldValue( pAlias, pSym );

   zh_rddSelectWorkAreaNumber( iCurrArea );
}

/* It pops the last item from the stack to use it to select a workarea
 * and next pushes the value of either a field or a memvar based on alias value
 * (for performance reason it replaces alias value with field value)
 * This is used in the following context:
 * ( any_alias )->variable
 */
static void zh_vmPushAliasedVar( PZH_SYMBOL pSym )
{
   
   PZH_ITEM pAlias = zh_stackItemFromTop( -1 );

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPushAliasedVar(%p)", ( void * ) pSym ) );

   if( ZH_IS_STRING( pAlias ) )
   {
      const char * szAlias = pAlias->item.asString.value;

      if( szAlias[ 0 ] == 'M' || szAlias[ 0 ] == 'm' )
      {
         if( pAlias->item.asString.length == 1 || /* M->variable */
             ( pAlias->item.asString.length >= 4 &&
               zh_strnicmp( szAlias, "MEMVAR", /* MEMVAR-> or MEMVA-> or MEMV-> */
                                     pAlias->item.asString.length ) == 0 ) )
         {
            zh_memvarGetValue( pAlias, pSym );
            return;
         }
      }
      else if( pAlias->item.asString.length >= 4 &&
               ( zh_strnicmp( szAlias, "FIELD", /* FIELD-> or FIEL-> */
                                       pAlias->item.asString.length ) == 0 ||
                 zh_strnicmp( szAlias, "_FIELD", /* _FIELD-> or _FIE-> */
                                       pAlias->item.asString.length ) == 0 ) )
      {
         zh_rddGetFieldValue( pAlias, pSym );
         return;
      }
   }
   zh_vmPushAliasedField( pSym );
}

static void zh_vmPushLocal( int iLocal )
{
   
   PZH_ITEM pLocal;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPushLocal(%d)", iLocal ) );

   if( iLocal >= 0 )
   {
      /* local variable or local parameter */
      pLocal = zh_stackLocalVariable( iLocal );
   }
   else
   {
      /* local variable referenced in a codeblock
       * zh_stackSelfItem() points to a codeblock that is currently evaluated
       */
      pLocal = zh_codeblockGetRef( zh_stackSelfItem()->item.asBlock.value, iLocal );
   }

   zh_itemCopy( zh_stackAllocItem(),
                ZH_IS_BYREF( pLocal ) ? zh_itemUnRef( pLocal ) : pLocal );
}

static void zh_vmPushLocalByRef( int iLocal )
{
   
   PZH_ITEM pTop;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPushLocalByRef(%d)", iLocal ) );

   pTop = zh_stackAllocItem();
   /* we store its stack offset instead of a pointer to support a dynamic stack */
   if( iLocal >= 0 )
   {
      PZH_ITEM pLocal = zh_stackLocalVariableAt( &iLocal );
      if( ZH_IS_BYREF( pLocal ) && ! ZH_IS_ENUM( pLocal ) )
      {
         zh_itemCopy( pTop, pLocal );
         return;
      }
      pTop->item.asRefer.BasePtr.itemsbasePtr = zh_stackItemBasePtr();
   }
   else
   {
      /* store direct codeblock address because an item where a codeblock
       * is stored can be no longer placed on the eval stack at the time
       * of a codeblock evaluation or variable access
       */
      pTop->item.asRefer.BasePtr.block = zh_stackSelfItem()->item.asBlock.value;
   }
   pTop->type = ZH_IT_BYREF;
   pTop->item.asRefer.value = iLocal;
   pTop->item.asRefer.offset = zh_stackBaseOffset();
}

static void zh_vmPushStatic( ZH_USHORT uiStatic )
{
   
   PZH_ITEM pStatic;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPushStatic(%hu)", uiStatic ) );

   pStatic = ( ( PZH_ITEM ) zh_stackGetStaticsBase() )->item.asArray.value->pItems + uiStatic - 1;
   zh_itemCopy( zh_stackAllocItem(),
                ZH_IS_BYREF( pStatic ) ? zh_itemUnRef( pStatic ) : pStatic );
}

static void zh_vmPushStaticByRef( ZH_USHORT uiStatic )
{
   
   PZH_ITEM pTop, pBase;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPushStaticByRef(%hu)", uiStatic ) );

   pTop = zh_stackAllocItem();
   pBase = ( PZH_ITEM ) zh_stackGetStaticsBase();

   if( ZH_IS_BYREF( pBase->item.asArray.value->pItems + uiStatic - 1 ) &&
       ! ZH_IS_ENUM( pBase->item.asArray.value->pItems + uiStatic - 1 ) )
   {
      zh_itemCopy( pTop, pBase->item.asArray.value->pItems + uiStatic - 1 );
      return;
   }
   pTop->type = ZH_IT_BYREF;
   /* we store the offset instead of a pointer to support a dynamic stack */
   pTop->item.asRefer.value = uiStatic - 1;
   pTop->item.asRefer.offset = 0;    /* 0 for static variables */
   pTop->item.asRefer.BasePtr.array = pBase->item.asArray.value;
   zh_gcRefInc( pBase->item.asArray.value );
}

static void zh_vmPushVariable( PZH_SYMBOL pVarSymb )
{
   
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_INFO, ( "(zh_vmPushVariable)" ) );

   pItem = zh_stackAllocItem();

   /* First try if passed symbol is a name of field
    * in a current workarea - if it is not a field (ZH_FAILURE)
    * then try the memvar variable
    */
   if( zh_rddFieldGet( pItem, pVarSymb ) != ZH_SUCCESS &&
       zh_memvarGet( pItem, pVarSymb ) != ZH_SUCCESS )
   {
      PZH_ITEM pError;

      pError = zh_errRT_New( ES_ERROR, NULL, EG_NOVAR, 1003,
                             NULL, pVarSymb->szName, 0, EF_CANRETRY );
      zh_itemClear( pItem );

      while( zh_errLaunch( pError ) == E_RETRY )
      {
         if( zh_rddFieldGet( pItem, pVarSymb ) == ZH_SUCCESS ||
             zh_memvarGet( pItem, pVarSymb ) == ZH_SUCCESS )
            break;
      }

      zh_errRelease( pError );
   }
}


static void zh_vmDuplicate( void )
{
   
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmDuplicate()" ) );

   pItem = zh_stackItemFromTop( -1 );
   zh_itemCopy( zh_stackAllocItem(), pItem );
}

static void zh_vmDuplUnRef( void )
{
   
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmDuplUnRef()" ) );

   pItem = zh_stackItemFromTop( -1 );
   zh_itemCopy( zh_stackAllocItem(), pItem );
   if( ZH_IS_BYREF( pItem ) )
      zh_itemCopy( pItem, zh_itemUnRef( pItem ) );
}

static void zh_vmPushUnRef( void )
{
   
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPushUnRef()" ) );

   pItem = zh_stackItemFromTop( -1 );
   zh_itemCopy( zh_stackAllocItem(),
                ZH_IS_BYREF( pItem ) ? zh_itemUnRef( pItem ) : pItem );
}

static void zh_vmSwap( int iCount )
{
   
   int i = -1;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmSwap(%d)", iCount ) );

   do
   {
      zh_itemSwap( zh_stackItemFromTop( i ), zh_stackItemFromTop( i - 1 ) );
      --i;
   }
   while( iCount-- );
}

/* ------------------------------- */
/* Pop                             */
/* ------------------------------- */

static ZH_BOOL zh_vmPopLogical( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPopLogical()" ) );

   if( ZH_IS_LOGICAL( zh_stackItemFromTop( -1 ) ) )
   {
      ZH_BOOL fValue = zh_stackItemFromTop( -1 )->item.asLogical.value;

      zh_stackDec();
      return fValue;
   }
   else
   {
      zh_errRT_BASE( EG_ARG, 1066, NULL, zh_langDGetErrorDesc( EG_CONDITION ), 1, zh_stackItemFromTop( -1 ) );
      return ZH_FALSE;
   }
}

/* Pops the item from the eval stack and uses it to select the current
 * workarea
 */
static void zh_vmPopAlias( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPopAlias()" ) );

   zh_vmSelectWorkarea( zh_stackItemFromTop( -1 ), NULL ); /* it clears the passed item */
   zh_stackDec();
}

/* Pops the alias to use it to select a workarea and next pops a value
 * into a given field
 */
static void zh_vmPopAliasedField( PZH_SYMBOL pSym )
{
   
   int iCurrArea;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPopAliasedField(%p)", ( void * ) pSym ) );

   iCurrArea = zh_rddGetCurrentWorkAreaNumber();
   if( zh_vmSelectWorkarea( zh_stackItemFromTop( -1 ), pSym ) == ZH_SUCCESS )
      zh_rddPutFieldValue( zh_stackItemFromTop( -2 ), pSym );

   zh_rddSelectWorkAreaNumber( iCurrArea );
   zh_stackDec();    /* alias - it was cleared in zh_vmSelectWorkarea() */
   zh_stackPop();    /* field value */
}

/* Pops the alias to use it to select a workarea and next pops a value
 * into either a field or a memvar based on the alias value
 * This is used in the following context:
 * ( any_alias )->variable
 */
static void zh_vmPopAliasedVar( PZH_SYMBOL pSym )
{
   
   PZH_ITEM pAlias = zh_stackItemFromTop( -1 );

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPopAliasedVar(%p)", ( void * ) pSym ) );

   /*
    * "M", "MEMV" - "MEMVAR" and "FIEL" - "FIELD" are reserved aliases
    */
   if( ZH_IS_STRING( pAlias ) )
   {
      const char * szAlias = pAlias->item.asString.value;

      if( szAlias[ 0 ] == 'M' || szAlias[ 0 ] == 'm' )
      {
         if( pAlias->item.asString.length == 1 || /* M->variable */
             ( pAlias->item.asString.length >= 4 &&
               zh_strnicmp( szAlias, "MEMVAR", /* MEMVAR-> or MEMVA-> or MEMV-> */
                                     pAlias->item.asString.length ) == 0 ) )
         {
            zh_memvarSetValue( pSym, zh_stackItemFromTop( -2 ) );
            zh_stackPop();    /* alias */
            zh_stackPop();    /* value */
            return;
         }
      }
      else if( pAlias->item.asString.length >= 4 &&
               ( zh_strnicmp( szAlias, "FIELD", /* FIELD-> or FIEL-> */
                                     pAlias->item.asString.length ) == 0 ||
                 zh_strnicmp( szAlias, "_FIELD", /* _FIELD-> or _FIE-> */
                                       pAlias->item.asString.length ) == 0 ) )
      {
         zh_rddPutFieldValue( zh_stackItemFromTop( -2 ), pSym );
         zh_stackPop();    /* alias */
         zh_stackPop();    /* value */
         return;
      }
   }
   zh_vmPopAliasedField( pSym );
}

static void zh_vmPopLocal( int iLocal )
{
   
   PZH_ITEM pLocal, pVal;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPopLocal(%d)", iLocal ) );

   pVal = zh_stackItemFromTop( -1 );

   /* Remove MEMOFLAG if exists (assignment from field). */
   pVal->type &= ~( ZH_IT_MEMOFLAG | ZH_IT_DEFAULT );

   if( iLocal >= 0 )
   {
      /* local variable or local parameter */
      pLocal = zh_stackLocalVariable( iLocal );
   }
   else
   {
      /* local variable referenced in a codeblock
       * zh_stackSelfItem() points to a codeblock that is currently evaluated
       */
      pLocal = zh_codeblockGetRef( zh_stackSelfItem()->item.asBlock.value, iLocal );
   }

   zh_itemMoveToRef( pLocal, pVal );

   zh_stackDec();
}

static void zh_vmPopStatic( ZH_USHORT uiStatic )
{
   
   PZH_ITEM pStatic, pVal;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPopStatic(%hu)", uiStatic ) );

   pVal = zh_stackItemFromTop( -1 );

   /* Remove MEMOFLAG if exists (assignment from field). */
   pVal->type &= ~( ZH_IT_MEMOFLAG | ZH_IT_DEFAULT );
   pStatic = ( ( PZH_ITEM ) zh_stackGetStaticsBase() )->item.asArray.value->pItems + uiStatic - 1;

   zh_itemMoveToRef( pStatic, pVal );
   zh_stackDec();
}

/* ------------------------------- */
/*
 * Functions to manage module symbols
 */

PZH_SYMBOL zh_vmGetRealFuncSym( PZH_SYMBOL pSym )
{
   if( pSym && ! ( pSym->scope.value & ZH_FS_LOCAL ) )
   {
      pSym = pSym->pDynSym &&
           ( pSym->pDynSym->pSymbol->scope.value & ZH_FS_LOCAL ) ?
             pSym->pDynSym->pSymbol : NULL;
   }

   return pSym;
}

ZH_BOOL zh_vmLockModuleSymbols( void )
{
   return ! s_pSymbolsMtx || zh_threadMutexLock( s_pSymbolsMtx );
}

void zh_vmUnlockModuleSymbols( void )
{
   if( s_pSymbolsMtx )
      zh_threadMutexUnlock( s_pSymbolsMtx );
}

const char * zh_vmFindModuleSymbolName( PZH_SYMBOL pSym )
{
   if( pSym )
   {
      PZH_SYMBOLS pLastSymbols = s_pSymbols;

      while( pLastSymbols )
      {
         if( pSym >= pLastSymbols->pModuleSymbols &&
             pSym < pLastSymbols->pModuleSymbols + pLastSymbols->uiModuleSymbols )
         {
            return pLastSymbols->szModuleName;
         }
         pLastSymbols = pLastSymbols->pNext;
      }
   }
   return NULL;
}

ZH_BOOL zh_vmFindModuleSymbols( PZH_SYMBOL pSym, PZH_SYMBOL * pSymbols,
                                ZH_USHORT * puiSymbols )
{
   if( pSym )
   {
      PZH_SYMBOLS pLastSymbols = s_pSymbols;


      while( pLastSymbols )
      {
         if( pLastSymbols->fActive &&
             pSym >= pLastSymbols->pModuleSymbols &&
             pSym < pLastSymbols->pModuleSymbols + pLastSymbols->uiModuleSymbols )
         {
            *pSymbols   = pLastSymbols->pModuleSymbols;
            *puiSymbols = pLastSymbols->uiModuleSymbols;
            return ZH_TRUE;
         }
         pLastSymbols = pLastSymbols->pNext;
      }
   }

   *pSymbols   = NULL;
   *puiSymbols = 0;
   return ZH_FALSE;
}

PZH_SYMBOL zh_vmFindFuncSym( const char * szFuncName, void * hDynLib )
{
   static PZH_SYMBOL pFuncSym = NULL;

   if( szFuncName )
   {
      PZH_SYMBOLS pSymbols = s_pSymbols;

      while( pSymbols )
      {
         if( pSymbols->fActive && pSymbols->hDynLib == hDynLib )
         {
            ZH_USHORT ui;

            for( ui = 0; ui < pSymbols->uiModuleSymbols; ++ui )
            {
               PZH_SYMBOL pSymbol = &pSymbols->pModuleSymbols[ ui ];

               if( ( pSymbol->scope.value & ZH_FS_LOCAL ) != 0 &&
                   zh_stricmp( pSymbol->szName, szFuncName ) == 0 )
               {
                  if( ( pSymbol->scope.value & ZH_FS_STATIC ) == 0 )
                     return pSymbol;
                  else if( ! pFuncSym )
                     pFuncSym = pSymbol;
               }
            }
         }
         pSymbols = pSymbols->pNext;
      }
   }

   return pFuncSym;
}

#define ZH_SYM_STATICSBASE( p )  \
   ( ( PZH_ITEM ) ( ( ( p )->scope.value & ZH_FS_FRAME ) ? \
                    ( p )->value.pStaticsBase : NULL ) )

static void zh_vmStaticsClear( void )
{
   PZH_SYMBOLS pLastSymbols = s_pSymbols;

   while( pLastSymbols )
   {
      if( pLastSymbols->uiStaticsOffset )
      {
         PZH_SYMBOL pSym = pLastSymbols->pModuleSymbols + pLastSymbols->uiStaticsOffset;
         PZH_ITEM pStatics = ZH_SYM_STATICSBASE( pSym );
         if( pStatics )
         {
            ZH_SIZE nLen = zh_arrayLen( pStatics ), ul;
            for( ul = 1; ul <= nLen; ++ul )
            {
               PZH_ITEM pItem = zh_arrayGetItemPtr( pStatics, ul );
               
               if( pItem && ZH_IS_COMPLEX( pItem ) ){
                  //printf("static complex %d\n", ul);
                  zh_itemClear( pItem );
               } else {
                  //printf("static simple %d\n", ul);
                  zh_itemSetNil( pItem );
               }

               //if ( pItem && ZH_IS_POINTER( pItem) ) {
               //    pItem = zh_itemPutNil(pItem);
               //}

            }
         }
      }
      pLastSymbols = pLastSymbols->pNext;
   }
}

static void zh_vmStaticsRelease( void )
{
   PZH_SYMBOLS pLastSymbols = s_pSymbols;

   while( pLastSymbols )
   {
      if( pLastSymbols->uiStaticsOffset )
      {
         PZH_SYMBOL pSym = pLastSymbols->pModuleSymbols + pLastSymbols->uiStaticsOffset;
         PZH_ITEM pStatics = ZH_SYM_STATICSBASE( pSym );
         if( pStatics )
         {
            zh_itemRelease( pStatics );
            pSym->value.pStaticsBase = NULL;

         }
      }
      pLastSymbols = pLastSymbols->pNext;
   }
}

static ZH_SIZE zh_vmStaticsCount( void )
{
   ZH_SIZE nStatics = 0;

   if( zh_vmLockModuleSymbols() )
   {
      PZH_SYMBOLS pLastSymbols = s_pSymbols;
      while( pLastSymbols )
      {
         if( pLastSymbols->uiStaticsOffset )
         {
            PZH_SYMBOL pSym = pLastSymbols->pModuleSymbols + pLastSymbols->uiStaticsOffset;
            PZH_ITEM pStatics = ZH_SYM_STATICSBASE( pSym );
            if( pStatics )
               nStatics += zh_arrayLen( pStatics );
         }
         pLastSymbols = pLastSymbols->pNext;
      }
      zh_vmUnlockModuleSymbols();
   }

   return nStatics;
}

static PZH_ITEM zh_vmStaticsArray( void )
{
   PZH_ITEM pArray = NULL;

   if( zh_vmLockModuleSymbols() )
   {
      PZH_SYMBOLS pLastSymbols = s_pSymbols;
      ZH_SIZE nOffset, nCount;

      nCount = zh_vmStaticsCount();
      pArray = zh_itemArrayNew( nCount );
      nOffset = 0;

      while( pLastSymbols )
      {
         if( pLastSymbols->uiStaticsOffset )
         {
            PZH_SYMBOL pSym = pLastSymbols->pModuleSymbols + pLastSymbols->uiStaticsOffset;
            PZH_ITEM pStatics = ZH_SYM_STATICSBASE( pSym );
            if( pStatics )
            {
               ZH_SIZE nLen = zh_arrayLen( pStatics ), n;

               for( n = 1; n <= nLen; ++n )
                  zh_arraySet( pArray, ++nOffset, zh_arrayGetItemPtr( pStatics, n ) );
            }
         }
         pLastSymbols = pLastSymbols->pNext;
      }
      zh_vmUnlockModuleSymbols();
   }

   return pArray;
}

static PZH_SYMBOLS zh_vmFindFreeModule( PZH_SYMBOL pSymbols, ZH_USHORT uiSymbols,
                                        const char * szModuleName, ZH_ULONG ulID )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmFindFreeModule(%p,%hu,%s,%lu)", ( void * ) pSymbols, uiSymbols, szModuleName, ulID ) );

   if( s_ulFreeSymbols )
   {
      PZH_SYMBOLS pLastSymbols = s_pSymbols;

      while( pLastSymbols )
      {
         if( ! pLastSymbols->fActive &&
             pLastSymbols->ulID == ulID &&
             pLastSymbols->uiModuleSymbols == uiSymbols &&
             pLastSymbols->szModuleName != NULL &&
             strcmp( pLastSymbols->szModuleName, szModuleName ) == 0 )
         {
            PZH_SYMBOL pModuleSymbols = pLastSymbols->pModuleSymbols;
            ZH_USHORT ui;

            for( ui = 0; ui < uiSymbols; ++ui )
            {
               if( ( ( pSymbols[ ui ].scope.value & ~( ZH_FS_PCODEFUNC | ZH_FS_DYNCODE | ZH_FS_DEFERRED ) ) !=
                     ( pModuleSymbols[ ui ].scope.value & ~ZH_FS_DEFERRED ) &&
                     ! ( ui != 0 && ui == pLastSymbols->uiStaticsOffset &&
                         ZH_SYM_STATICSBASE( &pModuleSymbols[ ui ] ) ) ) ||
                   strcmp( pSymbols[ ui ].szName, pModuleSymbols[ ui ].szName ) != 0 )
               {
                  break;
               }
            }
            if( ui == uiSymbols )
            {
               --s_ulFreeSymbols;
               return pLastSymbols;
            }
         }
         pLastSymbols = pLastSymbols->pNext;
      }
   }

   return NULL;
}

void zh_vmFreeSymbols( PZH_SYMBOLS pSymbols )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmFreeSymbols(%p)", ( void * ) pSymbols ) );

   if( pSymbols->fActive && zh_vmLockModuleSymbols() )
   {
      if( pSymbols->fActive )
      {
         ZH_USHORT ui;

         for( ui = 0; ui < pSymbols->uiModuleSymbols; ++ui )
         {
            PZH_SYMBOL pSymbol = &pSymbols->pModuleSymbols[ ui ];

            /* do not overwrite already initialized statics' frame */
            if( ui == 0 || ui != pSymbols->uiStaticsOffset ||
                ! ZH_SYM_STATICSBASE( pSymbol ) )
            {
               pSymbol->value.pFunPtr = NULL;
               if( pSymbol->pDynSym && pSymbol->pDynSym->pSymbol != pSymbol &&
                   ( pSymbol->scope.value & ZH_FS_LOCAL ) == 0 )
                  pSymbol->scope.value |= ZH_FS_DEFERRED;
               pSymbol->scope.value &= ~( ZH_FS_PCODEFUNC | ZH_FS_DYNCODE );
            }
         }
         pSymbols->hDynLib = NULL;
         pSymbols->fActive = ZH_FALSE;
         ++s_ulFreeSymbols;
      }
      zh_vmUnlockModuleSymbols();
   }
}

void zh_vmBeginSymbolGroup( void * hDynLib, ZH_BOOL fClone )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmBeginSymbolGroup(%p,%d)", hDynLib, ( int ) fClone ) );

   s_hDynLibID = hDynLib;
   s_fCloneSym = fClone;
}

void zh_vmInitSymbolGroup( void * hNewDynLib, int argc, const char * argv[] )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmInitSymbolGroup(%p,%d,%p)", hNewDynLib, argc, ( const void * ) argv ) );

   s_fCloneSym = ZH_FALSE;

   if( s_hDynLibID )
   {
      PZH_SYMBOLS pLastSymbols = s_pSymbols;
      void * hDynLib = s_hDynLibID;
      ZH_BOOL fFound = ZH_FALSE;
      ZH_USHORT ui;

      s_hDynLibID = NULL;

      while( pLastSymbols )
      {
         if( pLastSymbols->hDynLib == hDynLib )
         {
            fFound = ZH_TRUE;

            if( pLastSymbols->fInitStatics && pLastSymbols->fActive )
            {
               for( ui = 0; ui < pLastSymbols->uiModuleSymbols; ui++ )
               {
                  ZH_SYMBOLSCOPE scope = ( pLastSymbols->pModuleSymbols + ui )->scope.value & ZH_FS_INITEXIT;

                  if( scope == ZH_FS_INITEXIT &&
                      ! ( ui != 0 && ui == pLastSymbols->uiStaticsOffset &&
                          ZH_SYM_STATICSBASE( pLastSymbols->pModuleSymbols + ui ) ) )
                  {
                     zh_vmPushSymbol( pLastSymbols->pModuleSymbols + ui );
                     zh_vmPushNil();
                     zh_vmProc( 0 );
                  }
               }
               pLastSymbols->fInitStatics = ZH_FALSE;
            }

            pLastSymbols->hDynLib = hNewDynLib;
         }
         pLastSymbols = pLastSymbols->pNext;
      }

      /* library symbols are modified beforeinit functions
         execution intentionally because init functions may
         load new modules [druzus] */
      zh_vmDoModuleSetLibID( s_InitFunctions, hDynLib, hNewDynLib );
      zh_vmDoModuleSetLibID( s_ExitFunctions, hDynLib, hNewDynLib );
      zh_vmDoModuleSetLibID( s_QuitFunctions, hDynLib, hNewDynLib );
      zh_vmDoModuleLibFunctions( &s_InitFunctions, hNewDynLib );

      if( fFound )
      {
         ZH_BOOL fClipInit = ZH_TRUE;

         do
         {
            pLastSymbols = s_pSymbols;
            while( pLastSymbols && zh_vmRequestQuery() == 0 )
            {
               if( pLastSymbols->hDynLib == hNewDynLib )
               {
                  if( pLastSymbols->fActive && ( pLastSymbols->hScope & ZH_FS_INIT ) != 0 )
                  {
                     ui = pLastSymbols->uiModuleSymbols;
                     while( ui-- )
                     {
                        ZH_SYMBOLSCOPE scope = ( pLastSymbols->pModuleSymbols + ui )->scope.value & ZH_FS_INITEXIT;

                        if( scope == ZH_FS_INIT &&
                            ( strcmp( ( pLastSymbols->pModuleSymbols + ui )->szName,
                                      "CLIPINIT$" ) == 0 ? fClipInit : ! fClipInit ) )
                        {
                           int i;
                           zh_vmPushSymbol( pLastSymbols->pModuleSymbols + ui );
                           zh_vmPushNil();
                           for( i = 0; i < argc; ++i )
                           {
                              zh_vmPushString( argv[ i ], strlen( argv[ i ] ) );
                           }
                           zh_vmProc( ( ZH_USHORT ) argc );
                           if( zh_vmRequestQuery() != 0 )
                              break;
                        }
                     }
                  }
               }
               pLastSymbols = pLastSymbols->pNext;
            }
            fClipInit = ! fClipInit;
         }
         while( ! fClipInit );
      }
   }
}

void zh_vmExitSymbolGroup( void * hDynLib )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmExitSymbolGroup(%p)", hDynLib ) );

   if( hDynLib )
   {
      PZH_SYMBOLS pLastSymbols = s_pSymbols;
      ZH_BOOL fFound = ZH_FALSE;

      while( pLastSymbols )
      {
         if( pLastSymbols->hDynLib == hDynLib )
         {
            fFound = ZH_TRUE;
            if( pLastSymbols->fActive && ( pLastSymbols->hScope & ZH_FS_EXIT ) != 0 )
            {
               ZH_USHORT ui;
               for( ui = 0; ui < pLastSymbols->uiModuleSymbols; ui++ )
               {
                  ZH_SYMBOLSCOPE scope = ( pLastSymbols->pModuleSymbols + ui )->scope.value & ZH_FS_INITEXIT;

                  if( scope == ZH_FS_EXIT )
                  {
                     zh_vmPushSymbol( pLastSymbols->pModuleSymbols + ui );
                     zh_vmPushNil();
                     zh_vmProc( 0 );
                  }
               }
            }
         }
         pLastSymbols = pLastSymbols->pNext;
      }

      zh_vmDoModuleLibFunctions( &s_ExitFunctions, hDynLib );
      zh_vmDoModuleLibFunctions( &s_QuitFunctions, hDynLib );

      if( fFound )
      {
         pLastSymbols = s_pSymbols;
         while( pLastSymbols )
         {
            if( pLastSymbols->hDynLib == hDynLib )
               zh_vmFreeSymbols( pLastSymbols );
            pLastSymbols = pLastSymbols->pNext;
         }
      }
   }
}

PZH_SYMBOLS zh_vmRegisterSymbols( PZH_SYMBOL pModuleSymbols, ZH_USHORT uiSymbols,
                                  const char * szModuleName, ZH_ULONG ulID,
                                  ZH_BOOL fDynLib, ZH_BOOL fClone,
                                  ZH_BOOL fOverLoad )
{
   PZH_SYMBOLS pNewSymbols;
   ZH_BOOL fRecycled, fInitStatics = ZH_FALSE;
   ZH_USHORT ui;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmRegisterSymbols(%p,%hu,%s,%lu,%d,%d,%d)", ( void * ) pModuleSymbols, uiSymbols, szModuleName, ulID, ( int ) fDynLib, ( int ) fClone, ( int ) fOverLoad ) );

   //puts("hernad registerSymbols"); puts(szModuleName);

   pNewSymbols = s_ulFreeSymbols == 0 ? NULL :
                 zh_vmFindFreeModule( pModuleSymbols, uiSymbols, szModuleName, ulID );

   if( pNewSymbols )
   {
      pNewSymbols->fActive = fRecycled = ZH_TRUE;
      pNewSymbols->hDynLib = s_hDynLibID;
      pNewSymbols->hScope = 0;
   }
   else
   {
      fRecycled = ZH_FALSE;

      if( fClone )
      {
         ZH_SIZE nSymSize = uiSymbols * sizeof( ZH_SYMBOL );
         ZH_SIZE nSize;
         char * buffer;

         nSize = nSymSize;
         for( ui = 0; ui < uiSymbols; ui++ )
            nSize += strlen( pModuleSymbols[ ui ].szName ) + 1;
         buffer = ( char * ) memcpy( zh_xgrab( nSize ), pModuleSymbols, nSymSize );
         pModuleSymbols = ( PZH_SYMBOL ) buffer;
         for( ui = 0; ui < uiSymbols; ui++ )
         {
            buffer += nSymSize;
            nSymSize = strlen( pModuleSymbols[ ui ].szName ) + 1;
            memcpy( buffer, pModuleSymbols[ ui ].szName, nSymSize );
            pModuleSymbols[ ui ].szName = buffer;
         }
      }

      pNewSymbols = ( PZH_SYMBOLS ) zh_xgrab( sizeof( ZH_SYMBOLS ) );
      pNewSymbols->pModuleSymbols = pModuleSymbols;
      pNewSymbols->uiModuleSymbols = uiSymbols;
      pNewSymbols->uiStaticsOffset = 0;
      pNewSymbols->szModuleName = zh_strdup( szModuleName );
      pNewSymbols->ulID = ulID;
      pNewSymbols->fAllocated = fClone;
      pNewSymbols->fActive = ZH_TRUE;
      pNewSymbols->fInitStatics = ZH_FALSE;
      pNewSymbols->hDynLib = s_hDynLibID;
      pNewSymbols->hScope = 0;
      pNewSymbols->pNext = NULL;

      if( s_pSymbols == NULL )
      {
         s_pSymbols = pNewSymbols;
      }
      else
      {
         PZH_SYMBOLS pLastSymbols = s_pSymbols;

         while( pLastSymbols->pNext ) /* locates the latest processed group of symbols */
            pLastSymbols = pLastSymbols->pNext;
         pLastSymbols->pNext = pNewSymbols;
      }
   }

   // printf("hernad uiSymbolds %d\n", uiSymbols);

   for( ui = 0; ui < uiSymbols; ui++ ) /* register each public symbol on the dynamic symbol table */
   {
      PZH_SYMBOL pSymbol = pNewSymbols->pModuleSymbols + ui;
      ZH_SYMBOLSCOPE hSymScope;
      ZH_BOOL fPublic, fStatics;

      fStatics = ( pSymbol->scope.value & ZH_FS_INITEXIT ) == ZH_FS_INITEXIT ||
                 ( fRecycled && ui != 0 && ui == pNewSymbols->uiStaticsOffset &&
                   ZH_SYM_STATICSBASE( pSymbol ) );

      if( fRecycled && ! fStatics )
      {
         pSymbol->value.pFunPtr = ( pModuleSymbols + ui )->value.pFunPtr;
         pSymbol->scope.value = ( pModuleSymbols + ui )->scope.value;
      }
      if( fDynLib )
      {
         pSymbol->scope.value |= ZH_FS_DYNCODE;
      }

      hSymScope = pSymbol->scope.value;
      pNewSymbols->hScope |= hSymScope;

      fPublic = ( hSymScope & ( ZH_FS_INITEXIT | ZH_FS_STATIC | ZH_FS_FRAME ) ) == 0;
      if( fStatics )
      {
         if( ! fRecycled && strncmp( pSymbol->szName, "(_INITSTATICS", 13 ) == 0 )
            pNewSymbols->uiStaticsOffset = ui;
         fInitStatics = ZH_TRUE;
      }

      if( ( hSymScope & ( ZH_FS_PCODEFUNC | ZH_FS_LOCAL | ZH_FS_FRAME ) ) ==
          ( ZH_FS_PCODEFUNC | ZH_FS_LOCAL ) && ( fRecycled || fClone ) )
      {
         pSymbol->value.pCodeFunc->pSymbols = pNewSymbols->pModuleSymbols;
      }

      if( ! s_pSymStart && ! fDynLib && ! fStatics &&
          ( hSymScope & ZH_FS_FIRST ) != 0 &&
          ( hSymScope & ZH_FS_INITEXIT ) == 0 )
      {
         /* first public defined symbol to start execution */
         s_pSymStart = pSymbol;
      }

      /* Enable this code to see static functions which are registered in global dynsym table */
#if 0
      if( fPublic && ( hSymScope & ( ZH_FS_INITEXIT | ZH_FS_STATIC ) ) != 0 )
      {
         ZH_TRACE( ZH_TR_DEBUG, ( "Registering: %s:%s scope %04x", szModuleName, pSymbol->szName, hSymScope ) );
      }
#endif

      if( fPublic )
      {
         if( fDynLib && ZH_VM_ISFUNC( pSymbol ) )
         {
            PZH_DYNSYMBOL pDynSym;

            pDynSym = zh_dynsymFind( pSymbol->szName );

            if( pDynSym )
            {
               if( fOverLoad && ( pSymbol->scope.value & ZH_FS_LOCAL ) != 0 )
               {
                  /* overload existing public function */
                  pDynSym->pSymbol = pSymbol;
                  zh_vmSetDynFunc( pDynSym );
                  continue;
               }
               pSymbol->pDynSym = pDynSym;
               if( pDynSym->pSymbol != pSymbol && ZH_VM_ISFUNC( pDynSym->pSymbol ) &&
                   ( pDynSym->pSymbol->value.pFunPtr != pSymbol->value.pFunPtr ||
                     ( pDynSym->pSymbol->scope.value & ZH_FS_LOCAL ) != 0 ||
                     ( ( pSymbol->scope.value & ( ZH_FS_LOCAL | ZH_FS_DYNCODE ) ) !=
                       ( ZH_FS_LOCAL | ZH_FS_DYNCODE ) ) ) )
               {
                  pSymbol->scope.value =
                     ( pSymbol->scope.value & ~( ZH_FS_PCODEFUNC | ZH_FS_LOCAL ) ) |
                     ( pDynSym->pSymbol->scope.value & ZH_FS_PCODEFUNC );
                  pSymbol->value.pFunPtr = pDynSym->pSymbol->value.pFunPtr;
               }
               else
               {
                  pDynSym->pSymbol = pSymbol;
               }
               continue;
            }
         }

         zh_dynsymNew( pSymbol );
      }
   }

   if( ! fRecycled )
   {
      pNewSymbols->fInitStatics = fInitStatics;
   }

   return pNewSymbols;
}

static void zh_vmVerifySymbols( PZH_ITEM pArray )
{
   PZH_SYMBOLS pLastSymbols = s_pSymbols;
   PZH_ITEM pItem = NULL;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmVerifySymbols(%p)", ( void * ) pArray ) );

   zh_arrayNew( pArray, 0 );

   while( pLastSymbols )
   {
      ZH_USHORT ui, uiSymbols = pLastSymbols->uiModuleSymbols;

      for( ui = 0; ui < uiSymbols; ++ui )
      {
         PZH_SYMBOL pSym = pLastSymbols->pModuleSymbols + ui;

         if( pSym->pDynSym &&
             zh_dynsymFind( pSym->szName ) != pSym->pDynSym )
         {
            char szText[ 256 ];

            zh_snprintf( szText, sizeof( szText ), "%s->%s",
                         pLastSymbols->szModuleName, pSym->szName );
            pItem = zh_itemPutC( pItem, szText );
            zh_arrayAddForward( pArray, pItem );
         }
      }
      pLastSymbols = pLastSymbols->pNext;
   }
   if( pItem )
      zh_itemRelease( pItem );
}

static void zh_vmVerifyPCodeVersion( const char * szModuleName, ZH_USHORT uiPCodeVer )
{
   if( uiPCodeVer != 0 )
   {
      if( uiPCodeVer > ZH_PCODE_VER ||    /* the module is compiled with newer compiler version then ZHVM */
          uiPCodeVer < ZH_PCODE_VER_MIN ) /* the module is compiled with old not longer supported by ZHVM compiler version */
      {
         char szPCode[ 10 ];
         zh_snprintf( szPCode, sizeof( szPCode ), "%i.%i", uiPCodeVer >> 8, uiPCodeVer & 0xff );

         zh_errInternal( ZH_EI_ERRUNRECOV, "Module '%s'\n"
                         "was compiled with unsupported PCODE version %s.\n"
                         "Please recompile.", szModuleName, szPCode );
      }
   }
}

/*
 * module symbols initialization with extended information
 */
PZH_SYMBOL zh_vmProcessSymbols( PZH_SYMBOL pSymbols, ZH_USHORT uiModuleSymbols,
                              const char * szModuleName, ZH_ULONG ulID,
                              ZH_USHORT uiPCodeVer )
{
   //ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmProcessSymbols(%p,%hu,%s,%lu,%hu)", ( void * ) pSymbols, uiModuleSymbols, szModuleName, ulID, uiPCodeVer ) );

   zh_vmVerifyPCodeVersion( szModuleName, uiPCodeVer );
   return zh_vmRegisterSymbols( pSymbols, uiModuleSymbols, szModuleName, ulID,
                                s_fCloneSym, s_fCloneSym,
                                ZH_FALSE )->pModuleSymbols;
}

PZH_SYMBOL zh_vmProcessDynLibSymbols( PZH_SYMBOL pSymbols, ZH_USHORT uiModuleSymbols,
                                    const char * szModuleName, ZH_ULONG ulID,
                                    ZH_USHORT uiPCodeVer )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmProcessDynLibSymbols(%p,%hu,%s,%lu,%hu)", ( void * ) pSymbols, uiModuleSymbols, szModuleName, ulID, uiPCodeVer ) );

   zh_vmVerifyPCodeVersion( szModuleName, uiPCodeVer );
   return zh_vmRegisterSymbols( pSymbols, uiModuleSymbols, szModuleName, ulID,
                                ZH_TRUE, ZH_TRUE, ZH_FALSE )->pModuleSymbols;
}

static void zh_vmReleaseLocalSymbols( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmReleaseLocalSymbols()" ) );

   while( s_pSymbols )
   {
      PZH_SYMBOLS pDestroy;

      pDestroy = s_pSymbols;
      s_pSymbols = s_pSymbols->pNext;
      if( pDestroy->szModuleName )
         zh_xfree( pDestroy->szModuleName );
      if( pDestroy->fAllocated )
         zh_xfree( pDestroy->pModuleSymbols );
      zh_xfree( pDestroy );
   }
}

/* This calls all _INITSTATICS functions defined in the application.
 * We are using a special symbol's scope ZH_FS_INITEXIT to mark
 * this function. These two bits cannot be marked at the same
 * time for normal user defined functions.
 */
static void zh_vmDoInitStatics( void )
{
   PZH_SYMBOLS pLastSymbols = s_pSymbols;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmDoInitStatics()" ) );

   while( pLastSymbols )
   {
      if( pLastSymbols->fInitStatics )
      {
         ZH_USHORT ui;

         for( ui = 0; ui < pLastSymbols->uiModuleSymbols; ui++ )
         {
            ZH_SYMBOLSCOPE scope = ( pLastSymbols->pModuleSymbols + ui )->scope.value & ZH_FS_INITEXIT;

            if( scope == ZH_FS_INITEXIT )
            {
               zh_vmPushSymbol( pLastSymbols->pModuleSymbols + ui );
               zh_vmPushNil();
               zh_vmProc( 0 );
            }
         }
         pLastSymbols->fInitStatics = ZH_FALSE;
      }
      pLastSymbols = pLastSymbols->pNext;
   }
}

static void zh_vmDoInitFunctions()
{
   PZH_SYMBOLS pLastSymbols = s_pSymbols;

   //ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmDoInitFunctions(%d)", fClipInit ) );
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmDoInitFunctions()") );

   while( pLastSymbols && zh_vmRequestQuery() == 0 )
   {
      /* only if module contains some INIT functions */
      if( pLastSymbols->fActive && ( pLastSymbols->hScope & ZH_FS_INIT ) != 0 )
      {
         ZH_USHORT ui = pLastSymbols->uiModuleSymbols;

         while( ui-- )
         {
            ZH_SYMBOLSCOPE scope = ( pLastSymbols->pModuleSymbols + ui )->scope.value & ZH_FS_INITEXIT;

            if( scope == ZH_FS_INIT )
            //&&
            //    ( strcmp( ( pLastSymbols->pModuleSymbols + ui )->szName,
            //              "CLIPINIT$" ) == 0 ? fClipInit : ! fClipInit ) )
            {
               zh_vmPushSymbol( pLastSymbols->pModuleSymbols + ui );
               zh_vmPushNil();
               zh_vmProc( ( ZH_USHORT ) zh_cmdargPushArgs() );
               if( zh_vmRequestQuery() != 0 )
                  break;
            }
         }
      }
      pLastSymbols = pLastSymbols->pNext;
   }
}

static void zh_vmDoExitFunctions( void )
{
   
   PZH_SYMBOLS pLastSymbols = s_pSymbols;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmDoExitFunctions()" ) );

   /* EXIT procedures should be processed? */
   if( s_fDoExitProc )
   {
      s_fDoExitProc = ZH_FALSE;
      zh_stackSetActionRequest( 0 );

      while( pLastSymbols )
      {
         /* only if module contains some EXIT functions */
         if( pLastSymbols->fActive && pLastSymbols->hScope & ZH_FS_EXIT )
         {
            ZH_USHORT ui;

            for( ui = 0; ui < pLastSymbols->uiModuleSymbols; ui++ )
            {
               ZH_SYMBOLSCOPE scope = ( pLastSymbols->pModuleSymbols + ui )->scope.value & ZH_FS_INITEXIT;

               if( scope == ZH_FS_EXIT )
               {
                  zh_vmPushSymbol( pLastSymbols->pModuleSymbols + ui );
                  zh_vmPushNil();
                  zh_vmProc( 0 );
                  if( zh_stackGetActionRequest() )
                     /* QUIT or BREAK was issued - stop processing
                      */
                     return;
               }
            }
         }
         pLastSymbols = pLastSymbols->pNext;
      }
   }
}

/* ------------------------------- */
/* Extended references             */
/* ------------------------------- */

/*
 * extended item reference functions
 */
static PZH_ITEM zh_vmItemRawRefRead( PZH_ITEM pRefer )
{
   return ( PZH_ITEM ) pRefer->item.asExtRef.value;
}

static PZH_ITEM zh_vmItemRawRefWrite( PZH_ITEM pRefer, PZH_ITEM pSource )
{
   ZH_SYMBOL_UNUSED( pSource );
   return ( PZH_ITEM ) pRefer->item.asExtRef.value;
}

static void zh_vmItemRawRefCopy( PZH_ITEM pDest )
{
   pDest->type = ZH_IT_NIL;
   zh_itemCopy( pDest, ( PZH_ITEM ) pDest->item.asExtRef.value );
}

static void zh_vmItemRawRefDummy( void * value )
{
   ZH_SYMBOL_UNUSED( value );
}

static const ZH_EXTREF s_ItmExtRawRef = {
   zh_vmItemRawRefRead,
   zh_vmItemRawRefWrite,
   zh_vmItemRawRefCopy,
   zh_vmItemRawRefDummy,
   zh_vmItemRawRefDummy
};

typedef struct
{
   ZH_ITEM  memvar;
   PZH_ITEM value;
} ZH_ITMREF, * PZH_ITMREF;

static PZH_ITEM zh_vmItemRefRead( PZH_ITEM pRefer )
{
   return &( ( PZH_ITMREF ) pRefer->item.asExtRef.value )->memvar;
}

static PZH_ITEM zh_vmItemRefWrite( PZH_ITEM pRefer, PZH_ITEM pSource )
{
   return zh_itemUnRefWrite( ( ( PZH_ITMREF ) pRefer->item.asExtRef.value )->value, pSource );
}

static void zh_vmItemRefCopy( PZH_ITEM pDest )
{
   pDest->type = ZH_IT_NIL;
   zh_itemCopy( pDest, &( ( PZH_ITMREF ) pDest->item.asExtRef.value )->memvar );
}

static void zh_vmItemRefClear( void * value )
{
   PZH_ITMREF pItmRef = ( PZH_ITMREF ) value;

#if 1
   if( ! ZH_IS_MEMVAR( &pItmRef->memvar ) ||
       pItmRef->memvar.item.asMemvar.value != pItmRef->value ||
       ! ZH_IS_EXTREF( pItmRef->value ) ||
       pItmRef->value->item.asExtRef.func != &s_ItmExtRawRef )
      zh_errInternal( ZH_EI_ERRUNRECOV, "zh_vmItemRefClear()", NULL, NULL );
#endif

   if( zh_xRefDec( pItmRef->value ) )
      zh_xfree( pItmRef->value );
   else
   {
      pItmRef->memvar.type = ZH_IT_NIL;
      zh_itemCopyFromRef( &pItmRef->memvar, pItmRef->value );
      zh_itemMove( pItmRef->value, &pItmRef->memvar );
   }

   zh_xfree( value );
}

static void zh_vmItemRefMark( void * value )
{
   /* the original value should be accessible from initial item so it's
    * not necessary to mark if form this point.
    */
#if 1
   ZH_SYMBOL_UNUSED( value );
#else
   zh_gcItemRef( ( ( PZH_ITMREF ) value )->memvar );
   zh_gcItemRef( ( ( PZH_ITMREF ) value )->value );
#endif
}

/*
 * push extended item reference
 */
void zh_vmPushItemRef( PZH_ITEM pItem )
{
   static const ZH_EXTREF s_ItmExtRef = {
      zh_vmItemRefRead,
      zh_vmItemRefWrite,
      zh_vmItemRefCopy,
      zh_vmItemRefClear,
      zh_vmItemRefMark
   };

   
   PZH_ITMREF pItmRef;
   PZH_ITEM pRefer;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmPushItemRef(%p)", ( void * ) pItem ) );

   pItmRef = ( PZH_ITMREF ) zh_xgrab( sizeof( ZH_ITMREF ) );

   pItmRef->value = ( PZH_ITEM ) zh_xgrab( sizeof( ZH_ITEM ) );
   pItmRef->value->type = ZH_IT_BYREF | ZH_IT_EXTREF;
   pItmRef->value->item.asExtRef.value = ( void * ) pItem;
   pItmRef->value->item.asExtRef.func = &s_ItmExtRawRef;

   pItmRef->memvar.type = ZH_IT_BYREF | ZH_IT_MEMVAR;
   pItmRef->memvar.item.asMemvar.value = pItmRef->value;

   pRefer = zh_stackAllocItem();
   pRefer->type = ZH_IT_BYREF | ZH_IT_EXTREF;
   pRefer->item.asExtRef.value = ( void * ) pItmRef;
   pRefer->item.asExtRef.func = &s_ItmExtRef;
}

/* ------------------------------- */

/*
 * extended message reference structure
 */
typedef struct
{
   PZH_DYNSYMBOL access;
   PZH_DYNSYMBOL assign;
   ZH_ITEM  object;
   ZH_ITEM  value;
} ZH_MSGREF, * PZH_MSGREF;

/*
 * extended message reference functions
 */
static PZH_ITEM zh_vmMsgRefRead( PZH_ITEM pRefer )
{
   PZH_MSGREF pMsgRef = ( PZH_MSGREF ) pRefer->item.asExtRef.value;

   if( zh_vmRequestQuery() == 0 )
   {
      

      zh_stackPushReturn();
      if( ( pMsgRef->value.type & ZH_IT_DEFAULT ) == 0 )
      {
         zh_vmPushDynSym( pMsgRef->assign );
         zh_vmPush( &pMsgRef->object );
         zh_vmPush( &pMsgRef->value );
         zh_vmSend( 1 );
      }
      else
      {
         if( ! pMsgRef->access )
            pMsgRef->access = zh_dynsymGetCase( pMsgRef->assign->pSymbol->szName + 1 );
         zh_vmPushDynSym( pMsgRef->access );
         zh_vmPush( &pMsgRef->object );
         zh_vmSend( 0 );
      }
      zh_itemMove( &pMsgRef->value, zh_stackReturnItem() );
      pMsgRef->value.type |= ZH_IT_DEFAULT;
      zh_stackPopReturn();
   }
   return &pMsgRef->value;
}

static PZH_ITEM zh_vmMsgRefWrite( PZH_ITEM pRefer, PZH_ITEM pSource )
{
   PZH_MSGREF pMsgRef = ( PZH_MSGREF ) pRefer->item.asExtRef.value;

   if( zh_vmRequestQuery() == 0 )
   {
      

      zh_stackPushReturn();
      zh_vmPushDynSym( pMsgRef->assign );
      zh_vmPush( &pMsgRef->object );
      zh_vmPush( pSource );
      zh_vmSend( 1 );
      zh_itemCopy( &pMsgRef->value, pSource );
      pMsgRef->value.type |= ZH_IT_DEFAULT;
      zh_stackPopReturn();
   }
   return NULL;
   #if 0
   return &pMsgIdxRef->value;
   #endif
}

static void zh_vmMsgRefCopy( PZH_ITEM pDest )
{
   PZH_MSGREF pMsgRef = ( PZH_MSGREF ) pDest->item.asExtRef.value;

   zh_xRefInc( pMsgRef );

   if( ( pMsgRef->value.type & ZH_IT_DEFAULT ) == 0 )
   {
      if( zh_vmRequestReenter() )
      {
         zh_vmPushDynSym( pMsgRef->assign );
         zh_vmPush( &pMsgRef->object );
         zh_vmPush( &pMsgRef->value );
         zh_vmSend( 1 );
         zh_vmRequestRestore();
         pMsgRef->value.type |= ZH_IT_DEFAULT;
      }
   }
}

static void zh_vmMsgRefClear( void * value )
{
   PZH_MSGREF pMsgRef = ( PZH_MSGREF ) value;

   /* value were change by C code without calling RefWrite(),
    *  e.g. zh_stor*() function
    */
   if( ( pMsgRef->value.type & ZH_IT_DEFAULT ) == 0 )
   {
      if( zh_vmRequestReenter() )
      {
         zh_vmPushDynSym( pMsgRef->assign );
         zh_vmPush( &pMsgRef->object );
         zh_vmPush( &pMsgRef->value );
         zh_vmSend( 1 );
         zh_vmRequestRestore();
         pMsgRef->value.type |= ZH_IT_DEFAULT;
      }
   }

   if( zh_xRefDec( value ) )
   {
      if( ZH_IS_COMPLEX( &pMsgRef->value ) )
         zh_itemClear( &pMsgRef->value );
      if( ZH_IS_COMPLEX( &pMsgRef->object ) )
         zh_itemClear( &pMsgRef->object );
      zh_xfree( value );
   }
}

static void zh_vmMsgRefMark( void * value )
{
   if( ZH_IS_GCITEM( &( ( PZH_MSGREF ) value )->object ) )
      zh_gcItemRef( &( ( PZH_MSGREF ) value )->object );
   if( ZH_IS_GCITEM( &( ( PZH_MSGREF ) value )->value ) )
      zh_gcItemRef( &( ( PZH_MSGREF ) value )->value );
}

/*
 * create extended message reference
 */
ZH_BOOL zh_vmMsgReference( PZH_ITEM pObject, PZH_DYNSYMBOL pMessage, PZH_DYNSYMBOL pAccMsg )
{
   static const ZH_EXTREF s_MsgExtRef = {
      zh_vmMsgRefRead,
      zh_vmMsgRefWrite,
      zh_vmMsgRefCopy,
      zh_vmMsgRefClear,
      zh_vmMsgRefMark
   };

   
   PZH_MSGREF pMsgRef;
   PZH_ITEM pRefer;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmMsgReference(%p,%p,%p)", ( void * ) pObject, ( void * ) pMessage, ( void * ) pAccMsg ) );

   pMsgRef = ( PZH_MSGREF ) zh_xgrab( sizeof( ZH_MSGREF ) );
   pMsgRef->access = pAccMsg;
   pMsgRef->assign = pMessage;
   pMsgRef->value.type = ZH_IT_NIL | ZH_IT_DEFAULT;
   pMsgRef->object.type = ZH_IT_NIL;
   zh_itemMove( &pMsgRef->object, pObject );

   pRefer = zh_stackReturnItem();
   if( ZH_IS_COMPLEX( pRefer ) )
      zh_itemClear( pRefer );
   pRefer->type = ZH_IT_BYREF | ZH_IT_EXTREF;
   pRefer->item.asExtRef.value = ( void * ) pMsgRef;
   pRefer->item.asExtRef.func = &s_MsgExtRef;

   return ZH_TRUE;
}

/* ------------------------------- */

/*
 * extended object index reference structure
 */
typedef struct
{
   ZH_ITEM object;
   ZH_ITEM value;
   ZH_ITEM index;
} ZH_MSGIDXREF, * PZH_MSGIDXREF;

/*
 * extended object index reference functions
 */
static PZH_ITEM zh_vmMsgIdxRefRead( PZH_ITEM pRefer )
{
   PZH_MSGIDXREF pMsgIdxRef = ( PZH_MSGIDXREF ) pRefer->item.asExtRef.value;

   if( zh_vmRequestQuery() == 0 )
   {
      
      PZH_ITEM pObject = ZH_IS_BYREF( &pMsgIdxRef->object ) ?
                         zh_itemUnRef( &pMsgIdxRef->object ) :
                         &pMsgIdxRef->object;

      zh_stackPushReturn();
      if( ( pMsgIdxRef->value.type & ZH_IT_DEFAULT ) == 0 )
         zh_objOperatorCall( ZH_OO_OP_ARRAYINDEX, pObject, pObject,
                             &pMsgIdxRef->index, &pMsgIdxRef->value );
      else
         zh_objOperatorCall( ZH_OO_OP_ARRAYINDEX, &pMsgIdxRef->value, pObject,
                             &pMsgIdxRef->index, NULL );
      zh_stackPopReturn();
      pMsgIdxRef->value.type |= ZH_IT_DEFAULT;
   }
   return &pMsgIdxRef->value;
}

static PZH_ITEM zh_vmMsgIdxRefWrite( PZH_ITEM pRefer, PZH_ITEM pSource )
{
   PZH_MSGIDXREF pMsgIdxRef = ( PZH_MSGIDXREF ) pRefer->item.asExtRef.value;

   if( zh_vmRequestQuery() == 0 )
   {
      
      PZH_ITEM pObject = ZH_IS_BYREF( &pMsgIdxRef->object ) ?
                         zh_itemUnRef( &pMsgIdxRef->object ) :
                         &pMsgIdxRef->object;
      zh_stackPushReturn();
      zh_objOperatorCall( ZH_OO_OP_ARRAYINDEX, pObject, pObject,
                          &pMsgIdxRef->index, pSource );
      zh_stackPopReturn();
      pMsgIdxRef->value.type |= ZH_IT_DEFAULT;
   }

   return NULL;
   #if 0
   return &pMsgIdxRef->value;
   #endif
}

static void zh_vmMsgIdxRefCopy( PZH_ITEM pDest )
{
   PZH_MSGIDXREF pMsgIdxRef = ( PZH_MSGIDXREF ) pDest->item.asExtRef.value;

   zh_xRefInc( pMsgIdxRef );

   /* value were change by C code without calling RefWrite(),
    *  e.g. zh_stor*() function
    */
   if( ( pMsgIdxRef->value.type & ZH_IT_DEFAULT ) == 0 )
   {
      if( zh_vmRequestReenter() )
      {
         PZH_ITEM pObject = ZH_IS_BYREF( &pMsgIdxRef->object ) ?
                            zh_itemUnRef( &pMsgIdxRef->object ) :
                            &pMsgIdxRef->object;
         zh_objOperatorCall( ZH_OO_OP_ARRAYINDEX, pObject, pObject,
                             &pMsgIdxRef->index, &pMsgIdxRef->value );
         zh_vmRequestRestore();
      }
   }
}

static void zh_vmMsgIdxRefClear( void * value )
{
   PZH_MSGIDXREF pMsgIdxRef = ( PZH_MSGIDXREF ) value;

   /* value were change by C code without calling RefWrite(),
    *  e.g. zh_stor*() function
    */
   if( ( pMsgIdxRef->value.type & ZH_IT_DEFAULT ) == 0 )
   {
      if( zh_vmRequestReenter() )
      {
         PZH_ITEM pObject = ZH_IS_BYREF( &pMsgIdxRef->object ) ?
                            zh_itemUnRef( &pMsgIdxRef->object ) :
                            &pMsgIdxRef->object;
         zh_objOperatorCall( ZH_OO_OP_ARRAYINDEX, pObject, pObject,
                             &pMsgIdxRef->index, &pMsgIdxRef->value );
         zh_vmRequestRestore();
      }
   }

   if( zh_xRefDec( value ) )
   {
      if( ZH_IS_COMPLEX( &pMsgIdxRef->value ) )
         zh_itemClear( &pMsgIdxRef->value );
      if( ZH_IS_COMPLEX( &pMsgIdxRef->object ) )
         zh_itemClear( &pMsgIdxRef->object );
      if( ZH_IS_COMPLEX( &pMsgIdxRef->index ) )
         zh_itemClear( &pMsgIdxRef->index );
      zh_xfree( value );
   }
}

static void zh_vmMsgIdxRefMark( void * value )
{
   if( ZH_IS_GCITEM( &( ( PZH_MSGIDXREF ) value )->object ) )
      zh_gcItemRef( &( ( PZH_MSGIDXREF ) value )->object );
   if( ZH_IS_GCITEM( &( ( PZH_MSGIDXREF ) value )->index ) )
      zh_gcItemRef( &( ( PZH_MSGIDXREF ) value )->index );
   if( ZH_IS_GCITEM( &( ( PZH_MSGIDXREF ) value )->value ) )
      zh_gcItemRef( &( ( PZH_MSGIDXREF ) value )->value );
}

/*
 * create extended message reference
 */
static void zh_vmMsgIndexReference( PZH_ITEM pRefer, PZH_ITEM pObject, PZH_ITEM pIndex )
{
   static const ZH_EXTREF s_MsgIdxExtRef = {
      zh_vmMsgIdxRefRead,
      zh_vmMsgIdxRefWrite,
      zh_vmMsgIdxRefCopy,
      zh_vmMsgIdxRefClear,
      zh_vmMsgIdxRefMark
   };

   PZH_MSGIDXREF pMsgIdxRef;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmMsgIndexReference(%p,%p,%p)", ( void * ) pRefer, ( void * ) pObject, ( void * ) pIndex ) );

   pMsgIdxRef = ( PZH_MSGIDXREF ) zh_xgrab( sizeof( ZH_MSGIDXREF ) );
   pMsgIdxRef->value.type = ZH_IT_NIL | ZH_IT_DEFAULT;
   pMsgIdxRef->object.type = ZH_IT_NIL;
   pMsgIdxRef->index.type = ZH_IT_NIL;
   zh_itemCopy( &pMsgIdxRef->object, ZH_IS_STRING( pObject ) ? pRefer : pObject );
   zh_itemMove( &pMsgIdxRef->index, pIndex );

   pIndex->type = ZH_IT_BYREF | ZH_IT_EXTREF;
   pIndex->item.asExtRef.value = ( void * ) pMsgIdxRef;
   pIndex->item.asExtRef.func = &s_MsgIdxExtRef;
   zh_itemMove( pRefer, pIndex );
}

/* ------------------------------- */
/* VM exceptions                   */
/* ------------------------------- */

void zh_vmRequestQuit( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmRequestQuit()" ) );

   /* In MT mode EXIT functions are executed only from zh_vmQuit()
    * when all other threads have terminated
    */
   zh_stackSetActionRequest( ZH_QUIT_REQUESTED );
}

void zh_vmRequestEndProc( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmRequestEndProc()" ) );

   zh_stackSetActionRequest( ZH_ENDPROC_REQUESTED );
}

void zh_vmRequestBreak( PZH_ITEM pItem )
{
   
   ZH_I_SIZE nRecoverBase;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmRequestBreak(%p)", ( void * ) pItem ) );

   nRecoverBase = zh_stackGetRecoverBase();
   while( nRecoverBase && ( zh_stackItem( nRecoverBase +
               ZH_RECOVER_STATE )->item.asRecover.flags & ZH_SEQ_DOALWAYS ) )
   {
#if defined( _ZH_RECOVER_DEBUG )
      if( zh_stackItem( nRecoverBase + ZH_RECOVER_STATE )->type != ZH_IT_RECOVER )
         zh_errInternal( ZH_EI_ERRUNRECOV, "zh_vmRequestBreak", NULL, NULL );
#endif
      nRecoverBase = zh_stackItem( nRecoverBase +
                                   ZH_RECOVER_STATE )->item.asRecover.base;
   }

   if( nRecoverBase )
   {
#if defined( _ZH_RECOVER_DEBUG )
      if( zh_stackItem( nRecoverBase + ZH_RECOVER_STATE )->type != ZH_IT_RECOVER )
         zh_errInternal( ZH_EI_ERRUNRECOV, "zh_vmRequestBreak2", NULL, NULL );
#endif
      if( pItem )
         zh_itemCopy( zh_stackItem( nRecoverBase + ZH_RECOVER_VALUE ), pItem );

      zh_stackSetActionRequest( ZH_BREAK_REQUESTED );
   }
   else
   {

      zh_vmRequestQuit();

   }
}

void zh_vmRequestCancel( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmRequestCancel()" ) );

   if( zh_stackSetStruct()->ZH_SET_CANCEL )
   {
      char buffer[ ZH_SYMBOL_NAME_LEN + ZH_SYMBOL_NAME_LEN + 5 + 10 ]; /* additional 10 bytes for line info (%hu) overhead */
      char file[ ZH_PATH_MAX ];
      ZH_USHORT uiLine;
      int iLevel = 0;

      zh_conOutErr( zh_conNewLine(), 0 );
      zh_conOutErr( "Cancelled at: ", 0 );

      while( zh_procinfo( iLevel++, buffer, &uiLine, file ) )
      {
         int l = ( int ) strlen( buffer );
         zh_snprintf( buffer + l, sizeof( buffer ) - l, " (%hu)%s%s", uiLine, *file ? ZH_I_( " in " ) : "", file );

         zh_conOutErr( buffer, 0 );
         zh_conOutErr( zh_conNewLine(), 0 );
      }

      s_fDoExitProc = ZH_FALSE;
      zh_stackSetActionRequest( ZH_QUIT_REQUESTED );
   }
}

ZH_USHORT zh_vmRequestQuery( void )
{
   

   if( zh_vmThreadRequest & ZH_THREQUEST_QUIT )
   {
      if( ! zh_stackQuitState() )
      {
         zh_stackSetQuitState( ZH_TRUE );
         zh_stackSetActionRequest( ZH_QUIT_REQUESTED );
      }
   }

   return zh_stackGetActionRequest();
}

ZH_BOOL zh_vmRequestReenter( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmRequestReenter()" ) );

   if( s_fZHVMActive )
   {
      
      PZH_ITEM pItem;
      int iLocks = 0;

      if( zh_stackId() == NULL )
         return ZH_FALSE;
      else
      {
         while( zh_stackLockCount() > 0 )
         {
            zh_vmLock();
            ++iLocks;
         }
      }

      zh_stackPushReturn();

      pItem = zh_stackAllocItem();
      pItem->type = ZH_IT_RECOVER;
      pItem->item.asRecover.recover = NULL;
      pItem->item.asRecover.base    = iLocks;
      pItem->item.asRecover.flags   = 0;
      pItem->item.asRecover.request = zh_stackGetActionRequest();

      zh_stackSetActionRequest( 0 );

      return ZH_TRUE;
   }
   return ZH_FALSE;
}

void zh_vmRequestRestore( void )
{
   
   ZH_USHORT uiAction;
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmRequestRestore()" ) );

   pItem = zh_stackItemFromTop( -1 );

   if( pItem->type != ZH_IT_RECOVER )
      zh_errInternal( ZH_EI_ERRUNRECOV, "zh_vmRequestRestore", NULL, NULL );

   uiAction = pItem->item.asRecover.request | zh_stackGetActionRequest();

   if( uiAction & ZH_VMSTACK_REQUESTED )
      zh_vmThreadQuit();
   else
   {
      int iCount = ( int ) pItem->item.asRecover.base;
      if( uiAction & ZH_QUIT_REQUESTED )
         zh_stackSetActionRequest( ZH_QUIT_REQUESTED );
      else if( uiAction & ZH_BREAK_REQUESTED )
         zh_stackSetActionRequest( ZH_BREAK_REQUESTED );
      else if( uiAction & ZH_ENDPROC_REQUESTED )
         zh_stackSetActionRequest( ZH_ENDPROC_REQUESTED );
      else
         zh_stackSetActionRequest( 0 );

      zh_stackDec();
      zh_stackPopReturn();

      while( iCount-- > 0 )
         zh_vmUnlock();
   }
}

ZH_BOOL zh_vmRequestReenterExt( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmRequestReenterExt()" ) );

   if( s_fZHVMActive )
   {
      ZH_USHORT uiAction = 0;
      int iLocks = 0;
      PZH_ITEM pItem;

      

      if( zh_stackId() == NULL )
      {
         uiAction = ZH_VMSTACK_REQUESTED;

         /* protection against executing zh_threadStateNew() during GC pass */
         ZH_VM_LOCK();
         for( ;; )
         {
            if( zh_vmThreadRequest & ZH_THREQUEST_STOP )
               zh_threadCondWait( &s_vmCond, &s_vmMtx );
            else
               break;
         }
         s_iRunningCount++;
         ZH_VM_UNLOCK();

         zh_vmThreadInit( NULL );
      

         ZH_VM_LOCK();
         s_iRunningCount--;
         zh_threadCondBroadcast( &s_vmCond );
         ZH_VM_UNLOCK();
      }
      else
      {
         while( zh_stackLockCount() > 0 )
         {
            zh_vmLock();
            ++iLocks;
         }
         zh_stackPushReturn();
      }

      pItem = zh_stackAllocItem();
      pItem->type = ZH_IT_RECOVER;
      pItem->item.asRecover.recover = NULL;
      pItem->item.asRecover.base    = iLocks;
      pItem->item.asRecover.flags   = 0;
      pItem->item.asRecover.request = uiAction | zh_stackGetActionRequest();

      zh_stackSetActionRequest( 0 );

      return ZH_TRUE;
   }

   return ZH_FALSE;
}

ZH_BOOL zh_vmTryEval( PZH_ITEM * pResult, PZH_ITEM pItem, ZH_ULONG ulPCount, ... )
{
   ZH_BOOL fResult;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmTryEval(%p, %p, %lu)", ( void * ) pResult, ( void * ) pItem, ulPCount ) );

   fResult = ZH_FALSE;
   *pResult = NULL;
   if( s_fZHVMActive )
   {
      PZH_SYMBOL pSymbol = NULL;

      if( ZH_IS_STRING( pItem ) )
      {
         PZH_DYNSYMBOL pDynSym = zh_dynsymFindName( pItem->item.asString.value );

         if( pDynSym )
         {
            pSymbol = pDynSym->pSymbol;
            pItem = NULL;
         }
      }
      else if( ZH_IS_SYMBOL( pItem ) )
      {
         pSymbol = pItem->item.asSymbol.value;
         pItem = NULL;
      }
      else if( ZH_IS_BLOCK( pItem ) )
      {
         pSymbol = pZhSymEval;
      }

      if( pSymbol && zh_vmRequestReenter() )
      {
         

         zh_xvmSeqBegin();
         zh_vmPush( zh_breakBlock() );
         zh_vmSeqBlock();

         zh_vmPushSymbol( pSymbol );
         if( pItem )
            zh_vmPush( pItem );
         else
            zh_vmPushNil();

         if( ulPCount )
         {
            ZH_ULONG ulParam;
            va_list va;
            va_start( va, ulPCount );
            for( ulParam = 1; ulParam <= ulPCount; ulParam++ )
               zh_vmPush( va_arg( va, PZH_ITEM ) );
            va_end( va );
         }
         if( pItem )
            zh_vmSend( ( ZH_USHORT ) ulPCount );
         else
            zh_vmProc( ( ZH_USHORT ) ulPCount );

         zh_stackPop();
         if( zh_xvmSeqEndTest() )
         {
            zh_xvmSeqRecover();
            *pResult = zh_itemNew( NULL );
            zh_itemMove( *pResult, zh_stackItemFromTop( -1 ) );
            zh_stackDec();
            zh_stackSetActionRequest( 0 );
         }
         else
         {
            *pResult = zh_itemNew( zh_stackReturnItem() );
            fResult = ZH_TRUE;
         }
         zh_vmRequestRestore();
      }
   }
   return fResult;
}

ZH_BOOL zh_vmIsActive( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmIsActive()" ) );

   return s_fZHVMActive;
}

ZH_BOOL zh_vmIsReady( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmIsReady(%d)", s_fZHVMActive ) );

   if( s_fZHVMActive )
   {
      
      ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmIsReady(%d) == zh_stackId() != NULL", zh_stackId() != NULL ) );
      return zh_stackId() != NULL;
   }
   else
      return ZH_FALSE;
}

ZH_BOOL zh_vmInternalsEnabled( void )
{
   return s_fInternalsEnabled;
}

PZH_CODEPAGE zh_vmCodepage( void )
{
   

   return ( PZH_CODEPAGE ) zh_stackGetCodepage();
}

void zh_vmSetCDP( PZH_CODEPAGE pCDP )
{
   

   zh_stackSetCDP( ( void * ) pCDP );
}

PZH_LANG zh_vmLang( void )
{
   

   return ( PZH_LANG ) zh_stackGetLang();
}

void zh_vmSetLang( PZH_LANG pLang )
{
   

   zh_stackSetLang( ZH_UNCONST( pLang ) );
}

void * zh_vmI18N( void )
{
   

   return zh_stackGetI18N();
}

void zh_vmSetI18N( void * pI18N )
{
   

   zh_i18n_release( zh_stackGetI18N() );
   zh_stackSetI18N( pI18N );
}

#  define ZH_XVM_RETURN \
   { \
      if( zh_vmThreadRequest ) \
         zh_vmRequestTest(); \
      return ( zh_stackGetActionRequest() & \
               ( ZH_ENDPROC_REQUESTED | ZH_BREAK_REQUESTED | ZH_QUIT_REQUESTED ) ) != 0; \
   }

void zh_xvmExitProc( void )
{
   

   if( zh_stackGetActionRequest() & ZH_ENDPROC_REQUESTED )
      zh_stackSetActionRequest( 0 );
}

void zh_xvmEndProc( void )
{
   

   if( ! ( zh_stackGetActionRequest() & ( ZH_QUIT_REQUESTED | ZH_BREAK_REQUESTED ) ) )
      zh_stackSetActionRequest( ZH_ENDPROC_REQUESTED );
}

void zh_xvmSeqBegin( void )
{
   

   PZH_ITEM pItem;

   /*
    * Create the SEQUENCE envelope
    * To keep compatibility with pure PCODE evaluation we have
    * use exactly the same SEQUENCE envelope or zh_vmRequestBreak()
    * will not work as expected.
    *
    * [ break return value ]  -2
    * [ recover envelope   ]  -1
    * [                    ] <- new recover base
    */

   /* 1) clear the storage for value returned by BREAK statement */
   zh_stackAllocItem()->type = ZH_IT_NIL;
   /* 2) recovery state */
   pItem = zh_stackAllocItem();
   /* mark type as NIL - it's not real item */
   pItem->type = ZH_IT_RECOVER;
   /* address of RECOVER or END opcode - not used in C code */
   pItem->item.asRecover.recover = NULL;
   /* store current RECOVER base */
   pItem->item.asRecover.base = zh_stackGetRecoverBase();
   /* store current bCanRecover flag - not used in C code */
   pItem->item.asRecover.flags = 0;
   /* clear new recovery state */
   pItem->item.asRecover.request = 0;

   /* set new recover base */
   zh_stackSetRecoverBase( zh_stackTopOffset() );
}

ZH_BOOL zh_xvmSeqEnd( void )
{
   

   /*
    * remove all items placed on the stack after BEGIN code
    */
   zh_stackRemove( zh_stackGetRecoverBase() );
#if defined( _ZH_RECOVER_DEBUG )
   if( zh_stackItemFromTop( ZH_RECOVER_STATE )->type != ZH_IT_RECOVER )
      zh_errInternal( ZH_EI_ERRUNRECOV, "zh_xvmSeqEnd", NULL, NULL );
#endif
   /*
    * Remove the SEQUENCE envelope
    * This is executed either at the end of sequence or as the
    * response to the break statement if there is no RECOVER clause
    */

   /* 2) Restore previous recovery base address */
   zh_stackSetRecoverBase( zh_stackItemFromTop( ZH_RECOVER_STATE )->item.asRecover.base );
   zh_stackDec();
   /* 1) Discard the value returned by BREAK statement */
   zh_stackPop();

   if( zh_vmThreadRequest )
      zh_vmRequestTest();
   if( zh_stackGetActionRequest() & ( ZH_ENDPROC_REQUESTED | ZH_QUIT_REQUESTED ) )
      return ZH_TRUE;
   else if( zh_stackGetActionRequest() & ZH_BREAK_REQUESTED )
      zh_stackSetActionRequest( 0 );
   return ZH_FALSE;
}

ZH_BOOL zh_xvmSeqEndTest( void )
{
   

   if( zh_vmThreadRequest )
      zh_vmRequestTest();
   if( ( zh_stackGetActionRequest() &
         ( ZH_ENDPROC_REQUESTED | ZH_BREAK_REQUESTED | ZH_QUIT_REQUESTED ) ) != 0 )
      return ZH_TRUE;

   /*
    * remove all items placed on the stack after BEGIN code
    */
   zh_stackRemove( zh_stackGetRecoverBase() );
#if defined( _ZH_RECOVER_DEBUG )
   if( zh_stackItemFromTop( ZH_RECOVER_STATE )->type != ZH_IT_RECOVER )
      zh_errInternal( ZH_EI_ERRUNRECOV, "zh_xvmSeqEndTest", NULL, NULL );
#endif
   /*
    * Remove the SEQUENCE envelope
    * This is executed either at the end of sequence or as the
    * response to the break statement if there is no RECOVER clause
    */

   /* 2) Restore previous recovery base address */
   zh_stackSetRecoverBase( zh_stackItemFromTop( ZH_RECOVER_STATE )->item.asRecover.base );
   zh_stackDec();
   /* 1) Discard the value returned by BREAK statement */
   zh_stackPop();
   return ZH_FALSE;
}

ZH_BOOL zh_xvmSeqRecover( void )
{
   

   /*
    * Execute the RECOVER code
    */

   /*
    * remove all items placed on the stack after BEGIN code
    */
   zh_stackRemove( zh_stackGetRecoverBase() );
#if defined( _ZH_RECOVER_DEBUG )
   if( zh_stackItemFromTop( ZH_RECOVER_STATE )->type != ZH_IT_RECOVER )
      zh_errInternal( ZH_EI_ERRUNRECOV, "zh_xvmSeqRecover", NULL, NULL );
#endif
   /* 2) Restore previous recovery base address */
   zh_stackSetRecoverBase( zh_stackItemFromTop( ZH_RECOVER_STATE )->item.asRecover.base );
   zh_stackDec();
   /* 1) Leave the value returned from BREAK */

   if( zh_vmThreadRequest )
      zh_vmRequestTest();
   if( zh_stackGetActionRequest() & ( ZH_ENDPROC_REQUESTED | ZH_QUIT_REQUESTED ) )
      return ZH_TRUE;
   else if( zh_stackGetActionRequest() & ZH_BREAK_REQUESTED )
      zh_stackSetActionRequest( 0 );
   return ZH_FALSE;
}

void zh_xvmSeqAlways( void )
{
   
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmSeqAlways()" ) );

   /* Create the SEQUENCE ALWAYS envelope */
   /* 1) clear the storage for RETURN value */
   zh_stackAllocItem()->type = ZH_IT_NIL;
   /* 2) recovery state */
   pItem = zh_stackAllocItem();
   /* mark type as NIL - it's not real item */
   pItem->type = ZH_IT_RECOVER;
   /* address of RECOVER or END opcode - not used in C code */
   pItem->item.asRecover.recover = NULL;
   /* store current RECOVER base */
   pItem->item.asRecover.base = zh_stackGetRecoverBase();
   /* store current bCanRecover flag - not used in C code */
   pItem->item.asRecover.flags = 0;
   /* clear new recovery state */
   pItem->item.asRecover.request = 0;
   /* set sequence type */
   pItem->item.asRecover.flags = ZH_SEQ_DOALWAYS;
   /* set new recover base */
   zh_stackSetRecoverBase( zh_stackTopOffset() );
}

ZH_BOOL zh_xvmAlwaysBegin( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmAlwaysBegin()" ) );

   /* remove all items placed on the stack after BEGIN code */
   zh_stackRemove( zh_stackGetRecoverBase() );
#if defined( _ZH_RECOVER_DEBUG )
   if( zh_stackItemFromTop( ZH_RECOVER_STATE )->type != ZH_IT_RECOVER )
      zh_errInternal( ZH_EI_ERRUNRECOV, "zh_xvmAlwaysBegin", NULL, NULL );
#endif
   /* store and reset action */
   zh_stackItemFromTop( ZH_RECOVER_STATE )->item.asRecover.request = zh_stackGetActionRequest();
   zh_stackSetActionRequest( 0 );
   /* store RETURN value */
   if( zh_stackItemFromTop( ZH_RECOVER_STATE )->item.asRecover.request & ZH_ENDPROC_REQUESTED )
      zh_itemMove( zh_stackItemFromTop( ZH_RECOVER_VALUE ), zh_stackReturnItem() );

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmAlwaysEnd( void )
{
   
   ZH_USHORT uiPrevAction, uiCurrAction;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmAlwaysEnd()" ) );

   /* remove all items placed on the stack after ALWAYSBEGIN code */
   zh_stackRemove( zh_stackGetRecoverBase() );

#if defined( _ZH_RECOVER_DEBUG )
   if( zh_stackItemFromTop( ZH_RECOVER_STATE )->type != ZH_IT_RECOVER )
      zh_errInternal( ZH_EI_ERRUNRECOV, "zh_xvmAlwaysEnd", NULL, NULL );
#endif
   /* restore previous recovery base address */
   zh_stackSetRecoverBase( zh_stackItemFromTop( ZH_RECOVER_STATE )->item.asRecover.base );
   uiCurrAction = zh_stackGetActionRequest();
   uiPrevAction = zh_stackItemFromTop( ZH_RECOVER_STATE )->item.asRecover.request;
   /* restore requested action */
   if( ( uiCurrAction | uiPrevAction ) & ZH_QUIT_REQUESTED )
      zh_stackSetActionRequest( ZH_QUIT_REQUESTED );
   else if( ( uiCurrAction | uiPrevAction ) & ZH_BREAK_REQUESTED )
      zh_stackSetActionRequest( ZH_BREAK_REQUESTED );
   else if( ( uiCurrAction | uiPrevAction ) & ZH_ENDPROC_REQUESTED )
      zh_stackSetActionRequest( ZH_ENDPROC_REQUESTED );
   else
      zh_stackSetActionRequest( 0 );
   /* remove the ALWAYS envelope */
   zh_stackDec();
   /* restore RETURN value if not overloaded inside ALWAYS code */
   if( ! ( uiCurrAction & ZH_ENDPROC_REQUESTED ) &&
         ( uiPrevAction & ZH_ENDPROC_REQUESTED ) )
      zh_stackPopReturn();
   else
      zh_stackPop();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmSeqBlock( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmSeqBlock()" ) );

   zh_vmSeqBlock();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmEnumStart( int nVars, int nDescend )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmEnumStart(%d,%d)", nVars, nDescend ) );

   zh_vmEnumStart( nVars, nDescend );

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmEnumNext( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmEnumNext()" ) );

   zh_vmEnumNext();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmEnumPrev( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmEnumPrev()" ) );

   zh_vmEnumPrev();

   ZH_XVM_RETURN
}

void zh_xvmEnumEnd( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmEnumEnd()" ) );

   zh_vmEnumEnd();
}

ZH_BOOL zh_xvmSwitchGet( PZH_ITEM * pSwitchPtr )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmSwitchGet(%p)", ( void * ) pSwitchPtr ) );

   *pSwitchPtr = zh_vmSwitchGet();

   ZH_XVM_RETURN
}

void zh_xvmSetLine( ZH_USHORT uiLine )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmSetLine(%hu)", uiLine ) );

   zh_stackBaseItem()->item.asSymbol.stackstate->uiLineNo = uiLine;
   if( zh_stackBaseItem()->item.asSymbol.stackstate->fDebugging )
      zh_vmDebuggerShowLine( uiLine );

}

void zh_xvmFrame( int iLocals, int iParams )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmFrame(%d, %d)", iLocals, iParams ) );

   zh_vmFrame( ( ZH_USHORT ) iLocals, ( unsigned char ) iParams );
}

void zh_xvmVFrame( int iLocals, int iParams )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmVFrame(%d, %d)", iLocals, iParams ) );

   zh_vmVFrame( ( ZH_USHORT ) iLocals, ( unsigned char ) iParams );
}

void zh_xvmSFrame( PZH_SYMBOL pSymbol )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmSFrame(%p)", ( void * ) pSymbol ) );

   zh_vmSFrame( pSymbol );
}

ZH_BOOL zh_xvmDo( ZH_USHORT uiParams )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmDo(%hu)", uiParams ) );

   zh_vmProc( uiParams );

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmFunction( ZH_USHORT uiParams )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmFunction(%hu)", uiParams ) );

   zh_itemSetNil( zh_stackReturnItem() );
   zh_vmProc( uiParams );
   zh_stackPushReturn();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmSend( ZH_USHORT uiParams )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmSend(%hu)", uiParams ) );

   zh_itemSetNil( zh_stackReturnItem() );
   zh_vmSend( uiParams );
   zh_stackPushReturn();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmPushObjectVarRef( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmPushObjectVarRef()" ) );

   zh_vmPushObjectVarRef();

   ZH_XVM_RETURN
}

void zh_xvmRetValue( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmRetValue()" ) );

   zh_stackPopReturn();
   zh_stackReturnItem()->type &= ~ZH_IT_MEMOFLAG;
}

void zh_xvmRetNil( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmRetNil()" ) );

   zh_itemSetNil( zh_stackReturnItem() );
}

void zh_xvmRetInt( ZH_LONG lValue )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmRetInt(%ld)", lValue ) );

   zh_itemPutNL( zh_stackReturnItem(), lValue );
}

void zh_xvmStatics( PZH_SYMBOL pSymbol, ZH_USHORT uiStatics )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmStatics(%p,%hu)", ( void * ) pSymbol, uiStatics ) );

   zh_vmStatics( pSymbol, uiStatics );
}

void zh_xvmThreadStatics( ZH_USHORT uiStatics, const ZH_BYTE * statics )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmThreadStatics(%hu,%p)", uiStatics, ( const void * ) statics ) );

   zh_vmInitThreadStatics( uiStatics, statics );
}

void zh_xvmParameter( PZH_SYMBOL pSymbol, int iParams )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmParameter(%p,%d)", ( void * ) pSymbol, iParams ) );

   zh_memvarNewParameter( pSymbol, zh_stackItemFromBase( iParams ) );
}

void zh_xvmPushLocal( ZH_SHORT iLocal )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmPushLocal(%hd)", iLocal ) );

   zh_vmPushLocal( iLocal );
}

void zh_xvmPushLocalByRef( ZH_SHORT iLocal )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmPushLocalByRef(%hd)", iLocal ) );

   zh_vmPushLocalByRef( iLocal );
}

void zh_xvmPopLocal( ZH_SHORT iLocal )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmPopLocal(%hd)", iLocal ) );

   zh_vmPopLocal( iLocal );
}

static PZH_ITEM zh_xvmLocalPtr( int iLocal )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmLocalPtr(%d)", iLocal ) );

   if( iLocal >= 0 )
   {
      /* local variable or local parameter */
      return zh_stackLocalVariable( iLocal );
   }
   else
   {
      /* local variable referenced in a codeblock
       * zh_stackSelfItem() points to a codeblock that is currently evaluated
       */
      return zh_codeblockGetRef( zh_stackSelfItem()->item.asBlock.value, iLocal );
   }
}

void zh_xvmCopyLocals( int iDest, int iSource )
{
   PZH_ITEM pDest;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmCopyLocals(%d,%d)", iDest, iSource ) );

   pDest = zh_xvmLocalPtr( iDest );
   zh_itemCopyToRef( zh_xvmLocalPtr( iSource ),
                     ZH_IS_BYREF( pDest ) ? zh_itemUnRef( pDest ) : pDest );
}

void zh_xvmPushStatic( ZH_USHORT uiStatic )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmPushStatic(%hu)", uiStatic ) );

   zh_vmPushStatic( uiStatic );
}

void zh_xvmPushStaticByRef( ZH_USHORT uiStatic )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmPushStaticByRef(%hu)", uiStatic ) );

   zh_vmPushStaticByRef( uiStatic );
}

void zh_xvmPopStatic( ZH_USHORT uiStatic )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmPopStatic(%hu)", uiStatic ) );

   zh_vmPopStatic( uiStatic );
}

ZH_BOOL zh_xvmPushVar( PZH_SYMBOL pSymbol )
{
   

   ZH_TRACE( ZH_TR_INFO, ( "zh_xvmPushVar(%p)", ( void * ) pSymbol ) );

   zh_vmPushVariable( pSymbol );

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmPopVar( PZH_SYMBOL pSymbol )
{
   

   ZH_TRACE( ZH_TR_INFO, ( "zh_xvmPopVar(%p)", ( void * ) pSymbol ) );

   zh_memvarSetValue( pSymbol, zh_stackItemFromTop( -1 ) );
   zh_stackPop();

   ZH_XVM_RETURN
}

void zh_xvmPushBlockShort( const ZH_BYTE * pCode, PZH_SYMBOL pSymbols )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmPushBlockShort(%p, %p)", ( const void * ) pCode, ( void * ) pSymbols ) );

   zh_vmPushBlockShort( pCode, pSymbols, ZH_FALSE );
}

void zh_xvmPushBlock( const ZH_BYTE * pCode, PZH_SYMBOL pSymbols )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmPushBlock(%p, %p)", ( const void * ) pCode, ( void * ) pSymbols ) );

   zh_vmPushBlock( pCode, pSymbols, ZH_FALSE );
}

void zh_xvmPushSelf( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmPushSelf()" ) );

   zh_vmPush( zh_stackSelfItem() );
}

void zh_xvmPushFuncSymbol( PZH_SYMBOL pSym )
{
   
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmPushFuncSymbol(%p)", ( void * ) pSym ) );

   pItem = zh_stackAllocItem();
   pItem->type = ZH_IT_SYMBOL;
   pItem->item.asSymbol.value = pSym;
   pItem->item.asSymbol.stackstate = NULL;
   zh_stackAllocItem()->type = ZH_IT_NIL;
}

ZH_BOOL zh_xvmPopLogical( ZH_BOOL * pfValue )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmPopLogical(%p)", ( void * ) pfValue ) );

   *pfValue = zh_vmPopLogical();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmPopAlias( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmPopAlias()" ) );

   zh_vmSelectWorkarea( zh_stackItemFromTop( -1 ), NULL ); /* it clears the passed item */
   zh_stackDec();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmSwapAlias( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmSwapAlias()" ) );

   zh_vmSwapAlias();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmPushField( PZH_SYMBOL pSymbol )
{
   

   ZH_TRACE( ZH_TR_INFO, ( "zh_xvmPushField(%p)", ( void * ) pSymbol ) );

   zh_rddGetFieldValue( zh_stackAllocItem(), pSymbol );

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmPushAlias( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmPushAlias()" ) );

   zh_vmPushAlias();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmPushAliasedField( PZH_SYMBOL pSymbol )
{
   

   ZH_TRACE( ZH_TR_INFO, ( "zh_xvmPushAliasedField(%p)", ( void * ) pSymbol ) );

   zh_vmPushAliasedField( pSymbol );

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmPushAliasedFieldExt( PZH_SYMBOL pAlias, PZH_SYMBOL pField )
{
   
   int iCurrArea;

   ZH_TRACE( ZH_TR_INFO, ( "zh_xvmPushAliasedFieldExt(%p,%p)", ( void * ) pAlias, ( void * ) pField ) );

   iCurrArea = zh_rddGetCurrentWorkAreaNumber();
   if( zh_rddSelectWorkAreaSymbol( pAlias ) == ZH_SUCCESS )
      zh_rddGetFieldValue( zh_stackAllocItem(), pField );
   zh_rddSelectWorkAreaNumber( iCurrArea );

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmPushAliasedVar( PZH_SYMBOL pSymbol )
{
   

   ZH_TRACE( ZH_TR_INFO, ( "zh_xvmPushAliasedVar(%p)", ( void * ) pSymbol ) );

   zh_vmPushAliasedVar( pSymbol );

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmPopField( PZH_SYMBOL pSymbol )
{
   

   ZH_TRACE( ZH_TR_INFO, ( "zh_xvmPopField(%p)", ( void * ) pSymbol ) );

   zh_rddPutFieldValue( zh_stackItemFromTop( -1 ), pSymbol );
   zh_stackPop();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmPushMemvar( PZH_SYMBOL pSymbol )
{
   

   ZH_TRACE( ZH_TR_INFO, ( "zh_xvmPushMemvar(%p)", ( void * ) pSymbol ) );

   zh_memvarGetValue( zh_stackAllocItem(), pSymbol );

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmPushMemvarByRef( PZH_SYMBOL pSymbol )
{
   

   ZH_TRACE( ZH_TR_INFO, ( "zh_xvmPushMemvarByRef(%p)", ( void * ) pSymbol ) );

   zh_memvarGetRefer( zh_stackAllocItem(), pSymbol );

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmPopMemvar( PZH_SYMBOL pSymbol )
{
   

   ZH_TRACE( ZH_TR_INFO, ( "zh_xvmPopMemvar(%p)", ( void * ) pSymbol ) );

   zh_memvarSetValue( pSymbol, zh_stackItemFromTop( -1 ) );
   zh_stackPop();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmPopAliasedField( PZH_SYMBOL pSymbol )
{
   

   ZH_TRACE( ZH_TR_INFO, ( "zh_xvmPopAliasedField(%p)", ( void * ) pSymbol ) );

   zh_vmPopAliasedField( pSymbol );

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmPopAliasedFieldExt( PZH_SYMBOL pAlias, PZH_SYMBOL pField )
{
   
   int iCurrArea;

   ZH_TRACE( ZH_TR_INFO, ( "zh_xvmPopAliasedFieldExt(%p,%p)", ( void * ) pAlias, ( void * ) pField ) );

   iCurrArea = zh_rddGetCurrentWorkAreaNumber();
   if( zh_rddSelectWorkAreaSymbol( pAlias ) == ZH_SUCCESS )
   {
      zh_rddPutFieldValue( zh_stackItemFromTop( -1 ), pField );
      zh_stackPop();
   }
   zh_rddSelectWorkAreaNumber( iCurrArea );

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmPopAliasedVar( PZH_SYMBOL pSymbol )
{
   

   ZH_TRACE( ZH_TR_INFO, ( "zh_xvmPopAliasedVar(%p)", ( void * ) pSymbol ) );

   zh_vmPopAliasedVar( pSymbol );

   ZH_XVM_RETURN
}

void zh_xvmLocalSetInt( int iLocal, ZH_LONG lValue )
{
   
   PZH_ITEM pLocal;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmLocalSetInt(%d, %ld)", iLocal, lValue ) );

   if( iLocal >= 0 )
   {
      /* local variable or local parameter */
      pLocal = zh_stackLocalVariable( iLocal );
      if( ZH_IS_BYREF( pLocal ) )
         pLocal = zh_itemUnRef( pLocal );
   }
   else
   {
      /* local variable referenced in a codeblock
       * zh_stackSelfItem() points to a codeblock that is currently evaluated
       */
      pLocal = zh_codeblockGetVar( zh_stackSelfItem(), iLocal );
   }

   if( ZH_IS_OBJECT( pLocal ) && zh_objHasOperator( pLocal, ZH_OO_OP_ASSIGN ) )
   {
      zh_vmPushLong( lValue );
      zh_objOperatorCall( ZH_OO_OP_ASSIGN, pLocal, pLocal,
                          zh_stackItemFromTop( -1 ), NULL );
      zh_stackPop();
   }
   else
   {
      zh_itemPutNL( pLocal, lValue );
   }
}

ZH_BOOL zh_xvmLocalAddInt( int iLocal, ZH_LONG lAdd )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmLocalAddInt(%d,%ld)", iLocal, lAdd ) );

   zh_vmAddInt( zh_stackLocalVariable( iLocal ), lAdd );

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmLocalInc( int iLocal )
{
   
   PZH_ITEM pLocal;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmLocalInc(%d)", iLocal ) );

   pLocal = zh_stackLocalVariable( iLocal );
   zh_vmInc( ZH_IS_BYREF( pLocal ) ? zh_itemUnRef( pLocal ) : pLocal );

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmLocalDec( int iLocal )
{
   
   PZH_ITEM pLocal;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmLocalDec(%d)", iLocal ) );

   pLocal = zh_stackLocalVariable( iLocal );
   zh_vmDec( ZH_IS_BYREF( pLocal ) ? zh_itemUnRef( pLocal ) : pLocal );

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmLocalIncPush( int iLocal )
{
   
   PZH_ITEM pLocal;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmLocalInc(%d)", iLocal ) );

   pLocal = zh_stackLocalVariable( iLocal );
   if( ZH_IS_BYREF( pLocal ) )
      pLocal = zh_itemUnRef( pLocal );
   zh_vmInc( pLocal );
   zh_itemCopy( zh_stackAllocItem(), pLocal );

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmLocalAdd( int iLocal )
{
   
   PZH_ITEM pLocal;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmLocalAdd(%d)", iLocal ) );

   pLocal = zh_stackLocalVariable( iLocal );
   if( ZH_IS_BYREF( pLocal ) )
      pLocal = zh_itemUnRef( pLocal );
   zh_vmPlus( pLocal, zh_stackItemFromTop( -2 ), zh_stackItemFromTop( -1 ) );
   zh_stackPop();
   zh_stackPop();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmStaticAdd( ZH_USHORT uiStatic )
{
   
   PZH_ITEM pStatic;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmStaticAdd(%hu)", uiStatic ) );

   pStatic = ( ( PZH_ITEM ) zh_stackGetStaticsBase() )->item.asArray.value->pItems + uiStatic - 1;
   if( ZH_IS_BYREF( pStatic ) )
      pStatic = zh_itemUnRef( pStatic );
   zh_vmPlus( pStatic, zh_stackItemFromTop( -2 ), zh_stackItemFromTop( -1 ) );
   zh_stackPop();
   zh_stackPop();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmMemvarAdd( PZH_SYMBOL pSymbol )
{
   
   PZH_ITEM pVal1, pVal2;

   ZH_TRACE( ZH_TR_INFO, ( "zh_xvmMemvarAdd(%p)", ( void * ) pSymbol ) );

   pVal1 = zh_stackItemFromTop( -2 );
   pVal2 = zh_stackItemFromTop( -1 );
   if( ZH_IS_STRING( pVal1 ) && ZH_IS_STRING( pVal2 ) )
   {
      PZH_ITEM pMemVar = zh_memvarGetItem( pSymbol );
      if( pMemVar )
      {
         zh_vmPlus( pMemVar, pVal1, pVal2 );
         zh_stackPop();
         zh_stackPop();
         ZH_XVM_RETURN
      }
   }

   zh_vmPlus( pVal1, pVal1, pVal2 );
   zh_memvarSetValue( pSymbol, pVal1 );
   zh_stackPop();
   zh_stackPop();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmAnd( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmAnd()" ) );

   zh_vmAnd();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmOr( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmOr()" ) );

   zh_vmOr();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmNot( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmNot()" ) );

   zh_vmNot();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmNegate( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmNegate()" ) );

   zh_vmNegate();

   ZH_XVM_RETURN
}

void zh_xvmDuplicate( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmDuplicate()" ) );

   zh_vmDuplicate();
}

void zh_xvmDuplUnRef( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmDuplUnRef()" ) );

   zh_vmDuplUnRef();
}

void zh_xvmPushUnRef( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmPushUnRef()" ) );

   zh_vmPushUnRef();
}

void zh_xvmSwap( int iCount )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmSwap(%d)", iCount ) );

   zh_vmSwap( iCount );
}

ZH_BOOL zh_xvmForTest( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmForTest()" ) );

   zh_vmForTest();

   ZH_XVM_RETURN
}

void zh_xvmFuncPtr( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmFuncPtr()" ) );

   zh_vmFuncPtr();
}

ZH_BOOL zh_xvmEqual( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmEqual()" ) );

   zh_vmEqual();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmExactlyEqual( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmExactlyEqual()" ) );

   zh_vmExactlyEqual();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmEqualInt( ZH_LONG lValue )
{
   
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmEqualInt(%ld)", lValue ) );

   pItem = zh_stackItemFromTop( -1 );
   if( ZH_IS_INTEGER( pItem ) )
   {
      pItem->item.asLogical.value = ( ZH_LONG ) pItem->item.asInteger.value == lValue;
      pItem->type = ZH_IT_LOGICAL;
   }
   else if( ZH_IS_LONG( pItem ) )
   {
      pItem->item.asLogical.value = pItem->item.asLong.value == ( ZH_MAXINT ) lValue;
      pItem->type = ZH_IT_LOGICAL;
   }
   else if( ZH_IS_DOUBLE( pItem ) )
   {
      pItem->item.asLogical.value = pItem->item.asDouble.value == ( double ) lValue;
      pItem->type = ZH_IT_LOGICAL;
   }
   else if( ZH_IS_NIL( pItem ) )
   {
      pItem->item.asLogical.value = ZH_FALSE;
      pItem->type = ZH_IT_LOGICAL;
   }
   else if( zh_objHasOperator( pItem, ZH_OO_OP_EQUAL ) )
   {
      zh_vmPushLong( lValue );
      zh_objOperatorCall( ZH_OO_OP_EQUAL, pItem, pItem,
                          zh_stackItemFromTop( -1 ), NULL );
      zh_stackPop();
   }
   else
   {
      PZH_ITEM pResult;

      zh_vmPushLong( lValue );
      pResult = zh_errRT_BASE_Subst( EG_ARG, 1071, NULL, "=", 2, pItem, zh_stackItemFromTop( -1 ) );

      if( pResult )
      {
         zh_stackPop();
         zh_itemMove( pItem, pResult );
         zh_itemRelease( pResult );
      }
   }

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmEqualIntIs( ZH_LONG lValue, ZH_BOOL * pfValue )
{
   
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmEqualIntIs(%ld,%p)", lValue, ( void * ) pfValue ) );

   pItem = zh_stackItemFromTop( -1 );
   if( ZH_IS_INTEGER( pItem ) )
   {
      *pfValue = ( ZH_LONG ) pItem->item.asInteger.value == lValue;
      zh_stackDec();
   }
   else if( ZH_IS_LONG( pItem ) )
   {
      *pfValue = pItem->item.asLong.value == ( ZH_MAXINT ) lValue;
      zh_stackDec();
   }
   else if( ZH_IS_DOUBLE( pItem ) )
   {
      *pfValue = pItem->item.asDouble.value == ( double ) lValue;
      zh_stackDec();
   }
   else if( ZH_IS_NIL( pItem ) )
   {
      *pfValue = ZH_FALSE;
      zh_stackDec();
   }
   else if( zh_objHasOperator( pItem, ZH_OO_OP_EQUAL ) )
   {
      zh_vmPushLong( lValue );
      zh_objOperatorCall( ZH_OO_OP_EQUAL, pItem, pItem,
                          zh_stackItemFromTop( -1 ), NULL );
      zh_stackPop();
      return zh_xvmPopLogical( pfValue );
   }
   else
   {
      PZH_ITEM pResult;

      zh_vmPushLong( lValue );
      pResult = zh_errRT_BASE_Subst( EG_ARG, 1071, NULL, "=", 2, pItem, zh_stackItemFromTop( -1 ) );

      if( pResult )
      {
         zh_stackPop();
         zh_itemMove( pItem, pResult );
         zh_itemRelease( pResult );
         return zh_xvmPopLogical( pfValue );
      }
   }

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmNotEqual( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmNotEqual()" ) );

   zh_vmNotEqual();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmNotEqualInt( ZH_LONG lValue )
{
   
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmNotEqualInt(%ld)", lValue ) );

   pItem = zh_stackItemFromTop( -1 );
   if( ZH_IS_INTEGER( pItem ) )
   {
      pItem->item.asLogical.value = ( ZH_LONG ) pItem->item.asInteger.value != lValue;
      pItem->type = ZH_IT_LOGICAL;
   }
   else if( ZH_IS_LONG( pItem ) )
   {
      pItem->item.asLogical.value = pItem->item.asLong.value != ( ZH_MAXINT ) lValue;
      pItem->type = ZH_IT_LOGICAL;
   }
   else if( ZH_IS_DOUBLE( pItem ) )
   {
      pItem->item.asLogical.value = pItem->item.asDouble.value != ( double ) lValue;
      pItem->type = ZH_IT_LOGICAL;
   }
   else if( ZH_IS_NIL( pItem ) )
   {
      pItem->item.asLogical.value = ZH_TRUE;
      pItem->type = ZH_IT_LOGICAL;
   }
   else if( zh_objHasOperator( pItem, ZH_OO_OP_NOTEQUAL ) )
   {
      zh_vmPushLong( lValue );
      zh_objOperatorCall( ZH_OO_OP_NOTEQUAL, pItem, pItem,
                          zh_stackItemFromTop( -1 ), NULL );
      zh_stackPop();
   }
   else
   {
      PZH_ITEM pResult;

      zh_vmPushLong( lValue );
      pResult = zh_errRT_BASE_Subst( EG_ARG, 1072, NULL, "<>", 2, pItem, zh_stackItemFromTop( -1 ) );

      if( pResult )
      {
         zh_stackPop();
         zh_itemMove( pItem, pResult );
         zh_itemRelease( pResult );
      }
   }

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmNotEqualIntIs( ZH_LONG lValue, ZH_BOOL * pfValue )
{
   
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmNotEqualIntIs(%ld,%p)", lValue, ( void * ) pfValue ) );

   pItem = zh_stackItemFromTop( -1 );
   if( ZH_IS_INTEGER( pItem ) )
   {
      *pfValue = ( ZH_LONG ) pItem->item.asInteger.value != lValue;
      zh_stackDec();
   }
   else if( ZH_IS_LONG( pItem ) )
   {
      *pfValue = pItem->item.asLong.value != ( ZH_MAXINT ) lValue;
      zh_stackDec();
   }
   else if( ZH_IS_DOUBLE( pItem ) )
   {
      *pfValue = pItem->item.asDouble.value != ( double ) lValue;
      zh_stackDec();
   }
   else if( ZH_IS_NIL( pItem ) )
   {
      *pfValue = ZH_TRUE;
      zh_stackDec();
   }
   else if( zh_objHasOperator( pItem, ZH_OO_OP_NOTEQUAL ) )
   {
      zh_vmPushLong( lValue );
      zh_objOperatorCall( ZH_OO_OP_NOTEQUAL, pItem, pItem,
                          zh_stackItemFromTop( -1 ), NULL );
      zh_stackPop();
      return zh_xvmPopLogical( pfValue );
   }
   else
   {
      PZH_ITEM pResult;

      zh_vmPushLong( lValue );
      pResult = zh_errRT_BASE_Subst( EG_ARG, 1072, NULL, "<>", 2, pItem, zh_stackItemFromTop( -1 ) );

      if( pResult )
      {
         zh_stackPop();
         zh_itemMove( pItem, pResult );
         zh_itemRelease( pResult );
         return zh_xvmPopLogical( pfValue );
      }
   }

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmLess( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmLess()" ) );

   zh_vmLess();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmLessThenInt( ZH_LONG lValue )
{
   
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmLessThenInt(%ld)", lValue ) );

   pItem = zh_stackItemFromTop( -1 );
   if( ZH_IS_INTEGER( pItem ) )
   {
      pItem->item.asLogical.value = ( ZH_LONG ) pItem->item.asInteger.value < lValue;
      pItem->type = ZH_IT_LOGICAL;
   }
   else if( ZH_IS_LONG( pItem ) )
   {
      pItem->item.asLogical.value = pItem->item.asLong.value < ( ZH_MAXINT ) lValue;
      pItem->type = ZH_IT_LOGICAL;
   }
   else if( ZH_IS_DOUBLE( pItem ) )
   {
      pItem->item.asLogical.value = pItem->item.asDouble.value < ( double ) lValue;
      pItem->type = ZH_IT_LOGICAL;
   }
   else if( zh_objHasOperator( pItem, ZH_OO_OP_LESS ) )
   {
      zh_vmPushLong( lValue );
      zh_objOperatorCall( ZH_OO_OP_LESS, pItem, pItem,
                          zh_stackItemFromTop( -1 ), NULL );
      zh_stackPop();
   }
   else
   {
      PZH_ITEM pResult;

      zh_vmPushLong( lValue );
      pResult = zh_errRT_BASE_Subst( EG_ARG, 1073, NULL, "<", 2, pItem, zh_stackItemFromTop( -1 ) );

      if( pResult )
      {
         zh_stackPop();
         zh_itemMove( pItem, pResult );
         zh_itemRelease( pResult );
      }
   }

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmLessThenIntIs( ZH_LONG lValue, ZH_BOOL * pfValue )
{
   
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmLessThenIntIs(%ld,%p)", lValue, ( void * ) pfValue ) );

   pItem = zh_stackItemFromTop( -1 );
   if( ZH_IS_INTEGER( pItem ) )
   {
      *pfValue = ( ZH_LONG ) pItem->item.asInteger.value < lValue;
      zh_stackDec();
   }
   else if( ZH_IS_LONG( pItem ) )
   {
      *pfValue = pItem->item.asLong.value < ( ZH_MAXINT ) lValue;
      zh_stackDec();
   }
   else if( ZH_IS_DOUBLE( pItem ) )
   {
      *pfValue = pItem->item.asDouble.value < ( double ) lValue;
      zh_stackDec();
   }
   else if( zh_objHasOperator( pItem, ZH_OO_OP_LESS ) )
   {
      zh_vmPushLong( lValue );
      zh_objOperatorCall( ZH_OO_OP_LESS, pItem, pItem,
                          zh_stackItemFromTop( -1 ), NULL );
      zh_stackPop();
      return zh_xvmPopLogical( pfValue );
   }
   else
   {
      PZH_ITEM pResult;

      zh_vmPushLong( lValue );
      pResult = zh_errRT_BASE_Subst( EG_ARG, 1073, NULL, "<", 2, pItem, zh_stackItemFromTop( -1 ) );

      if( pResult )
      {
         zh_stackPop();
         zh_itemMove( pItem, pResult );
         zh_itemRelease( pResult );
         return zh_xvmPopLogical( pfValue );
      }
   }

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmLessEqual( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmLessEqual()" ) );

   zh_vmLessEqual();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmLessEqualThenInt( ZH_LONG lValue )
{
   
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmLessEqualThenInt(%ld)", lValue ) );

   pItem = zh_stackItemFromTop( -1 );
   if( ZH_IS_INTEGER( pItem ) )
   {
      pItem->item.asLogical.value = ( ZH_LONG ) pItem->item.asInteger.value <= lValue;
      pItem->type = ZH_IT_LOGICAL;
   }
   else if( ZH_IS_LONG( pItem ) )
   {
      pItem->item.asLogical.value = pItem->item.asLong.value <= ( ZH_MAXINT ) lValue;
      pItem->type = ZH_IT_LOGICAL;
   }
   else if( ZH_IS_DOUBLE( pItem ) )
   {
      pItem->item.asLogical.value = pItem->item.asDouble.value <= ( double ) lValue;
      pItem->type = ZH_IT_LOGICAL;
   }
   else if( zh_objHasOperator( pItem, ZH_OO_OP_LESSEQUAL ) )
   {
      zh_vmPushLong( lValue );
      zh_objOperatorCall( ZH_OO_OP_LESSEQUAL, pItem, pItem,
                          zh_stackItemFromTop( -1 ), NULL );
      zh_stackPop();
   }
   else
   {
      PZH_ITEM pResult;

      zh_vmPushLong( lValue );
      pResult = zh_errRT_BASE_Subst( EG_ARG, 1074, NULL, "<=", 2, pItem, zh_stackItemFromTop( -1 ) );

      if( pResult )
      {
         zh_stackPop();
         zh_itemMove( pItem, pResult );
         zh_itemRelease( pResult );
      }
   }

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmLessEqualThenIntIs( ZH_LONG lValue, ZH_BOOL * pfValue )
{
   
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmLessEqualThenIntIs(%ld,%p)", lValue, ( void * ) pfValue ) );

   pItem = zh_stackItemFromTop( -1 );
   if( ZH_IS_INTEGER( pItem ) )
   {
      *pfValue = ( ZH_LONG ) pItem->item.asInteger.value <= lValue;
      zh_stackDec();
   }
   else if( ZH_IS_LONG( pItem ) )
   {
      *pfValue = pItem->item.asLong.value <= ( ZH_MAXINT ) lValue;
      zh_stackDec();
   }
   else if( ZH_IS_DOUBLE( pItem ) )
   {
      *pfValue = pItem->item.asDouble.value <= ( double ) lValue;
      zh_stackDec();
   }
   else if( zh_objHasOperator( pItem, ZH_OO_OP_LESSEQUAL ) )
   {
      zh_vmPushLong( lValue );
      zh_objOperatorCall( ZH_OO_OP_LESSEQUAL, pItem, pItem,
                          zh_stackItemFromTop( -1 ), NULL );
      zh_stackPop();
      return zh_xvmPopLogical( pfValue );
   }
   else
   {
      PZH_ITEM pResult;

      zh_vmPushLong( lValue );
      pResult = zh_errRT_BASE_Subst( EG_ARG, 1074, NULL, "<=", 2, pItem, zh_stackItemFromTop( -1 ) );

      if( pResult )
      {
         zh_stackPop();
         zh_itemMove( pItem, pResult );
         zh_itemRelease( pResult );
         return zh_xvmPopLogical( pfValue );
      }
   }

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmGreater( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmGreater()" ) );

   zh_vmGreater();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmGreaterThenInt( ZH_LONG lValue )
{
   
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmGreaterThenInt(%ld)", lValue ) );

   pItem = zh_stackItemFromTop( -1 );
   if( ZH_IS_INTEGER( pItem ) )
   {
      pItem->item.asLogical.value = ( ZH_LONG ) pItem->item.asInteger.value > lValue;
      pItem->type = ZH_IT_LOGICAL;
   }
   else if( ZH_IS_LONG( pItem ) )
   {
      pItem->item.asLogical.value = pItem->item.asLong.value > ( ZH_MAXINT ) lValue;
      pItem->type = ZH_IT_LOGICAL;
   }
   else if( ZH_IS_DOUBLE( pItem ) )
   {
      pItem->item.asLogical.value = pItem->item.asDouble.value > ( double ) lValue;
      pItem->type = ZH_IT_LOGICAL;
   }
   else if( zh_objHasOperator( pItem, ZH_OO_OP_GREATER ) )
   {
      zh_vmPushLong( lValue );
      zh_objOperatorCall( ZH_OO_OP_GREATER, pItem, pItem,
                          zh_stackItemFromTop( -1 ), NULL );
      zh_stackPop();
   }
   else
   {
      PZH_ITEM pResult;

      zh_vmPushLong( lValue );
      pResult = zh_errRT_BASE_Subst( EG_ARG, 1075, NULL, ">", 2, pItem, zh_stackItemFromTop( -1 ) );

      if( pResult )
      {
         zh_stackPop();
         zh_itemMove( pItem, pResult );
         zh_itemRelease( pResult );
      }
   }

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmGreaterThenIntIs( ZH_LONG lValue, ZH_BOOL * pfValue )
{
   
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmGreaterThenIntIs(%ld,%p)", lValue, ( void * ) pfValue ) );

   pItem = zh_stackItemFromTop( -1 );
   if( ZH_IS_INTEGER( pItem ) )
   {
      *pfValue = ( ZH_LONG ) pItem->item.asInteger.value > lValue;
      zh_stackDec();
   }
   else if( ZH_IS_LONG( pItem ) )
   {
      *pfValue = pItem->item.asLong.value > ( ZH_MAXINT ) lValue;
      zh_stackDec();
   }
   else if( ZH_IS_DOUBLE( pItem ) )
   {
      *pfValue = pItem->item.asDouble.value > ( double ) lValue;
      zh_stackDec();
   }
   else if( zh_objHasOperator( pItem, ZH_OO_OP_GREATER ) )
   {
      zh_vmPushLong( lValue );
      zh_objOperatorCall( ZH_OO_OP_GREATER, pItem, pItem,
                          zh_stackItemFromTop( -1 ), NULL );
      zh_stackPop();
      return zh_xvmPopLogical( pfValue );
   }
   else
   {
      PZH_ITEM pResult;

      zh_vmPushLong( lValue );
      pResult = zh_errRT_BASE_Subst( EG_ARG, 1075, NULL, ">", 2, pItem, zh_stackItemFromTop( -1 ) );

      if( pResult )
      {
         zh_stackPop();
         zh_itemMove( pItem, pResult );
         zh_itemRelease( pResult );
         return zh_xvmPopLogical( pfValue );
      }
   }

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmGreaterEqual( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmGreaterEqual()" ) );

   zh_vmGreaterEqual();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmGreaterEqualThenInt( ZH_LONG lValue )
{
   
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmGreaterEqualThenInt(%ld)", lValue ) );

   pItem = zh_stackItemFromTop( -1 );
   if( ZH_IS_INTEGER( pItem ) )
   {
      pItem->item.asLogical.value = ( ZH_LONG ) pItem->item.asInteger.value >= lValue;
      pItem->type = ZH_IT_LOGICAL;
   }
   else if( ZH_IS_LONG( pItem ) )
   {
      pItem->item.asLogical.value = pItem->item.asLong.value >= ( ZH_MAXINT ) lValue;
      pItem->type = ZH_IT_LOGICAL;
   }
   else if( ZH_IS_DOUBLE( pItem ) )
   {
      pItem->item.asLogical.value = pItem->item.asDouble.value >= ( double ) lValue;
      pItem->type = ZH_IT_LOGICAL;
   }
   else if( zh_objHasOperator( pItem, ZH_OO_OP_GREATEREQUAL ) )
   {
      zh_vmPushLong( lValue );
      zh_objOperatorCall( ZH_OO_OP_GREATEREQUAL, pItem, pItem,
                          zh_stackItemFromTop( -1 ), NULL );
      zh_stackPop();
   }
   else
   {
      PZH_ITEM pResult;

      zh_vmPushLong( lValue );
      pResult = zh_errRT_BASE_Subst( EG_ARG, 1076, NULL, ">=", 2, pItem, zh_stackItemFromTop( -1 ) );

      if( pResult )
      {
         zh_stackPop();
         zh_itemMove( pItem, pResult );
         zh_itemRelease( pResult );
      }
   }

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmGreaterEqualThenIntIs( ZH_LONG lValue, ZH_BOOL * pfValue )
{
   
   PZH_ITEM pItem;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmGreaterEqualThenIntIs(%ld,%p)", lValue, ( void * ) pfValue ) );

   pItem = zh_stackItemFromTop( -1 );
   if( ZH_IS_INTEGER( pItem ) )
   {
      *pfValue = ( ZH_LONG ) pItem->item.asInteger.value >= lValue;
      zh_stackDec();
   }
   else if( ZH_IS_LONG( pItem ) )
   {
      *pfValue = pItem->item.asLong.value >= ( ZH_MAXINT ) lValue;
      zh_stackDec();
   }
   else if( ZH_IS_DOUBLE( pItem ) )
   {
      *pfValue = pItem->item.asDouble.value >= ( double ) lValue;
      zh_stackDec();
   }
   else if( zh_objHasOperator( pItem, ZH_OO_OP_GREATEREQUAL ) )
   {
      zh_vmPushLong( lValue );
      zh_objOperatorCall( ZH_OO_OP_GREATEREQUAL, pItem, pItem,
                          zh_stackItemFromTop( -1 ), NULL );
      zh_stackPop();
      return zh_xvmPopLogical( pfValue );
   }
   else
   {
      PZH_ITEM pResult;

      zh_vmPushLong( lValue );
      pResult = zh_errRT_BASE_Subst( EG_ARG, 1074, NULL, "<=", 2, pItem, zh_stackItemFromTop( -1 ) );

      if( pResult )
      {
         zh_stackPop();
         zh_itemMove( pItem, pResult );
         zh_itemRelease( pResult );
         return zh_xvmPopLogical( pfValue );
      }
   }

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmInstring( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmInstring()" ) );

   zh_vmInstring();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmAddInt( ZH_LONG lAdd )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmAddInt(%ld)", lAdd ) );

   zh_vmAddInt( zh_stackItemFromTop( -1 ), lAdd );

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmPlus( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmPlus()" ) );

   zh_vmPlus( zh_stackItemFromTop( -2 ), zh_stackItemFromTop( -2 ),
              zh_stackItemFromTop( -1 ) );
   zh_stackPop();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmPlusEq( void )
{
   
   PZH_ITEM pResult, pValue;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmPlusEq()" ) );

   pResult = zh_itemUnRef( zh_stackItemFromTop( -2 ) );
   pValue = zh_stackItemFromTop( -1 );
   zh_vmPlus( pResult, pResult, pValue );
   zh_itemCopy( pValue, pResult );
   zh_itemMove( zh_stackItemFromTop( -2 ), pValue );
   zh_stackPop();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmPlusEqPop( void )
{
   
   PZH_ITEM pResult;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmPlusEqPop()" ) );

   pResult = zh_itemUnRef( zh_stackItemFromTop( -2 ) );
   zh_vmPlus( pResult, pResult, zh_stackItemFromTop( -1 ) );
   zh_stackPop();
   zh_stackPop();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmMinus( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmMinus()" ) );

   zh_vmMinus( zh_stackItemFromTop( -2 ), zh_stackItemFromTop( -2 ),
               zh_stackItemFromTop( -1 ) );
   zh_stackPop();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmMinusEq( void )
{
   
   PZH_ITEM pResult, pValue;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmMinusEq()" ) );

   pResult = zh_itemUnRef( zh_stackItemFromTop( -2 ) );
   pValue = zh_stackItemFromTop( -1 );
   zh_vmMinus( pResult, pResult, pValue );
   zh_itemCopy( pValue, pResult );
   zh_itemMove( zh_stackItemFromTop( -2 ), pValue );
   zh_stackPop();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmMinusEqPop( void )
{
   
   PZH_ITEM pResult;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmMinusEqPop()" ) );

   pResult = zh_itemUnRef( zh_stackItemFromTop( -2 ) );
   zh_vmMinus( pResult, pResult, zh_stackItemFromTop( -1 ) );
   zh_stackPop();
   zh_stackPop();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmMultByInt( ZH_LONG lValue )
{
   
   PZH_ITEM pValue;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmMultByInt(%ld)", lValue ) );

   pValue = zh_stackItemFromTop( -1 );

   if( ZH_IS_NUMERIC( pValue ) )
   {
      int iDec;
      double dValue = zh_itemGetNDDec( pValue, &iDec );

      zh_itemPutNumType( pValue, dValue * lValue, iDec,
                         ZH_ITEM_TYPERAW( pValue ), ZH_IT_INTEGER );
   }
   else if( zh_objHasOperator( pValue, ZH_OO_OP_MULT ) )
   {
      zh_vmPushLong( lValue );
      zh_objOperatorCall( ZH_OO_OP_MULT, pValue, pValue,
                          zh_stackItemFromTop( -1 ), NULL );
      zh_stackPop();
   }
   else
   {
      PZH_ITEM pSubst;

      zh_vmPushLong( lValue );
      pSubst = zh_errRT_BASE_Subst( EG_ARG, 1083, NULL, "*", 2, pValue, zh_stackItemFromTop( -1 ) );

      if( pSubst )
      {
         zh_stackPop();
         zh_itemMove( pValue, pSubst );
         zh_itemRelease( pSubst );
      }
   }

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmMult( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmMult()" ) );

   zh_vmMult( zh_stackItemFromTop( -2 ), zh_stackItemFromTop( -2 ), zh_stackItemFromTop( -1 ) );
   zh_stackPop();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmMultEq( void )
{
   
   PZH_ITEM pResult, pValue;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmMultEq()" ) );

   pResult = zh_itemUnRef( zh_stackItemFromTop( -2 ) );
   pValue = zh_stackItemFromTop( -1 );
   zh_vmMult( pResult, pResult, pValue );
   zh_itemCopy( pValue, pResult );
   zh_itemMove( zh_stackItemFromTop( -2 ), pValue );
   zh_stackPop();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmMultEqPop( void )
{
   
   PZH_ITEM pResult;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmMultEqPop()" ) );

   pResult = zh_itemUnRef( zh_stackItemFromTop( -2 ) );
   zh_vmMult( pResult, pResult, zh_stackItemFromTop( -1 ) );
   zh_stackPop();
   zh_stackPop();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmDivideByInt( ZH_LONG lDivisor )
{
   
   PZH_ITEM pValue;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmDivideByInt(%ld)", lDivisor ) );

   pValue = zh_stackItemFromTop( -1 );

   if( ZH_IS_NUMERIC( pValue ) )
   {
      if( lDivisor == 0 )
      {
         PZH_ITEM pSubst;

         zh_vmPushLong( lDivisor );
         pSubst = zh_errRT_BASE_Subst( EG_ZERODIV, 1340, NULL, "/", 2, pValue, zh_stackItemFromTop( -1 ) );

         if( pSubst )
         {
            zh_stackPop();
            zh_itemMove( pValue, pSubst );
            zh_itemRelease( pSubst );
         }
      }
      else
         zh_itemPutND( pValue, zh_itemGetND( pValue ) / lDivisor );
   }
   else if( zh_objHasOperator( pValue, ZH_OO_OP_DIVIDE ) )
   {
      zh_vmPushLong( lDivisor );
      zh_objOperatorCall( ZH_OO_OP_DIVIDE, pValue, pValue,
                          zh_stackItemFromTop( -1 ), NULL );
      zh_stackPop();
   }
   else
   {
      PZH_ITEM pSubst;

      zh_vmPushLong( lDivisor );
      pSubst = zh_errRT_BASE_Subst( EG_ARG, 1084, NULL, "/", 2, pValue, zh_stackItemFromTop( -1 ) );

      if( pSubst )
      {
         zh_stackPop();
         zh_itemMove( pValue, pSubst );
         zh_itemRelease( pSubst );
      }
   }

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmModulusByInt( ZH_LONG lDivisor )
{
   
   PZH_ITEM pValue;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmModulusByInt(%ld)", lDivisor ) );

   pValue = zh_stackItemFromTop( -1 );

   if( ZH_IS_NUMERIC( pValue ) )
   {
      if( lDivisor == 0 )
      {
         PZH_ITEM pSubst;

         zh_vmPushLong( lDivisor );
         pSubst = zh_errRT_BASE_Subst( EG_ZERODIV, 1341, NULL, "%", 2, pValue, zh_stackItemFromTop( -1 ) );

         if( pSubst )
         {
            zh_stackPop();
            zh_itemMove( pValue, pSubst );
            zh_itemRelease( pSubst );
         }
      }
      else if( ZH_IS_NUMINT( pValue ) )
         zh_itemPutND( pValue, ( double ) ( ZH_ITEM_GET_NUMINTRAW( pValue ) % lDivisor ) );
      else
         zh_itemPutND( pValue, fmod( zh_itemGetND( pValue ), lDivisor ) );

   }
   else if( zh_objHasOperator( pValue, ZH_OO_OP_MOD ) )
   {
      zh_vmPushLong( lDivisor );
      zh_objOperatorCall( ZH_OO_OP_MOD, pValue, pValue,
                          zh_stackItemFromTop( -1 ), NULL );
      zh_stackPop();
   }
   else
   {
      PZH_ITEM pSubst;

      zh_vmPushLong( lDivisor );
      pSubst = zh_errRT_BASE_Subst( EG_ARG, 1085, NULL, "%", 2, pValue, zh_stackItemFromTop( -1 ) );

      if( pSubst )
      {
         zh_stackPop();
         zh_itemMove( pValue, pSubst );
         zh_itemRelease( pSubst );
      }
   }

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmDivide( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmDivide()" ) );

   zh_vmDivide( zh_stackItemFromTop( -2 ), zh_stackItemFromTop( -2 ), zh_stackItemFromTop( -1 ) );
   zh_stackPop();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmDivEq( void )
{
   
   PZH_ITEM pResult, pValue;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmDivEq()" ) );

   pResult = zh_itemUnRef( zh_stackItemFromTop( -2 ) );
   pValue = zh_stackItemFromTop( -1 );
   zh_vmDivide( pResult, pResult, pValue );
   zh_itemCopy( pValue, pResult );
   zh_itemMove( zh_stackItemFromTop( -2 ), pValue );
   zh_stackPop();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmDivEqPop( void )
{
   
   PZH_ITEM pResult;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmDivEqPop()" ) );

   pResult = zh_itemUnRef( zh_stackItemFromTop( -2 ) );
   zh_vmDivide( pResult, pResult, zh_stackItemFromTop( -1 ) );
   zh_stackPop();
   zh_stackPop();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmModulus( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmModulus()" ) );

   zh_vmModulus( zh_stackItemFromTop( -2 ), zh_stackItemFromTop( -2 ), zh_stackItemFromTop( -1 ) );
   zh_stackPop();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmModEq( void )
{
   
   PZH_ITEM pResult, pValue;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmModEq()" ) );

   pResult = zh_itemUnRef( zh_stackItemFromTop( -2 ) );
   pValue = zh_stackItemFromTop( -1 );
   zh_vmModulus( pResult, pResult, pValue );
   zh_itemCopy( pValue, pResult );
   zh_itemMove( zh_stackItemFromTop( -2 ), pValue );
   zh_stackPop();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmModEqPop( void )
{
   
   PZH_ITEM pResult;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmModEqPop()" ) );

   pResult = zh_itemUnRef( zh_stackItemFromTop( -2 ) );
   zh_vmModulus( pResult, pResult, zh_stackItemFromTop( -1 ) );
   zh_stackPop();
   zh_stackPop();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmPower( void )
{
   
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmPower()" ) );

   zh_vmPower( zh_stackItemFromTop( -2 ), zh_stackItemFromTop( -2 ), zh_stackItemFromTop( -1 ) );
   zh_stackPop();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmExpEq( void )
{
   
   PZH_ITEM pResult, pValue;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmExpEq()" ) );

   pResult = zh_itemUnRef( zh_stackItemFromTop( -2 ) );
   pValue = zh_stackItemFromTop( -1 );
   zh_vmPower( pResult, pResult, pValue );
   zh_itemCopy( pValue, pResult );
   zh_itemMove( zh_stackItemFromTop( -2 ), pValue );
   zh_stackPop();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmExpEqPop( void )
{
   
   PZH_ITEM pResult;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmExpEqPop()" ) );

   pResult = zh_itemUnRef( zh_stackItemFromTop( -2 ) );
   zh_vmPower( pResult, pResult, zh_stackItemFromTop( -1 ) );
   zh_stackPop();
   zh_stackPop();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmInc( void )
{
   
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmInc()" ) );

   zh_vmInc( zh_stackItemFromTop( -1 ) );

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmIncEq( void )
{
   
   PZH_ITEM pResult, pValue, pTemp;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmIncEq()" ) );

   pResult = zh_stackItemFromTop( -1 );
   pValue = zh_itemUnRef( pResult );
   zh_vmInc( pValue );
   pTemp = zh_stackAllocItem();
   zh_itemCopy( pTemp, pValue );
   zh_itemMove( pResult, pTemp );
   zh_stackDec();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmIncEqPop( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmIncEqPop()" ) );

   zh_vmInc( zh_itemUnRef( zh_stackItemFromTop( -1 ) ) );
   zh_stackPop();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmDec( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmDec()" ) );

   zh_vmDec( zh_stackItemFromTop( -1 ) );

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmDecEq( void )
{
   
   PZH_ITEM pResult, pValue, pTemp;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmDecEq()" ) );

   pResult = zh_stackItemFromTop( -1 );
   pValue = zh_itemUnRef( pResult );
   zh_vmDec( pValue );
   pTemp = zh_stackAllocItem();
   zh_itemCopy( pTemp, pValue );
   zh_itemMove( pResult, pTemp );
   zh_stackDec();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmDecEqPop( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmDecEqPop()" ) );

   zh_vmDec( zh_itemUnRef( zh_stackItemFromTop( -1 ) ) );
   zh_stackPop();

   ZH_XVM_RETURN
}

void zh_xvmArrayDim( ZH_USHORT uiDimensions )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmArrayDim(%hu)", uiDimensions ) );

   zh_vmArrayDim( uiDimensions );
}

void zh_xvmArrayGen( ZH_SIZE nElements )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmArrayGen(%" ZH_PFS "u)", nElements ) );

   zh_vmArrayGen( nElements );
}

void zh_xvmHashGen( ZH_SIZE nElements )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmHashGen(%" ZH_PFS "u)", nElements ) );

   zh_vmHashGen( nElements );
}

static void zh_vmArrayItemPush( ZH_SIZE nIndex )
{
   
   PZH_ITEM pArray;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmArrayItemPush(%" ZH_PFS "u)", nIndex ) );

   pArray = zh_stackItemFromTop( -1 );

   if( ZH_IS_ARRAY( pArray ) )
   {
      if( ZH_IS_OBJECT( pArray ) && zh_objHasOperator( pArray, ZH_OO_OP_ARRAYINDEX ) )
      {
         zh_vmPushNumInt( nIndex );
         zh_objOperatorCall( ZH_OO_OP_ARRAYINDEX, pArray, pArray,
                             zh_stackItemFromTop( -1 ), NULL );
         zh_stackPop();
         return;
      }

      if( ZH_IS_VALID_INDEX( nIndex, pArray->item.asArray.value->nLen ) )
      {
         PZH_ITEM pItem = zh_stackAllocItem();

         zh_itemCopy( pItem, pArray->item.asArray.value->pItems + nIndex - 1 );
         zh_itemMove( pArray, pItem );
         zh_stackDec();
      }
      else
      {
         zh_vmPushNumInt( nIndex );
         if( ! ZH_IS_OBJECT( pArray ) &&
             zh_objOperatorCall( ZH_OO_OP_ARRAYINDEX, pArray, pArray,
                                 zh_stackItemFromTop( -1 ), NULL ) )
            zh_stackPop();
         else
            zh_errRT_BASE( EG_BOUND, 1132, NULL, zh_langDGetErrorDesc( EG_ARRACCESS ),
                           2, pArray, zh_stackItemFromTop( -1 ) );
      }
   }
   else if( ZH_IS_HASH( pArray ) )
   {
      PZH_ITEM pValue, pIndex;

      zh_vmPushNumInt( nIndex );
      pIndex = zh_stackItemFromTop( -1 );
      pValue = zh_hashGetItemPtr( pArray, pIndex, ZH_HASH_AUTOADD_ACCESS );

      if( pValue )
      {
         zh_itemCopy( pIndex, pValue );
         zh_itemMove( pArray, pIndex );
         zh_stackDec();
      }
      else if( zh_objOperatorCall( ZH_OO_OP_ARRAYINDEX, pArray, pArray,
                                   pIndex, NULL ) )
         zh_stackPop();
      else
         zh_errRT_BASE( EG_BOUND, 1132, NULL, zh_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, pIndex );
   }
   else
   {
      zh_vmPushNumInt( nIndex );
      if( zh_objOperatorCall( ZH_OO_OP_ARRAYINDEX, pArray, pArray,
                              zh_stackItemFromTop( -1 ), NULL ) )
         zh_stackPop();
      else
         zh_errRT_BASE( EG_ARG, 1068, NULL, zh_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, zh_stackItemFromTop( -1 ) );
   }
}

static void zh_vmArrayItemPop( ZH_SIZE nIndex )
{
   
   PZH_ITEM pValue;
   PZH_ITEM pArray;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmArrayItemPop(%" ZH_PFS "u)", nIndex ) );

   pValue = zh_stackItemFromTop( -2 );
   pArray = zh_stackItemFromTop( -1 );

   if( ZH_IS_BYREF( pArray ) )
      pArray = zh_itemUnRef( pArray );

   if( ZH_IS_ARRAY( pArray ) )
   {
      if( ZH_IS_OBJECT( pArray ) && zh_objHasOperator( pArray, ZH_OO_OP_ARRAYINDEX ) )
      {
         zh_vmPushNumInt( nIndex );
         zh_objOperatorCall( ZH_OO_OP_ARRAYINDEX, pArray, pArray,
                             zh_stackItemFromTop( -1 ), pValue );
         zh_stackPop();
         zh_stackPop();
         zh_stackPop();
         return;
      }

      if( ZH_IS_VALID_INDEX( nIndex, pArray->item.asArray.value->nLen ) )
      {
         pValue->type &= ~( ZH_IT_MEMOFLAG | ZH_IT_DEFAULT );
         zh_itemMoveRef( pArray->item.asArray.value->pItems + nIndex - 1, pValue );
         zh_stackPop();
         zh_stackDec();    /* value was moved above zh_stackDec() is enough */
      }
      else
      {
         zh_vmPushNumInt( nIndex );
         if( ! ZH_IS_OBJECT( pArray ) &&
             zh_objOperatorCall( ZH_OO_OP_ARRAYINDEX, pArray, pArray,
                                 zh_stackItemFromTop( -1 ), pValue ) )
         {
            zh_stackPop();
            zh_stackPop();
            zh_stackPop();
         }
         else

            zh_errRT_BASE( EG_BOUND, 1133, NULL, zh_langDGetErrorDesc( EG_ARRASSIGN ),
                           1, zh_stackItemFromTop( -1 ) );
      }
   }
   else if( ZH_IS_HASH( pArray ) )
   {
      PZH_ITEM pDest;

      zh_vmPushNumInt( nIndex );
      pDest = zh_hashGetItemPtr( pArray, zh_stackItemFromTop( -1 ), ZH_HASH_AUTOADD_ASSIGN );

      if( pDest )
      {
         pValue->type &= ~( ZH_IT_MEMOFLAG | ZH_IT_DEFAULT );
         zh_itemMoveRef( pDest, pValue );
         zh_stackPop();
         zh_stackPop();
         zh_stackDec();    /* value was moved above zh_stackDec() is enough */
      }
      else if( zh_objOperatorCall( ZH_OO_OP_ARRAYINDEX, pArray, pArray,
                                   zh_stackItemFromTop( -1 ), pValue ) )
      {
         zh_stackPop();
         zh_stackPop();
         zh_stackPop();
      }
      else
         zh_errRT_BASE( EG_BOUND, 1133, NULL, zh_langDGetErrorDesc( EG_ARRASSIGN ), 3, pArray, zh_stackItemFromTop( -1 ), pValue );
   }
   else
   {
      zh_vmPushNumInt( nIndex );
      if( zh_objOperatorCall( ZH_OO_OP_ARRAYINDEX, pArray, pArray,
                              zh_stackItemFromTop( -1 ), pValue ) )
      {
         zh_stackPop();
         zh_stackPop();
         zh_stackPop();
      }
      else
         zh_errRT_BASE( EG_ARG, 1069, NULL, zh_langDGetErrorDesc( EG_ARRASSIGN ),
                        1, zh_stackItemFromTop( -1 ) );
   }
}


ZH_BOOL zh_xvmArrayPush( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmArrayPush()" ) );

   zh_vmArrayPush();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmArrayPushRef( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmArrayPushRef()" ) );

   zh_vmArrayPushRef();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmArrayItemPush( ZH_SIZE nIndex )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmArrayItemPush(%" ZH_PFS "u)", nIndex ) );

   zh_vmArrayItemPush( nIndex );

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmArrayPop( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmArrayPop()" ) );

   zh_vmArrayPop();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmArrayItemPop( ZH_SIZE nIndex )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmArrayItemPop(%" ZH_PFS "u)", nIndex ) );

   zh_vmArrayItemPop( nIndex );

   ZH_XVM_RETURN
}

void zh_xvmPushDouble( double dNumber, int iWidth, int iDec )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmPushDouble(%lf, %d, %d)", dNumber, iWidth, iDec ) );

   zh_vmPushDoubleConst( dNumber, iWidth, iDec );
}

#ifdef ZH_LONG_LONG_OFF
void zh_xvmPushLongLong( double dNumber )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmPushLongLong(%l.0f)", dNumber ) );

   zh_vmPushDoubleConst( dNumber, ZH_DEFAULT_WIDTH, 0 );
}
#else
void zh_xvmPushLongLong( ZH_LONGLONG llNumber )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmPushLongLong(%" PFLL "i)", llNumber ) );

   zh_vmPushLongLongConst( llNumber );
}
#endif

void zh_xvmPushStringHidden( int iMethod, const char * szText, ZH_SIZE nSize )
{
   
   char * szString;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmPushStringHidden(%d, %s, %" ZH_PFS "u)", iMethod, szText, nSize ) );

   szString = zh_compDecodeString( iMethod, szText, &nSize );
   zh_itemPutCLPtr( zh_stackAllocItem(), szString, nSize );
}

void zh_xvmLocalName( ZH_USHORT uiLocal, const char * szLocalName )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmLocalName(%hu, %s)", uiLocal, szLocalName ) );


   zh_vmLocalName( uiLocal, szLocalName );
}

void zh_xvmStaticName( ZH_BYTE bIsGlobal, ZH_USHORT uiStatic, const char * szStaticName )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmStaticName(%d, %hu, %s)", ( int ) bIsGlobal, uiStatic, szStaticName ) );

   zh_vmStaticName( bIsGlobal, uiStatic, szStaticName );
}

void zh_xvmModuleName( const char * szModuleName )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmModuleName(%s)", szModuleName ) );

   zh_vmModuleName( szModuleName );
}

ZH_BOOL zh_xvmMacroArrayGen( ZH_USHORT uiArgSets )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmMacroArrayGen(%hu)", uiArgSets ) );

   zh_vmMacroArrayGen( uiArgSets );

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmMacroDo( ZH_USHORT uiArgSets )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmMacroDo(%hu)", uiArgSets ) );

   zh_vmMacroDo( uiArgSets );

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmMacroFunc( ZH_USHORT uiArgSets )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmMacroFunc(%hu)", uiArgSets ) );

   zh_vmMacroFunc( uiArgSets );

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmMacroSend( ZH_USHORT uiArgSets )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmMacroSend(%hu)", uiArgSets ) );

   zh_vmMacroSend( uiArgSets );

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmMacroPush( int iFlags )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmMacroPush(%d)", iFlags ) );

   zh_macroGetValue( zh_stackItemFromTop( -1 ), 0, iFlags );

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmMacroPushRef( void )
{
   

   PZH_ITEM pMacro;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmMacroPushRef()" ) );

   pMacro = zh_stackItemFromTop( -1 );
   zh_macroPushReference( pMacro );

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmMacroPushIndex( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmMacroPushIndex()" ) );

   zh_vmMacroPushIndex();

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmMacroPushList( int iFlags )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmMacroPushList(%d)", iFlags ) );

   zh_macroGetValue( zh_stackItemFromTop( -1 ), ZH_P_MACRO_PUSHLIST, iFlags );

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmMacroPushPare( int iFlags )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmMacroPushPare(%d)", iFlags ) );

   zh_macroGetValue( zh_stackItemFromTop( -1 ), ZH_P_MACRO_PUSHPARE, iFlags );

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmMacroPushAliased( int iFlags )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmMacroPushAliased(%d)", iFlags ) );

   zh_macroPushAliasedValue( zh_stackItemFromTop( -2 ), zh_stackItemFromTop( -1 ), iFlags );

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmMacroPop( int iFlags )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmMacroPop(%d)", iFlags ) );

   zh_macroSetValue( zh_stackItemFromTop( -1 ), iFlags );

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmMacroPopAliased( int iFlags )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmMacroPopAliased(%d)", iFlags ) );

   zh_macroPopAliasedValue( zh_stackItemFromTop( -2 ), zh_stackItemFromTop( -1 ), iFlags );

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmMacroSymbol( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmMacroSymbol()" ) );

   zh_macroPushSymbol( zh_stackItemFromTop( -1 ) );

   ZH_XVM_RETURN
}

ZH_BOOL zh_xvmMacroText( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmMacroText()" ) );

   zh_macroTextValue( zh_stackItemFromTop( -1 ) );

   ZH_XVM_RETURN
}

void zh_xvmPushVParams( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmPushVParams()" ) );

   zh_vmPushVParams();
}

void zh_xvmPushAParams( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmPushAParams()" ) );

   zh_vmPushAParams();
}

void zh_xvmWithObjectStart( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmWithObjectStart()" ) );

   zh_vmWithObjectStart();
}

void zh_xvmWithObjectEnd( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmWithObjectEnd()" ) );

   zh_stackPop();  /* remove with object envelope */
   zh_stackPop();  /* remove implicit object */
}

void zh_xvmWithObjectMessage( PZH_SYMBOL pSymbol )
{
   PZH_ITEM pWith;

   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_xvmWithObjectMessage(%p)", ( void * ) pSymbol ) );

   if( pSymbol )
      zh_vmPushSymbol( pSymbol );

   pWith = zh_stackWithObjectItem();
   if( pWith )
      zh_vmPush( pWith );
   else
      zh_stackAllocItem()->type = ZH_IT_NIL;
}

/* ------------------------------------------------------------------------ */
/* The debugger support functions */
/* ------------------------------------------------------------------------ */

void zh_vmRequestDebug( void )
{
   

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmRequestDebug()" ) );

   *( zh_stackDebugRequest() ) = ZH_TRUE;

}

ZH_BOOL zh_dbg_InvokeDebug( ZH_BOOL bInvoke )
{
   
   ZH_BOOL * pfRequest = zh_stackDebugRequest();
   ZH_BOOL bRequest = *pfRequest;
   *pfRequest = bInvoke;
   return bRequest;
}

ZH_DBGENTRY_FUNC zh_dbg_SetEntry( ZH_DBGENTRY_FUNC pFunDbgEntry )
{
   ZH_DBGENTRY_FUNC pPrevFunc;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_dbg_SetEntry(%p)", ( void * ) pFunDbgEntry ) );

   pPrevFunc = s_pFunDbgEntry;
   s_pFunDbgEntry = pFunDbgEntry;

   return pPrevFunc;
}

PZH_ITEM zh_dbg_vmVarSGet( PZH_ITEM pStaticsBase, int nOffset )
{
   if( pStaticsBase )
      return zh_arrayGetItemPtr( pStaticsBase, nOffset );
   else
      return NULL;
}

ZH_ULONG zh_dbg_ProcLevel( void )
{
   return zh_stackCallDepth();
}

/*
 * check if the debugger activation was requested or request the debugger
 * activation if .T. is passed
 */
ZH_FUNC( __DBGINVOKEDEBUG )
{
   

   if( zh_vmInternalsEnabled() )
   {
      ZH_BOOL * pfRequest = zh_stackDebugRequest();

      zh_retl( *pfRequest );
      *pfRequest = zh_parl( 1 );
   }
   else
      zh_retl( ZH_FALSE );
}

/* Return the statics array. Please AClone() before assignments
 * __dbgVMVarSList() --> <aStat>
 */
ZH_FUNC( __DBGVMVARSLIST )
{
   if( zh_vmInternalsEnabled() )
      zh_itemReturnRelease( zh_vmStaticsArray() );
   else
   {
      
      zh_reta( 0 );
   }
}

/* Return the statics array length.
 * __dbgVMVarSLen() --> <nStatics>
 */
ZH_FUNC( __DBGVMVARSLEN )
{
   

   if( zh_vmInternalsEnabled() )
      zh_retnint( zh_vmStaticsCount() );
   else
      zh_retnint( 0 );
}

/* Return a specified statics
 * __dbgVMVarSGet( <nStatic> ) --> <xStat>
 */
ZH_FUNC( __DBGVMVARSGET )
{
   if( zh_vmInternalsEnabled() )
      zh_itemReturn( zh_dbg_vmVarSGet( zh_param( 1, ZH_IT_ARRAY ), zh_parni( 2 ) ) );
}

/*
 * Sets the value of a specified statics
 * __dbgVMVarSSet( <nStatic>, <uValue> ) --> NIL
 */
ZH_FUNC( __DBGVMVARSSET )
{
   if( zh_vmInternalsEnabled() )
   {
      PZH_ITEM pStaticsBase = zh_param( 1, ZH_IT_ARRAY );
      PZH_ITEM pItem = zh_param( 3, ZH_IT_ANY );

      if( pStaticsBase && pItem )
         zh_arraySet( pStaticsBase, zh_parni( 2 ), pItem );
   }
}

ZH_FUNC( __DBGPROCLEVEL )
{
   if( zh_vmInternalsEnabled() )
   {
      
      zh_retnl( zh_dbg_ProcLevel() - 1 );   /* Don't count self */
   }
}

/*
 * These functions are for GLOBAL variables - now they are only for
 * compatibility with xZiher debugger - Ziher does not support
 * GLOBALs
 */
ZH_ULONG zh_dbg_vmVarGCount( void )
{
   return 0;
}

PZH_ITEM zh_dbg_vmVarGGet( int nGlobal, int nOffset )
{
   ZH_SYMBOL_UNUSED( nGlobal );
   ZH_SYMBOL_UNUSED( nOffset );
   return NULL;
}

/*
 * Return a clone of the globals array.
 * __dbgVMVarGList() --> <aStat>
 */
ZH_FUNC( __DBGVMVARGLIST )
{
   if( zh_vmInternalsEnabled() )
   {
      PZH_ITEM pGlobals = zh_itemArrayNew( 0 );

      zh_itemReturnRelease( pGlobals );
   }
   else
   {
      
      zh_reta( 0 );
   }
}

ZH_FUNC( __DBGVMVARGGET )
{
   if( zh_vmInternalsEnabled() )
      zh_itemReturn( zh_dbg_vmVarGGet( zh_parni( 1 ), zh_parni( 2 ) ) );
}

ZH_FUNC( __DBGVMVARGSET )
{
#if 0
   if( zh_vmInternalsEnabled() )
   {
      PZH_ITEM pItem = zh_param( 3, ZH_IT_ANY );
      if( pItem )
         zh_arraySet( &s_aGlobals, zh_parni( 1 ) + zh_parni( 2 ), pItem );
   }
#endif
}


/* ------------------------------------------------------------------------ */
/* The garbage collector interface */
/* ------------------------------------------------------------------------ */

/* Mark all statics as used so they will not be released by the
 * garbage collector
 */
void zh_vmIsStaticRef( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmIsStaticRef()" ) );

   /* statics are stored as an item of arrays allocated by zh_itemNew() so
    * they do not need any special GC support
    */
}

void zh_vmIsStackRef( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmIsStackRef()" ) );

   if( s_vmStackLst )
   {
      PZH_THREADSTATE pStack = s_vmStackLst;
      do
      {
         zh_gcMark( pStack );
         if( pStack->fActive && pStack->pStackId )
            zh_stackIsStackRef( pStack->pStackId, zh_vmTSVarClean );
         pStack = pStack->pNext;
      }
      while( pStack != s_vmStackLst );
   }
}

void zh_vmUpdateAllocator( PZH_ALLOCUPDT_FUNC pFunc, int iCount )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmUpdateAllocator(%p, %d)", ( void * ) pFunc, iCount ) );

   if( s_vmStackLst )
   {
      PZH_THREADSTATE pStack = s_vmStackLst;
      do
      {
         if( pStack->pStackId )
            zh_stackUpdateAllocator( pStack->pStackId, pFunc, iCount );
         pStack = pStack->pNext;
      }
      while( pStack != s_vmStackLst );
   }
}

/* ------------------------------------------------------------------------ */

/*
 * Turns on | off the profiler activity
 * __SetProfiler( <lOnOff> ) --> <lOldValue>
 */
ZH_FUNC( __SETPROFILER )
{
   
#ifdef ZH_NO_PROFILER
   zh_retl( ZH_FALSE );
#else
   zh_retl( zh_bProfiler );
   if( ZH_ISLOGICAL( 1 ) )
      zh_bProfiler = zh_parl( 1 );
#endif
}

ZH_FUNC( __OPCOUNT ) /* it returns the total amount of opcodes */
{
   
   zh_retnl( ZH_P_LAST_PCODE - 1 );
}

ZH_FUNC( __OPGETPRF ) /* profiler: It returns an array with an opcode called and
                         consumed times { nTimes, nTime },
                         given the opcode index */
{
   
#ifndef ZH_NO_PROFILER
   ZH_ULONG ulOpcode = zh_parnl( 1 );

   zh_reta( 2 );
   if( ulOpcode < ZH_P_LAST_PCODE )
   {
      zh_storvnl( zh_ulOpcodesCalls[ ulOpcode ], -1, 1 );
      zh_storvnl( zh_ulOpcodesTime[ ulOpcode ], -1, 2 );
   }
   else
#else
   zh_reta( 2 );
#endif
   {
      zh_storvnl( 0, -1, 1 );
      zh_storvnl( 0, -1, 2 );
   }
}

/*
 * Turns on | off tracing of ZH-level function and method calls
 * __TracePrgCalls( <lOnOff> ) --> <lOldValue>
 */
ZH_FUNC( __TRACEPRGCALLS )
{
   
#if defined( ZH_PRG_TRACE )
   zh_retl( zh_bTracePrgCalls );
   if( ZH_ISLOGICAL( 1 ) )
      zh_bTracePrgCalls = zh_parl( 1 );
#else
   zh_retl( ZH_FALSE );
#endif
}

ZH_FUNC( __QUITCANCEL )
{
   

   if( ! zh_stackQuitState() )
   {
      ZH_I_SIZE nRecoverBase = zh_stackGetRecoverBase();

      if( nRecoverBase )
      {
         PZH_ITEM pRecover = zh_stackItem( nRecoverBase + ZH_RECOVER_STATE );

#if defined( _ZH_RECOVER_DEBUG )
         if( pRecover->type != ZH_IT_RECOVER )
            zh_errInternal( ZH_EI_ERRUNRECOV, "zh_vmRequestBreak", NULL, NULL );
#endif
         if( pRecover->item.asRecover.flags & ZH_SEQ_DOALWAYS )
         {
            pRecover->item.asRecover.flags   &= ~ZH_QUIT_REQUESTED;
            pRecover->item.asRecover.request &= ~ZH_QUIT_REQUESTED;
         }
      }
   }
}

ZH_FUNC( __VMNOINTERNALS )
{
   s_fInternalsEnabled = ZH_FALSE;
}

ZH_FUNC( __VMITEMID )
{
   

   PZH_ITEM pItem = zh_param( 1, ZH_IT_ANY );

   if( pItem )
   {
      if( ZH_IS_ARRAY( pItem ) )
         zh_retptr( zh_arrayId( pItem ) );
      else if( ZH_IS_HASH( pItem ) )
         zh_retptr( zh_hashId( pItem ) );
      else if( ZH_IS_BLOCK( pItem ) )
         zh_retptr( zh_codeblockId( pItem ) );
      else if( ZH_IS_SYMBOL( pItem ) )
         zh_retptr( pItem->item.asSymbol.value );
   }
}

ZH_FUNC( __VMITEMREFS )
{
   

   PZH_ITEM pItem = zh_param( 1, ZH_IT_ANY );

   if( pItem )
   {
      if( ZH_IS_ARRAY( pItem ) )
         zh_retnint( zh_arrayRefs( pItem ) );
      else if( ZH_IS_HASH( pItem ) )
         zh_retnint( zh_hashRefs( pItem ) );
      else if( ZH_IS_BLOCK( pItem ) )
         zh_retnint( zh_codeblockRefs( pItem ) );
      else if( ZH_IS_POINTER( pItem ) )
         zh_retnint( zh_gcRefCount( pItem->item.asPointer.value ) );
      else if( ZH_IS_STRING( pItem ) )
         zh_retnint( zh_xRefCount( pItem->item.asString.value ) );
   }
}

ZH_FUNC( __VMMODULESVERIFY )
{
   

   zh_vmVerifySymbols( zh_stackReturnItem() );
}

ZH_FUNC( __VMCOUNTTHREADS )
{
   int iStacks, iThreads;

   

   ZH_VM_LOCK();

   iStacks = s_iStackCount;
   iThreads = s_iRunningCount;

   ZH_VM_UNLOCK();

   zh_storni( iStacks, 1 );
   zh_storni( iThreads, 2 );

   zh_retni( iThreads );
}

ZH_FUNC( __BREAKBLOCK )
{
   zh_itemReturn( zh_breakBlock() );
}

ZH_FUNC( __RECOVERERRORBLOCK )
{
   

   ZH_I_SIZE nRecoverBase = zh_stackGetRecoverBase();

   if( nRecoverBase > 0 && nRecoverBase < zh_stackTopOffset() )
   {
      PZH_ITEM pItem = zh_stackItem( nRecoverBase );

      if( ZH_IS_POINTER( pItem ) &&
          pItem->item.asPointer.collect && pItem->item.asPointer.single &&
          zh_gcFuncs( pItem->item.asPointer.value ) == &s_gcSeqBlockFuncs )
         zh_itemReturn( ( PZH_ITEM ) pItem->item.asPointer.value );
   }
}

ZH_FUNC( ZH_ARRAYTOPARAMS )
{
   

   PZH_ITEM pArray = zh_param( 1, ZH_IT_ARRAY );

   if( pArray )
      zh_arrayLast( pArray, zh_stackReturnItem() );
}

ZH_FUNC( ERRORLEVEL )
{
   

   zh_retni( s_nErrorLevel );

   if( zh_pcount() >= 1 )
      /* Only replace the error level if a parameter was passed */
      s_nErrorLevel = zh_parni( 1 );
}


/* NOTE: We should make sure that these get linked.
         Don't make this function static, because it's not called from
         this file. [vszakats] */

/* hernad
extern void zh_vmForceLink( void );
void zh_vmForceLink( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmForceLink()" ) );

   ZH_FUNC_EXEC( SYSINIT );
}
*/

/* NOTE: Pass string literals only. */
void zh_vmSetLinkedMain( const char * szMain )
{
   s_vm_pszLinkedMain = szMain;
}

void zh_vmSetDefaultGT( const char * szGtName )
{
   zh_gtSetDefault( szGtName );
}

/* Force linking default language and codepage modules */
ZH_CODEPAGE_REQUEST( ZH_CODEPAGE_DEFAULT )
ZH_LANG_REQUEST( ZH_LANG_DEFAULT )


