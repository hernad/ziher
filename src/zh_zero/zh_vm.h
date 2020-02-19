/*
 * Header file for the Virtual Machine API
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

#ifndef ZH_VM_H_
#define ZH_VM_H_

#include "zh_api.h"

ZH_EXTERN_BEGIN

/* Ziher virtual machine init/exit functions */
extern ZH_EXPORT void     zh_vmInit( ZH_BOOL bStartMainProc );
extern ZH_EXPORT int      zh_vmQuit( void ); /* Immediately quits the virtual machine, return ERRORLEVEL code */

/* registration AtInit, AtExit and AtQuit functions.
 * AtInit functions are executed just before .zh INIT procedures.
 * AtExit functions are executed just after .zh EXIT procedures.
 * AtQuit functions are executed after deallocating all ZHVM items and
 * disabling .zh destructors. They can make final cleanup at C level
 * but should not reenter ZHVM.
 */
extern ZH_EXPORT void     zh_vmAtInit( ZH_INIT_FUNC pFunc, void * cargo );
extern ZH_EXPORT void     zh_vmAtExit( ZH_INIT_FUNC pFunc, void * cargo );
extern ZH_EXPORT void     zh_vmAtQuit( ZH_INIT_FUNC pFunc, void * cargo );

/* Ziher virtual machine functions */
extern ZH_EXPORT void     zh_vmExecute( const ZH_BYTE * pCode, PZH_SYMB pSymbols ) ZH_FLATTEN_ATTR;  /* invokes the virtual machine */
extern ZH_EXPORT PZH_SYMB zh_vmProcessSymbols( PZH_SYMB pSymbols, ZH_USHORT uiSymbols, const char * szModuleName, ZH_ULONG ulID, ZH_USHORT uiPcodeVer ); /* module symbols initialization with extended information */
extern ZH_EXPORT PZH_SYMB zh_vmProcessDynLibSymbols( PZH_SYMB pSymbols, ZH_USHORT uiSymbols, const char * szModuleName, ZH_ULONG ulID, ZH_USHORT uiPcodeVer ); /* module symbols initialization with extended information */


#ifdef _ZH_API_INTERNAL_
   typedef struct _ZH_SYMBOLS
   {
      PZH_SYMB  pModuleSymbols;     /* pointer to module symbol table */
      ZH_USHORT uiModuleSymbols;    /* number of symbols on that table */
      ZH_USHORT uiStaticsOffset;    /* offset of statics base symbol */
      struct _ZH_SYMBOLS * pNext;   /* pointer to the next SYMBOLS structure */
      ZH_SYMBOLSCOPE hScope;        /* scope collected from all symbols in module used to speed initialization code */
      void *    hDynLib;            /* handler to dynamic library */
      ZH_BOOL   fAllocated;         /* the symbol table is dynamically allocated and should be freed on ZHVM exit */
      ZH_BOOL   fActive;            /* the symbol table is currently active */
      ZH_BOOL   fInitStatics;       /* static initialization should be executed */
      char *    szModuleName;       /* module name */
      ZH_ULONG  ulID;               /* module unique identifier */
   } ZH_SYMBOLS, * PZH_SYMBOLS;     /* structure to keep track of all modules symbol tables */

   extern PZH_SYMBOLS   zh_vmRegisterSymbols( PZH_SYMB pModuleSymbols, ZH_USHORT uiSymbols, const char * szModuleName, ZH_ULONG ulID, ZH_BOOL fDynLib, ZH_BOOL fClone, ZH_BOOL fOverLoad );
   extern ZH_BOOL       zh_vmLockModuleSymbols( void );
   extern void          zh_vmUnlockModuleSymbols( void );
   extern void          zh_vmFreeSymbols( PZH_SYMBOLS pSymbols );
   extern void          zh_vmBeginSymbolGroup( void * hDynLib, ZH_BOOL fClone );
   extern void          zh_vmInitSymbolGroup( void * hNewDynLib, int argc, const char * argv[] );
   extern void          zh_vmExitSymbolGroup( void * hDynLib );
   extern PZH_SYMB      zh_vmFindFuncSym( const char * szFuncName, void * hDynLib );
   extern const char *  zh_vmFindModuleSymbolName( PZH_SYMB pSym );
   extern ZH_BOOL       zh_vmFindModuleSymbols( PZH_SYMB pSym, PZH_SYMB * pSymbols, ZH_USHORT * puiSymbols );
   extern PZH_SYMB      zh_vmGetRealFuncSym( PZH_SYMB pSym );
   extern void          zh_vmSetFunction( PZH_SYMB pOldSym, PZH_SYMB pNewSym );
   extern void          zh_vmSetDynFunc( PZH_DYNS pDynSym );

   extern void          zh_vmEnumRelease( PZH_ITEM pBase, PZH_ITEM pValue );
   extern ZH_BOOL       zh_vmMsgReference( PZH_ITEM pObject, PZH_DYNS pMessage, PZH_DYNS pAccMsg ); /* create extended message reference */

   extern void          zh_vmUpdateAllocator( PZH_ALLOCUPDT_FUNC pFunc, int iCount );

   extern void          zh_vmEval( ZH_USHORT uiParams );
#endif

extern void zh_vmSetExceptionHandler( void );
extern void zh_vmUnsetExceptionHandler( void );

extern ZH_EXPORT void     zh_vmSymbolInit_RT( void );   /* initialization of runtime support symbols */

/* Ziher virtual machine escaping API */
extern ZH_EXPORT void      zh_vmRequestDebug( void );
extern ZH_EXPORT void      zh_vmRequestBreak( PZH_ITEM pItem );
extern ZH_EXPORT void      zh_vmRequestCancel( void );
extern ZH_EXPORT void      zh_vmRequestQuit( void );
extern ZH_EXPORT void      zh_vmRequestEndProc( void );
extern ZH_EXPORT ZH_USHORT zh_vmRequestQuery( void );
extern ZH_EXPORT ZH_BOOL   zh_vmRequestReenter( void );
extern ZH_EXPORT void      zh_vmRequestRestore( void );
extern ZH_EXPORT ZH_BOOL   zh_vmRequestReenterExt( void );
extern ZH_EXPORT ZH_BOOL   zh_vmTryEval( PZH_ITEM * pResult, PZH_ITEM pItem, ZH_ULONG ulPCount, ... );

extern ZH_EXPORT ZH_BOOL   zh_vmIsActive( void );
extern ZH_EXPORT ZH_BOOL   zh_vmIsReady( void );

/* Return values of zh_vmRequestQuery() */
#define ZH_QUIT_REQUESTED     1     /* immediately quit the application */
#define ZH_BREAK_REQUESTED    2     /* break to nearest RECOVER/END sequence */
#define ZH_ENDPROC_REQUESTED  4     /* immediately return from procedure (error handler in macro evaluation) */
#ifdef _ZH_API_INTERNAL_
#define ZH_VMSTACK_REQUESTED  0x100 /* internal flag to signal thread local stack */
#endif

/* Public PCode functions */

/* Execution */
extern ZH_EXPORT void     zh_vmDo( ZH_USHORT uiParams );      /* invoke the virtual machine */
extern ZH_EXPORT void     zh_vmProc( ZH_USHORT uiParams );     /* executes a function or procedure */
extern ZH_EXPORT void     zh_vmFunction( ZH_USHORT uiParams ); /* executes a function */
extern ZH_EXPORT void     zh_vmSend( ZH_USHORT uiParams ); /* sends a message to an object */
extern ZH_EXPORT PZH_ITEM zh_vmEvalBlock( PZH_ITEM pBlockItem ); /* executes passed codeblock with no arguments */
/* executes passed codeblock with variable number of arguments */
extern ZH_EXPORT PZH_ITEM zh_vmEvalBlockV( PZH_ITEM pBlockItem, ZH_ULONG ulArgCount, ... );
extern ZH_EXPORT PZH_ITEM zh_vmEvalBlockOrMacro( PZH_ITEM pItem ); /* executes codeblock or macro pointed by given item */
extern ZH_EXPORT void     zh_vmDestroyBlockOrMacro( PZH_ITEM pItem ); /* destroy codeblock or macro in given item */

/* Push */
extern ZH_EXPORT void     zh_vmPush( PZH_ITEM pItem );     /* pushes a generic item onto the stack */
extern ZH_EXPORT void     zh_vmPushNil( void );            /* in this case it places nil at self */
extern ZH_EXPORT void     zh_vmPushNumber( double dNumber, int iDec ); /* pushes a number on to the stack and decides if it is integer, long or double */
extern ZH_EXPORT void     zh_vmPushInteger( int iNumber ); /* pushes a integer number onto the stack */
extern ZH_EXPORT void     zh_vmPushLong( long lNumber ); /* pushes a long number onto the stack */
extern ZH_EXPORT void     zh_vmPushDouble( double dNumber, int iDec ); /* pushes a double number onto the stack */
extern ZH_EXPORT void     zh_vmPushSize( ZH_ISIZ nNumber ); /* pushes a ZH_SIZE number onto the stack */
extern ZH_EXPORT void     zh_vmPushNumInt( ZH_MAXINT nNumber );  /* pushes a number on to the stack and decides if it is integer or ZH_MAXINT */
extern ZH_EXPORT void     zh_vmPushLogical( ZH_BOOL bValue );    /* pushes a logical value onto the stack */
extern ZH_EXPORT void     zh_vmPushString( const char * szText, ZH_SIZE length );  /* pushes a string on to the stack */
extern ZH_EXPORT void     zh_vmPushStringPcode( const char * szText, ZH_SIZE length );  /* pushes a string from pcode on to the stack */
extern ZH_EXPORT void     zh_vmPushDate( long lDate );   /* pushes a long date onto the stack */
extern ZH_EXPORT void     zh_vmPushTimeStamp( long lJulian, long lMilliSec ); /* pushes two long value as timestamp onto the stack */
extern ZH_EXPORT void     zh_vmPushSymbol( PZH_SYMB pSym ); /* pushes a function pointer onto the stack */
extern ZH_EXPORT void     zh_vmPushDynSym( PZH_DYNS pDynSym ); /* pushes a function/method pointer onto the stack */
extern ZH_EXPORT void     zh_vmPushEvalSym( void ); /* pushes a codeblock eval symbol onto the stack */
extern ZH_EXPORT void     zh_vmPushPointer( void * pPointer ); /* push an item of ZH_IT_POINTER type */
extern ZH_EXPORT void     zh_vmPushPointerGC( void * pPointer ); /* push an item of GC ZH_IT_POINTER type */
extern ZH_EXPORT void     zh_vmPushItemRef( PZH_ITEM pItem ); /* push item reference */

extern ZH_EXPORT ZH_BOOL  zh_vmIsMt( void ); /* return ZH_TRUE if ZHVM is compiled with thread support */
extern ZH_EXPORT void     zh_vmLock( void ); /* lock VM blocking GC execution by other threads */
extern ZH_EXPORT void     zh_vmLockForce( void ); /* lock VM blocking GC execution by other threads, ignore GC request */
extern ZH_EXPORT void     zh_vmUnlock( void ); /* unlock VM, allow GC execution */
#ifdef _ZH_API_INTERNAL_
extern ZH_EXPORT ZH_BOOL  zh_vmSuspendThreads( ZH_BOOL fWait ); /* (try to) stop all threads except current one */
extern ZH_EXPORT void     zh_vmResumeThreads( void ); /* unblock execution of threads stopped by zh_vmSuspendThreads() */
#endif
extern ZH_EXPORT ZH_BOOL  zh_vmThreadRegister( void * ); /* Register new thread without local thread ZHVM stack */
extern ZH_EXPORT void     zh_vmThreadRelease( void * ); /* Remove registered thread which does not have local thread ZHVM stack yet */
extern ZH_EXPORT void     zh_vmThreadInit( void * ); /* allocate local thread ZHVM stack */
extern ZH_EXPORT void     zh_vmThreadQuit( void ); /* destroy local thread ZHVM stack */
extern ZH_EXPORT void     zh_vmThreadQuitRequest( void * ); /* send QUIT request to given thread */
extern ZH_EXPORT void     zh_vmWaitForThreads( void ); /* wait for all threads to terminate can be called only by main ZHVM thread */
extern ZH_EXPORT void     zh_vmTerminateThreads( void ); /* send QUIT request to all threads except current one and wait for their termination, should be called only by main ZHVM thread */
extern ZH_EXPORT ZH_BOOL  zh_vmThreadIsMain( void * ); /* check if given or current thread is main ZHVM thread */
extern ZH_EXPORT PZH_ITEM zh_vmThreadStart( ZH_ULONG ulAttr, PZH_CARGO_FUNC pThreadFunc, void * cargo ); /* create new thread with ZHVM stack */
extern ZH_EXPORT void *   zh_vmThreadState( void );

ZH_EXTERN_END

#endif /* ZH_VM_H_ */
