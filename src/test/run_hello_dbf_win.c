#include <stdio.h>

#include "zh_vm_pub.h"
#include "zh_trace.h"
//#include "zh_pcode.h"
#include "zh_init.h"
//#include "zh_xvm.h"

#include <windows.h>

ZH_FUNC_EXTERN( MAIN );
ZH_FUNC_EXTERN( QOUT );
ZH_FUNC_EXTERN( DBCREATE );
ZH_FUNC_EXTERN( DBUSEAREA );
ZH_FUNC_EXTERN( DBAPPEND );
ZH_FUNC_EXTERN( PADL );
ZH_FUNC_EXTERN( ALLTRIM );
ZH_FUNC_EXTERN( STR );
ZH_FUNC_EXTERN( ORDCONDSET );
ZH_FUNC_EXTERN( RECNO );
ZH_FUNC_EXTERN( ORDCREATE );
ZH_FUNC_EXTERN( DBCLOSEAREA );
ZH_FUNC_EXTERN( BREAK );
ZH_FUNC_EXTERN( DBGOTOP );
ZH_FUNC_EXTERN( EOF );
ZH_FUNC_EXTERN( TRIM );
ZH_FUNC_EXTERN( DBSKIP );
ZH_FUNC_EXTERN( ALERT );
ZH_FUNC_EXTERN( VALTOSTR );
ZH_FUNC_EXTERN( RECCOUNT );
ZH_FUNC_EXTERN( __QUIT );

ZH_INIT_SYMBOLS_BEGIN( zh_vm_SymbolInit_HELLO_DBF_ZH )
{ "MAIN", { ZH_FS_PUBLIC | ZH_FS_FIRST | ZH_FS_LOCAL }, { ZH_FUNCNAME( MAIN ) }, NULL },
{ "QOUT", { ZH_FS_PUBLIC }, { ZH_FUNCNAME( QOUT ) }, NULL },
{ "DBCREATE", { ZH_FS_PUBLIC }, { ZH_FUNCNAME( DBCREATE ) }, NULL },
{ "DBUSEAREA", { ZH_FS_PUBLIC }, { ZH_FUNCNAME( DBUSEAREA ) }, NULL },
{ "DBAPPEND", { ZH_FS_PUBLIC }, { ZH_FUNCNAME( DBAPPEND ) }, NULL },
{ "PADL", { ZH_FS_PUBLIC }, { ZH_FUNCNAME( PADL ) }, NULL },
{ "ALLTRIM", { ZH_FS_PUBLIC }, { ZH_FUNCNAME( ALLTRIM ) }, NULL },
{ "STR", { ZH_FS_PUBLIC }, { ZH_FUNCNAME( STR ) }, NULL },
{ "ID", { ZH_FS_PUBLIC | ZH_FS_MEMVAR }, { NULL }, NULL },
{ "DESC", { ZH_FS_PUBLIC | ZH_FS_MEMVAR }, { NULL }, NULL },
{ "VALUE", { ZH_FS_PUBLIC | ZH_FS_MEMVAR }, { NULL }, NULL },
{ "ORDCONDSET", { ZH_FS_PUBLIC }, { ZH_FUNCNAME( ORDCONDSET ) }, NULL },
{ "RECNO", { ZH_FS_PUBLIC }, { ZH_FUNCNAME( RECNO ) }, NULL },
{ "ORDCREATE", { ZH_FS_PUBLIC }, { ZH_FUNCNAME( ORDCREATE ) }, NULL },
{ "DBCLOSEAREA", { ZH_FS_PUBLIC }, { ZH_FUNCNAME( DBCLOSEAREA ) }, NULL },
{ "BREAK", { ZH_FS_PUBLIC }, { ZH_FUNCNAME( BREAK ) }, NULL },
{ "DBGOTOP", { ZH_FS_PUBLIC }, { ZH_FUNCNAME( DBGOTOP ) }, NULL },
{ "EOF", { ZH_FS_PUBLIC }, { ZH_FUNCNAME( EOF ) }, NULL },
{ "TEST_DBF", { ZH_FS_PUBLIC }, { NULL }, NULL },
{ "TRIM", { ZH_FS_PUBLIC }, { ZH_FUNCNAME( TRIM ) }, NULL },
{ "DBSKIP", { ZH_FS_PUBLIC }, { ZH_FUNCNAME( DBSKIP ) }, NULL },
{ "ALERT", { ZH_FS_PUBLIC }, { ZH_FUNCNAME( ALERT ) }, NULL },
{ "VALTOSTR", { ZH_FS_PUBLIC }, { ZH_FUNCNAME( VALTOSTR ) }, NULL },
{ "RECCOUNT", { ZH_FS_PUBLIC }, { ZH_FUNCNAME( RECCOUNT ) }, NULL },
{ "__QUIT", { ZH_FS_PUBLIC }, { ZH_FUNCNAME( __QUIT ) }, NULL }
ZH_INIT_SYMBOLS_EX_END( zh_vm_SymbolInit_HELLO_DBF_ZH, "", 0x0, 0x0003 )

//#define DLLIMPORT __declspec(dllimport)


//extern "C" DLLEXPORT void say_hello(char *);
//DLLIMPORT void dll_say_hello(char *message);

//ZH_IMPORT ZH_FUNC(MAIN);
// typedef int                 ZH_BOOL;
// #define ZH_TRUE  ( ! 0 )

//extern DLLIMPORT 
//void     zh_vmInit( int bStartMainProc );
//int      zh_vmQuit( void ); /* Immediately quits the virtual machine, return ERRORLEVEL code */
//void     zh_cmdargInit( int argc, char * argv[] ); /* initialize command-line argument API's */

int main( int argc, char * argv[] )
{

   dll_say_hello("Hello dll", "22");

/*
C:\dev\ziher\src>dumpbin /imports bazel-bin\test\run_hello_dbf_win.exe

Dump of file bazel-bin\test\run_hello_dbf_win.exe

File Type: EXECUTABLE IMAGE

  Section contains the following imports:

    dll_hello_dbf.dll
             14025BBA0 Import Address Table
             1402F1768 Import Name Table
                     0 time date stamp
                     0 Index of first forwarder reference

                         1EA dll_say_hello
*/

   puts("************************** run_hello_dbf win *****************************");
   ZH_TRACE( ZH_TR_INFO, ( "main(%d, %p)", argc, ( void * ) argv ) );
   
   zh_cmdargInit( argc, argv );
   puts("-- 2 --");
   zh_vmInit( 1 );
   puts("");
   puts("************************ end run_hello_dbf win ****************************");

   return zh_vmQuit();
}
