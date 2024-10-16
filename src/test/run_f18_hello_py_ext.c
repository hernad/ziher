#include <stdio.h>

#include "zh_vm_pub.h"
#include "zh_trace.h"
#include "zh_init.h"

#if defined( _MSC_VER )
#include <windows.h>
#endif

ZH_FUNC_EXTERN( MAIN );

ZH_INIT_SYMBOLS_BEGIN( zh_vm_SymbolInit_HELLO_PY_EXT_ZH )
{ "MAIN", { ZH_FS_PUBLIC | ZH_FS_FIRST | ZH_FS_LOCAL }, { ZH_FUNCNAME( MAIN ) }, NULL }
ZH_INIT_SYMBOLS_EX_END( zh_vm_SymbolInit_HELLO_PY_EXT_ZH, "", 0x0, 0x0003 )

extern int hello_py_ext_101();

int main( int argc, char * argv[] )
{

   printf("hello_py_ext_101: %d\n", hello_py_ext_101());

   puts("************************** run_hello_py_ext win *****************************");
   ZH_TRACE( ZH_TR_INFO, ( "main(%d, %p)", argc, ( void * ) argv ) );
   
   zh_cmdargInit( argc, argv );
   puts("-- 2 --");
   zh_vmInit( 1, 1, 1 );
   puts("");
   puts("************************ end run_hello_py_ext win ****************************");

   return zh_vmQuit( 1 );
}
