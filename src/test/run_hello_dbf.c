#include <stdio.h>

#include "zh_vm_pub.h"
#include "zh_pcode.h"
#include "zh_init.h"
#include "zh_xvm.h"


int main( int argc, char * argv[] )
{

   puts("************************** run_hello_dbf *****************************");
   //ZH_TRACE( ZH_TR_DEBUG, ( "main(%d, %p)", argc, ( void * ) argv ) );
   
   zh_cmdargInit( argc, argv );
   puts("-- 2 --");
   zh_vmInit( ZH_TRUE );
   puts("");
   puts("************************ end run_hello_dbf ****************************");

   return zh_vmQuit();
}
