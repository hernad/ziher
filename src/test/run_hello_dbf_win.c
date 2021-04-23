#include <stdio.h>

#include "zh_vm_pub.h"
#include "zh_pcode.h"
#include "zh_init.h"
#include "zh_xvm.h"

#include <windows.h>
#ifndef HELLO_LIBRARY_H
#define HELLO_LIBRARY_H

#ifdef COMPILING_DLL
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT __declspec(dllimport)
#endif

//extern "C" DLLEXPORT void say_hello(char *);
DLLEXPORT void dll_say_hello(char *message);

#endif
int main( int argc, char * argv[] )
{

   dll_say_hello("Hello dll");

   puts("************************** run_hello_dbf *****************************");
   //ZH_TRACE( ZH_TR_DEBUG, ( "main(%d, %p)", argc, ( void * ) argv ) );
   
   zh_cmdargInit( argc, argv );
   puts("-- 2 --");
   zh_vmInit( ZH_TRUE );
   puts("");
   puts("************************ end run_hello_dbf ****************************");

   return zh_vmQuit();
}
