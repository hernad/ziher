#include <stdio.h>
#define SHARED_LIB

#include "zh_vm_pub.h"
#include "zh_trace.h"
//#include "zh_pcode.h"
#include "zh_init.h"
//#include "zh_xvm.h"

#if defined( _MSC_VER )
#include <windows.h>
#endif

#include <Python.h>

int c_consumer_py_callback(int cva, int cvb);

ZH_FUNC( PY_CALLBACK );

//static 
extern ZH_EXPORT PyObject *f18lib_set_callback(PyObject *self, PyObject *args);

void simple_printf(const char* fmt, ...);
