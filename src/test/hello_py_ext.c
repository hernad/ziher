#include <stdio.h>

#include "zh_vm_pub.h"
#include "zh_trace.h"
#include "zh_init.h"

#if defined(_MSC_VER)
#include <windows.h>
#endif

#if defined( _MSC_VER )
#define EXTERNAL extern __declspec( dllexport )
#else
#define EXTERNAL extern
#endif


#include "zh_api.h"
#include "zh_item_api.h"
#include <Python.h>
#include "zh_python.h"

ZH_FUNC_EXTERN( MAIN );
ZH_FUNC_EXTERN( MAIN0 );

ZH_INIT_SYMBOLS_BEGIN( zh_vm_SymbolInit_F18_EXT_ZH )
{ "MAIN", { ZH_FS_PUBLIC | ZH_FS_FIRST | ZH_FS_LOCAL }, { ZH_FUNCNAME( MAIN ) }, NULL },
{ "MAIN0", { ZH_FS_PUBLIC | ZH_FS_FIRST | ZH_FS_LOCAL }, { ZH_FUNCNAME( MAIN0 ) }, NULL }
ZH_INIT_SYMBOLS_EX_END( zh_vm_SymbolInit_F18_EXT_ZH, "F18_EXT", 0x0, 0x0003 )


EXTERNAL int hello_py_ext_101() {
    return 101+1;
}

static PyObject *
f18_system(PyObject *self, PyObject *args)
{
    const char *command;
    int sts;

    if (!PyArg_ParseTuple(args, "s", &command))
        return NULL;
    sts = system(command);
    return PyLong_FromLong(sts);
}

static PyObject *
f18_f18(PyObject *self, PyObject *args)
{
   const char *command;
   //char *argv[1];
   char* f18_args[] = { "--help", NULL };
    //int sts;

   if (!PyArg_ParseTuple(args, "s", &command))
        return NULL;
   //sts = system(command);
   //argv[0] = command;
   
   ZH_TRACE( ZH_TR_INFO, ( "main(%d, %p)", 1, ( void * ) f18_args ) );

   printf("dynsymcount: %d\n", zh_dynsymCount());

   zh_cmdargInit( 1, f18_args );

   zh_vmInit( ZH_TRUE, ZH_TRUE );
   zh_vmQuit( ZH_FALSE );
   return PyLong_FromLong(0);
}


static PyObject *
f18_vminit(PyObject *self, PyObject *args)
{

   zh_vmInit( ZH_FALSE, ZH_TRUE );

   zh_conRelease();

   return PyLong_FromLong(0);
}

static PyObject *
ziher_conInit(PyObject *self, PyObject *args)
{

   zh_conInit();

   return PyLong_FromLong(0);
}

static PyObject *
ziher_conRelease(PyObject *self, PyObject *args)
{

   zh_conRelease();
   return PyLong_FromLong(0);
}


static PyObject *
f18_run(PyObject *self, PyObject *args)
{

   char *sFunc;
   int initConsole = 0;
   int releaseConsole = 0;

   // arg 1: ime funkcije, arg 2: 0/1-initconsole, arg 3: 0/1-releaseconsole"
   if (!PyArg_ParseTuple(args, "s|ii", &sFunc, &initConsole, &releaseConsole))
        return NULL;

   PZH_SYMBOL pDynSym =  zh_dynsymFind( sFunc );

   if (initConsole)
      zh_conInit();
      

   if( pDynSym )
   {
      zh_vmPushDynSym( pDynSym );
      zh_vmPushNil();
      zh_vmProc( 0 );
   } else {
       printf("nema ziher func %s\n", sFunc);
   }

   if (releaseConsole)
      zh_conRelease();

   return PyLong_FromLong(0);
}


static PyObject *
f18_run_get(PyObject *self, PyObject *args)
{

   char *sFunc;
   int initConsole = 0;
   int releaseConsole = 0;
   const char *ret;
   PyObject *pyValue;

   // https://stackoverflow.com/questions/10625865/how-does-pyarg-parsetupleandkeywords-work


   // arg 1: ime funkcije, arg 2: 0/1-initconsole, arg 3: 0/1-releaseconsole"
   if (!PyArg_ParseTuple(args, "s|ii", &sFunc, &initConsole, &releaseConsole))
        return NULL;

   PZH_SYMBOL pDynSym =  zh_dynsymFind( sFunc );

   if (initConsole)
      zh_conInit();
      
   if( pDynSym )
   {
      zh_vmPushDynSym( pDynSym );
      zh_vmPushNil();
      // vmdo je funkcija
      zh_vmDo( 0 );

      //PZH_ITEM pResult = zh_itemNew( zh_stackReturnItem() );
      //PZH_ITEM pResult = zh_stackReturnItem();
      const char * ret = zh_parc( -1 );
      //printf("return: %s\n", ret);
      pyValue = Py_BuildValue("s", ret);
      
   
   } else {
       printf("nema ziher func %s\n", sFunc);
   }

   //#define zh_retc( szText )                    zh_itemPutC( zh_stackReturnItem(), szText )

   

/*
   if( pRetVal && ZH_IS_STRING( pRetVal ) ) {
       ret = zh_itemGetCPtr(pRetVal);
       pyValue = PyUnicode_FromString(ret);
   } else {
       pyValue = PyUnicode_FromString("");
   }

*/

   if (releaseConsole)
      zh_conRelease();

   //return PyUnicode_FromString(ret);
   return pyValue;
}


static PyObject *
f18_put_get(PyObject *self, PyObject *args)
{

   char *sFunc, *sParam;
   int initConsole = 0;
   int releaseConsole = 0;
   const char *ret;
   PyObject *pyValue;

   if (!PyArg_ParseTuple(args, "ss|ii", &sFunc, &sParam, &initConsole, &releaseConsole))
        return NULL;

   PZH_SYMBOL pDynSym = zh_dynsymFind( sFunc );

   if (initConsole)
      zh_conInit();
      
   if( pDynSym )
   {
      zh_vmPushDynSym( pDynSym );
      zh_vmPushNil(); //pSelf = zh_stackSelfItem();   /* NIL, OBJECT or BLOCK */

      //printf("sParam='%s'\n", sParam);
     
      zh_vmPushString(sParam, strlen( sParam ));
      //zh_vmPushLogical();
      //zh_vmPushInteger();
      //zh_vmPushItemRef();

      puts("step 4x");
      zh_vmDo( 1 ); // 1 param
      //zh_vmProc(1);
      puts("step 5x");

      const char * ret = zh_parc( -1 );
      printf("return: %s\n", ret);
      pyValue = Py_BuildValue("s", ret);
      
   
   } else {
       printf("nema ziher func %s\n", sFunc);
   }


   if (releaseConsole)
      zh_conRelease();

   //return PyUnicode_FromString(ret);
   return pyValue;
}

// >>> import f18klijentlib; f18klijentlib.vminit()
// >>> f18klijentlib.hash("PP_2",1,1)
// init console
//    
// (hash): key1 / value1 ; release console                                                                                                       
// 
// key=1 value=OK
// key=2 value=OK2
// {'1': 'OK', '2': 'OK2'}

static PyObject *
f18_hash(PyObject *self, PyObject *args) {

   char *sFunc;
   int initConsole = 0;
   int releaseConsole = 0;
   PyObject *pyValue = NULL;
   char *cKey1 = "key1";
   char *cValue1 = "value 1";

   // arg 1: ime funkcije, arg 2: 0/1-initconsole, arg 3: 0/1-releaseconsole"
   if (!PyArg_ParseTuple(args, "s|ii", &sFunc, &initConsole, &releaseConsole))
        return NULL;

   if (initConsole) {
      puts("init console");
      zh_conInit();
   }

   PZH_SYMBOL pDynSym = zh_dynsymFind( sFunc );
      
   if( pDynSym )
   {
      zh_vmPushDynSym( pDynSym );
      zh_vmPushNil(); //pSelf = zh_stackSelfItem();   /* NIL, OBJECT or BLOCK */
   
     PZH_ITEM pHash = zh_hashNew( NULL );

     //for( iParam = 1; iParam <= iPCount; iParam += 2 )
     PZH_ITEM pKey = zh_itemNew( NULL );
     PZH_ITEM pValue = zh_itemNew( NULL );
     //zh_vmPushString( cKey1, strlen( cKey1 ));
     //PZH_ITEM pValue = zh_vmPushString( cValue1, strlen( cValue1 ));
     zh_itemPutC( pKey, "key1" );
     zh_itemPutC( pValue, "value1");
     zh_hashAdd( pHash, pKey, pValue );
 
     //zh_itemRelease( pKey );
     //zh_itemRelease( pValue );
 
     zh_vmPush(pHash);
     zh_vmDo( 1 );

     if (releaseConsole) {
      puts("release console");
      zh_conRelease();
     }

     PZH_ITEM pHashRet = zh_param( -1, ZH_IT_HASH);
     int len = zh_hashLen( pHashRet );
     // https://stackoverflow.com/questions/51632300/python-c-api-problems-trying-to-build-a-dictionary
     
     pyValue = PyDict_New();
     for( int i = 1; i <= len; i++) {
        PZH_ITEM pKey = zh_hashGetKeyAt( pHashRet, i );
        PZH_ITEM pValue = zh_hashGetValueAt( pHashRet, i );
        char *cKey = "00", *cValue = "?";
        if( ( zh_itemType( pKey ) & ZH_IT_STRING ) && ( zh_itemType( pValue ) & ZH_IT_STRING ) ) {
           //const char * szText = zh_itemGetCPtr( pText );
           //int iWidth, iDec, iLen = ( int ) zh_itemGetCLen( pText );
           cKey = zh_itemGetCPtr( pKey );
           cValue = zh_itemGetCPtr( pValue);
           printf("key=%s value=%s\n", cKey, cValue);
        }
        //Py_BuildValue("{s:i,s:O}",
        //   cKey, cValue);
        PyDict_SetItemString(pyValue, cKey, Py_BuildValue("s", cValue));
     }

    }

    
    return pyValue;
}

// https://www.oreilly.com/library/view/python-cookbook/0596001673/ch16s07.html

// python side

// >>> def callback_2(a,b):
// ...     return (a+b)*10
// >>> f18klijentlib.set_callback(callback)
// >>> f18klijentlib.hash("PP_2",1,1)
// init console
//    
// py_callback(10,10):        200                                                                                                                
// (hash): key1 / value1 ; release console
// 
// key=1 value=OK
// key=2 value=OK2
// {'1': 'OK', '2': 'OK2'}
// >>> def callback_2(a,b):
// ...     return (a+b)*5
// ... 
// >>> f18klijentlib.set_callback(callback_2)
// >>> f18klijentlib.hash("PP_2",1,1)
// init console
//    
// py_callback(10,10):        100                                                                                                                
// (hash): key1 / value1 ; release console
// 
// key=1 value=OK
// key=2 value=OK2
// {'1': 'OK', '2': 'OK2'}

// ziher strana

//FUNCTION pp_2( x )
//
//  ...
//   ? "py_callback(10,10):", py_callback(10,10)
//
//   ? _tmp
//   RETURN hRet

//static PyObject *
//f18lib_set_callback(PyObject *self, PyObject *args)
//{
//
//  
//  PyObject *pyCallback;
//  //PyObject *list;
//  if (!PyArg_ParseTuple(args, "O", &pyCallback))
//       return NULL;
//
//   if (!PyCallable_Check(pyCallback)) {
//        PyErr_SetString(PyExc_TypeError, "Need a callable object!");
//    } else {
//        // Save the compare function. This obviously won't work for multithreaded
//        // programs and is not even a reentrant, alas -- qsort's fault!
//        s_py_callback_func = pyCallback;
//        /*
//        if (PyList_Check(list)) {
//            int size = PyList_Size(list);
//            int i;
//
//            // Make an array of (PyObject *), because qsort does not know about
//            // the PyList object
//            PyObject **v = (PyObject **) malloc( sizeof(PyObject *) * size );
//            for (i=0; i<size; ++i) {
//                v[i] = PyList_GetItem(list, i);
//                // Increment the reference count, because setting the list 
//                // items below will decrement the reference count
//                Py_INCREF(v[i]);
//            }
//            c_consumer_callback(v, size, sizeof(PyObject*), pyCallback);
//            for (i=0; i<size; ++i) {
//                PyList_SetItem(list, i, v[i]);
//                // need not do Py_DECREF - see above
//            }
//            free(v);
//        }
//        */
//    }
//    Py_INCREF(Py_None);
//    return Py_None;
//}

static PyMethodDef F18ZiherPyMethods[] = {
    {"system",  f18_system, METH_VARARGS, "Execute a shell command."},
    {"f18",  f18_f18, METH_VARARGS, "Execute f18 command."},
    {"vminit",  f18_vminit, METH_VARARGS, "Execute f18 command /1."},
    {"con_init",  ziher_conInit, METH_VARARGS, "ziher initialize console"},
    {"con_release",  ziher_conRelease, METH_VARARGS, "ziher release console"},
    {"run",  f18_run, METH_VARARGS, "run ziher func"},
    {"run_get",  f18_run_get, METH_VARARGS, "run ziher func"},
    {"put_get",  f18_put_get, METH_VARARGS, "run ziher func"},
    {"hash",  f18_hash, METH_VARARGS, "in/out hash"},
    {"set_callback",  ziher_set_py_callback, METH_VARARGS, "set callback function"},
    {NULL, NULL, 0, NULL}
};

static struct PyModuleDef F18ZiherPyModule = {
    PyModuleDef_HEAD_INIT,
    "f18ziherpy",   /* name of module */
    NULL, /* module documentation, may be NULL */
    -1,       /* size of per-interpreter state of the module,
                 or -1 if the module keeps state in global variables. */
    F18ZiherPyMethods
};


PyMODINIT_FUNC
PyInit_f18ziherpy(void)
{
    
    // zh_itemDoC( "init_ziher_py", 0, 0);
    return PyModule_Create(&F18ZiherPyModule);
}
