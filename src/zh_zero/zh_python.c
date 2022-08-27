#include "zh_python.h"

static PyObject *s_py_callback_func = NULL;

// ziher: py_callback(2, 3 ) => python => ret number
ZH_FUNC( PY_CALLBACK )
{
   //char *       pszFree;
   //const char * pszFileName = zh_fsNameConv( zh_parcx( 2 ), &pszFree );

   int zhArg1 = zh_parni( 1 ); /* retrieve a numeric parameter as a integer */
   int zhArg2 = zh_parni( 2 );

   // zh_retni - integer to ziher numbeer
   zh_retni( c_consumer_py_callback( zhArg1, zhArg2 ) );

   //if( pszFree )
   //   zh_xfree( pszFree );
}

int c_consumer_py_callback(int cva, int cvb) {

   int retvalue = 0;
   const PyObject *pya = Py_BuildValue("i", cva);
   const PyObject *pyb = Py_BuildValue("i", cvb);

    // Build up the argument list tuple
    PyObject *arglist = Py_BuildValue("(OO)", pya, pyb);

    // ...for calling the Python function
    PyObject *result = PyEval_CallObject(s_py_callback_func, arglist);

    if (result && PyLong_Check(result)) {
        retvalue = PyLong_AsLong(result);
    }

    Py_XDECREF(result);
    Py_DECREF(arglist);

    return retvalue;
}

//static 
PyObject *
f18lib_set_callback(PyObject *self, PyObject *args)
{

  
  PyObject *pyCallback;
  //PyObject *list;
  if (!PyArg_ParseTuple(args, "O", &pyCallback))
       return NULL;

   if (!PyCallable_Check(pyCallback)) {
        PyErr_SetString(PyExc_TypeError, "Need a callable object!");
    } else {
        // Save the compare function. This obviously won't work for multithreaded
        // programs and is not even a reentrant, alas -- qsort's fault!
        s_py_callback_func = pyCallback;
        /*
        if (PyList_Check(list)) {
            int size = PyList_Size(list);
            int i;

            // Make an array of (PyObject *), because qsort does not know about
            // the PyList object
            PyObject **v = (PyObject **) malloc( sizeof(PyObject *) * size );
            for (i=0; i<size; ++i) {
                v[i] = PyList_GetItem(list, i);
                // Increment the reference count, because setting the list 
                // items below will decrement the reference count
                Py_INCREF(v[i]);
            }
            c_consumer_callback(v, size, sizeof(PyObject*), pyCallback);
            for (i=0; i<size; ++i) {
                PyList_SetItem(list, i, v[i]);
                // need not do Py_DECREF - see above
            }
            free(v);
        }
        */
    }
    Py_INCREF(Py_None);
    return Py_None;
}


void simple_printf(const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
 
    while (*fmt != '\0') {
        if (*fmt == 'd') {
            int i = va_arg(args, int);
            printf("%d\n", i);
        } else if (*fmt == 'c') {
            // A 'char' variable will be promoted to 'int'
            // A character literal in C is already 'int' by itself
            int c = va_arg(args, int);
            printf("%c\n", c);
        } else if (*fmt == 'f') {
            double d = va_arg(args, double);
            printf("%f\n", d);
        }
        ++fmt;
    }
 
    va_end(args);
}
