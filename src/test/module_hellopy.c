#include <Python.h>
#include <stdio.h>

#if defined( _MSC_VER )
#define EXTERNAL extern __declspec( dllexport )
#else
#define EXTERNAL extern
#endif

EXTERNAL int hello_py_ext_109() {
    return 109;
}

static PyObject*
hello_sayHello(PyObject *self, PyObject *args) {

    printf("Hello World using printf");

    /* We wish for our function to return None, the Python
    equivalent of returning void. We have to do the following
    to return None. */

    Py_INCREF(Py_None);
    return Py_None; 
}

static PyMethodDef HelloMethods[] = {
    {"sayHello", hello_sayHello, METH_VARARGS, "Print Hello World"},
    {NULL, NULL, 0, NULL} /* The sentinel value. */
};

static struct PyModuleDef hellomodule = {

    PyModuleDef_HEAD_INIT,
    "hellopy",
    NULL, /*This is for documentation, which we won't use; so it is NULL. */
    -1,
    HelloMethods
};

PyMODINIT_FUNC
PyInit_hellopy(void) {
    return PyModule_Create(&hellomodule);
}
