#include <stdio.h>
#include <time.h>
#include <windows.h>

//#include "examples/windows/dll/hello-library.h"
#define DLLEXPORT __declspec(dllexport)

DLLEXPORT char *get_time() {
  time_t ltime;
  time(&ltime);
  return ctime(&ltime);
}

DLLEXPORT void dll_say_hello(char *message) {
  printf("Hello from dll!\n%s", message);
}