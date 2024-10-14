#!/usr/bin/env python

import sys
import os

from textual_splash import run as textual_run

# arg 1: ime funkcije, arg 2: 0/1-initconsole, arg 3: 0/1-releaseconsole"
arg1 = sys.argv[1]
arg2 = int(sys.argv[2])
arg3 = int(sys.argv[3])

if arg1 == "textual":
   textual_run(header=None)

elif arg1 == "func_hello_ziher":
   ziher_home = os.getenv("ZIHER_HOME")
   print("ziher_home:", ziher_home)
   sys.path.append(ziher_home + "/lib")
   import f18klijentlib
   f18klijentlib.vminit()
   ziher_header = f18klijentlib.run_get(arg1.upper(), 0, 1)
   textual_run(header=ziher_header)

elif arg1 == "func_hello_ziher_2":
   ziher_home = os.getenv("ZIHER_HOME")
   print("ziher_home:", ziher_home)
   sys.path.append(ziher_home + "/lib")
   import f18klijentlib
   f18klijentlib.vminit()
   ziher_header = f18klijentlib.run_get("func_hello_ziher".upper(), 0, 1)
   textual_run(header=ziher_header)
   ziher_header = f18klijentlib.run_get("func_hello_ziher_2".upper(), 0, 1)
   textual_run(header=ziher_header)
   f18klijentlib.run("NASLOVNI_EKRAN_SPLASH_SCREEN", 1, 1)
   f18klijentlib.run("MAIN", 1, 1)
   ziher_header = f18klijentlib.run_get("func_hello_ziher_2".upper(), 0, 1)
   textual_run(header=ziher_header)
   ziher_header = f18klijentlib.run_get("func_hello_ziher".upper(), 0, 1)
   textual_run(header=ziher_header)
else:

   ziher_home = os.getenv("ZIHER_HOME")
   print("ziher_home:", ziher_home)
   sys.path.append(ziher_home + "/lib")
   import f18klijentlib
   f18klijentlib.vminit()
   f18klijentlib.run(arg1, arg2, arg3)


