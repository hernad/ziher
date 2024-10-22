#!/usr/bin/env python

import sys
import os
import gc
from importlib import reload

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

   bStartMainProc = False
   bInitRT = True
   bInitConsole = True
   bReleaseConsole = True

   #f18klijentlib.vminit(bStartMainProc, bInitRT, bInitConsole)
   #
   #bInitConsole = 1
   #bReleaseConsole = 1
   #returnType = 1 # integer
   #func = "ZH_GTCOUNT"
   #print("zh_gtcount:", f18klijentlib.run_get(func, bInitConsole, bReleaseConsole, returnType))
   #s = input()
   #
   ##if 'f18klijentlib' in sys.modules:  
   ##   #del sys.modules["f18klijentlib"]
   ##   reload(sys.modules['f18klijentlib'])
   ##   #__import__("f18klijentlib")
   ##reload(f18klijentlib)
   #
   #bStartMainProc = False
   #bInitRT = True
   #bInitConsole = True
   #bReleaseConsole = True
   #
   #bInitConsole = 1
   #bReleaseConsole = 1
   #returnType = 1 # integer
   #f18klijentlib.vmquit(bInitRT)

   nLastStep = 10
   for iter in range(1, nLastStep + 1):
      f18klijentlib.vminit(bStartMainProc, bInitRT, bInitConsole)
   
      print("=======================================", iter , "=========================""")
      #bInitConsole = 1
      #bReleaseConsole = 1
      #returnType = 0 # string
      #ziher_header = f18klijentlib.run_get("func_hello_ziher_2".upper(), bInitConsole, bReleaseConsole, returnType)
      ##s = input()
      ##textual_run(header=ziher_header)

      #ziher_header = f18klijentlib.run_get("func_hello_ziher_2".upper(), bInitConsole, bReleaseConsole, returnType)
      ##s = input()
      ##textual_run(header=ziher_header)

      
      bInitConsole = 1
      bReleaseConsole = 1
      f18klijentlib.run("MAIN".upper(), bInitConsole, bReleaseConsole)
      
      returnType = 0
      for j in range(0, 50):
         print("ziher func_hello_ziher_2", j)
         ziher_header = f18klijentlib.run_get("func_hello_ziher_2".upper(), bInitConsole, bReleaseConsole, returnType)
      #s = input()
      #textual_run(header=ziher_header)

      # nemoj dva puta pozivati main unutar vminit/vmquit
      #f18klijentlib.run("MAIN".upper(), bInitConsole, bReleaseConsole)
      print("=====>>>>>>>>>>>>>>>>>>====== AFTER MAIN =============>>>>>>========================")

      if (iter == nLastStep): 
         textual_run(header="LAST STEP: " + str(nLastStep))
         f18klijentlib.vmquit(1)
      else:
         f18klijentlib.vmquit(0)
      #gc.collect()
      print("=====>>>>>>>>>>>>>>>>>>====== QQQ after vmQuit QQQQ =============>>>>>>========================")

   del sys.modules["f18klijentlib"]

   textual_run(header="back to python - del f18 klijentlib")
   textual_run(header="back to python 2")


else:

   ziher_home = os.getenv("ZIHER_HOME")
   print("ziher_home:", ziher_home)
   sys.path.append(ziher_home + "/lib")
   import f18klijentlib
   bStartMainProc = False
   bInitRT = True
   bReleaseConsole = True
   f18klijentlib.vminit(bStartMainProc, bInitRT, bReleaseConsole)
   f18klijentlib.run(arg1, arg2, arg3)


