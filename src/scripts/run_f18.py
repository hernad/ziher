#!/usr/bin/env python

import sys
import os

ziher_home = os.getenv("ZIHER_HOME")
print("ziher_home:", ziher_home)
sys.path.append(ziher_home + "/lib")

import f18klijentlib

# arg 1: ime funkcije, arg 2: 0/1-initconsole, arg 3: 0/1-releaseconsole"
arg1 = sys.argv[1]
arg2 = int(sys.argv[2])
arg3 = int(sys.argv[3])


arg1 = sys.argv[1]
f18klijentlib.vminit()
f18klijentlib.run(arg1, arg2, arg3)
