#!/usr/bin/env python

import sys
import os

ziher_home = os.getenv("ZIHER_HOME")
print("ziher_home:", ziher_home)
sys.path.append(ziher_home + "/lib")

import f18klijentlib

f18klijentlib.vminit()
f18klijentlib.run("MAIN",1,1)


