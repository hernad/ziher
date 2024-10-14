#!/usr/bin/env bash

ZH_SRC=/home/hernad/ziher/ziher_src/src

ZH_DEBUG_PATH="$ZH_SRC/F18"

for p in zh_debug zh_zero zh_rtl F18/common F18/core F18/core_dbf F18/core_pdf F18/core_sql F18/core_string F18/fin F18/kalk F18/fakt F18/pos; do
  ZH_DEBUG_PATH+=":$ZH_SRC/$p"
done

echo "ZH_DEBUG_PATH: $ZH_DEBUG_PATH"

export ZH_DEBUG_PATH

fn="MAIN"
python scripts/run_f18.py $fn 1 1

echo "============== kraj prvog poziva F18 ============================="
read

fn="textual"
python scripts/run_f18.py $fn 0 0

fn="NASLOVNI_EKRAN_SPLASH_SCREEN"
python scripts/run_f18.py $fn 1 1

echo "============== kraj splash ============================="

#fn="SHOW_CMDLINE"
#python scripts/run_f18.py $fn 1 1

# kad stavis pip u python onda ne radi input
#cat <<EOF | python
#import sys
#import os
#
#ziher_home = os.getenv("ZIHER_HOME")
#print("ziher_home:", ziher_home)
#sys.path.append(ziher_home + "/lib")
#
#import f18klijentlib
#
#f18klijentlib.vminit()
#f18klijentlib.run("MAIN",1,1)
#
#EOF

reset
