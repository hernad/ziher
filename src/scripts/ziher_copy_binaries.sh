#!/usr/bin/env bash

cp -av bazel-bin/zh_comp/main/zhcomp $ZIHER_HOME/bin

cp -av bazel-bin/F18/F18-klijent-lib.so $ZIHER_HOME/lib/f18klijentlib.so

cp -av bazel-bin/F18/{ziher,F18}.so $ZIHER_HOME/lib/


