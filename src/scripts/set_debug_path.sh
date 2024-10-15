#!/usr/bin/env bash

set -o errexit

trap "read; reset" EXIT

ZH_SRC=/home/hernad/ziher/ziher_src/src

ZH_DEBUG_PATH="$ZH_SRC/F18"

for p in zh_debug zh_zero zh_rtl F18/common F18/core F18/core_dbf F18/core_pdf F18/core_sql F18/core_string F18/fin F18/kalk F18/fakt F18/pos; do
  ZH_DEBUG_PATH+=":$ZH_SRC/$p"
done

echo "ZH_DEBUG_PATH: $ZH_DEBUG_PATH"

export ZH_DEBUG_PATH

