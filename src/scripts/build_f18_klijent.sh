#!/usr/bin/env bash

set -o errexit

bazel build //zh_comp/main:zhcomp --verbose_failures

bazel build //F18:F18-klijent-lib.so  --verbose_failures
