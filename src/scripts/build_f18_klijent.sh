#!/usr/bin/env bash

bazel build //zh_comp/main:zhcomp
bazel build //F18:F18-klijent-lib.so
