load("@rules_cc//cc:defs.bzl", "cc_library")

#load("//:windows_dll_library.bzl", "windows_dll_library")

cc_library(
    name = "zh_zero",
    srcs = glob( ["*.c"]),
    hdrs = glob( ["*.h", "*.zhh"]),
    copts = ["-DZH_MACRO_SUPPORT"],
    alwayslink = 1,
    visibility = ["//visibility:public"],
)

#windows_dll_library(
#    name = "zero_win",
#    srcs = ["lib.cc"],
#    hdrs = ["lib.h"],
#    # Define COMPILING_DLL to export symbols during compiling the DLL.
#    # See lib.h
#    copts = ["/DCOMPILING_DLL"],
#)

