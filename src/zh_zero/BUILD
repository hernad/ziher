load("@rules_cc//cc:defs.bzl", "cc_library")

#load("//:windows_dll_library.bzl", "windows_dll_library")

cc_library(
    name = "zh_zero",
    srcs = glob( ["*.c"]),
    hdrs = glob( ["*.h", "*.zhh"]),
    #alwayslink = 1,
    visibility = ["//visibility:public"],
    # https://github.com/bazelbuild/bazel/issues/92
    includes = ["."],
)

filegroup(
    name = "headers",
    srcs = glob([
        "*.h",
        "*.zhh",
    ]),
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

