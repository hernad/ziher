load("@rules_cc//cc:defs.bzl", "cc_library")
load("//bazel:variables.bzl", "C_OPTS")

#load("//:windows_dll_library.bzl", "windows_dll_library")

cc_library(
    name = "zh_zero",
    srcs = glob( ["*.c"]),
    hdrs = glob( ["*.h", "*.zhh"]),
    #alwayslink = 1,
    copts = C_OPTS,
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

filegroup(
    name = "c_sources",
    srcs = glob([
        "*.c"
    ]),
    visibility = ["//visibility:public"],
)



