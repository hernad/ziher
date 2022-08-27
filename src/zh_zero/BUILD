load("@rules_cc//cc:defs.bzl", "cc_library")
load("//bazel:variables.bzl", "C_OPTS",
    "PYTHON_HEADERS", "PYTHON_COPT" )

#load("//:shared_library.bzl", "windows_dll_library")

cc_library(
    name = "zh_zero",
    srcs = glob( ["*.c"]),
    hdrs = glob( ["*.h", "*.zhh"]) + PYTHON_HEADERS,
    #alwayslink = 1,
    copts = C_OPTS + PYTHON_COPT,
    visibility = ["//visibility:public"],
    # https://github.com/bazelbuild/bazel/issues/92
    includes = ["."],
)

cc_library(
    name = "headers",
    hdrs = glob([
        "*.h",
        "*.zhh",
    ]),
    visibility = ["//visibility:public"],
)

filegroup(
    name = "headers_filegroup",
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



