load("@rules_cc//cc:defs.bzl", "cc_library")
load("//bazel:zh_comp.bzl", "zh_comp_all")
load("//bazel:variables.bzl", "ZH_COMP_OPTS", "ZH_DEPS_STD", "C_OPTS")

ZH_LIB="zh_minizip"

cc_library(
    name = ZH_LIB,
    srcs = glob( [
        "*.c",
    ]),
    hdrs = glob([
        "*.h", 
        "*.zhh"
    ]) + [
        "//zh_rtl:headers", 
        "//third_party/minizip:headers",
        "//third_party/zlib:headers",
    ],
    copts = [
        "-Izh_rtl",
        "-Ithird_party/minizip",
        "-Ithird_party/zlib"
    ],
    alwayslink = 1,
    deps = [ 
        "//zh_zero:zh_zero", 
        "//zh_vm:zh_vm",
        "//third_party/minizip:minizip"
    ],
    visibility = ["//visibility:public"],
    includes = ["*.*", "string/*.*"]
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
        "*.c",
    ]),
    visibility = ["//visibility:public"],
)

cc_library(
    name = ZH_LIB + "_zh_c",
    srcs = [ ":" + ZH_LIB + "_zh"],
    hdrs = glob([
        "*.h",
        "*.zhh"
    ]),
    deps = [ 
        "//zh_zero:zh_zero", 
        "//zh_rtl:zh_rtl" 
    ],
    copts = C_OPTS,
    alwayslink = 1,
    visibility = ["//visibility:public"],
)

zh_comp_all(
    name = ZH_LIB + "_zh", 
    srcs = glob([ 
        "*.zh",
    ]),
    args = ZH_COMP_OPTS + [ 
        "-ithird_party/minizip" 
    ],
    deps = ZH_DEPS_STD + [  
        "//third_party/minizip:headers", 
        ":headers_filegroup" 
    ],
    visibility = ["//visibility:public"],
)
