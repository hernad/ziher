load("@rules_cc//cc:defs.bzl", "cc_library")
load("//bazel:zh_comp.bzl", "zh_comp_all")
load("//bazel:variables.bzl", "C_OPTS", "ZH_COMP_OPTS")

ZH_LIB="zh_tools"

cc_library(
    name = ZH_LIB,
    srcs = glob( [
        "*.c",
    ]),
    hdrs = glob([
        "*.h", 
        "*.zhh"
    ]) + [ "//zh_rtl/gt:headers" ],
    copts = [
        "-Izh_rtl/gt",
        "-Izh_rtl"
    ] + C_OPTS,
    alwayslink = 1,
    deps = [
        "//zh_zero:zh_zero",
        "//zh_rtl:zh_rtl", 
        "//zh_vm:zh_vm"
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
    copts = C_OPTS,
    deps = [ 
        "//zh_zero:zh_zero",
        "//zh_rtl:zh_rtl"
    ],
    alwayslink = 1,
    visibility = ["//visibility:public"],
)

zh_comp_all(
    name = ZH_LIB + "_zh", 
    srcs = glob([ 
        "*.zh",
    ]),
    args = ZH_COMP_OPTS,
    deps = [ 
        "//zh_zero:headers", 
        "//zh_rtl:headers", 
        "//zh_rtl/gt:headers", 
        ":headers" 
    ],
    visibility = ["//visibility:public"],
)