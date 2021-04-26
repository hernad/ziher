load("@rules_cc//cc:defs.bzl", "cc_library")
load("//bazel:zh_comp.bzl", "zh_comp_all")
load("//bazel:variables.bzl", "ZH_COMP_OPTS", "C_OPTS")

cc_library(
    name = "zh_vm",
    srcs = glob( ["*.c"] + [ "zh_runtime_symbols/init_runtime_symbols.c"]),
    hdrs = glob( ["*.h", "*.zhh"]),
    copts = C_OPTS + [ "-DZH_DYNLIB" ],
    #alwayslink = 1,
    deps = [ 
        "//zh_zero:zh_zero",  
    ],
    visibility = ["//visibility:public"],
    includes = ["."],
)


cc_library(
    name = "zh_vm_zh_c",
    srcs = [ ":zh_vm_zh" ],
    hdrs = glob([
        "*.zh",
        "*.zhh",
    ]),
    copts = [
        "-Izh_rtl"
    ],
    deps = [ 
        "//zh_zero:zh_zero", 
        "//zh_rtl:zh_rtl" 
    ],
    alwayslink = 1,
    visibility = ["//visibility:public"],
)

zh_comp_all(
    name = "zh_vm_zh", 
    srcs = glob([ 
        "*.zh", 
    ]),
    args = ZH_COMP_OPTS,
    deps = [ "//zh_zero:headers", "//zh_rtl:headers" ],
    visibility = ["//visibility:public"],
)

cc_library(
    name = "headers",
    hdrs = glob([
        "*.h",
        "*.zhh"
    ]),
    visibility = ["//visibility:public"],
)

filegroup(
    name = "c_sources",
    srcs = glob( ["*.c"] + [ "zh_runtime_symbols/init_runtime_symbols.c"]),
    visibility = ["//visibility:public"],
)