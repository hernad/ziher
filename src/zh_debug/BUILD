load("@rules_cc//cc:defs.bzl", "cc_library")
load("//bazel:zh_comp.bzl", "zh_comp_all")
load("//bazel:variables.bzl", "ZH_COMP_OPTS", "ZH_DEPS_STD", "C_OPTS")


cc_library(
    name = "zh_debug",
    srcs = glob([
        "*.c",
    ]),
    hdrs = glob([
        "*.h", 
    ]),
    copts = C_OPTS,
    alwayslink = 1,
    deps = [ 
        "//zh_zero:zh_zero", 
        "//zh_rtl:zh_rtl",
    ],
    visibility = ["//visibility:public"],
    includes = [
        "*.*",
    ]
)

cc_library(
    name = "zh_debug_zh_c",
    srcs = [ ":zh_debug_zh" ],
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
    name = "zh_debug_zh", 
    srcs = glob([ 
        "*.zh"
    ]),
    hdrs = glob( [
        "en.txt"
    ]),
    deps = ZH_DEPS_STD,
    args = ZH_COMP_OPTS + [ "-ithird_party/xlsxwriter" ],
    #args = ZH_COMP_OPTS,
    #deps = [ 
    #    "//zh_zero:zh_zero", 
    #    "//zh_rtl:zh_rtl",
    #    "//zh_zero:headers",
    #    "//zh_rtl:headers"
    #],
    visibility = ["//visibility:public"],
)

filegroup(
    name = "c_sources",
    srcs = glob([
        "*.c"
    ]),
    visibility = ["//visibility:public"],
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
