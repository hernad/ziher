load("@rules_cc//cc:defs.bzl", "cc_library")
load("//bazel:zh_comp.bzl", "zh_comp_all")
load("//bazel:variables.bzl", "ZH_COMP_OPTS", "C_OPTS")


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
    args = ZH_COMP_OPTS + [ "-ithird_party/xlsxwriter" ],
    deps = [ 
        "//zh_zero:headers",
        "//zh_rtl:headers"
    ],
    visibility = ["//visibility:public"],
)
