load("@rules_cc//cc:defs.bzl", "cc_library")
load("//bazel:zh_comp.bzl", "zh_comp_all")
load("//bazel:variables.bzl", "ZH_COMP_OPTS", "C_OPTS")

ZH_LIB="zh_debug"

cc_library(
    name = ZH_LIB,
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
