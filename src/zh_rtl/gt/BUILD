load("@rules_cc//cc:defs.bzl", "cc_library")
load("//bazel:zh_comp.bzl", "zh_comp_all")
load("//bazel:variables.bzl", "ZH_COMP_OPTS")

cc_library(
    name = "zh_rtl_gt",
    srcs = glob([
        "*.c",
        "gt_std/*.c",
        "gt_trm/*.c",
        #"gt_win/*.c",
        #"gt_wvt/*.c",
        "gt_xwc/*.c"
    ]),
    hdrs = glob( [
        "*.h", 
        "*.zhh",
        "gt_std/*.h",
        "gt_std/*.zhh",
        "gt_trm/*.h",
        "gt_trm/*.zhh",
        #"gt_win/*.h",
        #"gt_win/*.zhh",
        #"gt_wvt/*.h",
        #"gt_wvt/*.zhh",
        "gt_xwc/*.h",
        "gt_wvt/*.zhh",

    ]),
    #alwayslink = 1,
    deps = [ "//zh_zero:zh_zero", "//zh_vm:zh_vm", "//zh_rtl:zh_rtl" ],
    visibility = ["//visibility:public"],
)


cc_library(
    name = "zh_rtl_gt_zh_c",
    srcs = [ ":zh_rtl_gt_zh" ],
    hdrs = glob([
        "*.zh",
        "*.zhh",
    ]),
    deps = [ "//zh_zero:zh_zero", "//zh_rtl:zh_rtl" ],
    visibility = ["//visibility:public"],
)

zh_comp_all(
    name = "zh_rtl_gt_zh", 
    srcs = glob([ 
        "*.zh", 
    ]),
    args = ZH_COMP_OPTS,
    deps = [ "//zh_zero:headers", "//zh_rtl:headers" ],
    visibility = ["//visibility:public"],
)
