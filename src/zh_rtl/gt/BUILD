load("@rules_cc//cc:defs.bzl", "cc_library")

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
    alwayslink = 1,
    deps = [ "//zh_zero:zh_zero", "//zh_vm:zh_vm", "//zh_rtl:zh_rtl" ],
    visibility = ["//visibility:public"],
)

