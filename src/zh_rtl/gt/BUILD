load("@rules_cc//cc:defs.bzl", "cc_library")
load("//bazel:zh_comp.bzl", "zh_comp_all")
load("//bazel:variables.bzl", "ZH_COMP_OPTS", "C_OPTS")


_WINDOWS_C_SRC = glob([
        "*.c",
        "gt_std/*.c",
        "gt_win/*.c",
        "gt_wvt/*.c"
    ])

_WINDOWS_C_HDR = glob( [
        "*.h", 
        "*.zhh",
        "gt_std/*.h",
        "gt_std/*.zhh",
        "gt_win/*.h",
        "gt_win/*.zhh",
        "gt_wvt/*.h",
        "gt_wvt/*.zhh"
    ])


_LINUX_C_SRC = glob([
        "*.c",
        "gt_std/*.c",
        "gt_trm/*.c",
        #"gt_xwc/*.c"
    ])

_LINUX_C_HDR = glob( [
        "*.h", 
        "*.zhh",
        "gt_std/*.h",
        "gt_std/*.zhh",
        "gt_trm/*.h",
        "gt_trm/*.zhh",
        #"gt_xwc/*.h",
    ])

_C_SRC = select({
        "@bazel_tools//src/conditions:darwin": _LINUX_C_SRC,
        "@bazel_tools//src/conditions:windows": _WINDOWS_C_SRC,
        "//conditions:default": _LINUX_C_SRC,
    })

_C_HDR = select({
        "@bazel_tools//src/conditions:darwin": _LINUX_C_HDR,
        "@bazel_tools//src/conditions:windows": _WINDOWS_C_HDR,
        "//conditions:default": _LINUX_C_HDR,
    })


cc_library(
    name = "zh_rtl_gt",
    srcs = _C_SRC,
    hdrs = _C_HDR,
    alwayslink = 1,
    copts = C_OPTS,
    deps = [ 
        "//zh_zero:zh_zero", 
        "//zh_vm:zh_vm", 
        "//zh_rtl:zh_rtl" ],
    visibility = ["//visibility:public"],
)

filegroup(
    name = "c_sources",
    srcs = _C_SRC,
    visibility = ["//visibility:public"],
)

cc_library(
    name = "headers",
    hdrs = _C_HDR,
    visibility = ["//visibility:public"],
)

cc_library(
    name = "zh_rtl_gt_zh_c",
    srcs = [ ":zh_rtl_gt_zh", "//zh_rtl:headers" ],
    hdrs = glob([
        "*.zh",
        "*.zhh",
    ]),
    copts = [
        "-Izh_rtl"
    ] + C_OPTS,
    deps = [ "//zh_zero:zh_zero", "//zh_rtl:zh_rtl" ],
    alwayslink = 1,
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


