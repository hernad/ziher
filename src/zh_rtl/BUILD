load("@rules_cc//cc:defs.bzl", "cc_library")
load("//bazel:zh_comp.bzl", "zh_comp_all")
load("//bazel:variables.bzl", "ZH_COMP_OPTS")

cc_library(
    name = "zh_rtl",
    srcs = glob( [
        "*.c",
        "zlib/*.c",
        "codepage/*.c",
        "crypto/*.c", 
        "string/*.c", 
        "file/*.c",
        "language/*.c",
        "date/*.c",
        "memo/*.c",
        "gt/*.c",
    ]),
    hdrs = glob([
        "*.h", 
        "*.zhh",
        "zlib/*.h", 
        "zlib/*.zhh",
        "codepage/*.h",
        "language/*.h",
        "codepage/*.zhh",
        "crypto/*.h", 
        "crypto/*.zhh",
        "file/*.zhh",
        "file/*.h",
        "string/*.h",
        "string/*.zhh",
        "gt/*.h",
        "gt/*.zhh",
    ]),
    alwayslink = 1,
    deps = [ 
        "//zh_zero:zh_zero", 
        "//zh_vm:zh_vm"
    ],
    visibility = ["//visibility:public"],
    includes = ["*.*", "string/*.*"]
)

filegroup(
    name = "headers",
    srcs = glob([
        "*.h",
        "*.zhh",
        "zh/*.zhh",
        "file/*.zhh",
        "crypto/*.h",
        "crypto/*.zhh"
    ]),
    visibility = ["//visibility:public"],
)


cc_library(
    name = "zh_rtl_zh_c",
    srcs = [ ":zh_rtl_zh" ],
    hdrs = glob([
        "*.h",
        "*.zhh"
    ]),
    deps = [ "//zh_zero:zh_zero", "//zh_rtl:zh_rtl" ],
    alwayslink = 1,
    visibility = ["//visibility:public"],
)

zh_comp_all(
    name = "zh_rtl_zh", 
    srcs = glob([ 
        "*.zh", 
        "file/*.zh",
        "memo/*.zh",
        "zlib/*.zh"
    ]),
    args = ZH_COMP_OPTS,
    deps = [ "//zh_zero:headers", "//zh_rtl:headers" ],
    visibility = ["//visibility:public"],
)