load("@rules_cc//cc:defs.bzl", "cc_library")
load("//bazel:zh_comp.bzl", "zh_comp_all")
load("//bazel:variables.bzl", "ZH_COMP_OPTS")

cc_library(
    name = "zh_rtl_rdd",
    srcs = glob([
        "*.c",
        #"nul_sys/*.c",
        "dbf_cdx/*.c",
        "dbf_fpt/*.c",
        "dbf_nsx/*.c",
        "dbf_six/*.c",
        "hsx/*.c",
        "rdd_misc/*.c",
        "rdd_sql/*.c",
        "rdd_usr/*.c",
        "sdd_pgsql/*.c"
    ]) + [
        "//zh_rtl:headers",
        "//zh_rtl/rdd:headers"
    ],
    copts = [
        "-Izh_rtl/rdd/rdd_sql"
    ],
    hdrs = glob( [
        "*.h", 
        "*.zhh",
        #"nul_sys/*.h",
        "dbf_cdx/*.h",
        "dbf_cdx/*.zhh",
        "dbf_fpt/*.h",
        "dbf_fpt/*.zhh",
        "dbf_nsx/*.h",
        "dbf_nsx/*.zhh",
        "dbf_six/*.h",
        "dbf_six/*.zhh",
        "hsx/*.h",
        "rdd_misc/*.h",
        "rdd_sql/*.h",
        "rdd_sql/*.zhh",
        "rdd_usr/*.h",
        "rdd_usr/*.zhh",
        "sdd_pgsql/*.h",
        "sdd_pgsql/*.zhh"
    ]),
    alwayslink = 1,
    deps = [ 
        "//zh_zero:zh_zero", 
        "//zh_vm:zh_vm", 
        "//zh_rtl:zh_rtl" 
    ],
    visibility = ["//visibility:public"],
)

filegroup(
    name = "headers",
    srcs = glob([
        "*.h",
        "*.zhh",
        "rdd_sql/*.h",
        "rdd_sql/*.zhh"
    ]),
    visibility = ["//visibility:public"],
)


cc_library(
    name = "zh_rtl_rdd_zh_c",
    srcs = [ ":zh_rtl_rdd_zh" ],
    hdrs = glob([
        "*.h",
        "*.zhh",
    ]),
    deps = [ "//zh_zero:zh_zero", "//zh_rtl:zh_rtl" ],
    alwayslink = 1,
    visibility = ["//visibility:public"],
)

zh_comp_all(
    name = "zh_rtl_rdd_zh", 
    srcs = glob([ 
        "*.zh", 
        "rdd_misc/*.zh",
        "dbf_six/*.zh",
    ]),
    args = ZH_COMP_OPTS,
    deps = [ "//zh_zero:headers", "//zh_rtl:headers" ],
    visibility = ["//visibility:public"],
)