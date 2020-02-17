load("@rules_cc//cc:defs.bzl", "cc_library")

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
        "rdd_usr/*.c"
    ]),
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
        "rdd_usr/*.zhh"
    ]),
    alwayslink = 1,
    deps = [ "//zh_zero:zh_zero", "//zh_vm:zh_vm", "//zh_rtl:zh_rtl" ],
    visibility = ["//visibility:public"],
)

