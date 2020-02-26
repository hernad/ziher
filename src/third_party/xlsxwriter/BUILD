load("@rules_cc//cc:defs.bzl", "cc_library")


cc_library(
    name = "xlsxwriter",
    srcs = glob([ "*.c" ]),
    hdrs = glob([
        "*.h",
        "**/*.h",
    ]),
    deps = [ 
       "//zh_zero:zh_zero", 
       "//zh_rtl:zh_rtl",
       "//zh_vm:zh_vm"
    ],
    copts = [
        "-DUSE_STANDARD_TMPFILE"
    ],
    alwayslink=1,
    visibility = ["//visibility:public"],
)

filegroup(
    name = "headers",
    srcs = glob([
        "*.h",
        "*.zhh",
    ]),
    visibility = ["//visibility:public"],
)