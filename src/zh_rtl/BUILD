load("@rules_cc//cc:defs.bzl", "cc_library")

cc_library(
    name = "zh_rtl",
    srcs = glob( [
        "*.c", 
        "zlib/*.c", 
        "crypto/*.c", 
        "string/*.c", 
        "file/*.c",
        "date/*.c",
        "memo/*.c",
        "gt/*.c"
    ]),
    hdrs = glob( [
        "*.h", 
        "*.zhh", 
        "zlib/*.h", 
        "zlib/*.zhh", 
        "crypto/*.h", 
        "crypto/*.zhh",
        "file/*.zhh",
        "file/*.h",
        "string/*.h",
        "string/*.zhh",
        "gt/*.h",
        "gt/*.zhh"
    ]),
    alwayslink = 1,
    deps = [ "//zh_zero:zh_zero", "//zh_vm:zh_vm" ],
    visibility = ["//visibility:public"],
)

