load("@rules_cc//cc:defs.bzl", "cc_library")

cc_library(
    name = "zh_rtl",
    srcs = glob( ["*.c", "zlib/*.c", "crypto/*.c", "string/*.c", "file/*.c"]),
    hdrs = glob( [
        "*.h", 
        "*.zhh", 
        "zlib/*.h", 
        "zlib/*.zhh", 
        "crypto/*.h", 
        "crypto/*.zhh",
        "file/*.zhh",
        "file/*.h"
    ]),
    alwayslink = 1,
    deps = [ "//zh_zero:zh_zero", "//zh_vm:zh_vm" ],
    visibility = ["//visibility:public"],
)

