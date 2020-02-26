load("@rules_cc//cc:defs.bzl", "cc_library")


cc_library(
    name = "harupdf",
    srcs = glob([ "*.c" ]),
    hdrs = glob([
        "*.h",
        "**/*.h",
    ]),
    deps = [ 
       "//zh_zero:zh_zero", 
       "//zh_rtl:zh_rtl",
       "//zh_vm:zh_vm",
       "//third_party/png:png"
    ],
    copts = [
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