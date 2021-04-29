load("@rules_cc//cc:defs.bzl", "cc_library")

cc_library(
    name = "png",
    srcs = glob([ "*.c" ]),
    hdrs = glob([
        "*.h",
        "**/*.h",
    ]),
    deps = [ 
       "//zh_zero:zh_zero", 
       "//zh_rtl:zh_rtl",
       "//zh_vm:zh_vm",
       "//third_party/zlib:zlib"
    ],
    copts = [
        "-Ithird_party/zlib"
    ],
    alwayslink=1,
    visibility = ["//visibility:public"],
)

cc_library(
    name = "headers",
    srcs = glob([
        "*.h",
        "**/.h",
    ]),
    visibility = ["//visibility:public"],
)

filegroup(
    name = "c_sources",
    srcs = glob([ "*.c" ]),
    visibility = ["//visibility:public"],
)
