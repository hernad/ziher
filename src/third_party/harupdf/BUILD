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
       "//third_party/png:png",
       "//third_party/zlib:zlib"
    ],
    copts = [
        "-Ithird_party/zlib",
        "-Ithird_party/png"
    ],
    alwayslink=1,
    visibility = ["//visibility:public"],
)

cc_library(
    name = "headers",
    hdrs = glob([
        "*.h",
        "*.zhh",
    ]),
    visibility = ["//visibility:public"],
)

filegroup(
    name = "c_sources",
    srcs = glob([ "*.c" ]),
    visibility = ["//visibility:public"],
)
