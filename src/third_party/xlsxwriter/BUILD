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
       "//zh_vm:zh_vm",
       "//third_party/zlib:zlib"
    ],
    copts = [
        "-DUSE_STANDARD_TMPFILE",
        "-Ithird_party/zlib"
    ],
    alwayslink=1,
    visibility = ["//visibility:public"],
)

cc_library(
    name = "headers",
    hdrs = glob([
        "*.h",
        "**/*.h",
    ]),
    visibility = ["//visibility:public"],
)

filegroup(
    name = "c_sources",
    srcs = glob([ "*.c" ]),
    visibility = ["//visibility:public"],
)
