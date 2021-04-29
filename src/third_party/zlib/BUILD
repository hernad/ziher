load("@rules_cc//cc:defs.bzl", "cc_library")


cc_library(
    name = "zlib",
    srcs = glob([ "*.c" ]),
    hdrs = glob([
        "*.h",
        "**/*.h",
    ]),
    deps = [
    ],
    copts = [
    ],
    alwayslink=1,
    visibility = ["//visibility:public"],
)

filegroup(
    name = "c_sources",
    srcs = glob([
        "*.c"
    ]),
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
