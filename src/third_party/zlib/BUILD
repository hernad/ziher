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
    name = "headers",
    srcs = glob([
        "*.h",
        "*.zhh",
    ]),
    visibility = ["//visibility:public"],
)