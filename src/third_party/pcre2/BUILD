load("@rules_cc//cc:defs.bzl", "cc_library")


cc_library(
    name = "pcre2",
    srcs = glob([ "*.c" ]),
    hdrs = glob([
        "*.h",
        "**/*.h",
    ]),
    deps = [ 
       "//zh_zero:zh_zero"
    ],
    copts = [ 
       "-DSUPPORT_UNICODE",
       "-DPCRE2_CODE_UNIT_WIDTH=8",
       "-DPCRE2_STATIC",
       "-DHAVE_CONFIG_H",
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
