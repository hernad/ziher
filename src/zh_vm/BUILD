load("@rules_cc//cc:defs.bzl", "cc_library")

cc_library(
    name = "zh_vm",
    srcs = glob( ["*.c"]),
    hdrs = glob( ["*.h", "*.zhh"]) ,
    alwayslink = 1,
    deps = [ "//zh_zero:zh_zero" ],
    visibility = ["//visibility:public"],
    includes = ["."],
)
