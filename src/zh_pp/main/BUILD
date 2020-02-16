load("@rules_cc//cc:defs.bzl", "cc_binary")

cc_binary(
    name = "zhpp",
    srcs = [ "main.c" ],
    deps = [ 
        "//zh_zero:zh_zero",
        "//zh_pp:zh_pp",
    ],
    visibility = ["//visibility:public"],
)
