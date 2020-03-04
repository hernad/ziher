load("@rules_cc//cc:defs.bzl", "cc_binary")
load("//bazel:variables.bzl", "C_OPTS", "L_OPTS")

cc_binary(
    name = "zhpp",
    srcs = [ "main.c" ],
    deps = [ 
        "//zh_zero:zh_zero",
        "//zh_pp:zh_pp",
    ],
    copts = C_OPTS,
    linkopts = L_OPTS,
    visibility = ["//visibility:public"],
)
