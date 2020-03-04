load("@rules_cc//cc:defs.bzl", "cc_binary")
load("//bazel:variables.bzl", "C_OPTS", "L_OPTS")

cc_binary(
    name = "zhcomp",
    srcs = [ "main.c", "//zh_pp:pp_table" ],
    deps = [ 
        "//zh_zero:zh_zero", 
        "//zh_comp:zh_comp",
        "//zh_pp:zh_pp"
    ],
    copts = C_OPTS,
    linkopts = L_OPTS,
    visibility = ["//visibility:public"],
)
