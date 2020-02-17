load("@rules_cc//cc:defs.bzl", "cc_binary")

cc_binary(
    name = "zhcomp",
    srcs = [ "main.c", "//zh_pp:pp_table" ],
    deps = [ 
        "//zh_zero:zh_zero", 
        "//zh_comp:zh_comp",
        "//zh_pp:zh_pp"
    ],
    visibility = ["//visibility:public"],
)
