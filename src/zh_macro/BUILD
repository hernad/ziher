#load("@rules_bison//bison:bison.bzl", "bison_cc_library")
load("@rules_cc//cc:defs.bzl", "cc_library")
load("//bazel:cmd_bison.bzl", "cmd_bison")

cc_library(
    name = "zh_macro",
    srcs = glob( ["*.c"]) + [":zh_macro_y"],
    hdrs = glob( ["*.h", "*.zhh"]) ,
    alwayslink = 1,
    deps = [ "//zh_zero:zh_zero", "//zh_comp:zh_comp" ],
    visibility = ["//visibility:public"],
)

#bison_cc_library(
#    name = "zh_macroy",
#    src = "zh_macro.y",
#    deps = ["//zh_zero:zh_zero" ],
#)

cmd_bison(
    name = "zh_macro_y",
    src = "zh_macro.y",
    out = "zh_macro_y",
    visibility = ["//visibility:public"],
)
