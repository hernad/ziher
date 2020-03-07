#load("@rules_bison//bison:bison.bzl", "bison_cc_library")
load("@rules_cc//cc:defs.bzl", "cc_library")
load("//bazel:variables.bzl", "C_OPTS")
load("//bazel:cmd_bison.bzl", "cmd_bison")

cc_library(
    name = "zh_comp",
    srcs = glob( ["*.c"]) + [ ":zh_comp_y" ],
    hdrs = glob( ["*.h", "*.zhh"]),
    #alwayslink = 1,
    copts = C_OPTS,
    deps = [ "//zh_zero:zh_zero"],
    visibility = ["//visibility:public"],
)

#bison_cc_library(
#    name = "zh_comp_y",
#    src = "zh_comp.y",
#    deps = ["//zh_zero:zh_zero" ],
#)

cmd_bison(
    name = "zh_comp_y",
    src = "zh_comp.y",
    out = "zh_comp_y",
    visibility = ["//visibility:public"],
)
