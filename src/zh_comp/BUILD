load("@rules_bison//bison:bison.bzl", "bison_cc_library")
load("@rules_cc//cc:defs.bzl", "cc_library")

cc_library(
    name = "zh_comp",
    srcs = glob( ["*.c"]),
    hdrs = glob( ["*.h", "*.zhh"]) ,
    #alwayslink = 1,
    deps = [ "//zh_zero:zh_zero", ":zh_comp_y" ],
    visibility = ["//visibility:public"],
)

bison_cc_library(
    name = "zh_comp_y",
    src = "zh_comp.y",
    deps = ["//zh_zero:zh_zero" ],
)
