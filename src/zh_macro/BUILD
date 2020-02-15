load("@rules_bison//bison:bison.bzl", "bison_cc_library")
load("@rules_cc//cc:defs.bzl", "cc_library")

cc_library(
    name = "zh_macro",
    srcs = glob( ["*.c"]),
    hdrs = glob( ["*.h", "*.zhh"]) ,
    alwayslink = 1,
    deps = [ "//zh_zero:zh_zero", "//zh_comp:zh_comp", ":zh_macroy" ],
    visibility = ["//visibility:public"],
)

bison_cc_library(
    name = "zh_macroy",
    src = "zh_macro.y",
    deps = ["//zh_zero:zh_zero" ],
)
