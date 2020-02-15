load("@rules_bison//bison:bison.bzl", "bison_cc_library")
load("@rules_cc//cc:defs.bzl", "cc_library")

cc_library(
    name = "zh_comp",
    srcs = glob( ["*.c"]),
    hdrs = glob( ["*.h", "*.zhh"]) ,
    #copts = ["-DZH_MACRO_SUPPORT"],
    alwayslink = 1,
    deps = [ "//zh_zero:zh_zero", ":zihery" ],
    visibility = ["//visibility:public"],
)

bison_cc_library(
    name = "zihery",
    src = "ziher.y",
    deps = ["//zh_zero:zh_zero" ],
)
