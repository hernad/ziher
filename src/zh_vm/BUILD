load("@rules_cc//cc:defs.bzl", "cc_library")
load("//bazel:zh_comp.bzl", "zh_comp_all")
load("//bazel:variables.bzl", "ZH_COMP_OPTS")

cc_library(
    name = "zh_vm",
    srcs = glob( ["*.c"]),
    hdrs = glob( ["*.h", "*.zhh"]) ,
    #alwayslink = 1,
    deps = [ 
        "//zh_zero:zh_zero",  
    ],
    visibility = ["//visibility:public"],
    includes = ["."],
)


cc_library(
    name = "zh_vm_zh_c",
    srcs = [ ":zh_vm_zh" ],
    hdrs = glob([
        "*.zh",
        "*.zhh",
    ]),
    deps = [ "//zh_zero:zh_zero", "//zh_rtl:zh_rtl" ],
    alwayslink = 1,
    visibility = ["//visibility:public"],
)

zh_comp_all(
    name = "zh_vm_zh", 
    srcs = glob([ 
        "*.zh", 
    ]),
    args = ZH_COMP_OPTS,
    deps = [ "//zh_zero:headers", "//zh_rtl:headers" ],
    visibility = ["//visibility:public"],
)
