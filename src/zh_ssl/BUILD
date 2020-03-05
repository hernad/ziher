load("@rules_cc//cc:defs.bzl", "cc_library")
load("//bazel:zh_comp.bzl", "zh_comp_all")
load("//bazel:variables.bzl", "ZH_COMP_OPTS", "POSTGRESQL_HEADERS", "POSTGRESQL_COPT", "C_OPTS")

ZH_LIB="zh_ssl"

cc_library(
    name = ZH_LIB,
    srcs = glob( [
        "*.c",
    ]),
    hdrs = glob([
        "*.h", 
        "*.zhh"
    ]) + [
        "//zh_rtl:headers",
    ] + POSTGRESQL_HEADERS,
    copts = [
        "-Izh_rtl",
    ] + POSTGRESQL_COPT + C_OPTS,
    alwayslink = 1,
    deps = [ 
        "//zh_zero:zh_zero", 
        "//zh_vm:zh_vm",
    ],
    visibility = ["//visibility:public"],
    includes = ["*.*", "string/*.*"]
)

filegroup(
    name = "headers",
    srcs = glob([
        "*.h",
        "*.zhh",
    ]),
    visibility = ["//visibility:public"],
)


cc_library(
    name = ZH_LIB + "_zh_c",
    srcs = [ ":" + ZH_LIB + "_zh"],
    hdrs = glob([
        "*.h",
        "*.zhh"
    ]),
    deps = [ 
        "//zh_zero:zh_zero", 
        "//zh_rtl:zh_rtl" 
    ],
    copts = C_OPTS,
    alwayslink = 1,
    visibility = ["//visibility:public"],
)

zh_comp_all(
    name = ZH_LIB + "_zh", 
    srcs = glob([ 
        "*.zh",
    ]),
    args = ZH_COMP_OPTS + [ 
        #   "-ithird_party/minizip" 
    ],
    deps = [ 
        "//zh_zero:headers", 
        "//zh_rtl:headers", 
        #"//third_party/minizip:headers", 
        ":headers" 
    ],
    visibility = ["//visibility:public"],
)
