load("@rules_cc//cc:defs.bzl", "cc_library")
load("//bazel:zh_comp.bzl", "zh_comp_all")
load("//bazel:variables.bzl", "ZH_COMP_OPTS", "ZH_DEPS_STD", "POSTGRESQL_HEADERS", "POSTGRESQL_COPT", "C_OPTS")

ZH_LIB="zh_pgsql"

cc_library(
    name = ZH_LIB,
    srcs = glob([
        "*.c",
    ]),
    hdrs = glob([
        "*.h", 
        "*.zhh"
    ]) + POSTGRESQL_HEADERS,
    copts = [
    ]  + POSTGRESQL_COPT,
    alwayslink = 1,
    deps = [ 
        "//zh_zero:zh_zero", 
        "//zh_vm:zh_vm",
        #"//third_party/xlsxwriter:xlsxwriter"
    ],
    visibility = ["//visibility:public"],
    includes = ["*.*", "string/*.*"]
)

cc_library(
    name = "headers",
    hdrs = glob([
        "*.h",
        "*.zhh",
    ]),
    visibility = ["//visibility:public"],
)

filegroup(
    name = "headers_filegroup",
    srcs = glob([
        "*.h",
        "*.zhh",
    ]),
    visibility = ["//visibility:public"],
)

filegroup(
    name = "c_sources",
    srcs = glob([
        "*.c",
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
    args = ZH_COMP_OPTS + [ "-ithird_party/xlsxwriter" ],
    deps = ZH_DEPS_STD + [ 
        #"//third_party/xlsxwriter:headers", 
        ":headers_filegroup" 
    ],
    visibility = ["//visibility:public"],
)
