load("@rules_cc//cc:defs.bzl", "cc_library")
load("//bazel:zh_comp.bzl", "zh_comp_all")
load("//bazel:variables.bzl", "ZH_COMP_OPTS", "C_OPTS")

cc_library(
    name = "zh_rtl",
    srcs = glob([
        "*.c",
        "zlib/*.c",
        "codepage/*.c",
        "crypto/*.c", 
        "string/*.c", 
        "file/*.c",
        "language/*.c",
        "date/*.c",
        "memo/*.c",
        "gt/*.c",
    ]),
    hdrs = glob([
        "*.h", 
        "*.zhh",
        "zlib/*.h", 
        "zlib/*.zhh",
        "codepage/*.h",
        "language/*.h",
        "codepage/*.zhh",
        "crypto/*.h", 
        "crypto/*.zhh",
        "file/*.zhh",
        "file/*.h",
        "string/*.h",
        "string/*.zhh",
        "gt/*.h",
        "gt/*.zhh",
    ]),
    copts = C_OPTS + [
       "-Ithird_party/pcre2",
       "-Ithird_party/zlib",
       "-DSUPPORT_UNICODE",
       "-DPCRE2_CODE_UNIT_WIDTH=8",
       "-DPCRE2_STATIC",
       "-DHAVE_CONFIG_H",
    ],
    alwayslink = 1,
    deps = [ 
        "//zh_zero:zh_zero", 
        "//zh_vm:zh_vm",
        "//third_party/pcre2:headers",
        "//third_party/pcre2:pcre2",
        "//third_party/zlib:zlib"
    ],
    visibility = ["//visibility:public"],
    includes = [
        "*.*", 
        "string/*.*"
    ]
)

filegroup(
    name = "c_sources",
    srcs = glob([
        "*.c",
        "*.h",
        "zlib/*.c",
        "codepage/*.c",
        "crypto/*.c", 
        "string/*.c", 
        "file/*.c",
        "language/*.c",
        "date/*.c",
        "memo/*.c",
        "gt/*.c",
    ]),
    visibility = ["//visibility:public"],
)

#filegroup(
#    name = "headers",
#    srcs = glob([
#        "*.h",
#        "*.zhh",
#        "zh/*.zhh",
#        "file/*.zhh",
#        "crypto/*.h",
#        "crypto/*.zhh",
#        "language/*.h",
#        "zlib/*.h",
#        "zlib/*.zhh"
#    ]),
#    visibility = ["//visibility:public"],
#)

cc_library(
    name = "headers",
    hdrs = glob([
        "*.h",
        "*.zhh",
        "zh/*.zhh",
        "file/*.zhh",
        "crypto/*.h",
        "crypto/*.zhh",
        "language/*.h",
        "zlib/*.h",
        "zlib/*.zhh",
        "string/*.h",
        "string/*.zhh"
    ]),
    visibility = ["//visibility:public"],
)

cc_library(
    name = "zh_rtl_zh_c",
    srcs = [ ":zh_rtl_zh" ],
    hdrs = glob([
        "*.h",
        "*.zhh"
    ]),
    copts = [
        "-Izh_rtl"
    ] + C_OPTS,
    deps = [
        "//zh_zero:zh_zero", 
        "//zh_rtl:zh_rtl",
        "//zh_rtl:headers"
    ],
    alwayslink = 1,
    visibility = ["//visibility:public"],
)

zh_comp_all(
    name = "zh_rtl_zh", 
    srcs = glob([ 
        "*.zh", 
        "file/*.zh",
        "memo/*.zh",
        "zlib/*.zh"
    ]),
    args = ZH_COMP_OPTS,
    deps = [ 
        "//zh_zero:headers", 
        "//zh_rtl:headers" 
    ],
    visibility = ["//visibility:public"],
)