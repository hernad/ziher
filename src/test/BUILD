load("@rules_cc//cc:defs.bzl", "cc_binary", "cc_library")
load("//bazel:zh_comp.bzl", "zh_comp_all")
load("//bazel:variables.bzl", "ZH_COMP_OPTS")


cc_binary(
    name = "hello_world",
    deps = [ 
        "//zh_zero:zh_zero", 
        "//zh_vm:zh_vm",
        "//zh_vm:zh_vm_zh_c",
        "//zh_macro:zh_macro",
        "//zh_rtl:zh_rtl",
        "//zh_rtl:zh_rtl_zh_c",
        "//zh_rtl/gt:zh_rtl_gt",
        "//zh_rtl/gt:zh_rtl_gt_zh_c",
        "//zh_rtl/rdd:zh_rtl_rdd",
        "//zh_rtl/rdd:zh_rtl_rdd_zh_c",
        ":hello_world_zh_lib_c",
    ],
    linkopts = ["-lz", "-ldl", "-lpthread"],
    linkstatic = True,
    visibility = ["//visibility:public"],
)


cc_library(
    name = "hello_world_zh_lib_c",
    srcs = [ ":hello_world_zh_lib" ],
    hdrs = glob([
        "*.h",
    ]),
    deps = [ 
       "//zh_zero:zh_zero", 
       "//zh_rtl:zh_rtl",
       "//zh_vm:zh_vm"
    ],
    alwayslink=1,
    visibility = ["//visibility:public"],
)

zh_comp_all(
    name = "hello_world_zh_lib", 
    srcs = [ "hello.zh" ],
    args = ZH_COMP_OPTS,
    deps = [ 
        "//zh_zero:headers", 
        "//zh_rtl:headers" 
    ],
    visibility = ["//visibility:public"],
)





cc_binary(
    name = "hello_code_block",
    deps = [ 
        "//zh_zero:zh_zero", 
        "//zh_vm:zh_vm",
        "//zh_vm:zh_vm_zh_c",
        "//zh_macro:zh_macro",
        "//zh_rtl:zh_rtl",
        "//zh_rtl:zh_rtl_zh_c",
        "//zh_rtl/gt:zh_rtl_gt",
        "//zh_rtl/gt:zh_rtl_gt_zh_c",
        "//zh_rtl/rdd:zh_rtl_rdd",
        "//zh_rtl/rdd:zh_rtl_rdd_zh_c",
        ":hello_code_block_zh_lib_c",
    ],
    linkopts = ["-lz", "-ldl", "-lpthread"],
    linkstatic = True,
    visibility = ["//visibility:public"],
)


cc_library(
    name = "hello_code_block_zh_lib_c",
    srcs = [ ":hello_code_block_zh_lib" ],
    hdrs = glob([
        "*.h",
    ]),
    deps = [ 
       "//zh_zero:zh_zero", 
       "//zh_rtl:zh_rtl",
       "//zh_vm:zh_vm"
    ],
    alwayslink=1,
    visibility = ["//visibility:public"],
)

zh_comp_all(
    name = "hello_code_block_zh_lib", 
    srcs = [ "hello_code_block.zh" ],
    args = ZH_COMP_OPTS,
    deps = [ 
        "//zh_zero:headers", 
        "//zh_rtl:headers" 
    ],
    visibility = ["//visibility:public"],
)