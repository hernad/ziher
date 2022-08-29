load("@rules_cc//cc:defs.bzl", "cc_binary", "cc_library")
load("//bazel:zh_comp.bzl", "zh_comp_all")
load("//bazel:variables.bzl", "ZH_COMP_OPTS_DEBUG", "ZH_DEPS_STD", 
   "L_OPTS", "L_OPTS_2",
   "PYTHON_HEADERS", "PYTHON_COPT", "PYTHON_LIB", 
   "POSTGRESQL_HEADERS", "POSTGRESQL_COPT", "POSTGRESQL_LIB")
load("//bazel:shared_library.bzl", "windows_dll_library")


cc_binary(
    name = "hello_proj_static",
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
        "//zh_debug:zh_debug",
        "//zh_debug:zh_debug_zh_c",
        ":hello_proj_zh_lib_c",
    ] + POSTGRESQL_LIB,
    linkopts = L_OPTS + L_OPTS_2,
    copts = [
        "-Izh_rtl",
    ],
    linkstatic = True,
    visibility = ["//visibility:public"],
)



cc_library(
    name = "hello_proj_zh_lib_c",
    srcs = [ ":hello_proj_zh_lib" ],
    hdrs = glob([
        "*.h",
    ]),
    copts = [
        "-Izh_rtl"
    ],
    deps = [ 
       "//zh_zero:zh_zero", 
       "//zh_rtl:zh_rtl",
       "//zh_vm:zh_vm"
    ],
    alwayslink=1,
    visibility = ["//visibility:public"],
)

zh_comp_all(
    name = "hello_proj_zh_lib", 
    srcs = [ 
      "hello_proj.zh",
      "threads/thread1.zh",
      #"vscode_debug.zh" 
    ],
    args = ZH_COMP_OPTS_DEBUG,
    deps = ZH_DEPS_STD,
    visibility = ["//visibility:public"],
)