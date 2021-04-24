load("@rules_cc//cc:defs.bzl", "cc_binary", "cc_library")
load("//bazel:zh_comp.bzl", "zh_comp_all")
load("//bazel:variables.bzl", "ZH_COMP_OPTS", "L_OPTS", "L_OPTS_2", "POSTGRESQL_LIB")
load("//bazel:windows_dll_library.bzl", "windows_dll_library")

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
    linkopts = L_OPTS_2,
    copts = [
        "-Izh_rtl"
    ],
    linkstatic = True,
    visibility = ["//visibility:public"],
)

cc_binary(
    name = "hello_threads",
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
        ":hello_threads_zh_lib_c",
    ],
    linkopts = L_OPTS_2,
    copts = [
        "-Izh_rtl"
    ],
    linkstatic = True,
    visibility = ["//visibility:public"],
)

cc_library(
    name = "hello_threads_zh_lib_c",
    srcs = [ ":hello_threads_zh_lib" ],
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
    name = "hello_threads_zh_lib", 
    srcs = [ "hello_threads.zh" ],
    args = ZH_COMP_OPTS,
    deps = [ 
        "//zh_zero:headers", 
        "//zh_rtl:headers",
        "//zh_vm:headers"
    ],
    visibility = ["//visibility:public"],
)

cc_library(
    name = "hello_world_zh_lib_c",
    srcs = [ ":hello_world_zh_lib" ],
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
    linkopts = ["-lz", "-ldl", "-lpthread", "-lpq"],
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

cc_binary(
    name = "run_hello_dbf_win",
    deps = [
        ":hello_dbf_lib_win_c",
        ":dll_hello_dbf",
    ] + POSTGRESQL_LIB,
    linkopts = L_OPTS + L_OPTS_2,
    copts = [
        "-Izh_rtl",
        "-DZH_DYNIMP"
    ],
    #linkstatic = True,
    visibility = ["//visibility:public"],
)

cc_library(
    name = "hello_dbf_lib_win_c",
    srcs = [ "run_hello_dbf_win.c" ],
    hdrs = glob([
        "*.h",
    ]),
    deps = [ 
       "//zh_zero:zh_zero", 
       "//zh_rtl:zh_rtl",
       "//zh_vm:zh_vm"
    ],
    copts = [
        "-Izh_rtl"
    ],
    alwayslink=1,
    visibility = ["//visibility:public"],
)



#cc_import(
#    name = "libhello_dbf_so",
#    shared_library = "libhello_dbf.dll",
#    hdrs = [],
#    interface_library = "libhello_dbf.lib",
#    # mylib.dll is provided by system environment, for example it can be found in PATH.
#    # This indicates that Bazel is not responsible for making mylib.dll available.
#)


windows_dll_library(
    name = "dll_hello_dbf",
    srcs = [ ":hello_dbf_zh_lib", "lib_hello_dbf_1.c", "//zh_vm:c_sources", "//zh_vm:zh_vm_zh" ],
    hdrs = glob([
        "*.h",
    ]) + [ "//zh_vm:headers"],
    deps = [ 
        "//zh_zero:zh_zero", 
        "//zh_macro:zh_macro",
        "//zh_rtl:zh_rtl",
        "//zh_rtl:zh_rtl_zh_c",
        "//zh_rtl/gt:zh_rtl_gt",
        "//zh_rtl/gt:zh_rtl_gt_zh_c",
        "//zh_rtl/rdd:zh_rtl_rdd",
        "//zh_rtl/rdd:zh_rtl_rdd_zh_c",
    ] + POSTGRESQL_LIB,
    linkopts = L_OPTS + L_OPTS_2,
    copts = [
        "-Izh_rtl",
        "/DCOMPILING_DLL"
    ],
    #linkstatic = False,
    #linkshared = True,
    visibility = ["//visibility:public"],
)


cc_library(
    name = "hello_dbf_zh_lib_c",
    srcs = [ ":hello_dbf_zh_lib" ],
    hdrs = glob([
        "*.h",
    ]),
    deps = [ 
       "//zh_zero:zh_zero", 
       "//zh_rtl:zh_rtl",
       "//zh_vm:zh_vm"
    ],
    copts = [
        "-Izh_rtl",
        "-DZH_DYNLIB"
    ],
    alwayslink=1,
    visibility = ["//visibility:public"],
)

zh_comp_all(
    name = "hello_dbf_zh_lib", 
    srcs = [ "hello_dbf.zh" ],
    args = ZH_COMP_OPTS + [ "-DZH_DYNLIB" ],
    deps = [ 
        "//zh_zero:headers", 
        "//zh_rtl:headers" 
    ],
    visibility = ["//visibility:public"],
)


cc_binary(
    name = "hello_codepage",
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
        ":hello_codepage_zh_lib_c",
    ],
    linkopts = L_OPTS + L_OPTS_2,
    copts = [
        "-Izh_rtl"
    ],
    linkstatic = True,
    visibility = ["//visibility:public"],
)


cc_library(
    name = "hello_codepage_zh_lib_c",
    srcs = [ ":hello_codepage_zh_lib" ],
    hdrs = glob([
        "*.h",
    ]),
    deps = [ 
       "//zh_zero:zh_zero", 
       "//zh_rtl:zh_rtl",
       "//zh_vm:zh_vm"
    ],
    copts = [
        "-Izh_rtl"
    ],
    alwayslink=1,
    visibility = ["//visibility:public"],
)

zh_comp_all(
    name = "hello_codepage_zh_lib", 
    srcs = [ "hello_codepage.zh" ],
    args = ZH_COMP_OPTS,
    deps = [ 
        "//zh_zero:headers", 
        "//zh_rtl:headers" 
    ],
    visibility = ["//visibility:public"],
)
