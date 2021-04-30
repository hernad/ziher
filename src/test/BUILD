load("@rules_cc//cc:defs.bzl", "cc_binary", "cc_library")
load("//bazel:zh_comp.bzl", "zh_comp_all")
load("//bazel:variables.bzl", "ZH_COMP_OPTS", "L_OPTS", "L_OPTS_2", "POSTGRESQL_HEADERS", "POSTGRESQL_COPT", "POSTGRESQL_LIB")
load("//bazel:windows_dll_library.bzl", "windows_dll_library")

#load("@bazel_skylib//rules:common_settings.bzl", "bool_flag")
#bool_flag(
#    name = "arch_x86",
#    build_setting_default = False, 
#    visibility = ["//visibility:public"],
#)
#config_setting(
#    name = "is_flag_at_top_enabled",
#    flag_values = {":arch_x86": "True"},
#)

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
    srcs = ["run_hello_dbf_win.c"],
    deps = [
        "//zh_zero:headers",
        ":hello_dbf",
    ] + POSTGRESQL_LIB,
    linkopts = L_OPTS + L_OPTS_2,
    copts = [
        "-Izh_zero",
        "-DZH_DYNIMP",
        #"-DZH_TR_LEVEL=5",
    ],
    #linkstatic = True,
    visibility = ["//visibility:public"],
)


windows_dll_library(
    name = "hello_dbf",
    #srcs = select({
    #    ":is_flag_at_top_enabled": ["top.txt"],
    #    "//conditions:default": [ ":hello_dbf_zh_lib", "lib_hello_dbf_1.c"],
    #}),
    #srcs = [ ":hello_dbf_zh_lib", "lib_hello_dbf_1.c"],
    srcs = [ ":hello_dbf_zh_lib" ], 
    hdrs = glob([
        "*.h",
        "*.zhh"
    ]) + POSTGRESQL_HEADERS,
    deps = [ 
        ":ziher",
    ] + POSTGRESQL_LIB,
    linkopts = L_OPTS + L_OPTS_2,
    copts = [
        "-Izh_zero",
        "-Izh_vm",
        "-Izh_rtl",
        "-DZH_DYNLIB",
        #"-DZH_TR_LEVEL=4", #INFO
        #"-DZH_TR_LEVEL=5", DEBUG
    ]  + POSTGRESQL_COPT,
    #linkstatic = False,
    #linkshared = True,
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

# c:\dev\bazelisk.exe build --explain=explain.log --verbose_explanations //test:run_hello_dbf_win

# bazel-out\x64_windows-fastbuild\bin\test\_objs\dll_hello_dbf.dll

# c:\dev\bazelisk.exe build --subcommands //test:run_hello_dbf_win

# https://stackoverflow.com/questions/43921911/how-to-resolve-bazel-undeclared-inclusions-error

#C:\dev\ziher\src>dumpbin /imports  bazel-bin\test\run_hello_dbf_win.exe | grep zh
#                         C76 zh_vmQuit
#                         C57 zh_vmInit
#                         71C zh_cmdargInit

windows_dll_library(
    name = "ziher",
    srcs = [ 
       "lib_hello_dbf_1.c",
       "//zh_zero:c_sources",
       "//zh_vm:c_sources", 
       "//zh_vm:zh_vm_zh",
       "//zh_macro:c_sources",
       "//zh_rtl:c_sources",
       "//zh_rtl:zh_rtl_zh",
       "//zh_rtl/gt:c_sources",
       "//zh_rtl/gt:zh_rtl_gt_zh",
       "//zh_rtl/rdd:c_sources",
       "//zh_rtl/rdd:zh_rtl_rdd_zh",
       "//third_party/zlib:c_sources",
       "//third_party/pcre2:c_sources"
    ],
    hdrs = glob([
        "*.h",
        "*.zhh"
    ]) + POSTGRESQL_HEADERS,
    deps = [ 
        "//zh_zero:headers",
        "//zh_vm:headers",
        "//zh_macro:headers",
        "//zh_comp:headers",
        "//zh_rtl:headers",
        "//zh_rtl/gt:headers",
        "//zh_rtl/rdd:headers",
        "//third_party/zlib:headers",
        "//third_party/pcre2:headers",
    ] + POSTGRESQL_LIB,
    linkopts = L_OPTS + L_OPTS_2,
    copts = [
        "-DSUPPORT_UNICODE",
        "-DHAVE_CONFIG_H",
        "-DPCRE2_CODE_UNIT_WIDTH=8",
        "-Izh_zero",
        "-Izh_vm",
        "-Izh_rtl",
        "-Izh_rtl/rdd/rdd_sql",
        "-Ithird_party/zlib",
        "-Ithird_party/pcre2",
        "-DZH_DYNLIB",
        #"-DZH_TR_LEVEL=4", #INFO
        #"-DZH_TR_LEVEL=5", DEBUG
    ]  + POSTGRESQL_COPT,
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
        "-DZH_DYNLIB",
        "-DZH_TR_LEVEL=5"
    ],
    alwayslink=1,
    visibility = ["//visibility:public"],
)

zh_comp_all(
    name = "hello_dbf_zh_lib", 
    srcs = [ "hello_dbf.zh" ],
    args = ZH_COMP_OPTS + [ "-DZH_DYNLIB", "-DZH_TR_LEVEL=5" ],
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
    ] + POSTGRESQL_LIB,
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
