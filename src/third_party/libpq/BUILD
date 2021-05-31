load("@rules_cc//cc:defs.bzl", "cc_library")

cc_library(
    name = "libpq",
    srcs = [
        "fe-auth-scram.c",
        "fe-connect.c",
        "fe-exec.c",
        "fe-lobj.c",
        "fe-misc.c",
        "fe-print.c",
        "fe-protocol2.c",
        "fe-protocol3.c",
        "fe-secure.c",
        "legacy-pqsignal.c",
        "libpq-events.c",
        "pqexpbuffer.c",
        "fe-auth.c",
    ],
    hdrs = glob([
        "*.h",
        "**/*.h",
    ]),
    deps = [
    ],
    copts = [
        "-DFRONTEND",
        "-DUNSAFE_STAT_OK",
        "-Itop/port",
        "-Ithird_party/libpq/top/include"
    ],
    alwayslink=1,
    visibility = ["//visibility:public"],
)

filegroup(
    name = "c_sources",
    srcs = glob([
        "*.c"
    ]),
    visibility = ["//visibility:public"],
)

cc_library(
    name = "headers",
    hdrs = glob([
        "*.h",
        "**/*.h",
    ]),
    visibility = ["//visibility:public"],
)
