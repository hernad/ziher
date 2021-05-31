load("@rules_cc//cc:defs.bzl", "cc_library")

_C_SOURCES = [
        #"fe-auth-scram.c",
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
        "top/port/snprintf.c",
        "top/port/chklocale.c",
        "top/port/strerror.c",
        "top/port/inet_net_ntop.c",
        "top/port/getpeereid.c",
        "top/port/noblock.c",
        "top/port/thread.c",
        "top/port/strlcpy.c",
        "top/port/pgstrcasecmp.c",
        "top/common/ip.c",
        "top/common/link-canary.c",
        "top/common/string.c",
        "top/common/wchar.c",
        "top/common/md5.c",
    ]

cc_library(
    name = "libpq",
    srcs = _C_SOURCES,
    hdrs = glob([
        "*.h",
        "**/*.h",
    ]),
    deps = [
    ],
    copts = [
        "-DFRONTEND",
        "-DUNSAFE_STAT_OK",
        #"-Itop/port",
        "-Ithird_party/libpq/top/include",
        "-Ithird_party/libpq/gen",
        #"-DSO_PEERCRED=18",   #define SO_PASSCRED 17 #define SO_PEERCRED 18  # Linux: use getsockopt(SO_PEERCRED)  
    ],
    alwayslink=1,
    visibility = ["//visibility:public"],
)

filegroup(
    name = "c_sources",
    srcs = _C_SOURCES,
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
