
_DARWIN_COPTS = ["-DZH_OS_DARWIN"]
_WINDOWS_COPTS = ["-DZH_OS_WIN"]
_LINUX_COPTS = ["-DZH_OS_LINUX"]


_WINDOWS_LOPTS = [
    "WinMM.lib",
    "user32.lib"
]

_WINDOWS_LOPTS_2 = [
    "kernel32.lib",
    "ws2_32.lib",
    "iphlpapi.lib",
    "advapi32.lib",
    "gdi32.lib",
    "libpq.lib",
    "libssl.lib",
    "libcrypto.lib",
    "/LIBPATH:external/postgresql_windows_lib"
]


_LINUX_LOPTS = [
]

_LINUX_LOPTS_2 = [
    "-lz", 
    "-ldl", 
    "-lpthread",
    "-lpq"
]

_LINUX_POSTGRESQL_HEADERS = [ "@postgresql_linux//:headers" ]
_WINDOWS_POSTGRESQL_HEADERS = [ "@postgresql_windows//:headers" ]

_LINUX_POSTGRESQL_LIB = [ "@postgresql_linux_lib//:postgresql_lib" ]
_WINDOWS_POSTGRESQL_LIB = [ "@postgresql_windows_lib//:postgresql_lib" ]


_LINUX_POSTGRESQL_COPT = [ "-iexternal/postgresql_linux" ]
_WINDOWS_POSTGRESQL_COPT = [ 
    "/Iexternal/postgresql_windows" 
]


C_OPTS = select({
        "@bazel_tools//src/conditions:darwin": _DARWIN_COPTS,
        "@bazel_tools//src/conditions:windows": _WINDOWS_COPTS,
        "//conditions:default": _LINUX_COPTS,
    })

L_OPTS = select({
        "@bazel_tools//src/conditions:darwin": _LINUX_LOPTS,
        "@bazel_tools//src/conditions:windows": _WINDOWS_LOPTS,
        "//conditions:default": _LINUX_LOPTS,
    })

L_OPTS_2 = select({
        "@bazel_tools//src/conditions:darwin": _LINUX_LOPTS_2,
        "@bazel_tools//src/conditions:windows": _WINDOWS_LOPTS_2,
        "//conditions:default": _LINUX_LOPTS_2,
    })

POSTGRESQL_HEADERS = select({
        "@bazel_tools//src/conditions:darwin": _LINUX_POSTGRESQL_HEADERS,
        "@bazel_tools//src/conditions:windows": _WINDOWS_POSTGRESQL_HEADERS,
        "//conditions:default": _LINUX_POSTGRESQL_HEADERS,
    })

POSTGRESQL_LIB = select({
        "@bazel_tools//src/conditions:darwin": _LINUX_POSTGRESQL_LIB,
        "@bazel_tools//src/conditions:windows": _WINDOWS_POSTGRESQL_LIB,
        "//conditions:default": _LINUX_POSTGRESQL_LIB,
    })

POSTGRESQL_COPT = select({
        "@bazel_tools//src/conditions:darwin": _LINUX_POSTGRESQL_COPT,
        "@bazel_tools//src/conditions:windows": _WINDOWS_POSTGRESQL_COPT,
        "//conditions:default": _LINUX_POSTGRESQL_COPT,
    })

ZH_COMP_OPTS= [
    "-n",
    "-izh_zero", 
    "-izh_rtl",
    "-izh_rtl/gt"
]

ZH_Z18_COMP_OPTS=[
    "-iZ18/src/include",
    "-izh_harupdf",
    "-DGT_DEFAULT_CONSOLE",
    "-DF18_DEBUG",
    "-b"
]

ZH_Z18_HEADERS=[
    "//zh_zero:headers", 
    "//zh_rtl:headers",
    "//Z18/src/include:headers",
    "//zh_harupdf:headers"
]