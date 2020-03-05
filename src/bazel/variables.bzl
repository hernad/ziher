
_DARWIN_COPTS = ["-DZH_OS_DARWIN"]
_WINDOWS_COPTS = ["-DZH_OS_WIN"]
_LINUX_COPTS = ["-DZH_OS_LINUX"]

C_OPTS = select({
        "@bazel_tools//src/conditions:darwin": _DARWIN_COPTS,
        "@bazel_tools//src/conditions:windows": _WINDOWS_COPTS,
        "//conditions:default": _LINUX_COPTS,
    })

_WINDOWS_LOPTS = [
    "WinMM.lib",
    "user32.lib"
]
_LINUX_LOPTS =  [
    "-lpthread"
]
_DARWIN_LOPTS = ["-lpthread"]
L_OPTS = select({
        "@bazel_tools//src/conditions:darwin": _DARWIN_LOPTS,
        "@bazel_tools//src/conditions:windows": _WINDOWS_LOPTS,
        "//conditions:default": _LINUX_LOPTS,
    })


ZH_COMP_OPTS=[
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