#_WINDOWS_LINUX="windows"
WINDOWS_LINUX = "windows"

_DARWIN_COPTS = [
    "-DUNICODE",
    "-DZH_OS_DARWIN"
]
_WINDOWS_COPTS = [
    "-DUNICODE",
    "-DZH_OS_WIN",
    "-DZH_OS_WIN_64",
    "-Izh_rtl"
]
_WINDOWS_X86_COPTS = [
    "-DZH_OS_WIN",
    "-Izh_rtl"
]

_LINUX_COPTS = [
    "-DUNICODE",
    "-DZH_OS_LINUX"
]

_WINDOWS_LOPTS = [
    "WinMM.lib",
    "user32.lib"
]
_WINDOWS_X86_LOPTS = _WINDOWS_LOPTS

_WINDOWS_LOPTS_2 = [
    "kernel32.lib",
    "ws2_32.lib",
    "iphlpapi.lib",
    "advapi32.lib",
    "gdi32.lib",
    "libpq.lib",
    "libssl.lib",
    "libcrypto.lib",
    "/LIBPATH:external/postgresql_windows_lib",
    "/LIBPATH:external/python_windows_lib",
]
_WINDOWS_X86_LOPTS_2 = [
    "kernel32.lib",
    "ws2_32.lib",
    "iphlpapi.lib",
    "advapi32.lib",
    "gdi32.lib",
    "libpq.lib",
    "libssl.lib",
    "libcrypto.lib",
    "/LIBPATH:external/postgresql_x86_windows_lib",
    "/LIBPATH:external/python_x86_windows_lib"
]

_LINUX_LOPTS = [
    "-lm",
    "-lX11",
]

_LINUX_LOPTS_2 = [
    "-lz", 
    "-ldl", 
    "-lpthread",
    "-lpq",
    "-lssl",
    "-lcrypto"
]

_LINUX_POSTGRESQL_HEADERS = [ "@postgresql_linux//:headers" ]
_WINDOWS_POSTGRESQL_HEADERS = [ "@postgresql_windows//:headers" ]

_WINDOWS_X86_POSTGRESQL_HEADERS = [ "@postgresql_x86_windows//:headers" ]

_LINUX_POSTGRESQL_LIB = [ "@postgresql_linux_lib//:postgresql_lib" ]
_WINDOWS_POSTGRESQL_LIB = [ "@postgresql_windows_lib//:postgresql_lib" ]
_WINDOWS_X86_POSTGRESQL_LIB = [ "@postgresql_x86_windows_lib//:postgresql_lib" ]

_LINUX_POSTGRESQL_COPT = [ "-Iexternal/postgresql_linux" ]

_WINDOWS_POSTGRESQL_COPT = [ 
    "/Iexternal/postgresql_windows" 
]
_WINDOWS_X86_POSTGRESQL_COPT = [ 
    "/Iexternal/postgresql_x86_windows" 
]

_LINUX_PYTHON_HEADERS = [ "@python_linux//:headers" ]
_WINDOWS_PYTHON_HEADERS = [ "@python_windows//:headers" ]
_WINDOWS_X86_PYTHON_HEADERS = [ "@python_x86_windows//:headers" ]

_WINDOWS_PYTHON_LIB = [ "@python_windows_lib//:python_lib" ]


_LINUX_PYTHON_COPT = [ "-Iexternal/python_linux" ]
_WINDOWS_X86_PYTHON_COPT = [ 
    "/Iexternal/python_x86_windows" 
]
_WINDOWS_PYTHON_COPT = [ 
    "/Iexternal/python_windows" 
]

# C:\dev\ziher-32\src>c:\dev\bazel-x86.exe  query @bazel_tools//src/conditions:all
#@bazel_tools//src/conditions:windows_msys
#@bazel_tools//src/conditions:windows_msvc
#@bazel_tools//src/conditions:windows
#..
#@bazel_tools//src/conditions:linux_x86_64
#@bazel_tools//src/conditions:linux_s390x
#@bazel_tools//src/conditions:linux_ppc
#@bazel_tools//src/conditions:linux_aarch64
#@bazel_tools//src/conditions:host_windows_msys
#@bazel_tools//src/conditions:host_windows_msvc
#@bazel_tools//src/conditions:host_windows
#..
#@bazel_tools//src/conditions:darwin_x86_64
#@bazel_tools//src/conditions:darwin


C_OPTS = select({
        "//bazel:windows_x86": _WINDOWS_X86_COPTS,
        "//bazel:windows_x64": _WINDOWS_COPTS,
        "@bazel_tools//src/conditions:darwin": _DARWIN_COPTS,
        "//conditions:default": _LINUX_COPTS,
    })


L_OPTS = select({
        "//bazel:windows_x86": _WINDOWS_X86_LOPTS,
        "//bazel:windows_x64": _WINDOWS_LOPTS,
        "@bazel_tools//src/conditions:darwin": _LINUX_LOPTS,
        "//conditions:default": _LINUX_LOPTS,
    })

L_OPTS_2 = select({
        "//bazel:windows_x86": _WINDOWS_X86_LOPTS_2,
        "//bazel:windows_x64": _WINDOWS_LOPTS_2,
        "@bazel_tools//src/conditions:darwin": _LINUX_LOPTS_2,
        "//conditions:default": _LINUX_LOPTS_2,
    }) + []


POSTGRESQL_HEADERS = select({
        "//bazel:windows_x86": _WINDOWS_X86_POSTGRESQL_HEADERS,
        "//bazel:windows_x64": _WINDOWS_POSTGRESQL_HEADERS,
        "@bazel_tools//src/conditions:darwin": _LINUX_POSTGRESQL_HEADERS,
        "//conditions:default": _LINUX_POSTGRESQL_HEADERS,
    })


POSTGRESQL_LIB = select({
        "//bazel:windows_x86": _WINDOWS_X86_POSTGRESQL_LIB,
        "//bazel:windows_x64": _WINDOWS_POSTGRESQL_LIB,
        "@bazel_tools//src/conditions:darwin": _LINUX_POSTGRESQL_LIB,
        "//conditions:default": _LINUX_POSTGRESQL_LIB,
    })


POSTGRESQL_COPT = select({
        "//bazel:windows_x86": _WINDOWS_X86_POSTGRESQL_COPT,
        "//bazel:windows_x64": _WINDOWS_POSTGRESQL_COPT,
        "@bazel_tools//src/conditions:darwin": _LINUX_POSTGRESQL_COPT,
        "//conditions:default": _LINUX_POSTGRESQL_COPT,
    })

PYTHON_HEADERS = select({
        "//bazel:windows_x86": _WINDOWS_X86_PYTHON_HEADERS,
        "//bazel:windows_x64": _WINDOWS_PYTHON_HEADERS,
        "@bazel_tools//src/conditions:darwin": _LINUX_PYTHON_HEADERS,
        "//conditions:default": _LINUX_PYTHON_HEADERS,
    })
    
PYTHON_LIB = select({
        "//bazel:windows_x64": _WINDOWS_PYTHON_LIB,
        "//conditions:default": _WINDOWS_PYTHON_LIB,
    })

PYTHON_COPT = select({
        "//bazel:windows_x86": _WINDOWS_X86_PYTHON_COPT,
        "//bazel:windows_x64": _WINDOWS_PYTHON_COPT,
        "@bazel_tools//src/conditions:darwin": _LINUX_PYTHON_COPT,
        "//conditions:default": _LINUX_PYTHON_COPT,
    })

ZH_COMP_OPTS= [
    "-n",
    #"-gc3", # generate real C code 0=compact (default) 1=normal 2=verbose 3=generate real C code
    "-izh_zero", 
    "-izh_rtl",
    "-izh_rtl/gt"
]

ZH_COMP_OPTS_DEBUG= [
    "-n",
    #"-gc3", # generate real C code 0=compact (default) 1=normal 2=verbose 3=generate real C code
    "-izh_zero", 
    "-izh_rtl",
    "-izh_rtl/gt",
    "-b"
]

ZH_Z18_COMP_OPTS=[
    "-iZ18/src/include",
    "-izh_harupdf",
    #"-DGT_DEFAULT_CONSOLE",
    "-DF18_DEBUG",
    "-b"
]

ZH_DEPS_F18=[
    "//zh_zero:headers_filegroup", 
    "//zh_rtl:headers_filegroup",
    "//Z18/src/include:headers",
    "//zh_harupdf:headers"
]

ZH_DEPS_STD=[ 
    "//zh_zero:headers_filegroup", 
    "//zh_rtl:headers_filegroup" 
]
