"""
This is a simple windows_dll_library rule for builing a DLL Windows
that can be depended on by other cc rules.
Example useage:
  windows_dll_library(
      name = "hellolib",
      srcs = [
          "hello-library.cpp",
      ],
      hdrs = ["hello-library.h"],
      # Define COMPILING_DLL to export symbols during compiling the DLL.
      copts = ["/DCOMPILING_DLL"],
  )
"""

load("@rules_cc//cc:defs.bzl", "cc_binary", "cc_import", "cc_library")

def shared_library(
        name,
        os = "windows",
        srcs = [],
        deps = [],
        hdrs = [],
        linkopts = [],
        copts = [],
        visibility = None,
        **kwargs):

    # https://github.com/bazelbuild/bazel/issues/6883
    if os == "windows":
        windows_dll_library(name, srcs, deps, hdrs, linkopts, copts, visibility, **kwargs)
    else:
        linux_so_library(name, srcs, deps, hdrs, linkopts, copts, visibility, **kwargs)

# https://docs.bazel.build/versions/master/skylark/macros.html

def windows_dll_library(
        name,
        srcs = [],
        deps = [],
        hdrs = [],
        linkopts = [],
        copts = [],
        visibility = None,
        **kwargs):
    """A simple windows_dll_library rule for builing a DLL Windows."""
    dll_name = name + ".dll"
    import_lib_name = name + "_import_lib"
    import_target_name = name + "_dll_import"

    # Build the shared library
    cc_binary(
        name = dll_name,
        srcs = srcs + hdrs,
        deps = deps,
        linkopts = linkopts,
        copts = copts,
        linkshared = 1,
        **kwargs
    )

    # Get the import library for the dll
    native.filegroup(
        name = import_lib_name,
        srcs = [":" + dll_name],
        output_group = "interface_library",
    )

    # Because we cannot directly depend on cc_binary from other cc rules in deps attribute,
    # we use cc_import as a bridge to depend on the dll.
    cc_import(
        name = import_target_name,
        interface_library = ":" + import_lib_name,
        shared_library = ":" + dll_name,
    )

    # Create a new cc_library to also include the headers needed for the shared library
    cc_library(
        name = name,
        hdrs = hdrs,
        linkopts = linkopts,
        copts = copts,
        visibility = visibility,
        deps = deps + [
            ":" + import_target_name,
        ],
    )

def linux_so_library(
        name,
        srcs = [],
        deps = [],
        hdrs = [],
        linkopts = [],
        copts = [],
        visibility = None,
        **kwargs):
    so_name = name + ".so"
    import_target_name = name + "_import"


    # Build the shared library
    cc_binary(
        name = so_name,
        srcs = srcs + hdrs,
        deps = deps,        
        linkopts = linkopts,
        copts = copts,
        linkshared = 1,
        visibility = visibility,
        **kwargs
    )

    
    # Because we cannot directly depend on cc_binary from other cc rules in deps attribute,
    # we use cc_import as a bridge to depend on the dll.
    cc_import(
        name = import_target_name,
        shared_library = ":" + so_name,
        visibility = visibility
    )

