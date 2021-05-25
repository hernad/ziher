# Build

Install [bazel.build](https://bazel.build/).

    # [ziher/src]
    bazel query //... 

    # bazel run //zh_comp/main:zhcomp -- -credits
    
    # fedora33 - gcc with cccahe
    # bazel run //zh_comp/main:zhcomp --strategy=CppCompile=standalone -- -credits


## Windows

    # Install cygwin64
    
    # bazel build //zh_comp:zh_comp --experimental_enable_runfiles=yes
    # c:\dev\bazelisk.exe run //test:run_hello_dbf_win --//bazel:windows_build=x64

## Linux

    #~/.bazelrc 
    build --strategy=CppCompile=standalone


## Trace

    export ZH_TR_OUTPUT=trace.log
    export ZH_TR_LEVEL=5 #debug
