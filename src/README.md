# Build

Install [bazel.build](https://bazel.build/).

    # [ziher/src]
    bazel query //... 

    # bazel run //zh_comp/main:zhcomp -- -credits


## Windows


    # Install `Windows Subsystem for Linux`
    
    # bazel build //zh_comp:zh_comp --experimental_enable_runfiles=yes
