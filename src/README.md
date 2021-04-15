# Build

Install [bazel.build](https://bazel.build/).

    # [ziher/src]
    bazel query //... 

    # bazel run //zh_comp/main:zhcomp -- -credits
    
    # fedora33 - gcc with cccahe
    # bazel run //zh_comp/main:zhcomp --strategy=CppCompile=standalone -- -credits


## Windows


    # Install `Windows Subsystem for Linux`
    
    # bazel build //zh_comp:zh_comp --experimental_enable_runfiles=yes
