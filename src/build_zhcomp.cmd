cd \dev\ziher\ziher_mono\ziher\src

c:\dev\ziher\bazel build //zh_comp:zh_comp --//bazel:windows_build=x64


REM cd C:\dev\ziher\ziher_mono\ziher\src>cd bazel-bin\test
REM dir f18ziherpy.dll




copy /y bazel-src\bazel-out\host\bin\zh_comp\main\zhcomp.exe c:\dev\ziher\zhcomp.exe
