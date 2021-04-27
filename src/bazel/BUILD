#load(":build_settings.bzl", "arch_x86")

# build arch_x86 = True
#c:\dev\bazel-x86.exe build  //test:run_hello_dbf_win --//bazel:arch_x86

# build arch_x86 = False
#c:\dev\bazel-x86.exe build  //test:run_hello_dbf_win --no//bazel:arch_x86

load("@bazel_skylib//rules:common_settings.bzl", "bool_flag")
bool_flag(
    name = "arch_x86",
    build_setting_default = False, 
)

# https://docs.bazel.build/versions/1.2.0/configurable-attributes.html
config_setting(
    name = "windows_x86",
    flag_values = {
        ":arch_x86": "True"
    },
)

config_setting(
    name = "windows_x64",
    flag_values = {
        ":arch_x86": "False"
    },
)
