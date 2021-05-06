#load(":build_settings.bzl", "arch_x86")

# build arch_x86 = True
#c:\dev\bazel-x86.exe build  //test:run_hello_dbf_win --//bazel:arch_x86

# build arch_x86 = False
#c:\dev\bazel-x86.exe build  //test:run_hello_dbf_win --no//bazel:arch_x86

load("@bazel_skylib//rules:common_settings.bzl", "bool_flag", "string_flag")
load("@bazel_skylib//lib:selects.bzl", "selects")

#bool_flag(
#    name = "arch_x86",
#    build_setting_default = False, 
#)

string_flag(
    name = "windows_build",
    build_setting_default = "", 
)


#config_setting(
#    name = "windows_xx",
#    values = {
#        "crosstool_top": "//crosstools/windows",
#    },
#)
#bazel build ... --crosstool_top=//crosstools/windows


# https://docs.bazel.build/versions/1.2.0/configurable-attributes.html


#config_setting(
#    name = "windows_x64",
#    flag_values = {
#        ":windows": "True",
#        ":arch_x86": "False"
#    },
#)

#C:\dev\ziher-32\src>c:\dev\bazel-x86.exe build  //test:run_hello_dbf_win --//bazel:windows_build=x86

config_setting(
    name = "windows_x86",
    flag_values = {
        "//bazel:windows_build": "x86",
    },
)

config_setting(
    name = "windows_x64",
    flag_values = {
        "//bazel:windows_build": "x64",
    },
)

selects.config_setting_group(
    name = "windows",
    match_any = [":windows_x86", ":windows_x64"],
)
