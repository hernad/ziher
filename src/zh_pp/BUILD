load("@rules_cc//cc:defs.bzl", "cc_library")
load("//bazel:zh_pp.bzl", "zh_pp")

cc_library(
    name = "zh_pp",
    srcs = glob( ["*.c"]),
    hdrs = glob( ["*.h", "*.zhh"]) ,
    #alwayslink = 1,
    deps = [ "//zh_zero:zh_zero" ],
    visibility = ["//visibility:public"],
)

zh_pp(
    name = "pp_table", 
    ppin = "std_gen.zhh",
    out = "pp_table.c",
    args = "-q2 -izh_zero -vpp_table.h",
    visibility = ["//visibility:public"],
)

# zh_pp 
#pptable.c : zh_pp$(BIN_EXT)
#	$(ZH_HOST_BIN_DIR)/zh_pp$(ZH_HOST_BIN_EXT) $(TOP)$(ROOT)include/hbstdgen.ch -opptable.c -q -c$(TOP)$(ROOT) -r$(TOP)$(ROOT)include/_repover.txt -v$(ZH_VERHEADER)
#	-@$(RM) zh_pp$(OBJ_EXT) $(if $(OBJ_DYN_SUFFIX),zh_pp$(OBJ_DYN_SUFFIX)$(OBJ_EXT),)


#[hernad@xps-9570-out-ba harbour]$ cat include/_repover.txt 
#91f35ee89ad8d2390d6d616ecaede4be67e7e031
#91f35ee8
#2020-01-08 17:05:14 +0100
#hernad
#hernad@bring.out.ba
#git@github.com:hernad/harbour.git
