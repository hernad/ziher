load("@rules_cc//cc:defs.bzl", "cc_library")

cc_library(
    name = "zh_pp",
    srcs = glob( ["*.c"]),
    hdrs = glob( ["*.h", "*.zhh"]) ,
    alwayslink = 1,
    deps = [ "//zh_zero:zh_zero" ],
    visibility = ["//visibility:public"],
)

# zh_pp 
#pptable.c : zh_pp$(BIN_EXT)
#	$(ZH_HOST_BIN_DIR)/zh_pp$(ZH_HOST_BIN_EXT) $(TOP)$(ROOT)include/hbstdgen.ch -opptable.c -q -c$(TOP)$(ROOT) -r$(TOP)$(ROOT)include/_repover.txt -v$(ZH_VERHEADER)
#	-@$(RM) zh_pp$(OBJ_EXT) $(if $(OBJ_DYN_SUFFIX),zh_pp$(OBJ_DYN_SUFFIX)$(OBJ_EXT),)
