ROOT := ../../

# TODO: Add harbour.rc to zh_pp.exe on Windows

C_SOURCES := \
   pptable.c \
   ppcore.c \
   pplib.c \
   pplib2.c \

LIBNAME := zh_pp

LIBS := \
   hbnortl \
   hbcommon \

include $(TOP)$(ROOT)config/lib.mk

ifneq ($(ZH_PLATFORM),)
ifneq ($(ZH_COMPILER),)

ZH_VERHEADER := $(TOP)$(ROOT)include/hbverbld.h

BIN_FILE := $(BIN_DIR)/zh_pp$(BIN_EXT)

vpath zh_pp$(BIN_EXT) $(BIN_DIR)

pptable.c : zh_pp$(BIN_EXT)
	$(ZH_HOST_BIN_DIR)/zh_pp$(ZH_HOST_BIN_EXT) $(TOP)$(ROOT)include/hbstdgen.ch -opptable.c -q -c$(TOP)$(ROOT) -r$(TOP)$(ROOT)include/_repover.txt -v$(ZH_VERHEADER)
	-@$(RM) zh_pp$(OBJ_EXT) $(if $(OBJ_DYN_SUFFIX),zh_pp$(OBJ_DYN_SUFFIX)$(OBJ_EXT),)

#ifneq ($(ZH_BUILD_PARTS),lib)
   ifneq ($(ZH_INSTALL_BIN),)
      INSTALL_FILES := $(BIN_DIR)/zh_pp$(BIN_EXT)
      INSTALL_DIR := $(ZH_INSTALL_BIN)
      include $(TOP)$(ROOT)config/install.mk
   endif
#endif

clean::
	$(if $(wildcard $(ZH_VERHEADER)), -@$(RM) $(subst /,$(DIRSEP),$(ZH_VERHEADER)),)

endif
endif
