ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := gtwvt

C_SOURCES := \
   gtwvt.c \

ifneq ($(filter $(ZH_PLATFORM),win wce),)
   include $(TOP)$(ROOT)config/lib.mk
else
   ZH_SKIP_REASON := platform not supported
   include $(TOP)$(ROOT)config/none.mk
endif
