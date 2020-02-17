ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := gtwin

C_SOURCES := \
   gtwin.c \

ifeq ($(ZH_PLATFORM),win)
   include $(TOP)$(ROOT)config/lib.mk
else
   ZH_SKIP_REASON := platform not supported
   include $(TOP)$(ROOT)config/none.mk
endif
