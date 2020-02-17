ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := gttrm

C_SOURCES := \
   gttrm.c \

ZH_SUPPORTED := yes
ifeq ($(ZH_PLATFORM),win)
   ZH_SUPPORTED := no
endif
ifeq ($(ZH_PLATFORM),wce)
   ZH_SUPPORTED := no
endif
ifeq ($(ZH_PLATFORM),dos)
   ZH_SUPPORTED := no
endif
ifeq ($(ZH_PLATFORM),os2)
   ZH_SUPPORTED := no
endif
ifeq ($(ZH_COMPILER),djgpp)
   ZH_SUPPORTED := yes
endif
ifeq ($(ZH_PLATFORM),vxworks)
   ZH_SUPPORTED := no
endif

ifeq ($(ZH_SUPPORTED),yes)

   ifneq ($(ZH_HAS_GPM),)
      ZH_CFLAGS += -DZH_HAS_GPM
   endif

   include $(TOP)$(ROOT)config/lib.mk
else
   ZH_SKIP_REASON := platform or compiler not supported
   include $(TOP)$(ROOT)config/none.mk
endif
