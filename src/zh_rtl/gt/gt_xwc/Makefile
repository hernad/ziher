ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := gtxwc

C_SOURCES := \
   gtxwc.c \

ifneq ($(ZH_HAS_X11),)

   ZH_CFLAGS += $(QUOTE)-I$(ZH_HAS_X11)$(QUOTE)

   ifeq ($(ZH_PLATFORM),darwin)
      ZH_CFLAGS += -DX_HAVE_UTF8_STRING
   endif

   include $(TOP)$(ROOT)config/lib.mk
else
   ZH_SKIP_REASON := component not found
   include $(TOP)$(ROOT)config/none.mk
endif
