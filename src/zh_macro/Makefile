ROOT := ../../

#YACC_FLAGS := -p zh_macro
YACC_SOURCE := macro.y
YACC_DEPEND := macrolex.c
YACC_HEADERS := \
   zh_macro.h \
   hbsetup.h \
   hbpcode.h \
   hbdefs.h \

C_SOURCES := \
   macroa.c \
   macrob.c \
   macrolex.c \

LIBNAME := hbmacro

include $(TOP)$(ROOT)config/lib.mk
