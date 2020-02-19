ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

#vpath %.c ../
#vpath %.zh ../

ifeq ($(ZH_PLATFORM),win)
   ifneq ($(filter $(ZH_COMPILER),mingw mingw64),)
      C_MAIN := mainwin.c
   else
      C_MAIN := mainstd.c mainwin.c
   endif
else
   ifeq ($(ZH_PLATFORM),cygwin)
      C_MAIN := mainwin.c
   else
      C_MAIN := mainstd.c
   endif
endif

#ifeq ($(ZH_ZHVM_ALL),yes)
#
#C_SOURCES := \
#   hvmall.c \
#
#else

C_SOURCES := \
   hvm.c \
   arrays.c \
   classes.c \
   dynsym.c \
   codebloc.c \
   estack.c \
   extend.c \
   fm.c \
   garbage.c \
   hashes.c \
   itemapi.c \
   strapi.c \
   macro.c \
   memvars.c \
   set.c \
   thread.c \

#endif

C_SOURCES += \
   arrayshb.c \
   asort.c \
   break.c \
   cmdarg.c \
   debug.c \
   dynlibhb.c \
   eval.c \
   evalhb.c \
   extrap.c \
   hashfunc.c \
   initexit.c \
   initsymb.c \
   legacy.c \
   memvclip.c \
   pbyref.c \
   pcount.c \
   pvalue.c \
   pvaluehb.c \
   proc.c \
   procaddr.c \
   runner.c \
   vm.c \
   $(C_MAIN) \

PRG_SOURCES := \
   harbinit.zh \

LIBNAME := zh_vmmt

#ZH_CFLAGS += -DZH_MT_VM

include $(TOP)$(ROOT)config/lib.mk
