ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := xlsxwriter

ZH_BUILD_WARN := no
ZH_BUILD_MODE := c

C_SOURCES := \
    app.c \
    chart.c \
    chartsheet.c \
    content_types.c \
    core.c \
    custom.c \
    drawing.c \
    format.c \
    hash_table.c \
    packager.c \
    relationships.c \
    shared_strings.c \
    styles.c \
    theme.c \
    utility.c \
    workbook.c \
    worksheet.c \
    xmlwriter.c \


ifneq ($(ZH_HAS_XLSWRITER_LOCAL),)
   ZH_CFLAGS += -DUSE_STANDARD_TMPFILE
   ifeq ($(ZH_PLATFORM),win)
     ZH_CFLAGS += -D_WIN32 -DUSE_SYSTEM_MINIZIP
   endif
   ZH_CFLAGS += $(QUOTE)-I$(ZH_HAS_ZLIB)$(QUOTE)
   ZH_CFLAGS += $(QUOTE)-I$(ZH_HAS_PNG)$(QUOTE)
   ZH_CFLAGS += $(QUOTE)-I$(ZH_HAS_XLSWRITER)$(QUOTE)
   include $(TOP)$(ROOT)config/lib.mk
else
    ZH_SKIP_REASON := xlsxwriter local copy unused
    include $(TOP)$(ROOT)config/none.mk
endif
