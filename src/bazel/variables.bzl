ZH_COMP_OPTS=[
    "-n","-izh_zero", 
    "-izh_rtl",
    "-izh_rtl/gt"
]

ZH_Z18_COMP_OPTS=[
    "-iZ18/src/include",
    "-izh_harupdf",
    "-DGT_DEFAULT_CONSOLE",
    "-b"
]

ZH_Z18_HEADERS=[
    "//zh_zero:headers", 
    "//zh_rtl:headers",
    "//Z18/src/include:headers",
    "//zh_harupdf:headers"
]