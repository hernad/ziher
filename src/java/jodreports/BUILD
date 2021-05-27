java_library(
    # https://stackoverflow.com/questions/56893697/how-to-resolve-a-dependency-in-an-external-package-workspace-file

    name = "jodreports",
    srcs = glob(["src/main/java/**/*.java"]),
    #resources = glob(["src/main/resources/**"]),
    #https://www.baeldung.com/bazel-build-tool
    deps = [ 
      #"@maven//:junit_junit"
      #"@maven//:org_apache_commons_commons_lang3",
      "@maven//:org_freemarker_freemarker",
      "@maven//:org_slf4j_slf4j_api",
      "@maven//:commons_io_commons_io",
      "@maven//:org_slf4j_slf4j_jdk14",
      "@maven//:xml_apis_xml_apis",
      "@maven//:org_bluestemsoftware_open_maven_tparty_xerces_impl",
      "@maven//:xalan_xalan",
      "@maven//:jaxen_jaxen",
    ],
    visibility = ["//visibility:public"],

)