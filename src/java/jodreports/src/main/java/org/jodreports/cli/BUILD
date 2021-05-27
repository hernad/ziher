
# build deploy.jar

# bazel build //java/jodreports/src/main/java/org/jodreports/cli:jodreports-cli_deploy.jar

# cp -av bazel-bin/java/jodreports/src/main/java/org/jodreports/cli/jodreports-cli_deploy.jar jodreports-cli.jar

#export CLASSPATH=`pwd`/jodreports-cli.jar

#>>> from jnius import autoclass
#>>> JOD = autoclass('org.jodreports.cli.CreateDocument')
#>>> JOD.main(["/home/ernad.husremovic/ziher/src/java/jodreports/src/test/resources/order.odt","/home/ernad.husremovic/ziher/src/java/jodreports/src/test/resources/order-data.xml","/home/ernad.husremovic/ziher/src/out.odt"])


java_binary(
    name = "jodreports-cli",
    main_class = "org.jodreports.cli.CreateDocument",
    #runtime_deps = [ ":jodreports" ]
    srcs = [ "CreateDocument.java" ],
    deps = [
      "//java/jodreports:jodreports",
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
    ]
)